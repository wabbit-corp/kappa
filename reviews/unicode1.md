I reviewed the uploaded compiler sources statically. I could not run the build or tests because this sandbox does not have the .NET SDK installed, because apparently civilization remains optional. The static review was still enough to find several concrete semantic gaps.

## Verdict

The front-end has a reasonable start: source files are decoded as strict UTF-8, scalar and grapheme literals are checked, and the interpreter has partial runtime values for `Byte`, `Bytes`, `UnicodeScalar`, `Grapheme`, and hash types.

But the compiler does **not** currently have coherent Unicode / bytes / hash semantics across the full stack. The biggest problems are:

1. **Byte literal decoding accepts values the spec says should be rejected.**
2. **`std.unicode` declares more than the interpreter implements.**
3. **`decodeUtf8` has a broken error path.**
4. **String ordering is UTF-16 ordinal in the interpreter, not Unicode-scalar order.**
5. **`std.hash` is seriously broken, especially the streaming API and `Hashable`.**
6. **The IL, Zig/C, and hosted runtimes do not preserve these semantics.**

The hash layer is the worst offender. It is not “a few rough edges”; it is a semantic trapdoor wearing a small fake moustache.

---

## What is solid

### Strict source UTF-8 loading is good

`CompilationFrontend.readSource` reads bytes and calls `UnicodeText.decodeUtf8Strict`; invalid UTF-8 becomes `E_UNICODE_INVALID_UTF8` before lexing. See:

* `src/Kappa.Compiler/CompilationFrontend.fs:63-80`
* `src/Kappa.Compiler/UnicodeText.fs:15-24`

That matches the spec’s basic invariant that source text is UTF-8 and that `String` values are valid Unicode scalar text.

### Scalar and grapheme literal validation is directionally right

The literal decoder rejects invalid scalar escapes through `UnicodeText.tryScalarFromValue`, which excludes surrogate code points and out-of-range values. Scalar literals require exactly one `Rune`; grapheme literals require exactly one grapheme cluster. See:

* `src/Kappa.Compiler/Syntax.fs:1066-1144`
* `src/Kappa.Compiler/Syntax.fs:1250-1278`
* `src/Kappa.Compiler/UnicodeText.fs:52-74`
* `src/Kappa.Compiler/UnicodeText.fs:79-103`

So things like `'\u{D800}'` should be rejected, and `'e\u{301}'` should not pass as a scalar literal. That part is conceptually aligned.

---

## Unicode issues

### 1. Source locations are UTF-16 code-unit based

`SourceText.Length`, spans, and columns are based on .NET `string.Length`, so columns count UTF-16 code units, not Unicode scalar values or UTF-8 bytes. See:

* `src/Kappa.Compiler/Text.fs:29-33`
* `src/Kappa.Compiler/Text.fs:61-77`

This means a non-BMP character such as `🙂` counts as two columns internally. The spec allows extra offset forms if clearly named, but the user-facing line/column output will be surprising unless documented. If diagnostics are meant to be human-facing Unicode columns, this needs fixing.

### 2. Unicode warning diagnostics exist but are not emitted

The compiler declares:

* `W_UNICODE_BIDI_CONTROL`
* `W_UNICODE_CONFUSABLE_IDENTIFIER`
* `W_UNICODE_NON_NORMALIZED_SOURCE_TEXT`

in `Diagnostics.fs`, but I found no emitters for them. See:

* `src/Kappa.Compiler/Diagnostics.fs:66-72`
* `src/Kappa.Compiler/Diagnostics.fs:151-157`
* `src/Kappa.Compiler/Diagnostics.fs:297-302`

The spec says bidi controls outside string/byte literals **should** warn, and confusable/non-normalized warnings may be emitted. Right now these are decorative labels, like a fire alarm painted on a brick.

### 3. Identifiers are UTF-16 `char` based

Identifier rules use:

```fsharp
Char.IsLetter(character) || character = '_'
Char.IsLetterOrDigit(character) || character = '_'
```

See `src/Kappa.Compiler/Syntax.fs:755-758`.

That works for many BMP scripts but not for non-BMP identifier characters, because those arrive as surrogate pairs in .NET `char` iteration. It also does not enforce a pinned Unicode identifier profile, mixed-script policy, confusable policy, or normalization policy.

### 4. Interpreter string ordering is not Unicode scalar order

`tryCompareValues` uses `String.CompareOrdinal` for `String`, `Character`, and `Grapheme`:

* `src/Kappa.Compiler/Interpreter.fs:283-293`

The spec says `Ord String` is lexicographic by Unicode scalar value. UTF-16 ordinal comparison is **not equivalent** for supplementary characters. Example:

* `"\u{1F600}"` has UTF-16 leading surrogate `D83D`
* `"\u{E000}"` has BMP code unit `E000`

UTF-16 ordinal says `D83D < E000`, so the emoji sorts before `U+E000`. Unicode scalar order says `U+1F600 > U+E000`. That is a real semantic mismatch.

Equality is fine for valid strings. Ordering is not.

### 5. `std.unicode` exports functions the interpreter does not create

`StandardModules.fs` declares:

* `scalars`
* `graphemes`
* `words`
* `sentences`

See `src/Kappa.Compiler/StandardModules.fs:33-56`.

But `Interpreter.tryCreateIntrinsicTermValue` omits those names. It only creates things like `utf8Bytes`, `decodeUtf8`, `scalarCount`, `graphemeCount`, `normalize`, etc. See:

* `src/Kappa.Compiler/Interpreter.fs:336-361`

So the module advertises APIs that the interpreter cannot instantiate. That is not an implementation detail. That is an IOU stapled to a standard library.

### 6. `decodeUtf8` error path is broken

`decodeUtf8` declares:

```kappa
Bytes -> Result UnicodeDecodeError String
```

See `src/Kappa.Compiler/StandardModules.fs:38-40`.

The interpreter does this on failure:

```fsharp
resultError (StringValue message)
```

See `src/Kappa.Compiler/Interpreter.fs:1374-1380`.

But `resultError` constructs `"Error"`:

```fsharp
let resultError value = constructPreludeValue "Error" [ value ]
```

See `src/Kappa.Compiler/Interpreter.fs:258-259`.

The prelude constructor is `Err`, not `Error`:

* `src/Kappa.Compiler/Stdlib/std/prelude.kp:35-37`

So invalid UTF-8 decoding will attempt to construct a nonexistent `std.prelude.Error`. Also, the error payload is a `StringValue`, not a real `UnicodeDecodeError`. That is two bugs in one trench coat.

### 7. Unicode version is hardcoded but behavior is delegated

The interpreter returns:

```fsharp
UnicodeVersionValue "15.1.0"
```

See `src/Kappa.Compiler/Interpreter.fs:336-337`.

But grapheme segmentation uses `StringInfo.ParseCombiningCharacters`:

* `src/Kappa.Compiler/UnicodeText.fs:79-103`

Normalization uses .NET normalization:

* `src/Kappa.Compiler/UnicodeText.fs:26-44`

That means behavior depends on the .NET runtime and its globalization backend, not on compiler-pinned Unicode data. Returning `"15.1.0"` is only valid if you can prove the underlying segmentation and normalization data matches that version. I would not bet the estate silverware on it.

### 8. `words` and `sentences` helpers are not UAX #29

Even though the interpreter does not expose `words` and `sentences`, the helper implementations are naive ASCII splitting:

* `src/Kappa.Compiler/UnicodeText.fs:116-124`

The spec calls for Unicode Standard Annex #29 word/sentence boundaries unless a tailored locale API is used. The current implementation is not close.

---

## Bytes issues

### 1. Byte literal semantics are wrong for non-ASCII scalar text

Spec rule: `\xNN` denotes a raw byte, but Unicode escapes are allowed in byte literals only when the resulting scalar’s UTF-8 encoding is exactly one byte. See `Spec.md:5555-5565`.

The implementation decodes the byte literal body through the ordinary string unescaper, then accepts any single scalar with `rune.Value <= 0xFF`:

* `src/Kappa.Compiler/Syntax.fs:1270-1278`
* `src/Kappa.Compiler/UnicodeText.fs:108-114`

That means these are likely accepted:

```kappa
b'ÿ'
b'\u{00FF}'
b'\u{0080}'
```

But by the spec, they should be rejected because their UTF-8 encodings are not exactly one byte. Only this should produce byte `0xFF`:

```kappa
b'\xFF'
```

The current decoder loses escape provenance too early. It cannot distinguish “raw byte escape `\xFF`” from “Unicode scalar U+00FF”. Byte literals need their own decoder, not a string decoder followed by `<= 0xFF`.

### 2. `Bytes` exists mostly in the interpreter

Runtime has:

```fsharp
| BytesValue of byte array
```

See `src/Kappa.Compiler/Interpreter.fs:18-31`.

But `std.bytes` declares only a `BytesBuilder` type and no operations:

* `src/Kappa.Compiler/StandardModules.fs:59-63`

`Bytes` can be produced by `utf8Bytes`, but there is no ordinary byte-array literal or robust construction API visible in the standard module. That makes it hard to test `decodeUtf8` on invalid byte sequences from pure Kappa code. A bytes type that cannot conveniently contain arbitrary bytes is, in technical terms, cosplay.

### 3. Backends do not preserve `Bytes`

The interpreter has `BytesValue`, but the hosted runtime and Zig runtime do not have equivalent byte-array value representations. The Zig runtime value tag list has `STRING` and `CHAR`, but no `BYTE` or `BYTES` tag:

* `src/Kappa.Compiler/Runtime/Zig/kappa_runtime.c:10-22`
* `src/Kappa.Compiler/Runtime/Zig/kappa_runtime.c:52-65`

The hosted runtime similarly lacks `Byte`, `Bytes`, `Grapheme`, and hash values in its literal kind/runtime value model:

* `src/Kappa.Compiler/Runtime/Hosted/KappaRuntime.cs:14-21`
* `src/Kappa.Compiler/Runtime/Hosted/KappaRuntime.cs:396-414`

So these semantics are interpreter-local, not compiler-wide.

---

## Hash issues

This is the critical section.

### 1. `Hashable` is declared without its required member

The spec says:

```kappa
trait Eq a => Hashable (a : Type) =
    hashInto : (& value : a) -> (1 state : HashState) -> HashState
```

See `Spec.md:18884-18888`.

The compiler declares:

```fsharp
Traits = [ { Name = "Hashable"; TypeParameterCount = 1; Members = [] } ]
```

See `src/Kappa.Compiler/StandardModules.fs:65-84`.

So `Hashable` has no `hashInto`, no `Eq` superclass, no ordinary instance coherence, and no actual runtime dictionary behavior. The compiler synthesizes fake builtin dictionaries for selected types, then erases them for `hashField` and `hashWith`:

* `src/Kappa.Compiler/SurfaceElaboration.fs:5294-5320`
* `src/Kappa.Compiler/SurfaceElaboration.fs:5474-5500`
* `src/Kappa.Compiler/SurfaceElaboration.fs:18645-18658`

That is not the spec’s trait. It is a compiler-known permission slip.

### 2. Streaming hash functions return `Option`, not `HashState`

This is the worst concrete bug I found.

`hashIntoState` returns a host `RuntimeValue option`:

* `src/Kappa.Compiler/Interpreter.fs:1155-1180`

Then the streaming hash intrinsics do this:

```fsharp
hashIntoState state value |> optionValue |> Result.map Some
```

See:

* `src/Kappa.Compiler/Interpreter.fs:1521-1530`

But `optionValue` constructs a Kappa `Option` value, `Some x` or `None`. Therefore:

```kappa
hashString "a" (newHashState defaultHashSeed)
```

does not return a `HashState`. It returns something shaped like:

```kappa
Some (HashState ...)
```

despite the declared type being `HashState`.

Unsupported values return Kappa `None`, also despite the declared return type being `HashState`.

This is not a corner case. It breaks `hashBool`, `hashChar`, `hashString`, `hashBytes`, `hashInt`, `hashInteger`, `hashFloatRaw`, `hashDoubleRaw`, `hashNatTag`, and `hashField`.

`hashWith` bypasses this particular bug for its direct primitive cases, but it has other issues.

### 3. The advertised `Hashable` fallback accepts types the runtime cannot hash

The typechecker fallback says `Hashable` is available for:

* `Unit`
* `Bool`
* `Byte`
* `Bytes`
* `UnicodeScalar`
* `Grapheme`
* `String`
* `Int`
* `Integer`
* `Float`
* `Double`
* `Ordering`
* `Option`
* `List`
* `Array`
* `Result`

See `src/Kappa.Compiler/SurfaceElaboration.fs:5294-5320`.

But the interpreter’s `hashIntoState` only handles primitive runtime values plus `Unit`. It does not recursively hash `ConstructedValue`, lists, options, results, arrays, or `Ordering`:

* `src/Kappa.Compiler/Interpreter.fs:1159-1180`

So the front-end can approve `hashWith defaultHashSeed (Some 1)`, but the interpreter cannot actually hash it. A typeclass that typechecks more programs than the runtime can execute is how one summons production incidents.

### 4. Hash state transition is not a proper streaming update

`UnicodeText.hashBytesWithSeed` does:

```fsharp
let mutable state = uint64 seed ^^^ fnvOffset
for value in bytes do
    state <- state ^^^ uint64 value
    state <- state * fnvPrime
```

See `src/Kappa.Compiler/UnicodeText.fs:126-136`.

Then `newHashState seed` just stores the seed:

* `src/Kappa.Compiler/Interpreter.fs:1497-1498`

And each `hashX` reuses `hashBytesWithSeed state bytes`.

That means every field update re-xors the current state with `fnvOffset`. This is a one-shot keyed hash helper being used as a streaming state transition. Those are not the same operation. If the intended hash is FNV-like, the update function should continue from the current state, not reinitialize from `state ^ offset` each time.

Also, the constant named like an FNV offset is not the standard 64-bit FNV offset basis. Standard FNV-1a 64 offset basis is `14695981039346656037`; this code uses `1469598103934665603`, missing a digit. Maybe intentional. If intentional, document it. If not, the hash starts life already concussed.

### 5. No domain separation or length framing

`hashString` hashes UTF-8 bytes. `hashBytes` hashes exact bytes. `hashChar` and `hashGrapheme` also hash UTF-8 bytes. There are no type tags, length prefixes, field separators, constructor tags except the exposed `hashNatTag`, or record/tuple shape delimiters.

For primitive standalone hashing, `Hashable String` may hash canonical UTF-8 bytes. That part is allowed by the spec. But structural hashing needs framing. Without it, derived or manual field hashing can collide structurally:

```text
("a", "bc")
("ab", "c")
```

Both reduce to the byte stream `61 62 63` unless field boundaries are included. The spec’s derived `Hashable` section requires constructor identity and field order semantics. See `Spec.md:19237-19263`.

### 6. Integer and float hashing are platform-endian

The interpreter uses:

```fsharp
BitConverter.GetBytes value
```

for integers and floats:

* `src/Kappa.Compiler/Interpreter.fs:1171-1174`
* `src/Kappa.Compiler/Interpreter.fs:1547-1548`

`BitConverter.GetBytes` is platform-endian. Portable hashing should use a fixed byte order, probably little-endian, explicitly.

### 7. Float hashing and equality likely disagree

Runtime equality for floats uses ordinary `left = right`:

* `src/Kappa.Compiler/Interpreter.fs:405-406`

Hashing uses raw `BitConverter.GetBytes`:

* `src/Kappa.Compiler/Interpreter.fs:1173-1174`

So `0.0` and `-0.0` compare equal under ordinary floating equality but hash differently under raw-bit hashing. Either `Eq Float` must be raw-bit, or `Hashable Float` must canonicalize values according to equality. At present the pieces disagree.

### 8. Backend hash support is mostly nonexistent

`std.hash` is declared in `StandardModules.fs`, but backends do not implement the intrinsics.

IL backend intrinsic typing supports only a small prelude/file/ref/bool/numeric set:

* `src/Kappa.Compiler/IlDotNetBackendInput.fs:655-687`

Unsupported intrinsics produce backend errors:

* `src/Kappa.Compiler/IlDotNetBackendEmit.fs:802-898`

Zig emits:

```fsharp
zig does not yet support intrinsic ...
```

for unknown intrinsics:

* `src/Kappa.Compiler/ZigCcBackendEmit.fs:707`

Hosted runtime rejects non-prelude intrinsic modules:

* `src/Kappa.Compiler/Runtime/Hosted/KappaRuntime.cs:1567-1589`

So hash semantics are not portable across profiles.

---

## Backend consistency problems

### Zig/C strings are NUL-terminated, not length-carrying

The Zig/C runtime represents strings as `const char*`:

* `src/Kappa.Compiler/Runtime/Zig/kappa_runtime.c:52-65`
* `src/Kappa.Compiler/Runtime/Zig/kappa_runtime.c:133-145`

Equality uses `strcmp`:

* `src/Kappa.Compiler/Runtime/Zig/kappa_runtime.c:213-215`
* `src/Kappa.Compiler/Runtime/Zig/kappa_runtime.c:425-426`

Printing uses `fputs`:

* `src/Kappa.Compiler/Runtime/Zig/kappa_runtime.c:461-462`
* `src/Kappa.Compiler/Runtime/Zig/kappa_runtime.c:504-511`

Therefore a valid Kappa string containing `\u{0}` will be truncated in equality/printing. The spec says `String` is valid UTF-8 scalar text; U+0000 is a valid scalar. A NUL-terminated representation is not sufficient unless every operation carries length separately.

### Zig literal lowering collapses distinct text types

Zig literal lowering boxes `String`, `Character`, and `Grapheme` all as `kappa_box_string`; `Byte` becomes an integer:

* `src/Kappa.Compiler/ZigCcBackendSupport.fs:200-215`

That erases the runtime distinction between `UnicodeScalar`, `Grapheme`, and `String`. The runtime has `K_TAG_CHAR`, but literal lowering does not use it.

### Backend lowering does not really resolve standard module intrinsic terms

`StandardModules.toRuntimeModule` creates modules with `IntrinsicTerms` but no `Bindings`:

* `src/Kappa.Compiler/StandardModules.fs:99-119`

`KBackendLowering.tryResolveModuleMember` resolves only runtime `Bindings` and constructors, not `IntrinsicTerms`:

* `src/Kappa.Compiler/KBackendLowering.fs:317-339`

So even before target-specific codegen, standard module intrinsic references are structurally awkward for backend lowering. The interpreter handles `IntrinsicTerms` separately; the backends do not have equivalent support.

---

## Concrete fix order

### P0: fix byte literal decoding

Do not decode byte literals through `tryUnescapeStringContent`.

Implement a byte-literal-specific decoder:

* ordinary ASCII source chars produce their ASCII byte only if `<= 0x7F`;
* common escapes like `\n`, `\t`, `\r`, `\\`, `\'` produce their byte;
* `\xNN` produces raw byte `NN`;
* `\uXXXX` and `\u{...}` produce a byte only if the scalar’s UTF-8 encoding length is exactly 1;
* reject everything else.

This makes `b'\xFF'` valid and `b'ÿ'` / `b'\u{00FF}'` invalid, as the spec requires.

### P0: fix `decodeUtf8`

Change:

```fsharp
let resultError value = constructPreludeValue "Error" [ value ]
```

to:

```fsharp
let resultError value = constructPreludeValue "Err" [ value ]
```

Then decide what `UnicodeDecodeError` actually is. Returning a `StringValue` where the type says `UnicodeDecodeError` is not a real implementation.

### P0: fix hash return values

Replace:

```fsharp
hashIntoState state value |> optionValue |> Result.map Some
```

with explicit success/error handling:

```fsharp
match hashIntoState state value with
| Some hashed -> ok (Some hashed)
| None -> error $"Intrinsic '{builtin.Name}' cannot hash {RuntimeValue.format value}."
```

Do this for the grouped `hashBool`/`hashString`/etc. case and for `hashField`.

### P0: redesign hash state

Split hash initialization from hash update.

At minimum:

```fsharp
let init seed = uint64 seed ^^^ fixedOffset
let update state bytes =
    let mutable s = state
    for b in bytes do
        s <- s ^^^ uint64 b
        s <- s * prime
    s
```

Then:

* `newHashState seed` stores `init seed`;
* `hashBytes` calls `update currentState bytes`;
* `finishHashState` finalizes, possibly with an avalanche;
* `hashWith seed value` is `newHashState seed |> hash value |> finish`.

Use fixed endian encodings for integers/floats.

### P0: decide whether `Hashable` is real

Either implement the spec’s trait:

```kappa
trait Eq a => Hashable a =
    hashInto : (& value : a) -> (1 state : HashState) -> HashState
```

with real members, real instances, and coherent dictionary dispatch,

or explicitly make `Hashable` compiler-magic and update the spec/standard module so it does not pretend to be an ordinary trait.

The current middle path gives you the failure modes of both. Charming, in the same way a bridge made of cheese is charming.

### P1: implement missing `std.unicode` terms or remove them

Either implement:

* `scalars`
* `graphemes`
* `words`
* `sentences`

in `Interpreter.tryCreateIntrinsicTermValue` and `invokeBuiltin`, or stop exporting them until they work.

For `words` and `sentences`, use UAX #29 or clearly mark them non-conforming. ASCII split is not Unicode segmentation.

### P1: fix string ordering

Replace `String.CompareOrdinal` with scalar-order comparison over `Rune` sequences for `String`, `UnicodeScalar`, and `Grapheme`.

Equality can remain exact string equality as long as internal strings are guaranteed valid scalar text.

### P1: pin Unicode behavior

If `unicodeVersion` returns `"15.1.0"`, then grapheme segmentation, normalization, word/sentence boundaries, and related behavior need to use pinned data for 15.1.0.

If behavior is delegated to .NET/ICU, return/document the actual runtime Unicode data version, or say it is implementation-defined. Lying about Unicode versions is how text systems become haunted.

### P1: add real backend representations

Every backend needs:

* `String` as UTF-8 bytes plus length, or a representation that preserves U+0000 and exact scalar sequence semantics;
* `Bytes` as byte array plus length;
* `UnicodeScalar` as scalar value, not host `char`;
* `Grapheme` as validated text with one EGC;
* `Byte` as `uint8`;
* `HashSeed`, `HashState`, and `HashCode` as fixed-width portable values.

Until then, gate `std.unicode`, `std.bytes`, and `std.hash` by backend capability instead of accepting programs and failing late.

---

## Tests to add

These should be small, mean, and unromantic.

```kappa
-- valid
let a : Byte = b'\xFF'
let b : Byte = b'\n'
let c : UnicodeScalar = '\u{1F600}'
let g : Grapheme = g'e\u{301}'

-- invalid
let badByte1 : Byte = b'ÿ'
let badByte2 : Byte = b'\u{00FF}'
let badScalar : UnicodeScalar = 'e\u{301}'
let badGrapheme : Grapheme = g'ab'
```

Add runtime tests for:

```kappa
decodeUtf8 (utf8Bytes "x") == Ok "x"
decodeUtf8 invalidBytes == Err ...
```

once arbitrary `Bytes` construction exists.

Add ordering tests:

```kappa
"\u{E000}" < "\u{1F600}"  -- should be True by scalar order
```

Add hash tests:

```kappa
finishHashState (hashString "a" (newHashState defaultHashSeed))
```

This must produce a `HashCode`, not `Some HashState`.

Add structural hash tests once `Hashable` is real:

```kappa
hashWith seed ("a", "bc") != hashWith seed ("ab", "c")
```

assuming tuple hashing is expected to frame fields.

Add backend tests for strings containing U+0000:

```kappa
printlnString "a\u{0}b"
```

Interpreter and compiled backends should agree exactly.

---

## Bottom line

The compiler currently has **partial interpreter semantics** for Unicode and bytes, and **mostly broken semantics** for hash. The front-end’s strict UTF-8 and scalar literal validation are good foundations. The byte literal decoder, `decodeUtf8` error path, `std.unicode` surface, `Hashable` trait, streaming hash API, and backend representations need correction before these semantics can be called portable or spec-conforming.

Most urgent fixes: byte literal decoder, `decodeUtf8` constructor, hash API returning `Option`, and backend capability gating. Those are not polish items. They are load-bearing beams, and several are presently made of damp cardboard.
