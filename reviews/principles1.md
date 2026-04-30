I did a static pass over the compiler source. I did **not** run the full build or tests because the container has no `dotnet` SDK installed. Naturally, the one tool needed to compile the compiler is absent, because civilization is a thin crust over chaos.

## Overall verdict

The compiler has several hardcoded choices that are defensible as bootstrap machinery, but a few have crossed the line into **semantic special cases**. The main problem is not that strings exist. The problem is that the same semantic facts are repeated across the frontend, elaborator, runtime lowering, interpreter, IL backend, Zig backend, and import checker.

The biggest theme: **the compiler does not have one authoritative model of intrinsics, standard modules, compile-time-only types, backend representations, or implicit prelude behavior.** Instead, it has a confederation of switch statements quietly pretending to be a constitution.

---

## Highest-priority findings

### 1. Implicit prelude constructors are hardcoded and appear broader than the spec

In `src/Kappa.Compiler/Stdlib.fs:23-44`, `FixedPreludeConstructors` includes:

```fsharp
"Reusable"
"OneShot"
"QZero"
"QOne"
"QZeroOrOne"
"QOneOrMore"
"QZeroOrMore"
"QueryMode"
":&"
```

But `Spec.md:911-920` lists the implicit unqualified constructor subset as:

```text
True, False,
None, Some,
Ok, Err,
Nil, (::),
LT, EQ, GT,
refl
```

Then `SurfaceElaboration.fs:3089-3095` imports exactly the implementation list into visible constructors.

That means the implementation appears to grant extra unqualified constructors beyond the normative subset. This is not merely hardcoded. It looks like a **spec mismatch**.

**Why it matters:** user code may compile because `Reusable`, `QZero`, `QueryMode`, or `:&` are magically in scope, even though portable Kappa code should not rely on that.

**Fix:** split this into:

```fsharp
ImplicitPreludeTerms
ImplicitPreludeConstructorsSpecifiedBySpec
QualifiedPreludeExports
```

Then add a regression test asserting the implementation’s implicit constructor list exactly equals the spec subset. Tedious, yes. But less tedious than debugging ghost imports in six months.

---

### 2. Intrinsics are stringly typed and duplicated across the whole compiler

`src/Kappa.Compiler/IntrinsicCatalog.fs` is already trying to be the central authority, but it is not actually authoritative.

Examples:

* Builtin unary names: `IntrinsicCatalog.fs:63-64`
* Builtin binary names: `IntrinsicCatalog.fs:66-76`
* Prelude names available during elaboration: `IntrinsicCatalog.fs:78-99`
* Runtime arities: `IntrinsicCatalog.fs:121-197`
* Result representations: `IntrinsicCatalog.fs:199+`
* Backend emit logic repeats names in:

  * `IlDotNetBackendEmit.fs:850-898`
  * `ZigCcBackendEmit.fs:627-672`
  * `Interpreter.fs:1319-1334`
  * `KBackendLowering.fs:782-803`

The worst bit is `IntrinsicCatalog.fs:196-197`:

```fsharp
| _ ->
    0
```

Unknown intrinsic name? Arity zero. Lovely. Nothing says “robust compiler architecture” like silently classifying the unknown as nullary.

**Why it matters:** adding or changing an intrinsic requires editing several places. If one is missed, the compiler may accept code that one backend cannot lower, or lower it with the wrong shape.

**Fix:** introduce a real intrinsic manifest:

```fsharp
type Intrinsic =
  { ModuleName: ModuleName
    Name: string
    RuntimeArity: int
    Parameters: BackendRep list
    Result: BackendRep
    Availability: IntrinsicAvailability
    Effects: EffectInfo
    InterpreterImpl: InterpreterIntrinsic option
    IlImpl: IlIntrinsic option
    ZigImpl: ZigIntrinsic option }
```

Then generate lookup tables from that manifest. Unknown intrinsic should be an error, not “zero arguments, probably fine, ship it.”

---

### 3. Synthetic standard modules are split across incompatible inventories

`src/Kappa.Compiler/StandardModules.fs:86-90` defines only:

```fsharp
[ unicodeModule; bytesModule; hashModule ]
```

But `src/Kappa.Compiler/CompilationFrontend.fs:811-905` separately defines inventories for:

```text
std.gradual
std.ffi
std.ffi.c
std.bridge
std.atomic
std.supervisor
std.hash
```

Then `CompilationFrontend.fs:906-924` merges `StandardModules.all` while explicitly filtering out several hardcoded module names.

This creates a split brain:

* Import validation knows about more standard modules.
* Surface elaboration builds surface info from `StandardModules.byName`.
* Runtime compilation appends `StandardModules.all`.

**Why it matters:** a module can be known to import validation but absent from elaboration/runtime machinery. That is exactly the kind of maintenance trap that smiles politely while holding a knife.

**Fix:** all standard modules should be described by one descriptor format containing:

```fsharp
module name
types
constructors
terms
traits
instances
runtime availability
backend support status
```

Frontend import validation, elaboration, runtime lowering, and backends should consume the same data.

---

### 4. `std.hash.Hashable` has compiler-only magic instead of real instance machinery

`SurfaceElaboration.fs:5294-5320` hardcodes which types count as intrinsically `Hashable`:

```fsharp
Unit
Bool
Byte
Bytes
UnicodeScalar
Grapheme
String
Int
Integer
Float
Double
Ordering
Option
List
Array
Result
```

Then `SurfaceElaboration.fs:5473-5501` only enables this fallback for:

```fsharp
std.hash.hashField
std.hash.hashWith
```

And `SurfaceElaboration.fs:18645-18654` erases implicit dictionary arguments only for the same two names.

That is not “trait resolution.” That is a VIP door in the side of the type system.

**Why it matters:** `Hashable` looks like a trait, but these functions are special-cased by name. Other functions requiring `Hashable a` do not get the same treatment unless they are exactly `hashField` or `hashWith`.

**Fix options:**

1. Generate real builtin instances for these types and let normal instance resolution handle them.
2. Or represent builtin dictionaries explicitly in the intrinsic manifest.
3. But do not hide this in call preparation and runtime argument erasure.

---

### 5. File/data intrinsics are backend stubs masquerading as semantics

In the interpreter:

```fsharp
Interpreter.fs:1319-1320
openFile -> "<file:{value}>"

Interpreter.fs:1327-1328
readData -> "chunk"

Interpreter.fs:1333-1334
primitiveCloseFile -> writes "closed"
```

In Zig:

```fsharp
ZigCcBackendEmit.fs:635-640
openFile -> "<file:data.txt>"

ZigCcBackendEmit.fs:646-652
readData -> "chunk"

ZigCcBackendEmit.fs:658-669
close -> "closed"
```

In IL:

```fsharp
IlDotNetBackendEmit.fs:864-873
openFile/readData/closeFile emit synthetic handle/"chunk"/"closed"
```

This may be acceptable in fixtures, but as compiler/runtime behavior it is extremely unprincipled.

**Why it matters:** program behavior depends on fake I/O. If this is intentional test-mode behavior, it needs to be gated. If it is production semantics, then the language has invented files that contain only `"chunk"`, which is at least mercifully simpler than POSIX.

**Fix:** create a runtime capability layer:

```fsharp
type RuntimeIoProvider =
  { OpenFile: string -> RuntimeResult<FileHandle>
    ReadData: FileHandle -> RuntimeResult<Bytes/String>
    CloseFile: FileHandle -> RuntimeResult<Unit> }
```

Then select a fixture provider only in tests.

---

### 6. Backend representation is inferred by parsing rendered type text

`KBackendLowering.fs:42-64` does this:

```fsharp
text.Replace("(", " ")
    .Replace(")", " ")
    .Split(...)
|> Array.tryHead
```

Then maps the first token:

```fsharp
"Int" -> BackendRepInt64
"Float" -> BackendRepFloat64
"Bool" -> BackendRepBoolean
"IO" -> BackendRepIOAction
"Ref" -> BackendRepRef(...)
```

This is brittle. It ignores resolved type identity, module qualification, aliases, applications, and pretty-printer changes.

**Why it matters:** if type text changes, backend behavior changes. If a user-defined type has the same last/head name, it may be misclassified. Text is a display format, not a semantic API. This is compiler architecture, not a horoscope.

**Fix:** carry resolved type identities and representation metadata through the typed IR. Backend representation should be computed from typed nodes, not reparsed strings.

---

### 7. Compile-time-only type erasure is repeated by hardcoded names

`KRuntimeLowering.fs` repeats lists of compile-time-only type names in multiple places:

* `KRuntimeLowering.fs:56-72`
* `KRuntimeLowering.fs:184-209`
* `KRuntimeLowering.fs:591-612`

Names include:

```fsharp
Type
Universe
Constraint
Quantity
Region
RecRow
VarRow
EffRow
Label
EffLabel
Syntax
Code
std.prelude.Syntax
std.prelude.Code
```

**Why it matters:** this uses text names as semantic categories. If aliases, qualification, shadowing, or future modules interact with these names, the erasure behavior can drift or misfire.

**Fix:** assign compile-time-only-ness as a property of resolved type constructors. Then runtime lowering asks:

```fsharp
typeInfo.RuntimePresence = Erased
```

rather than checking whether the name happens to be `"Syntax"`.

---

### 8. Query semantics use last-segment name matching

`QuerySemantics.fs` classifies query-related types and constructors by the final name segment:

```fsharp
QuerySemantics.fs:44-57
Reusable, OneShot, QZero, QOne, ...

QuerySemantics.fs:124-158
Query, OnceQuery, OptionalQuery, NonEmptyQuery, SingletonQuery, QueryCore

QuerySemantics.fs:175-196
List, Array, Set, Option
```

This is very convenient. So is leaving the front door open because keys are fiddly.

**Why it matters:** semantics are tied to spelling, not identity. A type named `foo.Option` or `my.Query` risks being treated as a builtin query source if normalization leaves the last segment looking right.

**Fix:** use resolved builtin type IDs:

```fsharp
BuiltinType.Query
BuiltinType.Option
BuiltinType.List
BuiltinType.Array
```

Do not use `lastSegment`.

---

### 9. Numeric literal handling bakes in runtime ranges and builtin target names

`SurfaceElaboration.fs:5133-5160` hardcodes builtin numeric targets:

```fsharp
Int
Integer
Nat
Float
Double
Real
```

`SurfaceElaboration.fs:5162-5165` hardcodes trait names:

```fsharp
FromInteger
FromFloat
```

`SurfaceElaboration.fs:5256-5267` lowers integers only if they fit `Int64`, and real literals through `Double.TryParse`.

Then `SurfaceElaboration.fs:18709-18712` does this fallback:

```fsharp
tryLowerNumericLiteralForRuntime literal
|> Option.defaultValue (KCoreLiteral LiteralValue.Unit)
```

That last fallback is especially suspicious. A numeric literal that cannot lower should not quietly become `Unit`, even if later diagnostics catch it. It is the kind of thing that creates haunted IR.

**Why it matters:** `Integer`, `Nat`, and `Real` appear to exist at the surface level, but runtime lowering is int64/double-centric unless trait machinery takes over cleanly.

**Fix:** represent numeric literals as arbitrary-precision literal nodes until backend selection. Only lower to fixed-width representations after target type resolution.

---

### 10. Backend profile strings are scattered

Backend/profile handling appears in several places:

* `Stdlib.fs:114-130`
* `Stdlib.fs:174-182`
* `Compilation.fs:12-17`
* `CompilationCheckpoints.fs:111-118`
* `HostBindings.fs:74-77`
* `Program.fs:574-593`

Strings include:

```text
interpreter
dotnet
dotnet-il
zig
zigcc
```

**Why it matters:** adding a backend means updating several switches. Missing one gives you a backend that half exists, which is worse than not existing.

**Fix:** define:

```fsharp
type BackendProfile =
  | Interpreter
  | DotNet
  | DotNetIl
  | Zig
```

Parse once at the boundary. Everything internal should use the discriminated union.

---

### 11. `Float = Double` exists both in the prelude and inside type-signature normalization

Prelude:

```text
src/Kappa.Compiler/Stdlib/std/prelude.kp:120
type Float = Double
```

Compiler builtin context:

```fsharp
TypeSignatures.fs:149-152
Float = Double
std.prelude.Float = std.prelude.Double
```

**Why it matters:** this duplicates language facts. If the prelude changes, the type checker may still believe the old truth.

**Fix:** either make `Float = Double` a compiler primitive and generate the prelude declaration from it, or remove the compiler builtin and rely on the prelude alias. One boss per fact. Even feudalism understood this.

---

### 12. Module case-fold collision rule appears unimplemented

The spec says implementations must reject source files whose path-derived module names are equal after ASCII lowercase but differ in case:

```text
Spec.md:149-155
```

I found module-name inference in `CompilationFrontend.fs:33-61`, and validation in `CompilationFrontend.fs:84-118`, but I did not find an obvious pass that groups effective module names by lowercase ASCII form and reports all colliding files.

Confidence: **medium**, because this is based on static search, not exhaustive symbolic execution.

**Why it matters:** this is a portability rule. Without it, a package may compile on one filesystem and explode on another.

**Fix:** after resolving effective module names, group by:

```fsharp
segments |> List.map asciiLower |> String.concat "."
```

If a group contains multiple distinct spellings, emit a diagnostic listing all files.

---

## Defensible hardcoding

Not all hardcoding is bad. Some is language bootstrap.

These are mostly acceptable, provided they are documented and tested:

### Prelude fixity bootstrapping

The parser reads leading prelude fixity declarations. That is a reasonable bootstrap mechanism, though it should be treated as part of the language’s bootstrap contract.

### Syntax keywords and parser desugaring

Things like quantity prefixes, `lazy`, `thunk`, query syntax, and operator parsing have to be recognized somewhere. The problem is not that parser syntax is hardcoded. The problem starts when parser desugaring depends on library names without a stable semantic identity.

### FNV/hash constants

`UnicodeText.fs` defines FNV-style constants, and `Interpreter.fs:363` uses `1469598103934665603L` as the default hash seed. Constants are fine when they are documented as the algorithm. The duplication is the issue.

---

## Recommended cleanup order

1. **Create a single intrinsic manifest** and make `IntrinsicCatalog`, interpreter, IL backend, Zig backend, and backend lowering consume it.
2. **Fix implicit prelude constructor import** to match `Spec.md:911-920`, unless the spec is intentionally stale. If the spec is stale, update the spec first, not the vibes.
3. **Unify standard module descriptions** so import validation, elaboration, and runtime all see the same module inventory.
4. **Replace name-based semantic checks with resolved identities**, especially for query types, compile-time-only types, `IO`, `Ref`, `Option`, `List`, and `Hashable`.
5. **Remove the `std.hash` special case** by generating real builtin instances or explicit intrinsic dictionaries.
6. **Stop parsing type text for backend representation.** Carry representation metadata through typed IR.
7. **Gate fake I/O behind a test runtime provider.**
8. **Add regression tests for spec-sensitive behavior:**

   * implicit prelude constructor list
   * module case-fold collisions
   * user-defined `Option`, `Query`, `Hashable`, `IO`, `Ref` names
   * unknown intrinsic rejection
   * all standard module inventories agree
   * every intrinsic has arity, result representation, and backend support status

## Final assessment

The compiler is not riddled with random hardcoding everywhere. It is more specific than that: the hardcoding clusters around **intrinsics, standard modules, prelude import policy, type identity, and backend representation**.

Those are exactly the places where hardcoding is most dangerous, because they define the contract between the language and the implementation. Right now, too many of those contracts are encoded as scattered strings and special cases. That works until it doesn’t, which is also the motto on most software tombstones.
