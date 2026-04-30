I reviewed the uploaded compiler source with the Zig backend in mind. The short version, since apparently we’re letting C cosplay as Zig now: the backend works for the narrow demo path, mostly integers, lists, simple matches, refs, and basic IO, but it is not semantically aligned with the interpreter or the .NET backend.

I could not run the full suite here because this sandbox has neither `dotnet` nor `zig` installed. I did static review, inspected the generated C/runtime path, and compiled a minimal C runtime skeleton with `cc -std=c11`; the runtime template is at least syntactically plausible C. The historical `.trx` files in the archive show prior passing runs, but they are not a live run and they do not cover the nastier cases below.

## Main findings

### 1. Float arithmetic is broken in the Zig backend

**Severity: high**

`KBackendLowering.fs` correctly infers `BackendRepFloat64` for float arithmetic when either side is float:

`src/Kappa.Compiler/KBackendLowering.fs:148-155`

But `ZigCcBackendEmit.fs` maps every arithmetic operator to integer helpers:

`src/Kappa.Compiler/ZigCcBackendEmit.fs:494-505`

```fsharp
| "+" -> return! binaryIntCall "add" "kappa_int_add"
| "-" -> return! binaryIntCall "sub" "kappa_int_subtract"
| "*" -> return! binaryIntCall "mul" "kappa_int_multiply"
| "/" -> return! binaryIntCall "div" "kappa_int_divide"
...
| "negate" -> return! unaryIntCall "neg" "kappa_int_negate"
```

The runtime helpers then call `kappa_expect_int`:

`src/Kappa.Compiler/Runtime/Zig/kappa_runtime.c:339-384`

So a program like:

```kappa
module main
let result = 1.5 + 2.25
```

will likely emit C successfully, then panic at runtime with “expected Int value”. Elegant, in the same way a trapdoor is elegant.

**Fix:** make `emitIntrinsicCall` representation-aware. It currently receives only the intrinsic name and arguments, losing the `BackendCall` convention/representations. Pass the `KBackendCallingConvention` through and choose `kappa_float_add`, `kappa_float_subtract`, etc. Add runtime helpers using `kappa_expect_float`.

Also fix float comparisons. `<`, `<=`, `>`, `>=` are also hardwired to integer helpers.

---

### 2. `&&` and `||` are not correctly supported

**Severity: high**

The intrinsic catalog exposes `&&` and `||` as builtin binary operators:

`src/Kappa.Compiler/IntrinsicCatalog.fs:66-70`

But the Zig emitter supports `"and"` and `"or"`, not `"&&"` and `"||"`:

`src/Kappa.Compiler/ZigCcBackendEmit.fs:506-508`

```fsharp
| "not" -> return! unaryBoolCall "not" "kappa_bool_not"
| "and" -> return! binaryBoolCall "and" "kappa_bool_and"
| "or" -> return! binaryBoolCall "or" "kappa_bool_or"
```

Meanwhile `KBackendLowering.fs` lowers both operands before creating a call:

`src/Kappa.Compiler/KBackendLowering.fs:1129-1155`

That means there are two problems:

1. `&&` and `||` likely fail emission as unsupported intrinsics.
2. If you “fix” them by mapping to `kappa_bool_and` and `kappa_bool_or`, you still lose short-circuiting because both operands were already lowered/evaluated.

The interpreter and .NET backend explicitly preserve short-circuit behavior. The Zig backend does not.

**Minimal repro:**

```kappa
module main
let boom = 1 / 0
let result = False && (boom == 0)
```

Expected: `False`, no division.

Likely Zig backend behavior: emit error now, or runtime panic after the naive mapping.

**Fix:** lower `a && b` to:

```text
if a then b else False
```

and `a || b` to:

```text
if a then True else b
```

Do this in KBackend lowering, not in the C runtime.

---

### 3. Zig advertises many intrinsics it does not implement

**Severity: high**

`Stdlib.fs` gives the Zig profile the full prelude intrinsic set:

`src/Kappa.Compiler/Stdlib.fs:122-128`

```fsharp
| "interpreter"
| "dotnet"
| "dotnet-il"
| "zig" ->
    preludeIntrinsicSet
```

But the Zig emitter implements only a small subset:

`src/Kappa.Compiler/ZigCcBackendEmit.fs:494-707`

Missing or mismatched examples include:

* `printlnString`
* `runPure`
* `compare`
* `floatEq`
* Unicode module functions like `byteLength`, `normalize`, `graphemeCount`
* Hash module functions like `hashString`, `hashWith`
* Code-generation intrinsics like `closeCode`, `genlet`, `runCode`

The standard modules expose Unicode and hash intrinsics to runtime modules regardless of backend:

`src/Kappa.Compiler/StandardModules.fs:26-84`
`src/Kappa.Compiler/Stdlib.fs:152-172`

`printlnString` is the simplest smoking crater. The prelude declares it:

`src/Kappa.Compiler/Stdlib/std/prelude.kp:321`

```kappa
expect term printlnString : String -> UIO Unit
```

The interpreter implements it, but Zig does not.

**Minimal repro:**

```kappa
module main
let main : IO Unit = do
    printlnString "hello"
```

Expected: print with newline.

Likely Zig backend behavior: emission error, unsupported intrinsic.

**Fix:** either implement the missing intrinsics or stop exposing them under the `zig` backend profile. If Zig is intentionally a milestone subset, the compiler should say so at type/checkpoint time instead of letting users wander into the swamp with a candle.

---

### 4. Trait dispatch generation is fragile and can miss required functions

**Severity: high**

Trait calls emit calls to generated dispatch functions:

`src/Kappa.Compiler/ZigCcBackendEmit.fs:370-404`

But `emitTraitDispatchFunctions` does not derive dispatch entries from KRuntimeIR or KBackendIR. It scans raw source documents for `InstanceDeclarationNode`:

`src/Kappa.Compiler/ZigCcBackendRuntime.fs:7-35`

That is a bad source of truth for a backend. The compiler already has lowered trait instance data in `KRuntimeIR`:

`src/Kappa.Compiler/KRuntimeIR.fs:88-92`

```fsharp
type KRuntimeTraitInstance =
    { TraitName: string
      InstanceKey: string
      HeadTypeTexts: string list
      MemberBindings: (string * string) list }
```

Current risks:

* Instances generated or rewritten by earlier phases can be missed.
* Imported/runtime instances can be missed.
* A trait call can reference a dispatch function that was never emitted, causing C compilation failure.
* Dispatch matching checks only `module_name` and `instance_key`, not `trait_name`, even though dictionaries store `trait_name`.

The missing trait-name check is here:

`src/Kappa.Compiler/ZigCcBackendRuntime.fs:55-65`

```fsharp
if (strcmp(dictionary->as.dictionary_value.module_name, "...") == 0
    && strcmp(dictionary->as.dictionary_value.instance_key, "...") == 0)
```

The runtime dictionary has `trait_name` available:

`src/Kappa.Compiler/Runtime/Zig/kappa_runtime.c:315-320`

**Fix:** generate dispatch tables from `workspace.KRuntimeIR` or `workspace.KBackendIR`, not raw syntax. Match on `trait_name`, `module_name`, and `instance_key`. Also emit a dispatch function for every trait/member pair that appears in `BackendTraitCall`, even if there are zero known instances, so failures become controlled diagnostics or runtime panics instead of missing C symbols.

---

### 5. First-class intrinsic values are mostly unsupported

**Severity: medium-high**

The interpreter supports builtin/intrinsic functions as values. The Zig emitter only allows `True` and `False` as first-class intrinsics:

`src/Kappa.Compiler/ZigCcBackendEmit.fs:72-84`

```fsharp
| BackendIntrinsicName(_, bindingName, _) ->
    match bindingName with
    | "True" -> ...
    | "False" -> ...
    | other ->
        Result.Error $"zig does not yet support using intrinsic '{other}' as a first-class value."
```

So this shape is likely broken:

```kappa
module main
let f = printString
let main : IO Unit = do
    f "hello"
```

Same class of problem applies to first-class builtin operators like `(+)`, depending on how the frontend represents them. Constructors are mostly handled by KBackend lowering via synthesized closures, but the emitter-level unsupported branch is still there for malformed or alternate paths.

**Fix:** generate closure wrappers for intrinsic functions, same as synthesized constructor closures. Or reject such programs explicitly under the Zig profile before backend emission.

---

### 6. C symbol generation has real collision risks

**Severity: medium-high**

The sanitizer is not injective:

`src/Kappa.Compiler/ZigCcBackendSupport.fs:53-69`

It leaves underscores untouched and encodes non-alnum characters as `_uXXXX`.

That means these collide:

```text
"a.b"      -> "a_u002eb"
"a_u002eb" -> "a_u002eb"
```

Function names also concatenate sanitized module and binding names with `_`:

`src/Kappa.Compiler/ZigCcBackendSupport.fs:92-96`

```fsharp
kappa_module_{sanitizeIdentifier moduleName}_{sanitizeIdentifier bindingName}
```

So these collide:

```text
module a_b, binding c   -> kappa_module_a_b_c
module a,   binding b_c -> kappa_module_a_b_c
```

Type IDs have the same issue.

**Fix:** use length-prefixed components or a stable hash suffix. For example:

```text
kappa_module_m3_a_b_b1_c
```

or:

```text
kappa_module_<hash>_<debug_slug>
```

Also record generated names in a table and fail fast on collision. Letting the C compiler discover your naming scheme has become a haunted doll is not ideal.

---

### 7. Closure environment typedefs can collide across modules

**Severity: medium-high**

Closure environment C type names are derived only from the environment layout name:

`src/Kappa.Compiler/ZigCcBackendEmit.fs:98-140`

```fsharp
let environmentTypeName = sanitizeIdentifier environmentLayout
```

Constructor closure environment layout names are created from only the constructor name:

`src/Kappa.Compiler/KBackendLowering.fs:536-538`
`src/Kappa.Compiler/KBackendLowering.fs:577-580`

```fsharp
let freshEnvironmentLayoutName prefix =
    nextEnvironmentLayoutId <- nextEnvironmentLayoutId + 1
    $"{prefix}$env{nextEnvironmentLayoutId}"
...
let environmentLayoutName = freshEnvironmentLayoutName constructorInfo.Name
```

`nextEnvironmentLayoutId` resets per module. So two modules with a partially applied constructor named `Box` can both generate:

```c
typedef struct Box_u0024env1 { ... } Box_u0024env1;
```

One C translation unit, duplicate typedef. Humanity invented namespaces and then, bravely, forgot them.

**Fix:** include the module name in environment layout names, or let the Zig/C backend assign globally unique environment type names independent of IR layout names.

---

### 8. C string escaping is unsafe for control characters

**Severity: medium**

`cStringLiteral` emits control characters using `\xNN`:

`src/Kappa.Compiler/ZigCcBackendSupport.fs:70-80`

```fsharp
| ch when Char.IsControl(ch) -> $"\\x{int ch:x2}"
```

In C, `\x` escapes greedily consume following hex digits. So a source string containing control character `0x01` followed by `A` becomes:

```c
"\x01A"
```

C interprets that as one hex escape, not `0x01` plus `'A'`.

There is also a broader runtime issue: strings are stored as NUL-terminated `const char*`, so embedded `\0` will truncate behavior in `strlen`, `strcmp`, printing, etc.

**Fix:** use octal escapes with fixed width, split string literals after hex escapes, or store strings with explicit byte length in `KValue`.

---

### 9. Equality and ordering diverge from the interpreter

**Severity: medium**

The interpreter recursively compares constructed values:

`src/Kappa.Compiler/Interpreter.fs:401-434`

The Zig runtime only compares primitive scalar tags and returns false for data values unless the pointers are identical:

`src/Kappa.Compiler/Runtime/Zig/kappa_runtime.c:405-434`

```c
case K_TAG_INT:
case K_TAG_FLOAT:
case K_TAG_BOOL:
case K_TAG_STRING:
case K_TAG_CHAR:
case K_TAG_UNIT:
    ...
default:
    return kappa_box_bool(0);
```

If the language permits `==` on constructed data, Zig gives different answers from the interpreter.

Similarly, interpreter comparisons support ints, floats, bools, strings, chars, graphemes, bytes, and some hash values:

`src/Kappa.Compiler/Interpreter.fs:283-294`

Zig comparisons are integer-only:

`src/Kappa.Compiler/ZigCcBackendEmit.fs:501-504`

**Fix:** either implement structural equality and comparison in the Zig runtime or restrict these operators before backend emission. The latter is less impressive, but at least it is honest.

---

### 10. The `is` operator is half-lowered but not implemented

**Severity: medium**

KBackend lowering treats `"is"` as a boolean-producing binary operator:

`src/Kappa.Compiler/KBackendLowering.fs:148-149`
`src/Kappa.Compiler/KBackendLowering.fs:1135-1145`

The interpreter implements it:

`src/Kappa.Compiler/Interpreter.fs:497-502`

But `IntrinsicCatalog.builtinBinaryOperatorNames` does not include `"is"`:

`src/Kappa.Compiler/IntrinsicCatalog.fs:66-70`

And the Zig emitter has no case for it.

**Fix:** decide whether `is` is a real runtime operator. If yes, add it to the intrinsic catalog and implement it in Zig. If no, remove it from KBackend lowering.

---

### 11. Prefixed/interpolated strings are explicitly unsupported in Zig

**Severity: medium**

The backend rejects `BackendPrefixedString`:

`src/Kappa.Compiler/ZigCcBackendEmit.fs:37-38`

```fsharp
Result.Error(sprintf "zig does not support prefixed string backend expression '%s\"...\"' yet." prefix)
```

This is acceptable only if the Zig profile excludes features that lower to `BackendPrefixedString`. Right now the broader frontend/compiler appears happy to produce those nodes.

**Fix:** implement prefixed string rendering in Zig or reject such programs earlier with a backend-profile diagnostic.

---

### 12. `KAPPA_ZIG_EXE=zig` fails even though unset uses PATH

**Severity: low**

CLI resolution accepts `"zig"` from PATH only when `KAPPA_ZIG_EXE` is unset:

`src/Kappa.Compiler.Cli/Program.fs:436-447`

If `KAPPA_ZIG_EXE=zig`, it checks `File.Exists("zig")`, which fails unless there happens to be a local file named `zig`.

**Fix:** if the configured value has no directory separator, treat it like a command name and let `ProcessStartInfo` resolve it through PATH. Or explicitly search PATH.

---

### 13. Zig test coverage is too narrow

**Severity: medium**

Current Zig-specific tests mostly cover recursive lists, basic CLI execution, and one IO entry path:

`tests/Kappa.Compiler.Tests/ZigBackendTests.fs:10-121`

They do not cover:

* Float arithmetic
* Float/string/bool comparisons
* `&&` / `||` short-circuiting
* `printlnString`
* Unicode/hash standard modules
* First-class intrinsics
* Name collisions
* Partial constructor closures across modules
* Structural equality
* Trait dispatch failure modes

Also, this uploaded archive references repo-local Zig bootstrap scripts:

`tests/Kappa.Compiler.Tests/HarnessSupport.fs:62-80`

but the `scripts/ensure-zig.sh` and `scripts/ensure-zig.ps1` files are not present in the zip. That may be an archive omission, but in this source bundle those tests would fail before reaching the backend.

## Tests I would add immediately

These are the ones most likely to catch actual backend regressions instead of merely confirming the compiler can print `72` again, which is comforting but not exactly a moon landing.

```kappa
module main
let result = 1.5 + 2.25
```

Expected Zig run result: `3.75`.

```kappa
module main
let boom = 1 / 0
let result = False && (boom == 0)
```

Expected: `False`, no panic.

```kappa
module main
let boom = 1 / 0
let result = True || (boom == 0)
```

Expected: `True`, no panic.

```kappa
module main
let main : IO Unit = do
    printlnString "hello"
```

Expected: `hello` plus newline.

```kappa
module main
data Pair = Pair Int Int
let result = (Pair 1 2) == (Pair 1 2)
```

Expected should match interpreter semantics. If equality on data is not meant to be supported, this should be rejected before Zig emission.

For symbol collisions, create two modules whose emitted names collide:

```kappa
module a_b
let c = 1
```

```kappa
module a
let b_c = 2
```

Then force both into the generated artifact.

For constructor closure environment collisions:

```kappa
module m1
data Box a = Box a
let mk = Box
```

```kappa
module m2
data Box a = Box a
let mk = Box
```

Force both `mk` values into emitted code.

## Fix order

I would fix these in this order:

1. **Backend capability reporting**, so Zig stops claiming it supports intrinsics it cannot emit.
2. **Short-circuit lowering**, because the semantic bug is fundamental.
3. **Representation-aware numeric emission**, especially floats and comparisons.
4. **Trait dispatch generation from IR**, not raw syntax.
5. **Global C symbol/name hygiene**, including environment typedef names.
6. **String literal escaping**, because C’s `\x` rules are a small, sharp rake left in the grass.
7. **Equality/comparison semantics**, either implement or reject consistently.

The backend is not hopeless. It is a useful milestone backend. But right now it is over-advertising its abilities, and the most dangerous failures are not syntax failures, they are semantic mismatches: programs that typecheck and emit C, then behave differently from the interpreter or .NET backend.
