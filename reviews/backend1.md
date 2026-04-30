I reviewed the backend source against `Spec.md`, focusing on §17.4–§17.14 plus the runtime semantics that the backend is required to preserve. I tried to run the suite, but this environment has no .NET SDK installed:

```text
cd /mnt/data/kappa_review && dotnet test Kappa.Compiler.sln -v minimal
bash: line 1: dotnet: command not found
```

So this is a static/source-level review. The structural findings are high confidence; runtime behavior claims are high confidence where the emitter path is explicit, and medium confidence where they depend on frontend reachability.

## Verdict

The backend is **not spec-conformant** as a `zig`, `dotnet`, or general KBackendIR backend. It is a useful milestone backend for a narrow executable subset: simple scalar values, simple data constructors/matches, refs, a few IO-ish intrinsics, some closures on the Zig path, and a narrow `using`/cleanup path.

Against the actual spec, however, it is missing the central runtime model: capability profiles, full representation selection, fibers, timers, promises, STM, handlers, resumptions, stable runtime layout guarantees, complete intrinsic enforcement, and target lowering legality. Humanity once again built a bridge by painting “bridge” on a plank.

The useful label is: **prototype backend checkpoint, not a conforming backend contract**.

---

## What is partially implemented correctly

### 1. There is a real `KBackendIR` checkpoint

`KBackendIR.fs` defines backend-neutral functions, representations, closures, calls, matches, constructor layouts, environment layouts, and a structured-expression control form. See `src/Kappa.Compiler/KBackendIR.fs:4-172`.

This partially matches the spec’s requirement that KBackendIR be the final backend-independent representation after runtime erasure and representation selection. The spec definition is much broader, though: it explicitly requires runtime structures for fibers, promises, timer queues, TVars, STM journals, handlers, resumptions, cleanup scopes, and error propagation. See `Spec.md:25077-25122`.

### 2. There is some runtime erasure and arity lowering

`KRuntimeLowering.fs` filters compile-time-only parameters and lowers core terms to runtime terms. Notably:

* Runtime parameter filtering exists at `src/Kappa.Compiler/KRuntimeLowering.fs:180-218`.
* Intrinsic signatures are filtered for compile-time-only parameters at `src/Kappa.Compiler/KRuntimeLowering.fs:743-756`.
* Term bindings drop compile-time-only bodies and parameters at `src/Kappa.Compiler/KRuntimeLowering.fs:802-841`.

This partially satisfies the spec’s calling-convention requirement that quantity-`0` and erased implicit parameters be absent at KBackendIR boundaries. See `Spec.md:25339-25353`.

Caveat: the implementation is still heuristic in places. For example, call-argument erasure only uses the runtime mask for unqualified direct names in `KCoreAppSpine`, at `src/Kappa.Compiler/KRuntimeLowering.fs:522-536`. Qualified/global/function-valued calls may escape that path. Confidence: medium, because this depends on how earlier phases normalize names.

### 3. There is a user-invokable backend verifier

The CLI exposes `--verify` at `src/Kappa.Compiler.Cli/Program.fs:99-104`, and invokes checkpoint verification at `src/Kappa.Compiler.Cli/Program.fs:536-544`.

The verifier recognizes `KBackendIR` at `src/Kappa.Compiler/CheckpointVerification.fs:839-850`. This satisfies the spec’s requirement that the KBackendIR verifier be user-invokable. See `Spec.md:25257-25289`.

The verifier is still much too shallow for full conformance, but the plumbing exists.

### 4. KBackendIR dumps are graph-ish

`CompilationDump.fs` emits JSON/S-expression dumps for KBackendIR and includes function nodes, expression nodes, edges, layouts, calling-convention facts, and provenance. The relevant dump logic lives around `src/Kappa.Compiler/CompilationDump.fs:452-740`.

This partially satisfies the spec’s graph-capable dump requirement at `Spec.md:25257-25289`.

Again, partial is the operative word. The dump cannot expose fibers, STM journals, handler frames, resumption objects, cleanup scopes, or atomic memory orders because the IR does not model them explicitly.

### 5. `using`/cleanup is not entirely ignored

This deserves nuance. The backend does preserve one cleanup shape:

* `KRuntimeScheduleExit` lowers to a synthetic `BackendLet` binding named `__kappa_scope_result`, followed by cleanup and result return. See `src/Kappa.Compiler/KBackendLowering.fs:1037-1055`.
* The Zig emitter recognizes exactly that synthetic pattern and emits a `setjmp`/panic-frame cleanup path. See `src/Kappa.Compiler/ZigCcBackendEmit.fs:243-297`.
* The CLR path reconstructs `KRuntimeScheduleExit` from the synthetic let-pattern at `src/Kappa.Compiler/ClrAssemblyLowering.fs:33-35`, then emits a `finally` block at `src/Kappa.Compiler/IlDotNetBackendEmit.fs:1313-1329`.

So the issue is **not** “cleanup never runs.” The issue is that cleanup semantics are encoded as a magic variable-name convention instead of explicit KBackendIR cleanup/error-propagation structure. That is brittle, under-specified, and not enough for the spec’s backend obligations. See `Spec.md:25110-25122` and `Spec.md:25484-25496`.

---

## Major non-conformances

### 1. KBackendIR is far too small for the spec

The spec says KBackendIR must contain explicit runtime constructs for:

* heap allocation and field access,
* mutable cells,
* fibers,
* fiber IDs and labels,
* supervision scopes,
* monitor handles,
* promises,
* timers,
* TVars,
* STM journals,
* handler frames,
* resumption objects,
* cleanup scopes,
* error propagation.

See `Spec.md:25112-25122`.

The actual `KBackendExpression` union contains literals, names, closures, `if`, `match`, `execute`, `let`, `sequence`, `while`, calls, dictionary/trait calls, data construction, and prefixed strings. See `src/Kappa.Compiler/KBackendIR.fs:44-105`.

That is nowhere near the runtime surface required by §17.4. It is an expression IR with some representation annotations, not the full runtime IR the spec describes.

**Severity:** critical.
**Confidence:** high.

---

### 2. Effects, handlers, and resumptions are not lowered

`KRuntimeIR` still has effect constructs:

* `KRuntimeEffectLabel`
* `KRuntimeEffectOperation`
* `KRuntimeHandle`

See `src/Kappa.Compiler/KRuntimeIR.fs:13-26`.

`KRuntimeLowering.fs` mostly copies effect constructs from KCore into KRuntimeIR rather than lowering them to backend runtime structures. See `src/Kappa.Compiler/KRuntimeLowering.fs:434-443` and `src/Kappa.Compiler/KRuntimeLowering.fs:477-484`.

Then `KBackendLowering.fs` simply rejects them:

```fsharp
| KRuntimeEffectLabel _
| KRuntimeEffectOperation _
| KRuntimeHandle _ ->
    Result.Error "KBackendIR lowering does not support effect handlers yet."
```

See `src/Kappa.Compiler/KBackendLowering.fs:891-894`.

Rejecting unsupported programs is better than silently compiling garbage, a small mercy in a profession built on both. But the spec requires a backend capability model and precise rejection based on capability needs. It also states that absence of `rt-multishot-effects` does not disable effect handlers generally. See `Spec.md:26938-26956`.

**Severity:** critical.
**Confidence:** high.

---

### 3. Runtime capability profiles are absent

The spec requires every backend profile to declare a runtime capability set: `rt-core`, `rt-parallel`, `rt-shared-stm`, `rt-blocking`, `rt-atomics`, and `rt-multishot-effects`. See `Spec.md:26882-26957`.

The implementation has no real capability model. `Stdlib.BackendIntrinsicSet` tracks names of types, traits, prelude terms, and runtime terms, but not backend runtime capabilities. See `src/Kappa.Compiler/Stdlib.fs:6-13`.

Worse, `dotnet`, `dotnet-il`, and `zig` all receive the same prelude intrinsic set:

```fsharp
| "interpreter"
| "dotnet"
| "dotnet-il"
| "zig" ->
    preludeIntrinsicSet
```

See `src/Kappa.Compiler/Stdlib.fs:122-130`.

But the prelude expects runtime features such as `fork`, `await`, `interrupt`, `sleepFor`, `race`, `atomically`, `newTVar`, `readTVar`, `writeTVar`, and `retry`. See `src/Kappa.Compiler/Stdlib/std/prelude.kp:260-308`.

The target emitters do not implement that surface. This means the compiler currently confuses “name is known to the prelude” with “the backend has a runtime implementation.” That is the sort of administrative error that bankrupts kingdoms.

**Severity:** critical.
**Confidence:** high.

---

### 4. Representation selection is heuristic and semantically wrong in places

The spec requires representation selection for closures, data, variants, records, dictionaries, refs, fibers, supervision scopes, TVars, STM journals, handlers, resumptions, strings, bytes, arrays, maps, sets, and numeric types. See `Spec.md:25091-25110`.

The implementation mostly infers representations from the first token of type text:

```fsharp
match typeText |> Option.bind tryTypeHead with
| Some "Int" -> Some BackendRepInt64
| Some "Float" -> Some BackendRepFloat64
| Some "Bool" -> Some BackendRepBoolean
| Some "String" -> Some BackendRepString
...
```

See `src/Kappa.Compiler/KBackendLowering.fs:42-64`.

Concrete problems:

* `Char`, `UnicodeScalar`, and `Grapheme` are mapped to `BackendRepString`. See `src/Kappa.Compiler/KBackendLowering.fs:54-56`.
* `Byte` is mapped to `BackendRepInt64`. See `src/Kappa.Compiler/KBackendLowering.fs:57`.
* Literal lowering similarly maps graphemes to strings and bytes to ints. See `src/Kappa.Compiler/KBackendLowering.fs:35-36`.
* There is no true `Bytes` representation.
* Data constructor field representations default to opaque rather than resolved field layouts. See `src/Kappa.Compiler/KBackendLowering.fs:231-248`.
* Data layout uses `"tagged-object"` and `"ordinal"` tag encoding. See `src/Kappa.Compiler/KBackendLowering.fs:1289-1310`.

This conflicts with the portable runtime obligations requiring preservation of `UnicodeScalar`, `String` vs `Bytes`, stable tag identity, and specified numeric behavior. See `Spec.md:25457-25470`.

**Severity:** critical for full conformance; moderate for the current toy subset.
**Confidence:** high.

---

### 5. Float equality violates the spec

The spec says `Float`/`Double` equality is raw-bit equality: `+0.0` is not equal to `-0.0`, and identical NaN payloads may compare equal. See `Spec.md:4888-4903`.

The Zig runtime uses C `==` for float equality:

```c
return value != NULL && value->tag == K_TAG_FLOAT && value->as.float_value == expected;
...
return kappa_box_bool(left->as.float_value == right->as.float_value);
```

See `src/Kappa.Compiler/Runtime/Zig/kappa_runtime.c:208-210` and `src/Kappa.Compiler/Runtime/Zig/kappa_runtime.c:417-423`.

That gives IEEE numeric equality, not raw-bit equality. It makes `+0.0 == -0.0` true and `NaN == NaN` false, both contrary to the spec’s default `Eq Float`.

**Severity:** high.
**Confidence:** high.

---

### 6. The CLR backend does not really preserve KBackendIR semantics

`ClrAssemblyLowering.fs` says it lowers KBackendIR into CLR assembly IR. See `src/Kappa.Compiler/ClrAssemblyLowering.fs:5`.

But mechanically, it converts `KBackendExpression` back into `KRuntimeExpression`-like forms:

* `BackendClosure` becomes `KRuntimeClosure`, dropping captures, environment layout, and calling convention. See `src/Kappa.Compiler/ClrAssemblyLowering.fs:18-19`.
* Backend intrinsic calls become `KRuntimeUnary` or `KRuntimeBinary`. See `src/Kappa.Compiler/ClrAssemblyLowering.fs:42-47`.
* Backend data construction becomes `KRuntimeApply`/name forms. See `src/Kappa.Compiler/ClrAssemblyLowering.fs:62-69`.
* `BackendRepIOAction` becomes `"IO Unit"`, losing the actual result type. See `src/Kappa.Compiler/ClrAssemblyLowering.fs:113-134`.

That is not a faithful target lowering from KBackendIR. It is a round-trip through an older representation, and it discards backend facts the spec expects to be fixed by this stage.

**Severity:** high.
**Confidence:** high.

---

### 7. Target checkpoint verification gives false confidence for CLR

The target checkpoint contract says target lowering depends on KBackendIR. See `src/Kappa.Compiler/CompilationCheckpoints.fs:38-45`.

For Zig, `tryEmitTargetTranslationUnit` actually emits a translation unit from KBackendIR. See `src/Kappa.Compiler/CompilationCheckpoints.fs:111-115`.

For CLR, it emits only a manifest with symbol names and empty `SourceText` after verifying KBackendIR. See `src/Kappa.Compiler/CompilationCheckpoints.fs:72-109` and `src/Kappa.Compiler/CompilationCheckpoints.fs:115-117`.

Then `verifyTargetCheckpoint` treats that as success. See `src/Kappa.Compiler/CompilationCheckpoints.fs:120-130`.

This means the CLR target checkpoint can pass even when actual IL emission would later reject closures, prefixed strings, function-valued applications, unsupported intrinsics, or other constructs. That is not a meaningful target-lowering verifier. It is a receipt printed before the restaurant checks whether it has food.

**Severity:** high.
**Confidence:** high.

---

### 8. Intrinsic availability and implementation are inconsistent

The spec requires backend intrinsics to have exact signatures, stable identity, and hard failure when absent. See `Spec.md:25511-25528`.

The current implementation advertises or recognizes a broad prelude surface but implements only a small subset in target emitters.

Examples:

* `IntrinsicCatalog.intrinsicRuntimeArity` returns hard-coded arities for a subset and defaults unknown names to arity `0`. See `src/Kappa.Compiler/IntrinsicCatalog.fs:121-197`.
* KBackend lowering uses `IntrinsicCatalog` for intrinsic arity and result representation. See `src/Kappa.Compiler/KBackendLowering.fs:204-229`.
* IL stubs `openFile`, `primitiveReadData`, `readData`, and `primitiveCloseFile` rather than performing real IO. See `src/Kappa.Compiler/IlDotNetBackendEmit.fs:864-874`.
* Zig stubs the same file operations: `openFile` returns `"<file:data.txt>"`, reads return `"chunk"`, and close prints `"closed"`. See `src/Kappa.Compiler/ZigCcBackendEmit.fs:635-672`.
* Zig rejects any other unsupported intrinsic at `src/Kappa.Compiler/ZigCcBackendEmit.fs:706-707`.

These stubs are fine for milestone tests, but they are not spec runtime behavior.

**Severity:** high.
**Confidence:** high.

---

### 9. Concrete bug: `negate` likely breaks on the CLR KBackend path

`IntrinsicCatalog` recognizes `"negate"` as a builtin unary intrinsic. See `src/Kappa.Compiler/IntrinsicCatalog.fs:63-64`.

`KBackendLowering` lowers `KRuntimeUnary("negate", operand)` to a backend intrinsic call. See `src/Kappa.Compiler/KBackendLowering.fs:1114-1128`.

`ClrAssemblyLowering` converts backend unary intrinsic calls back to `KRuntimeUnary(operatorName, operand)`. See `src/Kappa.Compiler/ClrAssemblyLowering.fs:42-44`.

But the IL emitter only accepts `KRuntimeUnary("-", operand)`, not `"negate"`:

```fsharp
| KRuntimeUnary("-", operand) -> ...
| KRuntimeUnary(operatorName, _) ->
    Result.Error $"IL backend does not support unary operator '{operatorName}' yet."
```

See `src/Kappa.Compiler/IlDotNetBackendEmit.fs:451-458` and `src/Kappa.Compiler/IlDotNetBackendEmit.fs:1157-1170`.

There is an IL intrinsic implementation for `"negate"` elsewhere, but this KBackend-to-CLR round-trip prevents that path from being used.

**Severity:** medium-high.
**Confidence:** high.

---

### 10. Zig likely violates short-circuit/evaluation-count semantics

The spec requires preservation of evaluation-count guarantees. See `Spec.md:25457-25470`.

The catalog distinguishes short-circuit binary operators:

```fsharp
let shortCircuitBinaryOperatorNames =
    Set.ofList [ "&&"; "||" ]
```

See `src/Kappa.Compiler/IntrinsicCatalog.fs:69-73`.

But `KBackendLowering` lowers all runtime binary operators, including `&&` and `||`, into named runtime calls after lowering both operands. See `src/Kappa.Compiler/KBackendLowering.fs:1129-1156`.

The Zig emitter evaluates all intrinsic arguments before dispatch:

```fsharp
let! emittedArguments = emitExpressions context scope arguments
...
let argumentStatements =
    emittedArguments |> List.collect (fun emitted -> emitted.Statements)
```

See `src/Kappa.Compiler/ZigCcBackendEmit.fs:436-444`.

Then it maps `"and"` and `"or"` to eager boolean helper calls. See `src/Kappa.Compiler/ZigCcBackendEmit.fs:507-508`.

The CLR path reconstructs `KRuntimeBinary` and the IL emitter has explicit short-circuit code beginning at `src/Kappa.Compiler/IlDotNetBackendEmit.fs:1171`, so this looks target-specific: CLR may preserve short-circuiting, Zig likely does not.

**Severity:** high for Zig.
**Confidence:** high.

---

### 11. Many KBackendIR constructs are accepted by the verifier but rejected later by emitters

Examples:

* IL rejects general application: `src/Kappa.Compiler/IlDotNetBackendEmit.fs:1361-1362`.
* IL rejects closures: `src/Kappa.Compiler/IlDotNetBackendEmit.fs:1363-1364`.
* IL rejects prefixed strings: `src/Kappa.Compiler/IlDotNetBackendEmit.fs:1365-1366`.
* Zig rejects `BackendPrefixedString`: `src/Kappa.Compiler/ZigCcBackendEmit.fs:37-38`.
* Zig rejects unsupported intrinsics: `src/Kappa.Compiler/ZigCcBackendEmit.fs:706-707`.

The KBackendIR verifier mostly checks names, arities, duplicate layouts, environment layout presence, and basic recursive well-formedness. See `src/Kappa.Compiler/CheckpointVerification.fs:625-837`.

It does not verify that the selected target can actually lower every reachable backend construct. That violates the spec’s “must not silently pass malformed or partially lowered runtime IR to target-profile lowering” rule. See `Spec.md:25230-25255`.

**Severity:** high.
**Confidence:** high.

---

## Backend-specific summary

### Zig backend

Good pieces:

* It emits from KBackendIR rather than bypassing it. See `src/Kappa.Compiler/CompilationCheckpoints.fs:111-115`.
* It has closure support with environment structs/captures. See `src/Kappa.Compiler/ZigCcBackendEmit.fs:98-203`.
* It preserves the narrow cleanup pattern using a panic frame. See `src/Kappa.Compiler/ZigCcBackendEmit.fs:243-297`.

Major issues:

* No real `rt-core`: no scheduler, promises, timers, fibers, STM, monitors, etc.
* File IO is stubbed.
* Float equality is wrong.
* `Char`, `UnicodeScalar`, `Grapheme`, `Byte`, and `Bytes` are not represented per spec.
* `&&`/`||` are probably eager.
* Many intrinsics are missing.
* Prefixed strings are rejected despite being representable in KBackendIR.

Verdict: useful prototype, not conforming.

### Dotnet / CLR backend

Good pieces:

* It has an IL emitter for simple functions.
* It preserves the narrow cleanup pattern through `finally`.
* It supports some refs via `StrongBox` and simple intrinsics.

Major issues:

* The public `dotnet` path converts KBackendIR back into KRuntime-like expressions, losing backend facts.
* The target checkpoint only emits a manifest, not actual verified target lowering.
* Closures, prefixed strings, and general application are rejected in IL emission.
* File IO is stubbed.
* `negate` is likely broken through the KBackend path.
* The profile has no real capability declaration/gating.

Verdict: even less conforming than Zig because the KBackendIR-to-target boundary is mostly ceremonial.

---

## Highest-priority fixes

### 1. Add a real backend capability model

Introduce a `BackendCapabilitySet` with at least:

```text
rt-core
rt-parallel
rt-shared-stm
rt-blocking
rt-atomics
rt-multishot-effects
```

Then make `zig`, `dotnet`, and `dotnet-il` declare what they actually support. Unsupported reachable constructs must be rejected before target lowering, with diagnostics tied to the missing capability.

This addresses `Spec.md:26882-26957`.

### 2. Stop treating prelude names as backend implementations

Separate these three concepts:

1. A name exists in the source prelude.
2. A name is lowered to a backend intrinsic.
3. A selected target actually implements that intrinsic with the required signature and semantics.

Right now those are muddled. The result is fake confidence, the deadliest kind except for C macros.

### 3. Make KBackendIR explicit enough for runtime semantics

Either expand KBackendIR to model the required runtime structures, or formally declare the current compiler as a restricted experimental subset.

For conformance, KBackendIR needs explicit nodes or equivalent structured forms for:

* cleanup scopes,
* error propagation,
* handlers and resumptions,
* fibers and scheduler interactions,
* promises and timers,
* TVars and STM journals,
* atomic operations and memory orders,
* heap/object field access,
* string/bytes/scalar representations.

The current expression-only model cannot satisfy §17.4.

### 4. Replace text-based representation selection

Representation selection should use resolved type information, not first-token type text. Fix at minimum:

* `Char`
* `UnicodeScalar`
* `Grapheme`
* `Byte`
* `Bytes`
* `Float` raw equality/order
* data/variant stable tag identity
* field layouts
* records
* arrays/maps/sets

### 5. Make target checkpoint verification real

For each target checkpoint, actually lower enough to prove the target can emit the reachable module.

For CLR, a manifest of symbol names is not enough. The verifier should catch the same things the IL emitter later rejects: closures, general application, prefixed strings, unsupported intrinsic calls, malformed type representations, and calling-convention mismatches.

### 6. Fix concrete semantic bugs

Start with these because they are clean, testable failures:

* Implement raw-bit float equality in Zig runtime.
* Preserve `&&`/`||` short-circuiting in Zig.
* Fix CLR `negate` lowering.
* Stop lowering `Char`/`UnicodeScalar`/`Grapheme` to `String`.
* Stop lowering `Byte` to `Int64`.
* Replace file IO stubs with either real runtime behavior or explicit “testing-only intrinsic” diagnostics.
* Reject unsupported prelude runtime terms before KBackendIR verification claims success.

### 7. Add conformance tests that will currently fail

Add backend tests for:

* `+0.0 != -0.0` under default float equality.
* identical NaN payload equality if literal/bit construction permits it.
* `floatEq` differs from default `Eq Float`.
* `String` vs `Bytes` distinction.
* `UnicodeScalar` round-trip and invalid scalar rejection.
* `&&` and `||` with side-effecting RHS.
* nested `using` cleanup under abrupt exit.
* closure capture on CLR path.
* prefixed strings through backend profiles.
* unsupported `fork`, `sleepFor`, `atomically`, `newTVar`, and effect handlers producing capability diagnostics, not emitter crashes or bogus arity errors.

---

## Bottom line

The backend has respectable scaffolding: a KBackendIR type, lowering pass, dumps, verification hooks, target profiles, and a few working runtime patterns. But compared to the spec, the implementation is missing most of the required runtime contract.

The most dangerous blind spot is that the compiler currently checks **shape** and **name availability** where the spec requires **semantic runtime capability and target-lowering legality**. That makes the backend look more complete than it is, which is how software projects become archaeological sites with CI badges.
