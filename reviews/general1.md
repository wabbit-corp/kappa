## Verdict

This is **not a full spec-conforming compiler**. It is a serious partial implementation with a substantial frontend, resource checker, IR pipeline, and multiple backend attempts, but the spec is much larger than the implementation. The gap is not “a few edge cases”; it is more like the spec describes an empire and the compiler currently governs a fortified village with ambitious signage.

I could not build or run the test suite in this container because `dotnet` is not installed, and the archive explicitly excludes `tests/.../Fixtures/`, which the harness expects. I therefore treated this as a static conformance review plus inspection of the checked-in `.trx` artifacts.

The checked-in test artifacts are mixed. Some final-labeled runs pass, for example `application-elaboration-final-sln.trx` with 587/587 passed, `codex-rest-tests-final.trx` with 718/718 passed, and `new-tests-principled-final.trx` with 611/611 passed. But there are also “current” or intermediate artifacts with failures, for example `codex-current.trx` has 37 failures, `codex-current2.trx` has 18, and `places-projectors-borrowview-current.trx` has 32. These artifacts are historical breadcrumbs, not proof of the present source state. The usual human ritual: leave fossils everywhere, then ask whether the dinosaur is alive.

## Overall assessment

| Area                       |                                                                                      Status |
| -------------------------- | ------------------------------------------------------------------------------------------: |
| Parser / syntax coverage   |                                                          Partial, broad, but not spec-exact |
| Module/import system       | Substantial, but materially nonconforming in path rules, case collision, URL imports, attrs |
| Prelude surface            |                                                                       Large scaffold exists |
| Standard modules           |                                            Frontend advertises more than runtime implements |
| Type/literal semantics     |                                                 Partial, with important semantic mismatches |
| Resource checking          |                                   Serious implementation effort, but not fully audited here |
| Effects / handlers         |                                                                        Not backend-complete |
| Interpreter                |                                                                               Useful subset |
| Dotnet / IL / Zig backends |                                                                      Explicitly subset-only |
| Diagnostics                |                                                 Structured, but not as rich as spec demands |
| Full spec conformance      |                                                                                          No |

## Material conformance failures and gaps

### 1. Path-derived module names do not follow the spec exactly

The spec requires path-derived module segments to match ASCII-only:

```text
[A-Za-z_][A-Za-z0-9_]*
```

and requires rejection of case-fold collisions between module names. See `Spec.md:132-155`.

The compiler uses:

```fsharp
let private isValidModuleSegment (segment: string) =
    not (String.IsNullOrWhiteSpace(segment))
    && SyntaxFacts.isIdentifierStart segment[0]
    && segment |> Seq.forall SyntaxFacts.isIdentifierPart
```

from `src/Kappa.Compiler/CompilationFrontend.fs:28-31`.

But `SyntaxFacts.isIdentifierStart` and `isIdentifierPart` are Unicode-based:

```fsharp
let isIdentifierStart character = Char.IsLetter(character) || character = '_'

let isIdentifierPart character =
    Char.IsLetterOrDigit(character) || character = '_'
```

from `src/Kappa.Compiler/Syntax.fs:755-758`.

That means path-derived module segments may accept Unicode letters/digits even though the spec says ASCII only. The compiler also accepts `.kp` case-insensitively:

```fsharp
if not (fullPath.EndsWith(".kp", StringComparison.OrdinalIgnoreCase)) then
```

from `CompilationFrontend.fs:37`, while the spec says the path must end in `.kp`.

There is another bug: `tryInferModuleName` strips the filename at the first dot:

```fsharp
let moduleStem =
    match fileStem.IndexOf('.') with
    | -1 -> fileStem
    | index -> fileStem.Substring(0, index)
```

from `CompilationFrontend.fs:47-50`.

That correctly ignores optional fragments for the module name, but it does **not validate the ignored fragments**, even though the spec requires each optional fragment segment to match the same ASCII identifier regex. A file like `main.💀.kp` could derive module `main` rather than reporting the invalid fragment. Tiny corpse in the filename, no alarm bell.

Also, I found no implementation of the required ASCII-lowercase case-fold collision check. The compiler groups documents by exact module text:

```fsharp
documents
|> List.choose (fun document ->
document.ModuleName
|> Option.map (fun moduleName -> SyntaxFacts.moduleNameToText moduleName, document))
|> List.groupBy fst
```

from `CompilationFrontend.fs:1088-1094`.

That does not reject `Foo.kp` and `foo.kp` as required by `Spec.md:149-155`.

**Severity:** High.
**Confidence:** High.

---

### 2. Source identifiers are more permissive than the spec

The spec’s standard identifiers are ASCII-only:

```text
[A-Za-z_][A-Za-z0-9_]*
```

Backtick identifiers are the escape hatch for weird names like `` `λ` ``. See `Spec.md:4578-4605`.

The lexer again uses `Char.IsLetter` and `Char.IsLetterOrDigit`, so unquoted Unicode identifiers are accepted as ordinary identifiers. `Lexer.fs:126-130` reads identifiers using `SyntaxFacts.isIdentifierPart`, and token recognition uses `SyntaxFacts.isIdentifierStart` in several places, for example `Lexer.fs:478-480`.

So `let λ = 42` appears to be accepted as a normal identifier, even though the spec only permits that as ``let `λ` = 42``.

**Severity:** Medium to high, depending on whether Unicode identifiers are intended.
**Fix:** Either change the lexer to ASCII-only for unquoted identifiers or amend the spec. Do not leave the two disagreeing like a committee minutes document.

---

### 3. Unknown module attributes are silently accepted

The spec says:

> An unknown module attribute is a compile-time error unless the implementation explicitly documents it.

See `Spec.md:174-188`.

The parser accepts arbitrary `@Ident` attributes:

```fsharp
while this.Current.Kind = AtSign do
    this.Advance() |> ignore
    moduleAttributes.Add(this.ConsumeName("Expected a module attribute name after '@'."))
    this.SkipLayout()
```

from `src/Kappa.Compiler/Parser.fs:1855-1860`.

But the frontend only checks `PrivateByDefault` for visibility:

```fsharp
let private isPrivateByDefault (document: ParsedDocument) =
    document.Syntax.ModuleAttributes
    |> List.exists (fun attributeName -> String.Equals(attributeName, "PrivateByDefault", StringComparison.Ordinal))
```

from `CompilationFrontend.fs:711-713`.

I found no validation pass rejecting unknown attributes. So `@PrivateByDefualt module main`, typo and all, would likely be accepted and ignored. This is a bad failure mode because the user thinks they asked for privacy, and the compiler says, “marvelous,” then exports the furniture.

**Severity:** High.
**Confidence:** High.

---

### 4. URL imports are parsed and pin-checked, but not actually resolved as modules

The spec gives URL imports real module identity and reproducibility semantics. See `Spec.md:314` and `Spec.md:448-519`.

The parser supports string-literal import specifiers:

```fsharp
| StringLiteral ->
    ...
    match SyntaxFacts.tryParseUrlModuleSpecifier value with
    | Result.Ok specifier -> Url specifier
```

from `Parser.fs:933-948`.

The frontend validates pinning/reproducibility rules in package mode, but tests show URL imports still end up unresolved. For example, `tests/Kappa.Compiler.Tests/Tests.fs:646-665` expects a SHA-pinned URL import to avoid reproducibility diagnostics but still produce `ModuleNameUnresolved`.

That means the current implementation has syntax and policy checks, but no real URL module fetch/resolve/cache/lock mechanism.

**Severity:** High for spec conformance.
**Confidence:** High.
**Fix:** Either implement URL resolution and lockfile behavior, or explicitly document URL imports as unsupported and reject them with a dedicated diagnostic rather than letting them become unresolved modules.

---

### 5. Standard modules are advertised beyond what runtime actually provides

The spec requires standard modules such as `std.ffi`, `std.gradual`, `std.bridge`, `std.supervisor`, `std.hash`, `std.unicode`, and `std.bytes` in various sections. See `Spec.md:2727-3963`.

The frontend advertises inventories for many of these:

```fsharp
"std.gradual", ...
"std.ffi", ...
"std.ffi.c", ...
"std.bridge", ...
"std.atomic", ...
"std.supervisor", ...
"std.hash", ...
```

from `CompilationFrontend.fs:811-905`.

But runtime-injected standard modules come only from:

```fsharp
let all =
    [ unicodeModule; bytesModule; hashModule ]
```

from `src/Kappa.Compiler/StandardModules.fs:86-87`.

And compilation injects only `StandardModules.all` into KRuntimeIR:

```fsharp
@ (StandardModules.all |> List.map StandardModules.toRuntimeModule)
```

from `src/Kappa.Compiler/Compilation.fs:130-137`.

So the frontend can recognize/import more standard modules than the runtime/backends can execute. Worse, `std.bytes` is only:

```fsharp
{ ModuleName = "std.bytes"
  Types = [ "BytesBuilder" ]
  Terms = []
  Traits = [] }
```

from `StandardModules.fs:59-63`.

That is miles short of the byte-sequence module described by the spec. Miles, not meters. Imperial miles, since we are apparently doing old-world compiler archaeology.

**Severity:** High.
**Confidence:** High.

---

### 6. Prelude contract is much larger than implemented runtime intrinsics

`Stdlib/std/prelude.kp` declares a very broad surface: fibers, scopes, promises, STM, time, IO, refs, staging, equality, numeric traits, and more. For example, it declares `fork`, `await`, `atomically`, `newTVar`, `sleepFor`, `timeout`, etc. around `prelude.kp:263-307`.

The interpreter only creates intrinsic values for a much smaller subset:

```fsharp
| "pure"
| ">>="
| ">>"
| "runPure"
| "not"
| "and"
| "or"
| "negate"
| "println"
| "print"
| "printInt"
| "printString"
| "printlnString"
| "compare"
| "primitiveIntToString"
| "unsafeConsume"
| "newRef"
| "readRef"
| "writeRef" when isPreludeModule ->
```

from `Interpreter.fs:311-329`.

The hosted C# runtime is narrower still:

```csharp
"True" => new BooleanValue(true),
"False" => new BooleanValue(false),
"pure" => new BuiltinValue("pure"),
">>=" => new BuiltinValue(">>="),
">>" => new BuiltinValue(">>"),
"not" => new BuiltinValue("not"),
"and" => new BuiltinValue("and"),
"or" => new BuiltinValue("or"),
"negate" => new BuiltinValue("negate"),
"println" => new BuiltinValue("println"),
"print" => new BuiltinValue("print"),
"printInt" => new BuiltinValue("printInt"),
"unsafeConsume" => new BuiltinValue("unsafeConsume"),
```

from `Runtime/Hosted/KappaRuntime.cs:1574-1589`.

Anything else becomes:

```csharp
throw new RuntimeError($"Intrinsic term '{name}' is not implemented for module '{moduleName}'.")
```

So the compiler exposes a large prelude surface that runtime execution cannot honor.

**Severity:** High.
**Confidence:** High.

---

### 7. Float equality and ordering violate the spec

The spec says default `Eq Float` / `Eq Double` use raw IEEE-754 bit equality, not IEEE numeric equality. It also requires a total order, including NaNs. See `Spec.md:2319` and `Spec.md:4888-4903`.

The interpreter compares floats using normal .NET numeric equality:

```fsharp
| FloatValue left, FloatValue right ->
    left = right
```

from `Interpreter.fs:405-406`.

That makes `+0.0 = -0.0` true and `NaN = NaN` false, which is exactly what the spec says the default equality must **not** do.

Ordering uses:

```fsharp
| FloatValue left, FloatValue right -> Some(compare left right)
```

from `Interpreter.fs:285-286`.

That is not an explicit IEEE totalOrder implementation. It may work for many ordinary values, but it does not establish the specified NaN/payload/sign total ordering semantics.

**Severity:** High. This is an observable semantic mismatch.
**Confidence:** High.

---

### 8. Integer/Nat/Integer runtime values are limited to `int64`

The spec exposes `Nat`, `Integer`, and `Int` as distinct standard types and routes integer literals through `FromInteger : Nat -> t`. See `Spec.md:981-992` and `Spec.md:4916-4956`.

The parser stores integer literals as `BigInteger`, but runtime lowering only accepts `Int64` range:

```fsharp
| SurfaceIntegerLiteral(value, _, _) when value >= BigInteger(Int64.MinValue) && value <= BigInteger(Int64.MaxValue) ->
    Some(KCoreLiteral(LiteralValue.Integer(int64 value)))
```

from `SurfaceElaboration.fs:5256-5259`.

Runtime values are:

```fsharp
| IntegerValue of int64
```

from `Interpreter.fs:18-20`.

And out-of-range literals produce:

```fsharp
Numeric literal '{sourceText}' is outside the currently supported runtime range for its target type.
```

from `SurfaceElaboration.fs:8406-8408`.

So `Integer` and `Nat` are not truly arbitrary-precision operational runtime values here. That may be an acceptable implementation subset if documented, but it is not the full spec’s numeric behavior.

**Severity:** Medium to high.
**Confidence:** High.

---

### 9. Effects and handlers are not backend-conformant

The spec has a substantial effect/handler model, and backend conformance says accepted programs must behave observationally according to the spec. See `Spec.md:27053-27064`.

KBackendIR lowering rejects handlers:

```fsharp
| KRuntimeEffectLabel _
| KRuntimeEffectOperation _
| KRuntimeHandle _ ->
    Result.Error "KBackendIR lowering does not support effect handlers yet."
```

from `KBackendLowering.fs:891-894`.

The IL backend also rejects them:

```fsharp
| KRuntimeEffectLabel _
| KRuntimeEffectOperation _
| KRuntimeHandle _ ->
    Result.Error "IL backend does not support effect handlers yet."
```

from `IlDotNetBackendEmit.fs:412-415`.

Resource checking also gates multishot scoped operations by backend capability:

```fsharp
Backend profile '{currentBackendProfile ()}' does not provide capability 'rt-multishot-effects'
```

from `ResourceChecking.fs:5783-5788`.

This is acceptable only if affected programs are rejected under the relevant backend profile. It is not full backend conformance for the full effect language.

**Severity:** High for compiled backend profiles.
**Confidence:** High.

---

### 10. Dotnet/IL/Zig backends are explicitly subset implementations

The IL backend rejects ordinary functional features:

```fsharp
| KRuntimeApply _ ->
    Result.Error "IL backend currently supports application only when the callee is a named binding."
| KRuntimeClosure _ ->
    Result.Error "IL backend does not support closures yet."
| KRuntimePrefixedString _ ->
    Result.Error "IL backend does not support prefixed strings yet."
```

from `IlDotNetBackendEmit.fs:614-619`.

The Zig backend rejects prefixed strings:

```fsharp
Result.Error(sprintf "zig does not support prefixed string backend expression '%s\"...\"' yet." prefix)
```

from `ZigCcBackendEmit.fs:37-38`.

The CLR-backed dotnet profile also says:

```fsharp
"The CLR-backed dotnet profile currently supports managed builds only."
```

from `Backend.fs:174`.

None of this is inherently wrong if the compiler treats these profiles as subsets and rejects unsupported programs. But it means the backends are not general implementations of the spec.

**Severity:** Medium to high.
**Confidence:** High.

---

### 11. Keyword handling does not match the spec’s lexer statement

The spec says the lexer recognizes a broader set of soft/contextual keywords, including:

```text
block, open, exists, captures, force, decreases, structural, effect, handle, deep,
with, scoped, pattern, get, set, sink
```

See `Spec.md:4611-4639`.

The compiler’s `Keyword` enum and keyword table omit many of these. See `Syntax.fs:7-74` and `Syntax.fs:78-147`.

Some omitted words are handled contextually elsewhere, for example `pattern`, `effect`, `captures`, and `force`. So this may not break parsing in every case. But it does not match the spec statement that the lexer recognizes these keyword tokens.

**Severity:** Medium.
**Confidence:** Medium-high.

---

### 12. Projection/accessor support appears incomplete

The spec has extensive projection/place/accessor machinery. The compiler has real support in the AST and checker, but many cases still emit `ProjectionDefinitionUnsupported`.

Examples:

```fsharp
"Projection-section update requires an accessor/setter or selector projection."
```

from `SurfaceElaboration.fs:9807`.

```fsharp
"A projection definition must declare at least one place binder."
"A selector projection yield must denote a stable place..."
"An expanded accessor projection must declare exactly one place binder."
```

from `SurfaceElaboration.fs:14776-14831`.

Resource checking also emits `ProjectionDefinitionUnsupported` around `ResourceChecking.fs:2029-2041`.

That does not prove the feature is unusable, but it does show the implementation is still guarded by unsupported-feature diagnostics. The `.trx` artifacts also show a history of projection-related failures, although I would not treat those artifacts as current truth.

**Severity:** Medium.
**Confidence:** Medium-high.

---

### 13. Diagnostics are structured, but not rich enough for the spec’s diagnostic contract

The spec makes diagnostics part of the language contract: they should explain the rule, where the obligation came from, and local repair. The implementation does have structured diagnostics:

```fsharp
type Diagnostic =
    { Severity: DiagnosticSeverity
      Code: DiagnosticCode
      Stage: string option
      Phase: string option
      Message: string
      Location: SourceLocation option
      RelatedLocations: DiagnosticRelatedLocation list }
```

from `Diagnostics.fs:314-320`.

That is good. But many diagnostics use generic codes such as `ParseError`, `FrontendValidation`, or unsupported-feature codes. `tryGetExplanation` also returns `None` for many diagnostic codes, falling through at `Diagnostics.fs:307-308`.

So the diagnostic system is structurally on the right road, but it does not yet meet the full “diagnostics as contract” standard in the spec.

**Severity:** Medium.
**Confidence:** High.

## Things the implementation does well

The compiler is not a toy. The frontend has a real multi-stage pipeline: lexing, parsing, import validation, KFrontIR, KCore, KRuntimeIR, KBackendIR, CLR assembly IR, dumps, metadata, and checkpoint verification. That is actual architecture, not “parse a string and pray.”

Good signs:

* Strict UTF-8 source decoding exists, with `UnicodeInvalidUtf8`.
* There is meaningful module/import validation.
* The bundled prelude is real source, not just comments in a trench coat.
* The resource checker is substantial.
* The IR pipeline is explicitly represented and dumpable.
* Unsupported backend constructs usually become diagnostics/errors rather than silent miscompiles.
* The frontend inventories for standard modules suggest the author is tracking the spec’s surface area, even where runtime execution is not ready.

## Recommended fix order

1. **Declare implementation profiles honestly.**
   Add a conformance matrix: `frontend-only`, `interpreter-subset`, `dotnet-subset`, `zig-subset`, and maybe `full-spec` as aspirational. Right now the compiler and spec are pretending to be married while living in different countries.

2. **Generate conformance tests directly from spec MUSTs.**
   The spec has hundreds of normative requirements. Convert the obvious ones into fixture tests:

   * ASCII-only identifiers.
   * Invalid path fragments.
   * case-fold module collision.
   * unknown module attributes.
   * Float raw-bit equality.
   * URL import behavior.
   * standard module availability.

3. **Fix module path validation first.**
   This is cheap and foundational:

   * Require lowercase `.kp` exactly unless you intentionally amend the spec.
   * Validate every directory, basename, and optional fragment.
   * Use ASCII regex, not Unicode `Char.IsLetter`.
   * Add case-fold collision detection across effective module names.
   * Diagnostic must identify all colliding files.

4. **Validate module attributes.**
   Reject unknown attributes unless explicitly documented. If internal attributes like `allow_assert_terminates` are intentional, document and gate them properly.

5. **Decide what URL imports are.**
   Either implement real fetch/cache/sha256/ref/lock behavior or reject URL imports with a clear unsupported-feature diagnostic. Do not parse them as if they work and then let module resolution wander into a ditch.

6. **Make standard module frontend/runtime inventories agree.**
   If `std.gradual`, `std.ffi`, `std.bridge`, `std.atomic`, and `std.supervisor` are accepted by import validation, they need runtime modules or compile-time unsupported diagnostics before execution.

7. **Fix Float equality/order.**
   Implement raw-bit equality using bit representation. Implement explicit total ordering for floats/doubles. This is a crisp semantic bug, not a vague “future work” mist.

8. **Clarify numeric runtime scope.**
   If `Integer` and `Nat` are intended to be arbitrary precision, runtime values must stop being `int64`. If not, the spec or profile documentation must say so.

9. **Gate backend subsets explicitly.**
   Backend limitations are fine when rejected early and predictably. They are not fine if source programs pass frontend conformance and explode later with “not implemented yet.”

10. **Improve diagnostic related locations and explanations.**
    The diagnostic record has the right fields. Use them more consistently, especially for imports, module collisions, visibility, trait obligations, resource errors, and backend capability failures.

## Bottom line

The compiler is a credible prototype or evolving implementation, not a conforming implementation of `Spec.md`.

The biggest hard failures are:

1. module/path/identifier conformance,
2. unknown module attributes,
3. URL imports,
4. standard module/runtime mismatch,
5. Float equality/order semantics,
6. numeric runtime limits,
7. incomplete effect/backend support.

Fix those before claiming spec conformance. Otherwise the spec is less a contract and more a decorative tapestry hung over a construction site.
