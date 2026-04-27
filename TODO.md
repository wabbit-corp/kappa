# TODO

This file consolidates the current review backlog from:

- `reviews/backend1.md`
- `reviews/dotnet1.md`
- `reviews/effects1.md`
- `reviews/frontend1.md`
- `reviews/general1.md`
- `reviews/principles1.md`
- `reviews/qtt1.md`
- `reviews/tests1.md`
- `reviews/zig1.md`

Duplicates are merged. The organization below is by compiler stage rather than by review file. The intent is to preserve the review content and proposed fixes with minimal editorial changes.

## 1. Cross-Cutting Foundations

- `[High] Create one authoritative intrinsic manifest instead of repeating intrinsic facts across the compiler.`
  - `IntrinsicCatalog.fs` is not actually authoritative today. Intrinsic names, arities, result representations, elaboration-time availability, interpreter behavior, IL behavior, Zig behavior, and backend lowering behavior are repeated in multiple places.
  - `IntrinsicCatalog.intrinsicRuntimeArity` currently falls back to arity `0` for unknown intrinsic names. Unknown intrinsic should be an error, not “nullary by default”.
  - A single manifest should carry at least: module name, intrinsic name, runtime arity, parameter representations, result representation, availability, effects, interpreter implementation, IL implementation, Zig implementation, and any backend support status.
  - Frontend import validation, elaboration, runtime lowering, interpreter, IL backend, Zig backend, and backend lowering should all consume the same data.
  - Sources: `reviews/principles1.md`, `reviews/backend1.md`, `reviews/general1.md`.

- `[High] Unify standard module descriptions into one descriptor set.`
  - `CompilationFrontend.fs` advertises inventories for `std.gradual`, `std.ffi`, `std.ffi.c`, `std.bridge`, `std.atomic`, `std.supervisor`, and `std.hash`.
  - `StandardModules.fs` only injects a much smaller runtime set, currently centered on `std.unicode`, `std.bytes`, and `std.hash`.
  - This creates a split brain where import validation knows about modules that elaboration/runtime/backend stages do not actually model or inject.
  - All standard modules should be described by one shared descriptor format containing at least module name, types, constructors, terms, traits, instances, runtime availability, and backend support status.
  - Sources: `reviews/principles1.md`, `reviews/general1.md`.

- `[High] Replace stringly backend/profile handling with a typed internal model.`
  - Backend profile strings such as `interpreter`, `dotnet`, `dotnet-il`, `zig`, and `zigcc` are scattered across `Stdlib.fs`, `Compilation.fs`, `CompilationCheckpoints.fs`, `HostBindings.fs`, and `Program.fs`.
  - Add a discriminated union such as `BackendProfile = Interpreter | DotNet | DotNetIl | Zig`, parse once at the boundary, and use the typed form internally.
  - Sources: `reviews/principles1.md`.

- `[High] Stop repeating semantic facts as hardcoded names or rendered type text.`
  - Compile-time-only type erasure in `KRuntimeLowering.fs` uses repeated hardcoded type-name lists such as `Type`, `Universe`, `Constraint`, `Quantity`, `Region`, `Syntax`, and `Code`.
  - Query semantics classify important types by final-name-segment matching such as `Query`, `Option`, `List`, and `Array`.
  - Backend representation selection reparses rendered type text and classifies based on the first token, which is brittle against aliases, qualification, pretty-printer changes, and user-defined same-spelling types.
  - These decisions should be driven by resolved semantic identities and representation metadata, not display text.
  - Sources: `reviews/principles1.md`, `reviews/backend1.md`.

- `[Medium] Remove duplicated language facts where the compiler and prelude each encode the same rule.`
  - `Float = Double` exists both as a prelude alias and as a compiler builtin normalization fact.
  - The implicit prelude constructor set in `Stdlib.fs` appears broader than the spec’s implicit unqualified constructor subset.
  - Pick one source of truth per language fact, or generate one layer from the other.
  - The current prelude constructor-subset test also encodes the broader implementation list, so this is now a test-spec drift issue as well as an implementation-spec drift issue.
  - Sources: `reviews/principles1.md`, `reviews/tests1.md`.

## 2. Source Loading, Module Mapping, and Lexing

- `[High] Make path-derived module mapping exactly match the spec.`
  - Path-derived module segments must be ASCII-only and match `[A-Za-z_][A-Za-z0-9_]*`.
  - The implementation currently uses Unicode-aware identifier predicates, accepts `.kp` case-insensitively, and validates only the derived module segments rather than also validating ignored fragment segments.
  - Files such as `main.💀.kp` can derive module `main` instead of being rejected for invalid fragments.
  - Add the required ASCII-lowercase case-fold collision check so `Foo.kp` and `foo.kp` are rejected together with diagnostics identifying all files.
  - Centralize path-to-module inference in one spec-facing function.
  - Sources: `reviews/frontend1.md`, `reviews/general1.md`, `reviews/principles1.md`.

- `[High] Validate unknown module attributes instead of silently accepting them.`
  - The parser accepts arbitrary `@Ident` attributes.
  - The frontend appears to meaningfully consume only `PrivateByDefault` and a small number of assertion-gating attributes.
  - Unknown attributes currently appear to pass through as inert metadata, which contradicts the spec unless they are explicitly documented as implementation-defined.
  - Add a validation pass that rejects unknown module attributes with direct diagnostics.
  - Sources: `reviews/frontend1.md`, `reviews/general1.md`.

- `[Medium-High] Align identifier and keyword handling with the lexical spec.`
  - Standard identifiers are specified as ASCII-only, but the current lexer accepts unquoted Unicode identifiers through `Char.IsLetter` and `Char.IsLetterOrDigit`.
  - Decide whether the spec should permit Unicode standard identifiers or the lexer should reject them except through backtick identifiers.
  - Keyword handling is also thinner than the spec: many contextual keywords listed in `Spec.md` are not represented in the token model even if some are parsed contextually.
  - Tooling that relies on token streams will see a different language than the spec if keyword recognition remains incomplete.
  - Sources: `reviews/frontend1.md`, `reviews/general1.md`.

- `[Medium] Tighten malformed numeric literal handling in the lexer.`
  - Prefixes such as `0x`, `0b`, and `0o` currently fall back too leniently when no valid digits follow, producing misleading later parse/name errors instead of lexical or literal diagnostics.
  - Add direct tests for malformed forms such as `0x`, `0b102`, `0o89`, and invalid suffix adjacency like `123abc`.
  - Sources: `reviews/frontend1.md`.

## 3. Parsing and Surface Syntax Coverage

- `[High] Complete the spec-required `do`-block syntax.`
  - Current `SurfaceDoStatement` and `ParseDoLine` support only a limited subset: `let`, `let?`, bind, `var`, assignment, `using`, `defer`, `if`, `while`, `return`, and expression items.
  - Missing spec-required forms include local signatures, named local definitions, `data`, `type`, `trait`, `instance`, `derive`, `import`, fixity declarations, `break`, `continue`, `for`, and labeled control flow.
  - Add AST support, parser support, lowering rules, and tests for each missing form.
  - Sources: `reviews/frontend1.md`.

- `[High] Implement `try` / `except` / `finally` and `try match` as real surface expressions.`
  - The parser/layout logic knows the words, but the surface AST and expression parser do not appear to represent or lower these forms.
  - They should not silently degrade into ordinary parse failures.
  - Sources: `reviews/frontend1.md`.

- `[Medium] Implement `derive` as syntax and semantics, not only as a reserved keyword.`
  - `Keyword.Derive` exists, but there is no parser/elaboration support for top-level or local `derive` declarations.
  - Sources: `reviews/frontend1.md`.

- `[High] Implement the actual totality / `decreases` surface language instead of only assertion gates.`
  - The spec’s totality system, `decreases` syntax, structural termination checking, and explicit measure language are far beyond the current implementation.
  - The visible implementation today mostly gates `assertTerminates` / `assertReducible` behind module attributes rather than implementing the full totality model.
  - Add surface syntax, elaboration, checking, and diagnostics for `decreases` and structural termination.
  - Sources: `reviews/frontend1.md`.

- `[Medium] Add dedicated diagnostics for multiple module headers and similar malformed top-level structure.`
  - Current behavior seems to fall back to generic parse errors rather than the more precise diagnostics implied by the spec.
  - Sources: `reviews/frontend1.md`.

## 4. Imports, Standard Modules, KFrontIR, and Diagnostics

- `[High] Finish import semantics for `unhide` and `clarify`.`
  - The parser recognizes `unhide` and `clarify` modifiers and rejects duplicate modifiers.
  - Semantic validation and export inventory construction do not appear to use `item.Modifiers` in a way that implements their specified semantics.
  - `unhide` must request access to private imported items, and `clarify` must request transparent treatment for opaque imported items.
  - Sources: `reviews/frontend1.md`.

- `[High] Either implement URL imports fully or reject them explicitly as unsupported.`
  - URL import syntax and pin-policy checks exist.
  - Real provider/fetch/cache/lock/resolution behavior does not appear to exist; URL imports still end up unresolved.
  - Do not leave them in a half-state where syntax and policy validate but semantic resolution fails generically.
  - Sources: `reviews/frontend1.md`, `reviews/general1.md`.

- `[Medium] Complete duplicate declaration checking across all top-level declaration categories.`
  - Current duplicate checks appear to focus mainly on `let`, `data`, `type`, and constructors.
  - Audit and add checks for signatures, projections, effects, traits, instances, and any other declaration categories that can collide.
  - Sources: `reviews/frontend1.md`.

- `[High] Upgrade diagnostics toward the spec’s machine-readable contract.`
  - Diagnostic records are currently much thinner than the spec requires.
  - Many diagnostics lack source locations or the richer payload/label/related/fix structure expected by the harness appendix and Chapter 17.
  - Add standardized families, payloads, labels, related origins, fix-it support, and long-form explanation entries where the spec requires them.
  - Sources: `reviews/frontend1.md`, `reviews/general1.md`.

- `[Medium] Complete the intended KFrontIR phase model instead of leaving it as scaffolding.`
  - The phase/checkpoint scaffolding exists and is useful, but the spec’s KFrontIR model is more detailed than the current implementation.
  - Continue filling in phase-accurate semantics, error tolerance, and tooling-facing behavior.
  - Sources: `reviews/frontend1.md`.

## 5. Surface Elaboration and Static Semantics

- `[High] Reconcile the implementation and tests with the spec’s explicit top-level signature rule.`
  - The spec says every exported top-level `let` definition must have an explicit top-level signature, while many current tests compile public top-level definitions such as `let result = 42` and assert success.
  - This appears across core/interpreter tests, backend tests, Zig tests, and generated fuzz-style tests.
  - Choose one:
  - update the spec if exported top-level inference is now intended; or
  - update tests and examples to add explicit signatures or make the module private via `@PrivateByDefault`.
  - Do not leave this as an implicit implementation exception while tests continue to treat it as normal conformance behavior.
  - Sources: `reviews/tests1.md`.

- `[High] Replace `std.hash.Hashable` special-casing with real instance machinery or explicit builtin dictionaries.`
  - `SurfaceElaboration.fs` hardcodes a set of intrinsically `Hashable` types and only enables the fallback for `std.hash.hashField` and `std.hash.hashWith`.
  - Implicit dictionary erasure also special-cases those names.
  - This should either become actual builtin instances resolved through ordinary instance search, or explicit intrinsic dictionaries represented in the shared intrinsic manifest.
  - Sources: `reviews/principles1.md`.

- `[Medium-High] Keep numeric literals abstract until target type resolution is complete.`
  - Current elaboration hardcodes numeric target names such as `Int`, `Integer`, `Nat`, `Float`, `Double`, and `Real`, plus trait names such as `FromInteger` and `FromFloat`.
  - Runtime lowering appears centered on `int64` and `double`, and there is even a fallback path that can lower an unhandled numeric literal to `Unit`, which is especially dangerous.
  - Keep numeric literals arbitrary-precision and unresolved until the target type and backend representation are known.
  - Sources: `reviews/principles1.md`, `reviews/general1.md`.

- `[Medium] Audit projection/accessor support for spec completeness.`
  - `reviews/general1.md` calls out projection/accessor support as incomplete.
  - Audit surface syntax, elaboration, and lowering for record projections, projector descriptors, accessor bundles, and any associated missing capability checks or diagnostics.
  - Sources: `reviews/general1.md`.

## 6. Resource Checking and QTT

- `[Critical] Separate demand tracking from linear discharge tracking.`
  - Borrowed demand can currently satisfy a linear `1` obligation without actually consuming/discharging it.
  - `UseMinimum` / `UseMaximum` are being asked to model runtime demand, ownership transfer, and lower-bound discharge at once.
  - Introduce a real demand interval plus separate discharge state, for example `DemandInterval` and `LinearDischarge`.
  - Borrowed demand should contribute runtime demand without discharging linear ownership.
  - Sources: `reviews/qtt1.md`.

- `[Critical] Give loop analysis an unbounded upper bound instead of modeling loops as zero-or-one iterations.`
  - `while` currently merges current state with one pass of the body, which under-approximates upper usage bounds.
  - The state model stores `UseMaximum: int`, which cannot represent `∞` even though the resource model’s interval type can.
  - Loop transfer should widen upper bounds for touched outer bindings to `None` / infinity unless the loop is statically bounded.
  - Sources: `reviews/qtt1.md`.

- `[High] Make `do` checking completion-indexed.`
  - The checker loses `return` / abrupt completion information by merging branch states into one ordinary continuation state.
  - Replace the single-state model with completion-indexed summaries such as `Normal`, `Return`, `Break`, `Continue`, and `Unreachable`.
  - Sequential composition should feed only `Normal` into following statements.
  - Sources: `reviews/qtt1.md`.

- `[High] Either reject quantity variables in QTT-relevant positions or implement symbolic quantity checking fully.`
  - Quantity variables are parsed in `TypeSignatures`, but the resource checker and resource model mostly do not support them meaningfully.
  - Current outcomes are likely partial, inconsistent, or silently weak.
  - Pick one of two honest states:
  - reject `QuantityVariable` in QTT-relevant positions with a clear diagnostic; or
  - implement symbolic quantity constraints, substitution, solving, and interface serialization.
  - Sources: `reviews/qtt1.md`.

- `[High] Give hidden borrow roots a real discharge story.`
  - Hidden borrow roots introduced for non-place borrowed bindings are created with quantity `1` but exempted from drop-checking.
  - That is not equivalent to the conceptual transform described by the spec.
  - Hidden roots should either be real owned bindings with drop-checking, compiler-managed temporaries with explicit destructor/release rules, or disallowed when the temporary would otherwise escape soundness.
  - Sources: `reviews/qtt1.md`.

- `[Medium-High] Preserve explicit borrow regions end-to-end.`
  - `TypeSignatures` can parse `&[s]`, but `ResourceCheckingSignatures` only understands bare `&`.
  - Constructor/signature token reconstruction also loses explicit region names.
  - Region identity is currently ignored by satisfaction checks such as `Borrow _, Borrow _ -> true`.
  - Signature collection should consume parsed binder/type AST, not stripped token fragments, and region equality/substitution should be explicit.
  - Sources: `reviews/qtt1.md`.

- `[Medium] Make application borrow/consume overlap analysis symmetric across the full application spine.`
  - Current checking only looks for earlier borrow versus later consume in argument order.
  - If borrow lifetimes extend for the duration of the call, the analysis should treat all arguments as overlapping for borrow/consume footprint checking unless a narrower evaluation/lifetime rule is proven.
  - Sources: `reviews/qtt1.md`.

- `[Medium] Delete the duplicate signature-quantity parser in resource checking.`
  - Quantity-bearing binders are parsed once in `TypeSignatures` and then partially reparsed in `ResourceCheckingSignatures`.
  - Unify signature extraction around parsed `TypeScheme` / `TypeExpr` instead of token surgery.
  - Sources: `reviews/qtt1.md`.

## 7. Effects and Handlers

- `[Critical] Introduce semantic effect/interface/label identities and use them everywhere.`
  - Runtime handler matching currently compares effect labels by name instead of by identity.
  - Resource checking and row splitting also rediscover handled effects syntactically in places where they should consume elaborated semantic identity.
  - Add stable IDs such as `EffectInterfaceId`, `EffectLabelId`, and operation IDs; handlers and row entries should match on IDs, not strings.
  - Sources: `reviews/effects1.md`.

- `[High] Resolve handled labels and operations before resource checking.`
  - Resource checking appears to recover one-shot and multi-shot semantics only for narrow syntactic forms such as direct names.
  - Alias-carried labels, record-carried labels, qualified imports, and rebound operation values can bypass or weaken the intended checks.
  - Add a resolved effect/operation representation so later passes do not need to rediscover effect identity from syntax.
  - Sources: `reviews/effects1.md`.

- `[High] Make one-shot resumption checking robust across aliases and first-class labels.`
  - One-shot resumption overuse should be rejected even when the handled label is rebound, stored in a record, or accessed through imports and member paths.
  - Add regression tests for alias-based and record-carried-label one-shot overuse.
  - Sources: `reviews/effects1.md`.

- `[High] Make multi-shot continuation capture analysis semantic rather than syntax-shaped.`
  - Current logic appears too tied to narrow `DoBind` / direct-operation-call forms.
  - It needs to recognize semantically equivalent operation invocations through aliases, records, qualified names, and nested expressions.
  - Tie continuation capture analysis to the nearest dynamically relevant handler boundary rather than just the lexical remainder of a `do` block.
  - Sources: `reviews/effects1.md`.

- `[High] Decide and document the backend story for effects.`
  - The current implementation parses, elaborates, checks, and interprets effect handlers, but backend lowering rejects them.
  - Either document effect handlers as interpreter-only for now, or implement a compiled backend strategy such as CPS or an explicit `Eff` runtime representation.
  - Sources: `reviews/effects1.md`, `reviews/backend1.md`, `reviews/general1.md`.

- `[Medium-High] Support non-self effect-row entries correctly.`
  - The implementation often appears to assume rows like `<[State : State]>`, while the spec allows first-class labels whose interface is `State`.
  - Effect-row entries should carry both label identity and interface identity.
  - Handlers should remove row entries by label identity, not by matching names.
  - Sources: `reviews/effects1.md`.

- `[Medium] Validate handler clauses as a first-class structure before elaboration proceeds.`
  - Preserve all parsed handler clauses first, then validate:
  - exactly one return clause;
  - return clause arity exactly one;
  - no duplicate operation clauses;
  - no missing operation clauses;
  - no unexpected operation clauses;
  - operation clause arity/binder agreement with the declared operation telescope.
  - This avoids silently mishandling duplicate `case return` clauses or malformed return-clause binder lists.
  - Sources: `reviews/effects1.md`.

- `[Medium] Parse and elaborate parameterized local scoped effects the same way as top-level effects.`
  - Local scoped effects appear to ignore header parameters even though the AST has header-token fields.
  - Do not maintain divergent parsers for nearly the same declaration form.
  - Sources: `reviews/effects1.md`.

- `[Medium] Apply ordinary import/visibility rules to top-level effect declarations.`
  - Audit private effect leakage and missing selective/wildcard import behavior for effects.
  - Effects should participate in qualified import, wildcard import, selective import, aliases, and visibility filtering the same way as other declarations.
  - Add tests for private-effect leakage and public-effect import under each import form.
  - Sources: `reviews/effects1.md`.

- `[Medium] Replace token-preserved effect operation signatures with typed telescopes.`
  - Operation signatures and handler clause binders currently preserve too much token trivia and too little elaborated type structure.
  - Dependent operation signatures require elaborated parameter telescopes, result types, quantities, and substitution support.
  - This is also needed for correct handler argument binding, resumption result typing, rebound operation values, and better diagnostics.
  - Sources: `reviews/effects1.md`.

- `[Medium] Represent rebound operation values explicitly instead of as guessed native functions.`
  - Rebound operations such as `let ask = Ask.ask` risk losing their `Eff` wrapper or having incorrect runtime arity, especially for multi-argument operations.
  - Add an explicit runtime value case for operation values and use declared operation telescopes during application.
  - Sources: `reviews/effects1.md`.

- `[Medium] Add a runtime one-shot fail-safe in the interpreter.`
  - Static checking should catch one-shot overuse, but the interpreter should still track consumed one-shot resumptions and fail loudly if a consumed one-shot resumption is called again.
  - This is a guardrail against compiler bugs, not a replacement for static checking.
  - Sources: `reviews/effects1.md`.

## 8. Runtime Lowering, Interpreter, and Runtime Surface

- `[High] Align the prelude/runtime contract with what the implementation actually provides.`
  - The prelude declares a much larger runtime surface than the interpreter and current backends implement, including fibers, scopes, promises, STM, time, and more.
  - Do not treat “name exists in the prelude” as equivalent to “the selected runtime/backend actually implements the semantics”.
  - Either trim the exposed surface to what exists, add capability-gated diagnostics, or implement the missing runtime features.
  - Sources: `reviews/general1.md`, `reviews/backend1.md`.

- `[High] Replace fake file/data intrinsics with real runtime behavior or explicit testing-only gating.`
  - Current interpreter, Zig, and IL paths stub file/data intrinsics with synthetic handles, `"chunk"`, and `"closed"`.
  - If this is fixture-only behavior, gate it behind a test runtime provider.
  - If it is intended to be general runtime behavior, it is not principled enough to leave as language semantics.
  - Sources: `reviews/principles1.md`, `reviews/backend1.md`.

- `[Medium-High] Stop lowering `Integer` / `Nat` / `Real` into a narrowly `int64` / `double`-centric runtime too early.`
  - The current implementation appears to limit runtime numeric behavior to `int64` and `double` in places where the surface language suggests broader numeric semantics.
  - This should remain abstract until representation selection is complete and explicit.
  - Sources: `reviews/general1.md`, `reviews/principles1.md`.

## 9. Backend-Agnostic Lowering, KBackendIR, and Verification

- `[Critical] Expand KBackendIR or explicitly narrow the supported backend contract.`
  - The current KBackendIR is a useful milestone IR for a narrow subset, but it is far smaller than the spec’s required backend/runtime model.
  - It does not model fibers, promises, timers, TVars, STM journals, handlers, resumptions, cleanup scopes, error propagation, or other required runtime structures explicitly.
  - Either expand it to model the required runtime semantics, or document the implementation as a restricted experimental subset rather than a conforming backend contract.
  - Sources: `reviews/backend1.md`.

- `[Critical] Add a real backend capability model.`
  - Backend profiles should declare capabilities such as `rt-core`, `rt-parallel`, `rt-shared-stm`, `rt-blocking`, `rt-atomics`, and `rt-multishot-effects`.
  - Unsupported reachable constructs should be rejected before target lowering with capability diagnostics tied to the actual requirement.
  - Current code tracks intrinsic-name sets, not runtime capability sets.
  - Sources: `reviews/backend1.md`.

- `[High] Stop treating prelude name availability as backend implementation availability.`
  - Separate three questions:
  - whether a name exists in source;
  - whether lowering maps it to a backend intrinsic;
  - whether the selected target actually implements that intrinsic with the required semantics.
  - Sources: `reviews/backend1.md`, `reviews/general1.md`.

- `[High] Lower effects to backend-supported constructs or reject them via capability diagnostics before KBackendIR claims success.`
  - `KRuntimeIR` still carries effect labels, operations, and handlers.
  - `KBackendLowering` currently rejects them directly.
  - Capability-aware rejection or real lowering should happen in a principled way.
  - Sources: `reviews/backend1.md`.

- `[High] Replace text-based representation selection with resolved representation metadata.`
  - Current backend representation selection from first-token type text is semantically wrong for `Char`, `UnicodeScalar`, `Grapheme`, `Byte`, `Bytes`, data layouts, and any alias/same-spelling cases.
  - The backend must preserve stable runtime layout guarantees, representation distinctions, and numeric semantics required by the spec.
  - Sources: `reviews/backend1.md`, `reviews/principles1.md`.

- `[High] Make target checkpoint verification prove real lowerability, not just shallow shape.`
  - The verifier currently checks mostly structural well-formedness and does not ensure that the selected target can lower every reachable construct.
  - For CLR specifically, target verification can succeed on a manifest even when actual IL emission would later reject closures, general application, prefixed strings, or unsupported intrinsics.
  - Verification should fail for any construct the selected target cannot actually lower.
  - Sources: `reviews/backend1.md`.

- `[Medium-High] Fix concrete cross-backend semantic bugs called out by the backend review.`
  - Raw-bit float equality is required by the spec, but Zig currently uses C/IEEE numeric equality.
  - `&&` / `||` evaluation-count and short-circuit behavior are likely wrong on Zig because arguments are emitted eagerly into runtime helper calls.
  - The CLR KBackend path likely breaks `negate` by round-tripping through the wrong runtime operator form.
  - `Char` / `UnicodeScalar` / `Grapheme` / `Byte` lowering is semantically wrong.
  - Sources: `reviews/backend1.md`.

## 10. Target Backends: CLR / `dotnet` / `dotnet-il`

- `[High] Make `host.dotnet` bindings self-contained in emitted IR and emitted artifacts.`
  - Current host binding emission depends on process-local compiler caches rather than durable IR-carried callable identity.
  - Generated runner projects also do not appear to copy or reference dependent host assemblies.
  - Persist host-callable identity into IR and either reference or copy required assemblies into generated output.
  - Sources: `reviews/dotnet1.md`.

- `[High] Either disable `--native-aot` for the managed `dotnet` backend or implement a real NativeAOT-compatible runner.`
  - The CLI advertises a native-AOT path, but the backend rejects it.
  - The current reflective `Assembly.LoadFrom` / `GetMethod` / `Invoke` runner shape is also a bad fit for NativeAOT.
  - Either fail early at CLI validation or generate a runner that statically references the emitted assembly and calls the entrypoint directly.
  - Sources: `reviews/dotnet1.md`.

- `[High] Fix string equality and string literal pattern matching in IL emission.`
  - Literal pattern matching currently uses `ceq`, which gives reference equality for strings.
  - String `==` / `!=` handling also needs a type-aware string equality path.
  - Use ordinal string equality calls when the compared type is `System.String`.
  - Sources: `reviews/dotnet1.md`.

- `[High] Implement or reject or-patterns honestly on the IL path.`
  - Current typing and emission appear to compile `A | B` as only the first alternative.
  - Either implement shared-success-label multi-alternative pattern matching correctly, including binder compatibility checks, or reject or-patterns before IL emission.
  - Sources: `reviews/dotnet1.md`.

- `[Medium-High] Pick one canonical CLR-facing representation for `Char`, `UnicodeScalar`, `Grapheme`, and `Byte`.`
  - The current pipeline inconsistently treats them as `System.Char`, `System.String`, or integer-like values depending on where the value entered the pipeline.
  - Decide the representation model explicitly and make literals, type parsing, host interop, backend lowering, and emission agree.
  - Sources: `reviews/dotnet1.md`, `reviews/backend1.md`.

- `[Medium] Implement or align missing printing intrinsics across prelude, runtime, and IL backend.`
  - `printlnString` is declared in the prelude and intrinsic catalog but appears missing or inconsistent in some runtime/backend layers.
  - Normalize `print`, `println`, `printString`, `printlnString`, and `printInt` across all layers and decide whether some are aliases.
  - Sources: `reviews/dotnet1.md`.

- `[Medium-High] Fix generic ADT construction inside generic methods.`
  - Constructor resolution on the IL path appears to expect result types to be more concrete than they should be, which risks breaking generic code such as `singleton x = Cons x Nil`.
  - Thread method-level generic parameters through constructor resolution and emitted member resolution.
  - Sources: `reviews/dotnet1.md`.

- `[Medium] Make `dotnet` and `dotnet-il` result-printing behavior consistent.`
  - `dotnet-il` appears to print unit results that the managed runner suppresses.
  - Centralize result-printing policy and apply it across both backend paths.
  - Sources: `reviews/dotnet1.md`.

- `[Medium] Read stdout/stderr asynchronously when shelling out to `dotnet build/run/publish`.`
  - The current CLI helper shape can deadlock on large output if it reads one stream to completion while the child process blocks on the other.
  - Use async/event-based process stream collection.
  - Sources: `reviews/dotnet1.md`.

- `[Low-Medium] Resolve entrypoints from the actually emitted CLR model, or assert their mapping from KBackendIR to ClrAssemblyIR.`
  - Current entrypoint resolution uses `workspace.KBackendIR` while emission uses `workspace.ClrAssemblyIR`.
  - This is probably fine when kept in lockstep, but it is a cheap place to add validation rather than relying on a silent invariant.
  - Sources: `reviews/dotnet1.md`.

## 11. Target Backends: Zig

- `[High] Make Zig arithmetic and comparisons representation-aware instead of always routing through integer helpers.`
  - `KBackendLowering.fs` can infer `BackendRepFloat64` correctly for float arithmetic, but `ZigCcBackendEmit.fs` currently maps arithmetic operators such as `+`, `-`, `*`, `/`, comparisons, and `negate` to integer-only runtime helpers.
  - This means float arithmetic likely emits C successfully and then fails at runtime with “expected Int value”.
  - Fix `emitIntrinsicCall` to receive and use the calling convention / operand representation data so it can choose `kappa_float_*` helpers where appropriate.
  - Add float arithmetic and float comparison tests on the Zig path.
  - Sources: `reviews/zig1.md`.

- `[High] Preserve short-circuit and evaluation-count semantics for `&&` and `||` on the Zig path.`
  - Current lowering/emission likely evaluates both operands eagerly and then dispatches to helper calls.
  - This violates the spec’s evaluation-count requirements if the RHS has effects or observable behavior.
  - There is also a concrete intrinsic-name mismatch: the Zig emitter handles `"and"` / `"or"` while the intrinsic catalog exposes `&&` / `||`.
  - The right fix is to lower `a && b` to `if a then b else False` and `a || b` to `if a then True else b` in backend lowering rather than trying to preserve them as eager runtime helper calls.
  - Sources: `reviews/backend1.md`, `reviews/zig1.md`.

- `[High] Implement raw-bit float equality for Zig runtime behavior.`
  - Default `Eq Float` / `Eq Double` semantics require raw-bit equality, not IEEE numeric equality.
  - Current C runtime behavior treats `+0.0` and `-0.0` as equal and all NaNs as unequal.
  - Sources: `reviews/backend1.md`, `reviews/general1.md`, `reviews/zig1.md`.

- `[High] Stop exposing the full prelude intrinsic surface under Zig unless the Zig emitter/runtime actually implement it.`
  - `Stdlib.fs` currently gives `zig` the full prelude intrinsic set, but the Zig emitter only implements a small subset.
  - Missing or mismatched examples called out in the Zig review include `printlnString`, `runPure`, `compare`, `floatEq`, several Unicode/hash intrinsics, and code-generation intrinsics such as `closeCode`, `genlet`, and `runCode`.
  - Either implement the missing intrinsics or restrict exposure under the Zig backend profile and fail earlier with clear capability/profile diagnostics.
  - Sources: `reviews/zig1.md`, `reviews/backend1.md`.

- `[High] Generate Zig trait dispatch from lowered runtime/backend data, not raw syntax documents.`
  - Current Zig trait-dispatch generation scans raw source documents for `InstanceDeclarationNode` rather than deriving dispatch entries from `KRuntimeIR` / `KBackendIR`.
  - This risks missing generated, rewritten, imported, or runtime-injected instances.
  - Dispatch matching also checks `module_name` and `instance_key` but omits `trait_name` even though dictionaries carry it.
  - Generate dispatch tables from lowered trait-instance data, match on `trait_name`, `module_name`, and `instance_key`, and emit dispatch functions for every reachable trait/member call site even when there are zero known instances.
  - Sources: `reviews/zig1.md`.

- `[Medium-High] Support first-class intrinsic values on the Zig path, or reject them before backend emission.`
  - The interpreter supports builtin/intrinsic functions as values, but the Zig emitter currently only accepts `True` and `False` as first-class intrinsics.
  - Shapes such as `let f = printString` are therefore likely broken on Zig.
  - Either synthesize closure wrappers for intrinsic functions, similar to constructor closures, or reject these programs explicitly under the Zig profile before backend emission.
  - Sources: `reviews/zig1.md`.

- `[Medium-High] Make Zig C symbol generation injective and collision-checked.`
  - The current identifier sanitizer is not injective, so different source names can sanitize to the same C symbol.
  - Function/type names also concatenate sanitized module and binding components in a way that allows cross-component collisions.
  - Use length-prefixed components or stable hash suffixes, and maintain a generated-name table so collisions are detected as compiler errors instead of surfacing later as C compile failures.
  - Sources: `reviews/zig1.md`.

- `[Medium-High] Make Zig closure environment type names globally unique across modules.`
  - Closure environment typedef names currently derive only from environment-layout names, and constructor closure environment names are created from constructor names plus per-module counters.
  - Because the counter resets per module, two modules can generate the same C typedef name for partially applied constructor closures.
  - Include module identity in environment layout names, or let the Zig/C backend assign globally unique environment type names independent of IR layout strings.
  - Sources: `reviews/zig1.md`.

- `[Medium] Fix Zig C string escaping and embedded-NUL handling.`
  - `cStringLiteral` currently emits control characters as `\xNN`, but C hex escapes are greedy and can absorb following hex digits unexpectedly.
  - The broader runtime model also stores strings as NUL-terminated `const char*`, so embedded `\0` truncates runtime behavior in `strlen`, `strcmp`, printing, and related operations.
  - Use safer fixed-width escape strategies or explicit-length string storage in `KValue`.
  - Sources: `reviews/zig1.md`.

- `[Medium] Align Zig equality and ordering behavior with interpreter semantics, or reject unsupported cases before emission.`
  - The interpreter recursively compares constructed values and supports comparison across several primitive categories.
  - The Zig runtime currently handles only a narrower scalar subset and falls back to false / integer-only comparisons for several cases.
  - If structural equality/order on constructed data is intended, implement it in Zig. If not, reject such programs before Zig emission instead of silently diverging from interpreter semantics.
  - Sources: `reviews/zig1.md`.

- `[Medium] Decide whether `is` is a real runtime operator and make Zig/backend lowering consistent.`
  - `KBackendLowering` treats `"is"` as a boolean-producing binary operator and the interpreter implements it, but the intrinsic catalog and Zig emitter do not support it consistently.
  - Either add `"is"` to the intrinsic catalog and implement it on the Zig path, or remove it from backend lowering.
  - Sources: `reviews/zig1.md`.

- `[Medium-High] Finish Unicode/bytes/byte/char runtime representation on the Zig path.`
  - The current representation model does not preserve the distinctions required by the spec.
  - Sources: `reviews/backend1.md`, `reviews/zig1.md`.

- `[Medium] Either implement prefixed/interpolated strings on Zig or reject them earlier with backend-profile diagnostics.`
  - The Zig emitter explicitly rejects `BackendPrefixedString`.
  - This is only acceptable if the Zig profile excludes all surface features that can lower to those nodes. Otherwise, rejection should happen earlier and more clearly than “unsupported backend expression” during C emission.
  - Sources: `reviews/zig1.md`.

- `[Low] Make `KAPPA_ZIG_EXE=zig` resolve through `PATH` the same way the unset case does.`
  - CLI resolution currently accepts `zig` from `PATH` only when `KAPPA_ZIG_EXE` is unset; if the environment variable is set to the bare command name `zig`, the code checks `File.Exists("zig")` instead.
  - If the configured value has no directory separator, treat it as a command name and let process launch resolve it through `PATH`, or search `PATH` explicitly.
  - Sources: `reviews/zig1.md`.

## 12. Tests, Harness, and Conformance Backlog

- `[High] Split the suite conceptually into spec conformance, compiler regression, backend regression, and integration tests.`
  - The current suite mixes at least three different categories without clearly separating them:
  - spec conformance tests;
  - current compiler regression tests;
  - backend/toolchain integration tests.
  - That makes failures harder to interpret and makes green runs overclaim what they prove.
  - Add explicit categories or naming conventions such as:
  - `SpecConformance.*`
  - `CompilerRegression.*`
  - `BackendRegression.*`
  - `Integration.*`
  - Sources: `reviews/tests1.md`.

- `[High] Add regression tests for the merged frontend/module/import issues.`
  - Module/file mapping:
  - exact `.kp` extension behavior;
  - ASCII-only path-derived segments;
  - invalid fragment rejection;
  - case-fold collisions.
  - Module attributes:
  - unknown attribute rejection.
  - Identifiers:
  - Unicode identifier acceptance/rejection according to the chosen policy.
  - Missing surface forms:
  - missing `do` control flow items;
  - local declarations in `do`;
  - `derive`;
  - `try` / `except` / `finally`;
  - totality / `decreases`.
  - Ordinary lexical/name ambiguity:
  - ambiguous imported names should be compile-time diagnostics, not runtime/interpreter failures.
  - Sources: `reviews/frontend1.md`, `reviews/general1.md`.

- `[High] Move ambiguous ordinary-name/import ambiguity detection fully into compilation and test it as such.`
  - There is at least one current test that expects ambiguous imported ordinary names to survive compilation and fail only during interpreter evaluation.
  - The spec makes ordinary lexical ambiguity a compile-time error.
  - Add or use a dedicated diagnostic such as `NameAmbiguous`, and convert these tests into frontend diagnostic tests rather than runtime-error tests.
  - Sources: `reviews/tests1.md`.

- `[High] Add regression tests for the QTT fixes before changing behavior.`
  - Borrow-only must not discharge linear `1`.
  - Multiple borrowed uses of exact `1` must be rejected or otherwise accounted for correctly.
  - Relevant `>=1` may still be satisfied by borrowed demand.
  - Loop upper bound must become infinite / unbounded.
  - Return branches must not flow into later statements.
  - Quantity variables must either work or be explicitly rejected.
  - Explicit borrow region preservation must round-trip.
  - Sources: `reviews/qtt1.md`.

- `[High] Add regression tests for effects/handlers based on the review’s concrete failure shapes.`
  - One-shot resumption through alias.
  - One-shot resumption through record-carried label.
  - Duplicate return clause.
  - Invalid return-clause arity.
  - Borrowed effect operation quantity.
  - Rebound zero-argument operation.
  - Rebound multi-argument operation.
  - Distinct same-named scoped effects.
  - Private effect import leak.
  - Wildcard/selective effect imports.
  - Non-self label row handling.
  - Sources: `reviews/effects1.md`.

- `[High] Add backend/runtime conformance tests for the issues already identified.`
  - Raw-bit float equality and NaN behavior.
  - `String` vs `Bytes` distinction.
  - `UnicodeScalar` round-trip and invalid scalar rejection.
  - `&&` / `||` with side-effecting RHS.
  - Nested `using` cleanup under abrupt exit.
  - Closure capture on the CLR path.
  - Prefixed strings through backend profiles.
  - Unsupported runtime terms such as `fork`, `sleepFor`, `atomically`, `newTVar`, and effect handlers should fail with capability diagnostics rather than later emitter crashes.
  - Sources: `reviews/backend1.md`.

- `[High] Add targeted .NET backend tests for the IL/runner-specific issues.`
  - String pattern matching with dynamically constructed strings.
  - String equality on distinct but content-equal string instances.
  - Or-pattern semantics.
  - `printlnString`.
  - Generic constructor inside a generic function.
  - `host.dotnet` binding through a generated runner process, not just in-process test loading.
  - `dotnet` vs `dotnet-il` unit-result printing behavior.
  - Sources: `reviews/dotnet1.md`.

- `[High] Expand Zig-specific test coverage beyond the current narrow list/CLI/demo cases.`
  - Add immediate tests for:
  - float arithmetic;
  - float/string/bool comparisons;
  - `&&` / `||` short-circuiting;
  - `printlnString`;
  - Unicode/hash standard-module intrinsics;
  - first-class intrinsic values;
  - symbol/name collisions;
  - partial constructor closures across modules;
  - structural equality on constructed data;
  - trait dispatch failure modes.
  - Also ensure any repo-local Zig bootstrap/test scripts referenced by the harness are actually present and exercised in normal test environments.
  - Sources: `reviews/zig1.md`.

- `[High] Reconcile prelude-oriented tests with the actual spec instead of treating implementation drift as normative.`
  - The implicit prelude constructor-subset test currently expects constructors beyond the spec’s exact fixed subset.
  - The “bundled bootstrap prelude exposes the normative minimum surface” test is not actually checking the spec’s full normative minimum, and it also appears to require `Char` even though the spec treats `Char` only as an optional deprecated compatibility alias.
  - Either:
  - update the spec to match the intended implementation surface; or
  - shrink/rename the tests so they honestly describe implementation surface rather than normative minimum conformance.
  - Sources: `reviews/tests1.md`, `reviews/principles1.md`.

- `[Medium-High] Replace weak negative test assertions with diagnostic-code assertions wherever possible.`
  - Several negative tests assert only `workspace.HasErrors` or message substrings.
  - That is too weak: parser errors, unrelated module errors, or any other failure can satisfy the test accidentally.
  - Prefer assertions on:
  - `workspace.HasErrors = true`;
  - the expected `DiagnosticCode`;
  - expected source span/module where practical;
  - message text only as a secondary check.
  - Sources: `reviews/tests1.md`.

- `[Medium] Replace the current short-circuit boolean regression that uses `1 / 0` with a well-typed RHS that fails only if evaluated.`
  - The current short-circuit test is trying to test runtime laziness, but it uses checked division by zero in the RHS.
  - Under the spec, division by zero should not silently typecheck just because it appears in a branch that is not evaluated at runtime.
  - Keep the short-circuit test, but use a statically valid RHS that would only fail if actually evaluated.
  - Sources: `reviews/tests1.md`.

- `[Medium] Rename or redesign misleading tests whose names overclaim what they prove.`
  - Example called out in the review:
  - “type imports from modules with identical export shapes stay distinct” uses transparent aliases to `Int`, which does not prove distinct nominal identity.
  - Similar honesty fixes may be needed for “normative minimum prelude surface” and other implementation-detail tests currently named like spec tests.
  - Sources: `reviews/tests1.md`.

- `[Medium] Label URL import tests as staging/policy tests until there is a real resolver.`
  - Current URL import tests are useful for parser and package/script-mode policy checks, but they do not exercise full URL-import conformance such as fetching, digest verification, transient locking, lockfile requirements, or mismatch behavior.
  - Keep them, but classify them honestly until a resolver/mock-resolver exists.
  - Sources: `reviews/tests1.md`.

- `[Medium] Make archive/toolchain-dependent tests explicit integration tests and ensure they cannot silently execute zero useful cases.`
  - Fixture theories should not degenerate into “no discovered cases” when fixture directories are absent from a source archive.
  - Zig integration tests should either ship the required scripts/tooling or be clearly skipped when prerequisites are unavailable.
  - More generally, archive/source-package test layouts should not rely on excluded files while still presenting themselves as runnable test surfaces.
  - Sources: `reviews/tests1.md`.

- `[Low-Medium] Remove stale `.trx` result artifacts from source-oriented archives unless they are intentionally used as golden observability artifacts.`
  - Historical `TestResults/*.trx` files add noise and can mislead review by mixing passing and failing historical snapshots that do not prove the current tree state.
  - Keep them only if they have a deliberate golden-artifact role.
  - Sources: `reviews/tests1.md`.

- `[Medium] Promote staged spec-derived tests from `new-tests/` into the live fixture suite once parser/elaboration/harness support exists.`
  - `new-tests/` is explicitly a staging area for spec-derived tests that are not yet wired into `tests/Kappa.Compiler.Tests/Fixtures`.
  - Move tests into the live fixture suite as support lands, and keep the promotion checklist current.
  - Sources: `new-tests/README.md`.
