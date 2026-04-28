# TODO

This file consolidates the current review backlog from:

- `reviews/backend1.md`
- `reviews/dotnet1.md`
- `reviews/effects1.md`
- `reviews/frontend1.md`
- `reviews/general1.md`
- `reviews/macros1.md`
- `reviews/observability1.md`
- `reviews/principles1.md`
- `reviews/queries1.md`
- `reviews/qtt1.md`
- `reviews/tests1.md`
- `reviews/traits1.md`
- `reviews/unicode1.md`
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
  - Backend profile strings such as `interpreter`, `dotnet`, `zig`, and `zigcc` are scattered across `Stdlib.fs`, `Compilation.fs`, `CompilationCheckpoints.fs`, `HostBindings.fs`, and `Program.fs`.
  - Add a discriminated union such as `BackendProfile = Interpreter | DotNet | Zig`, parse once at the boundary, and use the typed form internally.
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

- `[Medium-High] Align identifier and keyword handling with the lexical spec.`
  - Standard identifiers are specified as ASCII-only, but the current lexer accepts unquoted Unicode identifiers through `Char.IsLetter` and `Char.IsLetterOrDigit`.
  - Decide whether the spec should permit Unicode standard identifiers or the lexer should reject them except through backtick identifiers.
  - Keyword handling is also thinner than the spec: many contextual keywords listed in `Spec.md` are not represented in the token model even if some are parsed contextually.
  - Tooling that relies on token streams will see a different language than the spec if keyword recognition remains incomplete.
  - Sources: `reviews/frontend1.md`, `reviews/general1.md`.

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

## 4. Imports, Standard Modules, KFrontIR, and Diagnostics

- `[High] Finish the remaining diagnostics-contract work beyond the import family.`
  - Import-cycle diagnostics now expose related origins and the import-family codes now have long-form explanations, but many other frontend families still lack the richer payload/label/related/fix structure expected by the harness appendix and Chapter 17.
  - Continue with standardized families, payloads, labels, related origins, fix-it support, and long-form explanation entries for visibility, trait, resource, and backend diagnostics.
  - Sources: `reviews/frontend1.md`, `reviews/general1.md`.

- `[Medium] Complete the remaining KFrontIR query/error-tolerance model beyond honest phase claims.`
  - KFrontIR snapshots now stop claiming `BODY_RESOLVE`/`CHECKERS` when frontend errors prevented those phases from running, but the spec’s fuller KFrontIR model is still more detailed than the current implementation.
  - Continue filling in declaration/query-level lazy resolution, error-tolerant placeholder nodes, and tooling-facing behavior grounded in checker facts.
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

## 5A. Macros, Staging, and Elaboration-Time Evaluation

- `[Critical] Implement a real elaboration-time evaluator for `Elab` instead of only syntax/type-shape scaffolding.`
  - The current compiler recognizes `Elab`, `Syntax`, top-level splices, and some elaboration-available names, but there is no real meta-phase evaluator in the compilation pipeline.
  - `PipelineTraceEvent.EvaluateElaboration` and `MacroExpansionUnit` currently function more like metadata shells than evidence of actual elaboration-time execution.
  - Top-level splices should run `Elab (Syntax t)` at the splice site, produce syntax, and then elaborate the produced syntax under the ordinary checker stack.
  - Until that exists, `$(...)` should not pretend to implement macro expansion.
  - Sources: `reviews/macros1.md`.

- `[Critical] Implement `$(...)` as actual splice expansion followed by ordinary elaboration, not as a ceremonial wrapper that survives into lowering.`
  - The current implementation type-shapes `TopLevelSyntaxSplice`, lowers it structurally to `KCoreTopLevelSyntaxSplice`, and then largely ignores the macro-expansion semantics during runtime lowering.
  - The required semantics are:
  - execute `Elab`;
  - obtain `Syntax t`;
  - elaborate the generated syntax at the splice site;
  - re-run ordinary obligations such as implicit insertion, borrow/region checks, quantity checks, opacity, and visibility.
  - Sources: `reviews/macros1.md`.

- `[High] Replace the current name-based `Syntax` escape checks with a real hygienic `Syntax` value model.`
  - The spec requires syntax values to carry scope metadata for local variables, globals, implicit evidence, quantity capabilities, borrow regions, captures, local nominals, and source/synthetic origins.
  - The current implementation has a useful name-based escape diagnostic, but it is a much weaker approximation than spec-level hygiene.
  - Add a `SyntaxValue`/equivalent representation with hygienic binder identities, scope metadata, and origin chains, and make generated binders alpha-safe.
  - Sources: `reviews/macros1.md`.

- `[High] Stop ordinary substitutions and rewrites from crossing quote boundaries blindly.`
  - Existing substitution paths recurse through syntax/code quote nodes in a way that is a hygiene red flag.
  - Substitution inside `SyntaxQuote` / `CodeQuote` must be phase-aware and hygiene-aware.
  - Sources: `reviews/macros1.md`.

- `[High] Provide the required surface `Syntax` inspection and origin APIs.`
  - The spec requires a stable `Syntax` inspection API or equivalent public inspection surface.
  - The prelude currently exposes only a few related declarations such as `syntaxOrigin`, `renderSyntax`, and `normalizeSyntax`, and it is missing `withSyntaxOrigin`.
  - Add the required public inspection/origin surface rather than only parser support for quote syntax.
  - Sources: `reviews/macros1.md`.

- `[High] Implement semantic reflection APIs or stop claiming support for them.`
  - The spec requires types and operations such as `CoreCtx`, `Core`, `CoreEq`, `Symbol`, `asCore`, `reifyCore`, `inferType`, `whnf`, `normalize`, `tryProveDefEq`, `proveDefEq`, `defEq`, `headSymbol`, and `sameSymbol`.
  - These do not appear in the prelude/runtime surface in a conforming form today.
  - Sources: `reviews/macros1.md`.

- `[High] Implement the actual `Elab` monad/tooling surface instead of a small whitelist with a trench coat.`
  - The spec requires `currentGoal`, structured elaboration diagnostics, `emitElabDiagnostic`, `failElabDiagnostic`, `asCoreGoal`, `withExpectedGoal`, `ensureDefEqGoal`, and a coherent `Functor`/`Applicative`/`Monad` story for `Elab`.
  - The current implementation has declarations such as `failElab`, `warnElab`, and ad hoc recognition of `pure`, but not the required semantics.
  - Sources: `reviews/macros1.md`.

- `[Critical] Separate staging from macros properly; `Code` must not erase to the inner expression.`
  - Current runtime lowering erases the distinction between syntax/code quote/splice nodes by lowering many of them to the inner expression directly.
  - That is categorically wrong for `Code`, `ClosedCode`, `closeCode`, `genlet`, `runCode`, and the staging safety model.
  - Implement a distinct staged-code representation, closedness checking, scope safety, let-insertion/sharing for `genlet`, and runtime semantics for `runCode`.
  - `Code` must not be classified as just another compile-time-erased carrier.
  - Sources: `reviews/macros1.md`.

- `[Medium] Make macro-expansion incremental metadata real once macros actually exist.`
  - `MacroExpansionUnit` currently has no meaningful fingerprints or evaluator output dependencies.
  - Once elaboration-time expansion is real, its units/fingerprints must depend on macro definitions, imported interfaces, transcript/config inputs, compiler version, and expansion environment.
  - Sources: `reviews/macros1.md`, `reviews/observability1.md`.

## 5B. Traits, Instances, Coherence, and Deriving

- `[Critical] Represent constrained instance evidence explicitly instead of lowering only closed global instance keys.`
  - The current resolver uses instance premises to decide candidate viability, but selected dictionaries do not carry solved premise dictionaries into generated dictionaries or member bodies.
  - Constrained instances therefore behave more like Prolog clauses during lookup and like monomorphic global dictionaries at runtime, which is unsound for real typeclass semantics.
  - Add an internal `Evidence` model or equivalent dictionary-constructor representation so constrained instances, premise evidence, and member bodies have somewhere sane to live.
  - Sources: `reviews/traits1.md`.

- `[High] Rework supertraits as actual dictionary projection/evidence projection, not just member-name reachability.`
  - The current implementation makes reachable supertrait members callable by reusing the same dictionary parameter name, rather than projecting a supertrait dictionary or embedded evidence.
  - That can produce runtime-invalid calls such as passing an `Ord` dictionary directly where an `Eq` dictionary is required.
  - Sources: `reviews/traits1.md`.

- `[High] Strengthen instance declaration validation before lowering/runtime metadata generation.`
  - Current validation is too permissive and appears to miss or under-check:
  - unknown traits in instance heads;
  - wrong trait arity;
  - missing required members;
  - extra members;
  - duplicate members;
  - full member type compatibility, including parameter/quantity/implicitness compatibility;
  - default member validity under instance context.
  - Add a hard validation pass that resolves the trait, instantiates its member schemes, validates members/defaults, and fails early.
  - Sources: `reviews/traits1.md`.

- `[High] Enforce instance-search termination at declaration time instead of relying only on a runtime recursion guard.`
  - The current resolver has a runtime recursion guard, but that is not the spec’s declaration-time termination discipline.
  - Add Paterson-style structural checks over every premise/head pair in each instance declaration.
  - Sources: `reviews/traits1.md`.

- `[High] Implement the spec’s coherence model rather than rejecting all multiple surviving candidates uniformly.`
  - Current resolution is “zero candidates unresolved, one resolved, many ambiguous”.
  - The spec requires a richer coherence story around equivalent surviving candidates and canonical representative selection.
  - Add a richer resolution result/evidence model, then implement canonicalization/hash-based coherence rather than blanket ambiguity rejection.
  - Sources: `reviews/traits1.md`.

- `[Medium] Fix constraint parsing/model gaps in instance and trait contexts.`
  - Parenthesized multi-constraint contexts in instance headers are currently suspect because constraint splitting/parsing is duplicated and inconsistent.
  - Silent `List.choose` constraint dropping is also a bad failure mode.
  - Unify constraint-context parsing and make failures diagnostic rather than disappearing parse fragments.
  - Sources: `reviews/traits1.md`.

- `[Medium] Distinguish rigid goal variables from candidate-instantiable variables during instance matching.`
  - Current unification appears too symmetric for instance matching and likely allows goal/source rigid variables to be solved as if they were ordinary inference metavariables.
  - Add a dedicated instance-head matcher where only variables bound by the instance declaration are assignable.
  - Sources: `reviews/traits1.md`.

- `[Medium] Either support constrained trait members end-to-end or reject them explicitly.`
  - Overloaded trait member call preparation currently appears to accept only the owning trait constraint and drop/fail members with additional constraints.
  - If constrained trait members are allowed, all required dictionaries need to be passed and resolved. If not, reject them during trait validation with a direct diagnostic.
  - Sources: `reviews/traits1.md`.

- `[Medium] Add AST/model support for local instances and local deriving if they remain part of the language.`
  - The current local-expression model has local lets, signatures, type aliases, and scoped effects, but no local instance or local derive forms.
  - Either add these as real local declaration forms or narrow the language claim.
  - Sources: `reviews/traits1.md`.

- `[Medium] Rebuild deriving as synthetic instance generation over the same instance pipeline, not as a future parallel mechanism.`
  - The language reserves `derive`, but the AST/parser/elaboration/runtime path do not implement it.
  - Trait review reinforces that deriving should lower to synthetic `InstanceInfo` / equivalent generated instance declarations and then go through the same coherence, validation, and runtime-lowering path as handwritten instances.
  - Do not add a separate backend-only deriving mechanism.
  - Sources: `reviews/traits1.md`, `reviews/frontend1.md`.

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

- `[High] Make multi-shot continuation capture analysis semantic rather than syntax-shaped.`
  - Current logic appears too tied to narrow `DoBind` / direct-operation-call forms.
  - It needs to recognize semantically equivalent operation invocations through aliases, records, qualified names, and nested expressions.
  - Tie continuation capture analysis to the nearest dynamically relevant handler boundary rather than just the lexical remainder of a `do` block.
  - Sources: `reviews/effects1.md`.

- `[Medium-High] Support non-self effect-row entries correctly.`
  - The implementation often appears to assume rows like `<[State : State]>`, while the spec allows first-class labels whose interface is `State`.
  - Effect-row entries should carry both label identity and interface identity.
  - Handlers should remove row entries by label identity, not by matching names.
  - Sources: `reviews/effects1.md`.

  - Do not maintain divergent parsers for nearly the same declaration form.
  - Sources: `reviews/effects1.md`.

- `[Medium] Replace token-preserved effect operation signatures with typed telescopes.`
  - Operation signatures and handler clause binders currently preserve too much token trivia and too little elaborated type structure.
  - Dependent operation signatures require elaborated parameter telescopes, result types, quantities, and substitution support.
  - This is also needed for correct handler argument binding, resumption result typing, rebound operation values, and better diagnostics.
  - Sources: `reviews/effects1.md`.

## 7A. Query and Comprehension Semantics

- `[Critical] Stop lowering comprehensions in the parser and build a real query/comprehension plan after semantic resolution.`
  - The parser currently lowers comprehensions far too early and `SurfaceComprehension` carries lowered expressions.
  - That prevents correct implementation of carrier selection, source conversion, orderedness, resource semantics, custom sinks, and delayed query behavior.
  - Introduce a real normalized `ComprehensionPlan` after semantic resolution and lower only after mode/cardinality/orderedness/source-demand/carrier facts are known.
  - Sources: `reviews/queries1.md`.

- `[Critical] Implement `IntoQuery` and `BorrowIntoQuery` for real instead of exposing empty traits plus list-shaped lowering.`
  - The spec’s query/comprehension model depends on source conversion traits.
  - The current prelude traits are empty, and lowering largely pattern-matches list-like syntax directly.
  - Built-ins such as list, array, set, option, query, and once-query should flow through ordinary instance hooks or compiler-known equivalents with the same semantics.
  - Sources: `reviews/queries1.md`.

- `[Critical] Make `Query`, `OnceQuery`, and `QueryCore` first-class runtime/IR carriers rather than type-shaped decorations over ordinary collections.`
  - Current checking can make a comprehension look query-shaped, but the lowered code often becomes an ordinary list/set/map expression.
  - This is a semantic mismatch, not a harmless implementation detail.
  - Add real runtime/IR representations and collector/lowering behavior for first-class query carriers.
  - Sources: `reviews/queries1.md`.

- `[High] Implement custom comprehension sinks via `FromComprehensionRaw` / `FromComprehensionPlan`, or reject them honestly.`
  - The spec requires raw-hook and plan-hook custom sinks, with raw preferred over plan.
  - The current implementation has some trait-shaped checks but no actual hook invocation machinery.
  - Sources: `reviews/queries1.md`.

- `[High] Implement refutable and borrowed query clauses semantically, not just syntactically.`
  - `for?` is parsed but lowering/resource checking ignore its filter semantics.
  - `let?` appears unsupported or semantically ignored.
  - Borrowed generators are parsed but `BorrowIntoQuery`, borrowed item mode, and region/resource semantics are not implemented.
  - Sources: `reviews/queries1.md`.

- `[High] Implement `group by` as actual grouping/folding rather than row-wrapping.`
  - The current implementation maps rows into key/aggregate-shaped records but does not partition rows, combine groups, or fold aggregates monoidally.
  - This is semantically wrong for nontrivial groups.
  - Sources: `reviews/queries1.md`.

- `[High] Implement map-comprehension conflict policy and validate map yields strictly.`
  - `on conflict` is parsed but ignored during final collection construction.
  - Map comprehensions currently accept invalid plain `yield` forms that should require `key : value`.
  - Ordinary map literals also appear broken nearby in the same parser area, which should be audited/fixed alongside comprehension handling.
  - Sources: `reviews/queries1.md`.

- `[Medium-High] Tighten orderedness semantics for `skip` / `take`.`
  - The current implementation rejects these clauses only when orderedness is explicitly known to be unordered, but allows unknown orderedness.
  - The spec requires proof of orderedness, not just absence of proof of unorderedness.
  - Source orderedness inference itself is also too weak and largely syntactic today.
  - Sources: `reviews/queries1.md`.

- `[Medium-High] Finish `order by` semantics.`
  - Single-key insertion-sort lowering exists, but multi-key ordering with per-key directions is missing.
  - Implement `order by asc expr`, `order by desc expr`, and tuple/multi-key ordering as specified.
  - Sources: `reviews/queries1.md`.

- `[Medium] Audit `distinct`, joins, and left joins against the spec’s actual delayed-query and row semantics.`
  - `distinct` is only partial and relies on coarse equality/resource assumptions.
  - Inner joins are only approximately lowered and inherit the broken refutable-clause story.
  - `left join ... into name` currently fakes a `QueryCore` type but materializes an eager list, breaking delayed-query semantics and capture rules.
  - Sources: `reviews/queries1.md`.

- `[Medium] Validate clause ordering and duplicate final-clause errors instead of silently discarding malformed comprehension structure.`
  - Earlier `yield` / `on conflict` clauses can currently be discarded by later parser stages instead of producing direct diagnostics.
  - Invalid comprehensions should be rejected, not quietly normalized into something else.
  - Sources: `reviews/queries1.md`.

- `[Medium] Build real comprehension/query observability and reflection structures if the spec-level hook story is intended.`
  - The spec requires normalized plan/raw reflection structures such as `RawComprehension`, `ComprehensionPlan`, and `lowerComprehension`.
  - The current AST has `SurfaceComprehension`, but not the spec-level exposed plan representation that custom carriers/hooks need.
  - Sources: `reviews/queries1.md`.

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

## 8A. Unicode, Bytes, and Hash Semantics

- `[High] Fix byte literal decoding to preserve spec semantics and escape provenance.`
  - Byte literals are currently decoded through the ordinary string unescaper, then accepted whenever the resulting scalar value is `<= 0xFF`.
  - That incorrectly accepts forms like `b'ÿ'` / `b'\u{00FF}'` even though the spec only permits Unicode escapes when the resulting scalar’s UTF-8 encoding is exactly one byte.
  - Implement a byte-literal-specific decoder so `\xNN` is a raw byte and Unicode escapes are accepted only when their UTF-8 encoding length is exactly one byte.
  - Sources: `reviews/unicode1.md`.

- `[High] Fix `decodeUtf8` to return the right constructor and the right error type.`
  - The current error path appears to construct `Error` instead of the prelude’s `Err`, and it returns a `StringValue` where the declared result type says `UnicodeDecodeError`.
  - That is a concrete runtime/type-shape bug, not just a missing feature.
  - Sources: `reviews/unicode1.md`.

- `[High] Implement or retract missing `std.unicode` surface terms.`
  - `StandardModules.fs` advertises terms such as `scalars`, `graphemes`, `words`, and `sentences`, but the interpreter does not create them.
  - Either implement these terms in the interpreter/runtime surface or stop exporting them until they work.
  - Sources: `reviews/unicode1.md`.

- `[High] Make `String` ordering Unicode-scalar-order, not UTF-16 ordinal.`
  - The interpreter currently uses `String.CompareOrdinal` for `String`, `Character`, and `Grapheme`.
  - That diverges from Unicode scalar order for supplementary characters.
  - Replace this with scalar-sequence comparison over `Rune` values.
  - Sources: `reviews/unicode1.md`.

- `[Medium-High] Emit the declared Unicode warning diagnostics or remove the illusion that they exist.`
  - The diagnostic codes for bidi control, confusable identifiers, and non-normalized source text exist, but the review found no emitters.
  - Implement them or clearly narrow the current behavior.
  - Sources: `reviews/unicode1.md`.

- `[Medium] Decide what user-facing location metrics mean for Unicode text and make the implementation/tooling consistent.`
  - Source positions and columns are currently based on UTF-16 code units.
  - That is not automatically wrong, but it must be documented and consistent with the intended user-facing model; otherwise non-BMP characters get surprising columns.
  - Sources: `reviews/unicode1.md`.

- `[Medium] Pin Unicode behavior honestly.`
  - The interpreter returns a hardcoded Unicode version string while segmentation/normalization behavior is delegated to the underlying .NET runtime/globalization backend.
  - Either pin the actual Unicode data and behavior, or make the version/behavior explicitly implementation-defined.
  - `words` / `sentences` also need real UAX #29 behavior if they are going to exist.
  - Sources: `reviews/unicode1.md`.

- `[High] Decide whether `Hashable` is a real trait or compiler-magic, and make the entire stack consistent with that answer.`
  - `std.hash.Hashable` is currently declared without the spec-required member and superclass structure, while the compiler synthesizes special-case support in selected places.
  - Pick one model:
  - a real trait with `hashInto` and coherent dictionary semantics; or
  - explicit compiler-magic with the spec/stdlib updated to match.
  - The current middle state gives you the failure modes of both.
  - Sources: `reviews/unicode1.md`, `reviews/principles1.md`.

- `[Critical] Fix the streaming hash API so it returns `HashState`, not `Option HashState`, and redesign the state transition properly.`
  - The current grouped hash intrinsics appear to wrap `hashIntoState` results through `optionValue`, which means the advertised `HashState`-returning functions can actually produce `Some ...` / `None`.
  - The state transition also appears to reinitialize through `seed ^ offset` on every update rather than continuing a real streaming update.
  - Split initialization from update, use fixed-width/fixed-endian encodings, and make unsupported values a proper error rather than `None` inside a mismatched declared type.
  - Sources: `reviews/unicode1.md`.

- `[High] Do not advertise `Hashable` fallback support for values the runtime cannot actually hash.`
  - The typechecker currently accepts a built-in `Hashable` fallback for types like `Option`, `List`, `Array`, `Result`, and others, but the interpreter’s actual hash implementation handles only a much narrower primitive subset.
  - Either implement structural hashing for those types or shrink the fallback surface to what the runtime truly supports.
  - Sources: `reviews/unicode1.md`.

- `[Medium-High] Add structural framing/domain separation and fixed byte order to hashing.`
  - Current hashing lacks type tags/field separators/length framing for structural values, uses platform-endian `BitConverter.GetBytes`, and likely lets float hashing disagree with float equality.
  - Add domain separation, field/constructor framing, fixed byte order, and an explicit decision about whether float equality is raw-bit or canonicalized for hashing purposes.
  - Sources: `reviews/unicode1.md`, `reviews/backend1.md`.

- `[High] Add real backend representations/capability gates for `Bytes`, `Byte`, `UnicodeScalar`, `Grapheme`, and hash values.`
  - Today these semantics are largely interpreter-local.
  - Hosted/IL/Zig backends do not preserve them coherently, and Zig/C string representation is especially incompatible with valid strings containing U+0000.
  - Every backend needs length-carrying string/bytes semantics or a clearer capability gate that rejects unsupported programs early.
  - Sources: `reviews/unicode1.md`, `reviews/backend1.md`, `reviews/zig1.md`.

- `[Medium] Make backend lowering/runtime support for standard-module intrinsic terms real instead of structurally awkward.`
  - `StandardModules.toRuntimeModule` creates intrinsic-term-only runtime modules, but backend lowering/runtime support does not resolve them cleanly the way the interpreter does.
  - This especially affects `std.unicode`, `std.bytes`, and `std.hash`.
  - Sources: `reviews/unicode1.md`.

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

## 10. Target Backends: CLR / `dotnet`

- `[Medium-High] Pick one canonical CLR-facing representation for `Char`, `UnicodeScalar`, `Grapheme`, and `Byte`.`
  - The current pipeline inconsistently treats them as `System.Char`, `System.String`, or integer-like values depending on where the value entered the pipeline.
  - Decide the representation model explicitly and make literals, type parsing, host interop, backend lowering, and emission agree.
  - Sources: `reviews/dotnet1.md`, `reviews/backend1.md`.

  - Sources: `reviews/dotnet1.md`.


- `[Medium] Read stdout/stderr asynchronously when shelling out to `dotnet build/run/publish`.`
  - The current CLI helper shape can deadlock on large output if it reads one stream to completion while the child process blocks on the other.
  - Use async/event-based process stream collection.
  - Sources: `reviews/dotnet1.md`.

## 11. Target Backends: Zig

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

- `[Medium-High] Finish Unicode/bytes/byte/char runtime representation on the Zig path.`
  - The current representation model does not preserve the distinctions required by the spec.
  - Sources: `reviews/backend1.md`, `reviews/zig1.md`.

- `[Medium] Either implement prefixed/interpolated strings on Zig or reject them earlier with backend-profile diagnostics.`
  - The Zig emitter explicitly rejects `BackendPrefixedString`.
  - This is only acceptable if the Zig profile excludes all surface features that can lower to those nodes. Otherwise, rejection should happen earlier and more clearly than “unsupported backend expression” during C emission.
  - Sources: `reviews/zig1.md`.

## 11A. Observability, Incrementality, and Checkpoint Metadata

- `[Critical] Add imported-interface dependency edges to the incremental/query graph.`
  - The current metadata largely models per-file linear chains and omits dependencies from importing modules to imported module interfaces/fingerprints.
  - That makes downstream invalidation unsound when exported signatures change.
  - Build a real resolved import graph and make import-surface/header/body/KCore/backend units depend on imported interfaces and the effective import environment as required.
  - Sources: `reviews/observability1.md`.

- `[High] Model generated standard modules, host-binding modules, runtime intrinsic sets, and backend runtime support as explicit incremental dependencies.`
  - `KRuntimeIR` includes generated host-binding and standard modules, but backend fingerprints/units/target dependencies currently filter through user-module source files and can omit these generated contributors.
  - Add explicit units/fingerprints for standard runtime modules, host-binding modules, runtime intrinsic sets, and backend runtime support, then thread them into KBackendIR and target-lowering dependencies.
  - Sources: `reviews/observability1.md`.

- `[High] Replace the current synthetic pipeline trace with actual execution tracing, or rename it honestly.`
  - The current pipeline trace is reconstructed after compilation from the final document list and target list, and it does not reflect real execution ordering, skipped work, lazy queries, failures, or reuse.
  - `Reuse` exists as a trace event but is not actually emitted.
  - Collect trace events at the point work really executes, including reuse/invalidate/skip/cache-probe events, or rename the current output to make clear that it is a planned/synthesized trace.
  - Sources: `reviews/observability1.md`.

- `[Medium] Reconcile `KRuntimeIR` checkpoint contract metadata with actual profile influence.`
  - The checkpoint contract currently marks `KRuntimeIR` as not profile-specific, but KRuntimeIR construction includes profile-dependent host binding modules.
  - Either mark it profile-specific or split profile-neutral runtime IR from profile-specific runtime augmentation.
  - Sources: `reviews/observability1.md`.

- `[Medium] Replace index-based declaration keys with more stable declaration identities.`
  - Declaration input keys currently include declaration index, so inserting one declaration at the top of a file shifts the keys of all following declarations and harms reuse.
  - Prefer stable identities based on resolved symbol identity, source span, declaration kind, and spelling, with a tie-breaker only for true ambiguity.
  - Sources: `reviews/observability1.md`.

- `[Medium] Decide whether `queryPlan` and related metadata are observability sketches or real invalidation models, and rename/strengthen accordingly.`
  - The current query plan is useful as a descriptive sketch, but it does not capture real semantic dependencies such as imports, trait solving, macro expansion, host bindings, or backend runtime dependencies.
  - If it is public observability only, name it that way. If it is intended for incremental reuse, strengthen it materially.
  - Sources: `reviews/observability1.md`.

- `[Low-Medium] Expand CLI/tooling exposure for the observability APIs, or explicitly keep them library-only.`
  - The public API exposes checkpoint contracts, available checkpoints, all-checkpoint verification, runtime obligations, query plans, fingerprints, incremental units, and pipeline traces.
  - The CLI exposes only a smaller subset.
  - This is not a spec failure by itself, but the practical tooling story is weaker unless the public/CLI split is deliberate.
  - Sources: `reviews/observability1.md`.

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
  - Unicode warning emission for bidi controls / confusables / non-normalized source text once implemented.
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
  - `Bytes` semantics and invalid UTF-8 decoding.
  - `&&` / `||` with side-effecting RHS.
  - Nested `using` cleanup under abrupt exit.
  - Closure capture on the CLR path.
  - Prefixed strings through backend profiles.
  - Unsupported runtime terms such as `fork`, `sleepFor`, `atomically`, `newTVar`, and effect handlers should fail with capability diagnostics rather than later emitter crashes.
  - Sources: `reviews/backend1.md`.

- `[High] Add Unicode / bytes / hash regression tests around the newly reviewed failure modes.`
  - Valid and invalid byte/scalar/grapheme literals.
  - `decodeUtf8` success and invalid-byte failure behavior.
  - Scalar-order string comparison with supplementary characters.
  - `words` / `sentences` behavior once implemented or a profile-capability rejection if not.
  - `HashState`-returning APIs must return `HashState`, not `Some HashState`.
  - Structural hash framing tests such as `("a","bc")` vs `("ab","c")` once structural hashing exists.
  - Strings containing U+0000 should behave consistently between interpreter and compiled backends.
  - Sources: `reviews/unicode1.md`.

- `[High] Add trait/instance/deriving tests for the missing semantic cases, not just simple closed dispatch.`
  - Constrained instances carrying premise evidence.
  - Supertrait member projection through real evidence/dictionaries.
  - Imported multi-parameter trait instance metadata.
  - Parenthesized multi-constraint instance headers.
  - Rigid-goal-vs-instance-head matching behavior.
  - Constrained trait members with additional constraints.
  - Local instances/local deriving, if those remain in scope.
  - Real `derive` behavior once implemented, through the same validation/coherence/runtime path as handwritten instances.
  - Sources: `reviews/traits1.md`.

- `[High] Add targeted .NET backend tests for the IL/runner-specific issues.`
  - String pattern matching with dynamically constructed strings.
  - String equality on distinct but content-equal string instances.
  - Or-pattern semantics.
  - `printlnString`.
  - Generic constructor inside a generic function.
  - `host.dotnet` binding through a generated runner process, not just in-process test loading.
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
