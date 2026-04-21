# Spec Alignment Plan

## 1. Baseline audit

Preferred resolution: neither yet; establish the exact alignment map first.

- [x] Build a section-by-section alignment matrix for the parts of `Spec.md` that the compiler or tests currently exercise.
- [x] Classify each reviewed section as one of: aligned, compiler divergence, spec/docs divergence, or not implemented yet.
- [x] Keep "not implemented yet" separate from "implemented but contradictory", so we do not blur roadmap gaps with actual conformance bugs.

Current audit scope: this is an initial baseline for sections exercised by source modules, fixtures, backend tests, milestone tests, observability tests, and the M3 blueprint as of the start of M3. It is not a claim that unlisted spec sections are conforming.

| Spec area | Exercised by | Classification | Notes |
| --- | --- | --- | --- |
| 2.1 Modules and files | `spec_2_1_*` fixtures | aligned | Module headers, path/header mismatch, and invalid path checks are covered. |
| 2.2 Acyclic imports | `spec_2_2_*` fixtures | aligned | Import and export cycle rejection is covered. |
| 2.3 Imports | `spec_2_3_*` fixtures and backend import tests | aligned | Local wildcard, selective, alias, multiple import specs, and imported runtime bindings are covered. |
| 2.3.1 Import item qualifiers | `spec_2_3_1_*` fixtures | not implemented yet | Term/type/constructor namespace boundaries are partially covered; `type T(..)` and more explicit constructor boundary cases still need tests and implementation confirmation. |
| 2.3.1.1 `unhide` and `clarify` | `spec_2_3_1_1_*` fixtures | aligned | Positive modifier behavior is covered for the current parser/resolver subset. |
| 2.3.2 URL imports | `spec_2_3_2_*` fixtures | not implemented yet | URL syntax and virtual fixture resolution are exercised; full pinning, reproducibility, and external fetch semantics are not implemented. |
| 2.4 Exports | `spec_2_4_*` fixtures | aligned | Direct exports and re-export fixtures are covered. |
| 2.5 Visibility and opacity | `spec_2_5_*` fixtures | compiler divergence | Visibility is exercised, but private access is still surfaced through runtime/interpreter failure in at least one fixture, and opaque definitional transparency is not enforced as a full typechecking boundary. |
| 2.6 Prelude interface | `implicit_prelude`, `spec_2_6_*`, import tests | aligned | Implicit prelude import behavior and the fixed unqualified constructor subset are covered. |
| 2.7 Prelude contents | `spec_2_7_*`, `Stdlib/std/prelude.kp` | spec/docs divergence | The repo uses a bootstrap prelude contract that is tested, but the spec still presents the larger normative minimum without an explicit bootstrap profile split. |
| 3.1-3.5 Lexing, whitespace, operators, and fixity | `spec_3_*` fixtures, backend user-operator tests | aligned | Identifiers, keywords, comments, indentation, user-defined operators, operator sections, and fixity are covered for the current surface. |
| 4.1-4.5 Literals | `spec_4_*` fixtures | not implemented yet | Numeric, boolean-as-constructors, string escape/interpolation, char, and unit basics are covered; raw strings, multiline fixed dedent, prefixed strings, suffixes, and type-level string parsing remain incomplete. |
| 5.1.3 Constraints and dictionaries | M2 trait/dictionary tests | not implemented yet | M2 dictionary passing is real for simple constraints, but full constraint solving, coherence, proof irrelevance, and supertrait behavior are not implemented. |
| 5.1.5-5.1.7 Quantities and borrowing | M3 blueprint/plan | not implemented yet | No current compiler path enforces owned, borrowed, region, or place-based resource rules. This is the main M3 work. |
| 5.7 Monadic splicing | M2 `!(...)` usage | not implemented yet | The M2 do-block splice form is implemented for the target shape; general monadic contexts and broader typing rules are incomplete. |
| 6.1-6.2 Declarations and signatures | `spec_6_1_*`, `spec_6_2_*`, backend typed binding tests | aligned | Signature-only and signature-plus-definition basics are covered. |
| 6.5 `expect` declarations | `spec_6_5_*`, observability intrinsic tests | not implemented yet | Backend-scoped intrinsic satisfaction is implemented; build-cache identity and the full elaboration-available intrinsic boundary remain incomplete. |
| 7.1 Variables, application, and dotted forms | `spec_7_1_*` fixtures, backend call tests | not implemented yet | Ordinary application and module qualification are covered; full projection, safe navigation, method sugar, and application-boundary subsumption are incomplete. |
| 7.2 Lambdas and closure capture | `spec_7_2_*` fixtures | not implemented yet | Interpreter closure behavior is covered; quantity-aware capture and backend closure support are incomplete. |
| 7.4 Conditionals | `spec_7_4_*` fixtures | aligned | Basic pure and runtime conditional behavior is covered. |
| 7.5-7.6 Match expressions and patterns | `spec_7_5_*` fixtures, M1 backend tests | not implemented yet | Basic ADT matches and non-exhaustive runtime failure are covered; indexed exhaustiveness, `impossible`, or-patterns, active patterns, and QTT pattern interactions are incomplete. |
| 8.2 Do blocks | `spec_8_2_*`, M1/M2 milestone tests | not implemented yet | Basic sequencing, bind result, `printInt`, and M2 stateful do paths are covered; labels, local declarations, abrupt completion, `defer`, and `using` are incomplete. |
| 8.5 Loops and mutable variables | M2 tests | not implemented yet | `var`, assignment, `while`, and `Ref` desugaring work for M2; `for`, loop `else`, scoped region escape checks, and full normative loop elaboration are incomplete. |
| 8.7 Exit actions and `using` | M3 blueprint/plan | not implemented yet | Deterministic cleanup and completion-aware exit actions are not implemented. |
| 8.8 `inout` and `~` | M3 blueprint/plan | not implemented yet | No parser, elaboration, or backend support exists yet. |
| 11.1 and 11.3 ADTs and type aliases | `spec_11_*`, M1 backend tests | not implemented yet | Simple ADTs, constructor lowering, and aliases are covered; GADTs, named constructor arguments, and full well-formedness are incomplete. |
| 12.1-12.3 Traits and instances | `spec_12_*`, M2 tests | not implemented yet | Basic traits, members, instances, dictionary lowering, and direct calls are covered; supertraits, overloaded member ambiguity, full instance resolution, termination, and deriving are incomplete. |
| 14.2 Dynamic semantics | `spec_14_2_*` fixtures, interpreter tests | aligned | Current interpreter behavior for the exercised subset, including short-circuiting and selected runtime negatives, is covered. |
| 14.4 Erasure | backend tests and M3 plan | not implemented yet | Current backends erase existing bootstrap-only metadata implicitly; QTT quantity/region erasure is not implemented because the metadata does not exist yet. |
| 17.1 Observability and verification | `ObservabilityTests.fs` | aligned | Checkpoint names, traces, JSON/S-expression dumps, and verifier entry points are covered for current stages. |
| 17.2 KFrontIR | `ObservabilityTests.fs`, fixture assertions | not implemented yet | Phase snapshots exist; the full query model, lazy resolution, tooling queries, invalidation, and cancellation model are not implemented. |
| 17.3 KCore | `ObservabilityTests.fs`, recent KCore path work | not implemented yet | KCore dumps and selected path operations exist; semantic object stores, elaboration-time evaluation, and full application-spine semantics are incomplete. |
| 17.4 KBackendIR | backend verifier tests, ZigCc and CLR lowering | compiler divergence | The current `KBackendIR` is useful and verified for the current backends, but it is not yet the full runtime-facing representation described by sections 17.4 and 17.4.1. |
| 17.6 Backend intrinsics and `expect` | intrinsic metadata tests, `Stdlib.fs` | not implemented yet | Backend-profile intrinsic sets are implemented; cache identity and full frontend/backend extension boundaries remain incomplete. |
| 17.8 Native backend profile (`zig`) | ZigCc backend tests, M1/M2 milestone tests | aligned | The standardized `zig` profile lowers from `KBackendIR` to generated C compiled by `zig cc` for the M1/M2 subset. |
| 17.10 CLR backend profile (`dotnet`) | dotnet backend tests, M1/M2 milestone tests | aligned | Public `dotnet` emits managed CLR artifacts containing CIL/metadata for the M1/M2 subset; hosted dotnet remains explicit bootstrap support. |
| Appendix T Standard test harness | Appendix T fixtures and harness code | compiler divergence | The harness supports `.kp` fixtures, `suite.ktest` basics, and extension directives, but many standard directives/assertions, stable diagnostic codes, and incremental suites are missing. |

## 2. Public `dotnet` backend profile

Preferred resolution: adjust the compiler.

- [x] Align the public `dotnet` backend with `Spec.md` section 17.10 so it lowers to real CLR artifacts containing CIL and metadata.
- [x] Repoint the public entry points that currently call `Backend.emitDotNetArtifact` and CLI `--backend dotnet` away from the hosted source-generating path and onto the real CLR backend.
- [x] Keep the hosted backend only as an explicitly nonstandard/bootstrap profile, renamed so it is not confused with the standardized `dotnet` profile.
- [x] Add conformance tests that compile and run the same program through the public `dotnet` profile and inspect the emitted managed artifact shape.

## 3. `KBackendIR` shape and lowering semantics

Preferred resolution: adjust the compiler.

- [ ] Audit the current `KBackendIR` model in `src/Kappa.Compiler/Compilation.fs` against `Spec.md` sections 17.4 and 17.4.1.
- [x] Decide whether to evolve the current `KBackendIR` into a true runtime IR or to insert a new explicit runtime IR and rename the current form.
- [ ] Introduce runtime-facing constructs for representation choice, runtime calls, data layout, field access, retained dictionaries/type parameters, and explicit runtime control.
- [x] Strengthen `KBackendIR` verification so it checks the legality conditions from section 17.4.2 instead of only structural uniqueness checks.
- [ ] Extend observability so `KBackendIR` dumps and post-`KBackendIR` CLR-lowering dumps expose the runtime information required by the spec.
- [x] Add a second post-`KBackendIR` lowering path using the standardized `zig` profile (implemented initially as generated C compiled by `zig cc`) so the runtime/data-layout boundary is exercised by more than the CLR backend.
- [x] Make the native lowering path consume `KBackendIR` directly rather than bypassing it through the interpreter or hosted-source runtime.
- [x] Keep the first native slice intentionally small but real: zero-argument entry points, recursive top-level functions, closures needed by `do` desugaring, tagged ADT allocation/matching, integer arithmetic/comparison, and `printInt`.
- [x] Expose at least one post-`KBackendIR` native-lowering checkpoint once the first `zig` lowering slice exists, so target-specific debugging is not CLR-only.

## 4. `expect` satisfaction and backend intrinsics

Preferred resolution: adjust the compiler.

- [x] Replace the hardcoded intrinsic satisfaction logic in `src/Kappa.Compiler/Stdlib.fs` with backend-profile-scoped intrinsic registries.
- [ ] Model the selected backend profile and backend-intrinsic set as part of the effective build configuration and cache identity, as required by section 17.6.
- [ ] Decide which intrinsics are elaboration-available and enforce that distinction during elaboration-time evaluation.
- [x] Add tests showing that the same `expect` can be satisfied or rejected depending on the selected backend profile and intrinsic set.

## 5. Prelude import semantics

Preferred resolution: adjust the compiler.

- [x] Fix implicit prelude handling so it matches sections 2.6 and 2.3.1 exactly: wildcard import plus only the fixed unqualified constructor subset.
- [x] Audit wildcard import resolution so constructors are not imported unqualified except where the spec explicitly permits it.
- [ ] Add tests covering the boundary between term/type imports and constructor imports, including explicit `ctor` imports and `type T(..)` imports.
- [ ] Re-check existing prelude fixtures after the import rules are corrected, because some current tests may be passing only because constructor import is too permissive.

## 6. Prelude contents and bootstrap surface

Preferred resolution: adjust the spec or split the spec surface into profiles.

- [ ] Compare `src/Kappa.Compiler/Stdlib/std/prelude.kp` against the normative minimum in section 2.7 and produce a missing-items list.
- [ ] Decide whether M1 really intends the full section 2.7 prelude or a smaller bootstrap prelude.
- [ ] If the goal is the full spec surface, expand the compiler prelude and intrinsic/runtime support until the missing mandatory names exist.
- [ ] If the goal is a smaller bootstrap surface, update `Spec.md` to define that bootstrap prelude explicitly instead of leaving the current reduced prelude as an undocumented deviation.
- [x] Add tests that assert exactly the chosen prelude contract, so the bootstrap/full distinction is machine-checked.

## 7. Appendix T standard harness

Preferred resolution: adjust the compiler and harness, and narrow any claims until that work is done.

- [ ] Stop treating the current harness as Appendix-T-conformant until the missing standard directives are implemented.
- [ ] Add support for `suite.ktest` and `incremental.ktest`, including suite-level configuration and incremental step execution.
- [ ] Implement the standard configuration directives from Appendix T: `mode`, `packageMode`, `scriptMode`, `backend`, `entry`, `runArgs`, `stdinFile`, `dumpFormat`, and `requires`.
- [ ] Introduce stable diagnostic codes in compiler diagnostics so `assertDiagnostic`, `assertDiagnosticNext`, `assertDiagnosticAt`, and inline `--!!` markers are actually possible.
- [ ] Make `assertType` compare elaborated types by definitional equality rather than by token-text equality.
- [ ] Implement the remaining standard assertions we do not support yet, including file-relative diagnostic assertions, `assertFileDeclKinds`, stage-dump assertions, pipeline-trace assertions, and incremental cross-step assertions.
- [ ] Keep existing `x-...` directives explicitly marked as extensions rather than letting them silently stand in for missing standard behavior.

## 8. Recommended execution order

Preferred resolution: sequence the work so public behavior becomes honest first, then broaden conformance.

- [x] Fix the public-profile mismatches first: `dotnet` backend routing, implicit prelude import semantics, and backend-scoped `expect` handling.
- [x] Before starting M3 implementation work, align the 17.1-17.6 pipeline contracts: named checkpoints, trace/dump semantics, KFrontIR/KCore/KBackendIR shape, portable runtime obligations, and backend-intrinsic build identity.
- [ ] Then align the internal architecture: true `KBackendIR`, stronger verifier rules, and post-`KBackendIR` target-lowering checkpoints.
- [x] In parallel with that architectural cleanup, stand up the first real native path under the standardized `zig` profile by lowering `KBackendIR` to generated C and compiling it with `zig cc`.
- [ ] Use the first `zig` slice to pressure-test what still belongs in `KBackendIR` versus what is really target-specific lowering state before expanding the CLR backend further.
- [ ] Then decide whether section 2.7 stays normative for the current milestone or whether the spec needs a bootstrap prelude/profile split.
- [ ] Finally, bring the test harness up to Appendix T and convert more of the existing suites to the standard directive set.

Current milestone status note: M2 execution is complete, and the checkpoint/build-identity portion of the 17.1-17.6 cleanup is no longer a blocker for starting M3. The compiler now normalizes the effective backend profile, verifies `KBackendIR` before native emission, lowers the standardized `zig` profile directly from `KBackendIR`, exposes post-`KBackendIR` `zig.c` and `dotnet.clr` checkpoints with verification and stage dumps, and runs the M2 target shape on the interpreter, `zig`, and public CLR-backed `dotnet` profiles. The query substrate is being added before M3 resource-state work so QTT metadata has a stable analysis-session/query contract.

## 9. Milestone 2 (`Traits` + `Ref` + `while`)

Preferred resolution: adjust the compiler.

- [x] Extend the surface syntax and parser for `instance` declarations, `var`, assignment forms, `while ... do ...`, and monadic splice `!(...)` inside `do`.
- [x] Introduce an explicit M2 elaboration layer that rewrites constrained functions to explicit dictionary parameters, synthesizes dictionary artifacts for trait declarations and instances, and resolves instance evidence at call sites.
- [x] Desugar mutable-variable forms through `newRef` / `readRef` / `writeRef` with the uniform reference semantics required by `Spec.md` section 8.5.1.
- [x] Lower `while ... do ...` through an internal recursive helper form that works on both real backends without depending on the hosted interpreter path.
- [x] Extend the intrinsic/builtin surface with the M2 runtime contract (`MonadRef IO`, `newRef`, `readRef`, `writeRef`, `primitiveIntToString`, `printString`, and the concrete `Ref`/dictionary runtime support needed by the backends).
- [x] Make the standardized `zig` profile compile and run the M2 target end-to-end.
- [x] Make the public CLR-backed `dotnet` profile compile and run the M2 target end-to-end.
- [x] Add direct compiler/backend tests that execute the M2 program shape on both `zig` and `dotnet`, then keep them green while refactoring.

Current M2 status note: the interpreter, standardized `zig` backend, and public CLR-backed `dotnet` backend all compile and run the M2 milestone program shape end-to-end.

## 10. Pre-M3 priority: pipeline contracts (`Spec.md` 17.1-17.6)

Preferred resolution: adjust the compiler before adding M3 ownership semantics, because QTT erasure and resource-safe lowering depend on stable checkpoint and IR contracts.

- [x] Audit sections 17.1-17.6 against the current implementation and tests, with explicit notes for `KRuntimeIR` as an implementation-defined intermediate versus spec-named checkpoints.
- [x] Define the canonical pipeline graph and checkpoint contract for `surface-source`, `KFrontIR.*`, `KCore`, `KRuntimeIR`, `KBackendIR`, and post-`KBackendIR` target units.
- [x] Decide whether current `KRuntimeIR` remains an implementation-defined post-KCore form or should be folded into a spec-shaped `KBackendIR` lowering sequence.
- [x] Decide whether current `KBackendIR` evolves into the true runtime-facing IR from sections 17.4 and 17.4.1, or whether it should be renamed and a new spec-shaped `KBackendIR` inserted.
- [x] Make stage dumps expose the information required by 17.1.3-17.1.6 without relying on backend-specific implementation details.
- [x] Add or update checkpoint verification so legality witnesses cover KFrontIR, KCore, KRuntimeIR if retained, KBackendIR, and target-lowering checkpoints consistently.
- [x] Add a post-`KBackendIR` CLR target checkpoint and dump beside the existing `zig.c` target checkpoint, so target-specific debugging is not ZigCc-only.
- [x] Model selected backend profile, intrinsic set, and elaboration-available intrinsic set as part of the effective build configuration/cache identity required by 17.1.2 and 17.6.
- [x] Thread deployment mode into artifact-level build configuration identity where target emission has deployment-specific behavior.
- [x] Clarify portable runtime obligations from 17.5 in the compiler model: which obligations are guaranteed by KBackendIR, which are backend-specific, and which are still out of scope.
- [x] Add regression tests that compare pipeline trace, checkpoint availability, dump shape, verification behavior, and backend identity for interpreter, ZigCc, and CLR dotnet profiles.
- [x] Only start M3 QTT implementation after this track has either resolved the discrepancy or documented a deliberate spec adjustment.
- [x] Add the `KFrontIR.MODAL_SOLVE` phase from section 17.2.2 so the observable phase lattice matches the current spec.
- [x] Introduce a minimal analysis-session and query-plan model for sections 17.2.3, 17.2.6, and 17.2.7, derived from the current eager pipeline rather than claiming full lazy incremental evaluation.
- [x] Add stable diagnostic codes and diagnostic stage/phase metadata required by section 17.2.4.
- [x] Introduce compiler-fingerprint/incremental-unit metadata for sections 17.1.1, 17.1.2, 17.2.6, and 17.2.7.
- [x] Keep full lazy per-query execution, fine-grained invalidation, editor queries, and semantic-object-store browsing as later implementation work; do not block the first M3 parser/typechecker slice on those.

Current 17.1-17.6 status note: the checkpoint/build-identity prerequisite for M3 is resolved enough to begin QTT work without knowingly building on a contradictory checkpoint model, but the broader query/incremental model remains intentionally partial. Completed cleanup slices: both ZigCc and CLR now expose post-`KBackendIR` target checkpoints with dumpable manifests; stage metadata now carries an effective build configuration identity including backend profile, intrinsic sets, and deployment mode; the compiler now exposes a typed checkpoint contract, includes that contract in JSON/S-expression stage dumps, verifies all contract checkpoints through one API, exposes a portable runtime-obligation classification, includes `KFrontIR.MODAL_SOLVE`, exposes a minimal analysis-session/query-plan surface over the eager pipeline, includes stable diagnostic code/stage/phase metadata in diagnostic dumps, and exposes deterministic compiler-fingerprint plus conceptual incremental-unit metadata for the eager pipeline. Current decisions: retain `KRuntimeIR` as an implementation-defined checkpoint under section 17.4.9; evolve the current `KBackendIR` in place into the full spec-shaped runtime IR rather than renaming it or inserting another public `KBackendIR`; keep all public target-lowering checkpoints consuming `KBackendIR`; defer true lazy query execution, cache reuse, editor-only queries, and semantic-object-store browsing until they are needed by later milestones.

## 11. Milestone 3 (`QTT` + borrowing + deterministic resources)

Preferred resolution: adjust the compiler, keeping QTT information explicit through elaboration and erased before backend-specific lowering.

- [x] Start with data-driven M3 tests before implementation: one positive `using`/linear-file program and at least three negative programs for dropped owned resources, duplicated owned resources, and borrowed-region escape through a returned closure.
  The negative tests should assert stable diagnostic codes, not prose-only matches: `E_QTT_LINEAR_DROP`, `E_QTT_LINEAR_OVERUSE`, and `E_QTT_BORROW_ESCAPE`.
  Add two immediate mirrors: a downward-only closure that captures a `using` borrow and is called inside the protected scope must pass, and `if cond then consume x else consume x` for `x : 1 T` must pass because branch usage joins rather than adds.
  Add single-application overuse coverage such as `h x x`, not only sequential `consume x; consume x`.
- [x] Implement the M3 diagnostic-origin contract for the supported BODY_RESOLVE subset: dropped owned resources use the unconsumed binder as primary origin; overuse uses the second consuming site as primary with the first consume and binder as related origins; borrow escape uses the escape site as primary with borrow/capture related origins.
- [x] Replace the current source-text origin recovery in the resource checker with parser-carried spans before broadening the checker beyond root-place do-body forms.
  The current implementation uses lexer/parser token spans for binder and use origins, so comments and string literal text no longer affect resource diagnostics.
- [x] Audit `Spec.md` sections 5.1.5-5.1.7, 7.1.3, 8.7.4, 8.8, 14.4, and the relevant KCore sections before changing the typechecker, then record any spec/compiler mismatch discovered during implementation.
- [x] Extend the lexer/parser for quantity binders (`0`, `1`, `&`, and `omega`), borrowed binders (`(& x : T)` and `(&[s] x : T)`), `using pat <- expr`, `inout`, and `~` call-site syntax.
- [x] Add data-driven syntax fixtures that prove quantity annotations round-trip into the pre-elaboration observable forms without changing runtime semantics.
- [x] Introduce typed surface quantity metadata rather than representing quantities as ad hoc strings on binders.
- [x] Define the M3 checker as a `KFrontIR.BODY_RESOLVE` sub-pass/query over resolved declarations, not as a parallel semantic oracle.
  The first supported subset is resolved do bodies, `let`/`let <-` bind patterns, `using`, root-variable stable places, named function parameters, simple lambdas, resolved maximal application sites with known explicit binder quantities, and conditionals needed for join tests.
  Unsupported forms in the supported body must be represented as `unknown`/`deferred` ownership facts or rejected conservatively; they must never be silently treated as ownership-neutral.
- [ ] Introduce a resource model with explicit invariants: interval quantities are separate from borrow mode; place is root plus path even when the first implementation only constructs root paths; borrow regions distinguish explicit user regions from anonymous rigid regions and carry owner scope; `using` creates one region per protected scope, not per bound name.
  First cleanup split is in place: `ResourceModel.fs` owns the current resource places, borrow regions, bindings, and checker context so future context-splitting work has a separate model module. The full quantity algebra and active-using obligation model remain open.
- [ ] Add first-class ownership events (`ResourceUse`/`OwnershipEvent`) for consume, borrow, capture, move, copy-forbidden, and escape attempts so diagnostics and dumps are not inferred from final binding state alone.
- [x] Add the first ownership event model for the supported subset: consume, borrow, capture, and escape events are recorded and serialized from BODY_RESOLVE. Move and copy-forbidden events are still part of the full resource-context work above.
- [ ] Replace the simple name-to-type typing environment in the affected checker paths with a lexical resource context that tracks binder kind, origin, declared quantity, inferred demand or borrow obligation, current place, region environment, availability/consumption state, and active `using` obligations.
- [ ] Implement syntax-directed context splitting and consumption for applications, `let`, `do`, `match`, lambdas/closures, and control-flow joins.
- [ ] Implement the quantity satisfaction relation from `Spec.md`, including the strict separation between owned (`1`) and borrowed (`&`) obligations.
  First strict-separation rules are covered: passing a borrowed `using` binding or a local `let &`/bind-pattern borrow to a consuming `1` parameter now reports `E_QTT_BORROW_CONSUME`.
- [ ] Implement stable-place analysis and borrow introduction for variables, parameters, hidden temporaries, and the initial field/path subset required by M3.
- [ ] Implement skolem borrow regions for `using` and borrowed binders, including closure-capture tainting and non-escape diagnostics.
  First local-binder slice is in place: borrowed do `let`/bind patterns introduce a checker-visible borrow region and attach it to all names bound by the pattern.
  Borrowed-parameter closure escape is now rejected for direct lambda results in the supported function-body subset.
- [ ] Implement deterministic release scheduling for `using pat <- expr` as the primitive split from the spec: keep one hidden owned resource in the scope frame, expose only borrowed pattern bindings in the protected body, create one shared rigid region for the whole pattern, and schedule exactly-once release on all exits.
  Do not satisfy this by source-expanding to `defer`; the early checker and lowering model must preserve the protected-scope primitive.
- [x] Add the initial KCore representation for protected-resource lowering: `DoScope`, `ScheduleExit`, and `Release` are now observable at `KCore`, and `using` lowers to a hidden owned binding plus a borrowed alias inside the scheduled protected body.
- [x] Preserve protected-resource actions across the implementation-defined `KRuntimeIR` boundary so backend lowering can target the same `DoScope`/`ScheduleExit`/`Release` structure instead of reconstructing cleanup from source.
- [x] Add `Releasable IO a` dictionary/evidence handling for the `using` KCore lowering path, reusing the existing M2 trait machinery where possible.
  The current slice generalizes lightweight trait constraints/instances to multiple arguments, records selected `Releasable m a` evidence in the KCore `Release` action, and has first normal-exit cleanup execution coverage in the interpreter plus CLR/Zig backends.
- [x] Implement the M3 intrinsic/runtime contract initially as backend-profile intrinsics: `openFile`, `readData`, `primitiveCloseFile`, and any minimal file-handle test shim needed by the positive target.
  The first shim is intentionally deterministic for tests: `openFile` produces a File-like handle, `readData` returns `"chunk"`, and `primitiveCloseFile` writes `"closed"`.
- [ ] Implement `inout` / `~` as a type-directed elaboration rewrite after the resource context exists, not as parser-only sugar.
- [ ] Define and implement a canonical ownership-fact dump schema at `KFrontIR.BODY_RESOLVE` and `KFrontIR.CHECKERS`.
  Required deterministic fact groups: bindings (`id`, origin, kind, declared quantity, inferred demand/borrow obligation, state), uses/events (origin, use kind, target binding/place), borrow regions (id, explicit vs anonymous, introduction origin, owner scope), `using` scopes (surface origin, hidden owned binding, shared region, hidden release obligation), closures (capture set, inferred quantity, inferred region environment, escape status), and diagnostics (stable code, primary origin, related origins).
  Unknown or unsupported facts must be emitted explicitly as `unknown`/`deferred`, not omitted.
- [x] Add the initial BODY_RESOLVE ownership dump for the supported subset in JSON and S-expression stage dumps, including deterministic bindings, uses, borrow regions, using scopes, closures, deferred list, and diagnostic-code list.
- [x] Record explicit deferred ownership facts for unsupported control-flow/resource analyses in the current subset, starting with `while-resource-fixed-point` and `match-pattern-resource-checking`.
- [x] Add checkpoint-level tests for QTT metadata once the resource checker starts attaching ownership, borrow-region, and place facts to KFrontIR/KCore observability.
- [x] Preserve QTT metadata through KCore observability so tests can assert ownership and borrow decisions before erasure.
  KCore module dumps now carry the same pre-erasure ownership fact groups as BODY_RESOLVE, and tests assert local borrowed binding region facts before backend erasure.
- [ ] Erase quantities, regions, and borrow-only metadata before `KBackendIR`, while preserving ordinary runtime values and retained backend generics/dictionaries.
- [ ] Strengthen checkpoint verification so post-erasure IR rejects leaked quantity, borrow, region, or place-only constructs.
- [x] Lower `using` exit actions in the CLR backend to `try` / `finally`, calling the selected `Releasable` dictionary's release implementation exactly once.
  The first regression covers normal completion and exception unwinding from the protected body.
- [ ] Lower `using` exit actions in the ZigCc backend with equivalent cleanup behavior, even if the first implementation uses a small generated-C runtime shim.
  First normal-exit cleanup invocation is implemented through the shared `KBackendIR` lowering path; cleanup on abrupt exits remains open.
- [x] Add regression tests that execute the M3 positive program on the interpreter, standardized `zig`/ZigCc backend, and public `dotnet` profile, then keep all three green while refactoring.
- [x] Add the first interpreter execution regression for `using`: the protected body runs, then the selected `Releasable` release action runs once, preserving the body result.
- [ ] Stop for a cleanup pass after the first green M3 slice: separate resource-context logic, borrow-region logic, and backend cleanup lowering so M4 effect-handler work does not inherit a monolith.
  First cleanup pass completed for resource model extraction and token-origin diagnostics. Backend cleanup lowering and full borrow-region logic still need additional separation.

Current M3 status note: started with the source/KFrontIR surface slice and the first enforceable BODY_RESOLVE ownership slice. The compiler now parses and preserves typed quantity metadata for function parameters and do-bind patterns, parses borrowed binders with optional explicit regions, parses `using pat <- expr` with the spec-required rejection of explicit quantity markers, and parses `inout` parameters plus `~` call-site markers. Data-driven fixtures cover those surfaces plus the required M3 resource-checker contract cases: dropped owned resource, sequential overuse, single-application overuse, borrowed `using` and local `let &` consumption, borrowed-region escape through do-block closures and borrowed-parameter direct lambda results, downward-only borrowed closure, and branch-join consumption. The first resource checker enforces linear drop/overuse, rejects consumption of checker-visible borrowed bindings, and handles basic using/local-borrow/parameter-borrow region closure escape for the supported root-place subset, also reading simple quantity obligations from top-level signatures and `expect term` signatures. It reports stable diagnostic codes at `KFrontIR.BODY_RESOLVE`, records consume/borrow/capture/escape ownership events, exposes initial ownership fact groups in KFrontIR and KCore JSON/S-expression dumps, reports primary plus related origins for the supported M3 diagnostics using parser token spans instead of source-text string scanning, and emits deferred ownership facts for unsupported loop/match analyses instead of omitting them. KCore and KRuntimeIR now preserve the first protected-resource shape with `DoScope`, `ScheduleExit`, and `Release`; `using` release actions can select multi-parameter `Releasable m a` evidence at KCore. The interpreter, standardized `zig`/ZigCc backend, and public CLR-backed `dotnet` profile all execute the first normal-exit `using` cleanup regression using backend-profile file-resource intrinsics. The CLR backend also lowers scheduled exits to real `try` / `finally` and has exception-unwinding coverage. The remaining backend gap is ZigCc cleanup during abrupt unwinding/completion propagation.
