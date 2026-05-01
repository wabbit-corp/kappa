# Open Alignment Plan

Backlog provenance note:
The sections below are the active execution plan. `TODO.md` remains the consolidated review backlog, and new additive items pulled from it should keep an explicit provenance trail back to `TODO.md` and the relevant `reviews/*.md` files.

## 1. `KBackendIR` shape and lowering semantics

- [ ] Audit the current `KBackendIR` model in `src/Kappa.Compiler/KBackendIR.fs` and its lowering path against `Spec.md` sections 17.4 and 17.4.1.
- [ ] Introduce runtime-facing constructs for representation choice, runtime calls, data layout, field access, retained dictionaries/type parameters, and explicit runtime control.
- [ ] Keep record-typed runtime values as dedicated record carriers through backend lowering rather than re-encoding them as nested `Res` pairs; for the CLR backend, follow up by mapping synthesized record carriers onto a native record/class/struct representation with predictable field layout so query-row temporaries and ordinary records do not pay avoidable allocation or indirection costs.
- [ ] Extend observability as `KBackendIR` grows so `KBackendIR` dumps and post-`KBackendIR` CLR-lowering dumps continue to expose the graph/provenance/runtime information required by sections 17.4.3-17.4.4.

## 2. `expect` satisfaction and backend intrinsics

- [ ] Record a stable implementation identity for each backend intrinsic in emitted artifacts, and include that identity in any hashing or coherence decision that depends on the intrinsic, as required by section 17.6.
- [ ] Decide which intrinsics are elaboration-available and enforce that distinction during elaboration-time evaluation.

## 3. Prelude import semantics

Status note:
Explicit term/type/constructor import-boundary coverage already exists in the fixture suite, including explicit `ctor` imports and `type T(..)` cases. No separate prelude-import follow-up remains here; future import work should add coverage under section 14.

## 4. Prelude contents and bootstrap surface

- [ ] Compare `src/Kappa.Compiler/Stdlib/std/prelude.kp` against the normative minimum in section 2.6.2 and produce a missing-items list.
- [ ] Decide whether M1 really intends the full section 2.6.2 prelude or a smaller bootstrap prelude.
- [ ] If the goal is the full spec surface, expand the compiler prelude and intrinsic/runtime support until the missing mandatory names exist.
- [ ] If the goal is a smaller bootstrap surface, update `Spec.md` to define that bootstrap prelude explicitly instead of leaving the current reduced prelude as an undocumented deviation.

## 5. Appendix T standard harness

- [ ] Finish the remaining Appendix-T gaps without overstating current conformance. Already implemented: `suite.ktest`, `incremental.ktest`, `mode`, `packageMode`, `scriptMode`, `backend`, `entry`, `runArgs`, `stdinFile`, `dumpFormat`, `requires`, `assertDiagnostic*`, `assertType` by definitional equality, `assertFileDeclKinds`, and trace-count assertions.
- [ ] Implement the remaining standard assertions we still lack, especially stage-dump assertions and any incremental cross-step assertions that are still outside the current harness model.
- [ ] Keep existing `x-...` directives explicitly marked as extensions rather than letting them silently stand in for missing standard behavior.

## 6. Milestone 4 (`Effects` + handlers + row polymorphism)

- [ ] Implement row-polymorphic effect solving for `EffRow`, including normalization plus `SplitEff`-style reasoning instead of the current single-label scoped-effect approximation.
- [ ] Type handler clauses against the handled computation precisely, including remainder-row reasoning and resumption types derived from the clause result type instead of the current conservative `TypeArrow(ω, opResult, handledType)` approximation.
- [ ] Lower one-shot handlers through `KBackendIR` and into at least the public CLR-backed `dotnet` profile; interpreter-only support is not sufficient to close M4.

Current M4 status note: started, not complete. The compiler now has a real effect-handler kernel in `KCore` / `KRuntimeIR`, one-shot scoped-effect execution in the interpreter, parser support for multiline handler clause bodies, imported/top-level effect-label resolution across module boundaries, an ownership-checked multi-shot capture rejection for live linear or borrowed continuation suffixes, and checkpoint coverage that rejects surviving handler runtime constructs or leaked `Eff`/effect-row metadata before `KBackendIR`.

## 7. Symbolic names, spelling, and semantic identity

- [ ] Finish propagating declaration-level and semantic-object identities beyond frontend elaboration and the internal CLR model, so `KRuntimeIR`, `KBackendIR`, and other target backends stop collapsing resolved declarations, constructors, traits, effect labels, projections, and reified static objects back to plain `string`.
- [ ] Replace visible-name environments that collapse declaration kinds into plain `Map<string, ...>` lookup with an explicit binding-group model matching section 2.8.
  Ordinary term lookup and ambiguity now route through `VisibleOrdinaryGroups`, and qualified type/trait/static-object resolution now goes through structured `VisibleStaticGroups`, `VisibleQualifiedTypeFacets`, `VisibleQualifiedTraits`, and `VisibleModulePaths`.
  The remaining gap is the deeper same-spelling binding-group model itself: type/trait/static-object membership is still assembled from split maps instead of one first-class binding-group representation.
- [ ] Rework same-spelling data-family handling so it is represented as one binding group with typed facets instead of separate text-keyed maps plus special cases.
  Unqualified import aliases like `import M.(Box as AliasBox)` now preserve the constructor/type pairing across term and type positions, but the implementation still accomplishes that through import-time constructor/type rewrites rather than one first-class same-spelling binding-group object.
- [ ] Finish the provenance/ownership conversion so `KCoreOrigin`, `KCoreModule`, runtime lowering, dumps, and verification carry structured semantic identity end-to-end rather than rendered text.
- [ ] Replace text-based post-resolution lookup in lowering and backend preparation with resolved symbolic references.
  Current hot spots:
  [KBackendLowering.fs](/D:/ws/kappa/src/Kappa.Compiler/KBackendLowering.fs),
  [IlDotNetBackendInput.fs](/D:/ws/kappa/src/Kappa.Compiler/IlDotNetBackendInput.fs),
  [CheckpointVerification.fs](/D:/ws/kappa/src/Kappa.Compiler/CheckpointVerification.fs),
  [Backend.fs](/D:/ws/kappa/src/Kappa.Compiler/Backend.fs),
  [ZigCcBackendSupport.fs](/D:/ws/kappa/src/Kappa.Compiler/ZigCcBackendSupport.fs).
- [ ] Replace backend/runtime DTO fields that still encode semantic ownership as raw strings:
  `ModuleName`, `TypeName`, and `TraitName` fields in
  [KBackendIR.fs](/D:/ws/kappa/src/Kappa.Compiler/KBackendIR.fs),
  [KBackendLowering.fs](/D:/ws/kappa/src/Kappa.Compiler/KBackendLowering.fs),
  [ClrAssemblyIR.fs](/D:/ws/kappa/src/Kappa.Compiler/ClrAssemblyIR.fs),
  [IlDotNetBackendModel.fs](/D:/ws/kappa/src/Kappa.Compiler/IlDotNetBackendModel.fs),
  and remaining hosted/runtime boundary models.
- [ ] Continue the CLR backend migration past `IlNamed`.
  `IlDotNetBackendModel.IlType`, `RawDataTypeInfo`, `ConstructorInfo`, `DataTypeInfo`, `ModuleSurface`, and `TraitInstanceInfo` now store structured identities internally, with text only exposed as derived projections for compatibility.
  Remaining work is to remove those compatibility projections from downstream lowering and to stop the runtime/portable backend models from reintroducing text-based semantic ownership.
- [ ] Finish slimming `Stdlib.fs` after the new `StandardLibraryCatalog` introduction. Prelude/bundled/synthetic module definitions now live in one typed catalog, but some convenience lookups still route through `Stdlib` rather than consuming the catalog directly.
- [ ] Replace verifier/runtime stringly type carriers that force semantic checks over rendered text, especially `KRuntimeIR` type-text fields and the substring fallback in `CheckpointVerification.runtimeTypeLeaksErasureMetadata`.
- [ ] Replace stringly trait/dictionary conventions that still depend on synthesized textual names or prefixes, including `TraitRuntime.dictionaryTypeName ...`, `__kappa_dict_*` prefix checks, and literal trait-name comparisons such as `InterpolatedMacro`, `LacksRec`, `IsProp`, and `IsTrait`.
- [ ] Keep propagating semantic trait identity through backend/runtime lowering. `TraitConstraint` now preserves resolved `TraitReference` identity through elaboration and instance search, and source instances now normalize their declaring-trait identity before dictionary synthesis, but `KRuntimeIR`/`KBackendIR` still collapse that information back to text.
- [ ] Replace text-based trait, constructor, and type matching in compile-time evaluation and backend typing with symbolic references.
  Current hot spots:
  [ElaborationEvaluation.fs](/D:/ws/kappa/src/Kappa.Compiler/ElaborationEvaluation.fs),
  [IlDotNetBackendTyping.fs](/D:/ws/kappa/src/Kappa.Compiler/IlDotNetBackendTyping.fs),
  [ZigCcBackendArtifact.fs](/D:/ws/kappa/src/Kappa.Compiler/ZigCcBackendArtifact.fs).
- [ ] Replace remaining last-segment and ad hoc scoped-path checks for reified static/meta objects with declaration identities instead of `string list` path inspection.
  Current hot spots:
  [ResourceChecking.fs](/D:/ws/kappa/src/Kappa.Compiler/ResourceChecking.fs) (`hasTypeLastSegment` over shape/meta carriers),
  [SurfaceElaboration.fs](/D:/ws/kappa/src/Kappa.Compiler/SurfaceElaboration.fs) (manual `QueryCore`/`QueryMode`/`Res` scoped `TypeName` construction).
- [ ] Remove remaining effect- and static-object lookup tables keyed by rendered dotted text instead of structured identities.
  Current hot spots:
  [SurfaceElaboration.fs](/D:/ws/kappa/src/Kappa.Compiler/SurfaceElaboration.fs) (remaining dotted-name lookups around trait evidence, dictionary naming, and same-spelling import qualification).
- [ ] Remove any remaining fabricated semantic identities in backend/lowering/runtime code paths. The old `SurfaceElaboration.validateFrontendModule` `__unknown__` fallback is already gone.
- [ ] Preserve canonical module identity casing through all artifact names and backend metadata per section 17.3.4.2.
- [ ] Keep bridge/host spelling distinct from Kappa semantic identity.
- [ ] Add regression tests before each refactor slice and keep the symbolic-name validator clean outside the boundary allowlist.
- [ ] Add observability for symbolic identity.

## 8. Recommended execution order

- [ ] Align the internal architecture: true `KBackendIR`, stronger verifier rules, and post-`KBackendIR` target-lowering checkpoints.
- [ ] Keep using the Zig backend as a pressure test for what belongs in shared `KBackendIR` versus target-specific lowering state before expanding the CLR backend further.
- [ ] Decide whether section 2.6.2 stays normative for the current milestone or whether the spec needs a bootstrap prelude/profile split.
- [ ] Bring the test harness up to Appendix T and convert more of the existing suites to the standard directive set.

## 9. Structured diagnostics only

- [ ] Remove the remaining string escape hatches from `Diagnostics.fs`, especially `SimpleDiagnosticEvidence.Detail` and `CodeDetailEvidence.Detail`, so compiler phases cannot smuggle raw prose into emitted diagnostics.
- [x] Finish `Parser.fs` diagnostics conversion by replacing the remaining dynamic raw-string parse failures, especially string-literal decoding and URL module-specifier parsing.
- [x] Finish `CoreParsing.fs` diagnostics conversion for the remaining dynamic literal and Unicode decode failures after string-literal decode: character/grapheme/byte decode failures.
- [ ] Finish `CoreParsing.fs` diagnostics conversion for the remaining structural frontend paths: projection bodies, record/application/update forms, and expression-tail validation.
  Query/comprehension diagnostics now route through `CoreExpressionParsingEvidence`, including generator/group/join/left-join/conflict-clause errors, unordered `skip` / `take`, unsupported comprehension clauses, and missing trailing `yield`.
  Handler-expression diagnostics now route through `CoreExpressionParsingEvidence`, including missing `with`, malformed clause heads, missing clause arrows, and missing resumption binders.
  Function and local-function header diagnostics now route through `CoreExpressionParsingEvidence`, and both sites share the same header-parameter parser loop with only the header context varying.
  Expression-tail diagnostics now route through `CoreExpressionParsingEvidence` for missing effect-label names, missing constructor names after `is`, safe-navigation member access failures, chained tag tests, explicit member projection failures, unexpected trailing tokens, and record-update close failures; record literal/application field diagnostics and the remaining seal/selector cases are still stringly.
- [ ] Convert `Lexer.fs` diagnostics to typed evidence ADTs with centralized formatting.
- [ ] Convert elaboration/typechecking diagnostics to typed evidence ADTs with centralized formatting.
- [ ] Convert backend/lowering diagnostics to typed evidence ADTs with centralized formatting.
- [ ] Convert checkpoint verification and target-checkpoint diagnostics to typed evidence ADTs with centralized formatting.
- [ ] Preserve machine-readable payload structure as the primary contract, and keep tests asserting codes/families/payload fields rather than exact prose except where the spec mandates wording-sensitive content.

## 10. Foundations and frontend spec coverage

- [ ] Replace the current split intrinsic facts with one authoritative intrinsic manifest consumed by frontend import validation, elaboration, runtime lowering, interpreter, CLR lowering, Zig lowering, and backend verification.
  References: `TODO.md` §1; `reviews/principles1.md`; `reviews/backend1.md`; `reviews/general1.md`.
- [ ] Finish unifying standard-library/module descriptors so import validation, elaboration, runtime injection, and backends all consume the same module catalog and capability/support metadata.
  References: `TODO.md` §1, §8, §8A; `reviews/principles1.md`; `reviews/general1.md`; `reviews/unicode1.md`.
- [ ] Align identifier, keyword, and module/path-derived name handling with the lexical spec, including the ASCII-vs-Unicode identifier decision and the token model exposed to tooling.
  References: `TODO.md` §2, §12; `reviews/frontend1.md`; `reviews/general1.md`; `reviews/unicode1.md`.
- [ ] Complete the missing surface-language coverage for `do` blocks, `try` / `except` / `finally` / `try match`, `derive`, and `decreases`/totality syntax, with end-to-end lowering and diagnostics rather than reserved-word-only support.
  References: `TODO.md` §3, §5B, §12; `reviews/frontend1.md`; `reviews/traits1.md`; `reviews/tests1.md`.
- [ ] Finish the KFrontIR query/error-tolerance model beyond honest phase reporting, including declaration/query-level lazy resolution, error-tolerant placeholders, and tooling-facing checker facts.
  References: `TODO.md` §4; `reviews/frontend1.md`.
- [ ] Reconcile the spec's explicit top-level-signature rule with the current compiler/tests, instead of leaving exported definition inference as an undocumented implementation exception.
  References: `TODO.md` §5; `reviews/tests1.md`.
- [ ] Keep numeric literals abstract until target-type and representation resolution are complete, and remove any fallback path that silently lowers unhandled numeric literals to semantically wrong runtime values.
  References: `TODO.md` §5, §8; `reviews/principles1.md`; `reviews/general1.md`.
- [ ] Audit projections, accessors, and projector descriptors for full spec coverage across parsing, elaboration, lowering, and diagnostics.
  References: `TODO.md` §5; `reviews/general1.md`.

## 11. Macros, staging, traits, and QTT

- [ ] Implement a real elaboration-time evaluator for `Elab`, real splice execution for `$(...)`, hygienic `Syntax` values, stage-aware substitution, the missing public `Syntax`/reflection APIs, and a distinct `Code` staging model that does not erase to the inner expression.
  References: `TODO.md` §5A; `reviews/macros1.md`; `reviews/observability1.md`.
- [ ] Make macro-expansion incrementality/observability real once elaboration-time execution exists, including fingerprints over macro definitions, imports, configuration, and expansion environment.
  References: `TODO.md` §5A, §11A; `reviews/macros1.md`; `reviews/observability1.md`.
- [ ] Rebuild trait/instance semantics around explicit evidence objects: constrained-instance premises, supertrait projection, stronger instance validation, Paterson-style termination checks, richer coherence, rigid-vs-instantiable matching, constrained members, and local instances/deriving policy.
  References: `TODO.md` §5B; `reviews/traits1.md`.
- [ ] Route `derive` through synthetic instance generation on the same validation/coherence/runtime path as handwritten instances, rather than through a parallel backend-specific mechanism.
  References: `TODO.md` §3, §5B; `reviews/frontend1.md`; `reviews/traits1.md`.
- [ ] Rework the resource checker so demand tracking, linear discharge, loop upper bounds, abrupt completion, quantity-variable handling, hidden borrow roots, explicit borrow regions, and whole-application borrow overlap follow the QTT rules instead of the current approximations.
  References: `TODO.md` §6; `reviews/qtt1.md`.
- [ ] Delete duplicate signature/quantity reparsing in resource checking and consume parsed signature/type structures directly.
  References: `TODO.md` §6; `reviews/qtt1.md`.

## 12. Effects, queries, runtime surface, and Unicode/hash semantics

- [ ] Finish effect-row semantics beyond the current self-label approximation, including distinct label identity vs interface identity and typed operation telescopes for handlers/operations.
  References: `TODO.md` §7; `reviews/effects1.md`.
- [ ] Replace parser-time comprehension lowering with a post-resolution query/comprehension plan, and implement real `IntoQuery` / `BorrowIntoQuery`, first-class `Query`/`OnceQuery`/`QueryCore` carriers, custom sinks, borrowed/refutable clauses, grouping, map conflict policy, orderedness proofs, multi-key ordering, and honest plan observability.
  References: `TODO.md` §7A; `reviews/queries1.md`.
- [ ] Align the prelude/runtime contract with actual runtime availability, and stop treating prelude exposure as proof that interpreter/CLR/Zig semantics exist.
  References: `TODO.md` §8, §9; `reviews/general1.md`; `reviews/backend1.md`.
- [ ] Replace fake file/data intrinsics with real runtime behavior or explicit test-runtime gating.
  References: `TODO.md` §8; `reviews/principles1.md`; `reviews/backend1.md`.
- [ ] Make `std.hash.Hashable` a real trait or replace all fallback behavior with explicit builtin evidence, then fix the streaming-hash API/state model, structural framing, fixed byte order, and backend/runtime support so hash semantics match the declared surface.
  References: `TODO.md` §5, §8A; `reviews/principles1.md`; `reviews/unicode1.md`; `reviews/backend1.md`.
- [ ] Implement or retract the missing `std.unicode`/`std.bytes` surface terms, Unicode warning emitters, Unicode location/column policy, pinned Unicode-behavior policy, and backend/runtime representations for `Bytes`, `Byte`, `UnicodeScalar`, `Grapheme`, and embedded NUL strings.
  References: `TODO.md` §8A; `reviews/unicode1.md`; `reviews/backend1.md`; `reviews/zig1.md`.

## 13. Backend capability model and target backends

- [ ] Pair section 1's `KBackendIR` work with an explicit backend-capability/profile contract, so any still-unsupported runtime/control structures are rejected early and honestly in documentation, checkpoints, and capability diagnostics.
  References: `TODO.md` §9; `reviews/backend1.md`.
- [ ] Add a real backend capability model, separate source-name availability from lowering support from target implementation availability, and reject unsupported reachable constructs before target lowering with capability diagnostics.
  References: `TODO.md` §8, §9, §11; `reviews/backend1.md`; `reviews/general1.md`; `reviews/zig1.md`.
- [ ] Replace text-based backend representation selection with resolved representation metadata, and make checkpoint verification prove real lowerability for the selected target instead of only shallow structural well-formedness.
  References: `TODO.md` §1, §9; `reviews/backend1.md`; `reviews/principles1.md`.
- [ ] Fix the concrete cross-backend semantic mismatches already identified: raw-bit float equality, `&&` / `||` evaluation/short-circuit behavior, numeric/operator lowering edge cases, and `Char` / `UnicodeScalar` / `Grapheme` / `Byte` representation mismatches.
  References: `TODO.md` §9, §10, §11; `reviews/backend1.md`; `reviews/dotnet1.md`; `reviews/zig1.md`.
- [ ] Pick one canonical CLR-facing representation for `Char`, `UnicodeScalar`, `Grapheme`, and `Byte`, and make host interop, runtime lowering, IL typing, and emission agree on it.
  References: `TODO.md` §10; `reviews/dotnet1.md`; `reviews/backend1.md`.
- [ ] Make the `dotnet` toolchain runner collect stdout/stderr asynchronously so large child output cannot deadlock build/run/publish helpers.
  References: `TODO.md` §10; `reviews/dotnet1.md`.
- [ ] Tighten the Zig backend around real lowered/runtime data: restrict intrinsic exposure to implemented capability, generate trait dispatch from lowered IR instead of syntax scans, support or reject first-class intrinsic values explicitly, use injective collision-checked symbol generation, make closure environment names globally unique, and fix C-string/NUL/prefixed-string behavior.
  References: `TODO.md` §11; `reviews/zig1.md`; `reviews/backend1.md`.

## 14. Tests, harness, and conformance bookkeeping

- [ ] Split the suite into explicit categories such as spec conformance, compiler regression, backend regression, and integration, so green runs describe what they actually prove.
  References: `TODO.md` §12; `reviews/tests1.md`.
- [ ] Add targeted regression coverage for the reviewed frontend/module/import gaps, QTT failure shapes, effects/handlers failure shapes, backend/runtime capability failures, Unicode/bytes/hash semantics, trait/instance/deriving semantics, CLR backend edge cases, and Zig backend edge cases.
  References: `TODO.md` §12; `reviews/frontend1.md`; `reviews/qtt1.md`; `reviews/effects1.md`; `reviews/backend1.md`; `reviews/unicode1.md`; `reviews/traits1.md`; `reviews/dotnet1.md`; `reviews/zig1.md`.
- [ ] Replace weak negative assertions with diagnostic-code assertions, expected spans/modules where practical, and prose checks only as a secondary layer.
  References: `TODO.md` §12; `reviews/tests1.md`.
- [ ] Keep URL import tests labeled as policy/staging tests until there is a real resolver/mock-resolver, and make archive/toolchain-dependent suites explicit integration tests that cannot silently discover zero meaningful cases.
  References: `TODO.md` §12; `reviews/tests1.md`.
- [ ] Remove stale `.trx` artifacts unless they are deliberate golden observability assets, and keep promoting staged spec-derived tests from `new-tests/` into the live fixture suite as parser/elaboration/harness support lands.
  References: `TODO.md` §12; `reviews/tests1.md`; `new-tests/README.md`.
