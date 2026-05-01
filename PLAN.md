# Open Alignment Plan

## 1. `KBackendIR` shape and lowering semantics

- [ ] Audit the current `KBackendIR` model in `src/Kappa.Compiler/KBackendIR.fs` and its lowering path against `Spec.md` sections 17.4 and 17.4.1.
- [ ] Introduce runtime-facing constructs for representation choice, runtime calls, data layout, field access, retained dictionaries/type parameters, and explicit runtime control.
- [ ] Keep record-typed runtime values as dedicated record carriers through backend lowering rather than re-encoding them as nested `Res` pairs; for the CLR backend, follow up by mapping synthesized record carriers onto a native record/class/struct representation with predictable field layout so query-row temporaries and ordinary records do not pay avoidable allocation or indirection costs.
- [ ] Extend observability as `KBackendIR` grows so `KBackendIR` dumps and post-`KBackendIR` CLR-lowering dumps continue to expose the graph/provenance/runtime information required by sections 17.4.3-17.4.4.

## 2. `expect` satisfaction and backend intrinsics

- [ ] Record a stable implementation identity for each backend intrinsic in emitted artifacts, and include that identity in any hashing or coherence decision that depends on the intrinsic, as required by section 17.6.
- [ ] Decide which intrinsics are elaboration-available and enforce that distinction during elaboration-time evaluation.

## 3. Prelude import semantics

- [ ] Add tests covering the boundary between term/type imports and constructor imports, including explicit `ctor` imports and `type T(..)` imports.
- [ ] Re-check existing prelude fixtures after the import rules are corrected, because some current tests may be passing only because constructor import is too permissive.

## 4. Prelude contents and bootstrap surface

- [ ] Compare `src/Kappa.Compiler/Stdlib/std/prelude.kp` against the normative minimum in section 2.6.2 and produce a missing-items list.
- [ ] Decide whether M1 really intends the full section 2.6.2 prelude or a smaller bootstrap prelude.
- [ ] If the goal is the full spec surface, expand the compiler prelude and intrinsic/runtime support until the missing mandatory names exist.
- [ ] If the goal is a smaller bootstrap surface, update `Spec.md` to define that bootstrap prelude explicitly instead of leaving the current reduced prelude as an undocumented deviation.

## 5. Appendix T standard harness

- [ ] Stop treating the current harness as Appendix-T-conformant until the missing standard directives are implemented.
- [ ] Add support for `suite.ktest` and `incremental.ktest`, including suite-level configuration and incremental step execution.
- [ ] Implement the standard configuration directives from Appendix T: `mode`, `packageMode`, `scriptMode`, `backend`, `entry`, `runArgs`, `stdinFile`, `dumpFormat`, and `requires`.
- [ ] Use the existing stable diagnostic codes in the harness so `assertDiagnostic`, `assertDiagnosticNext`, `assertDiagnosticAt`, and inline `--!!` markers are actually possible.
- [ ] Make `assertType` compare elaborated types by definitional equality rather than by token-text equality.
- [ ] Implement the remaining standard assertions we do not support yet, including file-relative diagnostic assertions, `assertFileDeclKinds`, stage-dump assertions, pipeline-trace assertions, and incremental cross-step assertions.
- [ ] Keep existing `x-...` directives explicitly marked as extensions rather than letting them silently stand in for missing standard behavior.

## 6. Milestone 4 (`Effects` + handlers + row polymorphism)

- [ ] Implement row-polymorphic effect solving for `EffRow`, including normalization plus `SplitEff`-style reasoning instead of the current single-label scoped-effect approximation.
- [ ] Type handler clauses against the handled computation precisely, including remainder-row reasoning and resumption types derived from the clause result type instead of the current conservative `TypeArrow(ω, opResult, handledType)` approximation.
- [ ] Lower one-shot handlers through `KBackendIR` and into at least the public CLR-backed `dotnet` profile; interpreter-only support is not sufficient to close M4.

Current M4 status note: started, not complete. The compiler now has a real effect-handler kernel in `KCore` / `KRuntimeIR`, one-shot scoped-effect execution in the interpreter, parser support for multiline handler clause bodies, imported/top-level effect-label resolution across module boundaries, an ownership-checked multi-shot capture rejection for live linear or borrowed continuation suffixes, and checkpoint coverage that rejects surviving handler runtime constructs or leaked `Eff`/effect-row metadata before `KBackendIR`.

## 7. Symbolic names, spelling, and semantic identity

- [ ] Introduce declaration-level and semantic-object identities beyond `ModuleIdentity`, so resolved declarations, constructors, traits, effect labels, projections, and reified static objects stop falling back to plain `string`.
- [ ] Replace visible-name environments that collapse declaration kinds into plain `Map<string, ...>` lookup with an explicit binding-group model matching section 2.8.
- [ ] Rework same-spelling data-family handling so it is represented as one binding group with typed facets instead of separate text-keyed maps plus special cases.
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
- [ ] Continue the CLR backend migration past `IlNamed`. `IlDotNetBackendModel.IlType` now has a structured `TypeIdentity`, but metadata carriers such as `RawDataTypeInfo`, `ConstructorInfo`, `DataTypeInfo`, `ModuleSurface`, and `TraitInstanceInfo` still record semantic ownership as raw text.
- [ ] Finish slimming `Stdlib.fs` after the new `StandardLibraryCatalog` introduction. Prelude/bundled/synthetic module definitions now live in one typed catalog, but some convenience lookups still route through `Stdlib` rather than consuming the catalog directly.
- [ ] Replace verifier/runtime stringly type carriers that force semantic checks over rendered text, especially `KRuntimeIR` type-text fields and the substring fallback in `CheckpointVerification.runtimeTypeLeaksErasureMetadata`.
- [ ] Replace stringly trait/dictionary conventions that still depend on synthesized textual names or prefixes, including `TraitRuntime.dictionaryTypeName ...`, `__kappa_dict_*` prefix checks, and literal trait-name comparisons such as `InterpolatedMacro`, `LacksRec`, `IsProp`, and `IsTrait`.
- [ ] Replace `TypeSignatures.TraitConstraint.TraitName : string` with a parsed trait reference that preserves qualification and can carry semantic identity through elaboration, instance search, dictionary synthesis, and backend lowering. The recent qualified-trait-alias regression showed that collapsing `score.Score` to bare `"Score"` still breaks later phases even when import resolution itself is correct.
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
  [SurfaceElaboration.fs](/D:/ws/kappa/src/Kappa.Compiler/SurfaceElaboration.fs) (remaining dotted-name lookups around trait evidence/import qualification).
- [ ] Remove fabricated semantic identities such as the `ModuleIdentity.ofSegments [ "__unknown__" ]` fallback that still exists in `SurfaceElaboration.validateFrontendModule`.
- [ ] Preserve canonical module identity casing through all artifact names and backend metadata per section 17.3.4.2.
- [ ] Keep bridge/host spelling distinct from Kappa semantic identity.
- [ ] Add regression tests before each refactor slice and keep the symbolic-name validator clean outside the boundary allowlist.
- [ ] Add observability for symbolic identity.

## 8. Recommended execution order

- [ ] Align the internal architecture: true `KBackendIR`, stronger verifier rules, and post-`KBackendIR` target-lowering checkpoints.
- [ ] Use the first `zig` slice to pressure-test what still belongs in `KBackendIR` versus what is really target-specific lowering state before expanding the CLR backend further.
- [ ] Decide whether section 2.6.2 stays normative for the current milestone or whether the spec needs a bootstrap prelude/profile split.
- [ ] Bring the test harness up to Appendix T and convert more of the existing suites to the standard directive set.
