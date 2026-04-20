# Spec Alignment Plan

## 1. Baseline audit

Preferred resolution: neither yet; establish the exact alignment map first.

- [ ] Build a section-by-section alignment matrix for the parts of `Spec.md` that the compiler or tests currently exercise.
- [ ] Classify each reviewed section as one of: aligned, compiler divergence, spec/docs divergence, or not implemented yet.
- [ ] Keep "not implemented yet" separate from "implemented but contradictory", so we do not blur roadmap gaps with actual conformance bugs.

## 2. Public `dotnet` backend profile

Preferred resolution: adjust the compiler.

- [x] Align the public `dotnet` backend with `Spec.md` section 17.10 so it lowers to real CLR artifacts containing CIL and metadata.
- [x] Repoint the public entry points that currently call `Backend.emitDotNetArtifact` and CLI `--backend dotnet` away from the hosted source-generating path and onto the real CLR backend.
- [x] Keep the hosted backend only as an explicitly nonstandard/bootstrap profile, renamed so it is not confused with the standardized `dotnet` profile.
- [x] Add conformance tests that compile and run the same program through the public `dotnet` profile and inspect the emitted managed artifact shape.

## 3. `KBackendIR` shape and lowering semantics

Preferred resolution: adjust the compiler.

- [ ] Audit the current `KBackendIR` model in `src/Kappa.Compiler/Compilation.fs` against `Spec.md` sections 17.4 and 17.4.1.
- [ ] Decide whether to evolve the current `KBackendIR` into a true runtime IR or to insert a new explicit runtime IR and rename the current form.
- [ ] Introduce runtime-facing constructs for representation choice, runtime calls, data layout, field access, retained dictionaries/type parameters, and explicit runtime control.
- [x] Strengthen `KBackendIR` verification so it checks the legality conditions from section 17.4.2 instead of only structural uniqueness checks.
- [ ] Extend observability so `KBackendIR` dumps and post-`KBackendIR` CLR-lowering dumps expose the runtime information required by the spec.

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
- [ ] Add tests that assert exactly the chosen prelude contract, so the bootstrap/full distinction is machine-checked.

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
- [ ] Then align the internal architecture: true `KBackendIR`, stronger verifier rules, and post-`KBackendIR` CLR lowering checkpoints.
- [ ] Then decide whether section 2.7 stays normative for the current milestone or whether the spec needs a bootstrap prelude/profile split.
- [ ] Finally, bring the test harness up to Appendix T and convert more of the existing suites to the standard directive set.
