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
- [ ] Then align the internal architecture: true `KBackendIR`, stronger verifier rules, and post-`KBackendIR` target-lowering checkpoints.
- [x] In parallel with that architectural cleanup, stand up the first real native path under the standardized `zig` profile by lowering `KBackendIR` to generated C and compiling it with `zig cc`.
- [ ] Use the first `zig` slice to pressure-test what still belongs in `KBackendIR` versus what is really target-specific lowering state before expanding the CLR backend further.
- [ ] Then decide whether section 2.7 stays normative for the current milestone or whether the spec needs a bootstrap prelude/profile split.
- [ ] Finally, bring the test harness up to Appendix T and convert more of the existing suites to the standard directive set.

Current milestone status note: M2 is complete enough to start M3. The compiler now normalizes the effective backend profile, verifies `KBackendIR` before native emission, lowers the standardized `zig` profile directly from `KBackendIR`, exposes a post-`KBackendIR` `zig.c` checkpoint with verification and stage dumps, and runs the M2 target shape on the interpreter, `zig`, and public CLR-backed `dotnet` profiles. The remaining unchecked items are broader conformance work rather than blockers to starting M3.

## 9. Milestone 2 (`Traits` + `Ref` + `while`)

Preferred resolution: adjust the compiler.

- [x] Extend the surface syntax and parser for `instance` declarations, `var`, assignment forms, `while ... do ...`, and monadic splice `!(...)` inside `do`.
- [x] Introduce an explicit M2 elaboration layer that rewrites constrained functions to explicit dictionary parameters, synthesizes dictionary artifacts for trait declarations and instances, and resolves instance evidence at call sites.
- [x] Desugar mutable-variable forms through `newRef` / `readRef` / `writeRef` with the uniform reference semantics required by `Spec.md` §8.5.1.
- [x] Lower `while ... do ...` through an internal recursive helper form that works on both real backends without depending on the hosted interpreter path.
- [x] Extend the intrinsic/builtin surface with the M2 runtime contract (`MonadRef IO`, `newRef`, `readRef`, `writeRef`, `primitiveIntToString`, `printString`, and the concrete `Ref`/dictionary runtime support needed by the backends).
- [x] Make the standardized `zig` profile compile and run the M2 target end-to-end.
- [x] Make the public CLR-backed `dotnet` profile compile and run the M2 target end-to-end.
- [x] Add direct compiler/backend tests that execute the M2 program shape on both `zig` and `dotnet`, then keep them green while refactoring.

Current M2 status note: the interpreter, standardized `zig` backend, and public CLR-backed `dotnet` backend all compile and run the M2 milestone program shape end-to-end.

## 10. Milestone 3 (`QTT` + borrowing + deterministic resources)

Preferred resolution: adjust the compiler, keeping QTT information explicit through elaboration and erased before backend-specific lowering.

- [ ] Start with data-driven M3 tests before implementation: one positive `using`/linear-file program and at least three negative programs for dropped owned resources, duplicated owned resources, and borrowed-region escape through a returned closure.
- [ ] Audit `Spec.md` sections 5.1.5-5.1.7, 7.1.3, 8.7.4, 8.8, 14.4, and the relevant KCore sections before changing the typechecker, then record any spec/compiler mismatch discovered during implementation.
- [ ] Extend the lexer/parser for quantity binders (`0`, `1`, `&`, and `omega`/`ω`), borrowed binders (`(& x : T)` and `(&[s] x : T)`), `using pat <- expr`, `inout`, and `~` call-site syntax.
- [ ] Add syntax and checkpoint tests that prove quantity annotations round-trip into the pre-elaboration observable forms without changing runtime semantics.
- [ ] Introduce a typed quantity/region/place model rather than representing quantities as ad hoc strings on binders.
- [ ] Replace the simple name-to-type typing environment in the affected checker paths with a resource context that tracks type, quantity obligation, stable place, region, and use state.
- [ ] Implement syntax-directed context splitting and consumption for applications, `let`, `do`, `match`, closures, and control-flow joins.
- [ ] Implement the quantity satisfaction relation from `Spec.md`, including the strict separation between owned (`1`) and borrowed (`&`) obligations.
- [ ] Implement stable-place analysis and borrow introduction for variables, parameters, hidden temporaries, and the initial field/path subset required by M3.
- [ ] Implement skolem borrow regions for `using` and borrowed binders, including closure-capture tainting and non-escape diagnostics.
- [ ] Implement deterministic release scheduling for `using pat <- expr`: keep the hidden owned resource, expose only borrowed bindings in the protected body, and schedule exactly-once release on all exits.
- [ ] Add `Releasable IO a` dictionary/evidence handling for the `using` lowering path, reusing the existing M2 trait machinery where possible.
- [ ] Implement the M3 intrinsic/runtime contract initially as backend-profile intrinsics: `openFile`, `readData`, `primitiveCloseFile`, and any minimal file-handle test shim needed by the positive target.
- [ ] Implement `inout` / `~` as a type-directed elaboration rewrite after the resource context exists, not as parser-only sugar.
- [ ] Preserve QTT metadata through KCore observability so tests can assert ownership and borrow decisions before erasure.
- [ ] Erase quantities, regions, and borrow-only metadata before `KBackendIR`, while preserving ordinary runtime values and retained backend generics/dictionaries.
- [ ] Strengthen checkpoint verification so post-erasure IR rejects leaked quantity, borrow, region, or place-only constructs.
- [ ] Lower `using` exit actions in the CLR backend to `try` / `finally`, calling the selected `Releasable` dictionary's release implementation exactly once.
- [ ] Lower `using` exit actions in the ZigCc backend with equivalent cleanup behavior, even if the first implementation uses a small generated-C runtime shim.
- [ ] Add regression tests that execute the M3 positive program on the interpreter, standardized `zig`/ZigCc backend, and public `dotnet` profile, then keep all three green while refactoring.
- [ ] Stop for a cleanup pass after the first green M3 slice: separate resource-context logic, borrow-region logic, and backend cleanup lowering so M4 effect-handler work does not inherit a monolith.

Current M3 status note: not started. The first slice should be test-first and should reject the three negative ownership cases before expanding syntax coverage beyond the target program shape.
