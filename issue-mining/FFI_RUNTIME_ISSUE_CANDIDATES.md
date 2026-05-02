# FFI / Runtime Boundary Issue Candidates

Purpose: track non-Kotlin issues, PRs, docs, and upstream tests relevant to Kappa's boundary-honesty, FFI, dynamic-value,
bridge, and portable ABI spec.

Primary spec areas:

- §1.1 Boundary honesty and progressive precision
- §2.7 `std.ffi`
- §2.7A `std.gradual`
- §2.7B `std.bridge`
- §5.10 Dynamic values, runtime representations, and checked boundaries
- §17.7 Foreign interop and portable foreign ABI
- Appendix O Boundary and graduality roadmap

This note is deliberately narrow. It covers FFI and runtime boundary semantics, not general ownership or concurrency unless
the issue crosses a foreign/runtime boundary.

## Search Coverage Ledger

| Repo | Manifest / artifact paths | Search coverage | Direct reads | PR / test coverage | Next pass |
| --- | --- | --- | --- | --- | --- |
| `zig` | `repos/github-ziglang-zig/manifest.json`, `issues.json`, `pulls.json` | issue search: `ffi`, `foreign`, `extern`, `c abi`, `abi`, `interop`, `native`, `callback`, `dynamic`, `cast`, `unsafe`, `linker`, `dlopen`, `wasm`, `jni`, `host`; PR search: `c abi`, `byvalue`, `extern fn`, `calling convention`, `errno`, `cimport`, `mangled symbol`, `same c function`, `foreign abi`; harvested-test path search: `extern`, `c_abi`, `callconv`, `ABI`, `slice`, `comptime`, `function pointer` | `#180`, `#244`, `#274`, `#277`, `#317` | PR title hits recorded: `#4500`, `#4502`, `#5461`, `#6607`, `#6829`, `#8152`, `#9064`, `#9443`, plus related `translate-c` / `cImport` PRs; harvested tests read: `tests/test/c_abi/main.zig`, `tests/test/c_abi/cfuncs.c`, `tests/test/cases/compile_errors/function_with_non-extern_non-packed_struct_parameter.zig`, `tests/test/cases/compile_errors/slice_used_as_extern_fn_param.zig`, `tests/test/cases/compile_errors/extern_function_with_comptime_parameter.zig`, `tests/test/cases/compile_errors/extern_function_pointer_mismatch.zig`, `tests/test/cases/compile_errors/extern_function_with_unspecified_calling_convention.zig`, `tests/test/cases/compile_errors/reject_extern_function_definitions_with_body.zig`, `tests/test/standalone/extern/main.zig`, `tests/test/standalone/extern/exports.zig`, `tests/test/standalone/extern/shared.c`, `tests/test/behavior/extern.zig` | next Zig test pass should read `test/standalone/extern/build.zig`, `test/behavior/extern_struct_zero_size_fields.zig`, and `test/behavior/multiple_externs_with_conflicting_types.zig` |
| `mojo` | `repos/github-modularml-mojo/manifest.json`, `issues.json`, `pulls.json` | issue search: same FFI/boundary regex as Zig; PR search: `cffi`, `python interop`, `hello_interop`, `dynamic`, `foreign`, `ffi`, `importlib`, `host`, `bridge`; harvested-test path search: `ffi`, `python`, `interop`, `external_call`, `bridge`, `unsafe_union`, `errno` | `#122`, `#214`, `#215`, `#317`, `#449`, `#514` | PR title hits recorded: `#3557`, `#3592`, `#3638`, `#4405`, `#4885`, `#5072`, `#5140`, `#5319`, `#5514`, `#5562`, `#5599`, `#5971`; harvested tests read: `tests/mojo/stdlib/test/ffi/test_external_call.mojo`, `tests/mojo/stdlib/test/python/test_python_interop.mojo`, `tests/mojo/stdlib/test/python/test_python_error_handling.mojo`, `tests/mojo/stdlib/test/python/test_python_to_mojo.mojo`, `tests/mojo/stdlib/test/sys/test_ffi.mojo`, `tests/mojo/stdlib/test/ffi/test_unsafe_union.mojo` | next body reads: PRs `#3557`, `#5072`, `#5514`, `#5562`, `#5971`; next test read: `tests/mojo/stdlib/test/ffi/test_external_call.mojo` companion sources if harvested |
| `gleam` | `repos/github-gleam-lang-gleam/manifest.json`, `issues.json`, `pulls.json` | issue search: same FFI/boundary regex as Zig; PR search: `external function`, `external type`, `multiple external`, `ffi`, `javascript module`, `prevent recursion`, `esm re-export`, `dynamic module`, `unknown`, `foreign`; harvested-test path search: `external`, `externals`, `ffi`, `target_implementations` | `#43`, `#328`, `#419`, `#915`, `#1049`, `#1068`, `#1145`, `#1170`, `#1190` | PR title hits recorded: `#279`, `#573`, `#639`, `#912`, `#1059`, `#2230`, `#2792`, `#3601`, `#3681`, `#3728`, `#4256`, `#4509`; harvested tests read: `tests/compiler-core/src/type_/tests/externals.rs`, `tests/compiler-core/src/javascript/tests/externals.rs`, `tests/compiler-core/src/erlang/tests/external_fn.rs`, `tests/compiler-core/src/format/tests/external_fn.rs`, `tests/compiler-core/src/format/tests/external_types.rs` | read PR `#1059`, `#2230`, `#2792`, and `#3681` bodies; then read `test/language/test/ffi.*` harvested runtime files |
| `ocaml` | `repos/github-ocaml-ocaml/manifest.json`, legacy `../../ocaml-issues.json`, repo-local `pulls.json` | issue search: `ffi`, `foreign`, `external`, `extern`, `c abi`, `abi`, `interop`, `native`, `callback`, `dynamic`, `cast`, `unsafe`, `dlopen`, `ctypes`, `stub`, `primitive` | `#2471`, `#2609`, `#2670`, `#3953`, `#5707`, `#6896`, `#7855` | no PR/test reads yet | next pass should narrow to C stubs, callbacks, dynlink, primitive availability, and calling convention tests |
| `roc` | `repos/github-roc-lang-roc/manifest.json`, `issues.json`, `pulls.json` | issue search: FFI/boundary regex; PR search: `C ABI`, `platform`, `host`, `linking`, `roc call`, `roc result`, `closure`, `function pointer`, `requires types`, `provides`, `panic`, `allocator`; harvested-test path search: `platform`, `host`, `zig`, `module_params`, `requires`, `provides` | `#295`, `#333`, `#1242`, `#1325`, `#1403`, `#1501` | PR title hits recorded: `#518`, `#600`, `#631`, `#645`, `#666`, `#761`, `#1257`, `#1324`, `#1331`; harvested tests read: `tests/crates/cli/tests/test-projects/test-platform-simple-zig/app.roc`, `tests/crates/cli/tests/snapshots/cli_tests__cli_tests__test_platform_simple_zig__module_params_unexpected_fn.snap` | read `#1242` follow-up / PRs for closure exclusion and platform boundary rules; continue with `test-platform-simple-zig/main.roc` and `test-platform-effects-zig` files |
| `aeneas` | `repos/github-aeneasverif-aeneas/manifest.json`, `issues.json` | issue search: `ffi`, `foreign`, `extern`, `abi`, `interop`, `native`, `callback`, `dynamic`, `cast`, `unsafe`, `linker`, `wasm`, `host`, `opaque`, `trusted` | `#252`, `#371`, `#784` | no PR/test reads yet | decide whether `#371` belongs here or in `OWNERSHIP_ISSUE_CANDIDATES.md` as a boundary/borrow crossover |
| `creusot` | `repos/github-creusot-rs-creusot/manifest.json`, `issues.json` | issue search: FFI/boundary regex plus `borrow`, `lifetime`, `opaque`, `trusted` | `#18`, `#37`, `#286`, `#347`, `#386`, `#440`, `#492` | no PR/test reads yet | split trusted/extern-spec issues from pure ownership soundness issues |

## Staged Tests Added

These are direct spec-point fixtures in `new-tests/`. They intentionally exercise the Kappa standard boundary modules
rather than copying source-language FFI syntax.

| Fixture root | Source evidence | Spec refs | Purpose |
| --- | --- | --- | --- |
| `ffi.standard_module.positive_import_core_ffi_types` | `zig#180`, `roc#295`, `ocaml#5707` | §2.7, §17.7.1 | The exact-width scalar, pointer, and opaque-handle vocabulary is importable only through `std.ffi`. |
| `ffi.standard_module.negative_ffi_types_not_implicit` | `zig#317`, `gleam#43` | §2.7 | `std.ffi` is not implicitly imported; raw boundary vocabulary must be explicit. |
| `ffi.standard_module.positive_import_c_abi_spelling_types` | Zig harvested C ABI tests, `ocaml#5707` | §2.7, §17.7.1, §17.7.2.1 | C/native ABI spelling types are available through `std.ffi.c` when the selected backend exposes native ABI bindings. |
| `ffi.standard_module.negative_c_abi_types_not_implicit` | Zig harvested C ABI tests | §2.7 | C/native ABI spelling types are not ambient prelude names; `std.ffi.c` must be requested explicitly. |
| `gradual.standard_module.positive_import_dynamic_boundary_types` | `mojo#122`, `gleam#1190` | §2.7A, §5.10 | Dynamic boundary vocabulary is explicit runtime evidence, not an ambient unknown type. |
| `gradual.standard_module.negative_dyn_not_implicit` | `mojo#122`, `gleam#1190` | §2.7A, §5.10 | `std.gradual` is not implicitly imported; dynamic values must be explicit boundary evidence. |
| `bridge.standard_module.positive_import_bridge_contract_types` | `mojo#215`, `mojo#317`, `mojo#514` | §2.7B, §17.7.7, §17.7.7A | Runtime bridge vocabulary is ordinary values and contracts, not static imports or erased type tags. |
| `bridge.standard_module.negative_bridge_contract_not_implicit` | `mojo#215`, `mojo#317`, `mojo#514` | §2.7B, §17.7.7A | Bridge contracts are not ambient erased type information; the `std.bridge` vocabulary must be imported. |
| `bridge.standard_module.positive_import_boundary_classification_ctors` | `mojo#514` plus Python error-handling harvested tests | §2.7B, §17.7.4A, §17.7.7A | Boundary direction and precision constructors are importable as the standard bridge failure-classification vocabulary. |

## Candidate Records

### `zig#180`: incorrect C ABI for byvalue struct on exported function parameters and return values

- URL: https://github.com/ziglang/zig/issues/180
- Artifact path: `repos/github-ziglang-zig/issues.json`
- Artifact state: `issue-plus-pr-title-scan-plus-test-read`
- Bucket: `ffi_bridge_boundaries`
- Spec refs: §17.7.1, §17.7.2.1
- Fit: `accepted-later`
- Current coverage: `partial`
- Proposed fixture root: `ffi.portable_abi.positive_record_layout_identity_pinned_target`
- Expected outcome: `positive-check` plus backend/layout witness when host-binding tests exist
- Notes: This maps directly to Kappa's rule that closed record ABI layout is portable only when the selected adapter
  documents layout, field order, alignment, padding, and calling-convention behavior. The current staged FFI import test
  and C ABI spelling-type import test only lock the standard vocabulary; the actual byvalue record ABI test should wait
  for host-binding adapter test support. Corroborating harvested test: `tests/test/c_abi/main.zig`.

### Zig harvested C ABI tests: native spelling types and non-portable boundary shapes

- URL: https://github.com/ziglang/zig/tree/master/test/c_abi
- Artifact paths:
  - `repos/github-ziglang-zig/tests/test/c_abi/main.zig`
  - `repos/github-ziglang-zig/tests/test/c_abi/cfuncs.c`
  - `repos/github-ziglang-zig/tests/test/cases/compile_errors/function_with_non-extern_non-packed_struct_parameter.zig`
  - `repos/github-ziglang-zig/tests/test/cases/compile_errors/slice_used_as_extern_fn_param.zig`
  - `repos/github-ziglang-zig/tests/test/cases/compile_errors/extern_function_with_comptime_parameter.zig`
  - `repos/github-ziglang-zig/tests/test/cases/compile_errors/extern_function_pointer_mismatch.zig`
  - `repos/github-ziglang-zig/tests/test/cases/compile_errors/extern_function_with_unspecified_calling_convention.zig`
  - `repos/github-ziglang-zig/tests/test/cases/compile_errors/reject_extern_function_definitions_with_body.zig`
  - `repos/github-ziglang-zig/tests/test/standalone/extern/main.zig`
  - `repos/github-ziglang-zig/tests/test/standalone/extern/exports.zig`
  - `repos/github-ziglang-zig/tests/test/standalone/extern/shared.c`
  - `repos/github-ziglang-zig/tests/test/behavior/extern.zig`
- Artifact state: `upstream-test`
- Bucket: `ffi_bridge_boundaries`
- Spec refs: §2.7, §17.7.1, §17.7.2.1, §17.7.4B
- Fit: `accepted-now` for `std.ffi.c` vocabulary; `accepted-later` for portable-ABI exclusion diagnostics
- Current coverage: `partial`
- Proposed fixture roots:
  - `ffi.standard_module.positive_import_c_abi_spelling_types`
  - `ffi.standard_module.negative_c_abi_types_not_implicit`
  - later: `ffi.portable_abi.negative_non_portable_record_parameter`
  - later: `ffi.portable_abi.negative_slice_or_borrow_in_boundary_signature`
  - later: `ffi.portable_abi.negative_comptime_or_generic_boundary_parameter`
  - later: `ffi.portable_abi.negative_function_value_calling_convention_mismatch`
  - later: `ffi.raw_host_binding.negative_extern_binding_body_not_allowed`
- Expected outcome: current roots are `positive-check` / `negative-diagnostic`; later roots should use stable portable
  ABI diagnostics
- Notes: The C ABI positive test establishes why Kappa keeps C/native spelling types separate from exact-width
  `std.ffi` scalars. The compile-error tests are direct evidence for Kappa's portable ABI exclusions: internal record
  layout, slice/borrow-like values, and compile-time/generic parameters must not be guessed into ABI-stable boundary
  signatures. The later extern-function tests add a related rule: function values at a foreign boundary need explicit
  calling-convention/monitoring evidence and cannot be silently treated as ordinary Kappa function values.

### `roc#295`: C ABI compatibility for returning structs

- URL: https://github.com/roc-lang/roc/issues/295
- Artifact path: `repos/github-roc-lang-roc/issues.json`
- Artifact state: `issue-plus-pr-title-scan`
- Bucket: `ffi_bridge_boundaries`
- Spec refs: §17.7.1, §17.7.2.1, §17.7.4
- Fit: `accepted-later`
- Current coverage: `partial`
- Proposed fixture root: `ffi.portable_abi.positive_record_return_uses_adapter_layout`
- Expected outcome: `backend-profile` or `trace`
- Notes: Same underlying obligation as `zig#180`, from a separate compiler. Keep both sources merged unless the later
  backend harness can distinguish input parameter and return-value ABI paths.

### `zig#317`: explicit cast from `extern fn()` to ordinary `fn()`

- URL: https://github.com/ziglang/zig/issues/317
- Artifact path: `repos/github-ziglang-zig/issues.json`
- Artifact state: `issue-plus-test`
- Bucket: `ffi_bridge_boundaries`
- Spec refs: §17.7.1, §17.7.4B, §17.7.4C
- Fit: `accepted-now` for standard-module explicitness; `accepted-later` for function/callback ABI
- Current coverage: `partial`
- Proposed fixture root: `ffi.portable_abi.negative_function_value_not_portable_abi`
- Expected outcome: `negative-diagnostic`
- Notes: Kappa intentionally excludes function values, callbacks, closures, dictionaries, effects, handlers, and
  resumptions from the portable foreign ABI subset unless a monitored higher-order boundary is explicitly supplied. The
  staged `ffi.standard_module.negative_ffi_types_not_implicit` test covers only the import-explicitness side. The same
  exclusion family is corroborated by Zig compile-error tests for non-extern records, slices, and comptime/generic
  parameters at extern boundaries, plus function-pointer calling-convention mismatch tests.

### `zig#277`: same C function imported through two generated raw surfaces

- URL: https://github.com/ziglang/zig/issues/277
- Artifact path: `repos/github-ziglang-zig/issues.json`
- Artifact state: `issue-only`
- Bucket: `ffi_bridge_boundaries`
- Spec refs: §17.7.2, §17.7.2.1, §17.1.9
- Fit: `accepted-later`
- Current coverage: `none`
- Proposed fixture root: `ffi.raw_host_binding.positive_duplicate_host_member_identity_stable`
- Expected outcome: `stage-dump` or interface-artifact assertion
- Notes: Kappa's raw host binding interface must record original host member identity, generated Kappa spelling,
  overload rule, adapter mode, and host signature metadata. This issue is a good future artifact-identity test once raw
  host binding generation exists.

### `zig#274`: errno and host call-state capture

- URL: https://github.com/ziglang/zig/issues/274
- Artifact path: `repos/github-ziglang-zig/issues.json`
- Artifact state: `issue-only`
- Bucket: `ffi_bridge_boundaries`
- Spec refs: §17.7.2.1, §17.7.3, §17.7.4
- Fit: `accepted-later`
- Current coverage: `none`
- Proposed fixture root: `ffi.raw_host_binding.positive_call_state_capture_is_metadata_not_error_type`
- Expected outcome: `positive-check`
- Notes: Kappa already says adapter-mode call-state capture, such as `errno`, does not by itself determine a typed error
  channel. A refined overlay or trusted summary must name the error translation contract.

### `gleam#43`: external function definition

- URL: https://github.com/gleam-lang/gleam/issues/43
- Artifact path: `repos/github-gleam-lang-gleam/issues.json`
- Artifact state: `issue-plus-test`
- Bucket: `ffi_bridge_boundaries`
- Spec refs: §17.7.4, §17.7.5
- Fit: `accepted-now` for standard-module explicitness; `reject` for source syntax
- Current coverage: `partial`
- Proposed fixture root: `ffi.standard_module.negative_ffi_types_not_implicit`
- Expected outcome: `negative-diagnostic`
- Notes: Kappa does not require a distinct `external fn` declaration form. The useful import is the semantic requirement
  that FFI surfaces go through explicit standard modules, host binding modules, overlays, summaries, or bridge contracts.
  Corroborating harvested tests: `tests/compiler-core/src/type_/tests/externals.rs`,
  `tests/compiler-core/src/erlang/tests/external_fn.rs`, and `tests/compiler-core/src/javascript/tests/externals.rs`.

### `gleam#419`: multiple external functions with the same source name

- URL: https://github.com/gleam-lang/gleam/issues/419
- Artifact path: `repos/github-gleam-lang-gleam/issues.json`
- Artifact state: `issue-plus-test`
- Bucket: `ffi_bridge_boundaries`
- Spec refs: §17.7.2
- Fit: `accepted-later`
- Current coverage: `none`
- Proposed fixture root: `ffi.raw_host_binding.positive_overload_spelling_records_host_identity`
- Expected outcome: `stage-dump`
- Notes: Kappa raw host binding generation must assign deterministic spellings for same-spelling host members and record
  the original host identity plus disambiguation rule. This is a direct analogue of arity-distinguished external
  functions. Corroborating harvested tests include `same_module_multiple_imports`, `duplicate_import`, and
  `discarded_names_in_external_are_passed_correctly` in `tests/compiler-core/src/javascript/tests/externals.rs` plus
  Erlang external call/reference tests in `tests/compiler-core/src/erlang/tests/external_fn.rs`.

### `gleam#1145`: JS external function recursively calls itself

- URL: https://github.com/gleam-lang/gleam/issues/1145
- Artifact path: `repos/github-gleam-lang-gleam/issues.json`
- Artifact state: `issue-plus-test`
- Bucket: `ffi_bridge_boundaries`
- Spec refs: §17.7.2, §17.7.5
- Fit: `accepted-later`
- Current coverage: `none`
- Proposed fixture root: `ffi.raw_host_binding.negative_external_name_collision_self_reference`
- Expected outcome: `negative-diagnostic` or generated-artifact assertion
- Notes: Kappa's deterministic exported-spelling and original-host-member identity rules should prevent accidental
  self-recursive wrapper generation when a generated binding shadows the host member name. Corroborating harvested tests:
  JS external codegen cases for same-name externals, escaped names, and external attributes in
  `tests/compiler-core/src/javascript/tests/externals.rs`.

### `mojo#215`: Python runtime interop in both directions

- URL: https://github.com/modular/modular/issues/215
- Artifact path: `repos/github-modularml-mojo/issues.json`
- Artifact state: `issue-plus-pr-title-scan-plus-test-read`
- Bucket: `ffi_bridge_boundaries`
- Spec refs: §1.1, §17.7.6, §17.7.7
- Fit: `accepted-now` for bridge vocabulary; `accepted-later` for actual Python bridge behavior
- Current coverage: `partial`
- Proposed fixture root: `bridge.runtime.positive_bind_module_yields_value_not_import`
- Expected outcome: `positive-check`
- Notes: Kappa already distinguishes static imports from runtime bridge binding. The staged bridge standard-module test
  and bridge explicitness test cover the vocabulary; later bridge tests should assert that dynamic binding yields
  ordinary values and does not add module dependency edges. Corroborating harvested tests:
  `tests/mojo/stdlib/test/python/test_python_interop.mojo` and
  `tests/mojo/stdlib/test/python/test_python_to_mojo.mojo`.

### `mojo#317`: foreign-language-agnostic CFFI bridge

- URL: https://github.com/modular/modular/issues/317
- Artifact path: `repos/github-modularml-mojo/issues.json`
- Artifact state: `issue-plus-test`
- Bucket: `ffi_bridge_boundaries`
- Spec refs: §17.7.6, §17.7.7, §17.7.7A
- Fit: `accepted-now` for bridge contract vocabulary; `accepted-later` for actual bridge runtime
- Current coverage: `partial`
- Proposed fixture root: `bridge.contract.negative_erased_signature_not_runtime_evidence`
- Expected outcome: `negative-diagnostic`
- Notes: Kappa's bridge contract is explicit runtime evidence. The erased `sig` type argument is never enough to validate
  a foreign surface. The staged bridge import and explicitness tests cover the vocabulary; the behavioral contract
  negative remains future work. Corroborating harvested tests: `tests/mojo/stdlib/test/ffi/test_external_call.mojo`,
  `tests/mojo/stdlib/test/sys/test_ffi.mojo`, and `tests/mojo/stdlib/test/ffi/test_unsafe_union.mojo`.

### `mojo#514`: Python interop reports an error but exits successfully

- URL: https://github.com/modular/modular/issues/514
- Artifact path: `repos/github-modularml-mojo/issues.json`
- Artifact state: `issue-plus-pr-title-scan-plus-test-read`
- Bucket: `ffi_bridge_boundaries`
- Spec refs: §17.7.4A, §17.7.6, §17.7.7, §17.7.7A
- Fit: `accepted-later`
- Current coverage: `partial`
- Proposed fixture root: `bridge.runtime.runtime_negative_bridge_failure_not_success_exit`
- Expected outcome: `positive-run` or `runtime-negative`
- Notes: Bridge failures must remain observable as bridge failure, typed failure, interruption, defect, or cast blame. A
  foreign error must not be silently converted into success. The staged
  `bridge.standard_module.positive_import_boundary_classification_ctors` fixture covers only the standard vocabulary for
  direction and precision classification. Corroborating harvested test:
  `tests/mojo/stdlib/test/python/test_python_error_handling.mojo`.

### `mojo#122`: fully dynamic features are not wired up

- URL: https://github.com/modular/modular/issues/122
- Artifact path: `repos/github-modularml-mojo/issues.json`
- Artifact state: `issue-plus-test`
- Bucket: `ffi_bridge_boundaries`
- Spec refs: §2.7A, §5.10, §17.7.4B
- Fit: `accepted-now` for dynamic vocabulary; `accepted-later` for higher-order boundary monitoring
- Current coverage: `partial`
- Proposed fixture root: `gradual.standard_module.positive_import_dynamic_boundary_types`
- Expected outcome: `positive-check`
- Notes: Kappa's dynamic values are explicit `Dyn` / `DynRep` values and do not introduce ambient unknown typing.
  Higher-order dynamic behavior needs explicit monitoring or must stay opaque/dynamic. The staged positive and negative
  `std.gradual` tests cover only the explicit-vocabulary side. Corroborating harvested tests: Python
  object/property/call/conversion cases in `tests/mojo/stdlib/test/python/test_python_interop.mojo` and
  `tests/mojo/stdlib/test/python/test_python_to_mojo.mojo`.

### `roc#1242`: platform required types must not permit closures

- URL: https://github.com/roc-lang/roc/issues/1242
- Artifact path: `repos/github-roc-lang-roc/issues.json`
- Artifact state: `issue-only`
- Bucket: `ffi_bridge_boundaries`
- Spec refs: §17.7.1, §17.7.4B
- Fit: `accepted-later`
- Current coverage: `none`
- Proposed fixture root: `ffi.portable_abi.negative_closure_in_boundary_signature`
- Expected outcome: `negative-diagnostic`
- Notes: This is a precise Kappa fit: portable ABI excludes closures and higher-order values unless a monitored boundary
  explicitly justifies them.

### `roc#1403` / `roc#1501`: host panic / result protocol at application boundary

- URLs: https://github.com/roc-lang/roc/issues/1403, https://github.com/roc-lang/roc/issues/1501
- Artifact path: `repos/github-roc-lang-roc/issues.json`
- Artifact state: `issue-plus-pr-title-scan`
- Bucket: `ffi_bridge_boundaries`
- Spec refs: §17.7.4, §17.7.4A, §17.7.5
- Fit: `accepted-later`
- Current coverage: `none`
- Proposed fixture root: `ffi.boundary_contract.positive_host_failure_translation_named`
- Expected outcome: `positive-check`
- Notes: Kappa overlays or trusted summaries that expose typed errors must name the triggering host conditions and
  translation contract. Host panic/throw protocols are defects or expected errors only when the boundary states that.

### `ocaml#2471` / `ocaml#2670`: external functions do not pull required native library

- URLs: https://github.com/ocaml/ocaml/issues/2471, https://github.com/ocaml/ocaml/issues/2670
- Artifact path: legacy `ocaml-issues.json`
- Artifact state: `issue-only`
- Bucket: `ffi_bridge_boundaries`
- Spec refs: §17.1.9, §17.7.2.1, §17.7.5
- Fit: `accepted-later`
- Current coverage: `none`
- Proposed fixture root: `ffi.raw_host_binding.positive_auxiliary_artifacts_are_binding_realization`
- Expected outcome: `stage-dump`
- Notes: Selected adapter modes may require companion artifacts, native helper objects, libraries, or registration
  tables. Those artifacts are part of the binding realization and separate-compilation identity.

### `ocaml#5707`: function called from C with more than eight parameters corrupts call

- URL: https://github.com/ocaml/ocaml/issues/5707
- Artifact path: legacy `ocaml-issues.json`
- Artifact state: `issue-only`
- Bucket: `ffi_bridge_boundaries`
- Spec refs: §17.7.1, §17.7.2.1
- Fit: `accepted-later`
- Current coverage: `none`
- Proposed fixture root: `ffi.portable_abi.positive_many_parameter_calling_convention_pinned`
- Expected outcome: `backend-profile`
- Notes: Another concrete reminder that adapter-mode and target ABI identity must include calling convention facts.

### `aeneas#371`: external opaque types need borrow metadata

- URL: https://github.com/AeneasVerif/aeneas/issues/371
- Artifact path: `repos/github-aeneasverif-aeneas/issues.json`
- Artifact state: `issue-only`
- Bucket: `ffi_bridge_boundaries`
- Spec refs: §1.1, §17.7.3, §17.7.4, §17.7.7A
- Fit: `accepted-later`
- Current coverage: `partial`
- Proposed fixture root: `ffi.boundary_contract.negative_opaque_external_borrow_without_contract`
- Expected outcome: `negative-diagnostic`
- Notes: Kappa raw opaque handles carry no ownership, borrow, validity, release, thread-affinity, lifetime, or aliasing
  semantics. A refined or portable surface must state those facts in a wrapper, bridge package, resource type, overlay,
  or trusted summary.

### `creusot#37`: trusted annotation for unsafe/external library models

- URL: https://github.com/creusot-rs/creusot/issues/37
- Artifact path: `repos/github-creusot-rs-creusot/issues.json`
- Artifact state: `issue-only`
- Bucket: `ffi_bridge_boundaries`
- Spec refs: §17.7.4, §17.7.4A, §16
- Fit: `accepted-later`
- Current coverage: `none`
- Proposed fixture root: `ffi.trusted_summary.positive_refined_error_contract_names_translation`
- Expected outcome: `positive-check`
- Notes: Kappa's trusted binding summaries are the analogue, but the test should assert a concrete summary property
  rather than introduce a generic "trusted" escape.

### `creusot#347`: duplicate extern specs across crates

- URL: https://github.com/creusot-rs/creusot/issues/347
- Artifact path: `repos/github-creusot-rs-creusot/issues.json`
- Artifact state: `issue-only`
- Bucket: `ffi_bridge_boundaries`
- Spec refs: §17.1.9, §17.7.2, §17.7.4
- Fit: `accepted-later`
- Current coverage: `none`
- Proposed fixture root: `ffi.trusted_summary.negative_duplicate_summary_identity`
- Expected outcome: `negative-diagnostic`
- Notes: Binding summaries that affect exported signatures or runtime semantics participate in build inputs and semantic
  identity. Duplicate or conflicting summaries should be a deterministic compile-time error.

### `creusot#492`: returning a reborrowed reference in a tuple causes unsoundness

- URL: https://github.com/creusot-rs/creusot/issues/492
- Artifact path: `repos/github-creusot-rs-creusot/issues.json`
- Artifact state: `issue-only`
- Bucket: `quantities_borrowing`
- Spec refs: §5.1.6, §5.1.7, §8.8, §17.7.3
- Fit: `accepted-later`
- Current coverage: `partial`
- Proposed fixture root: `types.records.negative_reborrow_tuple_future_fact_escape`
- Expected outcome: `negative-diagnostic`
- Notes: This is primarily an ownership/proof soundness issue, not FFI. Keep it visible as a crossover because boundary
  contracts for external/opaque borrow-bearing types must not fabricate future facts from past observations.

## Spec Additions To Consider

The following are missing diagnostics / examples rather than test fixtures:

- Add a stable diagnostic family for portable ABI exclusions:
  - non-first-order ABI-visible function
  - closure/callback/function value without higher-order boundary monitoring
  - dictionary/trait evidence/effect/handler/resumption in portable ABI
  - direct borrowed parameter or result in portable ABI
  - bare owning `OpaqueHandle` in a refined portable surface
- Add a worked raw-host-binding example for deterministic overload spelling and host-member identity recording.
- Add a worked bridge example showing that a foreign error from runtime binding is not successful completion.
- Add a worked trusted-summary example that strengthens raw nullability or error typing and records the translation
  contract.
- Clarify the portable story for raw C/native unions, including whether they are opaque, backend-specific unsafe values,
  generated tagged wrappers, or a future `std.ffi` / `std.unsafe` layout type.
- Clarify whether C `long double` and C complex scalar spellings are part of `std.ffi.c`, or whether such bindings must
  remain opaque / shimmed / backend-specific.
