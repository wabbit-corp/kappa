# Issue Import Plan

Purpose: turn mined issues, PRs, docs, and upstream test snapshots from `../datatron/data-scala-issues/` into a
disciplined, repeatable stream of Kappa tests and spec-gap notes.

This plan is deliberately procedural. We should be able to hand it to any contributor and get roughly the same triage decisions, fixture layout, and assertion quality.

## 1. Non-Negotiable Rules

- `Spec.md` is the source of truth.
- We import semantics, not source-language syntax.
- We use test-driven development when an issue implies a behavior change: add the failing fixture first, then implement until it passes.
- One fixture should have one primary reason to exist.
- We prefer the standard Appendix T harness directives, plus current local harness extensions when they materially improve precision.
- We do not land intentionally failing tests.
  - Appendix T has no standard `xfail` or `todo`.
  - If an issue is accepted but the implementation is not ready, it stays in tracking state until the PR that fixes it.
- Every imported issue keeps provenance.
  - The source issue URL must be recorded in the planning/tracking notes.
  - The fixture itself should include a short provenance comment when that helps future readers.
- `SPEC_ADDITIONS.md` is only for missing spec features, unclear spec obligations, or ergonomic proposals.
  - Do not add a `SPEC_ADDITIONS.md` note for an existing spec point that already got a staged or real test.
- The repo-centric corpus under `../datatron/data-scala-issues/repos/<slug>/` is authoritative for new work.
  - Legacy top-level `*-issues.json` files may still be referenced by per-repo manifests and older notes.
  - New searches should start from `manifest.json` and then the relevant per-repo manifest.

## 2. Primary Organizing Principle

We should organize work by **Kappa spec bucket first** and **source repo second**.

Reason:

- Kappa tests should reflect Kappa semantics.
- Different source repos often expose the same underlying Kappa obligation.
- Working bucket-first reduces duplicate fixtures and keeps naming consistent.

The source repo still matters for provenance and prioritization, but it is not the primary taxonomy.

## 3. Corpus Layout And Repo Tiers

The corpus is now repo-centric. The current local snapshot has 86 per-repo manifests under
`../datatron/data-scala-issues/repos/`, with roughly:

- 309,067 archived issues
- 342,780 archived PRs / merge requests
- 12,136 harvested docs files
- 290,543 harvested upstream test files

These counts come from the per-repo manifests, not just the small global index. The global
`../datatron/data-scala-issues/manifest.json` is for discovery; the per-repo manifests are the source of truth for
artifact paths and counts.

### Source Artifact Priority

Use artifacts in this order unless the batch has a reason to do otherwise:

1. `issues.json`: start here for problem statements, regression reports, and direct user-facing failures.
2. `pulls.json`: use PRs to confirm the fixed behavior, locate upstream regression tests, and disambiguate issues whose
   bodies are vague.
3. `tests/manifest.json` plus harvested test files: use these to find minimized fixtures and oracle names, but import the
   Kappa semantic obligation rather than the original source syntax.
4. `docs/manifest.json` plus harvested docs: use docs to understand source-language semantics and to avoid false
   analogies; docs alone do not make an issue importable.
5. `papers/`: use paper notes for concept-first features, especially effects, linearity, totality, staging, and runtime
   semantics.

### Tier 0: Already Active / Proven High Yield

These repos already have candidate notes, staged tests, or a known high hit rate:

- `scala2`
- `scala3`
- `ghc`
- `kotlin`
- `rust`
- `chalk`
- `hylo`
- `granule`
- `koka`
- `effekt`
- `links`
- `unison`
- `lean4`
- `Idris2`
- `agda`
- `racket`
- `rhombus`

### Tier 1: New High-Yield Queues

These new or newly-harvested repos should get first-pass keyword ledgers because they map to new or expanded Kappa spec
areas:

- Dependent types, totality, erasure, universes, and proof elaboration: `FStar`, `rocq`, `Arend`, `cooltt`, `cubical`,
  `narya`, `rzk`, `smalltt`, `1lab`, `ATS-Postiats`, `ATS-Xanadu`
- Ownership, borrowing, regions, resource protocols, and verified extraction: `aeneas`, `creusot`, `Vale`, `Carp`,
  `move`, `inko`
- FFI, host boundaries, checked dynamic values, backend intrinsics, and ABI portability: `zig`, `mojo`, `ocaml`,
  `purescript`, `gleam`, `roc`, `cairo`
- Runtime, fibers, supervision, parallelism, interruption, actors, and resource cleanup: `chapel`, `ponyc`, `encore`,
  `elixir`, `gleam`, `inko`, `rust`, `unison`, `koka`, `effekt`
- Query, relational, rewrite, and dataflow semantics: `flix`, `souffle`, `differential-datalog`, `egglog`, `k`, `maude`,
  `rosette`
- Finite-field, proof-carrying, target-restricted, or blockchain-adjacent compilers where Kappa backend/boundary lessons
  may transfer: `noir`, `circom`, `leo`, `cairo`, `plutus`

### Tier 2: Focused / Selective Mining

These are useful, but should be mined only with a narrow keyword tranche or after a Tier 0/1 pass identifies a matching
gap:

- `elm-compiler`
- `mercury`
- `dhall-lang`
- `coalton`
- `futhark`
- `dex-lang`
- `nickel`
- `factor`
- `BQN`
- `uiua`
- `Odin`
- `stan`
- `webppl`
- `anglican`
- `boomerang`
- `kitten`
- `lamdu`
- `pikelet`
- `mlatu`
- `dyna`
- `scribble-java`
- `sketch-frontend`

### Tier 3: Mostly Background / Tooling / Library-Heavy

These can still produce useful notes, but do not start here for direct fixture import:

- `rust-analyzer`
- `ghc-proposals`
- `mathlib4`
- library-heavy proof corpora unless the issue is about elaborator/compiler behavior rather than library content

Tiering is only a scheduling device. It is not a judgment on repo quality, and a Tier 2/3 repo can be promoted when a
specific Kappa bucket needs it.

### Repo-Specific Steering Notes

- `ghc`
  - Prioritize: indentation and layout, pattern semantics, exhaustiveness, `do`-style elaboration, typeclass-style resolution, and type-inference edge cases that map cleanly onto Kappa.
  - De-prioritize: laziness-specific behavior, Haskell-specific deriving machinery, and GHC runtime or extension interactions that have no Kappa analogue.

- `kotlin`
  - Prioritize: flow-sensitive typing, safe-navigation `?.`, Elvis `?:`, imports and name resolution, nullability-adjacent narrowing patterns, and control-flow-sensitive smart-cast-like behavior that matches Kappa's Chapter 7 rules.
  - De-prioritize: JVM interop, class hierarchy semantics, overload behavior tied to Kotlin's OO method model, and platform-type or Java-specific issues with no Kappa analogue.
  - Read YouTrack issue identity from `idReadable`; the internal numeric `id` is not a stable issue key.

- `aeneas` / `creusot`
  - Prioritize: borrow preservation through translation, region escape, proof/erasure boundaries, trusted summaries, and
    extracted-code boundary contracts.
  - De-prioritize: Rust-specific MIR internals unless the issue exposes a Kappa ownership, erasure, or FFI obligation.

- `FStar` / `rocq` / `Arend` / `cooltt` / `narya` / `rzk`
  - Prioritize: totality, termination checking, universe constraints, erased arguments, pattern coverage, equality
    transport, elaboration-time evaluation, and proof irrelevance.
  - De-prioritize: tactic-library failures and proof-library maintenance unless they expose compiler-visible semantics.

- `zig` / `mojo` / `ocaml` / `purescript` / `gleam` / `roc`
  - Prioritize: FFI boundaries, ABI shape, host binding diagnostics, effect inference around foreign calls, runtime
    capability values, dynamic values, and portable backend limits.
  - De-prioritize: source-language-specific overload or package-manager behavior unless it maps to Chapter 17 pipeline
    or boundary rules.

- `chapel` / `ponyc` / `encore` / `inko` / `elixir`
  - Prioritize: concurrency semantics, structured runtime behavior, fiber/task supervision, cancellation/interruption,
    actor isolation, and cleanup guarantees.
  - De-prioritize: scheduler performance bugs unless they expose a required Chapter 8 / Chapter 14.8 observable.

- `noir` / `circom` / `leo` / `cairo` / `plutus`
  - Prioritize: target-restricted values, checked boundary contracts, finite/integer representation constraints,
    deterministic backend obligations, and proof-carrying compilation boundaries.
  - De-prioritize: chain-specific tooling, wallets, deployment, or ecosystem integration with no Kappa runtime or backend
    analogue.

## 3A. Live Execution Checklist

This is the live tracker for issue-import work. It should be updated whenever staged tests, mining notes, or spec-addition notes change.

### Coordination and tracking

- [x] Add `ghc` and `kotlin` to repo tiers, steering notes, and wave order.
- [x] Create a detailed live execution checklist inside this plan.
- [x] Create `new-tests/README.md` to explain staged spec-derived tests.
- [x] Create `SPEC_ADDITIONS.md`.
- [x] Restrict `SPEC_ADDITIONS.md` to unresolved spec gaps and ergonomic candidates, not staged `new-tests/` coverage.
- [x] Curate a dedicated `GHC_ISSUE_CANDIDATES.md`.
- [x] Maintain a search-coverage ledger in this plan recording keyword scans and direct issue reads by repo.
- [x] Curate a dedicated `LINKS_ISSUE_CANDIDATES.md`.
- [x] Curate a dedicated `TOTALITY_ISSUE_CANDIDATES.md`.
- [x] Curate a dedicated `EFFECTS_ISSUE_CANDIDATES.md`.
- [x] Curate a dedicated `OWNERSHIP_ISSUE_CANDIDATES.md`.
- [x] Curate a dedicated `HYLO_MEMBER_ALIAS_ISSUE_CANDIDATES.md`.
- [x] Curate a dedicated `MACRO_ISSUE_CANDIDATES.md`.
- [x] Confirm a local Kotlin issue corpus entry (`kotlin-issues.json` plus manifest entry) exists under `../datatron/data-scala-issues/`.
- [x] Curate a dedicated `KOTLIN_ISSUE_CANDIDATES.md`.
- [x] Curate a dedicated `COHERENCE_ISSUE_CANDIDATES.md`.
- [x] Backfill exact keyword-search logs for the older Scala mining notes so the ledger format is complete for `scala2` and `scala3`.
- [x] Revise this plan for the repo-centric corpus layout under `repos/<slug>/`.
- [x] Record the current per-repo artifact inventory model: `issues.json`, `pulls.json`, `docs/manifest.json`, and `tests/manifest.json`.
- [x] Add new Kappa buckets for FFI / bridge boundaries and runtime / concurrency obligations now covered by `Spec.md`.
- [x] Add newly available languages to the scheduling tiers without treating every new repo as equally urgent.
- [x] Add PR and harvested-test coverage expectations to the issue-import workflow.
- [x] Create `FFI_RUNTIME_ISSUE_CANDIDATES.md` for §1.1, §2.7, §5.10, §17.7, and Appendix O candidates.
- [x] Create `RUNTIME_CONCURRENCY_ISSUE_CANDIDATES.md` for §8.1, §8.6-§8.7, §9, §14.8, and §17.5 candidates.
- [x] Re-read refreshed `Spec.md` sections for `std.atomic`, `std.supervisor`, `std.hash`, runtime model, and runtime capabilities before staging the runtime/hash tests.
- [x] Curate a dedicated `HASH_COLLECTION_ISSUE_CANDIDATES.md` for §2.7E and collection/query hash acceleration evidence.
- [x] Create `DEPENDENT_PROOF_ISSUE_CANDIDATES.md` for the new Tier 1 proof / totality / erasure repos not already covered by `TOTALITY_ISSUE_CANDIDATES.md`.
- [ ] Add first-pass keyword ledgers for every Tier 1 new repo before reading individual bodies opportunistically.
- [ ] Add a PR/test read ledger row whenever a candidate relies on a PR fix or upstream test file rather than an issue body.

### Initial staged tests in `new-tests/`

- [x] `expressions.safe_navigation.positive_wrap`
- [x] `expressions.safe_navigation.positive_flatten`
- [x] `expressions.elvis.positive_default`
- [x] `expressions.conditionals.positive_constructor_test_projection`
- [x] `expressions.conditionals.positive_constructor_test_disambiguates_projection`
- [x] `queries.for_clauses.positive_dependent_generators`
- [x] `queries.ordering.positive_basic`
- [x] `queries.distinct.positive_basic`
- [x] `lexical.whitespace_indentation_continuation.negative_explicit_brace_after_layout`
- [x] `expressions.safe_navigation.negative_ambiguous_generic`
- [x] `queries.ordering.negative_skip_unordered`
- [x] `expressions.match.negative_guard_does_not_make_exhaustive`
- [x] `queries.distinct_by.runtime_positive_first_representative`
- [x] `queries.distinct.runtime_positive_row_environment_key`
- [x] `queries.ordering.runtime_positive_stable_ties`
- [x] `queries.ordering.runtime_positive_order_by_let_bound_current_row`
- [x] `queries.for_clauses.runtime_positive_nested_query_captures_outer_row`
- [x] `queries.for_clauses.runtime_positive_nested_and_multiple_generator_equivalent`
- [x] `queries.nested.runtime_positive_projection_stability`
- [x] `expressions.implicit_parameters.runtime_positive_lexical_local_shadowing`
- [x] `declarations.totality.negative_recursive_cycle_rejected`
- [x] `declarations.totality.positive_let_alias_preserves_decrease_evidence`
- [x] `declarations.totality.positive_with_lowering_preserves_decrease_evidence`
- [x] `declarations.totality.positive_with_helper_preserves_structural_descent`
- [x] `declarations.totality.positive_with_lowering_retains_constructor_relation`
- [x] `declarations.totality.positive_index_refined_recursive_map`
- [x] `declarations.totality.positive_mutual_recursion_hidden_phase`
- [x] `expressions.match.positive_index_refinement_exhaustive`
- [x] `expressions.match.positive_case_impossible_indexed_remainder`
- [x] `expressions.match.negative_misindented_case_body`
- [x] `expressions.patterns.positive_or_pattern_shared_binders`
- [x] `expressions.patterns.negative_or_pattern_mismatched_binders`
- [x] `modules.imports.positive_hidden_wildcard_does_not_shadow_visible`
- [x] `expressions.implicit_parameters.positive_nested_constraint_scopes`
- [x] `expressions.implicit_parameters.positive_branch_refinement`
- [x] `traits.instances.positive_unrelated_import_does_not_disable_candidate`
- [x] `traits.instances.positive_associated_member_from_resolved_dictionary`
- [x] `expressions.lambdas.negative_capture_prevents_earlier_last_use`
- [x] `expressions.lambdas.negative_match_capture_makes_closure_linear`
- [x] `types.universes.quantities.qtt.negative_condition_consumes_before_branch_reuse`
- [x] `types.universes.quantities.qtt.negative_branch_drop`
- [x] `types.universes.quantities.qtt.negative_match_on_erased_scrutinee`
- [x] `declarations.let_in.positive_local_function_capture`
- [x] `declarations.let_in.positive_local_function_depends_on_earlier_local`
- [x] `declarations.let_in.positive_local_type_alias_captures_outer_type_parameter`
- [x] `declarations.let_in.positive_pattern_bound_local_capture`
- [x] `traits.members.positive_default_member_uses_required_member`
- [x] `data_types.type_aliases.positive_alias_preserves_member_projection`
- [x] `traits.instances.positive_same_spelling_associated_members_across_traits`
- [x] `traits.instances.positive_associated_member_projection_through_alias`
- [x] `effects.handlers.runtime_positive_short_circuit_preserved_under_handler`
- [x] `effects.handlers.runtime_positive_non_resuming_operation_runs_finalizer`
- [x] `effects.handlers.negative_first_class_capability_escape`
- [x] `effects.handlers.negative_higher_order_capability_escape`
- [x] `effects.handlers.negative_handler_clause_for_wrong_effect_label`
- [x] `effects.handlers.runtime_positive_mutually_exclusive_handler_cases_order_invariant`
- [x] `macros.hygiene.positive_spliced_name_not_captured_by_generated_binder`
- [x] `macros.hygiene.positive_generated_dependent_record_type_resolves_inner_name`
- [x] `macros.hygiene.positive_imported_macro_preserves_call_site_binding`
- [x] `expressions.elvis.positive_expression_context`
- [x] `expressions.application.positive_generic_lambda_result_inference`
- [x] `expressions.application.positive_polymorphic_result_from_expected_type`
- [x] `expressions.application.positive_polymorphic_returned_function_immediate_application`
- [x] `expressions.application.positive_generic_receiver_method_sugar`
- [x] `effects.return.runtime_positive_labeled_lambda_local_return`
- [x] `effects.return.runtime_positive_labeled_lambda_argument_return`
- [x] `effects.return.negative_labeled_return_crosses_local_function`
- [x] `effects.return.negative_bare_return_crosses_anonymous_lambda`
- [x] `expressions.conditionals.positive_disjunction_constructor_projection`
- [x] `expressions.conditionals.positive_conjunction_constructor_projection`
- [x] `expressions.conditionals.positive_stable_alias_transport`
- [x] `expressions.conditionals.negative_disjunction_projection_type_mismatch`
- [x] `expressions.safe_navigation.negative_non_optional_receiver`
- [x] `modules.imports.negative_duplicate_alias`
- [x] `modules.names.negative_duplicate_term_declaration`
- [x] `modules.names.positive_same_names_in_distinct_modules`
- [x] `modules.files.positive_same_module_fragments`
- [x] `lexical.operator_identifiers_fixity.runtime_positive_pipe_preserves_dotted_forms`
- [x] `expressions.implicit_parameters.positive_supertrait_projection`
- [x] `traits.instances.positive_failed_premise_candidate_discarded_before_coherence`
- [x] `traits.instances.negative_overlapping_instance_heads_rejected`
- [x] `effects.do_blocks.positive_postdominating_constructor_refinement`
- [x] `effects.do_blocks.negative_assignment_requires_var`
- [x] `literals.character_literals_char.negative_empty_or_multiple_scalars`
- [x] `expressions.application.positive_receiver_method_explicit_call`
- [x] `lexical.identifiers.positive_backtick_weird_names`
- [x] `types.generics.positive_qualified_constructor_type_inference`
- [x] `declarations.let_in.positive_pure_block_expression_value`
- [x] `literals.character_literals_char.negative_dangling_escape`
- [x] `literals.character_literals_char.positive_escape_sequences`
- [x] `literals.numeric_literals.negative_trailing_dot_float`
- [x] `effects.handlers.negative_resumption_escape`
- [x] `declarations.totality.positive_structural_recursion_on_mutual_family`
- [x] `traits.instances.negative_overlap_via_associated_member_equality`
- [x] `expressions.lambdas.positive_unit_binder_is_single_argument`
- [x] `types.unit.positive_unit_as_generic_argument`
- [x] `traits.eq.negative_nullable_equality_requires_same_type`
- [x] `expressions.conditionals.positive_refinement_applies_in_let_initializer`
- [x] `expressions.application.positive_named_function_application`
- [x] `expressions.application.negative_named_function_application_labels`
- [x] `types.generics.runtime_positive_nested_generic_payload_projection`
- [x] `expressions.application.positive_generic_receiver_method_infers_argument`
- [x] `expressions.application.positive_function_declaration_rebinds_as_value`
- [x] `data_types.constructor_defaults.positive_named_defaults`
- [x] `data_types.constructor_defaults.negative_default_expression_checked`
- [x] `data_types.constructor_defaults.negative_default_dependency_order`
- [x] `effects.return.runtime_positive_labeled_lambda_rebound_as_value`
- [x] `expressions.conditionals.negative_constructor_test_rhs_must_be_constructor`
- [x] `types.generics.positive_constructor_type_arguments_inferred`
- [x] `data_types.data_declarations.negative_type_object_is_not_constructor`
- [x] `data_types.data_declarations.positive_empty_data_type`
- [x] `types.generics.negative_optional_generic_return_not_plain`
- [x] `types.generics.positive_optional_generic_unwrap_with_fallback`
- [x] `declarations.let_in.positive_local_function_inside_lambda`
- [x] `definitional_equality.positive_optional_function_type_sugar`
- [x] `definitional_equality.positive_optional_tuple_type_sugar`
- [x] `expressions.conditionals.positive_refinement_visible_in_local_function`
- [x] `expressions.conditionals.positive_pattern_condition_refines_scrutinee`
- [x] `effects.for_loops.positive_iterator_protocol`
- [x] `expressions.lambdas.negative_expected_unit_does_not_discard_result`
- [x] `effects.loops.negative_loop_not_pure_expression`
- [x] `effects.loops.positive_loop_else_typechecks`
- [x] `declarations.let_in.negative_local_value_signature_checked`
- [x] `types.projections.positive_receiver_projection_sugar`
- [x] `ffi.standard_module.positive_import_core_ffi_types`
- [x] `ffi.standard_module.negative_ffi_types_not_implicit`
- [x] `ffi.standard_module.positive_import_c_abi_spelling_types`
- [x] `ffi.standard_module.negative_c_abi_types_not_implicit`
- [x] `gradual.standard_module.positive_import_dynamic_boundary_types`
- [x] `gradual.standard_module.negative_dyn_not_implicit`
- [x] `bridge.standard_module.positive_import_bridge_contract_types`
- [x] `bridge.standard_module.negative_bridge_contract_not_implicit`
- [x] `bridge.standard_module.positive_import_boundary_classification_ctors`
- [x] `runtime.hash.behavior.positive_hashable_refines_eq_and_borrows`
- [x] `runtime.hash.behavior.negative_hash_state_linear_reuse`
- [x] `runtime.hash.behavior.positive_set_map_wellformed_without_hashable`
- [x] `runtime.atomic.behavior.runtime_positive_exchange_and_fetch_add_return_old`
- [x] `runtime.atomic.behavior.runtime_positive_compare_exchange_success_failure`
- [x] `runtime.atomic.behavior.negative_atomic_ref_not_ordinary_ref`
- [x] `runtime.supervisor.behavior.runtime_positive_shutdown_idempotent_await_success`

### Spec-gap / ergonomics note batches

- [x] Optional-chaining diagnostic / ergonomics batch
- [x] Query diagnostic batch
- [x] Handler selection / finalization note batch
- [x] Totality / elaboration note batch
- [x] Macro-hygiene-across-module-boundaries note batch
- [x] Start dependent-proof / elaboration wave with first Idris2 tranche
- [x] Stage first Idris2 local-let layout direct-fit test
- [x] Stage Idris2 residual catch-all constructor-refinement test
- [x] Add Idris2 ruled-out-pattern normalization spec-clarity note
- [x] Kotlin Array / pipeline ergonomics note batch
- [x] Kotlin finite type-set bounds ergonomics note
- [x] Kotlin dotted-continuation ergonomics note
- [x] Kotlin Unit-return callback type shorthand ergonomics note
- [x] Kotlin Unit-return parser shorthand ergonomics note update
- [x] Kotlin function-valued application diagnostic parameter-name note
- [x] Kotlin Array contains / sort ergonomics note update
- [x] Kotlin Array binary-search ergonomics note update
- [x] Kotlin DynamicType / DynRep portable-instance clarity note
- [x] Kotlin erased generic runtime-test / reified-modifier note update
- [x] Kotlin redundant safe-navigation diagnostic note
- [x] Kotlin redundant safe-navigation backend diagnostic note update
- [x] Kotlin ordinary function default-argument ergonomics note update
- [x] Kotlin Array documentation / construction ergonomics note update
- [x] Kotlin Array creation helper ergonomics note update
- [x] Kotlin Array multi-index accessor ergonomics note update
- [x] Kotlin portable range surface / endpoint-overflow note
- [x] Kotlin unary-plus numeric-expression portability note
- [x] Kotlin signed numeric expected-type propagation test
- [x] Kotlin try / finally finalizer typing tests
- [x] Kotlin try / except handler-result test
- [x] Kotlin qualified generic constructor with function-type argument test update
- [x] Kotlin structural forwarding / delegation ergonomics note
- [x] Kotlin user-defined plusAssign ergonomics note update
- [x] Kotlin runtime class-token / DynamicType note update
- [x] Kotlin nested elif desugaring test
- [x] Kotlin try / finally result typing coverage update
- [x] Kotlin expected-Unit no-discard coverage update
- [x] Kotlin same-name extension/computed projection ergonomics note update
- [x] Kotlin test-root / private-access visibility note
- [x] Kotlin explicit Unit no-op match branch test
- [x] Kotlin optional iteration source ergonomics note
- [x] Kotlin map literal test
- [x] Kotlin optional tuple coverage update
- [x] Kotlin contextual generic lambda inference test
- [x] Kotlin projection result-type inference ergonomics note
- [x] Kotlin single-expression loop body ergonomics note
- [x] Kotlin augmented-assignment / plusAssign note update
- [x] Kotlin nullable assertion / redundant safe-navigation diagnostic note update
- [x] Kotlin direct iterator-loop coverage update
- [x] Kotlin import-alias coverage mapping
- [x] Kotlin contextual lambda inference coverage update
- [x] Kotlin function-type / lambda syntax ergonomics note update
- [x] Kotlin identity equality note update
- [x] Kotlin class-object increment ergonomics note update
- [x] Kotlin loop-body syntax diagnostic note update
- [x] Kotlin non-optional Elvis negative test
- [x] Kotlin hashability current-spec reclassification
- [x] Kotlin Array literal / range accessor ergonomics note update
- [x] Kotlin String portable convenience-member note
- [x] Kotlin String template rendering note update
- [x] Kotlin identity-equality spec-clarity note
- [x] Kotlin String compound-assignment convenience note update
- [x] Kotlin augmented-assignment / increment ergonomic note update
- [x] Kotlin class-object / receiver-lambda background classification update
- [x] Kotlin boolean-refinement coverage mapping update
- [x] Kotlin infix-gating / nullable-array background classification update
- [x] Kotlin generic safe-navigation ergonomics note update
- [x] Kotlin Array constructor ergonomics note update
- [x] Kotlin namespace-import / receiver-call background classification update
- [x] Kotlin immutable-assignment coverage mapping update
- [x] Kotlin diagnostics-only unused-value classification update
- [x] Kotlin generic-runtime-is poor-fit classification update
- [x] Kotlin increment-lowering ergonomics note update
- [x] Kotlin nullable-type-test / tooling background classification update
- [x] Kotlin overloaded-assignment ergonomics note update
- [x] Kotlin dotted-suffix formatting note update
- [x] Kotlin erased-cast DynamicType note update
- [x] Kotlin recursive/performance/null-assertion background classification update
- [x] Kotlin assignment-expression divergence mapping update
- [x] Kotlin Array constructor descriptor note update
- [x] Kotlin indexed-increment backend classification update
- [x] Kotlin nullable-receiver helper ergonomics note update
- [x] Kotlin class-object verify-error background classification update
- [x] Kotlin set/map encounter-order portability note update
- [x] Kotlin nominal class/trait syntax background classification update
- [x] Kotlin branch-RHS anonymous-lambda parser fixture
- [x] Kotlin type-constructor term-facet coverage mapping update
- [x] Kotlin loop-body/layout and overload-specificity background classification update
- [x] Kotlin literal-format annotation ergonomics note
- [x] Kotlin redeclaration diagnostic deduplication note update
- [x] Kotlin computed-projection getter shorthand ergonomics note
- [x] FFI boundary contract ergonomics / diagnostics note batch
- [x] Kotlin host-callback adapter ergonomics note
- [x] Kotlin variadic host/backend ABI diagnostic note update
- [x] Kotlin typed / inferred varargs ABI diagnostic note update
- [x] Kotlin primitive-literal vararg overload ABI note update
- [x] Kotlin external constant nullability / trusted-summary note update
- [x] Raw C/native union surface spec-clarity note
- [x] C `long double` / complex ABI spelling spec-clarity note
- [x] Runtime capability and structured-concurrency ergonomics note batch
- [ ] Dynamic representation / checked-cast blame ergonomics note batch
- [ ] Backend profile and portable-ABI diagnostics note batch

### Corpus artifact checklist

- [x] Confirm per-repo manifests exist for the active Tier 0 repos.
- [x] Confirm `ghc` has repo-local PR, docs, and test artifacts.
- [x] Confirm `kotlin` has repo-local docs and test artifacts, with YouTrack issues still referenced through the legacy top-level JSON path.
- [x] Confirm Scala 2 / Scala 3 repo-local PR, docs, and test artifacts exist for backfilling upstream regression names.
- [ ] For each new Tier 1 repo, record the manifest path and artifact counts in the first candidate note that uses it.
- [ ] For every accepted-now issue, check whether a linked PR or upstream regression test exists before writing the Kappa fixture.
- [ ] For every imported fixture derived primarily from upstream test files, record the harvested test path in the candidate note.

### Promotion work still needed

- [ ] Move staged `?.`, `?:`, `is`, and query tests from `new-tests/` into `tests/Kappa.Compiler.Tests/Fixtures` once parser and harness support are ready.
- [ ] Move staged FFI / gradual / bridge standard-module tests from `new-tests/` into `tests/Kappa.Compiler.Tests/Fixtures` once the standard modules exist in the local stdlib.
- [ ] Move staged atomic / supervisor / hash behavior tests from `new-tests/` into `tests/Kappa.Compiler.Tests/Fixtures` once those standard modules exist in the local stdlib and the backend capability profile checks are wired.
- [x] Stage the remaining unstaged Scala direct-fit batch from `SCALA_ISSUE_FIXTURE_CANDIDATES.md`.
- [x] Stage the first remaining unstaged Scala implicit / typeclass batch from `SCALA_IMPLICIT_ISSUE_CANDIDATES.md`.
- [x] Start a GHC direct-fit mining pass and promote the first accepted GHC batch to staged tests.
- [x] Start a Links direct-fit mining pass and promote the first accepted Links batch to staged tests.
- [x] Start a totality direct-fit mining pass and promote the first accepted totality batch to staged tests.
- [x] Curate a dedicated ownership / borrowing note and stage the first accepted-now ownership batch.
- [x] Curate a dedicated effects-wave note before starting the first handler / resumption staging batch.
- [x] Stage the first accepted-now effects batch from `EFFECTS_ISSUE_CANDIDATES.md`.
- [x] Curate a dedicated macro-wave note and stage the first accepted-now macro batch.
- [x] Start a Kotlin direct-fit mining pass now that the local Kotlin dataset exists.
- [x] Curate a dedicated solver/coherence note and stage the first accepted-now solver batch.
- [ ] Add precise diagnostic assertions to staged negative tests after stable codes exist.

### Active Wave Status Board

This is the high-level execution board. It should answer, at a glance, which waves are merely planned, which have
staged output already, and which have identified follow-up work that must not get lost.

| Wave | Status | Evidence already in hand | Remaining work that should stay visible |
| --- | --- | --- | --- |
| Wave 1: Immediate direct fits | `in_progress` | existing Scala backlog notes with backfilled keyword logs; several Scala direct / implicit imports already exist in the real fixture suite; the remaining Scala direct-fit tranche, the remaining Scala implicit tranche, and the remaining Scala import-invariance / associated-member tranche are now staged in `new-tests/`; `GHC_ISSUE_CANDIDATES.md`; staged GHC roots for layout, guard exhaustiveness, indexed refinement, constructor-informed projection, and lexical implicit shadowing; `HYLO_MEMBER_ALIAS_ISSUE_CANDIDATES.md`; staged Hylo roots for local type-alias capture of outer generic context, default-member resolution, transparent alias projection, associated-member projection through alias, and same-spelling associated members across traits | promote staged Scala / GHC / Hylo direct-fit roots into real fixtures when ready; the only Scala items still intentionally not staged are the float-literal spec-holds |
| Wave 2: Ownership and borrowing | `expanded_staged_batch_in_progress` | `OWNERSHIP_ISSUE_CANDIDATES.md`; staged ownership roots for closure-aware last-use, match-induced linear capture, erased-scrutinee discrimination, branch-condition consumption, branch-drop, block-local function capture, transitive local-function capture, local type-alias capture of outer generic context, and pattern-bound local capture; existing real fixtures already cover disjoint sibling borrows, borrow escape, and basic linear duplication | promote the staged ownership roots; keep `hylo#1599`, `hylo#571`, and other later projection / `inout` analogues visible; only reopen the Rust / Hylo / Granule scan when we deliberately start a variant-pattern, alias-plus-associated-member, modal-extension, or diagnostics follow-up |
| Wave 3: Effects, handlers, and resumptions | `first_staged_batch_complete` | `EFFECTS_ISSUE_CANDIDATES.md`; staged effects roots for short-circuit preservation under handlers, non-resuming operations still running `defer`, first-class capability escape, higher-order capability escape, wrong-label handler-clause rejection, and handler-case order invariance; spec-gap notes already recorded in `SPEC_ADDITIONS.md` | promote the staged effects roots; keep the optimizer / trace-shaped effects backlog visible for the later backend pass |
| Wave 4: Queries | `first_staged_batch_complete` | `LINKS_ISSUE_CANDIDATES.md`; staged Links roots for nested capture, ordering over let-bound row names, generator equivalence, nested projection stability, and query ordering/distinctness basics | promote staged query roots; continue the next Links normalization / scoping tranche; revisit `links#340` in the later solver/effects wave |
| Wave 5: Flow typing, safe navigation, and Elvis | `expanded_staged_batch_in_progress` | `KOTLIN_ISSUE_CANDIDATES.md`; existing real fixtures already cover the stable Kotlin-adjacent baseline for safe navigation, Elvis, constructor-test projection, ordinary function application, imports/modules, literals, comments, and script-mode hooks; staged Kotlin roots now cover the direct imports enumerated in the Kotlin note, including generic/result/contextual lambda inference, qualified constructors with function-type payloads, receiver call/projection sugar, constructor defaults, Unit behavior, equality/refinement, optional function and tuple type precedence, iterator/loop semantics, explicit no-op `Unit` branches, map literals, pure blocks, assignment-target rejection, character and numeric literal edge cases, non-optional Elvis rejection, `try` / `finally` finalizer typing, `try` / `except` handler-result semantics, nested `elif`, backtick identifiers, constructor-only `is`, import/name conflicts, module fragments, labeled returns, boolean refinement, post-dominating `do` refinement, and parenthesized anonymous-function application as a branch RHS; Kotlin direct reads are classified through `KT-667`, with `KT-499`, `KT-506`, `KT-519`, `KT-522`, `KT-532`, `KT-539`, `KT-540`, `KT-541`, `KT-543`, `KT-544`, `KT-545`, `KT-546`, `KT-547`, `KT-548`, `KT-558`, `KT-561`, `KT-563`, `KT-564`, `KT-565`, `KT-567`, `KT-582`, `KT-583`, `KT-584`, `KT-595`, `KT-596`, `KT-599`, `KT-601`, `KT-635`, `KT-637`, `KT-638`, `KT-639`, `KT-641`, `KT-643`, `KT-646`, `KT-648`, `KT-649`, `KT-652`, `KT-656`, `KT-658`, and `KT-667` confirmed absent and `KT-498`, `KT-501`, `KT-502`, `KT-503`, `KT-504`, `KT-508`, `KT-509`, `KT-510`, `KT-511`, `KT-512`, `KT-513`, `KT-514`, `KT-515`, `KT-518`, `KT-520`, `KT-521`, `KT-523`, `KT-524`, `KT-525`, `KT-526`, `KT-527`, `KT-528`, `KT-529`, `KT-531`, `KT-533`, `KT-534`, `KT-535`, `KT-536`, `KT-538`, `KT-542`, `KT-550`, `KT-551`, `KT-552`, `KT-553`, `KT-554`, `KT-555`, `KT-556`, `KT-557`, `KT-559`, `KT-560`, `KT-562`, `KT-568`, `KT-569`, `KT-570`, `KT-571`, `KT-572`, `KT-573`, `KT-574`, `KT-575`, `KT-576`, `KT-577`, `KT-578`, `KT-579`, `KT-580`, `KT-581`, `KT-585`, `KT-586`, `KT-587`, `KT-588`, `KT-589`, `KT-590`, `KT-591`, `KT-592`, `KT-593`, `KT-594`, `KT-597`, `KT-598`, `KT-600`, `KT-602`, `KT-603`, `KT-604`, `KT-605`, `KT-606`, `KT-607`, `KT-608`, `KT-609`, `KT-610`, `KT-611`, `KT-612`, `KT-613`, `KT-614`, `KT-615`, `KT-616`, `KT-617`, `KT-618`, `KT-619`, `KT-620`, `KT-621`, `KT-622`, `KT-623`, `KT-624`, `KT-625`, `KT-626`, `KT-627`, `KT-628`, `KT-629`, `KT-630`, `KT-631`, `KT-632`, `KT-633`, `KT-634`, `KT-636`, `KT-640`, `KT-642`, `KT-644`, `KT-645`, `KT-650`, `KT-651`, `KT-653`, `KT-655`, `KT-657`, `KT-660`, `KT-661`, `KT-662`, `KT-663`, `KT-664`, `KT-665`, and `KT-666`, plus `KT-647` and the unary-plus subcase of `KT-494`, tracked as background, already-covered imports, current tests, or future spec/ergonomic evidence; `KT-566` is already covered by the iterator-protocol fixture, `KT-654` is covered by the staged branch-RHS lambda-application fixture, and `KT-659` is already covered by existing type-constructor term-facet fixtures | promote the staged Kotlin roots; keep the safe-call-implies-present-receiver family visible as an ergonomic/spec-follow-up rather than pretending v0.1 already guarantees it; continue with `KT-668`-`KT-672` by `idReadable` in the refreshed YouTrack corpus |
| Wave 6: Macros, hygiene, and staging | `first_staged_batch_complete` | `MACRO_ISSUE_CANDIDATES.md`; staged macro roots for local binder hygiene, generated dependent-record-type hygiene, and imported-macro module-boundary hygiene | promote the staged macro roots; keep export-round-trip and syntax-category-specific background issues visible without pretending Kappa already has a declaration-macro surface |
| Wave 7: Totality and termination | `first_staged_batch_complete` | `TOTALITY_ISSUE_CANDIDATES.md`; staged totality roots for cycles, alias-preserved descent, helper lowering, constructor-spine descent, index-refined recursion, and mutual recursion | promote staged totality roots; keep `lean4#262` and `agda#3` visible as later imports once the surface form is pinned down more tightly |
| Wave 8: Coherence and solver edges | `expanded_staged_batch_in_progress` | `COHERENCE_ISSUE_CANDIDATES.md`; staged solver roots now cover local supertrait projection, failed-premise candidate discard before coherence, and a basic overlapping-instance rejection; Scala implicit note now records supertrait projection as staged rather than still missing | promote the staged coherence roots; keep the associated-member overlap and unrelated-premise ambiguity cases visible for the next solver tranche |
| Wave 9: FFI, dynamic values, and bridge boundaries | `test_backfill_started` | `FFI_RUNTIME_ISSUE_CANDIDATES.md`; first non-Kotlin FFI pass covers Zig, Mojo, Gleam, OCaml, Roc, Aeneas, and Creusot issue searches; harvested-test reads now include Zig C ABI positive and compile-error fixtures, Gleam external-function target/annotation/codegen tests, Mojo FFI/Python interop tests, and Roc platform-boundary test snapshots; staged roots now cover explicit `std.ffi`, `std.ffi.c`, `std.gradual`, and `std.bridge` standard-module vocabulary plus boundary classification constructors | continue PR/body backfill for the accepted-later ABI/layout/bridge candidates; promote staged standard-module tests once `std.ffi`, `std.ffi.c`, `std.gradual`, and `std.bridge` exist locally; later ABI diagnostics should use the read Zig compile-error fixtures as oracle shapes |
| Wave 10: Runtime, fibers, interruption, and resource cleanup | `behavior_batch_staged` | `RUNTIME_CONCURRENCY_ISSUE_CANDIDATES.md`; first manifest/keyword tranche covers Chapel, Pony, Encore, Elixir, and Inko inventory; direct reads include Chapel structured task synchronization / cleanup / atomics, Pony finalization / monitoring / stdio blocking / CAS memory orders, Encore future completion and tracing, Elixir supervisor/startup/interrupt/receive cases, and Inko process scheduling / receive waits / scoped panics / blocking pools / destructor escape / atomics; staged roots now cover atomic exchange/fetch-old, compare-exchange success/failure, atomic-vs-ordinary-ref separation, and supervisor shutdown idempotence | backfill PR/test artifacts for the accepted-later runtime candidates; read harvested runtime tests for Inko/Chapel/Pony/Encore/Elixir; stage deeper runnable fiber/interruption/supervision fixtures only after the local runtime surface can execute the standard `fork`, `await`, `Scope`, `Monitor`, `timeout`, `race`, `defer`, and `using` shapes reliably |
| Wave 10B: Runtime hashing and collection acceleration | `behavior_batch_staged` | `HASH_COLLECTION_ISSUE_CANDIDATES.md`; first hash/collection pass covers Inko `HashMap` / map-performance / hash algorithm issues and Chapel user-defined hashing / hash-combine / return-requirement issues; staged roots now cover `Hashable` refining `Eq`, borrow-not-consume hashing, linear `HashState`, and Set/Map well-formedness without `Hashable`; `SPEC_ADDITIONS.md` tracks the open decision about whether `HashCode` should expose `Eq` / `Ord` as opaque same-execution operations | backfill PR/test artifacts for accepted hash candidates; read harvested map/set/distinct/grouping tests; later fixtures should prove hash acceleration preserves denotation rather than standardizing a specific hash algorithm |
| Wave 11: Dependent proof, erasure, and elaboration | `first_idris2_batches_started` | `DEPENDENT_PROOF_ISSUE_CANDIDATES.md`; Idris2 direct reads now cover `Idris2#13-#27`; staged roots `expressions.implicit_parameters.positive_lambda_binds_implicit_argument_in_context`, `declarations.let_in.positive_multiline_initializer_without_ascription`, and `expressions.match.positive_residual_catch_all_derives_remaining_constructor` import the strongest current-spec fits from `Idris2#14`, `Idris2#20`, and `Idris2#27`; the initial keyword ledger for this note records `implicit`, `metavariable`, `elaboration`, `hole`, `coverage`, `normalization`, `erased`, `universe`, `transport`, `equality`, `with`, `total`, and `termination` against the Idris2 corpus, and `Idris2#26` is now recorded as a branch-local normalization spec-clarity follow-up | continue with `Idris2#28-#32`, keep `#26` visible as a normalization follow-up, cross-link `#19` back to `TOTALITY_ISSUE_CANDIDATES.md`, then broaden the same note to `FStar`, `rocq`, `Arend`, `cooltt`, `cubical`, `narya`, `rzk`, `smalltt`, `ATS-Postiats`, `ATS-Xanadu`, and `1lab` while avoiding proof-library-only failures |
| Wave 12: PR and upstream-test backfill | `planned_cross_cutting_wave` | repo-local `pulls.json`, `docs/manifest.json`, and `tests/manifest.json` are now available for most repos, including the active Tier 0 repos; the biggest harvested test snapshots include `rust`, `kotlin`, `chapel`, `ghc`, `scala3`, `scala2`, `agda`, `lean4`, `mercury`, and `roc` | for each existing candidate note, add a pass that resolves accepted-now issues to linked PRs and harvested regression tests; record exact PR ids and test paths in the ledger before using them as fixture evidence |

### Logging Rules For Search And Read Coverage

Every repo row in the ledger below should record enough detail that a later pass can resume without redoing the same
triage.

- Record either exact issue ids or exact numeric ranges.
  - If body reads are sparse, store exact ids like `#11`, `#13`, `#25`.
  - If a contiguous tranche was reviewed, store it as a range like `#100-#125`.
- Record the exact keywords used for corpus scans.
  - Do not summarize as "query scan" or "effect scan" alone.
- Record where the triage outcome lives.
  - Usually this is the dedicated candidate note for that repo or batch.
- Record artifact coverage separately.
  - Issue reads, PR reads, docs reads, and harvested-test reads are different work; do not collapse them into one
    "repo scanned" claim.
  - For PRs, record exact PR numbers or numeric ranges.
  - For harvested tests/docs, record exact manifest paths and source paths or a glob that was actually searched.
- Record the next unread tranche explicitly.
  - Use either exact issue ids or the next numeric range.
- When a previously missing local corpus appears, update the ledger on the same day so stale blockers do not persist.
- When a repo has both legacy top-level issue archives and repo-local artifacts, record which path was used.

### Corpus Search Coverage Ledger

Update this ledger whenever a repo-level keyword search or a direct issue read changes. The goal is to track both work
done and obvious next work, so we do not repeatedly scan the same ground.

| Repo | Note / backlog file | Search coverage so far | Direct issue reads or confirmed ranges | Current outputs / state | Next unread / next pass |
| --- | --- | --- | --- | --- | --- |
| `scala2` | `SCALA_ISSUE_FIXTURE_CANDIDATES.md`, `SCALA_IMPLICIT_ISSUE_CANDIDATES.md` | backfill keyword scans run against titles and bodies for `import|misindent|indent|layout|guard|match|infix|lambda|anonymous function|or-pattern|float literal|trailing dot` and `implicit|given|context bound|type alias|alias|shadow|search|summon|companion|package object` | issue ids currently captured in notes include `#182`, `#211`, `#265`, `#1272`, `#12736`, `#1564`, `#2709`, `#2714`, `#298`, `#3453`, `#3927`, `#4270`, `#4414`; exact body-read confirmations still need selective backfill where we rely on more than the issue title | the Scala 2 direct slice is now fully represented as either real fixtures or staged tests, and the later import-invariance slice is staged as `traits.instances.positive_unrelated_import_does_not_disable_candidate`; the only Scala 2 item still intentionally unstaged is the spec-hold float-tokenization issue `#265` | keep `#265` parked as a spec-hold |
| `scala3` | `SCALA_ISSUE_FIXTURE_CANDIDATES.md`, `SCALA_IMPLICIT_ISSUE_CANDIDATES.md` | backfill keyword scans run against titles and bodies for `import|misindent|indent|layout|guard|match|infix|lambda|anonymous function|or-pattern|float literal|trailing dot` and `implicit|given|context bound|type alias|alias|shadow|search|summon|companion|package object` | issue ids currently captured in notes include `#530`, `#739`, `#877`, `#1286`, `#1857`, `#2234`, `#2334`, `#2911`, `#3284`, `#5427`, `#5469`, `#5549`, `#8708`, `#9822`, `#9992`, `#11632`; exact body-read confirmations still need selective backfill where we rely on more than the issue title | the Scala 3 direct slice, remaining implicit slice, and associated-member slice are now all represented as either real fixtures or staged tests: `#5469`, `#877`, and `#1857` are staged; the only Scala 3 item still intentionally unstaged is the spec-hold float-literal issue `#2334` | keep `#2334` parked as a spec-hold |
| `rust` | `OWNERSHIP_ISSUE_CANDIDATES.md` | full-corpus keyword scan for `borrow`, `lifetime`, `closure capture`, `capture`, `disjoint`, `move`, `last use` | exact body reads: `#925`, `#1243`, `#1399`, `#1455`, `#1566`, `#1818`, `#1894`, `#2041`, `#2329`, `#8636`, `#14273`, `#27889`, `#30745` | ownership note complete; staged batch covers closure-aware future use under capture and match-induced linear capture; the later Rust reads are now classified as explicit-capture, optimization, or syntax-shaped background rather than new direct imports | pause the current Rust ownership scan unless we deliberately open a dedicated variant-payload / move-pattern follow-up |
| `hylo` | `OWNERSHIP_ISSUE_CANDIDATES.md`, `HYLO_MEMBER_ALIAS_ISSUE_CANDIDATES.md` | full-corpus keyword scan for `borrow`, `lifetime`, `capture`, `inout`, `accessor`, `alias`; focused follow-up scan for `typealias`, `associated type`, `generic arguments`, `receiver`, `member`, `tuple labels`, `extension`, `conformance`, `lookup` | exact body reads: `#3`, `#39`, `#179`, `#410`, `#424`, `#571`, `#629`, `#674`, `#675`, `#676`, `#688`, `#689`, `#814`, `#819`, `#850`, `#878`, `#921`, `#936`, `#955`, `#1042`, `#1088`, `#1398`, `#1550`, `#1582`, `#1599`, `#1707`, `#1807` | ownership note complete; staged batch covers block-local function capture, a spec-driven transitive local-capture companion, local type-alias capture of outer generic context, and pattern-bound local capture; the focused Hylo member/alias note now also covers associated-member projection through alias and same-spelling associated members across traits | pause the current Hylo direct-fit scan unless we deliberately open a new targeted Hylo pass beyond the current alias / associated-member cluster |
| `granule` | `OWNERSHIP_ISSUE_CANDIDATES.md` | full-corpus keyword scan for `linear`, `linearity`, `nonlinear`, `case`, `match`, `usage` | exact body reads: `#4`, `#9`, `#17`, `#23`, `#37`, `#38`, `#42`, `#45`, `#48`, `#52`, `#53`, `#54`, `#56`, `#59`, `#65`, `#195`, `#207`, `#252` | ownership note complete; staged batch covers branch-condition consumption, branch-drop, and erased-scrutinee discrimination; the later Granule reads are now classified as modal-extension or diagnostic-shaped background rather than new ownership fixtures | pause the current Granule ownership scan unless we explicitly open a later modal-extension or diagnostics pass |
| `ghc` | `GHC_ISSUE_CANDIDATES.md` | curated direct-fit pass around layout, guards, indexed exhaustiveness, constructor-informed projection, and local implicit behavior | exact note set includes `#1`, `#29`, `#30`, `#1041`, `#1060`, `#1401`; direct body reads confirmed in this workspace for `#1`, `#366`, `#1401` | direct-fit GHC note complete; staged batch covers layout, guard exhaustiveness, indexed refinement, constructor-informed projection, and lexical implicit shadowing | revisit only after current staged GHC roots are promoted or if we need more layout / indexed-pattern / implicit-evidence cases |
| `links` | `LINKS_ISSUE_CANDIDATES.md` | full-corpus keyword scan for `distinct`, `order by`, `skip`, `take`, `group by`, `for clause`, `where clause`, `query`, `comprehension` | exact body reads: `#11`, `#13`, `#25`, `#26`, `#340`, `#834` | direct-fit Links note complete; staged batch covers nested capture, let-bound ordering, generator equivalence, and projection stability | continue with nearby query scoping / normalization issues after the first staged batch is promoted; keep `links#340` for the later solver/effects wave |
| `unison` | `EFFECTS_ISSUE_CANDIDATES.md` | full-corpus keyword scan for `handler`, `resume`, `resumption`, `finally`, `finalizer`, `effect`, `ability`, `capability`, `short-circuit` | exact body reads: `#374`, `#696`, `#697`, `#979`, `#1010` | effects note now captures the strongest direct and indirect Unison fits; the first staged effects batch now includes wrong-label rejection and handler-case order invariance | keep `#374`, `#697`, and `#696` for the later or spec-follow-up tranche |
| `koka` | `EFFECTS_ISSUE_CANDIDATES.md` | full-corpus keyword scan for `handler`, `resume`, `resumption`, `finally`, `finalizer`, `effect`, `ability`, `capability`, `short-circuit` | exact body reads: `#129`, `#360`, `#564`, `#649` | effects note now captures the strongest Koka direct fits; the first staged effects batch now includes short-circuit preservation and non-resuming-finalizer behavior | revisit `#649` after the first direct batch; leave `#564` as poor-fit background evidence |
| `effekt` | `EFFECTS_ISSUE_CANDIDATES.md` | full-corpus keyword scan for `handler`, `resume`, `resumption`, `finally`, `finalizer`, `effect`, `ability`, `capability`, `short-circuit` | exact body reads: `#50`, `#548`, `#861`, `#971` | effects note now captures the strongest Effekt direct fits; the first staged effects batch now includes direct and higher-order capability-escape negatives | keep `#861` and `#971` visible for the later optimizer / trace-oriented tranche |
| `lean4` | `TOTALITY_ISSUE_CANDIDATES.md`, `MACRO_ISSUE_CANDIDATES.md` | full-corpus keyword scan for `termination`, `non-exhaustive`, `exhaustive`, `with function`, `hygiene`, `macro`; full-corpus macro scan for `macro`, `hygiene`, `quote`, `quotation`, `splice`, `splicing`, `syntax`, `antiquot`, `quasisyntax`, `template`; focused macro follow-up scan for `hygiene`, `scope`, `binding`, `identifier`, `double-quoted`, `structure`, `module`, `export` | exact body reads: `#108`, `#262`, `#281`, `#414`, `#430`, `#586`, `#793`, `#821`, `#1006`, `#1124` | totality note now tracks first-pass termination imports as staged in `new-tests/` | for the macro tranche, continue with `#249` and only then revisit `#821` if we need more generated-record coverage |
| `Idris2` | `TOTALITY_ISSUE_CANDIDATES.md`, `DEPENDENT_PROOF_ISSUE_CANDIDATES.md` | totality scan: `termination`, `total`, `covering`, `with rule`, `with function`; dependent/elaboration scan: `implicit`, `metavariable`, `elaboration`, `hole`, `coverage`, `normalization`, `erased`, `universe`, `transport`, `equality`, `with`, `total`, `termination` | totality reads: `#19`, `#403`, `#493`; dependent/elaboration reads: `#13-#27`; local archive note: earliest available Idris2 issue in this snapshot is `#13` | totality note remains complete for the first batch; dependent-proof note now stages `expressions.implicit_parameters.positive_lambda_binds_implicit_argument_in_context` from `Idris2#14`, `declarations.let_in.positive_multiline_initializer_without_ascription` from `Idris2#20`, and `expressions.match.positive_residual_catch_all_derives_remaining_constructor` from `Idris2#27`, cross-links `#19` back to the totality note, classifies `#23`, `#24`, and `#25` as tooling/import-order/library background, and records `#26` as a branch-local normalization spec-clarity follow-up | continue with `#28-#32` in `DEPENDENT_PROOF_ISSUE_CANDIDATES.md`, then revisit `#164`, `#300`, `#402`, `#430`, `#484` when the Idris2 totality tranche resumes |
| `agda` | `TOTALITY_ISSUE_CANDIDATES.md` | full-corpus keyword scan for `termination`, `with function`, `macro`, `hygiene` | exact body reads: `#3`, `#59`, `#238` | totality note complete; staged batch covers helper-lowered structural descent and fixed-constructor-spine analogues | continue with `#8`, `#53`, `#58`, `#142`, `#217`; keep `#3` visible as an accepted-later helper-lowering import |
| `racket` | `MACRO_ISSUE_CANDIDATES.md` | full-corpus keyword scan for `macro`, `hygiene`, `quote`, `quotation`, `splice`, `splicing`, `syntax`, `antiquot`, `quasisyntax`, `template`; focused follow-up scan for `hygiene`, `scope`, `binding`, `identifier`, `gensym`, `renaming`, `rename`, `export`, `module`, `syntax class`, `template`, `splicing` | exact body reads: `#1483`, `#1493`, `#1746`, `#2133` | macro note complete; the first staged macro batch now includes an imported-macro module-boundary analogue for the strongest Racket export / renaming issues | continue with `#1410`, `#1479`, `#1495` if the macro wave resumes immediately |
| `kotlin` | `KOTLIN_ISSUE_CANDIDATES.md` | full-corpus keyword scan for `?.`, `?:`, `safe call`, `elvis`, `smart cast`, `smartcast`, `flow typing`, `nullability`, `nullable`, `null check`; after the 2026-04-24 YouTrack layout refresh, issue ids must be read from `idReadable`, not internal `id` | exact body reads are enumerated in `KOTLIN_ISSUE_CANDIDATES.md`; latest incremental read attempts classified `KT-663`-`KT-667`, with `KT-667` absent; confirmed absent in current corpus: `KT-269`, `KT-271`, `KT-315`, `KT-356`, `KT-365`, `KT-373`, `KT-414`, `KT-420`, `KT-430`, `KT-452`, `KT-477`, `KT-488`, `KT-499`, `KT-506`, `KT-519`, `KT-522`, `KT-532`, `KT-539`, `KT-540`, `KT-541`, `KT-543`, `KT-544`, `KT-545`, `KT-546`, `KT-547`, `KT-548`, `KT-558`, `KT-561`, `KT-563`, `KT-564`, `KT-565`, `KT-567`, `KT-582`, `KT-583`, `KT-584`, `KT-595`, `KT-596`, `KT-599`, `KT-601`, `KT-635`, `KT-637`, `KT-638`, `KT-639`, `KT-641`, `KT-643`, `KT-646`, `KT-648`, `KT-649`, `KT-652`, `KT-656`, `KT-658`, `KT-667` | Kotlin note complete through `KT-667`; `KT-549` and `KT-571` are staged as contextual lambda inference; `KT-566` is covered by the staged iterator-protocol fixture; `KT-570` is covered by real import-alias fixtures; `KT-577` is staged as a non-optional Elvis negative; `KT-592` is covered by the real conjunction-refinement fixture; `KT-606` is covered by the staged receiver-method-sugar fixtures; `KT-607` is covered by the staged assignment-target negative; `KT-611` is covered by the staged iterator-protocol fixture; `KT-629` is covered by the existing assignment-expression divergence note; `KT-654` is staged as `expressions.match.positive_lambda_application_branch_rhs`; `KT-659` is covered by existing first-class type-constructor fixtures; `KT-578` is now current-spec hashability coverage rather than a missing-spec note; `KT-600` and `KT-636` are folded into the generic safe-navigation ergonomics note; `KT-613`, `KT-618`, `KT-633`, and `KT-634` are folded into the augmented-assignment / increment ergonomics note; `KT-621` is folded into the line-break-before-dotted-suffix ergonomics note; `KT-624` is folded into the `DynamicType` / `DynRep` portability note; `KT-632` is folded into the portable `Array` construction note; `KT-647` is folded into the new set/map encounter-order portability note; `KT-550`, `KT-552`, `KT-556`, `KT-557`, `KT-572`, `KT-574`, `KT-575`, `KT-576`, `KT-579`, `KT-580`, `KT-581`, `KT-600`, `KT-602`, `KT-613`, `KT-618`, `KT-621`, `KT-624`, `KT-632`, `KT-636`, and `KT-647` are spec-addition ergonomics/diagnostic notes; `KT-551`, `KT-553`, `KT-554`, `KT-555`, `KT-559`, `KT-560`, `KT-562`, `KT-568`, `KT-569`, `KT-573`, `KT-585`, `KT-586`, `KT-587`, `KT-588`, `KT-589`, `KT-590`, `KT-591`, `KT-593`, `KT-594`, `KT-597`, `KT-598`, `KT-603`, `KT-604`, `KT-605`, `KT-608`, `KT-609`, `KT-610`, `KT-614`, `KT-615`, `KT-616`, `KT-617`, `KT-619`, `KT-620`, `KT-623`, `KT-625`, `KT-626`, `KT-627`, `KT-628`, `KT-630`, `KT-631`, `KT-633`, `KT-634`, `KT-640`, `KT-642`, `KT-644`, `KT-645`, `KT-650`, `KT-651`, `KT-653`, `KT-655`, `KT-657`, `KT-660`, `KT-661`, `KT-662`, `KT-663`, `KT-664`, `KT-665`, and `KT-666` are compiler-internal, backend, Kotlin-syntax, diagnostic, incomplete-code, or OO/class-object/receiver-lambda/operator-gating/nullable-array/backing-field/namespace-import/unused-analysis/tooling/performance/null-assertion/increment-lowering/class-object/overload-resolution/raw-null/nominal-class-trait/string-supertype/documentation/brace-free-loop-body background; `KT-612` remains a poor fit because Kappa `is` tests constructors rather than erased generic runtime types, and `KT-622` / `KT-657` remain syntax background because Kappa already has `if` / `elif` chains and guarded cases instead of Kotlin `when`-without-subject syntax | continue with `KT-668`, `KT-669`, `KT-670`, `KT-671`, `KT-672` by `idReadable`; keep `KT-247` visible as already-mined safe-navigation coverage, `KT-2127` / `KT-8492` in the ergonomics/spec-follow-up bucket, `KT-2146` in the spec-clarity bucket, `KT-58` in the later finalization bucket, and the `!!` / mutable-local tranche in the intentional-divergence bucket |
| `chalk` | `COHERENCE_ISSUE_CANDIDATES.md` | full-corpus keyword scan for `coherence`, `overlap`, `orphan`, `associated type`, `supertrait`, `ambiguity`, `projection`, `normalize`, `cycle`, `recursive`, `implied bound` | exact body reads: `#2`, `#5`, `#6`, `#8`, `#10`, `#12`, `#13`, `#16`, `#17`, `#42`, `#44`, `#58`, `#62`, `#70`, `#71`, `#74`, `#79`, `#80`, `#85`, `#90`, `#92`, `#111`, `#115`, `#116`, `#144`, `#189`, `#203`, `#214`, `#219`, `#234`, `#235`, `#246`, `#248`, `#250`, `#289`, `#306`, `#313`, `#429`, `#515`, `#716`, `#750`, `#777` | solver/coherence note complete; staged solver roots now cover local supertrait projection, failed-premise candidate discard, basic overlap and overlap-via-associated-member coherence negatives; newer Chalk reads are explicitly classified as GAT-specific background, coinduction / negative-cycle engine internals, IDE performance noise, or solver-architecture details instead of being left unread | continue with `#318`, `#335`, `#351`, `#369`, `#399`; keep `#750` visible for the next coherence tranche and `#74` visible as solver-background evidence for projection uniqueness |
| `chapel` | `RUNTIME_CONCURRENCY_ISSUE_CANDIDATES.md`, `HASH_COLLECTION_ISSUE_CANDIDATES.md` | runtime scan for `fiber`, `task`, `thread`, `async`, `await`, `cancel`, `interrupt`, `mask`, `supervisor`, `scope`, `defer`, `finally`, `cleanup`, `resource`, `atomic`, `race`, `timeout`, `scheduler`, `stm`, `memory order`, `fence`; hash scan for `hash`, `hashing`, `defaultHash`, `hashCombine`, `Hashable`, `user-defined hash`, `Map`, `Associative`, `collision`, `seed` | runtime reads: `#4966`, `#5037`, `#5482`, `#5608`, `#5751`, `#5796`, `#6223`, `#6316`, `#6334`; hash reads: `#9182`, `#11341`, `#14039`, `#18234`, `#18291`, `#18340`, `#19285` | runtime note records atomic candidates and staged atomic behavior tests; hash note records `std.hash` candidates and staged hash behavior tests | backfill Chapel PR/test artifacts for atomics, associative domains, maps, and task cleanup before adding runnable conformance tests |
| `ponyc` | `RUNTIME_CONCURRENCY_ISSUE_CANDIDATES.md` | runtime scan for `fiber`, `task`, `thread`, `actor`, `monitor`, `async`, `await`, `cancel`, `interrupt`, `timeout`, `scheduler`, `supervisor`, `defer`, `finally`, `cleanup`, `resource`, `atomic`, `compare exchange`, `acq_rel` | exact body reads: `#34`, `#126`, `#205`, `#224`, `#344`, `#350` | runtime note records CAS memory-order and monitoring candidates; staged atomic compare-exchange and supervisor shutdown tests use this evidence | read Pony finalizer, monitor, scheduler, and atomics tests before adding backend or runnable runtime fixtures |
| `inko` | `RUNTIME_CONCURRENCY_ISSUE_CANDIDATES.md`, `HASH_COLLECTION_ISSUE_CANDIDATES.md` | runtime scan for `process`, `async`, `recover`, `throw`, `panic`, `fiber`, `actor`, `timeout`, `scheduler`, `drop`, `resource`, `future`, `task`, `thread`, `cancel`, `interrupt`, `atomic`, `race`, `concurrent`; hash scan for `hash`, `hashable`, `hash code`, `hashmap`, `hash map`, `map performance`, `collision`, `random`, `seed`, `distinct`, `group by` | runtime reads: `#110`, `#112`, `#113`, `#213`, `#216`, `#254`, `#256`, `#278`, `#316`, `#321`, `#332`, `#339`, `#375`, `#398`, `#416`; hash reads: `#424`, `#459`, `#828`, `#923` | runtime note records process/scheduler/destructor/atomic candidates; hash note records map hashing and performance candidates; staged tests cover atomic and hash behavior rather than only import discipline | read Inko `test_async_await`, `test_process`, `test_drop`, `test_sync`, `std.map` tests, and linked PRs before runnable fixtures |

### New Corpus Intake Ledger

These rows are intentionally explicit even when no search has started. A future pass should replace `not started` with
the exact keyword query, exact issue/PR ranges, and exact harvested-test paths read.

| Repo group | Manifest paths | Primary Kappa bucket | First required keyword tranche | Current coverage | Next concrete action |
| --- | --- | --- | --- | --- | --- |
| `aeneas`, `creusot` | `repos/github-aeneasverif-aeneas/manifest.json`, `repos/github-creusot-rs-creusot/manifest.json` | `quantities_borrowing`, `ffi_bridge_boundaries` | `borrow`, `lifetime`, `region`, `escape`, `loan`, `move`, `drop`, `ffi`, `extract`, `trusted`, `summary`, `panic`, `unsafe`; FFI pass also searched `opaque`, `trusted`, and external-spec terms | first FFI/boundary reads are recorded in `FFI_RUNTIME_ISSUE_CANDIDATES.md`: `aeneas#252`, `aeneas#371`, `aeneas#784`, `creusot#18`, `creusot#37`, `creusot#286`, `creusot#347`, `creusot#386`, `creusot#440`, `creusot#492`; no PR/test reads yet | decide which borrow-heavy external-type candidates should move to `OWNERSHIP_ISSUE_CANDIDATES.md`; then backfill PR/test artifacts |
| `FStar`, `rocq`, `Arend`, `cooltt`, `narya`, `rzk`, `smalltt`, `1lab` | per-repo manifests under `repos/github-fstarlang-fstar`, `repos/github-rocq-prover-rocq`, `repos/github-jetbrains-arend`, `repos/github-redprl-cooltt`, `repos/github-gwaithimirdain-narya`, `repos/github-rzk-lang-rzk`, `repos/github-andraskovacs-smalltt`, `repos/github-the1lab-1lab` | `totality_termination`, `dependent_proof_elaboration`, `definitional_equality` | `total`, `termination`, `decreases`, `structural`, `erased`, `irrelevant`, `universe`, `level`, `transport`, `equality`, `normalization`, `coverage`, `impossible`, `with`, `elaboration` | not started as a combined dependent-proof wave beyond the new Idris2 seed tranche in `DEPENDENT_PROOF_ISSUE_CANDIDATES.md` | extend `DEPENDENT_PROOF_ISSUE_CANDIDATES.md` repo-by-repo after the first Idris2 tranche; then begin PR/test backfill for accepted-now records |
| `ATS-Postiats`, `ATS-Xanadu`, `Vale`, `Carp`, `move`, `inko` | corresponding per-repo manifests under `repos/` | `quantities_borrowing`, `runtime_concurrency_resources`, `unsafe_debug_backend` | `linear`, `affine`, `borrow`, `consume`, `resource`, `region`, `capability`, `drop`, `destructor`, `move`, `alias`, `unsafe`, `ffi` | not started in the repo-centric corpus | decide whether each accepted candidate belongs in `OWNERSHIP_ISSUE_CANDIDATES.md` or the new FFI/runtime note before writing tests |
| `zig`, `mojo`, `ocaml`, `purescript`, `gleam`, `roc`, `cairo` | corresponding per-repo manifests under `repos/` | `ffi_bridge_boundaries`, `runtime_concurrency_resources`, `compilation_pipeline_backend` | `ffi`, `foreign`, `extern`, `native`, `abi`, `c abi`, `interop`, `host`, `callback`, `dynamic`, `cast`, `unsafe`, `panic`, `resource`, `effect`, `backend`, `wasm`, `js`, `jvm`; harvested-test reads are recorded in `FFI_RUNTIME_ISSUE_CANDIDATES.md` for Zig C ABI/compile-errors, Gleam external functions, Mojo FFI/Python interop, and Roc platform tests | first FFI pass is recorded in `FFI_RUNTIME_ISSUE_CANDIDATES.md` for `zig`, `mojo`, `ocaml`, `gleam`, and `roc`; `purescript` and `cairo` are still not started in this wave; staged standard-module tests now cover `std.ffi.c` as well as `std.ffi`, `std.gradual`, and `std.bridge` | continue with PR bodies and harvested tests for accepted-later records; start `purescript` and `cairo` only after the current ABI/bridge tranche is normalized |
| `chapel`, `ponyc`, `encore`, `elixir`, `inko` | corresponding per-repo manifests under `repos/`; Elixir issues are still referenced through legacy `../../elixir-issues.json` | `runtime_concurrency_resources` | `fiber`, `task`, `thread`, `actor`, `async`, `await`, `cancel`, `interrupt`, `timeout`, `race`, `scheduler`, `supervisor`, `scope`, `defer`, `finally`, `cleanup`, `resource`, `atomic`, `stm`; Pony pass also searched `monitor`, `compare exchange`, `acq_rel`; Chapel pass also searched `memory order`, `fence`; Encore pass also searched `future`, `active object`, `get`, `blocking`; Elixir pass also searched `gen_server`, `supervisor`, `assert_receive`; Inko pass also searched `process`, `recover`, `throw`, `panic`, `drop`, `race`, `concurrent` | first runtime/concurrency note created: `RUNTIME_CONCURRENCY_ISSUE_CANDIDATES.md`; direct reads recorded for `chapel#4966`, `#5037`, `#5482`, `#5608`, `#5751`, `#5796`, `#6223`, `#6316`, `#6334`; `ponyc#34`, `#126`, `#205`, `#224`, `#344`, `#350`; `encore#184`, `#274`, `#412`, `#429`, `#758`, `#802`, `#880`; `elixir#392`, `#849`, `#869`, `#982`, `#1566`, `#1724`; `inko#110`, `#112`, `#113`, `#213`, `#216`, `#254`, `#256`, `#278`, `#316`, `#321`, `#332`, `#339`, `#375`, `#398`, `#416`; staged tests now cover atomic operation behavior and basic supervisor shutdown behavior; Inko harvested runtime test paths found but not read | backfill PR/test artifacts for accepted candidates; read Inko `test_async_await`, `test_process`, `test_drop`, `test_sync`, atomic tests, and linked runtime PRs next |
| `chapel`, `inko` hash / collection acceleration | `repos/github-chapel-lang-chapel/manifest.json`, `repos/github-inko-lang-inko/manifest.json` | `runtime_hash_collections`, `queries` | Chapel search: `hash`, `hashing`, `defaultHash`, `hashCombine`, `Hashable`, `user-defined hash`, `Map`, `Associative`, `collision`, `seed`; Inko search: `hash`, `hashable`, `hash code`, `hashmap`, `hash map`, `map performance`, `collision`, `random`, `seed`, `distinct`, `group by` | first hash note created: `HASH_COLLECTION_ISSUE_CANDIDATES.md`; direct reads recorded for `chapel#9182`, `#11341`, `#14039`, `#18234`, `#18291`, `#18340`, `#19285`; `inko#424`, `#459`, `#828`, `#923`; staged tests now cover hash behavior rules, not just importability | backfill PR/test artifacts for map/set/hash/distinct/grouping candidates before writing denotation-preservation runtime fixtures |
| `flix`, `souffle`, `differential-datalog`, `egglog`, `k`, `maude`, `rosette` | corresponding per-repo manifests under `repos/` | `queries`, `compilation_pipeline_backend`, `definitional_equality` | `query`, `comprehension`, `join`, `group`, `order`, `distinct`, `fixpoint`, `datalog`, `rewrite`, `egraph`, `normalize`, `incremental`, `determinism` | not started beyond the existing Links query pass | open a second query/dataflow wave only after the Links staged roots are promoted or explicitly parked |
| `noir`, `circom`, `leo`, `plutus` | corresponding per-repo manifests under `repos/` | `ffi_bridge_boundaries`, `compilation_pipeline_backend`, `unsafe_debug_backend` | `field`, `constraint`, `witness`, `prove`, `verify`, `backend`, `target`, `deterministic`, `range`, `overflow`, `abi`, `foreign`, `opaque`, `erasure` | not started in the repo-centric corpus | treat as backend/boundary evidence first; do not import chain-specific behavior unless it maps to Kappa backend-profile rules |
| PR/test backfill for Tier 0 notes | active Tier 0 per-repo manifests | cross-cutting | exact PR/test keywords should mirror the candidate note bucket; do not use generic repo-wide grep without recording it | not started as a systematic backfill, though issue reads are already logged for active notes | for each accepted-now record in existing notes, search linked PRs and harvested tests; add exact PR ids and `tests/manifest.json` source paths to the note |

### Accepted-Later Backlog Already Identified

These are concrete issue imports that have already been identified as real Kappa fits but were intentionally not staged in
the current pass. Keeping them here avoids losing them inside longer note files.

| Source | Tracking note | Proposed fixture root | Why it is not staged yet | Next action |
| --- | --- | --- | --- | --- |
| `links#340` | `LINKS_ISSUE_CANDIDATES.md` | `traits.instances.negative_effectful_generalization_escape` | better fit for the later solver / effects wave than the current query-only batch | revisit after the first effects note and the first Scala implicit tranche land |
| `hylo#1599` | `OWNERSHIP_ISSUE_CANDIDATES.md` | `effects.inout.negative_projection_alias_outlives_call` | Kappa's `inout` surface is linear state threading rather than a first-class returned mutable reference, so the first ownership batch should not pretend the source surface matches exactly | revisit after the first ownership batch is promoted and the projection / `inout` tranche is ready for tighter analogues |
| `lean4#821` | `MACRO_ISSUE_CANDIDATES.md` | `macros.hygiene.positive_generated_record_this_reference_survives_splice` | the current staged dependent-record-type fixture already covers the closest Kappa analogue, so a second generated-record hygiene fixture would be redundant until the first macro batch is promoted | revisit after the first macro batch is promoted and we know whether broader generated-record coverage is still missing |
| `chalk#750` | `COHERENCE_ISSUE_CANDIDATES.md` | `traits.instances.positive_irrelevant_premise_does_not_block_associated_projection` | worthwhile solver guardrail, but better staged after the basic overlap/coherence negatives and after verifying how much associated-member projection is already stable in the current solver | revisit after the first coherence-negative tranche lands |
| `KT-2146` | `KOTLIN_ISSUE_CANDIDATES.md` | `expressions.conditionals.positive_else_branch_negative_constructor_refinement` | plausible Kappa fit, but the current spec is not explicit enough about what failure-side `LacksCtor` evidence guarantees for projection from the remaining constructor case | revisit only after the Chapter 7 note in `SPEC_ADDITIONS.md` is resolved or made explicit in `Spec.md` |
| `KT-2127`, `KT-8492` | `KOTLIN_ISSUE_CANDIDATES.md` | `expressions.conditionals.positive_safe_navigation_condition_implies_present_receiver` | plausible future ergonomic extension, but v0.1 does not currently specify refinement from safe-navigation/elvis-derived boolean success back to receiver presence | revisit only if Chapter 7 grows an explicit rule for this family; for now keep it tracked in `SPEC_ADDITIONS.md` |
| `agda#3` | `TOTALITY_ISSUE_CANDIDATES.md` | `declarations.totality.positive_helper_lowering_preserves_hidden_binders` | useful helper-lowering evidence, but not yet a clean surface repro in the current staging style | revisit after the first helper-lowering totality fixtures are promoted and we know whether this needs a directory suite |

### Scala Coverage Snapshot

This snapshot exists so Wave 1 work does not accidentally re-import candidates that are already present in the real
fixture suite.

| Source candidate | Current state | Concrete fixture / note |
| --- | --- | --- |
| `scala3#530` | `imported` | `tests/Kappa.Compiler.Tests/Fixtures/modules.imports.positive_symbolic_term` |
| `scala3#1286` | `imported` | `tests/Kappa.Compiler.Tests/Fixtures/modules.imports.negative_missing_item` |
| `scala3#9992` | `imported` | `tests/Kappa.Compiler.Tests/Fixtures/lexical.whitespace_indentation_continuation.negative_misindented_import` |
| `scala3#11632` | `imported` | `tests/Kappa.Compiler.Tests/Fixtures/lexical.whitespace_indentation_continuation.positive_infix_operator` |
| `scala2#211` | `imported` | `tests/Kappa.Compiler.Tests/Fixtures/expressions.match.runtime_positive_guard_fallthrough` |
| `scala2#1564` | `imported` | `tests/Kappa.Compiler.Tests/Fixtures/expressions.lambdas.runtime_positive_multiple_typed_binders` |
| `scala3#5427` | `imported` | `tests/Kappa.Compiler.Tests/Fixtures/expressions.implicit_parameters.positive_dependent_later_implicit` |
| `scala3#5549` | `imported` | `tests/Kappa.Compiler.Tests/Fixtures/traits.instances.positive_premise_dependency_propagation` |
| `scala3#2234` | `imported` | `tests/Kappa.Compiler.Tests/Fixtures/expressions.implicit_parameters.positive_alias_definitional_equality` |
| `scala3#739` | `imported` | `tests/Kappa.Compiler.Tests/Fixtures/expressions.implicit_parameters.positive_instantiate_before_search` |
| `scala3#8708` | `staged` | `new-tests/expressions.match.negative_guard_does_not_make_exhaustive` |
| `scala3#9822` | `staged` | `new-tests/expressions.match.negative_misindented_case_body` |
| `scala2#182` | `staged` | `new-tests/expressions.patterns.positive_or_pattern_shared_binders`, `new-tests/expressions.patterns.negative_or_pattern_mismatched_binders` |
| `scala2#12736` | `staged` | `new-tests/modules.imports.positive_hidden_wildcard_does_not_shadow_visible` |
| `scala3#5469` | `staged` | `new-tests/expressions.implicit_parameters.positive_branch_refinement` |
| `scala3#877` | `staged` | `new-tests/expressions.implicit_parameters.positive_nested_constraint_scopes` |
| `scala2#2714`, `scala2#2709` | `staged` | `new-tests/traits.instances.positive_unrelated_import_does_not_disable_candidate` |
| `scala3#1857` | `staged` | `new-tests/traits.instances.positive_associated_member_from_resolved_dictionary` |

## 4. Canonical Work Unit

Every candidate issue should be normalized into one **Issue Import Record** before any fixture work starts.

Required fields:

| Field | Meaning |
| --- | --- |
| `source` | Dataset label and issue number, for example `scala3#5427` |
| `artifact_kind` | `issue`, `pr`, `upstream-test`, `doc`, or `paper-note` |
| `artifact_path` | Local corpus path, for example `repos/github-scala-scala3/pulls.json` or `repos/.../tests/...` |
| `source_repo` | Canonical upstream repo or tracker, for example `scala/scala3`, `ghc/ghc`, or YouTrack `KT` |
| `url` | Canonical issue URL |
| `title` | Issue title |
| `bucket` | One primary Kappa bucket |
| `spec_refs` | Exact `Spec.md` sections |
| `fit` | `direct`, `indirect`, `hold-spec`, `hold-impl`, or `reject` |
| `current_coverage` | `none`, `partial`, `covered` |
| `fixture_kind` | `single-file`, `directory-suite`, or `incremental-suite` |
| `expected_outcome` | `positive-check`, `positive-run`, `negative-diagnostic`, `stage-dump`, or `trace` |
| `proposed_fixture_root` | Planned fixture directory name |
| `status` | See status model below |
| `corroborating_artifacts` | Linked PR ids, upstream regression-test paths, docs paths, or paper notes used to confirm semantics |
| `notes` | Why it matters, adaptation notes, or blocking details |

We do not need a separate file for every record immediately, but every accepted candidate should have these fields somewhere in the tracking notes or batch document.

## 5. Status Model

Each issue should be in exactly one of these states:

| Status | Meaning |
| --- | --- |
| `new` | Mined, not yet normalized |
| `triaged` | Bucket and spec refs assigned |
| `accepted-now` | Good fit and ready to become a fixture in the current implementation wave |
| `accepted-later` | Good fit, but should wait for a later batch |
| `hold-spec` | We need spec clarification before writing the test |
| `hold-impl` | The spec is clear, but the implementation area is not ready and we cannot land a failing test |
| `imported` | Fixture exists and is passing |
| `rejected` | Not a good Kappa fit, or already sufficiently covered |

Artifact read state is tracked separately from import status:

| Artifact state | Meaning |
| --- | --- |
| `issue-only` | Only the issue body/title has been read |
| `issue-plus-pr` | A fixing PR or related PR has been read and recorded |
| `issue-plus-test` | A harvested upstream regression test has been read and recorded |
| `docs-confirmed` | Harvested docs were read to verify the source-language semantic claim |
| `paper-confirmed` | A paper note was read to confirm the concept-first analogue |

## 6. Kappa Bucket Taxonomy

Every issue should be assigned to exactly one primary bucket.

Recommended primary buckets:

| Bucket | Typical fixture roots | Main spec areas |
| --- | --- | --- |
| `modules.imports_resolution` | `modules.imports.*`, `modules.names.*`, `modules.visibility_opacity_*` | Chapter 2 |
| `lexical_layout_parser` | `lexical.*`, `literals.*` | Chapters 3-4 |
| `application_lambdas` | `expressions.application.*`, `expressions.lambdas.*` | Chapter 7.1-7.2 |
| `implicit_parameters_traits` | `expressions.implicit_parameters.*`, `traits.*` | Chapters 6.3, 7.3, 12 |
| `conditionals_match_patterns` | `expressions.conditionals.*`, `expressions.match.*` | Chapters 7.4-7.7 |
| `quantities_borrowing` | `types.universes.quantities.*`, `types.records.*` | Chapters 5.1.5-5.1.7, 8.8 |
| `effects_handlers_control` | `effects.*` | Chapter 8, 14.8 |
| `queries` | future `queries.*` roots | Chapter 10 |
| `data_records_unions` | `data_types.*`, `types.records.*` | Chapters 5.4-5.5, 11 |
| `macros_staging` | future `macros.*`, `staging.*` roots | Chapters 5.8-5.9 |
| `totality_termination` | future `declarations.totality.*` or similar | Chapter 6.4 |
| `dependent_proof_elaboration` | `types.universes.*`, `definitional_equality.*`, `declarations.totality.*` | Chapters 5.1, 5.6, 6.4, 14.3, 17.3 |
| `ffi_bridge_boundaries` | `ffi.*`, `dynamic.*`, `boundary.*`, `bridge.*` | §1.1, §2.7, §2.7B, §5.10, §17.7, Appendix O |
| `runtime_concurrency_resources` | `effects.io.*`, `effects.fibers.*`, `runtime.*`, `errors.*` | §8.1-§8.7, Chapter 9, §14.8, §17.5, §17.13 |
| `runtime_hash_collections` | `runtime.hash.*`, `queries.*`, future `collections.*` roots | §2.7E, Chapter 10, §12.1, §15 |
| `unsafe_debug_backend` | `unsafe.*`, `debug.*`, `backend.*`, `ffi.*` | Chapter 16, §17.6-§17.14 |
| `compilation_pipeline_backend` | `backend.*`, `pipeline.*`, `appendices.test_harness.*` | Chapter 17, especially §17.1, §17.4, §17.6-§17.14 |
| `pipeline_incremental_observability` | `appendices.test_harness.*`, future incremental roots | Chapter 17, Appendix T |

Secondary tags are allowed in notes, but only one primary bucket should determine fixture naming and batching.

## 7. Importability Decision Rules

We should decide importability with the following strict order.

### Rule 0: Identify the exact corpus artifact

Before deciding semantics, record the repo manifest and artifact path:

- legacy issue archive path if the manifest still points to a top-level `*-issues.json`
- repo-local `issues.json` or `pulls.json`
- harvested docs or test source path when those artifacts are used
- provider-specific stable id, such as GitHub issue number, GitLab issue IID, or YouTrack `idReadable`

Do not classify a candidate as read if only a search hit was seen.

### Rule 1: Is there a real Kappa analogue?

Accept only if the issue exposes a semantic or structural obligation that Kappa actually has.

Good fits:

- name resolution
- parser/layout behavior
- flow-sensitive typing and narrowing
- pattern semantics
- ownership and borrowing
- effects and handlers
- trait and instance search
- staging and hygiene
- queries
- totality or termination
- FFI, dynamic values, bridge contracts, and host-boundary behavior
- runtime capability values, fibers, supervision, interruption, cleanup, and resource protocols
- backend-profile portability and unsafe/debug gating

Poor fits:

- source-language-specific package objects
- JVM-only or CLR-only quirks with no Kappa analogue
- IDE-only bugs unless they reflect a Chapter 17 obligation
- ecosystem packaging, deployment, CI, or editor behavior unless it exposes a specified pipeline obligation
- scheduler performance bugs without a required Kappa runtime observable
- chain-specific blockchain behavior unless it maps to Kappa backend, boundary, or representation rules
- language features Kappa intentionally does not have

### Rule 2: Is the spec clear enough to test?

If the expected Kappa behavior is not already pinned down by `Spec.md`, the issue goes to `hold-spec`.

### Rule 3: Is the behavior already covered?

Before creating a fixture:

- search fixture names
- search diagnostic codes
- search for a semantically equivalent repro

If current coverage is:

- `covered`: reject or note as already handled
- `partial`: either extend the existing family with a sibling fixture, or add assertions only if the delta is real
- `none`: proceed

### Rule 4: Can we land it now without an `xfail`?

If the issue is valid but would produce a red suite until major implementation work lands, mark it `hold-impl` or `accepted-later`.

## 8. Fixture Granularity Rules

Default rule: **one issue imports to one fixture**.

Split an issue into multiple fixtures only when the source issue actually covers multiple distinct Kappa obligations.

Good reasons to split:

- parser behavior and runtime behavior are separate
- one issue reveals both a positive and a negative law
- one issue maps to two different spec buckets

Bad reasons to split:

- the original issue had several examples of the same bug
- we want to batch several unrelated regressions into one directory

Good reasons to merge:

- several source issues collapse to the same minimal Kappa repro
- multiple repos expose the same spec obligation and the same observable

When we merge multiple source issues into one fixture, the tracking note must say so explicitly.

## 9. Harness Selection Rules

Choose the simplest harness form that can express the obligation.

### Use a single-file inline test when:

- the issue is local to one file
- imports are not required
- the behavior is parse/check/type/runtime for a small repro

### Use a directory suite when:

- module structure matters
- there are multiple files
- imports, exports, visibility, dotted lookup, or cross-file instance resolution matter
- a golden `suite.ktest` makes the assertions clearer

### Use an incremental step suite when:

- the issue is about incremental invalidation
- separate compilation artifacts matter
- interface emission or reuse behavior matters
- cache reuse or determinism is part of the obligation

## 10. Assertion Policy

Assertions should be as specific as possible without being brittle.

### Negative static tests

Default pattern:

- assert one error when possible
- assert the exact diagnostic code
- assert the location
- optionally assert a regex for a key word or phrase

Preferred directives:

- `assertErrorCount`
- `assertDiagnosticAt`
- `assertDiagnosticCodes`
- `assertDiagnosticMatch`

Message-text assertions are supplemental, not primary.

### Positive static tests

Default pattern:

- `assertNoErrors`
- optionally `assertType`
- optionally `assertDeclKinds`

### Runtime tests

Default pattern:

- `mode run`
- explicit backend, usually `interpreter` unless another backend is required
- explicit entry
- `assertNoErrors`
- `assertExitCode 0`
- exact `assertStdout` when stable

### Stage dump or trace tests

Use only when surface assertions are insufficient.

Examples:

- Chapter 17 checkpoints
- trace count obligations
- lowering shape that has no stable source-visible output

## 11. Fixture Naming Convention

Fixture roots should match the current repo style:

```text
<area>.<subarea>.<polarity>[_qualifier]
```

Examples already in use:

- `modules.imports.negative_missing_item`
- `expressions.implicit_parameters.positive_dependent_later_implicit`
- `expressions.match.runtime_positive_guard_fallthrough`
- `types.universes.quantities.qtt.negative_borrowed_parameter_closure_escape`

Rules:

- Do not put source repo names in fixture directory names.
- Do not put issue numbers in fixture directory names.
- Do encode the Kappa behavior clearly.
- Keep sibling fixtures under the same root family when they test the same spec area.

## 12. Fixture File Layout Rules

Use these default names:

- `main.kp` for the principal file
- additional `.kp` files for support modules
- `suite.ktest` when suite-level directives are cleaner than inline directives
- `incremental.ktest` only for incremental suites

Recommended provenance header for imported issues:

```kappa
-- Source: rust-lang/rust#8636
-- Spec: §5.1.7, §7.6
```

This is not required for every fixture, but it is strongly recommended for imported issue tests.

## 13. Methodical Workflow

Every batch should follow the same sequence.

### Step 1: Pick a narrow batch

Choose one bucket, one source artifact class, and one tranche of 5-10 records.

Good batch examples:

- Scala imports and layout
- trait and implicit-resolution direct fits
- borrowing and closure-capture issues
- handler and resumption issues
- query lowering issues
- FFI bridge-boundary issue titles from `zig` + `mojo`
- runtime cleanup PRs with upstream tests from `koka` + `effekt`

Bad batch examples:

- one parser issue, one borrowing issue, one macro issue, one query issue in the same pass

### Step 2: Normalize every candidate

Create the Issue Import Record fields before writing code, including `artifact_kind`, `artifact_path`, and
`corroborating_artifacts`.

### Step 3: Search current coverage

Mechanically search for:

- existing fixture roots in the same family
- existing diagnostic codes
- nearby positive or negative examples
- existing staged `new-tests/` roots
- existing `SPEC_ADDITIONS.md` notes that would make the candidate `hold-spec`

If a near-duplicate already exists, stop and decide whether the new issue truly adds coverage.

If the issue body is vague, search the repo-local `pulls.json` and harvested `tests/manifest.json` before accepting or
rejecting it. A PR or upstream regression test can turn an ambiguous issue into a precise Kappa analogue, but it must be
recorded separately in the ledger.

### Step 4: Decide acceptance state

Each candidate must be classified as:

- `accepted-now`
- `accepted-later`
- `hold-spec`
- `hold-impl`
- `rejected`

Do not leave the state implicit.

### Step 5: Write the Kappa adaptation note

For each `accepted-now` issue, record:

- the minimal original-language failure mode
- the Kappa analogue
- the exact spec sections
- the smallest useful fixture shape
- the expected observable

This is the point where we strip away source-language-specific syntax.

### Step 6: Create the failing fixture first

When the issue requires an implementation change:

- add the fixture
- assert the intended Kappa behavior
- verify that it fails for the right reason
- only then change compiler code

When the current compiler already behaves correctly and coverage is missing:

- add the fixture and verify it passes

### Step 7: Implement the minimum compiler change

Fix only the behavior needed for the fixture batch.
Avoid mixing unrelated cleanup into the same issue-import PR.

### Step 8: Verify the fixture family

Run the relevant tests for the fixture family, not just the new single case.

Examples:

- related fixture directory family
- affected harness tests
- any compiler unit tests or backend tests touched by the implementation

### Step 9: Update tracking state

Mark the imported issues as `imported` and note:

- final fixture root
- any merged or split source issues
- any residual follow-up issues discovered during import

## 14. Batch Order

The recommended pull order is:

### Wave 1: Immediate direct fits

Primary sources:

- `scala2`
- `scala3`
- `ghc`

Reason:

- highest spec clarity
- shortest path to new coverage
- best current sources for direct parser, layout, pattern, and typeclass-style fixture imports
- the existing Scala notes are still the densest backlog of direct spec-aligned parser, import, and implicit-resolution candidates

Operational note:

- Start with the existing Scala notes:
  - `SCALA_ISSUE_FIXTURE_CANDIDATES.md`
  - `SCALA_IMPLICIT_ISSUE_CANDIDATES.md`
- Treat `GHC_ISSUE_CANDIDATES.md` as the completed sibling note for this wave.
- Backfill exact Scala keyword / range logging before the next Scala tranche so Wave 1 uses the same tracking standard as later waves.

### Wave 2: Ownership and borrowing

Primary sources:

- `rust`
- `hylo`
- `granule`

Reason:

- these are some of Kappa's most distinctive semantics
- soundness bugs here are expensive if not locked down early

Operational note:

- Treat `OWNERSHIP_ISSUE_CANDIDATES.md` as the current source of truth for this wave.
- The first Wave 2 staged batch is now:
  - `new-tests/expressions.lambdas.negative_capture_prevents_earlier_last_use`
  - `new-tests/types.universes.quantities.qtt.negative_condition_consumes_before_branch_reuse`
  - `new-tests/types.universes.quantities.qtt.negative_match_on_erased_scrutinee`
- The real fixture suite already covers the disjoint-path, borrow-escape, erased-runtime-use, and basic linear-duplication anchors for this wave.
- Keep the Hylo `inout` / projection-lifetime slice explicitly parked until we decide whether it deserves a direct Kappa analogue or only note-level tracking.

### Wave 3: Effects, handlers, and resumptions

Primary sources:

- `koka`
- `effekt`
- `unison`

Reason:

- Kappa's control semantics are rich and easy to accidentally break in lowering

### Wave 4: Queries

Primary source:

- `links`

Reason:

- Kappa has strong planned query semantics, but current fixture coverage is still light

Operational note:

- This wave was pulled forward before the Kotlin corpus arrived locally and should stay active until the staged Links batch is either promoted or explicitly parked.

### Wave 5: Flow Typing, Safe Navigation, And Elvis

Primary source:

- `kotlin`

Reason:

- Kotlin issues are especially likely to expose direct Kappa obligations around `?.`, `?:`, branch-local refinement, and smart-cast-like narrowing.
- These fit Kappa well, but should be imported only after a dedicated Kotlin triage note exists so we do not mix true flow-typing imports with Kotlin-only OO noise.

Operational note:

- Keep this wave limited to `Option`-like safe-navigation, Elvis, and constructor-test refinement. Do not import JVM/platform-type or OO-method-resolution issues just because they mention nullability.

### Wave 6: Macros, hygiene, and staging

Primary sources:

- `lean4`
- `racket`

Reason:

- Kappa's `Syntax`, `$(...)`, and hygiene rules are already specified tightly enough to stage the first direct fits without inventing command-macro surface that the spec does not have

Operational note:

- Keep this wave limited to term/type splicing and module-boundary hygiene. Do not invent Lean/Racket-style declaration-macro, syntax-class, or parser-category surfaces just to reproduce source issues more literally.

### Wave 7: Totality and termination

Primary sources:

- `Idris2`
- `lean4`
- `agda`

Reason:

- high-value and spec-important
- many of these tests should land only when local recursion and totality support are ready enough to avoid dead backlog

### Wave 8: Coherence and solver edges

Primary sources:

- `chalk`
- additional Scala implicit cases

Reason:

- useful once the basic trait and instance suite is denser

Operational note:

- Keep the first solver tranche narrow: local supertrait projection first, then coherence negatives, then associated-member ambiguity / irrelevant-premise cases. Do not mix dyn-trait or trait-object issues into this wave; those are poor fits for current Kappa.

### Wave 9: FFI, Dynamic Values, And Bridge Boundaries

Primary sources:

- `zig`
- `mojo`
- `ocaml`
- `purescript`
- `gleam`
- `roc`
- `aeneas`
- `creusot`
- `noir`
- `circom`
- `leo`
- `cairo`
- `plutus`

Reason:

- Kappa now has explicit boundary-honesty, `std.ffi`, `std.bridge`, dynamic value, checked-boundary, and portable foreign
  ABI spec sections.
- These areas are too new to rely on incidental tests from earlier parser/type-system waves.

Operational note:

- Treat `FFI_RUNTIME_ISSUE_CANDIDATES.md` as the current source of truth for this wave.
- Start with issues and PRs; only use harvested upstream tests after the issue/PR makes the Kappa analogue clear.
- Classify source-specific ABI details as `reject` unless they map to §17.7 portable ABI, boundary contracts, callbacks,
  bridge-bound packages, runtime capabilities, or backend-profile portability.

### Wave 10: Runtime, Fibers, Interruption, And Resource Cleanup

Primary sources:

- `chapel`
- `ponyc`
- `encore`
- `inko`
- `elixir`
- `gleam`
- `rust`
- `unison`
- `koka`
- `effekt`

Reason:

- Kappa now specifies runtime-owned `IO`, fibers, interruption, masking, cleanup, STM, supervision, completion-aware
  sequencing, and portable runtime obligations.
- Runtime bugs often require positive run tests or trace tests rather than only typechecker fixtures.

Operational note:

- Treat `RUNTIME_CONCURRENCY_ISSUE_CANDIDATES.md` as the current source of truth for runtime-concurrency candidates.
- Prioritize observable cleanup, causality, interruption, scope, and resource behavior.
- Defer pure scheduler-performance issues unless a spec-observable fairness or safe-point rule is implicated.
- Do not count standard-module vocabulary smoke fixtures as semantic coverage. Prefer runnable or type-system-observable
  behavior tests for atomic cells, supervisor lifecycle, and fiber/interruption rules.

### Wave 10B: Runtime Hashing And Collection Acceleration

Primary sources:

- `inko`
- `chapel`
- later query/dataflow repos when they expose hash-accelerated `distinct`, grouping, or map/set behavior

Reason:

- Kappa now has an explicit `std.hash` surface with `HashSeed`, linear `HashState`, opaque `HashCode`, and `Hashable`
  as an acceleration trait rather than a semantic equality substitute.
- Upstream collection bugs often expose denotation-vs-performance obligations: collisions and seed changes must not alter
  `Map`, `Set`, `distinct`, or `group by` results.

Operational note:

- Treat `HASH_COLLECTION_ISSUE_CANDIDATES.md` as the current source of truth for this wave.
- Do not add `SPEC_ADDITIONS.md` entries for issues already covered by §2.7E.
- Classify hash algorithm choice and collision quality as backend/library conformance unless a source-visible denotation
  changes.

### Wave 11: Dependent Proof, Erasure, And Elaboration

Primary sources:

- `FStar`
- `rocq`
- `Arend`
- `cooltt`
- `cubical`
- `narya`
- `rzk`
- `smalltt`
- `1lab`
- `ATS-Postiats`
- `ATS-Xanadu`

Reason:

- The existing totality wave covers the first Lean / Idris / Agda tranche, but the refreshed corpus now has many more
  proof-oriented compilers with relevant issues and upstream tests.
- This wave should feed both `TOTALITY_ISSUE_CANDIDATES.md` and a new dependent-proof note for erasure, universes,
  equality transport, and elaboration-time evaluation.

Operational note:

- Create `DEPENDENT_PROOF_ISSUE_CANDIDATES.md` before broad mining.
- Keep proof-library-only failures out of the fixture stream.
- Prefer compiler-visible obligations: totality, coverage, hidden/erased parameters, universe constraints, definitional
  equality, proof irrelevance, and elaboration normalization.

### Wave 12: PR And Upstream-Test Backfill

Primary sources:

- all active Tier 0 notes
- any Tier 1 candidate note that accepts a source issue based on a PR fix or upstream regression test

Reason:

- The new corpus has PR archives and large harvested upstream test snapshots.
- Existing issue-only notes can often be made more precise by recording the actual fixing PR or regression test path.

Operational note:

- Do not globally grep tests and call the repo "read".
- For each accepted-now candidate, search for linked PRs and harvested regression tests, then record exact PR ids and
  test source paths in the candidate note.
- Use upstream tests as corroborating evidence; the Kappa fixture still belongs to the Kappa spec bucket.

## 15. Review Checklist For Each Imported Fixture

Before considering an imported issue done, check all of the following:

- The source issue has exact provenance recorded.
- The local corpus artifact path is recorded.
- Any corroborating PR, docs file, paper note, or upstream regression-test path is recorded.
- The fixture has exact `Spec.md` references in the tracking notes.
- The fixture has one primary behavior.
- The chosen harness form is the smallest one that fits.
- Assertions are stable and specific.
- Negative tests assert code and location, not just message text.
- The fixture is named in the existing repo style.
- Coverage search was done and duplicates were considered.
- If the fixture came from a source issue with multiple examples, the imported version is the minimal Kappa repro.
- If the fixture depends on a missing or unclear spec rule, no fixture was added and the gap went to `SPEC_ADDITIONS.md`.
- Related tests were run.
- Tracking status was updated to `imported`.

## 16. Definition Of Done

An issue is only fully imported when:

- it has a normalized Issue Import Record
- its issue/PR/test/docs artifact paths are recorded
- it has a final accepted fixture root
- the fixture exists in `tests/Kappa.Compiler.Tests/Fixtures`
- the fixture passes in the suite
- any required compiler change has landed
- provenance and status tracking are updated

Until then, it is still backlog, not completed work.

## 17. Immediate Operational Recommendation

Use this file as the process document, and keep the mined candidate notes as the content backlogs:

- `SCALA_ISSUE_FIXTURE_CANDIDATES.md`
- `SCALA_IMPLICIT_ISSUE_CANDIDATES.md`
- `GHC_ISSUE_CANDIDATES.md`
- `LINKS_ISSUE_CANDIDATES.md`
- `TOTALITY_ISSUE_CANDIDATES.md`
- `EFFECTS_ISSUE_CANDIDATES.md`
- `OWNERSHIP_ISSUE_CANDIDATES.md`
- `MACRO_ISSUE_CANDIDATES.md`
- `KOTLIN_ISSUE_CANDIDATES.md`
- `COHERENCE_ISSUE_CANDIDATES.md`
- `FFI_RUNTIME_ISSUE_CANDIDATES.md`
- `RUNTIME_CONCURRENCY_ISSUE_CANDIDATES.md`
- `HASH_COLLECTION_ISSUE_CANDIDATES.md`
- `SPEC_RELEVANT_ISSUE_CANDIDATES.md`
- future `DEPENDENT_PROOF_ISSUE_CANDIDATES.md`

In practice, each implementation wave should:

1. choose one bucket,
2. choose one exact artifact tranche,
3. promote a small set of records to `accepted-now`,
4. add or update fixtures,
5. implement only the necessary compiler work,
6. mark the imported items as done with issue/PR/test/docs provenance updated.

That is the smallest loop that stays organized and scales across all the mined repos.
