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
- [x] Stage Idris2 local-signature layout direct-fit test
- [x] Add Idris2 IEEE floating-constants portability note
- [x] Stage Idris2 range-driven list-comprehension direct-fit test
- [x] Stage Idris2 explicit prelude-filter direct-fit test
- [x] Stage Idris2 nested record-projection direct-fit test
- [x] Stage Idris2 multi-supertrait default-member direct-fit test
- [x] Stage Idris2 multi-parameter trait-instance ordering direct-fit test
- [x] Stage Idris2 lambda-body match-expression direct-fit test
- [x] Add Idris2 constructor-only pattern ergonomics note
- [x] Stage Idris2 mixed local-signature plus inferred-binding direct-fit test
- [x] Stage Idris2 higher-kinded trait-instance resolution direct-fit test
- [x] Stage Idris2 trait-member implicit-parameter direct-fit test
- [x] Stage Idris2 stronger-trait-to-helper-supertrait projection direct-fit test
- [x] Fold Idris2 indexed branch-reduction bug into the normalization spec-clarity note
- [x] Cross-link Idris2 quantity-sensitive function-type equality to existing real coverage
- [x] Stage Idris2 portable `Bytes` prelude-surface direct-fit test
- [x] Record sparse Idris2 `#100-#104` hashing and `--find-ipkg` background tranche
- [x] Stage Idris2 constructor-pattern shadowing direct-fit test
- [x] Stage Idris2 integer-literal case-discrimination direct-fit test
- [x] Stage Idris2 do-block final-expression alias direct-fit test
- [x] Add Idris2 single-constructor-wrapper specialization note
- [x] Record sparse Idris2 `#125-#129` bootstrap-build background tranche
- [x] Add Idris2 stack-safe eager-aggregate note
- [x] Record sparse Idris2 `#132-#136` aggregate/editor-tooling tranche
- [x] Record sparse Idris2 `#140-#144` provider-collision/runtime-library tranche
- [x] Stage Idris2 local `case impossible` coverage direct-fit test
- [x] Record sparse Idris2 `#163-#167` totality/bootstrap tranche
- [x] Add Idris2 indexed-hole-context tooling note
- [x] Add Idris2 unnamed function-binder ergonomics note
- [x] Fold Idris2 non-first-argument clause simplification into the normalization follow-up
- [x] Record sparse Idris2 `#173-#177` refinement/ergonomics tranche
- [x] Add Idris2 overloaded-alias numeric-defaulting note
- [x] Add Idris2 module-alias/type-name collision diagnostic note
- [x] Record sparse Idris2 `#179-#183` defaulting/diagnostic tranche
- [x] Add Idris2 generated-edit tooling conformance note
- [x] Stage Idris2 quantity-annotated function-components-in-tuple direct-fit test
- [x] Record Idris2 `#184-#188` tooling/syntax/tuple tranche
- [x] Stage Idris2 identity-preserves-linear-function-argument-quantity direct-fit test
- [x] Record sparse Idris2 `#189-#193` linearity tranche
- [x] Add Idris2 portable line-reading semantics note
- [x] Record sparse Idris2 `#194-#198` install/stdio tranche
- [x] Stage Idris2 indexed-missing-reachable-constructor exhaustiveness negative
- [x] Record sparse Idris2 `#199-#203` exhaustiveness tranche
- [x] Stage Idris2 irrefutable-tuple-destructure definitional-equality proof
- [x] Record sparse Idris2 `#204-#208` normalization tranche
- [x] Stage Idris2 equality-refinement-preserves-distinct-linear-binders direct-fit test
- [x] Add Idris2 indexed-impossible diagnostic-quality note
- [x] Add Idris2 chunked binary file-read portability note
- [x] Add Idris2 recursive-family equality-normalization note
- [x] Record sparse Idris2 `#209-#218` io/diagnostic/linearity tranche
- [x] Stage Idris2 implicit-alias-binder hygiene direct-fit test
- [x] Record sparse Idris2 `#221-#225` bootstrap/implicit-hygiene tranche
- [x] Add Idris2 bounded/streamed hole-search tooling note
- [x] Stage Idris2 alpha-renamed `if`-codomain definitional-equality proof
- [x] Record sparse Idris2 `#226-#230` hole-search/definitional-equality tranche
- [x] Add Idris2 declaration-head layout ergonomics note
- [x] Add Idris2 declaration-head sort-inference ergonomics note
- [x] Record sparse Idris2 `#231-#240` declaration-layout / sort-inference tranche
- [x] Stage Idris2 type-level `if` / `match` signature-normalization direct-fit test
- [x] Record sparse Idris2 `#241-#245` branch-normalization tranche
- [x] Add Idris2 residual transparent-family witness-accessibility note
- [x] Strengthen the staged mixed local-`let` fixture to cover sibling bindings around a signed local
- [x] Record sparse Idris2 `#251-#255` witness/local-`let` tranche
- [x] Add Idris2 unsupported-feature-harness-timeout note
- [x] Record sparse Idris2 `#256-#265` precedence/install/totality/harness tranche
- [x] Record sparse Idris2 `#266-#270` bootstrap/implicit-ambiguity tranche
- [x] Record sparse Idris2 `#271-#275` platform-build tranche
- [x] Record sparse Idris2 `#276-#297` source-root/module-header and tracker-template tranche
- [x] Record sparse Idris2 `#298-#302` totality/quantity tranche
- [x] Record sparse Idris2 `#303-#307` with-binder/diagnostic/unused-local tranche
- [x] Record sparse Idris2 `#308-#312` hole-goal/reflection tranche
- [x] Record sparse Idris2 `#313-#317` install-docs tranche
- [x] Add Idris2 helper-lowering quantity-preservation note
- [x] Stage Idris2 duplicate-constructor direct-fit negative
- [x] Record sparse Idris2 `#331-#340` linearity/duplicate-constructor tranche
- [x] Add Idris2 portable prelude-cardinality helper note
- [x] Record sparse Idris2 `#341-#350` absent/prelude tranche
- [x] Add Idris2 compile-time alias-binder quantity note
- [x] Record sparse Idris2 `#351-#360` quantity/totality tranche
- [x] Add Idris2 forward-only policy-directive note
- [x] Stage Idris2 trait-member-chain-from-result-type direct-fit test
- [x] Record sparse Idris2 `#361-#365` directive/implicit-resolution tranche
- [x] Stage Idris2 parenthesized-equality-operator direct-fit test
- [x] Add Idris2 dependent-signature index-constraint note
- [x] Record sparse Idris2 `#366-#375` parser/default-implicit/well-founded tranche
- [x] Record sparse Idris2 `#376-#380` parser/default-implicit/constraint tranche
- [x] Add Idris2 machine-protocol framing note for interactive EOF/interruption paths
- [x] Add Idris2 suspension-insertion timing note
- [x] Add Idris2 rewrite-rule binder/unification stability note
- [x] Stage Idris2 multi-linear indexed-singleton coverage direct-fit test
- [x] Record sparse Idris2 `#394-#405` protocol/laziness/transform/coverage tranche
- [x] Add Idris2 stable generated-helper naming note for hole-goal output
- [x] Stage Idris2 implicit-generalization-inside-type-level-match direct-fit test
- [x] Record sparse Idris2 `#406-#415` hole-display/implicit-generalization/build tranche
- [x] Cross-link Idris2 implicit-record-update repair coverage to existing real fixtures
- [x] Add Idris2 dependent-package coverage note
- [x] Record sparse Idris2 `#421-#430` implicit-record-update / package-coverage tranche
- [x] Add Idris2 clause-head linearity-parity note
- [x] Stage Idris2 parameter-name / field-selector direct-fit test
- [x] Fold Idris2 cross-module reflection issue into the existing semantic-reification note
- [x] Fold Idris2 JS fold-stack issue into the stack-safe aggregate note
- [x] Fold Idris2 missing-file crash into the deterministic tooling-failure note
- [x] Record sparse Idris2 `#440-#454` clause-head/name-resolution/reflection/backend tranche
- [x] Stage Idris2 record-update projection normalization direct-fit test
- [x] Cross-link Idris2 unsafe-totality checker divergence issue into the totality wave
- [x] Add Idris2 erased-parameter composition-helper ergonomics note
- [x] Record sparse Idris2 `#457-#461` record-update/totality/backend/quantity tranche
- [x] Add Idris2 doc-comment attachment consistency note
- [x] Add Idris2 clause-signature binder reuse ergonomics note
- [x] Record sparse Idris2 `#467-#471` documentation/compiler-internals/scope tranche
- [x] Add Idris2 basic comment-surface documentation note
- [x] Add Idris2 backend library-artifact generation note
- [x] Record sparse Idris2 `#472-#476` documentation/backend-output tranche
- [x] Stage Idris2 final-`case impossible` missing-coverage direct-fit negative
- [x] Add Idris2 exact-width signed integer semantics note
- [x] Record sparse Idris2 `#484-#488` coverage/primitives tranche
- [x] Add Idris2 Unicode identifier/runtime-text coherence note
- [x] Add Idris2 pattern-alias quantity-preservation note
- [x] Cross-link Idris2 `let`/`if` termination issue back to the totality wave
- [x] Record sparse Idris2 `#489-#493` unicode/quantity/termination tranche
- [x] Add Idris2 infix-pattern implicit-binder ergonomics note
- [x] Stage Idris2 local-instance non-leak direct-fit negative
- [x] Record sparse Idris2 `#494-#498` infix-pattern/local-instance tranche
- [x] Stage Idris2 erased-witness package-pattern direct-fit positive
- [x] Record sparse Idris2 `#499-#503` erased-witness tranche
- [x] Stage Idris2 same-spelling type/constructor direct-fit positive
- [x] Record sparse Idris2 `#504-#508` same-spelling/CLI tranche
- [x] Fold Idris2 reflection-unquote robustness into the existing reflection spec note
- [x] Record sparse Idris2 `#509-#513` reflection/generalization tranche
- [x] Stage Idris2 local-signature equality-type parser direct-fit positive
- [x] Fold Idris2 quote-linearity reflection robustness into the existing reflection spec note
- [x] Record sparse Idris2 `#514-#518` REPL/reflection/equality-signature tranche
- [x] Cross-link Idris2 positivity/totality soundness issue back to the totality wave
- [x] Fold Idris2 empty-unifier-message evidence into the existing diagnostic note
- [x] Record sparse Idris2 `#524-#528` positivity/unification tranche
- [x] Record sparse Idris2 `#529-#533` installation/parser-performance tranche
- [x] Add Idris2 proof-DSL soundness requirement note
- [x] Record sparse Idris2 `#534-#538` proof-soundness tranche
- [x] Fold Idris2 composition accessibility evidence into the existing composition note
- [x] Add Idris2 literate-source visibility semantics note
- [x] Record sparse Idris2 `#539-#543` composition/literate tranche
- [x] Add Idris2 recoverable-`Elab` automation note
- [x] Fold Idris2 exact-width cast coverage into the existing numeric note
- [x] Record sparse Idris2 `#544-#548` Elab/casts/FAQ tranche
- [x] Classify Idris2 exact-width FFI backend mismatch issue
- [x] Stage Idris2 nested-list-head coverage direct-fit positive
- [x] Cross-link Idris2 named constructor field-instantiation issue to existing coverage
- [x] Record sparse Idris2 `#549-#553` FFI/coverage/constructor-names tranche
- [x] Cross-link Idris2 string interpolation request to existing coverage
- [x] Fold Idris2 linear-arrow sugar into the unnamed-binder ergonomics note
- [x] Fold Idris2 forced-erased-`with` evidence into the helper-lowering linearity note
- [x] Record sparse Idris2 `#554-#558` interpolation/quantity/helper tranche
- [x] Fold Idris2 case-split duplicate-hole evidence into the generated-edit tooling note
- [x] Fold Idris2 lowered-helper name collision evidence into the stable-helper-name tooling note
- [x] Record sparse Idris2 `#564-#568` interactive/helper-name tranche
- [x] Stage Idris2 large-literal constructor-equality refinement direct-fit positive
- [x] Fold Idris2 erased-hole-application evidence into the hole-goal tooling note
- [x] Add Idris2 mutual declaration-group quantity invariance note
- [x] Fold Idris2 backend newline-retention issue into the line-reading semantics note
- [x] Add Idris2 local nested-type/data scoping note
- [x] Record sparse Idris2 `#574-#578` literal-normalization tranche
- [x] Record sparse Idris2 `#579-#583` hole/relevance tranche
- [x] Record sparse Idris2 `#584-#588` mutual/newline/nested-data tranche
- [x] Stage Idris2 dependent-package function-projection direct-fit positive
- [x] Classify Idris2 string-unpack slowness as performance background
- [x] Add Idris2 integer-exponentiation ergonomics note
- [x] Record sparse Idris2 `#589-#593` dependent-pair/performance/numeric tranche
- [x] Add Idris2 stack-safe string conversion note
- [x] Add Idris2 erased constructor-injectivity helper note
- [x] Record sparse Idris2 `#594-#598` backend-stack/proof-erasure tranche
- [x] Stage Idris2 float signed-zero raw-bit equality direct-fit negative
- [x] Classify Idris2 same-line export/interface modifier issue as syntax background
- [x] Classify Idris2 samples CI request as admin background
- [x] Record sparse Idris2 `#599-#603` modifier/admin/float tranche
- [x] Cross-link Idris2 same-spelling variable-capture report to existing binder-hygiene coverage
- [x] Add Idris2 normalization-before-implicit-search requirement note
- [x] Record sparse Idris2 `#604-#608` hygiene/search tranche
- [x] Add Idris2 scoped linear-builder requirement note
- [x] Record sparse Idris2 `#609-#613` linear-array-design tranche
- [x] Classify Idris2 qualified-`do` namespace issue as syntax background
- [x] Classify Idris2 empty-interface `where` issue as syntax background
- [x] Stage Idris2 top-level constant definitional-equality direct-fit positive
- [x] Add Idris2 telescopic `with`-abstraction requirement note
- [x] Add Idris2 floating excess-precision conformance note
- [x] Record sparse Idris2 `#619-#623` syntax/defeq/float tranche
- [x] Add Idris2 reflection-surface parity requirement note
- [x] Classify Idris2 record-syntax RFC as syntax background
- [x] Add Idris2 `Elab` declaration environment-stability note
- [x] Stage Idris2 constructor-field-is-not-record-update direct-fit negative
- [x] Record sparse Idris2 `#624-#628` reflection/record tranche
- [x] Fold Idris2 `enumFromTo` request into the portable range-surface note
- [x] Classify Idris2 namespace-migration ambiguity issue as name-resolution background
- [x] Add Idris2 function-pattern non-discrimination soundness note
- [x] Record sparse Idris2 `#629-#633` range/namespace/function-pattern tranche
- [x] Add Idris2 case-split constructor-identity tooling note
- [x] Classify Idris2 dependent-pair parenthesization issue as syntax background
- [x] Fold Idris2 `with` indentation inconsistency into the `with`-surface layout note
- [x] Record sparse Idris2 `#634-#638` case-split/layout tranche
- [x] Classify Idris2 multi-version installation request as tooling background
- [x] Classify Idris2 application/record-syntax fusion RFC as syntax background
- [x] Add Idris2 backend temporary-name hygiene note
- [x] Record sparse Idris2 `#639-#643` install/syntax/backend tranche
- [x] Cross-link Idris2 totality-checker slowness issue to the totality wave
- [x] Stage Idris2 higher-kinded constrained call inside `if`/`case` direct-fit positive
- [x] Record sparse Idris2 `#644-#648` totality/constraint-solve tranche
- [x] Classify Idris2 dot-pattern internal-error issue as syntax/elaboration background
- [x] Add Idris2 builtin-type-object reduction uniformity note
- [x] Add Idris2 reserved-name help-query note
- [x] Cross-link Idris2 nonterminating `with`-helper loop issue to the totality wave
- [x] Record sparse Idris2 `#649-#653` builtin/doc/with tranche
- [x] Cross-link Idris2 partial-with-under-total-default issue to the totality wave
- [x] Stage Idris2 default trait member uses associated static member direct-fit positive
- [x] Stage Idris2 trait member signature uses prior member in implicit binder direct-fit positive
- [x] Record sparse Idris2 `#654-#658` totality/trait-body tranche
- [x] Cross-link Idris2 false non-strict-positivity rejection issue to the totality wave
- [x] Stage Idris2 erased dependent-package witness match under forcing runtime field direct-fit positive
- [x] Record sparse Idris2 `#659-#663` positivity/erased-witness tranche
- [x] Fold Idris2 linear-composition hole issue into the composition-helper eta-equivalence note
- [x] Classify Idris2 explicit `SCHEME` bootstrap issue as installation background
- [x] Record sparse Idris2 `#664-#668` composition/install tranche
- [x] Cross-link Idris2 linear-function inference issue to existing higher-order linear identity coverage
- [x] Classify Idris2 ungated elaborator-script issue as extension-gating background
- [x] Record sparse Idris2 `#669-#673` linearity/elab-gating tranche
- [x] Fold Idris2 applicative pair/constructor mismatch into a future idiom-sugar equivalence note
- [x] Fold Idris2 `Refl` pattern-lambda bug into the anonymous pattern-lambda hygiene note
- [x] Record sparse Idris2 `#674-#678` applicative/pattern-lambda tranche
- [x] Fold Idris2 `-p` library order bug into package/import-order stability notes
- [x] Add Idris2 warning-policy note for residual holes and partial declarations
- [x] Record sparse Idris2 `#684-#688` package-order/warning tranche
- [x] Stage Idris2 local `match` binder shadows surrounding local definition direct-fit positive
- [x] Stage Idris2 local type helper reduces through later alias/signature direct-fit positive
- [x] Classify Idris2 JavaScript `Show Bool` issue as backend background
- [x] Record sparse Idris2 `#694-#698` shadowing/normalization/backend tranche
- [x] Record empty Idris2 `#699-#703` gap and advance the focused cursor to `#704-#708`
- [x] Add Idris2 suspicious top-level-shadowing warning-policy note
- [x] Classify Idris2 type-argument clause-match bug as current-Kappa background
- [x] Stage Idris2 paired transparent dependent-package shared-index direct-fit positive
- [x] Classify Idris2 lowercase-namespace regression as name-resolution background
- [x] Stage Idris2 `--}` line-comment parser direct-fit positive
- [x] Record sparse Idris2 `#704-#708` and `#709-#713` shadowing/dependent-package/comment tranches
- [x] Advance the focused Idris cursor to `#714-#718`
- [x] Stage Idris2 duplicate compile-time syntax in local quote helper direct-fit positive
- [x] Record sparse Idris2 `#714-#718` quotation/linearity tranche
- [x] Record empty Idris2 `#719-#722` gap
- [x] Fold Idris2 mutual-interface visibility bug into the declaration-group elaboration note
- [x] Classify Idris2 Nat optimization regression as backend/performance background
- [x] Cross-link Idris2 bogus explicit-implicit applications to existing negative coverage
- [x] Record sparse Idris2 `#723-#727` mutual/optimizer/implicit-application tranche
- [x] Advance the focused Idris cursor to `#728-#732`
- [x] Cross-link Idris2 bogus implicit-pattern hole issue to existing explicit-implicit negative coverage
- [x] Add Idris2 helper-lowered hole hygiene note
- [x] Stage Idris2 indexed witness constructor refines outer arguments direct-fit positive
- [x] Preserve Idris2 interface-descriptor pattern-match issue as future descriptor-identity background
- [x] Add Idris2 immutable-installation prefix split note
- [x] Record sparse Idris2 `#728-#732` and `#733-#737` witness/hole/install tranches
- [x] Advance the focused Idris cursor to `#738-#742`
- [x] Fold Idris2 local private hint leakage into the scoped proof-search hint note
- [x] Classify Idris2 llvm-mingw chmod issue as installation/runtime background
- [x] Record sparse Idris2 `#738-#742` hint/install tranche
- [x] Advance the focused Idris cursor to `#743-#747`
- [x] Stage Idris2 unrestricted-function-value versus linear-type direct-fit negative
- [x] Fold Idris2 mutual-block constructor scope bug into the declaration-group invariance note
- [x] Record sparse Idris2 `#743-#747` quantity/scope tranche
- [x] Advance the focused Idris cursor to `#748-#752`
- [x] Fold Idris2 post-`with` case-split regression into the future `with`-lowering note
- [x] Record sparse Idris2 `#748-#752` later-case-split tranche
- [x] Advance the focused Idris cursor to `#758-#762`
- [x] Cross-link Idris2 monadic linear-function bind issue to existing do-block linearity fixture
- [x] Fold Idris2 Unicode string display issue into the Unicode-rendering note
- [x] Add Idris2 forced-pattern saturation note
- [x] Record sparse Idris2 `#758-#762` monad/unicode/forced-pattern tranche
- [x] Advance the focused Idris cursor to `#763-#767`
- [x] Stage Idris2 bare overloaded-member passing as constrained polymorphic value direct-fit positive
- [x] Stage Idris2 do-bound `match` scrutinee inference direct-fit positive
- [x] Record sparse Idris2 `#763-#767` constrained-member/do-scrutinee tranche
- [x] Advance the focused Idris cursor to `#768-#772`
- [x] Fold Idris2 IDE protocol rework into the structured tooling-protocol note
- [x] Add Idris2 semantic-highlighting consistency note
- [x] Classify Idris2 websocket client issue as library/network background
- [x] Record sparse Idris2 `#768-#772` tooling/highlighting/network tranche
- [x] Advance the focused Idris cursor to `#773-#777`
- [x] Fold Idris2 wrong-target AddClause output into the generated-edit tooling note
- [x] Add Idris2 signed-integer to `Nat` runtime-conversion note
- [x] Add Idris2 future rewrite-after-normalization note
- [x] Add Idris2 telescopic/evidence-binder trait-header note
- [x] Record sparse Idris2 `#773-#777` tooling/numeric/rewrite/trait-header tranche
- [x] Advance the focused Idris cursor to `#778-#782`
- [x] Strengthen Idris2 certified-wrapper trait-header note with proof-hypothesis normalization evidence
- [x] Add Idris2 cross-module constructor-identity pattern-resolution note
- [x] Record sparse Idris2 `#778-#782` certified-wrapper/pattern-resolution tranche
- [x] Advance the focused Idris cursor to `#783-#787`
- [x] Add Idris2 instance-resolution repeated-subgoal performance note
- [x] Add Idris2 declaration-diagnostic source-range precision note
- [x] Strengthen Idris2 literate-source note with interactive tooling alignment evidence
- [x] Record sparse Idris2 `#783-#787` performance/diagnostic/literate tranche
- [x] Advance the focused Idris cursor to `#788-#792`
- [x] Strengthen Idris2 literate-source note with marker-family semantics evidence
- [x] Add Idris2 interactive-evaluation robustness note
- [x] Strengthen Idris2 hidden-structure mismatch diagnostic note with implicit-binder evidence
- [x] Record sparse Idris2 `#788-#792` literate/repl/diagnostic tranche
- [x] Advance the focused Idris cursor to `#793-#797`
- [x] Cross-link Idris2 view/append false-positive issue into the totality wave
- [x] Add Idris2 FFI type-argument erasure note
- [x] Add Idris2 case-ordering codegen stability note
- [x] Record sparse Idris2 `#793-#797` totality/FFI/backend tranche
- [x] Advance the focused Idris cursor to `#798-#802`
- [x] Add Idris2 block-local dependent-helper codomain-solving note
- [x] Add Idris2 future foreign-descriptor helper-import note
- [x] Classify Idris2 JS `Int`/`Integer` representation issue as backend-only background
- [x] Stage Idris2 typed-`Array Double` numeric-literal contextual-inference test
- [x] Record sparse Idris2 `#798-#802` local/FFI/numeric/backend tranche
- [x] Advance the focused Idris cursor to `#803-#807`
- [x] Switch the active mining cursor from Idris2 to Lean 4
- [x] Read Lean 4 `#249` and preserve it as a scoped macro/notation activation note
- [x] Advance the Lean 4 macro cursor from `#249` to the next unread exact-id set
- [x] Read Lean 4 `#99`, `#181`, and `#391`
- [x] Fold Lean 4 local-parser activation into the scoped macro/notation activation note
- [x] Add a Lean 4 quotation-robustness note and classify antiquotation optimization as background
- [x] Read Lean 4 `#174`, `#180`, and `#182`
- [x] Add Lean 4 optional-syntax-view, macro-linting, and generated-syntax diagnostic-rendering notes
- [x] Read Lean 4 `#184`, `#317`, and `#447`
- [x] Fold Lean 4 pretty-printer breakage into the quoted-syntax round-trip note, add macro tracing observability, and classify `declare_syntax_cat` panic as parser-category background
- [x] Advance the Lean 4 macro cursor to `#446`, `#465`, and `#485`
- [x] Read Lean 4 `#446`, `#465`, and `#485`
- [x] Fold Lean 4 parenthesizer/formatter failures into the generated-syntax rendering note and add local-head custom-delaboration guidance
- [x] Advance the Lean 4 macro cursor to `#451`, `#452`, and `#494`
- [x] Read Lean 4 `#451`, `#452`, and `#494`
- [x] Add Lean 4 layout-integration notes for future parser customizations and fold quote-depth pretty-printing into the extension-rendering note
- [x] Advance the Lean 4 macro cursor to `#308`, `#362`, and `#375`
- [x] Read Lean 4 `#308`, `#362`, and `#375`
- [x] Add Lean 4 syntax-abbreviation equivalence, partial-syntax semantic-service progress, and source-like signature-rendering notes
- [x] Advance the Lean 4 macro cursor to `#111`, `#191`, and `#242`
- [x] Read Lean 4 `#111`, `#191`, and `#242`
- [x] Fold Lean 4 built-in-token macro rejection into definition-site validation, add future `where`-scope guidance, and classify default-parameter explicitness as frontend/background for this wave
- [x] Advance the Lean 4 macro cursor to `#233`, `#240`, and `#529`
- [x] Read Lean 4 `#233`, `#240`, and `#529`
- [x] Fold Lean 4 local `open ... in ...` parser scope into the scoped activation note and classify docs/editor-client issues as background
- [x] Advance the Lean 4 macro cursor to `#312`, `#366`, and `#453`
- [x] Read Lean 4 `#312`, `#366`, and `#453`
- [x] Fold Lean 4 local option scoping into the policy-directive note, preserve dangling elaborator/syntax drift as an extension-surface consistency note, and classify unstructured induction naming as tactic/background for this wave
- [x] Advance the Lean 4 macro cursor to `#177`, `#190`, and `#255`
- [x] Read Lean 4 rendering/notation tranche `#177`, `#190`, `#192`, `#193`, `#198`, and `#255`
- [x] Fold Lean 4 matcher/universe/bvar/anonymous-binder rendering issues into the existing rendering notes, preserve local-notation lexical capture as a syntax-alias note, and classify C++ pretty-printer reuse as implementation background
- [x] Advance the Lean 4 macro cursor to `#239`, `#247`, and `#250`
- [x] Read Lean 4 semantic-query/diagnostic/interface tranche `#239`, `#247`, and `#250`
- [x] Add Lean 4 local-variable hover baseline, desugaring-aware monadic-lifting diagnostic, and recursive-group interface-metadata notes
- [x] Advance the Lean 4 macro cursor to `#2`, `#219`, and `#243`
- [x] Read Lean 4 placeholder/delaboration/shadowing tranche `#2`, `#219`, and `#243`
- [x] Fold Lean 4 placeholder-context provenance into diagnostic hygiene, partially applied matcher delaboration into rendering fallback, and global-shadowing confusion into the shadowing-warning note
- [x] Advance the Lean 4 macro cursor to `#29`, `#30`, and `#67`
- [x] Read Lean 4 lowering/module-identity/trace-failure tranche `#29`, `#30`, and `#67`
- [x] Fold Lean 4 equation-lowering misdiagnostics into desugaring-aware diagnostics, preserve module-name case drift as an artifact-identity note, and preserve panic-time trace flushing as a tooling failure-path note
- [x] Advance the Lean 4 macro cursor to `#196`, `#217`, and `#220`
- [x] Read Lean 4 composite-placeholder-alias-rendering tranche `#196`, `#217`, and `#220`
- [x] Add Lean 4 multi-location composite diagnostics, source-like placeholder-type rendering, and as-pattern delaboration notes
- [x] Advance the Lean 4 macro cursor to `#333`, `#335`, and `#346`
- [x] Read Lean 4 hidden-structure/field-resolution tranche `#333`, `#335`, and `#346`
- [x] Fold Lean 4 identical-type, implicit-binder, and invalid-field-notation misreports into the hidden-structure diagnostic note
- [x] Advance the Lean 4 macro cursor to `#337`, `#348`, and `#350`
- [x] Read Lean 4 blocked-unification / parser-repair / term-vs-tactic tranche `#337`, `#348`, and `#350`
- [x] Fold Lean 4 duplicated blocked-unification reporting into the hidden-structure and duplicate-diagnostic notes, fold the parenthesis-token misparse into the parser-repair note, and preserve trivial-wrapper elaboration equivalence as a future requirement
- [x] Advance the Lean 4 macro cursor to `#351`, `#352`, and `#355`
- [x] Read Lean 4 default-instance / universe-instance / dependent-instance-search tranche `#351`, `#352`, and `#355`
- [x] Fold Lean 4 field-notation-before-default-instance and universe-instance failures into the field-resolution / hidden-structure notes, and preserve solved-prerequisite propagation in instance search as a future requirement
- [x] Advance the Lean 4 macro cursor to `#343`, `#353`, and `#365`
- [x] Switch Idris poor-fit policy to feature/tooling-requirement-first triage
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
- [x] Kotlin contextual lambda receiver-body inference fixture
- [x] Kotlin labeled-tuple and assignment-condition classification update
- [x] Kotlin refreshed repo-local tranche `KT-678`-`KT-682` classification update
- [x] Kotlin absent-corpus tranche `KT-683`-`KT-687` update
- [x] Kotlin absent-corpus tranche `KT-688`-`KT-692` update
- [x] Kotlin absent-corpus tranche `KT-693`-`KT-697` update
- [x] Kotlin absent-corpus tranche `KT-698`-`KT-702` update
- [x] Kotlin absent-corpus tranche `KT-703`-`KT-707` update
- [x] Kotlin absent-corpus tranche `KT-708`-`KT-712` update
- [x] Kotlin absent-corpus tranche `KT-713`-`KT-717` update
- [x] Kotlin absent-corpus tranche `KT-718`-`KT-722` update
- [x] Kotlin absent-corpus tranche `KT-723`-`KT-727` update
- [x] Kotlin absent-corpus tranche `KT-728`-`KT-732` update
- [x] Kotlin absent-corpus tranche `KT-733`-`KT-737` update
- [x] Kotlin absent-corpus tranche `KT-738`-`KT-742` update
- [x] Kotlin absent-corpus tranche `KT-743`-`KT-747` update
- [x] Kotlin refreshed repo-local YouTrack correction for `KT-748`-`KT-757`
- [x] Kotlin refreshed repo-local tranche `KT-758`-`KT-762` update
- [x] Kotlin refreshed repo-local tranche `KT-763`-`KT-767` update
- [x] Kotlin refreshed repo-local tranche `KT-768`-`KT-770` update
- [x] Kotlin refreshed repo-local tranche `KT-771`-`KT-775` update
- [x] Kotlin refreshed repo-local tranche `KT-776`-`KT-780` update
- [x] Kotlin refreshed repo-local tranche `KT-781`-`KT-785` update
- [x] Kotlin refreshed repo-local tranche `KT-786`-`KT-790` update
- [x] Kotlin refreshed repo-local tranche `KT-791`-`KT-795` update
- [x] Kotlin refreshed repo-local tranche `KT-796`-`KT-800` update
- [x] Kotlin refreshed repo-local tranche `KT-801`-`KT-805` update
- [x] Kotlin refreshed repo-local tranche `KT-806`-`KT-810` update
- [x] Kotlin refreshed repo-local tranche `KT-811`-`KT-815` update
- [x] Kotlin refreshed repo-local tranche `KT-816`-`KT-820` update
- [x] Kotlin refreshed repo-local tranche `KT-821`-`KT-825` update
- [x] Kotlin refreshed repo-local tranche `KT-826`-`KT-830` update
- [x] Kotlin refreshed repo-local tranche `KT-831`-`KT-835` update
- [x] Kotlin refreshed repo-local tranche `KT-836`-`KT-840` update
- [x] Kotlin refreshed repo-local tranche `KT-841`-`KT-845` update
- [x] Kotlin refreshed repo-local tranche `KT-846`-`KT-850` update
- [x] Kotlin refreshed repo-local tranche `KT-851`-`KT-855` update
- [x] Kotlin refreshed repo-local tranche `KT-856`-`KT-860` update
- [x] Kotlin refreshed repo-local tranche `KT-861`-`KT-865` update
- [x] Kotlin refreshed repo-local tranche `KT-866`-`KT-870` update
- [x] Kotlin refreshed repo-local tranche `KT-871`-`KT-875` update
- [x] Kotlin refreshed repo-local tranche `KT-876`-`KT-880` update
- [x] Kotlin refreshed repo-local tranche `KT-881`-`KT-885` update
- [x] Kotlin refreshed repo-local tranche `KT-886`-`KT-890` update
- [x] Kotlin refreshed repo-local tranche `KT-891`-`KT-895` update
- [x] Kotlin refreshed repo-local tranche `KT-896`-`KT-900` update
- [x] Kotlin refreshed repo-local tranche `KT-901`-`KT-905` update
- [x] Kotlin refreshed repo-local tranche `KT-911`-`KT-915` update
- [x] Kotlin refreshed repo-local tranche `KT-916`-`KT-920` update
- [x] Kotlin refreshed repo-local tranche `KT-921`-`KT-925` update
- [x] Kotlin refreshed repo-local tranche `KT-926`-`KT-930` update
- [x] Kotlin refreshed repo-local tranche `KT-931`-`KT-935` update
- [x] Kotlin refreshed repo-local tranche `KT-936`-`KT-940` update
- [x] Kotlin refreshed repo-local tranche `KT-941`-`KT-945` update
- [x] Kotlin refreshed repo-local tranche `KT-946`-`KT-950` update
- [x] Kotlin refreshed repo-local tranche `KT-951`-`KT-955` update
- [x] Kotlin refreshed repo-local tranche `KT-956`-`KT-960` update
- [x] Kotlin refreshed repo-local tranche `KT-961`-`KT-965` update
- [x] Kotlin refreshed repo-local tranche `KT-966`-`KT-970` update
- [x] Kotlin refreshed repo-local tranche `KT-971`-`KT-975` update
- [x] Kotlin refreshed repo-local tranche `KT-976`-`KT-980` update
- [x] Kotlin refreshed repo-local tranche `KT-981`-`KT-985` update
- [x] Kotlin refreshed repo-local tranche `KT-986`-`KT-990` update
- [x] Kotlin refreshed repo-local tranche `KT-991`-`KT-995` update
- [x] Kotlin refreshed repo-local tranche `KT-996`-`KT-1000` update
- [x] Kotlin refreshed repo-local tranche `KT-1001`-`KT-1005` update
- [x] Kotlin refreshed repo-local tranche `KT-1006`-`KT-1010` update
- [x] Kotlin refreshed repo-local tranche `KT-1011`-`KT-1015` update
- [x] Kotlin refreshed repo-local tranche `KT-1016`-`KT-1020` update
- [x] Kotlin refreshed repo-local tranche `KT-1021`-`KT-1025` update
- [x] Kotlin refreshed repo-local tranche `KT-1026`-`KT-1030` update
- [x] Kotlin refreshed repo-local tranche `KT-1031`-`KT-1035` update
- [x] Kotlin refreshed repo-local tranche `KT-1036`-`KT-1040` update
- [x] Kotlin refreshed repo-local tranche `KT-1041`-`KT-1045` update
- [x] Kotlin refreshed repo-local tranche `KT-1046`-`KT-1050` update
- [x] Kotlin refreshed repo-local tranche `KT-1051`-`KT-1055` update
- [x] Compare Kotlin absent-labeled set against refreshed repo-local YouTrack corpus
- [x] Stage Kotlin function-typed receiver-method fixture from `KT-1030`
- [x] Stage Kotlin local-function-returned-as-value fixture from `KT-1037`
- [x] Re-read stale Kotlin false-absent range `KT-678`-`KT-686`
- [x] Stage Kotlin singleton term-import sugar fixture from `KT-682`
- [x] Stage Kotlin safe-navigation single-evaluation fixture from `KT-962`
- [x] Re-read stale Kotlin false-absent range `KT-689`-`KT-695`
- [x] Re-read stale Kotlin false-absent range `KT-698`-`KT-702`
- [x] Re-read stale Kotlin false-absent range `KT-703`-`KT-707`
- [x] Re-read stale Kotlin false-absent range `KT-708`-`KT-712`
- [x] Re-read stale Kotlin false-absent range `KT-713`-`KT-717`
- [x] Re-read stale Kotlin false-absent ranges `KT-718`-`KT-725`, `KT-727`-`KT-732`, and `KT-734`-`KT-747`
- [x] Stage Kotlin tuple-return/destructure fixture from `KT-730`
- [x] Kotlin type-constructor term-facet coverage mapping update
- [x] Kotlin loop-body/layout and overload-specificity background classification update
- [x] Kotlin contextual lambda plus receiver-body inference fixture
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
- [x] Confirm `kotlin` has repo-local docs, test artifacts, and a refreshed repo-local `issues.json`; keep the legacy top-level snapshot only as historical fallback.
- [x] Reopen `ghc` and classify a second five-issue direct-fit/background tranche.
- [x] Record GHC alias-expansion and constructor-evidence spec-follow-up notes in `SPEC_ADDITIONS.md`.
- [x] Record GHC associated-type record-update spec-follow-up note in `SPEC_ADDITIONS.md`.
- [x] Fold additional GHC alias-expansion application evidence into `SPEC_ADDITIONS.md` from `#981`.
- [x] Stage the GHC trailing-implicit partial-application fixture from `#1445`.
- [x] Record GHC associated-member instance-search normalization note in `SPEC_ADDITIONS.md`.
- [x] Stage the GHC normalized-associated-goal instance-search fixture from `#1834` / `#3038`.
- [x] Stage the GHC invalid-record-update negative fixture from `#2141`.
- [x] Stage the GHC imported-trait-alias instance fixture from `#2435` / `#5852`.
- [x] Stage the GHC associated-member-typed implicit-argument fixture from `#5120`.
- [x] Record GHC optional-implicit / empty-record-update / default-associated-member follow-up notes in `SPEC_ADDITIONS.md`.
- [x] Fold GHC scoped-associated-default and dynamic-placeholder evidence into existing `SPEC_ADDITIONS.md` notes.
- [x] Stage the GHC premise-supertrait-satisfies-instance-superclass fixture from `#26714`.
- [x] Stage the GHC alias-premise-satisfies-instance-superclass fixture from `#10335`.
- [x] Fold `GHC#17104` into the staged lexical implicit-shadowing fixture.
- [x] Record the GHC type-changing / existential record-update follow-up note from `#10808`, `#10856`, `#16501`, and `#18802`.
- [x] Record the GHC grouped-constraint-alias follow-up note from `#10362` and `#11278`.
- [x] Fold `GHC#12507` into the staged richer-local-implicit-context coverage.
- [x] Fold `GHC#18311` into the type-changing record-update follow-up note.
- [x] Fold `GHC#11156` and `GHC#18809` into the type-changing/desugared record-update follow-up note.
- [x] Classify the later GHC record-update diagnostic/name-resolution tranche `#7989`, `#9023`, `#18650`, `#18999`, and `#19084`.
- [x] Record the GHC diagnostic-hygiene follow-up note from `#19972`.
- [x] Classify the later GHC field-resolution / irrefutability / backend tranche `#11343`, `#12190`, `#16839`, `#17647`, `#19088`, and `#19463`.
- [x] Record the GHC associated-member normalization diagnostic follow-up note from `#11115`.
- [x] Fold `GHC#10361`, `#10817`, `#10899`, and `#11136` into the existing default-associated-static-member follow-up note.
- [x] Classify `GHC#10132` as kind-inference background and `GHC#11451` as binder-validation/warning-policy follow-up.
- [x] Record the GHC associated-member instance-head substitution/validation follow-up note from `#10815` and `#11450`.
- [x] Classify the nearby GHC associated-type tranche `#10020`, `#10811`, and `#11534`.
- [x] Record the later GHC associated-member normalization/validation follow-up note from `#13398` and `#13773`.
- [x] Record the GHC deriving-with-associated-members follow-up note from `#13404`.
- [x] Fold `GHC#13971` into the default-associated-static-member follow-up note and `GHC#13972` into the hidden-structure diagnostic note.
- [x] Fold `GHC#14094` into the default-associated-static-member follow-up note for auto-generated missing-member warnings.
- [x] Fold `GHC#14132` into the associated-member normalization diagnostic note and `GHC#14230` into the hidden-structure diagnostic note.
- [x] Fold `GHC#14462` into the deriving-with-associated-members follow-up note and classify `GHC#14046` as a fundep-specific poor fit.
- [x] Fold `GHC#14661`, `#14728`, `#14916`, and `#15052` into the deriving-with-associated-members / derive-admissibility follow-up note.
- [x] Classify `GHC#15711` as kind-inference background.
- [x] Record the possible GHC `derive via` / coercion-based deriving follow-up note from `#15178`, `#15376`, `#15434`, and `#15831`.
- [x] Fold `GHC#15191` into the deriving-admissibility note as evidence that derive sugar should match the corresponding explicit instance path.
- [x] Fold `GHC#15868` and `#16958` into the broader deriving-premise-inference follow-up note.
- [x] Fold `GHC#16362` and `#16641` into the explicit `derive via` follow-up note.
- [x] Classify `GHC#15969` as implementation/performance background rather than a new surface obligation.
- [x] Fold `GHC#16179` into the derivation-strategy warning follow-up note and `GHC#16341` into the indexed-deriving follow-up note.
- [x] Classify `GHC#16181`, `#16194`, and `#16322` as implementation/panic-or-soundness background.
- [x] Fold `GHC#15839` and `#16655` into the derivation-strategy warning follow-up note.
- [x] Fold `GHC#16714` into the broader role-sensitive deriving follow-up note and `GHC#16923` into the `derive via` validation note.
- [x] Classify `GHC#15932` as tooling/coverage background and `GHC#16578` as implementation/performance background.
- [x] Fold `GHC#17013` into the broader deriving-admissibility note as evidence that generated instances must obey the same explicit-instance determinism checks.
- [x] Fold `GHC#17014`, `#17183`, and `#17312` into the derivation-strategy / `derive via` syntax-and-warning follow-up note.
- [x] Record `GHC#17210` as a future projection-based derive-customization ergonomics note.
- [x] Fold `GHC#17767`, `#18047`, and `#18130` into the `derive via` constraint-and-syntax follow-up note.
- [x] Classify `GHC#17880` as implementation/soundness background and record `GHC#17899` as a generated-derived-member diagnostic follow-up.
- [x] Record the synthesized-warning-hygiene follow-up note from `GHC#18148` and `#18165`.
- [x] Fold `GHC#18219`, `#18258`, and `#18271` into the `derive via` context-inference / empty-clause / standalone-binder follow-up note.
- [x] Classify `GHC#18321` as implementation/pipeline background and `GHC#18388` as macro/template roundtrip background.
- [x] Fold `GHC#18474` into the indexed-deriving follow-up note and `GHC#18483` / `#18488` into the `derive via` unreduced-type / kind-propagation follow-up note.
- [x] Record the missed later GHC derive tranche `#18820`, `#18874`, `#18914`, `#19079`, and `#19141`.
- [x] Fold `GHC#18874` into the grouped standalone-deriving ergonomics note, `#18914` / `#19079` into the `derive via` scope-and-representation note, and `#19141` into the indexed/data-family deriving follow-up note.
- [x] Record the later GHC warning/strategy tranche `#19418`, `#19692`, `#19865`, `#20054`, and `#20223`.
- [x] Fold `GHC#19418` into the broader future-deriving premise-inference note, `#19692` into the derive-diagnostic suggestion note, `#19865` / `#20054` into synthesized-warning hygiene, and `#20223` into the `derive via` admissible-trait-shape note.
- [x] Record the refreshed later GHC keyword slices from `ghc-issues.json`: `derive|DerivingVia|Standalone deriving|GeneralizedNewtypeDeriving|Derive` over `18800..19150`, then `functional dependency|DerivingStrategies|record|constraint|derive|instance|GADT|kind|projection` over `19142..20550`.
- [x] Record the later GHC derive tranche `#20314`, `#20375`, `#20387`, `#20501`, and `#20524`.
- [x] Fold `GHC#20314` into the broader traversal/bifunctor deriving note, `#20375` / `#20387` into the unlifted/runtime-representation deriving admissibility bucket, `#20501` into the deriving-context-generality note, and `#20524` into the `derive via` extension-sensitivity note.
- [x] Record the focused later GHC keyword slice from `ghc-issues.json`: `deriv|Deriv|derive|constraint|instance|GADT|newtype|Coercible|Generic|quantified` over `20300..20540`.
- [x] Record the later GHC overlap/visibility tranche `#20465`, `#20466`, `#20527`, `#20529`, and `#20538`.
- [x] Fold `GHC#20465` / `#20466` into the hidden-structure overlap-diagnostic note, classify `GHC#20527` as kinding/runtime-representation background and `#20529` as import/interface background, and fold `GHC#20538` into the `derive via` standalone-vs-inline elaboration-equivalence note.
- [x] Record the focused later GHC keyword slice from `ghc-issues.json`: `overlap|overlapping|poly-kinded|type family|visibility|component|QuantifiedConstraints|DerivingStrategies|GeneralizedNewtypeDeriving` over `20460..20540`.
- [x] Record the later GHC diagnostic tranche `#20542`, `#20584`, `#20595`, `#20602`, and `#20627`.
- [x] Fold `GHC#20542` into the hidden-structure/specificity overlap-diagnostic note, classify `GHC#20584` as kind-defaulting background, and record `GHC#20595`, `#20602`, and `#20627` as exhaustiveness-detail, redundancy-blame, and built-in-name pretty-printing diagnostic follow-ups.
- [x] Record the focused later GHC keyword slice from `ghc-issues.json`: `overlap|kind variable|defaulting|view pattern|exhaustive|redundant constraint|Type kind|scope` over `20540..20630`.
- [x] Record the later GHC match/implicit tranche `#20630`, `#20631`, `#20633`, `#20642`, and `#20643`.
- [x] Classify `GHC#20630` as lexical-implicit/desugaring background and `#20631` as impossible-branch checker background, fold `GHC#20633` and `#20642` into the match-diagnostic quality notes, and record `GHC#20643` as a fine-grained warning-control ergonomics follow-up.
- [x] Record the focused later GHC keyword slice from `ghc-issues.json`: `ImplicitParameters|ApplicativeDo|UnliftedDatatypes|redundant pattern|complete-uni-pattern|coverage checker` over `20630..20643`.
- [x] Record the later GHC defaulting/interface tranche `#20654`, `#20661`, `#20666`, `#20675`, and `#20686`.
- [x] Classify `GHC#20654` as parser/syntax background, `#20661` as import/interface background, `#20666` as superclass-checking/soundness background, and fold `GHC#20675` / `#20686` into the explicit kind/representation defaulting-policy note.
- [x] Record the focused later GHC keyword slice from `ghc-issues.json`: `higher rank implicit|hs-boot|SOURCE import|superclass|uninferrable type variables|defaulting|RuntimeRep|PolyKinds` over `20654..20686`.
- [x] Record the later GHC derive/template/match tranche `#20688`, `#20691`, `#20696`, `#20703`, and `#20719`.
- [x] Fold `GHC#20688` into synthesized-warning hygiene, classify `GHC#20691` as optimization-warning background and `#20696` as tooling/build-graph background, fold `GHC#20703` into the inaccessible-rhs / redundant-branch diagnostic note, and fold `GHC#20719` into the future deriving note for alias-expanded higher-rank default-member signatures.
- [x] Record the focused later GHC keyword slice from `ghc-issues.json`: `implicit-lift|specialisation warning|TemplateHaskellOrQQ|COMPLETE pragma|inaccessible rhs|default signature|rank-N deriving` over `20688..20719`.
- [x] Record the later GHC extension/tooling tranche `#20723`, `#20737`, `#20762`, `#20808`, and `#20818`.
- [x] Classify `GHC#20723` as parser/syntax background, `#20737` as local-generalization/equality-given background, fold `GHC#20762` into the extension-provenance diagnostic note and `GHC#20808` into the implicit-hole-fit tooling note, and classify `GHC#20818` as implicit-parameter/subsumption background.
- [x] Record the focused later GHC keyword slice from `ghc-issues.json`: `OverloadedRecordDot|MonoLocalBinds|equality constraint|extension enabled|hole fit|implicit parameters|simplified subsumption` over `20720..20818`.
- [x] Record the later GHC coercion/diagnostic tranche `#20815`, `#20821`, `#20835`, `#20860`, and `#20873`.
- [x] Fold `GHC#20815` / `#20821` into the coercion-based deriving note, record `GHC#20835` as an explicit unsatisfiable-constraint follow-up, fold `GHC#20860` into the unresolved-name diagnostic note, and classify `GHC#20873` as kind-syntax/parser-policy background.
- [x] Record the focused later GHC keyword slice from `ghc-issues.json`: `coerce-derived dictionaries|coerce function|Unsatisfiable|TypeError|out-of-scope identifier|kind signature alias|DataKinds` over `20815..20873`.
- [x] Record the later GHC tooling/pretty-print tranche `#20883`, `#20893`, `#20895`, `#20902`, and `#20910`.
- [x] Fold `GHC#20883` into the interactive-vs-batch warning-consistency note, `#20893` into the quoted-syntax round-trip note, `#20895` into the plugin-origin provenance note, `#20902` into the built-in-name namespace-hygiene note, and `#20910` into the semantic-query kind/runtime-representation rendering note.
- [x] Record the focused later GHC keyword slice from `ghc-issues.json`: `GHCi warning parity|TH pretty-printer|newWanted source location|mkName namespace|levity-polymorphic primops` over `20883..20910`.
- [x] Record the later GHC kind/inference tranche `#20916`, `#20921`, `#20926`, `#20939`, and `#20942`.
- [x] Fold `GHC#20916` / `#20926` into the scoped kind/type binder naming note, classify `GHC#20921` as partial-signature/ambiguity-checking background, `#20939` as implicit-parameter/quantified-polymorphism background, and `#20942` as inferred-context/instance-environment background.
- [x] Record the focused later GHC keyword slice from `ghc-issues.json`: `same kind variable|partial type signature ambiguity|SAK inference|polymorphic implicit parameters|inferred context depends on instances` over `20916..20942`.
- [x] Record the later GHC diagnostics/tooling tranche `#20962`, `#20974`, `#20981`, `#21006`, and `#21012`.
- [x] Fold `GHC#20962` / `#21006` into the hidden-structure diagnostic note, `GHC#20974` into the semantic-query rendering note, `GHC#20981` into the plugin/tooling provenance note, and `GHC#21012` into the standard fix-it / repair-suggestion note.
- [x] Record the focused later GHC keyword slice from `ghc-issues.json`: `DisambiguateRecordFields|type families ugly printing|constraints lost by accident|Kind equality error|replacing -> with =>` over `20962..21012`.
- [x] Record the later GHC diagnostics/tooling tranche `#21030`, `#21031`, `#21063`, `#21086`, and `#21088`.
- [x] Fold `GHC#21030` into the redundancy/suggested-signature simplification note, `GHC#21031` into the standard fix-it ranking note, `GHC#21063` into the structured diagnostic-record note, `GHC#21086` into the numeric-literal mismatch note, and `GHC#21088` into the semantic-query instantiation note.
- [x] Record the focused later GHC keyword slice from `ghc-issues.json`: `redundant constraints inferred|required|DataKinds confusing error|structured errors integration|Num literals|inferred type variables instantiation` over `21030..21088`.
- [x] Record the later GHC derive/associated-member/record-diagnostic tranche `#21087`, `#21092`, `#21098`, `#21100`, and `#21101`.
- [x] Fold `GHC#21087` into the `derive via` trusted-evidence admissibility note, classify `GHC#21092` as `Type`/`Constraint` soundness background, fold `GHC#21098` into the missing-governing-instance associated-member note, `GHC#21100` into the undeclared associated-member definition note, and `GHC#21101` into the standard fix-it / record-wildcard repair note.
- [x] Record the focused later GHC keyword slice from `ghc-issues.json`: `KnownNat derivable|Type not apart from Constraint|associated type family non-instance|associated type wording|record wildcard` over `21087..21101`.
- [x] Record the later GHC profile/gating/signature tranche `#21102`, `#21103`, `#21104`, `#21126`, and `#21140`.
- [x] Fold `GHC#21102` / `#21103` / `#21104` into the language-profile and extension-gating note, `GHC#21126` into the scoped kind/type binder naming note, and `GHC#21140` into the placeholder-type-in-explicit-type-application note.
- [x] Record the focused later GHC keyword slice from `ghc-issues.json`: `GADTs without GADTs|Haskell98 constructors|ExplicitForAll parser|heterogeneous equality SAKS|TypeApplications PartialTypeSignatures` over `21102..21140`.
- [x] Record the later GHC warning/ambiguity/record-update tranche `#21106`, `#21110`, `#21121`, `#21151`, and `#21158`.
- [x] Fold `GHC#21106` into the whole-build diagnostic-context note, `GHC#21110` into the interactive-vs-batch warning-policy note, `GHC#21121` into the unordered-candidate normalization note, `GHC#21151` into the ambiguity-check / hidden-structure diagnostic notes, and `GHC#21158` into the record-update equivalence note.
- [x] Record the focused later GHC keyword slice from `ghc-issues.json`: `unused-packages component|unused-packages interactive|instance order optimisations|ambiguity check insoluble constraints|record update typing` over `21106..21158`.
- [x] Record the later GHC Unicode/kind/custom-error tranche `#21149`, `#21161`, `#21162`, `#21167`, and `#21172`.
- [x] Fold `GHC#21149` into the rejected/custom-error forcing-timing note, `GHC#21161` into the Unicode-rendering note, `GHC#21162` into the future Unicode operator-profile note, `GHC#21167` into the stable diagnostic-delimiter note, and `GHC#21172` into the kind-position family-normalization note.
- [x] Record the focused later GHC keyword slice from `ghc-issues.json`: `TypeErrors eagerly|Unicode display|Unicode mathematical functions|constraints in parens|kind families reduction` over `21149..21172`.
- [x] Record the later GHC subsumption/build/coherence/quotation/docs tranche `#21168`, `#21170`, `#21208`, `#21216`, and `#21220`.
- [x] Fold `GHC#21168` into the higher-order subsumption note, `GHC#21170` into the external build-unit identity note, `GHC#21208` into the conservative overlap/coherence note, `GHC#21216` into the quote/quasiquote active-grammar note, and `GHC#21220` into the generated-vs-user documentation merge-precedence note.
- [x] Record the focused later GHC keyword slice from `ghc-issues.json`: `implicit params simplified subsumption|build systems package names|overlapping instances regression|OverloadedRecordDotSyntax QuasiQuote|TH added documentation` over `21168..21220`.
- [x] Record the later GHC constraints/projection/Unicode/interactive-warning tranche `#21209`, `#21226`, `#21228`, `#21237`, and `#21252`.
- [x] Fold `GHC#21209` into the grouped/higher-order constraint note, `GHC#21226` into the projection-syntax field-name compatibility note, `GHC#21228` into the Unicode source-acceptance note, `GHC#21237` into the non-deferrable-constraints note, and `GHC#21252` into the interactive-vs-batch warning-policy note.
- [x] Record the focused later GHC keyword slice from `ghc-issues.json`: `quantified constraint functional dependency|OverloadedRecordDot label field|ZWJ string literal|non-deferrable constraints|Wunused-local-binds GHCi` over `21209..21252`.
- [x] Record the later GHC docs/rendering/provenance tranche `#21269`, `#21275`, `#21297`, `#21299`, and `#21315`.
- [x] Fold `GHC#21269` into the documentation-attachment/parsing note, `GHC#21275` into the semantic-query multiplicity-rendering note, `GHC#21297` into the quoted-syntax round-trip note, `GHC#21299` into the source-provenance note, and `GHC#21315` into the source-provenance / redundancy-warning notes.
- [x] Record the focused later GHC keyword slice from `ghc-issues.json`: `haddock parse error trailing doc comment|linear constructor printed identically|TH pretty-printer parentheses|SrcSpan TH splices|redundant-constraints provenance` over `21269..21315`.
- [x] Record the later GHC arity/package/generalization/field-warning tranche `#21306`, `#21307`, `#21309`, `#21324`, and `#21360`.
- [x] Fold `GHC#21306` into the malformed-constructor/pattern-arity robustness note, `#21307` into the human-facing package-identity diagnostic note, `#21309` into the declaration-order/generalization note, `#21324` into the exported-surface-vs-hidden-field-protocol note, and `#21360` into the path-refined record-update warning note.
- [x] Record the Hylo mutable-iteration/refinement tranche `#1510`, `#1512`, `#1517`, `#1523`, and `#1534`.
- [x] Fold `hylo#1510` / `#1512` into the future mutable-iteration-sugar note and later ownership hold row, `#1517` into the trait/supertrait cycle-checking note, `#1523` into the augmented-assignment / immutable-binding mutation policy note, and `#1534` into existing iterator-loop coverage plus backend-background classification.
- [x] Record the focused Hylo keyword slice from `hylo-issues.json`: `unexpected coercion Self.Element|for inout|circular refinement|mutation through let binding|for loop crashes Iterator` over `1510..1534`.
- [x] Record the Hylo array/source-range/coherence/assertion tranche `#1533`, `#1541`, `#1557`, `#1562`, and `#1566`.
- [x] Fold `hylo#1533` into the portable `Array` surface note, `#1541` into the diagnostic source-range note, classify `#1557` as async-entry/typechecker implementation background, stage `hylo#1562` as an assignment type-mismatch negative and also fold it into the ordinary-user-error robustness note, and record `#1566` as a later coherence/spec-clarity follow-up for conditional supertrait source selection.
- [x] Record the focused Hylo keyword slice from `hylo-issues.json`: `large buffer literals slow inference|source locations line column|async entrypoint dispatch memory corruption|assertion failure in assignment|multiple conditional sources of conformances` over `1533..1566`.
- [x] Record the focused later GHC keyword slice from `ghc-issues.json`: `too few fields GADT constructor panic|unused imports package-id|order dependence monomorphism restriction|HasField hidden selector|incomplete-record-updates do-notation` over `21306..21360`.
- [x] Record the Hylo tuple-label/subscript/property/mutability tranche `#1509`, `#1526`, `#1527`, `#1555`, `#1596`, and `#1600`.
- [x] Fold `hylo#1509` into the existing alias/member-projection coverage, `#1526` into builtin subscript-surface background only, `#1527` into the primary-root-cause diagnostic note, `#1555` into the future getter-only property-requirement ergonomics note, `#1596` into the staged immutable-assignment coverage, and `#1600` into the later projection / sink-consumption ownership tranche.
- [x] Record the focused Hylo keyword slice from `hylo-issues.json`: `tuple labels|subscript|ambiguous use print|stored properties satisfy property requirements|let binding mutation|sink in subscripts` over `1509..1600`.
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
| Wave 5: Flow typing, safe navigation, and Elvis | `expanded_staged_batch_in_progress` | `KOTLIN_ISSUE_CANDIDATES.md`; existing real fixtures already cover the stable Kotlin-adjacent baseline for safe navigation, Elvis, constructor-test projection, ordinary function application, imports/modules, literals, comments, script-mode hooks, and string interpolation; staged Kotlin roots now cover the direct imports enumerated in the Kotlin note, including generic/result/contextual lambda inference, qualified constructors with function-type payloads, receiver call/projection sugar, constructor defaults, Unit behavior, equality/refinement, optional function and tuple type precedence, iterator/loop semantics, explicit no-op `Unit` branches, map literals, pure blocks, assignment-target rejection, character and numeric literal edge cases, non-optional Elvis rejection, `try` / `finally` finalizer typing, `try` / `except` handler-result semantics, nested `elif`, backtick identifiers, constructor-only `is`, import/name conflicts, module fragments, labeled returns, boolean refinement, post-dominating `do` refinement, parenthesized anonymous-function application as a branch RHS, contextual lambda inference for receiver-style bodies, singleton-import sugar, tuple-return/destructure, and user-defined-operator resolution inside string interpolation; Kotlin direct reads are now classified through `KT-900` in the refreshed repo-local YouTrack corpus, with the stale legacy-snapshot false-absent backlog fully re-read through `KT-747`; the Kotlin note currently carries 44 absent labels and all 44 are true absences in the refreshed corpus, with the exact absent-id set kept current in `KOTLIN_ISSUE_CANDIDATES.md`; `KT-730` is staged as tuple-return/destructure, `KT-762` maps to the existing unresolved-import fixture, `KT-767` maps to the existing non-exhaustive-match fixture, `KT-784` maps to the existing generic higher-order lambda result-inference fixture, `KT-787` maps to the existing recursive-cycle negative, `KT-793` splits across the existing explicit-`Unit` no-op-branch fixture and existing non-exhaustive-match coverage plus diagnostic-quality background, `KT-709`, `KT-748`, `KT-778`, `KT-779`, `KT-796`, and `KT-797` fold into the narrowed `Array` convenience/exposure note, `KT-789` and `KT-868` reinforce the portable prelude exposure policy for basic sequence/cardinality helpers, `KT-842` reinforces the portable `String` convenience-member note, `KT-723`, `KT-724`, `KT-725`, `KT-740`, `KT-790`, `KT-817`, and `KT-894` reinforce the augmented-assignment / increment ergonomics note, `KT-742`, `KT-743`, `KT-750`, and `KT-877` reinforce future ergonomics notes already tracked in `SPEC_ADDITIONS.md`, `KT-774` reinforces the mutable-`var` stable-alias divergence note, `KT-821` is already covered by the staged range-generator runtime fixture and now also reinforces the portable range-surface note, `KT-826` maps to the existing constructor-default dependency-order negative while ordinary function defaults remain intentionally absent in Kappa v0.1, `KT-828` adds a future-ergonomics note for eager boolean-algebra helpers beyond short-circuit operators, `KT-848` is already covered by the real nested-comment fixture, `KT-850` is already covered by the current multiline/raw string-literal surface, `KT-867` maps to the staged iterator-protocol fixture, `KT-869` maps to the staged labeled-lambda-return fixture, `KT-873`, `KT-883`, `KT-884`, `KT-885`, `KT-886`, `KT-891`, `KT-892`, and `KT-896` are backend verifier/plugin/codegen/light-class, error-type, or performance-optimization background, `KT-875` is diagnostic-string-formatting background, `KT-876` is receiver-bound import/object background, `KT-878` is an intentional divergence around nullable `Byte` conversion-property surface, `KT-881` maps to the existing real interpolation fixture, `KT-887` is runtime packaging/classpath background, `KT-888` is Java interop / class-object static-field background, `KT-889` is staged as interpolation operator-resolution coverage, `KT-890` is backend/main-entry validation background, `KT-895` is parser diagnostic-quality background, `KT-897` and `KT-898` are Kotlin property/initializer-block ordering/reinitialization background, `KT-900` is class-object accessibility / Java-interop background, `KT-798`, `KT-799`, and `KT-804` are intentional divergences because Kappa `return` remains a `do`-statement, not an expression, `KT-802`, `KT-810`, `KT-818`, `KT-829`, `KT-834`, `KT-840`, `KT-841`, `KT-856`, and `KT-880` are namespace/script-mode, JavaBean/Java-interop, backend-conversion, or OO override-resolution background only, `KT-811` and `KT-816` are stdlib/prelude placement background, `KT-812`, `KT-814`, `KT-823`, `KT-825`, `KT-827`, `KT-831`, `KT-832`, `KT-835`, `KT-838`, `KT-843`, `KT-847`, `KT-853`, `KT-854`, `KT-855`, `KT-857`, and `KT-862` are tooling/project-resolution, completion, testing-UX, inference-reporting, parser-recovery, syntax/lexical, formatter, renderer, or debugger/test-harness background, `KT-833`, `KT-839`, `KT-844`, `KT-845`, `KT-846`, `KT-849`, `KT-858`, and `KT-860` are compiler/frontend/backend/plugin/linkage/lowering/concurrency robustness background, `KT-859` is Kotlin delegation / `by`-clause background only, `KT-863` is Java collection interop background only, `KT-865` is string-DSL / compiler-robustness background, and `KT-815` plus `KT-819` are already covered in simpler Kappa form by the existing duplicate-term-declaration negative | promote the staged Kotlin roots; keep the safe-call-implies-present-receiver family visible as an ergonomic/spec-follow-up rather than pretending v0.1 already guarantees it; continue with `KT-901`-`KT-905` by `idReadable` in the refreshed YouTrack corpus |
| Wave 6: Macros, hygiene, and staging | `first_staged_batch_complete` | `MACRO_ISSUE_CANDIDATES.md`; staged macro roots for local binder hygiene, generated dependent-record-type hygiene, and imported-macro module-boundary hygiene | promote the staged macro roots; keep export-round-trip and syntax-category-specific background issues visible without pretending Kappa already has a declaration-macro surface |
| Wave 7: Totality and termination | `first_staged_batch_complete` | `TOTALITY_ISSUE_CANDIDATES.md`; staged totality roots for cycles, alias-preserved descent, helper lowering, constructor-spine descent, index-refined recursion, and mutual recursion | promote staged totality roots; keep `lean4#262` and `agda#3` visible as later imports once the surface form is pinned down more tightly |
| Wave 8: Coherence and solver edges | `expanded_staged_batch_in_progress` | `COHERENCE_ISSUE_CANDIDATES.md`; staged solver roots now cover local supertrait projection, failed-premise candidate discard before coherence, and a basic overlapping-instance rejection; Scala implicit note now records supertrait projection as staged rather than still missing | promote the staged coherence roots; keep the associated-member overlap and unrelated-premise ambiguity cases visible for the next solver tranche |
| Wave 9: FFI, dynamic values, and bridge boundaries | `test_backfill_started` | `FFI_RUNTIME_ISSUE_CANDIDATES.md`; first non-Kotlin FFI pass covers Zig, Mojo, Gleam, OCaml, Roc, Aeneas, and Creusot issue searches; harvested-test reads now include Zig C ABI positive and compile-error fixtures, Gleam external-function target/annotation/codegen tests, Mojo FFI/Python interop tests, and Roc platform-boundary test snapshots; staged roots now cover explicit `std.ffi`, `std.ffi.c`, `std.gradual`, and `std.bridge` standard-module vocabulary plus boundary classification constructors | continue PR/body backfill for the accepted-later ABI/layout/bridge candidates; promote staged standard-module tests once `std.ffi`, `std.ffi.c`, `std.gradual`, and `std.bridge` exist locally; later ABI diagnostics should use the read Zig compile-error fixtures as oracle shapes |
| Wave 10: Runtime, fibers, interruption, and resource cleanup | `behavior_batch_staged` | `RUNTIME_CONCURRENCY_ISSUE_CANDIDATES.md`; first manifest/keyword tranche covers Chapel, Pony, Encore, Elixir, and Inko inventory; direct reads include Chapel structured task synchronization / cleanup / atomics, Pony finalization / monitoring / stdio blocking / CAS memory orders, Encore future completion and tracing, Elixir supervisor/startup/interrupt/receive cases, and Inko process scheduling / receive waits / scoped panics / blocking pools / destructor escape / atomics; staged roots now cover atomic exchange/fetch-old, compare-exchange success/failure, atomic-vs-ordinary-ref separation, and supervisor shutdown idempotence | backfill PR/test artifacts for the accepted-later runtime candidates; read harvested runtime tests for Inko/Chapel/Pony/Encore/Elixir; stage deeper runnable fiber/interruption/supervision fixtures only after the local runtime surface can execute the standard `fork`, `await`, `Scope`, `Monitor`, `timeout`, `race`, `defer`, and `using` shapes reliably |
| Wave 10B: Runtime hashing and collection acceleration | `behavior_batch_staged` | `HASH_COLLECTION_ISSUE_CANDIDATES.md`; first hash/collection pass covers Inko `HashMap` / map-performance / hash algorithm issues and Chapel user-defined hashing / hash-combine / return-requirement issues; staged roots now cover `Hashable` refining `Eq`, borrow-not-consume hashing, linear `HashState`, and Set/Map well-formedness without `Hashable`; `SPEC_ADDITIONS.md` tracks the open decision about whether `HashCode` should expose `Eq` / `Ord` as opaque same-execution operations | backfill PR/test artifacts for accepted hash candidates; read harvested map/set/distinct/grouping tests; later fixtures should prove hash acceleration preserves denotation rather than standardizing a specific hash algorithm |
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
| `hylo` | `OWNERSHIP_ISSUE_CANDIDATES.md`, `HYLO_MEMBER_ALIAS_ISSUE_CANDIDATES.md`, `COHERENCE_ISSUE_CANDIDATES.md` | full-corpus keyword scan for `borrow`, `lifetime`, `capture`, `inout`, `accessor`, `alias`; focused follow-up scan for `typealias`, `associated type`, `generic arguments`, `receiver`, `member`, `tuple labels`, `extension`, `conformance`, `lookup`, `property`, `subscript`; focused later keyword slice `tuple labels|subscript|ambiguous use print|stored properties satisfy property requirements|let binding mutation|sink in subscripts` over `1509..1600`; focused later keyword slice `unexpected coercion Self.Element|for inout|circular refinement|mutation through let binding|for loop crashes Iterator` over `1510..1534`; focused later keyword slice `large buffer literals slow inference|source locations line column|async entrypoint dispatch memory corruption|assertion failure in assignment|multiple conditional sources of conformances` over `1533..1566` | exact body reads: `#3`, `#39`, `#179`, `#410`, `#424`, `#571`, `#629`, `#674`, `#675`, `#676`, `#688`, `#689`, `#814`, `#819`, `#850`, `#878`, `#921`, `#936`, `#955`, `#1042`, `#1088`, `#1398`, `#1509`, `#1510`, `#1512`, `#1517`, `#1523`, `#1526`, `#1527`, `#1533`, `#1534`, `#1541`, `#1550`, `#1555`, `#1557`, `#1562`, `#1566`, `#1582`, `#1599`, `#1600`, `#1707`, `#1807` | ownership note complete; staged batch covers block-local function capture, a spec-driven transitive local-capture companion, local type-alias capture of outer generic context, and pattern-bound local capture; the focused Hylo member/alias note now also covers tuple-labeled alias projection, a future getter-only property-requirement follow-up, mutable iteration is parked as a later ergonomics/ownership surface, iterator-loop crashes are mapped to existing protocol coverage plus background, `hylo#1562` is now staged as a direct assignment type-mismatch negative, and `hylo#1566` is parked as a later coherence/spec-clarity follow-up rather than forced into a premature fixture | pause the current Hylo direct-fit scan unless we deliberately open a new targeted Hylo pass beyond the current alias / associated-member / projection-property / mutable-iteration / conditional-conformance cluster |
| `granule` | `OWNERSHIP_ISSUE_CANDIDATES.md` | full-corpus keyword scan for `linear`, `linearity`, `nonlinear`, `case`, `match`, `usage` | exact body reads: `#4`, `#9`, `#17`, `#23`, `#37`, `#38`, `#42`, `#45`, `#48`, `#52`, `#53`, `#54`, `#56`, `#59`, `#65`, `#195`, `#207`, `#252` | ownership note complete; staged batch covers branch-condition consumption, branch-drop, and erased-scrutinee discrimination; the later Granule reads are now classified as modal-extension or diagnostic-shaped background rather than new ownership fixtures | pause the current Granule ownership scan unless we explicitly open a later modal-extension or diagnostics pass |
| `ghc` | `GHC_ISSUE_CANDIDATES.md` | curated direct-fit pass around layout, guards, indexed exhaustiveness, constructor-informed projection, local implicit behavior, and adjacent record-update / associated-type edge cases; keyword backfill against titles/bodies for `implicit parameter|type synonym|associated type|type family|record update|field|GADT|scoped type variable`, with later focused backfill for `superclass|constraint synonym|existential|desugaring|tuple constraint` and later associated-member follow-ups for `nullary class|Template Haskell|functional dependency|kind inference`, `derive instance|default declaration|normalize instance`, `derive any class|missing-methods|associated data`, and `generalized newtype deriving|TypeError|special classes`, followed by a `derive via|standalone deriving|DeriveGeneric` pass, an `inferred constraints|ambiguous deriving|covered` pass, a small `strategy warning|indexed deriving impossible cases` pass, a refreshed top-level scan over the moved `ghc-issues.json` snapshot for `functional dependency|DerivingStrategies|missing deriving strategies|stock deriving|via`, and later derive-focused tranches for `quantified constraint|InstanceSigs|via type|rank-n deriving`, `redundant constraint|unused deriving|exotic context|empty class list|visible dependent quantification`, and `TyClGroups|Template Haskell|unreduced type|kinding information` | exact note set now includes `#1`, `#29`, `#30`, `#246`, `#271`, `#281`, `#366`, `#430`, `#462`, `#518`, `#595`, `#700`, `#734`, `#760`, `#810`, `#821`, `#827`, `#890`, `#961`, `#981`, `#10020`, `#10132`, `#10361`, `#1041`, `#1060`, `#10808`, `#10811`, `#10815`, `#10817`, `#10856`, `#10899`, `#11067`, `#11115`, `#11136`, `#11156`, `#11278`, `#11343`, `#11450`, `#11451`, `#11466`, `#11534`, `#1204`, `#12190`, `#12507`, `#13398`, `#13404`, `#13773`, `#13971`, `#13972`, `#1401`, `#14046`, `#14094`, `#14132`, `#14230`, `#1445`, `#14462`, `#14661`, `#14728`, `#14916`, `#15052`, `#15178`, `#15191`, `#15376`, `#15434`, `#15711`, `#15831`, `#15839`, `#15868`, `#15932`, `#15969`, `#16179`, `#16181`, `#16194`, `#16322`, `#16341`, `#16362`, `#16578`, `#16641`, `#16655`, `#16714`, `#16923`, `#16958`, `#17013`, `#17014`, `#17183`, `#17210`, `#17312`, `#17767`, `#17880`, `#17899`, `#18047`, `#18130`, `#18148`, `#18165`, `#18219`, `#18258`, `#18271`, `#18321`, `#18388`, `#18474`, `#18483`, `#18488`, `#16501`, `#16839`, `#17104`, `#1723`, `#17647`, `#1823`, `#18311`, `#1834`, `#18650`, `#18802`, `#18809`, `#18999`, `#19084`, `#19088`, `#1948`, `#19463`, `#19972`, `#2141`, `#23570`, `#23768`, `#2435`, `#25853`, `#2595`, `#25991`, `#2661`, `#26714`, `#2852`, `#3038`, `#3743`, `#4226`, `#5120`, `#5713`, `#5852`, `#7658`, `#8011`, `#8297`, `#8978`, `#9063`, `#9167`, `#9264`, `#9318`, `#9394`, `#9429`, `#9433`, and `#9437`; direct body reads confirmed in this workspace for `#1`, `#246`, `#271`, `#281`, `#366`, `#430`, `#462`, `#518`, `#595`, `#700`, `#734`, `#760`, `#810`, `#821`, `#827`, `#890`, `#961`, `#981`, `#10020`, `#10132`, `#10361`, `#1041`, `#1060`, `#10808`, `#10811`, `#10815`, `#10817`, `#10856`, `#10899`, `#11067`, `#11115`, `#11136`, `#11156`, `#11278`, `#11343`, `#11450`, `#11451`, `#11466`, `#11534`, `#1204`, `#12190`, `#12507`, `#13398`, `#13404`, `#13773`, `#13971`, `#13972`, `#1401`, `#14046`, `#14094`, `#14132`, `#14230`, `#1445`, `#14462`, `#14661`, `#14728`, `#14916`, `#15052`, `#15178`, `#15191`, `#15376`, `#15434`, `#15711`, `#15831`, `#15839`, `#15868`, `#15932`, `#15969`, `#16179`, `#16181`, `#16194`, `#16322`, `#16341`, `#16362`, `#16578`, `#16641`, `#16655`, `#16714`, `#16923`, `#16958`, `#17013`, `#17014`, `#17183`, `#17210`, `#17312`, `#17767`, `#17880`, `#17899`, `#18047`, `#18130`, `#18148`, `#18165`, `#18219`, `#18258`, `#18271`, `#16501`, `#16839`, `#17104`, `#1723`, `#17647`, `#1823`, `#18311`, `#1834`, `#18650`, `#18802`, `#18809`, `#18999`, `#19084`, `#19088`, `#1948`, `#19463`, `#19972`, `#2141`, `#23570`, `#23768`, `#2435`, `#25853`, `#2595`, `#25991`, `#2661`, `#26714`, `#2852`, `#3038`, `#3743`, `#4226`, `#5120`, `#5713`, `#5852`, `#7658`, `#8011`, `#8297`, `#8978`, `#9063`, `#9167`, `#9264`, `#9318`, `#9394`, `#9429`, `#9433`, and `#9437` | direct-fit GHC note remains complete; staged batch still covers layout, guard exhaustiveness, indexed refinement, constructor-informed projection, lexical implicit shadowing, nested richer-local-implicit-context selection, associated-member use in trait signatures, trailing-implicit partial application, normalized associated-member instance search through aliases, invalid-record-update rejection, imported-trait-alias identity inside instance declarations, associated-member-typed implicit arguments, premise-supertrait satisfaction of instance superclass obligations, and alias-premise satisfaction of instance superclass obligations, while the later tranche classifies `#246` as laziness/background, `#518` as overlap-warning duplicate evidence, `#700` as higher-rank-pattern background, `#734` as diagnostic-cascade background, `#760` as macro/scoped-type-variable background, `#827` as lazy-pattern poor fit, `#890` as constructor-constraint / record-update background, `#961` as already covered by associated-member fixtures, `#10020` as nullary-associated-family model-mismatch background, `#10132` and `#15711` as kind-inference background, `#10361`, `#10817`, `#10899`, `#11136`, `#13971`, `#14094`, `#23768`, `#9063`, and `#9264` as follow-up evidence for possible default associated static members, `#10811` as Template Haskell quotation/printing background, `#10815`, `#11450`, `#13398`, and `#13773` as follow-up evidence for associated-member instance-head substitution/validation under ordinary normalization, `#11115` and `#14132` as follow-up evidence for diagnosing failed associated-member normalization via the missing governing trait instance or upstream owner dictionary, `#10362` and `#11278` as follow-up evidence for a reusable grouped-constraint alias surface, `#11451` as binder-validation / warning-policy follow-up, `#11466` as a model-mismatch around Haskell implicit-parameter constraint descriptors, `#11534` and `#14046` as fundep-specific poor fits, `#1204` as evidence for record updates through associated-type normalization, `#12507` as covered by the staged richer-local-implicit-context fixture, `#13404`, `#14462`, `#14661`, `#14728`, `#14916`, `#15052`, `#15191`, `#15868`, `#16341`, `#16714`, `#16958`, and `#17013` as follow-up evidence for future deriving support on traits with associated static members, richer coercion-based deriving, indexed impossibility pruning, inferred premises, role-sensitive traversal derivation, admissibility checks, non-skippable rejected constraints, and explicit-instance-equivalent determinism checks, `#15178`, `#15376`, `#15434`, `#15831`, `#15839`, `#16179`, `#16362`, `#16641`, `#16655`, `#16923`, `#17014`, `#17183`, `#17312`, `#17767`, `#18047`, `#18130`, `#18219`, `#18258`, `#18271`, `#18483`, and `#18488` as follow-up evidence for a possible explicit coercion-based `derive via` surface, including standalone-vs-inline scoping, explicit strategy syntax, extension-gating policy, representation-sensitive traits like `Generic`, consistent rejected-constraint timing, explicit source-instance resolution for nullary members, strategy-default warnings and warning flags, warning only when strategy choice is genuinely ambiguous, extra method-local premises, quantified-constraint / role interactions, optional-parenthesis syntax, inferred-context simplification policy, empty-list / representation-witness semantics, standalone-binder restrictions, unreduced via/source type acceptance, whole-declaration kind propagation, upfront via-type validation, and forbidding hidden quantification in the `via` type, `#17210` as a spec-addition follow-up for possible projection-based derive customization, `#17880` as implementation/soundness background for higher-rank deriving code generation, `#17899` as a diagnostic-quality/spec-addition follow-up for keeping generated derived member signatures visible in dumps/diagnostics, `#18148` and `#18165` as warning-policy/spec-addition follow-ups for synthesized-code warning hygiene, `#18321` as implementation/pipeline background, `#18388` as macro/template roundtrip background, and `#18474` as more indexed-deriving / impossibility-pruning follow-up evidence, `#15932` as tooling/coverage background for generated deriving code, `#15969` and `#16578` as implementation/performance background around generating better coercion-based code for `Generic1` and enumeration-derived `Ord`, `#16181`, `#16194`, and `#16322` as implementation/panic-or-soundness background rather than new surface obligations, `#13972` and `#14230` as follow-up evidence for hidden-structure / invisible-kind / associated-index diagnostic rendering, `#1723` as indexed-normalization soundness background, `#1823` as scoped-type-variable nested-pattern background, `#1948` as associated-type recursion / solver background, `#23570`, `#25991`, and `#10749` as import/interface background, `#25853` as covered by the imported-trait-alias instance fixture, `#2595` as structural-record / existential-package adjacency already largely covered, `#2661` and `#8978` as covered by the normalized-associated-member alias fixture, `#26714` and `#10335` as staged superclass-obligation coverage, `#2852`, `#11067`, and `#9023` as Haskell-specific poor fits, `#3743` as inference/background only, `#4226` and `#17104` as already covered by lexical implicit capture, `#5713` as covered by the imported-trait-alias instance fixture, `#7658`, `#8297`, and `#9429` as spec-addition follow-ups for empty-record-update ergonomics, optional/default implicit arguments, and dynamic-placeholder / `DynRep` portability clarification, `#10808`, `#10856`, `#11156`, `#16501`, `#18311`, and `#18802`, and `#18809` as spec-addition follow-ups for possible type-changing / existential / desugared record updates, `#7989`, `#18650`, `#19084`, `#19972`, `#13972`, and `#14230` as diagnostic/warning-quality follow-up or background, `#18999` and `#11343` as field-name-resolution background, `#12190` as explicit irrefutability/generalization background, `#16839` as backend/lint background, `#17647` and `#19088` as optimization background, `#19463` as compiler-representation background, `#8011` and `#9394` as tooling-only background, `#9167` as neutral background, `#9318` as diagnostic-only background, and `#9433` / `#9437` as syntax/model-specific poor fits, and records `#810`, `#821`, `#981`, `#1204`, `#1834`, and `#3038` as evidence for spec-clarity notes about constructor-local branch evidence, alias-preserved implicit structure, alias-preserved implicit structure under application sugar, record updates through associated-type normalization, and normalized associated-member goals in instance search | revisit only after current staged GHC roots are promoted or if we deliberately open a later generic-scope, macro-hygiene, deeper indexed-pattern, associated-member record-update, type-family normalization, deriving-semantics, projection-customized deriving, or explicit `derive via` follow-up |
| `links` | `LINKS_ISSUE_CANDIDATES.md` | full-corpus keyword scan for `distinct`, `order by`, `skip`, `take`, `group by`, `for clause`, `where clause`, `query`, `comprehension` | exact body reads: `#11`, `#13`, `#25`, `#26`, `#340`, `#834` | direct-fit Links note complete; staged batch covers nested capture, let-bound ordering, generator equivalence, and projection stability | continue with nearby query scoping / normalization issues after the first staged batch is promoted; keep `links#340` for the later solver/effects wave |
| `unison` | `EFFECTS_ISSUE_CANDIDATES.md` | full-corpus keyword scan for `handler`, `resume`, `resumption`, `finally`, `finalizer`, `effect`, `ability`, `capability`, `short-circuit` | exact body reads: `#374`, `#696`, `#697`, `#979`, `#1010` | effects note now captures the strongest direct and indirect Unison fits; the first staged effects batch now includes wrong-label rejection and handler-case order invariance | keep `#374`, `#697`, and `#696` for the later or spec-follow-up tranche |
| `koka` | `EFFECTS_ISSUE_CANDIDATES.md` | full-corpus keyword scan for `handler`, `resume`, `resumption`, `finally`, `finalizer`, `effect`, `ability`, `capability`, `short-circuit` | exact body reads: `#129`, `#360`, `#564`, `#649` | effects note now captures the strongest Koka direct fits; the first staged effects batch now includes short-circuit preservation and non-resuming-finalizer behavior | revisit `#649` after the first direct batch; leave `#564` as poor-fit background evidence |
| `effekt` | `EFFECTS_ISSUE_CANDIDATES.md` | full-corpus keyword scan for `handler`, `resume`, `resumption`, `finally`, `finalizer`, `effect`, `ability`, `capability`, `short-circuit` | exact body reads: `#50`, `#548`, `#861`, `#971` | effects note now captures the strongest Effekt direct fits; the first staged effects batch now includes direct and higher-order capability-escape negatives | keep `#861` and `#971` visible for the later optimizer / trace-oriented tranche |
| `lean4` | `TOTALITY_ISSUE_CANDIDATES.md`, `MACRO_ISSUE_CANDIDATES.md` | full-corpus keyword scan for `termination`, `non-exhaustive`, `exhaustive`, `with function`, `hygiene`, `macro`; full-corpus macro scan for `macro`, `hygiene`, `quote`, `quotation`, `splice`, `splicing`, `syntax`, `antiquot`, `quasisyntax`, `template`; focused macro follow-up scan for `hygiene`, `scope`, `binding`, `identifier`, `double-quoted`, `structure`, `module`, `export`; focused early-issue follow-up filter for `error message`, `panic`, `dbgTrace`, `trace`, `module`, `symbol`, `equation`, `elaborat`, `delab`, `shadow`, `linker` | exact body reads: `#2`, `#29`, `#30`, `#67`, `#99`, `#108`, `#111`, `#174`, `#177`, `#180`, `#181`, `#182`, `#184`, `#190`, `#191`, `#192`, `#193`, `#196`, `#198`, `#217`, `#219`, `#220`, `#233`, `#239`, `#240`, `#242`, `#243`, `#247`, `#249`, `#250`, `#255`, `#262`, `#281`, `#308`, `#312`, `#317`, `#333`, `#335`, `#337`, `#346`, `#348`, `#350`, `#351`, `#352`, `#355`, `#362`, `#366`, `#375`, `#391`, `#414`, `#430`, `#446`, `#447`, `#451`, `#452`, `#453`, `#465`, `#485`, `#494`, `#529`, `#586`, `#793`, `#821`, `#1006`, `#1124` | totality note now tracks first-pass termination imports as staged in `new-tests/`; macro note now records `#2` as placeholder-context provenance evidence, `#29` as equation/helper-lowering misdiagnostic evidence, `#30` as module/artifact case-identity evidence, `#67` as panic-time trace-flush evidence, `#196` as multi-location composite-diagnostic evidence, `#217` as source-like placeholder-type rendering evidence, `#219` as partial lowered-helper delaboration fallback evidence, `#220` as future as-pattern delaboration evidence, `#243` as suspicious-shadowing / same-spelling diagnostic-disambiguation evidence, `#239` as local-variable hover/query-baseline evidence, `#247` as desugaring-aware monadic-lifting diagnostic evidence, `#250` as recursive-group interface-metadata evidence, `#249` plus `#181` plus `#529` as future scoped macro / notation activation evidence, `#174` as optional-syntax-view evidence, `#177` plus `#219` plus `#317` as quoted/generated-syntax round-trip and source-like matcher-rendering evidence, `#180` plus `#242` as macro-definition linting/validation evidence, `#182` plus `#192` plus `#193` plus `#446` plus `#485` plus `#494` as generated-syntax rendering / formatter-coverage evidence, `#184` as macro tracing observability evidence, `#190` as fully explicit universe/kind rendering evidence, `#191` as future `where`-style local-scope evidence, `#255` plus `#308` as syntax-abbreviation / local-notation lexical-capture evidence, `#333`, `#335`, `#337`, and `#352` as hidden-structure / implicit-binder / blocked-metavariable / universe-mismatch evidence, `#348` as parser-repair/local-token-blame evidence, `#350` as trivial-wrapper elaboration-equivalence evidence, `#351` plus `#346` as field-resolution/default-instance fallback evidence, `#355` as solved-prerequisite propagation evidence for instance search, `#362` as partial-syntax semantic-service progress evidence, `#366` as future lexical policy-override scope evidence for nested elaboration contexts, `#375` as source-like signature-rendering evidence, `#391` as a quotation-robustness note, `#451` plus `#452` as future layout-integration evidence for parser customizations, `#453` as extension-surface consistency evidence for registered elaboration hooks, `#465` as future local-head custom-delaboration evidence, and classifies `#99` plus `#111` plus `#198` plus `#233` plus `#240` plus `#312` plus `#447` as antiquotation-pipeline / default-parameter-frontend / implementation-plumbing / docs-only / editor-client / tactic-surface / parser-category background | for the macro tranche, continue with `#343`, `#353`, and `#365`; only then revisit `#821` if we still need broader generated-record coverage |
| `Idris2` | `TOTALITY_ISSUE_CANDIDATES.md`, `DEPENDENT_PROOF_ISSUE_CANDIDATES.md` | totality scan: `termination`, `total`, `covering`, `with rule`, `with function`; dependent/elaboration scan: `implicit`, `metavariable`, `elaboration`, `hole`, `coverage`, `normalization`, `erased`, `universe`, `transport`, `equality`, `with`, `total`, `termination` | totality reads: `#19`, `#163`, `#403`, `#493`, `#660`, `#794`; dependent/elaboration reads now extend through `#798-#802`, with exact sparse tracking in the detailed ledger for `#694-#698`, `#704-#708`, `#709-#713`, `#714-#718`, `#723-#727`, `#728-#732`, `#733-#737`, `#738-#742`, `#743-#747`, `#748-#752`, `#758-#762`, `#763-#767`, `#768-#772`, `#773-#777`, `#778-#782`, `#783-#787`, `#788-#792`, `#793-#797`, and `#798-#802`; the next focused Idris cursor is `#803-#807` (`#803`, `#804`, and `#806` present locally; `#805` and `#807` absent) | totality note remains complete for the imported tranche; dependent-proof note also stages `expressions.match.positive_indexed_witness_refines_outer_arguments` from `Idris2#733`, `types.universes.quantities.qtt.negative_unrestricted_function_value_not_assignable_to_linear` from `Idris2#743`, `expressions.implicit_parameters.positive_pass_member_as_constrained_polymorphic_value` from `Idris2#763`, `effects.do_blocks.positive_do_bound_scrutinee_match_infers_type` from `Idris2#764`, and `literals.numeric_literals.positive_integer_literals_in_array_double_context` from `Idris2#802`, cross-links `#758` to the existing do-block linearity fixture, maps `#728` to existing explicit-implicit negatives, preserves `#735` as future descriptor-identity background, strengthens the scoped proof-search hint note with `#738`, the declaration-group elaboration note with `#745`, the future `with`-lowering/later-case-split note with `#751`, the Unicode-rendering note with `#759`, turns `#732` plus `#737` into helper-lowered hole-hygiene and immutable-installation prefix-split notes, strengthens the structured tooling-protocol note with `#768`, adds a semantic-highlighting consistency note from `#769`, folds `#773` into the generated-edit tooling note, adds a signed-integer-to-`Nat` conversion note from `#775`, adds a future rewrite-after-normalization note from `#776`, adds an evidence-binder / telescopic trait-header note from `#777`, strengthens that certified-wrapper note with proof-hypothesis normalization evidence from `#778`, adds a cross-module constructor-identity pattern-resolution note from `#779`, adds an instance-resolution repeated-subgoal performance note from `#783`, adds a declaration-diagnostic source-range precision note from `#784`, strengthens the literate-source note with interactive-tooling alignment evidence from `#785`, further strengthens the literate-source note with marker-family semantics evidence from `#788`, adds an interactive-evaluation robustness note from `#791`, strengthens the hidden-structure mismatch diagnostic note with implicit-binder evidence from `#792`, cross-links `#794` into the totality wave, adds an FFI type-argument erasure note from `#796`, adds a backend case-ordering codegen-stability note from `#797`, adds a block-local dependent-helper codomain-solving note from `#798`, adds a future foreign-descriptor helper-import note from `#800`, classifies `#801` as JS-backend numeric-representation background, and keeps the future forced-pattern saturation note from `#762` visible | continue with `#803-#807` in `DEPENDENT_PROOF_ISSUE_CANDIDATES.md`; keep `#660`, `#661`, `#664`, `#670`, `#677`, `#686`, and `#794` visible in their existing totality/spec-addition buckets while resuming the next sparse slice |
| `agda` | `TOTALITY_ISSUE_CANDIDATES.md` | full-corpus keyword scan for `termination`, `with function`, `macro`, `hygiene` | exact body reads: `#3`, `#59`, `#238` | totality note complete; staged batch covers helper-lowered structural descent and fixed-constructor-spine analogues | continue with `#8`, `#53`, `#58`, `#142`, `#217`; keep `#3` visible as an accepted-later helper-lowering import |
| `racket` | `MACRO_ISSUE_CANDIDATES.md` | full-corpus keyword scan for `macro`, `hygiene`, `quote`, `quotation`, `splice`, `splicing`, `syntax`, `antiquot`, `quasisyntax`, `template`; focused follow-up scan for `hygiene`, `scope`, `binding`, `identifier`, `gensym`, `renaming`, `rename`, `export`, `module`, `syntax class`, `template`, `splicing` | exact body reads: `#1483`, `#1493`, `#1746`, `#2133` | macro note complete; the first staged macro batch now includes an imported-macro module-boundary analogue for the strongest Racket export / renaming issues | continue with `#1410`, `#1479`, `#1495` if the macro wave resumes immediately |
| `kotlin` | `KOTLIN_ISSUE_CANDIDATES.md` | full-corpus keyword scan for `?.`, `?:`, `safe call`, `elvis`, `smart cast`, `smartcast`, `flow typing`, `nullability`, `nullable`, `null check`; refreshed repo-local follow-up scans for `sure()`, `rangeTo`, `unresolved imports`, `VerifyError`, `ClassFormatError`, `FunctionDescriptorBoundToReceiver`, `unary +`, `Too many characters in a character literal`, `Overload resolution ambiguity`, `Error generating method`, `NoClassDefFoundError`, `class object`, `0.dbl`, `JavaPackageScope.getProperties`, `should not compile an error type`, `CompilationException`, `Wrong error message`, `plusAssign`, `Reinitialization of val`, and `Inaccessible class should be unresolved`; absent-label audit compares every `absent in ... corpus snapshot` entry in `KOTLIN_ISSUE_CANDIDATES.md` against repo-local `issues.json` by `idReadable`; after the 2026-04-24 YouTrack layout refresh, issue ids must be read from `idReadable`, not internal `id` | exact body reads are enumerated in `KOTLIN_ISSUE_CANDIDATES.md`; latest incremental repo-local reads classified `KT-678`-`KT-900`, correcting every stale false-absent legacy-snapshot label through `KT-747`; the Kotlin note now carries 44 absent-labeled entries and all 44 are true absences in the refreshed corpus | Kotlin note is now current through `KT-900`; `KT-680` maps to existing same-type-equality coverage, `KT-682` is staged as singleton-term-import sugar, `KT-705` maps to existing named-function-as-value coverage, `KT-711` maps to existing non-optional-Elvis rejection coverage, `KT-730` is staged as tuple-return/destructure coverage, `KT-731` maps to expected-result polymorphic inference coverage, `KT-762` maps to real unresolved-import coverage, `KT-767` maps to real non-exhaustive-match coverage, `KT-784` maps to the existing generic higher-order lambda result-inference fixture, `KT-787` maps to the existing recursive-cycle negative, `KT-793` splits across the existing explicit-`Unit` no-op-branch fixture and existing non-exhaustive-match coverage plus diagnostic-quality background, `KT-709`, `KT-748`, `KT-778`, `KT-779`, `KT-796`, and `KT-797` fold into the narrowed `Array` convenience/exposure note, `KT-789` and `KT-868` reinforce the portable prelude exposure policy for basic sequence/cardinality helpers, `KT-842` reinforces the portable `String` convenience-member note, `KT-723`, `KT-724`, `KT-725`, `KT-740`, `KT-790`, `KT-817`, and `KT-894` fold into the augmented-assignment / increment ergonomics note, `KT-742`, `KT-743`, `KT-750`, and `KT-877` reinforce future ergonomics notes already tracked in `SPEC_ADDITIONS.md`, `KT-776` is later finalization/control-flow-analysis background, `KT-791` is the same intentional divergence as constructor-only `is` over erased generic runtime types, `KT-821` maps to the existing staged range-generator runtime fixture and reinforces the portable range-surface note, `KT-826` maps to the existing constructor-default dependency-order negative while ordinary function defaults remain intentionally absent in v0.1, `KT-828` adds a future-ergonomics note for eager boolean-algebra helpers beyond short-circuit operators, `KT-848` is already covered by the real nested-comment fixture, `KT-850` is already covered by the current multiline/raw string-literal surface, `KT-867` maps to staged iterator-protocol coverage, `KT-869` maps to staged labeled-lambda-return coverage, `KT-873`, `KT-883`, `KT-884`, `KT-885`, `KT-886`, `KT-891`, `KT-892`, and `KT-896` are backend verifier/plugin/codegen/light-class, error-type, or performance-optimization background, `KT-875` is diagnostic-formatting background, `KT-876` is receiver-bound import/object background, `KT-878` is an intentional divergence around nullable `Byte` conversion-property surface, `KT-881` maps to the real interpolation fixture, `KT-882` is floating-literal/backend numeric-lowering background, `KT-887` is runtime packaging/classpath background, `KT-888` is Java interop / class-object static-field background, `KT-889` is staged as interpolation operator-resolution coverage, `KT-890` is backend/main-entry validation background, `KT-895` is parser diagnostic-quality background, `KT-897` and `KT-898` are Kotlin property/initializer-block ordering/reinitialization background, `KT-900` is class-object accessibility / Java-interop background, `KT-798`, `KT-799`, and `KT-804` are intentional divergences because Kappa `return` remains a `do`-statement rather than an expression, `KT-802`, `KT-810`, `KT-818`, `KT-829`, `KT-834`, `KT-840`, `KT-841`, `KT-856`, and `KT-880` are namespace/script-mode, Java-interop/backend-conversion, or OO background only, `KT-811` and `KT-816` are stdlib/prelude placement background, `KT-812`, `KT-814`, `KT-823`, `KT-825`, `KT-827`, `KT-831`, `KT-832`, `KT-835`, `KT-838`, `KT-843`, `KT-847`, `KT-853`, `KT-854`, `KT-855`, `KT-857`, and `KT-862` are tooling/project-resolution, completion, testing-UX, inference-reporting, parser-recovery, operator-resolution, formatter, renderer, debugger, or unused-warning background, `KT-833`, `KT-839`, `KT-844`, `KT-845`, `KT-846`, `KT-849`, `KT-858`, and `KT-860` are compiler/backend/plugin/linkage/lowering/concurrency robustness background, `KT-859` is Kotlin delegation / `by`-clause background only, `KT-863` is Java collection interop background only, `KT-865` is string-DSL / compiler-robustness background, and `KT-815` plus `KT-819` map to the existing duplicate-term-declaration negative | continue with `KT-901`, `KT-902`, `KT-903`, `KT-904`, `KT-905` by `idReadable`; keep `KT-247` visible as already-mined safe-navigation coverage, `KT-2127` / `KT-8492` in the ergonomics/spec-follow-up bucket, `KT-2146` in the spec-clarity bucket, `KT-58` in the later finalization bucket, and the `!!` / mutable-local tranche in the intentional-divergence bucket |
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
