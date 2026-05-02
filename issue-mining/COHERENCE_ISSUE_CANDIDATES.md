# Coherence And Solver Issue Candidates For Kappa

Mined from:

- `../datatron/data-scala-issues/chalk-issues.json`

Keyword scans completed on 2026-04-23:

- full-corpus scan for `coherence`, `overlap`, `orphan`, `associated type`, `supertrait`, `ambiguity`, `projection`, `normalize`, `cycle`, `recursive`, `implied bound`

Direct reads completed on 2026-04-23:

- `chalk#2`, `#5`, `#6`, `#8`, `#10`, `#12`, `#13`, `#16`, `#17`, `#42`, `#44`, `#58`, `#62`, `#70`, `#71`, `#74`, `#79`, `#80`, `#85`, `#90`, `#92`, `#111`, `#115`, `#116`, `#144`, `#189`, `#203`, `#214`, `#219`, `#234`, `#235`, `#246`, `#248`, `#250`, `#289`, `#306`, `#313`, `#429`, `#515`, `#716`, `#750`, `#777`

Purpose: collect coherence, supertrait, associated-member, and solver-edge issues that map to Kappa's local implicit
search, trait-instance coherence, and associated-member recovery rules.

## Strongest Kappa Fits

| source | bucket | spec_refs | fit | current_coverage | fixture_kind | expected_outcome | proposed_fixture_root | status | notes |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| `chalk#71` | `supertrait_projection` | `§7.3.3`, `§12.1.1` | `direct` | `partial` | `single-file` | `positive-check` | `expressions.implicit_parameters.positive_supertrait_projection` | `accepted-now` | Staged in `new-tests/`; the Kappa analogue exercises exactly the rule Chalk was missing: an in-scope dictionary for a subtrait must project to its declared supertrait during local implicit resolution. |
| `chalk#235` | `premise_filtered_instance_selection` | `§12.3`, `§12.3.1`, `§15.2.1` | `direct` | `partial` | `single-file` | `positive-run` | `traits.instances.positive_failed_premise_candidate_discarded_before_coherence` | `accepted-now` | Staged in `new-tests/`; the Kappa analogue checks that an overlapping candidate with an unsatisfied premise is discarded before coherence, so a more specific surviving instance still determines the result. |
| `chalk#10` | `coherence_overlap` | `§12.3`, `§12.3.1`, `§15.2.1` | `direct` | `partial` | `single-file` | `negative-diagnostic` | `traits.instances.negative_overlapping_instance_heads_rejected` | `accepted-now` | Staged in `new-tests/`; the same overlap family as the `#235` analogue becomes a real coherence error once the generic candidate's premise also survives. |
| `chalk#515` | `coherence_assoc_overlap` | `§12.2.1`, `§12.3`, `§15.2.1` | `direct` | `partial` | `single-file` | `negative-diagnostic` | `traits.instances.negative_overlap_via_associated_member_equality` | `accepted-now` | Staged in `new-tests/`. This case ensures that associated-member equality in overlapping instance heads still enforces coherence rejection. |
| `chalk#234` | `associated_member_recovery` | `§7.3.3`, `§12.2.1`, `§12.3.1` | `direct` | `partial` | `single-file` | `positive-check` | `traits.instances.positive_associated_member_from_resolved_dictionary` | `imported` | Already staged elsewhere from the Scala tranche; Chalk's ambiguity report is another version of the same underlying obligation that associated-member projection should not stay stuck when the resolved dictionary determines it. |

## Useful But Secondary Fits

| source | bucket | spec_refs | fit | current_coverage | fixture_kind | expected_outcome | proposed_fixture_root | status | notes |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| `chalk#750` | `solver_irrelevant_env_ambiguity` | `§12.3.1`, `§12.2.1` | `indirect` | `none` | `single-file` | `positive-check` | `traits.instances.positive_irrelevant_premise_does_not_block_associated_projection` | `accepted-later` | Valuable later guardrail: unrelated premises should not turn a determined associated-member projection into ambiguity. |
| `chalk#74` | `projection_uniqueness` | `§12.2.1`, `§12.3.1` | `indirect` | `partial` | `single-file` | `positive-check` | `traits.instances.positive_associated_member_from_resolved_dictionary` | `accepted-later` | This is broad solver background rather than a one-line Kappa surface repro, but it reinforces the same projection-uniqueness obligation already represented by the associated-member recovery fixtures. |
| `hylo#1566` | `conditional_supertrait_source_selection` | `§12.1.1`, `§12.3`, `§15.2.1` | `indirect` | `none` | `single-file` | `negative-diagnostic` | `traits.instances.negative_ambiguous_conditional_supertrait_source` | `hold-spec` | Useful later guardrail if Kappa grows richer conditional-conformance/source-selection behavior: once several refinement paths can imply the same weaker trait, the source-selection rule must be explicit rather than implementation-dependent. |

## Poor Fits / Background Only

| source | why it is not a clean Kappa import |
| --- | --- |
| `chalk#8` | Marker-trait partial-overlap behavior does not map directly because Kappa coherence is defined by Section 15.2.1 rather than by Rust-style marker-trait exceptions. |
| `chalk#44` | This is mostly a solver-internals discussion about uniqueness and negative reasoning, not a clean surface-level Kappa regression. |
| `chalk#58` | Chalk's `T::Item`-without-known-trait problem does not map directly because Kappa requires ordinary dictionary projection rather than in-scope-trait guessing for associated members. |
| `chalk#2` | This is a broad umbrella for Chalk's projection and normalization work, not a single importable surface regression. The concrete Kappa-facing obligations are better tracked via `#71`, `#234`, and `#235`. |
| `chalk#5` | Projection occurs-check behavior is solver-internals work rather than a clean Kappa source-level repro. |
| `chalk#6` | Same as `#5`, with extra lazy-normalization solver detail that Kappa does not expose as a direct surface-level test shape. |
| `chalk#16` | Kind checking on lowered Rust trait/struct arguments is a general front-end well-formedness concern, not a specific coherence or solver-edge import for this wave. |
| `chalk#17` | This is about Rust generic associated-type arity checking in impls. Kappa does not currently expose Rust-style generic associated types as a direct surface analogue. |
| `chalk#42` | Cycle handling and tabling strategy are solver-engine concerns, not a clean Kappa surface regression. |
| `chalk#85` | Same divergence as `chalk#58`; it depends on Rust's unselected projection surface rather than Kappa's explicit dictionary/member story. |
| `chalk#111` | This is mostly solver guidance for existentially recovering projection witnesses from an implied `FutureResult` premise. Kappa's current surface does not expose that Rust-style query shape directly, and the closest obligations are already covered by premise-dependency and associated-member-recovery fixtures. |
| `chalk#12` | The supertrait-elaboration side is already represented by the staged `chalk#71` analogue, while the projection-where-clause side depends on Rust-style associated-type equality constraints that are not yet a clean Kappa fixture shape. |
| `chalk#13` | Rust generic associated types are not a direct Kappa import. Kappa's associated static members can be compile-time functions, but that is a different surface and should not be conflated with Rust GAT support. |
| `chalk#62` | Marker-trait overlap is an intentional Rust-specific coherence exception. Kappa coherence does not grant a corresponding marker-trait carve-out. |
| `chalk#79` | Delayed-literal simplification for negative cycles is solver-engine machinery. Kappa does not currently expose user-level negative trait goals that would import cleanly from this issue. |
| `chalk#80` | Subgoal-selection heuristics are solver-performance and search-strategy concerns, not a clean Kappa surface regression. |
| `chalk#203` | Trait-object implied bounds do not have a direct Kappa analogue because Kappa does not currently expose Rust-style `dyn Trait` surfaces. |
| `chalk#219` | Rust specialization and reveal-mode normalization are not part of Kappa's trait model. Kappa coherence is defined by surviving instance dictionaries, not by specialized default-item revelation. |
| `chalk#429` | The interesting part is recursive-solver guidance for existential projection goals with unresolved premises. Kappa's current surface does not present a comparably direct query form, so this is better treated as solver background than as a clean fixture import. |
| `chalk#90` | Choosing between Chalk solver implementations is tool-internal design work, not an importable Kappa semantic issue. |
| `chalk#92` | Rust's open-vs-closed-world negative reasoning model is driven by orphan rules and future external impls. Kappa's instance model is defined over the compilation unit's module closure and does not import that design space directly. |
| `chalk#115` | This is an internal IR / lowering abstraction discussion about Rust where clauses, not a clean Kappa fixture source. |
| `chalk#116` | Rust generic associated types are still the core concern here. Kappa should not treat Chalk's GAT implementation plan as a direct solver/coherence fixture source. |
| `chalk#144` | This is another GAT-specific projection bug. Without a direct Kappa analogue for Rust-style generic associated types, it stays background only. |
| `chalk#189` | Variance of an internal engine structure is embedding/runtime integration work, not surface Kappa semantics. |
| `chalk#214` | Crate reorganization is implementation architecture, not language behavior. |
| `chalk#246` | Fuel budgeting for the Chalk inner loop is solver-engine scheduling work, not a direct Kappa fixture import. |
| `chalk#248` | Coinductive-loop soundness is solver-core logic, not a clean Kappa source-level regression. |
| `chalk#250` | Negative-cycle handling is again solver-core machinery for Rust's internal logic model, not an importable Kappa fixture. |
| `chalk#289` | This is an IDE / performance / OOM report rather than a language-semantic issue. |
| `chalk#306` | Environment truncation loops are solver implementation details, not a direct Kappa semantic obligation. |
| `chalk#313` | Coinductive self-cycle detection is another engine-internal follow-up, not a clean surface import. |
| `chalk#70` | This is another Rust generic-associated-type / mixed-kind crash report. Kappa should not import it directly into the current solver/coherence wave. |
| `chalk#716` | This is about identifying the concrete method body selected for Rust trait dispatch. Kappa specifies dictionary member selection, not a separate method-identity normalization query. |
| `chalk#777` | Same reason as `#203`, plus the issue depends on trait-object projection from supertrait bounds rather than ordinary dictionary projection. |

## Staged Fixtures

- `new-tests/expressions.implicit_parameters.positive_supertrait_projection`
  - source: `chalk#71`
  - purpose: prove that a local implicit dictionary for a subtrait can satisfy a supertrait goal by projection during local search

- `new-tests/traits.instances.positive_failed_premise_candidate_discarded_before_coherence`
  - source: `chalk#235`
  - purpose: prove that an overlapping candidate with an unsatisfied premise is discarded before coherence, so a surviving direct instance still resolves uniquely

- `new-tests/traits.instances.negative_overlapping_instance_heads_rejected`
  - source: `chalk#10`
  - purpose: prove that once two overlapping candidates both survive premise solving, Section 15.2.1 requires rejection

## Next Focused Pass

- `chalk#318`
- `chalk#335`
- `chalk#351`
- `chalk#369`
- `chalk#399`

These are the next exact IDs to read if Wave 8 resumes immediately after the current staged batch.
