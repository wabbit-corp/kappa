# Totality Issue Candidates For Kappa

Mined from:

- `../datatron/data-scala-issues/lean4-issues.json`
- `../datatron/data-scala-issues/Idris2-issues.json`
- `../datatron/data-scala-issues/agda-issues.json`

Direct reads completed on 2026-04-23:

- `lean4#262`, `lean4#281`
- `Idris2#19`, `Idris2#403`, `Idris2#493`
- `agda#3`, `agda#59`, `agda#238`

Purpose: collect totality / termination / helper-lowering issues that map to Kappa's Chapter 6.4 obligations, while
separating those from macro-only or parser-only issues.

## Strongest Kappa Fits

| source | bucket | spec_refs | fit | current_coverage | fixture_kind | expected_outcome | proposed_fixture_root | status | notes |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| `lean4#262` | `totality_termination` | `§6.4.2`, `§6.4.3` | `direct` | `partial` | `single-file` | `positive-check` | `declarations.totality.positive_structural_recursion_on_mutual_family` | `accepted-now` | Staged in `new-tests/`. This case is a direct analogue of mutual-family recursion with structural descent through an index-like family. |
| `lean4#281` | `totality_termination` | `§6.4.2`, `§7.5.1A` | `direct` | `partial` | `single-file` | `positive-run` | `declarations.totality.positive_index_refined_recursive_map` | `accepted-now` | Staged in `new-tests/`; the Kappa analogue uses constructor-forced index refinement in the recursive branch. |
| `Idris2#19` | `totality_termination` | `§6.4.1`, `§16.4` | `direct` | `partial` | `single-file` | `negative-diagnostic` | `declarations.totality.negative_recursive_cycle_rejected` | `accepted-now` | Staged in `new-tests/`; obvious recursive cycles should be rejected during checking. |
| `Idris2#403` | `totality_termination` | `§6.4.2`, `§6.4.5`, `§17.3.1.4` | `direct` | `partial` | `single-file` | `positive-run` | `declarations.totality.positive_with_lowering_preserves_decrease_evidence` | `accepted-now` | Staged in `new-tests/`; the Kappa analogue keeps the branch-split decrease-evidence core while avoiding Idris-specific `with` syntax. |
| `Idris2#493` | `totality_termination` | `§6.4.2`, `§6.3.1`, `§6.4.5` | `direct` | `partial` | `single-file` | `positive-run` | `declarations.totality.positive_let_alias_preserves_decrease_evidence` | `accepted-now` | Staged in `new-tests/`; transparent local aliases should not destroy an inferred structural descent argument. |
| `agda#59` | `totality_termination` | `§6.4.2`, `§6.4.5`, `§17.3.1.4` | `direct` | `partial` | `single-file` | `positive-run` | `declarations.totality.positive_with_helper_preserves_structural_descent` | `accepted-now` | Staged in `new-tests/`; the Kappa analogue uses the explicit helper-lowered form directly. |
| `agda#238` | `totality_termination` | `§6.4.2`, `§6.4.5`, `§17.3.1.4` | `direct` | `partial` | `single-file` | `positive-run` | `declarations.totality.positive_with_lowering_retains_constructor_relation` | `accepted-now` | Staged in `new-tests/`; the Kappa analogue tests descent under a fixed constructor spine. |

## Useful But Secondary Fits

| source | bucket | spec_refs | fit | current_coverage | fixture_kind | expected_outcome | proposed_fixture_root | status | notes |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| `agda#3` | `totality_termination` | `§6.4.5`, `§17.3.1.4` | `indirect` | `none` | `directory-suite` | `positive-check` | `declarations.totality.positive_helper_lowering_preserves_hidden_binders` | `accepted-later` | Not purely a termination bug, but it is strong evidence that generated helper binders and hidden arguments must be preserved exactly across lowering. |

## Companion Spec-Derived Staging

- `new-tests/declarations.totality.positive_mutual_recursion_hidden_phase`
  - source: spec-driven companion for `§6.4.2` and `§6.4.3`
  - purpose: cover the hidden-phase / shared-measure rule directly even though it is not tied to one single mined issue

## Recommended Pull Order

1. `Idris2#493`
2. `Idris2#403`
3. `agda#59` and `agda#238`
4. `lean4#262` and `lean4#281`
5. `Idris2#19`
6. `agda#3`

That order matches Kappa's current totality story: first local aliases and helper-lowering preservation, then broader
family/index recursion cases, then the more policy-like helper-lowering follow-ups.
