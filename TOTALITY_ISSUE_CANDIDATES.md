# Totality Issue Candidates For Kappa

Mined from:

- `../datatron/data-scala-issues/lean4-issues.json`
- `../datatron/data-scala-issues/Idris2-issues.json`
- `../datatron/data-scala-issues/agda-issues.json`

Direct reads completed on 2026-04-23:

- `lean4#262`, `lean4#281`
- `Idris2#19`, `Idris2#403`, `Idris2#493`
- `agda#3`, `agda#59`, `agda#238`

Cross-linked later reads:

- `Idris2#163` remains tracked in `DEPENDENT_PROOF_ISSUE_CANDIDATES.md` as a totality-checker hang/performance item.
- `Idris2#202` is staged in `DEPENDENT_PROOF_ISSUE_CANDIDATES.md` as an indexed exhaustiveness negative rather than a
  separate totality fixture, because the transferable Kappa obligation is the refined-match coverage rule of §7.5.1 /
  §17.3.6.
- `Idris2#300` is the same alias-preserves-descent family as `Idris2#493`; it is already covered by the staged
  `declarations.totality.positive_let_alias_preserves_decrease_evidence` fixture rather than needing a second totality
  test for parameter-alias spelling specifically.
- `Idris2#360` is a later mutual-interface / dictionary-recursion totality read. Keep it visible for the next totality
  tranche rather than duplicating it in the dependent-proof note.
- `Idris2#374` is a later well-founded-relation / implicit-resolution read around the standard `WellFounded` surface.
  Keep it visible for the next totality / well-foundedness tranche rather than duplicating it in the dependent-proof note.
- `Idris2#458` is a later totality-checker divergence / assertion-safety read: even if a recursive definition is
  incorrectly marked total, using it in a type must still fail deterministically instead of making elaboration loop.
  Keep it visible for the next totality / assertion-safety tranche rather than duplicating it in the dependent-proof
  note.
- `Idris2#524` is a later positivity/totality soundness read: uses of a rejected non-strictly-positive family must not
  be allowed back into the trusted totality fragment through ordinary recursive definitions. Keep it visible for the
  next totality / positivity-soundness tranche rather than duplicating it in the dependent-proof note.
- `Idris2#645` is a later totality-checker performance / pessimism read: the checker should reject or accept recursive
  definitions without pathologically slow exploration or obviously avoidable pessimism on mutually recursive size-style
  families. Keep it visible for the next totality / performance tranche rather than duplicating it in the
  dependent-proof note.
- `Idris2#653` is a later helper-lowering / nontermination-performance read: a non-terminating helper introduced by a
  `with`-style lowering or equivalent branch split must fail deterministically instead of making the checker loop or
  leak. Keep it visible for the next totality / helper-lowering-performance tranche rather than duplicating it in the
  dependent-proof note.
- `Idris2#654` is a later partiality / helper-lowering policy read: if a language supports explicit partiality markers,
  helper-lowered `with`-style forms should inherit that partiality policy consistently instead of re-checking the
  lowered helper as if it had to satisfy the enclosing default totality mode. Keep it visible for the next totality /
  helper-lowering-policy tranche rather than duplicating it in the dependent-proof note.
- `Idris2#660` is a later positivity-check precision read: strict-positivity checking should accept genuinely positive
  recursive families instead of conservatively rejecting them as non-positive. Keep it visible for the next totality /
  positivity-precision tranche rather than duplicating it in the dependent-proof note.
- `Idris2#794` is a later totality / case-split precision read around normalized list views and append-shaped indices:
  view-like normalization and helper-generated case splits must not produce false-positive totality results or bogus
  impossible branches when the indexed family still distinguishes real constructor cases. Keep it visible for the next
  totality / coverage-precision tranche rather than duplicating it in the dependent-proof note.

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
