# Dependent Proof / Elaboration Issue Candidates For Kappa

Mined from:

- `../datatron/data-scala-issues/Idris2-issues.json`

Direct reads completed on 2026-04-26:

- `Idris2#13-#17`
- `Idris2#18-#22`
- `Idris2#23-#27`

Keyword scan completed on 2026-04-26 against `Idris2-issues.json` titles and bodies:

- `implicit`, `metavariable`, `elaboration`, `hole`, `coverage`, `normalization`, `erased`, `universe`, `transport`, `equality`, `with`, `total`, `termination`

Notes:

- The earliest available issue numbers in the local Idris2 archive start at `#13`; no `#1-#12` issues are present in
  this snapshot.

Purpose: collect Idris2 dependent-proof, elaboration, hole, and normalization issues that map onto Kappa's explicit
implicit-parameter, elaboration, and dependent-typing rules, while separating those from platform-build, performance,
and repo-management noise.

## Strongest Kappa Fits

| source | bucket | spec_refs | fit | current_coverage | fixture_kind | expected_outcome | proposed_fixture_root | status | notes |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| `Idris2#14` | `implicit_lambda_binders` | `§7.2.1`, `§7.3` | `direct` | `none` | `single-file` | `positive-run` | `expressions.implicit_parameters.positive_lambda_binds_implicit_argument_in_context` | `accepted-now` | Staged in `new-tests/`; a lambda used in expression context should be allowed to introduce an implicit binder explicitly and then elaborate under the contextual function type. |
| `Idris2#20` | `local_let_layout_without_ascription` | `§3.4`, `§6.3.1` | `direct` | `partial` | `single-file` | `positive-run` | `declarations.let_in.positive_multiline_initializer_without_ascription` | `accepted-now` | Staged in `new-tests/`; a local `let` without a separate local signature should still accept a multiline indented initializer and then bind the resulting value in the surrounding block. |
| `Idris2#27` | `residual_catch_all_constructor_refinement` | `§7.4.1`, `§7.5.4`, `§2.8.3` | `direct` | `none` | `single-file` | `positive-run` | `expressions.match.positive_residual_catch_all_derives_remaining_constructor` | `accepted-now` | Staged in `new-tests/`; after an unguarded `None` branch fails, the residual catch-all branch may use the remaining `Some`-like constructor refinement for projection. |

## Background / Poor Fits

- `Idris2#13`
  - tracker / repo-management issue only. It is not a language or compiler-semantics candidate.

- `Idris2#15`
  - dependent elaboration / metavariable-cycle robustness background only for now. It points at solver/elaboration
    fragility, but the issue body does not yet provide a compact transferable source rule for a Kappa conformance
    fixture.

- `Idris2#16`
  - runtime complexity / performance background only. This is useful as implementation pressure, not as a new Kappa
    source-level acceptance rule.

- `Idris2#17`
  - Windows build / FFI-platform background only.

- `Idris2#18`
  - REPL code-generation / derive-definition tooling background only. It is not a direct source-semantics obligation for Kappa.

- `Idris2#19`
  - already tracked in `TOTALITY_ISSUE_CANDIDATES.md` as a direct totality / termination fit; keep it cross-linked there rather than duplicating the staging logic in this note.

- `Idris2#21`
  - hole-display / REPL tooling robustness background only. It reinforces the importance of elaboration tooling quality, but does not add a current conformance fixture.

- `Idris2#22`
  - imported-definition normalization / ambiguous-name reduction background only. The transferable concern is real, but the issue is phrased as REPL evaluation behavior rather than a compact current Kappa source rule.

- `Idris2#23`
  - REPL reload timestamp / file-watch tooling background only.

- `Idris2#24`
  - import-order-sensitive elaboration / normalization background only for now. It is a real implementation-stability concern, but the issue body does not isolate a compact current Kappa source rule.

- `Idris2#25`
  - library API ergonomics background only. It is about `Either` branch convention, not a current Kappa source-semantics obligation.

- `Idris2#26`
  - good evidence for a future spec-clarity note around branch-local normalization under ruled-out patterns, but not yet a clean
    current-spec direct-fit fixture. Kappa explicitly specifies residual negative constructor evidence and remaining-constructor
    refinement, but it does not yet spell out the stronger definitional-equality normalization that this Idris2 issue wants.

## Current Coverage Map

- `new-tests/expressions.implicit_parameters.positive_lambda_binds_implicit_argument_in_context`
  - source: `Idris2#14`; a lambda nested inside another expression may still introduce an explicit implicit binder and
    elaborate against a contextual higher-order function type.

- `new-tests/declarations.let_in.positive_multiline_initializer_without_ascription`
  - source: `Idris2#20`; a block-local `let` should accept a multiline continued initializer without needing a local type declaration solely to disambiguate layout.

- `new-tests/expressions.match.positive_residual_catch_all_derives_remaining_constructor`
  - source: `Idris2#27`; a residual unguarded catch-all branch after `None` may use the remaining visible `Some`-like
    constructor refinement for payload projection.

## Next Focused Pass

- `Idris2#28`
- `Idris2#29`
- `Idris2#30`
- `Idris2#31`
- `Idris2#32`

Notes for the next pass:

- `Idris2#26` should stay visible as a spec-clarity / normalization follow-up even though it is not yet staged as a current-spec test.
