# Ownership Issue Candidates For Kappa

Mined from:

- `../datatron/data-scala-issues/rust-issues.json`
- `../datatron/data-scala-issues/hylo-issues.json`
- `../datatron/data-scala-issues/granule-issues.json`

Search coverage for this note:

- `rust`: full-corpus keyword scan for `borrow`, `lifetime`, `closure capture`, `capture`, `disjoint`, `move`, `last use`
- `hylo`: full-corpus keyword scan for `borrow`, `lifetime`, `capture`, `inout`, `accessor`, `alias`
- `granule`: full-corpus keyword scan for `linear`, `linearity`, `nonlinear`, `case`, `match`, `usage`
- direct body reads so far:
  - `rust#925`, `rust#1243`, `rust#1399`, `rust#1455`, `rust#1566`, `rust#1818`, `rust#1894`, `rust#2041`, `rust#2329`, `rust#8636`, `rust#14273`, `rust#27889`, `rust#30745`
  - `hylo#3`, `hylo#39`, `hylo#179`, `hylo#410`, `hylo#424`, `hylo#571`, `hylo#674`, `hylo#675`, `hylo#676`, `hylo#688`, `hylo#689`, `hylo#814`, `hylo#850`, `hylo#878`, `hylo#1088`, `hylo#1510`, `hylo#1512`, `hylo#1523`, `hylo#1534`, `hylo#1599`, `hylo#1600`, `hylo#1707`, `hylo#1807`
  - `granule#4`, `granule#9`, `granule#17`, `granule#23`, `granule#37`, `granule#38`, `granule#42`, `granule#45`, `granule#48`, `granule#52`, `granule#53`, `granule#54`, `granule#56`, `granule#59`, `granule#65`, `granule#195`, `granule#207`, `granule#252`

Purpose: identify ownership, quantity, borrow-lifetime, and path-sensitive resource issues that map cleanly onto
Kappa's existing §§5.1.5-5.1.7 rules, while separating those from source-language-specific aliasing or implementation
details that Kappa does not surface directly.

## Accepted Now

| source | bucket | spec_refs | fit | current_coverage | fixture_kind | expected_outcome | proposed_fixture_root | status | notes |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| `rust#1243`, `rust#1399` | `ownership_quantities_closures` | `§5.1.5`, `§7.2.1` | `direct` | `partial` | `single-file` | `negative-diagnostic` | `expressions.lambdas.negative_capture_prevents_earlier_last_use` | `accepted-now` | Staged in `new-tests/`; a closure capture must keep the captured value live, so an earlier consume/move cannot be treated as a final use. |
| `rust#30745` | `ownership_quantities_closures` | `§5.1.5`, `§7.2.1`, `§7.5` | `direct` | `partial` | `single-file` | `negative-diagnostic` | `expressions.lambdas.negative_match_capture_makes_closure_linear` | `accepted-now` | Staged in `new-tests/`; moving through a captured pattern match must still make the closure linear, not silently leave it unrestricted. |
| `rust#8636` | `disjoint_path_borrowing` | `§5.1.7`, `§7.5`, `§7.6` | `direct` | `imported` | `single-file` | `positive-check` | `types.universes.disjoint_path_borrowing_records.positive_disjoint_sibling_borrows` | `accepted-now` | Already covered in the real fixture suite; sibling path borrows from one root stay disjoint instead of collapsing into one undifferentiated alias set. |
| `rust#27889` | `constructor_refinement_disjointness` | `§5.1.7`, `§7.4.1`, `§2.8.3` | `direct` | `partial` | `single-file` | `positive-check` | `expressions.conditionals.positive_constructor_test_disambiguates_projection` | `accepted-now` | Already staged elsewhere in `new-tests/`; constructor knowledge should make same-named fields from different variants non-conflicting at the use site. |
| `hylo#1807` | `borrow_escape` | `§5.1.6`, `§7.2.1` | `direct` | `imported` | `directory-suite` | `negative-diagnostic` | `borrow_capture_escape.*` | `accepted-now` | Already covered in the real fixture suite by the anonymous-borrow-escape negatives and the local non-escaping closure positive. |
| `hylo#1088` | `local_function_capture` | `§6.3.1`, `§7.2.1`, `§14.1.1` | `direct` | `partial` | `single-file` | `positive-check` | `declarations.let_in.positive_local_function_capture` | `accepted-now` | Staged in `new-tests/`; a block-local named function should closure-convert over free variables from the surrounding lexical scope. |
| `hylo#674` | `local_generic_context_capture` | `§6.3.1`, `§11.3`, `§14.1.1` | `direct` | `partial` | `single-file` | `positive-check` | `declarations.let_in.positive_local_type_alias_captures_outer_type_parameter` | `accepted-now` | Staged in `new-tests/`; this issue family turned out to be local generic-context capture rather than ownership proper, but it was discovered in the ownership pass and belongs in the same local-declaration tranche. |
| `hylo#878` | `local_capture_pattern_binding` | `§6.1.2`, `§6.3.1`, `§14.1.1` | `direct` | `partial` | `single-file` | `positive-check` | `declarations.let_in.positive_pattern_bound_local_capture` | `accepted-now` | Staged in `new-tests/`; a later local declaration should capture names introduced by an earlier irrefutable local pattern binding just as it captures any other free variable. |
| `granule#4` | `ownership_quantities` | `§5.1.5`, `§5.1.5.1` | `direct` | `imported` | `single-file` | `negative-diagnostic` | `types.universes.quantities.qtt.negative_quantity0_runtime_use` | `accepted-now` | Existing real fixture already covers the basic “erased value used at runtime” failure mode. |
| `granule#42` | `ownership_quantities` | `§5.1.5`, `§5.1.5.1`, `§7.2.1` | `direct` | `imported` | `single-file` | `negative-diagnostic` | `types.universes.quantities.qtt.negative_linear_alias_duplication` | `accepted-now` | Existing real fixtures already cover duplicated use of a linear value both through explicit aliasing and through unrestricted closure reuse. |
| `granule#54` | `ownership_branch_join` | `§5.1.5` | `direct` | `partial` | `single-file` | `negative-diagnostic` | `types.universes.quantities.qtt.negative_condition_consumes_before_branch_reuse` | `accepted-now` | Staged in `new-tests/`; a condition that consumes a linear value must make that value unavailable in every branch. |
| `granule#195` | `ownership_branch_join` | `§5.1.5` | `direct` | `partial` | `single-file` | `negative-diagnostic` | `types.universes.quantities.qtt.negative_branch_drop` | `accepted-now` | Staged in `new-tests/`; using a linear value in only one branch is still a drop on the other branch. |
| `granule#56` | `match_erasure_ownership` | `§5.1.5`, `§7.5.1B` | `direct` | `partial` | `single-file` | `negative-diagnostic` | `types.universes.quantities.qtt.negative_match_on_erased_scrutinee` | `accepted-now` | Staged in `new-tests/`; matching on a quantity-`0` scrutinee without independent forcing evidence is forbidden runtime discrimination. |

## Accepted Later / Hold

| source | bucket | spec_refs | fit | current_coverage | fixture_kind | expected_outcome | proposed_fixture_root | status | notes |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| `hylo#1510`, `hylo#1512` | `mutable_iteration_borrow_surface` | `§6.1.1`, `§8.7.5`, `§8.8.3` | `indirect` | `none` | `single-file` | `mixed` | `effects.for_loops.*` | `hold-spec` | Kappa does not currently expose `for inout`-style mutable iteration. If it adds such sugar later, the borrowed root and yielded element access mode should be explicit, and lowering must preserve them instead of silently coercing mutable elements to plain values. |
| `hylo#1599` | `inout_projection_aliasing` | `§5.1.7.2`, `§8.8.3` | `indirect` | `none` | `single-file` | `negative-diagnostic` | `effects.inout.negative_projection_alias_outlives_call` | `accepted-later` | The safety concern is real, but Kappa's `inout` model is linear state threading rather than a first-class returned mutable reference, so the first ownership batch should not pretend the surface forms line up exactly. |
| `hylo#1600` | `projection_sink_argument_consumption` | `§5.1.5`, `§6.1.1`, `§8.8.3` | `indirect` | `partial` | `single-file` | `negative-diagnostic` | `effects.inout.negative_projection_sink_argument_not_consumed` | `accepted-later` | Useful evidence that projection/subscript lowering must preserve consumption of sink/owned arguments, but Kappa's current projection surface differs enough that this belongs in the later projection / `inout` tranche rather than the first ownership batch. |
| `hylo#571` | `projection_lifetimes` | `§5.1.7.2`, `§8.8.3` | `indirect` | `none` | `directory-suite` | `mixed` | `effects.inout.*` | `accepted-later` | Good background for projection-call lifetime lowering and overlap checks, but too implementation-shaped for the first ownership batch. |

## Companion Spec-Derived Staging

- `new-tests/declarations.let_in.positive_local_function_depends_on_earlier_local`
  - source: spec-driven companion for `§14.1.1`
  - purpose: cover a later local declaration depending on an earlier local declaration that already captures outer lexical state

## Rejected / Poor Fits For Now

- `hylo#3`
  - internal type-checker overlapping-access bug; useful implementation background, but not a source-level Kappa fixture.

- `hylo#814`
  - better treated as a name-resolution / local-capture bug than an ownership issue.

- `hylo#850`
  - backend / LLVM-attribute discussion, not a source-level ownership or local-capture regression.

- `hylo#39`
  - design discussion about ownership spelling and overload ergonomics, not a concrete Kappa fixture import.

- `hylo#179`
  - naming/clarity complaint in compiler internals, not a language-semantics fixture.

- `hylo#410`
  - depends on mixed-introducer pattern forms such as `(inout x, let y)` that are not a current Kappa surface fit.

- `hylo#424`
  - parser infinite loop for a capture-list-heavy surface syntax, not a meaningful Kappa semantic import.

- `hylo#1707`
  - terminology / design question rather than a source-level bug.

- `hylo#1534`
  - backend / emitter crash around iterator lowering. Kappa already has direct iterator-loop semantic coverage, so this is
    better treated as background evidence than as a fresh ownership fixture.

- `hylo#675`
  - implementation crash around builtin pointer subscripts and mutation, not a clean source-level Kappa regression.

- `hylo#676`
  - same class as `#675`: backend / emitter crash around a rich subscript surface, not a crisp Kappa fixture source.

- `hylo#688`
  - overload-resolution corner around `inout`; better treated as spec-clarity follow-up for the `~` marker and overload selection than as an ownership fixture.

- `hylo#689`
  - same class as `#688`: useful as a prompt for a worked `inout` overload example, but weak as a standalone ownership test.

- `granule#23`
  - repeated-variable pattern policy belongs to Kappa's pattern-formation rules, not primarily to the ownership wave.

- `granule#9`
  - feature request for length-indexed lists, not a regression in an existing Kappa ownership rule.

- `granule#37`
  - primarily a diagnostic-shape complaint about a source-language-specific modal-pattern spelling.

- `granule#38`
  - about tighter quantitative inference / exact resource bounds rather than a crisp v0.1 ownership regression.

- `granule#207`
  - theory / design discussion about uniqueness results, not a direct Kappa regression.

- `granule#45`
  - better treated as ordinary exhaustiveness coverage; Kappa already has direct non-exhaustive and indexed-exhaustive fixtures outside the ownership wave.

- `granule#48`
  - broad design discussion about classes and records, not a concrete Kappa regression import.

- `granule#52`
  - discussion of a falsifiable resource-bound example, closer to quantitative inference tuning than to a new ownership conformance test.

- `granule#53`
  - test-harness/process note rather than a language issue.

- `granule#59`
  - asks for an infinity/coeffect extension rather than exercising Kappa's current quantity rules; v0.1 keeps such work in
    the reserved modal-extension lane instead of the core ownership suite.

- `granule#65`
  - partly diagnostic-shaped, and the core wildcard obligation is already represented by
    `types.universes.quantities.qtt.positive_underscore_pattern_is_no_borrow`; the remaining difference is source-language-specific.

- `granule#252`
  - mixes an ordinary linear-reuse complaint with a source-language Prelude/import crash, so it is weak as a clean Kappa fixture source.

- `rust#925`
  - optimization request about detecting last-use moves, not a distinct source-level Kappa semantic obligation.

- `rust#1455`
  - specific to Rust task spawning and static-item treatment, with no useful Kappa analogue.

- `rust#1566`
  - compiler crash on nested closures, not a source-level semantic regression.

- `rust#14273`
  - mostly a diagnostic wording issue; useful background, but not a strong fixture import target.

- `rust#1818`
  - move-optimization detail for anonymous functions; useful background, but Kappa's source-level ownership rules are already better captured by the explicit linear-capture fixtures.

- `rust#1894`
  - depends on Rust's explicit closure-capture modes and pointer-identity expectations; Kappa has no corresponding
    capture-clause surface to import directly.

- `rust#2041`
  - pure optimization-policy discussion, not a new source-level ownership obligation.

- `rust#2329`
  - useful background for value-binding from variant patterns, but Rust's proposed `alt move` surface does not map
    directly to Kappa and the current match/linearity suite already exercises payload binding under pattern matching.

## Recommended Pull Order

1. merged `rust#1243` + `rust#1399`
2. `rust#30745`
3. `granule#56`
4. `granule#54` and `granule#195`
5. `rust#8636` and `rust#27889` as the imported / already-staged disjointness anchors
6. `hylo#1807` as the imported borrow-escape anchor
7. `hylo#1088`, `hylo#674`, and `hylo#878` as the local-declaration / capture anchors, then `hylo#1599` and `hylo#571` after the projection and `inout` surfaces are ready for tighter analogues
