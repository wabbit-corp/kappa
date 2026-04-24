# Macro, Hygiene, And Staging Issue Candidates For Kappa

Mined from:

- `../datatron/data-scala-issues/lean4-issues.json`
- `../datatron/data-scala-issues/racket-issues.json`

Keyword scans completed on 2026-04-23:

- Lean 4 full-corpus scan for `macro`, `hygiene`, `quote`, `quotation`, `splice`, `splicing`, `syntax`, `antiquot`, `quasisyntax`, `template`
- Lean 4 focused follow-up scan for `hygiene`, `scope`, `binding`, `identifier`, `double-quoted`, `structure`, `module`, `export`
- Racket full-corpus scan for `macro`, `hygiene`, `quote`, `quotation`, `splice`, `splicing`, `syntax`, `antiquot`, `quasisyntax`, `template`
- Racket focused follow-up scan for `hygiene`, `scope`, `binding`, `identifier`, `gensym`, `renaming`, `rename`, `export`, `module`, `syntax class`, `template`, `splicing`

Direct reads completed on 2026-04-23:

- `lean4#108`, `lean4#414`, `lean4#430`, `lean4#586`, `lean4#793`, `lean4#821`, `lean4#1006`, `lean4#1124`
- `racket#1483`, `racket#1493`, `racket#1746`, `racket#2133`

Purpose: collect macro, hygiene, and staged-code issues that map to Kappa's `Syntax`, `$(...)`, and module-boundary
macro obligations in `§5.8`-`§5.9`, while separating those from parser-framework or macro-engine-specific internals that
do not cleanly survive translation into Kappa's surface.

## Strongest Kappa Fits

| source | bucket | spec_refs | fit | current_coverage | fixture_kind | expected_outcome | proposed_fixture_root | status | notes |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| `lean4#586` | `macros_hygiene` | `§5.8.1`, `§5.8.3`, `§5.8.4` | `direct` | `partial` | `single-file` | `positive-run` | `macros.hygiene.positive_spliced_name_not_captured_by_generated_binder` | `accepted-now` | Staged in `new-tests/`; the Kappa analogue uses a quoted lambda binder so the spliced call-site syntax must keep its own `x` rather than being captured by the generated one. |
| `lean4#793` | `macros_hygiene` | `§5.5.1`, `§5.5.9`, `§5.8.2`, `§5.8.4` | `direct` | `partial` | `single-file` | `positive-run` | `macros.hygiene.positive_generated_dependent_record_type_resolves_inner_name` | `accepted-now` | Staged in `new-tests/`; the Kappa analogue generates a dependent record type whose later field refers to an earlier generated field via `this.a`. |
| `racket#1746` | `macros_modules` | `§2.3`, `§5.8.2`, `§5.8.4` | `indirect` | `partial` | `directory-suite` | `positive-run` | `macros.hygiene.positive_imported_macro_preserves_call_site_binding` | `accepted-now` | Staged in `new-tests/`; the imported macro function is defined in one module and spliced in another, so the call-site binding must still win over the macro's generated binder. |
| `racket#2133` | `macros_modules` | `§2.3`, `§5.8.2`, `§5.8.4` | `indirect` | `partial` | `directory-suite` | `positive-run` | `macros.hygiene.positive_imported_macro_preserves_call_site_binding` | `accepted-now` | Shares the same staged Kappa analogue as `racket#1746`; the focus is module-boundary preservation of imported renamings rather than Racket's exact gensym export surface. |

## Useful But Secondary Fits

| source | bucket | spec_refs | fit | current_coverage | fixture_kind | expected_outcome | proposed_fixture_root | status | notes |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| `lean4#821` | `macros_hygiene` | `§5.5.1`, `§5.8.4` | `indirect` | `partial` | `single-file` | `positive-check` | `macros.hygiene.positive_generated_record_this_reference_survives_splice` | `accepted-later` | Kappa's `this` is record-telescope syntax rather than Lean's anaphoric local `this`, but the staged dependent-record-type fixture is close enough that any additional dedicated import should wait until we need broader generated-record coverage. |

## Poor Fits / Background Only

| source | why it is not a clean Kappa import |
| --- | --- |
| `lean4#108` | Lean's antiquotation-kind propagation problem depends on syntax-category annotations in `macro` declarations. Kappa's macro surface is typed `Syntax t` plus ordinary functions, not category-indexed `macro_rules`, so there is no faithful direct repro yet. |
| `lean4#414` | Same problem as `lean4#108`: antiquotation-only pattern declarations are specific to Lean's `macro_rules` surface, which Kappa does not currently specify. |
| `lean4#430` | Quotation over alternate syntax categories / parsers has no Kappa analogue today because Kappa quotes typed terms and types, not arbitrary parser categories. |
| `lean4#1006` | The failure is about imported macro-generated internal declaration names colliding during environment construction. Kappa currently has no declaration-splicing surface, so a direct analogue would be speculative. |
| `lean4#1124` | The malformed-quotation bug comes from optional antiquotation syntax-category machinery that Kappa does not expose directly. |
| `racket#1483` | `define-syntax-class` scope behavior is a syntax-framework internal, not a direct Kappa surface behavior. |
| `racket#1493` | Template metafunction and internal-definition-context interactions are macro-engine internals with no clean Kappa source analogue today. |

## Staged Fixtures

- `new-tests/macros.hygiene.positive_spliced_name_not_captured_by_generated_binder`
  - source: `lean4#586`
  - purpose: prove that quoted binders are hygienic and do not capture spliced call-site syntax

- `new-tests/macros.hygiene.positive_generated_dependent_record_type_resolves_inner_name`
  - source: `lean4#793`
  - purpose: prove that generated dependent names inside quoted types still resolve correctly after splicing

- `new-tests/macros.hygiene.positive_imported_macro_preserves_call_site_binding`
  - source: `racket#1746`, `racket#2133`
  - purpose: prove that a macro function imported from another module preserves call-site bindings across the module boundary

## Next Focused Pass

- Lean 4: `#249`
- Racket: `#1410`, `#1479`, `#1495`

These are the next exact IDs to read if Wave 6 resumes immediately after the current staged batch.
