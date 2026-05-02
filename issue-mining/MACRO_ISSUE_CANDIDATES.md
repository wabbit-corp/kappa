# Macro, Hygiene, And Staging Issue Candidates For Kappa

Mined from:

- `../datatron/data-scala-issues/lean4-issues.json`
- `../datatron/data-scala-issues/racket-issues.json`

Keyword scans completed on 2026-04-23:

- Lean 4 full-corpus scan for `macro`, `hygiene`, `quote`, `quotation`, `splice`, `splicing`, `syntax`, `antiquot`, `quasisyntax`, `template`
- Lean 4 focused follow-up scan for `hygiene`, `scope`, `binding`, `identifier`, `double-quoted`, `structure`, `module`, `export`
- Lean 4 focused early-issue follow-up filter for `error message`, `panic`, `dbgTrace`, `trace`, `module`, `symbol`, `equation`, `elaborat`, `delab`, `shadow`, `linker`
- Racket full-corpus scan for `macro`, `hygiene`, `quote`, `quotation`, `splice`, `splicing`, `syntax`, `antiquot`, `quasisyntax`, `template`
- Racket focused follow-up scan for `hygiene`, `scope`, `binding`, `identifier`, `gensym`, `renaming`, `rename`, `export`, `module`, `syntax class`, `template`, `splicing`

Direct reads completed on 2026-04-23:

- `lean4#108`, `lean4#414`, `lean4#430`, `lean4#586`, `lean4#793`, `lean4#821`, `lean4#1006`, `lean4#1124`
- `racket#1483`, `racket#1493`, `racket#1746`, `racket#2133`

Additional direct reads completed on 2026-04-28:

- `lean4#249`
- `lean4#99`, `lean4#181`, `lean4#391`
- `lean4#174`, `lean4#180`, `lean4#182`
- `lean4#184`, `lean4#317`, `lean4#447`
- `lean4#446`, `lean4#465`, `lean4#485`
- `lean4#451`, `lean4#452`, `lean4#494`
- `lean4#308`, `lean4#362`, `lean4#375`
- `lean4#111`, `lean4#191`, `lean4#242`
- `lean4#233`, `lean4#240`, `lean4#529`
- `lean4#312`, `lean4#366`, `lean4#453`
- `lean4#177`, `lean4#190`, `lean4#192`, `lean4#193`, `lean4#198`, `lean4#255`
- `lean4#239`, `lean4#247`, `lean4#250`
- `lean4#2`, `lean4#219`, `lean4#243`
- `lean4#29`, `lean4#30`, `lean4#67`
- `lean4#196`, `lean4#217`, `lean4#220`
- `lean4#333`, `lean4#335`, `lean4#346`
- `lean4#337`, `lean4#348`, `lean4#350`
- `lean4#351`, `lean4#352`, `lean4#355`

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
| `lean4#249` | `macros_scoped_activation` | `§2.3`, `§5.8.2`, `§5.8.4` | `indirect` | `none` | `note-only` | `spec-addition` | `n/a` | `accepted-later` | Kappa does not currently have Lean-style `scoped macro_rules`, but this is useful evidence for a future lexical scoped macro / notation activation surface tied to modules or namespaces rather than a global parser mutation. |
| `lean4#174` | `macros_syntax_views` | `§5.8.1`, `§5.8.5` | `indirect` | `none` | `note-only` | `spec-addition` | `n/a` | `accepted-later` | Kappa does not currently expose Lean-style syntax-pattern quotations, but this is good evidence that any future source-like `Syntax` view or pattern API should represent optional subforms directly instead of forcing duplicated patterns. |
| `lean4#180` | `macros_linting` | `§5.8.4`, `§17.2.5` | `indirect` | `none` | `note-only` | `spec-addition` | `n/a` | `accepted-later` | Definition-site linting for obvious precedence hazards is a useful future requirement even though Kappa does not currently ship a dedicated macro declaration surface. |
| `lean4#181` | `macros_local_parsers` | `§2.3`, `§5.8.2`, `§5.8.4` | `indirect` | `none` | `note-only` | `spec-addition` | `n/a` | `accepted-later` | Kappa does not currently mutate parser tables while parsing, but this is useful evidence that any future scoped macro / notation activation surface should account for recursive or locally activated syntax helpers without requiring global parser-state mutation. |
| `lean4#182` | `macros_diagnostic_rendering` | `§5.8.1`, `§17.1.6`, `§17.2.5` | `indirect` | `none` | `note-only` | `spec-addition` | `n/a` | `accepted-later` | Diagnostics and macro-expansion traces should render generated syntax through the ordinary formatter/parenthesizer and hide raw hygiene suffixes by default. |
| `lean4#184` | `macros_tracing` | `§5.8.4`, `§17.1.6`, `§17.2.5` | `indirect` | `none` | `note-only` | `spec-addition` | `n/a` | `accepted-later` | Future macro/tooling observability should let user-authored macro or elaboration helpers contribute deterministic trace events associated with the current splice or macro invocation. |
| `lean4#446` | `macros_diagnostic_rendering` | `§5.8.1`, `§17.1.6`, `§17.2.5` | `indirect` | `none` | `note-only` | `spec-addition` | `n/a` | `accepted-later` | The same rendering rule should degrade gracefully on partial/incomplete syntax shown in traces instead of panicking inside the parenthesizer. |
| `lean4#451` | `macros_layout_integration` | `§3.4`, `§5.8.4` | `indirect` | `none` | `note-only` | `spec-addition` | `n/a` | `accepted-later` | If Kappa later adds trailing-parser or block-like syntax customizations, they should inherit the ordinary continuation/indentation rules instead of parsing later indented lines as application continuations. |
| `lean4#452` | `macros_layout_integration` | `§3.4`, `§5.8.4` | `indirect` | `none` | `note-only` | `spec-addition` | `n/a` | `accepted-later` | Future `|`-delimited syntax extensions should use the same column-sensitive layout discipline as built-in match-like forms so nested alternatives do not swallow following clauses. |
| `lean4#465` | `macros_local_delaboration` | `§2.3`, `§5.8.1`, `§17.2.5` | `indirect` | `none` | `note-only` | `spec-addition` | `n/a` | `accepted-later` | If Kappa later exposes custom notation/delaboration hooks, they should work for locally bound heads too, not only globally named declarations. |
| `lean4#485` | `macros_extensible_pretty_printing` | `§5.8.1`, `§17.2.5` | `indirect` | `none` | `note-only` | `spec-addition` | `n/a` | `accepted-later` | If Kappa later adds extension-defined syntax categories or parser extensions, formatter/parenthesizer support should apply uniformly rather than being built-in-only. |
| `lean4#494` | `macros_extensible_pretty_printing` | `§5.8.1`, `§17.2.5` | `indirect` | `none` | `note-only` | `spec-addition` | `n/a` | `accepted-later` | Extension-defined quotation syntax that adjusts quote depth should still pretty-print through the ordinary rendering pipeline instead of falling back to raw core output. |
| `lean4#308` | `macros_syntax_abbreviation_equivalence` | `§5.8.1`, `§5.8.4` | `indirect` | `none` | `note-only` | `spec-addition` | `n/a` | `accepted-later` | If Kappa later adds syntax abbreviations or alias categories, they should behave like true aliases of the expanded form rather than triggering recursive expansion/stack-overflow behavior. |
| `lean4#362` | `macros_partial_syntax_tooling` | `§17.2.5`, `§17.2.8` | `indirect` | `none` | `note-only` | `spec-addition` | `n/a` | `accepted-later` | Editor-oriented elaboration and semantic-query services should still make bounded progress on partial syntax trees instead of suppressing completion/hover information after one syntax error. |
| `lean4#375` | `macros_signature_rendering` | `§5.1`, `§5.3`, `§17.2.5` | `indirect` | `none` | `note-only` | `spec-addition` | `n/a` | `accepted-later` | Hover/query output should prefer source-like telescope/signature rendering over naively printing the whole type as a raw term to the right of `:`. |
| `lean4#191` | `macros_local_where_scope` | `§6.3.1`, `§14.1.1` | `indirect` | `none` | `note-only` | `spec-addition` | `n/a` | `accepted-later` | If Kappa later adds trailing `where`-style local-definition sugar for named declarations or clause groups, helper scope should cover the whole attached definition group. |
| `lean4#242` | `macros_definition_validation` | `§5.8.4`, `§17.2.5` | `indirect` | `none` | `note-only` | `spec-addition` | `n/a` | `accepted-later` | Macro-definition validation should reject declarations that name built-in literal/token spellings the host lexer/parser will never dispatch through the macro surface. |
| `lean4#529` | `macros_scoped_activation` | `§2.3`, `§5.8.2`, `§5.8.4` | `indirect` | `none` | `note-only` | `spec-addition` | `n/a` | `accepted-later` | If Kappa later adds local scoped activation forms analogous to `open ... in ...`, parser/quotation services should see the same scoped syntax customizations that ordinary name resolution sees in that lexical region. |
| `lean4#366` | `macros_local_policy_scope` | `§5.8.2`, `§17.1`, `§17.2` | `indirect` | `none` | `note-only` | `spec-addition` | `n/a` | `accepted-later` | Kappa does not currently have Lean-style `set_option ... in`, but this is useful evidence that any future lexical checking/tracing policy overrides should be visible inside nested tactic, macro, and elaboration contexts within that scoped region. |
| `lean4#453` | `macros_extension_surface_consistency` | `§5.8.3`, `§17.2.5` | `indirect` | `none` | `note-only` | `spec-addition` | `n/a` | `accepted-later` | If Kappa later exposes registered tactic, DSL, or elaboration hooks beyond ordinary `$(...)`, parser reachability, help/docs, and handler registration should stay in sync so there is no dangling elaborator with no user-invocable surface form. |
| `lean4#239` | `macros_semantic_query_baseline` | `§5.1`, `§17.2.5` | `indirect` | `none` | `note-only` | `spec-addition` | `n/a` | `accepted-later` | Hover/info services should answer local-variable and binder type queries, not just top-level names. |
| `lean4#247` | `macros_desugaring_diagnostics` | `§8.2`, `§17.2.4`, `§17.2.5` | `indirect` | `none` | `note-only` | `spec-addition` | `n/a` | `accepted-later` | Diagnostics for monadic lifting and similar elaboration rewrites should explain the source-level repair instead of leaking an out-of-scope binder from the lowered helper form. |
| `lean4#250` | `macros_interface_metadata` | `§6.4`, `§17.1.9`, `§17.2.12` | `indirect` | `none` | `note-only` | `spec-addition` | `n/a` | `accepted-later` | Interface artifacts should preserve explicit recursive-group membership metadata even for partial/unsafe groups, not only for total-certified recursive SCCs. |
| `lean4#2` | `macros_placeholder_diagnostics` | `§17.2.4`, `§17.2.4.2`, `§17.2.5` | `indirect` | `none` | `note-only` | `spec-addition` | `n/a` | `accepted-later` | Placeholder diagnostics should preserve the original local goal/context of the source `_` instead of reporting an internal helper metavariable introduced by abstraction or elaboration lowering. |
| `lean4#219` | `macros_partial_delaboration_fallback` | `§5.8.1`, `§17.2.5`, `Appendix T` | `indirect` | `none` | `note-only` | `spec-addition` | `n/a` | `accepted-later` | If a lowered matcher/helper is only partially applied, delaboration should fall back to an ordinary application rendering instead of panicking while trying to reconstruct a source-like `match`. |
| `lean4#243` | `macros_shadowing_diagnostics` | `§2.8.1`, `§6.3`, `§17.2.4`, `§17.2.5` | `indirect` | `none` | `note-only` | `spec-addition` | `n/a` | `accepted-later` | Pattern binders that shadow a visible global declaration should either trigger a clear dedicated warning/error or be distinguished explicitly in diagnostics rather than producing two visually identical names with different identities. |
| `lean4#29` | `macros_lowering_diagnostics` | `§6.4.5`, `§17.2.4`, `§17.2.5` | `indirect` | `none` | `note-only` | `spec-addition` | `n/a` | `accepted-later` | Diagnostics after recursion/elaboration lowering should blame the source clause/equation structure rather than reporting that a generated split clause was unused as if the user had written it directly. |
| `lean4#30` | `macros_module_artifact_identity` | `§2.3`, `§15`, `§17.2.12`, `§17.3.4` | `indirect` | `none` | `note-only` | `spec-addition` | `n/a` | `accepted-later` | Module-name spelling, including case, should stay consistent across imports, generated artifacts, and exported initializer/symbol names instead of drifting between phases. |
| `lean4#67` | `macros_failure_trace_flush` | `§17.1.6`, `§17.2.5`, `§17.2.8` | `indirect` | `none` | `note-only` | `spec-addition` | `n/a` | `accepted-later` | Interactive evaluation and trace/reporting surfaces should flush already-produced debug/trace output on abort or panic instead of discarding buffered messages. |
| `lean4#196` | `macros_composite_diagnostics` | `§17.2.4`, `§17.2.5`, `Appendix T` | `indirect` | `none` | `note-only` | `spec-addition` | `n/a` | `accepted-later` | Diagnostics that summarize several failed elaboration/termination/overload attempts should preserve each source location as nested submessages instead of flattening everything to one location or hiding the details behind trace flags. |
| `lean4#217` | `macros_placeholder_rendering` | `§17.2.4`, `§17.2.4.2`, `§17.2.5` | `indirect` | `none` | `note-only` | `spec-addition` | `n/a` | `accepted-later` | Placeholder diagnostics should prefer a source-like rendering of the expected type instead of eagerly normalizing to an internal elaborator stack that obscures the user's apparent goal. |
| `lean4#220` | `macros_pattern_alias_rendering` | `§7.5`, `§17.2.5`, `§17.3.6` | `indirect` | `none` | `note-only` | `spec-addition` | `n/a` | `accepted-later` | If Kappa later adds as-pattern syntax, lowered matcher forms should retain enough structure for source-like delaboration to reconstruct the alias binder rather than silently dropping it. |
| `lean4#333` | `macros_hidden_structure_diagnostics` | `§17.2.4`, `§17.2.4A.2`, `§17.2.4A.3` | `indirect` | `none` | `note-only` | `spec-addition` | `n/a` | `accepted-later` | Unification/type-mismatch diagnostics must not claim two textually identical types differ unless they also reveal the hidden generalized or implicit structure that actually differs. |
| `lean4#335` | `macros_hidden_structure_diagnostics` | `§17.2.4`, `§17.2.4A.2`, `§17.2.4A.3` | `indirect` | `none` | `note-only` | `spec-addition` | `n/a` | `accepted-later` | Type mismatches involving implicit binders should explain which hidden binder or motive instantiation differs instead of reporting a confusing mismatch on a metavariable-instantiated local name. |
| `lean4#337` | `macros_hidden_structure_diagnostics` | `§17.2.4`, `§17.2.4A.2`, `§17.2.4A.3` | `indirect` | `none` | `note-only` | `spec-addition` | `n/a` | `accepted-later` | If a metavariable cannot be instantiated because a hidden unification obligation blocks it, diagnostics should explain that blocking structure rather than just printing the metavariable, and duplicate copies of the same blocked-unification report should be coalesced. |
| `lean4#348` | `macros_parser_repair_diagnostics` | `§17.2.4`, `§17.2.5`, `Appendix T` | `indirect` | `none` | `note-only` | `spec-addition` | `n/a` | `accepted-later` | A local delimiter/token mistake should be diagnosed at the bad token with a source-preserving repair hint, instead of surfacing only a downstream type mismatch plus a later generic parse complaint. |
| `lean4#350` | `macros_elaboration_equivalence` | `§7.1.3`, `§7.3.3`, `§14.3`, `§17.3.1` | `indirect` | `none` | `note-only` | `spec-addition` | `n/a` | `accepted-later` | Trivial explicit-proof wrappers such as `by exact e` should not strengthen ordinary unification or instance-resolution success relative to the direct term unless the language states that distinction explicitly. |
| `lean4#351` | `macros_field_resolution_diagnostics` | `§7.1.3`, `§17.2.4`, `§17.2.4A.2` | `indirect` | `none` | `note-only` | `spec-addition` | `n/a` | `accepted-later` | Field/projection checking should not commit to a fatal field-notation error before ordinary default-instance and unification work has exposed the actual carrier type being projected. |
| `lean4#352` | `macros_hidden_structure_diagnostics` | `§17.2.4`, `§17.2.4A.2`, `§17.2.4A.3` | `indirect` | `none` | `note-only` | `spec-addition` | `n/a` | `accepted-later` | Universe-constrained instance-search failures should explain the blocking level/unification structure instead of only dumping an unsynthesized instance head and trace noise. |
| `lean4#355` | `macros_instance_search_propagation` | `§7.3.3`, `§14.3`, `§17.3.1` | `indirect` | `none` | `note-only` | `spec-addition` | `n/a` | `accepted-later` | Instance search should propagate instantiations learned from earlier resolved instances before deciding later goals are unsolved. |
| `lean4#346` | `macros_field_resolution_diagnostics` | `§2.3`, `§17.2.4`, `§17.2.4A.2` | `indirect` | `none` | `note-only` | `spec-addition` | `n/a` | `accepted-later` | Qualified-name lookup should not fall through to an irrelevant field-notation error when the real source problem is simply that `Namespace.name` is unknown. |
| `lean4#177` | `macros_pretty_printing` | `§17.2.5`, `Appendix T` | `indirect` | `none` | `note-only` | `spec-addition` | `n/a` | `accepted-later` | Internal lowering artifacts such as generated dependent-pattern matchers should pretty-print back to the corresponding source-like `match` form when that is the faithful user view. |
| `lean4#190` | `macros_explicit_rendering` | `§5.1`, `§17.2.5` | `indirect` | `none` | `note-only` | `spec-addition` | `n/a` | `accepted-later` | Fully explicit or debug-oriented renderings should still print universe/kind arguments in syntactically well-formed source-like form. |
| `lean4#192` | `macros_diagnostic_rendering` | `§5.8.1`, `§17.1.6`, `§17.2.5` | `indirect` | `none` | `note-only` | `spec-addition` | `n/a` | `accepted-later` | Trace/debug rendering should not expose raw bound-variable encodings like `#.0` when a stable user-oriented rendering is possible. |
| `lean4#193` | `macros_diagnostic_rendering` | `§5.8.1`, `§17.1.6`, `§17.2.5` | `indirect` | `none` | `note-only` | `spec-addition` | `n/a` | `accepted-later` | Anonymous/generated binders shown in traces or pretty-printer output should use a stable explanatory rendering rather than a raw implementation sentinel. |
| `lean4#255` | `macros_syntax_abbreviation_equivalence` | `§2.3`, `§5.8.1`, `§5.8.4` | `indirect` | `none` | `note-only` | `spec-addition` | `n/a` | `accepted-later` | If future local notation/syntax aliases may mention surrounding locals, they should capture that lexical environment hygienically or be rejected up front, not silently drop it or recurse. |
| `lean4#317` | `macros_pretty_printing` | `§5.8.1`, `§17.2.5` | `indirect` | `none` | `note-only` | `spec-addition` | `n/a` | `accepted-later` | Generated or transformed binder syntax should still pretty-print back to valid source-like syntax instead of falling back to raw AST renderings when formatter metadata is missing. |
| `lean4#391` | `macros_quotation_robustness` | `§5.8.1`, `§17.2.5` | `indirect` | `none` | `note-only` | `spec-addition` | `n/a` | `accepted-later` | Quotation over malformed but parser-adjacent syntax should fail with an ordinary structured diagnostic rather than reaching an internal panic or unreachable-code path. |

## Poor Fits / Background Only

| source | why it is not a clean Kappa import |
| --- | --- |
| `lean4#108` | Lean's antiquotation-kind propagation problem depends on syntax-category annotations in `macro` declarations. Kappa's macro surface is typed `Syntax t` plus ordinary functions, not category-indexed `macro_rules`, so there is no faithful direct repro yet. |
| `lean4#99` | Antiquotation choice-node optimization is an implementation-detail performance problem inside Lean's category-indexed parser/quotation compiler pipeline, not a direct Kappa surface obligation. |
| `lean4#414` | Same problem as `lean4#108`: antiquotation-only pattern declarations are specific to Lean's `macro_rules` surface, which Kappa does not currently specify. |
| `lean4#430` | Quotation over alternate syntax categories / parsers has no Kappa analogue today because Kappa quotes typed terms and types, not arbitrary parser categories. |
| `lean4#447` | The panic is tied to Lean's `declare_syntax_cat` command and incomplete user-defined parser-category declarations. Kappa currently has no comparable parser-category declaration surface, so this is parser-framework background rather than a clean macro import. |
| `lean4#111` | This is frontend/default-parameter explicitness behavior around a library helper, not a macro/hygiene/staging obligation. It is only weakly useful as future optional/default-argument evidence, so it should not drive a separate macro-wave fixture. |
| `lean4#312` | This is tactic-surface ergonomics around naming induction/injection-generated binders in unstructured proof scripts. Kappa does not currently standardize a comparable tactic language, so it is background for this macro wave. |
| `lean4#233` | This is documentation drift around core lambda syntax, not a macro/hygiene/staging obligation. It is useful as a docs-quality reminder but not as a separate macro-wave requirement. |
| `lean4#240` | A basic editor client is generic tooling/project work, not a distinct Kappa macro/syntax requirement. The underlying query/diagnostic surfaces are already tracked elsewhere. |
| `lean4#198` | This is implementation/embedding plumbing about reusing one pretty printer from C++; it is useful background for renderer consistency, but not a separate Kappa source-surface requirement by itself. |
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

- Lean 4: `#343`, `#353`, `#365`
- Racket: `#1410`, `#1479`, `#1495`

These are the next exact IDs to read if Wave 6 resumes immediately after the current staged batch.
