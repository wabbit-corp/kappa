# Scala Typeclass / Implicit Resolution Candidates For Kappa

This note mines `../datatron/data-scala-issues/` for Scala issues that are plausibly adaptable into Kappa fixtures.

Current Kappa coverage is no longer empty here, but the remaining backlog is still meaningful:

* syntax smoke tests for trait headers and trait members:
  * `tests/Kappa.Compiler.Tests/Fixtures/traits.headers.positive`
  * `tests/Kappa.Compiler.Tests/Fixtures/traits.members.positive`
* one end-to-end trait instance program in `tests/Kappa.Compiler.Tests/MilestoneTwoTests.fs`
* already imported or staged:
  * alias-aware implicit search
  * dependent later implicit parameters
  * premise dependency propagation
  * instantiate-before-search
  * branch-local refinement affecting implicit search
  * nested constraint scopes
  * supertrait projection in local implicit search
* still missing or still thin:
  * local implicit binders `let (@x : T) = ...`
  * local instances
  * ordinary-import invariance of global instance search
  * associated-member recovery from resolved dictionaries

Relevant Kappa rules:

* local implicit search order: `Spec.md` §7.3.3
* local implicit binders: `Spec.md` §6.3
* supertrait projection in local search: `Spec.md` §12.1.1
* global instance search: `Spec.md` §12.3.1
* branch-local evidence participating in search: `Spec.md` §§7.4.1, 7.5.4

## Best Direct Fits

* Scala 3 [#5427](https://github.com/scala/scala3/issues/5427) `implicit search fails with intra-parameter-list dependency`
  * Kappa fit: very strong.
  * Why: §7.3.3 and the application elaboration rules require earlier accepted arguments to substitute through later binder types before omitted implicits are synthesized.
  * Current status: imported as `tests/Kappa.Compiler.Tests/Fixtures/expressions.implicit_parameters.positive_dependent_later_implicit`
  * Adaptation: a function whose second implicit parameter depends on an associated member of the first implicit dictionary.
  * Suggested fixture: `implicit_resolution_positive_dependent_later_implicit`

* Scala 3 [#5549](https://github.com/scala/scala3/issues/5549) `Dependencies not propagated properly within an implicit parameter block`
  * Kappa fit: very strong.
  * Why: same dependency-propagation risk as `#5427`, but for recursive premise solving in instance search rather than just ordinary local implicit search.
  * Current status: imported as `tests/Kappa.Compiler.Tests/Fixtures/traits.instances.positive_premise_dependency_propagation`
  * Adaptation: resolve `Foo A`, then use its associated type/member to solve a second trait premise `Bar foo.T`.
  * Suggested fixture: `instance_resolution_positive_premise_dependency_propagation`

* Scala 3 [#2234](https://github.com/scala/scala3/issues/2234) `Minor inconsistency with type aliases and implicit resolution`
  * Kappa fit: strong.
  * Why: §7.3.3 compares candidate types to the goal by definitional equality, so transparent aliases must not block local implicit search.
  * Current status: imported as `tests/Kappa.Compiler.Tests/Fixtures/expressions.implicit_parameters.positive_alias_definitional_equality`
  * Adaptation: use `type Dummy a = a`, bind a local implicit `String`, and satisfy a goal `Dummy String` or vice versa.
  * Suggested fixture: `implicit_resolution_positive_alias_definitional_equality`

* Scala 3 [#739](https://github.com/scala/scala3/issues/739) `Type parameters should be instantiated before doing an implicit search`
  * Kappa fit: strong.
  * Why: omitted implicits after explicit arguments should see the instantiated type, not a still-abstract placeholder.
  * Current status: imported as `tests/Kappa.Compiler.Tests/Fixtures/expressions.implicit_parameters.positive_instantiate_before_search`
  * Adaptation: `f : (x : a) -> (@ev : a) -> a`, then call `f value` with a local implicit whose type matches the instantiated `a`.
  * Suggested fixture: `implicit_resolution_positive_instantiate_before_search`

* Scala 3 [#5469](https://github.com/scala/scala3/issues/5469) `Implicit resolution differs from Scala 2 in presence of GADTs`
  * Kappa fit: medium-to-strong.
  * Why: Kappa explicitly states that branch-local constructor refinement evidence participates in local implicit resolution.
  * Current status: staged as `new-tests/expressions.implicit_parameters.positive_branch_refinement`
  * Adaptation: inside a refined `match` branch, call an overloaded trait member whose instance should become solvable only after the branch refines the scrutinee index/type.
  * Suggested fixture: `implicit_resolution_positive_branch_refinement`
  * Caveat: this depends on how much indexed-data refinement is already implemented in the current compiler.

* Scala 3 [#877](https://github.com/scala/scala3/issues/877) `Nested defs with context bounds are desugared to nested defs with implicit parameters that have the same name`
  * Kappa fit: medium.
  * Why: Kappa also elaborates trait constraints into implicit evidence. Reused generated names could accidentally shadow outer evidence in nested local functions.
  * Current status: staged as `new-tests/expressions.implicit_parameters.positive_nested_constraint_scopes`
  * Adaptation: nested local functions with different trait constraints; the inner body should still be able to use both outer and inner evidence.
  * Suggested fixture: `implicit_resolution_positive_nested_constraint_scopes`
  * Caveat: more of an implementation-pitfall test than a surface-semantics test.

## Good Indirect Fits

* Scala 2 [#2714](https://github.com/scala/bug/issues/2714) `companion object not in implicit scope when other explicit import present`
  * Kappa fit: partial but useful.
  * Why: Kappa does not use Scala-style companion implicit scope, but it does specify that ordinary imports do not control global instance visibility once a module is in the compilation-unit closure.
  * Current status: merged and staged as `new-tests/traits.instances.positive_unrelated_import_does_not_disable_candidate`
  * Adaptation: add an unrelated ordinary import and verify it does not disable an otherwise available instance from an already-present module.
  * Suggested fixture: `instance_resolution_positive_unrelated_import_does_not_disable_candidate`

* Scala 2 [#2709](https://github.com/scala/bug/issues/2709) `package objects, implicit resolution, and imports`
  * Kappa fit: partial.
  * Why: again, not a package-object match, but it points at the same class of bug: name-resolution/import machinery incorrectly perturbing implicit search.
  * Current status: merged and staged as `new-tests/traits.instances.positive_unrelated_import_does_not_disable_candidate`
  * Adaptation: same general import-invariance fixture family as `#2714`.

* Scala 3 [#1857](https://github.com/scala/scala3/issues/1857) `implicit summon fails in dotty, works in 2.*`
  * Kappa fit: partial and more advanced.
  * Why: the issue is really about recovering concrete information from a summoned typeclass instance. Kappa has associated static members on traits, so this could become a good regression once those paths are exercised.
  * Current status: staged as `new-tests/traits.instances.positive_associated_member_from_resolved_dictionary`
  * Adaptation: resolve an implicit dictionary with an associated static member, then project that member through the resolved dictionary in downstream code.
  * Suggested fixture: `traits_positive_associated_member_from_resolved_dictionary`
  * Caveat: probably better after baseline instance-resolution tests exist.

* Scala 2 [#3927](https://github.com/scala/bug/issues/3927) `local implicit lazy val ambiguous with itself`
* Scala 2 [#4414](https://github.com/scala/bug/issues/4414) `An implicit value creates ambiguity with itself`
  * Kappa fit: low-to-medium.
  * Why: these are duplication bugs rather than semantic rules, but they are good guardrails if Kappa ever builds implicit contexts by merging multiple sources like record-field unpacking, local instances, and projected supertraits.
  * Adaptation: ensure one lexical implicit source does not surface twice as two distinct candidates.

## Poor Fits / Probably Not Worth Importing

* Scala 2 [#298](https://github.com/scala/bug/issues/298) `Ambigious implicits even though more specific implicit exists`
  * Poor fit: Kappa does not use Scala-style “most specific implicit wins” selection for local or global typeclass search.
  * Kappa local search is lexical-level based, and global instance search uses coherence, not specificity.

* Scala 2 [#1272](https://github.com/scala/bug/issues/1272) `Inconsistent implicit ambiguity`
  * Poor fit for now: the example is tied to Scala’s generic implicit specificity and search heuristics more than to Kappa’s spec.

* Scala 3 [#2911](https://github.com/scala/scala3/issues/2911) `Priority of implicits defined in companions`
  * Poor fit: Kappa has no companion-object-based instance scope.

* Scala 2 [#3453](https://github.com/scala/bug/issues/3453) `Implicit resolution ignores non-implicit shadowing`
* Scala 2 [#4270](https://github.com/scala/bug/issues/4270) `Implicits don't abide by normal shadowing`
  * Poor fit: these are mostly about Scala’s implicit-conversion lookup among accessible term names.
  * Kappa’s ordinary implicit search is over the local implicit context by type, not over all accessible implicit defs/imports.

* Scala 3 [#3284](https://github.com/scala/scala3/issues/3284) `Importing implicit values`
  * Poor fit: Kappa intentionally does not search imported top-level term bindings for ordinary non-constraint implicit goals.

## Recommended Pull Order

* First:
  * `#5427`
  * `#5549`
  * `#2234`
  * `#739`

* Second:
  * `#5469`
  * `#877`

* Third:
  * `#2714`
  * `#2709`
  * `#1857`

If we want the highest-yield fixture work, the first four are the clearest spec-aligned candidates.
