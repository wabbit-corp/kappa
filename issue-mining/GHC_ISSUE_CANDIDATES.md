# GHC Issue Candidates For Kappa

Mined from `../datatron/data-scala-issues/ghc-issues.json` on 2026-04-23.

Purpose: identify GHC issues that map cleanly onto Kappa's existing spec, without importing Haskell-specific features that Kappa intentionally does not have.

## Best Direct Fits

### 1. Incorrect layout must be rejected directly

- Source issue: GHC [#1060](https://gitlab.haskell.org/ghc/ghc/-/issues/1060) `GHC accepts program with incorrect layout`
- Kappa fit: strong
- Spec tie-in: `Spec.md` §3.4
- Why it matters:
  Kappa is indentation-sensitive. A regression here should produce a layout diagnostic, not silently accept malformed nesting or emit downstream nonsense.
- Current status:
  - staged as `new-tests/lexical.whitespace_indentation_continuation.negative_explicit_brace_after_layout`

### 2. Guards do not make a match exhaustive

- Source issues:
  - GHC [#29](https://gitlab.haskell.org/ghc/ghc/-/issues/29) `Check for exhaustive patterns is broken`
  - GHC [#30](https://gitlab.haskell.org/ghc/ghc/-/issues/30) `Check for exhaustive patterns continued`
- Kappa fit: strong
- Spec tie-in: `Spec.md` §7.5.6
- Why it matters:
  Kappa already states that guard failure does not exclude later cases and should not make a match exhaustive. This is exactly the kind of exhaustiveness rule that should have a dedicated negative fixture.
- Current status:
  - staged as `new-tests/expressions.match.negative_guard_does_not_make_exhaustive`

### 3. GADT or index refinement should suppress bogus incomplete-pattern results

- Source issue: GHC [#366](https://gitlab.haskell.org/ghc/ghc/-/issues/366) `incomplete patterns and GADT`
- Kappa fit: strong
- Spec tie-in: `Spec.md` §7.5.1, §7.5.1A
- Why it matters:
  Kappa's indexed-pattern semantics are explicit. If constructor knowledge refines the index enough to rule out other cases, the checker should treat the match as exhaustive.
- Current status:
  - staged as `new-tests/expressions.match.positive_index_refinement_exhaustive`

### 4. Constructor knowledge should disambiguate field projection

- Source issue: GHC [#1401](https://gitlab.haskell.org/ghc/ghc/-/issues/1401) `otherwise ambiguous field names shouldn’t be treated as ambiguous when the data constructor is known`
- Kappa fit: medium-to-strong
- Spec tie-in: `Spec.md` §2.8.3, §7.4.1
- Why it matters:
  Kappa explicitly allows constructor-field projection after success-side constructor refinement. That means constructor knowledge should be strong enough to disambiguate the projected field path.
- Current status:
  - staged as `new-tests/expressions.conditionals.positive_constructor_test_disambiguates_projection`

## Useful But Indirect Fits

### 5. Local implicit rebinding should stay lexical and deterministic

- Source issue: GHC [#1](https://gitlab.haskell.org/ghc/ghc/-/issues/1) `Implicit parameters cause strange behavi`
- Kappa fit: indirect
- Spec tie-in: `Spec.md` §6.3, §7.3.3
- Why it matters:
  Haskell implicit parameters are not Kappa's model, but this is still a useful reminder that implicit evidence should be governed by lexical scope and deterministic search, not dynamic rebinding surprises.
- Current status:
  - staged as `new-tests/expressions.implicit_parameters.runtime_positive_lexical_local_shadowing`

## Poor Fits / Skip For Now

- GHC [#1041](https://gitlab.haskell.org/ghc/ghc/-/issues/1041) `Bang patterns in do notation and lambdas`
  - poor fit: Kappa does not have Haskell-style bang patterns.

## Additional Follow-up Reads

- GHC [#271](https://gitlab.haskell.org/ghc/ghc/-/issues/271) `panic with GADTs`
  - classification: indexed-type / GADT engine background only
  - why not imported directly:
    the report is a compiler panic in a normalization-by-evaluation style GADT program, not a stable surface-language rule that adds a new Kappa obligation beyond the existing indexed-refinement fixtures

- GHC [#281](https://gitlab.haskell.org/ghc/ghc/-/issues/281) `Wrong overlapped/missing pattern warnings`
  - classification: pattern-checking background only
  - why not imported directly:
    the concrete trigger is Haskell `n+k` patterns, which Kappa does not have; the transferable overlap/exhaustiveness core is already represented by the staged guard and indexed-refinement cases

- GHC [#430](https://gitlab.haskell.org/ghc/ghc/-/issues/430) `Result type signatures and lexically scoped type variables`
  - classification: scoped-type-variable syntax background only
  - why not imported directly:
    Kappa has explicit `forall` binders and lexical generic scope, but it does not import Haskell's result-type-signature binding surface, so this stays as background for any later generic-scope pass

- GHC [#462](https://gitlab.haskell.org/ghc/ghc/-/issues/462) `Incomplete pattern warnings with GADTs`
  - classification: same direct-fit family as `#366`, already covered
  - mapping:
    this is duplicate-strength evidence for the existing staged `new-tests/expressions.match.positive_index_refinement_exhaustive` fixture rather than a distinct new Kappa test

- GHC [#595](https://gitlab.haskell.org/ghc/ghc/-/issues/595) `Overhaul GHC's overlapping/non-exhaustive pattern checking`
  - classification: checker-architecture background only
  - why not imported directly:
    this is meta-level evidence that overlap/exhaustiveness checking is fragile infrastructure, not a separate surface rule; Kappa already captures the strongest direct obligations with the existing guard and indexed-pattern fixtures

- GHC [#700](https://gitlab.haskell.org/ghc/ghc/-/issues/700) `Inconsistent typechecking of pattern match in function binding`
  - classification: higher-rank polymorphism background only
  - why not imported directly:
    the report depends on Haskell's rank-n polymorphic argument shape and pattern binding over `forall`-quantified values, which is not a current Kappa direct-fit surface

- GHC [#734](https://gitlab.haskell.org/ghc/ghc/-/issues/734) `Spurious type variable scope error report`
  - classification: diagnostic-quality background only
  - why not imported directly:
    the transferable lesson is that scope diagnostics should not cascade misleadingly from an earlier kind/type error, but this does not define a new Kappa surface rule

- GHC [#760](https://gitlab.haskell.org/ghc/ghc/-/issues/760) `Template Haskell doesn't like scoped type variables`
  - classification: macro/template background only
  - why not imported directly:
    this is specific to Haskell quotation/splice machinery rather than Kappa's current direct-fit surface; any later macro hygiene pass can treat it as background

- GHC [#810](https://gitlab.haskell.org/ghc/ghc/-/issues/810) `GHC complains about missing instance in conjunction with GADTs`
  - classification: spec-clarity follow-up
  - mapping:
    this is good evidence that Kappa should show, with an explicit example, whether constructor-local premises from indexed/GADT-style constructors become branch-local implicit evidence in the same way that constructor choice becomes branch-local index refinement

- GHC [#821](https://gitlab.haskell.org/ghc/ghc/-/issues/821) `implicit parameters and type synonyms`
  - classification: spec-clarity follow-up
  - mapping:
    this is useful evidence for adding a worked example that transparent type aliases preserve implicit/constraint structure under elaboration rather than behaving like ad hoc sugar that drops context

- GHC [#246](https://gitlab.haskell.org/ghc/ghc/-/issues/246) `Wrong pat-match order for records`
  - classification: Haskell laziness / bottom-observability background only
  - why not imported directly:
    the ambiguity is about observable field-match order in the presence of `undefined`; Kappa's record and constructor patterns are specified structurally rather than as a user-visible lazy match-order surface

- GHC [#518](https://gitlab.haskell.org/ghc/ghc/-/issues/518) `Erroneous pattern-match overlap warning`
  - classification: overlap-checking duplicate evidence
  - mapping:
    this is more evidence that overlap/exhaustiveness diagnostics are easy to get wrong, but the direct Kappa obligation is already covered by the staged guard/exhaustiveness and indexed-pattern fixtures

- GHC [#827](https://gitlab.haskell.org/ghc/ghc/-/issues/827) `Overlapped pattern warnings for lazy patterns`
  - classification: poor fit
  - why not imported directly:
    the trigger depends on Haskell lazy patterns, which Kappa does not have

- GHC [#890](https://gitlab.haskell.org/ghc/ghc/-/issues/890) `ghc panics on accessing a labeled field`
  - classification: constructor-constraint / record-update background
  - mapping:
    this is adjacent evidence for the constructor-local evidence and record-update typing family, but the surface trigger is a compiler panic around Haskell constructor contexts rather than a new direct Kappa rule

- GHC [#1204](https://gitlab.haskell.org/ghc/ghc/-/issues/1204) `Associated types don't work with record updates`
  - classification: spec-clarity follow-up
  - mapping:
    this is useful evidence for adding a worked example that ordinary record updates typecheck through associated-type normalization and instantiation, rather than appearing to require explicit ascription at the update site

- GHC [#961](https://gitlab.haskell.org/ghc/ghc/-/issues/961) `implement associated type synonyms`
  - classification: already covered by existing associated-member fixtures
  - mapping:
    Kappa already stages the core obligation that an associated member may appear in the same trait's member signatures and in later dependent signatures; the closest staged traces are `traits.instances.positive_associated_member_from_resolved_dictionary` and `traits.instances.positive_associated_member_projection_through_alias`

- GHC [#981](https://gitlab.haskell.org/ghc/ghc/-/issues/981) `implicit parameters, type synonyms, and $`
  - classification: spec-clarity follow-up
  - mapping:
    this adds more evidence to the existing alias-expansion note: transparent type aliases that carry implicit constraints should preserve that structure uniformly across ordinary application forms, rather than depending on direct-call spelling

- GHC [#1445](https://gitlab.haskell.org/ghc/ghc/-/issues/1445) `implicit parameter not hoisted`
  - classification: direct current-spec fixture
  - mapping:
    this is a clean Kappa fit because partial application of earlier explicit arguments should leave any remaining implicit parameter in the result type; stage `new-tests/expressions.implicit_parameters.positive_partial_application_preserves_remaining_implicit`

- GHC [#1723](https://gitlab.haskell.org/ghc/ghc/-/issues/1723) `type unsafety with type family + GADT`
  - classification: normalization / indexed-soundness background only
  - why not imported directly:
    the report is an unsoundness bug in the interaction between type-family reduction and GADT refinement, not a distinct surface rule that obviously extends Kappa beyond its existing definitional-equality and indexed-refinement obligations

- GHC [#1823](https://gitlab.haskell.org/ghc/ghc/-/issues/1823) `GADTs and scoped type variables don't work right`
  - classification: nested-pattern scoped-type-variable background
  - why not imported directly:
    the concrete trigger depends on Haskell pattern type signatures and scoped type variables; the transferable lesson is only that refinement facts must be available while checking nested patterns, which Kappa does not currently expose through the same surface syntax

- GHC [#1834](https://gitlab.haskell.org/ghc/ghc/-/issues/1834) `No instance for <type family result type> in ghci command line`
  - classification: direct current-spec fixture plus spec-clarity follow-up
  - mapping:
    this is a clean Kappa fit because instance search should see a normalized associated-member goal when matching trait instances; stage `new-tests/traits.instances.positive_associated_member_alias_normalizes_instance_search` and record a worked-example follow-up in `SPEC_ADDITIONS.md`

- GHC [#1948](https://gitlab.haskell.org/ghc/ghc/-/issues/1948) `panic compiling associated type synonyms`
  - classification: associated-type recursion / solver background only
  - why not imported directly:
    the report is a compiler panic in recursive associated-type equalities, not a distinct surface obligation beyond Kappa's existing associated-member projection and definitional-normalization rules

- GHC [#2141](https://gitlab.haskell.org/ghc/ghc/-/issues/2141) `Internal error on invalid record update`
  - classification: direct current-spec negative fixture
  - mapping:
    this is a straightforward Kappa fit because an update naming a missing field should be rejected as an ordinary user-facing error rather than falling into an internal failure; stage `new-tests/types.records.update.negative_unknown_field`

- GHC [#2595](https://gitlab.haskell.org/ghc/ghc/-/issues/2595) `Implement record update for existential and GADT data types`
  - classification: structural-record / existential-package adjacency, already substantially covered
  - mapping:
    Kappa's ordinary record update rules are structural rather than constructor-specific, and the closest portable obligation is already represented by package-like dependent-record repair/update fixtures such as `types.records.update.positive_implicit_compile_time_field_repair`

- GHC [#3038](https://gitlab.haskell.org/ghc/ghc/-/issues/3038) `Associated type use triggers a bogus error message`
  - classification: direct current-spec fixture plus spec-clarity follow-up
  - mapping:
    this adds more evidence that transparent aliases mentioning associated members should preserve the same normalized behavior in ordinary reuse and solver-facing positions; the direct behavioral slice is staged as `new-tests/traits.instances.positive_associated_member_alias_normalizes_instance_search`, and the remaining gap is an explicit worked example in `SPEC_ADDITIONS.md`

- GHC [#2435](https://gitlab.haskell.org/ghc/ghc/-/issues/2435) `Qualified name required when defining type family instance in instance declaration`
  - classification: direct current-spec fixture
  - mapping:
    Kappa should keep imported trait aliases semantically uniform inside an instance declaration rather than splitting the instance head from its associated members; stage `new-tests/traits.instances.positive_imported_trait_alias_preserves_instance_member_identity`

- GHC [#2661](https://gitlab.haskell.org/ghc/ghc/-/issues/2661) `Associated type synonyms not fully simplified in GHCi`
  - classification: direct current-spec fixture already staged
  - mapping:
    the closest Kappa obligation is that associated-member projections and aliases normalize before instance search and ordinary reuse; that slice is now staged as `new-tests/traits.instances.positive_associated_member_alias_normalizes_instance_search`

- GHC [#2852](https://gitlab.haskell.org/ghc/ghc/-/issues/2852) `Type family checking oddity`
  - classification: poor fit
  - why not imported directly:
    the report depends on Haskell's unconstrained use of class-associated type families in data declarations, whereas Kappa associated members are projected from explicit dictionaries or sealed packages rather than silently introducing a latent class context on a data constructor

- GHC [#5852](https://gitlab.haskell.org/ghc/ghc/-/issues/5852) `methods and associated types treated differently wrt. qualification`
  - classification: direct current-spec fixture
  - mapping:
    this is the same semantic-identity obligation as `#2435`, and the staged Kappa analogue is `new-tests/traits.instances.positive_imported_trait_alias_preserves_instance_member_identity`

- GHC [#8978](https://gitlab.haskell.org/ghc/ghc/-/issues/8978) `Type synonyms in class associated types behave strangely`
  - classification: direct current-spec fixture already staged
  - mapping:
    this adds more evidence that transparent aliases naming associated members must behave like real aliases during constraint use and method checking; the direct Kappa slice is covered by `new-tests/traits.instances.positive_associated_member_alias_normalizes_instance_search`

- GHC [#3743](https://gitlab.haskell.org/ghc/ghc/-/issues/3743) `type checker fails to infer an implicit parameter constraint in the presence of existential types`
  - classification: inference/background only
  - why not imported directly:
    the report is about principal-type inference through an existential-pattern match rather than a distinct source-language rule; Kappa may still want similar behavior, but the stronger direct obligations are already the lexical implicit rules and the explicit-signature fixtures

- GHC [#4226](https://gitlab.haskell.org/ghc/ghc/-/issues/4226) `Lifting constraints is questionably correct for implicit parameters`
  - classification: direct current-spec fixture already staged
  - mapping:
    the direct Kappa obligation is lexical rather than dynamic implicit capture, especially when a local implicit is closed over by a returned function; that slice is already staged as `new-tests/expressions.implicit_parameters.runtime_positive_lexical_local_shadowing`

- GHC [#5120](https://gitlab.haskell.org/ghc/ghc/-/issues/5120) `inferred type of an implicit parameter rejected (associated type)`
  - classification: direct current-spec fixture
  - mapping:
    this is a clean Kappa fit because an explicit signature may quantify an implicit value whose type is an associated member of another implicit dictionary; stage `new-tests/expressions.implicit_parameters.positive_associated_member_typed_implicit_argument`

- GHC [#5713](https://gitlab.haskell.org/ghc/ghc/-/issues/5713) `Non-backward compatible scoping-change for associated types in GHC-7.4`
  - classification: direct current-spec fixture already staged
  - mapping:
    the imported-trait-alias instance fixture `new-tests/traits.instances.positive_imported_trait_alias_preserves_instance_member_identity` already captures the Kappa obligation that imported trait identity remains stable across the instance head and associated members

- GHC [#8011](https://gitlab.haskell.org/ghc/ghc/-/issues/8011) `-ddump-minimal-imports creates incorrect imports for associated types`
  - classification: tooling-only background
  - why not imported directly:
    the report is about pretty-printing minimal imports rather than source-language behavior; it is useful context for future tooling, but not a direct spec or compiler obligation for current Kappa language semantics

- GHC [#7658](https://gitlab.haskell.org/ghc/ghc/-/issues/7658) `Support empty record update syntax`
  - classification: ergonomics/spec-addition follow-up
  - mapping:
    this is a plausible Kappa ergonomics extension rather than a current-spec test: consider whether an explicit empty record-update identity form such as `r.{}` should be permitted and how it interacts with compile-time/static-field retagging

- GHC [#8297](https://gitlab.haskell.org/ghc/ghc/-/issues/8297) `Allow implicit parameters to take a default value`
  - classification: ergonomics/spec-addition follow-up
  - mapping:
    this is a good candidate for a future Kappa ergonomics feature around optional/default implicit arguments, since current Kappa implicits are always mandatory once demanded by the type

- GHC [#9063](https://gitlab.haskell.org/ghc/ghc/-/issues/9063) `Default associated type instances are too general`
  - classification: feature-completion/spec-addition follow-up
  - mapping:
    Kappa does not currently stage default associated static members as a user-facing feature, but this issue is good evidence that if such defaults are ever added they should come with very explicit scoping and normalization rules

- GHC [#9167](https://gitlab.haskell.org/ghc/ghc/-/issues/9167) `Associated type is accepted even without mentioning class parameters`
  - classification: neutral/background
  - why not imported directly:
    Kappa associated static members are projected from explicit dictionaries rather than silently entangled with a trait head's parameters in the Haskell way, so a member that ignores some trait parameters is not obviously problematic in the same way

- GHC [#9437](https://gitlab.haskell.org/ghc/ghc/-/issues/9437) `Wrong error message when using \`..\` with a record update`
  - classification: poor fit
  - why not imported directly:
    the trigger depends on Haskell `..` record-wildcard update syntax, which Kappa does not have; the transferable diagnostic concern is too syntax-specific to warrant a direct import

- GHC [#9264](https://gitlab.haskell.org/ghc/ghc/-/issues/9264) `Scoped kind variables do not work with default associated types`
  - classification: feature-completion/spec-addition follow-up
  - mapping:
    this strengthens the existing Kappa follow-up note about possible default associated static members: if that feature is ever added, the spec should say exactly how scoped type/kind parameters flow into the default body and signature

- GHC [#9318](https://gitlab.haskell.org/ghc/ghc/-/issues/9318) `Type error reported in wrong place with repeated type family expressions`
  - classification: diagnostic-only background
  - why not imported directly:
    the underlying semantic obligation is just ordinary normalization before comparison; the issue itself is about misplaced/confused diagnostics rather than a missing Kappa language rule

- GHC [#9394](https://gitlab.haskell.org/ghc/ghc/-/issues/9394) `Show data/type family instances with ghci's :info command`
  - classification: tooling-only background
  - why not imported directly:
    this is about REPL inspection ergonomics for family instances, not source-language behavior or current compiler acceptance rules

- GHC [#9429](https://gitlab.haskell.org/ghc/ghc/-/issues/9429) `Alternative to type family Any`
  - classification: dynamic-representation/spec-addition follow-up
  - mapping:
    this adds more evidence to the existing `DynamicType` / `DynRep` portability note: Kappa should be explicit about whether there is any portable universal placeholder representation story, or whether higher-kinded dynamic packaging always requires explicit carriers and witnesses

- GHC [#9433](https://gitlab.haskell.org/ghc/ghc/-/issues/9433) `Partially applied type family allowed but unusable`
  - classification: Haskell type-family background
  - why not imported directly:
    Kappa's compile-time functions are specified as ordinary compile-time function spaces rather than Haskell-style open type families, so this exact partial-application pathology is not the right direct import target

- GHC [#23570](https://gitlab.haskell.org/ghc/ghc/-/issues/23570) `Odd (and inconsistent) behavior when importing associated type families with (..)`
  - classification: import-system background
  - why not imported directly:
    Kappa's import system uses explicit kind selectors rather than Haskell-style class-child `(..)` import lists for associated members, so this exact confusion does not transfer directly

- GHC [#23768](https://gitlab.haskell.org/ghc/ghc/-/issues/23768) `Check for unused type variables in type instances is buggy for associated type defaults`
  - classification: feature-completion/spec-addition follow-up
  - mapping:
    this strengthens the existing Kappa follow-up note about possible default associated static members: if defaults are added, binder-validation and unused-variable rules need to be specified as carefully as the default body itself

- GHC [#25853](https://gitlab.haskell.org/ghc/ghc/-/issues/25853) `Type synonyms aren't expanded for class names in instance heads`
  - classification: direct current-spec fixture already staged
  - mapping:
    Kappa already stages the relevant alias-in-instance-head behavior with `new-tests/traits.instances.positive_imported_trait_alias_preserves_instance_member_identity`

- GHC [#25991](https://gitlab.haskell.org/ghc/ghc/-/issues/25991) `Punned class method and associated type break explicit import`
  - classification: import-system background
  - why not imported directly:
    Kappa's `term` / `trait` / `type` import selectors keep the term/type namespace split explicit, so this exact punned-child import ambiguity does not map directly

- GHC [#26714](https://gitlab.haskell.org/ghc/ghc/-/issues/26714) `Superclass resolution for a type family synonym fails despite succeeding in method body`
  - classification: direct current-spec fixture
  - mapping:
    the closest Kappa analogue is that an instance premise carrying the needed supertrait should satisfy the instance head's own superclass obligation; stage `new-tests/traits.instances.positive_premise_supertrait_satisfies_instance_superclass`

- GHC [#10335](https://gitlab.haskell.org/ghc/ghc/-/issues/10335) `Failure to construct superclasses in instance`
  - classification: direct current-spec fixture
  - mapping:
    Kappa should also satisfy an instance head's superclass obligation when the needed evidence arrives through a transparent alias in the instance premise; stage `new-tests/traits.instances.positive_alias_premise_satisfies_instance_superclass`

- GHC [#10808](https://gitlab.haskell.org/ghc/ghc/-/issues/10808) `Odd interaction between record update and type families`
  - classification: record-update ergonomics/spec-addition follow-up
  - mapping:
    this strengthens the Kappa note about possible type-changing record updates: users expect `r.{ field = ... }` to behave like manual destructure-and-rebuild when omitted fields still normalize at the new index

- GHC [#10856](https://gitlab.haskell.org/ghc/ghc/-/issues/10856) `Record update doesn't emit new constraints`
  - classification: record-update ergonomics/spec-addition follow-up
  - mapping:
    this strengthens the same Kappa follow-up note, especially for any future update form that would retag existential or constrained fields and therefore need to re-establish fresh local evidence

- GHC [#16501](https://gitlab.haskell.org/ghc/ghc/-/issues/16501) `Record updates for existential fields do not work + confusing error`
  - classification: record-update ergonomics/spec-addition follow-up
  - mapping:
    this is further evidence that if Kappa ever extends record update beyond same-type reconstruction, the existential-payload case should match ordinary manual rebuild rather than needing a special-case mental model

- GHC [#17104](https://gitlab.haskell.org/ghc/ghc/-/issues/17104) `Implicit parameters not properly scoped`
  - classification: direct current-spec fixture already staged
  - mapping:
    Kappa's direct obligation is lexical rather than dynamic implicit capture, and that slice is already staged as `new-tests/expressions.implicit_parameters.runtime_positive_lexical_local_shadowing`

- GHC [#18802](https://gitlab.haskell.org/ghc/ghc/-/issues/18802) `Typecheck record update via desugaring`
  - classification: record-update ergonomics/spec-addition follow-up
  - mapping:
    this strengthens the same Kappa note by pointing at a desugaring-first formulation for any future type-changing or existential-aware update design

- GHC [#10362](https://gitlab.haskell.org/ghc/ghc/-/issues/10362) `Make tuple constraints into a class`
  - classification: feature-completion/spec-addition follow-up
  - mapping:
    this points at a plausible Kappa ergonomics extension: a reusable grouped-constraint alias or equivalent first-class conjunction surface under `Constraint`, rather than only ad hoc multi-constraint syntax in headers and arrows

- GHC [#10749](https://gitlab.haskell.org/ghc/ghc/-/issues/10749) `Boot file instances should imply superclasses`
  - classification: module/interface background
  - why not imported directly:
    this depends on Haskell `hs-boot` / Backpack interface behavior rather than ordinary source-level trait resolution, so the direct Kappa analogue is about interface serialization completeness, not current surface syntax or fixture coverage

- GHC [#11067](https://gitlab.haskell.org/ghc/ghc/-/issues/11067) `Spurious superclass cycle error with type equalities`
  - classification: quantified-constraint / equality-cycle background
  - why not imported directly:
    the trigger depends on Haskell quantified-constraint encodings and equality-superclass cycle detection machinery that Kappa does not currently expose as source-level surface

- GHC [#11278](https://gitlab.haskell.org/ghc/ghc/-/issues/11278) `Spurious potential superclass cycle with constraint synonyms`
  - classification: feature-completion/spec-addition follow-up
  - mapping:
    this strengthens the same grouped-constraint alias note: if Kappa adds reusable conjunction aliases for constraints, ordinary transparent expansion and cycle checking need to be specified rather than left to implementation accident

- GHC [#11466](https://gitlab.haskell.org/ghc/ghc/-/issues/11466) `Constraint synonym with Implicit Parameter`
  - classification: model-mismatch background
  - why not imported directly:
    Kappa implicit parameters are ordinary implicit binders and lexical local values, not Haskell-style first-class implicit-parameter constraint descriptors, so this exact aliasing pathology does not transfer directly

- GHC [#12507](https://gitlab.haskell.org/ghc/ghc/-/issues/12507) `Can't deduce implicit parameter`
  - classification: direct current-spec fixture already staged
  - mapping:
    the direct Kappa obligation is that a richer local implicit context still satisfies a narrower exact goal, and that slice is already staged as `new-tests/expressions.implicit_parameters.positive_nested_constraint_scopes`

- GHC [#18311](https://gitlab.haskell.org/ghc/ghc/-/issues/18311) `Record update is over-restrictive`
  - classification: record-update ergonomics/spec-addition follow-up
  - mapping:
    this adds more evidence to the same Kappa note about possible type-changing record updates when manual destructure-and-rebuild would be valid and omitted fields still normalize correctly

- GHC [#7989](https://gitlab.haskell.org/ghc/ghc/-/issues/7989) `"No constructor has all these fields" message can be improved`
  - classification: diagnostic-quality background
  - why not imported directly:
    the concrete trigger depends on nominal sum-constructor field sets, but the transferable Kappa point is only diagnostic quality for multi-field record-update failures, which is already covered more directly by the standardized record-update diagnostic families

- GHC [#9023](https://gitlab.haskell.org/ghc/ghc/-/issues/9023) `Error when using empty record update on binary pattern synonym`
  - classification: syntax/model-specific poor fit
  - why not imported directly:
    this is about Haskell pattern-synonym `{}` syntax rather than Kappa's record-patch surface; it is not additional evidence for Kappa's own empty-record-update note

- GHC [#11156](https://gitlab.haskell.org/ghc/ghc/-/issues/11156) `Type-changing record update catch-all in sum type doesn't typecheck`
  - classification: record-update ergonomics/spec-addition follow-up
  - mapping:
    this adds more evidence to the same Kappa note about type-changing record updates, specifically when branch refinement has already ruled out constructors whose omitted fields would block the reconstruction

- GHC [#18650](https://gitlab.haskell.org/ghc/ghc/-/issues/18650) `Warning for partial solutions to HasField constraints`
  - classification: warning-policy / record-selector background
  - why not imported directly:
    this is primarily about warning granularity for Haskell partial-field and `HasField` use sites rather than Kappa source semantics; Kappa's transferable concern is only later warning precision, not a new current-spec fixture

- GHC [#18809](https://gitlab.haskell.org/ghc/ghc/-/issues/18809) `Core Lint error when using record update with GADT in data family`
  - classification: record-update ergonomics/spec-addition follow-up
  - mapping:
    this is more evidence that any future Kappa type-changing/existential record-update extension should be specified via reconstructive elaboration rather than bespoke update-specific typing machinery once refined families participate

- GHC [#18999](https://gitlab.haskell.org/ghc/ghc/-/issues/18999) `DisambiguateRecordFields should cover record updates`
  - classification: field-name-resolution background
  - why not imported directly:
    this depends on Haskell's overloaded field namespace and selector/value ambiguity, whereas Kappa record-patch left-hand sides are syntactically field positions and do not share that exact resolution problem

- GHC [#19084](https://gitlab.haskell.org/ghc/ghc/-/issues/19084) `False positives from -Wincomplete-record-updates`
  - classification: warning-precision background
  - why not imported directly:
    the transferable idea is path-sensitive warning precision under prior constructor refinement, but this is warning policy rather than a current Kappa source-language acceptance rule

- GHC [#11343](https://gitlab.haskell.org/ghc/ghc/-/issues/11343) `Unable to infer type when using DuplicateRecordFields`
  - classification: field-name-resolution background
  - why not imported directly:
    this depends on Haskell duplicate-record-field disambiguation for updates, whereas Kappa record-patch left-hand sides are resolved structurally from the scrutinee type rather than through an overloaded field namespace

- GHC [#12190](https://gitlab.haskell.org/ghc/ghc/-/issues/12190) `Generalize irrefutable patterns (static semantics like let-bindings)`
  - classification: pattern-generalization background
  - why not imported directly:
    Kappa already specifies irrefutable-pattern positions explicitly and intentionally restricts constructor-pattern irrefutability to the provable-single-constructor case, so this Haskell generalization request does not expose a current spec gap by itself

- GHC [#16839](https://gitlab.haskell.org/ghc/ghc/-/issues/16839) `Implement lint check for empty cases with non-divergent scrutinee`
  - classification: backend/lint background
  - why not imported directly:
    this is an internal Core lint invariant rather than a source-level Kappa behavior question

- GHC [#17647](https://gitlab.haskell.org/ghc/ghc/-/issues/17647) `Optimize large constructor updates.`
  - classification: optimization background
  - why not imported directly:
    the issue is about backend code quality for large nominal-constructor updates, not a source-language acceptance rule or missing Kappa surface requirement

- GHC [#19088](https://gitlab.haskell.org/ghc/ghc/-/issues/19088) `Set cheap-to-compare IdInfo fields only if they have changed`
  - classification: compiler-optimization background
  - why not imported directly:
    this is compiler-internal allocation tuning, not a transferable Kappa surface or conformance point

- GHC [#19463](https://gitlab.haskell.org/ghc/ghc/-/issues/19463) `Refactor RecordUpd`
  - classification: compiler-representation background
  - why not imported directly:
    this is an internal AST refactor for GHC's record-update representation, not a source-semantics issue

- GHC [#19972](https://gitlab.haskell.org/ghc/ghc/-/issues/19972) `RecordDotSyntax error message with Shouldn'tHappenOrigin`
  - classification: diagnostic-quality/spec-addition follow-up
  - mapping:
    this strengthens a Kappa diagnostic-hygiene note: internal fallback origins and placeholder markers must never leak into user-visible diagnostics

- GHC [#10132](https://gitlab.haskell.org/ghc/ghc/-/issues/10132) `Inconsistent kind polymorphism for top-level and associated type families`
  - classification: kind-inference background
  - why not imported directly:
    this is largely about GHC's default kind-polymorphism policy for open families versus top-level declarations, whereas Kappa's associated static members are ordinary compile-time members projected from dictionaries rather than separate family declarations

- GHC [#10361](https://gitlab.haskell.org/ghc/ghc/-/issues/10361) `DeriveAnyClass does not fill in associated type defaults`
  - classification: feature-completion/spec-addition follow-up
  - mapping:
    this strengthens the existing Kappa follow-up note about possible default associated static members, especially around what counts as inheriting or materializing a default when an instance is introduced by sugar or derivation-like mechanisms

- GHC [#10817](https://gitlab.haskell.org/ghc/ghc/-/issues/10817) `Looping default associated type family without UndecidableInstances`
  - classification: feature-completion/spec-addition follow-up
  - mapping:
    this strengthens the same Kappa default-associated-member note: if defaults are ever added, termination/cycle checks must be specified explicitly rather than left to accidental nontermination

- GHC [#10899](https://gitlab.haskell.org/ghc/ghc/-/issues/10899) `Polytype accepted in RHS of default associated type`
  - classification: feature-completion/spec-addition follow-up
  - mapping:
    this strengthens the same Kappa note around default associated static members, specifically that the RHS must satisfy ordinary well-formedness and not silently admit illegal polymorphic shapes

- GHC [#11115](https://gitlab.haskell.org/ghc/ghc/-/issues/11115) `Indicate missing associated type instances`
  - classification: diagnostic-quality/spec-addition follow-up
  - mapping:
    this motivates an explicit Kappa diagnostic rule that failed associated-member normalization should mention the missing governing trait instance rather than only an unreduced type mismatch

- GHC [#11136](https://gitlab.haskell.org/ghc/ghc/-/issues/11136) `Associated type family: panic due to mismatch in arity of default instances`
  - classification: feature-completion/spec-addition follow-up
  - mapping:
    this strengthens the same Kappa default-associated-member note around arity validation and rejecting malformed defaults as ordinary user-facing errors rather than internal failures

- GHC [#11451](https://gitlab.haskell.org/ghc/ghc/-/issues/11451) `Inconsistent warnings for unused binders in type and instance declarations`
  - classification: warning-policy / binder-validation follow-up
  - mapping:
    this is additional evidence that if Kappa ever adds default associated static members, binder-validation and unused-binder policy should be specified consistently across trait headers, instances, and default bodies

- GHC [#10020](https://gitlab.haskell.org/ghc/ghc/-/issues/10020) `GHC 7.10 rejects nullary type class with associated data`
  - classification: model-mismatch background
  - why not imported directly:
    this depends on Haskell's nullary type classes plus associated data-family validity rules, whereas Kappa associated static members are ordinary dictionary projections and do not have a parallel "must mention a class variable" restriction

- GHC [#10811](https://gitlab.haskell.org/ghc/ghc/-/issues/10811) `Template Haskell does associated types poorly (printing & quoting)`
  - classification: macro / quotation / tooling background
  - why not imported directly:
    this is about Template Haskell round-tripping and pretty-printing of associated-type defaults rather than Kappa source semantics; it is only relevant later if Kappa grows declaration macros with quoted trait members

- GHC [#10815](https://gitlab.haskell.org/ghc/ghc/-/issues/10815) `Need more kind inference in associated type instances`
  - classification: spec-clarity/spec-addition follow-up
  - mapping:
    this strengthens a Kappa follow-up note that instance checking for associated static members should substitute the instance head, including refined kind information, before validating the member definition

- GHC [#11450](https://gitlab.haskell.org/ghc/ghc/-/issues/11450) `Associated types at wrong type in instance`
  - classification: spec-clarity/spec-addition follow-up
  - mapping:
    this strengthens the same Kappa note: after substituting the instance head into a trait's associated static member declaration, the instance definition must match that instantiated member shape rather than silently accepting reordered or mismatched parameters

- GHC [#11534](https://gitlab.haskell.org/ghc/ghc/-/issues/11534) `Allow class associated types to reference functional dependencies`
  - classification: poor fit / model-mismatch background
  - why not imported directly:
    this relies on Haskell functional dependencies feeding associated-type defaults, and Kappa does not currently have an analogous fundep mechanism

- GHC [#13398](https://gitlab.haskell.org/ghc/ghc/-/issues/13398) `Associated type family instance validity checking is too conservative`
  - classification: spec-clarity/spec-addition follow-up
  - mapping:
    this strengthens the Kappa note that associated static members should be checked after instance-head substitution and ordinary normalization, rather than by over-conservative syntactic validity checks that reject shapes whose meaning is already determined

- GHC [#13404](https://gitlab.haskell.org/ghc/ghc/-/issues/13404) `Derive instances for classes with associated types`
  - classification: feature-completion/spec-addition follow-up
  - mapping:
    this motivates a Kappa follow-up for future deriving: if `derive` expands beyond the current minimal baseline, the spec should say whether traits with associated static members can be derived and when representation-preserving coercion is enough to synthesize those members

- GHC [#13773](https://gitlab.haskell.org/ghc/ghc/-/issues/13773) `Types are not normalized in instance declarations`
  - classification: spec-clarity/spec-addition follow-up
  - mapping:
    this strengthens the same Kappa note around instance checking and matching under ordinary compile-time normalization, so users are not forced to hand-normalize transparent arithmetic or alias-shaped instance heads

- GHC [#13971](https://gitlab.haskell.org/ghc/ghc/-/issues/13971) `Misleading "Kind mis-match on LHS of default declaration" error`
  - classification: feature-completion/spec-addition follow-up
  - mapping:
    this strengthens the existing Kappa follow-up note about possible default associated static members: if defaults are ever added, diagnostics should blame the real failing side of a malformed default declaration rather than pointing at a syntactically fine left-hand side

- GHC [#13972](https://gitlab.haskell.org/ghc/ghc/-/issues/13972) `GHC 8.2 error message around indexes for associated type instances is baffling`
  - classification: diagnostic-quality/spec-addition follow-up
  - mapping:
    this strengthens two Kappa diagnostic notes at once: associated-member instance validation should explain hidden kind/index structure when that is the real mismatch, and user-facing errors must not collapse into identical-looking expected/actual renderings

- GHC [#14046](https://gitlab.haskell.org/ghc/ghc/-/issues/14046) `“Illegal type synonym family application in instance” is too strict in the presence of functional dependencies`
  - classification: poor fit / fundep-specific background
  - why not imported directly:
    this relies on Haskell functional dependencies making a family application effectively determined in an instance head, and Kappa does not currently have a matching fundep mechanism

- GHC [#14094](https://gitlab.haskell.org/ghc/ghc/-/issues/14094) `DeriveAnyClass doesn't warn about unimplemented type families`
  - classification: feature-completion/spec-addition follow-up
  - mapping:
    this strengthens the existing Kappa follow-up note about possible default associated static members: if auto-generated instances ever participate, missing associated members must trigger the same ordinary completeness warnings as handwritten instances

- GHC [#14132](https://gitlab.haskell.org/ghc/ghc/-/issues/14132) `Report an error for a missing class instance before an error for type family instances of an associated type.`
  - classification: diagnostic-quality/spec-addition follow-up
  - mapping:
    this strengthens the Kappa note that associated-member normalization failures should report the missing upstream owner instance first, instead of leading with a downstream unreduced associated-member mismatch

- GHC [#14230](https://gitlab.haskell.org/ghc/ghc/-/issues/14230) `Gruesome kind mismatch errors for associated data family instances`
  - classification: diagnostic-quality/spec-addition follow-up
  - mapping:
    this strengthens the hidden-structure diagnostic note: associated-family validation errors should expose the real mismatching index/kind structure instead of printing baffling expected/actual kinds that conceal the instance-head refinement

- GHC [#14462](https://gitlab.haskell.org/ghc/ghc/-/issues/14462) `deriving on associated data types fails to find constraints`
  - classification: feature-completion/spec-addition follow-up
  - mapping:
    this strengthens the Kappa deriving follow-up for traits with associated static members: if deriving eventually synthesizes instances whose payloads depend on associated members, the derivation story must say how the needed premises are inferred or required explicitly

- GHC [#14661](https://gitlab.haskell.org/ghc/ghc/-/issues/14661) `Cannot derive (newtype I a b = I (F a -> F b) deriving newtype Category) for type family F`
  - classification: feature-completion/spec-addition follow-up
  - mapping:
    this strengthens the Kappa deriving follow-up for traits with associated static members and richer coercion-based deriving: if representation-preserving deriving is extended, the spec should say exactly when coercion through compile-time families is sufficient to synthesize a valid ordinary instance

- GHC [#14728](https://gitlab.haskell.org/ghc/ghc/-/issues/14728) `Is (GeneralizedNewtypeDeriving + associated type classes) completely bogus?`
  - classification: spec-clarity/spec-addition follow-up
  - mapping:
    this strengthens the same Kappa deriving note: any derive path that synthesizes associated static members should be explainable as a valid explicit instance package, not as magical generated equalities that users could not themselves write coherently

- GHC [#14916](https://gitlab.haskell.org/ghc/ghc/-/issues/14916) `Missing checks when deriving special classes`
  - classification: spec-clarity/spec-addition follow-up
  - mapping:
    this strengthens the Kappa deriving note around admissibility: generated instances should be subject to the same forbidden-class and well-formedness checks as handwritten or standalone instances rather than bypassing special-case restrictions

- GHC [#15052](https://gitlab.haskell.org/ghc/ghc/-/issues/15052) `DeriveAnyClass instances may skip TypeError constraints`
  - classification: spec-clarity/spec-addition follow-up
  - mapping:
    this strengthens the same deriving note: auto-generated instances must still enforce ordinary superclass and rejected-constraint obligations instead of sneaking through constraints that would reject a handwritten instance

- GHC [#15711](https://gitlab.haskell.org/ghc/ghc/-/issues/15711) `Kind inference of class variables does not examine associated types`
  - classification: kind-inference background
  - why not imported directly:
    Kappa trait parameters carry explicit annotations in the trait header, so this specific failure mode around inferring a class parameter's kind from associated-family declarations is not a direct surface gap in the same way

- GHC [#15178](https://gitlab.haskell.org/ghc/ghc/-/issues/15178) `Implement DerivingVia`
  - classification: feature-completion/spec-addition follow-up
  - mapping:
    this motivates a Kappa follow-up for a possible explicit coercion-based `derive via` surface rather than leaving all richer derivation paths implicit or implementation-defined

- GHC [#15191](https://gitlab.haskell.org/ghc/ghc/-/issues/15191) `Deriving via DeriveAnyClass not behaving the same as an emply instance declaration`
  - classification: spec-clarity/spec-addition follow-up
  - mapping:
    this strengthens the Kappa deriving-admissibility note: a derive form that claims to elaborate to an ordinary instance should behave like the corresponding explicit empty/defaulted instance rather than following a separate overlap-resolution path

- GHC [#15376](https://gitlab.haskell.org/ghc/ghc/-/issues/15376) `GHC determine illegal kind for standalone deriving with Deriving via`
  - classification: feature-completion/spec-addition follow-up
  - mapping:
    this strengthens the possible Kappa `derive via` note: if such a surface exists, standalone and inline forms should bind kind/type variables identically instead of giving the same derivation different kinding behavior

- GHC [#15434](https://gitlab.haskell.org/ghc/ghc/-/issues/15434) `DerivingVia (and perhaps even GND) works badly with DeriveGeneric`
  - classification: feature-completion/spec-addition follow-up
  - mapping:
    this strengthens the same Kappa `derive via` note around representation-sensitive traits: coercion-based deriving must distinguish traits whose semantics depend on structural metadata from traits that are safely representation-preserving

- GHC [#15831](https://gitlab.haskell.org/ghc/ghc/-/issues/15831) `DerivingVia allows bogus implicit quantification in \`via\` type`
  - classification: feature-completion/spec-addition follow-up
  - mapping:
    this strengthens the possible Kappa `derive via` note: the `via` type should not admit hidden binder introduction or under-specified quantification that later leaks as nonsensical generated code

- GHC [#15868](https://gitlab.haskell.org/ghc/ghc/-/issues/15868) `Standard deriving should be less conservative when \`UndecidableInstances\` is enabled`
  - classification: feature-completion/spec-addition follow-up
  - mapping:
    this strengthens the broader Kappa deriving note: if richer deriving is supported, the spec should say whether and when it may infer premises that are already derivable from field structure instead of forcing users to restate them manually

- GHC [#15969](https://gitlab.haskell.org/ghc/ghc/-/issues/15969) `Generic1 deriving should use more coercions`
  - classification: implementation/performance background
  - why not imported directly:
    this is mostly about generating more efficient coercion-based code for `Generic1` instances rather than a distinct surface-language obligation; the user-visible Kappa question is already covered by the separate note about which metadata-sensitive traits should even permit coercion-based deriving

- GHC [#16362](https://gitlab.haskell.org/ghc/ghc/-/issues/16362) `Deriving a class via an instance that has a TypeError constraint using standalone deriving fails during compilation.`
  - classification: spec-clarity/spec-addition follow-up
  - mapping:
    this strengthens the Kappa `derive via` note: standalone and inline forms should check rejected constraints at the same phase instead of one form eagerly failing on prerequisite `TypeError`-style constraints that the other defers until use

- GHC [#16641](https://gitlab.haskell.org/ghc/ghc/-/issues/16641) `[DerivingVia, GeneralisedNewtypeDeriving] 'Ambiguous type' error when trying to derive an instance for a newtype`
  - classification: spec-clarity/spec-addition follow-up
  - mapping:
    this strengthens the same Kappa `derive via` note: generated member bodies should resolve the source trait instance from the explicit `via` choice rather than relying on ambiguous local inference for nullary members

- GHC [#16958](https://gitlab.haskell.org/ghc/ghc/-/issues/16958) `Derive instances without making the user explicitly demand constraints the compiler already knows about`
  - classification: feature-completion/spec-addition follow-up
  - mapping:
    this strengthens the broader Kappa deriving note around inferred premises: if the implementation can already compute the needed derived constraints from the payload types, the spec should say whether a supported derive form may request them automatically

- GHC [#16179](https://gitlab.haskell.org/ghc/ghc/-/issues/16179) `Mention DerivingStrategies in the warning when DeriveAnyClass and GeneralizedNewtypeDeriving are both enabled`
  - classification: warning-policy/spec-addition follow-up
  - mapping:
    this strengthens the possible Kappa `derive via` / derivation-strategy note: if multiple derivation strategies are simultaneously plausible, the compiler should point users toward making the strategy explicit instead of silently relying on a default

- GHC [#16181](https://gitlab.haskell.org/ghc/ghc/-/issues/16181) `ghc panic when using DerivingVia`
  - classification: implementation/panic background
  - why not imported directly:
    the interesting user-facing obligation is already captured by the `derive via` notes about rejected constraints and source-instance selection; the remaining symptom here is primarily that GHC crashes instead of stopping at the ordinary missing-premise error

- GHC [#16194](https://gitlab.haskell.org/ghc/ghc/-/issues/16194) `deriving, wrong code: newtype T cat a = MkT ((forall xx. cat xx xx) -> a) deriving stock Functor`
  - classification: implementation/soundness background
  - why not imported directly:
    this is a bug in one specific stock-deriving algorithm over higher-rank fields, not a new Kappa surface commitment beyond the existing rule that any supported derive path must elaborate to a valid ordinary instance

- GHC [#16322](https://gitlab.haskell.org/ghc/ghc/-/issues/16322) `"deriving newtype instance" generates an infinite loop`
  - classification: implementation/background
  - why not imported directly:
    this is runtime fallout from one generated newtype instance rather than a distinct portable source-language rule; the spec-facing part remains the existing derive-admissibility and coercion-based-derivation notes

- GHC [#16341](https://gitlab.haskell.org/ghc/ghc/-/issues/16341) `Standalone deriving for GADTs should avoid impossible cases`
  - classification: feature-completion/spec-addition follow-up
  - mapping:
    this strengthens the broader Kappa deriving note: if deriving is eventually supported for indexed/GADT-like data, the generated code should exploit impossible branches from index refinement instead of demanding constraints for unreachable constructors

- GHC [#15839](https://gitlab.haskell.org/ghc/ghc/-/issues/15839) `DerivingStrategies defaulting warning has no associated enable/suppress flag`
  - classification: warning-policy/spec-addition follow-up
  - mapping:
    this strengthens the Kappa derivation-strategy note: if the compiler warns when it chooses a default derivation strategy, that warning family should be explicitly nameable/suppressible rather than hardwired

- GHC [#15932](https://gitlab.haskell.org/ghc/ghc/-/issues/15932) `DeriveFunctor and GeneralizedNewtypeDeriving instances never reporting as covered`
  - classification: tooling/coverage background
  - why not imported directly:
    this is about HPC coverage attribution for generated code rather than a distinct Kappa language-spec obligation

- GHC [#16578](https://gitlab.haskell.org/ghc/ghc/-/issues/16578) `Use dataToTag# to derive Ord instances for enumerations`
  - classification: implementation/performance background
  - why not imported directly:
    this is an optimization/codegen choice for derived enumeration instances, not a new portable surface rule beyond the broader question of which traits are derivable

- GHC [#16655](https://gitlab.haskell.org/ghc/ghc/-/issues/16655) `Warn on ambiguous derivation`
  - classification: warning-policy/spec-addition follow-up
  - mapping:
    this strengthens the Kappa derivation-strategy note: if more than one derivation strategy is semantically plausible, the language should prefer an explicit warning-and-pick-story over silent strategy selection

- GHC [#16714](https://gitlab.haskell.org/ghc/ghc/-/issues/16714) `Bitraversable is not derivable for These`
  - classification: feature-completion/spec-addition follow-up
  - mapping:
    this strengthens the broader Kappa deriving note around role- and variance-sensitive traits: future deriving should say explicitly which traversal/bifunctor-style traits are derivable by coercion and which are intentionally excluded

- GHC [#16923](https://gitlab.haskell.org/ghc/ghc/-/issues/16923) `DerivingVia does not typecheck the via type when deriving an empty list of classes`
  - classification: spec-clarity/spec-addition follow-up
  - mapping:
    this strengthens the Kappa `derive via` note: malformed `via` types and empty derive lists should be rejected up front, and the `via` type should be checked exactly once per clause rather than opportunistically per derived trait

- GHC [#17013](https://gitlab.haskell.org/ghc/ghc/-/issues/17013) `GHC accepts derived instances that violate functional dependencies`
  - classification: spec-clarity/spec-addition follow-up
  - mapping:
    this strengthens the broader Kappa deriving note: any future derive surface should remain subject to the same admissibility/coherence checks as the corresponding explicit instance, rather than letting generation bypass determinism constraints that a handwritten instance would fail

- GHC [#17014](https://gitlab.haskell.org/ghc/ghc/-/issues/17014) `DerivingStrategies should not require individual strategy extensions to be enabled`
  - classification: ergonomics/spec-addition follow-up
  - mapping:
    this strengthens the Kappa derivation-strategy note: if the language adds explicit derive strategies, naming one should be a source-level choice with clear semantics, not an accidental side effect of enabling or disabling unrelated implicit-defaulting behavior

- GHC [#17183](https://gitlab.haskell.org/ghc/ghc/-/issues/17183) `-XDerivingVia (and deriving strategies) is under specified in the manual`
  - classification: spec-clarity/spec-addition follow-up
  - mapping:
    this strengthens the Kappa `derive via` note: if Kappa adds explicit derive strategies or `derive via`, the concrete syntax, scoping, and typing rules should appear in the spec as normative text rather than only through examples

- GHC [#17210](https://gitlab.haskell.org/ghc/ghc/-/issues/17210) `Parameterizing Stock instance deriving`
  - classification: feature-completion/spec-addition follow-up
  - mapping:
    this suggests a plausible future Kappa ergonomics feature: a projection-based derive-customization surface for ordinary equality/ordering/hash traits, where the derived instance uses an explicit pure view function instead of forcing a wrapper type or handwritten instance

- GHC [#17312](https://gitlab.haskell.org/ghc/ghc/-/issues/17312) `Potentially unnecessary warning about missing deriving strategies`
  - classification: warning-policy/spec-addition follow-up
  - mapping:
    this strengthens the Kappa derivation-strategy note: if the compiler warns about omitted derive strategies, it should do so only when more than one strategy is actually viable rather than warning in trivially unambiguous cases

- GHC [#17767](https://gitlab.haskell.org/ghc/ghc/-/issues/17767) `how does derivingVia interact with class constraints or quantified constraints?`
  - classification: feature-completion/spec-addition follow-up
  - mapping:
    this strengthens the Kappa `derive via` note: coercion-based derivation needs explicit rules for extra method-local premises, source-instance selection under additional constraints, and role/coercibility restrictions instead of pretending every representational wrapper can derive every constrained method automatically

- GHC [#17880](https://gitlab.haskell.org/ghc/ghc/-/issues/17880) `Refactor DeriveFunctor to handle rank-n field types better`
  - classification: implementation/soundness background
  - why not imported directly:
    this is a bug in one derived-code generator over higher-rank fields, not a distinct Kappa surface obligation beyond the broader rule that any supported derive path must elaborate to valid ordinary code

- GHC [#17899](https://gitlab.haskell.org/ghc/ghc/-/issues/17899) `Code generated by GND/DerivingVia should use InstanceSigs`
  - classification: diagnostic-quality/spec-addition follow-up
  - mapping:
    this suggests a small but transferable Kappa requirement: if the implementation surfaces generated derived members in diagnostics or dumps, it should preserve the generated member signatures explicitly instead of only showing coercion-heavy right-hand sides

- GHC [#18047](https://gitlab.haskell.org/ghc/ghc/-/issues/18047) `Deriving quantified methods`
  - classification: feature-completion/spec-addition follow-up
  - mapping:
    this strengthens the Kappa `derive via` note: if derived members may carry their own extra premises, the spec should say whether coercion-based derivation may synthesize those members only when the needed method-local constraints are preserved transparently

- GHC [#18130](https://gitlab.haskell.org/ghc/ghc/-/issues/18130) `Don't require parentheses for via type`
  - classification: ergonomics/spec-addition follow-up
  - mapping:
    this strengthens the Kappa `derive via` syntax note: if the surface includes explicit `via` types, the accepted parenthesization and annotation forms should be specified deliberately instead of left as parser accidents

- GHC [#18148](https://gitlab.haskell.org/ghc/ghc/-/issues/18148) `ConstrainedClassMethods can trigger un-actionable redundant constraint warnings together with GND`
  - classification: warning-policy/spec-addition follow-up
  - mapping:
    this suggests a general Kappa warning-hygiene rule for synthesized deriving code: hidden generated members should not introduce redundant-constraint or similar lints that the user cannot actually repair in source

- GHC [#18165](https://gitlab.haskell.org/ghc/ghc/-/issues/18165) `Data definitions with deriving don't get warnings about unused definitions`
  - classification: warning-policy/spec-addition follow-up
  - mapping:
    this reinforces the same synthesized-warning-hygiene note: generated derives should not count as meaningful user uses that suppress an otherwise helpful unused-definition warning

- GHC [#18219](https://gitlab.haskell.org/ghc/ghc/-/issues/18219) `Relax inferred context simplification to allow "exotic" contexts for GeneralizedNewtypeDeriving and DerivingVia instances.`
  - classification: feature-completion/spec-addition follow-up
  - mapping:
    this strengthens the Kappa `derive via` note: if coercion-based derivation infers valid but nontrivial premises from the source/via instance, the spec should say whether those premises are preserved transparently or rejected by a stricter simplification policy

- GHC [#18258](https://gitlab.haskell.org/ghc/ghc/-/issues/18258) `DerivingVia: check via type with empty class list, allow omitting empty class list`
  - classification: ergonomics/spec-addition follow-up
  - mapping:
    this strengthens the Kappa `derive via` note: the language should decide deliberately whether an empty-class `via` clause is rejected, checked as a pure representation-compatibility witness, or surfaced by a distinct syntax, rather than leaving the corner case to parser accident

- GHC [#18271](https://gitlab.haskell.org/ghc/ghc/-/issues/18271) `Visible dependent quantification permitted in standalone deriving declarations`
  - classification: spec-clarity/spec-addition follow-up
  - mapping:
    this strengthens the Kappa standalone-deriving side of the `derive via` note: standalone forms should admit only the same binder forms and quantification categories as the corresponding ordinary instance surface, with no extra parser/elaborator loopholes

- GHC [#18321](https://gitlab.haskell.org/ghc/ghc/-/issues/18321) `Core Lint error when deriving Ix/Enum instances in separate TyClGroups`
  - classification: implementation/pipeline background
  - why not imported directly:
    this is backend/elaboration fallout from declaration-group staging and Template Haskell interaction, not a distinct Kappa source-language rule beyond the ordinary expectation that deriving remains robust under declaration grouping

- GHC [#18388](https://gitlab.haskell.org/ghc/ghc/-/issues/18388) `DerivingVia fails to roundtrip through TH`
  - classification: macro/template background
  - why not imported directly:
    the transferable concern is binder preservation through quoted declaration roundtrips, but this issue is tied to GHC's Template Haskell declaration encoding rather than a direct current-spec Kappa surface obligation

- GHC [#18474](https://gitlab.haskell.org/ghc/ghc/-/issues/18474) `Standalone deriving fails for GADTs when unifying type variables within a context`
  - classification: feature-completion/spec-addition follow-up
  - mapping:
    this strengthens the broader Kappa deriving note: if deriving is supported for indexed/GADT-like data, generated code should exploit impossible branches and contextual equalities instead of generating inaccessible patterns that fail to typecheck

- GHC [#18483](https://gitlab.haskell.org/ghc/ghc/-/issues/18483) `DerivingVia: Permit deriving instances at unreduced types`
  - classification: feature-completion/spec-addition follow-up
  - mapping:
    this strengthens the Kappa `derive via` note: the spec should say whether a via/source type may remain at an unreduced but well-formed family or associated-type expression instead of demanding eager normalization before derivation

- GHC [#18488](https://gitlab.haskell.org/ghc/ghc/-/issues/18488) `DerivingVia: Kinding information isn't propagated`
  - classification: spec-clarity/spec-addition follow-up
  - mapping:
    this strengthens the Kappa `derive via` note: all kind information available from the whole standalone derivation should participate in checking the generated instance rather than only the via type in isolation

- GHC [#18820](https://gitlab.haskell.org/ghc/ghc/-/issues/18820) `misspelled internal link in documentation of 9.6 Extensions of the deriving mechanism`
  - classification: docs-only background
  - why not imported directly:
    this is a pure documentation typo in GHC's manual rather than a distinct Kappa language or tooling obligation

- GHC [#18874](https://gitlab.haskell.org/ghc/ghc/-/issues/18874) `Deriving multiple classes in standalone (or document this limitation)`
  - classification: ergonomics/spec-addition follow-up
  - mapping:
    this suggests a plausible future Kappa ergonomics feature: grouped standalone deriving for several traits that share the same target type and the same `via` or strategy payload

- GHC [#18914](https://gitlab.haskell.org/ghc/ghc/-/issues/18914) `Out-of-scope type variables when combining GeneralizedNewtypeDeriving with higher-rank type synonyms`
  - classification: spec-clarity/spec-addition follow-up
  - mapping:
    this strengthens the Kappa `derive via` note: generated members must preserve higher-rank binder scope from source synonyms or equivalent source-level abstractions instead of inventing out-of-scope binders during elaboration

- GHC [#19079](https://gitlab.haskell.org/ghc/ghc/-/issues/19079) `DerivingVia: incorrectly accepts deriving via types with differing runtime representations`
  - classification: spec-clarity/spec-addition follow-up
  - mapping:
    this strengthens the Kappa `derive via` note: coercion-based derivation must require actual runtime-representation compatibility for the source and target types, not merely method-wise compatibility for one particular trait

- GHC [#19141](https://gitlab.haskell.org/ghc/ghc/-/issues/19141) `Standalone deriving for GADT data family instance now produces bogus case that results in type error`
  - classification: feature-completion/spec-addition follow-up
  - mapping:
    this strengthens the broader Kappa indexed-deriving note: data-family-style indexed instances should receive the same impossibility pruning as ordinary indexed/GADT-like data instead of generating impossible branches

- GHC [#19418](https://gitlab.haskell.org/ghc/ghc/-/issues/19418) `Support overriding "No instance" errors using the default deriving strategy`
  - classification: feature-completion/spec-addition follow-up
  - mapping:
    this strengthens the broader future-deriving note: if derivation over associated families or descriptor-indexed fields needs extra premises, the language should say whether those premises may be inferred, overridden, or stated explicitly instead of only failing with a raw missing-instance error

- GHC [#19692](https://gitlab.haskell.org/ghc/ghc/-/issues/19692) `Don't suggest turning on \`DeriveAnyClass\` when an instance cannot be derived`
  - classification: diagnostic-quality/spec-addition follow-up
  - mapping:
    this strengthens the Kappa derive-diagnostic note: repair suggestions around deriving should not recommend a feature or strategy that merely trades one compile-time failure for missing-method or runtime-hole behavior

- GHC [#19865](https://gitlab.haskell.org/ghc/ghc/-/issues/19865) `Generalized newtype deriving can cause spurious redundant constraint warning`
  - classification: warning-policy/spec-addition follow-up
  - mapping:
    this reinforces the synthesized-warning-hygiene note: hidden generated deriving code should not emit redundant-constraint warnings that the user cannot meaningfully fix in source

- GHC [#20054](https://gitlab.haskell.org/ghc/ghc/-/issues/20054) `Redundant constraint triggered by deriving via`
  - classification: warning-policy/spec-addition follow-up
  - mapping:
    this reinforces the same synthesized-warning-hygiene note for `derive via`: warning passes should treat generated coercion-heavy helper terms as elaboration artifacts, not as independent user-authored sources of redundant-constraint blame

- GHC [#20223](https://gitlab.haskell.org/ghc/ghc/-/issues/20223) `Deriving Via complains 'not a unary constraint, as expected by a deriving clause' on reasonable code`
  - classification: spec-clarity/spec-addition follow-up
  - mapping:
    this strengthens the Kappa `derive via` note: the spec should say explicitly which trait-head shapes are admissible derivation targets, especially for higher-kinded or transformer-style traits, instead of leaving the boundary as an under-documented parser/typechecker artifact

- GHC [#20314](https://gitlab.haskell.org/ghc/ghc/-/issues/20314) `feature request: deriving for Data.Bifoldable Data.Bifunctor Data.Bitraversable`
  - classification: feature-completion/spec-addition follow-up
  - mapping:
    this strengthens the broader future-deriving note around traversal/bifunctor-style traits: if Kappa grows derive support beyond the current baseline, it should say explicitly which bifunctor/bifoldable/bitraversable-style traits are intentionally derivable

- GHC [#20375](https://gitlab.haskell.org/ghc/ghc/-/issues/20375) `Deriving fails for type parameterized with unlifted type`
  - classification: feature-completion/spec-addition follow-up
  - mapping:
    this strengthens the broader future-deriving note: if Kappa ever supports derive over unlifted or representation-indexed payload types, the substitution and admissibility rules should be stated explicitly rather than silently falling back to the unconstrained type parameter

- GHC [#20387](https://gitlab.haskell.org/ghc/ghc/-/issues/20387) ``isUnliftedType` panic with derived Generic1 instance for runtime-polymorphic newtype`
  - classification: implementation/soundness background
  - why not imported directly:
    this is a specific compiler panic in one `Generic1`-deriving path; the spec-facing part is already covered by the broader note about whether derivation is supported for runtime-representation-polymorphic or unlifted payloads at all

- GHC [#20501](https://gitlab.haskell.org/ghc/ghc/-/issues/20501) `deriving-inferred contexts for types with DatatypeContexts are not as general as they should be`
  - classification: spec-clarity/spec-addition follow-up
  - mapping:
    this strengthens the broader future-deriving note: inferred deriving contexts should be no stronger than the weakest explicit instance that the constructor shapes actually justify, even for deprecated or legacy surface forms

- GHC [#20524](https://gitlab.haskell.org/ghc/ghc/-/issues/20524) `DerivingVia and GND should not need UnboxedTuples`
  - classification: spec-clarity/spec-addition follow-up
  - mapping:
    this strengthens the Kappa `derive via` note: generated code should not force users to enable unrelated syntax extensions merely because a derived method's signature mentions an advanced representation-sensitive form that is already present in an imported trait

- GHC [#20465](https://gitlab.haskell.org/ghc/ghc/-/issues/20465) `Overlapping instances and poly-kinded types`
  - classification: diagnostic-quality/spec-addition follow-up
  - mapping:
    this strengthens the hidden-structure diagnostic note: overlap errors should expose the hidden kind-level structure that makes competing instances ambiguous instead of printing an empty instantiation site or hiding one of the competing instances

- GHC [#20466](https://gitlab.haskell.org/ghc/ghc/-/issues/20466) `Overlapping instances and type families`
  - classification: diagnostic-quality/spec-addition follow-up
  - mapping:
    this strengthens the same hidden-structure diagnostic note for type families: if overlap depends on an unreduced family application, the diagnostic should say so directly rather than rendering an empty "depends on the instantiation of ..." explanation

- GHC [#20527](https://gitlab.haskell.org/ghc/ghc/-/issues/20527) `GHC 9.2 rejects certain poly-kinded unlifted newtype instances`
  - classification: kinding/runtime-representation background
  - why not imported directly:
    this is about GHC's representation-polymorphic newtype-instance acceptance path rather than a direct Kappa surface obligation; the closest Kappa-facing future note about unlifted or representation-indexed derivation admissibility is already covered by broader deriving follow-ups

- GHC [#20529](https://gitlab.haskell.org/ghc/ghc/-/issues/20529) `Instances become invisible across a component boundary`
  - classification: import/interface background
  - why not imported directly:
    this is about GHC instance visibility during cross-component Template Haskell reification, which is adjacent to interface/import machinery but not a direct current-spec Kappa import target

- GHC [#20538](https://gitlab.haskell.org/ghc/ghc/-/issues/20538) `Strange Behavior with QuanitifiedConstraints, DerivingStrategies, and GND`
  - classification: spec-clarity/spec-addition follow-up
  - mapping:
    this strengthens the Kappa `derive via` note: if inline and standalone coercion-based deriving forms are both supported, they should elaborate equivalently for the same admissible strategy/target pair even when quantified constraints and role-sensitive coercions participate

- GHC [#20542](https://gitlab.haskell.org/ghc/ghc/-/issues/20542) `Overlapping/Overlappable doesn't seem to do anything?`
  - classification: diagnostic-quality/spec-addition follow-up
  - mapping:
    this strengthens the same hidden-structure/overlap-diagnostic note as `#20465` and `#20466`: if competing instances are ambiguous because neither is strictly more specific, the error should say that directly instead of leaving users to infer the specificity rule from a raw overlap failure

- GHC [#20584](https://gitlab.haskell.org/ghc/ghc/-/issues/20584) `Several Hackage libraries fail to compile due to kind variables not defaulting in HEAD`
  - classification: kind-defaulting / compiler-policy background
  - why not imported directly:
    this is about a GHC policy/regression around implicit runtime-representation/kind defaulting in type-family declarations; Kappa's direct spec obligation is to keep kinding/defaulting rules explicit rather than importing this specific fallback behavior

- GHC [#20595](https://gitlab.haskell.org/ghc/ghc/-/issues/20595) `Exhaustive check does not show enough pattern information for view patterns`
  - classification: diagnostic-quality/spec-addition follow-up
  - mapping:
    this strengthens the Kappa exhaustiveness-diagnostic note: reported missing cases should preserve enough residual pattern detail after lowering to remain actionable

- GHC [#20602](https://gitlab.haskell.org/ghc/ghc/-/issues/20602) `Redundant-constraints warning complains about the wrong constraint`
  - classification: diagnostic-quality/spec-addition follow-up
  - mapping:
    this strengthens a general warning-quality note for Kappa: redundancy warnings should blame the actually redundant premise rather than another premise it happens to imply

- GHC [#20627](https://gitlab.haskell.org/ghc/ghc/-/issues/20627) `The ‘Type’ kind is printed without accounting for scope`
  - classification: diagnostic-quality/spec-addition follow-up
  - mapping:
    this strengthens the pretty-printing hygiene note: rendered built-in type/kind names should qualify themselves when shadowed by in-scope user declarations

- GHC [#20630](https://gitlab.haskell.org/ghc/ghc/-/issues/20630) `ApplicativeDo breaks uses of ImplicitParameters`
  - classification: lexical-implicit / desugaring background
  - mapping:
    this is a useful reminder that any future statement-reordering or applicative regrouping in Kappa must preserve lexical implicit scope exactly, but Kappa does not currently import Haskell `ApplicativeDo` surface directly

- GHC [#20631](https://gitlab.haskell.org/ghc/ghc/-/issues/20631) `Make the pattern match exhaustiveness checker smarter about UnliftedDatatypes`
  - classification: impossible-branch checker background
  - why not imported directly:
    this is about GHC unlifted-datatype emptiness reasoning rather than a current Kappa surface obligation; the transferable part is the general impossible-branch pruning principle already represented by existing indexed/exhaustiveness notes

- GHC [#20633](https://gitlab.haskell.org/ghc/ghc/-/issues/20633) `Wrong "Pattern match is redundant" in 9.2.1`
  - classification: diagnostic-quality/spec-addition follow-up
  - mapping:
    this strengthens the general match-diagnostic quality bucket: redundancy warnings should only fire when the checker can justify actual inaccessibility, not from a spurious over-approximation of prior cases

- GHC [#20642](https://gitlab.haskell.org/ghc/ghc/-/issues/20642) `-Wcomplete-uni-pattern: GHC 8.10 regression in error message`
  - classification: diagnostic-quality/spec-addition follow-up
  - mapping:
    this strengthens the exhaustiveness-detail note: missing-pattern reports should prefer the smallest informative residual shape instead of exploding one real missing case into unnecessary case splits

- GHC [#20643](https://gitlab.haskell.org/ghc/ghc/-/issues/20643) `Per-definition control over pattern completeness (= coverage) checker`
  - classification: ergonomics/spec-addition follow-up
  - mapping:
    this suggests a plausible future Kappa ergonomics feature: fine-grained per-definition warning suppression instead of forcing module-wide suppression for one intentionally partial definition

- GHC [#20654](https://gitlab.haskell.org/ghc/ghc/-/issues/20654) `Implicit parameters with higher rank types are incorrectly rejected by the parser`
  - classification: parser/syntax background
  - why not imported directly:
    this is a Haskell parser-parenthesization quirk around implicit-parameter constraint syntax, not a new Kappa surface obligation; the transferable point is only that higher-rank implicit types should not depend on accidental parser precedence

- GHC [#20661](https://gitlab.haskell.org/ghc/ghc/-/issues/20661) `hs-boot files don't support boring classes`
  - classification: import/interface background
  - why not imported directly:
    this is about GHC `hs-boot` / `{-# SOURCE #-}` interface matching for empty classes, which is adjacent to interface machinery but not a direct Kappa language-surface import target

- GHC [#20666](https://gitlab.haskell.org/ghc/ghc/-/issues/20666) `Regression in superclass checking of instances`
  - classification: superclass-checking / soundness background
  - why not imported directly:
    the interesting part here is that the older accepted behavior turned out to be unsound; Kappa already needs ordinary explicit superclass-obligation checking, so this serves as background against being overly permissive rather than as a new direct test target

- GHC [#20675](https://gitlab.haskell.org/ghc/ghc/-/issues/20675) `[regression] ghc 9.2.1 complains about "Uninferrable type variables" that ghc 9.0.1 can infer fine`
  - classification: kind-defaulting/spec-addition follow-up
  - mapping:
    this strengthens the Kappa defaulting-policy note: if unresolved kind/representation variables in aliases or inferred declarations are defaulted at all, the phases and admissible defaults should be specified directly instead of changing as an implementation artifact

- GHC [#20686](https://gitlab.haskell.org/ghc/ghc/-/issues/20686) `Make the design of defaulting explicit`
  - classification: kind-defaulting/spec-addition follow-up
  - mapping:
    this is direct evidence for the same Kappa note: defaulting of kind, representation, or similar non-user-written inference variables should be a documented design with clear phases and scope, not an emergent implementation convention

- GHC [#20688](https://gitlab.haskell.org/ghc/ghc/-/issues/20688) `Deriving Lift is not -Wimplicit-lift friendly`
  - classification: warning-policy/spec-addition follow-up
  - mapping:
    this strengthens the synthesized-warning-hygiene note: generated derive code should not trigger source-oriented warnings such as implicit-lift complaints in ways the user cannot repair directly

- GHC [#20691](https://gitlab.haskell.org/ghc/ghc/-/issues/20691) `Lots of missed specialisation warnings with GHC 9.2.1`
  - classification: optimization-warning background
  - why not imported directly:
    this is about the precision of GHC specialization diagnostics rather than a direct Kappa language rule; it is useful general warning-policy background but not a current spec-import target

- GHC [#20696](https://gitlab.haskell.org/ghc/ghc/-/issues/20696) `needsTemplateHaskellOrQQ is not very precise`
  - classification: tooling/build-graph background
  - why not imported directly:
    this is about build-graph precision for enabling dynamic codegen paths, not source-language behavior or a direct Kappa semantics obligation

- GHC [#20703](https://gitlab.haskell.org/ghc/ghc/-/issues/20703) `GHC wrongly(?) reports inaccessible rhs with patsyns, strict field, COMPLETE pragma`
  - classification: diagnostic-quality/spec-addition follow-up
  - mapping:
    this strengthens the general match-diagnostic quality note: inaccessible-rhs or redundant-branch warnings should only fire when the checker can justify real unreachability, even in the presence of pattern synonyms, COMPLETE sets, and strict fields

- GHC [#20719](https://gitlab.haskell.org/ghc/ghc/-/issues/20719) `Ambiguous tyvar when deriving a class with default signature involving type synonym to rank-N type`
  - classification: feature-completion/spec-addition follow-up
  - mapping:
    this strengthens the broader future-deriving note: if Kappa later supports richer derive/default-member synthesis, alias-expanded higher-rank default member signatures should behave the same under inline sugar as in an explicit instance

- GHC [#20723](https://gitlab.haskell.org/ghc/ghc/-/issues/20723) `Parse errors on OverloadedRecordDot`
  - classification: parser/syntax background
  - why not imported directly:
    this is a parser keyword-classification issue in Haskell's record-dot surface rather than a current Kappa spec obligation; the transferable lesson is only that member-projection sugar should not accidentally reserve ordinary identifier spellings

- GHC [#20737](https://gitlab.haskell.org/ghc/ghc/-/issues/20737) `Warn when an equality constraint comes into scope without -XMonoLocalBinds`
  - classification: local-generalization / equality-given background
  - why not imported directly:
    this is specific to GHC's interaction between equality givens and local generalization defaults; Kappa can treat it as background for future inference-policy documentation rather than a direct current-spec import

- GHC [#20762](https://gitlab.haskell.org/ghc/ghc/-/issues/20762) `Display more information about why an extension is enabled/disabled`
  - classification: diagnostic-quality/spec-addition follow-up
  - mapping:
    this strengthens the extension-provenance tooling note: extension-sensitive messages should be able to explain not just which mode is active, but where that mode came from

- GHC [#20808](https://gitlab.haskell.org/ghc/ghc/-/issues/20808) `Hole-fit suggestions don't include implicit parameters`
  - classification: tooling/spec-addition follow-up
  - mapping:
    this strengthens the hole-fit tooling note: in-scope implicit evidence should appear in hole suggestions and similar typed-completion surfaces just like ordinary explicit values

- GHC [#20818](https://gitlab.haskell.org/ghc/ghc/-/issues/20818) `Simplified subsumption hurts ImplicitParams usability`
  - classification: implicit-parameter / subsumption background
  - why not imported directly:
    this depends on Haskell's implicit-parameter constraint model and simplified subsumption rules; the transferable Kappa lesson is only to avoid eta-expansion-sensitive behavior for implicit evidence, which is broader than a single direct fixture

- GHC [#20815](https://gitlab.haskell.org/ghc/ghc/-/issues/20815) `Make coerce-derived dictionaries coercible`
  - classification: feature-completion/spec-addition follow-up
  - mapping:
    this strengthens the Kappa `derive via` note: if coercion-based deriving is ever exposed, the spec should say whether coercing an instance package as a whole is permitted and how derived prerequisite dictionaries are transported across the coercion

- GHC [#20821](https://gitlab.haskell.org/ghc/ghc/-/issues/20821) ``coerce`ing a function should be possible without type applications when we can newtype derive the method`
  - classification: feature-completion/spec-addition follow-up
  - mapping:
    this strengthens the same coercion-based deriving note: the user-facing `coerce` story and the automatically synthesized deriving story should not diverge gratuitously when both are performing the same representational transport

- GHC [#20835](https://gitlab.haskell.org/ghc/ghc/-/issues/20835) `Implement Unsatisfiable as an alternative to TypeError`
  - classification: feature-completion/spec-addition follow-up
  - mapping:
    this suggests a plausible future Kappa surface distinction between "this constraint is unsatisfiable" and richer custom error-computation mechanisms, instead of forcing both stories through one `TypeError`-like hook

- GHC [#20860](https://gitlab.haskell.org/ghc/ghc/-/issues/20860) `Multiple occurrence of the same out-of-scope identifier should be commoned up`
  - classification: diagnostic-quality/spec-addition follow-up
  - mapping:
    this strengthens the unresolved-name diagnostic note: repeated uses of one unresolved identifier should be reported with a commoned-up or at least consistent inferred problem shape rather than as unrelated fresh misses

- GHC [#20873](https://gitlab.haskell.org/ghc/ghc/-/issues/20873) `Using type-aliases in kind signatures requires DataKinds`
  - classification: kind-syntax / parser-policy background
  - why not imported directly:
    this is about a GHC extension boundary around aliases in standalone kind signatures; the transferable Kappa concern is just that alias expansion in type/kind syntax should not depend on accidental parser extension coupling

- GHC [#20883](https://gitlab.haskell.org/ghc/ghc/-/issues/20883) `Different -Woverlapping-patterns warnings in GHCi versus compiled code`
  - classification: diagnostic-quality/spec-addition follow-up
  - mapping:
    this strengthens the warning-consistency note: interactive and batch checking should not silently diverge on ordinary source-based redundancy/overlap warnings

- GHC [#20893](https://gitlab.haskell.org/ghc/ghc/-/issues/20893) `TH pretty-printer emits syntactically incorrect case statements inside do blocks`
  - classification: tooling/spec-addition follow-up
  - mapping:
    this strengthens the quoted-syntax round-trip note: pretty-printers for quoted declarations/expressions should emit valid source even for nested block constructs

- GHC [#20895](https://gitlab.haskell.org/ghc/ghc/-/issues/20895) `TcPlugins: newWanted discards the source location`
  - classification: tooling/spec-addition follow-up
  - mapping:
    this strengthens the plugin/tooling provenance note: helper APIs that emit derived obligations should preserve the originating source location unless the caller explicitly redirects it

- GHC [#20902](https://gitlab.haskell.org/ghc/ghc/-/issues/20902) `TH's mkName fails to check namespace for built-in syntax`
  - classification: tooling/spec-addition follow-up
  - mapping:
    this strengthens the built-in-name namespace-hygiene note: raw name-construction or quoting helpers should respect namespace distinctions even for built-in syntax names

- GHC [#20910](https://gitlab.haskell.org/ghc/ghc/-/issues/20910) `:type does not print the correct kinds for levity-polymorphic primops`
  - classification: tooling/spec-addition follow-up
  - mapping:
    this strengthens the semantic-query rendering note: printed types should include the levity/runtime-representation structure needed for correctness instead of showing a misleading simplified signature

- GHC [#20916](https://gitlab.haskell.org/ghc/ghc/-/issues/20916) `GHC accepts two names for the same kind variable`
  - classification: spec-clarity/spec-addition follow-up
  - mapping:
    this strengthens the scoped-binder naming note: if signatures and declaration headers can both introduce kind variables, the language should say when later names are aliases versus fresh binders instead of accepting contradictory spellings inconsistently

- GHC [#20921](https://gitlab.haskell.org/ghc/ghc/-/issues/20921) `Regression in ambiguity checking for partial type signatures in GHC 9.2`
  - classification: partial-signature / ambiguity-checking background
  - why not imported directly:
    this is tied to GHC partial type signatures and their ambiguity checker; the transferable lesson is mainly diagnostic quality around fresh/internal variables rather than a current Kappa direct-fit surface

- GHC [#20926](https://gitlab.haskell.org/ghc/ghc/-/issues/20926) `Inconsistency between SAKs and inference`
  - classification: spec-clarity/spec-addition follow-up
  - mapping:
    this strengthens the same scoped-binder naming note: standalone signatures and inference should agree about whether later binder names are aliases for earlier scoped variables or fresh binders

- GHC [#20939](https://gitlab.haskell.org/ghc/ghc/-/issues/20939) `Implicit parameters can’t be polymorphic even if QuantifiedConstraints and ImpredicativeTypes are enabled; if this is intentional, it is poorly communicated`
  - classification: implicit-parameter / quantified-polymorphism background
  - why not imported directly:
    this depends on Haskell's implicit-parameter constraint model plus impredicative/quantified-constraint interactions; for Kappa it mainly serves as background that any unsupported polymorphic-implicit story should be stated clearly

- GHC [#20942](https://gitlab.haskell.org/ghc/ghc/-/issues/20942) `Inferred context depends on whether instances are in scope`
  - classification: inferred-context / instance-environment background
  - why not imported directly:
    this is about GHC's partial-signature/context-simplification heuristics varying with ambient instance scope, which is useful inference-policy background but not a direct current-spec Kappa import

- GHC [#20962](https://gitlab.haskell.org/ghc/ghc/-/issues/20962) `Bad error message under DisambiguateRecordFields`
  - classification: diagnostic-quality/spec-addition follow-up
  - mapping:
    this strengthens the hidden-structure diagnostic note: when field selection or update fails because the chosen constructor/record simply does not provide that field, the primary diagnostic should prefer that direct structural explanation over a downstream fallback ambiguity story

- GHC [#20974](https://gitlab.haskell.org/ghc/ghc/-/issues/20974) `GHCi >= 9.2.1 prints type signatures containing type families in an ugly way`
  - classification: tooling/spec-addition follow-up
  - mapping:
    this strengthens the semantic-query rendering note: type/query surfaces should simplify ordinary type-family or constraint structure enough to stay readable instead of exposing an unnecessarily raw internal form

- GHC [#20981](https://gitlab.haskell.org/ghc/ghc/-/issues/20981) `Error messages and constraints can easily be lost by accident`
  - classification: tooling/spec-addition follow-up
  - mapping:
    this strengthens the plugin/tooling provenance note: helper APIs that move or discharge obligations should preserve the active error/constraint payload by default instead of silently routing it into state that no later reporter inspects

- GHC [#21006](https://gitlab.haskell.org/ghc/ghc/-/issues/21006) `Unhelpful Kind equality error at the start of file`
  - classification: diagnostic-quality/spec-addition follow-up
  - mapping:
    this strengthens the hidden-structure diagnostic note: kind-equality failures should reveal the concrete hidden kind mismatch rather than collapsing to an effectively content-free top-of-file error

- GHC [#21012](https://gitlab.haskell.org/ghc/ghc/-/issues/21012) `Suggest replacing -> with =>`
  - classification: diagnostic-quality/spec-addition follow-up
  - mapping:
    this strengthens the standard fix-it note: parser and early front-end diagnostics should surface obvious source-preserving repair suggestions when one token-level substitution is overwhelmingly likely

- GHC [#21030](https://gitlab.haskell.org/ghc/ghc/-/issues/21030) `Redundant constraints sometimes inferred and/or required`
  - classification: diagnostic-quality/spec-addition follow-up
  - mapping:
    this strengthens the redundancy-warning note: user-facing inferred or suggested signatures should simplify away constraints already implied by visible supertrait or equality information instead of presenting them as if they were independently necessary

- GHC [#21031](https://gitlab.haskell.org/ghc/ghc/-/issues/21031) `Potentially confusing error message suggests DataKinds`
  - classification: diagnostic-quality/spec-addition follow-up
  - mapping:
    this strengthens the standard fix-it note: when a local syntax repair such as adding tuple parentheses is more plausible than enabling a feature, the diagnostic should rank the local repair first instead of leading with a broad feature-enabling hint

- GHC [#21063](https://gitlab.haskell.org/ghc/ghc/-/issues/21063) `HLS Structured Errors Integration`
  - classification: tooling/spec-addition follow-up
  - mapping:
    this motivates the structured diagnostic-record note: editor tooling should receive stable typed fields for inferred signatures, expected/actual mismatches, and repair facts rather than scraping human prose

- GHC [#21086](https://gitlab.haskell.org/ghc/ghc/-/issues/21086) `Better error messages for Num Literals`
  - classification: diagnostic-quality/spec-addition follow-up
  - mapping:
    this motivates the numeric-literal mismatch note: literal elaboration failures should foreground the expected user-visible type/domain and relegate missing-instance details to supporting context

- GHC [#21088](https://gitlab.haskell.org/ghc/ghc/-/issues/21088) `Inconsistent instantiation of inferred type variables when they don't all come first`
  - classification: tooling/spec-addition follow-up
  - mapping:
    this strengthens the semantic-query rendering note: presentation-time instantiation of inferred binders should be coherent rather than leaving a confusing partially instantiated quantifier prefix

- GHC [#21087](https://gitlab.haskell.org/ghc/ghc/-/issues/21087) `KnownNat and friends are derivable`
  - classification: spec-addition follow-up
  - mapping:
    this strengthens the future `derive via` admissibility note: some trusted singleton-evidence or compiler-reserved witness traits should be explicitly non-derivable even if the member types appear coercible

- GHC [#21092](https://gitlab.haskell.org/ghc/ghc/-/issues/21092) `Make Type not apart from Constraint`
  - classification: kind/system soundness background
  - why not imported directly:
    this is a soundness consequence of GHC identifying `Type` and `Constraint` too aggressively in one internal layer while separating them in another; Kappa's spec already treats `Constraint` descriptors as a distinct intrinsic compile-time sort with separate evidence, so this is mainly background confirmation for keeping that boundary explicit

- GHC [#21098](https://gitlab.haskell.org/ghc/ghc/-/issues/21098) `Report an error when applying a type family associated with a class to a type that's not an instance of the class`
  - classification: diagnostic-quality/spec-addition follow-up
  - mapping:
    this strengthens the associated-member normalization note: stuck associated-family applications should name the missing owner instance directly instead of remaining silently stuck or surfacing only an unreduced family head

- GHC [#21100](https://gitlab.haskell.org/ghc/ghc/-/issues/21100) `New wording for error message around associated type families`
  - classification: diagnostic-quality/spec-addition follow-up
  - mapping:
    this motivates the undeclared associated-member definition note: diagnostics should explain the missing declaration form and how a default equation relates to it, not merely state that the name is not an associated member

- GHC [#21101](https://gitlab.haskell.org/ghc/ghc/-/issues/21101) `Error message text for invalid record wildcard match`
  - classification: diagnostic-quality/spec-addition follow-up
  - mapping:
    this strengthens the standard fix-it note: invalid record wildcard patterns should come with concrete source-level rewrites such as dropping the wildcard or switching to an explicit empty-record form

- GHC [#21102](https://gitlab.haskell.org/ghc/ghc/-/issues/21102) `GADT accepted without -XGADTs`
  - classification: spec-clarity/spec-addition follow-up
  - mapping:
    this motivates the language-profile/extension-gating note: a syntax-only gate such as GADT-style declaration syntax must not accidentally admit the stronger semantic feature if that feature also changes generalization or other typing behavior

- GHC [#21103](https://gitlab.haskell.org/ghc/ghc/-/issues/21103) `Non-Haskell98 constructors accepted in Haskell98 mode`
  - classification: spec-clarity/spec-addition follow-up
  - mapping:
    this strengthens the same language-profile gating note: restrictive source profiles should reject out-of-profile constructor forms even if the parser happens to accept them

- GHC [#21104](https://gitlab.haskell.org/ghc/ghc/-/issues/21104) `Move ExplicitForAll checks out of the parser`
  - classification: diagnostic-quality/spec-addition follow-up
  - mapping:
    this strengthens the same gating note: feature ownership should live in one semantic checking layer so tooling/recovery can parse broadly while diagnostics still point at the real missing feature gate

- GHC [#21126](https://gitlab.haskell.org/ghc/ghc/-/issues/21126) `Definition of heterogeneous equality rejected unless using a SAKS`
  - classification: spec-clarity/spec-addition follow-up
  - mapping:
    this strengthens the scoped-binder naming note: mixed-kind declarations should not depend on arbitrary standalone-signature placement to decide whether two binder names are aliases or distinct variables

- GHC [#21140](https://gitlab.haskell.org/ghc/ghc/-/issues/21140) `TypeApplications implies PartialTypeSignatures?`
  - classification: spec-addition follow-up
  - mapping:
    this motivates the placeholder-type-in-type-application note: if placeholder types are ever allowed, the language should say whether explicit type applications may contain them and whether doing so implicitly enables a broader partial-signature mode

- GHC [#21106](https://gitlab.haskell.org/ghc/ghc/-/issues/21106) `-Wunused-packages doesn't display for which component the warning is raised`
  - classification: diagnostic-quality/spec-addition follow-up
  - mapping:
    this motivates the whole-build diagnostic-context note: location-free build warnings should still name the owning component or target so users can tell which unit triggered them

- GHC [#21110](https://gitlab.haskell.org/ghc/ghc/-/issues/21110) `Should -Wunused-packages be ignored in --interactive mode?`
  - classification: warning-policy/spec-addition follow-up
  - mapping:
    this strengthens the interactive-vs-batch warning-consistency note: build-time warning families need an explicit REPL-mode policy instead of silently carrying over or silently disappearing

- GHC [#21121](https://gitlab.haskell.org/ghc/ghc/-/issues/21121) `Test failures because instances are loaded in a different order depending on optimisations`
  - classification: tooling/determinism/spec-addition follow-up
  - mapping:
    this motivates the unordered-candidate normalization note: visible enumerations of instances or similar unordered sets should be canonicalized before rendering so optimization-dependent load order does not leak into output

- GHC [#21151](https://gitlab.haskell.org/ghc/ghc/-/issues/21151) `Insoluble constraints arising from an ambiguity check are reported poorly`
  - classification: diagnostic-quality/spec-addition follow-up
  - mapping:
    this strengthens both the hidden-structure and ambiguity-check notes: the reporter should name the insoluble obligation and the ambiguity-check phase directly instead of printing a nonsensical identical-looking expected/actual pair

- GHC [#21158](https://gitlab.haskell.org/ghc/ghc/-/issues/21158) `Record update typing is wrong`
  - classification: spec-clarity/spec-addition follow-up
  - mapping:
    this motivates the record-update equivalence note: in the supported same-type fragment, ordinary record update should agree with explicit destructure-and-rebuild after ordinary normalization of copied fields

- GHC [#21149](https://gitlab.haskell.org/ghc/ghc/-/issues/21149) `servant-0.19 fails to compile on HEAD due to TypeErrors triggering more eagerly`
  - classification: spec-addition follow-up
  - mapping:
    this motivates the rejected/custom-error forcing-timing note: if descriptive rejected constraints participate in reduction, the language should say when they are forced during normalization and instance search

- GHC [#21161](https://gitlab.haskell.org/ghc/ghc/-/issues/21161) `GHC displays Unicode improperly`
  - classification: tooling/spec-addition follow-up
  - mapping:
    this motivates the Unicode-rendering note: diagnostics and REPL displays should either show the actual Unicode scalar or use one canonical escape form consistently

- GHC [#21162](https://gitlab.haskell.org/ghc/ghc/-/issues/21162) `GHC does not support Unicode mathematical functions`
  - classification: ergonomics/spec-addition follow-up
  - mapping:
    this strengthens the future Unicode identifier/operator profile note: if Kappa ever widens its source surface, it should say explicitly whether mathematical relation/logical symbols are admitted as ordinary operators

- GHC [#21167](https://gitlab.haskell.org/ghc/ghc/-/issues/21167) `Proposal: don't print constraints in parens`
  - classification: diagnostic-quality/spec-addition follow-up
  - mapping:
    this motivates the stable diagnostic-delimiter note: rendered code fragments inside human diagnostics should use one consistent quoting/delimiting convention

- GHC [#21172](https://gitlab.haskell.org/ghc/ghc/-/issues/21172) `No reduction of "kind families"`
  - classification: spec-clarity/spec-addition follow-up
  - mapping:
    this motivates the kind-position family-normalization note: transparent families used in kinds should reduce under the same ordinary normalization discipline as elsewhere

- GHC [#21168](https://gitlab.haskell.org/ghc/ghc/-/issues/21168) `Possible work-around for implicit params and simplified subsumption?`
  - classification: ergonomics/spec-addition follow-up
  - mapping:
    this motivates the higher-order subsumption note: eta-equivalent higher-order uses of implicit/constraint-carrying aliases should have one explicit acceptance policy rather than depending on whether users manually eta-expand

- GHC [#21170](https://gitlab.haskell.org/ghc/ghc/-/issues/21170) `Accommodate build systems with their own notion of package names`
  - classification: build/tooling/spec-addition follow-up
  - mapping:
    this motivates the external build-unit identity note: semantic unit identity should be distinguished from human-facing package naming so external build systems are not forced through one ecosystem's name grammar

- GHC [#21208](https://gitlab.haskell.org/ghc/ghc/-/issues/21208) `GHC doesn't notice some overlapping instances (regression)`
  - classification: spec-clarity/spec-addition follow-up
  - mapping:
    this motivates the conservative overlap/coherence note: transparent family applications and local evidence must still participate in ambiguous-overlap rejection before acceptance

- GHC [#21216](https://gitlab.haskell.org/ghc/ghc/-/issues/21216) `OverloadedRecordDotSyntax not working in a QuasiQuote block`
  - classification: quotation/spec-addition follow-up
  - mapping:
    this motivates the quote/quasiquote active-grammar note: embedded expression blocks need a stated relationship to the surrounding feature/profile set instead of silently lagging behind it

- GHC [#21220](https://gitlab.haskell.org/ghc/ghc/-/issues/21220) `Resolve interaction between TH added documentation and user given documentation`
  - classification: documentation/spec-addition follow-up
  - mapping:
    this motivates the generated-versus-user-doc merge-precedence note: once metaprogramming can attach docs, the merge policy must be explicit

- GHC [#21209](https://gitlab.haskell.org/ghc/ghc/-/issues/21209) `Quantified constraint blocks functional dependency`
  - classification: future-constraints/spec-addition follow-up
  - mapping:
    this strengthens the grouped/higher-order constraint note: determinant and ambiguity reasoning should be able to see through immediately available transparent constraint wrappers rather than treating them as opaque blockers

- GHC [#21226](https://gitlab.haskell.org/ghc/ghc/-/issues/21226) `OverloadedRecordDot: "label" can't be used as a field name`
  - classification: projection-syntax/spec-addition follow-up
  - mapping:
    this motivates the projection-syntax field-name compatibility note: if future projection sugar exists, the language should say which post-dot spellings remain legal field names and how collisions are escaped

- GHC [#21228](https://gitlab.haskell.org/ghc/ghc/-/issues/21228) `GHC Lexer reports error in string literal including ZWJ`
  - classification: Unicode/spec-addition follow-up
  - mapping:
    this motivates the Unicode source-acceptance note: direct literal text and escaped text that denote the same scalar sequence should follow one acceptance rule

- GHC [#21237](https://gitlab.haskell.org/ghc/ghc/-/issues/21237) `Add a note about non-deferrable constraints`
  - classification: diagnostics/spec-addition follow-up
  - mapping:
    this motivates the non-deferrable-constraints note: if some diagnostics are recoverable or deferrable, the language/tooling contract should also name the ones that remain immediate hard failures

- GHC [#21252](https://gitlab.haskell.org/ghc/ghc/-/issues/21252) `-Wunused-local-binds doesn't work for expressions in GHCi`
  - classification: warning-policy/spec-addition follow-up
  - mapping:
    this strengthens the interactive-vs-batch warning-consistency note: local-unused warnings also need an explicit REPL policy instead of being silently disabled

- GHC [#21269](https://gitlab.haskell.org/ghc/ghc/-/issues/21269) `Parse Error: Binds.hs with -haddock "-- ^ After the renamer..."`
  - classification: documentation/spec-addition follow-up
  - mapping:
    this strengthens the documentation-attachment/parsing note: documentation-mode parsing should handle modifier-adjacent and trailing doc comments under one deterministic rule instead of reclassifying valid comment text as a parse error

- GHC [#21275](https://gitlab.haskell.org/ghc/ghc/-/issues/21275) `Linear and non-linear data constructor types are printed identically`
  - classification: semantic-query/spec-addition follow-up
  - mapping:
    this strengthens the semantic-query rendering note: printed constructor types must preserve multiplicity/linearity information when that affects how the constructor may be used

- GHC [#21297](https://gitlab.haskell.org/ghc/ghc/-/issues/21297) `TH pretty-printer misprints parentheses in a type`
  - classification: quotation/spec-addition follow-up
  - mapping:
    this strengthens the quoted-syntax round-trip note: quoted/declaration pretty-printers must preserve necessary parentheses in higher-rank or annotated type forms

- GHC [#21299](https://gitlab.haskell.org/ghc/ghc/-/issues/21299) `Inconsistent SrcSpan treatment in TH splices`
  - classification: provenance/spec-addition follow-up
  - mapping:
    this strengthens the source-provenance note: splice-generated syntax should preserve consistent source-span/provenance treatment instead of dropping or relocating origins unpredictably

- GHC [#21315](https://gitlab.haskell.org/ghc/ghc/-/issues/21315) `HEAD loses source location and provenance for -Wredundant-constraints warning`
  - classification: diagnostic-quality/spec-addition follow-up
  - mapping:
    this strengthens the source-provenance and redundancy-warning notes: even when the checker identifies the right redundant premise, the warning still needs the precise source location and declaration provenance of that premise

- GHC [#21306](https://gitlab.haskell.org/ghc/ghc/-/issues/21306) `"No skolem info" panic when matching on too few fields of a GADT constructor`
  - classification: diagnostic-robustness/spec-addition follow-up
  - mapping:
    malformed indexed/GADT constructor patterns with too few fields should be rejected as ordinary user-facing arity errors rather than triggering internal failures

- GHC [#21307](https://gitlab.haskell.org/ghc/ghc/-/issues/21307) `ghc reports package-id in unused imports warning instead the package name+version+id`
  - classification: diagnostic-quality/spec-addition follow-up
  - mapping:
    this strengthens the whole-build warning note: warnings that mention imported packages or units should prefer stable human-facing package identity rather than opaque internal package ids

- GHC [#21309](https://gitlab.haskell.org/ghc/ghc/-/issues/21309) `Order dependence in type checking with the monomorphism restriction`
  - classification: spec-clarity/spec-addition follow-up
  - mapping:
    if Kappa keeps any reduced-generalization mode, the spec should say whether semantically equivalent declaration reordering is permitted to affect acceptance

- GHC [#21324](https://gitlab.haskell.org/ghc/ghc/-/issues/21324) `HasField instances cannot be defined on newtypes that have a field selector even if that isn't exported`
  - classification: projection-protocol/spec-addition follow-up
  - mapping:
    future projection or field-protocol legality should be governed by exported surface fields rather than hidden representation selectors

- GHC [#21360](https://gitlab.haskell.org/ghc/ghc/-/issues/21360) `Gratuitous -Wincomplete-record-updates warning in do-notation`
  - classification: warning-precision/spec-addition follow-up
  - mapping:
    record-update completeness warnings should account for path-local constructor refinement already established by surrounding control flow
## Recommended Pull Order

1. `#1060`
2. `#29` and `#30`
3. `#366`
4. `#1401`
5. `#1`

That order matches Kappa's current needs: first layout, then match/exhaustiveness, then indexed refinement, then constructor-informed projection, and only after that the more indirect implicit-evidence case.
