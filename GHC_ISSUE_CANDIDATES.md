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

## Recommended Pull Order

1. `#1060`
2. `#29` and `#30`
3. `#366`
4. `#1401`
5. `#1`

That order matches Kappa's current needs: first layout, then match/exhaustiveness, then indexed refinement, then constructor-informed projection, and only after that the more indirect implicit-evidence case.
