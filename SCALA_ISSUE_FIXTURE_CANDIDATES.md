# Scala Issue Fixture Candidates

Mined from `../datatron/data-scala-issues/` on 2026-04-23.

Purpose: identify Scala issue repros that map cleanly onto Kappa fixtures, with emphasis on Kappa's current or spec-defined surface rather than Scala-specific features.

## Immediate Candidates

These are the main Wave 1 direct-fit Scala candidates. The status line under each item records whether it is already
imported in the real suite, staged in `new-tests/`, or still pending.

### 1. Symbolic term imports

- Source issue: Scala 3 [#530](https://github.com/scala/scala3/issues/530) `Import of symbolic identifiers does not work`
- Kappa spec tie-in: `Spec.md` §2.3 allows `nameRef ::= ... '(' operator_token ')'` and explicitly shows `import std.prelude.((>>=))`.
- Why it matters for Kappa:
  The current fixture set covers constructor/type/trait selectors, but not an import item whose name is an operator token.
- Current status:
  - imported as `tests/Kappa.Compiler.Tests/Fixtures/modules.imports.positive_symbolic_term`
- Proposed Kappa fixture:
  - `spec_2_3_imports_positive_symbolic_term`
  - or `spec_2_3_1_import_item_kind_selectors_positive_symbolic_term`
- Suggested shape:
  - define an operator in one module
  - import it explicitly with extra parentheses
  - use it unqualified from another module

### 2. Missing imported item from an existing module

- Source issue: Scala 3 [#1286](https://github.com/scala/scala3/issues/1286) `Trying to import a class that doesn't exist from a package that exists does not produce an error`
- Kappa spec tie-in: `Spec.md` §2.3 import resolution.
- Why it matters for Kappa:
  Negative import coverage exists for cycles, visibility, and wildcard constructors, but not for "module exists, imported item does not".
- Current status:
  - imported as `tests/Kappa.Compiler.Tests/Fixtures/modules.imports.negative_missing_item`
- Proposed Kappa fixture:
  - `spec_2_3_imports_negative_missing_item`
- Suggested shape:
  - module `foo` exports some names
  - importer uses `import foo.(Missing)`
  - expect a stable unresolved-import diagnostic

### 3. Misindented top-level import

- Source issue: Scala 3 [#9992](https://github.com/scala/scala3/issues/9992) `Misindent imports produces a confusing error`
- Kappa spec tie-in: `Spec.md` §3.4 whitespace/indentation.
- Why it matters for Kappa:
  Current Kappa fixtures cover positive indentation and continuation, but not a negative case where a top-level import is misindented and should be rejected directly.
- Current status:
  - imported as `tests/Kappa.Compiler.Tests/Fixtures/lexical.whitespace_indentation_continuation.negative_misindented_import`
- Proposed Kappa fixture:
  - `spec_3_4_whitespace_and_continuation_negative_misindented_import`
- Suggested shape:
  - indent a top-level `import` under no enclosing block
  - expect an indentation/layout diagnostic, not a cascade of downstream name errors

### 4. Misindented `match` case body

- Source issue: Scala 3 [#9822](https://github.com/scala/scala3/issues/9822) `Confusing error message value not found when wrong indentation is used within a case definition`
- Kappa spec tie-in: `Spec.md` §3.4 and §7.5.
- Why it matters for Kappa:
  Kappa has `match` fixtures, but not a negative indentation case where a case-body line is dedented and the compiler should blame layout instead of producing a misleading unresolved-name error.
- Current status:
  - staged as `new-tests/expressions.match.negative_misindented_case_body`
- Proposed Kappa fixture:
  - `spec_7_5_match_negative_misindented_case_body`
- Suggested shape:
  - put one line of a `case` body at the wrong indentation
  - expect an indentation/layout diagnostic

### 5. Continuation after an infix operator

- Source issue: Scala 3 [#11632](https://github.com/scala/scala3/issues/11632) `Leading Infix Operator Not Working With Colon ':' Indentation`
- Kappa spec tie-in: `Spec.md` §3.4 says a newline after an infix operator token is a continuation context.
- Why it matters for Kappa:
  Kappa has a continuation fixture for function application, but not for line breaks after infix operators.
- Current status:
  - imported as `tests/Kappa.Compiler.Tests/Fixtures/lexical.whitespace_indentation_continuation.positive_infix_operator`
- Proposed Kappa fixture:
  - `spec_3_4_whitespace_and_continuation_positive_infix_operator`
- Suggested shape:
  - split `left || right` or `left + right` across lines after the operator
  - verify parse and runtime behavior

### 6. Guarded case must fall through to later cases

- Source issue: Scala 2 [#211](https://github.com/scala/bug/issues/211) `match case is skipped when identical patterns and a guard are used`
- Kappa spec tie-in: `Spec.md` §7.5.6 says guard failure does not exclude the pattern from subsequent cases.
- Why it matters for Kappa:
  I did not find a current fixture that exercises two identical patterns where the first has a failing guard and the second must still match.
- Current status:
  - imported as `tests/Kappa.Compiler.Tests/Fixtures/expressions.match.runtime_positive_guard_fallthrough`
- Proposed Kappa fixture:
  - `spec_7_5_match_runtime_positive_guard_fallthrough`
- Suggested shape:
  - same pattern twice
  - first case `if False`
  - second case succeeds
  - verify runtime result

### 7. Multi-parameter typed lambdas

- Source issue: Scala 2 [#1564](https://github.com/scala/bug/issues/1564) `Problem parsing anonymous functions with 2+ params`
- Kappa spec tie-in: `Spec.md` §7.2 lambdas.
- Why it matters for Kappa:
  Kappa already tests a single typed binder lambda, but I did not find a fixture for multiple typed parameters in one lambda.
- Current status:
  - imported as `tests/Kappa.Compiler.Tests/Fixtures/expressions.lambdas.runtime_positive_multiple_typed_binders`
- Proposed Kappa fixture:
  - `spec_7_2_lambdas_runtime_positive_multiple_typed_binders`
- Suggested shape:
  - `let add = \(x : Int) (y : Int) -> x + y`
  - evaluate `add 20 22`

## Future Or Spec-Gap Candidates

These are relevant, but they depend on missing implementation work or on areas where Kappa's current behavior still trails the spec.

### 8. Guarded matches must still be checked for exhaustiveness

- Source issue: Scala 3 [#8708](https://github.com/scala/scala3/issues/8708) `Missing exhaustivity warning when pattern has a guard`
- Kappa spec tie-in: `Spec.md` §7.5 and §7.5.6.
- Why it matters for Kappa:
  The spec requires compile-time exhaustiveness for closed types, and guards do not make a case exhaustive. Current Kappa behavior still allows runtime non-exhaustive matches in fixtures.
- Current status:
  - staged as `new-tests/expressions.match.negative_guard_does_not_make_exhaustive`
- Proposed Kappa fixture:
  - `spec_7_5_match_negative_guard_does_not_make_exhaustive`

### 9. Or-patterns with shared binders

- Source issue: Scala 2 [#182](https://github.com/scala/bug/issues/182) `allow variable binding in alternative pattern`
- Kappa spec tie-in: `Spec.md` §7.6.3 explicitly allows or-patterns when all alternatives bind the same names with the same types and quantities.
- Why it matters for Kappa:
  This is a direct spec feature and a good future fixture pair once or-pattern support is implemented.
- Current status:
  - staged as `new-tests/expressions.patterns.positive_or_pattern_shared_binders`
  - staged as `new-tests/expressions.patterns.negative_or_pattern_mismatched_binders`
- Proposed Kappa fixtures:
  - positive: `spec_7_6_patterns_positive_or_pattern_shared_binders`
  - negative: `spec_7_6_patterns_negative_or_pattern_mismatched_binders`

### 10. Hidden names from wildcard imports must not affect lookup

- Source issue: Scala 2 [#12736](https://github.com/scala/bug/issues/12736) `Non-visible symbol in wildcard import taking precedence over visible symbol in current package`
- Kappa spec tie-in: `Spec.md` §2.3.1 wildcard imports and §2.5 visibility.
- Why it matters for Kappa:
  Kappa wildcard imports are supposed to bring in public names only. A regression test here would be useful once import resolution is stressed more heavily.
- Current status:
  - staged as `new-tests/modules.imports.positive_hidden_wildcard_does_not_shadow_visible`
- Proposed Kappa fixture:
  - `spec_2_5_visibility_and_imports_negative_hidden_wildcard_precedence`

## Hold For Spec Clarification

These surfaced in the mining pass, but I would not import them as fixtures until Kappa's intended semantics are pinned down more explicitly.

### 11. Float literal tokenization around dot lookup

- Source issue: Scala 2 [#265](https://github.com/scala/bug/issues/265) `Unexpected tokenisation of float literal`
- Why on hold:
  Kappa uses `.` for dotted lookup, and `Spec.md` §4.1.2 currently leaves room for interpretation around literals like `0.` versus `0.x`.
- Potential future fixture shapes:
  - `spec_4_1_numeric_literals_negative_float_tokenization`
  - `spec_2_8_3_dotted_name_resolution_negative_numeric_dot_lookup`

### 12. Whether trailing-dot floats are legal

- Source issue: Scala 3 [#2334](https://github.com/scala/scala3/issues/2334) `Disallow floating point literals without a digit after "."`
- Why on hold:
  Kappa's spec currently describes floating syntax broadly enough that `2.` is not obviously forbidden.

## Notes

- I intentionally did not keep Scala issues that depend on local imports, givens, inline, metaprogramming, enums, or JVM/backend-specific behavior unless they exposed a parser or name-resolution pattern that maps cleanly onto Kappa.
- Existing Kappa fixture coverage already handles:
  - constructor import selectors
  - wildcard imports and `except`
  - ambiguous imported names at runtime
  - operator declarations and sections
  - interpolation basics
  - single-parameter typed lambdas
  - positive whitespace/continuation cases
