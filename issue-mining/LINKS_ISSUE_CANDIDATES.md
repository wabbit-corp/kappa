# Links Issue Candidates For Kappa

Mined from `../datatron/data-scala-issues/links-issues.json` on 2026-04-23.

Search coverage for this note:

- keyword scan across the full corpus for `distinct`, `order by`, `skip`, `take`, `group by`, `for clause`, `where clause`, `query`, `comprehension`
- direct body reads so far: `#11`, `#13`, `#25`, `#26`, `#340`, `#834`

Purpose: identify Links issues that map cleanly onto Kappa's existing Chapter 10 query semantics, while separating true
query-semantics imports from database-backend or source-language-specific behavior.

## Accepted Now

| source | bucket | spec_refs | fit | current_coverage | fixture_kind | expected_outcome | proposed_fixture_root | status | notes |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| `links#11` | `queries` | `§10.4`, `§10.10.2`, `§10.10.3` | `direct` | `partial` | `single-file` | `positive-run` | `queries.for_clauses.runtime_positive_nested_query_captures_outer_row` | `accepted-now` | Staged in `new-tests/`; nested query clauses should resolve outer-row names lexically and consistently. |
| `links#13` | `queries` | `§10.3.2`, `§10.6.1`, `§10.10.2`, `§10.10.3` | `direct` | `partial` | `single-file` | `positive-run` | `queries.ordering.runtime_positive_order_by_let_bound_current_row` | `accepted-now` | Staged in `new-tests/`; `order by` should read the current row environment, including names introduced by earlier clauses. |
| `links#26` | `queries` | `§10.4`, `§10.10.3` | `direct` | `partial` | `single-file` | `positive-run` | `queries.for_clauses.runtime_positive_nested_and_multiple_generator_equivalent` | `accepted-now` | Staged in `new-tests/`; we already had dependent generators, and this adds the explicit nested-vs-multiple-generator equivalence angle. |
| `links#834` | `queries` | `§10.4`, `§10.10.3` | `direct` | `partial` | `single-file` | `positive-run` | `queries.for_clauses.runtime_positive_nested_and_multiple_generator_equivalent` | `accepted-now` | Same underlying obligation as `#26`; merged into one staged Kappa fixture. |
| `links#25` | `queries` | `§10.10.2`, `§10.10.3` | `indirect` | `partial` | `single-file` | `positive-run` | `queries.nested.runtime_positive_projection_stability` | `accepted-now` | Staged in `new-tests/`; the Kappa analogue keeps the row-environment / projection-stability core while dropping the DB-backend-specific surface. |

## Accepted Later / Hold

| source | bucket | spec_refs | fit | current_coverage | fixture_kind | expected_outcome | proposed_fixture_root | status | notes |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| `links#340` | `implicit_parameters_traits` | `§6.3`, `§7.3.3`, `§12` | `indirect` | `none` | `directory-suite` | `negative-diagnostic` | `traits.instances.negative_effectful_generalization_escape` | `accepted-later` | Valuable generalization / purity warning, but it is not primarily a query issue and should move in a later solver/effects wave. |

## Rejected / Poor Fits For Now

- None in the current direct-read set. The main distinction so far is between direct query-semantics fits and DB-backend-specific repros that should wait.

## Recommended Pull Order

1. `#13`
2. `#11`
3. merged `#26` + `#834`
4. `#25`
5. `#340`

That order matches the current Kappa test gap: row-environment lookup and ordering first, nested lexical capture second, then generator-equivalence coverage, and only after that the more implementation-heavy or non-query-adjacent cases.
