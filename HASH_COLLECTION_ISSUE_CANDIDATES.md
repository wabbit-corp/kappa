# Hash / Collection Acceleration Issue Candidates

Purpose: track issues relevant to Kappa's explicit runtime hashing surface, `Hashable` evidence, hash-accelerated
collection/query implementations, and the boundary between semantic equality and performance-only hashing.

Primary spec areas:

- §2.7E Standard runtime hashing support module `std.hash`
- §10.6.3 Query duplicate elimination and `distinct`
- §10.8 Built-in collectors and hash-accelerated maps/sets
- §12.1 Equality soundness
- §15 Semantic hashes, which are intentionally separate from runtime `std.hash`

This note is intentionally about runtime collection acceleration. It does not treat hashing as observable source
semantics, and it does not import source-language syntax for user-defined hash methods.

## Search Coverage Ledger

| Repo | Manifest / artifact paths | Search coverage | Direct reads | PR / test coverage | Next pass |
| --- | --- | --- | --- | --- | --- |
| `inko` | `repos/github-inko-lang-inko/manifest.json`, `issues.json`, `pulls.json`, `docs/manifest.json`, `tests/manifest.json` | issue-title/body search: `hash`, `hashable`, `hash code`, `hashmap`, `hash map`, `map performance`, `collision`, `random`, `seed`, `distinct`, `group by`; manifest counts: 884 issues, 73 PRs, 112 docs, 293 tests | `#424`, `#459`, `#828`, `#923` | no PR/test reads yet | read `std.map` / hash tests and linked PRs for `#424`, `#459`, and `#923` if present |
| `chapel` | `repos/github-chapel-lang-chapel/manifest.json`, `issues.json`, `pulls.json`, `tests/manifest.json` | issue-title/body search: `hash`, `hashing`, `defaultHash`, `hashCombine`, `Hashable`, `user-defined hash`, `Map`, `Associative`, `collision`, `seed`; manifest counts: 7,432 issues, 21,212 PRs, 324 docs, 48,656 tests | `#9182`, `#11341`, `#14039`, `#18234`, `#18291`, `#18340`, `#19285` | no PR/test reads yet | read Chapel associative-domain / map hashing tests and the PRs attached to `#18291` / `#19285` |

## Staged Tests Added

| Fixture root | Source evidence | Spec refs | Purpose |
| --- | --- | --- | --- |
| `new-tests/runtime.hash.behavior.positive_hashable_refines_eq_and_borrows` | `inko#424`, `chapel#9182`, `chapel#19285` | §2.7E, §12.1 | Positive check that `Hashable` provides usable `Eq` evidence and that hashing borrows rather than consumes a value. |
| `new-tests/runtime.hash.behavior.negative_hash_state_linear_reuse` | `inko#424`, `chapel#19285` | §2.7E | Negative check that `HashState` is a linear accumulator and cannot be reused after `finishHashState`. |
| `new-tests/runtime.hash.behavior.positive_set_map_wellformed_without_hashable` | `chapel#9182`, `chapel#18234`, `inko#424` | §2.7E, §10.8 | Positive check that `Set a` and `Map k v` are well-formed without `Hashable` evidence. |

## Candidate Records

### `inko#424`: `HashMap` internals and RNG support

- URL: https://github.com/inko-lang/inko/issues/424
- Artifact path: `repos/github-inko-lang-inko/issues.json`
- Artifact state: `issue-only`
- Bucket: `runtime_hash_collections`
- Spec refs: §2.7E, §10.6.3, §10.8, §12.1
- Fit: `accepted-now`
- Current coverage: `partial`
- Proposed fixture roots: `runtime.hash.behavior.positive_hashable_refines_eq_and_borrows`,
  `runtime.hash.behavior.negative_hash_state_linear_reuse`,
  `runtime.hash.behavior.positive_set_map_wellformed_without_hashable`
- Expected outcome: `positive-check`, `negative-diagnostic`
- Notes: The Kappa analogue is explicit seeding/state and non-semantic hashing. The staged fixtures now pin the real
  obligations: `HashState` is linear, `hashWith` borrows its argument, and collections remain well-formed without
  `Hashable` evidence.

### `inko#459` / `inko#828` / `inko#923`: hash algorithm choice and map performance

- URLs: https://github.com/inko-lang/inko/issues/459, https://github.com/inko-lang/inko/issues/828,
  https://github.com/inko-lang/inko/issues/923
- Artifact path: `repos/github-inko-lang-inko/issues.json`
- Artifact state: `issue-only`
- Bucket: `runtime_hash_collections`
- Spec refs: §2.7E, §10.6.3, §10.8
- Fit: `accepted-later`
- Current coverage: `partial`
- Proposed fixture root: `runtime.hash.runtime_positive_hash_acceleration_preserves_map_denotation`
- Expected outcome: `runtime-positive` or backend/library conformance.
- Notes: These are primarily quality/performance issues. Kappa should test that changing hash implementations or seeds
  does not change `Map`, `Set`, `distinct`, or `group by` denotation. It should not standardize a particular portable
  hash algorithm for runtime collections.

### `chapel#9182` / `chapel#18234` / `chapel#18291`: user-defined hashing for associative collections

- URLs: https://github.com/chapel-lang/chapel/issues/9182, https://github.com/chapel-lang/chapel/issues/18234,
  https://github.com/chapel-lang/chapel/issues/18291
- Artifact path: `repos/github-chapel-lang-chapel/issues.json`
- Artifact state: `issue-only`
- Bucket: `runtime_hash_collections`
- Spec refs: §2.7E, §10.6.3, §10.8, §12.1
- Fit: `accepted-now`
- Current coverage: `partial`
- Proposed fixture roots: `runtime.hash.behavior.positive_hashable_refines_eq_and_borrows`,
  `runtime.hash.behavior.positive_set_map_wellformed_without_hashable`, later
  `runtime.hash.positive_hashable_refines_eq_for_collection_acceleration`
- Expected outcome: `positive-check`, later `positive-run` or `negative-diagnostic`.
- Notes: The Kappa analogue is not a magic method name. It is explicit `std.hash.Hashable` evidence, with `Hashable`
  refining `Eq` and with hashing allowed only as an as-if-preserving acceleration for collections and queries.

### `chapel#18340` / `chapel#19285`: standard hash method naming and return requirements

- URLs: https://github.com/chapel-lang/chapel/issues/18340, https://github.com/chapel-lang/chapel/issues/19285
- Artifact path: `repos/github-chapel-lang-chapel/issues.json`
- Artifact state: `issue-only`
- Bucket: `runtime_hash_collections`
- Spec refs: §2.7E, §12.1, §15
- Fit: `hold-spec`
- Current coverage: `none`
- Proposed fixture root: pending the `HashCode` operation decision in `SPEC_ADDITIONS.md`
- Expected outcome: `positive-check` or `negative-diagnostic`
- Notes: Kappa already avoids a method-name bikeshed by putting the operation in `std.hash.Hashable`. A future fixture
  should pin down `HashCode` opacity after the spec decides whether same-execution `Eq HashCode` and `Ord HashCode` are
  portable operations. Numeric conversion, serialization, printed form, and stable cross-run identity remain separate
  representation-exposure concerns.

### `chapel#14039`: hash-combine collisions with RNG seeds

- URL: https://github.com/chapel-lang/chapel/issues/14039
- Artifact path: `repos/github-chapel-lang-chapel/issues.json`
- Artifact state: `issue-only`
- Bucket: `runtime_hash_collections`
- Spec refs: §2.7E, §10.6.3, §10.8
- Fit: `accepted-later`
- Current coverage: `none`
- Proposed fixture root: `runtime.hash.runtime_positive_distinct_denotation_independent_of_hash_seed`
- Expected outcome: `runtime-positive`
- Notes: This is quality/performance evidence rather than source semantics. The importable Kappa law is that collisions
  may affect performance but must not affect `distinct`, grouping, map lookup, or set membership denotation.

## Spec Additions To Consider

The staged fixtures cover the unambiguous direct issues: `Hashable` as an acceleration trait refining `Eq`, linear
`HashState`, explicit seeds, and collection well-formedness without `Hashable`. `SPEC_ADDITIONS.md` tracks the remaining
open question: whether `HashCode` should expose portable `Eq` and `Ord` as opaque same-execution operations.
