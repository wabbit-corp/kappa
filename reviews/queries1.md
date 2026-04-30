I reviewed the uploaded compiler sources against the uploaded `Spec.md`, mainly §§2.6.2 and 10.3-10.10.

Static review only: I could not run the test suite because this environment does not have `dotnet` installed. So this is a line-level semantic review, not an execution-verified one. Naturally, the one time a compiler review would benefit from a compiler, the box has chosen monastic abstinence.

## Verdict

The implementation is **not compliant** with the spec yet.

What exists is mostly a **surface-parser plus list-desugaring prototype** with some query-shape validation layered on top. The spec describes a first-class query/comprehension system based on `QueryCore`, `IntoQuery`, `BorrowIntoQuery`, `FromComprehensionRaw`, `FromComprehensionPlan`, orderedness, cardinality, mode tracking, hooks, and normalized comprehension plans. The compiler currently parses many of the keywords, but most of the specified semantics are either missing, approximated, or only checked diagnostically without being implemented in the lowered code.

The most dangerous mismatch is this:

> The type checker can make a comprehension look like a `QueryCore`, but the lowering often emits an ordinary list/set/map expression.

That is not a harmless implementation detail. It means the semantic model promised by the type system is not the runtime model being generated.

---

# What is implemented reasonably

There are a few parts that are broadly in the right direction.

## Basic comprehension detection

The parser uses the first significant token after the opener to decide whether a collection literal is a comprehension:

* `yield`
* `for`
* `for?`

This matches the spec’s detection rule reasonably well.

Relevant code:

* `CoreParsing.fs:2841-2852`

## Basic list-style lowering

For simple list comprehensions of the form:

```kappa
[ for x in xs yield f(x) ]
```

the compiler lowers into recursive list-processing code.

Relevant code:

* `CoreParsing.fs:2389-2441`
* `CoreParsing.fs:2959-2963`

This is usable for a narrow subset: ordered, list-shaped sources, simple `for`, `let`, `if`, and `yield`.

## Some query type metadata exists

`QuerySemantics.fs` has a useful start for parsing/querying mode and cardinality metadata:

* `QueryUseMode`
* `QueryCardinality`
* `QueryModeInfo`
* `QueryTypeInfo`

Relevant code:

* `QuerySemantics.fs:1-20`
* `QuerySemantics.fs:89-155`
* `QuerySemantics.fs:158-208`
* `QuerySemantics.fs:210-277`

This is not enough for conformance, but it is the seed of the right layer.

## Plain `for` rejects obviously refutable patterns

The spec says refutable generator patterns need `for?`. The implementation has a validation diagnostic for plain `for` with refutable patterns.

Relevant code:

* `SurfaceElaboration.fs:11087-11097`

That part is directionally correct.

---

# Major non-compliance findings

## 1. `IntoQuery` and `BorrowIntoQuery` are not implemented

The spec makes source conversion central:

* `for pat in src` should use `IntoQuery`.
* `for & pat in src` should use `BorrowIntoQuery`.
* Query sources should have associated mode, cardinality, item type, and source demand.

But in the prelude, the traits are empty:

```kappa
trait IntoQuery t
trait BorrowIntoQuery t
```

Relevant code:

* `Stdlib/std/prelude.kp:206-209`

There are no associated types, no `toQuery`, no `toBorrowQuery`, and no actual hook implementation found elsewhere.

The parser/lowerer directly pattern-matches the source as a list:

* `CoreParsing.fs:2389-2441`

The source-unwrapping helper only recognizes one syntactic shape for `Set`:

* `CoreParsing.fs:2354-2359`

So although `QuerySemantics.fs` claims metadata support for things like `QueryCore`, `List`, `Array`, `Set`, and `Option`, the lowered runtime behavior does not actually enumerate those sources generically.

### Impact

A typed source may be accepted as query-like by the semantic checker but then lowered as if it were an ordinary list. That breaks the spec’s core abstraction.

Examples likely broken or incorrectly accepted:

```kappa
[ for x in Some 1 yield x ]
[ for x in Array(...) yield x ]
[ for x in QueryCore ... yield x ]
[ for & x in xs yield x ]
```

The compiler has query-shaped metadata, but no real source-conversion semantics. A splendid bureaucracy: all the paperwork, no bridge.

---

## 2. `Query`, `OnceQuery`, and `QueryCore` carriers are type/runtime inconsistent

The spec treats `QueryCore mode itemQuantity item` as a first-class query value. `Query`, `OnceQuery`, etc. are not merely ornamental hats for lists.

The implementation recognizes query carriers here:

* `SurfaceElaboration.fs:4570-4613`

It fabricates/infer query carrier types here:

* `SurfaceElaboration.fs:4769-4808`

It validates mode/cardinality compatibility here:

* `SurfaceElaboration.fs:12849-12893`

But lowering discards the carrier and returns the lowered comprehension body:

* `SurfaceElaboration.fs:19320-19322`
* `SurfaceElaboration.fs:18744-18745`

The comprehension itself lowers to a list/set/map constructor:

* `CoreParsing.fs:2959-2963`

### Impact

Something like:

```kappa
Query [ for x in xs yield x ]
```

may be assigned a query-ish type, but the lowered expression is just the lowered collection expression. That means the compiler is lying to itself, which is usually how compilers become interpretive dance.

This is probably the single biggest blocker.

---

## 3. Custom comprehension sinks are not actually supported

The spec says carrier prefixes should support:

* built-in collection carriers
* `Query` / `OnceQuery` / `QueryCore`
* custom sinks via `FromComprehensionRaw`
* custom sinks via `FromComprehensionPlan`
* raw hook preferred over plan hook

The current implementation only has special recognition for:

* `Query`
* `OnceQuery`
* `QueryCore`

Relevant code:

* `SurfaceElaboration.fs:4570-4613`

There is a diagnostic-ish check for custom sink item type compatibility:

* `SurfaceElaboration.fs:12899-12934`

But the actual hooks are never invoked. The prelude traits are empty, and there is no `fromComprehensionRaw` or `fromComprehensionPlan` machinery.

### Impact

Custom comprehension carriers are essentially not implemented. At best, the compiler may validate some trait-shaped surface, then ignore it and lower as a built-in collection.

Spec requirement: semantic hook.

Implementation: decorative trait check.

---

## 4. `for?` is parsed but not semantically implemented

The parser records whether a generator is refutable:

* `CoreParsing.fs:2122-2144`

But the lowering ignores `isRefutable`:

* `CoreParsing.fs:2389-2441`

The resource checker also ignores the distinction when processing the clause:

* `ResourceChecking.fs:4676-4729`

The spec says:

```kappa
for? pat in src
```

should drop rows where the pattern does not match.

The implementation currently uses `wrapPatternBindings` in a way that does not implement the specified filter semantics for refutable generator patterns.

### Impact

This is wrong:

```kappa
[ for? Some x in xs yield x ]
```

It should keep only matching rows. Current lowering does not faithfully implement that behavior.

---

## 5. `let?` appears unsupported

The spec defines:

```kappa
let? pat = expr
```

as a refutable local-binding filter.

The syntax model contains a refutability flag for `LetClause`, and there is a `Keyword.LetQuestion` in the language, but the parser only handles `let`, not `let?`.

Relevant parser area:

* `CoreParsing.fs:2195-2352`

Lowering ignores the refutability flag anyway:

* `CoreParsing.fs:2486-2520`

Resource checking ignores it too:

* `ResourceChecking.fs:4720-4729`

### Impact

`let?` either fails to parse or behaves incorrectly if somehow constructed. This is a direct spec miss.

---

## 6. Borrowed generators are parsed but not implemented

The parser recognizes borrowed generator syntax:

```kappa
for & pat in src
for? & pat in src
```

Relevant code:

* `CoreParsing.fs:2122-2144`

But lowering ignores `isBorrowed`:

* `CoreParsing.fs:2389-2441`

There is no use of `BorrowIntoQuery`, no borrowed item mode, no reference-region semantics, and no distinction in resource handling.

### Impact

Borrowed comprehensions are currently syntactic theater.

The spec’s borrowed-source semantics are not present.

---

## 7. `group by` is not implemented as grouping

The spec says `group by` partitions rows by key using `Eq`, folds aggregate values with `Monoid`, hides pre-group names, and produces grouped rows.

The implementation does not group. It maps each input row into a record with a key and aggregation values:

* `CoreParsing.fs:2443-2484`

There is no partitioning, no equality-based grouping, no monoidal fold, no duplicate key accumulation, and no real aggregate semantics.

### Impact

This is semantically wrong for any nontrivial group.

For example, this should combine rows with the same key:

```kappa
[ for x in xs
  group by x.category aggregating total = x.amount
  yield total
]
```

The implementation instead emits one row per original row with a record-shaped wrapper. That is not grouping. That is wearing a fake moustache and calling oneself “aggregation.”

---

## 8. `on conflict` is parsed but ignored

The spec defines map conflict policies:

* default `keep last`
* `keep first`
* `keep last`
* `combine`
* possibly explicit resolver forms, depending on the full grammar

The parser handles conflict policy syntax:

* `CoreParsing.fs:2164-2193`

But final collection construction ignores the policy:

* `CoreParsing.fs:2959-2963`

Also, `on conflict` appears not to be restricted to map comprehensions/final map yields.

### Impact

Map conflict behavior is not spec-compliant. Duplicate keys will be handled only by whatever the `Map` constructor does, not by the comprehension semantics.

---

## 9. Map comprehensions accept invalid `yield`

The spec requires map comprehensions to yield key/value pairs:

```kappa
{ for x in xs yield key : value }
```

But `parseYieldExpression` allows a map comprehension without a colon and returns `YieldValue`:

* `CoreParsing.fs:2149-2162`

### Impact

This likely accepts invalid map comprehensions such as:

```kappa
{ for x in xs yield x }
```

That should be rejected for map carriers.

---

## 10. Ordinary map literals appear broken nearby

This is slightly outside query semantics, but it sits directly in the same collection parser.

For non-comprehension collection literals, list and set items are parsed, but map literals are lowered to an empty map:

* `CoreParsing.fs:2854-2865`

So something like:

```kappa
{ "a": 1 }
```

appears to produce an empty map.

That is not a comprehension bug strictly, but it is adjacent enough to matter.

---

## 11. `skip` / `take` orderedness checking is too weak

The spec says `skip` and `take` require an ordered row stream.

The implementation rejects them only when orderedness is explicitly `KnownUnordered`:

* `CoreParsing.fs:2587-2673`

But it allows `UnknownOrderedness`.

The source orderedness inference itself is weak:

* `CoreParsing.fs:2354-2359`

It only identifies a syntactic `Set` shape specially. It does not reliably classify variables, maps, custom sources, query carriers, or trait-derived sources.

### Impact

The compiler may allow:

```kappa
[ for x in mysterySource
  skip 10
  yield x
]
```

even when it cannot prove `mysterySource` is ordered.

The spec requires proof of orderedness, not absence of proof of unorderedness. The current behavior is optimistic, which is adorable in a type checker and usually catastrophic in a bank vault.

---

## 12. `order by` is incomplete

The spec supports:

```kappa
order by expr
order by asc expr
order by desc expr
order by (asc a, desc b, c)
```

The parser currently supports only a single optional direction and a single expression:

* `CoreParsing.fs:2288-2298`

Lowering performs an insertion sort:

* `CoreParsing.fs:2744-2840`

It does precompute one key per row, which is good. The sort also appears intended to be stable. But it does not implement tuple/multi-key ordering with per-key direction.

### Impact

Single-key `order by` may work.

Multi-key ordered sorts are missing.

---

## 13. `distinct` is only partially implemented

The implementation lowers `distinct` / `distinct by` by keeping a list of seen keys and comparing with `==`:

* `CoreParsing.fs:2552-2585`

This roughly matches equality-based distinct behavior for simple cases.

But there are issues:

* no explicit `Eq` evidence or plan-level requirement
* `distinct` without `by` uses `makeRowExpression currentNames`, which creates a nested `Res`-style row, not necessarily the canonical quantitative row identity the spec describes
* resource/droppability checks are coarse and partly disconnected from actual row semantics

Relevant checker behavior:

* `ResourceChecking.fs:4864-4879`

### Impact

Basic `distinct by x` may work for simple rows. Spec-level distinct semantics are incomplete.

---

## 14. Inner `join` is only approximately lowered

The spec says inner join lowers like:

```kappa
for tmp in source
let? pat = tmp
if condition
```

The implementation parses `join` and lowers it roughly through generator + condition machinery:

* `CoreParsing.fs:2907-2921`

But since `for?`/`let?` semantics are not correctly implemented, refutable join patterns are suspect.

### Impact

Joins with irrefutable patterns and simple conditions may work. Joins relying on proper refutable pattern filtering are not trustworthy.

---

## 15. `left join` does not produce the specified first-class delayed query

The spec says `left join ... into name` binds `name` as a first-class query:

```kappa
QueryCore innerMode innerItemQuantity t
```

with restrictions around delayed capture of linear/one-shot resources.

The type inference/checking side fakes a `QueryCore` type for the `into` name:

* `SurfaceElaboration.fs:4718-4742`
* `ResourceChecking.fs:4554-4566`

But lowering materializes an eager list of matching items:

* `CoreParsing.fs:2675-2742`

The capture diagnostic is coarse and based on simple name reference analysis:

* `ResourceChecking.fs:4639-4658`

### Impact

The type says “query,” the runtime value is a list.

This breaks delayed/lazy query semantics, capture rules, and one-shot behavior. It may be acceptable as a temporary implementation strategy only if the spec explicitly allowed eager materialization there. It does not.

---

## 16. Clause ordering and duplicate final clauses are under-validated

The spec has strict rules around final `yield`, map conflict clauses, and clause forms.

The parser separates final yield/conflict from earlier clauses like this:

* `CoreParsing.fs:2867-2886`

But earlier `yield` and `on conflict` clauses can be silently discarded because only normal clauses are kept through `List.choose`.

That means something like:

```kappa
[ yield 1, yield 2 ]
```

may be accepted as just `yield 2`, instead of being rejected.

### Impact

Invalid comprehensions can be accepted silently. Silent acceptance is worse than rejection because it lets bugs rent a small apartment in the user’s program.

---

## 17. Comprehension plan/raw reflection is missing

The spec requires:

* `RawComprehension`
* `ComprehensionPlan`
* `lowerComprehension`
* `FromComprehensionRaw`
* `FromComprehensionPlan`
* stable inspection API for macro-like/custom carrier behavior

The current AST has a `SurfaceComprehension`, but there is no real normalized plan object matching the spec contract.

Relevant syntax:

* `Syntax.fs:388-405`

The compiler does some local shape inference:

* `SurfaceElaboration.fs:4615-4768`

But this is not a real plan representation exposed to hooks.

### Impact

Spec §10.9 and §10.10 custom carrier behavior cannot work.

---

# Resource and linearity semantics

The implementation has resource checking for comprehensions:

* `ResourceChecking.fs:4660-4924`

This includes some useful pieces:

* cardinality diagnostics for exact-one row bindings
* non-consuming checks for group/order/distinct-ish expressions
* capture checks for left joins
* basic binding of row names through clauses

But it remains incomplete relative to the spec because the underlying semantic model is not implemented.

Specific issues:

## Cardinality checks are partial

The checker has machinery for cardinality diagnostics:

* `ResourceChecking.fs:4606-4631`

But because query source conversion and item quantity are incomplete, the checker cannot reliably know row multiplicity across arbitrary `IntoQuery` sources.

## Refutable clauses are ignored

`for?` and `let?` should affect row survival. The resource checker mostly treats them like ordinary bindings.

Relevant code:

* `ResourceChecking.fs:4676-4729`

## Left-join capture rules are too shallow

The spec forbids delayed capture of linear/one-shot entries unless explicitly materialized. The checker has a name-reference diagnostic:

* `ResourceChecking.fs:4639-4658`

That is better than nothing, but it is not the full delayed-query capture model.

---

# Carrier behavior summary

| Carrier / source feature | Spec expectation                               | Current implementation                 |
| ------------------------ | ---------------------------------------------- | -------------------------------------- |
| List comprehension       | Ordered row stream, list result                | Partially implemented                  |
| Set comprehension        | Unordered result                               | Partially implemented as `Set(list)`   |
| Map comprehension        | key/value yield plus conflict policy           | Partially parsed, conflict ignored     |
| `Query` carrier          | First-class `QueryCore`                        | Type-shaped only, lowers to collection |
| `OnceQuery` carrier      | One-shot query with cardinality checks         | Type-shaped only, runtime missing      |
| `QueryCore` carrier      | First-class query with explicit mode/card/item | Type-shaped only, runtime missing      |
| `IntoQuery`              | Required source conversion trait               | Not implemented                        |
| `BorrowIntoQuery`        | Required borrowed source conversion            | Not implemented                        |
| `FromComprehensionRaw`   | Custom raw hook                                | Not implemented                        |
| `FromComprehensionPlan`  | Custom plan hook                               | Not implemented                        |
| `for?`                   | Refutable filter                               | Parsed, not implemented                |
| `let?`                   | Refutable filter                               | Missing                                |
| `group by`               | Real grouping/folding                          | Not implemented                        |
| `left join`              | Delayed query binding                          | Eager list materialization             |
| `order by`               | Stable, multi-key directions                   | Single-key only                        |
| `distinct`               | Equality-based dedup over row/key              | Partial                                |
| `skip` / `take`          | Ordered streams only                           | Allows unknown orderedness             |

---

# Recommended fix strategy

Do not try to patch this by adding more checks around the current parse-time lowering. That path leads to a compiler made of exception cases and regret.

The better route is to split the implementation into proper phases.

## 1. Stop lowering comprehensions in the parser

Right now `SurfaceComprehension` carries a `Lowered` expression:

* `Syntax.fs:388-405`

That is the root design problem. The parser should produce raw syntax only.

A comprehension should not be lowered until after:

* carrier is known
* source query conversions are resolved
* item type is known
* mode/cardinality are known
* orderedness is known
* resource effects are known
* custom hooks have had their chance to run

## 2. Implement real prelude traits

The prelude needs the actual associated members from the spec:

```kappa
trait IntoQuery s where
  type Mode
  type ItemQuantity
  type Item
  type SourceDemand
  fn toQuery(...)
```

Likewise for:

* `BorrowIntoQuery`
* `FromComprehensionRaw`
* `FromComprehensionPlan`

The empty traits in `prelude.kp` are not enough.

## 3. Build a normalized `ComprehensionPlan`

The compiler needs a real intermediate representation for clauses:

* source generators
* refutable generators
* borrowed generators
* let/let?
* filters
* joins
* left joins
* grouping
* ordering
* distinct
* skip/take
* yield
* map conflict policy

It should carry:

* item type
* item quantity
* mode
* orderedness
* source demand
* captures
* bound names and their quantities
* terminal carrier kind

That is what custom sinks and diagnostics should consume.

## 4. Implement source conversion before lowering

Every `for` source should elaborate through `IntoQuery`.

Every `for &` source should elaborate through `BorrowIntoQuery`.

Built-ins like list, array, set, option, map, query, and once-query should be ordinary instances or compiler-known equivalents with the same semantics.

## 5. Implement carrier lowering as a separate collector step

After the plan is built, lower into one of:

* list collector
* set collector
* map collector with conflict policy
* query collector / delayed `QueryCore`
* once-query collector
* custom raw hook
* custom plan hook

Raw hook should be preferred over plan hook, per spec.

## 6. Fix refutable clauses

Implement:

```kappa
for? pat in src
let? pat = expr
join pat in src on cond
```

as row filters, not ordinary pattern binds.

Plain `for`/`let` with refutable patterns should remain diagnostics.

## 7. Implement real grouping

`group by` needs actual partitioning by key and aggregation folding.

That means:

* require/evidence `Eq key`
* require/evidence `Monoid agg`
* fold aggregation fields per group
* hide pre-group names afterward
* expose only grouped names
* mark output unordered

The current row-mapping implementation should be replaced.

## 8. Fix map semantics

Map comprehensions need:

* required `yield key : value`
* map-only `on conflict`
* correct default `keep last`
* `keep first`
* `keep last`
* `combine`
* resolver semantics if the spec includes custom conflict functions

Also fix ordinary map literals in the same area.

## 9. Make orderedness conservative

`skip` and `take` should require `KnownOrdered`.

`UnknownOrderedness` should be rejected unless the carrier/source provides orderedness evidence.

## 10. Add conformance tests

The current source tree did not show substantive query/comprehension tests. Add tests before this feature hardens into archaeology.

Good minimum tests:

```kappa
[ for x in [1, 2, 3] yield x + 1 ]
```

```kappa
[ for? Some x in [None, Some 1, Some 2] yield x ]
```

```kappa
[ let? Some x = maybeValue yield x ]
```

```kappa
[ for x in Some 1 yield x ]
```

```kappa
Query [ for x in xs yield x ]
```

```kappa
OnceQuery [ for x in xs yield x ]
```

```kappa
[ for x in {| 1, 2, 3 |} skip 1 yield x ]
// should reject: unordered
```

```kappa
[ for x in xs order by (asc x.a, desc x.b) yield x ]
```

```kappa
{ for x in xs yield x.key : x.value on conflict keep first }
```

```kappa
[ for x in xs group by x.k aggregating total = x.v yield total ]
```

```kappa
[ for x in xs
  left join y in ys on x.k == y.k into matches
  yield matches
]
```

Also add negative tests:

```kappa
[ yield 1, yield 2 ]
```

```kappa
{ for x in xs yield x }
```

```kappa
[ for Some x in xs yield x ]
// should reject unless using for?
```

```kappa
[ for & x in xs yield x ]
// should require BorrowIntoQuery semantics
```

---

# Bottom line

The compiler currently supports a **small useful subset** of comprehension syntax, mostly list-like comprehensions with simple filtering and yielding.

It does **not** yet implement the spec’s query/comprehension semantics as designed.

The key blockers are:

1. no real `IntoQuery` / `BorrowIntoQuery`
2. no real `QueryCore` runtime lowering
3. no custom raw/plan hooks
4. no proper refutable `for?` / `let?`
5. no real `group by`
6. ignored map conflict policy
7. weak orderedness checking
8. type/runtime mismatch for query carriers

The current implementation is a plausible prototype, but not a conforming implementation. It should be treated as an early lowering experiment, not as the semantic foundation.
