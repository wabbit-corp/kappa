# Spec-Relevant Issue Candidates

Mined from `../datatron/data-scala-issues/` on 2026-04-23.

Scope: a curated cross-language shortlist of issues that are relevant to Kappa's stated semantics in `Spec.md`. This is not an exhaustive scrape. I kept issues where the failure mode maps cleanly onto a Kappa spec rule, not just where the surface syntax happens to look similar.

## Existing Scala-Focused Notes

These two notes already cover the most direct near-term fixture imports from the corpus:

- `SCALA_ISSUE_FIXTURE_CANDIDATES.md`
  - imports, indentation/layout, symbolic operators, pattern/guard behavior, and a few parser-facing fixtures.
- `SCALA_IMPLICIT_ISSUE_CANDIDATES.md`
  - local implicit search, trait premises, alias-aware search, and related instance-resolution behavior.

The rest of this note adds broader non-Scala issues that still matter for Kappa's spec.

## Modules, Imports, And Opacity

- Unison [#516](https://github.com/unisonweb/unison/issues/516) `use A.x has no effect but silently succeeds`
  - Spec tie-in: `Spec.md` sections 2.3, 2.8.
  - Why it matters: Kappa should reject syntactically plausible but semantically empty import/use forms early, rather than letting them fail later as missing-name errors.

- Links [#606](https://github.com/links-lang/links/issues/606) `Names shadowed by transitive dependencies`
  - Spec tie-in: `Spec.md` sections 2.3, 2.8.
  - Why it matters: Kappa's module/import rules should not let one module's opens/imports perturb lookup in another module through transitive leakage.

- Unison [#494](https://github.com/unisonweb/unison/issues/494) `Design and implement opaque types`
  - Spec tie-in: `Spec.md` sections 2.5.2, 2.5.3, 5.5.10, 17.1.9.1.
  - Why it matters: Kappa's opacity rules reach well beyond visibility. They interact with interface canonicalization, hashing, separate compilation, and representation hiding.

## Ownership, Quantities, And Borrowing

- Rust [#1399](https://github.com/rust-lang/rust/issues/1399) `Last use doesn't consider closure bodies properly`
  - Spec tie-in: `Spec.md` sections 5.1.5, 5.1.6, 7.2.1.
  - Why it matters: Kappa's quantity and capture rules need closure-aware use analysis. A value that looks dead outside the lambda may still be live through a captured path.

- Rust [#8636](https://github.com/rust-lang/rust/issues/8636) `Matches on &mut[] move the .. match & don't consider disjointness`
  - Spec tie-in: `Spec.md` sections 5.1.7, 7.5, 7.6.
  - Why it matters: this is very close to Kappa's disjoint-path borrowing story. Pattern decomposition must preserve disjointness facts instead of collapsing everything into one aliasing region.

- Hylo [#1807](https://github.com/hylo-lang/hylo/issues/1807) `Use after lifetime UB in lambda let environment captures`
  - Spec tie-in: `Spec.md` sections 5.1.6, 7.2.1.
  - Why it matters: Kappa explicitly forbids borrow escape. Lambda environment capture is one of the easiest places to accidentally extend a temporary beyond its lifetime.

- Hylo [#1599](https://github.com/hylo-lang/hylo/issues/1599) `Soundness bug in inout accessor`
  - Spec tie-in: `Spec.md` sections 5.1.6, 5.1.7, 8.5.
  - Why it matters: Kappa's `inout` and place-based rules need to ensure yielded access paths do not create unsound aliases or illegal re-entry points.

- Granule [#42](https://github.com/granule-project/granule/issues/42) `Nonlinear use of linear variables is accepted`
  - Spec tie-in: `Spec.md` sections 5.1.5, 5.1.5.1.
  - Why it matters: this is the base failure mode for any quantity system. If Kappa accepts duplicated use of owned or linear values, later borrow/resource checks are downstream noise.

- Granule [#56](https://github.com/granule-project/granule/issues/56) `Case does unboxing and matching should count as a usage`
  - Spec tie-in: `Spec.md` sections 5.1.5, 7.5, 7.6.
  - Why it matters: Kappa's narrowing is specified as non-consuming in some cases, but inspection still has quantity consequences. Matching itself cannot be treated as "free".

## Effects, Handlers, And Resumptions

- Koka [#129](https://github.com/koka-lang/koka/issues/129) `(||) does not short-circuit in the presence of exception handlers`
  - Spec tie-in: `Spec.md` sections 7.1.2A, 8, 14.8.
  - Why it matters: Kappa's short-circuit operators are semantically fixed. Effect elaboration must not accidentally reintroduce eager evaluation.

- Koka [#360](https://github.com/koka-lang/koka/issues/360) `Invoking non-resuming effect in effect handler skips finalizers`
  - Spec tie-in: `Spec.md` sections 8, 14.8.8, 14.8.9.
  - Why it matters: this is almost exactly the kind of finalization/resumption interaction Kappa calls out explicitly in the runtime obligations.

- Koka [#649](https://github.com/koka-lang/koka/issues/649) `Resumptions can escape without using raw ctl`
  - Spec tie-in: `Spec.md` sections 8.8, 14.8.5, 14.8.6.
  - Why it matters: Kappa's captured continuation boundary and one-shot or multi-shot realization need strong escape discipline. Otherwise finalizers and handler scopes become unsound.

- Effekt [#50](https://github.com/effekt-lang/effekt/issues/50) `Undetected escaping capability when using first class functions`
  - Spec tie-in: `Spec.md` sections 8, 14.8, 17.3.1.7.
  - Why it matters: first-class functions are a natural escape hatch for effect capabilities. Kappa's capture-annotated types and handler scoping need to prevent exactly this.

- Effekt [#548](https://github.com/effekt-lang/effekt/issues/548) `Capability escapes from higher-order function`
  - Spec tie-in: `Spec.md` sections 8, 14.8, 17.3.1.7.
  - Why it matters: same risk class as `#50`, but through higher-order API boundaries instead of a directly returned closure.

- Effekt [#861](https://github.com/effekt-lang/effekt/issues/861) `Multiple resumption misoptimization`
  - Spec tie-in: `Spec.md` sections 14.8.6, 17.4.
  - Why it matters: even if the front-end typing is sound, optimizer passes can easily break multi-resume semantics unless the IR carries those constraints explicitly.

- Unison [#1590](https://github.com/unisonweb/unison/issues/1590) `Wrong handler may be used at runtime when multiple handlers provide the same ability`
  - Spec tie-in: `Spec.md` sections 8, 14.8.9.
  - Why it matters: Kappa's deep-handler reinstallation rules need runtime selection to follow lexical semantics, not whatever handler happens to survive lowering.

- Unison [#1010](https://github.com/unisonweb/unison/issues/1010) `Order of mutually exclusive cases affects the result of applying ability handlers`
  - Spec tie-in: `Spec.md` sections 7.5, 8, 14.8.
  - Why it matters: case ordering should not leak into handler semantics once branches are mutually exclusive. This is a good guard against incorrect lowering or residual-control bugs.

## Macros, Hygiene, And Staging

- Lean 4 [#586](https://github.com/leanprover/lean4/issues/586) `Double-quoted names are not hygienic`
  - Spec tie-in: `Spec.md` sections 5.8.1, 5.8.4.
  - Why it matters: Kappa's quoted syntax and hygiene rules need to preserve scope even when syntax objects contain explicit or escaped names.

- Lean 4 [#793](https://github.com/leanprover/lean4/issues/793) `Name hygiene in expanded structure command`
  - Spec tie-in: `Spec.md` sections 5.8.4, 5.8.7.
  - Why it matters: hygiene bugs often surface only once generated code carries dependent names through elaboration, not at raw parse time.

- Lean 4 [#1124](https://github.com/leanprover/lean4/issues/1124) `syntax quotation produces malformed expression`
  - Spec tie-in: `Spec.md` sections 5.8.1, 5.9.2.
  - Why it matters: quotation and antiquotation need structural invariants. A malformed quoted term is a direct threat to staged safety.

- Racket [#1746](https://github.com/racket/racket/issues/1746) `Nondeterministic results from expander: cannot find exports to restore imported renamings`
  - Spec tie-in: `Spec.md` sections 5.8, 17.2.8, 17.2.10.
  - Why it matters: Kappa explicitly wants parallelism and determinism in the front-end. Expander/import renaming nondeterminism is exactly the kind of bug the spec is trying to forbid.

- Racket [#2133](https://github.com/racket/racket/issues/2133) `Importing exports that are gensyms doesn't seem to work`
  - Spec tie-in: `Spec.md` sections 2.3, 5.8.4.
  - Why it matters: macro-generated names and module export/import need to compose. Hygiene is not enough if exported generated names cannot round-trip through the module system.

## Queries And Comprehensions

- Links [#26](https://github.com/links-lang/links/issues/26) `Scoping of nested for comprehensions vs multiple binders`
  - Spec tie-in: `Spec.md` sections 10.4, 10.10.3.
  - Why it matters: Kappa's query syntax should assign one meaning to "nested generators" versus "multiple generators in one clause". This is a classic desugaring mismatch.

- Links [#834](https://github.com/links-lang/links/issues/834) `Comprehensions should allow dependencies between multiple generators`
  - Spec tie-in: `Spec.md` sections 10.4, 10.10.3.
  - Why it matters: left-to-right dependency between generators is explicitly useful and easy to break if lowering treats all sources as independent.

- Links [#856](https://github.com/links-lang/links/issues/856) `Ordering behavior is incorrect/inconsistent on some database drivers`
  - Spec tie-in: `Spec.md` sections 10.6.1, 10.10.6.
  - Why it matters: Kappa's query lowering needs an as-if rule that still preserves the normative meaning of `order by` across backends.

- Links [#1067](https://github.com/links-lang/links/issues/1067) `Use of dedup and distinct`
  - Spec tie-in: `Spec.md` sections 10.6.3, 10.10.4.
  - Why it matters: `distinct` and `dedup` are close enough semantically that implementations often blur them. Kappa should keep the lowering and result-shape rules explicit.

- Links [#1130](https://github.com/links-lang/links/issues/1130) `Record extension does not work for non-literals in query`
  - Spec tie-in: `Spec.md` sections 5.5.6, 10.10.3.
  - Why it matters: Kappa allows row extension and record manipulation inside query syntax. Lowering cannot assume that only literal records appear there.

## Totality, Termination, And Elaboration

- Idris 2 [#19](https://github.com/idris-lang/Idris2/issues/19) `Totality annotations are ignored`
  - Spec tie-in: `Spec.md` sections 6.4, 16.4.
  - Why it matters: if Kappa accepts explicit totality claims, those claims must actually constrain elaboration and checking. Otherwise `assertTotal`-style escape hatches become meaningless.

- Idris 2 [#300](https://github.com/idris-lang/Idris2/issues/300) `totality checker has difficulty with aliases for destructured parameters`
  - Spec tie-in: `Spec.md` sections 6.4.2, 6.4.5.
  - Why it matters: elaboration often inserts aliases and local lets. Kappa's termination checker needs to see through those transformations.

- Lean 4 [#1237](https://github.com/leanprover/lean4/issues/1237) `let+match breaks structural recursion`
  - Spec tie-in: `Spec.md` sections 6.3, 6.4.2, 6.4.5.
  - Why it matters: local syntax sugar should not hide a structurally decreasing recursive call from the checker.

- Lean 4 [#860](https://github.com/leanprover/lean4/issues/860) `Undocumented let rec syntax impossible to provide termination proof`
  - Spec tie-in: `Spec.md` sections 6.3, 6.4.4.
  - Why it matters: if Kappa supports local recursion, the same decreases story needs to work there too, not only at top level.

- Agda [#425](https://github.com/agda/agda/issues/425) `Projections in dot patterns not correctly counted in checking for decrease`
  - Spec tie-in: `Spec.md` sections 6.4, 7.6.
  - Why it matters: Kappa uses dotted forms and projection-heavy patterns. Termination checking needs to recognize decreases across those projections.

- Agda [#238](https://github.com/agda/agda/issues/238) `"with" looses information needed for termination`
  - Spec tie-in: `Spec.md` sections 6.4.5, 17.3.1.4.
  - Why it matters: any elaboration step that introduces helper bindings or auxiliary matches can erase the evidence needed for termination unless the checker reasons on the elaborated core carefully.

## Traits, Instances, And Coherence

The most direct Kappa-aligned trait and local implicit candidates are already in `SCALA_IMPLICIT_ISSUE_CANDIDATES.md`, especially:

- Scala 3 `#5427`
- Scala 3 `#5549`
- Scala 3 `#2234`
- Scala 3 `#739`

Additional solver/coherence issues worth keeping in mind:

- Chalk [#71](https://github.com/rust-lang/chalk/issues/71) `impl SuperTrait for T: SubTrait does not work`
  - Spec tie-in: `Spec.md` sections 12.1.1, 12.3.1.
  - Why it matters: Kappa's supertrait projection should participate in search predictably. Losing implied supertrait evidence is a direct coherence bug.

- Chalk [#515](https://github.com/rust-lang/chalk/issues/515) `Coherence issue with overlapping imps with assoc types`
  - Spec tie-in: `Spec.md` sections 12.3.1, 15.2.1.
  - Why it matters: associated members and coherence interact in hard ways. Kappa's hash-based coherence story should be tested against exactly these overlap patterns.

- Lean 4 [#3996](https://github.com/leanprover/lean4/issues/3996) `Instance search continues even when an instance was found after a later instance could not be found`
  - Spec tie-in: `Spec.md` sections 7.3.3, 12.3.1.
  - Why it matters: search order and backtracking policy affect both performance and semantics. Kappa's local and global search procedures should be explicit enough to rule this out.

## Suggested Pull Order

1. Convert the direct Scala notes into fixtures first.
   - They are closest to Kappa's implemented surface and have the shortest path to regression coverage.

2. Next, prioritize ownership and effect-handler issues.
   - These are the easiest places for Kappa's more distinctive semantics to become unsound.

3. After that, cover queries, macros/staging, and totality.
   - These are high-value, but several depend on implementation areas that are not yet as mature.
