# Spec Additions

Purpose: track unresolved spec gaps, missing requirements, and ergonomic improvements discovered during issue import.

This file is not a mirror of `new-tests/`. If a point is already represented by a staged test and does not imply new spec
material or a new ergonomic requirement, it should not be tracked here.

## Missing Spec Material

### Diagnostics and explicit spec hooks

- Reconsider the blanket ban on portable operations for `HashCode`.
  - shape: keep `HashCode` opaque by forbidding numeric conversion, serialization, printed form, and stable cross-run identity, but consider allowing `Eq HashCode` and `Ord HashCode` if they are specified as same-execution opaque-token operations that do not expose representation
  - motivation: equality or ordering on opaque runtime tokens can be useful for caching, profiling, and deterministic data structures; the important rule is still that hash-code equality or ordering cannot prove key equality and hash collisions remain semantically invisible
  - related spec: §2.7E, §12.1

- Add a standard diagnostic code for the generic `?.` ambiguity case.
  - motivation: the semantics are already specified, but the absence of a stable code makes negative conformance tests less precise than they should be
  - related spec: §7.1.1.2

- Add a standard warning or diagnostic hook for redundant safe-navigation when flow facts prove the receiver is present.
  - motivation: Kotlin issues `KT-358`, `KT-416`, and `KT-557` are diagnostic/backend-shaped rather than semantic, but Kappa tooling could provide the same quality-of-feedback when a constructor test or stable-alias fact makes `?.` unnecessary or when an explicit unwrap/refinement makes later safe navigation contradictory
  - related spec: §7.1.1.2, §7.4.1, §7.4.3, §17.2.4

- Add a standard diagnostic code for `skip` / `take` used on an unordered pipeline.
  - motivation: the spec already makes this a compile-time error, but a stable code would make regression tests and downstream tooling more disciplined
  - related spec: §10.3.2, §10.6.2

- Add a diagnostic-quality rule that function-valued application errors should surface preserved source binder names.
  - motivation: Kotlin issue `KT-435` is a reminder that diagnostics for returned or projected function values become much less useful when they fall back to synthetic parameter names even though binder metadata exists
  - related spec: §7.1.3C, §17.1.3, §17.2.4

- Add a standard fix-it / repair-suggestion shape for parser and early front-end diagnostics.
  - motivation: Kotlin issue `KT-453` shows that parser-classified errors can still have obvious source-preserving repairs, but Kappa currently standardizes partial tooling queries without saying how parser diagnostics should expose suggested edits
  - related spec: §17.2.4, §17.2.5, Appendix T

- Add guidance for suppressing cascaded duplicate diagnostics that share one root cause.
  - motivation: Kotlin issues `KT-455`, `KT-470`, and `KT-474` are definite-assignment / redeclaration specific, but the transferable Kappa concern is diagnostic quality: repeated downstream errors from one malformed binding, missing initialization, or duplicate declaration should not obscure the primary error
  - related spec: §17.2.4, §17.2.5

- Add a worked example for overload resolution between ordinary and `inout` parameters.
  - shape: show that an ordinary call such as `f(x)` does not consider an `inout`-only overload, while `f ~x` is valid only when overload resolution already determines an `inout`-compatible callee
  - motivation: Hylo-style `inout` overload bugs are a good sign that the current prose would benefit from one concrete example
  - related spec: §8.8.3, §7.1.3

- Add a worked example showing that mutable `var` locals do not participate in stable-alias flow typing.
  - motivation: Kotlin smart-cast issue families repeatedly rely on mutable-local flow analysis; Kappa intentionally keeps refinement syntax-directed, but one explicit counterexample would make that boundary much clearer
  - related spec: §7.4.3, §8.5.1

- Add a worked example clarifying what failure-side `LacksCtor` evidence implies for finite constructor types.
  - shape: show whether `if x is None then d else ...` on an `Option`-like type is enough to treat the `else` branch as the remaining constructor case for projection or payload binding
  - motivation: the current prose is much clearer on success-side `HasCtor` projection than on residual negative-constructor narrowing
  - related spec: §7.4.1, §17.3.1.8

### Effects, handlers, resumptions, and finalization

- Add an explicit lexical-selection example for cases where multiple handlers of the same effect head are active.
  - motivation: Unison-style handler-selection bugs are exactly the kind of thing the spec should pin down with a worked example
  - related spec: §8.1.9, §8.1.10, §14.8.9

- Add an explicit example showing finalizer behavior when an operation does not resume.
  - motivation: Koka-style "skips finalizers" failures are easier to prevent when the intended behavior is exemplified, not only stated abstractly
  - related spec: §8.1.11, §8.7.2, §14.8.8

- Add a short optimizer-facing note or example showing that multi-resume semantics must be preserved across lowering and optimization.
  - motivation: several issue families fail only after optimization, not at the surface typing layer
  - related spec: §8.1.8.1, §14.8.6, §17.4

### FFI, dynamic values, and bridge boundaries

- Add a stable diagnostic family for portable ABI exclusions.
  - shape: distinct codes for non-first-order ABI-visible functions, closure/callback/function values without monitored higher-order boundaries, dictionaries/trait evidence/effects/handlers/resumptions in portable ABI, direct borrowed ABI parameters/results, variadic raw bindings without an explicit backend adapter profile, and bare owning `OpaqueHandle` in a refined portable surface
  - motivation: Zig, Roc, Mojo, OCaml, and Kotlin issues `KT-387`, `KT-388`, `KT-389`, and `KT-422` repeatedly collapse to "this source shape must not be treated as a portable ABI value"; stable codes would make negative conformance tests precise
  - related spec: §17.7.1, §17.7.3, §17.7.4B

- Add a worked raw-host-binding example for deterministic overload spelling and host-member identity recording.
  - shape: two same-spelling host functions that differ by arity or host signature, with generated Kappa spellings and recorded original host identities
  - motivation: Gleam and Zig external-function issues show that raw binding generation needs deterministic identity and diagnostics, not just "whatever name links"
  - related spec: §17.7.2, §17.1.9

- Add a worked bridge example showing that a runtime bridge failure is not successful completion.
  - shape: a bridge bind or later member invocation fails contract validation and returns bridge failure / cast blame / typed failure rather than fabricating a success value
  - motivation: Mojo interop issues show how easy it is for foreign errors to be printed while the boundary still exits successfully
  - related spec: §17.7.4A, §17.7.6, §17.7.7, §17.7.7A

- Add a worked trusted-summary example that strengthens a raw foreign surface.
  - shape: raw nullable pointer or untyped host failure becomes a refined `Option`, `Result`, or `IO e a` only after a trusted summary names the nullability or error-translation contract
  - motivation: Zig `errno`, Roc host panic, Creusot trusted-external-spec issues, and Kotlin `KT-406` Java external-constant nullability all point at the same missing example
  - related spec: §17.7.3, §17.7.4, §17.7.4A

- Clarify how raw C/native union layouts are surfaced.
  - shape: state whether raw unions are exposed as opaque handles, backend-specific unsafe values, generated tagged wrappers, or a future `std.ffi` / `std.unsafe` layout type, and require explicit unsafe/backend gating for type punning
  - motivation: Mojo's `UnsafeUnion` tests and Zig C ABI tests show that union size/alignment and reinterpretation semantics are real FFI needs, but Kappa currently mentions union layout in adapter identity without defining a portable user-facing raw-union surface
  - related spec: §16.5, §17.7.1, §17.7.2.1, §17.14

- Clarify whether C `long double` and C complex scalar spellings are in scope.
  - shape: either add explicit `std.ffi.c` spelling types for `long double` and C complex values with target-ABI identity requirements, or state that bindings for those values must remain opaque / shimmed / backend-specific
  - motivation: Zig's C ABI tests include `long double` and C complex values, which are common native ABI edge cases not covered by the current `std.ffi.c` spelling list
  - related spec: §2.7, §17.7.1, §17.7.2.1

- Consider a standard explicit adapter surface for host callback interfaces.
  - shape: a library or bridge form that adapts a Kappa function value to a named single-method host interface only when a monitored higher-order bridge contract is available, rather than silently treating every closure as a host object
  - motivation: Kotlin issue `KT-379` shows the ergonomic pressure for passing closures to `Runnable` / `Callable`-style APIs; Kappa should keep the boundary explicit while still offering a disciplined callback adapter story
  - related spec: §2.7, §17.7.4A, §17.7.7, §17.7.7A

### Runtime, fibers, and resource coordination

- Consider a standard collection-oriented fiber combinator such as `awaitAll`, `joinAll`, or `collectAll`.
  - shape: specify ordering of results, whether all fibers are awaited after the first failure, how interruption of siblings is handled, and how `Cause.Both` / `Cause.Then` composition records multiple failures and cleanup failures
  - motivation: Encore issue `#802` shows recurring demand for synchronizing a collection of futures without serial blocking loops; Kappa already specifies `await`, `join`, `race`, and mandatory finalizer waits, but not a portable all-of combinator
  - related spec: §8.1.3, §8.1.3B, §14.8.4C

- Consider a backend-conformance hook for runtime initialization that mutates ambient host-process state.
  - shape: specify whether runtime startup may alter stdio blocking flags, signal handlers, thread attributes, or equivalent host state, and how such changes must be restored or documented
  - motivation: Pony issue `#205` shows that scheduler/blocking implementation choices can leak outside the program as ambient host-state mutation
  - related spec: §17.5, §17.13, §17.14

### Totality, local recursion, and elaboration-preserved decrease evidence

- Add an explicit local-recursion example under Chapter 6.4.
  - shape: `let rec` or local recursive helper with an observable decrease argument
  - motivation: Lean and Idris issue families repeatedly hit the gap between top-level and local recursion stories
  - related spec: §6.3, §6.4

- Add an explicit statement, with example, that elaboration-introduced helpers must preserve the evidence needed for termination checking.
  - motivation: Agda- and Lean-style `with` / helper-lowering failures are exactly this class of bug
  - related spec: §6.4.5, §17.3.1.4

- Add an explicit statement, with example, about whether ruled-out constructor facts participate in branch-local normalization.
  - shape: clarify whether a residual branch such as `case LT -> ...; case _ -> ...` may treat a scrutinee-dependent boolean
    defined by that case split as definitionally equal to `True` in the residual branch, and whether the same principle applies
    to multi-argument clause-style definitions once earlier patterns have been ruled out
  - motivation: Idris2 issues `#26` and `#27` show that users expect ruled-out-pattern information to help not only projection
    and reachability, but also compile-time proof normalization
  - related spec: §7.4.1, §7.5.4, §14.3, §17.3.1.8

### Local declarations and captured generic context

- Add a worked example where a block-local declaration captures an outer generic or compile-time parameter.
  - motivation: Hylo-style nested-generic failures are a good sign that §14.1.1 needs at least one concrete example involving more than ordinary term capture
  - related spec: §6.3.1, §14.1.1

### Macro hygiene across module boundaries

- Add a worked example where macro-generated names are exported from one module and imported from another.
  - motivation: hygiene problems often appear only at the module boundary
  - related spec: §2.3, §5.8.4

- Add a worked example where expanded generated structure or record code refers to dependent names and must still resolve hygienically.
  - motivation: Lean-style failures in expanded dependent structure code are good evidence that this deserves an explicit example
  - related spec: §5.8.4, §5.8.7

## Candidate Ergonomic Improvements

These are not necessarily v0.1 requirements, but they are worth tracking while importing issue families.

- Consider whether `?.` should eventually gain a less annotation-heavy story in fully generic code.
  - current v0.1 design is explicit and coherent, but Kotlin-style use cases such as `KT-600` and `KT-636` may produce recurring friction around generic optional unwrapping helpers, expected result types, and whether portable receiver-oriented `Option` helper functions should exist in the standard library
  - related spec: §7.1.1.2

- Consider a future refinement rule that treats successful safe-navigation/elvis boolean conditions as evidence that the receiver is present.
  - shape: conditions like `x?.pred ?: False` would refine `x` in the success branch when the condition is pure and the success path can arise only from the present case
  - motivation: Kotlin issue families such as `KT-2127` and `KT-8492` suggest this would be a meaningful ergonomic improvement, but v0.1 does not currently promise it
  - related spec: §7.1.1.2, §7.1.2, §7.4.1, §7.4.2

- Consider a scoped condition-binding form for introducing a branch-local alias of a tested expression.
  - shape: an `if let`-style or condition-local alias form that binds a name only within the success branch, instead of forcing a wider-scope `let` just to participate in refinement
  - motivation: Kotlin issue `KT-132` is a good example of the friction here: users want a narrow-scope alias for a complex expression so the success branch can reuse it without widening the temporary's scope
  - related spec: §6.3, §7.4.1, §7.4.3

- Consider specifying the portable `Array` construction and convenience-member surface more fully.
  - shape: standardize how users construct an `Array a`, query its length, index/update it, test membership, sort/search it, obtain range-like views such as `indices` or `lastIndex`, and whether any multi-index accessor/update forms are intended to be portable prelude members
  - motivation: Kotlin issues `KT-199`, `KT-200`, `KT-326`, `KT-331`, `KT-346`, `KT-347`, `KT-348`, `KT-376`, `KT-417`, `KT-425`, `KT-579`, `KT-580`, `KT-581`, `KT-602`, and `KT-632` point at the same gap: `Array a` is a prelude type in Kappa, but the current spec does not define a user-facing constructor, indexing/update behavior, membership/sorting/search helpers, range-like views, or common accessor vocabulary
  - related spec: §2.6, §2.8.3, §2.8.4, §17.1

- Clarify the portable encounter-order story for ordinary `Set` / `Map` iteration and transformations.
  - shape: specify whether plain `Set` / `Map` iteration is ordered, insertion-ordered, sorted-by-structure, or explicitly unordered, and if future surface operations such as union/difference/filter/mapping are portable, specify whether they preserve left-input encounter order or deliberately yield unordered results
  - motivation: Kotlin issue `KT-647` highlights that users rely on stable order from collection transforms, but Kappa currently specifies orderedness mainly for query/comprehension pipelines and separately forbids backend hash-table iteration order from changing source semantics; the ordinary `Set` / `Map` surface still needs an explicit portability story
  - related spec: §10.3.2, §10.6, §17.1

- Clarify the portable range surface and endpoint-overflow behavior.
  - shape: standardize which prelude scalar types have `Rangeable` evidence, whether exact-width integer and `Char` ranges share one canonical range representation or distinct representations, and how inclusive ranges near maximum endpoints terminate without overflow
  - motivation: Kotlin issues `KT-489`, `KT-492`, and `KT-580` show that numeric and character range APIs need an explicit portability story, especially when an inclusive range reaches the maximum representable endpoint or when a range endpoint comes from an array accessor helper
  - related spec: §4.4, §10.2, §10.10.1A, §17.4

- Consider explicitly accepting or rejecting unary plus as numeric-expression sugar.
  - shape: decide whether `+n` should be a standard prefix operator analogous to unary `-`, whether it is only ordinary user-defined prefix syntax when imported, or whether it should remain absent from v0.1 numeric literal conventions
  - motivation: Kotlin issue `KT-494` highlights the same target-typing pressure for unary `+` as for unary `-`; Kappa currently specifies only prefix `-`, so conformance tests can cover `-1` but not `+1` without a clearer portability rule
  - related spec: §3.5.3, §4.1.4, §4.1.5

- Consider structural forwarding / delegation helpers for signature-shaped APIs.
  - shape: decide whether Kappa should offer library or language sugar for constructing a dictionary/package/signature value by forwarding same-named compatible members from an unrelated value, without adding ambient structural subtyping or nominal inheritance
  - motivation: Kotlin issue `KT-505` asks for delegation by method signature rather than by declared interface conformance; Kappa already has explicit records, packages, and dictionaries, but no concise forwarding helper for the common "this value has the right operations" adapter case
  - related spec: §5.5.10, §5.5.13, §7.1.3B, §12.2

- Consider specifying the portable `String` convenience-member surface more fully.
  - shape: standardize the core operations that every portable `String` implementation must expose, such as replacement, slicing/search, length, and Unicode/codepoint expectations, instead of leaving this entirely to host bridges or ad hoc prelude growth
  - motivation: Kotlin issues `KT-427`, `KT-446`, and `KT-480` are library-shaped, but they highlight the same portability risk for Kappa: if `String` is a core prelude type, users need to know which operations and renderings are portable and which are host-profile extensions
  - related spec: §2.6, §4.3.4, §17.1, §17.7

- Clarify whether source-visible reference / object identity equality exists.
  - shape: either explicitly decline a general `identityEquals`-style primitive in favor of `Eq`, semantic-object identity, `FiberId`, and explicit handle/fiber identity APIs, or define a narrow standard primitive for reference-like values that have source-visible identity
  - motivation: Kotlin issues `KT-481` and `KT-574` are about an intrinsic `identityEquals`; Kappa currently specifies several identity notions but no general source-level physical/reference equality operator
  - related spec: §2.8.6, §8.1.3F, §14.4, §17.3.4

- Clarify which `DynamicType` / `DynRep` instances are portable.
  - shape: explicitly list whether tuples, records, functions, receiver-marked functions, dictionaries, erased/indexed/generic type applications, and other higher-order values have canonical `DynamicType` instances, and if not, state the intended opaque or non-representable behavior
  - motivation: Kotlin issues `KT-349`, `KT-350`, `KT-445`, `KT-447`, and `KT-515` are reminders that "runtime type information exists" is not enough; portable users need to know which type forms can be represented by default, which can expose host class tokens through a bridge, and which require custom runtime carriers
  - related spec: §2.11, §17.7, §17.10, §17.12.1

- Consider a scope-function or pipeline-binding form for transformation chains.
  - shape: a `with` / `let-pipe` style form that evaluates a receiver once, binds it under a short branch-local name, and continues the chain without forcing a wider-scope temporary
  - motivation: Kotlin issue `KT-240` captures the same readability pressure as pipeline-heavy Kappa code: users want the clarity of named intermediate values without repeatedly inventing outer-scope names
  - related spec: §6.3, §7.1, Appendix B

- Consider permitting a line break before a dotted-form suffix as an expression-continuation context.
  - shape: allow a chain such as `value` followed by an indented `.member` / `?.member` suffix on the next line, with the same semantics as `value.member`
  - motivation: Kotlin issues `KT-337` and `KT-621` capture a common formatting expectation for long fluent chains; Kappa's §3.4 continuation list currently mentions infix operators but not a leading dotted suffix
  - related spec: §3.4, §2.8.3, §7.1.1

- Consider a procedure/function type shorthand for `Unit` results.
  - shape: optional syntax for a callback or function type whose result is `Unit`, without requiring the result type to be written at every use site
  - motivation: Kotlin issues `KT-339`, `KT-432`, and `KT-572` reflect recurring ergonomic pressure around `Unit`-returning callback and function types; Kappa intentionally keeps `Unit` explicit today, but callback-heavy APIs may benefit from a compact spelling later
  - related spec: §4.5, §7.2.1, §8.4

- Consider ordinary function defaults as a future ergonomic extension.
  - current v0.1 design deliberately limits defaults to named constructor application and says ordinary function defaults do not exist
  - motivation: Kotlin issues `KT-265` and `KT-374` are reminders that users often expect the same named/default-argument ergonomics for ordinary functions that Kappa now has for constructors and named function application
  - related spec: §7.1.3C, §11.1, §11.1.1

- Consider augmented assignment and increment/decrement sugar as a future ergonomic extension.
  - shape: if added, it should be explicit about whether `x += y`, `x++`, and similar forms require a mutable `var`/place, whether they desugar through named receiver-marked functions or trait members, and whether immutable bindings may call mutating methods on their internal state without rebinding the binding itself
  - motivation: Kotlin issues `KT-469`, `KT-471`, `KT-512`, `KT-556`, `KT-575`, and `KT-613` show recurring pressure for user-defined compound assignment and increment protocols, including backend fragility around increment lowering on mutable places; Kappa currently keeps mutation explicit through `var`, references, `inout`, and ordinary function calls
  - related spec: §3.5, §8.2, §8.8, §12

- Consider optional collection / iterator sources as explicit iteration sugar.
  - shape: decide whether `for` and query generators should accept `Option src` as zero-or-one source expansion when `src` itself is iterable, or require users to spell the `match` / `?: empty` conversion explicitly
  - motivation: Kotlin issue `KT-531` asks for nullable iterators to mean "skip the loop if absent"; Kappa has `Option`, `Iterator`, and `IntoQuery`, but does not currently define an `Option`-as-empty iteration source convention
  - related spec: §5.4.9, §8.7.5, §8.7.6, §10.10.1

- Consider statically checked literal-format annotations or literal-refinement macros.
  - shape: a way to attach a parser / format witness to a string-like literal so literals such as decimal, path, regex, SQL fragment, or domain-specific numeric text can be checked at compile time without becoming an ordinary unchecked `String`
  - motivation: Kotlin issue `KT-473` requests format annotations for statically checking literal strings; Kappa has numeric literal suffixes and prefixed interpolated macros, but not a general spec-level contract for arbitrary literal format refinement
  - related spec: §4.1.6, §4.3.4, §5.8

- Consider a compact computed-projection getter shorthand.
  - shape: allow a projection whose body is only a pure getter to use a shorter spelling than `get -> expr`, and clarify whether same-spelling computed projections may coexist when their receiver/root types are distinct, while keeping selector projections and read/write accessor bundles syntactically unambiguous
  - motivation: Kotlin issues `KT-487` and `KT-523` ask for extension-property initializer syntax and same-name extension properties across receiver types; Kappa already has explicit expanded-form projections, but simple computed read-only projections are verbose for extension-property-style use and receiver-specific projection naming policy is not yet ergonomic
  - related spec: §6.1.1, §2.8.4

- Consider result-type inference for getter-only projections.
  - shape: allow a read-only expanded projection whose only accessor is `get -> expr` to omit the inline result type, inferring it from the getter body while still requiring explicit result types for selector, settable, sinkable, or exported ABI-sensitive projection surfaces
  - motivation: Kotlin issue `KT-550` asks for property type inference from an explicit getter; Kappa v0.1 currently requires every projection definition to carry an inline result type, which is rigorous but verbose for simple computed read-only views
  - related spec: §6.1.1, §6.2, §17.3

- Consider single-expression loop body sugar.
  - shape: decide whether `while` / `for` may have a compact one-expression or one-statement body form that elaborates to the existing `do`-suite body, or explicitly keep loop bodies block-only for readability and control-flow clarity
  - motivation: Kotlin issues `KT-552` and `KT-576` are about loop binders and diagnostics when the loop body is not a block; Kappa currently specifies `while ... do` and `for ... do` bodies as do-block suites, but users may still expect a compact single-body form for trivial mutation or effect calls
  - related spec: §8.5, §8.7.5, §8.7.6

- Consider a test-scope visibility/profile model for package builds.
  - shape: distinguish production roots from test roots in package mode, and decide whether tests may access private declarations through explicit friend/test visibility, generated adapters, or only through public APIs
  - motivation: Kotlin issue `KT-525` requests test compilation roots and synthetic accessors for private members; Kappa has an Appendix T harness and package/script modes, but not a build-profile visibility story for test-only access
  - related spec: §2.5, §2.7, §17.3, Appendix T

- Consider finite type-set bounds for generic functions as a future ergonomic extension.
  - shape: a way to say a type parameter ranges over a closed set of known types, enabling type-directed branch bodies without replacing the API with an ordinary value-level union
  - motivation: Kotlin issue `KT-320` references Ceylon-style enumerated type bounds; Kappa has value-level unions and finite-inhabitant tooling, but no compact type-parameter-bound syntax for this use case
  - related spec: §5.3, §5.4, §17.3.6A

- Consider assertion / requirement precondition forms with lazy diagnostics.
  - shape: a lightweight `assert condition, message` or `require condition, message` surface, or an equivalent standard-library convention, where expensive diagnostic messages are evaluated only on failure
  - motivation: Kotlin issue `KT-295` points at both ergonomics and performance: users want concise precondition checks without eagerly building failure messages
  - related spec: §8, §9

- Consider standard diagnostic codes for:
  - handler-selection ambiguity, if such ambiguity is ever exposed to users
  - applying a non-callable value, especially when the callee resolves to a field/projection value rather than a function
    or receiver-method descriptor (`KT-310`)

- Consider adding a compact "spec testing examples" appendix subsection for:
  - local recursion
  - handler finalization
  - macro hygiene across module boundaries

The remaining items here are intended to represent actual spec work, not simple evidence that an existing rule can be
tested.
