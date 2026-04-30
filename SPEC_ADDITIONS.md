# Spec Additions

Purpose: track unresolved spec gaps, missing requirements, and ergonomic improvements discovered during issue import.

This file is not a mirror of `new-tests/`. If a point is already represented by a staged test and does not imply new spec
material or a new ergonomic requirement, it should not be tracked here.

## Missing Spec Material

### Diagnostics and explicit spec hooks



- Add a warning-policy rule that synthesized code must not hide real unused items or emit un-actionable warnings.
  - shape: warnings such as `unused`, `redundant constraint`, `implicit lift`, or similar source-oriented lints should be computed against the user's apparent declarations rather than naively against hidden compiler-generated helper code, so generated derives do not suppress a real unused-item warning and do not introduce warnings the user cannot fix without disabling the lint family entirely
  - motivation: GHC issues `#18148`, `#18165`, `#19865`, `#20054`, and `#20688` show both sides of the same hygiene problem: once deriving elaborates to hidden code, warning passes can accidentally treat the generated code as if it were user-written and thereby either invent un-actionable warnings or suppress the warning the user actually needed
  - related spec: §12.5, §17.2.4, §17.2.5

- Consider an optional suspicious-shadowing warning for local binders that hide a visible top-level declaration.
  - shape: if Kappa standardizes shadowing warnings or lints, pattern binders, function parameters, and local bindings that shadow an in-scope top-level declaration of the same spelling should be eligible for that warning even though the underlying lexical-shadowing semantics remain legal and deterministic; if such shadowing remains legal and later appears in diagnostics, the renderer should distinguish the local binder from the top-level declaration clearly instead of printing two visually identical names with different identities
  - motivation: Idris2 issue `#704` shows how easy it is for lawful shadowing to derail debugging and normalization reasoning when a proof unexpectedly mentions a fresh local binder rather than the intended top-level constant, and Lean 4 issue `#243` shows the diagnostic version of the same problem when a pattern binder shadows a global declaration and the error message renders both as the same spelling
  - related spec: §2.8.1, §6.3, §17.2.4, §17.2.5

- Add a diagnostic-suggestion rule that repair hints must not recommend derive strategies that only defer failure into runtime holes.
  - shape: if a derivation fails and the compiler suggests enabling a derive-related feature or switching strategies, that suggestion should be reserved for cases where the resulting code would still be statically well-formed and total by Kappa's ordinary standards, not merely accepted with missing methods, bottom members, or another deferred failure mode
  - motivation: GHC issues `#19418` and `#19692` show that feature-enabling hints around deriving can become actively misleading when the suggested flag only trades one compile-time failure for a weaker runtime or missing-method failure
  - related spec: §12.5, §17.2.4, §17.2.5

- Add a structured diagnostic-record requirement for expected/actual mismatch facts.
  - shape: missing-signature and inferred-signature records are now covered by the P0 diagnostic-code/payload patches; the remaining gap is to pin down the stable typed payload for ordinary expected-versus-actual mismatches, including expected type, actual type, mismatch origin, coercion/subsumption attempt, and candidate repair edits, so editors do not scrape human prose with regexes
  - motivation: GHC issue `#21063` still applies to ordinary mismatch facts even after inferred-signature payloads are standardized
  - related spec: §17.2.4, §17.2.5, Appendix T

- Add a diagnostic-quality rule that redundancy warnings blame the actually redundant premise.
  - shape: when a warning reports a redundant constraint, bound variable, branch, inaccessible rhs, or similar declaration fragment, the primary blame site should be the eliminable item itself rather than a prerequisite that it merely implies or subsumes, the warning should fire only when the checker can justify true inaccessibility rather than a fragile approximation, and inferred or suggested signatures should simplify away constraints already implied by visible supertrait or equality information before they are shown to users
  - motivation: GHC issues `#20602`, `#20703`, and `#21030` are simple reminders that a technically correct "something here is redundant" warning is still poor feedback if it points at the wrong premise, if the checker has not actually established inaccessibility, or if an inferred signature displays an extra constraint that the visible context already entails
  - related spec: §12.1, §17.2.4, §17.2.5

- Add a pretty-printing hygiene rule that built-in kind/type names respect lexical scope when rendered.
  - shape: if a diagnostic, REPL query, or explanatory rendering prints a built-in type or kind name that is shadowed by an in-scope user declaration, the printer should qualify or otherwise disambiguate the built-in name instead of emitting a misleading unqualified rendering
  - motivation: GHC issue `#20627` shows that even a semantically correct pretty-printer becomes misleading once a built-in name such as `Type` can be shadowed in user scope
  - related spec: §3.3, §17.2.4, §17.2.5

- Add a Unicode-rendering rule for diagnostics and REPL displays.
  - shape: when a diagnostic or interactive query prints a Unicode scalar or short text fragment, the renderer should either display the actual scalar or use one documented canonical escape form consistently, rather than switching ad hoc between decimal escapes, glyphs, and other spellings that obscure what character was actually present
  - motivation: GHC issue `#21161` and Idris2 issue `#759` show that "supports Unicode" is not enough if ordinary character or string displays still choose confusing escape forms inconsistently
  - related spec: §2.7F, §17.2.4.2A, §17.2.5

- Clarify Unicode source acceptance for literal text versus escaped text.
  - shape: if a source file contains a Unicode scalar sequence directly inside a string/character/grapheme literal, the acceptance rules should match the corresponding escaped form whenever both denote the same scalar sequence under the current source encoding/profile, including default-ignorable scalars such as joiners when they are part of otherwise valid literal text
  - motivation: GHC issue `#21228` shows that Unicode source support feels broken if the escaped spelling of a text fragment is accepted but the direct literal spelling is rejected by the lexer
  - related spec: §2.7F, §4.3, §4.4, §17.2.4.2A

- Consider first-class optional-subform views for future `Syntax`-inspection APIs.
  - shape: if Kappa exposes quotation-pattern matching or source-preserving view combinators over `Syntax t`, optional or punctuationally optional subforms should be matchable in one source-level pattern or view without forcing macro authors to duplicate the whole pattern for present/absent cases
  - motivation: Lean 4 issue `#174` shows that syntax-pattern APIs become awkward quickly when optional arm markers or optional result-type fragments cannot be expressed directly
  - related spec: §5.8.1, §5.8.5

- Add a definition-site linting rule for obvious macro precedence hazards.
  - shape: if a macro or elaboration helper surface admits adjacent syntax arguments whose default precedence/category choice predictably causes downstream misparse at ordinary call sites, or if it names built-in literal/token spellings that the host lexer/parser will never actually route through that macro surface, the implementation should emit a definition-site warning or error with a suggested explicit annotation or correction rather than waiting for an unrelated later use-site parse failure
  - motivation: Lean 4 issues `#180` and `#242` show that precedence footguns and unreachable built-in-token macro declarations are much cheaper to repair when caught where the macro is defined instead of at some later use site
  - related spec: §5.8.4, §17.2.5

- Add a macro-observability requirement that traces can attribute steps inside macro/elaboration helpers.
  - shape: if Kappa exposes pipeline traces, elaboration traces, or macro debugging output, user-authored macro/elaboration helpers should be able to contribute deterministic trace events associated with the current splice or macro invocation instead of appearing only as an opaque black box between outer checkpoints
  - motivation: Lean 4 issue `#184` shows that macro authors need observability inside macro execution, not only around it
  - related spec: §5.8.4, §17.1.6, §17.2.5

- Add an extension-surface consistency rule for registered elaboration hooks.
  - shape: if Kappa later exposes registered tactic, DSL, parser, or elaboration hooks beyond ordinary `$(...)`, the registration path should either also install reachable surface syntax and help/docs metadata for that hook or be rejected up front; the implementation should not accept a dangling handler that no user-written source form can actually invoke
  - motivation: Lean 4 issue `#453` shows how easy it is for an extension surface to drift into a half-registered state where an elaborator exists in the implementation but no corresponding syntax or user-facing entry point reaches it
  - related spec: §5.8.3, §17.2.5

- Add a desugaring-aware diagnostic rule for monadic lifting and similar elaboration rewrites.
  - shape: when a source form is elaborated through monadic lifting, implicit `do`-block restructuring, clause splitting, helper-lowering for recursion/termination, or another local desugaring step, diagnostics should explain the source-level scoping, overlap, or missing-construct problem rather than primarily reporting an out-of-scope binder, unused generated clause, or other artifact that exists only in the lowered helper form
  - motivation: Lean 4 issue `#247` shows that once elaboration lifts `(← ...)` out of its apparent lexical nest, a raw "unknown identifier" on the lowered binder is much less useful than a source-oriented message that points at the need for an explicit nested `do` or equivalent source repair, and Lean 4 issue `#29` shows the same provenance problem after equation-compiler splitting when a generated internal clause is blamed as if the user had written a dead equation
  - related spec: §8.2, §17.2.4, §17.2.5

- Consider a future custom-delaboration / notation-rendering surface that works for local heads too.
  - shape: if Kappa later exposes user-authored pretty-print, delaboration, or notation-rendering hooks over reflected syntax, those hooks should be able to match locally bound heads and simple applied heads under the current lexical environment, not only globally named declarations, so semantic queries and goal displays can recover local notation faithfully
  - motivation: Lean 4 issue `#465` shows that notation/rendering hooks become much less useful if locally bound relation or operator heads cannot round-trip through the same rendering path as global ones
  - related spec: §2.3, §5.8.1, §17.2.5

- Add a tooling/help-query requirement that reserved and built-in names remain queriable uniformly.
  - shape: documentation, hover, REPL help, and similar semantic-query surfaces should accept built-in types, keywords, and other reserved spellings through the same user-facing query path as ordinary names, even if the internal parser classifies them specially
  - motivation: Idris2 issue `#651` shows that a help surface becomes surprisingly brittle if builtin names such as `Int` or reserved spellings fall outside the query grammar that works for ordinary declarations
  - related spec: §3.3, §17.2.5

- Add a namespace-hygiene rule for built-in syntax names in quoted/name-construction APIs.
  - shape: if Kappa offers APIs that construct names or quoted syntax from raw strings, those APIs should respect namespace and built-in-syntax distinctions consistently, so asking for a term constructor name cannot silently resolve to an unrelated built-in type/operator namespace entry
  - motivation: GHC issue `#20902` shows that raw name-construction helpers become unreliable when built-in syntax handling ignores namespace in some cases
  - related spec: §3.3, §17.2.5, Appendix T

- Add a semantic-query rendering rule that printed types include the full kind/runtime-representation structure needed for correctness.
  - shape: REPL `:type`-style output, hover signatures, and other semantic-query surfaces should not drop levity, runtime-representation, kind information, multiplicity/linearity information, or obviously simplifiable type-family/constraint structure when that information changes the meaning, readability, or admissibility of the type being shown; likewise, if a query instantiates inferred binders for presentation, it should do so coherently rather than showing a confusing half-instantiated binder prefix, and if a verbose or fully explicit mode prints universe/kind arguments it should still render them in syntactically well-formed source-like form
  - motivation: GHC issues `#20910`, `#20974`, `#21088`, and `#21275` show that type query output can become actively misleading if it omits the kind/runtime-representation or multiplicity structure that makes a binding legal, leaves simple type-family/constraint structure in an unnecessarily raw form, or instantiates only some inferred binders and leaves the rest in a surprising residual quantifier list; Lean 4 issue `#190` shows that even when the renderer chooses to expose explicit universe information, that output still needs an explicit well-formedness contract
  - related spec: §5.1, §17.2.5

- Add a source-like signature-rendering rule for semantic queries and hover output.
  - shape: when tooling renders a declaration signature, function type, or theorem type for users, it should prefer a source-like telescope/signature layout that keeps named parameters on the left of the colon and preserves user-facing binder sugar when available, rather than naively dumping the fully elaborated term to the right of `:`
  - motivation: Lean 4 issue `#375` shows that hover text and similar tooling become harder to read once signatures are rendered as raw terms instead of as source-like declarations
  - related spec: §5.1, §5.3, §17.2.5

- Add a semantic-query baseline rule that hover/info queries cover local binders and variables.
  - shape: if Kappa exposes hover, point-info, or similar semantic-query services, those services should report the types of local variables, parameters, and other in-scope binders at least as reliably as they report top-level names, rather than limiting semantic info to global declarations
  - motivation: Lean 4 issue `#239` shows that semantic-query support feels incomplete if a hover service cannot answer the most basic local-variable type questions inside ordinary code
  - related spec: §5.1, §17.2.5

- Add a tooling/determinism rule that unordered candidate sets are normalized before rendering.
  - shape: when diagnostics, REPL queries, or tooling surfaces display a set of instances, candidates, packages, or other semantically unordered items, the implementation should sort or otherwise canonicalize the presentation so optimization choices, load order, or parallel execution do not change the visible ordering
  - motivation: GHC issue `#21121` shows that semantically irrelevant instance-loading order can leak into observable output unless the presentation layer canonicalizes unordered sets before printing them
  - related spec: §15.2.1, §17.2.5, §17.2.8

- Add a hole-fit / completion requirement that in-scope implicit evidence is suggested like ordinary values.
  - shape: when tooling reports hole fits, typed completion candidates, or local proof terms, it should surface usable in-scope implicit binders, dictionaries, or equivalent evidence providers alongside ordinary explicit names whenever they satisfy the requested type
  - motivation: GHC issue `#20808` shows that hole-driven workflows become less effective if the tool reports ordinary explicit fits but silently omits the implicit evidence already available in scope
  - related spec: §7.3, §12.3, §17.2.5

- Consider fine-grained per-definition warning control.
  - shape: if Kappa standardizes warning-suppression pragmas or attributes, allow them to target one declaration, binding group, or local definition rather than only whole modules, so users can silence one intentionally partial or intentionally unused definition without muting the same warning family everywhere nearby
  - motivation: GHC issue `#20643` points at a real ergonomics gap in any warning-heavy language: module-wide suppression is often too coarse once the language intentionally warns about incomplete patterns, unused declarations, or similar local hazards
  - related spec: §17.2.4, §17.2.5

- Consider a build-policy / warning surface for residual holes and explicitly partial declarations.
  - shape: a module, package, or build profile should be able to request warnings or hard errors when compilation leaves placeholder holes, admitted definitions, or explicitly partial declarations in artifacts intended for normal builds, without forcing the same policy in exploratory or teaching contexts
  - motivation: Idris2 issue `#686` shows the practical need for a configurable policy between "always allow while iterating" and "fail production builds if holes/partials remain"
  - related spec: §6.4, §17.2, Appendix T

- Clarify whether declaration-order changes are permitted to affect generalization under restricted polymorphism modes.
  - shape: if Kappa ever supports a monomorphism-restriction-like mode or any reduced-generalization profile, specify whether equivalent declarations may still be checked in an order-independent way, or else state exactly which dependency/order information is allowed to affect generalization so users do not get acceptance changes from harmless reordering alone
  - motivation: GHC issue `#21309` shows that once reduced generalization depends on partial type information, source-order sensitivity quickly becomes a user-visible semantic hazard unless it is either forbidden or explicitly documented
  - related spec: §6.4, §7.1.3, §17.1, §17.3.1

- Clarify the scoped-name policy for explicit kind/type binders introduced by signatures versus declaration headers.
  - shape: if Kappa allows both standalone signatures and inline binder annotations, specify when a later binder name is merely an alias for an already introduced variable and when it must be a fresh variable, so kind/type equality does not accidentally depend on which surface form introduced the binder first
  - motivation: GHC issues `#20916`, `#20926`, and `#21126` show that scoped binder naming becomes inconsistent quickly if standalone signatures, declaration headers, and mixed-kind equality declarations disagree about whether two names may denote the same variable
  - related spec: §5.1, §5.1.2, §5.3

- Clarify whether placeholder types are ever permitted inside explicit type applications.
  - shape: if Kappa later exposes typed holes or placeholder types beyond expression holes, specify whether an explicit type application may contain them in ordinary source, only in interactive/query contexts, or never; also specify whether enabling such placeholders implicitly enables any broader partial-signature mode and whether that implication can be disabled
  - motivation: GHC issue `#21140` shows that users reasonably expect placeholder types inside explicit type applications to follow a stated hole policy rather than silently inheriting permission from an unrelated feature
  - related spec: §7.1.3, §7.3.2, §17.2.4, §17.2.5

- Consider whether application-boundary subsumption should preserve more eta-equivalent higher-order uses of constrained aliases.
  - shape: if Kappa keeps its current localized subsumption model, say explicitly whether passing a helper of alias type `T = implicit/constraint => R` to a higher-order consumer should require user-written eta-expansion, receive an implementation-inserted eta-expansion, or be rejected uniformly; the goal is that obviously eta-equivalent programs do not diverge unpredictably merely because the constrained arrow sits behind a type alias
  - motivation: GHC issue `#21168` shows the ergonomic friction once a language supports localized higher-order subsumption plus implicit/constraint-carrying aliases but leaves eta-equivalent acceptance underspecified
  - related spec: §7.1.3, §7.1.3C, §7.3, §17.3.1

- Consider an explicit `Unsatisfiable`-style rejected-constraint surface distinct from custom type-error computation.
  - shape: if Kappa later wants library-authored "this constraint can never be solved" declarations, consider a dedicated rejected-constraint form whose operational meaning is simply that the obligation is unsatisfiable, instead of overloading richer custom-error computation or elaboration tricks for that purpose
  - motivation: GHC issue `#20835` is a good reminder that "always reject this constraint" and "compute a custom type error message" are related but not identical design needs
  - related spec: §12.1, §12.3, §17.2.4

- Add a diagnostic/elaboration-display rule that generated derived members keep their declared signatures visible.
  - shape: if a diagnostic, dump, or explanatory rendering shows compiler-generated member definitions from `derive`, the rendered form should present each member's signature separately from any coercion-heavy or normalization-heavy implementation term, so the user can see which generated declaration failed without reverse-engineering a large RHS annotation
  - motivation: GHC issue `#17899` shows that once generated deriving code is surfaced to users, signature-less coercion-heavy renderings quickly become harder to read and produce worse downstream diagnostics than a signature-first presentation
  - related spec: §12.5, §17.2.4, §17.2.5

- Add a worked example for overload resolution between ordinary and `inout` parameters.
  - shape: show that an ordinary call such as `f(x)` does not consider an `inout`-only overload, while `f ~x` is valid only when overload resolution already determines an `inout`-compatible callee
  - motivation: Hylo-style `inout` overload bugs are a good sign that the current prose would benefit from one concrete example
  - related spec: §8.8.3, §7.1.3

- Add a worked example showing that mutable `var` locals do not participate in stable-alias flow typing.
  - motivation: Kotlin smart-cast issue families such as `KT-774` repeatedly rely on mutable-local flow analysis; Kappa intentionally keeps refinement syntax-directed, but one explicit counterexample would make that boundary much clearer
  - related spec: §7.4.3, §8.5.1

- Add a worked example clarifying what failure-side `LacksCtor` evidence implies for finite constructor types.
  - shape: show whether `if x is None then d else ...` on an `Option`-like type is enough to treat the `else` branch as the remaining constructor case for projection or payload binding
  - motivation: the current prose is much clearer on success-side `HasCtor` projection than on residual negative-constructor narrowing
  - related spec: §7.4.1, §17.3.1.8

### Effects, handlers, resumptions, and finalization

### Optimization and specialization stability

- If Kappa later broadens expected-type-directed suspension insertion beyond its current explicit sites, require the insertion point to wait for ordinary inference and constraint solving.
  - shape: any future elaboration rule that inserts `thunk` / `lazy` implicitly outside the already-specified field/binder-directed cases should first determine the unsuspended inner type and ordinary implicit/constraint solutions, instead of eagerly freezing a `Thunk` / `Need` mismatch before the surrounding term has finished elaborating
  - motivation: Idris2 issue `#395` shows that laziness/suspension coercions become brittle when they fire too early in the elaboration pipeline
  - related spec: §5.2.1, §7.1.3, §14.3

### FFI, dynamic values, and bridge boundaries

- If Kappa later standardizes inline foreign declarations or backend-specific source stubs, specify whether descriptor arguments are literal-only or may be imported compile-time string helpers.
  - shape: any source surface that accepts a foreign entry descriptor, calling-convention string, or backend-specific code stub should state whether the argument must be a raw literal, may be a module-qualified compile-time constant/helper application, or may be any admitted compile-time expression, and imported helpers should elaborate equivalently to local helpers when they are permitted
  - motivation: Idris2 issue `#800` shows how brittle a foreign-declaration surface becomes if equivalent compile-time string builders work only when defined locally rather than when imported from another module
  - related spec: §17.7.2, §17.12, §17.1.9

- Consider a backend library-artifact generation mode distinct from executable generation.
  - shape: allow selected backends to emit a loadable/library artifact for an exported module or declaration set without requiring a source-level `main`, and specify how exported symbol identities, entrypoint expectations, and any explicit unmangled export surface are recorded in interface/build artifacts
  - motivation: Idris2 issue `#475` shows the practical gap between “compile a program” and “compile a backend-consumable library”; today users reasonably try `-o` for the latter and hit an unnecessary `main` requirement
  - related spec: §17.1, §17.1.9, §17.7.2, §17.7.4A

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

- Consider standard trusted-summary or adapter patterns for common JVM host collections.
  - shape: define whether a `host.jvm` binding summary may or should adapt surfaces such as `java.util.Iterable`, `Collection`, `List`, `Set`, `Map`, and `Iterator` into portable Kappa iteration/collection interfaces, and if so, pin down mutability, nullability, exception, and encounter-order obligations rather than leaving every bridge to invent its own mapping
  - motivation: Kotlin issues `KT-924` and `KT-1015` show how quickly a JVM language feels incomplete or incoherent if commonplace `java.util` collections do not flow cleanly into the language's ordinary collection abstractions; Kappa already has explicit host bindings and trusted summaries, so the missing question is the standard adapter story rather than whether ambient Java subtyping should leak into source typing
  - related spec: §10.3, §10.6, §17.7, §17.9

- Consider explicit managed-host array projection rules for JVM interop.
  - shape: specify how `host.jvm` should project Java array types into Kappa surfaces, including when argument positions become read-only/covariant views, when result positions remain invariant concrete arrays, how nullability is represented, and whether trusted summaries may substitute safer projected facades for raw host arrays
  - motivation: Kotlin issue `KT-937` shows that array interop becomes incoherent if Java array variance is translated ad hoc; Kappa already has explicit `Array` and `SizedArray` surface plus managed-host bindings, so the missing piece is the standard projection rule for host arrays rather than a general source-level variance feature
  - related spec: §2.6, §17.7, §17.9, §17.10

- Clarify managed-host nominal identity and nullability normalization for JVM bindings.
  - shape: specify when a raw `host.jvm` type such as `java.lang.String`, boxed numerics, or collection/member return types stays as a distinct host nominal, when it is normalized to a portable prelude type or `Option`-wrapped portable type, and how trusted summaries may strengthen or preserve host identity without introducing alias confusion
  - motivation: Kotlin issues `KT-987` and `KT-989` show two sides of the same JVM interop gap: accidental eager unboxing on nullable host returns and inconsistent treatment of `java.lang.String` versus language-level `String`
  - related spec: §17.7.3, §17.7.4, §17.9.1

- Clarify how `host.jvm` projects nested and non-static inner classes.
  - shape: specify how raw JVM bindings represent static nested classes versus inner classes that capture an outer instance, including how generic signatures, constructor surfaces, and outer-instance requirements appear in generated `host.jvm` modules or trusted overlays
  - motivation: Kotlin issue `KT-1008` shows that managed-host interop becomes incoherent if nested and inner class shapes are translated ad hoc, especially when generic inner classes behave differently across Java and Kotlin views
  - related spec: §17.7.2, §17.7.3, §17.9.1

### Runtime, fibers, and resource coordination

- Consider a standard collection-oriented fiber combinator such as `awaitAll`, `joinAll`, or `collectAll`.
  - shape: specify ordering of results, whether all fibers are awaited after the first failure, how interruption of siblings is handled, and how `Cause.Both` / `Cause.Then` composition records multiple failures and cleanup failures
  - motivation: Encore issue `#802` shows recurring demand for synchronizing a collection of futures without serial blocking loops; Kappa already specifies `await`, `join`, `race`, and mandatory finalizer waits, but not a portable all-of combinator
  - related spec: §8.1.3, §8.1.3B, §14.8.4C

- Consider a backend-conformance hook for runtime initialization that mutates ambient host-process state.
  - shape: specify whether runtime startup may alter stdio blocking flags, signal handlers, thread attributes, or equivalent host state, and how such changes must be restored or documented
  - motivation: Pony issue `#205` shows that scheduler/blocking implementation choices can leak outside the program as ambient host-state mutation
  - related spec: §17.5, §17.13, §17.14

- Consider a standard scoped builder surface for linear arrays, buffers, or similar owned mutable resources.
  - shape: if the portable surface later exposes constructors that allocate a linear container or resource and initialize it through user code, prefer an API that returns the owned value only via `Res`, a scoped continuation, or another explicit resource-threading form, rather than an unrestricted callback result that can accidentally smuggle the linear resource out of the intended initialization scope
  - motivation: Idris2 issue `#613` shows that "initialize through a callback" APIs become unsound or misleading when the callback can simply return the supposedly scoped linear array itself
  - related spec: §5.1.5, §5.1.6, §7.2.1, §14.1.1

- Consider a telescopic multi-scrutinee `with`-abstraction surface.
  - shape: if Kappa later adds `with`-style helper-lowering sugar beyond explicit nested `match`, allow one source step to abstract over several linked intermediate expressions or witnesses at once, while preserving the same branch-local refinement, normalization, and totality facts as the equivalent explicit nested matches
  - motivation: Idris2 issue `#622` shows that single-scrutinee helper lowering becomes awkward once one clause needs several mutually determined witnesses or intermediate reductions, Idris2 issue `#637` is a reminder that any such sugar should also have one context-independent layout rule instead of indentation-sensitive corner cases that change between top-level and nested scopes, and Idris2 issue `#751` is a reminder that later case-splitting on the original indexed scrutinee must behave the same way as in the equivalent explicit nested matches
  - related spec: §7.5, §14.3A, §17.3.1.4

### Totality, local recursion, and elaboration-preserved decrease evidence

- If Kappa later adds forced or inaccessible pattern syntax, saturation checks should apply only to discriminating positions.
  - shape: a future forced/inaccessible pattern form that serves only to record an already-determined constructor shape or type-level head should not require constructor or type-constructor applications there to be fully saturated in the same way as an ordinary discriminating value pattern; saturation validation should follow the semantic role of the pattern position rather than blindly reusing the ordinary pattern checker
  - motivation: Idris2 issue `#762` shows that inaccessible-pattern machinery becomes brittle when forced positions are checked as though they performed ordinary constructor discrimination
  - related spec: §7.5.1A, §7.5.1B, §17.3.6

- Clarify that recursive transparent family reduction and equality transport preserve instantiated branch-local equalities through inductive proofs.
  - shape: add a worked proof over a recursively defined family such as list append / membership showing that an induction hypothesis proved under one instantiated family equation can be re-used after recursive reduction and equality transport without losing the already-fixed indices
  - motivation: Idris2 issue `#215` shows that recursive-family elaboration can regress even when the underlying proof goal is straightforward; the spec should say more clearly what normalization and transport are expected to preserve
  - related spec: §5.6.1, §6.4.1, §14.3, §14.3A

- If Kappa later standardizes source-level checking-policy directives or local policy overrides, scope them lexically and make nested elaboration contexts observe them.
  - shape: a file-local default such as a totality mode, warning policy, trace flag, or similar checking/elaboration directive should affect only subsequent declarations in lexical order unless the feature explicitly says otherwise; a local `... in ...` override or equivalent should also apply inside nested tactic, macro, or elaboration sublanguages within that lexical region rather than being silently ignored there
  - motivation: Idris2 issue `#361` shows how surprising retroactive whole-file defaults become once users mix different checking policies in one file, and Lean 4 issue `#366` shows that local option scopes become unreliable if nested tactic/elaboration modes ignore them
  - related spec: §6.4, §17.1, §17.2

- Add a worked example showing that branch-local constructor refinement can expose residual witnesses from transparent family indices.
  - shape: when a constructor result refines an indexed family through an equation such as `prefix ++ rest`, the refined branch should preserve access to the residual `rest` witness, whether via explicit branch-local equalities, an opened existential witness, or another specified source-level mechanism
  - motivation: Idris2 issue `#253` shows how easy it is for elaboration to lose the "remaining suffix" witness even when constructor choice and transparent family normalization should determine it in the branch
  - related spec: §5.6.1, §7.5.1A, §14.3, §14.3A

- Add a worked example clarifying coverage over transparent dependent packages / witness-carrying records with finite witness families.
  - shape: when a payload is packaged together with a witness family that restricts the payload shape to a finite set of cases, coverage checking should account for that witness family instead of inventing impossible residual payload shapes that contradict the witness
  - motivation: Idris2 issue `#430` shows that dependent-package coverage can regress even when the witness family has only a small finite constructor space and every real case is already covered
  - related spec: §5.5.9, §5.5.11, §7.5.1A, §17.3.6

- If Kappa later adds clause-head refutable pattern sugar for named function definitions, require its linearity accounting to match the equivalent explicit `match`.
  - shape: a future surface that allows named functions to be written by several literal/constructor-pattern equations should consume linear scrutinees and wildcard/default branches exactly as an explicit `match` over the same arguments would, rather than imposing stricter or looser use-count rules merely because the patterns were written in the function head
  - motivation: Idris2 issue `#440` shows that clause-head sugar and explicit `case`/`match` can drift apart in surprising ways once linear arguments participate
  - related spec: §5.1.5, §7.5, §7.6



### Local declarations and captured generic context

- Add a worked example where a block-local declaration captures an outer generic or compile-time parameter.
  - motivation: Hylo-style nested-generic failures are a good sign that §14.1.1 needs at least one concrete example involving more than ordinary term capture
  - related spec: §6.3.1, §14.1.1

- Clarify that an explicitly ascribed block-local helper may solve hidden arguments from its declared dependent codomain after transparent normalization.
  - shape: when a local declaration carries an explicit dependent function type and its rhs is checked against that type, elaboration should propagate the declared codomain through local lambdas and helper applications strongly enough to solve straightforward hidden arguments or proof obligations that become definitionally equal after transparent normalization, instead of getting stuck on fresh metavariables whose solutions are already forced by the ascription
  - motivation: Idris2 issue `#798` shows how block-local dependent helpers become brittle if the checker does not reuse the declared codomain to instantiate obviously equal hidden arguments inside the helper body
  - related spec: §6.3.1, §7.3.3, §14.1.1, §14.3

- If Kappa later adds trailing `where`-style local-definition sugar for named declarations or clause groups, require helper scope across the whole declaration group.
  - shape: a helper introduced by a future `where`-style local-definition surface should be in scope for the whole attached definition or equation group according to one explicit rule, rather than only for the nearest preceding clause or rhs
  - motivation: Lean 4 issue `#191` shows that `where`-style sugar becomes unreliable quickly if helper scope does not match the user's natural whole-definition reading
  - related spec: §6.3.1, §14.1.1

- If Kappa later standardizes nested type/data declarations inside local scopes, require them to establish scope deterministically before sibling local terms elaborate.
  - shape: a block-local or clause-local type/data declaration should be available to later local signatures, matches, and helper terms in the same lexical group according to one explicit ordering rule, and failures should blame that rule directly rather than surfacing an unrelated downstream unification mismatch from a stale outer binding
  - motivation: Idris2 issue `#588` shows how quickly local declaration scopes become confusing when a nested type name is not visible where users expect, and the resulting error leaks as an unrelated mismatch
  - related spec: §6.3.1, §11.1, §17.2.4

### Idris-inspired tooling and proof-search requirements

- Add a semantic-highlighting conformance requirement that standardized editor/semantic-query surfaces classify repeated symbol occurrences consistently.
  - shape: if Kappa exposes semantic-token, highlighting, or symbol-occurrence queries, then a declaration head, recursive occurrences in later clauses, and ordinary body references to the same resolved symbol should be reported under one coherent classification policy instead of silently dropping some occurrences because they happen to appear in a clause body or recursively referenced tail position
  - motivation: Idris2 issue `#769` shows how editor integrations become misleading when one occurrence of a function name is recognized but the same resolved symbol in a later clause body is omitted from the semantic report
  - related spec: §2.8.1, §17.2.5

- Consider a controlled proof-search hint surface.
  - shape: explicit declarations, modifiers, or library hooks that contribute values to hole search / proof search under clear lexical or module-level scoping and ordinary visibility/export rules, rather than relying only on ordinary implicit resolution
  - motivation: Idris2 issues `#69`, `#71`, and `#738` show recurring demand for user-directed search inventories, but also the need for those inventories to respect lexical scope and private visibility instead of leaking globally
  - related spec: §7.3.3, §17.2.5

- Consider a normalization-before-search rule for ordinary implicit proof goals over transparent total computations.
  - shape: before rejecting a non-trait implicit goal as unsolved, optionally permit one portable normalization pass under the current branch-local refinement context over transparent total definitions, so goals such as `IsJust (parse s)` can match in-scope evidence for their reduced form when that computation is definitionally determined
  - motivation: Idris2 issue `#606` shows a real expectation gap between "implicit search sees only the surface goal" and "implicit search sees the same reduced proposition users can inspect by ordinary total evaluation"; Kappa currently specifies only the narrower boolean/equality special cases explicitly
  - related spec: §7.3.3, §14.3, §14.3A

- Consider whether `Elab` should eventually expose a controlled recovery / backtracking surface.
  - shape: if Kappa grows richer elaboration-time automation APIs, allow macros or `Elab` helpers to try a query, inspect structured failure, and continue deterministically without converting every failed branch into a hard elaboration abort
  - motivation: Idris2 issue `#544` shows that user-directed search and proof automation quickly hit a ceiling when every elaboration-time failure is terminal instead of being recoverable under explicit control
  - related spec: §5.8.5, §5.8.7, §17.2.5

- Add an elaboration-environment stability requirement for `Elab` declarations.
  - shape: if an `Elab` action injects declarations into the current module or local elaboration environment, those declarations should affect later lookup exactly as specified by the resulting environment extension, without perturbing unrelated surrounding type inference, instance solving, or monad inference in already-well-scoped code
  - motivation: Idris2 issue `#627` shows that declaration injection inside reflection can otherwise poison later inference in ways that do not correspond to any ordinary explicit source rewrite
  - related spec: §5.8.5, §6.4.1, §17.2.5

- If Kappa later standardizes equational-reasoning / preorder-reasoning proof combinators, require them to be definitionally sound wrappers over ordinary equality evidence.
  - shape: any standard reasoning DSL, chain combinator, or proof-builder helper that claims to produce `x = y`, `x <= y`, or another proof-relevant proposition must elaborate to ordinary trusted primitive proof rules without accepting impossible endpoints merely because an intermediate chain node is reflexive or omitted
  - motivation: Idris2 issue `#536` shows that proof-oriented library DSLs can silently reintroduce unsoundness even when the core equality type itself remains sound
  - related spec: §5.6.1, §14.3, §17.1

- Add a tooling conformance requirement that hole-goal and semantic-query services work uniformly for private and nested definitions.
  - shape: generated proof/program edits and constructor-preserving case-split output are now covered by the P0 tooling patch; the remaining gap is that hole-goal and semantic-query services should see the same local/private environment that batch checking sees, including nested namespaces and non-exported declarations
  - motivation: Idris2 issue `#74` shows that interactive tooling becomes misleading when private nested definitions cannot be analyzed even though they typecheck normally
  - related spec: §2.5.1, §17.2.5

- Add a tooling conformance requirement that hole search and finite-inhabitant suggestions be bounded or streamed.
  - shape: expression search, completion, and finite-inhabitant suggestion queries should be allowed to return a deterministic prefix of results, preferably smallest/cheapest inhabitants first, without eagerly materializing the whole search space in memory
  - motivation: Idris2 issue `#228` shows that naive eager enumeration can turn a useful hole-search feature into an immediate memory exhaustion path
  - related spec: §7.3.2, §17.2.5

- Consider a split configuration for readonly toolchain prefixes and writable user package roots.
  - shape: if Kappa standardizes package-installation flags or environment variables, distinguish the compiler/toolchain resource prefix from user-writable package/cache/install roots so immutable installations can still locate bundled standard libraries while installing user packages elsewhere without path spoofing
  - motivation: Idris2 issue `#737` shows that one overloaded prefix often forces awkward launcher hacks once the compiler is installed in an immutable store
  - related spec: §2.3, §15, §17.2.12

- Clarify external build-unit identity versus user-facing package naming.
  - shape: if Kappa interfaces with external build systems, specify which identities are semantic/unit keys used for interface hashing and dependency resolution versus which names are merely human-facing display labels, and avoid constraining external build graphs to one package-name character set when that restriction is not semantically required
  - motivation: GHC issue `#21170` shows that large external build systems quickly run into friction when compiler-facing unit identity is accidentally coupled to one ecosystem's package-name grammar
  - related spec: §2.3, §15.1, §17.1, §17.4

- Add a documentation-attachment rule for doc comments around declaration modifiers.
  - shape: if Kappa standardizes doc comments, attachment must be deterministic across visibility/export modifiers and declaration heads, so documentation placed immediately before or immediately after a modifier block attaches to the same declaration uniformly for functions, type constructors, and nested constructors instead of depending on declaration kind
  - motivation: Idris2 issue `#468` and GHC issue `#21269` show that documentation-mode parsing becomes brittle when modifier-adjacent comments or trailing doc-comment forms are not handled by one deterministic attachment/parsing rule
  - related spec: §2.5, §3.4, §17.2.5

- Add a merge-precedence rule for user-written versus generated documentation.
  - shape: if macros, derives, generators, or quote/elaboration hooks can attach documentation to declarations, the language or tooling contract should say whether user-written docs override generated docs, generated docs override user-written docs, or both are preserved in a stable order, so documentation output does not depend on unspecified merge behavior
  - motivation: GHC issue `#21220` shows that generated-documentation support quickly needs an explicit precedence/merge rule once both user code and metaprogramming can annotate the same declaration
  - related spec: §2.5, §5.8.4, §17.2.5

- Add a minimal documented source-comment surface.
  - shape: if Kappa standardizes doc comments separately from ordinary comments, the user-facing docs/spec should state the ordinary line-comment spelling and the doc-comment spelling together in one obvious place, rather than leaving users to infer them from examples or implementation source
  - motivation: Idris2 issue `#472` is simple but real evidence that even basic comment syntax becomes discoverability debt when the documentation never states it plainly
  - related spec: §3.1, §17.2.5

### Macro hygiene across module boundaries

- Add a worked example where macro-generated names are exported from one module and imported from another.
  - motivation: hygiene problems often appear only at the module boundary
  - related spec: §2.3, §5.8.4

- Add a worked example where expanded generated structure or record code refers to dependent names and must still resolve hygienically.
  - motivation: Lean-style failures in expanded dependent structure code are good evidence that this deserves an explicit example
  - related spec: §5.8.4, §5.8.7

- If Kappa later standardizes source-level rewrite or optimization rules, require left-hand-side binders and explicit implicit instantiations to scope canonically into the replacement template.
  - shape: a rule surface over normalized terms should make every left-hand-side binder available on the right-hand side, including binders introduced by nested subterms and variables fixed by explicit implicit arguments, and the rule matcher should unify those variables canonically enough that the replacement template does not depend on internal metavariable naming accidents
  - motivation: Idris2 issues `#397` and `#398` show that user-directed transform facilities become unreliable if nested binders disappear from scope or if obviously determined implicit instantiations fail to line up between the left- and right-hand sides
  - related spec: §5.8.5, §5.8.6, §17.4

- If Kappa later standardizes source-level `rewrite`-by-equality sugar, require rewrite progress to be judged after ordinary normalization.
  - shape: a rewrite step that replaces one side of an equality proof inside a goal should be checked against the goal after the same transparent normalization and definitional-equality simplification that ordinary branch checking uses, rather than requiring the pre-rewrite surface type to change syntactically before the rewrite is accepted
  - motivation: Idris2 issue `#776` shows how rewrite-oriented proof sugar becomes misleading if the implementation rejects a rewrite with "did not change type" even though the post-normalization goal does change in the semantically relevant way
  - related spec: §5.6.1, §14.3, §17.2.5

## Candidate Ergonomic Improvements

These are not necessarily v0.1 requirements, but they are worth tracking while importing issue families.

- Consider whether `?.` should eventually gain a less annotation-heavy story in fully generic code.
  - current v0.1 design is explicit and coherent, but Kotlin-style use cases such as `KT-600`, `KT-636`, `KT-742`, `KT-743`, `KT-750`, and `KT-948` may produce recurring friction around generic optional unwrapping helpers, expected result types, primitive-like results, inference after explicit unwraps, and whether portable receiver-oriented `Option` helper functions should exist in the standard library
  - related spec: §7.1.1.2

- Consider specifying a portable IEEE floating-constants surface.
  - shape: standard names or constructors for `NaN`, positive/negative infinity, machine epsilon, and any companion
    predicates or classification helpers that are intended to be portable for `Float` / `Double`
  - motivation: Idris2 issue `#29` shows that defining IEEE-754 semantics alone is not enough; users also need a
    portable way to name the important exceptional floating values and precision constants
  - related spec: §4.1.3, §17.1

- Clarify whether portable `Float` / `Double` arithmetic requires binary64 rounding at every operation boundary.
  - shape: specify whether each portable arithmetic operation on `Float` / `Double` must truncate/round to IEEE-754 binary64 before any subsequent dependent use, equality test, or branch refinement observes the result, even on hosts with wider intermediates or optimizer-driven excess precision, and treat any relaxed-performance alternative as explicitly non-portable
  - motivation: Idris2 issue `#623` shows that dependent typing and raw-bit equality become backend-fragile if intermediate floating-point results depend on architecture-specific excess precision rather than one portable binary64 semantics
  - related spec: §4.1.3, §5.6.1, §17.1

- Consider a portable integer exponentiation surface.
  - shape: if the standard library exposes `pow`/`power`/`(^)`-style helpers for `Nat`, `Int`, or `Integer`, specify
    which domains are supported, whether the exponent must be non-negative, and how fixed-width overflow behaves versus
    unbounded-integer exponentiation
  - motivation: Idris2 issue `#592` shows that users quickly expect `Integer` exponentiation once ordinary numeric
    literals and arithmetic are in place, but portability depends on making the domain and overflow story explicit
  - related spec: §2.6, §4.1.5, §17.1

- Clarify portable line-reading semantics for console and file APIs.
  - shape: if the standard library exposes `getLine`-style operations for terminals and files, specify whether returned
    strings keep or strip trailing newlines, whether EOF yields `None` / an error / an empty string, and whether the
    console and file variants intentionally differ
  - motivation: Idris2 issues `#195` and `#587` show how quickly line-oriented code becomes non-portable when newline
    stripping and EOF behavior are left implicit or inconsistent across adjacent APIs and backends
  - related spec: §17.1

- Consider a stack-safe string/character sequence conversion surface.
  - shape: if the portable prelude exposes conversions such as `pack`, `unpack`, or adjacent whole-string/list
    adapters, specify them so large inputs are handled with iterative, chunked, or otherwise stack-safe realizations
    instead of leaving recursion depth as a backend accident
  - motivation: Idris2 issue `#596` shows that even simple standard-library conversions can become backend-fragile when
    their lowering depends on unbounded recursive calls
  - related spec: §2.6, §17.1

- Consider a reusable chunked binary file-read surface.
  - shape: if the portable standard library grows beyond whole-file `Bytes` loading, specify whether partial reads return fresh `Bytes`, fill a reusable mutable buffer, or expose both surfaces, and pin down EOF / short-read behavior so large-file processing does not require implementation-specific APIs
  - motivation: Idris2 issue `#209` shows that whole-file reads are not enough for large binary inputs or buffer reuse
  - related spec: §8.8, §17.1

- Consider a stack-safe eager aggregate surface for standard numeric folds such as `sum` and `product`.
  - shape: if the portable prelude exposes eager aggregate helpers over `List` / `Array` / `Foldable`, specify them in a
    way that is observationally equivalent to a strict left fold or another stack-safe implementation strategy, rather
    than leaving stack behavior as an accidental consequence of a right-fold definition
  - motivation: Idris2 issues `#132` and `#447` are good reminders that eager languages and their backends can
    accidentally ship pleasant-looking aggregate or fold surfaces whose lowering becomes stack-fragile even on modest inputs
  - related spec: §2.6, §17.1

- Consider allowing declaration heads to break before their block introducer.
  - shape: permit forms such as `data T : Type` followed by a line-start `=` and indented constructor block, and decide whether the same layout rule should apply to analogous block-introducing declarations such as `trait` and `effect`
  - motivation: Idris2 issue `#234` shows that users naturally expect indentation-sensitive declaration blocks to tolerate a newline before the block introducer instead of forcing every declaration head to keep `=` on the same line
  - related spec: §3.4, §11.1

- Consider a limited declaration-head sort-inference convenience for obviously type-level parameters.
  - shape: if a `data` / `record` / `trait` header binder omits `: Type` but later field/result uses force it to be a type-level parameter unambiguously, either recover that sort deterministically or state explicitly that Kappa rejects the omission in favor of the current explicitness principle
  - motivation: Idris2 issue `#240` shows the ergonomic pressure around repeating `: Type` in wrapper declarations even when later uses already force the classification
  - related spec: §1, §11.1, §11.4

- Consider a constructor-only pattern shorthand as a future ergonomic extension.
  - shape: a compact pattern form that matches a constructor while intentionally discarding all of its fields, as sugar
    for spelling every field position with `_`
  - motivation: Idris2 issue `#61` shows the readability value of saying "this branch only cares that the constructor is
    `Node`" without repeating one wildcard per field; Kappa already has ordinary constructor patterns, so this would be
    pure surface sugar rather than a semantic change
  - related spec: §5.3.3, §7.5

- Consider a future refinement rule that treats successful safe-navigation/elvis boolean conditions as evidence that the receiver is present.
  - shape: conditions like `x?.pred ?: False` would refine `x` in the success branch when the condition is pure and the success path can arise only from the present case
  - motivation: Kotlin issue families such as `KT-2127` and `KT-8492` suggest this would be a meaningful ergonomic improvement, but v0.1 does not currently promise it
  - related spec: §7.1.1.2, §7.1.2, §7.4.1, §7.4.2

- Consider a scoped condition-binding form for introducing a branch-local alias of a tested expression.
  - shape: an `if let`-style or condition-local alias form that binds a name only within the success branch, instead of forcing a wider-scope `let` just to participate in refinement
  - motivation: Kotlin issue `KT-132` is a good example of the friction here: users want a narrow-scope alias for a complex expression so the success branch can reuse it without widening the temporary's scope
  - related spec: §6.3, §7.4.1, §7.4.3

- Consider a future clause-signature binder reuse or alias surface.
  - shape: if Kappa later adds richer clause-style function definitions or signature-directed clause elaboration, decide whether value/index names introduced in the signature are reusable inside matching clauses by default or whether users must spell an explicit alias form, so dependent definitions do not have to restate obvious binder names purely to mention them in the right-hand side
  - motivation: Idris2 issue `#471` shows the recurring ergonomic question around whether type-level or signature-introduced names should automatically scope into function clauses, especially for dependent indices
  - related spec: §5.3.3, §7.5, §14.1.1

- Consider anonymous pattern-lambda / one-clause match-function shorthand.
  - shape: a compact lambda form whose binder is itself a pattern, desugaring to an ordinary lambda plus immediate `match`; if such a surface is added, constructor names and proof constructors inside the pattern must retain their constructor meaning instead of being reinterpreted as fresh binder names
  - motivation: Idris2 issues `#46` and `#677` suggest this is useful enough in dependently typed code that Kappa should at least track it as a plausible future ergonomic surface, and that binder hygiene for names like `refl` must be nailed down explicitly
  - related spec: §7.2.1, §7.5, §7.6

- If Kappa later adds idiom-bracket or equivalent applicative-expression sugar, require tuple/constructor spellings to elaborate uniformly.
  - shape: equivalent applicative expressions such as a tuple/pair literal form and an explicit constructor form must lower through the same `pure`/`<*>` structure up to observational equivalence, rather than one spelling bypassing applicative combination and forcing an unrelated ambiguity path
  - motivation: Idris2 issue `#676` shows how quickly sugar becomes unreliable if one constructor spelling is lowered as an applicative expression while another is treated as an ordinary partially applied term
  - related spec: Appendix G, §7.1.3, §14.1

- Consider a future scoped macro / notation activation surface tied to modules or namespaces.
  - shape: if Kappa later allows macros, notation rewrites, or parser customizations to be attached to a namespace/module scope and activated by `open`, imports, or an explicit scope directive, the activation rules should be lexical and compositional, so local customizations can override built-ins without mutating global parsing behavior for unrelated code; if recursive or mutually defined syntax helpers are permitted, they should resolve under that same scoped activation model rather than requiring a global parser-table mutation during parsing; local activation forms analogous to `open ... in ...` should affect syntax matching and quotation/parsing services consistently within that lexical scope
  - motivation: Lean 4 issues `#249`, `#181`, and `#529` show the practical value of letting users package syntax customizations with a namespace and enable them on demand rather than choosing between globally overriding notation, mutating parser state mid-parse, and fully spelling out every local customization
  - related spec: §2.3, §5.8.2, §5.8.4

- If Kappa later adds parser or notation customizations that introduce new block-like or `|`-delimited forms, require them to obey the core layout rules.
  - shape: any future syntax extension surface with trailing-parser behavior, custom block forms, or alternatives separated by `|` should compose with the ordinary continuation and column-sensitive indentation rules of the core language, so nested uses cannot silently capture later clauses or reinterpret an indented line as continuing a previous expression
  - motivation: Lean 4 issues `#451` and `#452` show that syntax extensions quickly become brittle when they do not inherit the same indentation and match-column rules as the built-in block forms they resemble
  - related spec: §3.4, §5.8.4

- If Kappa later adds syntax abbreviations, alias categories, or other source-level syntax aliases, require them to be expansion-equivalent and terminating.
  - shape: a syntax alias or abbreviation that is documented as equivalent to another term/category form should elaborate, expand, and render observationally equivalently to that target form, without introducing recursive expansion loops or stack-overflow behavior merely because the alias is routed through an intermediate syntax category; if such an alias may be declared in a local lexical context and its expansion body mentions surrounding locals, it should either capture that lexical environment hygienically or be rejected immediately rather than silently dropping the local reference or replacing it with a metavariable-shaped fallback
  - motivation: Lean 4 issues `#255` and `#308` show that syntax-abbreviation surfaces become fragile if the alias form is not treated as a true elaboration-equivalent view of the underlying syntax or if local notation silently loses the lexical environment it was supposed to abbreviate
  - related spec: §5.8.1, §5.8.4

- If Kappa later adds clause-head function patterns or direct function-shape matching, require functions to remain non-discriminable by ordinary pattern coverage.
  - shape: any future surface that lets a clause head or `match` mention a lambda/function-valued pattern must elaborate either to ordinary equality/proof obligations or to an explicit rejected form; it must not treat one concrete lambda body as a constructor-like discriminator that can prove contradictions about another function of the same type
  - motivation: Idris2 issue `#633` shows that once a language starts pattern-matching directly on functions, it can accidentally smuggle unsound extensional/function-identity assumptions into coverage and equality reasoning
  - related spec: §5.6.1, §7.2.1, §7.5, §17.3.6

- Consider a future pattern-alias / as-pattern surface with quantity preservation.
  - shape: if Kappa later adds a syntax that binds the whole scrutinee alongside a refined subpattern, any explicit or implicit binders brought into scope by the matched constructor/result type should retain their ordinary inferred quantity/relevance instead of being silently forced to compile-time-only just because an alias form was used; elaboration should also preserve enough alias structure that pretty-printers, semantic queries, and delaboration of lowered matchers can reconstruct the source-like alias form rather than dropping the whole-pattern binder during rendering
  - motivation: Idris2 issue `#490` shows that alias-pattern sugar can accidentally distort binder-quantity information even when the underlying match introduces no new erased-use reason, and Lean 4 issue `#220` shows that lowered matcher rendering can otherwise lose the alias binder entirely
  - related spec: §5.1.4, §5.1.5, §7.5, §17.3.6

- Consider allowing `_` as an explicit unnamed binder in function and Pi types when the binder name is not referenced.
  - shape: permit types such as `(1 _ : a) -> b` and analogous ordinary / implicit Pi binders to state quantity and domain information without pretending the binder name matters later
  - motivation: Idris2 issues `#175` and `#556` show the readability benefit of marking unused binder names as intentionally unnamed rather than inventing throwaway identifiers only to attach quantity annotations
  - related spec: §5.1.5, §7.1.3, §11.1

- Clarify how quantity checking counts compile-time alias parameters through nested type formers.
  - shape: state whether a compile-time binder such as `(1 A : Type)` or `(0 A : Type)` in a type alias is counted exactly as if its body had been written inline when `A` appears under function/Pi codomains, nested type constructors, transparent aliases, and similar type-level forms
  - motivation: Idris2 issue `#356` shows that quantity checking on compile-time parameters becomes brittle if type aliases or nested type formers silently change whether a use is treated as erased, linear, or otherwise demandful
  - related spec: §5.1.4.1, §5.1.5, §11.3, §14.3

- Clarify that dependent signature elaboration solves straightforward index constraints induced by earlier binders and transparent families.
  - shape: a theorem or helper signature that introduces values such as `xs : Vec x a`, `ys : Vec y a`, and then mentions transparent family applications over those same binders in later types should elaborate with the obvious induced index equations, rather than requiring users to restate redundant size indices only to make metavariables solvable
  - motivation: Idris2 issue `#378` shows how brittle dependent type elaboration becomes if transparent family applications in type signatures do not feed their induced index constraints back into ordinary checking
  - related spec: §5.3.3, §11.3, §14.3, §14.3A

- Clarify that clause simplification and definitional equality may use refinements coming from any matched argument position, not only the first.
  - shape: branch-local normalization and trivial-proof simplification should be allowed to exploit constructor/index refinements introduced by any matched argument in a clause, not just the leftmost scrutinized argument
  - motivation: Idris2 issue `#173` adds more evidence that users expect definitional simplification to follow all established branch facts, including those from later arguments
  - related spec: §7.5, §14.3, §17.3.1

- Add a worked example showing that transparent type aliases preserve outer implicit/constraint structure when expanded.
  - shape: if a type alias body contains an implicit binder, trait constraint, or effect-related wrapper, using the alias should elaborate exactly as using the expanded body rather than silently dropping or reordering that structure
  - motivation: GHC issues `#821` and `#981` are reminders that alias expansion around implicit constraints is easy for implementations and users to misunderstand, especially once ordinary application sugar is involved, even when the intended semantics are just ordinary expansion
  - related spec: §2.5.2, §6.3, §7.3.3, §14.3

- Clarify that any future compile-time branching or classification over type objects treats built-in and user-defined type constructors uniformly.
  - shape: if Kappa exposes transparent compile-time functions that inspect or branch on `Type`-valued arguments, reduction should not special-case builtin type constructors such as `Int`, `String`, or `Type` itself relative to ordinary user-defined datatypes unless the surface explicitly says so
  - motivation: Idris2 issue `#650` shows that definitional reduction over type classifiers becomes unpredictable if built-in type constructors fail to participate in the same reduction story as ordinary constructors
  - related spec: §14.3, §17.3.1, §24.3

- Add a worked example for constructor-local implicit evidence flowing into an indexed-pattern branch.
  - shape: when an indexed or GADT-style constructor carries local trait/constraint premises, matching that constructor should make the corresponding evidence available only inside the reachable branch, alongside the usual index refinement facts
  - motivation: GHC issue `#810` shows that users reasonably expect branch-local evidence from constructor choice, but the current Kappa text is much more explicit about index refinement than about the branch-local evidence side
  - related spec: §7.5.1, §7.5.1A, §12.3, §17.3.1

- Add a worked example showing that ordinary record updates typecheck through associated-type normalization.
  - shape: if a record-typed field is reached through an associated type or associated member instantiation that normalizes to a concrete record type, an update such as `x.{ field = ... }` should be explained using the same ordinary full-record reconstruction rules rather than appearing to require a manual ascription or normalization step
  - motivation: GHC issue `#1204` shows that record update through associated-type instantiation is a common place where implementations and users lose track of the intended normalized field type
  - related spec: §5.5.5, §12.5, §14.3

- Add a worked example stating that ordinary record update agrees with explicit destructure-and-rebuild in the supported same-type fragment.
  - shape: for updates that remain within Kappa's current same-record-type fragment, show that `r.{ f = v }` typechecks exactly when the equivalent explicit pattern/destructure-and-rebuild form does, even when omitted fields mention non-injective transparent families or other compile-time structure that requires ordinary normalization to see the agreement
  - motivation: GHC issue `#21158` shows that users naturally expect record update and explicit reconstruction to coincide on supported cases, and become confused when an implementation gives them different typing behavior merely because one path exposes the copied fields more directly
  - related spec: §5.5.5, §14.3, §17.3.1

- Consider whether ordinary record update should admit type-changing or existential-payload reconstruction when manual destructure-and-rebuild would be valid.
  - shape: if all omitted copied fields remain definitionally valid after substituting the updated fields, allow `r.{ field = ... }` to elaborate through an explicit reconstructive/desugared form that yields a different instantiation of the same record family; if such an extension is ever added, specify the existential-field case the same way, so updating an existential payload succeeds exactly when the rebuilt record can re-establish the required local evidence
  - motivation: GHC issues `#10808`, `#10856`, `#11156`, `#16501`, and `#18311` point at the same ergonomic gap: users expect record update to match manual destructure-and-rebuild when only type indices, constructor-refined catch-all cases, or existential payloads change, but Kappa currently specifies same-type full-record reconstruction only. GHC issues `#18802` and `#18809` are further evidence that a desugaring-first formulation is easier to reason about than ad hoc update-specific typing rules, especially once GADT/data-family refinements participate
  - related spec: §5.5.5, §5.5.11, §14.3, §17.3.1

- Add a worked example showing that trait-instance resolution matches normalized associated-member goals, including through transparent aliases.
  - shape: if a goal such as `Render basis.Out` or an alias expanding to `basis.Out -> ...` normalizes to an ordinary instance head like `Render String`, instance search should pick that instance without requiring the user to manually expand the associated member or avoid an alias-shaped signature
  - motivation: GHC issues `#1834` and `#3038` show that users reasonably expect associated-type normalization to be visible not just to ordinary typechecking but also to constraint solving and simple alias-preserving reuse
  - related spec: §7.3.3, §12.2.1, §12.3.1, §14.3

- Add an elaboration-equivalence rule for direct terms versus trivial explicit-proof wrappers.
  - shape: if a term `e` and an observationally trivial wrapper such as `by exact e`, an immediately discharged local block, or an equivalent no-new-evidence ascription expose the same source information, ordinary elaboration should not succeed for one form but fail for the other merely because one path delays unification, transparent reduction, or instance solving longer than the other; if Kappa intentionally keeps a distinction here, that distinction should be stated explicitly
  - motivation: Lean 4 issue `#350` shows that users reasonably expect the plain term and a trivial `exact` wrapper to have the same acceptance when the wrapper introduces no genuinely new evidence
  - related spec: §7.1.3, §7.3.3, §14.3, §17.3.1

- Add an instance-search propagation rule for solved prerequisites that determine later goals.
  - shape: when ordinary instance resolution solves an earlier prerequisite and that solution instantiates out-parameters, universe levels, equality witnesses, or default-instance targets needed by later subgoals, those instantiations should be propagated before later instance-search or projection checks are rejected; users should not need to restate the intermediate solved fact merely to make the remaining goal visible to the same elaboration pass
  - motivation: Lean 4 issues `#351`, `#352`, and `#355` show the same failure mode in three guises: field projection rejected before a default instance exposes the real carrier, universe-constrained instance search reported as an opaque failure instead of solving the forced level equations, and later instance goals left unsolved even though an earlier resolved instance determines the remaining unification problem
  - related spec: §7.3.3, §14.3, §17.3.1

- Consider whether numeric-literal defaulting should propagate through explicitly typed overloaded aliases when later use leaves only a defaultable numeric ambiguity.
  - shape: decide whether a binding such as `x : FromInteger a => Negatable a => a; x = -1` should be instantiated by the ordinary numeric default when a later use site still leaves `a` ambiguous except for defaultable numeric classes, or whether defaulting is intentionally limited to literal elaboration sites
  - motivation: Idris2 issue `#179` exposes a real user expectation gap around whether defaultable numeric ambiguity may survive through an overloaded top-level alias and then be discharged at use
  - related spec: §4.1.5, §7.1.3, §7.3.3

- Consider an explicit irrelevance-only definition surface.
  - shape: a way to declare a definition or parameter block as compile-time/proof-only even when the body is not merely an ordinary quantity-`0` binder occurrence, with clear erasure and non-runtime-use rules
  - motivation: Idris2 issue `#55` highlights a user need that is stronger than Kappa's current "quantity `0` inside otherwise ordinary code" story
  - related spec: §5.1.4, §5.1.5, §14.4

- Consider standard constructor-injectivity proof helpers with erased arguments.
  - shape: if the portable prelude or proof library eventually exposes helpers such as constructor injectivity or
    no-confusion lemmas, the constructor arguments that appear only in the result proof should be quantity `0` by
    default, so these helpers remain usable in erased proof contexts without forcing runtime relevance
  - motivation: Idris2 issue `#597` shows that seemingly harmless proof helpers become awkward if they keep runtime
    access to constructor arguments that are needed only for compile-time equality reasoning
  - related spec: §5.1.4, §5.6.1, §14.4, §17.1

- Consider named instance-selection / local search-space control.
  - shape: a disciplined way to choose one implementation or dictionary explicitly at a use site or within a local scope, without opening global incoherence
  - motivation: Idris2 issue `#57` shows that users sometimes want more control than plain implicit resolution and global instance search provide
  - related spec: §7.3.3, §12.3, §15.2.1

- Add an instance-resolution performance/conformance expectation for repeated structural obligations.
  - shape: trait/instance resolution over structurally recursive evidence goals should share or otherwise avoid pathological duplicate work on repeated subgoals, so ordinary uses of derived comparison/equality dictionaries over nested heterogeneous structures do not become exponentially slower merely because one method delegates through another trait layer
  - motivation: Idris2 issue `#783` shows that two extensionally similar dictionary constructions can differ by orders of magnitude in elaboration time unless the instance-resolution story says something about repeated structural goals and obvious shared subproblems
  - related spec: §12.3.1, §12.3.2, §17.1

- Consider explicit evidence-binder / telescopic trait headers.
  - shape: if Kappa later wants certified wrappers around existing boolean-valued traits, permit trait headers whose superclass/evidence binders are named and may appear in later trait indices or associated proof parameters, rather than restricting trait headers to unnamed superclass constraints only
  - motivation: Idris2 issues `#777` and `#778` show a real expressiveness gap for "certified `Eq`/`Ord`" style interfaces once the trait being declared needs to index itself by the chosen witness for an earlier superclass/evidence premise, and once proofs inside the certified wrapper need hypotheses mentioning default boolean-valued members to normalize against that chosen witness
  - related spec: §12.1, §12.1.1

- Add a cross-module constructor-identity requirement for pattern resolution.
  - shape: when constructors, traits, records, or similar pattern heads are forward-declared, re-exported, or defined across split modules/namespaces, later pattern matching must still resolve them as constructors/type heads under the ordinary namespace rules, rather than misclassifying one imported spelling as a linear local term or another non-pattern entity
  - motivation: Idris2 issue `#779` shows that pattern resolution becomes brittle if cross-module forward declarations or re-exports can make one constructor spelling behave like a non-linear/local binding error instead of the same constructor identity users see in the defining modules
  - related spec: §2.3, §2.8.1, §7.5, §11.1.1

- Consider optional/default implicit arguments as a library-surface ergonomics feature.
  - shape: allow a declaration to provide a fallback value for an implicit parameter so ordinary callers may omit tuning/configuration arguments while advanced callers still override them explicitly or lexically, and make the explicit-application surface behave observably differently from omission/defaulting when users request it
  - motivation: GHC issue `#8297`, Idris2 issue `#371`, and Lean 4 issue `#111` are good reminders that some implicit parameters are really optional configuration knobs rather than mandatory context, and that once defaults exist the language must still preserve a coherent explicit-override path
  - related spec: §7.3, §7.3.3

- Consider a future composition-helper or composition-operator surface that preserves eta-equivalent behavior for erased or irrelevant inputs.
  - shape: if Kappa later standardizes function-composition helpers or operators, an eta-expanded term such as `\x => absurd (f x)` and the composed form should type the same even when `f` consumes an erased, irrelevant, or uninhabited input, rather than the helper imposing a stricter relevance discipline than ordinary lambda elaboration
  - motivation: Idris2 issues `#461`, `#539`, and `#664` show that composition surfaces can accidentally become less expressive than their eta-expanded equivalents once quantity, accessibility, wrapper records, or dependent proof fields participate
  - related spec: §5.1.4, §5.1.5, §7.1.3

- If Kappa later standardizes literate-source formats, require the visible/invisible code classification to be explicit and semantically binding.
  - shape: each supported literate format should say exactly which lines/blocks are executable code, which are comments/documentation only, and how those classes interact with module discovery, entrypoint lookup, and generated diagnostics, so "invisible" code never executes merely because one parser path disagrees with the docs
  - motivation: Idris2 issues `#542`, `#785`, and `#788` show that literate-source support becomes untrustworthy if the documented visibility categories do not match what the compiler actually executes, if interactive tooling such as case-split/add-clause commands no longer tracks the visible code layout that batch checking accepts, or if one literate marker family is silently reinterpreted from comment text into executable code
  - related spec: §3.1, §17.2.5

- Consider a reusable grouped-constraint alias surface.
  - shape: allow users to name a bundle of several constraints as one reusable `Constraint`-valued alias or equivalent surface form, so the same grouped requirement can appear in trait headers, signatures, and higher-order constraint parameters with ordinary transparent expansion and ordinary cycle checking
  - motivation: GHC issues `#10362`, `#11278`, and `#21209` show that grouped or higher-order constraints are easier to reason about when the language gives them an ordinary reusable representation and lets determinant/ambiguity reasoning see through immediately available transparent wrappers; Kappa already has first-class `Constraint` descriptors and multi-supertrait syntax, but not yet a named portable conjunction surface
  - related spec: §5.1.3, §7.3.1, §12.1, §12.1.1

- Clarify whether future projection/field protocols are governed by exported surface fields or hidden representation fields.
  - shape: if Kappa later standardizes `HasField`-like projection protocols, custom field instances, or generalized dot sugar, specify whether coherence and legality are determined by the exported surface field set or by hidden representation selectors that exist only inside the defining module, so libraries can intentionally expose one field protocol without being blocked by an unexported implementation field
  - motivation: GHC issue `#21324` shows that field-projection protocols become awkward when hidden representation selectors constrain what user-facing instances can exist even though those selectors are not part of the public surface
  - related spec: §2.3, §5.5, §12.3, §15.2.1

- Add a warning-precision rule that record-update exhaustiveness warnings account for the current refined path.
  - shape: when a record update occurs only on a control-flow path that has already refined the scrutinee or bound value to constructors supporting the updated field, the warning checker should use that path-local refinement instead of re-warning as though the update were performed on the unreduced outer type
  - motivation: GHC issue `#21360` shows that record-update warnings become noisy when control-flow desugaring obscures the fact that a preceding pattern match already restricted the value to constructors that do contain the field
  - related spec: §5.5.5, §7.4, §7.5, §17.2.4

- Clarify future projection-syntax field-name compatibility and escaping.
  - shape: if Kappa later expands postfix projection/dot sugar or label-aware projection sections, specify whether every ordinary record field name remains accessible after the projection token, which spellings are reserved by the parser in that position, and whether a quoting/escaping form exists for collisions instead of leaving some field names accepted only in record declarations but not in projection syntax
  - motivation: GHC issue `#21226` shows that projection sugar becomes a latent compatibility hazard if a legal field name in declarations is not always legal after the projection token
  - related spec: §5.5, §7.1.3, §17.2.4

- Consider an explicit empty record-update identity form.
  - shape: decide whether `r.{}` should be permitted as a well-typed no-op record update, especially when a surrounding expected type or compile-time field change is already forcing normalization/rechecking and users want to express "same runtime fields, new static view" without inventing a dummy rewrite
  - motivation: GHC issue `#7658` points at a small but real ergonomics gap around record-preserving retagging and phantom/static view changes
  - related spec: §5.5.5, §14.3

- Consider whether associated static members in trait bodies should admit defaults, and if so, specify their parameter scoping precisely.
  - shape: if Kappa wants parity between default term members and associated static members, allow a default associated static definition in a trait body and state exactly which trait parameters and local universals scope into that default
  - motivation: GHC issues `#9063`, `#9264`, `#23768`, `#10361`, `#10817`, `#10899`, `#11136`, `#13971`, and `#14094` show that default associated-type machinery is easy to get subtly wrong, especially once derivation behavior, termination/cycle checks, RHS well-formedness, arity validation, scoped type/kind parameters, binder-validation rules, diagnostics that blame the real failing side of a malformed default, and warnings for auto-generated instances missing associated members all participate, which is exactly why Kappa should either forbid it explicitly or specify it carefully if added later
  - related spec: §12.2, §12.2.1, §12.3

- Consider how future deriving support should interact with traits that carry associated static members.
  - shape: if Kappa expands `derive` beyond the current implementation-defined `Eq` / `Show`-like / `Hashable` baseline, specify whether a derived instance may synthesize associated static members, when representation-preserving coercion is sufficient, when extra derived premises may be inferred from field or associated-member payloads, when indexed/refined result types let the derivation omit unreachable constructors and respect contextual equalities without generating inaccessible branches, whether some ordinary traversal/bifunctor-style traits are intentionally derivable or intentionally excluded by role/variance rules, whether data-family-style indexed instances receive the same impossibility pruning as closed ordinary datatypes, whether derivation may target unlifted or runtime-representation-indexed payload types when the chosen trait semantics make sense there, how default member signatures and alias-expanded higher-rank member types participate in generated instances, and when users must still write the associated member definitions explicitly; also specify that any richer deriving path still elaborates to a well-formed ordinary instance/associated-member package and therefore cannot bypass ordinary admissibility checks, functional-dependency-like determinism rules if Kappa ever adds them, forbidden-class rules, or rejected superclass/constraint obligations, and that inferred contexts should not be stronger than the weakest explicit instance users could have written
  - motivation: GHC issues `#13404`, `#14462`, `#14661`, `#14728`, `#14916`, `#15052`, `#15868`, `#16341`, `#16714`, `#16958`, `#17013`, `#18474`, `#19141`, `#20314`, `#20375`, `#20387`, `#20501`, and `#20719` show the same cluster of risks: "derive the instance body but not the associated static member or its needed premises" is already an awkward middle ground, and once coercion-based deriving, associated members, less-conservative inferred constraints, indexed impossibility pruning, contextual equality reasoning, data-family indexing, role-sensitive traversal classes, unlifted/runtime-polymorphic payloads, special built-in classes, deprecated-but-still-accepted context forms, deferred `TypeError`-style constraints, default member signatures, alias-expanded higher-rank method types, or extra admissibility/coherence checks participate, the language must say whether generated code is required to correspond to a valid explicit instance users could have written and how much premise inference is intentionally supported
  - related spec: §12.2.1, §12.3, §12.5

- Consider an explicit coercion-based `derive via` surface, and specify its admissibility precisely.
  - shape: if Kappa ever adds a user-facing `derive via`-style form, specify when the chosen `via` type must be fully explicit and well-kinded, whether standalone and inline forms bind kind/type variables identically and elaborate equivalently once they name the same admissible strategy, whether standalone forms permit only the same binder forms as ordinary instance declarations, exactly how explicit derivation strategies are written in source, whether the `via` type admits optional parentheses or annotations in more than one syntactic position, whether naming a strategy requires enabling any separate implicit-defaulting behavior, whether the via/source type may remain at an unreduced but well-formed family/associated-type expression, whether the surface supports only ordinary unary trait heads or also higher-kinded / transformer-shaped trait heads, whether derived code may mention advanced representation-sensitive syntax without requiring the user module to enable unrelated surface extensions, how generated member definitions preserve higher-rank binder scope from source synonyms, how generated member definitions resolve the source trait instance without ambiguous inference, how role/coercibility restrictions, whole-dictionary coercions, method-by-method coercions, runtime-representation equality, and method-local extra premises interact with generated members, and whether ordinary source `coerce` sugar is expected to infer the same coercions that deriving would synthesize automatically; also specify whether all kind information available from the whole declaration participates in checking the derived instance, whether context inference may preserve unsimplified or "exotic" but valid premises from the via/source instance, when rejected constraints are checked, when missing-strategy warnings fire, and whether malformed `via` types or empty derive lists are rejected before any per-trait derivation work begins or optionally accepted as a pure representation-compatibility witness; also specify which traits are representation-preserving enough to permit coercion-based derivation, which metadata-sensitive traits must instead use bespoke derivation or be rejected, and which trusted evidence traits or compiler-reserved witness traits are intentionally non-derivable even if the method types appear coercible
  - motivation: GHC issues `#15178`, `#15191`, `#15376`, `#15434`, `#15831`, `#15839`, `#16179`, `#16362`, `#16641`, `#16923`, `#17014`, `#17183`, `#17312`, `#17767`, `#18047`, `#18130`, `#18219`, `#18258`, `#18271`, `#18483`, `#18488`, `#18914`, `#19079`, `#20223`, `#20524`, `#20538`, `#20815`, `#20821`, and `#21087` show that a `derive via` story becomes subtle immediately: users expect it to behave like an explicit ordinary instance, but implicit quantification, standalone-vs-inline binder scoping and elaboration equivalence, invalid standalone binder forms, overlap/default-method behavior, ambiguity in zero-argument members, unreduced source/via types, higher-kinded target traits, higher-rank binder scope, whole-dictionary versus method-by-method coercion, runtime-representation equality, extension-sensitive generated syntax, extra method constraints, quantified-constraint/role interactions, kind-information propagation, inferred-context simplification, `TypeError`-style prerequisite timing, strategy-syntax/documentation clarity, extension-gating policy, warning-surface design, optional-parenthesis syntax, empty-list corner cases, and trusted singleton-evidence traits that should never be forgeable all need explicit rules
  - related spec: §5.3.3, §12.3, §12.5

- Consider a grouped standalone-deriving surface for several traits sharing one target and one derivation strategy.
  - shape: if Kappa later exposes richer standalone deriving declarations, consider allowing one declaration to derive several traits together when they share the same target type and the same `via`/strategy payload, so users do not duplicate a large `via` specification or accidentally let paired traits drift apart
  - motivation: GHC issue `#18874` points at a real ergonomics/documentation gap once standalone `via` derivations become large: repeating the same standalone clause for `Eq`, `Show`, serialization pairs, or similar trait families is both noisy and easy to make inconsistent
  - related spec: §5.3.3, §12.5

- Consider a future projection-based derive-customization surface for ordinary equality/ordering/hash traits.
  - shape: if Kappa expands `derive` beyond the current baseline, consider whether a user should be able to request a derived `Eq` / `Ord` / `Hashable`-like instance "via" an explicit pure projection or normalization function, so the generated instance compares or hashes a canonical view without requiring a wrapper data type or fully handwritten instance
  - motivation: GHC issue `#17210` highlights a recurring ergonomics gap between completely fixed stock deriving and completely manual instances: users often want "derive by this key/view" rather than a brand-new deriving algorithm
  - related spec: §5.3.3, §12.3, §12.5, §14.1

- Consider specifying the portable `Array` construction and convenience-member surface more fully.
  - shape: the core portable surface is now specified (`arrayEmpty`, `arraySingleton`, `arrayFromList`, `arrayToList`, `arrayLength`, `arrayGet`, `arrayIndex`, and the `Array`/`SizedArray` bridge). The remaining question is whether Kappa should also standardize richer convenience APIs such as mutation/update helpers, membership/sorting/search helpers, range-like views such as `indices` or `lastIndex`, and any multi-index accessor/update forms as portable library surface rather than implementation extras
  - motivation: Kotlin issues `KT-199`, `KT-200`, `KT-326`, `KT-331`, `KT-346`, `KT-347`, `KT-348`, `KT-376`, `KT-417`, `KT-425`, `KT-579`, `KT-580`, `KT-581`, `KT-602`, `KT-632`, `KT-709`, `KT-748`, `KT-778`, `KT-779`, `KT-796`, `KT-797`, and `KT-923`, plus Hylo issue `#1533`, still point at the same follow-up gap: once `Array a` is a standard prelude type, users will expect a clearly portable convenience vocabulary and literal-construction story beyond the now-specified core constructors/length/get/index bridge
  - related spec: §2.6, §2.8.3, §2.8.4, §17.1

- Consider specifying a portable prelude exposure policy for basic sequence/cardinality helpers.
  - shape: decide whether names such as `length` are guaranteed by `std.prelude` for `List`, `String`, `Array`, and similar finite sequence types, or whether portable source must instead rely on module-qualified collection-specific helper names
  - motivation: Idris2 issue `#346` and Kotlin issues `KT-789` and `KT-868` show how quickly the user model breaks when one ubiquitous `size` / `length` / `empty` helper is implicitly available for one collection family but not another or depends on ad hoc extension-property surfaces
  - related spec: §2.6, §17.1

- Consider whether portable `Bool` should expose an eager boolean-algebra helper surface beyond short-circuit operators.
  - shape: decide whether helpers such as `xor`, eager conjunction/disjunction, or other boolean-algebra operations are guaranteed by the standard library/prelude, and distinguish them clearly from control-flow-sensitive short-circuit `&&` / `||`
  - motivation: Kotlin issue `KT-828` is library-shaped, but it points at a real portability question for Kappa too: once `Bool` is a core prelude type, users may expect a portable eager boolean helper vocabulary rather than reconstructing everything ad hoc from short-circuit control operators
  - related spec: §2.6, §4.2

- Clarify the portable encounter-order story for ordinary `Set` / `Map` iteration and transformations.
  - shape: specify whether plain `Set` / `Map` iteration is ordered, insertion-ordered, sorted-by-structure, or explicitly unordered, and if future surface operations such as union/difference/filter/mapping are portable, specify whether they preserve left-input encounter order or deliberately yield unordered results
  - motivation: Kotlin issue `KT-647` highlights that users rely on stable order from collection transforms, but Kappa currently specifies orderedness mainly for query/comprehension pipelines and separately forbids backend hash-table iteration order from changing source semantics; the ordinary `Set` / `Map` surface still needs an explicit portability story
  - related spec: §10.3.2, §10.6, §17.1

- Clarify the portable range surface and endpoint-overflow behavior.
  - shape: standardize which prelude scalar types have `Rangeable` evidence, whether exact-width integer and `Char` ranges share one canonical range representation or distinct representations, and how inclusive ranges near maximum endpoints terminate without overflow
  - motivation: Kotlin issues `KT-489`, `KT-492`, `KT-580`, `KT-821`, `KT-925`, `KT-930`, `KT-931`, `KT-944`, `KT-958`, and `KT-1045` show that numeric and character range APIs need an explicit portability story, especially when an inclusive range reaches the maximum representable endpoint, when a range endpoint comes from an array accessor helper, when users expect stepped-range or named range-construction convenience APIs, when descending/reversed-range or empty-range behavior might otherwise be implicit, when negated or otherwise transformed ranges are expected to remain iterable, when membership tests lower through host backends, or when explicit range construction/runtime lowering is otherwise left underspecified
  - related spec: §4.4, §10.2, §10.10.1A, §17.4

- Clarify the portable exact-width signed integer operational surface.
  - shape: if Kappa exposes exact-width signed integers directly or through backend-specific profiles, specify literal/narrowing conversion, widening casts between widths, casts to/from floating-point and unbounded integers, left/right shifts, and overflow behavior together, including whether out-of-range `fromInteger`-style construction wraps, traps, saturates, or is rejected, and whether shift operations are defined by mathematical scaling followed by narrowing or by some width-masked primitive rule
  - motivation: Idris2 issues `#486`, `#545`, and `#549` show that primitive arithmetic and raw foreign interop become unusable when literal conversion, cross-width casts, shift/add/sub overflow semantics, and FFI width preservation disagree or are only half-specified
  - related spec: §2.6, §4.1.4, §4.1.5, §17.1

- Clarify the runtime conversion story from signed integers into `Nat`.
  - shape: specify whether converting a negative signed or unbounded integer into `Nat` clamps to `0`, rejects, traps, or follows some other rule, and require the runtime result to agree with any static reasoning or library laws that treat `Nat` as nonnegative
  - motivation: Idris2 issue `#775` shows that backend/runtime conversion paths can silently manufacture impossible negative inhabitants of `Nat` unless the signed-to-`Nat` rule is explicit and uniformly enforced
  - related spec: §2.6, §4.1.4, §17.1

- Consider explicitly accepting or rejecting unary plus as numeric-expression sugar.
  - shape: decide whether `+n` should be a standard prefix operator analogous to unary `-`, whether it is only ordinary user-defined prefix syntax when imported, or whether it should remain absent from v0.1 numeric literal conventions
  - motivation: Kotlin issues `KT-494` and `KT-877` highlight the same target-typing and backend-portability pressure for unary `+` as for unary `-`; Kappa currently specifies only prefix `-`, so conformance tests can cover `-1` but not `+1` without a clearer portability rule
  - related spec: §3.5.3, §4.1.4, §4.1.5

- Consider structural forwarding / delegation helpers for signature-shaped APIs.
  - shape: decide whether Kappa should offer library or language sugar for constructing a dictionary/package/signature value by forwarding same-named compatible members from an unrelated value, without adding ambient structural subtyping or nominal inheritance
  - motivation: Kotlin issue `KT-505` asks for delegation by method signature rather than by declared interface conformance; Kappa already has explicit records, packages, and dictionaries, but no concise forwarding helper for the common "this value has the right operations" adapter case
  - related spec: §5.5.10, §5.5.13, §7.1.3B, §12.2

- Consider specifying the portable `String` convenience-member surface more fully.
  - shape: standardize the core operations that every portable `String` implementation must expose, such as replacement, slicing/search, length, direct iteration or query adapters, and Unicode/codepoint expectations, instead of leaving this entirely to host bridges or ad hoc prelude growth
  - motivation: Kotlin issues `KT-427`, `KT-446`, `KT-480`, `KT-842`, and `KT-968` are library-shaped, but they highlight the same portability risk for Kappa: if `String` is a core prelude type, users need to know which operations, iteration surfaces, and renderings are portable, which helpers are intentionally omitted, and which are host-profile extensions
  - related spec: §2.6, §4.3.4, §17.1, §17.7

- Consider a future Unicode identifier/operator profile.
  - shape: if Kappa ever expands beyond its current ASCII-centric standard identifier surface, specify which Unicode categories are allowed in identifiers and operator spellings, whether mathematical relation/logical symbols are admitted as ordinary operators or require explicit aliases/macros, which normalization/security checks apply, and whether library helpers such as case classification are required to follow the pinned `std.unicode` version consistently across source processing and runtime text operations
  - motivation: Idris2 issue `#489` and GHC issue `#21162` show that "Unicode support" is not one question: source identifiers, operator spellings, mathematical-symbol surface policy, and runtime text classification all need one coherent policy
  - related spec: §2.7F, §3.1, §3.3, §17.2.4.2A

- Consider a future syntax for introducing implicit binders inside infix constructor patterns.
  - shape: if Kappa eventually allows constructor patterns to bring implicit parameters into scope explicitly, specify one canonical spelling that works equally for prefix and infix constructor patterns, rather than forcing users to fall back to prefix-only forms when they need access to hidden constructor arguments
  - motivation: Idris2 issue `#495` shows the ergonomic friction when infix-pattern sugar and explicit implicit-binder introduction drift apart
  - related spec: §7.3, §7.6.1

- Clarify whether source-visible reference / object identity equality exists.
  - shape: either explicitly decline a general `identityEquals`-style primitive in favor of `Eq`, semantic-object identity, `FiberId`, and explicit handle/fiber identity APIs, or define a narrow standard primitive for reference-like values that have source-visible identity
  - motivation: Kotlin issues `KT-481`, `KT-574`, and `KT-1055` are about an intrinsic `identityEquals`; Kappa currently specifies several identity notions but no general source-level physical/reference equality operator
  - related spec: §2.8.6, §8.1.3F, §14.4, §17.3.4

- Clarify which `DynamicType` / `DynRep` instances are portable.
  - shape: explicitly list whether tuples, records, functions, receiver-marked functions, dictionaries, erased/indexed/generic type applications, and other higher-order values have canonical `DynamicType` instances, and if not, state the intended opaque or non-representable behavior
  - motivation: Kotlin issues `KT-349`, `KT-350`, `KT-445`, `KT-447`, and `KT-515`, plus GHC issue `#9429`, are reminders that "runtime type information exists" is not enough; portable users need to know which type forms can be represented by default, which can expose host class tokens through a bridge, and which require custom runtime carriers instead of magical universal placeholders
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
  - motivation: Kotlin issues `KT-339`, `KT-432`, `KT-572`, and `KT-1020` reflect recurring ergonomic pressure around `Unit`-returning callback and function types, including lighter notation for very small function signatures; Kappa intentionally keeps `Unit` explicit today, but callback-heavy APIs may benefit from a compact spelling later
  - related spec: §4.5, §7.2.1, §8.4

- Consider ordinary function defaults as a future ergonomic extension.
  - current v0.1 design deliberately limits defaults to named constructor application and says ordinary function defaults do not exist
  - motivation: Kotlin issues `KT-265` and `KT-374` are reminders that users often expect the same named/default-argument ergonomics for ordinary functions that Kappa now has for constructors and named function application
  - related spec: §7.1.3C, §11.1, §11.1.1

- Consider augmented assignment and increment/decrement sugar as a future ergonomic extension.
  - shape: if added, it should be explicit about whether `x += y`, `x++`, and similar forms require a mutable `var`/place, whether they desugar through named receiver-marked functions or trait members, and whether immutable bindings may call mutating methods on their internal state without rebinding the binding itself
  - motivation: Kotlin issues `KT-469`, `KT-471`, `KT-512`, `KT-556`, `KT-575`, `KT-613`, `KT-723`, `KT-724`, `KT-725`, `KT-740`, `KT-790`, `KT-817`, `KT-894`, `KT-903`, and `KT-935`, plus Hylo issue `#1523`, show recurring pressure for user-defined compound assignment and increment protocols, including backend fragility around increment/`+=` lowering on mutable/optional places, conditional-expression operands, the need to evaluate the mutated receiver exactly once, and confusion about whether immutable bindings may still expose mutating receiver-style updates; Kappa currently keeps mutation explicit through `var`, references, `inout`, and ordinary function calls
  - related spec: §3.5, §8.2, §8.8, §12

- Consider optional collection / iterator sources as explicit iteration sugar.
  - shape: decide whether `for` and query generators should accept `Option src` as zero-or-one source expansion when `src` itself is iterable, or require users to spell the `match` / `?: empty` conversion explicitly
  - motivation: Kotlin issue `KT-531` asks for nullable iterators to mean "skip the loop if absent"; Kappa has `Option`, `Iterator`, and `IntoQuery`, but does not currently define an `Option`-as-empty iteration source convention
  - related spec: §5.4.9, §8.7.5, §8.7.6, §10.10.1

- Consider explicit mutable-iteration sugar for loops.
  - shape: if Kappa later adds `for` forms that yield mutable elements, borrowed places, or `inout` aliases from an iterable root, require the borrow root marker to be explicit and specify whether the loop binder is a copied value, a borrow, or a mutable place; iterator lowering should preserve that access mode instead of silently coercing mutable elements to ordinary values
  - motivation: Hylo issues `#1510` and `#1512` show that mutable iteration becomes confusing quickly when the borrowed root is implicit or when lowering loses the element's access mode
  - related spec: §6.1.1, §8.7.5, §8.8.3

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

- Consider letting exact stored members satisfy future getter-only property requirements directly.
  - shape: if Kappa later standardizes projection/property requirements inside traits or interfaces, an ordinary stored field or exact selector projection with the required type should satisfy a getter-only requirement without forcing a trivial forwarding wrapper definition
  - motivation: Hylo issue `#1555` shows the ergonomic cost of making property-oriented interfaces second-class when a stored member already has the exact read-only shape the requirement asks for
  - related spec: §6.1.1, §12.2.1

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

- Consider a standard diagnostic code for handler-selection ambiguity, if such ambiguity is ever exposed to users.

- Consider adding a compact "spec testing examples" appendix subsection for:
  - local recursion
  - handler finalization
  - macro hygiene across module boundaries

The remaining items here are intended to represent actual spec work, not simple evidence that an existing rule can be
tested.
