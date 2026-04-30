I reviewed the uploaded compiler sources statically. I did **not** run the F# test suite because `dotnet` is not installed in the sandbox. Tragic, naturally. So this is a source-level review, with file/line references from the extracted tree.

## Executive verdict

The trait system is **partially implemented**, but the implementation is still much closer to “dictionary dispatch prototype” than to the spec’s full typeclass model.

The biggest gaps are:

1. **Deriving is not implemented at all**, apart from reserving the keyword.
2. **Constrained instances are not represented correctly**, because instance premises are used only to decide candidate viability, not carried into generated dictionaries or member bodies.
3. **Supertraits are treated as name reachability, not as dictionary projection**, which can produce runtime-invalid calls.
4. **Instance resolution does not implement the spec’s coherence algorithm**, especially Easy/Hard Hash equivalence.
5. **Instance declaration validation is too weak**, allowing missing members, extra members, unknown traits, wrong arities, and many bad member implementations to slip too far.
6. **The runtime metadata path guesses trait arity locally**, which breaks instances for traits imported from other modules, especially multi-parameter traits.
7. **Local instances / local derives from the spec are absent.**

The bones are there. The ligaments are, regrettably, decorative.

---

## 1. Deriving: effectively absent

`derive` is reserved as a keyword:

* `src/Kappa.Compiler/Syntax.fs:21`
* `src/Kappa.Compiler/Syntax.fs:93`

But there is no AST case for derive declarations. `TopLevelDeclaration` has `TraitDeclarationNode` and `InstanceDeclarationNode`, then falls through to `UnknownDeclaration`:

* `src/Kappa.Compiler/Syntax.fs:713-725`

There is also no `ParseDerive`, no `Keyword.Derive` handling in `Parser.fs`, and no deriving logic in `SurfaceElaboration.fs`.

So `derive Eq Foo` is not a feature. It is a haunted signpost.

### What to do

Add a real representation:

```fsharp
type DeriveDeclaration =
    { TraitName: string
      TargetTokens: Token list
      FullTokens: Token list
      Span: TextSpan }

type TopLevelDeclaration =
    ...
    | DeriveDeclarationNode of DeriveDeclaration
```

Then lower derives into synthetic `InstanceInfo` values before normal instance validation/resolution. Do not special-case derived instances later in the backend. Deriving should become “instance generation”, not a parallel world with its own rules. Parallel worlds are how compilers become sociology experiments.

Minimum derivation pipeline:

1. Parse `derive Trait Target`.
2. Resolve `Trait` in trait namespace.
3. Resolve `Target` as a visible type constructor or concrete type.
4. Validate that representation is visible, especially for `opaque data`.
5. Generate a synthetic instance declaration or directly generate `InstanceInfo`.
6. Run the **same** instance coherence, missing-member, member-type, and runtime-lowering pipeline as handwritten instances.
7. Add provenance marking it as `derived`.

For `Eq`, do not claim conformance until `eqSound` and `eqComplete` are synthesized or explicitly stubbed as unsupported. The spec requires those proofs. The current implementation cannot honestly derive spec-level `Eq`.

---

## 2. Instance resolution is viable, but not spec-compliant

The core resolver is here:

* `src/Kappa.Compiler/SurfaceElaboration.fs:4964-5035`

It does this:

1. Normalize the goal.
2. Search `environment.VisibleInstances`.
3. Filter by trait name and arity.
4. Unify instance head with goal.
5. Recursively solve premises.
6. If zero candidates survive, unresolved.
7. If one candidate survives, resolved.
8. If multiple survive, ambiguous.

The relevant ambiguity behavior is here:

* `src/Kappa.Compiler/SurfaceElaboration.fs:5027-5033`

That directly contradicts the spec’s intended coherence model, where multiple surviving candidates may be accepted if their instantiated dictionaries have the same canonical Hard Hash.

There is no implementation of:

* Easy Hash comparison.
* Hard Hash comparison.
* Canonical representative selection.
* Equivalence acceptance for duplicate instances.
* Deterministic representative selection beyond sorting for diagnostics.

I found no implementation references to `HardHash`, `EasyHash`, or the canonical representative algorithm. The resolver simply rejects all multiple survivors. A small mercy, I suppose, but not the language you specified.

### Recommendation

Introduce a richer resolution result:

```fsharp
type InstanceCandidate =
    { Instance: InstanceInfo
      Substitution: Map<string, TypeExpr>
      PremiseEvidence: Evidence list
      CanonicalHash: Hash option }

type ConstraintResolutionResult =
    | ConstraintUnresolved
    | ConstraintResolved of InstanceCandidate
    | ConstraintAmbiguous of TraitConstraint * InstanceCandidate list
    | ConstraintIncoherent of TraitConstraint * InstanceCandidate list
```

Then split resolution into phases:

```text
collect candidates
instantiate candidate heads
solve premises, preserving evidence
canonicalize instantiated dictionaries
compare hashes
select representative or reject incoherence
```

Do not throw away premise evidence. Which brings us to the main structural problem.

---

## 3. Constrained instances are not actually represented

This is the most serious bug.

The resolver uses instance premises to decide whether a candidate survives:

* `src/Kappa.Compiler/SurfaceElaboration.fs:4996-5015`

But once the instance is selected, the generated dictionary is just:

```fsharp
KCoreDictionaryValue(moduleName, instanceInfo.TraitName, instanceInfo.InstanceKey)
```

The dictionary does **not** carry the solved premise dictionaries.

The synthetic instance dictionary is zero-argument:

* `src/Kappa.Compiler/SurfaceElaboration.fs:20343-20347`

And handwritten instance member bodies are lowered under an environment with no constrained members:

* `src/Kappa.Compiler/SurfaceElaboration.fs:20253-20270`
* `src/Kappa.Compiler/SurfaceElaboration.fs:20302-20310`

So this kind of instance is not truly implementable:

```kappa
trait Show a =
    show : a -> String

data Box a =
    Box a

instance Show a => Show (Box a) =
    let show box =
        match box
        case Box x -> show x
```

The premise `Show a` is needed inside the member body. The resolver checks that such a premise can be solved for a concrete goal, but the generated member function has nowhere to receive it.

That means constrained instances are being treated like Prolog clauses during lookup, but like monomorphic global dictionaries at runtime. That is two models duct-taped together, and duct tape has never been a type system.

### Correct model

A constrained instance should lower as a dictionary constructor/function, not a closed dictionary constant.

Conceptually:

```kappa
instance Show a => Show (Box a)
```

should produce something like:

```fsharp
__dict_Show_Box :
    Dict (Show a) -> Dict (Show (Box a))
```

and member functions should either:

1. close over premise dictionaries, or
2. accept the dictionary as a runtime record containing premise evidence, or
3. be specialized per resolved instantiation.

Your current `KCoreDictionaryValue(module, trait, instanceKey)` is not enough. It needs to become something more like:

```fsharp
type KCoreEvidence =
    | KCoreLocalEvidence of string
    | KCoreInstanceEvidence of moduleName: string * traitName: string * instanceKey: string * premises: KCoreEvidence list
```

Or dictionaries need fields:

```fsharp
type RuntimeDictionary =
    { ModuleName: string
      TraitName: string
      InstanceKey: string
      Premises: RuntimeDictionary list }
```

Then resolving `Show (Box Int)` would produce:

```text
InstanceEvidence(BoxShow, [InstanceEvidence(IntShow, [])])
```

Without this, recursive/constrained typeclass instances are going to fail or accidentally resolve through unrelated globals. Beautiful in the way a bridge collapse is “structurally expressive.”

---

## 4. Supertraits are name reachability, not dictionary projection

The compiler does parse and track supertraits:

* `TraitInfo.Supertraits`: `src/Kappa.Compiler/SurfaceElaboration.fs:17-18`
* Supertrait instantiation: `src/Kappa.Compiler/SurfaceElaboration.fs:4810-4818`
* Reachability traversal: `src/Kappa.Compiler/SurfaceElaboration.fs:4822-4846`

Then `buildConstrainedMembers` makes members from reachable supertraits callable:

* `src/Kappa.Compiler/SurfaceElaboration.fs:5082-5106`

The problem is that it maps a supertrait member to the **same dictionary parameter name**:

```fsharp
Map.add memberName (reachableConstraint.TraitName, dictionaryParameterName)
```

So if you have:

```kappa
trait Eq a =
    eq : a -> a -> Bool

trait Eq a => Ord a =
    compare : a -> a -> Ordering

use : Ord a => a -> a -> Bool
let use x y = eq x y
```

`eq` is reachable from `Ord a`, so the compiler will lower it roughly as:

```fsharp
KCoreTraitCall("Eq", "eq", KCoreName ["__kappa_dict_Ord_0"], ...)
```

That is not a valid dictionary unless an `Ord` dictionary is also physically an `Eq` dictionary. The spec says a dictionary for `Tr args` contains coherent evidence for supertraits. That requires actual projection or embedded evidence, not just changing the trait name in the call while reusing the same object.

The same problem exists in local implicit projection:

* `src/Kappa.Compiler/SurfaceElaboration.fs:5325-5370`

When a local dictionary for a subtrait can satisfy a supertrait goal, the compiler returns the same `KCoreName [name]` instead of generating a projection.

### Correct fix

Represent supertrait evidence explicitly:

```fsharp
type Evidence =
    | LocalDict of string * TraitConstraint
    | InstanceDict of InstanceInfo * Evidence list
    | SupertraitProjection of Evidence * fromTrait: TraitConstraint * toTrait: TraitConstraint
```

Then lower `SupertraitProjection` to either:

1. a dictionary field access,
2. a synthetic wrapper dictionary,
3. a runtime dictionary projection function, or
4. a compile-time erased coercion if dictionaries are represented uniformly and safely.

But do not pass an `Ord` dictionary to an `Eq` dispatch call and hope nobody checks the label. Hope is not a calling convention.

---

## 5. Instance validation is too permissive

There is some validation for instance member return types:

* `src/Kappa.Compiler/SurfaceElaboration.fs:15540-15587`

And there is a supertrait-satisfaction diagnostic:

* `src/Kappa.Compiler/SurfaceElaboration.fs:15591-15638`

But the validation is far from sufficient.

Currently I do not see robust checks for:

* unknown trait in instance head,
* wrong trait arity,
* missing required members,
* extra member names,
* duplicate member definitions,
* full member type compatibility,
* parameter type compatibility,
* quantity compatibility,
* implicit/explicit parameter compatibility,
* default member validity under the instance context,
* declaration-time termination conditions for instance search,
* orphan warnings.

The return-type check only triggers if the member explicitly declares a return type:

* `src/Kappa.Compiler/SurfaceElaboration.fs:15566-15587`

If the member omits it, the check quietly does nothing. Parameter compatibility also appears to be ignored there.

In `synthesizeInstanceBindings`, if `traitInfo` is not found, `memberBindings` becomes empty, but a dictionary binding is still created:

* Trait lookup: `src/Kappa.Compiler/SurfaceElaboration.fs:20236-20238`
* Empty member fallback: `src/Kappa.Compiler/SurfaceElaboration.fs:20283-20333`
* Dictionary still emitted: `src/Kappa.Compiler/SurfaceElaboration.fs:20335-20347`

That is a bad failure mode. An instance for an unresolved trait should be a hard frontend error, not a ghost dictionary.

### Add an instance validation pass

Before lowering, validate every `InstanceInfo` against its `TraitInfo`:

```text
resolve trait
check arity
instantiate trait member schemes
check every required member exists
check no unexpected member exists
check member schemes match after substitution
check defaults typecheck under self + premises
check premises obey termination rule
emit orphan warning if applicable
```

This should produce structured diagnostics, not silent omission.

---

## 6. Instance search termination is not enforced at declaration time

Spec §12.3.2 requires a Paterson-style structural check. The current resolver has only a runtime recursion guard:

* `src/Kappa.Compiler/SurfaceElaboration.fs:4976-4980`

If the same goal appears again, it returns `ConstraintUnresolved`.

That prevents some infinite loops, but it is not the same as rejecting illegal recursive instances. It also produces misleading “could not resolve” behavior instead of “this instance declaration is non-terminating.”

### Add declaration-time checks

For each instance:

```kappa
instance P1, ..., Pn => H
```

validate each premise `Pi` against head `H`:

* premise must not be identical to head,
* no type variable occurs more often in premise than in head,
* total constructor-and-variable size of premise is strictly smaller than head.

This belongs in the frontend validation pass, near the existing instance supertrait diagnostics.

---

## 7. Imported multi-parameter trait instances get bad runtime metadata

Runtime lowering computes trait arity using only trait declarations in the current core module:

* `src/Kappa.Compiler/KRuntimeLowering.fs:930-935`

Then for each instance it does:

* `src/Kappa.Compiler/KRuntimeLowering.fs:937-968`

The key bug:

```fsharp
let argumentCount =
    moduleTraitArities
    |> Map.tryFind instanceDeclaration.TraitName
    |> Option.defaultValue 1
```

That means if module `impl` imports a trait declared elsewhere:

```kappa
trait Rel a b = ...
```

and declares:

```kappa
instance Rel Int String = ...
```

then `impl` does not have `Rel` in `moduleTraitArities`, so the runtime metadata assumes arity `1`.

That corrupts `HeadTypeTexts` for imported multi-parameter traits. This can affect runtime dumps, checkpoint verification, IL backend typing, and any metadata consumer that expects correct instance heads.

### Fix

Do not rediscover arity from local syntax during runtime lowering. Carry `InstanceInfo.HeadTypes` from surface elaboration into core/runtime metadata, or carry a global trait arity table into `KRuntimeLowering`.

The current code already had the correct head types earlier in `InstanceInfo`. Throwing them away and reparsing header tokens later is the kind of thing compilers do when they long for chaos.

---

## 8. Constraint parsing has a parenthesized-context bug

`TypeSignatures.ParseScheme` parses constraints here:

* `src/Kappa.Compiler/TypeSignatures.fs:1181-1237`

The constraint splitter strips outer parentheses by not adding them when `nestedDepth = 1`, but it only splits commas when `nestedDepth = 0`:

* `src/Kappa.Compiler/TypeSignatures.fs:1212-1228`

So this likely fails or drops constraints:

```kappa
instance (Eq a, Show a) => Ord a = ...
```

because the comma occurs while `nestedDepth = 1`.

Trait supertrait parsing has its own splitter and appears to handle this better:

* `src/Kappa.Compiler/SurfaceElaboration.fs:1935-1990`
* `src/Kappa.Compiler/SurfaceElaboration.fs:2055-2074`

But instance headers use:

* `src/Kappa.Compiler/SurfaceElaboration.fs:2100-2113`

which calls `TypeSignatures.parseScheme`.

So parenthesized multi-constraint instance headers are suspect.

### Fix

Unify the constraint splitting logic. There should be one tokenizer-level function:

```fsharp
splitConstraintContext : Token list -> Token list list
```

It should:

1. strip one balanced outer parenthesis pair if it encloses the whole context,
2. split commas at top level,
3. respect parentheses, brackets, braces, effect rows, and set braces,
4. return parse errors instead of silently dropping invalid constraints.

The current `List.choose parseConstraint` silently discards constraints that fail to parse. That is a bad habit. Silent compiler recovery is how bugs become folklore.

---

## 9. Unification probably treats rigid variables as solvable

Instance candidate matching uses:

* `src/Kappa.Compiler/SurfaceElaboration.fs:4030-4037`
* `src/Kappa.Compiler/SurfaceElaboration.fs:4994`

That delegates to `TypeSignatures.tryUnifyMany`, which delegates to `tryUnify`.

`tryUnify` binds type variables on either side:

* left variable case: `src/Kappa.Compiler/TypeSignatures.fs:1683-1687`
* right variable case: `src/Kappa.Compiler/TypeSignatures.fs:1688-1692`

That is fine for ordinary unification, but instance resolution usually needs **matching**, not symmetric unification. Candidate variables may be instantiated. Goal variables are often rigid.

Risk:

```kappa
instance C Int = ...
```

should not solve a generic goal:

```kappa
C a
```

unless `a` is genuinely an inference metavariable that the surrounding context is allowed to refine to `Int`.

Right now `TypeVariable "a"` appears to represent source-level type variables too, not just inference metavariables. If that is true, the resolver can over-select concrete instances for generic goals.

### Fix

Introduce a candidate-head matcher:

```fsharp
matchInstanceHead :
    candidateVars: Set<string> ->
    candidateHead: TypeExpr list ->
    rigidGoal: TypeExpr list ->
    Map<string, TypeExpr> option
```

Only variables bound by the instance declaration should be assignable. Goal variables should compare rigidly unless explicitly marked as inference metas.

---

## 10. Overloaded trait member calls reject members with extra constraints

`tryPrepareVisibleTraitMemberCall` creates an overloaded scheme by prepending the owning trait constraint:

* `src/Kappa.Compiler/SurfaceElaboration.fs:5873-5878`

But then it only accepts the prepared call if the instantiated scheme has exactly one constraint:

* `src/Kappa.Compiler/SurfaceElaboration.fs:5890-5898`

```fsharp
match preparedCall.InstantiatedScheme.Constraints with
| owningConstraint :: [] -> ...
| _ -> None
```

That rejects trait members whose signatures have additional constraints.

If the language allows constrained trait members, this is wrong. Even if you do not want to support them yet, this needs an explicit diagnostic. Right now it just silently fails to prepare the trait call and falls through to ordinary name application.

### Fix

Accept:

```fsharp
owningConstraint :: additionalConstraints
```

Resolve all constraints, then pass all required dictionaries. Or reject additional constraints during trait declaration validation with a direct diagnostic.

---

## 11. Local instances and local deriving are absent

The spec allows local block declarations:

```kappa
block
    instance C T = ...
    derive Eq T
    ...
```

But `SurfaceExpression` has local forms only for:

* `LocalLet`
* `LocalSignature`
* `LocalTypeAlias`
* `LocalScopedEffect`

See:

* `src/Kappa.Compiler/Syntax.fs:343-357`

There is no `LocalData`, `LocalTrait`, `LocalInstance`, or `LocalDerive` expression form, despite the spec discussing these. The parser may have some block machinery elsewhere, but the AST shown here cannot carry local instances as semantic declarations.

So the local instance part of trait resolution is not implemented in the model. Local implicit binders are implemented, but local instances are not the same thing.

---

## 12. Runtime dispatch model is very thin

Trait call execution in the interpreter:

* `src/Kappa.Compiler/Interpreter.fs:686-714`

It evaluates the dictionary expression, expects a `DictionaryValue`, then constructs a member binding name from:

```fsharp
traitName
dictionary.InstanceKey
memberName
```

and looks it up in `dictionary.ModuleName`.

This works for simple closed instances:

```kappa
instance Show Int = ...
```

It does not naturally support:

* constrained instances,
* superclass dictionaries,
* dictionary projection,
* associated static members,
* per-instantiation dictionaries,
* coherent equivalent instance representatives.

The current runtime dictionary is essentially a tagged pointer to a module-level member table. That is acceptable for milestone-level dispatch, but not for the trait system described in the spec.

---

## What is good

There is real groundwork here. Not just vaporware dressed in a waistcoat.

The strong parts:

* Traits and instances have first-class parsed nodes.
* Trait member dispatch is lowered into synthetic functions.
* Simple instance dispatch works across interpreter/backend paths.
* Instance visibility follows module dependency closure rather than lexical import-only lookup:

  * `src/Kappa.Compiler/SurfaceElaboration.fs:3277-3285`
* Ambiguous global instances are rejected deterministically rather than picked by map iteration.
* Transparent type aliases are normalized during instance matching.
* Supertrait information is parsed and partially used.
* Diagnostics exist for trait ambiguity and unsatisfied supertraits.
* The tests cover basic trait dispatch, alias-based instance lookup, module-closure visibility, and ambiguity rejection.

So this is not a disaster. It is a scaffold that has started believing it is a cathedral.

---

## Recommended implementation order

### Phase 1: Make existing trait instances sound

Do this before deriving. Deriving unsound instances automatically would simply automate the production of nonsense, which humanity already has social media for.

1. Add `Evidence` as a first-class internal representation.
2. Make instance resolution return evidence trees, not just `InstanceInfo`.
3. Represent constrained instance dictionaries as dictionary constructors or dictionary values with premise evidence.
4. Add real supertrait projection evidence.
5. Lower trait calls from evidence, not from raw `(module, trait, instanceKey)` triples.

### Phase 2: Add hard validation

Add a validation pass over all instance declarations:

```text
resolve trait
check arity
check missing members
check extra members
check duplicate members
check full member type compatibility
check default member bodies
check supertrait evidence
check termination
warn on orphan
```

Fail early. Do not let malformed instances reach runtime lowering.

### Phase 3: Fix parser/model gaps

1. Add `DeriveDeclarationNode`.
2. Add local declaration AST support if local instances remain in the spec.
3. Replace duplicated ad hoc header splitting with shared parser utilities.
4. Make constraint parse failures diagnostics, not `List.choose` disappearances.

### Phase 4: Implement coherence

Only after evidence is represented properly:

1. Canonicalize instantiated dictionaries.
2. Compute Easy Hash / Hard Hash.
3. Accept equivalent overlaps.
4. Reject incoherent overlaps.
5. Select deterministic representative.

### Phase 5: Implement deriving

Start with narrow, honest deriving:

```text
derive Show T
derive Hashable T
```

Then approach `Eq` only when proof member synthesis has a real story.

For `Eq`, choose one:

* implement actual proof synthesis for `eqSound` / `eqComplete`,
* restrict `derive Eq` to compiler-known structural cases with internal proof terms,
* or temporarily reject it with a diagnostic saying proof synthesis is not implemented.

Pretending `Eq` is just boolean equality would violate your own spec. A rare moment where the spec is less naïve than the code. Savour it.

---

## Bottom line

The current implementation is acceptable for **simple, closed, user-defined trait dispatch**:

```kappa
trait Show a =
    show : a -> String

instance Show Int =
    let show x = primitiveIntToString x
```

It is not yet sound for the richer system promised by the spec:

```kappa
instance Show a => Show (Box a)
trait Eq a => Ord a
derive Eq Foo
local instance C T
coherent overlapping equivalent instances
```

The architectural fix is to stop treating instance resolution as “find a global instance key” and start treating it as “construct evidence.” Once evidence is explicit, constrained instances, supertraits, deriving, coherence, and runtime lowering all have somewhere sane to live.
