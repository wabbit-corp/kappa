I reviewed the QTT path statically. I could not run the test suite because `dotnet` is not installed in this container, so this is a code review, not a “green CI” blessing from the machine priests.

## Verdict

The QTT implementation is **not yet sound against the spec**. The broad architecture is reasonable: surface quantities flow into `ResourceModel`, `ResourceChecking`, ownership facts, then most quantity information is erased during runtime lowering. That means the checker must be right before lowering, because after that the compiler politely forgets the evidence, like a bureaucrat shredding the murder weapon.

The main problem is not syntax. It is the **usage algebra**. The checker currently mixes several concepts that the spec keeps distinct:

1. runtime-relevant demand,
2. consuming ownership transfer,
3. positive lower-bound discharge,
4. borrow liveness,
5. control-flow completion paths,
6. path-sensitive field consumption.

That conflation produces real QTT holes.

---

## 1. Critical: borrowed demand can satisfy a linear `1` obligation without consuming it

Spec says a borrowed parameter demand counts as runtime-relevant, but a `1` obligation is not discharged merely because the value appeared in an expression. Linear discharge requires transfer/consume/destructor-style use. See `Spec.md:6147-6150` and especially `Spec.md:6267-6279`.

The implementation does this:

```fsharp
let private addNonConsumingDemand document name state =
    ...
    |> updateBinding binding.Id (fun current ->
        { current with
            UseMinimum = max current.UseMinimum 1 })
```

Source: `src/Kappa.Compiler/ResourceChecking.fs:354-364`.

That only raises `UseMinimum`; it does **not** mark a consuming discharge, and it does **not** raise `UseMaximum`. Then scope exit checks only:

```fsharp
if binding.CheckLinearDrop
   && binding.UseMinimum < 1
```

Source: `src/Kappa.Compiler/ResourceChecking.fs:4197-4214`.

So this shape is likely accepted by QTT when it should be rejected:

```kappa
inspect : (& t : Token) -> Unit

bad : (1 t : Token) -> Unit
let bad t =
    inspect t
```

Why this is bad:

* `inspect t` is a borrow demand.
* Borrowing `t` is runtime-relevant, yes.
* But it is not a linear discharge.
* The checker sees `UseMinimum = 1` and decides the linear obligation is satisfied.
* Worse, ownership facts can represent an impossible inferred interval like `[1,0]`, because `bindingDemand` reports `Interval(binding.UseMinimum, binding.UseMaximum)` at `ResourceChecking.fs:285-288`.

This is the most important bug I found. It is not a diagnostics polish issue. It means QTT can accept generic discard-by-borrow patterns for linear values.

### Fix

Split the state:

```fsharp
type DemandInterval =
    { Min: int
      Max: int option }   // option is needed for infinity

type LinearDischarge =
    | NotDischarged
    | DischargedByConsume of SourceLocation option
    | Transferred
    | StructurallyDischarged of ResourcePlace list

type ResourceBinding =
    { Demand: DemandInterval
      Discharge: LinearDischarge
      ... }
```

Then:

* borrowed demand contributes `[1,1]` to runtime demand,
* borrowed demand does **not** discharge `1`,
* consuming demand contributes `[1,1]` and discharges/updates ownership,
* `>=1` checks demand lower bound,
* `1` checks both exact demand interval and linear discharge.

Right now one pair of integers is trying to be a proof system. Predictably, it has taken up lying.

---

## 2. Critical: loops under-approximate upper bounds

The spec explicitly says loop bodies may execute zero times, but uses inside an unbounded loop may contribute upper bound `∞`. See `Spec.md:15158-15175`.

Implementation:

```fsharp
| DoWhile(condition, body) :: rest ->
    let current =
        checkExpression ... condition
        |> addDeferredFact OwnershipDeferredFact.WhileResourceFixedPoint

    let bodyState = checkDoStatementsInScope "while" ... current body
    let current = mergeBranchState current bodyState
    loop localTypes current rest
```

Source: `src/Kappa.Compiler/ResourceChecking.fs:6914-6921`.

The merge uses:

```fsharp
UseMinimum = min leftBinding.UseMinimum rightBinding.UseMinimum
UseMaximum = max leftBinding.UseMaximum rightBinding.UseMaximum
```

Source: `src/Kappa.Compiler/ResourceChecking.fs:3669-3688`.

That treats a loop like “zero or one iteration,” not “zero or many.” The state model also stores `UseMaximum: int`, so it cannot represent `∞` for inferred usage even though `ResourceQuantity.Interval` supports `maximum: int option`.

This shape should be rejected:

```kappa
touch : (1 x : Token) -> Unit

bad : (<=1 x : Token) -> IO e Unit
let bad x =
    do
        while cond do
            touch x
        pure ()
```

If the loop runs twice, `x` is demanded twice. Spec says upper contribution is `∞`. The current checker computes max one body pass and can accept affine/at-most-once cases incorrectly.

### Fix

Loop transfer should widen upper bounds for outer bindings touched in the body:

```text
body demand [l,u]
while demand [0,∞] for any touched outer binding unless statically bounded
```

More concretely:

* if body demand for an outer binding has upper `> 0`, loop upper becomes `None`,
* loop lower is `0` unless the language proves at least one iteration,
* break/continue/return require completion-indexed summaries, discussed next.

The existing `WhileResourceFixedPoint` deferred fact is observability, not enforcement. A fact saying “somebody should prove this later” is adorable, but the backend does not become safe because a note was written on a napkin.

---

## 3. High: `do` control flow is not completion-indexed

The spec requires completion-indexed QTT summaries: `Normal`, `Return`, `Break`, `Continue`, `Unreachable`. See `Spec.md:6154-6175`, `Spec.md:6188-6190`, and `Spec.md:14864-14879`.

The implementation checks an `if` in a `do` block by checking both branches, merging their states, and then continuing with the rest:

```fsharp
let trueState = checkDoStatementsInScope "if_then" ... current whenTrue
let falseState =
    if List.isEmpty whenFalse then current
    else checkDoStatementsInScope "if_else" ... current whenFalse

loop localTypes (mergeBranchState trueState falseState) rest
```

Source: `src/Kappa.Compiler/ResourceChecking.fs:6893-6907`.

A `return` is just another state-producing path:

```fsharp
| DoReturn expression :: _ ->
    ...
    |> checkAllActiveLinearDrops document
```

Source: `src/Kappa.Compiler/ResourceChecking.fs:6908-6913`.

The checker loses the fact that the `then` branch returned and should not flow into following statements.

This should be accepted:

```kappa
consume : (1 t : Token) -> Unit

ok : (1 t : Token) -> Bool -> IO e Unit
let ok t b =
    do
        if b then
            consume t
            return ()
        consume t
```

Pathwise:

* if `b = true`, consume once, return;
* if `b = false`, consume once after the `if`.

The current merge likely reports overuse because it carries the returned branch’s `UseMaximum = 1` into the normal continuation, then sees another consume.

So this is partly a false-positive bug. More importantly, it proves the current checker is not implementing the spec’s control-flow model. Once upper bounds, loops, break/continue, and partial path consumption interact, the single-state approach will keep generating wrong answers.

### Fix

Make statement checking return a summary:

```fsharp
type Completion =
    | Normal
    | Return of label option
    | Break of label option
    | Continue of label option
    | Unreachable

type QttSummary = Map<Completion, CheckState>
```

Sequential composition should only feed `Normal` into the next statement. Abrupt completions are carried upward and checked at the boundary they exit.

---

## 4. High: quantity variables are parsed, but not actually supported by QTT

The spec allows:

```kappa
forall (q : Quantity). ...
```

and use of `q` as a binder quantity. See `Spec.md:5960-5977`.

The parser/type-signature layer knows about quantity variables:

```fsharp
| head :: rest when isQuantityVariableToken head ->
    ...
    Some(QuantityVariable(...), rest)
```

Source: `src/Kappa.Compiler/TypeSignatures.fs:263-296`.

But the resource checker does not meaningfully support them:

* `ResourceQuantity.Variable` exists: `ResourceModel.fs:19-23`.
* `ResourceQuantity.satisfies` has no variable case: `ResourceModel.fs:90-96`.
* `minimumRequiredUses` returns `0` for non-intervals, including variables: `ResourceModel.fs:76-82`.
* `concreteQuantityCountsUse` ignores variables: `ResourceChecking.fs:205-209`.
* separate signature parsing only recognizes literal quantities, not variable quantities: `ResourceCheckingSignatures.fs:36-75`.
* `functionParameterInfoFromType` walks only `TypeArrow`, not `forall`/`TypeLambda`: `ResourceChecking.fs:964-973`.

This means quantity-polymorphic APIs are in the uncanny valley: syntactically present, semantically mostly absent.

This kind of signature is not reliably enforced:

```kappa
apply : forall (q : Quantity) (a : Type) (b : Type).
        (q fn : a -> b) -> (q x : a) -> b
```

Possible outcomes are all bad:

* `q` gets ignored as “no positive lower bound,”
* application checks fail because `Variable` satisfies nothing,
* parameter name/type extraction gets confused,
* or the checker accepts code without proving the symbolic quantity constraints.

### Fix

Pick one:

1. **Temporarily reject `QuantityVariable` in QTT-relevant positions**, with a clear diagnostic.
2. Implement symbolic quantity constraints:

   * `q ⊑ q`,
   * `q ⊑ ω` only if known,
   * interval instantiation,
   * forall quantity substitution,
   * constraint solving at applications,
   * serialization through signatures/interfaces.

Half-supporting quantity polymorphism is worse than rejecting it. It creates the illusion of a type system, which is the most dangerous kind of stationery.

---

## 5. High: hidden borrow roots are created as `1` but are not drop-checked

Spec says a borrowed local binding over a non-place expression is conceptually:

```kappa
let 1 __tmp = expr
let & pat = __tmp
in body
```

See `Spec.md:6493-6514` and `Spec.md:10172-10184`.

Implementation:

```fsharp
let private introduceHiddenBorrowRoot scopeLabel state =
    let hiddenName = $"__tmp{state.NextBindingId}"

    ResourcePlace.root hiddenName,
    addBinding LocalBinding hiddenName (Some ResourceQuantity.one) None Set.empty [] false None None None state
```

Source: `src/Kappa.Compiler/ResourceChecking.fs:3799-3803`.

The hidden root is given quantity `1`, but `checkDrop=false`.

That means the implementation is not actually doing the conceptual transform. It is doing:

```kappa
let 1 __tmp = expr   // but magically exempt from linear checking
let & pat = __tmp
```

That weakens linearity around `let & pat = expr` for non-place expressions.

A suspicious shape:

```kappa
makeToken : Unit -> Token
inspect : (& t : Token) -> Unit

bad =
    let & t = makeToken ()
    inspect t
```

If `makeToken ()` produces a linear resource, the hidden `__tmp` should not evaporate. The checker currently creates it as owned but exempts it from the drop check. Marvellous. A linear resource with diplomatic immunity.

### Fix

Hidden roots need one of these designs:

* make them real owned bindings with `checkDrop=true`, then require an explicit discharge;
* or model them as compiler-managed temporaries with a specified destructor/release rule;
* or reject hidden-root borrowing of linear-producing expressions unless a protected scope discharges the hidden root.

The current “quantity one, but don’t check it” design is semantically incoherent.

---

## 6. Medium-high: explicit borrow regions are parsed in one layer and erased/mangled in another

`TypeSignatures` parses explicit borrowed regions:

```fsharp
| "&" "[" region "]" -> QuantityBorrow(Some region)
```

Source: `src/Kappa.Compiler/TypeSignatures.fs:276-279`.

But `ResourceCheckingSignatures.quantityFromSignatureSegment` only recognizes bare `&`:

```fsharp
| token :: _ when token.Text = "&" -> Some(ResourceQuantity.Borrow None)
```

Source: `src/Kappa.Compiler/ResourceCheckingSignatures.fs:65-67`.

The signature token stripper also strips only the `&`, not the `[s]` region tokens:

```fsharp
| { Kind = Operator; Text = "&" } :: rest -> rest
```

Source: `src/Kappa.Compiler/ResourceCheckingSignatures.fs:111-126` and `175-190`.

So a signature binder like:

```kappa
(&[s] x : T)
```

can become quantity `&` plus leftover `[s] x : T` tokens in the name/type path. That is exactly the sort of thing one hopes not to find in a compiler unless one enjoys litigation with one’s future self.

Also, region identity is ignored by satisfaction:

```fsharp
| Borrow _, Borrow _ -> true
```

Source: `src/Kappa.Compiler/ResourceModel.fs:90-96`.

And constructor parameter token reconstruction loses explicit borrow regions:

```fsharp
| Some(QuantityBorrow _) -> [ "&" ]
```

Source: `src/Kappa.Compiler/TypeSignatures.fs:1465-1477`.

### Fix

Explicit regions must survive end-to-end:

* signature collection should use the parsed `TypeScheme`/binder AST, not re-split tokens;
* `Borrow(Some s)` should not be equivalent to `Borrow(Some t)` unless region equality/substitution proves it;
* anonymous regions should be fresh rigid skolems, not interchangeable names;
* constructor and public signature serialization must preserve `&[s]`.

---

## 7. Medium: application borrow/consume overlap check is order-asymmetric

The checker detects only earlier borrow vs later consume in the same application spine:

```fsharp
borrowArguments
|> List.filter (fun (borrowIndex, _, _) -> borrowIndex < consumeSourceIndex)
```

Source: `src/Kappa.Compiler/ResourceChecking.fs:6010-6040`.

But the spec says a temporary borrow created for function application ends when the call returns. See `Spec.md:6127-6129`.

That suggests all arguments participating in the call overlap for the duration of the call, not merely earlier borrowed arguments. If one argument consumes `r` and a later argument borrows `r.field`, the order should not make it safe unless the language defines argument evaluation and borrow lifetimes in a very particular way.

Current check catches:

```kappa
f (&r.field) (consume r)
```

but may miss the symmetric form:

```kappa
f (consume r) (&r.field)
```

Confidence: medium. This depends on the exact intended evaluation/lifetime rule. Given the spec language, the safer implementation is symmetric overlap checking across the full application spine.

### Fix

Build two sets for the entire application:

* borrowed footprints,
* consuming footprints,

then reject any overlap unless there is a proven evaluation/lifetime rule that makes the pair non-overlapping.

---

## 8. Medium: signature quantity parsing is duplicated and drifting

There are two parsers for quantity-bearing binders:

* `TypeSignatures.TryParseQuantityPrefix` handles `&[s]`, variables, `<=1`, `>=1`, omega, etc. Source: `TypeSignatures.fs:263-296`.
* `ResourceCheckingSignatures.quantityFromSignatureSegment` handles only a subset. Source: `ResourceCheckingSignatures.fs:36-75`.

This is the source of several bugs above. The compiler already has a type-signature parser. The QTT checker should consume the parsed signature AST, not perform token surgery with a butter knife.

### Fix

Make signature collection produce `FunctionSignature` from `TypeScheme`/`TypeExpr`:

```fsharp
TypeScheme
  -> peel forall binders
  -> peel arrows
  -> collect Quantity * parameter type * binder name metadata
```

Then delete the duplicate quantity parser. Dead code is the best code; tragically, humans insist on writing the other kind.

---

## Good parts

The concrete interval/borrow satisfaction table is mostly aligned with the spec:

```fsharp
| Interval(capMin, capMax), Interval(demMin, demMax) ->
    capMin <= demMin && maximumContains capMax demMax
| Borrow _, Borrow _ -> true
| Borrow _, Interval(0, None) -> true
| _ -> false
```

Source: `src/Kappa.Compiler/ResourceModel.fs:90-96`.

For the six concrete surface quantities, this matches the intended `q_cap ⊑ q_dem` relation, ignoring the explicit-region caveat above.

Runtime erasure is also structurally reasonable. `KRuntimeLowering` erases arrow quantities to omega and drops quantity-zero record fields: `KRuntimeLowering.fs:75-99`. That is fine **only if** QTT diagnostics are authoritative before backend emission. The CLR backend does gate emission on `workspace.HasErrors`: `ClrDotNetBackend.fs:8-14`.

So the compiler has the right broad staging: check QTT, then erase. The problem is that the checker is not yet strong enough to deserve that trust.

---

## Regression tests I would add first

### 1. Borrow-only must not discharge linear `1`

```kappa
inspect : (& t : Token) -> Unit

bad : (1 t : Token) -> Unit
let bad t =
    inspect t
```

Expected: reject.

### 2. Multiple borrowed uses of exact `1` must affect demand accounting

```kappa
inspect : (& t : Token) -> Unit

bad : (1 t : Token) -> Unit
let bad t =
    inspect t
    inspect t
```

Expected: reject, either because exact runtime demand exceeds one or because linear discharge never happens. The precise diagnostic depends on the intended split, but accepting it is wrong.

### 3. Relevant `>=1` may be satisfied by borrowed demand

```kappa
inspect : (& t : Token) -> Unit

ok : (>=1 t : Token) -> Unit
let ok t =
    inspect t
```

Expected: accept. This guards against overcorrecting by treating borrow as irrelevant.

### 4. Loop upper bound must be infinite

```kappa
touch : (1 x : Token) -> Unit

bad : (<=1 x : Token) -> IO e Unit
let bad x =
    do
        while cond do
            touch x
        pure ()
```

Expected: reject.

### 5. Return branch must not flow into later statements

```kappa
consume : (1 t : Token) -> Unit

ok : (1 t : Token) -> Bool -> IO e Unit
let ok t b =
    do
        if b then
            consume t
            return ()
        consume t
```

Expected: accept.

### 6. Quantity variable should either work or be explicitly rejected

```kappa
idQ : forall (q : Quantity) (a : Type). (q x : a) -> a
let idQ x = x
```

Expected: either accepted with real symbolic quantity checking, or rejected with a specific “quantity variables not yet supported by QTT” diagnostic. Silent partial behavior is the worst option.

### 7. Explicit region preservation

```kappa
borrowed :
    forall (s : Region). (&[s] x : Token) -> BorrowView s Token
```

Expected: region `s` survives signature extraction, checking, facts, and serialization.

---

## Recommended repair order

1. **Replace `UseMinimum`/`UseMaximum` with a real demand interval plus separate ownership discharge state.**
2. **Add `int option` maximums to inferred usage**, because loops require `∞`.
3. **Implement completion-indexed summaries** for `do`, `if`, `match`, `while`, and later `break`/`continue`.
4. **Unify signature parsing around `TypeSignatures.TypeScheme`.**
5. **Either ban or implement quantity variables.**
6. **Fix explicit region preservation.**
7. **Make application borrow/consume overlap symmetric unless the spec is narrowed.**
8. **Revisit hidden borrow roots and give them a real discharge story.**

The current QTT layer is a serious start, but it is still closer to an ownership linter than a sound quantitative type checker. The danger is that the compiler lowers and erases quantities after this pass, so any QTT mistake becomes invisible downstream. That is efficient in the same way removing the brakes makes a car lighter.
