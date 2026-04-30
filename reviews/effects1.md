## Review: effects and effect handlers

I reviewed the compiler source and tests in the uploaded archive. I did **not** run the test suite because the environment has no `dotnet` installed, so this is a static review. Annoying, but apparently even compilers must occasionally be judged by reading them instead of letting silicon scream.

### Executive verdict

The effect subsystem is a credible **interpreter-stage prototype**, not yet a sound implementation of the spec.

The strongest parts are:

* Effects and handlers are represented all the way through surface syntax, KCore, and KRuntime.
* Shallow and deep handler semantics are present.
* Deep handlers are lowered into a recursive shallow-handler driver before runtime verification.
* There is meaningful test coverage for basic one-shot handlers, row splitting, carrier agreement, handler clauses, aliases, and some multi-shot rejection cases.
* The interpreter’s shallow/deep propagation logic is broadly on the right track.

The serious weaknesses are structural:

1. **Effect label identity is still too string-based.**
2. **Resource checking still reasons over surface syntax in places where it needs elaborated semantic identity.**
3. **One-shot resumption safety can likely be bypassed through label aliases or record-carried labels.**
4. **Multi-shot safety is incomplete and non-compositional.**
5. **The implementation mostly assumes self-label rows like `<[State : State]>`, while the spec allows first-class labels carrying effect interfaces.**
6. **Backend support for effects is explicitly not implemented.**

So the good news is that this is not decorative syntax taped to a runtime. The bad news is that the tape is still holding several load-bearing beams together.

---

# What is solid

## 1. The compiler has a real effect pipeline

Effects are not merely parsed and thrown into a corner to die quietly, as many language features are.

Relevant places:

* `Syntax.fs`

  * `EffectDeclaration`
  * `EffectOperation`
  * `SurfaceExpression.LocalScopedEffect`
  * `SurfaceExpression.Handle`
  * `SurfaceEffectHandlerClause`

* `KCore.fs`

  * `KCoreEffectLabel`
  * `KCoreEffectOperation`
  * `KCoreHandle`

* `KRuntimeIR.fs`

  * runtime-level mirrors of labels, operations, and handlers

* `Interpreter.fs`

  * runtime execution of operation requests
  * shallow and deep handler behavior
  * handler clause dispatch
  * resumption construction

This is a proper staged implementation path, not merely a parser stunt. Humanity may yet recover.

## 2. Deep handlers are desugared before runtime

`SurfaceElaboration.fs` lowers deep handlers into a recursive shallow-handler driver. That matches the spec’s intended story: a deep handler can be treated as a recursive shallow handler that re-installs itself around resumptions.

The checkpoint verifier also checks that direct deep handlers do not survive into runtime/backend stages. That is good compiler hygiene.

Relevant areas:

* `SurfaceElaboration.fs`, around the deep-handler lowering logic
* `CheckpointVerification.fs`, where deep handlers are rejected past the expected stage

## 3. Tests cover more than the happy path

`MilestoneFourTests.fs` has meaningful tests for:

* one-shot scoped-effect handlers
* deep handler lowering
* shallow handler rehandling semantics
* missing, duplicate, unexpected, and arity-mismatched handler clauses
* handler carrier agreement
* row mismatch/interface mismatch
* closed and open effect row splitting
* label aliases and record-carried labels
* some multi-shot rejection behavior
* top-level effect labels and qualified imports

That is a better test surface than the usual “parse `hello world`, declare victory, update LinkedIn” situation.

---

# High-severity findings

## 1. Runtime handler matching uses label names, not label identity

This is probably the most important correctness issue.

In `Interpreter.fs`, handler matching compares effect labels by name, roughly:

```fsharp
String.Equals(request.Label.Name, label.Name)
```

That violates the spec’s requirement that handlers match by **effect-label identity**, not spelling or interface name.

Why this matters:

```kappa
scoped effect State =
  get : Unit -> Int

-- somewhere else, independently:
scoped effect State =
  get : Unit -> Bool
```

These should be distinct labels even though their printed names collide. Likewise, if labels are first-class and can be stored in records, passed around, or imported under aliases, the identity must follow the label object, not the name string.

Right now, the runtime model appears vulnerable to accidental capture between unrelated labels that share a textual name.

### Fix

Introduce a stable semantic identity:

```fsharp
type EffectInterfaceId = private EffectInterfaceId of Guid
type EffectLabelId = private EffectLabelId of Guid
```

Then runtime labels should carry both:

```fsharp
type RuntimeEffectLabel =
  { Id: EffectLabelId
    Name: string
    InterfaceId: EffectInterfaceId
    Operations: Map<string, RuntimeEffectOperationInfo> }
```

Handlers should match on `EffectLabelId`, not `Name`.

Textual names should be debug metadata only. Names are for humans, and humans are famously bad hash functions.

---

## 2. One-shot resumption checking can likely be bypassed through label aliases

This is a major resource-safety issue.

In `ResourceChecking.fs`, handler clause resumption quantities are resolved only when the handled label has a narrow syntactic form, roughly:

```fsharp
match label with
| Name [ effectName ] -> ...
| _ -> None
```

But the language supports label aliases and record-carried labels. Tests even cover alias and record-carried label usage for successful handling.

That creates a dangerous split:

* elaboration understands aliases
* runtime may execute aliases
* resource checking may fail to recover the handled effect identity
* therefore the resumption binder may not receive the correct one-shot quantity

A likely bypass shape:

```kappa
scoped effect Ask =
  ask : Unit -> Bool

let l = Ask

handle l computation with
  case return x -> pure x
  case ask () k ->
    do
      let a <- k True
      let b <- k False
      pure a
```

If `Ask.ask` is one-shot by default, calling `k` twice should be rejected. But if `handle l ...` does not resolve back to the `Ask` effect declaration during resource checking, `k` may not be checked as quantity-1.

The same concern applies to record-carried labels:

```kappa
let pkg = { label = Ask }

handle pkg.label computation with
  case ask () k ->
    do
      let a <- k True
      let b <- k False
      pure a
```

### Fix

Do not perform handler resource checking by re-parsing surface syntax.

Resolve the handled label once during elaboration into something like:

```fsharp
type ResolvedEffectLabel =
  { LabelId: EffectLabelId
    InterfaceId: EffectInterfaceId
    Operations: Map<string, EffectOperationSignature> }
```

Then resource checking should use that semantic object directly.

At minimum, `ResourceChecking.fs` needs the same label-resolution capability as `SurfaceElaboration.fs`, but duplicating that logic is how compilers grow mold. The cleaner fix is to make elaboration produce an IR where labels are already resolved.

### Tests to add

```kappa
-- one-shot resumption overuse through alias
let l = Ask
handle l comp with
  case return x -> pure x
  case ask () k ->
    do
      let a <- k True
      let b <- k False
      pure a
```

```kappa
-- one-shot resumption overuse through record-carried label
let pkg = { label = Ask }
handle pkg.label comp with
  case return x -> pure x
  case ask () k ->
    do
      let a <- k True
      let b <- k False
      pure a
```

Both should fail.

---

## 3. Multi-shot safety is too syntactic and too narrow

The spec requires multi-shot effect handling to reject continuation capture when live quantity-1 or borrowed values would be duplicated. Diagnostics should identify:

* the operation
* the handler boundary
* at least one captured value
* the cause

The current resource checker has some logic for this, but it appears too local and syntax-driven.

Relevant areas:

* `ResourceChecking.fs`, backend capability check
* `tryFindMultiShotScopedOperation`
* continuation capture hazard analysis in `DoBind`

The current implementation recognizes only a limited set of operation call shapes. It appears focused on cases like:

```kappa
let x <- Choice.choose ()
```

But it is much less clear that it catches:

```kappa
let choose = Choice.choose
let x <- choose ()
```

or:

```kappa
let pkg = { label = Choice }
let x <- pkg.label.choose ()
```

or qualified/imported forms:

```kappa
let x <- lib.Choice.choose ()
```

or operation invocations hidden inside larger expressions.

Even more importantly, the continuation capture check seems tied to `DoBind` over a syntactically recognized operation. That is not enough for a language with first-class labels, rebound operations, records, imports, and nested effectful expressions.

### Fix

Represent operation invocation semantically before resource checking:

```fsharp
type ResolvedOperationInvocation =
  { LabelId: EffectLabelId
    InterfaceId: EffectInterfaceId
    OperationName: string
    ResumptionQuantity: Quantity
    ArgumentTypes: Type list
    ResultType: Type }
```

Then resource checking should inspect this node regardless of whether the source wrote:

```kappa
Choice.choose ()
```

or:

```kappa
let c = Choice.choose
c ()
```

or:

```kappa
pkg.label.choose ()
```

or some other syntactic disguise, because apparently programmers insist on naming things and then using the names.

Also, continuation capture analysis should be tied to the **nearest dynamically relevant handler boundary**, not merely the lexical remainder of a `do` block.

### Tests to add

* Multi-shot operation through alias.
* Multi-shot operation through record-carried label.
* Multi-shot operation through qualified import.
* Multi-shot operation not directly in `let x <- Op.foo ()` shape.
* Multi-shot operation inside nested function application.
* Multi-shot capture across `using`, `defer`, borrowed values, and linear values.

---

## 4. Backend support for effects is not implemented

This is not subtle.

`KBackendLowering.fs` rejects effect labels, effect operations, and handlers with messages equivalent to “backend does not support effect handlers yet.”

Similarly, the IL backend rejects these constructs.

Relevant areas:

* `KBackendLowering.fs`
* `IlDotNetBackendEmit.fs`
* `IlDotNetBackendTyping.fs`
* `CompilationMetadata.fs`

So the current implementation is essentially:

* parse effects
* elaborate effects
* resource-check some effects
* interpret effects
* reject effects for backend compilation

That is acceptable if documented as an interpreter milestone. It is not acceptable if users expect compiled effect handlers.

### Fix

Either:

1. explicitly classify effect handlers as **interpreter-only** for now, or
2. implement a backend lowering strategy.

Possible backend strategies:

* CPS transform effectful functions
* free monad / freer monad encoding
* delimited continuation runtime support
* stackful coroutine/fiber support
* exception-like unwinding plus resumable continuation objects
* whole-program transformation for known handlers

Given the existing design, CPS or an explicit `Eff` runtime representation is probably the most coherent first backend path. Native delimited continuations are powerful but tend to summon demons from both the optimizer and the garbage collector.

---

# Spec mismatches and likely bugs

## 5. The implementation mostly assumes self-label rows

The spec allows rows like:

```kappa
<[label : State]>
```

where `label` is an effect label whose interface is `State`.

But the implementation repeatedly appears to assume the label name and effect interface name are the same:

```kappa
<[State : State]>
```

Relevant areas:

* `SurfaceElaboration.fs`

  * `scopedEffectRowType`
  * `trySplitHandledEffectRow`
  * operation invocation typing
* row splitting logic compares label/effect names structurally

This weakens the first-class-label story. If labels are real values, the effect row should distinguish:

* the label identity
* the effect interface
* the operations supplied by that interface

Right now those concepts are still partially fused.

### Fix

Effect row entries should be semantic:

```fsharp
type EffectRowEntry =
  { LabelId: EffectLabelId
    LabelName: string
    InterfaceId: EffectInterfaceId
    InterfaceName: string }
```

The type checker may display names, but matching should use IDs.

A handler removes or transforms the row entry with the matching `LabelId`, not the matching string.

---

## 6. Borrowed operation resumption quantities appear to be accepted

The spec says borrowed `&` should be disallowed for effect operation resumption quantity.

But `CoreParsing.fs` appears to parse borrowed quantity prefixes for operation declarations:

```kappa
& op : A -> B
&[r] op : A -> B
```

I did not find a validation pass that rejects this specifically for effect operations.

That means this may currently parse:

```kappa
scoped effect Bad =
  & ask : Unit -> Bool
```

But according to the spec, it should be rejected.

### Fix

Add a declaration validation step:

```fsharp
match operation.ResumptionQuantity with
| Some (QuantityBorrow _) ->
    error "Effect operation resumptions cannot use borrowed quantity."
| _ -> ...
```

Do this after parsing, not only in the parser, because diagnostics are usually better once you know you are validating an effect operation.

### Test to add

```kappa
scoped effect Bad =
  & ask : Unit -> Bool
```

Expected: diagnostic rejecting borrowed effect operation resumption quantity.

---

## 7. Duplicate `case return` clauses may be silently mishandled

The parser separates the return clause from operation clauses. It appears to select the first `case return` and then validate operation clauses afterward.

That creates a likely bug:

```kappa
handle Ask comp with
  case return x -> pure x
  case return y -> pure y
  case ask () k -> k True
```

This should be rejected as a duplicate return clause.

Instead, the second return clause may be dropped from the operation-clause validation path or mishandled.

### Fix

Preserve all parsed clauses initially:

```fsharp
type ParsedHandlerClause =
  | ParsedReturnClause of ...
  | ParsedOperationClause of ...
```

Then validate:

* exactly one return clause
* no duplicate operation clauses
* no unknown operations
* all operations covered
* return clause has exactly one binder

Do not “helpfully” choose the first return clause. Helpful compiler recovery is how bugs put on a little hat and pretend to be features.

---

## 8. Return clause arity is not clearly enforced

The spec requires a return clause of the shape:

```kappa
case return x -> ...
```

But the parser appears to accept grouped arguments more generally.

This should be rejected:

```kappa
case return x y -> ...
```

or:

```kappa
case return -> ...
```

unless the grammar explicitly allows unit return binders, which the spec does not suggest.

### Fix

Validate return clause arity directly:

```fsharp
case return payload -> body
```

must have exactly one payload binder.

### Tests to add

```kappa
case return -> pure ()
```

```kappa
case return x y -> pure x
```

Both should fail.

---

## 9. Local scoped effect declarations ignore header parameters

The spec permits declarations like:

```kappa
scoped effect State (s : Type) =
  get : Unit -> s
  put : s -> Unit
```

But the scoped-effect parser appears to handle only:

```kappa
scoped effect Name = ...
```

and sets header tokens to empty.

That means local scoped effects probably cannot express parameterized effect interfaces even though the AST has `HeaderTokens`.

Relevant area:

* `CoreParsing.fs`, scoped effect parsing

### Fix

Parse local scoped-effect headers the same way top-level effect headers are parsed. Better yet, do not maintain two divergent parsers for essentially the same declaration form. Divergent parsers are where language features go to be selectively murdered.

---

## 10. Top-level effect import/visibility handling looks wrong

Top-level effects are indexed and imported through `SurfaceElaboration.fs`.

I saw two likely issues:

### Private effect declarations may leak

The effect declaration index appears to collect all top-level effect declarations regardless of visibility.

Then qualified imports may expose them.

That means this kind of thing may accidentally work:

```kappa
module Lib

private effect Secret =
  reveal : Unit -> Int
```

```kappa
module Main

import Lib

let x = Lib.Secret.reveal ()
```

If `Secret` is private, it should not be importable.

### Selective and wildcard imports may not import effect declarations

The implementation path I saw specifically handles qualified-only imports. It may ignore:

```kappa
import Lib.*
```

or:

```kappa
import Lib.(Ask)
```

for effects.

### Fix

The surface index should store visibility with effect declarations. Import resolution should apply the same rules to effects that it applies to types, values, and modules:

* qualified import
* wildcard import
* selective import
* aliases
* visibility filtering

### Tests to add

* private effect is not importable
* public effect is importable qualified
* public effect is importable through wildcard
* public effect is importable through selective import
* private effect stays private under all import forms

---

# Type-system and elaboration limitations

## 11. Operation signatures are still too token-based

`EffectOperation.SignatureTokens` preserves the operation signature as tokens. Handler arguments are similarly represented as token groups rather than a fully elaborated dependent telescope.

That is not enough for the spec’s more ambitious operation signatures.

The spec says operation signatures may be dependently typed Pi types. Handler typing must substitute operation arguments into the operation result type.

For example, conceptually:

```kappa
effect Choose =
  choose : (n : Nat) -> Fin n
```

Then in a handler clause:

```kappa
case choose n k -> ...
```

the resumption should know:

```kappa
k : Fin n -> ...
```

That requires the compiler to parse and elaborate the operation signature into:

```fsharp
type EffectOperationSignature =
  { Parameters: Telescope
    ResultType: Type
    ResumptionQuantity: Quantity }
```

Right now, much of the handler typing seems based on splitting function parts and counting arguments. That is enough for `Unit -> Bool`. It is not enough for dependent signatures.

### Fix

Lower every effect operation declaration to a typed operation signature during declaration elaboration.

Do not keep handler argument structure as token trivia. Preserve:

* binder names
* quantities
* implicit/explicit status
* parameter types
* result type
* dependencies between result and parameters

Then handler clause checking should instantiate the operation result type using the actual clause binders.

---

## 12. Rebound operation values are suspicious

The spec implies operations can be selected and rebound:

```kappa
let ask = Ask.ask
let b <- ask ()
```

At runtime, operation selection appears to produce a native function that issues an operation request. But there are two concerns.

### Static typing may lose the `Eff` wrapper

When applying `Ask.ask` directly, there is special typing logic that can infer:

```kappa
Eff <[Ask : Ask]> Bool
```

But when the operation is rebound to a local variable, ordinary function application may see only the raw operation signature:

```kappa
Unit -> Bool
```

instead of:

```kappa
Unit -> Eff <[Ask : Ask]> Bool
```

I did not prove this from execution, but the static shape strongly suggests a risk.

### Runtime arity may break multi-argument operations

Direct runtime application of a `KRuntimeEffectOperation` seems to handle arbitrary argument lists.

But when an operation is evaluated as a value, it becomes a native function with hard-coded arity `1`.

That is suspicious for:

```kappa
scoped effect Pairing =
  pair : Int -> Bool -> String

let p = Pairing.pair
let s <- p 1 True
```

Direct call may work. Rebound call may not.

### Fix

Represent operation values explicitly, not as generic native functions with guessed arity.

For example:

```fsharp
type RuntimeValue =
  | OperationValue of RuntimeEffectOperation
  | ClosureValue of ...
  | NativeFunctionValue of ...
```

Then application can handle operation values using the declared operation telescope.

### Tests to add

```kappa
let ask = Ask.ask
let b <- ask ()
```

```kappa
let pair = Pairing.pair
let x <- pair 1 True
```

Both should typecheck and run.

---

# Handler semantics

## 13. Shallow vs deep handler runtime behavior is mostly reasonable

The interpreter appears to do the right conceptual thing:

* shallow handler resumptions continue without automatically reinstalling the same handler
* deep handler resumptions re-enter the handler
* unmatched operations are propagated outward while preserving the current handler around the continuation

That propagation behavior is one of the trickier parts, and the implementation is directionally sound.

The main concern is not the shallow/deep algorithm itself. The main concern is the identity and resource metadata feeding it.

Good semantics with bad identity is like a courtroom with excellent procedure and witnesses named “Bob” matched by vibes.

---

## 14. Runtime has no one-shot fail-safe

The interpreter does not appear to enforce one-shot resumption consumption at runtime.

If static checking misses an overuse, the runtime likely permits it.

In a perfect compiler, static checking catches everything. In the world we actually occupy, where parsers accept borrowed effect quantities and aliases sneak past resource logic, a runtime fail-safe would be useful.

### Fix

Represent resumptions with consumption state when quantity is one-shot:

```fsharp
type ResumptionState =
  | Reusable
  | OneShot of consumed: ref<bool>
```

Then calling a consumed one-shot resumption should fail loudly.

This is not a substitute for static checking. It is a guardrail for compiler bugs, and compiler bugs are not theoretical. They are just undocumented collaborators.

---

# Backend/compiler architecture

## 15. Resource checking should operate after semantic effect resolution

The core architectural problem is that effect identity is resolved in some places and rediscovered syntactically in others.

That creates duplicated logic like:

* direct label handling
* alias handling
* record-carried label handling
* qualified-import handling
* member-access handling
* operation-call recognition

Every duplicated resolver will miss a case. The current code already appears to.

### Better architecture

Use phases like this:

```text
Surface syntax
  ↓ parse
Parsed AST
  ↓ name/effect resolution
Resolved Surface AST
  ↓ typechecking/resource checking
Typed Core
  ↓ lowering
Runtime IR / Backend IR
```

The resolved AST should contain semantic effect references:

```fsharp
type ResolvedExpression =
  | ResolvedEffectLabel of EffectLabelId
  | ResolvedEffectOperation of EffectLabelId * OperationName
  | ResolvedHandle of EffectLabelId * ...
```

After this phase, no later pass should need to ask:

> Is this syntax `Name [x]`, `KindQualifiedName`, `MemberAccess`, or some eldritch little record path?

It should already know the label identity.

---

# Missing tests I would add immediately

These are the tests most likely to expose real bugs.

## One-shot resumption through alias

```kappa
scoped effect Ask =
  ask : Unit -> Bool

let l = Ask

handle l comp with
  case return x -> pure x
  case ask () k ->
    do
      let a <- k True
      let b <- k False
      pure a
```

Expected: reject.

## One-shot resumption through record-carried label

```kappa
let pkg = { label = Ask }

handle pkg.label comp with
  case return x -> pure x
  case ask () k ->
    do
      let a <- k True
      let b <- k False
      pure a
```

Expected: reject.

## Duplicate return clause

```kappa
handle Ask comp with
  case return x -> pure x
  case return y -> pure y
  case ask () k -> k True
```

Expected: reject duplicate return clause.

## Invalid return clause arity

```kappa
handle Ask comp with
  case return x y -> pure x
  case ask () k -> k True
```

Expected: reject.

## Borrowed effect operation quantity

```kappa
scoped effect Bad =
  & ask : Unit -> Bool
```

Expected: reject.

## Rebound zero-argument operation

```kappa
let ask = Ask.ask
let b <- ask ()
```

Expected: typechecks and invokes `Ask.ask`.

## Rebound multi-argument operation

```kappa
scoped effect Pairing =
  pair : Int -> Bool -> String

let p = Pairing.pair
let s <- p 1 True
```

Expected: typechecks and invokes `Pairing.pair`.

## Distinct same-named scoped effects

```kappa
scoped effect E =
  op : Unit -> Int

let outer = E

scoped effect E =
  op : Unit -> Bool

-- handlers for inner E must not catch operations from outer E
```

Expected: handlers distinguish labels by identity, not spelling.

## Private effect import leak

```kappa
module Lib

private effect Secret =
  reveal : Unit -> Int
```

```kappa
module Main

import Lib

let x <- Lib.Secret.reveal ()
```

Expected: reject due to visibility.

## Wildcard and selective effect imports

```kappa
import Lib.*
```

and:

```kappa
import Lib.(Ask)
```

Expected: public effects imported according to normal import rules.

## Non-self label row

Something equivalent to:

```kappa
let l = State

-- computation has row <[l : State]>
-- handler handles l, not State by name
```

Expected: handler removes the `l` row entry by label identity.

---

# Suggested repair plan

## Step 1: Introduce real effect identities

Add semantic IDs for:

* effect interfaces
* effect labels
* operation declarations

Stop comparing labels by string except for diagnostics.

This is the central repair. Everything else becomes less stupid afterward, which is a rare luxury in compiler work.

## Step 2: Resolve labels and operations before resource checking

Do not let resource checking infer effect identity from syntax.

Surface forms like:

```kappa
Ask.ask
l.ask
pkg.label.ask
Lib.Ask.ask
```

should all lower to the same resolved operation representation if they refer to the same operation.

## Step 3: Rework effect operation signatures

Replace token-preserved operation signatures with typed telescopes.

Needed for:

* dependent operation signatures
* correct handler argument binding
* correct resumption result type
* arity checking
* rebound operation values
* diagnostics

## Step 4: Harden handler validation

Validate:

* exactly one return clause
* return clause has exactly one binder
* no duplicate operation clauses
* no missing operation clauses
* no unexpected operation clauses
* operation clause arity matches declaration
* operation clause binders match operation telescope
* resumption binder quantity matches operation declaration

Some of this already exists. The gaps are mostly around return clauses and dependent signatures.

## Step 5: Add runtime one-shot fail-safes

Even if the type checker should catch resumption overuse, the interpreter should not blindly allow it when metadata says one-shot.

A runtime fail-safe would make missed static checks obvious instead of letting bugs masquerade as semantics.

## Step 6: Decide the backend story

Right now, effect handlers are interpreter-only.

That is fine if this is a milestone. It is not fine if the compiler claims general support.

The project needs an explicit choice:

* document effects as interpreter-only for now
* or lower `Eff` and handlers into backend-supported constructs

For a first compiled implementation, I would favor explicit CPS or an `Eff` runtime representation over trying to bolt resumable continuations onto the backend. One is ugly. The other is ugly and haunted.

---

# Bottom line

The effects implementation has a serious foundation, especially in the interpreter and the deep-handler lowering path. The tests are also better than ceremonial.

But the implementation is not yet sound against the spec. The core defect is that effect labels are not consistently semantic objects. They are sometimes names, sometimes syntax, sometimes runtime records, and sometimes imported declarations. That fragmentation infects handler matching, row splitting, resource checking, imports, aliases, and multi-shot safety.

The priority fix is not “add more cases.” That road ends in a switch statement large enough to develop weather.

The priority fix is:

```text
resolve effect labels once;
carry semantic IDs everywhere;
make resource checking and runtime consume those IDs.
```

Until then, effects work for the curated paths covered by tests, but the more first-class labels become, the more the current implementation will leak.
