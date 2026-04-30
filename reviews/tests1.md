Reviewed the compiler tests against `Spec.md` and the surrounding source. Static review only: `dotnet` is not installed in the container, so I could not execute the test suite. Humanity survives another day by making toolchains optional.

## Verdict

No, **not all tests make sense as spec-conformance tests**.

Most of the tests are useful regression tests for the current compiler implementation, but several encode behavior that conflicts with the written spec, or test implementation details while pretending to test normative language behavior. The biggest issue is not one rogue assertion. It is that the test suite mixes three different categories without clearly separating them:

1. **Spec conformance tests**
2. **Current compiler regression tests**
3. **Backend/toolchain integration tests**

That mixture makes the suite misleading. A test can be valuable and still be wrong as a spec test. Several are exactly that.

I found **326 `[<Fact>]` tests**, plus theories/fuzz-style tests. The main test files are:

* `Tests.fs`: 104 facts
* `ObservabilityTests.fs`: 83 facts
* `ResourceModelTests.fs`: 40 facts
* `MilestoneFourTests.fs`: 32 facts
* `CoreTests.fs`: 28 facts
* `IlBackendTests.fs`: 14 facts
* `BackendTests.fs`: 9 facts
* `MilestoneThreeTests.fs`: 10 facts
* `MilestoneTwoTests.fs`: 3 facts
* `ZigBackendTests.fs`: 3 facts

There are also fixture and fuzzball-inspired theories.

---

# High-priority problems

## 1. Many tests violate the spec’s explicit top-level signature rule

This is the largest spec mismatch.

`Spec.md` says:

* Every exported top-level `let` definition **must** have an explicit top-level signature.
* Non-exported, non-recursive top-level definitions may omit a signature.
* Modules export top-level names by default unless marked `@PrivateByDefault`.

That means examples like this should generally be invalid unless the module is private or the binding is non-exported:

```kappa
module main

let result = 42
```

But many tests compile exactly that shape and assert success.

Examples:

### `CoreTests.fs`

The test:

```fsharp
[<Fact>]
let ``interpreter evaluates imported functions and closures`` () =
```

uses source like:

```kappa
module math

let twice x = x * 2

module main

import math.*

let makeAdder x = fn y => x + y

let result =
    let addFive = makeAdder 5 in
    addFive (twice 10)
```

and then asserts:

```fsharp
Assert.False(workspace.HasErrors, diagnosticsToString workspace)
```

But under the spec, `twice`, `makeAdder`, and `result` are exported top-level definitions without explicit signatures.

### `BackendTests.fs`

The test:

```fsharp
[<Fact>]
let ``dotnet backend emits a managed project that runs`` () =
```

uses:

```kappa
module main

let twice x = x * 2
let result = twice 21
```

Again, exported top-level definitions without signatures.

### `ZigBackendTests.fs`

The Zig backend tests also use `let result = ...` without signatures.

### `FuzzballInspiredTests.fs`

The generated “valid” programs similarly generate unsigned top-level `let result = ...` forms.

### Why this matters

This is not a harmless style issue. The tests are implicitly saying:

> Top-level exported inference is allowed.

The spec says:

> It is not.

So either the spec is wrong, or the tests are wrong. Currently, the tests are enforcing implementation behavior that contradicts the written language contract.

### Recommended fix

Choose one:

```kappa
module main

result : Int
let result = 42
```

or mark implementation-style examples private where appropriate:

```kappa
@PrivateByDefault
module main

let result = 42
```

But do not let public top-level inference quietly pass in tests that claim to represent the spec. That is how languages rot, one convenient exception at a time.

---

## 2. Ambiguous imported names are tested as runtime/interpreter errors, but the spec makes them compile-time errors

`CoreTests.fs` contains:

```fsharp
[<Fact>]
let ``interpreter reports ambiguous imported names`` () =
```

The source imports two modules with the same ordinary name:

```kappa
module a

value : Int
let value = 1

module b

value : Int
let value = 2

module main

import a.*
import b.*

result : Int
let result = value
```

The test expects:

```fsharp
Assert.False(workspace.HasErrors, diagnosticsToString workspace)
```

and then expects the interpreter to report ambiguity.

That is wrong per spec.

`Spec.md` ordinary lexical lookup says that if more than one declaration survives filtering, the use is ambiguous, except for a special data-family case. This should be diagnosed during compilation/name resolution, not deferred to interpreter execution.

### Why this matters

The test currently encodes:

> Ambiguous names can reach runtime.

The spec says:

> Ambiguous names are rejected by lexical lookup.

That is a meaningful semantic difference.

### Recommended fix

Change this into a frontend diagnostic test. It should assert that compilation has errors, ideally with a dedicated diagnostic code such as `NameAmbiguous`.

Currently the diagnostic model appears to have `ImportAmbiguous` and `TraitInstanceAmbiguous`, but not a general ordinary-name ambiguity diagnostic. That gap is probably the real issue the test is accidentally hiding.

---

## 3. The implicit prelude constructor subset test contradicts the spec

`Tests.fs` has:

```fsharp
[<Fact>]
let ``implicit prelude import models the wildcard and constructor subset separately`` () =
```

The test expects the implicit prelude constructor subset to include names like:

```text
:&
Reusable
OneShot
QZero
QOne
QZeroOrOne
QOneOrMore
QZeroOrMore
QueryMode
```

But `Spec.md` says the implementation-inserted unqualified constructor import is the **exact fixed subset**:

```text
True
False
None
Some
Ok
Err
Nil
(::)
LT
EQ
GT
refl
```

The word “exact” is doing actual work there, unless we are now treating the spec as a mood board.

The implementation in `src/Kappa.Compiler/Stdlib.fs` appears to include the larger constructor set, and the test validates that implementation behavior. But that behavior does not match the written spec.

### Recommended fix

Either:

1. Update the spec to include the larger constructor subset, if that is intentional, or
2. Shrink `FixedPreludeConstructors` and the test expectations to match the spec.

Right now, the test is not a spec test. It is an implementation drift test.

---

## 4. The “normative minimum prelude surface” test is not actually testing the normative minimum

`Tests.fs` contains:

```fsharp
[<Fact>]
let ``bundled bootstrap prelude exposes the normative minimum surface and IO shape`` () =
```

This test is problematic in two directions.

### It requires things the spec does not require

The test requires `Char`.

But the spec says portable Kappa uses `UnicodeScalar`, and `Char` is only an optional deprecated compatibility alias.

So requiring `Char` as part of the normative minimum is wrong.

### It omits many things the spec does require

The spec’s normative prelude surface includes many types, traits, and terms that this test does not check.

Examples of required or apparently normative items missing from the test include many of:

```text
Byte
UnicodeScalar
Grapheme
Variant
SyntaxOrigin
ElabGoal
CoreCtx
Core
CoreEq
Symbol
OnceQuery
OptionalQuery
NonEmptyQuery
SingletonQuery
QueryCore
QueryUse
QueryCard
QueryMode
SizedArray
Eff
```

The trait surface also appears incomplete. The spec names things like:

```text
Zero
One
Add
Mul
CheckedSub
CheckedDiv
CheckedMod
AdditiveMonoid
Ring
Rangeable
BorrowIntoQuery
```

The term/operator surface is also only partially checked. Missing examples include:

```text
==
/=
compare
+
*
/
%
fromInteger
buildInterpolated
range
..
..<
throwError
catchError
release
nanos
show
```

Some of these may be covered elsewhere, but then this test should not claim to validate the normative minimum.

### Recommended fix

Rename the test to something honest, for example:

```text
bundled bootstrap prelude exposes selected implementation surface
```

or make it genuinely spec-driven by checking the actual required set from `Spec.md`.

The current name overclaims. Tests should not wear fake badges. That is how codebases get police unions.

---

## 5. Short-circuit boolean test uses checked division in a way that conflicts with the spec

`CoreTests.fs` has:

```fsharp
[<Fact>]
let ``interpreter short circuits boolean operators`` () =
```

with source equivalent to:

```kappa
result : Int
let result = if False && ((1 / 0) == 0) then 0 else 42
```

The test expects this to compile and evaluate to `42`.

The short-circuit part is reasonable: `False && rhs` should not evaluate `rhs`.

But the spec’s checked division rules say `/` requires proof that division is defined. Division by zero should not silently typecheck merely because it appears in a branch that is not evaluated at runtime.

So this test conflates two things:

1. Runtime short-circuiting
2. Static validity of the right-hand operand

A short-circuited RHS still has to be well-typed.

### Recommended fix

Use a well-typed RHS that would only fail if evaluated. For example, use a function call that triggers an interpreter-side failure, an effect, or some observable trap that is still valid code.

Do not use `1 / 0` unless the language intentionally has unchecked division. The spec says it does not.

---

## 6. “Type imports from modules with identical export shapes stay distinct” does not prove what it says

`Tests.fs` contains:

```fsharp
[<Fact>]
let ``type imports from modules with identical export shapes stay distinct`` () =
```

The tested source uses aliases like:

```kappa
module left
type T = Int

module right
type T = Int
```

Then it imports both and uses them as if they were distinct.

But transparent type aliases to `Int` are definitionally just `Int`. This does not prove distinct type identity. It proves that two aliases can both unfold to the same type and arithmetic still works. Not exactly a breakthrough; even JavaScript would manage this on a sober Tuesday.

### Recommended fix

If the goal is to test namespace/import identity, rename the test.

If the goal is to test distinct nominal identity, use `data T` in both modules and assert that mixing the two is rejected, or inspect semantic symbols/IDs directly.

As written, the test name is misleading.

---

## 7. Backend tests are useful, but many are implementation regression tests, not spec tests

Several backend tests assert exact implementation details:

### `IlBackendTests.fs`

These tests assert generated CLR details such as:

```text
Kappa.Generated.main
public static methods
CLR int64 return types
```

### `BackendTests.fs`

One test checks that generated managed project output contains:

```text
Assembly.LoadFrom
```

### `ZigBackendTests.fs`

These tests assert generated names and fragments such as:

```text
kappa_make_data
kappa_module_main_sumList
KTYPE_
List
```

and absence of:

```text
__KAPPA_
```

These can be valuable ABI/codegen regression tests. But they are not general language conformance tests.

The spec gives backend latitude. For example, the Zig backend can lower through C, Zig source, LLVM IR, or another internal form. The .NET backend needs CLR-compatible artifacts, not necessarily these exact strings.

### Recommended fix

Keep these tests, but classify them as backend implementation regression tests. Do not present them as spec conformance.

Also, many backend test source programs still have the explicit-signature problem described earlier.

---

## 8. URL import tests are partial staging tests, not full URL import conformance

The URL import tests in `Tests.fs` cover useful things:

* Package mode rejects unpinned URL imports.
* Package mode rejects unresolved ref pins without a lock.
* Package mode accepts SHA-256 syntax before eventually failing unresolved module lookup.
* Script mode permits unpinned/ref-pinned syntax but still eventually reports unresolved modules.

That is fine as far as it goes.

But the spec’s URL import section has more behavior than these tests validate:

* Pinned URL format
* Ref resolution
* Digest verification
* Digest mismatch behavior
* Script-mode transient locking
* Package-mode lock requirements
* Fetch/cache interaction

The current tests mostly validate parser/staging behavior in a compiler with no real resolver.

### Recommended fix

Label these as parser/import-policy staging tests, not full URL import conformance tests.

For real conformance, add resolver-mock tests that simulate:

```text
unpinned script import → resolved transient lock
package import with SHA pin → fetch and verify
digest mismatch → verification error
ref pin in package mode without lock → rejected
ref pin with lock → accepted if digest matches
```

---

## 9. Some negative tests assert weakly on “has errors” or message substrings

Several tests only check `workspace.HasErrors` or string fragments in diagnostics.

Examples:

### Host module tests

`Tests.fs` includes tests like:

```fsharp
[<Fact>]
let ``host dotnet imports are rejected for unsupported backend`` () =
```

and:

```fsharp
[<Fact>]
let ``host roots outside the reserved namespace are rejected`` () =
```

They assert message substrings like:

```text
host.dotnet
reserved host
```

But the diagnostic model has codes such as:

```fsharp
DiagnosticCode.HostModuleUnsupportedBackend
DiagnosticCode.HostModuleReservedRoot
```

The tests should assert those codes directly.

### Numeric literal instance test

A missing `FromInteger` instance test appears to assert only that there are errors.

That is too weak. A parser error, module error, or totally unrelated failure could make the test pass. This is the testing equivalent of declaring the patient cured because the monitor unplugged itself.

### Recommended fix

For negative tests, assert:

1. `workspace.HasErrors = true`
2. The expected `DiagnosticCode`
3. The expected span/module where practical
4. Only then optionally assert message text

Message text is a poor primary oracle.

---

## 10. Fixture and Zig integration tests do not make sense in this source archive as shipped

The archive’s `README.md` says fixture directories and scripts are excluded.

But the test project includes:

```xml
<None Include="Fixtures\**\*.kp" />
```

and there is a fixture discovery test:

```fsharp
[<Theory>]
[<MemberData(nameof HarnessFixtureDiscovery.RawFixtureCases, MemberType = typeof<HarnessFixtureDiscovery>)>]
let ``raw kp fixtures satisfy their assertions`` ...
```

`HarnessFixtureDiscovery.fs` looks for:

```fsharp
Path.Combine(__SOURCE_DIRECTORY__, "Fixtures")
```

If that directory is absent, it returns no cases.

There is an assertion inside the theory:

```fsharp
Assert.NotEmpty(fixtures)
```

but that assertion is unreachable if no theory cases are generated. Depending on xUnit behavior/settings, this either becomes an empty theory problem or silently tests nothing. Splendid little foot-gun.

Similarly, Zig CLI tests eventually rely on scripts such as:

```text
scripts/ensure-zig.sh
scripts/ensure-zig.ps1
```

but `scripts/` is excluded from the source archive.

### Recommended fix

For this source package:

* Remove fixture tests, or include fixtures.
* Remove Zig integration tests, or include scripts/tooling.
* Mark toolchain-dependent tests explicitly as integration tests and skip them when prerequisites are missing.
* Do not include tests that can degenerate into “zero cases executed.”

---

# Areas that mostly make sense

Not everything is a dumpster fire wearing a typechecker costume. A large portion of the suite is coherent.

## Parser and lexer tests

Most parser/lexer coverage appears reasonable:

* Import selectors
* URL import syntax
* Raw strings
* Prefixed interpolation
* Operator sections
* Fixity declarations
* Safe navigation / Elvis syntax
* Patterns
* Effect rows
* Syntax quotes
* Staged code

These broadly match the surface syntax in the spec.

## Resource model tests

The resource model tests are generally sensible:

* Quantity checking
* Borrow escape rejection
* Record field moves and repairs
* Linear capture
* QTT-style diagnostics
* Reuse / one-shot distinctions

These align with the ownership/resource sections of the spec at a conceptual level.

## Milestone three/four tests

The tests around:

* `using`
* `release`
* inout threading
* effects and handlers
* backend capability handling for multishot effects

are conceptually useful and mostly plausible as regression tests.

## Observability tests

The observability tests broadly match the spec’s observability surface:

* Named checkpoints
* JSON/sexpr dumps
* Trace steps
* Verification output
* Intermediate representation inspection

But many of them assert implementation-specific dump names, shapes, or strings. That is acceptable for compiler regression testing, not language conformance.

## Instance tests

The instance tests mostly align with the spec’s instance visibility and ambiguity model:

* Global instance visibility through module closure
* Ambiguity rejection for non-equivalent overlapping instances

One likely gap: I did not see strong coverage for the spec’s allowance that equivalent surviving instances may be accepted by hard hash / equivalence logic. That deserves a targeted test if the compiler intends to implement it.

---

# Recommended triage

## First: split the suite conceptually

Create explicit categories or naming conventions:

```text
SpecConformance.*
CompilerRegression.*
BackendRegression.*
Integration.*
```

Right now the tests blur those lines. That makes failures harder to interpret and successes easier to overvalue, the classic software engineering ritual of confusing green lights with truth.

## Second: fix the top-level signature mismatch

This is the most pervasive issue.

Either:

1. Update the spec to allow exported top-level inference, or
2. Update tests to add signatures / use `@PrivateByDefault`.

Given the spec is very explicit, I would fix the tests unless the language design has changed.

## Third: move ambiguous import detection into compilation

The ambiguous imported names test should expect a compiler diagnostic, not an interpreter failure.

Add or use a diagnostic along the lines of:

```fsharp
DiagnosticCode.NameAmbiguous
```

## Fourth: reconcile prelude tests with the spec

Fix both prelude issues:

* The implicit constructor subset should match the spec’s exact list, or the spec should be updated.
* The “normative minimum” test should either become genuinely normative or be renamed.

## Fifth: replace the short-circuit division-by-zero test

Test short-circuiting with a well-typed RHS that would fail only if evaluated.

Do not use statically invalid checked division as the RHS.

## Sixth: strengthen negative diagnostics

Prefer diagnostic code assertions over message substring checks.

Bad:

```fsharp
Assert.Contains("reserved host", diagnostics)
```

Better:

```fsharp
Assert.Contains(workspace.Diagnostics, fun d ->
    d.Code = DiagnosticCode.HostModuleReservedRoot)
```

## Seventh: clean the archive

The archive contains many `.trx` files under:

```text
tests/Kappa.Compiler.Tests/TestResults
```

These are historical run artifacts, not source. They add noise and size. I saw one result file showing many failures and another showing all passed, which is exactly the kind of stale archaeological sediment that makes test review more annoying than it needs to be.

Remove them from the source archive unless they are intentionally being used as golden observability artifacts. They do not appear to be.

---

# Bottom line

The suite is broadly useful, but it is not cleanly aligned with the spec.

The main actionable problems are:

1. **Widespread missing explicit signatures in tests that should fail under the spec.**
2. **Ambiguous imports are wrongly treated as runtime errors.**
3. **Prelude constructor subset test contradicts the spec.**
4. **Normative prelude surface test is neither complete nor fully normative.**
5. **Short-circuit test relies on division by zero that should be statically rejected.**
6. **Several backend tests are implementation ABI tests, not spec tests.**
7. **Fixture/Zig integration tests rely on files excluded from the archive.**
8. **Some negative tests use weak assertions and could pass for the wrong reason.**

My judgment: **keep most of the tests, but reclassify them and fix the spec contradictions.** The suite is not nonsense. It is just wearing three hats at once, and two of them are on fire.
