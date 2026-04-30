## Verdict

The implementation is **not conformant** to the macro, staging, or elaboration-time evaluation spec. It has a respectable amount of **syntax recognition and type-shape plumbing**, but the main semantic machinery is missing. In less polite terms: the parser has learned the ritual words, but nobody summoned the demon.

I could not run the test suite because `dotnet` is not installed in this container, so this is a static review of the uploaded source. Confidence is high for the structural findings, because the relevant paths are explicit.

---

## What the compiler currently does implement

### 1. Surface syntax is recognized

The parser and AST have nodes for the required forms:

* `'{ ... }`
* `${ ... }`
* `$(...)`
* `.< ... >.`
* `.~...`

Relevant locations:

* `src/Kappa.Compiler/Syntax.fs:348-352`
* `src/Kappa.Compiler/CoreParsing.fs:1339-1343`
* `src/Kappa.Compiler/CoreParsing.fs:5734-5737`
* `src/Kappa.Compiler/CoreParsing.fs:5800-5801`
* `src/Kappa.Compiler/CoreParsing.fs:5892-5903`
* `src/Kappa.Compiler/KCore.fs:45-49`

That part broadly matches the spec’s syntax surface in `Spec.md:8519-8628` and `Spec.md:9397-9432`.

### 2. Basic type-shape checking exists

The compiler recognizes:

* `Syntax t`
* `Elab (Syntax t)`
* `Code t`

For example:

* `tryUnwrapElabType`, `tryUnwrapSyntaxType`, and `tryUnwrapTopLevelSyntaxSpliceType` are in `SurfaceElaboration.fs:3837-3856`.
* `SyntaxQuote`, `SyntaxSplice`, `TopLevelSyntaxSplice`, `CodeQuote`, and `CodeSplice` get approximate inferred types in `SurfaceElaboration.fs:6060-6075`.

This partially lines up with:

* `Syntax t` in `Spec.md:8524-8540`
* top-level splicing in `Spec.md:8577-8604`
* staging in `Spec.md:9403-9432`

### 3. Quote-local splices correctly reject `Elab (Syntax t)`

Inside a syntax quote, `${...}` requires `Syntax t`; it does not implicitly run `Elab (Syntax t)`. The compiler has a diagnostic for that exact case:

* `SurfaceElaboration.fs:11415-11439`

That matches the spec:

* `Spec.md:8560-8563`

This is one of the few areas where the implementation is pretty directly aligned.

### 4. Prefixed strings are routed through macro-looking KCore

Interpolated macro strings lower into a `KCoreTopLevelSyntaxSplice` around an `InterpolatedMacro.buildInterpolated` trait call:

* `SurfaceElaboration.fs:18140-18205`
* tested in `tests/Kappa.Compiler.Tests/Tests.fs:2736-2797`

This is useful scaffolding. It is not, by itself, an elaboration-time macro evaluator.

### 5. There are some direct-splice resource checks

The tests include:

* unspliced quote defers linearity checking: `tests/.../Tests.fs:2798-2814`
* direct top-level splice charges duplicated linear use: `tests/.../Tests.fs:2816-2831`

That tracks one important rule from the spec:

* `Spec.md:8606-8619`

But the implementation only catches a narrow syntactic case. It does not prove that arbitrary macro-expanded syntax is re-elaborated and rechecked after running an `Elab` action.

---

## Major conformance failures

## 1. There is no real elaboration-time evaluator

The spec says:

> `Elab a` is an action run by the elaboration-time evaluator.

See:

* `Spec.md:9193-9202`
* `Spec.md:9301-9372`

And top-level splices must execute the action at the splice site:

* `Spec.md:8606-8613`

The implementation does not appear to have such an evaluator. The actual compilation pipeline goes:

```text
parse / validate / resource check
→ lower KCore
→ lower KRuntimeIR
→ lower backend IR
```

Relevant location:

* `src/Kappa.Compiler/Compilation.fs:100-140`

There is a `PipelineTraceEvent.EvaluateElaboration` and a `MacroExpansionUnit`, but these are metadata/signalling shells:

* `CompilationPipeline.fs:260-280`
* `CompilationMetadata.fs:590-598`

The macro expansion unit has no fingerprints and no observable evaluator output. Splendid paperwork, no government.

### Consequence

A spec example like:

```kappa
myMacro : Syntax Int -> Elab (Syntax Int)
let myMacro e =
    pure '{ ${e} + 1 }

let x = $(myMacro '{ 10 })
```

should elaborate as generated object code equivalent to:

```kappa
let x = 10 + 1
```

The current compiler instead preserves/lower-wraps the expression structurally and eventually runtime-lowers the inner expression. It does not execute `myMacro` during elaboration and then elaborate the produced `Syntax`.

---

## 2. `$(...)` is not implemented as splice expansion

The spec requires:

* run `Elab`
* produce `Syntax t`
* elaborate produced syntax at splice site
* re-run normal obligations, including quantity, borrow, region, opacity, implicit insertion, etc.

See:

* `Spec.md:8606-8619`

Implementation:

* `TopLevelSyntaxSplice` type-checks as either `Syntax t` or `Elab (Syntax t)` in `SurfaceElaboration.fs:3837-3856` and `SurfaceElaboration.fs:6066-6068`.
* It lowers structurally to `KCoreTopLevelSyntaxSplice` in `SurfaceElaboration.fs:18738-18739`.
* Runtime lowering either special-cases prefixed strings or just lowers the inner expression in `KRuntimeLowering.fs:446-460`.

That is not macro expansion. That is “we put a ceremonial wrapper around it, then mostly ignored it.” A classic software architecture move, very dignified.

---

## 3. Hygiene is not implemented to spec

The spec requires every `Syntax t` value to carry hidden syntax-scope metadata covering:

* local variables
* globals
* implicit evidence
* quantity capabilities
* borrow regions
* captures
* local nominal tokens
* origin info

See:

* `Spec.md:8670-8706`

The compiler has a name-based escape diagnostic:

* `SurfaceElaboration.fs:7970-8370`

It detects some captured visible names escaping through `Syntax`. That is useful, but far weaker than the required hygiene model.

The implementation does **not** appear to have an actual `Syntax` value representation containing hidden scope metadata. It also recursively substitutes through syntax and code quote nodes:

* `SurfaceElaboration.fs:18522-18531`
* `SurfaceElaboration.fs:20142-20151`

That is a hygiene red flag. Substitution crossing quotation boundaries blindly is exactly how generated-code systems develop haunted-house scoping bugs.

### Spec gap

Missing or incomplete:

* hygienic binder identity
* generated binder alpha-renaming
* splice-site compatibility checks for type, quantity, region, capture, visibility, opacity
* source/synthetic origin preservation
* borrow-region escape through syntax metadata
* nominal-scope token escape through syntax metadata

The current approach is a lexical-name approximation, not spec-level hygiene.

---

## 4. Surface `Syntax` inspection API is missing

The spec requires either quotation-pattern matching or a stable public `Syntax` inspection API:

* `Spec.md:8731-8755`

The prelude exposes only a few related declarations:

* `syntaxOrigin`
* `renderSyntax`
* `normalizeSyntax`

See:

* `src/Kappa.Compiler/Stdlib/std/prelude.kp:309-311`

But the minimum spec coverage requires inspection of identifiers, literals, application spines, lambdas, lets, matches, records, variants, quotes, splices, comprehensions, and origins.

I found no conforming public API for that. A macro cannot portably inspect `Syntax` in the way the spec requires.

---

## 5. `withSyntaxOrigin` is missing

The spec requires:

```kappa
syntaxOrigin :
    forall (@0 t : Type).
    Syntax t -> Elab SyntaxOrigin

withSyntaxOrigin :
    forall (@0 t : Type).
    SyntaxOrigin -> Syntax t -> Elab (Syntax t)
```

See:

* `Spec.md:8757-8769`

The prelude has `syntaxOrigin`, but not `withSyntaxOrigin`:

* `src/Kappa.Compiler/Stdlib/std/prelude.kp:309-315`

So origin support is incomplete even at the declaration level.

---

## 6. Semantic reflection is essentially absent

The spec requires:

```kappa
CoreCtx : Type
Core    : CoreCtx -> Type -> Type
CoreEq  : ...
Symbol  : Type
```

and operations including:

* `asCore`
* `asCoreIn`
* `reifyCore`
* `inferType`
* `whnf`
* `normalize`
* `tryProveDefEq`
* `proveDefEq`
* `defEq`
* `headSymbol`
* `sameSymbol`

See:

* `Spec.md:8780-8912`

I did not find these in the prelude. The prelude has:

* `Syntax`
* `SyntaxOrigin`
* `Elab`
* `ElabGoal`

but not the required semantic reflection types and operations:

* `src/Kappa.Compiler/Stdlib/std/prelude.kp:124-127`
* `src/Kappa.Compiler/Stdlib/std/prelude.kp:309-319`

This is a hard spec failure.

---

## 7. `Elab` is declared but not semantically implemented

The prelude declares:

* `Elab`
* `ElabGoal`
* `failElab`
* `failElabWith`
* `warnElab`
* `warnElabWith`

See:

* `src/Kappa.Compiler/Stdlib/std/prelude.kp:124-127`
* `src/Kappa.Compiler/Stdlib/std/prelude.kp:312-315`

But the spec also requires:

* `currentGoal`
* structured diagnostic types
* `emitElabDiagnostic`
* `failElabDiagnostic`
* `asCoreGoal`
* `withExpectedGoal`
* `ensureDefEqGoal`
* coherent `Functor Elab`, `Applicative Elab`, `Monad Elab`

See:

* `Spec.md:9208-9299`

These are missing or not meaningfully implemented.

There is some ad hoc recognition of `pure` and some `Elab`-available names:

* `SurfaceElaboration.fs:7646-7665`
* `IntrinsicCatalog.fs:78-93`

But that is not a coherent `Elab` monad. It is a small whitelist with a trench coat.

---

## 8. Phase separation is heuristic, not spec-level

The spec says:

* `Elab` bodies are checked in the meta phase.
* object runtime values cannot be directly captured
* object terms enter elaboration only through carriers like `Syntax`, `Core`, etc.
* `pure` for `Elab` lifts only meta-phase values

See:

* `Spec.md:9301-9312`

The compiler has a diagnostic pass trying to catch direct runtime arguments to Elab-ish calls:

* `SurfaceElaboration.fs:11855-11976`

But the detection is based on recognizing callable result types and checking whether arguments are “meta phase transfer types.” That is not a full phase system.

Worse, `typeIsMetaPhaseTransferType` includes `Code` and `ClosedCode`-style staging carriers as meta-ish in places, while the staging spec treats `Code` as generative staged code, not inspectable macro syntax and not compile-time-only in the same sense as `Syntax`.

This area is under-specified in the implementation and likely unsound once real macros exist.

---

## 9. Staging is badly wrong

The spec for staging requires:

* `Code t` as typed generative staged code
* `ClosedCode t`
* `liftCode`
* `closeCode`
* `genlet`
* `runCode`
* scope safety
* closedness checks
* `genlet` sharing
* no inspection-as-syntax

See:

* `Spec.md:9397-9557`

The implementation has parser and AST nodes for staging, but the runtime lowering erases the distinction:

```fsharp
| KCoreSyntaxQuote inner
| KCoreSyntaxSplice inner
| KCoreCodeQuote inner
| KCoreCodeSplice inner ->
    lowerKRuntimeExpression runtimeParameterMasks inner
```

Location:

* `src/Kappa.Compiler/KRuntimeLowering.fs:461-465`

That means:

```kappa
.< 1 + 2 >.
```

does not become a code value. It lowers as the inner expression.

That violates the core meaning of `Code`.

### Additional staging problems

`Code` is treated as compile-time/erased in runtime type erasure:

* `KRuntimeLowering.fs:58-72`
* `KRuntimeLowering.fs:194-209`

But the spec says `Code t` is staged code, not `Syntax t`, and `runCode : ClosedCode t -> UIO t` is a runtime operation over closed generated code.

The prelude declares:

* `closeCode`
* `genlet`
* `runCode`

at:

* `src/Kappa.Compiler/Stdlib/std/prelude.kp:257-259`

and the intrinsic catalog names them:

* `IntrinsicCatalog.fs:135-137`

But I found no implementation of their required semantics:

* no closedness checker
* no scope extrusion metadata
* no `genlet` let-insertion logic
* no `runCode` restriction to closed code beyond the type declaration
* no generative code representation

This is the largest staging failure. Currently staging is syntax-shaped decoration that gets flattened away. Very efficient, in the same way deleting the engine makes a car lighter.

---

## 10. Incremental macro-expansion metadata is decorative

The compiler has:

* `MacroExpansionUnit`
* trace event `EvaluateElaboration`

Locations:

* `CompilationPipeline.fs:224-242`
* `CompilationPipeline.fs:260-280`
* `CompilationMetadata.fs:590-598`

But the actual compilation path does not execute macro expansion between frontend and KCore lowering:

* `Compilation.fs:100-140`

Also, the macro expansion incremental unit has no fingerprints:

```fsharp
(Some "macro-expansion")
[]
[ headerUnit.Id ]
```

Location:

* `CompilationMetadata.fs:590-598`

Given the spec’s determinism, transcript, import-stability, and elaboration-time execution requirements, this is not enough for separate-compilation correctness.

---

## Conformance matrix

| Area                        | Spec expectation                                                   |                Current implementation | Status       |
| --------------------------- | ------------------------------------------------------------------ | ------------------------------------: | ------------ |
| Syntax quote parsing        | Parse `'{ ... }`                                                   |                           Implemented | Partial pass |
| Quote-local splice          | `${s}` requires `Syntax t`, not `Elab`                             |                     Diagnostic exists | Mostly pass  |
| Top-level splice type shape | `Syntax t` sugar or `Elab (Syntax t)`                              |                 Type-shape recognized | Partial pass |
| Top-level splice semantics  | Execute `Elab`, elaborate generated syntax at site                 |                       Not implemented | Fail         |
| Macro functions             | Ordinary functions returning `Elab (Syntax t)`                     | Type shape possible, execution absent | Fail         |
| Hygiene                     | Hidden syntax-scope metadata, alpha-renaming, compatibility checks |         Name-based approximation only | Fail         |
| Surface syntax inspection   | Public inspection API or quote patterns                            |                               Missing | Fail         |
| Origins                     | `syntaxOrigin` and `withSyntaxOrigin`                              |          only `syntaxOrigin` declared | Fail         |
| Semantic reflection         | `CoreCtx`, `Core`, `CoreEq`, `Symbol`, operations                  |                               Missing | Fail         |
| `Elab` evaluator            | Compile-time evaluator and phase rules                             |               Missing / ad hoc checks | Fail         |
| `Elab` diagnostics          | structured diagnostics and goal APIs                               |                        Mostly missing | Fail         |
| Staged code quote           | `.< e >. : Code t` as generated code                               |           lowered to inner expression | Fail         |
| Code escape                 | `.~c` splices staged code into code quote                          |           lowered to inner expression | Fail         |
| `liftCode`                  | cross-stage persistence                                            |                   trait declared only | Fail         |
| `closeCode`                 | closedness check                                                   |                         declared only | Fail         |
| `genlet`                    | let insertion and sharing                                          |                         declared only | Fail         |
| `runCode`                   | execute only closed code                                           |                         declared only | Fail         |
| Macro expansion checkpoint  | meaningful expansion output and hashes                             |                        metadata shell | Fail         |

---

## Most important fixes

### 1. Add a real meta-phase evaluator

This is the foundation. Without it, `Elab` is only a type constructor with aspirations.

Needed:

* representation for meta-phase values
* evaluator for total/deterministic elaboration-time subset
* execution of `Elab (Syntax t)` at splice sites
* diagnostic emission from `failElab`, `warnElab`, structured diagnostics
* import and transcript tracking
* phase-separated environments

Until this exists, `$(...)` should probably reject `Elab (Syntax t)` splices instead of pretending they work.

### 2. Represent `Syntax` as an actual hygienic value

A conforming `SyntaxValue` needs at least:

```text
surface AST
+ hygienic binder IDs
+ lexical scope metadata
+ referenced global IDs
+ implicit evidence refs
+ quantity/borrow/region/capture metadata
+ source/synthetic origin chain
```

String names are not enough. Names are what humans use before inventing bugs.

### 3. Stop ordinary substitutions from crossing quote boundaries blindly

These paths are suspicious:

* `SurfaceElaboration.fs:18522-18531`
* `SurfaceElaboration.fs:20142-20151`

Substitution inside `SyntaxQuote` / `CodeQuote` must be phase-aware and hygiene-aware. Otherwise compiler rewrites can mutate quoted object programs incorrectly.

### 4. Implement actual splice expansion before final checking/lowering

Pipeline should roughly become:

```text
parse
→ resolve headers/imports
→ type-shape check macro definitions
→ evaluate elaboration-time splices
→ produce expanded syntax
→ elaborate expanded syntax at splice site
→ run ordinary resource/type/borrow/region/visibility checks
→ lower KCore
```

The current direct-lowering of `KCoreTopLevelSyntaxSplice` should not survive into runtime lowering except perhaps in debug dumps.

### 5. Implement reflection APIs or remove claims of support

The prelude needs the required declarations and the evaluator needs primitive support for them:

* `CoreCtx`
* `Core`
* `CoreEq`
* `Symbol`
* `asCore`
* `reifyCore`
* `inferType`
* `whnf`
* `normalize`
* `tryProveDefEq`
* `headSymbol`
* etc.

The public API must be QTT-aware, per `Spec.md:8802-8828`.

### 6. Split staging from macros properly

`Code` must not be treated like `Syntax`.

Needed:

* a `CodeValue` representation distinct from `SyntaxValue`
* quotation contexts
* escape handling
* lift-based cross-stage persistence
* scope metadata
* closedness checking
* `genlet` sharing
* `runCode` execution only for `ClosedCode`

These lowering rules are currently unacceptable:

* `KRuntimeLowering.fs:461-465`

They erase the entire staging model.

### 7. Fix compile-time erasure classification

`KRuntimeLowering.fs:58-72` and `KRuntimeLowering.fs:194-209` erase/classify `Code` as compile-time. That contradicts the staging contract.

Also, the compile-time-only list is inconsistent: it erases `Syntax` and `Code`, but not all the spec-required compile-time-only types like reflection carriers. Once those are added, this needs a principled phase classifier rather than a hard-coded shortlist.

### 8. Expand tests from parser-shape tests to semantic tests

Add tests for:

```kappa
myMacro : Syntax Int -> Elab (Syntax Int)
let myMacro e = pure '{ ${e} + 1 }

let x = $(myMacro '{ 10 })
```

Expected: generated object code behaves as `11`.

Also add tests for:

* macro-generated duplicated linear use rejected at splice site
* hygienic binder capture avoidance
* escaping `Syntax` containing borrow-region references rejected
* `withSyntaxOrigin`
* `currentGoal`
* `failElab` / `warnElab`
* `asCore` / `reifyCore`
* `.< ... >.` produces `Code`, not the inner value
* `.~` only works inside code quote
* `closeCode` rejects open code
* `genlet` shares generated binders
* `runCode` rejects non-closed code

---

## Bottom line

The current implementation has **front-end recognition** for the macro and staging syntax and a few useful diagnostics. It does **not** implement the spec’s actual semantic contract.

The most severe issues are:

1. **No elaboration-time evaluator.**
2. **No real macro expansion.**
3. **No hygienic `Syntax` value model.**
4. **No semantic reflection API.**
5. **Staging is erased into the inner expression, which is categorically wrong.**

So the accurate status is:

> **Macros/staging/Elab are parser-visible and partially type-shaped, but semantically mostly unimplemented.**

A lesser reviewer might call this “early scaffolding.” I shall call it what it is: a stage set with painted doors. Useful for orientation, dangerous if anyone tries to walk through it.
