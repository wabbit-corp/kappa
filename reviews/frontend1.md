## Verdict

I reviewed the frontend against `Spec.md` by static inspection of the extracted source. I could **not** run the compiler or tests because the container does not have `dotnet` installed:

```text
bash: dotnet: command not found
```

So this is not a build/test verdict. It is a spec-conformance review of the frontend source. Annoyingly, that still tells us quite a lot.

The frontend is **not spec-complete**. It is a serious milestone implementation with a real lexer, parser, import resolver, KFrontIR scaffolding, surface elaboration, and resource checking, but the spec describes a much larger language. Several `MUST` rules in the spec are either violated, only partially implemented, or silently treated as ordinary parse failures. The cathedral has scaffolding, but also some rooms that are just painted doors.

---

## Highest-priority findings

|        Severity | Area          | Finding                                                                                                            |
| --------------: | ------------- | ------------------------------------------------------------------------------------------------------------------ |
|        **High** | Modules/files | Path-derived module rules do not match the spec.                                                                   |
|        **High** | Imports       | `unhide` / `clarify` are parsed but not semantically implemented. URL imports mostly stop at pin validation.       |
|        **High** | Blocks / `do` | Spec-required local declarations and several control-flow items are missing from `do` blocks.                      |
|        **High** | Totality      | Totality/decreases checking is essentially not implemented as specified. Assertions are mostly gate checks.        |
|        **High** | Diagnostics   | Diagnostic records are much thinner than the spec requires, and many frontend diagnostics have no source location. |
| **Medium-High** | Lexing        | Identifier and keyword handling diverges from the lexical spec.                                                    |
|      **Medium** | Declarations  | Duplicate declaration checking misses several declaration categories.                                              |
|      **Medium** | Deriving      | `derive` exists as a keyword only; no parser/elaboration support found.                                            |

---

## 1. Module path mapping is non-conformant

The spec’s module/file mapping is precise. A source file must end in `.kp`; path segments, basename, and fragment segments must match ASCII identifier regex `[A-Za-z_][A-Za-z0-9_]*`; fragment segments are ignored for the module name but still must be validated; and case-folding collisions must be rejected with diagnostics identifying all files. See `Spec.md:132-155`.

The implementation does this in `CompilationFrontend.fs:28-62`:

```fsharp
if not (fullPath.EndsWith(".kp", StringComparison.OrdinalIgnoreCase)) then None
...
let fileStem = Path.GetFileNameWithoutExtension(relativePath)
let moduleStem =
    match fileStem.IndexOf('.') with
    | -1 -> fileStem
    | index -> fileStem.Substring(0, index)
...
if segments |> List.forall isValidModuleSegment then Some segments else None
```

Problems:

1. It accepts `.KP` / `.Kp` because it uses `OrdinalIgnoreCase`, while the spec says the extension is `.kp`.
2. It validates only the derived module segments, not the ignored fragment segments. So `main.bad-frag.kp` can infer module `main` instead of rejecting the invalid fragment.
3. `isValidModuleSegment` uses `SyntaxFacts.isIdentifierStart` / `isIdentifierPart`, which are Unicode-aware via `Char.IsLetter` and `Char.IsLetterOrDigit` (`Syntax.fs:755-758`). The spec requires ASCII for path-derived module segments.
4. I found no case-fold collision detection. Searching the frontend found only unrelated uses of `OrdinalIgnoreCase`, and no diagnostic/code path for module case collisions.
5. The under-root check uses `relativePath.StartsWith("..")` (`CompilationFrontend.fs:40-43`), which can falsely reject valid paths such as a directory named `..foo`. Human filesystem handling, the traditional ritual of sadness.

Recommended fix: centralize module path inference into one spec-facing function, make it ASCII-only, validate all fragment segments, use exact `.kp`, and add a pre-grouping case-fold collision pass over all source files.

---

## 2. Module attributes are under-validated

The spec says unknown module attributes are compile-time errors unless explicitly documented (`Spec.md:174-177`).

The parser collects module attributes at the top of the file (`Parser.fs:1855-1867`), but I found no general unknown-attribute validation. Known consumers include:

* `PrivateByDefault` in `CompilationFrontend.fs:711-719`
* `allow_assert_terminates` / `allow_assert_reducible` in `SurfaceElaboration.fs:14884-14897`

Everything else appears to pass through as inert metadata. That contradicts the spec unless those attributes are documented somewhere as implementation-defined, which I did not find in the frontend.

Also, a second `module` header after the first one is not diagnosed as “multiple module headers”; it falls into the ordinary top-level declaration parser and becomes a generic parse error. That technically produces an error, but not the spec-quality diagnostic the language contract implies.

---

## 3. Identifier and keyword handling drift from the lexical spec

The spec says standard identifiers are ASCII:

```text
[A-Za-z_][A-Za-z0-9_]*
```

See `Spec.md:4578-4584`.

The implementation accepts Unicode identifiers:

```fsharp
let isIdentifierStart character = Char.IsLetter(character) || character = '_'

let isIdentifierPart character =
    Char.IsLetterOrDigit(character) || character = '_'
```

That is `Syntax.fs:755-758`.

This may be intentional, since the frontend has Unicode diagnostics such as `UnicodeConfusableIdentifier`, `UnicodeNonNormalizedSourceText`, and scalar/grapheme/byte literal checks. But the spec and implementation disagree. Either the spec needs to permit Unicode standard identifiers, or the lexer must reject them except inside backtick identifiers.

Keyword handling is also inconsistent with the spec. The spec lists important soft/contextual keywords such as `exists`, `block`, `open`, `captures`, `force`, `decreases`, `structural`, `effect`, `handle`, `deep`, `with`, `scoped`, `get`, `set`, and `sink` (`Spec.md:4611-4629`). The implementation’s `Keyword` enum does not include many of these (`Syntax.fs:7-74`).

Some of these are handled contextually as ordinary identifiers, for example `block`, `effect`, `scoped`, and `captures`. That may work for parsing, but it violates the spec statement that the lexer recognizes keyword tokens while allowing them as ordinary identifiers in non-keyword contexts (`Spec.md:4634-4640`). Tooling that relies on the token stream will see a different language than the spec describes. Naturally, the machine is gaslighting the editor now.

---

## 4. Numeric literal scanning accepts malformed prefixes too leniently

The scanner handles `0x`, `0o`, and `0b` prefixes in `Syntax.fs:917-958`. If there are no valid digits after the prefix, it falls back to building a decimal integer `"0"` and leaves the prefix character behind:

```fsharp
| None ->
    buildInteger 10 "0" (startIndex + 1)
```

That means malformed literals like `0x`, `0b`, or `0o` are not necessarily lexed as malformed numeric literals. They can split into separate tokens and later produce misleading parse/name errors.

This should be a lexical or numeric-literal diagnostic, not a slow-motion failure several phases later. Add tests for:

```kappa
let a = 0x
let b = 0b102
let c = 0o89
let d = 123abc
```

Then decide which suffixes are valid and reject the rest with literal diagnostics.

---

## 5. Import parsing is broad, but import semantics are incomplete

The parser supports many import forms, including items, namespace selectors, `(..)`, aliases, wildcard imports, and `except` forms. That broadly tracks `Spec.md:203-260`.

However, several semantic gaps remain.

### `unhide` / `clarify`

The spec gives `unhide` and `clarify` real semantics: `unhide` requests access to private imported items, and `clarify` requests transparent treatment for opaque imported items (`Spec.md:437-445`).

The parser recognizes them and rejects duplicate modifiers (`Parser.fs:793-819`). But in semantic validation and export inventory construction, I found no meaningful use of `item.Modifiers`. The relevant validation checks item existence by public export inventory only (`CompilationFrontend.fs:1153-1169`, `1235-1335`). There is also a pretty-printer for modifiers (`CompilationFrontend.fs:273-304`), which is cute in the way a decorative steering wheel is cute.

Result: `unhide` and `clarify` are syntax without the specified semantics.

### URL imports

The spec supports URL imports and pins (`Spec.md:226-232`, later §2.3.2). The implementation validates pin policy in package mode (`CompilationFrontend.fs:1188-1201`) but does not appear to implement a provider/fetch/cache/resolution mechanism. URL imports then try to resolve against `exportInventories` by identity (`CompilationFrontend.fs:1266-1304`).

So URL imports are not really implemented as a source/module provider mechanism. They are partially parsed and partially validated.

---

## 6. `do` blocks are missing spec-required forms

The spec says `do` blocks may contain effectful items and also shared block-scope declarations: local signatures, named `let` definitions, `data`, `type`, `trait`, `scoped effect`, `instance`, `derive`, imports, and fixity declarations (`Spec.md:14677-14682`, `14785-14803`).

The implementation’s `SurfaceDoStatement` only has:

```fsharp
DoLet
DoLetQuestion
DoBind
DoVar
DoAssign
DoUsing
DoDefer
DoIf
DoWhile
DoReturn
DoExpression
```

That is `Syntax.fs:487-498`.

The parser’s `ParseDoLine` mirrors this limited set (`CoreParsing.fs:4323-4462`). There are no do-statement variants for local `data`, `type`, `trait`, `instance`, `derive`, `import`, fixity declarations, local signatures, or local named definitions. Those forms cannot be represented in the AST.

Also missing from `do`:

* `break`
* `continue`
* `for` loops
* labeled `return@L`, `break@L`, `continue@L`

The spec explicitly requires `break` and `continue` as do-items (`Spec.md:14684-14690`, `14785-14808`) and specifies `while` / `for` loop rules (`Spec.md:15154-15179`). The frontend has `Keyword.Break`, `Keyword.Continue`, and `Keyword.For`, but no corresponding do-statement AST cases and no parse branches for them in `ParseDoLine`.

That is a hard conformance gap.

---

## 7. `try` / `except` / `finally` are not implemented as surface expressions

The spec requires `try` / `except` / `finally` and `try match` syntax (`Spec.md:16340-16447`). The parser’s layout logic knows these words can introduce blocks (`Parser.fs:718-721`), but I found no `SurfaceExpression` cases for `Try`, `TryMatch`, `Except`, or `Finally` (`Syntax.fs:360-378`), and no expression parser branches implementing them.

The spec says `raise` is an ordinary prelude helper, not special syntax (`Spec.md:16452-16469`), so that part may be fine if the prelude provides it. But the actual `try` surface forms are absent.

---

## 8. `derive` is only a keyword

The spec has deriving syntax:

```kappa
derive Eq Foo
```

See `Spec.md:19212-19234`.

The implementation has `Keyword.Derive` in `Syntax.fs:21` and keyword text mapping at `Syntax.fs:93`, but searching the frontend found no parser or elaboration support for `derive`. It does not appear in `ParseTopLevelDeclaration` (`Parser.fs:1794-1841`), nor in local block/do declaration handling.

So `derive` is currently a reserved-looking word that does nothing useful. Language design: where hope goes to become a token.

---

## 9. Totality and `decreases` are far below spec

The spec says Kappa is total by default for transparent definitions (`Spec.md:10387-10403`), defines termination certification (`Spec.md:10406-10418`), structural descent (`Spec.md:10471-10486`), and explicit `decreases` clauses (`Spec.md:10641-10656`).

The implementation has totality assertion modifiers:

* `assertTerminates`
* `assertReducible`
* `assertTotal`

But the visible validation I found mostly checks whether unsafe assertion attributes are enabled:

```fsharp
let allowAssertTerminates = moduleHasAttribute frontendModule "allow_assert_terminates"
let allowAssertReducible = moduleHasAttribute frontendModule "allow_assert_reducible"
```

That is `SurfaceElaboration.fs:14884-14897`.

I did not find implementation of the specified `decreases` syntax or structural/well-founded termination checking. `decreases` and `structural` are not keywords in the lexer enum, and `ParseTopLevelDeclaration` has no totality/decreases grammar branch.

So the frontend currently supports assertion gates, not the spec’s totality system.

Confidence: **high** for missing `decreases` syntax and gate-only assertion checks. **Medium** for “no termination checking anywhere” because a complete proof would require a deeper audit of all elaboration paths, but the key public syntax and validation hooks are not there.

---

## 10. Duplicate declaration checking is incomplete

`SurfaceElaboration.fs:14839-14882` checks duplicates for:

* term definitions from `let`
* type declarations from `data` and `type`
* constructors from `data`

It does **not** appear to include all declaration kinds in duplicate checks, even though earlier code collects many top-level names (`SurfaceElaboration.fs:14640-14652`), including signatures, projections, effects, traits, and expects.

Likely missed or inconsistently handled cases include:

```kappa
trait C a = ...
trait C a = ...

effect E = ...
effect E = ...

projection foo ...
let foo = ...

x : Int
x : Bool
```

Some of these may fail later because maps overwrite or resolution becomes ambiguous, but the explicit duplicate declaration validator is not namespace-complete. Silent overwrite is not a compiler feature; it is a trap wearing a monocle.

---

## 11. Diagnostics do not meet the spec’s diagnostic contract

The spec requires structured diagnostics with schema version, code, family, severity, stage, phase, primary origin, labels, notes, helps, fixes, related origins, payload, explanation key, and suppressed summaries (`Spec.md:21662-21720`). It also requires source/synthetic origins with proper ranges (`Spec.md:21793-21870`) and fix-it structures (`Spec.md:21871-21930`).

The implementation’s `Diagnostic` is much smaller:

```fsharp
type Diagnostic =
    { Severity: DiagnosticSeverity
      Code: DiagnosticCode
      Stage: string option
      Phase: string option
      Message: string
      Location: SourceLocation option
      RelatedLocations: DiagnosticRelatedLocation list }
```

That is `Diagnostics.fs:314-321`.

That is not fatal internally, but it is not spec-conformant as the public diagnostic model. More seriously, many frontend diagnostics are created with `Location = None`:

```fsharp
let makeDiagnostic code message =
    { Severity = DiagnosticSeverity.Error
      Code = code
      Stage = Some "KFrontIR"
      Phase = Some(KFrontIRPhase.phaseName CORE_LOWERING)
      Message = message
      Location = None
      RelatedLocations = [] }
```

That is `SurfaceElaboration.fs:14665-14672`.

This directly conflicts with the spec’s requirement that diagnostics attach to source or synthetic origins (`Spec.md:21655-21660`). It also makes downstream tooling worse, because editors cannot underline a shrug.

Recommended fix: add spans/origins to AST/KFrontIR nodes, then make `makeDiagnostic` take an origin. For now, at minimum, carry declaration token spans through top-level declarations and use those for validation diagnostics.

---

## 12. KFrontIR phase scaffolding exists, but the spec model is not fully realized

The phase enum aligns with the spec:

```fsharp
RAW
IMPORTS
DECLARATION_SHAPES
HEADER_TYPES
STATUS
IMPLICIT_SIGNATURES
BODY_RESOLVE
MODAL_SOLVE
CHECKERS
CORE_LOWERING
```

That is `CompilationPipeline.fs:58-68`.

But the spec also requires lazy/query-level resolution, error-tolerant KFrontIR with placeholders, and diagnostics produced from KFrontIR checker facts (`Spec.md:21595-21660`). The current compilation flow is a mostly batch pipeline:

```fsharp
parseFile
reparseDocumentsWithImportedFixities
resolveImportExportSemantics
validateImportSelections
validateSurfaceModules
...
if frontendDiagnostics has errors then skip resource checking
```

See `Compilation.fs:79-118`.

That is a reasonable implementation strategy for a milestone compiler, but it is not the full tooling/incremental KFrontIR contract in the spec.

---

## What is reasonably aligned

The frontend is not a toy. It does implement a meaningful slice:

* Lexer/tokenization, indentation handling, comments, string/character-like literals, and Unicode diagnostics are substantial.
* Soft keywords are partly supported because `Token.isName` treats identifiers and keywords as names (`Syntax.fs:739-743`).
* Backtick identifiers are supported through `SyntaxFacts.trimIdentifierQuotes`.
* Grapheme and byte literals are parsed contextually as adjacent `g`/`b` prefixes plus character literals (`CoreParsing.fs:1365-1385`, `5786-5793`).
* Many import forms parse correctly, including namespace selectors and duplicate `unhide`/`clarify` modifier rejection (`Parser.fs:793-874`).
* Import cycle detection exists (`CompilationFrontend.fs:1820-1882`, from earlier inspection).
* Fixity reparse is implemented in the main pipeline (`Compilation.fs:89-92`).
* Surface AST supports records, record updates, safe navigation, Elvis, comprehensions, prefixed/interpolated strings, matches, handlers, and a fair bit more.
* Resource checking has a real dedicated pass and model, though I did not perform a full proof-style audit of its conformance.

So the frontend is a credible implementation of a subset. The problem is that the spec is not a subset; it is a continent.

---

## Recommended repair order

1. **Define the supported conformance level.**
   Right now the compiler looks like it wants to be judged against the whole spec, which is a splendid way to lose in court against your own markdown file. Mark unsupported spec sections explicitly, or implement them.

2. **Fix module path inference first.**
   This is foundational and easy to test. Make it exact, ASCII, fragment-validating, and collision-detecting.

3. **Make unsupported syntax fail deliberately.**
   For `derive`, `try`, `for` in `do`, `break`, `continue`, `decreases`, and local declarations in `do`, either implement them or emit targeted diagnostics like `E_UNSUPPORTED_SURFACE_FORM`. Do not let them rot into generic parse errors.

4. **Add spans/origins to declarations and expressions.**
   Diagnostics cannot be spec-grade until AST/KFrontIR nodes can identify where they came from.

5. **Separate “parsed” from “implemented.”**
   `unhide`, `clarify`, URL imports, and totality assertions currently parse better than they semantically behave. That is dangerous because tests may accidentally validate syntax while the language feature remains hollow.

6. **Expand duplicate declaration checks.**
   Cover signatures, projections, effects, traits, instances where appropriate, expects, and cross-namespace conflicts specified by the name-resolution model.

7. **Add conformance tests for every `MUST`.**
   Especially module path rules, diagnostics origin presence, keyword tokenization, and unsupported/missing syntax.

---

## Concrete tests I would add immediately

### Module/file mapping

```text
src/main.bad-frag.kp        -- must reject invalid fragment
src/Δelta.kp                -- must reject path-derived non-ASCII segment
src/Foo.kp + src/foo.kp     -- must reject case-fold collision and name both files
src/main.KP                 -- should not be accepted if `.kp` is exact
```

### Module attributes

```kappa
@Nonsense module main
let x = 1
```

Expected: unknown module attribute diagnostic.

### Identifiers

```kappa
let π = 3
```

Expected under current spec: reject unless written as backtick identifier.

```kappa
let `π` = 3
```

Expected: accept.

### Missing `do` control flow

```kappa
let f =
    do
        while cond do
            break
        return ()
```

Expected per spec: parse and check. Current frontend likely treats `break` as an expression/name-ish failure path.

### Missing `for` in `do`

```kappa
let f xs =
    do
        for x in xs do
            use x
        return ()
```

Expected per spec: parse/check. Current AST has no `DoFor`.

### Missing local declarations in `do`

```kappa
let f =
    do
        data Local = MkLocal
        return MkLocal
```

Expected per spec: local declaration allowed. Current `SurfaceDoStatement` cannot represent it.

### Deriving

```kappa
data Foo = Foo Int
derive Eq Foo
```

Expected per spec: parse, then synthesize or fail with a deriving diagnostic. Current frontend has no parser branch.

### Totality

```kappa
let f x decreases structural x =
    f x
```

Expected per spec: parse `decreases`, then reject for non-decreasing recursion. Current frontend does not appear to parse the clause.

### Diagnostic origin

Create a duplicate declaration and assert every emitted error has a non-null source location. Current `makeDiagnostic` in `SurfaceElaboration.fs` will fail this for some validation diagnostics.

---

## Bottom line

This frontend is **not compliant with the full spec**. It implements a substantial and useful subset, but several normative frontend areas are missing or only syntactically present:

* exact module/path rules,
* complete import semantics,
* full lexical keyword/identifier contract,
* `do` block declaration/control-flow forms,
* deriving,
* totality/decreases,
* try/except/finally,
* spec-grade diagnostics and origins.

Treat it as a milestone frontend, not a conforming implementation. The most urgent fixes are module-path conformance, source-origin diagnostics, and making unsupported syntax fail explicitly instead of pretending the parser merely had a bad afternoon.
