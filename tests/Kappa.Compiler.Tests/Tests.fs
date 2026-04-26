// Covers broad compiler smoke tests and repository-level regressions.
module SmokeTests

open System
open System.IO
open System.Numerics
open System.Runtime.InteropServices
open Kappa.Compiler
open Harness
open Xunit

let private parseTypeText (text: string) =
    let source = createSource "__type_signature_test__.kp" text
    let lexed = Lexer.tokenize source
    Assert.Empty(lexed.Diagnostics)

    match TypeSignatures.parseType lexed.Tokens with
    | Some parsed -> parsed
    | None -> failwithf "Failed to parse type text: %s" text

let private parseSchemeText (text: string) =
    let source = createSource "__type_signature_scheme_test__.kp" text
    let lexed = Lexer.tokenize source
    Assert.Empty(lexed.Diagnostics)

    match TypeSignatures.parseScheme lexed.Tokens with
    | Some parsed -> parsed
    | None -> failwithf "Failed to parse scheme text: %s" text

let private tokensText (tokens: Token list) =
    tokens
    |> List.map (fun token -> token.Text)
    |> String.concat " "

let private parseExpressionWithFixities (fixities: FixityTable) (fileName: string) (text: string) =
    let source = createSource fileName text
    let lexed = Lexer.tokenize source
    Assert.Empty(lexed.Diagnostics)
    let diagnostics = DiagnosticBag()
    let parsed = CoreParsing.parseExpression fixities source diagnostics lexed.Tokens
    parsed, diagnostics.Items

let private parseDocumentWithFixities (fixities: FixityTable) (fileName: string) (text: string) =
    let source = createSource fileName text
    let lexed = Lexer.tokenize source
    Assert.Empty(lexed.Diagnostics)
    Parser.parseWithInitialFixities fixities source lexed.Tokens

let private tryFindDocument moduleName (workspace: WorkspaceCompilation) =
    workspace.Documents
    |> List.tryFind (fun document ->
        match document.ModuleName with
        | Some segments -> SyntaxFacts.moduleNameToText segments = moduleName
        | None -> false)

let private tryFindLetDeclaration declarationName (document: ParsedDocument) =
    document.Syntax.Declarations
    |> List.tryPick (function
        | LetDeclaration declaration when declaration.Name = Some declarationName -> Some declaration
        | _ -> None)

let private assertSurfaceIntegerLiteral (expectedValue: int) expectedText expression =
    match expression with
    | NumericLiteral(SurfaceIntegerLiteral(value, sourceText, None)) ->
        Assert.Equal(BigInteger(expectedValue), value)
        Assert.Equal(expectedText, sourceText)
    | other ->
        failwithf "Expected surface integer literal %s, got %A" expectedText other

[<Fact>]
let ``repo zig bootstrap command uses the native shell for this platform`` () =
    let shellProgram, shellArguments = repoZigBootstrapCommand ()

    if RuntimeInformation.IsOSPlatform(OSPlatform.Windows) then
        Assert.Equal("powershell", shellProgram)
        Assert.Contains("ensure-zig.ps1", shellArguments)
    else
        Assert.Equal("sh", shellProgram)
        Assert.Contains("ensure-zig.sh", shellArguments)
        Assert.DoesNotContain("pwsh", shellProgram.ToLowerInvariant())

[<Fact>]
let ``source text reports 1-based line and column positions`` () =
    let source = createSource "memory.kp" "alpha\nbeta\n"
    let location = source.GetLocation(TextSpan.FromBounds(6, 10))

    Assert.Equal(2, location.Start.Line)
    Assert.Equal(1, location.Start.Column)
    Assert.Equal(2, location.End.Line)
    Assert.Equal(5, location.End.Column)

[<Fact>]
let ``lexer emits indent and dedent tokens for indented declarations`` () =
    let sourceText =
        [
            "data Maybe (a : Type) : Type ="
            "    Nothing"
            "    Just a"
        ]
        |> String.concat "\n"

    let _, lexed, _ =
        lexAndParse
            "memory.kp"
            sourceText

    let kinds = lexed.Tokens |> List.map (fun token -> token.Kind)

    Assert.Empty(lexed.Diagnostics)
    Assert.Contains(Indent, kinds)
    Assert.Contains(Dedent, kinds)

[<Fact>]
let ``lexer recognizes spec numeric literal forms including suffixes`` () =
    let source =
        createSource
            "__numeric_literals__.kp"
            "0xDEAD_BEEF 0o1_2_3 0b1_0_1_0 1_000 6.022_140_857E23 12px 3.14rad"

    let lexed = Lexer.tokenize source

    let actualTokens =
        lexed.Tokens
        |> List.filter (fun token ->
            match token.Kind with
            | Newline
            | EndOfFile -> false
            | _ -> true)
        |> List.map (fun token -> token.Kind, token.Text)

    Assert.Empty(lexed.Diagnostics)
    Assert.Equal<(TokenKind * string) list>(
        [
            IntegerLiteral, "0xDEAD_BEEF"
            IntegerLiteral, "0o1_2_3"
            IntegerLiteral, "0b1_0_1_0"
            IntegerLiteral, "1_000"
            FloatLiteral, "6.022_140_857E23"
            IntegerLiteral, "12px"
            FloatLiteral, "3.14rad"
        ],
        actualTokens
    )

[<Fact>]
let ``lexer recognizes raw and multiline string literal spellings`` () =
    let sourceText =
        [
            "#\"C:\\tmp\\file.txt\"#"
            "##\"she said \"hello\"\"##"
            "let cooked = \"\"\""
            "    alpha"
            "    \"\"\""
            "let raw = #\"\"\""
            "    beta\\n"
            "    \"\"\"#"
        ]
        |> String.concat "\n"

    let source = createSource "__string_literals__.kp" sourceText
    let lexed = Lexer.tokenize source

    let stringTokens =
        lexed.Tokens
        |> List.choose (fun token ->
            match token.Kind with
            | StringLiteral -> Some token.Text
            | _ -> None)

    Assert.Empty(lexed.Diagnostics)
    Assert.Equal<string list>(
        [
            "#\"C:\\tmp\\file.txt\"#"
            "##\"she said \"hello\"\"##"
            "\"\"\"\n    alpha\n    \"\"\""
            "#\"\"\"\n    beta\\n\n    \"\"\"#"
        ],
        stringTokens
    )

[<Fact>]
let ``parser captures import selectors and declaration kinds`` () =
    let sourceText =
        [
            "module demo.hello"
            "import std.list.(type List, ctor Cons)"
            "export std.math.(sin, cos)"
            "let answer = 42"
        ]
        |> String.concat "\n"

    let _, lexed, parsed =
        lexAndParse
            "demo/hello.kp"
            sourceText

    Assert.Empty(lexed.Diagnostics)
    Assert.Empty(parsed.Diagnostics)

    match parsed.Syntax.ModuleHeader with
    | Some [ "demo"; "hello" ] -> ()
    | other -> failwithf "Unexpected module header: %A" other

    match parsed.Syntax.Declarations with
    | [ ImportDeclaration (false, [ importSpec ])
        ImportDeclaration (true, [ exportSpec ])
        LetDeclaration definition ] ->
        match importSpec.Source, importSpec.Selection with
        | Dotted [ "std"; "list" ], Items items ->
            let first = List.item 0 items
            let second = List.item 1 items

            Assert.Equal("List", first.Name)
            Assert.Equal(Some ImportNamespace.Type, first.Namespace)
            Assert.Equal("Cons", second.Name)
            Assert.Equal(Some ImportNamespace.Constructor, second.Namespace)
        | other -> failwithf "Unexpected import declaration: %A" other

        match exportSpec.Source, exportSpec.Selection with
        | Dotted [ "std"; "math" ], Items items ->
            let names = items |> List.map (fun item -> item.Name)
            Assert.Equal<string list>([ "sin"; "cos" ], names)
        | other -> failwithf "Unexpected export declaration: %A" other

        Assert.Equal(Some "answer", definition.Name)
    | other ->
        failwithf "Unexpected top-level declarations: %A" other

[<Fact>]
let ``parser captures effect declarations and handler expressions`` () =
    let sourceText =
        [
            "module demo.effects"
            "effect State (s : Type) ="
            "    1 get : Unit -> s"
            "    1 put : s -> Unit"
            "let handled ="
            "    block"
            "        scoped effect Ask ="
            "            ask : Unit -> Bool"
            "        let comp : Eff <[Ask : Ask]> Int ="
            "            do"
            "                let b <- Ask.ask ()"
            "                if b then 1 else 0"
            "        handle Ask comp with"
            "            case return x -> pure x"
            "            case ask _ k -> k True"
            "let deepHandled ="
            "    deep handle Ask comp with"
            "        case return x -> pure x"
            "        case ask _ k -> k False"
        ]
        |> String.concat "\n"

    let _, lexed, parsed =
        lexAndParse
            "demo/effects.kp"
            sourceText

    Assert.Empty(lexed.Diagnostics)
    Assert.Empty(parsed.Diagnostics)

    match parsed.Syntax.Declarations with
    | [ EffectDeclarationNode effectDeclaration
        LetDeclaration { Name = Some "handled"; Body = Some handledBody }
        LetDeclaration { Name = Some "deepHandled"; Body = Some(Handle(true, Name [ "Ask" ], Name [ "comp" ], deepReturnClause, [ deepOpClause ])) } ] ->
        let localEffect, returnClause, opClause =
            match handledBody with
            | LocalScopedEffect(
                localEffect,
                LocalLet(
                    { Pattern = NamePattern "comp" },
                    Do _,
                    Handle(false, Name [ "Ask" ], Name [ "comp" ], returnClause, [ opClause ])
                )
              ) ->
                localEffect, returnClause, opClause
            | other ->
                failwithf "Unexpected handled expression shape: %A" other

        Assert.Equal("State", effectDeclaration.Name)
        Assert.Equal<string list>([ "("; "s"; ":"; "Type"; ")" ], effectDeclaration.HeaderTokens |> List.map (fun token -> token.Text))
        Assert.Equal<string list>([ "get"; "put" ], effectDeclaration.Operations |> List.map (fun operation -> operation.Name))
        Assert.Equal(Some QuantityOne, effectDeclaration.Operations[0].ResumptionQuantity)
        Assert.Equal("Ask", localEffect.Name)
        Assert.Equal<string list>([ "ask" ], localEffect.Operations |> List.map (fun operation -> operation.Name))
        Assert.Equal("return", returnClause.OperationName)
        Assert.Equal(None, returnClause.ResumptionName)
        Assert.Equal("ask", opClause.OperationName)
        Assert.Equal(Some "k", opClause.ResumptionName)
        Assert.Equal("return", deepReturnClause.OperationName)
        Assert.Equal("ask", deepOpClause.OperationName)
    | other ->
        failwithf "Unexpected declarations for effect/handler test: %A" other

[<Fact>]
let ``parser captures constructor-bundle import items and kind-qualified wildcard exclusions`` () =
    let sourceText =
        [
            "module demo.hello"
            "import std.list.(type List(..), ctor Cons)"
            "import std.math.* except (term sin, type pi, ctor Cons)"
        ]
        |> String.concat "\n"

    let _, lexed, parsed =
        lexAndParse
            "demo/hello.kp"
            sourceText

    Assert.Empty(lexed.Diagnostics)
    Assert.Empty(parsed.Diagnostics)

    match parsed.Syntax.Declarations with
    | [ ImportDeclaration (false, [ itemSpec ])
        ImportDeclaration (false, [ exceptSpec ]) ] ->
        match itemSpec.Source, itemSpec.Selection with
        | Dotted [ "std"; "list" ], Items [ listItem; ctorItem ] ->
            Assert.Equal(Some ImportNamespace.Type, listItem.Namespace)
            Assert.Equal("List", listItem.Name)
            Assert.True(listItem.IncludeConstructors)
            Assert.Equal(Some ImportNamespace.Constructor, ctorItem.Namespace)
            Assert.Equal("Cons", ctorItem.Name)
            Assert.False(ctorItem.IncludeConstructors)
        | other ->
            failwithf "Unexpected constructor-bundle import item: %A" other

        match exceptSpec.Source, exceptSpec.Selection with
        | Dotted [ "std"; "math" ], AllExcept excludedItems ->
            Assert.Equal<ExceptItem list>(
                [
                    { Namespace = Some ImportNamespace.Term; Name = "sin" }
                    { Namespace = Some ImportNamespace.Type; Name = "pi" }
                    { Namespace = Some ImportNamespace.Constructor; Name = "Cons" }
                ],
                excludedItems
            )
        | other ->
            failwithf "Unexpected wildcard exclusion import: %A" other
    | other ->
        failwithf "Unexpected declarations: %A" other

[<Fact>]
let ``parser rejects duplicate unhide and clarify modifiers within one import item`` () =
    let sourceText =
        [
            "module demo.hello"
            "import std.rope.(unhide unhide term normalizeWorker, clarify clarify type Rope)"
        ]
        |> String.concat "\n"

    let _, lexed, parsed =
        lexAndParse
            "demo/hello.kp"
            sourceText

    Assert.Empty(lexed.Diagnostics)
    Assert.Equal<DiagnosticCode list>(
        [ DiagnosticCode.ParseError; DiagnosticCode.ParseError ],
        parsed.Diagnostics |> List.map (fun diagnostic -> diagnostic.Code)
    )
    Assert.Contains(parsed.Diagnostics, fun diagnostic -> diagnostic.Message.Contains("Duplicate 'unhide'"))
    Assert.Contains(parsed.Diagnostics, fun diagnostic -> diagnostic.Message.Contains("Duplicate 'clarify'"))

[<Fact>]
let ``parser rejects aliases on constructor-bundle import items`` () =
    let sourceText =
        [
            "module demo.hello"
            "import std.list.(type List(..) as StdList)"
        ]
        |> String.concat "\n"

    let _, lexed, parsed =
        lexAndParse
            "demo/hello.kp"
            sourceText

    Assert.Empty(lexed.Diagnostics)
    Assert.Equal<DiagnosticCode list>([ DiagnosticCode.ParseError ], parsed.Diagnostics |> List.map (fun diagnostic -> diagnostic.Code))
    Assert.Contains(parsed.Diagnostics, fun diagnostic -> diagnostic.Message.Contains("ctorAll may not be combined with an alias"))

[<Fact>]
let ``parser rejects malformed type aliases with trailing operator garbage`` () =
    let sourceText =
        [
            "module main"
            ""
            "type I0 = (left : I0)\\I0\\I1 I1"
        ]
        |> String.concat "\n"

    let _, lexed, parsed =
        lexAndParse
            "memory.kp"
            sourceText

    Assert.Empty(lexed.Diagnostics)
    Assert.Equal<DiagnosticCode list>([ DiagnosticCode.ParseError ], parsed.Diagnostics |> List.map (fun diagnostic -> diagnostic.Code))
    Assert.Contains(parsed.Diagnostics, fun diagnostic -> diagnostic.Message.Contains("type alias body"))

[<Fact>]
let ``parser rejects malformed trait members that are not signatures or defaults`` () =
    let sourceText =
        [
            "module main"
            ""
            "type I1 = (left : I0)\\I0\\I1 as I3"
            ""
            "trait I4 i0 ="
            "    while I4 (I4 this = I0 40 0)"
        ]
        |> String.concat "\n"

    let _, lexed, parsed =
        lexAndParse
            "memory.kp"
            sourceText

    Assert.Empty(lexed.Diagnostics)
    Assert.Equal<DiagnosticCode list>(
        [ DiagnosticCode.ParseError; DiagnosticCode.ParseError ],
        parsed.Diagnostics |> List.map (fun diagnostic -> diagnostic.Code)
    )
    Assert.Contains(parsed.Diagnostics, fun diagnostic -> diagnostic.Message.Contains("type alias body"))
    Assert.Contains(parsed.Diagnostics, fun diagnostic -> diagnostic.Message.Contains("trait member"))

[<Fact>]
let ``parser rejects unterminated data constructors before the next declaration`` () =
    let sourceText =
        [
            "module main"
            ""
            "data I0 : Type ="
            "    I1 (I0 : Int)"
            "    I3 (I1 : I1"
            "let I1 (1 I1 : I1) = I1 I1"
        ]
        |> String.concat "\n"

    let _, lexed, parsed =
        lexAndParse
            "memory.kp"
            sourceText

    Assert.Empty(lexed.Diagnostics)
    Assert.Equal<DiagnosticCode list>([ DiagnosticCode.ParseError ], parsed.Diagnostics |> List.map (fun diagnostic -> diagnostic.Code))
    Assert.Contains(parsed.Diagnostics, fun diagnostic -> diagnostic.Message.Contains("Expected ')'"))

[<Fact>]
let ``parser rejects malformed top level signatures with stray block bodies`` () =
    let sourceText =
        [
            "module main"
            ""
            "data I1 : Type ="
            "    I1 (I0 : Int)"
            ""
            "I0 : (1 I0 : I0) -> IO (1]"
            "    case Nil -> 0"
        ]
        |> String.concat "\n"

    let _, lexed, parsed =
        lexAndParse
            "memory.kp"
            sourceText

    Assert.Empty(lexed.Diagnostics)
    Assert.Equal<DiagnosticCode list>([ DiagnosticCode.ParseError ], parsed.Diagnostics |> List.map (fun diagnostic -> diagnostic.Code))
    Assert.Contains(parsed.Diagnostics, fun diagnostic -> diagnostic.Message.Contains("valid signature type"))

[<Fact>]
let ``parser accepts URL singleton sugar for import and export`` () =
    let sourceText =
        [
            "module demo.hello"
            "import \"https://example.com/lib\".value"
            "export \"https://example.com/lib\".result"
        ]
        |> String.concat "\n"

    let _, lexed, parsed =
        lexAndParse
            "demo/hello.kp"
            sourceText

    Assert.Empty(lexed.Diagnostics)
    Assert.Empty(parsed.Diagnostics)

    match parsed.Syntax.Declarations with
    | [ ImportDeclaration (false, [ importSpec ])
        ImportDeclaration (true, [ exportSpec ]) ] ->
        match importSpec.Source, importSpec.Selection with
        | Url { OriginalText = "https://example.com/lib"; BaseUrl = "https://example.com/lib"; Pin = None }, Items [ item ] ->
            Assert.Equal("value", item.Name)
            Assert.Equal(None, item.Namespace)
            Assert.Equal(None, item.Alias)
            Assert.False(item.IncludeConstructors)
        | other ->
            failwithf "Unexpected URL singleton import: %A" other

        match exportSpec.Source, exportSpec.Selection with
        | Url { OriginalText = "https://example.com/lib"; BaseUrl = "https://example.com/lib"; Pin = None }, Items [ item ] ->
            Assert.Equal("result", item.Name)
            Assert.Equal(None, item.Namespace)
            Assert.Equal(None, item.Alias)
            Assert.False(item.IncludeConstructors)
        | other ->
            failwithf "Unexpected URL singleton export: %A" other
    | other ->
        failwithf "Unexpected declarations for URL singleton sugar: %A" other

[<Fact>]
let ``parser parses URL pins structurally and canonicalizes sha256 digests`` () =
    let sourceText =
        [
            "module demo.hello"
            "import \"https://example.com/lib#sha256:ABCD1234\".*"
            "import \"https://example.com/lib#ref:v1.2.3\".(value)"
        ]
        |> String.concat "\n"

    let _, lexed, parsed =
        lexAndParse
            "demo/hello.kp"
            sourceText

    Assert.Empty(lexed.Diagnostics)
    Assert.Empty(parsed.Diagnostics)

    match parsed.Syntax.Declarations with
    | [ ImportDeclaration (false, [ shaSpec ])
        ImportDeclaration (false, [ refSpec ]) ] ->
        match shaSpec.Source with
        | Url { OriginalText = "https://example.com/lib#sha256:ABCD1234"
                BaseUrl = "https://example.com/lib"
                Pin = Some(Sha256Pin "abcd1234") } -> ()
        | other ->
            failwithf "Unexpected sha256 URL import specifier: %A" other

        match refSpec.Source with
        | Url { OriginalText = "https://example.com/lib#ref:v1.2.3"
                BaseUrl = "https://example.com/lib"
                Pin = Some(RefPin "v1.2.3") } -> ()
        | other ->
            failwithf "Unexpected ref URL import specifier: %A" other
    | other ->
        failwithf "Unexpected declarations for URL pin parsing: %A" other

[<Fact>]
let ``parser rejects malformed URL pins`` () =
    let sourceText =
        [
            "module demo.hello"
            "import \"https://example.com/lib#sha256:not-hex\".*"
        ]
        |> String.concat "\n"

    let _, lexed, parsed =
        lexAndParse
            "demo/hello.kp"
            sourceText

    Assert.Empty(lexed.Diagnostics)
    Assert.Equal<DiagnosticCode list>([ DiagnosticCode.ParseError ], parsed.Diagnostics |> List.map (fun diagnostic -> diagnostic.Code))
    Assert.Contains(parsed.Diagnostics, fun diagnostic -> diagnostic.Message.Contains("sha256", StringComparison.Ordinal))

[<Fact>]
let ``package mode rejects unpinned URL imports before unresolved-module validation`` () =
    let workspace =
        compileInMemoryWorkspace
            "memory-package-unpinned-url"
            [
                "main.kp",
                String.concat
                    "\n"
                    [
                        "module main"
                        "import \"https://example.com/lib\".*"
                    ]
            ]

    let codes = workspace.Diagnostics |> List.map (fun diagnostic -> diagnostic.Code)
    Assert.Contains(DiagnosticCode.UrlImportUnpinnedInPackageMode, codes)
    Assert.DoesNotContain(DiagnosticCode.ModuleNameUnresolved, codes)

[<Fact>]
let ``package mode rejects ref pinned URL imports without a locked immutable resolution`` () =
    let workspace =
        compileInMemoryWorkspace
            "memory-package-ref-url"
            [
                "main.kp",
                String.concat
                    "\n"
                    [
                        "module main"
                        "import \"https://example.com/lib#ref:v1.2.3\".*"
                    ]
            ]

    let codes = workspace.Diagnostics |> List.map (fun diagnostic -> diagnostic.Code)
    Assert.Contains(DiagnosticCode.UrlImportRefPinRequiresLock, codes)
    Assert.DoesNotContain(DiagnosticCode.ModuleNameUnresolved, codes)

[<Fact>]
let ``package mode accepts sha256 pinned URL syntax without reproducibility diagnostics`` () =
    let workspace =
        compileInMemoryWorkspace
            "memory-package-sha-url"
            [
                "main.kp",
                String.concat
                    "\n"
                    [
                        "module main"
                        "import \"https://example.com/lib#sha256:ABCD1234\".*"
                    ]
            ]

    let codes = workspace.Diagnostics |> List.map (fun diagnostic -> diagnostic.Code)
    Assert.DoesNotContain(DiagnosticCode.UrlImportUnpinnedInPackageMode, codes)
    Assert.DoesNotContain(DiagnosticCode.UrlImportRefPinRequiresLock, codes)
    Assert.Contains(DiagnosticCode.ModuleNameUnresolved, codes)

[<Fact>]
let ``script mode permits unpinned and ref pinned URL imports without package-mode reproducibility diagnostics`` () =
    let workspace =
        compileInMemoryWorkspaceWithPackageMode
            "memory-script-url-imports"
            false
            [
                "main.kp",
                String.concat
                    "\n"
                    [
                        "module main"
                        "import \"https://example.com/lib\".*"
                        "import \"https://example.com/lib#ref:v1.2.3\".(value)"
                    ]
            ]

    let codes = workspace.Diagnostics |> List.map (fun diagnostic -> diagnostic.Code)
    Assert.DoesNotContain(DiagnosticCode.UrlImportUnpinnedInPackageMode, codes)
    Assert.DoesNotContain(DiagnosticCode.UrlImportRefPinRequiresLock, codes)
    Assert.Equal(2, codes |> List.filter ((=) DiagnosticCode.ModuleNameUnresolved) |> List.length)

[<Fact>]
let ``bare dotted import resolves to singleton sugar when only the parent item exists`` () =
    let workspace =
        compileInMemoryWorkspace
            "memory-bare-dotted-singleton-import"
            [
                "a/b.kp",
                String.concat
                    "\n"
                    [
                        "module a.b"
                        "value : Int"
                        "let value = 41"
                    ]
                "main.kp",
                String.concat
                    "\n"
                    [
                        "module main"
                        "import a.b.value"
                        "result : Int"
                        "let result = value"
                    ]
            ]

    Assert.False(workspace.HasErrors, sprintf "Expected no diagnostics, got %A" workspace.Diagnostics)

[<Fact>]
let ``bare dotted export resolves to singleton sugar when only the parent item exists`` () =
    let workspace =
        compileInMemoryWorkspace
            "memory-bare-dotted-singleton-export"
            [
                "lib.kp",
                String.concat
                    "\n"
                    [
                        "module lib"
                        "value : Int"
                        "let value = 41"
                    ]
                "facade.kp",
                String.concat
                    "\n"
                    [
                        "module facade"
                        "export lib.value"
                    ]
                "main.kp",
                String.concat
                    "\n"
                    [
                        "module main"
                        "import facade.(value)"
                        "result : Int"
                        "let result = value"
                    ]
            ]

    Assert.False(workspace.HasErrors, sprintf "Expected no diagnostics, got %A" workspace.Diagnostics)

[<Fact>]
let ``bare dotted import is rejected when both module and singleton interpretations resolve`` () =
    let workspace =
        compileInMemoryWorkspace
            "memory-bare-dotted-import-ambiguous"
            [
                "a/b.kp",
                String.concat
                    "\n"
                    [
                        "module a.b"
                        "c : Int"
                        "let c = 1"
                    ]
                "a/b/c.kp",
                String.concat
                    "\n"
                    [
                        "module a.b.c"
                        "answer : Int"
                        "let answer = 42"
                    ]
                "main.kp",
                String.concat
                    "\n"
                    [
                        "module main"
                        "import a.b.c"
                        "result : Int"
                        "let result = 0"
                    ]
            ]

    Assert.True(workspace.HasErrors, "Expected ambiguous bare dotted import to become a compile-time error.")
    Assert.Contains(workspace.Diagnostics, fun diagnostic -> diagnostic.Code = DiagnosticCode.ImportAmbiguous)
    Assert.Contains(workspace.Diagnostics, fun diagnostic -> diagnostic.Message.Contains("a.b.c", StringComparison.Ordinal))

[<Fact>]
let ``bare dotted export is rejected when both module and singleton interpretations resolve`` () =
    let workspace =
        compileInMemoryWorkspace
            "memory-bare-dotted-export-ambiguous"
            [
                "a/b.kp",
                String.concat
                    "\n"
                    [
                        "module a.b"
                        "c : Int"
                        "let c = 1"
                    ]
                "a/b/c.kp",
                String.concat
                    "\n"
                    [
                        "module a.b.c"
                        "answer : Int"
                        "let answer = 42"
                    ]
                "main.kp",
                String.concat
                    "\n"
                    [
                        "module main"
                        "export a.b.c"
                    ]
            ]

    Assert.True(workspace.HasErrors, "Expected ambiguous bare dotted export to become a compile-time error.")
    Assert.Contains(workspace.Diagnostics, fun diagnostic -> diagnostic.Code = DiagnosticCode.ImportAmbiguous)
    Assert.Contains(workspace.Diagnostics, fun diagnostic -> diagnostic.Message.Contains("a.b.c", StringComparison.Ordinal))

[<Fact>]
let ``package mode rejects unpinned URL imports and exports before module resolution`` () =
    let workspace =
        compileInMemoryWorkspace
            "memory-url-import-validation"
            [
                "main.kp",
                String.concat
                    "\n"
                    [
                        "module main"
                        "import \"https://example.com/lib\".(value)"
                        "export \"https://example.com/lib\".*"
                    ]
            ]

    Assert.True(workspace.HasErrors, "Expected unpinned URL imports/exports to produce package-mode diagnostics.")

    let urlDiagnostics =
        workspace.Diagnostics
        |> List.filter (fun diagnostic -> diagnostic.Code = DiagnosticCode.UrlImportUnpinnedInPackageMode)

    Assert.Equal(2, urlDiagnostics.Length)
    Assert.All(
        urlDiagnostics,
        fun diagnostic -> Assert.Contains("https://example.com/lib", diagnostic.Message, StringComparison.Ordinal)
    )

[<Fact>]
let ``dotnet host type modules import as ordinary modules on the dotnet backend`` () =
    let workspace =
        compileInMemoryWorkspaceWithBackend
            "memory-host-dotnet-import-root"
            "dotnet"
            [
                "main.kp",
                String.concat
                    "\n"
                    [
                        "module main"
                        "import host.dotnet.Kappa.Compiler.TestHost.Sample.(type Sample, term new, term Echo, term Create)"
                        "result : String"
                        "let result ="
                        "    let fromCtor = new \"hello\""
                        "    let _ = fromCtor.Echo ()"
                        "    let fromStatic = Create \"world\""
                        "    fromStatic.Echo ()"
                    ]
            ]

    Assert.False(workspace.HasErrors, sprintf "Expected host.dotnet import to compile on the dotnet backend, got %A" workspace.Diagnostics)

[<Fact>]
let ``host dotnet imports are rejected on unsupported backends`` () =
    let workspace =
        compileInMemoryWorkspaceWithBackend
            "memory-host-dotnet-unsupported-root"
            "interpreter"
            [
                "main.kp",
                String.concat
                    "\n"
                    [
                        "module main"
                        "import host.dotnet.Kappa.Compiler.TestHost.Sample.(type Sample)"
                    ]
            ]

    Assert.True(workspace.HasErrors, "Expected host.dotnet imports to be rejected on backends that do not provide host.dotnet.")
    Assert.Contains(workspace.Diagnostics, fun diagnostic -> diagnostic.Message.Contains("host.dotnet", StringComparison.Ordinal))

[<Fact>]
let ``source modules under reserved host roots are rejected`` () =
    let workspace =
        compileInMemoryWorkspaceWithBackend
            "memory-host-root-reserved"
            "dotnet"
            [
                "host/dotnet/System/Text/StringBuilder.kp",
                String.concat
                    "\n"
                    [
                        "module host.dotnet.System.Text.StringBuilder"
                        "let fake = 0"
                    ]
            ]

    Assert.True(workspace.HasErrors, "Expected source-defined modules under reserved host roots to be rejected.")
    Assert.Contains(workspace.Diagnostics, fun diagnostic -> diagnostic.Message.Contains("reserved host", StringComparison.OrdinalIgnoreCase))

[<Fact>]
let ``parser decodes raw and multiline strings using spec dedent rules`` () =
    let sourceText =
        [
            "module main"
            "let raw = #\"C:\\tmp\\file.txt\"#"
            "let cooked = \"\"\""
            "    alpha"
            "    beta\\n"
            "    \"\"\""
            "let rawMulti = #\"\"\""
            "    gamma\\n"
            "      delta"
            "    \"\"\"#"
        ]
        |> String.concat "\n"

    let _, lexed, parsed =
        lexAndParse
            "main.kp"
            sourceText

    Assert.Empty(lexed.Diagnostics)
    Assert.Empty(parsed.Diagnostics)

    match parsed.Syntax.Declarations with
    | [ LetDeclaration { Body = Some(Literal(LiteralValue.String raw)) }
        LetDeclaration { Body = Some(Literal(LiteralValue.String cooked)) }
        LetDeclaration { Body = Some(Literal(LiteralValue.String rawMulti)) } ] ->
        Assert.Equal("C:\\tmp\\file.txt", raw)
        Assert.Equal("alpha\nbeta\n", cooked)
        Assert.Equal("gamma\\n\n  delta", rawMulti)
    | other ->
        failwithf "Unexpected parsed declarations for raw/multiline strings: %A" other

[<Fact>]
let ``lexer and parser support raw prefixed interpolation with matching hash count`` () =
    let sourceText =
        [
            "module main"
            "let query = sql#\"select #{userId}\"#"
            "let regex = re##\"\\b##{word}\\b\"##"
        ]
        |> String.concat "\n"

    let _, lexed, parsed =
        lexAndParse
            "main.kp"
            sourceText

    Assert.Empty(lexed.Diagnostics)
    Assert.Empty(parsed.Diagnostics)

    let tokenKinds = lexed.Tokens |> List.map (fun token -> token.Kind)
    Assert.Contains(InterpolatedStringStart, tokenKinds)
    Assert.Contains(StringTextSegment, tokenKinds)
    Assert.Contains(InterpolationStart, tokenKinds)
    Assert.Contains(InterpolationEnd, tokenKinds)
    Assert.Contains(InterpolatedStringEnd, tokenKinds)

    match parsed.Syntax.Declarations with
    | [ LetDeclaration { Body = Some(PrefixedString("sql", [ StringText "select "; StringInterpolation(Name [ "userId" ], None) ])) }
        LetDeclaration { Body = Some(PrefixedString("re", [ StringText "\\b"; StringInterpolation(Name [ "word" ], None); StringText "\\b" ])) } ] -> ()
    | other ->
        failwithf "Unexpected parsed raw prefixed string: %A" other

[<Fact>]
let ``parser preserves prefixed string format specifiers`` () =
    let sourceText =
        [
            "module main"
            "let padded = f\"pre${value : %04d}post\""
            "let sql = sql#\"select #{userId:param}\"#"
        ]
        |> String.concat "\n"

    let _, lexed, parsed =
        lexAndParse
            "main.kp"
            sourceText

    Assert.Empty(lexed.Diagnostics)
    Assert.Empty(parsed.Diagnostics)

    match parsed.Syntax.Declarations with
    | [ LetDeclaration { Body = Some(PrefixedString("f", [ StringText "pre"; StringInterpolation(Name [ "value" ], Some "%04d"); StringText "post" ])) }
        LetDeclaration { Body = Some(PrefixedString("sql", [ StringText "select "; StringInterpolation(Name [ "userId" ], Some "param") ])) } ] -> ()
    | other ->
        failwithf "Unexpected parsed prefixed string with format specifiers: %A" other

[<Fact>]
let ``lexer and parser support raw multiline prefixed interpolation with formats`` () =
    let sourceText =
        [
            "module main"
            "let query = sql#\"\"\""
            "    select *"
            "    from users"
            "    where id = #{userId : param}"
            "      and region = #{region : ident}"
            "\"\"\"#"
        ]
        |> String.concat "\n"

    let _, lexed, parsed =
        lexAndParse
            "main.kp"
            sourceText

    Assert.Empty(lexed.Diagnostics)
    Assert.Empty(parsed.Diagnostics)

    match parsed.Syntax.Declarations with
    | [ LetDeclaration { Body = Some(PrefixedString("sql", parts)) } ] ->
        Assert.Equal<SurfaceInterpolatedStringPart list>(
            [
                StringText "\n    select *\n    from users\n    where id = "
                StringInterpolation(Name [ "userId" ], Some "param")
                StringText "\n      and region = "
                StringInterpolation(Name [ "region" ], Some "ident")
                StringText "\n"
            ],
            parts
        )
    | other ->
        failwithf "Unexpected parsed raw multiline prefixed string: %A" other

[<Fact>]
let ``parser preserves parenthesized interpolation type ascriptions before format parsing`` () =
    let sourceText =
        [
            "module main"
            "let query = sql\"select * from users where id = ${(userId : Int) : param}\""
        ]
        |> String.concat "\n"

    let _, lexed, parsed =
        lexAndParse
            "main.kp"
            sourceText

    Assert.Empty(lexed.Diagnostics)
    Assert.Empty(parsed.Diagnostics)

    match parsed.Syntax.Declarations with
    | [ LetDeclaration { Body = Some(PrefixedString("sql", parts)) } ] ->
        Assert.Equal<SurfaceInterpolatedStringPart list>(
            [
                StringText "select * from users where id = "
                StringInterpolation(Name [ "userId" ], Some "param")
            ],
            parts
        )
    | other ->
        failwithf "Unexpected parsed interpolation type ascription: %A" other

[<Fact>]
let ``parser preserves suffixed numeric literals as exact surface literals`` () =
    let sourceText =
        [
            "module main"
            "let pixels = 12px"
            "let angle = 3.14rad"
            "let mask = 0xFFu32"
        ]
        |> String.concat "\n"

    let _, lexed, parsed =
        lexAndParse
            "main.kp"
            sourceText

    Assert.Empty(lexed.Diagnostics)
    Assert.Empty(parsed.Diagnostics)

    match parsed.Syntax.Declarations with
    | [ LetDeclaration { Body = Some(NumericLiteral(SurfaceIntegerLiteral(value, sourceText, Some "px"))) }
        LetDeclaration { Body = Some(NumericLiteral(SurfaceRealLiteral(_, sourceText2, Some "rad"))) }
        LetDeclaration { Body = Some(NumericLiteral(SurfaceIntegerLiteral(value2, sourceText3, Some "u32"))) } ] ->
        Assert.Equal(BigInteger(12), value)
        Assert.Equal("12", sourceText)
        Assert.Equal("3.14", sourceText2)
        Assert.Equal(BigInteger(255), value2)
        Assert.Equal("0xFF", sourceText3)
    | other ->
        failwithf "Unexpected parsed declarations for suffixed numeric literals: %A" other

[<Fact>]
let ``compilation resolves suffixed numeric literals through ordinary bindings`` () =
    let sourceText =
        [
            "module main"
            "px : Int -> Int"
            "let px n = n + 1"
            "rad : Float -> Float"
            "let rad d = d"
            "u32 : Int -> Int"
            "let u32 n = n"
            "pixels : Int"
            "let pixels = 12px"
            "angle : Float"
            "let angle = 3.14rad"
            "mask : Int"
            "let mask = 0xFFu32"
        ]
        |> String.concat "\n"

    let workspace =
        compileInMemoryWorkspace
            "memory-suffixed-numerics"
            [ "main.kp", sourceText ]

    Assert.False(workspace.HasErrors, sprintf "Expected no diagnostics, got %A" workspace.Diagnostics)

[<Fact>]
let ``unsuffixed integer literals adopt explicit Integer parameter context`` () =
    let workspace =
        compileInMemoryWorkspace
            "memory-integer-literal-parameter-context"
            [
                "main.kp",
                [
                    "module main"
                    ""
                    "idInt : Integer -> Integer"
                    "let idInt x = x"
                    ""
                    "value : Integer"
                    "let value = idInt 42"
                ]
                |> String.concat "\n"
            ]

    Assert.False(workspace.HasErrors, sprintf "Expected no diagnostics, got %A" workspace.Diagnostics)

[<Fact>]
let ``unsuffixed integer literals elaborate through an in-scope FromInteger instance`` () =
    let workspace, result =
        evaluateInMemoryBinding
            "memory-integer-literal-instance-context"
            "main.value"
            [
                "main.kp",
                [
                    "module main"
                    ""
                    "data Score : Type ="
                    "    Score Int"
                    ""
                    "trait FromInteger (a : Type) ="
                    "    fromInteger : Integer -> a"
                    ""
                    "instance FromInteger Score ="
                    "    let fromInteger n = Score 7"
                    ""
                    "value : Score"
                    "let value = 42"
                ]
                |> String.concat "\n"
            ]

    Assert.False(workspace.HasErrors, sprintf "Expected no diagnostics, got %A" workspace.Diagnostics)

    match result with
    | Result.Ok value ->
        Assert.Equal("Score 7", RuntimeValue.format value)
    | Result.Error issue ->
        failwithf "Expected FromInteger instance elaboration to evaluate, got %s" issue.Message

[<Fact>]
let ``unsuffixed integer literals reject custom runtime targets without a FromInteger instance`` () =
    let workspace =
        compileInMemoryWorkspace
            "memory-integer-literal-missing-instance"
            [
                "main.kp",
                [
                    "module main"
                    ""
                    "data Score : Type ="
                    "    Score Int"
                    ""
                    "value : Score"
                    "let value = 42"
                ]
                |> String.concat "\n"
            ]

    Assert.True(workspace.HasErrors, "Expected a numeric literal without FromInteger support to be rejected.")

[<Fact>]
let ``oversized Int literals report a range diagnostic instead of compiling through`` () =
    let workspace =
        compileInMemoryWorkspace
            "memory-oversized-int-literal"
            [
                "main.kp",
                [
                    "module main"
                    ""
                    "big : Int"
                    "let big = 9223372036854775808"
                ]
                |> String.concat "\n"
            ]

    Assert.True(workspace.HasErrors, "Expected an out-of-range integer literal diagnostic.")
    Assert.Contains(workspace.Diagnostics, fun diagnostic -> diagnostic.Code = DiagnosticCode.NumericLiteralOutOfRange)

[<Fact>]
let ``oversized suffixed integer literals report a range diagnostic instead of lowering to zero`` () =
    let workspace =
        compileInMemoryWorkspace
            "memory-oversized-suffixed-integer-literal"
            [
                "main.kp",
                [
                    "module main"
                    ""
                    "px : Integer -> Int"
                    "let px n = 1"
                    ""
                    "value : Int"
                    "let value = 9223372036854775808px"
                ]
                |> String.concat "\n"
            ]

    Assert.True(workspace.HasErrors, "Expected an out-of-range suffixed integer literal diagnostic.")
    Assert.Contains(workspace.Diagnostics, fun diagnostic -> diagnostic.Code = DiagnosticCode.NumericLiteralOutOfRange)

[<Fact>]
let ``parser reports unexpected indentation for a misindented match case body`` () =
    let sourceText =
        [
            "module main"
            ""
            "result : Int"
            "let result ="
            "    match True"
            "      case True ->"
            "    1"
            "      case False -> 0"
        ]
        |> String.concat "\n"

    let _, lexed, parsed =
        lexAndParse
            "memory.kp"
            sourceText

    Assert.Empty(lexed.Diagnostics)
    Assert.Equal<DiagnosticCode list>([ DiagnosticCode.UnexpectedIndentation ], parsed.Diagnostics |> List.map (fun diagnostic -> diagnostic.Code))

[<Fact>]
let ``parser reports a single parse error for an explicit brace after layout-introduced do`` () =
    let sourceText =
        [
            "module main"
            ""
            "bad : Bool -> IO Bool"
            "let bad x ="
            "    match x"
            "    case False -> do"
            "    { return x }"
            "    case True -> return x"
        ]
        |> String.concat "\n"

    let _, lexed, parsed =
        lexAndParse
            "memory.kp"
            sourceText

    Assert.Empty(lexed.Diagnostics)
    Assert.Equal<DiagnosticCode list>([ DiagnosticCode.ParseError ], parsed.Diagnostics |> List.map (fun diagnostic -> diagnostic.Code))

[<Fact>]
let ``parser keeps a multiline lambda body inside an implicit pure block suite`` () =
    let sourceText =
        [
            "module main"
            ""
            "data Token : Type ="
            "    Token"
            ""
            "data MaybeToken : Type ="
            "    None"
            "    Some Token"
            ""
            "consume : (1 token : Token) -> Unit"
            "let consume token = ()"
            ""
            "twice : (ω f : Unit -> Unit) -> Unit"
            "let twice f = do"
            "    f ()"
            "    f ()"
            ""
            "bad : (1 tokens : MaybeToken) -> Unit"
            "let bad tokens ="
            "    let f = \\(_ : Unit) ->"
            "        match tokens"
            "          case Some token -> consume token"
            "          case None -> ()"
            "    twice f"
        ]
        |> String.concat "\n"

    let _, lexed, parsed =
        lexAndParse
            "memory.kp"
            sourceText

    Assert.Empty(lexed.Diagnostics)
    Assert.Empty(parsed.Diagnostics)

[<Fact>]
let ``parser treats constructor tag tests as dedicated infix expressions`` () =
    let source =
        createSource
            "__tag_test_precedence__.kp"
            "module main\nlet result = if p is IntBox || p is NatBox then p.value else 0"
    let lexed = Lexer.tokenize source
    Assert.Empty(lexed.Diagnostics)

    let parsed = Parser.parse source lexed.Tokens
    Assert.Empty(parsed.Diagnostics)

    match parsed.Syntax.Declarations with
    | [ LetDeclaration { Body = Some(
            IfThenElse(
                Binary(TagTest(Name [ "p" ], [ "IntBox" ]), "||", TagTest(Name [ "p" ], [ "NatBox" ])),
                Name [ "p"; "value" ],
                elseBranch
            ))
        } ] ->
        assertSurfaceIntegerLiteral 0 "0" elseBranch
    | other ->
        failwithf "Unexpected parsed tag-test expression: %A" other

[<Fact>]
let ``parser keeps or-pattern alternatives in match cases`` () =
    let source =
        createSource
            "__or_pattern__.kp"
            "module main\nlet read value = match value\n  case LeftInt x | RightInt x -> x\n  case Empty -> 0"
    let lexed = Lexer.tokenize source
    Assert.Empty(lexed.Diagnostics)

    let parsed = Parser.parse source lexed.Tokens
    Assert.Empty(parsed.Diagnostics)

    match parsed.Syntax.Declarations with
    | [ LetDeclaration { Body = Some(Match(_, [ firstCase; _ ])) } ] ->
        match firstCase.Pattern with
        | OrPattern [ ConstructorPattern([ "LeftInt" ], [ NamePattern "x" ]); ConstructorPattern([ "RightInt" ], [ NamePattern "x" ]) ] -> ()
        | other ->
            failwithf "Unexpected parsed or-pattern: %A" other
    | other ->
        failwithf "Unexpected parsed declarations for or-pattern test: %A" other

[<Fact>]
let ``parser captures typed as tuple and record-rest patterns`` () =
    let source =
        createSource
            "__pattern_forms__.kp"
            "module main\nlet demo value = match value\n  case whole@((head, tail) : (Int, Int)) -> head\n  case (@proof = p, name = n, ..rest) -> n"
    let lexed = Lexer.tokenize source
    Assert.Empty(lexed.Diagnostics)

    let parsed = Parser.parse source lexed.Tokens
    Assert.Empty(parsed.Diagnostics)

    match parsed.Syntax.Declarations with
    | [ LetDeclaration { Body = Some(Match(_, [ firstCase; secondCase ])) } ] ->
        match firstCase.Pattern with
        | AsPattern("whole", TypedPattern(TuplePattern [ NamePattern "head"; NamePattern "tail" ], typeTokens)) ->
            Assert.Equal("( Int , Int )", tokensText typeTokens)
        | other ->
            failwithf "Unexpected typed/as/tuple pattern: %A" other

        match secondCase.Pattern with
        | AnonymousRecordPattern(fields, Some(BindRecordPatternRest "rest")) ->
            match fields with
            | [ { Name = "proof"
                  IsImplicit = true
                  Pattern = NamePattern "p" }
                { Name = "name"
                  IsImplicit = false
                  Pattern = NamePattern "n" } ] -> ()
            | other ->
                failwithf "Unexpected anonymous record fields: %A" other
        | other ->
            failwithf "Unexpected anonymous record rest pattern: %A" other
    | other ->
        failwithf "Unexpected parsed declarations for pattern forms test: %A" other

[<Fact>]
let ``parser captures named constructor and variant patterns`` () =
    let source =
        createSource
            "__named_constructor_pattern__.kp"
            "module main\ntype U = (| Int | String |)\nlet demo value other = match value\n  case User { name, age = years } -> years\n  case (| item : Int |) -> item\n  case (| ..rest |) -> other"
    let lexed = Lexer.tokenize source
    Assert.Empty(lexed.Diagnostics)

    let parsed = Parser.parse source lexed.Tokens
    Assert.Empty(parsed.Diagnostics)

    match parsed.Syntax.Declarations with
    | [ TypeAliasNode _
        LetDeclaration { Body = Some(Match(_, [ firstCase; secondCase; thirdCase ])) } ] ->
        match firstCase.Pattern with
        | NamedConstructorPattern([ "User" ], fields) ->
            match fields with
            | [ { Name = "name"
                  IsImplicit = false
                  Pattern = NamePattern "name" }
                { Name = "age"
                  IsImplicit = false
                  Pattern = NamePattern "years" } ] -> ()
            | other ->
                failwithf "Unexpected named constructor fields: %A" other
        | other ->
            failwithf "Unexpected named constructor pattern: %A" other

        match secondCase.Pattern with
        | VariantPattern(BoundVariantPattern("item", Some typeTokens)) ->
            Assert.Equal("Int", tokensText typeTokens)
        | other ->
            failwithf "Unexpected typed variant pattern: %A" other

        match thirdCase.Pattern with
        | VariantPattern(RestVariantPattern "rest") -> ()
        | other ->
            failwithf "Unexpected variant rest pattern: %A" other
    | other ->
        failwithf "Unexpected parsed declarations for named constructor/variant test: %A" other

[<Fact>]
let ``compilation reports import cycles across modules`` () =
    let root = Path.GetFullPath("memory-cycle-root")
    let moduleASource =
        [
            "module a"
            "import b"
            "let a = 1"
        ]
        |> String.concat "\n"

    let moduleBSource =
        [
            "module b"
            "import a"
            "let b = 2"
        ]
        |> String.concat "\n"

    let fileSystem =
        InMemoryFileSystem(
            [
                Path.Combine(root, "a.kp"), moduleASource
                Path.Combine(root, "b.kp"), moduleBSource
            ]
        )

    let workspace =
        Compilation.parse (CompilationOptions.createWithFileSystem fileSystem root) [ root ]

    Assert.True(workspace.HasErrors, "Expected a cycle diagnostic.")

    let cycleDiagnostic =
        workspace.Diagnostics
        |> List.tryFind (fun diagnostic -> diagnostic.Code = DiagnosticCode.ImportCycle)

    Assert.True(cycleDiagnostic.IsSome, sprintf "Expected an import-cycle diagnostic, got %A" workspace.Diagnostics)

[<Fact>]
let ``parser captures expect declarations including soft keyword names and operator terms`` () =
    let sourceText =
        [
            "module demo.expect"
            "expect type type a"
            "expect trait trait a"
            "expect term (>>=) : IO a -> (a -> IO b) -> IO b"
        ]
        |> String.concat "\n"

    let _, lexed, parsed =
        lexAndParse
            "demo/expect.kp"
            sourceText

    Assert.Empty(lexed.Diagnostics)
    Assert.Empty(parsed.Diagnostics)

    match parsed.Syntax.Declarations with
    | [ ExpectDeclarationNode (ExpectTypeDeclaration typeDeclaration)
        ExpectDeclarationNode (ExpectTraitDeclaration traitDeclaration)
        ExpectDeclarationNode (ExpectTermDeclaration termDeclaration) ] ->
        Assert.Equal("type", typeDeclaration.Name)
        Assert.Equal<string list>([ "a" ], typeDeclaration.HeaderTokens |> List.map (fun token -> token.Text))

        Assert.Equal("trait", traitDeclaration.Name)
        Assert.Equal<string list>([ "a" ], traitDeclaration.HeaderTokens |> List.map (fun token -> token.Text))

        Assert.Equal(">>=", termDeclaration.Name)
        Assert.Equal<string list>(
            [ "IO"; "a"; "->"; "("; "a"; "->"; "IO"; "b"; ")"; "->"; "IO"; "b" ],
            termDeclaration.TypeTokens |> List.map (fun token -> token.Text)
        )
    | other ->
        failwithf "Unexpected declarations: %A" other

[<Fact>]
let ``type signatures normalize built in optional postfix`` () =
    let optionalInt = parseTypeText "Int?"
    let preludeOptionInt = parseTypeText "std.prelude.Option Int"

    Assert.True(TypeSignatures.definitionallyEqual optionalInt preludeOptionInt)

[<Fact>]
let ``type signatures parse intrinsic compile-time classifiers and primitive universes`` () =
    Assert.Equal(TypeSignatures.TypeIntrinsic TypeSignatures.UniverseClassifier, parseTypeText "Universe")
    Assert.Equal(TypeSignatures.TypeIntrinsic TypeSignatures.QuantityClassifier, parseTypeText "Quantity")
    Assert.Equal(TypeSignatures.TypeIntrinsic TypeSignatures.RegionClassifier, parseTypeText "Region")
    Assert.Equal(TypeSignatures.TypeIntrinsic TypeSignatures.ConstraintClassifier, parseTypeText "Constraint")
    Assert.Equal(TypeSignatures.TypeIntrinsic TypeSignatures.RecRowClassifier, parseTypeText "RecRow")
    Assert.Equal(TypeSignatures.TypeIntrinsic TypeSignatures.VarRowClassifier, parseTypeText "VarRow")
    Assert.Equal(TypeSignatures.TypeIntrinsic TypeSignatures.EffRowClassifier, parseTypeText "EffRow")
    Assert.Equal(TypeSignatures.TypeIntrinsic TypeSignatures.LabelClassifier, parseTypeText "Label")
    Assert.Equal(TypeSignatures.TypeIntrinsic TypeSignatures.EffLabelClassifier, parseTypeText "EffLabel")
    Assert.Equal(TypeSignatures.TypeUniverse None, parseTypeText "Type")
    Assert.Equal(TypeSignatures.TypeUniverse None, parseTypeText "*")
    Assert.Equal(TypeSignatures.TypeUniverse(Some(TypeSignatures.TypeLevelLiteral 0)), parseTypeText "Type0")
    Assert.Equal(TypeSignatures.TypeUniverse(Some(TypeSignatures.TypeLevelLiteral 2)), parseTypeText "Type2")
    Assert.Equal(TypeSignatures.TypeUniverse(Some(TypeSignatures.TypeVariable "u")), parseTypeText "Type u")

[<Fact>]
let ``lexer recognizes reserved effect row delimiters`` () =
    let source = createSource "__effect_row_tokens__.kp" "<[label : Console | r]>"
    let lexed = Lexer.tokenize source

    Assert.Empty(lexed.Diagnostics)
    Assert.Equal<string list>([ "<["; "label"; ":"; "Console"; "|"; "r"; "]>" ], lexed.Tokens |> List.take 7 |> List.map (fun token -> token.Text))
    Assert.Equal<TokenKind list>(
        [ LeftEffectRow; Identifier; Colon; Identifier; Operator; Identifier; RightEffectRow ],
        lexed.Tokens |> List.take 7 |> List.map (fun token -> token.Kind)
    )
    Assert.Equal(EndOfFile, lexed.Tokens |> List.last |> fun token -> token.Kind)

[<Fact>]
let ``type signatures parse effect row surface syntax and normalize entry order`` () =
    let emptyRow = parseTypeText "<[ ]>"
    let closedRow = parseTypeText "<[log : Console, state : State]>"
    let openRow = parseTypeText "<[log : Console | r]>"

    Assert.Equal(TypeSignatures.TypeEffectRow([], None), emptyRow)
    Assert.Equal(
        TypeSignatures.TypeEffectRow(
            [ { Label = TypeSignatures.TypeVariable "log"
                Effect = TypeSignatures.TypeName([ "Console" ], []) }
              { Label = TypeSignatures.TypeVariable "state"
                Effect = TypeSignatures.TypeName([ "State" ], []) } ],
            None
        ),
        closedRow
    )
    Assert.Equal(
        TypeSignatures.TypeEffectRow(
            [ { Label = TypeSignatures.TypeVariable "log"
                Effect = TypeSignatures.TypeName([ "Console" ], []) } ],
            Some(TypeSignatures.TypeVariable "r")
        ),
        openRow
    )
    Assert.True(
        TypeSignatures.definitionallyEqual
            (parseTypeText "<[b : B, a : A]>")
            (parseTypeText "<[a : A, b : B]>")
    )

[<Fact>]
let ``type signatures reject unterminated effect rows without hanging`` () =
    let source = createSource "__unterminated_effect_row_signature__.kp" "I9 <[I5] ["
    let lexed = Lexer.tokenize source

    Assert.Empty(lexed.Diagnostics)
    Assert.True(TypeSignatures.parseScheme lexed.Tokens |> Option.isNone)

[<Fact>]
let ``parser gives built in safe navigation and elvis their spec precedence`` () =
    let source = createSource "__safe_navigation_precedence__.kp" "a?.b ?: fallback"
    let lexed = Lexer.tokenize source
    let parsed = CoreParsing.parseExpression (Parser.bootstrapFixities ()) source (DiagnosticBag()) lexed.Tokens

    match parsed with
    | Some(Elvis(SafeNavigation(Name [ "a" ], { Segments = [ "b" ]; Arguments = [] }), Name [ "fallback" ])) -> ()
    | other -> failwithf "Unexpected expression shape: %A" other

[<Fact>]
let ``parser preserves syntax quotes, quote-local splices, and top level syntax splices`` () =
    let fixities = Parser.bootstrapFixities ()

    let syntaxQuoteParsed, syntaxQuoteDiagnostics =
        parseExpressionWithFixities fixities "__syntax_quote__.kp" "'{ ${macroBody} + 1 }"

    Assert.Empty(syntaxQuoteDiagnostics)

    match syntaxQuoteParsed with
    | Some(
        SyntaxQuote(
            Binary(
                SyntaxSplice(Name [ "macroBody" ]),
                "+",
                NumericLiteral(SurfaceIntegerLiteral(value, "1", None))
            )
        )
      ) ->
        Assert.Equal(BigInteger.One, value)
    | other ->
        failwithf "Unexpected syntax quote parse shape: %A" other

    let topLevelSpliceParsed, topLevelSpliceDiagnostics =
        parseExpressionWithFixities fixities "__top_level_syntax_splice__.kp" "$(build '{ 10 })"

    Assert.Empty(topLevelSpliceDiagnostics)

    match topLevelSpliceParsed with
    | Some(
        TopLevelSyntaxSplice(
            Apply(
                Name [ "build" ],
                [ SyntaxQuote(NumericLiteral(SurfaceIntegerLiteral(value, "10", None))) ]
            )
        )
      ) ->
        Assert.Equal(BigInteger(10), value)
    | other ->
        failwithf "Unexpected top level syntax splice shape: %A" other

[<Fact>]
let ``parser preserves staged code quotes and escapes`` () =
    let parsed, diagnostics =
        parseExpressionWithFixities (Parser.bootstrapFixities ()) "__code_quote__.kp" ".< .~shared + 1 >."

    Assert.Empty(diagnostics)

    match parsed with
    | Some(
        CodeQuote(
            Binary(
                CodeSplice(Name [ "shared" ]),
                "+",
                NumericLiteral(SurfaceIntegerLiteral(value, "1", None))
            )
        )
      ) ->
        Assert.Equal(BigInteger.One, value)
    | other ->
        failwithf "Unexpected code quote parse shape: %A" other

[<Fact>]
let ``operator sections prefer prefix and postfix fixities over section desugaring`` () =
    let fixities =
        Parser.bootstrapFixities ()
        |> FixityTable.add { Fixity = Postfix; Precedence = 80; OperatorName = "!" }

    let prefixParsed, prefixDiagnostics =
        parseExpressionWithFixities fixities "__operator_section_prefix__.kp" "(- x)"

    Assert.Empty(prefixDiagnostics)

    match prefixParsed with
    | Some(Unary("-", Name [ "x" ])) -> ()
    | other -> failwithf "Expected prefix parse shape for (- x), got %A" other

    let postfixParsed, postfixDiagnostics =
        parseExpressionWithFixities fixities "__operator_section_postfix__.kp" "(x !)"

    Assert.Empty(postfixDiagnostics)

    match postfixParsed with
    | Some(Apply(Name [ "!" ], [ Name [ "x" ] ])) -> ()
    | other -> failwithf "Expected postfix parse shape for (x !), got %A" other

[<Fact>]
let ``operator sections require infix fixity and enforce operand precedence floors`` () =
    let noFixityParsed, noFixityDiagnostics =
        parseExpressionWithFixities FixityTable.empty "__operator_section_requires_infix__.kp" "(% 1)"

    Assert.True(noFixityParsed.IsSome, "Parser should still return a placeholder expression on failure.")
    Assert.NotEmpty(noFixityDiagnostics)

    let equalPrecedenceParsed, equalPrecedenceDiagnostics =
        parseExpressionWithFixities (Parser.bootstrapFixities ()) "__operator_section_precedence_floor__.kp" "(+ 1 + 2)"

    Assert.True(equalPrecedenceParsed.IsSome, "Parser should still return a placeholder expression on failure.")
    Assert.NotEmpty(equalPrecedenceDiagnostics)

    let parenthesizedOperandParsed, parenthesizedOperandDiagnostics =
        parseExpressionWithFixities (Parser.bootstrapFixities ()) "__operator_section_parenthesized_operand__.kp" "(+ (1 + 2))"

    Assert.Empty(parenthesizedOperandDiagnostics)

    match parenthesizedOperandParsed with
    | Some(Lambda(parameters, Binary(Name [ generated ], "+", Binary(left, "+", right)))) ->
        Assert.Single(parameters) |> ignore
        Assert.Equal(parameters.Head.Name, generated)
        assertSurfaceIntegerLiteral 1 "1" left
        assertSurfaceIntegerLiteral 2 "2" right
    | other ->
        failwithf "Unexpected right-section parse shape: %A" other

[<Fact>]
let ``operator sections use hygienic binders instead of fixed reserved spellings`` () =
    let parsed, diagnostics =
        parseExpressionWithFixities (Parser.bootstrapFixities ()) "__operator_section_hygiene__.kp" "(__sectionArg +)"

    Assert.Empty(diagnostics)

    match parsed with
    | Some(Lambda(parameters, Binary(Name [ leftName ], "+", Name [ rightName ]))) ->
        Assert.Single(parameters) |> ignore
        Assert.Equal(parameters.Head.Name, rightName)
        Assert.Equal("__sectionArg", leftName)
        Assert.False(String.Equals("__sectionArg", rightName, StringComparison.Ordinal))
        Assert.False(String.Equals("__sectionLeft", rightName, StringComparison.Ordinal))
        Assert.False(String.Equals("__sectionRight", rightName, StringComparison.Ordinal))
    | other ->
        failwithf "Unexpected left-section hygiene parse shape: %A" other

[<Fact>]
let ``operator section hygiene does not capture user written names`` () =
    let mainSource =
        [
            "module main"
            "let __sectionArg = 41"
            "let addOuter = (__sectionArg +)"
            "let result = addOuter 1"
        ]
        |> String.concat "\n"

    let workspace, result =
        evaluateInMemoryBinding
            "memory-operator-section-hygiene-root"
            "main.result"
            [ "main.kp", mainSource ]

    Assert.False(workspace.HasErrors, sprintf "Expected no diagnostics, got %A" workspace.Diagnostics)

    match result with
    | Result.Ok value ->
        Assert.Equal("42", RuntimeValue.format value)
    | Result.Error issue ->
        failwithf "Expected hygienic operator section evaluation to succeed, got %s" issue.Message

[<Fact>]
let ``top level fixity declarations apply only from their declaration onward`` () =
    let parsed =
        parseDocumentWithFixities
            (Parser.bootstrapFixities ())
            "__fixity_scope_declaration_order__.kp"
            (String.concat
                "\n"
                [ "module main"
                  "let before = 1 %% 2"
                  "infix left 65 (%%)"
                  "let (%%) x y = x"
                  "let after = 1 %% 2" ])

    Assert.NotEmpty(parsed.Diagnostics)

    let beforeDeclaration =
        parsed.Syntax.Declarations
        |> List.choose (function
            | LetDeclaration declaration when declaration.Name = Some "before" -> Some declaration
            | _ -> None)
        |> List.exactlyOne

    let afterDeclaration =
        parsed.Syntax.Declarations
        |> List.choose (function
            | LetDeclaration declaration when declaration.Name = Some "after" -> Some declaration
            | _ -> None)
        |> List.exactlyOne

    match beforeDeclaration.Body with
    | Some(Binary _) ->
        failwith "Expected the pre-fixity declaration expression to remain unparsed as an infix application."
    | _ ->
        ()

    match afterDeclaration.Body with
    | Some(Binary(left, "%%", right)) ->
        assertSurfaceIntegerLiteral 1 "1" left
        assertSurfaceIntegerLiteral 2 "2" right
    | other ->
        failwithf "Expected the post-fixity declaration expression to parse as an infix application, got %A" other

[<Fact>]
let ``imported fixities apply only from the import declaration onward`` () =
    let workspace =
        compileInMemoryWorkspace
            "memory-import-fixity-scope-root"
            [ "ops.kp",
              String.concat
                  "\n"
                  [ "module ops"
                    "infix left 65 (%%)"
                    "let (%%) x y = x" ]
              "main.kp",
              String.concat
                  "\n"
                  [ "module main"
                    "let before = 1 %% 2"
                    "import ops.((%%))"
                    "let after = 1 %% 2" ] ]

    let mainDocument =
        tryFindDocument "main" workspace
        |> Option.defaultWith (fun () -> failwith "Expected a main module document.")

    Assert.True(
        mainDocument.Diagnostics |> List.exists (fun diagnostic -> diagnostic.Code = DiagnosticCode.ParseError),
        sprintf "Expected an early parse error before the import brought the fixity into scope, got %A" mainDocument.Diagnostics
    )

    let beforeDeclaration =
        tryFindLetDeclaration "before" mainDocument
        |> Option.defaultWith (fun () -> failwith "Expected to find the 'before' declaration.")

    let afterDeclaration =
        tryFindLetDeclaration "after" mainDocument
        |> Option.defaultWith (fun () -> failwith "Expected to find the 'after' declaration.")

    match beforeDeclaration.Body with
    | Some(Binary _) ->
        failwith "Expected the pre-import expression to remain outside the imported fixity scope."
    | _ ->
        ()

    match afterDeclaration.Body with
    | Some(Binary(left, "%%", right)) ->
        assertSurfaceIntegerLiteral 1 "1" left
        assertSurfaceIntegerLiteral 2 "2" right
    | other ->
        failwithf "Expected the post-import expression to parse with the imported fixity in scope, got %A" other

[<Fact>]
let ``bootstrap fixity table matches spec defaults for core reserved and prelude operators`` () =
    let fixities = Parser.bootstrapFixities ()

    Assert.Equal(Some(NonAssociative, 50), FixityTable.tryFindInfix "==" fixities)
    Assert.Equal(Some(NonAssociative, 40), FixityTable.tryFindInfix "~=" fixities)
    Assert.Equal(Some(NonAssociative, 40), FixityTable.tryFindInfix "/=" fixities)
    Assert.Equal(Some(RightAssociative, 30), FixityTable.tryFindInfix "&&" fixities)
    Assert.Equal(Some(RightAssociative, 20), FixityTable.tryFindInfix "||" fixities)
    Assert.Equal(Some(RightAssociative, 50), FixityTable.tryFindInfix "::" fixities)

[<Fact>]
let ``bootstrap fixities parse and/or and cons with the spec associativity`` () =
    let sourceText =
        [
            "module main"
            "let a = True && False && True"
            "let b = True || False || True"
            "let c = 1 :: 2 :: Nil"
        ]
        |> String.concat "\n"

    let _, lexed, parsed =
        lexAndParse
            "memory-bootstrap-assoc.kp"
            sourceText

    Assert.Empty(lexed.Diagnostics)
    Assert.Empty(parsed.Diagnostics)

    match parsed.Syntax.Declarations with
    | [ LetDeclaration andDecl; LetDeclaration orDecl; LetDeclaration consDecl ] ->
        match andDecl.Body with
        | Some(Binary(Name [ "True" ], "&&", Binary(Name [ "False" ], "&&", Name [ "True" ]))) -> ()
        | other -> failwithf "Unexpected && parse shape: %A" other

        match orDecl.Body with
        | Some(Binary(Name [ "True" ], "||", Binary(Name [ "False" ], "||", Name [ "True" ]))) -> ()
        | other -> failwithf "Unexpected || parse shape: %A" other

        match consDecl.Body with
        | Some(
            Binary(
                NumericLiteral(SurfaceIntegerLiteral(left, "1", None)),
                "::",
                Binary(NumericLiteral(SurfaceIntegerLiteral(middle, "2", None)), "::", Name [ "Nil" ])
            )
          ) ->
            Assert.Equal(BigInteger.One, left)
            Assert.Equal(BigInteger(2), middle)
        | other -> failwithf "Unexpected :: parse shape: %A" other
    | other ->
        failwithf "Unexpected declarations: %A" other

[<Fact>]
let ``user bare question-mark fixities do not affect reserved safe navigation or elvis tokens`` () =
    let source = createSource "__question_fixity_reserved_tokens__.kp" "a?.b ?: fallback"
    let lexed = Lexer.tokenize source
    let diagnostics = DiagnosticBag()
    let customFixities =
        Parser.bootstrapFixities ()
        |> FixityTable.add { Fixity = Postfix; Precedence = 1; OperatorName = "?" }
        |> FixityTable.add { Fixity = Infix LeftAssociative; Precedence = 99; OperatorName = "?" }

    let parsed =
        CoreParsing.parseExpression
            customFixities
            source
            diagnostics
            lexed.Tokens

    Assert.Empty(lexed.Diagnostics)
    Assert.Empty(diagnostics.Items)

    match parsed with
    | Some(Elvis(SafeNavigation(Name [ "a" ], { Segments = [ "b" ]; Arguments = [] }), Name [ "fallback" ])) -> ()
    | other -> failwithf "Unexpected reserved-token parse shape: %A" other

[<Fact>]
let ``type signatures treat lawful record reorderings as definitionally equal`` () =
    let left = parseTypeText "(y : Int, x : Int)"
    let right = parseTypeText "(x : Int, y : Int)"

    Assert.True(TypeSignatures.definitionallyEqual left right)

[<Fact>]
let ``type signature schemes parse typed forall binders and captures`` () =
    let scheme =
        parseSchemeText "forall (q : Quantity) (r : EffRow) (s : Region) (u : Universe) (a : Type u). (&[s] box : Box a) -> ((Unit -> a) captures (s))"

    Assert.Collection(
        scheme.Forall,
        (fun (binder: TypeSignatures.ForallBinder) ->
            Assert.Equal("q", binder.Name)
            Assert.Equal(QuantityZero, binder.Quantity)
            Assert.Equal(TypeSignatures.TypeIntrinsic TypeSignatures.QuantityClassifier, binder.Sort)),
        (fun (binder: TypeSignatures.ForallBinder) ->
            Assert.Equal("r", binder.Name)
            Assert.Equal(QuantityZero, binder.Quantity)
            Assert.Equal(TypeSignatures.TypeIntrinsic TypeSignatures.EffRowClassifier, binder.Sort)),
        (fun (binder: TypeSignatures.ForallBinder) ->
            Assert.Equal("s", binder.Name)
            Assert.Equal(QuantityZero, binder.Quantity)
            Assert.Equal(TypeSignatures.TypeIntrinsic TypeSignatures.RegionClassifier, binder.Sort)),
        (fun (binder: TypeSignatures.ForallBinder) ->
            Assert.Equal("u", binder.Name)
            Assert.Equal(QuantityZero, binder.Quantity)
            Assert.Equal(TypeSignatures.TypeIntrinsic TypeSignatures.UniverseClassifier, binder.Sort)),
        (fun (binder: TypeSignatures.ForallBinder) ->
            Assert.Equal("a", binder.Name)
            Assert.Equal(QuantityZero, binder.Quantity)
            Assert.Equal(TypeSignatures.TypeUniverse(Some(TypeSignatures.TypeVariable "u")), binder.Sort))
    )

[<Fact>]
let ``type signature schemes preserve dependent forall telescope structure under instantiation and alpha equality`` () =
    let left =
        parseSchemeText "forall (u : Universe) (a : Type u). a -> a"

    let right =
        parseSchemeText "forall (v : Universe) (b : Type v). b -> b"

    let instantiated = TypeSignatures.instantiate "t" 0 left

    Assert.True(TypeSignatures.schemeDefinitionallyEqual left right)

    Assert.Collection(
        instantiated.Forall,
        (fun (binder: TypeSignatures.ForallBinder) ->
            Assert.Equal("t0", binder.Name)
            Assert.Equal(TypeSignatures.TypeIntrinsic TypeSignatures.UniverseClassifier, binder.Sort)),
        (fun (binder: TypeSignatures.ForallBinder) ->
            Assert.Equal("t1", binder.Name)
            Assert.Equal(TypeSignatures.TypeUniverse(Some(TypeSignatures.TypeVariable "t0")), binder.Sort))
    )

[<Fact>]
let ``type signatures definitional equality reduces beta delta eta suspension and record projection forms`` () =
    let intType = TypeSignatures.TypeName([ "Int" ], [])
    let boolType = TypeSignatures.TypeName([ "Bool" ], [])

    let context =
        TypeSignatures.emptyDefinitionContext
        |> TypeSignatures.addTransparentDefinition [ "Id" ] [ "a" ] (TypeSignatures.TypeVariable "a")
        |> TypeSignatures.addTransparentDefinition [ "AliasInt" ] [] intType

    let betaRedex =
        TypeSignatures.TypeApply(
            TypeSignatures.TypeLambda("x", TypeSignatures.TypeUniverse None, TypeSignatures.TypeVariable "x"),
            [ intType ]
        )

    let etaExpanded =
        TypeSignatures.TypeLambda(
            "x",
            TypeSignatures.TypeUniverse None,
            TypeSignatures.TypeApply(TypeSignatures.TypeVariable "f", [ TypeSignatures.TypeVariable "x" ])
        )

    let projectedRecord =
        TypeSignatures.TypeProject(
            TypeSignatures.TypeRecord(
                [ { Name = "value"
                    Quantity = QuantityOmega
                    Type = boolType } ]
            ),
            "value"
        )

    Assert.True(TypeSignatures.definitionallyEqualIn context betaRedex intType)
    Assert.True(TypeSignatures.definitionallyEqualIn context (TypeSignatures.TypeName([ "Id" ], [ intType ])) intType)
    Assert.True(TypeSignatures.definitionallyEqualIn context (TypeSignatures.TypeName([ "AliasInt" ], [])) intType)
    Assert.True(TypeSignatures.definitionallyEqualIn context (TypeSignatures.TypeVariable "f") etaExpanded)
    Assert.True(TypeSignatures.definitionallyEqualIn context (TypeSignatures.TypeForce(TypeSignatures.TypeDelay intType)) intType)
    Assert.True(TypeSignatures.definitionallyEqualIn context (TypeSignatures.TypeForce(TypeSignatures.TypeMemo intType)) intType)
    Assert.True(TypeSignatures.definitionallyEqualIn context projectedRecord boolType)
    Assert.True(TypeSignatures.definitionallyEqualIn context (TypeSignatures.TypeRecord []) (TypeSignatures.TypeName([ "Unit" ], [])))

[<Fact>]
let ``type signatures refuse definitional equality for cyclic record telescopes`` () =
    let cyclicRecord =
        TypeSignatures.TypeRecord(
            [ { Name = "x"
                Quantity = QuantityOmega
                Type = TypeSignatures.TypeProject(TypeSignatures.TypeVariable "this", "y") }
              { Name = "y"
                Quantity = QuantityOmega
                Type = TypeSignatures.TypeProject(TypeSignatures.TypeVariable "this", "x") } ]
        )

    Assert.False(TypeSignatures.definitionallyEqual cyclicRecord cyclicRecord)

[<Fact>]
let ``type signatures unfold only conversion reducible transparent definitions`` () =
    let intType = TypeSignatures.TypeName([ "Int" ], [])

    let context =
        TypeSignatures.emptyDefinitionContext
        |> TypeSignatures.addDefinition
            [ "CheckedAlias" ]
            { ParameterNames = []
              DefinitionBody = intType
              Transparent = true
              TerminationCertified = true
              ConversionReducible = true
              CertificationSource = Some TypeSignatures.CheckedDefinition }
        |> TypeSignatures.addDefinition
            [ "AssertedOnly" ]
            { ParameterNames = []
              DefinitionBody = intType
              Transparent = true
              TerminationCertified = false
              ConversionReducible = false
              CertificationSource = Some TypeSignatures.AssertTerminatesUnverified }
        |> TypeSignatures.addDefinition
            [ "OpaqueAlias" ]
            { ParameterNames = []
              DefinitionBody = intType
              Transparent = false
              TerminationCertified = true
              ConversionReducible = false
              CertificationSource = Some TypeSignatures.CheckedDefinition }

    Assert.True(TypeSignatures.definitionallyEqualIn context (TypeSignatures.TypeName([ "CheckedAlias" ], [])) intType)
    Assert.False(TypeSignatures.definitionallyEqualIn context (TypeSignatures.TypeName([ "AssertedOnly" ], [])) intType)
    Assert.False(TypeSignatures.definitionallyEqualIn context (TypeSignatures.TypeName([ "OpaqueAlias" ], [])) intType)

[<Fact>]
let ``frontend preserves totality assertions on let declarations and gates unsafe escapes`` () =
    let allowedWorkspace =
        compileInMemoryWorkspace
            "memory-assert-terminates-root"
            [
                "main.kp",
                [
                    "@allow_assert_terminates"
                    "@allow_assert_reducible"
                    "module main"
                    "assertTerminates let keep (x : Int) : Int = x"
                    "assertReducible let uncheckedId (x : Int) : Int = x"
                ]
                |> String.concat "\n"
            ]

    Assert.False(allowedWorkspace.HasErrors, sprintf "Expected no diagnostics, got %A" allowedWorkspace.Diagnostics)

    let mainDocument =
        allowedWorkspace.Documents
        |> List.find (fun document -> document.ModuleName = Some [ "main" ])

    let keepDefinition =
        mainDocument.Syntax.Declarations
        |> List.pick (function
            | LetDeclaration declaration when declaration.Name = Some "keep" -> Some declaration
            | _ -> None)

    let uncheckedDefinition =
        mainDocument.Syntax.Declarations
        |> List.pick (function
            | LetDeclaration declaration when declaration.Name = Some "uncheckedId" -> Some declaration
            | _ -> None)

    Assert.Equal(Some AssertTerminatesAssertion, keepDefinition.TotalityAssertion)
    Assert.Equal(Some AssertReducibleAssertion, uncheckedDefinition.TotalityAssertion)

    let blockedTerminatesWorkspace =
        compileInMemoryWorkspace
            "memory-blocked-assert-terminates-root"
            [
                "main.kp",
                [
                    "module main"
                    "assertTerminates let keep (x : Int) : Int = x"
                ]
                |> String.concat "\n"
            ]

    Assert.True(blockedTerminatesWorkspace.HasErrors, "Expected assertTerminates to require allow_assert_terminates.")
    Assert.Contains(
        blockedTerminatesWorkspace.Diagnostics,
        fun diagnostic ->
            diagnostic.Code = DiagnosticCode.FrontendValidation
            && diagnostic.Message.Contains("allow_assert_terminates", StringComparison.Ordinal)
    )

    let blockedReducibleWorkspace =
        compileInMemoryWorkspace
            "memory-blocked-assert-reducible-root"
            [
                "main.kp",
                [
                    "module main"
                    "assertReducible let uncheckedId (x : Int) : Int = x"
                ]
                |> String.concat "\n"
            ]

    Assert.True(blockedReducibleWorkspace.HasErrors, "Expected assertReducible to require allow_assert_reducible.")
    Assert.Contains(
        blockedReducibleWorkspace.Diagnostics,
        fun diagnostic ->
            diagnostic.Code = DiagnosticCode.FrontendValidation
            && diagnostic.Message.Contains("allow_assert_reducible", StringComparison.Ordinal)
    )

[<Fact>]
let ``compilation injects bundled std prelude and satisfies its intrinsic expects`` () =
    let mainSource =
        [
            "module main"
            "flag : Bool"
            "let flag = not False"
        ]
        |> String.concat "\n"

    let workspace =
        compileInMemoryWorkspace
            "memory-stdlib-root"
            [ "main.kp", mainSource ]

    Assert.False(workspace.HasErrors, sprintf "Expected no diagnostics, got %A" workspace.Diagnostics)

    let moduleNames =
        workspace.Documents
        |> List.choose (fun document -> document.ModuleName |> Option.map SyntaxFacts.moduleNameToText)

    Assert.Contains<string>(Stdlib.PreludeModuleText, moduleNames)
    Assert.Contains<string>("main", moduleNames)

    let preludeDocument =
        workspace.Documents
        |> List.find (fun document -> document.ModuleName = Some Stdlib.PreludeModuleName)

    let intrinsicExpect =
        preludeDocument.Syntax.Declarations
        |> List.tryPick (function
            | ExpectDeclarationNode (ExpectTermDeclaration declaration) when declaration.Name = "not" ->
                Some declaration
            | _ ->
                None)

    Assert.True(intrinsicExpect.IsSome, "Expected bundled std.prelude to declare intrinsic term 'not'.")

[<Fact>]
let ``supported backend profiles satisfy the current prelude intrinsic surface`` () =
    let mainSource =
        [
            "module main"
            "flag : Bool"
            "let flag = not False"
        ]
        |> String.concat "\n"

    for backendProfile in [ "interpreter"; "dotnet"; "dotnet-il"; "zig"; "zigcc" ] do
        let workspace =
            compileInMemoryWorkspaceWithBackend
                $"memory-supported-intrinsics-{backendProfile}"
                backendProfile
                [ "main.kp", mainSource ]

        Assert.False(
            workspace.HasErrors,
            $"Expected backend profile '{backendProfile}' to satisfy the current prelude intrinsics, got {workspace.Diagnostics}."
        )

        let preludeModule =
            workspace.KCore
            |> List.find (fun moduleDump -> moduleDump.Name = Stdlib.PreludeModuleText)

        Assert.Contains("not", preludeModule.IntrinsicTerms)
        Assert.Contains("printInt", preludeModule.IntrinsicTerms)

[<Fact>]
let ``KCore preserves compile time binders and structured capture aware types`` () =
    let workspace =
        compileInMemoryWorkspace
            "memory-kcore-structured-types-root"
            [
                "main.kp",
                [
                    "module main"
                    "makeGetter : forall (s : Region). (&[s] x : Int) -> ((Unit -> Int) captures (s))"
                    "let makeGetter (@0 s : Region) (&[s] x : Int) : ((Unit -> Int) captures (s)) ="
                    "    \\() -> x"
                ]
                |> String.concat "\n"
            ]

    let binding =
        workspace.KCore
        |> List.find (fun moduleDump -> moduleDump.Name = "main")
        |> fun moduleDump ->
            moduleDump.Declarations
            |> List.pick (fun declaration ->
                match declaration.Binding with
                | Some binding when binding.Name = Some "makeGetter" -> Some binding
                | _ -> None)

    Assert.Equal(2, binding.Parameters.Length)

    let regionParameter = binding.Parameters[0]
    Assert.Equal("s", regionParameter.Name)
    Assert.Equal(Some QuantityZero, regionParameter.Quantity)
    Assert.True(regionParameter.IsImplicit)
    Assert.Equal(Some(parseTypeText "Region"), regionParameter.Type)

    let borrowedParameter = binding.Parameters[1]
    Assert.Equal("x", borrowedParameter.Name)
    Assert.Equal(Some(QuantityBorrow(Some "s")), borrowedParameter.Quantity)
    Assert.Equal(Some(parseTypeText "Int"), borrowedParameter.Type)

    Assert.Equal(
        Some(parseTypeText "((Unit -> Int) captures (s))"),
        binding.ReturnType
    )

[<Fact>]
let ``KCore lowers calls as application spines with explicit arguments`` () =
    let workspace =
        compileInMemoryWorkspace
            "memory-kcore-app-spine-root"
            [
                "main.kp",
                [
                    "module main"
                    "id : Int -> Int"
                    "let id value = value"
                    "let answer = id 42"
                ]
                |> String.concat "\n"
            ]

    let answerBinding =
        workspace.KCore
        |> List.find (fun moduleDump -> moduleDump.Name = "main")
        |> fun moduleDump ->
            moduleDump.Declarations
            |> List.pick (fun declaration ->
                match declaration.Binding with
                | Some binding when binding.Name = Some "answer" -> Some binding
                | _ -> None)

    match answerBinding.Body with
    | Some(
        KCoreAppSpine(
            KCoreName [ "id" ],
            [ { ArgumentKind = KCoreExplicitArgument
                Expression = KCoreLiteral(LiteralValue.Integer 42L) } ]
        )
      ) -> ()
    | other ->
        failwithf "Expected KCore app spine for answer, got %A" other

[<Fact>]
let ``prefixed strings elaborate through Elab-backed prelude handlers`` () =
    let workspace =
        compileInMemoryWorkspace
            "memory-prefixed-string-elab-handler-root"
            [
                "main.kp",
                [
                    "module main"
                    "value : String"
                    "let value = f\"123\""
                ]
                |> String.concat "\n"
            ]

    Assert.False(workspace.HasErrors, $"Expected Elab-backed prefixed string elaboration to succeed, got {workspace.Diagnostics}.")

[<Fact>]
let ``prefixed strings reject runtime dictionary parameters as handlers`` () =
    let workspace =
        compileInMemoryWorkspace
            "memory-prefixed-string-runtime-handler-root"
            [
                "main.kp",
                [
                    "module main"
                    "value : Dict (InterpolatedMacro String) -> String"
                    "let value prefix = prefix\"123\""
                ]
                |> String.concat "\n"
            ]

    Assert.True(workspace.HasErrors, "Expected runtime dictionary parameters to be rejected as prefixed-string handlers.")
    Assert.Contains(workspace.Diagnostics, fun diagnostic -> diagnostic.Code = DiagnosticCode.TypeEqualityMismatch && diagnostic.Message.Contains("Elab", StringComparison.Ordinal))

[<Fact>]
let ``prefixed strings reject unresolved interpolation names and non-macro prefixes`` () =
    let workspace =
        compileInMemoryWorkspace
            "memory-prefixed-string-invalid-prefix-root"
            [
                "main.kp",
                [
                    "module main"
                    ""
                    "I1 : Int"
                    "let I1 = I1\"${I1\" $i0 \"}\""
                ]
                |> String.concat "\n"
            ]

    Assert.True(workspace.HasErrors, "Expected malformed prefixed-string macro usage to be rejected in frontend validation.")
    Assert.Contains(workspace.Diagnostics, fun diagnostic -> diagnostic.Code = DiagnosticCode.NameUnresolved && diagnostic.Message.Contains("'i0'", StringComparison.Ordinal))
    Assert.Contains(workspace.Diagnostics, fun diagnostic -> diagnostic.Code = DiagnosticCode.TypeEqualityMismatch && diagnostic.Message.Contains("InterpolatedMacro", StringComparison.Ordinal))

[<Fact>]
let ``prefixed string prefixes respect lexical shadowing over prelude dictionaries`` () =
    let workspace =
        compileInMemoryWorkspace
            "memory-prefixed-string-shadowing-root"
            [
                "main.kp",
                [
                    "module main"
                    ""
                    "bad : Int -> String"
                    "let bad f = f\"hi\""
                ]
                |> String.concat "\n"
            ]

    Assert.True(workspace.HasErrors, "Expected a local value that shadows a prelude string macro dictionary to be rejected.")
    Assert.Contains(workspace.Diagnostics, fun diagnostic -> diagnostic.Code = DiagnosticCode.TypeEqualityMismatch && diagnostic.Message.Contains("InterpolatedMacro", StringComparison.Ordinal))
    Assert.DoesNotContain(workspace.Diagnostics, fun diagnostic -> diagnostic.Code = DiagnosticCode.NameUnresolved && diagnostic.Message.Contains("'f'", StringComparison.Ordinal))

[<Fact>]
let ``KCore lowers prefixed strings through buildInterpolated macro splices`` () =
    let workspace =
        compileInMemoryWorkspace
            "memory-prefixed-string-kcore-root"
            [
                "main.kp",
                [
                    "module main"
                    "name : String"
                    "let name = \"done\""
                    "value : String"
                    "let value = f\"pre${name : %04d}post\""
                ]
                |> String.concat "\n"
            ]

    let valueBinding =
        workspace.KCore
        |> List.find (fun moduleDump -> moduleDump.Name = "main")
        |> fun moduleDump ->
            moduleDump.Declarations
            |> List.pick (fun declaration ->
                match declaration.Binding with
                | Some binding when binding.Name = Some "value" -> Some binding
                | _ -> None)

    match valueBinding.Body with
    | Some(
        KCoreTopLevelSyntaxSplice(
            KCoreTraitCall(
                "InterpolatedMacro",
                "buildInterpolated",
                KCoreName [ "f" ],
                [ fragments ]
            )
        )
      ) ->
        let rec flattenList expression =
            match expression with
            | KCoreName [ "Nil" ] -> []
            | KCoreAppSpine(
                KCoreName [ "Cons" ],
                [ { Expression = head }
                  { Expression = tail } ]
              ) ->
                head :: flattenList tail
            | other ->
                failwithf "Expected Cons/Nil fragment list, got %A" other

        match flattenList fragments with
        | [ KCoreAppSpine(KCoreName [ "Lit" ], [ { Expression = KCoreLiteral(LiteralValue.String "pre") } ])
            KCoreAppSpine(
                KCoreName [ "InterpFmt" ],
                [ _
                  { Expression = KCoreSyntaxQuote(KCoreName [ "name" ]) }
                  { Expression = KCoreLiteral(LiteralValue.String "%04d") } ]
              )
            KCoreAppSpine(KCoreName [ "Lit" ], [ { Expression = KCoreLiteral(LiteralValue.String "post") } ]) ] -> ()
        | other ->
            failwithf "Unexpected macro fragment list: %A" other
    | other ->
        failwithf "Expected prefixed string lowering through top-level macro splice, got %A" other

[<Fact>]
let ``syntax quotes defer linearity checking until splice elaboration`` () =
    let workspace =
        compileInMemoryWorkspace
            "memory-syntax-quote-defers-linearity-root"
            [
                "main.kp",
                [
                    "module main"
                    "quoted : (1 x : Int) -> Syntax Int"
                    "let quoted (1 x : Int) = '{ x + x }"
                ]
                |> String.concat "\n"
            ]

    Assert.False(workspace.HasErrors, $"Expected linearity inside an unspliced syntax quote to be deferred, got {workspace.Diagnostics}.")

[<Fact>]
let ``top level syntax splices still charge quoted linear uses at the splice site`` () =
    let workspace =
        compileInMemoryWorkspace
            "memory-top-level-splice-linearity-root"
            [
                "main.kp",
                [
                    "module main"
                    "bad : (1 x : Int) -> Int"
                    "let bad (1 x : Int) = $('{ x + x })"
                ]
                |> String.concat "\n"
            ]

    Assert.True(workspace.HasErrors, "Expected splicing duplicated linear use to fail at the splice site.")
    Assert.Contains(workspace.Diagnostics, fun diagnostic -> diagnostic.Code = DiagnosticCode.QttLinearOveruse)

[<Fact>]
let ``backend profile aliases normalize to the effective backend identity`` () =
    let workspace =
        compileInMemoryWorkspaceWithBackend
            "memory-backend-alias-root"
            "zigcc"
            [
                "main.kp",
                [
                    "module main"
                    "let answer = 42"
                ]
                |> String.concat "\n"
            ]

    Assert.Equal("zig", workspace.BackendProfile)
    Assert.Equal("bootstrap-prelude-v2", workspace.BackendIntrinsicIdentity)

[<Fact>]
let ``bundled bootstrap prelude exposes the normative minimum surface and IO shape`` () =
    let workspace =
        compileInMemoryWorkspace
            "memory-bootstrap-prelude-root"
            [
                "main.kp",
                [
                    "module main"
                    "let answer = 42"
                ]
                |> String.concat "\n"
            ]

    let preludeDocument =
        workspace.Documents
        |> List.find (fun document -> document.ModuleName = Some Stdlib.PreludeModuleName)

    let expectTypes =
        preludeDocument.Syntax.Declarations
        |> List.choose (function
            | ExpectDeclarationNode (ExpectTypeDeclaration declaration) -> Some declaration.Name
            | _ -> None)
        |> Set.ofList

    let dataTypes =
        preludeDocument.Syntax.Declarations
        |> List.choose (function
            | DataDeclarationNode declaration -> Some declaration.Name
            | _ -> None)
        |> Set.ofList

    let aliasTypes =
        preludeDocument.Syntax.Declarations
        |> List.choose (function
            | TypeAliasNode declaration -> Some declaration.Name
            | _ -> None)
        |> Set.ofList

    let exportedTypes =
        Set.unionMany [ expectTypes; dataTypes; aliasTypes ]

    let expectTraits =
        preludeDocument.Syntax.Declarations
        |> List.choose (function
            | ExpectDeclarationNode (ExpectTraitDeclaration declaration) -> Some declaration.Name
            | _ -> None)
        |> Set.ofList

    let declaredTraits =
        preludeDocument.Syntax.Declarations
        |> List.choose (function
            | TraitDeclarationNode declaration -> Some declaration.Name
            | _ -> None)
        |> Set.ofList

    let exportedTraits =
        Set.union expectTraits declaredTraits

    let expectTerms =
        preludeDocument.Syntax.Declarations
        |> List.choose (function
            | ExpectDeclarationNode (ExpectTermDeclaration declaration) -> Some declaration.Name
            | _ -> None)
        |> Set.ofList

    let declaredTerms =
        preludeDocument.Syntax.Declarations
        |> List.choose (function
            | SignatureDeclaration declaration -> Some declaration.Name
            | LetDeclaration declaration -> declaration.Name
            | ProjectionDeclarationNode declaration -> Some declaration.Name
            | _ -> None)
        |> Set.ofList

    let exportedTerms =
        Set.union expectTerms declaredTerms

    let constructors =
        preludeDocument.Syntax.Declarations
        |> List.choose (function
            | DataDeclarationNode declaration -> Some declaration.Constructors
            | _ -> None)
        |> List.collect id
        |> List.map (fun constructor -> constructor.Name)
        |> Set.ofList

    let requiredTypes =
        Set.ofList
            [ "Unit"
              "Void"
              "Bool"
              "Char"
              "String"
              "Int"
              "Nat"
              "Integer"
              "Float"
              "Double"
              "Real"
              "Bytes"
              "Ordering"
              "Syntax"
              "SyntaxFragment"
              "Elab"
              "Query"
              "RawComprehension"
              "ComprehensionPlan"
              "Option"
              "Result"
              "List"
              "Array"
              "Set"
              "Map"
              "Res"
              "Match"
              "Dec"
              "Dict"
              "WellFounded"
              "Acc"
              "IO"
              "UIO"
              "Code"
              "ClosedCode"
              "Fiber"
              "FiberId"
              "InterruptTag"
              "InterruptCause"
              "DefectTag"
              "DefectInfo"
              "Exit"
              "Cause"
              "Scope"
              "Monitor"
              "FiberRef"
              "Promise"
              "STM"
              "TVar"
              "Duration"
              "Instant"
              "TimeoutError"
              "RaceResult"
              "Thunk"
              "Need"
              "Regex"
              "=" ]

    let requiredTraits =
        Set.ofList
            [ "Equiv"
              "Eq"
              "Ord"
              "Show"
              "Shareable"
              "Functor"
              "Applicative"
              "Monad"
              "Alternative"
              "Foldable"
              "Traversable"
              "Filterable"
              "FilterMap"
              "Monoid"
              "Iterator"
              "InterpolatedMacro"
              "Lift"
              "IntoQuery"
              "FromComprehensionPlan"
              "FromComprehensionRaw"
              "WellFoundedRelation"
              "FromInteger"
              "FromFloat"
              "FromString"
              "MonadError"
              "MonadFinally"
              "MonadResource"
              "MonadRef"
              "Releasable" ]

    let requiredTerms =
        Set.ofList
            [ "witness"
              "summon"
              "pure"
              ">>="
              ">>"
              "|>"
              "<|"
              "not"
              "and"
              "or"
              "force"
              "empty"
              "<|>"
              "orElse"
              "negate"
              "absurd"
              "subst"
              "sym"
              "trans"
              "cong"
              "measureRelation"
              "lexRelation"
              "floatEq"
              "runPure"
              "sandbox"
              "unsandbox"
              "fork"
              "forkDaemon"
              "await"
              "join"
              "interrupt"
              "interruptFork"
              "interruptAs"
              "interruptForkAs"
              "fiberId"
              "currentFiberId"
              "getFiberLabel"
              "setFiberLabel"
              "locallyFiberLabel"
              "cede"
              "blocking"
              "poll"
              "uninterruptible"
              "mask"
              "ensuring"
              "acquireRelease"
              "newScope"
              "withScope"
              "forkIn"
              "shutdownScope"
              "monitor"
              "awaitMonitor"
              "demonitor"
              "newFiberRef"
              "getFiberRef"
              "setFiberRef"
              "locallyFiberRef"
              "newPromise"
              "awaitPromiseExit"
              "awaitPromise"
              "completePromise"
              "nowMonotonic"
              "sleepFor"
              "sleepUntil"
              "timeout"
              "race"
              "atomically"
              "newTVar"
              "readTVar"
              "writeTVar"
              "retry"
              "check"
              "f"
              "re"
              "b"
              "type"
              "println"
              "print"
              "printInt"
              "printString"
              "primitiveIntToString"
              "closeCode"
              "genlet"
              "runCode"
              "newRef"
              "readRef"
              "writeRef" ]

    let requiredConstructors =
        Set.ofList
            [ "True"
              "False"
              "Unit"
              "None"
              "Some"
              "Ok"
              "Err"
              "Nil"
              "::"
              ":&"
              "LT"
              "EQ"
              "GT"
              "Hit"
              "Miss"
              "Yes"
              "No"
              "Success"
              "Failure"
              "Fail"
              "Interrupt"
              "Defect"
              "Both"
              "Then"
              "Requested"
              "ScopeShutdown"
              "TimedOut"
              "RaceLost"
              "External"
              "Custom"
              "InterruptCause"
              "Panic"
              "AssertionFailed"
              "ArithmeticFault"
              "StackOverflow"
              "OutOfMemory"
              "HostFailure"
              "ForeignContractViolation"
              "UnhandledChildFailure"
              "OtherDefect"
              "DefectInfo"
              "Timeout"
              "LeftWins"
              "RightWins"
              "Lit"
              "Interp"
              "InterpFmt"
              "refl"
              ]

    Assert.True(Set.isSubset requiredTypes exportedTypes, $"Missing prelude types: {Set.difference requiredTypes exportedTypes}")
    Assert.True(Set.isSubset requiredTraits exportedTraits, $"Missing prelude traits: {Set.difference requiredTraits exportedTraits}")
    Assert.True(Set.isSubset requiredTerms exportedTerms, $"Missing prelude terms: {Set.difference requiredTerms exportedTerms}")

    Assert.True(
        Set.isSubset requiredConstructors constructors,
        $"Missing prelude constructors: {Set.difference requiredConstructors constructors}"
    )

    let ioHeaderText =
        preludeDocument.Syntax.Declarations
        |> List.pick (function
            | ExpectDeclarationNode (ExpectTypeDeclaration declaration)
                when String.Equals(declaration.Name, "IO", StringComparison.Ordinal) ->
                Some(tokensText declaration.HeaderTokens)
            | _ -> None)

    let expectTermTypes =
        preludeDocument.Syntax.Declarations
        |> List.choose (function
            | ExpectDeclarationNode (ExpectTermDeclaration declaration) ->
                Some(declaration.Name, tokensText declaration.TypeTokens)
            | _ ->
                None)
        |> Map.ofList

    let traitMemberTypes =
        preludeDocument.Syntax.Declarations
        |> List.choose (function
            | TraitDeclarationNode declaration ->
                Some(
                    declaration.Name,
                    declaration.Members
                    |> List.choose (fun memberDeclaration ->
                        memberDeclaration.Name
                        |> Option.map (fun memberName -> memberName, tokensText memberDeclaration.Tokens))
                    |> Map.ofList
                )
            | _ ->
                None)
        |> Map.ofList

    let uioAliasHeaderText, uioAliasBodyText =
        preludeDocument.Syntax.Declarations
        |> List.pick (function
            | TypeAliasNode declaration when String.Equals(declaration.Name, "UIO", StringComparison.Ordinal) ->
                Some(
                    tokensText declaration.HeaderTokens,
                    declaration.BodyTokens |> Option.map tokensText |> Option.defaultValue ""
                )
            | _ -> None)

    Assert.Equal("( e : Type ) ( a : Type )", ioHeaderText)
    Assert.Equal("( a : Type )", uioAliasHeaderText)
    Assert.Equal("IO Void a", uioAliasBodyText)
    Assert.Equal("buildInterpolated : List SyntaxFragment -> Elab ( Syntax t )", traitMemberTypes["InterpolatedMacro"]["buildInterpolated"])
    Assert.Equal("Elab ( Dict ( InterpolatedMacro String ) )", expectTermTypes["f"])
    Assert.Equal("Elab ( Dict ( InterpolatedMacro String ) )", expectTermTypes["re"])
    Assert.Equal("Elab ( Dict ( InterpolatedMacro String ) )", expectTermTypes["b"])
    Assert.Equal("Elab ( Dict ( InterpolatedMacro Type ) )", expectTermTypes["type"])
    Assert.Equal("forall ( @ 0 t : Type ) . Code t -> Option ( ClosedCode t )", expectTermTypes["closeCode"])
    Assert.Equal("forall ( @ 0 t : Type ) . Code t -> Code t", expectTermTypes["genlet"])
    Assert.Equal("ClosedCode t -> UIO t", expectTermTypes["runCode"])
    Assert.Equal("( 1 value : a ) -> UIO a", expectTermTypes["pure"])
    Assert.Equal("UIO a -> ( a -> UIO b ) -> UIO b", expectTermTypes[">>="])
    Assert.Equal("UIO a -> UIO b -> UIO b", expectTermTypes[">>"])
    Assert.Equal("String -> UIO Unit", expectTermTypes["println"])
    Assert.Equal("String -> UIO Unit", expectTermTypes["print"])
    Assert.Equal("Int -> UIO Unit", expectTermTypes["printInt"])
    Assert.Equal("String -> UIO Unit", expectTermTypes["printString"])
    Assert.Equal("a -> UIO ( Ref a )", expectTermTypes["newRef"])
    Assert.Equal("Ref a -> UIO a", expectTermTypes["readRef"])
    Assert.Equal("Ref a -> a -> UIO Unit", expectTermTypes["writeRef"])

[<Fact>]
let ``backend intrinsic bootstrap contract is derived from bundled prelude expectations with explicit module local supplement`` () =
    let preludeSource =
        createSource Stdlib.BundledPreludeVirtualPath (Stdlib.loadBundledPreludeText ())

    let preludeLexed = Lexer.tokenize preludeSource
    let preludeParsed = Parser.parse preludeSource preludeLexed.Tokens

    Assert.Empty(preludeLexed.Diagnostics)
    Assert.Empty(preludeParsed.Diagnostics)

    let expectTypes =
        preludeParsed.Syntax.Declarations
        |> List.choose (function
            | ExpectDeclarationNode (ExpectTypeDeclaration declaration) -> Some declaration.Name
            | _ -> None)
        |> Set.ofList

    let expectTraits =
        preludeParsed.Syntax.Declarations
        |> List.choose (function
            | ExpectDeclarationNode (ExpectTraitDeclaration declaration) -> Some declaration.Name
            | _ -> None)
        |> Set.ofList

    let expectTerms =
        preludeParsed.Syntax.Declarations
        |> List.choose (function
            | ExpectDeclarationNode (ExpectTermDeclaration declaration) -> Some declaration.Name
            | _ -> None)
        |> Set.ofList

    let intrinsicSet = Stdlib.intrinsicSetForBackendProfile "interpreter"

    Assert.Equal<string>(expectTypes |> Set.toList, intrinsicSet.TypeNames |> Set.toList)
    Assert.Equal<string>(expectTraits |> Set.toList, intrinsicSet.TraitNames |> Set.toList)
    Assert.Equal<string>(expectTerms |> Set.toList, intrinsicSet.PreludeTermNames |> Set.toList)
    Assert.DoesNotContain("openFile", intrinsicSet.PreludeTermNames)

    let expectedModuleLocalTerms =
        Set.ofList [ "openFile"; "primitiveReadData"; "readData"; "primitiveCloseFile" ]

    Assert.Equal<string>(expectedModuleLocalTerms |> Set.toList, intrinsicSet.ModuleLocalTermNames |> Set.toList)
    Assert.Equal<string>(
        expectedModuleLocalTerms |> Set.toList,
        Stdlib.intrinsicTermNamesAvailableInModule "interpreter" [ "main" ] |> Set.toList
    )

[<Fact>]
let ``bundled prelude bootstrap fixities are derived from leading prelude declarations`` () =
    let preludeSource =
        createSource Stdlib.BundledPreludeVirtualPath (Stdlib.loadBundledPreludeText ())

    let preludeLexed = Lexer.tokenize preludeSource
    let preludeParsed = Parser.parseWithInitialFixities FixityTable.empty preludeSource preludeLexed.Tokens

    Assert.Empty(preludeLexed.Diagnostics)
    Assert.Empty(preludeParsed.Diagnostics)

    let rec splitLeadingFixities collected declarations =
        match declarations with
        | FixityDeclarationNode declaration :: rest ->
            splitLeadingFixities (declaration :: collected) rest
        | _ ->
            List.rev collected, declarations

    let bootstrapDeclarations, remainingDeclarations =
        splitLeadingFixities [] preludeParsed.Syntax.Declarations

    Assert.NotEmpty(bootstrapDeclarations)
    Assert.DoesNotContain(remainingDeclarations, function | FixityDeclarationNode _ -> true | _ -> false)

    let bootstrapFixities =
        bootstrapDeclarations
        |> List.fold (fun table declaration -> FixityTable.add declaration table) FixityTable.empty

    let userSourceText =
        [
            "module main"
            "let result = 1 + 2 * 3"
        ]
        |> String.concat "\n"

    let userSource =
        createSource "memory-bootstrap-fixity-root/main.kp" userSourceText

    let userLexed = Lexer.tokenize userSource
    let userParsed = Parser.parseWithInitialFixities bootstrapFixities userSource userLexed.Tokens

    Assert.Empty(userLexed.Diagnostics)
    Assert.Empty(userParsed.Diagnostics)

    match userParsed.Syntax.Declarations with
    | [ LetDeclaration declaration ] ->
        match declaration.Body with
        | Some(
            Binary(
                left,
                "+",
                Binary(
                    innerLeft,
                    "*",
                    innerRight
                )
            )
          ) ->
            assertSurfaceIntegerLiteral 1 "1" left
            assertSurfaceIntegerLiteral 2 "2" innerLeft
            assertSurfaceIntegerLiteral 3 "3" innerRight
        | other -> failwithf "Unexpected bootstrap fixity parse shape: %A" other
    | other ->
        failwithf "Unexpected declarations when parsing with bootstrap prelude fixities: %A" other

[<Fact>]
let ``implicit prelude import models the wildcard and constructor subset separately`` () =
    let imports = Stdlib.implicitImportsFor (Some [ "main" ])

    match imports with
    | [ wildcardImport; constructorImport ] ->
        match wildcardImport.Source, wildcardImport.Selection with
        | Dotted moduleName, All ->
            Assert.Equal<string list>(Stdlib.PreludeModuleName, moduleName)
        | other ->
            failwithf "Unexpected implicit wildcard prelude import: %A" other

        match constructorImport.Source, constructorImport.Selection with
        | Dotted moduleName, Items items ->
            Assert.Equal<string list>(Stdlib.PreludeModuleName, moduleName)

            let actualItems =
                items
                |> List.map (fun item -> item.Namespace, item.Name)

            let expectedItems =
                [
                    Some ImportNamespace.Constructor, "True"
                    Some ImportNamespace.Constructor, "False"
                    Some ImportNamespace.Constructor, "None"
                    Some ImportNamespace.Constructor, "Some"
                    Some ImportNamespace.Constructor, "Ok"
                    Some ImportNamespace.Constructor, "Err"
                    Some ImportNamespace.Constructor, "Nil"
                    Some ImportNamespace.Constructor, "::"
                    Some ImportNamespace.Constructor, ":&"
                    Some ImportNamespace.Constructor, "LT"
                    Some ImportNamespace.Constructor, "EQ"
                    Some ImportNamespace.Constructor, "GT"
                    Some ImportNamespace.Constructor, "Reusable"
                    Some ImportNamespace.Constructor, "OneShot"
                    Some ImportNamespace.Constructor, "QZero"
                    Some ImportNamespace.Constructor, "QOne"
                    Some ImportNamespace.Constructor, "QZeroOrOne"
                    Some ImportNamespace.Constructor, "QOneOrMore"
                    Some ImportNamespace.Constructor, "QZeroOrMore"
                    Some ImportNamespace.Constructor, "QueryMode"
                    Some ImportNamespace.Constructor, "refl"
                ]

            Assert.Equal<(ImportNamespace option * string) list>(expectedItems, actualItems)
        | other ->
            failwithf "Unexpected implicit prelude constructor import: %A" other
    | other ->
        failwithf "Expected two implicit prelude import specs, got %A" other

[<Fact>]
let ``unknown backend profiles leave prelude expects unsatisfied`` () =
    let mainSource =
        [
            "module main"
            "flag : Bool"
            "let flag = not False"
        ]
        |> String.concat "\n"

    let workspace =
        compileInMemoryWorkspaceWithBackend
            "memory-unsupported-intrinsics-root"
            "custom-backend"
            [ "main.kp", mainSource ]

    Assert.True(workspace.HasErrors, "Expected an unknown backend profile to leave prelude expects unsatisfied.")

    let matchingDiagnostic =
        workspace.Diagnostics
        |> List.tryFind (fun diagnostic -> diagnostic.Code = DiagnosticCode.ExpectUnsatisfied)

    Assert.True(matchingDiagnostic.IsSome, sprintf "Expected an unsatisfied prelude-term diagnostic, got %A" workspace.Diagnostics)

    let preludeModule =
        workspace.KCore
        |> List.find (fun moduleDump -> moduleDump.Name = Stdlib.PreludeModuleText)

    Assert.DoesNotContain("not", preludeModule.IntrinsicTerms)
    Assert.DoesNotContain("printInt", preludeModule.IntrinsicTerms)

[<Fact>]
let ``compilation reports unsatisfied expect declarations outside std prelude`` () =
    let mainSource =
        [
            "module main"
            "expect term mystery : Int"
        ]
        |> String.concat "\n"

    let workspace =
        compileInMemoryWorkspace
            "memory-unsatisfied-expect-root"
            [ "main.kp", mainSource ]

    Assert.True(workspace.HasErrors, "Expected an unsatisfied expect diagnostic.")

    let diagnostic =
        workspace.Diagnostics
        |> List.tryFind (fun item -> item.Code = DiagnosticCode.ExpectUnsatisfied)

    Assert.True(diagnostic.IsSome, sprintf "Expected an unsatisfied expect diagnostic, got %A" workspace.Diagnostics)

[<Fact>]
let ``compilation reports unsatisfied top level signatures without same file definitions`` () =
    let mainSource =
        [
            "module main"
            "undefined_string : String"
        ]
        |> String.concat "\n"

    let workspace =
        compileInMemoryWorkspace
            "memory-unsatisfied-signature-root"
            [ "main.kp", mainSource ]

    Assert.True(workspace.HasErrors, "Expected an unsatisfied top-level signature diagnostic.")

    let diagnostic =
        workspace.Diagnostics
        |> List.tryFind (fun item -> item.Code = DiagnosticCode.SignatureUnsatisfied)

    Assert.True(diagnostic.IsSome, sprintf "Expected an unsatisfied top-level signature diagnostic, got %A" workspace.Diagnostics)

[<Fact>]
let ``top level signatures are satisfied only by same file definitions`` () =
    let headerSource =
        [
            "module main"
            "answer : Int"
        ]
        |> String.concat "\n"

    let definitionSource =
        [
            "module main"
            "let answer : Int = 42"
        ]
        |> String.concat "\n"

    let workspace =
        compileInMemoryWorkspace
            "memory-same-module-fragments-signature-root"
            [
                "a.kp", headerSource
                "b.kp", definitionSource
            ]

    Assert.True(workspace.HasErrors, "Expected the header-only fragment to remain unsatisfied.")

    let matchingDiagnostics =
        workspace.Diagnostics
        |> List.filter (fun item -> item.Code = DiagnosticCode.SignatureUnsatisfied)

    Assert.Single(matchingDiagnostics)

[<Fact>]
let ``parser treats a following signature as a top level boundary after a let body`` () =
    let sourceText =
        [
            "module demo.boundary"
            "flag : Bool"
            "let flag = not False"
            "message : String"
            "let message = \"ready\""
        ]
        |> String.concat "\n"

    let _, lexed, parsed =
        lexAndParse
            "demo/boundary.kp"
            sourceText

    Assert.Empty(lexed.Diagnostics)
    Assert.Empty(parsed.Diagnostics)

    match parsed.Syntax.Declarations with
    | [ SignatureDeclaration flagSignature
        LetDeclaration flagDefinition
        SignatureDeclaration messageSignature
        LetDeclaration messageDefinition ] ->
        Assert.Equal("flag", flagSignature.Name)
        Assert.Equal(Some "flag", flagDefinition.Name)
        Assert.Equal("message", messageSignature.Name)
        Assert.Equal(Some "message", messageDefinition.Name)
    | other ->
        failwithf "Unexpected declarations: %A" other
