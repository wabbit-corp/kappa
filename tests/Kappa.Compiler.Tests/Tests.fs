// Covers broad compiler smoke tests and repository-level regressions.
module SmokeTestSupport

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

let private diagnosticsSummary diagnostics =
    diagnostics
    |> List.map (fun diagnostic -> $"{DiagnosticCode.toIdentifier diagnostic.Code}: {diagnostic.Message}")
    |> String.concat Environment.NewLine

let private tryFindPayloadText fieldName (diagnostic: Diagnostic) =
    diagnostic.Payload.Fields
    |> List.tryPick (fun field ->
        if field.Name <> fieldName then
            None
        else
            match field.Value with
            | DiagnosticPayloadText value -> Some value
            | DiagnosticPayloadTextList _ -> None)

let rec private tryFindLocalScopedEffect expression =
    match expression with
    | LocalScopedEffect(declaration, _) ->
        Some declaration
    | LocalLet(_, value, body) ->
        tryFindLocalScopedEffect value |> Option.orElseWith (fun () -> tryFindLocalScopedEffect body)
    | LocalSignature(_, body)
    | LocalTypeAlias(_, body)
    | Lambda(_, body)
    | Unary(_, body)
    | SyntaxQuote body
    | SyntaxSplice body
    | TopLevelSyntaxSplice body
    | CodeQuote body
    | CodeSplice body
    | MonadicSplice body
    | ExplicitImplicitArgument body
    | InoutArgument body ->
        tryFindLocalScopedEffect body
    | TypeSyntaxTokens _ ->
        None
    | IfThenElse(condition, whenTrue, whenFalse) ->
        tryFindLocalScopedEffect condition
        |> Option.orElseWith (fun () -> tryFindLocalScopedEffect whenTrue)
        |> Option.orElseWith (fun () -> tryFindLocalScopedEffect whenFalse)
    | Elvis(left, right) ->
        tryFindLocalScopedEffect left |> Option.orElseWith (fun () -> tryFindLocalScopedEffect right)
    | Handle(_, label, body, returnClause, operationClauses) ->
        tryFindLocalScopedEffect label
        |> Option.orElseWith (fun () -> tryFindLocalScopedEffect body)
        |> Option.orElseWith (fun () -> tryFindLocalScopedEffect returnClause.Body)
        |> Option.orElseWith (fun () ->
            operationClauses
            |> List.tryPick (fun clause -> tryFindLocalScopedEffect clause.Body))
    | Match(scrutinee, cases) ->
        tryFindLocalScopedEffect scrutinee
        |> Option.orElseWith (fun () ->
            cases
            |> List.tryPick (fun caseClause ->
                caseClause.Guard
                |> Option.bind tryFindLocalScopedEffect
                |> Option.orElseWith (fun () -> tryFindLocalScopedEffect caseClause.Body)))
    | RecordLiteral fields
    | NamedApplicationBlock fields ->
        fields |> List.tryPick (fun field -> tryFindLocalScopedEffect field.Value)
    | Seal(value, _)
    | TagTest(value, _) ->
        tryFindLocalScopedEffect value
    | RecordUpdate(receiver, fields) ->
        tryFindLocalScopedEffect receiver
        |> Option.orElseWith (fun () -> fields |> List.tryPick (fun field -> tryFindLocalScopedEffect field.Value))
    | MemberAccess(receiver, _, arguments) ->
        tryFindLocalScopedEffect receiver
        |> Option.orElseWith (fun () -> arguments |> List.tryPick tryFindLocalScopedEffect)
    | SafeNavigation(receiver, navigation) ->
        tryFindLocalScopedEffect receiver
        |> Option.orElseWith (fun () -> navigation.Arguments |> List.tryPick tryFindLocalScopedEffect)
    | Do statements ->
        let rec inDo statement =
            match statement with
            | DoLet(_, value)
            | DoBind(_, value)
            | DoUsing(_, value)
            | DoVar(_, value)
            | DoAssign(_, value)
            | DoDefer value
            | DoExpression value
            | DoReturn value ->
                tryFindLocalScopedEffect value
            | DoLetQuestion(_, value, failure) ->
                tryFindLocalScopedEffect value
                |> Option.orElseWith (fun () ->
                    failure
                    |> Option.bind (fun block -> block.Body |> List.tryPick inDo))
            | DoIf(condition, whenTrue, whenFalse) ->
                tryFindLocalScopedEffect condition
                |> Option.orElseWith (fun () -> whenTrue |> List.tryPick inDo)
                |> Option.orElseWith (fun () -> whenFalse |> List.tryPick inDo)
            | DoWhile(condition, body) ->
                tryFindLocalScopedEffect condition
                |> Option.orElseWith (fun () -> body |> List.tryPick inDo)

        statements |> List.tryPick inDo
    | Apply(callee, arguments) ->
        tryFindLocalScopedEffect callee
        |> Option.orElseWith (fun () -> arguments |> List.tryPick tryFindLocalScopedEffect)
    | Binary(left, _, right) ->
        tryFindLocalScopedEffect left |> Option.orElseWith (fun () -> tryFindLocalScopedEffect right)
    | Comprehension comprehension ->
        tryFindLocalScopedEffect comprehension.Lowered
    | PrefixedString(_, parts) ->
        parts
        |> List.tryPick (function
            | StringText _ -> None
            | StringInterpolation(inner, _) -> tryFindLocalScopedEffect inner)
    | Literal _
    | NumericLiteral _
    | Name _
    | KindQualifiedName _ ->
        None

let private assertSurfaceIntegerLiteral (expectedValue: int) expectedText expression =
    match expression with
    | NumericLiteral(SurfaceIntegerLiteral(value, sourceText, None)) ->
        Assert.Equal(BigInteger(expectedValue), value)
        Assert.Equal(expectedText, sourceText)
    | other ->
        failwithf "Expected surface integer literal %s, got %A" expectedText other


module SmokeTestsShard0 =

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
    let ``frontend rejects module case fold collisions and identifies all colliding files`` () =
        let workspace =
            compileInMemoryWorkspace
                "memory-module-case-fold-collision-root"
                [
                    "Foo.kp", ""
                    "foo.debug.kp", ""
                ]

        let collisions =
            workspace.Diagnostics
            |> List.filter (fun diagnostic -> diagnostic.Code = DiagnosticCode.ModuleCaseFoldCollision)

        Assert.NotEmpty(collisions)

        let filePaths =
            collisions
            |> List.collect (fun diagnostic ->
                [
                    diagnostic.Location |> Option.map (fun location -> location.FilePath)
                    yield! diagnostic.RelatedLocations |> List.map (fun related -> related.Location.FilePath |> Some)
                ]
                |> List.choose id)
            |> Set.ofList

        Assert.Contains(
            filePaths,
            fun path -> path.EndsWith($"{Path.DirectorySeparatorChar}Foo.kp", StringComparison.Ordinal)
        )

        Assert.Contains(
            filePaths,
            fun path -> path.EndsWith($"{Path.DirectorySeparatorChar}foo.debug.kp", StringComparison.Ordinal)
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
    let ``source compilation imports private terms only via unhide in script mode`` () =
        let plainWorkspace =
            compileInMemoryWorkspaceWithPackageMode
                "memory-import-private-term-without-unhide-root"
                false
                [
                    "lib.kp",
                    [
                        "@PrivateByDefault module lib"
                        "secret : Int"
                        "let secret = 42"
                    ]
                    |> String.concat "\n"
                    "main.kp",
                    [
                        "module main"
                        "import lib.(term secret)"
                        "result : Int"
                        "let result = secret"
                    ]
                    |> String.concat "\n"
                ]

        Assert.True(plainWorkspace.HasErrors, "Expected ordinary import of a private term to be rejected.")
        Assert.Contains(plainWorkspace.Diagnostics, fun diagnostic -> diagnostic.Code = DiagnosticCode.ImportItemNotFound)

        let unhiddenWorkspace =
            compileInMemoryWorkspaceWithPackageMode
                "memory-import-private-term-with-unhide-root"
                false
                [
                    "lib.kp",
                    [
                        "@PrivateByDefault module lib"
                        "secret : Int"
                        "let secret = 42"
                    ]
                    |> String.concat "\n"
                    "main.kp",
                    [
                        "module main"
                        "import lib.(unhide term secret)"
                        "result : Int"
                        "let result = secret"
                    ]
                    |> String.concat "\n"
                ]

        Assert.False(unhiddenWorkspace.HasErrors, sprintf "Expected unhide import of a private term to succeed in script mode, got %A" unhiddenWorkspace.Diagnostics)


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
            [ DiagnosticCode.ExpectedSyntaxToken; DiagnosticCode.ExpectedSyntaxToken ],
            parsed.Diagnostics |> List.map (fun diagnostic -> diagnostic.Code)
        )
        Assert.Contains(parsed.Diagnostics, fun diagnostic -> diagnostic.Message.Contains("type alias body"))
        Assert.Contains(parsed.Diagnostics, fun diagnostic -> diagnostic.Message.Contains("trait member"))


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
        let diagnostic = Assert.Single(parsed.Diagnostics)
        Assert.Equal(DiagnosticCode.ExpectedSyntaxToken, diagnostic.Code)
        Assert.Equal("parser-syntax", diagnostic.Payload.Kind)
        Assert.Equal(Some "invalid-url-module-specifier", tryFindPayloadText "reason" diagnostic)
        Assert.Equal(Some "invalid-sha256-digest", tryFindPayloadText "url-parse-error" diagnostic)
        Assert.Equal(Some "sha256:not-hex", tryFindPayloadText "pin-text" diagnostic)


    [<Fact>]
    let ``parser rejects malformed string literal module specifiers with structured payload`` () =
        let sourceText =
            [
                "module demo.hello"
                "import \"https://example.com/lib\\q\".*"
            ]
            |> String.concat "\n"

        let _, lexed, parsed =
            lexAndParse
                "demo/hello.kp"
                sourceText

        Assert.Empty(lexed.Diagnostics)

        let diagnostic = Assert.Single(parsed.Diagnostics)
        Assert.Equal(DiagnosticCode.ExpectedSyntaxToken, diagnostic.Code)
        Assert.Equal("parser-syntax", diagnostic.Payload.Kind)
        Assert.Equal(Some "invalid-string-literal", tryFindPayloadText "reason" diagnostic)
        Assert.Equal(Some "unknown-escape-sequence", tryFindPayloadText "string-literal-error" diagnostic)
        Assert.Equal(Some "\\q", tryFindPayloadText "escape-text" diagnostic)


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
    let ``instances are visible through the full module closure independent of qualified imports`` () =
        let scoreSource =
            [
                "module providers.score"
                "trait Score a ="
                "    score : a -> Int"
            ]
            |> String.concat "\n"

        let implSource =
            [
                "module providers.impl"
                "import providers.score.(trait Score)"
                "instance Score Int ="
                "    let score x = x"
            ]
            |> String.concat "\n"

        let apiSource =
            [
                "module providers.api"
                "import providers.score.(trait Score)"
                "import providers.impl"
                "useScore : Score a => a -> Int"
                "let useScore x = score x"
            ]
            |> String.concat "\n"

        let mainSource =
            [
                "module main"
                "import providers.score.(trait Score)"
                "import providers.api.(term useScore)"
                "result : Int"
                "let result = useScore 7"
            ]
            |> String.concat "\n"

        let workspace, result =
            evaluateInMemoryBinding
                "memory-instance-closure-root"
                "main.result"
                [
                    "providers/score.kp", scoreSource
                    "providers/impl.kp", implSource
                    "providers/api.kp", apiSource
                    "main.kp", mainSource
                ]

        Assert.False(workspace.HasErrors, sprintf "Expected module-closure instance visibility, got %A" workspace.Diagnostics)

        match result with
        | Result.Ok value ->
            Assert.Equal("7", RuntimeValue.format value)
        | Result.Error issue ->
            failwithf "Expected transitive qualified-only instance visibility to succeed, got %s" issue.Message


    [<Fact>]
    let ``source compilation rejects unterminated type alias bodies`` () =
        let unterminatedBracketWorkspace =
            compileInMemoryWorkspace
                "memory-compile-unterminated-type-alias-bracket-root"
                [
                    "main.kp",
                    [
                        "module main"
                        "type I0 ="
                        "    ["
                    ]
                    |> String.concat "\n"
                ]

        let malformedConstructorWorkspace =
            compileInMemoryWorkspace
                "memory-compile-malformed-type-alias-constructor-root"
                [
                    "main.kp",
                    [
                        "module main"
                        "type I0 ="
                        "    I0 ["
                    ]
                    |> String.concat "\n"
                ]

        Assert.True(unterminatedBracketWorkspace.HasErrors, "Expected an unterminated type alias body to produce a parse diagnostic.")
        Assert.True(malformedConstructorWorkspace.HasErrors, "Expected a malformed type alias body to produce a parse diagnostic.")
        Assert.Contains(unterminatedBracketWorkspace.Diagnostics, fun diagnostic -> diagnostic.Code = DiagnosticCode.ExpectedSyntaxToken)
        Assert.Contains(malformedConstructorWorkspace.Diagnostics, fun diagnostic -> diagnostic.Code = DiagnosticCode.ExpectedSyntaxToken)


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
    let ``type unification matches higher kinded applications against applied named types`` () =
        let functionApplication =
            TypeSignatures.TypeApply(TypeSignatures.TypeVariable "f", [ TypeSignatures.TypeVariable "a" ])

        let concreteApplication =
            TypeSignatures.TypeName([ "Option" ], [ TypeSignatures.TypeName([ "Int" ], []) ])

        match TypeSignatures.tryUnifyMany [ functionApplication, concreteApplication ] with
        | Some substitution ->
            Assert.Equal(TypeSignatures.TypeName([ "Option" ], []), substitution["f"])
            Assert.Equal(TypeSignatures.TypeName([ "Int" ], []), substitution["a"])
        | None ->
            failwith "Expected higher-kinded application unification to succeed."


    [<Fact>]
    let ``supported backend profiles satisfy the current prelude intrinsic surface`` () =
        let mainSource =
            [
                "module main"
                "flag : Bool"
                "let flag = not False"
            ]
            |> String.concat "\n"

        for backendProfile in [ "interpreter"; "dotnet"; "zig"; "zigcc" ] do
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

            Assert.DoesNotContain("not", preludeModule.IntrinsicTerms)
            Assert.Contains("print", preludeModule.IntrinsicTerms)
            Assert.Contains("println", preludeModule.IntrinsicTerms)
            Assert.DoesNotContain("printInt", preludeModule.IntrinsicTerms)


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
    let ``parser preserves ordinary named constructor parameters`` () =
        let sourceText =
            [
                "module main"
                "data User : Type ="
                "    User (name : String) (age : Int)"
            ]
            |> String.concat "\n"

        let parsed = parseDocumentWithFixities FixityTable.empty "__constructor_parameters__.kp" sourceText

        Assert.Empty(parsed.Diagnostics)

        match parsed.Syntax.Declarations with
        | [ DataDeclarationNode declaration ] ->
            match declaration.Constructors with
            | [ constructor ] ->
                match constructor.Parameters with
                | Some parameters ->
                    Assert.Equal<string option>([ Some "name"; Some "age" ], parameters |> List.map _.ParameterName)
                    Assert.Equal<string>([ "String"; "Int" ], parameters |> List.map (fun parameter -> tokensText parameter.ParameterTypeTokens))
                | None ->
                    failwith "Expected ordinary constructor parameters to be preserved."
            | other ->
                failwithf "Expected one constructor, got %A" other
        | other ->
            failwithf "Unexpected declarations when parsing data constructor parameters: %A" other


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

        Assert.Equal(BackendProfile.Zig, workspace.Backend)
        Assert.Equal("zig", workspace.BackendProfile)
        Assert.Equal("bootstrap-prelude-v2", workspace.BackendIntrinsicIdentity)


    [<Fact>]
    let ``map collection sugar lowers through qualified Res constructor semantics`` () =
        let rec tryFindQualifiedRes expression =
            match expression with
            | Apply(MemberAccess(Name [ "Res" ], [ ":&" ], []), [ keyExpression; valueExpression ]) ->
                Some(keyExpression, valueExpression)
            | Apply(callee, arguments) ->
                tryFindQualifiedRes callee
                |> Option.orElseWith (fun () -> arguments |> List.tryPick tryFindQualifiedRes)
            | LocalLet(_, value, body) ->
                tryFindQualifiedRes value
                |> Option.orElseWith (fun () -> tryFindQualifiedRes body)
            | Lambda(_, body)
            | SyntaxQuote body
            | SyntaxSplice body
            | TopLevelSyntaxSplice body
            | CodeQuote body
            | CodeSplice body
            | MonadicSplice body
            | ExplicitImplicitArgument body
            | InoutArgument body
            | Unary(_, body)
            | Seal(body, _) ->
                tryFindQualifiedRes body
            | IfThenElse(condition, thenBranch, elseBranch) ->
                tryFindQualifiedRes condition
                |> Option.orElseWith (fun () -> tryFindQualifiedRes thenBranch)
                |> Option.orElseWith (fun () -> tryFindQualifiedRes elseBranch)
            | Match(subject, cases) ->
                tryFindQualifiedRes subject
                |> Option.orElseWith (fun () -> cases |> List.tryPick (fun case -> tryFindQualifiedRes case.Body))
            | RecordLiteral fields ->
                fields |> List.tryPick (fun field -> tryFindQualifiedRes field.Value)
            | RecordUpdate(receiver, fields) ->
                tryFindQualifiedRes receiver
                |> Option.orElseWith (fun () -> fields |> List.tryPick (fun field -> tryFindQualifiedRes field.Value))
            | MemberAccess(receiver, _, arguments) ->
                tryFindQualifiedRes receiver
                |> Option.orElseWith (fun () -> arguments |> List.tryPick tryFindQualifiedRes)
            | SafeNavigation(receiver, navigation) ->
                tryFindQualifiedRes receiver
                |> Option.orElseWith (fun () -> navigation.Arguments |> List.tryPick tryFindQualifiedRes)
            | Binary(left, _, right)
            | Elvis(left, right) ->
                tryFindQualifiedRes left
                |> Option.orElseWith (fun () -> tryFindQualifiedRes right)
            | Comprehension comprehension ->
                tryFindQualifiedRes comprehension.Lowered
            | _ ->
                None

        let parsed =
            parseDocumentWithFixities
                FixityTable.empty
                "__map_literal__.kp"
                ([ "module main"; "let result = { yield 1 : 2 }" ] |> String.concat "\n")

        Assert.Empty(parsed.Diagnostics)
        Assert.Equal<string list option>(Some [ "main" ], parsed.Syntax.ModuleHeader)

        match parsed.Syntax.Declarations with
        | [ LetDeclaration resultDeclaration ] ->
            match resultDeclaration.Body with
            | Some body ->
                match tryFindQualifiedRes body with
                | Some(keyExpression, valueExpression) ->
                    assertSurfaceIntegerLiteral 1 "1" keyExpression
                    assertSurfaceIntegerLiteral 2 "2" valueExpression
                | None ->
                    failwithf "Expected map collection lowering to use Res.(:&), got %A" body
            | None ->
                failwith "Expected let result to have a body."
        | other ->
            failwithf "Unexpected declarations: %A" other


module SmokeTestsShard1 =

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
    let ``source compilation rejects unhide imports in package mode`` () =
        let workspace =
            compileInMemoryWorkspace
                "memory-import-unhide-package-mode-root"
                [
                    "lib.kp",
                    [
                        "@PrivateByDefault module lib"
                        "secret : Int"
                        "let secret = 42"
                    ]
                    |> String.concat "\n"
                    "main.kp",
                    [
                        "module main"
                        "import lib.(unhide term secret)"
                        "result : Int"
                        "let result = secret"
                    ]
                    |> String.concat "\n"
                ]

        Assert.True(workspace.HasErrors, "Expected package mode to reject unhide imports.")
        Assert.Contains(
            workspace.Diagnostics,
            fun diagnostic ->
                diagnostic.Code = DiagnosticCode.ImportUnhideRequiresBuildSetting
                && diagnostic.Message.Contains("allow_unhiding", StringComparison.Ordinal)
        )


    [<Fact>]
    let ``parser preserves trait supertraits operator members and typed defaults`` () =
        let sourceText =
            [
                "module main"
                ""
                "trait (Eq a, Show a) => Ord a ="
                "    (==) : a -> a -> Bool"
                "    let debug : a -> String = \\x -> show x"
            ]
            |> String.concat "\n"

        let _, lexed, parsed =
            lexAndParse
                "memory.kp"
                sourceText

        Assert.Empty(lexed.Diagnostics)
        Assert.Empty(parsed.Diagnostics)

        let declaration =
            parsed.Syntax.Declarations
            |> List.pick (function
                | TraitDeclarationNode declaration -> Some declaration
                | _ -> None)

        Assert.Equal("Ord", declaration.Name)
        Assert.Equal("( Eq a , Show a ) = > Ord a", tokensText declaration.FullHeaderTokens)
        Assert.Equal("a", tokensText declaration.HeaderTokens)

        match declaration.Members with
        | [ operatorMember; defaultMember ] ->
            Assert.Equal(Some "==", operatorMember.Name)
            Assert.Equal(Some "debug", defaultMember.Name)
            Assert.True(defaultMember.DefaultDefinition.IsSome)
            Assert.Equal(Some "debug", defaultMember.DefaultDefinition.Value.Name)
        | other ->
            failwithf "Expected two trait members, got %A" other


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
    let ``multiple surviving global instances are rejected instead of picking the first match`` () =
        let scoreSource =
            [
                "module providers.score"
                "trait Score a ="
                "    score : a -> Int"
            ]
            |> String.concat "\n"

        let leftSource =
            [
                "module providers.left"
                "import providers.score.(trait Score)"
                "instance Score Int ="
                "    let score x = x"
            ]
            |> String.concat "\n"

        let rightSource =
            [
                "module providers.right"
                "import providers.score.(trait Score)"
                "instance Score Int ="
                "    let score x = x + 1"
            ]
            |> String.concat "\n"

        let mainSource =
            [
                "module main"
                "import providers.score.(trait Score)"
                "import providers.left.*"
                "import providers.right.*"
                "useScore : Score a => a -> Int"
                "let useScore x = score x"
                "result : Int"
                "let result = useScore 41"
            ]
            |> String.concat "\n"

        let workspace =
            compileInMemoryWorkspace
                "memory-instance-ambiguity-root"
                [
                    "providers/score.kp", scoreSource
                    "providers/left.kp", leftSource
                    "providers/right.kp", rightSource
                    "main.kp", mainSource
                ]

        Assert.True(workspace.HasErrors, "Expected overlapping global instances to be rejected.")

        let ambiguityDiagnostic =
            workspace.Diagnostics
            |> List.tryFind (fun diagnostic -> diagnostic.Code = DiagnosticCode.TraitInstanceAmbiguous)

        Assert.True(ambiguityDiagnostic.IsSome, sprintf "Expected an instance ambiguity diagnostic, got %A" workspace.Diagnostics)


    [<Fact>]
    let ``parser gives built in safe navigation and elvis their spec precedence`` () =
        let source = createSource "__safe_navigation_precedence__.kp" "a?.b ?: fallback"
        let lexed = Lexer.tokenize source
        let parsed = CoreParsing.parseExpression (Parser.bootstrapFixities ()) source (DiagnosticBag()) lexed.Tokens

        match parsed with
        | Some(Elvis(SafeNavigation(Name [ "a" ], { Segments = [ "b" ]; Arguments = [] }), Name [ "fallback" ])) -> ()
        | other -> failwithf "Unexpected expression shape: %A" other


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
    let ``type signature schemes accept both grouped and chained constraint prefixes`` () =
        let grouped =
            parseSchemeText "(Eq a, Hashable a) => Hashable (Option a)"

        let chained =
            parseSchemeText "Eq a => Hashable a => Hashable (Option a)"

        let expectedConstraints : TypeSignatures.TraitConstraint list =
            [
                { Trait = TypeSignatures.TraitReference.unqualified "Eq"
                  Arguments = [ TypeSignatures.TypeVariable "a" ] }
                { Trait = TypeSignatures.TraitReference.unqualified "Hashable"
                  Arguments = [ TypeSignatures.TypeVariable "a" ] }
            ]

        let expectedBody =
            TypeSignatures.TypeName(
                [ "Hashable" ],
                [ TypeSignatures.TypeName([ "Option" ], [ TypeSignatures.TypeVariable "a" ]) ]
            )

        Assert.Equal<TypeSignatures.TraitConstraint list>(expectedConstraints, grouped.Constraints)
        Assert.Equal<TypeSignatures.TraitConstraint list>(expectedConstraints, chained.Constraints)
        Assert.Equal(expectedBody, grouped.Body)
        Assert.Equal(expectedBody, chained.Body)


    [<Fact>]
    let ``compilation still resolves Float through the visible prelude alias`` () =
        let workspace =
            compileInMemoryWorkspace
                "memory-float-prelude-alias-root"
                [
                    "main.kp",
                    [
                        "module main"
                        "widen : Float -> Double"
                        "let widen x = x"
                        "narrow : Double -> Float"
                        "let narrow x = x"
                    ]
                    |> String.concat "\n"
                ]

        Assert.False(workspace.HasErrors, $"Expected std.prelude.Float alias elaboration to succeed, got {workspace.Diagnostics}.")


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
    let ``data declarations accept inline pipe separated constructor alternatives`` () =
        let sourceText =
            [
                "module main"
                "data Tree a : Type = Leaf | Branch (Tree a) a (Tree a)"
            ]
            |> String.concat "\n"

        let parsed = parseDocumentWithFixities FixityTable.empty "__inline_pipe_constructors__.kp" sourceText

        Assert.Empty(parsed.Diagnostics)

        match parsed.Syntax.Declarations with
        | [ DataDeclarationNode declaration ] ->
            Assert.Equal("Tree", declaration.Name)

            match declaration.Constructors with
            | [ leaf; branch ] ->
                Assert.Equal("Leaf", leaf.Name)
                Assert.Equal(0, leaf.Arity)
                Assert.Equal("Branch", branch.Name)
                Assert.Equal(3, branch.Arity)
            | other ->
                failwithf "Expected two inline constructor alternatives, got %A" other
        | other ->
            failwithf "Unexpected declarations when parsing inline constructor alternatives: %A" other


    [<Fact>]
    let ``bundled bootstrap prelude exposes the current bootstrap surface and IO shape`` () =
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
                [ "IsSubsingleton"
                  "IsProp"
                  "IsSet"
                  "IsTrait"
                  "Equiv"
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
                  "BorrowIntoQuery"
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
                  "/="
                  "<"
                  "<="
                  ">"
                  ">="
                  "|>"
                  "<|"
                  "not"
                  "and"
                  "or"
                  ".."
                  "..<"
                  "force"
                  "++"
                  "for_"
                  "sequence"
                  "sequence_"
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
                  "printlnString"
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

        let signatureTypes =
            preludeDocument.Syntax.Declarations
            |> List.choose (function
                | SignatureDeclaration declaration -> Some(declaration.Name, tokensText declaration.TypeTokens)
                | _ -> None)
            |> Map.ofList

        let letNames =
            preludeDocument.Syntax.Declarations
            |> List.choose (function
                | LetDeclaration declaration -> declaration.Name
                | _ -> None)
            |> Set.ofList

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
        Assert.Equal("allEqual : ( x : t ) -> ( y : t ) -> x = y", traitMemberTypes["IsSubsingleton"]["allEqual"])
        Assert.Empty(traitMemberTypes["IsProp"])
        Assert.Equal(
            "pathIsProp : forall ( x : a ) ( y : a ) . IsProp ( x = y )",
            traitMemberTypes["IsSet"]["pathIsProp"]
        )
        Assert.Equal("buildInterpolated : List SyntaxFragment -> Elab ( Syntax t )", traitMemberTypes["InterpolatedMacro"]["buildInterpolated"])
        Assert.Equal("Elab ( InterpolatedMacro String )", expectTermTypes["f"])
        Assert.Equal("Elab ( InterpolatedMacro String )", expectTermTypes["re"])
        Assert.Equal("Elab ( InterpolatedMacro String )", expectTermTypes["b"])
        Assert.Equal("Elab ( InterpolatedMacro Type )", expectTermTypes["type"])
        Assert.Equal("( ~= ) : ( & x : a ) -> ( & y : a ) -> Bool", traitMemberTypes["Equiv"]["~="])
        Assert.Equal("equivRefl : ( & x : a ) -> ( ( x ~= x ) = True )", traitMemberTypes["Equiv"]["equivRefl"])
        Assert.Equal(
            "equivSym : ( & x : a ) -> ( & y : a ) -> ( ( x ~= y ) = True -> ( y ~= x ) = True )",
            traitMemberTypes["Equiv"]["equivSym"]
        )
        Assert.Equal(
            "equivTrans : ( & x : a ) -> ( & y : a ) -> ( & z : a ) -> ( ( x ~= y ) = True -> ( y ~= z ) = True -> ( x ~= z ) = True )",
            traitMemberTypes["Equiv"]["equivTrans"]
        )
        Assert.Equal("( == ) : ( & x : a ) -> ( & y : a ) -> Bool", traitMemberTypes["Eq"]["=="])
        Assert.Equal("eqSound : ( & x : a ) -> ( & y : a ) -> ( ( x == y ) = True -> x = y )", traitMemberTypes["Eq"]["eqSound"])
        Assert.Equal(
            "eqComplete : ( & x : a ) -> ( & y : a ) -> ( x = y -> ( x == y ) = True )",
            traitMemberTypes["Eq"]["eqComplete"]
        )
        Assert.Equal("eqIsSet : IsSet a", traitMemberTypes["Eq"]["eqIsSet"])
        Assert.Equal("compare : ( & x : a ) -> ( & y : a ) -> Ordering", traitMemberTypes["Ord"]["compare"])
        Assert.Equal(
            "map : forall ( a : Type ) ( b : Type ) . ( a -> b ) -> f a -> f b",
            traitMemberTypes["Functor"]["map"]
        )
        Assert.Equal("pure : forall ( a : Type ) . a -> f a", traitMemberTypes["Applicative"]["pure"])
        Assert.Equal(
            "liftA2 : forall ( a : Type ) ( b : Type ) ( c : Type ) . ( a -> b -> c ) -> f a -> f b -> f c",
            traitMemberTypes["Applicative"]["liftA2"]
        )
        Assert.Equal(
            "let ( <*> ) : forall ( a : Type ) ( b : Type ) . f ( a -> b ) -> f a -> f b = \\ ff -> \\ fa -> liftA2 ( \\ f -> \\ x -> f x ) ff fa",
            traitMemberTypes["Applicative"]["<*>"]
        )
        Assert.Equal(
            "( >>= ) : forall ( a : Type ) ( b : Type ) . m a -> ( a -> m b ) -> m b",
            traitMemberTypes["Monad"][">>="]
        )
        Assert.Equal(
            "let ( >> ) : forall ( a : Type ) ( b : Type ) . m a -> m b -> m b = \\ ma -> \\ mb -> ma >>= \\ ignored -> mb",
            traitMemberTypes["Monad"][">>"]
        )
        Assert.Equal("empty : f a", traitMemberTypes["Alternative"]["empty"])
        Assert.Equal("( <|> ) : f a -> f a -> f a", traitMemberTypes["Alternative"]["<|>"])
        Assert.Equal("let orElse x y = x <|> y", traitMemberTypes["Alternative"]["orElse"])
        Assert.Equal("foldr : forall ( a : Type ) ( b : Type ) . ( a -> b -> b ) -> b -> f a -> b", traitMemberTypes["Foldable"]["foldr"])
        Assert.Equal("foldl : forall ( a : Type ) ( b : Type ) . ( b -> a -> b ) -> b -> f a -> b", traitMemberTypes["Foldable"]["foldl"])
        Assert.Equal(
            "foldMap : forall ( a : Type ) ( m : Type ) . ( @ _ : Monoid m ) -> ( a -> m ) -> f a -> m",
            traitMemberTypes["Foldable"]["foldMap"]
        )
        Assert.Equal(
            "traverse : forall ( g : Type -> Type ) ( a : Type ) ( b : Type ) . ( @ _ : Applicative g ) -> ( a -> g b ) -> f a -> g ( f b )",
            traitMemberTypes["Traversable"]["traverse"]
        )
        Assert.Equal("filter : forall ( a : Type ) . ( a -> Bool ) -> f a -> f a", traitMemberTypes["Filterable"]["filter"])
        Assert.Equal(
            "filterMap : forall ( a : Type ) ( b : Type ) . ( a -> Option b ) -> f a -> f b",
            traitMemberTypes["FilterMap"]["filterMap"]
        )
        Assert.Equal("empty : a", traitMemberTypes["Monoid"]["empty"])
        Assert.Equal("append : a -> a -> a", traitMemberTypes["Monoid"]["append"])
        Assert.Equal("Range : Type", traitMemberTypes["Rangeable"]["Range"])
        Assert.Equal("range : ( from : v ) -> ( to : v ) -> ( exclusive : Bool ) -> Range", traitMemberTypes["Rangeable"]["range"])
        Assert.Equal("Item : Type", traitMemberTypes["Iterator"]["Item"])
        Assert.Equal("next : ( 1 this : it ) -> Option ( item : Item , rest : it )", traitMemberTypes["Iterator"]["next"])
        Assert.Equal("buildInterpolated : List SyntaxFragment -> Elab ( Syntax t )", traitMemberTypes["InterpolatedMacro"]["buildInterpolated"])
        Assert.Equal("liftCode : a -> Code a", traitMemberTypes["Lift"]["liftCode"])
        Assert.Equal("Mode : QueryMode", traitMemberTypes["IntoQuery"]["Mode"])
        Assert.Equal("ItemQuantity : Quantity", traitMemberTypes["IntoQuery"]["ItemQuantity"])
        Assert.Equal("Item : Type", traitMemberTypes["IntoQuery"]["Item"])
        Assert.Equal("SourceDemand : Quantity", traitMemberTypes["IntoQuery"]["SourceDemand"])
        Assert.Equal("toQuery : src -> QueryCore Mode ItemQuantity Item", traitMemberTypes["IntoQuery"]["toQuery"])
        Assert.Equal("Card : QueryCard", traitMemberTypes["BorrowIntoQuery"]["Card"])
        Assert.Equal("Item : Type", traitMemberTypes["BorrowIntoQuery"]["Item"])
        Assert.Equal(
            "toBorrowQuery : forall ( ρ : Region ) . ( & [ ρ ] source : src ) -> QueryCore ( QueryMode Reusable Card ) ω ( BorrowView ρ Item ) captures ( ρ )",
            traitMemberTypes["BorrowIntoQuery"]["toBorrowQuery"]
        )
        Assert.Equal("Item : Type", traitMemberTypes["FromComprehensionPlan"]["Item"])
        Assert.Equal(
            "fromComprehensionPlan : ComprehensionPlan Item -> Elab ( Syntax c )",
            traitMemberTypes["FromComprehensionPlan"]["fromComprehensionPlan"]
        )
        Assert.Equal("Item : Type", traitMemberTypes["FromComprehensionRaw"]["Item"])
        Assert.Equal(
            "fromComprehensionRaw : RawComprehension Item -> Elab ( Syntax c )",
            traitMemberTypes["FromComprehensionRaw"]["fromComprehensionRaw"]
        )
        Assert.Equal("rel : a -> a -> Type", traitMemberTypes["WellFoundedRelation"]["rel"])
        Assert.Equal("wf : WellFounded a rel", traitMemberTypes["WellFoundedRelation"]["wf"])
        Assert.Equal("fromInteger : Nat -> a", traitMemberTypes["FromInteger"]["fromInteger"])
        Assert.Equal("fromFloat : Double -> a", traitMemberTypes["FromFloat"]["fromFloat"])
        Assert.Equal("fromString : String -> a", traitMemberTypes["FromString"]["fromString"])
        Assert.Equal("finally : forall ( a : Type ) . m a -> m Unit -> m a", traitMemberTypes["MonadFinally"]["finally"])
        Assert.Equal("Error : Type", traitMemberTypes["MonadError"]["Error"])
        Assert.Equal("throwError : Error -> m a", traitMemberTypes["MonadError"]["throwError"])
        Assert.Equal("catchError : m a -> ( Error -> m a ) -> m a", traitMemberTypes["MonadError"]["catchError"])
        Assert.Equal(
            "bracket : forall ( a : Type ) ( b : Type ) . ( 1 acquire : m a ) -> ( 1 release : ( 1 r : a ) -> m Unit ) -> ( 1 use : ( & r : a ) -> m b ) -> m b",
            traitMemberTypes["MonadResource"]["bracket"]
        )
        Assert.Equal("Ref : Type -> Type", traitMemberTypes["MonadRef"]["Ref"])
        Assert.Equal("release : ( 1 resource : a ) -> m Unit", traitMemberTypes["Releasable"]["release"])
        Assert.Equal("forall ( @ 0 t : Type ) . Code t -> Option ( ClosedCode t )", expectTermTypes["closeCode"])
        Assert.Equal("forall ( @ 0 t : Type ) . Code t -> Code t", expectTermTypes["genlet"])
        Assert.Equal("ClosedCode t -> UIO t", expectTermTypes["runCode"])
        Assert.Equal("( 0 t : Type ) -> ( @ _ : IsTrait t ) -> ( @ evidence : t ) -> t", signatureTypes["summon"])
        Assert.Equal("( 1 value : a ) -> UIO a", expectTermTypes["pure"])
        Assert.Equal("UIO a -> ( a -> UIO b ) -> UIO b", expectTermTypes[">>="])
        Assert.Equal("forall ( v : Type ) . ( @ rangeable : Rangeable v ) -> v -> v -> rangeable . Range", signatureTypes[".."])
        Assert.Equal("forall ( v : Type ) . ( @ rangeable : Rangeable v ) -> v -> v -> rangeable . Range", signatureTypes["..<"])
        Assert.Equal("String -> UIO Unit", expectTermTypes["println"])
        Assert.Equal("String -> UIO Unit", expectTermTypes["print"])
        Assert.Equal("a -> UIO ( Ref a )", expectTermTypes["newRef"])
        Assert.Equal("Ref a -> UIO a", expectTermTypes["readRef"])
        Assert.Equal("Ref a -> a -> UIO Unit", expectTermTypes["writeRef"])
        Assert.Equal("String -> UIO Unit", signatureTypes["printlnString"])
        Assert.Equal("Int -> UIO Unit", signatureTypes["printInt"])
        Assert.Equal("String -> UIO Unit", signatureTypes["printString"])
        Assert.Equal("forall ( a : Type ) . ( @ eq : Eq a ) -> ( & x : a ) -> ( & y : a ) -> Bool", signatureTypes["/="])
        Assert.Equal("forall ( a : Type ) . ( @ ord : Ord a ) -> ( & x : a ) -> ( & y : a ) -> Bool", signatureTypes["<"])
        Assert.Equal("forall ( a : Type ) . ( @ ord : Ord a ) -> ( & x : a ) -> ( & y : a ) -> Bool", signatureTypes["<="])
        Assert.Equal("forall ( a : Type ) . ( @ ord : Ord a ) -> ( & x : a ) -> ( & y : a ) -> Bool", signatureTypes[">"])
        Assert.Equal("forall ( a : Type ) . ( @ ord : Ord a ) -> ( & x : a ) -> ( & y : a ) -> Bool", signatureTypes[">="])
        Assert.Equal("forall ( a : Type ) . ( @ monoid : Monoid a ) -> a -> a -> a", signatureTypes["++"])
        Assert.Equal(
            "forall ( t : Type -> Type ) ( m : Type -> Type ) ( a : Type ) . ( @ foldable : Foldable t ) -> ( @ applicative : Applicative m ) -> t a -> ( a -> m Unit ) -> m Unit",
            signatureTypes["for_"]
        )
        Assert.Equal(
            "forall ( t : Type -> Type ) ( m : Type -> Type ) ( a : Type ) . ( @ traversable : Traversable t ) -> ( @ applicative : Applicative m ) -> t ( m a ) -> m ( t a )",
            signatureTypes["sequence"]
        )
        Assert.Equal(
            "forall ( t : Type -> Type ) ( m : Type -> Type ) ( a : Type ) . ( @ foldable : Foldable t ) -> ( @ applicative : Applicative m ) -> t ( m a ) -> m Unit",
            signatureTypes["sequence_"]
        )
        Assert.Contains("printlnString", letNames)
        Assert.Contains("printInt", letNames)
        Assert.Contains("printString", letNames)
        Assert.Equal("UIO a -> UIO b -> UIO b", expectTermTypes[">>"])
        Assert.Equal("Void -> a", expectTermTypes["absurd"])
        Assert.Equal("( ( = ) a x y ) -> b -> b", expectTermTypes["subst"])
        Assert.Equal("a -> ( a -> b ) -> b", expectTermTypes["|>"])
        Assert.Equal("( a -> b ) -> a -> b", expectTermTypes["<|"])
        Assert.Equal("( ( = ) a x y ) -> ( ( = ) a y x )", expectTermTypes["sym"])
        Assert.Equal(
            "( ( = ) a x y ) -> ( ( = ) a y z ) -> ( ( = ) a x z )",
            expectTermTypes["trans"]
        )
        Assert.Equal("( ( = ) a x y ) -> ( ( = ) b u v )", expectTermTypes["cong"])
        Assert.Contains("..", letNames)
        Assert.Contains("..<", letNames)
        Assert.Contains("/=", letNames)
        Assert.Contains("<", letNames)
        Assert.Contains("<=", letNames)
        Assert.Contains(">", letNames)
        Assert.Contains(">=", letNames)
        Assert.Contains("++", letNames)
        Assert.Contains("for_", letNames)
        Assert.Contains("sequence", letNames)
        Assert.Contains("sequence_", letNames)


    [<Fact>]
    let ``bundled prelude Match constructors follow the spec payload residue order`` () =
        let workspace =
            compileInMemoryWorkspace
                "memory-prelude-match-shape-root"
                [
                    "main.kp",
                    [
                        "@PrivateByDefault module main"
                        ""
                        "data Buf : Type ="
                        "    Buf (n : Int)"
                        ""
                        "hit : Match Int Buf"
                        "let hit = Match.Hit 1"
                        ""
                        "miss : Match Int Buf"
                        "let miss = Match.Miss (Buf 0)"
                    ]
                    |> String.concat "\n"
                ]

        Assert.False(workspace.HasErrors, sprintf "Expected Match.Hit / Match.Miss to match the spec shape, got %A" workspace.Diagnostics)


module SmokeTestsShard2 =

    [<Fact>]
    let ``import diagnostics expose long form explanations`` () =
        let explainedCodes =
            [
                DiagnosticCode.ImportCycle
                DiagnosticCode.ImportAmbiguous
                DiagnosticCode.ImportItemNotFound
                DiagnosticCode.ModuleNameUnresolved
                DiagnosticCode.ModuleCaseFoldCollision
                DiagnosticCode.NameAmbiguous
            ]

        for code in explainedCodes do
            let explanation = DiagnosticCode.tryGetExplanation code
            Assert.True(explanation.IsSome, $"Expected {DiagnosticCode.toIdentifier code} to have a long-form explanation.")


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
    let ``parser reports a dedicated diagnostic for misplaced module headers`` () =
        let sourceText =
            [
                "module demo.one"
                "let x = 1"
                "module demo.two"
                "let y = 2"
            ]
            |> String.concat "\n"

        let _, lexed, parsed =
            lexAndParse
                "demo/one.kp"
                sourceText

        Assert.Empty(lexed.Diagnostics)
        Assert.Equal<DiagnosticCode list>([ DiagnosticCode.ModuleHeaderMisplaced ], parsed.Diagnostics |> List.map (fun diagnostic -> diagnostic.Code))


    [<Fact>]
    let ``source compilation keeps imported opaque type aliases abstract unless clarified`` () =
        let abstractWorkspace =
            compileInMemoryWorkspaceWithPackageMode
                "memory-import-opaque-alias-without-clarify-root"
                false
                [
                    "lib.kp",
                    [
                        "module lib"
                        "opaque type Box = (value : Int)"
                    ]
                    |> String.concat "\n"
                    "main.kp",
                    [
                        "module main"
                        "import lib.(type Box)"
                        "copy : Box"
                        "let copy : Box = (value = 1)"
                    ]
                    |> String.concat "\n"
                ]

        Assert.True(abstractWorkspace.HasErrors, "Expected imported opaque type aliases to remain abstract without clarify.")

        let clarifiedWorkspace =
            compileInMemoryWorkspaceWithPackageMode
                "memory-import-opaque-alias-with-clarify-root"
                false
                [
                    "lib.kp",
                    [
                        "module lib"
                        "opaque type Box = (value : Int)"
                    ]
                    |> String.concat "\n"
                    "main.kp",
                    [
                        "module main"
                        "import lib.(clarify type Box)"
                        "copy : Box"
                        "let copy : Box = (value = 1)"
                    ]
                    |> String.concat "\n"
                ]

        Assert.False(clarifiedWorkspace.HasErrors, sprintf "Expected clarify import of an opaque type alias to expose its equation locally, got %A" clarifiedWorkspace.Diagnostics)


    [<Fact>]
    let ``parser accepts multiline local let in trait default member body`` () =
        let sourceText =
            [
                "module main"
                ""
                "trait Eq a => Ord a ="
                "    compare : a -> a -> Ordering"
                "    let (<=) x y ="
                "        let ordering = compare x y"
                "        in (ordering == LT) || (ordering == EQ)"
                "    let (>) x y = compare x y == GT"
            ]
            |> String.concat "\n"

        let _, lexed, parsed =
            lexAndParse
                "memory.kp"
                sourceText

        Assert.Empty(lexed.Diagnostics)
        Assert.Empty(parsed.Diagnostics)

        let declaration =
            parsed.Syntax.Declarations
            |> List.pick (function
                | TraitDeclarationNode declaration -> Some declaration
                | _ -> None)

        Assert.Equal("Ord", declaration.Name)

        match declaration.Members with
        | _ :: defaultLeq :: defaultGt :: [] ->
            Assert.Equal(Some "<=", defaultLeq.Name)
            Assert.True(defaultLeq.DefaultDefinition.IsSome)
            Assert.Equal(Some ">", defaultGt.Name)
            Assert.True(defaultGt.DefaultDefinition.IsSome)
        | other ->
            failwithf "Expected three trait members, got %A" other


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
    let ``type signature schemes accept infix proposition forms used by Equiv and Eq members`` () =
        let equivRefl =
            parseSchemeText "(& x : a) -> ((x ~= x) = True)"

        let eqSound =
            parseSchemeText "(& x : a) -> (& y : a) -> ((x == y) = True -> x = y)"

        let eqComplete =
            parseSchemeText "(& x : a) -> (& y : a) -> (x = y -> (x == y) = True)"

        Assert.Equal(
            TypeSignatures.TypeArrow(
                QuantityBorrow None,
                TypeSignatures.TypeVariable "a",
                TypeSignatures.TypeEquality(
                    TypeSignatures.TypeApply(
                        TypeSignatures.TypeName([ "~=" ], []),
                        [ TypeSignatures.TypeVariable "x"; TypeSignatures.TypeVariable "x" ]
                    ),
                    TypeSignatures.TypeName([ "True" ], [])
                )
            ),
            equivRefl.Body
        )

        Assert.Equal(
            TypeSignatures.TypeArrow(
                QuantityBorrow None,
                TypeSignatures.TypeVariable "a",
                TypeSignatures.TypeArrow(
                    QuantityBorrow None,
                    TypeSignatures.TypeVariable "a",
                    TypeSignatures.TypeArrow(
                        QuantityOmega,
                        TypeSignatures.TypeEquality(
                            TypeSignatures.TypeApply(
                                TypeSignatures.TypeName([ "==" ], []),
                                [ TypeSignatures.TypeVariable "x"; TypeSignatures.TypeVariable "y" ]
                            ),
                            TypeSignatures.TypeName([ "True" ], [])
                        ),
                        TypeSignatures.TypeEquality(TypeSignatures.TypeVariable "x", TypeSignatures.TypeVariable "y")
                    )
                )
            ),
            eqSound.Body
        )

        Assert.Equal(
            TypeSignatures.TypeArrow(
                QuantityBorrow None,
                TypeSignatures.TypeVariable "a",
                TypeSignatures.TypeArrow(
                    QuantityBorrow None,
                    TypeSignatures.TypeVariable "a",
                    TypeSignatures.TypeArrow(
                        QuantityOmega,
                        TypeSignatures.TypeEquality(TypeSignatures.TypeVariable "x", TypeSignatures.TypeVariable "y"),
                        TypeSignatures.TypeEquality(
                            TypeSignatures.TypeApply(
                                TypeSignatures.TypeName([ "==" ], []),
                                [ TypeSignatures.TypeVariable "x"; TypeSignatures.TypeVariable "y" ]
                            ),
                            TypeSignatures.TypeName([ "True" ], [])
                        )
                    )
                )
            ),
            eqComplete.Body
        )


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
                diagnostic.Code = DiagnosticCode.AssertTerminatesRequiresModuleAttribute
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
                diagnostic.Code = DiagnosticCode.AssertReducibleRequiresModuleAttribute
                && diagnostic.Message.Contains("allow_assert_reducible", StringComparison.Ordinal)
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
    let ``macro deriving shape inspection preserves ordinary constructor field names`` () =
        let workspace =
            compileInMemoryWorkspace
                "memory-deriving-shape-field-names-root"
                [
                    "main.kp",
                    [
                        "module main"
                        "import support.shape_assert.(assertAdtShape)"
                        "data User : Type ="
                        "    User (name : String) (age : Int)"
                        "shapeSmoke : Unit"
                        "let shapeSmoke ="
                        "    $(assertAdtShape @User"
                        "        '{ User }"
                        "        ((\"name\" :: \"age\" :: Nil) :: Nil))"
                    ]
                    |> String.concat "\n"
                    "support/shape_assert.kp",
                    [
                        "module support.shape_assert"
                        "import std.deriving.shape.*"
                        ""
                        "sameStringList : List String -> List String -> Bool"
                        "let sameStringList xs ys ="
                        "    match (xs, ys)"
                        "    case (Nil, Nil) ->"
                        "        True"
                        "    case ((::) x xs1, (::) y ys1) ->"
                        "        (x == y) && sameStringList xs1 ys1"
                        "    case _ ->"
                        "        False"
                        ""
                        "sameStringListList : List (List String) -> List (List String) -> Bool"
                        "let sameStringListList xs ys ="
                        "    match (xs, ys)"
                        "    case (Nil, Nil) ->"
                        "        True"
                        "    case ((::) x xs1, (::) y ys1) ->"
                        "        sameStringList x y && sameStringListList xs1 ys1"
                        "    case _ ->"
                        "        False"
                        ""
                        "fieldNames : List ShapeField -> List String"
                        "let fieldNames fields ="
                        "    match fields"
                        "    case Nil ->"
                        "        Nil"
                        "    case (::) field rest ->"
                        "        (::) field.renderName (fieldNames rest)"
                        ""
                        "constructorFieldNames : List ShapeConstructor -> List (List String)"
                        "let constructorFieldNames constructors ="
                        "    match constructors"
                        "    case Nil ->"
                        "        Nil"
                        "    case (::) ctor rest ->"
                        "        (::) (fieldNames ctor.fields) (constructorFieldNames rest)"
                        ""
                        "assertAdtShape :"
                        "    forall (@0 a : Type)."
                        "    Syntax Type -> List (List String) -> Elab (Syntax Unit)"
                        "let assertAdtShape @a target expectedFields = do"
                        "    shape <- inspectAdt @a target"
                        "    if sameStringListList expectedFields (constructorFieldNames shape.constructors) then"
                        "        unitSyntax"
                        "    else"
                        "        failElabWith \"KAPPA_TEST_SHAPE_MISMATCH\" \"ADT fields\" []"
                    ]
                    |> String.concat "\n"
                ]

        Assert.False(workspace.HasErrors, $"Expected deriving shape field-name expansion to succeed, got:{Environment.NewLine}{diagnosticsSummary workspace.Diagnostics}")


    [<Fact>]
    let ``bundled std hash module exposes real Hashable members and ordinary helpers`` () =
        let workspace =
            compileInMemoryWorkspace
                "memory-bundled-std-hash-surface-root"
                [
                    "main.kp",
                    [
                        "module main"
                        "import std.hash.*"
                        "result : HashCode"
                        "let result = hashWith defaultHashSeed \"a\""
                    ]
                    |> String.concat "\n"
                ]

        Assert.False(workspace.HasErrors, sprintf "Expected bundled std.hash surface to compile, got %A" workspace.Diagnostics)

        let hashDocument =
            workspace.Documents
            |> List.find (fun document -> document.ModuleName = Some [ "std"; "hash" ])

        let traitMembers =
            hashDocument.Syntax.Declarations
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

        Assert.Equal("hashInto : ( & value : a ) -> ( 1 state : HashState ) -> HashState", traitMembers["Hashable"]["hashInto"])

        let hashFieldIsOrdinary =
            hashDocument.Syntax.Declarations
            |> List.exists (function
                | LetDeclaration declaration when declaration.Name = Some "hashField" -> true
                | _ -> false)

        let hashWithIsOrdinary =
            hashDocument.Syntax.Declarations
            |> List.exists (function
                | LetDeclaration declaration when declaration.Name = Some "hashWith" -> true
                | _ -> false)

        Assert.True(hashFieldIsOrdinary, "Expected std.hash.hashField to be an ordinary bundled definition.")
        Assert.True(hashWithIsOrdinary, "Expected std.hash.hashWith to be an ordinary bundled definition.")


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


module SmokeTestsShard3 =

    [<Fact>]
    let ``structured diagnostic facts normalize through the shared formatter`` () =
        let source = createSource "fact.kp" "missing"
        let location = source.GetLocation(TextSpan.FromBounds(0, 7))

        let diagnostic =
            Diagnostics.errorFact
                "KFrontIR"
                (Some(KFrontIRPhase.phaseName CHECKERS))
                (Some location)
                []
                (DiagnosticFact.nameUnresolved "missing")

        Assert.Equal(DiagnosticCode.NameUnresolved, diagnostic.Code)
        Assert.Equal("Name 'missing' is not in scope.", diagnostic.Message)
        Assert.Equal(Some "kappa.name.unresolved", diagnostic.Family)
        Assert.Equal("name-unresolved", diagnostic.Payload.Kind)
        Assert.Contains(
            diagnostic.Payload.Fields,
            fun field ->
                field.Name = "spelling"
                && field.Value = DiagnosticPayloadText "missing"
        )


    [<Fact>]
    let ``lexer reports malformed prefixed numeric literals directly`` () =
        let source =
            createSource
                "__malformed_numeric_literals__.kp"
                "0x 0b102 0o89"

        let lexed = Lexer.tokenize source

        let actualTokens =
            lexed.Tokens
            |> List.filter (fun token ->
                match token.Kind with
                | Newline
                | EndOfFile -> false
                | _ -> true)
            |> List.map (fun token -> token.Kind, token.Text)

        Assert.Equal<(TokenKind * string) list>(
            [
                BadToken, "0x"
                BadToken, "0b102"
                BadToken, "0o89"
            ],
            actualTokens
        )

        let lexicalDiagnostics =
            lexed.Diagnostics
            |> List.filter (fun diagnostic -> diagnostic.Code = DiagnosticCode.MalformedNumericLiteral)

        Assert.Equal(3, lexicalDiagnostics.Length)


    [<Fact>]
    let ``parser reports a dedicated diagnostic for module attributes without a header`` () =
        let sourceText =
            [
                "@PrivateByDefault"
                "let x = 1"
            ]
            |> String.concat "\n"

        let _, lexed, parsed =
            lexAndParse
                "demo/attrs_only.kp"
                sourceText

        Assert.Empty(lexed.Diagnostics)
        Assert.Equal<DiagnosticCode list>([ DiagnosticCode.ModuleHeaderExpectedAfterAttributes ], parsed.Diagnostics |> List.map (fun diagnostic -> diagnostic.Code))
        Assert.Empty(parsed.Syntax.ModuleAttributes)
        Assert.Equal(None, parsed.Syntax.ModuleHeader)


    [<Fact>]
    let ``source compilation exposes opaque data constructors only via clarify in script mode`` () =
        let abstractWorkspace =
            compileInMemoryWorkspaceWithPackageMode
                "memory-import-opaque-data-without-clarify-root"
                false
                [
                    "lib.kp",
                    [
                        "module lib"
                        "opaque data Token : Type ="
                        "    Token"
                    ]
                    |> String.concat "\n"
                    "main.kp",
                    [
                        "module main"
                        "import lib.(type Token(..))"
                        "make : Token"
                        "let make = Token"
                    ]
                    |> String.concat "\n"
                ]

        Assert.True(abstractWorkspace.HasErrors, "Expected opaque data constructors to stay hidden without clarify.")

        let clarifiedWorkspace =
            compileInMemoryWorkspaceWithPackageMode
                "memory-import-opaque-data-with-clarify-root"
                false
                [
                    "lib.kp",
                    [
                        "module lib"
                        "opaque data Token : Type ="
                        "    Token"
                    ]
                    |> String.concat "\n"
                    "main.kp",
                    [
                        "module main"
                        "import lib.(clarify type Token(..))"
                        "make : Token"
                        "let make = Token"
                    ]
                    |> String.concat "\n"
                ]

        Assert.False(clarifiedWorkspace.HasErrors, sprintf "Expected clarify import of opaque data to expose constructors locally, got %A" clarifiedWorkspace.Diagnostics)


    [<Fact>]
    let ``parser accepts multiline instance member bodies before sibling members`` () =
        let sourceText =
            [
                "module main"
                ""
                "trait Eq a ="
                "    (==) : a -> a -> Bool"
                ""
                "instance Eq Int ="
                "    let (==) x y ="
                "        match primitiveIntCompare x y"
                "        case LT -> False"
                "        case EQ -> True"
                "        case GT -> False"
                "    let same x y = x == y"
            ]
            |> String.concat "\n"

        let _, lexed, parsed =
            lexAndParse
                "memory.kp"
                sourceText

        Assert.Empty(lexed.Diagnostics)
        Assert.Empty(parsed.Diagnostics)

        let declaration =
            parsed.Syntax.Declarations
            |> List.pick (function
                | InstanceDeclarationNode declaration -> Some declaration
                | _ -> None)

        match declaration.Members with
        | equality :: same :: [] ->
            Assert.Equal(Some "==", equality.Name)
            Assert.Equal(Some "same", same.Name)
            Assert.True(equality.BodyTokens.Length > 0)
            Assert.True(same.BodyTokens.Length > 0)
        | other ->
            failwithf "Expected two instance members, got %A" other


    [<Fact>]
    let ``package mode rejects sha256 pinned URL imports explicitly when URL providers are unavailable`` () =
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
        Assert.Contains(DiagnosticCode.UrlImportUnsupported, codes)
        Assert.DoesNotContain(DiagnosticCode.ModuleNameUnresolved, codes)


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
    let ``same module fragments merge surface declarations across files`` () =
        let mainSource =
            [
                "module main"
                "x : Int"
                "let x = 1"
            ]
            |> String.concat "\n"

        let extraSource =
            [
                "module main"
                "y : Int"
                "let y = x"
            ]
            |> String.concat "\n"

        let workspace, result =
            evaluateInMemoryBinding
                "memory-module-fragment-merge-root"
                "main.y"
                [
                    "main.kp", mainSource
                    "main.extra.kp", extraSource
                ]

        Assert.False(workspace.HasErrors, sprintf "Expected fragment-merged module to compile, got %A" workspace.Diagnostics)

        match result with
        | Result.Ok value ->
            Assert.Equal("1", RuntimeValue.format value)
        | Result.Error issue ->
            failwithf "Expected cross-fragment binding to evaluate, got %A" issue


    [<Fact>]
    let ``type signatures normalize built in optional postfix`` () =
        let optionalInt = parseTypeText "Int?"
        let preludeOptionInt = parseTypeText "std.prelude.Option Int"

        Assert.True(TypeSignatures.definitionallyEqual optionalInt preludeOptionInt)


    [<Fact>]
    let ``parser preserves tuple syntax quotes with quote local splices`` () =
        let fixities = Parser.bootstrapFixities ()

        let parsed, diagnostics =
            parseExpressionWithFixities fixities "__syntax_quote_tuple_splices__.kp" "'{ (${left}, ${right}) }"

        Assert.Empty(diagnostics)

        match parsed with
        | Some(
            SyntaxQuote(
                RecordLiteral
                    [ { Name = "_1"; Value = SyntaxSplice(Name [ "left" ]) }
                      { Name = "_2"; Value = SyntaxSplice(Name [ "right" ]) } ]
            )
          ) -> ()
        | other ->
            failwithf "Unexpected tuple syntax quote parse shape: %A" other


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
            mainDocument.Diagnostics |> List.exists (fun diagnostic -> diagnostic.Code = DiagnosticCode.ExpectedSyntaxToken),
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
    let ``macro failElab defaults to elaboration failed diagnostics`` () =
        let workspace =
            compileInMemoryWorkspaceWithPackageMode
                "memory-fail-elab-default-code-root"
                false
                [
                    "main.kp",
                    [
                        "module main"
                        "broken : Unit"
                        "let broken = $(failElab \"macro rejected this declaration\")"
                    ]
                    |> String.concat "\n"
                ]

        Assert.True(workspace.HasErrors, "Expected failElab to surface a dedicated elaboration failure diagnostic.")
        Assert.Contains(
            workspace.Diagnostics,
            fun diagnostic ->
                diagnostic.Code = DiagnosticCode.ElaborationFailed
                && diagnostic.Message.Contains("macro rejected this declaration", StringComparison.Ordinal)
        )


    [<Fact>]
    let ``KCore resolves builtin compare through Ord trait dispatch`` () =
        let workspace =
            compileInMemoryWorkspace
                "memory-kcore-compare-builtin-root"
                [
                    "main.kp",
                    [
                        "module main"
                        "let ordering = compare \"a\" \"b\""
                    ]
                    |> String.concat "\n"
                ]

        let orderingBinding =
            workspace.KCore
            |> List.find (fun moduleDump -> moduleDump.Name = "main")
            |> fun moduleDump ->
                moduleDump.Declarations
                |> List.pick (fun declaration ->
                    match declaration.Binding with
                    | Some binding when binding.Name = Some "ordering" -> Some binding
                    | _ -> None)

        match orderingBinding.Body with
        | Some(
            KCoreTraitCall(
                "Ord",
                "compare",
                KCoreDictionaryValue("std.prelude", "Ord", "builtin-prelude:Ord:String", []),
                [ KCoreLiteral(LiteralValue.String "a")
                  KCoreLiteral(LiteralValue.String "b") ]
            )
          ) -> ()
        | other ->
            failwithf "Expected builtin compare to lower through Ord trait dispatch, got %A" other


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
    let ``macro deriving shape can generate a match body for ordinary constructors`` () =
        let workspace =
            compileInMemoryWorkspace
                "memory-deriving-shape-match-root"
                [
                    "main.kp",
                    [
                        "module main"
                        "import support.shape_assert.(deriveTagNameBody)"
                        "data Expr : Type ="
                        "    Lit (value : Int)"
                        "    Add (left : Expr) (right : Expr)"
                        "tagName : Expr -> String"
                        "let tagName value ="
                        "    $(deriveTagNameBody @Expr '{ Expr } '{ value })"
                    ]
                    |> String.concat "\n"
                    "support/shape_assert.kp",
                    [
                        "module support.shape_assert"
                        "import std.deriving.shape.*"
                        "deriveTagNameBody :"
                        "    forall (@0 a : Type)."
                        "    Syntax Type -> Syntax a -> Elab (Syntax String)"
                        "let deriveTagNameBody @a target value = do"
                        "    shape <- inspectAdt @a target"
                        "    matchAdt shape value \\ctor fields ->"
                        "        stringSyntax ctor.renderName"
                    ]
                    |> String.concat "\n"
                ]

        Assert.False(workspace.HasErrors, $"Expected deriving-shape match expansion to succeed, got:{Environment.NewLine}{diagnosticsSummary workspace.Diagnostics}")


    [<Fact>]
    let ``package mode resolves builtin Ord trait member compare for HashCode Bytes and Byte`` () =
        let workspace =
            compileInMemoryWorkspaceWithPackageMode
                "memory-package-builtin-ord-compare-root"
                true
                [
                    "main.kp",
                    [
                        "import std.hash.*"
                        "import std.unicode.*"
                        "let h1 : HashCode = hashWith defaultHashSeed \"abc\""
                        "let h2 : HashCode = hashWith defaultHashSeed \"abc\""
                        "let hashOrd : Ordering = compare h1 h2"
                        "let aBytes : Bytes = utf8Bytes \"a\""
                        "let bBytes : Bytes = utf8Bytes \"b\""
                        "let bytesOrd : Ordering = compare aBytes bBytes"
                        "let low : Byte = b'\\x00'"
                        "let high : Byte = b'\\xFF'"
                        "let byteOrd : Ordering = compare low high"
                    ]
                    |> String.concat "\n"
                ]

        Assert.False(workspace.HasErrors, diagnosticsSummary workspace.Diagnostics)


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


module SmokeTestsShard4 =

    [<Fact>]
    let ``code-detail diagnostic facts normalize through the shared formatter`` () =
        let bag = DiagnosticBag()
        bag.AddError(DiagnosticFact.codeDetail DiagnosticCode.ExpectedSyntaxToken "Expected an expression.")

        let diagnostic = Assert.Single(bag.Items)
        Assert.Equal(DiagnosticCode.ExpectedSyntaxToken, diagnostic.Code)
        Assert.Equal("Expected an expression.", diagnostic.Message)
        Assert.Equal("expected-syntax-token", diagnostic.Payload.Kind)
        Assert.Contains(
            diagnostic.Payload.Fields,
            fun field ->
                field.Name = "detail"
                && field.Value = DiagnosticPayloadText "Expected an expression."
        )

    [<Fact>]
    let ``QTT structured diagnostics render from typed evidence`` () =
        let bag = DiagnosticBag()

        bag.AddError(
            DiagnosticFact.qttLinearOveruse
                (QuantityCannotSatisfyParameterDemand("1", "&", Available))
        )

        let diagnostic = Assert.Single(bag.Items)
        Assert.Equal(DiagnosticCode.QttLinearOveruse, diagnostic.Code)
        Assert.Equal("An argument available at quantity '1' cannot satisfy parameter demand '&'.", diagnostic.Message)
        Assert.Equal("qtt-linear-overuse", diagnostic.Payload.Kind)
        Assert.Contains(
            diagnostic.Payload.Fields,
            fun field ->
                field.Name = "reason"
                && field.Value = DiagnosticPayloadText "quantity-cannot-satisfy-parameter-demand"
        )

    [<Fact>]
    let ``QTT drop diagnostics render canonical messages from typed evidence`` () =
        let bag = DiagnosticBag()

        bag.AddError(
            DiagnosticFact.qttLinearDrop
                (BindingNotConsumedOnEveryPath "file")
        )

        let diagnostic = Assert.Single(bag.Items)
        Assert.Equal(DiagnosticCode.QttLinearDrop, diagnostic.Code)
        Assert.Equal("Linear resource 'file' is not consumed on every path.", diagnostic.Message)
        Assert.Equal("qtt-linear-drop", diagnostic.Payload.Kind)
        Assert.Contains(
            diagnostic.Payload.Fields,
            fun field ->
                field.Name = "binding"
                && field.Value = DiagnosticPayloadText "file"
        )

    [<Fact>]
    let ``frontend import diagnostics render from typed evidence`` () =
        let bag = DiagnosticBag()

        bag.AddError(
            DiagnosticFact.importItemNotFound
                "Hidden"
                "library.secret"
                ExcludedImportItem
        )

        let diagnostic = Assert.Single(bag.Items)
        Assert.Equal(DiagnosticCode.ImportItemNotFound, diagnostic.Code)
        Assert.Equal("Excluded import item 'Hidden' was not found in module 'library.secret'.", diagnostic.Message)
        Assert.Equal("import-item-not-found", diagnostic.Payload.Kind)
        Assert.Contains(
            diagnostic.Payload.Fields,
            fun field ->
                field.Name = "lookup-context"
                && field.Value = DiagnosticPayloadText "excluded-import-item"
        )

    [<Fact>]
    let ``frontend expect and signature diagnostics render from typed evidence`` () =
        let bag = DiagnosticBag()
        bag.AddError(DiagnosticFact.expectUnsatisfied ExpectedTraitDeclaration "Eq")
        bag.AddError(DiagnosticFact.signatureUnsatisfied "main" "IO Unit")

        let diagnostics = bag.Items
        let expectDiagnostic = diagnostics |> List.find (fun item -> item.Code = DiagnosticCode.ExpectUnsatisfied)
        let signatureDiagnostic = diagnostics |> List.find (fun item -> item.Code = DiagnosticCode.SignatureUnsatisfied)

        Assert.Equal("Unsatisfied expect declaration for trait 'Eq'.", expectDiagnostic.Message)
        Assert.Equal("expect-unsatisfied", expectDiagnostic.Payload.Kind)
        Assert.Contains(
            expectDiagnostic.Payload.Fields,
            fun field ->
                field.Name = "expected-kind"
                && field.Value = DiagnosticPayloadText "trait"
        )

        Assert.Equal(
            "Top-level signature 'main : IO Unit' has no matching definition in the same source file. Define it with 'let main = ...' or declare it as 'expect term main : IO Unit'.",
            signatureDiagnostic.Message
        )
        Assert.Equal("signature-unsatisfied", signatureDiagnostic.Payload.Kind)
        Assert.Contains(
            signatureDiagnostic.Payload.Fields,
            fun field ->
                field.Name = "signature-name"
                && field.Value = DiagnosticPayloadText "main"
        )

    [<Fact>]
    let ``frontend url import and refl diagnostics render from typed evidence`` () =
        let bag = DiagnosticBag()
        bag.AddError(DiagnosticFact.urlImportRefPinRequiresLock "https://example.com/mod.kp#ref=main")
        bag.AddError(DiagnosticFact.reflRequiresDefinitionallyEqualSides)

        let diagnostics = bag.Items
        let urlDiagnostic = diagnostics |> List.find (fun item -> item.Code = DiagnosticCode.UrlImportRefPinRequiresLock)
        let reflDiagnostic = diagnostics |> List.find (fun item -> item.Code = DiagnosticCode.TypeEqualityMismatch)

        Assert.Equal(
            "URL import 'https://example.com/mod.kp#ref=main' uses a ref pin, but this toolchain has no recorded immutable resolution for it in package mode.",
            urlDiagnostic.Message
        )
        Assert.Equal("url-import-package-mode", urlDiagnostic.Payload.Kind)
        Assert.Contains(
            urlDiagnostic.Payload.Fields,
            fun field ->
                field.Name = "reason"
                && field.Value = DiagnosticPayloadText "url-import-ref-pin-requires-lock"
        )

        Assert.Equal(
            "The proof term 'refl' requires both sides of the equality type to be definitionally equal. Function binder quantities are part of type identity.",
            reflDiagnostic.Message
        )
        Assert.Equal("type-equality-mismatch", reflDiagnostic.Payload.Kind)
        Assert.Contains(
            reflDiagnostic.Payload.Fields,
            fun field ->
                field.Name = "reason"
                && field.Value = DiagnosticPayloadText "refl-requires-definitionally-equal-sides"
        )

    [<Fact>]
    let ``parser syntax diagnostics render from typed evidence`` () =
        let bag = DiagnosticBag()
        bag.AddError(DiagnosticFact.parserSyntax (ExpectedListComma ImportItemList))
        bag.AddError(DiagnosticFact.parserSyntax (ExpectedEqualsInDeclaration(ParserNamedDeclarationSubject "let")))
        bag.AddError(DiagnosticFact.parserSyntax (TotalityAssertionNotApplicableTo SignatureDeclarationTarget))

        let diagnostics = bag.Items
        let commaDiagnostic = diagnostics |> List.find (fun item -> item.Message = "Expected ',' between import items.")
        let equalsDiagnostic = diagnostics |> List.find (fun item -> item.Message = "Expected '=' in the let declaration.")
        let totalityDiagnostic =
            diagnostics
            |> List.find (fun item -> item.Message = "Totality assertions do not apply to signature declarations.")

        Assert.Equal(DiagnosticCode.ExpectedSyntaxToken, commaDiagnostic.Code)
        Assert.Equal("parser-syntax", commaDiagnostic.Payload.Kind)
        Assert.Contains(
            commaDiagnostic.Payload.Fields,
            fun field ->
                field.Name = "list-context"
                && field.Value = DiagnosticPayloadText "import item list"
        )

        Assert.Equal(DiagnosticCode.ExpectedSyntaxToken, equalsDiagnostic.Code)
        Assert.Contains(
            equalsDiagnostic.Payload.Fields,
            fun field ->
                field.Name = "declaration-subject"
                && field.Value = DiagnosticPayloadText "let declaration"
        )

        Assert.Equal(DiagnosticCode.ExpectedSyntaxToken, totalityDiagnostic.Code)
        Assert.Contains(
            totalityDiagnostic.Payload.Fields,
            fun field ->
                field.Name = "target"
                && field.Value = DiagnosticPayloadText "signature declarations"
        )

    [<Fact>]
    let ``parser dynamic diagnostics render from typed evidence`` () =
        let bag = DiagnosticBag()
        bag.AddError(DiagnosticFact.parserSyntax (InvalidStringLiteral(UnknownEscapeSequence "\\q")))
        bag.AddError(
            DiagnosticFact.parserSyntax
                (InvalidUrlModuleSpecifier("https://example.com/lib#sha256:not-hex", InvalidSha256Digest "sha256:not-hex"))
        )

        let diagnostics = bag.Items
        let literalDiagnostic =
            diagnostics
            |> List.find (fun item -> item.Message = "String literal text is invalid: unknown escape sequence '\\q'.")

        let urlDiagnostic =
            diagnostics
            |> List.find (fun item -> item.Message = "URL module specifier 'https://example.com/lib#sha256:not-hex' is invalid: sha256 pins must use a hexadecimal digest.")

        Assert.Equal(DiagnosticCode.ExpectedSyntaxToken, literalDiagnostic.Code)
        Assert.Equal("parser-syntax", literalDiagnostic.Payload.Kind)
        Assert.Contains(
            literalDiagnostic.Payload.Fields,
            fun field ->
                field.Name = "string-literal-error"
                && field.Value = DiagnosticPayloadText "unknown-escape-sequence"
        )

        Assert.Equal(DiagnosticCode.ExpectedSyntaxToken, urlDiagnostic.Code)
        Assert.Equal("parser-syntax", urlDiagnostic.Payload.Kind)
        Assert.Contains(
            urlDiagnostic.Payload.Fields,
            fun field ->
                field.Name = "url-parse-error"
                && field.Value = DiagnosticPayloadText "invalid-sha256-digest"
        )

    [<Fact>]
    let ``core pattern parsing diagnostics render from typed evidence`` () =
        let bag = DiagnosticBag()
        bag.AddError(DiagnosticFact.corePatternParsing (NumericLiteralSuffixesNotPermittedInPatterns "1u8"))
        bag.AddError(DiagnosticFact.corePatternParsing ExpectedRecordPatternFieldLabel)
        bag.AddError(DiagnosticFact.corePatternParsing OrPatternAlternativesMustBindSameNames)
        bag.AddError(DiagnosticFact.corePatternParsing (InvalidNumericLiteralPattern(InvalidNumericLiteral "1e+")))

        let diagnostics = bag.Items
        let literalDiagnostic =
            diagnostics
            |> List.find (fun item -> item.Message = "Numeric literal suffixes are not permitted in patterns: '1u8'.")

        let fieldDiagnostic =
            diagnostics
            |> List.find (fun item -> item.Message = "Expected a record pattern field label.")

        let orPatternDiagnostic =
            diagnostics
            |> List.find (fun item -> item.Code = DiagnosticCode.OrPatternBinderMismatch)

        let invalidNumericDiagnostic =
            diagnostics
            |> List.find (fun item -> item.Message = "Numeric literal pattern text is invalid: '1e+'.")

        Assert.Equal("core-pattern-parsing", literalDiagnostic.Payload.Kind)
        Assert.Contains(
            literalDiagnostic.Payload.Fields,
            fun field ->
                field.Name = "token-text"
                && field.Value = DiagnosticPayloadText "1u8"
        )

        Assert.Equal(DiagnosticCode.ExpectedSyntaxToken, fieldDiagnostic.Code)
        Assert.Contains(
            fieldDiagnostic.Payload.Fields,
            fun field ->
                field.Name = "reason"
                && field.Value = DiagnosticPayloadText "expected-record-pattern-field-label"
        )

        Assert.Equal("Each or-pattern alternative must bind the same set of names.", orPatternDiagnostic.Message)
        Assert.Equal("core-pattern-parsing", orPatternDiagnostic.Payload.Kind)
        Assert.Equal(DiagnosticCode.ExpectedSyntaxToken, invalidNumericDiagnostic.Code)
        Assert.Contains(
            invalidNumericDiagnostic.Payload.Fields,
            fun field ->
                field.Name = "numeric-literal-error"
                && field.Value = DiagnosticPayloadText "invalid-numeric-literal"
        )

    [<Fact>]
    let ``core expression parsing diagnostics render from typed evidence`` () =
        let bag = DiagnosticBag()
        bag.AddError(DiagnosticFact.coreExpressionParsing ExpectedUsingBindingArrow)
        bag.AddError(DiagnosticFact.coreExpressionParsing MatchExpressionMustDeclareAtLeastOneCase)
        bag.AddError(DiagnosticFact.coreExpressionParsing UnexpectedIndentationInIndentedCaseBody)
        bag.AddError(DiagnosticFact.coreExpressionParsing (InvalidNumericLiteralExpression(InvalidNumericLiteral "1e+")))
        bag.AddError(DiagnosticFact.coreExpressionParsing (InvalidStringTextSegment(UnknownEscapeSequence "\\q")))
        bag.AddError(DiagnosticFact.coreExpressionParsing ExpectedComprehensionGeneratorIn)
        bag.AddError(DiagnosticFact.coreExpressionParsing (QueryPagingRequiresOrderedPipeline "skip"))
        bag.AddError(DiagnosticFact.coreExpressionParsing ComprehensionMustEndWithYieldClause)
        bag.AddError(DiagnosticFact.coreExpressionParsing ExpectedHandlerClauseArrow)
        bag.AddError(DiagnosticFact.coreExpressionParsing ExpectedHandlerWith)
        bag.AddError(DiagnosticFact.coreExpressionParsing (ExpectedImplicitParameterAfterAt TopLevelFunctionHeader))
        bag.AddError(DiagnosticFact.coreExpressionParsing (ExpectedImplicitParameterAfterAt LocalFunctionHeader))
        bag.AddError(DiagnosticFact.coreExpressionParsing ExpectedNameAfterEffectLabel)
        bag.AddError(DiagnosticFact.coreExpressionParsing ExpectedExplicitMemberProjectionName)
        bag.AddError(DiagnosticFact.coreExpressionParsing ExpectedSafeNavigationMemberAccess)
        bag.AddError(DiagnosticFact.coreExpressionParsing UnexpectedTrailingExpressionTokens)
        bag.AddError(DiagnosticFact.coreExpressionParsing ExpectedNamedApplicationFieldLabel)
        bag.AddError(DiagnosticFact.coreExpressionParsing ExpectedNamedApplicationField)
        bag.AddError(DiagnosticFact.coreExpressionParsing ExpectedRecordFieldLabel)
        bag.AddError(DiagnosticFact.coreExpressionParsing ExpectedRecordField)
        bag.AddError(DiagnosticFact.coreExpressionParsing ExpectedSealAs)
        bag.AddError(DiagnosticFact.coreExpressionParsing ExpectedSealValue)
        bag.AddError(DiagnosticFact.coreExpressionParsing ExpectedProjectionBodyHead)
        bag.AddError(DiagnosticFact.coreExpressionParsing ExpectedProjectionMatchCaseBlock)
        bag.AddError(DiagnosticFact.coreExpressionParsing ExpectedProjectionMatchCaseClause)
        bag.AddError(DiagnosticFact.coreExpressionParsing ExpectedProjectionCaseClauseArrow)
        bag.AddError(DiagnosticFact.coreExpressionParsing ExpectedLocalLetIn)
        bag.AddError(DiagnosticFact.coreExpressionParsing ExpectedLocalLetEquals)
        bag.AddError(DiagnosticFact.coreExpressionParsing ExpectedExpressionCloseParenthesis)
        bag.AddError(DiagnosticFact.coreExpressionParsing ExpectedExpression)
        bag.AddError(DiagnosticFact.coreExpressionParsing ExpectedInterpolationEndBeforeStringResumes)
        bag.AddError(DiagnosticFact.coreExpressionParsing ExpectedInterpolatedStringContent)
        bag.AddError(DiagnosticFact.coreExpressionParsing UnterminatedInterpolatedString)
        bag.AddError(DiagnosticFact.coreExpressionParsing DuplicateHandlerReturnClause)
        bag.AddError(DiagnosticFact.coreExpressionParsing MissingHandlerReturnClause)
        bag.AddError(DiagnosticFact.coreExpressionParsing (HandlerReturnClauseArityMismatch 2))
        bag.AddError(DiagnosticFact.coreExpressionParsing RecordPatchExtensionMustBeTopLevelLabel)
        bag.AddError(DiagnosticFact.coreExpressionParsing ExpectedRecordPatchPath)
        bag.AddError(DiagnosticFact.coreExpressionParsing ExpectedRecordPatchItem)
        bag.AddError(DiagnosticFact.coreExpressionParsing ExpectedProjectionThen)
        bag.AddError(DiagnosticFact.coreExpressionParsing ExpectedProjectionElse)

        let diagnostics = bag.Items
        let usingDiagnostic =
            diagnostics
            |> List.find (fun item -> item.Message = "Expected '<-' in the using binding.")

        let matchDiagnostic =
            diagnostics
            |> List.find (fun item -> item.Message = "A match expression must declare at least one case.")

        let indentationDiagnostic =
            diagnostics
            |> List.find (fun item -> item.Code = DiagnosticCode.UnexpectedIndentation)

        let invalidNumericDiagnostic =
            diagnostics
            |> List.find (fun item -> item.Message = "Numeric literal text is invalid: '1e+'.")

        let invalidStringSegmentDiagnostic =
            diagnostics
            |> List.find (fun item -> item.Message = "String text segment is invalid: unknown escape sequence '\\q'.")

        let generatorDiagnostic =
            diagnostics
            |> List.find (fun item -> item.Message = "Expected 'in' in the comprehension generator.")

        let pagingDiagnostic =
            diagnostics
            |> List.find (fun item -> item.Message = "skip and take require an ordered query pipeline; the current pipeline is unordered.")

        let yieldDiagnostic =
            diagnostics
            |> List.find (fun item -> item.Message = "A comprehension must end with a yield clause.")

        let handlerArrowDiagnostic =
            diagnostics
            |> List.find (fun item -> item.Message = "Expected '->' in the handler clause.")

        let handlerWithDiagnostic =
            diagnostics
            |> List.find (fun item -> item.Message = "Expected 'with' in the handler expression.")

        let functionHeaderDiagnostic =
            diagnostics
            |> List.find (fun item -> item.Message = "Expected an implicit parameter after '@' in the function header.")

        let localFunctionHeaderDiagnostic =
            diagnostics
            |> List.find (fun item -> item.Message = "Expected an implicit parameter after '@' in the local function header.")

        let effectLabelDiagnostic =
            diagnostics
            |> List.find (fun item -> item.Message = "Expected a name after 'effect-label'.")

        let explicitProjectionDiagnostic =
            diagnostics
            |> List.find (fun item -> item.Message = "Expected an operator or member name inside explicit member projection.")

        let safeNavigationDiagnostic =
            diagnostics
            |> List.find (fun item -> item.Message = "Expected a member access after '?.'.")

        let trailingDiagnostic =
            diagnostics
            |> List.find (fun item -> item.Message = "Unexpected tokens at the end of the expression.")

        let namedApplicationFieldLabelDiagnostic =
            diagnostics
            |> List.find (fun item -> item.Message = "Expected a named application field label.")

        let namedApplicationFieldDiagnostic =
            diagnostics
            |> List.find (fun item -> item.Message = "Expected a named application field of the form 'name = expr' or a punned field name.")

        let recordFieldLabelDiagnostic =
            diagnostics
            |> List.find (fun item -> item.Message = "Expected a record field label.")

        let recordFieldDiagnostic =
            diagnostics
            |> List.find (fun item -> item.Message = "Expected a record field of the form 'name = expr' or a punned field name.")

        let sealAsDiagnostic =
            diagnostics
            |> List.find (fun item -> item.Message = "Expected 'as' in the seal expression.")

        let sealValueDiagnostic =
            diagnostics
            |> List.find (fun item -> item.Message = "Expected a value to seal.")

        let projectionBodyHeadDiagnostic =
            diagnostics
            |> List.find (fun item -> item.Message = "Expected 'yield', 'if', or 'match' in the projection body.")

        let projectionMatchCaseBlockDiagnostic =
            diagnostics
            |> List.find (fun item -> item.Message = "Expected an indented case block in the projection match body.")

        let projectionMatchCaseClauseDiagnostic =
            diagnostics
            |> List.find (fun item -> item.Message = "Expected a 'case' clause in the projection match body.")

        let projectionCaseClauseArrowDiagnostic =
            diagnostics
            |> List.find (fun item -> item.Message = "Expected '->' in the projection case clause.")

        let localLetInDiagnostic =
            diagnostics
            |> List.find (fun item -> item.Message = "Expected 'in' after the local let binding.")

        let localLetEqualsDiagnostic =
            diagnostics
            |> List.find (fun item -> item.Message = "Expected '=' in the local let binding.")

        let expressionCloseParenthesisDiagnostic =
            diagnostics
            |> List.find (fun item -> item.Message = "Expected ')' to close the expression.")

        let expressionDiagnostic =
            diagnostics
            |> List.find (fun item -> item.Message = "Expected an expression.")

        let interpolationEndDiagnostic =
            diagnostics
            |> List.find (fun item -> item.Message = "Expected the interpolation to end before the string resumes.")

        let interpolatedStringContentDiagnostic =
            diagnostics
            |> List.find (fun item -> item.Message = "Expected interpolated string content.")

        let unterminatedInterpolatedStringDiagnostic =
            diagnostics
            |> List.find (fun item -> item.Message = "Unterminated interpolated string.")

        let duplicateHandlerReturnDiagnostic =
            diagnostics
            |> List.find (fun item -> item.Message = "A handler must not define more than one return clause.")

        let missingHandlerReturnDiagnostic =
            diagnostics
            |> List.find (fun item -> item.Message = "A handler must define exactly one return clause of the form 'case return x -> ...'.")

        let handlerReturnArityDiagnostic =
            diagnostics
            |> List.find (fun item -> item.Message = "A handler return clause must bind exactly one payload argument, but binds 2.")

        let recordPatchTopLevelLabelDiagnostic =
            diagnostics
            |> List.find (fun item -> item.Message = "Row-extension fields must be top-level labels of the form 'name := expr'.")

        let recordPatchPathDiagnostic =
            diagnostics
            |> List.find (fun item -> item.Message = "Expected a record patch path before '=' or ':='.")

        let recordPatchItemDiagnostic =
            diagnostics
            |> List.find (fun item -> item.Message = "Expected a record patch item of the form 'path = expr' or 'name := expr'.")

        let projectionThenDiagnostic =
            diagnostics
            |> List.find (fun item -> item.Message = "Expected 'then' in the projection body.")

        let projectionElseDiagnostic =
            diagnostics
            |> List.find (fun item -> item.Message = "Expected 'else' in the projection body.")

        Assert.Equal("core-expression-parsing", usingDiagnostic.Payload.Kind)
        Assert.Contains(
            usingDiagnostic.Payload.Fields,
            fun field ->
                field.Name = "reason"
                && field.Value = DiagnosticPayloadText "expected-using-binding-arrow"
        )

        Assert.Equal(DiagnosticCode.ExpectedSyntaxToken, matchDiagnostic.Code)
        Assert.Contains(
            matchDiagnostic.Payload.Fields,
            fun field ->
                field.Name = "reason"
                && field.Value = DiagnosticPayloadText "match-expression-must-declare-at-least-one-case"
        )

        Assert.Equal("Unexpected indentation.", indentationDiagnostic.Message)
        Assert.Equal("core-expression-parsing", indentationDiagnostic.Payload.Kind)
        Assert.Contains(
            invalidNumericDiagnostic.Payload.Fields,
            fun field ->
                field.Name = "numeric-literal-error"
                && field.Value = DiagnosticPayloadText "invalid-numeric-literal"
        )
        Assert.Contains(
            invalidStringSegmentDiagnostic.Payload.Fields,
            fun field ->
                field.Name = "string-literal-error"
                && field.Value = DiagnosticPayloadText "unknown-escape-sequence"
        )
        Assert.Contains(
            generatorDiagnostic.Payload.Fields,
            fun field ->
                field.Name = "reason"
                && field.Value = DiagnosticPayloadText "expected-comprehension-generator-in"
        )
        Assert.Contains(
            pagingDiagnostic.Payload.Fields,
            fun field ->
                field.Name = "operation-kind"
                && field.Value = DiagnosticPayloadText "skip"
        )
        Assert.Contains(
            yieldDiagnostic.Payload.Fields,
            fun field ->
                field.Name = "reason"
                && field.Value = DiagnosticPayloadText "comprehension-must-end-with-yield-clause"
        )
        Assert.Contains(
            handlerArrowDiagnostic.Payload.Fields,
            fun field ->
                field.Name = "reason"
                && field.Value = DiagnosticPayloadText "expected-handler-clause-arrow"
        )
        Assert.Contains(
            handlerWithDiagnostic.Payload.Fields,
            fun field ->
                field.Name = "reason"
                && field.Value = DiagnosticPayloadText "expected-handler-with"
        )
        Assert.Contains(
            functionHeaderDiagnostic.Payload.Fields,
            fun field ->
                field.Name = "header-context"
                && field.Value = DiagnosticPayloadText "function-header"
        )
        Assert.Contains(
            localFunctionHeaderDiagnostic.Payload.Fields,
            fun field ->
                field.Name = "header-context"
                && field.Value = DiagnosticPayloadText "local-function-header"
        )
        Assert.Contains(
            effectLabelDiagnostic.Payload.Fields,
            fun field ->
                field.Name = "reason"
                && field.Value = DiagnosticPayloadText "expected-name-after-effect-label"
        )
        Assert.Contains(
            explicitProjectionDiagnostic.Payload.Fields,
            fun field ->
                field.Name = "reason"
                && field.Value = DiagnosticPayloadText "expected-explicit-member-projection-name"
        )
        Assert.Contains(
            safeNavigationDiagnostic.Payload.Fields,
            fun field ->
                field.Name = "reason"
                && field.Value = DiagnosticPayloadText "expected-safe-navigation-member-access"
        )
        Assert.Contains(
            trailingDiagnostic.Payload.Fields,
            fun field ->
                field.Name = "reason"
                && field.Value = DiagnosticPayloadText "unexpected-trailing-expression-tokens"
        )
        Assert.Contains(
            namedApplicationFieldLabelDiagnostic.Payload.Fields,
            fun field ->
                field.Name = "reason"
                && field.Value = DiagnosticPayloadText "expected-named-application-field-label"
        )
        Assert.Contains(
            namedApplicationFieldDiagnostic.Payload.Fields,
            fun field ->
                field.Name = "reason"
                && field.Value = DiagnosticPayloadText "expected-named-application-field"
        )
        Assert.Contains(
            recordFieldLabelDiagnostic.Payload.Fields,
            fun field ->
                field.Name = "reason"
                && field.Value = DiagnosticPayloadText "expected-record-field-label"
        )
        Assert.Contains(
            recordFieldDiagnostic.Payload.Fields,
            fun field ->
                field.Name = "reason"
                && field.Value = DiagnosticPayloadText "expected-record-field"
        )
        Assert.Contains(
            sealAsDiagnostic.Payload.Fields,
            fun field ->
                field.Name = "reason"
                && field.Value = DiagnosticPayloadText "expected-seal-as"
        )
        Assert.Contains(
            sealValueDiagnostic.Payload.Fields,
            fun field ->
                field.Name = "reason"
                && field.Value = DiagnosticPayloadText "expected-seal-value"
        )
        Assert.Contains(
            projectionBodyHeadDiagnostic.Payload.Fields,
            fun field ->
                field.Name = "reason"
                && field.Value = DiagnosticPayloadText "expected-projection-body-head"
        )
        Assert.Contains(
            projectionMatchCaseBlockDiagnostic.Payload.Fields,
            fun field ->
                field.Name = "reason"
                && field.Value = DiagnosticPayloadText "expected-projection-match-case-block"
        )
        Assert.Contains(
            projectionMatchCaseClauseDiagnostic.Payload.Fields,
            fun field ->
                field.Name = "reason"
                && field.Value = DiagnosticPayloadText "expected-projection-match-case-clause"
        )
        Assert.Contains(
            projectionCaseClauseArrowDiagnostic.Payload.Fields,
            fun field ->
                field.Name = "reason"
                && field.Value = DiagnosticPayloadText "expected-projection-case-clause-arrow"
        )
        Assert.Contains(
            localLetInDiagnostic.Payload.Fields,
            fun field ->
                field.Name = "reason"
                && field.Value = DiagnosticPayloadText "expected-local-let-in"
        )
        Assert.Contains(
            localLetEqualsDiagnostic.Payload.Fields,
            fun field ->
                field.Name = "reason"
                && field.Value = DiagnosticPayloadText "expected-local-let-equals"
        )
        Assert.Contains(
            expressionCloseParenthesisDiagnostic.Payload.Fields,
            fun field ->
                field.Name = "reason"
                && field.Value = DiagnosticPayloadText "expected-expression-close-parenthesis"
        )
        Assert.Contains(
            expressionDiagnostic.Payload.Fields,
            fun field ->
                field.Name = "reason"
                && field.Value = DiagnosticPayloadText "expected-expression"
        )
        Assert.Contains(
            interpolationEndDiagnostic.Payload.Fields,
            fun field ->
                field.Name = "reason"
                && field.Value = DiagnosticPayloadText "expected-interpolation-end-before-string-resumes"
        )
        Assert.Contains(
            interpolatedStringContentDiagnostic.Payload.Fields,
            fun field ->
                field.Name = "reason"
                && field.Value = DiagnosticPayloadText "expected-interpolated-string-content"
        )
        Assert.Contains(
            unterminatedInterpolatedStringDiagnostic.Payload.Fields,
            fun field ->
                field.Name = "reason"
                && field.Value = DiagnosticPayloadText "unterminated-interpolated-string"
        )
        Assert.Contains(
            duplicateHandlerReturnDiagnostic.Payload.Fields,
            fun field ->
                field.Name = "reason"
                && field.Value = DiagnosticPayloadText "duplicate-handler-return-clause"
        )
        Assert.Contains(
            missingHandlerReturnDiagnostic.Payload.Fields,
            fun field ->
                field.Name = "reason"
                && field.Value = DiagnosticPayloadText "missing-handler-return-clause"
        )
        Assert.Contains(
            handlerReturnArityDiagnostic.Payload.Fields,
            fun field ->
                field.Name = "argument-count"
                && field.Value = DiagnosticPayloadText "2"
        )
        Assert.Contains(
            recordPatchTopLevelLabelDiagnostic.Payload.Fields,
            fun field ->
                field.Name = "reason"
                && field.Value = DiagnosticPayloadText "record-patch-extension-must-be-top-level-label"
        )
        Assert.Contains(
            recordPatchPathDiagnostic.Payload.Fields,
            fun field ->
                field.Name = "reason"
                && field.Value = DiagnosticPayloadText "expected-record-patch-path"
        )
        Assert.Contains(
            recordPatchItemDiagnostic.Payload.Fields,
            fun field ->
                field.Name = "reason"
                && field.Value = DiagnosticPayloadText "expected-record-patch-item"
        )
        Assert.Contains(
            projectionThenDiagnostic.Payload.Fields,
            fun field ->
                field.Name = "reason"
                && field.Value = DiagnosticPayloadText "expected-projection-then"
        )
        Assert.Contains(
            projectionElseDiagnostic.Payload.Fields,
            fun field ->
                field.Name = "reason"
                && field.Value = DiagnosticPayloadText "expected-projection-else"
        )

    [<Fact>]
    let ``unicode literal diagnostics render from typed evidence`` () =
        let bag = DiagnosticBag()
        bag.AddError(DiagnosticFact.unicodeInvalidScalarLiteral (UnicodeScalarTextInvalid(UnknownEscapeSequence "\\q")))
        bag.AddError(DiagnosticFact.unicodeInvalidGraphemeLiteral GraphemeMustDecodeToExactlyOneExtendedCluster)
        bag.AddError(DiagnosticFact.unicodeInvalidByteLiteral (ByteInvalidEscape "\\xGG"))

        let diagnostics = bag.Items
        let scalarDiagnostic =
            diagnostics
            |> List.find (fun item -> item.Code = DiagnosticCode.UnicodeInvalidScalarLiteral)

        let graphemeDiagnostic =
            diagnostics
            |> List.find (fun item -> item.Code = DiagnosticCode.UnicodeInvalidGraphemeLiteral)

        let byteDiagnostic =
            diagnostics
            |> List.find (fun item -> item.Code = DiagnosticCode.UnicodeInvalidByteLiteral)

        Assert.Equal("Unicode scalar literal text is invalid: unknown escape sequence '\\q'.", scalarDiagnostic.Message)
        Assert.Equal("unicode-invalid-scalar-literal", scalarDiagnostic.Payload.Kind)
        Assert.Contains(
            scalarDiagnostic.Payload.Fields,
            fun field ->
                field.Name = "unicode-scalar-error"
                && field.Value = DiagnosticPayloadText "unknown-escape-sequence"
        )

        Assert.Equal("Grapheme literal must decode to exactly one extended grapheme cluster.", graphemeDiagnostic.Message)
        Assert.Equal("unicode-invalid-grapheme-literal", graphemeDiagnostic.Payload.Kind)
        Assert.Contains(
            graphemeDiagnostic.Payload.Fields,
            fun field ->
                field.Name = "reason"
                && field.Value = DiagnosticPayloadText "must-decode-to-exactly-one-extended-cluster"
        )

        Assert.Equal("Byte literal text is invalid: invalid byte escape '\\xGG'.", byteDiagnostic.Message)
        Assert.Equal("unicode-invalid-byte-literal", byteDiagnostic.Payload.Kind)
        Assert.Contains(
            byteDiagnostic.Payload.Fields,
            fun field ->
                field.Name = "byte-literal-error"
                && field.Value = DiagnosticPayloadText "invalid-byte-escape"
        )


    [<Fact>]
    let ``byte literal decoder preserves raw byte escape provenance`` () =
        match SyntaxFacts.tryDecodeByteLiteral "'\\xFF'" with
        | Result.Ok value ->
            Assert.Equal(0xFFuy, value)
        | Result.Error error ->
            failwithf "Expected \\xFF byte escape to decode, got %A" error

        match SyntaxFacts.tryDecodeByteLiteral "'\\u{61}'" with
        | Result.Ok value ->
            Assert.Equal(byte 'a', value)
        | Result.Error error ->
            failwithf "Expected one-byte Unicode escape to decode, got %A" error

        match SyntaxFacts.tryDecodeByteLiteral "'ÿ'" with
        | Result.Ok value ->
            failwithf "Expected raw non-ASCII byte literal to be rejected, but decoded to %A" value
        | Result.Error ByteMustDecodeToExactlyOneByte -> ()
        | Result.Error other -> failwithf "Expected exact one-byte failure, got %A" other

        match SyntaxFacts.tryDecodeByteLiteral "'\\u{00FF}'" with
        | Result.Ok value ->
            failwithf "Expected two-byte UTF-8 Unicode escape to be rejected, but decoded to %A" value
        | Result.Error ByteMustDecodeToExactlyOneByte -> ()
        | Result.Error other -> failwithf "Expected exact one-byte failure, got %A" other

        match SyntaxFacts.tryDecodeByteLiteral "'\\u{0080}'" with
        | Result.Ok value ->
            failwithf "Expected non-ASCII two-byte UTF-8 escape to be rejected, but decoded to %A" value
        | Result.Error ByteMustDecodeToExactlyOneByte -> ()
        | Result.Error other -> failwithf "Expected exact one-byte failure, got %A" other

    [<Fact>]
    let ``parser reports structured unicode literal diagnostics`` () =
        let sourceText =
            [
                "module main"
                "let badScalar = '\\q'"
                "let badGrapheme = g'ab'"
                "let badByte = b'\\u{00FF}'"
            ]
            |> String.concat "\n"

        let _, lexed, parsed =
            lexAndParse
                "main.kp"
                sourceText

        Assert.Empty(lexed.Diagnostics)

        let scalarDiagnostic =
            parsed.Diagnostics
            |> List.find (fun diagnostic -> diagnostic.Code = DiagnosticCode.UnicodeInvalidScalarLiteral)

        let graphemeDiagnostic =
            parsed.Diagnostics
            |> List.find (fun diagnostic -> diagnostic.Code = DiagnosticCode.UnicodeInvalidGraphemeLiteral)

        let byteDiagnostic =
            parsed.Diagnostics
            |> List.find (fun diagnostic -> diagnostic.Code = DiagnosticCode.UnicodeInvalidByteLiteral)

        Assert.Equal("unicode-invalid-scalar-literal", scalarDiagnostic.Payload.Kind)
        Assert.Equal(Some "unknown-escape-sequence", tryFindPayloadText "unicode-scalar-error" scalarDiagnostic)
        Assert.Equal("unicode-invalid-grapheme-literal", graphemeDiagnostic.Payload.Kind)
        Assert.Equal(Some "must-decode-to-exactly-one-extended-cluster", tryFindPayloadText "reason" graphemeDiagnostic)
        Assert.Equal("unicode-invalid-byte-literal", byteDiagnostic.Payload.Kind)
        Assert.Equal(Some "must-decode-to-exactly-one-byte", tryFindPayloadText "reason" byteDiagnostic)


    [<Fact>]
    let ``parser reports structured comprehension diagnostics`` () =
        let sourceText =
            [
                "module main"
                "let missingIn = [ for x xs, yield x ]"
                "let missingYield = [ for x in xs ]"
                "let unorderedSkip = [ for x in Set [1, 2], skip 1, yield x ]"
            ]
            |> String.concat "\n"

        let _, lexed, parsed =
            lexAndParse
                "main.kp"
                sourceText

        Assert.Empty(lexed.Diagnostics)

        let generatorDiagnostic =
            parsed.Diagnostics
            |> List.find (fun diagnostic -> diagnostic.Message = "Expected 'in' in the comprehension generator.")

        let yieldDiagnostic =
            parsed.Diagnostics
            |> List.find (fun diagnostic -> diagnostic.Message = "A comprehension must end with a yield clause.")

        let pagingDiagnostic =
            parsed.Diagnostics
            |> List.find (fun diagnostic ->
                diagnostic.Message = "skip and take require an ordered query pipeline; the current pipeline is unordered."
            )

        Assert.Equal("core-expression-parsing", generatorDiagnostic.Payload.Kind)
        Assert.Equal(Some "expected-comprehension-generator-in", tryFindPayloadText "reason" generatorDiagnostic)
        Assert.Equal("core-expression-parsing", yieldDiagnostic.Payload.Kind)
        Assert.Equal(Some "comprehension-must-end-with-yield-clause", tryFindPayloadText "reason" yieldDiagnostic)
        Assert.Equal("core-expression-parsing", pagingDiagnostic.Payload.Kind)
        Assert.Equal(Some "query-paging-requires-ordered-pipeline", tryFindPayloadText "reason" pagingDiagnostic)
        Assert.Equal(Some "skip", tryFindPayloadText "operation-kind" pagingDiagnostic)

    [<Fact>]
    let ``parser reports structured handler diagnostics`` () =
        let sourceText =
            [
                "module main"
                "let missingWith = handle Ask comp"
                "let missingCase ="
                "    handle Ask comp with"
                "        return x -> pure x"
                "let missingArrow ="
                "    handle Ask comp with"
                "        case return x pure x"
            ]
            |> String.concat "\n"

        let _, lexed, parsed =
            lexAndParse
                "main.kp"
                sourceText

        Assert.Empty(lexed.Diagnostics)

        let withDiagnostic =
            parsed.Diagnostics
            |> List.find (fun diagnostic -> diagnostic.Message = "Expected 'with' in the handler expression.")

        let caseDiagnostic =
            parsed.Diagnostics
            |> List.find (fun diagnostic -> diagnostic.Message = "Expected a handler clause starting with 'case'.")

        let arrowDiagnostic =
            parsed.Diagnostics
            |> List.find (fun diagnostic -> diagnostic.Message = "Expected '->' in the handler clause.")

        Assert.Equal("core-expression-parsing", withDiagnostic.Payload.Kind)
        Assert.Equal(Some "expected-handler-with", tryFindPayloadText "reason" withDiagnostic)
        Assert.Equal("core-expression-parsing", caseDiagnostic.Payload.Kind)
        Assert.Equal(Some "expected-handler-clause-starting-with-case", tryFindPayloadText "reason" caseDiagnostic)
        Assert.Equal("core-expression-parsing", arrowDiagnostic.Payload.Kind)
        Assert.Equal(Some "expected-handler-clause-arrow", tryFindPayloadText "reason" arrowDiagnostic)

    [<Fact>]
    let ``parser reports structured handler return clause validation diagnostics`` () =
        let sourceText =
            [
                "module main"
                "let duplicateReturn ="
                "    handle Ask comp with"
                "        case return x -> pure x"
                "        case return y -> pure y"
                "let missingReturn ="
                "    handle Ask comp with"
                "        case ask _ k -> k ()"
                "let badReturnArity ="
                "    handle Ask comp with"
                "        case return x y -> pure x"
            ]
            |> String.concat "\n"

        let _, lexed, parsed =
            lexAndParse
                "main.kp"
                sourceText

        Assert.Empty(lexed.Diagnostics)

        let duplicateDiagnostic =
            parsed.Diagnostics
            |> List.find (fun diagnostic ->
                tryFindPayloadText "reason" diagnostic = Some "duplicate-handler-return-clause")

        let missingDiagnostic =
            parsed.Diagnostics
            |> List.find (fun diagnostic ->
                tryFindPayloadText "reason" diagnostic = Some "missing-handler-return-clause")

        let arityDiagnostic =
            parsed.Diagnostics
            |> List.find (fun diagnostic ->
                tryFindPayloadText "reason" diagnostic = Some "handler-return-clause-arity-mismatch"
                && tryFindPayloadText "argument-count" diagnostic = Some "2")

        Assert.Equal("core-expression-parsing", duplicateDiagnostic.Payload.Kind)
        Assert.Equal("core-expression-parsing", missingDiagnostic.Payload.Kind)
        Assert.Equal("core-expression-parsing", arityDiagnostic.Payload.Kind)
        Assert.Contains(
            arityDiagnostic.Payload.Fields,
            fun field ->
                field.Name = "argument-count"
                && field.Value = DiagnosticPayloadText "2"
        )

    [<Fact>]
    let ``parser reports structured function header diagnostics`` () =
        let sourceText =
            [
                "module main"
                "let badTop @ = 0"
                "let outer ="
                "    block"
                "        let badLocal @ = 0"
                "        badLocal"
            ]
            |> String.concat "\n"

        let _, lexed, parsed =
            lexAndParse
                "main.kp"
                sourceText

        Assert.Empty(lexed.Diagnostics)

        let topDiagnostic =
            parsed.Diagnostics
            |> List.find (fun diagnostic ->
                tryFindPayloadText "reason" diagnostic = Some "expected-implicit-parameter-after-at"
                && tryFindPayloadText "header-context" diagnostic = Some "function-header"
            )

        let localDiagnostic =
            parsed.Diagnostics
            |> List.find (fun diagnostic ->
                tryFindPayloadText "reason" diagnostic = Some "expected-implicit-parameter-after-at"
                && tryFindPayloadText "header-context" diagnostic = Some "local-function-header"
            )

        Assert.Equal("core-expression-parsing", topDiagnostic.Payload.Kind)
        Assert.Equal(Some "expected-implicit-parameter-after-at", tryFindPayloadText "reason" topDiagnostic)
        Assert.Equal(Some "function-header", tryFindPayloadText "header-context" topDiagnostic)
        Assert.Equal("core-expression-parsing", localDiagnostic.Payload.Kind)
        Assert.Equal(Some "expected-implicit-parameter-after-at", tryFindPayloadText "reason" localDiagnostic)
        Assert.Equal(Some "local-function-header", tryFindPayloadText "header-context" localDiagnostic)

    [<Fact>]
    let ``parser reports structured selector and expression tail diagnostics`` () =
        let sourceText =
            [
                "module main"
                "let badEffect = effect-label"
                "let badSafe = value?."
                "let badIs = value is"
                "let badChain = value is Foo is Bar"
                "let badTail = value )"
            ]
            |> String.concat "\n"

        let _, lexed, parsed =
            lexAndParse
                "main.kp"
                sourceText

        Assert.Empty(lexed.Diagnostics)

        let effectDiagnostic =
            parsed.Diagnostics
            |> List.find (fun diagnostic -> tryFindPayloadText "reason" diagnostic = Some "expected-name-after-effect-label")

        let safeDiagnostic =
            parsed.Diagnostics
            |> List.find (fun diagnostic -> tryFindPayloadText "reason" diagnostic = Some "expected-safe-navigation-member-access")

        let constructorDiagnostic =
            parsed.Diagnostics
            |> List.find (fun diagnostic -> tryFindPayloadText "reason" diagnostic = Some "expected-constructor-name-after-is")

        let chainedDiagnostic =
            parsed.Diagnostics
            |> List.find (fun diagnostic -> tryFindPayloadText "reason" diagnostic = Some "constructor-tag-tests-cannot-be-chained")

        let trailingDiagnostic =
            parsed.Diagnostics
            |> List.find (fun diagnostic -> tryFindPayloadText "reason" diagnostic = Some "unexpected-trailing-expression-tokens")

        Assert.Equal("core-expression-parsing", effectDiagnostic.Payload.Kind)
        Assert.Equal("core-expression-parsing", safeDiagnostic.Payload.Kind)
        Assert.Equal("core-expression-parsing", constructorDiagnostic.Payload.Kind)
        Assert.Equal("core-expression-parsing", chainedDiagnostic.Payload.Kind)
        Assert.Equal("core-expression-parsing", trailingDiagnostic.Payload.Kind)

    [<Fact>]
    let ``parser reports structured named application and seal diagnostics`` () =
        let sourceText =
            [
                "module main"
                "let badNamedLabel = f { 1 = x }"
                "let badNamedForm = f { @x }"
                "let badSealAs = seal value Int"
                "let badSealValue = seal as Int"
            ]
            |> String.concat "\n"

        let _, lexed, parsed =
            lexAndParse
                "main.kp"
                sourceText

        Assert.Empty(lexed.Diagnostics)

        let namedLabelDiagnostic =
            parsed.Diagnostics
            |> List.find (fun diagnostic -> tryFindPayloadText "reason" diagnostic = Some "expected-named-application-field-label")

        let namedFieldDiagnostic =
            parsed.Diagnostics
            |> List.find (fun diagnostic -> tryFindPayloadText "reason" diagnostic = Some "expected-named-application-field")

        let sealAsDiagnostic =
            parsed.Diagnostics
            |> List.find (fun diagnostic -> tryFindPayloadText "reason" diagnostic = Some "expected-seal-as")

        let sealValueDiagnostic =
            parsed.Diagnostics
            |> List.find (fun diagnostic -> tryFindPayloadText "reason" diagnostic = Some "expected-seal-value")

        Assert.Equal("core-expression-parsing", namedLabelDiagnostic.Payload.Kind)
        Assert.Equal("core-expression-parsing", namedFieldDiagnostic.Payload.Kind)
        Assert.Equal("core-expression-parsing", sealAsDiagnostic.Payload.Kind)
        Assert.Equal("core-expression-parsing", sealValueDiagnostic.Payload.Kind)

    [<Fact>]
    let ``parser reports structured record field diagnostics`` () =
        let sourceText =
            [
                "module main"
                "let badRecordLabel = (1 = x)"
                "let badRecordForm = (1 = x, @x)"
            ]
            |> String.concat "\n"

        let _, lexed, parsed =
            lexAndParse
                "main.kp"
                sourceText

        Assert.Empty(lexed.Diagnostics)

        let recordLabelDiagnostic =
            parsed.Diagnostics
            |> List.find (fun diagnostic -> tryFindPayloadText "reason" diagnostic = Some "expected-record-field-label")

        let recordFieldDiagnostic =
            parsed.Diagnostics
            |> List.find (fun diagnostic -> tryFindPayloadText "reason" diagnostic = Some "expected-record-field")

        Assert.Equal("core-expression-parsing", recordLabelDiagnostic.Payload.Kind)
        Assert.Equal("core-expression-parsing", recordFieldDiagnostic.Payload.Kind)

    [<Fact>]
    let ``parser reports structured record patch diagnostics`` () =
        let sourceText =
            [
                "module main"
                "let badTop = rec.{ x.y := value }"
                "let badMissingPath = rec.{ = value }"
                "let badPunned = rec.{ x }"
            ]
            |> String.concat "\n"

        let _, lexed, parsed =
            lexAndParse
                "main.kp"
                sourceText

        Assert.Empty(lexed.Diagnostics)

        let topLevelLabelDiagnostic =
            parsed.Diagnostics
            |> List.find (fun diagnostic ->
                tryFindPayloadText "reason" diagnostic = Some "record-patch-extension-must-be-top-level-label")

        let pathDiagnostic =
            parsed.Diagnostics
            |> List.find (fun diagnostic ->
                tryFindPayloadText "reason" diagnostic = Some "expected-record-patch-path")

        let itemDiagnostic =
            parsed.Diagnostics
            |> List.find (fun diagnostic ->
                tryFindPayloadText "reason" diagnostic = Some "expected-record-patch-item")

        Assert.Equal("core-expression-parsing", topLevelLabelDiagnostic.Payload.Kind)
        Assert.Equal("core-expression-parsing", pathDiagnostic.Payload.Kind)
        Assert.Equal("core-expression-parsing", itemDiagnostic.Payload.Kind)

    [<Fact>]
    let ``parser reports structured projection body diagnostics`` () =
        let sourceText =
            [
                "module main"
                "projection badBody (place this : Int) : Int = nope"
                "projection badMatchBlock (place this : Int) : Int = match this"
                "projection badMatchClause (place this : Int) : Int ="
                "    match this"
                "        nope"
                "projection badMatchArrow (place this : Int) : Int ="
                "    match this"
                "        case _ 0"
            ]
            |> String.concat "\n"

        let _, lexed, parsed =
            lexAndParse
                "main.kp"
                sourceText

        Assert.Empty(lexed.Diagnostics)

        let bodyDiagnostic =
            parsed.Diagnostics
            |> List.find (fun diagnostic -> tryFindPayloadText "reason" diagnostic = Some "expected-projection-body-head")

        let blockDiagnostic =
            parsed.Diagnostics
            |> List.find (fun diagnostic -> tryFindPayloadText "reason" diagnostic = Some "expected-projection-match-case-block")

        let clauseDiagnostic =
            parsed.Diagnostics
            |> List.find (fun diagnostic -> tryFindPayloadText "reason" diagnostic = Some "expected-projection-match-case-clause")

        let arrowDiagnostic =
            parsed.Diagnostics
            |> List.find (fun diagnostic -> tryFindPayloadText "reason" diagnostic = Some "expected-projection-case-clause-arrow")

        Assert.Equal("core-expression-parsing", bodyDiagnostic.Payload.Kind)
        Assert.Equal("core-expression-parsing", blockDiagnostic.Payload.Kind)
        Assert.Equal("core-expression-parsing", clauseDiagnostic.Payload.Kind)
        Assert.Equal("core-expression-parsing", arrowDiagnostic.Payload.Kind)

    [<Fact>]
    let ``parser reports structured projection if-body diagnostics`` () =
        let sourceText =
            [
                "module main"
                "projection badThen (place this : Int) : Int ="
                "    if this"
                "projection badElse (place this : Int) : Int ="
                "    if this then this"
            ]
            |> String.concat "\n"

        let _, lexed, parsed =
            lexAndParse
                "main.kp"
                sourceText

        Assert.Empty(lexed.Diagnostics)

        let thenDiagnostic =
            parsed.Diagnostics
            |> List.find (fun diagnostic ->
                tryFindPayloadText "reason" diagnostic = Some "expected-projection-then")

        let elseDiagnostic =
            parsed.Diagnostics
            |> List.find (fun diagnostic ->
                tryFindPayloadText "reason" diagnostic = Some "expected-projection-else")

        Assert.Equal("core-expression-parsing", thenDiagnostic.Payload.Kind)
        Assert.Equal("core-expression-parsing", elseDiagnostic.Payload.Kind)

    [<Fact>]
    let ``parser reports structured local let and expression recovery diagnostics`` () =
        let sourceText =
            [
                "module main"
                "let missingIn = let x = 1"
                "let missingEquals = let x in missingIn"
                "let missingClose = ("
                "let missingExpr = ,"
            ]
            |> String.concat "\n"

        let _, lexed, parsed =
            lexAndParse
                "main.kp"
                sourceText

        Assert.Empty(lexed.Diagnostics)

        let localLetInDiagnostic =
            parsed.Diagnostics
            |> List.find (fun diagnostic -> tryFindPayloadText "reason" diagnostic = Some "expected-local-let-in")

        let localLetEqualsDiagnostic =
            parsed.Diagnostics
            |> List.find (fun diagnostic -> tryFindPayloadText "reason" diagnostic = Some "expected-local-let-equals")

        let expressionCloseParenthesisDiagnostic =
            parsed.Diagnostics
            |> List.find (fun diagnostic -> tryFindPayloadText "reason" diagnostic = Some "expected-expression-close-parenthesis")

        let expressionDiagnostic =
            parsed.Diagnostics
            |> List.find (fun diagnostic -> tryFindPayloadText "reason" diagnostic = Some "expected-expression")

        Assert.Equal("core-expression-parsing", localLetInDiagnostic.Payload.Kind)
        Assert.Equal("core-expression-parsing", localLetEqualsDiagnostic.Payload.Kind)
        Assert.Equal("core-expression-parsing", expressionCloseParenthesisDiagnostic.Payload.Kind)
        Assert.Equal("core-expression-parsing", expressionDiagnostic.Payload.Kind)

    [<Fact>]
    let ``parser reports structured interpolated string recovery diagnostics`` () =
        let sourceText =
            [
                "module main"
                "let bad = f\"${x\""
            ]
            |> String.concat "\n"

        let _, lexed, parsed =
            lexAndParse
                "main.kp"
                sourceText

        Assert.NotEmpty(lexed.Diagnostics)

        let interpolationEndDiagnostic =
            parsed.Diagnostics
            |> List.find (fun diagnostic ->
                tryFindPayloadText "reason" diagnostic = Some "expected-interpolation-end-before-string-resumes")

        let unterminatedDiagnostic =
            parsed.Diagnostics
            |> List.find (fun diagnostic ->
                tryFindPayloadText "reason" diagnostic = Some "unterminated-interpolated-string")

        Assert.Equal("core-expression-parsing", interpolationEndDiagnostic.Payload.Kind)
        Assert.Equal("core-expression-parsing", unterminatedDiagnostic.Payload.Kind)

    [<Fact>]
    let ``parser reports structured unexpected interpolated string content diagnostics`` () =
        let source = createSource "__interpolated_string_content__.kp" "f\"oops"
        let diagnostics = DiagnosticBag()
        let startText = SyntaxFacts.encodePrefixedStringStart "f" 0

        let tokens =
            [
                { Kind = InterpolatedStringStart
                  Text = startText
                  Span = { Start = 0; Length = 2 } }
                { Kind = Identifier
                  Text = "oops"
                  Span = { Start = 2; Length = 4 } }
                { Kind = EndOfFile
                  Text = ""
                  Span = { Start = 6; Length = 0 } }
            ]

        let _ = CoreParsing.parseExpression FixityTable.empty source diagnostics tokens

        let diagnostic =
            diagnostics.Items
            |> List.find (fun item ->
                tryFindPayloadText "reason" item = Some "expected-interpolated-string-content")

        Assert.Equal("core-expression-parsing", diagnostic.Payload.Kind)


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
                "        scoped effect Ask (a : Type) ="
                "            ask : Unit -> a"
                "        let comp : Eff <[Ask : Ask Bool]> Bool ="
                "            do"
                "                let b <- Ask.ask ()"
                "                b"
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
            Assert.Equal<string list>([ "("; "a"; ":"; "Type"; ")" ], localEffect.HeaderTokens |> List.map (fun token -> token.Text))
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
    let ``source compilation rejects clarify imports in package mode`` () =
        let workspace =
            compileInMemoryWorkspace
                "memory-import-clarify-package-mode-root"
                [
                    "lib.kp",
                    [
                        "module lib"
                        "opaque type Id a = a"
                    ]
                    |> String.concat "\n"
                    "main.kp",
                    [
                        "module main"
                        "import lib.(clarify type Id)"
                        "coerce : Id Int -> Int"
                        "let coerce x : Int = x"
                    ]
                    |> String.concat "\n"
                ]

        Assert.True(workspace.HasErrors, "Expected package mode to reject clarify imports.")
        Assert.Contains(
            workspace.Diagnostics,
            fun diagnostic ->
                diagnostic.Code = DiagnosticCode.ImportClarifyRequiresBuildSetting
                && diagnostic.Message.Contains("allow_clarify", StringComparison.Ordinal)
        )


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
        Assert.NotEmpty(parsed.Diagnostics)
        Assert.All(parsed.Diagnostics, fun diagnostic -> Assert.Equal(DiagnosticCode.ExpectedSyntaxToken, diagnostic.Code))
        Assert.Contains(parsed.Diagnostics, fun diagnostic -> diagnostic.Message.Contains("Expected ')'"))


    [<Fact>]
    let ``script mode rejects URL imports explicitly without package mode reproducibility diagnostics`` () =
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
        Assert.Equal(2, codes |> List.filter ((=) DiagnosticCode.UrlImportUnsupported) |> List.length)
        Assert.DoesNotContain(DiagnosticCode.ModuleNameUnresolved, codes)


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
        Assert.Equal<DiagnosticCode list>([ DiagnosticCode.ExpectedSyntaxToken ], parsed.Diagnostics |> List.map (fun diagnostic -> diagnostic.Code))


    [<Fact>]
    let ``imports see declarations from all fragments of a module`` () =
        let mainSource =
            [
                "module main"
                "x : Int"
                "let x = 1"
            ]
            |> String.concat "\n"

        let extraSource =
            [
                "module main"
                "y : Int"
                "let y = x"
            ]
            |> String.concat "\n"

        let consumerSource =
            [
                "module consumer"
                "import main.*"
                "result : Int"
                "let result = x + y"
            ]
            |> String.concat "\n"

        let workspace, result =
            evaluateInMemoryBinding
                "memory-module-fragment-import-root"
                "consumer.result"
                [
                    "main.kp", mainSource
                    "main.extra.kp", extraSource
                    "consumer.kp", consumerSource
                ]

        Assert.False(workspace.HasErrors, sprintf "Expected imports to see all fragment declarations, got %A" workspace.Diagnostics)

        match result with
        | Result.Ok value ->
            Assert.Equal("2", RuntimeValue.format value)
        | Result.Error issue ->
            failwithf "Expected imported fragment binding to evaluate, got %A" issue


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
    let ``parser accepts wildcard do bind patterns`` () =
        let parsed, diagnostics =
            parseExpressionWithFixities
                (Parser.bootstrapFixities ())
                "__do_wildcard_bind__.kp"
                "do\n    _ <- action\n    unit"

        Assert.Empty(diagnostics)

        match parsed with
        | Some(Do [ DoBind(binding, Name [ "action" ]); DoExpression(Name [ "unit" ]) ]) ->
            Assert.Equal(WildcardPattern, binding.Pattern)
        | other ->
            failwithf "Unexpected wildcard do-bind parse shape: %A" other


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
    let ``frontend rejects inout declarations whose result does not thread the named field`` () =
        let workspace =
            compileInMemoryWorkspace
                "memory-inout-threaded-field-root"
                [
                    "main.kp",
                    [
                        "module main"
                        "type Cell = (1 value : Int)"
                        "badStep : (inout c : Cell) -> Cell"
                        "let badStep c = c"
                    ]
                    |> String.concat "\n"
                ]

        Assert.True(workspace.HasErrors, "Expected invalid inout declaration to be rejected in frontend validation.")
        Assert.Contains(
            workspace.Diagnostics,
            fun diagnostic -> diagnostic.Code = DiagnosticCode.QttInoutThreadedFieldMissing
        )


    [<Fact>]
    let ``KCore preserves qualified Res constructor applications from map sugar`` () =
        let rec containsQualifiedResConstructor expression =
            match expression with
            | KCoreAppSpine(
                KCoreName [ "Res"; ":&" ],
                [ { Expression = KCoreLiteral(LiteralValue.Integer 1L) }
                  { Expression = KCoreLiteral(LiteralValue.Integer 2L) } ]
              ) -> true
            | KCoreAppSpine(callee, arguments) ->
                containsQualifiedResConstructor callee
                || (arguments |> List.exists (fun argument -> containsQualifiedResConstructor argument.Expression))
            | KCoreLet(_, value, body)
            | KCoreSequence(value, body) ->
                containsQualifiedResConstructor value || containsQualifiedResConstructor body
            | KCoreDoScope(_, body)
            | KCoreExecute body
            | KCoreLambda(_, body)
            | KCoreUnary(_, body) ->
                containsQualifiedResConstructor body
            | KCoreBinary(left, _, right) ->
                containsQualifiedResConstructor left || containsQualifiedResConstructor right
            | KCoreIfThenElse(condition, whenTrue, whenFalse) ->
                containsQualifiedResConstructor condition
                || containsQualifiedResConstructor whenTrue
                || containsQualifiedResConstructor whenFalse
            | KCoreWhile(condition, body) ->
                containsQualifiedResConstructor condition || containsQualifiedResConstructor body
            | KCoreMatch(scrutinee, cases) ->
                containsQualifiedResConstructor scrutinee
                || (cases
                    |> List.exists (fun caseClause ->
                        caseClause.Guard |> Option.exists containsQualifiedResConstructor
                        || containsQualifiedResConstructor caseClause.Body))
            | KCoreHandle(_, label, body, returnClause, operationClauses) ->
                containsQualifiedResConstructor label
                || containsQualifiedResConstructor body
                || containsQualifiedResConstructor returnClause.Body
                || (operationClauses |> List.exists (fun clause -> containsQualifiedResConstructor clause.Body))
            | KCoreTraitCall(_, _, dictionary, arguments) ->
                containsQualifiedResConstructor dictionary || (arguments |> List.exists containsQualifiedResConstructor)
            | KCoreScheduleExit(_, KCoreDeferred deferred, body) ->
                containsQualifiedResConstructor deferred || containsQualifiedResConstructor body
            | KCoreScheduleExit(_, KCoreRelease(_, release, resource), body) ->
                containsQualifiedResConstructor release
                || containsQualifiedResConstructor resource
                || containsQualifiedResConstructor body
            | KCorePrefixedString(_, parts) ->
                parts
                |> List.exists (function
                    | KCoreStringText _ -> false
                    | KCoreStringInterpolation inner -> containsQualifiedResConstructor inner)
            | KCoreSyntaxQuote inner
            | KCoreSyntaxSplice inner
            | KCoreTopLevelSyntaxSplice inner
            | KCoreCodeQuote inner
            | KCoreCodeSplice inner ->
                containsQualifiedResConstructor inner
            | KCoreLiteral _
            | KCoreName _
            | KCoreStaticObject _
            | KCoreEffectLabel _
            | KCoreEffectOperation _
            | KCoreDictionaryValue _ ->
                false

        let workspace =
            compileInMemoryWorkspace
                "memory-kcore-qualified-res-root"
                [
                    "main.kp",
                    [
                        "module main"
                        "let result = { yield 1 : 2 }"
                    ]
                    |> String.concat "\n"
                ]

        Assert.False(workspace.HasErrors, $"Expected map sugar compilation to succeed, got {workspace.Diagnostics}.")

        let resultBinding =
            workspace.KCore
            |> List.find (fun moduleDump -> moduleDump.Name = "main")
            |> fun moduleDump ->
                moduleDump.Declarations
                |> List.pick (fun declaration ->
                    match declaration.Binding with
                    | Some binding when binding.Name = Some "result" -> Some binding
                    | _ -> None)

        match resultBinding.Body with
        | Some body when containsQualifiedResConstructor body -> ()
        | other ->
            failwithf "Expected KCore map sugar lowering to preserve Res.(:&), got %A" other


    [<Fact>]
    let ``top level macro helpers honor leading forall binders and expand tuple syntax`` () =
        let workspace =
            compileInMemoryWorkspace
                "memory-top-level-macro-forall-root"
                [
                    "main.kp",
                    [
                        "module main"
                        "import support.helper.(dupSyntax)"
                        "pair : (Int, Int)"
                        "let pair ="
                        "    $(dupSyntax @Int '{ 42 })"
                    ]
                    |> String.concat "\n"
                    "support/helper.kp",
                    [
                        "module support.helper"
                        "dupSyntax : forall (@0 a : Type). Syntax a -> Elab (Syntax (a, a))"
                        "let dupSyntax s ="
                        "    pure '{ (${s}, ${s}) }"
                    ]
                    |> String.concat "\n"
                ]

        Assert.False(workspace.HasErrors, $"Expected macro helper expansion to succeed, got:{Environment.NewLine}{diagnosticsSummary workspace.Diagnostics}")

        let mainDocument =
            tryFindDocument "main" workspace
            |> Option.defaultWith (fun () -> failwith "Expected main document.")

        let pairDeclaration =
            tryFindLetDeclaration "pair" mainDocument
            |> Option.defaultWith (fun () -> failwith "Expected pair declaration.")

        match pairDeclaration.Body with
        | Some(RecordLiteral [ { Name = "_1"; Value = NumericLiteral(SurfaceIntegerLiteral(left, "42", None)) }
                               { Name = "_2"; Value = NumericLiteral(SurfaceIntegerLiteral(right, "42", None)) } ]) ->
            Assert.Equal(BigInteger(42), left)
            Assert.Equal(BigInteger(42), right)
        | other ->
            failwithf "Expected expanded tuple syntax body, got %A" other


    [<Fact>]
    let ``macro do blocks accept a final if expression after a bind`` () =
        let workspace =
            compileInMemoryWorkspace
                "memory-macro-do-final-if-root"
                [
                    "main.kp",
                    [
                        "module main"
                        "import support.helper.(smoke)"
                        "data User : Type ="
                        "    User (name : String) (age : Int)"
                        "result : Unit"
                        "let result ="
                        "    $(smoke @User '{ User })"
                    ]
                    |> String.concat "\n"
                    "support/helper.kp",
                    [
                        "module support.helper"
                        "import std.deriving.shape.*"
                        "smoke : forall (@0 a : Type). Syntax Type -> Elab (Syntax Unit)"
                        "let smoke @a target = do"
                        "    shape <- inspectAdt @a target"
                        "    if True then"
                        "        unitSyntax"
                        "    else"
                        "        failElabWith \"KAPPA_TEST_UNREACHABLE\" \"unreachable\" []"
                    ]
                    |> String.concat "\n"
                ]

        Assert.False(workspace.HasErrors, $"Expected macro do-block final if-expression to elaborate, got:{Environment.NewLine}{diagnosticsSummary workspace.Diagnostics}")


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


module SmokeTestsShard5 =

    [<Fact>]
    let ``frontend assigns effect identities before elaboration and checking`` () =
        let workspace =
            compileInMemoryWorkspace
                "memory-effect-identities-root"
                [
                    "main.kp",
                    [
                        "@PrivateByDefault module main"
                        "effect Top ="
                        "    ping : Unit -> Unit"
                        ""
                        "result : Unit"
                        "let result ="
                        "    block"
                        "        scoped effect Local ="
                        "            pong : Unit -> Unit"
                        "        ()"
                    ]
                    |> String.concat "\n"
                ]

        Assert.False(workspace.HasErrors, diagnosticsSummary workspace.Diagnostics)

        let document =
            workspace
            |> tryFindDocument "main"
            |> Option.defaultWith (fun () -> failwith "Expected main document.")

        let topLevelEffect =
            document.Syntax.Declarations
            |> List.pick (function
                | EffectDeclarationNode declaration -> Some declaration
                | _ -> None)

        Assert.True(topLevelEffect.EffectInterfaceId.IsSome)
        Assert.True(topLevelEffect.EffectLabelId.IsSome)
        Assert.True(topLevelEffect.Operations |> List.forall (fun operation -> operation.OperationId.IsSome))

        let resultDeclaration =
            document
            |> tryFindLetDeclaration "result"
            |> Option.defaultWith (fun () -> failwith "Expected result declaration.")

        let localEffect =
            resultDeclaration.Body
            |> Option.bind tryFindLocalScopedEffect
            |> Option.defaultWith (fun () -> failwith "Expected local scoped effect.")

        Assert.True(localEffect.EffectInterfaceId.IsSome)
        Assert.True(localEffect.EffectLabelId.IsSome)
        Assert.True(localEffect.Operations |> List.forall (fun operation -> operation.OperationId.IsSome))


    [<Fact>]
    let ``frontend rejects byte literals whose payload is not exactly one byte under spec rules`` () =
        let workspace =
            compileInMemoryWorkspaceWithPackageMode
                "memory-invalid-byte-literal-root"
                true
                [
                    "main.kp",
                    [
                        "let raw = b'ÿ'"
                        "let escaped = b'\\u{00FF}'"
                        "let escaped2 = b'\\u{0080}'"
                    ]
                    |> String.concat "\n"
                ]

        let invalidByteDiagnostics =
            workspace.Diagnostics
            |> List.filter (fun diagnostic -> diagnostic.Code = DiagnosticCode.UnicodeInvalidByteLiteral)

        Assert.True(workspace.HasErrors, "Expected invalid byte literals to produce diagnostics.")
        Assert.Equal(3, invalidByteDiagnostics.Length)


    [<Fact>]
    let ``parser captures borrowed effect resumption quantities without reclassifying them`` () =
        let sourceText =
            [
                "module demo.effects"
                "effect Ask ="
                "    & ask : Unit -> Bool"
                "    &[r] reply : Bool -> Unit"
            ]
            |> String.concat "\n"

        let _, lexed, parsed =
            lexAndParse
                "demo/effects.kp"
                sourceText

        Assert.Empty(lexed.Diagnostics)
        Assert.Empty(parsed.Diagnostics)

        match parsed.Syntax.Declarations with
        | [ EffectDeclarationNode effectDeclaration ] ->
            Assert.Equal("Ask", effectDeclaration.Name)
            Assert.Equal<string list>([ "ask"; "reply" ], effectDeclaration.Operations |> List.map (fun operation -> operation.Name))
            Assert.Equal(Some(QuantityBorrow None), effectDeclaration.Operations[0].ResumptionQuantity)
            Assert.Equal(Some(QuantityBorrow(Some "r")), effectDeclaration.Operations[1].ResumptionQuantity)
        | other ->
            failwithf "Unexpected declarations for borrowed resumption quantity test: %A" other


    [<Fact>]
    let ``source compilation rejects re exporting items imported via unhide or clarify`` () =
        let workspace =
            compileInMemoryWorkspaceWithPackageMode
                "memory-reexport-unhide-clarify-root"
                false
                [
                    "lib.kp",
                    [
                        "@PrivateByDefault module lib"
                        "opaque type Id a = a"
                        "secret : Int"
                        "let secret = 42"
                    ]
                    |> String.concat "\n"
                    "main.kp",
                    [
                        "module main"
                        "export lib.(unhide term secret, clarify type Id)"
                    ]
                    |> String.concat "\n"
                ]

        Assert.True(workspace.HasErrors, "Expected re-export of unhide/clarify imports to be rejected.")
        Assert.Contains(
            workspace.Diagnostics,
            fun diagnostic ->
                diagnostic.Code = DiagnosticCode.ImportItemModifierReexportForbidden
                && diagnostic.Message.Contains("must not be re-exported", StringComparison.Ordinal)
        )


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
        Assert.Equal<DiagnosticCode list>([ DiagnosticCode.ExpectedSyntaxToken ], parsed.Diagnostics |> List.map (fun diagnostic -> diagnostic.Code))
        Assert.Contains(parsed.Diagnostics, fun diagnostic -> diagnostic.Message.Contains("valid signature type"))


    [<Fact>]
    let ``frontend rejects unknown module attributes`` () =
        let workspace =
            compileInMemoryWorkspace
                "memory-unknown-module-attribute-root"
                [
                    "main.kp",
                    String.concat
                        "\n"
                        [
                            "@NotAKappaModuleAttribute"
                            "module main"
                            "result : Int"
                            "let result = 0"
                        ]
                ]

        Assert.True(workspace.HasErrors, "Expected unknown module attributes to be rejected.")
        Assert.Contains(
            workspace.Diagnostics,
            fun diagnostic ->
                diagnostic.Code = DiagnosticCode.ModuleAttributeUnknown
                && diagnostic.Message.Contains("@NotAKappaModuleAttribute", StringComparison.Ordinal)
        )


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
    let ``compilation accepts ordinary bindings with leading erased type binders`` () =
        let sourceText =
            [
                "module main"
                ""
                "id : forall (a : Type). a -> a"
                "let id (@0 a : Type) x = x"
                ""
                "value : Int"
                "let value = id 1"
            ]
            |> String.concat "\n"

        let workspace =
            compileInMemoryWorkspace
                "memory-leading-erased-type-binder"
                [ "main.kp", sourceText ]

        Assert.False(workspace.HasErrors, sprintf "Expected no diagnostics, got %A" workspace.Diagnostics)


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
    let ``compilation reports import cycles across module fragments`` () =
        let moduleABaseSource =
            [
                "module a"
                "let a = 1"
            ]
            |> String.concat "\n"

        let moduleAFragmentSource =
            [
                "module a"
                "import b"
                "let fromB = b"
            ]
            |> String.concat "\n"

        let moduleBSource =
            [
                "module b"
                "import a"
                "let b = a"
            ]
            |> String.concat "\n"

        let workspace =
            compileInMemoryWorkspace
                "memory-fragment-cycle-root"
                [
                    "a.kp", moduleABaseSource
                    "a.extra.kp", moduleAFragmentSource
                    "b.kp", moduleBSource
                ]

        Assert.True(workspace.HasErrors, "Expected a fragment-spanning cycle diagnostic.")

        let cycleDiagnostic =
            workspace.Diagnostics
            |> List.tryFind (fun diagnostic -> diagnostic.Code = DiagnosticCode.ImportCycle)

        Assert.True(cycleDiagnostic.IsSome, sprintf "Expected an import-cycle diagnostic from fragment imports, got %A" workspace.Diagnostics)


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
    let ``frontend rejects missing tilde at resolved inout call sites before generic body mismatch`` () =
        let workspace =
            compileInMemoryWorkspace
                "memory-inout-missing-tilde-root"
                [
                    "main.kp",
                    [
                        "module main"
                        "data File : Type ="
                        "    Handle Int"
                        "let step (inout file : File) : IO File = pure file"
                        "let main : IO Unit = do"
                        "    let 1 file = Handle 1"
                        "    step file"
                    ]
                    |> String.concat "\n"
                ]

        Assert.True(workspace.HasErrors, "Expected missing '~' at an inout call site to be rejected in frontend validation.")
        Assert.Contains(
            workspace.Diagnostics,
            fun diagnostic -> diagnostic.Code = DiagnosticCode.QttInoutMarkerRequired
        )
        Assert.Contains(
            workspace.Diagnostics,
            fun diagnostic -> diagnostic.Code = DiagnosticCode.QttInoutThreadedFieldMissing
        )


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
    let ``resource checking aligns later runtime parameters after leading forall binders`` () =
        let workspace =
            compileInMemoryWorkspace
                "memory-resource-leading-forall-alignment-root"
                [
                    "main.kp",
                    [
                        "module main"
                        "sameStringList : List String -> List String -> Bool"
                        "let sameStringList xs ys = True"
                        "helper : forall (@0 a : Type). String -> List String -> List (List String) -> Elab (Syntax Unit)"
                        "let helper @a label expectedConstructors expectedFields ="
                        "    if sameStringList expectedConstructors expectedConstructors then"
                        "        pure '{ () }"
                        "    else"
                        "        pure '{ () }"
                    ]
                    |> String.concat "\n"
                ]

        Assert.False(
            workspace.HasErrors,
            $"Expected later runtime parameters to retain their declared types after leading forall binders, got:{Environment.NewLine}{diagnosticsSummary workspace.Diagnostics}"
        )


    [<Fact>]
    let ``macro evaluator do blocks return their final expression value`` () =
        let workspace =
            compileInMemoryWorkspace
                "memory-macro-do-final-expression-root"
                [
                    "main.kp",
                    [
                        "module main"
                        "import support.helper.(smoke)"
                        "data User : Type ="
                        "    User (name : String) (age : Int)"
                        "result : Unit"
                        "let result ="
                        "    $(smoke @User '{ User })"
                    ]
                    |> String.concat "\n"
                    "support/helper.kp",
                    [
                        "module support.helper"
                        "import std.deriving.shape.*"
                        "smoke : forall (@0 a : Type). Syntax Type -> Elab (Syntax Unit)"
                        "let smoke @a target = do"
                        "    shape <- inspectAdt @a target"
                        "    unitSyntax"
                    ]
                    |> String.concat "\n"
                ]

        Assert.False(workspace.HasErrors, $"Expected do-block macro helper expansion to succeed, got:{Environment.NewLine}{diagnosticsSummary workspace.Diagnostics}")

        let mainDocument =
            tryFindDocument "main" workspace
            |> Option.defaultWith (fun () -> failwith "Expected main document.")

        let resultDeclaration =
            tryFindLetDeclaration "result" mainDocument
            |> Option.defaultWith (fun () -> failwith "Expected result declaration.")

        match resultDeclaration.Body with
        | Some(Literal Unit) -> ()
        | other ->
            failwithf "Expected expanded unit syntax body, got %A" other


    [<Fact>]
    let ``bundled prelude defines show and compare as trait members not standalone expect terms`` () =
        let preludeSource =
            createSource Stdlib.BundledPreludeVirtualPath (Stdlib.loadBundledPreludeText ())

        let preludeLexed = Lexer.tokenize preludeSource
        let preludeParsed = Parser.parse preludeSource preludeLexed.Tokens

        Assert.Empty(preludeLexed.Diagnostics)
        Assert.Empty(preludeParsed.Diagnostics)

        let expectTerms =
            preludeParsed.Syntax.Declarations
            |> List.choose (function
                | ExpectDeclarationNode (ExpectTermDeclaration declaration) -> Some declaration.Name
                | _ -> None)
            |> Set.ofList

        Assert.DoesNotContain("show", expectTerms)
        Assert.DoesNotContain("compare", expectTerms)

        let traitMembers =
            preludeParsed.Syntax.Declarations
            |> List.choose (function
                | TraitDeclarationNode declaration ->
                    Some(
                        declaration.Name,
                        declaration.Members
                        |> List.choose (fun memberDeclaration -> memberDeclaration.Name)
                        |> Set.ofList
                    )
                | _ -> None)
            |> Map.ofList

        Assert.Contains("show", traitMembers["Show"])
        Assert.Contains("compare", traitMembers["Ord"])


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


module SmokeTestsShard6 =

    [<Fact>]
    let ``path derived module inference is ascii only validates fragments and requires exact kp suffix`` () =
        let root = Path.GetFullPath(Path.Combine(Path.GetTempPath(), "kappa-module-path-inference-root"))

        let rooted (relativePath: string) =
            Path.Combine(root, relativePath.Replace('/', Path.DirectorySeparatorChar))

        let fileSystem =
            HarnessWorkspace.InMemoryFileSystem(
                [
                    rooted "main.win32.kp", ""
                    rooted "main.bad-frag.kp", ""
                    rooted "Δelta.kp", ""
                    rooted "main.KP", ""
                ]
            )
            :> IFileSystem

        Assert.Equal<string list option>(Some [ "main" ], Compilation.tryInferModuleName fileSystem root (rooted "main.win32.kp"))
        Assert.Equal<string list option>(None, Compilation.tryInferModuleName fileSystem root (rooted "main.bad-frag.kp"))
        Assert.Equal<string list option>(None, Compilation.tryInferModuleName fileSystem root (rooted "Δelta.kp"))
        Assert.Equal<string list option>(None, Compilation.tryInferModuleName fileSystem root (rooted "main.KP"))


    [<Fact>]
    let ``lexer preserves spec suffixed identifier adjacency for numeric literals`` () =
        let source =
            createSource
                "__suffixed_numeric_literal_adjacency__.kp"
                "123abc 0xFFu32"

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
                IntegerLiteral, "123abc"
                IntegerLiteral, "0xFFu32"
            ],
            actualTokens
        )


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
        Assert.Equal<DiagnosticCode list>([ DiagnosticCode.ExpectedSyntaxToken ], parsed.Diagnostics |> List.map (fun diagnostic -> diagnostic.Code))
        Assert.Contains(parsed.Diagnostics, fun diagnostic -> diagnostic.Message.Contains("ctorAll may not be combined with an alias"))


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
            Assert.Equal("alpha\nbeta\n\n", cooked)
            Assert.Equal("gamma\\n\n  delta\n", rawMulti)
        | other ->
            failwithf "Unexpected parsed declarations for raw/multiline strings: %A" other


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
    let ``multiple import specs from the same module preserve distinct selections`` () =
        let libSource =
            [
                "module lib"
                "alpha : Int"
                "let alpha = 1"
                "beta : Int"
                "let beta = 2"
            ]
            |> String.concat "\n"

        let mainSource =
            [
                "module main"
                "import lib.(alpha), lib.(beta)"
                "result : Int"
                "let result = alpha + beta"
            ]
            |> String.concat "\n"

        let workspace, result =
            evaluateInMemoryBinding
                "memory-duplicate-import-spec-root"
                "main.result"
                [
                    "lib.kp", libSource
                    "main.kp", mainSource
                ]

        Assert.False(workspace.HasErrors, sprintf "Expected both import specs to remain visible, got %A" workspace.Diagnostics)

        match result with
        | Result.Ok value ->
            Assert.Equal("3", RuntimeValue.format value)
        | Result.Error issue ->
            failwithf "Expected both import specs to remain usable, got %A" issue


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
    let ``source compilation rejects active patterns without an explicit scrutinee binder`` () =
        let workspace =
            compileInMemoryWorkspaceWithPackageMode
                "memory-active-pattern-missing-scrutinee-root"
                false
                [
                    "main.kp",
                    [
                        "module main"
                        "pattern Bad : Option Int ="
                        "    Some 1"
                        "bad : Int"
                        "let bad = 0"
                    ]
                    |> String.concat "\n"
                ]

        Assert.True(workspace.HasErrors, "Expected active pattern declarations without an explicit scrutinee binder to be rejected.")
        Assert.Contains(
            workspace.Diagnostics,
            fun diagnostic -> diagnostic.Code = DiagnosticCode.ActivePatternMissingScrutineeBinder
        )


    [<Fact>]
    let ``prefixed strings reject runtime trait evidence parameters as handlers`` () =
        let workspace =
            compileInMemoryWorkspace
                "memory-prefixed-string-runtime-handler-root"
                [
                    "main.kp",
                    [
                        "module main"
                        "value : InterpolatedMacro String -> String"
                        "let value prefix = prefix\"123\""
                    ]
                    |> String.concat "\n"
                ]

        Assert.True(workspace.HasErrors, "Expected runtime trait evidence parameters to be rejected as prefixed-string handlers.")
        Assert.Contains(workspace.Diagnostics, fun diagnostic -> diagnostic.Code = DiagnosticCode.TypeEqualityMismatch && diagnostic.Message.Contains("Elab", StringComparison.Ordinal))


    [<Fact>]
    let ``compile time shape helpers typecheck with leading forall binders and inspectAdt`` () =
        let workspace =
            compileInMemoryWorkspace
                "memory-shape-helper-leading-forall-root"
                [
                    "main.kp",
                    [
                        "module main"
                        "import std.deriving.shape.*"
                        "assertHasAdtRuntimeFieldInstances :"
                        "    forall (tc : Type -> Constraint) (@0 a : Type)."
                        "    Syntax Type -> Elab (Syntax Unit)"
                        "let assertHasAdtRuntimeFieldInstances @tc @a target = do"
                        "    shape <- inspectAdt @a target"
                        "    requireRuntimeFieldInstances @tc shape"
                        "    unitSyntax"
                    ]
                    |> String.concat "\n"
                ]

        Assert.False(
            workspace.HasErrors,
            $"Expected compile-time shape helper to typecheck through inspectAdt and requireRuntimeFieldInstances, got:{Environment.NewLine}{diagnosticsSummary workspace.Diagnostics}"
        )


    [<Fact>]
    let ``macro evaluator resolves dotted local field access through meta bindings`` () =
        let workspace =
            compileInMemoryWorkspace
                "memory-macro-dotted-field-root"
                [
                    "main.kp",
                    [
                        "module main"
                        "import support.helper.(smoke)"
                        "data User : Type ="
                        "    User (name : String) (age : Int)"
                        "result : String"
                        "let result ="
                        "    $(smoke @User '{ User })"
                    ]
                    |> String.concat "\n"
                    "support/helper.kp",
                    [
                        "module support.helper"
                        "import std.deriving.shape.*"
                        "smoke : forall (@0 a : Type). Syntax Type -> Elab (Syntax String)"
                        "let smoke @a target = do"
                        "    shape <- inspectAdt @a target"
                        "    stringSyntax shape.renderName"
                    ]
                    |> String.concat "\n"
                ]

        Assert.False(workspace.HasErrors, $"Expected dotted local field access macro expansion to succeed, got:{Environment.NewLine}{diagnosticsSummary workspace.Diagnostics}")

        let mainDocument =
            tryFindDocument "main" workspace
            |> Option.defaultWith (fun () -> failwith "Expected main document.")

        let resultDeclaration =
            tryFindLetDeclaration "result" mainDocument
            |> Option.defaultWith (fun () -> failwith "Expected result declaration.")

        match resultDeclaration.Body with
        | Some(Literal(String text)) ->
            Assert.Equal("User", text)
        | other ->
            failwithf "Expected expanded string literal body, got %A" other


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

module SmokeTestsShard7 =

    [<Fact>]
    let ``source root discovery ignores files without an exact kp suffix`` () =
        let workspace =
            compileInMemoryWorkspace
                "memory-exact-kp-source-discovery-root"
                [
                    "real.kp", ""
                    "ignored.KP", ""
                ]

        Assert.DoesNotContain(
            workspace.Documents,
            fun document -> document.Source.FilePath.EndsWith("ignored.KP", StringComparison.Ordinal)
        )

        Assert.Contains(workspace.Documents, fun document -> document.ModuleName = Some [ "real" ])


    [<Fact>]
    let ``compilation surfaces malformed prefixed numeric literals at the lexical boundary`` () =
        let workspace =
            compileInMemoryWorkspace
                "memory-malformed-prefixed-numeric-literals"
                [
                    "main.kp",
                    [
                        "module main"
                        ""
                        "hex : Int"
                        "let hex = 0x"
                        ""
                        "bits : Int"
                        "let bits = 0b102"
                        ""
                        "perms : Int"
                        "let perms = 0o89"
                    ]
                    |> String.concat "\n"
                ]

        Assert.True(workspace.HasErrors, "Expected malformed prefixed numeric literals to be rejected.")
        Assert.Contains(workspace.Diagnostics, fun diagnostic -> diagnostic.Code = DiagnosticCode.MalformedNumericLiteral)


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
            [ DiagnosticCode.ExpectedSyntaxToken; DiagnosticCode.ExpectedSyntaxToken ],
            parsed.Diagnostics |> List.map (fun diagnostic -> diagnostic.Code)
        )
        Assert.Contains(parsed.Diagnostics, fun diagnostic -> diagnostic.Message.Contains("Duplicate 'unhide'"))
        Assert.Contains(parsed.Diagnostics, fun diagnostic -> diagnostic.Message.Contains("Duplicate 'clarify'"))


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
        Assert.Equal<DiagnosticCode list>([ DiagnosticCode.ExpectedSyntaxToken ], parsed.Diagnostics |> List.map (fun diagnostic -> diagnostic.Code))
        Assert.Contains(parsed.Diagnostics, fun diagnostic -> diagnostic.Message.Contains("type alias body"))


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
    let ``aliased type imports from modules with identical export shapes remain simultaneously usable`` () =
        let leftSource =
            [
                "module left"
                "data T : Type ="
                "    Mk Int"
            ]
            |> String.concat "\n"

        let rightSource =
            [
                "module right"
                "data T : Type ="
                "    Mk Int"
            ]
            |> String.concat "\n"

        let mainSource =
            [
                "module main"
                "import left, right"
                "import left.(type T as LeftT), right.(type T as RightT)"
                "leftInt : LeftT -> Int"
                "let leftInt value ="
                "    match value"
                "      case left.Mk n -> n"
                "rightInt : RightT -> Int"
                "let rightInt value ="
                "    match value"
                "      case right.Mk n -> n"
                "result : Int"
                "let result = leftInt (left.Mk 1) + rightInt (right.Mk 2)"
            ]
            |> String.concat "\n"

        let workspace, result =
            evaluateInMemoryBinding
                "memory-identical-import-shapes-root"
                "main.result"
                [
                    "left.kp", leftSource
                    "right.kp", rightSource
                    "main.kp", mainSource
                ]

        Assert.False(workspace.HasErrors, sprintf "Expected aliased type imports from distinct modules to stay visible, got %A" workspace.Diagnostics)

        match result with
        | Result.Ok value ->
            Assert.Equal("3", RuntimeValue.format value)
        | Result.Error issue ->
            failwithf "Expected aliased imports from same-shaped modules to remain usable together, got %A" issue


    [<Fact>]
    let ``type signatures reject unterminated effect rows without hanging`` () =
        let source = createSource "__unterminated_effect_row_signature__.kp" "I9 <[I5] ["
        let lexed = Lexer.tokenize source

        Assert.Empty(lexed.Diagnostics)
        Assert.True(TypeSignatures.parseScheme lexed.Tokens |> Option.isNone)


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
    let ``type signatures treat lawful record reorderings as definitionally equal`` () =
        let left = parseTypeText "(y : Int, x : Int)"
        let right = parseTypeText "(x : Int, y : Int)"

        Assert.True(TypeSignatures.definitionallyEqual left right)


    [<Fact>]
    let ``raw type signature normalization does not treat Float as a builtin alias of Double`` () =
        let floatType = TypeSignatures.TypeName([ "Float" ], [])
        let doubleType = TypeSignatures.TypeName([ "Double" ], [])
        let qualifiedFloatType = TypeSignatures.TypeName([ "std"; "prelude"; "Float" ], [])
        let qualifiedDoubleType = TypeSignatures.TypeName([ "std"; "prelude"; "Double" ], [])

        Assert.False(TypeSignatures.definitionallyEqual floatType doubleType)
        Assert.False(TypeSignatures.definitionallyEqual qualifiedFloatType qualifiedDoubleType)
        Assert.Equal("Float", TypeSignatures.toText floatType)
        Assert.Equal("Float", TypeSignatures.toText qualifiedFloatType)


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

        Assert.True(intrinsicExpect.IsNone, "Expected bundled std.prelude to define 'not' ordinarily, not as an intrinsic expect.")


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
    let ``compile time shape helpers reject missing runtime field trait instances`` () =
        let workspace =
            compileInMemoryWorkspace
                "memory-shape-helper-missing-runtime-field-instance-root"
                [
                    "main.kp",
                    [
                        "module main"
                        "import std.deriving.shape.*"
                        "assertHasAdtRuntimeFieldInstances :"
                        "    forall (tc : Type -> Constraint) (@0 a : Type)."
                        "    Syntax Type -> Elab (Syntax Unit)"
                        "let assertHasAdtRuntimeFieldInstances @tc @a target = do"
                        "    shape <- inspectAdt @a target"
                        "    requireRuntimeFieldInstances @tc shape"
                        "    unitSyntax"
                        "trait MiniShow (a : Type) ="
                        "    miniShow : (& value : a) -> String"
                        "data Mystery : Type ="
                        "    Mystery"
                        "data NeedsMystery : Type ="
                        "    NeedsMystery (value : Mystery)"
                        "probe : Unit"
                        "let probe ="
                        "    $(assertHasAdtRuntimeFieldInstances @MiniShow @NeedsMystery '{ NeedsMystery })"
                    ]
                    |> String.concat "\n"
                ]

        Assert.True(workspace.HasErrors, "Expected missing runtime field trait instances to be rejected during elaboration.")
        Assert.Contains(workspace.Diagnostics, fun diagnostic -> diagnostic.Code = DiagnosticCode.DerivingShapeMissingRuntimeFieldInstance)


    [<Fact>]
    let ``backend profile parser accepts portable names and legacy aliases`` () =
        Assert.Equal(BackendProfile.Interpreter, BackendProfile.normalizeConfigured "")
        Assert.Equal(BackendProfile.Interpreter, BackendProfile.normalizeConfigured " interpreter ")
        Assert.Equal(BackendProfile.DotNet, BackendProfile.normalizeConfigured "dotnet")
        Assert.Equal(BackendProfile.DotNet, BackendProfile.normalizeConfigured "dotnet-il")
        Assert.Equal(BackendProfile.Zig, BackendProfile.normalizeConfigured "zig")
        Assert.Equal(BackendProfile.Zig, BackendProfile.normalizeConfigured "zigcc")
        Assert.Equal(BackendProfile.Unknown "custom-backend", BackendProfile.normalizeConfigured "custom-backend")

        Assert.Equal("interpreter", BackendProfile.Interpreter |> BackendProfile.toPortableName)
        Assert.Equal("dotnet", BackendProfile.DotNet |> BackendProfile.toPortableName)
        Assert.Equal("zig", BackendProfile.Zig |> BackendProfile.toPortableName)
        Assert.Equal("custom-backend", BackendProfile.Unknown "custom-backend" |> BackendProfile.toPortableName)


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
                        Some ImportNamespace.Constructor, "LT"
                        Some ImportNamespace.Constructor, "EQ"
                        Some ImportNamespace.Constructor, "GT"
                        Some ImportNamespace.Constructor, "refl"
                    ]

                Assert.Equal<(ImportNamespace option * string) list>(expectedItems, actualItems)
            | other ->
                failwithf "Unexpected implicit prelude constructor import: %A" other
        | other ->
            failwithf "Expected two implicit prelude import specs, got %A" other

