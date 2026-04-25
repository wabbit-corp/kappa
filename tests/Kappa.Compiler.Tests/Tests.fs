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
    | [ LetDeclaration { Body = Some(PrefixedString("sql", [ StringText "select "; StringInterpolation(Name [ "userId" ]) ])) }
        LetDeclaration { Body = Some(PrefixedString("re", [ StringText "\\b"; StringInterpolation(Name [ "word" ]); StringText "\\b" ])) } ] -> ()
    | other ->
        failwithf "Unexpected parsed raw prefixed string: %A" other

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
let ``parser gives built in safe navigation and elvis their spec precedence`` () =
    let source = createSource "__safe_navigation_precedence__.kp" "a?.b ?: fallback"
    let lexed = Lexer.tokenize source
    let parsed = CoreParsing.parseExpression (Parser.bootstrapFixities ()) source (DiagnosticBag()) lexed.Tokens

    match parsed with
    | Some(Elvis(SafeNavigation(Name [ "a" ], { Segments = [ "b" ]; Arguments = [] }), Name [ "fallback" ])) -> ()
    | other -> failwithf "Unexpected expression shape: %A" other

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
              "SyntaxFragment"
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
              "Fiber"
              "FiberId"
              "Exit"
              "Cause"
              "STM"
              "TVar"
              "Thunk"
              "Need"
              "Regex" ]

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
              "not"
              "and"
              "or"
              "negate"
              "println"
              "print"
              "printInt"
              "printString"
              "primitiveIntToString"
              "newRef"
              "readRef"
              "writeRef" ]

    let requiredConstructors =
        Set.ofList
            [ "True"
              "False"
              "None"
              "Some"
              "Ok"
              "Err"
              "Nil"
              "::"
              "LT"
              "EQ"
              "GT"
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
