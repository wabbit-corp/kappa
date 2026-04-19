module CoreTests

open System
open Kappa.Compiler
open Harness
open Xunit

[<Fact>]
let ``parser captures core let parameters return type and expression tree`` () =
    let sourceText =
        [
            "module core.choose"
            "let choose (flag : Bool) (left : Int) (right : Int) : Int = if flag then left else right + 1 * 2"
        ]
        |> String.concat "\n"

    let _, lexed, parsed =
        lexAndParse
            "core/choose.kp"
            sourceText

    Assert.Empty(lexed.Diagnostics)
    Assert.Empty(parsed.Diagnostics)

    match parsed.Syntax.Declarations with
    | [ LetDeclaration definition ] ->
        let parameterNames = definition.Parameters |> List.map (fun parameter -> parameter.Name)
        Assert.Equal<string list>([ "flag"; "left"; "right" ], parameterNames)

        let typeTexts =
            definition.Parameters
            |> List.map (fun parameter ->
                parameter.TypeTokens
                |> Option.defaultValue []
                |> List.map (fun token -> token.Text))

        Assert.Equal<string list list>([ [ "Bool" ]; [ "Int" ]; [ "Int" ] ], typeTexts)

        let returnType =
            definition.ReturnTypeTokens
            |> Option.defaultValue []
            |> List.map (fun token -> token.Text)

        Assert.Equal<string list>([ "Int" ], returnType)

        match definition.Body with
        | Some(
            IfThenElse(
                Name [ "flag" ],
                Name [ "left" ],
                Binary(
                    Name [ "right" ],
                    "+",
                    Binary(
                        Literal(LiteralValue.Integer 1L),
                        "*",
                        Literal(LiteralValue.Integer 2L)
                    )
                )
            )
          ) -> ()
        | other ->
            failwithf "Unexpected core expression: %A" other
    | other ->
        failwithf "Unexpected declarations: %A" other

[<Fact>]
let ``interpreter evaluates imported functions and closures`` () =
    let mathSource =
        [
            "module math"
            "let twice x = x * 2"
        ]
        |> String.concat "\n"

    let mainSource =
        [
            "module main"
            "import math.*"
            "let makeAdder x = \\y -> x + y"
            "let result = makeAdder (twice 20) 2"
        ]
        |> String.concat "\n"

    let workspace, result =
        evaluateInMemoryBinding
            "memory-core-root"
            "main.result"
            [
                "math.kp", mathSource
                "main.kp", mainSource
            ]

    Assert.False(workspace.HasErrors, sprintf "Expected no diagnostics, got %A" workspace.Diagnostics)

    match result with
    | Result.Ok value ->
        Assert.Equal("42", RuntimeValue.format value)
    | Result.Error issue ->
        failwithf "Expected successful evaluation, got %s" issue.Message

[<Fact>]
let ``parser and interpreter support user defined operators via fixity declarations`` () =
    let mainSource =
        [
            "module main"
            "infix left 60 (++)"
            "let (++) x y = x + y"
            "let result = 20 ++ 22"
        ]
        |> String.concat "\n"

    let workspace =
        compileInMemoryWorkspace
            "memory-user-operator-root"
            [ "main.kp", mainSource ]

    Assert.False(workspace.HasErrors, sprintf "Expected no diagnostics, got %A" workspace.Diagnostics)

    let mainDocument =
        workspace.Documents
        |> List.find (fun document -> document.ModuleName = Some [ "main" ])

    match mainDocument.Syntax.Declarations with
    | [ FixityDeclarationNode declaration
        LetDeclaration operatorDefinition
        LetDeclaration resultDefinition ] ->
        Assert.Equal("++", declaration.OperatorName)

        match operatorDefinition.Name with
        | Some "++" -> ()
        | other -> failwithf "Unexpected operator binding name: %A" other

        match resultDefinition.Body with
        | Some(Binary(Literal(LiteralValue.Integer 20L), "++", Literal(LiteralValue.Integer 22L))) -> ()
        | other -> failwithf "Unexpected operator expression: %A" other
    | other ->
        failwithf "Unexpected declarations: %A" other

    match Interpreter.evaluateBinding workspace "main.result" with
    | Result.Ok value ->
        Assert.Equal("42", RuntimeValue.format value)
    | Result.Error issue ->
        failwithf "Expected user-defined operator evaluation to succeed, got %s" issue.Message

[<Fact>]
let ``parser keeps Bool constructors as names rather than literals`` () =
    let sourceText =
        [
            "module core.bool"
            "let result = if True then False else True"
        ]
        |> String.concat "\n"

    let _, lexed, parsed =
        lexAndParse
            "core/bool.kp"
            sourceText

    Assert.Empty(lexed.Diagnostics)
    Assert.Empty(parsed.Diagnostics)

    match parsed.Syntax.Declarations with
    | [ LetDeclaration definition ] ->
        match definition.Body with
        | Some(IfThenElse(Name [ "True" ], Name [ "False" ], Name [ "True" ])) -> ()
        | other ->
            failwithf "Unexpected Bool expression tree: %A" other
    | other ->
        failwithf "Unexpected declarations: %A" other

[<Fact>]
let ``interpreter resolves prelude Bool constructors and functions by implicit import`` () =
    let mainSource =
        [
            "module main"
            "let result = if not False then 42 else 0"
        ]
        |> String.concat "\n"

    let workspace, result =
        evaluateInMemoryBinding
            "memory-bool-constructors-root"
            "main.result"
            [ "main.kp", mainSource ]

    Assert.False(workspace.HasErrors, sprintf "Expected no diagnostics, got %A" workspace.Diagnostics)

    match result with
    | Result.Ok value ->
        Assert.Equal("42", RuntimeValue.format value)
    | Result.Error issue ->
        failwithf "Expected prelude Bool constructor evaluation to succeed, got %s" issue.Message

[<Fact>]
let ``interpreter evaluates the default inequality operator`` () =
    let mainSource =
        [
            "module main"
            "let result = 1 != 2"
        ]
        |> String.concat "\n"

    let workspace, result =
        evaluateInMemoryBinding
            "memory-spec-inequality-root"
            "main.result"
            [ "main.kp", mainSource ]

    Assert.False(workspace.HasErrors, sprintf "Expected no diagnostics, got %A" workspace.Diagnostics)

    match result with
    | Result.Ok value ->
        Assert.Equal("True", RuntimeValue.format value)
    | Result.Error issue ->
        failwithf "Expected '!=' evaluation to succeed, got %s" issue.Message

[<Fact>]
let ``parser captures interpolated strings with nested expressions`` () =
    let sourceText =
        [
            "module core.interpolation"
            "let result = f\"test = ${ \"test\" } and $value\""
        ]
        |> String.concat "\n"

    let _, lexed, parsed =
        lexAndParse
            "core/interpolation.kp"
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
    | [ LetDeclaration definition ] ->
        match definition.Body with
        | Some(
            PrefixedString(
                "f",
                [ StringText "test = "
                  StringInterpolation(Literal(LiteralValue.String "test"))
                  StringText " and "
                  StringInterpolation(Name [ "value" ]) ]
            )
          ) -> ()
        | other ->
            failwithf "Unexpected interpolated string body: %A" other
    | other ->
        failwithf "Unexpected declarations: %A" other

[<Fact>]
let ``interpreter evaluates f strings with nested and simple interpolation`` () =
    let mainSource =
        [
            "module main"
            "let value = \"done\""
            "let result = f\"test = ${ \"test\" } and $value\""
        ]
        |> String.concat "\n"

    let workspace, result =
        evaluateInMemoryBinding
            "memory-interpolation-root"
            "main.result"
            [ "main.kp", mainSource ]

    Assert.False(workspace.HasErrors, sprintf "Expected no diagnostics, got %A" workspace.Diagnostics)

    match result with
    | Result.Ok value ->
        Assert.Equal("\"test = test and done\"", RuntimeValue.format value)
    | Result.Error issue ->
        failwithf "Expected interpolated string evaluation to succeed, got %s" issue.Message

[<Fact>]
let ``interpreter evaluates nested f strings inside interpolation`` () =
    let mainSource =
        [
            "module main"
            "let y = \"yo\""
            "let x = f\"${ f\" $y \" }\""
        ]
        |> String.concat "\n"

    let workspace, result =
        evaluateInMemoryBinding
            "memory-nested-f-string-root"
            "main.x"
            [ "main.kp", mainSource ]

    Assert.False(workspace.HasErrors, sprintf "Expected no diagnostics, got %A" workspace.Diagnostics)

    match result with
    | Result.Ok value ->
        Assert.Equal("\" yo \"", RuntimeValue.format value)
    | Result.Error issue ->
        failwithf "Expected nested f string evaluation to succeed, got %s" issue.Message

[<Fact>]
let ``interpreter short circuits boolean operators`` () =
    let mainSource =
        [
            "module main"
            "let result = if False && ((1 / 0) == 0) then 0 else 42"
        ]
        |> String.concat "\n"

    let workspace, result =
        evaluateInMemoryBinding
            "memory-short-circuit-root"
            "main.result"
            [ "main.kp", mainSource ]

    Assert.False(workspace.HasErrors, sprintf "Expected no diagnostics, got %A" workspace.Diagnostics)

    match result with
    | Result.Ok value ->
        Assert.Equal("42", RuntimeValue.format value)
    | Result.Error issue ->
        failwithf "Expected short-circuit evaluation to succeed, got %s" issue.Message

[<Fact>]
let ``interpreter resolves qualified aliases`` () =
    let mathSource =
        [
            "module util.math"
            "let fortyTwo = 42"
        ]
        |> String.concat "\n"

    let mainSource =
        [
            "module main"
            "import util.math as math"
            "let result = math.fortyTwo"
        ]
        |> String.concat "\n"

    let workspace, result =
        evaluateInMemoryBinding
            "memory-qualified-root"
            "main.result"
            [
                "util/math.kp", mathSource
                "main.kp", mainSource
            ]

    Assert.False(workspace.HasErrors, sprintf "Expected no diagnostics, got %A" workspace.Diagnostics)

    match result with
    | Result.Ok value ->
        Assert.Equal("42", RuntimeValue.format value)
    | Result.Error issue ->
        failwithf "Expected qualified lookup to succeed, got %s" issue.Message

[<Fact>]
let ``interpreter reports ambiguous imported names`` () =
    let moduleASource =
        [
            "module a"
            "let value = 1"
        ]
        |> String.concat "\n"

    let moduleBSource =
        [
            "module b"
            "let value = 2"
        ]
        |> String.concat "\n"

    let mainSource =
        [
            "module main"
            "import a.*"
            "import b.*"
            "let result = value"
        ]
        |> String.concat "\n"

    let workspace, result =
        evaluateInMemoryBinding
            "memory-ambiguous-root"
            "main.result"
            [
                "a.kp", moduleASource
                "b.kp", moduleBSource
                "main.kp", mainSource
            ]

    Assert.False(workspace.HasErrors, sprintf "Expected no diagnostics, got %A" workspace.Diagnostics)

    match result with
    | Result.Ok value ->
        failwithf "Expected an ambiguity error, got %s" (RuntimeValue.format value)
    | Result.Error issue ->
        Assert.Contains("ambiguous", issue.Message)

[<Fact>]
let ``parser captures recursive list matches and do blocks`` () =
    let sourceText =
        [
            "module main"
            "sumList : List Int -> Int"
            "let sumList xs ="
            "    match xs"
            "    case Nil -> 0"
            "    case head :: tail -> head + sumList tail"
            "let main : IO Unit = do"
            "    let nums = 10 :: 20 :: 42 :: Nil"
            "    let total = sumList nums"
            "    printInt total"
        ]
        |> String.concat "\n"

    let _, lexed, parsed =
        lexAndParse
            "main.kp"
            sourceText

    Assert.Empty(lexed.Diagnostics)
    Assert.Empty(parsed.Diagnostics)

    match parsed.Syntax.Declarations with
    | [ SignatureDeclaration sumListSignature
        LetDeclaration sumListDefinition
        LetDeclaration mainDefinition ] ->
        Assert.Equal("sumList", sumListSignature.Name)

        match sumListDefinition.Body with
        | Some(Match(Name [ "xs" ], cases)) ->
            match cases with
            | [ nilCase; consCase ] ->
                match nilCase.Pattern with
                | ConstructorPattern([ "Nil" ], []) -> ()
                | other -> failwithf "Unexpected Nil pattern: %A" other

                match nilCase.Body with
                | Literal(LiteralValue.Integer 0L) -> ()
                | other -> failwithf "Unexpected Nil case body: %A" other

                match consCase.Pattern with
                | ConstructorPattern([ "::" ], [ NamePattern "head"; NamePattern "tail" ]) -> ()
                | other -> failwithf "Unexpected cons pattern: %A" other

                match consCase.Body with
                | Binary(
                    Name [ "head" ],
                    "+",
                    Apply(Name [ "sumList" ], [ Name [ "tail" ] ])
                  ) -> ()
                | other ->
                    failwithf "Unexpected cons case body: %A" other
            | other ->
                failwithf "Unexpected match cases: %A" other
        | other ->
            failwithf "Unexpected sumList body: %A" other

        match mainDefinition.ReturnTypeTokens with
        | Some returnTypeTokens ->
            let tokenTexts = returnTypeTokens |> List.map (fun token -> token.Text)
            Assert.Equal<string list>([ "IO"; "Unit" ], tokenTexts)
        | None ->
            failwith "Expected main to declare a return type."

        match mainDefinition.Body with
        | Some(
            Do
                [ DoLet(
                    "nums",
                    Binary(
                        Literal(LiteralValue.Integer 10L),
                        "::",
                        Binary(
                            Literal(LiteralValue.Integer 20L),
                            "::",
                            Binary(
                                Literal(LiteralValue.Integer 42L),
                                "::",
                                Name [ "Nil" ]
                            )
                        )
                    )
                  )
                  DoLet("total", Apply(Name [ "sumList" ], [ Name [ "nums" ] ]))
                  DoExpression(Apply(Name [ "printInt" ], [ Name [ "total" ] ])) ]
          ) -> ()
        | other ->
            failwithf "Unexpected main body: %A" other
    | other ->
        failwithf "Unexpected declarations: %A" other

[<Fact>]
let ``interpreter evaluates recursive list matches built from the cons operator`` () =
    let mainSource =
        [
            "module main"
            "sumList : List Int -> Int"
            "let sumList xs ="
            "    match xs"
            "    case Nil -> 0"
            "    case head :: tail -> head + sumList tail"
            "let result = sumList (10 :: 20 :: 42 :: Nil)"
        ]
        |> String.concat "\n"

    let workspace, result =
        evaluateInMemoryBinding
            "memory-list-match-root"
            "main.result"
            [ "main.kp", mainSource ]

    Assert.False(workspace.HasErrors, sprintf "Expected no diagnostics, got %A" workspace.Diagnostics)

    match result with
    | Result.Ok value ->
        Assert.Equal("72", RuntimeValue.format value)
    | Result.Error issue ->
        failwithf "Expected recursive list evaluation to succeed, got %s" issue.Message
