// Exercises compiler behavior that does not fit a backend- or milestone-specific suite.
module CoreTestSupport

open System
open System.Numerics
open System.Text
open Kappa.Compiler
open DiagnosticTestSupport
open Harness
open Xunit

let private assertSurfaceIntegerLiteral (expectedValue: int) expectedText expression =
    match expression with
    | NumericLiteral(SurfaceIntegerLiteral(value, sourceText, None)) ->
        Assert.Equal(BigInteger(expectedValue), value)
        Assert.Equal(expectedText, sourceText)
    | other ->
        failwithf "Expected surface integer literal %s, got %A" expectedText other


module CoreTestsShard0 =

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
                            leftFactor,
                            "*",
                            rightFactor
                        )
                    )
                )
              ) ->
                assertSurfaceIntegerLiteral 1 "1" leftFactor
                assertSurfaceIntegerLiteral 2 "2" rightFactor
            | other ->
                failwithf "Unexpected core expression: %A" other
        | other ->
            failwithf "Unexpected declarations: %A" other


    [<Fact>]
    let ``decodeUtf8 returns Err UnicodeDecodeError on invalid bytes`` () =
        let mainSource =
            [
                "module main"
                "import std.unicode.*"
                "let decoder = decodeUtf8"
            ]
            |> String.concat "\n"

        let workspace =
            compileInMemoryWorkspace
                "memory-unicode-decodeutf8-invalid-root"
                [ "main.kp", mainSource ]

        Assert.False(workspace.HasErrors, sprintf "Expected no diagnostics, got %A" workspace.Diagnostics)

        let session =
            match Interpreter.createRuntimeSession workspace with
            | Result.Ok session -> session
            | Result.Error issue ->
                failwithf "Expected runtime session materialization to succeed, got %s" issue.Message

        let decoder =
            match Interpreter.evaluateBindingInSession session "main.decoder" with
            | Result.Ok value -> value
            | Result.Error issue ->
                failwithf "Expected decoder binding to evaluate successfully, got %s" issue.Message

        let isUnicodeDecodeErrorField fields =
            match fields with
            | [ UnicodeDecodeErrorValue _ ] -> true
            | _ -> false

        match Interpreter.applyRuntimeValueInSession session decoder [ BytesValue [| 0xFFuy |] ] with
        | Result.Ok(ConstructedValue constructed)
            when constructed.Constructor.QualifiedName = "std.prelude.Err"
                 && constructed.Constructor.Name = "Err"
                 && isUnicodeDecodeErrorField constructed.Fields -> ()
        | Result.Ok value ->
            failwithf "Expected invalid UTF-8 to return Err UnicodeDecodeError, got %A" value
        | Result.Error issue ->
            failwithf "Expected invalid UTF-8 to return a Result value, got %s" issue.Message


    [<Fact>]
    let ``ordinary calls synthesize omitted leading implicit runtime parameters before explicit arguments`` () =
        let mainSource =
            [
                "module main"
                "trait Render (a : Type) ="
                "    render : a -> String"
                "instance Render Int ="
                "    let render x = primitiveIntToString x"
                "f : (@_ : Render Int) -> Int -> String"
                "let f @D x = D.render x"
                "let result = f 1"
            ]
            |> String.concat "\n"

        let workspace, result =
            evaluateInMemoryBinding
                "memory-leading-implicit-runtime-parameter-root"
                "main.result"
                [ "main.kp", mainSource ]

        Assert.False(
            workspace.HasErrors,
            sprintf "Expected omitted leading implicit runtime parameters to synthesize before explicit arguments, got %A" workspace.Diagnostics
        )

        match result with
        | Result.Ok(StringValue "1") -> ()
        | Result.Ok value ->
            failwithf "Expected omitted implicit Render argument to synthesize and return \"1\", got %A" value
        | Result.Error issue ->
            failwithf "Expected omitted implicit Render argument to synthesize successfully, got %s" issue.Message


    [<Fact>]
    let ``prelude show and compare resolve through trait members for builtin and user instances`` () =
        let mainSource =
            [
                "module main"
                "data Box : Type = Box Int"
                "instance Show Box ="
                "    let show value = \"box\""
                "let builtinShow = show 42"
                "let builtinCompare = compare 1 2"
                "let userShow = show (Box 0)"
                "let result = (builtinShow == \"42\") && (builtinCompare == LT) && (userShow == \"box\")"
            ]
            |> String.concat "\n"

        let workspace, result =
            evaluateInMemoryBinding
                "memory-prelude-trait-member-show-compare-root"
                "main.result"
                [ "main.kp", mainSource ]

        Assert.False(
            workspace.HasErrors,
            sprintf "Expected prelude show/compare to resolve through trait members, got %A" workspace.Diagnostics
        )

        match result with
        | Result.Ok(BooleanValue true) -> ()
        | Result.Ok value ->
            failwithf "Expected prelude show/compare trait member regression to evaluate to True, got %A" value
        | Result.Error issue ->
            failwithf "Expected prelude show/compare trait member regression to evaluate successfully, got %s" issue.Message


    [<Fact>]
    let ``interpreter supports bare wildcard lambda binders`` () =
        let mainSource =
            [
                "module main"
                "let ignore = \\_ -> 42"
                "let result = ignore ()"
            ]
            |> String.concat "\n"

        let workspace, result =
            evaluateInMemoryBinding
                "memory-wildcard-lambda-root"
                "main.result"
                [ "main.kp", mainSource ]

        Assert.False(workspace.HasErrors, sprintf "Expected no diagnostics, got %A" workspace.Diagnostics)

        match result with
        | Result.Ok value ->
            Assert.Equal("42", RuntimeValue.format value)
        | Result.Error issue ->
            failwithf "Expected wildcard lambda evaluation to succeed, got %s" issue.Message


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
    let ``interpreter resolves ordinary implicit helpers across imported trait modules`` () =
        let mainSource =
            [
                "module main"
                ""
                "import providers.score.(trait Score)"
                ""
                "resolve : forall (t : Type). (@_ : IsTrait t) -> (@v : t) -> t"
                "let resolve @v = v"
                ""
                "scoreInt : Score Int"
                "let scoreInt = resolve"
                ""
                "result : Int"
                "let result = scoreInt.score 42"
            ]
            |> String.concat "\n"

        let scoreSource =
            [
                "module providers.score"
                ""
                "trait Score a ="
                "    score : a -> Int"
                ""
                "instance Score Int ="
                "    let score x = x"
            ]
            |> String.concat "\n"

        let workspace, result =
            evaluateInMemoryBinding
                "memory-imported-implicit-helper-root"
                "main.result"
                [ "main.kp", mainSource
                  "providers/score.kp", scoreSource ]

        Assert.False(workspace.HasErrors, sprintf "Expected no diagnostics, got %A" workspace.Diagnostics)

        match result with
        | Result.Ok value ->
            Assert.Equal("42", RuntimeValue.format value)
        | Result.Error issue ->
            failwithf "Expected imported helper-based dictionary resolution to succeed, got %s" issue.Message


    [<Fact>]
    let ``interpreter resolves qualified trait aliases through module imports`` () =
        let mainSource =
            [
                "module main"
                ""
                "import providers.score as score"
                ""
                "resolve : forall (t : Type). (@_ : IsTrait t) -> (@v : t) -> t"
                "let resolve @v = v"
                ""
                "scoreInt : score.Score Int"
                "let scoreInt = resolve"
                ""
                "result : Int"
                "let result = scoreInt.score 42"
            ]
            |> String.concat "\n"

        let scoreSource =
            [
                "module providers.score"
                ""
                "trait Score a ="
                "    score : a -> Int"
                ""
                "instance Score Int ="
                "    let score x = x"
            ]
            |> String.concat "\n"

        let workspace, result =
            evaluateInMemoryBinding
                "memory-qualified-trait-alias-root"
                "main.result"
                [ "main.kp", mainSource
                  "providers/score.kp", scoreSource ]

        Assert.False(workspace.HasErrors, sprintf "Expected no diagnostics, got %A" workspace.Diagnostics)

        match result with
        | Result.Ok value ->
            Assert.Equal("42", RuntimeValue.format value)
        | Result.Error issue ->
            failwithf "Expected qualified trait alias resolution to succeed, got %s" issue.Message


    [<Fact>]
    let ``instance search distinguishes same-spelling qualified traits by semantic identity`` () =
        let mainSource =
            [
                "module main"
                ""
                "import providers.a as a"
                "import providers.b as b"
                ""
                "resolve : forall (t : Type). (@_ : IsTrait t) -> (@v : t) -> t"
                "let resolve @v = v"
                ""
                "showA : a.Show Int"
                "let showA = resolve"
                ""
                "result : Int"
                "let result = showA.show 0"
            ]
            |> String.concat "\n"

        let traitASource =
            [
                "module providers.a"
                ""
                "trait Show a ="
                "    show : a -> Int"
                ""
                "instance Show Int ="
                "    let show _ = 1"
            ]
            |> String.concat "\n"

        let traitBSource =
            [
                "module providers.b"
                ""
                "trait Show a ="
                "    show : a -> Int"
                ""
                "instance Show Int ="
                "    let show _ = 2"
            ]
            |> String.concat "\n"

        let workspace, result =
            evaluateInMemoryBinding
                "memory-qualified-trait-identity-root"
                "main.result"
                [ "main.kp", mainSource
                  "providers/a.kp", traitASource
                  "providers/b.kp", traitBSource ]

        Assert.False(workspace.HasErrors, sprintf "Expected no diagnostics, got %A" workspace.Diagnostics)

        match result with
        | Result.Ok value ->
            Assert.Equal("1", RuntimeValue.format value)
        | Result.Error issue ->
            failwithf "Expected semantic trait identity resolution to succeed, got %s" issue.Message


    [<Fact>]
    let ``interpreter resolves bundled summon helper across imported trait modules`` () =
        let mainSource =
            [
                "module main"
                ""
                "import providers.score.(trait Score)"
                ""
                "scoreInt : Score Int"
                "let scoreInt = summon"
                ""
                "result : Int"
                "let result = scoreInt.score 42"
            ]
            |> String.concat "\n"

        let scoreSource =
            [
                "module providers.score"
                ""
                "trait Score a ="
                "    score : a -> Int"
                ""
                "instance Score Int ="
                "    let score x = x"
            ]
            |> String.concat "\n"

        let workspace, result =
            evaluateInMemoryBinding
                "memory-prelude-imported-summon-root"
                "main.result"
                [ "main.kp", mainSource
                  "providers/score.kp", scoreSource ]

        Assert.False(workspace.HasErrors, sprintf "Expected no diagnostics, got %A" workspace.Diagnostics)

        match result with
        | Result.Ok value ->
            Assert.Equal("42", RuntimeValue.format value)
        | Result.Error issue ->
            failwithf "Expected imported bundled summon helper resolution to succeed, got %s" issue.Message


    [<Fact>]
    let ``interpreter resolves bundled ordering helpers as ordinary bindings`` () =
        let mainSource =
            [
                "module main"
                ""
                "result : Int"
                "let result ="
                "    if (1 /= 2) && (1 < 2) && (1 <= 1) && (2 > 1) && (2 >= 2) then 1 else 0"
            ]
            |> String.concat "\n"

        let workspace, result =
            evaluateInMemoryBinding
                "memory-prelude-ordering-helpers-root"
                "main.result"
                [ "main.kp", mainSource ]

        Assert.False(workspace.HasErrors, sprintf "Expected no diagnostics, got %A" workspace.Diagnostics)

        match result with
        | Result.Ok value ->
            Assert.Equal("1", RuntimeValue.format value)
        | Result.Error issue ->
            failwithf "Expected bundled ordering helpers to resolve as ordinary bindings, got %s" issue.Message


    [<Fact>]
    let ``resource checker lets local let bindings shadow prelude interpolation helpers`` () =
        let mainSource =
            [
                "module main"
                ""
                "data Box : Type ="
                "    Box (value : Int)"
                ""
                "readBox : (& b : Box) -> Int"
                "let readBox (& b : Box) : Int = b.value"
                ""
                "ok : Unit -> Int"
                "let ok () : Int ="
                "    let & b = Box 1"
                "    in readBox b"
            ]
            |> String.concat "\n"

        let workspace =
            compileInMemoryWorkspace
                "memory-resource-let-shadow-root"
                [ "main.kp", mainSource ]

        Assert.False(workspace.HasErrors, sprintf "Expected local let shadowing to win over implicit prelude names, got %A" workspace.Diagnostics)


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
    let ``compiler rejects ambiguous imported ordinary names`` () =
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

        let workspace =
            compileInMemoryWorkspace
                "memory-ambiguous-root"
                [
                    "a.kp", moduleASource
                    "b.kp", moduleBSource
                    "main.kp", mainSource
                ]

        Assert.True(workspace.HasErrors, "Expected ordinary lexical ambiguity to be rejected during compilation.")
        Assert.Contains(
            workspace.Diagnostics,
            fun diagnostic ->
                diagnostic.Code = DiagnosticCode.NameAmbiguous
                && diagnostic.Payload.Kind = "name-ambiguous"
                && hasPayloadText "spelling" "value" diagnostic
        )


    [<Fact>]
    let ``compiler rejects a binding group that exposes both a term and a module in term position`` () =
        let valuesSource =
            [
                "module values"
                "let target = 1"
            ]
            |> String.concat "\n"

        let containerSource =
            [
                "module container"
                "let payload = 2"
            ]
            |> String.concat "\n"

        let mainSource =
            [
                "module main"
                "import values.(target as item)"
                "import container as item"
                "let result = item"
            ]
            |> String.concat "\n"

        let workspace =
            compileInMemoryWorkspace
                "memory-binding-group-term-module-root"
                [
                    "values.kp", valuesSource
                    "container.kp", containerSource
                    "main.kp", mainSource
                ]

        Assert.True(workspace.HasErrors, "Expected the mixed term/module binding group to be ambiguous in term position.")
        Assert.Contains(
            workspace.Diagnostics,
            fun diagnostic ->
                diagnostic.Code = DiagnosticCode.NameAmbiguous
                && diagnostic.Payload.Kind = "name-ambiguous"
                && hasPayloadText "spelling" "item" diagnostic
        )


    [<Fact>]
    let ``kind-qualified type expressions resolve imported same-spelling data-family aliases`` () =
        let librarySource =
            [
                "module library"
                "data Box ="
                "    Box"
            ]
            |> String.concat "\n"

        let mainSource =
            [
                "module main"
                "import library.(type Box as AliasBox)"
                "boxType : Type"
                "let boxType = type AliasBox"
            ]
            |> String.concat "\n"

        let workspace =
            compileInMemoryWorkspace
                "memory-kind-qualified-type-alias-root"
                [
                    "library.kp", librarySource
                    "main.kp", mainSource
                ]

        Assert.False(workspace.HasErrors, sprintf "Expected imported kind-qualified type alias to resolve, got %A" workspace.Diagnostics)

    [<Fact>]
    let ``unqualified same-spelling data-family aliases keep constructor term semantics`` () =
        let librarySource =
            [
                "module library"
                "data Box ="
                "    Box"
            ]
            |> String.concat "\n"

        let mainSource =
            [
                "module main"
                "import library.(Box as AliasBox)"
                "termValue : AliasBox"
                "let termValue = AliasBox"
            ]
            |> String.concat "\n"

        let workspace =
            compileInMemoryWorkspace
                "memory-same-spelling-binding-group-alias-root"
                [
                    "library.kp", librarySource
                    "main.kp", mainSource
                ]

        Assert.False(workspace.HasErrors, sprintf "Expected same-spelling alias binding group to compile, got %A" workspace.Diagnostics)


    [<Fact>]
    let ``compilation rejects refl when function type equality changes binder quantity`` () =
        let mainSource =
            [
                "module main"
                "badEq : ((x : Nat) -> Nat) = ((1 x : Nat) -> Nat)"
                "let badEq = refl"
            ]
            |> String.concat "\n"

        let workspace =
            compileInMemoryWorkspace
                "memory-function-quantity-equality-root"
                [ "main.kp", mainSource ]

        Assert.True(workspace.HasErrors, "Expected a quantity-sensitive equality diagnostic.")

        Assert.True(
            workspace.Diagnostics
            |> List.exists (fun diagnostic -> diagnostic.Code = DiagnosticCode.TypeEqualityMismatch)
        )


module CoreTestsShard1 =

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
    let ``interpreter orders text values by unicode scalar sequence not utf16 ordinal`` () =
        let mainSource =
            [
                "module main"
                "let stringOrder = \"\\u{E000}\" < \"\\u{1F600}\""
                "let charOrder = '\\u{E000}' < '\\u{1F600}'"
                "let result = stringOrder && charOrder"
            ]
            |> String.concat "\n"

        let workspace, result =
            evaluateInMemoryBinding
                "memory-unicode-scalar-order-root"
                "main.result"
                [ "main.kp", mainSource ]

        Assert.False(workspace.HasErrors, sprintf "Expected no diagnostics, got %A" workspace.Diagnostics)

        match result with
        | Result.Ok(BooleanValue true) -> ()
        | Result.Ok value ->
            failwithf "Expected Unicode scalar ordering regression to evaluate to True, got %A" value
        | Result.Error issue ->
            failwithf "Expected Unicode scalar ordering regression to evaluate successfully, got %s" issue.Message


    [<Fact>]
    let ``binding bodies can use omitted leading implicit runtime parameters from their signatures`` () =
        let mainSource =
            [
                "module main"
                "trait Render (a : Type) ="
                "    render : a -> String"
                "instance Render Int ="
                "    let render x = primitiveIntToString x"
                "format : (@_ : Render Int) -> Int -> String"
                "let format x = render x"
                "let result = format 1"
            ]
            |> String.concat "\n"

        let workspace, result =
            evaluateInMemoryBinding
                "memory-omitted-implicit-binding-body-root"
                "main.result"
                [ "main.kp", mainSource ]

        Assert.False(
            workspace.HasErrors,
            sprintf "Expected omitted implicit signature binders to stay available in the binding body, got %A" workspace.Diagnostics
        )

        match result with
        | Result.Ok(StringValue "1") -> ()
        | Result.Ok value ->
            failwithf "Expected binding body to use omitted implicit Render evidence and return \"1\", got %A" value
        | Result.Error issue ->
            failwithf "Expected omitted implicit Render evidence to evaluate successfully, got %s" issue.Message


    [<Fact>]
    let ``real Hashable instances support structural option and result hashing`` () =
        let mainSource =
            [
                "module main"
                "import std.hash.*"
                "noneValue : Option Int"
                "let noneValue = None"
                "okValue : Result Int Int"
                "let okValue = Ok 1"
                "errValue : Result Int Int"
                "let errValue = Err 1"
                "let someHash = hashWith defaultHashSeed (Some 1)"
                "let noneHash = hashWith defaultHashSeed noneValue"
                "let okHash = hashWith defaultHashSeed okValue"
                "let errHash = hashWith defaultHashSeed errValue"
                "let result = (someHash != noneHash) && (okHash != errHash)"
            ]
            |> String.concat "\n"

        let workspace, result =
            evaluateInMemoryBinding
                "memory-hash-structural-instances-root"
                "main.result"
                [ "main.kp", mainSource ]

        Assert.False(workspace.HasErrors, sprintf "Expected structural Hashable instances to compile, got %A" workspace.Diagnostics)

        match result with
        | Result.Ok(BooleanValue true) -> ()
        | Result.Ok value ->
            failwithf "Expected structural Hashable instances to distinguish constructors, got %A" value
        | Result.Error issue ->
            failwithf "Expected structural Hashable instances to evaluate successfully, got %s" issue.Message


    [<Fact>]
    let ``interpreter supports typed wildcard binders in lambdas and function headers`` () =
        let mainSource =
            [
                "module main"
                "twice : (Unit -> Int) -> Int"
                "let twice f = f () + f ()"
                "keep : (_ : Int) -> Int -> Int"
                "let keep (_ : Int) x = x"
                "demo : Int -> Int"
                "let demo n = keep 0 (twice (\\(_ : Unit) -> n))"
                "result : Int"
                "let result = demo 21"
            ]
            |> String.concat "\n"

        let workspace, result =
            evaluateInMemoryBinding
                "memory-typed-wildcard-lambda-root"
                "main.result"
                [ "main.kp", mainSource ]

        Assert.False(workspace.HasErrors, sprintf "Expected no diagnostics, got %A" workspace.Diagnostics)

        match result with
        | Result.Ok value ->
            Assert.Equal("42", RuntimeValue.format value)
        | Result.Error issue ->
            failwithf "Expected typed wildcard binders to evaluate successfully, got %s" issue.Message


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
    let ``resource checking does not demand runtime use for compile time trait evidence classifiers`` () =
        let sqlSource =
            [
                "module dsl.sql"
                ""
                "data PreparedSql (row : Type) : Type ="
                "    PreparedSql (text : String)"
                ""
                "resolve : forall (t : Type). (@_ : IsTrait t) -> (@v : t) -> t"
                "let resolve @v = v"
                ""
                "instance InterpolatedMacro (PreparedSql row) ="
                "    let buildInterpolated fragments ="
                "        pure '{ PreparedSql \"prepared-sql-from-fragments\" }"
                ""
                "macro : forall (row : Type). InterpolatedMacro (PreparedSql row)"
                "let macro @row = resolve"
                ""
                "sql : forall (row : Type). Elab (InterpolatedMacro (PreparedSql row))"
                "let sql @row = pure (macro @row)"
            ]
            |> String.concat "\n"

        let workspace =
            compileInMemoryWorkspace
                "memory-compile-time-constraint-parameter-root"
                [ "dsl/sql.kp", sqlSource ]

        Assert.False(workspace.HasErrors, sprintf "Expected compile-time IsTrait parameters not to trigger runtime quantity demand, got %A" workspace.Diagnostics)


    [<Fact>]
    let ``interpreter evaluates nested for clauses over query lowered top level bindings`` () =
        let mainSource =
            [
                "module main"
                ""
                "type Node = (id : Int, parent : Int, name : Int)"
                ""
                "nodes : List Node"
                "let nodes ="
                "    ["
                "        (id = 0, parent = -1, name = 0),"
                "        (id = 1, parent = 0, name = 1),"
                "        (id = 2, parent = 1, name = 2),"
                "        (id = 3, parent = 2, name = 3),"
                "        (id = 4, parent = 1, name = 4),"
                "        (id = 5, parent = 4, name = 5),"
                "        (id = 6, parent = 4, name = 6)"
                "    ]"
                ""
                "descendantsTwoAway : Node -> List Node"
                "let descendantsTwoAway root ="
                "    ["
                "        for child in nodes"
                "        for grandChild in nodes"
                "        if child.parent == root.id"
                "        if grandChild.parent == child.id"
                "        yield grandChild"
                "    ]"
                ""
                "hits : List Node"
                "let hits ="
                "    ["
                "        for root in nodes"
                "        if root.parent == -1"
                "        for hit in descendantsTwoAway root"
                "        yield hit"
                "    ]"
                ""
                "result : List Int"
                "let result ="
                "    ["
                "        for hit in hits"
                "        yield hit.name"
                "    ]"
            ]
            |> String.concat "\n"

        let workspace, result =
            evaluateInMemoryBinding
                "memory-query-top-level-nested-for-bindings-root"
                "main.result"
                [ "main.kp", mainSource ]

        Assert.False(workspace.HasErrors, sprintf "Expected no diagnostics, got %A" workspace.Diagnostics)

        match result with
        | Result.Ok value ->
            Assert.Equal("2 :: 4 :: Nil", RuntimeValue.format value)
        | Result.Error issue ->
            failwithf "Expected nested for evaluation to succeed, got %s" issue.Message


    [<Fact>]
    let ``interpreter resolves bundled range operators as ordinary bindings`` () =
        let mainSource =
            [
                "module main"
                ""
                "data Span : Type ="
                "    Span Int Int Bool"
                ""
                "instance Rangeable Int ="
                "    let Range = Span"
                "    let range from to exclusive = Span from to exclusive"
                ""
                "closedValue : Int"
                "let closedValue ="
                "    match 2 .. 5"
                "    case Span from to exclusive ->"
                "        if exclusive then 0 else from + to"
                ""
                "openValue : Int"
                "let openValue ="
                "    match 2 ..< 5"
                "    case Span from to exclusive ->"
                "        if exclusive then from + to + 100 else 0"
                ""
                "result : Int"
                "let result = closedValue + openValue"
            ]
            |> String.concat "\n"

        let workspace, result =
            evaluateInMemoryBinding
                "memory-prelude-range-operators-root"
                "main.result"
                [ "main.kp", mainSource ]

        Assert.False(workspace.HasErrors, sprintf "Expected no diagnostics, got %A" workspace.Diagnostics)

        match result with
        | Result.Ok value ->
            Assert.Equal("114", RuntimeValue.format value)
        | Result.Error issue ->
            failwithf "Expected bundled range operators to resolve as ordinary bindings, got %s" issue.Message


    [<Fact>]
    let ``resource checker preserves constructor parameter quantities`` () =
        let mainSource =
            [
                "module main"
                ""
                "data Box : Type ="
                "    Box (1 value : Int)"
                ""
                "wrap : (1 value : Int) -> Box"
                "let wrap (1 value : Int) : Box = Box value"
            ]
            |> String.concat "\n"

        let workspace =
            compileInMemoryWorkspace
                "memory-resource-constructor-quantity-root"
                [ "main.kp", mainSource ]

        Assert.False(workspace.HasErrors, sprintf "Expected constructor signatures to preserve parameter quantities, got %A" workspace.Diagnostics)


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
                    | expression ->
                        assertSurfaceIntegerLiteral 0 "0" expression

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
                        { Pattern = NamePattern "nums"
                          Quantity = None },
                        Binary(
                            firstValue,
                            "::",
                            Binary(
                                secondValue,
                                "::",
                                Binary(
                                    thirdValue,
                                    "::",
                                    Name [ "Nil" ]
                                )
                            )
                        )
                      )
                      DoLet(
                          { Pattern = NamePattern "total"
                            Quantity = None },
                          Apply(Name [ "sumList" ], [ Name [ "nums" ] ])
                      )
                      DoExpression(Apply(Name [ "printInt" ], [ Name [ "total" ] ])) ]
              ) ->
                assertSurfaceIntegerLiteral 10 "10" firstValue
                assertSurfaceIntegerLiteral 20 "20" secondValue
                assertSurfaceIntegerLiteral 42 "42" thirdValue
            | other ->
                failwithf "Unexpected main body: %A" other
        | other ->
            failwithf "Unexpected declarations: %A" other


    [<Fact>]
    let ``interpreter executes io programs against an injected output sink`` () =
        let mainSource =
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

        let workspace =
            compileInMemoryWorkspace
                "memory-injected-output-root"
                [ "main.kp", mainSource ]

        Assert.False(workspace.HasErrors, sprintf "Expected no diagnostics, got %A" workspace.Diagnostics)

        let buffer = StringBuilder()

        let output: RuntimeOutput =
            { Write = fun text -> buffer.Append(text) |> ignore
              WriteLine = fun text -> buffer.AppendLine(text) |> ignore }

        match Interpreter.executeBindingWithOutput workspace output "main.main" with
        | Result.Ok UnitValue -> ()
        | Result.Ok value ->
            failwithf "Expected IO entry point to execute to Unit, got %s" (RuntimeValue.format value)
        | Result.Error issue ->
            failwithf "Expected injected-output execution to succeed, got %s" issue.Message

        Assert.Equal("72\n", buffer.ToString().Replace("\r\n", "\n"))

module CoreTestsShard2 =

    [<Fact>]
    let ``interpreter evaluates ordinary prelude convenience bindings`` () =
        let mainSource =
            [
                "module main"
                "let result ="
                "    if and True (or False (not False)) then 43 else 0"
            ]
            |> String.concat "\n"

        let workspace, result =
            evaluateInMemoryBinding
                "memory-ordinary-prelude-convenience-bindings-root"
                "main.result"
                [ "main.kp", mainSource ]

        Assert.False(workspace.HasErrors, sprintf "Expected no diagnostics, got %A" workspace.Diagnostics)

        match result with
        | Result.Ok value ->
            Assert.Equal("43", RuntimeValue.format value)
        | Result.Error issue ->
            failwithf "Expected ordinary prelude convenience bindings to evaluate successfully, got %s" issue.Message


    [<Fact>]
    let ``streaming hash intrinsics return HashState and can be finished`` () =
        let mainSource =
            [
                "module main"
                "import std.hash.*"
                "let result = finishHashState (hashString \"a\" (newHashState defaultHashSeed))"
            ]
            |> String.concat "\n"

        let workspace, result =
            evaluateInMemoryBinding
                "memory-hash-streaming-finish-root"
                "main.result"
                [ "main.kp", mainSource ]

        Assert.False(workspace.HasErrors, sprintf "Expected hash streaming program to compile, got %A" workspace.Diagnostics)

        match result with
        | Result.Ok(HashCodeValue _) -> ()
        | Result.Ok value ->
            failwithf "Expected hashString/newHashState composition to produce a finishable HashState, got %A" value
        | Result.Error issue ->
            failwithf "Expected finishHashState(hashString ...) to evaluate successfully, got %s" issue.Message


    [<Fact>]
    let ``grouped constrained instances expose premise dictionaries to ordinary helper calls`` () =
        let mainSource =
            [
                "module main"
                "trait Render (a : Type) ="
                "    render : a -> String"
                "instance Render Int ="
                "    let render x = primitiveIntToString x"
                "helper : (@_ : Render a) -> a -> String"
                "let helper @R x = R.render x"
                "trait Lift (a : Type) ="
                "    lift : a -> String"
                "instance (Render a) => Lift (Option a) ="
                "    let lift value ="
                "        match value"
                "        case Some inner -> helper inner"
                "        case None -> \"none\""
                "let result = lift (Some 1)"
            ]
            |> String.concat "\n"

        let workspace, result =
            evaluateInMemoryBinding
                "memory-grouped-instance-premise-helper-root"
                "main.result"
                [ "main.kp", mainSource ]

        Assert.False(
            workspace.HasErrors,
            sprintf "Expected grouped constrained instance premises to satisfy ordinary helper implicits, got %A" workspace.Diagnostics
        )

        match result with
        | Result.Ok(StringValue "1") -> ()
        | Result.Ok value ->
            failwithf "Expected grouped constrained instance helper call to return \"1\", got %A" value
        | Result.Error issue ->
            failwithf "Expected grouped constrained instance helper call to evaluate successfully, got %s" issue.Message


    [<Fact>]
    let ``missing Hashable evidence now fails in compilation instead of surviving to runtime`` () =
        let workspace =
            compileInMemoryWorkspace
                "memory-hash-missing-evidence-root"
                [
                    "main.kp",
                    [
                        "module main"
                        "import std.hash.*"
                        "result : HashCode"
                        "let result = hashWith defaultHashSeed (\\x -> x)"
                    ]
                    |> String.concat "\n"
                ]

        Assert.True(workspace.HasErrors, "Expected non-Hashable function values to be rejected during compilation.")


    [<Fact>]
    let ``interpreter supports typed local let bindings in expression position`` () =
        let mainSource =
            [
                "module main"
                "result : Int"
                "let result ="
                "    let value : Int = 42"
                "    value"
            ]
            |> String.concat "\n"

        let workspace, result =
            evaluateInMemoryBinding
                "memory-typed-local-let-root"
                "main.result"
                [ "main.kp", mainSource ]

        Assert.False(workspace.HasErrors, sprintf "Expected no diagnostics, got %A" workspace.Diagnostics)

        match result with
        | Result.Ok value ->
            Assert.Equal("42", RuntimeValue.format value)
        | Result.Error issue ->
            failwithf "Expected typed local let evaluation to succeed, got %s" issue.Message


    [<Fact>]
    let ``interpreter resolves trait instances through transparent type aliases`` () =
        let mainSource =
            [
                "module main"
                "type Alias = Int"
                ""
                "trait Show a ="
                "    show : a -> String"
                ""
                "instance Show Int ="
                "    let show x = primitiveIntToString x"
                ""
                "render : Show Alias => Alias -> String"
                "let render x = show x"
                ""
                "message : String"
                "let message = render 7"
            ]
            |> String.concat "\n"

        let workspace, result =
            evaluateInMemoryBinding
                "memory-transparent-alias-instance-root"
                "main.message"
                [ "main.kp", mainSource ]

        Assert.False(workspace.HasErrors, sprintf "Expected no diagnostics, got %A" workspace.Diagnostics)

        match result with
        | Result.Ok value ->
            Assert.Equal("\"7\"", RuntimeValue.format value)
        | Result.Error issue ->
            failwithf "Expected alias-based instance resolution to succeed, got %s" issue.Message


    [<Fact>]
    let ``resource checking allows payload pattern matching without double consuming the scrutinee root`` () =
        let mainSource =
            [
                "module main"
                ""
                "isOkString : Result String String -> String -> Bool"
                "let isOkString result expected ="
                "    match result"
                "    case Ok s -> s == expected"
                "    case Err _ -> False"
                ""
                "result : Bool"
                "let result = isOkString (Ok \"hi\") \"hi\""
            ]
            |> String.concat "\n"

        let workspace =
            compileInMemoryWorkspace
                "memory-match-payload-does-not-double-consume-root"
                [ "main.kp", mainSource ]

        Assert.False(workspace.HasErrors, sprintf "Expected constructor payload pattern matching not to double-consume the scrutinee root, got %A" workspace.Diagnostics)


    [<Fact>]
    let ``explicit trait evidence values do not satisfy implicit trait constraints`` () =
        let mainSource =
            [
                "module main"
                "trait Score (a : Type) ="
                "    score : a -> Int"
                ""
                "needsImplicit : (@s : Score Int) -> Int"
                "let needsImplicit @s = s.score 42"
                ""
                "bad : Score Int -> Int"
                "let bad dict = needsImplicit"
            ]
            |> String.concat "\n"

        let workspace =
            compileInMemoryWorkspace
                "memory-explicit-trait-value-does-not-satisfy-implicit-root"
                [ "main.kp", mainSource ]

        Assert.True(workspace.HasErrors, "Expected explicit trait evidence values not to satisfy implicit trait constraints.")

        Assert.Contains(
            workspace.Diagnostics,
            fun diagnostic ->
                diagnostic.Code = DiagnosticCode.TypeEqualityMismatch
                && hasPayloadText "reason" "trait-constraint-unresolved" diagnostic
                && hasPayloadText "constraint-text" "Score Int" diagnostic
        )


    [<Fact>]
    let ``interpreter resolves bundled equality proof helpers as ordinary bindings`` () =
        let mainSource =
            [
                "module main"
                ""
                "proof1 : 1 = 1"
                "let proof1 = sym refl"
                ""
                "proof2 : 1 = 1"
                "let proof2 = trans proof1 refl"
                ""
                "proof3 : (1 + 1) = (1 + 1)"
                "let proof3 = cong (\\x -> x + 1) proof2"
                ""
                "result : Int"
                "let result = 1"
            ]
            |> String.concat "\n"

        let workspace, result =
            evaluateInMemoryBinding
                "memory-prelude-equality-helpers-root"
                "main.result"
                [ "main.kp", mainSource ]

        Assert.False(workspace.HasErrors, sprintf "Expected no diagnostics, got %A" workspace.Diagnostics)

        match result with
        | Result.Ok value ->
            Assert.Equal("1", RuntimeValue.format value)
        | Result.Error issue ->
            failwithf "Expected bundled equality proof helpers to resolve as ordinary bindings, got %s" issue.Message


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
    let ``interpreter short circuits boolean operators`` () =
        let mainSource =
            [
                "module main"
                "import std.testing.*"
                "let result ="
                "    if False && failNow \"short-circuit rhs evaluated\""
                "    then 0"
                "    else 42"
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
    let ``parser captures local borrowed anonymous record patterns`` () =
        let sourceText =
            [
                "module main"
                "let pair = source"
                "let demo ="
                "    let & (x = bx, y = by) = pair"
                "    bx + by"
            ]
            |> String.concat "\n"

        let _, lexed, parsed =
            lexAndParse
                "main.kp"
                sourceText

        Assert.Empty(lexed.Diagnostics)
        Assert.Empty(parsed.Diagnostics)

        match parsed.Syntax.Declarations with
        | [ LetDeclaration _
            LetDeclaration definition ] ->
            match definition.Body with
            | Some(LocalLet(binding, Name [ "pair" ], Binary(Name [ "bx" ], "+", Name [ "by" ]))) ->
                Assert.Equal<Quantity option>(Some(QuantityBorrow None), binding.Quantity)

                match binding.Pattern with
                | AnonymousRecordPattern(
                    [ { Name = "x"
                        IsImplicit = false
                        Pattern = NamePattern "bx" }
                      { Name = "y"
                        IsImplicit = false
                        Pattern = NamePattern "by" } ],
                    None
                  ) -> ()
                | other ->
                    failwithf "Unexpected local borrowed record pattern: %A" other
            | other ->
                failwithf "Unexpected local borrowed record pattern body: %A" other
        | other ->
            failwithf "Unexpected declarations: %A" other


module CoreTestsShard3 =

    [<Fact>]
    let ``decodeUtf8 round trips valid utf8 bytes`` () =
        let mainSource =
            [
                "module main"
                "import std.unicode.*"
                "let result = decodeUtf8 (utf8Bytes \"x\")"
            ]
            |> String.concat "\n"

        let workspace, result =
            evaluateInMemoryBinding
                "memory-unicode-decodeutf8-roundtrip-root"
                "main.result"
                [ "main.kp", mainSource ]

        Assert.False(workspace.HasErrors, sprintf "Expected no diagnostics, got %A" workspace.Diagnostics)

        let isSingleStringField fields =
            match fields with
            | [ StringValue "x" ] -> true
            | _ -> false

        match result with
        | Result.Ok(ConstructedValue constructed)
            when constructed.Constructor.QualifiedName = "std.prelude.Ok"
                 && constructed.Constructor.Name = "Ok"
                 && isSingleStringField constructed.Fields -> ()
        | Result.Ok value ->
            failwithf "Expected decodeUtf8 to return Ok \"x\", got %A" value
        | Result.Error issue ->
            failwithf "Expected valid UTF-8 to decode successfully, got %s" issue.Message


    [<Fact>]
    let ``streaming hash updates compose across chunks`` () =
        let mainSource =
            [
                "module main"
                "import std.hash.*"
                "let direct = finishHashState (hashString \"ab\" (newHashState defaultHashSeed))"
                "let chunked ="
                "    finishHashState"
                "        (hashString \"b\""
                "            (hashString \"a\" (newHashState defaultHashSeed)))"
                "let result = direct == chunked"
            ]
            |> String.concat "\n"

        let workspace, result =
            evaluateInMemoryBinding
                "memory-hash-streaming-compose-root"
                "main.result"
                [ "main.kp", mainSource ]

        Assert.False(workspace.HasErrors, sprintf "Expected chunked hash streaming program to compile, got %A" workspace.Diagnostics)

        match result with
        | Result.Ok(BooleanValue true) -> ()
        | Result.Ok value ->
            failwithf "Expected chunked hash updates to match one-shot hashing, got %A" value
        | Result.Error issue ->
            failwithf "Expected chunked hash updates to evaluate successfully, got %s" issue.Message


    [<Fact>]
    let ``prelude boolean helpers evaluate through ordinary definitions`` () =
        let mainSource =
            [
                "module main"
                "let bools = (not False) && (and True True) && (or False True)"
                "let result = bools"
            ]
            |> String.concat "\n"

        let workspace, result =
            evaluateInMemoryBinding
                "memory-prelude-boolean-definitions-root"
                "main.result"
                [ "main.kp", mainSource ]

        Assert.False(
            workspace.HasErrors,
            sprintf "Expected prelude boolean helpers to compile as ordinary definitions, got %A" workspace.Diagnostics
        )

        match result with
        | Result.Ok(BooleanValue true) -> ()
        | Result.Ok value ->
            failwithf "Expected ordinary prelude boolean helper definitions to evaluate to True, got %A" value
        | Result.Error issue ->
            failwithf "Expected ordinary prelude boolean helper definitions to evaluate successfully, got %s" issue.Message


    [<Fact>]
    let ``interpreter supports type scoped constructor patterns`` () =
        let mainSource =
            [
                "module main"
                "result : Int"
                "let result ="
                "    match Some 41"
                "    case Option.Some x -> x + 1"
                "    case _ -> 0"
            ]
            |> String.concat "\n"

        let workspace, result =
            evaluateInMemoryBinding
                "memory-type-scoped-pattern-root"
                "main.result"
                [ "main.kp", mainSource ]

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
            | Some(Binary(left, "++", right)) ->
                assertSurfaceIntegerLiteral 20 "20" left
                assertSurfaceIntegerLiteral 22 "22" right
            | other -> failwithf "Unexpected operator expression: %A" other
        | other ->
            failwithf "Unexpected declarations: %A" other

        match Interpreter.evaluateBinding workspace "main.result" with
        | Result.Ok value ->
            Assert.Equal("42", RuntimeValue.format value)
        | Result.Error issue ->
            failwithf "Expected user-defined operator evaluation to succeed, got %s" issue.Message


    [<Fact>]
    let ``interpreter resolves dependent implicit helpers as ordinary bindings`` () =
        let mainSource =
            [
                "module main"
                ""
                "trait Score a ="
                "    score : a -> Int"
                ""
                "instance Score Int ="
                "    let score x = x"
                ""
                "resolve : forall (t : Type). (@_ : IsTrait t) -> (@v : t) -> t"
                "let resolve @v = v"
                ""
                "scoreInt : Score Int"
                "let scoreInt = resolve"
                ""
                "result : Int"
                "let result = scoreInt.score 42"
            ]
            |> String.concat "\n"

        let workspace, result =
            evaluateInMemoryBinding
                "memory-ordinary-implicit-helper-root"
                "main.result"
                [ "main.kp", mainSource ]

        Assert.False(workspace.HasErrors, sprintf "Expected no diagnostics, got %A" workspace.Diagnostics)

        match result with
        | Result.Ok value ->
            Assert.Equal("42", RuntimeValue.format value)
        | Result.Error issue ->
            failwithf "Expected ordinary helper-based dictionary resolution to succeed, got %s" issue.Message


    [<Fact>]
    let ``interpreter resolves bundled summon helper as an ordinary binding`` () =
        let mainSource =
            [
                "module main"
                ""
                "trait Score a ="
                "    score : a -> Int"
                ""
                "instance Score Int ="
                "    let score x = x"
                ""
                "scoreInt : Score Int"
                "let scoreInt = summon"
                ""
                "result : Int"
                "let result = scoreInt.score 42"
            ]
            |> String.concat "\n"

        let workspace, result =
            evaluateInMemoryBinding
                "memory-prelude-summon-root"
                "main.result"
                [ "main.kp", mainSource ]

        Assert.False(workspace.HasErrors, sprintf "Expected no diagnostics, got %A" workspace.Diagnostics)

        match result with
        | Result.Ok value ->
            Assert.Equal("42", RuntimeValue.format value)
        | Result.Error issue ->
            failwithf "Expected bundled summon helper resolution to succeed, got %s" issue.Message


    [<Fact>]
    let ``interpreter resolves bundled orElse helper as an ordinary binding`` () =
        let mainSource =
            [
                "module main"
                ""
                "result : Int"
                "let result ="
                "    match orElse None (Some 42)"
                "    case Some value -> value"
                "    case None -> 0"
            ]
            |> String.concat "\n"

        let workspace, result =
            evaluateInMemoryBinding
                "memory-prelude-orelse-root"
                "main.result"
                [ "main.kp", mainSource ]

        Assert.False(workspace.HasErrors, sprintf "Expected no diagnostics, got %A" workspace.Diagnostics)

        match result with
        | Result.Ok value ->
            Assert.Equal("42", RuntimeValue.format value)
        | Result.Error issue ->
            failwithf "Expected bundled orElse helper resolution to succeed, got %s" issue.Message


    [<Fact>]
    let ``resource checker lets parameters shadow prelude interpolation helpers`` () =
        let mainSource =
            [
                "@allow_unsafe_consume"
                ""
                "module main"
                ""
                "data File : Type ="
                "    Handle Int"
                ""
                "let consume (1 f : File) = unsafeConsume f"
            ]
            |> String.concat "\n"

        let workspace =
            compileInMemoryWorkspaceWithUnsafeConsume
                "memory-resource-parameter-shadow-root"
                [ "main.kp", mainSource ]

        Assert.False(workspace.HasErrors, sprintf "Expected parameter shadowing to win over implicit prelude names, got %A" workspace.Diagnostics)


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
                      StringInterpolation(Literal(LiteralValue.String "test"), None)
                      StringText " and "
                      StringInterpolation(Name [ "value" ], None) ]
                )
              ) -> ()
            | other ->
                failwithf "Unexpected interpolated string body: %A" other
        | other ->
            failwithf "Unexpected declarations: %A" other


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

