// Covers the first M4 slice: effect operations, one-shot deep handlers, and multishot capture safety.
module MilestoneFourTests

open System
open System.IO
open Kappa.Compiler
open Harness
open Xunit

let private diagnosticsText diagnostics =
    diagnostics
    |> List.map (fun diagnostic -> $"{DiagnosticCode.toIdentifier diagnostic.Code}: {diagnostic.Message}")
    |> String.concat Environment.NewLine

[<Fact>]
let ``interpreter executes a one shot deep handler over a scoped effect`` () =
    let mainSource =
        [
            "module main"
            ""
            "result : Int"
            "let result : Int ="
            "    block"
            "        scoped effect Ask ="
            "            ask : Unit -> Bool"
            ""
            "        let comp : Eff <[Ask : Ask]> Int ="
            "            do"
            "                let b <- Ask.ask ()"
            "                if b then 1 else 0"
            ""
            "        let handled : Eff <[ ]> Int ="
            "            deep handle Ask comp with"
            "                case return x -> pure x"
            "                case ask () k -> k True"
            ""
            "        runPure handled"
        ]
        |> String.concat "\n"

    let workspace, result =
        evaluateInMemoryBinding
            "memory-m4-deep-state-root"
            "main.result"
            [ "main.kp", mainSource ]

    Assert.False(workspace.HasErrors, sprintf "Expected no diagnostics, got:%s%s" Environment.NewLine (diagnosticsText workspace.Diagnostics))

    match result with
    | Result.Ok value ->
        Assert.Equal("1", RuntimeValue.format value)
    | Result.Error issue ->
        failwithf "Expected deep handler evaluation to succeed, got %s" issue.Message

[<Fact>]
let ``shallow handler resumptions remain in the unhandled effect carrier`` () =
    let mainSource =
        [
            "module main"
            ""
            "result : Int"
            "let result : Int ="
            "    block"
            "        scoped effect Ask ="
            "            ask : Unit -> Bool"
            ""
            "        let comp : Eff <[Ask : Ask]> Int ="
            "            do"
            "                let b <- Ask.ask ()"
            "                if b then 1 else 0"
            ""
            "        let rehandle : (1 k : ((1 x : Bool) -> Eff <[Ask : Ask]> Int)) -> Eff <[ ]> Int ="
            "            \\(1 k : ((1 x : Bool) -> Eff <[Ask : Ask]> Int)) ->"
            "                handle Ask (k True) with"
            "                    case return y -> pure y"
            "                    case ask () k2 -> k2 False"
            ""
            "        let handled : Eff <[ ]> Int ="
            "            handle Ask comp with"
            "                case return y -> pure y"
            "                case ask () k -> rehandle k"
            ""
            "        runPure handled"
        ]
        |> String.concat "\n"

    let workspace, result =
        evaluateInMemoryBinding
            "memory-m4-shallow-rehandle-root"
            "main.result"
            [ "main.kp", mainSource ]

    Assert.False(workspace.HasErrors, sprintf "Expected no diagnostics, got:%s%s" Environment.NewLine (diagnosticsText workspace.Diagnostics))

    match result with
    | Result.Ok value ->
        Assert.Equal("1", RuntimeValue.format value)
    | Result.Error issue ->
        failwithf "Expected shallow handler rehandle evaluation to succeed, got %s" issue.Message

[<Fact>]
let ``multishot scoped effect invocation rejects captured linear suffix at the operation site`` () =
    let fixturePath =
        Path.Combine(
            __SOURCE_DIRECTORY__,
            "Fixtures",
            "borrow_qtt.100_interactions.handler_reject_multishot_resumption_captures_linear_suffix",
            "main.kp"
        )

    let mainSource = File.ReadAllText fixturePath

    let workspace =
        compileInMemoryWorkspace
            "memory-m4-multishot-linear-root"
            [ "main.kp", mainSource ]

    Assert.True(workspace.HasErrors, "Expected the multishot capture rule to reject the operation site.")
    Assert.Contains(workspace.Diagnostics, fun diagnostic -> diagnostic.Code = DiagnosticCode.QttContinuationCapture)
    Assert.Contains(workspace.Diagnostics, fun diagnostic -> diagnostic.Code = DiagnosticCode.MultishotEffectUnsupportedBackend)

[<Fact>]
let ``backends without multishot capability reject direct multishot invocations`` () =
    let mainSource =
        [
            "@PrivateByDefault module main"
            ""
            "result : Eff <[Choice : Choice]> Bool"
            "let result ="
            "    block"
            "        scoped effect Choice ="
            "            ω choose : Unit -> Bool"
            ""
            "        Choice.choose ()"
        ]
        |> String.concat "\n"

    let workspace =
        compileInMemoryWorkspaceWithBackend
            "memory-m4-multishot-capability-root"
            "dotnet-il"
            [ "main.kp", mainSource ]

    Assert.True(workspace.HasErrors, "Expected backends without rt-multishot-effects to reject direct multishot invocation.")

    let diagnostic =
        workspace.Diagnostics
        |> List.tryFind (fun current -> current.Code = DiagnosticCode.MultishotEffectUnsupportedBackend)

    Assert.True(diagnostic.IsSome, "Expected a multishot backend-capability diagnostic.")
    Assert.Contains("Choice.choose", diagnostic.Value.Message)
    Assert.Contains("dotnet-il", diagnostic.Value.Message)

[<Fact>]
let ``backends without multishot capability reject exported declarations that may invoke multishot effects`` () =
    let mainSource =
        [
            "module main"
            ""
            "result : Eff <[Choice : Choice]> Bool"
            "let result ="
            "    block"
            "        scoped effect Choice ="
            "            ω choose : Unit -> Bool"
            ""
            "        Choice.choose ()"
        ]
        |> String.concat "\n"

    let workspace =
        compileInMemoryWorkspaceWithBackend
            "memory-m4-multishot-export-root"
            "zig"
            [ "main.kp", mainSource ]

    Assert.True(workspace.HasErrors, "Expected exported multishot definitions to be rejected without backend capability.")

    let exportedDiagnostic =
        workspace.Diagnostics
        |> List.tryFind (fun diagnostic ->
            diagnostic.Code = DiagnosticCode.MultishotEffectUnsupportedBackend
            && diagnostic.Message.Contains("exported declaration 'result'", StringComparison.Ordinal))

    Assert.True(exportedDiagnostic.IsSome, "Expected an exported-declaration multishot backend-capability diagnostic.")
    Assert.Contains("Choice.choose", exportedDiagnostic.Value.Message)
    Assert.Contains("zig", exportedDiagnostic.Value.Message)

[<Fact>]
let ``shallow handler resumptions do not collapse into the handled carrier`` () =
    let mainSource =
        [
            "@PrivateByDefault module main"
            ""
            "bad : Eff <[ ]> Int"
            "let bad : Eff <[ ]> Int ="
            "    block"
            "        scoped effect Ask ="
            "            ask : Unit -> Bool"
            ""
            "        let comp : Eff <[Ask : Ask]> Int ="
            "            do"
            "                let b <- Ask.ask ()"
            "                if b then 1 else 0"
            ""
            "        let acceptHandled : (1 k : ((1 x : Bool) -> Eff <[ ]> Int)) -> Eff <[ ]> Int ="
            "            \\(1 k : ((1 x : Bool) -> Eff <[ ]> Int)) -> k True"
            ""
            "        handle Ask comp with"
            "            case return y -> pure y"
            "            case ask () k ->"
            "                acceptHandled k"
        ]
        |> String.concat "\n"

    let workspace =
        compileInMemoryWorkspace
            "memory-m4-shallow-carrier-root"
            [ "main.kp", mainSource ]

    Assert.True(workspace.HasErrors, "Expected shallow handler resumptions with the handled carrier to be rejected.")
    Assert.Contains(workspace.Diagnostics, fun diagnostic -> diagnostic.Code = DiagnosticCode.TypeEqualityMismatch)

[<Fact>]
let ``one shot resumptions cannot be resumed twice`` () =
    let fixturePath =
        Path.Combine(
            __SOURCE_DIRECTORY__,
            "Fixtures",
            "borrow_qtt.100_interactions.handler_reject_oneshot_resumption_overuse",
            "main.kp"
        )

    let mainSource = File.ReadAllText fixturePath

    let workspace =
        compileInMemoryWorkspace
            "memory-m4-oneshot-overuse-root"
            [ "main.kp", mainSource ]

    Assert.True(workspace.HasErrors, "Expected one-shot resumption overuse to be rejected.")
    Assert.Contains(workspace.Diagnostics, fun diagnostic -> diagnostic.Code = DiagnosticCode.QttLinearOveruse)
