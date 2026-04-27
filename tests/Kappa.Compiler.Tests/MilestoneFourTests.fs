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

    let diagnosticText = diagnosticsText workspace.Diagnostics
    Assert.Contains("choose", diagnosticText)
    Assert.Matches("(?i)(multi[- ]shot|resumption|continuation).*(linear|borrow)|(?:linear|borrow).*(multi[- ]shot|resumption|continuation)", diagnosticText)
