module MilestoneThreeTests

open System
open System.Text
open Kappa.Compiler
open Harness
open Xunit

let private diagnosticsText diagnostics =
    diagnostics
    |> List.map (fun diagnostic -> diagnostic.Message)
    |> String.concat Environment.NewLine

let private executeWithCapturedOutput workspace entryPoint =
    let builder = StringBuilder()

    let output: RuntimeOutput =
        { Write = fun text -> builder.Append(text) |> ignore
          WriteLine = fun text -> builder.AppendLine(text) |> ignore }

    let result = Interpreter.executeBindingWithOutput workspace output entryPoint

    result, builder.ToString().Replace("\r\n", "\n").TrimEnd([| '\r'; '\n' |])

[<Fact>]
let ``interpreter executes using release after protected body`` () =
    let source =
        [
            "module main"
            ""
            "data File : Type ="
            "    Handle Int"
            ""
            "trait Releasable m a ="
            "    release : a -> IO Unit"
            ""
            "instance Releasable IO File ="
            "    let release f = printString \"closed\""
            ""
            "openFile : String -> IO File"
            "let openFile name = pure (Handle 1)"
            "let readData (& file : File) = pure \"chunk\""
            ""
            "let main : IO Unit = do"
            "    using file <- openFile \"data.txt\""
            "    let chunk = !(readData file)"
            "    printString chunk"
        ]
        |> String.concat "\n"

    let workspace =
        compileInMemoryWorkspace
            "memory-m3-interpreter-using-root"
            [
                "main.kp", source
            ]

    Assert.False(workspace.HasErrors, diagnosticsText workspace.Diagnostics)

    let result, output = executeWithCapturedOutput workspace "main.main"

    match result with
    | Result.Ok value ->
        Assert.Equal("()", RuntimeValue.format value)
        Assert.Equal("chunkclosed", output)
    | Result.Error issue ->
        failwith issue.Message
