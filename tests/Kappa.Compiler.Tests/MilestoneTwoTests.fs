// Covers M2 traits, refs, and while-loop behavior across supported backends.
module MilestoneTwoTests

open System
open System.IO
open System.Text
open Kappa.Compiler
open Harness
open Xunit

let private milestoneTwoProgram =
    [
        "module main"
        "trait Show a ="
        "    show : a -> String"
        "instance Show Int ="
        "    let show x = primitiveIntToString x"
        "printShowable : forall a. Show a => a -> IO Unit"
        "let printShowable x = printString (show x)"
        "let main : IO Unit = do"
        "    var counter = 0"
        "    while !(readRef counter) < 3 do"
        "        let current = !(readRef counter)"
        "        printShowable current"
        "        counter = current + 1"
    ]
    |> String.concat "\n"

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
let ``interpreter executes the milestone two trait and ref program`` () =
    let workspace =
        compileInMemoryWorkspace
            "memory-m2-interpreter-root"
            [
                "main.kp", milestoneTwoProgram
            ]

    Assert.False(workspace.HasErrors, diagnosticsText workspace.Diagnostics)

    let result, output = executeWithCapturedOutput workspace "main.main"

    match result with
    | Result.Ok value ->
        Assert.Equal("()", RuntimeValue.format value)
        Assert.Equal("012", output)
    | Result.Error issue ->
        failwith issue.Message

[<Fact>]
let ``zig backend runs the milestone two trait and ref program`` () =
    let workspace =
        compileInMemoryWorkspaceWithBackend
            "memory-m2-zig-root"
            "zig"
            [
                "main.kp", milestoneTwoProgram
            ]

    Assert.False(workspace.HasErrors, diagnosticsText workspace.Diagnostics)

    let outputDirectory = createScratchDirectory "zig-m2-backend"

    let artifact =
        match Backend.emitZigArtifact workspace "main.main" outputDirectory with
        | Result.Ok artifact -> artifact
        | Result.Error message -> failwith message

    let compileResult =
        runProcessWithEnvironment
            artifact.OutputDirectory
            (ensureRepoZigExecutablePath ())
            $"cc -std=c11 -O0 -o \"{artifact.ExecutableFilePath}\" \"{artifact.SourceFilePath}\""
            []

    Assert.Equal(0, compileResult.ExitCode)

    let runResult = runProcess artifact.OutputDirectory artifact.ExecutableFilePath ""
    Assert.True(runResult.ExitCode = 0, runResult.StandardError + runResult.StandardOutput)
    Assert.Equal("012", runResult.StandardOutput.Trim())
    Assert.True(String.IsNullOrWhiteSpace(runResult.StandardError), runResult.StandardError)

[<Fact>]
let ``dotnet backend runs the milestone two trait and ref program`` () =
    let workspace =
        compileInMemoryWorkspaceWithBackend
            "memory-m2-dotnet-root"
            "dotnet"
            [
                "main.kp", milestoneTwoProgram
            ]

    Assert.False(workspace.HasErrors, diagnosticsText workspace.Diagnostics)

    let outputDirectory = createScratchDirectory "dotnet-m2-backend"

    let artifact =
        match Backend.emitDotNetArtifact workspace "main.main" outputDirectory DotNetDeployment.Managed with
        | Result.Ok artifact -> artifact
        | Result.Error message -> failwith message

    let runResult =
        runProcess outputDirectory "dotnet" $"run --project \"{artifact.ProjectFilePath}\" -c Release"

    Assert.True(runResult.ExitCode = 0, runResult.StandardError + runResult.StandardOutput)
    Assert.Equal("012", runResult.StandardOutput.Trim())
    Assert.True(String.IsNullOrWhiteSpace(runResult.StandardError), runResult.StandardError)
