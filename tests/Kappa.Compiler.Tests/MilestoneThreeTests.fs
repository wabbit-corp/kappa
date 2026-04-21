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

let private usingReleaseProgram =
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

let private usingReleaseThrowingProgram =
    [
        "module main"
        ""
        "data File : Type ="
        "    Handle Int"
        ""
        "data Crash : Type ="
        "    Nope"
        "    Yep Int"
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
        "crash : Crash -> IO Unit"
        "let crash value ="
        "    match value"
        "    case Yep payload -> printString \"never\""
        ""
        "let main : IO Unit = do"
        "    using file <- openFile \"data.txt\""
        "    let chunk = !(readData file)"
        "    printString chunk"
        "    crash Nope"
    ]
    |> String.concat "\n"

[<Fact>]
let ``interpreter executes using release after protected body`` () =
    let workspace =
        compileInMemoryWorkspace
            "memory-m3-interpreter-using-root"
            [
                "main.kp", usingReleaseProgram
            ]

    Assert.False(workspace.HasErrors, diagnosticsText workspace.Diagnostics)

    let result, output = executeWithCapturedOutput workspace "main.main"

    match result with
    | Result.Ok value ->
        Assert.Equal("()", RuntimeValue.format value)
        Assert.Equal("chunkclosed", output)
    | Result.Error issue ->
        failwith issue.Message

[<Fact>]
let ``zig backend executes using release after protected body`` () =
    let workspace =
        compileInMemoryWorkspaceWithBackend
            "memory-m3-zig-using-root"
            "zig"
            [
                "main.kp", usingReleaseProgram
            ]

    Assert.False(workspace.HasErrors, diagnosticsText workspace.Diagnostics)

    let outputDirectory = createScratchDirectory "zig-m3-using-backend"

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
    Assert.Equal("chunkclosed", runResult.StandardOutput.Trim())
    Assert.True(String.IsNullOrWhiteSpace(runResult.StandardError), runResult.StandardError)

[<Fact>]
let ``dotnet backend executes using release after protected body`` () =
    let workspace =
        compileInMemoryWorkspaceWithBackend
            "memory-m3-dotnet-using-root"
            "dotnet"
            [
                "main.kp", usingReleaseProgram
            ]

    Assert.False(workspace.HasErrors, diagnosticsText workspace.Diagnostics)

    let outputDirectory = createScratchDirectory "dotnet-m3-using-backend"

    let artifact =
        match Backend.emitDotNetArtifact workspace "main.main" outputDirectory DotNetDeployment.Managed with
        | Result.Ok artifact -> artifact
        | Result.Error message -> failwith message

    let runResult =
        runProcess outputDirectory "dotnet" $"run --project \"{artifact.ProjectFilePath}\" -c Release"

    Assert.True(runResult.ExitCode = 0, runResult.StandardError + runResult.StandardOutput)
    Assert.Equal("chunkclosed", runResult.StandardOutput.Trim())
    Assert.True(String.IsNullOrWhiteSpace(runResult.StandardError), runResult.StandardError)

[<Fact>]
let ``dotnet backend executes using release while unwinding protected body`` () =
    let workspace =
        compileInMemoryWorkspaceWithBackend
            "memory-m3-dotnet-using-unwind-root"
            "dotnet"
            [
                "main.kp", usingReleaseThrowingProgram
            ]

    Assert.False(workspace.HasErrors, diagnosticsText workspace.Diagnostics)

    let outputDirectory = createScratchDirectory "dotnet-m3-using-unwind-backend"

    let artifact =
        match Backend.emitDotNetArtifact workspace "main.main" outputDirectory DotNetDeployment.Managed with
        | Result.Ok artifact -> artifact
        | Result.Error message -> failwith message

    let runResult =
        runProcess outputDirectory "dotnet" $"run --project \"{artifact.ProjectFilePath}\" -c Release"

    Assert.NotEqual(0, runResult.ExitCode)
    Assert.Equal("chunkclosed", runResult.StandardOutput.Trim())
    Assert.Contains("Non-exhaustive match.", runResult.StandardError)
