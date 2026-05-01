// Covers M3 ownership, using, and cleanup behavior across supported backends.
module MilestoneThreeTestSupport

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
        "expect term openFile : String -> IO File"
        "expect term readData : (& file : File) -> IO String"
        "expect term primitiveCloseFile : File -> IO Unit"
        ""
        "instance Releasable IO File ="
        "    let release f = primitiveCloseFile f"
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
        "expect term openFile : String -> IO File"
        "expect term readData : (& file : File) -> IO String"
        "expect term primitiveCloseFile : File -> IO Unit"
        ""
        "instance Releasable IO File ="
        "    let release f = primitiveCloseFile f"
        ""
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

let private inoutThreadingProgram =
    [
        "module main"
        ""
        "data File : Type ="
        "    Handle Int"
        ""
        "step : (inout file : File) -> IO (1 file : File)"
        "let step file ="
        "    match file"
        "      case Handle n -> pure (file = Handle (n + 1))"
        ""
        "let main : IO Int = do"
        "    let file = Handle 1"
        "    step ~file"
        "    step ~file"
        "    match file"
        "      case Handle n -> pure n"
    ]
    |> String.concat "\n"

let private inoutMonadicResidualProgram =
    [
        "module main"
        ""
        "data File : Type ="
        "    Handle Int"
        ""
        "readStep : (inout file : File) -> IO (data : Int, 1 file : File)"
        "let readStep file ="
        "    match file"
        "      case Handle n -> pure (data = 1, file = Handle (n + 1))"
        ""
        "let main : IO Int = do"
        "    let file = Handle 1"
        "    let (data = first) <- readStep ~file"
        "    let (data = second) <- readStep ~file"
        "    match file"
        "      case Handle n -> pure (first + second + n)"
    ]
    |> String.concat "\n"

let private inoutPureResidualProgram =
    [
        "module main"
        ""
        "data File : Type ="
        "    Handle Int"
        ""
        "step : (inout file : File) -> (data : Int, 1 file : File)"
        "let step file ="
        "    match file"
        "      case Handle n -> (data = 1, file = Handle (n + 1))"
        ""
        "let main : IO Int = do"
        "    let file = Handle 1"
        "    let (data = first) = step ~file"
        "    let (data = second) = step ~file"
        "    match file"
        "      case Handle n -> pure (first + second + n)"
    ]
    |> String.concat "\n"

let private inoutSelectorProjectionProgram =
    [
        "module main"
        ""
        "data Buffer : Type ="
        "    Buffer Int"
        ""
        "type Pair = (left : Buffer, right : Buffer)"
        ""
        "bump : (inout buf : Buffer) -> IO (1 buf : Buffer)"
        "let bump buf ="
        "    match buf"
        "      case Buffer n -> pure (buf = Buffer (n + 1))"
        ""
        "value : Buffer -> Int"
        "let value buf ="
        "    match buf"
        "      case Buffer n -> n"
        ""
        "projection pick (place pair : Pair) (which : Bool) : Buffer ="
        "    if which then yield pair.left else yield pair.right"
        ""
        "let main : IO Int = do"
        "    let pair = (left = Buffer 1, right = Buffer 10)"
        "    bump ~(pick pair True)"
        "    bump ~(pick pair False)"
        "    pure ((value pair.left) + (value pair.right))"
    ]
    |> String.concat "\n"


module MilestoneThreeTestsShard0 =

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
    let ``interpreter threads inout successor values through pure residual binds`` () =
        let workspace =
            compileInMemoryWorkspace
                "memory-m3-inout-pure-residual-root"
                [
                    "main.kp", inoutPureResidualProgram
                ]

        Assert.False(workspace.HasErrors, diagnosticsText workspace.Diagnostics)

        let result, output = executeWithCapturedOutput workspace "main.main"

        Assert.True(String.IsNullOrWhiteSpace(output), output)

        match result with
        | Result.Ok value ->
            Assert.Equal("5", RuntimeValue.format value)
        | Result.Error issue ->
            failwith issue.Message


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
    let ``zig backend executes using release while unwinding protected body`` () =
        let workspace =
            compileInMemoryWorkspaceWithBackend
                "memory-m3-zig-using-unwind-root"
                "zig"
                [
                    "main.kp", usingReleaseThrowingProgram
                ]

        Assert.False(workspace.HasErrors, diagnosticsText workspace.Diagnostics)

        let outputDirectory = createScratchDirectory "zig-m3-using-unwind-backend"

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
        Assert.NotEqual(0, runResult.ExitCode)
        Assert.Equal("chunkclosed", runResult.StandardOutput.Trim())
        Assert.Contains("Non-exhaustive match", runResult.StandardError)

module MilestoneThreeTestsShard1 =

    [<Fact>]
    let ``interpreter threads inout successor values across repeated calls`` () =
        let workspace =
            compileInMemoryWorkspace
                "memory-m3-inout-threading-root"
                [
                    "main.kp", inoutThreadingProgram
                ]

        Assert.False(workspace.HasErrors, diagnosticsText workspace.Diagnostics)

        let result, output = executeWithCapturedOutput workspace "main.main"

        Assert.True(String.IsNullOrWhiteSpace(output), output)

        match result with
        | Result.Ok value ->
            Assert.Equal("3", RuntimeValue.format value)
        | Result.Error issue ->
            failwith issue.Message


    [<Fact>]
    let ``interpreter threads inout successor values through selector projections`` () =
        let workspace =
            compileInMemoryWorkspace
                "memory-m3-inout-selector-projection-root"
                [
                    "main.kp", inoutSelectorProjectionProgram
                ]

        Assert.False(workspace.HasErrors, diagnosticsText workspace.Diagnostics)

        let result, output = executeWithCapturedOutput workspace "main.main"

        Assert.True(String.IsNullOrWhiteSpace(output), output)

        match result with
        | Result.Ok value ->
            Assert.Equal("13", RuntimeValue.format value)
        | Result.Error issue ->
            failwith issue.Message


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


module MilestoneThreeTestsShard2 =

    [<Fact>]
    let ``interpreter threads inout successor values through monadic residual binds`` () =
        let workspace =
            compileInMemoryWorkspace
                "memory-m3-inout-monadic-residual-root"
                [
                    "main.kp", inoutMonadicResidualProgram
                ]

        Assert.False(workspace.HasErrors, diagnosticsText workspace.Diagnostics)

        let result, output = executeWithCapturedOutput workspace "main.main"

        Assert.True(String.IsNullOrWhiteSpace(output), output)

        match result with
        | Result.Ok value ->
            Assert.Equal("5", RuntimeValue.format value)
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
    let ``dotnet backend executes using release while unwinding protected body without KRuntimeIR`` () =
        let workspace =
            compileInMemoryWorkspaceWithBackend
                "memory-m3-dotnet-using-unwind-no-kruntime-root"
                "dotnet"
                [
                    "main.kp", usingReleaseThrowingProgram
                ]

        Assert.False(workspace.HasErrors, diagnosticsText workspace.Diagnostics)

        let outputDirectory = createScratchDirectory "dotnet-m3-using-unwind-no-kruntime-backend"

        let artifact =
            match Backend.emitDotNetArtifact { workspace with KRuntimeIR = [] } "main.main" outputDirectory DotNetDeployment.Managed with
            | Result.Ok artifact -> artifact
            | Result.Error message -> failwith message

        let runResult =
            runProcess outputDirectory "dotnet" $"run --project \"{artifact.ProjectFilePath}\" -c Release"

        Assert.NotEqual(0, runResult.ExitCode)
        Assert.Equal("chunkclosed", runResult.StandardOutput.Trim())
        Assert.Contains("Non-exhaustive match.", runResult.StandardError)

