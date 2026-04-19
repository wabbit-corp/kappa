module BackendTests

open System
open System.IO
open Kappa.Compiler
open Harness
open Xunit

[<Fact>]
let ``dotnet backend emits a managed project that runs`` () =
    let workspace =
        compileInMemoryWorkspace
            "memory-dotnet-managed-root"
            [
                "math.kp",
                [
                    "module math"
                    "let twice x = x * 2"
                ]
                |> String.concat "\n"
                "main.kp",
                [
                    "module main"
                    "import math.*"
                    "let result = twice 21"
                ]
                |> String.concat "\n"
            ]

    let outputDirectory = createScratchDirectory "dotnet-managed-backend"

    let artifact =
        match Backend.emitDotNetArtifact workspace "main.result" outputDirectory DotNetDeployment.Managed with
        | Result.Ok artifact -> artifact
        | Result.Error message -> failwith message

    let runResult =
        runProcess outputDirectory "dotnet" $"run --project \"{artifact.ProjectFilePath}\" -c Release"

    Assert.Equal(0, runResult.ExitCode)
    Assert.Equal("42", runResult.StandardOutput.Trim())
    Assert.True(String.IsNullOrWhiteSpace(runResult.StandardError), runResult.StandardError)

[<Fact>]
let ``dotnet backend native aot publish runs`` () =
    let workspace =
        compileInMemoryWorkspace
            "memory-dotnet-native-root"
            [
                "main.kp",
                [
                    "module main"
                    "let result = if not False then 42 else 0"
                ]
                |> String.concat "\n"
            ]

    let outputDirectory = createScratchDirectory "dotnet-native-backend"

    let artifact =
        match Backend.emitDotNetArtifact workspace "main.result" outputDirectory DotNetDeployment.NativeAot with
        | Result.Ok artifact -> artifact
        | Result.Error message -> failwith message

    let rid = currentRid ()

    let publishResult =
        runProcess outputDirectory "dotnet" $"publish \"{artifact.ProjectFilePath}\" -c Release -r {rid}"

    Assert.Equal(0, publishResult.ExitCode)

    let publishDirectory =
        Path.Combine(outputDirectory, "bin", "Release", "net10.0", rid, "publish")

    let executableName =
        Path.GetFileNameWithoutExtension(artifact.ProjectFilePath)

    let executable =
        executablePath publishDirectory executableName

    Assert.True(File.Exists(executable), $"Expected published executable at '{executable}'.")

    let runResult =
        runProcess publishDirectory executable ""

    Assert.Equal(0, runResult.ExitCode)
    Assert.Equal("42", runResult.StandardOutput.Trim())
    Assert.True(String.IsNullOrWhiteSpace(runResult.StandardError), runResult.StandardError)

[<Fact>]
let ``cli can run the managed dotnet backend`` () =
    let workspaceRoot = createScratchDirectory "cli-dotnet-workspace"

    writeWorkspaceFiles
        workspaceRoot
        [
            "main.kp",
            [
                "module main"
                "let result = if not False then 42 else 0"
            ]
            |> String.concat "\n"
        ]

    let emitDirectory = createScratchDirectory "cli-dotnet-emit"

    let cliProjectPath =
        Path.GetFullPath(
            Path.Combine(__SOURCE_DIRECTORY__, "..", "..", "src", "Kappa.Compiler.Cli", "Kappa.Compiler.Cli.fsproj")
        )

    let runResult =
        runProcess
            workspaceRoot
            "dotnet"
            $"run --project \"{cliProjectPath}\" -v q -- --source-root \"{workspaceRoot}\" --backend dotnet --emit-dir \"{emitDirectory}\" --run main.result"

    Assert.Equal(0, runResult.ExitCode)
    Assert.Contains("42", runResult.StandardOutput)
    Assert.True(String.IsNullOrWhiteSpace(runResult.StandardError), runResult.StandardError)
