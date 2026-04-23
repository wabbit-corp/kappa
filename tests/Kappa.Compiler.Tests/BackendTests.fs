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
                    "twice : Int -> Int"
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

    Assert.True(File.Exists(artifact.GeneratedFilePath), $"Expected generated CLR artifact at '{artifact.GeneratedFilePath}'.")
    Assert.True(artifact.GeneratedFilePath.EndsWith(".dll", StringComparison.OrdinalIgnoreCase))

    let programSource = File.ReadAllText(artifact.ProgramFilePath)
    Assert.Contains("Assembly.LoadFrom", programSource)

    let runResult =
        runProcess outputDirectory "dotnet" $"run --project \"{artifact.ProjectFilePath}\" -c Release"

    Assert.Equal(0, runResult.ExitCode)
    Assert.Equal("42", runResult.StandardOutput.Trim())
    Assert.True(String.IsNullOrWhiteSpace(runResult.StandardError), runResult.StandardError)

[<Fact>]
let ``dotnet backend artifact emission does not depend on frontend documents`` () =
    let workspace =
        compileInMemoryWorkspace
            "memory-dotnet-no-documents-root"
            [
                "main.kp",
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
            ]

    let outputDirectory = createScratchDirectory "dotnet-no-documents-backend"

    let artifact =
        match Backend.emitDotNetArtifact { workspace with Documents = [] } "main.result" outputDirectory DotNetDeployment.Managed with
        | Result.Ok artifact -> artifact
        | Result.Error message -> failwith message

    Assert.True(File.Exists(artifact.GeneratedFilePath), $"Expected generated CLR artifact at '{artifact.GeneratedFilePath}'.")
    Assert.True(artifact.GeneratedFilePath.EndsWith(".dll", StringComparison.OrdinalIgnoreCase))

[<Fact>]
let ``dotnet backend execution does not depend on KCore or KRuntimeIR`` () =
    let workspace =
        compileInMemoryWorkspaceWithBackend
            "memory-dotnet-no-kruntime-root"
            "dotnet"
            [
                "main.kp",
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
            ]

    let outputDirectory = createScratchDirectory "dotnet-no-kruntime-backend"

    let artifact =
        match Backend.emitDotNetArtifact { workspace with KCore = []; KRuntimeIR = [] } "main.result" outputDirectory DotNetDeployment.Managed with
        | Result.Ok artifact -> artifact
        | Result.Error message -> failwith message

    let runResult =
        runProcess outputDirectory "dotnet" $"run --project \"{artifact.ProjectFilePath}\" -c Release"

    Assert.Equal(0, runResult.ExitCode)
    Assert.Equal("72", runResult.StandardOutput.Trim())
    Assert.True(String.IsNullOrWhiteSpace(runResult.StandardError), runResult.StandardError)

[<Fact>]
let ``hosted dotnet backend remains available`` () =
    let workspace =
        compileInMemoryWorkspace
            "memory-dotnet-hosted-root"
            [
                "main.kp",
                [
                    "module main"
                    "let result = if not False then 42 else 0"
                ]
                |> String.concat "\n"
            ]

    let outputDirectory = createScratchDirectory "dotnet-hosted-backend"

    let artifact =
        match Backend.emitHostedDotNetArtifact workspace "main.result" outputDirectory DotNetDeployment.Managed with
        | Result.Ok artifact -> artifact
        | Result.Error message -> failwith message

    let runtimeSource = File.ReadAllText(artifact.RuntimeFilePath)
    Assert.Contains("abstract class KExpr", runtimeSource)
    Assert.DoesNotContain("__KAPPA_", runtimeSource)

    let runResult =
        runProcess outputDirectory "dotnet" $"run --project \"{artifact.ProjectFilePath}\" -c Release"

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
                "let result = 40 + 2"
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

[<Fact>]
let ``cli can run the managed dotnet il backend for zero argument bindings`` () =
    let workspaceRoot = createScratchDirectory "cli-dotnet-il-workspace"

    writeWorkspaceFiles
        workspaceRoot
        [
            "main.kp",
            [
                "module main"
                "twice : Int -> Int"
                "let twice value = value * 2"
                "let answer = 40"
                "let result = twice answer + 2"
            ]
            |> String.concat "\n"
        ]

    let emitDirectory = createScratchDirectory "cli-dotnet-il-emit"

    let cliProjectPath =
        Path.GetFullPath(
            Path.Combine(__SOURCE_DIRECTORY__, "..", "..", "src", "Kappa.Compiler.Cli", "Kappa.Compiler.Cli.fsproj")
        )

    let runResult =
        runProcess
            workspaceRoot
            "dotnet"
            $"run --project \"{cliProjectPath}\" -v q -- --source-root \"{workspaceRoot}\" --backend dotnet-il --emit-dir \"{emitDirectory}\" --run main.result"

    Assert.Equal(0, runResult.ExitCode)
    Assert.Equal("82", runResult.StandardOutput.Trim())
    Assert.True(String.IsNullOrWhiteSpace(runResult.StandardError), runResult.StandardError)

[<Fact>]
let ``cli can run the managed dotnet il backend for recursive list matches`` () =
    let workspaceRoot = createScratchDirectory "cli-dotnet-il-list-workspace"

    writeWorkspaceFiles
        workspaceRoot
        [
            "main.kp",
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
        ]

    let emitDirectory = createScratchDirectory "cli-dotnet-il-list-emit"

    let cliProjectPath =
        Path.GetFullPath(
            Path.Combine(__SOURCE_DIRECTORY__, "..", "..", "src", "Kappa.Compiler.Cli", "Kappa.Compiler.Cli.fsproj")
        )

    let runResult =
        runProcess
            workspaceRoot
            "dotnet"
            $"run --project \"{cliProjectPath}\" -v q -- --source-root \"{workspaceRoot}\" --backend dotnet-il --emit-dir \"{emitDirectory}\" --run main.result"

    Assert.Equal(0, runResult.ExitCode)
    Assert.Equal("72", runResult.StandardOutput.Trim())
    Assert.True(String.IsNullOrWhiteSpace(runResult.StandardError), runResult.StandardError)

[<Fact>]
let ``cli interpreter backend can execute io entry points`` () =
    let workspaceRoot = createScratchDirectory "cli-interpreter-workspace"

    writeWorkspaceFiles
        workspaceRoot
        [
            "main.kp",
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
        ]

    let cliProjectPath =
        Path.GetFullPath(
            Path.Combine(__SOURCE_DIRECTORY__, "..", "..", "src", "Kappa.Compiler.Cli", "Kappa.Compiler.Cli.fsproj")
        )

    let runResult =
        runProcess
            workspaceRoot
            "dotnet"
            $"run --project \"{cliProjectPath}\" -v q -- --source-root \"{workspaceRoot}\" --run main.main"

    Assert.Equal(0, runResult.ExitCode)
    Assert.Equal("72", runResult.StandardOutput.Trim())
    Assert.True(String.IsNullOrWhiteSpace(runResult.StandardError), runResult.StandardError)

[<Fact>]
let ``dotnet backend runs the milestone one recursive list program`` () =
    let workspace =
        compileInMemoryWorkspace
            "memory-m1-dotnet-root"
            [
                "main.kp",
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
            ]

    let outputDirectory = createScratchDirectory "dotnet-m1-backend"

    let artifact =
        match Backend.emitDotNetArtifact workspace "main.main" outputDirectory DotNetDeployment.Managed with
        | Result.Ok artifact -> artifact
        | Result.Error message -> failwith message

    let runResult =
        runProcess outputDirectory "dotnet" $"run --project \"{artifact.ProjectFilePath}\" -c Release"

    Assert.True(runResult.ExitCode = 0, runResult.StandardError + runResult.StandardOutput)
    Assert.Equal("72", runResult.StandardOutput.Trim())
    Assert.True(String.IsNullOrWhiteSpace(runResult.StandardError), runResult.StandardError)

[<Fact>]
let ``hosted dotnet backend runs the milestone one recursive list program`` () =
    let workspace =
        compileInMemoryWorkspace
            "memory-m1-dotnet-hosted-root"
            [
                "main.kp",
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
            ]

    let outputDirectory = createScratchDirectory "dotnet-hosted-m1-backend"

    let artifact =
        match Backend.emitHostedDotNetArtifact workspace "main.main" outputDirectory DotNetDeployment.Managed with
        | Result.Ok artifact -> artifact
        | Result.Error message -> failwith message

    let runResult =
        runProcess outputDirectory "dotnet" $"run --project \"{artifact.ProjectFilePath}\" -c Release"

    Assert.Equal(0, runResult.ExitCode)
    Assert.Equal("72", runResult.StandardOutput.Trim())
    Assert.True(String.IsNullOrWhiteSpace(runResult.StandardError), runResult.StandardError)
