// Covers zig backend translation units, native execution, and CLI wiring.
module ZigBackendTests

open System
open System.IO
open Kappa.Compiler
open Harness
open Xunit

[<Fact>]
let ``zig backend emits C source for recursive list matches`` () =
    let workspace =
        compileInMemoryWorkspaceWithBackend
            "memory-zig-source-root"
            "zig"
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

    let outputDirectory = createScratchDirectory "zig-source"

    let artifact =
        match Backend.emitZigArtifact workspace "main.result" outputDirectory with
        | Result.Ok artifact -> artifact
        | Result.Error message -> failwith message

    Assert.True(File.Exists(artifact.SourceFilePath), $"Expected generated native source at '{artifact.SourceFilePath}'.")

    let source = File.ReadAllText(artifact.SourceFilePath)
    Assert.Contains("kappa_make_data", source)
    Assert.Contains("kappa_module_main_sumList", source)
    Assert.Contains("KTYPE_", source)
    Assert.Contains("List", source)
    Assert.DoesNotContain("__KAPPA_", source)

[<Fact>]
let ``cli can run the zig backend for recursive list matches`` () =
    let workspaceRoot = createScratchDirectory "cli-zig-workspace"

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

    let emitDirectory = createScratchDirectory "cli-zig-emit"

    let runResult =
        runBuiltCliWithEnvironment
            workspaceRoot
            $"--source-root \"{workspaceRoot}\" --backend zig --emit-dir \"{emitDirectory}\" --run main.result"
            [ "KAPPA_ZIG_EXE", ensureRepoZigExecutablePath () ]

    Assert.Equal(0, runResult.ExitCode)
    Assert.Equal("72", runResult.StandardOutput.Trim())
    Assert.True(String.IsNullOrWhiteSpace(runResult.StandardError), runResult.StandardError)

[<Fact>]
let ``cli can run the zig backend for the milestone one io entry point`` () =
    let workspaceRoot = createScratchDirectory "cli-zig-m1-workspace"

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

    let emitDirectory = createScratchDirectory "cli-zig-m1-emit"

    let runResult =
        runBuiltCliWithEnvironment
            workspaceRoot
            $"--source-root \"{workspaceRoot}\" --backend zig --emit-dir \"{emitDirectory}\" --run main.main"
            [ "KAPPA_ZIG_EXE", ensureRepoZigExecutablePath () ]

    Assert.Equal(0, runResult.ExitCode)
    Assert.Equal("72", runResult.StandardOutput.Trim())
    Assert.True(String.IsNullOrWhiteSpace(runResult.StandardError), runResult.StandardError)
