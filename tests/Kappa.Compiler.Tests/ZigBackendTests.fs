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
let ``zig backend emits float helpers for float arithmetic`` () =
    let workspace =
        compileInMemoryWorkspaceWithBackend
            "memory-zig-float-source-root"
            "zig"
            [
                "main.kp",
                [
                    "module main"
                    "let result = 1.5 + 2.25"
                ]
                |> String.concat "\n"
            ]

    let outputDirectory = createScratchDirectory "zig-float-source"

    let artifact =
        match Backend.emitZigArtifact workspace "main.result" outputDirectory with
        | Result.Ok artifact -> artifact
        | Result.Error message -> failwith message

    let source = File.ReadAllText(artifact.SourceFilePath)
    Assert.Contains("kappa_float_add", source)

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
let ``zig backend runs constructor or patterns`` () =
    let workspace =
        compileInMemoryWorkspaceWithBackend
            "memory-zig-or-pattern-root"
            "zig"
            [
                "main.kp",
                [
                    "module main"
                    "data Choice : Type ="
                    "    A Int"
                    "    B Int"
                    "unwrap : Choice -> Int"
                    "let unwrap choice ="
                    "    match choice"
                    "    case A value | B value -> value"
                    "let result = unwrap (B 42)"
                ]
                |> String.concat "\n"
            ]

    let outputDirectory = createScratchDirectory "zig-or-pattern"

    let artifact =
        match Backend.emitZigArtifact workspace "main.result" outputDirectory with
        | Result.Ok artifact -> artifact
        | Result.Error message -> failwith message

    let compileResult =
        runProcess
            outputDirectory
            (ensureRepoZigExecutablePath ())
            $"cc -std=c11 -O0 -o \"{artifact.ExecutableFilePath}\" \"{artifact.SourceFilePath}\""

    Assert.Equal(0, compileResult.ExitCode)
    Assert.True(String.IsNullOrWhiteSpace(compileResult.StandardError), compileResult.StandardError)

    let runResult = runProcess outputDirectory artifact.ExecutableFilePath ""
    Assert.Equal(0, runResult.ExitCode)
    Assert.Equal("42", runResult.StandardOutput.Trim())
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

[<Fact>]
let ``cli can run the zig backend with print string intrinsics`` () =
    let workspaceRoot = createScratchDirectory "cli-zig-print-string-workspace"

    writeWorkspaceFiles
        workspaceRoot
        [
            "main.kp",
            [
                "module main"
                "let main : IO Unit = do"
                "    printString \"A\""
                "    printlnString \"B\""
            ]
            |> String.concat "\n"
        ]

    let emitDirectory = createScratchDirectory "cli-zig-print-string-emit"

    let runResult =
        runBuiltCliWithEnvironment
            workspaceRoot
            $"--source-root \"{workspaceRoot}\" --backend zig --emit-dir \"{emitDirectory}\" --run main.main"
            [ "KAPPA_ZIG_EXE", ensureRepoZigExecutablePath () ]

    Assert.Equal(0, runResult.ExitCode)
    Assert.Equal("AB", runResult.StandardOutput.Trim())
    Assert.True(String.IsNullOrWhiteSpace(runResult.StandardError), runResult.StandardError)

[<Fact>]
let ``cli can run the zig backend with float arithmetic`` () =
    let workspaceRoot = createScratchDirectory "cli-zig-float-arithmetic-workspace"

    writeWorkspaceFiles
        workspaceRoot
        [
            "main.kp",
            [
                "module main"
                "let result = 1.5 + 2.25"
            ]
            |> String.concat "\n"
        ]

    let emitDirectory = createScratchDirectory "cli-zig-float-arithmetic-emit"

    let runResult =
        runBuiltCliWithEnvironment
            workspaceRoot
            $"--source-root \"{workspaceRoot}\" --backend zig --emit-dir \"{emitDirectory}\" --run main.result"
            [ "KAPPA_ZIG_EXE", ensureRepoZigExecutablePath () ]

    Assert.Equal(0, runResult.ExitCode)
    Assert.Equal("3.75", runResult.StandardOutput.Trim())
    Assert.True(String.IsNullOrWhiteSpace(runResult.StandardError), runResult.StandardError)

[<Fact>]
let ``cli can run the zig backend with float comparisons`` () =
    let workspaceRoot = createScratchDirectory "cli-zig-float-comparison-workspace"

    writeWorkspaceFiles
        workspaceRoot
        [
            "main.kp",
            [
                "module main"
                "let result = 1.5 < 2.25"
            ]
            |> String.concat "\n"
        ]

    let emitDirectory = createScratchDirectory "cli-zig-float-comparison-emit"

    let runResult =
        runBuiltCliWithEnvironment
            workspaceRoot
            $"--source-root \"{workspaceRoot}\" --backend zig --emit-dir \"{emitDirectory}\" --run main.result"
            [ "KAPPA_ZIG_EXE", ensureRepoZigExecutablePath () ]

    Assert.Equal(0, runResult.ExitCode)
    Assert.Equal("True", runResult.StandardOutput.Trim())
    Assert.True(String.IsNullOrWhiteSpace(runResult.StandardError), runResult.StandardError)

[<Fact>]
let ``cli can run the zig backend with raw bit float equality`` () =
    let workspaceRoot = createScratchDirectory "cli-zig-float-raw-equality-workspace"

    writeWorkspaceFiles
        workspaceRoot
        [
            "main.kp",
            [
                "module main"
                "let result = 0.0 == negate 0.0"
            ]
            |> String.concat "\n"
        ]

    let emitDirectory = createScratchDirectory "cli-zig-float-raw-equality-emit"

    let runResult =
        runBuiltCliWithEnvironment
            workspaceRoot
            $"--source-root \"{workspaceRoot}\" --backend zig --emit-dir \"{emitDirectory}\" --run main.result"
            [ "KAPPA_ZIG_EXE", ensureRepoZigExecutablePath () ]

    Assert.Equal(0, runResult.ExitCode)
    Assert.Equal("False", runResult.StandardOutput.Trim())
    Assert.True(String.IsNullOrWhiteSpace(runResult.StandardError), runResult.StandardError)

[<Fact>]
let ``cli can run the zig backend with short circuit and semantics`` () =
    let workspaceRoot = createScratchDirectory "cli-zig-short-circuit-and-workspace"

    writeWorkspaceFiles
        workspaceRoot
        [
            "main.kp",
            [
                "module main"
                "let result ="
                "    if False && ((1 / 0) == 0)"
                "    then 0"
                "    else 42"
            ]
            |> String.concat "\n"
        ]

    let emitDirectory = createScratchDirectory "cli-zig-short-circuit-and-emit"

    let runResult =
        runBuiltCliWithEnvironment
            workspaceRoot
            $"--source-root \"{workspaceRoot}\" --backend zig --emit-dir \"{emitDirectory}\" --run main.result"
            [ "KAPPA_ZIG_EXE", ensureRepoZigExecutablePath () ]

    Assert.Equal(0, runResult.ExitCode)
    Assert.Equal("42", runResult.StandardOutput.Trim())
    Assert.True(String.IsNullOrWhiteSpace(runResult.StandardError), runResult.StandardError)

[<Fact>]
let ``cli can run the zig backend with short circuit or semantics`` () =
    let workspaceRoot = createScratchDirectory "cli-zig-short-circuit-or-workspace"

    writeWorkspaceFiles
        workspaceRoot
        [
            "main.kp",
            [
                "module main"
                "let result = True || ((1 / 0) == 0)"
            ]
            |> String.concat "\n"
        ]

    let emitDirectory = createScratchDirectory "cli-zig-short-circuit-or-emit"

    let runResult =
        runBuiltCliWithEnvironment
            workspaceRoot
            $"--source-root \"{workspaceRoot}\" --backend zig --emit-dir \"{emitDirectory}\" --run main.result"
            [ "KAPPA_ZIG_EXE", ensureRepoZigExecutablePath () ]

    Assert.Equal(0, runResult.ExitCode)
    Assert.Equal("True", runResult.StandardOutput.Trim())
    Assert.True(String.IsNullOrWhiteSpace(runResult.StandardError), runResult.StandardError)
