// Covers public backend artifact emission and CLI execution paths.
module BackendTestSupport

open System
open System.IO
open System.Reflection
open System.Text.Json
open Kappa.Compiler
open Harness
open Xunit


module BackendTestsShard0 =

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
    let ``effectful dotnet backend execution does not depend on KCore or KRuntimeIR`` () =
        let workspace =
            compileInMemoryWorkspaceWithBackend
                "memory-dotnet-effectful-no-kruntime-root"
                "dotnet"
                [
                    "main.kp",
                    [
                        "@PrivateByDefault module main"
                        ""
                        "result : Int"
                        "let result ="
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
                ]

        let outputDirectory = createScratchDirectory "dotnet-effectful-no-kruntime-backend"

        let artifact =
            match Backend.emitDotNetArtifact { workspace with KCore = []; KRuntimeIR = [] } "main.result" outputDirectory DotNetDeployment.Managed with
            | Result.Ok artifact -> artifact
            | Result.Error message -> failwith message

        let runResult =
            runProcess outputDirectory "dotnet" $"run --project \"{artifact.ProjectFilePath}\" -c Release"

        Assert.Equal(0, runResult.ExitCode)
        Assert.Equal("1", runResult.StandardOutput.Trim())
        Assert.True(String.IsNullOrWhiteSpace(runResult.StandardError), runResult.StandardError)


    [<Fact>]
    let ``cli rejects native aot for the managed dotnet backend before emission`` () =
        let workspaceRoot = createScratchDirectory "cli-dotnet-native-aot-workspace"

        writeWorkspaceFiles
            workspaceRoot
            [
                "main.kp",
                [
                    "module main"
                    "let result = 42"
                ]
                |> String.concat "\n"
            ]

        let emitDirectory = createScratchDirectory "cli-dotnet-native-aot-emit"

        let runResult =
            runBuiltCli
                workspaceRoot
                $"--source-root \"{workspaceRoot}\" --backend dotnet --emit-dir \"{emitDirectory}\" --native-aot --run main.result"

        Assert.Equal(1, runResult.ExitCode)
        Assert.Contains(
            "--native-aot is not supported for backend dotnet.",
            runResult.StandardError,
            StringComparison.Ordinal
        )
        Assert.True(String.IsNullOrWhiteSpace(runResult.StandardOutput), runResult.StandardOutput)
        Assert.False(File.Exists(Path.Combine(emitDirectory, "Kappa.Generated.Runner.csproj")))
        Assert.False(File.Exists(Path.Combine(emitDirectory, "Program.cs")))


module BackendTestsShard1 =

    [<Fact>]
    let ``dotnet backend preserves Bool results in parameterized bindings`` () =
        let workspace =
            compileInMemoryWorkspaceWithBackend
                "memory-dotnet-bool-binding-root"
                "dotnet"
                [
                    "main.kp",
                    [
                        "module main"
                        "alwaysTrue : Int -> Bool"
                        "let alwaysTrue _ = True"
                        "let result = if alwaysTrue 0 then 1 else 0"
                    ]
                    |> String.concat "\n"
                ]

        Assert.False(workspace.HasErrors, sprintf "Expected no diagnostics, got %A" workspace.Diagnostics)

        let outputDirectory = createScratchDirectory "dotnet-bool-binding-backend"

        let artifact =
            match Backend.emitDotNetArtifact workspace "main.result" outputDirectory DotNetDeployment.Managed with
            | Result.Ok artifact -> artifact
            | Result.Error message -> failwith message

        let runResult =
            runProcess outputDirectory "dotnet" $"run --project \"{artifact.ProjectFilePath}\" -c Release"

        Assert.Equal(0, runResult.ExitCode)
        Assert.Equal("1", runResult.StandardOutput.Trim())
        Assert.True(String.IsNullOrWhiteSpace(runResult.StandardError), runResult.StandardError)

    [<Fact>]
    let ``dotnet backend runs bundled inequality helper via builtin Eq evidence`` () =
        let workspace =
            compileInMemoryWorkspaceWithBackend
                "memory-dotnet-neq-root"
                "dotnet"
                [
                    "main.kp",
                    [
                        "module main"
                        "let result = if True /= False then 1 else 0"
                    ]
                    |> String.concat "\n"
                ]

        Assert.False(workspace.HasErrors, sprintf "Expected no diagnostics, got %A" workspace.Diagnostics)

        let outputDirectory = createScratchDirectory "dotnet-neq-backend"

        let artifact =
            match Backend.emitDotNetArtifact workspace "main.result" outputDirectory DotNetDeployment.Managed with
            | Result.Ok artifact -> artifact
            | Result.Error message -> failwith message

        let runResult =
            runProcess outputDirectory "dotnet" $"run --project \"{artifact.ProjectFilePath}\" -c Release"

        Assert.Equal(0, runResult.ExitCode)
        Assert.Equal("1", runResult.StandardOutput.Trim())
        Assert.True(String.IsNullOrWhiteSpace(runResult.StandardError), runResult.StandardError)

    [<Fact>]
    let ``dotnet backend emits enum-like data types as CLR enums`` () =
        let workspace =
            compileInMemoryWorkspaceWithBackend
                "memory-dotnet-enum-like-data-root"
                "dotnet"
                [
                    "main.kp",
                    [
                        "module main"
                        "data Color : Type ="
                        "    Red"
                        "    Green"
                        "    Blue"
                        "let result : Color = Blue"
                    ]
                    |> String.concat "\n"
                ]

        Assert.False(workspace.HasErrors, sprintf "Expected no diagnostics, got %A" workspace.Diagnostics)

        let outputDirectory = createScratchDirectory "dotnet-enum-like-data-backend"

        let artifact =
            match Backend.emitDotNetArtifact workspace "main.result" outputDirectory DotNetDeployment.Managed with
            | Result.Ok artifact -> artifact
            | Result.Error message -> failwith message

        let generatedAssembly = Assembly.LoadFrom(artifact.GeneratedFilePath)
        let colorType = generatedAssembly.GetType("Kappa.Generated.main.Color", throwOnError = true, ignoreCase = false)

        Assert.True(colorType.IsEnum, $"Expected generated type '{colorType.FullName}' to be a CLR enum.")

        let caseNames =
            colorType.GetFields(BindingFlags.Public ||| BindingFlags.Static)
            |> Array.map _.Name
            |> Array.sort

        Assert.Equal<string>([| "Blue"; "Green"; "Red" |], caseNames)
        Assert.Null(generatedAssembly.GetType("Kappa.Generated.main.Red", throwOnError = false, ignoreCase = false))

    [<Fact>]
    let ``dotnet backend runs builtin compare through CLR Ordering enum`` () =
        let workspace =
            compileInMemoryWorkspaceWithBackend
                "memory-dotnet-ordering-enum-root"
                "dotnet"
                [
                    "main.kp",
                    [
                        "module main"
                        "let result = if compare 1 2 == LT then 1 else 0"
                    ]
                    |> String.concat "\n"
                ]

        Assert.False(workspace.HasErrors, sprintf "Expected no diagnostics, got %A" workspace.Diagnostics)

        let outputDirectory = createScratchDirectory "dotnet-ordering-enum-backend"

        let artifact =
            match Backend.emitDotNetArtifact workspace "main.result" outputDirectory DotNetDeployment.Managed with
            | Result.Ok artifact -> artifact
            | Result.Error message -> failwith message

        let runResult =
            runProcess outputDirectory "dotnet" $"run --project \"{artifact.ProjectFilePath}\" -c Release"

        Assert.Equal(0, runResult.ExitCode)
        Assert.Equal("1", runResult.StandardOutput.Trim())
        Assert.True(String.IsNullOrWhiteSpace(runResult.StandardError), runResult.StandardError)


    [<Fact>]
    let ``dotnet backend executes constructor or patterns without KRuntimeIR`` () =
        let workspace =
            compileInMemoryWorkspaceWithBackend
                "memory-dotnet-or-pattern-no-kruntime-root"
                "dotnet"
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

        let outputDirectory = createScratchDirectory "dotnet-or-pattern-no-kruntime"

        let artifact =
            match Backend.emitDotNetArtifact { workspace with KCore = []; KRuntimeIR = [] } "main.result" outputDirectory DotNetDeployment.Managed with
            | Result.Ok artifact -> artifact
            | Result.Error message -> failwith message

        let runResult =
            runProcess outputDirectory "dotnet" $"run --project \"{artifact.ProjectFilePath}\" -c Release"

        Assert.Equal(0, runResult.ExitCode)
        Assert.Equal("42", runResult.StandardOutput.Trim())
        Assert.True(String.IsNullOrWhiteSpace(runResult.StandardError), runResult.StandardError)


    [<Fact>]
    let ``cli can run the dotnet backend for zero argument bindings with call heavy bodies`` () =
        let workspaceRoot = createScratchDirectory "cli-dotnet-workspace-calls"

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

        let emitDirectory = createScratchDirectory "cli-dotnet-emit-calls"

        let runResult =
            runBuiltCli
                workspaceRoot
                $"--source-root \"{workspaceRoot}\" --backend dotnet --emit-dir \"{emitDirectory}\" --run main.result"

        Assert.Equal(0, runResult.ExitCode)
        Assert.Equal("82", runResult.StandardOutput.Trim())
        Assert.True(String.IsNullOrWhiteSpace(runResult.StandardError), runResult.StandardError)


module BackendTestsShard2 =

    [<Fact>]
    let ``dotnet backend runner copies host dotnet dependency assemblies`` () =
        let workspace =
            compileInMemoryWorkspaceWithBackend
                "memory-dotnet-host-runner-root"
                "dotnet"
                [
                    "main.kp",
                    [
                        "module main"
                        "import host.dotnet.Kappa.Compiler.TestHost.Sample.(term new, term Echo, term Create)"
                        "result : String"
                        "let result ="
                        "    let fromCtor = new \"wrapped\""
                        "    let _ = Echo fromCtor ()"
                        "    let fromStatic = Create \"ok\""
                        "    Echo fromStatic ()"
                    ]
                    |> String.concat "\n"
                ]

        Assert.False(workspace.HasErrors, sprintf "Expected host.dotnet wrappers to compile, got %A" workspace.Diagnostics)

        let outputDirectory = createScratchDirectory "dotnet-host-runner"

        let artifact =
            match Backend.emitDotNetArtifact workspace "main.result" outputDirectory DotNetDeployment.Managed with
            | Result.Ok artifact -> artifact
            | Result.Error message -> failwith message

        let expectedHostAssemblyPath = typeof<Kappa.Compiler.TestHost.Sample>.Assembly.Location
        let expectedHostAssemblyFileName = Path.GetFileName(expectedHostAssemblyPath)
        let copiedHostAssemblyPath = Path.Combine(outputDirectory, expectedHostAssemblyFileName)

        Assert.True(File.Exists(copiedHostAssemblyPath), $"Expected copied host dependency assembly at '{copiedHostAssemblyPath}'.")

        let runResult =
            runProcess outputDirectory "dotnet" $"run --project \"{artifact.ProjectFilePath}\" -c Release"

        Assert.Equal(0, runResult.ExitCode)
        Assert.Equal("\"ok\"", runResult.StandardOutput.Trim())
        Assert.True(String.IsNullOrWhiteSpace(runResult.StandardError), runResult.StandardError)


    [<Fact>]
    let ``dotnet backend rejects or patterns whose shared binders have different types`` () =
        let workspace =
            compileInMemoryWorkspaceWithBackend
                "memory-dotnet-or-pattern-mismatch-root"
                "dotnet"
                [
                    "main.kp",
                    [
                        "module main"
                        "data Choice : Type ="
                        "    A Int"
                        "    B String"
                        "probe : Choice -> Unit"
                        "let probe choice ="
                        "    match choice"
                        "    case A value | B value -> ()"
                    ]
                    |> String.concat "\n"
                ]

        Assert.True(workspace.HasErrors, "Expected mismatched or-pattern binder representations to be rejected.")
        Assert.Contains(
            workspace.Diagnostics,
            fun diagnostic ->
                diagnostic.Code = DiagnosticCode.OrPatternBinderTypeMismatch
                && diagnostic.Message.IndexOf("Binder 'value' has type", StringComparison.OrdinalIgnoreCase) >= 0
        )


    [<Fact>]
    let ``cli can run the dotnet backend for recursive list matches`` () =
        let workspaceRoot = createScratchDirectory "cli-dotnet-list-workspace"

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

        let emitDirectory = createScratchDirectory "cli-dotnet-list-emit"

        let runResult =
            runBuiltCli
                workspaceRoot
                $"--source-root \"{workspaceRoot}\" --backend dotnet --emit-dir \"{emitDirectory}\" --run main.result"

        Assert.Equal(0, runResult.ExitCode)
        Assert.Equal("72", runResult.StandardOutput.Trim())
        Assert.True(String.IsNullOrWhiteSpace(runResult.StandardError), runResult.StandardError)


module BackendTestsShard3 =

    [<Fact>]
    let ``dotnet backend runs constructor tag tests`` () =
        let workspace =
            compileInMemoryWorkspaceWithBackend
                "memory-dotnet-is-root"
                "dotnet"
                [
                    "main.kp",
                    [
                        "module main"
                        "data Payload : Type ="
                        "    Box Int"
                        "    NatBox Int"
                        "let result = if Box 42 is Box then 42 else 0"
                    ]
                    |> String.concat "\n"
                ]

        let outputDirectory = createScratchDirectory "dotnet-is-backend"

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
    let ``dotnet backend resolves entrypoints from the emitted CLR model`` () =
        let workspace =
            compileInMemoryWorkspaceWithBackend
                "memory-dotnet-entrypoint-clr-model-root"
                "dotnet"
                [
                    "main.kp",
                    [
                        "module main"
                        "let result = 42"
                    ]
                    |> String.concat "\n"
                ]

        let driftedBackendIR =
            workspace.KBackendIR
            |> List.map (fun moduleDump ->
                if moduleDump.Name = "main" then
                    { moduleDump with
                        EntryPoints =
                            moduleDump.EntryPoints
                            |> List.map (fun entryPointName -> if entryPointName = "result" then "staleResult" else entryPointName)
                        Functions =
                            moduleDump.Functions
                            |> List.map (fun binding ->
                                if binding.Name = "result" then
                                    { binding with
                                        Name = "staleResult"
                                        Exported = binding.Exported
                                        EntryPoint = binding.EntryPoint }
                                else
                                    binding) }
                else
                    moduleDump)

        let outputDirectory = createScratchDirectory "dotnet-entrypoint-clr-model"

        let artifact =
            match Backend.emitDotNetArtifact { workspace with KBackendIR = driftedBackendIR } "main.result" outputDirectory DotNetDeployment.Managed with
            | Result.Ok artifact -> artifact
            | Result.Error message -> failwith message

        let runResult =
            runProcess outputDirectory "dotnet" $"run --project \"{artifact.ProjectFilePath}\" -c Release"

        Assert.Equal(0, runResult.ExitCode)
        Assert.Equal("42", runResult.StandardOutput.Trim())
        Assert.True(String.IsNullOrWhiteSpace(runResult.StandardError), runResult.StandardError)


    [<Fact>]
    let ``cli verify does not print backend checkpoint fallout when source diagnostics already exist`` () =
        let workspaceRoot = createScratchDirectory "cli-verify-source-errors-workspace"

        writeWorkspaceFiles
            workspaceRoot
            [
                "main.kp",
                [
                    "module main"
                    "let i0 = I1 2"
                ]
                |> String.concat "\n"
            ]

        let runResult =
            runBuiltCli
                workspaceRoot
                $"--source-root \"{workspaceRoot}\" --backend dotnet --verify KBackendIR"

        Assert.Equal(1, runResult.ExitCode)
        Assert.Contains("Name 'I1' is not in scope.", runResult.StandardOutput)
        Assert.DoesNotContain("requires a backend module", runResult.StandardOutput)


module BackendTestsShard4 =

    [<Fact>]
    let ``dotnet backend reports structured workspace diagnostic failures`` () =
        let workspace =
            compileInMemoryWorkspaceWithBackend
                "memory-dotnet-workspace-diagnostics-root"
                "dotnet"
                [
                    "main.kp",
                    [
                        "module main"
                        "let result = 1"
                    ]
                    |> String.concat "\n"
                    "bad.kp",
                    [
                        "module bad"
                        "let broken = missing"
                    ]
                    |> String.concat "\n"
                ]

        Assert.True(workspace.HasErrors, "Expected the workspace to retain frontend diagnostics.")

        let outputDirectory = createScratchDirectory "dotnet-workspace-diagnostics"

        match Backend.emitDotNetArtifact workspace "main.result" outputDirectory DotNetDeployment.Managed with
        | Result.Ok artifact ->
            failwithf "Expected dotnet artifact emission to reject a diagnostic workspace, but emitted '%s'." artifact.GeneratedFilePath
        | Result.Error message ->
            let detail =
                workspace.Diagnostics
                |> List.map (fun diagnostic -> diagnostic.Message)
                |> String.concat Environment.NewLine

            Assert.Equal(
                $"The CLR-backed dotnet profile could not lower 'main.result': Cannot emit a CLR assembly for a workspace with diagnostics:{Environment.NewLine}{detail}",
                message
            )

    [<Fact>]
    let ``dotnet backend reports structured closure support failures`` () =
        let workspace =
            compileInMemoryWorkspaceWithBackend
                "memory-dotnet-closure-unsupported-root"
                "dotnet"
                [
                    "main.kp",
                    [
                        "module main"
                        "mk : Int -> Int"
                        "let mk value ="
                        "    let add = \\x -> x + value"
                        "    add 2"
                        "let result = mk 1"
                    ]
                    |> String.concat "\n"
                ]

        Assert.False(workspace.HasErrors, sprintf "Expected no frontend diagnostics, got %A" workspace.Diagnostics)

        let outputDirectory = createScratchDirectory "dotnet-closure-unsupported"

        match Backend.emitDotNetArtifact workspace "main.result" outputDirectory DotNetDeployment.Managed with
        | Result.Ok artifact ->
            failwithf "Expected dotnet artifact emission to reject runtime closures, but emitted '%s'." artifact.GeneratedFilePath
        | Result.Error message ->
            Assert.Equal(
                "The CLR-backed dotnet profile could not lower 'main.result': The CLR dotnet backend does not yet support runtime closures.",
                message
            )

    [<Fact>]
    let ``effectful dotnet backend reports structured missing body failures`` () =
        let workspace =
            compileInMemoryWorkspaceWithBackend
                "memory-dotnet-effectful-missing-body-root"
                "dotnet"
                [
                    "main.kp",
                    [
                        "@PrivateByDefault module main"
                        ""
                        "bad : Int"
                        "let bad ="
                        "    block"
                        "        scoped effect Ask ="
                        "            ask : Unit -> Int"
                        ""
                        "        let comp : Eff <[Ask : Ask]> Int ="
                        "            do"
                        "                let value <- Ask.ask ()"
                        "                pure value"
                        ""
                        "        let handled : Eff <[ ]> Int ="
                        "            deep handle Ask comp with"
                        "                case return x -> pure x"
                        "                case ask () k -> k 1"
                        ""
                        "        runPure handled"
                        ""
                        "result : Int"
                        "let result ="
                        "    block"
                        "        scoped effect Ask ="
                        "            ask : Unit -> Int"
                        ""
                        "        let comp : Eff <[Ask : Ask]> Int ="
                        "            do"
                        "                let value <- Ask.ask ()"
                        "                pure value"
                        ""
                        "        let handled : Eff <[ ]> Int ="
                        "            deep handle Ask comp with"
                        "                case return x -> pure x"
                        "                case ask () k -> k 0"
                        ""
                        "        runPure handled"
                    ]
                    |> String.concat "\n"
                ]

        Assert.False(workspace.HasErrors, sprintf "Expected no frontend diagnostics, got %A" workspace.Diagnostics)
        Assert.DoesNotContain(workspace.Diagnostics, fun diagnostic -> diagnostic.Code = DiagnosticCode.EffectRuntimeUnsupportedBackend)

        let driftedClrAssemblyIR =
            workspace.ClrAssemblyIR
            |> List.map (fun moduleDump ->
                if moduleDump.Name = "main" then
                    { moduleDump with
                        Bindings =
                            moduleDump.Bindings
                            |> List.map (fun binding ->
                                if binding.Name = "bad" then
                                    { binding with Body = None }
                                else
                                    binding) }
                else
                    moduleDump)

        let outputDirectory = createScratchDirectory "dotnet-effectful-missing-body"

        match Backend.emitDotNetArtifact { workspace with ClrAssemblyIR = driftedClrAssemblyIR } "main.result" outputDirectory DotNetDeployment.Managed with
        | Result.Ok artifact ->
            failwithf "Expected effectful dotnet artifact emission to reject missing effectful bodies, but emitted '%s'." artifact.GeneratedFilePath
        | Result.Error message ->
            Assert.Equal(
                "The CLR-backed dotnet profile could not lower 'main.result': The effectful CLR dotnet backend requires a body for 'main.bad'.",
                message
            )

    [<Fact>]
    let ``dotnet backend reports structured missing body failures`` () =
        let workspace =
            compileInMemoryWorkspaceWithBackend
                "memory-dotnet-missing-body-root"
                "dotnet"
                [
                    "main.kp",
                    [
                        "module main"
                        "bad : Int"
                        "let bad = 1"
                        "result : Int"
                        "let result = bad"
                    ]
                    |> String.concat "\n"
                ]

        Assert.False(workspace.HasErrors, sprintf "Expected no frontend diagnostics, got %A" workspace.Diagnostics)

        let driftedClrAssemblyIR =
            workspace.ClrAssemblyIR
            |> List.map (fun moduleDump ->
                if moduleDump.Name = "main" then
                    { moduleDump with
                        Bindings =
                            moduleDump.Bindings
                            |> List.map (fun binding ->
                                if binding.Name = "bad" then
                                    { binding with Body = None }
                                else
                                    binding) }
                else
                    moduleDump)

        let outputDirectory = createScratchDirectory "dotnet-missing-body"

        match Backend.emitDotNetArtifact { workspace with ClrAssemblyIR = driftedClrAssemblyIR } "main.result" outputDirectory DotNetDeployment.Managed with
        | Result.Ok artifact ->
            failwithf "Expected dotnet artifact emission to reject missing bodies, but emitted '%s'." artifact.GeneratedFilePath
        | Result.Error message ->
            Assert.Equal(
                "The CLR-backed dotnet profile could not lower 'main.result': The CLR dotnet backend requires a body for 'main.bad'.",
                message
            )

    [<Fact>]
    let ``dotnet backend reports structured bodyless zero argument binding return type failures`` () =
        let workspace =
            compileInMemoryWorkspaceWithBackend
                "memory-dotnet-bodyless-zero-arg-return-type-root"
                "dotnet"
                [
                    "main.kp",
                    [
                        "module main"
                        "let result = 0"
                    ]
                    |> String.concat "\n"
                ]

        Assert.False(workspace.HasErrors, sprintf "Expected no frontend diagnostics, got %A" workspace.Diagnostics)

        let driftedClrAssemblyIR =
            workspace.ClrAssemblyIR
            |> List.map (fun moduleDump ->
                if moduleDump.Name = "main" then
                    { moduleDump with
                        Bindings =
                            moduleDump.Bindings
                            |> List.map (fun binding ->
                                if binding.Name = "result" then
                                    { binding with
                                        Body = None
                                        ReturnTypeText = None }
                                else
                                    binding) }
                else
                    moduleDump)

        let outputDirectory = createScratchDirectory "dotnet-bodyless-zero-arg-return-type"

        match Backend.emitDotNetArtifact { workspace with ClrAssemblyIR = driftedClrAssemblyIR } "main.result" outputDirectory DotNetDeployment.Managed with
        | Result.Ok artifact ->
            failwithf "Expected dotnet artifact emission to reject bodyless zero-argument bindings without return types, but emitted '%s'." artifact.GeneratedFilePath
        | Result.Error message ->
            Assert.Equal(
                "The CLR-backed dotnet profile could not lower 'main.result': The CLR dotnet backend requires a declared return type for bodyless binding 'main.result'.",
                message
            )

    [<Fact>]
    let ``dotnet backend reports structured parameterized binding parameter type failures`` () =
        let workspace =
            compileInMemoryWorkspaceWithBackend
                "memory-dotnet-parameterized-binding-parameter-types-root"
                "dotnet"
                [
                    "main.kp",
                    [
                        "module main"
                        "f : Int -> Int"
                        "let f x = x"
                        "let result = f 1"
                    ]
                    |> String.concat "\n"
                ]

        Assert.False(workspace.HasErrors, sprintf "Expected no frontend diagnostics, got %A" workspace.Diagnostics)

        let driftedClrAssemblyIR =
            workspace.ClrAssemblyIR
            |> List.map (fun moduleDump ->
                if moduleDump.Name = "main" then
                    { moduleDump with
                        Bindings =
                            moduleDump.Bindings
                            |> List.map (fun binding ->
                                if binding.Name = "f" then
                                    { binding with
                                        Parameters =
                                            binding.Parameters
                                            |> List.map (fun parameter -> { parameter with TypeText = None }) }
                                else
                                    binding) }
                else
                    moduleDump)

        let outputDirectory = createScratchDirectory "dotnet-parameterized-binding-parameter-types"

        match Backend.emitDotNetArtifact { workspace with ClrAssemblyIR = driftedClrAssemblyIR } "main.result" outputDirectory DotNetDeployment.Managed with
        | Result.Ok artifact ->
            failwithf "Expected dotnet artifact emission to reject parameterized bindings without declared parameter types, but emitted '%s'." artifact.GeneratedFilePath
        | Result.Error message ->
            Assert.Equal(
                "The CLR-backed dotnet profile could not lower 'main.result': The CLR dotnet backend currently requires parameter types for 'main.f'.",
                message
            )

    [<Fact>]
    let ``dotnet backend reports structured bodyless parameterized binding return type failures`` () =
        let workspace =
            compileInMemoryWorkspaceWithBackend
                "memory-dotnet-bodyless-parameterized-binding-return-type-root"
                "dotnet"
                [
                    "main.kp",
                    [
                        "module main"
                        "f : Int -> Int"
                        "let f x = x"
                        "let result = f 1"
                    ]
                    |> String.concat "\n"
                ]

        Assert.False(workspace.HasErrors, sprintf "Expected no frontend diagnostics, got %A" workspace.Diagnostics)

        let driftedClrAssemblyIR =
            workspace.ClrAssemblyIR
            |> List.map (fun moduleDump ->
                if moduleDump.Name = "main" then
                    { moduleDump with
                        Bindings =
                            moduleDump.Bindings
                            |> List.map (fun binding ->
                                if binding.Name = "f" then
                                    { binding with
                                        Body = None
                                        ReturnTypeText = None }
                                else
                                    binding) }
                else
                    moduleDump)

        let outputDirectory = createScratchDirectory "dotnet-bodyless-parameterized-binding-return-type"

        match Backend.emitDotNetArtifact { workspace with ClrAssemblyIR = driftedClrAssemblyIR } "main.result" outputDirectory DotNetDeployment.Managed with
        | Result.Ok artifact ->
            failwithf "Expected dotnet artifact emission to reject bodyless parameterized bindings without explicit return types, but emitted '%s'." artifact.GeneratedFilePath
        | Result.Error message ->
            Assert.Equal(
                "The CLR-backed dotnet profile could not lower 'main.result': The CLR dotnet backend requires an explicit return type for bodyless binding 'main.f'.",
                message
            )

    [<Fact>]
    let ``dotnet backend reports structured expected expression type failures`` () =
        let workspace =
            compileInMemoryWorkspaceWithBackend
                "memory-dotnet-binding-return-type-mismatch-root"
                "dotnet"
                [
                    "main.kp",
                    [
                        "module main"
                        "result : Int"
                        "let result = 0"
                    ]
                    |> String.concat "\n"
                ]

        Assert.False(workspace.HasErrors, sprintf "Expected no frontend diagnostics, got %A" workspace.Diagnostics)

        let driftedClrAssemblyIR =
            workspace.ClrAssemblyIR
            |> List.map (fun moduleDump ->
                if moduleDump.Name = "main" then
                    { moduleDump with
                        Bindings =
                            moduleDump.Bindings
                            |> List.map (fun binding ->
                                if binding.Name = "result" then
                                    { binding with ReturnTypeText = Some "String" }
                                else
                                    binding) }
                else
                    moduleDump)

        let outputDirectory = createScratchDirectory "dotnet-binding-return-type-mismatch"

        match Backend.emitDotNetArtifact { workspace with ClrAssemblyIR = driftedClrAssemblyIR } "main.result" outputDirectory DotNetDeployment.Managed with
        | Result.Ok artifact ->
            failwithf "Expected dotnet artifact emission to reject expected-expression type mismatches, but emitted '%s'." artifact.GeneratedFilePath
        | Result.Error message ->
            Assert.Equal(
                "The CLR-backed dotnet profile could not lower 'main.result': The CLR dotnet backend expected expression of type String, but found Int.",
                message
            )

    [<Fact>]
    let ``dotnet backend reports structured declared type resolution failures`` () =
        let workspace =
            compileInMemoryWorkspaceWithBackend
                "memory-dotnet-declared-type-resolution-root"
                "dotnet"
                [
                    "main.kp",
                    [
                        "module main"
                        "helper : Int -> Int"
                        "let helper x = x"
                        "let result = helper 1"
                    ]
                    |> String.concat "\n"
                ]

        Assert.False(workspace.HasErrors, sprintf "Expected no frontend diagnostics, got %A" workspace.Diagnostics)

        let driftedClrAssemblyIR =
            workspace.ClrAssemblyIR
            |> List.map (fun moduleDump ->
                if moduleDump.Name = "main" then
                    { moduleDump with
                        Bindings =
                            moduleDump.Bindings
                            |> List.map (fun binding ->
                                if binding.Name = "helper" then
                                    { binding with ReturnTypeText = Some "Missing" }
                                else
                                    binding) }
                else
                    moduleDump)

        let outputDirectory = createScratchDirectory "dotnet-declared-type-resolution"

        match Backend.emitDotNetArtifact { workspace with ClrAssemblyIR = driftedClrAssemblyIR } "main.result" outputDirectory DotNetDeployment.Managed with
        | Result.Ok artifact ->
            failwithf "Expected dotnet artifact emission to reject unresolved declared types, but emitted '%s'." artifact.GeneratedFilePath
        | Result.Error message ->
            Assert.Equal(
                "The CLR-backed dotnet profile could not lower 'main.result': The CLR dotnet backend could not resolve type 'Missing'.",
                message
            )

    [<Fact>]
    let ``dotnet backend reports structured binding arity failures`` () =
        let workspace =
            compileInMemoryWorkspaceWithBackend
                "memory-dotnet-binding-arity-root"
                "dotnet"
                [
                    "main.kp",
                    [
                        "module main"
                        "helper : Int -> Int"
                        "let helper x = x"
                        "let result = helper 1"
                    ]
                    |> String.concat "\n"
                ]

        Assert.False(workspace.HasErrors, sprintf "Expected no frontend diagnostics, got %A" workspace.Diagnostics)

        let malformedBody =
            KRuntimeApply(
                KRuntimeName [ "helper" ],
                [
                    KRuntimeLiteral(LiteralValue.Integer 1L)
                    KRuntimeLiteral(LiteralValue.Integer 2L)
                ]
            )

        let driftedClrAssemblyIR =
            workspace.ClrAssemblyIR
            |> List.map (fun moduleDump ->
                if moduleDump.Name = "main" then
                    { moduleDump with
                        Bindings =
                            moduleDump.Bindings
                            |> List.map (fun binding ->
                                if binding.Name = "result" then
                                    { binding with Body = Some malformedBody }
                                else
                                    binding) }
                else
                    moduleDump)

        let outputDirectory = createScratchDirectory "dotnet-binding-arity"

        match Backend.emitDotNetArtifact { workspace with ClrAssemblyIR = driftedClrAssemblyIR } "main.result" outputDirectory DotNetDeployment.Managed with
        | Result.Ok artifact ->
            failwithf "Expected dotnet artifact emission to reject binding arity mismatches, but emitted '%s'." artifact.GeneratedFilePath
        | Result.Error message ->
            Assert.Equal(
                "The CLR-backed dotnet profile could not lower 'main.result': The CLR dotnet backend expected 'helper' to receive 1 argument(s), but received 2.",
                message
            )

    [<Fact>]
    let ``dotnet backend reports structured constructor arity failures`` () =
        let workspace =
            compileInMemoryWorkspaceWithBackend
                "memory-dotnet-constructor-arity-root"
                "dotnet"
                [
                    "main.kp",
                    [
                        "module main"
                        "data Box : Type ="
                        "    Box Int"
                        "let result = Box 1"
                    ]
                    |> String.concat "\n"
                ]

        Assert.False(workspace.HasErrors, sprintf "Expected no frontend diagnostics, got %A" workspace.Diagnostics)

        let malformedBody =
            KRuntimeApply(
                KRuntimeName [ "Box" ],
                [
                    KRuntimeLiteral(LiteralValue.Integer 1L)
                    KRuntimeLiteral(LiteralValue.Integer 2L)
                ]
            )

        let driftedClrAssemblyIR =
            workspace.ClrAssemblyIR
            |> List.map (fun moduleDump ->
                if moduleDump.Name = "main" then
                    { moduleDump with
                        Bindings =
                            moduleDump.Bindings
                            |> List.map (fun binding ->
                                if binding.Name = "result" then
                                    { binding with Body = Some malformedBody }
                                else
                                    binding) }
                else
                    moduleDump)

        let outputDirectory = createScratchDirectory "dotnet-constructor-arity"

        match Backend.emitDotNetArtifact { workspace with ClrAssemblyIR = driftedClrAssemblyIR } "main.result" outputDirectory DotNetDeployment.Managed with
        | Result.Ok artifact ->
            failwithf "Expected dotnet artifact emission to reject constructor arity mismatches, but emitted '%s'." artifact.GeneratedFilePath
        | Result.Error message ->
            Assert.Equal(
                "The CLR-backed dotnet profile could not lower 'main.result': The CLR dotnet backend expected constructor 'Box' to receive 1 argument(s), but received 2.",
                message
            )

    [<Fact>]
    let ``dotnet backend reports structured unsupported lowered type form failures`` () =
        let workspace =
            compileInMemoryWorkspaceWithBackend
                "memory-dotnet-unsupported-type-form-root"
                "dotnet"
                [
                    "main.kp",
                    [
                        "module main"
                        "data Box : Type ="
                        "    Box Int"
                        "result : Box"
                        "let result = Box 0"
                    ]
                    |> String.concat "\n"
                ]

        Assert.False(workspace.HasErrors, sprintf "Expected no frontend diagnostics, got %A" workspace.Diagnostics)

        let driftedClrAssemblyIR =
            workspace.ClrAssemblyIR
            |> List.map (fun moduleDump ->
                if moduleDump.Name = "main" then
                    { moduleDump with
                        Bindings =
                            moduleDump.Bindings
                            |> List.map (fun binding ->
                                if binding.Name = "result" then
                                    { binding with ReturnTypeText = Some "List (Int -> Int)" }
                                else
                                    binding) }
                else
                    moduleDump)

        let outputDirectory = createScratchDirectory "dotnet-unsupported-type-form"

        match Backend.emitDotNetArtifact { workspace with ClrAssemblyIR = driftedClrAssemblyIR } "main.result" outputDirectory DotNetDeployment.Managed with
        | Result.Ok artifact ->
            failwithf "Expected dotnet artifact emission to reject unsupported lowered type forms, but emitted '%s'." artifact.GeneratedFilePath
        | Result.Error message ->
            Assert.Equal(
                "The CLR-backed dotnet profile could not lower 'main.result': The CLR dotnet backend could not lower this type form.",
                message
            )

    [<Fact>]
    let ``dotnet backend reports structured constructor generic inference failures`` () =
        let workspace =
            compileInMemoryWorkspaceWithBackend
                "memory-dotnet-constructor-generic-inference-root"
                "dotnet"
                [
                    "main.kp",
                    [
                        "module main"
                        "data PhantomPair (a : Type) (b : Type) ="
                        "    PhantomPair a"
                        "result : PhantomPair Int String"
                        "let result = PhantomPair 1"
                    ]
                    |> String.concat "\n"
                ]

        Assert.False(workspace.HasErrors, sprintf "Expected no frontend diagnostics, got %A" workspace.Diagnostics)

        let driftedClrAssemblyIR =
            workspace.ClrAssemblyIR
            |> List.map (fun moduleDump ->
                if moduleDump.Name = "main" then
                    { moduleDump with
                        Bindings =
                            moduleDump.Bindings
                            |> List.map (fun binding ->
                                if binding.Name = "result" then
                                    { binding with ReturnTypeText = None }
                                else
                                    binding) }
                else
                    moduleDump)

        let outputDirectory = createScratchDirectory "dotnet-constructor-generic-inference"

        match Backend.emitDotNetArtifact { workspace with ClrAssemblyIR = driftedClrAssemblyIR } "main.result" outputDirectory DotNetDeployment.Managed with
        | Result.Ok artifact ->
            failwithf "Expected dotnet artifact emission to reject unresolved constructor type arguments, but emitted '%s'." artifact.GeneratedFilePath
        | Result.Error message ->
            Assert.Equal(
                "The CLR-backed dotnet profile could not lower 'main.result': The CLR dotnet backend could not infer concrete type arguments for constructor 'PhantomPair'.",
                message
            )

    [<Fact>]
    let ``dotnet backend reports structured binding generic inference failures`` () =
        let workspace =
            compileInMemoryWorkspaceWithBackend
                "memory-dotnet-binding-generic-inference-root"
                "dotnet"
                [
                    "main.kp",
                    [
                        "module main"
                        "helper : Int -> Int"
                        "let helper x = x"
                        "let result = helper 1"
                    ]
                    |> String.concat "\n"
                ]

        Assert.False(workspace.HasErrors, sprintf "Expected no frontend diagnostics, got %A" workspace.Diagnostics)

        let driftedClrAssemblyIR =
            workspace.ClrAssemblyIR
            |> List.map (fun moduleDump ->
                if moduleDump.Name = "main" then
                    { moduleDump with
                        Bindings =
                            moduleDump.Bindings
                            |> List.map (fun binding ->
                                if binding.Name = "helper" then
                                    { binding with
                                        Body = None
                                        ReturnTypeText = Some "a" }
                                else
                                    binding) }
                else
                    moduleDump)

        let outputDirectory = createScratchDirectory "dotnet-binding-generic-inference"

        match Backend.emitDotNetArtifact { workspace with ClrAssemblyIR = driftedClrAssemblyIR } "main.result" outputDirectory DotNetDeployment.Managed with
        | Result.Ok artifact ->
            failwithf "Expected dotnet artifact emission to reject unresolved binding type arguments, but emitted '%s'." artifact.GeneratedFilePath
        | Result.Error message ->
            Assert.Equal(
                "The CLR-backed dotnet profile could not lower 'main.result': The CLR dotnet backend could not infer concrete type arguments for 'helper'.",
                message
            )

    [<Fact>]
    let ``dotnet backend reports structured constructor type unification failures`` () =
        let workspace =
            compileInMemoryWorkspaceWithBackend
                "memory-dotnet-constructor-type-unification-root"
                "dotnet"
                [
                    "main.kp",
                    [
                        "module main"
                        "data Box : Type ="
                        "    Box Int"
                        "result : Box"
                        "let result = Box 0"
                    ]
                    |> String.concat "\n"
                ]

        Assert.False(workspace.HasErrors, sprintf "Expected no frontend diagnostics, got %A" workspace.Diagnostics)

        let driftedClrAssemblyIR =
            workspace.ClrAssemblyIR
            |> List.map (fun moduleDump ->
                if moduleDump.Name = "main" then
                    { moduleDump with
                        Bindings =
                            moduleDump.Bindings
                            |> List.map (fun binding ->
                                if binding.Name = "result" then
                                    { binding with ReturnTypeText = Some "String" }
                                else
                                    binding) }
                else
                    moduleDump)

        let outputDirectory = createScratchDirectory "dotnet-constructor-type-unification"

        match Backend.emitDotNetArtifact { workspace with ClrAssemblyIR = driftedClrAssemblyIR } "main.result" outputDirectory DotNetDeployment.Managed with
        | Result.Ok artifact ->
            failwithf "Expected dotnet artifact emission to reject constructor type unification failures, but emitted '%s'." artifact.GeneratedFilePath
        | Result.Error message ->
            Assert.Equal(
                "The CLR-backed dotnet profile could not lower 'main.result': The CLR dotnet backend could not unify Box with String.",
                message
            )

    [<Fact>]
    let ``dotnet backend reports structured binding type unification failures`` () =
        let workspace =
            compileInMemoryWorkspaceWithBackend
                "memory-dotnet-binding-type-unification-root"
                "dotnet"
                [
                    "main.kp",
                    [
                        "module main"
                        "helper : Int -> Int"
                        "let helper x = x"
                        "result : Int"
                        "let result = helper 1"
                    ]
                    |> String.concat "\n"
                ]

        Assert.False(workspace.HasErrors, sprintf "Expected no frontend diagnostics, got %A" workspace.Diagnostics)

        let driftedClrAssemblyIR =
            workspace.ClrAssemblyIR
            |> List.map (fun moduleDump ->
                if moduleDump.Name = "main" then
                    { moduleDump with
                        Bindings =
                            moduleDump.Bindings
                            |> List.map (fun binding ->
                                if binding.Name = "result" then
                                    { binding with ReturnTypeText = Some "String" }
                                else
                                    binding) }
                else
                    moduleDump)

        let outputDirectory = createScratchDirectory "dotnet-binding-type-unification"

        match Backend.emitDotNetArtifact { workspace with ClrAssemblyIR = driftedClrAssemblyIR } "main.result" outputDirectory DotNetDeployment.Managed with
        | Result.Ok artifact ->
            failwithf "Expected dotnet artifact emission to reject binding type unification failures, but emitted '%s'." artifact.GeneratedFilePath
        | Result.Error message ->
            Assert.Equal(
                "The CLR-backed dotnet profile could not lower 'main.result': The CLR dotnet backend could not unify Int with String.",
                message
            )

    [<Fact>]
    let ``dotnet backend reports structured constructor pattern resolution failures`` () =
        let workspace =
            compileInMemoryWorkspaceWithBackend
                "memory-dotnet-constructor-pattern-resolution-root"
                "dotnet"
                [
                    "main.kp",
                    [
                        "module main"
                        "let result = 0"
                    ]
                    |> String.concat "\n"
                ]

        Assert.False(workspace.HasErrors, sprintf "Expected no frontend diagnostics, got %A" workspace.Diagnostics)

        let malformedBody =
            KRuntimeMatch(
                KRuntimeLiteral LiteralValue.Unit,
                [
                    { Pattern = KRuntimeConstructorPattern([ "Missing" ], [])
                      Guard = None
                      Body = KRuntimeLiteral(LiteralValue.Integer 0L) }
                ]
            )

        let driftedClrAssemblyIR =
            workspace.ClrAssemblyIR
            |> List.map (fun moduleDump ->
                if moduleDump.Name = "main" then
                    { moduleDump with
                        Bindings =
                            moduleDump.Bindings
                            |> List.map (fun binding ->
                                if binding.Name = "result" then
                                    { binding with Body = Some malformedBody }
                                else
                                    binding) }
                else
                    moduleDump)

        let outputDirectory = createScratchDirectory "dotnet-constructor-pattern-resolution"

        match Backend.emitDotNetArtifact { workspace with ClrAssemblyIR = driftedClrAssemblyIR } "main.result" outputDirectory DotNetDeployment.Managed with
        | Result.Ok artifact ->
            failwithf "Expected dotnet artifact emission to reject unresolved constructor patterns, but emitted '%s'." artifact.GeneratedFilePath
        | Result.Error message ->
            Assert.Equal(
                "The CLR-backed dotnet profile could not lower 'main.result': The CLR dotnet backend could not resolve constructor pattern 'Missing'.",
                message
            )

    [<Fact>]
    let ``dotnet backend reports structured unresolved name failures`` () =
        let workspace =
            compileInMemoryWorkspaceWithBackend
                "memory-dotnet-unresolved-name-root"
                "dotnet"
                [
                    "main.kp",
                    [
                        "module main"
                        "let result = 0"
                    ]
                    |> String.concat "\n"
                ]

        Assert.False(workspace.HasErrors, sprintf "Expected no frontend diagnostics, got %A" workspace.Diagnostics)

        let driftedClrAssemblyIR =
            workspace.ClrAssemblyIR
            |> List.map (fun moduleDump ->
                if moduleDump.Name = "main" then
                    { moduleDump with
                        Bindings =
                            moduleDump.Bindings
                            |> List.map (fun binding ->
                                if binding.Name = "result" then
                                    { binding with Body = Some(KRuntimeName [ "missing" ]) }
                                else
                                    binding) }
                else
                    moduleDump)

        let outputDirectory = createScratchDirectory "dotnet-unresolved-name"

        match Backend.emitDotNetArtifact { workspace with ClrAssemblyIR = driftedClrAssemblyIR } "main.result" outputDirectory DotNetDeployment.Managed with
        | Result.Ok artifact ->
            failwithf "Expected dotnet artifact emission to reject unresolved names, but emitted '%s'." artifact.GeneratedFilePath
        | Result.Error message ->
            Assert.Equal(
                "The CLR-backed dotnet profile could not lower 'main.result': The CLR dotnet backend could not resolve name 'missing'. Locals in scope: [].",
                message
            )

    [<Fact>]
    let ``dotnet backend reports structured unresolved callee failures`` () =
        let workspace =
            compileInMemoryWorkspaceWithBackend
                "memory-dotnet-unresolved-callee-root"
                "dotnet"
                [
                    "main.kp",
                    [
                        "module main"
                        "let result = 0"
                    ]
                    |> String.concat "\n"
                ]

        Assert.False(workspace.HasErrors, sprintf "Expected no frontend diagnostics, got %A" workspace.Diagnostics)

        let driftedClrAssemblyIR =
            workspace.ClrAssemblyIR
            |> List.map (fun moduleDump ->
                if moduleDump.Name = "main" then
                    { moduleDump with
                        Bindings =
                            moduleDump.Bindings
                            |> List.map (fun binding ->
                                if binding.Name = "result" then
                                    { binding with Body = Some(KRuntimeApply(KRuntimeName [ "missing" ], [])) }
                                else
                                    binding) }
                else
                    moduleDump)

        let outputDirectory = createScratchDirectory "dotnet-unresolved-callee"

        match Backend.emitDotNetArtifact { workspace with ClrAssemblyIR = driftedClrAssemblyIR } "main.result" outputDirectory DotNetDeployment.Managed with
        | Result.Ok artifact ->
            failwithf "Expected dotnet artifact emission to reject unresolved callees, but emitted '%s'." artifact.GeneratedFilePath
        | Result.Error message ->
            Assert.Equal(
                "The CLR-backed dotnet profile could not lower 'main.result': The CLR dotnet backend could not resolve callee 'missing'.",
                message
            )

    [<Fact>]
    let ``dotnet backend reports structured unresolved trait instance failures`` () =
        let workspace =
            compileInMemoryWorkspaceWithBackend
                "memory-dotnet-trait-instance-resolution-root"
                "dotnet"
                [
                    "main.kp",
                    [
                        "module main"
                        "trait Render (a : Type) ="
                        "    render : a -> String"
                        "instance Render Int ="
                        "    let render x = primitiveIntToString x"
                        "dict : Render Int"
                        "let dict = summon"
                        "let result = 0"
                    ]
                    |> String.concat "\n"
                ]

        Assert.False(workspace.HasErrors, sprintf "Expected no frontend diagnostics, got %A" workspace.Diagnostics)

        let mainModule =
            workspace.ClrAssemblyIR
            |> List.find (fun moduleDump -> moduleDump.Name = "main")

        let instanceInfo =
            mainModule.TraitInstances |> List.exactlyOne

        let driftedClrAssemblyIR =
            workspace.ClrAssemblyIR
            |> List.map (fun moduleDump ->
                if moduleDump.Name = "main" then
                    { moduleDump with
                        TraitInstances = []
                        Bindings =
                            moduleDump.Bindings
                            |> List.map (fun binding ->
                                if binding.Name = "result" then
                                    { binding with
                                        Body =
                                            Some(KRuntimeDictionaryValue(moduleDump.Name, instanceInfo.TraitName, instanceInfo.InstanceKey, [])) }
                                else
                                    binding) }
                else
                    moduleDump)

        let outputDirectory = createScratchDirectory "dotnet-trait-instance-resolution"

        match Backend.emitDotNetArtifact { workspace with ClrAssemblyIR = driftedClrAssemblyIR } "main.result" outputDirectory DotNetDeployment.Managed with
        | Result.Ok artifact ->
            failwithf "Expected dotnet artifact emission to reject unresolved trait instances, but emitted '%s'." artifact.GeneratedFilePath
        | Result.Error message ->
            Assert.Equal(
                $"The CLR-backed dotnet profile could not lower 'main.result': The CLR dotnet backend could not resolve trait instance 'main.{instanceInfo.TraitName}.{instanceInfo.InstanceKey}'.",
                message
            )

    [<Fact>]
    let ``dotnet backend reports structured missing trait route failures`` () =
        let workspace =
            compileInMemoryWorkspaceWithBackend
                "memory-dotnet-trait-route-resolution-root"
                "dotnet"
                [
                    "main.kp",
                    [
                        "module main"
                        "trait Render (a : Type) ="
                        "    render : a -> String"
                        "instance Render Int ="
                        "    let render x = primitiveIntToString x"
                        "let result = render 1"
                    ]
                    |> String.concat "\n"
                ]

        Assert.False(workspace.HasErrors, sprintf "Expected no frontend diagnostics, got %A" workspace.Diagnostics)

        let driftedClrAssemblyIR =
            workspace.ClrAssemblyIR
            |> List.map (fun moduleDump ->
                if moduleDump.Name = "main" then
                    { moduleDump with
                        TraitInstances =
                            moduleDump.TraitInstances
                            |> List.map (fun instanceInfo ->
                                { instanceInfo with MemberBindings = [] })
                        Bindings =
                            moduleDump.Bindings
                            |> List.map (fun binding ->
                                if binding.Name = "result" then
                                    { binding with
                                        Body =
                                            Some(
                                                KRuntimeTraitCall(
                                                    "Render",
                                                    "render",
                                                    KRuntimeLiteral LiteralValue.Unit,
                                                    [ KRuntimeLiteral(LiteralValue.Integer 1L) ]
                                                )
                                            ) }
                                else
                                    binding) }
                else
                    moduleDump)

        let outputDirectory = createScratchDirectory "dotnet-trait-route-resolution"

        match Backend.emitDotNetArtifact { workspace with ClrAssemblyIR = driftedClrAssemblyIR } "main.result" outputDirectory DotNetDeployment.Managed with
        | Result.Ok artifact ->
            failwithf "Expected dotnet artifact emission to reject missing trait routes, but emitted '%s'." artifact.GeneratedFilePath
        | Result.Error message ->
            Assert.Equal(
                "The CLR-backed dotnet profile could not lower 'main.result': The CLR dotnet backend could not find any routes for trait call 'Render.render'.",
                message
            )

    [<Fact>]
    let ``dotnet backend reports structured missing durable host metadata failures`` () =
        let workspace =
            compileInMemoryWorkspaceWithBackend
                "memory-dotnet-host-metadata-resolution-root"
                "dotnet"
                [
                    "main.kp",
                    [
                        "module main"
                        "import host.dotnet.Kappa.Compiler.TestHost.Sample.(term Create)"
                        "let result = Create \"ok\""
                    ]
                    |> String.concat "\n"
                ]

        Assert.False(workspace.HasErrors, sprintf "Expected host.dotnet wrappers to compile, got %A" workspace.Diagnostics)

        let driftedClrAssemblyIR =
            workspace.ClrAssemblyIR
            |> List.map (fun moduleDump ->
                if moduleDump.Name = "host.dotnet.Kappa.Compiler.TestHost.Sample" then
                    { moduleDump with
                        Bindings =
                            moduleDump.Bindings
                            |> List.map (fun binding ->
                                if binding.Name = "Create" then
                                    { binding with
                                        ExternalBinding = Some(DotNetHostMethod("Missing.Type, Missing.Assembly", "Create", [], []))
                                        Body = None }
                                else
                                    binding) }
                else
                    moduleDump)

        let outputDirectory = createScratchDirectory "dotnet-host-metadata-resolution"

        match Backend.emitDotNetArtifact { workspace with ClrAssemblyIR = driftedClrAssemblyIR } "main.result" outputDirectory DotNetDeployment.Managed with
        | Result.Ok artifact ->
            failwithf "Expected dotnet artifact emission to reject missing durable host metadata, but emitted '%s'." artifact.GeneratedFilePath
        | Result.Error message ->
            Assert.Equal(
                "The CLR-backed dotnet profile could not lower 'main.result': The CLR dotnet backend could not resolve durable host binding metadata for 'host.dotnet.Kappa.Compiler.TestHost.Sample.Create'.",
                message
            )

    [<Fact>]
    let ``dotnet backend reports structured openFile missing default file type failures`` () =
        let workspace =
            compileInMemoryWorkspaceWithBackend
                "memory-dotnet-openfile-default-file-root"
                "dotnet"
                [
                    "main.kp",
                    [
                        "module main"
                        "let result = 0"
                    ]
                    |> String.concat "\n"
                ]

        Assert.False(workspace.HasErrors, sprintf "Expected no frontend diagnostics, got %A" workspace.Diagnostics)

        let driftedClrAssemblyIR =
            workspace.ClrAssemblyIR
            |> List.map (fun moduleDump ->
                if moduleDump.Name = "main" then
                    { moduleDump with
                        Bindings =
                            moduleDump.Bindings
                            |> List.map (fun binding ->
                                if binding.Name = "result" then
                                    { binding with
                                        Body =
                                            Some(
                                                KRuntimeApply(
                                                    KRuntimeName [ "openFile" ],
                                                    [ KRuntimeLiteral(LiteralValue.String "data.txt") ]
                                                )
                                            ) }
                                else
                                    binding) }
                else
                    moduleDump)

        let outputDirectory = createScratchDirectory "dotnet-openfile-default-file"

        match Backend.emitDotNetArtifact { workspace with ClrAssemblyIR = driftedClrAssemblyIR } "main.result" outputDirectory DotNetDeployment.Managed with
        | Result.Ok artifact ->
            failwithf "Expected dotnet artifact emission to reject openFile without a File result type, but emitted '%s'." artifact.GeneratedFilePath
        | Result.Error message ->
            Assert.Equal(
                "The CLR-backed dotnet profile could not lower 'main.result': The CLR dotnet backend intrinsic 'openFile' requires a File data type in the current module when no expected type is available.",
                message
            )

    [<Fact>]
    let ``dotnet backend reports structured openFile result type failures`` () =
        let workspace =
            compileInMemoryWorkspaceWithBackend
                "memory-dotnet-openfile-result-type-root"
                "dotnet"
                [
                    "main.kp",
                    [
                        "module main"
                        "result : Int"
                        "let result = 0"
                    ]
                    |> String.concat "\n"
                ]

        Assert.False(workspace.HasErrors, sprintf "Expected no frontend diagnostics, got %A" workspace.Diagnostics)

        let driftedClrAssemblyIR =
            workspace.ClrAssemblyIR
            |> List.map (fun moduleDump ->
                if moduleDump.Name = "main" then
                    { moduleDump with
                        Bindings =
                            moduleDump.Bindings
                            |> List.map (fun binding ->
                                if binding.Name = "result" then
                                    { binding with
                                        Body =
                                            Some(
                                                KRuntimeApply(
                                                    KRuntimeName [ "openFile" ],
                                                    [ KRuntimeLiteral(LiteralValue.String "data.txt") ]
                                                )
                                            ) }
                                else
                                    binding) }
                else
                    moduleDump)

        let outputDirectory = createScratchDirectory "dotnet-openfile-result-type"

        match Backend.emitDotNetArtifact { workspace with ClrAssemblyIR = driftedClrAssemblyIR } "main.result" outputDirectory DotNetDeployment.Managed with
        | Result.Ok artifact ->
            failwithf "Expected dotnet artifact emission to reject openFile with a non-File result type, but emitted '%s'." artifact.GeneratedFilePath
        | Result.Error message ->
            Assert.Equal(
                "The CLR-backed dotnet profile could not lower 'main.result': The CLR dotnet backend intrinsic 'openFile' expected a concrete File-like ADT result, but got Int.",
                message
            )

    [<Fact>]
    let ``dotnet backend reports structured non bool if condition failures`` () =
        let workspace =
            compileInMemoryWorkspaceWithBackend
                "memory-dotnet-if-condition-root"
                "dotnet"
                [
                    "main.kp",
                    [
                        "module main"
                        "let result = 0"
                    ]
                    |> String.concat "\n"
                ]

        Assert.False(workspace.HasErrors, sprintf "Expected no frontend diagnostics, got %A" workspace.Diagnostics)

        let malformedBody =
            KRuntimeIfThenElse(
                KRuntimeLiteral(LiteralValue.Integer 0L),
                KRuntimeLiteral(LiteralValue.Integer 1L),
                KRuntimeLiteral(LiteralValue.Integer 2L)
            )

        let driftedClrAssemblyIR =
            workspace.ClrAssemblyIR
            |> List.map (fun moduleDump ->
                if moduleDump.Name = "main" then
                    { moduleDump with
                        Bindings =
                            moduleDump.Bindings
                            |> List.map (fun binding ->
                                if binding.Name = "result" then
                                    { binding with Body = Some malformedBody }
                                else
                                    binding) }
                else
                    moduleDump)

        let outputDirectory = createScratchDirectory "dotnet-if-condition"

        match Backend.emitDotNetArtifact { workspace with ClrAssemblyIR = driftedClrAssemblyIR } "main.result" outputDirectory DotNetDeployment.Managed with
        | Result.Ok artifact ->
            failwithf "Expected dotnet artifact emission to reject non-Bool if conditions, but emitted '%s'." artifact.GeneratedFilePath
        | Result.Error message ->
            Assert.Equal(
                "The CLR-backed dotnet profile could not lower 'main.result': The CLR dotnet backend requires Bool conditions for if expressions.",
                message
            )

    [<Fact>]
    let ``dotnet backend reports structured non bool while condition failures`` () =
        let workspace =
            compileInMemoryWorkspaceWithBackend
                "memory-dotnet-while-condition-root"
                "dotnet"
                [
                    "main.kp",
                    [
                        "module main"
                        "let result = 0"
                    ]
                    |> String.concat "\n"
                ]

        Assert.False(workspace.HasErrors, sprintf "Expected no frontend diagnostics, got %A" workspace.Diagnostics)

        let malformedBody =
            KRuntimeWhile(
                KRuntimeLiteral(LiteralValue.Integer 0L),
                KRuntimeLiteral LiteralValue.Unit
            )

        let driftedClrAssemblyIR =
            workspace.ClrAssemblyIR
            |> List.map (fun moduleDump ->
                if moduleDump.Name = "main" then
                    { moduleDump with
                        Bindings =
                            moduleDump.Bindings
                            |> List.map (fun binding ->
                                if binding.Name = "result" then
                                    { binding with Body = Some malformedBody }
                                else
                                    binding) }
                else
                    moduleDump)

        let outputDirectory = createScratchDirectory "dotnet-while-condition"

        match Backend.emitDotNetArtifact { workspace with ClrAssemblyIR = driftedClrAssemblyIR } "main.result" outputDirectory DotNetDeployment.Managed with
        | Result.Ok artifact ->
            failwithf "Expected dotnet artifact emission to reject non-Bool while conditions, but emitted '%s'." artifact.GeneratedFilePath
        | Result.Error message ->
            Assert.Equal(
                "The CLR-backed dotnet profile could not lower 'main.result': The CLR dotnet backend requires Bool conditions for while expressions.",
                message
            )

    [<Fact>]
    let ``dotnet backend reports structured empty match failures`` () =
        let workspace =
            compileInMemoryWorkspaceWithBackend
                "memory-dotnet-empty-match-root"
                "dotnet"
                [
                    "main.kp",
                    [
                        "module main"
                        "let result = 0"
                    ]
                    |> String.concat "\n"
                ]

        Assert.False(workspace.HasErrors, sprintf "Expected no frontend diagnostics, got %A" workspace.Diagnostics)

        let malformedBody =
            KRuntimeMatch(KRuntimeLiteral(LiteralValue.Integer 0L), [])

        let driftedClrAssemblyIR =
            workspace.ClrAssemblyIR
            |> List.map (fun moduleDump ->
                if moduleDump.Name = "main" then
                    { moduleDump with
                        Bindings =
                            moduleDump.Bindings
                            |> List.map (fun binding ->
                                if binding.Name = "result" then
                                    { binding with Body = Some malformedBody }
                                else
                                    binding) }
                else
                    moduleDump)

        let outputDirectory = createScratchDirectory "dotnet-empty-match"

        match Backend.emitDotNetArtifact { workspace with ClrAssemblyIR = driftedClrAssemblyIR } "main.result" outputDirectory DotNetDeployment.Managed with
        | Result.Ok artifact ->
            failwithf "Expected dotnet artifact emission to reject empty matches, but emitted '%s'." artifact.GeneratedFilePath
        | Result.Error message ->
            Assert.Equal(
                "The CLR-backed dotnet profile could not lower 'main.result': The CLR dotnet backend requires at least one match case.",
                message
            )

    [<Fact>]
    let ``dotnet backend reports structured non named application failures`` () =
        let workspace =
            compileInMemoryWorkspaceWithBackend
                "memory-dotnet-nonnamed-application-root"
                "dotnet"
                [
                    "main.kp",
                    [
                        "module main"
                        "let result = 0"
                    ]
                    |> String.concat "\n"
                ]

        Assert.False(workspace.HasErrors, sprintf "Expected no frontend diagnostics, got %A" workspace.Diagnostics)

        let malformedBody =
            KRuntimeApply(KRuntimeLiteral(LiteralValue.Integer 0L), [])

        let driftedClrAssemblyIR =
            workspace.ClrAssemblyIR
            |> List.map (fun moduleDump ->
                if moduleDump.Name = "main" then
                    { moduleDump with
                        Bindings =
                            moduleDump.Bindings
                            |> List.map (fun binding ->
                                if binding.Name = "result" then
                                    { binding with Body = Some malformedBody }
                                else
                                    binding) }
                else
                    moduleDump)

        let outputDirectory = createScratchDirectory "dotnet-nonnamed-application"

        match Backend.emitDotNetArtifact { workspace with ClrAssemblyIR = driftedClrAssemblyIR } "main.result" outputDirectory DotNetDeployment.Managed with
        | Result.Ok artifact ->
            failwithf "Expected dotnet artifact emission to reject non-named application callees, but emitted '%s'." artifact.GeneratedFilePath
        | Result.Error message ->
            Assert.Equal(
                "The CLR-backed dotnet profile could not lower 'main.result': The CLR dotnet backend currently supports application only when the callee is a named binding.",
                message
            )

    [<Fact>]
    let ``clr assembly ir carries durable host dotnet binding metadata`` () =
        let workspace =
            compileInMemoryWorkspaceWithBackend
                "memory-dotnet-host-clr-ir-root"
                "dotnet"
                [
                    "main.kp",
                    [
                        "module main"
                        "import host.dotnet.Kappa.Compiler.TestHost.Sample.(term Create)"
                        "let result = Create \"ok\""
                    ]
                    |> String.concat "\n"
                ]

        Assert.False(workspace.HasErrors, sprintf "Expected host.dotnet wrappers to compile, got %A" workspace.Diagnostics)

        let hostModule =
            workspace.ClrAssemblyIR
            |> List.find (fun moduleDump -> moduleDump.Name = "host.dotnet.Kappa.Compiler.TestHost.Sample")

        let createBinding =
            hostModule.Bindings
            |> List.find (fun binding -> binding.Name = "Create")

        match createBinding.ExternalBinding with
        | Some(DotNetHostMethod(declaringTypeAssemblyQualifiedName, methodName, _, requiredAssemblyPaths)) ->
            Assert.Contains("Kappa.Compiler.TestHost.Sample", declaringTypeAssemblyQualifiedName, StringComparison.Ordinal)
            Assert.Equal("Create", methodName)
            Assert.Contains(typeof<Kappa.Compiler.TestHost.Sample>.Assembly.Location, requiredAssemblyPaths)
        | _ ->
            failwithf "Expected durable host metadata for Create, got %A" createBinding.ExternalBinding


    [<Fact>]
    let ``dotnet backend rejects entrypoints absent from the emitted CLR model`` () =
        let workspace =
            compileInMemoryWorkspaceWithBackend
                "memory-dotnet-entrypoint-clr-missing-root"
                "dotnet"
                [
                    "main.kp",
                    [
                        "module main"
                        "let result = 42"
                    ]
                    |> String.concat "\n"
                ]

        let outputDirectory = createScratchDirectory "dotnet-entrypoint-clr-missing"

        match Backend.emitDotNetArtifact { workspace with ClrAssemblyIR = [] } "main.result" outputDirectory DotNetDeployment.Managed with
        | Result.Ok artifact ->
            failwithf "Expected dotnet entrypoint resolution from ClrAssemblyIR to fail, but emitted '%s'." artifact.GeneratedFilePath
        | Result.Error message ->
            Assert.Contains("dotnet requires a zero-argument binding named 'result' in module 'main'.", message, StringComparison.Ordinal)


    [<Fact>]
    let ``cli can dump observability topics as machine readable json`` () =
        let workspaceRoot = createScratchDirectory "cli-observability-workspace"

        writeWorkspaceFiles
            workspaceRoot
            [
                "main.kp",
                [
                    "module main"
                    "let answer = 42"
                ]
                |> String.concat "\n"
            ]

        let dumpTopic topic =
            let result =
                runBuiltCli
                    workspaceRoot
                    $"--source-root \"{workspaceRoot}\" --backend dotnet --dump-observability {topic}"

            Assert.Equal(0, result.ExitCode)
            Assert.True(String.IsNullOrWhiteSpace(result.StandardError), result.StandardError)

            let json = result.StandardOutput.Trim()
            use document = JsonDocument.Parse(json)
            document.RootElement.Clone()

        let topicNames =
            [
                "analysis-session"
                "checkpoints"
                "checkpoint-contracts"
                "verify-all"
                "runtime-obligations"
                "query-sketch"
                "fingerprints"
                "incremental-units"
            ]

        for topic in topicNames do
            let root = dumpTopic topic
            Assert.Equal(topic, root.GetProperty("topic").GetString())

        let analysisSession = dumpTopic "analysis-session"
        let analysisValue = analysisSession.GetProperty("value")
        Assert.Equal("dotnet", analysisValue.GetProperty("backendProfile").GetString())

        let checkpoints = dumpTopic "checkpoints"
        let checkpointValues =
            checkpoints.GetProperty("value").EnumerateArray() |> Seq.map (fun item -> item.GetString()) |> Seq.toList

        Assert.Contains("KBackendIR", checkpointValues)
        Assert.Contains("dotnet.clr", checkpointValues)

        let verifyAll = dumpTopic "verify-all"
        let verifyResults = verifyAll.GetProperty("value").EnumerateArray() |> Seq.toList
        Assert.NotEmpty(verifyResults)
        Assert.All(verifyResults, fun result -> Assert.True(result.GetProperty("succeeded").GetBoolean()))

        let runtimeObligations = dumpTopic "runtime-obligations"
        let obligationNames =
            runtimeObligations.GetProperty("value").EnumerateArray()
            |> Seq.map (fun item -> item.GetProperty("name").GetString())
            |> Seq.toList

        Assert.Contains("effect-handlers", obligationNames)

        let querySketch = dumpTopic "query-sketch"
        let queryValues = querySketch.GetProperty("value").EnumerateArray() |> Seq.toList
        Assert.NotEmpty(queryValues)
        Assert.Contains(
            queryValues,
            fun item -> item.GetProperty("dependencyModel").GetString() = "observability-sketch"
        )

        let incrementalUnits = dumpTopic "incremental-units"
        let unitKinds =
            incrementalUnits.GetProperty("value").EnumerateArray()
            |> Seq.map (fun item -> item.GetProperty("unitKind").GetString())
            |> Seq.toList

        Assert.Contains("target-lowering", unitKinds)


module BackendTestsShard5 =

    [<Fact>]
    let ``dotnet backend rejects native aot deployment explicitly`` () =
        let workspace =
            compileInMemoryWorkspace
                "memory-dotnet-native-aot-root"
                [
                    "main.kp",
                    [
                        "module main"
                        "let result = 42"
                    ]
                    |> String.concat "\n"
                ]

        let outputDirectory = createScratchDirectory "dotnet-native-aot-backend"

        match Backend.emitDotNetArtifact workspace "main.result" outputDirectory DotNetDeployment.NativeAot with
        | Result.Ok artifact ->
            failwithf "Expected dotnet NativeAOT emission to be rejected, but emitted '%s'." artifact.ProjectFilePath
        | Result.Error message ->
            Assert.Contains(
                "--native-aot is not supported for backend dotnet.",
                message,
                StringComparison.Ordinal
            )


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

        let runResult =
            runBuiltCli
                workspaceRoot
                $"--source-root \"{workspaceRoot}\" --backend dotnet --emit-dir \"{emitDirectory}\" --run main.result"

        Assert.Equal(0, runResult.ExitCode)
        Assert.Contains("42", runResult.StandardOutput)
        Assert.True(String.IsNullOrWhiteSpace(runResult.StandardError), runResult.StandardError)


    [<Fact>]
    let ``cli dump observability rejects conflicting stdout producing flags`` () =
        let workspaceRoot = createScratchDirectory "cli-observability-conflict-workspace"

        writeWorkspaceFiles
            workspaceRoot
            [
                "main.kp",
                [
                    "module main"
                    "let answer = 42"
                ]
                |> String.concat "\n"
            ]

        let result =
            runBuiltCli
                workspaceRoot
                $"--source-root \"{workspaceRoot}\" --dump-observability analysis-session --trace"

        Assert.Equal(1, result.ExitCode)
        Assert.Contains("--dump-observability cannot be combined with --trace", result.StandardError, StringComparison.Ordinal)
        Assert.True(String.IsNullOrWhiteSpace(result.StandardOutput), result.StandardOutput)


module BackendTestsShard6 =

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
    let ``cli can run the managed dotnet backend with print string intrinsics`` () =
        let workspaceRoot = createScratchDirectory "cli-dotnet-print-string-workspace"

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

        let emitDirectory = createScratchDirectory "cli-dotnet-print-string-emit"

        let runResult =
            runBuiltCli
                workspaceRoot
                $"--source-root \"{workspaceRoot}\" --backend dotnet --emit-dir \"{emitDirectory}\" --run main.main"

        Assert.Equal(0, runResult.ExitCode)
        Assert.Equal("AB", runResult.StandardOutput.Trim())
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

        let runResult =
            runBuiltCli
                workspaceRoot
                $"--source-root \"{workspaceRoot}\" --run main.main"

        Assert.Equal(0, runResult.ExitCode)
        Assert.Equal("72", runResult.StandardOutput.Trim())
        Assert.True(String.IsNullOrWhiteSpace(runResult.StandardError), runResult.StandardError)


module BackendTestsShard7 =

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
    let ``cli can run the managed dotnet backend for generic constructor methods`` () =
        let workspaceRoot = createScratchDirectory "cli-dotnet-generic-constructor-workspace"

        writeWorkspaceFiles
            workspaceRoot
            [
                "main.kp",
                [
                    "module main"
                    "singleton : forall (a : Type). a -> List a"
                    "let singleton x = x :: Nil"
                    "headOrZero : List Int -> Int"
                    "let headOrZero xs ="
                    "    match xs"
                    "    case head :: _ -> head"
                    "    case Nil -> 0"
                    "let result = headOrZero (singleton 42)"
                ]
                |> String.concat "\n"
            ]

        let emitDirectory = createScratchDirectory "cli-dotnet-generic-constructor-emit"

        let runResult =
            runBuiltCli
                workspaceRoot
                $"--source-root \"{workspaceRoot}\" --backend dotnet --emit-dir \"{emitDirectory}\" --run main.result"

        Assert.Equal(0, runResult.ExitCode)
        Assert.Equal("42", runResult.StandardOutput.Trim())
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
