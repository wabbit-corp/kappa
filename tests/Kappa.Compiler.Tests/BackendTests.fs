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
