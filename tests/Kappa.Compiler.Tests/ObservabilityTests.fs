module ObservabilityTests

open System
open System.IO
open System.Text.Json
open Kappa.Compiler
open Harness
open Xunit

[<Fact>]
let ``workspace exposes spec-shaped checkpoints and portable pipeline trace events`` () =
    let workspace =
        compileInMemoryWorkspace
            "memory-observability-root"
            [
                "main.kp",
                [
                    "module main"
                    "let answer = 42"
                ]
                |> String.concat "\n"
            ]

    let checkpoints = Compilation.availableCheckpoints workspace

    Assert.Contains("surface-source", checkpoints)
    Assert.Contains("KFrontIR.RAW", checkpoints)
    Assert.Contains("KFrontIR.IMPORTS", checkpoints)
    Assert.Contains("KFrontIR.CHECKERS", checkpoints)
    Assert.Contains("KCore", checkpoints)
    Assert.Contains("KRuntimeIR", checkpoints)
    Assert.Contains("KBackendIR", checkpoints)

    let trace =
        Compilation.pipelineTrace workspace
        |> List.map (fun step ->
            PipelineTraceEvent.toPortableName step.Event,
            PipelineTraceSubject.toPortableName step.Subject,
            step.InputCheckpoint,
            step.OutputCheckpoint)

    Assert.Contains(("parse", "file", "surface-source", "surface-source"), trace)
    Assert.Contains(("buildKFrontIR", "file", "surface-source", "KFrontIR.RAW"), trace)
    Assert.Contains(("advancePhase", "module", "KFrontIR.RAW", "KFrontIR.IMPORTS"), trace)
    Assert.Contains(("lowerKCore", "module", "KFrontIR.CORE_LOWERING", "KCore"), trace)
    Assert.Contains(("lowerKRuntimeIR", "KCoreUnit", "KCore", "KRuntimeIR"), trace)
    Assert.Contains(("lowerKBackendIR", "KRuntimeIRUnit", "KRuntimeIR", "KBackendIR"), trace)

[<Fact>]
let ``checkpoint contract makes implementation defined runtime IR and profile targets explicit`` () =
    let source =
        [
            "main.kp",
            [
                "module main"
                "let answer = 42"
            ]
            |> String.concat "\n"
        ]

    let interpreterWorkspace =
        compileInMemoryWorkspaceWithBackend "memory-contract-interpreter-root" "interpreter" source

    let zigWorkspace =
        compileInMemoryWorkspaceWithBackend "memory-contract-zig-root" "zig" source

    let dotnetWorkspace =
        compileInMemoryWorkspaceWithBackend "memory-contract-dotnet-root" "dotnet" source

    let interpreterContracts = Compilation.checkpointContracts interpreterWorkspace
    let interpreterNames = interpreterContracts |> List.map (fun contract -> contract.Name)

    Assert.Equal<string list>(Compilation.availableCheckpoints interpreterWorkspace, interpreterNames)
    Assert.DoesNotContain("zig.c", interpreterNames)
    Assert.DoesNotContain("dotnet.clr", interpreterNames)

    let runtimeContract =
        interpreterContracts
        |> List.find (fun contract -> contract.Name = "KRuntimeIR")

    Assert.Equal(ImplementationDefinedCheckpoint, runtimeContract.CheckpointKind)
    Assert.Equal(Some "KCore", runtimeContract.InputCheckpoint)
    Assert.False(runtimeContract.RequiredBySpec)
    Assert.False(runtimeContract.ProfileSpecific)

    let backendContract =
        interpreterContracts
        |> List.find (fun contract -> contract.Name = "KBackendIR")

    Assert.Equal(KBackendIRCheckpoint, backendContract.CheckpointKind)
    Assert.Equal(Some "KRuntimeIR", backendContract.InputCheckpoint)
    Assert.True(backendContract.RequiredBySpec)
    Assert.False(backendContract.ProfileSpecific)

    let zigContract =
        Compilation.checkpointContracts zigWorkspace
        |> List.find (fun contract -> contract.Name = "zig.c")

    Assert.Equal(TargetLoweringCheckpoint, zigContract.CheckpointKind)
    Assert.Equal(Some "KBackendIR", zigContract.InputCheckpoint)
    Assert.True(zigContract.RequiredBySpec)
    Assert.True(zigContract.ProfileSpecific)

    let dotnetContract =
        Compilation.checkpointContracts dotnetWorkspace
        |> List.find (fun contract -> contract.Name = "dotnet.clr")

    Assert.Equal(TargetLoweringCheckpoint, dotnetContract.CheckpointKind)
    Assert.Equal(Some "KBackendIR", dotnetContract.InputCheckpoint)
    Assert.True(dotnetContract.RequiredBySpec)
    Assert.True(dotnetContract.ProfileSpecific)

[<Fact>]
let ``pipeline trace records representation changes and verification outcomes`` () =
    let workspace =
        compileInMemoryWorkspaceWithBackend
            "memory-trace-contract-root"
            "dotnet"
            [
                "main.kp",
                [
                    "module main"
                    "let answer = 42"
                ]
                |> String.concat "\n"
            ]

    let trace = Compilation.pipelineTrace workspace

    let findStep event outputCheckpoint =
        trace
        |> List.find (fun step -> step.Event = event && step.OutputCheckpoint = outputCheckpoint)

    let parseStep = findStep PipelineTraceEvent.Parse "surface-source"
    Assert.False(parseStep.ChangedRepresentation)
    Assert.False(parseStep.VerificationAttempted)
    Assert.Equal(None, parseStep.VerificationSucceeded)

    let frontendStep = findStep PipelineTraceEvent.BuildKFrontIR "KFrontIR.RAW"
    Assert.True(frontendStep.ChangedRepresentation)
    Assert.False(frontendStep.VerificationAttempted)
    Assert.Equal(None, frontendStep.VerificationSucceeded)

    let backendVerifyStep = findStep PipelineTraceEvent.Verify "KBackendIR"
    Assert.False(backendVerifyStep.ChangedRepresentation)
    Assert.True(backendVerifyStep.VerificationAttempted)
    Assert.Equal(Some true, backendVerifyStep.VerificationSucceeded)

    let targetLoweringStep = findStep PipelineTraceEvent.LowerTarget "dotnet.clr"
    Assert.True(targetLoweringStep.ChangedRepresentation)
    Assert.False(targetLoweringStep.VerificationAttempted)
    Assert.Equal(None, targetLoweringStep.VerificationSucceeded)

    let targetVerifyStep =
        trace
        |> List.find (fun step ->
            step.Event = PipelineTraceEvent.Verify
            && step.InputCheckpoint = "dotnet.clr"
            && step.OutputCheckpoint = "dotnet.clr")

    Assert.False(targetVerifyStep.ChangedRepresentation)
    Assert.True(targetVerifyStep.VerificationAttempted)
    Assert.Equal(Some true, targetVerifyStep.VerificationSucceeded)

[<Fact>]
let ``zig backend exposes a post KBackendIR target checkpoint`` () =
    let workspace =
        compileInMemoryWorkspaceWithBackend
            "memory-zig-checkpoint-root"
            "zig"
            [
                "main.kp",
                [
                    "module main"
                    "let answer = 42"
                ]
                |> String.concat "\n"
            ]

    let checkpoints = Compilation.availableCheckpoints workspace

    Assert.Contains("zig.c", checkpoints)

    let trace =
        Compilation.pipelineTrace workspace
        |> List.map (fun step ->
            PipelineTraceEvent.toPortableName step.Event,
            PipelineTraceSubject.toPortableName step.Subject,
            step.InputCheckpoint,
            step.OutputCheckpoint)

    Assert.Contains(("lowerTarget", "targetUnit", "KBackendIR", "zig.c"), trace)

[<Fact>]
let ``stage dumps serialize checkpoints in json and sexpr`` () =
    let workspace =
        compileInMemoryWorkspace
            "memory-stage-dump-root"
            [
                "main.kp",
                [
                    "module main"
                    "let answer = 42"
                ]
                |> String.concat "\n"
            ]

    let rawJson =
        match Compilation.dumpStage workspace "KFrontIR.RAW" StageDumpFormat.Json with
        | Result.Ok dump -> dump
        | Result.Error message -> failwith message

    Assert.Contains("\"checkpoint\": \"KFrontIR.RAW\"", rawJson)
    Assert.Contains("\"phase\": \"RAW\"", rawJson)
    Assert.Contains("\"moduleIdentity\": \"main\"", rawJson)

    let runtimeSexpr =
        match Compilation.dumpStage workspace "KRuntimeIR" StageDumpFormat.SExpression with
        | Result.Ok dump -> dump
        | Result.Error message -> failwith message

    Assert.Contains("(checkpoint \"KRuntimeIR\")", runtimeSexpr)
    Assert.Contains("(module (name \"main\")", runtimeSexpr)
    Assert.Contains("(binding (name \"answer\")", runtimeSexpr)

    let backendSexpr =
        match Compilation.dumpStage workspace "KBackendIR" StageDumpFormat.SExpression with
        | Result.Ok dump -> dump
        | Result.Error message -> failwith message

    Assert.Contains("(checkpoint \"KBackendIR\")", backendSexpr)
    Assert.Contains("(module (name \"main\")", backendSexpr)
    Assert.Contains("(function (name \"answer\")", backendSexpr)

[<Fact>]
let ``stage dumps expose checkpoint contract metadata`` () =
    let workspace =
        compileInMemoryWorkspaceWithBackend
            "memory-stage-contract-root"
            "dotnet"
            [
                "main.kp",
                [
                    "module main"
                    "let answer = 42"
                ]
                |> String.concat "\n"
            ]

    let backendJson =
        match Compilation.dumpStage workspace "KBackendIR" StageDumpFormat.Json with
        | Result.Ok dump -> dump
        | Result.Error message -> failwith message

    use backendDocument = JsonDocument.Parse(backendJson)

    let backendContract =
        backendDocument.RootElement.GetProperty("checkpointContract")

    Assert.Equal("KBackendIR", backendContract.GetProperty("name").GetString())
    Assert.Equal("KBackendIR", backendContract.GetProperty("kind").GetString())
    Assert.Equal("KRuntimeIR", backendContract.GetProperty("inputCheckpoint").GetString())
    Assert.True(backendContract.GetProperty("requiredBySpec").GetBoolean())
    Assert.False(backendContract.GetProperty("profileSpecific").GetBoolean())

    let targetJson =
        match Compilation.dumpStage workspace "dotnet.clr" StageDumpFormat.Json with
        | Result.Ok dump -> dump
        | Result.Error message -> failwith message

    use targetDocument = JsonDocument.Parse(targetJson)

    let targetContract =
        targetDocument.RootElement.GetProperty("checkpointContract")

    Assert.Equal("dotnet.clr", targetContract.GetProperty("name").GetString())
    Assert.Equal("target-lowering", targetContract.GetProperty("kind").GetString())
    Assert.Equal("KBackendIR", targetContract.GetProperty("inputCheckpoint").GetString())
    Assert.True(targetContract.GetProperty("requiredBySpec").GetBoolean())
    Assert.True(targetContract.GetProperty("profileSpecific").GetBoolean())

    let runtimeSexpr =
        match Compilation.dumpStage workspace "KRuntimeIR" StageDumpFormat.SExpression with
        | Result.Ok dump -> dump
        | Result.Error message -> failwith message

    Assert.Contains("(checkpoint-contract", runtimeSexpr)
    Assert.Contains("(kind \"implementation-defined\")", runtimeSexpr)
    Assert.Contains("(input-checkpoint \"KCore\")", runtimeSexpr)
    Assert.Contains("(required-by-spec false)", runtimeSexpr)

[<Fact>]
let ``zig target checkpoint dumps a manifest for the generated translation unit`` () =
    let workspace =
        compileInMemoryWorkspaceWithBackend
            "memory-zig-stage-dump-root"
            "zig"
            [
                "main.kp",
                [
                    "module main"
                    "let answer = 42"
                ]
                |> String.concat "\n"
            ]

    let zigJson =
        match Compilation.dumpStage workspace "zig.c" StageDumpFormat.Json with
        | Result.Ok dump -> dump
        | Result.Error message -> failwith message

    Assert.Contains("\"checkpoint\": \"zig.c\"", zigJson)
    Assert.Contains("\"inputCheckpoint\": \"KBackendIR\"", zigJson)
    Assert.Contains("\"artifactKind\": \"c-translation-unit\"", zigJson)
    Assert.Contains("\"translationUnitName\": \"kappa.generated.c\"", zigJson)
    Assert.Contains("\"entrySymbols\": [", zigJson)
    Assert.Contains("kappa_module_main_answer", zigJson)

    let zigSexpr =
        match Compilation.dumpStage workspace "zig.c" StageDumpFormat.SExpression with
        | Result.Ok dump -> dump
        | Result.Error message -> failwith message

    Assert.Contains("(checkpoint \"zig.c\")", zigSexpr)
    Assert.Contains("(input-checkpoint \"KBackendIR\")", zigSexpr)
    Assert.Contains("(artifact-kind \"c-translation-unit\")", zigSexpr)
    Assert.Contains("(translation-unit-name \"kappa.generated.c\")", zigSexpr)

[<Fact>]
let ``dotnet backend exposes a post KBackendIR target checkpoint`` () =
    let workspace =
        compileInMemoryWorkspaceWithBackend
            "memory-dotnet-checkpoint-root"
            "dotnet"
            [
                "main.kp",
                [
                    "module main"
                    "let answer = 42"
                ]
                |> String.concat "\n"
            ]

    let checkpoints = Compilation.availableCheckpoints workspace

    Assert.Contains("dotnet.clr", checkpoints)

    let trace =
        Compilation.pipelineTrace workspace
        |> List.map (fun step ->
            PipelineTraceEvent.toPortableName step.Event,
            PipelineTraceSubject.toPortableName step.Subject,
            step.InputCheckpoint,
            step.OutputCheckpoint)

    Assert.Contains(("lowerTarget", "targetUnit", "KBackendIR", "dotnet.clr"), trace)
    Assert.Empty(Compilation.verifyCheckpoint workspace "dotnet.clr")

    let dotnetJson =
        match Compilation.dumpStage workspace "dotnet.clr" StageDumpFormat.Json with
        | Result.Ok dump -> dump
        | Result.Error message -> failwith message

    Assert.Contains("\"checkpoint\": \"dotnet.clr\"", dotnetJson)
    Assert.Contains("\"inputCheckpoint\": \"KBackendIR\"", dotnetJson)
    Assert.Contains("\"artifactKind\": \"clr-assembly\"", dotnetJson)
    Assert.Contains("\"translationUnitName\": \"Kappa.Generated.dll\"", dotnetJson)
    Assert.Contains("Kappa.Generated.main.answer", dotnetJson)

    let dotnetSexpr =
        match Compilation.dumpStage workspace "dotnet.clr" StageDumpFormat.SExpression with
        | Result.Ok dump -> dump
        | Result.Error message -> failwith message

    Assert.Contains("(checkpoint \"dotnet.clr\")", dotnetSexpr)
    Assert.Contains("(input-checkpoint \"KBackendIR\")", dotnetSexpr)
    Assert.Contains("(artifact-kind \"clr-assembly\")", dotnetSexpr)

[<Fact>]
let ``workspace and stage dumps expose backend intrinsic identity`` () =
    let supportedWorkspace =
        compileInMemoryWorkspaceWithBackend
            "memory-intrinsic-metadata-supported-root"
            "interpreter"
            [
                "main.kp",
                [
                    "module main"
                    "let answer = 42"
                ]
                |> String.concat "\n"
            ]

    Assert.Equal("bootstrap-prelude-v1", supportedWorkspace.BackendIntrinsicIdentity)
    Assert.Contains("backendProfile=interpreter", supportedWorkspace.BuildConfigurationIdentity)
    Assert.Contains("backendIntrinsicSet=bootstrap-prelude-v1", supportedWorkspace.BuildConfigurationIdentity)

    let supportedJson =
        match Compilation.dumpStage supportedWorkspace "KCore" StageDumpFormat.Json with
        | Result.Ok dump -> dump
        | Result.Error message -> failwith message

    Assert.Contains("\"backendProfile\": \"interpreter\"", supportedJson)
    Assert.Contains("\"backendIntrinsicSet\": \"bootstrap-prelude-v1\"", supportedJson)
    Assert.Contains("\"identity\": \"packageMode=true;backendProfile=interpreter;backendIntrinsicSet=bootstrap-prelude-v1", supportedJson)

    let unsupportedWorkspace =
        compileInMemoryWorkspaceWithBackend
            "memory-intrinsic-metadata-unsupported-root"
            "custom-backend"
            [
                "main.kp",
                [
                    "module main"
                    "let answer = 42"
                ]
                |> String.concat "\n"
            ]

    Assert.Equal("none", unsupportedWorkspace.BackendIntrinsicIdentity)
    Assert.Contains("backendProfile=custom-backend", unsupportedWorkspace.BuildConfigurationIdentity)
    Assert.Contains("backendIntrinsicSet=none", unsupportedWorkspace.BuildConfigurationIdentity)

    let unsupportedSexpr =
        match Compilation.dumpStage unsupportedWorkspace "KCore" StageDumpFormat.SExpression with
        | Result.Ok dump -> dump
        | Result.Error message -> failwith message

    Assert.Contains("(backend-profile \"custom-backend\")", unsupportedSexpr)
    Assert.Contains("(backend-intrinsic-set \"none\")", unsupportedSexpr)
    Assert.Contains("(identity \"packageMode=true;backendProfile=custom-backend;backendIntrinsicSet=none", unsupportedSexpr)

[<Fact>]
let ``workspace and stage dumps expose deployment mode in build identity`` () =
    let root = Path.GetFullPath("memory-deployment-identity-root")

    let fileSystem =
        InMemoryFileSystem(
            [
                Path.Combine(root, "main.kp"),
                [
                    "module main"
                    "let answer = 42"
                ]
                |> String.concat "\n"
            ]
        )

    let options =
        { CompilationOptions.createWithFileSystem fileSystem root with
            BackendProfile = "dotnet"
            DeploymentMode = "native-aot" }

    let workspace = Compilation.parse options [ root ]

    Assert.Equal("native-aot", workspace.DeploymentMode)
    Assert.Contains("deploymentMode=native-aot", workspace.BuildConfigurationIdentity)

    let backendJson =
        match Compilation.dumpStage workspace "KBackendIR" StageDumpFormat.Json with
        | Result.Ok dump -> dump
        | Result.Error message -> failwith message

    Assert.Contains("\"deploymentMode\": \"native-aot\"", backendJson)
    Assert.Contains("deploymentMode=native-aot", backendJson)

    let backendSexpr =
        match Compilation.dumpStage workspace "KBackendIR" StageDumpFormat.SExpression with
        | Result.Ok dump -> dump
        | Result.Error message -> failwith message

    Assert.Contains("(deployment-mode \"native-aot\")", backendSexpr)
    Assert.Contains("deploymentMode=native-aot", backendSexpr)

[<Fact>]
let ``workspace and stage dumps expose elaboration available intrinsic terms`` () =
    let supportedWorkspace =
        compileInMemoryWorkspaceWithBackend
            "memory-elaboration-intrinsics-supported-root"
            "interpreter"
            [
                "main.kp",
                [
                    "module main"
                    "let answer = 42"
                ]
                |> String.concat "\n"
            ]

    Assert.Contains("not", supportedWorkspace.ElaborationAvailableIntrinsicTerms)
    Assert.DoesNotContain("printInt", supportedWorkspace.ElaborationAvailableIntrinsicTerms)

    let supportedJson =
        match Compilation.dumpStage supportedWorkspace "KCore" StageDumpFormat.Json with
        | Result.Ok dump -> dump
        | Result.Error message -> failwith message

    use supportedDocument = JsonDocument.Parse(supportedJson)

    let supportedTerms =
        supportedDocument.RootElement.GetProperty("elaborationAvailableIntrinsicTerms").EnumerateArray()
        |> Seq.map (fun item -> item.GetString())
        |> Seq.filter (isNull >> not)
        |> Seq.toList

    Assert.Contains("not", supportedTerms)
    Assert.DoesNotContain("printInt", supportedTerms)

    let unsupportedWorkspace =
        compileInMemoryWorkspaceWithBackend
            "memory-elaboration-intrinsics-unsupported-root"
            "custom-backend"
            [
                "main.kp",
                [
                    "module main"
                    "let answer = 42"
                ]
                |> String.concat "\n"
            ]

    Assert.Empty(unsupportedWorkspace.ElaborationAvailableIntrinsicTerms)

    let unsupportedSexpr =
        match Compilation.dumpStage unsupportedWorkspace "KCore" StageDumpFormat.SExpression with
        | Result.Ok dump -> dump
        | Result.Error message -> failwith message

    Assert.Contains("(elaboration-available-intrinsic-terms)", unsupportedSexpr)

[<Fact>]
let ``checkpoint verification is available for frontend core and backend snapshots`` () =
    let workspace =
        compileInMemoryWorkspace
            "memory-verify-root"
            [
                "main.kp",
                [
                    "module main"
                    "let answer = 42"
                ]
                |> String.concat "\n"
            ]

    Assert.Empty(Compilation.verifyCheckpoint workspace "KFrontIR.CHECKERS")
    Assert.Empty(Compilation.verifyCheckpoint workspace "KCore")
    Assert.Empty(Compilation.verifyCheckpoint workspace "KRuntimeIR")
    Assert.Empty(Compilation.verifyCheckpoint workspace "KBackendIR")

[<Fact>]
let ``verify all checkpoints follows checkpoint contracts for each backend profile`` () =
    let source =
        [
            "main.kp",
            [
                "module main"
                "let answer = 42"
            ]
            |> String.concat "\n"
        ]

    for backendProfile in [ "interpreter"; "zig"; "dotnet" ] do
        let workspace =
            compileInMemoryWorkspaceWithBackend $"memory-verify-all-{backendProfile}-root" backendProfile source

        let contractNames =
            Compilation.checkpointContracts workspace
            |> List.map (fun contract -> contract.Name)

        let results = Compilation.verifyAllCheckpoints workspace

        Assert.Equal<string list>(contractNames, results |> List.map (fun result -> result.Checkpoint))

        for result in results do
            if not result.Succeeded then
                let diagnosticText =
                    result.Diagnostics
                    |> List.map (fun diagnostic -> diagnostic.Message)
                    |> String.concat "\n"

                failwithf "Expected checkpoint '%s' for backend '%s' to verify, got:\n%s" result.Checkpoint backendProfile diagnosticText

[<Fact>]
let ``verify all checkpoints reports target failures after malformed KBackendIR`` () =
    let workspace =
        compileInMemoryWorkspaceWithBackend
            "memory-verify-all-target-failure-root"
            "dotnet"
            [
                "main.kp",
                [
                    "module main"
                    "let answer = 42"
                ]
                |> String.concat "\n"
            ]

    let malformedWorkspace = { workspace with KBackendIR = [] }

    let results = Compilation.verifyAllCheckpoints malformedWorkspace

    let backendResult =
        results |> List.find (fun result -> result.Checkpoint = "KBackendIR")

    Assert.False(backendResult.Succeeded)

    let targetResult =
        results |> List.find (fun result -> result.Checkpoint = "dotnet.clr")

    Assert.False(targetResult.Succeeded)

    Assert.Contains(
        targetResult.Diagnostics,
        fun diagnostic ->
            diagnostic.Message.Contains("Cannot emit CLR target manifest", StringComparison.OrdinalIgnoreCase)
    )

[<Fact>]
let ``zig target checkpoint verification is available`` () =
    let workspace =
        compileInMemoryWorkspaceWithBackend
            "memory-zig-verify-root"
            "zig"
            [
                "main.kp",
                [
                    "module main"
                    "let answer = 42"
                ]
                |> String.concat "\n"
            ]

    Assert.Empty(Compilation.verifyCheckpoint workspace "zig.c")

[<Fact>]
let ``backend verification rejects missing backend modules`` () =
    let workspace =
        compileInMemoryWorkspace
            "memory-verify-missing-backend-module-root"
            [
                "main.kp",
                [
                    "module main"
                    "let answer = 42"
                ]
                |> String.concat "\n"
            ]

    let malformedWorkspace =
        { workspace with
            KBackendIR =
                workspace.KBackendIR
                |> List.filter (fun moduleDump -> not (String.Equals(moduleDump.Name, "main", StringComparison.Ordinal))) }

    let diagnostics = Compilation.verifyCheckpoint malformedWorkspace "KBackendIR"

    Assert.Contains(
        diagnostics,
        fun diagnostic ->
            diagnostic.Message.Contains("backend module for runtime module 'main'", StringComparison.OrdinalIgnoreCase)
    )

[<Fact>]
let ``backend verification rejects unsupported backend intrinsics`` () =
    let workspace =
        compileInMemoryWorkspace
            "memory-verify-unsupported-intrinsic-root"
            [
                "main.kp",
                [
                    "module main"
                    "let answer = 42"
                ]
                |> String.concat "\n"
            ]

    let malformedPreludeBinding =
        { Name = "mysteryIntrinsic"
          Parameters = []
          Body = None
          Intrinsic = true
          Provenance =
            { FilePath = Stdlib.BundledPreludeVirtualPath
              ModuleName = Stdlib.PreludeModuleText
              DeclarationName = Some "mysteryIntrinsic"
              IntroductionKind = "intrinsic" } }

    let malformedWorkspace =
        { workspace with
            KRuntimeIR =
                workspace.KRuntimeIR
                |> List.map (fun moduleDump ->
                    if moduleDump.Name = Stdlib.PreludeModuleText then
                        { moduleDump with
                            IntrinsicTerms = "mysteryIntrinsic" :: moduleDump.IntrinsicTerms
                            Exports = "mysteryIntrinsic" :: moduleDump.Exports
                            Bindings = malformedPreludeBinding :: moduleDump.Bindings }
                    else
                        moduleDump) }

    let diagnostics = Compilation.verifyCheckpoint malformedWorkspace "KRuntimeIR"

    Assert.Contains(
        diagnostics,
        fun diagnostic ->
            diagnostic.Message.Contains("intrinsic term 'mysteryIntrinsic'", StringComparison.OrdinalIgnoreCase)
            && diagnostic.Message.Contains("provided by backend profile", StringComparison.OrdinalIgnoreCase)
    )

[<Fact>]
let ``backend verification rejects missing imported runtime modules`` () =
    let workspace =
        compileInMemoryWorkspace
            "memory-verify-missing-import-root"
            [
                "main.kp",
                [
                    "module main"
                    "let answer = 42"
                ]
                |> String.concat "\n"
            ]

    let missingImport =
        { Source = Dotted [ "missing" ]
          Alias = None
          Selection = All }

    let malformedWorkspace =
        { workspace with
            KRuntimeIR =
                workspace.KRuntimeIR
                |> List.map (fun moduleDump ->
                    if moduleDump.Name = "main" then
                        { moduleDump with Imports = missingImport :: moduleDump.Imports }
                    else
                        moduleDump) }

    let diagnostics = Compilation.verifyCheckpoint malformedWorkspace "KRuntimeIR"

    Assert.Contains(
        diagnostics,
        fun diagnostic ->
            diagnostic.Message.Contains("imported runtime module 'missing'", StringComparison.OrdinalIgnoreCase)
            && diagnostic.Message.Contains("module 'main'", StringComparison.OrdinalIgnoreCase)
    )

[<Fact>]
let ``backend verification rejects runtime bindings without bodies`` () =
    let workspace =
        compileInMemoryWorkspace
            "memory-verify-runtime-body-root"
            [
                "main.kp",
                [
                    "module main"
                    "let answer = 42"
                ]
                |> String.concat "\n"
            ]

    let malformedWorkspace =
        { workspace with
            KRuntimeIR =
                workspace.KRuntimeIR
                |> List.map (fun moduleDump ->
                    if moduleDump.Name = "main" then
                        { moduleDump with
                            Bindings =
                                moduleDump.Bindings
                                |> List.map (fun binding ->
                                    if binding.Name = "answer" then
                                        { binding with Body = None }
                                    else
                                        binding) }
                    else
                        moduleDump) }

    let diagnostics = Compilation.verifyCheckpoint malformedWorkspace "KRuntimeIR"

    Assert.Contains(
        diagnostics,
        fun diagnostic ->
            diagnostic.Message.Contains("runtime binding 'main.answer'", StringComparison.OrdinalIgnoreCase)
            && diagnostic.Message.Contains("have a body", StringComparison.OrdinalIgnoreCase)
    )

[<Fact>]
let ``backend verification rejects intrinsic bindings with bodies`` () =
    let workspace =
        compileInMemoryWorkspace
            "memory-verify-intrinsic-body-root"
            [
                "main.kp",
                [
                    "module main"
                    "let answer = 42"
                ]
                |> String.concat "\n"
            ]

    let preludeModule =
        workspace.KRuntimeIR
        |> List.find (fun moduleDump -> moduleDump.Name = Stdlib.PreludeModuleText)

    let intrinsicName =
        preludeModule.Bindings
        |> List.find (fun binding -> binding.Intrinsic)
        |> fun binding -> binding.Name

    let malformedWorkspace =
        { workspace with
            KRuntimeIR =
                workspace.KRuntimeIR
                |> List.map (fun moduleDump ->
                    if moduleDump.Name = Stdlib.PreludeModuleText then
                        { moduleDump with
                            Bindings =
                                moduleDump.Bindings
                                |> List.map (fun binding ->
                                    if binding.Name = intrinsicName then
                                        { binding with Body = Some(KRuntimeLiteral LiteralValue.Unit) }
                                    else
                                        binding) }
                    else
                        moduleDump) }

    let diagnostics = Compilation.verifyCheckpoint malformedWorkspace "KRuntimeIR"

    Assert.Contains(
        diagnostics,
        fun diagnostic ->
            diagnostic.Message.Contains(
                $"intrinsic binding '{Stdlib.PreludeModuleText}.{intrinsicName}'",
                StringComparison.OrdinalIgnoreCase
            )
            && diagnostic.Message.Contains("omit a body", StringComparison.OrdinalIgnoreCase)
    )

[<Fact>]
let ``backend verification rejects intrinsic bindings missing intrinsic listings`` () =
    let workspace =
        compileInMemoryWorkspace
            "memory-verify-intrinsic-listing-root"
            [
                "main.kp",
                [
                    "module main"
                    "let answer = 42"
                ]
                |> String.concat "\n"
            ]

    let preludeModule =
        workspace.KRuntimeIR
        |> List.find (fun moduleDump -> moduleDump.Name = Stdlib.PreludeModuleText)

    let intrinsicName =
        preludeModule.Bindings
        |> List.find (fun binding -> binding.Intrinsic)
        |> fun binding -> binding.Name

    let malformedWorkspace =
        { workspace with
            KRuntimeIR =
                workspace.KRuntimeIR
                |> List.map (fun moduleDump ->
                    if moduleDump.Name = Stdlib.PreludeModuleText then
                        { moduleDump with
                            IntrinsicTerms =
                                moduleDump.IntrinsicTerms
                                |> List.filter (fun name -> not (String.Equals(name, intrinsicName, StringComparison.Ordinal))) }
                    else
                        moduleDump) }

    let diagnostics = Compilation.verifyCheckpoint malformedWorkspace "KRuntimeIR"

    Assert.Contains(
        diagnostics,
        fun diagnostic ->
            diagnostic.Message.Contains(
                $"intrinsic binding '{Stdlib.PreludeModuleText}.{intrinsicName}'",
                StringComparison.OrdinalIgnoreCase
            )
            && diagnostic.Message.Contains("listed in module intrinsic terms", StringComparison.OrdinalIgnoreCase)
    )

[<Fact>]
let ``backend verification rejects duplicate closure parameters`` () =
    let workspace =
        compileInMemoryWorkspace
            "memory-verify-closure-parameters-root"
            [
                "main.kp",
                [
                    "module main"
                    "let answer = 42"
                ]
                |> String.concat "\n"
            ]

    let malformedWorkspace =
        { workspace with
            KRuntimeIR =
                workspace.KRuntimeIR
                |> List.map (fun moduleDump ->
                    if moduleDump.Name = "main" then
                        { moduleDump with
                            Bindings =
                                moduleDump.Bindings
                                |> List.map (fun binding ->
                                    if binding.Name = "answer" then
                                        { binding with Body = Some(KRuntimeClosure([ "x"; "x" ], KRuntimeName [ "x" ])) }
                                    else
                                        binding) }
                    else
                        moduleDump) }

    let diagnostics = Compilation.verifyCheckpoint malformedWorkspace "KRuntimeIR"

    Assert.Contains(
        diagnostics,
        fun diagnostic ->
            diagnostic.Message.Contains("closures in 'main.answer'", StringComparison.OrdinalIgnoreCase)
            && diagnostic.Message.Contains("unique parameter names", StringComparison.OrdinalIgnoreCase)
            && diagnostic.Message.Contains("'x' was duplicated", StringComparison.OrdinalIgnoreCase)
    )

[<Fact>]
let ``backend verification rejects duplicate pattern binders`` () =
    let workspace =
        compileInMemoryWorkspace
            "memory-verify-pattern-binders-root"
            [
                "main.kp",
                [
                    "module main"
                    "let answer = 42"
                ]
                |> String.concat "\n"
            ]

    let malformedBody =
        KRuntimeMatch(
            KRuntimeLiteral LiteralValue.Unit,
            [
                { Pattern =
                    KRuntimeConstructorPattern(
                        [ "::" ],
                        [ KRuntimeNamePattern "x"; KRuntimeNamePattern "x" ]
                    )
                  Body = KRuntimeLiteral(LiteralValue.Integer 0L) }
            ]
        )

    let malformedWorkspace =
        { workspace with
            KRuntimeIR =
                workspace.KRuntimeIR
                |> List.map (fun moduleDump ->
                    if moduleDump.Name = "main" then
                        { moduleDump with
                            Bindings =
                                moduleDump.Bindings
                                |> List.map (fun binding ->
                                    if binding.Name = "answer" then
                                        { binding with Body = Some malformedBody }
                                    else
                                        binding) }
                    else
                        moduleDump) }

    let diagnostics = Compilation.verifyCheckpoint malformedWorkspace "KRuntimeIR"

    Assert.Contains(
        diagnostics,
        fun diagnostic ->
            diagnostic.Message.Contains("pattern binder names", StringComparison.OrdinalIgnoreCase)
            && diagnostic.Message.Contains("'main.answer'", StringComparison.OrdinalIgnoreCase)
            && diagnostic.Message.Contains("'x' was duplicated", StringComparison.OrdinalIgnoreCase)
    )

[<Fact>]
let ``backend verification rejects malformed calling conventions`` () =
    let workspace =
        compileInMemoryWorkspace
            "memory-verify-backend-calling-convention-root"
            [
                "main.kp",
                [
                    "module main"
                    "let answer = 42"
                ]
                |> String.concat "\n"
            ]

    let malformedWorkspace =
        { workspace with
            KBackendIR =
                workspace.KBackendIR
                |> List.map (fun moduleDump ->
                    if moduleDump.Name = "main" then
                        { moduleDump with
                            Functions =
                                moduleDump.Functions
                                |> List.map (fun binding ->
                                    if binding.Name = "answer" then
                                        { binding with
                                            CallingConvention =
                                                { binding.CallingConvention with
                                                    RuntimeArity = binding.CallingConvention.RuntimeArity + 1 } }
                                    else
                                        binding) }
                    else
                        moduleDump) }

    let diagnostics = Compilation.verifyCheckpoint malformedWorkspace "KBackendIR"

    Assert.Contains(
        diagnostics,
        fun diagnostic ->
            diagnostic.Message.Contains("function 'main.answer'", StringComparison.OrdinalIgnoreCase)
            && diagnostic.Message.Contains("calling convention arity", StringComparison.OrdinalIgnoreCase)
    )

[<Fact>]
let ``workspace materializes frontend core and backend modules`` () =
    let workspace =
        compileInMemoryWorkspace
            "memory-materialized-stage-root"
            [
                "main.kp",
                [
                    "module main"
                    "let answer = 20 + 22"
                ]
                |> String.concat "\n"
            ]

    let frontendModule =
        workspace.KFrontIR
        |> List.find (fun moduleDump -> moduleDump.ModuleIdentity = Some [ "main" ])

    Assert.Contains(CHECKERS, frontendModule.ResolvedPhases)

    let coreModule =
        workspace.KCore
        |> List.find (fun moduleDump -> moduleDump.Name = "main")

    let coreBinding =
        coreModule.Declarations
        |> List.pick (fun declaration ->
            match declaration.Binding with
            | Some binding when binding.Name = Some "answer" -> Some binding
            | _ -> None)

    match coreBinding.Body with
    | Some(KCoreBinary(KCoreLiteral(LiteralValue.Integer 20L), "+", KCoreLiteral(LiteralValue.Integer 22L))) -> ()
    | other ->
        failwithf "Unexpected KCore body: %A" other

    let runtimeModule =
        workspace.KRuntimeIR
        |> List.find (fun moduleDump -> moduleDump.Name = "main")

    let runtimeBinding =
        runtimeModule.Bindings
        |> List.find (fun binding -> binding.Name = "answer")

    match runtimeBinding.Body with
    | Some(KRuntimeBinary(KRuntimeLiteral(LiteralValue.Integer 20L), "+", KRuntimeLiteral(LiteralValue.Integer 22L))) -> ()
    | other ->
        failwithf "Unexpected KRuntimeIR body: %A" other

    let backendModule =
        workspace.KBackendIR
        |> List.find (fun moduleDump -> moduleDump.Name = "main")

    let backendBinding =
        backendModule.Functions
        |> List.find (fun binding -> binding.Name = "answer")

    match backendBinding.Body with
    | Some(
        BackendCall(
            BackendName(BackendIntrinsicName(moduleName, "+", Some BackendRepInt64)),
            [ BackendLiteral(LiteralValue.Integer 20L, BackendRepInt64)
              BackendLiteral(LiteralValue.Integer 22L, BackendRepInt64) ],
            _,
            BackendRepInt64
        )
      ) ->
        Assert.Equal(Stdlib.PreludeModuleText, moduleName)
    | other ->
        failwithf "Unexpected KBackendIR function body: %A" other

[<Fact>]
let ``backend lowering resolves user defined operators through ordinary bindings`` () =
    let workspace =
        compileInMemoryWorkspace
            "memory-backend-user-operator-root"
            [
                "main.kp",
                [
                    "module main"
                    "infix left 60 (++)"
                    "let (++) x y = x + y"
                    "let result = 20 ++ 22"
                ]
                |> String.concat "\n"
            ]

    Assert.Empty(Compilation.verifyCheckpoint workspace "KBackendIR")

    let backendModule =
        workspace.KBackendIR
        |> List.find (fun moduleDump -> moduleDump.Name = "main")

    let resultBinding =
        backendModule.Functions
        |> List.find (fun binding -> binding.Name = "result")

    match resultBinding.Body with
    | Some(
        BackendCall(
            BackendName(BackendGlobalBindingName(moduleName, "++", _)),
            [ BackendLiteral(LiteralValue.Integer 20L, _)
              BackendLiteral(LiteralValue.Integer 22L, _) ],
            _,
            _
        )
      ) ->
        Assert.Equal("main", moduleName)
    | other ->
        failwithf "Unexpected backend operator lowering: %A" other

[<Fact>]
let ``backend verification accepts recursive list matches lowered through constructors`` () =
    let workspace =
        compileInMemoryWorkspaceWithBackend
            "memory-backend-list-root"
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

    Assert.Empty(Compilation.verifyCheckpoint workspace "KBackendIR")

    let backendModule =
        workspace.KBackendIR
        |> List.find (fun moduleDump -> moduleDump.Name = "main")

    let resultBinding =
        backendModule.Functions
        |> List.find (fun binding -> binding.Name = "result")

    match resultBinding.Body with
    | Some(
        BackendCall(
            BackendName(BackendGlobalBindingName("main", "sumList", _)),
            [ BackendConstructData(_, "List", "::", _, _, _) ],
            _,
            _
        )
      ) -> ()
    | other ->
        failwithf "Unexpected recursive list backend lowering: %A" other

[<Fact>]
let ``core and backend artifacts remain usable after source documents are discarded`` () =
    let workspace =
        compileInMemoryWorkspace
            "memory-materialized-pipeline-root"
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

    let materializedOnlyWorkspace =
        { workspace with
            Documents = [] }

    let coreJson =
        match Compilation.dumpStage materializedOnlyWorkspace "KCore" StageDumpFormat.Json with
        | Result.Ok dump -> dump
        | Result.Error message -> failwith message

    Assert.Contains("\"checkpoint\": \"KCore\"", coreJson)
    Assert.Contains("\"name\": \"main\"", coreJson)
    Assert.Empty(Compilation.verifyCheckpoint materializedOnlyWorkspace "KCore")
    Assert.Empty(Compilation.verifyCheckpoint materializedOnlyWorkspace "KRuntimeIR")
    Assert.Empty(Compilation.verifyCheckpoint materializedOnlyWorkspace "KBackendIR")

    match Interpreter.evaluateBinding materializedOnlyWorkspace "main.result" with
    | Result.Ok value ->
        Assert.Equal("42", RuntimeValue.format value)
    | Result.Error issue ->
        failwithf "Expected KRuntimeIR execution to succeed, got %s" issue.Message
