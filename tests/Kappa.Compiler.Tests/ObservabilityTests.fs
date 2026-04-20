module ObservabilityTests

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
    Assert.Contains(("lowerKBackendIR", "KCoreUnit", "KCore", "KBackendIR"), trace)

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

    let backendSexpr =
        match Compilation.dumpStage workspace "KBackendIR" StageDumpFormat.SExpression with
        | Result.Ok dump -> dump
        | Result.Error message -> failwith message

    Assert.Contains("(checkpoint \"KBackendIR\")", backendSexpr)
    Assert.Contains("(module (name \"main\")", backendSexpr)
    Assert.Contains("(binding (name \"answer\")", backendSexpr)

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

    Assert.Equal("prelude-core-v1", supportedWorkspace.BackendIntrinsicIdentity)

    let supportedJson =
        match Compilation.dumpStage supportedWorkspace "KCore" StageDumpFormat.Json with
        | Result.Ok dump -> dump
        | Result.Error message -> failwith message

    Assert.Contains("\"backendProfile\": \"interpreter\"", supportedJson)
    Assert.Contains("\"backendIntrinsicSet\": \"prelude-core-v1\"", supportedJson)

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

    let unsupportedSexpr =
        match Compilation.dumpStage unsupportedWorkspace "KCore" StageDumpFormat.SExpression with
        | Result.Ok dump -> dump
        | Result.Error message -> failwith message

    Assert.Contains("(backend-profile \"custom-backend\")", unsupportedSexpr)
    Assert.Contains("(backend-intrinsic-set \"none\")", unsupportedSexpr)

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
    Assert.Empty(Compilation.verifyCheckpoint workspace "KBackendIR")

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

    let backendModule =
        workspace.KBackendIR
        |> List.find (fun moduleDump -> moduleDump.Name = "main")

    let backendBinding =
        backendModule.Bindings
        |> List.find (fun binding -> binding.Name = "answer")

    match backendBinding.Body with
    | Some(KBackendBinary(KBackendLiteral(LiteralValue.Integer 20L), "+", KBackendLiteral(LiteralValue.Integer 22L))) -> ()
    | other ->
        failwithf "Unexpected KBackendIR body: %A" other

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
    Assert.Empty(Compilation.verifyCheckpoint materializedOnlyWorkspace "KBackendIR")

    match Interpreter.evaluateBinding materializedOnlyWorkspace "main.result" with
    | Result.Ok value ->
        Assert.Equal("42", RuntimeValue.format value)
    | Result.Error issue ->
        failwithf "Expected KBackendIR execution to succeed, got %s" issue.Message
