module ObservabilityTests

open System
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
    Assert.Contains("printInt", supportedWorkspace.ElaborationAvailableIntrinsicTerms)

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
    Assert.Contains("printInt", supportedTerms)

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
            BackendName(BackendIntrinsicName(_, "+", Some BackendRepInt64)),
            [ BackendLiteral(LiteralValue.Integer 20L, BackendRepInt64)
              BackendLiteral(LiteralValue.Integer 22L, BackendRepInt64) ],
            _,
            BackendRepInt64
        )
      ) -> ()
    | other ->
        failwithf "Unexpected KBackendIR function body: %A" other

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
