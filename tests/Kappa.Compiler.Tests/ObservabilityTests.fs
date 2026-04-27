// Verifies checkpoints, dumps, traces, fingerprints, and other observability contracts.
module ObservabilityTests

open System
open System.IO
open System.Text.Json
open Kappa.Compiler
open Harness
open Xunit

let private hasDiagnosticCode code (diagnostic: Diagnostic) =
    diagnostic.Code = code

let private diagnosticsText diagnostics =
    diagnostics
    |> List.map (fun diagnostic -> diagnostic.Message)
    |> String.concat Environment.NewLine

let rec private containsCoreSyntheticRecordApply expression =
    match expression with
    | KCoreAppSpine(KCoreName [ constructorName ], arguments)
        when constructorName.StartsWith("__kappa_record_", StringComparison.Ordinal) ->
        true
    | KCoreAppSpine(callee, arguments) ->
        containsCoreSyntheticRecordApply callee
        || (arguments |> List.exists (fun argument -> containsCoreSyntheticRecordApply argument.Expression))
    | KCoreLet(_, value, body) ->
        containsCoreSyntheticRecordApply value || containsCoreSyntheticRecordApply body
    | KCoreDoScope(_, body)
    | KCoreExecute body ->
        containsCoreSyntheticRecordApply body
    | KCoreScheduleExit(_, KCoreDeferred deferred, body) ->
        containsCoreSyntheticRecordApply deferred || containsCoreSyntheticRecordApply body
    | KCoreScheduleExit(_, KCoreRelease(_, release, resource), body) ->
        containsCoreSyntheticRecordApply release
        || containsCoreSyntheticRecordApply resource
        || containsCoreSyntheticRecordApply body
    | KCoreEffectLabel _ ->
        false
    | KCoreEffectOperation(label, _) ->
        containsCoreSyntheticRecordApply label
    | KCoreSequence(first, second)
    | KCoreBinary(first, _, second) ->
        containsCoreSyntheticRecordApply first || containsCoreSyntheticRecordApply second
    | KCoreSyntaxQuote inner
    | KCoreSyntaxSplice inner
    | KCoreTopLevelSyntaxSplice inner
    | KCoreCodeQuote inner
    | KCoreCodeSplice inner ->
        containsCoreSyntheticRecordApply inner
    | KCoreIfThenElse(condition, whenTrue, whenFalse) ->
        containsCoreSyntheticRecordApply condition
        || containsCoreSyntheticRecordApply whenTrue
        || containsCoreSyntheticRecordApply whenFalse
    | KCoreHandle(_, label, body, returnClause, operationClauses) ->
        containsCoreSyntheticRecordApply label
        || containsCoreSyntheticRecordApply body
        || containsCoreSyntheticRecordApply returnClause.Body
        || (operationClauses |> List.exists (fun clause -> containsCoreSyntheticRecordApply clause.Body))
    | KCoreMatch(scrutinee, cases) ->
        containsCoreSyntheticRecordApply scrutinee
        || (cases
            |> List.exists (fun caseClause ->
                caseClause.Guard |> Option.exists containsCoreSyntheticRecordApply
                || containsCoreSyntheticRecordApply caseClause.Body))
    | KCoreWhile(condition, body) ->
        containsCoreSyntheticRecordApply condition || containsCoreSyntheticRecordApply body
    | KCoreUnary(_, operand) ->
        containsCoreSyntheticRecordApply operand
    | KCoreTraitCall(_, _, dictionary, arguments) ->
        containsCoreSyntheticRecordApply dictionary || (arguments |> List.exists containsCoreSyntheticRecordApply)
    | KCoreLambda(_, body) ->
        containsCoreSyntheticRecordApply body
    | KCorePrefixedString(_, parts) ->
        parts
        |> List.exists (function
            | KCoreStringText _ -> false
            | KCoreStringInterpolation inner -> containsCoreSyntheticRecordApply inner)
    | KCoreLiteral _
    | KCoreName _
    | KCoreStaticObject _
    | KCoreDictionaryValue _ ->
        false

let rec private containsRuntimeSyntheticRecordApply expression =
    match expression with
    | KRuntimeApply(KRuntimeName [ constructorName ], arguments)
        when constructorName.StartsWith("__kappa_record_", StringComparison.Ordinal) ->
        true
    | KRuntimeApply(callee, arguments) ->
        containsRuntimeSyntheticRecordApply callee || (arguments |> List.exists containsRuntimeSyntheticRecordApply)
    | KRuntimeLet(_, value, body) ->
        containsRuntimeSyntheticRecordApply value || containsRuntimeSyntheticRecordApply body
    | KRuntimeDoScope(_, body)
    | KRuntimeExecute body ->
        containsRuntimeSyntheticRecordApply body
    | KRuntimeScheduleExit(_, KRuntimeDeferred deferred, body) ->
        containsRuntimeSyntheticRecordApply deferred || containsRuntimeSyntheticRecordApply body
    | KRuntimeScheduleExit(_, KRuntimeRelease(_, release, resource), body) ->
        containsRuntimeSyntheticRecordApply release
        || containsRuntimeSyntheticRecordApply resource
        || containsRuntimeSyntheticRecordApply body
    | KRuntimeEffectLabel _ ->
        false
    | KRuntimeEffectOperation(label, _) ->
        containsRuntimeSyntheticRecordApply label
    | KRuntimeSequence(first, second)
    | KRuntimeBinary(first, _, second) ->
        containsRuntimeSyntheticRecordApply first || containsRuntimeSyntheticRecordApply second
    | KRuntimeIfThenElse(condition, whenTrue, whenFalse) ->
        containsRuntimeSyntheticRecordApply condition
        || containsRuntimeSyntheticRecordApply whenTrue
        || containsRuntimeSyntheticRecordApply whenFalse
    | KRuntimeHandle(_, label, body, returnClause, operationClauses) ->
        containsRuntimeSyntheticRecordApply label
        || containsRuntimeSyntheticRecordApply body
        || containsRuntimeSyntheticRecordApply returnClause.Body
        || (operationClauses |> List.exists (fun clause -> containsRuntimeSyntheticRecordApply clause.Body))
    | KRuntimeMatch(scrutinee, cases) ->
        containsRuntimeSyntheticRecordApply scrutinee
        || (cases
            |> List.exists (fun caseClause ->
                caseClause.Guard |> Option.exists containsRuntimeSyntheticRecordApply
                || containsRuntimeSyntheticRecordApply caseClause.Body))
    | KRuntimeWhile(condition, body) ->
        containsRuntimeSyntheticRecordApply condition || containsRuntimeSyntheticRecordApply body
    | KRuntimeUnary(_, operand) ->
        containsRuntimeSyntheticRecordApply operand
    | KRuntimeTraitCall(_, _, dictionary, arguments) ->
        containsRuntimeSyntheticRecordApply dictionary || (arguments |> List.exists containsRuntimeSyntheticRecordApply)
    | KRuntimeClosure(_, body) ->
        containsRuntimeSyntheticRecordApply body
    | KRuntimePrefixedString(_, parts) ->
        parts
        |> List.exists (function
            | KRuntimeStringText _ -> false
            | KRuntimeStringInterpolation(inner, _) -> containsRuntimeSyntheticRecordApply inner)
    | KRuntimeLiteral _
    | KRuntimeName _
    | KRuntimeDictionaryValue _ ->
        false

let rec private reintroduceDeepRuntimeHandle expression =
    match expression with
    | KRuntimeHandle(false, label, body, returnClause, operationClauses) ->
        KRuntimeHandle(true, label, body, returnClause, operationClauses)
    | KRuntimeHandle(isDeep, label, body, returnClause, operationClauses) ->
        KRuntimeHandle(
            isDeep,
            reintroduceDeepRuntimeHandle label,
            reintroduceDeepRuntimeHandle body,
            { returnClause with Body = reintroduceDeepRuntimeHandle returnClause.Body },
            operationClauses
            |> List.map (fun clause -> { clause with Body = reintroduceDeepRuntimeHandle clause.Body })
        )
    | KRuntimeClosure(parameters, body) ->
        KRuntimeClosure(parameters, reintroduceDeepRuntimeHandle body)
    | KRuntimeIfThenElse(condition, whenTrue, whenFalse) ->
        KRuntimeIfThenElse(
            reintroduceDeepRuntimeHandle condition,
            reintroduceDeepRuntimeHandle whenTrue,
            reintroduceDeepRuntimeHandle whenFalse
        )
    | KRuntimeMatch(scrutinee, cases) ->
        KRuntimeMatch(
            reintroduceDeepRuntimeHandle scrutinee,
            cases
            |> List.map (fun caseClause ->
                { caseClause with
                    Guard = caseClause.Guard |> Option.map reintroduceDeepRuntimeHandle
                    Body = reintroduceDeepRuntimeHandle caseClause.Body })
        )
    | KRuntimeExecute inner ->
        KRuntimeExecute(reintroduceDeepRuntimeHandle inner)
    | KRuntimeLet(bindingName, value, body) ->
        KRuntimeLet(bindingName, reintroduceDeepRuntimeHandle value, reintroduceDeepRuntimeHandle body)
    | KRuntimeDoScope(scopeLabel, body) ->
        KRuntimeDoScope(scopeLabel, reintroduceDeepRuntimeHandle body)
    | KRuntimeScheduleExit(scopeLabel, KRuntimeDeferred deferred, body) ->
        KRuntimeScheduleExit(scopeLabel, KRuntimeDeferred(reintroduceDeepRuntimeHandle deferred), reintroduceDeepRuntimeHandle body)
    | KRuntimeScheduleExit(scopeLabel, KRuntimeRelease(typeText, release, resource), body) ->
        KRuntimeScheduleExit(
            scopeLabel,
            KRuntimeRelease(typeText, reintroduceDeepRuntimeHandle release, reintroduceDeepRuntimeHandle resource),
            reintroduceDeepRuntimeHandle body
        )
    | KRuntimeSequence(first, second) ->
        KRuntimeSequence(reintroduceDeepRuntimeHandle first, reintroduceDeepRuntimeHandle second)
    | KRuntimeWhile(condition, body) ->
        KRuntimeWhile(reintroduceDeepRuntimeHandle condition, reintroduceDeepRuntimeHandle body)
    | KRuntimeApply(callee, arguments) ->
        KRuntimeApply(reintroduceDeepRuntimeHandle callee, arguments |> List.map reintroduceDeepRuntimeHandle)
    | KRuntimeTraitCall(traitName, memberName, dictionary, arguments) ->
        KRuntimeTraitCall(
            traitName,
            memberName,
            reintroduceDeepRuntimeHandle dictionary,
            arguments |> List.map reintroduceDeepRuntimeHandle
        )
    | KRuntimeUnary(operatorName, operand) ->
        KRuntimeUnary(operatorName, reintroduceDeepRuntimeHandle operand)
    | KRuntimeBinary(left, operatorName, right) ->
        KRuntimeBinary(reintroduceDeepRuntimeHandle left, operatorName, reintroduceDeepRuntimeHandle right)
    | KRuntimePrefixedString(prefix, parts) ->
        KRuntimePrefixedString(
            prefix,
            parts
            |> List.map (function
                | KRuntimeStringText text -> KRuntimeStringText text
                | KRuntimeStringInterpolation(inner, format) -> KRuntimeStringInterpolation(reintroduceDeepRuntimeHandle inner, format))
        )
    | KRuntimeEffectOperation(label, operationName) ->
        KRuntimeEffectOperation(reintroduceDeepRuntimeHandle label, operationName)
    | KRuntimeLiteral _
    | KRuntimeName _
    | KRuntimeEffectLabel _
    | KRuntimeDictionaryValue _ ->
        expression

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
    Assert.Contains("KFrontIR.MODAL_SOLVE", checkpoints)
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
    Assert.Contains(("advancePhase", "module", "KFrontIR.BODY_RESOLVE", "KFrontIR.MODAL_SOLVE"), trace)
    Assert.Contains(("advancePhase", "module", "KFrontIR.MODAL_SOLVE", "KFrontIR.CHECKERS"), trace)
    Assert.Contains(("lowerKCore", "module", "KFrontIR.CORE_LOWERING", "KCore"), trace)
    Assert.Contains(("lowerKRuntimeIR", "KCoreUnit", "KCore", "KRuntimeIR"), trace)
    Assert.Contains(("lowerKBackendIR", "module", "KRuntimeIR", "KBackendIR"), trace)

[<Fact>]
let ``analysis session records effective build inputs for query identity`` () =
    let workspace =
        compileInMemoryWorkspaceWithBackend
            "memory-analysis-session-root"
            "dotnet"
            [
                "main.kp",
                [
                    "module main"
                    "let answer = 42"
                ]
                |> String.concat "\n"
            ]

    let session = Compilation.analysisSession workspace

    Assert.Equal(workspace.AnalysisSessionIdentity, session.Identity)
    Assert.Equal(workspace.SourceRoot, session.SourceRoot)
    Assert.Equal(workspace.BuildConfigurationIdentity, session.BuildConfigurationIdentity)
    Assert.Equal("dotnet", session.BackendProfile)
    Assert.Equal("bootstrap-prelude-v2", session.BackendIntrinsicSet)
    Assert.Equal("managed", session.DeploymentMode)
    Assert.Contains("backendProfile=dotnet", session.Identity)
    Assert.Contains("deploymentMode=managed", session.Identity)

[<Fact>]
let ``query plan records stable query kinds checkpoints and dependencies`` () =
    let workspace =
        compileInMemoryWorkspaceWithBackend
            "memory-query-plan-root"
            "zig"
            [
                "main.kp",
                [
                    "module main"
                    "let answer = 42"
                ]
                |> String.concat "\n"
            ]

    let queries = Compilation.queryPlan workspace

    let queryByCheckpoint checkpoint =
        queries
        |> List.find (fun query -> query.OutputCheckpoint = checkpoint)

    let rawQuery = queryByCheckpoint "KFrontIR.RAW"
    Assert.Equal(BuildKFrontIRQuery, rawQuery.QueryKind)
    Assert.Equal(Some RAW, rawQuery.RequiredPhase)
    Assert.Equal(workspace.AnalysisSessionIdentity, rawQuery.AnalysisSessionIdentity)
    Assert.Equal(workspace.BuildConfigurationIdentity, rawQuery.BuildConfigurationIdentity)

    let modalSolveQuery = queryByCheckpoint "KFrontIR.MODAL_SOLVE"
    Assert.Equal(AdvanceKFrontIRPhaseQuery, modalSolveQuery.QueryKind)
    Assert.Equal(Some MODAL_SOLVE, modalSolveQuery.RequiredPhase)
    Assert.Contains((queryByCheckpoint "KFrontIR.BODY_RESOLVE").Id, modalSolveQuery.DependencyIds)

    let coreQuery = queryByCheckpoint "KCore"
    Assert.Equal(LowerKCoreQuery, coreQuery.QueryKind)
    Assert.Equal(Some CORE_LOWERING, coreQuery.RequiredPhase)
    Assert.Contains((queryByCheckpoint "KFrontIR.CORE_LOWERING").Id, coreQuery.DependencyIds)

    let backendQuery = queryByCheckpoint "KBackendIR"
    Assert.Equal(LowerKBackendIRQuery, backendQuery.QueryKind)
    Assert.Contains((queryByCheckpoint "KRuntimeIR").Id, backendQuery.DependencyIds)

    let targetQuery = queryByCheckpoint "zig.c"
    Assert.Equal(LowerTargetQuery, targetQuery.QueryKind)
    Assert.Equal(None, targetQuery.RequiredPhase)
    Assert.Contains(backendQuery.Id, targetQuery.DependencyIds)

    Assert.All(
        queries,
        fun query ->
            Assert.False(String.IsNullOrWhiteSpace(query.Id))
            Assert.False(String.IsNullOrWhiteSpace(query.InputKey))
            Assert.False(String.IsNullOrWhiteSpace(query.OutputCheckpoint))
            Assert.Equal("zig", query.BackendProfile)
            Assert.Equal("bootstrap-prelude-v2", query.BackendIntrinsicSet)
    )

[<Fact>]
let ``compiler fingerprints and incremental units expose dependency inputs`` () =
    let workspace =
        compileInMemoryWorkspaceWithBackend
            "memory-incremental-units-root"
            "zig"
            [
                "main.kp",
                [
                    "module main"
                    "let answer = 42"
                ]
                |> String.concat "\n"
            ]

    let fingerprints = Compilation.compilerFingerprints workspace

    let fingerprintKinds = fingerprints |> List.map (fun fingerprint -> fingerprint.FingerprintKind)
    Assert.Contains(SourceFingerprint, fingerprintKinds)
    Assert.Contains(HeaderFingerprint, fingerprintKinds)
    Assert.Contains(InterfaceFingerprint, fingerprintKinds)
    Assert.Contains(BodyFingerprint, fingerprintKinds)
    Assert.Contains(BackendFingerprint, fingerprintKinds)

    let mainFingerprint kind =
        fingerprints
        |> List.find (fun fingerprint ->
            fingerprint.FingerprintKind = kind
            && fingerprint.InputKey.EndsWith("main.kp", StringComparison.OrdinalIgnoreCase))

    let sourceFingerprint = mainFingerprint SourceFingerprint
    Assert.False(String.IsNullOrWhiteSpace(sourceFingerprint.Identity))
    Assert.Equal(workspace.AnalysisSessionIdentity, sourceFingerprint.AnalysisSessionIdentity)

    let backendFingerprint = mainFingerprint BackendFingerprint
    Assert.Equal("zig", backendFingerprint.BackendProfile)
    Assert.Equal("bootstrap-prelude-v2", backendFingerprint.BackendIntrinsicSet)
    Assert.Contains(workspace.BuildConfigurationIdentity, backendFingerprint.Identity)

    let units = Compilation.incrementalUnits workspace

    let unitKinds = units |> List.map (fun unit -> unit.UnitKind)
    Assert.Contains(SourceFileTextUnit, unitKinds)
    Assert.Contains(ModuleImportSurfaceUnit, unitKinds)
    Assert.Contains(DeclarationHeaderUnit, unitKinds)
    Assert.Contains(DeclarationBodyUnit, unitKinds)
    Assert.Contains(MacroExpansionUnit, unitKinds)
    Assert.Contains(ModuleInterfaceUnit, unitKinds)
    Assert.Contains(KCoreModuleUnit, unitKinds)
    Assert.Contains(KBackendIRModuleUnit, unitKinds)
    Assert.Contains(TargetLoweringUnit, unitKinds)

    let mainUnit kind =
        units
        |> List.find (fun unit ->
            unit.UnitKind = kind
            && unit.InputKey.Contains("main.kp", StringComparison.OrdinalIgnoreCase))

    Assert.NotNull(mainUnit SourceFileTextUnit)
    Assert.NotNull(mainUnit ModuleImportSurfaceUnit)
    Assert.NotNull(mainUnit DeclarationHeaderUnit)
    Assert.NotNull(mainUnit DeclarationBodyUnit)
    Assert.NotNull(mainUnit MacroExpansionUnit)
    Assert.NotNull(mainUnit ModuleInterfaceUnit)
    Assert.NotNull(mainUnit KCoreModuleUnit)
    Assert.NotNull(mainUnit KBackendIRModuleUnit)

    let backendUnit = mainUnit KBackendIRModuleUnit
    Assert.Contains((mainUnit KCoreModuleUnit).Id, backendUnit.DependencyUnitIds)

    let targetUnit =
        units |> List.find (fun unit -> unit.UnitKind = TargetLoweringUnit && unit.OutputCheckpoint = Some "zig.c")

    Assert.Contains(backendUnit.Id, targetUnit.DependencyUnitIds)

    Assert.All(
        units,
        fun unit ->
            Assert.Equal(workspace.AnalysisSessionIdentity, unit.AnalysisSessionIdentity)
            Assert.Equal(workspace.BuildConfigurationIdentity, unit.BuildConfigurationIdentity)
            Assert.Equal("zig", unit.BackendProfile)
            Assert.False(String.IsNullOrWhiteSpace(unit.Id))
    )

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
let ``frontend checkpoints dump phase specific snapshots`` () =
    let workspace =
        compileInMemoryWorkspace
            "memory-frontend-snapshot-root"
            [
                "main.kp",
                [
                    "module main"
                    "let answer = 40 + 2"
                ]
                |> String.concat "\n"
            ]

    let rawJson =
        match Compilation.dumpStage workspace "KFrontIR.RAW" StageDumpFormat.Json with
        | Result.Ok dump -> dump
        | Result.Error message -> failwith message

    use rawDocument = JsonDocument.Parse(rawJson)

    let rawMainDocument =
        rawDocument.RootElement.GetProperty("documents").EnumerateArray()
        |> Seq.find (fun item -> item.GetProperty("moduleIdentity").GetString() = "main")

    let rawResolvedPhases =
        rawMainDocument.GetProperty("resolvedPhases").EnumerateArray()
        |> Seq.map (fun item -> item.GetString())
        |> Seq.filter (isNull >> not)
        |> Seq.toList

    Assert.Equal<string list>([ "RAW" ], rawResolvedPhases)
    Assert.Equal(0, rawMainDocument.GetProperty("imports").GetArrayLength())
    Assert.Equal(JsonValueKind.Null, rawMainDocument.GetProperty("ownership").ValueKind)

    let bodyResolveJson =
        match Compilation.dumpStage workspace "KFrontIR.BODY_RESOLVE" StageDumpFormat.Json with
        | Result.Ok dump -> dump
        | Result.Error message -> failwith message

    use bodyResolveDocument = JsonDocument.Parse(bodyResolveJson)

    let bodyResolveMainDocument =
        bodyResolveDocument.RootElement.GetProperty("documents").EnumerateArray()
        |> Seq.find (fun item -> item.GetProperty("moduleIdentity").GetString() = "main")

    let bodyResolvePhases =
        bodyResolveMainDocument.GetProperty("resolvedPhases").EnumerateArray()
        |> Seq.map (fun item -> item.GetString())
        |> Seq.filter (isNull >> not)
        |> Seq.toList

    Assert.Contains("BODY_RESOLVE", bodyResolvePhases)
    Assert.DoesNotContain("CHECKERS", bodyResolvePhases)
    Assert.Contains(
        bodyResolveMainDocument.GetProperty("imports").EnumerateArray(),
        fun item -> item.GetString() = "std.prelude.*"
    )
    Assert.NotEqual(JsonValueKind.Null, bodyResolveMainDocument.GetProperty("ownership").ValueKind)

[<Fact>]
let ``KBackendIR dumps expose graph ids provenance and dump format metadata`` () =
    let workspace =
        compileInMemoryWorkspace
            "memory-backend-graph-dump-root"
            [
                "main.kp",
                [
                    "module main"
                    "let answer = 20 + 22"
                ]
                |> String.concat "\n"
            ]

    let backendJson =
        match Compilation.dumpStage workspace "KBackendIR" StageDumpFormat.Json with
        | Result.Ok dump -> dump
        | Result.Error message -> failwith message

    use backendDocument = JsonDocument.Parse(backendJson)

    Assert.Equal("json", backendDocument.RootElement.GetProperty("dumpFormat").GetString())

    let backendModule =
        backendDocument.RootElement.GetProperty("modules").EnumerateArray()
        |> Seq.find (fun item -> item.GetProperty("name").GetString() = "main")

    let answerFunction =
        backendModule.GetProperty("functions").EnumerateArray()
        |> Seq.find (fun item -> item.GetProperty("name").GetString() = "answer")

    Assert.False(String.IsNullOrWhiteSpace(answerFunction.GetProperty("id").GetString()))

    let provenance = answerFunction.GetProperty("provenance")
    Assert.Equal("main", provenance.GetProperty("moduleName").GetString())
    Assert.Equal("answer", provenance.GetProperty("declarationName").GetString())

    let bodyGraph = answerFunction.GetProperty("bodyGraph")
    Assert.False(String.IsNullOrWhiteSpace(bodyGraph.GetProperty("rootNodeId").GetString()))
    Assert.True(bodyGraph.GetProperty("nodes").GetArrayLength() >= 3)
    Assert.True(bodyGraph.GetProperty("edges").GetArrayLength() >= 2)

[<Fact>]
let ``BODY_RESOLVE dump exposes M3 ownership facts`` () =
    let workspace =
        compileInMemoryWorkspaceWithUnsafeConsume
            "memory-m3-ownership-dump-root"
            [
                "main.kp",
                [
                    "module main"
                    ""
                    "data File : Type ="
                    "    Handle Int"
                    ""
                    "let consume (1 f : File) = unsafeConsume f"
                    "let openFile name = pure (Handle 1)"
                    "let readData (& file : File) = pure \"chunk\""
                    ""
                    "let main : IO Unit = do"
                    "    let 1 owned = Handle 1"
                    "    consume owned"
                    "    using file <- openFile \"data.txt\""
                    "    let reader = \\ unit -> file"
                    "    readData (reader ())"
                ]
                |> String.concat "\n"
            ]

    let bodyResolveJson =
        match Compilation.dumpStage workspace "KFrontIR.BODY_RESOLVE" StageDumpFormat.Json with
        | Result.Ok dump -> dump
        | Result.Error message -> failwith message

    use document = JsonDocument.Parse(bodyResolveJson)

    let mainDocument =
        document.RootElement.GetProperty("documents").EnumerateArray()
        |> Seq.find (fun item -> item.GetProperty("moduleIdentity").GetString() = "main")

    let ownership = mainDocument.GetProperty("ownership")

    let bindings = ownership.GetProperty("bindings").EnumerateArray()
    let ownedBinding =
        bindings
        |> Seq.find (fun item -> item.GetProperty("name").GetString() = "owned")

    Assert.Equal("1", ownedBinding.GetProperty("declaredQuantity").GetString())
    Assert.Equal("[1,1]", ownedBinding.GetProperty("inferredDemand").GetString())
    Assert.Equal("consumed", ownedBinding.GetProperty("state").GetString())
    Assert.Equal(11, ownedBinding.GetProperty("origin").GetProperty("startLine").GetInt32())

    let uses = ownership.GetProperty("uses").EnumerateArray()
    let ownedConsume =
        uses
        |> Seq.find (fun item ->
            item.GetProperty("useKind").GetString() = "consume"
            && item.GetProperty("targetName").GetString() = "owned")

    Assert.Equal(12, ownedConsume.GetProperty("origin").GetProperty("startLine").GetInt32())

    let borrowRegions = ownership.GetProperty("borrowRegions").EnumerateArray()
    Assert.Contains(
        borrowRegions,
        fun item ->
            item.GetProperty("ownerScope").GetString() = "using"
            && item.GetProperty("explicitName").ValueKind = JsonValueKind.Null
            && item.GetProperty("introductionOrigin").GetProperty("startLine").GetInt32() = 13
    )

    let usingScopes = ownership.GetProperty("usingScopes").EnumerateArray()
    let usingScope =
        usingScopes
        |> Seq.find (fun item -> item.GetProperty("sharedRegionId").GetString().StartsWith("rho", StringComparison.Ordinal))

    Assert.Equal(13, usingScope.GetProperty("surfaceOrigin").GetProperty("startLine").GetInt32())

    let hiddenOwnedBindingName = usingScope.GetProperty("hiddenOwnedBinding").GetString()
    let hiddenReleaseObligation = usingScope.GetProperty("hiddenReleaseObligation").GetString()
    let sharedRegionId = usingScope.GetProperty("sharedRegionId").GetString()

    Assert.StartsWith(usingScope.GetProperty("id").GetString() + ".owned", hiddenOwnedBindingName)
    Assert.StartsWith(usingScope.GetProperty("id").GetString() + ".release", hiddenReleaseObligation)

    let usingRelease =
        uses
        |> Seq.find (fun item ->
            item.GetProperty("useKind").GetString() = "release"
            && item.GetProperty("targetName").GetString() = hiddenOwnedBindingName)

    Assert.Equal(13, usingRelease.GetProperty("origin").GetProperty("startLine").GetInt32())

    let borrowedUsingBinding =
        bindings
        |> Seq.find (fun item ->
            item.GetProperty("name").GetString() = "file"
            && item.GetProperty("kind").GetString() = "pattern"
            && item.GetProperty("borrowRegionId").GetString() = sharedRegionId)

    Assert.Equal(13, borrowedUsingBinding.GetProperty("origin").GetProperty("startLine").GetInt32())

    Assert.Contains(
        bindings,
        fun item ->
            item.GetProperty("name").GetString() = hiddenOwnedBindingName
            && item.GetProperty("kind").GetString() = "using-owned"
            && item.GetProperty("declaredQuantity").GetString() = "1"
            && item.GetProperty("state").GetString() = "consumed"
    )

    let closures = ownership.GetProperty("closures").EnumerateArray()
    Assert.Contains(
        closures,
        fun item ->
            item.GetProperty("name").GetString() = "reader"
            && item.GetProperty("escapeStatus").GetString() = "contained"
    )

[<Fact>]
let ``CHECKERS dump preserves M3 ownership facts`` () =
    let workspace =
        compileInMemoryWorkspaceWithUnsafeConsume
            "memory-m3-checkers-ownership-dump-root"
            [
                "main.kp",
                [
                    "module main"
                    ""
                    "data File : Type ="
                    "    Handle Int"
                    ""
                    "let openFile name = pure (Handle 1)"
                    "let readData (& file : File) = pure \"chunk\""
                    ""
                    "let main : IO Unit = do"
                    "    using file <- openFile \"data.txt\""
                    "    let reader = \\ unit -> file"
                    "    readData (reader ())"
                ]
                |> String.concat "\n"
            ]

    let checkerJson =
        match Compilation.dumpStage workspace "KFrontIR.CHECKERS" StageDumpFormat.Json with
        | Result.Ok dump -> dump
        | Result.Error message -> failwith message

    use document = JsonDocument.Parse(checkerJson)

    let mainDocument =
        document.RootElement.GetProperty("documents").EnumerateArray()
        |> Seq.find (fun item -> item.GetProperty("moduleIdentity").GetString() = "main")

    let ownership = mainDocument.GetProperty("ownership")

    Assert.NotEqual(JsonValueKind.Null, ownership.ValueKind)
    Assert.Equal(JsonValueKind.Array, ownership.GetProperty("bindings").ValueKind)
    Assert.Equal(JsonValueKind.Array, ownership.GetProperty("uses").ValueKind)
    Assert.Equal(JsonValueKind.Array, ownership.GetProperty("borrowRegions").ValueKind)
    Assert.Equal(JsonValueKind.Array, ownership.GetProperty("usingScopes").ValueKind)
    Assert.Equal(JsonValueKind.Array, ownership.GetProperty("closures").ValueKind)
    Assert.Equal(JsonValueKind.Array, ownership.GetProperty("deferred").ValueKind)
    Assert.Equal(JsonValueKind.Array, ownership.GetProperty("diagnostics").ValueKind)

    let usingScope =
        ownership.GetProperty("usingScopes").EnumerateArray()
        |> Seq.find (fun item -> item.GetProperty("sharedRegionId").GetString().StartsWith("rho", StringComparison.Ordinal))

    let sharedRegionId = usingScope.GetProperty("sharedRegionId").GetString()

    Assert.Contains(
        ownership.GetProperty("bindings").EnumerateArray(),
        fun item ->
            item.GetProperty("name").GetString() = "file"
            && item.GetProperty("borrowRegionId").GetString() = sharedRegionId
    )

    Assert.Contains(
        ownership.GetProperty("closures").EnumerateArray(),
        fun item ->
            item.GetProperty("name").GetString() = "reader"
            && item.GetProperty("escapeStatus").GetString() = "contained"
    )

[<Fact>]
let ``BODY_RESOLVE dump records linear move events`` () =
    let workspace =
        compileInMemoryWorkspaceWithUnsafeConsume
            "memory-m3-linear-move-event-root"
            [
                "main.kp",
                [
                    "module main"
                    ""
                    "data File : Type ="
                    "    Handle Int"
                    ""
                    "let consume (1 file : File) = unsafeConsume file"
                    ""
                    "let main : IO Unit = do"
                    "    let 1 file = Handle 1"
                    "    let alias = file"
                    "    consume alias"
                ]
                |> String.concat "\n"
            ]

    let bodyResolveJson =
        match Compilation.dumpStage workspace "KFrontIR.BODY_RESOLVE" StageDumpFormat.Json with
        | Result.Ok dump -> dump
        | Result.Error message -> failwith message

    use document = JsonDocument.Parse(bodyResolveJson)

    let mainDocument =
        document.RootElement.GetProperty("documents").EnumerateArray()
        |> Seq.find (fun item -> item.GetProperty("moduleIdentity").GetString() = "main")

    let uses = mainDocument.GetProperty("ownership").GetProperty("uses").EnumerateArray()

    Assert.Contains(
        uses,
        fun item ->
            item.GetProperty("useKind").GetString() = "move"
            && item.GetProperty("targetName").GetString() = "file"
    )

[<Fact>]
let ``KCore dump preserves M3 ownership facts before erasure`` () =
    let workspace =
        compileInMemoryWorkspace
            "memory-m3-kcore-ownership-dump-root"
            [
                "main.kp",
                [
                    "module main"
                    ""
                    "data File : Type ="
                    "    Handle Int"
                    ""
                    "let readData (& file : File) = pure \"chunk\""
                    ""
                    "let main : IO Unit = do"
                    "    let & file = Handle 1"
                    "    readData file"
                ]
                |> String.concat "\n"
            ]

    let coreJson =
        match Compilation.dumpStage workspace "KCore" StageDumpFormat.Json with
        | Result.Ok dump -> dump
        | Result.Error message -> failwith message

    use document = JsonDocument.Parse(coreJson)

    let mainModule =
        document.RootElement.GetProperty("modules").EnumerateArray()
        |> Seq.find (fun item -> item.GetProperty("name").GetString() = "main")

    let ownership = mainModule.GetProperty("ownership")
    let bindings = ownership.GetProperty("bindings").EnumerateArray()

    let fileBinding =
        bindings
        |> Seq.find (fun item -> item.GetProperty("name").GetString() = "file")

    Assert.Equal("&", fileBinding.GetProperty("declaredQuantity").GetString())
    Assert.Equal("&", fileBinding.GetProperty("inferredDemand").GetString())
    Assert.Equal("borrowed", fileBinding.GetProperty("state").GetString())
    Assert.StartsWith("rho", fileBinding.GetProperty("borrowRegionId").GetString())

    let borrowRegions = ownership.GetProperty("borrowRegions").EnumerateArray()

    Assert.Contains(
        borrowRegions,
        fun item ->
            item.GetProperty("id").GetString() = fileBinding.GetProperty("borrowRegionId").GetString()
            && item.GetProperty("ownerScope").GetString().EndsWith(".let", StringComparison.Ordinal)
    )

[<Fact>]
let ``KCore preserves using as protected release schedule`` () =
    let workspace =
        compileInMemoryWorkspace
            "memory-m3-kcore-using-root"
            [
                "main.kp",
                [
                    "module main"
                    ""
                    "data File : Type ="
                    "    Handle Int"
                    ""
                    "let openFile name = pure (Handle 1)"
                    "let readData (& file : File) = pure \"chunk\""
                    ""
                    "let main : IO Unit = do"
                    "    using file <- openFile \"data.txt\""
                    "    readData file"
                ]
                |> String.concat "\n"
            ]

    let coreModule =
        workspace.KCore
        |> List.find (fun moduleDump -> moduleDump.Name = "main")

    let mainBinding =
        coreModule.Declarations
        |> List.pick (fun declaration ->
            match declaration.Binding with
            | Some binding when binding.Name = Some "main" -> Some binding
            | _ -> None)

    match mainBinding.Body with
    | Some(
        KCoreDoScope(
            scopeLabel,
            KCoreLet(
                hiddenOwnedName,
                KCoreExecute(
                    KCoreAppSpine(
                        KCoreName [ "openFile" ],
                        [ { ArgumentKind = KCoreExplicitArgument
                            Expression = KCoreLiteral(LiteralValue.String "data.txt") } ]
                    )
                ),
                KCoreScheduleExit(
                    scheduledScopeLabel,
                    KCoreRelease(None, KCoreName [ "release" ], KCoreName [ releasedName ]),
                    KCoreLet(
                        "file",
                        KCoreName [ borrowedRoot ],
                        KCoreExecute(
                            KCoreAppSpine(
                                KCoreName [ "readData" ],
                                [ { ArgumentKind = KCoreExplicitArgument
                                    Expression = KCoreName [ "file" ] } ]
                            )
                        )
                    )
                )
            )
        )
      ) ->
        Assert.Equal(scopeLabel, scheduledScopeLabel)
        Assert.StartsWith("__kappa_using_file_", hiddenOwnedName)
        Assert.Equal(hiddenOwnedName, releasedName)
        Assert.Equal(hiddenOwnedName, borrowedRoot)
    | other ->
        failwithf "Expected using to lower through KCore DoScope/ScheduleExit/Release, got %A" other

    let coreSexpr =
        match Compilation.dumpStage workspace "KCore" StageDumpFormat.SExpression with
        | Result.Ok dump -> dump
        | Result.Error message -> failwith message

    Assert.Contains("do-scope", coreSexpr)
    Assert.Contains("schedule-exit", coreSexpr)
    Assert.Contains("release", coreSexpr)
    Assert.Contains("__kappa_using_file_", coreSexpr)

[<Fact>]
let ``KCore using release action selects Releasable evidence`` () =
    let workspace =
        compileInMemoryWorkspace
            "memory-m3-kcore-releasable-root"
            [
                "main.kp",
                [
                    "module main"
                    ""
                    "data File : Type ="
                    "    Handle Int"
                    ""
                    "trait Releasable m a ="
                    "    release : a -> IO Unit"
                    ""
                    "instance Releasable IO File ="
                    "    let release f = printString \"closed\""
                    ""
                    "openFile : String -> IO File"
                    "let openFile name = pure (Handle 1)"
                    "let readData (& file : File) = pure \"chunk\""
                    ""
                    "let main : IO Unit = do"
                    "    using file <- openFile \"data.txt\""
                    "    readData file"
                ]
                |> String.concat "\n"
            ]

    let coreModule =
        workspace.KCore
        |> List.find (fun moduleDump -> moduleDump.Name = "main")

    let mainBinding =
        coreModule.Declarations
        |> List.pick (fun declaration ->
            match declaration.Binding with
            | Some binding when binding.Name = Some "main" -> Some binding
            | _ -> None)

    match mainBinding.Body with
    | Some(
        KCoreDoScope(
            _,
            KCoreLet(
                hiddenOwnedName,
                _,
                KCoreScheduleExit(
                    _,
                    KCoreRelease(
                        Some "File",
                        KCoreTraitCall(
                            "Releasable",
                            "release",
                            KCoreDictionaryValue("main", "Releasable", "IO_File"),
                            []
                        ),
                        KCoreName [ releasedName ]
                    ),
                    _
                )
            )
        )
      ) ->
        Assert.Equal(hiddenOwnedName, releasedName)
    | other ->
        failwithf "Expected using release action to select Releasable IO File evidence, got %A" other

[<Fact>]
let ``KRuntimeIR preserves imported multi parameter trait arity in instance head metadata`` () =
    let workspace =
        compileInMemoryWorkspace
            "memory-traits-imported-arity-root"
            [
                "lib.kp",
                [
                    "module lib"
                    ""
                    "trait Pairing a b ="
                    "    pair : a -> b -> Int"
                ]
                |> String.concat "\n"
                "main.kp",
                [
                    "module main"
                    "import lib.(trait Pairing)"
                    ""
                    "instance Pairing Int Bool ="
                    "    let pair x y = if y then x else 0"
                    ""
                    "result : Int"
                    "let result = 0"
                ]
                |> String.concat "\n"
            ]

    Assert.False(workspace.HasErrors, sprintf "Expected no diagnostics, got %A" workspace.Diagnostics)

    let runtimeModule =
        workspace.KRuntimeIR
        |> List.find (fun moduleDump -> moduleDump.Name = "main")

    let pairingInstance =
        runtimeModule.TraitInstances
        |> List.find (fun instanceInfo -> instanceInfo.TraitName = "Pairing")

    Assert.Equal<string list>([ "Int"; "Bool" ], pairingInstance.HeadTypeTexts)

[<Fact>]
let ``KRuntimeIR preserves using release schedules for backend lowering`` () =
    let workspace =
        compileInMemoryWorkspace
            "memory-m3-kruntime-using-root"
            [
                "main.kp",
                [
                    "module main"
                    ""
                    "data File : Type ="
                    "    Handle Int"
                    ""
                    "trait Releasable m a ="
                    "    release : a -> IO Unit"
                    ""
                    "instance Releasable IO File ="
                    "    let release f = printString \"closed\""
                    ""
                    "openFile : String -> IO File"
                    "let openFile name = pure (Handle 1)"
                    "let readData (& file : File) = pure \"chunk\""
                    ""
                    "let main : IO Unit = do"
                    "    using file <- openFile \"data.txt\""
                    "    readData file"
                ]
                |> String.concat "\n"
            ]

    let runtimeModule =
        workspace.KRuntimeIR
        |> List.find (fun moduleDump -> moduleDump.Name = "main")

    let mainBinding =
        runtimeModule.Bindings
        |> List.find (fun binding -> binding.Name = "main")

    match mainBinding.Body with
    | Some(
        KRuntimeDoScope(
            _,
            KRuntimeLet(
                hiddenOwnedName,
                _,
                KRuntimeScheduleExit(
                    _,
                    KRuntimeRelease(
                        Some "File",
                        KRuntimeTraitCall(
                            "Releasable",
                            "release",
                            KRuntimeDictionaryValue("main", "Releasable", "IO_File"),
                            []
                        ),
                        KRuntimeName [ releasedName ]
                    ),
                    _
                )
            )
        )
      ) ->
        Assert.Equal(hiddenOwnedName, releasedName)
    | other ->
        failwithf "Expected KRuntimeIR to preserve using release schedule, got %A" other

    let runtimeSexpr =
        match Compilation.dumpStage workspace "KRuntimeIR" StageDumpFormat.SExpression with
        | Result.Ok dump -> dump
        | Result.Error message -> failwith message

    Assert.Contains("schedule-exit", runtimeSexpr)
    Assert.Contains("release", runtimeSexpr)

[<Fact>]
let ``M3 overuse diagnostics expose primary and related origins`` () =
    let workspace =
        compileInMemoryWorkspaceWithUnsafeConsume
            "memory-m3-diagnostic-origin-root"
            [
                "main.kp",
                [
                    "module main"
                    ""
                    "data File : Type ="
                    "    Handle Int"
                    ""
                    "let consume (1 f : File) = unsafeConsume f"
                    ""
                    "let main : IO Unit = do"
                    "    let 1 file = Handle 1"
                    "    consume file"
                    "    consume file"
                ]
                |> String.concat "\n"
            ]

    let diagnostic =
        workspace.Diagnostics
        |> List.find (hasDiagnosticCode DiagnosticCode.QttLinearOveruse)

    match diagnostic.Location with
    | Some location ->
        Assert.Equal(11, location.Start.Line)
    | None ->
        failwith "Expected overuse diagnostic to have a primary origin."

    let relatedOriginLines =
        diagnostic.RelatedLocations
        |> List.map (fun related -> related.Location.Start.Line)
        |> List.sort

    Assert.Equal<int list>([ 9; 10 ], relatedOriginLines)

    let bodyResolveJson =
        match Compilation.dumpStage workspace "KFrontIR.BODY_RESOLVE" StageDumpFormat.Json with
        | Result.Ok dump -> dump
        | Result.Error message -> failwith message

    use document = JsonDocument.Parse(bodyResolveJson)

    let expectedOveruseCode =
        DiagnosticCode.toIdentifier DiagnosticCode.QttLinearOveruse

    let dumpedDiagnostic =
        document.RootElement.GetProperty("diagnostics").EnumerateArray()
        |> Seq.find (fun item -> item.GetProperty("code").GetString() = expectedOveruseCode)

    Assert.Equal(11, dumpedDiagnostic.GetProperty("startLine").GetInt32())
    Assert.Equal(2, dumpedDiagnostic.GetProperty("relatedOrigins").GetArrayLength())

[<Fact>]
let ``M3 overuse diagnostic origins ignore names inside string literals`` () =
    let workspace =
        compileInMemoryWorkspaceWithUnsafeConsume
            "memory-m3-diagnostic-origin-string-root"
            [
                "main.kp",
                [
                    "module main"
                    ""
                    "data File : Type ="
                    "    Handle Int"
                    ""
                    "let consume (1 f : File) = unsafeConsume f"
                    ""
                    "let main : IO Unit = do"
                    "    let 1 file = Handle 1"
                    "    printString \"file\""
                    "    consume file"
                    "    consume file"
                ]
                |> String.concat "\n"
            ]

    let diagnostic =
        workspace.Diagnostics
        |> List.find (hasDiagnosticCode DiagnosticCode.QttLinearOveruse)

    match diagnostic.Location with
    | Some location ->
        Assert.Equal(12, location.Start.Line)
    | None ->
        failwith "Expected overuse diagnostic to have a primary origin."

    let relatedOriginLines =
        diagnostic.RelatedLocations
        |> List.map (fun related -> related.Location.Start.Line)
        |> List.sort

    Assert.Equal<int list>([ 9; 11 ], relatedOriginLines)

[<Fact>]
let ``BODY_RESOLVE dump exposes deferred ownership facts for unsupported control flow`` () =
    let workspace =
        compileInMemoryWorkspace
            "memory-m3-deferred-ownership-root"
            [
                "main.kp",
                [
                    "module main"
                    ""
                    "let main : IO Unit = do"
                    "    while False do"
                    "        printInt 0"
                    "    printInt 1"
                ]
                |> String.concat "\n"
            ]

    let bodyResolveJson =
        match Compilation.dumpStage workspace "KFrontIR.BODY_RESOLVE" StageDumpFormat.Json with
        | Result.Ok dump -> dump
        | Result.Error message -> failwith message

    use document = JsonDocument.Parse(bodyResolveJson)

    let mainDocument =
        document.RootElement.GetProperty("documents").EnumerateArray()
        |> Seq.find (fun item -> item.GetProperty("moduleIdentity").GetString() = "main")

    let deferred =
        mainDocument.GetProperty("ownership").GetProperty("deferred").EnumerateArray()
        |> Seq.map (fun item -> item.GetString())
        |> Seq.toList

    Assert.Contains("while-resource-fixed-point", deferred)

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

    Assert.Equal("bootstrap-prelude-v2", supportedWorkspace.BackendIntrinsicIdentity)
    Assert.Contains("backendProfile=interpreter", supportedWorkspace.BuildConfigurationIdentity)
    Assert.Contains("backendIntrinsicSet=bootstrap-prelude-v2", supportedWorkspace.BuildConfigurationIdentity)

    let supportedJson =
        match Compilation.dumpStage supportedWorkspace "KCore" StageDumpFormat.Json with
        | Result.Ok dump -> dump
        | Result.Error message -> failwith message

    Assert.Contains("\"backendProfile\": \"interpreter\"", supportedJson)
    Assert.Contains("\"backendIntrinsicSet\": \"bootstrap-prelude-v2\"", supportedJson)
    Assert.Contains("\"identity\": \"packageMode=true;backendProfile=interpreter;backendIntrinsicSet=bootstrap-prelude-v2", supportedJson)

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
let ``stage dumps expose stable diagnostic codes and frontend phase metadata`` () =
    let workspace =
        compileInMemoryWorkspace
            "memory-diagnostic-metadata-root"
            [
                "main.kp",
                [
                    "module wrong"
                    "let answer = 42"
                ]
                |> String.concat "\n"
            ]

    let checkerJson =
        match Compilation.dumpStage workspace "KFrontIR.CHECKERS" StageDumpFormat.Json with
        | Result.Ok dump -> dump
        | Result.Error message -> failwith message

    use checkerDocument = JsonDocument.Parse(checkerJson)

    let expectedModulePathMismatchCode =
        DiagnosticCode.toIdentifier DiagnosticCode.ModulePathMismatch

    let diagnostic =
        checkerDocument.RootElement.GetProperty("diagnostics").EnumerateArray()
        |> Seq.find (fun item -> item.GetProperty("code").GetString() = expectedModulePathMismatchCode)

    Assert.Equal(expectedModulePathMismatchCode, diagnostic.GetProperty("code").GetString())
    Assert.Equal("KFrontIR", diagnostic.GetProperty("stage").GetString())
    Assert.Equal("CHECKERS", diagnostic.GetProperty("phase").GetString())
    Assert.Equal("error", diagnostic.GetProperty("severity").GetString())

    let checkerSexpr =
        match Compilation.dumpStage workspace "KFrontIR.CHECKERS" StageDumpFormat.SExpression with
        | Result.Ok dump -> dump
        | Result.Error message -> failwith message

    Assert.Contains($"(code \"{DiagnosticCode.toIdentifier DiagnosticCode.ModulePathMismatch}\")", checkerSexpr)
    Assert.Contains("(stage \"KFrontIR\")", checkerSexpr)
    Assert.Contains("(phase \"CHECKERS\")", checkerSexpr)

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
let ``observability metadata is comparable across interpreter zig and dotnet profiles`` () =
    let source =
        [
            "main.kp",
            [
                "module main"
                "let answer = 42"
            ]
            |> String.concat "\n"
        ]

    let profiles =
        [
            "interpreter", "default", None
            "zig", "executable", Some "zig.c"
            "dotnet", "managed", Some "dotnet.clr"
        ]

    for backendProfile, expectedDeploymentMode, expectedTargetCheckpoint in profiles do
        let workspace =
            compileInMemoryWorkspaceWithBackend
                $"memory-observability-profile-{backendProfile}-root"
                backendProfile
                source

        Assert.Equal(backendProfile, workspace.BackendProfile)
        Assert.Equal(expectedDeploymentMode, workspace.DeploymentMode)
        Assert.Contains($"backendProfile={backendProfile}", workspace.BuildConfigurationIdentity)
        Assert.Contains($"deploymentMode={expectedDeploymentMode}", workspace.BuildConfigurationIdentity)

        let checkpoints = Compilation.availableCheckpoints workspace
        Assert.Contains("KBackendIR", checkpoints)

        let targetContracts =
            Compilation.checkpointContracts workspace
            |> List.filter (fun contract -> contract.CheckpointKind = TargetLoweringCheckpoint)

        match expectedTargetCheckpoint with
        | Some targetCheckpoint ->
            Assert.Contains(targetCheckpoint, checkpoints)
            Assert.Single(targetContracts) |> ignore
        | None ->
            Assert.Empty(targetContracts)

        let trace = Compilation.pipelineTrace workspace
        Assert.Contains(trace, fun step -> step.OutputCheckpoint = "KBackendIR")

        for result in Compilation.verifyAllCheckpoints workspace do
            Assert.True(result.Succeeded, $"Checkpoint '{result.Checkpoint}' failed for backend '{backendProfile}'.")

        let dumpCheckpoint =
            expectedTargetCheckpoint |> Option.defaultValue "KBackendIR"

        let dumpJson =
            match Compilation.dumpStage workspace dumpCheckpoint StageDumpFormat.Json with
            | Result.Ok dump -> dump
            | Result.Error message -> failwith message

        use dumpDocument = JsonDocument.Parse(dumpJson)
        let buildConfiguration = dumpDocument.RootElement.GetProperty("buildConfiguration")

        Assert.Equal(backendProfile, buildConfiguration.GetProperty("backendProfile").GetString())
        Assert.Equal(expectedDeploymentMode, buildConfiguration.GetProperty("deploymentMode").GetString())
        Assert.Equal(workspace.BackendIntrinsicIdentity, buildConfiguration.GetProperty("backendIntrinsicSet").GetString())

        let checkpointContract = dumpDocument.RootElement.GetProperty("checkpointContract")
        Assert.Equal(dumpCheckpoint, checkpointContract.GetProperty("name").GetString())

[<Fact>]
let ``portable runtime obligations classify backend neutral backend specific and deferred work`` () =
    let workspace =
        compileInMemoryWorkspaceWithBackend
            "memory-runtime-obligations-root"
            "dotnet"
            [
                "main.kp",
                [
                    "module main"
                    "let answer = 42"
                ]
                |> String.concat "\n"
            ]

    let obligations = Compilation.portableRuntimeObligations workspace

    let obligation name =
        obligations |> List.find (fun obligation -> obligation.Name = name)

    Assert.Equal(KBackendIRGuaranteed, (obligation "tagged-data-layout").Owner)
    Assert.Equal(KBackendIRGuaranteed, (obligation "runtime-calling-convention").Owner)
    Assert.Equal(BackendSpecificRuntime, (obligation "memory-management").Owner)
    Assert.Equal(KBackendIRGuaranteed, (obligation "deterministic-cleanup").Owner)
    Assert.Equal(DeferredRuntimeObligation, (obligation "effect-handlers").Owner)

    Assert.All(
        obligations,
        fun obligation ->
            Assert.False(String.IsNullOrWhiteSpace(obligation.Name))
            Assert.False(String.IsNullOrWhiteSpace(obligation.Description))
    )

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
        hasDiagnosticCode DiagnosticCode.TargetCheckpoint
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
let ``source compilation surfaces missing imported runtime modules as diagnostics`` () =
    let workspace =
        compileInMemoryWorkspaceWithBackend
            "memory-compile-missing-runtime-import-root"
            "dotnet"
            [
                "main.kp",
                [
                    "module main"
                    "import missing.mod"
                    "let answer = 42"
                ]
                |> String.concat "\n"
            ]

    Assert.True(workspace.HasErrors, "Expected missing imported module to become a frontend compile diagnostic.")
    Assert.Contains(workspace.Diagnostics, hasDiagnosticCode DiagnosticCode.ModuleNameUnresolved)
    Assert.Contains("Neither module 'missing.mod' nor item 'mod' from module 'missing' was found", diagnosticsText workspace.Diagnostics)

[<Fact>]
let ``source compilation surfaces backend lowering failures as diagnostics`` () =
    let workspace =
        compileInMemoryWorkspaceWithBackend
            "memory-compile-backend-lowering-failure-root"
            "dotnet"
            [
                "main.kp",
                [
                    "module main"
                    "let I0 = I1"
                ]
                |> String.concat "\n"
            ]

    Assert.True(workspace.HasErrors, "Expected unresolved top-level alias to become a frontend compile diagnostic.")
    Assert.Contains(workspace.Diagnostics, hasDiagnosticCode DiagnosticCode.NameUnresolved)
    Assert.Contains("Name 'I1' is not in scope", diagnosticsText workspace.Diagnostics)

[<Fact>]
let ``source compilation surfaces unresolved applied names before backend lowering`` () =
    let workspace =
        compileInMemoryWorkspaceWithBackend
            "memory-compile-unresolved-applied-name-root"
            "dotnet"
            [
                "main.kp",
                [
                    "module main"
                    "let i0 = I1 2"
                ]
                |> String.concat "\n"
            ]

    Assert.True(workspace.HasErrors, "Expected unresolved applied name to become a frontend compile diagnostic.")
    Assert.Contains(workspace.Diagnostics, hasDiagnosticCode DiagnosticCode.NameUnresolved)
    Assert.Contains("Name 'I1' is not in scope", diagnosticsText workspace.Diagnostics)
    Assert.DoesNotContain("requires a backend module", diagnosticsText workspace.Diagnostics)

[<Fact>]
let ``source compilation surfaces unresolved lowercase applied names before backend lowering`` () =
    let workspace =
        compileInMemoryWorkspaceWithBackend
            "memory-compile-unresolved-lowercase-applied-name-root"
            "dotnet"
            [
                "main.kp",
                [
                    "module main"
                    "let result = i0 2"
                ]
                |> String.concat "\n"
            ]

    Assert.True(workspace.HasErrors, "Expected unresolved lowercase applied name to become a frontend compile diagnostic.")
    Assert.Contains(workspace.Diagnostics, hasDiagnosticCode DiagnosticCode.NameUnresolved)
    Assert.Contains("Name 'i0' is not in scope", diagnosticsText workspace.Diagnostics)
    Assert.DoesNotContain("requires a backend module", diagnosticsText workspace.Diagnostics)

[<Fact>]
let ``source compilation rejects bare module names used as ordinary terms`` () =
    let workspace =
        compileInMemoryWorkspaceWithBackend
            "memory-compile-bare-module-name-term-root"
            "dotnet"
            [
                "main.kp",
                [
                    "module main"
                    "let result = block main"
                ]
                |> String.concat "\n"
            ]

    Assert.True(workspace.HasErrors, "Expected a bare module name in term position to be rejected during frontend validation.")
    Assert.Contains(workspace.Diagnostics, hasDiagnosticCode DiagnosticCode.NameUnresolved)
    Assert.Contains("Name 'main' is not in scope", diagnosticsText workspace.Diagnostics)

[<Fact>]
let ``source compilation rejects dotted value roots that are not valid module qualifiers or projections`` () =
    let workspace =
        compileInMemoryWorkspaceWithBackend
            "memory-compile-dotted-value-root-root"
            "dotnet"
            [
                "main.kp",
                [
                    "module main"
                    "let i0 = i0.right"
                    "let result = i0 String 0"
                ]
                |> String.concat "\n"
            ]

    Assert.True(workspace.HasErrors, "Expected dotted access on an ordinary value root to be rejected before runtime.")
    Assert.Contains(workspace.Diagnostics, hasDiagnosticCode DiagnosticCode.NameUnresolved)
    let diagnostics = diagnosticsText workspace.Diagnostics
    Assert.True(
        diagnostics.Contains("Name 'i0' is not in scope") || diagnostics.Contains("Name 'right' is not in scope"),
        $"Expected an unresolved-name diagnostic for the invalid dotted value root, but saw:{Environment.NewLine}{diagnostics}"
    )

[<Fact>]
let ``source compilation surfaces unresolved dotted receiver names before backend lowering`` () =
    let workspace =
        compileInMemoryWorkspaceWithBackend
            "memory-compile-unresolved-dotted-receiver-root"
            "dotnet"
            [
                "main.kp",
                [
                    "module main"
                    "let result = I0.I0 + 2"
                ]
                |> String.concat "\n"
            ]

    Assert.True(workspace.HasErrors, "Expected unresolved dotted receiver to become a frontend compile diagnostic.")
    Assert.Contains(workspace.Diagnostics, hasDiagnosticCode DiagnosticCode.NameUnresolved)
    Assert.Contains("Module qualifier 'I0' is not in scope", diagnosticsText workspace.Diagnostics)
    Assert.DoesNotContain("requires a backend module", diagnosticsText workspace.Diagnostics)

[<Fact>]
let ``source compilation rejects stray top level forms instead of accepting unknown declarations`` () =
    let typeWorkspace =
        compileInMemoryWorkspace
            "memory-compile-stray-top-level-type-root"
            [
                "main.kp",
                [
                    "module main"
                    "(I0 : Int) -> I0 -> Int"
                ]
                |> String.concat "\n"
            ]

    let caseWorkspace =
        compileInMemoryWorkspace
            "memory-compile-stray-top-level-case-root"
            [
                "main.kp",
                [
                    "module main"
                    "case left () i2 -> pure 42"
                ]
                |> String.concat "\n"
            ]

    Assert.True(typeWorkspace.HasErrors, "Expected stray top-level type text to become a parse diagnostic.")
    Assert.True(caseWorkspace.HasErrors, "Expected stray top-level case text to become a parse diagnostic.")
    Assert.Contains(typeWorkspace.Diagnostics, hasDiagnosticCode DiagnosticCode.ParseError)
    Assert.Contains(caseWorkspace.Diagnostics, hasDiagnosticCode DiagnosticCode.ParseError)

[<Fact>]
let ``source compilation surfaces non callable applications before backend lowering`` () =
    let workspace =
        compileInMemoryWorkspaceWithBackend
            "memory-compile-non-callable-application-root"
            "dotnet"
            [
                "main.kp",
                [
                    "module main"
                    "let result ="
                    "    if True then 42 else 42"
                    ""
                    "    pure 0"
                ]
                |> String.concat "\n"
            ]

    Assert.True(workspace.HasErrors, "Expected non-callable application to become a frontend compile diagnostic.")
    Assert.Contains(workspace.Diagnostics, hasDiagnosticCode DiagnosticCode.ApplicationNonCallable)
    Assert.DoesNotContain("requires a backend module", diagnosticsText workspace.Diagnostics)

[<Fact>]
let ``source compilation surfaces non callable top level value applications before runtime`` () =
    let workspace =
        compileInMemoryWorkspaceWithBackend
            "memory-compile-top-level-non-callable-application-root"
            "dotnet"
            [
                "main.kp",
                [
                    "module main"
                    "let i0 = 42"
                    "let result = i0 ()"
                ]
                |> String.concat "\n"
            ]

    Assert.True(workspace.HasErrors, "Expected applying a top-level Int binding to become a frontend compile diagnostic.")
    Assert.Contains(workspace.Diagnostics, hasDiagnosticCode DiagnosticCode.ApplicationNonCallable)

[<Fact>]
let ``source compilation surfaces non callable literal applications before runtime`` () =
    let workspace =
        compileInMemoryWorkspace
            "memory-compile-literal-non-callable-application-root"
            [
                "main.kp",
                [
                    "module main"
                    "result : Int"
                    "let result = 42()"
                ]
                |> String.concat "\n"
            ]

    Assert.True(workspace.HasErrors, "Expected applying a literal Int value to become a frontend compile diagnostic.")
    Assert.Contains(workspace.Diagnostics, hasDiagnosticCode DiagnosticCode.ApplicationNonCallable)

[<Fact>]
let ``source compilation rejects top level lambda applications with incompatible arguments`` () =
    let workspace =
        compileInMemoryWorkspace
            "memory-compile-top-level-lambda-argument-mismatch-root"
            [
                "main.kp",
                [
                    "module main"
                    "i0 : Unit -> Int"
                    "let i0 = \\() -> 42"
                    "main : IO Int"
                    "let main ="
                    "    pure (i0 (10 :: 12 :: 20 :: Nil))"
                ]
                |> String.concat "\n"
            ]

    Assert.True(workspace.HasErrors, "Expected applying a Unit function to a List argument to be rejected in the frontend.")
    Assert.Contains(workspace.Diagnostics, hasDiagnosticCode DiagnosticCode.TypeEqualityMismatch)

[<Fact>]
let ``source compilation rejects non bindable do bind sources`` () =
    let workspace =
        compileInMemoryWorkspace
            "memory-compile-do-bind-non-bindable-source-root"
            [
                "main.kp",
                [
                    "module main"
                    "let i0 = \\() -> 42"
                    "let main = do"
                    "    i1 <- i0"
                    "    pure (i1 * 10 + i1)"
                ]
                |> String.concat "\n"
            ]

    Assert.True(workspace.HasErrors, "Expected a do-bind source that is not a bindable carrier to be rejected in the frontend.")
    Assert.Contains(workspace.Diagnostics, hasDiagnosticCode DiagnosticCode.TypeEqualityMismatch)

[<Fact>]
let ``source compilation rejects invalid indented do bind continuations before runtime`` () =
    let workspace =
        compileInMemoryWorkspace
            "memory-compile-invalid-indented-do-bind-continuation-root"
            [
                "main.kp",
                [
                    "module main"
                    "let result = do"
                    "    i0 <- pure 40"
                    "      pure 42"
                ]
                |> String.concat "\n"
            ]

    Assert.True(workspace.HasErrors, "Expected an invalid indented continuation after a do-bind source to be rejected in the frontend.")
    Assert.Contains(workspace.Diagnostics, hasDiagnosticCode DiagnosticCode.ParseError)

[<Fact>]
let ``source compilation rejects sibling callable values in arithmetic before runtime`` () =
    let workspace =
        compileInMemoryWorkspace
            "memory-compile-sibling-callable-arithmetic-root"
            [
                "main.kp",
                [
                    "module main"
                    "let i0 = \\() -> 42"
                    "result : Int"
                    "let result = i0 * 2"
                ]
                |> String.concat "\n"
            ]

    Assert.True(workspace.HasErrors, "Expected arithmetic over a sibling callable binding to be rejected in the frontend.")
    Assert.Contains(workspace.Diagnostics, hasDiagnosticCode DiagnosticCode.TypeEqualityMismatch)

[<Fact>]
let ``source compilation rejects recursive function typed values whose bodies are not functions`` () =
    let workspace =
        compileInMemoryWorkspace
            "memory-compile-recursive-function-typed-value-body-root"
            [
                "main.kp",
                [
                    "module main"
                    "i0 : Unit -> Int"
                    "let i0 = i0 ()"
                    "result : Int"
                    "let result = i0 ()"
                ]
                |> String.concat "\n"
            ]

    Assert.True(workspace.HasErrors, "Expected a recursive function-typed value whose body reduces to Int to be rejected in the frontend.")
    Assert.Contains(workspace.Diagnostics, hasDiagnosticCode DiagnosticCode.TypeEqualityMismatch)

[<Fact>]
let ``source compilation rejects unannotated sibling functions called with unit when body requires numeric binder`` () =
    let workspace =
        compileInMemoryWorkspace
            "memory-compile-unannotated-sibling-function-unit-argument-root"
            [
                "main.kp",
                [
                    "module main"
                    "let i0 i0 ="
                    "    i0 + 1"
                    "let result = i0 ()"
                ]
                |> String.concat "\n"
            ]

    Assert.True(workspace.HasErrors, "Expected applying Unit to an unannotated named function whose body requires a numeric binder to be rejected in the frontend.")
    Assert.Contains(workspace.Diagnostics, hasDiagnosticCode DiagnosticCode.TypeEqualityMismatch)

[<Fact>]
let ``source compilation rejects unannotated sibling functions called with non numeric options when body requires numeric binder`` () =
    let workspace =
        compileInMemoryWorkspace
            "memory-compile-unannotated-sibling-function-option-argument-root"
            [
                "main.kp",
                [
                    "module main"
                    "let i0 i0 ="
                    "    i0 + 1"
                    "let result = i0 None"
                ]
                |> String.concat "\n"
            ]

    Assert.True(workspace.HasErrors, "Expected applying Option.None to an unannotated named function whose body requires a numeric binder to be rejected in the frontend.")
    Assert.Contains(workspace.Diagnostics, hasDiagnosticCode DiagnosticCode.TypeEqualityMismatch)

[<Fact>]
let ``source compilation rejects unannotated sibling functions whose sole binder is used numerically before unit application`` () =
    let workspace =
        compileInMemoryWorkspace
            "memory-compile-unannotated-sibling-function-numeric-binder-unit-root"
            [
                "main.kp",
                [
                    "module main"
                    "let i0 i1 = i1 * 2"
                    "let result = i0 ()"
                ]
                |> String.concat "\n"
            ]

    Assert.True(workspace.HasErrors, "Expected applying Unit to an unannotated named function whose binder is consumed by numeric multiplication to be rejected in the frontend.")
    Assert.Contains(workspace.Diagnostics, hasDiagnosticCode DiagnosticCode.TypeEqualityMismatch)

[<Fact>]
let ``source compilation rejects compile time parameters used as runtime numeric operands`` () =
    let workspace =
        compileInMemoryWorkspace
            "memory-compile-time-parameter-runtime-arithmetic-root"
            [
                "main.kp",
                [
                    "module main"
                    "i0 : Type -> Int"
                    "let i0 i0 ="
                    "    i0 + 1"
                    "let result = i0 ()"
                ]
                |> String.concat "\n"
            ]

    Assert.True(workspace.HasErrors, "Expected a compile-time parameter consumed by runtime arithmetic to be rejected in the frontend.")
    Assert.Contains(workspace.Diagnostics, hasDiagnosticCode DiagnosticCode.TypeEqualityMismatch)

[<Fact>]
let ``source compilation rejects unconstrained polymorphic parameters used in arithmetic`` () =
    let workspace =
        compileInMemoryWorkspace
            "memory-compile-polymorphic-parameter-runtime-arithmetic-root"
            [
                "main.kp",
                [
                    "module main"
                    "i0 : a -> Int"
                    "let i0 i0 ="
                    "    i0 + 1"
                    "result : Int"
                    "let result = 0"
                ]
                |> String.concat "\n"
            ]

    Assert.True(workspace.HasErrors, "Expected an unconstrained polymorphic parameter consumed by arithmetic to be rejected in the frontend.")
    Assert.Contains(workspace.Diagnostics, hasDiagnosticCode DiagnosticCode.TypeEqualityMismatch)

[<Fact>]
let ``source compilation rejects malformed instance declarations without crashing`` () =
    let workspace =
        compileInMemoryWorkspace
            "memory-malformed-instance-declaration-root"
            [
                "main.kp",
                [
                    "module main"
                    "instance I0 (I0 Int) -> Int"
                ]
                |> String.concat "\n"
            ]

    Assert.True(workspace.HasErrors, "Expected a malformed instance declaration to fail with frontend diagnostics instead of crashing lowering.")
    Assert.Contains(workspace.Diagnostics, hasDiagnosticCode DiagnosticCode.ParseError)

[<Fact>]
let ``source compilation rejects sibling lambda values in arithmetic inside applications`` () =
    let workspace =
        compileInMemoryWorkspace
            "memory-compile-sibling-lambda-arithmetic-application-root"
            [
                "main.kp",
                [
                    "module main"
                    "let i0 = \\() -> 42"
                    "let result = i0 (i0 + i0)"
                ]
                |> String.concat "\n"
            ]

    Assert.True(workspace.HasErrors, "Expected arithmetic over a sibling lambda-valued binding inside an application to be rejected in the frontend.")
    Assert.Contains(workspace.Diagnostics, hasDiagnosticCode DiagnosticCode.TypeEqualityMismatch)

[<Fact>]
let ``source compilation rejects receiver marked definitions whose bodies disagree with trailing function signatures`` () =
    let workspace =
        compileInMemoryWorkspace
            "memory-compile-receiver-marked-definition-signature-mismatch-root"
            [
                "main.kp",
                [
                    "module main"
                    "i0 : Int -> Int -> Int"
                    "let i0 (this i0 : Int) : Int ="
                    "    i0 + i0"
                    "let result = i0 ()"
                ]
                |> String.concat "\n"
            ]

    Assert.True(workspace.HasErrors, "Expected a receiver-marked definition body to be rejected when the remaining function signature still requires another argument.")
    Assert.Contains(workspace.Diagnostics, hasDiagnosticCode DiagnosticCode.TypeEqualityMismatch)

[<Fact>]
let ``source compilation rejects unused ill typed top level bindings`` () =
    let workspace =
        compileInMemoryWorkspace
            "memory-compile-unused-ill-typed-top-level-binding-root"
            [
                "main.kp",
                [
                    "module main"
                    "i0 : IO Int"
                    "let i0 = \\() -> 42"
                    "result : Int"
                    "let result = 0"
                ]
                |> String.concat "\n"
            ]

    Assert.True(workspace.HasErrors, "Expected an unused top-level binding with a mismatched body to still be rejected.")
    Assert.Contains(workspace.Diagnostics, hasDiagnosticCode DiagnosticCode.TypeEqualityMismatch)

[<Fact>]
let ``source compilation rejects parenthesized record literal bodies under incompatible term signatures`` () =
    let workspace =
        compileInMemoryWorkspace
            "memory-compile-parenthesized-record-literal-body-term-result-mismatch-root"
            [
                "main.kp",
                [
                    "module main"
                    "result : Int"
                    "let result = (i0 = 20)"
                ]
                |> String.concat "\n"
            ]

    Assert.True(workspace.HasErrors, "Expected a parenthesized record-literal body to be rejected when it does not typecheck as the declared Int result.")
    Assert.Contains(workspace.Diagnostics, hasDiagnosticCode DiagnosticCode.TypeEqualityMismatch)

[<Fact>]
let ``source compilation rejects ill typed list cons bodies under explicit result signatures`` () =
    let workspace =
        compileInMemoryWorkspace
            "memory-compile-list-cons-body-result-mismatch-root"
            [
                "main.kp",
                [
                    "module main"
                    "i0 : IO Int -> Int"
                    "let i0 i0 ="
                    "    i0 :: Int"
                    "result : Int"
                    "let result = 0"
                ]
                |> String.concat "\n"
            ]

    Assert.True(workspace.HasErrors, "Expected an ill-typed list cons body to be rejected when the remaining declared result type is Int.")
    Assert.Contains(workspace.Diagnostics, hasDiagnosticCode DiagnosticCode.TypeEqualityMismatch)

[<Fact>]
let ``source compilation rejects do blocks whose inferred result disagrees with the signature`` () =
    let workspace =
        compileInMemoryWorkspaceWithBackend
            "memory-compile-do-tail-unit-signature-mismatch-root"
            "dotnet"
            [
                "main.kp",
                [
                    "module main"
                    "main : IO Int"
                    "let main = do"
                    "    i0 <- pure 42"
                ]
                |> String.concat "\n"
            ]

    Assert.True(workspace.HasErrors, "Expected a do block that ends in Unit to be rejected against IO Int.")
    Assert.Contains(workspace.Diagnostics, hasDiagnosticCode DiagnosticCode.TypeEqualityMismatch)

[<Fact>]
let ``source compilation rejects unresolved dotted member access on literals`` () =
    let workspace =
        compileInMemoryWorkspaceWithBackend
            "memory-compile-literal-dotted-member-root"
            "dotnet"
            [
                "main.kp",
                [
                    "module main"
                    "let result = 21.i0"
                ]
                |> String.concat "\n"
            ]

    Assert.True(workspace.HasErrors, "Expected dotted member access on an Int literal with no matching member to be rejected.")
    Assert.Contains(workspace.Diagnostics, hasDiagnosticCode DiagnosticCode.NameUnresolved)

[<Fact>]
let ``source compilation does not treat ordinary visible function calls as non callable`` () =
    let workspace =
        compileInMemoryWorkspace
            "memory-compile-visible-function-call-root"
            [
                "main.kp",
                [
                    "module main"
                    "let main : IO Unit ="
                    "    printInt 0"
                ]
                |> String.concat "\n"
            ]

    Assert.False(workspace.HasErrors, diagnosticsText workspace.Diagnostics)
    Assert.DoesNotContain(workspace.Diagnostics, hasDiagnosticCode DiagnosticCode.ApplicationNonCallable)

[<Fact>]
let ``source compilation surfaces backend verification call arity failures as diagnostics`` () =
    let workspace =
        compileInMemoryWorkspaceWithBackend
            "memory-compile-backend-arity-failure-root"
            "dotnet"
            [
                "main.kp",
                [
                    "module main"
                    "let I1 = I1 1"
                ]
                |> String.concat "\n"
            ]

    Assert.True(workspace.HasErrors, "Expected unsigned recursion to become a frontend compile diagnostic.")
    Assert.Contains(workspace.Diagnostics, hasDiagnosticCode DiagnosticCode.RecursionRequiresSignature)
    Assert.Contains("recursive but has no preceding signature declaration", diagnosticsText workspace.Diagnostics)

[<Fact>]
let ``source compilation surfaces malformed constructor pattern arity mismatches as diagnostics`` () =
    let workspace =
        compileInMemoryWorkspace
            "memory-compile-constructor-pattern-arity-root"
            [
                "main.kp",
                [
                    "module main"
                    ""
                    "type I0 = (I1 : Int)"
                    "type I1 = (left : Int, right : Int)"
                    "type I2 = (1 i1 : I0, I3 : Int)"
                    "type I2 = (left : Int, right : Int)"
                    "type I1 = (1 I1 : I0, I1 : Int)"
                    "type I2 = (left : Int, right : Int)"
                    "type I2 = (left : Int, right : Int)"
                    "type I2 = (left : Int, right : Int)"
                    "type I2 = (left : Int, right : Int)"
                    "type I2 = (1 I1 : I0, I2 : Int)"
                    "type I3 = (1 I3 : I1, I3 : Int)"
                    "type I3 = (1 I1 : Int, i4 : Int)"
                    "type I3 = (1 I1 : I0, I4 : Int)"
                    "type I3 = (1 I1 : I0)"
                    ""
                    "data I7 : Type ="
                    "    I8 (1 I7 : I1)"
                    ""
                    "data I11 : Type ="
                    "    I13 (1 i2 : I1)"
                    ""
                    "data I11 : Type ="
                    "    I6 (1 I9 : I10)"
                    ""
                    "data I11 : Type ="
                    "    I11 (1 i2 : I0)"
                    ""
                    "data I3 : Type ="
                    "    I13 (1 i2 : I7)"
                    ""
                    "data I11 : Type ="
                    "    I11 (1 i1 : I0)"
                    ""
                    "data I8 : Type ="
                    "    I8 (1 I7 : I0)"
                    ""
                    "data I11 : Type ="
                    "    I13 (1 I0 : I0)"
                    ""
                    "data I11 : Type ="
                    "    I1 this.left"
                    ""
                    "I13 I3 (I3 this : I7) : Int ="
                    "    i1 this.left"
                    ""
                    "I13 i1 (I3 this : I0) : Int ="
                    "    I1 this.right"
                    ""
                    "i1 i1 (I3 this : I3) : Int ="
                    "    i1 this.i0"
                    ""
                    "I3 : (I3 I3 : Int) -> (i4 : I2) -> I4"
                    "let i4 (1 i4 : I1) : Int = i4"
                    ""
                    "i4 : (1 i4 : I0) -> Int"
                    "let i1 i1 ="
                    "    match I3"
                    "        case I1 I3 -> I3 I3"
                ]
                |> String.concat "\n"
            ]

    Assert.True(workspace.HasErrors, "Expected malformed constructor pattern lowering to surface diagnostics instead of crashing.")
    Assert.Contains(workspace.Diagnostics, hasDiagnosticCode DiagnosticCode.DuplicateDeclaration)

[<Fact>]
let ``source compilation allows a single top level signature paired with one let definition`` () =
    let workspace =
        compileInMemoryWorkspace
            "memory-duplicate-terms-valid-signature-let-pair-root"
            [
                "main.kp",
                [
                    "@PrivateByDefault module main"
                    "answer : Int"
                    "let answer = 42"
                ]
                |> String.concat "\n"
            ]

    Assert.False(workspace.HasErrors, sprintf "Expected a single signature paired with one let definition to remain valid, got %A" workspace.Diagnostics)

[<Fact>]
let ``source compilation rejects duplicate ordinary term namespace declarations including projections`` () =
    let workspace =
        compileInMemoryWorkspace
            "memory-duplicate-term-namespace-root"
            [
                "main.kp",
                [
                    "@PrivateByDefault module main"
                    "x : Int"
                    "x : Int"
                    "projection y (place this : Int) : Int = yield this"
                    "let y = 0"
                ]
                |> String.concat "\n"
            ]

    Assert.True(workspace.HasErrors, "Expected duplicate ordinary term-namespace declarations to be rejected.")

    let duplicateMessages =
        workspace.Diagnostics
        |> List.filter (fun diagnostic -> diagnostic.Code = DiagnosticCode.DuplicateDeclaration)
        |> List.map (fun diagnostic -> diagnostic.Message)

    Assert.Contains(duplicateMessages, fun message -> message.Contains("'x'"))
    Assert.Contains(duplicateMessages, fun message -> message.Contains("'y'"))

[<Fact>]
let ``source compilation allows ordinary term declarations to coexist with reified static object facets`` () =
    let workspace =
        compileInMemoryWorkspace
            "memory-term-and-static-facet-coexist-root"
            [
                "main.kp",
                [
                    "@PrivateByDefault module main"
                    "data Box : Type ="
                    "    MkBox"
                    "trait Shape ="
                    "    marker : Unit"
                    "let Box = 0"
                    "let Shape = 1"
                ]
                |> String.concat "\n"
            ]

    Assert.False(
        workspace.Diagnostics |> List.exists (fun diagnostic -> diagnostic.Code = DiagnosticCode.DuplicateDeclaration),
        sprintf "Expected ordinary term declarations to coexist with reified static object facets, got %A" workspace.Diagnostics
    )

[<Fact>]
let ``source compilation rejects duplicate traits and effects across module fragments`` () =
    let workspace =
        compileInMemoryWorkspace
            "memory-duplicate-fragment-declarations-root"
            [
                "main.kp",
                [
                    "@PrivateByDefault module main"
                    "trait Display a ="
                    "    show : a -> String"
                    "effect State ="
                    "    1 get : Unit -> Int"
                ]
                |> String.concat "\n"
                "main.extra.kp",
                [
                    "@PrivateByDefault module main"
                    "trait Display a ="
                    "    show : a -> String"
                    "effect State ="
                    "    1 get : Unit -> Int"
                ]
                |> String.concat "\n"
            ]

    Assert.True(workspace.HasErrors, "Expected duplicate trait and effect declarations across same-module fragments to be rejected.")

    let duplicateMessages =
        workspace.Diagnostics
        |> List.filter (fun diagnostic -> diagnostic.Code = DiagnosticCode.DuplicateDeclaration)
        |> List.map (fun diagnostic -> diagnostic.Message)

    Assert.Contains(duplicateMessages, fun message -> message.Contains("'Display'"))
    Assert.Contains(duplicateMessages, fun message -> message.Contains("'State'"))

[<Fact>]
let ``source compilation rejects duplicate instance heads in the same module`` () =
    let workspace =
        compileInMemoryWorkspace
            "memory-duplicate-instance-head-root"
            [
                "main.kp",
                [
                    "@PrivateByDefault module main"
                    "trait Flag a ="
                    "    flag : a -> Bool"
                    "instance Flag Int ="
                    "    let flag x = True"
                    "instance Flag Int ="
                    "    let flag x = False"
                ]
                |> String.concat "\n"
            ]

    Assert.True(workspace.HasErrors, "Expected duplicate instance heads in the same module to be rejected.")
    Assert.Contains(workspace.Diagnostics, hasDiagnosticCode DiagnosticCode.DuplicateDeclaration)

[<Fact>]
let ``backend verification accepts constructor underapplication as ordinary application`` () =
    let source =
        [
            "main.kp",
            [
                "module main"
                ""
                "data I0 : Type ="
                "    I0 + I0"
                ""
                "result : Int"
                "let result = I0 (I0 4 2)"
            ]
            |> String.concat "\n"
        ]

    let interpreterWorkspace =
        compileInMemoryWorkspaceWithBackend
            "memory-constructor-underapplication-interpreter-root"
            "interpreter"
            source

    match Interpreter.evaluateBinding interpreterWorkspace "main.result" with
    | Result.Ok value ->
        Assert.Equal("<constructor I0/2 [1]>", RuntimeValue.format value)
    | Result.Error issue ->
        failwithf "Expected constructor underapplication to evaluate in the interpreter, got %s" issue.Message

    for backendProfile in [ "dotnet"; "zig" ] do
        let workspace =
            compileInMemoryWorkspaceWithBackend
                $"memory-constructor-underapplication-{backendProfile}-root"
                backendProfile
                source

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

    Assert.Single(diagnostics) |> ignore
    Assert.Contains(diagnostics, hasDiagnosticCode DiagnosticCode.CheckpointVerification)

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

    let malformedPreludeBinding: KRuntimeBinding =
        { Name = "mysteryIntrinsic"
          Parameters = []
          ReturnTypeText = None
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

    Assert.Single(diagnostics) |> ignore
    Assert.Contains(diagnostics, hasDiagnosticCode DiagnosticCode.CheckpointVerification)

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

    Assert.Single(diagnostics) |> ignore
    Assert.Contains(diagnostics, hasDiagnosticCode DiagnosticCode.CheckpointVerification)

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

    Assert.Single(diagnostics) |> ignore
    Assert.Contains(diagnostics, hasDiagnosticCode DiagnosticCode.CheckpointVerification)

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

    Assert.Single(diagnostics) |> ignore
    Assert.Contains(diagnostics, hasDiagnosticCode DiagnosticCode.CheckpointVerification)

[<Fact>]
let ``KRuntimeIR verification rejects direct deep handler nodes after desugaring`` () =
    let workspace =
        compileInMemoryWorkspace
            "memory-verify-deep-handler-runtime-root"
            [
                "main.kp",
                [
                    "@PrivateByDefault module main"
                    "handled : Eff <[ ]> Int"
                    "let handled : Eff <[ ]> Int ="
                    "    block"
                    "        scoped effect Ask ="
                    "            ask : Unit -> Bool"
                    ""
                    "        let comp : Eff <[Ask : Ask]> Int ="
                    "            do"
                    "                let b <- Ask.ask ()"
                    "                if b then 1 else 0"
                    ""
                    "        deep handle Ask comp with"
                    "            case return x -> pure x"
                    "            case ask () k -> k True"
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
                                    if binding.Name = "handled" then
                                        { binding with Body = binding.Body |> Option.map reintroduceDeepRuntimeHandle }
                                    else
                                        binding) }
                    else
                        moduleDump) }

    let diagnostics = Compilation.verifyCheckpoint malformedWorkspace "KRuntimeIR"

    Assert.Single(diagnostics) |> ignore
    Assert.Contains(diagnostics, hasDiagnosticCode DiagnosticCode.CheckpointVerification)
    Assert.Contains("direct deep-handle runtime nodes", diagnosticsText diagnostics)

[<Fact>]
let ``KBackendIR verification surfaces surviving effect runtime constructs before backend lowering`` () =
    let workspace =
        compileInMemoryWorkspace
            "memory-verify-kbackendir-effect-runtime-root"
            [
                "main.kp",
                [
                    "@PrivateByDefault module main"
                    "handled : Eff <[ ]> Int"
                    "let handled : Eff <[ ]> Int ="
                    "    block"
                    "        scoped effect Ask ="
                    "            ask : Unit -> Bool"
                    ""
                    "        let comp : Eff <[Ask : Ask]> Int ="
                    "            do"
                    "                let b <- Ask.ask ()"
                    "                if b then 1 else 0"
                    ""
                    "        deep handle Ask comp with"
                    "            case return x -> pure x"
                    "            case ask () k -> k True"
                ]
                |> String.concat "\n"
            ]

    let diagnostics = Compilation.verifyCheckpoint workspace "KBackendIR"

    Assert.Contains(diagnostics, hasDiagnosticCode DiagnosticCode.CheckpointVerification)
    Assert.Contains("effect runtime constructs", diagnosticsText diagnostics)
    Assert.Contains("main.handled", diagnosticsText diagnostics)

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

    Assert.Single(diagnostics) |> ignore
    Assert.Contains(diagnostics, hasDiagnosticCode DiagnosticCode.CheckpointVerification)

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

    Assert.Single(diagnostics) |> ignore
    Assert.Contains(diagnostics, hasDiagnosticCode DiagnosticCode.CheckpointVerification)

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
                  Guard = None
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

    Assert.Single(diagnostics) |> ignore
    Assert.Contains(diagnostics, hasDiagnosticCode DiagnosticCode.CheckpointVerification)

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

    Assert.Single(diagnostics) |> ignore
    Assert.Contains(diagnostics, hasDiagnosticCode DiagnosticCode.CheckpointVerification)

[<Fact>]
let ``backend verification rejects leaked pre-erasure runtime metadata`` () =
    let workspace =
        compileInMemoryWorkspace
            "memory-verify-runtime-erasure-root"
            [
                "main.kp",
                [
                    "module main"
                    "id : Int -> Int"
                    "let id value = value"
                    "let answer = id 42"
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
                                    if binding.Name = "id" then
                                        { binding with
                                            Parameters =
                                                binding.Parameters
                                                |> List.map (fun parameter ->
                                                    if parameter.Name = "value" then
                                                        { parameter with TypeText = Some "Int captures (s)" }
                                                    else
                                                        parameter)
                                            ReturnTypeText = Some "((Unit -> Int) captures (s))" }
                                    else
                                        binding) }
                    else
                        moduleDump) }

    let diagnostics = Compilation.verifyCheckpoint malformedWorkspace "KBackendIR"

    Assert.NotEmpty(diagnostics)
    Assert.Contains(diagnostics, hasDiagnosticCode DiagnosticCode.CheckpointVerification)

[<Fact>]
let ``backend verification rejects leaked effect row runtime metadata`` () =
    let workspace =
        compileInMemoryWorkspace
            "memory-verify-effect-row-erasure-root"
            [
                "main.kp",
                [
                    "module main"
                    "id : Int -> Int"
                    "let id value = value"
                    "let answer = id 42"
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
                                    if binding.Name = "id" then
                                        { binding with
                                            ReturnTypeText = Some "Eff <[Ask : Ask]> Int" }
                                    else
                                        binding) }
                    else
                        moduleDump) }

    let diagnostics = Compilation.verifyCheckpoint malformedWorkspace "KBackendIR"

    Assert.NotEmpty(diagnostics)
    Assert.Contains(diagnostics, hasDiagnosticCode DiagnosticCode.CheckpointVerification)
    Assert.Contains("pre-erasure runtime metadata", diagnosticsText diagnostics)

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
let ``structural records lower through synthetic constructor layouts in core and runtime`` () =
    let workspace =
        compileInMemoryWorkspace
            "memory-structural-record-lowering-root"
            [
                "main.kp",
                [
                    "module main"
                    "type Point = (x : Int, y : Int)"
                    "build : Int -> Point"
                    "let build n = (y = n, x = n + 1)"
                    "let move (p : Point) : Point = p.{ y = 99 }"
                    "let project (p : Point) = p.x"
                ]
                |> String.concat "\n"
            ]

    Assert.False(workspace.HasErrors, diagnosticsText workspace.Diagnostics)

    let coreModule =
        workspace.KCore
        |> List.find (fun moduleDump -> moduleDump.Name = "main")

    let runtimeModule =
        workspace.KRuntimeIR
        |> List.find (fun moduleDump -> moduleDump.Name = "main")

    Assert.Contains(
        runtimeModule.DataTypes,
        fun dataType -> dataType.Name.StartsWith("__kappa_record_", StringComparison.Ordinal)
    )

    let buildCoreBinding =
        coreModule.Declarations
        |> List.pick (fun declaration ->
            match declaration.Binding with
            | Some binding when binding.Name = Some "build" -> Some binding
            | _ -> None)

    let moveCoreBinding =
        coreModule.Declarations
        |> List.pick (fun declaration ->
            match declaration.Binding with
            | Some binding when binding.Name = Some "move" -> Some binding
            | _ -> None)

    let buildRuntimeBinding =
        runtimeModule.Bindings
        |> List.find (fun binding -> binding.Name = "build")

    let moveRuntimeBinding =
        runtimeModule.Bindings
        |> List.find (fun binding -> binding.Name = "move")

    match buildCoreBinding.Body with
    | Some body ->
        Assert.True(containsCoreSyntheticRecordApply body, $"Expected synthetic record constructor in KCore build body, got %A{body}")
    | None ->
        failwith "Expected KCore build body."

    match moveCoreBinding.Body with
    | Some(KCoreMatch(_, _)) -> ()
    | Some other ->
        failwithf "Expected record update to lower through a KCore match/rebuild, got %A" other
    | None ->
        failwith "Expected KCore move body."

    match buildRuntimeBinding.Body with
    | Some body ->
        Assert.True(
            containsRuntimeSyntheticRecordApply body,
            $"Expected synthetic record constructor in KRuntime build body, got %A{body}"
        )
    | None ->
        failwith "Expected KRuntime build body."

    match moveRuntimeBinding.Body with
    | Some(KRuntimeMatch(_, _)) -> ()
    | Some other ->
        failwithf "Expected record update to lower through a KRuntime match/rebuild, got %A" other
    | None ->
        failwith "Expected KRuntime move body."

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
