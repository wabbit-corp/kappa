namespace Kappa.Compiler

open System

// Provides the public compiler facade for parsing, lowering, tracing, verification, and stage dumps.
module Compilation =
    open CompilationCommon
    open CompilationFrontend

    let tryInferModuleName = CompilationFrontend.tryInferModuleName

    type private EffectRuntimeUse =
        | EffectLabelUse of string
        | EffectOperationUse of string
        | EffectHandlerUse of isDeep: bool

    let private tokenName (token: Token) =
        match token.Kind with
        | Identifier
        | Keyword _ ->
            Some(SyntaxFacts.trimIdentifierQuotes token.Text)
        | Operator ->
            Some token.Text
        | _ ->
            None

    let private provenanceLocation (documents: ParsedDocument list) (origin: KCoreOrigin) =
        documents
        |> List.tryFind (fun document -> String.Equals(document.Source.FilePath, origin.FilePath, StringComparison.Ordinal))
        |> Option.map (fun document ->
            let defaultLocation = document.Source.GetLocation(TextSpan.FromBounds(0, 0))

            origin.DeclarationName
            |> Option.bind (fun declarationName ->
                document.Syntax.Tokens
                |> List.tryPick (fun token ->
                    tokenName token
                    |> Option.filter (fun tokenText -> String.Equals(tokenText, declarationName, StringComparison.Ordinal))
                    |> Option.map (fun _ -> document.Source.GetLocation(token.Span))))
            |> Option.defaultValue defaultLocation)

    let private describeEffectLabelExpression expression =
        match expression with
        | KRuntimeEffectLabel(labelName, _, _, _) -> labelName
        | KRuntimeName segments -> String.concat "." segments
        | _ -> "<effect>"

    let rec private tryFindEffectRuntimeUse expression =
        match expression with
        | KRuntimeEffectLabel(labelName, _, _, _) ->
            Some(EffectLabelUse labelName)
        | KRuntimeEffectOperation(label, _, operationName) ->
            Some(EffectOperationUse $"{describeEffectLabelExpression label}.{operationName}")
        | KRuntimeHandle(isDeep, _, _, _, _) ->
            Some(EffectHandlerUse isDeep)
        | KRuntimeClosure(_, body)
        | KRuntimeExecute body
        | KRuntimeDoScope(_, body)
        | KRuntimeUnary(_, body) ->
            tryFindEffectRuntimeUse body
        | KRuntimeIfThenElse(condition, whenTrue, whenFalse) ->
            [ condition; whenTrue; whenFalse ] |> List.tryPick tryFindEffectRuntimeUse
        | KRuntimeLet(_, value, body)
        | KRuntimeSequence(value, body) ->
            [ value; body ] |> List.tryPick tryFindEffectRuntimeUse
        | KRuntimeScheduleExit(_, KRuntimeDeferred deferred, body) ->
            [ deferred; body ] |> List.tryPick tryFindEffectRuntimeUse
        | KRuntimeScheduleExit(_, KRuntimeRelease(_, release, resource), body) ->
            [ release; resource; body ] |> List.tryPick tryFindEffectRuntimeUse
        | KRuntimeWhile(condition, body)
        | KRuntimeBinary(condition, _, body) ->
            [ condition; body ] |> List.tryPick tryFindEffectRuntimeUse
        | KRuntimeApply(callee, arguments) ->
            callee :: arguments |> List.tryPick tryFindEffectRuntimeUse
        | KRuntimeTraitCall(_, _, dictionary, arguments) ->
            dictionary :: arguments |> List.tryPick tryFindEffectRuntimeUse
        | KRuntimeMatch(scrutinee, cases) ->
            tryFindEffectRuntimeUse scrutinee
            |> Option.orElseWith (fun () ->
                cases
                |> List.tryPick (fun caseClause ->
                    caseClause.Guard
                    |> Option.bind tryFindEffectRuntimeUse
                    |> Option.orElseWith (fun () -> tryFindEffectRuntimeUse caseClause.Body)))
        | KRuntimePrefixedString(_, parts) ->
            parts
            |> List.tryPick (function
                | KRuntimeStringText _ -> None
                | KRuntimeStringInterpolation(inner, _) -> tryFindEffectRuntimeUse inner)
        | KRuntimeLiteral _
        | KRuntimeName _
        | KRuntimeDictionaryValue _ ->
            None

    let private validateBackendRuntimeSupport backendProfile (documents: ParsedDocument list) (kRuntimeIR: KRuntimeModule list) =
        let normalizedBackendProfile = BackendProfile.toPortableName backendProfile

        if
            match backendProfile with
            | BackendProfile.Interpreter
            | BackendProfile.DotNet
            | BackendProfile.Zig -> true
            | BackendProfile.Unknown _ -> false
        then
            []
        else
            let describeDeclaration (binding: KRuntimeBinding) =
                match binding.Provenance.DeclarationName with
                | Some declarationName -> $"{binding.Provenance.ModuleName}.{declarationName}"
                | None -> binding.Provenance.ModuleName

            let describeUse = function
                | EffectLabelUse labelName -> $"effect label '{labelName}'"
                | EffectOperationUse operationName -> $"effect operation '{operationName}'"
                | EffectHandlerUse true -> "deep effect handler"
                | EffectHandlerUse false -> "effect handler"

            kRuntimeIR
            |> List.collect (fun runtimeModule ->
                runtimeModule.Bindings
                |> List.choose (fun binding ->
                    binding.Body
                    |> Option.bind tryFindEffectRuntimeUse
                    |> Option.map (fun effectUse ->
                        { Severity = DiagnosticSeverity.Error
                          Code = DiagnosticCode.EffectRuntimeUnsupportedBackend
                          Stage = Some "KRuntimeIR"
                          Phase = None
                          Message =
                            $"Backend profile '{normalizedBackendProfile}' does not implement {describeUse effectUse} in declaration '{describeDeclaration binding}'. This backend must reject unsupported effect runtime constructs before target lowering."
                          Location = provenanceLocation documents binding.Provenance
                          RelatedLocations = [] })))

    let private defaultDeploymentModeForBackendProfile backendProfile =
        match backendProfile with
        | BackendProfile.DotNet -> "managed"
        | BackendProfile.Zig -> "executable"
        | BackendProfile.Interpreter
        | BackendProfile.Unknown _ -> "default"

    let private normalizeDeploymentMode backendProfile deploymentMode =
        if
            String.IsNullOrWhiteSpace(deploymentMode)
            || String.Equals(deploymentMode.Trim(), "default", StringComparison.OrdinalIgnoreCase)
        then
            defaultDeploymentModeForBackendProfile backendProfile
        else
            deploymentMode.Trim().ToLowerInvariant()

    let private makeBuildConfigurationIdentity
        packageMode
        backendProfile
        deploymentMode
        backendIntrinsicIdentity
        elaborationAvailableIntrinsicTerms
        allowUnsafeConsume
        =
        let elaborationTerms =
            elaborationAvailableIntrinsicTerms |> String.concat ","

        let packageModeText =
            if packageMode then "true" else "false"

        let allowUnsafeConsumeText =
            if allowUnsafeConsume then "true" else "false"

        [
            $"packageMode={packageModeText}"
            $"backendProfile={backendProfile}"
            $"backendIntrinsicSet={backendIntrinsicIdentity}"
            $"deploymentMode={deploymentMode}"
            $"allowUnsafeConsume={allowUnsafeConsumeText}"
            $"elaborationAvailableIntrinsicTerms=[{elaborationTerms}]"
        ]
        |> String.concat ";"

    let parse (options: CompilationOptions) inputs =
        let backendProfile = BackendProfile.normalizeConfigured options.BackendProfile
        let normalizedBackendProfile = BackendProfile.toPortableName backendProfile
        let deploymentMode = normalizeDeploymentMode backendProfile options.DeploymentMode
        let backendIntrinsicSet =
            Stdlib.intrinsicSetForCompilationProfile backendProfile options.AllowUnsafeConsume
        let backendIntrinsicIdentity = backendIntrinsicSet.Identity

        let elaborationAvailableIntrinsicTerms =
            backendIntrinsicSet.ElaborationAvailableTermNames
            |> Set.toList
            |> List.sort

        let buildConfigurationIdentity =
            makeBuildConfigurationIdentity
                options.PackageMode
                normalizedBackendProfile
                deploymentMode
                backendIntrinsicIdentity
                elaborationAvailableIntrinsicTerms
                options.AllowUnsafeConsume

        let analysisSessionIdentity =
            $"sourceRoot={options.SourceRoot};{buildConfigurationIdentity}"

        let traceRecorder = CompilationTrace.Recorder()

        let parsedUserDocuments =
            collectInputFiles options inputs
            |> List.map (fun filePath ->
                let document = parseFile options filePath
                traceRecorder.RecordParse document.Source.FilePath
                document)

        let initialDocuments =
            if parsedUserDocuments |> List.exists (fun document -> document.ModuleName = Some Stdlib.PreludeModuleName) then
                parsedUserDocuments
            else
                let preludeDocument = parseBundledPrelude options.AllowUnsafeConsume
                traceRecorder.RecordParse preludeDocument.Source.FilePath
                preludeDocument :: parsedUserDocuments

        let withBundledStdlibDocuments =
            let bundledByModuleName =
                BundledStandardModules.all
                |> List.map (fun moduleInfo -> moduleInfo.ModuleName, moduleInfo)
                |> Map.ofList

            let rec loadRequiredBundledModules documents loadedModules =
                let newlyReferencedBundledModules =
                    documents
                    |> List.collect collectImportSpecs
                    |> List.choose (fun spec ->
                        match spec.Source with
                        | Dotted moduleName -> Some moduleName
                        | Url _ -> None)
                    |> List.distinct
                    |> List.filter (fun moduleName -> not (Set.contains moduleName loadedModules))
                    |> List.choose (fun moduleName ->
                        bundledByModuleName |> Map.tryFind moduleName)

                match newlyReferencedBundledModules with
                | [] ->
                    documents
                | modulesToLoad ->
                    let parsedBundledDocuments =
                        modulesToLoad
                        |> List.map (fun moduleInfo ->
                            let document = parseBundledStandardModule moduleInfo
                            traceRecorder.RecordParse document.Source.FilePath
                            document)

                    let nextLoadedModules =
                        modulesToLoad
                        |> List.fold (fun state moduleInfo -> Set.add moduleInfo.ModuleName state) loadedModules

                    loadRequiredBundledModules (documents @ parsedBundledDocuments) nextLoadedModules

            let existingModules =
                initialDocuments
                |> List.choose (fun document -> document.ModuleName)
                |> Set.ofList

            loadRequiredBundledModules initialDocuments existingModules

        let documents =
            withBundledStdlibDocuments
            |> reparseDocumentsWithImportedFixities options
            |> ElaborationEvaluation.expandTopLevelSplices
            |> resolveImportExportSemantics backendProfile
            |> List.map EffectSemantics.assignDocumentEffectIdentities

        let hostBindingModules =
            HostBindings.collectImportedModules backendProfile documents

        let frontendModulesForValidation =
            documents
            |> List.map (fun document ->
                let frontendModule = buildKFrontIRModule Map.empty document
                traceRecorder.RecordBuildKFrontIR document.Source.FilePath
                frontendModule)

        let frontendDiagnostics =
            (documents |> List.collect (fun document -> document.Diagnostics))
            @ validateModuleCaseFoldCollisions options.SourceRoot documents
            @ detectImportCycles documents
            @ validateImportSelections options.PackageMode backendProfile documents
            @ CompilationFrontend.validateReflEqualityDeclarations documents
            @ CompilationFrontend.validateTopLevelSignatureDeclarations documents
            @ SurfaceElaboration.validateSurfaceModules backendProfile frontendModulesForValidation hostBindingModules
            @ validateExpectDeclarations backendProfile options.AllowUnsafeConsume documents

        let resourceCheckResult: ResourceChecking.CheckResult =
            if frontendDiagnostics |> List.exists (fun diagnostic -> diagnostic.Severity = Error) then
                { Diagnostics = []
                  OwnershipFactsByFile = Map.empty }
            else
                ResourceChecking.checkDocumentsWithFactsForBackend backendProfile documents

        let sourceDiagnostics =
            frontendDiagnostics @ resourceCheckResult.Diagnostics

        let frontendSnapshots =
            CompilationSnapshots.buildFrontendSnapshots
                resourceCheckResult.OwnershipFactsByFile
                sourceDiagnostics
                documents
                (fun phase snapshot ->
                    if phase <> RAW then
                        let fromPhase = KFrontIRPhase.all |> List.item (KFrontIRPhase.ordinal phase - 1)
                        traceRecorder.RecordAdvancePhase(fromPhase, phase, snapshot.ModuleIdentity, snapshot.FilePath))

        let kFrontIR =
            frontendSnapshots[CORE_LOWERING].Modules

        let loweredKCore =
            SurfaceElaboration.lowerKCoreModules backendProfile options.AllowUnsafeConsume kFrontIR hostBindingModules

        do
            loweredKCore
            |> List.iter (fun moduleDump -> traceRecorder.RecordLowerKCore(Some [ moduleDump.Name ], moduleDump.SourceFile))

        let kCore =
            loweredKCore |> List.sortBy (fun moduleDump -> moduleDump.SourceFile)

        let loweredKRuntimeIR =
            kCore
            |> List.map (fun coreModule ->
                let runtimeModule = KRuntimeLowering.lowerKRuntimeModule coreModule
                traceRecorder.RecordLowerKRuntimeIR runtimeModule.Name
                runtimeModule)

        let kRuntimeIR =
            loweredKRuntimeIR
            |> fun loweredModules ->
                loweredModules
                @ (hostBindingModules |> Map.values |> Seq.map HostBindings.toRuntimeModule |> Seq.toList)
                @ (StandardModules.all |> List.map StandardModules.toRuntimeModule)
            |> List.sortBy (fun moduleDump -> moduleDump.SourceFile)

        let runtimeCapabilityDiagnostics =
            validateBackendRuntimeSupport backendProfile documents kRuntimeIR

        let loweredKBackendIR, backendLoweringDiagnostics =
            KBackendLowering.lowerKBackendModules backendProfile options.AllowUnsafeConsume kRuntimeIR

        do
            loweredKBackendIR
            |> List.iter (fun moduleDump -> traceRecorder.RecordLowerKBackendIR moduleDump.Name)

        let kBackendIR =
            loweredKBackendIR |> List.sortBy (fun moduleDump -> moduleDump.SourceFile)

        let clrAssemblyIR =
            ClrAssemblyLowering.lowerModules kRuntimeIR kBackendIR
            |> List.sortBy (fun moduleDump -> moduleDump.SourceFile)

        let requiresBackendImplementation =
            Stdlib.targetCheckpointNamesForBackend backendProfile
            |> List.isEmpty
            |> not

        let backendDiagnostics =
            if
                requiresBackendImplementation
                && not ((sourceDiagnostics @ runtimeCapabilityDiagnostics) |> List.exists (fun diagnostic -> diagnostic.Severity = Error))
            then
                backendLoweringDiagnostics
            else
                []

        let provisionalDiagnostics =
            sourceDiagnostics @ runtimeCapabilityDiagnostics @ backendDiagnostics

        let workspaceWithoutTrace =
            { SourceRoot = options.SourceRoot
              PackageMode = options.PackageMode
              Backend = backendProfile
              BackendProfile = normalizedBackendProfile
              DeploymentMode = deploymentMode
              AllowUnsafeConsume = options.AllowUnsafeConsume
              BackendIntrinsicIdentity = backendIntrinsicIdentity
              BuildConfigurationIdentity = buildConfigurationIdentity
              AnalysisSessionIdentity = analysisSessionIdentity
              ElaborationAvailableIntrinsicTerms = elaborationAvailableIntrinsicTerms
              Documents = documents
              FrontendSnapshots = frontendSnapshots
              KFrontIR = kFrontIR
              KCore = kCore
              KRuntimeIR = kRuntimeIR
              KBackendIR = kBackendIR
              ClrAssemblyIR = clrAssemblyIR
              Diagnostics = provisionalDiagnostics
              PipelineTrace = [] }

        let implementationVerificationDiagnostics =
            if
                not requiresBackendImplementation
                || workspaceWithoutTrace.Diagnostics |> List.exists (fun diagnostic -> diagnostic.Severity = Error)
            then
                None
            else
                Some(CheckpointVerification.verifyCheckpoint workspaceWithoutTrace "KBackendIR")

        let implementationDiagnostics =
            implementationVerificationDiagnostics |> Option.defaultValue []

        let workspaceWithoutTrace =
            { workspaceWithoutTrace with
                Diagnostics = workspaceWithoutTrace.Diagnostics @ implementationDiagnostics }

        let checkerSnapshot = frontendSnapshots[CHECKERS]

        let frontendVerificationWorkspace =
            { workspaceWithoutTrace with
                KFrontIR = checkerSnapshot.Modules
                Diagnostics = checkerSnapshot.Diagnostics }

        let frontendVerificationDiagnostics =
            CheckpointVerification.verifyCheckpoint frontendVerificationWorkspace (KFrontIRPhase.checkpointName CHECKERS)

        traceRecorder.RecordVerification(
            PipelineTraceSubject.Module,
            $"verify at {KFrontIRPhase.checkpointName CHECKERS}",
            KFrontIRPhase.checkpointName CHECKERS,
            List.isEmpty frontendVerificationDiagnostics
        )

        let kCoreVerificationDiagnostics =
            CheckpointVerification.verifyCheckpoint workspaceWithoutTrace "KCore"

        traceRecorder.RecordVerification(
            PipelineTraceSubject.KCoreUnit,
            "verify at KCore",
            "KCore",
            List.isEmpty kCoreVerificationDiagnostics
        )

        let kRuntimeIRVerificationDiagnostics =
            CheckpointVerification.verifyCheckpoint workspaceWithoutTrace "KRuntimeIR"

        traceRecorder.RecordVerification(
            PipelineTraceSubject.KRuntimeIRUnit,
            "verify at KRuntimeIR",
            "KRuntimeIR",
            List.isEmpty kRuntimeIRVerificationDiagnostics
        )

        let kBackendIRVerificationDiagnostics =
            implementationVerificationDiagnostics
            |> Option.defaultValue (CheckpointVerification.verifyCheckpoint workspaceWithoutTrace "KBackendIR")

        traceRecorder.RecordVerification(
            PipelineTraceSubject.KBackendIRUnit,
            "verify at KBackendIR",
            "KBackendIR",
            List.isEmpty kBackendIRVerificationDiagnostics
        )

        do
            CompilationCheckpoints.targetCheckpointNames workspaceWithoutTrace
            |> List.iter (fun checkpoint ->
                let targetOutcome =
                    CompilationCheckpoints.verifyTargetCheckpointDetailed workspaceWithoutTrace checkpoint

                if targetOutcome.LoweringAttempted then
                    traceRecorder.RecordLowerTarget(checkpoint, CompilationCheckpoints.targetInputCheckpoint workspaceWithoutTrace checkpoint)

                traceRecorder.RecordVerification(
                    PipelineTraceSubject.TargetUnit,
                    $"verify target at {checkpoint}",
                    checkpoint,
                    List.isEmpty targetOutcome.Diagnostics
                ))

        { workspaceWithoutTrace with
            PipelineTrace = traceRecorder.Finish() }

    let checkpointContracts (workspace: WorkspaceCompilation) =
        CompilationCheckpoints.contractsForWorkspace workspace

    let availableCheckpoints (workspace: WorkspaceCompilation) =
        checkpointContracts workspace
        |> List.map (fun contract -> contract.Name)

    let verifyCheckpoint (workspace: WorkspaceCompilation) checkpoint =
        if CompilationCheckpoints.targetCheckpointNames workspace |> List.contains checkpoint then
            CompilationCheckpoints.verifyTargetCheckpoint workspace checkpoint
        else
            CheckpointVerification.verifyCheckpoint workspace checkpoint

    let verifyAllCheckpoints (workspace: WorkspaceCompilation) =
        checkpointContracts workspace
        |> List.map (fun contract ->
            let diagnostics = verifyCheckpoint workspace contract.Name

            { Checkpoint = contract.Name
              Succeeded = List.isEmpty diagnostics
              Diagnostics = diagnostics })

    let portableRuntimeObligations = CompilationMetadata.portableRuntimeObligations

    let analysisSession = CompilationMetadata.analysisSession

    let querySketch = CompilationMetadata.querySketch

    let compilerFingerprints = CompilationMetadata.compilerFingerprints

    let incrementalUnits = CompilationMetadata.incrementalUnits

    let pipelineTrace (workspace: WorkspaceCompilation) =
        workspace.PipelineTrace

    let dumpStage (workspace: WorkspaceCompilation) checkpoint format =
        let available = availableCheckpoints workspace

        if not (available |> List.contains checkpoint) then
            let availableText = String.concat ", " available
            Result.Error $"Unknown checkpoint '{checkpoint}'. Available checkpoints: {availableText}."
        elif CompilationCheckpoints.targetCheckpointNames workspace |> List.contains checkpoint then
            CompilationCheckpoints.tryEmitTargetTranslationUnit workspace checkpoint
            |> Result.map (fun translationUnit ->
                match format with
                | StageDumpFormat.Json ->
                    CompilationDump.renderTargetCheckpointJson workspace checkpoint translationUnit
                | StageDumpFormat.SExpression ->
                    CompilationDump.renderTargetCheckpointSexpr workspace checkpoint translationUnit)
        else
            match format with
            | StageDumpFormat.Json ->
                Result.Ok(CompilationDump.dumpStageJson workspace checkpoint)
            | StageDumpFormat.SExpression ->
                Result.Ok(CompilationDump.dumpStageSexpr workspace checkpoint)
