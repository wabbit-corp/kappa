namespace Kappa.Compiler

open System

// Provides the public compiler facade for parsing, lowering, tracing, verification, and stage dumps.
module Compilation =
    open CompilationCommon
    open CompilationFrontend

    let tryInferModuleName = CompilationFrontend.tryInferModuleName

    let private defaultDeploymentModeForBackendProfile backendProfile =
        match Stdlib.normalizeBackendProfile backendProfile with
        | "dotnet"
        | "dotnet-il"
        | "hosted-dotnet" -> "managed"
        | "zig" -> "executable"
        | _ -> "default"

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
        =
        let elaborationTerms =
            elaborationAvailableIntrinsicTerms |> String.concat ","

        let packageModeText =
            if packageMode then "true" else "false"

        [
            $"packageMode={packageModeText}"
            $"backendProfile={backendProfile}"
            $"backendIntrinsicSet={backendIntrinsicIdentity}"
            $"deploymentMode={deploymentMode}"
            $"elaborationAvailableIntrinsicTerms=[{elaborationTerms}]"
        ]
        |> String.concat ";"

    let parse (options: CompilationOptions) inputs =
        let normalizedBackendProfile = Stdlib.normalizeBackendProfile options.BackendProfile
        let deploymentMode = normalizeDeploymentMode normalizedBackendProfile options.DeploymentMode
        let backendIntrinsicSet = Stdlib.intrinsicSetForBackendProfile normalizedBackendProfile
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

        let analysisSessionIdentity =
            $"sourceRoot={options.SourceRoot};{buildConfigurationIdentity}"

        let userDocuments =
            collectInputFiles options inputs
            |> List.map (parseFile options)

        let documents =
            if userDocuments |> List.exists (fun document -> document.ModuleName = Some Stdlib.PreludeModuleName) then
                userDocuments
            else
                parseBundledPrelude () :: userDocuments

        let frontendDiagnostics =
            (documents |> List.collect (fun document -> document.Diagnostics))
            @ detectImportCycles documents
            @ validateExpectDeclarations normalizedBackendProfile documents

        let resourceCheckResult: ResourceChecking.CheckResult =
            if frontendDiagnostics |> List.exists (fun diagnostic -> diagnostic.Severity = Error) then
                { Diagnostics = []
                  OwnershipFactsByFile = Map.empty }
            else
                ResourceChecking.checkDocumentsWithFacts documents

        let diagnostics =
            frontendDiagnostics @ resourceCheckResult.Diagnostics

        let frontendSnapshots =
            CompilationSnapshots.buildFrontendSnapshots resourceCheckResult.OwnershipFactsByFile diagnostics documents

        let kFrontIR =
            frontendSnapshots[CORE_LOWERING].Modules

        let kCore =
            SurfaceElaboration.lowerKCoreModules normalizedBackendProfile kFrontIR
            |> List.sortBy (fun moduleDump -> moduleDump.SourceFile)

        let kRuntimeIR =
            kCore
            |> List.map KRuntimeLowering.lowerKRuntimeModule
            |> List.sortBy (fun moduleDump -> moduleDump.SourceFile)

        let kBackendIR =
            KBackendLowering.lowerKBackendModules normalizedBackendProfile kRuntimeIR
            |> List.sortBy (fun moduleDump -> moduleDump.SourceFile)

        let clrAssemblyIR =
            ClrAssemblyLowering.lowerModules kRuntimeIR kBackendIR
            |> List.sortBy (fun moduleDump -> moduleDump.SourceFile)

        let workspaceWithoutTrace =
            { SourceRoot = options.SourceRoot
              PackageMode = options.PackageMode
              BackendProfile = normalizedBackendProfile
              DeploymentMode = deploymentMode
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
              Diagnostics = diagnostics
              PipelineTrace = [] }

        let checkerSnapshot = frontendSnapshots[CHECKERS]

        let frontendVerificationWorkspace =
            { workspaceWithoutTrace with
                KFrontIR = checkerSnapshot.Modules
                Diagnostics = checkerSnapshot.Diagnostics }

        let verification: CompilationTrace.VerificationSummary =
            { Frontend =
                CheckpointVerification.verifyCheckpoint frontendVerificationWorkspace (KFrontIRPhase.checkpointName CHECKERS)
                |> List.isEmpty
              KCore = CheckpointVerification.verifyCheckpoint workspaceWithoutTrace "KCore" |> List.isEmpty
              KRuntimeIR = CheckpointVerification.verifyCheckpoint workspaceWithoutTrace "KRuntimeIR" |> List.isEmpty
              KBackendIR = CheckpointVerification.verifyCheckpoint workspaceWithoutTrace "KBackendIR" |> List.isEmpty
              Targets =
                CompilationCheckpoints.targetCheckpointNames workspaceWithoutTrace
                |> List.map (fun checkpoint ->
                    checkpoint, (CompilationCheckpoints.verifyTargetCheckpoint workspaceWithoutTrace checkpoint |> List.isEmpty))
                |> Map.ofList }

        { workspaceWithoutTrace with
            PipelineTrace =
                CompilationTrace.buildPipelineTrace
                    kFrontIR
                    (CompilationCheckpoints.targetCheckpointNames workspaceWithoutTrace)
                    verification }

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

    let queryPlan = CompilationMetadata.queryPlan

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
