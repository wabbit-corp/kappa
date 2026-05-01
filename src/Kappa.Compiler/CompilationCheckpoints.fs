namespace Kappa.Compiler

open System

// Describes published checkpoint contracts and target checkpoint manifests.
module internal CompilationCheckpoints =
    let targetCheckpointNames (workspace: WorkspaceCompilation) =
        Stdlib.targetCheckpointNamesForBackend workspace.Backend

    let targetInputCheckpoint (_workspace: WorkspaceCompilation) _checkpoint = "KBackendIR"

    let private checkpointContract name kind inputCheckpoint requiredBySpec profileSpecific =
        { Name = name
          CheckpointKind = kind
          InputCheckpoint = inputCheckpoint
          RequiredBySpec = requiredBySpec
          ProfileSpecific = profileSpecific }

    let private frontendCheckpointContracts =
        let rec loop inputCheckpoint phases =
            match phases with
            | [] -> []
            | phase :: remainingPhases ->
                let checkpoint = KFrontIRPhase.checkpointName phase

                checkpointContract checkpoint KFrontIRCheckpoint (Some inputCheckpoint) true false
                :: loop checkpoint remainingPhases

        loop "surface-source" KFrontIRPhase.all

    let private baseCheckpointContracts =
        [
            checkpointContract "surface-source" SurfaceSourceCheckpoint None true false
            yield! frontendCheckpointContracts
            checkpointContract "KCore" KCoreCheckpoint (Some(KFrontIRPhase.checkpointName CORE_LOWERING)) true false
            checkpointContract "KRuntimeIR" ImplementationDefinedCheckpoint (Some "KCore") false true
            checkpointContract "KBackendIR" KBackendIRCheckpoint (Some "KRuntimeIR") true false
        ]

    let private targetCheckpointContracts (workspace: WorkspaceCompilation) =
        targetCheckpointNames workspace
        |> List.map (fun checkpoint ->
            checkpointContract checkpoint TargetLoweringCheckpoint (Some(targetInputCheckpoint workspace checkpoint)) true true)

    let contractsForWorkspace (workspace: WorkspaceCompilation) =
        baseCheckpointContracts
        @ targetCheckpointContracts workspace
        |> List.distinct

    let checkpointContractFor workspace checkpoint =
        contractsForWorkspace workspace
        |> List.tryFind (fun contract -> String.Equals(contract.Name, checkpoint, StringComparison.Ordinal))

    let checkpointContractJson workspace checkpoint =
        match checkpointContractFor workspace checkpoint with
        | Some contract ->
            {| name = contract.Name
               kind = CheckpointKind.toPortableName contract.CheckpointKind
               inputCheckpoint = contract.InputCheckpoint |> Option.toObj
               requiredBySpec = contract.RequiredBySpec
               profileSpecific = contract.ProfileSpecific |}
        | None ->
            {| name = checkpoint
               kind = "unknown"
               inputCheckpoint = null
               requiredBySpec = false
               profileSpecific = false |}

    let private diagnosticCodeIdentifiers diagnostics =
        diagnostics
        |> List.map (fun diagnostic -> DiagnosticCode.toIdentifier diagnostic.Code)
        |> List.distinct
        |> List.sort

    type TargetVerificationOutcome =
        { Diagnostics: Diagnostic list
          LoweringAttempted: bool }

    let private ensureTargetLoweringPreconditions (workspace: WorkspaceCompilation) checkpoint =
        if workspace.HasErrors then
            Result.Error(
                TargetCheckpointWorkspaceHasDiagnostics(
                    checkpoint,
                    List.length workspace.Diagnostics,
                    diagnosticCodeIdentifiers workspace.Diagnostics
                )
            )
        else
            let inputCheckpoint = targetInputCheckpoint workspace checkpoint
            let verificationDiagnostics = CheckpointVerification.verifyCheckpoint workspace inputCheckpoint

            if not (List.isEmpty verificationDiagnostics) then
                Result.Error(
                    TargetCheckpointMalformedInput(
                        checkpoint,
                        inputCheckpoint,
                        List.length verificationDiagnostics,
                        diagnosticCodeIdentifiers verificationDiagnostics
                    )
                )
            else
                Result.Ok()

    let private emitClrTargetManifest (workspace: WorkspaceCompilation) =
        let inputCheckpoint = targetInputCheckpoint workspace Stdlib.ClrTargetCheckpointName

        let clrSymbol moduleName functionName =
            $"{IlDotNetBackend.emittedModuleTypeName moduleName}.{IlDotNetBackend.emittedMethodName functionName}"

        let functions =
            workspace.KBackendIR
            |> List.collect (fun moduleDump ->
                moduleDump.Functions
                |> List.filter (fun functionDump -> not functionDump.Intrinsic)
                |> List.map (fun functionDump -> moduleDump.Name, functionDump))

        let functionSymbols =
            functions
            |> List.map (fun (moduleName, functionDump) -> clrSymbol moduleName functionDump.Name)
            |> List.sort

        let entrySymbols =
            functions
            |> List.choose (fun (moduleName, functionDump) ->
                if functionDump.EntryPoint then
                    Some(clrSymbol moduleName functionDump.Name)
                else
                    None)
            |> List.sort

        Result.Ok
            { ArtifactKind = "clr-assembly"
              TranslationUnitName = "Kappa.Generated.dll"
              InputCheckpoint = inputCheckpoint
              EntrySymbols = entrySymbols
              FunctionSymbols = functionSymbols
              SourceText = "" }

    let private tryEmitTargetTranslationUnitDetailed (workspace: WorkspaceCompilation) checkpoint =
        match workspace.Backend, checkpoint with
        | BackendProfile.Zig, checkpointName when checkpointName = Stdlib.ZigTargetCheckpointName ->
            match ensureTargetLoweringPreconditions workspace checkpoint with
            | Result.Ok () ->
                ZigCcBackend.emitTranslationUnit workspace
                |> Result.mapError (fun detail ->
                    TargetCheckpointEmitterFailure(
                        checkpoint,
                        BackendProfile.toPortableName workspace.Backend,
                        detail
                    )),
                true
            | Result.Error error ->
                Result.Error error, false
        | BackendProfile.DotNet, checkpointName when checkpointName = Stdlib.ClrTargetCheckpointName ->
            match ensureTargetLoweringPreconditions workspace checkpoint with
            | Result.Ok () ->
                emitClrTargetManifest workspace, true
            | Result.Error error ->
                Result.Error error, false
        | _ ->
            Result.Error(UnknownTargetCheckpoint checkpoint), false

    let tryEmitTargetTranslationUnit (workspace: WorkspaceCompilation) checkpoint =
        tryEmitTargetTranslationUnitDetailed workspace checkpoint
        |> fst
        |> Result.mapError (fun error -> (DiagnosticFact.describe (DiagnosticFact.targetCheckpoint error)).Message)

    let verifyTargetCheckpointDetailed (workspace: WorkspaceCompilation) checkpoint =
        let result, loweringAttempted = tryEmitTargetTranslationUnitDetailed workspace checkpoint

        let diagnostics =
            match result with
            | Result.Ok _ ->
                []
            | Result.Error error ->
                [ Diagnostics.errorFact "target-lowering" None None [] (DiagnosticFact.targetCheckpoint error) ]

        { Diagnostics = diagnostics
          LoweringAttempted = loweringAttempted }

    let verifyTargetCheckpoint (workspace: WorkspaceCompilation) checkpoint =
        (verifyTargetCheckpointDetailed workspace checkpoint).Diagnostics
