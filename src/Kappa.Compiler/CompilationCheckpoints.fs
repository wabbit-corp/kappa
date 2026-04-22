namespace Kappa.Compiler

open System

module internal CompilationCheckpoints =
    let targetCheckpointNames (workspace: WorkspaceCompilation) =
        Stdlib.targetCheckpointNamesFor workspace.BackendProfile

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
            checkpointContract "KRuntimeIR" ImplementationDefinedCheckpoint (Some "KCore") false false
            checkpointContract "KBackendIR" KBackendIRCheckpoint (Some "KRuntimeIR") true false
        ]

    let private targetCheckpointContracts (workspace: WorkspaceCompilation) =
        targetCheckpointNames workspace
        |> List.map (fun checkpoint ->
            checkpointContract checkpoint TargetLoweringCheckpoint (Some "KBackendIR") true true)

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

    let private aggregateDiagnostics diagnostics =
        diagnostics
        |> List.map (fun diagnostic -> diagnostic.Message)
        |> String.concat Environment.NewLine

    let private emitClrTargetManifest (workspace: WorkspaceCompilation) =
        let verificationDiagnostics = CheckpointVerification.verifyCheckpoint workspace "KBackendIR"

        if not (List.isEmpty verificationDiagnostics) then
            Result.Error
                $"Cannot emit CLR target manifest from malformed KBackendIR:{Environment.NewLine}{aggregateDiagnostics verificationDiagnostics}"
        else
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
                  InputCheckpoint = "KBackendIR"
                  EntrySymbols = entrySymbols
                  FunctionSymbols = functionSymbols
                  SourceText = "" }

    let tryEmitTargetTranslationUnit (workspace: WorkspaceCompilation) checkpoint =
        match Stdlib.normalizeBackendProfile workspace.BackendProfile, checkpoint with
        | "zig", checkpointName when checkpointName = Stdlib.ZigTargetCheckpointName ->
            ZigCcBackend.emitTranslationUnit workspace
        | ("dotnet" | "dotnet-il"), checkpointName when checkpointName = Stdlib.ClrTargetCheckpointName ->
            emitClrTargetManifest workspace
        | _ ->
            Result.Error $"Unknown checkpoint '{checkpoint}'."

    let verifyTargetCheckpoint (workspace: WorkspaceCompilation) checkpoint =
        match tryEmitTargetTranslationUnit workspace checkpoint with
        | Result.Ok _ ->
            []
        | Result.Error message ->
            [ { Severity = Error
                Code = "E_TARGET_CHECKPOINT"
                Stage = Some "target-lowering"
                Phase = None
                Message = message
                Location = None
                RelatedLocations = [] } ]
