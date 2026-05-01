namespace Kappa.Compiler

open System

// Emits the public dotnet backend artifact from precomputed CLR assembly IR.
module internal ClrDotNetBackend =
    let emitAssemblyArtifact (workspace: WorkspaceCompilation) (outputDirectory: string) =
        if workspace.HasErrors then
            Result.Error(
                DiagnosticFact.ClrArtifactEmitterError.message
                    (ClrArtifactWorkspaceHasDiagnostics(
                        PlainClrAssembly,
                        String.concat Environment.NewLine (workspace.Diagnostics |> List.map (fun diagnostic -> diagnostic.Message))
                    ))
            )
        else
            let verificationDiagnostics = CheckpointVerification.verifyCheckpoint workspace "KBackendIR"

            if not (List.isEmpty verificationDiagnostics) then
                Result.Error(
                    DiagnosticFact.ClrArtifactEmitterError.message
                        (ClrArtifactMalformedBackendIr(
                            String.concat Environment.NewLine (verificationDiagnostics |> List.map (fun diagnostic -> diagnostic.Message))
                        ))
                )
            else
                if ClrAssemblyIR.modulesUseEffectRuntime workspace.ClrAssemblyIR then
                    IlDotNetEffectBackend.emitClrAssemblyArtifact workspace.ClrAssemblyIR outputDirectory
                else
                    IlDotNetBackend.emitClrAssemblyArtifact workspace.ClrAssemblyIR outputDirectory

    let emitAssemblyArtifactForEntryPoint
        (workspace: WorkspaceCompilation)
        (entryModuleName: string)
        (entryBindingName: string)
        (outputDirectory: string)
        =
        if workspace.HasErrors then
            Result.Error(
                DiagnosticFact.ClrArtifactEmitterError.message
                    (ClrArtifactWorkspaceHasDiagnostics(
                        PlainClrAssembly,
                        String.concat Environment.NewLine (workspace.Diagnostics |> List.map (fun diagnostic -> diagnostic.Message))
                    ))
            )
        else
            let verificationDiagnostics = CheckpointVerification.verifyCheckpoint workspace "KBackendIR"

            if not (List.isEmpty verificationDiagnostics) then
                Result.Error(
                    DiagnosticFact.ClrArtifactEmitterError.message
                        (ClrArtifactMalformedBackendIr(
                            String.concat Environment.NewLine (verificationDiagnostics |> List.map (fun diagnostic -> diagnostic.Message))
                        ))
                )
            else
                if ClrAssemblyIR.modulesUseEffectRuntime workspace.ClrAssemblyIR then
                    IlDotNetEffectBackend.emitClrAssemblyArtifact workspace.ClrAssemblyIR outputDirectory
                else
                    IlDotNetBackend.emitClrAssemblyArtifactWithRoots
                        workspace.ClrAssemblyIR
                        (Some [ entryModuleName, entryBindingName ])
                        outputDirectory
