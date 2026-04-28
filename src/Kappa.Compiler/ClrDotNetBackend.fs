namespace Kappa.Compiler

open System

// Emits the public dotnet backend artifact from precomputed CLR assembly IR.
module internal ClrDotNetBackend =
    let emitAssemblyArtifact (workspace: WorkspaceCompilation) (outputDirectory: string) =
        if workspace.HasErrors then
            Result.Error $"Cannot emit a CLR assembly for a workspace with diagnostics:{Environment.NewLine}{String.concat Environment.NewLine (workspace.Diagnostics |> List.map (fun diagnostic -> diagnostic.Message))}"
        else
            let verificationDiagnostics = CheckpointVerification.verifyCheckpoint workspace "KBackendIR"

            if not (List.isEmpty verificationDiagnostics) then
                Result.Error $"Cannot emit malformed KBackendIR:{Environment.NewLine}{String.concat Environment.NewLine (verificationDiagnostics |> List.map (fun diagnostic -> diagnostic.Message))}"
            else
                if ClrAssemblyIR.modulesUseEffectRuntime workspace.ClrAssemblyIR then
                    IlDotNetEffectBackend.emitClrAssemblyArtifact workspace.ClrAssemblyIR outputDirectory
                else
                    IlDotNetBackend.emitClrAssemblyArtifact workspace.ClrAssemblyIR outputDirectory
