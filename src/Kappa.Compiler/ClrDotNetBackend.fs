namespace Kappa.Compiler

open System

module internal ClrDotNetBackend =
    let emitAssemblyArtifact (workspace: WorkspaceCompilation) (outputDirectory: string) =
        if workspace.HasErrors then
            Result.Error $"Cannot emit a CLR assembly for a workspace with diagnostics:{Environment.NewLine}{String.concat Environment.NewLine (workspace.Diagnostics |> List.map (fun diagnostic -> diagnostic.Message))}"
        else
            let verificationDiagnostics = CheckpointVerification.verifyCheckpoint workspace "KBackendIR"

            if not (List.isEmpty verificationDiagnostics) then
                Result.Error $"Cannot emit malformed KBackendIR:{Environment.NewLine}{String.concat Environment.NewLine (verificationDiagnostics |> List.map (fun diagnostic -> diagnostic.Message))}"
            else
                IlDotNetBackend.emitClrAssemblyArtifact workspace.ClrAssemblyIR outputDirectory
