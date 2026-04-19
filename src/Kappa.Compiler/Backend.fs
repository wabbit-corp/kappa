namespace Kappa.Compiler

module Backend =
    let emitHostedDotNetArtifact
        (workspace: WorkspaceCompilation)
        (entryPoint: string)
        (outputDirectory: string)
        (deployment: DotNetDeployment)
        =
        HostedRuntimeDotNetBackend.emitDotNetArtifact workspace entryPoint outputDirectory deployment

    let emitDotNetArtifact
        (workspace: WorkspaceCompilation)
        (entryPoint: string)
        (outputDirectory: string)
        (deployment: DotNetDeployment)
        =
        emitHostedDotNetArtifact workspace entryPoint outputDirectory deployment

    let emitIlAssemblyArtifact (workspace: WorkspaceCompilation) (outputDirectory: string) =
        IlDotNetBackend.emitAssemblyArtifact workspace outputDirectory
