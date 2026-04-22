namespace Kappa.Compiler

type WorkspaceCompilation =
    { SourceRoot: string
      PackageMode: bool
      BackendProfile: string
      DeploymentMode: string
      BackendIntrinsicIdentity: string
      BuildConfigurationIdentity: string
      AnalysisSessionIdentity: string
      ElaborationAvailableIntrinsicTerms: string list
      Documents: ParsedDocument list
      KFrontIR: KFrontIRModule list
      KCore: KCoreModule list
      KRuntimeIR: KRuntimeModule list
      KBackendIR: KBackendModule list
      ClrAssemblyIR: ClrAssemblyModule list
      Diagnostics: Diagnostic list
      PipelineTrace: PipelineTraceStep list }

    member this.HasErrors =
        this.Diagnostics |> List.exists (fun diagnostic -> diagnostic.Severity = Error)
