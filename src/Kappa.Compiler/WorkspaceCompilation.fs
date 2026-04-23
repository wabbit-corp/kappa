namespace Kappa.Compiler

// The aggregate workspace snapshot that threads checkpoints, artifacts, and metadata through the compiler.
type FrontendSnapshot =
    { Modules: KFrontIRModule list
      Diagnostics: Diagnostic list }

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
      FrontendSnapshots: Map<KFrontIRPhase, FrontendSnapshot>
      KFrontIR: KFrontIRModule list
      KCore: KCoreModule list
      KRuntimeIR: KRuntimeModule list
      KBackendIR: KBackendModule list
      ClrAssemblyIR: ClrAssemblyModule list
      Diagnostics: Diagnostic list
      PipelineTrace: PipelineTraceStep list }

    member this.HasErrors =
        this.Diagnostics |> List.exists (fun diagnostic -> diagnostic.Severity = Error)
