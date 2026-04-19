namespace Kappa.Compiler

type DotNetDeployment =
    | Managed
    | NativeAot

type DotNetArtifact =
    { ProjectDirectory: string
      ProjectFilePath: string
      ProgramFilePath: string
      RuntimeFilePath: string
      GeneratedFilePath: string
      EntryPoint: string
      Deployment: DotNetDeployment }

type ClrAssemblyArtifact =
    { OutputDirectory: string
      AssemblyName: string
      AssemblyFilePath: string }
