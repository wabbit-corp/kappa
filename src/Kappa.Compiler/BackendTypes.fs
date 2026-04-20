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

type NativeArtifact =
    { OutputDirectory: string
      SourceFilePath: string
      ExecutableFilePath: string
      EntryPoint: string }

type NativeTranslationUnit =
    { TranslationUnitName: string
      InputCheckpoint: string
      EntrySymbols: string list
      FunctionSymbols: string list
      SourceText: string }
