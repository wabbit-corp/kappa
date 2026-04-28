namespace Kappa.Compiler

// Shared public artifact record types used by backend emitters and their callers.
type DotNetDeployment =
    | Managed
    | NativeAot

[<RequireQualifiedAccess>]
module DotNetDeployment =
    let unsupportedForDotNetMessage =
        "--native-aot is not supported for backend dotnet. Omit --native-aot for the managed dotnet backend."

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
    { ArtifactKind: string
      TranslationUnitName: string
      InputCheckpoint: string
      EntrySymbols: string list
      FunctionSymbols: string list
      SourceText: string }
