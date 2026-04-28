namespace Kappa.Compiler

// Shared public artifact record types used by backend emitters and their callers.
type BackendProfile =
    | Interpreter
    | DotNet
    | Zig
    | Unknown of string

[<RequireQualifiedAccess>]
module BackendProfile =
    let defaultValue = Interpreter

    let toPortableName backendProfile =
        match backendProfile with
        | Interpreter -> "interpreter"
        | DotNet -> "dotnet"
        | Zig -> "zig"
        | Unknown portableName -> portableName

    let normalizeConfigured (backendProfile: string) =
        if System.String.IsNullOrWhiteSpace(backendProfile) then
            defaultValue
        else
            match backendProfile.Trim().ToLowerInvariant() with
            | "zigcc" -> Zig
            | "dotnet-il" -> DotNet
            | "dotnet" -> DotNet
            | "zig" -> Zig
            | "interpreter" -> Interpreter
            | normalized -> Unknown normalized

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
