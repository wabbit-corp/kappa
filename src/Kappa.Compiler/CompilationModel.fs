namespace Kappa.Compiler

open System
open System.IO

type IFileSystem =
    abstract member GetFullPath: string -> string
    abstract member FileExists: string -> bool
    abstract member DirectoryExists: string -> bool
    abstract member EnumerateFiles: string * string * SearchOption -> seq<string>
    abstract member ReadAllText: string -> string

module FileSystem =
    let defaultImplementation =
        { new IFileSystem with
            member _.GetFullPath(path: string) = Path.GetFullPath(path)
            member _.FileExists(path: string) = File.Exists(path)
            member _.DirectoryExists(path: string) = Directory.Exists(path)

            member _.EnumerateFiles(path: string, searchPattern: string, searchOption: SearchOption) =
                Directory.EnumerateFiles(path, searchPattern, searchOption)

            member _.ReadAllText(path: string) = File.ReadAllText(path) }

type CompilationOptions =
    { SourceRoot: string
      PackageMode: bool
      BackendProfile: string
      FileSystem: IFileSystem }

module CompilationOptions =
    let createWithFileSystem (fileSystem: IFileSystem) sourceRoot =
        { SourceRoot = fileSystem.GetFullPath(sourceRoot)
          PackageMode = true
          BackendProfile = "interpreter"
          FileSystem = fileSystem }

    let create sourceRoot =
        createWithFileSystem FileSystem.defaultImplementation sourceRoot

type ParsedDocument =
    { Source: SourceText
      InferredModuleName: string list option
      Syntax: CompilationUnit
      Diagnostics: Diagnostic list }

    member this.ModuleName =
        match this.Syntax.ModuleHeader with
        | Some moduleName -> Some moduleName
        | None -> this.InferredModuleName

type KFrontIRPhase =
    | RAW
    | IMPORTS
    | DECLARATION_SHAPES
    | HEADER_TYPES
    | STATUS
    | IMPLICIT_SIGNATURES
    | BODY_RESOLVE
    | CHECKERS
    | CORE_LOWERING

module KFrontIRPhase =
    let all =
        [
            RAW
            IMPORTS
            DECLARATION_SHAPES
            HEADER_TYPES
            STATUS
            IMPLICIT_SIGNATURES
            BODY_RESOLVE
            CHECKERS
            CORE_LOWERING
        ]

    let checkpointName phase =
        match phase with
        | RAW -> "KFrontIR.RAW"
        | IMPORTS -> "KFrontIR.IMPORTS"
        | DECLARATION_SHAPES -> "KFrontIR.DECLARATION_SHAPES"
        | HEADER_TYPES -> "KFrontIR.HEADER_TYPES"
        | STATUS -> "KFrontIR.STATUS"
        | IMPLICIT_SIGNATURES -> "KFrontIR.IMPLICIT_SIGNATURES"
        | BODY_RESOLVE -> "KFrontIR.BODY_RESOLVE"
        | CHECKERS -> "KFrontIR.CHECKERS"
        | CORE_LOWERING -> "KFrontIR.CORE_LOWERING"

    let phaseName phase =
        checkpointName phase
        |> fun checkpoint -> checkpoint.Substring("KFrontIR.".Length)

type PipelineTraceEvent =
    | Parse
    | BuildKFrontIR
    | AdvancePhase
    | EmitInterface
    | EvaluateElaboration
    | LowerKCore
    | LowerKBackendIR
    | LowerTarget
    | Reuse
    | Verify

module PipelineTraceEvent =
    let toPortableName eventName =
        match eventName with
        | Parse -> "parse"
        | BuildKFrontIR -> "buildKFrontIR"
        | AdvancePhase -> "advancePhase"
        | EmitInterface -> "emitInterface"
        | EvaluateElaboration -> "evaluateElaboration"
        | LowerKCore -> "lowerKCore"
        | LowerKBackendIR -> "lowerKBackendIR"
        | LowerTarget -> "lowerTarget"
        | Reuse -> "reuse"
        | Verify -> "verify"

type PipelineTraceSubject =
    | File
    | Declaration
    | Module
    | Interface
    | KCoreUnit
    | KBackendIRUnit
    | TargetUnit

module PipelineTraceSubject =
    let toPortableName subject =
        match subject with
        | File -> "file"
        | Declaration -> "declaration"
        | Module -> "module"
        | Interface -> "interface"
        | KCoreUnit -> "KCoreUnit"
        | KBackendIRUnit -> "KBackendIRUnit"
        | TargetUnit -> "targetUnit"

type PipelineTraceStep =
    { Event: PipelineTraceEvent
      Subject: PipelineTraceSubject
      StepName: string
      InputCheckpoint: string
      OutputCheckpoint: string
      ChangedRepresentation: bool
      VerificationAttempted: bool
      VerificationSucceeded: bool option }

type StageDumpFormat =
    | Json
    | SExpression

type KFrontIRModule =
    { FilePath: string
      ModuleHeader: string list option
      InferredModuleName: string list option
      ModuleIdentity: string list option
      ModuleAttributes: string list
      Imports: ImportSpec list
      Tokens: Token list
      Declarations: TopLevelDeclaration list
      Diagnostics: Diagnostic list
      ResolvedPhases: Set<KFrontIRPhase> }

type KCoreOrigin =
    { FilePath: string
      ModuleName: string
      DeclarationName: string option
      IntroductionKind: string }

type KCoreParameter =
    { Name: string
      TypeText: string option }

type KCoreExpression =
    | KCoreLiteral of LiteralValue
    | KCoreName of string list
    | KCoreLambda of KCoreParameter list * KCoreExpression
    | KCoreIfThenElse of KCoreExpression * KCoreExpression * KCoreExpression
    | KCoreMatch of KCoreExpression * KCoreMatchCase list
    | KCoreApply of KCoreExpression * KCoreExpression list
    | KCoreUnary of operatorName: string * KCoreExpression
    | KCoreBinary of KCoreExpression * operatorName: string * KCoreExpression
    | KCorePrefixedString of prefix: string * parts: KCoreStringPart list

and KCoreStringPart =
    | KCoreStringText of string
    | KCoreStringInterpolation of KCoreExpression

and KCorePattern =
    | KCoreWildcardPattern
    | KCoreNamePattern of string
    | KCoreLiteralPattern of LiteralValue
    | KCoreConstructorPattern of string list * KCorePattern list

and KCoreMatchCase =
    { Pattern: KCorePattern
      Body: KCoreExpression }

type KCoreBinding =
    { Visibility: Visibility option
      IsOpaque: bool
      Name: string option
      Parameters: KCoreParameter list
      ReturnTypeText: string option
      Body: KCoreExpression option
      BodyText: string option
      Provenance: KCoreOrigin }

type KCoreDeclaration =
    { Source: TopLevelDeclaration
      Binding: KCoreBinding option
      Provenance: KCoreOrigin }

type KCoreModule =
    { Name: string
      SourceFile: string
      ModuleAttributes: string list
      Imports: ImportSpec list
      IntrinsicTerms: string list
      Declarations: KCoreDeclaration list }

type KBackendExpression =
    | KBackendLiteral of LiteralValue
    | KBackendName of string list
    | KBackendClosure of string list * KBackendExpression
    | KBackendIfThenElse of KBackendExpression * KBackendExpression * KBackendExpression
    | KBackendMatch of KBackendExpression * KBackendMatchCase list
    | KBackendApply of KBackendExpression * KBackendExpression list
    | KBackendUnary of operatorName: string * KBackendExpression
    | KBackendBinary of KBackendExpression * operatorName: string * KBackendExpression
    | KBackendPrefixedString of prefix: string * parts: KBackendStringPart list

and KBackendStringPart =
    | KBackendStringText of string
    | KBackendStringInterpolation of KBackendExpression

and KBackendPattern =
    | KBackendWildcardPattern
    | KBackendNamePattern of string
    | KBackendLiteralPattern of LiteralValue
    | KBackendConstructorPattern of string list * KBackendPattern list

and KBackendMatchCase =
    { Pattern: KBackendPattern
      Body: KBackendExpression }

type KBackendConstructor =
    { Name: string
      Arity: int
      TypeName: string
      Provenance: KCoreOrigin }

type KBackendBinding =
    { Name: string
      Parameters: string list
      Body: KBackendExpression option
      Intrinsic: bool
      Provenance: KCoreOrigin }

type KBackendModule =
    { Name: string
      SourceFile: string
      Imports: ImportSpec list
      Exports: string list
      IntrinsicTerms: string list
      Constructors: KBackendConstructor list
      Bindings: KBackendBinding list }

type WorkspaceCompilation =
    { SourceRoot: string
      PackageMode: bool
      BackendProfile: string
      BackendIntrinsicIdentity: string
      ElaborationAvailableIntrinsicTerms: string list
      Documents: ParsedDocument list
      KFrontIR: KFrontIRModule list
      KCore: KCoreModule list
      KBackendIR: KBackendModule list
      Diagnostics: Diagnostic list
      PipelineTrace: PipelineTraceStep list }

    member this.HasErrors =
        this.Diagnostics |> List.exists (fun diagnostic -> diagnostic.Severity = Error)
