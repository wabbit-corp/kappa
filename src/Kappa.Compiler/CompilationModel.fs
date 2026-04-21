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
      DeploymentMode: string
      FileSystem: IFileSystem }

module CompilationOptions =
    let createWithFileSystem (fileSystem: IFileSystem) sourceRoot =
        { SourceRoot = fileSystem.GetFullPath(sourceRoot)
          PackageMode = true
          BackendProfile = "interpreter"
          DeploymentMode = "default"
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
    | MODAL_SOLVE
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
            MODAL_SOLVE
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
        | MODAL_SOLVE -> "KFrontIR.MODAL_SOLVE"
        | CHECKERS -> "KFrontIR.CHECKERS"
        | CORE_LOWERING -> "KFrontIR.CORE_LOWERING"

    let phaseName phase =
        checkpointName phase
        |> fun checkpoint -> checkpoint.Substring("KFrontIR.".Length)

type CheckpointKind =
    | SurfaceSourceCheckpoint
    | KFrontIRCheckpoint
    | KCoreCheckpoint
    | ImplementationDefinedCheckpoint
    | KBackendIRCheckpoint
    | TargetLoweringCheckpoint

module CheckpointKind =
    let toPortableName checkpointKind =
        match checkpointKind with
        | SurfaceSourceCheckpoint -> "surface-source"
        | KFrontIRCheckpoint -> "KFrontIR"
        | KCoreCheckpoint -> "KCore"
        | ImplementationDefinedCheckpoint -> "implementation-defined"
        | KBackendIRCheckpoint -> "KBackendIR"
        | TargetLoweringCheckpoint -> "target-lowering"

type CheckpointContract =
    { Name: string
      CheckpointKind: CheckpointKind
      InputCheckpoint: string option
      RequiredBySpec: bool
      ProfileSpecific: bool }

type CheckpointVerificationResult =
    { Checkpoint: string
      Succeeded: bool
      Diagnostics: Diagnostic list }

type PortableRuntimeObligationOwner =
    | KBackendIRGuaranteed
    | BackendSpecificRuntime
    | DeferredRuntimeObligation

type PortableRuntimeObligation =
    { Name: string
      Owner: PortableRuntimeObligationOwner
      Description: string }

type AnalysisSession =
    { Identity: string
      SourceRoot: string
      PackageMode: bool
      BuildConfigurationIdentity: string
      BackendProfile: string
      BackendIntrinsicSet: string
      DeploymentMode: string }

type QueryKind =
    | ParseSourceFileQuery
    | BuildKFrontIRQuery
    | AdvanceKFrontIRPhaseQuery
    | ComputeDiagnosticsQuery
    | LowerKCoreQuery
    | LowerKRuntimeIRQuery
    | LowerKBackendIRQuery
    | LowerTargetQuery

module QueryKind =
    let toPortableName queryKind =
        match queryKind with
        | ParseSourceFileQuery -> "parseSourceFile"
        | BuildKFrontIRQuery -> "buildKFrontIR"
        | AdvanceKFrontIRPhaseQuery -> "advanceKFrontIRPhase"
        | ComputeDiagnosticsQuery -> "computeDiagnostics"
        | LowerKCoreQuery -> "lowerKCore"
        | LowerKRuntimeIRQuery -> "lowerKRuntimeIR"
        | LowerKBackendIRQuery -> "lowerKBackendIR"
        | LowerTargetQuery -> "lowerTarget"

type QueryRecord =
    { Id: string
      QueryKind: QueryKind
      InputKey: string
      OutputCheckpoint: string
      RequiredPhase: KFrontIRPhase option
      AnalysisSessionIdentity: string
      BuildConfigurationIdentity: string
      BackendProfile: string
      BackendIntrinsicSet: string
      DependencyIds: string list }

type PipelineTraceEvent =
    | Parse
    | BuildKFrontIR
    | AdvancePhase
    | EmitInterface
    | EvaluateElaboration
    | LowerKCore
    | LowerKRuntimeIR
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
        | LowerKRuntimeIR -> "lowerKRuntimeIR"
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
    | KRuntimeIRUnit
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
        | KRuntimeIRUnit -> "KRuntimeIRUnit"
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
    | KCoreExecute of KCoreExpression
    | KCoreLet of bindingName: string * value: KCoreExpression * body: KCoreExpression
    | KCoreSequence of KCoreExpression * KCoreExpression
    | KCoreWhile of condition: KCoreExpression * body: KCoreExpression
    | KCoreApply of KCoreExpression * KCoreExpression list
    | KCoreDictionaryValue of moduleName: string * traitName: string * instanceKey: string
    | KCoreTraitCall of traitName: string * memberName: string * dictionary: KCoreExpression * arguments: KCoreExpression list
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

type KRuntimeExpression =
    | KRuntimeLiteral of LiteralValue
    | KRuntimeName of string list
    | KRuntimeClosure of string list * KRuntimeExpression
    | KRuntimeIfThenElse of KRuntimeExpression * KRuntimeExpression * KRuntimeExpression
    | KRuntimeMatch of KRuntimeExpression * KRuntimeMatchCase list
    | KRuntimeExecute of KRuntimeExpression
    | KRuntimeLet of bindingName: string * value: KRuntimeExpression * body: KRuntimeExpression
    | KRuntimeSequence of KRuntimeExpression * KRuntimeExpression
    | KRuntimeWhile of condition: KRuntimeExpression * body: KRuntimeExpression
    | KRuntimeApply of KRuntimeExpression * KRuntimeExpression list
    | KRuntimeDictionaryValue of moduleName: string * traitName: string * instanceKey: string
    | KRuntimeTraitCall of traitName: string * memberName: string * dictionary: KRuntimeExpression * arguments: KRuntimeExpression list
    | KRuntimeUnary of operatorName: string * KRuntimeExpression
    | KRuntimeBinary of KRuntimeExpression * operatorName: string * KRuntimeExpression
    | KRuntimePrefixedString of prefix: string * parts: KRuntimeStringPart list

and KRuntimeStringPart =
    | KRuntimeStringText of string
    | KRuntimeStringInterpolation of KRuntimeExpression

and KRuntimePattern =
    | KRuntimeWildcardPattern
    | KRuntimeNamePattern of string
    | KRuntimeLiteralPattern of LiteralValue
    | KRuntimeConstructorPattern of string list * KRuntimePattern list

and KRuntimeMatchCase =
    { Pattern: KRuntimePattern
      Body: KRuntimeExpression }

type KRuntimeConstructor =
    { Name: string
      Arity: int
      TypeName: string
      Provenance: KCoreOrigin }

type KRuntimeBinding =
    { Name: string
      Parameters: string list
      Body: KRuntimeExpression option
      Intrinsic: bool
      Provenance: KCoreOrigin }

type KRuntimeModule =
    { Name: string
      SourceFile: string
      Imports: ImportSpec list
      Exports: string list
      IntrinsicTerms: string list
      Constructors: KRuntimeConstructor list
      Bindings: KRuntimeBinding list }

type KBackendRepresentationClass =
    | BackendRepInt64
    | BackendRepFloat64
    | BackendRepBoolean
    | BackendRepString
    | BackendRepChar
    | BackendRepUnit
    | BackendRepRef of elementRepresentation: KBackendRepresentationClass
    | BackendRepDictionary of traitName: string
    | BackendRepTaggedData of moduleName: string * typeName: string
    | BackendRepClosure of environmentLayout: string
    | BackendRepIOAction
    | BackendRepOpaque of string option

type KBackendParameter =
    { Name: string
      Representation: KBackendRepresentationClass }

type KBackendCapture =
    { Name: string
      Representation: KBackendRepresentationClass }

type KBackendCallingConvention =
    { RuntimeArity: int
      ParameterRepresentations: KBackendRepresentationClass list
      ResultRepresentation: KBackendRepresentationClass option
      RetainedDictionaryParameters: string list }

type KBackendResolvedName =
    | BackendLocalName of name: string * representation: KBackendRepresentationClass option
    | BackendGlobalBindingName of moduleName: string * bindingName: string * representation: KBackendRepresentationClass option
    | BackendIntrinsicName of moduleName: string * bindingName: string * representation: KBackendRepresentationClass option
    | BackendConstructorName of
        moduleName: string *
        typeName: string *
        constructorName: string *
        tag: int *
        arity: int *
        representation: KBackendRepresentationClass

type KBackendExpression =
    | BackendLiteral of LiteralValue * KBackendRepresentationClass
    | BackendName of KBackendResolvedName
    | BackendClosure of
        parameters: KBackendParameter list *
        captures: KBackendCapture list *
        environmentLayout: string *
        body: KBackendExpression *
        convention: KBackendCallingConvention *
        representation: KBackendRepresentationClass
    | BackendIfThenElse of
        condition: KBackendExpression *
        whenTrue: KBackendExpression *
        whenFalse: KBackendExpression *
        resultRepresentation: KBackendRepresentationClass
    | BackendMatch of
        scrutinee: KBackendExpression *
        cases: KBackendMatchCase list *
        resultRepresentation: KBackendRepresentationClass
    | BackendExecute of
        expression: KBackendExpression *
        resultRepresentation: KBackendRepresentationClass
    | BackendLet of
        binding: KBackendParameter *
        value: KBackendExpression *
        body: KBackendExpression *
        resultRepresentation: KBackendRepresentationClass
    | BackendSequence of
        first: KBackendExpression *
        second: KBackendExpression *
        resultRepresentation: KBackendRepresentationClass
    | BackendWhile of
        condition: KBackendExpression *
        body: KBackendExpression
    | BackendCall of
        callee: KBackendExpression *
        arguments: KBackendExpression list *
        convention: KBackendCallingConvention *
        resultRepresentation: KBackendRepresentationClass
    | BackendDictionaryValue of
        moduleName: string *
        traitName: string *
        instanceKey: string *
        representation: KBackendRepresentationClass
    | BackendTraitCall of
        traitName: string *
        memberName: string *
        dictionary: KBackendExpression *
        arguments: KBackendExpression list *
        resultRepresentation: KBackendRepresentationClass
    | BackendConstructData of
        moduleName: string *
        typeName: string *
        constructorName: string *
        tag: int *
        fields: KBackendExpression list *
        representation: KBackendRepresentationClass
    | BackendPrefixedString of
        prefix: string *
        parts: KBackendStringPart list *
        resultRepresentation: KBackendRepresentationClass

and KBackendStringPart =
    | BackendStringText of string
    | BackendStringInterpolation of KBackendExpression

and KBackendPatternBinding =
    { Name: string
      Representation: KBackendRepresentationClass }

and KBackendPattern =
    | BackendWildcardPattern
    | BackendBindPattern of KBackendPatternBinding
    | BackendLiteralPattern of LiteralValue * KBackendRepresentationClass
    | BackendConstructorPattern of
        moduleName: string *
        typeName: string *
        constructorName: string *
        tag: int *
        fieldPatterns: KBackendPattern list

and KBackendMatchCase =
    { Pattern: KBackendPattern
      Body: KBackendExpression }

type KBackendEnvironmentLayout =
    { Name: string
      Slots: KBackendCapture list }

type KBackendConstructorLayout =
    { Name: string
      Tag: int
      FieldRepresentations: KBackendRepresentationClass list
      Provenance: KCoreOrigin }

type KBackendDataLayout =
    { TypeName: string
      RepresentationClass: string
      TagEncoding: string
      Constructors: KBackendConstructorLayout list
      Provenance: KCoreOrigin }

type KBackendControlForm =
    | StructuredExpression

type KBackendFunction =
    { Name: string
      Parameters: KBackendParameter list
      CallingConvention: KBackendCallingConvention
      ReturnRepresentation: KBackendRepresentationClass option
      EnvironmentLayout: string option
      Intrinsic: bool
      Exported: bool
      EntryPoint: bool
      ControlForm: KBackendControlForm
      Body: KBackendExpression option
      Provenance: KCoreOrigin }

type KBackendModule =
    { Name: string
      SourceFile: string
      Imports: ImportSpec list
      Exports: string list
      EntryPoints: string list
      IntrinsicTerms: string list
      DataLayouts: KBackendDataLayout list
      EnvironmentLayouts: KBackendEnvironmentLayout list
      Functions: KBackendFunction list }

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
      Diagnostics: Diagnostic list
      PipelineTrace: PipelineTraceStep list }

    member this.HasErrors =
        this.Diagnostics |> List.exists (fun diagnostic -> diagnostic.Severity = Error)
