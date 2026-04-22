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

    let ordinal phase =
        all
        |> List.findIndex ((=) phase)

    let phasesThrough phase =
        all
        |> List.take (ordinal phase + 1)

    let tryParsePhaseName (phaseNameText: string) =
        all
        |> List.tryFind (fun phase -> String.Equals(phaseName phase, phaseNameText, StringComparison.Ordinal))

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

type CompilerFingerprintKind =
    | SourceFingerprint
    | HeaderFingerprint
    | InterfaceFingerprint
    | BodyFingerprint
    | BackendFingerprint

module CompilerFingerprintKind =
    let toPortableName fingerprintKind =
        match fingerprintKind with
        | SourceFingerprint -> "source"
        | HeaderFingerprint -> "header"
        | InterfaceFingerprint -> "interface"
        | BodyFingerprint -> "body"
        | BackendFingerprint -> "backend"

type CompilerFingerprint =
    { Id: string
      FingerprintKind: CompilerFingerprintKind
      InputKey: string
      Identity: string
      AnalysisSessionIdentity: string
      BuildConfigurationIdentity: string
      BackendProfile: string
      BackendIntrinsicSet: string
      DependencyFingerprintIds: string list }

type IncrementalUnitKind =
    | SourceFileTextUnit
    | ModuleImportSurfaceUnit
    | DeclarationHeaderUnit
    | DeclarationBodyUnit
    | MacroExpansionUnit
    | ModuleInterfaceUnit
    | KCoreModuleUnit
    | KBackendIRModuleUnit
    | TargetLoweringUnit

module IncrementalUnitKind =
    let toPortableName unitKind =
        match unitKind with
        | SourceFileTextUnit -> "source-file-text"
        | ModuleImportSurfaceUnit -> "module-import-surface"
        | DeclarationHeaderUnit -> "declaration-header"
        | DeclarationBodyUnit -> "declaration-body"
        | MacroExpansionUnit -> "macro-expansion"
        | ModuleInterfaceUnit -> "module-interface"
        | KCoreModuleUnit -> "KCore-module"
        | KBackendIRModuleUnit -> "KBackendIR-module"
        | TargetLoweringUnit -> "target-lowering"

type IncrementalUnit =
    { Id: string
      UnitKind: IncrementalUnitKind
      InputKey: string
      OutputCheckpoint: string option
      FingerprintIds: string list
      DependencyUnitIds: string list
      AnalysisSessionIdentity: string
      BuildConfigurationIdentity: string
      BackendProfile: string
      BackendIntrinsicSet: string }

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

type OwnershipBindingFact =
    { BindingId: string
      BindingName: string
      BindingKind: string
      BindingDeclaredQuantity: string option
      BindingInferredDemand: string
      BindingState: string
      BindingPlaceRoot: string
      BindingPlacePath: string list
      BindingBorrowRegionId: string option
      BindingOrigin: SourceLocation option }

type OwnershipUseFact =
    { UseId: string
      UseKindName: string
      UseTargetBindingId: string option
      UseTargetName: string
      UsePlaceRoot: string
      UsePlacePath: string list
      UseOrigin: SourceLocation option }

type OwnershipBorrowRegionFact =
    { BorrowRegionId: string
      BorrowRegionExplicitName: string option
      BorrowRegionIntroductionOrigin: SourceLocation option
      BorrowRegionOwnerScope: string }

type OwnershipUsingScopeFact =
    { UsingScopeId: string
      UsingScopeSurfaceOrigin: SourceLocation option
      UsingScopeHiddenOwnedBinding: string
      UsingScopeSharedRegionId: string
      UsingScopeHiddenReleaseObligation: string }

type OwnershipClosureFact =
    { ClosureId: string
      ClosureName: string option
      ClosureCaptureBindingIds: string list
      ClosureCaptureNames: string list
      ClosureRegionEnvironment: string list
      ClosureEscapeStatus: string
      ClosureOrigin: SourceLocation option }

type OwnershipFactSet =
    { OwnershipBindings: OwnershipBindingFact list
      OwnershipUses: OwnershipUseFact list
      OwnershipBorrowRegions: OwnershipBorrowRegionFact list
      OwnershipUsingScopes: OwnershipUsingScopeFact list
      OwnershipClosures: OwnershipClosureFact list
      OwnershipDeferred: string list
      OwnershipDiagnostics: string list }
