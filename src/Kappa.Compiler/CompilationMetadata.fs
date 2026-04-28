namespace Kappa.Compiler

open System
open System.IO
open System.Security.Cryptography
open System.Text
open CompilationCommon
open CompilationFrontend

// Computes analysis-session, fingerprint, incremental-unit, and runtime-obligation metadata.
module internal CompilationMetadata =
    let private sha256Hex (text: string) =
        text
        |> Encoding.UTF8.GetBytes
        |> SHA256.HashData
        |> Convert.ToHexString
        |> fun hex -> hex.ToLowerInvariant()

    let private targetCheckpointNames (workspace: WorkspaceCompilation) =
        Stdlib.targetCheckpointNamesFor workspace.BackendProfile

    let private targetInputCheckpoint (_workspace: WorkspaceCompilation) _checkpoint = "KBackendIR"

    let portableRuntimeObligations (_workspace: WorkspaceCompilation) =
        [
            { Name = "tagged-data-layout"
              Owner = KBackendIRGuaranteed
              Description = "KBackendIR fixes tagged ADT layout, constructor tags, and field representation classes before target lowering." }
            { Name = "runtime-calling-convention"
              Owner = KBackendIRGuaranteed
              Description = "KBackendIR functions, closures, calls, and entrypoints carry fixed runtime arity and representation-level calling conventions." }
            { Name = "retained-dictionaries-and-intrinsics"
              Owner = KBackendIRGuaranteed
              Description = "Retained dictionaries and runtime backend intrinsics must have concrete KBackendIR representations or verification rejects the checkpoint." }
            { Name = "memory-management"
              Owner = BackendSpecificRuntime
              Description = "Allocation and collection strategy is backend-specific; target lowering must preserve KBackendIR value and lifetime semantics." }
            { Name = "deterministic-cleanup"
              Owner = KBackendIRGuaranteed
              Description = "KCore, KRuntimeIR, and KBackendIR carry explicit cleanup scopes and scheduled exit actions; target lowering must preserve deferred and release sequencing." }
            { Name = "effect-handlers"
              Owner = DeferredRuntimeObligation
              Description = "Scoped-effect handlers are carried through KCore and KRuntimeIR, lowered into KBackendIR runtime forms, and then lowered by effect-capable target backends. Backend profiles that lack the required runtime support still reject unsupported effect runtime constructs explicitly." }
        ]

    let analysisSession (workspace: WorkspaceCompilation) =
        { Identity = workspace.AnalysisSessionIdentity
          SourceRoot = workspace.SourceRoot
          PackageMode = workspace.PackageMode
          BuildConfigurationIdentity = workspace.BuildConfigurationIdentity
          BackendProfile = workspace.BackendProfile
          BackendIntrinsicSet = workspace.BackendIntrinsicIdentity
          DeploymentMode = workspace.DeploymentMode }

    let private metadataIdScope (workspace: WorkspaceCompilation) =
        [
            $"compiler={compilerImplementationId}/{compilerImplementationVersion}"
            $"session={workspace.AnalysisSessionIdentity}"
            $"build={workspace.BuildConfigurationIdentity}"
            $"backend={workspace.BackendProfile}"
            $"intrinsics={workspace.BackendIntrinsicIdentity}"
        ]
        |> String.concat ";"

    let private queryId (workspace: WorkspaceCompilation) queryKind inputKey outputCheckpoint =
        $"{QueryKind.toPortableName queryKind}:{metadataIdScope workspace}:input={inputKey}:output={outputCheckpoint}"

    let private queryRecord
        (workspace: WorkspaceCompilation)
        queryKind
        inputKey
        outputCheckpoint
        requiredPhase
        dependencyIds
        =
        { Id = queryId workspace queryKind inputKey outputCheckpoint
          QueryKind = queryKind
          InputKey = inputKey
          OutputCheckpoint = outputCheckpoint
          RequiredPhase = requiredPhase
          AnalysisSessionIdentity = workspace.AnalysisSessionIdentity
          BuildConfigurationIdentity = workspace.BuildConfigurationIdentity
          BackendProfile = workspace.BackendProfile
          BackendIntrinsicSet = workspace.BackendIntrinsicIdentity
          DependencyIds = dependencyIds }

    let queryPlan (workspace: WorkspaceCompilation) =
        let documents =
            workspace.KFrontIR
            |> List.sortBy (fun document -> document.FilePath)

        let documentQueries =
            documents
            |> List.collect (fun document ->
                let inputKey = document.FilePath
                let parse =
                    queryRecord workspace ParseSourceFileQuery inputKey "surface-source" None []

                let raw =
                    queryRecord
                        workspace
                        BuildKFrontIRQuery
                        inputKey
                        (KFrontIRPhase.checkpointName RAW)
                        (Some RAW)
                        [ parse.Id ]

                let phaseQueries =
                    ((raw, []), KFrontIRPhase.all |> List.tail)
                    ||> List.fold (fun (previous, collected) phase ->
                        let checkpoint = KFrontIRPhase.checkpointName phase

                        let current =
                            queryRecord
                                workspace
                                AdvanceKFrontIRPhaseQuery
                                inputKey
                                checkpoint
                                (Some phase)
                                [ previous.Id ]

                        current, collected @ [ current ])
                    |> snd

                let diagnostics =
                    let checkers =
                        phaseQueries
                        |> List.find (fun query -> query.OutputCheckpoint = KFrontIRPhase.checkpointName CHECKERS)

                    queryRecord
                        workspace
                        ComputeDiagnosticsQuery
                        inputKey
                        $"{KFrontIRPhase.checkpointName CHECKERS}.diagnostics"
                        (Some CHECKERS)
                        [ checkers.Id ]

                let core =
                    let coreLowering =
                        phaseQueries
                        |> List.find (fun query -> query.OutputCheckpoint = KFrontIRPhase.checkpointName CORE_LOWERING)

                    queryRecord workspace LowerKCoreQuery inputKey "KCore" (Some CORE_LOWERING) [ coreLowering.Id ]

                let runtime =
                    queryRecord workspace LowerKRuntimeIRQuery inputKey "KRuntimeIR" None [ core.Id ]

                let backend =
                    queryRecord workspace LowerKBackendIRQuery inputKey "KBackendIR" None [ runtime.Id ]

                [ parse; raw ] @ phaseQueries @ [ diagnostics; core; runtime; backend ])

        let backendDependencies =
            documentQueries
            |> List.filter (fun query -> query.QueryKind = LowerKBackendIRQuery)
            |> List.map (fun query -> query.Id)

        let targetQueries =
            targetCheckpointNames workspace
            |> List.map (fun checkpoint ->
                queryRecord
                    workspace
                    LowerTargetQuery
                    workspace.SourceRoot
                    checkpoint
                    None
                    backendDependencies)

        documentQueries @ targetQueries

    let private fingerprintId (workspace: WorkspaceCompilation) fingerprintKind inputKey =
        $"{CompilerFingerprintKind.toPortableName fingerprintKind}:{metadataIdScope workspace}:input={inputKey}"

    let private fingerprintRecord
        (workspace: WorkspaceCompilation)
        fingerprintKind
        inputKey
        identity
        dependencyFingerprintIds
        =
        { Id = fingerprintId workspace fingerprintKind inputKey
          FingerprintKind = fingerprintKind
          InputKey = inputKey
          Identity = identity
          AnalysisSessionIdentity = workspace.AnalysisSessionIdentity
          BuildConfigurationIdentity = workspace.BuildConfigurationIdentity
          BackendProfile = workspace.BackendProfile
          BackendIntrinsicSet = workspace.BackendIntrinsicIdentity
          DependencyFingerprintIds = dependencyFingerprintIds }

    let private declarationInputKey filePath index declaration =
        let kind = declarationKindText declaration
        let name = declarationName declaration |> Option.defaultValue "<anonymous>"
        $"{filePath}#declaration:{index}:{kind}:{name}"

    let private parameterHeaderIdentity (parameter: Parameter) =
        match parameter.TypeTokens with
        | Some typeTokens -> $"{parameter.Name}:{tokensText typeTokens}"
        | None -> parameter.Name

    let private declarationHeaderIdentity declaration =
        let name = declarationName declaration |> Option.defaultValue "<anonymous>"
        let visibility = declarationVisibility declaration |> Option.defaultValue "<default>"
        let typeText = declarationTypeText declaration |> Option.defaultValue "<none>"

        let commonPrefix =
            [
                $"kind={declarationKindText declaration}"
                $"name={name}"
                $"visibility={visibility}"
                $"opaque={declarationIsOpaque declaration}"
                $"type={typeText}"
            ]
            |> String.concat ";"

        let shape =
            match declaration with
            | ImportDeclaration (isExport, specs) ->
                let keyword = if isExport then "export" else "import"
                let specsText = specs |> List.map importSpecText |> String.concat ","
                $"keyword={keyword};specs=[{specsText}]"
            | FixityDeclarationNode fixity ->
                $"precedence={fixity.Precedence};operator={fixity.OperatorName}"
            | ExpectDeclarationNode (ExpectTypeDeclaration expected) ->
                $"expect=type;header={tokensText expected.HeaderTokens}"
            | ExpectDeclarationNode (ExpectTraitDeclaration expected) ->
                $"expect=trait;header={tokensText expected.HeaderTokens}"
            | ExpectDeclarationNode (ExpectTermDeclaration expected) ->
                $"expect=term;type={tokensText expected.TypeTokens}"
            | SignatureDeclaration signature ->
                $"signatureType={tokensText signature.TypeTokens}"
            | EffectDeclarationNode declaration ->
                let operations =
                    declaration.Operations
                    |> List.map (fun operation ->
                        let quantityText =
                            operation.ResumptionQuantity
                            |> Option.map Quantity.toSurfaceText
                            |> Option.map (fun quantity -> quantity + " ")
                            |> Option.defaultValue ""

                        $"{quantityText}{operation.Name}:{tokensText operation.SignatureTokens}")
                    |> String.concat ","

                $"header={tokensText declaration.HeaderTokens};ops=[{operations}]"
            | LetDeclaration definition ->
                let parameters =
                    definition.Parameters
                    |> List.map parameterHeaderIdentity
                    |> String.concat ","

                let returnType =
                    definition.ReturnTypeTokens
                    |> Option.map tokensText
                    |> Option.defaultValue "<inferred>"

                $"parameters=[{parameters}];header={tokensText definition.HeaderTokens};return={returnType}"
            | ProjectionDeclarationNode declaration ->
                let binders =
                    declaration.Binders
                    |> List.map (function
                        | ProjectionPlaceBinder binder ->
                            let receiverMarker = if binder.IsReceiver then "receiver:" else ""
                            $"{receiverMarker}{binder.Name}:{tokensText binder.TypeTokens}"
                        | ProjectionValueBinder binder ->
                            let quantity =
                                binder.Quantity
                                |> Option.map Quantity.toSurfaceText
                                |> Option.defaultValue "<default>"

                            let typeText = binder.TypeTokens |> Option.map tokensText |> Option.defaultValue "<inferred>"
                            $"{binder.Name}:{quantity}:{typeText}")
                    |> String.concat ","

                $"binders=[{binders}];return={tokensText declaration.ReturnTypeTokens}"
            | DataDeclarationNode dataDeclaration ->
                let constructors =
                    dataDeclaration.Constructors
                    |> List.map (fun constructor ->
                        $"{constructor.Name}/{constructor.Arity}:{tokensText constructor.Tokens}")
                    |> String.concat ","

                $"header={tokensText dataDeclaration.HeaderTokens};constructors=[{constructors}]"
            | TypeAliasNode alias ->
                $"header={tokensText alias.HeaderTokens}"
            | TraitDeclarationNode traitDeclaration ->
                let members =
                    traitDeclaration.Members
                    |> List.map (fun memberDeclaration ->
                        let memberName = memberDeclaration.Name |> Option.defaultValue "<anonymous>"
                        $"{memberName}:{tokensText memberDeclaration.Tokens}")
                    |> String.concat ","

                $"header={tokensText traitDeclaration.HeaderTokens};members=[{members}]"
            | InstanceDeclarationNode instanceDeclaration ->
                let members =
                    instanceDeclaration.Members
                    |> List.map (fun definition ->
                        let memberName = definition.Name |> Option.defaultValue "<anonymous>"
                        $"{memberName}:{tokensText definition.HeaderTokens}")
                    |> String.concat ","

                $"trait={instanceDeclaration.TraitName};header={tokensText instanceDeclaration.HeaderTokens};members=[{members}]"
            | UnknownDeclaration tokens ->
                $"tokens={tokensText tokens}"

        $"header:{commonPrefix};{shape}"

    let private declarationBodyIdentity declaration =
        let body =
            match declaration with
            | LetDeclaration definition ->
                declarationBodyText declaration
                |> Option.defaultValue (tokensText definition.BodyTokens)
            | TypeAliasNode alias ->
                alias.BodyTokens
                |> Option.map tokensText
                |> Option.defaultValue "<none>"
            | InstanceDeclarationNode instanceDeclaration ->
                instanceDeclaration.Members
                |> List.map (fun definition ->
                    let memberName = definition.Name |> Option.defaultValue "<anonymous>"

                    let bodyText =
                        definition.Body
                        |> Option.map expressionText
                        |> Option.defaultValue (tokensText definition.BodyTokens)

                    $"{memberName}={bodyText}")
                |> String.concat ";"
            | _ ->
                "<none>"

        let name = declarationName declaration |> Option.defaultValue "<anonymous>"
        $"body:kind={declarationKindText declaration};name={name};body={body}"

    let private moduleInterfaceIdentity (frontendModule: KFrontIRModule) =
        let imports =
            frontendModule.Imports
            |> List.map importSpecText
            |> String.concat ","

        let declarations =
            frontendModule.Declarations
            |> List.map declarationHeaderIdentity
            |> String.concat "|"

        $"interface:module={moduleNameText frontendModule.ModuleIdentity};imports=[{imports}];declarations=[{declarations}]"

    let private backendFingerprintIdentity (workspace: WorkspaceCompilation) (backendModule: KBackendModule) =
        let imports =
            backendModule.Imports
            |> List.map importSpecText
            |> String.concat ","

        let layouts =
            backendModule.DataLayouts
            |> List.map (fun layout ->
                let constructors =
                    layout.Constructors
                    |> List.map (fun constructor ->
                        let fieldRepresentations =
                            constructor.FieldRepresentations
                            |> List.map (fun representation -> $"{representation}")
                            |> String.concat ","

                        $"{constructor.Name}/{constructor.Tag}({fieldRepresentations})")
                    |> String.concat ","

                $"{layout.TypeName}:{layout.RepresentationClass}:{layout.TagEncoding}[{constructors}]")
            |> String.concat "|"

        let functions =
            backendModule.Functions
            |> List.map (fun fn ->
                let parameters =
                    fn.Parameters
                    |> List.map (fun parameter -> $"{parameter.Name}:{parameter.Representation}")
                    |> String.concat ","

                let returnRepresentation =
                    fn.ReturnRepresentation
                    |> Option.map (fun representation -> $"{representation}")
                    |> Option.defaultValue "<unit>"

                $"{fn.Name}({parameters}):{returnRepresentation}:intrinsic={fn.Intrinsic}:exported={fn.Exported}:entry={fn.EntryPoint}")
            |> String.concat "|"

        let entryPoints = String.concat "," backendModule.EntryPoints

        [
            $"backend:compiler={compilerImplementationId}/{compilerImplementationVersion}"
            $"profile={workspace.BackendProfile}"
            $"build={workspace.BuildConfigurationIdentity}"
            $"intrinsics={workspace.BackendIntrinsicIdentity}"
            $"module={backendModule.Name}"
            $"imports=[{imports}]"
            $"entryPoints=[{entryPoints}]"
            $"layouts=[{layouts}]"
            $"functions=[{functions}]"
        ]
        |> String.concat ";"

    let compilerFingerprints (workspace: WorkspaceCompilation) =
        let sourceFingerprints =
            workspace.Documents
            |> List.sortBy (fun document -> document.Source.FilePath)
            |> List.map (fun document ->
                let inputKey = document.Source.FilePath
                let normalizedPath = Path.GetFullPath(document.Source.FilePath)
                let contentHash = sha256Hex document.Source.Content

                let identity =
                    [
                        $"source:path={normalizedPath}"
                        $"contentSha256={contentHash}"
                        $"length={document.Source.Length}"
                        $"lines={document.Source.LineCount}"
                        $"module={moduleNameText document.ModuleName}"
                    ]
                    |> String.concat ";"

                fingerprintRecord workspace SourceFingerprint inputKey identity [])

        let sourceFingerprintByFile =
            sourceFingerprints
            |> List.map (fun fingerprint -> fingerprint.InputKey, fingerprint)
            |> Map.ofList

        let declarationFingerprints =
            workspace.KFrontIR
            |> List.sortBy (fun frontendModule -> frontendModule.FilePath)
            |> List.collect (fun frontendModule ->
                frontendModule.Declarations
                |> List.mapi (fun index declaration ->
                    let inputKey = declarationInputKey frontendModule.FilePath index declaration
                    let sourceDependency = sourceFingerprintByFile[frontendModule.FilePath].Id

                    let header =
                        fingerprintRecord
                            workspace
                            HeaderFingerprint
                            inputKey
                            (declarationHeaderIdentity declaration)
                            [ sourceDependency ]

                    let body =
                        fingerprintRecord
                            workspace
                            BodyFingerprint
                            inputKey
                            (declarationBodyIdentity declaration)
                            [ header.Id ]

                    [ header; body ])
                |> List.concat)

        let declarationFingerprintIdsByFile =
            declarationFingerprints
            |> List.groupBy (fun fingerprint ->
                let marker = "#declaration:"
                let markerIndex = fingerprint.InputKey.IndexOf(marker, StringComparison.Ordinal)

                if markerIndex < 0 then
                    fingerprint.InputKey
                else
                    fingerprint.InputKey.Substring(0, markerIndex))
            |> Map.ofList

        let interfaceFingerprints =
            workspace.KFrontIR
            |> List.sortBy (fun frontendModule -> frontendModule.FilePath)
            |> List.map (fun frontendModule ->
                let sourceDependency = sourceFingerprintByFile[frontendModule.FilePath].Id

                let declarationDependencies =
                    declarationFingerprintIdsByFile
                    |> Map.tryFind frontendModule.FilePath
                    |> Option.defaultValue []
                    |> List.filter (fun fingerprint -> fingerprint.FingerprintKind = HeaderFingerprint)
                    |> List.map (fun fingerprint -> fingerprint.Id)

                fingerprintRecord
                    workspace
                    InterfaceFingerprint
                    frontendModule.FilePath
                    (moduleInterfaceIdentity frontendModule)
                    (sourceDependency :: declarationDependencies))

        let interfaceFingerprintByFile =
            interfaceFingerprints
            |> List.map (fun fingerprint -> fingerprint.InputKey, fingerprint)
            |> Map.ofList

        let bodyFingerprintIdsByFile =
            declarationFingerprints
            |> List.filter (fun fingerprint -> fingerprint.FingerprintKind = BodyFingerprint)
            |> List.groupBy (fun fingerprint ->
                let marker = "#declaration:"
                let markerIndex = fingerprint.InputKey.IndexOf(marker, StringComparison.Ordinal)

                if markerIndex < 0 then
                    fingerprint.InputKey
                else
                    fingerprint.InputKey.Substring(0, markerIndex))
            |> Map.ofList

        let backendFingerprints =
            workspace.KBackendIR
            |> List.filter (fun backendModule -> interfaceFingerprintByFile.ContainsKey(backendModule.SourceFile))
            |> List.sortBy (fun backendModule -> backendModule.SourceFile)
            |> List.map (fun backendModule ->
                let interfaceDependency = interfaceFingerprintByFile[backendModule.SourceFile].Id

                let bodyDependencies =
                    bodyFingerprintIdsByFile
                    |> Map.tryFind backendModule.SourceFile
                    |> Option.defaultValue []
                    |> List.map (fun fingerprint -> fingerprint.Id)

                fingerprintRecord
                    workspace
                    BackendFingerprint
                    backendModule.SourceFile
                    (backendFingerprintIdentity workspace backendModule)
                    (interfaceDependency :: bodyDependencies))

        sourceFingerprints @ declarationFingerprints @ interfaceFingerprints @ backendFingerprints

    let private unitId (workspace: WorkspaceCompilation) unitKind inputKey =
        $"{IncrementalUnitKind.toPortableName unitKind}:{metadataIdScope workspace}:input={inputKey}"

    let private incrementalUnit
        (workspace: WorkspaceCompilation)
        unitKind
        inputKey
        outputCheckpoint
        fingerprintIds
        dependencyUnitIds
        =
        { Id = unitId workspace unitKind inputKey
          UnitKind = unitKind
          InputKey = inputKey
          OutputCheckpoint = outputCheckpoint
          FingerprintIds = fingerprintIds
          DependencyUnitIds = dependencyUnitIds
          AnalysisSessionIdentity = workspace.AnalysisSessionIdentity
          BuildConfigurationIdentity = workspace.BuildConfigurationIdentity
          BackendProfile = workspace.BackendProfile
          BackendIntrinsicSet = workspace.BackendIntrinsicIdentity }

    let incrementalUnits (workspace: WorkspaceCompilation) =
        let fingerprints = compilerFingerprints workspace

        let fingerprintById =
            fingerprints
            |> List.map (fun fingerprint -> fingerprint.Id, fingerprint)
            |> Map.ofList

        let fingerprintIdFor fingerprintKind inputKey =
            fingerprintId workspace fingerprintKind inputKey

        let sourceUnitByFile =
            workspace.Documents
            |> List.sortBy (fun document -> document.Source.FilePath)
            |> List.map (fun document ->
                let inputKey = document.Source.FilePath

                let unit =
                    incrementalUnit
                        workspace
                        SourceFileTextUnit
                        inputKey
                        (Some "surface-source")
                        [ fingerprintIdFor SourceFingerprint inputKey ]
                        []

                inputKey, unit)
            |> Map.ofList

        let importSurfaceUnits =
            workspace.KFrontIR
            |> List.sortBy (fun frontendModule -> frontendModule.FilePath)
            |> List.map (fun frontendModule ->
                incrementalUnit
                    workspace
                    ModuleImportSurfaceUnit
                    frontendModule.FilePath
                    (Some (KFrontIRPhase.checkpointName IMPORTS))
                    [ fingerprintIdFor SourceFingerprint frontendModule.FilePath ]
                    [ sourceUnitByFile[frontendModule.FilePath].Id ])

        let importSurfaceUnitByFile =
            importSurfaceUnits
            |> List.map (fun unit -> unit.InputKey, unit)
            |> Map.ofList

        let declarationUnits =
            workspace.KFrontIR
            |> List.sortBy (fun frontendModule -> frontendModule.FilePath)
            |> List.collect (fun frontendModule ->
                frontendModule.Declarations
                |> List.mapi (fun index declaration ->
                    let inputKey = declarationInputKey frontendModule.FilePath index declaration

                    let headerUnit =
                        incrementalUnit
                            workspace
                            DeclarationHeaderUnit
                            inputKey
                            (Some (KFrontIRPhase.checkpointName IMPLICIT_SIGNATURES))
                            [ fingerprintIdFor HeaderFingerprint inputKey ]
                            [ importSurfaceUnitByFile[frontendModule.FilePath].Id ]

                    let macroExpansionUnit =
                        incrementalUnit
                            workspace
                            MacroExpansionUnit
                            inputKey
                            (Some "macro-expansion")
                            []
                            [ headerUnit.Id ]

                    let bodyUnit =
                        incrementalUnit
                            workspace
                            DeclarationBodyUnit
                            inputKey
                            (Some (KFrontIRPhase.checkpointName BODY_RESOLVE))
                            [ fingerprintIdFor BodyFingerprint inputKey ]
                            [ macroExpansionUnit.Id ]

                    [ headerUnit; macroExpansionUnit; bodyUnit ])
                |> List.concat)

        let unitsByKindAndFile unitKind filePath =
            declarationUnits
            |> List.filter (fun unit ->
                unit.UnitKind = unitKind
                && unit.InputKey.StartsWith(filePath + "#declaration:", StringComparison.Ordinal))

        let moduleInterfaceUnits =
            workspace.KFrontIR
            |> List.sortBy (fun frontendModule -> frontendModule.FilePath)
            |> List.map (fun frontendModule ->
                let headerDependencies =
                    unitsByKindAndFile DeclarationHeaderUnit frontendModule.FilePath
                    |> List.map (fun unit -> unit.Id)

                incrementalUnit
                    workspace
                    ModuleInterfaceUnit
                    frontendModule.FilePath
                    (Some "module-interface")
                    [ fingerprintIdFor InterfaceFingerprint frontendModule.FilePath ]
                    (importSurfaceUnitByFile[frontendModule.FilePath].Id :: headerDependencies))

        let moduleInterfaceUnitByFile =
            moduleInterfaceUnits
            |> List.map (fun unit -> unit.InputKey, unit)
            |> Map.ofList

        let kCoreUnits =
            workspace.KCore
            |> List.sortBy (fun coreModule -> coreModule.SourceFile)
            |> List.map (fun coreModule ->
                let bodyDependencies =
                    unitsByKindAndFile DeclarationBodyUnit coreModule.SourceFile
                    |> List.map (fun unit -> unit.Id)

                incrementalUnit
                    workspace
                    KCoreModuleUnit
                    coreModule.SourceFile
                    (Some "KCore")
                    []
                    (moduleInterfaceUnitByFile[coreModule.SourceFile].Id :: bodyDependencies))

        let kCoreUnitByFile =
            kCoreUnits
            |> List.map (fun unit -> unit.InputKey, unit)
            |> Map.ofList

        let kBackendUnits =
            workspace.KBackendIR
            |> List.filter (fun backendModule -> kCoreUnitByFile.ContainsKey(backendModule.SourceFile))
            |> List.sortBy (fun backendModule -> backendModule.SourceFile)
            |> List.map (fun backendModule ->
                incrementalUnit
                    workspace
                    KBackendIRModuleUnit
                    backendModule.SourceFile
                    (Some "KBackendIR")
                    [ fingerprintIdFor BackendFingerprint backendModule.SourceFile ]
                    [ kCoreUnitByFile[backendModule.SourceFile].Id ])

        let kBackendUnitIds =
            kBackendUnits
            |> List.map (fun unit -> unit.Id)

        let backendFingerprintIds =
            fingerprints
            |> List.filter (fun fingerprint -> fingerprint.FingerprintKind = BackendFingerprint)
            |> List.map (fun fingerprint -> fingerprint.Id)

        let targetUnits =
            targetCheckpointNames workspace
            |> List.map (fun checkpoint ->
                let inputKey = $"{workspace.SourceRoot}#target:{checkpoint}"

                incrementalUnit
                    workspace
                    TargetLoweringUnit
                    inputKey
                    (Some checkpoint)
                    backendFingerprintIds
                    kBackendUnitIds)

        let units =
            sourceUnitByFile
            |> Map.toList
            |> List.map snd

        let referencedFingerprintIds =
            units
            @ importSurfaceUnits
            @ declarationUnits
            @ moduleInterfaceUnits
            @ kCoreUnits
            @ kBackendUnits
            @ targetUnits
            |> List.collect (fun unit -> unit.FingerprintIds)

        for fingerprintId in referencedFingerprintIds do
            if not (fingerprintById.ContainsKey fingerprintId) then
                invalidOp $"Internal error: incremental unit references missing compiler fingerprint '{fingerprintId}'."

        units
        @ importSurfaceUnits
        @ declarationUnits
        @ moduleInterfaceUnits
        @ kCoreUnits
        @ kBackendUnits
        @ targetUnits
