namespace Kappa.Compiler

open System
open System.IO
open System.Security.Cryptography
open System.Text
open CompilationCommon
open CompilationFrontend

// Computes analysis-session, query-sketch, fingerprint, incremental-unit, and runtime-obligation metadata.
module internal CompilationMetadata =
    let private sha256Hex (text: string) =
        text
        |> Encoding.UTF8.GetBytes
        |> SHA256.HashData
        |> Convert.ToHexString
        |> fun hex -> hex.ToLowerInvariant()

    let private targetCheckpointNames (workspace: WorkspaceCompilation) =
        Stdlib.targetCheckpointNamesForBackend workspace.Backend

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
          DependencyModel = ObservabilitySketchDependencyModel
          RequiredPhase = requiredPhase
          AnalysisSessionIdentity = workspace.AnalysisSessionIdentity
          BuildConfigurationIdentity = workspace.BuildConfigurationIdentity
          BackendProfile = workspace.BackendProfile
          BackendIntrinsicSet = workspace.BackendIntrinsicIdentity
          DependencyIds = dependencyIds }

    let private importedFrontendFilesByFile (workspace: WorkspaceCompilation) =
        let moduleFileByIdentity =
            workspace.KFrontIR
            |> List.choose (fun frontendModule ->
                frontendModule.ModuleIdentity
                |> Option.map (fun moduleIdentity -> SyntaxFacts.moduleNameToText moduleIdentity, frontendModule.FilePath))
            |> List.groupBy fst
            |> List.choose (fun (moduleName, entries) ->
                match entries with
                | [ _, filePath ] -> Some(moduleName, filePath)
                | _ -> None)
            |> Map.ofList

        workspace.KFrontIR
        |> List.map (fun frontendModule ->
            let importedFiles =
                frontendModule.Imports
                |> List.choose (fun importSpec ->
                    match importSpec.Source with
                    | Dotted nameSegments ->
                        moduleFileByIdentity
                        |> Map.tryFind (SyntaxFacts.moduleNameToText nameSegments)
                    | Url _ ->
                        None)
                |> List.filter (fun importedFilePath ->
                    not (String.Equals(importedFilePath, frontendModule.FilePath, StringComparison.Ordinal)))
                |> List.distinct
                |> List.sort

            frontendModule.FilePath, importedFiles)
        |> Map.ofList

    let private isGeneratedStandardRuntimeSourceFile (sourceFile: string) =
        sourceFile.StartsWith("<std:", StringComparison.Ordinal)

    let private isGeneratedHostBindingSourceFile (sourceFile: string) =
        sourceFile.StartsWith("<host:", StringComparison.Ordinal)

    let private generatedStandardBackendModules (workspace: WorkspaceCompilation) =
        workspace.KBackendIR
        |> List.filter (fun backendModule -> isGeneratedStandardRuntimeSourceFile backendModule.SourceFile)
        |> List.sortBy (fun backendModule -> backendModule.SourceFile)

    let private generatedHostBindingBackendModules (workspace: WorkspaceCompilation) =
        workspace.KBackendIR
        |> List.filter (fun backendModule -> isGeneratedHostBindingSourceFile backendModule.SourceFile)
        |> List.sortBy (fun backendModule -> backendModule.SourceFile)

    let private runtimeIntrinsicSetInputKey (workspace: WorkspaceCompilation) =
        $"{workspace.SourceRoot}#runtime-intrinsic-set"

    let private backendRuntimeSupportInputKey (workspace: WorkspaceCompilation) =
        $"{workspace.SourceRoot}#backend-runtime-support"

    let private runtimeIntrinsicSetIdentity (workspace: WorkspaceCompilation) =
        let intrinsicSet = Stdlib.intrinsicSetForCompilationProfile workspace.Backend workspace.AllowUnsafeConsume

        let setText (values: Set<string>) =
            values |> Set.toList |> List.sort |> String.concat ","

        [
            $"identity={intrinsicSet.Identity}"
            $"types=[{setText intrinsicSet.TypeNames}]"
            $"traits=[{setText intrinsicSet.TraitNames}]"
            $"preludeTerms=[{setText intrinsicSet.PreludeTermNames}]"
            $"moduleLocalTerms=[{setText intrinsicSet.ModuleLocalTermNames}]"
            $"runtimeTerms=[{setText intrinsicSet.RuntimeTermNames}]"
            $"elaborationTerms=[{setText intrinsicSet.ElaborationAvailableTermNames}]"
        ]
        |> String.concat ";"

    let private backendRuntimeSupportIdentity (workspace: WorkspaceCompilation) =
        let ownerText owner =
            match owner with
            | KBackendIRGuaranteed -> "KBackendIRGuaranteed"
            | BackendSpecificRuntime -> "BackendSpecificRuntime"
            | DeferredRuntimeObligation -> "DeferredRuntimeObligation"

        let runtimeObligations =
            portableRuntimeObligations workspace
            |> List.map (fun obligation -> $"{obligation.Name}:{ownerText obligation.Owner}")
            |> List.sort
            |> String.concat ","

        let targetCheckpoints =
            targetCheckpointNames workspace |> List.sort |> String.concat ","

        [
            $"backend={workspace.BackendProfile}"
            $"deployment={workspace.DeploymentMode}"
            $"targets=[{targetCheckpoints}]"
            $"runtimeObligations=[{runtimeObligations}]"
        ]
        |> String.concat ";"

    let querySketch (workspace: WorkspaceCompilation) =
        let documents =
            workspace.KFrontIR
            |> List.sortBy (fun document -> document.FilePath)

        let importedFilesByFile = importedFrontendFilesByFile workspace
        let runtimeIntrinsicSetQuery =
            queryRecord
                workspace
                ResolveRuntimeIntrinsicSetQuery
                (runtimeIntrinsicSetInputKey workspace)
                "runtime-intrinsic-set"
                None
                []

        let backendRuntimeSupportQuery =
            queryRecord
                workspace
                ResolveBackendRuntimeSupportQuery
                (backendRuntimeSupportInputKey workspace)
                "backend-runtime-support"
                None
                [ runtimeIntrinsicSetQuery.Id ]

        let standardRuntimeModuleQueries =
            generatedStandardBackendModules workspace
            |> List.map (fun backendModule ->
                queryRecord
                    workspace
                    MaterializeGeneratedStandardModuleQuery
                    backendModule.SourceFile
                    $"generated-standard-runtime-module:{backendModule.Name}"
                    None
                    [ runtimeIntrinsicSetQuery.Id; backendRuntimeSupportQuery.Id ])

        let hostBindingModuleQueries =
            generatedHostBindingBackendModules workspace
            |> List.map (fun backendModule ->
                queryRecord
                    workspace
                    MaterializeGeneratedHostBindingModuleQuery
                    backendModule.SourceFile
                    $"generated-host-binding-module:{backendModule.Name}"
                    None
                    [ runtimeIntrinsicSetQuery.Id; backendRuntimeSupportQuery.Id ])

        let importedInterfaceQueryIds inputKey =
            importedFilesByFile
            |> Map.tryFind inputKey
            |> Option.defaultValue []
            |> List.map (fun importedFile -> queryId workspace EmitInterfaceQuery importedFile "module-interface")

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
                        let dependencyIds =
                            let priorDependencies = [ previous.Id ]

                            if phase = IMPORTS then
                                priorDependencies @ importedInterfaceQueryIds inputKey
                            else
                                priorDependencies

                        let current =
                            queryRecord
                                workspace
                                AdvanceKFrontIRPhaseQuery
                                inputKey
                                checkpoint
                                (Some phase)
                                dependencyIds

                        current, collected @ [ current ])
                    |> snd

                let implicitSignatures =
                    phaseQueries
                    |> List.find (fun query -> query.OutputCheckpoint = KFrontIRPhase.checkpointName IMPLICIT_SIGNATURES)

                let interfaceQuery =
                    queryRecord
                        workspace
                        EmitInterfaceQuery
                        inputKey
                        "module-interface"
                        (Some IMPLICIT_SIGNATURES)
                        (implicitSignatures.Id :: importedInterfaceQueryIds inputKey)

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

                    queryRecord
                        workspace
                        LowerKCoreQuery
                        inputKey
                        "KCore"
                        (Some CORE_LOWERING)
                        ([ coreLowering.Id; interfaceQuery.Id ] @ importedInterfaceQueryIds inputKey)

                let runtime =
                    queryRecord workspace LowerKRuntimeIRQuery inputKey "KRuntimeIR" None [ core.Id ]

                let backend =
                    queryRecord
                        workspace
                        LowerKBackendIRQuery
                        inputKey
                        "KBackendIR"
                        None
                        [ runtime.Id; runtimeIntrinsicSetQuery.Id; backendRuntimeSupportQuery.Id ]

                [ parse; raw ] @ phaseQueries @ [ interfaceQuery; diagnostics; core; runtime; backend ])

        let backendDependencies =
            documentQueries
            |> List.filter (fun query -> query.QueryKind = LowerKBackendIRQuery)
            |> List.map (fun query -> query.Id)

        let generatedRuntimeDependencies =
            standardRuntimeModuleQueries @ hostBindingModuleQueries
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
                    (backendDependencies
                     @ generatedRuntimeDependencies
                     @ [ runtimeIntrinsicSetQuery.Id; backendRuntimeSupportQuery.Id ]))

        [ runtimeIntrinsicSetQuery; backendRuntimeSupportQuery ]
        @ standardRuntimeModuleQueries
        @ hostBindingModuleQueries
        @ documentQueries
        @ targetQueries

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

    let private declarationInputKeyBase filePath declaration =
        let kind = declarationKindText declaration
        let name = declarationName declaration |> Option.defaultValue "<anonymous>"
        let spellingHash = sha256Hex $"{declarationHeaderIdentity declaration}|{declarationBodyIdentity declaration}"
        $"{filePath}#declaration:{kind}:{name}:spelling={spellingHash}"

    let private declarationInputKeys filePath declarations =
        let occurrenceCounts = System.Collections.Generic.Dictionary<string, int>(StringComparer.Ordinal)

        declarations
        |> List.map (fun declaration ->
            let baseKey = declarationInputKeyBase filePath declaration

            let occurrence =
                match occurrenceCounts.TryGetValue(baseKey) with
                | true, count ->
                    occurrenceCounts[baseKey] <- count + 1
                    count + 1
                | false, _ ->
                    occurrenceCounts[baseKey] <- 1
                    1

            if occurrence = 1 then
                declaration, baseKey
            else
                declaration, $"{baseKey}:peer={occurrence}")

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
        let importedFilesByFile = importedFrontendFilesByFile workspace
        let runtimeIntrinsicSetFingerprintId = fingerprintId workspace RuntimeIntrinsicSetFingerprint (runtimeIntrinsicSetInputKey workspace)
        let backendRuntimeSupportFingerprintId =
            fingerprintId workspace BackendRuntimeSupportFingerprint (backendRuntimeSupportInputKey workspace)

        let importedInterfaceFingerprintIds filePath =
            importedFilesByFile
            |> Map.tryFind filePath
            |> Option.defaultValue []
            |> List.map (fun importedFilePath -> fingerprintId workspace InterfaceFingerprint importedFilePath)

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

        let runtimeIntrinsicSetFingerprint =
            fingerprintRecord
                workspace
                RuntimeIntrinsicSetFingerprint
                (runtimeIntrinsicSetInputKey workspace)
                (runtimeIntrinsicSetIdentity workspace)
                []

        let backendRuntimeSupportFingerprint =
            fingerprintRecord
                workspace
                BackendRuntimeSupportFingerprint
                (backendRuntimeSupportInputKey workspace)
                (backendRuntimeSupportIdentity workspace)
                [ runtimeIntrinsicSetFingerprint.Id ]

        let declarationFingerprints =
            workspace.KFrontIR
            |> List.sortBy (fun frontendModule -> frontendModule.FilePath)
            |> List.collect (fun frontendModule ->
                declarationInputKeys frontendModule.FilePath frontendModule.Declarations
                |> List.map (fun (declaration, inputKey) ->
                    let sourceDependency = sourceFingerprintByFile[frontendModule.FilePath].Id
                    let importDependencies = importedInterfaceFingerprintIds frontendModule.FilePath

                    let header =
                        fingerprintRecord
                            workspace
                            HeaderFingerprint
                            inputKey
                            (declarationHeaderIdentity declaration)
                            (sourceDependency :: importDependencies)

                    let body =
                        fingerprintRecord
                            workspace
                            BodyFingerprint
                            inputKey
                            (declarationBodyIdentity declaration)
                            (header.Id :: importDependencies)

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
                let importDependencies = importedInterfaceFingerprintIds frontendModule.FilePath

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
                    (sourceDependency :: declarationDependencies @ importDependencies))

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
                let importDependencies = importedInterfaceFingerprintIds backendModule.SourceFile

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
                    (interfaceDependency
                     :: bodyDependencies
                     @ importDependencies
                     @ [ runtimeIntrinsicSetFingerprintId; backendRuntimeSupportFingerprintId ]))

        let standardRuntimeModuleFingerprints =
            generatedStandardBackendModules workspace
            |> List.map (fun backendModule ->
                fingerprintRecord
                    workspace
                    GeneratedStandardRuntimeModuleFingerprint
                    backendModule.SourceFile
                    (backendFingerprintIdentity workspace backendModule)
                    [ runtimeIntrinsicSetFingerprintId; backendRuntimeSupportFingerprintId ])

        let hostBindingModuleFingerprints =
            generatedHostBindingBackendModules workspace
            |> List.map (fun backendModule ->
                fingerprintRecord
                    workspace
                    GeneratedHostBindingModuleFingerprint
                    backendModule.SourceFile
                    (backendFingerprintIdentity workspace backendModule)
                    [ runtimeIntrinsicSetFingerprintId; backendRuntimeSupportFingerprintId ])

        sourceFingerprints
        @ [ runtimeIntrinsicSetFingerprint; backendRuntimeSupportFingerprint ]
        @ declarationFingerprints
        @ interfaceFingerprints
        @ backendFingerprints
        @ standardRuntimeModuleFingerprints
        @ hostBindingModuleFingerprints

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
        let importedFilesByFile = importedFrontendFilesByFile workspace
        let fingerprints = compilerFingerprints workspace
        let runtimeIntrinsicSetFingerprintId =
            fingerprintId workspace RuntimeIntrinsicSetFingerprint (runtimeIntrinsicSetInputKey workspace)

        let backendRuntimeSupportFingerprintId =
            fingerprintId workspace BackendRuntimeSupportFingerprint (backendRuntimeSupportInputKey workspace)

        let fingerprintById =
            fingerprints
            |> List.map (fun fingerprint -> fingerprint.Id, fingerprint)
            |> Map.ofList

        let fingerprintIdFor fingerprintKind inputKey =
            fingerprintId workspace fingerprintKind inputKey

        let runtimeIntrinsicSetUnitId =
            unitId workspace RuntimeIntrinsicSetUnit (runtimeIntrinsicSetInputKey workspace)

        let backendRuntimeSupportUnitId =
            unitId workspace BackendRuntimeSupportUnit (backendRuntimeSupportInputKey workspace)

        let importedInterfaceFingerprintIds filePath =
            importedFilesByFile
            |> Map.tryFind filePath
            |> Option.defaultValue []
            |> List.map (fun importedFilePath -> fingerprintIdFor InterfaceFingerprint importedFilePath)

        let importedInterfaceUnitIds filePath =
            importedFilesByFile
            |> Map.tryFind filePath
            |> Option.defaultValue []
            |> List.map (fun importedFilePath -> unitId workspace ModuleInterfaceUnit importedFilePath)

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

        let runtimeIntrinsicSetUnit =
            incrementalUnit
                workspace
                RuntimeIntrinsicSetUnit
                (runtimeIntrinsicSetInputKey workspace)
                (Some "runtime-intrinsic-set")
                [ runtimeIntrinsicSetFingerprintId ]
                []

        let backendRuntimeSupportUnit =
            incrementalUnit
                workspace
                BackendRuntimeSupportUnit
                (backendRuntimeSupportInputKey workspace)
                (Some "backend-runtime-support")
                [ backendRuntimeSupportFingerprintId ]
                [ runtimeIntrinsicSetUnitId ]

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
                    ([ sourceUnitByFile[frontendModule.FilePath].Id ] @ importedInterfaceUnitIds frontendModule.FilePath
                     |> List.distinct))

        let importSurfaceUnitByFile =
            importSurfaceUnits
            |> List.map (fun unit -> unit.InputKey, unit)
            |> Map.ofList

        let declarationUnits =
            workspace.KFrontIR
            |> List.sortBy (fun frontendModule -> frontendModule.FilePath)
            |> List.collect (fun frontendModule ->
                declarationInputKeys frontendModule.FilePath frontendModule.Declarations
                |> List.map (fun (_declaration, inputKey) ->
                    let importFingerprintIds = importedInterfaceFingerprintIds frontendModule.FilePath

                    let headerUnit =
                        incrementalUnit
                            workspace
                            DeclarationHeaderUnit
                            inputKey
                            (Some (KFrontIRPhase.checkpointName IMPLICIT_SIGNATURES))
                            ([ fingerprintIdFor HeaderFingerprint inputKey ] @ importFingerprintIds |> List.distinct)
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
                            ([ fingerprintIdFor BodyFingerprint inputKey ] @ importFingerprintIds |> List.distinct)
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

                let importFingerprintIds = importedInterfaceFingerprintIds frontendModule.FilePath
                let importUnitIds = importedInterfaceUnitIds frontendModule.FilePath

                incrementalUnit
                    workspace
                    ModuleInterfaceUnit
                    frontendModule.FilePath
                    (Some "module-interface")
                    ([ fingerprintIdFor InterfaceFingerprint frontendModule.FilePath ] @ importFingerprintIds
                     |> List.distinct)
                    ([ importSurfaceUnitByFile[frontendModule.FilePath].Id ] @ headerDependencies @ importUnitIds
                     |> List.distinct))

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

                let importFingerprintIds = importedInterfaceFingerprintIds coreModule.SourceFile

                incrementalUnit
                    workspace
                    KCoreModuleUnit
                    coreModule.SourceFile
                    (Some "KCore")
                    importFingerprintIds
                    (moduleInterfaceUnitByFile[coreModule.SourceFile].Id :: bodyDependencies))

        let kCoreUnitByFile =
            kCoreUnits
            |> List.map (fun unit -> unit.InputKey, unit)
            |> Map.ofList

        let standardRuntimeModuleUnits =
            generatedStandardBackendModules workspace
            |> List.map (fun backendModule ->
                incrementalUnit
                    workspace
                    GeneratedStandardRuntimeModuleUnit
                    backendModule.SourceFile
                    (Some $"generated-standard-runtime-module:{backendModule.Name}")
                    [ fingerprintIdFor GeneratedStandardRuntimeModuleFingerprint backendModule.SourceFile ]
                    [ runtimeIntrinsicSetUnitId; backendRuntimeSupportUnitId ])

        let hostBindingModuleUnits =
            generatedHostBindingBackendModules workspace
            |> List.map (fun backendModule ->
                incrementalUnit
                    workspace
                    GeneratedHostBindingModuleUnit
                    backendModule.SourceFile
                    (Some $"generated-host-binding-module:{backendModule.Name}")
                    [ fingerprintIdFor GeneratedHostBindingModuleFingerprint backendModule.SourceFile ]
                    [ runtimeIntrinsicSetUnitId; backendRuntimeSupportUnitId ])

        let kBackendUnits =
            workspace.KBackendIR
            |> List.filter (fun backendModule -> kCoreUnitByFile.ContainsKey(backendModule.SourceFile))
            |> List.sortBy (fun backendModule -> backendModule.SourceFile)
            |> List.map (fun backendModule ->
                let importFingerprintIds = importedInterfaceFingerprintIds backendModule.SourceFile

                incrementalUnit
                    workspace
                    KBackendIRModuleUnit
                    backendModule.SourceFile
                    (Some "KBackendIR")
                    ([ fingerprintIdFor BackendFingerprint backendModule.SourceFile
                       runtimeIntrinsicSetFingerprintId
                       backendRuntimeSupportFingerprintId ]
                     @ importFingerprintIds
                     |> List.distinct)
                    [ kCoreUnitByFile[backendModule.SourceFile].Id
                      runtimeIntrinsicSetUnitId
                      backendRuntimeSupportUnitId ])

        let kBackendUnitIds =
            kBackendUnits
            |> List.map (fun unit -> unit.Id)

        let backendFingerprintIds =
            fingerprints
            |> List.filter (fun fingerprint -> fingerprint.FingerprintKind = BackendFingerprint)
            |> List.map (fun fingerprint -> fingerprint.Id)

        let generatedRuntimeFingerprintIds =
            fingerprints
            |> List.filter (fun fingerprint ->
                fingerprint.FingerprintKind = GeneratedStandardRuntimeModuleFingerprint
                || fingerprint.FingerprintKind = GeneratedHostBindingModuleFingerprint)
            |> List.map (fun fingerprint -> fingerprint.Id)

        let generatedRuntimeUnitIds =
            standardRuntimeModuleUnits @ hostBindingModuleUnits
            |> List.map (fun unit -> unit.Id)

        let targetUnits =
            targetCheckpointNames workspace
            |> List.map (fun checkpoint ->
                let inputKey = $"{workspace.SourceRoot}#target:{checkpoint}"

                incrementalUnit
                    workspace
                    TargetLoweringUnit
                    inputKey
                    (Some checkpoint)
                    (backendFingerprintIds
                     @ generatedRuntimeFingerprintIds
                     @ [ runtimeIntrinsicSetFingerprintId; backendRuntimeSupportFingerprintId ])
                    (kBackendUnitIds
                     @ generatedRuntimeUnitIds
                     @ [ runtimeIntrinsicSetUnitId; backendRuntimeSupportUnitId ]))

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
            @ [ runtimeIntrinsicSetUnit; backendRuntimeSupportUnit ]
            @ standardRuntimeModuleUnits
            @ hostBindingModuleUnits
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
        @ [ runtimeIntrinsicSetUnit; backendRuntimeSupportUnit ]
        @ standardRuntimeModuleUnits
        @ hostBindingModuleUnits
        @ kBackendUnits
        @ targetUnits
