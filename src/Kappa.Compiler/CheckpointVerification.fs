namespace Kappa.Compiler

open System

// Checks published checkpoints against the compiler's observable pipeline contract.
module CheckpointVerification =
    let private makeStructuredDiagnostic evidence =
        Diagnostics.errorFact "checkpoint-verification" None None [] (DiagnosticFact.checkpointVerification evidence)

    let private tokenName (token: Token) =
        match token.Kind with
        | Identifier
        | Keyword _ ->
            Some(SyntaxFacts.trimIdentifierQuotes token.Text)
        | Operator ->
            Some token.Text
        | _ ->
            None

    let private relatedLocation role location =
        { Message = role
          Location = location }

    let private fileOriginLocation (documents: ParsedDocument list) filePath =
        documents
        |> List.tryFind (fun document -> String.Equals(document.Source.FilePath, filePath, StringComparison.Ordinal))
        |> Option.map (fun document -> document.Source.GetLocation(TextSpan.FromBounds(0, 0)))

    let private renderedModuleIdentity moduleName =
        Some(ModuleIdentity.ofDottedTextUnchecked moduleName)

    let private provenancePrimaryLocation (documents: ParsedDocument list) (origin: KCoreOrigin) =
        documents
        |> List.tryFind (fun document -> String.Equals(document.Source.FilePath, origin.FilePath, StringComparison.Ordinal))
        |> Option.bind (fun document ->
            origin.DeclarationName
            |> Option.bind (fun declarationName ->
                document.Syntax.Tokens
                |> List.tryPick (fun token ->
                    tokenName token
                    |> Option.filter (fun tokenText -> String.Equals(tokenText, declarationName, StringComparison.Ordinal))
                    |> Option.map (fun _ -> document.Source.GetLocation(token.Span)))))

    let private moduleOriginRelatedLocations (documents: ParsedDocument list) moduleIdentity filePath =
        let moduleName =
            moduleIdentity
            |> Option.map ModuleIdentity.text
            |> Option.defaultValue "<unknown>"

        fileOriginLocation documents filePath
        |> Option.map (fun location -> [ relatedLocation $"module origin: {moduleName}" location ])
        |> Option.defaultValue []

    let private makeStructuredModuleDiagnostic (documents: ParsedDocument list) moduleIdentity filePath evidence =
        let location = fileOriginLocation documents filePath

        { makeStructuredDiagnostic evidence with
            Location = location
            RelatedLocations = moduleOriginRelatedLocations documents moduleIdentity filePath }

    let private makeStructuredOriginDiagnostic (documents: ParsedDocument list) (origin: KCoreOrigin) evidence =
        let location =
            provenancePrimaryLocation documents origin
            |> Option.orElseWith (fun () -> fileOriginLocation documents origin.FilePath)

        { makeStructuredDiagnostic evidence with
            Location = location
            RelatedLocations = moduleOriginRelatedLocations documents origin.ModuleIdentity origin.FilePath }

    let private makeStructuredDuplicateLocationDiagnostic evidence role locations =
        match locations with
        | head :: tail ->
            { makeStructuredDiagnostic evidence with
                Location = Some head
                RelatedLocations = tail |> List.map (relatedLocation role) }
        | [] ->
            makeStructuredDiagnostic evidence

    let private tryParseTypeText text =
        let source = SourceText.From("__checkpoint_type__.kp", text)
        let lexResult = Lexer.tokenize source

        if not (List.isEmpty lexResult.Diagnostics) then
            None
        else
            TypeSignatures.parseType lexResult.Tokens

    let availableCheckpointNames =
        [
            "surface-source"
            yield! KFrontIRPhase.all |> List.map KFrontIRPhase.checkpointName
            "KCore"
            "KRuntimeIR"
            "KBackendIR"
        ]

    let tryParseCheckpoint checkpoint =
        if String.Equals(checkpoint, "surface-source", StringComparison.Ordinal) then
            Some None
        else
            KFrontIRPhase.all
            |> List.tryFind (fun phase -> String.Equals(KFrontIRPhase.checkpointName phase, checkpoint, StringComparison.Ordinal))
            |> Option.map Some

    let private verifySurfaceSource (workspace: WorkspaceCompilation) =
        let duplicatePaths =
            workspace.Documents
            |> List.groupBy (fun document -> document.Source.FilePath)
            |> List.filter (fun (_, documents) -> List.length documents > 1)

        [
            for filePath, documents in duplicatePaths do
                let locations =
                    documents
                    |> List.map (fun document -> document.Source.GetLocation(TextSpan.FromBounds(0, 0)))

                yield
                    makeStructuredDuplicateLocationDiagnostic
                        (DuplicateFileIdentity("surface-source", filePath))
                        "also appears here"
                        locations

            for document in workspace.Documents do
                if document.Source.LineCount <= 0 then
                    yield
                        makeStructuredModuleDiagnostic
                            workspace.Documents
                            (document.ModuleName |> ModuleIdentity.ofOptionalSegments)
                            document.Source.FilePath
                            (NonEmptyLineTableRequired("surface-source", document.Source.FilePath))
        ]

    let private frontendWorkspaceForPhase (workspace: WorkspaceCompilation) phase =
        match workspace.FrontendSnapshots |> Map.tryFind phase with
        | Some snapshot ->
            { workspace with
                KFrontIR = snapshot.Modules
                Diagnostics = snapshot.Diagnostics }
        | None ->
            workspace

    let private verifyFrontendCheckpoint (workspace: WorkspaceCompilation) checkpoint phase =
        [
            let duplicatePaths =
                workspace.KFrontIR
                |> List.groupBy (fun document -> document.FilePath)
                |> List.filter (fun (_, documents) -> List.length documents > 1)

            for filePath, documents in duplicatePaths do
                let locations =
                    documents
                    |> List.choose (fun document -> fileOriginLocation workspace.Documents document.FilePath)

                yield
                    makeStructuredDuplicateLocationDiagnostic
                        (DuplicateFileIdentity(checkpoint, filePath))
                        "also appears here"
                        locations

            for document in workspace.KFrontIR do
                match List.tryLast document.Tokens with
                | Some token when token.Kind = EndOfFile -> ()
                | _ ->
                    yield
                        makeStructuredModuleDiagnostic
                            workspace.Documents
                            (document.ModuleIdentity |> ModuleIdentity.ofOptionalSegments)
                            document.FilePath
                            (EndOfFileTokenRequired(checkpoint, document.FilePath))

                let expectedPhase =
                    if KFrontIRPhase.ordinal phase < KFrontIRPhase.ordinal BODY_RESOLVE then
                        phase
                    elif document.Ownership.IsSome then
                        phase
                    else
                        IMPLICIT_SIGNATURES

                let expectedResolvedPhases =
                    KFrontIRPhase.phasesThrough expectedPhase |> Set.ofList

                if document.ResolvedPhases <> expectedResolvedPhases then
                    yield
                        makeStructuredModuleDiagnostic
                            workspace.Documents
                            (document.ModuleIdentity |> ModuleIdentity.ofOptionalSegments)
                            document.FilePath
                            (
                                ResolvedPhaseSetMismatch(
                                    checkpoint,
                                    document.FilePath,
                                    expectedResolvedPhases |> Set.toList |> List.map KFrontIRPhase.phaseName,
                                    document.ResolvedPhases |> Set.toList |> List.map KFrontIRPhase.phaseName
                                )
                            )

                if
                    KFrontIRPhase.ordinal phase < KFrontIRPhase.ordinal BODY_RESOLVE
                    && document.Ownership.IsSome
                then
                    yield
                        makeStructuredModuleDiagnostic
                            workspace.Documents
                            (document.ModuleIdentity |> ModuleIdentity.ofOptionalSegments)
                            document.FilePath
                            (OwnershipFactsExposedBeforeBodyResolve(checkpoint, document.FilePath))
        ]

    let private verifyKCoreCheckpoint (workspace: WorkspaceCompilation) =
        let duplicateModules =
            workspace.KCore
            |> List.groupBy (fun moduleDump -> moduleDump.Name)
            |> List.filter (fun (_, modules) -> List.length modules > 1)

        [
            let frontendWorkspace = frontendWorkspaceForPhase workspace CORE_LOWERING
            yield! verifyFrontendCheckpoint frontendWorkspace "KCore" CORE_LOWERING

            for moduleName, modules in duplicateModules do
                let locations =
                    modules
                    |> List.choose (fun moduleDump -> fileOriginLocation workspace.Documents moduleDump.SourceFile)

                yield
                    makeStructuredDuplicateLocationDiagnostic
                        (DuplicateModuleIdentity("KCore", moduleName))
                        "also appears here"
                        locations
        ]

    let private importedModuleName (spec: ImportSpec) : string option =
        match spec.Source with
        | Dotted moduleSegments ->
            Some(SyntaxFacts.moduleNameToText moduleSegments)
        | Url _ ->
            None

    let private availableIntrinsicTerms backendProfile allowUnsafeConsume moduleName =
        Stdlib.intrinsicTermNamesAvailableInModuleTextForCompilationProfile
            backendProfile
            allowUnsafeConsume
            moduleName

    let private verifyRuntimePattern documents checkpoint bindingLabel bindingOrigin (pattern: KRuntimePattern) =
        let rec verify locals runtimePattern =
            match runtimePattern with
            | KRuntimeWildcardPattern
            | KRuntimeLiteralPattern _ ->
                locals, []
            | KRuntimeNamePattern name ->
                if Set.contains name locals then
                    locals,
                    [ makeStructuredOriginDiagnostic
                          documents
                          bindingOrigin
                          (DuplicateRuntimePatternBinder(checkpoint, bindingLabel, name)) ]
                else
                    Set.add name locals, []
            | KRuntimeOrPattern alternatives ->
                ((locals, []), alternatives)
                ||> List.fold (fun (localsSoFar, diagnosticsSoFar) alternative ->
                    let nextLocals, nextDiagnostics = verify localsSoFar alternative
                    nextLocals, diagnosticsSoFar @ nextDiagnostics)
            | KRuntimeConstructorPattern(_, argumentPatterns) ->
                ((locals, []), argumentPatterns)
                ||> List.fold (fun (localsSoFar, diagnosticsSoFar) argumentPattern ->
                    let nextLocals, nextDiagnostics = verify localsSoFar argumentPattern
                    nextLocals, diagnosticsSoFar @ nextDiagnostics)

        verify Set.empty pattern

    let private runtimeTypeLeaksErasureMetadata allowEffectRows (typeText: string) =
        let rec loop typeExpr =
            match typeExpr with
            | TypeSignatures.TypeLevelLiteral _ ->
                false
            | TypeSignatures.TypeUniverse None ->
                true
            | TypeSignatures.TypeUniverse(Some _) ->
                true
            | TypeSignatures.TypeIntrinsic _ ->
                true
            | TypeSignatures.TypeApply(callee, arguments) ->
                loop callee || (arguments |> List.exists loop)
            | TypeSignatures.TypeLambda(_, parameterSort, body) ->
                loop parameterSort || loop body
            | TypeSignatures.TypeDelay inner
            | TypeSignatures.TypeMemo inner
            | TypeSignatures.TypeForce inner ->
                true
            | TypeSignatures.TypeProject(target, _) ->
                loop target
            | TypeSignatures.TypeVariable _ ->
                false
            | TypeSignatures.TypeName(name, arguments) ->
                let head = name |> List.tryLast |> Option.defaultValue ""

                arguments |> List.exists loop
                || String.Equals(head, "Type", StringComparison.Ordinal)
                || String.Equals(head, CompilerKnownSymbols.KnownTypeNames.Constraint, StringComparison.Ordinal)
                || String.Equals(head, CompilerKnownSymbols.KnownTypeNames.Quantity, StringComparison.Ordinal)
                || String.Equals(head, CompilerKnownSymbols.KnownTypeNames.Region, StringComparison.Ordinal)
                || String.Equals(head, CompilerKnownSymbols.KnownTypeNames.RecRow, StringComparison.Ordinal)
                || String.Equals(head, CompilerKnownSymbols.KnownTypeNames.Label, StringComparison.Ordinal)
            | TypeSignatures.TypeArrow(quantity, parameterType, resultType) ->
                quantity <> QuantityOmega || loop parameterType || loop resultType
            | TypeSignatures.TypeEquality _ ->
                true
            | TypeSignatures.TypeCapture _ ->
                true
            | TypeSignatures.TypeEffectRow _ ->
                not allowEffectRows
            | TypeSignatures.TypeRecord fields ->
                fields
                |> List.exists (fun field -> field.Quantity <> QuantityOmega || loop field.Type)
            | TypeSignatures.TypeUnion members ->
                members |> List.exists loop

        match tryParseTypeText typeText with
        | Some parsed ->
            loop parsed
        | None ->
            typeText.Contains("captures (", StringComparison.Ordinal)
            || typeText.Contains(CompilerKnownSymbols.KnownTypeNames.Region, StringComparison.Ordinal)
            || typeText.Contains(CompilerKnownSymbols.KnownTypeNames.Constraint, StringComparison.Ordinal)
            || typeText.Contains(CompilerKnownSymbols.KnownTypeNames.Quantity, StringComparison.Ordinal)
            || typeText.Contains("&[", StringComparison.Ordinal)
            || typeText.Contains("(&", StringComparison.Ordinal)

    let private verifyRuntimeExpression documents checkpoint bindingLabel bindingOrigin (expression: KRuntimeExpression) =
        let rec verify locals runtimeExpression =
            match runtimeExpression with
            | KRuntimeLiteral _
            | KRuntimeName _ ->
                []
            | KRuntimeEffectLabel _ ->
                []
            | KRuntimeEffectOperation(label, _, _) ->
                verify locals label
            | KRuntimeClosure(parameters, body) ->
                let duplicateParameters =
                    parameters
                    |> List.countBy id
                    |> List.filter (fun (_, count) -> count > 1)
                    |> List.map (fun (name, _) ->
                        makeStructuredOriginDiagnostic
                            documents
                            bindingOrigin
                            (DuplicateRuntimeClosureParameter(checkpoint, bindingLabel, name)))

                let extendedLocals =
                    parameters
                    |> List.fold (fun state parameterName -> Set.add parameterName state) locals

                duplicateParameters @ verify extendedLocals body
            | KRuntimeIfThenElse(condition, whenTrue, whenFalse) ->
                verify locals condition @ verify locals whenTrue @ verify locals whenFalse
            | KRuntimeHandle(isDeep, label, body, returnClause, operationClauses) ->
                let verifyClause (clause: KRuntimeEffectHandlerClause) =
                    let clauseLocals =
                        clause.Arguments
                        |> List.choose (function
                            | KRuntimeEffectNameArgument name -> Some name
                            | _ -> None)
                        |> List.fold (fun state name -> Set.add name state) locals
                        |> fun state ->
                            clause.ResumptionName
                            |> Option.map (fun name -> Set.add name state)
                            |> Option.defaultValue state

                    verify clauseLocals clause.Body

                let deepHandlerDiagnostics =
                    if isDeep then
                        [ makeStructuredOriginDiagnostic
                              documents
                              bindingOrigin
                              (DeepRuntimeHandlerMustBeDesugared(checkpoint, bindingLabel)) ]
                    else
                        []

                deepHandlerDiagnostics
                @ verify locals label
                @ verify locals body
                @ verifyClause returnClause
                @ (operationClauses |> List.collect verifyClause)
            | KRuntimeMatch(scrutinee, cases) ->
                verify locals scrutinee
                @ (cases
                   |> List.collect (fun (caseClause: KRuntimeMatchCase) ->
                       let caseLocals, patternDiagnostics =
                           verifyRuntimePattern documents checkpoint bindingLabel bindingOrigin caseClause.Pattern

                       let extendedLocals = Set.union locals caseLocals

                       let guardDiagnostics =
                           caseClause.Guard
                           |> Option.map (verify extendedLocals)
                           |> Option.defaultValue []

                       patternDiagnostics @ guardDiagnostics @ verify extendedLocals caseClause.Body))
            | KRuntimeExecute expression ->
                verify locals expression
            | KRuntimeLet(bindingName, value, body) ->
                verify locals value @ verify (Set.add bindingName locals) body
            | KRuntimeDoScope(_, body) ->
                verify locals body
            | KRuntimeScheduleExit(_, action, body) ->
                let actionDiagnostics =
                    match action with
                    | KRuntimeDeferred expression ->
                        verify locals expression
                    | KRuntimeRelease(_, release, resource) ->
                        verify locals release @ verify locals resource

                actionDiagnostics @ verify locals body
            | KRuntimeSequence(first, second) ->
                verify locals first @ verify locals second
            | KRuntimeWhile(condition, body) ->
                verify locals condition @ verify locals body
            | KRuntimeApply(callee, arguments) ->
                verify locals callee
                @ (arguments |> List.collect (verify locals))
            | KRuntimeDictionaryValue _ ->
                []
            | KRuntimeTraitCall(_, _, dictionary, arguments) ->
                verify locals dictionary
                @ (arguments |> List.collect (verify locals))
            | KRuntimeUnary(_, operand) ->
                verify locals operand
            | KRuntimeBinary(left, _, right) ->
                verify locals left @ verify locals right
            | KRuntimePrefixedString(_, parts) ->
                parts
                |> List.collect (function
                    | KRuntimeStringText _ ->
                        []
                    | KRuntimeStringInterpolation(inner, _) ->
                        verify locals inner)

        verify Set.empty expression

    let rec private backendExpressionUsesEffectRuntime expression =
        match expression with
        | BackendEffectLabel _
        | BackendEffectOperation _
        | BackendPure _
        | BackendBind _
        | BackendThen _
        | BackendHandle _ ->
            true
        | BackendLiteral _
        | BackendName _
        | BackendDictionaryValue _ ->
            false
        | BackendClosure(_, _, _, body, _, _) ->
            backendExpressionUsesEffectRuntime body
        | BackendIfThenElse(condition, whenTrue, whenFalse, _) ->
            backendExpressionUsesEffectRuntime condition
            || backendExpressionUsesEffectRuntime whenTrue
            || backendExpressionUsesEffectRuntime whenFalse
        | BackendMatch(scrutinee, cases, _) ->
            backendExpressionUsesEffectRuntime scrutinee
            || (cases
                |> List.exists (fun caseClause ->
                    caseClause.Guard |> Option.exists backendExpressionUsesEffectRuntime
                    || backendExpressionUsesEffectRuntime caseClause.Body))
        | BackendExecute(inner, _) ->
            backendExpressionUsesEffectRuntime inner
        | BackendLet(_, value, body, _) ->
            backendExpressionUsesEffectRuntime value
            || backendExpressionUsesEffectRuntime body
        | BackendSequence(first, second, _) ->
            backendExpressionUsesEffectRuntime first
            || backendExpressionUsesEffectRuntime second
        | BackendWhile(condition, body) ->
            backendExpressionUsesEffectRuntime condition
            || backendExpressionUsesEffectRuntime body
        | BackendCall(callee, arguments, _, _) ->
            backendExpressionUsesEffectRuntime callee
            || (arguments |> List.exists backendExpressionUsesEffectRuntime)
        | BackendTraitCall(_, _, dictionary, arguments, _) ->
            backendExpressionUsesEffectRuntime dictionary
            || (arguments |> List.exists backendExpressionUsesEffectRuntime)
        | BackendConstructData(_, _, _, _, fields, _) ->
            fields |> List.exists backendExpressionUsesEffectRuntime
        | BackendPrefixedString(_, parts, _) ->
            parts
            |> List.exists (function
                | BackendStringText _ -> false
                | BackendStringInterpolation inner -> backendExpressionUsesEffectRuntime inner)

    let private verifyKRuntimeIRCheckpoint (workspace: WorkspaceCompilation) =
        let isGeneratedHostBinding provenance =
            String.Equals(provenance.IntroductionKind, "host-binding", StringComparison.Ordinal)

        let backendProfile = BackendProfile.toPortableName workspace.Backend

        let runtimeModules =
            workspace.KRuntimeIR
            |> List.sortBy (fun (moduleDump: KRuntimeModule) -> moduleDump.SourceFile)

        let moduleMap =
            runtimeModules
            |> List.map (fun moduleDump -> moduleDump.Name, moduleDump)
            |> Map.ofList

        [
            yield! verifyKCoreCheckpoint workspace

            for moduleDump in runtimeModules do
                for spec in moduleDump.Imports do
                    match importedModuleName spec with
                    | Some importedName when not (moduleMap.ContainsKey importedName) ->
                        yield
                            makeStructuredModuleDiagnostic
                                workspace.Documents
                                (renderedModuleIdentity moduleDump.Name)
                                moduleDump.SourceFile
                                (MissingImportedRuntimeModule("KRuntimeIR", moduleDump.Name, importedName))
                    | _ ->
                        ()

                let duplicateBindings =
                    moduleDump.Bindings
                    |> List.groupBy (fun binding -> binding.Name)
                    |> List.filter (fun (_, bindings) -> List.length bindings > 1)

                for bindingName, bindings in duplicateBindings do
                    let locations =
                        bindings
                        |> List.choose (fun binding -> provenancePrimaryLocation workspace.Documents binding.Provenance)

                    yield
                        makeStructuredDuplicateLocationDiagnostic
                            (DuplicateRuntimeBindingIdentity("KRuntimeIR", moduleDump.Name, bindingName))
                            "also declared here"
                            locations

                let duplicateConstructors =
                    moduleDump.Constructors
                    |> List.groupBy (fun constructor -> constructor.Name)
                    |> List.filter (fun (_, constructors) -> List.length constructors > 1)

                for constructorName, constructors in duplicateConstructors do
                    let locations =
                        constructors
                        |> List.choose (fun constructor -> provenancePrimaryLocation workspace.Documents constructor.Provenance)

                    yield
                        makeStructuredDuplicateLocationDiagnostic
                            (DuplicateRuntimeConstructorIdentity("KRuntimeIR", moduleDump.Name, constructorName))
                            "also declared here"
                            locations

                let supportedIntrinsics = availableIntrinsicTerms workspace.Backend workspace.AllowUnsafeConsume moduleDump.Name
                let unsupportedIntrinsicTerms = moduleDump.IntrinsicTerms |> List.filter (fun intrinsicName -> not (supportedIntrinsics.Contains intrinsicName))
                let unsupportedIntrinsicTermSet = unsupportedIntrinsicTerms |> Set.ofList

                for intrinsicName in unsupportedIntrinsicTerms do
                    yield
                        makeStructuredModuleDiagnostic
                            workspace.Documents
                            (renderedModuleIdentity moduleDump.Name)
                            moduleDump.SourceFile
                            (UnsupportedRuntimeIntrinsicTerm("KRuntimeIR", moduleDump.Name, intrinsicName, backendProfile))

                for binding in moduleDump.Bindings do
                    if binding.Intrinsic then
                        if binding.Body.IsSome then
                            yield
                                makeStructuredOriginDiagnostic
                                    workspace.Documents
                                    binding.Provenance
                                    (IntrinsicBindingMustOmitBody("KRuntimeIR", moduleDump.Name, binding.Name))

                        if not (List.contains binding.Name moduleDump.IntrinsicTerms) then
                            yield
                                makeStructuredOriginDiagnostic
                                    workspace.Documents
                                    binding.Provenance
                                    (IntrinsicBindingMustBeListedInModuleIntrinsicTerms("KRuntimeIR", moduleDump.Name, binding.Name))

                        if
                            not (supportedIntrinsics.Contains binding.Name)
                            && not (Set.contains binding.Name unsupportedIntrinsicTermSet)
                        then
                            yield
                                makeStructuredOriginDiagnostic
                                    workspace.Documents
                                    binding.Provenance
                                    (UnsupportedRuntimeIntrinsicBinding("KRuntimeIR", moduleDump.Name, binding.Name, backendProfile))
                    else
                        match binding.Body with
                        | None ->
                            if not (isGeneratedHostBinding binding.Provenance) then
                                yield
                                    makeStructuredOriginDiagnostic
                                        workspace.Documents
                                        binding.Provenance
                                        (RuntimeBindingMustHaveBody("KRuntimeIR", moduleDump.Name, binding.Name))
                        | Some body ->
                            let bindingLabel = $"{moduleDump.Name}.{binding.Name}"
                            yield! verifyRuntimeExpression workspace.Documents "KRuntimeIR" bindingLabel binding.Provenance body
        ]

    let private verifyKBackendPattern
        (documents: ParsedDocument list)
        (moduleMap: Map<string, KBackendModule>)
        checkpoint
        bindingLabel
        bindingOrigin
        (pattern: KBackendPattern)
        =
        let rec collectBindings backendPattern =
            match backendPattern with
            | BackendWildcardPattern
            | BackendLiteralPattern _ ->
                Map.empty, []
            | BackendBindPattern binding ->
                Map.ofList [ binding.Name, binding.Representation ], []
            | BackendConstructorPattern(_, _, _, _, fieldPatterns) ->
                ((Map.empty, []), fieldPatterns)
                ||> List.fold (fun (bindingsSoFar, diagnosticsSoFar) fieldPattern ->
                    let fieldBindings, fieldDiagnostics = collectBindings fieldPattern

                    let mergeDiagnostics =
                        fieldBindings
                        |> Map.fold
                            (fun state name _ ->
                                if Map.containsKey name bindingsSoFar then
                                    state
                                    @ [ makeStructuredOriginDiagnostic
                                            documents
                                            bindingOrigin
                                            (DuplicateBackendPatternBinder(checkpoint, bindingLabel, name)) ]
                                else
                                    state)
                            diagnosticsSoFar

                    let mergedBindings =
                        fieldBindings |> Map.fold (fun state name representation -> state |> Map.add name representation) bindingsSoFar

                    mergedBindings, mergeDiagnostics @ fieldDiagnostics)
            | BackendOrPattern alternatives ->
                let collected =
                    alternatives |> List.map collectBindings

                match collected with
                | [] ->
                    Map.empty, []
                | (firstBindings, firstDiagnostics) :: rest ->
                    let firstNames = firstBindings |> Map.keys |> Set.ofSeq

                    let diagnostics =
                        rest
                        |> List.fold
                            (fun state (candidateBindings, candidateDiagnostics) ->
                                let state = state @ candidateDiagnostics
                                let candidateNames = candidateBindings |> Map.keys |> Set.ofSeq

                                let state =
                                    if candidateNames <> firstNames then
                                        state
                                        @ [ makeStructuredOriginDiagnostic
                                                documents
                                                bindingOrigin
                                                (BackendOrPatternAlternativesMustBindSameNames(checkpoint, bindingLabel)) ]
                                    else
                                        state

                                firstBindings
                                |> Map.fold
                                    (fun comparisonState name representation ->
                                        match candidateBindings |> Map.tryFind name with
                                        | Some candidateRepresentation when candidateRepresentation = representation ->
                                            comparisonState
                                        | Some _ ->
                                            comparisonState
                                            @ [ makeStructuredOriginDiagnostic
                                                    documents
                                                    bindingOrigin
                                                    (BackendOrPatternBinderRepresentationMismatch(checkpoint, bindingLabel, name)) ]
                                        | None ->
                                            comparisonState)
                                    state)
                            firstDiagnostics

                    firstBindings, diagnostics

        let rec verifyConstructors backendPattern =
            match backendPattern with
            | BackendWildcardPattern
            | BackendLiteralPattern _
            | BackendBindPattern _ ->
                []
            | BackendOrPattern alternatives ->
                alternatives |> List.collect verifyConstructors
            | BackendConstructorPattern(moduleName, typeName, constructorName, tag, fieldPatterns) ->
                let constructorExists =
                    moduleMap
                    |> Map.tryFind moduleName
                    |> Option.exists (fun moduleDump ->
                        moduleDump.DataLayouts
                        |> List.exists (fun layout ->
                            String.Equals(layout.TypeName, typeName, StringComparison.Ordinal)
                            && (layout.Constructors
                                |> List.exists (fun constructor ->
                                    String.Equals(constructor.Name, constructorName, StringComparison.Ordinal)
                                    && constructor.Tag = tag))))

                let diagnostics =
                    if constructorExists then
                        []
                    else
                        let constructorText = $"{moduleName}.{typeName}.{constructorName}@{tag}"
                        [ makeStructuredOriginDiagnostic
                              documents
                              bindingOrigin
                              (BackendPatternConstructorMissingFromModuleGraph(checkpoint, bindingLabel, constructorText)) ]

                diagnostics @ (fieldPatterns |> List.collect verifyConstructors)

        let bindings, bindingDiagnostics = collectBindings pattern
        bindings |> Map.keys |> Set.ofSeq, bindingDiagnostics @ verifyConstructors pattern

    let private verifyKBackendIRCheckpoint (workspace: WorkspaceCompilation) =
        let isGeneratedHostBinding provenance =
            String.Equals(provenance.IntroductionKind, "host-binding", StringComparison.Ordinal)

        let backendModules =
            workspace.KBackendIR
            |> List.sortBy (fun (moduleDump: KBackendModule) -> moduleDump.SourceFile)

        let moduleMap =
            backendModules
            |> List.map (fun moduleDump -> moduleDump.Name, moduleDump)
            |> Map.ofList

        let globallyAvailableIntrinsicTerms =
            Stdlib.runtimeIntrinsicTermNamesForCompilationProfile
                workspace.Backend
                workspace.AllowUnsafeConsume
                Stdlib.PreludeModuleName

        let functionNames (moduleDump: KBackendModule) =
            moduleDump.Functions
            |> List.map (fun binding -> binding.Name)
            |> Set.ofList

        let dataLayoutNames (moduleDump: KBackendModule) =
            moduleDump.DataLayouts
            |> List.map (fun layout -> layout.TypeName)
            |> Set.ofList

        let environmentLayoutNames (moduleDump: KBackendModule) =
            moduleDump.EnvironmentLayouts
            |> List.map (fun layout -> layout.Name)
            |> Set.ofList

        let allowEffectRowMetadata =
            backendModules
            |> List.exists (fun moduleDump ->
                moduleDump.Functions
                |> List.exists (fun functionDump ->
                    functionDump.Body
                    |> Option.exists backendExpressionUsesEffectRuntime))

        let erasureDiagnostics =
            [
                for runtimeModule in workspace.KRuntimeIR do
                    for binding in runtimeModule.Bindings do
                        for parameter in binding.Parameters do
                            match parameter.TypeText with
                            | Some typeText when runtimeTypeLeaksErasureMetadata allowEffectRowMetadata typeText ->
                                yield
                                    makeStructuredOriginDiagnostic
                                        workspace.Documents
                                        binding.Provenance
                                        (PreErasureParameterTypeMetadataLeak("KBackendIR", $"{runtimeModule.Name}.{binding.Name}", parameter.Name, typeText))
                            | _ ->
                                ()

                        match binding.ReturnTypeText with
                        | Some typeText when runtimeTypeLeaksErasureMetadata allowEffectRowMetadata typeText ->
                            yield
                                makeStructuredOriginDiagnostic
                                    workspace.Documents
                                    binding.Provenance
                                    (PreErasureReturnTypeMetadataLeak("KBackendIR", $"{runtimeModule.Name}.{binding.Name}", typeText))
                        | _ ->
                            ()

                    for dataType in runtimeModule.DataTypes do
                        for constructor in dataType.Constructors do
                            for index, fieldTypeText in constructor.FieldTypeTexts |> List.indexed do
                                if runtimeTypeLeaksErasureMetadata allowEffectRowMetadata fieldTypeText then
                                    yield
                                        makeStructuredOriginDiagnostic
                                            workspace.Documents
                                            constructor.Provenance
                                            (PreErasureConstructorFieldMetadataLeak("KBackendIR", $"{runtimeModule.Name}.{constructor.Name}[{index}]", fieldTypeText))

                    for instanceInfo in runtimeModule.TraitInstances do
                        for index, headTypeText in instanceInfo.HeadTypeTexts |> List.indexed do
                            if runtimeTypeLeaksErasureMetadata allowEffectRowMetadata headTypeText then
                                yield
                                    makeStructuredModuleDiagnostic
                                        workspace.Documents
                                        (renderedModuleIdentity runtimeModule.Name)
                                        runtimeModule.SourceFile
                                        (PreErasureInstanceHeadMetadataLeak("KBackendIR", $"{runtimeModule.Name}.{instanceInfo.TraitName}[{index}]", headTypeText))
            ]

        let constructorExists moduleName typeName constructorName tag fieldCount =
            moduleMap
            |> Map.tryFind moduleName
            |> Option.exists (fun moduleDump ->
                moduleDump.DataLayouts
                |> List.exists (fun layout ->
                    String.Equals(layout.TypeName, typeName, StringComparison.Ordinal)
                    && (layout.Constructors
                        |> List.exists (fun constructor ->
                            String.Equals(constructor.Name, constructorName, StringComparison.Ordinal)
                            && constructor.Tag = tag
                            && List.length constructor.FieldRepresentations = fieldCount))))

        let resolvedNameExists (resolvedName: KBackendResolvedName) =
            match resolvedName with
            | BackendLocalName _ ->
                true
            | BackendGlobalBindingName(moduleName, bindingName, _)
            | BackendIntrinsicName(moduleName, bindingName, _) ->
                (moduleMap
                 |> Map.tryFind moduleName
                 |> Option.exists (fun moduleDump -> functionNames moduleDump |> Set.contains bindingName))
                || (String.Equals(moduleName, Stdlib.PreludeModuleText, StringComparison.Ordinal)
                    && globallyAvailableIntrinsicTerms.Contains bindingName)
            | BackendConstructorName(moduleName, typeName, constructorName, tag, arity, _) ->
                constructorExists moduleName typeName constructorName tag arity

        let resolvedNameText (resolvedName: KBackendResolvedName) =
            match resolvedName with
            | BackendLocalName(name, _) ->
                name
            | BackendGlobalBindingName(moduleName, bindingName, _) ->
                $"{moduleName}.{bindingName}"
            | BackendIntrinsicName(moduleName, bindingName, _) ->
                $"intrinsic {moduleName}.{bindingName}"
            | BackendConstructorName(moduleName, typeName, constructorName, tag, _, _) ->
                $"{moduleName}.{typeName}.{constructorName}@{tag}"

        let rec verifyBackendExpression (currentModule: KBackendModule) bindingLabel bindingOrigin backendExpression =
            match backendExpression with
            | BackendLiteral _ ->
                []
            | BackendName resolvedName ->
                if resolvedNameExists resolvedName then
                    []
                else
                    [ makeStructuredOriginDiagnostic
                          workspace.Documents
                          bindingOrigin
                          (BackendResolvedRuntimeNameMissing("KBackendIR", bindingLabel, resolvedNameText resolvedName)) ]
            | BackendEffectLabel _ ->
                []
            | BackendEffectOperation(label, _, _, _) ->
                verifyBackendExpression currentModule bindingLabel bindingOrigin label
            | BackendClosure(parameters, captures, environmentLayout, body, convention, representation) ->
                let duplicateParameters =
                    parameters
                    |> List.countBy (fun parameter -> parameter.Name)
                    |> List.filter (fun (_, count) -> count > 1)
                    |> List.map (fun (name, _) ->
                        makeStructuredOriginDiagnostic
                            workspace.Documents
                            bindingOrigin
                            (DuplicateBackendClosureParameter("KBackendIR", bindingLabel, name)))

                let duplicateCaptures =
                    captures
                    |> List.countBy (fun capture -> capture.Name)
                    |> List.filter (fun (_, count) -> count > 1)
                    |> List.map (fun (name, _) ->
                        makeStructuredOriginDiagnostic
                            workspace.Documents
                            bindingOrigin
                            (DuplicateBackendClosureCapture("KBackendIR", bindingLabel, name)))

                let environmentDiagnostics =
                    if environmentLayoutNames currentModule |> Set.contains environmentLayout then
                        []
                    else
                        [ makeStructuredOriginDiagnostic
                              workspace.Documents
                              bindingOrigin
                              (BackendClosureEnvironmentLayoutMissing("KBackendIR", bindingLabel, environmentLayout, currentModule.Name)) ]

                let conventionDiagnostics =
                    [
                        if convention.RuntimeArity <> List.length parameters then
                            yield
                                makeStructuredOriginDiagnostic
                                    workspace.Documents
                                    bindingOrigin
                                    (BackendClosureCallingConventionArityMismatch("KBackendIR", bindingLabel))

                        if convention.ParameterRepresentations <> (parameters |> List.map (fun parameter -> parameter.Representation)) then
                            yield
                                makeStructuredOriginDiagnostic
                                    workspace.Documents
                                    bindingOrigin
                                    (BackendClosureParameterRepresentationsMismatch("KBackendIR", bindingLabel))

                        match representation with
                        | BackendRepClosure layoutName when not (String.Equals(layoutName, environmentLayout, StringComparison.Ordinal)) ->
                            yield
                                makeStructuredOriginDiagnostic
                                    workspace.Documents
                                    bindingOrigin
                                    (BackendClosureRepresentationLayoutMismatch("KBackendIR", bindingLabel, environmentLayout))
                        | BackendRepClosure _ -> ()
                        | _ ->
                            yield
                                makeStructuredOriginDiagnostic
                                    workspace.Documents
                                    bindingOrigin
                                    (BackendClosureRepresentationMustBeClosure("KBackendIR", bindingLabel))
                    ]

                duplicateParameters
                @ duplicateCaptures
                @ environmentDiagnostics
                @ conventionDiagnostics
                @ verifyBackendExpression currentModule bindingLabel bindingOrigin body
            | BackendIfThenElse(condition, whenTrue, whenFalse, _) ->
                verifyBackendExpression currentModule bindingLabel bindingOrigin condition
                @ verifyBackendExpression currentModule bindingLabel bindingOrigin whenTrue
                @ verifyBackendExpression currentModule bindingLabel bindingOrigin whenFalse
            | BackendMatch(scrutinee, cases, _) ->
                verifyBackendExpression currentModule bindingLabel bindingOrigin scrutinee
                @ (cases
                   |> List.collect (fun caseClause ->
                       let _, patternDiagnostics =
                           verifyKBackendPattern workspace.Documents moduleMap "KBackendIR" bindingLabel bindingOrigin caseClause.Pattern

                       let guardDiagnostics =
                           caseClause.Guard
                           |> Option.map (verifyBackendExpression currentModule bindingLabel bindingOrigin)
                           |> Option.defaultValue []

                       patternDiagnostics @ guardDiagnostics @ verifyBackendExpression currentModule bindingLabel bindingOrigin caseClause.Body))
            | BackendExecute(expression, _) ->
                verifyBackendExpression currentModule bindingLabel bindingOrigin expression
            | BackendPure(expression, _) ->
                verifyBackendExpression currentModule bindingLabel bindingOrigin expression
            | BackendBind(action, binder, _) ->
                verifyBackendExpression currentModule bindingLabel bindingOrigin action
                @ verifyBackendExpression currentModule bindingLabel bindingOrigin binder
            | BackendThen(first, second, _) ->
                verifyBackendExpression currentModule bindingLabel bindingOrigin first
                @ verifyBackendExpression currentModule bindingLabel bindingOrigin second
            | BackendHandle(_, label, body, returnClause, operationClauses, _) ->
                verifyBackendExpression currentModule bindingLabel bindingOrigin label
                @ verifyBackendExpression currentModule bindingLabel bindingOrigin body
                @ verifyBackendExpression currentModule bindingLabel bindingOrigin returnClause.Body
                @ (operationClauses |> List.collect (fun clause -> verifyBackendExpression currentModule bindingLabel bindingOrigin clause.Body))
            | BackendLet(_, value, body, _) ->
                verifyBackendExpression currentModule bindingLabel bindingOrigin value
                @ verifyBackendExpression currentModule bindingLabel bindingOrigin body
            | BackendSequence(first, second, _) ->
                verifyBackendExpression currentModule bindingLabel bindingOrigin first
                @ verifyBackendExpression currentModule bindingLabel bindingOrigin second
            | BackendWhile(condition, body) ->
                verifyBackendExpression currentModule bindingLabel bindingOrigin condition
                @ verifyBackendExpression currentModule bindingLabel bindingOrigin body
            | BackendCall(callee, arguments, convention, _) ->
                let conventionDiagnostics =
                    [
                        if convention.RuntimeArity <> List.length arguments then
                            yield
                                makeStructuredOriginDiagnostic
                                    workspace.Documents
                                    bindingOrigin
                                    (BackendCallConventionArityMismatch("KBackendIR", bindingLabel))

                        if List.length convention.ParameterRepresentations <> List.length arguments then
                            yield
                                makeStructuredOriginDiagnostic
                                    workspace.Documents
                                    bindingOrigin
                                    (BackendCallParameterRepresentationCountMismatch("KBackendIR", bindingLabel))
                    ]

                conventionDiagnostics
                @ verifyBackendExpression currentModule bindingLabel bindingOrigin callee
                @ (arguments |> List.collect (verifyBackendExpression currentModule bindingLabel bindingOrigin))
            | BackendDictionaryValue(_, _, _, _, _) ->
                []
            | BackendTraitCall(_, _, dictionary, arguments, _) ->
                verifyBackendExpression currentModule bindingLabel bindingOrigin dictionary
                @ (arguments |> List.collect (verifyBackendExpression currentModule bindingLabel bindingOrigin))
            | BackendConstructData(moduleName, typeName, constructorName, tag, fields, _) ->
                let constructorDiagnostics =
                    if constructorExists moduleName typeName constructorName tag (List.length fields) then
                        []
                    else
                        let constructorText = $"{moduleName}.{typeName}.{constructorName}@{tag}"
                        [ makeStructuredOriginDiagnostic
                              workspace.Documents
                              bindingOrigin
                              (BackendConstructedDataMissingLayout("KBackendIR", bindingLabel, constructorText)) ]

                constructorDiagnostics @ (fields |> List.collect (verifyBackendExpression currentModule bindingLabel bindingOrigin))
            | BackendPrefixedString(_, parts, _) ->
                parts
                |> List.collect (function
                    | BackendStringText _ ->
                        []
                    | BackendStringInterpolation inner ->
                        verifyBackendExpression currentModule bindingLabel bindingOrigin inner)

        let backendProfile = BackendProfile.toPortableName workspace.Backend

        [
            yield! verifyKRuntimeIRCheckpoint workspace
            yield! erasureDiagnostics

            let runtimeModuleNames =
                workspace.KRuntimeIR
                |> List.map (fun moduleDump -> moduleDump.Name)
                |> Set.ofList

            for runtimeModuleName in runtimeModuleNames do
                if not (moduleMap.ContainsKey runtimeModuleName) then
                    match workspace.KRuntimeIR |> List.tryFind (fun runtimeModule -> String.Equals(runtimeModule.Name, runtimeModuleName, StringComparison.Ordinal)) with
                    | Some runtimeModule ->
                        yield
                            makeStructuredModuleDiagnostic
                                workspace.Documents
                                (renderedModuleIdentity runtimeModule.Name)
                                runtimeModule.SourceFile
                                (MissingBackendModuleForRuntimeModule("KBackendIR", runtimeModuleName))
                    | None ->
                        yield makeStructuredDiagnostic (MissingBackendModuleForRuntimeModule("KBackendIR", runtimeModuleName))

            for moduleDump in backendModules do
                for spec in moduleDump.Imports do
                    match importedModuleName spec with
                    | Some importedName when not (moduleMap.ContainsKey importedName) ->
                        yield
                            makeStructuredModuleDiagnostic
                                workspace.Documents
                                (renderedModuleIdentity moduleDump.Name)
                                moduleDump.SourceFile
                                (MissingImportedBackendModule("KBackendIR", moduleDump.Name, importedName))
                    | _ ->
                        ()

                let duplicateFunctions =
                    moduleDump.Functions
                    |> List.groupBy (fun binding -> binding.Name)
                    |> List.filter (fun (_, bindings) -> List.length bindings > 1)

                for functionName, bindings in duplicateFunctions do
                    let locations =
                        bindings
                        |> List.choose (fun binding -> provenancePrimaryLocation workspace.Documents binding.Provenance)

                    yield
                        makeStructuredDuplicateLocationDiagnostic
                            (DuplicateBackendFunctionIdentity("KBackendIR", moduleDump.Name, functionName))
                            "also declared here"
                            locations

                let duplicateDataLayouts =
                    moduleDump.DataLayouts
                    |> List.groupBy (fun layout -> layout.TypeName)
                    |> List.filter (fun (_, layouts) -> List.length layouts > 1)

                for typeName, layouts in duplicateDataLayouts do
                    let locations =
                        layouts
                        |> List.choose (fun layout -> provenancePrimaryLocation workspace.Documents layout.Provenance)

                    yield
                        makeStructuredDuplicateLocationDiagnostic
                            (DuplicateBackendDataLayoutIdentity("KBackendIR", moduleDump.Name, typeName))
                            "also declared here"
                            locations

                let duplicateEnvironmentLayouts =
                    moduleDump.EnvironmentLayouts
                    |> List.countBy (fun layout -> layout.Name)
                    |> List.filter (fun (_, count) -> count > 1)

                for layoutName, _ in duplicateEnvironmentLayouts do
                    yield
                        makeStructuredModuleDiagnostic
                            workspace.Documents
                            (renderedModuleIdentity moduleDump.Name)
                            moduleDump.SourceFile
                            (DuplicateBackendEnvironmentLayoutIdentity("KBackendIR", moduleDump.Name, layoutName))

                let supportedIntrinsics = availableIntrinsicTerms workspace.Backend workspace.AllowUnsafeConsume moduleDump.Name
                let unsupportedIntrinsicTerms = moduleDump.IntrinsicTerms |> List.filter (fun intrinsicName -> not (supportedIntrinsics.Contains intrinsicName))
                let unsupportedIntrinsicTermSet = unsupportedIntrinsicTerms |> Set.ofList

                for intrinsicName in unsupportedIntrinsicTerms do
                    yield
                        makeStructuredModuleDiagnostic
                            workspace.Documents
                            (renderedModuleIdentity moduleDump.Name)
                            moduleDump.SourceFile
                            (UnsupportedBackendIntrinsicTerm("KBackendIR", moduleDump.Name, intrinsicName, backendProfile))

                for entryPointName in moduleDump.EntryPoints do
                    match moduleDump.Functions |> List.tryFind (fun binding -> String.Equals(binding.Name, entryPointName, StringComparison.Ordinal)) with
                    | None ->
                        yield
                            makeStructuredModuleDiagnostic
                                workspace.Documents
                                (renderedModuleIdentity moduleDump.Name)
                                moduleDump.SourceFile
                                (ListedBackendEntryPointMustExistAsFunction("KBackendIR", moduleDump.Name, entryPointName))
                    | Some binding when not binding.EntryPoint ->
                        yield
                            makeStructuredOriginDiagnostic
                                workspace.Documents
                                binding.Provenance
                                (ListedBackendEntryPointMustBeMarked("KBackendIR", $"{moduleDump.Name}.{entryPointName}"))
                    | Some _ ->
                        ()

                for binding in moduleDump.Functions do
                    let bindingLabel = $"{moduleDump.Name}.{binding.Name}"

                    if binding.CallingConvention.RuntimeArity <> List.length binding.Parameters then
                        yield
                            makeStructuredOriginDiagnostic
                                workspace.Documents
                                binding.Provenance
                                (BackendFunctionCallingConventionArityMismatch("KBackendIR", bindingLabel))

                    if binding.CallingConvention.ParameterRepresentations <> (binding.Parameters |> List.map (fun parameter -> parameter.Representation)) then
                        yield
                            makeStructuredOriginDiagnostic
                                workspace.Documents
                                binding.Provenance
                                (BackendFunctionParameterRepresentationsMismatch("KBackendIR", bindingLabel))

                    match binding.EnvironmentLayout with
                    | Some layoutName when not (environmentLayoutNames moduleDump |> Set.contains layoutName) ->
                        yield
                            makeStructuredOriginDiagnostic
                                workspace.Documents
                                binding.Provenance
                                (BackendFunctionEnvironmentLayoutMissing("KBackendIR", bindingLabel, layoutName, moduleDump.Name))
                    | _ ->
                        ()

                    if binding.EntryPoint then
                        if not binding.Exported then
                            yield
                                makeStructuredOriginDiagnostic
                                    workspace.Documents
                                    binding.Provenance
                                    (BackendEntryPointMustBeExported("KBackendIR", bindingLabel))

                        if not (List.contains binding.Name moduleDump.EntryPoints) then
                            yield
                                makeStructuredOriginDiagnostic
                                    workspace.Documents
                                    binding.Provenance
                                    (BackendMarkedEntryPointMustBeListed("KBackendIR", bindingLabel))

                    if binding.Intrinsic then
                        if binding.Body.IsSome then
                            yield
                                makeStructuredOriginDiagnostic
                                    workspace.Documents
                                    binding.Provenance
                                    (IntrinsicBackendFunctionMustOmitBody("KBackendIR", bindingLabel))

                        if not (List.contains binding.Name moduleDump.IntrinsicTerms) then
                            yield
                                makeStructuredOriginDiagnostic
                                    workspace.Documents
                                    binding.Provenance
                                    (IntrinsicBackendFunctionMustBeListedInModuleIntrinsicTerms("KBackendIR", bindingLabel))

                        if
                            not (supportedIntrinsics.Contains binding.Name)
                            && not (Set.contains binding.Name unsupportedIntrinsicTermSet)
                        then
                            yield
                                makeStructuredOriginDiagnostic
                                    workspace.Documents
                                    binding.Provenance
                                    (UnsupportedBackendIntrinsicFunction("KBackendIR", moduleDump.Name, binding.Name, backendProfile))
                    else
                        match binding.Body with
                        | None ->
                            if not (isGeneratedHostBinding binding.Provenance) then
                                yield
                                    makeStructuredOriginDiagnostic
                                        workspace.Documents
                                        binding.Provenance
                                        (BackendFunctionMustHaveBody("KBackendIR", bindingLabel))
                        | Some body ->
                            yield! verifyBackendExpression moduleDump bindingLabel binding.Provenance body
        ]

    let verifyCheckpoint (workspace: WorkspaceCompilation) checkpoint =
        let diagnostics =
            match checkpoint with
            | "surface-source" ->
                verifySurfaceSource workspace
            | "KCore" ->
                verifyKCoreCheckpoint workspace
            | "KRuntimeIR" ->
                verifyKRuntimeIRCheckpoint workspace
            | "KBackendIR" ->
                verifyKBackendIRCheckpoint workspace
            | _ ->
                match tryParseCheckpoint checkpoint with
                | Some(Some phase) ->
                    let snapshotWorkspace = frontendWorkspaceForPhase workspace phase
                    verifyFrontendCheckpoint snapshotWorkspace checkpoint phase
                | Some None ->
                    verifySurfaceSource workspace
                | None ->
                    [ makeStructuredDiagnostic (UnknownVerificationCheckpoint checkpoint) ]

        diagnostics |> List.distinct
