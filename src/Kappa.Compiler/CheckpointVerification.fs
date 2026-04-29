namespace Kappa.Compiler

open System

// Checks published checkpoints against the compiler's observable pipeline contract.
module CheckpointVerification =
    let private makeDiagnostic message =
        { Severity = Error
          Code = DiagnosticCode.CheckpointVerification
          Stage = Some "checkpoint-verification"
          Phase = None
          Message = message
          Location = None
          RelatedLocations = [] }

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

    let private moduleOriginRelatedLocations (documents: ParsedDocument list) moduleName filePath =
        fileOriginLocation documents filePath
        |> Option.map (fun location -> [ relatedLocation $"module origin: {moduleName}" location ])
        |> Option.defaultValue []

    let private makeModuleDiagnostic (documents: ParsedDocument list) moduleName filePath message =
        let location = fileOriginLocation documents filePath

        { makeDiagnostic message with
            Location = location
            RelatedLocations = moduleOriginRelatedLocations documents moduleName filePath }

    let private makeOriginDiagnostic (documents: ParsedDocument list) (origin: KCoreOrigin) message =
        let location =
            provenancePrimaryLocation documents origin
            |> Option.orElseWith (fun () -> fileOriginLocation documents origin.FilePath)

        { makeDiagnostic message with
            Location = location
            RelatedLocations = moduleOriginRelatedLocations documents origin.ModuleName origin.FilePath }

    let private makeDuplicateLocationDiagnostic message role locations =
        match locations with
        | head :: tail ->
            { makeDiagnostic message with
                Location = Some head
                RelatedLocations = tail |> List.map (relatedLocation role) }
        | [] ->
            makeDiagnostic message

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
                    makeDuplicateLocationDiagnostic
                        $"Checkpoint 'surface-source' requires unique file identities, but '{filePath}' appeared more than once."
                        "also appears here"
                        locations

            for document in workspace.Documents do
                if document.Source.LineCount <= 0 then
                    yield
                        makeModuleDiagnostic
                            workspace.Documents
                            (document.ModuleName |> Option.map SyntaxFacts.moduleNameToText |> Option.defaultValue document.Source.FilePath)
                            document.Source.FilePath
                            $"Checkpoint 'surface-source' requires non-empty line tables for '{document.Source.FilePath}'."
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
                    makeDuplicateLocationDiagnostic
                        $"Checkpoint '{checkpoint}' requires unique file identities, but '{filePath}' appeared more than once."
                        "also appears here"
                        locations

            for document in workspace.KFrontIR do
                match List.tryLast document.Tokens with
                | Some token when token.Kind = EndOfFile -> ()
                | _ ->
                    yield
                        makeModuleDiagnostic
                            workspace.Documents
                            (document.ModuleIdentity |> Option.map SyntaxFacts.moduleNameToText |> Option.defaultValue document.FilePath)
                            document.FilePath
                            $"Checkpoint '{checkpoint}' requires an EOF token for '{document.FilePath}'."

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
                    let actual =
                        document.ResolvedPhases
                        |> Set.toList
                        |> List.map KFrontIRPhase.phaseName
                        |> String.concat ", "

                    let expected =
                        expectedResolvedPhases
                        |> Set.toList
                        |> List.map KFrontIRPhase.phaseName
                        |> String.concat ", "

                    yield
                        makeModuleDiagnostic
                            workspace.Documents
                            (document.ModuleIdentity |> Option.map SyntaxFacts.moduleNameToText |> Option.defaultValue document.FilePath)
                            document.FilePath
                            $"Checkpoint '{checkpoint}' requires '{document.FilePath}' to expose resolved phases [{expected}], but found [{actual}]."

                if
                    KFrontIRPhase.ordinal phase < KFrontIRPhase.ordinal BODY_RESOLVE
                    && document.Ownership.IsSome
                then
                    yield
                        makeModuleDiagnostic
                            workspace.Documents
                            (document.ModuleIdentity |> Option.map SyntaxFacts.moduleNameToText |> Option.defaultValue document.FilePath)
                            document.FilePath
                            $"Checkpoint '{checkpoint}' must not expose BODY_RESOLVE ownership facts for '{document.FilePath}' before BODY_RESOLVE."
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
                    makeDuplicateLocationDiagnostic
                        $"Checkpoint 'KCore' requires unique module identities, but '{moduleName}' appeared more than once."
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
                    [ makeOriginDiagnostic documents bindingOrigin $"Checkpoint '{checkpoint}' requires unique pattern binder names within '{bindingLabel}', but '{name}' was duplicated." ]
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
                || String.Equals(head, "Constraint", StringComparison.Ordinal)
                || String.Equals(head, "Quantity", StringComparison.Ordinal)
                || String.Equals(head, "Region", StringComparison.Ordinal)
                || String.Equals(head, "RecRow", StringComparison.Ordinal)
                || String.Equals(head, "Label", StringComparison.Ordinal)
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
            || typeText.Contains("Region", StringComparison.Ordinal)
            || typeText.Contains("Constraint", StringComparison.Ordinal)
            || typeText.Contains("Quantity", StringComparison.Ordinal)
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
                        makeOriginDiagnostic documents bindingOrigin $"Checkpoint '{checkpoint}' requires closures in '{bindingLabel}' to have unique parameter names, but '{name}' was duplicated.")

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
                        [ makeOriginDiagnostic documents bindingOrigin $"Checkpoint '{checkpoint}' requires direct deep-handle runtime nodes in '{bindingLabel}' to be desugared into the recursive shallow-handler driver before KRuntimeIR verification." ]
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
                            makeModuleDiagnostic
                                workspace.Documents
                                moduleDump.Name
                                moduleDump.SourceFile
                                $"Checkpoint 'KRuntimeIR' requires imported runtime module '{importedName}' to be present for module '{moduleDump.Name}'."
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
                        makeDuplicateLocationDiagnostic
                            $"Checkpoint 'KRuntimeIR' requires unique binding identities within module '{moduleDump.Name}', but '{bindingName}' was duplicated."
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
                        makeDuplicateLocationDiagnostic
                            $"Checkpoint 'KRuntimeIR' requires unique constructor identities within module '{moduleDump.Name}', but '{constructorName}' was duplicated."
                            "also declared here"
                            locations

                let supportedIntrinsics = availableIntrinsicTerms workspace.Backend workspace.AllowUnsafeConsume moduleDump.Name

                for intrinsicName in moduleDump.IntrinsicTerms do
                    if not (supportedIntrinsics.Contains intrinsicName) then
                        yield
                            makeModuleDiagnostic
                                workspace.Documents
                                moduleDump.Name
                                moduleDump.SourceFile
                                $"Checkpoint 'KRuntimeIR' requires intrinsic term '{intrinsicName}' in module '{moduleDump.Name}' to be provided by backend profile '{BackendProfile.toPortableName workspace.Backend}'."

                for binding in moduleDump.Bindings do
                    if binding.Intrinsic then
                        if binding.Body.IsSome then
                            yield
                                makeOriginDiagnostic
                                    workspace.Documents
                                    binding.Provenance
                                    $"Checkpoint 'KRuntimeIR' requires intrinsic binding '{moduleDump.Name}.{binding.Name}' to omit a body."

                        if not (List.contains binding.Name moduleDump.IntrinsicTerms) then
                            yield
                                makeOriginDiagnostic
                                    workspace.Documents
                                    binding.Provenance
                                    $"Checkpoint 'KRuntimeIR' requires intrinsic binding '{moduleDump.Name}.{binding.Name}' to be listed in module intrinsic terms."

                        if not (supportedIntrinsics.Contains binding.Name) then
                            yield
                                makeOriginDiagnostic
                                    workspace.Documents
                                    binding.Provenance
                                    $"Checkpoint 'KRuntimeIR' requires intrinsic term '{binding.Name}' in module '{moduleDump.Name}' to be provided by backend profile '{BackendProfile.toPortableName workspace.Backend}'."
                    else
                        match binding.Body with
                        | None ->
                            if not (isGeneratedHostBinding binding.Provenance) then
                                yield
                                    makeOriginDiagnostic
                                        workspace.Documents
                                        binding.Provenance
                                        $"Checkpoint 'KRuntimeIR' requires runtime binding '{moduleDump.Name}.{binding.Name}' to have a body."
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
                                    @ [ makeOriginDiagnostic documents bindingOrigin $"Checkpoint '{checkpoint}' requires unique pattern binder names within '{bindingLabel}', but '{name}' was duplicated." ]
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
                                        @ [ makeOriginDiagnostic documents bindingOrigin $"Checkpoint '{checkpoint}' requires each or-pattern alternative within '{bindingLabel}' to bind the same names." ]
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
                                            @ [ makeOriginDiagnostic documents bindingOrigin $"Checkpoint '{checkpoint}' requires binder '{name}' within '{bindingLabel}' to keep the same backend representation across every or-pattern alternative." ]
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
                        [ makeOriginDiagnostic documents bindingOrigin $"Checkpoint '{checkpoint}' requires resolved constructor patterns, but '{constructorText}' in '{bindingLabel}' is not present in the backend module graph." ]

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
                                    makeOriginDiagnostic
                                        workspace.Documents
                                        binding.Provenance
                                        $"Checkpoint 'KBackendIR' requires pre-erasure runtime metadata to be removed before backend lowering, but parameter '{runtimeModule.Name}.{binding.Name}.{parameter.Name}' still exposes '{typeText}'."
                            | _ ->
                                ()

                        match binding.ReturnTypeText with
                        | Some typeText when runtimeTypeLeaksErasureMetadata allowEffectRowMetadata typeText ->
                            yield
                                makeOriginDiagnostic
                                    workspace.Documents
                                    binding.Provenance
                                    $"Checkpoint 'KBackendIR' requires pre-erasure runtime metadata to be removed before backend lowering, but return type of '{runtimeModule.Name}.{binding.Name}' still exposes '{typeText}'."
                        | _ ->
                            ()

                    for dataType in runtimeModule.DataTypes do
                        for constructor in dataType.Constructors do
                            for index, fieldTypeText in constructor.FieldTypeTexts |> List.indexed do
                                if runtimeTypeLeaksErasureMetadata allowEffectRowMetadata fieldTypeText then
                                    yield
                                        makeOriginDiagnostic
                                            workspace.Documents
                                            constructor.Provenance
                                            $"Checkpoint 'KBackendIR' requires pre-erasure runtime metadata to be removed before backend lowering, but constructor field '{runtimeModule.Name}.{constructor.Name}[{index}]' still exposes '{fieldTypeText}'."

                    for instanceInfo in runtimeModule.TraitInstances do
                        for index, headTypeText in instanceInfo.HeadTypeTexts |> List.indexed do
                            if runtimeTypeLeaksErasureMetadata allowEffectRowMetadata headTypeText then
                                yield
                                    makeModuleDiagnostic
                                        workspace.Documents
                                        runtimeModule.Name
                                        runtimeModule.SourceFile
                                        $"Checkpoint 'KBackendIR' requires pre-erasure runtime metadata to be removed before backend lowering, but instance head '{runtimeModule.Name}.{instanceInfo.TraitName}[{index}]' still exposes '{headTypeText}'."
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
                    [ makeOriginDiagnostic workspace.Documents bindingOrigin $"Checkpoint 'KBackendIR' requires resolved runtime names, but '{resolvedNameText resolvedName}' in '{bindingLabel}' is not present in the backend module graph." ]
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
                        makeOriginDiagnostic workspace.Documents bindingOrigin $"Checkpoint 'KBackendIR' requires closures in '{bindingLabel}' to have unique parameter names, but '{name}' was duplicated.")

                let duplicateCaptures =
                    captures
                    |> List.countBy (fun capture -> capture.Name)
                    |> List.filter (fun (_, count) -> count > 1)
                    |> List.map (fun (name, _) ->
                        makeOriginDiagnostic workspace.Documents bindingOrigin $"Checkpoint 'KBackendIR' requires closures in '{bindingLabel}' to have unique capture names, but '{name}' was duplicated.")

                let environmentDiagnostics =
                    if environmentLayoutNames currentModule |> Set.contains environmentLayout then
                        []
                    else
                        [ makeOriginDiagnostic workspace.Documents bindingOrigin $"Checkpoint 'KBackendIR' requires closure environment layout '{environmentLayout}' in '{bindingLabel}' to be present in module '{currentModule.Name}'." ]

                let conventionDiagnostics =
                    [
                        if convention.RuntimeArity <> List.length parameters then
                            yield makeOriginDiagnostic workspace.Documents bindingOrigin $"Checkpoint 'KBackendIR' requires closure '{bindingLabel}' to have a calling convention arity matching its parameter count."

                        if convention.ParameterRepresentations <> (parameters |> List.map (fun parameter -> parameter.Representation)) then
                            yield makeOriginDiagnostic workspace.Documents bindingOrigin $"Checkpoint 'KBackendIR' requires closure '{bindingLabel}' to have calling convention parameter representations that match its parameters."

                        match representation with
                        | BackendRepClosure layoutName when not (String.Equals(layoutName, environmentLayout, StringComparison.Ordinal)) ->
                            yield makeOriginDiagnostic workspace.Documents bindingOrigin $"Checkpoint 'KBackendIR' requires closure '{bindingLabel}' to have a representation that references environment layout '{environmentLayout}'."
                        | BackendRepClosure _ -> ()
                        | _ ->
                            yield makeOriginDiagnostic workspace.Documents bindingOrigin $"Checkpoint 'KBackendIR' requires closure '{bindingLabel}' to use a closure representation."
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
                            yield makeOriginDiagnostic workspace.Documents bindingOrigin $"Checkpoint 'KBackendIR' requires calls in '{bindingLabel}' to have an argument count matching the calling convention arity."

                        if List.length convention.ParameterRepresentations <> List.length arguments then
                            yield makeOriginDiagnostic workspace.Documents bindingOrigin $"Checkpoint 'KBackendIR' requires calls in '{bindingLabel}' to have a parameter representation for each argument."
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
                        [ makeOriginDiagnostic workspace.Documents bindingOrigin $"Checkpoint 'KBackendIR' requires constructed data '{constructorText}' in '{bindingLabel}' to match a backend data layout." ]

                constructorDiagnostics @ (fields |> List.collect (verifyBackendExpression currentModule bindingLabel bindingOrigin))
            | BackendPrefixedString(_, parts, _) ->
                parts
                |> List.collect (function
                    | BackendStringText _ ->
                        []
                    | BackendStringInterpolation inner ->
                        verifyBackendExpression currentModule bindingLabel bindingOrigin inner)

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
                            makeModuleDiagnostic
                                workspace.Documents
                                runtimeModule.Name
                                runtimeModule.SourceFile
                                $"Checkpoint 'KBackendIR' requires a backend module for runtime module '{runtimeModuleName}'."
                    | None ->
                        yield makeDiagnostic $"Checkpoint 'KBackendIR' requires a backend module for runtime module '{runtimeModuleName}'."

            for moduleDump in backendModules do
                for spec in moduleDump.Imports do
                    match importedModuleName spec with
                    | Some importedName when not (moduleMap.ContainsKey importedName) ->
                        yield
                            makeModuleDiagnostic
                                workspace.Documents
                                moduleDump.Name
                                moduleDump.SourceFile
                                $"Checkpoint 'KBackendIR' requires imported backend module '{importedName}' to be present for module '{moduleDump.Name}'."
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
                        makeDuplicateLocationDiagnostic
                            $"Checkpoint 'KBackendIR' requires unique function identities within module '{moduleDump.Name}', but '{functionName}' was duplicated."
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
                        makeDuplicateLocationDiagnostic
                            $"Checkpoint 'KBackendIR' requires unique data-layout identities within module '{moduleDump.Name}', but '{typeName}' was duplicated."
                            "also declared here"
                            locations

                let duplicateEnvironmentLayouts =
                    moduleDump.EnvironmentLayouts
                    |> List.countBy (fun layout -> layout.Name)
                    |> List.filter (fun (_, count) -> count > 1)

                for layoutName, _ in duplicateEnvironmentLayouts do
                    yield
                        makeModuleDiagnostic
                            workspace.Documents
                            moduleDump.Name
                            moduleDump.SourceFile
                            $"Checkpoint 'KBackendIR' requires unique environment-layout identities within module '{moduleDump.Name}', but '{layoutName}' was duplicated."

                let supportedIntrinsics = availableIntrinsicTerms workspace.Backend workspace.AllowUnsafeConsume moduleDump.Name

                for intrinsicName in moduleDump.IntrinsicTerms do
                    if not (supportedIntrinsics.Contains intrinsicName) then
                        yield
                            makeModuleDiagnostic
                                workspace.Documents
                                moduleDump.Name
                                moduleDump.SourceFile
                                $"Checkpoint 'KBackendIR' requires intrinsic term '{intrinsicName}' in module '{moduleDump.Name}' to be provided by backend profile '{BackendProfile.toPortableName workspace.Backend}'."

                for entryPointName in moduleDump.EntryPoints do
                    match moduleDump.Functions |> List.tryFind (fun binding -> String.Equals(binding.Name, entryPointName, StringComparison.Ordinal)) with
                    | None ->
                        yield
                            makeModuleDiagnostic
                                workspace.Documents
                                moduleDump.Name
                                moduleDump.SourceFile
                                $"Checkpoint 'KBackendIR' requires listed entry point '{moduleDump.Name}.{entryPointName}' to be present as a function."
                    | Some binding when not binding.EntryPoint ->
                        yield
                            makeOriginDiagnostic
                                workspace.Documents
                                binding.Provenance
                                $"Checkpoint 'KBackendIR' requires listed entry point '{moduleDump.Name}.{entryPointName}' to be marked as an entry point."
                    | Some _ ->
                        ()

                for binding in moduleDump.Functions do
                    let bindingLabel = $"{moduleDump.Name}.{binding.Name}"

                    if binding.CallingConvention.RuntimeArity <> List.length binding.Parameters then
                        yield
                            makeOriginDiagnostic
                                workspace.Documents
                                binding.Provenance
                                $"Checkpoint 'KBackendIR' requires function '{bindingLabel}' to have a calling convention arity matching its parameter count."

                    if binding.CallingConvention.ParameterRepresentations <> (binding.Parameters |> List.map (fun parameter -> parameter.Representation)) then
                        yield
                            makeOriginDiagnostic
                                workspace.Documents
                                binding.Provenance
                                $"Checkpoint 'KBackendIR' requires function '{bindingLabel}' to have calling convention parameter representations that match its parameters."

                    match binding.EnvironmentLayout with
                    | Some layoutName when not (environmentLayoutNames moduleDump |> Set.contains layoutName) ->
                        yield
                            makeOriginDiagnostic
                                workspace.Documents
                                binding.Provenance
                                $"Checkpoint 'KBackendIR' requires function '{bindingLabel}' to reference an environment layout present in module '{moduleDump.Name}'."
                    | _ ->
                        ()

                    if binding.EntryPoint then
                        if not binding.Exported then
                            yield
                                makeOriginDiagnostic
                                    workspace.Documents
                                    binding.Provenance
                                    $"Checkpoint 'KBackendIR' requires entry point '{bindingLabel}' to be exported."

                        if not (List.contains binding.Name moduleDump.EntryPoints) then
                            yield
                                makeOriginDiagnostic
                                    workspace.Documents
                                    binding.Provenance
                                    $"Checkpoint 'KBackendIR' requires function '{bindingLabel}' marked as an entry point to be listed in module entry points."

                    if binding.Intrinsic then
                        if binding.Body.IsSome then
                            yield
                                makeOriginDiagnostic
                                    workspace.Documents
                                    binding.Provenance
                                    $"Checkpoint 'KBackendIR' requires intrinsic function '{bindingLabel}' to omit a body."

                        if not (List.contains binding.Name moduleDump.IntrinsicTerms) then
                            yield
                                makeOriginDiagnostic
                                    workspace.Documents
                                    binding.Provenance
                                    $"Checkpoint 'KBackendIR' requires intrinsic function '{bindingLabel}' to be listed in module intrinsic terms."

                        if not (supportedIntrinsics.Contains binding.Name) then
                            yield
                                makeOriginDiagnostic
                                    workspace.Documents
                                    binding.Provenance
                                    $"Checkpoint 'KBackendIR' requires intrinsic term '{binding.Name}' in module '{moduleDump.Name}' to be provided by backend profile '{BackendProfile.toPortableName workspace.Backend}'."
                    else
                        match binding.Body with
                        | None ->
                            if not (isGeneratedHostBinding binding.Provenance) then
                                yield
                                    makeOriginDiagnostic
                                        workspace.Documents
                                        binding.Provenance
                                        $"Checkpoint 'KBackendIR' requires function '{bindingLabel}' to have a body."
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
                    [ makeDiagnostic $"Unknown checkpoint '{checkpoint}'." ]

        diagnostics |> List.distinct
