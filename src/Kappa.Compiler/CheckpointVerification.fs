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
            |> List.countBy (fun document -> document.Source.FilePath)
            |> List.filter (fun (_, count) -> count > 1)

        [
            for filePath, _ in duplicatePaths do
                yield makeDiagnostic $"Checkpoint 'surface-source' requires unique file identities, but '{filePath}' appeared more than once."

            for document in workspace.Documents do
                if document.Source.LineCount <= 0 then
                    yield makeDiagnostic $"Checkpoint 'surface-source' requires non-empty line tables for '{document.Source.FilePath}'."
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
        let expectedResolvedPhases =
            KFrontIRPhase.phasesThrough phase |> Set.ofList

        [
            let duplicatePaths =
                workspace.KFrontIR
                |> List.countBy (fun document -> document.FilePath)
                |> List.filter (fun (_, count) -> count > 1)

            for filePath, _ in duplicatePaths do
                yield makeDiagnostic $"Checkpoint '{checkpoint}' requires unique file identities, but '{filePath}' appeared more than once."

            for document in workspace.KFrontIR do
                match List.tryLast document.Tokens with
                | Some token when token.Kind = EndOfFile -> ()
                | _ ->
                    yield makeDiagnostic $"Checkpoint '{checkpoint}' requires an EOF token for '{document.FilePath}'."

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
                        makeDiagnostic
                            $"Checkpoint '{checkpoint}' requires '{document.FilePath}' to expose resolved phases [{expected}], but found [{actual}]."

                if
                    KFrontIRPhase.ordinal phase < KFrontIRPhase.ordinal BODY_RESOLVE
                    && document.Ownership.IsSome
                then
                    yield
                        makeDiagnostic
                            $"Checkpoint '{checkpoint}' must not expose BODY_RESOLVE ownership facts for '{document.FilePath}' before BODY_RESOLVE."
        ]

    let private verifyKCoreCheckpoint (workspace: WorkspaceCompilation) =
        let duplicateModules =
            workspace.KCore
            |> List.map (fun moduleDump -> moduleDump.Name)
            |> List.countBy id
            |> List.filter (fun (_, count) -> count > 1)

        [
            let frontendWorkspace = frontendWorkspaceForPhase workspace CORE_LOWERING
            yield! verifyFrontendCheckpoint frontendWorkspace "KCore" CORE_LOWERING

            for moduleName, _ in duplicateModules do
                yield makeDiagnostic $"Checkpoint 'KCore' requires unique module identities, but '{moduleName}' appeared more than once."
        ]

    let private importedModuleName (spec: ImportSpec) : string option =
        match spec.Source with
        | Dotted moduleSegments ->
            Some(SyntaxFacts.moduleNameToText moduleSegments)
        | Url _ ->
            None

    let private availableIntrinsicTerms backendProfile allowUnsafeConsume moduleName =
        Stdlib.intrinsicTermNamesAvailableInModuleTextForCompilation backendProfile allowUnsafeConsume moduleName

    let private verifyRuntimePattern checkpoint bindingLabel (pattern: KRuntimePattern) =
        let rec verify locals runtimePattern =
            match runtimePattern with
            | KRuntimeWildcardPattern
            | KRuntimeLiteralPattern _ ->
                locals, []
            | KRuntimeNamePattern name ->
                if Set.contains name locals then
                    locals,
                    [ makeDiagnostic $"Checkpoint '{checkpoint}' requires unique pattern binder names within '{bindingLabel}', but '{name}' was duplicated." ]
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

    let private runtimeTypeLeaksErasureMetadata (typeText: string) =
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
                true
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

    let private verifyRuntimeExpression checkpoint bindingLabel (expression: KRuntimeExpression) =
        let rec verify locals runtimeExpression =
            match runtimeExpression with
            | KRuntimeLiteral _
            | KRuntimeName _ ->
                []
            | KRuntimeEffectLabel _ ->
                []
            | KRuntimeEffectOperation(label, _) ->
                verify locals label
            | KRuntimeClosure(parameters, body) ->
                let duplicateParameters =
                    parameters
                    |> List.countBy id
                    |> List.filter (fun (_, count) -> count > 1)
                    |> List.map (fun (name, _) ->
                        makeDiagnostic $"Checkpoint '{checkpoint}' requires closures in '{bindingLabel}' to have unique parameter names, but '{name}' was duplicated.")

                let extendedLocals =
                    parameters
                    |> List.fold (fun state parameterName -> Set.add parameterName state) locals

                duplicateParameters @ verify extendedLocals body
            | KRuntimeIfThenElse(condition, whenTrue, whenFalse) ->
                verify locals condition @ verify locals whenTrue @ verify locals whenFalse
            | KRuntimeHandle(_, label, body, returnClause, operationClauses) ->
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

                verify locals label
                @ verify locals body
                @ verifyClause returnClause
                @ (operationClauses |> List.collect verifyClause)
            | KRuntimeMatch(scrutinee, cases) ->
                verify locals scrutinee
                @ (cases
                   |> List.collect (fun (caseClause: KRuntimeMatchCase) ->
                       let caseLocals, patternDiagnostics =
                           verifyRuntimePattern checkpoint bindingLabel caseClause.Pattern

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
                        yield makeDiagnostic $"Checkpoint 'KRuntimeIR' requires imported runtime module '{importedName}' to be present for module '{moduleDump.Name}'."
                    | _ ->
                        ()

                let duplicateBindings =
                    moduleDump.Bindings
                    |> List.countBy (fun binding -> binding.Name)
                    |> List.filter (fun (_, count) -> count > 1)

                for bindingName, _ in duplicateBindings do
                    yield makeDiagnostic $"Checkpoint 'KRuntimeIR' requires unique binding identities within module '{moduleDump.Name}', but '{bindingName}' was duplicated."

                let duplicateConstructors =
                    moduleDump.Constructors
                    |> List.countBy (fun constructor -> constructor.Name)
                    |> List.filter (fun (_, count) -> count > 1)

                for constructorName, _ in duplicateConstructors do
                    yield makeDiagnostic $"Checkpoint 'KRuntimeIR' requires unique constructor identities within module '{moduleDump.Name}', but '{constructorName}' was duplicated."

                let supportedIntrinsics = availableIntrinsicTerms workspace.BackendProfile workspace.AllowUnsafeConsume moduleDump.Name

                for intrinsicName in moduleDump.IntrinsicTerms do
                    if not (supportedIntrinsics.Contains intrinsicName) then
                        yield makeDiagnostic $"Checkpoint 'KRuntimeIR' requires intrinsic term '{intrinsicName}' in module '{moduleDump.Name}' to be provided by backend profile '{workspace.BackendProfile}'."

                for binding in moduleDump.Bindings do
                    if binding.Intrinsic then
                        if binding.Body.IsSome then
                            yield makeDiagnostic $"Checkpoint 'KRuntimeIR' requires intrinsic binding '{moduleDump.Name}.{binding.Name}' to omit a body."

                        if not (List.contains binding.Name moduleDump.IntrinsicTerms) then
                            yield makeDiagnostic $"Checkpoint 'KRuntimeIR' requires intrinsic binding '{moduleDump.Name}.{binding.Name}' to be listed in module intrinsic terms."

                        if not (supportedIntrinsics.Contains binding.Name) then
                            yield makeDiagnostic $"Checkpoint 'KRuntimeIR' requires intrinsic term '{binding.Name}' in module '{moduleDump.Name}' to be provided by backend profile '{workspace.BackendProfile}'."
                    else
                        match binding.Body with
                        | None ->
                            if not (isGeneratedHostBinding binding.Provenance) then
                                yield makeDiagnostic $"Checkpoint 'KRuntimeIR' requires runtime binding '{moduleDump.Name}.{binding.Name}' to have a body."
                        | Some body ->
                            let bindingLabel = $"{moduleDump.Name}.{binding.Name}"
                            yield! verifyRuntimeExpression "KRuntimeIR" bindingLabel body
        ]

    let private verifyKBackendPattern
        (moduleMap: Map<string, KBackendModule>)
        checkpoint
        bindingLabel
        (pattern: KBackendPattern)
        =
        let rec verify locals backendPattern =
            match backendPattern with
            | BackendWildcardPattern
            | BackendLiteralPattern _ ->
                locals, []
            | BackendBindPattern binding ->
                if Set.contains binding.Name locals then
                    locals,
                    [ makeDiagnostic $"Checkpoint '{checkpoint}' requires unique pattern binder names within '{bindingLabel}', but '{binding.Name}' was duplicated." ]
                else
                    Set.add binding.Name locals, []
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
                        [ makeDiagnostic $"Checkpoint '{checkpoint}' requires resolved constructor patterns, but '{constructorText}' in '{bindingLabel}' is not present in the backend module graph." ]

                ((locals, diagnostics), fieldPatterns)
                ||> List.fold (fun (localsSoFar, diagnosticsSoFar) fieldPattern ->
                    let nextLocals, nextDiagnostics = verify localsSoFar fieldPattern
                    nextLocals, diagnosticsSoFar @ nextDiagnostics)

        verify Set.empty pattern

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
            Stdlib.runtimeIntrinsicTermNamesForCompilation
                workspace.BackendProfile
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

        let erasureDiagnostics =
            [
                for runtimeModule in workspace.KRuntimeIR do
                    for binding in runtimeModule.Bindings do
                        for parameter in binding.Parameters do
                            match parameter.TypeText with
                            | Some typeText when runtimeTypeLeaksErasureMetadata typeText ->
                                yield
                                    makeDiagnostic
                                        $"Checkpoint 'KBackendIR' requires pre-erasure runtime metadata to be removed before backend lowering, but parameter '{runtimeModule.Name}.{binding.Name}.{parameter.Name}' still exposes '{typeText}'."
                            | _ ->
                                ()

                        match binding.ReturnTypeText with
                        | Some typeText when runtimeTypeLeaksErasureMetadata typeText ->
                            yield
                                makeDiagnostic
                                    $"Checkpoint 'KBackendIR' requires pre-erasure runtime metadata to be removed before backend lowering, but return type of '{runtimeModule.Name}.{binding.Name}' still exposes '{typeText}'."
                        | _ ->
                            ()

                    for dataType in runtimeModule.DataTypes do
                        for constructor in dataType.Constructors do
                            for index, fieldTypeText in constructor.FieldTypeTexts |> List.indexed do
                                if runtimeTypeLeaksErasureMetadata fieldTypeText then
                                    yield
                                        makeDiagnostic
                                            $"Checkpoint 'KBackendIR' requires pre-erasure runtime metadata to be removed before backend lowering, but constructor field '{runtimeModule.Name}.{constructor.Name}[{index}]' still exposes '{fieldTypeText}'."

                    for instanceInfo in runtimeModule.TraitInstances do
                        for index, headTypeText in instanceInfo.HeadTypeTexts |> List.indexed do
                            if runtimeTypeLeaksErasureMetadata headTypeText then
                                yield
                                    makeDiagnostic
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

        let rec verifyBackendExpression (currentModule: KBackendModule) bindingLabel backendExpression =
            match backendExpression with
            | BackendLiteral _ ->
                []
            | BackendName resolvedName ->
                if resolvedNameExists resolvedName then
                    []
                else
                    [ makeDiagnostic $"Checkpoint 'KBackendIR' requires resolved runtime names, but '{resolvedNameText resolvedName}' in '{bindingLabel}' is not present in the backend module graph." ]
            | BackendClosure(parameters, captures, environmentLayout, body, convention, representation) ->
                let duplicateParameters =
                    parameters
                    |> List.countBy (fun parameter -> parameter.Name)
                    |> List.filter (fun (_, count) -> count > 1)
                    |> List.map (fun (name, _) ->
                        makeDiagnostic $"Checkpoint 'KBackendIR' requires closures in '{bindingLabel}' to have unique parameter names, but '{name}' was duplicated.")

                let duplicateCaptures =
                    captures
                    |> List.countBy (fun capture -> capture.Name)
                    |> List.filter (fun (_, count) -> count > 1)
                    |> List.map (fun (name, _) ->
                        makeDiagnostic $"Checkpoint 'KBackendIR' requires closures in '{bindingLabel}' to have unique capture names, but '{name}' was duplicated.")

                let environmentDiagnostics =
                    if environmentLayoutNames currentModule |> Set.contains environmentLayout then
                        []
                    else
                        [ makeDiagnostic $"Checkpoint 'KBackendIR' requires closure environment layout '{environmentLayout}' in '{bindingLabel}' to be present in module '{currentModule.Name}'." ]

                let conventionDiagnostics =
                    [
                        if convention.RuntimeArity <> List.length parameters then
                            yield makeDiagnostic $"Checkpoint 'KBackendIR' requires closure '{bindingLabel}' to have a calling convention arity matching its parameter count."

                        if convention.ParameterRepresentations <> (parameters |> List.map (fun parameter -> parameter.Representation)) then
                            yield makeDiagnostic $"Checkpoint 'KBackendIR' requires closure '{bindingLabel}' to have calling convention parameter representations that match its parameters."

                        match representation with
                        | BackendRepClosure layoutName when not (String.Equals(layoutName, environmentLayout, StringComparison.Ordinal)) ->
                            yield makeDiagnostic $"Checkpoint 'KBackendIR' requires closure '{bindingLabel}' to have a representation that references environment layout '{environmentLayout}'."
                        | BackendRepClosure _ -> ()
                        | _ ->
                            yield makeDiagnostic $"Checkpoint 'KBackendIR' requires closure '{bindingLabel}' to use a closure representation."
                    ]

                duplicateParameters
                @ duplicateCaptures
                @ environmentDiagnostics
                @ conventionDiagnostics
                @ verifyBackendExpression currentModule bindingLabel body
            | BackendIfThenElse(condition, whenTrue, whenFalse, _) ->
                verifyBackendExpression currentModule bindingLabel condition
                @ verifyBackendExpression currentModule bindingLabel whenTrue
                @ verifyBackendExpression currentModule bindingLabel whenFalse
            | BackendMatch(scrutinee, cases, _) ->
                verifyBackendExpression currentModule bindingLabel scrutinee
                @ (cases
                   |> List.collect (fun caseClause ->
                       let _, patternDiagnostics =
                           verifyKBackendPattern moduleMap "KBackendIR" bindingLabel caseClause.Pattern

                       let guardDiagnostics =
                           caseClause.Guard
                           |> Option.map (verifyBackendExpression currentModule bindingLabel)
                           |> Option.defaultValue []

                       patternDiagnostics @ guardDiagnostics @ verifyBackendExpression currentModule bindingLabel caseClause.Body))
            | BackendExecute(expression, _) ->
                verifyBackendExpression currentModule bindingLabel expression
            | BackendLet(_, value, body, _) ->
                verifyBackendExpression currentModule bindingLabel value
                @ verifyBackendExpression currentModule bindingLabel body
            | BackendSequence(first, second, _) ->
                verifyBackendExpression currentModule bindingLabel first
                @ verifyBackendExpression currentModule bindingLabel second
            | BackendWhile(condition, body) ->
                verifyBackendExpression currentModule bindingLabel condition
                @ verifyBackendExpression currentModule bindingLabel body
            | BackendCall(callee, arguments, convention, _) ->
                let conventionDiagnostics =
                    [
                        if convention.RuntimeArity <> List.length arguments then
                            yield makeDiagnostic $"Checkpoint 'KBackendIR' requires calls in '{bindingLabel}' to have an argument count matching the calling convention arity."

                        if List.length convention.ParameterRepresentations <> List.length arguments then
                            yield makeDiagnostic $"Checkpoint 'KBackendIR' requires calls in '{bindingLabel}' to have a parameter representation for each argument."
                    ]

                conventionDiagnostics
                @ verifyBackendExpression currentModule bindingLabel callee
                @ (arguments |> List.collect (verifyBackendExpression currentModule bindingLabel))
            | BackendDictionaryValue(_, _, _, _) ->
                []
            | BackendTraitCall(_, _, dictionary, arguments, _) ->
                verifyBackendExpression currentModule bindingLabel dictionary
                @ (arguments |> List.collect (verifyBackendExpression currentModule bindingLabel))
            | BackendConstructData(moduleName, typeName, constructorName, tag, fields, _) ->
                let constructorDiagnostics =
                    if constructorExists moduleName typeName constructorName tag (List.length fields) then
                        []
                    else
                        let constructorText = $"{moduleName}.{typeName}.{constructorName}@{tag}"
                        [ makeDiagnostic $"Checkpoint 'KBackendIR' requires constructed data '{constructorText}' in '{bindingLabel}' to match a backend data layout." ]

                constructorDiagnostics @ (fields |> List.collect (verifyBackendExpression currentModule bindingLabel))
            | BackendPrefixedString(_, parts, _) ->
                parts
                |> List.collect (function
                    | BackendStringText _ ->
                        []
                    | BackendStringInterpolation inner ->
                        verifyBackendExpression currentModule bindingLabel inner)

        [
            yield! verifyKRuntimeIRCheckpoint workspace
            yield! erasureDiagnostics

            let runtimeModuleNames =
                workspace.KRuntimeIR
                |> List.map (fun moduleDump -> moduleDump.Name)
                |> Set.ofList

            for runtimeModuleName in runtimeModuleNames do
                if not (moduleMap.ContainsKey runtimeModuleName) then
                    yield makeDiagnostic $"Checkpoint 'KBackendIR' requires a backend module for runtime module '{runtimeModuleName}'."

            for moduleDump in backendModules do
                for spec in moduleDump.Imports do
                    match importedModuleName spec with
                    | Some importedName when not (moduleMap.ContainsKey importedName) ->
                        yield makeDiagnostic $"Checkpoint 'KBackendIR' requires imported backend module '{importedName}' to be present for module '{moduleDump.Name}'."
                    | _ ->
                        ()

                let duplicateFunctions =
                    moduleDump.Functions
                    |> List.countBy (fun binding -> binding.Name)
                    |> List.filter (fun (_, count) -> count > 1)

                for functionName, _ in duplicateFunctions do
                    yield makeDiagnostic $"Checkpoint 'KBackendIR' requires unique function identities within module '{moduleDump.Name}', but '{functionName}' was duplicated."

                let duplicateDataLayouts =
                    moduleDump.DataLayouts
                    |> List.countBy (fun layout -> layout.TypeName)
                    |> List.filter (fun (_, count) -> count > 1)

                for typeName, _ in duplicateDataLayouts do
                    yield makeDiagnostic $"Checkpoint 'KBackendIR' requires unique data-layout identities within module '{moduleDump.Name}', but '{typeName}' was duplicated."

                let duplicateEnvironmentLayouts =
                    moduleDump.EnvironmentLayouts
                    |> List.countBy (fun layout -> layout.Name)
                    |> List.filter (fun (_, count) -> count > 1)

                for layoutName, _ in duplicateEnvironmentLayouts do
                    yield makeDiagnostic $"Checkpoint 'KBackendIR' requires unique environment-layout identities within module '{moduleDump.Name}', but '{layoutName}' was duplicated."

                let supportedIntrinsics = availableIntrinsicTerms workspace.BackendProfile workspace.AllowUnsafeConsume moduleDump.Name

                for intrinsicName in moduleDump.IntrinsicTerms do
                    if not (supportedIntrinsics.Contains intrinsicName) then
                        yield makeDiagnostic $"Checkpoint 'KBackendIR' requires intrinsic term '{intrinsicName}' in module '{moduleDump.Name}' to be provided by backend profile '{workspace.BackendProfile}'."

                for entryPointName in moduleDump.EntryPoints do
                    match moduleDump.Functions |> List.tryFind (fun binding -> String.Equals(binding.Name, entryPointName, StringComparison.Ordinal)) with
                    | None ->
                        yield makeDiagnostic $"Checkpoint 'KBackendIR' requires listed entry point '{moduleDump.Name}.{entryPointName}' to be present as a function."
                    | Some binding when not binding.EntryPoint ->
                        yield makeDiagnostic $"Checkpoint 'KBackendIR' requires listed entry point '{moduleDump.Name}.{entryPointName}' to be marked as an entry point."
                    | Some _ ->
                        ()

                for binding in moduleDump.Functions do
                    let bindingLabel = $"{moduleDump.Name}.{binding.Name}"

                    if binding.CallingConvention.RuntimeArity <> List.length binding.Parameters then
                        yield makeDiagnostic $"Checkpoint 'KBackendIR' requires function '{bindingLabel}' to have a calling convention arity matching its parameter count."

                    if binding.CallingConvention.ParameterRepresentations <> (binding.Parameters |> List.map (fun parameter -> parameter.Representation)) then
                        yield makeDiagnostic $"Checkpoint 'KBackendIR' requires function '{bindingLabel}' to have calling convention parameter representations that match its parameters."

                    match binding.EnvironmentLayout with
                    | Some layoutName when not (environmentLayoutNames moduleDump |> Set.contains layoutName) ->
                        yield makeDiagnostic $"Checkpoint 'KBackendIR' requires function '{bindingLabel}' to reference an environment layout present in module '{moduleDump.Name}'."
                    | _ ->
                        ()

                    if binding.EntryPoint then
                        if not binding.Exported then
                            yield makeDiagnostic $"Checkpoint 'KBackendIR' requires entry point '{bindingLabel}' to be exported."

                        if not (List.contains binding.Name moduleDump.EntryPoints) then
                            yield makeDiagnostic $"Checkpoint 'KBackendIR' requires function '{bindingLabel}' marked as an entry point to be listed in module entry points."

                    if binding.Intrinsic then
                        if binding.Body.IsSome then
                            yield makeDiagnostic $"Checkpoint 'KBackendIR' requires intrinsic function '{bindingLabel}' to omit a body."

                        if not (List.contains binding.Name moduleDump.IntrinsicTerms) then
                            yield makeDiagnostic $"Checkpoint 'KBackendIR' requires intrinsic function '{bindingLabel}' to be listed in module intrinsic terms."

                        if not (supportedIntrinsics.Contains binding.Name) then
                            yield makeDiagnostic $"Checkpoint 'KBackendIR' requires intrinsic term '{binding.Name}' in module '{moduleDump.Name}' to be provided by backend profile '{workspace.BackendProfile}'."
                    else
                        match binding.Body with
                        | None ->
                            if not (isGeneratedHostBinding binding.Provenance) then
                                yield makeDiagnostic $"Checkpoint 'KBackendIR' requires function '{bindingLabel}' to have a body."
                        | Some body ->
                            yield! verifyBackendExpression moduleDump bindingLabel body
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
