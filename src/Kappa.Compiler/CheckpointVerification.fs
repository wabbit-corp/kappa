namespace Kappa.Compiler

open System

module CheckpointVerification =
    let private makeDiagnostic message =
        { Severity = Error
          Message = message
          Location = None }

    let availableCheckpointNames =
        [
            "surface-source"
            yield! KFrontIRPhase.all |> List.map KFrontIRPhase.checkpointName
            "KCore"
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

    let private verifyFrontendCheckpoint (workspace: WorkspaceCompilation) checkpoint =
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
        ]

    let private verifyKCoreCheckpoint (workspace: WorkspaceCompilation) =
        let duplicateModules =
            workspace.KCore
            |> List.map (fun moduleDump -> moduleDump.Name)
            |> List.countBy id
            |> List.filter (fun (_, count) -> count > 1)

        [
            yield! verifyFrontendCheckpoint workspace "KCore"

            for moduleName, _ in duplicateModules do
                yield makeDiagnostic $"Checkpoint 'KCore' requires unique module identities, but '{moduleName}' appeared more than once."
        ]

    let private importedModuleName (spec: ImportSpec) : string option =
        match spec.Source with
        | Dotted moduleSegments ->
            Some(SyntaxFacts.moduleNameToText moduleSegments)
        | Url _ ->
            None

    let private selectionImportsBackendTermName selection name =
        match selection with
        | QualifiedOnly ->
            false
        | Items items ->
            items
            |> List.exists (fun item ->
                String.Equals(item.Name, name, StringComparison.Ordinal)
                && (item.Namespace.IsNone || item.Namespace = Some ImportNamespace.Term))
        | All ->
            true
        | AllExcept excludedNames ->
            not (List.contains name excludedNames)

    let private selectionImportsBackendConstructorName selection name =
        match selection with
        | QualifiedOnly ->
            false
        | Items items ->
            items
            |> List.exists (fun item ->
                String.Equals(item.Name, name, StringComparison.Ordinal)
                && item.Namespace = Some ImportNamespace.Constructor)
        | All
        | AllExcept _ ->
            false

    let private verifyKBackendIRCheckpoint (workspace: WorkspaceCompilation) =
        let backendModules : KBackendModule list =
            workspace.KBackendIR
            |> List.sortBy (fun (moduleDump: KBackendModule) -> moduleDump.SourceFile)

        let moduleMap : Map<string, KBackendModule> =
            backendModules
            |> List.map (fun (moduleDump: KBackendModule) -> moduleDump.Name, moduleDump)
            |> Map.ofList

        let bindingNames (moduleDump: KBackendModule) : Set<string> =
            moduleDump.Bindings
            |> List.map (fun (binding: KBackendBinding) -> binding.Name)
            |> Set.ofList

        let constructorNames (moduleDump: KBackendModule) : Set<string> =
            moduleDump.Constructors
            |> List.map (fun (constructor: KBackendConstructor) -> constructor.Name)
            |> Set.ofList

        let availableIntrinsicTerms (moduleName: string) : Set<string> =
            if String.Equals(moduleName, Stdlib.PreludeModuleText, StringComparison.Ordinal) then
                Stdlib.intrinsicTermNamesFor workspace.BackendProfile Stdlib.PreludeModuleName
            else
                Set.empty

        let tryResolveQualifiedBackendModule
            (currentModule: KBackendModule)
            (qualifierSegments: string list)
            : KBackendModule option =
            let qualifierText = SyntaxFacts.moduleNameToText qualifierSegments

            if String.Equals(qualifierText, currentModule.Name, StringComparison.Ordinal) then
                Some currentModule
            else
                moduleMap
                |> Map.tryFind qualifierText
                |> Option.orElseWith (fun () ->
                    currentModule.Imports
                    |> List.tryPick (fun (spec: ImportSpec) ->
                        match spec.Source, spec.Alias, spec.Selection with
                        | Dotted moduleSegments, Some alias, QualifiedOnly when qualifierSegments = [ alias ] ->
                            moduleMap |> Map.tryFind (SyntaxFacts.moduleNameToText moduleSegments)
                        | Dotted moduleSegments, None, QualifiedOnly when qualifierSegments = moduleSegments ->
                            moduleMap |> Map.tryFind (SyntaxFacts.moduleNameToText moduleSegments)
                        | _ ->
                            None))

        let resolveUnqualifiedBackendExpressionName
            (currentModule: KBackendModule)
            (locals: Set<string>)
            (name: string)
            : Result<unit, string> =
            if Set.contains name locals then
                Result.Ok()
            elif bindingNames currentModule |> Set.contains name then
                Result.Ok()
            elif constructorNames currentModule |> Set.contains name then
                Result.Ok()
            else
                let importedTermMatches =
                    currentModule.Imports
                    |> List.choose (fun (spec: ImportSpec) ->
                        importedModuleName spec
                        |> Option.bind (fun importedName ->
                            moduleMap
                            |> Map.tryFind importedName
                            |> Option.bind (fun (importedModule: KBackendModule) ->
                                if selectionImportsBackendTermName spec.Selection name
                                   && List.contains name importedModule.Exports
                                   && (bindingNames importedModule |> Set.contains name) then
                                    Some importedName
                                else
                                    None)))
                    |> List.distinct

                match importedTermMatches with
                | [ _ ] ->
                    Result.Ok()
                | _ :: _ :: _ ->
                    Result.Error $"ambiguous runtime name '{name}'"
                | [] ->
                    let importedConstructorMatches =
                        currentModule.Imports
                        |> List.choose (fun (spec: ImportSpec) ->
                            importedModuleName spec
                            |> Option.bind (fun importedName ->
                                moduleMap
                                |> Map.tryFind importedName
                                |> Option.bind (fun (importedModule: KBackendModule) ->
                                    if selectionImportsBackendConstructorName spec.Selection name
                                       && List.contains name importedModule.Exports
                                       && (constructorNames importedModule |> Set.contains name) then
                                        Some importedName
                                    else
                                        None)))
                        |> List.distinct

                    match importedConstructorMatches with
                    | [ _ ] ->
                        Result.Ok()
                    | _ :: _ :: _ ->
                        Result.Error $"ambiguous runtime name '{name}'"
                    | [] ->
                        Result.Error $"unresolved runtime name '{name}'"

        let resolveBackendExpressionName
            (currentModule: KBackendModule)
            (locals: Set<string>)
            (segments: string list)
            : Result<unit, string> =
            match segments with
            | [] ->
                Result.Error "empty runtime name"
            | [ name ] ->
                resolveUnqualifiedBackendExpressionName currentModule locals name
            | _ ->
                let qualifierSegments = segments |> List.take (segments.Length - 1)
                let bindingName = List.last segments

                match tryResolveQualifiedBackendModule currentModule qualifierSegments with
                | None ->
                    Result.Error $"unresolved module qualifier '{SyntaxFacts.moduleNameToText qualifierSegments}'"
                | Some targetModule when bindingNames targetModule |> Set.contains bindingName ->
                    Result.Ok()
                | Some targetModule when constructorNames targetModule |> Set.contains bindingName ->
                    Result.Ok()
                | Some _ ->
                    let nameText = String.concat "." segments
                    Result.Error $"unresolved runtime name '{nameText}'"

        let rec verifyBackendPattern
            (currentModule: KBackendModule)
            (bindingLabel: string)
            (locals: Set<string>)
            (pattern: KBackendPattern)
            : Set<string> * Diagnostic list =
            match pattern with
            | KBackendWildcardPattern
            | KBackendLiteralPattern _ ->
                locals, []
            | KBackendNamePattern name ->
                if Set.contains name locals then
                    locals,
                    [ makeDiagnostic $"Checkpoint 'KBackendIR' requires unique pattern binder names within '{bindingLabel}', but '{name}' was duplicated." ]
                else
                    Set.add name locals, []
            | KBackendConstructorPattern(nameSegments, argumentPatterns) ->
                let resolutionDiagnostics =
                    match resolveBackendExpressionName currentModule Set.empty nameSegments with
                    | Result.Ok() ->
                        []
                    | Result.Error issue ->
                        let patternText = String.concat "." nameSegments
                        [ makeDiagnostic $"Checkpoint 'KBackendIR' requires fully resolved constructor patterns, but '{patternText}' in '{bindingLabel}' has {issue}." ]

                ((locals, resolutionDiagnostics), argumentPatterns)
                ||> List.fold (fun (localsSoFar, diagnosticsSoFar) (argumentPattern: KBackendPattern) ->
                    let nextLocals, nextDiagnostics =
                        verifyBackendPattern currentModule bindingLabel localsSoFar argumentPattern

                    nextLocals, diagnosticsSoFar @ nextDiagnostics)

        let rec verifyBackendExpression
            (currentModule: KBackendModule)
            (bindingLabel: string)
            (locals: Set<string>)
            (expression: KBackendExpression)
            : Diagnostic list =
            match expression with
            | KBackendLiteral _ ->
                []
            | KBackendName segments ->
                match resolveBackendExpressionName currentModule locals segments with
                | Result.Ok() ->
                    []
                | Result.Error issue ->
                    let nameText = String.concat "." segments
                    [ makeDiagnostic $"Checkpoint 'KBackendIR' requires fully resolved runtime names, but '{nameText}' in '{bindingLabel}' has {issue}." ]
            | KBackendClosure(parameters, body) ->
                let duplicateParameters =
                    parameters
                    |> List.countBy id
                    |> List.filter (fun (_, count) -> count > 1)
                    |> List.map (fun (name, _) ->
                        makeDiagnostic $"Checkpoint 'KBackendIR' requires closures in '{bindingLabel}' to have unique parameter names, but '{name}' was duplicated.")

                let extendedLocals =
                    parameters
                    |> List.fold (fun state parameterName -> Set.add parameterName state) locals

                duplicateParameters @ verifyBackendExpression currentModule bindingLabel extendedLocals body
            | KBackendIfThenElse(condition, whenTrue, whenFalse) ->
                verifyBackendExpression currentModule bindingLabel locals condition
                @ verifyBackendExpression currentModule bindingLabel locals whenTrue
                @ verifyBackendExpression currentModule bindingLabel locals whenFalse
            | KBackendMatch(scrutinee, cases) ->
                verifyBackendExpression currentModule bindingLabel locals scrutinee
                @ (cases
                   |> List.collect (fun (caseClause: KBackendMatchCase) ->
                       let caseLocals, patternDiagnostics =
                           verifyBackendPattern currentModule bindingLabel locals caseClause.Pattern

                       patternDiagnostics @ verifyBackendExpression currentModule bindingLabel caseLocals caseClause.Body))
            | KBackendApply(callee, arguments) ->
                verifyBackendExpression currentModule bindingLabel locals callee
                @ (arguments
                   |> List.collect (fun (argument: KBackendExpression) ->
                       verifyBackendExpression currentModule bindingLabel locals argument))
            | KBackendUnary(_, operand) ->
                verifyBackendExpression currentModule bindingLabel locals operand
            | KBackendBinary(left, _, right) ->
                verifyBackendExpression currentModule bindingLabel locals left
                @ verifyBackendExpression currentModule bindingLabel locals right
            | KBackendPrefixedString(_, parts) ->
                parts
                |> List.collect (function
                    | KBackendStringText _ ->
                        []
                    | KBackendStringInterpolation inner ->
                        verifyBackendExpression currentModule bindingLabel locals inner)

        [
            yield! verifyKCoreCheckpoint workspace

            for moduleDump in backendModules do
                for spec in moduleDump.Imports do
                    match importedModuleName spec with
                    | Some importedName when not (moduleMap.ContainsKey importedName) ->
                        yield makeDiagnostic $"Checkpoint 'KBackendIR' requires imported runtime module '{importedName}' to be present for module '{moduleDump.Name}'."
                    | _ ->
                        ()

                let duplicateBindings =
                    moduleDump.Bindings
                    |> List.countBy (fun (binding: KBackendBinding) -> binding.Name)
                    |> List.filter (fun (_, count) -> count > 1)

                for bindingName, _ in duplicateBindings do
                    yield makeDiagnostic $"Checkpoint 'KBackendIR' requires unique binding identities within module '{moduleDump.Name}', but '{bindingName}' was duplicated."

                let duplicateConstructors =
                    moduleDump.Constructors
                    |> List.countBy (fun (constructor: KBackendConstructor) -> constructor.Name)
                    |> List.filter (fun (_, count) -> count > 1)

                for constructorName, _ in duplicateConstructors do
                    yield makeDiagnostic $"Checkpoint 'KBackendIR' requires unique constructor identities within module '{moduleDump.Name}', but '{constructorName}' was duplicated."

                let supportedIntrinsics = availableIntrinsicTerms moduleDump.Name

                for intrinsicName in moduleDump.IntrinsicTerms do
                    if not (supportedIntrinsics.Contains intrinsicName) then
                        yield makeDiagnostic $"Checkpoint 'KBackendIR' requires intrinsic term '{intrinsicName}' in module '{moduleDump.Name}' to be provided by backend profile '{workspace.BackendProfile}'."

                for binding in moduleDump.Bindings do
                    if binding.Intrinsic then
                        if binding.Body.IsSome then
                            yield makeDiagnostic $"Checkpoint 'KBackendIR' requires intrinsic binding '{moduleDump.Name}.{binding.Name}' to omit a body."

                        if not (List.contains binding.Name moduleDump.IntrinsicTerms) then
                            yield makeDiagnostic $"Checkpoint 'KBackendIR' requires intrinsic binding '{moduleDump.Name}.{binding.Name}' to be listed in module intrinsic terms."

                        if not (supportedIntrinsics.Contains binding.Name) then
                            yield makeDiagnostic $"Checkpoint 'KBackendIR' requires intrinsic term '{binding.Name}' in module '{moduleDump.Name}' to be provided by backend profile '{workspace.BackendProfile}'."
                    else
                        match binding.Body with
                        | None ->
                            yield makeDiagnostic $"Checkpoint 'KBackendIR' requires runtime binding '{moduleDump.Name}.{binding.Name}' to have a body."
                        | Some body ->
                            let bindingLabel = $"{moduleDump.Name}.{binding.Name}"
                            yield! verifyBackendExpression moduleDump bindingLabel (binding.Parameters |> Set.ofList) body
        ]

    let verifyCheckpoint (workspace: WorkspaceCompilation) checkpoint =
        match checkpoint with
        | "surface-source" ->
            verifySurfaceSource workspace
        | "KCore" ->
            verifyKCoreCheckpoint workspace
        | "KBackendIR" ->
            verifyKBackendIRCheckpoint workspace
        | _ ->
            match tryParseCheckpoint checkpoint with
            | Some(Some _) ->
                verifyFrontendCheckpoint workspace checkpoint
            | Some None ->
                verifySurfaceSource workspace
            | None ->
                [ makeDiagnostic $"Unknown checkpoint '{checkpoint}'." ]
