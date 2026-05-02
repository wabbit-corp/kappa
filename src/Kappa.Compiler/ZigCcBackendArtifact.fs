namespace Kappa.Compiler

open System
open System.IO

// Builds target manifests and native artifacts from emitted zig translation units.
module internal ZigCcBackendArtifact =
    open ZigCcBackendSupport
    open ZigCcBackendEmit
    open ZigCcBackendRuntime

    let private routeTraitMemberBindings
        (workspace: WorkspaceCompilation)
        (traitName: string)
        (memberName: string)
        (instanceFilter: (KBackendTraitInstance -> bool))
        =
        workspace.KBackendIR
        |> List.collect (fun moduleDump ->
            moduleDump.TraitInstances
            |> List.choose (fun instanceInfo ->
                if String.Equals(instanceInfo.TraitName, traitName, StringComparison.Ordinal) && instanceFilter instanceInfo then
                    instanceInfo.MemberBindings
                    |> List.tryFind (fun (candidateMemberName, _) ->
                        String.Equals(candidateMemberName, memberName, StringComparison.Ordinal))
                    |> Option.map (fun (_, bindingName) -> instanceInfo.ModuleName, bindingName)
                else
                    None))

    let private computeReachableFunctions (workspace: WorkspaceCompilation) (roots: (string * string) list) =
        let functionBodies =
            workspace.KBackendIR
            |> List.collect (fun moduleDump ->
                moduleDump.Functions
                |> List.choose (fun functionDump ->
                    if functionDump.Intrinsic then
                        None
                    else
                        Some((moduleDump.Name, functionDump.Name), functionDump.Body)))
            |> Map.ofList

        let rec visitBinding visited bindingKey =
            if Set.contains bindingKey visited then
                visited
            else
                let visited = Set.add bindingKey visited

                match functionBodies |> Map.tryFind bindingKey with
                | Some(Some body) -> visitExpression visited body
                | _ -> visited

        and visitExpression visited expression =
            match expression with
            | BackendLiteral _
            | BackendEffectLabel _ ->
                visited
            | BackendName(BackendGlobalBindingName(moduleName, bindingName, _)) ->
                visitBinding visited (moduleName, bindingName)
            | BackendName _
            | BackendEffectOperation _
            | BackendConstructData _ ->
                visited
            | BackendClosure(_, _, _, body, _, _)
            | BackendExecute(body, _)
            | BackendPure(body, _) ->
                visitExpression visited body
            | BackendIfThenElse(condition, whenTrue, whenFalse, _) ->
                let visitedAfterCondition = visitExpression visited condition
                let visitedAfterTrue = visitExpression visitedAfterCondition whenTrue
                visitExpression visitedAfterTrue whenFalse
            | BackendMatch(scrutinee, cases, _) ->
                let visitedAfterScrutinee = visitExpression visited scrutinee

                cases
                |> List.fold
                    (fun state caseClause ->
                        let stateAfterGuard =
                            caseClause.Guard
                            |> Option.map (visitExpression state)
                            |> Option.defaultValue state

                        visitExpression stateAfterGuard caseClause.Body)
                    visitedAfterScrutinee
            | BackendBind(action, binder, _)
            | BackendThen(action, binder, _)
            | BackendSequence(action, binder, _) ->
                let visitedAfterAction = visitExpression visited action
                visitExpression visitedAfterAction binder
            | BackendHandle(_, label, body, returnClause, operationClauses, _) ->
                let visitedAfterCore =
                    let visitedAfterLabel = visitExpression visited label
                    let visitedAfterBody = visitExpression visitedAfterLabel body
                    visitExpression visitedAfterBody returnClause.Body

                operationClauses
                |> List.fold (fun state clause -> visitExpression state clause.Body) visitedAfterCore
            | BackendLet(_, value, body, _) ->
                let visitedAfterValue = visitExpression visited value
                visitExpression visitedAfterValue body
            | BackendWhile(condition, body) ->
                let visitedAfterCondition = visitExpression visited condition
                visitExpression visitedAfterCondition body
            | BackendCall(callee, arguments, _, _) ->
                arguments
                |> List.fold visitExpression (visitExpression visited callee)
            | BackendDictionaryValue(_, _, _, captures, _) ->
                captures |> List.fold visitExpression visited
            | BackendTraitCall(traitName, memberName, dictionary, arguments, _) ->
                let visitedAfterInputs =
                    arguments
                    |> List.fold visitExpression (visitExpression visited dictionary)

                let routes =
                    match dictionary with
                    | BackendDictionaryValue(moduleName, _, instanceKey, _, _) ->
                        routeTraitMemberBindings
                            workspace
                            traitName
                            memberName
                            (fun instanceInfo ->
                                String.Equals(instanceInfo.ModuleName, moduleName, StringComparison.Ordinal)
                                && String.Equals(instanceInfo.InstanceKey, instanceKey, StringComparison.Ordinal))
                    | _ ->
                        routeTraitMemberBindings workspace traitName memberName (fun _ -> true)

                routes |> List.fold visitBinding visitedAfterInputs
            | BackendPrefixedString(_, parts, _) ->
                parts
                |> List.fold
                    (fun state part ->
                        match part with
                        | BackendStringText _ -> state
                        | BackendStringInterpolation inner -> visitExpression state inner)
                    visited

        roots |> List.fold visitBinding Set.empty

    let private filterWorkspaceToReachableFunctions (workspace: WorkspaceCompilation) (reachableFunctions: Set<string * string>) =
        let filteredBackendIr =
            workspace.KBackendIR
            |> List.map (fun moduleDump ->
                let filteredFunctions =
                    moduleDump.Functions
                    |> List.filter (fun functionDump -> functionDump.Intrinsic || Set.contains (moduleDump.Name, functionDump.Name) reachableFunctions)

                let filteredTraitInstances =
                    moduleDump.TraitInstances
                    |> List.filter (fun instanceInfo ->
                        instanceInfo.MemberBindings
                        |> List.exists (fun (_, bindingName) -> Set.contains (instanceInfo.ModuleName, bindingName) reachableFunctions))

                { moduleDump with
                    EntryPoints = moduleDump.EntryPoints |> List.filter (fun bindingName -> Set.contains (moduleDump.Name, bindingName) reachableFunctions)
                    TraitInstances = filteredTraitInstances
                    Functions = filteredFunctions })

        { workspace with KBackendIR = filteredBackendIr }

    let internal buildTranslationUnit (workspace: WorkspaceCompilation) (roots: (string * string) list option) =
        result {
            if workspace.HasErrors then
                let detail = aggregateDiagnostics workspace.Diagnostics
                return!
                    Result.Error(DiagnosticFact.ZigArtifactEmitterError.message (ZigArtifactWorkspaceHasDiagnostics detail))

            let verificationDiagnostics = CheckpointVerification.verifyCheckpoint workspace "KBackendIR"

            if not (List.isEmpty verificationDiagnostics) then
                let detail = aggregateDiagnostics verificationDiagnostics
                return!
                    Result.Error(DiagnosticFact.ZigArtifactEmitterError.message (ZigArtifactMalformedBackendIr detail))

            let effectiveRoots =
                match roots with
                | Some explicitRoots -> explicitRoots
                | None ->
                    workspace.KBackendIR
                    |> List.collect (fun moduleDump -> moduleDump.EntryPoints |> List.map (fun bindingName -> moduleDump.Name, bindingName))

            let effectiveWorkspace =
                if List.isEmpty effectiveRoots then
                    workspace
                else
                    effectiveRoots
                    |> computeReachableFunctions workspace
                    |> filterWorkspaceToReachableFunctions workspace

            let context = buildContext effectiveWorkspace

            let topLevelFunctions =
                effectiveWorkspace.KBackendIR
                |> List.collect (fun moduleDump ->
                    moduleDump.Functions
                    |> List.filter (fun functionDump -> not functionDump.Intrinsic)
                    |> List.map (fun functionDump -> moduleDump.Name, functionDump))

            let! emittedTopLevelFunctions =
                topLevelFunctions
                |> List.fold
                    (fun state (moduleName, functionDump) ->
                        result {
                            let! emitted = state
                            let! emittedFunction = emitFunctionDefinition context moduleName functionDump
                            return emitted @ [ emittedFunction ]
                        })
                    (Result.Ok [])

            let closureFunctions =
                context.GeneratedClosures |> Seq.toList

            let prototypes =
                emittedTopLevelFunctions
                |> List.map (fun functionDump -> functionDump.Prototype)
                |> List.append (closureFunctions |> List.map (fun functionDump -> functionDump.Prototype))

            let supportDefinitions =
                closureFunctions
                |> List.collect (fun functionDump -> functionDump.SupportDefinitions)

            let traitDispatchDefinitions =
                emitTraitDispatchFunctions context

            let definitions =
                emittedTopLevelFunctions
                |> List.map (fun functionDump -> functionDump.Definition)
                |> List.append (closureFunctions |> List.map (fun functionDump -> functionDump.Definition))

            let entrySymbols =
                effectiveWorkspace.KBackendIR
                |> List.collect (fun moduleDump ->
                    moduleDump.EntryPoints
                    |> List.choose (fun entryPointName ->
                        lookupFunctionName context moduleDump.Name entryPointName))
                |> List.distinct
                |> List.sort

            let functionSymbols =
                context.FunctionNames
                |> Map.toList
                |> List.map snd
                |> List.sort

            let sourceText =
                [
                    yield emitRuntimePrelude context
                    if not (List.isEmpty prototypes) then
                        yield joinLines prototypes
                    if not (List.isEmpty traitDispatchDefinitions) then
                        yield joinLines traitDispatchDefinitions
                    if not (List.isEmpty supportDefinitions) then
                        yield joinLines supportDefinitions
                    if not (List.isEmpty definitions) then
                        yield joinLines definitions
                ]
                |> String.concat (Environment.NewLine + Environment.NewLine)

            return
                { ArtifactKind = "c-translation-unit"
                  TranslationUnitName = "kappa.generated.c"
                  InputCheckpoint = "KBackendIR"
                  EntrySymbols = entrySymbols
                  FunctionSymbols = functionSymbols
                  SourceText = sourceText }
        }

    let emitTranslationUnit (workspace: WorkspaceCompilation) =
        buildTranslationUnit workspace None

    let emitArtifact (workspace: WorkspaceCompilation) (entryPoint: string) (outputDirectory: string) =
        result {
            let! entryModuleName, entryBindingName = resolveEntryPoint workspace entryPoint
            let roots = [ entryModuleName, entryBindingName ]
            let! translationUnit = buildTranslationUnit workspace (Some roots)

            let effectiveWorkspace =
                roots
                |> computeReachableFunctions workspace
                |> filterWorkspaceToReachableFunctions workspace

            let context = buildContext effectiveWorkspace

            let! entryFunctionName =
                lookupFunctionName context entryModuleName entryBindingName
                |> resultOfOption(
                    DiagnosticFact.ZigBackendEmitterError.message (ZigEmittedEntryPointResolutionFailed entryPoint)
                )

            let resolvedOutputDirectory = Path.GetFullPath(outputDirectory)
            Directory.CreateDirectory(resolvedOutputDirectory) |> ignore

            let sourceFilePath = Path.Combine(resolvedOutputDirectory, translationUnit.TranslationUnitName)
            let executableFilePath = executablePath resolvedOutputDirectory "kappa.generated"

            let sourceText =
                translationUnit.SourceText
                + (Environment.NewLine + Environment.NewLine)
                + emitEntryPointProgram entryFunctionName

            File.WriteAllText(sourceFilePath, sourceText)

            return
                { OutputDirectory = resolvedOutputDirectory
                  SourceFilePath = sourceFilePath
                  ExecutableFilePath = executableFilePath
                  EntryPoint = entryPoint }
        }
