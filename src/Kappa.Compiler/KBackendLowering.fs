namespace Kappa.Compiler

open System

// Lowers KRuntimeIR into target-neutral runtime-facing KBackendIR.
module internal KBackendLowering =
    let private typeReferenceSegments =
        TypeSignatures.TypeReference.segments

    let rec private isFunctionLikeType typeExpr =
        match typeExpr with
        | TypeSignatures.TypeArrow _ -> true
        | TypeSignatures.TypeCapture(inner, _) -> isFunctionLikeType inner
        | _ -> false

    type private BackendLoweringBindingInfo =
        { ModuleName: string
          Name: string
          Intrinsic: bool
          Arity: int
          ReturnRepresentation: KBackendRepresentationClass option }

    type private BackendLoweringConstructorInfo =
        { ModuleName: string
          TypeName: string
          Name: string
          Tag: int
          Arity: int
          FieldRepresentations: KBackendRepresentationClass list
          Representation: KBackendRepresentationClass
          Provenance: KCoreOrigin }

    type private BackendLoweringContext =
        { RuntimeModules: Map<string, KRuntimeModule>
          BindingInfos: Map<string * string, BackendLoweringBindingInfo>
          ConstructorInfos: Map<string * string, BackendLoweringConstructorInfo> }

    let private backendLiteralRepresentation literal =
        match literal with
        | LiteralValue.Integer _ -> BackendRepInt64
        | LiteralValue.Float _ -> BackendRepFloat64
        | LiteralValue.String _ -> BackendRepString
        | LiteralValue.Character _
        | LiteralValue.Grapheme _ -> BackendRepString
        | LiteralValue.Byte _ -> BackendRepInt64
        | LiteralValue.Unit -> BackendRepUnit

    let private backendOpaqueRepresentation name =
        BackendRepOpaque name

    let private effectResumptionAllowsMultiple quantity =
        match quantity |> Option.defaultValue QuantityOne |> ResourceModel.ResourceQuantity.ofSurface with
        | ResourceModel.ResourceQuantity.Interval(_, Some maximum) ->
            maximum > 1
        | ResourceModel.ResourceQuantity.Interval(_, None) ->
            true
        | ResourceModel.ResourceQuantity.Borrow _
        | ResourceModel.ResourceQuantity.Variable _ ->
            false

    let private tryBackendRepresentationFromTypeExpr typeExpr =
        let tryTypeHead typeExpr =
            match typeExpr with
            | TypeSignatures.TypeName(nameSegments, _) when not (List.isEmpty (typeReferenceSegments nameSegments)) ->
                List.tryLast (typeReferenceSegments nameSegments)
            | _ ->
                None

        match typeExpr with
        | Some parsedType when isFunctionLikeType parsedType ->
            Some(backendOpaqueRepresentation (Some "Function"))
        | _ ->
            match typeExpr |> Option.bind tryTypeHead with
            | Some head when head = Stdlib.KnownTypeNames.Int -> Some BackendRepInt64
            | Some head when head = Stdlib.KnownTypeNames.Nat -> Some BackendRepInt64
            | Some head when head = Stdlib.KnownTypeNames.Integer -> Some BackendRepInt64
            | Some head when head = Stdlib.KnownTypeNames.Float -> Some BackendRepFloat64
            | Some head when head = Stdlib.KnownTypeNames.Double -> Some BackendRepFloat64
            | Some head when head = Stdlib.KnownTypeNames.Real -> Some BackendRepFloat64
            | Some head when head = Stdlib.KnownTypeNames.Bool -> Some BackendRepBoolean
            | Some head when head = Stdlib.KnownTypeNames.String -> Some BackendRepString
            | Some head
                when head = Stdlib.KnownTypeNames.Char
                     || head = Stdlib.KnownTypeNames.UnicodeScalar
                     || head = Stdlib.KnownTypeNames.Grapheme -> Some BackendRepString
            | Some head when head = Stdlib.KnownTypeNames.Byte -> Some BackendRepInt64
            | Some head when head = Stdlib.KnownTypeNames.Bytes -> Some BackendRepString
            | Some head when head = Stdlib.KnownTypeNames.HashCode -> Some BackendRepInt64
            | Some head when head = Stdlib.KnownTypeNames.Unit -> Some BackendRepUnit
            | Some head when head.StartsWith("__kappa_dict_", StringComparison.Ordinal) ->
                Some(BackendRepDictionary(head.Substring("__kappa_dict_".Length)))
            | Some head when head = Stdlib.KnownTypeNames.IO -> Some BackendRepIOAction
            | Some head when head = Stdlib.KnownTypeNames.Ref -> Some(BackendRepRef(backendOpaqueRepresentation None))
            | Some head -> Some(backendOpaqueRepresentation (Some head))
            | None -> None

    let private mergeBackendRepresentations left right =
        if left = right then
            left
        else
            backendOpaqueRepresentation None

    let rec private inferKRuntimeExpressionRepresentation expression =
        match expression with
        | KRuntimeLiteral literal ->
            backendLiteralRepresentation literal
        | KRuntimeName [ "True" ]
        | KRuntimeName [ "False" ] ->
            BackendRepBoolean
        | KRuntimeEffectLabel _ ->
            BackendRepEffectLabel
        | KRuntimeEffectOperation _ ->
            BackendRepEffectOperation
        | KRuntimeName _ ->
            backendOpaqueRepresentation None
        | KRuntimeClosure _ ->
            backendOpaqueRepresentation (Some "Function")
        | KRuntimeIfThenElse(_, whenTrue, whenFalse) ->
            mergeBackendRepresentations
                (inferKRuntimeExpressionRepresentation whenTrue)
                (inferKRuntimeExpressionRepresentation whenFalse)
        | KRuntimeMatch(_, cases) ->
            match cases with
            | [] -> backendOpaqueRepresentation None
            | firstCase :: rest ->
                rest
                |> List.fold
                    (fun state caseClause ->
                        mergeBackendRepresentations state (inferKRuntimeExpressionRepresentation caseClause.Body))
                    (inferKRuntimeExpressionRepresentation firstCase.Body)
        | KRuntimeHandle _ ->
            BackendRepIOAction
        | KRuntimeExecute(KRuntimeApply(KRuntimeName [ intrinsicName ], _)) ->
            IntrinsicCatalog.executedIntrinsicResultRepresentation intrinsicName
            |> Option.defaultValue (backendOpaqueRepresentation None)
        | KRuntimeExecute inner ->
            inferKRuntimeExpressionRepresentation inner
        | KRuntimeLet(_, _, body) ->
            inferKRuntimeExpressionRepresentation body
        | KRuntimeDoScope(_, body) ->
            inferKRuntimeExpressionRepresentation body
        | KRuntimeScheduleExit(_, _, body) ->
            inferKRuntimeExpressionRepresentation body
        | KRuntimeSequence(_, second) ->
            inferKRuntimeExpressionRepresentation second
        | KRuntimeWhile _ ->
            BackendRepUnit
        | KRuntimeApply(KRuntimeName [ "pure" ], _)
        | KRuntimeApply(KRuntimeName [ ">>=" ], _)
        | KRuntimeApply(KRuntimeName [ ">>" ], _)
        | KRuntimeApply(KRuntimeName [ "print" ], _)
        | KRuntimeApply(KRuntimeName [ "println" ], _)
        | KRuntimeApply(KRuntimeName [ "openFile" ], _)
        | KRuntimeApply(KRuntimeName [ "primitiveReadData" ], _)
        | KRuntimeApply(KRuntimeName [ "readData" ], _)
        | KRuntimeApply(KRuntimeName [ "primitiveCloseFile" ], _)
        | KRuntimeApply(KRuntimeName [ "newRef" ], _)
        | KRuntimeApply(KRuntimeName [ "readRef" ], _)
        | KRuntimeApply(KRuntimeName [ "writeRef" ], _) ->
            BackendRepIOAction
        | KRuntimeApply(KRuntimeName [ "runPure" ], _) ->
            backendOpaqueRepresentation None
        | KRuntimeDictionaryValue(_, traitName, _, _) ->
            BackendRepDictionary traitName
        | KRuntimeTraitCall _ ->
            backendOpaqueRepresentation None
        | KRuntimeApply _ ->
            backendOpaqueRepresentation None
        | KRuntimeUnary("not", _) ->
            BackendRepBoolean
        | KRuntimeUnary("negate", operand) ->
            inferKRuntimeExpressionRepresentation operand
        | KRuntimeUnary _ ->
            backendOpaqueRepresentation None
        | KRuntimeBinary(_, ("&&" | "||" | "==" | "!=" | "<" | ">" | "<=" | ">=" | "is"), _) ->
            BackendRepBoolean
        | KRuntimeBinary(left, ("+" | "-" | "*" | "/"), right) ->
            match inferKRuntimeExpressionRepresentation left, inferKRuntimeExpressionRepresentation right with
            | BackendRepFloat64, _
            | _, BackendRepFloat64 ->
                BackendRepFloat64
            | BackendRepInt64, BackendRepInt64 ->
                BackendRepInt64
            | _ ->
                backendOpaqueRepresentation None
        | KRuntimeBinary _ ->
            backendOpaqueRepresentation None
        | KRuntimePrefixedString _ ->
            BackendRepString

    let private importedItemLocalName (item: ImportItem) =
        item.Alias |> Option.defaultValue item.Name

    let private itemImportsRuntimeTermName (item: ImportItem) =
        item.Namespace.IsNone || item.Namespace = Some ImportNamespace.Term

    let private itemImportsRuntimeConstructorName (item: ImportItem) =
        item.Namespace = Some ImportNamespace.Constructor

    let private selectionImportedRuntimeTermName selection name =
        let exceptMatches namespaceName (item: ExceptItem) =
            String.Equals(item.Name, name, StringComparison.Ordinal)
            && (item.Namespace.IsNone || item.Namespace = Some namespaceName)

        match selection with
        | QualifiedOnly ->
            None
        | Items items ->
            items
            |> List.tryFind (fun item ->
                String.Equals(importedItemLocalName item, name, StringComparison.Ordinal)
                && itemImportsRuntimeTermName item)
            |> Option.map (fun item -> item.Name)
        | All ->
            Some name
        | AllExcept excludedItems ->
            if (excludedItems |> List.exists (exceptMatches ImportNamespace.Term)) then
                None
            else
                Some name

    let private selectionImportedRuntimeConstructorName selection name typeName =
        match selection with
        | QualifiedOnly ->
            None
        | Items items ->
            items
            |> List.tryFind (fun item ->
                String.Equals(importedItemLocalName item, name, StringComparison.Ordinal)
                && (itemImportsRuntimeConstructorName item
                    || (item.IncludeConstructors
                        && item.Namespace = Some ImportNamespace.Type
                        && String.Equals(item.Name, typeName, StringComparison.Ordinal))
                    || (item.Namespace.IsNone
                        && String.Equals(item.Name, typeName, StringComparison.Ordinal))))
            |> Option.map (fun item -> item.Name)
        | All
        | AllExcept _ ->
            None

    let private buildBackendLoweringContext (kRuntimeIR: KRuntimeModule list) =
        let runtimeModules =
            kRuntimeIR
            |> List.map (fun moduleDump -> moduleDump.Name, moduleDump)
            |> Map.ofList

        let bindingInfos =
            kRuntimeIR
            |> List.collect (fun runtimeModule ->
                runtimeModule.Bindings
                |> List.map (fun binding ->
                    let returnRepresentation =
                        if binding.Intrinsic then
                            IntrinsicCatalog.intrinsicResultRepresentation binding.Name
                        else
                            tryBackendRepresentationFromTypeExpr binding.ReturnType
                            |> Option.orElseWith (fun () ->
                                binding.Body |> Option.map inferKRuntimeExpressionRepresentation)

                    let arity =
                        if binding.Intrinsic then
                            IntrinsicCatalog.intrinsicRuntimeArity binding.Name
                        else
                            List.length binding.Parameters

                    (runtimeModule.Name, binding.Name),
                    { ModuleName = runtimeModule.Name
                      Name = binding.Name
                      Intrinsic = binding.Intrinsic
                      Arity = arity
                      ReturnRepresentation = returnRepresentation }))
            |> Map.ofList

        let constructorInfos =
            kRuntimeIR
            |> List.collect (fun runtimeModule ->
                runtimeModule.Constructors
                |> List.groupBy (fun constructor -> constructor.TypeName)
                |> List.collect (fun (_, constructors) ->
                    constructors
                    |> List.mapi (fun tag constructor ->
                        (runtimeModule.Name, constructor.Name),
                        { ModuleName = runtimeModule.Name
                          TypeName = constructor.TypeName
                          Name = constructor.Name
                          Tag = tag
                          Arity = constructor.Arity
                          FieldRepresentations = List.replicate constructor.Arity (backendOpaqueRepresentation None)
                          Representation = BackendRepTaggedData(runtimeModule.Name, constructor.TypeName)
                          Provenance = constructor.Provenance })))
            |> Map.ofList

        { RuntimeModules = runtimeModules
          BindingInfos = bindingInfos
          ConstructorInfos = constructorInfos }

    let private loweringDiagnostic (runtimeModule: KRuntimeModule) (_binding: KRuntimeBinding option) error =
        Diagnostics.errorFact "KBackendIR" None None [] (DiagnosticFact.backend (KBackendLoweringFailed(runtimeModule.Name, error)))

    let lowerKBackendModules backendProfile allowUnsafeConsume (kRuntimeIR: KRuntimeModule list) =
        let context = buildBackendLoweringContext kRuntimeIR
        let availableRuntimeIntrinsics =
            Stdlib.runtimeIntrinsicTermNamesForCompilationProfile backendProfile allowUnsafeConsume Stdlib.PreludeModuleName

        let resolveQualifiedRuntimeModule (currentModule: KRuntimeModule) qualifierSegments =
            let qualifierText = SyntaxFacts.moduleNameToText qualifierSegments

            if String.Equals(qualifierText, currentModule.Name, StringComparison.Ordinal) then
                Some currentModule
            else
                context.RuntimeModules
                |> Map.tryFind qualifierText
                |> Option.orElseWith (fun () ->
                    currentModule.Imports
                    |> List.tryPick (fun (spec: ImportSpec) ->
                        match spec.Source, spec.Alias, spec.Selection with
                        | Dotted moduleSegments, Some alias, QualifiedOnly when qualifierSegments = [ alias ] ->
                            context.RuntimeModules |> Map.tryFind (SyntaxFacts.moduleNameToText moduleSegments)
                        | Dotted moduleSegments, None, QualifiedOnly when qualifierSegments = moduleSegments ->
                            context.RuntimeModules |> Map.tryFind (SyntaxFacts.moduleNameToText moduleSegments)
                        | _ ->
                            None))

        let resolveRuntimeName currentModule (locals: Map<string, KBackendRepresentationClass>) segments =
            let bindingNames (moduleDump: KRuntimeModule) =
                moduleDump.Bindings |> List.map (fun binding -> binding.Name) |> Set.ofList

            let constructorNames (moduleDump: KRuntimeModule) =
                moduleDump.Constructors |> List.map (fun constructor -> constructor.Name) |> Set.ofList

            let resolvedNameRepresentation resolvedName =
                match resolvedName with
                | BackendLocalName(_, Some representation) ->
                    representation
                | BackendLocalName _ ->
                    backendOpaqueRepresentation None
                | BackendGlobalBindingName(moduleName, bindingName, _) ->
                    let bindingInfo = context.BindingInfos[moduleName, bindingName]

                    if bindingInfo.Arity = 0 then
                        bindingInfo.ReturnRepresentation |> Option.defaultValue (backendOpaqueRepresentation None)
                    else
                        backendOpaqueRepresentation (Some "Function")
                | BackendIntrinsicName(moduleName, bindingName, _) ->
                    let bindingInfo = context.BindingInfos[moduleName, bindingName]

                    if bindingInfo.Arity = 0 then
                        bindingInfo.ReturnRepresentation |> Option.defaultValue (backendOpaqueRepresentation (Some "Intrinsic"))
                    else
                        backendOpaqueRepresentation (Some "Intrinsic")
                | BackendConstructorName(_, _, _, _, arity, representation) ->
                    if arity = 0 then representation else backendOpaqueRepresentation (Some "Constructor")

            let tryResolveModuleMember (targetModule: KRuntimeModule) memberName =
                if bindingNames targetModule |> Set.contains memberName then
                    let bindingInfo = context.BindingInfos[targetModule.Name, memberName]

                    if bindingInfo.Intrinsic then
                        Some(BackendIntrinsicName(targetModule.Name, memberName, bindingInfo.ReturnRepresentation))
                    else
                        Some(BackendGlobalBindingName(targetModule.Name, memberName, bindingInfo.ReturnRepresentation))
                elif constructorNames targetModule |> Set.contains memberName then
                    let constructorInfo = context.ConstructorInfos[targetModule.Name, memberName]

                    Some(
                        BackendConstructorName(
                            targetModule.Name,
                            constructorInfo.TypeName,
                            constructorInfo.Name,
                            constructorInfo.Tag,
                            constructorInfo.Arity,
                            constructorInfo.Representation
                        )
                    )
                else
                    None

            match segments with
            | [] ->
                Result.Error EmptyRuntimeName
            | [ name ] when locals.ContainsKey name ->
                let resolved = BackendLocalName(name, Map.tryFind name locals)
                Result.Ok(resolved, resolvedNameRepresentation resolved)
            | [ name ] ->
                let currentBinding =
                    tryResolveModuleMember currentModule name

                match currentBinding with
                | Some resolved ->
                    Result.Ok(resolved, resolvedNameRepresentation resolved)
                | None ->
                    let importedMatches =
                        currentModule.Imports
                        |> List.choose (fun (spec: ImportSpec) ->
                            match spec.Source with
                            | Url _ ->
                                None
                            | Dotted moduleSegments ->
                                let importedModuleName = SyntaxFacts.moduleNameToText moduleSegments

                                match context.RuntimeModules |> Map.tryFind importedModuleName with
                                | Some importedModule ->
                                    match selectionImportedRuntimeTermName spec.Selection name with
                                    | Some importedName ->
                                        tryResolveModuleMember importedModule importedName
                                        |> Option.map (fun resolved -> importedModuleName, resolved)
                                    | None ->
                                        importedModule.Constructors
                                        |> List.tryPick (fun constructorInfo ->
                                            selectionImportedRuntimeConstructorName spec.Selection name constructorInfo.TypeName
                                            |> Option.filter (fun importedName -> String.Equals(constructorInfo.Name, importedName, StringComparison.Ordinal))
                                            |> Option.bind (fun importedName ->
                                                tryResolveModuleMember importedModule importedName))
                                            |> Option.map (fun resolved -> importedModuleName, resolved)
                                | _ ->
                                    None)
                        |> List.distinctBy fst

                    match importedMatches with
                    | [ _, resolved ] ->
                        Result.Ok(resolved, resolvedNameRepresentation resolved)
                    | _ :: _ :: _ ->
                        Result.Error(AmbiguousRuntimeName name)
                    | [] ->
                        Result.Error(UnresolvedRuntimeName name)
            | _ ->
                let qualifierSegments = segments |> List.take (segments.Length - 1)
                let memberName = List.last segments

                match resolveQualifiedRuntimeModule currentModule qualifierSegments with
                | Some targetModule ->
                    match tryResolveModuleMember targetModule memberName with
                    | Some resolved ->
                        Result.Ok(resolved, resolvedNameRepresentation resolved)
                    | None ->
                        let text = String.concat "." segments
                        Result.Error(UnresolvedRuntimeName text)
                | None ->
                    Result.Error(UnresolvedModuleQualifier(SyntaxFacts.moduleNameToText qualifierSegments))

        let resolveRuntimeConstructor currentModule segments =
            let resolvedConstructorRepresentation resolvedName =
                match resolvedName with
                | BackendConstructorName(_, _, _, _, arity, representation) ->
                    if arity = 0 then representation else backendOpaqueRepresentation (Some "Constructor")
                | _ ->
                    backendOpaqueRepresentation None

            let tryResolveModuleConstructor (targetModule: KRuntimeModule) constructorName =
                if targetModule.Constructors |> List.exists (fun constructorInfo -> String.Equals(constructorInfo.Name, constructorName, StringComparison.Ordinal)) then
                    let constructorInfo = context.ConstructorInfos[targetModule.Name, constructorName]

                    Some(
                        BackendConstructorName(
                            targetModule.Name,
                            constructorInfo.TypeName,
                            constructorInfo.Name,
                            constructorInfo.Tag,
                            constructorInfo.Arity,
                            constructorInfo.Representation
                        )
                    )
                else
                    None

            match segments with
            | [] ->
                Result.Error EmptyRuntimeConstructorName
            | [ constructorName ] ->
                match tryResolveModuleConstructor currentModule constructorName with
                | Some resolved ->
                    Result.Ok(resolved, resolvedConstructorRepresentation resolved)
                | None ->
                    let importedMatches =
                        currentModule.Imports
                        |> List.choose (fun (spec: ImportSpec) ->
                            match spec.Source with
                            | Url _ ->
                                None
                            | Dotted moduleSegments ->
                                let importedModuleName = SyntaxFacts.moduleNameToText moduleSegments

                                match context.RuntimeModules |> Map.tryFind importedModuleName with
                                | Some importedModule ->
                                    importedModule.Constructors
                                    |> List.tryPick (fun constructorInfo ->
                                        selectionImportedRuntimeConstructorName
                                            spec.Selection
                                            constructorName
                                            constructorInfo.TypeName
                                        |> Option.filter (fun importedName -> String.Equals(constructorInfo.Name, importedName, StringComparison.Ordinal))
                                        |> Option.bind (fun importedName ->
                                            tryResolveModuleConstructor importedModule importedName
                                            |> Option.map (fun resolved -> importedModuleName, resolved)))
                                | None ->
                                    None)
                        |> List.distinctBy fst

                    match importedMatches with
                    | [ _, resolved ] ->
                        Result.Ok(resolved, resolvedConstructorRepresentation resolved)
                    | _ :: _ :: _ ->
                        Result.Error(AmbiguousRuntimeConstructorName constructorName)
                    | [] ->
                        Result.Error(UnresolvedRuntimeConstructorName constructorName)
            | _ ->
                let qualifierSegments = segments |> List.take (segments.Length - 1)
                let constructorName = List.last segments

                match resolveQualifiedRuntimeModule currentModule qualifierSegments with
                | Some targetModule ->
                    match tryResolveModuleConstructor targetModule constructorName with
                    | Some resolved ->
                        Result.Ok(resolved, resolvedConstructorRepresentation resolved)
                    | None ->
                        let text = String.concat "." segments
                        Result.Error(UnresolvedRuntimeConstructorName text)
                | None ->
                    Result.Error(UnresolvedModuleQualifier(SyntaxFacts.moduleNameToText qualifierSegments))

        let rec collectClosureCaptures (locals: Set<string>) (bound: Set<string>) expression =
            match expression with
            | KRuntimeLiteral _ ->
                Set.empty
            | KRuntimeEffectLabel _ ->
                Set.empty
            | KRuntimeEffectOperation(label, _, _) ->
                collectClosureCaptures locals bound label
            | KRuntimeName [ name ] when Set.contains name locals && not (Set.contains name bound) ->
                Set.singleton name
            | KRuntimeName _ ->
                Set.empty
            | KRuntimeClosure(parameters, body) ->
                collectClosureCaptures locals (Set.union bound (parameters |> Set.ofList)) body
            | KRuntimeIfThenElse(condition, whenTrue, whenFalse) ->
                collectClosureCaptures locals bound condition
                |> Set.union (collectClosureCaptures locals bound whenTrue)
                |> Set.union (collectClosureCaptures locals bound whenFalse)
            | KRuntimeHandle(_, label, body, returnClause, operationClauses) ->
                let clauseCaptures (clause: KRuntimeEffectHandlerClause) =
                    let clauseBound =
                        clause.Arguments
                        |> List.choose (function
                            | KRuntimeEffectNameArgument name -> Some name
                            | _ -> None)
                        |> fun names ->
                            names
                            |> List.fold (fun state name -> Set.add name state) bound
                        |> fun state ->
                            clause.ResumptionName
                            |> Option.map (fun name -> Set.add name state)
                            |> Option.defaultValue state

                    collectClosureCaptures locals clauseBound clause.Body

                collectClosureCaptures locals bound label
                |> Set.union (collectClosureCaptures locals bound body)
                |> Set.union (clauseCaptures returnClause)
                |> Set.union (operationClauses |> List.fold (fun state clause -> Set.union state (clauseCaptures clause)) Set.empty)
            | KRuntimeMatch(scrutinee, cases) ->
                let scrutineeCaptures = collectClosureCaptures locals bound scrutinee

                cases
                |> List.fold
                    (fun state caseClause ->
                        let rec collectPatternBindings pattern =
                            match pattern with
                            | KRuntimeWildcardPattern
                            | KRuntimeLiteralPattern _ ->
                                Set.empty
                            | KRuntimeNamePattern name ->
                                Set.singleton name
                            | KRuntimeOrPattern alternatives ->
                                alternatives
                                |> List.fold (fun state alternative -> Set.union state (collectPatternBindings alternative)) Set.empty
                            | KRuntimeConstructorPattern(_, arguments) ->
                                arguments
                                |> List.fold
                                    (fun patternState argumentPattern ->
                                        Set.union patternState (collectPatternBindings argumentPattern))
                                    Set.empty

                        let caseBound = Set.union bound (collectPatternBindings caseClause.Pattern)

                        let guardCaptures =
                            caseClause.Guard
                            |> Option.map (collectClosureCaptures locals caseBound)
                            |> Option.defaultValue Set.empty

                        state
                        |> Set.union guardCaptures
                        |> Set.union (collectClosureCaptures locals caseBound caseClause.Body))
                    scrutineeCaptures
            | KRuntimeExecute expression ->
                collectClosureCaptures locals bound expression
            | KRuntimeLet(bindingName, value, body) ->
                collectClosureCaptures locals bound value
                |> Set.union (collectClosureCaptures locals (Set.add bindingName bound) body)
            | KRuntimeDoScope(_, body) ->
                collectClosureCaptures locals bound body
            | KRuntimeScheduleExit(_, action, body) ->
                let actionCaptures =
                    match action with
                    | KRuntimeDeferred expression ->
                        collectClosureCaptures locals bound expression
                    | KRuntimeRelease(_, release, resource) ->
                        collectClosureCaptures locals bound release
                        |> Set.union (collectClosureCaptures locals bound resource)

                Set.union actionCaptures (collectClosureCaptures locals bound body)
            | KRuntimeSequence(first, second) ->
                collectClosureCaptures locals bound first
                |> Set.union (collectClosureCaptures locals bound second)
            | KRuntimeWhile(condition, body) ->
                collectClosureCaptures locals bound condition
                |> Set.union (collectClosureCaptures locals bound body)
            | KRuntimeApply(callee, arguments) ->
                arguments
                |> List.fold
                    (fun state argument -> Set.union state (collectClosureCaptures locals bound argument))
                    (collectClosureCaptures locals bound callee)
            | KRuntimeDictionaryValue _ ->
                Set.empty
            | KRuntimeTraitCall(_, _, dictionary, arguments) ->
                arguments
                |> List.fold
                    (fun state argument -> Set.union state (collectClosureCaptures locals bound argument))
                    (collectClosureCaptures locals bound dictionary)
            | KRuntimeUnary(_, operand) ->
                collectClosureCaptures locals bound operand
            | KRuntimeBinary(left, _, right) ->
                collectClosureCaptures locals bound left
                |> Set.union (collectClosureCaptures locals bound right)
            | KRuntimePrefixedString(_, parts) ->
                parts
                |> List.fold
                    (fun state part ->
                        match part with
                        | KRuntimeStringText _ ->
                            state
                        | KRuntimeStringInterpolation(inner, _) ->
                            Set.union state (collectClosureCaptures locals bound inner))
                    Set.empty

        let lowerModule (runtimeModule: KRuntimeModule) : Result<KBackendModule, BackendLoweringErrorEvidence> =
            let environmentLayouts = ResizeArray<KBackendEnvironmentLayout>()
            let mutable nextEnvironmentLayoutId = 0
            let mutable nextSyntheticBindingId = 0

            let makeCallingConvention runtimeArity parameterRepresentations resultRepresentation =
                { RuntimeArity = runtimeArity
                  ParameterRepresentations = parameterRepresentations
                  ResultRepresentation = resultRepresentation
                  RetainedDictionaryParameters = [] }

            let freshEnvironmentLayoutName prefix =
                nextEnvironmentLayoutId <- nextEnvironmentLayoutId + 1
                $"{prefix}$env{nextEnvironmentLayoutId}"

            let freshSyntheticBindingName prefix =
                nextSyntheticBindingId <- nextSyntheticBindingId + 1
                $"__kappa_{prefix}_{nextSyntheticBindingId}"

            let tryLookupBindingInfo moduleName bindingName =
                context.BindingInfos |> Map.tryFind (moduleName, bindingName)

            let synthesizeConstructorClosure
                (constructorInfo: BackendLoweringConstructorInfo)
                (loweredArguments: KBackendExpression list)
                (argumentRepresentations: KBackendRepresentationClass list)
                : Result<KBackendExpression * KBackendRepresentationClass, BackendLoweringErrorEvidence> =
                let remainingFieldRepresentations =
                    constructorInfo.FieldRepresentations
                    |> List.skip (List.length loweredArguments)

                let capturedBindings =
                    List.zip loweredArguments argumentRepresentations
                    |> List.mapi (fun _ (argumentExpression, argumentRepresentation) ->
                        let bindingName = freshSyntheticBindingName $"{constructorInfo.Name}_capture"

                        ({ Name = bindingName
                           Representation = argumentRepresentation }: KBackendParameter),
                        argumentExpression)

                let captures : KBackendCapture list =
                    capturedBindings
                    |> List.map (fun (binding, _) ->
                        { Name = binding.Name
                          Representation = binding.Representation })

                let parameters : KBackendParameter list =
                    remainingFieldRepresentations
                    |> List.mapi (fun _ representation ->
                        { Name = freshSyntheticBindingName $"{constructorInfo.Name}_arg"
                          Representation = representation })

                let environmentLayoutName = freshEnvironmentLayoutName constructorInfo.Name

                environmentLayouts.Add
                    { Name = environmentLayoutName
                      Slots = captures }

                let capturedFieldExpressions =
                    capturedBindings
                    |> List.map (fun (binding, _) ->
                        BackendName(BackendLocalName(binding.Name, Some binding.Representation)))

                let parameterFieldExpressions =
                    parameters
                    |> List.map (fun parameter ->
                        BackendName(BackendLocalName(parameter.Name, Some parameter.Representation)))

                let body =
                    BackendConstructData(
                        constructorInfo.ModuleName,
                        constructorInfo.TypeName,
                        constructorInfo.Name,
                        constructorInfo.Tag,
                        capturedFieldExpressions @ parameterFieldExpressions,
                        constructorInfo.Representation
                    )

                let convention =
                    makeCallingConvention
                        (List.length parameters)
                        (parameters |> List.map (fun parameter -> parameter.Representation))
                        (Some constructorInfo.Representation)

                let closureRepresentation = BackendRepClosure environmentLayoutName

                let closure =
                    BackendClosure(
                        parameters,
                        captures,
                        environmentLayoutName,
                        body,
                        convention,
                        closureRepresentation
                    )

                let wrappedClosure =
                    List.foldBack
                        (fun (binding, value) bodyExpression ->
                            BackendLet(binding, value, bodyExpression, closureRepresentation))
                        capturedBindings
                        closure

                Result.Ok(wrappedClosure, closureRepresentation)

            let lowerResolvedCall
                (loweredArguments: KBackendExpression list)
                (argumentRepresentations: KBackendRepresentationClass list)
                (fallbackResultRepresentation: KBackendRepresentationClass)
                (resolvedName: KBackendResolvedName)
                : Result<KBackendExpression * KBackendRepresentationClass, BackendLoweringErrorEvidence> =
                match resolvedName with
                | BackendConstructorName(moduleName, typeName, constructorName, tag, arity, representation)
                    when arity = List.length loweredArguments ->
                    Result.Ok(
                        BackendConstructData(
                            moduleName,
                            typeName,
                            constructorName,
                            tag,
                            loweredArguments,
                            representation
                        ),
                        representation
                    )
                | BackendConstructorName(moduleName, _, constructorName, _, arity, _) when List.length loweredArguments < arity ->
                    let constructorInfo = context.ConstructorInfos[moduleName, constructorName]
                    synthesizeConstructorClosure constructorInfo loweredArguments argumentRepresentations
                | BackendConstructorName(moduleName, typeName, constructorName, tag, arity, representation) ->
                    let directArguments = loweredArguments |> List.take arity
                    let remainingArguments = loweredArguments |> List.skip arity
                    let remainingArgumentRepresentations = argumentRepresentations |> List.skip arity
                    let constructedValue =
                        BackendConstructData(
                            moduleName,
                            typeName,
                            constructorName,
                            tag,
                            directArguments,
                            representation
                        )

                    let resultRepresentation = fallbackResultRepresentation
                    let convention =
                        makeCallingConvention
                            (List.length remainingArguments)
                            remainingArgumentRepresentations
                            (Some resultRepresentation)

                    Result.Ok(
                        BackendCall(constructedValue, remainingArguments, convention, resultRepresentation),
                        resultRepresentation
                    )
                | BackendIntrinsicName(moduleName, bindingName, _) ->
                    match IntrinsicCatalog.tryFindIntrinsicSpec bindingName with
                    | Some spec ->
                        let resultRepresentation =
                            spec.LoweredResultRepresentation
                            |> Option.defaultValue fallbackResultRepresentation

                        if List.length loweredArguments = spec.RuntimeArity then
                            Result.Ok(
                                BackendCall(
                                    BackendName resolvedName,
                                    loweredArguments,
                                    makeCallingConvention spec.RuntimeArity argumentRepresentations (Some resultRepresentation),
                                    resultRepresentation
                                ),
                                resultRepresentation
                            )
                        else
                            Result.Error(
                                RuntimeIntrinsicArityMismatch(
                                    $"{moduleName}.{bindingName}",
                                    spec.RuntimeArity,
                                    List.length loweredArguments
                                )
                            )
                    | None ->
                        match KnownPreludeSemantics.tryRuntimeSpecialBinaryOperatorSpec bindingName with
                        | Some specialSpec ->
                            let resultRepresentation =
                                if specialSpec.ReturnsBoolean then
                                    BackendRepBoolean
                                else
                                    fallbackResultRepresentation

                            if List.length loweredArguments = specialSpec.RuntimeArity then
                                Result.Ok(
                                    BackendCall(
                                        BackendName resolvedName,
                                        loweredArguments,
                                        makeCallingConvention
                                            specialSpec.RuntimeArity
                                            argumentRepresentations
                                            (Some resultRepresentation),
                                        resultRepresentation
                                    ),
                                    resultRepresentation
                                )
                            else
                                Result.Error(
                                    RuntimeIntrinsicArityMismatch(
                                        $"{moduleName}.{bindingName}",
                                        specialSpec.RuntimeArity,
                                        List.length loweredArguments
                                    )
                                )
                        | None ->
                            Result.Error(RuntimeCallTargetCouldNotBeLowered(moduleName, bindingName))
                | BackendGlobalBindingName(moduleName, bindingName, _) ->
                    let conventionResult =
                        match tryLookupBindingInfo moduleName bindingName with
                        | Some bindingInfo ->
                            let resultRepresentation =
                                bindingInfo.ReturnRepresentation
                                |> Option.defaultValue fallbackResultRepresentation

                            match resultRepresentation, List.length loweredArguments > bindingInfo.Arity with
                            | (BackendRepClosure _ | BackendRepOpaque(Some "Function")), true ->
                                let directArguments = loweredArguments |> List.take bindingInfo.Arity
                                let directArgumentRepresentations = argumentRepresentations |> List.take bindingInfo.Arity
                                let remainingArguments = loweredArguments |> List.skip bindingInfo.Arity
                                let remainingArgumentRepresentations = argumentRepresentations |> List.skip bindingInfo.Arity
                                let directConvention =
                                    makeCallingConvention
                                        bindingInfo.Arity
                                        directArgumentRepresentations
                                        (Some resultRepresentation)

                                let directCall =
                                    BackendCall(
                                        BackendName resolvedName,
                                        directArguments,
                                        directConvention,
                                        resultRepresentation
                                    )

                                let finalConvention =
                                    makeCallingConvention
                                        (List.length remainingArguments)
                                        remainingArgumentRepresentations
                                        (Some fallbackResultRepresentation)

                                Result.Ok(finalConvention, fallbackResultRepresentation, Some directCall, remainingArguments)
                            | _, false when List.length loweredArguments = bindingInfo.Arity ->
                                Result.Ok(
                                    makeCallingConvention
                                        bindingInfo.Arity
                                        argumentRepresentations
                                        (Some resultRepresentation),
                                    resultRepresentation,
                                    None,
                                    loweredArguments
                                )
                            | _ ->
                                Result.Error(RuntimeCallTargetArityMismatch(moduleName, bindingName, bindingInfo.Arity, List.length loweredArguments))
                        | None ->
                            Result.Error(RuntimeCallTargetCouldNotBeLowered(moduleName, bindingName))

                    conventionResult
                    |> Result.map (fun (convention, resultRepresentation, receiver, callArguments) ->
                        let callee =
                            receiver |> Option.defaultValue (BackendName resolvedName)

                        BackendCall(callee, callArguments, convention, resultRepresentation), resultRepresentation)
                | BackendLocalName _ ->
                    let convention =
                        makeCallingConvention
                            (List.length loweredArguments)
                            argumentRepresentations
                            (Some fallbackResultRepresentation)

                    Result.Ok(
                        BackendCall(BackendName resolvedName, loweredArguments, convention, fallbackResultRepresentation),
                        fallbackResultRepresentation
                    )

            let lowerNamedRuntimeCall
                (locals: Map<string, KBackendRepresentationClass>)
                (runtimeName: string)
                (loweredArguments: KBackendExpression list)
                (argumentRepresentations: KBackendRepresentationClass list)
                (fallbackResultRepresentation: KBackendRepresentationClass)
                : Result<KBackendExpression * KBackendRepresentationClass, BackendLoweringErrorEvidence> =
                let lowerCatalogIntrinsicRuntimeCall runtimeName =
                    let resolvedName =
                        BackendIntrinsicName(Stdlib.PreludeModuleText, runtimeName, Some fallbackResultRepresentation)

                    lowerResolvedCall loweredArguments argumentRepresentations fallbackResultRepresentation resolvedName

                let shouldPreferCatalogIntrinsic resolvedName =
                    match resolvedName with
                    | BackendGlobalBindingName(moduleName, bindingName, _)
                        when availableRuntimeIntrinsics.Contains bindingName ->
                        let bindingInfo = context.BindingInfos[moduleName, bindingName]

                        match IntrinsicCatalog.tryFindIntrinsicSpec bindingName with
                        | Some spec ->
                            bindingInfo.Arity > spec.RuntimeArity
                            && List.length loweredArguments = spec.RuntimeArity
                        | None ->
                            false
                    | _ ->
                        false

                match resolveRuntimeName runtimeModule locals [ runtimeName ] with
                | Result.Ok(resolvedName, _) when shouldPreferCatalogIntrinsic resolvedName ->
                    lowerCatalogIntrinsicRuntimeCall runtimeName
                | Result.Ok(resolvedName, _) ->
                    lowerResolvedCall loweredArguments argumentRepresentations fallbackResultRepresentation resolvedName
                | Result.Error _ when availableRuntimeIntrinsics.Contains runtimeName ->
                    lowerCatalogIntrinsicRuntimeCall runtimeName
                | Result.Error _ ->
                    Result.Error(UnresolvedRuntimeName runtimeName)

            let executedIntrinsicRepresentation runtimeName argumentRepresentations =
                match runtimeName, argumentRepresentations with
                | ("print" | "println" | "writeRef"), _ ->
                    Some BackendRepUnit
                | "primitiveIntToString", _ ->
                    Some BackendRepString
                | "unsafeConsume", _ ->
                    Some BackendRepUnit
                | "openFile", _ ->
                    Some(backendOpaqueRepresentation (Some "File"))
                | ("primitiveReadData" | "readData"), _ ->
                    Some BackendRepString
                | "primitiveCloseFile", _ ->
                    Some BackendRepUnit
                | "newRef", [ elementRepresentation ] ->
                    Some(BackendRepRef elementRepresentation)
                | "readRef", [ BackendRepRef elementRepresentation ] ->
                    Some elementRepresentation
                | "readRef", _ ->
                    Some(backendOpaqueRepresentation None)
                | _ ->
                    None

            let executedBindingRepresentation moduleName bindingName fallbackRepresentation =
                match tryLookupBindingInfo moduleName bindingName with
                | Some bindingInfo ->
                    match bindingInfo.ReturnRepresentation with
                    | Some BackendRepIOAction -> fallbackRepresentation
                    | Some representation -> representation
                    | None -> fallbackRepresentation
                | None ->
                    fallbackRepresentation

            let inferExecutedExpressionRepresentation
                (loweredExpression: KBackendExpression)
                (fallbackRepresentation: KBackendRepresentationClass)
                =
                match loweredExpression with
                | BackendPure(expression, _) ->
                    match expression with
                    | BackendLiteral(_, representation) -> representation
                    | BackendName(BackendLocalName(_, Some representation))
                    | BackendName(BackendGlobalBindingName(_, _, Some representation))
                    | BackendName(BackendIntrinsicName(_, _, Some representation)) -> representation
                    | _ -> fallbackRepresentation
                | BackendBind _
                | BackendThen _
                | BackendHandle _ ->
                    fallbackRepresentation
                | BackendCall(BackendName(BackendIntrinsicName(moduleName, bindingName, _)), _, convention, _) ->
                    match executedIntrinsicRepresentation bindingName convention.ParameterRepresentations with
                    | Some representation -> representation
                    | None -> executedBindingRepresentation moduleName bindingName fallbackRepresentation
                | BackendCall(BackendName(BackendGlobalBindingName(moduleName, bindingName, _)), _, _, _) ->
                    executedBindingRepresentation moduleName bindingName fallbackRepresentation
                | _ when fallbackRepresentation = BackendRepIOAction ->
                    BackendRepUnit
                | _ ->
                    fallbackRepresentation

            let lowerHandlerArgument argument =
                match argument with
                | KRuntimeEffectUnitArgument -> BackendEffectUnitArgument
                | KRuntimeEffectWildcardArgument -> BackendEffectWildcardArgument
                | KRuntimeEffectNameArgument name -> BackendEffectNameArgument name

            let validateOrPatternBindings
                (alternatives: (KBackendPattern * Map<string, KBackendRepresentationClass>) list)
                : Result<Map<string, KBackendRepresentationClass>, BackendLoweringErrorEvidence> =
                match alternatives with
                | [] ->
                    Result.Ok Map.empty
                | (_, firstBindings) :: rest ->
                    let firstNames = firstBindings |> Map.keys |> Set.ofSeq

                    rest
                    |> List.fold
                        (fun state (_, candidateBindings) ->
                            state
                            |> Result.bind (fun agreedBindings ->
                                let candidateNames = candidateBindings |> Map.keys |> Set.ofSeq

                                if candidateNames <> firstNames then
                                    Result.Error OrPatternAlternativesBindDifferentNames
                                else
                                    firstBindings
                                    |> Map.fold
                                        (fun comparisonState name representation ->
                                            comparisonState
                                            |> Result.bind (fun () ->
                                                let candidateRepresentation = candidateBindings[name]

                                                if candidateRepresentation = representation then
                                                    Result.Ok()
                                                else
                                                    Result.Error(OrPatternBinderRepresentationMismatch name)))
                                        (Result.Ok())
                                    |> Result.map (fun () -> agreedBindings))
                        )
                        (Result.Ok firstBindings)

            let rec lowerPattern
                (locals: Map<string, KBackendRepresentationClass>)
                (patternRepresentation: KBackendRepresentationClass)
                (pattern: KRuntimePattern)
                : Result<KBackendPattern * Map<string, KBackendRepresentationClass>, BackendLoweringErrorEvidence> =
                match pattern with
                | KRuntimeWildcardPattern ->
                    Result.Ok(BackendWildcardPattern, Map.empty)
                | KRuntimeLiteralPattern literal ->
                    Result.Ok(BackendLiteralPattern(literal, backendLiteralRepresentation literal), Map.empty)
                | KRuntimeNamePattern name ->
                    let binding: KBackendPatternBinding =
                        { Name = name
                          Representation = patternRepresentation }

                    Result.Ok(BackendBindPattern binding, Map.ofList [ name, patternRepresentation ])
                | KRuntimeOrPattern alternatives ->
                    alternatives
                    |> List.fold
                        (fun state alternative ->
                            state
                            |> Result.bind (fun loweredAlternatives ->
                                lowerPattern locals patternRepresentation alternative
                                |> Result.map (fun loweredAlternative -> loweredAlternative :: loweredAlternatives)))
                        (Result.Ok [])
                    |> Result.bind (fun loweredAlternatives ->
                        let loweredAlternatives = List.rev loweredAlternatives

                        validateOrPatternBindings loweredAlternatives
                        |> Result.map (fun agreedBindings ->
                            BackendOrPattern(loweredAlternatives |> List.map fst), agreedBindings))
                | KRuntimeConstructorPattern(nameSegments, argumentPatterns) ->
                    let constructorText = String.concat "." nameSegments

                    resolveRuntimeName runtimeModule locals nameSegments
                    |> Result.bind (fun (resolvedName, _) ->
                        match resolvedName with
                        | BackendConstructorName(moduleName, typeName, constructorName, tag, _, _) ->
                            let constructorInfo = context.ConstructorInfos[moduleName, constructorName]
                            let expectedArity = List.length constructorInfo.FieldRepresentations
                            let actualArity = List.length argumentPatterns

                            if expectedArity <> actualArity then
                                Result.Error(ConstructorPatternArityMismatch(constructorText, expectedArity, actualArity))
                            else
                                ((Result.Ok([], Map.empty)), List.zip argumentPatterns constructorInfo.FieldRepresentations)
                                ||> List.fold (fun state (argumentPattern, fieldRepresentation) ->
                                    state
                                    |> Result.bind (fun (patterns, discoveredLocals) ->
                                        lowerPattern locals fieldRepresentation argumentPattern
                                        |> Result.map (fun (loweredPattern, patternLocals) ->
                                            loweredPattern :: patterns,
                                            Map.fold (fun mapState key value -> Map.add key value mapState) discoveredLocals patternLocals)))
                                |> Result.map (fun (patterns, discoveredLocals) ->
                                    BackendConstructorPattern(moduleName, typeName, constructorName, tag, List.rev patterns),
                                    discoveredLocals)
                        | _ ->
                            Result.Error(PatternDoesNotResolveToConstructor constructorText))

            let rec lowerExpression
                (scopeLabel: string)
                (locals: Map<string, KBackendRepresentationClass>)
                (expression: KRuntimeExpression)
                : Result<KBackendExpression * KBackendRepresentationClass, BackendLoweringErrorEvidence> =
                match expression with
                | KRuntimeLiteral literal ->
                    let representation = backendLiteralRepresentation literal
                    Result.Ok(BackendLiteral(literal, representation), representation)
                | KRuntimeEffectLabel(labelName, interfaceId, labelId, operations) ->
                    let loweredOperations =
                        operations
                        |> List.map (fun operation ->
                            { OperationId = operation.OperationId
                              Name = operation.Name
                              ParameterArity = operation.ParameterArity
                              AllowsMultipleResumptions = effectResumptionAllowsMultiple operation.ResumptionQuantity })

                    Result.Ok(
                        BackendEffectLabel(labelName, interfaceId, labelId, loweredOperations, BackendRepEffectLabel),
                        BackendRepEffectLabel
                    )
                | KRuntimeEffectOperation(label, operationId, operationName) ->
                    lowerExpression scopeLabel locals label
                    |> Result.map (fun (loweredLabel, _) ->
                        BackendEffectOperation(loweredLabel, operationId, operationName, BackendRepEffectOperation),
                        BackendRepEffectOperation)
                | KRuntimeHandle(isDeep, label, body, returnClause, operationClauses) ->
                    let lowerClause (clause: KRuntimeEffectHandlerClause) =
                        let clauseLocals =
                            clause.Arguments
                            |> List.fold
                                (fun state argument ->
                                    match argument with
                                    | KRuntimeEffectNameArgument name ->
                                        Map.add name (backendOpaqueRepresentation None) state
                                    | KRuntimeEffectUnitArgument
                                    | KRuntimeEffectWildcardArgument ->
                                        state)
                                locals
                            |> fun state ->
                                clause.ResumptionName
                                |> Option.map (fun name -> Map.add name (backendOpaqueRepresentation (Some "Resumption")) state)
                                |> Option.defaultValue state

                        lowerExpression scopeLabel clauseLocals clause.Body
                        |> Result.map (fun (loweredClauseBody, _) ->
                            ({ OperationName = clause.OperationName
                               Arguments = clause.Arguments |> List.map lowerHandlerArgument
                               ResumptionName = clause.ResumptionName
                               Body = loweredClauseBody }: KBackendEffectHandlerClause))

                    lowerExpression scopeLabel locals label
                    |> Result.bind (fun (loweredLabel, _) ->
                        lowerExpression scopeLabel locals body
                        |> Result.bind (fun (loweredBody, _) ->
                            lowerClause returnClause
                            |> Result.bind (fun loweredReturnClause ->
                                operationClauses
                                |> List.fold
                                    (fun state clause ->
                                        state
                                        |> Result.bind (fun accumulated ->
                                            lowerClause clause
                                            |> Result.map (fun loweredClause -> accumulated @ [ loweredClause ])))
                                    (Result.Ok([]: KBackendEffectHandlerClause list))
                                |> Result.map (fun loweredOperationClauses ->
                                    BackendHandle(
                                        isDeep,
                                        loweredLabel,
                                        loweredBody,
                                        loweredReturnClause,
                                        loweredOperationClauses,
                                        BackendRepIOAction
                                    ),
                                    BackendRepIOAction))))
                | KRuntimeName segments ->
                    resolveRuntimeName runtimeModule locals segments
                    |> Result.bind (fun (resolvedName, representation) ->
                        match resolvedName with
                        | BackendConstructorName(moduleName, _, constructorName, _, arity, _) when arity > 0 ->
                            let constructorInfo = context.ConstructorInfos[moduleName, constructorName]
                            synthesizeConstructorClosure constructorInfo [] []
                        | _ ->
                            Result.Ok(BackendName resolvedName, representation))
                | KRuntimeClosure(parameters, body) ->
                    let parameterRepresentations: KBackendParameter list =
                        parameters
                        |> List.map (fun name ->
                            { Name = name
                              Representation = backendOpaqueRepresentation None })

                    let parameterLocals =
                        parameterRepresentations
                        |> List.map (fun parameter -> parameter.Name, parameter.Representation)
                        |> Map.ofList

                    let captureNames =
                        collectClosureCaptures (locals |> Map.toSeq |> Seq.map fst |> Set.ofSeq) (parameters |> Set.ofList) body
                        |> Set.toList
                        |> List.sort

                    let captures: KBackendCapture list =
                        captureNames
                        |> List.choose (fun name ->
                            locals
                            |> Map.tryFind name
                            |> Option.map (fun representation ->
                                    { Name = name
                                      Representation = representation }))

                    let environmentLayoutName = freshEnvironmentLayoutName scopeLabel

                    environmentLayouts.Add
                        { Name = environmentLayoutName
                          Slots = captures }

                    let closureLocals =
                        captures
                        |> List.map (fun capture -> capture.Name, capture.Representation)
                        |> List.append (parameterRepresentations |> List.map (fun parameter -> parameter.Name, parameter.Representation))
                        |> Map.ofList

                    lowerExpression $"{scopeLabel}$closure{nextEnvironmentLayoutId}" closureLocals body
                    |> Result.map (fun (loweredBody, resultRepresentation) ->
                        let convention =
                            { RuntimeArity = List.length parameters
                              ParameterRepresentations = parameterRepresentations |> List.map (fun parameter -> parameter.Representation)
                              ResultRepresentation = Some resultRepresentation
                              RetainedDictionaryParameters = [] }

                        let representation = BackendRepClosure environmentLayoutName

                        BackendClosure(
                            parameterRepresentations,
                            captures,
                            environmentLayoutName,
                            loweredBody,
                            convention,
                            representation
                        ),
                        representation)
                | KRuntimeIfThenElse(condition, whenTrue, whenFalse) ->
                    lowerExpression scopeLabel locals condition
                    |> Result.bind (fun (loweredCondition, _) ->
                        lowerExpression scopeLabel locals whenTrue
                        |> Result.bind (fun (loweredTrue, trueRepresentation) ->
                            lowerExpression scopeLabel locals whenFalse
                            |> Result.map (fun (loweredFalse, falseRepresentation) ->
                                let resultRepresentation =
                                    mergeBackendRepresentations trueRepresentation falseRepresentation

                                BackendIfThenElse(
                                    loweredCondition,
                                    loweredTrue,
                                    loweredFalse,
                                    resultRepresentation
                                ),
                                resultRepresentation)))
                | KRuntimeMatch(scrutinee, cases) ->
                    lowerExpression scopeLabel locals scrutinee
                    |> Result.bind (fun (loweredScrutinee, scrutineeRepresentation) ->
                        ((Result.Ok([], [])), cases)
                        ||> List.fold (fun state caseClause ->
                            state
                            |> Result.bind (fun (loweredCases, caseRepresentations) ->
                                lowerPattern locals scrutineeRepresentation caseClause.Pattern
                                |> Result.bind (fun (loweredPattern, discoveredLocals) ->
                                    let caseLocals =
                                        Map.fold (fun mapState key value -> Map.add key value mapState) locals discoveredLocals

                                    let loweredGuardResult =
                                        caseClause.Guard
                                        |> Option.map (fun guard ->
                                            lowerExpression scopeLabel caseLocals guard
                                            |> Result.map (fun (loweredGuard, _) -> Some loweredGuard))
                                        |> Option.defaultValue (Result.Ok None)

                                    loweredGuardResult
                                    |> Result.bind (fun loweredGuard ->
                                        lowerExpression scopeLabel caseLocals caseClause.Body
                                        |> Result.map (fun (loweredBody, bodyRepresentation) ->
                                            ({ Pattern = loweredPattern
                                               Guard = loweredGuard
                                               Body = loweredBody }: KBackendMatchCase)
                                            :: loweredCases,
                                            bodyRepresentation :: caseRepresentations)))))
                        |> Result.map (fun (loweredCases, caseRepresentations) ->
                            let resultRepresentation =
                                match List.rev caseRepresentations with
                                | [] ->
                                    backendOpaqueRepresentation None
                                | first :: rest ->
                                    rest |> List.fold mergeBackendRepresentations first

                            BackendMatch(loweredScrutinee, List.rev loweredCases, resultRepresentation),
                            resultRepresentation))
                | KRuntimeExecute inner ->
                    lowerExpression scopeLabel locals inner
                    |> Result.map (fun (loweredInner, innerRepresentation) ->
                        let resultRepresentation =
                            inferExecutedExpressionRepresentation loweredInner innerRepresentation

                        BackendExecute(loweredInner, resultRepresentation), resultRepresentation)
                | KRuntimeLet(bindingName, (KRuntimeClosure _ as value), body) ->
                    let recursiveLocals =
                        locals |> Map.add bindingName (backendOpaqueRepresentation (Some "Function"))

                    lowerExpression scopeLabel recursiveLocals value
                    |> Result.bind (fun (loweredValue, valueRepresentation) ->
                        let bindingParameter: KBackendParameter =
                            { Name = bindingName
                              Representation = valueRepresentation }

                        let bodyLocals = locals |> Map.add bindingName valueRepresentation

                        lowerExpression scopeLabel bodyLocals body
                        |> Result.map (fun (loweredBody, bodyRepresentation) ->
                            BackendLet(bindingParameter, loweredValue, loweredBody, bodyRepresentation), bodyRepresentation))
                | KRuntimeLet(bindingName, value, body) ->
                    lowerExpression scopeLabel locals value
                    |> Result.bind (fun (loweredValue, valueRepresentation) ->
                        let bindingParameter: KBackendParameter =
                            { Name = bindingName
                              Representation = valueRepresentation }

                        let bodyLocals = locals |> Map.add bindingName valueRepresentation

                        lowerExpression scopeLabel bodyLocals body
                        |> Result.map (fun (loweredBody, bodyRepresentation) ->
                            BackendLet(bindingParameter, loweredValue, loweredBody, bodyRepresentation), bodyRepresentation))
                | KRuntimeDoScope(nestedScopeLabel, body) ->
                    lowerExpression nestedScopeLabel locals body
                | KRuntimeScheduleExit(_, action, body) ->
                    lowerExpression scopeLabel locals body
                    |> Result.bind (fun (loweredBody, bodyRepresentation) ->
                        lowerExitAction scopeLabel locals action
                        |> Result.map (fun (loweredAction, _) ->
                            let resultBinding: KBackendParameter =
                                { Name = "__kappa_scope_result"
                                  Representation = bodyRepresentation }

                            let resultName =
                                BackendName(BackendLocalName(resultBinding.Name, Some bodyRepresentation))

                            BackendLet(
                                resultBinding,
                                loweredBody,
                                BackendSequence(loweredAction, resultName, bodyRepresentation),
                                bodyRepresentation
                            ),
                            bodyRepresentation))
                | KRuntimeSequence(first, second) ->
                    lowerExpression scopeLabel locals first
                    |> Result.bind (fun (loweredFirst, _) ->
                        lowerExpression scopeLabel locals second
                        |> Result.map (fun (loweredSecond, secondRepresentation) ->
                            BackendSequence(loweredFirst, loweredSecond, secondRepresentation), secondRepresentation))
                | KRuntimeWhile(condition, body) ->
                    lowerExpression scopeLabel locals condition
                    |> Result.bind (fun (loweredCondition, _) ->
                        lowerExpression scopeLabel locals body
                        |> Result.map (fun (loweredBody, _) ->
                            BackendWhile(loweredCondition, loweredBody), BackendRepUnit))
                | KRuntimeApply(callee, arguments) ->
                    let lowerArguments =
                        arguments
                        |> List.fold
                            (fun state argument ->
                                state
                                |> Result.bind (fun (loweredArguments, argumentRepresentations) ->
                                    lowerExpression scopeLabel locals argument
                                    |> Result.map (fun (loweredArgument, argumentRepresentation) ->
                                        loweredArgument :: loweredArguments,
                                        argumentRepresentation :: argumentRepresentations)))
                            (Result.Ok([], []))

                    lowerArguments
                    |> Result.bind (fun (loweredArguments, argumentRepresentations) ->
                        let loweredArguments = List.rev loweredArguments
                        let argumentRepresentations = List.rev argumentRepresentations
                        let fallbackResultRepresentation = backendOpaqueRepresentation None

                        match callee with
                        | KRuntimeName [ "pure" ] ->
                            match loweredArguments with
                            | [ loweredValue ] ->
                                Result.Ok(BackendPure(loweredValue, BackendRepIOAction), BackendRepIOAction)
                            | _ ->
                                Result.Error(RuntimeIntrinsicArityMismatch("pure", 1, List.length loweredArguments))
                        | KRuntimeName [ ">>=" ] ->
                            match loweredArguments with
                            | [ loweredAction; loweredBinder ] ->
                                Result.Ok(BackendBind(loweredAction, loweredBinder, BackendRepIOAction), BackendRepIOAction)
                            | _ ->
                                Result.Error(RuntimeIntrinsicArityMismatch(">>=", 2, List.length loweredArguments))
                        | KRuntimeName [ ">>" ] ->
                            match loweredArguments with
                            | [ loweredFirst; loweredSecond ] ->
                                Result.Ok(BackendThen(loweredFirst, loweredSecond, BackendRepIOAction), BackendRepIOAction)
                            | _ ->
                                Result.Error(RuntimeIntrinsicArityMismatch(">>", 2, List.length loweredArguments))
                        | KRuntimeName [ "runPure" ] ->
                            match loweredArguments with
                            | [ loweredAction ] ->
                                Result.Ok(BackendExecute(loweredAction, fallbackResultRepresentation), fallbackResultRepresentation)
                            | _ ->
                                Result.Error(RuntimeIntrinsicArityMismatch("runPure", 1, List.length loweredArguments))
                        | KRuntimeName [ runtimeName ] ->
                            lowerNamedRuntimeCall
                                locals
                                runtimeName
                                loweredArguments
                                argumentRepresentations
                                fallbackResultRepresentation
                        | KRuntimeName segments ->
                            resolveRuntimeName runtimeModule locals segments
                            |> Result.bind (fun (resolvedName, _) ->
                                lowerResolvedCall
                                    loweredArguments
                                    argumentRepresentations
                                    fallbackResultRepresentation
                                    resolvedName)
                        | _ ->
                            lowerExpression scopeLabel locals callee
                            |> Result.map (fun (loweredCallee, _) ->
                                let resultRepresentation = fallbackResultRepresentation
                                let convention =
                                    makeCallingConvention
                                        (List.length loweredArguments)
                                        argumentRepresentations
                                        (Some resultRepresentation)

                                BackendCall(loweredCallee, loweredArguments, convention, resultRepresentation), resultRepresentation))
                | KRuntimeUnary(operatorName, operand) ->
                    lowerExpression scopeLabel locals operand
                    |> Result.bind (fun (loweredOperand, operandRepresentation) ->
                        let resultRepresentation =
                            match operatorName with
                            | "not" -> BackendRepBoolean
                            | "negate" -> operandRepresentation
                            | _ -> backendOpaqueRepresentation None

                        lowerNamedRuntimeCall
                            locals
                            operatorName
                            [ loweredOperand ]
                            [ operandRepresentation ]
                            resultRepresentation)
                | KRuntimeBinary(left, operatorName, right) ->
                    match operatorName with
                    | "&&" ->
                        lowerExpression scopeLabel locals left
                        |> Result.bind (fun (loweredLeft, _) ->
                            lowerExpression scopeLabel locals right
                            |> Result.bind (fun (loweredRight, _) ->
                                lowerExpression scopeLabel locals (KRuntimeName [ "False" ])
                                |> Result.map (fun (loweredFalse, _) ->
                                    BackendIfThenElse(loweredLeft, loweredRight, loweredFalse, BackendRepBoolean),
                                    BackendRepBoolean)))
                    | "||" ->
                        lowerExpression scopeLabel locals left
                        |> Result.bind (fun (loweredLeft, _) ->
                            lowerExpression scopeLabel locals (KRuntimeName [ "True" ])
                            |> Result.bind (fun (loweredTrue, _) ->
                                lowerExpression scopeLabel locals right
                                |> Result.map (fun (loweredRight, _) ->
                                    BackendIfThenElse(loweredLeft, loweredTrue, loweredRight, BackendRepBoolean),
                                    BackendRepBoolean)))
                    | "is" ->
                        lowerExpression scopeLabel locals left
                        |> Result.bind (fun (loweredLeft, leftRepresentation) ->
                            match right with
                            | KRuntimeName constructorSegments ->
                                resolveRuntimeConstructor runtimeModule constructorSegments
                                |> Result.bind (fun (resolvedConstructor, constructorRepresentation) ->
                                    lowerNamedRuntimeCall
                                        locals
                                        operatorName
                                        [ loweredLeft; BackendName resolvedConstructor ]
                                        [ leftRepresentation; constructorRepresentation ]
                                        BackendRepBoolean)
                            | _ ->
                                Result.Error RuntimeTagTestRequiresConstructorName)
                    | _ ->
                        lowerExpression scopeLabel locals left
                        |> Result.bind (fun (loweredLeft, leftRepresentation) ->
                            lowerExpression scopeLabel locals right
                            |> Result.bind (fun (loweredRight, rightRepresentation) ->
                                let resultRepresentation =
                                    match operatorName with
                                    | "=="
                                    | "!="
                                    | "<"
                                    | ">"
                                    | "<="
                                    | ">="
                                    | "is" ->
                                        BackendRepBoolean
                                    | "+" | "-" | "*" | "/" ->
                                        mergeBackendRepresentations leftRepresentation rightRepresentation
                                    | _ ->
                                        backendOpaqueRepresentation None

                                lowerNamedRuntimeCall
                                    locals
                                    operatorName
                                    [ loweredLeft; loweredRight ]
                                    [ leftRepresentation; rightRepresentation ]
                                    resultRepresentation))
                | KRuntimeDictionaryValue(moduleName, traitName, instanceKey, captures) ->
                    let representation = BackendRepDictionary traitName

                    captures
                    |> List.fold
                        (fun stateResult captureExpression ->
                            stateResult
                            |> Result.bind (fun loweredCaptures ->
                                lowerExpression scopeLabel locals captureExpression
                                |> Result.map (fun (loweredCapture, _) -> loweredCapture :: loweredCaptures)))
                        (Result.Ok [])
                    |> Result.map (fun loweredCaptures ->
                        BackendDictionaryValue(
                            moduleName,
                            traitName,
                            instanceKey,
                            List.rev loweredCaptures,
                            representation
                        ),
                        representation)
                | KRuntimeTraitCall(traitName, memberName, dictionary, arguments) ->
                    lowerExpression scopeLabel locals dictionary
                    |> Result.bind (fun (loweredDictionary, _) ->
                        let lowerArguments =
                            arguments
                            |> List.fold
                                (fun state argument ->
                                    state
                                    |> Result.bind (fun (loweredArguments, argumentRepresentations) ->
                                        lowerExpression scopeLabel locals argument
                                        |> Result.map (fun (loweredArgument, argumentRepresentation) ->
                                            loweredArgument :: loweredArguments,
                                            argumentRepresentation :: argumentRepresentations)))
                                (Result.Ok([], []))

                        lowerArguments
                        |> Result.map (fun (loweredArguments, argumentRepresentations) ->
                            let loweredArguments = List.rev loweredArguments
                            let _argumentRepresentations = List.rev argumentRepresentations
                            let resultRepresentation = backendOpaqueRepresentation None

                            BackendTraitCall(
                                traitName,
                                memberName,
                                loweredDictionary,
                                loweredArguments,
                                resultRepresentation
                            ),
                            resultRepresentation))
                | KRuntimePrefixedString(prefix, parts) ->
                    let rec lowerStringParts parts =
                        match parts with
                        | [] ->
                            Result.Ok []
                        | KRuntimeStringText text :: rest ->
                            lowerStringParts rest
                            |> Result.map (fun loweredRest -> BackendStringText text :: loweredRest)
                        | KRuntimeStringInterpolation(inner, _) :: rest ->
                            lowerExpression scopeLabel locals inner
                            |> Result.bind (fun (loweredInner, _) ->
                                lowerStringParts rest
                                |> Result.map (fun loweredRest -> BackendStringInterpolation loweredInner :: loweredRest))

                    lowerStringParts parts
                    |> Result.map (fun loweredParts ->
                        let representation = BackendRepString
                        BackendPrefixedString(prefix, loweredParts, representation), representation)

            and lowerExitAction
                (scopeLabel: string)
                (locals: Map<string, KBackendRepresentationClass>)
                (action: KRuntimeExitAction)
                : Result<KBackendExpression * KBackendRepresentationClass, BackendLoweringErrorEvidence> =
                let cleanupExpression =
                    match action with
                    | KRuntimeDeferred expression ->
                        KRuntimeExecute expression
                    | KRuntimeRelease(_, KRuntimeTraitCall(traitName, memberName, dictionary, []), resource) ->
                        KRuntimeExecute(KRuntimeTraitCall(traitName, memberName, dictionary, [ resource ]))
                    | KRuntimeRelease(_, release, resource) ->
                        KRuntimeExecute(KRuntimeApply(release, [ resource ]))

                lowerExpression scopeLabel locals cleanupExpression

            let lowerBinding (binding: KRuntimeBinding) =
                let bindingInfo = context.BindingInfos[runtimeModule.Name, binding.Name]
                let exported = List.contains binding.Name runtimeModule.Exports
                let entryPoint = exported && bindingInfo.Arity = 0 && not bindingInfo.Intrinsic

                let parameterRepresentations: KBackendParameter list =
                    if binding.Intrinsic then
                        List.init bindingInfo.Arity (fun _ ->
                            { Name = "_"
                              Representation = backendOpaqueRepresentation None })
                    else
                        binding.Parameters
                        |> List.map (fun parameter ->
                            { Name = parameter.Name
                              Representation =
                                tryBackendRepresentationFromTypeExpr parameter.Type
                                |> Option.defaultValue (backendOpaqueRepresentation None) })

                let convention =
                    { RuntimeArity = bindingInfo.Arity
                      ParameterRepresentations = parameterRepresentations |> List.map (fun parameter -> parameter.Representation)
                      ResultRepresentation = bindingInfo.ReturnRepresentation
                      RetainedDictionaryParameters = [] }

                let bodyResult =
                    if binding.Intrinsic then
                        Result.Ok None
                    else
                        let locals =
                            parameterRepresentations
                            |> List.map (fun parameter -> parameter.Name, parameter.Representation)
                            |> Map.ofList

                        match binding.Body with
                        | None ->
                            Result.Ok None
                        | Some body ->
                            lowerExpression $"{runtimeModule.Name}.{binding.Name}" locals body
                            |> Result.map (fun (loweredBody, _) -> Some loweredBody)

                bodyResult
                |> Result.map (fun body ->
                    { Name = binding.Name
                      Parameters = parameterRepresentations
                      CallingConvention = convention
                      ReturnRepresentation = bindingInfo.ReturnRepresentation
                      EnvironmentLayout = None
                      Intrinsic = binding.Intrinsic
                      Exported = exported
                      EntryPoint = entryPoint
                      ControlForm = StructuredExpression
                      Body = body
                      Provenance = binding.Provenance })

            let backendFunctions =
                ((Result.Ok []), runtimeModule.Bindings)
                ||> List.fold (fun state binding ->
                    state
                    |> Result.bind (fun loweredBindings ->
                        lowerBinding binding
                        |> Result.map (fun loweredBinding -> loweredBinding :: loweredBindings)))
                |> Result.map List.rev

            let dataLayouts =
                context.ConstructorInfos
                |> Map.toList
                |> List.choose (fun ((moduleName, _), constructorInfo) ->
                    if String.Equals(moduleName, runtimeModule.Name, StringComparison.Ordinal) then
                        Some constructorInfo
                    else
                        None)
                |> List.groupBy (fun constructorInfo -> constructorInfo.TypeName)
                |> List.map (fun (typeName, constructors) ->
                    { TypeName = typeName
                      RepresentationClass = "tagged-object"
                      TagEncoding = "ordinal"
                      Constructors =
                        constructors
                        |> List.sortBy (fun constructor -> constructor.Tag)
                        |> List.map (fun constructor ->
                            { Name = constructor.Name
                              Tag = constructor.Tag
                              FieldRepresentations = constructor.FieldRepresentations
                              Provenance = constructor.Provenance })
                      Provenance = constructors.Head.Provenance })
                |> List.sortBy (fun layout -> layout.TypeName)

            backendFunctions
            |> Result.map (fun backendFunctions ->
                let backendTraitInstances =
                    runtimeModule.TraitInstances
                    |> List.map (fun instanceInfo ->
                        { ModuleName = runtimeModule.Name
                          TraitName = instanceInfo.TraitName
                          InstanceKey = instanceInfo.InstanceKey
                          HeadTypes = instanceInfo.HeadTypes
                          MemberBindings = instanceInfo.MemberBindings })

                { Name = runtimeModule.Name
                  SourceFile = runtimeModule.SourceFile
                  Imports = runtimeModule.Imports
                  Exports = runtimeModule.Exports
                  EntryPoints =
                    backendFunctions
                    |> List.filter (fun binding -> binding.EntryPoint)
                    |> List.map (fun binding -> binding.Name)
                  IntrinsicTerms = runtimeModule.IntrinsicTerms
                  DataLayouts = dataLayouts
                  EnvironmentLayouts = environmentLayouts |> Seq.toList
                  TraitInstances = backendTraitInstances
                  Functions = backendFunctions })

        (([], []), kRuntimeIR)
        ||> List.fold (fun (modules, diagnostics) runtimeModule ->
            match lowerModule runtimeModule with
            | Result.Ok backendModule ->
                backendModule :: modules, diagnostics
            | Result.Error issue ->
                modules, diagnostics @ [ loweringDiagnostic runtimeModule None issue ])
        |> fun (modules, diagnostics) -> List.rev modules, diagnostics
