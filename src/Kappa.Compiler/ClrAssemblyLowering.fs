namespace Kappa.Compiler

open System

module internal ClrAssemblyLowering =
    let private qualifiedName (moduleName: string) (memberName: string) =
        moduleName.Split('.', StringSplitOptions.RemoveEmptyEntries)
        |> Array.toList
        |> fun segments -> segments @ [ memberName ]

    let rec private lowerExpression (expression: KBackendExpression) =
        match expression with
        | BackendLiteral(literal, _) ->
            KRuntimeLiteral literal
        | BackendName resolvedName ->
            lowerResolvedName resolvedName
        | BackendClosure(parameters, _, _, body, _, _) ->
            KRuntimeClosure(parameters |> List.map (fun parameter -> parameter.Name), lowerExpression body)
        | BackendIfThenElse(condition, whenTrue, whenFalse, _) ->
            KRuntimeIfThenElse(lowerExpression condition, lowerExpression whenTrue, lowerExpression whenFalse)
        | BackendMatch(scrutinee, cases, _) ->
            KRuntimeMatch(
                lowerExpression scrutinee,
                cases
                |> List.map (fun caseClause ->
                    { Pattern = lowerPattern caseClause.Pattern
                      Body = lowerExpression caseClause.Body })
            )
        | BackendExecute(inner, _) ->
            KRuntimeExecute(lowerExpression inner)
        | BackendLet(binding, value, BackendSequence(cleanup, BackendName(BackendLocalName(resultName, _)), _), _)
            when binding.Name = "__kappa_scope_result" && resultName = binding.Name ->
            KRuntimeScheduleExit("__kappa_scope", lowerExitAction cleanup, lowerExpression value)
        | BackendLet(binding, value, body, _) ->
            KRuntimeLet(binding.Name, lowerExpression value, lowerExpression body)
        | BackendSequence(first, second, _) ->
            KRuntimeSequence(lowerExpression first, lowerExpression second)
        | BackendWhile(condition, body) ->
            KRuntimeWhile(lowerExpression condition, lowerExpression body)
        | BackendCall(BackendName(BackendIntrinsicName(_, operatorName, _)), [ operand ], _, _)
            when IntrinsicCatalog.isBuiltinUnaryIntrinsic operatorName ->
            KRuntimeUnary(operatorName, lowerExpression operand)
        | BackendCall(BackendName(BackendIntrinsicName(_, operatorName, _)), [ left; right ], _, _)
            when IntrinsicCatalog.isBuiltinBinaryOperator operatorName ->
            KRuntimeBinary(lowerExpression left, operatorName, lowerExpression right)
        | BackendCall(BackendName(BackendConstructorName(moduleName, _, constructorName, _, _, _)), arguments, _, _) ->
            let loweredArguments = arguments |> List.map lowerExpression

            match loweredArguments with
            | [] ->
                KRuntimeName(qualifiedName moduleName constructorName)
            | _ ->
                KRuntimeApply(KRuntimeName(qualifiedName moduleName constructorName), loweredArguments)
        | BackendCall(callee, arguments, _, _) ->
            KRuntimeApply(lowerExpression callee, arguments |> List.map lowerExpression)
        | BackendDictionaryValue(moduleName, traitName, instanceKey, _) ->
            KRuntimeDictionaryValue(moduleName, traitName, instanceKey)
        | BackendTraitCall(traitName, memberName, dictionary, arguments, _) ->
            KRuntimeTraitCall(traitName, memberName, lowerExpression dictionary, arguments |> List.map lowerExpression)
        | BackendConstructData(moduleName, _, constructorName, _, fields, _) ->
            let loweredFields = fields |> List.map lowerExpression

            match loweredFields with
            | [] ->
                KRuntimeName(qualifiedName moduleName constructorName)
            | _ ->
                KRuntimeApply(KRuntimeName(qualifiedName moduleName constructorName), loweredFields)
        | BackendPrefixedString(prefix, parts, _) ->
            KRuntimePrefixedString(prefix, parts |> List.map lowerStringPart)

    and private lowerResolvedName resolvedName =
        match resolvedName with
        | BackendLocalName(name, _) ->
            KRuntimeName [ name ]
        | BackendGlobalBindingName(moduleName, bindingName, _) ->
            KRuntimeName(qualifiedName moduleName bindingName)
        | BackendIntrinsicName(_, bindingName, _) ->
            KRuntimeName [ bindingName ]
        | BackendConstructorName(moduleName, _, constructorName, _, _, _) ->
            KRuntimeName(qualifiedName moduleName constructorName)

    and private lowerPattern pattern =
        match pattern with
        | BackendWildcardPattern ->
            KRuntimeWildcardPattern
        | BackendBindPattern binding ->
            KRuntimeNamePattern binding.Name
        | BackendLiteralPattern(literal, _) ->
            KRuntimeLiteralPattern literal
        | BackendConstructorPattern(moduleName, _, constructorName, _, fieldPatterns) ->
            KRuntimeConstructorPattern(qualifiedName moduleName constructorName, fieldPatterns |> List.map lowerPattern)

    and private lowerStringPart part =
        match part with
        | BackendStringText text ->
            KRuntimeStringText text
        | BackendStringInterpolation expression ->
            KRuntimeStringInterpolation(lowerExpression expression)

    and private lowerExitAction cleanup =
        match cleanup with
        | BackendExecute(BackendTraitCall(traitName, memberName, dictionary, [ resource ], _), _) ->
            KRuntimeRelease(None, KRuntimeTraitCall(traitName, memberName, lowerExpression dictionary, []), lowerExpression resource)
        | BackendExecute(BackendCall(release, [ resource ], _, _), _) ->
            KRuntimeRelease(None, lowerExpression release, lowerExpression resource)
        | BackendExecute(expression, _) ->
            KRuntimeDeferred(lowerExpression expression)
        | _ ->
            KRuntimeDeferred(lowerExpression cleanup)

    let private typeTextFromRepresentation representation =
        let rec render rep =
            match rep with
            | BackendRepInt64 -> Some "Int"
            | BackendRepFloat64 -> Some "Float"
            | BackendRepBoolean -> Some "Bool"
            | BackendRepString -> Some "String"
            | BackendRepChar -> Some "Char"
            | BackendRepUnit -> Some "Unit"
            | BackendRepRef elementRepresentation ->
                render elementRepresentation |> Option.map (fun elementText -> $"Ref {elementText}")
            | BackendRepDictionary traitName ->
                Some(TraitRuntime.dictionaryTypeName traitName)
            | BackendRepTaggedData(moduleName, typeName) ->
                Some($"{moduleName}.{typeName}")
            | BackendRepIOAction ->
                Some "IO Unit"
            | BackendRepClosure _
            | BackendRepOpaque _ ->
                None

        render representation

    let private lowerBinding (existingBinding: ClrAssemblyBinding option) (backendFunction: KBackendFunction) =
        match existingBinding with
        | Some binding when backendFunction.Intrinsic ->
            { binding with Intrinsic = true; Body = None }
        | Some binding ->
            { binding with
                Intrinsic = false
                Body = backendFunction.Body |> Option.map lowerExpression }
        | None ->
            { Name = backendFunction.Name
              Parameters =
                backendFunction.Parameters
                |> List.map (fun parameter ->
                    { Name = parameter.Name
                      TypeText = typeTextFromRepresentation parameter.Representation })
              ReturnTypeText = backendFunction.ReturnRepresentation |> Option.bind typeTextFromRepresentation
              Body = backendFunction.Body |> Option.map lowerExpression
              Intrinsic = backendFunction.Intrinsic
              Provenance = backendFunction.Provenance }

    let private mergeModule
        (shellModules: Map<string, ClrAssemblyModule>)
        (backendModule: KBackendModule)
        =
        let shellModule =
            shellModules |> Map.tryFind backendModule.Name

        let shellBindingsByName =
            shellModule
            |> Option.map (fun moduleDump -> moduleDump.Bindings |> List.map (fun binding -> binding.Name, binding) |> Map.ofList)
            |> Option.defaultValue Map.empty

        let backendBindingNames =
            backendModule.Functions
            |> List.map (fun binding -> binding.Name)
            |> Set.ofList

        let mergedBindings =
            let backendBindings =
                backendModule.Functions
                |> List.map (fun backendFunction ->
                    let existing = shellBindingsByName |> Map.tryFind backendFunction.Name
                    lowerBinding existing backendFunction)

            let shellOnlyBindings =
                shellModule
                |> Option.map (fun moduleDump ->
                    moduleDump.Bindings
                    |> List.filter (fun binding -> not (backendBindingNames.Contains binding.Name)))
                |> Option.defaultValue []

            backendBindings @ shellOnlyBindings

        match shellModule with
        | Some moduleDump ->
            { moduleDump with
                Bindings = mergedBindings
                Imports = backendModule.Imports
                Exports = backendModule.Exports
                IntrinsicTerms = backendModule.IntrinsicTerms }
        | None ->
            { Name = backendModule.Name
              SourceFile = backendModule.SourceFile
              Imports = backendModule.Imports
              Exports = backendModule.Exports
              IntrinsicTerms = backendModule.IntrinsicTerms
              DataTypes = []
              TraitInstances = []
              Bindings = mergedBindings }

    let lowerModules (runtimeModules: KRuntimeModule list) (backendModules: KBackendModule list) =
        let shellModules =
            runtimeModules
            |> List.map ClrAssemblyIR.ofRuntimeModule
            |> List.map (fun moduleDump -> moduleDump.Name, moduleDump)
            |> Map.ofList

        backendModules
        |> List.map (mergeModule shellModules)
