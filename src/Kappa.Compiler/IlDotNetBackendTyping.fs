namespace Kappa.Compiler

open System
open System.Collections.Generic
open System.IO
open System.Reflection
open System.Reflection.Emit
open System.Runtime.CompilerServices

// Infers and resolves CLR-side types needed before IL emission.
module internal IlDotNetBackendTyping =
    open IlDotNetBackendModel
    open IlDotNetBackendInput

    let internal buildEnvironment (modules: ClrAssemblyModule list) =
        let rawSkeletons, rawDataTypes = buildRawModuleSkeletons modules

        resolveDataTypes rawSkeletons rawDataTypes
        |> Result.bind (fun resolvedDataTypes ->
            let rawModules = attachResolvedDataTypes rawSkeletons resolvedDataTypes
            let declaredBindingLookup = buildDeclaredBindingLookup modules
            buildTraitInstances rawModules modules
            |> Result.bind (fun traitInstances ->
                let cache = Dictionary<string * string, BindingInfo>()

                let tryResolveDeclaredBindingTypes currentModule bindingName =
                    match declaredBindingLookup |> Map.tryFind (currentModule, bindingName) with
                    | Some declaredTexts ->
                        resolveDeclaredBindingTypes rawModules currentModule declaredTexts |> Result.map Some
                    | None ->
                        Result.Ok None

                let rec inferBindingInfo currentModule bindingName active =
                    let cacheKey = currentModule, bindingName

                    match cache.TryGetValue(cacheKey) with
                    | true, cached ->
                        Result.Ok cached
                    | _ when Set.contains cacheKey active ->
                        Result.Error $"IL backend recursive type inference is not implemented yet for '{currentModule}.{bindingName}'."
                    | _ ->
                        match rawModules |> Map.tryFind currentModule |> Option.bind (fun moduleInfo -> moduleInfo.Bindings |> Map.tryFind bindingName) with
                        | None ->
                            Result.Error $"IL backend could not resolve binding '{currentModule}.{bindingName}'."
                        | Some binding ->
                            result {
                                let! declaredTypes = tryResolveDeclaredBindingTypes currentModule bindingName

                                match binding.Parameters, declaredTypes, binding.Body with
                                | [], Some declared, None when declared.ParameterTypes |> Option.exists (List.isEmpty >> not) ->
                                    return!
                                        Result.Error
                                            $"IL backend expected zero-argument binding '{currentModule}.{bindingName}' to have a zero-argument declaration."
                                | [], declared, None ->
                                    match declared |> Option.bind (fun info -> info.ReturnType) with
                                    | Some declaredReturnType ->
                                        let info =
                                            { Binding = binding
                                              ParameterTypes = []
                                              ReturnType = declaredReturnType
                                              TypeParameters = bindingTypeParameters [] declaredReturnType
                                              EmittedMethodName = emittedMethodName bindingName }

                                        cache[cacheKey] <- info
                                        return info
                                    | None ->
                                        return!
                                            Result.Error
                                                $"IL backend requires a declared return type for bodyless binding '{currentModule}.{bindingName}'."
                                | [], declared, Some body ->
                                    let declaredReturnType =
                                        declared |> Option.bind (fun info -> info.ReturnType)

                                    let allowedTypeParameters =
                                        declaredReturnType
                                        |> Option.map (collectTypeParameters)
                                        |> Option.defaultValue Set.empty

                                    let! bodyType =
                                        inferExpressionType
                                            currentModule
                                            Map.empty
                                            (Set.add cacheKey active)
                                            allowedTypeParameters
                                            declaredReturnType
                                            body

                                    match declaredReturnType with
                                    | Some expectedReturnType when expectedReturnType <> bodyType ->
                                        return!
                                            Result.Error
                                                $"IL backend expected '{currentModule}.{bindingName}' to return {formatIlType expectedReturnType}, but the body returns {formatIlType bodyType}."
                                    | _ ->
                                        let info =
                                            { Binding = binding
                                              ParameterTypes = []
                                              ReturnType = declaredReturnType |> Option.defaultValue bodyType
                                              TypeParameters =
                                                bindingTypeParameters [] (declaredReturnType |> Option.defaultValue bodyType)
                                              EmittedMethodName = emittedMethodName bindingName }

                                        cache[cacheKey] <- info
                                        return info
                                | parameters, Some { ParameterTypes = None }, _ ->
                                    return!
                                        Result.Error
                                            $"IL backend currently requires parameter types for '{currentModule}.{bindingName}'."
                                | parameters, None, _ ->
                                    return!
                                        Result.Error
                                            $"IL backend currently requires declared types for parameterized binding '{currentModule}.{bindingName}'."
                                | parameters, Some { ParameterTypes = Some parameterTypes
                                                     ReturnType = declaredReturnType }, body ->
                                    if List.length parameters <> List.length parameterTypes then
                                        return!
                                            Result.Error
                                                $"IL backend expected declaration for '{currentModule}.{bindingName}' to declare {List.length parameters} parameter type(s), but found {List.length parameterTypes}."

                                    let parameterNames =
                                        parameters |> List.map (fun parameter -> parameter.Name)

                                    let provisionalReturnType =
                                        declaredReturnType |> Option.defaultValue unitIlType

                                    let allowedTypeParameters =
                                        bindingTypeParameters parameterTypes provisionalReturnType |> Set.ofList

                                    let info =
                                        { Binding = binding
                                          ParameterTypes = List.zip parameterNames parameterTypes
                                          ReturnType = provisionalReturnType
                                          TypeParameters = bindingTypeParameters parameterTypes provisionalReturnType
                                          EmittedMethodName = emittedMethodName bindingName }

                                    cache[cacheKey] <- info

                                    match body with
                                    | None ->
                                        match declaredReturnType with
                                        | Some resolvedReturnType ->
                                            let resolvedInfo =
                                                { info with
                                                    ReturnType = resolvedReturnType
                                                    TypeParameters = bindingTypeParameters parameterTypes resolvedReturnType }

                                            cache[cacheKey] <- resolvedInfo
                                            return resolvedInfo
                                        | None ->
                                            return!
                                                Result.Error
                                                    $"IL backend requires an explicit return type for bodyless binding '{currentModule}.{bindingName}'."
                                    | Some body ->
                                        let localTypes =
                                            List.zip parameterNames parameterTypes |> Map.ofList

                                        let! bodyType =
                                            inferExpressionType
                                                currentModule
                                                localTypes
                                                (Set.add cacheKey active)
                                                allowedTypeParameters
                                                declaredReturnType
                                                body

                                        let resolvedReturnType =
                                            declaredReturnType |> Option.defaultValue bodyType

                                        if bodyType <> resolvedReturnType then
                                            cache.Remove(cacheKey) |> ignore

                                            return!
                                                Result.Error
                                                    $"IL backend expected '{currentModule}.{bindingName}' to return {formatIlType resolvedReturnType}, but the body returns {formatIlType bodyType}."

                                        let resolvedInfo =
                                            { info with
                                                ReturnType = resolvedReturnType
                                                TypeParameters = bindingTypeParameters parameterTypes resolvedReturnType }

                                        cache[cacheKey] <- resolvedInfo

                                        return resolvedInfo
                            }

                and inferPatternBindings currentModule active expectedType pattern =
                    let mergeOrPatternBindings (alternatives: Map<string, IlType> list) =
                        match alternatives with
                        | [] ->
                            Result.Ok Map.empty
                        | firstBindings :: rest ->
                            let firstNames = firstBindings |> Map.keys |> Set.ofSeq

                            rest
                            |> List.fold
                                (fun state candidateBindings ->
                                    state
                                    |> Result.bind (fun agreedBindings ->
                                        let candidateNames = candidateBindings |> Map.keys |> Set.ofSeq

                                        if candidateNames <> firstNames then
                                            Result.Error "IL backend requires each or-pattern alternative to bind the same names."
                                        else
                                            firstBindings
                                            |> Map.fold
                                                (fun comparisonState name expectedBindingType ->
                                                    comparisonState
                                                    |> Result.bind (fun () ->
                                                        let actualBindingType = candidateBindings[name]

                                                        if actualBindingType = expectedBindingType then
                                                            Result.Ok()
                                                        else
                                                            Result.Error
                                                                $"IL backend requires binder '{name}' to have the same type in every or-pattern alternative, but found {formatIlType expectedBindingType} and {formatIlType actualBindingType}."))
                                                (Result.Ok())
                                            |> Result.map (fun () -> agreedBindings))
                                )
                                (Result.Ok firstBindings)

                    match pattern with
                    | KRuntimeWildcardPattern ->
                        Result.Ok(Map.empty<string, IlType>)
                    | KRuntimeNamePattern name ->
                        Result.Ok(Map.ofList [ name, expectedType ])
                    | KRuntimeLiteralPattern literal ->
                        let literalType =
                            match literal with
                            | LiteralValue.Integer _ -> IlPrimitive IlInt64
                            | LiteralValue.Float _ -> IlPrimitive IlFloat64
                            | LiteralValue.String _ -> IlPrimitive IlString
                            | LiteralValue.Character _
                            | LiteralValue.Grapheme _ -> IlPrimitive IlString
                            | LiteralValue.Byte _ -> IlPrimitive IlInt64
                            | LiteralValue.Unit -> IlNamed("std.prelude", "Unit", [])

                        if literalType = expectedType then
                            Result.Ok(Map.empty<string, IlType>)
                        else
                            Result.Error
                                $"IL backend cannot match literal of type {formatIlType literalType} against {formatIlType expectedType}."
                    | KRuntimeOrPattern alternatives ->
                        alternatives
                        |> List.fold
                            (fun state alternative ->
                                state
                                |> Result.bind (fun collected ->
                                    inferPatternBindings currentModule active expectedType alternative
                                    |> Result.map (fun bindings -> bindings :: collected)))
                            (Result.Ok [])
                        |> Result.bind (fun collected -> mergeOrPatternBindings (List.rev collected))
                    | KRuntimeConstructorPattern(nameSegments, argumentPatterns) ->
                        match tryResolveConstructor rawModules currentModule nameSegments with
                        | None ->
                            let patternName = String.concat "." nameSegments
                            Result.Error $"IL backend could not resolve constructor pattern '{patternName}'."
                        | Some(_, constructorInfo) ->
                            let resultTemplate = constructorResultType constructorInfo

                            unifyTypes Map.empty resultTemplate expectedType
                            |> Result.bind (fun substitution ->
                                if List.length argumentPatterns <> List.length constructorInfo.FieldTypes then
                                    Result.Error
                                        $"IL backend expected pattern '{constructorInfo.Name}' to receive {List.length constructorInfo.FieldTypes} argument(s), but received {List.length argumentPatterns}."
                                else
                                    List.zip argumentPatterns constructorInfo.FieldTypes
                                    |> List.fold
                                        (fun stateResult (argumentPattern, fieldTemplate) ->
                                            stateResult
                                            |> Result.bind (fun bindings ->
                                                let fieldType = substituteType substitution fieldTemplate

                                                inferPatternBindings currentModule active fieldType argumentPattern
                                                |> Result.map (fun childBindings ->
                                                    Map.fold (fun acc key value -> acc |> Map.add key value) bindings childBindings)))
                                        (Result.Ok(Map.empty<string, IlType>)))

                and inferExpressionType currentModule localTypes active allowedTypeParameters expectedType expression =
                    let ensureExpected actualType =
                        match expectedType with
                        | Some expected ->
                            match unifyTypes Map.empty expected actualType with
                            | Result.Ok _ ->
                                Result.Ok actualType
                            | Result.Error _ ->
                                Result.Error
                                    $"IL backend expected expression of type {formatIlType expected}, but found {formatIlType actualType}."
                        | _ ->
                            Result.Ok actualType

                    let inferNamedValue segments =
                        let nameText = String.concat "." segments

                        match segments with
                        | [ "True" ]
                        | [ "False" ] ->
                            ensureExpected (IlPrimitive IlBool)
                        | [ name ] when localTypes |> Map.containsKey name ->
                            ensureExpected localTypes[name]
                        | _ ->
                            match tryResolveBinding rawModules currentModule segments with
                            | Some(targetModule, bindingInfo) when List.isEmpty bindingInfo.Parameters ->
                                inferBindingInfo targetModule bindingInfo.Name active
                                |> Result.bind (fun resolvedBinding -> ensureExpected resolvedBinding.ReturnType)
                            | Some(targetModule, bindingInfo) ->
                                Result.Error $"IL backend does not support function-valued name '{targetModule}.{bindingInfo.Name}' yet."
                            | None ->
                                match tryResolveConstructor rawModules currentModule segments with
                                | Some(_, constructorInfo) when List.isEmpty constructorInfo.FieldTypes ->
                                    inferConstructorTypeFromArguments
                                        inferExpressionType
                                        currentModule
                                        localTypes
                                        active
                                        allowedTypeParameters
                                        expectedType
                                        []
                                        constructorInfo
                                | Some(targetModule, constructorInfo) ->
                                    Result.Error
                                        $"IL backend does not support constructor-valued name '{targetModule}.{constructorInfo.Name}' yet."
                                | None ->
                                    let localsText =
                                        localTypes |> Map.toList |> List.map fst |> String.concat ", "

                                    Result.Error
                                        $"IL backend could not resolve name '{nameText}'. Locals in scope: [{localsText}]."

                    let inferIntrinsicCall name arguments =
                        if not (knownIntrinsicNames.Contains name) then
                            Result.Error $"IL backend could not resolve callee '{name}'."
                        else
                            arguments
                            |> List.fold
                                (fun stateResult argumentExpression ->
                                    result {
                                        let! collected = stateResult
                                        let! argumentType =
                                            inferExpressionType currentModule localTypes active allowedTypeParameters None argumentExpression
                                        return argumentType :: collected
                                    })
                                (Result.Ok [])
                            |> Result.bind (fun reversedArgumentTypes ->
                                let argumentTypes = List.rev reversedArgumentTypes

                                match name, argumentTypes, expectedType with
                                | "openFile", [ IlPrimitive IlString ], Some resultType ->
                                    ensureExpected resultType
                                | "openFile", [ IlPrimitive IlString ], None ->
                                    match tryDefaultFileType rawModules currentModule with
                                    | Some fileType -> ensureExpected fileType
                                    | None ->
                                        Result.Error
                                            "IL backend intrinsic 'openFile' requires a File data type in the current module when no expected type is available."
                                | _ ->
                                    match intrinsicParameterTypes name argumentTypes with
                                    | Some(_, resultType) ->
                                        ensureExpected resultType
                                    | None ->
                                        let argumentText =
                                            argumentTypes |> List.map formatIlType |> String.concat ", "

                                        Result.Error
                                            $"IL backend does not support intrinsic '{name}' for argument types [{argumentText}].")

                    let inferTraitCall traitName memberName dictionary arguments =
                        result {
                            let! _ = inferExpressionType currentModule localTypes active allowedTypeParameters None dictionary

                            do!
                                arguments
                                |> List.fold
                                    (fun stateResult argumentExpression ->
                                        stateResult
                                        |> Result.bind (fun () ->
                                            inferExpressionType currentModule localTypes active allowedTypeParameters None argumentExpression
                                            |> Result.map (fun _ -> ())))
                                    (Result.Ok())

                            match expectedType, dictionary with
                            | Some expected, _ ->
                                return expected
                            | None, KRuntimeDictionaryValue(moduleName, _, instanceKey) ->
                                match
                                    traitInstances
                                    |> List.tryFind (fun instanceInfo ->
                                        String.Equals(instanceInfo.ModuleName, moduleName, StringComparison.Ordinal)
                                        && String.Equals(instanceInfo.TraitName, traitName, StringComparison.Ordinal)
                                        && String.Equals(instanceInfo.InstanceKey, instanceKey, StringComparison.Ordinal))
                                with
                                | Some instanceInfo ->
                                    match instanceInfo.MemberBindings |> Map.tryFind memberName with
                                    | Some bindingName ->
                                        let! bindingInfo = inferBindingInfo moduleName bindingName active
                                        return bindingInfo.ReturnType
                                    | None ->
                                        return!
                                            Result.Error
                                                $"IL backend could not resolve trait member '{traitName}.{memberName}' for instance '{moduleName}.{instanceKey}'."
                                | None ->
                                    return!
                                        Result.Error
                                            $"IL backend could not resolve trait instance '{moduleName}.{traitName}.{instanceKey}'."
                            | None, _ ->
                                let routes =
                                    traitInstances
                                    |> List.choose (fun instanceInfo ->
                                        instanceInfo.MemberBindings
                                        |> Map.tryFind memberName
                                        |> Option.bind (fun bindingName ->
                                            if String.Equals(instanceInfo.TraitName, traitName, StringComparison.Ordinal) then
                                                Some(instanceInfo.ModuleName, bindingName)
                                            else
                                                None))

                                match routes with
                                | [] ->
                                    return!
                                        Result.Error
                                            $"IL backend could not find any routes for trait call '{traitName}.{memberName}'."
                                | (firstModuleName, firstBindingName) :: remainingRoutes ->
                                    let! firstBinding = inferBindingInfo firstModuleName firstBindingName active

                                    let! remainingReturnTypes =
                                        remainingRoutes
                                        |> List.fold
                                            (fun stateResult (routeModuleName, routeBindingName) ->
                                                result {
                                                    let! collected = stateResult
                                                    let! routeBinding = inferBindingInfo routeModuleName routeBindingName active
                                                    return routeBinding.ReturnType :: collected
                                                })
                                            (Result.Ok [])

                                    if remainingReturnTypes |> List.exists ((<>) firstBinding.ReturnType) then
                                        return!
                                            Result.Error
                                                $"IL backend could not infer a unique result type for trait call '{traitName}.{memberName}'."

                                    return firstBinding.ReturnType
                        }

                    match expression with
                    | KRuntimeLiteral(LiteralValue.Integer _) ->
                        ensureExpected (IlPrimitive IlInt64)
                    | KRuntimeLiteral(LiteralValue.Float _) ->
                        ensureExpected (IlPrimitive IlFloat64)
                    | KRuntimeLiteral(LiteralValue.String _) ->
                        ensureExpected (IlPrimitive IlString)
                    | KRuntimeLiteral(LiteralValue.Character _)
                    | KRuntimeLiteral(LiteralValue.Grapheme _) ->
                        ensureExpected (IlPrimitive IlString)
                    | KRuntimeLiteral(LiteralValue.Byte _) ->
                        ensureExpected (IlPrimitive IlInt64)
                    | KRuntimeLiteral LiteralValue.Unit ->
                        ensureExpected unitIlType
                    | KRuntimeEffectLabel _
                    | KRuntimeEffectOperation _
                    | KRuntimeHandle _ ->
                        Result.Error "IL backend does not support effect handlers yet."
                    | KRuntimeName segments ->
                        inferNamedValue segments
                    | KRuntimeUnary("-", operand) ->
                        inferExpressionType currentModule localTypes active allowedTypeParameters None operand
                        |> Result.bind (function
                            | IlPrimitive IlInt64 -> ensureExpected (IlPrimitive IlInt64)
                            | IlPrimitive IlFloat64 -> ensureExpected (IlPrimitive IlFloat64)
                            | other -> Result.Error $"Unary '-' is not supported for {formatIlType other}.")
                    | KRuntimeUnary(operatorName, _) ->
                        Result.Error $"IL backend does not support unary operator '{operatorName}' yet."
                    | KRuntimeBinary(left, operatorName, right) ->
                        if IntrinsicCatalog.isBuiltinBinaryOperator operatorName then
                            let builtinResult =
                                result {
                                    let! leftType = inferExpressionType currentModule localTypes active allowedTypeParameters None left
                                    let! rightType = inferExpressionType currentModule localTypes active allowedTypeParameters None right

                                    return!
                                        match operatorName, leftType, rightType with
                                        | ("+" | "-" | "*" | "/"), IlPrimitive IlInt64, IlPrimitive IlInt64 ->
                                            ensureExpected (IlPrimitive IlInt64)
                                        | ("+" | "-" | "*" | "/"), IlPrimitive IlFloat64, IlPrimitive IlFloat64 ->
                                            ensureExpected (IlPrimitive IlFloat64)
                                        | ("==" | "!="), IlPrimitive IlInt64, IlPrimitive IlInt64 ->
                                            ensureExpected (IlPrimitive IlBool)
                                        | ("==" | "!="), IlPrimitive IlFloat64, IlPrimitive IlFloat64 ->
                                            ensureExpected (IlPrimitive IlBool)
                                        | ("==" | "!="), IlPrimitive IlBool, IlPrimitive IlBool ->
                                            ensureExpected (IlPrimitive IlBool)
                                        | ("==" | "!="), IlPrimitive IlString, IlPrimitive IlString ->
                                            ensureExpected (IlPrimitive IlBool)
                                        | ("==" | "!="), IlPrimitive IlChar, IlPrimitive IlChar ->
                                            ensureExpected (IlPrimitive IlBool)
                                        | ("<" | "<=" | ">" | ">="), IlPrimitive IlInt64, IlPrimitive IlInt64 ->
                                            ensureExpected (IlPrimitive IlBool)
                                        | ("<" | "<=" | ">" | ">="), IlPrimitive IlFloat64, IlPrimitive IlFloat64 ->
                                            ensureExpected (IlPrimitive IlBool)
                                        | ("&&" | "||"), IlPrimitive IlBool, IlPrimitive IlBool ->
                                            ensureExpected (IlPrimitive IlBool)
                                        | _ ->
                                            Result.Error ""
                                }

                            builtinResult
                        else
                            inferExpressionType
                                currentModule
                                localTypes
                                active
                                allowedTypeParameters
                                expectedType
                                (KRuntimeApply(KRuntimeName [ operatorName ], [ left; right ]))
                    | KRuntimeIfThenElse(condition, whenTrue, whenFalse) ->
                        result {
                            let! conditionType = inferExpressionType currentModule localTypes active allowedTypeParameters None condition

                            if conditionType <> IlPrimitive IlBool then
                                return! Result.Error "IL backend requires Bool conditions for if expressions."

                            let! trueType = inferExpressionType currentModule localTypes active allowedTypeParameters expectedType whenTrue
                            let! falseType = inferExpressionType currentModule localTypes active allowedTypeParameters (Some trueType) whenFalse

                            if trueType <> falseType then
                                return!
                                    Result.Error
                                        $"IL backend requires both if branches to have the same type, but saw {formatIlType trueType} and {formatIlType falseType}."

                            return trueType
                        }
                    | KRuntimeMatch(scrutinee, cases) ->
                        result {
                            let! scrutineeType = inferExpressionType currentModule localTypes active allowedTypeParameters None scrutinee

                            let! caseTypes =
                                cases
                                |> List.fold
                                    (fun stateResult caseClause ->
                                        result {
                                            let! collected = stateResult
                                            let! patternBindings = inferPatternBindings currentModule active scrutineeType caseClause.Pattern
                                            let extendedLocals =
                                                patternBindings
                                                |> Map.fold (fun locals name value -> locals |> Map.add name value) localTypes

                                            let expectedCaseType =
                                                match collected with
                                                | head :: _ -> Some head
                                                | [] -> expectedType

                                            match caseClause.Guard with
                                            | Some guard ->
                                                let! guardType =
                                                    inferExpressionType currentModule extendedLocals active allowedTypeParameters None guard

                                                if guardType <> IlPrimitive IlBool then
                                                    return! Result.Error "IL backend requires Bool guards for match cases."
                                            | None ->
                                                ()

                                            let! caseType =
                                                inferExpressionType
                                                    currentModule
                                                    extendedLocals
                                                    active
                                                    allowedTypeParameters
                                                    expectedCaseType
                                                    caseClause.Body

                                            return caseType :: collected
                                        })
                                    (Result.Ok [])

                            match caseTypes with
                            | [] ->
                                return! Result.Error "IL backend requires at least one match case."
                            | head :: tail ->
                                if tail |> List.exists ((<>) head) then
                                    return! Result.Error "IL backend requires all match cases to return the same type."

                                return head
                        }
                    | KRuntimeApply(KRuntimeName segments, arguments) ->
                        let nameText = String.concat "." segments
                        match segments, arguments with
                        | [ operatorName ], [ left; right ] when IntrinsicCatalog.isBuiltinBinaryOperator operatorName ->
                            inferExpressionType
                                currentModule
                                localTypes
                                active
                                allowedTypeParameters
                                expectedType
                                (KRuntimeBinary(left, operatorName, right))
                        | _ ->
                            match tryResolveBinding rawModules currentModule segments with
                            | Some(targetModule, bindingInfo) ->
                                inferBindingInfo targetModule bindingInfo.Name active
                                |> Result.bind (fun resolvedBinding ->
                                    inferBindingTypeFromArguments
                                        inferExpressionType
                                        currentModule
                                        localTypes
                                        active
                                        allowedTypeParameters
                                        expectedType
                                        arguments
                                        resolvedBinding
                                    |> Result.map snd)
                            | None ->
                                match tryResolveConstructor rawModules currentModule segments with
                                | Some(_, constructorInfo) ->
                                    inferConstructorTypeFromArguments
                                        inferExpressionType
                                        currentModule
                                        localTypes
                                        active
                                        allowedTypeParameters
                                        expectedType
                                        arguments
                                        constructorInfo
                                | None ->
                                    inferIntrinsicCall nameText arguments
                    | KRuntimeExecute inner ->
                        inferExpressionType currentModule localTypes active allowedTypeParameters expectedType inner
                    | KRuntimeLet(bindingName, value, body) ->
                        result {
                            let! valueType = inferExpressionType currentModule localTypes active allowedTypeParameters None value
                            let extendedLocals = localTypes |> Map.add bindingName valueType
                            return! inferExpressionType currentModule extendedLocals active allowedTypeParameters expectedType body
                        }
                    | KRuntimeDoScope(_, body) ->
                        inferExpressionType currentModule localTypes active allowedTypeParameters expectedType body
                    | KRuntimeScheduleExit(_, _, body) ->
                        inferExpressionType currentModule localTypes active allowedTypeParameters expectedType body
                    | KRuntimeSequence(first, second) ->
                        result {
                            do! inferExpressionType currentModule localTypes active allowedTypeParameters None first |> Result.map (fun _ -> ())
                            return! inferExpressionType currentModule localTypes active allowedTypeParameters expectedType second
                        }
                    | KRuntimeWhile(condition, body) ->
                        result {
                            let! conditionType = inferExpressionType currentModule localTypes active allowedTypeParameters None condition

                            if conditionType <> IlPrimitive IlBool then
                                return! Result.Error "IL backend requires Bool conditions for while expressions."

                            do! inferExpressionType currentModule localTypes active allowedTypeParameters None body |> Result.map (fun _ -> ())
                            return! ensureExpected unitIlType
                        }
                    | KRuntimeDictionaryValue(moduleName, traitName, instanceKey) ->
                        match
                            traitInstances
                            |> List.tryFind (fun instanceInfo ->
                                String.Equals(instanceInfo.ModuleName, moduleName, StringComparison.Ordinal)
                                && String.Equals(instanceInfo.TraitName, traitName, StringComparison.Ordinal)
                                && String.Equals(instanceInfo.InstanceKey, instanceKey, StringComparison.Ordinal))
                        with
                        | Some instanceInfo ->
                            ensureExpected (dictionaryIlType traitName instanceInfo.HeadTypes)
                        | None ->
                            Result.Error $"IL backend could not resolve trait instance '{moduleName}.{traitName}.{instanceKey}'."
                    | KRuntimeTraitCall(traitName, memberName, dictionary, arguments) ->
                        inferTraitCall traitName memberName dictionary arguments
                    | KRuntimeApply _ ->
                        Result.Error "IL backend currently supports application only when the callee is a named binding."
                    | KRuntimeClosure _ ->
                        Result.Error "IL backend does not support closures yet."
                    | KRuntimePrefixedString _ ->
                        Result.Error "IL backend does not support prefixed strings yet."

                rawModules
                |> Map.toList
                |> List.fold
                    (fun stateResult (moduleName, rawModule) ->
                        result {
                            let! state = stateResult

                            let! bindingEntries =
                                rawModule.Bindings
                                |> Map.toList
                                |> List.fold
                                    (fun bindingResult (bindingName, _) ->
                                        result {
                                            let! entriesSoFar = bindingResult
                                            let! bindingInfo = inferBindingInfo moduleName bindingName Set.empty
                                            return (bindingName, bindingInfo) :: entriesSoFar
                                        })
                                    (Result.Ok [])

                            let moduleInfo =
                                { Name = rawModule.Name
                                  Imports = rawModule.Imports
                                  Exports = rawModule.Exports
                                  TypeExports = rawModule.TypeExports
                                  DataTypes = rawModule.DataTypes
                                  Constructors = rawModule.Constructors
                                  Bindings = bindingEntries |> Map.ofList
                                  EmittedTypeName = rawModule.EmittedTypeName }

                            return state |> Map.add moduleName moduleInfo
                        })
                    (Result.Ok Map.empty)
                    |> Result.map (fun modules ->
                        let allDataTypes =
                            modules
                            |> Map.toList
                            |> List.collect (fun (moduleName, moduleInfo) ->
                                moduleInfo.DataTypes
                                |> Map.toList
                                |> List.map (fun (typeName, dataType) -> (moduleName, typeName), dataType))
                            |> Map.ofList

                        { Modules = modules
                          DataTypes = allDataTypes
                          TraitInstances = traitInstances })))
