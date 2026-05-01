namespace Kappa.Compiler

open System
open System.Collections.Generic
open System.IO
open System.Reflection
open System.Reflection.Emit
open System.Runtime.CompilerServices

// Generates Reflection.Emit assemblies from the CLR assembly input model.
module internal IlDotNetBackendEmit =
    open IlDotNetBackendModel
    open IlDotNetBackendInput
    open IlDotNetBackendTyping

    let private compareBuiltinSignMethod =
        typeof<Kappa.Runtime.KappaRuntime>.GetMethod("CompareBuiltinSign", [| typeof<obj>; typeof<obj> |])

    let private showBuiltinMethod =
        typeof<Kappa.Runtime.KappaRuntime>.GetMethod("ShowBuiltin", [| typeof<obj> |])

    let internal emitLiteral (il: ILGenerator) literal =
        match literal with
        | LiteralValue.Integer value ->
            il.Emit(OpCodes.Ldc_I8, value)
            Result.Ok()
        | LiteralValue.Float value ->
            il.Emit(OpCodes.Ldc_R8, value)
            Result.Ok()
        | LiteralValue.String value ->
            il.Emit(OpCodes.Ldstr, value)
            Result.Ok()
        | LiteralValue.Character value ->
            il.Emit(OpCodes.Ldstr, value)
            Result.Ok()
        | LiteralValue.Grapheme value ->
            il.Emit(OpCodes.Ldstr, value)
            Result.Ok()
        | LiteralValue.Byte value ->
            il.Emit(OpCodes.Ldc_I8, int64 value)
            Result.Ok()
        | LiteralValue.Unit ->
            let unitLocal = il.DeclareLocal(typeof<ValueTuple>)
            il.Emit(OpCodes.Ldloca, unitLocal)
            il.Emit(OpCodes.Initobj, typeof<ValueTuple>)
            il.Emit(OpCodes.Ldloc, unitLocal)
            Result.Ok()

    let internal emitComparisonFromCeq (il: ILGenerator) negate =
        il.Emit(OpCodes.Ceq)

        if negate then
            il.Emit(OpCodes.Ldc_I4_0)
            il.Emit(OpCodes.Ceq)

    let internal emitUnitValue (il: ILGenerator) =
        emitLiteral il LiteralValue.Unit

    let private lookupDataTypeEmission (state: EmissionState) moduleName typeName =
        match state.DataTypeBuilders |> Map.tryFind (moduleName, typeName) with
        | Some emission ->
            emission
        | None ->
            invalidOp $"IL backend is missing emitted type builder metadata for '{moduleName}.{typeName}'."

    let private lookupClassHierarchyEmission (state: EmissionState) moduleName typeName =
        match lookupDataTypeEmission state moduleName typeName with
        | ClassHierarchyEmission emission ->
            emission
        | EnumEmission _ ->
            invalidOp $"IL backend expected '{moduleName}.{typeName}' to use class-hierarchy lowering."

    let private lookupEnumCaseEmission (state: EmissionState) moduleName typeName constructorName =
        match lookupDataTypeEmission state moduleName typeName with
        | EnumEmission emission ->
            match emission.Cases |> Map.tryFind constructorName with
            | Some caseEmission ->
                caseEmission
            | None ->
                invalidOp
                    $"IL backend is missing enum case metadata for constructor '{moduleName}.{constructorName}'."
        | ClassHierarchyEmission _ ->
            invalidOp $"IL backend expected '{moduleName}.{typeName}' to use enum lowering."

    let private tryFindDataTypeInfo (state: EmissionState) (typeIdentity: TypeIdentity) =
        let moduleName = TypeIdentity.moduleIdentity typeIdentity |> ModuleIdentity.text
        let typeName = TypeIdentity.name typeIdentity
        state.Environment.DataTypes |> Map.tryFind (moduleName, typeName)

    let private isClrEnumIlType (state: EmissionState) ilType =
        match ilType with
        | IlNamed(typeIdentity, []) ->
            tryFindDataTypeInfo state typeIdentity
            |> Option.exists (fun dataTypeInfo ->
                dataTypeInfo.Representation = IlClrEnum && dataTypeInfo.ExternalRuntimeTypeName.IsNone)
        | _ ->
            false

    let private resolveExternalRuntimeType (typeName: string) =
        Type.GetType(typeName, throwOnError = false, ignoreCase = false)
        |> Option.ofObj
        |> Option.orElseWith (fun () ->
            AppDomain.CurrentDomain.GetAssemblies()
            |> Seq.tryPick (fun assembly ->
                try
                    assembly.GetType(typeName, throwOnError = false, ignoreCase = false) |> Option.ofObj
                with _ ->
                    None))
        |> Option.defaultWith (fun () -> invalidOp $"IL backend could not resolve external runtime type '{typeName}'.")

    let rec internal resolveClrType (state: EmissionState) (typeParameters: Map<string, Type>) ilType =
        match ilType with
        | IlPrimitive primitiveType ->
            primitiveRuntimeType primitiveType
        | IlTypeParameter name ->
            match typeParameters |> Map.tryFind name with
            | Some resolved ->
                resolved
            | None ->
                let available =
                    typeParameters
                    |> Map.keys
                    |> String.concat ", "

                invalidOp
                    $"IL backend could not resolve type parameter '{name}'. Available type parameters: [{available}]."
        | IlNamed(typeIdentity, []) when TypeIdentity.hasTopLevelName preludeModuleIdentity Stdlib.KnownTypeNames.Unit typeIdentity ->
            typeof<ValueTuple>
        | IlNamed(typeIdentity, [ elementType ]) when TypeIdentity.hasTopLevelName preludeModuleIdentity Stdlib.KnownTypeNames.Ref typeIdentity ->
            typedefof<StrongBox<_>>.MakeGenericType([| resolveClrType state typeParameters elementType |])
        | IlNamed(typeIdentity, [ elementType ]) when TypeIdentity.hasTopLevelName preludeModuleIdentity Stdlib.KnownTypeNames.IO typeIdentity ->
            resolveClrType state typeParameters elementType
        | IlNamed(typeIdentity, _) when TypeIdentity.moduleIdentity typeIdentity = preludeModuleIdentity && isDictionaryTypeName (TypeIdentity.name typeIdentity) ->
            typeof<Tuple<string, string, string>>
        | IlNamed(typeIdentity, arguments) ->
            let moduleName = TypeIdentity.moduleIdentity typeIdentity |> ModuleIdentity.text
            let typeName = TypeIdentity.name typeIdentity
            match state.Environment.DataTypes |> Map.tryFind (moduleName, typeName) with
            | Some dataTypeInfo when dataTypeInfo.ExternalRuntimeTypeName.IsSome ->
                let baseType = resolveExternalRuntimeType dataTypeInfo.ExternalRuntimeTypeName.Value

                if List.isEmpty arguments then
                    baseType
                else
                    let genericArguments =
                        arguments |> List.map (resolveClrType state typeParameters) |> List.toArray

                    baseType.MakeGenericType(genericArguments)
            | _ ->
                let emission = lookupDataTypeEmission state moduleName typeName
                match emission with
                | ClassHierarchyEmission classEmission ->
                    let baseType =
                        classEmission.BaseTypeBuilder :> Type

                    if List.isEmpty arguments then
                        baseType
                    else
                        let genericArguments =
                            arguments |> List.map (resolveClrType state typeParameters) |> List.toArray

                        baseType.MakeGenericType(genericArguments)
                | EnumEmission enumEmission ->
                    if not (List.isEmpty arguments) then
                        invalidOp $"IL backend enum '{moduleName}.{typeName}' cannot take CLR generic arguments."

                    enumEmission.EnumBuilder :> Type

    let internal resolveConstructorTypeAndMembers
        (state: EmissionState)
        (typeParameters: Map<string, Type>)
        (substitution: Map<string, IlType>)
        (constructorInfo: ConstructorInfo)
        =
        let dataTypeEmission = lookupClassHierarchyEmission state constructorInfo.ModuleName constructorInfo.TypeName
        let emission = dataTypeEmission.Constructors[constructorInfo.Name]

        if Array.isEmpty emission.GenericParameters then
            let fields = emission.FieldBuilders |> Array.map (fun fieldBuilder -> fieldBuilder :> FieldInfo)
            emission.TypeBuilder :> Type, emission.ConstructorBuilder :> System.Reflection.ConstructorInfo, fields
        else
            let genericArguments =
                constructorInfo.TypeParameters
                |> List.map (fun name -> resolveClrType state typeParameters substitution[name])
                |> List.toArray

            let constructedType = emission.TypeBuilder.MakeGenericType(genericArguments)
            let constructor = TypeBuilder.GetConstructor(constructedType, emission.ConstructorBuilder)
            let fields = emission.FieldBuilders |> Array.map (fun fieldBuilder -> TypeBuilder.GetField(constructedType, fieldBuilder))
            constructedType, constructor, fields

    let internal tryResolveEnumConstructorOrdinal (state: EmissionState) (constructorInfo: ConstructorInfo) =
        match tryFindDataTypeInfo state constructorInfo.ResultType with
        | Some dataTypeInfo when dataTypeInfo.Representation = IlClrEnum ->
            let caseEmission =
                lookupEnumCaseEmission state constructorInfo.ModuleName constructorInfo.TypeName constructorInfo.Name

            Some caseEmission.CaseOrdinal
        | _ ->
            None

    let internal loadValue (il: ILGenerator) localValue =
        match localValue.Location with
        | Argument argumentIndex ->
            il.Emit(OpCodes.Ldarg, int16 argumentIndex)
        | Local localBuilder ->
            il.Emit(OpCodes.Ldloc, localBuilder)

    let internal emitLoadLocal (il: ILGenerator) (localBuilder: LocalBuilder) =
        il.Emit(OpCodes.Ldloc, localBuilder)

    let internal emitCoerceStackValue (state: EmissionState) (typeParameters: Map<string, Type>) sourceType targetType (il: ILGenerator) =
        if sourceType <> targetType then
            let sourceClrType = resolveClrType state typeParameters sourceType
            let targetClrType = resolveClrType state typeParameters targetType

            if sourceClrType <> targetClrType then
                let sourceIsGeneric =
                    match sourceType with
                    | IlTypeParameter _ -> true
                    | _ -> false

                if sourceIsGeneric || sourceClrType.IsValueType then
                    il.Emit(OpCodes.Box, sourceClrType)

                if targetClrType.IsValueType then
                    il.Emit(OpCodes.Unbox_Any, targetClrType)
                else
                    il.Emit(OpCodes.Castclass, targetClrType)

    let internal resolveMethodInfoForCall
        (state: EmissionState)
        (typeParameters: Map<string, Type>)
        targetModule
        (bindingInfo: BindingInfo)
        (substitution: Map<string, IlType>)
        =
        result {
            let methodEmission = state.MethodBuilders[targetModule][bindingInfo.Binding.Name]

            match bindingInfo.TypeParameters with
            | [] ->
                return methodEmission.Builder :> MethodInfo
            | typeParameterNames ->
                let! genericArguments =
                    typeParameterNames
                    |> List.fold
                        (fun stateResult typeParameterName ->
                            result {
                                let! collected = stateResult
                                let specializedType = substituteType substitution (IlTypeParameter typeParameterName)

                                match specializedType with
                                | IlTypeParameter name when typeParameters.ContainsKey name ->
                                    return typeParameters[name] :: collected
                                | unresolved when containsTypeParameters unresolved ->
                                    return!
                                        Result.Error
                                            $"IL backend could not infer generic argument '{typeParameterName}' for '{targetModule}.{bindingInfo.Binding.Name}'."
                                | resolved ->
                                    return resolveClrType state typeParameters resolved :: collected
                            })
                        (Result.Ok [])
                    |> Result.map (List.rev >> List.toArray)

                return methodEmission.Builder.MakeGenericMethod(genericArguments)
        }

    let internal dictionaryTupleConstructor =
        typeof<Tuple<string, string, string>>.GetConstructor([| typeof<string>; typeof<string>; typeof<string> |])

    let internal dictionaryModuleGetter =
        typeof<Tuple<string, string, string>>.GetProperty("Item1").GetGetMethod()

    let internal dictionaryTraitGetter =
        typeof<Tuple<string, string, string>>.GetProperty("Item2").GetGetMethod()

    let internal dictionaryInstanceGetter =
        typeof<Tuple<string, string, string>>.GetProperty("Item3").GetGetMethod()

    let internal stringEqualityMethod =
        typeof<string>.GetMethod("op_Equality", BindingFlags.Public ||| BindingFlags.Static, null, [| typeof<string>; typeof<string> |], null)

    let internal emitStringEquality (il: ILGenerator) negate =
        il.Emit(OpCodes.Call, stringEqualityMethod)

        if negate then
            il.Emit(OpCodes.Ldc_I4_0)
            il.Emit(OpCodes.Ceq)

    let rec internal emitExpression
        (state: EmissionState)
        currentModule
        (typeParameters: Map<string, Type>)
        localValues
        expectedType
        (il: ILGenerator)
        expression
        =
        let localTypes =
            localValues |> Map.map (fun _ value -> value.Type)

        let activeTypeParameters =
            typeParameters |> Map.keys |> Set.ofSeq

        let rec inferExpressionType currentModule localTypes expectedType expression =
            let modules = state.Environment.Modules

            let rec inferPatternBindings expectedType pattern =
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
                        | LiteralValue.Unit -> unitIlType

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
                                inferPatternBindings expectedType alternative
                                |> Result.map (fun bindings -> bindings :: collected)))
                        (Result.Ok [])
                    |> Result.bind (fun collected -> mergeOrPatternBindings (List.rev collected))
                | KRuntimeConstructorPattern(nameSegments, argumentPatterns) ->
                    match tryResolveConstructor modules currentModule nameSegments with
                    | None ->
                        let patternName = String.concat "." nameSegments
                        Result.Error $"IL backend could not resolve constructor pattern '{patternName}'."
                    | Some(_, constructorInfo) ->
                        unifyTypes Map.empty (constructorResultType constructorInfo) expectedType
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

                                            inferPatternBindings fieldType argumentPattern
                                            |> Result.map (fun childBindings ->
                                                Map.fold (fun acc key value -> acc |> Map.add key value) bindings childBindings)))
                                    (Result.Ok(Map.empty<string, IlType>)))

            let rec infer expression expectedType =
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

                let inferIntrinsicCall name arguments =
                    if not (knownIntrinsicNames.Contains name) then
                        Result.Error $"IL backend could not resolve callee '{name}'."
                    else
                        arguments
                        |> List.fold
                            (fun stateResult argumentExpression ->
                                result {
                                    let! collected = stateResult
                                    let! argumentType = infer argumentExpression None
                                    return argumentType :: collected
                                })
                            (Result.Ok [])
                        |> Result.bind (fun reversedArgumentTypes ->
                            let argumentTypes = List.rev reversedArgumentTypes

                            match name, argumentTypes, expectedType with
                            | "openFile", [ IlPrimitive IlString ], Some resultType ->
                                ensureExpected resultType
                            | "openFile", [ IlPrimitive IlString ], None ->
                                match tryDefaultFileType state.Environment.Modules currentModule with
                                | Some fileType -> ensureExpected fileType
                                | None ->
                                    Result.Error
                                        "IL backend intrinsic 'openFile' requires a File data type in the current module when no expected type is available."
                            | _ ->
                                match intrinsicParameterTypes name argumentTypes with
                                    | Some(_, resultType) ->
                                        ensureExpected resultType
                                    | None ->
                                        Result.Error(
                                            DiagnosticFact.ClrBackendEmitterError.message
                                                (IntrinsicArgumentTypesUnsupported(
                                                    name,
                                                    argumentTypes |> List.map formatIlType
                                                ))
                                        ))

                let inferTraitCall traitName memberName dictionary arguments =
                    result {
                        let! _ = infer dictionary None

                        do!
                            arguments
                            |> List.fold
                                (fun stateResult argumentExpression ->
                                    stateResult
                                    |> Result.bind (fun () -> infer argumentExpression None |> Result.map (fun _ -> ())))
                                (Result.Ok())

                        match expectedType, dictionary with
                        | Some expected, _ ->
                            return expected
                        | None, KRuntimeDictionaryValue(moduleName, _, instanceKey, _) ->
                            match tryFindTraitInstance state.Environment moduleName traitName instanceKey with
                            | Some instanceInfo ->
                                match instanceInfo.MemberBindings |> Map.tryFind memberName with
                                | Some bindingName ->
                                    let routeModule = state.Environment.Modules[moduleName]
                                    let bindingInfo = routeModule.Bindings[bindingName]
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
                            let routes = traitMemberRoutes state.Environment traitName memberName

                            match routes with
                            | [] ->
                                return!
                                    Result.Error
                                        $"IL backend could not find any routes for trait call '{traitName}.{memberName}'."
                            | (firstInstanceInfo, firstBindingName) :: remainingRoutes ->
                                match state.Environment.Modules[firstInstanceInfo.ModuleName].Bindings |> Map.tryFind firstBindingName with
                                | None ->
                                    return!
                                        Result.Error
                                            $"IL backend could not resolve any binding info for trait call '{traitName}.{memberName}'."
                                | Some bindingInfo ->
                                    let remainingReturnTypes =
                                        remainingRoutes
                                        |> List.choose (fun (instanceInfo, bindingName) ->
                                            state.Environment.Modules[instanceInfo.ModuleName].Bindings
                                            |> Map.tryFind bindingName
                                            |> Option.map (fun routeBinding -> routeBinding.ReturnType))

                                    if remainingReturnTypes |> List.exists ((<>) bindingInfo.ReturnType) then
                                        return!
                                            Result.Error
                                                $"IL backend could not infer a unique result type for trait call '{traitName}.{memberName}'."

                                    return bindingInfo.ReturnType
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
                    Result.Error(DiagnosticFact.ClrBackendEmitterError.message EffectHandlersUnsupported)
                | KRuntimeName [ "True" ]
                | KRuntimeName [ "False" ] ->
                    ensureExpected (IlPrimitive IlBool)
                | KRuntimeName [ name ] when localTypes |> Map.containsKey name ->
                    ensureExpected localTypes[name]
                | KRuntimeName segments ->
                    let nameText = String.concat "." segments

                    match tryResolveBinding modules currentModule segments with
                    | Some(_, bindingInfo) when List.isEmpty bindingInfo.ParameterTypes ->
                        ensureExpected bindingInfo.ReturnType
                    | Some(targetModule, bindingInfo) ->
                        Result.Error(
                            DiagnosticFact.ClrBackendEmitterError.message
                                (FunctionValuedNameUnsupported(targetModule, bindingInfo.Binding.Name))
                        )
                    | None ->
                        match tryResolveConstructor modules currentModule segments with
                        | Some(_, constructorInfo) when List.isEmpty constructorInfo.FieldTypes ->
                            inferConstructorTypeFromArguments
                                (fun _ locals _ _ expected innerExpression ->
                                    inferBody locals expected innerExpression)
                                currentModule
                                localTypes
                                activeTypeParameters
                                activeTypeParameters
                                expectedType
                                []
                                constructorInfo
                        | Some(targetModule, constructorInfo) ->
                            Result.Error(
                                DiagnosticFact.ClrBackendEmitterError.message
                                    (ConstructorValuedNameUnsupported(targetModule, constructorInfo.Name))
                            )
                        | None ->
                            let localsText =
                                localTypes |> Map.toList |> List.map fst |> String.concat ", "

                            Result.Error
                                $"IL backend could not resolve name '{nameText}'. Locals in scope: [{localsText}]."
                | KRuntimeUnary("-", operand) ->
                    infer operand None
                    |> Result.bind (function
                        | IlPrimitive IlInt64 -> ensureExpected (IlPrimitive IlInt64)
                        | IlPrimitive IlFloat64 -> ensureExpected (IlPrimitive IlFloat64)
                        | other ->
                            Result.Error(
                                DiagnosticFact.ClrBackendEmitterError.message
                                    (UnaryOperatorOperandTypeUnsupported("-", formatIlType other))
                            ))
                | KRuntimeUnary(operatorName, _) ->
                    Result.Error(
                        DiagnosticFact.ClrBackendEmitterError.message (UnaryOperatorUnsupported operatorName)
                    )
                | KRuntimeBinary(left, operatorName, right) ->
                    if KnownPreludeSemantics.isBuiltinBinaryOperator operatorName then
                        let builtinResult =
                            result {
                                match operatorName, right with
                                | "is", KRuntimeName constructorName ->
                                    let! leftType = infer left None

                                    return!
                                        match tryResolveConstructor modules currentModule constructorName with
                                        | None ->
                                            let constructorText = String.concat "." constructorName
                                            Result.Error
                                                $"IL backend could not resolve constructor '{constructorText}' for operator 'is'."
                                        | Some(_, constructorInfo) ->
                                            unifyTypes Map.empty (constructorResultType constructorInfo) leftType
                                            |> Result.map (fun _ -> IlPrimitive IlBool)
                                            |> Result.bind ensureExpected
                                | _ ->
                                    let! leftType = infer left None
                                    let! rightType = infer right None

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
                                        | ("==" | "!="), IlNamed(leftIdentity, []), IlNamed(rightIdentity, [])
                                            when TypeIdentity.hasTopLevelName preludeModuleIdentity Stdlib.KnownTypeNames.Bool leftIdentity
                                                 && TypeIdentity.hasTopLevelName preludeModuleIdentity Stdlib.KnownTypeNames.Bool rightIdentity ->
                                            ensureExpected (IlPrimitive IlBool)
                                        | ("==" | "!="), leftEnumType, rightEnumType
                                            when leftEnumType = rightEnumType && isClrEnumIlType state leftEnumType ->
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
                                        | ("&&" | "||"), IlNamed(leftIdentity, []), IlNamed(rightIdentity, [])
                                            when TypeIdentity.hasTopLevelName preludeModuleIdentity Stdlib.KnownTypeNames.Bool leftIdentity
                                                 && TypeIdentity.hasTopLevelName preludeModuleIdentity Stdlib.KnownTypeNames.Bool rightIdentity ->
                                            ensureExpected (IlPrimitive IlBool)
                                        | _ ->
                                            Result.Error(
                                                DiagnosticFact.ClrBackendEmitterError.message
                                                    (BinaryOperatorUnsupported(
                                                        operatorName,
                                                        formatIlType leftType,
                                                        formatIlType rightType
                                                    ))
                                            )
                            }

                        builtinResult
                    else
                        infer (KRuntimeApply(KRuntimeName [ operatorName ], [ left; right ])) expectedType
                | KRuntimeIfThenElse(condition, whenTrue, whenFalse) ->
                    result {
                        let! conditionType = infer condition None

                        if conditionType <> IlPrimitive IlBool then
                            return! Result.Error "IL backend requires Bool conditions for if expressions."

                        let! trueType = infer whenTrue expectedType
                        let! falseType = infer whenFalse (Some trueType)

                        if trueType <> falseType then
                            return!
                                Result.Error
                                    $"IL backend requires both if branches to have the same type, but saw {formatIlType trueType} and {formatIlType falseType}."

                        return trueType
                    }
                | KRuntimeMatch(scrutinee, cases) ->
                    result {
                        let! scrutineeType = infer scrutinee None

                        let! caseTypes =
                            cases
                            |> List.fold
                                (fun stateResult caseClause ->
                                    result {
                                        let! collected = stateResult
                                        let! patternBindings = inferPatternBindings scrutineeType caseClause.Pattern

                                        let extendedLocals =
                                            patternBindings
                                            |> Map.fold (fun locals name value -> locals |> Map.add name value) localTypes

                                        let expectedCaseType =
                                            match collected with
                                            | head :: _ -> Some head
                                            | [] -> expectedType

                                        let! caseType = inferBody extendedLocals expectedCaseType caseClause.Body
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
                    | [ operatorName ], [ left; right ] when KnownPreludeSemantics.isBuiltinBinaryOperator operatorName ->
                        infer (KRuntimeBinary(left, operatorName, right)) expectedType
                    | _ ->
                        match tryResolveBinding modules currentModule segments with
                        | Some(_, bindingInfo) ->
                            inferBindingTypeFromArguments
                                (fun currentModule localTypes _ _ expected innerExpression ->
                                    inferExpressionType currentModule localTypes expected innerExpression)
                                currentModule
                                localTypes
                                activeTypeParameters
                                activeTypeParameters
                                expectedType
                                arguments
                                bindingInfo
                            |> Result.map snd
                        | None ->
                            match tryResolveConstructor modules currentModule segments with
                            | Some(_, constructorInfo) ->
                                inferConstructorTypeFromArguments
                                    (fun _ locals _ _ expected innerExpression ->
                                        inferBody locals expected innerExpression)
                                    currentModule
                                    localTypes
                                    activeTypeParameters
                                    activeTypeParameters
                                    expectedType
                                    arguments
                                    constructorInfo
                            | None ->
                                inferIntrinsicCall nameText arguments
                | KRuntimeExecute inner ->
                    infer inner expectedType
                | KRuntimeLet(bindingName, value, body) ->
                    result {
                        let! valueType = infer value None
                        let extendedLocals = localTypes |> Map.add bindingName valueType
                        return! inferBody extendedLocals expectedType body
                    }
                | KRuntimeDoScope(_, body) ->
                    infer body expectedType
                | KRuntimeScheduleExit(_, _, body) ->
                    infer body expectedType
                | KRuntimeSequence(first, second) ->
                    result {
                        do! infer first None |> Result.map (fun _ -> ())
                        return! infer second expectedType
                    }
                | KRuntimeWhile(condition, body) ->
                    result {
                        let! conditionType = infer condition None

                        if conditionType <> IlPrimitive IlBool then
                            return! Result.Error "IL backend requires Bool conditions for while expressions."

                        do! infer body None |> Result.map (fun _ -> ())
                        return! ensureExpected unitIlType
                    }
                | KRuntimeDictionaryValue(moduleName, traitName, instanceKey, _) ->
                    match tryFindTraitInstance state.Environment moduleName traitName instanceKey with
                    | Some instanceInfo ->
                        ensureExpected (dictionaryIlType traitName instanceInfo.HeadTypes)
                    | None ->
                        Result.Error $"IL backend could not resolve trait instance '{moduleName}.{traitName}.{instanceKey}'."
                | KRuntimeTraitCall(traitName, memberName, dictionary, arguments) ->
                    inferTraitCall traitName memberName dictionary arguments
                | KRuntimeApply _ ->
                    Result.Error "IL backend currently supports application only when the callee is a named binding."
                | KRuntimeClosure _ ->
                    Result.Error(DiagnosticFact.ClrBackendEmitterError.message RuntimeClosuresUnsupported)
                | KRuntimePrefixedString _ ->
                    Result.Error(DiagnosticFact.ClrBackendEmitterError.message PrefixedStringsUnsupported)

            and inferBody bodyLocals expectedType bodyExpression =
                inferExpressionType currentModule bodyLocals expectedType bodyExpression

            infer expression expectedType

        let resultType =
            inferExpressionType currentModule localTypes expectedType expression

        let rec emitConstructorApplication constructorInfo arguments expectedType =
            result {
                let initialSubstitutionResult =
                    match expectedType with
                    | Some expected ->
                        unifyTypes Map.empty (constructorResultType constructorInfo) expected
                    | None ->
                        Result.Ok Map.empty

                let! substitution, argumentTypes =
                    if List.length arguments <> List.length constructorInfo.FieldTypes then
                        Result.Error
                            $"IL backend expected constructor '{constructorInfo.Name}' to receive {List.length constructorInfo.FieldTypes} argument(s), but received {List.length arguments}."
                    else
                        List.zip arguments constructorInfo.FieldTypes
                        |> List.fold
                            (fun stateResult (argumentExpression, argumentTemplate) ->
                                result {
                                    let! substitution, collectedArgumentTypes = stateResult

                                    let expectedArgumentType =
                                        let specialized = substituteType substitution argumentTemplate

                                        if containsTypeParametersOutside activeTypeParameters specialized then
                                            None
                                        else
                                            Some specialized

                                    let! argumentType =
                                        inferExpressionType currentModule localTypes expectedArgumentType argumentExpression

                                    let! nextSubstitution =
                                        unifyTypes substitution argumentTemplate argumentType

                                    return nextSubstitution, argumentType :: collectedArgumentTypes
                                })
                            (initialSubstitutionResult |> Result.map (fun substitution -> substitution, []))
                        |> Result.map (fun (substitution, reversedArgumentTypes) -> substitution, List.rev reversedArgumentTypes)

                let resultType = substituteType substitution (constructorResultType constructorInfo)

                if containsTypeParametersOutside activeTypeParameters resultType then
                    return!
                        Result.Error
                            $"IL backend could not infer concrete type arguments for constructor '{constructorInfo.Name}'."

                match tryResolveEnumConstructorOrdinal state constructorInfo with
                | Some caseOrdinal ->
                    if not (List.isEmpty arguments) then
                        return!
                            Result.Error
                                $"IL backend enum constructor '{constructorInfo.Name}' cannot receive arguments."

                    il.Emit(OpCodes.Ldc_I4, caseOrdinal)
                | None ->
                    let _, constructor, _ = resolveConstructorTypeAndMembers state typeParameters substitution constructorInfo

                    do!
                        List.zip arguments argumentTypes
                        |> List.fold
                            (fun stateResult (argumentExpression, argumentType) ->
                                stateResult
                                |> Result.bind (fun () ->
                                    emitExpression state currentModule typeParameters localValues (Some argumentType) il argumentExpression))
                            (Result.Ok())

                    il.Emit(OpCodes.Newobj, constructor)
            }

        let emitBuiltinBinary operatorName left right =
            result {
                match operatorName, right with
                | "is", KRuntimeName constructorName ->
                    let! leftType = inferExpressionType currentModule localTypes None left
                    do! emitExpression state currentModule typeParameters localValues (Some leftType) il left

                    return!
                        match tryResolveConstructor state.Environment.Modules currentModule constructorName with
                                        | None ->
                                            let constructorText = String.concat "." constructorName
                                            Result.Error
                                                $"IL backend could not resolve constructor '{constructorText}' for operator 'is'."
                                        | Some(_, constructorInfo) ->
                                            unifyTypes Map.empty (constructorResultType constructorInfo) leftType
                                            |> Result.bind (fun substitution ->
                                                match tryResolveEnumConstructorOrdinal state constructorInfo with
                                                | Some caseOrdinal ->
                                                    il.Emit(OpCodes.Ldc_I4, caseOrdinal)
                                                    emitComparisonFromCeq il false
                                                    Result.Ok()
                                                | None ->
                                                    let constructorType, _, _ =
                                                        resolveConstructorTypeAndMembers state typeParameters substitution constructorInfo

                                                    let successLabel = il.DefineLabel()
                                                    let doneLabel = il.DefineLabel()
                                                    il.Emit(OpCodes.Isinst, constructorType)
                                                    il.Emit(OpCodes.Brtrue, successLabel)
                                                    il.Emit(OpCodes.Ldc_I4_0)
                                                    il.Emit(OpCodes.Br, doneLabel)
                                                    il.MarkLabel(successLabel)
                                                    il.Emit(OpCodes.Ldc_I4_1)
                                                    il.MarkLabel(doneLabel)
                                                    Result.Ok())
                | _ ->
                    let! leftType = inferExpressionType currentModule localTypes None left
                    let! rightType = inferExpressionType currentModule localTypes None right
                    do! emitExpression state currentModule typeParameters localValues (Some leftType) il left
                    do! emitExpression state currentModule typeParameters localValues (Some rightType) il right

                    match operatorName, leftType, rightType with
                    | "+", IlPrimitive IlInt64, IlPrimitive IlInt64 ->
                        il.Emit(OpCodes.Add)
                    | "-", IlPrimitive IlInt64, IlPrimitive IlInt64 ->
                        il.Emit(OpCodes.Sub)
                    | "*", IlPrimitive IlInt64, IlPrimitive IlInt64 ->
                        il.Emit(OpCodes.Mul)
                    | "/", IlPrimitive IlInt64, IlPrimitive IlInt64 ->
                        il.Emit(OpCodes.Div)
                    | "+", IlPrimitive IlFloat64, IlPrimitive IlFloat64 ->
                        il.Emit(OpCodes.Add)
                    | "-", IlPrimitive IlFloat64, IlPrimitive IlFloat64 ->
                        il.Emit(OpCodes.Sub)
                    | "*", IlPrimitive IlFloat64, IlPrimitive IlFloat64 ->
                        il.Emit(OpCodes.Mul)
                    | "/", IlPrimitive IlFloat64, IlPrimitive IlFloat64 ->
                        il.Emit(OpCodes.Div)
                    | "==", IlPrimitive IlInt64, IlPrimitive IlInt64 ->
                        emitComparisonFromCeq il false
                    | "!=", IlPrimitive IlInt64, IlPrimitive IlInt64 ->
                        emitComparisonFromCeq il true
                    | "==", IlPrimitive IlFloat64, IlPrimitive IlFloat64 ->
                        emitComparisonFromCeq il false
                    | "!=", IlPrimitive IlFloat64, IlPrimitive IlFloat64 ->
                        emitComparisonFromCeq il true
                    | "==", IlPrimitive IlBool, IlPrimitive IlBool ->
                        emitComparisonFromCeq il false
                    | "==", IlNamed(leftIdentity, []), IlNamed(rightIdentity, [])
                        when TypeIdentity.hasTopLevelName preludeModuleIdentity Stdlib.KnownTypeNames.Bool leftIdentity
                             && TypeIdentity.hasTopLevelName preludeModuleIdentity Stdlib.KnownTypeNames.Bool rightIdentity ->
                        emitComparisonFromCeq il false
                    | "==", leftEnumType, rightEnumType
                        when leftEnumType = rightEnumType && isClrEnumIlType state leftEnumType ->
                        emitComparisonFromCeq il false
                    | "!=", IlPrimitive IlBool, IlPrimitive IlBool ->
                        emitComparisonFromCeq il true
                    | "!=", IlNamed(leftIdentity, []), IlNamed(rightIdentity, [])
                        when TypeIdentity.hasTopLevelName preludeModuleIdentity Stdlib.KnownTypeNames.Bool leftIdentity
                             && TypeIdentity.hasTopLevelName preludeModuleIdentity Stdlib.KnownTypeNames.Bool rightIdentity ->
                        emitComparisonFromCeq il true
                    | "!=", leftEnumType, rightEnumType
                        when leftEnumType = rightEnumType && isClrEnumIlType state leftEnumType ->
                        emitComparisonFromCeq il true
                    | "==", IlPrimitive IlString, IlPrimitive IlString ->
                        emitStringEquality il false
                    | "!=", IlPrimitive IlString, IlPrimitive IlString ->
                        emitStringEquality il true
                    | "==", IlPrimitive IlChar, IlPrimitive IlChar ->
                        emitComparisonFromCeq il false
                    | "!=", IlPrimitive IlChar, IlPrimitive IlChar ->
                        emitComparisonFromCeq il true
                    | "<", IlPrimitive IlInt64, IlPrimitive IlInt64 ->
                        il.Emit(OpCodes.Clt)
                    | "<=", IlPrimitive IlInt64, IlPrimitive IlInt64 ->
                        il.Emit(OpCodes.Cgt)
                        il.Emit(OpCodes.Ldc_I4_0)
                        il.Emit(OpCodes.Ceq)
                    | ">", IlPrimitive IlInt64, IlPrimitive IlInt64 ->
                        il.Emit(OpCodes.Cgt)
                    | ">=", IlPrimitive IlInt64, IlPrimitive IlInt64 ->
                        il.Emit(OpCodes.Clt)
                        il.Emit(OpCodes.Ldc_I4_0)
                        il.Emit(OpCodes.Ceq)
                    | "<", IlPrimitive IlFloat64, IlPrimitive IlFloat64 ->
                        il.Emit(OpCodes.Clt)
                    | "<=", IlPrimitive IlFloat64, IlPrimitive IlFloat64 ->
                        il.Emit(OpCodes.Cgt)
                        il.Emit(OpCodes.Ldc_I4_0)
                        il.Emit(OpCodes.Ceq)
                    | ">", IlPrimitive IlFloat64, IlPrimitive IlFloat64 ->
                        il.Emit(OpCodes.Cgt)
                    | ">=", IlPrimitive IlFloat64, IlPrimitive IlFloat64 ->
                        il.Emit(OpCodes.Clt)
                        il.Emit(OpCodes.Ldc_I4_0)
                        il.Emit(OpCodes.Ceq)
                    | _ ->
                        return!
                            Result.Error(
                                DiagnosticFact.ClrBackendEmitterError.message
                                    (BinaryOperatorUnsupported(
                                        operatorName,
                                        formatIlType leftType,
                                        formatIlType rightType
                                    ))
                            )
            }

        let rec emitSyntheticFileHandle resultType =
            result {
                match resultType with
                | IlNamed(typeIdentity, [ innerType ]) when TypeIdentity.hasTopLevelName preludeModuleIdentity Stdlib.KnownTypeNames.IO typeIdentity ->
                    return! emitSyntheticFileHandle innerType
                | IlNamed(typeIdentity, typeArguments) ->
                    let moduleName = TypeIdentity.moduleIdentity typeIdentity |> ModuleIdentity.text
                    let typeName = TypeIdentity.name typeIdentity
                    match state.Environment.DataTypes |> Map.tryFind (moduleName, typeName) with
                    | None ->
                        return!
                            Result.Error
                                $"IL backend intrinsic 'openFile' expected a concrete File-like ADT result, but got {formatIlType resultType}."
                    | Some dataType ->
                        match dataType.Constructors |> Map.tryFind "Handle" with
                        | None ->
                            return!
                                Result.Error
                                    $"IL backend intrinsic 'openFile' expected '{formatIlType resultType}' to expose a 'Handle' constructor."
                        | Some constructorInfo ->
                            if List.length constructorInfo.FieldTypes <> 1 then
                                return!
                                    Result.Error
                                        $"IL backend intrinsic 'openFile' expected constructor '{moduleName}.{constructorInfo.Name}' to take one Int field."
                            else
                                let substitution =
                                    if List.length constructorInfo.TypeParameters = List.length typeArguments then
                                        List.zip constructorInfo.TypeParameters typeArguments |> Map.ofList
                                    else
                                        Map.empty

                                let fieldType = substituteType substitution constructorInfo.FieldTypes[0]

                                if fieldType <> IlPrimitive IlInt64 then
                                    return!
                                        Result.Error
                                            $"IL backend intrinsic 'openFile' expected constructor '{moduleName}.{constructorInfo.Name}' to take Int, but got {formatIlType fieldType}."

                                let _, constructor, _ = resolveConstructorTypeAndMembers state typeParameters substitution constructorInfo
                                il.Emit(OpCodes.Ldc_I8, 1L)
                                il.Emit(OpCodes.Newobj, constructor)
                | _ ->
                    return!
                        Result.Error
                            $"IL backend intrinsic 'openFile' expected a concrete File-like ADT result, but got {formatIlType resultType}."
            }

        let emitIntrinsicCall name arguments =
            result {
                let! argumentTypes =
                    arguments
                    |> List.fold
                        (fun stateResult argumentExpression ->
                            result {
                                let! collected = stateResult
                                let! argumentType = inferExpressionType currentModule localTypes None argumentExpression
                                return argumentType :: collected
                            })
                        (Result.Ok [])
                    |> Result.map List.rev

                let intrinsicTypes =
                    match name, argumentTypes, expectedType with
                    | "openFile", [ IlPrimitive IlString ], Some resultType ->
                        Some([ IlPrimitive IlString ], resultType)
                    | "openFile", [ IlPrimitive IlString ], None ->
                        tryDefaultFileType state.Environment.Modules currentModule
                        |> Option.map (fun resultType -> [ IlPrimitive IlString ], resultType)
                    | _ ->
                        intrinsicParameterTypes name argumentTypes

                match intrinsicTypes with
                | None ->
                    if not (knownIntrinsicNames.Contains name) then
                        return! Result.Error $"IL backend could not resolve callee '{name}'."
                    else
                        return!
                            Result.Error(
                                DiagnosticFact.ClrBackendEmitterError.message
                                    (IntrinsicArgumentTypesUnsupported(
                                        name,
                                        argumentTypes |> List.map formatIlType
                                    ))
                            )
                | Some(parameterTypes, resultType) ->
                    let usesCustomArgumentEmission =
                        String.Equals(name, KnownPreludeSemantics.BuiltinPreludeShowHelperName, StringComparison.Ordinal)
                        || String.Equals(name, KnownPreludeSemantics.BuiltinPreludeCompareHelperName, StringComparison.Ordinal)

                    if not usesCustomArgumentEmission then
                        do!
                            List.zip arguments parameterTypes
                            |> List.fold
                                (fun stateResult (argumentExpression, parameterType) ->
                                    stateResult
                                    |> Result.bind (fun () ->
                                        emitExpression state currentModule typeParameters localValues (Some parameterType) il argumentExpression))
                                (Result.Ok())

                    let emitBoxedArgument argumentExpression argumentType =
                        result {
                            do! emitExpression state currentModule typeParameters localValues (Some argumentType) il argumentExpression
                            let clrArgumentType = resolveClrType state typeParameters argumentType

                            if clrArgumentType.IsValueType then
                                il.Emit(OpCodes.Box, clrArgumentType)
                        }

                    let emitOrderingConstructor constructorName =
                        match tryResolveConstructor state.Environment.Modules currentModule [ constructorName ] with
                        | Some(_, constructorInfo) ->
                            match tryResolveEnumConstructorOrdinal state constructorInfo with
                            | Some caseOrdinal ->
                                il.Emit(OpCodes.Ldc_I4, caseOrdinal)
                                Result.Ok()
                            | None ->
                                let _, constructor, _ =
                                    resolveConstructorTypeAndMembers state typeParameters Map.empty constructorInfo

                                il.Emit(OpCodes.Newobj, constructor)
                                Result.Ok()
                        | None ->
                            Result.Error $"IL backend could not resolve Ordering constructor '{constructorName}'."

                    match name, resultType with
                    | "print", _ ->
                        il.Emit(OpCodes.Call, typeof<Console>.GetMethod("Write", [| typeof<string> |]))
                        do! emitUnitValue il
                    | "println", _ ->
                        il.Emit(OpCodes.Call, typeof<Console>.GetMethod("WriteLine", [| typeof<string> |]))
                        do! emitUnitValue il
                    | "primitiveIntToString", _ ->
                        il.Emit(OpCodes.Call, typeof<Convert>.GetMethod("ToString", [| typeof<int64> |]))
                    | "unsafeConsume", _ ->
                        il.Emit(OpCodes.Pop)
                        do! emitUnitValue il
                    | "pure", _ ->
                        ()
                    | "openFile", resultType ->
                        il.Emit(OpCodes.Pop)
                        do! emitSyntheticFileHandle resultType
                    | ("primitiveReadData" | "readData"), _ ->
                        il.Emit(OpCodes.Pop)
                        il.Emit(OpCodes.Ldstr, "chunk")
                    | "primitiveCloseFile", _ ->
                        il.Emit(OpCodes.Pop)
                        il.Emit(OpCodes.Ldstr, "closed")
                        il.Emit(OpCodes.Call, typeof<Console>.GetMethod("Write", [| typeof<string> |]))
                        do! emitUnitValue il
                    | "newRef", refType ->
                        let clrResultType = resolveClrType state typeParameters refType
                        let argumentClrType = resolveClrType state typeParameters argumentTypes[0]
                        let ctor = clrResultType.GetConstructor([| argumentClrType |])
                        il.Emit(OpCodes.Newobj, ctor)
                    | "readRef", _ ->
                        let refType = resolveClrType state typeParameters argumentTypes[0]
                        let valueField = refType.GetField("Value")
                        il.Emit(OpCodes.Ldfld, valueField)
                    | "writeRef", _ ->
                        let refType = resolveClrType state typeParameters argumentTypes[0]
                        let valueField = refType.GetField("Value")
                        il.Emit(OpCodes.Stfld, valueField)
                        do! emitUnitValue il
                    | "not", _ ->
                        il.Emit(OpCodes.Ldc_I4_0)
                        il.Emit(OpCodes.Ceq)
                    | "negate", IlPrimitive IlInt64
                    | "negate", IlPrimitive IlFloat64 ->
                        il.Emit(OpCodes.Neg)
                    | ("and" | "or"), _ ->
                        il.Emit(if name = "and" then OpCodes.And else OpCodes.Or)
                    | intrinsicName, IlPrimitive IlString when intrinsicName = KnownPreludeSemantics.BuiltinPreludeShowHelperName ->
                        do! emitBoxedArgument arguments[0] argumentTypes[0]
                        il.Emit(OpCodes.Call, showBuiltinMethod)
                    | intrinsicName, IlNamed(typeIdentity, []) when intrinsicName = KnownPreludeSemantics.BuiltinPreludeCompareHelperName && TypeIdentity.hasTopLevelName preludeModuleIdentity Stdlib.KnownTypeNames.Ordering typeIdentity ->
                        let comparisonLocal = il.DeclareLocal(typeof<int>)
                        let orderingLocal = il.DeclareLocal(resolveClrType state typeParameters resultType)
                        let lessLabel = il.DefineLabel()
                        let greaterLabel = il.DefineLabel()
                        let equalLabel = il.DefineLabel()
                        let doneLabel = il.DefineLabel()
                        do! emitBoxedArgument arguments[0] argumentTypes[0]
                        do! emitBoxedArgument arguments[1] argumentTypes[1]
                        il.Emit(OpCodes.Call, compareBuiltinSignMethod)
                        il.Emit(OpCodes.Stloc, comparisonLocal)
                        il.Emit(OpCodes.Ldloc, comparisonLocal)
                        il.Emit(OpCodes.Ldc_I4_0)
                        il.Emit(OpCodes.Blt, lessLabel)
                        il.Emit(OpCodes.Ldloc, comparisonLocal)
                        il.Emit(OpCodes.Ldc_I4_0)
                        il.Emit(OpCodes.Bgt, greaterLabel)
                        il.Emit(OpCodes.Br, equalLabel)
                        il.MarkLabel(lessLabel)
                        do! emitOrderingConstructor "LT"
                        il.Emit(OpCodes.Stloc, orderingLocal)
                        il.Emit(OpCodes.Br, doneLabel)
                        il.MarkLabel(greaterLabel)
                        do! emitOrderingConstructor "GT"
                        il.Emit(OpCodes.Stloc, orderingLocal)
                        il.Emit(OpCodes.Br, doneLabel)
                        il.MarkLabel(equalLabel)
                        do! emitOrderingConstructor "EQ"
                        il.Emit(OpCodes.Stloc, orderingLocal)
                        il.MarkLabel(doneLabel)
                        il.Emit(OpCodes.Ldloc, orderingLocal)
                    | _ ->
                        return! Result.Error $"IL backend intrinsic '{name}' is not implemented yet."
            }

        let cleanupExpressionForExitAction action =
            match action with
            | KRuntimeDeferred expression ->
                KRuntimeExecute expression
            | KRuntimeRelease(_, KRuntimeTraitCall(traitName, memberName, dictionary, []), resource) ->
                KRuntimeExecute(KRuntimeTraitCall(traitName, memberName, dictionary, [ resource ]))
            | KRuntimeRelease(_, release, resource) ->
                KRuntimeExecute(KRuntimeApply(release, [ resource ]))

        let emitTraitCall traitName memberName dictionary arguments =
            result {
                let! resultType =
                    inferExpressionType
                        currentModule
                        localTypes
                        expectedType
                        (KRuntimeTraitCall(traitName, memberName, dictionary, arguments))

                let! argumentTypes =
                    arguments
                    |> List.fold
                        (fun stateResult argumentExpression ->
                            result {
                                let! collected = stateResult
                                let! argumentType = inferExpressionType currentModule localTypes None argumentExpression
                                return argumentType :: collected
                            })
                        (Result.Ok [])
                    |> Result.map List.rev

                let routes = traitMemberRoutes state.Environment traitName memberName

                if List.isEmpty routes then
                    return! Result.Error $"IL backend could not find any routes for trait call '{traitName}.{memberName}'."

                let dictionaryLocal = il.DeclareLocal(typeof<Tuple<string, string, string>>)
                let resultLocal = il.DeclareLocal(resolveClrType state typeParameters resultType)

                let argumentLocals =
                    argumentTypes
                    |> List.map (resolveClrType state typeParameters >> il.DeclareLocal)

                do! emitExpression state currentModule typeParameters localValues None il dictionary
                il.Emit(OpCodes.Stloc, dictionaryLocal)

                do!
                    List.zip3 arguments argumentTypes argumentLocals
                    |> List.fold
                        (fun stateResult (argumentExpression, argumentType, argumentLocal) ->
                            stateResult
                            |> Result.bind (fun () ->
                                emitExpression state currentModule typeParameters localValues (Some argumentType) il argumentExpression
                                |> Result.map (fun () -> il.Emit(OpCodes.Stloc, argumentLocal))))
                        (Result.Ok())

                let endLabel = il.DefineLabel()

                do!
                    routes
                    |> List.fold
                        (fun stateResult (instanceInfo, bindingName) ->
                            stateResult
                            |> Result.bind (fun () ->
                                result {
                                let nextRouteLabel = il.DefineLabel()
                                let routeBindingInfo = state.Environment.Modules[instanceInfo.ModuleName].Bindings[bindingName]
                                let! targetMethod =
                                    resolveMethodInfoForCall
                                        state
                                        typeParameters
                                        instanceInfo.ModuleName
                                        routeBindingInfo
                                        Map.empty

                                if List.length routeBindingInfo.ParameterTypes <> List.length argumentLocals + 1 then
                                    return!
                                        Result.Error
                                            $"IL backend trait route '{instanceInfo.ModuleName}.{bindingName}' expected {List.length routeBindingInfo.ParameterTypes} argument(s), but the trait call supplies 1 dictionary and {List.length argumentLocals} explicit argument(s)."

                                il.Emit(OpCodes.Ldloc, dictionaryLocal)
                                il.Emit(OpCodes.Callvirt, dictionaryModuleGetter)
                                il.Emit(OpCodes.Ldstr, instanceInfo.ModuleName)
                                il.Emit(OpCodes.Call, stringEqualityMethod)
                                il.Emit(OpCodes.Brfalse, nextRouteLabel)

                                il.Emit(OpCodes.Ldloc, dictionaryLocal)
                                il.Emit(OpCodes.Callvirt, dictionaryTraitGetter)
                                il.Emit(OpCodes.Ldstr, traitName)
                                il.Emit(OpCodes.Call, stringEqualityMethod)
                                il.Emit(OpCodes.Brfalse, nextRouteLabel)

                                il.Emit(OpCodes.Ldloc, dictionaryLocal)
                                il.Emit(OpCodes.Callvirt, dictionaryInstanceGetter)
                                il.Emit(OpCodes.Ldstr, instanceInfo.InstanceKey)
                                il.Emit(OpCodes.Call, stringEqualityMethod)
                                il.Emit(OpCodes.Brfalse, nextRouteLabel)

                                emitLoadLocal il dictionaryLocal

                                List.zip3 argumentLocals argumentTypes (routeBindingInfo.ParameterTypes |> List.tail |> List.map snd)
                                |> List.iter (fun (argumentLocal, sourceType, targetType) ->
                                    emitLoadLocal il argumentLocal
                                    emitCoerceStackValue state typeParameters sourceType targetType il)

                                il.Emit(OpCodes.Call, targetMethod)
                                il.Emit(OpCodes.Stloc, resultLocal)
                                il.Emit(OpCodes.Br, endLabel)
                                il.MarkLabel(nextRouteLabel)
                                return ()
                                }))
                        (Result.Ok())

                let exceptionCtor = typeof<InvalidOperationException>.GetConstructor([| typeof<string> |])
                il.Emit(OpCodes.Ldstr, $"No trait route matched for '{traitName}.{memberName}'.")
                il.Emit(OpCodes.Newobj, exceptionCtor)
                il.Emit(OpCodes.Throw)
                il.MarkLabel(endLabel)
                il.Emit(OpCodes.Ldloc, resultLocal)
            }

        let rec emitPatternMatch currentScope expectedType pattern valueLocal failureLabel =
            match pattern with
            | KRuntimeWildcardPattern ->
                Result.Ok currentScope
            | KRuntimeNamePattern name ->
                Result.Ok(currentScope |> Map.add name { Location = Local valueLocal; Type = expectedType })
            | KRuntimeLiteralPattern literal ->
                result {
                    emitLoadLocal il valueLocal
                    do! emitLiteral il literal

                    match literal with
                    | LiteralValue.String _
                    | LiteralValue.Character _
                    | LiteralValue.Grapheme _ ->
                        emitStringEquality il false
                    | _ ->
                        emitComparisonFromCeq il false

                    il.Emit(OpCodes.Brfalse, (failureLabel: Label))
                    return currentScope
                }
            | KRuntimeOrPattern alternatives ->
                result {
                    let rec collectBindings currentExpectedType currentPattern =
                        match currentPattern with
                        | KRuntimeWildcardPattern ->
                            Result.Ok(Map.empty<string, IlType>)
                        | KRuntimeNamePattern name ->
                            Result.Ok(Map.ofList [ name, currentExpectedType ])
                        | KRuntimeLiteralPattern literal ->
                            let literalType =
                                match literal with
                                | LiteralValue.Integer _ -> IlPrimitive IlInt64
                                | LiteralValue.Float _ -> IlPrimitive IlFloat64
                                | LiteralValue.String _ -> IlPrimitive IlString
                                | LiteralValue.Character _
                                | LiteralValue.Grapheme _ -> IlPrimitive IlString
                                | LiteralValue.Byte _ -> IlPrimitive IlInt64
                                | LiteralValue.Unit -> unitIlType

                            if literalType = currentExpectedType then
                                Result.Ok(Map.empty<string, IlType>)
                            else
                                Result.Error
                                    $"IL backend cannot match literal of type {formatIlType literalType} against {formatIlType currentExpectedType}."
                        | KRuntimeOrPattern nestedAlternatives ->
                            nestedAlternatives
                            |> List.fold
                                (fun state alternative ->
                                    state
                                    |> Result.bind (fun collected ->
                                        collectBindings currentExpectedType alternative
                                        |> Result.map (fun bindings -> bindings :: collected)))
                                (Result.Ok [])
                            |> Result.bind (fun collected ->
                                match List.rev collected with
                                | [] ->
                                    Result.Ok(Map.empty<string, IlType>)
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
                                        (Result.Ok firstBindings))
                        | KRuntimeConstructorPattern(nameSegments, argumentPatterns) ->
                            match tryResolveConstructor state.Environment.Modules currentModule nameSegments with
                            | None ->
                                let patternName = String.concat "." nameSegments
                                Result.Error $"IL backend could not resolve constructor pattern '{patternName}'."
                            | Some(_, constructorInfo) ->
                                unifyTypes Map.empty (constructorResultType constructorInfo) currentExpectedType
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

                                                    collectBindings fieldType argumentPattern
                                                    |> Result.map (fun childBindings ->
                                                        Map.fold (fun acc key value -> acc |> Map.add key value) bindings childBindings)))
                                            (Result.Ok(Map.empty<string, IlType>)))

                    let! agreedBindings = collectBindings expectedType pattern

                    let sharedBindings =
                        agreedBindings
                        |> Map.toList
                        |> List.map (fun (name, bindingType) ->
                            let sharedLocal = il.DeclareLocal(resolveClrType state typeParameters bindingType)
                            name,
                            { Location = Local sharedLocal
                              Type = bindingType })

                    let sharedScope =
                        sharedBindings
                        |> List.fold (fun scope (name, value) -> scope |> Map.add name value) currentScope

                    let successLabel = il.DefineLabel()

                    let emitSharedAssignments alternativeScope =
                        sharedBindings
                        |> List.iter (fun (name, sharedValue) ->
                            let alternativeValue = alternativeScope |> Map.find name
                            loadValue il alternativeValue
                            emitCoerceStackValue state typeParameters alternativeValue.Type sharedValue.Type il

                            match sharedValue.Location with
                            | Local sharedLocal ->
                                il.Emit(OpCodes.Stloc, sharedLocal)
                            | Argument _ ->
                                invalidOp "IL backend shared or-pattern bindings must lower to locals.")

                    let rec emitAlternatives remaining =
                        match remaining with
                        | [] ->
                            il.Emit(OpCodes.Br, (failureLabel: Label))
                            Result.Ok()
                        | [ alternative ] ->
                            emitPatternMatch currentScope expectedType alternative valueLocal failureLabel
                            |> Result.map (fun alternativeScope ->
                                emitSharedAssignments alternativeScope
                                il.Emit(OpCodes.Br, (successLabel: Label)))
                        | alternative :: rest ->
                            let nextAlternativeLabel = il.DefineLabel()

                            emitPatternMatch currentScope expectedType alternative valueLocal nextAlternativeLabel
                            |> Result.bind (fun alternativeScope ->
                                emitSharedAssignments alternativeScope
                                il.Emit(OpCodes.Br, (successLabel: Label))
                                il.MarkLabel(nextAlternativeLabel)
                                emitAlternatives rest)

                    do! emitAlternatives alternatives
                    il.MarkLabel(successLabel)
                    return sharedScope
                }
            | KRuntimeConstructorPattern(nameSegments, argumentPatterns) ->
                match tryResolveConstructor state.Environment.Modules currentModule nameSegments with
                | None ->
                    let patternName = String.concat "." nameSegments
                    Result.Error $"IL backend could not resolve constructor pattern '{patternName}'."
                | Some(_, constructorInfo) ->
                    unifyTypes Map.empty (constructorResultType constructorInfo) expectedType
                    |> Result.bind (fun substitution ->
                        if List.length argumentPatterns <> List.length constructorInfo.FieldTypes then
                            Result.Error
                                $"IL backend expected pattern '{constructorInfo.Name}' to receive {List.length constructorInfo.FieldTypes} argument(s), but received {List.length argumentPatterns}."
                        else
                            match tryResolveEnumConstructorOrdinal state constructorInfo with
                            | Some caseOrdinal ->
                                emitLoadLocal il valueLocal
                                il.Emit(OpCodes.Ldc_I4, caseOrdinal)
                                il.Emit(OpCodes.Ceq)
                                il.Emit(OpCodes.Brfalse, (failureLabel: Label))
                                Result.Ok currentScope
                            | None ->
                                let constructorType, _, fieldInfos = resolveConstructorTypeAndMembers state typeParameters substitution constructorInfo
                                let castLocal = il.DeclareLocal(constructorType)
                                emitLoadLocal il valueLocal
                                il.Emit(OpCodes.Isinst, constructorType)
                                il.Emit(OpCodes.Stloc, castLocal)
                                il.Emit(OpCodes.Ldloc, castLocal)
                                il.Emit(OpCodes.Brfalse, (failureLabel: Label))

                                List.zip3 argumentPatterns constructorInfo.FieldTypes (fieldInfos |> Array.toList)
                                |> List.fold
                                    (fun stateResult (argumentPattern, fieldTemplate, fieldInfo) ->
                                        stateResult
                                        |> Result.bind (fun scope ->
                                            let fieldType = substituteType substitution fieldTemplate
                                            let fieldLocal = il.DeclareLocal(resolveClrType state typeParameters fieldType)
                                            il.Emit(OpCodes.Ldloc, castLocal)
                                            il.Emit(OpCodes.Ldfld, (fieldInfo: FieldInfo))
                                            il.Emit(OpCodes.Stloc, fieldLocal)
                                            emitPatternMatch scope fieldType argumentPattern fieldLocal failureLabel))
                                    (Result.Ok currentScope))

        let emitMatch (scrutinee: KRuntimeExpression) (cases: KRuntimeMatchCase list) =
            result {
                let! scrutineeType = inferExpressionType currentModule localTypes None scrutinee
                let! matchType = inferExpressionType currentModule localTypes expectedType (KRuntimeMatch(scrutinee, cases))

                let scrutineeLocal = il.DeclareLocal(resolveClrType state typeParameters scrutineeType)
                let resultLocal = il.DeclareLocal(resolveClrType state typeParameters matchType)
                let endLabel = il.DefineLabel()

                do! emitExpression state currentModule typeParameters localValues (Some scrutineeType) il scrutinee
                il.Emit(OpCodes.Stloc, scrutineeLocal)

                let rec emitCases (remainingCases: KRuntimeMatchCase list) =
                    match remainingCases with
                    | [] ->
                        let exceptionCtor = typeof<InvalidOperationException>.GetConstructor([| typeof<string> |])
                        il.Emit(OpCodes.Ldstr, "Non-exhaustive match.")
                        il.Emit(OpCodes.Newobj, exceptionCtor)
                        il.Emit(OpCodes.Throw)
                        Result.Ok()
                    | (caseClause: KRuntimeMatchCase) :: rest ->
                        let nextCaseLabel = il.DefineLabel()

                        emitPatternMatch localValues scrutineeType caseClause.Pattern scrutineeLocal nextCaseLabel
                        |> Result.bind (fun caseScope ->
                            let emitBody () =
                                emitExpression state currentModule typeParameters caseScope (Some matchType) il caseClause.Body
                                |> Result.map (fun () ->
                                    il.Emit(OpCodes.Stloc, resultLocal)
                                    il.Emit(OpCodes.Br, endLabel)
                                    il.MarkLabel(nextCaseLabel))

                            match caseClause.Guard with
                            | Some guard ->
                                emitExpression state currentModule typeParameters caseScope (Some(IlPrimitive IlBool)) il guard
                                |> Result.map (fun () ->
                                    il.Emit(OpCodes.Brfalse, nextCaseLabel))
                                |> Result.bind (fun () -> emitBody ())
                            | None ->
                                emitBody ())
                        |> Result.bind (fun () -> emitCases rest)

                do! emitCases cases
                il.MarkLabel(endLabel)
                il.Emit(OpCodes.Ldloc, resultLocal)
            }

        match expression with
        | KRuntimeLiteral literal ->
            emitLiteral il literal
        | KRuntimeEffectLabel _
        | KRuntimeEffectOperation _
        | KRuntimeHandle _ ->
            Result.Error(DiagnosticFact.ClrBackendEmitterError.message EffectHandlersUnsupported)
        | KRuntimeName [ "True" ] ->
            il.Emit(OpCodes.Ldc_I4_1)
            Result.Ok()
        | KRuntimeName [ "False" ] ->
            il.Emit(OpCodes.Ldc_I4_0)
            Result.Ok()
        | KRuntimeName [ name ] when localValues |> Map.containsKey name ->
            loadValue il localValues[name]
            Result.Ok()
        | KRuntimeName segments ->
            let nameText = String.concat "." segments

            match tryResolveBinding state.Environment.Modules currentModule segments with
            | Some(targetModule, bindingInfo) when List.isEmpty bindingInfo.ParameterTypes ->
                result {
                    let! targetMethod = resolveMethodInfoForCall state typeParameters targetModule bindingInfo Map.empty
                    il.Emit(OpCodes.Call, targetMethod)
                }
            | Some(targetModule, bindingInfo) ->
                Result.Error(
                    DiagnosticFact.ClrBackendEmitterError.message
                        (FunctionValuedNameUnsupported(targetModule, bindingInfo.Binding.Name))
                )
            | None ->
                match tryResolveConstructor state.Environment.Modules currentModule segments with
                | Some(_, constructorInfo) when List.isEmpty constructorInfo.FieldTypes ->
                    emitConstructorApplication constructorInfo [] expectedType
                | Some(targetModule, constructorInfo) ->
                    Result.Error(
                        DiagnosticFact.ClrBackendEmitterError.message
                            (ConstructorValuedNameUnsupported(targetModule, constructorInfo.Name))
                    )
                | None ->
                    let localsText =
                        localValues |> Map.toList |> List.map fst |> String.concat ", "

                    Result.Error $"IL backend could not resolve name '{nameText}'. Locals in scope: [{localsText}]."
        | KRuntimeUnary("-", operand) ->
            result {
                let! operandType = inferExpressionType currentModule localTypes None operand
                do! emitExpression state currentModule typeParameters localValues (Some operandType) il operand

                match operandType with
                | IlPrimitive IlInt64
                | IlPrimitive IlFloat64 ->
                    il.Emit(OpCodes.Neg)
                | other ->
                    return! Result.Error $"Unary '-' is not supported for {formatIlType other}."
            }
        | KRuntimeUnary(operatorName, _) ->
            Result.Error(
                DiagnosticFact.ClrBackendEmitterError.message (UnaryOperatorUnsupported operatorName)
            )
        | KRuntimeBinary(left, "&&", right) ->
            let falseLabel = il.DefineLabel()
            let endLabel = il.DefineLabel()

            result {
                do! emitExpression state currentModule typeParameters localValues (Some(IlPrimitive IlBool)) il left
                il.Emit(OpCodes.Brfalse, falseLabel)
                do! emitExpression state currentModule typeParameters localValues (Some(IlPrimitive IlBool)) il right
                il.Emit(OpCodes.Br, endLabel)
                il.MarkLabel(falseLabel)
                il.Emit(OpCodes.Ldc_I4_0)
                il.MarkLabel(endLabel)
            }
        | KRuntimeBinary(left, "||", right) ->
            let trueLabel = il.DefineLabel()
            let endLabel = il.DefineLabel()

            result {
                do! emitExpression state currentModule typeParameters localValues (Some(IlPrimitive IlBool)) il left
                il.Emit(OpCodes.Brtrue, trueLabel)
                do! emitExpression state currentModule typeParameters localValues (Some(IlPrimitive IlBool)) il right
                il.Emit(OpCodes.Br, endLabel)
                il.MarkLabel(trueLabel)
                il.Emit(OpCodes.Ldc_I4_1)
                il.MarkLabel(endLabel)
            }
        | KRuntimeBinary(left, operatorName, right) ->
            if KnownPreludeSemantics.isEagerBuiltinBinaryOperator operatorName then
                emitBuiltinBinary operatorName left right
            else
                emitExpression
                    state
                    currentModule
                    typeParameters
                    localValues
                    expectedType
                    il
                    (KRuntimeApply(KRuntimeName [ operatorName ], [ left; right ]))
        | KRuntimeIfThenElse(condition, whenTrue, whenFalse) ->
            result {
                let! resultType = inferExpressionType currentModule localTypes expectedType expression
                let falseLabel = il.DefineLabel()
                let endLabel = il.DefineLabel()

                do! emitExpression state currentModule typeParameters localValues (Some(IlPrimitive IlBool)) il condition
                il.Emit(OpCodes.Brfalse, falseLabel)
                do! emitExpression state currentModule typeParameters localValues (Some resultType) il whenTrue
                il.Emit(OpCodes.Br, endLabel)
                il.MarkLabel(falseLabel)
                do! emitExpression state currentModule typeParameters localValues (Some resultType) il whenFalse
                il.MarkLabel(endLabel)
            }
        | KRuntimeMatch(scrutinee, cases) ->
            emitMatch scrutinee cases
        | KRuntimeApply(KRuntimeName segments, arguments) ->
            let nameText = String.concat "." segments
            match segments, arguments with
            | [ operatorName ], [ left; right ] when KnownPreludeSemantics.isBuiltinBinaryOperator operatorName ->
                emitExpression state currentModule typeParameters localValues expectedType il (KRuntimeBinary(left, operatorName, right))
            | _ ->
                match tryResolveBinding state.Environment.Modules currentModule segments with
                | Some(targetModule, bindingInfo) ->
                    result {
                        let! substitution, _ =
                            inferBindingTypeFromArguments
                                (fun currentModule localTypes _ _ expected innerExpression ->
                                    inferExpressionType currentModule localTypes expected innerExpression)
                                currentModule
                                localTypes
                                activeTypeParameters
                                activeTypeParameters
                                expectedType
                                arguments
                                bindingInfo

                        do!
                            List.zip arguments bindingInfo.ParameterTypes
                            |> List.fold
                                (fun stateResult (argumentExpression, (_, parameterType)) ->
                                    stateResult
                                    |> Result.bind (fun () ->
                                        let specializedParameterType = substituteType substitution parameterType
                                        emitExpression
                                            state
                                            currentModule
                                            typeParameters
                                            localValues
                                            (Some specializedParameterType)
                                            il
                                            argumentExpression))
                                (Result.Ok())

                        let! targetMethod =
                            resolveMethodInfoForCall state typeParameters targetModule bindingInfo substitution

                        il.Emit(OpCodes.Call, targetMethod)
                    }
                | None ->
                    match tryResolveConstructor state.Environment.Modules currentModule segments with
                    | Some(_, constructorInfo) ->
                        emitConstructorApplication constructorInfo arguments expectedType
                    | None ->
                        emitIntrinsicCall nameText arguments
        | KRuntimeExecute inner ->
            emitExpression state currentModule typeParameters localValues expectedType il inner
        | KRuntimeLet(bindingName, value, body) ->
            result {
                let! valueType = inferExpressionType currentModule localTypes None value
                let valueLocal = il.DeclareLocal(resolveClrType state typeParameters valueType)
                do! emitExpression state currentModule typeParameters localValues (Some valueType) il value
                il.Emit(OpCodes.Stloc, valueLocal)

                let extendedLocals =
                    localValues
                    |> Map.add
                        bindingName
                        { Location = Local valueLocal
                          Type = valueType }

                do! emitExpression state currentModule typeParameters extendedLocals expectedType il body
            }
        | KRuntimeDoScope(_, body) ->
            emitExpression state currentModule typeParameters localValues expectedType il body
        | KRuntimeScheduleExit(_, action, body) ->
            result {
                let cleanupExpression = cleanupExpressionForExitAction action
                let! bodyType = inferExpressionType currentModule localTypes expectedType body
                let resultLocal = il.DeclareLocal(resolveClrType state typeParameters bodyType)

                il.BeginExceptionBlock() |> ignore
                do! emitExpression state currentModule typeParameters localValues (Some bodyType) il body
                il.Emit(OpCodes.Stloc, resultLocal)

                il.BeginFinallyBlock()
                do! emitExpression state currentModule typeParameters localValues (Some unitIlType) il cleanupExpression
                il.Emit(OpCodes.Pop)
                il.EndExceptionBlock()

                il.Emit(OpCodes.Ldloc, resultLocal)
            }
        | KRuntimeSequence(first, second) ->
            result {
                let! firstType = inferExpressionType currentModule localTypes None first
                do! emitExpression state currentModule typeParameters localValues (Some firstType) il first
                il.Emit(OpCodes.Pop)
                do! emitExpression state currentModule typeParameters localValues expectedType il second
            }
        | KRuntimeWhile(condition, body) ->
            result {
                let loopLabel = il.DefineLabel()
                let endLabel = il.DefineLabel()

                il.MarkLabel(loopLabel)
                do! emitExpression state currentModule typeParameters localValues (Some(IlPrimitive IlBool)) il condition
                il.Emit(OpCodes.Brfalse, endLabel)

                let! bodyType = inferExpressionType currentModule localTypes None body
                do! emitExpression state currentModule typeParameters localValues (Some bodyType) il body
                il.Emit(OpCodes.Pop)
                il.Emit(OpCodes.Br, loopLabel)
                il.MarkLabel(endLabel)
                do! emitUnitValue il
            }
        | KRuntimeDictionaryValue(moduleName, traitName, instanceKey, captures) ->
            if List.isEmpty captures then
                il.Emit(OpCodes.Ldstr, moduleName)
                il.Emit(OpCodes.Ldstr, traitName)
                il.Emit(OpCodes.Ldstr, instanceKey)
                il.Emit(OpCodes.Newobj, dictionaryTupleConstructor)
                Result.Ok()
            else
                Result.Error
                    $"IL backend does not yet support captured dictionaries for trait instance '{moduleName}.{traitName}.{instanceKey}'."
        | KRuntimeTraitCall(traitName, memberName, dictionary, arguments) ->
            emitTraitCall traitName memberName dictionary arguments
        | KRuntimeApply _ ->
            Result.Error "IL backend currently supports application only when the callee is a named binding."
        | KRuntimeClosure _ ->
            Result.Error(DiagnosticFact.ClrBackendEmitterError.message RuntimeClosuresUnsupported)
        | KRuntimePrefixedString _ ->
            Result.Error(DiagnosticFact.ClrBackendEmitterError.message PrefixedStringsUnsupported)

    let internal emitMethodBody (state: EmissionState) (moduleInfo: ModuleInfo) (bindingInfo: BindingInfo) =
        result {
            let methodEmission = state.MethodBuilders[moduleInfo.Name][bindingInfo.Binding.Name]
            let methodBuilder = methodEmission.Builder
            let il = methodBuilder.GetILGenerator()

            let localValues =
                bindingInfo.ParameterTypes
                |> List.mapi (fun index (name, parameterType) ->
                    name,
                    { Location = Argument index
                      Type = parameterType })
                |> Map.ofList

            let rec emitManagedNumericConversion sourceType targetType =
                if sourceType = targetType then
                    Result.Ok()
                elif sourceType = typeof<int64> && targetType = typeof<int32> then
                    il.Emit(OpCodes.Conv_I4)
                    Result.Ok()
                elif sourceType = typeof<int64> && targetType = typeof<int16> then
                    il.Emit(OpCodes.Conv_I2)
                    Result.Ok()
                elif sourceType = typeof<int64> && targetType = typeof<sbyte> then
                    il.Emit(OpCodes.Conv_I1)
                    Result.Ok()
                elif sourceType = typeof<int64> && targetType = typeof<byte> then
                    il.Emit(OpCodes.Conv_U1)
                    Result.Ok()
                elif sourceType = typeof<int64> && targetType = typeof<uint16> then
                    il.Emit(OpCodes.Conv_U2)
                    Result.Ok()
                elif sourceType = typeof<int64> && targetType = typeof<uint32> then
                    il.Emit(OpCodes.Conv_U4)
                    Result.Ok()
                elif sourceType = typeof<int64> && targetType = typeof<uint64> then
                    il.Emit(OpCodes.Conv_U8)
                    Result.Ok()
                elif sourceType = typeof<int32> && targetType = typeof<int64> then
                    il.Emit(OpCodes.Conv_I8)
                    Result.Ok()
                elif sourceType = typeof<int16> && targetType = typeof<int64> then
                    il.Emit(OpCodes.Conv_I8)
                    Result.Ok()
                elif sourceType = typeof<sbyte> && targetType = typeof<int64> then
                    il.Emit(OpCodes.Conv_I8)
                    Result.Ok()
                elif sourceType = typeof<byte> && targetType = typeof<int64> then
                    il.Emit(OpCodes.Conv_I8)
                    Result.Ok()
                elif sourceType = typeof<uint16> && targetType = typeof<int64> then
                    il.Emit(OpCodes.Conv_I8)
                    Result.Ok()
                elif sourceType = typeof<uint32> && targetType = typeof<int64> then
                    il.Emit(OpCodes.Conv_I8)
                    Result.Ok()
                elif sourceType = typeof<uint64> && targetType = typeof<int64> then
                    il.Emit(OpCodes.Conv_I8)
                    Result.Ok()
                elif sourceType = typeof<float> && targetType = typeof<float32> then
                    il.Emit(OpCodes.Conv_R4)
                    Result.Ok()
                elif sourceType = typeof<float32> && targetType = typeof<float> then
                    il.Emit(OpCodes.Conv_R8)
                    Result.Ok()
                elif targetType.IsEnum then
                    emitManagedNumericConversion sourceType (Enum.GetUnderlyingType targetType)
                elif sourceType.IsEnum then
                    emitManagedNumericConversion (Enum.GetUnderlyingType sourceType) targetType
                else
                    Result.Error $"IL backend cannot convert managed host type '{sourceType}' to '{targetType}'."

            let emitManagedArgument actualParameterType (localValue: LocalValue) =
                result {
                    let sourceClrType = resolveClrType state methodEmission.GenericParameters localValue.Type
                    loadValue il localValue

                    if actualParameterType = sourceClrType || actualParameterType.IsAssignableFrom(sourceClrType) then
                        return ()
                    else
                        do! emitManagedNumericConversion sourceClrType actualParameterType
                }

            let emitManagedReturnConversion actualReturnType =
                let wrapperReturnType = resolveClrType state methodEmission.GenericParameters bindingInfo.ReturnType

                if wrapperReturnType = actualReturnType || wrapperReturnType.IsAssignableFrom(actualReturnType) then
                    Result.Ok()
                else
                    emitManagedNumericConversion actualReturnType wrapperReturnType

            let isUnitWrapperParameter (_, parameterType) =
                match parameterType with
                | IlNamed(typeIdentity, []) ->
                    TypeIdentity.hasTopLevelName preludeModuleIdentity Stdlib.KnownTypeNames.Unit typeIdentity
                | _ ->
                    false

            let emitHostDotNetBindingBody (callable: HostBindings.DotNetHostCallable) =
                result {
                    match callable with
                    | HostBindings.HostConstructor constructorInfo ->
                        let actualParameters = constructorInfo.GetParameters()
                        let hasSyntheticUnit =
                            actualParameters.Length = 0
                            && bindingInfo.ParameterTypes.Length = 1
                            && isUnitWrapperParameter bindingInfo.ParameterTypes.Head

                        if actualParameters.Length <> bindingInfo.ParameterTypes.Length && not hasSyntheticUnit then
                            return!
                                Result.Error
                                    $"IL backend host constructor wrapper '{moduleInfo.Name}.{bindingInfo.Binding.Name}' expected {actualParameters.Length} parameter(s), but the wrapper declares {bindingInfo.ParameterTypes.Length}."

                        do!
                            List.zip
                                (if hasSyntheticUnit then
                                     []
                                 else
                                     bindingInfo.ParameterTypes)
                                (actualParameters |> Array.toList)
                            |> List.fold
                                (fun stateResult ((parameterName, parameterType), parameterInfo) ->
                                    result {
                                        do! stateResult
                                        do! emitManagedArgument parameterInfo.ParameterType localValues[parameterName]
                                    })
                                (Result.Ok())

                        il.Emit(OpCodes.Newobj, constructorInfo)
                        do! emitManagedReturnConversion constructorInfo.DeclaringType
                    | HostBindings.HostMethod methodInfo
                    | HostBindings.HostPropertyGetter methodInfo ->
                        let actualParameters = methodInfo.GetParameters()
                        let wrapperParameters = bindingInfo.ParameterTypes
                        let receiverCount = if methodInfo.IsStatic then 0 else 1
                        let hasSyntheticUnit =
                            match callable with
                            | HostBindings.HostMethod _ ->
                                actualParameters.Length = 0
                                && wrapperParameters.Length = actualParameters.Length + receiverCount + 1
                                && (wrapperParameters |> List.last |> isUnitWrapperParameter)
                            | HostBindings.HostPropertyGetter _ ->
                                false
                            | _ ->
                                false

                        if wrapperParameters.Length <> actualParameters.Length + receiverCount && not hasSyntheticUnit then
                            return!
                                Result.Error
                                    $"IL backend host method wrapper '{moduleInfo.Name}.{bindingInfo.Binding.Name}' does not match the managed member arity."

                        let explicitParameters =
                            if methodInfo.IsStatic then
                                wrapperParameters
                            else
                                wrapperParameters |> List.skip 1
                            |> fun parameters ->
                                if hasSyntheticUnit then
                                    parameters |> List.take actualParameters.Length
                                else
                                    parameters

                        if not methodInfo.IsStatic then
                            let receiverName, _ = wrapperParameters.Head
                            do! emitManagedArgument methodInfo.DeclaringType localValues[receiverName]

                        do!
                            List.zip explicitParameters (actualParameters |> Array.toList)
                            |> List.fold
                                (fun stateResult ((parameterName, _), parameterInfo) ->
                                    result {
                                        do! stateResult
                                        do! emitManagedArgument parameterInfo.ParameterType localValues[parameterName]
                                    })
                                (Result.Ok())

                        let opcode =
                            if methodInfo.IsStatic then
                                OpCodes.Call
                            elif methodInfo.IsVirtual && not methodInfo.IsFinal && not methodInfo.DeclaringType.IsValueType then
                                OpCodes.Callvirt
                            else
                                OpCodes.Call

                        il.Emit(opcode, methodInfo)

                        if methodInfo.ReturnType = typeof<Void> then
                            do! emitUnitValue il
                        else
                            do! emitManagedReturnConversion methodInfo.ReturnType
                }

            match bindingInfo.Binding.Body with
            | None ->
                match bindingInfo.Binding.ExternalBinding with
                | Some externalBinding ->
                    match HostBindings.tryResolveDotNetCallableFromIdentity externalBinding with
                    | Some callable ->
                        do! emitHostDotNetBindingBody callable
                        il.Emit(OpCodes.Ret)
                    | None ->
                        return!
                            Result.Error
                                $"IL backend could not resolve durable host binding metadata for '{moduleInfo.Name}.{bindingInfo.Binding.Name}'."
                | None ->
                    return! Result.Error $"IL backend requires a body for '{moduleInfo.Name}.{bindingInfo.Binding.Name}'."
            | Some body ->
                do!
                    emitExpression
                        state
                        moduleInfo.Name
                        methodEmission.GenericParameters
                        localValues
                        (Some bindingInfo.ReturnType)
                        il
                        body

                il.Emit(OpCodes.Ret)
        }

    let internal defineDataTypes (moduleBuilder: ModuleBuilder) (environment: EmissionEnvironment) =
        let baseTypeDefinitions =
            environment.DataTypes
            |> Map.toList
            |> List.filter (fun (_, dataTypeInfo) -> dataTypeInfo.ExternalRuntimeTypeName.IsNone)
            |> List.map (fun ((moduleName, typeName), dataTypeInfo) ->
                let emission =
                    match dataTypeInfo.Representation with
                    | IlClrEnum ->
                        let enumBuilder =
                            moduleBuilder.DefineEnum(
                                dataTypeInfo.EmittedTypeName,
                                TypeAttributes.Public,
                                typeof<int>
                            )

                        let cases =
                            dataTypeInfo.Constructors
                            |> Map.toList
                            |> List.map (fun (constructorName, constructorInfo) ->
                                constructorName,
                                { LiteralField =
                                    enumBuilder.DefineLiteral(
                                        constructorName,
                                        box constructorInfo.CaseOrdinal
                                    )
                                  CaseOrdinal = constructorInfo.CaseOrdinal })
                            |> Map.ofList

                        EnumEmission
                            { EnumBuilder = enumBuilder
                              Cases = cases }
                    | IlClassHierarchy ->
                        let baseTypeBuilder =
                            moduleBuilder.DefineType(
                                dataTypeInfo.EmittedTypeName,
                                TypeAttributes.Public ||| TypeAttributes.Abstract ||| TypeAttributes.Class
                            )

                        let genericParameters =
                            match dataTypeInfo.TypeParameters with
                            | [] -> [||]
                            | parameters -> baseTypeBuilder.DefineGenericParameters(parameters |> List.toArray)

                        let baseConstructor =
                            baseTypeBuilder.DefineDefaultConstructor(MethodAttributes.Family)

                        ClassHierarchyEmission
                            { BaseTypeBuilder = baseTypeBuilder
                              GenericParameters = genericParameters
                              BaseConstructor = baseConstructor
                              Constructors = Map.empty }

                (moduleName, typeName), emission)
            |> Map.ofList

        baseTypeDefinitions
        |> Map.map (fun (moduleName, typeName) dataEmission ->
            let dataTypeInfo = environment.DataTypes[moduleName, typeName]
            match dataEmission with
            | EnumEmission _ ->
                dataEmission
            | ClassHierarchyEmission classEmission ->
                let genericTypeParameterMap =
                    List.zip dataTypeInfo.TypeParameters (classEmission.GenericParameters |> Array.toList)
                    |> List.map (fun (name, parameter) -> name, (parameter :> Type))
                    |> Map.ofList

                let constructors =
                    dataTypeInfo.Constructors
                    |> Map.toList
                    |> List.map (fun (constructorName, constructorInfo) ->
                        let constructorTypeBuilder =
                            moduleBuilder.DefineType(
                                constructorInfo.EmittedTypeName,
                                TypeAttributes.Public ||| TypeAttributes.Sealed ||| TypeAttributes.Class
                            )

                        let constructorGenericParameters =
                            match dataTypeInfo.TypeParameters with
                            | [] -> [||]
                            | parameters -> constructorTypeBuilder.DefineGenericParameters(parameters |> List.toArray)

                        let constructorTypeParameterMap =
                            if Array.isEmpty constructorGenericParameters then
                                genericTypeParameterMap
                            else
                                List.zip dataTypeInfo.TypeParameters (constructorGenericParameters |> Array.toList)
                                |> List.map (fun (name, parameter) -> name, (parameter :> Type))
                                |> Map.ofList

                        let baseType =
                            if Array.isEmpty constructorGenericParameters then
                                classEmission.BaseTypeBuilder :> Type
                            else
                                let baseArguments =
                                    constructorGenericParameters |> Array.map (fun parameter -> parameter :> Type)

                                (classEmission.BaseTypeBuilder :> Type).MakeGenericType(baseArguments)

                        constructorTypeBuilder.SetParent(baseType)

                        let fieldBuilders =
                            constructorInfo.FieldTypes
                            |> List.mapi (fun index fieldType ->
                                constructorTypeBuilder.DefineField(
                                    $"Item{index + 1}",
                                    resolveClrType
                                        { Environment = environment
                                          ModuleBuilders = Map.empty
                                          MethodBuilders = Map.empty
                                          DataTypeBuilders = baseTypeDefinitions }
                                        constructorTypeParameterMap
                                        fieldType,
                                    FieldAttributes.Public ||| FieldAttributes.InitOnly
                                ))
                            |> List.toArray

                        let constructorParameterTypes =
                            constructorInfo.FieldTypes
                            |> List.map (resolveClrType
                                { Environment = environment
                                  ModuleBuilders = Map.empty
                                  MethodBuilders = Map.empty
                                  DataTypeBuilders = baseTypeDefinitions }
                                constructorTypeParameterMap)
                            |> List.toArray

                        let constructorBuilder =
                            constructorTypeBuilder.DefineConstructor(
                                MethodAttributes.Public,
                                CallingConventions.Standard,
                                constructorParameterTypes
                            )

                        constructorInfo.FieldTypes
                        |> List.iteri (fun index _ ->
                            constructorBuilder.DefineParameter(index + 1, ParameterAttributes.None, $"item{index + 1}")
                            |> ignore)

                        let constructorIl = constructorBuilder.GetILGenerator()
                        constructorIl.Emit(OpCodes.Ldarg_0)

                        let baseConstructor =
                            if Array.isEmpty constructorGenericParameters then
                                classEmission.BaseConstructor :> System.Reflection.ConstructorInfo
                            else
                                TypeBuilder.GetConstructor(baseType, classEmission.BaseConstructor)

                        constructorIl.Emit(OpCodes.Call, baseConstructor)

                        fieldBuilders
                        |> Array.iteri (fun index fieldBuilder ->
                            constructorIl.Emit(OpCodes.Ldarg_0)
                            constructorIl.Emit(OpCodes.Ldarg, index + 1)
                            constructorIl.Emit(OpCodes.Stfld, fieldBuilder))

                        constructorIl.Emit(OpCodes.Ret)

                        constructorName,
                        { TypeBuilder = constructorTypeBuilder
                          GenericParameters = constructorGenericParameters
                          ConstructorBuilder = constructorBuilder
                          FieldBuilders = fieldBuilders })
                    |> Map.ofList

                ClassHierarchyEmission { classEmission with Constructors = constructors })

    let emitClrAssemblyArtifactWithRoots
        (modules: ClrAssemblyModule list)
        (roots: (string * string) list option)
        (outputDirectory: string)
        =
        match buildEnvironment modules roots with
        | Result.Error message ->
            Result.Error message
        | Result.Ok environment ->
            try
                let resolvedOutputDirectory = Path.GetFullPath(outputDirectory)
                Directory.CreateDirectory(resolvedOutputDirectory) |> ignore

                let assemblyName =
                    "Kappa.Generated."
                    + sanitizeIdentifier(Path.GetFileName(resolvedOutputDirectory))

                let assemblyPath = Path.Combine(resolvedOutputDirectory, $"{assemblyName}.dll")
                let assemblyBuilder = PersistedAssemblyBuilder(AssemblyName(assemblyName), typeof<obj>.Assembly)
                let moduleBuilder = assemblyBuilder.DefineDynamicModule(assemblyName)

                let moduleBuilders =
                    environment.Modules
                    |> Map.toList
                    |> List.map (fun (moduleName, moduleInfo) ->
                        let moduleTypeBuilder =
                            moduleBuilder.DefineType(
                                moduleInfo.EmittedTypeName.Value,
                                TypeAttributes.Public ||| TypeAttributes.Abstract ||| TypeAttributes.Sealed ||| TypeAttributes.Class
                            )

                        moduleName, moduleTypeBuilder)
                    |> Map.ofList

                let dataTypeBuilders = defineDataTypes moduleBuilder environment

                let methodBuilders =
                    environment.Modules
                    |> Map.toList
                    |> List.map (fun (moduleName, moduleInfo) ->
                        let methods =
                            moduleInfo.Bindings
                            |> Map.toList
                            |> List.map (fun (bindingName, bindingInfo) ->
                                let methodBuilder =
                                    moduleBuilders[moduleName].DefineMethod(
                                        bindingInfo.EmittedMethodName,
                                        MethodAttributes.Public ||| MethodAttributes.Static ||| MethodAttributes.HideBySig
                                    )

                                let genericParameterMap =
                                    match bindingInfo.TypeParameters with
                                    | [] ->
                                        Map.empty
                                    | typeParameterNames ->
                                        methodBuilder.DefineGenericParameters(typeParameterNames |> List.toArray)
                                        |> Array.toList
                                        |> List.zip typeParameterNames
                                        |> List.map (fun (name, parameter) -> name, (parameter :> Type))
                                        |> Map.ofList

                                let partialState =
                                    { Environment = environment
                                      ModuleBuilders = moduleBuilders
                                      MethodBuilders = Map.empty
                                      DataTypeBuilders = dataTypeBuilders }

                                let parameterTypes =
                                    bindingInfo.ParameterTypes
                                    |> List.map (snd >> resolveClrType partialState genericParameterMap)
                                    |> List.toArray

                                methodBuilder.SetReturnType(resolveClrType partialState genericParameterMap bindingInfo.ReturnType)
                                methodBuilder.SetParameters(parameterTypes)

                                bindingInfo.ParameterTypes
                                |> List.iteri (fun index (parameterName, _) ->
                                    methodBuilder.DefineParameter(index + 1, ParameterAttributes.None, parameterName)
                                    |> ignore)

                                bindingName,
                                { Builder = methodBuilder
                                  GenericParameters = genericParameterMap })
                            |> Map.ofList

                        moduleName, methods)
                    |> Map.ofList

                let state =
                    { Environment = environment
                      ModuleBuilders = moduleBuilders
                      MethodBuilders = methodBuilders
                      DataTypeBuilders = dataTypeBuilders }

                let emissionResult =
                    environment.Modules
                    |> Map.toList
                    |> List.map snd
                    |> List.collect (fun moduleInfo -> moduleInfo.Bindings |> Map.toList |> List.map (fun (_, bindingInfo) -> moduleInfo, bindingInfo))
                    |> List.fold
                        (fun stateResult (moduleInfo, bindingInfo) ->
                            result {
                                do! stateResult
                                do! emitMethodBody state moduleInfo bindingInfo
                            })
                        (Result.Ok())

                match emissionResult with
                | Result.Error message ->
                    Result.Error message
                | Result.Ok() ->
                    dataTypeBuilders
                    |> Map.iter (fun _ dataEmission ->
                        match dataEmission with
                        | EnumEmission enumEmission ->
                            enumEmission.EnumBuilder.CreateType() |> ignore
                        | ClassHierarchyEmission classEmission ->
                            classEmission.BaseTypeBuilder.CreateType() |> ignore)

                    dataTypeBuilders
                    |> Map.iter (fun _ dataEmission ->
                        match dataEmission with
                        | EnumEmission _ ->
                            ()
                        | ClassHierarchyEmission classEmission ->
                            classEmission.Constructors
                            |> Map.iter (fun _ constructorEmission -> constructorEmission.TypeBuilder.CreateType() |> ignore))

                    moduleBuilders |> Map.iter (fun _ moduleTypeBuilder -> moduleTypeBuilder.CreateType() |> ignore)
                    assemblyBuilder.Save(assemblyPath)

                    Result.Ok
                        { OutputDirectory = resolvedOutputDirectory
                          AssemblyName = assemblyName
                          AssemblyFilePath = assemblyPath }
            with ex ->
                Result.Error $"IL backend emission failed: {ex}"

    let private bundledStdlibRoot =
        Path.GetFullPath("__kappa_stdlib__")

    let private isBundledStdlibModule (moduleDump: ClrAssemblyModule) =
        let sourcePath = Path.GetFullPath(moduleDump.SourceFile)
        sourcePath.StartsWith(bundledStdlibRoot, StringComparison.Ordinal)

    let private defaultAssemblyRoots (modules: ClrAssemblyModule list) =
        let userDefinedRoots =
            modules
            |> List.filter (isBundledStdlibModule >> not)
            |> List.collect (fun moduleDump ->
                moduleDump.Bindings
                |> List.filter (fun binding -> not binding.Intrinsic)
                |> List.map (fun binding -> moduleDump.Name, binding.Name))

        match userDefinedRoots with
        | [] -> None
        | roots -> Some roots

    let emitClrAssemblyArtifact (modules: ClrAssemblyModule list) (outputDirectory: string) =
        emitClrAssemblyArtifactWithRoots modules (defaultAssemblyRoots modules) outputDirectory

    let emitAssemblyArtifact (workspace: WorkspaceCompilation) (outputDirectory: string) =
        if workspace.HasErrors then
            Result.Error(
                DiagnosticFact.ClrArtifactEmitterError.message
                    (ClrArtifactWorkspaceHasDiagnostics(PlainClrAssembly, aggregateDiagnostics workspace.Diagnostics))
            )
        else
            let verificationDiagnostics = CheckpointVerification.verifyCheckpoint workspace "KBackendIR"

            if not (List.isEmpty verificationDiagnostics) then
                Result.Error(
                    DiagnosticFact.ClrArtifactEmitterError.message
                        (ClrArtifactMalformedBackendIr(aggregateDiagnostics verificationDiagnostics))
                )
            else
                emitClrAssemblyArtifact workspace.ClrAssemblyIR outputDirectory
