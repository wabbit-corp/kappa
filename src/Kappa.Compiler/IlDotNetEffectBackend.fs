namespace Kappa.Compiler

open System
open System.Collections.Generic
open System.IO
open System.Reflection
open System.Reflection.Emit
open Kappa.Runtime

// Emits direct IL for the effectful dotnet lane against the small Kappa.Runtime surface.
module internal IlDotNetEffectBackend =
    open IlDotNetBackendModel
    open IlDotNetBackendInput
    open IlDotNetBackendEmit

    type private BindingEmission =
        { Getter: MethodBuilder
          Compute: MethodBuilder option
          FunctionImpl: MethodBuilder option
          CacheField: FieldBuilder
          StateField: FieldBuilder }

    type private RuntimeLocal =
        | Local of LocalBuilder
        | ArgumentArrayIndex of int
        | CaptureArrayIndex of LocalBuilder * int

    type private EffectEmissionState =
        { Modules: Map<string, RawModuleInfo>
          DataTypes: Map<string * string, DataTypeInfo>
          ModuleBuilders: Map<string, TypeBuilder>
          DataTypeBuilders: Map<string * string, DataTypeEmission>
          BindingEmissions: Map<string, Map<string, BindingEmission>>
          mutable HelperCounter: int }

    let private objectType = typeof<obj>
    let private objectArrayType = typeof<obj[]>
    let private intType = typeof<int>
    let private stringType = typeof<string>
    let private boolType = typeof<bool>

    let private runtimeExceptionCtor =
        typeof<KappaRuntimeException>.GetConstructor([| stringType |])

    let private runtimeResolveIntrinsicMethod =
        typeof<KappaRuntime>.GetMethod("ResolveIntrinsic", [| stringType; stringType |])

    let private runtimeSupportsIntrinsicMethod =
        typeof<KappaRuntime>.GetMethod("SupportsIntrinsic", [| stringType; stringType |])

    let private runtimeApplyMethod =
        typeof<KappaRuntime>.GetMethod("Apply", [| objectType; objectArrayType |])

    let private runtimeUnaryMethod =
        typeof<KappaRuntime>.GetMethod("ApplyUnary", [| stringType; objectType |])

    let private runtimeBinaryMethod =
        typeof<KappaRuntime>.GetMethod("ApplyBinary", [| stringType; objectType; objectType |])

    let private runtimeExpectBoolMethod =
        typeof<KappaRuntime>.GetMethod("ExpectBool", [| objectType |])

    let private runtimeExpectUnitMethod =
        typeof<KappaRuntime>.GetMethod("ExpectUnit", [| objectType |])

    let private runtimeExpectObjectArrayMethod =
        typeof<KappaRuntime>.GetMethod("ExpectObjectArray", [| objectType; stringType |])

    let private runtimeExecuteMethod =
        typeof<KappaRuntime>.GetMethod("Execute", [| objectType |])

    let private runtimeHandleMethod =
        typeof<KappaRuntime>.GetMethod("Handle", [| boolType; objectType; objectType; objectType; objectType |])

    let private runtimeUnitMethod =
        typeof<KappaRuntime>.GetMethod("Unit", Type.EmptyTypes)

    let private runtimeCreateEffectLabelMethod =
        typeof<KappaRuntime>.GetMethod("CreateEffectLabel", [| stringType; stringType; stringType; typeof<KappaEffectOperationMetadata[]> |])

    let private runtimeCreateEffectOperationMethod =
        typeof<KappaRuntime>.GetMethod("CreateEffectOperation", [| objectType; stringType; stringType |])

    let private runtimeValuesEqualMethod =
        typeof<KappaRuntime>.GetMethod("ValuesEqual", [| objectType; objectType |])

    let private effectOperationMetadataCtor =
        typeof<KappaEffectOperationMetadata>.GetConstructor([| stringType; stringType; intType; boolType |])

    let private closureCtor =
        typeof<KappaClosure>.GetConstructor([| intType; objectType; typeof<KappaInvoke> |])

    let private constructorCallableCtor =
        typeof<KappaConstructorCallable>.GetConstructor([| stringType; intType; typeof<Func<obj array, obj>> |])

    let private invokeDelegateCtor =
        typeof<KappaInvoke>.GetConstructor([| objectType; typeof<IntPtr> |])

    let private constructorDelegateCtor =
        typeof<Func<obj array, obj>>.GetConstructor([| objectType; typeof<IntPtr> |])

    let private invalidOperationCtor =
        typeof<InvalidOperationException>.GetConstructor([| stringType |])

    let private stringEqualityMethod =
        typeof<string>.GetMethod("op_Equality", BindingFlags.Public ||| BindingFlags.Static, null, [| stringType; stringType |], null)

    let private aggregateDiagnostics diagnostics =
        diagnostics
        |> List.map (fun diagnostic -> diagnostic.Message)
        |> String.concat Environment.NewLine

    let private freshHelperName (state: EffectEmissionState) prefix =
        let current = state.HelperCounter
        state.HelperCounter <- current + 1
        $"__kappa_{prefix}_{current}"

    let private effectResumptionAllowsMultiple quantity =
        match quantity |> Option.defaultValue QuantityOne |> ResourceModel.ResourceQuantity.ofSurface with
        | ResourceModel.ResourceQuantity.Interval(_, Some maximum) ->
            maximum > 1
        | ResourceModel.ResourceQuantity.Interval(_, None) ->
            true
        | ResourceModel.ResourceQuantity.Borrow _
        | ResourceModel.ResourceQuantity.Variable _ ->
            false

    let private loadSmallInt (il: ILGenerator) value =
        match value with
        | -1 -> il.Emit(OpCodes.Ldc_I4_M1)
        | 0 -> il.Emit(OpCodes.Ldc_I4_0)
        | 1 -> il.Emit(OpCodes.Ldc_I4_1)
        | 2 -> il.Emit(OpCodes.Ldc_I4_2)
        | 3 -> il.Emit(OpCodes.Ldc_I4_3)
        | 4 -> il.Emit(OpCodes.Ldc_I4_4)
        | 5 -> il.Emit(OpCodes.Ldc_I4_5)
        | 6 -> il.Emit(OpCodes.Ldc_I4_6)
        | 7 -> il.Emit(OpCodes.Ldc_I4_7)
        | 8 -> il.Emit(OpCodes.Ldc_I4_8)
        | value -> il.Emit(OpCodes.Ldc_I4, value)

    let private loadRuntimeLocal (il: ILGenerator) runtimeLocal =
        match runtimeLocal with
        | Local localBuilder ->
            il.Emit(OpCodes.Ldloc, localBuilder)
        | ArgumentArrayIndex index ->
            il.Emit(OpCodes.Ldarg_1)
            loadSmallInt il index
            il.Emit(OpCodes.Ldelem_Ref)
        | CaptureArrayIndex(envLocal, index) ->
            il.Emit(OpCodes.Ldloc, envLocal)
            loadSmallInt il index
            il.Emit(OpCodes.Ldelem_Ref)

    let private emitRuntimeError (il: ILGenerator) (message: string) =
        il.Emit(OpCodes.Ldstr, message)
        il.Emit(OpCodes.Newobj, runtimeExceptionCtor)
        il.Emit(OpCodes.Throw)

    let private emitUnitValue (il: ILGenerator) =
        il.Emit(OpCodes.Call, runtimeUnitMethod)

    let private emitBoxedLiteral (il: ILGenerator) literal =
        match literal with
        | LiteralValue.Integer value ->
            il.Emit(OpCodes.Ldc_I8, value)
            il.Emit(OpCodes.Box, typeof<int64>)
        | LiteralValue.Float value ->
            il.Emit(OpCodes.Ldc_R8, value)
            il.Emit(OpCodes.Box, typeof<double>)
        | LiteralValue.String value ->
            il.Emit(OpCodes.Ldstr, value)
        | LiteralValue.Character value ->
            il.Emit(OpCodes.Ldstr, value)
        | LiteralValue.Grapheme value ->
            il.Emit(OpCodes.Ldstr, value)
        | LiteralValue.Byte value ->
            il.Emit(OpCodes.Ldc_I4, int value)
            il.Emit(OpCodes.Box, typeof<int>)
        | LiteralValue.Unit ->
            emitUnitValue il

    let private createClrResolutionState (state: EffectEmissionState) =
        { Environment =
            { Modules = Map.empty
              DataTypes = state.DataTypes
              TraitInstances = [] }
          ModuleBuilders = Map.empty
          MethodBuilders = Map.empty
          DataTypeBuilders = state.DataTypeBuilders }

    let private resolveNonGenericConstructor (state: EffectEmissionState) (constructorInfo: ConstructorInfo) =
        if not (List.isEmpty constructorInfo.TypeParameters) then
            Result.Error
                $"The effectful dotnet backend does not yet support generic constructor '{constructorInfo.ModuleName}.{constructorInfo.Name}'."
        else
            let dataEmission = state.DataTypeBuilders[constructorInfo.ModuleName, constructorInfo.TypeName]
            let constructorEmission = dataEmission.Constructors[constructorInfo.Name]
            let fieldInfos = constructorEmission.FieldBuilders |> Array.map (fun fieldBuilder -> fieldBuilder :> FieldInfo)
            Result.Ok(constructorEmission.ConstructorBuilder :> System.Reflection.ConstructorInfo, fieldInfos)

    let private resolveBindingGetter (state: EffectEmissionState) currentModule segments =
        tryResolveBinding state.Modules currentModule segments
        |> Option.bind (fun (moduleName, binding) ->
            state.BindingEmissions
            |> Map.tryFind moduleName
            |> Option.bind (fun bindings -> bindings |> Map.tryFind binding.Name)
            |> Option.map (fun emission -> moduleName, binding, emission.Getter))

    let private resolveIntrinsic segments =
        match segments with
        | [] ->
            None
        | [ name ] when KappaRuntime.SupportsIntrinsic(Stdlib.PreludeModuleText, name) ->
            Some(Stdlib.PreludeModuleText, name)
        | _ ->
            let moduleName = segments |> List.take (segments.Length - 1) |> String.concat "."
            let memberName = List.last segments

            if KappaRuntime.SupportsIntrinsic(moduleName, memberName) then
                Some(moduleName, memberName)
            else
                None

    let rec private collectClosureCaptures (locals: Set<string>) (bound: Set<string>) expression =
        match expression with
        | KRuntimeLiteral _
        | KRuntimeEffectLabel _
        | KRuntimeDictionaryValue _ ->
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
                    |> List.fold (fun state name -> Set.add name state) bound
                    |> fun state ->
                        clause.ResumptionName
                        |> Option.map (fun name -> Set.add name state)
                        |> Option.defaultValue state

                collectClosureCaptures locals clauseBound clause.Body

            collectClosureCaptures locals bound label
            |> Set.union (collectClosureCaptures locals bound body)
            |> Set.union (clauseCaptures returnClause)
            |> fun current ->
                operationClauses
                |> List.fold (fun state clause -> Set.union state (clauseCaptures clause)) current
        | KRuntimeMatch(scrutinee, cases) ->
            let collectPatternBindings pattern =
                let rec loop innerPattern =
                    match innerPattern with
                    | KRuntimeWildcardPattern
                    | KRuntimeLiteralPattern _ ->
                        Set.empty
                    | KRuntimeNamePattern name ->
                        Set.singleton name
                    | KRuntimeOrPattern alternatives ->
                        alternatives
                        |> List.tryHead
                        |> Option.map loop
                        |> Option.defaultValue Set.empty
                    | KRuntimeConstructorPattern(_, arguments) ->
                        arguments |> List.fold (fun state argument -> Set.union state (loop argument)) Set.empty

                loop pattern

            cases
            |> List.fold
                (fun state caseClause ->
                    let caseBound = Set.union bound (collectPatternBindings caseClause.Pattern)
                    let guardCaptures =
                        caseClause.Guard
                        |> Option.map (collectClosureCaptures locals caseBound)
                        |> Option.defaultValue Set.empty

                    state
                    |> Set.union (collectClosureCaptures locals caseBound caseClause.Body)
                    |> Set.union guardCaptures)
                (collectClosureCaptures locals bound scrutinee)
        | KRuntimeExecute expression
        | KRuntimeDoScope(_, expression)
        | KRuntimeUnary(_, expression) ->
            collectClosureCaptures locals bound expression
        | KRuntimeLet(bindingName, value, body) ->
            collectClosureCaptures locals bound value
            |> Set.union (collectClosureCaptures locals (Set.add bindingName bound) body)
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
        | KRuntimeWhile(condition, body)
        | KRuntimeBinary(condition, _, body) ->
            collectClosureCaptures locals bound condition
            |> Set.union (collectClosureCaptures locals bound body)
        | KRuntimeApply(callee, arguments)
        | KRuntimeTraitCall(_, _, callee, arguments) ->
            arguments
            |> List.fold
                (fun state argument -> Set.union state (collectClosureCaptures locals bound argument))
                (collectClosureCaptures locals bound callee)
        | KRuntimePrefixedString(_, parts) ->
            parts
            |> List.fold
                (fun state part ->
                    match part with
                    | KRuntimeStringText _ -> state
                    | KRuntimeStringInterpolation(inner, _) -> Set.union state (collectClosureCaptures locals bound inner))
                Set.empty

    let private cleanupExpressionForExitAction action =
        match action with
        | KRuntimeDeferred expression ->
            KRuntimeExecute expression
        | KRuntimeRelease(_, KRuntimeTraitCall(traitName, memberName, dictionary, []), resource) ->
            KRuntimeExecute(KRuntimeTraitCall(traitName, memberName, dictionary, [ resource ]))
        | KRuntimeRelease(_, release, resource) ->
            KRuntimeExecute(KRuntimeApply(release, [ resource ]))

    let emitClrAssemblyArtifact (modules: ClrAssemblyModule list) (outputDirectory: string) =
        let result = ResultBuilder()

        let buildState () =
            result {
                let rawModules, rawDataTypes = buildRawModuleSkeletons modules
                let! resolvedDataTypes = resolveDataTypes rawModules rawDataTypes
                let resolvedModules = attachResolvedDataTypes rawModules resolvedDataTypes

                let resolvedOutputDirectory = Path.GetFullPath(outputDirectory)
                Directory.CreateDirectory(resolvedOutputDirectory) |> ignore

                let assemblyName =
                    "Kappa.Generated."
                    + sanitizeIdentifier(Path.GetFileName(resolvedOutputDirectory))

                let assemblyPath = Path.Combine(resolvedOutputDirectory, $"{assemblyName}.dll")
                let assemblyBuilder = PersistedAssemblyBuilder(AssemblyName(assemblyName), typeof<obj>.Assembly)
                let moduleBuilder = assemblyBuilder.DefineDynamicModule(assemblyName)

                let moduleBuilders =
                    resolvedModules
                    |> Map.toList
                    |> List.map (fun (moduleName, moduleInfo) ->
                        let moduleTypeBuilder =
                            moduleBuilder.DefineType(
                                moduleInfo.EmittedTypeName.Value,
                                TypeAttributes.Public ||| TypeAttributes.Abstract ||| TypeAttributes.Sealed ||| TypeAttributes.Class
                            )

                        moduleName, moduleTypeBuilder)
                    |> Map.ofList

                let dataTypes =
                    resolvedDataTypes
                    |> Map.toList
                    |> List.collect (fun (moduleName, moduleDataTypes) ->
                        moduleDataTypes |> Map.toList |> List.map (fun (typeName, dataTypeInfo) -> (moduleName, typeName), dataTypeInfo))
                    |> Map.ofList

                let environment =
                    { Modules = Map.empty
                      DataTypes = dataTypes
                      TraitInstances = [] }

                let dataTypeBuilders = defineDataTypes moduleBuilder environment

                let bindingEmissions =
                    resolvedModules
                    |> Map.toList
                    |> List.map (fun (moduleName, moduleInfo) ->
                        let bindings =
                            moduleInfo.Bindings
                            |> Map.toList
                            |> List.map (fun (bindingName, binding) ->
                                let sanitizedName = emittedMethodName bindingName
                                let getter =
                                    moduleBuilders[moduleName].DefineMethod(
                                        sanitizedName,
                                        MethodAttributes.Public ||| MethodAttributes.Static ||| MethodAttributes.HideBySig,
                                        objectType,
                                        Type.EmptyTypes
                                    )

                                let cacheField =
                                    moduleBuilders[moduleName].DefineField(
                                        $"__kappa_cache_{sanitizedName}",
                                        objectType,
                                        FieldAttributes.Private ||| FieldAttributes.Static
                                    )

                                let stateField =
                                    moduleBuilders[moduleName].DefineField(
                                        $"__kappa_state_{sanitizedName}",
                                        intType,
                                        FieldAttributes.Private ||| FieldAttributes.Static
                                    )

                                let compute, functionImpl =
                                    match binding.Body with
                                    | Some _ when List.isEmpty binding.Parameters ->
                                        let computeMethod =
                                            moduleBuilders[moduleName].DefineMethod(
                                                $"__kappa_compute_{sanitizedName}",
                                                MethodAttributes.Private ||| MethodAttributes.Static ||| MethodAttributes.HideBySig,
                                                objectType,
                                                Type.EmptyTypes
                                            )

                                        Some computeMethod, None
                                    | Some _ ->
                                        let implMethod =
                                            moduleBuilders[moduleName].DefineMethod(
                                                $"__kappa_impl_{sanitizedName}",
                                                MethodAttributes.Private ||| MethodAttributes.Static ||| MethodAttributes.HideBySig,
                                                objectType,
                                                [| objectType; objectArrayType |]
                                            )

                                        None, Some implMethod
                                    | None ->
                                        None, None

                                bindingName,
                                { Getter = getter
                                  Compute = compute
                                  FunctionImpl = functionImpl
                                  CacheField = cacheField
                                  StateField = stateField })
                            |> Map.ofList

                        moduleName, bindings)
                    |> Map.ofList

                return
                    resolvedModules,
                    { Modules = resolvedModules
                      DataTypes = dataTypes
                      ModuleBuilders = moduleBuilders
                      DataTypeBuilders = dataTypeBuilders
                      BindingEmissions = bindingEmissions
                      HelperCounter = 0 },
                    assemblyBuilder,
                    assemblyPath
            }

        match buildState () with
        | Result.Error message ->
            Result.Error message
        | Result.Ok(resolvedModules, state, assemblyBuilder, assemblyPath) ->
            let clrResolverState = createClrResolutionState state

            let rec emitExpression
                (currentModule: string)
                (locals: Map<string, RuntimeLocal>)
                (il: ILGenerator)
                (expression: KRuntimeExpression)
                : Result<unit, string> =
                let localNames = locals |> Map.keys |> Set.ofSeq

                let emitObjectArrayFromExpressions expressions =
                    let argumentExpressions = expressions |> List.toArray
                    loadSmallInt il argumentExpressions.Length
                    il.Emit(OpCodes.Newarr, objectType)

                    argumentExpressions
                    |> Array.iteri (fun index argumentExpression ->
                        il.Emit(OpCodes.Dup)
                        loadSmallInt il index

                        match emitExpression currentModule locals il argumentExpression with
                        | Result.Ok() ->
                            ()
                        | Result.Error message ->
                            raise (InvalidOperationException message)

                        il.Emit(OpCodes.Stelem_Ref))

                    Result.Ok()

                let emitIntrinsicValue (moduleName: string) (intrinsicName: string) =
                    il.Emit(OpCodes.Ldstr, moduleName)
                    il.Emit(OpCodes.Ldstr, intrinsicName)
                    il.Emit(OpCodes.Call, runtimeResolveIntrinsicMethod)
                    Result.Ok()

                let emitClosureHelper
                    (captures: string list)
                    (parameters: string list)
                    (body: KRuntimeExpression)
                    =
                    let helperName = freshHelperName state "closure"
                    let helperMethod =
                        state.ModuleBuilders[currentModule].DefineMethod(
                            helperName,
                            MethodAttributes.Private ||| MethodAttributes.Static ||| MethodAttributes.HideBySig,
                            objectType,
                            [| objectType; objectArrayType |]
                        )

                    let helperIl = helperMethod.GetILGenerator()

                    let captureEnvLocal =
                        if List.isEmpty captures then
                            None
                        else
                            let envLocal = helperIl.DeclareLocal(objectArrayType)
                            helperIl.Emit(OpCodes.Ldarg_0)
                            helperIl.Emit(OpCodes.Castclass, objectArrayType)
                            helperIl.Emit(OpCodes.Stloc, envLocal)
                            Some envLocal

                    let captureLocals =
                        captures
                        |> List.mapi (fun index captureName ->
                            let slot =
                                match captureEnvLocal with
                                | Some envLocal -> CaptureArrayIndex(envLocal, index)
                                | None -> failwith "capture environment missing"

                            captureName, slot)
                        |> Map.ofList

                    let parameterLocals =
                        parameters
                        |> List.mapi (fun index parameterName -> parameterName, ArgumentArrayIndex index)
                        |> Map.ofList

                    let helperLocals =
                        (captureLocals, parameterLocals)
                        ||> Map.fold (fun state parameterName slot -> Map.add parameterName slot state)

                    match emitExpression currentModule helperLocals helperIl body with
                    | Result.Ok() ->
                        helperIl.Emit(OpCodes.Ret)
                        Result.Ok helperMethod
                    | Result.Error message ->
                        Result.Error message

                let emitClosureValue (captures: string list) (parameters: string list) (body: KRuntimeExpression) =
                    result {
                        let! helperMethod = emitClosureHelper captures parameters body

                        loadSmallInt il parameters.Length

                        if List.isEmpty captures then
                            il.Emit(OpCodes.Ldnull)
                        else
                            loadSmallInt il captures.Length
                            il.Emit(OpCodes.Newarr, objectType)

                            captures
                            |> List.iteri (fun index captureName ->
                                il.Emit(OpCodes.Dup)
                                loadSmallInt il index
                                loadRuntimeLocal il locals[captureName]
                                il.Emit(OpCodes.Stelem_Ref))

                        il.Emit(OpCodes.Ldnull)
                        il.Emit(OpCodes.Ldftn, helperMethod)
                        il.Emit(OpCodes.Newobj, invokeDelegateCtor)
                        il.Emit(OpCodes.Newobj, closureCtor)
                    }

                let emitRecursiveClosureLet bindingName parameters closureBody body =
                    result {
                        let captureNames =
                            collectClosureCaptures (Set.add bindingName localNames) Set.empty closureBody
                            |> Set.toList
                            |> List.sort

                        let! helperMethod = emitClosureHelper captureNames parameters closureBody

                        let environmentLocal =
                            if List.isEmpty captureNames then
                                None
                            else
                                let envLocal = il.DeclareLocal(objectArrayType)
                                loadSmallInt il captureNames.Length
                                il.Emit(OpCodes.Newarr, objectType)
                                il.Emit(OpCodes.Stloc, envLocal)

                                captureNames
                                |> List.iteri (fun index captureName ->
                                    if not (String.Equals(captureName, bindingName, StringComparison.Ordinal)) then
                                        il.Emit(OpCodes.Ldloc, envLocal)
                                        loadSmallInt il index
                                        loadRuntimeLocal il locals[captureName]
                                        il.Emit(OpCodes.Stelem_Ref))

                                Some envLocal

                        let closureLocal = il.DeclareLocal(objectType)

                        loadSmallInt il parameters.Length

                        match environmentLocal with
                        | Some envLocal ->
                            il.Emit(OpCodes.Ldloc, envLocal)
                        | None ->
                            il.Emit(OpCodes.Ldnull)

                        il.Emit(OpCodes.Ldnull)
                        il.Emit(OpCodes.Ldftn, helperMethod)
                        il.Emit(OpCodes.Newobj, invokeDelegateCtor)
                        il.Emit(OpCodes.Newobj, closureCtor)
                        il.Emit(OpCodes.Stloc, closureLocal)

                        match environmentLocal with
                        | Some envLocal ->
                            match captureNames |> List.tryFindIndex (fun captureName -> String.Equals(captureName, bindingName, StringComparison.Ordinal)) with
                            | Some selfIndex ->
                                il.Emit(OpCodes.Ldloc, envLocal)
                                loadSmallInt il selfIndex
                                il.Emit(OpCodes.Ldloc, closureLocal)
                                il.Emit(OpCodes.Stelem_Ref)
                            | None ->
                                ()
                        | None ->
                            ()

                        let bodyLocals = locals |> Map.add bindingName (Local closureLocal)
                        do! emitExpression currentModule bodyLocals il body
                    }

                match expression with
                | KRuntimeLiteral literal ->
                    emitBoxedLiteral il literal
                    Result.Ok()
                | KRuntimeName [ name ] when locals.ContainsKey name ->
                    loadRuntimeLocal il locals[name]
                    Result.Ok()
                | KRuntimeName segments ->
                    match resolveBindingGetter state currentModule segments with
                    | Some(_, _, getterMethod) ->
                        il.Emit(OpCodes.Call, getterMethod)
                        Result.Ok()
                    | None ->
                        match resolveIntrinsic segments with
                        | Some(moduleName, intrinsicName) ->
                            emitIntrinsicValue moduleName intrinsicName
                        | None ->
                            match tryResolveConstructor state.Modules currentModule segments with
                            | Some(_, constructorInfo) when List.isEmpty constructorInfo.FieldTypes ->
                                result {
                                    let! constructor, _ = resolveNonGenericConstructor state constructorInfo
                                    il.Emit(OpCodes.Newobj, constructor)
                                    if constructor.DeclaringType.IsValueType then
                                        il.Emit(OpCodes.Box, constructor.DeclaringType)
                                }
                            | Some(targetModule, constructorInfo) ->
                                Result.Error
                                    $"The effectful dotnet backend does not yet support constructor-valued name '{targetModule}.{constructorInfo.Name}'."
                            | None ->
                                let text = String.concat "." segments
                                Result.Error $"The effectful dotnet backend could not resolve name '{text}'."
                | KRuntimeEffectLabel(labelName, interfaceId, labelId, operations) ->
                    let operationsLocal = il.DeclareLocal(typeof<KappaEffectOperationMetadata[]>)

                    loadSmallInt il operations.Length
                    il.Emit(OpCodes.Newarr, typeof<KappaEffectOperationMetadata>)

                    operations
                    |> List.iteri (fun index (operation: KRuntimeEffectOperation) ->
                        il.Emit(OpCodes.Dup)
                        loadSmallInt il index
                        il.Emit(OpCodes.Ldstr, operation.OperationId)
                        il.Emit(OpCodes.Ldstr, operation.Name)
                        loadSmallInt il operation.ParameterArity
                        il.Emit(OpCodes.Ldc_I4, if effectResumptionAllowsMultiple operation.ResumptionQuantity then 1 else 0)
                        il.Emit(OpCodes.Newobj, effectOperationMetadataCtor)
                        il.Emit(OpCodes.Stelem_Ref))

                    il.Emit(OpCodes.Stloc, operationsLocal)
                    il.Emit(OpCodes.Ldstr, labelName)
                    il.Emit(OpCodes.Ldstr, interfaceId)
                    il.Emit(OpCodes.Ldstr, labelId)
                    il.Emit(OpCodes.Ldloc, operationsLocal)
                    il.Emit(OpCodes.Call, runtimeCreateEffectLabelMethod)
                    Result.Ok()
                | KRuntimeEffectOperation(labelExpression, operationId, operationName) ->
                    result {
                        do! emitExpression currentModule locals il labelExpression
                        il.Emit(OpCodes.Ldstr, operationId)
                        il.Emit(OpCodes.Ldstr, operationName)
                        il.Emit(OpCodes.Call, runtimeCreateEffectOperationMethod)
                    }
                | KRuntimeClosure(parameters, body) ->
                    let captures =
                        collectClosureCaptures localNames Set.empty body
                        |> Set.toList
                        |> List.sort

                    emitClosureValue captures parameters body
                | KRuntimeIfThenElse(condition, whenTrue, whenFalse) ->
                    let falseLabel = il.DefineLabel()
                    let endLabel = il.DefineLabel()
                    result {
                        do! emitExpression currentModule locals il condition
                        il.Emit(OpCodes.Call, runtimeExpectBoolMethod)
                        il.Emit(OpCodes.Brfalse, falseLabel)
                        do! emitExpression currentModule locals il whenTrue
                        il.Emit(OpCodes.Br, endLabel)
                        il.MarkLabel(falseLabel)
                        do! emitExpression currentModule locals il whenFalse
                        il.MarkLabel(endLabel)
                    }
                | KRuntimeHandle(isDeep, labelExpression, bodyExpression, returnClause, operationClauses) ->
                    let returnParameterName =
                        match returnClause.Arguments with
                        | [ KRuntimeEffectNameArgument name ] ->
                            name
                        | _ ->
                            "__kappa_return_payload"

                    let returnClosureBody =
                        returnClause.Body

                    let operationCaptures =
                        operationClauses
                        |> List.fold
                            (fun state currentClause ->
                                let clauseBound =
                                    currentClause.Arguments
                                    |> List.choose (function
                                        | KRuntimeEffectNameArgument name -> Some name
                                        | _ -> None)
                                    |> List.fold (fun bound name -> Set.add name bound) Set.empty
                                    |> fun bound ->
                                        currentClause.ResumptionName
                                        |> Option.map (fun name -> Set.add name bound)
                                        |> Option.defaultValue bound

                                Set.union state (collectClosureCaptures localNames clauseBound currentClause.Body))
                            Set.empty
                        |> Set.toList
                        |> List.sort

                    let emitDispatcherClosure () =
                        let helperName = freshHelperName state "handler_dispatch"
                        let helperMethod =
                            state.ModuleBuilders[currentModule].DefineMethod(
                                helperName,
                                MethodAttributes.Private ||| MethodAttributes.Static ||| MethodAttributes.HideBySig,
                                objectType,
                                [| objectType; objectArrayType |]
                            )

                        let helperIl = helperMethod.GetILGenerator()

                        let captureEnvLocal =
                            if List.isEmpty operationCaptures then
                                None
                            else
                                let envLocal = helperIl.DeclareLocal(objectArrayType)
                                helperIl.Emit(OpCodes.Ldarg_0)
                                helperIl.Emit(OpCodes.Castclass, objectArrayType)
                                helperIl.Emit(OpCodes.Stloc, envLocal)
                                Some envLocal

                        let captureLocals =
                            operationCaptures
                            |> List.mapi (fun index captureName ->
                                let slot =
                                    match captureEnvLocal with
                                    | Some envLocal -> CaptureArrayIndex(envLocal, index)
                                    | None -> failwith "capture environment missing"

                                captureName, slot)
                            |> Map.ofList

                        let argsLocal = helperIl.DeclareLocal(objectArrayType)
                        helperIl.Emit(OpCodes.Ldarg_1)
                        helperIl.Emit(OpCodes.Stloc, argsLocal)

                        let opNameLocal = helperIl.DeclareLocal(stringType)
                        helperIl.Emit(OpCodes.Ldloc, argsLocal)
                        loadSmallInt helperIl 0
                        helperIl.Emit(OpCodes.Ldelem_Ref)
                        helperIl.Emit(OpCodes.Castclass, stringType)
                        helperIl.Emit(OpCodes.Stloc, opNameLocal)

                        let rawArgsLocal = helperIl.DeclareLocal(objectArrayType)
                        helperIl.Emit(OpCodes.Ldloc, argsLocal)
                        loadSmallInt helperIl 1
                        helperIl.Emit(OpCodes.Ldelem_Ref)
                        helperIl.Emit(OpCodes.Ldstr, "handler dispatcher")
                        helperIl.Emit(OpCodes.Call, runtimeExpectObjectArrayMethod)
                        helperIl.Emit(OpCodes.Stloc, rawArgsLocal)

                        let resumptionLocal = helperIl.DeclareLocal(objectType)
                        helperIl.Emit(OpCodes.Ldloc, argsLocal)
                        loadSmallInt helperIl 2
                        helperIl.Emit(OpCodes.Ldelem_Ref)
                        helperIl.Emit(OpCodes.Stloc, resumptionLocal)

                        let endLabel = helperIl.DefineLabel()

                        let rec emitClauses (remainingClauses: KRuntimeEffectHandlerClause list) =
                            match remainingClauses with
                            | [] ->
                                emitRuntimeError helperIl "Handler dispatcher could not find a matching operation clause."
                                Result.Ok()
                            | clause :: rest ->
                                let nextClause = helperIl.DefineLabel()

                                helperIl.Emit(OpCodes.Ldloc, opNameLocal)
                                helperIl.Emit(OpCodes.Ldstr, clause.OperationName)
                                helperIl.Emit(OpCodes.Call, stringEqualityMethod)
                                helperIl.Emit(OpCodes.Brfalse, nextClause)

                                helperIl.Emit(OpCodes.Ldloc, rawArgsLocal)
                                helperIl.Emit(OpCodes.Ldlen)
                                helperIl.Emit(OpCodes.Conv_I4)
                                loadSmallInt helperIl clause.Arguments.Length
                                let arityMatches = helperIl.DefineLabel()
                                helperIl.Emit(OpCodes.Beq, arityMatches)
                                emitRuntimeError helperIl $"Handler clause '{clause.OperationName}' expected {clause.Arguments.Length} argument(s)."
                                helperIl.MarkLabel(arityMatches)

                                let mutable clauseLocals = captureLocals

                                clause.Arguments
                                |> List.iteri (fun index (argument: KRuntimeEffectHandlerArgument) ->
                                    helperIl.Emit(OpCodes.Ldloc, rawArgsLocal)
                                    loadSmallInt helperIl index
                                    helperIl.Emit(OpCodes.Ldelem_Ref)

                                    match argument with
                                    | KRuntimeEffectNameArgument name ->
                                        let local = helperIl.DeclareLocal(objectType)
                                        helperIl.Emit(OpCodes.Stloc, local)
                                        clauseLocals <- clauseLocals |> Map.add name (Local local)
                                    | KRuntimeEffectWildcardArgument ->
                                        helperIl.Emit(OpCodes.Pop)
                                    | KRuntimeEffectUnitArgument ->
                                        helperIl.Emit(OpCodes.Call, runtimeExpectUnitMethod))

                                match clause.ResumptionName with
                                | Some resumptionName ->
                                    clauseLocals <- clauseLocals |> Map.add resumptionName (Local resumptionLocal)
                                | None ->
                                    ()

                                match emitExpression currentModule clauseLocals helperIl clause.Body with
                                | Result.Ok() ->
                                    helperIl.Emit(OpCodes.Br, endLabel)
                                    helperIl.MarkLabel(nextClause)
                                    emitClauses rest
                                | Result.Error message ->
                                    Result.Error message

                        match emitClauses operationClauses with
                        | Result.Ok() ->
                            helperIl.MarkLabel(endLabel)
                            helperIl.Emit(OpCodes.Ret)
                            Result.Ok helperMethod
                        | Result.Error message ->
                            Result.Error message

                    result {
                        let! returnHelper =
                            emitClosureHelper [] [ returnParameterName ] returnClosureBody

                        let! dispatcherHelper = emitDispatcherClosure ()

                        let returnLocal = il.DeclareLocal(objectType)
                        let dispatcherLocal = il.DeclareLocal(objectType)
                        let labelLocal = il.DeclareLocal(objectType)
                        let bodyLocal = il.DeclareLocal(objectType)

                        loadSmallInt il 1

                        if List.isEmpty operationCaptures then
                            il.Emit(OpCodes.Ldnull)
                        else
                            loadSmallInt il operationCaptures.Length
                            il.Emit(OpCodes.Newarr, objectType)

                            operationCaptures
                            |> List.iteri (fun index captureName ->
                                il.Emit(OpCodes.Dup)
                                loadSmallInt il index
                                loadRuntimeLocal il locals[captureName]
                                il.Emit(OpCodes.Stelem_Ref))

                        il.Emit(OpCodes.Ldnull)
                        il.Emit(OpCodes.Ldftn, returnHelper)
                        il.Emit(OpCodes.Newobj, invokeDelegateCtor)
                        il.Emit(OpCodes.Newobj, closureCtor)
                        il.Emit(OpCodes.Stloc, returnLocal)

                        loadSmallInt il 3

                        if List.isEmpty operationCaptures then
                            il.Emit(OpCodes.Ldnull)
                        else
                            loadSmallInt il operationCaptures.Length
                            il.Emit(OpCodes.Newarr, objectType)

                            operationCaptures
                            |> List.iteri (fun index captureName ->
                                il.Emit(OpCodes.Dup)
                                loadSmallInt il index
                                loadRuntimeLocal il locals[captureName]
                                il.Emit(OpCodes.Stelem_Ref))

                        il.Emit(OpCodes.Ldnull)
                        il.Emit(OpCodes.Ldftn, dispatcherHelper)
                        il.Emit(OpCodes.Newobj, invokeDelegateCtor)
                        il.Emit(OpCodes.Newobj, closureCtor)
                        il.Emit(OpCodes.Stloc, dispatcherLocal)

                        do! emitExpression currentModule locals il labelExpression
                        il.Emit(OpCodes.Stloc, labelLocal)
                        do! emitExpression currentModule locals il bodyExpression
                        il.Emit(OpCodes.Stloc, bodyLocal)

                        il.Emit(OpCodes.Ldc_I4, if isDeep then 1 else 0)
                        il.Emit(OpCodes.Ldloc, labelLocal)
                        il.Emit(OpCodes.Ldloc, bodyLocal)
                        il.Emit(OpCodes.Ldloc, returnLocal)
                        il.Emit(OpCodes.Ldloc, dispatcherLocal)
                        il.Emit(OpCodes.Call, runtimeHandleMethod)
                    }
                | KRuntimeMatch _ ->
                    Result.Error "The effectful dotnet backend does not yet support match expressions."
                | KRuntimeExecute inner ->
                    result {
                        do! emitExpression currentModule locals il inner
                        il.Emit(OpCodes.Call, runtimeExecuteMethod)
                    }
                | KRuntimeLet(bindingName, KRuntimeClosure(parameters, closureBody), body) ->
                    emitRecursiveClosureLet bindingName parameters closureBody body
                | KRuntimeLet(bindingName, value, body) ->
                    result {
                        let local = il.DeclareLocal(objectType)
                        do! emitExpression currentModule locals il value
                        il.Emit(OpCodes.Stloc, local)
                        let nextLocals = locals |> Map.add bindingName (Local local)
                        do! emitExpression currentModule nextLocals il body
                    }
                | KRuntimeDoScope(_, body) ->
                    emitExpression currentModule locals il body
                | KRuntimeScheduleExit(_, action, body) ->
                    result {
                        let cleanupExpression = cleanupExpressionForExitAction action
                        let resultLocal = il.DeclareLocal(objectType)

                        il.BeginExceptionBlock() |> ignore
                        do! emitExpression currentModule locals il body
                        il.Emit(OpCodes.Stloc, resultLocal)
                        il.BeginFinallyBlock()
                        do! emitExpression currentModule locals il cleanupExpression
                        il.Emit(OpCodes.Pop)
                        il.EndExceptionBlock()
                        il.Emit(OpCodes.Ldloc, resultLocal)
                    }
                | KRuntimeSequence(first, second) ->
                    result {
                        do! emitExpression currentModule locals il first
                        il.Emit(OpCodes.Pop)
                        do! emitExpression currentModule locals il second
                    }
                | KRuntimeWhile(condition, body) ->
                    let loopLabel = il.DefineLabel()
                    let endLabel = il.DefineLabel()
                    result {
                        il.MarkLabel(loopLabel)
                        do! emitExpression currentModule locals il condition
                        il.Emit(OpCodes.Call, runtimeExpectBoolMethod)
                        il.Emit(OpCodes.Brfalse, endLabel)
                        do! emitExpression currentModule locals il body
                        il.Emit(OpCodes.Pop)
                        il.Emit(OpCodes.Br, loopLabel)
                        il.MarkLabel(endLabel)
                        emitUnitValue il
                    }
                | KRuntimeApply(KRuntimeName segments, arguments) ->
                    match tryResolveConstructor state.Modules currentModule segments with
                    | Some(_, constructorInfo) ->
                        result {
                            let! constructor, fieldInfos = resolveNonGenericConstructor state constructorInfo

                            if List.length arguments <> fieldInfos.Length then
                                return!
                                    Result.Error
                                        $"The effectful dotnet backend expected constructor '{constructorInfo.Name}' to receive {fieldInfos.Length} argument(s), but received {List.length arguments}."

                            let fieldTypes =
                                if List.isEmpty constructorInfo.TypeParameters then
                                    constructorInfo.FieldTypes
                                else
                                    []

                            do!
                                List.zip arguments fieldTypes
                                |> List.fold
                                    (fun stateResult (argumentExpression, fieldType) ->
                                        result {
                                            do! stateResult
                                            do! emitExpression currentModule locals il argumentExpression
                                            let clrFieldType = resolveClrType clrResolverState Map.empty fieldType

                                            if clrFieldType.IsValueType then
                                                il.Emit(OpCodes.Unbox_Any, clrFieldType)
                                            else
                                                il.Emit(OpCodes.Castclass, clrFieldType)
                                        })
                                    (Result.Ok())

                            il.Emit(OpCodes.Newobj, constructor)
                            if constructor.DeclaringType.IsValueType then
                                il.Emit(OpCodes.Box, constructor.DeclaringType)
                        }
                    | None ->
                        result {
                            do! emitExpression currentModule locals il (KRuntimeName segments)
                            do! emitObjectArrayFromExpressions arguments
                            il.Emit(OpCodes.Call, runtimeApplyMethod)
                        }
                | KRuntimeApply(callee, arguments) ->
                    result {
                        do! emitExpression currentModule locals il callee
                        do! emitObjectArrayFromExpressions arguments
                        il.Emit(OpCodes.Call, runtimeApplyMethod)
                    }
                | KRuntimeDictionaryValue _
                | KRuntimeTraitCall _ ->
                    Result.Error "The effectful dotnet backend does not yet support trait dictionary calls."
                | KRuntimeUnary(operatorName, operand) ->
                    result {
                        il.Emit(OpCodes.Ldstr, operatorName)
                        do! emitExpression currentModule locals il operand
                        il.Emit(OpCodes.Call, runtimeUnaryMethod)
                    }
                | KRuntimeBinary(left, "&&", right) ->
                    let falseLabel = il.DefineLabel()
                    let endLabel = il.DefineLabel()
                    result {
                        do! emitExpression currentModule locals il left
                        il.Emit(OpCodes.Call, runtimeExpectBoolMethod)
                        il.Emit(OpCodes.Brfalse, falseLabel)
                        do! emitExpression currentModule locals il right
                        il.Emit(OpCodes.Call, runtimeExpectBoolMethod)
                        il.Emit(OpCodes.Box, boolType)
                        il.Emit(OpCodes.Br, endLabel)
                        il.MarkLabel(falseLabel)
                        il.Emit(OpCodes.Ldc_I4_0)
                        il.Emit(OpCodes.Box, boolType)
                        il.MarkLabel(endLabel)
                    }
                | KRuntimeBinary(left, "||", right) ->
                    let trueLabel = il.DefineLabel()
                    let endLabel = il.DefineLabel()
                    result {
                        do! emitExpression currentModule locals il left
                        il.Emit(OpCodes.Call, runtimeExpectBoolMethod)
                        il.Emit(OpCodes.Brtrue, trueLabel)
                        do! emitExpression currentModule locals il right
                        il.Emit(OpCodes.Call, runtimeExpectBoolMethod)
                        il.Emit(OpCodes.Box, boolType)
                        il.Emit(OpCodes.Br, endLabel)
                        il.MarkLabel(trueLabel)
                        il.Emit(OpCodes.Ldc_I4_1)
                        il.Emit(OpCodes.Box, boolType)
                        il.MarkLabel(endLabel)
                    }
                | KRuntimeBinary(left, operatorName, right) ->
                    result {
                        il.Emit(OpCodes.Ldstr, operatorName)
                        do! emitExpression currentModule locals il left
                        do! emitExpression currentModule locals il right
                        il.Emit(OpCodes.Call, runtimeBinaryMethod)
                    }
                | KRuntimePrefixedString _ ->
                    Result.Error "The effectful dotnet backend does not yet support prefixed strings."

            let emitBindingBodies () =
                let result = ResultBuilder()

                let emitGetter moduleName (binding: ClrAssemblyBinding) (emission: BindingEmission) =
                    result {
                        let getterIl = emission.Getter.GetILGenerator()
                        let initializedLabel = getterIl.DefineLabel()
                        let computingLabel = getterIl.DefineLabel()

                        getterIl.Emit(OpCodes.Ldsfld, emission.StateField)
                        loadSmallInt getterIl 2
                        getterIl.Emit(OpCodes.Beq, initializedLabel)

                        getterIl.Emit(OpCodes.Ldsfld, emission.StateField)
                        loadSmallInt getterIl 1
                        getterIl.Emit(OpCodes.Beq, computingLabel)

                        loadSmallInt getterIl 1
                        getterIl.Emit(OpCodes.Stsfld, emission.StateField)

                        match binding.Body, binding.Parameters, emission.Compute, emission.FunctionImpl with
                        | Some _, [], Some computeMethod, _ ->
                            getterIl.Emit(OpCodes.Call, computeMethod)
                        | Some _, parameters, _, Some functionImpl ->
                            loadSmallInt getterIl parameters.Length
                            getterIl.Emit(OpCodes.Ldnull)
                            getterIl.Emit(OpCodes.Ldnull)
                            getterIl.Emit(OpCodes.Ldftn, functionImpl)
                            getterIl.Emit(OpCodes.Newobj, invokeDelegateCtor)
                            getterIl.Emit(OpCodes.Newobj, closureCtor)
                        | None, _, _, _ ->
                            return!
                                Result.Error
                                    $"The effectful dotnet backend requires a body for '{moduleName}.{binding.Name}'."
                        | _ ->
                            return!
                                Result.Error
                                    $"The effectful dotnet backend could not prepare binding '{moduleName}.{binding.Name}'."

                        getterIl.Emit(OpCodes.Stsfld, emission.CacheField)
                        loadSmallInt getterIl 2
                        getterIl.Emit(OpCodes.Stsfld, emission.StateField)
                        getterIl.MarkLabel(initializedLabel)
                        getterIl.Emit(OpCodes.Ldsfld, emission.CacheField)
                        getterIl.Emit(OpCodes.Ret)

                        getterIl.MarkLabel(computingLabel)
                        emitRuntimeError getterIl $"Recursive evaluation of '{moduleName}.{binding.Name}' is not supported for non-function values."

                        match emission.Compute, emission.FunctionImpl with
                        | Some computeMethod, _ ->
                            let computeIl = computeMethod.GetILGenerator()
                            do! emitExpression moduleName Map.empty computeIl binding.Body.Value
                            computeIl.Emit(OpCodes.Ret)
                        | _, Some functionImpl ->
                            let functionIl = functionImpl.GetILGenerator()
                            let parameterLocals =
                                binding.Parameters
                                |> List.mapi (fun index parameter -> parameter.Name, ArgumentArrayIndex index)
                                |> Map.ofList
                            do! emitExpression moduleName parameterLocals functionIl binding.Body.Value
                            functionIl.Emit(OpCodes.Ret)
                        | _ ->
                            ()
                    }

                resolvedModules
                |> Map.toList
                |> List.fold
                    (fun stateResult (moduleName, moduleInfo) ->
                        result {
                            do! stateResult

                            do!
                                moduleInfo.Bindings
                                |> Seq.toList
                                |> List.fold
                                    (fun bindingState (KeyValue(bindingName, binding)) ->
                                        result {
                                            do! bindingState
                                            let emission = state.BindingEmissions[moduleName][bindingName]
                                            do! emitGetter moduleName binding emission
                                        })
                                    (Result.Ok())
                        })
                    (Result.Ok())

            match emitBindingBodies () with
            | Result.Error message ->
                Result.Error message
            | Result.Ok() ->
                try
                    state.DataTypeBuilders
                    |> Map.iter (fun _ dataEmission -> dataEmission.BaseTypeBuilder.CreateType() |> ignore)

                    state.DataTypeBuilders
                    |> Map.iter (fun _ dataEmission ->
                        dataEmission.Constructors
                        |> Map.iter (fun _ constructorEmission -> constructorEmission.TypeBuilder.CreateType() |> ignore))

                    state.ModuleBuilders |> Map.iter (fun _ moduleTypeBuilder -> moduleTypeBuilder.CreateType() |> ignore)
                    assemblyBuilder.Save(assemblyPath)

                    Result.Ok
                        { OutputDirectory = Path.GetDirectoryName(assemblyPath)
                          AssemblyName = Path.GetFileNameWithoutExtension(assemblyPath)
                          AssemblyFilePath = assemblyPath }
                with ex ->
                    Result.Error $"The effectful dotnet backend failed to emit IL: {ex.Message}"

    let emitAssemblyArtifact (workspace: WorkspaceCompilation) (outputDirectory: string) =
        if workspace.HasErrors then
            Result.Error $"Cannot emit an effectful CLR assembly for a workspace with diagnostics:{Environment.NewLine}{aggregateDiagnostics workspace.Diagnostics}"
        else
            let verificationDiagnostics = CheckpointVerification.verifyCheckpoint workspace "KBackendIR"

            if not (List.isEmpty verificationDiagnostics) then
                Result.Error $"Cannot emit malformed KBackendIR:{Environment.NewLine}{aggregateDiagnostics verificationDiagnostics}"
            else
                emitClrAssemblyArtifact workspace.ClrAssemblyIR outputDirectory
