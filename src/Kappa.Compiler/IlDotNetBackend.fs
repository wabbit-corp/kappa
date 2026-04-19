namespace Kappa.Compiler

open System
open System.Collections.Generic
open System.IO
open System.Reflection
open System.Reflection.Emit

module IlDotNetBackend =
    type private IlValueType =
        | IlInt64
        | IlFloat64
        | IlBool
        | IlString
        | IlChar

    type private BindingInfo =
        { Binding: KBackendBinding
          ReturnType: IlValueType
          EmittedMethodName: string }

    type private ModuleInfo =
        { Name: string
          Bindings: Map<string, BindingInfo>
          EmittedTypeName: string }

    type private EmissionEnvironment =
        { Modules: Map<string, ModuleInfo> }

    type private EmissionState =
        { Environment: EmissionEnvironment
          TypeBuilders: Map<string, TypeBuilder>
          MethodBuilders: Map<string, Map<string, MethodBuilder>> }

    type private ResultBuilder() =
        member _.Bind(result, binder) = Result.bind binder result
        member _.Return(value) = Result.Ok value
        member _.ReturnFrom(result) = result
        member _.Zero() = Result.Ok()
        member _.Delay(generator) = generator
        member _.Run(generator) = generator()
        member _.Combine(first, second) =
            first |> Result.bind (fun () -> second())

    let private result = ResultBuilder()

    let private aggregateDiagnostics diagnostics =
        diagnostics
        |> List.map (fun diagnostic -> diagnostic.Message)
        |> String.concat Environment.NewLine

    let private sanitizeIdentifier (value: string) =
        let text =
            value
            |> Seq.collect (fun ch ->
                if Char.IsLetterOrDigit(ch) || ch = '_' then
                    Seq.singleton(string ch)
                else
                    Seq.singleton($"_u{int ch:x4}"))
            |> String.concat ""

        if String.IsNullOrWhiteSpace(text) then
            "_"
        elif Char.IsLetter(text[0]) || text[0] = '_' then
            text
        else
            "_" + text

    let emittedModuleTypeName (moduleName: string) =
        let segments =
            moduleName.Split('.', StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries)
            |> Array.map sanitizeIdentifier

        if Array.isEmpty segments then
            invalidOp "Module name cannot be empty."

        "Kappa.Generated." + String.concat "." segments

    let emittedMethodName (bindingName: string) =
        sanitizeIdentifier bindingName

    let private runtimeType =
        function
        | IlInt64 -> typeof<int64>
        | IlFloat64 -> typeof<double>
        | IlBool -> typeof<bool>
        | IlString -> typeof<string>
        | IlChar -> typeof<char>

    let private buildRawModules (workspace: WorkspaceCompilation) =
        workspace.KBackendIR
        |> List.map (fun moduleDump ->
            let bindings =
                moduleDump.Bindings
                |> List.filter (fun binding -> not binding.Intrinsic)
                |> List.map (fun binding -> binding.Name, binding)
                |> Map.ofList

            moduleDump.Name, bindings)
        |> Map.ofList

    let private tryResolveBinding (rawModules: Map<string, Map<string, KBackendBinding>>) currentModule segments =
        match segments with
        | [] ->
            None
        | [ bindingName ] ->
            rawModules
            |> Map.tryFind currentModule
            |> Option.bind (Map.tryFind bindingName)
            |> Option.map (fun binding -> currentModule, binding)
        | _ ->
            let moduleName =
                segments
                |> List.take (segments.Length - 1)
                |> String.concat "."

            let bindingName = List.last segments

            rawModules
            |> Map.tryFind moduleName
            |> Option.bind (Map.tryFind bindingName)
            |> Option.map (fun binding -> moduleName, binding)

    let private buildEnvironment (workspace: WorkspaceCompilation) =
        let rawModules = buildRawModules workspace
        let cache = Dictionary<string * string, IlValueType>()

        let rec inferBindingType currentModule bindingName active =
            let cacheKey = currentModule, bindingName

            match cache.TryGetValue(cacheKey) with
            | true, cached -> Result.Ok cached
            | _ when Set.contains cacheKey active ->
                Result.Error $"IL backend recursive type inference is not implemented yet for '{currentModule}.{bindingName}'."
            | _ ->
                match rawModules |> Map.tryFind currentModule |> Option.bind (Map.tryFind bindingName) with
                | None ->
                    Result.Error $"IL backend could not resolve binding '{currentModule}.{bindingName}'."
                | Some binding when not (List.isEmpty binding.Parameters) ->
                    Result.Error $"IL backend currently supports only zero-argument bindings, but '{currentModule}.{bindingName}' has parameters."
                | Some binding ->
                    match binding.Body with
                    | None ->
                        Result.Error $"IL backend requires a body for '{currentModule}.{bindingName}'."
                    | Some body ->
                        inferExpressionType currentModule (Set.add cacheKey active) body
                        |> Result.map (fun inferred ->
                            cache[cacheKey] <- inferred
                            inferred)

        and inferExpressionType currentModule active expression =
            match expression with
            | KBackendLiteral(LiteralValue.Integer _) ->
                Result.Ok IlInt64
            | KBackendLiteral(LiteralValue.Float _) ->
                Result.Ok IlFloat64
            | KBackendLiteral(LiteralValue.String _) ->
                Result.Ok IlString
            | KBackendLiteral(LiteralValue.Character _) ->
                Result.Ok IlChar
            | KBackendLiteral LiteralValue.Unit ->
                Result.Error "IL backend does not emit Unit-returning bindings yet."
            | KBackendName [ "True" ]
            | KBackendName [ "False" ] ->
                Result.Ok IlBool
            | KBackendName segments ->
                let nameText = String.concat "." segments

                match tryResolveBinding rawModules currentModule segments with
                | Some (targetModule, binding) when List.isEmpty binding.Parameters ->
                    inferBindingType targetModule binding.Name active
                | Some (targetModule, binding) ->
                    Result.Error $"IL backend does not support function-valued name '{targetModule}.{binding.Name}' yet."
                | None ->
                    Result.Error $"IL backend could not resolve name '{nameText}'."
            | KBackendUnary("-", operand) ->
                inferExpressionType currentModule active operand
                |> Result.bind (function
                    | IlInt64 -> Result.Ok IlInt64
                    | IlFloat64 -> Result.Ok IlFloat64
                    | other -> Result.Error $"Unary '-' is not supported for {other}.")
            | KBackendUnary(operatorName, _) ->
                Result.Error $"IL backend does not support unary operator '{operatorName}' yet."
            | KBackendBinary(left, operatorName, right) ->
                let inferred =
                    result {
                        let! leftType = inferExpressionType currentModule active left
                        let! rightType = inferExpressionType currentModule active right
                        return leftType, rightType
                    }

                inferred
                |> Result.bind (fun (leftType, rightType) ->
                    match operatorName, leftType, rightType with
                    | ("+" | "-" | "*" | "/"), IlInt64, IlInt64 -> Result.Ok IlInt64
                    | ("+" | "-" | "*" | "/"), IlFloat64, IlFloat64 -> Result.Ok IlFloat64
                    | ("==" | "!="), IlInt64, IlInt64 -> Result.Ok IlBool
                    | ("==" | "!="), IlFloat64, IlFloat64 -> Result.Ok IlBool
                    | ("==" | "!="), IlBool, IlBool -> Result.Ok IlBool
                    | ("==" | "!="), IlChar, IlChar -> Result.Ok IlBool
                    | ("<" | "<=" | ">" | ">="), IlInt64, IlInt64 -> Result.Ok IlBool
                    | ("<" | "<=" | ">" | ">="), IlFloat64, IlFloat64 -> Result.Ok IlBool
                    | ("&&" | "||"), IlBool, IlBool -> Result.Ok IlBool
                    | _ ->
                        Result.Error $"IL backend does not support '{operatorName}' for {leftType} and {rightType}.")
            | KBackendIfThenElse(condition, whenTrue, whenFalse) ->
                result {
                    let! conditionType = inferExpressionType currentModule active condition

                    if conditionType <> IlBool then
                        return! Result.Error "IL backend requires Bool conditions for if expressions."

                    let! trueType = inferExpressionType currentModule active whenTrue
                    let! falseType = inferExpressionType currentModule active whenFalse

                    if trueType <> falseType then
                        return!
                            Result.Error
                                $"IL backend requires both if branches to have the same type, but saw {trueType} and {falseType}."

                    return trueType
                }
            | KBackendApply _ ->
                Result.Error "IL backend does not support application yet."
            | KBackendClosure _ ->
                Result.Error "IL backend does not support closures yet."
            | KBackendMatch _ ->
                Result.Error "IL backend does not support pattern matching yet."
            | KBackendPrefixedString _ ->
                Result.Error "IL backend does not support prefixed strings yet."

        rawModules
        |> Map.toList
        |> List.fold
            (fun stateResult (moduleName, bindings) ->
                result {
                    let! state = stateResult
                    let! bindingEntries =
                        bindings
                        |> Map.toList
                        |> List.fold
                            (fun bindingResult (bindingName, binding) ->
                                result {
                                    let! bindingEntriesSoFar = bindingResult
                                    let! returnType = inferBindingType moduleName bindingName Set.empty
                                    let info =
                                        { Binding = binding
                                          ReturnType = returnType
                                          EmittedMethodName = emittedMethodName bindingName }

                                    return (bindingName, info) :: bindingEntriesSoFar
                                })
                            (Result.Ok [])

                    let moduleInfo =
                        { Name = moduleName
                          Bindings = bindingEntries |> Map.ofList
                          EmittedTypeName = emittedModuleTypeName moduleName }

                    return state |> Map.add moduleName moduleInfo
                })
            (Result.Ok Map.empty)
        |> Result.map (fun modules -> { Modules = modules })

    let private emitLiteral (il: ILGenerator) literal =
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
            il.Emit(OpCodes.Ldc_I4, int value)
            Result.Ok()
        | LiteralValue.Unit ->
            Result.Error "IL backend does not emit Unit values yet."

    let private emitComparisonFromCeq (il: ILGenerator) negate =
        il.Emit(OpCodes.Ceq)

        if negate then
            il.Emit(OpCodes.Ldc_I4_0)
            il.Emit(OpCodes.Ceq)

    let private emitName (state: EmissionState) currentModule (il: ILGenerator) segments =
        match segments with
        | [ "True" ] ->
            il.Emit(OpCodes.Ldc_I4_1)
            Result.Ok()
        | [ "False" ] ->
            il.Emit(OpCodes.Ldc_I4_0)
            Result.Ok()
        | _ ->
            let nameText = String.concat "." segments

            match tryResolveBinding (state.Environment.Modules |> Map.map (fun _ moduleInfo -> moduleInfo.Bindings |> Map.map (fun _ bindingInfo -> bindingInfo.Binding))) currentModule segments with
            | Some (targetModule, binding) when List.isEmpty binding.Parameters ->
                let targetMethod =
                    state.MethodBuilders[targetModule][binding.Name]

                il.Emit(OpCodes.Call, targetMethod)
                Result.Ok()
            | Some (targetModule, binding) ->
                Result.Error $"IL backend does not support invoking function-valued binding '{targetModule}.{binding.Name}' yet."
            | None ->
                Result.Error $"IL backend could not resolve name '{nameText}'."

    let rec private emitExpression (state: EmissionState) currentModule (il: ILGenerator) expression =
        let inferExpressionType currentModule expression =
            let moduleInfo = state.Environment.Modules[currentModule]

            let rec infer expression =
                match expression with
                | KBackendLiteral(LiteralValue.Integer _) -> Result.Ok IlInt64
                | KBackendLiteral(LiteralValue.Float _) -> Result.Ok IlFloat64
                | KBackendLiteral(LiteralValue.String _) -> Result.Ok IlString
                | KBackendLiteral(LiteralValue.Character _) -> Result.Ok IlChar
                | KBackendLiteral LiteralValue.Unit -> Result.Error "IL backend does not emit Unit values yet."
                | KBackendName [ "True" ]
                | KBackendName [ "False" ] -> Result.Ok IlBool
                | KBackendName [ bindingName ] ->
                    match moduleInfo.Bindings |> Map.tryFind bindingName with
                    | Some bindingInfo -> Result.Ok bindingInfo.ReturnType
                    | None -> Result.Error $"IL backend could not resolve name '{bindingName}'."
                | KBackendName segments ->
                    let targetModuleName =
                        segments
                        |> List.take (segments.Length - 1)
                        |> String.concat "."

                    let bindingName = List.last segments
                    let nameText = String.concat "." segments

                    match state.Environment.Modules |> Map.tryFind targetModuleName with
                    | Some targetModule ->
                        match targetModule.Bindings |> Map.tryFind bindingName with
                        | Some bindingInfo -> Result.Ok bindingInfo.ReturnType
                        | None -> Result.Error $"IL backend could not resolve name '{nameText}'."
                    | None -> Result.Error $"IL backend could not resolve name '{nameText}'."
                | KBackendUnary("-", operand) ->
                    infer operand
                    |> Result.bind (function
                        | IlInt64 -> Result.Ok IlInt64
                        | IlFloat64 -> Result.Ok IlFloat64
                        | other -> Result.Error $"Unary '-' is not supported for {other}.")
                | KBackendUnary(operatorName, _) ->
                    Result.Error $"IL backend does not support unary operator '{operatorName}' yet."
                | KBackendBinary(left, operatorName, right) ->
                    result {
                        let! leftType = infer left
                        let! rightType = infer right

                        return!
                            match operatorName, leftType, rightType with
                            | ("+" | "-" | "*" | "/"), IlInt64, IlInt64 -> Result.Ok IlInt64
                            | ("+" | "-" | "*" | "/"), IlFloat64, IlFloat64 -> Result.Ok IlFloat64
                            | ("==" | "!="), IlInt64, IlInt64 -> Result.Ok IlBool
                            | ("==" | "!="), IlFloat64, IlFloat64 -> Result.Ok IlBool
                            | ("==" | "!="), IlBool, IlBool -> Result.Ok IlBool
                            | ("==" | "!="), IlChar, IlChar -> Result.Ok IlBool
                            | ("<" | "<=" | ">" | ">="), IlInt64, IlInt64 -> Result.Ok IlBool
                            | ("<" | "<=" | ">" | ">="), IlFloat64, IlFloat64 -> Result.Ok IlBool
                            | ("&&" | "||"), IlBool, IlBool -> Result.Ok IlBool
                            | _ ->
                                Result.Error $"IL backend does not support '{operatorName}' for {leftType} and {rightType}."
                    }
                | KBackendIfThenElse(condition, whenTrue, whenFalse) ->
                    result {
                        let! conditionType = infer condition

                        if conditionType <> IlBool then
                            return! Result.Error "IL backend requires Bool conditions for if expressions."

                        let! trueType = infer whenTrue
                        let! falseType = infer whenFalse

                        if trueType <> falseType then
                            return!
                                Result.Error
                                    $"IL backend requires both if branches to have the same type, but saw {trueType} and {falseType}."

                        return trueType
                    }
                | KBackendApply _ ->
                    Result.Error "IL backend does not support application yet."
                | KBackendClosure _ ->
                    Result.Error "IL backend does not support closures yet."
                | KBackendMatch _ ->
                    Result.Error "IL backend does not support pattern matching yet."
                | KBackendPrefixedString _ ->
                    Result.Error "IL backend does not support prefixed strings yet."

            infer expression

        match expression with
        | KBackendLiteral literal ->
            emitLiteral il literal
        | KBackendName segments ->
            emitName state currentModule il segments
        | KBackendUnary("-", operand) ->
            result {
                let! operandType = inferExpressionType currentModule operand
                do! emitExpression state currentModule il operand
                match operandType with
                | IlInt64
                | IlFloat64 ->
                    il.Emit(OpCodes.Neg)
                    return ()
                | other ->
                    return! Result.Error $"Unary '-' is not supported for {other}."
            }
        | KBackendUnary(operatorName, _) ->
            Result.Error $"IL backend does not support unary operator '{operatorName}' yet."
        | KBackendBinary(left, operatorName, right) ->
            result {
                let! leftType = inferExpressionType currentModule left
                let! rightType = inferExpressionType currentModule right
                do! emitExpression state currentModule il left
                do! emitExpression state currentModule il right

                match operatorName, leftType, rightType with
                | "+", IlInt64, IlInt64
                | "+", IlFloat64, IlFloat64 ->
                    il.Emit(OpCodes.Add)
                | "-", IlInt64, IlInt64
                | "-", IlFloat64, IlFloat64 ->
                    il.Emit(OpCodes.Sub)
                | "*", IlInt64, IlInt64
                | "*", IlFloat64, IlFloat64 ->
                    il.Emit(OpCodes.Mul)
                | "/", IlInt64, IlInt64
                | "/", IlFloat64, IlFloat64 ->
                    il.Emit(OpCodes.Div)
                | "==", IlInt64, IlInt64
                | "==", IlFloat64, IlFloat64
                | "==", IlBool, IlBool
                | "==", IlChar, IlChar ->
                    emitComparisonFromCeq il false
                | "!=", IlInt64, IlInt64
                | "!=", IlFloat64, IlFloat64
                | "!=", IlBool, IlBool
                | "!=", IlChar, IlChar ->
                    emitComparisonFromCeq il true
                | "<", IlInt64, IlInt64
                | "<", IlFloat64, IlFloat64 ->
                    il.Emit(OpCodes.Clt)
                | ">", IlInt64, IlInt64
                | ">", IlFloat64, IlFloat64 ->
                    il.Emit(OpCodes.Cgt)
                | "<=", IlInt64, IlInt64
                | "<=", IlFloat64, IlFloat64 ->
                    il.Emit(OpCodes.Cgt)
                    il.Emit(OpCodes.Ldc_I4_0)
                    il.Emit(OpCodes.Ceq)
                | ">=", IlInt64, IlInt64
                | ">=", IlFloat64, IlFloat64 ->
                    il.Emit(OpCodes.Clt)
                    il.Emit(OpCodes.Ldc_I4_0)
                    il.Emit(OpCodes.Ceq)
                | "&&", IlBool, IlBool
                | "||", IlBool, IlBool ->
                    return! Result.Error "IL backend emits short-circuit boolean operators only through dedicated lowering."
                | _ ->
                    return! Result.Error $"IL backend does not support '{operatorName}' for {leftType} and {rightType}."

                return ()
            }
        | KBackendIfThenElse(condition, whenTrue, whenFalse) ->
            result {
                let! conditionType = inferExpressionType currentModule condition

                if conditionType <> IlBool then
                    return! Result.Error "IL backend requires Bool conditions for if expressions."

                do! emitExpression state currentModule il condition
                let falseLabel = il.DefineLabel()
                let endLabel = il.DefineLabel()

                il.Emit(OpCodes.Brfalse, falseLabel)
                do! emitExpression state currentModule il whenTrue
                il.Emit(OpCodes.Br, endLabel)
                il.MarkLabel(falseLabel)
                do! emitExpression state currentModule il whenFalse
                il.MarkLabel(endLabel)
            }
        | KBackendApply _ ->
            Result.Error "IL backend does not support application yet."
        | KBackendClosure _ ->
            Result.Error "IL backend does not support closures yet."
        | KBackendMatch _ ->
            Result.Error "IL backend does not support pattern matching yet."
        | KBackendPrefixedString _ ->
            Result.Error "IL backend does not support prefixed strings yet."

    let private emitMethodBody (state: EmissionState) (moduleInfo: ModuleInfo) (bindingInfo: BindingInfo) =
        result {
            let methodBuilder =
                state.MethodBuilders[moduleInfo.Name][bindingInfo.Binding.Name]

            let il = methodBuilder.GetILGenerator()

            match bindingInfo.Binding.Body with
            | None ->
                return! Result.Error $"IL backend requires a body for '{moduleInfo.Name}.{bindingInfo.Binding.Name}'."
            | Some body ->
                match body with
                | KBackendBinary(left, "&&", right) ->
                    let falseLabel = il.DefineLabel()
                    let endLabel = il.DefineLabel()

                    do! emitExpression state moduleInfo.Name il left
                    il.Emit(OpCodes.Brfalse, falseLabel)
                    do! emitExpression state moduleInfo.Name il right
                    il.Emit(OpCodes.Br, endLabel)
                    il.MarkLabel(falseLabel)
                    il.Emit(OpCodes.Ldc_I4_0)
                    il.MarkLabel(endLabel)
                    il.Emit(OpCodes.Ret)
                | KBackendBinary(left, "||", right) ->
                    let trueLabel = il.DefineLabel()
                    let endLabel = il.DefineLabel()

                    do! emitExpression state moduleInfo.Name il left
                    il.Emit(OpCodes.Brtrue, trueLabel)
                    do! emitExpression state moduleInfo.Name il right
                    il.Emit(OpCodes.Br, endLabel)
                    il.MarkLabel(trueLabel)
                    il.Emit(OpCodes.Ldc_I4_1)
                    il.MarkLabel(endLabel)
                    il.Emit(OpCodes.Ret)
                | _ ->
                    do! emitExpression state moduleInfo.Name il body
                    il.Emit(OpCodes.Ret)
        }

    let emitAssemblyArtifact (workspace: WorkspaceCompilation) (outputDirectory: string) =
        if workspace.HasErrors then
            Result.Error $"Cannot emit a CLR assembly for a workspace with diagnostics:{Environment.NewLine}{aggregateDiagnostics workspace.Diagnostics}"
        else
            let verificationDiagnostics = Compilation.verifyCheckpoint workspace "KBackendIR"

            if not (List.isEmpty verificationDiagnostics) then
                Result.Error $"Cannot emit malformed KBackendIR:{Environment.NewLine}{aggregateDiagnostics verificationDiagnostics}"
            else
                match buildEnvironment workspace with
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

                        let typeBuilders =
                            environment.Modules
                            |> Map.toList
                            |> List.map (fun (moduleName, moduleInfo) ->
                                let typeBuilder =
                                    moduleBuilder.DefineType(
                                        moduleInfo.EmittedTypeName,
                                        TypeAttributes.Public ||| TypeAttributes.Abstract ||| TypeAttributes.Sealed ||| TypeAttributes.Class
                                    )

                                moduleName, typeBuilder)
                            |> Map.ofList

                        let methodBuilders =
                            environment.Modules
                            |> Map.toList
                            |> List.map (fun (moduleName, moduleInfo) ->
                                let methods =
                                    moduleInfo.Bindings
                                    |> Map.toList
                                    |> List.map (fun (bindingName, bindingInfo) ->
                                        let methodBuilder =
                                            typeBuilders[moduleName].DefineMethod(
                                                bindingInfo.EmittedMethodName,
                                                MethodAttributes.Public ||| MethodAttributes.Static ||| MethodAttributes.HideBySig,
                                                runtimeType bindingInfo.ReturnType,
                                                Type.EmptyTypes
                                            )

                                        bindingName, methodBuilder)
                                    |> Map.ofList

                                moduleName, methods)
                            |> Map.ofList

                        let state =
                            { Environment = environment
                              TypeBuilders = typeBuilders
                              MethodBuilders = methodBuilders }

                        let emissionResult =
                            environment.Modules
                            |> Map.toList
                            |> List.map snd
                            |> List.collect (fun moduleInfo -> moduleInfo.Bindings |> Map.toList |> List.map (fun (_, bindingInfo) -> moduleInfo, bindingInfo))
                            |> List.fold
                                (fun resultSoFar (moduleInfo, bindingInfo) ->
                                    result {
                                        do! resultSoFar
                                        do! emitMethodBody state moduleInfo bindingInfo
                                    })
                                (Result.Ok())

                        match emissionResult with
                        | Result.Error message ->
                            Result.Error message
                        | Result.Ok() ->
                            typeBuilders |> Map.iter (fun _ typeBuilder -> typeBuilder.CreateType() |> ignore)
                            assemblyBuilder.Save(assemblyPath)

                            Result.Ok
                                { OutputDirectory = resolvedOutputDirectory
                                  AssemblyName = assemblyName
                                  AssemblyFilePath = assemblyPath }
                    with ex ->
                        Result.Error $"IL backend emission failed: {ex.Message}"
