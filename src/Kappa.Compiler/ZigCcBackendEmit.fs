namespace Kappa.Compiler

// Lowers KBackendIR expressions and bindings into generated C fragments for the zig backend.
module internal ZigCcBackendEmit =
    open ZigCcBackendSupport

    let rec private tryDecomposeRecursiveClosureValue expression =
        match expression with
        | BackendClosure(parameters, captures, environmentLayout, body, convention, _) ->
            Some([], parameters, captures, environmentLayout, body, convention)
        | BackendLet(binding, value, body, _) ->
            tryDecomposeRecursiveClosureValue body
            |> Option.map (fun (prefixBindings, parameters, captures, environmentLayout, closureBody, convention) ->
                ((binding, value) :: prefixBindings, parameters, captures, environmentLayout, closureBody, convention))
        | _ ->
            None

    let rec private collectLocalNameUses (available: Set<string>) (bound: Set<string>) (expression: KBackendExpression) =
        match expression with
        | BackendLiteral _
        | BackendDictionaryValue _ ->
            Set.empty
        | BackendName(BackendLocalName(name, _)) when Set.contains name available && not (Set.contains name bound) ->
            Set.singleton name
        | BackendName _ ->
            Set.empty
        | BackendEffectLabel _ ->
            Set.empty
        | BackendEffectOperation(label, _, _, _) ->
            collectLocalNameUses available bound label
        | BackendClosure(parameters, captures, _, body, _, _) ->
            let closureBound =
                parameters
                |> List.fold (fun state parameter -> Set.add parameter.Name state) bound
                |> fun state -> captures |> List.fold (fun captureState capture -> Set.add capture.Name captureState) state

            let explicitCaptureUses =
                captures
                |> List.map (fun capture -> capture.Name)
                |> List.filter (fun name -> Set.contains name available && not (Set.contains name bound))
                |> Set.ofList

            Set.union explicitCaptureUses (collectLocalNameUses available closureBound body)
        | BackendIfThenElse(condition, whenTrue, whenFalse, _) ->
            collectLocalNameUses available bound condition
            |> Set.union (collectLocalNameUses available bound whenTrue)
            |> Set.union (collectLocalNameUses available bound whenFalse)
        | BackendMatch(scrutinee, cases, _) ->
            let collectPatternBindings pattern =
                let rec loop inner =
                    match inner with
                    | BackendWildcardPattern
                    | BackendLiteralPattern _ ->
                        Set.empty
                    | BackendBindPattern binding ->
                        Set.singleton binding.Name
                    | BackendConstructorPattern(_, _, _, _, fieldPatterns) ->
                        fieldPatterns |> List.fold (fun state current -> Set.union state (loop current)) Set.empty

                loop pattern

            cases
            |> List.fold
                (fun state caseClause ->
                    let caseBound = Set.union bound (collectPatternBindings caseClause.Pattern)
                    state
                    |> Set.union (caseClause.Guard |> Option.map (collectLocalNameUses available caseBound) |> Option.defaultValue Set.empty)
                    |> Set.union (collectLocalNameUses available caseBound caseClause.Body))
                (collectLocalNameUses available bound scrutinee)
        | BackendExecute(inner, _)
        | BackendPure(inner, _) ->
            collectLocalNameUses available bound inner
        | BackendBind(action, binder, _)
        | BackendThen(action, binder, _)
        | BackendSequence(action, binder, _)
        | BackendCall(action, [ binder ], _, _)
        | BackendCall(action, binder :: [], _, _) ->
            collectLocalNameUses available bound action
            |> Set.union (collectLocalNameUses available bound binder)
        | BackendHandle(_, label, body, returnClause, operationClauses, _) ->
            let returnBound =
                returnClause.Arguments
                |> List.fold
                    (fun state argument ->
                        match argument with
                        | BackendEffectNameArgument name -> Set.add name state
                        | _ -> state)
                    bound

            let returnUses = collectLocalNameUses available returnBound returnClause.Body

            let operationUses =
                operationClauses
                |> List.fold
                    (fun state clause ->
                        let clauseBound =
                            clause.Arguments
                            |> List.fold
                                (fun boundState argument ->
                                    match argument with
                                    | BackendEffectNameArgument name -> Set.add name boundState
                                    | _ -> boundState)
                                bound
                            |> fun boundState ->
                                clause.ResumptionName
                                |> Option.map (fun name -> Set.add name boundState)
                                |> Option.defaultValue boundState

                        Set.union state (collectLocalNameUses available clauseBound clause.Body))
                    Set.empty

            collectLocalNameUses available bound label
            |> Set.union (collectLocalNameUses available bound body)
            |> Set.union returnUses
            |> Set.union operationUses
        | BackendLet(binding, value, body, _) ->
            collectLocalNameUses available bound value
            |> Set.union (collectLocalNameUses available (Set.add binding.Name bound) body)
        | BackendWhile(condition, body) ->
            collectLocalNameUses available bound condition
            |> Set.union (collectLocalNameUses available bound body)
        | BackendCall(callee, arguments, _, _)
        | BackendTraitCall(_, _, callee, arguments, _) ->
            arguments
            |> List.fold
                (fun state argument -> Set.union state (collectLocalNameUses available bound argument))
                (collectLocalNameUses available bound callee)
        | BackendConstructData(_, _, _, _, fields, _) ->
            fields
            |> List.fold (fun state field -> Set.union state (collectLocalNameUses available bound field)) Set.empty
        | BackendPrefixedString(_, parts, _) ->
            parts
            |> List.fold
                (fun state part ->
                    match part with
                    | BackendStringText _ -> state
                    | BackendStringInterpolation inner -> Set.union state (collectLocalNameUses available bound inner))
                Set.empty

    let rec internal emitExpression (context: GenerationContext) (scope: EmitScope) (expression: KBackendExpression) : Result<EmittedExpression, string> =
        match expression with
        | BackendLiteral(literal, _) ->
            Result.Ok
                { Statements = []
                  ValueExpression = literalToBoxedExpression literal }
        | BackendName resolvedName ->
            emitNameExpression context scope resolvedName
        | BackendEffectLabel(labelName, interfaceId, labelId, operations, _) ->
            emitEffectLabelExpression context labelName interfaceId labelId operations
        | BackendEffectOperation(label, operationId, operationName, _) ->
            emitEffectOperationExpression context scope label operationId operationName
        | BackendClosure(parameters, captures, environmentLayout, body, convention, _) ->
            emitClosureExpression context scope parameters captures environmentLayout body convention
        | BackendIfThenElse(condition, whenTrue, whenFalse, _) ->
            emitIfExpression context scope condition whenTrue whenFalse
        | BackendMatch(scrutinee, cases, _) ->
            emitMatchExpression context scope scrutinee cases
        | BackendExecute(expression, _) ->
            emitExecuteExpression context scope expression
        | BackendPure(expression, _) ->
            emitPureExpression context scope expression
        | BackendBind(action, binder, _) ->
            emitBindExpression context scope action binder
        | BackendThen(action, binder, _) ->
            emitThenExpression context scope action binder
        | BackendHandle(isDeep, label, body, returnClause, operationClauses, _) ->
            emitHandleExpression context scope isDeep label body returnClause operationClauses
        | BackendLet(binding, value, body, _) ->
            emitLetExpression context scope binding value body
        | BackendSequence(first, second, _) ->
            emitSequenceExpression context scope first second
        | BackendWhile(condition, body) ->
            emitWhileExpression context scope condition body
        | BackendCall(callee, arguments, _, _) ->
            emitCallExpression context scope callee arguments
        | BackendDictionaryValue(moduleName, traitName, instanceKey, _) ->
            emitDictionaryValueExpression context moduleName traitName instanceKey
        | BackendTraitCall(traitName, memberName, dictionary, arguments, _) ->
            emitTraitDispatchExpression context scope traitName memberName dictionary arguments
        | BackendConstructData(moduleName, typeName, _, tag, fields, _) ->
            emitConstructDataExpression context scope moduleName typeName tag fields
        | BackendPrefixedString(prefix, _, _) ->
            Result.Error(sprintf "zig does not support prefixed string backend expression '%s\"...\"' yet." prefix)

    and internal emitExpressions (context: GenerationContext) (scope: EmitScope) (expressions: KBackendExpression list) : Result<EmittedExpression list, string> =
        let rec loop remaining emitted =
            result {
                match remaining with
                | [] ->
                    return List.rev emitted
                | head :: tail ->
                    let! emittedHead = emitExpression context scope head
                    return! loop tail (emittedHead :: emitted)
            }

        loop expressions []

    and internal emitNameExpression (context: GenerationContext) (scope: EmitScope) resolvedName =
        match resolvedName with
        | BackendLocalName(name, _) ->
            scope.Bindings
            |> Map.tryFind name
            |> Option.map (fun expression ->
                { Statements = []
                  ValueExpression = expression })
            |> resultOfOption $"zig could not resolve local runtime name '{name}'."
        | BackendGlobalBindingName(moduleName, bindingName, _) ->
            match lookupFunction context moduleName bindingName, lookupFunctionName context moduleName bindingName with
            | Some functionDump, Some emittedName ->
                let debugName = cStringLiteral $"{moduleName}.{bindingName}"

                Result.Ok
                    { Statements = []
                      ValueExpression =
                        $"kappa_make_closure(&{emittedName}, NULL, {functionDump.Parameters.Length}, \"{debugName}\")" }
            | _ ->
                Result.Error $"zig could not resolve global runtime binding '{moduleName}.{bindingName}'."
        | BackendIntrinsicName(_, bindingName, _) ->
            match bindingName with
            | "True" ->
                Result.Ok
                    { Statements = []
                      ValueExpression = "kappa_box_bool(1)" }
            | "False" ->
                Result.Ok
                    { Statements = []
                      ValueExpression = "kappa_box_bool(0)" }
            | other ->
                Result.Error $"zig does not yet support using intrinsic '{other}' as a first-class value."
        | BackendConstructorName(moduleName, typeName, _, tag, arity, _) ->
            if arity <> 0 then
                Result.Error
                    $"zig does not yet support using constructor '{moduleName}.{typeName}' with arity {arity} as a first-class value."
            else
                result {
                    let! typeId = lookupTypeId context moduleName typeName

                    return
                        { Statements = []
                          ValueExpression = $"kappa_make_data({typeId}, {tag}, 0, NULL)" }
                }

    and internal emitEffectLabelExpression
        (context: GenerationContext)
        (labelName: string)
        (interfaceId: string)
        (labelId: string)
        (operations: KBackendEffectOperationInfo list)
        =
        let resultValue = freshTemp context "effect_label"

        let metadataStatements =
            if List.isEmpty operations then
                []
            else
                let metadataName = freshTemp context "effect_ops"

                [ $"KappaEffectOperationMetadata {metadataName}[{operations.Length}] = {{" ]
                @ (operations
                   |> List.map (fun operation ->
                       let allowsMultiple =
                           if operation.AllowsMultipleResumptions then 1 else 0

                       $"    {{ \"{cStringLiteral operation.OperationId}\", \"{cStringLiteral operation.Name}\", {operation.ParameterArity}, {allowsMultiple} }},"))
                @ [ "};"
                    $"KValue* {resultValue} = kappa_make_effect_label(\"{cStringLiteral labelName}\", \"{cStringLiteral interfaceId}\", \"{cStringLiteral labelId}\", {operations.Length}, {metadataName});" ]

        Result.Ok
            { Statements =
                if List.isEmpty operations then
                    [ $"KValue* {resultValue} = kappa_make_effect_label(\"{cStringLiteral labelName}\", \"{cStringLiteral interfaceId}\", \"{cStringLiteral labelId}\", 0, NULL);" ]
                else
                    metadataStatements
              ValueExpression = resultValue }

    and internal emitEffectOperationExpression
        (context: GenerationContext)
        (scope: EmitScope)
        (label: KBackendExpression)
        (operationId: string)
        (operationName: string)
        =
        result {
            let! emittedLabel = emitExpression context scope label
            let resultValue = freshTemp context "effect_op"

            return
                { Statements =
                    emittedLabel.Statements
                    @ [ $"KValue* {resultValue} = kappa_make_effect_operation({emittedLabel.ValueExpression}, \"{cStringLiteral operationId}\", \"{cStringLiteral operationName}\");" ]
                  ValueExpression = resultValue }
        }

    and internal emitHandlerReturnClosureExpression
        (context: GenerationContext)
        (scope: EmitScope)
        (clause: KBackendEffectHandlerClause)
        =
        let parameters : KBackendParameter list =
            clause.Arguments
            |> List.mapi (fun index argument ->
                let parameterName =
                    match argument with
                    | BackendEffectNameArgument name -> name
                    | BackendEffectUnitArgument -> $"__kappa_unit_{index}"
                    | BackendEffectWildcardArgument -> $"__kappa_wildcard_{index}"

                ({ Name = parameterName
                   Representation = BackendRepOpaque None }: KBackendParameter))

        let boundNames =
            parameters |> List.map (fun parameter -> parameter.Name) |> Set.ofList

        let availableNames =
            scope.Bindings |> Map.toList |> List.map fst |> Set.ofList

        let captures : KBackendCapture list =
            collectLocalNameUses availableNames boundNames clause.Body
            |> Set.toList
            |> List.sort
            |> List.map (fun captureName ->
                ({ Name = captureName
                   Representation = BackendRepOpaque None }: KBackendCapture))

        let convention =
            { RuntimeArity = parameters.Length
              ParameterRepresentations = parameters |> List.map (fun parameter -> parameter.Representation)
              ResultRepresentation = Some BackendRepIOAction
              RetainedDictionaryParameters = [] }

        emitClosureExpression
            context
            scope
            parameters
            captures
            $"handler_return_env_{context.NextClosureId}"
            clause.Body
            convention

    and internal emitHandlerDispatcherClosureExpression
        (context: GenerationContext)
        (scope: EmitScope)
        (operationClauses: KBackendEffectHandlerClause list)
        =
        result {
            let emittedFunctionName = freshClosureFunctionName context scope.CurrentModule
            let environmentTypeName = sanitizeIdentifier $"{emittedFunctionName}_env"
            let availableNames =
                scope.Bindings |> Map.toList |> List.map fst |> Set.ofList

            let captureNames =
                operationClauses
                |> List.fold
                    (fun state clause ->
                        let clauseBound =
                            clause.Arguments
                            |> List.choose (function
                                | BackendEffectNameArgument name -> Some name
                                | BackendEffectUnitArgument
                                | BackendEffectWildcardArgument -> None)
                            |> List.fold (fun bound name -> Set.add name bound) Set.empty
                            |> fun bound ->
                                clause.ResumptionName
                                |> Option.map (fun name -> Set.add name bound)
                                |> Option.defaultValue bound

                        Set.union state (collectLocalNameUses availableNames clauseBound clause.Body))
                    Set.empty
                |> Set.toList
                |> List.sort

            let captureBindings =
                captureNames
                |> List.mapi (fun index captureName ->
                    let fieldName = sprintf "capture_%d" index
                    captureName, $"env->{fieldName}")
                |> Map.ofList

            let environmentDefinitions =
                if List.isEmpty captureNames then
                    []
                else
                    [ $"typedef struct {environmentTypeName} {{"
                      yield!
                        captureNames
                        |> List.mapi (fun index _ -> $"    KValue* capture_{index};")
                      $"}} {environmentTypeName};" ]

            let captureSetup =
                if List.isEmpty captureNames then
                    [ "(void)raw_env;" ]
                else
                    [ $"{environmentTypeName}* env = ({environmentTypeName}*)raw_env;" ]

            let opNameVar = "__kappa_op_name"
            let rawArgsVar = "__kappa_raw_args"
            let resumptionVar = "__kappa_resumption"

            let emitClause (clause: KBackendEffectHandlerClause) =
                result {
                    let initialBindings =
                        captureBindings
                        |> Map.add opNameVar opNameVar
                        |> Map.add rawArgsVar rawArgsVar
                        |> Map.add resumptionVar resumptionVar

                    let clauseBindings, argumentStatements =
                        clause.Arguments
                        |> List.mapi (fun index argument ->
                            match argument with
                            | BackendEffectNameArgument name ->
                                let localName = $"{name}_{index}"
                                (Some(name, localName), Some $"KValue* {localName} = kappa_array_get({rawArgsVar}, {index});")
                            | BackendEffectUnitArgument ->
                                (None, Some $"kappa_expect_unit(kappa_array_get({rawArgsVar}, {index}));")
                            | BackendEffectWildcardArgument ->
                                (None, None))
                        |> List.fold
                            (fun (bindings, statements) (binding, statement) ->
                                let updatedBindings =
                                    binding
                                    |> Option.map (fun (name, localName) -> bindings |> Map.add name localName)
                                    |> Option.defaultValue bindings

                                let updatedStatements =
                                    statement
                                    |> Option.map (fun line -> statements @ [ line ])
                                    |> Option.defaultValue statements

                                updatedBindings, updatedStatements)
                            (initialBindings, [])

                    let clauseBindings =
                        clause.ResumptionName
                        |> Option.map (fun name -> clauseBindings |> Map.add name resumptionVar)
                        |> Option.defaultValue clauseBindings

                    let clauseScope =
                        { CurrentModule = scope.CurrentModule
                          Bindings = clauseBindings }

                    let! emittedBody = emitExpression context clauseScope clause.Body

                    return
                        [ $"if (strcmp({opNameVar}, \"{cStringLiteral clause.OperationName}\") == 0) {{" ]
                        @ ([
                               $"    if (kappa_array_length({rawArgsVar}) != {clause.Arguments.Length}) {{"
                               $"        kappa_panic(\"handler clause '{cStringLiteral clause.OperationName}' expected {clause.Arguments.Length} argument(s)\");"
                               "    }"
                           ]
                           @ (argumentStatements |> indentLines 1)
                           @ (emittedBody.Statements |> indentLines 1)
                           @ [ $"    return {emittedBody.ValueExpression};" ])
                        @ [ "}" ]
                }

            let! clauseBlocks =
                operationClauses
                        |> List.fold
                            (fun state clause ->
                                result {
                                    let! emitted = state
                                    let! emittedClause = emitClause clause
                                    return emitted @ emittedClause
                                })
                            (Result.Ok [])

            let functionLines =
                [
                    $"static KValue* {emittedFunctionName}(void* raw_env, KValue** args, int argc)"
                    "{"
                    $"    if (argc != 3) {{"
                    $"        kappa_panic_arity(\"{cStringLiteral emittedFunctionName}\", 3, argc);"
                    "    }"
                    yield! captureSetup |> indentLines 1
                    $"    const char* {opNameVar} = kappa_expect_string(args[0]);"
                    $"    KValue* {rawArgsVar} = args[1];"
                    $"    KValue* {resumptionVar} = args[2];"
                    yield! clauseBlocks |> indentLines 1
                    "    kappa_panic(\"handler dispatcher could not find a matching operation clause\");"
                    "    return kappa_unit();"
                    "}"
                ]

            context.GeneratedClosures.Add
                { Prototype = $"static KValue* {emittedFunctionName}(void* raw_env, KValue** args, int argc);"
                  SupportDefinitions = environmentDefinitions
                  Definition = joinLines functionLines }

            if List.isEmpty captureNames then
                return
                    { Statements = []
                      ValueExpression =
                        $"kappa_make_closure(&{emittedFunctionName}, NULL, 3, \"{cStringLiteral emittedFunctionName}\")" }
            else
                let environmentVariable = freshTemp context "handler_env"

                let captureAssignments =
                    captureNames
                    |> List.mapi (fun index captureName ->
                        scope.Bindings
                        |> Map.tryFind captureName
                        |> resultOfOption $"zig handler dispatcher could not resolve captured local '{captureName}'."
                        |> Result.map (fun capturedExpression ->
                            $"{environmentVariable}->capture_{index} = {capturedExpression};"))
                    |> List.fold
                        (fun state item ->
                            result {
                                let! lines = state
                                let! line = item
                                return lines @ [ line ]
                            })
                        (Result.Ok [])

                let! captureAssignments = captureAssignments

                return
                    { Statements =
                        [ $"{environmentTypeName}* {environmentVariable} = ({environmentTypeName}*)kappa_alloc(sizeof({environmentTypeName}));" ]
                        @ captureAssignments
                      ValueExpression =
                        $"kappa_make_closure(&{emittedFunctionName}, {environmentVariable}, 3, \"{cStringLiteral emittedFunctionName}\")" }
        }

    and internal emitHandleExpression
        (context: GenerationContext)
        (scope: EmitScope)
        (isDeep: bool)
        (label: KBackendExpression)
        (body: KBackendExpression)
        (returnClause: KBackendEffectHandlerClause)
        (operationClauses: KBackendEffectHandlerClause list)
        =
        result {
            let! emittedLabel = emitExpression context scope label
            let! emittedBody = emitExpression context scope body
            let! emittedReturnClosure = emitHandlerReturnClosureExpression context scope returnClause
            let! emittedDispatcher = emitHandlerDispatcherClosureExpression context scope operationClauses
            let resultValue = freshTemp context "handle"
            let deepFlag = if isDeep then 1 else 0

            return
                { Statements =
                    emittedLabel.Statements
                    @ emittedBody.Statements
                    @ emittedReturnClosure.Statements
                    @ emittedDispatcher.Statements
                    @ [ $"KValue* {resultValue} = kappa_handle({deepFlag}, {emittedLabel.ValueExpression}, {emittedBody.ValueExpression}, {emittedReturnClosure.ValueExpression}, {emittedDispatcher.ValueExpression});" ]
                  ValueExpression = resultValue }
        }

    and internal emitClosureExpression
        (context: GenerationContext)
        (scope: EmitScope)
        (parameters: KBackendParameter list)
        (captures: KBackendCapture list)
        (environmentLayout: string)
        (body: KBackendExpression)
        (convention: KBackendCallingConvention)
        =
        result {
            let emittedFunctionName = freshClosureFunctionName context scope.CurrentModule
            let environmentTypeName = sanitizeIdentifier environmentLayout

            let captureBindings =
                captures
                |> List.mapi (fun index capture ->
                    let fieldName = sprintf "capture_%d" index
                    capture.Name, $"env->{fieldName}"
                )
                |> Map.ofList

            let parameterBindings =
                parameters
                |> List.mapi (fun index parameter -> parameter.Name, $"args[{index}]")
                |> Map.ofList

            let closureScope =
                { CurrentModule = scope.CurrentModule
                  Bindings =
                    parameterBindings
                    |> Map.fold (fun acc name expression -> acc.Add(name, expression)) captureBindings }

            let! bodyEmission = emitExpression context closureScope body

            let environmentDefinitions =
                if List.isEmpty captures then
                    []
                else
                    [ $"typedef struct {environmentTypeName} {{"
                      yield!
                        captures
                        |> List.mapi (fun index _ -> $"    KValue* capture_{index};")
                      $"}} {environmentTypeName};" ]

            let captureSetup =
                if List.isEmpty captures then
                    [ "(void)raw_env;" ]
                else
                    [ $"{environmentTypeName}* env = ({environmentTypeName}*)raw_env;" ]

            let functionLines =
                [
                    $"static KValue* {emittedFunctionName}(void* raw_env, KValue** args, int argc)"
                    "{"
                    $"    if (argc != {convention.RuntimeArity}) {{"
                    $"        kappa_panic_arity(\"{cStringLiteral emittedFunctionName}\", {convention.RuntimeArity}, argc);"
                    "    }"
                    yield! captureSetup |> indentLines 1
                    yield! bodyEmission.Statements |> indentLines 1
                    $"    return {bodyEmission.ValueExpression};"
                    "}"
                ]

            context.GeneratedClosures.Add
                { Prototype = $"static KValue* {emittedFunctionName}(void* raw_env, KValue** args, int argc);"
                  SupportDefinitions = environmentDefinitions
                  Definition = joinLines functionLines }

            if List.isEmpty captures then
                return
                    { Statements = []
                      ValueExpression =
                        $"kappa_make_closure(&{emittedFunctionName}, NULL, {convention.RuntimeArity}, \"{cStringLiteral emittedFunctionName}\")" }
            else
                let environmentVariable = freshTemp context "closure_env"

                let captureAssignments =
                    captures
                    |> List.mapi (fun index capture ->
                        let capturedExpression =
                            scope.Bindings
                            |> Map.tryFind capture.Name
                            |> resultOfOption $"zig closure could not resolve captured local '{capture.Name}'."

                        capturedExpression
                        |> Result.map (fun expression ->
                            $"{environmentVariable}->capture_{index} = {expression};"))

                let! captureAssignments =
                    captureAssignments
                    |> List.fold
                        (fun state item ->
                            result {
                                let! lines = state
                                let! line = item
                                return lines @ [ line ]
                            })
                        (Result.Ok [])

                return
                    { Statements =
                        [ $"{environmentTypeName}* {environmentVariable} = ({environmentTypeName}*)kappa_alloc(sizeof({environmentTypeName}));" ]
                        @ captureAssignments
                      ValueExpression =
                        $"kappa_make_closure(&{emittedFunctionName}, {environmentVariable}, {convention.RuntimeArity}, \"{cStringLiteral emittedFunctionName}\")" }
        }

    and internal emitIfExpression (context: GenerationContext) (scope: EmitScope) condition whenTrue whenFalse =
        result {
            let! emittedCondition = emitExpression context scope condition
            let! emittedTrue = emitExpression context scope whenTrue
            let! emittedFalse = emitExpression context scope whenFalse

            let conditionValue = freshTemp context "if_condition"
            let resultValue = freshTemp context "if_result"

            return
                { Statements =
                    emittedCondition.Statements
                    @ [ $"KValue* {conditionValue} = {emittedCondition.ValueExpression};"
                        $"KValue* {resultValue} = NULL;"
                        $"if (kappa_expect_bool({conditionValue})) {{" ]
                    @ indentLines 1 emittedTrue.Statements
                    @ [ $"    {resultValue} = {emittedTrue.ValueExpression};"
                        "}"
                        "else {"
                      ]
                    @ indentLines 1 emittedFalse.Statements
                    @ [ $"    {resultValue} = {emittedFalse.ValueExpression};"
                        "}" ]
                  ValueExpression = resultValue }
        }

    and internal emitExecuteExpression (context: GenerationContext) (scope: EmitScope) expression =
        result {
            let! emittedExpression = emitExpression context scope expression
            let resultValue = freshTemp context "execute"

            return
                { Statements =
                    emittedExpression.Statements
                    @ [ $"KValue* {resultValue} = kappa_execute_value({emittedExpression.ValueExpression});" ]
                  ValueExpression = resultValue }
        }

    and internal emitPureExpression (context: GenerationContext) (scope: EmitScope) expression =
        result {
            let! emittedExpression = emitExpression context scope expression
            let resultValue = freshTemp context "pure"

            return
                { Statements =
                    emittedExpression.Statements
                    @ [ $"KValue* {resultValue} = kappa_pure({emittedExpression.ValueExpression});" ]
                  ValueExpression = resultValue }
        }

    and internal emitBindExpression (context: GenerationContext) (scope: EmitScope) action binder =
        result {
            let! emittedAction = emitExpression context scope action
            let! emittedBinder = emitExpression context scope binder
            let resultValue = freshTemp context "bind"

            return
                { Statements =
                    emittedAction.Statements
                    @ emittedBinder.Statements
                    @ [ $"KValue* {resultValue} = kappa_bind({emittedAction.ValueExpression}, {emittedBinder.ValueExpression});" ]
                  ValueExpression = resultValue }
        }

    and internal emitThenExpression (context: GenerationContext) (scope: EmitScope) first second =
        result {
            let! emittedFirst = emitExpression context scope first
            let! emittedSecond = emitExpression context scope second
            let resultValue = freshTemp context "then"

            return
                { Statements =
                    emittedFirst.Statements
                    @ emittedSecond.Statements
                    @ [ $"KValue* {resultValue} = kappa_then({emittedFirst.ValueExpression}, {emittedSecond.ValueExpression});" ]
                  ValueExpression = resultValue }
        }

    and internal emitRecursiveClosureLetExpression
        (context: GenerationContext)
        (scope: EmitScope)
        (binding: KBackendParameter)
        (prefixBindings: (KBackendParameter * KBackendExpression) list)
        (parameters: KBackendParameter list)
        (captures: KBackendCapture list)
        (environmentLayout: string)
        (closureBody: KBackendExpression)
        (convention: KBackendCallingConvention)
        (body: KBackendExpression)
        =
        result {
            let emittedFunctionName = freshClosureFunctionName context scope.CurrentModule
            let environmentTypeName = sanitizeIdentifier environmentLayout

            let captureBindings =
                captures
                |> List.mapi (fun index capture ->
                    let fieldName = sprintf "capture_%d" index
                    capture.Name, $"env->{fieldName}"
                )
                |> Map.ofList

            let parameterBindings =
                parameters
                |> List.mapi (fun index parameter -> parameter.Name, $"args[{index}]")
                |> Map.ofList

            let closureScope =
                { CurrentModule = scope.CurrentModule
                  Bindings =
                    parameterBindings
                    |> Map.fold (fun acc name expression -> acc.Add(name, expression)) captureBindings }

            let! emittedClosureBody = emitExpression context closureScope closureBody

            let environmentDefinitions =
                if List.isEmpty captures then
                    []
                else
                    [ $"typedef struct {environmentTypeName} {{"
                      yield!
                        captures
                        |> List.mapi (fun index _ -> $"    KValue* capture_{index};")
                      $"}} {environmentTypeName};" ]

            let captureSetup =
                if List.isEmpty captures then
                    [ "(void)raw_env;" ]
                else
                    [ $"{environmentTypeName}* env = ({environmentTypeName}*)raw_env;" ]

            let functionLines =
                [
                    $"static KValue* {emittedFunctionName}(void* raw_env, KValue** args, int argc)"
                    "{"
                    $"    if (argc != {convention.RuntimeArity}) {{"
                    $"        kappa_panic_arity(\"{cStringLiteral emittedFunctionName}\", {convention.RuntimeArity}, argc);"
                    "    }"
                    yield! captureSetup |> indentLines 1
                    yield! emittedClosureBody.Statements |> indentLines 1
                    $"    return {emittedClosureBody.ValueExpression};"
                    "}"
                ]

            context.GeneratedClosures.Add
                { Prototype = $"static KValue* {emittedFunctionName}(void* raw_env, KValue** args, int argc);"
                  SupportDefinitions = environmentDefinitions
                  Definition = joinLines functionLines }

            let localName = freshTemp context binding.Name
            let recursiveScope =
                { scope with
                    Bindings = scope.Bindings.Add(binding.Name, localName) }

            let! prefixScope, prefixStatements =
                prefixBindings
                |> List.fold
                    (fun state (prefixBinding, prefixValue) ->
                        result {
                            let! currentScope, currentStatements = state
                            let! emittedPrefixValue = emitExpression context currentScope prefixValue
                            let prefixLocalName = freshTemp context prefixBinding.Name

                            let nextScope =
                                { currentScope with
                                    Bindings = currentScope.Bindings.Add(prefixBinding.Name, prefixLocalName) }

                            return
                                nextScope,
                                (currentStatements
                                 @ emittedPrefixValue.Statements
                                 @ [ $"KValue* {prefixLocalName} = {emittedPrefixValue.ValueExpression};" ])
                        })
                    (Result.Ok(recursiveScope, []))

            let! closureCreationStatements =
                if List.isEmpty captures then
                    Result.Ok [ $"{localName} = kappa_make_closure(&{emittedFunctionName}, NULL, {convention.RuntimeArity}, \"{cStringLiteral emittedFunctionName}\");" ]
                else
                    result {
                        let environmentVariable = freshTemp context "closure_env"

                        let selfCaptureIndex =
                            captures |> List.tryFindIndex (fun capture -> capture.Name = binding.Name)

                        let captureAssignments =
                            captures
                            |> List.mapi (fun index capture ->
                                if capture.Name = binding.Name then
                                    Result.Ok None
                                else
                                    prefixScope.Bindings
                                    |> Map.tryFind capture.Name
                                    |> resultOfOption $"zig recursive closure could not resolve captured local '{capture.Name}'."
                                    |> Result.map (fun expression -> Some $"{environmentVariable}->capture_{index} = {expression};"))
                            |> List.fold
                                (fun state item ->
                                    result {
                                        let! lines = state
                                        let! next = item
                                        return next |> Option.map (fun line -> lines @ [ line ]) |> Option.defaultValue lines
                                    })
                                (Result.Ok [])

                        let! captureAssignments = captureAssignments

                        let selfAssignment =
                            selfCaptureIndex
                            |> Option.map (fun index -> [ $"{environmentVariable}->capture_{index} = {localName};" ])
                            |> Option.defaultValue []

                        return
                            [ $"{environmentTypeName}* {environmentVariable} = ({environmentTypeName}*)kappa_alloc(sizeof({environmentTypeName}));"
                              $"{localName} = kappa_make_closure(&{emittedFunctionName}, {environmentVariable}, {convention.RuntimeArity}, \"{cStringLiteral emittedFunctionName}\");" ]
                            @ captureAssignments
                            @ selfAssignment
                    }

            let bodyScope =
                { prefixScope with
                    Bindings = prefixScope.Bindings.Add(binding.Name, localName) }

            let! emittedBody = emitExpression context bodyScope body

            return
                { Statements =
                    [ $"KValue* {localName} = NULL;" ]
                    @ prefixStatements
                    @ closureCreationStatements
                    @ emittedBody.Statements
                  ValueExpression = emittedBody.ValueExpression }
        }

    and internal emitProtectedResultLetExpression
        (context: GenerationContext)
        (scope: EmitScope)
        (binding: KBackendParameter)
        (value: KBackendExpression)
        (cleanup: KBackendExpression)
        =
        result {
            let! emittedValue = emitExpression context scope value
            let! emittedCleanup = emitExpression context scope cleanup
            let localName = freshTemp context binding.Name
            let frameName = freshTemp context "panic_frame"
            let normalCleanupValue = freshTemp context "cleanup"
            let panicCleanupValue = freshTemp context "cleanup"

            return
                { Statements =
                    [ $"KValue* {localName} = NULL;"
                      $"KappaPanicFrame {frameName};"
                      $"{frameName}.previous = kappa_panic_frame;"
                      $"kappa_panic_frame = &{frameName};"
                      $"if (setjmp({frameName}.env) == 0)"
                      "{" ]
                    @ indentLines 1 emittedValue.Statements
                    @ [ $"    {localName} = {emittedValue.ValueExpression};"
                        $"    kappa_panic_frame = {frameName}.previous;" ]
                    @ indentLines 1 emittedCleanup.Statements
                    @ [ $"    KValue* {normalCleanupValue} = {emittedCleanup.ValueExpression};"
                        $"    (void){normalCleanupValue};"
                        "}"
                        "else"
                        "{" 
                        $"    kappa_panic_frame = {frameName}.previous;" ]
                    @ indentLines 1 emittedCleanup.Statements
                    @ [ $"    KValue* {panicCleanupValue} = {emittedCleanup.ValueExpression};"
                        $"    (void){panicCleanupValue};"
                        "    kappa_panic(kappa_current_panic_message != NULL ? kappa_current_panic_message : \"abrupt completion\");"
                        "}" ]
                  ValueExpression = localName }
        }

    and internal emitLetExpression
        (context: GenerationContext)
        (scope: EmitScope)
        (binding: KBackendParameter)
        (value: KBackendExpression)
        (body: KBackendExpression)
        =
        result {
            match binding.Name, value, body with
            // KBackendIR currently lowers scheduled exits to this synthetic result let.
            // Emit a C panic frame here so ZigCc preserves cleanup during abrupt exits.
            | "__kappa_scope_result", _, BackendSequence(cleanup, BackendName(BackendLocalName(resultName, _)), _) when resultName = binding.Name ->
                return! emitProtectedResultLetExpression context scope binding value cleanup
            | _ ->
                match tryDecomposeRecursiveClosureValue value with
                | Some(prefixBindings, parameters, captures, environmentLayout, closureBody, convention)
                    when captures |> List.exists (fun capture -> capture.Name = binding.Name) ->
                    return!
                        emitRecursiveClosureLetExpression
                            context
                            scope
                            binding
                            prefixBindings
                            parameters
                            captures
                            environmentLayout
                            closureBody
                            convention
                            body
                | _ ->
                    let! emittedValue = emitExpression context scope value
                    let localName = freshTemp context binding.Name

                    let bodyScope =
                        { scope with
                            Bindings = scope.Bindings.Add(binding.Name, localName) }

                    let! emittedBody = emitExpression context bodyScope body

                    return
                        { Statements =
                            emittedValue.Statements
                            @ [ $"KValue* {localName} = {emittedValue.ValueExpression};" ]
                            @ emittedBody.Statements
                          ValueExpression = emittedBody.ValueExpression }
        }

    and internal emitSequenceExpression (context: GenerationContext) (scope: EmitScope) first second =
        result {
            let! emittedFirst = emitExpression context scope first
            let! emittedSecond = emitExpression context scope second
            let ignoredValue = freshTemp context "seq"

            return
                { Statements =
                    emittedFirst.Statements
                    @ [ $"KValue* {ignoredValue} = {emittedFirst.ValueExpression};" ]
                    @ emittedSecond.Statements
                  ValueExpression = emittedSecond.ValueExpression }
        }

    and internal emitWhileExpression (context: GenerationContext) (scope: EmitScope) condition body =
        result {
            let conditionLabel = freshTemp context "while_cond"
            let exitLabel = freshTemp context "while_exit"
            let resultValue = freshTemp context "while_result"
            let! emittedCondition = emitExpression context scope condition
            let! emittedBody = emitExpression context scope body
            let bodyValue = freshTemp context "while_body"
            let conditionValue = freshTemp context "while_condition"

            return
                { Statements =
                    [ $"KValue* {resultValue} = kappa_unit();"
                      $"{conditionLabel}:;" ]
                    @ emittedCondition.Statements
                    @ [ $"KValue* {conditionValue} = {emittedCondition.ValueExpression};"
                        $"if (!kappa_expect_bool({conditionValue})) goto {exitLabel};" ]
                    @ emittedBody.Statements
                    @ [ $"KValue* {bodyValue} = {emittedBody.ValueExpression};"
                        $"(void){bodyValue};"
                        $"goto {conditionLabel};"
                        $"{exitLabel}:;" ]
                  ValueExpression = resultValue }
        }

    and internal emitDictionaryValueExpression
        (context: GenerationContext)
        (moduleName: string)
        (traitName: string)
        (instanceKey: string)
        =
        let statements, resultValue =
            wrapCallResult
                context
                "dictionary"
                $"kappa_make_dictionary(\"{cStringLiteral moduleName}\", \"{cStringLiteral traitName}\", \"{cStringLiteral instanceKey}\")"

        Result.Ok
            { Statements = statements
              ValueExpression = resultValue }

    and internal emitTraitDispatchExpression
        (context: GenerationContext)
        (scope: EmitScope)
        (traitName: string)
        (memberName: string)
        (dictionary: KBackendExpression)
        (arguments: KBackendExpression list)
        =
        result {
            let! emittedDictionary = emitExpression context scope dictionary
            let! emittedArguments = emitExpressions context scope arguments

            let argumentStatements =
                emittedArguments |> List.collect (fun emitted -> emitted.Statements)

            let argumentValues =
                emittedArguments |> List.map (fun emitted -> emitted.ValueExpression)

            let argumentArrayStatements, argumentArray =
                buildArgumentArray context argumentValues

            let statements, resultValue =
                wrapCallResult
                    context
                    "trait_dispatch"
                    $"{traitDispatchFunctionName traitName memberName}({emittedDictionary.ValueExpression}, {argumentArray}, {argumentValues.Length})"

            return
                { Statements =
                    emittedDictionary.Statements
                    @ argumentStatements
                    @ argumentArrayStatements
                    @ statements
                  ValueExpression = resultValue }
        }

    and internal emitConstructDataExpression (context: GenerationContext) (scope: EmitScope) moduleName typeName tag fields =
        result {
            let! emittedFields = emitExpressions context scope fields
            let! typeId = lookupTypeId context moduleName typeName

            let fieldStatements =
                emittedFields |> List.collect (fun emitted -> emitted.Statements)

            let fieldValues =
                emittedFields |> List.map (fun emitted -> emitted.ValueExpression)

            let argumentArrayStatements, argumentArray =
                buildArgumentArray context fieldValues

            let resultValue = freshTemp context "ctor"

            let constructorCall =
                if List.isEmpty fieldValues then
                    $"kappa_make_data({typeId}, {tag}, 0, NULL)"
                else
                    $"kappa_make_data({typeId}, {tag}, {fieldValues.Length}, {argumentArray})"

            return
                { Statements =
                    fieldStatements
                    @ argumentArrayStatements
                    @ [ $"KValue* {resultValue} = {constructorCall};" ]
                  ValueExpression = resultValue }
        }

    and internal emitIntrinsicCall (context: GenerationContext) (scope: EmitScope) bindingName arguments =
        result {
            let! emittedArguments = emitExpressions context scope arguments

            let argumentStatements =
                emittedArguments |> List.collect (fun emitted -> emitted.Statements)

            let argumentValues =
                emittedArguments |> List.map (fun emitted -> emitted.ValueExpression)

            let binaryIntCall prefix helper =
                match argumentValues with
                | [ left; right ] ->
                    let statements, resultValue =
                        wrapCallResult context prefix $"{helper}({left}, {right})"

                    Result.Ok
                        { Statements = argumentStatements @ statements
                          ValueExpression = resultValue }
                | _ ->
                    Result.Error $"zig intrinsic '{bindingName}' expected exactly 2 arguments."

            let unaryIntCall prefix helper =
                match argumentValues with
                | [ value ] ->
                    let statements, resultValue =
                        wrapCallResult context prefix $"{helper}({value})"

                    Result.Ok
                        { Statements = argumentStatements @ statements
                          ValueExpression = resultValue }
                | _ ->
                    Result.Error $"zig intrinsic '{bindingName}' expected exactly 1 argument."

            let binaryBoolCall prefix helper =
                match argumentValues with
                | [ left; right ] ->
                    let statements, resultValue =
                        wrapCallResult context prefix $"{helper}({left}, {right})"

                    Result.Ok
                        { Statements = argumentStatements @ statements
                          ValueExpression = resultValue }
                | _ ->
                    Result.Error $"zig intrinsic '{bindingName}' expected exactly 2 arguments."

            let unaryBoolCall prefix helper =
                match argumentValues with
                | [ value ] ->
                    let statements, resultValue =
                        wrapCallResult context prefix $"{helper}({value})"

                    Result.Ok
                        { Statements = argumentStatements @ statements
                          ValueExpression = resultValue }
                | _ ->
                    Result.Error $"zig intrinsic '{bindingName}' expected exactly 1 argument."

            match bindingName with
            | "+" -> return! binaryIntCall "add" "kappa_int_add"
            | "-" -> return! binaryIntCall "sub" "kappa_int_subtract"
            | "*" -> return! binaryIntCall "mul" "kappa_int_multiply"
            | "/" -> return! binaryIntCall "div" "kappa_int_divide"
            | "==" -> return! binaryBoolCall "eq" "kappa_value_equal"
            | "!=" -> return! binaryBoolCall "neq" "kappa_value_not_equal"
            | "<" -> return! binaryBoolCall "lt" "kappa_int_less"
            | "<=" -> return! binaryBoolCall "lte" "kappa_int_less_equal"
            | ">" -> return! binaryBoolCall "gt" "kappa_int_greater"
            | ">=" -> return! binaryBoolCall "gte" "kappa_int_greater_equal"
            | "negate" -> return! unaryIntCall "neg" "kappa_int_negate"
            | "not" -> return! unaryBoolCall "not" "kappa_bool_not"
            | "and" -> return! binaryBoolCall "and" "kappa_bool_and"
            | "or" -> return! binaryBoolCall "or" "kappa_bool_or"
            | "True" ->
                if not (List.isEmpty argumentValues) then
                    return! Result.Error "zig intrinsic 'True' does not take arguments."
                else
                    return
                        { Statements = argumentStatements
                          ValueExpression = "kappa_box_bool(1)" }
            | "False" ->
                if not (List.isEmpty argumentValues) then
                    return! Result.Error "zig intrinsic 'False' does not take arguments."
                else
                    return
                        { Statements = argumentStatements
                          ValueExpression = "kappa_box_bool(0)" }
            | "::" ->
                match argumentValues with
                | [ head; tail ] ->
                    let! listTypeId = lookupTypeId context Stdlib.PreludeModuleText "List"
                    let argumentArrayStatements, argumentArray =
                        buildArgumentArray context [ head; tail ]

                    let resultValue = freshTemp context "cons"

                    return
                        { Statements =
                            argumentStatements
                            @ argumentArrayStatements
                            @ [ $"KValue* {resultValue} = kappa_make_data({listTypeId}, 1, 2, {argumentArray});" ]
                          ValueExpression = resultValue }
                | _ ->
                    return! Result.Error "zig intrinsic '::' expected exactly 2 arguments."
            | "pure" ->
                match argumentValues with
                | [ value ] ->
                    let statements, resultValue =
                        wrapCallResult context "pure" $"kappa_pure({value})"

                    return
                        { Statements = argumentStatements @ statements
                          ValueExpression = resultValue }
                | _ ->
                    return! Result.Error "zig intrinsic 'pure' expected exactly 1 argument."
            | ">>" ->
                match argumentValues with
                | [ left; right ] ->
                    let statements, resultValue =
                        wrapCallResult context "then" $"kappa_then({left}, {right})"

                    return
                        { Statements = argumentStatements @ statements
                          ValueExpression = resultValue }
                | _ ->
                    return! Result.Error "zig intrinsic '>>' expected exactly 2 arguments."
            | ">>=" ->
                match argumentValues with
                | [ value; binder ] ->
                    let statements, resultValue =
                        wrapCallResult context "bind" $"kappa_bind({value}, {binder})"

                    return
                        { Statements = argumentStatements @ statements
                          ValueExpression = resultValue }
                | _ ->
                    return! Result.Error "zig intrinsic '>>=' expected exactly 2 arguments."
            | "printInt" ->
                match argumentValues with
                | [ value ] ->
                    let statements, resultValue =
                        wrapCallResult context "print_int" $"kappa_builtin_print_int({value})"

                    return
                        { Statements = argumentStatements @ statements
                          ValueExpression = resultValue }
                | _ ->
                    return! Result.Error "zig intrinsic 'printInt' expected exactly 1 argument."
            | "printString" ->
                match argumentValues with
                | [ value ] ->
                    let statements, resultValue =
                        wrapCallResult context "print_string" $"kappa_builtin_print_string({value})"

                    return
                        { Statements = argumentStatements @ statements
                          ValueExpression = resultValue }
                | _ ->
                    return! Result.Error "zig intrinsic 'printString' expected exactly 1 argument."
            | "print" ->
                match argumentValues with
                | [ value ] ->
                    let statements, resultValue =
                        wrapCallResult context "print" $"kappa_builtin_print({value}, 0)"

                    return
                        { Statements = argumentStatements @ statements
                          ValueExpression = resultValue }
                | _ ->
                    return! Result.Error "zig intrinsic 'print' expected exactly 1 argument."
            | "println" ->
                match argumentValues with
                | [ value ] ->
                    let statements, resultValue =
                        wrapCallResult context "println" $"kappa_builtin_print({value}, 1)"

                    return
                        { Statements = argumentStatements @ statements
                          ValueExpression = resultValue }
                | _ ->
                    return! Result.Error "zig intrinsic 'println' expected exactly 1 argument."
            | "primitiveIntToString" ->
                match argumentValues with
                | [ value ] ->
                    let statements, resultValue =
                        wrapCallResult context "int_to_string" $"kappa_int_to_string({value})"

                    return
                        { Statements = argumentStatements @ statements
                          ValueExpression = resultValue }
                | _ ->
                    return! Result.Error "zig intrinsic 'primitiveIntToString' expected exactly 1 argument."
            | "unsafeConsume" ->
                match argumentValues with
                | [ _ ] ->
                    return
                        { Statements = argumentStatements
                          ValueExpression = "kappa_unit()" }
                | _ ->
                    return! Result.Error "zig intrinsic 'unsafeConsume' expected exactly 1 argument."
            | "openFile" ->
                match argumentValues with
                | [ _ ] ->
                    let statements, resultValue =
                        wrapCallResult context "open_file" "kappa_box_string(\"<file:data.txt>\")"

                    return
                        { Statements = argumentStatements @ statements
                          ValueExpression = resultValue }
                | _ ->
                    return! Result.Error "zig intrinsic 'openFile' expected exactly 1 argument."
            | "primitiveReadData"
            | "readData" ->
                match argumentValues with
                | [ _ ] ->
                    let statements, resultValue =
                        wrapCallResult context "read_data" "kappa_box_string(\"chunk\")"

                    return
                        { Statements = argumentStatements @ statements
                          ValueExpression = resultValue }
                | _ ->
                    return! Result.Error $"zig intrinsic '{bindingName}' expected exactly 1 argument."
            | "primitiveCloseFile" ->
                match argumentValues with
                | [ _ ] ->
                    let closeText = freshTemp context "close_text"
                    let statements, resultValue =
                        wrapCallResult context "close_file" $"kappa_builtin_print_string({closeText})"

                    return
                        { Statements =
                            argumentStatements
                            @ [ $"KValue* {closeText} = kappa_box_string(\"closed\");" ]
                            @ statements
                          ValueExpression = resultValue }
                | _ ->
                    return! Result.Error "zig intrinsic 'primitiveCloseFile' expected exactly 1 argument."
            | "newRef" ->
                match argumentValues with
                | [ value ] ->
                    let statements, resultValue =
                        wrapCallResult context "new_ref" $"kappa_make_ref({value})"

                    return
                        { Statements = argumentStatements @ statements
                          ValueExpression = resultValue }
                | _ ->
                    return! Result.Error "zig intrinsic 'newRef' expected exactly 1 argument."
            | "readRef" ->
                match argumentValues with
                | [ value ] ->
                    let statements, resultValue =
                        wrapCallResult context "read_ref" $"kappa_ref_read({value})"

                    return
                        { Statements = argumentStatements @ statements
                          ValueExpression = resultValue }
                | _ ->
                    return! Result.Error "zig intrinsic 'readRef' expected exactly 1 argument."
            | "writeRef" ->
                match argumentValues with
                | [ referenceValue; value ] ->
                    let statements, resultValue =
                        wrapCallResult context "write_ref" $"kappa_ref_write({referenceValue}, {value})"

                    return
                        { Statements = argumentStatements @ statements
                          ValueExpression = resultValue }
                | _ ->
                    return! Result.Error "zig intrinsic 'writeRef' expected exactly 2 arguments."
            | other ->
                return! Result.Error $"zig does not yet support intrinsic '{other}'."
        }

    and internal emitCallExpression (context: GenerationContext) (scope: EmitScope) callee arguments =
        match callee with
        | BackendName(BackendGlobalBindingName(moduleName, bindingName, _)) ->
            result {
                let! emittedArguments = emitExpressions context scope arguments

                let argumentStatements =
                    emittedArguments |> List.collect (fun emitted -> emitted.Statements)

                let argumentValues =
                    emittedArguments |> List.map (fun emitted -> emitted.ValueExpression)

                let argumentArrayStatements, argumentArray =
                    buildArgumentArray context argumentValues

                let! emittedName =
                    lookupFunctionName context moduleName bindingName
                    |> resultOfOption $"zig could not resolve callee '{moduleName}.{bindingName}'."

                let resultValue = freshTemp context "call"

                return
                    { Statements =
                        argumentStatements
                        @ argumentArrayStatements
                        @ [ $"KValue* {resultValue} = {emittedName}(NULL, {argumentArray}, {argumentValues.Length});" ]
                      ValueExpression = resultValue }
            }
        | BackendName(BackendIntrinsicName(_, bindingName, _)) ->
            emitIntrinsicCall context scope bindingName arguments
        | BackendName(BackendConstructorName(moduleName, typeName, _, tag, arity, _)) ->
            if arity <> arguments.Length then
                Result.Error
                    $"zig expected constructor '{moduleName}.{typeName}' to receive {arity} argument(s), but received {arguments.Length}."
            else
                emitConstructDataExpression context scope moduleName typeName tag arguments
        | _ ->
            result {
                let! emittedCallee = emitExpression context scope callee
                let! emittedArguments = emitExpressions context scope arguments

                let argumentStatements =
                    emittedArguments |> List.collect (fun emitted -> emitted.Statements)

                let argumentValues =
                    emittedArguments |> List.map (fun emitted -> emitted.ValueExpression)

                let argumentArrayStatements, argumentArray =
                    buildArgumentArray context argumentValues

                let resultValue = freshTemp context "invoke"

                return
                    { Statements =
                        emittedCallee.Statements
                        @ argumentStatements
                        @ argumentArrayStatements
                        @ [ $"KValue* {resultValue} = kappa_apply_value({emittedCallee.ValueExpression}, {argumentArray}, {argumentValues.Length});" ]
                      ValueExpression = resultValue }
            }

    and internal emitPatternCase
        (context: GenerationContext)
        (scope: EmitScope)
        (scrutineeExpression: string)
        (pattern: KBackendPattern)
        (emitSuccess: EmitScope -> Result<string list, string>)
        =
        match pattern with
        | BackendWildcardPattern ->
            emitSuccess scope
        | BackendBindPattern binding ->
            let nextScope =
                { scope with
                    Bindings = scope.Bindings.Add(binding.Name, scrutineeExpression) }

            emitSuccess nextScope
        | BackendLiteralPattern(literal, _) ->
            result {
                let! successLines = emitSuccess scope
                return
                    [ $"if ({literalGuard literal scrutineeExpression}) {{" ]
                    @ (successLines |> indentLines 1)
                    @ [ "}" ]
            }
        | BackendConstructorPattern(moduleName, typeName, _, tag, fieldPatterns) ->
            result {
                let! typeId = lookupTypeId context moduleName typeName

                let rec emitFieldPatterns currentScope fieldIndex remainingPatterns =
                    match remainingPatterns with
                    | [] ->
                        emitSuccess currentScope
                    | fieldPattern :: tail ->
                        result {
                            let fieldValue = freshTemp context "field"
                            let! nestedLines =
                                emitPatternCase context currentScope fieldValue fieldPattern (fun nextScope ->
                                    emitFieldPatterns nextScope (fieldIndex + 1) tail)

                            return
                                [ $"KValue* {fieldValue} = kappa_get_field({scrutineeExpression}, {fieldIndex});" ]
                                @ nestedLines
                        }

                let! fieldLines = emitFieldPatterns scope 0 fieldPatterns

                return
                    [ $"if (kappa_is_ctor({scrutineeExpression}, {typeId}, {tag})) {{" ]
                    @ (fieldLines |> indentLines 1)
                    @ [ "}" ]
            }

    and internal emitMatchExpression (context: GenerationContext) (scope: EmitScope) scrutinee cases =
        result {
            let! emittedScrutinee = emitExpression context scope scrutinee

            let scrutineeValue = freshTemp context "match_scrutinee"
            let resultValue = freshTemp context "match_result"
            let endLabel = freshTemp context "match_end"

            let! caseLines =
                cases
                |> List.fold
                    (fun state case ->
                        result {
                            let! accumulated = state
                            let nextCaseLabel = freshTemp context "match_next_case"
                            let emittedCaseResult =
                                emitPatternCase context scope scrutineeValue case.Pattern (fun caseScope ->
                                    result {
                                        let! emittedGuard =
                                            match case.Guard with
                                            | Some guard ->
                                                emitExpression context caseScope guard
                                                |> Result.map Some
                                            | None ->
                                                Result.Ok None

                                        let! emittedBody = emitExpression context caseScope case.Body

                                        let guardStatements =
                                            match emittedGuard with
                                            | Some emittedGuard ->
                                                let guardValue = freshTemp context "match_guard"

                                                emittedGuard.Statements
                                                @ [ $"KValue* {guardValue} = {emittedGuard.ValueExpression};"
                                                    $"if (!kappa_expect_bool({guardValue})) {{"
                                                    $"    goto {nextCaseLabel};"
                                                    "}" ]
                                            | None ->
                                                []

                                        return
                                            guardStatements
                                            @ emittedBody.Statements
                                            @ [ $"{resultValue} = {emittedBody.ValueExpression};"
                                                $"goto {endLabel};" ]
                                    })

                            let! emittedCase =
                                emittedCaseResult
                                |> Result.map (fun emittedCase ->
                                    match case.Guard with
                                    | Some _ ->
                                        emittedCase @ [ $"{nextCaseLabel}:;" ]
                                    | None ->
                                        emittedCase)

                            return accumulated @ emittedCase
                        })
                    (Result.Ok [])

            return
                { Statements =
                    emittedScrutinee.Statements
                    @ [ $"KValue* {scrutineeValue} = {emittedScrutinee.ValueExpression};"
                        $"KValue* {resultValue} = NULL;" ]
                    @ caseLines
                    @ [ $"kappa_panic(\"Non-exhaustive match while evaluating module '{cStringLiteral scope.CurrentModule}'.\");"
                        $"{endLabel}:;" ]
                  ValueExpression = resultValue }
        }

    let internal emitFunctionDefinition (context: GenerationContext) (moduleName: string) (functionDump: KBackendFunction) =
        result {
            let! body =
                functionDump.Body
                |> resultOfOption $"zig requires a body for '{moduleName}.{functionDump.Name}'."

            let emittedName =
                context.FunctionNames[moduleName, functionDump.Name]

            let debugName = cStringLiteral $"{moduleName}.{functionDump.Name}"

            let parameterBindings =
                functionDump.Parameters
                |> List.mapi (fun index parameter -> parameter.Name, $"args[{index}]")
                |> Map.ofList

            let scope =
                { CurrentModule = moduleName
                  Bindings = parameterBindings }

            let! emittedBody = emitExpression context scope body

            let definitionLines =
                [
                    $"static KValue* {emittedName}(void* env, KValue** args, int argc)"
                    "{"
                    "    (void)env;"
                    $"    if (argc != {functionDump.Parameters.Length}) {{"
                    $"        kappa_panic_arity(\"{debugName}\", {functionDump.Parameters.Length}, argc);"
                    "    }"
                    yield! emittedBody.Statements |> indentLines 1
                    $"    return {emittedBody.ValueExpression};"
                    "}"
                ]

            return
                { Prototype = $"static KValue* {emittedName}(void* env, KValue** args, int argc);"
                  SupportDefinitions = []
                  Definition = joinLines definitionLines }
        }
