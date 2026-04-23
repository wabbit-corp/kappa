namespace Kappa.Compiler

// Lowers KBackendIR expressions and bindings into generated C fragments for the zig backend.
module internal ZigCcBackendEmit =
    open ZigCcBackendSupport

    let rec internal emitExpression (context: GenerationContext) (scope: EmitScope) (expression: KBackendExpression) : Result<EmittedExpression, string> =
        match expression with
        | BackendLiteral(literal, _) ->
            Result.Ok
                { Statements = []
                  ValueExpression = literalToBoxedExpression literal }
        | BackendName resolvedName ->
            emitNameExpression context scope resolvedName
        | BackendClosure(parameters, captures, environmentLayout, body, convention, _) ->
            emitClosureExpression context scope parameters captures environmentLayout body convention
        | BackendIfThenElse(condition, whenTrue, whenFalse, _) ->
            emitIfExpression context scope condition whenTrue whenFalse
        | BackendMatch(scrutinee, cases, _) ->
            emitMatchExpression context scope scrutinee cases
        | BackendExecute(expression, _) ->
            emitExecuteExpression context scope expression
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
                            |> resultOfOption $"zig could not resolve captured local '{capture.Name}'."

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
                    @ [ $"KValue* {resultValue} = {emittedExpression.ValueExpression};" ]
                  ValueExpression = resultValue }
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
            match binding.Name, body with
            // KBackendIR currently lowers scheduled exits to this synthetic result let.
            // Emit a C panic frame here so ZigCc preserves cleanup during abrupt exits.
            | "__kappa_scope_result", BackendSequence(cleanup, BackendName(BackendLocalName(resultName, _)), _) when resultName = binding.Name ->
                return! emitProtectedResultLetExpression context scope binding value cleanup
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
                    return
                        { Statements = argumentStatements
                          ValueExpression = value }
                | _ ->
                    return! Result.Error "zig intrinsic 'pure' expected exactly 1 argument."
            | ">>" ->
                match argumentValues with
                | [ _; value ] ->
                    return
                        { Statements = argumentStatements
                          ValueExpression = value }
                | _ ->
                    return! Result.Error "zig intrinsic '>>' expected exactly 2 arguments."
            | ">>=" ->
                match argumentValues with
                | [ value; binder ] ->
                    let argumentArrayStatements, argumentArray =
                        buildArgumentArray context [ value ]

                    let resultValue = freshTemp context "bind"

                    return
                        { Statements =
                            argumentStatements
                            @ argumentArrayStatements
                            @ [ $"KValue* {resultValue} = kappa_invoke_value({binder}, {argumentArray}, 1);" ]
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
                        @ [ $"KValue* {resultValue} = kappa_invoke_value({emittedCallee.ValueExpression}, {argumentArray}, {argumentValues.Length});" ]
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
