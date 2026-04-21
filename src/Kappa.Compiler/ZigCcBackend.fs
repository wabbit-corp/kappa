namespace Kappa.Compiler

open System
open System.Globalization
open System.IO

module ZigCcBackend =
    type private ResultBuilder() =
        member _.Bind(result, binder) = Result.bind binder result
        member _.Return(value) = Result.Ok value
        member _.ReturnFrom(result) = result
        member _.Zero() = Result.Ok()
        member _.Delay(generator) = generator
        member _.Run(generator) = generator()
        member _.Combine(first, second) =
            first |> Result.bind (fun () -> second())

    type private GeneratedCFunction =
        { Prototype: string
          SupportDefinitions: string list
          Definition: string }

    type private EmitScope =
        { CurrentModule: string
          Bindings: Map<string, string> }

    type private EmittedExpression =
        { Statements: string list
          ValueExpression: string }

    type private GenerationContext =
        { Workspace: WorkspaceCompilation
          Functions: Map<string * string, KBackendFunction>
          FunctionNames: Map<string * string, string>
          DataTypeIds: Map<string * string, string>
          GeneratedClosures: ResizeArray<GeneratedCFunction>
          mutable NextTempId: int
          mutable NextClosureId: int }

    let private result = ResultBuilder()

    let private aggregateDiagnostics diagnostics =
        diagnostics
        |> List.map (fun diagnostic -> diagnostic.Message)
        |> String.concat Environment.NewLine

    let private resultOfOption errorMessage optionValue =
        match optionValue with
        | Some value -> Result.Ok value
        | None -> Result.Error errorMessage

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

    let private cStringLiteral (value: string) =
        value
        |> Seq.map (fun ch ->
            match ch with
            | '\\' -> "\\\\"
            | '"' -> "\\\""
            | '\r' -> "\\r"
            | '\n' -> "\\n"
            | '\t' -> "\\t"
            | ch when Char.IsControl(ch) -> $"\\x{int ch:x2}"
            | ch -> string ch)
        |> String.concat ""

    let private indentLines level (lines: string list) =
        let prefix = String.replicate (level * 4) " "
        lines |> List.map (fun line -> prefix + line)

    let private joinLines (lines: string list) =
        String.concat Environment.NewLine lines

    let private executablePath (directory: string) (baseName: string) =
        if OperatingSystem.IsWindows() then
            Path.Combine(directory, $"{baseName}.exe")
        else
            Path.Combine(directory, baseName)

    let private functionName moduleName bindingName =
        $"kappa_module_{sanitizeIdentifier moduleName}_{sanitizeIdentifier bindingName}"

    let private typeIdName moduleName typeName =
        $"KTYPE_{sanitizeIdentifier moduleName}_{sanitizeIdentifier typeName}"

    let private traitDispatchFunctionName traitName memberName =
        $"kappa_trait_dispatch_{sanitizeIdentifier traitName}_{sanitizeIdentifier memberName}"

    let private freshTemp (context: GenerationContext) prefix =
        let value = context.NextTempId
        context.NextTempId <- context.NextTempId + 1
        $"{sanitizeIdentifier prefix}_{value}"

    let private freshClosureFunctionName (context: GenerationContext) (moduleName: string) =
        let value = context.NextClosureId
        context.NextClosureId <- context.NextClosureId + 1
        $"kappa_closure_{sanitizeIdentifier moduleName}_{value}"

    let private resolveEntryPoint (workspace: WorkspaceCompilation) (entryPoint: string) =
        let segments =
            entryPoint.Split('.', StringSplitOptions.RemoveEmptyEntries)
            |> Array.toList

        let tryMatchBinding moduleName bindingName =
            workspace.KBackendIR
            |> List.tryFind (fun moduleDump -> String.Equals(moduleDump.Name, moduleName, StringComparison.Ordinal))
            |> Option.bind (fun moduleDump ->
                moduleDump.Functions
                |> List.tryFind (fun binding ->
                    String.Equals(binding.Name, bindingName, StringComparison.Ordinal)
                    && not binding.Intrinsic
                    && List.isEmpty binding.Parameters))

        match segments with
        | [] ->
            Result.Error "Expected a binding name to run."
        | [ bindingName ] ->
            let matches =
                workspace.KBackendIR
                |> List.choose (fun moduleDump ->
                    moduleDump.Functions
                    |> List.tryFind (fun binding ->
                        String.Equals(binding.Name, bindingName, StringComparison.Ordinal)
                        && not binding.Intrinsic
                        && List.isEmpty binding.Parameters)
                    |> Option.map (fun binding -> moduleDump.Name, binding.Name))

            match matches with
            | [] ->
                Result.Error $"No zero-argument binding named '{bindingName}' was found for zig."
            | [ moduleName, resolvedBindingName ] ->
                Result.Ok(moduleName, resolvedBindingName)
            | _ ->
                Result.Error $"Binding name '{bindingName}' is ambiguous. Use a fully qualified name."
        | _ ->
            let moduleName = segments |> List.take (segments.Length - 1) |> String.concat "."
            let bindingName = List.last segments

            match tryMatchBinding moduleName bindingName with
            | Some _ ->
                Result.Ok(moduleName, bindingName)
            | None ->
                Result.Error $"zig requires a zero-argument binding named '{bindingName}' in module '{moduleName}'."

    let private buildContext (workspace: WorkspaceCompilation) =
        let functions =
            workspace.KBackendIR
            |> List.collect (fun moduleDump ->
                moduleDump.Functions
                |> List.filter (fun functionDump -> not functionDump.Intrinsic)
                |> List.map (fun functionDump -> (moduleDump.Name, functionDump.Name), functionDump))
            |> Map.ofList

        let functionNames =
            functions
            |> Map.toList
            |> List.map (fun ((moduleName, bindingName), _) ->
                (moduleName, bindingName), functionName moduleName bindingName)
            |> Map.ofList

        let dataTypeIds =
            workspace.KBackendIR
            |> List.collect (fun moduleDump ->
                moduleDump.DataLayouts
                |> List.map (fun layout ->
                    (moduleDump.Name, layout.TypeName), typeIdName moduleDump.Name layout.TypeName))
            |> Map.ofList

        { Workspace = workspace
          Functions = functions
          FunctionNames = functionNames
          DataTypeIds = dataTypeIds
          GeneratedClosures = ResizeArray()
          NextTempId = 0
          NextClosureId = 0 }

    let private lookupFunction (context: GenerationContext) moduleName bindingName =
        context.Functions |> Map.tryFind (moduleName, bindingName)

    let private lookupFunctionName (context: GenerationContext) moduleName bindingName =
        context.FunctionNames |> Map.tryFind (moduleName, bindingName)

    let private lookupTypeId (context: GenerationContext) moduleName typeName =
        context.DataTypeIds
        |> Map.tryFind (moduleName, typeName)
        |> resultOfOption $"zig could not resolve runtime type layout '{moduleName}.{typeName}'."

    let private literalToBoxedExpression literal =
        match literal with
        | Integer value ->
            $"kappa_box_int({value}LL)"
        | Float value ->
            let text = value.ToString("R", CultureInfo.InvariantCulture)
            $"kappa_box_float({text})"
        | String value ->
            $"kappa_box_string(\"{cStringLiteral value}\")"
        | Character value ->
            $"kappa_box_char({int value})"
        | Unit ->
            "kappa_unit()"

    let private literalGuard literal expression =
        match literal with
        | Integer value ->
            $"kappa_is_int_value({expression}, {value}LL)"
        | Float value ->
            let text = value.ToString("R", CultureInfo.InvariantCulture)
            $"kappa_is_float_value({expression}, {text})"
        | String value ->
            $"kappa_is_string_value({expression}, \"{cStringLiteral value}\")"
        | Character value ->
            $"kappa_is_char_value({expression}, {int value})"
        | Unit ->
            $"kappa_is_unit({expression})"

    let private buildArgumentArray (context: GenerationContext) (arguments: string list) =
        if List.isEmpty arguments then
            [], "NULL"
        else
            let arrayName = freshTemp context "args"
            let initializerText = String.concat ", " arguments
            [ $"KValue* {arrayName}[{arguments.Length}] = {{ {initializerText} }};" ], arrayName

    let private wrapCallResult (context: GenerationContext) prefix callExpression =
        let resultName = freshTemp context prefix
        [ $"KValue* {resultName} = {callExpression};" ], resultName

    let rec private emitExpression (context: GenerationContext) (scope: EmitScope) (expression: KBackendExpression) : Result<EmittedExpression, string> =
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

    and private emitExpressions (context: GenerationContext) (scope: EmitScope) (expressions: KBackendExpression list) : Result<EmittedExpression list, string> =
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

    and private emitNameExpression (context: GenerationContext) (scope: EmitScope) resolvedName =
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

    and private emitClosureExpression
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

    and private emitIfExpression (context: GenerationContext) (scope: EmitScope) condition whenTrue whenFalse =
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

    and private emitExecuteExpression (context: GenerationContext) (scope: EmitScope) expression =
        result {
            let! emittedExpression = emitExpression context scope expression
            let resultValue = freshTemp context "execute"

            return
                { Statements =
                    emittedExpression.Statements
                    @ [ $"KValue* {resultValue} = {emittedExpression.ValueExpression};" ]
                  ValueExpression = resultValue }
        }

    and private emitLetExpression
        (context: GenerationContext)
        (scope: EmitScope)
        (binding: KBackendParameter)
        (value: KBackendExpression)
        (body: KBackendExpression)
        =
        result {
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

    and private emitSequenceExpression (context: GenerationContext) (scope: EmitScope) first second =
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

    and private emitWhileExpression (context: GenerationContext) (scope: EmitScope) condition body =
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

    and private emitDictionaryValueExpression
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

    and private emitTraitDispatchExpression
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

    and private emitConstructDataExpression (context: GenerationContext) (scope: EmitScope) moduleName typeName tag fields =
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

    and private emitIntrinsicCall (context: GenerationContext) (scope: EmitScope) bindingName arguments =
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

    and private emitCallExpression (context: GenerationContext) (scope: EmitScope) callee arguments =
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

    and private emitPatternCase
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

    and private emitMatchExpression (context: GenerationContext) (scope: EmitScope) scrutinee cases =
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
                            let! emittedCase =
                                emitPatternCase context scope scrutineeValue case.Pattern (fun caseScope ->
                                    result {
                                        let! emittedBody = emitExpression context caseScope case.Body
                                        return
                                            emittedBody.Statements
                                            @ [ $"{resultValue} = {emittedBody.ValueExpression};"
                                                $"goto {endLabel};" ]
                                    })

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

    let private emitFunctionDefinition (context: GenerationContext) (moduleName: string) (functionDump: KBackendFunction) =
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

    let private emitTraitDispatchFunctions (context: GenerationContext) =
        let dispatchEntries =
            context.Workspace.Documents
            |> List.collect (fun document ->
                match document.ModuleName with
                | None ->
                    []
                | Some moduleSegments ->
                    let moduleName = SyntaxFacts.moduleNameToText moduleSegments

                    document.Syntax.Declarations
                    |> List.choose (function
                        | InstanceDeclarationNode declaration ->
                            Some(moduleName, declaration)
                        | _ ->
                            None)
                    |> List.collect (fun (instanceModuleName, declaration) ->
                        let instanceKey = TraitRuntime.instanceKeyFromTokens declaration.HeaderTokens

                        declaration.Members
                        |> List.choose (fun memberDeclaration ->
                            memberDeclaration.Name
                            |> Option.bind (fun memberName ->
                                let hiddenBindingName =
                                    TraitRuntime.instanceMemberBindingName declaration.TraitName instanceKey memberName

                                lookupFunctionName context instanceModuleName hiddenBindingName
                                |> Option.map (fun emittedFunctionName ->
                                    declaration.TraitName, memberName, instanceModuleName, instanceKey, emittedFunctionName)))))

        dispatchEntries
        |> List.groupBy (fun (traitName, memberName, _, _, _) -> traitName, memberName)
        |> List.collect (fun ((traitName, memberName), entries) ->
            let functionName = traitDispatchFunctionName traitName memberName
            let missingDictionaryMessage =
                cStringLiteral $"missing dictionary value for trait dispatch {traitName}.{memberName}"

            let missingInstanceMessage =
                cStringLiteral $"no instance available for trait dispatch {traitName}.{memberName}"

            [
                $"static KValue* {functionName}(KValue* dictionary, KValue** args, int argc)"
                "{"
                "    if (dictionary == NULL || dictionary->tag != K_TAG_DICTIONARY)"
                "    {"
                $"        kappa_panic(\"{missingDictionaryMessage}\");"
                "    }"
                ""
                yield!
                    entries
                    |> List.collect (fun (_, _, instanceModuleName, instanceKey, emittedFunctionName) ->
                        [
                            $"    if (strcmp(dictionary->as.dictionary_value.module_name, \"{cStringLiteral instanceModuleName}\") == 0"
                            $"        && strcmp(dictionary->as.dictionary_value.instance_key, \"{cStringLiteral instanceKey}\") == 0)"
                            "    {"
                            $"        return {emittedFunctionName}(NULL, args, argc);"
                            "    }"
                            ""
                        ])
                $"    kappa_panic(\"{missingInstanceMessage}\");"
                "    return kappa_unit();"
                "}"
                ""
            ])

    let private emitRuntimePrelude (context: GenerationContext) =
        let preludeBoolTypeId = typeIdName Stdlib.PreludeModuleText "Bool"

        let typeIdLines =
            if Map.isEmpty context.DataTypeIds then
                [ "enum { KTYPE_unused = 0 };" ]
            else
                [ "enum"
                  "{"
                  yield!
                    context.DataTypeIds
                    |> Map.toList
                    |> List.mapi (fun index ((_, _), typeId) -> $"    {typeId} = {index + 1},")
                  "};" ]

        let runtimeLines =
            [
                "#include <stdint.h>"
                "#include <stdio.h>"
                "#include <stdlib.h>"
                "#include <string.h>"
                ""
                "typedef struct KValue KValue;"
                "typedef KValue* (*KappaFunction)(void* env, KValue** args, int argc);"
                ""
                "typedef enum KValueTag"
                "{"
                "    K_TAG_INT = 1,"
                "    K_TAG_FLOAT = 2,"
                "    K_TAG_BOOL = 3,"
                "    K_TAG_STRING = 4,"
                "    K_TAG_CHAR = 5,"
                "    K_TAG_UNIT = 6,"
                "    K_TAG_DATA = 7,"
                "    K_TAG_CLOSURE = 8,"
                "    K_TAG_REF = 9,"
                "    K_TAG_DICTIONARY = 10"
                "} KValueTag;"
                ""
                "typedef struct KDataValue"
                "{"
                "    int type_id;"
                "    int ctor_tag;"
                "    int field_count;"
                "    KValue** fields;"
                "} KDataValue;"
                ""
                "typedef struct KClosureValue"
                "{"
                "    KappaFunction function;"
                "    void* env;"
                "    int arity;"
                "    const char* debug_name;"
                "} KClosureValue;"
                ""
                "typedef struct KRefValue"
                "{"
                "    KValue* value;"
                "} KRefValue;"
                ""
                "typedef struct KDictionaryValue"
                "{"
                "    const char* module_name;"
                "    const char* trait_name;"
                "    const char* instance_key;"
                "} KDictionaryValue;"
                ""
                "struct KValue"
                "{"
                "    KValueTag tag;"
                "    union"
                "    {"
                "        int64_t int_value;"
                "        double float_value;"
                "        int bool_value;"
                "        const char* string_value;"
                "        int32_t char_value;"
                "        KDataValue data_value;"
                "        KClosureValue closure_value;"
                "        KRefValue ref_value;"
                "        KDictionaryValue dictionary_value;"
                "    } as;"
                "};"
                ""
            ]
            @ typeIdLines
            @ [
                ""
                "static void* kappa_alloc(size_t size)"
                "{"
                "    void* memory = malloc(size);"
                "    if (memory == NULL)"
                "    {"
                "        fprintf(stderr, \"runtime error: out of memory\\n\");"
                "        exit(1);"
                "    }"
                ""
                "    return memory;"
                "}"
                ""
                "static void kappa_panic(const char* message)"
                "{"
                "    fprintf(stderr, \"runtime error: %s\\n\", message);"
                "    exit(1);"
                "}"
                ""
                "static void kappa_panic_arity(const char* name, int expected, int actual)"
                "{"
                "    fprintf(stderr, \"runtime error: callable '%s' expected %d argument(s) but received %d\\n\", name, expected, actual);"
                "    exit(1);"
                "}"
                ""
                "static KValue* kappa_alloc_value(KValueTag tag)"
                "{"
                "    KValue* value = (KValue*)kappa_alloc(sizeof(KValue));"
                "    value->tag = tag;"
                "    return value;"
                "}"
                ""
                "static KValue* kappa_box_int(int64_t value)"
                "{"
                "    KValue* boxed = kappa_alloc_value(K_TAG_INT);"
                "    boxed->as.int_value = value;"
                "    return boxed;"
                "}"
                ""
                "static KValue* kappa_box_float(double value)"
                "{"
                "    KValue* boxed = kappa_alloc_value(K_TAG_FLOAT);"
                "    boxed->as.float_value = value;"
                "    return boxed;"
                "}"
                ""
                "static KValue* kappa_box_string(const char* value)"
                "{"
                "    KValue* boxed = kappa_alloc_value(K_TAG_STRING);"
                "    boxed->as.string_value = value;"
                "    return boxed;"
                "}"
                ""
                "static char* kappa_duplicate_string(const char* value)"
                "{"
                "    size_t length = strlen(value);"
                "    char* copy = (char*)kappa_alloc(length + 1);"
                "    memcpy(copy, value, length + 1);"
                "    return copy;"
                "}"
                ""
                "static KValue* kappa_box_char(int32_t value)"
                "{"
                "    KValue* boxed = kappa_alloc_value(K_TAG_CHAR);"
                "    boxed->as.char_value = value;"
                "    return boxed;"
                "}"
                ""
                "static KValue* kappa_box_bool(int value)"
                "{"
                "    static KValue true_value = { K_TAG_BOOL, { .bool_value = 1 } };"
                "    static KValue false_value = { K_TAG_BOOL, { .bool_value = 0 } };"
                "    return value ? &true_value : &false_value;"
                "}"
                ""
                "static KValue* kappa_unit(void)"
                "{"
                "    static KValue unit_value = { K_TAG_UNIT, { .int_value = 0 } };"
                "    return &unit_value;"
                "}"
                ""
                "static int64_t kappa_expect_int(KValue* value)"
                "{"
                "    if (value == NULL || value->tag != K_TAG_INT)"
                "    {"
                "        kappa_panic(\"expected Int value\");"
                "    }"
                ""
                "    return value->as.int_value;"
                "}"
                ""
                "static double kappa_expect_float(KValue* value)"
                "{"
                "    if (value == NULL || value->tag != K_TAG_FLOAT)"
                "    {"
                "        kappa_panic(\"expected Float value\");"
                "    }"
                ""
                "    return value->as.float_value;"
                "}"
                ""
                "static int kappa_expect_bool(KValue* value)"
                "{"
                "    if (value == NULL || value->tag != K_TAG_BOOL)"
                "    {"
                "        kappa_panic(\"expected Bool value\");"
                "    }"
                ""
                "    return value->as.bool_value;"
                "}"
                ""
                "static int kappa_is_unit(KValue* value)"
                "{"
                "    return value != NULL && value->tag == K_TAG_UNIT;"
                "}"
                ""
                "static int kappa_is_int_value(KValue* value, int64_t expected)"
                "{"
                "    return value != NULL && value->tag == K_TAG_INT && value->as.int_value == expected;"
                "}"
                ""
                "static int kappa_is_float_value(KValue* value, double expected)"
                "{"
                "    return value != NULL && value->tag == K_TAG_FLOAT && value->as.float_value == expected;"
                "}"
                ""
                "static int kappa_is_string_value(KValue* value, const char* expected)"
                "{"
                "    return value != NULL && value->tag == K_TAG_STRING && strcmp(value->as.string_value, expected) == 0;"
                "}"
                ""
                "static int kappa_is_char_value(KValue* value, int32_t expected)"
                "{"
                "    return value != NULL && value->tag == K_TAG_CHAR && value->as.char_value == expected;"
                "}"
                ""
                "static KValue* kappa_make_data(int type_id, int ctor_tag, int field_count, KValue** fields)"
                "{"
                "    KValue* value = kappa_alloc_value(K_TAG_DATA);"
                "    value->as.data_value.type_id = type_id;"
                "    value->as.data_value.ctor_tag = ctor_tag;"
                "    value->as.data_value.field_count = field_count;"
                ""
                "    if (field_count == 0)"
                "    {"
                "        value->as.data_value.fields = NULL;"
                "    }"
                "    else"
                "    {"
                "        size_t field_bytes = sizeof(KValue*) * (size_t)field_count;"
                "        value->as.data_value.fields = (KValue**)kappa_alloc(field_bytes);"
                "        memcpy(value->as.data_value.fields, fields, field_bytes);"
                "    }"
                ""
                "    return value;"
                "}"
                ""
                "static int kappa_is_ctor(KValue* value, int type_id, int ctor_tag)"
                "{"
                "    if (value == NULL)"
                "    {"
                "        return 0;"
                "    }"
                ""
                $"    if (type_id == {preludeBoolTypeId} && value->tag == K_TAG_BOOL)"
                "    {"
                "        int actual_tag = value->as.bool_value ? 0 : 1;"
                "        return actual_tag == ctor_tag;"
                "    }"
                ""
                "    return value->tag == K_TAG_DATA"
                "        && value->as.data_value.type_id == type_id"
                "        && value->as.data_value.ctor_tag == ctor_tag;"
                "}"
                ""
                "static KValue* kappa_get_field(KValue* value, int index)"
                "{"
                "    if (value == NULL || value->tag != K_TAG_DATA)"
                "    {"
                "        kappa_panic(\"attempted to project a field from a non-data value\");"
                "    }"
                ""
                "    if (index < 0 || index >= value->as.data_value.field_count)"
                "    {"
                "        kappa_panic(\"attempted to project an out-of-range field\");"
                "    }"
                ""
                "    return value->as.data_value.fields[index];"
                "}"
                ""
                "static KValue* kappa_make_closure(KappaFunction function, void* env, int arity, const char* debug_name)"
                "{"
                "    KValue* value = kappa_alloc_value(K_TAG_CLOSURE);"
                "    value->as.closure_value.function = function;"
                "    value->as.closure_value.env = env;"
                "    value->as.closure_value.arity = arity;"
                "    value->as.closure_value.debug_name = debug_name;"
                "    return value;"
                "}"
                ""
                "static KValue* kappa_make_ref(KValue* initial)"
                "{"
                "    KValue* value = kappa_alloc_value(K_TAG_REF);"
                "    value->as.ref_value.value = initial;"
                "    return value;"
                "}"
                ""
                "static KValue* kappa_ref_read(KValue* reference)"
                "{"
                "    if (reference == NULL || reference->tag != K_TAG_REF)"
                "    {"
                "        kappa_panic(\"expected Ref value\");"
                "    }"
                ""
                "    return reference->as.ref_value.value;"
                "}"
                ""
                "static KValue* kappa_ref_write(KValue* reference, KValue* value)"
                "{"
                "    if (reference == NULL || reference->tag != K_TAG_REF)"
                "    {"
                "        kappa_panic(\"expected Ref value\");"
                "    }"
                ""
                "    reference->as.ref_value.value = value;"
                "    return kappa_unit();"
                "}"
                ""
                "static KValue* kappa_make_dictionary(const char* module_name, const char* trait_name, const char* instance_key)"
                "{"
                "    KValue* value = kappa_alloc_value(K_TAG_DICTIONARY);"
                "    value->as.dictionary_value.module_name = module_name;"
                "    value->as.dictionary_value.trait_name = trait_name;"
                "    value->as.dictionary_value.instance_key = instance_key;"
                "    return value;"
                "}"
                ""
                "static KValue* kappa_invoke_value(KValue* callee, KValue** args, int argc)"
                "{"
                "    if (callee == NULL || callee->tag != K_TAG_CLOSURE)"
                "    {"
                "        kappa_panic(\"attempted to call a non-function value\");"
                "    }"
                ""
                "    if (callee->as.closure_value.arity != argc)"
                "    {"
                "        kappa_panic_arity(callee->as.closure_value.debug_name, callee->as.closure_value.arity, argc);"
                "    }"
                ""
                "    return callee->as.closure_value.function(callee->as.closure_value.env, args, argc);"
                "}"
                ""
                "static KValue* kappa_int_add(KValue* left, KValue* right)"
                "{"
                "    return kappa_box_int(kappa_expect_int(left) + kappa_expect_int(right));"
                "}"
                ""
                "static KValue* kappa_int_subtract(KValue* left, KValue* right)"
                "{"
                "    return kappa_box_int(kappa_expect_int(left) - kappa_expect_int(right));"
                "}"
                ""
                "static KValue* kappa_int_multiply(KValue* left, KValue* right)"
                "{"
                "    return kappa_box_int(kappa_expect_int(left) * kappa_expect_int(right));"
                "}"
                ""
                "static KValue* kappa_int_divide(KValue* left, KValue* right)"
                "{"
                "    int64_t divisor = kappa_expect_int(right);"
                "    if (divisor == 0)"
                "    {"
                "        kappa_panic(\"division by zero\");"
                "    }"
                ""
                "    return kappa_box_int(kappa_expect_int(left) / divisor);"
                "}"
                ""
                "static KValue* kappa_int_negate(KValue* value)"
                "{"
                "    return kappa_box_int(-kappa_expect_int(value));"
                "}"
                ""
                "static KValue* kappa_int_less(KValue* left, KValue* right)"
                "{"
                "    return kappa_box_bool(kappa_expect_int(left) < kappa_expect_int(right));"
                "}"
                ""
                "static KValue* kappa_int_less_equal(KValue* left, KValue* right)"
                "{"
                "    return kappa_box_bool(kappa_expect_int(left) <= kappa_expect_int(right));"
                "}"
                ""
                "static KValue* kappa_int_greater(KValue* left, KValue* right)"
                "{"
                "    return kappa_box_bool(kappa_expect_int(left) > kappa_expect_int(right));"
                "}"
                ""
                "static KValue* kappa_int_greater_equal(KValue* left, KValue* right)"
                "{"
                "    return kappa_box_bool(kappa_expect_int(left) >= kappa_expect_int(right));"
                "}"
                ""
                "static KValue* kappa_bool_not(KValue* value)"
                "{"
                "    return kappa_box_bool(!kappa_expect_bool(value));"
                "}"
                ""
                "static KValue* kappa_bool_and(KValue* left, KValue* right)"
                "{"
                "    return kappa_box_bool(kappa_expect_bool(left) && kappa_expect_bool(right));"
                "}"
                ""
                "static KValue* kappa_bool_or(KValue* left, KValue* right)"
                "{"
                "    return kappa_box_bool(kappa_expect_bool(left) || kappa_expect_bool(right));"
                "}"
                ""
                "static KValue* kappa_value_equal(KValue* left, KValue* right)"
                "{"
                "    if (left == right)"
                "    {"
                "        return kappa_box_bool(1);"
                "    }"
                ""
                "    if (left == NULL || right == NULL || left->tag != right->tag)"
                "    {"
                "        return kappa_box_bool(0);"
                "    }"
                ""
                "    switch (left->tag)"
                "    {"
                "        case K_TAG_INT:"
                "            return kappa_box_bool(left->as.int_value == right->as.int_value);"
                "        case K_TAG_FLOAT:"
                "            return kappa_box_bool(left->as.float_value == right->as.float_value);"
                "        case K_TAG_BOOL:"
                "            return kappa_box_bool(left->as.bool_value == right->as.bool_value);"
                "        case K_TAG_STRING:"
                "            return kappa_box_bool(strcmp(left->as.string_value, right->as.string_value) == 0);"
                "        case K_TAG_CHAR:"
                "            return kappa_box_bool(left->as.char_value == right->as.char_value);"
                "        case K_TAG_UNIT:"
                "            return kappa_box_bool(1);"
                "        default:"
                "            return kappa_box_bool(0);"
                "    }"
                "}"
                ""
                "static KValue* kappa_value_not_equal(KValue* left, KValue* right)"
                "{"
                "    KValue* equal = kappa_value_equal(left, right);"
                "    return kappa_box_bool(!kappa_expect_bool(equal));"
                "}"
                ""
                "static void kappa_write_value(KValue* value)"
                "{"
                "    if (value == NULL)"
                "    {"
                "        fputs(\"<null>\", stdout);"
                "        return;"
                "    }"
                ""
                "    switch (value->tag)"
                "    {"
                "        case K_TAG_INT:"
                "            printf(\"%lld\", (long long)value->as.int_value);"
                "            return;"
                "        case K_TAG_FLOAT:"
                "            printf(\"%.17g\", value->as.float_value);"
                "            return;"
                "        case K_TAG_BOOL:"
                "            fputs(value->as.bool_value ? \"True\" : \"False\", stdout);"
                "            return;"
                "        case K_TAG_STRING:"
                "            fputs(value->as.string_value, stdout);"
                "            return;"
                "        case K_TAG_CHAR:"
                "            putchar((char)value->as.char_value);"
                "            return;"
                "        case K_TAG_UNIT:"
                "            fputs(\"()\", stdout);"
                "            return;"
                "        case K_TAG_DATA:"
                "            printf(\"<data:%d:%d>\", value->as.data_value.type_id, value->as.data_value.ctor_tag);"
                "            return;"
                "        case K_TAG_CLOSURE:"
                "            printf(\"<closure:%s>\", value->as.closure_value.debug_name);"
                "            return;"
                "        case K_TAG_REF:"
                "            fputs(\"<ref>\", stdout);"
                "            return;"
                "        case K_TAG_DICTIONARY:"
                "            printf(\"<dict:%s:%s:%s>\", value->as.dictionary_value.module_name, value->as.dictionary_value.trait_name, value->as.dictionary_value.instance_key);"
                "            return;"
                "        default:"
                "            fputs(\"<unknown>\", stdout);"
                "            return;"
                "    }"
                "}"
                ""
                "static KValue* kappa_builtin_print(KValue* value, int append_newline)"
                "{"
                "    kappa_write_value(value);"
                "    if (append_newline)"
                "    {"
                "        putchar('\\n');"
                "    }"
                "    return kappa_unit();"
                "}"
                ""
                "static KValue* kappa_builtin_print_int(KValue* value)"
                "{"
                "    printf(\"%lld\\n\", (long long)kappa_expect_int(value));"
                "    return kappa_unit();"
                "}"
                ""
                "static KValue* kappa_builtin_print_string(KValue* value)"
                "{"
                "    if (value == NULL || value->tag != K_TAG_STRING)"
                "    {"
                "        kappa_panic(\"expected String value\");"
                "    }"
                ""
                "    fputs(value->as.string_value, stdout);"
                "    return kappa_unit();"
                "}"
                ""
                "static KValue* kappa_int_to_string(KValue* value)"
                "{"
                "    int64_t integer = kappa_expect_int(value);"
                "    int length = snprintf(NULL, 0, \"%lld\", (long long)integer);"
                ""
                "    if (length < 0)"
                "    {"
                "        kappa_panic(\"failed to format Int as String\");"
                "    }"
                ""
                "    char* buffer = (char*)kappa_alloc((size_t)length + 1);"
                "    snprintf(buffer, (size_t)length + 1, \"%lld\", (long long)integer);"
                "    return kappa_box_string(buffer);"
                "}"
                ""
                "static void kappa_print_result_if_needed(KValue* value)"
                "{"
                "    if (!kappa_is_unit(value))"
                "    {"
                "        kappa_write_value(value);"
                "        putchar('\\n');"
                "    }"
                "}"
                ""
            ]

        joinLines runtimeLines

    let private emitEntryPointProgram entryFunctionName =
        joinLines
            [
                "int main(void)"
                "{"
                $"    KValue* result = {entryFunctionName}(NULL, NULL, 0);"
                "    kappa_print_result_if_needed(result);"
                "    return 0;"
                "}"
            ]

    let private buildTranslationUnit (workspace: WorkspaceCompilation) =
        result {
            if workspace.HasErrors then
                return!
                    Result.Error
                        $"Cannot emit native code for a workspace with diagnostics:{Environment.NewLine}{aggregateDiagnostics workspace.Diagnostics}"

            let verificationDiagnostics = CheckpointVerification.verifyCheckpoint workspace "KBackendIR"

            if not (List.isEmpty verificationDiagnostics) then
                return!
                    Result.Error
                        $"Cannot emit native code from malformed KBackendIR:{Environment.NewLine}{aggregateDiagnostics verificationDiagnostics}"

            let context = buildContext workspace

            let topLevelFunctions =
                workspace.KBackendIR
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
                workspace.KBackendIR
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
        buildTranslationUnit workspace

    let emitArtifact (workspace: WorkspaceCompilation) (entryPoint: string) (outputDirectory: string) =
        result {
            let! translationUnit = buildTranslationUnit workspace
            let! entryModuleName, entryBindingName = resolveEntryPoint workspace entryPoint
            let context = buildContext workspace

            let! entryFunctionName =
                lookupFunctionName context entryModuleName entryBindingName
                |> resultOfOption $"zig could not resolve emitted entry point '{entryPoint}'."

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
