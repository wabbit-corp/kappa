namespace Kappa.Compiler

open System
open System.Collections.Generic

type EvaluationError =
    { Message: string }

type RuntimeValue =
    | IntegerValue of int64
    | FloatValue of double
    | BooleanValue of bool
    | StringValue of string
    | CharacterValue of char
    | UnitValue
    | FunctionValue of RuntimeClosure
    | BuiltinFunctionValue of BuiltinFunction
and RuntimeClosure =
    { Parameters: string list
      Body: CoreExpression
      Scope: RuntimeScope }
and BuiltinFunction =
    { Name: string
      Arguments: RuntimeValue list }
and RuntimeScope =
    { Locals: Map<string, RuntimeValue>
      CurrentModule: string
      Context: RuntimeContext }
and RuntimeContext =
    { Modules: Map<string, RuntimeModule> }
and RuntimeModule =
    { Name: string
      Document: ParsedDocument
      Definitions: Map<string, LetDefinition>
      IntrinsicTerms: Set<string>
      Exports: Set<string>
      Imports: ImportSpec list
      Values: Dictionary<string, Lazy<Result<RuntimeValue, EvaluationError>>> }

module RuntimeValue =
    let format value =
        match value with
        | IntegerValue value -> string value
        | FloatValue value -> string value
        | BooleanValue true -> "True"
        | BooleanValue false -> "False"
        | StringValue value -> $"\"{value}\""
        | CharacterValue value -> $"'{value}'"
        | UnitValue -> "()"
        | FunctionValue _ -> "<function>"
        | BuiltinFunctionValue builtin -> $"<builtin {builtin.Name}>"

module Interpreter =
    let private error message =
        Result.Error { Message = message }

    let private ok value =
        Result.Ok value

    let private literalToValue literal =
        match literal with
        | LiteralValue.Integer value -> IntegerValue value
        | LiteralValue.Float value -> FloatValue value
        | LiteralValue.String value -> StringValue value
        | LiteralValue.Character value -> CharacterValue value
        | LiteralValue.Unit -> UnitValue

    let private isPrivateByDefault (document: ParsedDocument) =
        document.Syntax.ModuleAttributes
        |> List.exists (fun attributeName -> String.Equals(attributeName, "PrivateByDefault", StringComparison.Ordinal))

    let private isExported (document: ParsedDocument) (definition: LetDefinition) =
        match definition.Visibility with
        | Some Visibility.Public -> true
        | Some Visibility.Private -> false
        | None -> not (isPrivateByDefault document)

    let private collectImports (document: ParsedDocument) =
        Stdlib.implicitImportsFor document.ModuleName
        @ (document.Syntax.Declarations
        |> List.collect (function
            | ImportDeclaration (false, specs) -> specs
            | _ -> []))

    let private collectIntrinsicTerms (document: ParsedDocument) =
        match document.ModuleName with
        | Some moduleName ->
            document.Syntax.Declarations
            |> List.choose (function
                | ExpectDeclarationNode (ExpectTermDeclaration declaration)
                    when Stdlib.intrinsicallySatisfiesExpect moduleName (ExpectTermDeclaration declaration) ->
                    Some declaration.Name
                | _ ->
                    None)
            |> Set.ofList
        | None ->
            Set.empty

    let private buildContext (workspace: WorkspaceCompilation) =
        let moduleRuntimes =
            workspace.Documents
            |> List.choose (fun document ->
                document.ModuleName
                |> Option.map (fun moduleName ->
                    let moduleText = SyntaxFacts.moduleNameToText moduleName

                    let definitions =
                        document.Syntax.Declarations
                        |> List.choose (function
                            | LetDeclaration (definition: LetDefinition) when definition.Name.IsSome && definition.Body.IsSome ->
                                Some(definition.Name.Value, definition)
                            | _ ->
                                None)
                        |> Map.ofList

                    let intrinsicTerms =
                        collectIntrinsicTerms document

                    let exports =
                        (definitions
                         |> Map.toList
                         |> List.choose (fun (name, definition) -> if isExported document definition then Some name else None)
                         |> Set.ofList)
                        |> Set.union intrinsicTerms

                    moduleText,
                    { Name = moduleText
                      Document = document
                      Definitions = definitions
                      IntrinsicTerms = intrinsicTerms
                      Exports = exports
                      Imports = collectImports document
                      Values = Dictionary<string, Lazy<Result<RuntimeValue, EvaluationError>>>() }))
            |> Map.ofList

        let context = { Modules = moduleRuntimes }

        let builtinBinaryNames =
            Set.ofList [ "+"; "-"; "*"; "/"; "=="; "!="; "<"; "<="; ">"; ">="; "&&"; "||" ]

        let tryCreateBuiltinFunction name =
            if builtinBinaryNames.Contains(name) then
                Some(BuiltinFunctionValue { Name = name; Arguments = [] })
            else
                None

        let tryCreateIntrinsicTermValue moduleName name =
            if not (String.Equals(moduleName, Stdlib.PreludeModuleText, StringComparison.Ordinal)) then
                None
            else
                match name with
                | "True" -> Some(BooleanValue true)
                | "False" -> Some(BooleanValue false)
                | "pure"
                | ">>="
                | ">>"
                | "not"
                | "and"
                | "or"
                | "negate"
                | "println"
                | "print" ->
                    Some(BuiltinFunctionValue { Name = name; Arguments = [] })
                | _ ->
                    None

        let applyBuiltinUnary operatorName operand =
            match operatorName, operand with
            | "-", IntegerValue value ->
                ok (IntegerValue(-value))
            | "-", FloatValue value ->
                ok (FloatValue(-value))
            | "-", value ->
                error $"Unary '{operatorName}' expects a numeric value, but got {RuntimeValue.format value}."
            | _ ->
                error $"Unary operator '{operatorName}' is not supported."

        let applyBuiltinBinary operatorName left right =
            match operatorName, left, right with
            | "+", IntegerValue left, IntegerValue right ->
                ok (IntegerValue(left + right))
            | "-", IntegerValue left, IntegerValue right ->
                ok (IntegerValue(left - right))
            | "*", IntegerValue left, IntegerValue right ->
                ok (IntegerValue(left * right))
            | "/", IntegerValue left, IntegerValue right ->
                if right = 0L then
                    error "Division by zero."
                else
                    ok (IntegerValue(left / right))
            | "+", FloatValue left, FloatValue right ->
                ok (FloatValue(left + right))
            | "-", FloatValue left, FloatValue right ->
                ok (FloatValue(left - right))
            | "*", FloatValue left, FloatValue right ->
                ok (FloatValue(left * right))
            | "/", FloatValue left, FloatValue right ->
                ok (FloatValue(left / right))
            | "==", left, right ->
                ok (BooleanValue(left = right))
            | "!=", left, right ->
                ok (BooleanValue(left <> right))
            | "<", IntegerValue left, IntegerValue right ->
                ok (BooleanValue(left < right))
            | "<=", IntegerValue left, IntegerValue right ->
                ok (BooleanValue(left <= right))
            | ">", IntegerValue left, IntegerValue right ->
                ok (BooleanValue(left > right))
            | ">=", IntegerValue left, IntegerValue right ->
                ok (BooleanValue(left >= right))
            | "<", FloatValue left, FloatValue right ->
                ok (BooleanValue(left < right))
            | "<=", FloatValue left, FloatValue right ->
                ok (BooleanValue(left <= right))
            | ">", FloatValue left, FloatValue right ->
                ok (BooleanValue(left > right))
            | ">=", FloatValue left, FloatValue right ->
                ok (BooleanValue(left >= right))
            | "&&", BooleanValue left, BooleanValue right ->
                ok (BooleanValue(left && right))
            | "||", BooleanValue left, BooleanValue right ->
                ok (BooleanValue(left || right))
            | _ ->
                error $"Operator '{operatorName}' is not supported for {RuntimeValue.format left} and {RuntimeValue.format right}."

        let renderInterpolatedValue value =
            match value with
            | IntegerValue value -> ok (string value)
            | FloatValue value -> ok (string value)
            | BooleanValue true -> ok "True"
            | BooleanValue false -> ok "False"
            | StringValue value -> ok value
            | CharacterValue value -> ok (string value)
            | UnitValue -> ok "()"
            | FunctionValue _
            | BuiltinFunctionValue _ ->
                error $"Cannot interpolate {RuntimeValue.format value} into an f-string."

        let rec evaluateExpression (scope: RuntimeScope) (expression: CoreExpression) : Result<RuntimeValue, EvaluationError> =
            match expression with
            | Literal literal ->
                ok (literalToValue literal)
            | Name segments ->
                resolveName scope segments
            | PrefixedString (prefix, parts) ->
                evaluatePrefixedString scope prefix parts
            | Lambda (parameters, body) ->
                ok
                    (FunctionValue
                        { Parameters = parameters |> List.map (fun (parameter: Parameter) -> parameter.Name)
                          Body = body
                          Scope = scope })
            | IfThenElse (condition, whenTrue, whenFalse) ->
                evaluateExpression scope condition
                |> Result.bind (function
                    | BooleanValue true -> evaluateExpression scope whenTrue
                    | BooleanValue false -> evaluateExpression scope whenFalse
                    | value ->
                        error $"Expected a Boolean in the if condition, but got {RuntimeValue.format value}.")
            | Apply (callee, arguments) ->
                evaluateExpression scope callee
                |> Result.bind (fun functionValue ->
                    arguments
                    |> List.map (evaluateExpression scope)
                    |> List.fold
                        (fun state next ->
                            match state, next with
                            | Result.Ok values, Result.Ok value -> Result.Ok(value :: values)
                            | Result.Error issue, _ -> Result.Error issue
                            | _, Result.Error issue -> Result.Error issue)
                        (Result.Ok [])
                    |> Result.bind (fun values -> apply functionValue (List.rev values)))
            | Unary (operatorName, expression) ->
                evaluateExpression scope expression
                |> Result.bind (fun operand ->
                    if hasExplicitUnqualifiedName scope operatorName then
                        resolveName scope [ operatorName ]
                        |> Result.bind (fun functionValue -> apply functionValue [ operand ])
                    else
                        applyBuiltinUnary operatorName operand)
            | Binary (left, "&&", right) when not (hasExplicitUnqualifiedName scope "&&") ->
                evaluateExpression scope left
                |> Result.bind (function
                    | BooleanValue false -> ok (BooleanValue false)
                    | BooleanValue true ->
                        evaluateExpression scope right
                        |> Result.bind (function
                            | BooleanValue value -> ok (BooleanValue value)
                            | value ->
                                error $"Operator '&&' expects Boolean operands, but got {RuntimeValue.format value}.")
                    | value ->
                        error $"Operator '&&' expects Boolean operands, but got {RuntimeValue.format value}.")
            | Binary (left, "||", right) when not (hasExplicitUnqualifiedName scope "||") ->
                evaluateExpression scope left
                |> Result.bind (function
                    | BooleanValue true -> ok (BooleanValue true)
                    | BooleanValue false ->
                        evaluateExpression scope right
                        |> Result.bind (function
                            | BooleanValue value -> ok (BooleanValue value)
                            | value ->
                                error $"Operator '||' expects Boolean operands, but got {RuntimeValue.format value}.")
                    | value ->
                        error $"Operator '||' expects Boolean operands, but got {RuntimeValue.format value}.")
            | Binary (left, operatorName, right) ->
                evaluateExpression scope left
                |> Result.bind (fun leftValue ->
                    evaluateExpression scope right
                    |> Result.bind (fun rightValue ->
                        if hasExplicitUnqualifiedName scope operatorName then
                            resolveName scope [ operatorName ]
                            |> Result.bind (fun functionValue -> apply functionValue [ leftValue; rightValue ])
                        else
                            applyBuiltinBinary operatorName leftValue rightValue))

        and evaluatePrefixedString (scope: RuntimeScope) (prefix: string) (parts: InterpolatedStringPart list) =
            if not (String.Equals(prefix, "f", StringComparison.Ordinal)) then
                error $"Prefixed string '{prefix}\"...\"' is not supported by the interpreter yet."
            else
                parts
                |> List.fold
                    (fun state part ->
                        state
                        |> Result.bind (fun segments ->
                            match part with
                            | StringText text ->
                                ok (text :: segments)
                            | StringInterpolation expression ->
                                evaluateExpression scope expression
                                |> Result.bind renderInterpolatedValue
                                |> Result.map (fun text -> text :: segments)))
                    (ok [])
                |> Result.map (fun segments ->
                    segments
                    |> List.rev
                    |> String.concat ""
                    |> StringValue)

        and apply (functionValue: RuntimeValue) (arguments: RuntimeValue list) : Result<RuntimeValue, EvaluationError> =
            match functionValue with
            | FunctionValue closure ->
                applyClosure closure arguments
            | BuiltinFunctionValue builtin ->
                applyBuiltinFunction builtin arguments
            | value ->
                error $"Cannot apply {RuntimeValue.format value} as a function."

        and applyBuiltinFunction (builtin: BuiltinFunction) (arguments: RuntimeValue list) : Result<RuntimeValue, EvaluationError> =
            let rec invoke (currentBuiltin: BuiltinFunction) (remainingArguments: RuntimeValue list) =
                match remainingArguments with
                | [] ->
                    ok (BuiltinFunctionValue currentBuiltin)
                | argument :: rest ->
                    let nextBuiltin =
                        { currentBuiltin with
                            Arguments = currentBuiltin.Arguments @ [ argument ] }

                    invokeBuiltin nextBuiltin
                    |> Result.bind (function
                        | Some value ->
                            if List.isEmpty rest then
                                ok value
                            else
                                apply value rest
                        | None ->
                            invoke nextBuiltin rest)

            invoke builtin arguments

        and invokeBuiltin (builtin: BuiltinFunction) : Result<RuntimeValue option, EvaluationError> =
            match builtin.Name, builtin.Arguments with
            | "not", [ BooleanValue value ] ->
                ok (Some(BooleanValue(not value)))
            | "not", arguments when List.length arguments < 1 ->
                ok None
            | "not", [ value ] ->
                error $"Intrinsic 'not' expects a Bool, but got {RuntimeValue.format value}."
            | "not", _ ->
                error "Intrinsic 'not' received too many arguments."
            | "negate", [ IntegerValue value ] ->
                ok (Some(IntegerValue(-value)))
            | "negate", [ FloatValue value ] ->
                ok (Some(FloatValue(-value)))
            | "negate", arguments when List.length arguments < 1 ->
                ok None
            | "negate", [ value ] ->
                error $"Intrinsic 'negate' expects a numeric value, but got {RuntimeValue.format value}."
            | "negate", _ ->
                error "Intrinsic 'negate' received too many arguments."
            | "and", [ BooleanValue left; BooleanValue right ] ->
                ok (Some(BooleanValue(left && right)))
            | "and", arguments when List.length arguments < 2 ->
                ok None
            | "and", [ left; right ] ->
                error $"Intrinsic 'and' expects Bool arguments, but got {RuntimeValue.format left} and {RuntimeValue.format right}."
            | "and", _ ->
                error "Intrinsic 'and' received too many arguments."
            | "or", [ BooleanValue left; BooleanValue right ] ->
                ok (Some(BooleanValue(left || right)))
            | "or", arguments when List.length arguments < 2 ->
                ok None
            | "or", [ left; right ] ->
                error $"Intrinsic 'or' expects Bool arguments, but got {RuntimeValue.format left} and {RuntimeValue.format right}."
            | "or", _ ->
                error "Intrinsic 'or' received too many arguments."
            | "pure", [ _ ] ->
                error "Intrinsic 'pure' is declared in std.prelude but is not executable in the interpreter yet."
            | "pure", arguments when List.length arguments < 1 ->
                ok None
            | "pure", _ ->
                error "Intrinsic 'pure' received too many arguments."
            | ">>=", [ _; _ ] ->
                error "Intrinsic '>>=' is declared in std.prelude but is not executable in the interpreter yet."
            | ">>=", arguments when List.length arguments < 2 ->
                ok None
            | ">>=", _ ->
                error "Intrinsic '>>=' received too many arguments."
            | ">>", [ _; _ ] ->
                error "Intrinsic '>>' is declared in std.prelude but is not executable in the interpreter yet."
            | ">>", arguments when List.length arguments < 2 ->
                ok None
            | ">>", _ ->
                error "Intrinsic '>>' received too many arguments."
            | "print", [ _ ] ->
                error "Intrinsic 'print' is declared in std.prelude but is not executable in the interpreter yet."
            | "print", arguments when List.length arguments < 1 ->
                ok None
            | "print", _ ->
                error "Intrinsic 'print' received too many arguments."
            | "println", [ _ ] ->
                error "Intrinsic 'println' is declared in std.prelude but is not executable in the interpreter yet."
            | "println", arguments when List.length arguments < 1 ->
                ok None
            | "println", _ ->
                error "Intrinsic 'println' received too many arguments."
            | name, [ left; right ] when builtinBinaryNames.Contains(name) ->
                applyBuiltinBinary name left right
                |> Result.map Some
            | name, arguments when builtinBinaryNames.Contains(name) && List.length arguments < 2 ->
                ok None
            | name, _ when builtinBinaryNames.Contains(name) ->
                error $"Operator '{name}' received too many arguments."
            | _ ->
                error $"Unknown builtin '{builtin.Name}'."

        and applyClosure (closure: RuntimeClosure) (arguments: RuntimeValue list) : Result<RuntimeValue, EvaluationError> =
            let rec invoke (currentClosure: RuntimeClosure) (remainingArguments: RuntimeValue list) =
                match remainingArguments with
                | [] ->
                    ok (FunctionValue currentClosure)
                | argument :: rest ->
                    match currentClosure.Parameters with
                    | [] ->
                        error "The function received too many arguments."
                    | parameter :: remainingParameters ->
                        let nextScope =
                            { currentClosure.Scope with
                                Locals = currentClosure.Scope.Locals.Add(parameter, argument) }

                        if List.isEmpty remainingParameters then
                            evaluateExpression nextScope currentClosure.Body
                            |> Result.bind (fun value ->
                                if List.isEmpty rest then
                                    ok value
                                else
                                    apply value rest)
                        else
                            invoke
                                { currentClosure with
                                    Parameters = remainingParameters
                                    Scope = nextScope }
                                rest

            invoke closure arguments

        and resolveName (scope: RuntimeScope) (segments: string list) : Result<RuntimeValue, EvaluationError> =
            match segments with
            | [] ->
                error "Encountered an empty name."
            | [ name ] ->
                resolveUnqualifiedName scope name
            | _ ->
                let qualifierSegments = segments |> List.take (segments.Length - 1)
                let bindingName = List.last segments
                resolveQualifiedName scope qualifierSegments bindingName

        and findImportedModulesForName (scope: RuntimeScope) (name: string) =
            let currentModule = scope.Context.Modules[scope.CurrentModule]

            currentModule.Imports
            |> List.choose (fun spec ->
                match spec.Source with
                | Url _ ->
                    None
                | Dotted moduleSegments ->
                    let importedModuleName = SyntaxFacts.moduleNameToText moduleSegments

                    match Map.tryFind importedModuleName scope.Context.Modules with
                    | None ->
                        None
                    | Some importedModule ->
                        let importsName =
                            match spec.Selection with
                            | QualifiedOnly -> false
                            | Items items ->
                                items
                                |> List.exists (fun item ->
                                    String.Equals(item.Name, name, StringComparison.Ordinal)
                                    && (item.Namespace.IsNone || item.Namespace = Some ImportNamespace.Term))
                            | All ->
                                importedModule.Exports.Contains(name)
                            | AllExcept excludedNames ->
                                not (List.contains name excludedNames) && importedModule.Exports.Contains(name)

                        if importsName && importedModule.Exports.Contains(name) then
                            Some(importedModuleName, importedModule)
                        else
                            None)
            |> List.distinctBy fst
            |> List.map snd

        and hasExplicitUnqualifiedName (scope: RuntimeScope) (name: string) =
            let currentModule = scope.Context.Modules[scope.CurrentModule]

            Map.containsKey name scope.Locals
            || currentModule.Definitions.ContainsKey(name)
            || not (List.isEmpty (findImportedModulesForName scope name))

        and resolveUnqualifiedName (scope: RuntimeScope) (name: string) : Result<RuntimeValue, EvaluationError> =
            match Map.tryFind name scope.Locals with
            | Some value ->
                ok value
            | None ->
                let currentModule = scope.Context.Modules[scope.CurrentModule]

                if currentModule.Definitions.ContainsKey(name) then
                    forceBinding currentModule name
                else
                    let matches = findImportedModulesForName scope name

                    match matches with
                    | [] ->
                        match tryCreateBuiltinFunction name with
                        | Some builtin ->
                            ok builtin
                        | None ->
                            error $"Name '{name}' is not in scope."
                    | [ importedModule ] ->
                        forceBinding importedModule name
                    | _ ->
                        error $"Name '{name}' is ambiguous across imported modules."

        and resolveQualifiedName (scope: RuntimeScope) (qualifierSegments: string list) (bindingName: string) : Result<RuntimeValue, EvaluationError> =
            let qualifierText = SyntaxFacts.moduleNameToText qualifierSegments
            let currentModule = scope.Context.Modules[scope.CurrentModule]

            let targetModule =
                if String.Equals(qualifierText, scope.CurrentModule, StringComparison.Ordinal) then
                    Some currentModule
                else
                    currentModule.Imports
                    |> List.tryPick (fun spec ->
                        match spec.Source, spec.Alias, spec.Selection with
                        | Dotted moduleSegments, Some alias, QualifiedOnly when qualifierSegments = [ alias ] ->
                            Map.tryFind (SyntaxFacts.moduleNameToText moduleSegments) scope.Context.Modules
                        | Dotted moduleSegments, None, QualifiedOnly when qualifierSegments = moduleSegments ->
                            Map.tryFind (SyntaxFacts.moduleNameToText moduleSegments) scope.Context.Modules
                        | _ ->
                            None)

            match targetModule with
            | Some runtimeModule ->
                if runtimeModule.Name = scope.CurrentModule || runtimeModule.Exports.Contains(bindingName) then
                    forceBinding runtimeModule bindingName
                else
                    error $"'{bindingName}' is not exported from module '{runtimeModule.Name}'."
            | None ->
                error $"Module qualifier '{qualifierText}' is not in scope."

        and forceBinding (runtimeModule: RuntimeModule) (bindingName: string) : Result<RuntimeValue, EvaluationError> =
            match runtimeModule.Values.TryGetValue(bindingName) with
            | true, lazyValue ->
                try
                    lazyValue.Value
                with :? InvalidOperationException ->
                    error $"Recursive evaluation of '{runtimeModule.Name}.{bindingName}' is not supported for non-function values."
            | _ ->
                error $"Binding '{bindingName}' was not found in module '{runtimeModule.Name}'."

        and evaluateDefinition (context: RuntimeContext) (moduleName: string) (definition: LetDefinition) : Result<RuntimeValue, EvaluationError> =
            match definition.Body with
            | None ->
                let bindingName = defaultArg definition.Name "<anonymous>"
                error $"Binding '{bindingName}' does not belong to the executable core subset."
            | Some body ->
                let baseScope =
                    { Locals = Map.empty
                      CurrentModule = moduleName
                      Context = context }

                if List.isEmpty definition.Parameters then
                    evaluateExpression baseScope body
                else
                    ok
                        (FunctionValue
                            { Parameters = definition.Parameters |> List.map (fun (parameter: Parameter) -> parameter.Name)
                              Body = body
                              Scope = baseScope })

        for KeyValue(moduleName, runtimeModule) in moduleRuntimes do
            for KeyValue(bindingName, definition) in runtimeModule.Definitions do
                runtimeModule.Values[bindingName] <- lazy (evaluateDefinition context moduleName definition)

            for intrinsicName in runtimeModule.IntrinsicTerms do
                runtimeModule.Values[intrinsicName] <-
                    lazy
                        (match tryCreateIntrinsicTermValue moduleName intrinsicName with
                         | Some value -> ok value
                         | None -> error $"Intrinsic term '{intrinsicName}' is not implemented for module '{moduleName}'.")

        context

    let private resolveEntryPoint (context: RuntimeContext) (entryPoint: string) : Result<string * string, EvaluationError> =
        let segments =
            entryPoint.Split('.', StringSplitOptions.RemoveEmptyEntries)
            |> Array.toList

        match segments with
        | [] ->
            error "Expected a binding name to run."
        | [ bindingName ] ->
            let matches =
                context.Modules
                |> Map.toList
                |> List.choose (fun (moduleName, runtimeModule) ->
                    if runtimeModule.Definitions.ContainsKey(bindingName) then
                        Some(moduleName, bindingName)
                    else
                        None)

            match matches with
            | [] ->
                error $"No executable binding named '{bindingName}' was found."
            | [ matchValue ] ->
                Result.Ok matchValue
            | _ ->
                error $"Binding name '{bindingName}' is ambiguous. Use a fully qualified name."
        | _ ->
            let moduleName = segments |> List.take (segments.Length - 1) |> SyntaxFacts.moduleNameToText
            let bindingName = List.last segments

            match Map.tryFind moduleName context.Modules with
            | Some runtimeModule when runtimeModule.Definitions.ContainsKey(bindingName) ->
                ok (moduleName, bindingName)
            | Some _ ->
                error $"Module '{moduleName}' does not define '{bindingName}'."
            | None ->
                error $"Module '{moduleName}' was not found."

    let evaluateBinding (workspace: WorkspaceCompilation) (entryPoint: string) =
        if workspace.HasErrors then
            error "Cannot evaluate a workspace that already contains diagnostics."
        else
            let context = buildContext workspace

            resolveEntryPoint context entryPoint
            |> Result.bind (fun (moduleName, bindingName) ->
                let runtimeModule = context.Modules[moduleName]

                match runtimeModule.Values.TryGetValue(bindingName) with
                | true, lazyValue -> lazyValue.Value
                | _ -> error $"Binding '{bindingName}' was not found in module '{moduleName}'.")
