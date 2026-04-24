namespace Kappa.Compiler

open System
open System.Collections.Generic

type EvaluationError =
    { Message: string }

type RuntimeOutput =
    { Write: string -> unit
      WriteLine: string -> unit }

module RuntimeOutput =
    let console =
        { Write = Console.Write
          WriteLine = Console.WriteLine }

type RuntimeValue =
    | IntegerValue of int64
    | FloatValue of double
    | BooleanValue of bool
    | StringValue of string
    | CharacterValue of char
    | UnitValue
    | ConstructorFunctionValue of RuntimeConstructor * RuntimeValue list
    | ConstructedValue of RuntimeConstructed
    | IOActionValue of (unit -> Result<RuntimeValue, EvaluationError>)
    | FunctionValue of RuntimeClosure
    | BuiltinFunctionValue of BuiltinFunction
    | RefCellValue of RuntimeRefCell
    | DictionaryValue of RuntimeDictionary
and RuntimeConstructor =
    { Name: string
      QualifiedName: string
      Arity: int
      TypeName: string
      FieldNames: string option list }
and RuntimeConstructed =
    { Constructor: RuntimeConstructor
      Fields: RuntimeValue list }
and RuntimeClosure =
    { Parameters: string list
      Body: KRuntimeExpression
      Scope: RuntimeScope }
and BuiltinFunction =
    { Name: string
      Arguments: RuntimeValue list }
and RuntimeRefCell =
    { mutable Value: RuntimeValue }
and RuntimeDictionary =
    { ModuleName: string
      TraitName: string
      InstanceKey: string }
and RuntimeScope =
    { Locals: Map<string, RuntimeValue>
      CurrentModule: string
      Context: RuntimeContext }
and RuntimeContext =
    { Modules: Map<string, RuntimeModule> }
and RuntimeModule =
    { Name: string
      Definitions: Map<string, KRuntimeBinding>
      Constructors: Map<string, RuntimeConstructor>
      IntrinsicTerms: Set<string>
      Exports: Set<string>
      Imports: ImportSpec list
      Values: Dictionary<string, Lazy<Result<RuntimeValue, EvaluationError>>> }

module RuntimeValue =
    let rec format value =
        match value with
        | IntegerValue value -> string value
        | FloatValue value -> string value
        | BooleanValue true -> "True"
        | BooleanValue false -> "False"
        | StringValue value -> $"\"{value}\""
        | CharacterValue value -> $"'{value}'"
        | UnitValue -> "()"
        | ConstructorFunctionValue(constructor, arguments) ->
            $"<constructor {constructor.Name}/{constructor.Arity} [{arguments.Length}]>"
        | ConstructedValue constructed ->
            match constructed.Constructor.Name, constructed.Fields with
            | constructorName, [] ->
                constructorName
            | "::", [ head; tail ] ->
                $"{format head} :: {format tail}"
            | constructorName, fields ->
                let fieldText = fields |> List.map format |> String.concat " "
                $"{constructorName} {fieldText}"
        | IOActionValue _ -> "<io>"
        | FunctionValue _ -> "<function>"
        | BuiltinFunctionValue builtin -> $"<builtin {builtin.Name}>"
        | RefCellValue _ -> "<ref>"
        | DictionaryValue dictionary -> $"<dict {dictionary.ModuleName}.{dictionary.TraitName} {dictionary.InstanceKey}>"

// Executes KRuntimeIR directly for evaluation, tests, and bootstrap behavior.
module Interpreter =
    let private error message =
        Result.Error { Message = message }

    let private ok value =
        Result.Ok value

    let private runtimeConstructorOf value =
        match value with
        | ConstructorFunctionValue(constructor, _)
        | ConstructedValue { Constructor = constructor } ->
            Some constructor
        | _ ->
            None

    let private tryProjectConstructedField fieldName value =
        match value with
        | ConstructedValue constructed ->
            constructed.Constructor.FieldNames
            |> List.tryFindIndex (function
                | Some candidate -> String.Equals(candidate, fieldName, StringComparison.Ordinal)
                | None -> false)
            |> Option.bind (fun index -> constructed.Fields |> List.tryItem index)
        | _ ->
            None

    let private literalToValue literal =
        match literal with
        | LiteralValue.Integer value -> IntegerValue value
        | LiteralValue.Float value -> FloatValue value
        | LiteralValue.String value -> StringValue value
        | LiteralValue.Character value -> CharacterValue value
        | LiteralValue.Unit -> UnitValue

    let private buildContextWithOutput (output: RuntimeOutput) (workspace: WorkspaceCompilation) =
        let moduleRuntimes =
            workspace.KRuntimeIR
            |> List.map (fun backendModule ->
                let constructors =
                    backendModule.Constructors
                    |> List.map (fun constructor ->
                        constructor.Name,
                        { Name = constructor.Name
                          QualifiedName = $"{backendModule.Name}.{constructor.Name}"
                          Arity = constructor.Arity
                          TypeName = constructor.TypeName
                          FieldNames = constructor.FieldNames })
                    |> Map.ofList

                let definitions =
                    backendModule.Bindings
                    |> List.filter (fun binding -> not binding.Intrinsic)
                    |> List.map (fun binding -> binding.Name, binding)
                    |> Map.ofList

                backendModule.Name,
                { Name = backendModule.Name
                  Definitions = definitions
                  Constructors = constructors
                  IntrinsicTerms = backendModule.IntrinsicTerms |> Set.ofList
                  Exports = backendModule.Exports |> Set.ofList
                  Imports = backendModule.Imports
                  Values = Dictionary<string, Lazy<Result<RuntimeValue, EvaluationError>>>() })
            |> Map.ofList

        let context = { Modules = moduleRuntimes }

        let tryCreateBuiltinFunction name =
            if IntrinsicCatalog.isBuiltinBinaryOperator name then
                Some(BuiltinFunctionValue { Name = name; Arguments = [] })
            else
                None

        let tryCreateIntrinsicTermValue moduleName name =
            let isPreludeModule =
                String.Equals(moduleName, Stdlib.PreludeModuleText, StringComparison.Ordinal)

            match name with
            | "True" when isPreludeModule -> Some(BooleanValue true)
            | "False" when isPreludeModule -> Some(BooleanValue false)
            | "pure"
            | ">>="
            | ">>"
            | "not"
            | "and"
            | "or"
            | "negate"
            | "println"
            | "print"
            | "printInt"
            | "printString"
            | "primitiveIntToString"
            | "newRef"
            | "readRef"
            | "writeRef" when isPreludeModule ->
                Some(BuiltinFunctionValue { Name = name; Arguments = [] })
            | "openFile"
            | "primitiveReadData"
            | "readData"
            | "primitiveCloseFile" ->
                Some(BuiltinFunctionValue { Name = name; Arguments = [] })
            | _ ->
                None

        let constructorValue constructor =
            if constructor.Arity = 0 then
                ConstructedValue
                    { Constructor = constructor
                      Fields = [] }
            else
                ConstructorFunctionValue(constructor, [])

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

        let rec valuesEqual left right =
            match left, right with
            | IntegerValue left, IntegerValue right ->
                left = right
            | FloatValue left, FloatValue right ->
                left = right
            | BooleanValue left, BooleanValue right ->
                left = right
            | StringValue left, StringValue right ->
                left = right
            | CharacterValue left, CharacterValue right ->
                left = right
            | UnitValue, UnitValue ->
                true
            | ConstructedValue left, ConstructedValue right ->
                String.Equals(left.Constructor.QualifiedName, right.Constructor.QualifiedName, StringComparison.Ordinal)
                && List.length left.Fields = List.length right.Fields
                && List.forall2 valuesEqual left.Fields right.Fields
            | _ ->
                false

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
                ok (BooleanValue(valuesEqual left right))
            | "!=", left, right ->
                ok (BooleanValue(not (valuesEqual left right)))
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
            | "is", left, right ->
                match runtimeConstructorOf left, runtimeConstructorOf right with
                | Some leftConstructor, Some rightConstructor ->
                    ok (BooleanValue(String.Equals(leftConstructor.QualifiedName, rightConstructor.QualifiedName, StringComparison.Ordinal)))
                | _ ->
                    error $"Operator 'is' expects a constructed value and a constructor, but got {RuntimeValue.format left} and {RuntimeValue.format right}."
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
            | ConstructedValue _
            | ConstructorFunctionValue _
            | IOActionValue _
            | FunctionValue _
            | BuiltinFunctionValue _
            | RefCellValue _
            | DictionaryValue _ ->
                error $"Cannot interpolate {RuntimeValue.format value} into an f-string."

        let rec evaluateExpression (scope: RuntimeScope) (expression: KRuntimeExpression) : Result<RuntimeValue, EvaluationError> =
            match expression with
            | KRuntimeLiteral literal ->
                ok (literalToValue literal)
            | KRuntimeName segments ->
                resolveName scope segments
            | KRuntimePrefixedString (prefix, parts) ->
                evaluatePrefixedString scope prefix parts
            | KRuntimeClosure (parameters, body) ->
                ok
                    (FunctionValue
                        { Parameters = parameters
                          Body = body
                          Scope = scope })
            | KRuntimeIfThenElse (condition, whenTrue, whenFalse) ->
                evaluateExpression scope condition
                |> Result.bind (function
                    | BooleanValue true -> evaluateExpression scope whenTrue
                    | BooleanValue false -> evaluateExpression scope whenFalse
                    | value ->
                        error $"Expected a Boolean in the if condition, but got {RuntimeValue.format value}.")
            | KRuntimeMatch (scrutinee, cases) ->
                evaluateExpression scope scrutinee
                |> Result.bind (fun value -> evaluateMatch scope value cases)
            | KRuntimeExecute inner ->
                evaluateExpression scope inner
                |> Result.bind (function
                    | IOActionValue action ->
                        action ()
                    | value ->
                        ok value)
            | KRuntimeLet (bindingName, valueExpression, bodyExpression) ->
                evaluateExpression scope valueExpression
                |> Result.bind (fun value ->
                    let nextScope =
                        { scope with
                            Locals = scope.Locals.Add(bindingName, value) }

                    evaluateExpression nextScope bodyExpression)
            | KRuntimeDoScope (_, bodyExpression) ->
                evaluateExpression scope bodyExpression
            | KRuntimeScheduleExit (_, action, bodyExpression) ->
                let bodyResult = evaluateExpression scope bodyExpression
                let exitResult = executeExitAction scope action

                match bodyResult, exitResult with
                | Result.Ok value, Result.Ok () ->
                    ok value
                | Result.Ok _, Result.Error issue ->
                    Result.Error issue
                | Result.Error issue, Result.Ok () ->
                    Result.Error issue
                | Result.Error issue, Result.Error _ ->
                    Result.Error issue
            | KRuntimeSequence (firstExpression, secondExpression) ->
                evaluateExpression scope firstExpression
                |> Result.bind (fun _ -> evaluateExpression scope secondExpression)
            | KRuntimeWhile (conditionExpression, bodyExpression) ->
                let rec loop () =
                    evaluateExpression scope conditionExpression
                    |> Result.bind (function
                        | BooleanValue true ->
                            evaluateExpression scope bodyExpression
                            |> Result.bind (fun _ -> loop ())
                        | BooleanValue false ->
                            ok UnitValue
                        | value ->
                            error $"Expected a Boolean in the while condition, but got {RuntimeValue.format value}.")

                loop ()
            | KRuntimeApply (callee, arguments) ->
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
            | KRuntimeDictionaryValue (moduleName, traitName, instanceKey) ->
                ok
                    (DictionaryValue
                        { ModuleName = moduleName
                          TraitName = traitName
                          InstanceKey = instanceKey })
            | KRuntimeTraitCall (traitName, memberName, dictionaryExpression, arguments) ->
                evaluateExpression scope dictionaryExpression
                |> Result.bind (fun dictionaryValue ->
                    arguments
                    |> List.map (evaluateExpression scope)
                    |> List.fold
                        (fun state next ->
                            match state, next with
                            | Result.Ok values, Result.Ok value -> Result.Ok(value :: values)
                            | Result.Error issue, _ -> Result.Error issue
                            | _, Result.Error issue -> Result.Error issue)
                        (Result.Ok [])
                    |> Result.bind (fun values ->
                        match dictionaryValue with
                        | DictionaryValue dictionary ->
                            let bindingName =
                                TraitRuntime.instanceMemberBindingName
                                    traitName
                                    dictionary.InstanceKey
                                    memberName

                            match Map.tryFind dictionary.ModuleName scope.Context.Modules with
                            | Some runtimeModule ->
                                forceBinding runtimeModule bindingName
                                |> Result.bind (fun memberFunction -> apply memberFunction (List.rev values))
                            | None ->
                                error $"Dictionary module '{dictionary.ModuleName}' is not present in the runtime context."
                        | other ->
                            error $"Expected a dictionary value for trait call '{traitName}.{memberName}', but got {RuntimeValue.format other}."))
            | KRuntimeUnary (operatorName, expression) ->
                evaluateExpression scope expression
                |> Result.bind (fun operand ->
                    if hasExplicitUnqualifiedName scope operatorName then
                        resolveName scope [ operatorName ]
                        |> Result.bind (fun functionValue -> apply functionValue [ operand ])
                    else
                        applyBuiltinUnary operatorName operand)
            | KRuntimeBinary (left, "&&", right) when not (hasExplicitUnqualifiedName scope "&&") ->
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
            | KRuntimeBinary (left, "||", right) when not (hasExplicitUnqualifiedName scope "||") ->
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
            | KRuntimeBinary (left, operatorName, right) ->
                evaluateExpression scope left
                |> Result.bind (fun leftValue ->
                    evaluateExpression scope right
                    |> Result.bind (fun rightValue ->
                        if hasExplicitUnqualifiedName scope operatorName then
                            resolveName scope [ operatorName ]
                            |> Result.bind (fun functionValue -> apply functionValue [ leftValue; rightValue ])
                        else
                            applyBuiltinBinary operatorName leftValue rightValue))

        and evaluatePrefixedString (scope: RuntimeScope) (prefix: string) (parts: KRuntimeStringPart list) =
            if not (String.Equals(prefix, "f", StringComparison.Ordinal)) then
                error $"Prefixed string '{prefix}\"...\"' is not supported by the interpreter yet."
            else
                parts
                |> List.fold
                    (fun state part ->
                        state
                        |> Result.bind (fun segments ->
                            match part with
                            | KRuntimeStringText text ->
                                ok (text :: segments)
                            | KRuntimeStringInterpolation expression ->
                                evaluateExpression scope expression
                                |> Result.bind renderInterpolatedValue
                                |> Result.map (fun text -> text :: segments)))
                    (ok [])
                |> Result.map (fun segments ->
                    segments
                    |> List.rev
                    |> String.concat ""
                    |> StringValue)

        and evaluateMatch (scope: RuntimeScope) (scrutinee: RuntimeValue) (cases: KRuntimeMatchCase list) =
            let rec tryCases (remainingCases: KRuntimeMatchCase list) =
                match remainingCases with
                | [] ->
                    error $"Non-exhaustive match for {RuntimeValue.format scrutinee}."
                | (caseClause: KRuntimeMatchCase) :: rest ->
                    match tryMatchPattern scope scrutinee caseClause.Pattern with
                    | Result.Error issue ->
                        Result.Error issue
                    | Result.Ok None ->
                        tryCases rest
                    | Result.Ok(Some bindings) ->
                        let nextScope =
                            { scope with
                                Locals =
                                    bindings
                                    |> List.fold (fun locals (name, value) -> locals.Add(name, value)) scope.Locals }

                        match caseClause.Guard with
                        | Some guard ->
                            evaluateExpression nextScope guard
                            |> Result.bind (function
                                | BooleanValue true -> evaluateExpression nextScope caseClause.Body
                                | BooleanValue false -> tryCases rest
                                | value ->
                                    error $"Match guards must evaluate to Bool, but got {RuntimeValue.format value}.")
                        | None ->
                            evaluateExpression nextScope caseClause.Body

            tryCases cases

        and tryMatchPattern (scope: RuntimeScope) (value: RuntimeValue) (pattern: KRuntimePattern) =
            match pattern with
            | KRuntimeWildcardPattern ->
                ok (Some [])
            | KRuntimeNamePattern name ->
                ok (Some [ name, value ])
            | KRuntimeLiteralPattern literal ->
                let literalValue = literalToValue literal

                if valuesEqual value literalValue then
                    ok (Some [])
                else
                    ok None
            | KRuntimeOrPattern alternatives ->
                let rec tryAlternatives remaining =
                    match remaining with
                    | [] ->
                        ok None
                    | alternative :: rest ->
                        tryMatchPattern scope value alternative
                        |> Result.bind (function
                            | Some bindings ->
                                ok (Some bindings)
                            | None ->
                                tryAlternatives rest)

                tryAlternatives alternatives
            | KRuntimeConstructorPattern(nameSegments, argumentPatterns) ->
                resolvePatternConstructor scope nameSegments
                |> Result.bind (fun constructor ->
                    match value with
                    | BooleanValue booleanValue
                        when constructor.Arity = 0
                             && String.Equals(constructor.TypeName, "Bool", StringComparison.Ordinal)
                             && List.isEmpty argumentPatterns ->
                        let expectedValue = String.Equals(constructor.Name, "True", StringComparison.Ordinal)

                        if booleanValue = expectedValue then
                            ok (Some [])
                        else
                            ok None
                    | ConstructedValue constructed
                        when String.Equals(constructor.QualifiedName, constructed.Constructor.QualifiedName, StringComparison.Ordinal)
                             && List.length argumentPatterns = List.length constructed.Fields ->
                        let rec gatherBindings patterns fields acc =
                            match patterns, fields with
                            | [], [] ->
                                ok (Some(List.rev acc))
                            | patternHead :: remainingPatterns, fieldHead :: remainingFields ->
                                tryMatchPattern scope fieldHead patternHead
                                |> Result.bind (function
                                    | None ->
                                        ok None
                                    | Some bindings ->
                                        gatherBindings remainingPatterns remainingFields (List.rev bindings @ acc))
                            | _ ->
                                ok None

                        gatherBindings argumentPatterns constructed.Fields []
                    | _ ->
                        ok None)

        and apply (functionValue: RuntimeValue) (arguments: RuntimeValue list) : Result<RuntimeValue, EvaluationError> =
            match functionValue with
            | FunctionValue closure ->
                applyClosure closure arguments
            | ConstructorFunctionValue(constructor, existingArguments) ->
                applyConstructor constructor existingArguments arguments
            | BuiltinFunctionValue builtin ->
                applyBuiltinFunction builtin arguments
            | value ->
                error $"Cannot apply {RuntimeValue.format value} as a function."

        and applyConstructor (constructor: RuntimeConstructor) (existingArguments: RuntimeValue list) (arguments: RuntimeValue list) =
            let rec invoke collected remainingArguments =
                match remainingArguments with
                | [] ->
                    if List.length collected = constructor.Arity then
                        ok
                            (ConstructedValue
                                { Constructor = constructor
                                  Fields = collected })
                    else
                        ok (ConstructorFunctionValue(constructor, collected))
                | argument :: rest ->
                    let nextArguments = collected @ [ argument ]

                    if List.length nextArguments > constructor.Arity then
                        error $"Constructor '{constructor.Name}' received too many arguments."
                    elif List.length nextArguments = constructor.Arity then
                        let value =
                            ConstructedValue
                                { Constructor = constructor
                                  Fields = nextArguments }

                        if List.isEmpty rest then
                            ok value
                        else
                            apply value rest
                    else
                        invoke nextArguments rest

            invoke existingArguments arguments

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

        and executeIoAction value =
            match value with
            | IOActionValue action ->
                action ()
            | other ->
                error $"Expected an IO action, but got {RuntimeValue.format other}."

        and executeExitAction scope action =
            match action with
            | KRuntimeDeferred expression ->
                evaluateExpression scope expression
                |> Result.bind executeIoAction
                |> Result.map (fun _ -> ())
            | KRuntimeRelease(_, releaseExpression, resourceExpression) ->
                evaluateExpression scope releaseExpression
                |> Result.bind (fun releaseValue ->
                    evaluateExpression scope resourceExpression
                    |> Result.bind (fun resourceValue ->
                        apply releaseValue [ resourceValue ]
                        |> Result.bind executeIoAction
                        |> Result.map (fun _ -> ())))

        and toUnitIoAction (action: unit -> unit) =
            ok
                (Some(
                    IOActionValue(fun () ->
                        action ()
                        ok UnitValue)
                ))

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
            | "pure", [ value ] ->
                ok (Some(IOActionValue(fun () -> ok value)))
            | "pure", arguments when List.length arguments < 1 ->
                ok None
            | "pure", _ ->
                error "Intrinsic 'pure' received too many arguments."
            | ">>=", [ action; continuation ] ->
                ok
                    (Some(
                        IOActionValue(fun () ->
                            executeIoAction action
                            |> Result.bind (fun value ->
                                apply continuation [ value ]
                                |> Result.bind executeIoAction))
                    ))
            | ">>=", arguments when List.length arguments < 2 ->
                ok None
            | ">>=", _ ->
                error "Intrinsic '>>=' received too many arguments."
            | ">>", [ leftAction; rightAction ] ->
                ok
                    (Some(
                        IOActionValue(fun () ->
                            executeIoAction leftAction
                            |> Result.bind (fun _ -> executeIoAction rightAction))
                    ))
            | ">>", arguments when List.length arguments < 2 ->
                ok None
            | ">>", _ ->
                error "Intrinsic '>>' received too many arguments."
            | "print", [ StringValue value ] ->
                toUnitIoAction (fun () -> output.Write(value))
            | "print", arguments when List.length arguments < 1 ->
                ok None
            | "print", [ value ] ->
                error $"Intrinsic 'print' expects a String, but got {RuntimeValue.format value}."
            | "print", _ ->
                error "Intrinsic 'print' received too many arguments."
            | "println", [ StringValue value ] ->
                toUnitIoAction (fun () -> output.WriteLine(value))
            | "println", arguments when List.length arguments < 1 ->
                ok None
            | "println", [ value ] ->
                error $"Intrinsic 'println' expects a String, but got {RuntimeValue.format value}."
            | "println", _ ->
                error "Intrinsic 'println' received too many arguments."
            | "printInt", [ IntegerValue value ] ->
                toUnitIoAction (fun () -> output.WriteLine(string value))
            | "printInt", arguments when List.length arguments < 1 ->
                ok None
            | "printInt", [ value ] ->
                error $"Intrinsic 'printInt' expects an Int, but got {RuntimeValue.format value}."
            | "printInt", _ ->
                error "Intrinsic 'printInt' received too many arguments."
            | "printString", [ StringValue value ] ->
                toUnitIoAction (fun () -> output.Write(value))
            | "printString", arguments when List.length arguments < 1 ->
                ok None
            | "printString", [ value ] ->
                error $"Intrinsic 'printString' expects a String, but got {RuntimeValue.format value}."
            | "printString", _ ->
                error "Intrinsic 'printString' received too many arguments."
            | "primitiveIntToString", [ IntegerValue value ] ->
                ok (Some(StringValue(string value)))
            | "primitiveIntToString", arguments when List.length arguments < 1 ->
                ok None
            | "primitiveIntToString", [ value ] ->
                error $"Intrinsic 'primitiveIntToString' expects an Int, but got {RuntimeValue.format value}."
            | "primitiveIntToString", _ ->
                error "Intrinsic 'primitiveIntToString' received too many arguments."
            | "openFile", [ StringValue value ] ->
                ok (Some(IOActionValue(fun () -> ok (StringValue($"<file:{value}>")))))
            | "openFile", arguments when List.length arguments < 1 ->
                ok None
            | "openFile", [ value ] ->
                error $"Intrinsic 'openFile' expects a String, but got {RuntimeValue.format value}."
            | "openFile", _ ->
                error "Intrinsic 'openFile' received too many arguments."
            | ("primitiveReadData" | "readData"), [ _ ] ->
                ok (Some(IOActionValue(fun () -> ok (StringValue "chunk"))))
            | ("primitiveReadData" | "readData"), arguments when List.length arguments < 1 ->
                ok None
            | ("primitiveReadData" | "readData"), _ ->
                error "Intrinsic 'readData' received too many arguments."
            | "primitiveCloseFile", [ _ ] ->
                toUnitIoAction (fun () -> output.Write("closed"))
            | "primitiveCloseFile", arguments when List.length arguments < 1 ->
                ok None
            | "primitiveCloseFile", _ ->
                error "Intrinsic 'primitiveCloseFile' received too many arguments."
            | "newRef", [ value ] ->
                ok (Some(IOActionValue(fun () -> ok (RefCellValue { Value = value }))))
            | "newRef", arguments when List.length arguments < 1 ->
                ok None
            | "newRef", _ ->
                error "Intrinsic 'newRef' received too many arguments."
            | "readRef", [ RefCellValue cell ] ->
                ok (Some(IOActionValue(fun () -> ok cell.Value)))
            | "readRef", arguments when List.length arguments < 1 ->
                ok None
            | "readRef", [ value ] ->
                error $"Intrinsic 'readRef' expects a Ref, but got {RuntimeValue.format value}."
            | "readRef", _ ->
                error "Intrinsic 'readRef' received too many arguments."
            | "writeRef", [ RefCellValue cell; value ] ->
                ok
                    (Some(
                        IOActionValue(fun () ->
                            cell.Value <- value
                            ok UnitValue)
                    ))
            | "writeRef", arguments when List.length arguments < 2 ->
                ok None
            | "writeRef", [ referenceValue; _ ] ->
                error $"Intrinsic 'writeRef' expects a Ref as the first argument, but got {RuntimeValue.format referenceValue}."
            | "writeRef", _ ->
                error "Intrinsic 'writeRef' received too many arguments."
            | name, [ left; right ] when IntrinsicCatalog.isBuiltinBinaryOperator name ->
                applyBuiltinBinary name left right
                |> Result.map Some
            | name, arguments when IntrinsicCatalog.isBuiltinBinaryOperator name && List.length arguments < 2 ->
                ok None
            | name, _ when IntrinsicCatalog.isBuiltinBinaryOperator name ->
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
            | receiverName :: memberSegments ->
                match resolveUnqualifiedName scope receiverName with
                | Result.Ok receiverValue ->
                    let rec project current remaining =
                        match remaining with
                        | [] ->
                            ok current
                        | memberName :: rest ->
                            match tryProjectConstructedField memberName current with
                            | Some projected ->
                                project projected rest
                            | None ->
                                error $"Value '{receiverName}' has no projected member '{memberName}'."

                    project receiverValue memberSegments
                | Result.Error _ ->
                    let qualifierSegments = segments |> List.take (segments.Length - 1)
                    let bindingName = List.last segments
                    resolveQualifiedName scope qualifierSegments bindingName

        and resolvePatternConstructor (scope: RuntimeScope) (segments: string list) : Result<RuntimeConstructor, EvaluationError> =
            let currentModule = scope.Context.Modules[scope.CurrentModule]

            let resolveImportedConstructorModules name =
                currentModule.Imports
                |> List.choose (fun spec ->
                    match spec.Source with
                    | Url _ ->
                        None
                    | Dotted moduleSegments ->
                        let importedModuleName = SyntaxFacts.moduleNameToText moduleSegments

                        match Map.tryFind importedModuleName scope.Context.Modules with
                        | Some importedModule when selectionImportsConstructorName importedModule spec.Selection name ->
                            Some(importedModuleName, importedModule)
                        | _ ->
                            None)
                |> List.distinctBy fst
                |> List.map snd

            match segments with
            | [] ->
                error "Encountered an empty constructor pattern."
            | [ name ] ->
                if currentModule.Constructors.ContainsKey(name) then
                    ok currentModule.Constructors[name]
                else
                    match resolveImportedConstructorModules name with
                    | [] ->
                        error $"Pattern '{name}' is not in scope as a constructor."
                    | [ importedModule ] ->
                        ok importedModule.Constructors[name]
                    | _ ->
                        error $"Pattern constructor '{name}' is ambiguous across imported modules."
            | _ ->
                let qualifierSegments = segments |> List.take (segments.Length - 1)
                let constructorName = List.last segments
                let qualifierText = SyntaxFacts.moduleNameToText qualifierSegments

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
                | Some runtimeModule when runtimeModule.Name = scope.CurrentModule || runtimeModule.Exports.Contains(constructorName) ->
                    match runtimeModule.Constructors.TryGetValue(constructorName) with
                    | true, constructor ->
                        ok constructor
                    | _ ->
                        let patternName = String.concat "." segments
                        error $"Pattern '{patternName}' does not resolve to a constructor."
                | Some runtimeModule ->
                    error $"'{constructorName}' is not exported from module '{runtimeModule.Name}'."
                | None ->
                    error $"Module qualifier '{qualifierText}' is not in scope."

        and itemImportsTermName (item: ImportItem) =
            item.Namespace.IsNone || item.Namespace = Some ImportNamespace.Term

        and itemImportsConstructorName (item: ImportItem) =
            item.Namespace = Some ImportNamespace.Constructor

        and selectionImportsTermName (importedModule: RuntimeModule) (selection: ImportSelection) (name: string) =
            let exportsTerm =
                importedModule.Definitions.ContainsKey(name)
                || importedModule.IntrinsicTerms.Contains(name)

            match selection with
            | QualifiedOnly ->
                false
            | Items items ->
                items
                |> List.exists (fun item ->
                    String.Equals(item.Name, name, StringComparison.Ordinal)
                    && itemImportsTermName item)
            | All ->
                exportsTerm && importedModule.Exports.Contains(name)
            | AllExcept excludedNames ->
                not (List.contains name excludedNames)
                && exportsTerm
                && importedModule.Exports.Contains(name)

        and selectionImportsConstructorName (importedModule: RuntimeModule) (selection: ImportSelection) (name: string) =
            let exportsConstructor =
                importedModule.Constructors.ContainsKey(name)
                && importedModule.Exports.Contains(name)

            match selection with
            | QualifiedOnly ->
                false
            | Items items ->
                exportsConstructor
                && (items
                    |> List.exists (fun item ->
                        String.Equals(item.Name, name, StringComparison.Ordinal)
                        && itemImportsConstructorName item))
            | All
            | AllExcept _ ->
                false

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
                            selectionImportsTermName importedModule spec.Selection name
                            || selectionImportsConstructorName importedModule spec.Selection name

                        if importsName then
                            Some(importedModuleName, importedModule)
                        else
                            None)
            |> List.distinctBy fst
            |> List.map snd

        and hasExplicitUnqualifiedName (scope: RuntimeScope) (name: string) =
            let currentModule = scope.Context.Modules[scope.CurrentModule]

            Map.containsKey name scope.Locals
            || currentModule.Definitions.ContainsKey(name)
            || currentModule.Constructors.ContainsKey(name)
            || not (List.isEmpty (findImportedModulesForName scope name))

        and resolveUnqualifiedName (scope: RuntimeScope) (name: string) : Result<RuntimeValue, EvaluationError> =
            match Map.tryFind name scope.Locals with
            | Some value ->
                ok value
            | None ->
                let currentModule = scope.Context.Modules[scope.CurrentModule]

                if currentModule.Definitions.ContainsKey(name) then
                    forceBinding currentModule name
                elif currentModule.IntrinsicTerms.Contains(name) then
                    forceBinding currentModule name
                elif currentModule.Constructors.ContainsKey(name) then
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
            | _ when runtimeModule.Constructors.ContainsKey(bindingName) ->
                ok (constructorValue runtimeModule.Constructors[bindingName])
            | _ ->
                error $"Binding '{bindingName}' was not found in module '{runtimeModule.Name}'."

        and evaluateDefinition (context: RuntimeContext) (moduleName: string) (definition: KRuntimeBinding) : Result<RuntimeValue, EvaluationError> =
            match definition.Body with
            | None ->
                error $"Binding '{definition.Name}' does not belong to the executable backend subset."
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
                            { Parameters = definition.Parameters |> List.map (fun parameter -> parameter.Name)
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

    let private buildContext workspace =
        buildContextWithOutput RuntimeOutput.console workspace

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

    let private evaluateBindingWithContext (context: RuntimeContext) (entryPoint: string) =
        resolveEntryPoint context entryPoint
        |> Result.bind (fun (moduleName, bindingName) ->
            let runtimeModule = context.Modules[moduleName]

            match runtimeModule.Values.TryGetValue(bindingName) with
            | true, lazyValue -> lazyValue.Value
            | _ -> error $"Binding '{bindingName}' was not found in module '{moduleName}'.")

    let evaluateBindingWithOutput (workspace: WorkspaceCompilation) (output: RuntimeOutput) (entryPoint: string) =
        if workspace.HasErrors then
            error "Cannot evaluate a workspace that already contains diagnostics."
        else
            let context = buildContextWithOutput output workspace
            evaluateBindingWithContext context entryPoint

    let evaluateBinding (workspace: WorkspaceCompilation) (entryPoint: string) =
        evaluateBindingWithOutput workspace RuntimeOutput.console entryPoint

    let executeBindingWithOutput (workspace: WorkspaceCompilation) (output: RuntimeOutput) (entryPoint: string) =
        let rec execute value =
            match value with
            | IOActionValue action ->
                action ()
                |> Result.bind execute
            | other ->
                ok other

        evaluateBindingWithOutput workspace output entryPoint
        |> Result.bind execute

    let executeBinding (workspace: WorkspaceCompilation) (entryPoint: string) =
        executeBindingWithOutput workspace RuntimeOutput.console entryPoint

    let shouldPrintResult value =
        match value with
        | UnitValue -> false
        | _ -> true
