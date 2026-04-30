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

type RuntimeUnicodeDecodeError =
    { Message: string }

type RuntimeValue =
    | IntegerValue of int64
    | FloatValue of double
    | BooleanValue of bool
    | StringValue of string
    | CharacterValue of string
    | GraphemeValue of string
    | ByteValue of byte
    | BytesValue of byte array
    | UnicodeDecodeErrorValue of RuntimeUnicodeDecodeError
    | UnicodeVersionValue of string
    | NormalizationFormValue of UnicodeText.NormalizationFormName
    | HashSeedValue of int64
    | HashStateValue of int64
    | HashCodeValue of int64
    | UnitValue
    | EffectLabelValue of RuntimeEffectLabel
    | EffectOperationValue of RuntimeEffectOperationValue
    | ConstructorFunctionValue of RuntimeConstructor * RuntimeValue list
    | ConstructedValue of RuntimeConstructed
    | NativeFunctionValue of RuntimeNativeFunction
    | IOActionValue of (unit -> Result<RuntimeActionResult, EvaluationError>)
    | FunctionValue of RuntimeClosure
    | BuiltinFunctionValue of BuiltinFunction
    | RefCellValue of RuntimeRefCell
    | DictionaryValue of RuntimeDictionary
and RuntimeEffectLabel =
    { Name: string
      InterfaceId: string
      LabelId: string
      Operations: Map<string, RuntimeEffectOperationMetadata> }
and RuntimeEffectOperationMetadata =
    { OperationId: string
      Name: string
      ResumptionQuantity: Quantity option
      ParameterArity: int }
and RuntimeEffectOperationValue =
    { Label: RuntimeEffectLabel
      Operation: RuntimeEffectOperationMetadata
      AppliedArguments: RuntimeValue list }
and RuntimeActionResult =
    | RuntimeActionReturn of RuntimeValue
    | RuntimeActionRequest of RuntimeEffectRequest
and RuntimeEffectRequest =
    { Label: RuntimeEffectLabel
      Operation: RuntimeEffectOperationMetadata
      Arguments: RuntimeValue list
      ContinueWith: RuntimeValue -> Result<RuntimeActionResult, EvaluationError> }
and RuntimeNativeFunction =
    { Name: string
      Arity: int
      AppliedArguments: RuntimeValue list
      Invoke: RuntimeValue list -> Result<RuntimeValue, EvaluationError> }
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
      InstanceKey: string
      Captures: RuntimeValue list }
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

type RuntimeSession =
    private
        { Context: RuntimeContext
          ApplyRuntimeValue: RuntimeValue -> RuntimeValue list -> Result<RuntimeValue, EvaluationError>
          ExecuteRuntimeValue: RuntimeValue -> Result<RuntimeValue, EvaluationError> }

module RuntimeValue =
    let rec format value =
        match value with
        | IntegerValue value -> string value
        | FloatValue value -> string value
        | BooleanValue true -> "True"
        | BooleanValue false -> "False"
        | StringValue value -> $"\"{value}\""
        | CharacterValue value -> $"'{value}'"
        | GraphemeValue value -> $"g'{value}'"
        | ByteValue value -> $"b'\\x{int value:X2}'"
        | BytesValue bytes ->
            let body =
                bytes
                |> Array.map (fun value -> sprintf "%02X" value)
                |> String.concat " "

            $"#bytes[{body}]"
        | UnicodeDecodeErrorValue _ -> "<unicode-decode-error>"
        | UnicodeVersionValue value -> value
        | NormalizationFormValue value ->
            match value with
            | UnicodeText.NFC -> "NFC"
            | UnicodeText.NFD -> "NFD"
            | UnicodeText.NFKC -> "NFKC"
            | UnicodeText.NFKD -> "NFKD"
        | HashSeedValue value -> $"HashSeed({value})"
        | HashStateValue value -> $"HashState({value})"
        | HashCodeValue value -> $"HashCode({value})"
        | UnitValue -> "()"
        | EffectLabelValue label -> $"<effect-label {label.Name}>"
        | EffectOperationValue operation ->
            $"<effect-op {operation.Label.Name}.{operation.Operation.Name}/{operation.Operation.ParameterArity} [{operation.AppliedArguments.Length}]>"
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
        | NativeFunctionValue nativeFunction -> $"<native {nativeFunction.Name}>"
        | FunctionValue _ -> "<function>"
        | BuiltinFunctionValue builtin -> $"<builtin {builtin.Name}>"
        | RefCellValue _ -> "<ref>"
        | DictionaryValue dictionary -> $"<dict {dictionary.ModuleName}.{dictionary.TraitName} {dictionary.InstanceKey}>"

// Executes KRuntimeIR directly for evaluation, tests, and bootstrap behavior.
module Interpreter =
    let private error (message: string) : Result<'a, EvaluationError> =
        Result.Error({ Message = message }: EvaluationError)

    let private ok value =
        Result.Ok value

    let private resumptionQuantityPermitsMultiple quantity =
        match quantity |> Option.defaultValue QuantityOne |> ResourceModel.ResourceQuantity.ofSurface with
        | ResourceModel.ResourceQuantity.Interval(_, Some maximum) ->
            maximum > 1
        | ResourceModel.ResourceQuantity.Interval(_, None) ->
            true
        | ResourceModel.ResourceQuantity.Borrow _
        | ResourceModel.ResourceQuantity.Variable _ ->
            false

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
        | LiteralValue.Grapheme value -> GraphemeValue value
        | LiteralValue.Byte value -> ByteValue value
        | LiteralValue.Unit -> UnitValue

    let private buildSessionWithOutputFromModules (output: RuntimeOutput) (runtimeModules: KRuntimeModule list) =
        let moduleRuntimes =
            runtimeModules
            |> List.groupBy (fun backendModule -> backendModule.Name)
            |> List.map (fun (moduleName, fragments) ->
                let constructors =
                    fragments
                    |> List.collect (fun backendModule ->
                        backendModule.Constructors
                        |> List.map (fun constructor ->
                            constructor.Name,
                            { Name = constructor.Name
                              QualifiedName = $"{backendModule.Name}.{constructor.Name}"
                              Arity = constructor.Arity
                              TypeName = constructor.TypeName
                              FieldNames = constructor.FieldNames }))
                    |> Map.ofList

                let definitions =
                    fragments
                    |> List.collect (fun backendModule ->
                        backendModule.Bindings
                        |> List.filter (fun binding -> not binding.Intrinsic)
                        |> List.map (fun binding -> binding.Name, binding))
                    |> Map.ofList

                let intrinsicTerms =
                    fragments
                    |> List.collect (fun backendModule -> backendModule.IntrinsicTerms)
                    |> Set.ofList

                let exports =
                    fragments
                    |> List.collect (fun backendModule -> backendModule.Exports)
                    |> Set.ofList

                let imports =
                    fragments
                    |> List.collect (fun backendModule -> backendModule.Imports)

                moduleName,
                { Name = moduleName
                  Definitions = definitions
                  Constructors = constructors
                  IntrinsicTerms = intrinsicTerms
                  Exports = exports
                  Imports = imports
                  Values = Dictionary<string, Lazy<Result<RuntimeValue, EvaluationError>>>() })
            |> Map.ofList

        let context = { Modules = moduleRuntimes }

        let tryFindConstructor moduleName constructorName =
            context.Modules
            |> Map.tryFind moduleName
            |> Option.bind (fun runtimeModule -> runtimeModule.Constructors |> Map.tryFind constructorName)

        let constructValue moduleName constructorName fields =
            match tryFindConstructor moduleName constructorName with
            | Some constructor when List.length fields = constructor.Arity ->
                ok
                    (ConstructedValue
                        { Constructor = constructor
                          Fields = fields })
            | Some constructor ->
                error
                    $"Constructor '{moduleName}.{constructorName}' expected {constructor.Arity} fields but received {List.length fields}."
            | None ->
                error $"Constructor '{moduleName}.{constructorName}' is not available at runtime."

        let constructPreludeValue constructorName fields =
            constructValue Stdlib.PreludeModuleText constructorName fields

        let optionValue value =
            match value with
            | Some inner -> constructPreludeValue "Some" [ inner ]
            | None -> constructPreludeValue "None" []

        let resultOk value = constructPreludeValue "Ok" [ value ]
        let resultError value = constructPreludeValue "Err" [ value ]

        let unicodeDecodeError message =
            UnicodeDecodeErrorValue({ Message = message }: RuntimeUnicodeDecodeError)

        let orderingValue comparison =
            if comparison < 0 then
                constructPreludeValue "LT" []
            elif comparison > 0 then
                constructPreludeValue "GT" []
            else
                constructPreludeValue "EQ" []

        let compareByteArrays (left: byte array) (right: byte array) =
            let mutable comparison = 0
            let mutable index = 0
            let maxShared = min left.Length right.Length

            while comparison = 0 && index < maxShared do
                comparison <- compare left[index] right[index]
                index <- index + 1

            if comparison <> 0 then
                comparison
            else
                compare left.Length right.Length

        let tryViewSemanticBool value =
            match value with
            | BooleanValue value ->
                Some value
            | ConstructedValue constructed
                when List.isEmpty constructed.Fields
                     && String.Equals(constructed.Constructor.TypeName, "Bool", StringComparison.Ordinal) ->
                match constructed.Constructor.QualifiedName with
                | qualifiedName when String.Equals(qualifiedName, $"{Stdlib.PreludeModuleText}.True", StringComparison.Ordinal) -> Some true
                | qualifiedName when String.Equals(qualifiedName, $"{Stdlib.PreludeModuleText}.False", StringComparison.Ordinal) -> Some false
                | _ -> None
            | _ ->
                None

        let tryCompareValues left right =
            match tryViewSemanticBool left, tryViewSemanticBool right with
            | Some left, Some right ->
                Some(compare left right)
            | _ ->
                match left, right with
                | IntegerValue left, IntegerValue right -> Some(compare left right)
                | FloatValue left, FloatValue right -> Some(compare left right)
                | StringValue left, StringValue right -> Some(UnicodeText.compareScalarSequences left right)
                | CharacterValue left, CharacterValue right -> Some(UnicodeText.compareScalarSequences left right)
                | GraphemeValue left, GraphemeValue right -> Some(UnicodeText.compareScalarSequences left right)
                | ByteValue left, ByteValue right -> Some(compare left right)
                | BytesValue left, BytesValue right -> Some(compareByteArrays left right)
                | HashCodeValue left, HashCodeValue right -> Some(compare left right)
                | _ -> None

        let tryCreateBuiltinFunction name =
            if IntrinsicCatalog.isBuiltinBinaryOperator name then
                Some(BuiltinFunctionValue { Name = name; Arguments = [] })
            else
                None

        let tryCreateIntrinsicTermValue moduleName name =
            let isPreludeModule =
                String.Equals(moduleName, Stdlib.PreludeModuleText, StringComparison.Ordinal)
            let isUnicodeModule = String.Equals(moduleName, "std.unicode", StringComparison.Ordinal)
            let isHashModule = String.Equals(moduleName, "std.hash", StringComparison.Ordinal)
            let isTestingModule = String.Equals(moduleName, "std.testing", StringComparison.Ordinal)

            match name with
            | "True" when isPreludeModule -> Some(BooleanValue true)
            | "False" when isPreludeModule -> Some(BooleanValue false)
            | intrinsicName when isPreludeModule && intrinsicName = IntrinsicCatalog.BuiltinPreludeShowIntrinsicName ->
                Some(BuiltinFunctionValue { Name = "show"; Arguments = [] })
            | intrinsicName when isPreludeModule && intrinsicName = IntrinsicCatalog.BuiltinPreludeCompareIntrinsicName ->
                Some(BuiltinFunctionValue { Name = "compare"; Arguments = [] })
            | "pure"
            | ">>="
            | ">>"
            | "runPure"
            | "not"
            | "and"
            | "or"
            | "negate"
            | "println"
            | "print"
            | "compare"
            | "show"
            | "primitiveIntToString"
            | "unsafeConsume"
            | "newRef"
            | "readRef"
            | "writeRef" when isPreludeModule ->
                Some(BuiltinFunctionValue { Name = name; Arguments = [] })
            | "openFile"
            | "primitiveReadData"
            | "readData"
            | "primitiveCloseFile" ->
                Some(BuiltinFunctionValue { Name = name; Arguments = [] })
            | "unicodeVersion" when isUnicodeModule ->
                Some(UnicodeVersionValue "15.1.0")
            | "NFC" when isUnicodeModule ->
                Some(NormalizationFormValue UnicodeText.NFC)
            | "NFD" when isUnicodeModule ->
                Some(NormalizationFormValue UnicodeText.NFD)
            | "NFKC" when isUnicodeModule ->
                Some(NormalizationFormValue UnicodeText.NFKC)
            | "NFKD" when isUnicodeModule ->
                Some(NormalizationFormValue UnicodeText.NFKD)
            | ("utf8Bytes"
              | "decodeUtf8"
              | "decodeUtf8Lossy"
              | "byteLength"
              | "scalarCount"
              | "graphemeCount"
              | "scalarValue"
              | "unicodeScalarFromValue"
              | "scalarToString"
              | "graphemeToString"
              | "graphemeFromString"
              | "normalize"
              | "isNormalized"
              | "canonicalEquivalent"
              | "show") when isUnicodeModule ->
                Some(BuiltinFunctionValue { Name = name; Arguments = [] })
            | "defaultHashSeed" when isHashModule ->
                Some(HashSeedValue 1469598103934665603L)
            | ("newHashState"
              | "finishHashState"
              | "hashUnit"
              | "hashBool"
              | "hashUnicodeScalar"
              | "hashGrapheme"
              | "hashString"
              | "hashBytes"
              | "hashInt"
              | "hashInteger"
              | "hashFloatRaw"
              | "hashDoubleRaw"
              | "hashNatTag") when isHashModule ->
                Some(BuiltinFunctionValue { Name = name; Arguments = [] })
            | "failNow" when isTestingModule ->
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
            match tryViewSemanticBool left, tryViewSemanticBool right with
            | Some left, Some right ->
                left = right
            | _ ->
                match left, right with
                | IntegerValue left, IntegerValue right ->
                    left = right
                | FloatValue left, FloatValue right ->
                    left = right
                | StringValue left, StringValue right ->
                    left = right
                | CharacterValue left, CharacterValue right ->
                    left = right
                | GraphemeValue left, GraphemeValue right ->
                    left = right
                | ByteValue left, ByteValue right ->
                    left = right
                | BytesValue left, BytesValue right ->
                    compareByteArrays left right = 0
                | UnicodeDecodeErrorValue left, UnicodeDecodeErrorValue right ->
                    left.Message = right.Message
                | UnicodeVersionValue left, UnicodeVersionValue right ->
                    left = right
                | NormalizationFormValue left, NormalizationFormValue right ->
                    left = right
                | HashSeedValue left, HashSeedValue right ->
                    left = right
                | HashStateValue left, HashStateValue right ->
                    left = right
                | HashCodeValue left, HashCodeValue right ->
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
            | ("<" | "<=" | ">" | ">=") as operatorName, left, right ->
                match tryCompareValues left right with
                | Some comparison ->
                    let result =
                        match operatorName with
                        | "<" -> comparison < 0
                        | "<=" -> comparison <= 0
                        | ">" -> comparison > 0
                        | ">=" -> comparison >= 0
                        | _ -> false

                    ok (BooleanValue result)
                | None ->
                    error $"Operator '{operatorName}' is not supported for {RuntimeValue.format left} and {RuntimeValue.format right}."
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
            | GraphemeValue value -> ok value
            | ByteValue value -> ok (string value)
            | BytesValue value ->
                ok (RuntimeValue.format (BytesValue value))
            | UnicodeDecodeErrorValue _ ->
                ok (RuntimeValue.format value)
            | UnicodeVersionValue value -> ok value
            | NormalizationFormValue value -> ok (RuntimeValue.format (NormalizationFormValue value))
            | HashSeedValue value -> ok (string value)
            | HashStateValue value -> ok (string value)
            | HashCodeValue value -> ok (string value)
            | UnitValue -> ok "()"
            | EffectLabelValue _
            | EffectOperationValue _
            | ConstructedValue _
            | ConstructorFunctionValue _
            | IOActionValue _
            | NativeFunctionValue _
            | FunctionValue _
            | BuiltinFunctionValue _
            | RefCellValue _
            | DictionaryValue _ ->
                error $"Cannot interpolate {RuntimeValue.format value} into an f-string."

        let rec evaluateExpression (scope: RuntimeScope) (expression: KRuntimeExpression) : Result<RuntimeValue, EvaluationError> =
            match expression with
            | KRuntimeLiteral literal ->
                ok (literalToValue literal)
            | KRuntimeEffectLabel(labelName, interfaceId, labelId, operations) ->
                ok
                    (EffectLabelValue
                        { Name = labelName
                          InterfaceId = interfaceId
                          LabelId = labelId
                          Operations =
                            operations
                            |> List.map (fun operation ->
                                operation.Name,
                                { OperationId = operation.OperationId
                                  Name = operation.Name
                                  ResumptionQuantity = operation.ResumptionQuantity
                                  ParameterArity = operation.ParameterArity })
                            |> Map.ofList })
            | KRuntimeEffectOperation(labelExpression, operationId, operationName) ->
                evaluateExpression scope labelExpression
                |> Result.bind (function
                    | EffectLabelValue label ->
                        match label.Operations |> Map.tryFind operationName with
                        | Some operation when String.Equals(operation.OperationId, operationId, StringComparison.Ordinal) ->
                            ok
                                (EffectOperationValue
                                    { Label = label
                                      Operation = operation
                                      AppliedArguments = [] })
                        | Some _ ->
                            error $"Effect label '{label.Name}' does not declare operation id '{operationId}' for '{operationName}'."
                        | None ->
                            error $"Effect label '{label.Name}' does not declare operation '{operationName}'."
                    | other ->
                        error $"Expected an effect label for '{operationName}', but got {RuntimeValue.format other}.")
            | KRuntimeHandle(isDeep, labelExpression, bodyExpression, returnClause, operationClauses) ->
                evaluateExpression scope labelExpression
                |> Result.bind (function
                    | EffectLabelValue label ->
                        evaluateExpression scope bodyExpression
                        |> Result.map (fun bodyValue ->
                            IOActionValue(fun () ->
                                executeIoAction bodyValue
                                |> Result.bind (handleActionResult scope isDeep label returnClause operationClauses)))
                    | other ->
                        error $"Expected an effect label in handle, but got {RuntimeValue.format other}.")
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
                    | value ->
                        match tryViewSemanticBool value with
                        | Some true -> evaluateExpression scope whenTrue
                        | Some false -> evaluateExpression scope whenFalse
                        | None ->
                            error $"Expected a Boolean in the if condition, but got {RuntimeValue.format value}.")
            | KRuntimeMatch (scrutinee, cases) ->
                evaluateExpression scope scrutinee
                |> Result.bind (fun value -> evaluateMatch scope value cases)
            | KRuntimeExecute inner ->
                evaluateExpression scope inner
                |> Result.bind executeIoAction
                |> Result.bind (function
                    | RuntimeActionReturn value ->
                        ok value
                    | RuntimeActionRequest request ->
                        error $"Unhandled effect operation '{request.Label.Name}.{request.Operation.Name}'.")
            | KRuntimeLet (bindingName, valueExpression, bodyExpression) ->
                match valueExpression with
                | KRuntimeClosure(parameters, body) ->
                    let rec closureValue =
                        FunctionValue
                            { Parameters = parameters
                              Body = body
                              Scope = nextScope }
                    and nextScope =
                        { scope with
                            Locals = scope.Locals.Add(bindingName, closureValue) }

                    evaluateExpression nextScope bodyExpression
                | _ ->
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
                        | value ->
                            match tryViewSemanticBool value with
                            | Some true ->
                                evaluateExpression scope bodyExpression
                                |> Result.bind (fun _ -> loop ())
                            | Some false ->
                                ok UnitValue
                            | None ->
                                error $"Expected a Boolean in the while condition, but got {RuntimeValue.format value}.")

                loop ()
            | KRuntimeApply (callee, arguments) ->
                evaluateExpression scope callee
                |> Result.bind (fun functionValue ->
                    evaluateArguments scope arguments
                    |> Result.bind (apply functionValue))
            | KRuntimeDictionaryValue (moduleName, traitName, instanceKey, captures) ->
                evaluateArguments scope captures
                |> Result.map (fun captureValues ->
                    DictionaryValue
                        { ModuleName = moduleName
                          TraitName = traitName
                          InstanceKey = instanceKey
                          Captures = captureValues })
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
                                |> Result.bind (fun memberFunction ->
                                    apply memberFunction (dictionaryValue :: dictionary.Captures @ List.rev values))
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
                    | value ->
                        match tryViewSemanticBool value with
                        | Some false -> ok (BooleanValue false)
                        | Some true ->
                            evaluateExpression scope right
                            |> Result.bind (function
                                | value ->
                                    match tryViewSemanticBool value with
                                    | Some value -> ok (BooleanValue value)
                                    | None ->
                                        error $"Operator '&&' expects Boolean operands, but got {RuntimeValue.format value}.")
                        | None ->
                            error $"Operator '&&' expects Boolean operands, but got {RuntimeValue.format value}.")
            | KRuntimeBinary (left, "||", right) when not (hasExplicitUnqualifiedName scope "||") ->
                evaluateExpression scope left
                |> Result.bind (function
                    | value ->
                        match tryViewSemanticBool value with
                        | Some true -> ok (BooleanValue true)
                        | Some false ->
                            evaluateExpression scope right
                            |> Result.bind (function
                                | value ->
                                    match tryViewSemanticBool value with
                                    | Some value -> ok (BooleanValue value)
                                    | None ->
                                        error $"Operator '||' expects Boolean operands, but got {RuntimeValue.format value}.")
                        | None ->
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

        and evaluateArguments scope arguments =
            arguments
            |> List.map (evaluateExpression scope)
            |> List.fold
                (fun state next ->
                    match state, next with
                    | Result.Ok values, Result.Ok value -> Result.Ok(value :: values)
                    | Result.Error issue, _ -> Result.Error issue
                    | _, Result.Error issue -> Result.Error issue)
                (Result.Ok [])
            |> Result.map List.rev

        and bindHandlerClauseArguments
            (scope: RuntimeScope)
            (clause: KRuntimeEffectHandlerClause)
            (argumentValues: RuntimeValue list)
            (resumptionValue: RuntimeValue option)
            =
            if List.length clause.Arguments <> List.length argumentValues then
                error $"Handler clause '{clause.OperationName}' expected {clause.Arguments.Length} arguments, but received {argumentValues.Length}."
            else
                let boundLocals =
                    List.zip clause.Arguments argumentValues
                    |> List.fold
                        (fun state (argument, value) ->
                            state
                            |> Result.bind (fun locals ->
                                match argument with
                                | KRuntimeEffectNameArgument name ->
                                    Result.Ok(Map.add name value locals)
                                | KRuntimeEffectWildcardArgument ->
                                    Result.Ok locals
                                | KRuntimeEffectUnitArgument ->
                                    match value with
                                    | UnitValue -> Result.Ok locals
                                    | _ ->
                                        error
                                            $"Handler clause '{clause.OperationName}' expected a Unit argument, but received {RuntimeValue.format value}." ))
                        (Result.Ok scope.Locals)

                boundLocals
                |> Result.map (fun locals ->
                    let localsWithResumption =
                        match clause.ResumptionName, resumptionValue with
                        | Some name, Some value -> Map.add name value locals
                        | _ -> locals

                    { scope with Locals = localsWithResumption })

        and makeResumptionValue
            (scope: RuntimeScope)
            isDeep
            (label: RuntimeEffectLabel)
            operationName
            (resumptionQuantity: Quantity option)
            (returnClause: KRuntimeEffectHandlerClause)
            (operationClauses: KRuntimeEffectHandlerClause list)
            continueWith
            =
            let consumed = ref false
            let isOneShot = not (resumptionQuantityPermitsMultiple resumptionQuantity)

            NativeFunctionValue
                { Name = $"{label.Name}.resume"
                  Arity = 1
                  AppliedArguments = []
                  Invoke =
                    (fun arguments ->
                        match arguments with
                        | [ value ] ->
                            if isOneShot && consumed.Value then
                                error $"Consumed one-shot resumption '{label.Name}.{operationName}' cannot be resumed again."
                            else
                                if isOneShot then
                                    consumed.Value <- true

                                ok
                                    (IOActionValue(fun () ->
                                        continueWith value
                                        |> Result.bind (fun step ->
                                            if isDeep then
                                                handleActionResult scope isDeep label returnClause operationClauses step
                                            else
                                                ok step)))
                        | _ ->
                            error "Resumptions expect exactly one argument.") }

        and executeHandlerClause
            (scope: RuntimeScope)
            isDeep
            (label: RuntimeEffectLabel)
            (returnClause: KRuntimeEffectHandlerClause)
            (operationClauses: KRuntimeEffectHandlerClause list)
            (clause: KRuntimeEffectHandlerClause)
            argumentValues
            (resumptionValue: RuntimeValue option)
            =
            bindHandlerClauseArguments scope clause argumentValues resumptionValue
            |> Result.bind (fun clauseScope ->
                evaluateExpression clauseScope clause.Body
                |> Result.bind executeIoAction)

        and handleActionResult
            (scope: RuntimeScope)
            isDeep
            (label: RuntimeEffectLabel)
            (returnClause: KRuntimeEffectHandlerClause)
            (operationClauses: KRuntimeEffectHandlerClause list)
            step
            =
            match step with
            | RuntimeActionReturn value ->
                executeHandlerClause scope isDeep label returnClause operationClauses returnClause [ value ] None
            | RuntimeActionRequest request when String.Equals(request.Label.LabelId, label.LabelId, StringComparison.Ordinal) ->
                match operationClauses |> List.tryFind (fun clause -> String.Equals(clause.OperationName, request.Operation.Name, StringComparison.Ordinal)) with
                | Some clause ->
                    let resumptionQuantity = request.Operation.ResumptionQuantity |> Option.orElse (Some QuantityOne)

                    let resumptionValue =
                        makeResumptionValue
                            scope
                            isDeep
                            label
                            request.Operation.Name
                            resumptionQuantity
                            returnClause
                            operationClauses
                            request.ContinueWith

                    executeHandlerClause scope isDeep label returnClause operationClauses clause request.Arguments (Some resumptionValue)
                | None ->
                    error $"Handler for '{label.Name}' does not define a clause for operation '{request.Operation.Name}'."
            | RuntimeActionRequest request ->
                ok
                    (RuntimeActionRequest
                        { request with
                            ContinueWith =
                                fun value ->
                                    request.ContinueWith value
                                    |> Result.bind (handleActionResult scope isDeep label returnClause operationClauses) })

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
                            | KRuntimeStringInterpolation(expression, _) ->
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
                                | value ->
                                    match tryViewSemanticBool value with
                                    | Some true -> evaluateExpression nextScope caseClause.Body
                                    | Some false -> tryCases rest
                                    | None ->
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
            | EffectOperationValue operation ->
                applyEffectOperationValue operation arguments
            | ConstructorFunctionValue(constructor, existingArguments) ->
                applyConstructor constructor existingArguments arguments
            | NativeFunctionValue nativeFunction ->
                applyNativeFunction nativeFunction arguments
            | BuiltinFunctionValue builtin ->
                applyBuiltinFunction builtin arguments
            | value ->
                error $"Cannot apply {RuntimeValue.format value} as a function."

        and applyEffectOperationValue
            (operationValue: RuntimeEffectOperationValue)
            (arguments: RuntimeValue list)
            : Result<RuntimeValue, EvaluationError> =
            let invokeOperation collectedArguments =
                ok
                    (IOActionValue(fun () ->
                        ok
                            (RuntimeActionRequest
                                { Label = operationValue.Label
                                  Operation = operationValue.Operation
                                  Arguments = collectedArguments
                                  ContinueWith = runtimeActionReturn })))

            let rec invoke collected remainingArguments =
                match remainingArguments with
                | [] ->
                    if List.length collected = operationValue.Operation.ParameterArity then
                        invokeOperation collected
                    else
                        ok
                            (EffectOperationValue
                                { operationValue with
                                    AppliedArguments = collected })
                | argument :: rest ->
                    let nextArguments = collected @ [ argument ]
                    let parameterArity = operationValue.Operation.ParameterArity

                    if List.length nextArguments > parameterArity then
                        let invokedArguments = nextArguments |> List.take parameterArity

                        invokeOperation invokedArguments
                        |> Result.bind (fun value ->
                            let remaining = (nextArguments |> List.skip parameterArity) @ rest

                            apply value remaining)
                    elif List.length nextArguments = parameterArity then
                        invokeOperation nextArguments
                        |> Result.bind (fun value ->
                            if List.isEmpty rest then
                                ok value
                            else
                                apply value rest)
                    else
                        invoke nextArguments rest

            invoke operationValue.AppliedArguments arguments

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

        and applyNativeFunction (nativeFunction: RuntimeNativeFunction) (arguments: RuntimeValue list) =
            let rec invoke collected remainingArguments =
                match remainingArguments with
                | [] ->
                    if List.length collected = nativeFunction.Arity then
                        nativeFunction.Invoke collected
                    else
                        ok
                            (NativeFunctionValue
                                { nativeFunction with
                                    AppliedArguments = collected })
                | argument :: rest ->
                    let nextArguments = collected @ [ argument ]

                    if List.length nextArguments > nativeFunction.Arity then
                        error $"Native function '{nativeFunction.Name}' received too many arguments."
                    elif List.length nextArguments = nativeFunction.Arity then
                        nativeFunction.Invoke nextArguments
                        |> Result.bind (fun value ->
                            if List.isEmpty rest then
                                ok value
                            else
                                apply value rest)
                    else
                        invoke nextArguments rest

            invoke nativeFunction.AppliedArguments arguments

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

        and runtimeActionReturn value =
            ok (RuntimeActionReturn value)

        and executeIoAction value =
            match value with
            | IOActionValue action ->
                action ()
            | other ->
                runtimeActionReturn other

        and executeRuntimeValue value =
            match value with
            | IOActionValue action ->
                action ()
                |> Result.bind (function
                    | RuntimeActionReturn nextValue ->
                        executeRuntimeValue nextValue
                    | RuntimeActionRequest request ->
                        error $"Unhandled effect operation '{request.Label.Name}.{request.Operation.Name}'.")
            | other ->
                ok other

        and bindActionResult continuation step =
            match step with
            | RuntimeActionReturn value ->
                apply continuation [ value ]
                |> Result.bind executeIoAction
            | RuntimeActionRequest request ->
                ok
                    (RuntimeActionRequest
                        { request with
                            ContinueWith =
                                fun value ->
                                    request.ContinueWith value
                                    |> Result.bind (bindActionResult continuation) })

        and sequenceActionResult rightAction step =
            match step with
            | RuntimeActionReturn _ ->
                executeIoAction rightAction
            | RuntimeActionRequest request ->
                ok
                    (RuntimeActionRequest
                        { request with
                            ContinueWith =
                                fun value ->
                                    request.ContinueWith value
                                    |> Result.bind (sequenceActionResult rightAction) })

        and executeExitAction scope action =
            match action with
            | KRuntimeDeferred expression ->
                evaluateExpression scope expression
                |> Result.bind executeIoAction
                |> Result.bind (function
                    | RuntimeActionReturn _ -> ok ()
                    | RuntimeActionRequest _ ->
                        error "Deferred exit actions cannot suspend with unhandled effects.")
            | KRuntimeRelease(_, releaseExpression, resourceExpression) ->
                evaluateExpression scope releaseExpression
                |> Result.bind (fun releaseValue ->
                    evaluateExpression scope resourceExpression
                    |> Result.bind (fun resourceValue ->
                        apply releaseValue [ resourceValue ]
                        |> Result.bind executeIoAction
                        |> Result.bind (function
                            | RuntimeActionReturn _ -> ok ()
                            | RuntimeActionRequest _ ->
                                error "Release actions cannot suspend with unhandled effects.")))

        and toUnitIoAction (action: unit -> unit) =
            ok
                (Some(
                    IOActionValue(fun () ->
                        action ()
                        runtimeActionReturn UnitValue)
                ))

        and invokeBuiltin (builtin: BuiltinFunction) : Result<RuntimeValue option, EvaluationError> =
            let hashStateWithBytes state bytes =
                UnicodeText.updateHashStateWithBytes (uint64 state) bytes
                |> UnicodeText.finishHashState
                |> HashStateValue

            let hashIntoState state value =
                match value with
                | StringValue text ->
                    ok (hashStateWithBytes state (UnicodeText.encodeUtf8 text))
                | BytesValue bytes ->
                    ok (hashStateWithBytes state bytes)
                | CharacterValue scalar ->
                    ok (hashStateWithBytes state (UnicodeText.encodeUtf8 scalar))
                | GraphemeValue grapheme ->
                    ok (hashStateWithBytes state (UnicodeText.encodeUtf8 grapheme))
                | ByteValue value ->
                    ok (hashStateWithBytes state [| value |])
                | IntegerValue value ->
                    ok (hashStateWithBytes state (UnicodeText.int64ToLittleEndianBytes value))
                | FloatValue value ->
                    ok (hashStateWithBytes state (UnicodeText.doubleToLittleEndianBytes value))
                | BooleanValue value ->
                    ok (hashStateWithBytes state [| if value then 1uy else 0uy |])
                | UnitValue ->
                    ok (hashStateWithBytes state [||])
                | _ ->
                    error $"Intrinsic '{builtin.Name}' is not supported for {RuntimeValue.format value} and HashState({state})."

            match builtin.Name, builtin.Arguments with
            | "not", [ value ] ->
                match tryViewSemanticBool value with
                | Some value ->
                    ok (Some(BooleanValue(not value)))
                | None ->
                    error $"Intrinsic 'not' expects a Bool, but got {RuntimeValue.format value}."
            | "not", arguments when List.length arguments < 1 ->
                ok None
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
            | "and", [ left; right ] ->
                match tryViewSemanticBool left, tryViewSemanticBool right with
                | Some left, Some right ->
                    ok (Some(BooleanValue(left && right)))
                | _ ->
                    error $"Intrinsic 'and' expects Bool arguments, but got {RuntimeValue.format left} and {RuntimeValue.format right}."
            | "and", arguments when List.length arguments < 2 ->
                ok None
            | "and", _ ->
                error "Intrinsic 'and' received too many arguments."
            | "or", [ left; right ] ->
                match tryViewSemanticBool left, tryViewSemanticBool right with
                | Some left, Some right ->
                    ok (Some(BooleanValue(left || right)))
                | _ ->
                    error $"Intrinsic 'or' expects Bool arguments, but got {RuntimeValue.format left} and {RuntimeValue.format right}."
            | "or", arguments when List.length arguments < 2 ->
                ok None
            | "or", _ ->
                error "Intrinsic 'or' received too many arguments."
            | "pure", [ value ] ->
                ok (Some(IOActionValue(fun () -> runtimeActionReturn value)))
            | "pure", arguments when List.length arguments < 1 ->
                ok None
            | "pure", _ ->
                error "Intrinsic 'pure' received too many arguments."
            | ">>=", [ action; continuation ] ->
                ok
                    (Some(
                        IOActionValue(fun () ->
                            executeIoAction action
                            |> Result.bind (bindActionResult continuation))
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
                            |> Result.bind (sequenceActionResult rightAction))
                    ))
            | ">>", arguments when List.length arguments < 2 ->
                ok None
            | ">>", _ ->
                error "Intrinsic '>>' received too many arguments."
            | "runPure", [ action ] ->
                executeIoAction action
                |> Result.bind (function
                    | RuntimeActionReturn value -> ok (Some value)
                    | RuntimeActionRequest request ->
                        error $"runPure encountered an unhandled effect operation '{request.Label.Name}.{request.Operation.Name}'.")
            | "runPure", arguments when List.length arguments < 1 ->
                ok None
            | "runPure", _ ->
                error "Intrinsic 'runPure' received too many arguments."
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
            | "compare", [ left; right ] ->
                match tryCompareValues left right with
                | Some comparison ->
                    orderingValue comparison |> Result.map Some
                | None ->
                    error $"Intrinsic 'compare' is not supported for {RuntimeValue.format left} and {RuntimeValue.format right}."
            | "compare", arguments when List.length arguments < 2 ->
                ok None
            | "compare", _ ->
                error "Intrinsic 'compare' received too many arguments."
            | "primitiveIntToString", [ IntegerValue value ] ->
                ok (Some(StringValue(string value)))
            | "primitiveIntToString", arguments when List.length arguments < 1 ->
                ok None
            | "primitiveIntToString", [ value ] ->
                error $"Intrinsic 'primitiveIntToString' expects an Int, but got {RuntimeValue.format value}."
            | "primitiveIntToString", _ ->
                error "Intrinsic 'primitiveIntToString' received too many arguments."
            | "failNow", [ StringValue message ] ->
                error $"Testing failure: {message}"
            | "failNow", arguments when List.length arguments < 1 ->
                ok None
            | "failNow", [ value ] ->
                error $"Intrinsic 'failNow' expects a String, but got {RuntimeValue.format value}."
            | "failNow", _ ->
                error "Intrinsic 'failNow' received too many arguments."
            | "unsafeConsume", [ _ ] ->
                ok (Some UnitValue)
            | "unsafeConsume", arguments when List.length arguments < 1 ->
                ok None
            | "unsafeConsume", _ ->
                error "Intrinsic 'unsafeConsume' received too many arguments."
            | "openFile", [ StringValue value ] ->
                ok (Some(IOActionValue(fun () -> runtimeActionReturn (StringValue($"<file:{value}>")))))
            | "openFile", arguments when List.length arguments < 1 ->
                ok None
            | "openFile", [ value ] ->
                error $"Intrinsic 'openFile' expects a String, but got {RuntimeValue.format value}."
            | "openFile", _ ->
                error "Intrinsic 'openFile' received too many arguments."
            | ("primitiveReadData" | "readData"), [ _ ] ->
                ok (Some(IOActionValue(fun () -> runtimeActionReturn (StringValue "chunk"))))
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
                ok (Some(IOActionValue(fun () -> runtimeActionReturn (RefCellValue { Value = value }))))
            | "newRef", arguments when List.length arguments < 1 ->
                ok None
            | "newRef", _ ->
                error "Intrinsic 'newRef' received too many arguments."
            | "readRef", [ RefCellValue cell ] ->
                ok (Some(IOActionValue(fun () -> runtimeActionReturn cell.Value)))
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
                            runtimeActionReturn UnitValue)
                    ))
            | "writeRef", arguments when List.length arguments < 2 ->
                ok None
            | "writeRef", [ referenceValue; _ ] ->
                error $"Intrinsic 'writeRef' expects a Ref as the first argument, but got {RuntimeValue.format referenceValue}."
            | "writeRef", _ ->
                error "Intrinsic 'writeRef' received too many arguments."
            | "utf8Bytes", [ StringValue value ] ->
                ok (Some(BytesValue(UnicodeText.encodeUtf8 value)))
            | "utf8Bytes", arguments when List.length arguments < 1 ->
                ok None
            | "utf8Bytes", [ value ] ->
                error $"Intrinsic 'utf8Bytes' expects a String, but got {RuntimeValue.format value}."
            | "utf8Bytes", _ ->
                error "Intrinsic 'utf8Bytes' received too many arguments."
            | "decodeUtf8", [ BytesValue bytes ] ->
                UnicodeText.decodeUtf8Strict bytes
                |> function
                    | Result.Ok text ->
                        resultOk (StringValue text) |> Result.map Some
                    | Result.Error message ->
                        resultError (unicodeDecodeError message) |> Result.map Some
            | "decodeUtf8", arguments when List.length arguments < 1 ->
                ok None
            | "decodeUtf8", [ value ] ->
                error $"Intrinsic 'decodeUtf8' expects Bytes, but got {RuntimeValue.format value}."
            | "decodeUtf8", _ ->
                error "Intrinsic 'decodeUtf8' received too many arguments."
            | "decodeUtf8Lossy", [ BytesValue bytes ] ->
                ok (Some(StringValue(System.Text.Encoding.UTF8.GetString(bytes))))
            | "decodeUtf8Lossy", arguments when List.length arguments < 1 ->
                ok None
            | "decodeUtf8Lossy", [ value ] ->
                error $"Intrinsic 'decodeUtf8Lossy' expects Bytes, but got {RuntimeValue.format value}."
            | "decodeUtf8Lossy", _ ->
                error "Intrinsic 'decodeUtf8Lossy' received too many arguments."
            | "byteLength", [ StringValue value ] ->
                ok (Some(IntegerValue(UnicodeText.byteLength value)))
            | "byteLength", arguments when List.length arguments < 1 ->
                ok None
            | "byteLength", [ value ] ->
                error $"Intrinsic 'byteLength' expects a String, but got {RuntimeValue.format value}."
            | "byteLength", _ ->
                error "Intrinsic 'byteLength' received too many arguments."
            | "scalarCount", [ StringValue value ] ->
                ok (Some(IntegerValue(int64 (UnicodeText.scalarCount value))))
            | "scalarCount", arguments when List.length arguments < 1 ->
                ok None
            | "scalarCount", [ value ] ->
                error $"Intrinsic 'scalarCount' expects a String, but got {RuntimeValue.format value}."
            | "scalarCount", _ ->
                error "Intrinsic 'scalarCount' received too many arguments."
            | "graphemeCount", [ StringValue value ] ->
                ok (Some(IntegerValue(int64 (UnicodeText.graphemeCount value))))
            | "graphemeCount", arguments when List.length arguments < 1 ->
                ok None
            | "graphemeCount", [ value ] ->
                error $"Intrinsic 'graphemeCount' expects a String, but got {RuntimeValue.format value}."
            | "graphemeCount", _ ->
                error "Intrinsic 'graphemeCount' received too many arguments."
            | "scalarValue", [ CharacterValue value ] ->
                match UnicodeText.trySingleScalar value with
                | Some rune -> ok (Some(IntegerValue(UnicodeText.scalarValue rune)))
                | None -> error $"Intrinsic 'scalarValue' expected a valid Unicode scalar, but got {RuntimeValue.format (CharacterValue value)}."
            | "scalarValue", arguments when List.length arguments < 1 ->
                ok None
            | "scalarValue", [ value ] ->
                error $"Intrinsic 'scalarValue' expects a UnicodeScalar, but got {RuntimeValue.format value}."
            | "scalarValue", _ ->
                error "Intrinsic 'scalarValue' received too many arguments."
            | "unicodeScalarFromValue", [ IntegerValue value ] ->
                UnicodeText.tryScalarFromValue value
                |> Option.map (fun rune -> CharacterValue(UnicodeText.scalarToString rune))
                |> optionValue
                |> Result.map Some
            | "unicodeScalarFromValue", arguments when List.length arguments < 1 ->
                ok None
            | "unicodeScalarFromValue", [ value ] ->
                error $"Intrinsic 'unicodeScalarFromValue' expects an Int, but got {RuntimeValue.format value}."
            | "unicodeScalarFromValue", _ ->
                error "Intrinsic 'unicodeScalarFromValue' received too many arguments."
            | "scalarToString", [ CharacterValue value ] ->
                ok (Some(StringValue value))
            | "scalarToString", arguments when List.length arguments < 1 ->
                ok None
            | "scalarToString", [ value ] ->
                error $"Intrinsic 'scalarToString' expects a UnicodeScalar, but got {RuntimeValue.format value}."
            | "scalarToString", _ ->
                error "Intrinsic 'scalarToString' received too many arguments."
            | "graphemeToString", [ GraphemeValue value ] ->
                ok (Some(StringValue value))
            | "graphemeToString", arguments when List.length arguments < 1 ->
                ok None
            | "graphemeToString", [ value ] ->
                error $"Intrinsic 'graphemeToString' expects a Grapheme, but got {RuntimeValue.format value}."
            | "graphemeToString", _ ->
                error "Intrinsic 'graphemeToString' received too many arguments."
            | "graphemeFromString", [ StringValue value ] ->
                UnicodeText.trySingleGrapheme value
                |> Option.map GraphemeValue
                |> optionValue
                |> Result.map Some
            | "graphemeFromString", arguments when List.length arguments < 1 ->
                ok None
            | "graphemeFromString", [ value ] ->
                error $"Intrinsic 'graphemeFromString' expects a String, but got {RuntimeValue.format value}."
            | "graphemeFromString", _ ->
                error "Intrinsic 'graphemeFromString' received too many arguments."
            | "normalize", [ NormalizationFormValue form; StringValue value ] ->
                ok (Some(StringValue(UnicodeText.normalize form value)))
            | "normalize", arguments when List.length arguments < 2 ->
                ok None
            | "normalize", [ form; value ] ->
                error $"Intrinsic 'normalize' expects (NormalizationForm, String), but got {RuntimeValue.format form} and {RuntimeValue.format value}."
            | "normalize", _ ->
                error "Intrinsic 'normalize' received too many arguments."
            | "isNormalized", [ NormalizationFormValue form; StringValue value ] ->
                ok (Some(BooleanValue(UnicodeText.isNormalized form value)))
            | "isNormalized", arguments when List.length arguments < 2 ->
                ok None
            | "isNormalized", [ form; value ] ->
                error $"Intrinsic 'isNormalized' expects (NormalizationForm, String), but got {RuntimeValue.format form} and {RuntimeValue.format value}."
            | "isNormalized", _ ->
                error "Intrinsic 'isNormalized' received too many arguments."
            | "canonicalEquivalent", [ StringValue left; StringValue right ] ->
                ok (Some(BooleanValue(UnicodeText.canonicalEquivalent left right)))
            | "canonicalEquivalent", arguments when List.length arguments < 2 ->
                ok None
            | "canonicalEquivalent", [ left; right ] ->
                error $"Intrinsic 'canonicalEquivalent' expects String arguments, but got {RuntimeValue.format left} and {RuntimeValue.format right}."
            | "canonicalEquivalent", _ ->
                error "Intrinsic 'canonicalEquivalent' received too many arguments."
            | "show", [ value ] ->
                ok (Some(StringValue(RuntimeValue.format value)))
            | "show", arguments when List.length arguments < 1 ->
                ok None
            | "show", _ ->
                error "Intrinsic 'show' received too many arguments."
            | "newHashState", [ HashSeedValue seed ] ->
                ok (Some(HashStateValue(UnicodeText.initHashState seed |> UnicodeText.finishHashState)))
            | "newHashState", arguments when List.length arguments < 1 ->
                ok None
            | "newHashState", [ value ] ->
                error $"Intrinsic 'newHashState' expects a HashSeed, but got {RuntimeValue.format value}."
            | "newHashState", _ ->
                error "Intrinsic 'newHashState' received too many arguments."
            | "finishHashState", [ HashStateValue state ] ->
                ok (Some(HashCodeValue(UnicodeText.finishHashState (uint64 state))))
            | "finishHashState", arguments when List.length arguments < 1 ->
                ok None
            | "finishHashState", [ value ] ->
                error $"Intrinsic 'finishHashState' expects a HashState, but got {RuntimeValue.format value}."
            | "finishHashState", _ ->
                error "Intrinsic 'finishHashState' received too many arguments."
            | "hashUnit", [ HashStateValue state ] ->
                ok (Some(hashStateWithBytes state [||]))
            | "hashUnit", arguments when List.length arguments < 1 ->
                ok None
            | "hashUnit", [ value ] ->
                error $"Intrinsic 'hashUnit' expects a HashState, but got {RuntimeValue.format value}."
            | "hashUnit", _ ->
                error "Intrinsic 'hashUnit' received too many arguments."
            | ("hashBool" | "hashUnicodeScalar" | "hashGrapheme" | "hashString" | "hashBytes" | "hashInt" | "hashInteger" | "hashFloatRaw" | "hashDoubleRaw" | "hashNatTag"), [ value; HashStateValue state ] ->
                hashIntoState state value |> Result.map Some
            | ("hashBool" | "hashUnicodeScalar" | "hashGrapheme" | "hashString" | "hashBytes" | "hashInt" | "hashInteger" | "hashFloatRaw" | "hashDoubleRaw" | "hashNatTag"), arguments when List.length arguments < 2 ->
                ok None
            | ("hashBool" | "hashUnicodeScalar" | "hashGrapheme" | "hashString" | "hashBytes" | "hashInt" | "hashInteger" | "hashFloatRaw" | "hashDoubleRaw" | "hashNatTag"), [ value; state ] ->
                error $"Intrinsic '{builtin.Name}' is not supported for {RuntimeValue.format value} and {RuntimeValue.format state}."
            | ("hashBool" | "hashUnicodeScalar" | "hashGrapheme" | "hashString" | "hashBytes" | "hashInt" | "hashInteger" | "hashFloatRaw" | "hashDoubleRaw" | "hashNatTag"), _ ->
                error $"Intrinsic '{builtin.Name}' received too many arguments."
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
                    match resolveTypeScopedConstructorCandidates scope qualifierSegments bindingName with
                    | [ _, constructor ] ->
                        ok (constructorValue constructor)
                    | [] ->
                        resolveQualifiedName scope qualifierSegments bindingName
                    | _ ->
                        let qualifiedName = String.concat "." segments
                        error $"Constructor-like reference '{qualifiedName}' is ambiguous across visible types."

        and selectionImportsTypeName (importedModule: RuntimeModule) (selection: ImportSelection) (typeName: string) =
            let exportsType =
                importedModule.Exports.Contains(typeName)
                || (importedModule.Constructors
                    |> Map.exists (fun constructorName constructor ->
                        String.Equals(constructor.TypeName, typeName, StringComparison.Ordinal)
                        && importedModule.Exports.Contains(constructorName)))

            match selection with
            | QualifiedOnly ->
                false
            | Items items ->
                exportsType
                && (items
                    |> List.exists (fun item ->
                        String.Equals(item.Name, typeName, StringComparison.Ordinal)
                        && item.Namespace = Some ImportNamespace.Type))
            | All ->
                exportsType
            | AllExcept excludedItems ->
                exportsType
                && not (
                    excludedItems
                    |> List.exists (fun item ->
                        String.Equals(item.Name, typeName, StringComparison.Ordinal)
                        && (item.Namespace.IsNone || item.Namespace = Some ImportNamespace.Type))
                )

        and resolveTypeScopedConstructorCandidates
            (scope: RuntimeScope)
            (typeQualifierSegments: string list)
            (constructorName: string)
            =
            let currentModule = scope.Context.Modules[scope.CurrentModule]
            let typeName = SyntaxFacts.moduleNameToText typeQualifierSegments

            let currentModuleCandidate =
                currentModule.Constructors
                |> Map.tryFind constructorName
                |> Option.filter (fun constructor -> String.Equals(constructor.TypeName, typeName, StringComparison.Ordinal))
                |> Option.map (fun constructor -> currentModule.Name, constructor)

            let importedCandidates =
                currentModule.Imports
                |> List.choose (fun spec ->
                    match spec.Source with
                    | Url _ ->
                        None
                    | Dotted moduleSegments ->
                        let importedModuleName = SyntaxFacts.moduleNameToText moduleSegments

                        match Map.tryFind importedModuleName scope.Context.Modules with
                        | Some importedModule when selectionImportsTypeName importedModule spec.Selection typeName ->
                            importedModule.Constructors
                            |> Map.tryFind constructorName
                            |> Option.filter (fun constructor ->
                                String.Equals(constructor.TypeName, typeName, StringComparison.Ordinal)
                                && importedModule.Exports.Contains(constructorName))
                            |> Option.map (fun constructor -> importedModuleName, constructor)
                        | _ ->
                            None)

            currentModuleCandidate
            |> Option.toList
            |> List.append importedCandidates
            |> List.distinctBy fst

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

                let moduleQualifiedTarget =
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

                match moduleQualifiedTarget with
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
                    match resolveTypeScopedConstructorCandidates scope qualifierSegments constructorName with
                    | [ _, constructor ] ->
                        ok constructor
                    | [] ->
                        error $"Module qualifier '{qualifierText}' is not in scope."
                    | _ ->
                        let patternName = String.concat "." segments
                        error $"Pattern constructor '{patternName}' is ambiguous across visible types."

        and itemImportsTermName (item: ImportItem) =
            item.Namespace.IsNone || item.Namespace = Some ImportNamespace.Term

        and itemImportsConstructorName (item: ImportItem) =
            item.Namespace = Some ImportNamespace.Constructor

        and itemImportsConstructorsOfType typeName (item: ImportItem) =
            item.IncludeConstructors
            && item.Namespace = Some ImportNamespace.Type
            && String.Equals(item.Name, typeName, StringComparison.Ordinal)

        and exceptMatches namespaceName name (item: ExceptItem) =
            String.Equals(item.Name, name, StringComparison.Ordinal)
            && (item.Namespace.IsNone || item.Namespace = Some namespaceName)

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
            | AllExcept excludedItems ->
                not (excludedItems |> List.exists (exceptMatches ImportNamespace.Term name))
                && exportsTerm
                && importedModule.Exports.Contains(name)

        and selectionImportsConstructorName (importedModule: RuntimeModule) (selection: ImportSelection) (name: string) =
            let exportsConstructor =
                importedModule.Constructors.ContainsKey(name)
                && importedModule.Exports.Contains(name)

            let constructorTypeName =
                importedModule.Constructors
                |> Map.tryFind name
                |> Option.map (fun constructorInfo -> constructorInfo.TypeName)

            match selection with
            | QualifiedOnly ->
                false
            | Items items ->
                exportsConstructor
                && (items
                    |> List.exists (fun item ->
                        ((String.Equals(item.Name, name, StringComparison.Ordinal)
                          && itemImportsConstructorName item)
                        || (constructorTypeName
                            |> Option.exists (fun typeName -> itemImportsConstructorsOfType typeName item)))))
            | All
            | AllExcept _ ->
                false

        and importSelectionPriority selection =
            match selection with
            | Items _ -> 0
            | All
            | AllExcept _ -> 1
            | QualifiedOnly -> 2

        and findImportedModulesForName (scope: RuntimeScope) (name: string) =
            let currentModule = scope.Context.Modules[scope.CurrentModule]

            let matches =
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
                                Some(importSelectionPriority spec.Selection, importedModuleName, importedModule)
                            else
                                None)

            match matches with
            | [] ->
                []
            | _ ->
                let bestPriority =
                    matches
                    |> List.map (fun (priority, _, _) -> priority)
                    |> List.min

                matches
                |> List.filter (fun (priority, _, _) -> priority = bestPriority)
                |> List.distinctBy (fun (_, importedModuleName, _) -> importedModuleName)
                |> List.map (fun (_, _, importedModule) -> importedModule)

        and findNonPreludeImportedModulesForName (scope: RuntimeScope) (name: string) =
            findImportedModulesForName scope name
            |> List.filter (fun importedModule ->
                not (String.Equals(importedModule.Name, Stdlib.PreludeModuleText, StringComparison.Ordinal)))

        and tryFindImplicitPreludeModuleForName (scope: RuntimeScope) (name: string) =
            scope.Context.Modules
            |> Map.tryFind Stdlib.PreludeModuleText
            |> Option.filter (fun preludeModule ->
                preludeModule.Definitions.ContainsKey(name)
                || preludeModule.IntrinsicTerms.Contains(name)
                || preludeModule.Constructors.ContainsKey(name))

        and hasExplicitUnqualifiedName (scope: RuntimeScope) (name: string) =
            let currentModule = scope.Context.Modules[scope.CurrentModule]

            Map.containsKey name scope.Locals
            || currentModule.Definitions.ContainsKey(name)
            || currentModule.IntrinsicTerms.Contains(name)
            || currentModule.Constructors.ContainsKey(name)
            || not (List.isEmpty (findNonPreludeImportedModulesForName scope name))

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
                        match tryFindImplicitPreludeModuleForName scope name with
                        | Some preludeModule ->
                            forceBinding preludeModule name
                        | None ->
                            match tryCreateBuiltinFunction name with
                            | Some builtin ->
                                ok builtin
                            | None ->
                                match tryCreateIntrinsicTermValue Stdlib.PreludeModuleText name with
                                | Some intrinsicValue
                                    when IntrinsicCatalog.hiddenRuntimePreludeIntrinsicTermNames.Contains name ->
                                    ok intrinsicValue
                                | _ ->
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

        { Context = context
          ApplyRuntimeValue = apply
          ExecuteRuntimeValue = executeRuntimeValue }

    let private buildContextWithOutputFromModules (output: RuntimeOutput) (runtimeModules: KRuntimeModule list) =
        (buildSessionWithOutputFromModules output runtimeModules).Context

    let private buildContextWithOutput (output: RuntimeOutput) (workspace: WorkspaceCompilation) =
        buildContextWithOutputFromModules output workspace.KRuntimeIR

    let private buildSessionWithOutput (output: RuntimeOutput) (workspace: WorkspaceCompilation) =
        buildSessionWithOutputFromModules output workspace.KRuntimeIR

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

    let createRuntimeSessionWithOutput (workspace: WorkspaceCompilation) (output: RuntimeOutput) =
        if workspace.HasErrors then
            error "Cannot evaluate a workspace that already contains diagnostics."
        else
            ok (buildSessionWithOutput output workspace)

    let createRuntimeSession (workspace: WorkspaceCompilation) =
        createRuntimeSessionWithOutput workspace RuntimeOutput.console

    let evaluateBindingInSession (session: RuntimeSession) (entryPoint: string) =
        evaluateBindingWithContext session.Context entryPoint

    let applyRuntimeValueInSession (session: RuntimeSession) (functionValue: RuntimeValue) (arguments: RuntimeValue list) =
        session.ApplyRuntimeValue functionValue arguments

    let executeRuntimeValueInSession (session: RuntimeSession) (value: RuntimeValue) =
        session.ExecuteRuntimeValue value

    let evaluateBinding (workspace: WorkspaceCompilation) (entryPoint: string) =
        evaluateBindingWithOutput workspace RuntimeOutput.console entryPoint

    let executeBindingWithOutput (workspace: WorkspaceCompilation) (output: RuntimeOutput) (entryPoint: string) =
        let rec execute value =
            match value with
            | IOActionValue action ->
                action ()
                |> Result.bind (function
                    | RuntimeActionReturn nextValue ->
                        execute nextValue
                    | RuntimeActionRequest request ->
                        error $"Unhandled effect operation '{request.Label.Name}.{request.Operation.Name}'.")
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
