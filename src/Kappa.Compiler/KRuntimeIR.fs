namespace Kappa.Compiler

// The implementation-defined runtime IR that sits between KCore and KBackendIR.
type KRuntimeEffectOperation =
    { Name: string
      ResumptionQuantity: Quantity option
      ParameterArity: int }

type KRuntimeEffectHandlerArgument =
    | KRuntimeEffectUnitArgument
    | KRuntimeEffectWildcardArgument
    | KRuntimeEffectNameArgument of string

type KRuntimeExpression =
    | KRuntimeLiteral of LiteralValue
    | KRuntimeName of string list
    | KRuntimeEffectLabel of labelName: string * operations: KRuntimeEffectOperation list
    | KRuntimeEffectOperation of label: KRuntimeExpression * operationName: string
    | KRuntimeClosure of string list * KRuntimeExpression
    | KRuntimeIfThenElse of KRuntimeExpression * KRuntimeExpression * KRuntimeExpression
    | KRuntimeHandle of
        isDeep: bool *
        label: KRuntimeExpression *
        body: KRuntimeExpression *
        returnClause: KRuntimeEffectHandlerClause *
        operationClauses: KRuntimeEffectHandlerClause list
    | KRuntimeMatch of KRuntimeExpression * KRuntimeMatchCase list
    | KRuntimeExecute of KRuntimeExpression
    | KRuntimeLet of bindingName: string * value: KRuntimeExpression * body: KRuntimeExpression
    | KRuntimeDoScope of scopeLabel: string * body: KRuntimeExpression
    | KRuntimeScheduleExit of scopeLabel: string * action: KRuntimeExitAction * body: KRuntimeExpression
    | KRuntimeSequence of KRuntimeExpression * KRuntimeExpression
    | KRuntimeWhile of condition: KRuntimeExpression * body: KRuntimeExpression
    | KRuntimeApply of KRuntimeExpression * KRuntimeExpression list
    | KRuntimeDictionaryValue of moduleName: string * traitName: string * instanceKey: string
    | KRuntimeTraitCall of traitName: string * memberName: string * dictionary: KRuntimeExpression * arguments: KRuntimeExpression list
    | KRuntimeUnary of operatorName: string * KRuntimeExpression
    | KRuntimeBinary of KRuntimeExpression * operatorName: string * KRuntimeExpression
    | KRuntimePrefixedString of prefix: string * parts: KRuntimeStringPart list

and KRuntimeExitAction =
    | KRuntimeDeferred of KRuntimeExpression
    | KRuntimeRelease of resourceTypeText: string option * release: KRuntimeExpression * resource: KRuntimeExpression

and KRuntimeStringPart =
    | KRuntimeStringText of string
    | KRuntimeStringInterpolation of expression: KRuntimeExpression * format: string option

and KRuntimePattern =
    | KRuntimeWildcardPattern
    | KRuntimeNamePattern of string
    | KRuntimeLiteralPattern of LiteralValue
    | KRuntimeConstructorPattern of string list * KRuntimePattern list
    | KRuntimeOrPattern of KRuntimePattern list

and KRuntimeMatchCase =
    { Pattern: KRuntimePattern
      Guard: KRuntimeExpression option
      Body: KRuntimeExpression }

and KRuntimeEffectHandlerClause =
    { OperationName: string
      Arguments: KRuntimeEffectHandlerArgument list
      ResumptionName: string option
      Body: KRuntimeExpression }

type KRuntimeParameter =
    { Name: string
      TypeText: string option }

type KRuntimeConstructor =
    { Name: string
      Arity: int
      TypeName: string
      FieldNames: string option list
      FieldTypeTexts: string list
      Provenance: KCoreOrigin }

type KRuntimeDataType =
    { Name: string
      TypeParameters: string list
      Constructors: KRuntimeConstructor list
      ExternalRuntimeTypeName: string option }

type KRuntimeTrait =
    { Name: string
      TypeParameterCount: int }

type KRuntimeTraitInstance =
    { TraitName: string
      InstanceKey: string
      HeadTypeTexts: string list
      MemberBindings: (string * string) list }

type KRuntimeBinding =
    { Name: string
      Parameters: KRuntimeParameter list
      ReturnTypeText: string option
      Body: KRuntimeExpression option
      Intrinsic: bool
      Provenance: KCoreOrigin }

type KRuntimeModule =
    { Name: string
      SourceFile: string
      Imports: ImportSpec list
      Exports: string list
      IntrinsicTerms: string list
      DataTypes: KRuntimeDataType list
      Traits: KRuntimeTrait list
      TraitInstances: KRuntimeTraitInstance list
      Constructors: KRuntimeConstructor list
      Bindings: KRuntimeBinding list }

[<RequireQualifiedAccess>]
module KRuntimeEffectAnalysis =
    let rec expressionContainsEffectRuntime expression =
        match expression with
        | KRuntimeEffectLabel _
        | KRuntimeEffectOperation _
        | KRuntimeHandle _ ->
            true
        | KRuntimeClosure(_, body)
        | KRuntimeExecute body
        | KRuntimeDoScope(_, body)
        | KRuntimeUnary(_, body) ->
            expressionContainsEffectRuntime body
        | KRuntimeIfThenElse(condition, whenTrue, whenFalse) ->
            expressionContainsEffectRuntime condition
            || expressionContainsEffectRuntime whenTrue
            || expressionContainsEffectRuntime whenFalse
        | KRuntimeLet(_, value, body)
        | KRuntimeSequence(value, body) ->
            expressionContainsEffectRuntime value || expressionContainsEffectRuntime body
        | KRuntimeScheduleExit(_, KRuntimeDeferred deferred, body) ->
            expressionContainsEffectRuntime deferred || expressionContainsEffectRuntime body
        | KRuntimeScheduleExit(_, KRuntimeRelease(_, release, resource), body) ->
            expressionContainsEffectRuntime release
            || expressionContainsEffectRuntime resource
            || expressionContainsEffectRuntime body
        | KRuntimeWhile(condition, body)
        | KRuntimeBinary(condition, _, body) ->
            expressionContainsEffectRuntime condition || expressionContainsEffectRuntime body
        | KRuntimeApply(callee, arguments) ->
            expressionContainsEffectRuntime callee
            || (arguments |> List.exists expressionContainsEffectRuntime)
        | KRuntimeTraitCall(_, _, dictionary, arguments) ->
            expressionContainsEffectRuntime dictionary
            || (arguments |> List.exists expressionContainsEffectRuntime)
        | KRuntimeMatch(scrutinee, cases) ->
            expressionContainsEffectRuntime scrutinee
            || (cases
                |> List.exists (fun caseClause ->
                    caseClause.Guard |> Option.exists expressionContainsEffectRuntime
                    || expressionContainsEffectRuntime caseClause.Body))
        | KRuntimePrefixedString(_, parts) ->
            parts
            |> List.exists (function
                | KRuntimeStringText _ -> false
                | KRuntimeStringInterpolation(inner, _) -> expressionContainsEffectRuntime inner)
        | KRuntimeLiteral _
        | KRuntimeName _
        | KRuntimeDictionaryValue _ ->
            false

    let moduleContainsEffectRuntime (runtimeModule: KRuntimeModule) =
        runtimeModule.Bindings
        |> List.exists (fun binding ->
            binding.Body |> Option.exists expressionContainsEffectRuntime)

    let modulesContainEffectRuntime (runtimeModules: KRuntimeModule list) =
        runtimeModules |> List.exists moduleContainsEffectRuntime
