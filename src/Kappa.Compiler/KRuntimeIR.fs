namespace Kappa.Compiler

type ExternalRuntimeBinding =
    | DotNetHostConstructor of
        declaringTypeAssemblyQualifiedName: string *
        parameterTypeAssemblyQualifiedNames: string list *
        requiredAssemblyPaths: string list
    | DotNetHostMethod of
        declaringTypeAssemblyQualifiedName: string *
        methodName: string *
        parameterTypeAssemblyQualifiedNames: string list *
        requiredAssemblyPaths: string list
    | DotNetHostPropertyGetter of
        declaringTypeAssemblyQualifiedName: string *
        getterName: string *
        requiredAssemblyPaths: string list

[<RequireQualifiedAccess>]
module ExternalRuntimeBinding =
    let requiredAssemblyPaths binding =
        match binding with
        | DotNetHostConstructor(_, _, paths)
        | DotNetHostMethod(_, _, _, paths)
        | DotNetHostPropertyGetter(_, _, paths) ->
            paths

// The implementation-defined runtime IR that sits between KCore and KBackendIR.
type KRuntimeEffectOperation =
    { OperationId: string
      Name: string
      ResumptionQuantity: Quantity option
      ParameterArity: int }

type KRuntimeEffectHandlerArgument =
    | KRuntimeEffectUnitArgument
    | KRuntimeEffectWildcardArgument
    | KRuntimeEffectNameArgument of string

type KRuntimeExpression =
    | KRuntimeLiteral of LiteralValue
    | KRuntimeName of string list
    | KRuntimeEffectLabel of labelName: string * interfaceId: string * labelId: string * operations: KRuntimeEffectOperation list
    | KRuntimeEffectOperation of label: KRuntimeExpression * operationId: string * operationName: string
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
    | KRuntimeDictionaryValue of
        moduleName: string * traitName: string * instanceKey: string * captures: KRuntimeExpression list
    | KRuntimeTraitCall of traitName: string * memberName: string * dictionary: KRuntimeExpression * arguments: KRuntimeExpression list
    | KRuntimeUnary of operatorName: string * KRuntimeExpression
    | KRuntimeBinary of KRuntimeExpression * operatorName: string * KRuntimeExpression
    | KRuntimePrefixedString of prefix: string * parts: KRuntimeStringPart list

and KRuntimeExitAction =
    | KRuntimeDeferred of KRuntimeExpression
    | KRuntimeRelease of resourceType: TypeSignatures.TypeExpr option * release: KRuntimeExpression * resource: KRuntimeExpression

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
      Type: TypeSignatures.TypeExpr option }

type KRuntimeConstructor =
    { Name: string
      Arity: int
      TypeName: string
      FieldNames: string option list
      FieldTypes: TypeSignatures.TypeExpr list
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
      HeadTypes: TypeSignatures.TypeExpr list
      MemberBindings: (string * string) list }

type KRuntimeBinding =
    { Name: string
      Parameters: KRuntimeParameter list
      ReturnType: TypeSignatures.TypeExpr option
      ExternalBinding: ExternalRuntimeBinding option
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
module KRuntimeIR =
    let rec containsEffectRuntimeConstruct expression =
        match expression with
        | KRuntimeEffectLabel _
        | KRuntimeEffectOperation _
        | KRuntimeHandle _ ->
            true
        | KRuntimeClosure(_, body)
        | KRuntimeExecute body
        | KRuntimeDoScope(_, body)
        | KRuntimeUnary(_, body) ->
            containsEffectRuntimeConstruct body
        | KRuntimeIfThenElse(condition, whenTrue, whenFalse) ->
            containsEffectRuntimeConstruct condition
            || containsEffectRuntimeConstruct whenTrue
            || containsEffectRuntimeConstruct whenFalse
        | KRuntimeLet(_, value, body)
        | KRuntimeSequence(value, body)
        | KRuntimeWhile(value, body)
        | KRuntimeBinary(value, _, body) ->
            containsEffectRuntimeConstruct value || containsEffectRuntimeConstruct body
        | KRuntimeScheduleExit(_, KRuntimeDeferred deferred, body) ->
            containsEffectRuntimeConstruct deferred || containsEffectRuntimeConstruct body
        | KRuntimeScheduleExit(_, KRuntimeRelease(_, release, resource), body) ->
            containsEffectRuntimeConstruct release
            || containsEffectRuntimeConstruct resource
            || containsEffectRuntimeConstruct body
        | KRuntimeApply(callee, arguments)
        | KRuntimeTraitCall(_, _, callee, arguments) ->
            containsEffectRuntimeConstruct callee
            || (arguments |> List.exists containsEffectRuntimeConstruct)
        | KRuntimeMatch(scrutinee, cases) ->
            containsEffectRuntimeConstruct scrutinee
            || (cases
                |> List.exists (fun caseClause ->
                    caseClause.Guard |> Option.exists containsEffectRuntimeConstruct
                    || containsEffectRuntimeConstruct caseClause.Body))
        | KRuntimePrefixedString(_, parts) ->
            parts
            |> List.exists (function
                | KRuntimeStringText _ -> false
                | KRuntimeStringInterpolation(inner, _) -> containsEffectRuntimeConstruct inner)
        | KRuntimeLiteral _
        | KRuntimeName _
        | KRuntimeDictionaryValue _ ->
            false

    let bindingUsesEffectRuntime (binding: KRuntimeBinding) =
        binding.Body |> Option.exists containsEffectRuntimeConstruct

    let modulesUseEffectRuntime (modules: KRuntimeModule list) =
        modules
        |> List.exists (fun runtimeModule ->
            runtimeModule.Bindings |> List.exists bindingUsesEffectRuntime)
