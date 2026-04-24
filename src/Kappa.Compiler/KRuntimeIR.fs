namespace Kappa.Compiler

// The implementation-defined runtime IR that sits between KCore and KBackendIR.
type KRuntimeExitAction =
    | KRuntimeDeferred of KRuntimeExpression
    | KRuntimeRelease of resourceTypeText: string option * release: KRuntimeExpression * resource: KRuntimeExpression

and KRuntimeExpression =
    | KRuntimeLiteral of LiteralValue
    | KRuntimeName of string list
    | KRuntimeClosure of string list * KRuntimeExpression
    | KRuntimeIfThenElse of KRuntimeExpression * KRuntimeExpression * KRuntimeExpression
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

and KRuntimeStringPart =
    | KRuntimeStringText of string
    | KRuntimeStringInterpolation of KRuntimeExpression

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
      Constructors: KRuntimeConstructor list }

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
