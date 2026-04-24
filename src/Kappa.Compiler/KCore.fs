namespace Kappa.Compiler

// The public KCore data model used for post-elaboration observability and lowering.
type KCoreOrigin =
    { FilePath: string
      ModuleName: string
      DeclarationName: string option
      IntroductionKind: string }

type KCoreParameter =
    { Name: string
      TypeText: string option }

type KCoreExitAction =
    | KCoreDeferred of KCoreExpression
    | KCoreRelease of resourceTypeText: string option * release: KCoreExpression * resource: KCoreExpression

and KCoreExpression =
    | KCoreLiteral of LiteralValue
    | KCoreName of string list
    | KCoreLambda of KCoreParameter list * KCoreExpression
    | KCoreIfThenElse of KCoreExpression * KCoreExpression * KCoreExpression
    | KCoreMatch of KCoreExpression * KCoreMatchCase list
    | KCoreExecute of KCoreExpression
    | KCoreLet of bindingName: string * value: KCoreExpression * body: KCoreExpression
    | KCoreDoScope of scopeLabel: string * body: KCoreExpression
    | KCoreScheduleExit of scopeLabel: string * action: KCoreExitAction * body: KCoreExpression
    | KCoreSequence of KCoreExpression * KCoreExpression
    | KCoreWhile of condition: KCoreExpression * body: KCoreExpression
    | KCoreApply of KCoreExpression * KCoreExpression list
    | KCoreDictionaryValue of moduleName: string * traitName: string * instanceKey: string
    | KCoreTraitCall of traitName: string * memberName: string * dictionary: KCoreExpression * arguments: KCoreExpression list
    | KCoreUnary of operatorName: string * KCoreExpression
    | KCoreBinary of KCoreExpression * operatorName: string * KCoreExpression
    | KCorePrefixedString of prefix: string * parts: KCoreStringPart list

and KCoreStringPart =
    | KCoreStringText of string
    | KCoreStringInterpolation of KCoreExpression

and KCorePattern =
    | KCoreWildcardPattern
    | KCoreNamePattern of string
    | KCoreLiteralPattern of LiteralValue
    | KCoreConstructorPattern of string list * KCorePattern list
    | KCoreOrPattern of KCorePattern list

and KCoreMatchCase =
    { Pattern: KCorePattern
      Guard: KCoreExpression option
      Body: KCoreExpression }

type KCoreBinding =
    { Visibility: Visibility option
      IsOpaque: bool
      Name: string option
      Parameters: KCoreParameter list
      ReturnTypeText: string option
      Body: KCoreExpression option
      BodyText: string option
      Provenance: KCoreOrigin }

type KCoreDeclaration =
    { Source: TopLevelDeclaration
      Binding: KCoreBinding option
      Provenance: KCoreOrigin }

type KCoreSyntheticDataType =
    { Name: string
      TypeParameters: string list
      ConstructorName: string
      FieldNames: string list
      FieldTypeTexts: string list
      Provenance: KCoreOrigin }

type KCoreModule =
    { Name: string
      SourceFile: string
      ModuleAttributes: string list
      Imports: ImportSpec list
      IntrinsicTerms: string list
      SyntheticDataTypes: KCoreSyntheticDataType list
      Ownership: OwnershipFactSet option
      Declarations: KCoreDeclaration list }
