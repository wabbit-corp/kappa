namespace Kappa.Compiler

// The public KCore data model used for post-elaboration observability and lowering.
type KCoreType = TypeSignatures.TypeExpr

type KCoreArgumentKind =
    | KCoreExplicitArgument
    | KCoreImplicitArgument
    | KCoreInoutArgument

type KCoreParameter =
    { Name: string
      Quantity: Quantity option
      IsImplicit: bool
      Type: KCoreType option
      TypeText: string option }

type KCoreStaticObjectKind =
    | KCoreTypeObject
    | KCoreTraitObject
    | KCoreEffectLabelObject
    | KCoreModuleObject

type KCoreStaticObject =
    { ObjectKind: KCoreStaticObjectKind
      Name: string list
      Type: KCoreType option
      TypeText: string option }

type KCoreEffectOperation =
    { OperationId: string
      Name: string
      ResumptionQuantity: Quantity option
      ParameterArity: int }

type KCoreEffectHandlerArgument =
    | KCoreEffectUnitArgument
    | KCoreEffectWildcardArgument
    | KCoreEffectNameArgument of string

type KCoreExpression =
    | KCoreLiteral of LiteralValue
    | KCoreName of string list
    | KCoreStaticObject of KCoreStaticObject
    | KCoreEffectLabel of labelName: string * interfaceId: string * labelId: string * operations: KCoreEffectOperation list
    | KCoreEffectOperation of label: KCoreExpression * operationId: string * operationName: string
    | KCoreSyntaxQuote of KCoreExpression
    | KCoreSyntaxSplice of KCoreExpression
    | KCoreTopLevelSyntaxSplice of KCoreExpression
    | KCoreCodeQuote of KCoreExpression
    | KCoreCodeSplice of KCoreExpression
    | KCoreLambda of KCoreParameter list * KCoreExpression
    | KCoreIfThenElse of KCoreExpression * KCoreExpression * KCoreExpression
    | KCoreHandle of
        isDeep: bool *
        label: KCoreExpression *
        body: KCoreExpression *
        returnClause: KCoreEffectHandlerClause *
        operationClauses: KCoreEffectHandlerClause list
    | KCoreMatch of KCoreExpression * KCoreMatchCase list
    | KCoreExecute of KCoreExpression
    | KCoreLet of bindingName: string * value: KCoreExpression * body: KCoreExpression
    | KCoreDoScope of scopeLabel: string * body: KCoreExpression
    | KCoreScheduleExit of scopeLabel: string * action: KCoreExitAction * body: KCoreExpression
    | KCoreSequence of KCoreExpression * KCoreExpression
    | KCoreWhile of condition: KCoreExpression * body: KCoreExpression
    | KCoreAppSpine of KCoreExpression * KCoreArgument list
    | KCoreDictionaryValue of moduleName: string * traitName: string * instanceKey: string
    | KCoreTraitCall of traitName: string * memberName: string * dictionary: KCoreExpression * arguments: KCoreExpression list
    | KCoreUnary of operatorName: string * KCoreExpression
    | KCoreBinary of KCoreExpression * operatorName: string * KCoreExpression
    | KCorePrefixedString of prefix: string * parts: KCoreStringPart list

and KCoreArgument =
    { ArgumentKind: KCoreArgumentKind
      Expression: KCoreExpression }

and KCoreExitAction =
    | KCoreDeferred of KCoreExpression
    | KCoreRelease of resourceTypeText: string option * release: KCoreExpression * resource: KCoreExpression

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

and KCoreEffectHandlerClause =
    { OperationName: string
      Arguments: KCoreEffectHandlerArgument list
      ResumptionName: string option
      Body: KCoreExpression }

type KCoreBinding =
    { Visibility: Visibility option
      IsOpaque: bool
      Name: string option
      Parameters: KCoreParameter list
      ReturnType: KCoreType option
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
      VisibleTraitTypeParameterCounts: Map<string, int>
      IntrinsicTerms: string list
      SyntheticDataTypes: KCoreSyntheticDataType list
      Ownership: OwnershipFactSet option
      Declarations: KCoreDeclaration list }
