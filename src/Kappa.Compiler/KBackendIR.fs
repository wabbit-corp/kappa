namespace Kappa.Compiler

type KBackendRepresentationClass =
    | BackendRepInt64
    | BackendRepFloat64
    | BackendRepBoolean
    | BackendRepString
    | BackendRepChar
    | BackendRepUnit
    | BackendRepRef of elementRepresentation: KBackendRepresentationClass
    | BackendRepDictionary of traitName: string
    | BackendRepTaggedData of moduleName: string * typeName: string
    | BackendRepClosure of environmentLayout: string
    | BackendRepIOAction
    | BackendRepOpaque of string option

type KBackendParameter =
    { Name: string
      Representation: KBackendRepresentationClass }

type KBackendCapture =
    { Name: string
      Representation: KBackendRepresentationClass }

type KBackendCallingConvention =
    { RuntimeArity: int
      ParameterRepresentations: KBackendRepresentationClass list
      ResultRepresentation: KBackendRepresentationClass option
      RetainedDictionaryParameters: string list }

type KBackendResolvedName =
    | BackendLocalName of name: string * representation: KBackendRepresentationClass option
    | BackendGlobalBindingName of moduleName: string * bindingName: string * representation: KBackendRepresentationClass option
    | BackendIntrinsicName of moduleName: string * bindingName: string * representation: KBackendRepresentationClass option
    | BackendConstructorName of
        moduleName: string *
        typeName: string *
        constructorName: string *
        tag: int *
        arity: int *
        representation: KBackendRepresentationClass

type KBackendExpression =
    | BackendLiteral of LiteralValue * KBackendRepresentationClass
    | BackendName of KBackendResolvedName
    | BackendClosure of
        parameters: KBackendParameter list *
        captures: KBackendCapture list *
        environmentLayout: string *
        body: KBackendExpression *
        convention: KBackendCallingConvention *
        representation: KBackendRepresentationClass
    | BackendIfThenElse of
        condition: KBackendExpression *
        whenTrue: KBackendExpression *
        whenFalse: KBackendExpression *
        resultRepresentation: KBackendRepresentationClass
    | BackendMatch of
        scrutinee: KBackendExpression *
        cases: KBackendMatchCase list *
        resultRepresentation: KBackendRepresentationClass
    | BackendExecute of
        expression: KBackendExpression *
        resultRepresentation: KBackendRepresentationClass
    | BackendLet of
        binding: KBackendParameter *
        value: KBackendExpression *
        body: KBackendExpression *
        resultRepresentation: KBackendRepresentationClass
    | BackendSequence of
        first: KBackendExpression *
        second: KBackendExpression *
        resultRepresentation: KBackendRepresentationClass
    | BackendWhile of
        condition: KBackendExpression *
        body: KBackendExpression
    | BackendCall of
        callee: KBackendExpression *
        arguments: KBackendExpression list *
        convention: KBackendCallingConvention *
        resultRepresentation: KBackendRepresentationClass
    | BackendDictionaryValue of
        moduleName: string *
        traitName: string *
        instanceKey: string *
        representation: KBackendRepresentationClass
    | BackendTraitCall of
        traitName: string *
        memberName: string *
        dictionary: KBackendExpression *
        arguments: KBackendExpression list *
        resultRepresentation: KBackendRepresentationClass
    | BackendConstructData of
        moduleName: string *
        typeName: string *
        constructorName: string *
        tag: int *
        fields: KBackendExpression list *
        representation: KBackendRepresentationClass
    | BackendPrefixedString of
        prefix: string *
        parts: KBackendStringPart list *
        resultRepresentation: KBackendRepresentationClass

and KBackendStringPart =
    | BackendStringText of string
    | BackendStringInterpolation of KBackendExpression

and KBackendPatternBinding =
    { Name: string
      Representation: KBackendRepresentationClass }

and KBackendPattern =
    | BackendWildcardPattern
    | BackendBindPattern of KBackendPatternBinding
    | BackendLiteralPattern of LiteralValue * KBackendRepresentationClass
    | BackendConstructorPattern of
        moduleName: string *
        typeName: string *
        constructorName: string *
        tag: int *
        fieldPatterns: KBackendPattern list

and KBackendMatchCase =
    { Pattern: KBackendPattern
      Body: KBackendExpression }

type KBackendEnvironmentLayout =
    { Name: string
      Slots: KBackendCapture list }

type KBackendConstructorLayout =
    { Name: string
      Tag: int
      FieldRepresentations: KBackendRepresentationClass list
      Provenance: KCoreOrigin }

type KBackendDataLayout =
    { TypeName: string
      RepresentationClass: string
      TagEncoding: string
      Constructors: KBackendConstructorLayout list
      Provenance: KCoreOrigin }

type KBackendControlForm =
    | StructuredExpression

type KBackendFunction =
    { Name: string
      Parameters: KBackendParameter list
      CallingConvention: KBackendCallingConvention
      ReturnRepresentation: KBackendRepresentationClass option
      EnvironmentLayout: string option
      Intrinsic: bool
      Exported: bool
      EntryPoint: bool
      ControlForm: KBackendControlForm
      Body: KBackendExpression option
      Provenance: KCoreOrigin }

type KBackendModule =
    { Name: string
      SourceFile: string
      Imports: ImportSpec list
      Exports: string list
      EntryPoints: string list
      IntrinsicTerms: string list
      DataLayouts: KBackendDataLayout list
      EnvironmentLayouts: KBackendEnvironmentLayout list
      Functions: KBackendFunction list }
