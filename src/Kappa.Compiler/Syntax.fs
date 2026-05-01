namespace Kappa.Compiler

open System
open System.Numerics
open System.Text

type Keyword =
    | As
    | Asc
    | AssertReducible
    | AssertTerminates
    | AssertTotal
    | Break
    | By
    | Case
    | Clarify
    | Continue
    | Ctor
    | Data
    | Desc
    | Derive
    | Distinct
    | Do
    | Elif
    | Else
    | Except
    | Expect
    | Export
    | Finally
    | For
    | ForQuestion
    | Forall
    | Group
    | If
    | Import
    | Impossible
    | In
    | Inout
    | Defer
    | Infix
    | Instance
    | Is
    | Join
    | Lazy
    | Left
    | Let
    | LetQuestion
    | Match
    | Module
    | Opaque
    | Order
    | Place
    | Postfix
    | Prefix
    | Private
    | Projection
    | Public
    | Return
    | Right
    | Seal
    | Skip
    | Take
    | Term
    | Then
    | Thunk
    | Top
    | Trait
    | Try
    | Type
    | Unhide
    | Using
    | Var
    | While
    | Yield

// Maps reserved words between token text and the internal keyword enumeration.
module Keyword =
    let private entries =
        [
            "as", As
            "asc", Asc
            "assertReducible", AssertReducible
            "assertTerminates", AssertTerminates
            "assertTotal", AssertTotal
            "break", Break
            "by", By
            "case", Case
            "clarify", Clarify
            "continue", Continue
            "ctor", Ctor
            "data", Data
            "desc", Desc
            "derive", Derive
            "distinct", Distinct
            "do", Do
            "elif", Elif
            "else", Else
            "except", Except
            "expect", Expect
            "export", Export
            "finally", Finally
            "for", For
            "for?", ForQuestion
            "forall", Forall
            "group", Group
            "if", If
            "import", Import
            "impossible", Impossible
            "in", In
            "inout", Inout
            "defer", Defer
            "infix", Infix
            "instance", Instance
            "is", Is
            "join", Join
            "lazy", Lazy
            "left", Left
            "let", Let
            "let?", LetQuestion
            "match", Match
            "module", Module
            "opaque", Opaque
            "order", Order
            "place", Place
            "postfix", Postfix
            "prefix", Prefix
            "private", Private
            "projection", Projection
            "public", Public
            "return", Return
            "right", Right
            "seal", Seal
            "skip", Skip
            "take", Take
            "term", Term
            "then", Then
            "thunk", Thunk
            "top", Top
            "trait", Trait
            "try", Try
            "type", Type
            "unhide", Unhide
            "using", Using
            "var", Var
            "while", While
            "yield", Yield
        ]
        |> dict

    let tryParse (text: string) =
        match entries.TryGetValue(text) with
        | true, keyword -> Some keyword
        | _ -> None

    let toText keyword =
        entries
        |> Seq.find (fun pair -> pair.Value = keyword)
        |> fun pair -> pair.Key

type TokenKind =
    | Identifier
    | IntegerLiteral
    | FloatLiteral
    | StringLiteral
    | InterpolatedStringStart
    | StringTextSegment
    | InterpolationStart
    | InterpolationEnd
    | InterpolatedStringEnd
    | CharacterLiteral
    | Keyword of Keyword
    | Operator
    | AtSign
    | Backslash
    | Colon
    | Comma
    | Dot
    | Equals
    | Arrow
    | LeftParen
    | RightParen
    | LeftBracket
    | RightBracket
    | LeftEffectRow
    | RightEffectRow
    | LeftBrace
    | RightBrace
    | LeftSetBrace
    | RightSetBrace
    | Underscore
    | Newline
    | Indent
    | Dedent
    | EndOfFile
    | BadToken

type Token =
    { Kind: TokenKind
      Text: string
      Span: TextSpan }

type Visibility =
    | Public
    | Private

type UrlModulePin =
    | Sha256Pin of digest: string
    | RefPin of reference: string

type UrlModuleSpecifier =
    { OriginalText: string
      BaseUrl: string
      Pin: UrlModulePin option }

type ModuleSpecifier =
    | Dotted of string list
    | Url of UrlModuleSpecifier

type ImportNamespace =
    | Term
    | Type
    | Trait
    | Constructor

type ImportItemModifier =
    | Unhide
    | Clarify

type ImportItem =
    { Modifiers: ImportItemModifier list
      Namespace: ImportNamespace option
      Name: string
      IncludeConstructors: bool
      Alias: string option }

type ExceptItem =
    { Namespace: ImportNamespace option
      Name: string }

type ImportSelection =
    | QualifiedOnly
    | Items of ImportItem list
    | All
    | AllExcept of ExceptItem list

type ImportSpec =
    { Source: ModuleSpecifier
      Alias: string option
      Selection: ImportSelection }

type LiteralValue =
    | Integer of int64
    | Float of double
    | String of string
    | Character of string
    | Grapheme of string
    | Byte of byte
    | Unit

type SurfaceNumericLiteral =
    | SurfaceIntegerLiteral of value: BigInteger * sourceText: string * suffix: string option
    | SurfaceRealLiteral of decimalValue: decimal option * sourceText: string * suffix: string option

type Quantity =
    | QuantityZero
    | QuantityOne
    | QuantityBorrow of regionName: string option
    | QuantityOmega
    | QuantityAtMostOne
    | QuantityAtLeastOne
    | QuantityVariable of string

module Quantity =
    let toSurfaceText quantity =
        match quantity with
        | QuantityZero -> "0"
        | QuantityOne -> "1"
        | QuantityBorrow None -> "&"
        | QuantityBorrow (Some regionName) -> $"&[{regionName}]"
        | QuantityOmega -> "\u03c9"
        | QuantityAtMostOne -> "<=1"
        | QuantityAtLeastOne -> ">=1"
        | QuantityVariable name -> name

type Parameter =
    { Name: string
      TypeTokens: Token list option
      Quantity: Quantity option
      IsImplicit: bool
      IsInout: bool
      IsReceiver: bool }

type FixityAssociativity =
    | NonAssociative
    | LeftAssociative
    | RightAssociative

type FixityKind =
    | Infix of FixityAssociativity
    | Prefix
    | Postfix

type FixityDeclaration =
    { Fixity: FixityKind
      Precedence: int
      OperatorName: string }

type KindSelector =
    | TypeKind
    | TraitKind
    | EffectLabelKind
    | ModuleKind

type BindingSignature =
    { Visibility: Visibility option
      IsOpaque: bool
      Name: string
      TypeTokens: Token list }

type TotalityAssertion =
    | AssertTerminatesAssertion
    | AssertReducibleAssertion

type TypeAlias =
    { Visibility: Visibility option
      IsOpaque: bool
      TotalityAssertion: TotalityAssertion option
      Name: string
      HeaderTokens: Token list
      BodyTokens: Token list option }

type EffectOperation =
    { OperationId: string option
      Name: string
      ResumptionQuantity: Quantity option
      SignatureTokens: Token list }

type EffectDeclaration =
    { EffectInterfaceId: string option
      EffectLabelId: string option
      Visibility: Visibility option
      Name: string
      HeaderTokens: Token list
      Operations: EffectOperation list }

type SurfaceExpression =
    | Literal of LiteralValue
    | NumericLiteral of SurfaceNumericLiteral
    | Name of string list
    | KindQualifiedName of KindSelector * string list
    | TypeSyntaxTokens of Token list
    | SyntaxQuote of SurfaceExpression
    | SyntaxSplice of SurfaceExpression
    | TopLevelSyntaxSplice of SurfaceExpression
    | CodeQuote of SurfaceExpression
    | CodeSplice of SurfaceExpression
    | LocalLet of binding: SurfaceBindPattern * value: SurfaceExpression * body: SurfaceExpression
    | LocalSignature of declaration: BindingSignature * body: SurfaceExpression
    | LocalTypeAlias of declaration: TypeAlias * body: SurfaceExpression
    | LocalScopedEffect of declaration: EffectDeclaration * body: SurfaceExpression
    | Lambda of Parameter list * SurfaceExpression
    | IfThenElse of SurfaceExpression * SurfaceExpression * SurfaceExpression
    | Handle of isDeep: bool * label: SurfaceExpression * body: SurfaceExpression * returnClause: SurfaceEffectHandlerClause * operationClauses: SurfaceEffectHandlerClause list
    | Match of SurfaceExpression * SurfaceMatchCase list
    | RecordLiteral of fields: SurfaceRecordLiteralField list
    | Seal of value: SurfaceExpression * ascriptionTokens: Token list
    | RecordUpdate of receiver: SurfaceExpression * fields: SurfaceRecordUpdateField list
    | MemberAccess of receiver: SurfaceExpression * segments: string list * arguments: SurfaceExpression list
    | SafeNavigation of receiver: SurfaceExpression * navigation: SurfaceSafeNavigationMember
    | TagTest of receiver: SurfaceExpression * constructorName: string list
    | Do of SurfaceDoStatement list
    | MonadicSplice of SurfaceExpression
    | Apply of SurfaceExpression * SurfaceExpression list
    | ExplicitImplicitArgument of SurfaceExpression
    | NamedApplicationBlock of fields: SurfaceRecordLiteralField list
    | InoutArgument of SurfaceExpression
    | Unary of operatorName: string * SurfaceExpression
    | Binary of SurfaceExpression * operatorName: string * SurfaceExpression
    | Elvis of SurfaceExpression * SurfaceExpression
    | Comprehension of SurfaceComprehension
    | PrefixedString of prefix: string * parts: SurfaceInterpolatedStringPart list

and SurfaceCollectionKind =
    | ListCollection
    | SetCollection
    | MapCollection

and SurfaceOrderDirection =
    | Ascending
    | Descending

and SurfaceComprehensionYield =
    | YieldValue of SurfaceExpression
    | YieldKeyValue of key: SurfaceExpression * value: SurfaceExpression

and SurfaceMapConflictPolicy =
    | KeepLast
    | KeepFirst
    | CombineUsing of string list
    | CombineWith of SurfaceExpression

and SurfaceComprehension =
    { CollectionKind: SurfaceCollectionKind
      Clauses: SurfaceComprehensionClause list
      Yield: SurfaceComprehensionYield
      ConflictPolicy: SurfaceMapConflictPolicy option
      Lowered: SurfaceExpression }

and SurfaceComprehensionClause =
    | ForClause of isBorrowed: bool * isRefutable: bool * binding: SurfaceBindPattern * source: SurfaceExpression
    | JoinClause of binding: SurfaceBindPattern * source: SurfaceExpression * condition: SurfaceExpression
    | LetClause of isRefutable: bool * binding: SurfaceBindPattern * value: SurfaceExpression
    | IfClause of SurfaceExpression
    | GroupByClause of key: SurfaceExpression * aggregations: SurfaceRecordLiteralField list * intoName: string
    | OrderByClause of direction: SurfaceOrderDirection option * SurfaceExpression
    | DistinctClause
    | DistinctByClause of SurfaceExpression
    | SkipClause of SurfaceExpression
    | TakeClause of SurfaceExpression
    | LeftJoinClause of binding: SurfaceBindPattern * source: SurfaceExpression * condition: SurfaceExpression * intoName: string

and SurfaceEffectHandlerClause =
    { OperationName: string
      ArgumentTokens: Token list list
      ResumptionName: string option
      Body: SurfaceExpression }

and SurfaceInterpolatedStringPart =
    | StringText of string
    | StringInterpolation of expression: SurfaceExpression * format: string option

and SurfaceSafeNavigationMember =
    { Segments: string list
      Arguments: SurfaceExpression list }

and SurfaceRecordLiteralField =
    { Name: string
      IsImplicit: bool
      Value: SurfaceExpression }

and SurfaceRecordUpdateField =
    { Name: string
      IsImplicit: bool
      IsExtension: bool
      Path: SurfaceRecordUpdatePathSegment list
      Value: SurfaceExpression }

and SurfaceRecordUpdatePathSegment =
    { Name: string
      IsImplicit: bool }

and SurfacePattern =
    | WildcardPattern
    | NamePattern of string
    | LiteralPattern of LiteralValue
    | AsPattern of string * SurfacePattern
    | TypedPattern of SurfacePattern * Token list
    | ConstructorPattern of string list * SurfacePattern list
    | NamedConstructorPattern of string list * SurfaceRecordPatternField list
    | TuplePattern of SurfacePattern list
    | VariantPattern of SurfaceVariantPattern
    | OrPattern of SurfacePattern list
    | AnonymousRecordPattern of SurfaceRecordPatternField list * SurfaceRecordPatternRest option

and SurfaceRecordPatternField =
    { Name: string
      IsImplicit: bool
      Pattern: SurfacePattern }

and SurfaceRecordPatternRest =
    | DiscardRecordPatternRest
    | BindRecordPatternRest of string

and SurfaceVariantPattern =
    | BoundVariantPattern of name: string * typeTokens: Token list option
    | WildcardVariantPattern of typeTokens: Token list option
    | RestVariantPattern of string

and SurfaceMatchCase =
    { Pattern: SurfacePattern
      Guard: SurfaceExpression option
      Body: SurfaceExpression }

and SurfaceBindPattern =
    { Pattern: SurfacePattern
      Quantity: Quantity option
      IsImplicit: bool
      TypeTokens: Token list option
      BinderSpans: Map<string, TextSpan list> }

and SurfaceDoStatement =
    | DoLet of SurfaceBindPattern * SurfaceExpression
    | DoLetQuestion of SurfaceBindPattern * SurfaceExpression * SurfaceLetQuestionFailure option
    | DoBind of SurfaceBindPattern * SurfaceExpression
    | DoVar of string * SurfaceExpression
    | DoAssign of string * SurfaceExpression
    | DoUsing of SurfaceBindPattern * SurfaceExpression
    | DoDefer of SurfaceExpression
    | DoIf of condition: SurfaceExpression * whenTrue: SurfaceDoStatement list * whenFalse: SurfaceDoStatement list
    | DoWhile of condition: SurfaceExpression * body: SurfaceDoStatement list
    | DoReturn of SurfaceExpression
    | DoExpression of SurfaceExpression

and SurfaceLetQuestionFailure =
    { ResiduePattern: SurfaceBindPattern
      Body: SurfaceDoStatement list }

module SurfaceComprehensionOps =
    let mapExpressions (mapper: SurfaceExpression -> SurfaceExpression) (comprehension: SurfaceComprehension) =
        let mapClause clause =
            match clause with
            | ForClause(isBorrowed, isRefutable, binding, source) ->
                ForClause(isBorrowed, isRefutable, binding, mapper source)
            | JoinClause(binding, source, condition) ->
                JoinClause(binding, mapper source, mapper condition)
            | LetClause(isRefutable, binding, value) ->
                LetClause(isRefutable, binding, mapper value)
            | IfClause condition ->
                IfClause(mapper condition)
            | GroupByClause(key, aggregations, intoName) ->
                GroupByClause(
                    mapper key,
                    aggregations
                    |> List.map (fun field ->
                        { field with
                            Value = mapper field.Value }),
                    intoName
                )
            | OrderByClause(direction, key) ->
                OrderByClause(direction, mapper key)
            | DistinctClause ->
                DistinctClause
            | DistinctByClause key ->
                DistinctByClause(mapper key)
            | SkipClause count ->
                SkipClause(mapper count)
            | TakeClause count ->
                TakeClause(mapper count)
            | LeftJoinClause(binding, source, condition, intoName) ->
                LeftJoinClause(binding, mapper source, mapper condition, intoName)

        let mappedYield =
            match comprehension.Yield with
            | YieldValue value ->
                YieldValue(mapper value)
            | YieldKeyValue(key, value) ->
                YieldKeyValue(mapper key, mapper value)

        let mappedConflict =
            comprehension.ConflictPolicy
            |> Option.map (function
                | KeepLast -> KeepLast
                | KeepFirst -> KeepFirst
                | CombineUsing name -> CombineUsing name
                | CombineWith expression -> CombineWith(mapper expression))

        { CollectionKind = comprehension.CollectionKind
          Clauses = comprehension.Clauses |> List.map mapClause
          Yield = mappedYield
          ConflictPolicy = mappedConflict
          Lowered = mapper comprehension.Lowered }

    let iterExpressions (visitor: SurfaceExpression -> unit) (comprehension: SurfaceComprehension) =
        let visitClause clause =
            match clause with
            | ForClause(_, _, _, source) ->
                visitor source
            | JoinClause(_, source, condition) ->
                visitor source
                visitor condition
            | LetClause(_, _, value) ->
                visitor value
            | IfClause condition ->
                visitor condition
            | GroupByClause(key, aggregations, _) ->
                visitor key
                aggregations |> List.iter (fun field -> visitor field.Value)
            | OrderByClause(_, key) ->
                visitor key
            | DistinctClause ->
                ()
            | DistinctByClause key ->
                visitor key
            | SkipClause count ->
                visitor count
            | TakeClause count ->
                visitor count
            | LeftJoinClause(_, source, condition, _) ->
                visitor source
                visitor condition

        comprehension.Clauses |> List.iter visitClause
        match comprehension.Yield with
        | YieldValue value ->
            visitor value
        | YieldKeyValue(key, value) ->
            visitor key
            visitor value

        match comprehension.ConflictPolicy with
        | Some(CombineWith expression) ->
            visitor expression
        | _ ->
            ()

        visitor comprehension.Lowered

    let collectExpressions (comprehension: SurfaceComprehension) =
        let expressions = ResizeArray<SurfaceExpression>()
        iterExpressions expressions.Add comprehension
        List.ofSeq expressions

type ExpectedTypeDeclaration =
    { Name: string
      HeaderTokens: Token list
      Span: TextSpan }

type ExpectedTraitDeclaration =
    { Name: string
      HeaderTokens: Token list
      Span: TextSpan }

type ExpectedTermDeclaration =
    { Name: string
      TypeTokens: Token list
      Span: TextSpan }

type ExpectDeclaration =
    | ExpectTypeDeclaration of ExpectedTypeDeclaration
    | ExpectTraitDeclaration of ExpectedTraitDeclaration
    | ExpectTermDeclaration of ExpectedTermDeclaration

type LetDefinition =
    { Visibility: Visibility option
      IsOpaque: bool
      TotalityAssertion: TotalityAssertion option
      IsPattern: bool
      Name: string option
      Parameters: Parameter list
      HeaderTokens: Token list
      ReturnTypeTokens: Token list option
      BodyTokens: Token list
      Body: SurfaceExpression option }

type ProjectionPlaceBinder =
    { Name: string
      TypeTokens: Token list
      IsReceiver: bool }

type ProjectionBinder =
    | ProjectionPlaceBinder of ProjectionPlaceBinder
    | ProjectionValueBinder of Parameter

type SurfaceProjectionBody =
    | ProjectionYield of SurfaceExpression
    | ProjectionIfThenElse of SurfaceExpression * SurfaceProjectionBody * SurfaceProjectionBody
    | ProjectionMatch of SurfaceExpression * SurfaceProjectionCase list
    | ProjectionAccessors of SurfaceProjectionAccessorClause list

and SurfaceProjectionCase =
    { Pattern: SurfacePattern
      Guard: SurfaceExpression option
      Body: SurfaceProjectionBody }

and SurfaceProjectionAccessorClause =
    | ProjectionGet of SurfaceExpression
    | ProjectionInout of SurfaceExpression
    | ProjectionSet of parameterName: string * typeTokens: Token list * body: SurfaceExpression
    | ProjectionSink of SurfaceExpression

type ProjectionDeclaration =
    { Visibility: Visibility option
      Name: string
      Binders: ProjectionBinder list
      ReturnTypeTokens: Token list
      BodyTokens: Token list
      Body: SurfaceProjectionBody option }

type DataConstructorParameter =
    { ParameterName: string option
      ParameterTypeTokens: Token list
      ParameterQuantity: Quantity option
      ParameterIsImplicit: bool
      DefaultValue: SurfaceExpression option }

type DataConstructor =
    { Name: string
      Tokens: Token list
      Arity: int
      Parameters: DataConstructorParameter list option }

type DataDeclaration =
    { Visibility: Visibility option
      IsOpaque: bool
      Name: string
      HeaderTokens: Token list
      Constructors: DataConstructor list }

type TraitMember =
    { Name: string option
      DefaultDefinition: LetDefinition option
      Tokens: Token list }

type TraitDeclaration =
    { Visibility: Visibility option
      Name: string
      FullHeaderTokens: Token list
      HeaderTokens: Token list
      Members: TraitMember list }

type InstanceDeclaration =
    { TraitName: string
      FullHeaderTokens: Token list
      HeaderTokens: Token list
      Members: LetDefinition list }

type TopLevelDeclaration =
    | ImportDeclaration of isExport: bool * specs: ImportSpec list
    | FixityDeclarationNode of FixityDeclaration
    | ExpectDeclarationNode of ExpectDeclaration
    | SignatureDeclaration of BindingSignature
    | LetDeclaration of LetDefinition
    | DataDeclarationNode of DataDeclaration
    | TypeAliasNode of TypeAlias
    | EffectDeclarationNode of EffectDeclaration
    | ProjectionDeclarationNode of ProjectionDeclaration
    | TraitDeclarationNode of TraitDeclaration
    | InstanceDeclarationNode of InstanceDeclaration
    | UnknownDeclaration of Token list

type CompilationUnit =
    { ModuleAttributes: string list
      ModuleHeader: string list option
      Declarations: TopLevelDeclaration list
      Tokens: Token list }

module Token =
    let isKeyword keyword token =
        match token.Kind with
        | Keyword value when value = keyword -> true
        | _ -> false

    let isName token =
        match token.Kind with
        | Identifier
        | Keyword _ -> true
        | _ -> false

module ModuleAttribute =
    [<Literal>]
    let PrivateByDefault = "PrivateByDefault"

    [<Literal>]
    let AllowAssertTerminates = "allow_assert_terminates"

    [<Literal>]
    let AllowAssertReducible = "allow_assert_reducible"

    [<Literal>]
    let AllowUnsafeConsume = "allow_unsafe_consume"

    let knownAttributes =
        Set.ofList
            [
                PrivateByDefault
                AllowAssertTerminates
                AllowAssertReducible
                AllowUnsafeConsume
            ]

    let isKnown attributeName =
        Set.contains attributeName knownAttributes

// Holds syntax-level helpers for names, paths, declarations, and module facts.
module SyntaxFacts =
    type ParsedNumericLiteral =
        { Literal: SurfaceNumericLiteral
          Suffix: string option }

    type NumericTokenScan =
        | ValidNumericToken of kind: TokenKind * endIndex: int
        | InvalidNumericToken of message: string * endIndex: int

    type PrefixedStringStartInfo =
        { PrefixText: string
          HashCount: int }

    let isIdentifierStart character = Char.IsLetter(character) || character = '_'

    let isIdentifierPart character =
        Char.IsLetterOrDigit(character) || character = '_'

    let isOperatorCharacter character =
        "!$%&*+-./:<=>?@^|~".Contains(string character)

    let trimIdentifierQuotes (value: string) =
        if value.Length >= 2 && value[0] = '`' && value[value.Length - 1] = '`' then
            value.Substring(1, value.Length - 2)
        else
            value

    let trimStringQuotes (value: string) =
        if value.Length >= 2 && value[0] = '"' && value[value.Length - 1] = '"' then
            value.Substring(1, value.Length - 2)
        else
            value

    let tryParseUrlModuleSpecifier (text: string) =
        let hashIndex = text.IndexOf('#')

        if hashIndex < 0 then
            Result.Ok
                { OriginalText = text
                  BaseUrl = text
                  Pin = None }
        else
            let baseUrl = text.Substring(0, hashIndex)
            let pinText = text.Substring(hashIndex + 1)

            if String.IsNullOrWhiteSpace(baseUrl) then
                Result.Error MissingBaseUrl
            elif String.IsNullOrWhiteSpace(pinText) then
                Result.Error EmptyPin
            elif pinText.StartsWith("sha256:", StringComparison.OrdinalIgnoreCase) then
                let digest = pinText.Substring("sha256:".Length)

                if String.IsNullOrWhiteSpace(digest) then
                    Result.Error MissingSha256Digest
                elif digest |> Seq.forall Uri.IsHexDigit then
                    Result.Ok
                        { OriginalText = text
                          BaseUrl = baseUrl
                          Pin = Some(Sha256Pin(digest.ToLowerInvariant())) }
                else
                    Result.Error(InvalidSha256Digest pinText)
            elif pinText.StartsWith("ref:", StringComparison.Ordinal) then
                let reference = pinText.Substring("ref:".Length)

                if String.IsNullOrWhiteSpace(reference) then
                    Result.Error EmptyRefPin
                else
                    Result.Ok
                        { OriginalText = text
                          BaseUrl = baseUrl
                          Pin = Some(RefPin reference) }
            else
                Result.Error(UnsupportedPin pinText)

    let urlModuleSpecifierText (specifier: UrlModuleSpecifier) =
        specifier.OriginalText

    let urlModuleSpecifierIdentityText (specifier: UrlModuleSpecifier) =
        match specifier.Pin with
        | None ->
            specifier.BaseUrl
        | Some(Sha256Pin digest) ->
            $"{specifier.BaseUrl}#sha256:{digest}"
        | Some(RefPin reference) ->
            $"{specifier.BaseUrl}#ref:{reference}"

    let private prefixedStringMetadataSeparator = '\u001f'

    let encodePrefixedStringStart (prefixText: string) hashCount =
        $"{prefixText}{prefixedStringMetadataSeparator}{hashCount}"

    let tryDecodePrefixedStringStart (text: string) =
        let separatorIndex = text.LastIndexOf(prefixedStringMetadataSeparator)

        if separatorIndex <= 0 || separatorIndex >= text.Length - 1 then
            None
        else
            match Int32.TryParse(text.Substring(separatorIndex + 1)) with
            | true, hashCount ->
                Some
                    { PrefixText = text.Substring(0, separatorIndex)
                      HashCount = hashCount }
            | _ ->
                None

    type private ScannedNumericLiteral =
        { Kind: TokenKind
          IntegralDigits: string
          FractionalDigits: string option
          ExponentSign: char option
          ExponentDigits: string option
          IntegerBase: int
          Suffix: string option
          EndIndex: int }

    type private NumericLiteralScan =
        | ValidLiteralScan of NumericTokenScan * ScannedNumericLiteral
        | InvalidLiteralScan of NumericTokenScan

    let private tryReadDigits (text: string) startIndex (isDigit: char -> bool) =
        if startIndex >= text.Length || not (isDigit text[startIndex]) then
            None
        else
            let builder = StringBuilder()
            let mutable index = startIndex
            let mutable keepReading = true
            builder.Append(text[index]) |> ignore
            index <- index + 1

            while keepReading && index < text.Length do
                if isDigit text[index] then
                    builder.Append(text[index]) |> ignore
                    index <- index + 1
                elif text[index] = '_' && index + 1 < text.Length && isDigit text[index + 1] then
                    index <- index + 1
                else
                    keepReading <- false

            Some(builder.ToString(), index)

    let private readIdentifierSuffix (text: string) startIndex =
        if startIndex < text.Length && isIdentifierStart text[startIndex] then
            let mutable endIndex = startIndex + 1

            while endIndex < text.Length && isIdentifierPart text[endIndex] do
                endIndex <- endIndex + 1

            Some(text.Substring(startIndex, endIndex - startIndex)), endIndex
        else
            None, startIndex

    let private readMalformedNumericTail (text: string) startIndex =
        let mutable endIndex = startIndex

        while endIndex < text.Length && (Char.IsLetterOrDigit(text[endIndex]) || text[endIndex] = '_') do
            endIndex <- endIndex + 1

        endIndex

    let private tryDigitValue character =
        match character with
        | _ when character >= '0' && character <= '9' ->
            Some(int character - int '0')
        | _ when character >= 'a' && character <= 'f' ->
            Some(10 + int character - int 'a')
        | _ when character >= 'A' && character <= 'F' ->
            Some(10 + int character - int 'A')
        | _ ->
            None

    let private tryParseBigIntegerFromDigits integerBase (digits: string) =
        try
            let mutable value = BigInteger.Zero
            let baseValue = BigInteger(int integerBase)

            for character in digits do
                match tryDigitValue character with
                | Some digit when digit < integerBase ->
                    value <- value * baseValue + BigInteger(digit)
                | _ ->
                    raise (FormatException("Invalid digit"))

            Some value
        with
        | :? FormatException ->
            None

    let private scanNumericLiteralAt (text: string) startIndex =
        let length = text.Length

        let invalidPrefixedIntegerLiteral baseName invalidStartIndex =
            let endIndex = readMalformedNumericTail text invalidStartIndex
            InvalidLiteralScan(InvalidNumericToken($"Malformed {baseName} integer literal.", max endIndex invalidStartIndex))

        let buildInteger integerBase digits endIndex =
            let suffix, finalEndIndex = readIdentifierSuffix text endIndex

            ValidLiteralScan(
                ValidNumericToken(IntegerLiteral, finalEndIndex),
                { Kind = IntegerLiteral
                  IntegralDigits = digits
                  FractionalDigits = None
                  ExponentSign = None
                  ExponentDigits = None
                  IntegerBase = integerBase
                  Suffix = suffix
                  EndIndex = finalEndIndex }
            )

        if startIndex >= length || not (Char.IsDigit text[startIndex]) then
            None
        elif startIndex + 1 < length
             && text[startIndex] = '0'
            && (text[startIndex + 1] = 'x' || text[startIndex + 1] = 'X') then
            match tryReadDigits text (startIndex + 2) (fun character -> tryDigitValue character |> Option.exists (fun digit -> digit < 16)) with
            | Some(digits, endIndex) ->
                Some(buildInteger 16 digits endIndex)
            | None ->
                Some(invalidPrefixedIntegerLiteral "hexadecimal" (startIndex + 2))
        elif startIndex + 1 < length
             && text[startIndex] = '0'
             && (text[startIndex + 1] = 'o' || text[startIndex + 1] = 'O') then
            match tryReadDigits text (startIndex + 2) (fun character -> character >= '0' && character <= '7') with
            | Some(_, endIndex) when endIndex < length && Char.IsDigit text[endIndex] ->
                Some(invalidPrefixedIntegerLiteral "octal" endIndex)
            | Some(digits, endIndex) ->
                Some(buildInteger 8 digits endIndex)
            | None ->
                Some(invalidPrefixedIntegerLiteral "octal" (startIndex + 2))
        elif startIndex + 1 < length
             && text[startIndex] = '0'
             && (text[startIndex + 1] = 'b' || text[startIndex + 1] = 'B') then
            match tryReadDigits text (startIndex + 2) (fun character -> character = '0' || character = '1') with
            | Some(_, endIndex) when endIndex < length && Char.IsDigit text[endIndex] ->
                Some(invalidPrefixedIntegerLiteral "binary" endIndex)
            | Some(digits, endIndex) ->
                Some(buildInteger 2 digits endIndex)
            | None ->
                Some(invalidPrefixedIntegerLiteral "binary" (startIndex + 2))
        else
            match tryReadDigits text startIndex Char.IsDigit with
            | None ->
                None
            | Some(integralDigits, afterIntegral) ->
                let fractionalDigits, afterFraction =
                    if afterIntegral + 1 < length
                       && text[afterIntegral] = '.'
                       && Char.IsDigit text[afterIntegral + 1] then
                        match tryReadDigits text (afterIntegral + 1) Char.IsDigit with
                        | Some digits ->
                            Some(fst digits), snd digits
                        | None ->
                            None, afterIntegral
                    else
                        None, afterIntegral

                let exponentSign, exponentDigits, afterExponent =
                    if afterFraction < length && (text[afterFraction] = 'e' || text[afterFraction] = 'E') then
                        let signIndex =
                            if afterFraction + 1 < length && (text[afterFraction + 1] = '+' || text[afterFraction + 1] = '-') then
                                afterFraction + 2
                            else
                                afterFraction + 1

                        match tryReadDigits text signIndex Char.IsDigit with
                        | Some(digits, exponentEndIndex) ->
                            let sign =
                                if signIndex = afterFraction + 2 then
                                    Some text[afterFraction + 1]
                                else
                                    None

                            sign, Some digits, exponentEndIndex
                        | None ->
                            None, None, afterFraction
                    else
                        None, None, afterFraction

                let suffix, finalEndIndex = readIdentifierSuffix text afterExponent
                let kind = if fractionalDigits.IsSome || exponentDigits.IsSome then FloatLiteral else IntegerLiteral

                let scanned =
                    { Kind = kind
                      IntegralDigits = integralDigits
                      FractionalDigits = fractionalDigits
                      ExponentSign = exponentSign
                      ExponentDigits = exponentDigits
                      IntegerBase = 10
                      Suffix = suffix
                      EndIndex = finalEndIndex }

                Some(ValidLiteralScan(ValidNumericToken(kind, finalEndIndex), scanned))

    let scanNumericToken (text: string) startIndex =
        scanNumericLiteralAt text startIndex
        |> Option.map (function
            | ValidLiteralScan(tokenScan, _) -> tokenScan
            | InvalidLiteralScan tokenScan -> tokenScan)

    let private tryScanNumericLiteralAt (text: string) startIndex =
        match scanNumericLiteralAt text startIndex with
        | Some(ValidLiteralScan(_, scanned)) -> Some scanned
        | _ -> None

    let tryReadNumericLiteral (text: string) startIndex =
        match scanNumericToken text startIndex with
        | Some(ValidNumericToken(kind, endIndex)) -> Some(kind, endIndex)
        | _ -> None

    let tryParseNumericLiteral (text: string) =
        match tryScanNumericLiteralAt text 0 with
        | Some scanned when scanned.EndIndex = text.Length ->
            match scanned.Kind with
            | IntegerLiteral ->
                match tryParseBigIntegerFromDigits scanned.IntegerBase scanned.IntegralDigits with
                | Some value ->
                    Result.Ok
                        { Literal = SurfaceIntegerLiteral(value, text.Substring(0, text.Length - (scanned.Suffix |> Option.map String.length |> Option.defaultValue 0)), scanned.Suffix)
                          Suffix = scanned.Suffix }
                | None ->
                    Result.Error(InvalidNumericLiteral text)
            | FloatLiteral ->
                let builder = StringBuilder()
                builder.Append(scanned.IntegralDigits) |> ignore

                match scanned.FractionalDigits with
                | Some digits ->
                    builder.Append('.') |> ignore
                    builder.Append(digits) |> ignore
                | None ->
                    ()

                match scanned.ExponentDigits with
                | Some digits ->
                    builder.Append('e') |> ignore

                    match scanned.ExponentSign with
                    | Some sign -> builder.Append(sign) |> ignore
                    | None -> ()

                    builder.Append(digits) |> ignore
                | None ->
                    ()

                let normalizedText = builder.ToString()
                let decimalValue =
                    match Decimal.TryParse(normalizedText, Globalization.NumberStyles.Float, Globalization.CultureInfo.InvariantCulture) with
                    | true, value -> Some value
                    | _ -> None

                let sourceText = text.Substring(0, text.Length - (scanned.Suffix |> Option.map String.length |> Option.defaultValue 0))

                Result.Ok
                    { Literal = SurfaceRealLiteral(decimalValue, sourceText, scanned.Suffix)
                      Suffix = scanned.Suffix }
            | _ ->
                Result.Error(InvalidNumericLiteral text)
        | _ ->
            Result.Error(InvalidNumericLiteral text)

    let private renderStringLiteralDecodeError error =
        match error with
        | UnknownEscapeSequence escapeText -> $"Unknown escape sequence '{escapeText}'."
        | InvalidUnicodeEscape escapeText -> $"Invalid Unicode escape '{escapeText}'."
        | UnterminatedEscapeSequence -> "Unterminated escape sequence."
        | UnterminatedUnicodeEscapeSequence -> "Unterminated Unicode escape sequence."
        | InvalidMultilineClosingDelimiterIndentation -> "Invalid multiline string closing delimiter indentation."
        | MultilineContentIndentationMismatch -> "A multiline string content line does not match the closing delimiter indentation."
        | InvalidRawMultilineStringLiteral -> "Invalid raw multiline string literal."
        | InvalidRawStringLiteral -> "Invalid raw string literal."

    let tryUnescapeStringContentDetailed (value: string) =
        let builder = StringBuilder()
        let mutable index = 0
        let mutable error = None

        let appendRuneFromValue escapePrefix (value: int) =
            match UnicodeText.tryScalarFromValue value with
            | Some rune ->
                builder.Append(rune.ToString()) |> ignore
            | None ->
                error <- Some(InvalidUnicodeEscape escapePrefix)

        let appendHexScalar escapePrefix (codePointText: string) =
            match Int32.TryParse(codePointText, Globalization.NumberStyles.HexNumber, null) with
            | true, value ->
                appendRuneFromValue escapePrefix value
            | _ ->
                error <- Some(InvalidUnicodeEscape escapePrefix)

        while index < value.Length && error.IsNone do
            if value[index] <> '\\' then
                builder.Append(value[index]) |> ignore
                index <- index + 1
            elif index + 1 >= value.Length then
                error <- Some UnterminatedEscapeSequence
            else
                match value[index + 1] with
                | '\\' ->
                    builder.Append('\\') |> ignore
                    index <- index + 2
                | '"' ->
                    builder.Append('"') |> ignore
                    index <- index + 2
                | '\'' ->
                    builder.Append('\'') |> ignore
                    index <- index + 2
                | '$' ->
                    builder.Append('$') |> ignore
                    index <- index + 2
                | 'n' ->
                    builder.Append('\n') |> ignore
                    index <- index + 2
                | 't' ->
                    builder.Append('\t') |> ignore
                    index <- index + 2
                | 'r' ->
                    builder.Append('\r') |> ignore
                    index <- index + 2
                | 'b' ->
                    builder.Append('\b') |> ignore
                    index <- index + 2
                | 'x' when index + 3 < value.Length ->
                    appendHexScalar ($"\\x{value.Substring(index + 2, 2)}") (value.Substring(index + 2, 2))
                    index <- index + 4
                | 'u' when index + 2 < value.Length && value[index + 2] = '{' ->
                    let closingBrace = value.IndexOf('}', index + 3)

                    if closingBrace < 0 then
                        error <- Some UnterminatedUnicodeEscapeSequence
                        index <- value.Length
                    else
                        let codePointText = value.Substring(index + 3, closingBrace - (index + 3))

                        if String.IsNullOrWhiteSpace(codePointText) || codePointText.Length > 6 then
                            error <- Some(InvalidUnicodeEscape $"\\u{{{codePointText}}}")
                        else
                            appendHexScalar ($"\\u{{{codePointText}}}") codePointText

                        index <- closingBrace + 1
                | 'u' when index + 5 < value.Length ->
                    let codePointText = value.Substring(index + 2, 4)
                    appendHexScalar ($"\\u{codePointText}") codePointText
                    index <- index + 6
                | other ->
                    error <- Some(UnknownEscapeSequence $"\\{other}")

        match error with
        | Some message -> Result.Error message
        | None -> Result.Ok(builder.ToString())

    let tryUnescapeStringContent (value: string) =
        tryUnescapeStringContentDetailed value
        |> Result.mapError renderStringLiteralDecodeError

    let private normalizeStringLineEndings (value: string) =
        value.Replace("\r\n", "\n").Replace('\r', '\n')

    let private tryApplyFixedDedent (body: string) =
        let body = normalizeStringLineEndings body

        let body =
            if body.StartsWith("\n", StringComparison.Ordinal) then
                body.Substring(1)
            else
                body

        let closingLineStart =
            match body.LastIndexOf('\n') with
            | -1 -> 0
            | index -> index + 1

        let closingIndent = body.Substring(closingLineStart)

        if closingIndent |> Seq.exists (fun character -> character <> ' ') then
            Result.Error InvalidMultilineClosingDelimiterIndentation
        else
            let content =
                if closingLineStart = 0 then
                    body
                else
                    body.Substring(0, closingLineStart)

            let lines = content.Split('\n', StringSplitOptions.None)
            let dedented = ResizeArray<string>()
            let mutable error = None

            for line in lines do
                if error.IsNone then
                    if line.Length = 0 then
                        dedented.Add("")
                    elif line |> Seq.forall ((=) ' ') then
                        dedented.Add(line.Substring(min closingIndent.Length line.Length))
                    elif line.StartsWith(closingIndent, StringComparison.Ordinal) then
                        dedented.Add(line.Substring(closingIndent.Length))
                    else
                        error <- Some MultilineContentIndentationMismatch

            match error with
            | Some message -> Result.Error message
            | None -> Result.Ok(String.concat "\n" dedented)

    let private tryParseHashesBeforeQuote (value: string) =
        let mutable index = 0

        while index < value.Length && value[index] = '#' do
            index <- index + 1

        if index > 0 && index < value.Length && value[index] = '"' then
            Some index
        else
            None

    let tryDecodeStringLiteral (value: string) =
        let value = normalizeStringLineEndings value

        let tryDecodeOrdinary body =
            tryUnescapeStringContentDetailed body

        match tryParseHashesBeforeQuote value with
        | Some hashCount ->
            let isMultiline =
                hashCount + 2 < value.Length
                && value[hashCount] = '"'
                && value[hashCount + 1] = '"'
                && value[hashCount + 2] = '"'

            let trailingHashes = String.replicate hashCount "#"

            if isMultiline then
                let closingDelimiter = "\"\"\"" + trailingHashes

                if value.Length < hashCount + 6 || not (value.EndsWith(closingDelimiter, StringComparison.Ordinal)) then
                    Result.Error InvalidRawMultilineStringLiteral
                else
                    let body = value.Substring(hashCount + 3, value.Length - (hashCount + 3) - closingDelimiter.Length)
                    tryApplyFixedDedent body
            else
                let closingDelimiter = "\"" + trailingHashes

                if value.Length < hashCount + 2 || not (value.EndsWith(closingDelimiter, StringComparison.Ordinal)) then
                    Result.Error InvalidRawStringLiteral
                else
                    Result.Ok(value.Substring(hashCount + 1, value.Length - (hashCount + 1) - closingDelimiter.Length))
        | None when value.StartsWith("\"\"\"", StringComparison.Ordinal) && value.EndsWith("\"\"\"", StringComparison.Ordinal) ->
            let body = value.Substring(3, value.Length - 6)

            tryApplyFixedDedent body
            |> Result.bind tryDecodeOrdinary
        | None ->
            trimStringQuotes value
            |> tryDecodeOrdinary

    let private tryTrimQuotedLiteralBody prefix (value: string) =
        if value.Length >= 2 && value[0] = '\'' && value[value.Length - 1] = '\'' then
            Result.Ok(value.Substring(1, value.Length - 2))
        else
            Result.Error $"Invalid {prefix} literal."

    let private tryDecodeQuotedLiteralBody prefix (value: string) =
        tryTrimQuotedLiteralBody prefix value
        |> Result.bind tryUnescapeStringContent

    let private tryDecodeByteLiteralBody (body: string) =
        let bytes = ResizeArray<byte>()
        let mutable index = 0
        let mutable error = None

        let rejectInvalidByteLiteral () =
            error <- Some "Byte literals must decode to exactly one byte value."

        let appendByte (value: byte) =
            bytes.Add(value)

        let appendRawPrintableAscii (value: char) =
            if value >= ' ' && value <= '~' then
                appendByte (byte value)
            else
                rejectInvalidByteLiteral ()

        let appendUnicodeEscape escapePrefix (codePointText: string) =
            match Int32.TryParse(codePointText, Globalization.NumberStyles.HexNumber, null) with
            | true, value ->
                match UnicodeText.tryScalarFromValue value with
                | Some rune ->
                    let encoded = UnicodeText.encodeUtf8 (rune.ToString())

                    if encoded.Length = 1 then
                        appendByte encoded[0]
                    else
                        rejectInvalidByteLiteral ()
                | None ->
                    error <- Some $"Invalid Unicode escape '{escapePrefix}'."
            | _ ->
                error <- Some $"Invalid Unicode escape '{escapePrefix}'."

        let appendByteEscape escapePrefix (byteText: string) =
            match Byte.TryParse(byteText, Globalization.NumberStyles.HexNumber, null) with
            | true, value ->
                appendByte value
            | _ ->
                error <- Some $"Invalid byte escape '{escapePrefix}'."

        while index < body.Length && error.IsNone do
            if body[index] <> '\\' then
                appendRawPrintableAscii body[index]
                index <- index + 1
            elif index + 1 >= body.Length then
                error <- Some "Unterminated escape sequence."
            else
                match body[index + 1] with
                | '\\' ->
                    appendByte (byte '\\')
                    index <- index + 2
                | '"' ->
                    appendByte (byte '"')
                    index <- index + 2
                | '\'' ->
                    appendByte (byte '\'')
                    index <- index + 2
                | '$' ->
                    appendByte (byte '$')
                    index <- index + 2
                | 'n' ->
                    appendByte (byte '\n')
                    index <- index + 2
                | 't' ->
                    appendByte (byte '\t')
                    index <- index + 2
                | 'r' ->
                    appendByte (byte '\r')
                    index <- index + 2
                | 'b' ->
                    appendByte (byte '\b')
                    index <- index + 2
                | 'x' when index + 3 < body.Length ->
                    let byteText = body.Substring(index + 2, 2)
                    appendByteEscape ($"\\x{byteText}") byteText
                    index <- index + 4
                | 'u' when index + 2 < body.Length && body[index + 2] = '{' ->
                    let closingBrace = body.IndexOf('}', index + 3)

                    if closingBrace < 0 then
                        error <- Some "Unterminated Unicode escape sequence."
                        index <- body.Length
                    else
                        let codePointText = body.Substring(index + 3, closingBrace - (index + 3))

                        if String.IsNullOrWhiteSpace(codePointText) || codePointText.Length > 6 then
                            error <- Some $"Invalid Unicode escape '\\u{{{codePointText}}}'."
                        else
                            appendUnicodeEscape ($"\\u{{{codePointText}}}") codePointText

                        index <- closingBrace + 1
                | 'u' when index + 5 < body.Length ->
                    let codePointText = body.Substring(index + 2, 4)
                    appendUnicodeEscape ($"\\u{codePointText}") codePointText
                    index <- index + 6
                | other ->
                    error <- Some $"Unknown escape sequence '\\{other}'."

        match error with
        | Some message -> Result.Error message
        | None when bytes.Count = 1 -> Result.Ok bytes[0]
        | None -> Result.Error "Byte literals must decode to exactly one byte value."

    let tryDecodeCharacterLiteral (value: string) =
        tryDecodeQuotedLiteralBody "Unicode scalar" value
        |> Result.bind (fun text ->
            match UnicodeText.trySingleScalar text with
            | Some rune ->
                Result.Ok(rune.ToString())
            | None ->
                Result.Error "Unicode scalar literals must decode to exactly one valid Unicode scalar."
        )

    let tryDecodeGraphemeLiteral (value: string) =
        tryDecodeQuotedLiteralBody "grapheme" value
        |> Result.bind (fun text ->
            match UnicodeText.trySingleGrapheme text with
            | Some grapheme ->
                Result.Ok grapheme
            | None ->
                Result.Error "Grapheme literals must decode to exactly one extended grapheme cluster."
        )

    let tryDecodeByteLiteral (value: string) =
        tryTrimQuotedLiteralBody "byte" value
        |> Result.bind tryDecodeByteLiteralBody

    let moduleNameToText (segments: string seq) = String.Join(".", segments)

module SurfaceNumericLiteral =
    let sourceText literal =
        match literal with
        | SurfaceIntegerLiteral(_, text, _)
        | SurfaceRealLiteral(_, text, _) ->
            text

    let suffix literal =
        match literal with
        | SurfaceIntegerLiteral(_, _, suffix)
        | SurfaceRealLiteral(_, _, suffix) ->
            suffix

    let withoutSuffix literal =
        match literal with
        | SurfaceIntegerLiteral(value, text, _) ->
            SurfaceIntegerLiteral(value, text, None)
        | SurfaceRealLiteral(decimalValue, text, _) ->
            SurfaceRealLiteral(decimalValue, text, None)

    let toSurfaceText literal =
        sourceText literal + (suffix literal |> Option.defaultValue "")

    let tryLowerPrimitiveLiteral literal =
        match literal with
        | SurfaceIntegerLiteral(value, _, _) when value >= BigInteger(Int64.MinValue) && value <= BigInteger(Int64.MaxValue) ->
            Some(LiteralValue.Integer(int64 value))
        | SurfaceRealLiteral(_, text, _) ->
            match Double.TryParse(text, Globalization.NumberStyles.Float, Globalization.CultureInfo.InvariantCulture) with
            | true, value ->
                Some(LiteralValue.Float value)
            | _ ->
                None
        | _ ->
            None

type FixityEntry =
    { Infix: (FixityAssociativity * int) option
      Prefix: int option
      Postfix: int option }

type FixityTable = Map<string, FixityEntry>

// Tracks operator fixities for bootstrap parsing and user-defined precedence updates.
module FixityTable =
    let empty : FixityTable = Map.empty

    let private emptyEntry =
        { Infix = None
          Prefix = None
          Postfix = None }

    let add (declaration: FixityDeclaration) (table: FixityTable) =
        let existing =
            table
            |> Map.tryFind declaration.OperatorName
            |> Option.defaultValue emptyEntry

        let updated =
            match declaration.Fixity with
            | Infix associativity ->
                { existing with
                    Infix = Some(associativity, declaration.Precedence) }
            | Prefix ->
                { existing with
                    Prefix = Some declaration.Precedence }
            | Postfix ->
                { existing with
                    Postfix = Some declaration.Precedence }

        table.Add(declaration.OperatorName, updated)

    let tryFindInfix operatorName (table: FixityTable) =
        table
        |> Map.tryFind operatorName
        |> Option.bind (fun entry -> entry.Infix)

    let tryFindPrefix operatorName (table: FixityTable) =
        table
        |> Map.tryFind operatorName
        |> Option.bind (fun entry -> entry.Prefix)

    let tryFindPostfix operatorName (table: FixityTable) =
        table
        |> Map.tryFind operatorName
        |> Option.bind (fun entry -> entry.Postfix)
