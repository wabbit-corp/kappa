namespace Kappa.Compiler

open System
open System.Collections.Generic

// Parses and reasons about textual type signatures used across compiler stages.
module TypeSignatures =
    [<RequireQualifiedAccess>]
    type NameOriginKind =
        | SourceSpelling
        | Synthetic of introductionKind: string

    type NameProvenance =
        { OriginKind: NameOriginKind
          OriginSpan: TextSpan option }

    [<CustomEquality; CustomComparison>]
    type ReferenceName =
        private
            { Spelling: NamesModel.Spelling
              CanonicalText: string
              Provenance: NameProvenance option }

        override this.Equals(other) =
            match other with
            | :? ReferenceName as other ->
                String.Equals(this.CanonicalText, other.CanonicalText, StringComparison.Ordinal)
            | _ ->
                false

        override this.GetHashCode() =
            StringComparer.Ordinal.GetHashCode(this.CanonicalText)

        interface IComparable with
            member this.CompareTo(other) =
                match other with
                | :? ReferenceName as other ->
                    StringComparer.Ordinal.Compare(this.CanonicalText, other.CanonicalText)
                | _ ->
                    invalidArg (nameof other) "Cannot compare reference names of different types."

    module ReferenceName =
        let private classifySpelling (renderedText: string) (canonicalText: string) =
            if renderedText.Length >= 2
               && renderedText[0] = '`'
               && renderedText[renderedText.Length - 1] = '`' then
                NamesModel.Spelling.BacktickIdent canonicalText
            elif canonicalText.Length > 0 && canonicalText |> Seq.forall SyntaxFacts.isOperatorCharacter then
                NamesModel.Spelling.Operator canonicalText
            else
                NamesModel.Spelling.Ident canonicalText

        let createWithProvenance provenance text =
            if String.IsNullOrWhiteSpace(text) then
                invalidArg (nameof text) "Reference names must not be blank."

            { Spelling = classifySpelling text text
              CanonicalText = text
              Provenance = provenance }

        let create text = createWithProvenance None text

        let createSynthetic introductionKind text =
            createWithProvenance
                (Some
                    { OriginKind = NameOriginKind.Synthetic introductionKind
                      OriginSpan = None })
                text

        let fromToken (token: Token) =
            let renderedText = token.Text

            let canonicalText =
                match token.Kind with
                | Operator
                | Colon
                | Equals ->
                    renderedText
                | _ ->
                    SyntaxFacts.trimIdentifierQuotes renderedText

            createWithProvenance
                (Some
                    { OriginKind = NameOriginKind.SourceSpelling
                      OriginSpan = Some token.Span })
                canonicalText
            |> fun name ->
                { name with
                    Spelling = classifySpelling renderedText canonicalText }

        let text name = name.CanonicalText

        let spelling name = name.Spelling

        let renderedText name =
            match name.Spelling with
            | NamesModel.Spelling.Ident text
            | NamesModel.Spelling.Operator text ->
                text
            | NamesModel.Spelling.BacktickIdent text ->
                $"`{text}`"

        let provenance name = name.Provenance

    [<CustomEquality; CustomComparison>]
    type TypeVariableName =
        private
        | TypeVariableName of ReferenceName
        with

        override this.Equals(other) =
            match other with
            | :? TypeVariableName as other ->
                let (TypeVariableName thisName) = this
                let (TypeVariableName otherName) = other
                thisName = otherName
            | _ ->
                false

        override this.GetHashCode() =
            let (TypeVariableName name) = this
            hash name

        interface IComparable with
            member this.CompareTo(other) =
                match other with
                | :? TypeVariableName as other ->
                    let (TypeVariableName thisName) = this
                    let (TypeVariableName otherName) = other
                    compare thisName otherName
                | _ ->
                    invalidArg (nameof other) "Cannot compare type variable names of different types."

    module TypeVariableName =
        let create text = text |> ReferenceName.create |> TypeVariableName

        let ofReferenceName name = TypeVariableName name

        let fromToken token = token |> ReferenceName.fromToken |> TypeVariableName

        let createSynthetic introductionKind text =
            text |> ReferenceName.createSynthetic introductionKind |> TypeVariableName

        let text (TypeVariableName name) = name |> ReferenceName.text

        let provenance (TypeVariableName name) = name |> ReferenceName.provenance

    [<CustomEquality; CustomComparison>]
    type FieldName =
        private
        | FieldName of ReferenceName
        with

        override this.Equals(other) =
            match other with
            | :? FieldName as other ->
                let (FieldName thisName) = this
                let (FieldName otherName) = other
                thisName = otherName
            | _ ->
                false

        override this.GetHashCode() =
            let (FieldName name) = this
            hash name

        interface IComparable with
            member this.CompareTo(other) =
                match other with
                | :? FieldName as other ->
                    let (FieldName thisName) = this
                    let (FieldName otherName) = other
                    compare thisName otherName
                | _ ->
                    invalidArg (nameof other) "Cannot compare field names of different types."

    module FieldName =
        let create text = text |> ReferenceName.create |> FieldName

        let ofReferenceName name = FieldName name

        let fromToken token = token |> ReferenceName.fromToken |> FieldName

        let createSynthetic introductionKind text =
            text |> ReferenceName.createSynthetic introductionKind |> FieldName

        let text (FieldName name) = name |> ReferenceName.text

        let provenance (FieldName name) = name |> ReferenceName.provenance

    [<CustomEquality; CustomComparison>]
    type CaptureName =
        private
        | CaptureName of ReferenceName
        with

        override this.Equals(other) =
            match other with
            | :? CaptureName as other ->
                let (CaptureName thisName) = this
                let (CaptureName otherName) = other
                thisName = otherName
            | _ ->
                false

        override this.GetHashCode() =
            let (CaptureName name) = this
            hash name

        interface IComparable with
            member this.CompareTo(other) =
                match other with
                | :? CaptureName as other ->
                    let (CaptureName thisName) = this
                    let (CaptureName otherName) = other
                    compare thisName otherName
                | _ ->
                    invalidArg (nameof other) "Cannot compare capture names of different types."

    module CaptureName =
        let create text = text |> ReferenceName.create |> CaptureName

        let ofReferenceName name = CaptureName name

        let fromToken token = token |> ReferenceName.fromToken |> CaptureName

        let createSynthetic introductionKind text =
            text |> ReferenceName.createSynthetic introductionKind |> CaptureName

        let text (CaptureName name) = name |> ReferenceName.text

        let provenance (CaptureName name) = name |> ReferenceName.provenance

    [<StructuralEquality; StructuralComparison>]
    type QualifiedReference =
        private
            { QualifierSegments: ReferenceName list
              LocalName: ReferenceName }

    module QualifiedReference =
        let create qualifier localName =
            { QualifierSegments = qualifier
              LocalName = localName }

        let ofSegments segments =
            match segments with
            | [] ->
                invalidArg (nameof segments) "Qualified references must contain at least one segment."
            | _ ->
                let qualifier = segments |> List.take (max 0 (List.length segments - 1)) |> List.map ReferenceName.create
                let localName = segments |> List.last |> ReferenceName.create
                create qualifier localName

        let ofNames segments =
            match segments with
            | [] ->
                invalidArg (nameof segments) "Qualified references must contain at least one segment."
            | _ ->
                create (segments |> List.take (max 0 (List.length segments - 1))) (segments |> List.last)

        let qualifierSegments qualifiedReference = qualifiedReference.QualifierSegments

        let qualifierModuleIdentity qualifiedReference =
            match qualifierSegments qualifiedReference |> List.map ReferenceName.text with
            | [] -> None
            | segments -> Some(ModuleIdentity.ofSegments segments)

        let localName qualifiedReference = qualifiedReference.LocalName

        let segments qualifiedReference =
            (qualifierSegments qualifiedReference |> List.map ReferenceName.text)
            @ [ localName qualifiedReference |> ReferenceName.text ]

        let text qualifiedReference =
            qualifiedReference |> segments |> SyntaxFacts.moduleNameToText

    type IntrinsicClassifier =
        | UniverseClassifier
        | QuantityClassifier
        | RegionClassifier
        | ConstraintClassifier
        | RecRowClassifier
        | VarRowClassifier
        | EffRowClassifier
        | LabelClassifier
        | EffLabelClassifier

    type TypeExpr =
        | TypeName of TypeReference * TypeExpr list
        | TypeVariable of TypeVariableName
        | TypeLevelLiteral of int
        | TypeUniverse of TypeExpr option
        | TypeIntrinsic of IntrinsicClassifier
        | TypeApply of TypeExpr * TypeExpr list
        | TypeLambda of TypeVariableName * TypeExpr * TypeExpr
        | TypeDelay of TypeExpr
        | TypeMemo of TypeExpr
        | TypeForce of TypeExpr
        | TypeProject of TypeExpr * FieldName
        | TypeArrow of Quantity * TypeExpr * TypeExpr
        | TypeEquality of TypeExpr * TypeExpr
        | TypeCapture of TypeExpr * CaptureName list
        | TypeEffectRow of EffectRowEntry list * TypeExpr option
        | TypeRecord of RecordField list
        | TypeUnion of TypeExpr list

    and EffectRowEntry =
        { Label: TypeExpr
          Effect: TypeExpr }

    and RecordField =
        { Name: FieldName
          Quantity: Quantity
          Type: TypeExpr }

    and TypeReference = private TypeReference of QualifiedReference * TypeIdentity option

    module TypeReference =
        let create qualifiedReference identity =
            TypeReference(qualifiedReference, identity)

        let ofSegments segments = create (QualifiedReference.ofSegments segments) None

        let ofQualifiedReference qualifiedReference = create qualifiedReference None

        let qualifiedReference (TypeReference(qualifiedReference, _)) = qualifiedReference

        let identity (TypeReference(_, identity)) = identity

        let attachIdentity typeIdentity typeReference =
            create (qualifiedReference typeReference) (Some typeIdentity)

        let segments typeReference =
            typeReference |> qualifiedReference |> QualifiedReference.segments

        let text typeReference =
            typeReference |> qualifiedReference |> QualifiedReference.text

        let localName typeReference =
            typeReference |> qualifiedReference |> QualifiedReference.localName

        let canonicalText typeReference =
            typeReference
            |> identity
            |> Option.map (fun typeIdentity ->
                ((TypeIdentity.moduleIdentity typeIdentity |> ModuleIdentity.segments)
                 @ TypeIdentity.scopePath typeIdentity
                 @ [ TypeIdentity.name typeIdentity ])
                |> String.concat ".")
            |> Option.defaultValue (text typeReference)

        let matches left right =
            match identity left, identity right with
            | Some leftIdentity, Some rightIdentity -> leftIdentity = rightIdentity
            | _ -> qualifiedReference left = qualifiedReference right

    type TraitReference = private TraitReference of QualifiedReference * DeclarationIdentity option

    module TraitReference =
        let create qualifiedReference identity =
            TraitReference(qualifiedReference, identity)

        let ofSegments segments = create (QualifiedReference.ofSegments segments) None

        let ofQualifiedReference qualifiedReference = create qualifiedReference None

        let unqualified name = ofSegments [ name ]

        let qualifiedReference (TraitReference(qualifiedReference, _)) = qualifiedReference

        let segments traitReference = traitReference |> qualifiedReference |> QualifiedReference.segments

        let identity (TraitReference(_, identity)) = identity

        let attachIdentity declarationIdentity traitReference =
            create (qualifiedReference traitReference) (Some declarationIdentity)

        let text traitReference =
            traitReference |> qualifiedReference |> QualifiedReference.text

        let localName traitReference =
            traitReference |> qualifiedReference |> QualifiedReference.localName

        let canonicalName traitReference =
            traitReference
            |> identity
            |> Option.map DeclarationIdentity.canonicalText
            |> Option.defaultValue (text traitReference)

        let matches left right =
            match identity left, identity right with
            | Some leftIdentity, Some rightIdentity ->
                leftIdentity = rightIdentity
            | _ ->
                qualifiedReference left = qualifiedReference right

    type TraitConstraint =
        { Trait: TraitReference
          Arguments: TypeExpr list }

    type ForallBinder =
        { Name: TypeVariableName
          Quantity: Quantity
          Sort: TypeExpr }

    type TypeScheme =
        { Forall: ForallBinder list
          Constraints: TraitConstraint list
          Body: TypeExpr }

    type TypeDefinition =
        { ParameterNames: TypeVariableName list
          DefinitionBody: TypeExpr
          Transparent: bool
          TerminationCertified: bool
          ConversionReducible: bool
          CertificationSource: DefinitionCertificationSource option }

    and DefinitionCertificationSource =
        | CheckedDefinition
        | VerifiedAssertTerminates
        | AssertTerminatesUnverified
        | AssertReducibleUnverified

    type DefinitionContext = Map<QualifiedReference, TypeDefinition>

    let emptyDefinitionContext : DefinitionContext = Map.empty

    let knownType known arguments =
        TypeName(TypeReference.ofSegments (CompilerKnownSymbols.KnownTypes.canonicalPath known), arguments)

    let private unitTypeExpr =
        knownType CompilerKnownSymbols.UnitType []

    let private optionTypeExpr inner =
        knownType CompilerKnownSymbols.OptionType [ inner ]

    let private makeTypeVariable text = text |> TypeVariableName.create |> TypeVariable
    let private makeTypeVariableFromToken token = token |> TypeVariableName.fromToken |> TypeVariable
    let private makeFieldName text = text |> FieldName.create
    let private makeFieldNameFromToken token = token |> FieldName.fromToken
    let private makeCaptureName text = text |> CaptureName.create
    let private makeCaptureNameFromToken token = token |> CaptureName.fromToken
    let private makeTypeReference segments = segments |> TypeReference.ofSegments
    let private makeTypeReferenceFromQualifiedReference qualifiedReference =
        qualifiedReference |> TypeReference.ofQualifiedReference

    let matchesKnownTypeName known typeReference =
        CompilerKnownSymbols.KnownTypes.matchesName known (TypeReference.segments typeReference)

    let tryKnownTypeArguments known typeExpr =
        match typeExpr with
        | TypeName(typeReference, arguments) when matchesKnownTypeName known typeReference ->
            Some arguments
        | _ ->
            None

    let private intrinsicClassifierName classifier =
        match classifier with
        | UniverseClassifier -> CompilerKnownSymbols.KnownTypeNames.Universe
        | QuantityClassifier -> CompilerKnownSymbols.KnownTypeNames.Quantity
        | RegionClassifier -> CompilerKnownSymbols.KnownTypeNames.Region
        | ConstraintClassifier -> CompilerKnownSymbols.KnownTypeNames.Constraint
        | RecRowClassifier -> CompilerKnownSymbols.KnownTypeNames.RecRow
        | VarRowClassifier -> CompilerKnownSymbols.KnownTypeNames.VarRow
        | EffRowClassifier -> CompilerKnownSymbols.KnownTypeNames.EffRow
        | LabelClassifier -> CompilerKnownSymbols.KnownTypeNames.Label
        | EffLabelClassifier -> CompilerKnownSymbols.KnownTypeNames.EffLabel

    let private tryIntrinsicClassifier typeReference =
        match typeReference |> TypeReference.segments |> CompilerKnownSymbols.KnownTypes.tryClassifyName with
        | Some CompilerKnownSymbols.UniverseType -> Some UniverseClassifier
        | Some CompilerKnownSymbols.QuantityType -> Some QuantityClassifier
        | Some CompilerKnownSymbols.RegionType -> Some RegionClassifier
        | Some CompilerKnownSymbols.RecRowType -> Some RecRowClassifier
        | Some CompilerKnownSymbols.VarRowType -> Some VarRowClassifier
        | Some CompilerKnownSymbols.EffRowType -> Some EffRowClassifier
        | Some CompilerKnownSymbols.LabelType -> Some LabelClassifier
        | Some CompilerKnownSymbols.EffLabelType -> Some EffLabelClassifier
        | _ -> None

    let addDefinition name definition (context: DefinitionContext) =
        context |> Map.add name definition

    let addTransparentDefinition name parameters body context =
        addDefinition
            name
            { ParameterNames = parameters
              DefinitionBody = body
              Transparent = true
              TerminationCertified = true
              ConversionReducible = true
              CertificationSource = Some CheckedDefinition }
            context

    let addAssertTerminatesDefinition name parameters body context =
        addDefinition
            name
            { ParameterNames = parameters
              DefinitionBody = body
              Transparent = true
              TerminationCertified = false
              ConversionReducible = false
              CertificationSource = Some AssertTerminatesUnverified }
            context

    let addAssertReducibleDefinition name parameters body context =
        addDefinition
            name
            { ParameterNames = parameters
              DefinitionBody = body
              Transparent = true
              TerminationCertified = true
              ConversionReducible = true
              CertificationSource = Some AssertReducibleUnverified }
            context

    let private builtinDefinitionContext = emptyDefinitionContext

    type private Parser(tokens: Token list) =
        let tokenArray =
            tokens
            |> List.filter (fun token ->
                match token.Kind with
                | Newline
                | Indent
                | Dedent
                | EndOfFile -> false
                | _ -> true)
            |> List.toArray

        let mutable position = 0

        member private _.Current =
            if position < tokenArray.Length then
                Some tokenArray[position]
            else
                None

        member private _.Peek(offset: int) =
            let index = position + offset

            if index < tokenArray.Length then
                Some tokenArray[index]
            else
                None

        member private _.Advance() =
            let current =
                if position < tokenArray.Length then
                    Some tokenArray[position]
                else
                    None

            if position < tokenArray.Length then
                position <- position + 1

            current

        member _.IsAtEnd = position >= tokenArray.Length

        member private this.IsConstraintArrowAt(index: int, depth: int) =
            if depth <> 0 || index >= tokenArray.Length then
                false
            else
                match tokenArray[index].Kind with
                | Operator when String.Equals(tokenArray[index].Text, "=>", StringComparison.Ordinal) ->
                    true
                | Equals when index + 1 < tokenArray.Length ->
                    match tokenArray[index + 1] with
                    | { Kind = Operator; Text = ">" } -> true
                    | _ -> false
                | _ ->
                    false

        member private this.MatchConstraintArrow() =
            match this.Current with
            | Some { Kind = Operator; Text = "=>" } ->
                this.Advance() |> ignore
                true
            | Some { Kind = Equals } ->
                match this.Peek(1) with
                | Some { Kind = Operator; Text = ">" } ->
                    this.Advance() |> ignore
                    this.Advance() |> ignore
                    true
                | _ ->
                    false
            | _ ->
                false

        member private this.MatchKind(kind: TokenKind) =
            match this.Current with
            | Some token when token.Kind = kind ->
                this.Advance() |> ignore
                true
            | _ ->
                false

        member private this.MatchOperator(text: string) =
            match this.Current with
            | Some { Kind = Operator; Text = currentText } when String.Equals(currentText, text, StringComparison.Ordinal) ->
                this.Advance() |> ignore
                true
            | _ ->
                false

        member private _.FindMatchingIndex(leftKind: TokenKind, rightKind: TokenKind, startIndex: int) =
            let mutable depth = 0
            let mutable index = startIndex
            let mutable closingIndex = None

            while index < tokenArray.Length && closingIndex.IsNone do
                match tokenArray[index].Kind with
                | kind when kind = leftKind ->
                    depth <- depth + 1
                | kind when kind = rightKind ->
                    depth <- depth - 1

                    if depth = 0 then
                        closingIndex <- Some index
                | _ ->
                    ()

                index <- index + 1

            closingIndex

        member private _.TryParseQuantityPrefix(tokens: Token list) =
            let isQuantityVariableToken (token: Token) =
                if token.Kind <> Identifier then
                    false
                else
                    let text = SyntaxFacts.trimIdentifierQuotes token.Text
                    not (String.IsNullOrEmpty text) && not (Char.IsUpper text[0])

            match tokens with
            | { Kind = IntegerLiteral; Text = "0" } :: rest ->
                Some(QuantityZero, rest)
            | { Kind = IntegerLiteral; Text = "1" } :: rest ->
                Some(QuantityOne, rest)
            | { Kind = Operator; Text = "&" } :: { Kind = LeftBracket } :: regionToken :: { Kind = RightBracket } :: rest
                when Token.isName regionToken ->
                Some(QuantityBorrow(Some(SyntaxFacts.trimIdentifierQuotes regionToken.Text)), rest)
            | { Kind = Operator; Text = "&" } :: rest ->
                Some(QuantityBorrow None, rest)
            | { Kind = Operator; Text = "<=" } :: { Kind = IntegerLiteral; Text = "1" } :: rest ->
                Some(QuantityAtMostOne, rest)
            | { Kind = Operator; Text = ">=" } :: { Kind = IntegerLiteral; Text = "1" } :: rest ->
                Some(QuantityAtLeastOne, rest)
            | head :: rest when Token.isName head && String.Equals(head.Text, "\u03c9", StringComparison.Ordinal) ->
                Some(QuantityOmega, rest)
            | head :: rest when Token.isName head && String.Equals(head.Text, "omega", StringComparison.Ordinal) ->
                Some(QuantityOmega, rest)
            | head :: rest when isQuantityVariableToken head ->
                match rest with
                | next :: _ when Token.isName next ->
                    Some(QuantityVariable(SyntaxFacts.trimIdentifierQuotes head.Text), rest)
                | _ ->
                    None
            | _ ->
                None

        member private _.FindTopLevelColon(tokens: Token list) =
            let rec loop depth index =
                if index >= tokens.Length then
                    None
                else
                    match tokens[index].Kind with
                    | LeftParen
                    | LeftBracket
                    | LeftEffectRow
                    | LeftBrace
                    | LeftSetBrace ->
                        loop (depth + 1) (index + 1)
                    | RightParen
                    | RightBracket
                    | RightEffectRow
                    | RightBrace
                    | RightSetBrace ->
                        loop (max 0 (depth - 1)) (index + 1)
                    | Colon when depth = 0 ->
                        Some index
                    | _ ->
                        loop depth (index + 1)

            loop 0 0

        member private _.FindTopLevelEquals(tokens: Token list) =
            let rec loop depth index =
                if index >= tokens.Length then
                    None
                else
                    match tokens[index].Kind with
                    | LeftParen
                    | LeftBracket
                    | LeftEffectRow
                    | LeftBrace
                    | LeftSetBrace ->
                        loop (depth + 1) (index + 1)
                    | RightParen
                    | RightBracket
                    | RightEffectRow
                    | RightBrace
                    | RightSetBrace ->
                        loop (max 0 (depth - 1)) (index + 1)
                    | Equals when depth = 0 ->
                        Some index
                    | _ ->
                        loop depth (index + 1)

            loop 0 0

        member private this.TryParseNamedBinder(tokens: Token list, defaultQuantity: Quantity) =
            match this.FindTopLevelColon(tokens) with
            | Some colonIndex when colonIndex > 0 && colonIndex + 1 < tokens.Length ->
                let binderTokens = tokens |> List.take colonIndex
                let typeTokens = tokens |> List.skip (colonIndex + 1)

                let binderTokens =
                    match binderTokens with
                    | { Kind = AtSign } :: rest -> rest
                    | _ -> binderTokens

                let isInout, binderTokens =
                    match binderTokens with
                    | head :: rest when Token.isKeyword Keyword.Inout head -> true, rest
                    | _ -> false, binderTokens

                let quantity, afterQuantity =
                    if isInout then
                        QuantityOne, binderTokens
                    else
                        match this.TryParseQuantityPrefix binderTokens with
                        | Some(quantity, rest) ->
                            quantity, rest
                        | None ->
                            defaultQuantity, binderTokens

                let typeTokens, nameTokens =
                    match afterQuantity with
                    | head :: rest when Token.isKeyword Keyword.Thunk head ->
                        { head with Kind = Identifier; Text = CompilerKnownSymbols.KnownTypeNames.Thunk } :: typeTokens, rest
                    | head :: rest when Token.isKeyword Keyword.Lazy head ->
                        { head with Kind = Identifier; Text = CompilerKnownSymbols.KnownTypeNames.Need } :: typeTokens, rest
                    | _ ->
                        typeTokens, afterQuantity

                match nameTokens with
                | [ nameToken ] when Token.isName nameToken || nameToken.Kind = Underscore ->
                    Some(quantity, ReferenceName.fromToken nameToken, typeTokens)
                | _ ->
                    None
            | _ ->
                None

        member private _.SplitTopLevelCommaGroups(tokens: Token list) =
            let groups = ResizeArray<Token list>()
            let current = ResizeArray<Token>()
            let mutable depth = 0
            let mutable sawTopLevelComma = false

            for token in tokens do
                match token.Kind with
                | LeftParen
                | LeftBracket
                | LeftEffectRow
                | LeftBrace
                | LeftSetBrace ->
                    depth <- depth + 1
                    current.Add(token)
                | RightParen
                | RightBracket
                | RightEffectRow
                | RightBrace
                | RightSetBrace ->
                    depth <- max 0 (depth - 1)
                    current.Add(token)
                | Comma when depth = 0 ->
                    sawTopLevelComma <- true
                    groups.Add(List.ofSeq current)
                    current.Clear()
                | _ ->
                    current.Add(token)

            groups.Add(List.ofSeq current)
            List.ofSeq groups, sawTopLevelComma

        member this.ParseCompleteType() =
            match this.ParseType() with
            | Some parsed when this.IsAtEnd ->
                Some parsed
            | _ ->
                None

        member private this.TryParseBinderArrow() =
            match this.Current with
            | Some { Kind = LeftParen } ->
                match this.FindMatchingIndex(LeftParen, RightParen, position) with
                | Some rightParenIndex when rightParenIndex + 1 < tokenArray.Length && tokenArray[rightParenIndex + 1].Kind = Arrow ->
                    let innerTokens =
                        tokenArray[position + 1 .. rightParenIndex - 1]
                        |> Array.toList

                    match this.TryParseNamedBinder(innerTokens, QuantityOmega) with
                    | Some(quantity, _, typeTokens) ->
                        let nestedParser = Parser(typeTokens)

                        match nestedParser.ParseCompleteType() with
                        | Some parameterType ->
                            position <- rightParenIndex + 2

                            this.ParseType()
                            |> Option.map (fun resultType -> TypeArrow(quantity, parameterType, resultType))
                        | None ->
                            None
                    | None ->
                        None
                | _ ->
                    None
            | _ ->
                None

        member private this.TryParseRecordField(tokens: Token list) =
            match this.TryParseNamedBinder(tokens, QuantityOmega) with
            | Some(quantity, name, typeTokens) ->
                let nestedParser = Parser(typeTokens)

                nestedParser.ParseCompleteType()
                |> Option.map (fun fieldType ->
                    { Name = name |> FieldName.ofReferenceName
                      Quantity = quantity
                      Type = fieldType })
            | None ->
                None

        member private this.TryParseRecordValueField(tokens: Token list) =
            match this.FindTopLevelEquals(tokens) with
            | Some equalsIndex when equalsIndex > 0 && equalsIndex + 1 < tokens.Length ->
                let labelTokens = tokens |> List.take equalsIndex
                let valueTokens = tokens |> List.skip (equalsIndex + 1)

                match labelTokens with
                | [ nameToken ] when Token.isName nameToken ->
                    let nestedParser = Parser(valueTokens)

                    nestedParser.ParseCompleteType()
                    |> Option.map (fun fieldValue ->
                        { Name = nameToken |> makeFieldNameFromToken
                          Quantity = QuantityOmega
                          Type = fieldValue })
                | _ ->
                    None
            | _ ->
                None

        member private this.TryParseRecordType(innerTokens: Token list) =
            let groups, sawTopLevelComma = this.SplitTopLevelCommaGroups(innerTokens)
            let nonEmptyGroups =
                groups
                |> List.filter (List.isEmpty >> not)

            let binderFields =
                nonEmptyGroups
                |> List.map this.TryParseRecordField

            let valueFields =
                nonEmptyGroups
                |> List.map this.TryParseRecordValueField

            let canBeRecordValue =
                sawTopLevelComma || List.length nonEmptyGroups > 1

            if (sawTopLevelComma || List.length nonEmptyGroups = 1) && binderFields |> List.forall Option.isSome then
                binderFields
                |> List.choose id
                |> function
                    | [] -> None
                    | parsedFields -> Some(TypeRecord parsedFields)
            elif canBeRecordValue && valueFields |> List.forall Option.isSome then
                valueFields
                |> List.choose id
                |> function
                    | [] -> None
                    | parsedFields -> Some(TypeRecord parsedFields)
            elif sawTopLevelComma then
                let elementTypes =
                    nonEmptyGroups
                    |> List.map (fun group ->
                        let nestedParser = Parser(group)
                        nestedParser.ParseCompleteType())

                if elementTypes |> List.forall Option.isSome then
                    elementTypes
                    |> List.choose id
                    |> List.mapi (fun index elementType ->
                        { Name = makeFieldName $"_{index + 1}"
                          Quantity = QuantityOmega
                          Type = elementType })
                    |> TypeRecord
                    |> Some
                else
                    None
            else
                None

        member private this.SplitTopLevelPipeGroups(tokens: Token list) =
            let groups = ResizeArray<Token list>()
            let current = ResizeArray<Token>()
            let mutable depth = 0

            for token in tokens do
                match token.Kind with
                | LeftParen
                | LeftBracket
                | LeftEffectRow
                | LeftBrace
                | LeftSetBrace ->
                    depth <- depth + 1
                    current.Add(token)
                | RightParen
                | RightBracket
                | RightEffectRow
                | RightBrace
                | RightSetBrace ->
                    depth <- max 0 (depth - 1)
                    current.Add(token)
                | Operator when depth = 0 && String.Equals(token.Text, "|", StringComparison.Ordinal) ->
                    groups.Add(List.ofSeq current)
                    current.Clear()
                | _ ->
                    current.Add(token)

            groups.Add(List.ofSeq current)
            List.ofSeq groups

        member private this.TryParseUnionType(innerTokens: Token list) =
            match innerTokens with
            | first :: rest when first.Kind = Operator && String.Equals(first.Text, "|", StringComparison.Ordinal) ->
                match List.rev rest with
                | last :: reversedMembers when last.Kind = Operator && String.Equals(last.Text, "|", StringComparison.Ordinal) ->
                    let memberTokens =
                        reversedMembers
                        |> List.rev
                        |> this.SplitTopLevelPipeGroups
                        |> List.map (fun group ->
                            group
                            |> List.filter (fun token ->
                                match token.Kind with
                                | Newline
                                | Indent
                                | Dedent
                                | EndOfFile -> false
                                | _ -> true))
                        |> List.filter (List.isEmpty >> not)

                    let members =
                        memberTokens
                        |> List.map (fun tokens ->
                            let nested = Parser(tokens)
                            nested.ParseCompleteType())

                    if not (List.isEmpty members) && members |> List.forall Option.isSome then
                        members |> List.choose id |> TypeUnion |> Some
                    else
                        None
                | _ ->
                    None
            | _ ->
                None

        member private _.FindTopLevelEffectRowBar(tokens: Token list) =
            let rec loop depth index =
                if index >= tokens.Length then
                    None
                else
                    match tokens[index].Kind with
                    | LeftParen
                    | LeftBracket
                    | LeftEffectRow
                    | LeftBrace
                    | LeftSetBrace ->
                        loop (depth + 1) (index + 1)
                    | RightParen
                    | RightBracket
                    | RightEffectRow
                    | RightBrace
                    | RightSetBrace ->
                        loop (max 0 (depth - 1)) (index + 1)
                    | Operator when depth = 0 && String.Equals(tokens[index].Text, "|", StringComparison.Ordinal) ->
                        Some index
                    | _ ->
                        loop depth (index + 1)

            loop 0 0

        member private this.TryParseEffectRowEntry(tokens: Token list) =
            match this.FindTopLevelColon(tokens) with
            | Some colonIndex when colonIndex > 0 && colonIndex + 1 < tokens.Length ->
                let labelTokens = tokens |> List.take colonIndex
                let effectTokens = tokens |> List.skip (colonIndex + 1)
                let labelParser = Parser(labelTokens)
                let effectParser = Parser(effectTokens)

                match labelParser.ParseCompleteType(), effectParser.ParseCompleteType() with
                | Some label, Some effectType ->
                    Some { Label = label; Effect = effectType }
                | _ ->
                    None
            | _ ->
                None

        member private this.TryParseEffectRowType(innerTokens: Token list) =
            let innerTokens =
                innerTokens
                |> List.filter (fun token -> token.Kind <> EndOfFile)

            let entryTokens, tailTokens =
                match this.FindTopLevelEffectRowBar(innerTokens) with
                | Some barIndex ->
                    innerTokens |> List.take barIndex,
                    innerTokens |> List.skip (barIndex + 1) |> Some
                | None ->
                    innerTokens, None

            let entryGroups, _ = this.SplitTopLevelCommaGroups(entryTokens)

            let entries =
                entryGroups
                |> List.filter (List.isEmpty >> not)
                |> List.map this.TryParseEffectRowEntry

            let parsedTail =
                match tailTokens with
                | None -> Some None
                | Some [] -> None
                | Some tokens ->
                    let tailParser = Parser(tokens)
                    tailParser.ParseCompleteType() |> Option.map Some

            if entries |> List.forall Option.isSome then
                match parsedTail with
                | Some tail ->
                    Some(TypeEffectRow(entries |> List.choose id, tail))
                | None ->
                    None
            else
                None

        member private this.ParseQualifiedName() =
            let parseOperatorName () =
                let isSymbolToken token =
                    match token.Kind with
                    | Operator
                    | Colon
                    | Equals -> true
                    | _ -> false

                match this.Current with
                | Some { Kind = LeftParen } ->
                    let mutable offset = 1
                    let mutable parts = ResizeArray<Token>()
                    let mutable keepReading = true
                    let mutable sawRightParen = false

                    while keepReading do
                        match this.Peek(offset) with
                        | Some { Kind = RightParen } when parts.Count > 0 ->
                            sawRightParen <- true
                            keepReading <- false
                        | Some token when isSymbolToken token ->
                            parts.Add(token)
                            offset <- offset + 1
                        | _ ->
                            keepReading <- false

                    if sawRightParen then
                        let openingParen = this.Advance().Value

                        for _ in 1 .. parts.Count do
                            this.Advance() |> ignore

                        let closingParen = this.Advance().Value
                        let operatorText = parts |> Seq.map (fun token -> token.Text) |> String.Concat

                        ReferenceName.createWithProvenance
                            (Some
                                { OriginKind = NameOriginKind.SourceSpelling
                                  OriginSpan = Some(TextSpan.FromBounds(openingParen.Span.Start, closingParen.Span.End)) })
                            operatorText
                        |> List.singleton
                        |> QualifiedReference.ofNames
                        |> Some
                    else
                        None
                | _ ->
                    None

            match parseOperatorName () with
            | Some name -> Some name
            | None ->
                match this.Current with
                | Some token when Token.isName token ->
                    let segments = ResizeArray<ReferenceName>()
                    segments.Add(ReferenceName.fromToken token)
                    this.Advance() |> ignore

                    let mutable keepReading = true

                    while keepReading do
                        match this.Current, this.Peek(1) with
                        | Some { Kind = Dot }, Some nextToken when Token.isName nextToken ->
                            this.Advance() |> ignore
                            segments.Add(ReferenceName.fromToken nextToken)
                            this.Advance() |> ignore
                        | _ ->
                            keepReading <- false

                    segments |> List.ofSeq |> QualifiedReference.ofNames |> Some
                | _ ->
                    None

        member private this.TryParseQuantityAtom() =
            let quantityExpr quantity =
                TypeName(makeTypeReference [ Quantity.toSurfaceText quantity ], [])

            match this.Current with
            | Some { Kind = IntegerLiteral; Text = "0" } ->
                this.Advance() |> ignore
                Some(quantityExpr QuantityZero)
            | Some { Kind = IntegerLiteral; Text = "1" } ->
                this.Advance() |> ignore
                Some(quantityExpr QuantityOne)
            | Some token when Token.isName token && String.Equals(SyntaxFacts.trimIdentifierQuotes token.Text, "\u03c9", StringComparison.Ordinal) ->
                this.Advance() |> ignore
                Some(quantityExpr QuantityOmega)
            | Some token when Token.isName token && String.Equals(SyntaxFacts.trimIdentifierQuotes token.Text, "omega", StringComparison.Ordinal) ->
                this.Advance() |> ignore
                Some(quantityExpr QuantityOmega)
            | Some { Kind = Operator; Text = "<=" } ->
                match this.Peek(1) with
                | Some { Kind = IntegerLiteral; Text = "1" } ->
                    this.Advance() |> ignore
                    this.Advance() |> ignore
                    Some(quantityExpr QuantityAtMostOne)
                | _ ->
                    None
            | Some { Kind = Operator; Text = ">=" } ->
                match this.Peek(1) with
                | Some { Kind = IntegerLiteral; Text = "1" } ->
                    this.Advance() |> ignore
                    this.Advance() |> ignore
                    Some(quantityExpr QuantityAtLeastOne)
                | _ ->
                    None
            | Some { Kind = Operator; Text = "&" } ->
                match this.Peek(1), this.Peek(2), this.Peek(3) with
                | Some { Kind = LeftBracket }, Some regionToken, Some { Kind = RightBracket } when Token.isName regionToken ->
                    this.Advance() |> ignore
                    this.Advance() |> ignore
                    let regionName = SyntaxFacts.trimIdentifierQuotes regionToken.Text
                    this.Advance() |> ignore
                    this.Advance() |> ignore
                    Some(quantityExpr (QuantityBorrow(Some regionName)))
                | _ ->
                    this.Advance() |> ignore
                    Some(quantityExpr (QuantityBorrow None))
            | _ ->
                None

        member private this.ParseAtom() =
            match this.Current with
            | Some { Kind = LeftParen } ->
                match this.FindMatchingIndex(LeftParen, RightParen, position) with
                | Some rightParenIndex ->
                    let innerTokens =
                        tokenArray[position + 1 .. rightParenIndex - 1]
                        |> Array.toList

                    match this.TryParseRecordType innerTokens with
                    | Some recordType ->
                        position <- rightParenIndex + 1
                        Some recordType
                    | None ->
                        match this.TryParseUnionType innerTokens with
                        | Some unionType ->
                            position <- rightParenIndex + 1
                            Some unionType
                        | None ->
                            None
                | None ->
                    None
            | Some { Kind = LeftEffectRow } ->
                match this.FindMatchingIndex(LeftEffectRow, RightEffectRow, position) with
                | Some rightRowIndex ->
                    let innerTokens =
                        tokenArray[position + 1 .. rightRowIndex - 1]
                        |> Array.toList

                    match this.TryParseEffectRowType innerTokens with
                    | Some rowType ->
                        position <- rightRowIndex + 1
                        Some rowType
                    | None ->
                        None
                | None ->
                    None
            | _ ->
                None

        member private this.ParseGroupedOrAtomicType() =
            match this.TryParseQuantityAtom() with
            | Some quantityType ->
                Some quantityType
            | None ->
            match this.Current with
            | Some { Kind = Operator; Text = "*" } ->
                this.Advance() |> ignore
                Some(TypeUniverse None)
            | Some { Kind = LeftParen } ->
                this.Advance() |> ignore

                if this.MatchKind RightParen then
                    Some unitTypeExpr
                else
                    match this.ParseType() with
                    | Some inner when this.MatchKind RightParen -> Some inner
                    | _ -> None
            | _ ->
                this.ParseQualifiedName()
                |> Option.map (fun name ->
                    let nameSegments = name |> QualifiedReference.segments
                    let terminalSegment = name |> QualifiedReference.localName |> ReferenceName.text
                    let collapsedName = name |> QualifiedReference.text
                    let projectionChain =
                        match nameSegments with
                        | head :: tail when not (List.isEmpty tail) ->
                            let terminalIsLowercase =
                                terminalSegment.Length > 0 && Char.IsLower(terminalSegment[0])

                            if String.Equals(head, "this", StringComparison.Ordinal) || terminalIsLowercase then
                                tail
                                |> List.fold (fun current fieldName -> TypeProject(current, fieldName |> makeFieldName)) (makeTypeVariable head)
                                |> Some
                            else
                                None
                        | _ ->
                            None

                    match projectionChain with
                    | Some projected ->
                        projected
                    | None ->
                        let parsedTypeReference = makeTypeReferenceFromQualifiedReference name

                        match tryIntrinsicClassifier parsedTypeReference with
                        | Some classifier ->
                            TypeIntrinsic classifier
                        | None when nameSegments = [ "Type" ] ->
                            TypeUniverse None
                        | None when nameSegments.Length = 1 && terminalSegment.StartsWith("Type", StringComparison.Ordinal) ->
                            let suffix = terminalSegment.Substring("Type".Length)

                            match Int32.TryParse suffix with
                            | true, level ->
                                TypeUniverse(Some(TypeLevelLiteral level))
                            | false, _ ->
                                if terminalSegment.Length > 0 && Char.IsLower(terminalSegment[0]) then
                                    makeTypeVariable collapsedName
                                else
                                    TypeName(parsedTypeReference, [])
                        | None ->
                            if terminalSegment.Length > 0 && Char.IsLower(terminalSegment[0]) then
                                makeTypeVariable collapsedName
                            else
                                TypeName(parsedTypeReference, []))

        member private this.CanStartAtom() =
            match this.Current with
            | Some { Kind = LeftParen } -> true
            | Some { Kind = LeftEffectRow } -> true
            | Some { Kind = IntegerLiteral; Text = ("0" | "1") } -> true
            | Some { Kind = Operator; Text = "&" } -> true
            | Some { Kind = Operator; Text = "<=" } ->
                match this.Peek(1) with
                | Some { Kind = IntegerLiteral; Text = "1" } -> true
                | _ -> false
            | Some { Kind = Operator; Text = ">=" } ->
                match this.Peek(1) with
                | Some { Kind = IntegerLiteral; Text = "1" } -> true
                | _ -> false
            | Some token when Token.isName token ->
                not (String.Equals(SyntaxFacts.trimIdentifierQuotes token.Text, "captures", StringComparison.Ordinal))
            | _ -> false

        member private this.ParsePostfix() =
            match this.ParseAtom() |> Option.orElseWith (fun () -> this.ParseGroupedOrAtomicType()) with
            | None -> None
            | Some head ->
                let mutable parsed = head
                let mutable keepParsingProjections = true

                while keepParsingProjections do
                    match this.Current, this.Peek(1) with
                    | Some { Kind = Dot }, Some nextToken when Token.isName nextToken ->
                        this.Advance() |> ignore
                        let memberToken = this.Advance().Value
                        parsed <- TypeProject(parsed, memberToken |> makeFieldNameFromToken)
                    | _ ->
                        keepParsingProjections <- false

                while this.MatchOperator("?") do
                    parsed <- optionTypeExpr parsed

                Some parsed

        member private this.TryParseCaptureSuffix() =
            let isCapturesToken token =
                Token.isName token
                && String.Equals(SyntaxFacts.trimIdentifierQuotes token.Text, "captures", StringComparison.Ordinal)

            match this.Current, this.Peek(1) with
            | Some capturesToken, Some { Kind = LeftParen } when isCapturesToken capturesToken ->
                this.Advance() |> ignore
                this.Advance() |> ignore
                let regions = ResizeArray<CaptureName>()
                let mutable keepReading = true
                let mutable parsed = true

                while keepReading && parsed do
                    match this.Current with
                    | Some token when Token.isName token ->
                        regions.Add(token |> makeCaptureNameFromToken)
                        this.Advance() |> ignore

                        if this.MatchKind Comma then
                            ()
                        elif this.MatchKind RightParen then
                            keepReading <- false
                        else
                            parsed <- false
                    | Some { Kind = RightParen } ->
                        this.Advance() |> ignore
                        keepReading <- false
                    | _ ->
                        parsed <- false

                if parsed then
                    Some(List.ofSeq regions)
                else
                    None
            | _ ->
                None

        member private this.ParseApplication() =
            match this.ParsePostfix() with
            | None -> None
            | Some head ->
                let arguments = ResizeArray<TypeExpr>()
                let mutable keepParsingArguments = true

                while keepParsingArguments && this.CanStartAtom() do
                    match this.ParsePostfix() with
                    | Some argument ->
                        arguments.Add(argument)
                    | None ->
                        keepParsingArguments <- false

                let withCaptures parsed =
                    match this.TryParseCaptureSuffix() with
                    | Some captures -> TypeCapture(parsed, captures)
                    | None -> parsed

                match head with
                | TypeUniverse None when arguments.Count = 1 ->
                    TypeUniverse(Some arguments[0])
                    |> withCaptures
                    |> Some
                | TypeName(name, existingArguments) ->
                    TypeName(name, existingArguments @ List.ofSeq arguments)
                    |> withCaptures
                    |> Some
                | _ when arguments.Count = 0 ->
                    head |> withCaptures |> Some
                | _ ->
                    TypeApply(head, List.ofSeq arguments)
                    |> withCaptures
                    |> Some

        member private _.IsInfixTypeOperatorToken(token: Token) =
            match token.Kind with
            | Operator ->
                not (
                    String.Equals(token.Text, "=>", StringComparison.Ordinal)
                    || String.Equals(token.Text, "|", StringComparison.Ordinal)
                    || String.Equals(token.Text, "?", StringComparison.Ordinal)
                )
            | _ ->
                false

        member private this.ParseInfixTypeApplication() =
            match this.ParseApplication() with
            | None -> None
            | Some head ->
                let mutable parsed = head
                let mutable keepParsing = true
                let mutable valid = true

                while keepParsing && valid do
                    match this.Current with
                    | Some token when this.IsInfixTypeOperatorToken(token) ->
                        let operatorName = token.Text
                        this.Advance() |> ignore

                        match this.ParseApplication() with
                        | Some right ->
                            parsed <- TypeApply(TypeName(makeTypeReference [ operatorName ], []), [ parsed; right ])
                        | None ->
                            valid <- false
                    | _ ->
                        keepParsing <- false

                if valid then Some parsed else None

        member private this.ParseEquality() =
            match this.ParseInfixTypeApplication() with
            | Some left when this.MatchKind Equals ->
                this.ParseEquality()
                |> Option.map (fun right -> TypeEquality(left, right))
            | some ->
                some

        member private this.ParseArrow() =
            match this.TryParseBinderArrow() with
            | Some arrow ->
                Some arrow
            | None ->
                match this.ParseEquality() with
                | None -> None
                | Some left when this.MatchKind Arrow ->
                    this.ParseType()
                    |> Option.map (fun right -> TypeArrow(QuantityOmega, left, right))
                | Some left ->
                    Some left

        member private this.ParseForallType() =
            let parseForallBinder binderTokens =
                match this.TryParseNamedBinder(binderTokens, QuantityZero) with
                | Some(_, binderName, typeTokens) ->
                    let binderParser = Parser(typeTokens)

                    binderParser.ParseCompleteType()
                    |> Option.map (fun binderSort -> binderName |> TypeVariableName.ofReferenceName, binderSort)
                | None ->
                    None

            match this.Current with
            | Some token when Token.isKeyword Keyword.Forall token ->
                this.Advance() |> ignore

                let binders = ResizeArray<TypeVariableName * TypeExpr>()
                let mutable keepReading = true
                let mutable parsed = true

                while keepReading && parsed do
                    match this.Current with
                    | Some token when Token.isName token || token.Kind = Underscore ->
                        binders.Add(token |> TypeVariableName.fromToken, TypeUniverse None)
                        this.Advance() |> ignore
                    | Some { Kind = LeftParen } ->
                        match this.FindMatchingIndex(LeftParen, RightParen, position) with
                        | Some rightParenIndex ->
                            let binderTokens =
                                tokenArray[position + 1 .. rightParenIndex - 1]
                                |> Array.toList

                            position <- rightParenIndex + 1

                            match parseForallBinder binderTokens with
                            | Some binder ->
                                binders.Add(binder)
                            | None ->
                                parsed <- false
                        | None ->
                            parsed <- false
                    | Some { Kind = Dot } ->
                        this.Advance() |> ignore
                        keepReading <- false
                    | _ ->
                        parsed <- false

                if parsed && binders.Count > 0 then
                    this.ParseType()
                    |> Option.map (fun body ->
                        binders
                        |> Seq.rev
                        |> Seq.fold (fun current (binderName, binderSort) -> TypeLambda(binderName, binderSort, current)) body)
                else
                    None
            | _ ->
                None

        member this.ParseType() =
            match this.ParseForallType() with
            | Some parsed ->
                Some parsed
            | None ->
                this.ParseArrow()

        member this.ParseScheme() =
            let parseForallBinder binderTokens =
                match this.TryParseNamedBinder(binderTokens, QuantityZero) with
                | Some(quantity, binderName, typeTokens) ->
                    let binderParser = Parser(typeTokens)

                    binderParser.ParseCompleteType()
                    |> Option.map (fun binderSort ->
                        { Name = binderName |> TypeVariableName.ofReferenceName
                          Quantity = quantity
                          Sort = binderSort })
                | None ->
                    None

            let parseForall () =
                match this.Current with
                | Some token when Token.isKeyword Keyword.Forall token ->
                    this.Advance() |> ignore

                    let binders = ResizeArray<ForallBinder>()
                    let mutable keepReading = true

                    while keepReading do
                        match this.Current with
                        | Some token when Token.isName token || token.Kind = Underscore ->
                            binders.Add(
                                { Name = token |> TypeVariableName.fromToken
                                  Quantity = QuantityZero
                                  Sort = TypeUniverse None }
                            )
                            this.Advance() |> ignore
                        | Some { Kind = LeftParen } ->
                            match this.FindMatchingIndex(LeftParen, RightParen, position) with
                            | Some rightParenIndex ->
                                let binderTokens =
                                    tokenArray[position + 1 .. rightParenIndex - 1]
                                    |> Array.toList

                                position <- rightParenIndex + 1

                                match parseForallBinder binderTokens with
                                | Some binder ->
                                    binders.Add(binder)
                                | None ->
                                    keepReading <- false
                            | None ->
                                keepReading <- false
                        | Some { Kind = Dot } ->
                            this.Advance() |> ignore
                            keepReading <- false
                        | _ ->
                            keepReading <- false

                    List.ofSeq binders
                | _ ->
                    []

            let rec containsConstraintArrow depth index =
                if index >= tokenArray.Length then
                    false
                else
                    match tokenArray[index].Kind with
                    | LeftParen ->
                        containsConstraintArrow (depth + 1) (index + 1)
                    | RightParen ->
                        containsConstraintArrow (max 0 (depth - 1)) (index + 1)
                    | _ when this.IsConstraintArrowAt(index, depth) ->
                        true
                    | _ ->
                        containsConstraintArrow depth (index + 1)

            let parseConstraint constraintTokens =
                let constraintParser = Parser(constraintTokens)

                match constraintParser.ParseType() with
                | Some(TypeName(name, arguments)) ->
                    Some
                        { Trait = TraitReference.ofSegments (TypeReference.segments name)
                          Arguments = arguments }
                | _ ->
                    None

            let tryStripSingleOuterParens (tokens: Token list) =
                match tokens with
                | { Kind = LeftParen } :: _ when not (List.isEmpty tokens) && (List.last tokens).Kind = RightParen ->
                    let tokenArray = tokens |> List.toArray
                    let mutable depth = 0
                    let mutable wrapsWholeList = false
                    let mutable index = 0

                    while index < tokenArray.Length do
                        match tokenArray[index].Kind with
                        | LeftParen ->
                            depth <- depth + 1
                        | RightParen ->
                            depth <- depth - 1
                            if depth = 0 then
                                wrapsWholeList <- index = tokenArray.Length - 1
                        | _ ->
                            ()

                        index <- index + 1

                    if wrapsWholeList then
                        Some(tokens |> List.tail |> List.take (tokens.Length - 2))
                    else
                        None
                | _ ->
                    None

            let splitConstraintGroupTokens constraintTokens =
                let groupTokens =
                    constraintTokens
                    |> tryStripSingleOuterParens
                    |> Option.defaultValue constraintTokens

                let groups = ResizeArray<Token list>()
                let currentGroup = ResizeArray<Token>()
                let mutable parenDepth = 0
                let mutable bracketDepth = 0
                let mutable braceDepth = 0

                for token in groupTokens do
                    match token.Kind with
                    | LeftParen ->
                        parenDepth <- parenDepth + 1
                        currentGroup.Add(token)
                    | RightParen ->
                        parenDepth <- max 0 (parenDepth - 1)
                        currentGroup.Add(token)
                    | LeftBracket
                    | LeftEffectRow
                    | LeftBrace
                    | LeftSetBrace ->
                        bracketDepth <- bracketDepth + 1
                        currentGroup.Add(token)
                    | RightBracket
                    | RightEffectRow
                    | RightBrace
                    | RightSetBrace ->
                        bracketDepth <- max 0 (bracketDepth - 1)
                        currentGroup.Add(token)
                    | Comma when parenDepth = 0 && bracketDepth = 0 && braceDepth = 0 ->
                        if currentGroup.Count > 0 then
                            groups.Add(List.ofSeq currentGroup)
                            currentGroup.Clear()
                    | _ ->
                        currentGroup.Add(token)

                if currentGroup.Count > 0 then
                    groups.Add(List.ofSeq currentGroup)

                groups |> Seq.toList

            let tryParseConstraintGroup constraintTokens =
                let groups = splitConstraintGroupTokens constraintTokens
                let parsedConstraints = groups |> List.map parseConstraint

                if parsedConstraints |> List.forall Option.isSome then
                    Some(parsedConstraints |> List.choose id)
                else
                    None

            let parseConstraints () =
                let rec loop collected =
                    if not (containsConstraintArrow 0 position) then
                        List.rev collected
                    else
                        let constraintTokens = ResizeArray<Token>()
                        let mutable depth = 0
                        let mutable keepReading = true

                        while keepReading && not this.IsAtEnd do
                            match this.Current with
                            | Some { Kind = LeftParen } as current ->
                                depth <- depth + 1
                                constraintTokens.Add(current.Value)
                                this.Advance() |> ignore
                            | Some { Kind = RightParen } as current ->
                                depth <- max 0 (depth - 1)
                                constraintTokens.Add(current.Value)
                                this.Advance() |> ignore
                            | _ when this.MatchConstraintArrow() && depth = 0 ->
                                keepReading <- false
                            | Some token ->
                                constraintTokens.Add(token)
                                this.Advance() |> ignore
                            | None ->
                                keepReading <- false

                        match tryParseConstraintGroup (List.ofSeq constraintTokens) with
                        | Some parsedConstraints ->
                            loop (List.rev parsedConstraints @ collected)
                        | None ->
                            List.rev collected

                loop []

            let forall = parseForall ()
            let constraints = parseConstraints ()

            this.ParseType()
            |> Option.map (fun body ->
                { Forall = forall
                  Constraints = constraints
                  Body = body })

    let parseType tokens =
        let parser = Parser(tokens)
        parser.ParseCompleteType()

    let parseScheme tokens =
        let parser = Parser(tokens)
        match parser.ParseScheme() with
        | Some scheme when parser.IsAtEnd ->
            Some scheme
        | _ ->
            None

    let functionParts (typeExpr: TypeExpr) =
        let rec loop current parameters =
            match current with
            | TypeArrow(_, parameterType, resultType) ->
                loop resultType (parameterType :: parameters)
            | _ ->
                List.rev parameters, current

        loop typeExpr []

    let schemeParts scheme =
        let parameters, resultType = functionParts scheme.Body
        parameters, resultType

    let collectLeadingTypeParameters (tokens: Token list) =
        let significant =
            tokens
            |> List.filter (fun token ->
                match token.Kind with
                | Newline
                | Indent
                | Dedent
                | EndOfFile -> false
                | _ -> true)

        let rec collectHeader depth collected remaining =
            match remaining with
            | [] ->
                List.rev collected
            | token :: tail when token.Kind = LeftParen ->
                collectHeader (depth + 1) (token :: collected) tail
            | token :: tail when token.Kind = RightParen ->
                collectHeader (max 0 (depth - 1)) (token :: collected) tail
            | token :: _ when token.Kind = Colon && depth = 0 ->
                List.rev collected
            | token :: tail ->
                collectHeader depth (token :: collected) tail

        let headerTokens = collectHeader 0 [] significant

        let rec loop collected remaining =
            match remaining with
            | [] ->
                List.rev collected
            | { Kind = LeftParen } :: tail ->
                let rec collectBinder depth binderTokens rest =
                    match rest with
                    | [] ->
                        List.rev binderTokens, []
                    | token :: next when token.Kind = LeftParen ->
                        collectBinder (depth + 1) (token :: binderTokens) next
                    | token :: next when token.Kind = RightParen && depth = 0 ->
                        List.rev binderTokens, next
                    | token :: next when token.Kind = RightParen ->
                        collectBinder (depth - 1) (token :: binderTokens) next
                    | token :: next ->
                        collectBinder depth (token :: binderTokens) next

                let binderTokens, rest = collectBinder 0 [] tail

                let nextCollected =
                    binderTokens
                    |> List.tryFind Token.isName
                    |> Option.map (fun token -> (token |> TypeVariableName.fromToken) :: collected)
                    |> Option.defaultValue collected

                loop nextCollected rest
            | token :: tail when Token.isName token ->
                loop (TypeVariableName.fromToken token :: collected) tail
            | _ :: tail ->
                loop collected tail

        loop [] headerTokens

    let private stripConstructorDefaultTokens (tokens: Token list) =
        let tokenArray = tokens |> List.toArray
        let mutable parenDepth = 0
        let mutable braceDepth = 0
        let mutable bracketDepth = 0
        let mutable setBraceDepth = 0
        let mutable splitIndex = None

        for index = 0 to tokenArray.Length - 1 do
            match tokenArray[index].Kind with
            | LeftParen -> parenDepth <- parenDepth + 1
            | RightParen -> parenDepth <- max 0 (parenDepth - 1)
            | LeftBrace -> braceDepth <- braceDepth + 1
            | RightBrace -> braceDepth <- max 0 (braceDepth - 1)
            | LeftBracket -> bracketDepth <- bracketDepth + 1
            | RightBracket -> bracketDepth <- max 0 (bracketDepth - 1)
            | LeftSetBrace -> setBraceDepth <- setBraceDepth + 1
            | RightSetBrace -> setBraceDepth <- max 0 (setBraceDepth - 1)
            | Equals
                when parenDepth = 0
                     && braceDepth = 0
                     && bracketDepth = 0
                     && setBraceDepth = 0
                     && not (
                         index + 1 < tokenArray.Length
                         && tokenArray[index + 1].Kind = Operator
                         && String.Equals(tokenArray[index + 1].Text, ">", StringComparison.Ordinal)
                     ) ->
                splitIndex <- Some index
            | _ ->
                ()

        match splitIndex with
        | Some index when index > 0 ->
            tokenArray[0 .. index - 1] |> Array.toList
        | _ ->
            tokens

    let private stripSymbolicConstructorNameTokens tokens =
        let isSymbolToken token =
            match token.Kind with
            | Operator
            | Colon
            | Equals -> true
            | _ -> false

        match tokens with
        | { Kind = LeftParen } :: rest ->
            let rec loop remaining =
                match remaining with
                | { Kind = RightParen } :: tail ->
                    tail
                | token :: tail when isSymbolToken token ->
                    loop tail
                | _ ->
                    tokens

            loop rest
        | _nameToken :: rest ->
            rest
        | [] ->
            []

    let private constructorParameterTokenGroupsFromTokens (tokens: Token list) =
        let significant =
            tokens
            |> List.filter (fun token ->
                match token.Kind with
                | Newline
                | Indent
                | Dedent
                | EndOfFile -> false
                | _ -> true)

        let argumentTokens =
            stripSymbolicConstructorNameTokens significant

        let takeAtom remaining =
            match remaining with
            | [] ->
                [], []
            | first :: _ when first.Kind = LeftParen ->
                let tokenArray = remaining |> List.toArray
                let current = ResizeArray<Token>()
                let mutable depth = 0
                let mutable index = 0
                let mutable keepReading = true

                while keepReading && index < tokenArray.Length do
                    let token = tokenArray[index]
                    index <- index + 1

                    match token.Kind with
                    | LeftParen ->
                        depth <- depth + 1
                        if depth > 1 then
                            current.Add(token)
                    | RightParen ->
                        if depth > 1 then
                            current.Add(token)

                        depth <- depth - 1

                        if depth = 0 then
                            keepReading <- false
                    | _ ->
                        current.Add(token)

                List.ofSeq current, List.ofArray tokenArray[index ..]
            | first :: rest ->
                [ first ], rest

        let groups = ResizeArray<Token list>()
        let mutable position = 0
        let tokenArray = argumentTokens |> List.toArray

        while position < tokenArray.Length do
            let groupTokens, remaining = takeAtom (List.ofArray tokenArray[position ..])

            if not (List.isEmpty groupTokens) then
                groups.Add(groupTokens)

            position <- tokenArray.Length - remaining.Length

        groups |> Seq.toList

    let constructorParameterTokenGroups (constructor: DataConstructor) =
        match constructor.Parameters with
        | Some parameters ->
            parameters
            |> List.map (fun parameter ->
                let quantityTokens =
                    match parameter.ParameterQuantity with
                    | Some QuantityZero -> [ { Kind = IntegerLiteral; Text = "0"; Span = { Start = 0; Length = 1 } } ]
                    | Some QuantityOne -> [ { Kind = IntegerLiteral; Text = "1"; Span = { Start = 0; Length = 1 } } ]
                    | Some QuantityOmega -> [ { Kind = Identifier; Text = "omega"; Span = { Start = 0; Length = 5 } } ]
                    | Some(QuantityAtMostOne) ->
                        [ { Kind = Operator; Text = "<="; Span = { Start = 0; Length = 2 } }
                          { Kind = IntegerLiteral; Text = "1"; Span = { Start = 0; Length = 1 } } ]
                    | Some(QuantityAtLeastOne) ->
                        [ { Kind = Operator; Text = ">="; Span = { Start = 0; Length = 2 } }
                          { Kind = IntegerLiteral; Text = "1"; Span = { Start = 0; Length = 1 } } ]
                    | Some(QuantityBorrow _) -> [ { Kind = Operator; Text = "&"; Span = { Start = 0; Length = 1 } } ]
                    | Some(QuantityVariable name) -> [ { Kind = Identifier; Text = name; Span = { Start = 0; Length = name.Length } } ]
                    | None -> []

                let binderTokens =
                    match parameter.ParameterName with
                    | Some name ->
                        quantityTokens
                        @ [ { Kind = Identifier; Text = name; Span = { Start = 0; Length = name.Length } }
                            { Kind = Colon; Text = ":"; Span = { Start = 0; Length = 1 } } ]
                    | None ->
                        quantityTokens

                binderTokens @ parameter.ParameterTypeTokens)
        | None ->
            constructorParameterTokenGroupsFromTokens constructor.Tokens

    let private constructorFieldTokens (constructor: DataConstructor) =
        match constructor.Parameters with
        | Some parameters ->
            parameters |> List.map (fun parameter -> parameter.ParameterTypeTokens)
        | None ->
            constructorParameterTokenGroupsFromTokens constructor.Tokens
            |> List.choose (fun groupTokens ->
                match groupTokens |> List.tryFindIndex (fun token -> token.Kind = Colon) with
                | Some colonIndex when colonIndex + 1 < groupTokens.Length ->
                    Some(groupTokens |> List.skip (colonIndex + 1) |> stripConstructorDefaultTokens)
                | _ when not (List.isEmpty groupTokens) ->
                    Some(stripConstructorDefaultTokens groupTokens)
                | _ ->
                    None)

    let constructorFieldTypes (constructor: DataConstructor) =
        let significant =
            constructor.Tokens
            |> List.filter (fun token ->
                match token.Kind with
                | Newline
                | Indent
                | Dedent
                | EndOfFile -> false
                | _ -> true)

        match stripSymbolicConstructorNameTokens significant with
        | { Kind = Colon } :: typeTokens when constructor.Parameters.IsNone ->
            parseScheme typeTokens
            |> Option.map (schemeParts >> fst)
            |> Option.defaultValue []
        | _ ->
            constructorFieldTokens constructor
            |> List.choose parseType

    let constructorFieldTokenGroups (constructor: DataConstructor) =
        constructorFieldTokens constructor

    let rec applySubstitution substitution typeExpr =
        match typeExpr with
        | TypeVariable name ->
            substitution
            |> Map.tryFind name
            |> Option.defaultValue typeExpr
        | TypeLevelLiteral _ ->
            typeExpr
        | TypeUniverse None ->
            typeExpr
        | TypeUniverse(Some universeExpr) ->
            TypeUniverse(Some(applySubstitution substitution universeExpr))
        | TypeIntrinsic _ ->
            typeExpr
        | TypeApply(callee, arguments) ->
            TypeApply(
                applySubstitution substitution callee,
                arguments |> List.map (applySubstitution substitution)
            )
        | TypeLambda(parameterName, parameterSort, body) ->
            let nestedSubstitution =
                substitution |> Map.remove parameterName

            TypeLambda(
                parameterName,
                applySubstitution substitution parameterSort,
                applySubstitution nestedSubstitution body
            )
        | TypeDelay inner ->
            TypeDelay(applySubstitution substitution inner)
        | TypeMemo inner ->
            TypeMemo(applySubstitution substitution inner)
        | TypeForce inner ->
            TypeForce(applySubstitution substitution inner)
        | TypeProject(target, fieldName) ->
            TypeProject(applySubstitution substitution target, fieldName)
        | TypeName(typeReference, arguments) when List.length (TypeReference.segments typeReference) = 1 ->
            let name = TypeReference.localName typeReference |> ReferenceName.text |> TypeVariableName.create
            let substitutedArguments = arguments |> List.map (applySubstitution substitution)

            match substitution |> Map.tryFind name with
            | Some(TypeName(replacementReference, replacementArguments)) ->
                TypeName(replacementReference, replacementArguments @ substitutedArguments)
            | Some replacement when List.isEmpty substitutedArguments ->
                replacement
            | _ ->
                TypeName(makeTypeReference [ name |> TypeVariableName.text ], substitutedArguments)
        | TypeName(typeReference, arguments) ->
            TypeName(typeReference, arguments |> List.map (applySubstitution substitution))
        | TypeArrow(quantity, parameterType, resultType) ->
            TypeArrow(quantity, applySubstitution substitution parameterType, applySubstitution substitution resultType)
        | TypeEquality(left, right) ->
            TypeEquality(applySubstitution substitution left, applySubstitution substitution right)
        | TypeCapture(inner, captures) ->
            TypeCapture(applySubstitution substitution inner, captures)
        | TypeEffectRow(entries, tail) ->
            TypeEffectRow(
                entries
                |> List.map (fun entry ->
                    { Label = applySubstitution substitution entry.Label
                      Effect = applySubstitution substitution entry.Effect }),
                tail |> Option.map (applySubstitution substitution)
            )
        | TypeRecord fields ->
            TypeRecord(
                fields
                |> List.map (fun field ->
                    { field with
                        Type = applySubstitution substitution field.Type })
            )
        | TypeUnion members ->
            TypeUnion(members |> List.map (applySubstitution substitution))

    let applyConstraintSubstitution substitution (constraintInfo: TraitConstraint) =
        { constraintInfo with
            Arguments = constraintInfo.Arguments |> List.map (applySubstitution substitution) }

    let applyForallBinderSubstitution substitution (binder: ForallBinder) =
        { binder with
            Sort = applySubstitution substitution binder.Sort }

    let applySchemeSubstitution substitution (scheme: TypeScheme) =
        { scheme with
            Forall = scheme.Forall |> List.map (applyForallBinderSubstitution substitution)
            Constraints = scheme.Constraints |> List.map (applyConstraintSubstitution substitution)
            Body = applySubstitution substitution scheme.Body }

    let private occurs name typeExpr =
        let rec loop current =
            match current with
            | TypeVariable currentName ->
                currentName = name
            | TypeLevelLiteral _ ->
                false
            | TypeUniverse None ->
                false
            | TypeUniverse(Some universeExpr) ->
                loop universeExpr
            | TypeIntrinsic _ ->
                false
            | TypeApply(callee, arguments) ->
                loop callee || arguments |> List.exists loop
            | TypeLambda(parameterName, parameterSort, body) ->
                loop parameterSort
                || (if parameterName = name then false else loop body)
            | TypeDelay inner
            | TypeMemo inner
            | TypeForce inner ->
                loop inner
            | TypeProject(target, _) ->
                loop target
            | TypeName(_, arguments) ->
                arguments |> List.exists loop
            | TypeArrow(_, parameterType, resultType) ->
                loop parameterType || loop resultType
            | TypeEquality(left, right) ->
                loop left || loop right
            | TypeCapture(inner, _) ->
                loop inner
            | TypeEffectRow(entries, tail) ->
                entries |> List.exists (fun entry -> loop entry.Label || loop entry.Effect)
                || tail |> Option.exists loop
            | TypeRecord fields ->
                fields |> List.exists (fun field -> loop field.Type)
            | TypeUnion members ->
                members |> List.exists loop

        loop typeExpr

    let tryUnify left right =
        let rec unify substitution pending =
            match pending with
            | [] ->
                Some substitution
            | (leftType, rightType) :: rest ->
                let normalizedLeft = applySubstitution substitution leftType
                let normalizedRight = applySubstitution substitution rightType

                match normalizedLeft, normalizedRight with
                | TypeVariable leftName, TypeVariable rightName when leftName = rightName ->
                    unify substitution rest
                | TypeLevelLiteral leftLevel, TypeLevelLiteral rightLevel when leftLevel = rightLevel ->
                    unify substitution rest
                | TypeUniverse leftUniverse, TypeUniverse rightUniverse ->
                    match leftUniverse, rightUniverse with
                    | None, None ->
                        unify substitution rest
                    | Some leftExpr, Some rightExpr ->
                        unify substitution ((leftExpr, rightExpr) :: rest)
                    | _ ->
                        None
                | TypeIntrinsic leftClassifier, TypeIntrinsic rightClassifier when leftClassifier = rightClassifier ->
                    unify substitution rest
                | TypeVariable leftName, _ ->
                    if occurs leftName normalizedRight then
                        None
                    else
                        unify (substitution |> Map.add leftName normalizedRight) rest
                | _, TypeVariable rightName ->
                    if occurs rightName normalizedLeft then
                        None
                    else
                        unify (substitution |> Map.add rightName normalizedLeft) rest
                | TypeName(leftName, leftArguments), TypeName(rightName, rightArguments)
                    when leftName = rightName && List.length leftArguments = List.length rightArguments ->
                    unify substitution (List.zip leftArguments rightArguments @ rest)
                | TypeApply(leftCallee, leftArguments), TypeApply(rightCallee, rightArguments)
                    when List.length leftArguments = List.length rightArguments ->
                    unify substitution ((leftCallee, rightCallee) :: List.zip leftArguments rightArguments @ rest)
                | TypeApply(leftCallee, leftArguments), TypeName(rightName, rightArguments)
                    when List.length leftArguments = List.length rightArguments ->
                    unify
                        substitution
                        ((leftCallee, TypeName(rightName, [])) :: List.zip leftArguments rightArguments @ rest)
                | TypeName(leftName, leftArguments), TypeApply(rightCallee, rightArguments)
                    when List.length leftArguments = List.length rightArguments ->
                    unify
                        substitution
                        ((TypeName(leftName, []), rightCallee) :: List.zip leftArguments rightArguments @ rest)
                | TypeLambda(leftName, leftSort, leftBody), TypeLambda(rightName, rightSort, rightBody) ->
                    let alignedRightBody =
                        applySubstitution (Map.ofList [ rightName, TypeVariable leftName ]) rightBody

                    unify substitution ((leftSort, rightSort) :: (leftBody, alignedRightBody) :: rest)
                | TypeDelay leftInner, TypeDelay rightInner
                | TypeMemo leftInner, TypeMemo rightInner
                | TypeForce leftInner, TypeForce rightInner ->
                    unify substitution ((leftInner, rightInner) :: rest)
                | TypeProject(leftTarget, leftField), TypeProject(rightTarget, rightField)
                    when leftField = rightField ->
                    unify substitution ((leftTarget, rightTarget) :: rest)
                | TypeArrow(leftQuantity, leftParameter, leftResult), TypeArrow(rightQuantity, rightParameter, rightResult)
                    when leftQuantity = rightQuantity ->
                    unify substitution ((leftParameter, rightParameter) :: (leftResult, rightResult) :: rest)
                | TypeEquality(leftLeft, leftRight), TypeEquality(rightLeft, rightRight) ->
                    unify substitution ((leftLeft, rightLeft) :: (leftRight, rightRight) :: rest)
                | TypeCapture(leftInner, leftCaptures), TypeCapture(rightInner, rightCaptures)
                    when (leftCaptures |> List.distinct |> List.sort) = (rightCaptures |> List.distinct |> List.sort) ->
                    unify substitution ((leftInner, rightInner) :: rest)
                | TypeEffectRow(leftEntries, leftTail), TypeEffectRow(rightEntries, rightTail) ->
                    let normalizeEntries entries =
                        entries
                        |> List.sortBy (fun entry -> sprintf "%A:%A" entry.Label entry.Effect)

                    let normalizedLeftEntries = normalizeEntries leftEntries
                    let normalizedRightEntries = normalizeEntries rightEntries

                    if List.length normalizedLeftEntries = List.length normalizedRightEntries then
                        let pendingEntries =
                            List.zip normalizedLeftEntries normalizedRightEntries
                            |> List.collect (fun (leftEntry, rightEntry) ->
                                [ leftEntry.Label, rightEntry.Label
                                  leftEntry.Effect, rightEntry.Effect ])

                        match leftTail, rightTail with
                        | None, None ->
                            unify substitution (pendingEntries @ rest)
                        | Some leftTailExpr, Some rightTailExpr ->
                            unify substitution ((leftTailExpr, rightTailExpr) :: pendingEntries @ rest)
                        | _ ->
                            None
                    else
                        None
                | TypeRecord leftFields, TypeRecord rightFields ->
                    let normalizeFieldList (fields: RecordField list) =
                        fields
                        |> List.sortBy (fun (field: RecordField) -> field.Name)

                    let normalizedLeftFields = normalizeFieldList leftFields
                    let normalizedRightFields = normalizeFieldList rightFields

                    if List.length normalizedLeftFields = List.length normalizedRightFields
                       && List.forall2
                           (fun (leftField: RecordField) (rightField: RecordField) ->
                               leftField.Name = rightField.Name
                               && leftField.Quantity = rightField.Quantity)
                           normalizedLeftFields
                           normalizedRightFields then
                        let pendingFields =
                            List.zip normalizedLeftFields normalizedRightFields
                            |> List.map (fun ((leftField: RecordField), (rightField: RecordField)) -> leftField.Type, rightField.Type)

                        unify substitution (pendingFields @ rest)
                    else
                        None
                | TypeUnion leftMembers, TypeUnion rightMembers ->
                    let normalizeMembers members =
                        members
                        |> List.distinct
                        |> List.sortBy (sprintf "%A")

                    let normalizedLeftMembers = normalizeMembers leftMembers
                    let normalizedRightMembers = normalizeMembers rightMembers

                    if List.length normalizedLeftMembers = List.length normalizedRightMembers then
                        unify substitution (List.zip normalizedLeftMembers normalizedRightMembers @ rest)
                    else
                        None
                | _ ->
                    None

        unify Map.empty [ left, right ]

    let tryUnifyMany pairs =
        let rec loop substitution remaining =
            match remaining with
            | [] ->
                Some substitution
            | (left, right) :: rest ->
                let normalizedLeft = applySubstitution substitution left
                let normalizedRight = applySubstitution substitution right

                match tryUnify normalizedLeft normalizedRight with
                | Some nextSubstitution ->
                    let merged =
                        nextSubstitution
                        |> Map.fold (fun state name value -> Map.add name value state) substitution

                    loop merged rest
                | None ->
                    None

        loop Map.empty pairs

    let private instantiateBinders prefix index binders =
        let rec loop substitution nextIndex remaining instantiated =
            match remaining with
            | [] ->
                substitution, List.rev instantiated
            | binder :: rest ->
                let renamedName = $"{prefix}{nextIndex}" |> TypeVariableName.create
                let renamedBinder =
                    { binder with
                        Name = renamedName
                        Sort = applySubstitution substitution binder.Sort }

                loop
                    (Map.add binder.Name (TypeVariable renamedName) substitution)
                    (nextIndex + 1)
                    rest
                    (renamedBinder :: instantiated)

        loop Map.empty index binders []

    let instantiate prefix nextId (scheme: TypeScheme) =
        let substitution, instantiatedBinders = instantiateBinders prefix nextId scheme.Forall

        { Forall = instantiatedBinders
          Constraints = scheme.Constraints |> List.map (applyConstraintSubstitution substitution)
          Body = applySubstitution substitution scheme.Body }

    let private canonicalCaptureSet captures =
        captures
        |> List.distinct
        |> List.sortBy CaptureName.text

    let forallBinderNames binders =
        binders |> List.map (fun binder -> binder.Name)

    let inferredTypeForallBinders names =
        names
        |> List.map (fun name ->
            { Name = name
              Quantity = QuantityZero
              Sort = TypeUniverse None })

    let private tryFieldDependencyReference (fieldNames: Set<FieldName>) (referenceName: TypeVariableName) =
        let referenceText = referenceName |> TypeVariableName.text
        let directReference = referenceText |> FieldName.create

        let direct =
            if Set.contains directReference fieldNames then
                Some directReference
            else
                None

        direct
        |> Option.orElseWith (fun () ->
            if referenceText.StartsWith("this.", StringComparison.Ordinal) then
                let suffix = referenceText.Substring("this.".Length) |> FieldName.create

                if Set.contains suffix fieldNames then
                    Some suffix
                else
                    None
            else
                None)

    let rec private collectFreeTypeVariableSet typeExpr =
        let rec loop bound current =
            match current with
            | TypeVariable name when not (Set.contains name bound) ->
                Set.singleton name
            | TypeVariable _
            | TypeLevelLiteral _
            | TypeUniverse None
            | TypeIntrinsic _ ->
                Set.empty
            | TypeUniverse(Some universeExpr) ->
                loop bound universeExpr
            | TypeName(_, arguments) ->
                arguments
                |> List.fold (fun state argument -> Set.union state (loop bound argument)) Set.empty
            | TypeApply(callee, arguments) ->
                arguments
                |> List.fold (fun state argument -> Set.union state (loop bound argument)) (loop bound callee)
            | TypeLambda(parameterName, parameterSort, body) ->
                Set.union (loop bound parameterSort) (loop (Set.add parameterName bound) body)
            | TypeDelay inner
            | TypeMemo inner
            | TypeForce inner ->
                loop bound inner
            | TypeProject(target, _) ->
                loop bound target
            | TypeArrow(_, parameterType, resultType) ->
                Set.union (loop bound parameterType) (loop bound resultType)
            | TypeEquality(left, right) ->
                Set.union (loop bound left) (loop bound right)
            | TypeCapture(inner, _) ->
                loop bound inner
            | TypeEffectRow(entries, tail) ->
                let entryVariables =
                    entries
                    |> List.fold
                        (fun state entry ->
                            state
                            |> Set.union (loop bound entry.Label)
                            |> Set.union (loop bound entry.Effect))
                        Set.empty

                tail
                |> Option.map (loop bound)
                |> Option.defaultValue Set.empty
                |> Set.union entryVariables
            | TypeRecord fields ->
                fields
                |> List.fold (fun state field -> Set.union state (loop bound field.Type)) Set.empty
            | TypeUnion members ->
                members
                |> List.fold (fun state memberType -> Set.union state (loop bound memberType)) Set.empty

        loop Set.empty typeExpr

    let private recordFieldDependencyMap (fields: RecordField list) =
        let fieldNames =
            fields
            |> List.map (fun (field: RecordField) -> field.Name)
            |> Set.ofList

        let rec collectDependencies typeExpr =
            match typeExpr with
            | TypeVariable name ->
                tryFieldDependencyReference fieldNames name |> Option.toList |> Set.ofList
            | TypeLevelLiteral _
            | TypeUniverse None
            | TypeIntrinsic _ ->
                Set.empty
            | TypeUniverse(Some universeExpr) ->
                collectDependencies universeExpr
            | TypeName(_, arguments) ->
                arguments
                |> List.fold (fun state argument -> Set.union state (collectDependencies argument)) Set.empty
            | TypeApply(callee, arguments) ->
                arguments
                |> List.fold (fun state argument -> Set.union state (collectDependencies argument)) (collectDependencies callee)
            | TypeLambda(parameterName, parameterSort, body) ->
                Set.remove (parameterName |> TypeVariableName.text |> FieldName.create) (Set.union (collectDependencies parameterSort) (collectDependencies body))
            | TypeDelay inner
            | TypeMemo inner
            | TypeForce inner ->
                collectDependencies inner
            | TypeProject(target, fieldName) ->
                Set.union
                    (collectDependencies target)
                    ((if Set.contains fieldName fieldNames then Some fieldName else None) |> Option.toList |> Set.ofList)
            | TypeArrow(_, parameterType, resultType) ->
                Set.union (collectDependencies parameterType) (collectDependencies resultType)
            | TypeEquality(left, right) ->
                Set.union (collectDependencies left) (collectDependencies right)
            | TypeCapture(inner, _) ->
                collectDependencies inner
            | TypeEffectRow(entries, tail) ->
                let entryDependencies =
                    entries
                    |> List.fold
                        (fun state entry ->
                            state
                            |> Set.union (collectDependencies entry.Label)
                            |> Set.union (collectDependencies entry.Effect))
                        Set.empty

                tail
                |> Option.map collectDependencies
                |> Option.defaultValue Set.empty
                |> Set.union entryDependencies
            | TypeRecord nestedFields ->
                nestedFields
                |> List.fold (fun state (field: RecordField) -> Set.union state (collectDependencies field.Type)) Set.empty
            | TypeUnion members ->
                members
                |> List.fold (fun state memberType -> Set.union state (collectDependencies memberType)) Set.empty

        let fieldMap =
            fields
            |> List.map (fun (field: RecordField) -> field.Name, field)
            |> Map.ofList

        let dependencyMap =
            fields
            |> List.map (fun (field: RecordField) ->
                field.Name, (collectDependencies field.Type |> Set.remove field.Name))
            |> Map.ofList

        fieldMap, dependencyMap

    let private hasDependencyCycle (dependencyMap: Map<FieldName, Set<FieldName>>) =
        let rec visit visiting visited node =
            if Set.contains node visiting then
                true
            elif Set.contains node visited then
                false
            else
                let nextVisiting = Set.add node visiting
                let nextVisited = Set.add node visited

                dependencyMap[node]
                |> Set.exists (visit nextVisiting nextVisited)

        dependencyMap
        |> Map.exists (fun node _ -> visit Set.empty Set.empty node)

    let rec hasCyclicRecordDependencies typeExpr =
        let rec loop current =
            match current with
            | TypeVariable _
            | TypeLevelLiteral _
            | TypeUniverse None
            | TypeIntrinsic _ ->
                false
            | TypeUniverse(Some universeExpr) ->
                loop universeExpr
            | TypeName(_, arguments) ->
                arguments |> List.exists loop
            | TypeApply(callee, arguments) ->
                loop callee || (arguments |> List.exists loop)
            | TypeLambda(_, parameterSort, body) ->
                loop parameterSort || loop body
            | TypeDelay inner
            | TypeMemo inner
            | TypeForce inner ->
                loop inner
            | TypeProject(target, _) ->
                loop target
            | TypeArrow(_, parameterType, resultType) ->
                loop parameterType || loop resultType
            | TypeEquality(left, right) ->
                loop left || loop right
            | TypeCapture(inner, _) ->
                loop inner
            | TypeEffectRow(entries, tail) ->
                entries |> List.exists (fun entry -> loop entry.Label || loop entry.Effect)
                || tail |> Option.exists loop
            | TypeRecord fields ->
                let _, dependencyMap = recordFieldDependencyMap fields
                hasDependencyCycle dependencyMap || (fields |> List.exists (fun field -> loop field.Type))
            | TypeUnion members ->
                members |> List.exists loop

        loop typeExpr

    let private canonicalRecordFields (fields: RecordField list) =
        let fieldMap, dependencyMap = recordFieldDependencyMap fields
        let fieldNames = fieldMap |> Map.keys |> Set.ofSeq

        let rec loop remaining resolved ordered =
            if Set.isEmpty remaining then
                List.rev ordered
            else
                let ready =
                    remaining
                    |> Set.filter (fun name ->
                        dependencyMap[name] |> Set.forall (fun dependency -> Set.contains dependency resolved))
                    |> Set.toList
                    |> List.sortBy FieldName.text

                match ready with
                | next :: _ ->
                    loop (Set.remove next remaining) (Set.add next resolved) (fieldMap[next] :: ordered)
                | [] ->
                    let fallback =
                        remaining
                        |> Set.toList
                        |> List.sortBy FieldName.text
                        |> List.map (fun name -> fieldMap[name])

                    List.rev ordered @ fallback

        loop fieldNames Set.empty []

    let rec private tryEtaContractFunction typeExpr =
        match typeExpr with
        | TypeLambda(parameterName, parameterSort, TypeApply(callee, [ TypeVariable appliedName ]))
            when parameterName = appliedName
                 && not (Set.contains parameterName (collectFreeTypeVariableSet callee)) ->
            Some(callee, parameterSort)
        | _ ->
            None

    let rec private tryEtaContractRecord typeExpr =
        match typeExpr with
        | TypeRecord fields when not (List.isEmpty fields) ->
            let normalizedFields = canonicalRecordFields fields

            let projectedTargets =
                normalizedFields
                |> List.map (fun field ->
                    match field.Type with
                    | TypeProject(target, fieldName)
                        when field.Name = fieldName ->
                        Some target
                    | _ ->
                        None)

            if projectedTargets |> List.forall Option.isSome then
                let targets = projectedTargets |> List.choose id

                match targets with
                | first :: rest when rest |> List.forall ((=) first) ->
                    Some first
                | _ ->
                    None
            else
                None
        | _ ->
            None

    let rec private normalizeWithContext (context: DefinitionContext) typeExpr =
        let normalize = normalizeWithContext context

        let normalizeTypeName name arguments =
            let normalizedArguments = arguments |> List.map normalize
            let normalizedName =
                CompilerKnownSymbols.KnownTypes.tryClassifyName (TypeReference.segments name)
                |> Option.bind (fun knownType ->
                    if CompilerKnownSymbols.KnownTypes.acceptedPaths knownType |> List.exists ((=) (TypeReference.segments name)) then
                        Some(TypeReference.ofSegments (CompilerKnownSymbols.KnownTypes.canonicalPath knownType))
                    else
                        None)
                |> Option.defaultValue name

            let normalizeLegacyIntrinsicTypeName () =
                match TypeReference.segments normalizedName, normalizedArguments with
                | [ "Type" ], [] ->
                    Some(TypeUniverse None)
                | [ "Type" ], [ universeExpr ] ->
                    Some(TypeUniverse(Some universeExpr))
                | _ ->
                    match TypeReference.segments normalizedName with
                    | [ intrinsicName ] when List.isEmpty normalizedArguments ->
                        tryIntrinsicClassifier (TypeReference.ofSegments [ intrinsicName ]) |> Option.map TypeIntrinsic
                    | _ ->
                        None

            match normalizeLegacyIntrinsicTypeName () with
            | Some normalizedLegacy ->
                normalizedLegacy
            | None ->
                match context |> Map.tryFind (TypeReference.qualifiedReference normalizedName) with
                | Some definition when definition.Transparent && definition.ConversionReducible ->
                    if List.length normalizedArguments >= List.length definition.ParameterNames then
                        let appliedParameters, remainingArguments =
                            normalizedArguments |> List.splitAt (List.length definition.ParameterNames)

                        let substitutedBody =
                            List.zip definition.ParameterNames appliedParameters
                            |> List.fold (fun body (parameterName, argumentValue) ->
                                applySubstitution (Map.ofList [ parameterName, argumentValue ]) body) definition.DefinitionBody
                            |> normalize

                        match remainingArguments with
                        | [] ->
                            substitutedBody
                        | extra ->
                            normalize (TypeApply(substitutedBody, extra))
                    else
                        TypeName(normalizedName, normalizedArguments)
                | _ ->
                    TypeName(normalizedName, normalizedArguments)

        let canonicalizeRecord fields =
            let normalizedFields =
                fields
                |> List.map (fun (field: RecordField) ->
                    { field with
                        Type = normalize field.Type })
                |> canonicalRecordFields

            match normalizedFields with
            | [] ->
                unitTypeExpr
            | _ ->
                TypeRecord normalizedFields

        let rec normalizeOne current =
            match current with
            | TypeVariable _
            | TypeLevelLiteral _
            | TypeIntrinsic _ ->
                current
            | TypeUniverse None ->
                TypeUniverse None
            | TypeUniverse(Some universeExpr) ->
                TypeUniverse(Some(normalize universeExpr))
            | TypeName(name, arguments) ->
                normalizeTypeName name arguments
            | TypeApply(callee, arguments) ->
                let normalizedCallee = normalize callee
                let normalizedArguments = arguments |> List.map normalize

                match normalizedCallee, normalizedArguments with
                | TypeLambda(parameterName, _, body), argument :: remaining ->
                    let reduced =
                        body
                        |> applySubstitution (Map.ofList [ parameterName, argument ])

                    match remaining with
                    | [] -> normalize reduced
                    | _ -> normalize (TypeApply(reduced, remaining))
                | TypeName(name, existingArguments), _ ->
                    normalizeTypeName name (existingArguments @ normalizedArguments)
                | _, [] ->
                    normalizedCallee
                | _ ->
                    TypeApply(normalizedCallee, normalizedArguments)
            | TypeLambda(parameterName, parameterSort, body) ->
                let normalized =
                    TypeLambda(parameterName, normalize parameterSort, normalize body)

                match tryEtaContractFunction normalized with
                | Some(contracted, _) ->
                    normalize contracted
                | None ->
                    normalized
            | TypeDelay inner ->
                TypeDelay(normalize inner)
            | TypeMemo inner ->
                TypeMemo(normalize inner)
            | TypeForce inner ->
                match normalize inner with
                | TypeDelay delayed
                | TypeMemo delayed ->
                    normalize delayed
                | normalizedInner ->
                    TypeForce normalizedInner
            | TypeProject(target, fieldName) ->
                match normalize target with
                | TypeRecord fields ->
                    fields
                    |> List.tryFind (fun field -> field.Name = fieldName)
                    |> Option.map (fun field -> normalize field.Type)
                    |> Option.defaultValue (TypeProject(TypeRecord fields, fieldName))
                | normalizedTarget ->
                    TypeProject(normalizedTarget, fieldName)
            | TypeArrow(quantity, parameterType, resultType) ->
                TypeArrow(quantity, normalize parameterType, normalize resultType)
            | TypeEquality(left, right) ->
                TypeEquality(normalize left, normalize right)
            | TypeCapture(inner, captures) ->
                TypeCapture(normalize inner, canonicalCaptureSet captures)
            | TypeEffectRow(entries, tail) ->
                TypeEffectRow(
                    entries
                    |> List.map (fun entry ->
                        { Label = normalize entry.Label
                          Effect = normalize entry.Effect })
                    |> List.sortBy (fun entry -> sprintf "%A:%A" entry.Label entry.Effect),
                    tail |> Option.map normalize
                )
            | TypeRecord fields ->
                let normalized = canonicalizeRecord fields

                match tryEtaContractRecord normalized with
                | Some(contracted) ->
                    normalize contracted
                | None ->
                    normalized
            | TypeUnion members ->
                TypeUnion(
                    members
                    |> List.map normalize
                    |> List.distinct
                    |> List.sortBy (sprintf "%A")
                )

        normalizeOne typeExpr

    let private canonicalize = normalizeWithContext builtinDefinitionContext

    let private canonicalizeBoundTypeLambdas typeExpr =
        let rec loop substitution nextId current =
            match current with
            | TypeVariable name ->
                Map.tryFind name substitution |> Option.defaultValue current, nextId
            | TypeLevelLiteral _
            | TypeUniverse None
            | TypeIntrinsic _ ->
                current, nextId
            | TypeUniverse(Some universeExpr) ->
                let normalizedUniverseExpr, nextId = loop substitution nextId universeExpr
                TypeUniverse(Some normalizedUniverseExpr), nextId
            | TypeName(name, arguments) ->
                let normalizedArguments, nextId =
                    arguments
                    |> List.fold
                        (fun (state, nextId) argument ->
                            let normalizedArgument, nextId = loop substitution nextId argument
                            normalizedArgument :: state, nextId)
                        ([], nextId)

                TypeName(name, List.rev normalizedArguments), nextId
            | TypeApply(callee, arguments) ->
                let normalizedCallee, nextId = loop substitution nextId callee

                let normalizedArguments, nextId =
                    arguments
                    |> List.fold
                        (fun (state, nextId) argument ->
                            let normalizedArgument, nextId = loop substitution nextId argument
                            normalizedArgument :: state, nextId)
                        ([], nextId)

                TypeApply(normalizedCallee, List.rev normalizedArguments), nextId
            | TypeLambda(parameterName, parameterSort, body) ->
                let normalizedParameterSort, nextId = loop substitution nextId parameterSort
                let canonicalParameterName = $"__alpha{nextId}" |> TypeVariableName.create
                let substitution = Map.add parameterName (TypeVariable canonicalParameterName) substitution
                let normalizedBody, nextId = loop substitution (nextId + 1) body
                TypeLambda(canonicalParameterName, normalizedParameterSort, normalizedBody), nextId
            | TypeDelay inner ->
                let normalizedInner, nextId = loop substitution nextId inner
                TypeDelay normalizedInner, nextId
            | TypeMemo inner ->
                let normalizedInner, nextId = loop substitution nextId inner
                TypeMemo normalizedInner, nextId
            | TypeForce inner ->
                let normalizedInner, nextId = loop substitution nextId inner
                TypeForce normalizedInner, nextId
            | TypeProject(target, fieldName) ->
                let normalizedTarget, nextId = loop substitution nextId target
                TypeProject(normalizedTarget, fieldName), nextId
            | TypeArrow(quantity, parameterType, resultType) ->
                let normalizedParameterType, nextId = loop substitution nextId parameterType
                let normalizedResultType, nextId = loop substitution nextId resultType
                TypeArrow(quantity, normalizedParameterType, normalizedResultType), nextId
            | TypeEquality(left, right) ->
                let normalizedLeft, nextId = loop substitution nextId left
                let normalizedRight, nextId = loop substitution nextId right
                TypeEquality(normalizedLeft, normalizedRight), nextId
            | TypeCapture(inner, captures) ->
                let normalizedInner, nextId = loop substitution nextId inner
                TypeCapture(normalizedInner, captures), nextId
            | TypeEffectRow(entries, tail) ->
                let normalizedEntries, nextId =
                    entries
                    |> List.fold
                        (fun (state, nextId) entry ->
                            let normalizedLabel, nextId = loop substitution nextId entry.Label
                            let normalizedEffect, nextId = loop substitution nextId entry.Effect

                            { Label = normalizedLabel
                              Effect = normalizedEffect }
                            :: state,
                            nextId)
                        ([], nextId)

                let normalizedTail, nextId =
                    match tail with
                    | Some tailExpr ->
                        let normalizedTailExpr, nextId = loop substitution nextId tailExpr
                        Some normalizedTailExpr, nextId
                    | None ->
                        None, nextId

                TypeEffectRow(List.rev normalizedEntries, normalizedTail), nextId
            | TypeRecord fields ->
                let normalizedFields, nextId =
                    fields
                    |> List.fold
                        (fun (state, nextId) field ->
                            let normalizedFieldType, nextId = loop substitution nextId field.Type

                            { field with
                                Type = normalizedFieldType }
                            :: state,
                            nextId)
                        ([], nextId)

                TypeRecord(List.rev normalizedFields), nextId
            | TypeUnion members ->
                let normalizedMembers, nextId =
                    members
                    |> List.fold
                        (fun (state, nextId) memberType ->
                            let normalizedMemberType, nextId = loop substitution nextId memberType
                            normalizedMemberType :: state, nextId)
                        ([], nextId)

                TypeUnion(List.rev normalizedMembers), nextId

        loop Map.empty 0 typeExpr |> fst

    let definitionallyEqualIn context left right =
        not (hasCyclicRecordDependencies left || hasCyclicRecordDependencies right)
        && (normalizeWithContext context left |> canonicalizeBoundTypeLambdas)
           = (normalizeWithContext context right |> canonicalizeBoundTypeLambdas)

    let definitionallyEqual left right =
        definitionallyEqualIn builtinDefinitionContext left right

    let private canonicalizeScheme (scheme: TypeScheme) =
        let substitution, canonicalBinders =
            instantiateBinders "__canon" 0 scheme.Forall

        { Forall = canonicalBinders
          Constraints = scheme.Constraints |> List.map (applyConstraintSubstitution substitution)
          Body = applySubstitution substitution scheme.Body }

    let schemeDefinitionallyEqual left right =
        let left = canonicalizeScheme left
        let right = canonicalizeScheme right

        List.length left.Forall = List.length right.Forall
        && List.forall2
            (fun leftBinder rightBinder ->
                leftBinder.Quantity = rightBinder.Quantity
                && definitionallyEqual leftBinder.Sort rightBinder.Sort)
            left.Forall
            right.Forall
        && List.length left.Constraints = List.length right.Constraints
        && List.forall2
            (fun leftConstraint rightConstraint ->
                TraitReference.matches leftConstraint.Trait rightConstraint.Trait
                && List.length leftConstraint.Arguments = List.length rightConstraint.Arguments
                && List.forall2 definitionallyEqual leftConstraint.Arguments rightConstraint.Arguments)
            left.Constraints
            right.Constraints
        && definitionallyEqual left.Body right.Body

    let rec toText typeExpr =
        let renderTypeName name =
            let segments = TypeReference.segments name

            match segments with
            | _ when
                List.take (max 0 (List.length segments - 1)) segments = CompilerKnownSymbols.KnownModules.Prelude
                && (CompilerKnownSymbols.KnownTypes.tryClassifyName segments
                    |> Option.exists (fun knownType ->
                        CompilerKnownSymbols.KnownTypes.acceptedPaths knownType
                        |> List.exists ((=) segments))) ->
                TypeReference.localName name |> ReferenceName.text
            | _ -> TypeReference.text name

        let rec renderAtom current =
            match current with
            | TypeLevelLiteral level ->
                string level
            | TypeUniverse None ->
                "Type"
            | TypeUniverse(Some(TypeLevelLiteral level)) ->
                $"Type{level}"
            | TypeUniverse(Some universeExpr) ->
                $"Type {renderAtom universeExpr}"
            | TypeIntrinsic classifier ->
                intrinsicClassifierName classifier
            | TypeName(name, []) ->
                renderTypeName name
            | TypeName(name, arguments) ->
                let argumentText = arguments |> List.map renderAtom |> String.concat " "
                $"{renderTypeName name} {argumentText}"
            | TypeVariable name ->
                name |> TypeVariableName.text
            | TypeApply(callee, arguments) ->
                let argumentText = arguments |> List.map renderAtom |> String.concat " "
                $"{renderAtom callee} {argumentText}"
            | TypeLambda(parameterName, parameterSort, body) ->
                $"(\\({parameterName |> TypeVariableName.text} : {toText parameterSort}) -> {toText body})"
            | TypeDelay inner ->
                $"(thunk {toText inner})"
            | TypeMemo inner ->
                $"(lazy {toText inner})"
            | TypeForce inner ->
                $"(force {toText inner})"
            | TypeProject(target, fieldName) ->
                $"{renderAtom target}.{fieldName |> FieldName.text}"
            | TypeCapture _
            | TypeEffectRow _
            | TypeRecord _
            | TypeUnion _
            | TypeArrow _
            | TypeEquality _ ->
                $"({toText current})"

        match canonicalize typeExpr with
        | TypeArrow(quantity, parameterType, resultType) ->
            match quantity with
            | QuantityOmega ->
                $"{renderAtom parameterType} -> {toText resultType}"
            | _ ->
                $"({Quantity.toSurfaceText quantity} x : {toText parameterType}) -> {toText resultType}"
        | TypeEquality(left, right) ->
            $"{renderAtom left} = {toText right}"
        | TypeCapture(inner, captures) ->
            let capturesText = captures |> List.map CaptureName.text |> String.concat ", "
            $"{renderAtom inner} captures ({capturesText})"
        | TypeEffectRow(entries, tail) ->
            let entryText =
                entries
                |> List.map (fun entry -> $"{toText entry.Label} : {toText entry.Effect}")
                |> String.concat ", "

            match tail with
            | None when String.IsNullOrEmpty(entryText) ->
                "<[ ]>"
            | None ->
                $"<[{entryText}]>"
            | Some tailExpr when String.IsNullOrEmpty(entryText) ->
                $"<[{toText tailExpr}]>"
            | Some tailExpr ->
                $"<[{entryText} | {toText tailExpr}]>"
        | TypeRecord fields ->
            let fieldText =
                fields
                |> List.map (fun field ->
                    let quantityText =
                        match field.Quantity with
                        | QuantityOmega -> ""
                        | quantity -> Quantity.toSurfaceText quantity + " "

                    $"{quantityText}{field.Name |> FieldName.text} : {toText field.Type}")
                |> String.concat ", "

            $"({fieldText})"
        | TypeUnion members ->
            let memberText = members |> List.map toText |> String.concat " | "
            $"(| {memberText} |)"
        | _ ->
            renderAtom (canonicalize typeExpr)

    let collectFreeTypeVariables typeExpr =
        collectFreeTypeVariableSet typeExpr
        |> Set.toList
        |> List.map TypeVariableName.text
        |> List.sort
