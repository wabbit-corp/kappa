namespace Kappa.Compiler

open System

type LetHeaderParseResult =
    { Parameters: Parameter list
      ReturnTypeTokens: Token list option }

type ProjectionHeaderParseResult =
    { Binders: ProjectionBinder list
      ReturnTypeTokens: Token list }

type private LocalPureBlockItem =
    | LocalPureBinding of SurfaceBindPattern * SurfaceExpression
    | LocalPureSignature of BindingSignature
    | LocalPureTypeAlias of TypeAlias
    | LocalPureScopedEffect of string

// Parses shared binder forms used by both surface syntax and type/signature parsing.
module private SurfaceBinderParsing =
    let makeParameter name typeTokens quantity isImplicit isInout =
        { Name = name
          TypeTokens = typeTokens
          Quantity = quantity
          IsImplicit = isImplicit
          IsInout = isInout
          IsReceiver = false }

    let private makeReceiverParameter name typeTokens quantity =
        { Name = name
          TypeTokens = typeTokens
          Quantity = quantity
          IsImplicit = false
          IsInout = false
          IsReceiver = true }

    // Wildcard binders elaborate to fresh internal names that cannot be referenced from source.
    let wildcardParameterName (span: TextSpan) =
        $"__kappa_wildcard_{span.Start}"

    let private tokenTextEquals expected (token: Token) =
        String.Equals(token.Text, expected, StringComparison.Ordinal)

    let private isOmegaText text =
        String.Equals(text, "\u03c9", StringComparison.Ordinal)
        || String.Equals(text, "omega", StringComparison.Ordinal)

    let private isQuantityVariableToken (token: Token) =
        if token.Kind <> Identifier then
            false
        else
            let text = SyntaxFacts.trimIdentifierQuotes token.Text
            not (String.IsNullOrEmpty text)
            && not (Char.IsUpper text[0])
            && not (String.Equals(text, "this", StringComparison.Ordinal))

    let tryParseQuantityPrefix (tokens: Token list) =
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
        | head :: rest when Token.isName head && isOmegaText head.Text ->
            Some(QuantityOmega, rest)
        | head :: rest when isQuantityVariableToken head ->
            // Quantity variables are only unambiguous when another binder name follows.
            match rest with
            | next :: _ when Token.isName next ->
                Some(QuantityVariable(SyntaxFacts.trimIdentifierQuotes head.Text), rest)
            | _ ->
                None
        | _ ->
            None

    let private tryParseSuspensionMarker (tokens: Token list) =
        match tokens with
        | head :: rest when Token.isKeyword Keyword.Thunk head ->
            Some({ head with Kind = Identifier; Text = "Thunk" }, rest)
        | head :: rest when Token.isKeyword Keyword.Lazy head ->
            Some({ head with Kind = Identifier; Text = "Need" }, rest)
        | _ ->
            None

    let parseParameterFromTokens (diagnostics: DiagnosticBag) (source: SourceText) eofSpan (tokens: Token list) =
        let unsupported span =
            diagnostics.AddError(DiagnosticCode.ParseError, "Unsupported parameter binder syntax.", source.GetLocation(span))

        let rec parseBody isImplicit isInout explicitQuantity remaining =
            let quantity, bodyTokens =
                if isInout then
                    Some QuantityOne, remaining
                else
                    match explicitQuantity with
                        | Some quantity -> Some quantity, remaining
                        | None ->
                            match tryParseQuantityPrefix remaining with
                            | Some(quantity, rest) -> Some quantity, rest
                            | None -> None, remaining

            let suspensionTypeToken, bodyTokens =
                match tryParseSuspensionMarker bodyTokens with
                | Some(typeToken, rest) -> Some typeToken, rest
                | None -> None, bodyTokens

            match bodyTokens with
            | [] ->
                diagnostics.AddError(DiagnosticCode.ParseError, "Expected a parameter binder.", source.GetLocation(eofSpan))
                None
            | thisToken :: colon :: typeTokens
                when (not isImplicit)
                     && (not isInout)
                     && Token.isName thisToken
                     && String.Equals(SyntaxFacts.trimIdentifierQuotes thisToken.Text, "this", StringComparison.Ordinal)
                     && colon.Kind = Colon ->
                Some(makeReceiverParameter "this" (Some typeTokens) quantity)
            | thisToken :: nameToken :: colon :: typeTokens
                when (not isImplicit)
                     && (not isInout)
                     && Token.isName thisToken
                     && String.Equals(SyntaxFacts.trimIdentifierQuotes thisToken.Text, "this", StringComparison.Ordinal)
                     && Token.isName nameToken
                     && colon.Kind = Colon ->
                Some(makeReceiverParameter (SyntaxFacts.trimIdentifierQuotes nameToken.Text) (Some typeTokens) quantity)
            | { Kind = Underscore; Span = span } :: tail ->
                let name = wildcardParameterName span

                let typeTokens =
                    match tail with
                    | [] when suspensionTypeToken.IsNone -> None
                    | colon :: rest when colon.Kind = Colon ->
                        Some(suspensionTypeToken |> Option.map (fun typeToken -> typeToken :: rest) |> Option.defaultValue rest)
                    | _ ->
                        unsupported span
                        None

                Some(makeParameter name typeTokens quantity isImplicit isInout)
            | head :: tail when Token.isName head ->
                let name = SyntaxFacts.trimIdentifierQuotes head.Text

                let typeTokens =
                    match tail with
                    | [] when suspensionTypeToken.IsNone -> None
                    | colon :: rest when colon.Kind = Colon ->
                        Some(suspensionTypeToken |> Option.map (fun typeToken -> typeToken :: rest) |> Option.defaultValue rest)
                    | _ ->
                        unsupported head.Span
                        None

                Some(makeParameter name typeTokens quantity isImplicit isInout)
            | head :: _ ->
                diagnostics.AddError(DiagnosticCode.ParseError, "Expected a parameter name.", source.GetLocation(head.Span))
                None

        match tokens with
        | [] ->
            diagnostics.AddError(DiagnosticCode.ParseError, "Expected a parameter binder.", source.GetLocation(eofSpan))
            None
        | { Kind = AtSign } :: rest ->
            parseBody true false None rest
        | head :: rest when Token.isKeyword Keyword.Inout head ->
            parseBody false true (Some QuantityOne) rest
        | _ ->
            parseBody false false None tokens

    let private collectPatternNames pattern =
        let rec loop current =
            seq {
                match current with
                | NamePattern name ->
                    yield name
                | AsPattern(name, inner) ->
                    yield name
                    yield! loop inner
                | TypedPattern(inner, _) ->
                    yield! loop inner
                | ConstructorPattern(_, arguments) ->
                    for argument in arguments do
                        yield! loop argument
                | NamedConstructorPattern(_, fields) ->
                    for field in fields do
                        yield! loop field.Pattern
                | TuplePattern elements ->
                    for element in elements do
                        yield! loop element
                | VariantPattern(BoundVariantPattern(name, _))
                | VariantPattern(RestVariantPattern name) ->
                    yield name
                | VariantPattern(WildcardVariantPattern _) ->
                    ()
                | OrPattern alternatives ->
                    for alternative in alternatives do
                        yield! loop alternative
                | AnonymousRecordPattern(fields, rest) ->
                    for field in fields do
                        yield! loop field.Pattern
                    match rest with
                    | Some(BindRecordPatternRest name) -> yield name
                    | _ -> ()
                | WildcardPattern
                | LiteralPattern _ ->
                    ()
            }

        loop pattern |> Seq.toList

    let private tokenPatternName (token: Token) =
        match token.Kind with
        | Identifier
        | Keyword _ ->
            Some(SyntaxFacts.trimIdentifierQuotes token.Text)
        | Operator ->
            Some token.Text
        | _ ->
            None

    let private collectBinderSpans pattern patternTokens =
        let rec assign names remainingTokens spans =
            match names with
            | [] ->
                spans
            | name :: restNames ->
                match
                    remainingTokens
                    |> List.tryFindIndex (fun token ->
                        tokenPatternName token
                        |> Option.exists (fun tokenName -> String.Equals(tokenName, name, StringComparison.Ordinal)))
                with
                | Some index ->
                    let token = remainingTokens[index]
                    let nextTokens = remainingTokens |> List.skip (index + 1)

                    spans
                    |> Map.change name (fun existing -> Some(token.Span :: Option.defaultValue [] existing))
                    |> assign restNames nextTokens
                | None ->
                    assign restNames remainingTokens spans

        assign (collectPatternNames pattern) patternTokens Map.empty
        |> Map.map (fun _ spans -> List.rev spans)

    let makeBindPattern pattern quantity patternTokens =
        { Pattern = pattern
          Quantity = quantity
          IsImplicit = false
          TypeTokens = None
          BinderSpans = collectBinderSpans pattern patternTokens }

    let private stripOuterParens tokens =
        match tokens with
        | { Kind = LeftParen } :: rest ->
            match List.rev rest with
            | { Kind = RightParen } :: reversedInner -> Some(List.rev reversedInner)
            | _ -> None
        | _ ->
            None

    let private tryParseImplicitLocalBindPattern tokens =
        let bodyTokens =
            stripOuterParens tokens
            |> Option.defaultValue tokens

        match bodyTokens with
        | { Kind = AtSign } :: rest ->
            parseParameterFromTokens
                (DiagnosticBag())
                (SourceText.From("__implicit_local__.kp", ""))
                (TextSpan.FromBounds(0, 0))
                bodyTokens
            |> Option.bind (fun parameter ->
                parameter.TypeTokens
                |> Option.map (fun typeTokens ->
                    { Pattern = NamePattern parameter.Name
                      Quantity = parameter.Quantity
                      IsImplicit = true
                      TypeTokens = Some typeTokens
                      BinderSpans = collectBinderSpans (NamePattern parameter.Name) bodyTokens }))
        | _ ->
            None

    let parseBindPatternFromTokens (parsePattern: Token list -> SurfacePattern) (tokens: Token list) =
        let rec normalizeBinderPattern pattern =
            match pattern with
            | ConstructorPattern([ name ], []) ->
                NamePattern name
            | AsPattern(name, inner) ->
                AsPattern(name, normalizeBinderPattern inner)
            | TypedPattern(inner, typeTokens) ->
                TypedPattern(normalizeBinderPattern inner, typeTokens)
            | ConstructorPattern(name, arguments) ->
                ConstructorPattern(name, arguments |> List.map normalizeBinderPattern)
            | NamedConstructorPattern(name, fields) ->
                NamedConstructorPattern(
                    name,
                    fields
                    |> List.map (fun field ->
                        { field with
                            Pattern = normalizeBinderPattern field.Pattern })
                )
            | TuplePattern elements ->
                TuplePattern(elements |> List.map normalizeBinderPattern)
            | VariantPattern _ ->
                pattern
            | OrPattern alternatives ->
                OrPattern(alternatives |> List.map normalizeBinderPattern)
            | AnonymousRecordPattern(fields, rest) ->
                AnonymousRecordPattern(
                    fields
                    |> List.map (fun field ->
                        { field with
                            Pattern = normalizeBinderPattern field.Pattern })
                    ,
                    rest
                )
            | WildcardPattern
            | NamePattern _
            | LiteralPattern _ ->
                pattern

        match tryParseImplicitLocalBindPattern tokens with
        | Some binding ->
            binding
        | None ->
        match tryParseQuantityPrefix tokens with
        | Some(quantity, rest) ->
            let pattern = parsePattern rest |> normalizeBinderPattern
            makeBindPattern pattern (Some quantity) rest
        | None ->
            let pattern = parsePattern tokens |> normalizeBinderPattern
            makeBindPattern pattern None tokens

    let trySingleName pattern =
        match pattern with
        | NamePattern name -> Some name
        | _ -> None

type private PatternParser(tokens: Token list, source: SourceText, diagnostics: DiagnosticBag, fixities: FixityTable) =
    let eofSpan =
        match List.tryLast tokens with
        | Some token -> token.Span
        | None -> TextSpan.FromBounds(source.Length, source.Length)

    let tokenArray =
        tokens
        @ [ { Kind = EndOfFile
              Text = ""
              Span = eofSpan } ]
        |> List.toArray

    let mutable position = 0

    member private _.Current =
        let index = min position (tokenArray.Length - 1)
        tokenArray[index]

    member private _.Peek(offset: int) =
        let index = min (position + offset) (tokenArray.Length - 1)
        tokenArray[index]

    member private this.Advance() =
        let current = this.Current

        if position < tokenArray.Length - 1 then
            position <- position + 1

        current

    member private this.SkipLayout() =
        while (match this.Current.Kind with
               | Newline
               | Indent
               | Dedent -> true
               | _ -> false) do
            this.Advance() |> ignore

    member private this.IsNameToken(token: Token) =
        Token.isName token
        && not (Token.isKeyword Keyword.If token)
        && not (Token.isKeyword Keyword.Is token)
        && not (Token.isKeyword Keyword.Then token)
        && not (Token.isKeyword Keyword.Else token)
        && not (Token.isKeyword Keyword.In token)
        && not (Token.isKeyword Keyword.Do token)
        && not (Token.isKeyword Keyword.Match token)
        && not (Token.isKeyword Keyword.Case token)
        && not (Token.isKeyword Keyword.Let token)
        && not (Token.isKeyword Keyword.Do token)
        && not (Token.isKeyword Keyword.Match token)
        && not (Token.isKeyword Keyword.Case token)
        && not (Token.isKeyword Keyword.Let token)

    member private this.ParseQualifiedName() =
        this.SkipLayout()

        let segments = ResizeArray<string>()

        if this.IsNameToken(this.Current) then
            segments.Add(SyntaxFacts.trimIdentifierQuotes (this.Advance().Text))

            while this.Current.Kind = Dot && this.IsNameToken(this.Peek(1)) do
                this.Advance() |> ignore
                segments.Add(SyntaxFacts.trimIdentifierQuotes (this.Advance().Text))
        else
            diagnostics.AddError(DiagnosticCode.ParseError, "Expected a pattern name.", source.GetLocation(this.Current.Span))

        List.ofSeq segments

    member private _.IsConstructorSegments(segments: string list) =
        match segments with
        | [] -> false
        | _ :: _ :: _ -> true
        | [ segment ] ->
            not (String.IsNullOrWhiteSpace(segment))
            && (Char.IsUpper(segment[0]) || SyntaxFacts.isOperatorCharacter segment[0])

    member private this.CollectPatternNames(pattern: SurfacePattern) =
        let rec loop current =
            seq {
                match current with
                | NamePattern name ->
                    yield name
                | AsPattern(name, inner) ->
                    yield name
                    yield! loop inner
                | TypedPattern(inner, _) ->
                    yield! loop inner
                | ConstructorPattern(_, arguments) ->
                    for argument in arguments do
                        yield! loop argument
                | NamedConstructorPattern(_, fields) ->
                    for field in fields do
                        yield! loop field.Pattern
                | TuplePattern elements ->
                    for element in elements do
                        yield! loop element
                | VariantPattern(BoundVariantPattern(name, _))
                | VariantPattern(RestVariantPattern name) ->
                    yield name
                | VariantPattern(WildcardVariantPattern _) ->
                    ()
                | OrPattern alternatives ->
                    for alternative in alternatives do
                        yield! loop alternative
                | AnonymousRecordPattern(fields, rest) ->
                    for field in fields do
                        yield! loop field.Pattern
                    match rest with
                    | Some(BindRecordPatternRest name) -> yield name
                    | _ -> ()
                | WildcardPattern
                | LiteralPattern _ ->
                    ()
            }

        loop pattern |> Seq.toList

    member private this.MakeOrPattern(left: SurfacePattern, right: SurfacePattern, operatorToken: Token) =
        let alternatives =
            [ left; right ]
            |> List.collect (function
                | OrPattern items -> items
                | other -> [ other ])

        match alternatives with
        | first :: rest ->
            let firstNames = this.CollectPatternNames(first) |> Set.ofList
            let mismatched =
                rest
                |> List.exists (fun alternative ->
                    (this.CollectPatternNames alternative |> Set.ofList) <> firstNames)

            if mismatched then
                diagnostics.AddError(
                    DiagnosticCode.OrPatternBinderMismatch,
                    "Each or-pattern alternative must bind the same set of names.",
                    source.GetLocation(operatorToken.Span)
                )

            OrPattern alternatives
        | [] ->
            OrPattern []

    member private this.IsAtomicPatternStart(token: Token) =
        match token.Kind with
        | IntegerLiteral
        | FloatLiteral
        | StringLiteral
        | CharacterLiteral
        | LeftParen
        | Underscore -> true
        | _ -> this.IsNameToken(token)

    member private this.ParseLiteralPattern(token: Token) =
        match token.Kind with
        | IntegerLiteral ->
            match SyntaxFacts.tryParseNumericLiteral token.Text with
            | Result.Ok parsed ->
                match parsed.Suffix with
                | Some _ ->
                    diagnostics.AddError(
                        DiagnosticCode.ParseError,
                        $"Numeric literal suffixes are not permitted in patterns: '{token.Text}'.",
                        source.GetLocation(token.Span)
                    )

                    match SurfaceNumericLiteral.tryLowerPrimitiveLiteral parsed.Literal with
                    | Some literal ->
                        LiteralPattern literal
                    | None ->
                        diagnostics.AddError(
                            DiagnosticCode.ParseError,
                            $"Numeric literal '{token.Text}' is not representable in patterns.",
                            source.GetLocation(token.Span)
                        )

                        LiteralPattern(LiteralValue.Integer 0L)
                | None ->
                    match SurfaceNumericLiteral.tryLowerPrimitiveLiteral parsed.Literal with
                    | Some literal ->
                        LiteralPattern literal
                    | None ->
                        diagnostics.AddError(
                            DiagnosticCode.ParseError,
                            $"Numeric literal '{token.Text}' is not representable in patterns.",
                            source.GetLocation(token.Span)
                        )

                        LiteralPattern(LiteralValue.Integer 0L)
            | Result.Error message ->
                diagnostics.AddError(DiagnosticCode.ParseError, message, source.GetLocation(token.Span))
                LiteralPattern(LiteralValue.Integer 0L)
        | FloatLiteral ->
            match SyntaxFacts.tryParseNumericLiteral token.Text with
            | Result.Ok parsed ->
                match parsed.Suffix with
                | Some _ ->
                    diagnostics.AddError(
                        DiagnosticCode.ParseError,
                        $"Numeric literal suffixes are not permitted in patterns: '{token.Text}'.",
                        source.GetLocation(token.Span)
                    )

                    match SurfaceNumericLiteral.tryLowerPrimitiveLiteral parsed.Literal with
                    | Some literal ->
                        LiteralPattern literal
                    | None ->
                        diagnostics.AddError(
                            DiagnosticCode.ParseError,
                            $"Numeric literal '{token.Text}' is not representable in patterns.",
                            source.GetLocation(token.Span)
                        )

                        LiteralPattern(LiteralValue.Float 0.0)
                | None ->
                    match SurfaceNumericLiteral.tryLowerPrimitiveLiteral parsed.Literal with
                    | Some literal ->
                        LiteralPattern literal
                    | None ->
                        diagnostics.AddError(
                            DiagnosticCode.ParseError,
                            $"Numeric literal '{token.Text}' is not representable in patterns.",
                            source.GetLocation(token.Span)
                        )

                        LiteralPattern(LiteralValue.Float 0.0)
            | Result.Error message ->
                diagnostics.AddError(DiagnosticCode.ParseError, message, source.GetLocation(token.Span))
                LiteralPattern(LiteralValue.Float 0.0)
        | StringLiteral ->
            match SyntaxFacts.tryDecodeStringLiteral token.Text with
            | Result.Ok value -> LiteralPattern(LiteralValue.String value)
            | Result.Error message ->
                diagnostics.AddError(DiagnosticCode.ParseError, message, source.GetLocation(token.Span))
                LiteralPattern(LiteralValue.String(SyntaxFacts.trimStringQuotes token.Text))
        | CharacterLiteral ->
            match SyntaxFacts.tryDecodeCharacterLiteral token.Text with
            | Result.Ok value -> LiteralPattern(LiteralValue.Character value)
            | Result.Error message ->
                diagnostics.AddError(DiagnosticCode.ParseError, message, source.GetLocation(token.Span))
                LiteralPattern(LiteralValue.Character '\000')
        | _ ->
            diagnostics.AddError(DiagnosticCode.ParseError, "Expected a literal pattern.", source.GetLocation(token.Span))
            LiteralPattern LiteralValue.Unit

    member private this.CollectParenthesizedTokens() =
        let start = this.Current
        this.Advance() |> ignore

        let innerTokens = ResizeArray<Token>()
        let mutable depth = 1

        while depth > 0 && this.Current.Kind <> EndOfFile do
            match this.Current.Kind with
            | LeftParen ->
                depth <- depth + 1
                innerTokens.Add(this.Advance())
            | RightParen ->
                depth <- depth - 1

                if depth > 0 then
                    innerTokens.Add(this.Advance())
                else
                    this.Advance() |> ignore
            | _ ->
                innerTokens.Add(this.Advance())

        if depth > 0 then
            diagnostics.AddError(DiagnosticCode.ParseError, "Expected ')' to close the pattern.", source.GetLocation(start.Span))

        List.ofSeq innerTokens

    member private _.TrimPatternTokens(tokens: Token list) =
        tokens
        |> List.filter (fun token ->
            match token.Kind with
            | Newline
            | Indent
            | Dedent -> false
            | _ -> true)

    member private _.SplitTopLevelCommas(tokens: Token list) =
        let tokenArray = List.toArray tokens
        let items = ResizeArray<Token list>()
        let current = ResizeArray<Token>()
        let mutable depth = 0
        let mutable sawTopLevelComma = false

        for token in tokenArray do
            match token.Kind with
            | LeftParen
            | LeftBracket
            | LeftBrace
            | LeftSetBrace ->
                depth <- depth + 1
                current.Add(token)
            | RightParen
            | RightBracket
            | RightBrace
            | RightSetBrace ->
                depth <- max 0 (depth - 1)
                current.Add(token)
            | Comma when depth = 0 ->
                sawTopLevelComma <- true
                items.Add(List.ofSeq current)
                current.Clear()
            | _ ->
                current.Add(token)

        items.Add(List.ofSeq current)
        List.ofSeq items, sawTopLevelComma

    member private _.TryFindTopLevelEquals(fieldTokens: Token list) =
        let tokenArray = List.toArray fieldTokens
        let mutable depth = 0
        let mutable index = 0
        let mutable result = None

        while index < tokenArray.Length && result.IsNone do
            match tokenArray[index].Kind with
            | LeftParen
            | LeftBracket
            | LeftBrace
            | LeftSetBrace ->
                depth <- depth + 1
            | RightParen
            | RightBracket
            | RightBrace
            | RightSetBrace ->
                depth <- max 0 (depth - 1)
            | Equals when depth = 0 ->
                result <- Some index
            | _ ->
                ()

            index <- index + 1

        result

    member private _.TryFindTopLevelColon(tokens: Token list) =
        let tokenArray = List.toArray tokens
        let mutable depth = 0
        let mutable index = 0
        let mutable result = None

        while index < tokenArray.Length && result.IsNone do
            match tokenArray[index].Kind with
            | LeftParen
            | LeftBracket
            | LeftBrace
            | LeftSetBrace ->
                depth <- depth + 1
            | RightParen
            | RightBracket
            | RightBrace
            | RightSetBrace ->
                depth <- max 0 (depth - 1)
            | Colon when depth = 0 ->
                result <- Some index
            | _ ->
                ()

            index <- index + 1

        result

    member private this.TryParseVariantPattern(tokens: Token list) =
        let trimmed = this.TrimPatternTokens(tokens)

        match trimmed with
        | { Kind = Operator; Text = "|" } :: rest ->
            match List.rev rest with
            | { Kind = Operator; Text = "|" } :: reversedBody ->
                let body = reversedBody |> List.rev

                match body with
                | [ { Kind = Dot }; { Kind = Dot }; nameToken ] when this.IsNameToken(nameToken) ->
                    Some(VariantPattern(RestVariantPattern(SyntaxFacts.trimIdentifierQuotes nameToken.Text)))
                | [ { Kind = Underscore } ] ->
                    Some(VariantPattern(WildcardVariantPattern None))
                | [ nameToken ] when this.IsNameToken(nameToken) ->
                    Some(VariantPattern(BoundVariantPattern(SyntaxFacts.trimIdentifierQuotes nameToken.Text, None)))
                | _ ->
                    match this.TryFindTopLevelColon body with
                    | Some index ->
                        let tokenArray = List.toArray body
                        let headTokens = tokenArray[0 .. index - 1] |> Array.toList
                        let typeTokens = tokenArray[index + 1 ..] |> Array.toList

                        match headTokens with
                        | [ { Kind = Underscore } ] ->
                            Some(VariantPattern(WildcardVariantPattern(Some typeTokens)))
                        | [ nameToken ] when this.IsNameToken(nameToken) ->
                            Some(VariantPattern(BoundVariantPattern(SyntaxFacts.trimIdentifierQuotes nameToken.Text, Some typeTokens)))
                        | _ ->
                            None
                    | None ->
                        None
            | _ ->
                None
        | _ ->
            None

    member private this.TryParseTuplePattern(tokens: Token list) =
        let trimmed = this.TrimPatternTokens(tokens)
        let groups, sawTopLevelComma = this.SplitTopLevelCommas(trimmed)
        let nonEmptyGroups = groups |> List.filter (List.isEmpty >> not)

        if not sawTopLevelComma then
            None
        elif List.isEmpty nonEmptyGroups then
            None
        else
            let elements =
                nonEmptyGroups
                |> List.map (fun group ->
                    let nestedParser = PatternParser(group, source, diagnostics, fixities)
                    nestedParser.Parse())

            Some(TuplePattern elements)

    member private this.TryParseAnonymousRecordPattern(tokens: Token list) =
        let trimmed = this.TrimPatternTokens(tokens)

        let isRestPattern (fieldTokens: Token list) =
            match fieldTokens with
            | { Kind = Dot } :: { Kind = Dot } :: [] -> true
            | { Kind = Dot } :: { Kind = Dot } :: nameToken :: [] when this.IsNameToken(nameToken) -> true
            | _ -> false

        let fieldGroups =
            this.SplitTopLevelCommas trimmed
            |> fst
            |> List.filter (List.isEmpty >> not)

        if trimmed |> List.exists (fun token -> token.Kind = Equals) || fieldGroups |> List.exists isRestPattern then
            let mutable rest = None

            let fields =
                fieldGroups
                |> List.choose (fun fieldTokens ->
                    match this.TryFindTopLevelEquals fieldTokens with
                    | Some index ->
                        let tokenArray = List.toArray fieldTokens
                        let labelTokens = tokenArray[0 .. index - 1] |> Array.toList
                        let patternTokens = tokenArray[index + 1 ..] |> Array.toList

                        match labelTokens with
                        | [ labelToken ] when this.IsNameToken(labelToken) ->
                            let nestedParser = PatternParser(patternTokens, source, diagnostics, fixities)

                            Some
                                { Name = SyntaxFacts.trimIdentifierQuotes labelToken.Text
                                  IsImplicit = false
                                  Pattern = nestedParser.Parse() }
                        | [ { Kind = AtSign }; labelToken ] when this.IsNameToken(labelToken) ->
                            let nestedParser = PatternParser(patternTokens, source, diagnostics, fixities)

                            Some
                                { Name = SyntaxFacts.trimIdentifierQuotes labelToken.Text
                                  IsImplicit = true
                                  Pattern = nestedParser.Parse() }
                        | labelToken :: _ ->
                            diagnostics.AddError(DiagnosticCode.ParseError, "Expected a record pattern field label.", source.GetLocation(labelToken.Span))
                            Some
                                { Name = "<missing>"
                                  IsImplicit = false
                                  Pattern = WildcardPattern }
                        | [] ->
                            diagnostics.AddError(DiagnosticCode.ParseError, "Expected a record pattern field label.", source.GetLocation(eofSpan))
                            Some
                                { Name = "<missing>"
                                  IsImplicit = false
                                  Pattern = WildcardPattern }
                    | None ->
                        match fieldTokens with
                        | { Kind = Dot } :: { Kind = Dot } :: [] ->
                            rest <- Some DiscardRecordPatternRest
                            None
                        | { Kind = Dot } :: { Kind = Dot } :: nameToken :: [] when this.IsNameToken(nameToken) ->
                            rest <- Some(BindRecordPatternRest(SyntaxFacts.trimIdentifierQuotes nameToken.Text))
                            None
                        | _ ->
                            let errorSpan =
                                match fieldTokens with
                                | token :: _ -> token.Span
                                | [] -> eofSpan

                            diagnostics.AddError(DiagnosticCode.ParseError, "Expected a record pattern field of the form 'name = pattern'.", source.GetLocation(errorSpan))
                            Some
                                { Name = "<missing>"
                                  IsImplicit = false
                                  Pattern = WildcardPattern })

            Some(AnonymousRecordPattern(fields, rest))
        else
            None

    member private this.TryParseTypedPattern(tokens: Token list) =
        let trimmed = this.TrimPatternTokens(tokens)

        match this.TryFindTopLevelColon trimmed with
        | Some index ->
            let tokenArray = List.toArray trimmed
            let patternTokens = tokenArray[0 .. index - 1] |> Array.toList
            let typeTokens = tokenArray[index + 1 ..] |> Array.toList

            if List.isEmpty patternTokens || List.isEmpty typeTokens then
                None
            else
                let nestedParser = PatternParser(patternTokens, source, diagnostics, fixities)
                Some(TypedPattern(nestedParser.Parse(), typeTokens))
        | None ->
            None

    member private this.CollectBracedTokens() =
        let start = this.Current
        this.Advance() |> ignore

        let innerTokens = ResizeArray<Token>()
        let mutable depth = 1

        while depth > 0 && this.Current.Kind <> EndOfFile do
            match this.Current.Kind with
            | LeftBrace ->
                depth <- depth + 1
                innerTokens.Add(this.Advance())
            | RightBrace ->
                depth <- depth - 1

                if depth > 0 then
                    innerTokens.Add(this.Advance())
                else
                    this.Advance() |> ignore
            | _ ->
                innerTokens.Add(this.Advance())

        if depth > 0 then
            diagnostics.AddError(DiagnosticCode.ParseError, "Expected '}' to close the pattern.", source.GetLocation(start.Span))

        List.ofSeq innerTokens

    member private this.ParseNamedConstructorPattern(name: string list) =
        let innerTokens = this.CollectBracedTokens() |> this.TrimPatternTokens
        let fieldGroups =
            this.SplitTopLevelCommas innerTokens
            |> fst
            |> List.filter (List.isEmpty >> not)

        let fields =
            fieldGroups
            |> List.map (fun fieldTokens ->
                match this.TryFindTopLevelEquals fieldTokens with
                | Some index ->
                    let tokenArray = List.toArray fieldTokens
                    let labelTokens = tokenArray[0 .. index - 1] |> Array.toList
                    let patternTokens = tokenArray[index + 1 ..] |> Array.toList

                    match labelTokens with
                    | [ labelToken ] when this.IsNameToken(labelToken) ->
                        let nestedParser = PatternParser(patternTokens, source, diagnostics, fixities)

                        { Name = SyntaxFacts.trimIdentifierQuotes labelToken.Text
                          IsImplicit = false
                          Pattern = nestedParser.Parse() }
                    | [ { Kind = AtSign }; labelToken ] when this.IsNameToken(labelToken) ->
                        let nestedParser = PatternParser(patternTokens, source, diagnostics, fixities)

                        { Name = SyntaxFacts.trimIdentifierQuotes labelToken.Text
                          IsImplicit = true
                          Pattern = nestedParser.Parse() }
                    | labelToken :: _ ->
                        diagnostics.AddError(DiagnosticCode.ParseError, "Expected a named constructor pattern field label.", source.GetLocation(labelToken.Span))
                        { Name = "<missing>"
                          IsImplicit = false
                          Pattern = WildcardPattern }
                    | [] ->
                        diagnostics.AddError(DiagnosticCode.ParseError, "Expected a named constructor pattern field label.", source.GetLocation(eofSpan))
                        { Name = "<missing>"
                          IsImplicit = false
                          Pattern = WildcardPattern }
                | None ->
                    match fieldTokens with
                    | [ labelToken ] when this.IsNameToken(labelToken) ->
                        let name = SyntaxFacts.trimIdentifierQuotes labelToken.Text
                        { Name = name
                          IsImplicit = false
                          Pattern = NamePattern name }
                    | [ { Kind = AtSign }; labelToken ] when this.IsNameToken(labelToken) ->
                        let name = SyntaxFacts.trimIdentifierQuotes labelToken.Text
                        { Name = name
                          IsImplicit = true
                          Pattern = NamePattern name }
                    | token :: _ ->
                        diagnostics.AddError(DiagnosticCode.ParseError, "Expected a named constructor pattern field.", source.GetLocation(token.Span))
                        { Name = "<missing>"
                          IsImplicit = false
                          Pattern = WildcardPattern }
                    | [] ->
                        { Name = "<missing>"
                          IsImplicit = false
                          Pattern = WildcardPattern })

        NamedConstructorPattern(name, fields)

    member private this.ParseAtomicPattern() =
        this.SkipLayout()

        match this.Current.Kind with
        | Underscore ->
            this.Advance() |> ignore
            WildcardPattern
        | IntegerLiteral
        | FloatLiteral
        | StringLiteral
        | CharacterLiteral ->
            this.ParseLiteralPattern(this.Advance())
        | LeftParen ->
            let innerTokens = this.CollectParenthesizedTokens()

            match innerTokens with
            | [] -> LiteralPattern LiteralValue.Unit
            | [ operatorToken ] when operatorToken.Kind = Operator ->
                ConstructorPattern([ operatorToken.Text ], [])
            | _ ->
                match this.TryParseVariantPattern innerTokens with
                | Some pattern -> pattern
                | None ->
                    match this.TryParseAnonymousRecordPattern innerTokens with
                    | Some pattern -> pattern
                    | None ->
                        match this.TryParseTuplePattern innerTokens with
                        | Some pattern -> pattern
                        | None ->
                            match this.TryParseTypedPattern innerTokens with
                            | Some pattern -> pattern
                            | None ->
                                let nestedParser = PatternParser(innerTokens, source, diagnostics, fixities)
                                nestedParser.Parse()
        | _ when this.IsNameToken(this.Current) ->
            let segments = this.ParseQualifiedName()

            if this.IsConstructorSegments(segments) then
                if this.Current.Kind = LeftBrace then
                    this.ParseNamedConstructorPattern(segments)
                else
                    ConstructorPattern(segments, [])
            else
                match segments with
                | [ name ] -> NamePattern name
                | _ -> ConstructorPattern(segments, [])
        | _ ->
            diagnostics.AddError(DiagnosticCode.ParseError, "Expected a pattern.", source.GetLocation(this.Current.Span))
            WildcardPattern

    member private this.ParseApplicationPattern() =
        let head = this.ParseAtomicPattern()
        let arguments = ResizeArray<SurfacePattern>()

        while this.IsAtomicPatternStart(this.Current) do
            arguments.Add(this.ParseAtomicPattern())

        match head, List.ofSeq arguments with
        | ConstructorPattern(name, existingArguments), additionalArguments ->
            ConstructorPattern(name, existingArguments @ additionalArguments)
        | NamedConstructorPattern _, _ :: _ ->
            diagnostics.AddError(DiagnosticCode.ParseError, "Named constructor patterns cannot take positional subpatterns.", source.GetLocation(this.Current.Span))
            head
        | NamePattern name, [] ->
            NamePattern name
        | NamePattern name, additionalArguments ->
            ConstructorPattern([ name ], additionalArguments)
        | literalPattern, [] ->
            literalPattern
        | _, _ ->
            diagnostics.AddError(DiagnosticCode.ParseError, "Only constructor patterns may take arguments.", source.GetLocation(this.Current.Span))
            head

    member private this.ParsePattern(minimumPrecedence: int) =
        let mutable left = this.ParseApplicationPattern()
        let mutable keepParsing = true

        while keepParsing do
            this.SkipLayout()

            match this.Current.Kind with
            | AtSign ->
                match left with
                | NamePattern aliasName when minimumPrecedence <= 2 ->
                    this.Advance() |> ignore
                    let right = this.ParsePattern(2)
                    left <- AsPattern(aliasName, right)
                | _ ->
                    keepParsing <- false
            | Operator when this.Current.Text = "|" ->
                let precedence = 1

                if precedence >= minimumPrecedence then
                    let operatorToken = this.Advance()
                    let right = this.ParsePattern(precedence + 1)
                    left <- this.MakeOrPattern(left, right, operatorToken)
                else
                    keepParsing <- false
            | Operator ->
                let operatorText = this.Current.Text

                match FixityTable.tryFindInfix operatorText fixities with
                | Some(associativity, precedence) when precedence >= minimumPrecedence ->
                    let nextMinimumPrecedence =
                        match associativity with
                        | LeftAssociative
                        | NonAssociative -> precedence + 1
                        | RightAssociative -> precedence

                    this.Advance() |> ignore
                    let right = this.ParsePattern(nextMinimumPrecedence)
                    left <- ConstructorPattern([ operatorText ], [ left; right ])
                | _ ->
                    keepParsing <- false
            | _ ->
                keepParsing <- false

        left

    member this.Parse() =
        this.SkipLayout()

        if this.Current.Kind = EndOfFile then
            WildcardPattern
        else
            let pattern = this.ParsePattern(0)
            this.SkipLayout()

            if this.Current.Kind <> EndOfFile then
                diagnostics.AddError(DiagnosticCode.ParseError, "Unexpected tokens at the end of the pattern.", source.GetLocation(this.Current.Span))

            pattern

type private ExpressionParser(tokens: Token list, source: SourceText, diagnostics: DiagnosticBag, fixities: FixityTable) =
    let eofSpan =
        match List.tryLast tokens with
        | Some token -> token.Span
        | None -> TextSpan.FromBounds(source.Length, source.Length)

    let tokenArray =
        tokens
        @ [ { Kind = EndOfFile
              Text = ""
              Span = eofSpan } ]
        |> List.toArray

    let mutable position = 0
    let mutable syntheticNameCounter = 0

    member private _.Current =
        let index = min position (tokenArray.Length - 1)
        tokenArray[index]

    member private _.Peek(offset: int) =
        let index = min (position + offset) (tokenArray.Length - 1)
        tokenArray[index]

    member private this.Advance() =
        let current = this.Current

        if position < tokenArray.Length - 1 then
            position <- position + 1

        current

    member private this.SkipLayout() =
        while (match this.Current.Kind with
               | Newline
               | Indent
               | Dedent -> true
               | _ -> false) do
            this.Advance() |> ignore

    member private _.FreshSyntheticName(prefix: string) =
        syntheticNameCounter <- syntheticNameCounter + 1
        $"{prefix}\u001f{syntheticNameCounter}"

    member private this.ExpectKeyword(keyword: Keyword, message: string) =
        this.SkipLayout()

        if Token.isKeyword keyword this.Current then
            this.Advance() |> ignore
            true
        else
            diagnostics.AddError(DiagnosticCode.ParseError, message, source.GetLocation(this.Current.Span))
            false

    member private this.IsNameToken(token: Token) =
        Token.isName token
        && not (Token.isKeyword Keyword.If token)
        && not (Token.isKeyword Keyword.Is token)
        && not (Token.isKeyword Keyword.Then token)
        && not (Token.isKeyword Keyword.Else token)
        && not (Token.isKeyword Keyword.In token)

    member private this.IsExpressionStart(token: Token) =
        match token.Kind with
        | IntegerLiteral
        | FloatLiteral
        | StringLiteral
        | InterpolatedStringStart
        | CharacterLiteral
        | LeftParen
        | AtSign
        | Backslash -> true
        | Operator when FixityTable.tryFindPrefix token.Text fixities |> Option.isSome -> true
        | Keyword Keyword.If
        | Keyword Keyword.Seal -> true
        | _ -> this.IsNameToken(token)

    member private this.DecodeStringLiteral(token: Token) =
        match SyntaxFacts.tryDecodeStringLiteral token.Text with
        | Result.Ok value ->
            value
        | Result.Error message ->
            diagnostics.AddError(DiagnosticCode.ParseError, message, source.GetLocation(token.Span))
            SyntaxFacts.trimStringQuotes token.Text

    member private this.DecodeCharacterLiteral(token: Token) =
        match SyntaxFacts.tryDecodeCharacterLiteral token.Text with
        | Result.Ok value ->
            value
        | Result.Error message ->
            diagnostics.AddError(DiagnosticCode.ParseError, message, source.GetLocation(token.Span))
            '\000'

    member private this.DecodeStringTextSegment(token: Token) =
        match SyntaxFacts.tryUnescapeStringContent token.Text with
        | Result.Ok value ->
            value
        | Result.Error message ->
            diagnostics.AddError(DiagnosticCode.ParseError, message, source.GetLocation(token.Span))
            token.Text

    member private this.TryParseStandaloneExpression(tokens: Token list) =
        let nestedDiagnostics = DiagnosticBag()
        let nestedParser = ExpressionParser(tokens, source, nestedDiagnostics, fixities)

        match nestedParser.Parse(), nestedDiagnostics.Items with
        | Some expression, [] ->
            Some expression
        | _ ->
            None

    member private this.TryParseStandaloneExpressionWithMinimumPrecedence(tokens: Token list, minimumPrecedence: int) =
        let nestedDiagnostics = DiagnosticBag()
        let nestedParser = ExpressionParser(tokens, source, nestedDiagnostics, fixities)
        let parsed = nestedParser.ParseExpression(minimumPrecedence)
        nestedParser.SkipLayout()

        match nestedParser.Current.Kind, nestedDiagnostics.Items with
        | EndOfFile, [] ->
            Some parsed
        | _ ->
            None

    member private this.ParseStandaloneExpression(tokens: Token list) =
        let nestedParser = ExpressionParser(tokens, source, diagnostics, fixities)
        nestedParser.Parse() |> Option.defaultValue (Literal LiteralValue.Unit)

    member private this.ParsePatternFromTokens(tokens: Token list) =
        let nestedParser = PatternParser(tokens, source, diagnostics, fixities)
        nestedParser.Parse()

    member private this.ParseBindPatternFromTokens(tokens: Token list) =
        SurfaceBinderParsing.parseBindPatternFromTokens this.ParsePatternFromTokens tokens

    member private this.ParsePureBlockSuite
        (lines: Token list list)
        (emptyMessage: string)
        (malformedMessage: string)
        (errorSpan: TextSpan)
        =
        let tryFindTopLevelEquals tokens =
            let tokenArray = List.toArray tokens
            let mutable depth = 0
            let mutable index = 0
            let mutable result = None

            while index < tokenArray.Length && result.IsNone do
                match tokenArray[index].Kind with
                | LeftParen
                | LeftBracket
                | LeftBrace
                | LeftSetBrace ->
                    depth <- depth + 1
                | RightParen
                | RightBracket
                | RightBrace
                | RightSetBrace ->
                    depth <- max 0 (depth - 1)
                | Equals when depth = 0 ->
                    result <- Some index
                | _ ->
                    ()

                index <- index + 1

            result

        let stripBindingTypeAnnotation tokens =
            let tokenArray = List.toArray tokens
            let mutable depth = 0
            let mutable index = 0
            let mutable splitIndex = -1

            while index < tokenArray.Length && splitIndex < 0 do
                match tokenArray[index].Kind with
                | LeftParen
                | LeftBracket
                | LeftBrace
                | LeftSetBrace ->
                    depth <- depth + 1
                | RightParen
                | RightBracket
                | RightBrace
                | RightSetBrace ->
                    depth <- max 0 (depth - 1)
                | Colon when depth = 0 ->
                    splitIndex <- index
                | _ ->
                    ()

                index <- index + 1

            if splitIndex >= 0 then
                tokenArray[0 .. splitIndex - 1] |> Array.toList
            else
                tokens

        let tryFindTopLevelColon tokens =
            let tokenArray = List.toArray tokens
            let mutable depth = 0
            let mutable index = 0
            let mutable result = None

            while index < tokenArray.Length && result.IsNone do
                match tokenArray[index].Kind with
                | LeftParen
                | LeftBracket
                | LeftBrace
                | LeftSetBrace ->
                    depth <- depth + 1
                | RightParen
                | RightBracket
                | RightBrace
                | RightSetBrace ->
                    depth <- max 0 (depth - 1)
                | Colon when depth = 0 ->
                    result <- Some index
                | _ ->
                    ()

                index <- index + 1

            result

        let trimLineTokens lineTokens =
            let isLeadingLayoutToken token =
                match token.Kind with
                | Newline
                | Indent
                | Dedent -> true
                | _ -> false

            lineTokens
            |> List.skipWhile isLeadingLayoutToken
            |> List.rev
            |> List.skipWhile (fun token -> token.Kind = Newline)
            |> List.rev

        let parseNamedLocalBinding headerTokens valueTokens =
            let headerTokens =
                headerTokens
                |> List.filter (fun token ->
                    match token.Kind with
                    | Newline
                    | Indent
                    | Dedent
                    | EndOfFile -> false
                    | _ -> true)

            let bindingName, bindingNameSpan, parameterHeaderTokens =
                match headerTokens with
                | nameToken :: rest ->
                    SyntaxFacts.trimIdentifierQuotes nameToken.Text, nameToken.Span, rest
                | [] ->
                    "<missing>", TextSpan.FromBounds(source.Length, source.Length), []

            let splitReturnTypeTokens tokens =
                let tokenArray = List.toArray tokens
                let mutable depth = 0
                let mutable splitIndex = -1
                let mutable index = 0

                while index < tokenArray.Length && splitIndex < 0 do
                    match tokenArray[index].Kind with
                    | LeftParen
                    | LeftBracket
                    | LeftBrace
                    | LeftSetBrace ->
                        depth <- depth + 1
                    | RightParen
                    | RightBracket
                    | RightBrace
                    | RightSetBrace ->
                        depth <- max 0 (depth - 1)
                    | Colon when depth = 0 ->
                        splitIndex <- index
                    | _ ->
                        ()

                    index <- index + 1

                if splitIndex >= 0 then
                    tokenArray[0 .. splitIndex - 1] |> Array.toList,
                    Some(tokenArray[splitIndex + 1 ..] |> Array.toList)
                else
                    tokens, None

            let parameterTokens, _ =
                splitReturnTypeTokens parameterHeaderTokens

            let tokenArray = List.toArray parameterTokens
            let parameters = ResizeArray<Parameter>()
            let mutable index = 0

            while index < tokenArray.Length do
                match tokenArray[index].Kind with
                | AtSign ->
                    if index + 1 >= tokenArray.Length then
                        diagnostics.AddError(DiagnosticCode.ParseError, "Expected an implicit parameter after '@' in the local function header.", source.GetLocation(tokenArray[index].Span))
                        index <- tokenArray.Length
                    else
                        match tokenArray[index + 1].Kind with
                        | Identifier
                        | Keyword _ ->
                            parameters.Add(
                                SurfaceBinderParsing.makeParameter
                                    (SyntaxFacts.trimIdentifierQuotes tokenArray[index + 1].Text)
                                    None
                                    None
                                    true
                                    false
                            )

                            index <- index + 2
                        | LeftParen ->
                            let startToken = tokenArray[index + 1]
                            let mutable depth = 1
                            let mutable endIndex = index + 2

                            while endIndex < tokenArray.Length && depth > 0 do
                                match tokenArray[endIndex].Kind with
                                | LeftParen -> depth <- depth + 1
                                | RightParen -> depth <- depth - 1
                                | _ -> ()

                                endIndex <- endIndex + 1

                            if depth > 0 then
                                diagnostics.AddError(DiagnosticCode.ParseError, "Unterminated implicit parameter binder in local function header.", source.GetLocation(startToken.Span))
                                index <- tokenArray.Length
                            else
                                let innerTokens = List.ofArray tokenArray[index + 2 .. endIndex - 2]

                                match SurfaceBinderParsing.parseParameterFromTokens diagnostics source startToken.Span innerTokens with
                                | Some parameter -> parameters.Add({ parameter with IsImplicit = true })
                                | None -> ()

                                index <- endIndex
                        | _ ->
                            diagnostics.AddError(DiagnosticCode.ParseError, "Unsupported implicit parameter syntax in local function header.", source.GetLocation(tokenArray[index + 1].Span))
                            index <- index + 2
                | Identifier
                | Keyword _ ->
                    parameters.Add(
                        SurfaceBinderParsing.makeParameter
                            (SyntaxFacts.trimIdentifierQuotes tokenArray[index].Text)
                            None
                            None
                            false
                            false
                    )

                    index <- index + 1
                | Underscore ->
                    parameters.Add(
                        SurfaceBinderParsing.makeParameter
                            (SurfaceBinderParsing.wildcardParameterName tokenArray[index].Span)
                            None
                            None
                            false
                            false
                    )

                    index <- index + 1
                | LeftParen ->
                    let startToken = tokenArray[index]
                    let mutable depth = 1
                    let mutable endIndex = index + 1

                    while endIndex < tokenArray.Length && depth > 0 do
                        match tokenArray[endIndex].Kind with
                        | LeftParen -> depth <- depth + 1
                        | RightParen -> depth <- depth - 1
                        | _ -> ()

                        endIndex <- endIndex + 1

                    if depth > 0 then
                        diagnostics.AddError(DiagnosticCode.ParseError, "Unterminated parameter binder in local function header.", source.GetLocation(startToken.Span))
                        index <- tokenArray.Length
                    else
                        let innerTokens = List.ofArray tokenArray[index + 1 .. endIndex - 2]

                        if List.isEmpty innerTokens then
                            parameters.Add(
                                SurfaceBinderParsing.makeParameter
                                    (SurfaceBinderParsing.wildcardParameterName startToken.Span)
                                    None
                                    None
                                    false
                                    false
                            )
                        else
                            match SurfaceBinderParsing.parseParameterFromTokens diagnostics source startToken.Span innerTokens with
                            | Some parameter -> parameters.Add(parameter)
                            | None -> ()

                        index <- endIndex
                | _ ->
                    diagnostics.AddError(DiagnosticCode.ParseError, "Unsupported local function header syntax.", source.GetLocation(tokenArray[index].Span))
                    index <- index + 1

            let value =
                match List.ofSeq parameters with
                | [] ->
                    this.ParseStandaloneExpression(valueTokens)
                | parsedParameters ->
                    Lambda(parsedParameters, this.ParseStandaloneExpression(valueTokens))

            let binding =
                { Pattern = NamePattern bindingName
                  Quantity = None
                  IsImplicit = false
                  TypeTokens = None
                  BinderSpans = Map.ofList [ bindingName, [ bindingNameSpan ] ] }

            binding, value

        let parseLocalTypeAlias lineTokens =
            match trimLineTokens lineTokens with
            | typeToken :: nameToken :: rest when Token.isKeyword Keyword.Type typeToken && this.IsNameToken(nameToken) ->
                let aliasName = SyntaxFacts.trimIdentifierQuotes nameToken.Text
                let headerTokens = ResizeArray<Token>()
                let mutable index = 0
                let tokenArray = List.toArray rest

                while index < tokenArray.Length && tokenArray[index].Kind <> Equals do
                    headerTokens.Add(tokenArray[index])
                    index <- index + 1

                let bodyTokens =
                    if index < tokenArray.Length && tokenArray[index].Kind = Equals then
                        Some(tokenArray[index + 1 ..] |> Array.toList)
                    else
                        None

                Some
                    { Visibility = None
                      IsOpaque = false
                      TotalityAssertion = None
                      Name = aliasName
                      HeaderTokens = List.ofSeq headerTokens
                      BodyTokens = bodyTokens }
            | _ ->
                None

        let parseBlockItem lineTokens =
            match trimLineTokens lineTokens with
            | [] ->
                None
            | scopedToken :: effectToken :: nameToken :: rest
                when Token.isName scopedToken
                     && String.Equals(SyntaxFacts.trimIdentifierQuotes scopedToken.Text, "scoped", StringComparison.Ordinal)
                     && Token.isName effectToken
                     && String.Equals(SyntaxFacts.trimIdentifierQuotes effectToken.Text, "effect", StringComparison.Ordinal)
                     && Token.isName nameToken
                     && (rest |> List.exists (fun token -> token.Kind = Equals)) ->
                Some(LocalPureScopedEffect(SyntaxFacts.trimIdentifierQuotes nameToken.Text))
            | typeToken :: _ as tokens when Token.isKeyword Keyword.Type typeToken ->
                parseLocalTypeAlias tokens |> Option.map LocalPureTypeAlias
            | letToken :: rest when Token.isKeyword Keyword.Let letToken ->
                match tryFindTopLevelEquals rest with
                | Some index ->
                    let tokenArray = List.toArray rest
                    let headerTokens = tokenArray[0 .. index - 1] |> Array.toList
                    let valueTokens = tokenArray[index + 1 ..] |> Array.toList

                    let parsed =
                        match headerTokens with
                        | nameToken :: restHeader
                            when this.IsNameToken(nameToken)
                                 && not (List.isEmpty restHeader)
                                 && restHeader.Head.Kind <> Colon ->
                            parseNamedLocalBinding headerTokens valueTokens
                        | _ ->
                            let bindingTokens =
                                headerTokens
                                |> stripBindingTypeAnnotation

                            this.ParseBindPatternFromTokens bindingTokens,
                            this.ParseStandaloneExpression(valueTokens)

                    Some(LocalPureBinding parsed)
                | None ->
                    None
            | nameToken :: rest when this.IsNameToken(nameToken) ->
                match tryFindTopLevelColon rest with
                | Some index ->
                    let tokenArray = List.toArray rest
                    let typeTokens = tokenArray[index + 1 ..] |> Array.toList

                    Some(
                        LocalPureSignature
                            { Visibility = None
                              IsOpaque = false
                              Name = SyntaxFacts.trimIdentifierQuotes nameToken.Text
                              TypeTokens = typeTokens }
                    )
                | None ->
                    None
            | _ ->
                None

        let joinIndentedBodyLines (bodyLines: Token list list) =
            let syntheticNewline span =
                { Kind = Newline
                  Text = ""
                  Span = span }

            bodyLines
            |> List.mapi (fun index line ->
                if index = 0 || List.isEmpty line then
                    line
                else
                    syntheticNewline line.Head.Span :: line)
            |> List.concat

        let buildNestedExpression items bodyTokens =
            let body = this.ParseStandaloneExpression(bodyTokens)

            items
            |> List.rev
            |> List.fold (fun (current: SurfaceExpression) item ->
                match item with
                | LocalPureBinding(binding, value) ->
                    LocalLet(binding, value, current)
                | LocalPureSignature declaration ->
                    LocalSignature(declaration, current)
                | LocalPureTypeAlias declaration ->
                    LocalTypeAlias(declaration, current)
                | LocalPureScopedEffect name ->
                    LocalScopedEffect(name, current)) body

        match lines with
        | [] ->
            diagnostics.AddError(DiagnosticCode.ParseError, emptyMessage, source.GetLocation(errorSpan))
            Literal LiteralValue.Unit
        | _ ->
            let parsedItems =
                lines
                |> List.take (List.length lines - 1)
                |> List.map parseBlockItem

            if parsedItems |> List.forall Option.isSome then
                buildNestedExpression (parsedItems |> List.choose id) (List.last lines)
            else
                let parsedLines =
                    lines
                    |> List.map (fun line -> line, parseBlockItem line)

                let bindingLines, bodyLines =
                    parsedLines |> List.takeWhile (snd >> Option.isSome),
                    parsedLines |> List.skipWhile (snd >> Option.isSome)

                let startsStructuredFinalExpression =
                    match bodyLines with
                    | (lineTokens, _) :: _ ->
                        match trimLineTokens lineTokens with
                        | head :: _ when Token.isKeyword Keyword.Match head || Token.isKeyword Keyword.If head ->
                            true
                        | _ ->
                            false
                    | [] ->
                        false

                if not (List.isEmpty bindingLines)
                   && not (List.isEmpty bodyLines)
                   && startsStructuredFinalExpression then
                    buildNestedExpression (bindingLines |> List.choose snd) (bodyLines |> List.map fst |> joinIndentedBodyLines)
                else
                    diagnostics.AddError(DiagnosticCode.ParseError, malformedMessage, source.GetLocation(errorSpan))
                    Literal LiteralValue.Unit

    member private this.MakeOperatorSection body =
        let parameterName = this.FreshSyntheticName "__sectionArg"

        Lambda(
            [ SurfaceBinderParsing.makeParameter parameterName None None false false ],
            body
        )

    member private this.MakeOperatorFunction operatorName =
        let leftName = this.FreshSyntheticName "__sectionLeft"
        let rightName = this.FreshSyntheticName "__sectionRight"

        Lambda(
            [ SurfaceBinderParsing.makeParameter leftName None None false false
              SurfaceBinderParsing.makeParameter rightName None None false false ],
            Binary(Name [ leftName ], operatorName, Name [ rightName ])
        )

    member private _.MakeReceiverSection body =
        let parameterName = "__receiverSection"

        Lambda(
            [ SurfaceBinderParsing.makeParameter parameterName None None false false ],
            body
        )

    member private this.SplitReceiverSectionArgumentTokens(tokens: Token list) =
        let rec takeBalanced closing remaining current depth =
            match remaining with
            | [] ->
                List.rev current, []
            | token :: rest ->
                let nextDepth =
                    match token.Kind with
                    | LeftParen
                    | LeftBrace
                    | LeftBracket
                    | LeftSetBrace -> depth + 1
                    | kind when kind = closing -> depth - 1
                    | _ -> depth

                let nextCurrent = token :: current

                if nextDepth = 0 then
                    List.rev nextCurrent, rest
                else
                    takeBalanced closing rest nextCurrent nextDepth

        let rec loop remaining =
            match remaining with
            | [] ->
                []
            | token :: rest ->
                let chunk, tail =
                    match token.Kind with
                    | LeftParen -> takeBalanced RightParen rest [ token ] 1
                    | LeftBrace -> takeBalanced RightBrace rest [ token ] 1
                    | LeftBracket -> takeBalanced RightBracket rest [ token ] 1
                    | LeftSetBrace -> takeBalanced RightSetBrace rest [ token ] 1
                    | _ -> [ token ], rest

                chunk :: loop tail

        loop tokens

    member private this.TryParseOperatorSection(tokens: Token list) =
        match tokens with
        | [] ->
            None
        | [ operatorToken ] when operatorToken.Kind = Operator ->
            match FixityTable.tryFindPrefix operatorToken.Text fixities, FixityTable.tryFindInfix operatorToken.Text fixities with
            | Some _, None ->
                let parameterName = this.FreshSyntheticName "__sectionPrefix"

                Some(
                    Lambda(
                        [ SurfaceBinderParsing.makeParameter parameterName None None false false ],
                        Unary(operatorToken.Text, Name [ parameterName ])
                    )
                )
            | _, Some _ ->
                Some(this.MakeOperatorFunction operatorToken.Text)
            | _ ->
                None
        | operatorToken :: operandTokens when operatorToken.Kind = Operator ->
            match FixityTable.tryFindPrefix operatorToken.Text fixities, FixityTable.tryFindInfix operatorToken.Text fixities with
            | Some _, _ ->
                None
            | None, Some(_, precedence) ->
                this.TryParseStandaloneExpressionWithMinimumPrecedence(operandTokens, precedence + 1)
                |> Option.map (fun operand ->
                    let parameterName = this.FreshSyntheticName "__sectionArg"

                    Lambda(
                        [ SurfaceBinderParsing.makeParameter parameterName None None false false ],
                        Binary(Name [ parameterName ], operatorToken.Text, operand)
                    ))
            | None, None ->
                None
        | _ ->
            match List.rev tokens with
            | operatorToken :: prefixTokens when operatorToken.Kind = Operator ->
                match FixityTable.tryFindPostfix operatorToken.Text fixities, FixityTable.tryFindInfix operatorToken.Text fixities with
                | Some _, _ ->
                    None
                | None, Some(_, precedence) ->
                    this.TryParseStandaloneExpressionWithMinimumPrecedence(List.rev prefixTokens, precedence + 1)
                    |> Option.map (fun left ->
                        let parameterName = this.FreshSyntheticName "__sectionArg"

                        Lambda(
                            [ SurfaceBinderParsing.makeParameter parameterName None None false false ],
                            Binary(left, operatorToken.Text, Name [ parameterName ])
                        ))
                | None, None ->
                    None
            | _ ->
                None

    member private this.TryParseReceiverSection(tokens: Token list) =
        let rec collectPath path remaining =
            match remaining with
            | { Kind = Dot } :: memberToken :: rest when this.IsNameToken(memberToken) ->
                collectPath (path @ [ SyntaxFacts.trimIdentifierQuotes memberToken.Text ]) rest
            | _ ->
                path, remaining

        match tokens with
        | { Kind = Dot } :: memberToken :: rest when this.IsNameToken(memberToken) ->
            let path, argumentTokens =
                collectPath [ SyntaxFacts.trimIdentifierQuotes memberToken.Text ] rest

            let arguments =
                argumentTokens
                |> this.SplitReceiverSectionArgumentTokens
                |> List.map this.TryParseStandaloneExpression

            if arguments |> List.forall Option.isSome then
                let receiverMember = Name("__receiverSection" :: path)
                let body =
                    match arguments |> List.choose id with
                    | [] -> receiverMember
                    | parsedArguments -> Apply(receiverMember, parsedArguments)

                Some(this.MakeReceiverSection body)
            else
                None
        | _ ->
            None

    member private this.CollectParenthesizedTokens() =
        let start = this.Current
        this.Advance() |> ignore

        let innerTokens = ResizeArray<Token>()
        let mutable depth = 1

        while depth > 0 && this.Current.Kind <> EndOfFile do
            match this.Current.Kind with
            | LeftParen ->
                depth <- depth + 1
                innerTokens.Add(this.Advance())
            | RightParen ->
                depth <- depth - 1

                if depth > 0 then
                    innerTokens.Add(this.Advance())
                else
                    this.Advance() |> ignore
            | _ ->
                innerTokens.Add(this.Advance())

        if depth > 0 then
            diagnostics.AddError(DiagnosticCode.ParseError, "Expected ')' to close the expression.", source.GetLocation(start.Span))

        List.ofSeq innerTokens

    member private this.CollectBracedTokens(errorMessage: string) =
        let start = this.Current
        this.Advance() |> ignore

        let innerTokens = ResizeArray<Token>()
        let mutable depth = 1

        while depth > 0 && this.Current.Kind <> EndOfFile do
            match this.Current.Kind with
            | LeftBrace ->
                depth <- depth + 1
                innerTokens.Add(this.Advance())
            | RightBrace ->
                depth <- depth - 1

                if depth > 0 then
                    innerTokens.Add(this.Advance())
                else
                    this.Advance() |> ignore
            | _ ->
                innerTokens.Add(this.Advance())

        if depth > 0 then
            diagnostics.AddError(DiagnosticCode.ParseError, errorMessage, source.GetLocation(start.Span))

        List.ofSeq innerTokens

    member private this.ParseQualifiedName() =
        this.SkipLayout()

        let segments = ResizeArray<string>()

        if this.IsNameToken(this.Current) then
            segments.Add(SyntaxFacts.trimIdentifierQuotes (this.Advance().Text))

            while this.Current.Kind = Dot && this.IsNameToken(this.Peek(1)) do
                this.Advance() |> ignore
                segments.Add(SyntaxFacts.trimIdentifierQuotes (this.Advance().Text))
        else
            diagnostics.AddError(DiagnosticCode.ParseError, "Expected a name.", source.GetLocation(this.Current.Span))

        List.ofSeq segments

    member private this.ParseParameterFromTokens(tokens: Token list) =
        SurfaceBinderParsing.parseParameterFromTokens diagnostics source eofSpan tokens

    member private this.ParseParenthesizedBinder() =
        let start = this.Current
        this.Advance() |> ignore

        let innerTokens = ResizeArray<Token>()
        let mutable depth = 1

        while depth > 0 && this.Current.Kind <> EndOfFile do
            match this.Current.Kind with
            | LeftParen ->
                depth <- depth + 1
                innerTokens.Add(this.Advance())
            | RightParen ->
                depth <- depth - 1

                if depth > 0 then
                    innerTokens.Add(this.Advance())
                else
                    this.Advance() |> ignore
            | _ ->
                innerTokens.Add(this.Advance())

        if depth > 0 then
            diagnostics.AddError(DiagnosticCode.ParseError, "Unterminated parameter binder.", source.GetLocation(start.Span))
            None
        else
            let tokens = List.ofSeq innerTokens

            if List.isEmpty tokens then
                Some(
                    SurfaceBinderParsing.makeParameter
                        (SurfaceBinderParsing.wildcardParameterName start.Span)
                        None
                        None
                        false
                        false
                )
            else
                this.ParseParameterFromTokens(tokens)

    member private this.ParseLambdaParameters() =
        let parameters = ResizeArray<Parameter>()
        let mutable keepReading = true

        while keepReading && this.Current.Kind <> EndOfFile do
            this.SkipLayout()

            match this.Current.Kind with
            | Arrow ->
                keepReading <- false
            | LeftParen ->
                match this.ParseParenthesizedBinder() with
                | Some parameter -> parameters.Add(parameter)
                | None -> keepReading <- false
            | Underscore ->
                let token = this.Advance()

                parameters.Add(
                    SurfaceBinderParsing.makeParameter
                        (SurfaceBinderParsing.wildcardParameterName token.Span)
                        None
                        None
                        false
                        false
                )
            | _ when this.IsNameToken(this.Current) ->
                parameters.Add(
                    SurfaceBinderParsing.makeParameter
                        (SyntaxFacts.trimIdentifierQuotes (this.Advance().Text))
                        None
                        None
                        false
                        false
                )
            | _ ->
                diagnostics.AddError(DiagnosticCode.ParseError, "Expected a lambda parameter or '->'.", source.GetLocation(this.Current.Span))
                keepReading <- false

        if parameters.Count = 0 then
            diagnostics.AddError(DiagnosticCode.ParseError, "A lambda must declare at least one parameter.", source.GetLocation(this.Current.Span))

        List.ofSeq parameters

    member private this.ParseIfExpression() =
        this.ExpectKeyword(Keyword.If, "Expected 'if'.") |> ignore
        let condition = this.ParseExpression(0)
        this.ExpectKeyword(Keyword.Then, "Expected 'then' in the if expression.") |> ignore
        let whenTrue = this.ParseExpression(0)
        this.ExpectKeyword(Keyword.Else, "Expected 'else' in the if expression.") |> ignore
        let whenFalse = this.ParseExpression(0)
        IfThenElse(condition, whenTrue, whenFalse)

    member private this.ParseBlockExpression() =
        let blockToken = this.Advance()
        let lines =
            this.CollectIndentedLines()
            |> List.filter (List.isEmpty >> not)
        this.ParsePureBlockSuite
            lines
            "A pure block must contain at least one expression."
            "Expected a sequence of local declarations followed by a final expression in the block."
            blockToken.Span

    member private this.CollectIndentedLines() =
        let lines = ResizeArray<Token list>()

        if this.Current.Kind = Newline && this.Peek(1).Kind = Indent then
            this.Advance() |> ignore
            this.Advance() |> ignore

            let currentLine = ResizeArray<Token>()
            let mutable nestedIndents = 0

            let flushLine () =
                if currentLine.Count > 0 then
                    lines.Add(List.ofSeq currentLine)
                    currentLine.Clear()

            while not (this.Current.Kind = Dedent && nestedIndents = 0) && this.Current.Kind <> EndOfFile do
                match this.Current.Kind with
                | Newline when nestedIndents = 0 && this.Peek(1).Kind = Indent ->
                    currentLine.Add(this.Advance())
                    nestedIndents <- nestedIndents + 1
                    currentLine.Add(this.Advance())
                | Newline when nestedIndents = 0 ->
                    flushLine ()
                    this.Advance() |> ignore
                | Indent ->
                    nestedIndents <- nestedIndents + 1
                    currentLine.Add(this.Advance())
                | Dedent when nestedIndents > 0 ->
                    nestedIndents <- nestedIndents - 1
                    currentLine.Add(this.Advance())

                    if nestedIndents = 0 && not (Token.isKeyword Keyword.Else this.Current) then
                        flushLine ()
                | _ ->
                    currentLine.Add(this.Advance())

            flushLine ()

            if this.Current.Kind = Dedent then
                this.Advance() |> ignore
            else
                diagnostics.AddError(DiagnosticCode.ParseError, "Expected the do block to dedent.", source.GetLocation(this.Current.Span))
        else
            let inlineTokens = ResizeArray<Token>()

            while this.Current.Kind <> EndOfFile do
                inlineTokens.Add(this.Advance())

            if inlineTokens.Count > 0 then
                lines.Add(List.ofSeq inlineTokens)

        List.ofSeq lines

    member private this.SplitCasePatternTokens() =
        let tokens = ResizeArray<Token>()
        let mutable depth = 0
        let mutable result = None
        let mutable keepReading = true

        while keepReading && this.Current.Kind <> EndOfFile do
            match this.Current.Kind with
            | Arrow when depth = 0 ->
                result <- Some(List.ofSeq tokens)
                this.Advance() |> ignore
                keepReading <- false
            | LeftParen ->
                depth <- depth + 1
                tokens.Add(this.Advance())
            | RightParen ->
                depth <- max 0 (depth - 1)
                tokens.Add(this.Advance())
            | _ ->
                tokens.Add(this.Advance())

        match result with
        | Some tokens -> tokens
        | None ->
            diagnostics.AddError(DiagnosticCode.ParseError, "Expected '->' in the case clause.", source.GetLocation(this.Current.Span))
            List.ofSeq tokens

    member private this.CollectCurrentIndentedExpressionTokens() =
        let tokens = ResizeArray<Token>()

        if this.Current.Kind = Indent then
            this.Advance() |> ignore
            let mutable nestedIndents = 0

            while not (this.Current.Kind = Dedent && nestedIndents = 0) && this.Current.Kind <> EndOfFile do
                match this.Current.Kind with
                | Indent ->
                    nestedIndents <- nestedIndents + 1
                    tokens.Add(this.Advance())
                | Dedent when nestedIndents > 0 ->
                    nestedIndents <- nestedIndents - 1
                    tokens.Add(this.Advance())
                | _ ->
                    tokens.Add(this.Advance())

            if this.Current.Kind = Dedent then
                this.Advance() |> ignore
            else
                diagnostics.AddError(DiagnosticCode.ParseError, "Expected the indented expression to dedent.", source.GetLocation(this.Current.Span))

        List.ofSeq tokens

    member private this.ParseMatchExpression() =
        this.ExpectKeyword(Keyword.Match, "Expected 'match'.") |> ignore
        let scrutineeTokens = ResizeArray<Token>()

        while this.Current.Kind <> Newline && this.Current.Kind <> EndOfFile do
            scrutineeTokens.Add(this.Advance())

        let scrutinee = this.ParseStandaloneExpression(List.ofSeq scrutineeTokens)

        if this.Current.Kind = Newline then
            this.Advance() |> ignore

        let hasIndentedCases =
            if this.Current.Kind = Indent then
                this.Advance() |> ignore
                true
            else
                false

        let cases = ResizeArray<SurfaceMatchCase>()

        let tryFindTopLevelTokenIndex predicate (tokens: Token array) =
            let mutable depth = 0
            let mutable result = None
            let mutable index = 0

            while result.IsNone && index < tokens.Length do
                match tokens[index].Kind with
                | LeftParen
                | LeftBracket
                | LeftBrace
                | LeftSetBrace ->
                    depth <- depth + 1
                | RightParen
                | RightBracket
                | RightBrace
                | RightSetBrace ->
                    depth <- max 0 (depth - 1)
                | _ when depth = 0 && predicate tokens[index] ->
                    result <- Some index
                | _ ->
                    ()

                index <- index + 1

            result

        while Token.isKeyword Keyword.Case this.Current do
            this.Advance() |> ignore

            let lineTokens = ResizeArray<Token>()

            while this.Current.Kind <> Newline && this.Current.Kind <> EndOfFile do
                lineTokens.Add(this.Advance())

            if this.Current.Kind = Newline then
                this.Advance() |> ignore

            let tokenArray = lineTokens |> Seq.toArray
            let splitIndex = tryFindTopLevelTokenIndex (fun token -> token.Kind = Arrow) tokenArray

            match splitIndex with
            | Some index ->
                let caseHeadTokens = tokenArray[0 .. index - 1]
                let guardIndex =
                    tryFindTopLevelTokenIndex (fun token -> Token.isKeyword Keyword.If token) caseHeadTokens

                let patternTokens, guardTokens =
                    match guardIndex with
                    | Some guardIndex ->
                        caseHeadTokens[0 .. guardIndex - 1] |> Array.toList,
                        Some(caseHeadTokens[guardIndex + 1 ..] |> Array.toList)
                    | None ->
                        List.ofArray caseHeadTokens, None

                let bodyTokens = tokenArray[index + 1 ..] |> Array.toList
                let pattern = this.ParsePatternFromTokens(patternTokens)
                let guard = guardTokens |> Option.map this.ParseStandaloneExpression
                let body =
                    match bodyTokens with
                    | [ doToken ] when Token.isKeyword Keyword.Do doToken && this.Current.Kind = LeftBrace ->
                        diagnostics.AddError(
                            DiagnosticCode.ParseError,
                            "Explicit braces are not permitted after a layout-introduced block.",
                            source.GetLocation(this.Current.Span)
                        )

                        while this.Current.Kind <> Newline && this.Current.Kind <> EndOfFile do
                            this.Advance() |> ignore

                        if this.Current.Kind = Newline then
                            this.Advance() |> ignore

                        Literal LiteralValue.Unit
                    | [] when this.Current.Kind = Indent ->
                        this.ParseStandaloneExpression(this.CollectCurrentIndentedExpressionTokens())
                    | [] when this.Current.Kind = Dedent && not (Token.isKeyword Keyword.Case (this.Peek(1))) ->
                        diagnostics.AddError(
                            DiagnosticCode.UnexpectedIndentation,
                            "Unexpected indentation.",
                            source.GetLocation(this.Peek(1).Span)
                        )

                        this.Advance() |> ignore

                        while this.Current.Kind <> Newline && this.Current.Kind <> EndOfFile do
                            this.Advance() |> ignore

                        if this.Current.Kind = Newline then
                            this.Advance() |> ignore

                        if this.Current.Kind = Indent then
                            this.Advance() |> ignore

                        Literal LiteralValue.Unit
                    | _ ->
                        this.ParseStandaloneExpression(bodyTokens)

                cases.Add(
                    { Pattern = pattern
                      Guard = guard
                      Body = body }
                )

                while this.Current.Kind = Newline do
                    this.Advance() |> ignore
            | None ->
                diagnostics.AddError(DiagnosticCode.ParseError, "Expected '->' in the case clause.", source.GetLocation(this.Current.Span))

        if cases.Count = 0 then
            diagnostics.AddError(DiagnosticCode.ParseError, "A match expression must declare at least one case.", source.GetLocation(this.Current.Span))

        if hasIndentedCases then
            if this.Current.Kind = Dedent then
                this.Advance() |> ignore
            else
                diagnostics.AddError(DiagnosticCode.ParseError, "Expected the match cases to dedent.", source.GetLocation(this.Current.Span))

        Match(scrutinee, List.ofSeq cases)

    member private this.ParseDoLine(lineTokens: Token list) =
        let tryFindTopLevelSplit (tokens: Token list) predicate =
            let tokenArray = List.toArray tokens
            let mutable depth = 0
            let mutable index = 0
            let mutable splitIndex = -1

            while index < tokenArray.Length && splitIndex = -1 do
                match tokenArray[index].Kind with
                | LeftParen
                | LeftBracket
                | LeftBrace
                | LeftSetBrace ->
                    depth <- depth + 1
                | RightParen
                | RightBracket
                | RightBrace
                | RightSetBrace ->
                    depth <- max 0 (depth - 1)
                | _ when depth = 0 && predicate tokenArray[index] ->
                    splitIndex <- index
                | _ ->
                    ()

                index <- index + 1

            if splitIndex < 0 then
                None
            else
                Some(tokenArray, splitIndex)

        let parseDoBlockTokens (tokens: Token list) =
            let lines = ResizeArray<Token list>()
            let tokenArray = List.toArray tokens
            let mutable position = 0

            let current () =
                if position < tokenArray.Length then
                    tokenArray[position]
                else
                    { Kind = EndOfFile
                      Text = ""
                      Span = if Array.isEmpty tokenArray then TextSpan.FromBounds(source.Length, source.Length) else tokenArray[tokenArray.Length - 1].Span }

            let advance () =
                let token = current ()

                if position < tokenArray.Length then
                    position <- position + 1

                token

            let flushLine (currentLine: ResizeArray<Token>) =
                if currentLine.Count > 0 then
                    lines.Add(List.ofSeq currentLine)
                    currentLine.Clear()

            if position < tokenArray.Length && (current ()).Kind = Newline && position + 1 < tokenArray.Length && tokenArray[position + 1].Kind = Indent then
                advance () |> ignore
                advance () |> ignore

                let currentLine = ResizeArray<Token>()
                let mutable nestedIndents = 0

                while not ((current ()).Kind = Dedent && nestedIndents = 0) && (current ()).Kind <> EndOfFile do
                    match (current ()).Kind with
                    | Newline when nestedIndents = 0 && position + 1 < tokenArray.Length && tokenArray[position + 1].Kind = Indent ->
                        currentLine.Add(advance ())
                        nestedIndents <- nestedIndents + 1
                        currentLine.Add(advance ())
                    | Newline when nestedIndents = 0 ->
                        flushLine currentLine
                        advance () |> ignore
                    | Indent ->
                        nestedIndents <- nestedIndents + 1
                        currentLine.Add(advance ())
                    | Dedent when nestedIndents > 0 ->
                        nestedIndents <- nestedIndents - 1
                        currentLine.Add(advance ())

                        if nestedIndents = 0 && not (Token.isKeyword Keyword.Else (current ())) then
                            flushLine currentLine
                    | _ ->
                        currentLine.Add(advance ())

                flushLine currentLine

                if (current ()).Kind = Dedent then
                    advance () |> ignore
                else
                    diagnostics.AddError(DiagnosticCode.ParseError, "Expected the while body to dedent.", source.GetLocation((current ()).Span))
            else
                let inlineTokens =
                    tokens
                    |> List.filter (fun token ->
                        match token.Kind with
                        | Newline
                        | Indent
                        | Dedent -> false
                        | _ -> true)

                if not (List.isEmpty inlineTokens) then
                    lines.Add(inlineTokens)

            lines
            |> Seq.toList
            |> List.filter (List.isEmpty >> not)
            |> List.map this.ParseDoLine

        let stripBindingTypeAnnotation (tokens: Token list) =
            match tryFindTopLevelSplit tokens (fun token -> token.Kind = Colon) with
            | Some(tokenArray, splitIndex) ->
                tokenArray[0 .. splitIndex - 1] |> Array.toList
            | None ->
                tokens

        let parseLetQuestion (letQuestionToken: Token) (rest: Token list) =
            match tryFindTopLevelSplit rest (fun token -> token.Kind = Equals) with
            | Some(tokenArray, equalsIndex) ->
                let patternTokens = tokenArray[0 .. equalsIndex - 1] |> Array.toList
                let afterEquals = tokenArray[equalsIndex + 1 ..] |> Array.toList

                let valueTokens, failure =
                    match tryFindTopLevelSplit afterEquals (fun token -> Token.isKeyword Keyword.Else token) with
                    | Some(afterEqualsArray, elseIndex) ->
                        let valueTokens = afterEqualsArray[0 .. elseIndex - 1] |> Array.toList
                        let failureTokens = afterEqualsArray[elseIndex + 1 ..] |> Array.toList

                        match tryFindTopLevelSplit failureTokens (fun token -> token.Kind = Arrow) with
                        | Some(failureArray, arrowIndex) ->
                            let residuePatternTokens = failureArray[0 .. arrowIndex - 1] |> Array.toList
                            let bodyTokens = failureArray[arrowIndex + 1 ..] |> Array.toList
                            let body =
                                match bodyTokens with
                                | doToken :: rest when Token.isKeyword Keyword.Do doToken ->
                                    parseDoBlockTokens rest
                                | _ ->
                                    [ DoExpression(this.ParseStandaloneExpression(bodyTokens)) ]

                            valueTokens,
                            Some
                                { ResiduePattern = this.ParseBindPatternFromTokens residuePatternTokens
                                  Body = body }
                        | None ->
                            diagnostics.AddError(DiagnosticCode.ParseError, "Expected '->' in the let? failure arm.", source.GetLocation(letQuestionToken.Span))
                            valueTokens, None
                    | None ->
                        afterEquals, None

                DoLetQuestion(this.ParseBindPatternFromTokens patternTokens, this.ParseStandaloneExpression(valueTokens), failure)
            | None ->
                diagnostics.AddError(DiagnosticCode.ParseError, "Expected '=' in the let? binding.", source.GetLocation(letQuestionToken.Span))
                DoExpression(Literal LiteralValue.Unit)

        let parseDoIf (ifToken: Token) (rest: Token list) =
            match tryFindTopLevelSplit rest (fun token -> Token.isKeyword Keyword.Then token) with
            | Some(tokenArray, thenIndex) ->
                let conditionTokens = tokenArray[0 .. thenIndex - 1] |> Array.toList
                let afterThen = tokenArray[thenIndex + 1 ..] |> Array.toList
                let whenTrueTokens, whenFalseTokens =
                    match tryFindTopLevelSplit afterThen (fun token -> Token.isKeyword Keyword.Else token) with
                    | Some(afterThenArray, elseIndex) ->
                        afterThenArray[0 .. elseIndex - 1] |> Array.toList,
                        afterThenArray[elseIndex + 1 ..] |> Array.toList
                    | None ->
                        afterThen, []

                DoIf(
                    this.ParseStandaloneExpression(conditionTokens),
                    parseDoBlockTokens whenTrueTokens,
                    if List.isEmpty whenFalseTokens then [] else parseDoBlockTokens whenFalseTokens
                )
            | None ->
                diagnostics.AddError(DiagnosticCode.ParseError, "Expected 'then' in the do if statement.", source.GetLocation(ifToken.Span))
                DoExpression(Literal LiteralValue.Unit)

        match lineTokens with
        | varToken :: nameToken :: equalsToken :: rest
            when Token.isKeyword Keyword.Var varToken && Token.isName nameToken && equalsToken.Kind = Equals ->
            let name = SyntaxFacts.trimIdentifierQuotes nameToken.Text
            DoVar(name, this.ParseStandaloneExpression(rest))
        | returnToken :: rest when Token.isKeyword Keyword.Return returnToken ->
            DoReturn(this.ParseStandaloneExpression(rest))
        | deferToken :: rest when Token.isKeyword Keyword.Defer deferToken ->
            if List.isEmpty rest then
                diagnostics.AddError(DiagnosticCode.ParseError, "Expected an expression after 'defer'.", source.GetLocation(deferToken.Span))
                DoExpression(Literal LiteralValue.Unit)
            else
                DoDefer(this.ParseStandaloneExpression(rest))
        | ifToken :: rest when Token.isKeyword Keyword.If ifToken ->
            parseDoIf ifToken rest
        | usingToken :: rest when Token.isKeyword Keyword.Using usingToken ->
            match tryFindTopLevelSplit rest (fun token -> token.Kind = Operator && token.Text = "<-") with
            | Some(tokenArray, splitIndex) ->
                let patternTokens = tokenArray[0 .. splitIndex - 1] |> Array.toList

                let patternTokens =
                    match SurfaceBinderParsing.tryParseQuantityPrefix patternTokens with
                    | Some(_, restPatternTokens) ->
                        let location =
                            match patternTokens with
                            | head :: _ -> head.Span
                            | [] -> usingToken.Span

                        diagnostics.AddError(
                            DiagnosticCode.QttUsingExplicitQuantity,
                            "'using' binds its pattern at borrowed quantity '&'; explicit quantity markers are not permitted.",
                            source.GetLocation(location)
                        )

                        restPatternTokens
                    | None ->
                        patternTokens

                if List.isEmpty patternTokens then
                    diagnostics.AddError(DiagnosticCode.ParseError, 
                        "Expected a pattern in the using binding.",
                        source.GetLocation(usingToken.Span)
                    )

                let expressionTokens = tokenArray[splitIndex + 1 ..] |> Array.toList
                let pattern = this.ParsePatternFromTokens patternTokens
                let binding = SurfaceBinderParsing.makeBindPattern pattern (Some(QuantityBorrow None)) patternTokens
                DoUsing(binding, this.ParseStandaloneExpression(expressionTokens))
            | None ->
                diagnostics.AddError(DiagnosticCode.ParseError, "Expected '<-' in the using binding.", source.GetLocation(usingToken.Span))
                DoExpression(Literal LiteralValue.Unit)
        | letQuestionToken :: rest when Token.isKeyword Keyword.LetQuestion letQuestionToken ->
            parseLetQuestion letQuestionToken rest
        | letToken :: questionToken :: rest
            when Token.isKeyword Keyword.Let letToken && questionToken.Kind = Operator && questionToken.Text = "?" ->
            parseLetQuestion letToken rest
        | letToken :: rest when Token.isKeyword Keyword.Let letToken ->
            match
                tryFindTopLevelSplit
                    rest
                    (fun token -> token.Kind = Equals || (token.Kind = Operator && token.Text = "<-"))
            with
            | Some(tokenArray, splitIndex) ->
                let bindingTokens =
                    tokenArray[0 .. splitIndex - 1]
                    |> Array.toList
                    |> stripBindingTypeAnnotation

                let expressionTokens = tokenArray[splitIndex + 1 ..] |> Array.toList
                let binding = this.ParseBindPatternFromTokens bindingTokens

                if tokenArray[splitIndex].Kind = Operator && tokenArray[splitIndex].Text = "<-" then
                    DoBind(binding, this.ParseStandaloneExpression(expressionTokens))
                else
                    DoLet(binding, this.ParseStandaloneExpression(expressionTokens))
            | None ->
                diagnostics.AddError(DiagnosticCode.ParseError, "Expected '=' or '<-' in the do binding.", source.GetLocation(letToken.Span))
                DoExpression(Literal LiteralValue.Unit)
        | whileToken :: rest when Token.isKeyword Keyword.While whileToken ->
            let tokenArray = List.toArray rest
            let mutable splitIndex = -1
            let mutable depth = 0
            let mutable index = 0

            while index < tokenArray.Length && splitIndex = -1 do
                match tokenArray[index].Kind with
                | LeftParen -> depth <- depth + 1
                | RightParen -> depth <- max 0 (depth - 1)
                | Keyword Keyword.Do when depth = 0 -> splitIndex <- index
                | _ -> ()

                index <- index + 1

            if splitIndex < 0 then
                diagnostics.AddError(DiagnosticCode.ParseError, "Expected 'do' in the while statement.", source.GetLocation(whileToken.Span))
                DoExpression(Literal LiteralValue.Unit)
            else
                let conditionTokens = tokenArray[0 .. splitIndex - 1] |> Array.toList
                let bodyTokens =
                    if splitIndex + 1 < tokenArray.Length then
                        tokenArray[splitIndex + 1 ..] |> Array.toList
                    else
                        []

                DoWhile(this.ParseStandaloneExpression(conditionTokens), parseDoBlockTokens bodyTokens)
        | nameToken :: equalsToken :: rest when Token.isName nameToken && equalsToken.Kind = Equals ->
            let name = SyntaxFacts.trimIdentifierQuotes nameToken.Text
            DoAssign(name, this.ParseStandaloneExpression(rest))
        | nameToken :: operatorToken :: rest when Token.isName nameToken && operatorToken.Kind = Operator && operatorToken.Text = "<-" ->
            let binding =
                let name = SyntaxFacts.trimIdentifierQuotes nameToken.Text

                { Pattern = NamePattern name
                  Quantity = None
                  IsImplicit = false
                  TypeTokens = None
                  BinderSpans = Map.ofList [ name, [ nameToken.Span ] ] }

            DoBind(binding, this.ParseStandaloneExpression(rest))
        | _ ->
            DoExpression(this.ParseStandaloneExpression(lineTokens))

    member private this.ParseDoExpression() =
        this.ExpectKeyword(Keyword.Do, "Expected 'do'.") |> ignore

        let statements =
            this.CollectIndentedLines()
            |> List.filter (List.isEmpty >> not)
            |> List.map this.ParseDoLine

        match statements with
        | [] ->
            diagnostics.AddError(DiagnosticCode.ParseError, "A do block must contain at least one statement.", source.GetLocation(this.Current.Span))
        | _ ->
            ()

        Do statements

    member private this.ParseLambdaExpression() =
        this.SkipLayout()
        this.Advance() |> ignore
        let parameters = this.ParseLambdaParameters()

        if this.Current.Kind = Arrow then
            this.Advance() |> ignore
        else
            diagnostics.AddError(DiagnosticCode.ParseError, "Expected '->' after the lambda parameters.", source.GetLocation(this.Current.Span))

        let body =
            match this.Current.Kind, this.Peek(1).Kind with
            | Newline, Indent ->
                let firstBodyToken = this.Peek(2)
                let secondBodyToken = this.Peek(3)

                let startsPureBlockSuite =
                    Token.isKeyword Keyword.Let firstBodyToken
                    || Token.isKeyword Keyword.Type firstBodyToken
                    || (Token.isName firstBodyToken
                        && String.Equals(SyntaxFacts.trimIdentifierQuotes firstBodyToken.Text, "scoped", StringComparison.Ordinal)
                        && Token.isName secondBodyToken
                        && String.Equals(SyntaxFacts.trimIdentifierQuotes secondBodyToken.Text, "effect", StringComparison.Ordinal))
                    || ((firstBodyToken.Kind = Identifier || (match firstBodyToken.Kind with | Keyword _ -> true | _ -> false))
                        && secondBodyToken.Kind = Colon)

                if startsPureBlockSuite then
                    let lines =
                        this.CollectIndentedLines()
                        |> List.filter (List.isEmpty >> not)

                    this.ParsePureBlockSuite
                        lines
                        "A lambda pure block must contain at least one expression."
                        "Expected a sequence of local declarations followed by a final expression in the lambda body."
                        this.Current.Span
                else
                    this.Advance() |> ignore
                    this.ParseStandaloneExpression(this.CollectCurrentIndentedExpressionTokens())
            | _ ->
                this.ParseExpression(0)

        Lambda(parameters, body)

    member private this.ParseInterpolatedString() =
        let startToken = this.Advance()
        let startInfo : SyntaxFacts.PrefixedStringStartInfo =
            SyntaxFacts.tryDecodePrefixedStringStart startToken.Text
            |> Option.defaultValue
                { PrefixText = startToken.Text
                  HashCount = 0 }

        let prefix = SyntaxFacts.trimIdentifierQuotes startInfo.PrefixText
        let isRaw = startInfo.HashCount > 0
        let parts = ResizeArray<SurfaceInterpolatedStringPart>()
        let mutable closed = false

        while not closed && this.Current.Kind <> EndOfFile do
            match this.Current.Kind with
            | StringTextSegment ->
                let token = this.Advance()
                let text =
                    if isRaw then
                        token.Text
                    else
                        this.DecodeStringTextSegment token

                parts.Add(StringText text)
            | InterpolationStart ->
                this.Advance() |> ignore
                let expression = this.ParseExpression(0)
                parts.Add(StringInterpolation expression)

                if this.Current.Kind = InterpolationEnd then
                    this.Advance() |> ignore
                else
                    diagnostics.AddError(DiagnosticCode.ParseError, 
                        "Expected the interpolation to end before the string resumes.",
                        source.GetLocation(this.Current.Span)
                    )
            | InterpolatedStringEnd ->
                this.Advance() |> ignore
                closed <- true
            | _ ->
                diagnostics.AddError(DiagnosticCode.ParseError, "Expected interpolated string content.", source.GetLocation(this.Current.Span))
                closed <- true

        if not closed then
            diagnostics.AddError(DiagnosticCode.ParseError, "Unterminated interpolated string.", source.GetLocation(startToken.Span))

        PrefixedString(prefix, List.ofSeq parts)

    member private this.ParseSealExpression() =
        this.ExpectKeyword(Keyword.Seal, "Expected 'seal'.") |> ignore

        let valueTokens = ResizeArray<Token>()
        let mutable depth = 0
        let mutable foundAs = false

        while this.Current.Kind <> EndOfFile && not foundAs do
            match this.Current.Kind with
            | LeftParen
            | LeftBracket
            | LeftBrace
            | LeftSetBrace ->
                depth <- depth + 1
                valueTokens.Add(this.Advance())
            | RightParen
            | RightBracket
            | RightBrace
            | RightSetBrace ->
                depth <- max 0 (depth - 1)
                valueTokens.Add(this.Advance())
            | Keyword Keyword.As when depth = 0 ->
                this.Advance() |> ignore
                foundAs <- true
            | _ ->
                valueTokens.Add(this.Advance())

        if not foundAs then
            diagnostics.AddError(DiagnosticCode.ParseError, "Expected 'as' in the seal expression.", source.GetLocation(this.Current.Span))

        let ascriptionTokens = ResizeArray<Token>()

        while this.Current.Kind <> EndOfFile do
            ascriptionTokens.Add(this.Advance())

        let value =
            if valueTokens.Count = 0 then
                diagnostics.AddError(DiagnosticCode.ParseError, "Expected a value to seal.", source.GetLocation(this.Current.Span))
                Literal LiteralValue.Unit
            else
                this.ParseStandaloneExpression(List.ofSeq valueTokens)

        Seal(value, List.ofSeq ascriptionTokens)

    member private this.TryParseRecordLiteral(tokens: Token list) =
        let trimmed =
            tokens
            |> List.filter (fun token ->
                match token.Kind with
                | Newline
                | Indent
                | Dedent -> false
                | _ -> true)

        if
            match trimmed with
            | { Kind = Backslash } :: _ -> true
            | _ -> false
        then
            None
        else

        let splitTopLevelCommas (fieldTokens: Token list) =
            let tokenArray = List.toArray fieldTokens
            let items = ResizeArray<Token list>()
            let current = ResizeArray<Token>()
            let mutable depth = 0
            let mutable sawTopLevelComma = false

            for token in tokenArray do
                match token.Kind with
                | LeftParen
                | LeftBracket
                | LeftBrace
                | LeftSetBrace ->
                    depth <- depth + 1
                    current.Add(token)
                | RightParen
                | RightBracket
                | RightBrace
                | RightSetBrace ->
                    depth <- max 0 (depth - 1)
                    current.Add(token)
                | Comma when depth = 0 ->
                    sawTopLevelComma <- true
                    items.Add(List.ofSeq current)
                    current.Clear()
                | _ ->
                    current.Add(token)

            if current.Count > 0 then
                items.Add(List.ofSeq current)

            List.ofSeq items, sawTopLevelComma

        let tryFindTopLevelEquals (fieldTokens: Token list) =
            let tokenArray = List.toArray fieldTokens
            let mutable depth = 0
            let mutable index = 0
            let mutable result = None

            while index < tokenArray.Length && result.IsNone do
                match tokenArray[index].Kind with
                | LeftParen
                | LeftBracket
                | LeftBrace
                | LeftSetBrace ->
                    depth <- depth + 1
                | RightParen
                | RightBracket
                | RightBrace
                | RightSetBrace ->
                    depth <- max 0 (depth - 1)
                | Equals when depth = 0 ->
                    result <- Some index
                | _ ->
                    ()

                index <- index + 1

            result

        let fieldGroups, sawTopLevelComma =
            splitTopLevelCommas trimmed

        let containsTopLevelEquals =
            fieldGroups
            |> List.exists (fun fieldTokens -> tryFindTopLevelEquals fieldTokens |> Option.isSome)

        let trimFieldSignificantTokens (tokens: Token list) =
            tokens
            |> List.filter (fun token ->
                match token.Kind with
                | Newline
                | Indent
                | Dedent -> false
                | _ -> true)

        let isPunnedFieldGroup (fieldTokens: Token list) =
            match trimFieldSignificantTokens fieldTokens with
            | [ labelToken ] when this.IsNameToken(labelToken) -> true
            | _ -> false

        if not sawTopLevelComma && not containsTopLevelEquals then
            None
        elif sawTopLevelComma && not containsTopLevelEquals && not (fieldGroups |> List.forall isPunnedFieldGroup) then
            fieldGroups
            |> List.filter (List.isEmpty >> not)
            |> List.mapi (fun index fieldTokens ->
                { Name = $"_{index + 1}"
                  IsImplicit = false
                  Value = this.ParseStandaloneExpression(fieldTokens) })
            |> RecordLiteral
            |> Some
        else
            let parseField (fieldTokens: Token list) : SurfaceRecordLiteralField =
                let trimmedField =
                    fieldTokens
                    |> List.filter (fun token ->
                        match token.Kind with
                        | Newline
                        | Indent
                        | Dedent -> false
                        | _ -> true)

                let isImplicit, remaining =
                    match trimmedField with
                    | { Kind = AtSign } :: rest -> true, rest
                    | _ -> false, trimmedField

                match tryFindTopLevelEquals remaining with
                | Some index ->
                    let tokenArray = List.toArray remaining
                    let labelTokens = tokenArray[0 .. index - 1] |> Array.toList
                    let valueTokens = tokenArray[index + 1 ..] |> Array.toList

                    match labelTokens with
                    | [ labelToken ] when this.IsNameToken(labelToken) ->
                        { Name = SyntaxFacts.trimIdentifierQuotes labelToken.Text
                          IsImplicit = isImplicit
                          Value = this.ParseStandaloneExpression(valueTokens) }
                    | labelToken :: _ ->
                        diagnostics.AddError(DiagnosticCode.ParseError, "Expected a record field label.", source.GetLocation(labelToken.Span))
                        { Name = "<missing>"
                          IsImplicit = isImplicit
                          Value = Literal LiteralValue.Unit }
                    | [] ->
                        diagnostics.AddError(DiagnosticCode.ParseError, "Expected a record field label.", source.GetLocation(eofSpan))
                        { Name = "<missing>"
                          IsImplicit = isImplicit
                          Value = Literal LiteralValue.Unit }
                | None ->
                    match remaining with
                    | [ labelToken ] when not isImplicit && this.IsNameToken(labelToken) ->
                        let fieldName = SyntaxFacts.trimIdentifierQuotes labelToken.Text

                        { Name = fieldName
                          IsImplicit = false
                          Value = Name [ fieldName ] }
                    | token :: _ ->
                        diagnostics.AddError(
                            DiagnosticCode.ParseError,
                            "Expected a record field of the form 'name = expr' or a punned field name.",
                            source.GetLocation(token.Span)
                        )

                        { Name = "<missing>"
                          IsImplicit = isImplicit
                          Value = Literal LiteralValue.Unit }
                    | [] ->
                        diagnostics.AddError(
                            DiagnosticCode.ParseError,
                            "Expected a record field of the form 'name = expr' or a punned field name.",
                            source.GetLocation(eofSpan)
                        )

                        { Name = "<missing>"
                          IsImplicit = isImplicit
                          Value = Literal LiteralValue.Unit }

            fieldGroups
            |> List.filter (List.isEmpty >> not)
            |> List.map parseField
            |> RecordLiteral
            |> Some

    member private this.ParseNamedApplicationBlockArgument() =
        let tokens =
            this.CollectBracedTokens("Expected '}' to close the named application block.")
            |> List.filter (fun token ->
                match token.Kind with
                | Newline
                | Indent
                | Dedent -> false
                | _ -> true)

        let splitTopLevelCommas (fieldTokens: Token list) =
            let tokenArray = List.toArray fieldTokens
            let items = ResizeArray<Token list>()
            let current = ResizeArray<Token>()
            let mutable depth = 0

            for token in tokenArray do
                match token.Kind with
                | LeftParen
                | LeftBracket
                | LeftBrace
                | LeftSetBrace ->
                    depth <- depth + 1
                    current.Add(token)
                | RightParen
                | RightBracket
                | RightBrace
                | RightSetBrace ->
                    depth <- max 0 (depth - 1)
                    current.Add(token)
                | Comma when depth = 0 ->
                    items.Add(List.ofSeq current)
                    current.Clear()
                | _ ->
                    current.Add(token)

            if current.Count > 0 then
                items.Add(List.ofSeq current)

            List.ofSeq items

        let tryFindTopLevelEquals (fieldTokens: Token list) =
            let tokenArray = List.toArray fieldTokens
            let mutable depth = 0
            let mutable index = 0
            let mutable result = None

            while index < tokenArray.Length && result.IsNone do
                match tokenArray[index].Kind with
                | LeftParen
                | LeftBracket
                | LeftBrace
                | LeftSetBrace ->
                    depth <- depth + 1
                | RightParen
                | RightBracket
                | RightBrace
                | RightSetBrace ->
                    depth <- max 0 (depth - 1)
                | Equals when depth = 0 ->
                    result <- Some index
                | _ ->
                    ()

                index <- index + 1

            result

        let parseField (fieldTokens: Token list) =
            let trimmedField =
                fieldTokens
                |> List.filter (fun token ->
                    match token.Kind with
                    | Newline
                    | Indent
                    | Dedent -> false
                    | _ -> true)

            let isImplicit, remaining =
                match trimmedField with
                | { Kind = AtSign } :: rest -> true, rest
                | _ -> false, trimmedField

            match tryFindTopLevelEquals remaining with
            | Some index ->
                let tokenArray = List.toArray remaining
                let labelTokens = tokenArray[0 .. index - 1] |> Array.toList
                let valueTokens = tokenArray[index + 1 ..] |> Array.toList

                match labelTokens with
                | [ labelToken ] when this.IsNameToken(labelToken) ->
                    { Name = SyntaxFacts.trimIdentifierQuotes labelToken.Text
                      IsImplicit = isImplicit
                      Value = this.ParseStandaloneExpression(valueTokens) }
                | labelToken :: _ ->
                    diagnostics.AddError(DiagnosticCode.ParseError, "Expected a named application field label.", source.GetLocation(labelToken.Span))
                    { Name = "<missing>"
                      IsImplicit = isImplicit
                      Value = Literal LiteralValue.Unit }
                | [] ->
                    diagnostics.AddError(DiagnosticCode.ParseError, "Expected a named application field label.", source.GetLocation(eofSpan))
                    { Name = "<missing>"
                      IsImplicit = isImplicit
                      Value = Literal LiteralValue.Unit }
            | None ->
                match remaining with
                | [ labelToken ] when not isImplicit && this.IsNameToken(labelToken) ->
                    let fieldName = SyntaxFacts.trimIdentifierQuotes labelToken.Text

                    { Name = fieldName
                      IsImplicit = false
                      Value = Name [ fieldName ] }
                | token :: _ ->
                    diagnostics.AddError(
                        DiagnosticCode.ParseError,
                        "Expected a named application field of the form 'name = expr' or a punned field name.",
                        source.GetLocation(token.Span)
                    )

                    { Name = "<missing>"
                      IsImplicit = isImplicit
                      Value = Literal LiteralValue.Unit }
                | [] ->
                    diagnostics.AddError(
                        DiagnosticCode.ParseError,
                        "Expected a named application field of the form 'name = expr' or a punned field name.",
                        source.GetLocation(eofSpan)
                    )

                    { Name = "<missing>"
                      IsImplicit = isImplicit
                      Value = Literal LiteralValue.Unit }

        tokens
        |> splitTopLevelCommas
        |> List.filter (List.isEmpty >> not)
        |> List.map parseField
        |> NamedApplicationBlock

    member private this.ParseKindQualifiedName(selector: KindSelector, selectorDescription: string) =
        this.Advance() |> ignore
        this.SkipLayout()

        if this.IsNameToken(this.Current) then
            KindQualifiedName(selector, this.ParseQualifiedName())
        else
            diagnostics.AddError(
                DiagnosticCode.ParseError,
                $"Expected a name after '{selectorDescription}'.",
                source.GetLocation(this.Current.Span)
            )

            KindQualifiedName(selector, [ "<missing>" ])

    member private this.IsKindQualifiedSelectorStart() =
        match this.Current.Kind with
        | Keyword Keyword.Type
        | Keyword Keyword.Trait
        | Keyword Keyword.Module ->
            this.IsNameToken(this.Peek(1))
        | _ ->
            false

    member private this.IsEffectLabelSelectorStart() =
        this.Current.Kind = Identifier
        && String.Equals(this.Current.Text, "effect", StringComparison.Ordinal)
        && this.Peek(1).Kind = Operator
        && String.Equals(this.Peek(1).Text, "-", StringComparison.Ordinal)
        && this.Peek(2).Kind = Identifier
        && String.Equals(this.Peek(2).Text, "label", StringComparison.Ordinal)

    member private this.ParseEffectLabelKindQualifiedName() =
        this.Advance() |> ignore
        this.Advance() |> ignore
        this.Advance() |> ignore
        this.SkipLayout()

        if this.IsNameToken(this.Current) then
            KindQualifiedName(EffectLabelKind, this.ParseQualifiedName())
        else
            diagnostics.AddError(
                DiagnosticCode.ParseError,
                "Expected a name after 'effect-label'.",
                source.GetLocation(this.Current.Span)
            )

            KindQualifiedName(EffectLabelKind, [ "<missing>" ])

    member private this.ParsePrimaryExpression() =
        this.SkipLayout()

        match this.Current.Kind with
        | Keyword Keyword.Match when this.Peek(1).Kind = Dot ->
            let segments = this.ParseQualifiedName()
            Name segments
        | Keyword Keyword.Match ->
            this.ParseMatchExpression()
        | Keyword Keyword.Do ->
            this.ParseDoExpression()
        | Keyword Keyword.If ->
            this.ParseIfExpression()
        | Keyword Keyword.Seal ->
            this.ParseSealExpression()
        | Keyword Keyword.Type when this.IsKindQualifiedSelectorStart() ->
            this.ParseKindQualifiedName(TypeKind, "type")
        | Keyword Keyword.Trait when this.IsKindQualifiedSelectorStart() ->
            this.ParseKindQualifiedName(TraitKind, "trait")
        | Keyword Keyword.Module when this.IsKindQualifiedSelectorStart() ->
            this.ParseKindQualifiedName(ModuleKind, "module")
        | _ when this.IsEffectLabelSelectorStart() ->
            this.ParseEffectLabelKindQualifiedName()
        | _ when Token.isName this.Current && String.Equals(SyntaxFacts.trimIdentifierQuotes this.Current.Text, "block", StringComparison.Ordinal) ->
            this.ParseBlockExpression()
        | Backslash ->
            this.ParseLambdaExpression()
        | IntegerLiteral ->
            let token = this.Advance()

            match SyntaxFacts.tryParseNumericLiteral token.Text with
            | Result.Ok parsed ->
                NumericLiteral parsed.Literal
            | Result.Error message ->
                diagnostics.AddError(DiagnosticCode.ParseError, message, source.GetLocation(token.Span))
                Literal(Integer 0L)
        | FloatLiteral ->
            let token = this.Advance()

            match SyntaxFacts.tryParseNumericLiteral token.Text with
            | Result.Ok parsed ->
                NumericLiteral parsed.Literal
            | Result.Error message ->
                diagnostics.AddError(DiagnosticCode.ParseError, message, source.GetLocation(token.Span))
                Literal(Float 0.0)
        | StringLiteral ->
            let token = this.Advance()
            Literal(LiteralValue.String(this.DecodeStringLiteral token))
        | InterpolatedStringStart ->
            this.ParseInterpolatedString()
        | CharacterLiteral ->
            let token = this.Advance()
            Literal(Character(this.DecodeCharacterLiteral token))
        | Dot when this.Peek(1).Kind = Operator && this.Peek(1).Text = "<" ->
            let startToken = this.Advance()
            this.Advance() |> ignore

            while this.Current.Kind <> EndOfFile
                  && not (this.Current.Kind = Operator && this.Current.Text = ">.") do
                this.Advance() |> ignore

            if this.Current.Kind = Operator && this.Current.Text = ">." then
                this.Advance() |> ignore

            diagnostics.AddError(
                DiagnosticCode.ParseError,
                "Code quotations are specified but not implemented by this compiler milestone.",
                source.GetLocation(startToken.Span)
            )

            Literal Unit
        | Underscore ->
            this.Advance() |> ignore
            Name [ "_" ]
        | LeftParen ->
            let innerTokens = this.CollectParenthesizedTokens()

            match innerTokens with
            | [] ->
                Literal Unit
            | { Kind = Backslash } :: _ ->
                this.ParseStandaloneExpression innerTokens
            | _ ->
                match this.TryParseReceiverSection innerTokens with
                | Some expression ->
                    expression
                | None ->
                    match this.TryParseOperatorSection innerTokens with
                    | Some expression ->
                        expression
                    | None ->
                        match this.TryParseRecordLiteral innerTokens with
                        | Some expression ->
                            expression
                        | None ->
                            match this.TryParseStandaloneExpression innerTokens with
                            | Some expression ->
                                expression
                            | None ->
                                diagnostics.AddError(DiagnosticCode.ParseError, "Expected an expression inside parentheses.", source.GetLocation(this.Current.Span))
                                Literal Unit
        | _ when this.IsNameToken(this.Current) ->
            let segments = this.ParseQualifiedName()
            Name segments
        | _ ->
            diagnostics.AddError(DiagnosticCode.ParseError, "Expected an expression.", source.GetLocation(this.Current.Span))
            Literal Unit

    member private this.ParsePrefixExpression() =
        this.SkipLayout()

        match this.Current.Kind with
        | AtSign ->
            this.Advance() |> ignore
            ExplicitImplicitArgument(this.ParsePrefixExpression())
        | Operator ->
            if this.Current.Text = "'" && this.Peek(1).Kind = LeftBrace then
                this.Advance() |> ignore
                let innerTokens = this.CollectBracedTokens("Expected '}' to close the syntax quote.")
                this.ParseStandaloneExpression innerTokens
            elif this.Current.Text = "!" then
                this.Advance() |> ignore
                // Splice should bind tightly to the next atomic/application form without
                // swallowing surrounding infix operators in the enclosing expression.
                MonadicSplice(this.ParseApplicationExpression())
            elif this.Current.Text = "~" then
                this.Advance() |> ignore
                InoutArgument(this.ParsePrefixExpression())
            else
                match FixityTable.tryFindPrefix this.Current.Text fixities with
                | Some precedence ->
                    let operatorToken = this.Advance()
                    let expression = this.ParseExpression(precedence)
                    Unary(operatorToken.Text, expression)
                | None ->
                    this.ParsePrimaryExpression()
        | _ ->
            this.ParsePrimaryExpression()

    member private this.CanStartApplicationArgumentAfterHead(token: Token) =
        match token.Kind with
        | LeftBrace ->
            true
        | Operator ->
            if token.Text = "'" then
                true
            elif token.Text = "~" then
                true
            else
                match FixityTable.tryFindPrefix token.Text fixities with
                | Some _ ->
                    FixityTable.tryFindInfix token.Text fixities |> Option.isNone
                    && FixityTable.tryFindPostfix token.Text fixities |> Option.isNone
                | None ->
                    false
        | _ ->
            this.IsExpressionStart(token)

    member private this.TryParseSafeNavigationMember() =
        this.SkipLayout()

        if not (this.IsNameToken(this.Current)) then
            None
        else
            let segments = this.ParseQualifiedName()
            let arguments = ResizeArray<SurfaceExpression>()
            let mutable keepReadingArguments = true

            while keepReadingArguments do
                this.SkipLayout()

                if this.CanStartApplicationArgumentAfterHead(this.Current) then
                    arguments.Add(this.ParseApplicationArgument())
                else
                    keepReadingArguments <- false

            Some
                { Segments = segments
                  Arguments = List.ofSeq arguments }

    member private this.ParseApplicationArgument() =
        this.SkipLayout()

        match this.Current.Kind with
        | LeftBrace ->
            this.ParseNamedApplicationBlockArgument()
        | _ ->
            this.ParsePrefixExpression()

    member private this.ParseTagTestConstructor() =
        this.SkipLayout()

        if this.IsNameToken(this.Current) then
            this.ParseQualifiedName()
        else
            diagnostics.AddError(
                DiagnosticCode.ParseError,
                "Expected a constructor name after 'is'.",
                source.GetLocation(this.Current.Span)
            )

            [ "<missing>" ]

    member private this.ParseApplicationExpression() =
        let head = this.ParsePrefixExpression()
        let arguments = ResizeArray<SurfaceExpression>()
        let mutable keepReadingArguments = true

        while keepReadingArguments do
            this.SkipLayout()

            if this.CanStartApplicationArgumentAfterHead(this.Current) then
                arguments.Add(this.ParseApplicationArgument())
            else
                keepReadingArguments <- false

        let mutable expression =
            if arguments.Count = 0 then
                head
            else
                Apply(head, List.ofSeq arguments)

        let rec collectBracedTokens (startToken: Token) =
            let innerTokens = ResizeArray<Token>()
            let mutable depth = 1

            while depth > 0 && this.Current.Kind <> EndOfFile do
                match this.Current.Kind with
                | LeftBrace ->
                    depth <- depth + 1
                    innerTokens.Add(this.Advance())
                | RightBrace ->
                    depth <- depth - 1

                    if depth > 0 then
                        innerTokens.Add(this.Advance())
                    else
                        this.Advance() |> ignore
                | _ ->
                    innerTokens.Add(this.Advance())

            if depth > 0 then
                diagnostics.AddError(DiagnosticCode.ParseError, "Expected '}' to close the record update.", source.GetLocation(startToken.Span))

            List.ofSeq innerTokens

        let splitTopLevelCommas (tokens: Token list) =
            let tokenArray = List.toArray tokens
            let items = ResizeArray<Token list>()
            let current = ResizeArray<Token>()
            let mutable depth = 0

            for token in tokenArray do
                match token.Kind with
                | LeftParen
                | LeftBracket
                | LeftBrace
                | LeftSetBrace ->
                    depth <- depth + 1
                    current.Add(token)
                | RightParen
                | RightBracket
                | RightBrace
                | RightSetBrace ->
                    depth <- max 0 (depth - 1)
                    current.Add(token)
                | Comma when depth = 0 ->
                    items.Add(List.ofSeq current)
                    current.Clear()
                | _ ->
                    current.Add(token)

            if current.Count > 0 then
                items.Add(List.ofSeq current)

            items |> Seq.toList

        let parseRecordUpdateField (tokens: Token list) =
            let trimmed =
                tokens
                |> List.filter (fun token ->
                    match token.Kind with
                    | Newline
                    | Indent
                    | Dedent -> false
                    | _ -> true)

            let tokenArray = List.toArray trimmed

            let tryFindPatchOperator () =
                let mutable depth = 0
                let mutable index = 0
                let mutable result = None

                while index < tokenArray.Length && result.IsNone do
                    match tokenArray[index].Kind with
                    | LeftParen
                    | LeftBracket
                    | LeftBrace
                    | LeftSetBrace ->
                        depth <- depth + 1
                    | RightParen
                    | RightBracket
                    | RightBrace
                    | RightSetBrace ->
                        depth <- max 0 (depth - 1)
                    | Colon when depth = 0 && index + 1 < tokenArray.Length && tokenArray[index + 1].Kind = Equals ->
                        result <- Some(true, index, index + 2)
                    | Equals when depth = 0 ->
                        result <- Some(false, index, index + 1)
                    | _ ->
                        ()

                    index <- index + 1

                result

            let parsePath (pathTokens: Token list) =
                let trimmedPathTokens =
                    pathTokens
                    |> List.filter (fun token ->
                        match token.Kind with
                        | Newline
                        | Indent
                        | Dedent -> false
                        | _ -> true)

                let tryProjectionSectionPath () =
                    match trimmedPathTokens with
                    | { Kind = LeftParen } :: { Kind = Dot } :: memberToken :: { Kind = RightParen } :: []
                        when this.IsNameToken(memberToken) ->
                        Some
                            [
                                { Name = SyntaxFacts.trimIdentifierQuotes memberToken.Text
                                  IsImplicit = false }
                            ]
                    | _ ->
                        None

                let segments = ResizeArray<SurfaceRecordUpdatePathSegment>()
                let pathArray = List.toArray trimmedPathTokens
                let mutable index = 0
                let mutable valid = true
                let mutable expectSegment = true

                match tryProjectionSectionPath () with
                | Some path ->
                    Some path
                | None ->
                    while index < pathArray.Length && valid do
                        if expectSegment then
                            let isImplicit, segmentIndex =
                                if pathArray[index].Kind = AtSign then
                                    true, index + 1
                                else
                                    false, index

                            if segmentIndex < pathArray.Length && this.IsNameToken(pathArray[segmentIndex]) then
                                segments.Add(
                                    { Name = SyntaxFacts.trimIdentifierQuotes pathArray[segmentIndex].Text
                                      IsImplicit = isImplicit }
                                )
                                index <- segmentIndex + 1
                                expectSegment <- false
                            else
                                valid <- false
                        else if pathArray[index].Kind = Dot then
                            index <- index + 1
                            expectSegment <- true
                        else
                            valid <- false

                    if valid && not expectSegment && segments.Count > 0 then
                        Some(List.ofSeq segments)
                    else
                        None

            match tryFindPatchOperator () with
            | Some(isExtension, operatorStart, valueStart) ->
                let pathTokens = tokenArray[0 .. operatorStart - 1] |> Array.toList
                let valueTokens = tokenArray[valueStart ..] |> Array.toList

                match parsePath pathTokens with
                | Some [ segment ] when isExtension && not segment.IsImplicit ->
                    { Name = segment.Name
                      IsImplicit = false
                      IsExtension = true
                      Path = [ segment ]
                      Value = this.ParseStandaloneExpression(valueTokens) }
                | Some path when not isExtension ->
                    let firstSegment = List.head path

                    { Name = firstSegment.Name
                      IsImplicit = firstSegment.IsImplicit
                      IsExtension = false
                      Path = path
                      Value = this.ParseStandaloneExpression(valueTokens) }
                | Some _ ->
                    diagnostics.AddError(DiagnosticCode.RecordPatchInvalidItem, "Row-extension fields must be top-level labels of the form 'name := expr'.", source.GetLocation(tokenArray[operatorStart].Span))
                    { Name = "<missing>"
                      IsImplicit = false
                      IsExtension = isExtension
                      Path = []
                      Value = Literal LiteralValue.Unit }
                | None ->
                    let errorSpan =
                        match pathTokens with
                        | token :: _ -> token.Span
                        | [] -> tokenArray[operatorStart].Span

                    diagnostics.AddError(DiagnosticCode.RecordPatchInvalidItem, "Expected a record patch path before '=' or ':='.", source.GetLocation(errorSpan))
                    { Name = "<missing>"
                      IsImplicit = false
                      IsExtension = isExtension
                      Path = []
                      Value = Literal LiteralValue.Unit }
            | None ->
                let errorSpan =
                    match trimmed with
                    | token :: _ -> token.Span
                    | [] -> this.Current.Span

                diagnostics.AddError(DiagnosticCode.RecordPatchInvalidItem, "Expected a record patch item of the form 'path = expr' or 'name := expr'.", source.GetLocation(errorSpan))
                { Name = "<missing>"
                  IsImplicit = false
                  IsExtension = false
                  Path = []
                  Value = Literal LiteralValue.Unit }

        let mutable keepReadingPostfix = true

        while keepReadingPostfix do
            this.SkipLayout()

            if this.Current.Kind = Dot && this.Peek(1).Kind = LeftBrace then
                let startToken = this.Advance()
                this.Advance() |> ignore

                let fields =
                    collectBracedTokens startToken
                    |> splitTopLevelCommas
                    |> List.filter (List.isEmpty >> not)
                    |> List.map parseRecordUpdateField

                expression <- RecordUpdate(expression, fields)
            elif this.Current.Kind = Dot && this.IsNameToken(this.Peek(1)) then
                this.Advance() |> ignore
                let memberToken = this.Advance()
                let memberName = SyntaxFacts.trimIdentifierQuotes memberToken.Text

                let memberArguments = ResizeArray<SurfaceExpression>()
                let mutable keepReadingMemberArguments = true

                while keepReadingMemberArguments do
                    this.SkipLayout()

                    if this.CanStartApplicationArgumentAfterHead(this.Current) then
                        memberArguments.Add(this.ParseApplicationArgument())
                    else
                        keepReadingMemberArguments <- false

                expression <- MemberAccess(expression, [ memberName ], List.ofSeq memberArguments)
            elif this.Current.Kind = Operator && this.Current.Text = "?." then
                let safeNavigationToken = this.Advance()

                match this.TryParseSafeNavigationMember() with
                | Some navigation ->
                    expression <- SafeNavigation(expression, navigation)
                | None ->
                    diagnostics.AddError(
                        DiagnosticCode.ParseError,
                        "Expected a member access after '?.'.",
                        source.GetLocation(safeNavigationToken.Span)
                    )

                    keepReadingPostfix <- false
            else
                keepReadingPostfix <- false

        expression

    member private this.ParseExpression(minimumPrecedence: int) =
        let mutable left = this.ParseApplicationExpression()
        let mutable keepParsing = true

        while keepParsing do
            this.SkipLayout()

            match this.Current.Kind with
            | Keyword Keyword.Is ->
                let precedence =
                    FixityTable.tryFindInfix "==" fixities
                    |> Option.map snd
                    |> Option.defaultValue 40

                if precedence >= minimumPrecedence then
                    let isToken = this.Advance()

                    match left with
                    | TagTest _ ->
                        diagnostics.AddError(
                            DiagnosticCode.ParseError,
                            "Constructor tag tests cannot be chained without parentheses.",
                            source.GetLocation(isToken.Span)
                        )
                    | _ ->
                        let constructorName = this.ParseTagTestConstructor()
                        left <- TagTest(left, constructorName)
                else
                    keepParsing <- false
            | Operator when this.Current.Text = "?:" ->
                let precedence = 2

                if precedence >= minimumPrecedence then
                    this.Advance() |> ignore
                    let right = this.ParseExpression(precedence)
                    left <- Elvis(left, right)
                else
                    keepParsing <- false
            | Operator ->
                let operatorText = this.Current.Text

                match FixityTable.tryFindPostfix operatorText fixities with
                | Some precedence when precedence >= minimumPrecedence ->
                    this.Advance() |> ignore
                    left <- Apply(Name [ operatorText ], [ left ])
                | _ ->
                    match FixityTable.tryFindInfix operatorText fixities with
                    | Some(associativity, precedence) when precedence >= minimumPrecedence ->
                        let nextMinimumPrecedence =
                            match associativity with
                            | LeftAssociative
                            | NonAssociative -> precedence + 1
                            | RightAssociative -> precedence

                        this.Advance() |> ignore
                        let right = this.ParseExpression(nextMinimumPrecedence)
                        left <- Binary(left, operatorText, right)
                    | _ ->
                        keepParsing <- false
            | _ ->
                keepParsing <- false

        left

    member this.Parse() =
        this.SkipLayout()

        if this.Current.Kind = EndOfFile then
            None
        else
            let expression = this.ParseExpression(0)
            this.SkipLayout()

            if this.Current.Kind <> EndOfFile then
                diagnostics.AddError(DiagnosticCode.ParseError, "Unexpected tokens at the end of the expression.", source.GetLocation(this.Current.Span))

            Some expression

// Parses declarations, expressions, patterns, and types into the surface syntax tree.
module CoreParsing =
    let private trimSignificantTokens (tokens: Token list) =
        tokens
        |> List.filter (fun token ->
            match token.Kind with
            | Newline
            | Indent
            | Dedent
            | EndOfFile -> false
            | _ -> true)

    let private trimLeadingLayout (tokens: Token list) =
        tokens
        |> List.skipWhile (fun token ->
            match token.Kind with
            | Newline
            | Indent
            | Dedent -> true
            | _ -> false)

    let private splitTopLevelAtKeyword keyword (tokens: Token list) =
        let tokenArray = List.toArray tokens
        let mutable depth = 0
        let mutable splitIndex = -1
        let mutable index = 0

        while index < tokenArray.Length && splitIndex < 0 do
            match tokenArray[index].Kind with
            | LeftParen
            | LeftBracket
            | LeftBrace
            | LeftSetBrace ->
                depth <- depth + 1
            | RightParen
            | RightBracket
            | RightBrace
            | RightSetBrace ->
                depth <- max 0 (depth - 1)
            | _ when depth = 0 && Token.isKeyword keyword tokenArray[index] ->
                splitIndex <- index
            | _ ->
                ()

            index <- index + 1

        if splitIndex < 0 then
            None
        else
            Some(
                tokenArray[0 .. splitIndex - 1] |> Array.toList,
                tokenArray[splitIndex + 1 ..] |> Array.toList
            )

    let private splitTopLevelAtColon (tokens: Token list) =
        let tokenArray = List.toArray tokens
        let mutable depth = 0
        let mutable splitIndex = -1
        let mutable index = 0

        while index < tokenArray.Length && splitIndex < 0 do
            match tokenArray[index].Kind with
            | LeftParen
            | LeftBracket
            | LeftBrace
            | LeftSetBrace ->
                depth <- depth + 1
            | RightParen
            | RightBracket
            | RightBrace
            | RightSetBrace ->
                depth <- max 0 (depth - 1)
            | Colon when depth = 0 ->
                splitIndex <- index
            | _ ->
                ()

            index <- index + 1

        if splitIndex < 0 then
            None
        else
            Some(
                tokenArray[0 .. splitIndex - 1] |> Array.toList,
                tokenArray[splitIndex + 1 ..] |> Array.toList
            )

    let private splitReturnTypeTokens (tokens: Token list) =
        let mutable depth = 0
        let mutable splitIndex = -1
        let tokenArray = List.toArray tokens
        let mutable index = 0

        while index < tokenArray.Length && splitIndex = -1 do
            match tokenArray[index].Kind with
            | LeftParen -> depth <- depth + 1
            | RightParen -> depth <- max 0 (depth - 1)
            | Colon when depth = 0 -> splitIndex <- index
            | _ -> ()

            index <- index + 1

        if splitIndex >= 0 then
            List.ofArray tokenArray[0 .. splitIndex - 1], Some(List.ofArray tokenArray[splitIndex + 1 ..])
        else
            tokens, None

    let parseParameterLayout (source: SourceText) (diagnostics: DiagnosticBag) (tokens: Token list) =
        let eofSpan =
            match List.rev tokens with
            | last :: _ -> last.Span
            | [] -> TextSpan.FromBounds(0, 0)

        SurfaceBinderParsing.parseParameterFromTokens diagnostics source eofSpan tokens

    let parseLetHeader (source: SourceText) (diagnostics: DiagnosticBag) (tokens: Token list) =
        let parameterTokens, returnTypeTokens = splitReturnTypeTokens tokens
        let tokenArray = List.toArray parameterTokens
        let parameters = ResizeArray<Parameter>()
        let mutable index = 0

        while index < tokenArray.Length do
            match tokenArray[index].Kind with
            | AtSign ->
                if index + 1 >= tokenArray.Length then
                    diagnostics.AddError(DiagnosticCode.ParseError, "Expected an implicit parameter after '@' in the function header.", source.GetLocation(tokenArray[index].Span))
                    index <- tokenArray.Length
                else
                    match tokenArray[index + 1].Kind with
                    | Identifier
                    | Keyword _ ->
                        parameters.Add(
                            SurfaceBinderParsing.makeParameter
                                (SyntaxFacts.trimIdentifierQuotes tokenArray[index + 1].Text)
                                None
                                None
                                true
                                false
                        )

                        index <- index + 2
                    | LeftParen ->
                        let startToken = tokenArray[index + 1]
                        let mutable depth = 1
                        let mutable endIndex = index + 2

                        while endIndex < tokenArray.Length && depth > 0 do
                            match tokenArray[endIndex].Kind with
                            | LeftParen -> depth <- depth + 1
                            | RightParen -> depth <- depth - 1
                            | _ -> ()

                            endIndex <- endIndex + 1

                        if depth > 0 then
                            diagnostics.AddError(DiagnosticCode.ParseError, "Unterminated implicit parameter binder in function header.", source.GetLocation(startToken.Span))
                            index <- tokenArray.Length
                        else
                            let innerTokens = List.ofArray tokenArray[index + 2 .. endIndex - 2]

                            match SurfaceBinderParsing.parseParameterFromTokens diagnostics source startToken.Span innerTokens with
                            | Some parameter -> parameters.Add({ parameter with IsImplicit = true })
                            | None -> ()

                            index <- endIndex
                    | _ ->
                        diagnostics.AddError(DiagnosticCode.ParseError, "Unsupported implicit parameter syntax in function header.", source.GetLocation(tokenArray[index + 1].Span))
                        index <- index + 2
            | Identifier
            | Keyword _ ->
                parameters.Add(
                    SurfaceBinderParsing.makeParameter
                        (SyntaxFacts.trimIdentifierQuotes tokenArray[index].Text)
                        None
                        None
                        false
                        false
                )

                index <- index + 1
            | Underscore ->
                parameters.Add(
                    SurfaceBinderParsing.makeParameter
                        (SurfaceBinderParsing.wildcardParameterName tokenArray[index].Span)
                        None
                        None
                        false
                        false
                )

                index <- index + 1
            | LeftParen ->
                let startToken = tokenArray[index]
                let mutable depth = 1
                let mutable endIndex = index + 1

                while endIndex < tokenArray.Length && depth > 0 do
                    match tokenArray[endIndex].Kind with
                    | LeftParen -> depth <- depth + 1
                    | RightParen -> depth <- depth - 1
                    | _ -> ()

                    endIndex <- endIndex + 1

                if depth > 0 then
                    diagnostics.AddError(DiagnosticCode.ParseError, "Unterminated parameter binder in function header.", source.GetLocation(startToken.Span))
                    index <- tokenArray.Length
                else
                    let innerTokens = List.ofArray tokenArray[index + 1 .. endIndex - 2]

                    if List.isEmpty innerTokens then
                        parameters.Add(
                            SurfaceBinderParsing.makeParameter
                                (SurfaceBinderParsing.wildcardParameterName startToken.Span)
                                None
                                None
                                false
                                false
                        )
                    else
                        match SurfaceBinderParsing.parseParameterFromTokens diagnostics source startToken.Span innerTokens with
                        | Some parameter -> parameters.Add(parameter)
                        | None -> ()

                    index <- endIndex
            | _ ->
                diagnostics.AddError(DiagnosticCode.ParseError, "Unsupported function header syntax.", source.GetLocation(tokenArray[index].Span))
                index <- index + 1

        { Parameters = List.ofSeq parameters
          ReturnTypeTokens = returnTypeTokens }

    let parseProjectionHeader (source: SourceText) (diagnostics: DiagnosticBag) (tokens: Token list) =
        let isThisToken (token: Token) =
            Token.isName token
            && String.Equals(SyntaxFacts.trimIdentifierQuotes token.Text, "this", StringComparison.Ordinal)

        let parseProjectionBinder startSpan binderTokens =
            let trimmed = trimSignificantTokens binderTokens

            match trimmed with
            | placeToken :: rest when Token.isKeyword Keyword.Place placeToken ->
                match rest with
                | thisToken :: colonToken :: typeTokens when isThisToken thisToken && colonToken.Kind = Colon ->
                    Some(
                        ProjectionPlaceBinder
                            { Name = "this"
                              TypeTokens = typeTokens
                              IsReceiver = true }
                    )
                | thisToken :: nameToken :: colonToken :: typeTokens
                    when isThisToken thisToken && Token.isName nameToken && colonToken.Kind = Colon ->
                    Some(
                        ProjectionPlaceBinder
                            { Name = SyntaxFacts.trimIdentifierQuotes nameToken.Text
                              TypeTokens = typeTokens
                              IsReceiver = true }
                    )
                | nameToken :: colonToken :: typeTokens when Token.isName nameToken && colonToken.Kind = Colon ->
                    Some(
                        ProjectionPlaceBinder
                            { Name = SyntaxFacts.trimIdentifierQuotes nameToken.Text
                              TypeTokens = typeTokens
                              IsReceiver = false }
                    )
                | _ ->
                    diagnostics.AddError(DiagnosticCode.ParseError, "Expected a projection place binder of the form '(place name : Type)'.", source.GetLocation(startSpan))
                    None
            | _ ->
                match SurfaceBinderParsing.parseParameterFromTokens diagnostics source startSpan trimmed with
                | Some parameter when parameter.IsInout ->
                    diagnostics.AddError(DiagnosticCode.ParseError, "Projection parameters must not use 'inout'.", source.GetLocation(startSpan))
                    Some(ProjectionValueBinder { parameter with IsInout = false })
                | Some parameter ->
                    Some(ProjectionValueBinder parameter)
                | None ->
                    None

        let parameterTokens, returnTypeTokens =
            match splitTopLevelAtColon tokens with
            | Some(parameterTokens, returnTypeTokens) ->
                parameterTokens, returnTypeTokens
            | None ->
                diagnostics.AddError(DiagnosticCode.ParseError, "Expected ':' before the projection result type.", source.GetLocation(TextSpan.FromBounds(source.Length, source.Length)))
                tokens, []

        let tokenArray = List.toArray parameterTokens
        let binders = ResizeArray<ProjectionBinder>()
        let mutable index = 0

        while index < tokenArray.Length do
            match tokenArray[index].Kind with
            | LeftParen ->
                let startToken = tokenArray[index]
                let mutable depth = 1
                let mutable endIndex = index + 1

                while endIndex < tokenArray.Length && depth > 0 do
                    match tokenArray[endIndex].Kind with
                    | LeftParen -> depth <- depth + 1
                    | RightParen -> depth <- depth - 1
                    | _ -> ()

                    endIndex <- endIndex + 1

                if depth > 0 then
                    diagnostics.AddError(DiagnosticCode.ParseError, "Unterminated projection binder.", source.GetLocation(startToken.Span))
                    index <- tokenArray.Length
                else
                    let innerTokens = List.ofArray tokenArray[index + 1 .. endIndex - 2]

                    match parseProjectionBinder startToken.Span innerTokens with
                    | Some binder -> binders.Add(binder)
                    | None -> ()

                    index <- endIndex
            | Identifier
            | Keyword _
            | Underscore ->
                let startToken = tokenArray[index]

                match parseProjectionBinder startToken.Span [ tokenArray[index] ] with
                | Some binder -> binders.Add(binder)
                | None -> ()

                index <- index + 1
            | _ ->
                diagnostics.AddError(DiagnosticCode.ParseError, "Unsupported projection binder syntax.", source.GetLocation(tokenArray[index].Span))
                index <- index + 1

        { Binders = List.ofSeq binders
          ReturnTypeTokens = returnTypeTokens }

    let rec parseProjectionBody
        (fixities: FixityTable)
        (source: SourceText)
        (diagnostics: DiagnosticBag)
        (tokens: Token list)
        : SurfaceProjectionBody option =
        let tokens = trimLeadingLayout tokens

        let parseExpr tokens =
            let parser = ExpressionParser(tokens, source, diagnostics, fixities)
            parser.Parse() |> Option.defaultValue (Literal LiteralValue.Unit)

        let parseYield rest =
            parseExpr rest |> ProjectionYield

        let tokenName (token: Token) =
            if Token.isName token then
                Some(SyntaxFacts.trimIdentifierQuotes token.Text)
            else
                None

        let isAccessorHead token =
            match tokenName token with
            | Some("get" | "set" | "sink") -> true
            | _ -> Token.isKeyword Keyword.Inout token

        let isNamedAccessor name token =
            match tokenName token with
            | Some tokenText -> String.Equals(tokenText, name, StringComparison.Ordinal)
            | None -> false

        let isInoutAccessor token =
            Token.isKeyword Keyword.Inout token

        let splitAccessorClauses tokens =
            let tokenArray = List.toArray tokens
            let starts = ResizeArray<int>()
            let mutable depth = 0
            let mutable atLineStart = true

            for index = 0 to tokenArray.Length - 1 do
                let token = tokenArray[index]

                if depth = 0 && atLineStart && isAccessorHead token then
                    starts.Add(index)

                match token.Kind with
                | LeftParen
                | LeftBracket
                | LeftBrace
                | LeftSetBrace ->
                    depth <- depth + 1
                    atLineStart <- false
                | RightParen
                | RightBracket
                | RightBrace
                | RightSetBrace ->
                    depth <- max 0 (depth - 1)
                    atLineStart <- false
                | Indent ->
                    depth <- depth + 1
                    atLineStart <- true
                | Dedent ->
                    depth <- max 0 (depth - 1)
                    atLineStart <- true
                | Newline ->
                    atLineStart <- true
                | _ ->
                    atLineStart <- false

            if starts.Count = 0 then
                None
            else
                let clauses =
                    [ for startIndexIndex = 0 to starts.Count - 1 do
                          let startIndex = starts[startIndexIndex]
                          let endIndex =
                              if startIndexIndex + 1 < starts.Count then
                                  starts[startIndexIndex + 1]
                              else
                                  tokenArray.Length

                          yield
                              tokenArray[startIndex .. endIndex - 1]
                              |> Array.toList
                              |> trimSignificantTokens ]
                    |> List.filter (List.isEmpty >> not)

                Some clauses

        let tryFindTopLevelArrow (tokens: Token list) =
            let tokenArray = List.toArray tokens
            let mutable depth = 0
            let mutable index = 0
            let mutable result = None

            while result.IsNone && index < tokenArray.Length do
                match tokenArray[index].Kind with
                | LeftParen
                | LeftBracket
                | LeftBrace
                | LeftSetBrace ->
                    depth <- depth + 1
                | RightParen
                | RightBracket
                | RightBrace
                | RightSetBrace ->
                    depth <- max 0 (depth - 1)
                | Arrow when depth = 0 ->
                    result <- Some index
                | _ ->
                    ()

                index <- index + 1

            result

        let parseSetClauseHeader startSpan headerTokens bodyTokens =
            match headerTokens with
            | setToken :: leftParen :: rest when isNamedAccessor "set" setToken && leftParen.Kind = LeftParen ->
                match List.rev rest with
                | rightParen :: reversedInner when rightParen.Kind = RightParen ->
                    let innerTokens = List.rev reversedInner

                    match SurfaceBinderParsing.parseParameterFromTokens diagnostics source startSpan innerTokens with
                    | Some parameter ->
                        match parameter.TypeTokens with
                        | Some typeTokens ->
                            if parameter.IsImplicit || parameter.IsInout || Option.isSome parameter.Quantity then
                                diagnostics.AddError(DiagnosticCode.ParseError, "Projection set accessors use an ordinary '(name : Type)' parameter.", source.GetLocation(startSpan))

                            Some(ProjectionSet(parameter.Name, typeTokens, parseExpr bodyTokens))
                        | None ->
                            diagnostics.AddError(DiagnosticCode.ParseError, "Projection set accessors require a typed parameter.", source.GetLocation(startSpan))
                            None
                    | None ->
                        None
                | _ ->
                    diagnostics.AddError(DiagnosticCode.ParseError, "Expected a projection set accessor parameter of the form '(name : Type)'.", source.GetLocation(startSpan))
                    None
            | token :: _ ->
                diagnostics.AddError(DiagnosticCode.ParseError, "Expected a projection set accessor.", source.GetLocation(token.Span))
                None
            | [] ->
                diagnostics.AddError(DiagnosticCode.ParseError, "Expected a projection set accessor.", source.GetLocation(TextSpan.FromBounds(source.Length, source.Length)))
                None

        let parseAccessorClause clauseTokens =
            match tryFindTopLevelArrow clauseTokens with
            | Some arrowIndex ->
                let tokenArray = List.toArray clauseTokens
                let headerTokens = tokenArray[0 .. arrowIndex - 1] |> Array.toList |> trimSignificantTokens
                let bodyTokens = tokenArray[arrowIndex + 1 ..] |> Array.toList

                match headerTokens with
                | [ token ] when isNamedAccessor "get" token ->
                    Some(ProjectionGet(parseExpr bodyTokens))
                | [ token ] when isInoutAccessor token ->
                    Some(ProjectionInout(parseExpr bodyTokens))
                | [ token ] when isNamedAccessor "sink" token ->
                    Some(ProjectionSink(parseExpr bodyTokens))
                | setToken :: _ when isNamedAccessor "set" setToken ->
                    parseSetClauseHeader setToken.Span headerTokens bodyTokens
                | token :: _ ->
                    diagnostics.AddError(DiagnosticCode.ParseError, "Expected a projection accessor clause.", source.GetLocation(token.Span))
                    None
                | [] ->
                    diagnostics.AddError(DiagnosticCode.ParseError, "Expected a projection accessor clause.", source.GetLocation(TextSpan.FromBounds(source.Length, source.Length)))
                    None
            | None ->
                match clauseTokens with
                | token :: _ ->
                    diagnostics.AddError(DiagnosticCode.ParseError, "Expected '->' in the projection accessor clause.", source.GetLocation(token.Span))
                | [] ->
                    diagnostics.AddError(DiagnosticCode.ParseError, "Expected '->' in the projection accessor clause.", source.GetLocation(TextSpan.FromBounds(source.Length, source.Length)))

                None

        let parseAccessorBody tokens =
            splitAccessorClauses tokens
            |> Option.bind (fun clauseTokens ->
                let clauses = clauseTokens |> List.choose parseAccessorClause

                if List.length clauses = List.length clauseTokens then
                    Some(ProjectionAccessors clauses)
                else
                    Some(ProjectionAccessors clauses))

        let parseIfBody rest =
            match splitTopLevelAtKeyword Keyword.Then rest with
            | None ->
                diagnostics.AddError(DiagnosticCode.ParseError, "Expected 'then' in the projection body.", source.GetLocation(TextSpan.FromBounds(source.Length, source.Length)))
                parseYield rest
            | Some(conditionTokens, afterThen) ->
                match splitTopLevelAtKeyword Keyword.Else afterThen with
                | None ->
                    diagnostics.AddError(DiagnosticCode.ParseError, "Expected 'else' in the projection body.", source.GetLocation(TextSpan.FromBounds(source.Length, source.Length)))
                    parseYield afterThen
                | Some(whenTrueTokens, whenFalseTokens) ->
                    let condition =
                        parseExpr conditionTokens

                    let whenTrue =
                        parseProjectionBody fixities source diagnostics whenTrueTokens
                        |> Option.defaultValue (ProjectionYield(Literal LiteralValue.Unit))

                    let whenFalse =
                        parseProjectionBody fixities source diagnostics whenFalseTokens
                        |> Option.defaultValue (ProjectionYield(Literal LiteralValue.Unit))

                    ProjectionIfThenElse(condition, whenTrue, whenFalse)

        let parseMatchBody rest =
            let splitCases tokens =
                match tokens with
                | newlineToken :: indentToken :: caseTokens when newlineToken.Kind = Newline && indentToken.Kind = Indent ->
                    let lines = ResizeArray<Token list>()
                    let current = ResizeArray<Token>()
                    let mutable depth = 1
                    let mutable finished = false

                    for token in caseTokens do
                        if not finished then
                            match token.Kind with
                            | Indent ->
                                depth <- depth + 1
                                current.Add(token)
                            | Dedent ->
                                depth <- depth - 1

                                if depth = 0 then
                                    if current.Count > 0 then
                                        lines.Add(List.ofSeq current)

                                    finished <- true
                                else
                                    current.Add(token)
                            | Newline when depth = 1 ->
                                lines.Add(List.ofSeq current)
                                current.Clear()
                            | _ ->
                                current.Add(token)

                    if finished then Some(lines |> Seq.toList |> List.filter (List.isEmpty >> not)) else None
                | _ ->
                    None

            let tryFindTopLevelToken predicate (tokens: Token list) =
                let tokenArray = List.toArray tokens
                let mutable depth = 0
                let mutable result = None
                let mutable index = 0

                while result.IsNone && index < tokenArray.Length do
                    match tokenArray[index].Kind with
                    | LeftParen
                    | LeftBracket
                    | LeftBrace
                    | LeftSetBrace ->
                        depth <- depth + 1
                    | RightParen
                    | RightBracket
                    | RightBrace
                    | RightSetBrace ->
                        depth <- max 0 (depth - 1)
                    | _ when depth = 0 && predicate tokenArray[index] ->
                        result <- Some index
                    | _ ->
                        ()

                    index <- index + 1

                result

            let scrutineeTokens, caseTokens =
                match List.tryFindIndex (fun token -> token.Kind = Newline) rest with
                | Some index ->
                    let tokenArray = List.toArray rest
                    tokenArray[0 .. index - 1] |> Array.toList,
                    tokenArray[index ..] |> Array.toList
                | None ->
                    diagnostics.AddError(DiagnosticCode.ParseError, "Expected an indented case block in the projection match body.", source.GetLocation(TextSpan.FromBounds(source.Length, source.Length)))
                    rest, []

            let scrutinee =
                parseExpr scrutineeTokens

            let cases =
                match splitCases caseTokens with
                | Some lines ->
                    lines
                    |> List.choose (fun lineTokens ->
                        match trimLeadingLayout lineTokens with
                        | caseToken :: restCaseTokens when Token.isKeyword Keyword.Case caseToken ->
                            match tryFindTopLevelToken (fun token -> token.Kind = Arrow) restCaseTokens with
                            | Some arrowIndex ->
                                let tokenArray = List.toArray restCaseTokens
                                let headTokens = tokenArray[0 .. arrowIndex - 1] |> Array.toList
                                let bodyTokens = tokenArray[arrowIndex + 1 ..] |> Array.toList
                                let guardIndex = tryFindTopLevelToken (fun token -> Token.isKeyword Keyword.If token) headTokens

                                let patternTokens, guardTokens =
                                    match guardIndex with
                                    | Some index ->
                                        let headArray = List.toArray headTokens
                                        headArray[0 .. index - 1] |> Array.toList,
                                        Some(headArray[index + 1 ..] |> Array.toList)
                                    | None ->
                                        headTokens, None

                                Some
                                    { Pattern =
                                        let nestedParser = PatternParser(patternTokens, source, diagnostics, fixities)
                                        nestedParser.Parse()
                                      Guard = guardTokens |> Option.map parseExpr
                                      Body =
                                        parseProjectionBody fixities source diagnostics bodyTokens
                                        |> Option.defaultValue (ProjectionYield(Literal LiteralValue.Unit)) }
                            | None ->
                                diagnostics.AddError(DiagnosticCode.ParseError, "Expected '->' in the projection case clause.", source.GetLocation(caseToken.Span))
                                None
                        | _ ->
                            diagnostics.AddError(DiagnosticCode.ParseError, "Expected a 'case' clause in the projection match body.", source.GetLocation(TextSpan.FromBounds(source.Length, source.Length)))
                            None)
                | None ->
                    diagnostics.AddError(DiagnosticCode.ParseError, "Expected an indented case block in the projection match body.", source.GetLocation(TextSpan.FromBounds(source.Length, source.Length)))
                    []

            ProjectionMatch(scrutinee, cases)

        match tokens with
        | head :: rest when Token.isKeyword Keyword.Yield head ->
            Some(parseYield rest)
        | head :: rest when Token.isKeyword Keyword.If head ->
            Some(parseIfBody rest)
        | head :: rest when Token.isKeyword Keyword.Match head ->
            Some(parseMatchBody rest)
        | head :: _ when isAccessorHead head ->
            parseAccessorBody tokens
        | head :: _ ->
            diagnostics.AddError(DiagnosticCode.ParseError, "Expected 'yield', 'if', or 'match' in the projection body.", source.GetLocation(head.Span))
            Some(parseYield tokens)
        | [] ->
            None

    let rec parseExpression
        (fixities: FixityTable)
        (source: SourceText)
        (diagnostics: DiagnosticBag)
        (tokens: Token list)
        : SurfaceExpression option =
        let significantTokens = trimLeadingLayout tokens

        let startsIndentedLetSequence =
            match significantTokens with
            | letToken :: _ when Token.isKeyword Keyword.Let letToken ->
                true
            | scopedToken :: effectToken :: nameToken :: _
                when Token.isName scopedToken
                     && String.Equals(SyntaxFacts.trimIdentifierQuotes scopedToken.Text, "scoped", StringComparison.Ordinal)
                     && Token.isName effectToken
                     && String.Equals(SyntaxFacts.trimIdentifierQuotes effectToken.Text, "effect", StringComparison.Ordinal)
                     && Token.isName nameToken ->
                true
            | _ ->
                false

        if startsIndentedLetSequence then
            match tryParseIndentedLocalLetSequence fixities source diagnostics tokens with
            | Some expression ->
                Some expression
            | None ->
                let parser = ExpressionParser(tokens, source, diagnostics, fixities)
                parser.Parse()
        else
            let parser = ExpressionParser(tokens, source, diagnostics, fixities)
            parser.Parse()

    and private tryParseIndentedLocalLetSequence
        (fixities: FixityTable)
        (source: SourceText)
        (diagnostics: DiagnosticBag)
        (tokens: Token list)
        : SurfaceExpression option =
        let trimmedLeading =
            trimLeadingLayout tokens

        let splitIndentedLines remainingTokens =
            let lineEndsWithIndentedContinuation (tokens: ResizeArray<Token>) =
                tokens
                |> Seq.tryFindBack (fun token ->
                    match token.Kind with
                    | Newline
                    | Indent
                    | Dedent -> false
                    | _ -> true)
                |> Option.exists (fun token -> token.Kind = Arrow)

            match remainingTokens with
            | { Kind = Indent } :: rest ->
                let lines = ResizeArray<Token list>()
                let current = ResizeArray<Token>()
                let mutable depth = 1
                let mutable finished = false
                let mutable pendingBaseSplit = false

                for token in rest do
                    if not finished then
                        if pendingBaseSplit && token.Kind <> Newline then
                            lines.Add(List.ofSeq current)
                            current.Clear()
                            pendingBaseSplit <- false

                        match token.Kind with
                        | Indent ->
                            depth <- depth + 1
                            current.Add(token)
                        | Dedent ->
                            depth <- depth - 1

                            if depth = 0 then
                                if current.Count > 0 then
                                    lines.Add(List.ofSeq current)

                                finished <- true
                            else
                                current.Add(token)
                                pendingBaseSplit <- depth = 1
                        | Newline when depth = 1 && lineEndsWithIndentedContinuation current ->
                            current.Add(token)
                        | Newline when depth = 1 ->
                            lines.Add(List.ofSeq current)
                            current.Clear()
                            pendingBaseSplit <- false
                        | _ ->
                            current.Add(token)

                if not finished then
                    None
                else
                    Some(lines |> Seq.toList |> List.filter (List.isEmpty >> not))
            | { Kind = Newline } :: { Kind = Indent } :: rest ->
                let lines = ResizeArray<Token list>()
                let current = ResizeArray<Token>()
                let mutable depth = 1
                let mutable finished = false
                let mutable pendingBaseSplit = false

                for token in rest do
                    if not finished then
                        if pendingBaseSplit && token.Kind <> Newline then
                            lines.Add(List.ofSeq current)
                            current.Clear()
                            pendingBaseSplit <- false

                        match token.Kind with
                        | Indent ->
                            depth <- depth + 1
                            current.Add(token)
                        | Dedent ->
                            depth <- depth - 1

                            if depth = 0 then
                                if current.Count > 0 then
                                    lines.Add(List.ofSeq current)

                                finished <- true
                            else
                                current.Add(token)
                                pendingBaseSplit <- depth = 1
                        | Newline when depth = 1 && lineEndsWithIndentedContinuation current ->
                            current.Add(token)
                        | Newline when depth = 1 ->
                            lines.Add(List.ofSeq current)
                            current.Clear()
                            pendingBaseSplit <- false
                        | _ ->
                            current.Add(token)

                if not finished then
                    None
                else
                    Some(lines |> Seq.toList |> List.filter (List.isEmpty >> not))
            | _ ->
                None

        let tryFindTopLevelEquals tokens =
            let tokenArray = List.toArray tokens
            let mutable depth = 0
            let mutable index = 0
            let mutable result = None

            while index < tokenArray.Length && result.IsNone do
                match tokenArray[index].Kind with
                | LeftParen
                | LeftBracket
                | LeftBrace
                | LeftSetBrace ->
                    depth <- depth + 1
                | RightParen
                | RightBracket
                | RightBrace
                | RightSetBrace ->
                    depth <- max 0 (depth - 1)
                | Equals when depth = 0 ->
                    result <- Some index
                | _ ->
                    ()

                index <- index + 1

            result

        let stripBindingTypeAnnotation tokens =
            let tokenArray = List.toArray tokens
            let mutable depth = 0
            let mutable index = 0
            let mutable splitIndex = -1

            while index < tokenArray.Length && splitIndex < 0 do
                match tokenArray[index].Kind with
                | LeftParen
                | LeftBracket
                | LeftBrace
                | LeftSetBrace ->
                    depth <- depth + 1
                | RightParen
                | RightBracket
                | RightBrace
                | RightSetBrace ->
                    depth <- max 0 (depth - 1)
                | Colon when depth = 0 ->
                    splitIndex <- index
                | _ ->
                    ()

                index <- index + 1

            if splitIndex >= 0 then
                tokenArray[0 .. splitIndex - 1] |> Array.toList
            else
                tokens

        let parseBindingLineCore (bindingTokens: Token list) (valueTokens: Token list) =
            SurfaceBinderParsing.parseBindPatternFromTokens
                (fun tokens ->
                    let nestedParser = PatternParser(tokens, source, diagnostics, fixities)
                    nestedParser.Parse())
                bindingTokens,
            parseExpression fixities source diagnostics valueTokens
            |> Option.defaultValue (Literal LiteralValue.Unit)

        let parseBindingLineTokens (tokens: Token list) =
            match tryFindTopLevelEquals tokens with
            | Some index ->
                let tokenArray = List.toArray tokens
                let bindingTokens =
                    tokenArray[0 .. index - 1]
                    |> Array.toList
                    |> stripBindingTypeAnnotation
                let valueTokens = tokenArray[index + 1 ..] |> Array.toList
                Some(parseBindingLineCore bindingTokens valueTokens)
            | None ->
                None

        let trimLineTokens lineTokens =
            let isLeadingLayoutToken token =
                match token.Kind with
                | Newline
                | Indent
                | Dedent -> true
                | _ -> false

            lineTokens
            |> List.skipWhile isLeadingLayoutToken
            |> List.rev
            |> List.skipWhile (fun token -> token.Kind = Newline)
            |> List.rev

        let parseSimpleLocalBindingLine lineTokens =
            trimLineTokens lineTokens |> parseBindingLineTokens |> Option.map Choice1Of2

        let parseSimpleLocalLetLine lineTokens =
            match trimLineTokens lineTokens with
            | letToken :: rest when Token.isKeyword Keyword.Let letToken ->
                parseBindingLineTokens rest |> Option.map Choice1Of2
            | scopedToken :: effectToken :: nameToken :: rest
                when Token.isName scopedToken
                     && String.Equals(SyntaxFacts.trimIdentifierQuotes scopedToken.Text, "scoped", StringComparison.Ordinal)
                     && Token.isName effectToken
                     && String.Equals(SyntaxFacts.trimIdentifierQuotes effectToken.Text, "effect", StringComparison.Ordinal)
                     && Token.isName nameToken
                     && (rest |> List.exists (fun token -> token.Kind = Equals)) ->
                Some(Choice2Of2(SyntaxFacts.trimIdentifierQuotes nameToken.Text))
            | _ ->
                None

        let joinIndentedBodyLines (lines: Token list list) =
            let syntheticNewline span =
                { Kind = Newline
                  Text = ""
                  Span = span }

            lines
            |> List.mapi (fun index line ->
                if index = 0 || List.isEmpty line then
                    line
                else
                    syntheticNewline line.Head.Span :: line)
            |> List.concat

        let buildNestedExpression parseBindingLine bindingLines bodyTokens =
            let body : SurfaceExpression =
                parseExpression fixities source diagnostics bodyTokens
                |> Option.defaultValue (Literal LiteralValue.Unit)

            bindingLines
            |> List.rev
            |> List.fold (fun (current: SurfaceExpression) lineTokens ->
                match parseBindingLine lineTokens with
                | Some(Choice1Of2(binding, value)) -> LocalLet(binding, value, current)
                | Some(Choice2Of2 name) -> LocalScopedEffect(name, current)
                | None -> current) body

        let tryParseIndentedPureBlockSuite () =
            match splitIndentedLines tokens with
            | Some lines ->
                let parsedLines =
                    lines
                    |> List.map (fun line -> line, parseSimpleLocalLetLine line)

                let bindingLines, bodyLines =
                    parsedLines |> List.takeWhile (snd >> Option.isSome),
                    parsedLines |> List.skipWhile (snd >> Option.isSome)

                match bindingLines, bodyLines with
                | _ :: _, _ :: _ ->
                    Some(buildNestedExpression parseSimpleLocalLetLine (bindingLines |> List.map fst) (bodyLines |> List.map fst |> joinIndentedBodyLines))
                | _ ->
                    None
            | _ ->
                None

        let tryParseLetInExpression rest =
            match splitIndentedLines rest with
            | Some bindingLines ->
                let afterBindings =
                    rest
                    |> List.skipWhile (fun token -> token.Kind <> Dedent)
                    |> function
                        | _ :: remaining -> trimLeadingLayout remaining
                        | [] -> []

                match afterBindings with
                | inToken :: bodyTokens
                    when Token.isKeyword Keyword.In inToken
                         && not (List.isEmpty bindingLines)
                         && (bindingLines |> List.forall (parseSimpleLocalBindingLine >> Option.isSome)) ->
                    Some(buildNestedExpression parseSimpleLocalBindingLine bindingLines bodyTokens)
                | _ ->
                    None
            | None ->
                match splitTopLevelAtKeyword Keyword.In rest with
                | Some(bindingTokens, bodyTokens) ->
                    match parseBindingLineTokens bindingTokens with
                    | Some(binding, value) ->
                        let body =
                            parseExpression fixities source diagnostics bodyTokens
                            |> Option.defaultValue (Literal LiteralValue.Unit)

                        Some(LocalLet(binding, value, body))
                    | None ->
                        None
                | None ->
                    None

        match trimmedLeading with
        | letToken :: rest when Token.isKeyword Keyword.Let letToken ->
            match tryParseLetInExpression rest with
            | Some expression -> Some expression
            | None -> tryParseIndentedPureBlockSuite ()
        | _ ->
            tryParseIndentedPureBlockSuite ()
