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
    | LocalPureScopedEffect of EffectDeclaration

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
            Some({ head with Kind = Identifier; Text = CompilerKnownSymbols.KnownTypeNames.Thunk }, rest)
        | head :: rest when Token.isKeyword Keyword.Lazy head ->
            Some({ head with Kind = Identifier; Text = CompilerKnownSymbols.KnownTypeNames.Need }, rest)
        | _ ->
            None

    let parseParameterFromTokens (diagnostics: DiagnosticBag) (source: SourceText) eofSpan (tokens: Token list) =
        let unsupported span =
            diagnostics.AddError(DiagnosticFact.corePatternParsing UnsupportedParameterBinderSyntax, source.GetLocation(span))

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
                diagnostics.AddError(DiagnosticFact.corePatternParsing ExpectedParameterBinder, source.GetLocation(eofSpan))
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
                diagnostics.AddError(DiagnosticFact.corePatternParsing ExpectedParameterName, source.GetLocation(head.Span))
                None

        match tokens with
        | [] ->
            diagnostics.AddError(DiagnosticFact.corePatternParsing ExpectedParameterBinder, source.GetLocation(eofSpan))
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

module private SurfaceEffectParsing =
    let isContextualKeyword text (token: Token) =
        token.Kind = Identifier
        && String.Equals(SyntaxFacts.trimIdentifierQuotes token.Text, text, StringComparison.Ordinal)

    let splitNestedIndentedLines (tokens: Token list) =
        let lines = ResizeArray<Token list>()
        let tokenArray = List.toArray tokens
        let mutable position = 0

        let current () =
            if position < tokenArray.Length then
                tokenArray[position]
            else
                { Kind = EndOfFile
                  Text = ""
                  Span =
                    if Array.isEmpty tokenArray then
                        TextSpan.FromBounds(0, 0)
                    else
                        tokenArray[tokenArray.Length - 1].Span }

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

                    if nestedIndents = 0 then
                        flushLine currentLine
                | _ ->
                    currentLine.Add(advance ())

            flushLine currentLine
            lines |> Seq.toList |> List.filter (List.isEmpty >> not)
        else
            []

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
        | head :: rest when Token.isName head && String.Equals(SyntaxFacts.trimIdentifierQuotes head.Text, "omega", StringComparison.Ordinal) ->
            Some(QuantityOmega, rest)
        | head :: rest when Token.isName head && String.Equals(SyntaxFacts.trimIdentifierQuotes head.Text, "\u03c9", StringComparison.Ordinal) ->
            Some(QuantityOmega, rest)
        | _ ->
            None

    let parseOperationLine (lineTokens: Token list) =
        let significantTokens =
            lineTokens
            |> List.filter (fun token ->
                match token.Kind with
                | Newline
                | Indent
                | Dedent
                | EndOfFile -> false
                | _ -> true)

        let quantity, remainingTokens =
            match tryParseQuantityPrefix significantTokens with
            | Some(quantity, rest) -> Some quantity, rest
            | None -> None, significantTokens

        match remainingTokens with
        | nameToken :: colonToken :: signatureTokens when Token.isName nameToken && colonToken.Kind = Colon ->
            Some
                { OperationId = None
                  Name = SyntaxFacts.trimIdentifierQuotes nameToken.Text
                  ResumptionQuantity = quantity
                  SignatureTokens = signatureTokens }
        | _ ->
            None

    let parseScopedEffectDeclaration (lineTokens: Token list) =
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
            |> List.skipWhile isLeadingLayoutToken
            |> List.rev

        match trimLineTokens lineTokens with
        | scopedToken :: effectToken :: nameToken :: rest
            when isContextualKeyword "scoped" scopedToken
                 && isContextualKeyword "effect" effectToken
                 && Token.isName nameToken ->
            let name = SyntaxFacts.trimIdentifierQuotes nameToken.Text
            let rec splitHeader depth collected remaining =
                match remaining with
                | [] ->
                    List.rev collected, []
                | token :: tail when token.Kind = LeftParen ->
                    splitHeader (depth + 1) (token :: collected) tail
                | token :: tail when token.Kind = RightParen ->
                    splitHeader (max 0 (depth - 1)) (token :: collected) tail
                | token :: tail when token.Kind = Equals && depth = 0 ->
                    List.rev collected, tail
                | token :: tail ->
                    splitHeader depth (token :: collected) tail

            let headerTokens, bodyTokens = splitHeader 0 [] rest

            let operations =
                if List.isEmpty bodyTokens && not (rest |> List.exists (fun token -> token.Kind = Equals)) then
                    []
                else
                    splitNestedIndentedLines bodyTokens
                    |> List.choose parseOperationLine

            Some
                { EffectInterfaceId = None
                  EffectLabelId = None
                  Visibility = None
                  Name = name
                  HeaderTokens = headerTokens
                  Operations = operations }
        | _ ->
            None

module private HeaderParsing =
    let parseHeaderParameters
        (source: SourceText)
        (diagnostics: DiagnosticBag)
        (context: CoreHeaderContext)
        (skipOperatorToken: bool)
        (parameterTokens: Token list)
        =
        let tokenArray = List.toArray parameterTokens
        let parameters = ResizeArray<Parameter>()
        let mutable index = 0

        while index < tokenArray.Length do
            match tokenArray[index].Kind with
            | AtSign ->
                if index + 1 >= tokenArray.Length then
                    diagnostics.AddError(
                        DiagnosticFact.coreExpressionParsing (ExpectedImplicitParameterAfterAt context),
                        source.GetLocation(tokenArray[index].Span)
                    )

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
                            diagnostics.AddError(
                                DiagnosticFact.coreExpressionParsing (UnterminatedImplicitParameterBinder context),
                                source.GetLocation(startToken.Span)
                            )

                            index <- tokenArray.Length
                        else
                            let innerTokens = List.ofArray tokenArray[index + 2 .. endIndex - 2]

                            match SurfaceBinderParsing.parseParameterFromTokens diagnostics source startToken.Span innerTokens with
                            | Some parameter -> parameters.Add({ parameter with IsImplicit = true })
                            | None -> ()

                            index <- endIndex
                    | _ ->
                        diagnostics.AddError(
                            DiagnosticFact.coreExpressionParsing (UnsupportedImplicitParameterSyntax context),
                            source.GetLocation(tokenArray[index + 1].Span)
                        )

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
            | Operator when skipOperatorToken ->
                // In an infix binding header like `let x |> f = ...`, the operator token is the
                // binding name and the surrounding tokens are the real parameters.
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
                    diagnostics.AddError(
                        DiagnosticFact.coreExpressionParsing (UnterminatedParameterBinderInHeader context),
                        source.GetLocation(startToken.Span)
                    )

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
                diagnostics.AddError(
                    DiagnosticFact.coreExpressionParsing (UnsupportedHeaderSyntax context),
                    source.GetLocation(tokenArray[index].Span)
                )

                index <- index + 1

        List.ofSeq parameters

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
            diagnostics.AddError(DiagnosticFact.corePatternParsing ExpectedPatternName, source.GetLocation(this.Current.Span))

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
                diagnostics.AddError(DiagnosticFact.corePatternParsing OrPatternAlternativesMustBindSameNames,
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
                    diagnostics.AddError(DiagnosticFact.corePatternParsing (NumericLiteralSuffixesNotPermittedInPatterns token.Text),
                        source.GetLocation(token.Span)
                    )

                    match SurfaceNumericLiteral.tryLowerPrimitiveLiteral parsed.Literal with
                    | Some literal ->
                        LiteralPattern literal
                    | None ->
                        diagnostics.AddError(DiagnosticFact.corePatternParsing (NumericLiteralNotRepresentableInPatterns token.Text),
                            source.GetLocation(token.Span)
                        )

                        LiteralPattern(LiteralValue.Integer 0L)
                | None ->
                    match SurfaceNumericLiteral.tryLowerPrimitiveLiteral parsed.Literal with
                    | Some literal ->
                        LiteralPattern literal
                    | None ->
                        diagnostics.AddError(DiagnosticFact.corePatternParsing (NumericLiteralNotRepresentableInPatterns token.Text),
                            source.GetLocation(token.Span)
                        )

                        LiteralPattern(LiteralValue.Integer 0L)
            | Result.Error error ->
                diagnostics.AddError(
                    DiagnosticFact.corePatternParsing (InvalidNumericLiteralPattern error),
                    source.GetLocation(token.Span)
                )
                LiteralPattern(LiteralValue.Integer 0L)
        | FloatLiteral ->
            match SyntaxFacts.tryParseNumericLiteral token.Text with
            | Result.Ok parsed ->
                match parsed.Suffix with
                | Some _ ->
                    diagnostics.AddError(DiagnosticFact.corePatternParsing (NumericLiteralSuffixesNotPermittedInPatterns token.Text),
                        source.GetLocation(token.Span)
                    )

                    match SurfaceNumericLiteral.tryLowerPrimitiveLiteral parsed.Literal with
                    | Some literal ->
                        LiteralPattern literal
                    | None ->
                        diagnostics.AddError(DiagnosticFact.corePatternParsing (NumericLiteralNotRepresentableInPatterns token.Text),
                            source.GetLocation(token.Span)
                        )

                        LiteralPattern(LiteralValue.Float 0.0)
                | None ->
                    match SurfaceNumericLiteral.tryLowerPrimitiveLiteral parsed.Literal with
                    | Some literal ->
                        LiteralPattern literal
                    | None ->
                        diagnostics.AddError(DiagnosticFact.corePatternParsing (NumericLiteralNotRepresentableInPatterns token.Text),
                            source.GetLocation(token.Span)
                        )

                        LiteralPattern(LiteralValue.Float 0.0)
            | Result.Error error ->
                diagnostics.AddError(
                    DiagnosticFact.corePatternParsing (InvalidNumericLiteralPattern error),
                    source.GetLocation(token.Span)
                )
                LiteralPattern(LiteralValue.Float 0.0)
        | StringLiteral ->
            match SyntaxFacts.tryDecodeStringLiteral token.Text with
            | Result.Ok value -> LiteralPattern(LiteralValue.String value)
            | Result.Error error ->
                diagnostics.AddError(
                    DiagnosticFact.corePatternParsing (InvalidStringLiteralPattern error),
                    source.GetLocation(token.Span)
                )
                LiteralPattern(LiteralValue.String(SyntaxFacts.trimStringQuotes token.Text))
        | CharacterLiteral ->
            match SyntaxFacts.tryDecodeCharacterLiteral token.Text with
            | Result.Ok value -> LiteralPattern(LiteralValue.Character value)
            | Result.Error error ->
                diagnostics.AddError(
                    DiagnosticFact.unicodeInvalidScalarLiteral error,
                    source.GetLocation(token.Span)
                )
                LiteralPattern(LiteralValue.Character "\u0000")
        | _ ->
            diagnostics.AddError(DiagnosticFact.corePatternParsing ExpectedLiteralPattern, source.GetLocation(token.Span))
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
            diagnostics.AddError(DiagnosticFact.corePatternParsing ExpectedPatternCloseParenthesis, source.GetLocation(start.Span))

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
                | [ { Kind = Operator; Text = ".." }; nameToken ] when this.IsNameToken(nameToken) ->
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
            | { Kind = Operator; Text = ".." } :: [] -> true
            | { Kind = Operator; Text = ".." } :: nameToken :: [] when this.IsNameToken(nameToken) -> true
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
                            diagnostics.AddError(DiagnosticFact.corePatternParsing ExpectedRecordPatternFieldLabel, source.GetLocation(labelToken.Span))
                            Some
                                { Name = "<missing>"
                                  IsImplicit = false
                                  Pattern = WildcardPattern }
                        | [] ->
                            diagnostics.AddError(DiagnosticFact.corePatternParsing ExpectedRecordPatternFieldLabel, source.GetLocation(eofSpan))
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
                        | { Kind = Operator; Text = ".." } :: [] ->
                            rest <- Some DiscardRecordPatternRest
                            None
                        | { Kind = Operator; Text = ".." } :: nameToken :: [] when this.IsNameToken(nameToken) ->
                            rest <- Some(BindRecordPatternRest(SyntaxFacts.trimIdentifierQuotes nameToken.Text))
                            None
                        | _ ->
                            let errorSpan =
                                match fieldTokens with
                                | token :: _ -> token.Span
                                | [] -> eofSpan

                            diagnostics.AddError(DiagnosticFact.corePatternParsing ExpectedRecordPatternField, source.GetLocation(errorSpan))
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
            diagnostics.AddError(DiagnosticFact.corePatternParsing ExpectedPatternCloseBrace, source.GetLocation(start.Span))

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
                        diagnostics.AddError(DiagnosticFact.corePatternParsing ExpectedNamedConstructorPatternFieldLabel, source.GetLocation(labelToken.Span))
                        { Name = "<missing>"
                          IsImplicit = false
                          Pattern = WildcardPattern }
                    | [] ->
                        diagnostics.AddError(DiagnosticFact.corePatternParsing ExpectedNamedConstructorPatternFieldLabel, source.GetLocation(eofSpan))
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
                        diagnostics.AddError(DiagnosticFact.corePatternParsing ExpectedNamedConstructorPatternField, source.GetLocation(token.Span))
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
            diagnostics.AddError(DiagnosticFact.corePatternParsing ExpectedPattern, source.GetLocation(this.Current.Span))
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
            diagnostics.AddError(DiagnosticFact.corePatternParsing NamedConstructorPatternsCannotTakePositionalSubpatterns, source.GetLocation(this.Current.Span))
            head
        | NamePattern name, [] ->
            NamePattern name
        | NamePattern name, additionalArguments ->
            ConstructorPattern([ name ], additionalArguments)
        | literalPattern, [] ->
            literalPattern
        | _, _ ->
            diagnostics.AddError(DiagnosticFact.corePatternParsing OnlyConstructorPatternsMayTakeArguments, source.GetLocation(this.Current.Span))
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
                diagnostics.AddError(DiagnosticFact.corePatternParsing UnexpectedTrailingPatternTokens, source.GetLocation(this.Current.Span))

            pattern

type private CollectionOrderedness =
    | KnownOrdered
    | KnownUnordered
    | UnknownOrderedness

type private ExpressionParser
    (
        tokens: Token list,
        source: SourceText,
        diagnostics: DiagnosticBag,
        fixities: FixityTable,
        inSyntaxQuote: bool,
        inCodeQuote: bool
    ) =
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

    member private this.TryParseNestedExpressionWithoutDiagnostics(tokens: Token list, nestedInSyntaxQuote: bool, nestedInCodeQuote: bool) =
        let nestedDiagnostics = DiagnosticBag()
        let nestedParser = ExpressionParser(tokens, source, nestedDiagnostics, fixities, nestedInSyntaxQuote, nestedInCodeQuote)

        match nestedParser.Parse() with
        | Some expression when not nestedDiagnostics.HasErrors ->
            Some expression
        | _ ->
            None

    member private this.ParseNestedExpression(tokens: Token list, nestedInSyntaxQuote: bool, nestedInCodeQuote: bool) =
        let nestedParser = ExpressionParser(tokens, source, diagnostics, fixities, nestedInSyntaxQuote, nestedInCodeQuote)
        nestedParser.Parse() |> Option.defaultValue (Literal LiteralValue.Unit)

    member private this.ParseNestedSyntaxPayload(tokens: Token list) =
        match this.TryParseNestedExpressionWithoutDiagnostics(tokens, true, false) with
        | Some expression ->
            expression
        | None ->
            match TypeSignatures.parseType tokens with
            | Some _ -> TypeSyntaxTokens tokens
            | None -> this.ParseNestedExpression(tokens, true, false)

    member private this.ParseParenthesizedExplicitImplicitArgument(tokens: Token list) =
        match this.TryParseNestedExpressionWithoutDiagnostics(tokens, false, false) with
        | Some expression ->
            ExplicitImplicitArgument expression
        | None ->
            match TypeSignatures.parseType tokens with
            | Some _ -> ExplicitImplicitArgument(TypeSyntaxTokens tokens)
            | None -> ExplicitImplicitArgument(this.ParseNestedExpression(tokens, false, false))

    member private this.ExpectKeyword(keyword: Keyword, message: string) =
        this.SkipLayout()

        if Token.isKeyword keyword this.Current then
            this.Advance() |> ignore
            true
        else
            diagnostics.AddError(DiagnosticFact.simple SimpleDiagnosticKind.ExpectedSyntaxToken message, source.GetLocation(this.Current.Span))
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
        | LeftBracket
        | LeftSetBrace
        | AtSign
        | Backslash -> true
        | Dot when inCodeQuote && this.Peek(1).Kind = Operator && this.Peek(1).Text = "~" -> true
        | Dot when this.Peek(1).Kind = Operator && this.Peek(1).Text = "<" -> true
        | Operator when token.Text = "'" && this.Peek(1).Kind = LeftBrace -> true
        | Operator when token.Text = "$" && (this.Peek(1).Kind = LeftParen || (inSyntaxQuote && this.Peek(1).Kind = LeftBrace)) -> true
        | Operator when FixityTable.tryFindPrefix token.Text fixities |> Option.isSome -> true
        | Keyword Keyword.If
        | Keyword Keyword.Seal -> true
        | _ -> this.IsNameToken(token)

    member private this.DecodeStringLiteral(token: Token) =
        match SyntaxFacts.tryDecodeStringLiteral token.Text with
        | Result.Ok value ->
            value
        | Result.Error error ->
            diagnostics.AddError(
                DiagnosticFact.coreExpressionParsing (InvalidStringLiteralExpression error),
                source.GetLocation(token.Span)
            )
            SyntaxFacts.trimStringQuotes token.Text

    member private this.DecodeCharacterLiteral(token: Token) =
        match SyntaxFacts.tryDecodeCharacterLiteral token.Text with
        | Result.Ok value ->
            value
        | Result.Error error ->
            diagnostics.AddError(
                DiagnosticFact.unicodeInvalidScalarLiteral error,
                source.GetLocation(token.Span)
            )
            "\u0000"

    member private this.DecodeGraphemeLiteral(prefixToken: Token, token: Token) =
        let location =
            source.GetLocation(TextSpan.FromBounds(prefixToken.Span.Start, token.Span.End))

        match SyntaxFacts.tryDecodeGraphemeLiteral token.Text with
        | Result.Ok value ->
            value
        | Result.Error error ->
            diagnostics.AddError(
                DiagnosticFact.unicodeInvalidGraphemeLiteral error,
                location
            )
            "\u0000"

    member private this.DecodeByteLiteral(prefixToken: Token, token: Token) =
        let location =
            source.GetLocation(TextSpan.FromBounds(prefixToken.Span.Start, token.Span.End))

        match SyntaxFacts.tryDecodeByteLiteral token.Text with
        | Result.Ok value ->
            value
        | Result.Error error ->
            diagnostics.AddError(
                DiagnosticFact.unicodeInvalidByteLiteral error,
                location
            )
            0uy

    member private this.IsAdjacentPrefixedCharacterLiteral(prefix: string) =
        this.Current.Kind = Identifier
        && String.Equals(this.Current.Text, prefix, StringComparison.Ordinal)
        && this.Peek(1).Kind = CharacterLiteral
        && this.Current.Span.End = this.Peek(1).Span.Start

    member private this.CollectBracketedTokens(errorMessage: string) =
        let start = this.Current
        this.Advance() |> ignore

        let innerTokens = ResizeArray<Token>()
        let mutable depth = 1

        while depth > 0 && this.Current.Kind <> EndOfFile do
            match this.Current.Kind with
            | LeftBracket ->
                depth <- depth + 1
                innerTokens.Add(this.Advance())
            | RightBracket ->
                depth <- depth - 1

                if depth > 0 then
                    innerTokens.Add(this.Advance())
                else
                    this.Advance() |> ignore
            | _ ->
                innerTokens.Add(this.Advance())

        if depth > 0 then
            diagnostics.AddError(DiagnosticFact.simple SimpleDiagnosticKind.ExpectedSyntaxToken errorMessage, source.GetLocation(start.Span))

        List.ofSeq innerTokens

    member private this.CollectSetBracedTokens(errorMessage: string) =
        let start = this.Current
        this.Advance() |> ignore

        let innerTokens = ResizeArray<Token>()
        let mutable depth = 1

        while depth > 0 && this.Current.Kind <> EndOfFile do
            match this.Current.Kind with
            | LeftSetBrace ->
                depth <- depth + 1
                innerTokens.Add(this.Advance())
            | RightSetBrace ->
                depth <- depth - 1

                if depth > 0 then
                    innerTokens.Add(this.Advance())
                else
                    this.Advance() |> ignore
            | _ ->
                innerTokens.Add(this.Advance())

        if depth > 0 then
            diagnostics.AddError(DiagnosticFact.simple SimpleDiagnosticKind.ExpectedSyntaxToken errorMessage, source.GetLocation(start.Span))

        List.ofSeq innerTokens

    member private this.ParseCollectionExpression(kind: SurfaceCollectionKind, innerTokens: Token list) =
        let isContextualName name (token: Token) =
            Token.isName token && String.Equals(SyntaxFacts.trimIdentifierQuotes token.Text, name, StringComparison.Ordinal)

        let listNil = Name [ "Nil" ]
        let boolTrue = Name [ "True" ]
        let boolFalse = Name [ "False" ]
        let preludeResConstructorName = [ CompilerKnownSymbols.KnownTypeNames.Res; ":&" ]
        let preludeResConstructorExpression = MemberAccess(Name [ CompilerKnownSymbols.KnownTypeNames.Res ], [ ":&" ], [])

        let cons head tail =
            Apply(Name [ "::" ], [ head; tail ])

        let applyPreludeResConstructor left right =
            Apply(preludeResConstructorExpression, [ left; right ])

        let makeListExpression (items: SurfaceExpression list) =
            List.foldBack cons items listNil

        let makeSetExpression (items: SurfaceExpression list) =
            Apply(Name [ CompilerKnownSymbols.KnownTypeNames.Set ], [ makeListExpression items ])

        let makeMapExpression (entries: (SurfaceExpression * SurfaceExpression) list) =
            let entryExpressions =
                entries
                |> List.map (fun (key, value) -> applyPreludeResConstructor key value)

            Apply(Name [ CompilerKnownSymbols.KnownTypeNames.Map ], [ makeListExpression entryExpressions ])

        let rec patternIsDefinitelyIrrefutable pattern =
            match pattern with
            | WildcardPattern
            | NamePattern _ ->
                true
            | AsPattern(_, inner)
            | TypedPattern(inner, _) ->
                patternIsDefinitelyIrrefutable inner
            | TuplePattern elements ->
                elements |> List.forall patternIsDefinitelyIrrefutable
            | AnonymousRecordPattern(fields, _) ->
                fields |> List.forall (fun field -> patternIsDefinitelyIrrefutable field.Pattern)
            | LiteralPattern _
            | ConstructorPattern _
            | NamedConstructorPattern _
            | VariantPattern _
            | OrPattern _ ->
                false

        let patternBoundNames pattern =
            let rec loop current =
                match current with
                | WildcardPattern
                | LiteralPattern _ -> []
                | NamePattern name -> [ name ]
                | AsPattern(name, inner) -> name :: loop inner
                | TypedPattern(inner, _) -> loop inner
                | ConstructorPattern(_, arguments) ->
                    arguments |> List.collect loop
                | NamedConstructorPattern(_, fields) ->
                    fields |> List.collect (fun field -> loop field.Pattern)
                | TuplePattern elements ->
                    elements |> List.collect loop
                | VariantPattern variant ->
                    match variant with
                    | BoundVariantPattern(name, _) -> [ name ]
                    | WildcardVariantPattern _ -> []
                    | RestVariantPattern name -> [ name ]
                | OrPattern alternatives ->
                    alternatives |> List.tryHead |> Option.map loop |> Option.defaultValue []
                | AnonymousRecordPattern(fields, rest) ->
                    let fieldNames = fields |> List.collect (fun field -> loop field.Pattern)
                    let restNames =
                        match rest with
                        | Some(BindRecordPatternRest name) -> [ name ]
                        | _ -> []

                    fieldNames @ restNames

            loop pattern

        let extendRowNames existingNames newNames =
            let newNameSet = newNames |> Set.ofList

            (existingNames |> List.filter (fun name -> not (Set.contains name newNameSet)))
            @ newNames

        let makeUnitDo statements =
            Do(statements @ [ DoExpression(Literal LiteralValue.Unit) ])

        let makeNamePatternBinding name =
            SurfaceBinderParsing.makeBindPattern (NamePattern name) None []

        let makeParameter name =
            { Name = name
              TypeTokens = None
              Quantity = None
              IsImplicit = false
              IsInout = false
              IsReceiver = false }

        let bindName name value body =
            LocalLet(makeNamePatternBinding name, value, body)

        let applyName name arguments =
            Apply(Name [ name ], arguments)

        let rec makeRowExpression rowNames =
            match rowNames with
            | [] ->
                Literal LiteralValue.Unit
            | [ name ] ->
                Name [ name ]
            | name :: rest ->
                applyPreludeResConstructor (Name [ name ]) (makeRowExpression rest)

        let wrapPatternBindings binding value body =
            LocalLet(binding, value, body)

        let makePatternMatch value success failure =
            Match(
                value,
                [
                    { Pattern = success
                      Guard = None
                      Body = boolTrue }
                    { Pattern = WildcardPattern
                      Guard = None
                      Body = boolFalse }
                ]
            )

        let wrapRowBindings rowNames rowExpression body =
            match rowNames with
            | [] ->
                body
            | [ name ] ->
                wrapPatternBindings (makeNamePatternBinding name) rowExpression body
            | _ ->
                let rec makeRowPattern names =
                    match names with
                    | [] -> WildcardPattern
                    | [ name ] -> NamePattern name
                    | name :: rest -> ConstructorPattern(preludeResConstructorName, [ NamePattern name; makeRowPattern rest ])

                wrapPatternBindings
                    (SurfaceBinderParsing.makeBindPattern (makeRowPattern rowNames) None [])
                    rowExpression
                    body

        let makeListMatch listExpression headName tailName consBody nilBody =
            Match(
                listExpression,
                [
                    { Pattern = ConstructorPattern([ "::" ], [ NamePattern headName; NamePattern tailName ])
                      Guard = None
                      Body = consBody }
                    { Pattern = ConstructorPattern([ "Nil" ], [])
                      Guard = None
                      Body = nilBody }
                ]
            )

        let makeReverseExpression listExpression =
            let reverseName = this.FreshSyntheticName "__query_reverse"
            let remainingName = this.FreshSyntheticName "__query_reverse_remaining"
            let accumulatorName = this.FreshSyntheticName "__query_reverse_acc"
            let headName = this.FreshSyntheticName "__query_reverse_head"
            let tailName = this.FreshSyntheticName "__query_reverse_tail"

            bindName
                reverseName
                (Lambda(
                    [ makeParameter remainingName; makeParameter accumulatorName ],
                    makeListMatch
                        (Name [ remainingName ])
                        headName
                        tailName
                        (applyName reverseName [ Name [ tailName ]; cons (Name [ headName ]) (Name [ accumulatorName ]) ])
                        (Name [ accumulatorName ])
                ))
                (applyName reverseName [ listExpression; listNil ])

        let makeAppendExpression prefixExpression suffixExpression =
            let appendName = this.FreshSyntheticName "__query_append"
            let prefixName = this.FreshSyntheticName "__query_append_prefix"
            let suffixName = this.FreshSyntheticName "__query_append_suffix"
            let headName = this.FreshSyntheticName "__query_append_head"
            let tailName = this.FreshSyntheticName "__query_append_tail"

            bindName
                appendName
                (Lambda(
                    [ makeParameter prefixName; makeParameter suffixName ],
                    makeListMatch
                        (Name [ prefixName ])
                        headName
                        tailName
                        (cons (Name [ headName ]) (applyName appendName [ Name [ tailName ]; Name [ suffixName ] ]))
                        (Name [ suffixName ])
                ))
                (applyName appendName [ prefixExpression; suffixExpression ])

        let makeContainsExpression listExpression searchedValue =
            let containsName = this.FreshSyntheticName "__query_contains"
            let remainingName = this.FreshSyntheticName "__query_contains_remaining"
            let searchedName = this.FreshSyntheticName "__query_contains_value"
            let headName = this.FreshSyntheticName "__query_contains_head"
            let tailName = this.FreshSyntheticName "__query_contains_tail"

            bindName
                containsName
                (Lambda(
                    [ makeParameter remainingName; makeParameter searchedName ],
                    makeListMatch
                        (Name [ remainingName ])
                        headName
                        tailName
                        (IfThenElse(
                            Binary(Name [ headName ], "==", Name [ searchedName ]),
                            boolTrue,
                            applyName containsName [ Name [ tailName ]; Name [ searchedName ] ]
                        ))
                        boolFalse
                ))
                (applyName containsName [ listExpression; searchedValue ])

        let splitCollectionClauses (tokens: Token list) =
            let clauses = ResizeArray<Token list>()
            let current = ResizeArray<Token>()
            let tokenArray = tokens |> List.toArray
            let mutable parenDepth = 0
            let mutable bracketDepth = 0
            let mutable effectRowDepth = 0
            let mutable braceDepth = 0
            let mutable setBraceDepth = 0
            let mutable layoutDepth = 0
            let mutable neutralLayoutDepth = 0

            let isLayoutToken (token: Token) =
                match token.Kind with
                | Newline
                | Indent
                | Dedent -> true
                | _ -> false

            let trimOuterLayoutTokens (tokens: ResizeArray<Token>) =
                tokens
                |> Seq.toList
                |> List.skipWhile isLayoutToken
                |> List.rev
                |> List.skipWhile isLayoutToken
                |> List.rev

            let lastSignificantToken () =
                current |> Seq.toList |> List.rev |> List.tryFind (isLayoutToken >> not)

            let shouldContinueAcrossNewline nextToken =
                match lastSignificantToken (), nextToken with
                | Some lastToken, Some followingToken when this.IsExpressionStart(followingToken) ->
                    lastToken.Kind = Equals
                    || lastToken.Kind = Arrow
                    || lastToken.Kind = Colon
                    || lastToken.Kind = Operator
                    || Token.isKeyword Keyword.In lastToken
                    || Token.isKeyword Keyword.If lastToken
                    || Token.isKeyword Keyword.Yield lastToken
                    || Token.isKeyword Keyword.By lastToken
                | _ ->
                    false

            let flushCurrent () =
                let clauseTokens =
                    trimOuterLayoutTokens current

                if not (List.isEmpty clauseTokens) then
                    clauses.Add(clauseTokens)

                current.Clear()

            for index = 0 to tokenArray.Length - 1 do
                let token = tokenArray[index]
                let nextToken =
                    if index + 1 < tokenArray.Length then
                        Some tokenArray[index + 1]
                    else
                        None

                let atTopLevel =
                    parenDepth = 0
                    && bracketDepth = 0
                    && effectRowDepth = 0
                    && braceDepth = 0
                    && setBraceDepth = 0
                    && layoutDepth = 0

                match token.Kind with
                | LeftParen ->
                    parenDepth <- parenDepth + 1
                    current.Add(token)
                | RightParen ->
                    parenDepth <- max 0 (parenDepth - 1)
                    current.Add(token)
                | LeftBracket ->
                    bracketDepth <- bracketDepth + 1
                    current.Add(token)
                | RightBracket ->
                    bracketDepth <- max 0 (bracketDepth - 1)
                    current.Add(token)
                | LeftEffectRow ->
                    effectRowDepth <- effectRowDepth + 1
                    current.Add(token)
                | RightEffectRow ->
                    effectRowDepth <- max 0 (effectRowDepth - 1)
                    current.Add(token)
                | LeftBrace ->
                    braceDepth <- braceDepth + 1
                    current.Add(token)
                | RightBrace ->
                    braceDepth <- max 0 (braceDepth - 1)
                    current.Add(token)
                | LeftSetBrace ->
                    setBraceDepth <- setBraceDepth + 1
                    current.Add(token)
                | RightSetBrace ->
                    setBraceDepth <- max 0 (setBraceDepth - 1)
                    current.Add(token)
                | Comma when atTopLevel ->
                    flushCurrent ()
                | Newline
                    when atTopLevel
                         && ((nextToken |> Option.exists (fun following -> following.Kind = Indent))
                             || shouldContinueAcrossNewline nextToken) ->
                    current.Add(token)
                | Newline when atTopLevel ->
                    flushCurrent ()
                | Indent ->
                    let hasSignificantTokens =
                        current |> Seq.exists (isLayoutToken >> not)

                    if not hasSignificantTokens && layoutDepth = 0 then
                        neutralLayoutDepth <- neutralLayoutDepth + 1
                    else
                        layoutDepth <- layoutDepth + 1

                    current.Add(token)
                | Dedent ->
                    if neutralLayoutDepth > 0 && layoutDepth = 0 then
                        neutralLayoutDepth <- neutralLayoutDepth - 1
                    else
                        layoutDepth <- max 0 (layoutDepth - 1)

                    current.Add(token)
                | _ ->
                    current.Add(token)

            flushCurrent ()
            List.ofSeq clauses

        let splitTopLevelItems (tokens: Token list) =
            let items = ResizeArray<Token list>()
            let current = ResizeArray<Token>()
            let tokenArray = tokens |> List.toArray
            let mutable parenDepth = 0
            let mutable bracketDepth = 0
            let mutable effectRowDepth = 0
            let mutable braceDepth = 0
            let mutable setBraceDepth = 0
            let mutable layoutDepth = 0
            let mutable neutralLayoutDepth = 0

            let isLayoutToken (token: Token) =
                match token.Kind with
                | Newline
                | Indent
                | Dedent -> true
                | _ -> false

            let trimOuterLayoutTokens (tokens: ResizeArray<Token>) =
                tokens
                |> Seq.toList
                |> List.skipWhile isLayoutToken
                |> List.rev
                |> List.skipWhile isLayoutToken
                |> List.rev

            let lastSignificantToken () =
                current |> Seq.toList |> List.rev |> List.tryFind (isLayoutToken >> not)

            let shouldContinueAcrossNewline nextToken =
                match lastSignificantToken (), nextToken with
                | Some lastToken, Some followingToken when this.IsExpressionStart(followingToken) ->
                    lastToken.Kind = Equals
                    || lastToken.Kind = Arrow
                    || lastToken.Kind = Colon
                    || lastToken.Kind = Operator
                | _ ->
                    false

            let flushCurrent () =
                let itemTokens =
                    trimOuterLayoutTokens current

                if not (List.isEmpty itemTokens) then
                    items.Add(itemTokens)

                current.Clear()

            for index = 0 to tokenArray.Length - 1 do
                let token = tokenArray[index]
                let nextToken =
                    if index + 1 < tokenArray.Length then
                        Some tokenArray[index + 1]
                    else
                        None

                let atTopLevel =
                    parenDepth = 0
                    && bracketDepth = 0
                    && effectRowDepth = 0
                    && braceDepth = 0
                    && setBraceDepth = 0
                    && layoutDepth = 0

                match token.Kind with
                | LeftParen ->
                    parenDepth <- parenDepth + 1
                    current.Add(token)
                | RightParen ->
                    parenDepth <- max 0 (parenDepth - 1)
                    current.Add(token)
                | LeftBracket ->
                    bracketDepth <- bracketDepth + 1
                    current.Add(token)
                | RightBracket ->
                    bracketDepth <- max 0 (bracketDepth - 1)
                    current.Add(token)
                | LeftEffectRow ->
                    effectRowDepth <- effectRowDepth + 1
                    current.Add(token)
                | RightEffectRow ->
                    effectRowDepth <- max 0 (effectRowDepth - 1)
                    current.Add(token)
                | LeftBrace ->
                    braceDepth <- braceDepth + 1
                    current.Add(token)
                | RightBrace ->
                    braceDepth <- max 0 (braceDepth - 1)
                    current.Add(token)
                | LeftSetBrace ->
                    setBraceDepth <- setBraceDepth + 1
                    current.Add(token)
                | RightSetBrace ->
                    setBraceDepth <- max 0 (setBraceDepth - 1)
                    current.Add(token)
                | Comma when atTopLevel ->
                    flushCurrent ()
                | Newline
                    when atTopLevel
                         && ((nextToken |> Option.exists (fun following -> following.Kind = Indent))
                             || shouldContinueAcrossNewline nextToken) ->
                    current.Add(token)
                | Newline when atTopLevel ->
                    flushCurrent ()
                | Indent ->
                    let hasSignificantTokens =
                        current |> Seq.exists (isLayoutToken >> not)

                    if not hasSignificantTokens && layoutDepth = 0 then
                        neutralLayoutDepth <- neutralLayoutDepth + 1
                    else
                        layoutDepth <- layoutDepth + 1

                    current.Add(token)
                | Dedent ->
                    if neutralLayoutDepth > 0 && layoutDepth = 0 then
                        neutralLayoutDepth <- neutralLayoutDepth - 1
                    else
                        layoutDepth <- max 0 (layoutDepth - 1)

                    current.Add(token)
                | _ ->
                    current.Add(token)

            flushCurrent ()
            List.ofSeq items

        let tryFindTopLevelToken predicate (tokens: Token list) =
            let tokenArray = tokens |> List.toArray
            let mutable parenDepth = 0
            let mutable bracketDepth = 0
            let mutable effectRowDepth = 0
            let mutable braceDepth = 0
            let mutable setBraceDepth = 0
            let mutable layoutDepth = 0
            let mutable index = 0
            let mutable result = None

            while index < tokenArray.Length && result.IsNone do
                match tokenArray[index].Kind with
                | LeftParen -> parenDepth <- parenDepth + 1
                | RightParen -> parenDepth <- max 0 (parenDepth - 1)
                | LeftBracket -> bracketDepth <- bracketDepth + 1
                | RightBracket -> bracketDepth <- max 0 (bracketDepth - 1)
                | LeftEffectRow -> effectRowDepth <- effectRowDepth + 1
                | RightEffectRow -> effectRowDepth <- max 0 (effectRowDepth - 1)
                | LeftBrace -> braceDepth <- braceDepth + 1
                | RightBrace -> braceDepth <- max 0 (braceDepth - 1)
                | LeftSetBrace -> setBraceDepth <- setBraceDepth + 1
                | RightSetBrace -> setBraceDepth <- max 0 (setBraceDepth - 1)
                | Indent -> layoutDepth <- layoutDepth + 1
                | Dedent -> layoutDepth <- max 0 (layoutDepth - 1)
                | _
                    when parenDepth = 0
                         && bracketDepth = 0
                         && effectRowDepth = 0
                         && braceDepth = 0
                         && setBraceDepth = 0
                         && layoutDepth = 0
                         && predicate tokenArray[index] ->
                    result <- Some index
                | _ ->
                    ()

                index <- index + 1

            result

        let tryFindTopLevelTokenFrom startIndex predicate (tokens: Token list) =
            if startIndex >= List.length tokens then
                None
            else
                tokens[startIndex..]
                |> tryFindTopLevelToken predicate
                |> Option.map ((+) startIndex)

        let tryFindMatchingRightBrace (tokens: Token list) leftBraceIndex =
            let tokenArray = tokens |> List.toArray
            let mutable depth = 0
            let mutable index = leftBraceIndex
            let mutable result = None

            while index < tokenArray.Length && result.IsNone do
                match tokenArray[index].Kind with
                | LeftBrace ->
                    depth <- depth + 1
                | RightBrace ->
                    depth <- depth - 1

                    if depth = 0 then
                        result <- Some index
                | _ ->
                    ()

                index <- index + 1

            result

        let tryFindTopLevelLeftBrace (tokens: Token list) =
            let tokenArray = tokens |> List.toArray
            let mutable parenDepth = 0
            let mutable bracketDepth = 0
            let mutable effectRowDepth = 0
            let mutable braceDepth = 0
            let mutable setBraceDepth = 0
            let mutable layoutDepth = 0
            let mutable index = 0
            let mutable result = None

            while index < tokenArray.Length && result.IsNone do
                let token = tokenArray[index]

                match token.Kind with
                | LeftParen ->
                    parenDepth <- parenDepth + 1
                | RightParen ->
                    parenDepth <- max 0 (parenDepth - 1)
                | LeftBracket ->
                    bracketDepth <- bracketDepth + 1
                | RightBracket ->
                    bracketDepth <- max 0 (bracketDepth - 1)
                | LeftEffectRow ->
                    effectRowDepth <- effectRowDepth + 1
                | RightEffectRow ->
                    effectRowDepth <- max 0 (effectRowDepth - 1)
                | LeftSetBrace ->
                    setBraceDepth <- setBraceDepth + 1
                | RightSetBrace ->
                    setBraceDepth <- max 0 (setBraceDepth - 1)
                | Indent ->
                    layoutDepth <- layoutDepth + 1
                | Dedent ->
                    layoutDepth <- max 0 (layoutDepth - 1)
                | LeftBrace
                    when parenDepth = 0
                         && bracketDepth = 0
                         && effectRowDepth = 0
                         && braceDepth = 0
                         && setBraceDepth = 0
                         && layoutDepth = 0 ->
                    result <- Some index
                    braceDepth <- braceDepth + 1
                | LeftBrace ->
                    braceDepth <- braceDepth + 1
                | RightBrace ->
                    braceDepth <- max 0 (braceDepth - 1)
                | _ ->
                    ()

                index <- index + 1

            result

        let parseGroupAggregationFields (tokens: Token list) =
            let parseField (fieldTokens: Token list) =
                let trimmed =
                    fieldTokens
                    |> List.filter (fun token ->
                        match token.Kind with
                        | Newline
                        | Indent
                        | Dedent
                        | EndOfFile -> false
                        | _ -> true)

                match tryFindTopLevelToken (fun token -> token.Kind = Equals) trimmed with
                | Some equalsIndex when equalsIndex > 0 ->
                    let tokenArray = trimmed |> List.toArray
                    let nameTokens = tokenArray[0 .. equalsIndex - 1] |> Array.toList
                    let valueTokens = tokenArray[equalsIndex + 1 ..] |> Array.toList

                    match nameTokens with
                    | [ nameToken ] when Token.isName nameToken ->
                        Some
                            { Name = SyntaxFacts.trimIdentifierQuotes nameToken.Text
                              IsImplicit = false
                              Value = this.ParseStandaloneExpression(valueTokens) }
                    | _ ->
                        diagnostics.AddError(
                            DiagnosticFact.coreExpressionParsing ExpectedNamedGroupAggregation,
                            source.GetLocation((trimmed |> List.tryHead |> Option.defaultValue { Kind = EndOfFile; Text = ""; Span = eofSpan }).Span)
                        )
                        None
                | _ ->
                    diagnostics.AddError(
                        DiagnosticFact.coreExpressionParsing ExpectedGroupAggregationEquals,
                        source.GetLocation((trimmed |> List.tryHead |> Option.defaultValue { Kind = EndOfFile; Text = ""; Span = eofSpan }).Span)
                    )
                    None

            splitTopLevelItems tokens |> List.choose parseField

        let parseQualifiedNameTokens (tokens: Token list) =
            let trimmed =
                tokens
                |> List.filter (fun token ->
                    match token.Kind with
                    | Newline
                    | Indent
                    | Dedent
                    | EndOfFile -> false
                    | _ -> true)

            let rec loop current remaining =
                match remaining with
                | [] ->
                    Some(List.rev current)
                | [ nameToken ] when Token.isName nameToken ->
                    Some(List.rev (SyntaxFacts.trimIdentifierQuotes nameToken.Text :: current))
                | nameToken :: dotToken :: rest when Token.isName nameToken && dotToken.Kind = Dot ->
                    loop (SyntaxFacts.trimIdentifierQuotes nameToken.Text :: current) rest
                | _ ->
                    None

            loop [] trimmed

        let parseGeneratorClause isRefutable (generatorToken: Token) remainingTokens =
            match tryFindTopLevelToken (fun token -> Token.isKeyword Keyword.In token) remainingTokens with
            | Some inIndex ->
                let tokenArray = remainingTokens |> List.toArray
                let rawPatternTokens = tokenArray[0 .. inIndex - 1] |> Array.toList
                let sourceTokens = tokenArray[inIndex + 1 ..] |> Array.toList
                let isBorrowed, patternTokens =
                    match rawPatternTokens with
                    | borrowToken :: rest when borrowToken.Kind = Operator && borrowToken.Text = "&" ->
                        true, rest
                    | _ ->
                        false, rawPatternTokens

                Some(
                    Choice3Of3(
                        ForClause(
                            isBorrowed,
                            isRefutable,
                            this.ParseBindPatternFromTokens patternTokens,
                            this.ParseStandaloneExpression(sourceTokens)
                        )
                    )
                )
            | None ->
                diagnostics.AddError(
                    DiagnosticFact.coreExpressionParsing ExpectedComprehensionGeneratorIn,
                    source.GetLocation(generatorToken.Span)
                )
                None

        let parseYieldClause (remainingTokens: Token list) =
            match kind with
            | MapCollection ->
                match tryFindTopLevelToken (fun token -> token.Kind = Colon) remainingTokens with
                | Some colonIndex ->
                    let tokenArray = remainingTokens |> List.toArray
                    let keyTokens = tokenArray[0 .. colonIndex - 1] |> Array.toList
                    let valueTokens = tokenArray[colonIndex + 1 ..] |> Array.toList

                    Some(Choice1Of3(YieldKeyValue(this.ParseStandaloneExpression(keyTokens), this.ParseStandaloneExpression(valueTokens))))
                | None ->
                    Some(Choice1Of3(YieldValue(this.ParseStandaloneExpression(remainingTokens))))
            | _ ->
                Some(Choice1Of3(YieldValue(this.ParseStandaloneExpression(remainingTokens))))

        let parseConflictClause (startToken: Token) remainingTokens =
            match remainingTokens with
            | keepToken :: lastToken :: []
                when isContextualName "keep" keepToken && isContextualName "last" lastToken ->
                Some(Choice2Of3 KeepLast)
            | keepToken :: firstToken :: []
                when isContextualName "keep" keepToken && isContextualName "first" firstToken ->
                Some(Choice2Of3 KeepFirst)
            | combineToken :: withToken :: expressionTokens
                when isContextualName "combine" combineToken && isContextualName "with" withToken ->
                Some(Choice2Of3(CombineWith(this.ParseStandaloneExpression(expressionTokens))))
            | combineToken :: usingToken :: nameTokens
                when isContextualName "combine" combineToken && isContextualName "using" usingToken ->
                match parseQualifiedNameTokens nameTokens with
                | Some nameSegments ->
                    Some(Choice2Of3(CombineUsing nameSegments))
                | None ->
                    diagnostics.AddError(
                        DiagnosticFact.coreExpressionParsing ExpectedConflictCombineUsingQualifiedName,
                        source.GetLocation(startToken.Span)
                    )
                    None
            | _ ->
                diagnostics.AddError(
                    DiagnosticFact.coreExpressionParsing ExpectedConflictClauseForm,
                    source.GetLocation(startToken.Span)
                )
                None

        let parseClause (clauseTokens: Token list) =
            match clauseTokens with
            | yieldToken :: rest when Token.isKeyword Keyword.Yield yieldToken ->
                parseYieldClause rest
            | forToken :: rest when Token.isKeyword Keyword.For forToken ->
                parseGeneratorClause false forToken rest
            | forQuestionToken :: rest when Token.isKeyword Keyword.ForQuestion forQuestionToken ->
                parseGeneratorClause true forQuestionToken rest
            | letToken :: rest when Token.isKeyword Keyword.Let letToken ->
                match tryFindTopLevelToken (fun token -> token.Kind = Equals) rest with
                | Some equalsIndex ->
                    let tokenArray = rest |> List.toArray
                    let bindingTokens = tokenArray[0 .. equalsIndex - 1] |> Array.toList
                    let valueTokens = tokenArray[equalsIndex + 1 ..] |> Array.toList
                    Some(Choice3Of3(LetClause(false, this.ParseBindPatternFromTokens bindingTokens, this.ParseStandaloneExpression(valueTokens))))
                | None ->
                    diagnostics.AddError(
                        DiagnosticFact.coreExpressionParsing ExpectedComprehensionLetEquals,
                        source.GetLocation(letToken.Span)
                    )
                    None
            | ifToken :: rest when Token.isKeyword Keyword.If ifToken ->
                Some(Choice3Of3(IfClause(this.ParseStandaloneExpression(rest))))
            | joinToken :: rest when Token.isKeyword Keyword.Join joinToken ->
                match
                    tryFindTopLevelToken (fun token -> Token.isKeyword Keyword.In token) rest,
                    tryFindTopLevelToken (isContextualName "on") rest
                with
                | Some inIndex, Some onIndex when inIndex > 0 && onIndex > inIndex + 1 ->
                    let tokenArray = rest |> List.toArray
                    let patternTokens = tokenArray[0 .. inIndex - 1] |> Array.toList
                    let sourceTokens = tokenArray[inIndex + 1 .. onIndex - 1] |> Array.toList
                    let conditionTokens = tokenArray[onIndex + 1 ..] |> Array.toList

                    Some(
                        Choice3Of3(
                            JoinClause(
                                this.ParseBindPatternFromTokens patternTokens,
                                this.ParseStandaloneExpression(sourceTokens),
                                this.ParseStandaloneExpression(conditionTokens)
                            )
                        )
                    )
                | _ ->
                    diagnostics.AddError(
                        DiagnosticFact.coreExpressionParsing ExpectedComprehensionJoinClause,
                        source.GetLocation(joinToken.Span)
                    )
                    None
            | groupToken :: byToken :: rest when Token.isKeyword Keyword.Group groupToken && Token.isKeyword Keyword.By byToken ->
                match tryFindTopLevelLeftBrace rest with
                | Some leftBraceIndex ->
                    match tryFindMatchingRightBrace rest leftBraceIndex with
                    | Some rightBraceIndex ->
                        let tokenArray = rest |> List.toArray
                        let keyTokens = tokenArray[0 .. leftBraceIndex - 1] |> Array.toList
                        let aggregationTokens = tokenArray[leftBraceIndex + 1 .. rightBraceIndex - 1] |> Array.toList
                        let tailTokens =
                            if rightBraceIndex + 1 < tokenArray.Length then
                                tokenArray[rightBraceIndex + 1 ..] |> Array.toList
                            else
                                []

                        match tailTokens with
                        | intoToken :: [ nameToken ] when isContextualName "into" intoToken && Token.isName nameToken ->
                            Some(
                                Choice3Of3(
                                    GroupByClause(
                                        this.ParseStandaloneExpression(keyTokens),
                                        parseGroupAggregationFields aggregationTokens,
                                        SyntaxFacts.trimIdentifierQuotes nameToken.Text
                                    )
                                )
                            )
                        | _ ->
                            diagnostics.AddError(
                                DiagnosticFact.coreExpressionParsing ExpectedGroupByIntoName,
                                source.GetLocation(groupToken.Span)
                            )
                            None
                    | None ->
                        diagnostics.AddError(
                            DiagnosticFact.coreExpressionParsing ExpectedGroupAggregationBlockClose,
                            source.GetLocation(groupToken.Span)
                        )
                        None
                | None ->
                    diagnostics.AddError(
                        DiagnosticFact.coreExpressionParsing ExpectedGroupAggregationBlock,
                        source.GetLocation(groupToken.Span)
                    )
                    None
            | orderToken :: byToken :: rest when Token.isKeyword Keyword.Order orderToken && Token.isKeyword Keyword.By byToken ->
                let direction, keyTokens =
                    match rest with
                    | ascToken :: tail when Token.isKeyword Keyword.Asc ascToken ->
                        Some Ascending, tail
                    | descToken :: tail when Token.isKeyword Keyword.Desc descToken ->
                        Some Descending, tail
                    | _ ->
                        None, rest

                Some(Choice3Of3(OrderByClause(direction, this.ParseStandaloneExpression(keyTokens))))
            | distinctToken :: byToken :: rest when Token.isKeyword Keyword.Distinct distinctToken && Token.isKeyword Keyword.By byToken ->
                Some(Choice3Of3(DistinctByClause(this.ParseStandaloneExpression(rest))))
            | distinctToken :: [] when Token.isKeyword Keyword.Distinct distinctToken ->
                Some(Choice3Of3 DistinctClause)
            | skipToken :: rest when Token.isKeyword Keyword.Skip skipToken ->
                Some(Choice3Of3(SkipClause(this.ParseStandaloneExpression(rest))))
            | takeToken :: rest when Token.isKeyword Keyword.Take takeToken ->
                Some(Choice3Of3(TakeClause(this.ParseStandaloneExpression(rest))))
            | leftToken :: joinToken :: rest when Token.isKeyword Keyword.Left leftToken && Token.isKeyword Keyword.Join joinToken ->
                match
                    tryFindTopLevelToken (fun token -> Token.isKeyword Keyword.In token) rest,
                    tryFindTopLevelToken (isContextualName "on") rest,
                    tryFindTopLevelToken (isContextualName "into") rest
                with
                | Some inIndex, Some onIndex, Some intoIndex when inIndex > 0 && onIndex > inIndex + 1 && intoIndex > onIndex + 1 ->
                    let tokenArray = rest |> List.toArray
                    let patternTokens = tokenArray[0 .. inIndex - 1] |> Array.toList
                    let sourceTokens = tokenArray[inIndex + 1 .. onIndex - 1] |> Array.toList
                    let conditionTokens = tokenArray[onIndex + 1 .. intoIndex - 1] |> Array.toList
                    let intoTokens = tokenArray[intoIndex + 1 ..] |> Array.toList

                    match intoTokens with
                    | [ intoName ] when Token.isName intoName ->
                        Some(
                            Choice3Of3(
                                LeftJoinClause(
                                    this.ParseBindPatternFromTokens patternTokens,
                                    this.ParseStandaloneExpression(sourceTokens),
                                    this.ParseStandaloneExpression(conditionTokens),
                                    SyntaxFacts.trimIdentifierQuotes intoName.Text
                                )
                            )
                        )
                    | _ ->
                        diagnostics.AddError(
                            DiagnosticFact.coreExpressionParsing ExpectedLeftJoinIntoName,
                            source.GetLocation(leftToken.Span)
                        )
                        None
                | _ ->
                    diagnostics.AddError(
                        DiagnosticFact.coreExpressionParsing ExpectedLeftJoinClause,
                        source.GetLocation(leftToken.Span)
                    )
                    None
            | onToken :: conflictToken :: rest when isContextualName "on" onToken && isContextualName "conflict" conflictToken ->
                parseConflictClause onToken rest
            | token :: _ ->
                diagnostics.AddError(
                    DiagnosticFact.coreExpressionParsing UnsupportedComprehensionClause,
                    source.GetLocation(token.Span)
                )
                None
            | [] ->
                None

        let unwrapEnumeratedSource expression =
            match expression with
            | Apply(Name [ CompilerKnownSymbols.KnownTypeNames.Set ], [ items ]) ->
                items, KnownUnordered
            | _ ->
                expression, UnknownOrderedness

        let buildInitialRows () =
            makeListExpression [ Literal LiteralValue.Unit ]

        let mapYield rowNames rowsExpression yieldedExpression =
            let loopName = this.FreshSyntheticName "__query_yield_loop"
            let remainingRowsName = this.FreshSyntheticName "__query_yield_rows"
            let rowName = this.FreshSyntheticName "__query_yield_row"
            let tailName = this.FreshSyntheticName "__query_yield_tail"

            bindName
                loopName
                (Lambda(
                    [ makeParameter remainingRowsName ],
                    makeListMatch
                        (Name [ remainingRowsName ])
                        rowName
                        tailName
                        (wrapRowBindings
                            rowNames
                            (Name [ rowName ])
                            (cons yieldedExpression (applyName loopName [ Name [ tailName ] ])))
                        listNil
                ))
                (applyName loopName [ rowsExpression ])

        let mapYieldEntries rowNames rowsExpression keyExpression valueExpression =
            mapYield rowNames rowsExpression (applyPreludeResConstructor keyExpression valueExpression)

        let transformForClause
            (rowNames: string list)
            (rowsExpression: SurfaceExpression)
            (binding: SurfaceBindPattern)
            (sourceExpression: SurfaceExpression)
            =
            let nextRowNames = extendRowNames rowNames (patternBoundNames binding.Pattern)
            let enumeratedSourceExpression, sourceOrderedness = unwrapEnumeratedSource sourceExpression
            let loopRowsName = this.FreshSyntheticName "__query_for_rows"
            let remainingRowsName = this.FreshSyntheticName "__query_for_remaining_rows"
            let rowName = this.FreshSyntheticName "__query_for_row"
            let rowTailName = this.FreshSyntheticName "__query_for_row_tail"
            let loopItemsName = this.FreshSyntheticName "__query_for_items"
            let itemName = this.FreshSyntheticName "__query_for_item"
            let itemTailName = this.FreshSyntheticName "__query_for_item_tail"
            let sourceItemsName = this.FreshSyntheticName "__query_for_source_items"

            let transformedExpression =
                bindName
                    loopRowsName
                    (Lambda(
                        [ makeParameter remainingRowsName ],
                        makeListMatch
                            (Name [ remainingRowsName ])
                            rowName
                            rowTailName
                            (wrapRowBindings
                                rowNames
                                (Name [ rowName ])
                                (bindName
                                    loopItemsName
                                    (Lambda(
                                        [ makeParameter sourceItemsName ],
                                        makeListMatch
                                            (Name [ sourceItemsName ])
                                            itemName
                                            itemTailName
                                            (wrapPatternBindings
                                                binding
                                                (Name [ itemName ])
                                                (cons
                                                    (makeRowExpression nextRowNames)
                                                    (applyName loopItemsName [ Name [ itemTailName ] ])))
                                            listNil
                                    ))
                                    (makeAppendExpression
                                        (applyName loopItemsName [ enumeratedSourceExpression ])
                                        (applyName loopRowsName [ Name [ rowTailName ] ]))))
                            listNil
                    ))
                    (applyName loopRowsName [ rowsExpression ])

            transformedExpression, nextRowNames, sourceOrderedness

        let transformGroupByClause
            (rowNames: string list)
            (rowsExpression: SurfaceExpression)
            (keyExpression: SurfaceExpression)
            (aggregations: SurfaceRecordLiteralField list)
            intoName
            =
            let loopName = this.FreshSyntheticName "__query_group_loop"
            let remainingRowsName = this.FreshSyntheticName "__query_group_rows"
            let rowName = this.FreshSyntheticName "__query_group_row"
            let rowTailName = this.FreshSyntheticName "__query_group_row_tail"
            let groupedValue =
                RecordLiteral(
                    { Name = "key"
                      IsImplicit = false
                      Value = keyExpression }
                    :: aggregations
                )

            let transformedExpression =
                bindName
                    loopName
                    (Lambda(
                        [ makeParameter remainingRowsName ],
                        makeListMatch
                            (Name [ remainingRowsName ])
                            rowName
                            rowTailName
                            (wrapRowBindings
                                rowNames
                                (Name [ rowName ])
                                (bindName
                                    intoName
                                    groupedValue
                                    (cons
                                        (makeRowExpression [ intoName ])
                                        (applyName loopName [ Name [ rowTailName ] ]))))
                            listNil
                    ))
                    (applyName loopName [ rowsExpression ])

            transformedExpression, [ intoName ]

        let transformLetClause
            (rowNames: string list)
            (rowsExpression: SurfaceExpression)
            (binding: SurfaceBindPattern)
            (valueExpression: SurfaceExpression)
            =
            let nextRowNames = extendRowNames rowNames (patternBoundNames binding.Pattern)
            let loopName = this.FreshSyntheticName "__query_let_loop"
            let remainingRowsName = this.FreshSyntheticName "__query_let_rows"
            let rowName = this.FreshSyntheticName "__query_let_row"
            let rowTailName = this.FreshSyntheticName "__query_let_row_tail"

            let transformedExpression =
                bindName
                    loopName
                    (Lambda(
                        [ makeParameter remainingRowsName ],
                        makeListMatch
                            (Name [ remainingRowsName ])
                            rowName
                            rowTailName
                            (wrapRowBindings
                                rowNames
                                (Name [ rowName ])
                                (wrapPatternBindings
                                    binding
                                    valueExpression
                                    (cons
                                        (makeRowExpression nextRowNames)
                                        (applyName loopName [ Name [ rowTailName ] ]))))
                            listNil
                    ))
                    (applyName loopName [ rowsExpression ])

            transformedExpression, nextRowNames

        let transformIfClause rowNames rowsExpression conditionExpression =
            let loopName = this.FreshSyntheticName "__query_if_loop"
            let remainingRowsName = this.FreshSyntheticName "__query_if_rows"
            let rowName = this.FreshSyntheticName "__query_if_row"
            let rowTailName = this.FreshSyntheticName "__query_if_row_tail"
            let filteredTailName = this.FreshSyntheticName "__query_if_tail_filtered"

            bindName
                loopName
                (Lambda(
                    [ makeParameter remainingRowsName ],
                    makeListMatch
                        (Name [ remainingRowsName ])
                        rowName
                        rowTailName
                        (wrapRowBindings
                            rowNames
                            (Name [ rowName ])
                            (bindName
                                filteredTailName
                                (applyName loopName [ Name [ rowTailName ] ])
                                (IfThenElse(
                                    conditionExpression,
                                    cons (Name [ rowName ]) (Name [ filteredTailName ]),
                                    Name [ filteredTailName ]
                                ))))
                        listNil
                ))
                (applyName loopName [ rowsExpression ])

        let transformDistinctClause rowNames rowsExpression keySelector =
            let loopName = this.FreshSyntheticName "__query_distinct_loop"
            let remainingRowsName = this.FreshSyntheticName "__query_distinct_rows"
            let seenKeysName = this.FreshSyntheticName "__query_distinct_seen"
            let rowName = this.FreshSyntheticName "__query_distinct_row"
            let rowTailName = this.FreshSyntheticName "__query_distinct_row_tail"
            let keyName = this.FreshSyntheticName "__query_distinct_key"

            bindName
                loopName
                (Lambda(
                    [ makeParameter remainingRowsName; makeParameter seenKeysName ],
                    makeListMatch
                        (Name [ remainingRowsName ])
                        rowName
                        rowTailName
                        (wrapRowBindings
                            rowNames
                            (Name [ rowName ])
                            (bindName
                                keyName
                                keySelector
                                (IfThenElse(
                                    makeContainsExpression (Name [ seenKeysName ]) (Name [ keyName ]),
                                    applyName loopName [ Name [ rowTailName ]; Name [ seenKeysName ] ],
                                    cons
                                        (Name [ rowName ])
                                        (applyName
                                            loopName
                                            [ Name [ rowTailName ]; cons (Name [ keyName ]) (Name [ seenKeysName ]) ])
                                ))))
                        listNil
                ))
                (applyName loopName [ rowsExpression; listNil ])

        let transformSkipClause rowsExpression orderedness countExpression =
            let skipToken =
                innerTokens
                |> List.tryFind (Token.isKeyword Keyword.Skip)
                |> Option.defaultValue { Kind = Keyword Keyword.Skip; Text = "skip"; Span = eofSpan }

            if orderedness = KnownUnordered then
                diagnostics.AddError(
                    DiagnosticFact.coreExpressionParsing (QueryPagingRequiresOrderedPipeline "skip"),
                    source.GetLocation(skipToken.Span)
                )

            let loopName = this.FreshSyntheticName "__query_skip_loop"
            let remainingRowsName = this.FreshSyntheticName "__query_skip_rows"
            let remainingCountName = this.FreshSyntheticName "__query_skip_count"
            let rowName = this.FreshSyntheticName "__query_skip_row"
            let rowTailName = this.FreshSyntheticName "__query_skip_row_tail"

            bindName
                loopName
                (Lambda(
                    [ makeParameter remainingRowsName; makeParameter remainingCountName ],
                    makeListMatch
                        (Name [ remainingRowsName ])
                        rowName
                        rowTailName
                        (IfThenElse(
                            Binary(Name [ remainingCountName ], ">", NumericLiteral(SurfaceIntegerLiteral(0I, "0", None))),
                            applyName
                                loopName
                                [ Name [ rowTailName ]
                                  Binary(
                                      Name [ remainingCountName ],
                                      "-",
                                      NumericLiteral(SurfaceIntegerLiteral(1I, "1", None))
                                  ) ],
                            cons (Name [ rowName ]) (applyName loopName [ Name [ rowTailName ]; Name [ remainingCountName ] ])
                        ))
                        listNil
                ))
                (applyName loopName [ rowsExpression; countExpression ])

        let transformTakeClause rowsExpression orderedness countExpression =
            let takeToken =
                innerTokens
                |> List.tryFind (Token.isKeyword Keyword.Take)
                |> Option.defaultValue { Kind = Keyword Keyword.Take; Text = "take"; Span = eofSpan }

            if orderedness = KnownUnordered then
                diagnostics.AddError(
                    DiagnosticFact.coreExpressionParsing (QueryPagingRequiresOrderedPipeline "take"),
                    source.GetLocation(takeToken.Span)
                )

            let loopName = this.FreshSyntheticName "__query_take_loop"
            let remainingRowsName = this.FreshSyntheticName "__query_take_rows"
            let remainingCountName = this.FreshSyntheticName "__query_take_count"
            let rowName = this.FreshSyntheticName "__query_take_row"
            let rowTailName = this.FreshSyntheticName "__query_take_row_tail"

            bindName
                loopName
                (Lambda(
                    [ makeParameter remainingRowsName; makeParameter remainingCountName ],
                    makeListMatch
                        (Name [ remainingRowsName ])
                        rowName
                        rowTailName
                        (IfThenElse(
                            Binary(Name [ remainingCountName ], ">", NumericLiteral(SurfaceIntegerLiteral(0I, "0", None))),
                            cons
                                (Name [ rowName ])
                                (applyName
                                    loopName
                                    [ Name [ rowTailName ]
                                      Binary(
                                          Name [ remainingCountName ],
                                          "-",
                                          NumericLiteral(SurfaceIntegerLiteral(1I, "1", None))
                                      ) ]),
                            listNil
                        ))
                        listNil
                ))
                (applyName loopName [ rowsExpression; countExpression ])

        let transformLeftJoinClause
            (rowNames: string list)
            (rowsExpression: SurfaceExpression)
            (binding: SurfaceBindPattern)
            (sourceExpression: SurfaceExpression)
            (conditionExpression: SurfaceExpression)
            intoName
            =
            let nextRowNames = extendRowNames rowNames [ intoName ]
            let enumeratedSourceExpression, _ = unwrapEnumeratedSource sourceExpression
            let loopRowsName = this.FreshSyntheticName "__query_left_join_rows"
            let remainingRowsName = this.FreshSyntheticName "__query_left_join_remaining_rows"
            let rowName = this.FreshSyntheticName "__query_left_join_row"
            let rowTailName = this.FreshSyntheticName "__query_left_join_row_tail"
            let innerLoopName = this.FreshSyntheticName "__query_left_join_inner"
            let sourceItemsName = this.FreshSyntheticName "__query_left_join_items"
            let itemName = this.FreshSyntheticName "__query_left_join_item"
            let itemTailName = this.FreshSyntheticName "__query_left_join_item_tail"

            let transformedExpression =
                bindName
                    loopRowsName
                    (Lambda(
                        [ makeParameter remainingRowsName ],
                        makeListMatch
                            (Name [ remainingRowsName ])
                            rowName
                            rowTailName
                            (wrapRowBindings
                                rowNames
                                (Name [ rowName ])
                                (bindName
                                    innerLoopName
                                    (Lambda(
                                        [ makeParameter sourceItemsName ],
                                        makeListMatch
                                            (Name [ sourceItemsName ])
                                            itemName
                                            itemTailName
                                            (Match(
                                                Name [ itemName ],
                                                [
                                                    { Pattern = binding.Pattern
                                                      Guard = None
                                                      Body =
                                                        IfThenElse(
                                                            conditionExpression,
                                                            cons (Name [ itemName ]) (applyName innerLoopName [ Name [ itemTailName ] ]),
                                                            applyName innerLoopName [ Name [ itemTailName ] ]
                                                        ) }
                                                    { Pattern = WildcardPattern
                                                      Guard = None
                                                      Body = applyName innerLoopName [ Name [ itemTailName ] ] }
                                                ]
                                            ))
                                            listNil
                                    ))
                                    (bindName
                                        intoName
                                        (applyName innerLoopName [ enumeratedSourceExpression ])
                                        (cons
                                            (makeRowExpression nextRowNames)
                                            (applyName loopRowsName [ Name [ rowTailName ] ])))))
                            listNil
                    ))
                    (applyName loopRowsName [ rowsExpression ])

            transformedExpression, nextRowNames

        let transformOrderByClause rowNames rowsExpression direction keyExpression =
            let insertName = this.FreshSyntheticName "__query_order_insert"
            let sortName = this.FreshSyntheticName "__query_order_sort"
            let projectName = this.FreshSyntheticName "__query_order_project"
            let remainingRowsName = this.FreshSyntheticName "__query_order_rows"
            let sortedPairsName = this.FreshSyntheticName "__query_order_pairs"
            let rowName = this.FreshSyntheticName "__query_order_row"
            let rowTailName = this.FreshSyntheticName "__query_order_row_tail"
            let pairName = this.FreshSyntheticName "__query_order_pair"
            let headPairName = this.FreshSyntheticName "__query_order_head_pair"
            let pairTailName = this.FreshSyntheticName "__query_order_pair_tail"
            let currentKeyName = this.FreshSyntheticName "__query_order_key"
            let currentRowValueName = this.FreshSyntheticName "__query_order_row_value"
            let existingKeyName = this.FreshSyntheticName "__query_order_existing_key"
            let existingRowValueName = this.FreshSyntheticName "__query_order_existing_row"
            let orderedBeforeExisting =
                match direction with
                | Some Descending ->
                    Binary(
                        Binary(Name [ currentKeyName ], ">", Name [ existingKeyName ]),
                        "||",
                        Binary(Name [ currentKeyName ], "==", Name [ existingKeyName ])
                    )
                | _ ->
                    Binary(
                        Binary(Name [ currentKeyName ], "<", Name [ existingKeyName ]),
                        "||",
                        Binary(Name [ currentKeyName ], "==", Name [ existingKeyName ])
                    )

            let pairBinding name keyName rowValueName =
                SurfaceBinderParsing.makeBindPattern
                    (ConstructorPattern(preludeResConstructorName, [ NamePattern keyName; NamePattern rowValueName ]))
                    None
                    []

            bindName
                insertName
                (Lambda(
                    [ makeParameter pairName; makeParameter sortedPairsName ],
                    wrapPatternBindings
                        (pairBinding pairName currentKeyName currentRowValueName)
                        (Name [ pairName ])
                        (makeListMatch
                            (Name [ sortedPairsName ])
                            headPairName
                            pairTailName
                            (wrapPatternBindings
                                (pairBinding headPairName existingKeyName existingRowValueName)
                                (Name [ headPairName ])
                                (IfThenElse(
                                    orderedBeforeExisting,
                                    cons (Name [ pairName ]) (Name [ sortedPairsName ]),
                                    cons
                                        (Name [ headPairName ])
                                        (applyName insertName [ Name [ pairName ]; Name [ pairTailName ] ])
                                )))
                            (cons (Name [ pairName ]) listNil))
                ))
                (bindName
                    sortName
                    (Lambda(
                        [ makeParameter remainingRowsName ],
                        makeListMatch
                            (Name [ remainingRowsName ])
                            rowName
                            rowTailName
                            (wrapRowBindings
                                rowNames
                                (Name [ rowName ])
                                (bindName
                                    pairName
                                    (applyPreludeResConstructor keyExpression (Name [ rowName ]))
                                    (applyName
                                        insertName
                                        [ Name [ pairName ]
                                          applyName sortName [ Name [ rowTailName ] ] ])))
                            listNil
                    ))
                    (bindName
                        projectName
                        (Lambda(
                            [ makeParameter sortedPairsName ],
                            makeListMatch
                                (Name [ sortedPairsName ])
                                headPairName
                                pairTailName
                                (wrapPatternBindings
                                    (pairBinding headPairName existingKeyName existingRowValueName)
                                    (Name [ headPairName ])
                                    (cons
                                        (Name [ existingRowValueName ])
                                        (applyName projectName [ Name [ pairTailName ] ])))
                                listNil
                        ))
                        (applyName projectName [ applyName sortName [ rowsExpression ] ])))

        let startsComprehension =
            innerTokens
            |> List.tryFind (fun token ->
                match token.Kind with
                | Newline
                | Indent
                | Dedent -> false
                | _ -> true)
            |> Option.exists (fun token ->
                Token.isKeyword Keyword.Yield token
                || Token.isKeyword Keyword.For token
                || Token.isKeyword Keyword.ForQuestion token)

        if not startsComprehension then
            let items =
                splitTopLevelItems innerTokens
                |> List.map this.ParseStandaloneExpression

            match kind with
            | ListCollection ->
                makeListExpression items
            | SetCollection ->
                makeSetExpression items
            | MapCollection ->
                makeMapExpression []
        else
            let parsedClauses =
                splitCollectionClauses innerTokens
                |> List.choose parseClause

            let reversedClauses = List.rev parsedClauses
            let trailingConflict, reversedEntries =
                match reversedClauses with
                | Choice2Of3 conflict :: rest ->
                    Some conflict, rest
                | _ ->
                    None, reversedClauses

            match reversedEntries with
            | Choice1Of3 yielded :: reversedPrefix ->
                let rowClauses =
                    reversedPrefix
                    |> List.rev
                    |> List.choose (function
                        | Choice3Of3 clause -> Some clause
                        | _ -> None)

                let conflictPolicy =
                    trailingConflict

                let rowsExpression, rowNames, orderedness =
                    List.fold
                        (fun (currentRows, currentNames, currentOrderedness) clause ->
                        match clause with
                        | ForClause(_, _, binding, sourceExpression) ->
                            let transformedRows, nextNames, sourceOrderedness =
                                transformForClause currentNames currentRows binding sourceExpression

                            let nextOrderedness =
                                match currentOrderedness, sourceOrderedness with
                                | KnownUnordered, _
                                | _, KnownUnordered -> KnownUnordered
                                | KnownOrdered, KnownOrdered -> KnownOrdered
                                | _ -> UnknownOrderedness

                            transformedRows, nextNames, nextOrderedness
                        | JoinClause(binding, sourceExpression, conditionExpression) ->
                            let transformedRows, nextNames, sourceOrderedness =
                                transformForClause currentNames currentRows binding sourceExpression

                            let filteredRows =
                                transformIfClause nextNames transformedRows conditionExpression

                            let nextOrderedness =
                                match currentOrderedness, sourceOrderedness with
                                | KnownUnordered, _
                                | _, KnownUnordered -> KnownUnordered
                                | KnownOrdered, KnownOrdered -> KnownOrdered
                                | _ -> UnknownOrderedness

                            filteredRows, nextNames, nextOrderedness
                        | LetClause(_, binding, valueExpression) ->
                            let transformedRows, nextNames =
                                transformLetClause currentNames currentRows binding valueExpression

                            transformedRows, nextNames, currentOrderedness
                        | IfClause conditionExpression ->
                            transformIfClause currentNames currentRows conditionExpression, currentNames, currentOrderedness
                        | GroupByClause(keyExpression, aggregations, intoName) ->
                            let transformedRows, nextNames =
                                transformGroupByClause currentNames currentRows keyExpression aggregations intoName

                            transformedRows, nextNames, KnownUnordered
                        | DistinctClause ->
                            transformDistinctClause currentNames currentRows (makeRowExpression currentNames), currentNames, currentOrderedness
                        | DistinctByClause keyExpression ->
                            transformDistinctClause currentNames currentRows keyExpression, currentNames, currentOrderedness
                        | OrderByClause(direction, keyExpression) ->
                            transformOrderByClause currentNames currentRows direction keyExpression, currentNames, KnownOrdered
                        | SkipClause countExpression ->
                            transformSkipClause currentRows currentOrderedness countExpression, currentNames, currentOrderedness
                        | TakeClause countExpression ->
                            transformTakeClause currentRows currentOrderedness countExpression, currentNames, currentOrderedness
                        | LeftJoinClause(binding, sourceExpression, conditionExpression, intoName) ->
                            let transformedRows, nextNames =
                                transformLeftJoinClause currentNames currentRows binding sourceExpression conditionExpression intoName

                            transformedRows, nextNames, currentOrderedness)
                        (buildInitialRows (), [], KnownOrdered)
                        rowClauses

                let yieldedRows =
                    match yielded with
                    | YieldValue valueExpression ->
                        mapYield rowNames rowsExpression valueExpression
                    | YieldKeyValue(keyExpression, valueExpression) ->
                        mapYieldEntries rowNames rowsExpression keyExpression valueExpression

                let lowered =
                    match kind with
                    | ListCollection -> yieldedRows
                    | SetCollection -> Apply(Name [ CompilerKnownSymbols.KnownTypeNames.Set ], [ yieldedRows ])
                    | MapCollection -> Apply(Name [ CompilerKnownSymbols.KnownTypeNames.Map ], [ yieldedRows ])

                Comprehension
                    { CollectionKind = kind
                      Clauses = rowClauses
                      Yield = yielded
                      ConflictPolicy = conflictPolicy
                      Lowered = lowered }
            | _ ->
                diagnostics.AddError(
                    DiagnosticFact.coreExpressionParsing ComprehensionMustEndWithYieldClause,
                    source.GetLocation(eofSpan)
                )

                match kind with
                | ListCollection -> listNil
                | SetCollection -> Apply(Name [ CompilerKnownSymbols.KnownTypeNames.Set ], [ listNil ])
                | MapCollection -> Apply(Name [ CompilerKnownSymbols.KnownTypeNames.Map ], [ listNil ])

    member private this.DecodeStringTextSegment(token: Token) =
        match SyntaxFacts.tryUnescapeStringContentDetailed token.Text with
        | Result.Ok value ->
            value
        | Result.Error error ->
            diagnostics.AddError(
                DiagnosticFact.coreExpressionParsing (InvalidStringTextSegment error),
                source.GetLocation(token.Span)
            )
            token.Text

    member private this.TryParseStandaloneExpression(tokens: Token list) =
        let nestedDiagnostics = DiagnosticBag()
        let nestedParser = ExpressionParser(tokens, source, nestedDiagnostics, fixities, false, false)

        match nestedParser.Parse(), nestedDiagnostics.Items with
        | Some expression, [] ->
            Some expression
        | _ ->
            None

    member private this.TryParseStandaloneExpressionWithMinimumPrecedence(tokens: Token list, minimumPrecedence: int) =
        let nestedDiagnostics = DiagnosticBag()
        let nestedParser = ExpressionParser(tokens, source, nestedDiagnostics, fixities, false, false)
        let parsed = nestedParser.ParseExpression(minimumPrecedence)
        nestedParser.SkipLayout()

        match nestedParser.Current.Kind, nestedDiagnostics.Items with
        | EndOfFile, [] ->
            Some parsed
        | _ ->
            None

    member private this.ParseStandaloneExpression(tokens: Token list) =
        this.ParseNestedExpression(tokens, false, false)

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

        let splitBindingTypeAnnotation tokens =
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
                tokenArray[0 .. splitIndex - 1] |> Array.toList,
                Some(tokenArray[splitIndex + 1 ..] |> Array.toList)
            else
                tokens, None

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
                | leftToken :: operatorToken :: rest
                    when Token.isName leftToken && operatorToken.Kind = Operator ->
                    operatorToken.Text, operatorToken.Span, leftToken :: rest
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

            let parameterTokens, returnTypeTokens =
                splitReturnTypeTokens parameterHeaderTokens

            let parsedParameters =
                HeaderParsing.parseHeaderParameters source diagnostics LocalFunctionHeader false parameterTokens

            let value =
                match parsedParameters with
                | [] ->
                    this.ParseStandaloneExpression(valueTokens)
                | parsedParameters ->
                    Lambda(parsedParameters, this.ParseStandaloneExpression(valueTokens))

            let binding =
                { Pattern = NamePattern bindingName
                  Quantity = None
                  IsImplicit = false
                  TypeTokens = if List.isEmpty parsedParameters then returnTypeTokens else None
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
            | _ when SurfaceEffectParsing.parseScopedEffectDeclaration lineTokens |> Option.isSome ->
                SurfaceEffectParsing.parseScopedEffectDeclaration lineTokens |> Option.map LocalPureScopedEffect
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
                            let bindingTokens, bindingTypeTokens =
                                splitBindingTypeAnnotation headerTokens

                            let binding =
                                this.ParseBindPatternFromTokens bindingTokens
                                |> fun current ->
                                    { current with
                                        TypeTokens = bindingTypeTokens |> Option.orElse current.TypeTokens }

                            binding,
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
                | LocalPureScopedEffect declaration ->
                    LocalScopedEffect(declaration, current)) body

        match lines with
        | [] ->
            diagnostics.AddError(DiagnosticFact.simple SimpleDiagnosticKind.ExpectedSyntaxToken emptyMessage, source.GetLocation(errorSpan))
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
                    diagnostics.AddError(DiagnosticFact.simple SimpleDiagnosticKind.ExpectedSyntaxToken malformedMessage, source.GetLocation(errorSpan))
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
            diagnostics.AddError(DiagnosticFact.simple SimpleDiagnosticKind.ExpectedSyntaxToken "Expected ')' to close the expression.", source.GetLocation(start.Span))

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
            diagnostics.AddError(DiagnosticFact.simple SimpleDiagnosticKind.ExpectedSyntaxToken errorMessage, source.GetLocation(start.Span))

        List.ofSeq innerTokens

    member private this.CollectCodeQuoteTokens(errorMessage: string) =
        let start = this.Current
        let openingDot = this.Advance()

        if this.Current.Kind = Operator && this.Current.Text = "<" then
            this.Advance() |> ignore
        else
            diagnostics.AddError(DiagnosticFact.simple SimpleDiagnosticKind.ExpectedSyntaxToken errorMessage, source.GetLocation(openingDot.Span))

        let innerTokens = ResizeArray<Token>()
        let mutable quoteDepth = 1

        while quoteDepth > 0 && this.Current.Kind <> EndOfFile do
            if this.Current.Kind = Dot && this.Peek(1).Kind = Operator && this.Peek(1).Text = "<" then
                quoteDepth <- quoteDepth + 1
                innerTokens.Add(this.Advance())
                innerTokens.Add(this.Advance())
            elif this.Current.Kind = Operator && this.Current.Text = ">." then
                quoteDepth <- quoteDepth - 1

                if quoteDepth > 0 then
                    innerTokens.Add(this.Advance())
                else
                    this.Advance() |> ignore
            else
                innerTokens.Add(this.Advance())

        if quoteDepth > 0 then
            diagnostics.AddError(DiagnosticFact.simple SimpleDiagnosticKind.ExpectedSyntaxToken errorMessage, source.GetLocation(start.Span))

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
            diagnostics.AddError(DiagnosticFact.simple SimpleDiagnosticKind.ExpectedSyntaxToken "Expected a name.", source.GetLocation(this.Current.Span))

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
            diagnostics.AddError(DiagnosticFact.coreExpressionParsing UnterminatedParameterBinder, source.GetLocation(start.Span))
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
                diagnostics.AddError(DiagnosticFact.coreExpressionParsing ExpectedLambdaParameterOrArrow, source.GetLocation(this.Current.Span))
                keepReading <- false

        if parameters.Count = 0 then
            diagnostics.AddError(DiagnosticFact.coreExpressionParsing LambdaMustDeclareAtLeastOneParameter, source.GetLocation(this.Current.Span))

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
            elif this.Current.Kind = EndOfFile && nestedIndents = 0 then
                ()
            else
                diagnostics.AddError(DiagnosticFact.coreExpressionParsing ExpectedDoBlockDedent, source.GetLocation(this.Current.Span))
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
            diagnostics.AddError(DiagnosticFact.coreExpressionParsing ExpectedCaseClauseArrow, source.GetLocation(this.Current.Span))
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
                diagnostics.AddError(DiagnosticFact.coreExpressionParsing ExpectedIndentedExpressionDedent, source.GetLocation(this.Current.Span))

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
                        diagnostics.AddError(DiagnosticFact.coreExpressionParsing ExplicitBracesAfterLayoutIntroducedBlockForbidden,
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
                        diagnostics.AddError(DiagnosticFact.coreExpressionParsing UnexpectedIndentationInIndentedCaseBody,
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
                diagnostics.AddError(DiagnosticFact.coreExpressionParsing ExpectedCaseClauseArrow, source.GetLocation(this.Current.Span))

        if cases.Count = 0 then
            diagnostics.AddError(DiagnosticFact.coreExpressionParsing MatchExpressionMustDeclareAtLeastOneCase, source.GetLocation(this.Current.Span))

        if hasIndentedCases then
            if this.Current.Kind = Dedent then
                this.Advance() |> ignore
            else
                diagnostics.AddError(DiagnosticFact.coreExpressionParsing ExpectedMatchCasesDedent, source.GetLocation(this.Current.Span))

        Match(scrutinee, List.ofSeq cases)

    member private this.ParseDoLine(lineTokens: Token list) =
        let containsInvalidIndentedExpressionContinuation (tokens: Token list) =
            let tokenArray = tokens |> List.toArray
            let mutable parenDepth = 0
            let mutable bracketDepth = 0
            let mutable braceDepth = 0
            let mutable setBraceDepth = 0
            let currentLine = ResizeArray<Token>()
            let mutable invalid = false
            let mutable index = 0

            let lineAllowsIndentedContinuation () =
                let significantCurrentLine =
                    currentLine
                    |> Seq.filter (fun token ->
                        match token.Kind with
                        | Newline
                        | Indent
                        | Dedent -> false
                        | _ -> true)
                    |> Seq.toList

                let firstToken = significantCurrentLine |> List.tryHead
                let lastToken = significantCurrentLine |> List.tryLast

                match firstToken, lastToken with
                | Some first, _
                    when Token.isKeyword Keyword.Match first
                         || Token.isKeyword Keyword.Do first ->
                    true
                | _, Some last
                    when last.Kind = Arrow
                         || last.Kind = Equals
                         || last.Kind = Operator
                         || Token.isKeyword Keyword.Then last
                         || Token.isKeyword Keyword.Else last
                         || Token.isKeyword Keyword.Do last ->
                    true
                | _ ->
                    false

            while index < tokenArray.Length && not invalid do
                let token = tokenArray[index]

                match token.Kind with
                | LeftParen -> parenDepth <- parenDepth + 1
                | RightParen -> parenDepth <- max 0 (parenDepth - 1)
                | LeftBracket -> bracketDepth <- bracketDepth + 1
                | RightBracket -> bracketDepth <- max 0 (bracketDepth - 1)
                | LeftBrace -> braceDepth <- braceDepth + 1
                | RightBrace -> braceDepth <- max 0 (braceDepth - 1)
                | LeftSetBrace -> setBraceDepth <- setBraceDepth + 1
                | RightSetBrace -> setBraceDepth <- max 0 (setBraceDepth - 1)
                | Newline
                    when parenDepth = 0
                         && bracketDepth = 0
                         && braceDepth = 0
                         && setBraceDepth = 0
                         && index + 1 < tokenArray.Length
                         && tokenArray[index + 1].Kind = Indent ->
                    if lineAllowsIndentedContinuation () then
                        currentLine.Add(token)
                    else
                        invalid <- true
                | Newline
                    when parenDepth = 0
                         && bracketDepth = 0
                         && braceDepth = 0
                         && setBraceDepth = 0 ->
                    currentLine.Clear()
                | _ ->
                    ()

                if not invalid then
                    currentLine.Add(token)

                index <- index + 1

            invalid

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
                elif (current ()).Kind = EndOfFile && nestedIndents = 0 then
                    ()
                else
                    diagnostics.AddError(DiagnosticFact.coreExpressionParsing ExpectedWhileBodyDedent, source.GetLocation((current ()).Span))
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

        let splitBindingTypeAnnotation (tokens: Token list) =
            match tryFindTopLevelSplit tokens (fun token -> token.Kind = Colon) with
            | Some(tokenArray, splitIndex) ->
                tokenArray[0 .. splitIndex - 1] |> Array.toList,
                Some(tokenArray[splitIndex + 1 ..] |> Array.toList)
            | None ->
                tokens, None

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
                            diagnostics.AddError(DiagnosticFact.coreExpressionParsing ExpectedLetQuestionFailureArrow, source.GetLocation(letQuestionToken.Span))
                            valueTokens, None
                    | None ->
                        afterEquals, None

                DoLetQuestion(this.ParseBindPatternFromTokens patternTokens, this.ParseStandaloneExpression(valueTokens), failure)
            | None ->
                diagnostics.AddError(DiagnosticFact.coreExpressionParsing ExpectedLetQuestionEquals, source.GetLocation(letQuestionToken.Span))
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
                diagnostics.AddError(DiagnosticFact.coreExpressionParsing ExpectedDoIfThen, source.GetLocation(ifToken.Span))
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
                diagnostics.AddError(DiagnosticFact.coreExpressionParsing ExpectedExpressionAfterDefer, source.GetLocation(deferToken.Span))
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

                        diagnostics.AddError(DiagnosticFact.simple SimpleDiagnosticKind.QttUsingExplicitQuantity "'using' binds its pattern at borrowed quantity '&'; explicit quantity markers are not permitted.",
                            source.GetLocation(location)
                        )

                        restPatternTokens
                    | None ->
                        patternTokens

                if List.isEmpty patternTokens then
                    diagnostics.AddError(DiagnosticFact.coreExpressionParsing ExpectedUsingBindingPattern,
                        source.GetLocation(usingToken.Span)
                    )

                let expressionTokens = tokenArray[splitIndex + 1 ..] |> Array.toList
                let pattern = this.ParsePatternFromTokens patternTokens
                let binding = SurfaceBinderParsing.makeBindPattern pattern (Some(QuantityBorrow None)) patternTokens
                DoUsing(binding, this.ParseStandaloneExpression(expressionTokens))
            | None ->
                diagnostics.AddError(DiagnosticFact.coreExpressionParsing ExpectedUsingBindingArrow, source.GetLocation(usingToken.Span))
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
                let rawBindingTokens =
                    tokenArray[0 .. splitIndex - 1] |> Array.toList

                let bindingTokens, bindingTypeTokens =
                    splitBindingTypeAnnotation rawBindingTokens

                let expressionTokens = tokenArray[splitIndex + 1 ..] |> Array.toList
                let binding =
                    this.ParseBindPatternFromTokens bindingTokens
                    |> fun current ->
                        { current with
                            TypeTokens = bindingTypeTokens |> Option.orElse current.TypeTokens }

                if containsInvalidIndentedExpressionContinuation expressionTokens then
                    diagnostics.AddError(DiagnosticFact.coreExpressionParsing UnexpectedIndentedExpressionContinuationInDoBinding,
                        source.GetLocation(letToken.Span)
                    )

                if tokenArray[splitIndex].Kind = Operator && tokenArray[splitIndex].Text = "<-" then
                    DoBind(binding, this.ParseStandaloneExpression(expressionTokens))
                else
                    DoLet(binding, this.ParseStandaloneExpression(expressionTokens))
            | None ->
                diagnostics.AddError(DiagnosticFact.coreExpressionParsing ExpectedDoBindingAssignmentOrBind, source.GetLocation(letToken.Span))
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
                diagnostics.AddError(DiagnosticFact.coreExpressionParsing ExpectedWhileDo, source.GetLocation(whileToken.Span))
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
        | _ ->
            match tryFindTopLevelSplit lineTokens (fun token -> token.Kind = Operator && token.Text = "<-") with
            | Some(tokenArray, splitIndex) when splitIndex > 0 ->
                let bindingTokens = tokenArray[0 .. splitIndex - 1] |> Array.toList
                let expressionTokens = tokenArray[splitIndex + 1 ..] |> Array.toList
                let binding = this.ParseBindPatternFromTokens bindingTokens
                let bindingLocation =
                    match bindingTokens with
                    | head :: _ -> head.Span
                    | [] ->
                        match lineTokens with
                        | head :: _ -> head.Span
                        | [] -> this.Current.Span

                if containsInvalidIndentedExpressionContinuation expressionTokens then
                    diagnostics.AddError(DiagnosticFact.coreExpressionParsing UnexpectedIndentedExpressionContinuationInDoBinding,
                        source.GetLocation(bindingLocation)
                    )

                DoBind(binding, this.ParseStandaloneExpression(expressionTokens))
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
            diagnostics.AddError(DiagnosticFact.coreExpressionParsing DoBlockMustContainAtLeastOneStatement, source.GetLocation(this.Current.Span))
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
            diagnostics.AddError(DiagnosticFact.coreExpressionParsing ExpectedLambdaBodyArrow, source.GetLocation(this.Current.Span))

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

        let trySplitInterpolationTokensAtFormatColon (tokens: Token list) =
            let tokenArray = tokens |> List.toArray
            let mutable parenDepth = 0
            let mutable bracketDepth = 0
            let mutable braceDepth = 0
            let mutable setBraceDepth = 0
            let mutable interpolatedStringDepth = 0
            let mutable interpolationDepth = 0
            let mutable splitIndex = -1
            let mutable index = 0

            while index < tokenArray.Length && splitIndex < 0 do
                match tokenArray[index].Kind with
                | LeftParen ->
                    parenDepth <- parenDepth + 1
                | RightParen ->
                    parenDepth <- max 0 (parenDepth - 1)
                | LeftBracket ->
                    bracketDepth <- bracketDepth + 1
                | RightBracket ->
                    bracketDepth <- max 0 (bracketDepth - 1)
                | LeftBrace ->
                    braceDepth <- braceDepth + 1
                | RightBrace ->
                    braceDepth <- max 0 (braceDepth - 1)
                | LeftSetBrace ->
                    setBraceDepth <- setBraceDepth + 1
                | RightSetBrace ->
                    setBraceDepth <- max 0 (setBraceDepth - 1)
                | InterpolatedStringStart ->
                    interpolatedStringDepth <- interpolatedStringDepth + 1
                | InterpolatedStringEnd ->
                    interpolatedStringDepth <- max 0 (interpolatedStringDepth - 1)
                | InterpolationStart ->
                    interpolationDepth <- interpolationDepth + 1
                | InterpolationEnd ->
                    interpolationDepth <- max 0 (interpolationDepth - 1)
                | Colon
                    when parenDepth = 0
                         && bracketDepth = 0
                         && braceDepth = 0
                         && setBraceDepth = 0
                         && interpolatedStringDepth = 0
                         && interpolationDepth = 0 ->
                    splitIndex <- index
                | _ ->
                    ()

                index <- index + 1

            if splitIndex < 0 then
                None
            else
                Some(
                    tokenArray[0 .. splitIndex - 1] |> Array.toList,
                    tokenArray[splitIndex],
                    tokenArray[splitIndex + 1 ..] |> Array.toList
                )

        let normalizeInterpolationFormatText (text: string) =
            let trimmedAfterColon =
                if text.StartsWith(" ", StringComparison.Ordinal) then
                    text.Substring(1)
                else
                    text

            if trimmedAfterColon.EndsWith(" ", StringComparison.Ordinal) then
                trimmedAfterColon.Substring(0, trimmedAfterColon.Length - 1)
            else
                trimmedAfterColon

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
                let interpolationTokens = ResizeArray<Token>()
                let mutable nestedInterpolationDepth = 0

                while this.Current.Kind <> EndOfFile
                      && not (this.Current.Kind = InterpolationEnd && nestedInterpolationDepth = 0) do
                    let token = this.Advance()

                    match token.Kind with
                    | InterpolationStart ->
                        nestedInterpolationDepth <- nestedInterpolationDepth + 1
                    | InterpolationEnd when nestedInterpolationDepth > 0 ->
                        nestedInterpolationDepth <- nestedInterpolationDepth - 1
                    | _ ->
                        ()

                    interpolationTokens.Add(token)

                let expressionTokens, formatText =
                    let tokens = List.ofSeq interpolationTokens

                    match trySplitInterpolationTokensAtFormatColon tokens with
                    | Some(expressionTokens, colonToken, formatTokens) when this.Current.Kind = InterpolationEnd ->
                        let formatSpan =
                            let closingToken = this.Current
                            TextSpan.FromBounds(colonToken.Span.End, closingToken.Span.Start)

                        expressionTokens, Some(normalizeInterpolationFormatText (source.Slice formatSpan))
                    | _ ->
                        tokens, None

                let expression = this.ParseStandaloneExpression expressionTokens
                parts.Add(StringInterpolation(expression, formatText))

                if this.Current.Kind = InterpolationEnd then
                    this.Advance() |> ignore
                else
                    diagnostics.AddError(DiagnosticFact.simple SimpleDiagnosticKind.ExpectedSyntaxToken "Expected the interpolation to end before the string resumes.",
                        source.GetLocation(this.Current.Span)
                    )
            | InterpolatedStringEnd ->
                this.Advance() |> ignore
                closed <- true
            | _ ->
                diagnostics.AddError(DiagnosticFact.simple SimpleDiagnosticKind.ExpectedSyntaxToken "Expected interpolated string content.", source.GetLocation(this.Current.Span))
                closed <- true

        if not closed then
            diagnostics.AddError(DiagnosticFact.simple SimpleDiagnosticKind.ExpectedSyntaxToken "Unterminated interpolated string.", source.GetLocation(startToken.Span))

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
            diagnostics.AddError(
                DiagnosticFact.coreExpressionParsing ExpectedSealAs,
                source.GetLocation(this.Current.Span)
            )

        let ascriptionTokens = ResizeArray<Token>()

        while this.Current.Kind <> EndOfFile do
            ascriptionTokens.Add(this.Advance())

        let value =
            if valueTokens.Count = 0 then
                diagnostics.AddError(
                    DiagnosticFact.coreExpressionParsing ExpectedSealValue,
                    source.GetLocation(this.Current.Span)
                )
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
            None
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
                        diagnostics.AddError(DiagnosticFact.simple SimpleDiagnosticKind.ExpectedSyntaxToken "Expected a record field label.", source.GetLocation(labelToken.Span))
                        { Name = "<missing>"
                          IsImplicit = isImplicit
                          Value = Literal LiteralValue.Unit }
                    | [] ->
                        diagnostics.AddError(DiagnosticFact.simple SimpleDiagnosticKind.ExpectedSyntaxToken "Expected a record field label.", source.GetLocation(eofSpan))
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
                        diagnostics.AddError(DiagnosticFact.simple SimpleDiagnosticKind.ExpectedSyntaxToken "Expected a record field of the form 'name = expr' or a punned field name.",
                            source.GetLocation(token.Span)
                        )

                        { Name = "<missing>"
                          IsImplicit = isImplicit
                          Value = Literal LiteralValue.Unit }
                    | [] ->
                        diagnostics.AddError(DiagnosticFact.simple SimpleDiagnosticKind.ExpectedSyntaxToken "Expected a record field of the form 'name = expr' or a punned field name.",
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

        let startsComprehension =
            tokens
            |> List.tryFind (fun token ->
                match token.Kind with
                | Newline
                | Indent
                | Dedent -> false
                | _ -> true)
            |> Option.exists (fun token ->
                Token.isKeyword Keyword.Yield token
                || Token.isKeyword Keyword.For token
                || Token.isKeyword Keyword.ForQuestion token)

        if startsComprehension then
            this.ParseCollectionExpression(MapCollection, tokens)
        else
            let tokens =
                tokens
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
                        diagnostics.AddError(
                            DiagnosticFact.coreExpressionParsing ExpectedNamedApplicationFieldLabel,
                            source.GetLocation(labelToken.Span)
                        )
                        { Name = "<missing>"
                          IsImplicit = isImplicit
                          Value = Literal LiteralValue.Unit }
                    | [] ->
                        diagnostics.AddError(
                            DiagnosticFact.coreExpressionParsing ExpectedNamedApplicationFieldLabel,
                            source.GetLocation(eofSpan)
                        )
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
                            DiagnosticFact.coreExpressionParsing ExpectedNamedApplicationField,
                            source.GetLocation(token.Span)
                        )

                        { Name = "<missing>"
                          IsImplicit = isImplicit
                          Value = Literal LiteralValue.Unit }
                    | [] ->
                        diagnostics.AddError(
                            DiagnosticFact.coreExpressionParsing ExpectedNamedApplicationField,
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
            diagnostics.AddError(DiagnosticFact.simple SimpleDiagnosticKind.ExpectedSyntaxToken $"Expected a name after '{selectorDescription}'.",
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
                DiagnosticFact.coreExpressionParsing ExpectedNameAfterEffectLabel,
                source.GetLocation(this.Current.Span)
            )

            KindQualifiedName(EffectLabelKind, [ "<missing>" ])

    member private this.IsContextualIdentifier(text: string) =
        this.Current.Kind = Identifier
        && String.Equals(SyntaxFacts.trimIdentifierQuotes(this.Current.Text), text, StringComparison.Ordinal)

    member private this.IsHandleExpressionStart() =
        this.IsContextualIdentifier("handle")
        || (this.IsContextualIdentifier("deep")
            && this.Peek(1).Kind = Identifier
            && String.Equals(SyntaxFacts.trimIdentifierQuotes(this.Peek(1).Text), "handle", StringComparison.Ordinal))

    member private this.CollectUntilTopLevelWith() =
        let tokens = ResizeArray<Token>()
        let mutable depth = 0

        while this.Current.Kind <> EndOfFile
              && not (
                  depth = 0
                  && this.Current.Kind = Identifier
                  && String.Equals(SyntaxFacts.trimIdentifierQuotes this.Current.Text, "with", StringComparison.Ordinal)
              ) do
            match this.Current.Kind with
            | LeftParen
            | LeftBracket
            | LeftEffectRow
            | LeftBrace
            | LeftSetBrace ->
                depth <- depth + 1
            | RightParen
            | RightBracket
            | RightEffectRow
            | RightBrace
            | RightSetBrace ->
                depth <- max 0 (depth - 1)
            | _ ->
                ()

            tokens.Add(this.Advance())

        List.ofSeq tokens

    member private this.SplitAtomicGroups(tokens: Token list) =
        let takeAtom remaining =
            match remaining with
            | [] ->
                [], []
            | first :: _ when first.Kind = LeftParen ->
                let tokenArray = remaining |> List.toArray
                let mutable depth = 0
                let mutable index = 0
                let mutable keepReading = true

                while keepReading && index < tokenArray.Length do
                    match tokenArray[index].Kind with
                    | LeftParen ->
                        depth <- depth + 1
                    | RightParen ->
                        depth <- depth - 1

                        if depth = 0 then
                            keepReading <- false
                    | _ ->
                        ()

                    index <- index + 1

                List.ofArray tokenArray[0 .. index - 1], List.ofArray tokenArray[index ..]
            | first :: _ when first.Kind = LeftBrace ->
                let tokenArray = remaining |> List.toArray
                let mutable depth = 0
                let mutable index = 0
                let mutable keepReading = true

                while keepReading && index < tokenArray.Length do
                    match tokenArray[index].Kind with
                    | LeftBrace ->
                        depth <- depth + 1
                    | RightBrace ->
                        depth <- depth - 1

                        if depth = 0 then
                            keepReading <- false
                    | _ ->
                        ()

                    index <- index + 1

                List.ofArray tokenArray[0 .. index - 1], List.ofArray tokenArray[index ..]
            | first :: rest ->
                [ first ], rest

        let rec loop remaining groups =
            match remaining with
            | [] -> List.rev groups
            | _ ->
                let groupTokens, rest = takeAtom remaining
                loop rest (groupTokens :: groups)

        loop tokens []

    member private this.ParseEffectHandlerClause(lineTokens: Token list) =
        let trimLineTokens tokens =
            let isLayoutToken token =
                match token.Kind with
                | Newline
                | Indent
                | Dedent
                | EndOfFile -> true
                | _ -> false

            tokens
            |> List.skipWhile isLayoutToken
            |> List.rev
            |> List.skipWhile isLayoutToken
            |> List.rev

        let significantTokens tokens =
            tokens
            |> List.filter (fun token ->
                match token.Kind with
                | Newline
                | Indent
                | Dedent
                | EndOfFile -> false
                | _ -> true)

        let trimmedTokens = trimLineTokens lineTokens

        match trimmedTokens with
        | caseToken :: rest when Token.isKeyword Keyword.Case caseToken ->
            let tokenArray = rest |> List.toArray
            let mutable depth = 0
            let mutable splitIndex = -1
            let mutable index = 0

            while index < tokenArray.Length && splitIndex < 0 do
                match tokenArray[index].Kind with
                | Newline
                | Indent
                | Dedent
                | EndOfFile -> ()
                | LeftParen
                | LeftBracket
                | LeftEffectRow
                | LeftBrace
                | LeftSetBrace ->
                    depth <- depth + 1
                | RightParen
                | RightBracket
                | RightEffectRow
                | RightBrace
                | RightSetBrace ->
                    depth <- max 0 (depth - 1)
                | Arrow when depth = 0 ->
                    splitIndex <- index
                | _ ->
                    ()

                index <- index + 1

            if splitIndex < 0 then
                diagnostics.AddError(
                    DiagnosticFact.coreExpressionParsing ExpectedHandlerClauseArrow,
                    source.GetLocation(caseToken.Span)
                )
                { OperationName = "<missing>"
                  ArgumentTokens = []
                  ResumptionName = None
                  Body = Literal LiteralValue.Unit }
            else
                let headerTokens = tokenArray[0 .. splitIndex - 1] |> Array.toList |> significantTokens
                let bodyTokens = tokenArray[splitIndex + 1 ..] |> Array.toList

                match headerTokens with
                | operationToken :: clauseTokens when Token.isName operationToken || Token.isKeyword Keyword.Return operationToken ->
                    let operationName =
                        if Token.isKeyword Keyword.Return operationToken then
                            "return"
                        else
                            SyntaxFacts.trimIdentifierQuotes operationToken.Text

                    let argumentGroups = this.SplitAtomicGroups clauseTokens

                    let argumentTokens, resumptionName =
                        if String.Equals(operationName, "return", StringComparison.Ordinal) then
                            argumentGroups, None
                        else
                            match List.rev argumentGroups with
                            | [ ] ->
                                diagnostics.AddError(
                                    DiagnosticFact.coreExpressionParsing ExpectedHandlerResumptionBinder,
                                    source.GetLocation(operationToken.Span)
                                )

                                [], None
                            | lastGroup :: reversedArguments ->
                                let resumptionName =
                                    match lastGroup with
                                    | [ nameToken ] when this.IsNameToken(nameToken) ->
                                        Some(SyntaxFacts.trimIdentifierQuotes nameToken.Text)
                                    | _ ->
                                        diagnostics.AddError(
                                            DiagnosticFact.coreExpressionParsing ExpectedHandlerResumptionBinder,
                                            source.GetLocation(operationToken.Span)
                                        )

                                        None

                                List.rev reversedArguments, resumptionName

                    { OperationName = operationName
                      ArgumentTokens = argumentTokens
                      ResumptionName = resumptionName
                      Body = this.ParseStandaloneExpression(bodyTokens) }
                | _ ->
                    diagnostics.AddError(
                        DiagnosticFact.coreExpressionParsing ExpectedHandlerClauseHead,
                        source.GetLocation(caseToken.Span)
                    )
                    { OperationName = "<missing>"
                      ArgumentTokens = []
                      ResumptionName = None
                      Body = Literal LiteralValue.Unit }
        | token :: _ ->
            diagnostics.AddError(
                DiagnosticFact.coreExpressionParsing ExpectedHandlerClauseStartingWithCase,
                source.GetLocation(token.Span)
            )
            { OperationName = "<missing>"
              ArgumentTokens = []
              ResumptionName = None
              Body = Literal LiteralValue.Unit }
        | [] ->
            diagnostics.AddError(
                DiagnosticFact.coreExpressionParsing ExpectedHandlerClause,
                source.GetLocation(this.Current.Span)
            )
            { OperationName = "<missing>"
              ArgumentTokens = []
              ResumptionName = None
              Body = Literal LiteralValue.Unit }

    member private this.ParseHandleExpression() =
        let isDeep =
            if this.IsContextualIdentifier("deep") then
                this.Advance() |> ignore
                true
            else
                false

        this.Advance() |> ignore
        let label = this.ParsePrefixExpression()
        let bodyTokens = this.CollectUntilTopLevelWith()

        if not (this.IsContextualIdentifier("with")) then
            diagnostics.AddError(
                DiagnosticFact.coreExpressionParsing ExpectedHandlerWith,
                source.GetLocation(this.Current.Span)
            )

        this.Advance() |> ignore
        let handledBody = this.ParseStandaloneExpression(bodyTokens)
        let clauses = this.CollectIndentedLines() |> List.filter (List.isEmpty >> not) |> List.map this.ParseEffectHandlerClause
        let returnClauses = clauses |> List.filter (fun clause -> String.Equals(clause.OperationName, "return", StringComparison.Ordinal))
        let operationClauses = clauses |> List.filter (fun clause -> not (String.Equals(clause.OperationName, "return", StringComparison.Ordinal)))

        if List.length returnClauses > 1 then
            diagnostics.AddError(DiagnosticFact.simple SimpleDiagnosticKind.HandlerClauseDuplicate "A handler must not define more than one return clause.",
                source.GetLocation(this.Current.Span)
            )

        let returnClause =
            match returnClauses with
            | clause :: _ -> clause
            | [] ->
                diagnostics.AddError(DiagnosticFact.simple SimpleDiagnosticKind.HandlerClauseMissing "A handler must define exactly one return clause of the form 'case return x -> ...'.",
                    source.GetLocation(this.Current.Span)
                )
                { OperationName = "return"
                  ArgumentTokens = []
                  ResumptionName = None
                  Body = Literal LiteralValue.Unit }

        if List.length returnClause.ArgumentTokens <> 1 then
            diagnostics.AddError(DiagnosticFact.simple SimpleDiagnosticKind.HandlerClauseArityMismatch $"A handler return clause must bind exactly one payload argument, but binds {List.length returnClause.ArgumentTokens}.",
                source.GetLocation(this.Current.Span)
            )

        Handle(isDeep, label, handledBody, returnClause, operationClauses)

    member private this.ParseLetExpression() =
        let trimLineTokens lineTokens =
            let isLayoutToken token =
                match token.Kind with
                | Newline
                | Indent
                | Dedent -> true
                | _ -> false

            lineTokens
            |> List.skipWhile isLayoutToken
            |> List.rev
            |> List.skipWhile isLayoutToken
            |> List.rev

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

        let splitBindingTypeAnnotation tokens =
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
                tokenArray[0 .. splitIndex - 1] |> Array.toList,
                Some(tokenArray[splitIndex + 1 ..] |> Array.toList)
            else
                tokens, None

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
                | leftToken :: operatorToken :: rest
                    when Token.isName leftToken && operatorToken.Kind = Operator ->
                    operatorToken.Text, operatorToken.Span, leftToken :: rest
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

            let parameterTokens, returnTypeTokens =
                splitReturnTypeTokens parameterHeaderTokens

            let parsedParameters =
                HeaderParsing.parseHeaderParameters source diagnostics LocalFunctionHeader false parameterTokens

            let value =
                match parsedParameters with
                | [] ->
                    this.ParseStandaloneExpression(valueTokens)
                | parsedParameters ->
                    Lambda(parsedParameters, this.ParseStandaloneExpression(valueTokens))

            let binding =
                { Pattern = NamePattern bindingName
                  Quantity = None
                  IsImplicit = false
                  TypeTokens = if List.isEmpty parsedParameters then returnTypeTokens else None
                  BinderSpans = Map.ofList [ bindingName, [ bindingNameSpan ] ] }

            binding, value

        this.ExpectKeyword(Keyword.Let, "Expected 'let'.") |> ignore

        let bindingAndValueTokens = ResizeArray<Token>()
        let mutable bracketDepth = 0
        let mutable layoutDepth = 0
        let mutable nestedLocalLetDepth = 0
        let mutable foundIn = false

        while not foundIn && this.Current.Kind <> EndOfFile do
            if bracketDepth = 0 && Token.isKeyword Keyword.In this.Current then
                if nestedLocalLetDepth = 0 then
                    foundIn <- true
                else
                    nestedLocalLetDepth <- nestedLocalLetDepth - 1
                    bindingAndValueTokens.Add(this.Advance())
            else
                match this.Current.Kind with
                | Keyword Keyword.Let when bracketDepth = 0 ->
                    nestedLocalLetDepth <- nestedLocalLetDepth + 1
                    bindingAndValueTokens.Add(this.Advance())
                | LeftParen
                | LeftBracket
                | LeftBrace
                | LeftSetBrace ->
                    bracketDepth <- bracketDepth + 1
                    bindingAndValueTokens.Add(this.Advance())
                | RightParen
                | RightBracket
                | RightBrace
                | RightSetBrace ->
                    bracketDepth <- max 0 (bracketDepth - 1)
                    bindingAndValueTokens.Add(this.Advance())
                | Indent when bracketDepth = 0 ->
                    layoutDepth <- layoutDepth + 1
                    bindingAndValueTokens.Add(this.Advance())
                | Dedent when bracketDepth = 0 && layoutDepth > 0 ->
                    layoutDepth <- layoutDepth - 1
                    bindingAndValueTokens.Add(this.Advance())
                | _ ->
                    bindingAndValueTokens.Add(this.Advance())

        if this.Current.Kind = EndOfFile then
            diagnostics.AddError(DiagnosticFact.simple SimpleDiagnosticKind.ExpectedSyntaxToken "Expected 'in' after the local let binding.",
                source.GetLocation(eofSpan)
            )

            Literal LiteralValue.Unit
        else
            this.Advance() |> ignore

            let trimmedBindingAndValueTokens =
                bindingAndValueTokens |> Seq.toList |> trimLineTokens

            match tryFindTopLevelEquals trimmedBindingAndValueTokens with
            | Some index ->
                let tokenArray = List.toArray trimmedBindingAndValueTokens
                let headerTokens = tokenArray[0 .. index - 1] |> Array.toList
                let valueTokens = tokenArray[index + 1 ..] |> Array.toList |> trimLineTokens

                let binding, value =
                    match headerTokens with
                    | nameToken :: restHeader
                        when this.IsNameToken(nameToken)
                             && not (List.isEmpty restHeader)
                             && restHeader.Head.Kind <> Colon ->
                        parseNamedLocalBinding headerTokens valueTokens
                    | _ ->
                        let bindingTokens, bindingTypeTokens = splitBindingTypeAnnotation headerTokens
                        let binding =
                            this.ParseBindPatternFromTokens bindingTokens
                            |> fun current ->
                                { current with
                                    TypeTokens = bindingTypeTokens |> Option.orElse current.TypeTokens }
                        binding,
                        this.ParseStandaloneExpression(valueTokens)

                let body = this.ParseExpression(0)
                LocalLet(binding, value, body)
            | None ->
                diagnostics.AddError(DiagnosticFact.simple SimpleDiagnosticKind.ExpectedSyntaxToken "Expected '=' in the local let binding.",
                    source.GetLocation(eofSpan)
                )

                Literal LiteralValue.Unit

    member private this.ParsePrimaryExpression() =
        this.SkipLayout()

        match this.Current.Kind with
        | Dot when inCodeQuote && this.Peek(1).Kind = Operator && this.Peek(1).Text = "~" ->
            this.Advance() |> ignore
            this.Advance() |> ignore
            CodeSplice(this.ParseApplicationExpression())
        | Keyword Keyword.Match when this.Peek(1).Kind = Dot ->
            let segments = this.ParseQualifiedName()
            Name segments
        | Keyword Keyword.Match ->
            this.ParseMatchExpression()
        | Keyword Keyword.Do ->
            this.ParseDoExpression()
        | Keyword Keyword.If ->
            this.ParseIfExpression()
        | Keyword Keyword.Let ->
            this.ParseLetExpression()
        | Keyword Keyword.Seal ->
            this.ParseSealExpression()
        | _ when this.IsHandleExpressionStart() ->
            this.ParseHandleExpression()
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
            | Result.Error error ->
                diagnostics.AddError(
                    DiagnosticFact.coreExpressionParsing (InvalidNumericLiteralExpression error),
                    source.GetLocation(token.Span)
                )
                Literal(Integer 0L)
        | FloatLiteral ->
            let token = this.Advance()

            match SyntaxFacts.tryParseNumericLiteral token.Text with
            | Result.Ok parsed ->
                NumericLiteral parsed.Literal
            | Result.Error error ->
                diagnostics.AddError(
                    DiagnosticFact.coreExpressionParsing (InvalidNumericLiteralExpression error),
                    source.GetLocation(token.Span)
                )
                Literal(Float 0.0)
        | StringLiteral ->
            let token = this.Advance()
            Literal(LiteralValue.String(this.DecodeStringLiteral token))
        | _ when this.IsAdjacentPrefixedCharacterLiteral("g") ->
            let prefixToken = this.Advance()
            let token = this.Advance()
            Literal(LiteralValue.Grapheme(this.DecodeGraphemeLiteral(prefixToken, token)))
        | _ when this.IsAdjacentPrefixedCharacterLiteral("b") ->
            let prefixToken = this.Advance()
            let token = this.Advance()
            Literal(LiteralValue.Byte(this.DecodeByteLiteral(prefixToken, token)))
        | InterpolatedStringStart ->
            this.ParseInterpolatedString()
        | CharacterLiteral ->
            let token = this.Advance()
            Literal(Character(this.DecodeCharacterLiteral token))
        | Dot when this.Peek(1).Kind = Operator && this.Peek(1).Text = "<" ->
            let innerTokens = this.CollectCodeQuoteTokens("Expected '>.' to close the code quote.")
            CodeQuote(this.ParseNestedExpression(innerTokens, false, true))
        | Underscore ->
            this.Advance() |> ignore
            Name [ "_" ]
        | LeftParen ->
            let innerTokens = this.CollectParenthesizedTokens()
            let tryParseTupleExpression (tokens: Token list) =
                let trimmed =
                    tokens
                    |> List.filter (fun token ->
                        match token.Kind with
                        | Newline
                        | Indent
                        | Dedent -> false
                        | _ -> true)

                let tokenArray = List.toArray trimmed
                let groups = ResizeArray<Token list>()
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
                        groups.Add(List.ofSeq current)
                        current.Clear()
                    | _ ->
                        current.Add(token)

                groups.Add(List.ofSeq current)
                let groups = List.ofSeq groups
                let normalizedGroups =
                    if sawTopLevelComma then
                        groups
                        |> List.rev
                        |> function
                            | [] -> []
                            | [] :: rest -> List.rev rest
                            | current -> List.rev current
                    else
                        groups

                if not sawTopLevelComma || List.isEmpty normalizedGroups || (normalizedGroups |> List.exists List.isEmpty) then
                    None
                else
                    let parsedElements =
                        normalizedGroups
                        |> List.map (fun group -> this.TryParseNestedExpressionWithoutDiagnostics(group, inSyntaxQuote, inCodeQuote))

                    if parsedElements |> List.forall Option.isSome then
                        Some(
                            RecordLiteral(
                                parsedElements
                                |> List.choose id
                                |> List.mapi (fun index expression ->
                                    { Name = $"_{index + 1}"
                                      Value = expression
                                      IsImplicit = false })
                            )
                        )
                    else
                        None

            let tryParseTypedExpression (tokens: Token list) =
                let tokenArray = tokens |> List.toArray
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

                match splitIndex with
                | colonIndex when colonIndex > 0 && colonIndex + 1 < tokenArray.Length ->
                    let valueTokens = tokenArray[0 .. colonIndex - 1] |> Array.toList

                    this.TryParseStandaloneExpression valueTokens
                | _ ->
                    None

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
                            match tryParseTupleExpression innerTokens with
                            | Some expression ->
                                expression
                            | None ->
                                match tryParseTypedExpression innerTokens with
                                | Some expression ->
                                    expression
                                | None ->
                                    match this.TryParseStandaloneExpression innerTokens with
                                    | Some expression ->
                                        expression
                                    | None ->
                                        diagnostics.AddError(DiagnosticFact.simple SimpleDiagnosticKind.ExpectedSyntaxToken "Expected an expression inside parentheses.", source.GetLocation(this.Current.Span))
                                        Literal Unit
        | LeftBracket ->
            let innerTokens = this.CollectBracketedTokens("Expected ']' to close the list expression.")
            this.ParseCollectionExpression(ListCollection, innerTokens)
        | LeftBrace ->
            let innerTokens = this.CollectBracedTokens("Expected '}' to close the map expression.")
            this.ParseCollectionExpression(MapCollection, innerTokens)
        | LeftSetBrace ->
            let innerTokens = this.CollectSetBracedTokens("Expected '|}' to close the set expression.")
            this.ParseCollectionExpression(SetCollection, innerTokens)
        | _ when this.IsNameToken(this.Current) ->
            let segments = this.ParseQualifiedName()
            Name segments
        | _ ->
            diagnostics.AddError(DiagnosticFact.simple SimpleDiagnosticKind.ExpectedSyntaxToken "Expected an expression.", source.GetLocation(this.Current.Span))
            Literal Unit

    member private this.ParsePrefixExpression() =
        this.SkipLayout()

        match this.Current.Kind with
        | AtSign ->
            this.Advance() |> ignore
            if this.Current.Kind = LeftParen then
                let innerTokens = this.CollectParenthesizedTokens()
                this.ParseParenthesizedExplicitImplicitArgument(innerTokens)
            else
                ExplicitImplicitArgument(this.ParsePrefixExpression())
        | Operator ->
            if this.Current.Text = "'" && this.Peek(1).Kind = LeftBrace then
                this.Advance() |> ignore
                let innerTokens = this.CollectBracedTokens("Expected '}' to close the syntax quote.")
                SyntaxQuote(this.ParseNestedSyntaxPayload(innerTokens))
            elif this.Current.Text = "$" && this.Peek(1).Kind = LeftParen then
                this.Advance() |> ignore
                let innerTokens = this.CollectParenthesizedTokens()
                TopLevelSyntaxSplice(this.ParseNestedExpression(innerTokens, false, false))
            elif this.Current.Text = "$" && inSyntaxQuote && this.Peek(1).Kind = LeftBrace then
                this.Advance() |> ignore
                let innerTokens = this.CollectBracedTokens("Expected '}' to close the syntax splice.")
                SyntaxSplice(this.ParseNestedExpression(innerTokens, false, false))
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
                DiagnosticFact.coreExpressionParsing ExpectedConstructorNameAfterIs,
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
                diagnostics.AddError(
                    DiagnosticFact.coreExpressionParsing ExpectedRecordUpdateClose,
                    source.GetLocation(startToken.Span)
                )

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
                    diagnostics.AddError(DiagnosticFact.simple SimpleDiagnosticKind.RecordPatchInvalidItem "Row-extension fields must be top-level labels of the form 'name := expr'.", source.GetLocation(tokenArray[operatorStart].Span))
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

                    diagnostics.AddError(DiagnosticFact.simple SimpleDiagnosticKind.RecordPatchInvalidItem "Expected a record patch path before '=' or ':='.", source.GetLocation(errorSpan))
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

                diagnostics.AddError(DiagnosticFact.simple SimpleDiagnosticKind.RecordPatchInvalidItem "Expected a record patch item of the form 'path = expr' or 'name := expr'.", source.GetLocation(errorSpan))
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
            elif this.Current.Kind = Dot && this.Peek(1).Kind = LeftParen then
                this.Advance() |> ignore

                let innerTokens = this.CollectParenthesizedTokens()

                let memberName =
                    innerTokens
                    |> List.filter (fun token ->
                        match token.Kind with
                        | Newline
                        | Indent
                        | Dedent
                        | EndOfFile -> false
                        | _ -> true)
                    |> function
                        | [ token ] when this.IsNameToken(token) || token.Kind = Operator ->
                            Some(SyntaxFacts.trimIdentifierQuotes token.Text)
                        | tokens when tokens |> List.forall (fun token -> token.Kind = Operator) ->
                            tokens
                            |> List.map (fun token -> token.Text)
                            |> String.concat ""
                            |> Some
                        | _ ->
                            None

                match memberName with
                | Some memberName ->
                    let memberArguments = ResizeArray<SurfaceExpression>()
                    let mutable keepReadingMemberArguments = true

                    while keepReadingMemberArguments do
                        this.SkipLayout()

                        if this.CanStartApplicationArgumentAfterHead(this.Current) then
                            memberArguments.Add(this.ParseApplicationArgument())
                        else
                            keepReadingMemberArguments <- false

                    expression <- MemberAccess(expression, [ memberName ], List.ofSeq memberArguments)
                | None ->
                    diagnostics.AddError(
                        DiagnosticFact.coreExpressionParsing ExpectedExplicitMemberProjectionName,
                        source.GetLocation(this.Current.Span)
                    )
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
                        DiagnosticFact.coreExpressionParsing ExpectedSafeNavigationMemberAccess,
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
                            DiagnosticFact.coreExpressionParsing ConstructorTagTestsCannotBeChained,
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
                diagnostics.AddError(
                    DiagnosticFact.coreExpressionParsing UnexpectedTrailingExpressionTokens,
                    source.GetLocation(this.Current.Span)
                )

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
        let parameters =
            HeaderParsing.parseHeaderParameters source diagnostics TopLevelFunctionHeader true parameterTokens

        { Parameters = parameters
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
                    diagnostics.AddError(DiagnosticFact.simple SimpleDiagnosticKind.ExpectedSyntaxToken "Expected a projection place binder of the form '(place name : Type)'.", source.GetLocation(startSpan))
                    None
            | _ ->
                match SurfaceBinderParsing.parseParameterFromTokens diagnostics source startSpan trimmed with
                | Some parameter when parameter.IsInout ->
                    diagnostics.AddError(DiagnosticFact.simple SimpleDiagnosticKind.ExpectedSyntaxToken "Projection parameters must not use 'inout'.", source.GetLocation(startSpan))
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
                diagnostics.AddError(DiagnosticFact.simple SimpleDiagnosticKind.ExpectedSyntaxToken "Expected ':' before the projection result type.", source.GetLocation(TextSpan.FromBounds(source.Length, source.Length)))
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
                    diagnostics.AddError(DiagnosticFact.simple SimpleDiagnosticKind.ExpectedSyntaxToken "Unterminated projection binder.", source.GetLocation(startToken.Span))
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
                diagnostics.AddError(DiagnosticFact.simple SimpleDiagnosticKind.ExpectedSyntaxToken "Unsupported projection binder syntax.", source.GetLocation(tokenArray[index].Span))
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
            let parser = ExpressionParser(tokens, source, diagnostics, fixities, false, false)
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
                                diagnostics.AddError(DiagnosticFact.simple SimpleDiagnosticKind.ExpectedSyntaxToken "Projection set accessors use an ordinary '(name : Type)' parameter.", source.GetLocation(startSpan))

                            Some(ProjectionSet(parameter.Name, typeTokens, parseExpr bodyTokens))
                        | None ->
                            diagnostics.AddError(DiagnosticFact.simple SimpleDiagnosticKind.ExpectedSyntaxToken "Projection set accessors require a typed parameter.", source.GetLocation(startSpan))
                            None
                    | None ->
                        None
                | _ ->
                    diagnostics.AddError(DiagnosticFact.simple SimpleDiagnosticKind.ExpectedSyntaxToken "Expected a projection set accessor parameter of the form '(name : Type)'.", source.GetLocation(startSpan))
                    None
            | token :: _ ->
                diagnostics.AddError(DiagnosticFact.simple SimpleDiagnosticKind.ExpectedSyntaxToken "Expected a projection set accessor.", source.GetLocation(token.Span))
                None
            | [] ->
                diagnostics.AddError(DiagnosticFact.simple SimpleDiagnosticKind.ExpectedSyntaxToken "Expected a projection set accessor.", source.GetLocation(TextSpan.FromBounds(source.Length, source.Length)))
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
                    diagnostics.AddError(DiagnosticFact.simple SimpleDiagnosticKind.ExpectedSyntaxToken "Expected a projection accessor clause.", source.GetLocation(token.Span))
                    None
                | [] ->
                    diagnostics.AddError(DiagnosticFact.simple SimpleDiagnosticKind.ExpectedSyntaxToken "Expected a projection accessor clause.", source.GetLocation(TextSpan.FromBounds(source.Length, source.Length)))
                    None
            | None ->
                match clauseTokens with
                | token :: _ ->
                    diagnostics.AddError(DiagnosticFact.simple SimpleDiagnosticKind.ExpectedSyntaxToken "Expected '->' in the projection accessor clause.", source.GetLocation(token.Span))
                | [] ->
                    diagnostics.AddError(DiagnosticFact.simple SimpleDiagnosticKind.ExpectedSyntaxToken "Expected '->' in the projection accessor clause.", source.GetLocation(TextSpan.FromBounds(source.Length, source.Length)))

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
                diagnostics.AddError(DiagnosticFact.simple SimpleDiagnosticKind.ExpectedSyntaxToken "Expected 'then' in the projection body.", source.GetLocation(TextSpan.FromBounds(source.Length, source.Length)))
                parseYield rest
            | Some(conditionTokens, afterThen) ->
                match splitTopLevelAtKeyword Keyword.Else afterThen with
                | None ->
                    diagnostics.AddError(DiagnosticFact.simple SimpleDiagnosticKind.ExpectedSyntaxToken "Expected 'else' in the projection body.", source.GetLocation(TextSpan.FromBounds(source.Length, source.Length)))
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
                    diagnostics.AddError(
                        DiagnosticFact.coreExpressionParsing ExpectedProjectionMatchCaseBlock,
                        source.GetLocation(TextSpan.FromBounds(source.Length, source.Length))
                    )
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
                                diagnostics.AddError(
                                    DiagnosticFact.coreExpressionParsing ExpectedProjectionCaseClauseArrow,
                                    source.GetLocation(caseToken.Span)
                                )
                                None
                        | _ ->
                            diagnostics.AddError(
                                DiagnosticFact.coreExpressionParsing ExpectedProjectionMatchCaseClause,
                                source.GetLocation(TextSpan.FromBounds(source.Length, source.Length))
                            )
                            None)
                | None ->
                    diagnostics.AddError(
                        DiagnosticFact.coreExpressionParsing ExpectedProjectionMatchCaseBlock,
                        source.GetLocation(TextSpan.FromBounds(source.Length, source.Length))
                    )
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
            diagnostics.AddError(
                DiagnosticFact.coreExpressionParsing ExpectedProjectionBodyHead,
                source.GetLocation(head.Span)
            )
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
                let parser = ExpressionParser(tokens, source, diagnostics, fixities, false, false)
                parser.Parse()
        else
            let parser = ExpressionParser(tokens, source, diagnostics, fixities, false, false)
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

        let splitBindingTypeAnnotation tokens =
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
                tokenArray[0 .. splitIndex - 1] |> Array.toList,
                Some(tokenArray[splitIndex + 1 ..] |> Array.toList)
            else
                tokens, None

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
                let rawBindingTokens =
                    tokenArray[0 .. index - 1] |> Array.toList

                let bindingTokens, bindingTypeTokens =
                    splitBindingTypeAnnotation rawBindingTokens

                let valueTokens = tokenArray[index + 1 ..] |> Array.toList
                parseBindingLineCore bindingTokens valueTokens
                |> fun (binding, value) ->
                    Some(
                        { binding with
                            TypeTokens = bindingTypeTokens |> Option.orElse binding.TypeTokens },
                        value
                    )
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
            | _ when SurfaceEffectParsing.parseScopedEffectDeclaration lineTokens |> Option.isSome ->
                SurfaceEffectParsing.parseScopedEffectDeclaration lineTokens |> Option.map Choice2Of2
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
                | Some(Choice2Of2 declaration) -> LocalScopedEffect(declaration, current)
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
            | Some lines ->
                let trimmedLines =
                    lines |> List.map trimLineTokens

                let bindingLines =
                    trimmedLines |> List.takeWhile (parseSimpleLocalBindingLine >> Option.isSome)

                let remainingLines =
                    trimmedLines |> List.skipWhile (parseSimpleLocalBindingLine >> Option.isSome)

                match remainingLines with
                | inLine :: bodyLines
                    when not (List.isEmpty bindingLines) ->
                    match inLine with
                    | inToken :: bodyStartTokens when Token.isKeyword Keyword.In inToken ->
                        let bodyTokens =
                            bodyStartTokens
                            @ (bodyLines
                               |> List.mapi (fun index line ->
                                   if index = 0 || List.isEmpty line then
                                       line
                                   else
                                       { Kind = Newline
                                         Text = ""
                                         Span = line.Head.Span }
                                       :: line)
                               |> List.concat)

                        Some(buildNestedExpression parseSimpleLocalBindingLine bindingLines bodyTokens)
                    | _ ->
                        let afterBindings =
                            rest
                            |> List.skipWhile (fun token -> token.Kind <> Dedent)
                            |> function
                                | _ :: remaining -> trimLeadingLayout remaining
                                | [] -> []

                        match afterBindings with
                        | inToken :: bodyTokens
                            when Token.isKeyword Keyword.In inToken
                                 && (bindingLines |> List.forall (parseSimpleLocalBindingLine >> Option.isSome)) ->
                            Some(buildNestedExpression parseSimpleLocalBindingLine bindingLines bodyTokens)
                        | _ ->
                            None
                | _ ->
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
                    let trimmedBindingTokens = trimLineTokens bindingTokens
                    let trimmedBodyTokens = trimLineTokens bodyTokens

                    match parseBindingLineTokens trimmedBindingTokens with
                    | Some(binding, value) ->
                        let body =
                            parseExpression fixities source diagnostics trimmedBodyTokens
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
