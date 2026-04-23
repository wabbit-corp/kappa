namespace Kappa.Compiler

open System

type LetHeaderParseResult =
    { Parameters: Parameter list
      ReturnTypeTokens: Token list option }

type ProjectionHeaderParseResult =
    { Binders: ProjectionBinder list
      ReturnTypeTokens: Token list }

// Parses shared binder forms used by both surface syntax and type/signature parsing.
module private SurfaceBinderParsing =
    let makeParameter name typeTokens quantity isImplicit isInout =
        { Name = name
          TypeTokens = typeTokens
          Quantity = quantity
          IsImplicit = isImplicit
          IsInout = isInout }

    // Wildcard binders elaborate to fresh internal names that cannot be referenced from source.
    let wildcardParameterName (span: TextSpan) =
        $"__kappa_wildcard_{span.Start}"

    let private tokenTextEquals expected (token: Token) =
        String.Equals(token.Text, expected, StringComparison.Ordinal)

    let private isOmegaText text =
        String.Equals(text, "\u03c9", StringComparison.Ordinal)
        || String.Equals(text, "omega", StringComparison.Ordinal)

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
        | head :: rest when Token.isName head ->
            // Quantity variables are only unambiguous when another binder name follows.
            match rest with
            | next :: _ when Token.isName next ->
                Some(QuantityVariable(SyntaxFacts.trimIdentifierQuotes head.Text), rest)
            | _ ->
                None
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

            match bodyTokens with
            | [] ->
                diagnostics.AddError(DiagnosticCode.ParseError, "Expected a parameter binder.", source.GetLocation(eofSpan))
                None
            | { Kind = Underscore; Span = span } :: tail ->
                let name = wildcardParameterName span

                let typeTokens =
                    match tail with
                    | [] -> None
                    | colon :: rest when colon.Kind = Colon -> Some rest
                    | _ ->
                        unsupported span
                        None

                Some(makeParameter name typeTokens quantity isImplicit isInout)
            | head :: tail when Token.isName head ->
                let name = SyntaxFacts.trimIdentifierQuotes head.Text

                let typeTokens =
                    match tail with
                    | [] -> None
                    | colon :: rest when colon.Kind = Colon -> Some rest
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

    let parseBindPatternFromTokens (parsePattern: Token list -> SurfacePattern) (tokens: Token list) =
        match tryParseQuantityPrefix tokens with
        | Some(quantity, rest) ->
            { Pattern = parsePattern rest
              Quantity = Some quantity }
        | None ->
            { Pattern = parsePattern tokens
              Quantity = None }

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
            match Int64.TryParse(token.Text) with
            | true, value -> LiteralPattern(LiteralValue.Integer value)
            | _ ->
                diagnostics.AddError(DiagnosticCode.ParseError, $"Invalid integer literal '{token.Text}'.", source.GetLocation(token.Span))
                LiteralPattern(LiteralValue.Integer 0L)
        | FloatLiteral ->
            match Double.TryParse(token.Text) with
            | true, value -> LiteralPattern(LiteralValue.Float value)
            | _ ->
                diagnostics.AddError(DiagnosticCode.ParseError, $"Invalid float literal '{token.Text}'.", source.GetLocation(token.Span))
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

    member private this.TryParseAnonymousRecordPattern(tokens: Token list) =
        let trimmed =
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

            items |> Seq.toList

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

        if trimmed |> List.exists (fun token -> token.Kind = Equals) |> not then
            None
        else
            let fields =
                splitTopLevelCommas trimmed
                |> List.filter (List.isEmpty >> not)
                |> List.map (fun fieldTokens ->
                    match tryFindTopLevelEquals fieldTokens with
                    | Some index ->
                        let tokenArray = List.toArray fieldTokens
                        let labelTokens = tokenArray[0 .. index - 1] |> Array.toList
                        let patternTokens = tokenArray[index + 1 ..] |> Array.toList

                        match labelTokens with
                        | [ labelToken ] when this.IsNameToken(labelToken) ->
                            let nestedParser = PatternParser(patternTokens, source, diagnostics, fixities)

                            { Name = SyntaxFacts.trimIdentifierQuotes labelToken.Text
                              Pattern = nestedParser.Parse() }
                        | labelToken :: _ ->
                            diagnostics.AddError(DiagnosticCode.ParseError, "Expected a record pattern field label.", source.GetLocation(labelToken.Span))
                            { Name = "<missing>"
                              Pattern = WildcardPattern }
                        | [] ->
                            diagnostics.AddError(DiagnosticCode.ParseError, "Expected a record pattern field label.", source.GetLocation(eofSpan))
                            { Name = "<missing>"
                              Pattern = WildcardPattern }
                    | None ->
                        let errorSpan =
                            match fieldTokens with
                            | token :: _ -> token.Span
                            | [] -> eofSpan

                        diagnostics.AddError(DiagnosticCode.ParseError, "Expected a record pattern field of the form 'name = pattern'.", source.GetLocation(errorSpan))
                        { Name = "<missing>"
                          Pattern = WildcardPattern })

            Some(AnonymousRecordPattern fields)

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
                match this.TryParseAnonymousRecordPattern innerTokens with
                | Some pattern -> pattern
                | None ->
                    let nestedParser = PatternParser(innerTokens, source, diagnostics, fixities)
                    nestedParser.Parse()
        | _ when this.IsNameToken(this.Current) ->
            let segments = this.ParseQualifiedName()

            if this.IsConstructorSegments(segments) then
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
        | Backslash -> true
        | Operator when FixityTable.tryFindPrefix token.Text fixities |> Option.isSome -> true
        | Keyword Keyword.If -> true
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

    member private this.ParseStandaloneExpression(tokens: Token list) =
        let nestedParser = ExpressionParser(tokens, source, diagnostics, fixities)
        nestedParser.Parse() |> Option.defaultValue (Literal LiteralValue.Unit)

    member private this.ParsePatternFromTokens(tokens: Token list) =
        let nestedParser = PatternParser(tokens, source, diagnostics, fixities)
        nestedParser.Parse()

    member private this.ParseBindPatternFromTokens(tokens: Token list) =
        SurfaceBinderParsing.parseBindPatternFromTokens this.ParsePatternFromTokens tokens

    member private _.MakeOperatorSection body =
        let parameterName = "__sectionArg"

        Lambda(
            [ SurfaceBinderParsing.makeParameter parameterName None None false false ],
            body
        )

    member private this.TryParseOperatorSection(tokens: Token list) =
        match tokens with
        | [] ->
            None
        | [ operatorToken ] when operatorToken.Kind = Operator ->
            Some(Name [ operatorToken.Text ])
        | operatorToken :: operandTokens when operatorToken.Kind = Operator ->
            this.TryParseStandaloneExpression operandTokens
            |> Option.map (fun operand ->
                this.MakeOperatorSection(Binary(Name [ "__sectionArg" ], operatorToken.Text, operand)))
        | _ ->
            match List.rev tokens with
            | operatorToken :: prefixTokens when operatorToken.Kind = Operator ->
                this.TryParseStandaloneExpression (List.rev prefixTokens)
                |> Option.map (fun left ->
                    this.MakeOperatorSection(Binary(left, operatorToken.Text, Name [ "__sectionArg" ])))
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
            this.ParseParameterFromTokens(List.ofSeq innerTokens)

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
                let body = this.ParseStandaloneExpression(bodyTokens)
                cases.Add(
                    { Pattern = pattern
                      Guard = guard
                      Body = body }
                )
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

        match lineTokens with
        | varToken :: nameToken :: equalsToken :: rest
            when Token.isKeyword Keyword.Var varToken && Token.isName nameToken && equalsToken.Kind = Equals ->
            let name = SyntaxFacts.trimIdentifierQuotes nameToken.Text
            DoVar(name, this.ParseStandaloneExpression(rest))
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
                DoUsing(this.ParsePatternFromTokens patternTokens, this.ParseStandaloneExpression(expressionTokens))
            | None ->
                diagnostics.AddError(DiagnosticCode.ParseError, "Expected '<-' in the using binding.", source.GetLocation(usingToken.Span))
                DoExpression(Literal LiteralValue.Unit)
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
                { Pattern = NamePattern(SyntaxFacts.trimIdentifierQuotes nameToken.Text)
                  Quantity = None }

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

        let body = this.ParseExpression(0)
        Lambda(parameters, body)

    member private this.ParseInterpolatedString() =
        let startToken = this.Advance()
        let prefix = SyntaxFacts.trimIdentifierQuotes startToken.Text
        let parts = ResizeArray<SurfaceInterpolatedStringPart>()
        let mutable closed = false

        while not closed && this.Current.Kind <> EndOfFile do
            match this.Current.Kind with
            | StringTextSegment ->
                let token = this.Advance()
                parts.Add(StringText(this.DecodeStringTextSegment token))
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

    member private this.ParsePrimaryExpression() =
        this.SkipLayout()

        match this.Current.Kind with
        | Keyword Keyword.Match ->
            this.ParseMatchExpression()
        | Keyword Keyword.Do ->
            this.ParseDoExpression()
        | Keyword Keyword.If ->
            this.ParseIfExpression()
        | Backslash ->
            this.ParseLambdaExpression()
        | IntegerLiteral ->
            let token = this.Advance()

            match Int64.TryParse(token.Text) with
            | true, value -> Literal(Integer value)
            | _ ->
                diagnostics.AddError(DiagnosticCode.ParseError, $"Invalid integer literal '{token.Text}'.", source.GetLocation(token.Span))
                Literal(Integer 0L)
        | FloatLiteral ->
            let token = this.Advance()

            match Double.TryParse(token.Text) with
            | true, value -> Literal(Float value)
            | _ ->
                diagnostics.AddError(DiagnosticCode.ParseError, $"Invalid float literal '{token.Text}'.", source.GetLocation(token.Span))
                Literal(Float 0.0)
        | StringLiteral ->
            let token = this.Advance()
            Literal(LiteralValue.String(this.DecodeStringLiteral token))
        | InterpolatedStringStart ->
            this.ParseInterpolatedString()
        | CharacterLiteral ->
            let token = this.Advance()
            Literal(Character(this.DecodeCharacterLiteral token))
        | LeftParen ->
            let innerTokens = this.CollectParenthesizedTokens()

            match innerTokens with
            | [] ->
                Literal Unit
            | _ ->
                match this.TryParseOperatorSection innerTokens with
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
        | Operator ->
            if this.Current.Text = "!" then
                this.Advance() |> ignore
                // Splice should bind tightly to the next atomic/application form without
                // swallowing surrounding infix operators in the enclosing expression.
                MonadicSplice(this.ParseApplicationExpression())
            elif this.Current.Text = "~" then
                this.Advance() |> ignore
                InoutArgument(this.ParseApplicationExpression())
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
        | Operator ->
            if token.Text = "~" then
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

    member private this.ParseApplicationExpression() =
        let head = this.ParsePrefixExpression()
        let arguments = ResizeArray<SurfaceExpression>()
        let mutable keepReadingArguments = true

        while keepReadingArguments do
            this.SkipLayout()

            if this.CanStartApplicationArgumentAfterHead(this.Current) then
                arguments.Add(this.ParsePrefixExpression())
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

            let isImplicit, remaining =
                match trimmed with
                | { Kind = AtSign } :: rest -> true, rest
                | _ -> false, trimmed

            match remaining with
            | nameToken :: equalsToken :: valueTokens when Token.isName nameToken && equalsToken.Kind = Equals ->
                { Name = SyntaxFacts.trimIdentifierQuotes nameToken.Text
                  IsImplicit = isImplicit
                  Value = this.ParseStandaloneExpression(valueTokens) }
            | _ ->
                diagnostics.AddError(DiagnosticCode.ParseError, "Expected a record update field of the form 'name = expr'.", source.GetLocation(this.Current.Span))
                { Name = "<missing>"
                  IsImplicit = isImplicit
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
            else
                keepReadingPostfix <- false

        expression

    member private this.ParseExpression(minimumPrecedence: int) =
        let mutable left = this.ParseApplicationExpression()
        let mutable keepParsing = true

        while keepParsing do
            this.SkipLayout()

            match this.Current.Kind with
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

    let parseLetHeader (source: SourceText) (diagnostics: DiagnosticBag) (tokens: Token list) =
        let parameterTokens, returnTypeTokens = splitReturnTypeTokens tokens
        let tokenArray = List.toArray parameterTokens
        let parameters = ResizeArray<Parameter>()
        let mutable index = 0

        while index < tokenArray.Length do
            match tokenArray[index].Kind with
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
        match tryParseIndentedLocalLetSequence fixities source diagnostics tokens with
        | Some expression ->
            Some expression
        | None ->
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
            match remainingTokens with
            | { Kind = Indent } :: rest ->
                let lines = ResizeArray<Token list>()
                let current = ResizeArray<Token>()
                let mutable depth = 1
                let mutable finished = false

                for token in rest do
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

                if not finished then
                    None
                else
                    Some(lines |> Seq.toList |> List.filter (List.isEmpty >> not))
            | { Kind = Newline } :: { Kind = Indent } :: rest ->
                let lines = ResizeArray<Token list>()
                let current = ResizeArray<Token>()
                let mutable depth = 1
                let mutable finished = false

                for token in rest do
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
            lineTokens
            |> List.filter (fun token ->
                match token.Kind with
                | Newline
                | Indent
                | Dedent -> false
                | _ -> true)

        let parseSimpleLocalBindingLine lineTokens =
            trimLineTokens lineTokens |> parseBindingLineTokens

        let parseSimpleLocalLetLine lineTokens =
            match trimLineTokens lineTokens with
            | letToken :: rest when Token.isKeyword Keyword.Let letToken ->
                parseBindingLineTokens rest
            | _ ->
                None

        let buildNestedExpression parseBindingLine bindingLines bodyTokens =
            let body : SurfaceExpression =
                parseExpression fixities source diagnostics bodyTokens
                |> Option.defaultValue (Literal LiteralValue.Unit)

            bindingLines
            |> List.rev
            |> List.fold (fun (current: SurfaceExpression) lineTokens ->
                match parseBindingLine lineTokens with
                | Some(binding, value) -> LocalLet(binding, value, current)
                | None -> current) body

        let tryParseIndentedPureBlockSuite () =
            match splitIndentedLines tokens with
            | Some lines
                when List.length lines >= 2
                     && (lines |> List.take (List.length lines - 1) |> List.forall (parseSimpleLocalLetLine >> Option.isSome)) ->
                Some(buildNestedExpression parseSimpleLocalLetLine (lines |> List.take (List.length lines - 1)) (List.last lines))
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

        match tryParseIndentedPureBlockSuite () with
        | Some expression ->
            Some expression
        | None ->
            match trimmedLeading with
            | letToken :: rest when Token.isKeyword Keyword.Let letToken ->
                tryParseLetInExpression rest
            | _ ->
                None
