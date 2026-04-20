namespace Kappa.Compiler

open System

type LetHeaderParseResult =
    { Parameters: Parameter list
      ReturnTypeTokens: Token list option }

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
            diagnostics.AddError("Expected a pattern name.", source.GetLocation(this.Current.Span))

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
                diagnostics.AddError($"Invalid integer literal '{token.Text}'.", source.GetLocation(token.Span))
                LiteralPattern(LiteralValue.Integer 0L)
        | FloatLiteral ->
            match Double.TryParse(token.Text) with
            | true, value -> LiteralPattern(LiteralValue.Float value)
            | _ ->
                diagnostics.AddError($"Invalid float literal '{token.Text}'.", source.GetLocation(token.Span))
                LiteralPattern(LiteralValue.Float 0.0)
        | StringLiteral ->
            match SyntaxFacts.tryDecodeStringLiteral token.Text with
            | Result.Ok value -> LiteralPattern(LiteralValue.String value)
            | Result.Error message ->
                diagnostics.AddError(message, source.GetLocation(token.Span))
                LiteralPattern(LiteralValue.String(SyntaxFacts.trimStringQuotes token.Text))
        | CharacterLiteral ->
            match SyntaxFacts.tryDecodeCharacterLiteral token.Text with
            | Result.Ok value -> LiteralPattern(LiteralValue.Character value)
            | Result.Error message ->
                diagnostics.AddError(message, source.GetLocation(token.Span))
                LiteralPattern(LiteralValue.Character '\000')
        | _ ->
            diagnostics.AddError("Expected a literal pattern.", source.GetLocation(token.Span))
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
            diagnostics.AddError("Expected ')' to close the pattern.", source.GetLocation(start.Span))

        List.ofSeq innerTokens

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
            diagnostics.AddError("Expected a pattern.", source.GetLocation(this.Current.Span))
            WildcardPattern

    member private this.ParseApplicationPattern() =
        let head = this.ParseAtomicPattern()
        let arguments = ResizeArray<CorePattern>()

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
            diagnostics.AddError("Only constructor patterns may take arguments.", source.GetLocation(this.Current.Span))
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
                diagnostics.AddError("Unexpected tokens at the end of the pattern.", source.GetLocation(this.Current.Span))

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
            diagnostics.AddError(message, source.GetLocation(this.Current.Span))
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
            diagnostics.AddError(message, source.GetLocation(token.Span))
            SyntaxFacts.trimStringQuotes token.Text

    member private this.DecodeCharacterLiteral(token: Token) =
        match SyntaxFacts.tryDecodeCharacterLiteral token.Text with
        | Result.Ok value ->
            value
        | Result.Error message ->
            diagnostics.AddError(message, source.GetLocation(token.Span))
            '\000'

    member private this.DecodeStringTextSegment(token: Token) =
        match SyntaxFacts.tryUnescapeStringContent token.Text with
        | Result.Ok value ->
            value
        | Result.Error message ->
            diagnostics.AddError(message, source.GetLocation(token.Span))
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

    member private _.MakeOperatorSection body =
        let parameterName = "__sectionArg"

        Lambda(
            [ { Name = parameterName
                TypeTokens = None } ],
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
            diagnostics.AddError("Expected ')' to close the expression.", source.GetLocation(start.Span))

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
            diagnostics.AddError("Expected a name.", source.GetLocation(this.Current.Span))

        List.ofSeq segments

    member private this.ParseParameterFromTokens(tokens: Token list) =
        match tokens with
        | [] ->
            diagnostics.AddError("Expected a parameter binder.", source.GetLocation(eofSpan))
            None
        | head :: tail when Token.isName head ->
            let name = SyntaxFacts.trimIdentifierQuotes head.Text

            let typeTokens =
                match tail with
                | [] -> None
                | colon :: rest when colon.Kind = Colon -> Some rest
                | _ ->
                    diagnostics.AddError("Unsupported parameter binder syntax.", source.GetLocation(head.Span))
                    None

            Some
                { Name = name
                  TypeTokens = typeTokens }
        | head :: _ ->
            diagnostics.AddError("Expected a parameter name.", source.GetLocation(head.Span))
            None

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
            diagnostics.AddError("Unterminated parameter binder.", source.GetLocation(start.Span))
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
            | _ when this.IsNameToken(this.Current) ->
                parameters.Add(
                    { Name = SyntaxFacts.trimIdentifierQuotes (this.Advance().Text)
                      TypeTokens = None }
                )
            | _ ->
                diagnostics.AddError("Expected a lambda parameter or '->'.", source.GetLocation(this.Current.Span))
                keepReading <- false

        if parameters.Count = 0 then
            diagnostics.AddError("A lambda must declare at least one parameter.", source.GetLocation(this.Current.Span))

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
                diagnostics.AddError("Expected the do block to dedent.", source.GetLocation(this.Current.Span))
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
            diagnostics.AddError("Expected '->' in the case clause.", source.GetLocation(this.Current.Span))
            List.ofSeq tokens

    member private this.ParseMatchExpression() =
        this.ExpectKeyword(Keyword.Match, "Expected 'match'.") |> ignore
        let scrutineeTokens = ResizeArray<Token>()

        while this.Current.Kind <> Newline && this.Current.Kind <> EndOfFile do
            scrutineeTokens.Add(this.Advance())

        let scrutinee = this.ParseStandaloneExpression(List.ofSeq scrutineeTokens)

        if this.Current.Kind = Newline then
            this.Advance() |> ignore

        let cases = ResizeArray<MatchCase>()

        while Token.isKeyword Keyword.Case this.Current do
            this.Advance() |> ignore

            let lineTokens = ResizeArray<Token>()

            while this.Current.Kind <> Newline && this.Current.Kind <> EndOfFile do
                lineTokens.Add(this.Advance())

            if this.Current.Kind = Newline then
                this.Advance() |> ignore

            let tokenArray = lineTokens |> Seq.toArray
            let splitIndex = tokenArray |> Array.tryFindIndex (fun token -> token.Kind = Arrow)

            match splitIndex with
            | Some index ->
                let patternTokens = tokenArray[0 .. index - 1] |> Array.toList
                let bodyTokens = tokenArray[index + 1 ..] |> Array.toList
                let pattern = this.ParsePatternFromTokens(patternTokens)
                let body = this.ParseStandaloneExpression(bodyTokens)
                cases.Add({ Pattern = pattern; Body = body })
            | None ->
                diagnostics.AddError("Expected '->' in the case clause.", source.GetLocation(this.Current.Span))

        if cases.Count = 0 then
            diagnostics.AddError("A match expression must declare at least one case.", source.GetLocation(this.Current.Span))

        Match(scrutinee, List.ofSeq cases)

    member private this.ParseDoLine(lineTokens: Token list) =
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
                    diagnostics.AddError("Expected the while body to dedent.", source.GetLocation((current ()).Span))
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

        match lineTokens with
        | varToken :: nameToken :: equalsToken :: rest
            when Token.isKeyword Keyword.Var varToken && Token.isName nameToken && equalsToken.Kind = Equals ->
            let name = SyntaxFacts.trimIdentifierQuotes nameToken.Text
            DoVar(name, this.ParseStandaloneExpression(rest))
        | letToken :: nameToken :: rest when Token.isKeyword Keyword.Let letToken && Token.isName nameToken ->
            let mutable splitIndex = -1
            let tokenArray = List.toArray rest
            let mutable index = 0

            while index < tokenArray.Length && splitIndex = -1 do
                if tokenArray[index].Kind = Equals then
                    splitIndex <- index

                index <- index + 1

            if splitIndex < 0 then
                diagnostics.AddError("Expected '=' in the do binding.", source.GetLocation(nameToken.Span))
                DoExpression(Literal LiteralValue.Unit)
            else
                let name = SyntaxFacts.trimIdentifierQuotes nameToken.Text
                let expressionTokens = List.ofArray tokenArray[splitIndex + 1 ..]
                DoLet(name, this.ParseStandaloneExpression(expressionTokens))
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
                diagnostics.AddError("Expected 'do' in the while statement.", source.GetLocation(whileToken.Span))
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
            let name = SyntaxFacts.trimIdentifierQuotes nameToken.Text
            DoBind(name, this.ParseStandaloneExpression(rest))
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
            diagnostics.AddError("A do block must contain at least one statement.", source.GetLocation(this.Current.Span))
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
            diagnostics.AddError("Expected '->' after the lambda parameters.", source.GetLocation(this.Current.Span))

        let body = this.ParseExpression(0)
        Lambda(parameters, body)

    member private this.ParseInterpolatedString() =
        let startToken = this.Advance()
        let prefix = SyntaxFacts.trimIdentifierQuotes startToken.Text
        let parts = ResizeArray<InterpolatedStringPart>()
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
                    diagnostics.AddError(
                        "Expected the interpolation to end before the string resumes.",
                        source.GetLocation(this.Current.Span)
                    )
            | InterpolatedStringEnd ->
                this.Advance() |> ignore
                closed <- true
            | _ ->
                diagnostics.AddError("Expected interpolated string content.", source.GetLocation(this.Current.Span))
                closed <- true

        if not closed then
            diagnostics.AddError("Unterminated interpolated string.", source.GetLocation(startToken.Span))

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
                diagnostics.AddError($"Invalid integer literal '{token.Text}'.", source.GetLocation(token.Span))
                Literal(Integer 0L)
        | FloatLiteral ->
            let token = this.Advance()

            match Double.TryParse(token.Text) with
            | true, value -> Literal(Float value)
            | _ ->
                diagnostics.AddError($"Invalid float literal '{token.Text}'.", source.GetLocation(token.Span))
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
                        diagnostics.AddError("Expected an expression inside parentheses.", source.GetLocation(this.Current.Span))
                        Literal Unit
        | _ when this.IsNameToken(this.Current) ->
            let segments = this.ParseQualifiedName()
            Name segments
        | _ ->
            diagnostics.AddError("Expected an expression.", source.GetLocation(this.Current.Span))
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
        let arguments = ResizeArray<CoreExpression>()
        let mutable keepReading = true

        while keepReading do
            this.SkipLayout()

            if this.CanStartApplicationArgumentAfterHead(this.Current) then
                arguments.Add(this.ParsePrefixExpression())
            else
                keepReading <- false

        if arguments.Count = 0 then
            head
        else
            Apply(head, List.ofSeq arguments)

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
                diagnostics.AddError("Unexpected tokens at the end of the expression.", source.GetLocation(this.Current.Span))

            Some expression

module CoreParsing =
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
                    { Name = SyntaxFacts.trimIdentifierQuotes tokenArray[index].Text
                      TypeTokens = None }
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
                    diagnostics.AddError("Unterminated parameter binder in function header.", source.GetLocation(startToken.Span))
                    index <- tokenArray.Length
                else
                    let innerTokens = List.ofArray tokenArray[index + 1 .. endIndex - 2]

                    match innerTokens with
                    | [] ->
                        diagnostics.AddError("Expected a parameter binder.", source.GetLocation(startToken.Span))
                    | head :: tail when Token.isName head ->
                        let name = SyntaxFacts.trimIdentifierQuotes head.Text

                        let typeTokens =
                            match tail with
                            | [] -> None
                            | colon :: rest when colon.Kind = Colon -> Some rest
                            | _ ->
                                diagnostics.AddError("Unsupported parameter binder syntax.", source.GetLocation(head.Span))
                                None

                        parameters.Add(
                            { Name = name
                              TypeTokens = typeTokens }
                        )
                    | head :: _ ->
                        diagnostics.AddError("Expected a parameter name.", source.GetLocation(head.Span))

                    index <- endIndex
            | _ ->
                diagnostics.AddError("Unsupported function header syntax.", source.GetLocation(tokenArray[index].Span))
                index <- index + 1

        { Parameters = List.ofSeq parameters
          ReturnTypeTokens = returnTypeTokens }

    let parseExpression (fixities: FixityTable) (source: SourceText) (diagnostics: DiagnosticBag) (tokens: Token list) =
        let parser = ExpressionParser(tokens, source, diagnostics, fixities)
        parser.Parse()
