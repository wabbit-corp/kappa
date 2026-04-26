namespace Kappa.Compiler

open System
open System.Collections.Generic

type ParseResult =
    { Syntax: CompilationUnit
      Diagnostics: Diagnostic list }

type private ModifierState =
    { Visibility: Visibility option
      IsOpaque: bool
      TotalityAssertion: TotalityAssertion option }

type private TokenParser
    (
        tokens: Token list,
        source: SourceText,
        initialFixities: FixityTable,
        resolveImportedFixities: ImportSpec list -> FixityDeclaration list
    ) =
    let tokenArray = List.toArray tokens
    let diagnostics = DiagnosticBag()
    let mutable fixities = initialFixities
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
               | Dedent -> true
               | _ -> false) do
            this.Advance() |> ignore

    member private this.TryConsume(kind: TokenKind) =
        if this.Current.Kind = kind then
            Some(this.Advance())
        else
            None

    member private this.TryConsumeKeyword(keyword: Keyword) =
        if Token.isKeyword keyword this.Current then
            Some(this.Advance())
        else
            None

    member private this.Expect(kind: TokenKind, message: string) =
        if this.Current.Kind = kind then
            this.Advance()
        else
            diagnostics.AddError(DiagnosticCode.ParseError, message, source.GetLocation(this.Current.Span))
            { Kind = kind
              Text = ""
              Span = this.Current.Span }

    member private this.ExpectKeyword(keyword: Keyword, message: string) =
        if Token.isKeyword keyword this.Current then
            this.Advance()
        else
            diagnostics.AddError(DiagnosticCode.ParseError, message, source.GetLocation(this.Current.Span))
            { Kind = Keyword keyword
              Text = Keyword.toText keyword
              Span = this.Current.Span }

    member private this.ConsumeName(message: string) =
        if Token.isName this.Current then
            let token = this.Advance()
            SyntaxFacts.trimIdentifierQuotes token.Text
        else
            diagnostics.AddError(DiagnosticCode.ParseError, message, source.GetLocation(this.Current.Span))
            "<missing>"

    member private this.TryConsumeOperatorName() =
        let isSymbolToken token =
            match token.Kind with
            | Operator
            | Colon
            | Equals -> true
            | _ -> false

        if this.Current.Kind <> LeftParen then
            None
        else
            let mutable offset = 1
            let mutable parts = ResizeArray<string>()
            let mutable keepReading = true
            let mutable sawRightParen = false

            while keepReading do
                match this.Peek(offset).Kind with
                | RightParen when parts.Count > 0 ->
                    sawRightParen <- true
                    keepReading <- false
                | _ when isSymbolToken (this.Peek(offset)) ->
                    parts.Add(this.Peek(offset).Text)
                    offset <- offset + 1
                | _ ->
                    keepReading <- false

            if sawRightParen then
                this.Advance() |> ignore

                for _ in 1 .. parts.Count do
                    this.Advance() |> ignore

                this.Advance() |> ignore
                Some(String.Concat(parts))
            else
                None

    member private this.ConsumeTermBindingName(message: string) =
        if Token.isName this.Current then
            this.ConsumeName(message)
        else
            match this.TryConsumeOperatorName() with
            | Some operatorName -> operatorName
            | None ->
                diagnostics.AddError(DiagnosticCode.ParseError, message, source.GetLocation(this.Current.Span))
                "<missing>"

    member private this.ConsumeTypeBindingName(message: string) =
        if Token.isName this.Current then
            this.ConsumeName(message)
        else
            match this.TryConsumeOperatorName() with
            | Some operatorName -> operatorName
            | None ->
                diagnostics.AddError(DiagnosticCode.ParseError, message, source.GetLocation(this.Current.Span))
                "<missing>"

    member private this.TryConsumeTermBindingName() =
        if Token.isName this.Current then
            Some(this.ConsumeName("Expected a binding name."))
        else
            this.TryConsumeOperatorName()

    member private _.ParseConstructorNameAndArity(lineTokens: Token list) =
        let significantTokens =
            lineTokens
            |> List.filter (fun token ->
                match token.Kind with
                | Newline
                | Indent
                | Dedent -> false
                | _ -> true)

        let constructorTokens =
            match significantTokens with
            | { Kind = Operator; Text = "|" } :: rest -> rest
            | tokens -> tokens

        let tryParseSymbolicConstructorName tokens =
            let isSymbolToken token =
                match token.Kind with
                | Operator
                | Colon
                | Equals -> true
                | _ -> false

            match tokens with
            | { Kind = LeftParen } :: rest ->
                let rec collect acc remaining =
                    match remaining with
                    | { Kind = RightParen } :: tail when not (List.isEmpty acc) ->
                        Some(String.Concat(List.rev acc), tail)
                    | token :: tail when isSymbolToken token ->
                        collect (token.Text :: acc) tail
                    | _ ->
                        None

                collect [] rest
            | _ ->
                None

        let name, argumentTokens =
            match tryParseSymbolicConstructorName constructorTokens with
            | Some(name, rest) ->
                name, rest
            | None ->
                match constructorTokens with
                | head :: rest when Token.isName head ->
                    SyntaxFacts.trimIdentifierQuotes head.Text, rest
                | _ ->
                    "<anonymous>", []

        let tokenArray = List.toArray argumentTokens
        let mutable arity = 0
        let mutable index = 0

        while index < tokenArray.Length do
            match tokenArray[index].Kind with
            | LeftParen ->
                arity <- arity + 1
                let mutable depth = 1
                index <- index + 1

                while index < tokenArray.Length && depth > 0 do
                    match tokenArray[index].Kind with
                    | LeftParen -> depth <- depth + 1
                    | RightParen -> depth <- depth - 1
                    | _ -> ()

                    index <- index + 1
            | RightParen ->
                index <- index + 1
            | _ ->
                arity <- arity + 1
                index <- index + 1

        name, arity

    member private _.SignificantTokens(tokens: Token list) =
        tokens
        |> List.filter (fun token ->
            match token.Kind with
            | Newline
            | Indent
            | Dedent
            | EndOfFile -> false
            | _ -> true)

    member private this.ValidateTypeAliasBody(tokens: Token list) =
        let significantTokens = this.SignificantTokens tokens

        let hasInvalidTopLevelToken =
            let tokenArray = significantTokens |> List.toArray
            let mutable parenDepth = 0
            let mutable braceDepth = 0
            let mutable bracketDepth = 0
            let mutable setBraceDepth = 0
            let mutable foundInvalid = None
            let mutable index = 0

            while index < tokenArray.Length && foundInvalid.IsNone do
                match tokenArray[index].Kind with
                | LeftParen -> parenDepth <- parenDepth + 1
                | RightParen -> parenDepth <- max 0 (parenDepth - 1)
                | LeftBrace -> braceDepth <- braceDepth + 1
                | RightBrace -> braceDepth <- max 0 (braceDepth - 1)
                | LeftBracket -> bracketDepth <- bracketDepth + 1
                | RightBracket -> bracketDepth <- max 0 (bracketDepth - 1)
                | LeftEffectRow -> bracketDepth <- bracketDepth + 1
                | RightEffectRow -> bracketDepth <- max 0 (bracketDepth - 1)
                | LeftSetBrace -> setBraceDepth <- setBraceDepth + 1
                | RightSetBrace -> setBraceDepth <- max 0 (setBraceDepth - 1)
                | Backslash when parenDepth = 0 && braceDepth = 0 && bracketDepth = 0 && setBraceDepth = 0 ->
                    foundInvalid <- Some tokenArray[index]
                | Operator when parenDepth = 0 && braceDepth = 0 && bracketDepth = 0 && setBraceDepth = 0 && String.Equals(tokenArray[index].Text, "\\", StringComparison.Ordinal) ->
                    foundInvalid <- Some tokenArray[index]
                | Keyword Keyword.As when parenDepth = 0 && braceDepth = 0 && bracketDepth = 0 && setBraceDepth = 0 ->
                    foundInvalid <- Some tokenArray[index]
                | _ ->
                    ()

                index <- index + 1

            foundInvalid

        match significantTokens, hasInvalidTopLevelToken with
        | [], _ ->
            diagnostics.AddError(DiagnosticCode.ParseError, "Expected a type alias body.", source.GetLocation(this.Current.Span))
        | _, Some token ->
            diagnostics.AddError(DiagnosticCode.ParseError, "Expected a valid type alias body.", source.GetLocation(token.Span))
        | _ ->
            ()

    member private this.ValidateTraitMember(lineTokens: Token list) =
        let significantTokens = this.SignificantTokens lineTokens

        let isValidSignature tokens =
            match tokens with
            | head :: colon :: schemeTokens when Token.isName head && colon.Kind = Colon ->
                TypeSignatures.parseScheme schemeTokens |> Option.isSome
            | { Kind = LeftParen } :: { Kind = Operator } :: { Kind = RightParen } :: colon :: schemeTokens when colon.Kind = Colon ->
                TypeSignatures.parseScheme schemeTokens |> Option.isSome
            | _ ->
                false

        match significantTokens with
        | [] ->
            ()
        | head :: _ when Token.isKeyword Keyword.Let head ->
            ()
        | _ when isValidSignature significantTokens ->
            ()
        | head :: _ ->
            diagnostics.AddError(
                DiagnosticCode.ParseError,
                "Expected a trait member signature or a default member definition.",
                source.GetLocation(head.Span)
            )

    member private this.ValidateSignatureType(tokens: Token list) =
        let significantTokens = this.SignificantTokens tokens

        let findInvalidToken () =
            let tokenArray = significantTokens |> List.toArray
            let mutable parenDepth = 0
            let mutable braceDepth = 0
            let mutable bracketDepth = 0
            let mutable setBraceDepth = 0
            let mutable invalidToken = None
            let mutable index = 0

            while index < tokenArray.Length && invalidToken.IsNone do
                match tokenArray[index].Kind with
                | LeftParen ->
                    parenDepth <- parenDepth + 1
                | RightParen ->
                    if parenDepth = 0 then
                        invalidToken <- Some tokenArray[index]
                    else
                        parenDepth <- parenDepth - 1
                | LeftBrace ->
                    braceDepth <- braceDepth + 1
                | RightBrace ->
                    if braceDepth = 0 then
                        invalidToken <- Some tokenArray[index]
                    else
                        braceDepth <- braceDepth - 1
                | LeftBracket
                | LeftEffectRow ->
                    bracketDepth <- bracketDepth + 1
                | RightBracket
                | RightEffectRow ->
                    if bracketDepth = 0 then
                        invalidToken <- Some tokenArray[index]
                    else
                        bracketDepth <- bracketDepth - 1
                | LeftSetBrace ->
                    setBraceDepth <- setBraceDepth + 1
                | RightSetBrace ->
                    if setBraceDepth = 0 then
                        invalidToken <- Some tokenArray[index]
                    else
                        setBraceDepth <- setBraceDepth - 1
                | _ ->
                    ()

                index <- index + 1

            if invalidToken.IsSome then
                invalidToken
            elif parenDepth > 0 || braceDepth > 0 || bracketDepth > 0 || setBraceDepth > 0 then
                significantTokens |> List.tryLast
            else
                None

        match significantTokens, findInvalidToken () with
        | [], _ ->
            diagnostics.AddError(DiagnosticCode.ParseError, "Expected a signature type.", source.GetLocation(this.Current.Span))
        | _, Some token ->
            diagnostics.AddError(DiagnosticCode.ParseError, "Expected a valid signature type.", source.GetLocation(token.Span))
        | _ ->
            ()

    member private _.SplitTopLevelCommaGroups(tokens: Token list) =
        let groups = ResizeArray<Token list>()
        let tokenArray = tokens |> List.toArray
        let mutable parenDepth = 0
        let mutable braceDepth = 0
        let mutable bracketDepth = 0
        let mutable setBraceDepth = 0
        let mutable groupStart = 0

        for index = 0 to tokenArray.Length - 1 do
            match tokenArray[index].Kind with
            | LeftParen -> parenDepth <- parenDepth + 1
            | RightParen -> parenDepth <- max 0 (parenDepth - 1)
            | LeftBrace -> braceDepth <- braceDepth + 1
            | RightBrace -> braceDepth <- max 0 (braceDepth - 1)
            | LeftBracket -> bracketDepth <- bracketDepth + 1
            | RightBracket -> bracketDepth <- max 0 (bracketDepth - 1)
            | LeftEffectRow -> bracketDepth <- bracketDepth + 1
            | RightEffectRow -> bracketDepth <- max 0 (bracketDepth - 1)
            | LeftSetBrace -> setBraceDepth <- setBraceDepth + 1
            | RightSetBrace -> setBraceDepth <- max 0 (setBraceDepth - 1)
            | Comma when parenDepth = 0 && braceDepth = 0 && bracketDepth = 0 && setBraceDepth = 0 ->
                groups.Add(tokenArray[groupStart .. index - 1] |> Array.toList)
                groupStart <- index + 1
            | _ ->
                ()

        if groupStart <= tokenArray.Length then
            groups.Add(tokenArray[groupStart ..] |> Array.toList)

        groups
        |> Seq.toList
        |> List.filter (List.isEmpty >> not)

    member private _.TryFindLastTopLevelEquals(tokens: Token list) =
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
            | LeftEffectRow -> bracketDepth <- bracketDepth + 1
            | RightEffectRow -> bracketDepth <- max 0 (bracketDepth - 1)
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

        splitIndex

    member private this.ParseRecordStyleConstructorParameter(tokens: Token list) =
        let significantTokens =
            tokens
            |> List.filter (fun token ->
                match token.Kind with
                | Newline
                | Indent
                | Dedent
                | EndOfFile -> false
                | _ -> true)

        let declarationTokens, defaultTokens =
            match this.TryFindLastTopLevelEquals(significantTokens) with
            | Some splitIndex ->
                let tokenArray = significantTokens |> List.toArray
                tokenArray[0 .. splitIndex - 1] |> Array.toList,
                Some(tokenArray[splitIndex + 1 ..] |> Array.toList)
            | None ->
                significantTokens, None

        CoreParsing.parseParameterLayout source diagnostics declarationTokens
        |> Option.map (fun parameter ->
            let defaultValue =
                defaultTokens |> Option.bind (CoreParsing.parseExpression fixities source diagnostics)

            ({ ParameterName = Some parameter.Name
               ParameterTypeTokens = parameter.TypeTokens |> Option.defaultValue []
               ParameterQuantity = parameter.Quantity
               ParameterIsImplicit = parameter.IsImplicit
               DefaultValue = defaultValue }
             : DataConstructorParameter))

    member private this.ParseConstructor(lineTokens: Token list) =
        let constructorName, fallbackArity =
            this.ParseConstructorNameAndArity(lineTokens)

        let significantTokens =
            lineTokens
            |> List.filter (fun token ->
                match token.Kind with
                | Newline
                | Indent
                | Dedent
                | EndOfFile -> false
                | _ -> true)

        let constructorTokens =
            match significantTokens with
            | { Kind = Operator; Text = "|" } :: rest -> rest
            | tokens -> tokens

        match constructorTokens with
        | head :: _ when String.Equals(constructorName, "<anonymous>", StringComparison.Ordinal) ->
            diagnostics.AddError(
                DiagnosticCode.ParseError,
                "Expected a constructor name.",
                source.GetLocation(head.Span)
            )
        | _ ->
            ()

        let constructorBodyTokens =
            let isSymbolToken token =
                match token.Kind with
                | Operator
                | Colon
                | Equals -> true
                | _ -> false

            match constructorTokens with
            | { Kind = LeftParen } :: rest ->
                let rec stripSymbolicName remaining =
                    match remaining with
                    | { Kind = RightParen } :: tail ->
                        Some tail
                    | token :: tail when isSymbolToken token ->
                        stripSymbolicName tail
                    | _ ->
                        None

                stripSymbolicName rest |> Option.defaultValue constructorTokens
            | _nameToken :: rest ->
                rest
            | [] ->
                []

        let parameterMetadata =
            match constructorBodyTokens with
            | { Kind = LeftBrace } :: rest when not (List.isEmpty rest) && (List.last rest).Kind = RightBrace ->
                let innerTokens = rest |> List.take (List.length rest - 1)

                this.SplitTopLevelCommaGroups(innerTokens)
                |> List.map this.ParseRecordStyleConstructorParameter
                |> fun parameters ->
                    if parameters |> List.forall Option.isSome then
                        parameters |> List.choose id |> Some
                    else
                        None
            | _ ->
                None

        let signatureArity =
            match constructorBodyTokens with
            | { Kind = Colon } :: typeTokens ->
                let tokenArray = typeTokens |> List.toArray
                let mutable parenDepth = 0
                let mutable braceDepth = 0
                let mutable bracketDepth = 0
                let mutable arity = 0

                for index = 0 to tokenArray.Length - 1 do
                    match tokenArray[index].Kind with
                    | LeftParen -> parenDepth <- parenDepth + 1
                    | RightParen -> parenDepth <- max 0 (parenDepth - 1)
                    | LeftBrace -> braceDepth <- braceDepth + 1
                    | RightBrace -> braceDepth <- max 0 (braceDepth - 1)
                    | LeftBracket -> bracketDepth <- bracketDepth + 1
                    | RightBracket -> bracketDepth <- max 0 (bracketDepth - 1)
                    | LeftEffectRow -> bracketDepth <- bracketDepth + 1
                    | RightEffectRow -> bracketDepth <- max 0 (bracketDepth - 1)
                    | Arrow when parenDepth = 0 && braceDepth = 0 && bracketDepth = 0 ->
                        arity <- arity + 1
                    | _ ->
                        ()

                Some arity
            | _ ->
                None

        { Name = constructorName
          Tokens = lineTokens
          Arity =
            parameterMetadata
            |> Option.map List.length
            |> Option.orElse signatureArity
            |> Option.defaultValue fallbackArity
          Parameters = parameterMetadata }

    member private this.IsSignatureStart() =
        (this.Current.Kind = Identifier || (match this.Current.Kind with | Keyword _ -> true | _ -> false))
        && this.Peek(1).Kind = Colon
        || (this.Current.Kind = LeftParen && this.Peek(1).Kind = Operator && this.Peek(2).Kind = RightParen && this.Peek(3).Kind = Colon)

    member private this.IsProbableTopLevelStart(token: Token, nextToken: Token option) =
        match token.Kind with
        | Keyword Keyword.Import
        | Keyword Keyword.Export
        | Keyword Keyword.Expect
        | Keyword Keyword.Projection
        | Keyword Keyword.Let
        | Keyword Keyword.Data
        | Keyword Keyword.Type
        | Keyword Keyword.Trait
        | Keyword Keyword.Instance
        | Keyword Keyword.Infix
        | Keyword Keyword.Prefix
        | Keyword Keyword.Postfix
        | Keyword Keyword.Public
        | Keyword Keyword.Private
        | Keyword Keyword.AssertReducible
        | Keyword Keyword.AssertTerminates
        | Keyword Keyword.AssertTotal
        | Keyword Keyword.Opaque -> true
        | Identifier when String.Equals(token.Text, "pattern", StringComparison.Ordinal) -> true
        | Identifier
        | Keyword _ ->
            match nextToken with
               | Some next -> next.Kind = Colon
               | None -> false
        | _ -> false

    member private this.NextNonLayout(offset: int) =
        let mutable currentOffset = offset
        let mutable candidate = this.Peek(currentOffset)

        while (match candidate.Kind with
               | Newline
               | Indent
               | Dedent -> true
               | _ -> false) do
            currentOffset <- currentOffset + 1
            candidate <- this.Peek(currentOffset)

        candidate

    member private this.CollectUntilTopLevelBoundary() =
        let collected = ResizeArray<Token>()
        let mutable localIndents = 0
        let mutable keepCollecting = true

        let isLayoutToken (token: Token) =
            match token.Kind with
            | Newline
            | Indent
            | Dedent -> true
            | _ -> false

        let lastSignificantToken () =
            collected
            |> Seq.toList
            |> List.rev
            |> List.tryFind (isLayoutToken >> not)

        let isNameToken (token: Token) =
            Token.isName token
            && not (Token.isKeyword Keyword.If token)
            && not (Token.isKeyword Keyword.Is token)
            && not (Token.isKeyword Keyword.Then token)
            && not (Token.isKeyword Keyword.Else token)
            && not (Token.isKeyword Keyword.In token)

        let isExpressionStartToken (token: Token) =
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
            | Operator when token.Text = "'" -> true
            | Operator when token.Text = "$" -> true
            | Operator when FixityTable.tryFindPrefix token.Text fixities |> Option.isSome -> true
            | Keyword Keyword.If
            | Keyword Keyword.Seal -> true
            | _ -> isNameToken token

        let shouldContinueAcrossNewline nextToken =
            match lastSignificantToken (), nextToken with
            | None, Some followingToken when isExpressionStartToken followingToken ->
                true
            | Some lastToken, Some followingToken when isExpressionStartToken followingToken ->
                lastToken.Kind = Equals
                || lastToken.Kind = Arrow
                || lastToken.Kind = Colon
                || lastToken.Kind = Operator
                || Token.isKeyword Keyword.In lastToken
                || Token.isKeyword Keyword.Then lastToken
                || Token.isKeyword Keyword.Elif lastToken
                || Token.isKeyword Keyword.Else lastToken
                || Token.isKeyword Keyword.Do lastToken
                || Token.isKeyword Keyword.Match lastToken
                || Token.isKeyword Keyword.Try lastToken
                || Token.isKeyword Keyword.Case lastToken
                || Token.isKeyword Keyword.Except lastToken
                || Token.isKeyword Keyword.Finally lastToken
                || Token.isKeyword Keyword.If lastToken
                || Token.isKeyword Keyword.Yield lastToken
                || Token.isKeyword Keyword.By lastToken
            | _ ->
                false

        while keepCollecting && this.Current.Kind <> EndOfFile do
            match this.Current.Kind with
            | Newline when localIndents = 0 ->
                let next = this.NextNonLayout(1)
                let afterNext = this.NextNonLayout(2)

                if this.Peek(1).Kind = Indent && shouldContinueAcrossNewline (Some next) then
                    collected.Add(this.Advance())
                elif this.IsProbableTopLevelStart(next, Some afterNext) then
                    keepCollecting <- false
                else
                    collected.Add(this.Advance())
            | Indent ->
                localIndents <- localIndents + 1
                collected.Add(this.Advance())
            | Dedent when localIndents > 0 ->
                localIndents <- localIndents - 1
                collected.Add(this.Advance())

                if localIndents = 0 then
                    let next = this.NextNonLayout(0)
                    let afterNext = this.NextNonLayout(1)

                    if this.IsProbableTopLevelStart(next, Some afterNext) then
                        keepCollecting <- false
            | _ ->
                collected.Add(this.Advance())

        collected
        |> Seq.toList
        |> List.rev
        |> List.skipWhile (fun token -> token.Kind = Newline)
        |> List.rev

    member private this.ParseDottedName() =
        let segments = ResizeArray<string>()
        segments.Add(this.ConsumeName("Expected a module name."))

        while this.Current.Kind = Dot && Token.isName (this.Peek(1)) do
            this.Advance() |> ignore
            segments.Add(this.ConsumeName("Expected a module name segment."))

        List.ofSeq segments

    member private this.ParseNameList() =
        let items = ResizeArray<string>()
        this.Expect(LeftParen, "Expected '(' to start the list.") |> ignore

        while this.Current.Kind <> RightParen && this.Current.Kind <> EndOfFile do
            let startPosition = position
            items.Add(this.ConsumeTermBindingName("Expected a name in the list."))

            if this.TryConsume(Comma).IsNone && this.Current.Kind <> RightParen then
                diagnostics.AddError(DiagnosticCode.ParseError, "Expected ',' between list items.", source.GetLocation(this.Current.Span))

            if position = startPosition && this.Current.Kind <> RightParen && this.Current.Kind <> EndOfFile then
                this.Advance() |> ignore

        this.Expect(RightParen, "Expected ')' to close the list.") |> ignore
        List.ofSeq items

    member private this.ParseImportItem() =
        let modifiers = ResizeArray<ImportItemModifier>()
        let seenModifiers = HashSet<ImportItemModifier>()

        let addModifier modifier keywordText =
            if not (seenModifiers.Add(modifier)) then
                diagnostics.AddError(
                    DiagnosticCode.ParseError,
                    $"Duplicate '{keywordText}' modifier in import item.",
                    source.GetLocation(this.Current.Span)
                )

            modifiers.Add(modifier)

        let rec consumeModifiers () =
            match this.Current.Kind with
            | Keyword Keyword.Unhide ->
                addModifier Unhide "unhide"
                this.Advance() |> ignore
                consumeModifiers ()
            | Keyword Keyword.Clarify ->
                addModifier Clarify "clarify"
                this.Advance() |> ignore
                consumeModifiers ()
            | _ -> ()

        consumeModifiers ()

        let importNamespace =
            match this.Current.Kind with
            | Keyword Keyword.Term ->
                this.Advance() |> ignore
                Some ImportNamespace.Term
            | Keyword Keyword.Type ->
                this.Advance() |> ignore
                Some ImportNamespace.Type
            | Keyword Keyword.Trait ->
                this.Advance() |> ignore
                Some ImportNamespace.Trait
            | Keyword Keyword.Ctor ->
                this.Advance() |> ignore
                Some ImportNamespace.Constructor
            | _ ->
                None

        let name = this.ConsumeTermBindingName("Expected an imported name.")

        let includeConstructors =
            match this.Current.Kind, this.Peek(1).Kind, this.Peek(2).Kind, this.Peek(3).Kind with
            | LeftParen, Dot, Dot, RightParen ->
                this.Advance() |> ignore
                this.Advance() |> ignore
                this.Advance() |> ignore
                this.Advance() |> ignore
                true
            | _ ->
                false

        let alias =
            if this.TryConsumeKeyword(Keyword.As).IsSome then
                if includeConstructors then
                    diagnostics.AddError(
                        DiagnosticCode.ParseError,
                        "ctorAll may not be combined with an alias.",
                        source.GetLocation(this.Current.Span)
                    )

                Some(this.ConsumeName("Expected an alias after 'as'."))
            else
                None

        { Modifiers = List.ofSeq modifiers
          Namespace = importNamespace
          Name = name
          IncludeConstructors = includeConstructors
          Alias = alias }

    member private this.ParseExceptItem() =
        let importNamespace =
            match this.Current.Kind with
            | Keyword Keyword.Term ->
                this.Advance() |> ignore
                Some ImportNamespace.Term
            | Keyword Keyword.Type ->
                this.Advance() |> ignore
                Some ImportNamespace.Type
            | Keyword Keyword.Trait ->
                this.Advance() |> ignore
                Some ImportNamespace.Trait
            | Keyword Keyword.Ctor ->
                this.Advance() |> ignore
                Some ImportNamespace.Constructor
            | _ ->
                None

        let name = this.ConsumeTermBindingName("Expected an excluded import name.")

        { Namespace = importNamespace
          Name = name }

    member private this.ParseExceptItems() =
        let items = ResizeArray<ExceptItem>()
        this.Expect(LeftParen, "Expected '(' to start the exclusion list.") |> ignore

        while this.Current.Kind <> RightParen && this.Current.Kind <> EndOfFile do
            let startPosition = position
            items.Add(this.ParseExceptItem())

            if this.TryConsume(Comma).IsNone && this.Current.Kind <> RightParen then
                diagnostics.AddError(DiagnosticCode.ParseError, "Expected ',' between excluded import items.", source.GetLocation(this.Current.Span))

            if position = startPosition && this.Current.Kind <> RightParen && this.Current.Kind <> EndOfFile then
                this.Advance() |> ignore

        this.Expect(RightParen, "Expected ')' to close the exclusion list.") |> ignore
        List.ofSeq items

    member private this.ParseImportItems() =
        let items = ResizeArray<ImportItem>()
        this.Expect(LeftParen, "Expected '(' to start the import item list.") |> ignore

        while this.Current.Kind <> RightParen && this.Current.Kind <> EndOfFile do
            let startPosition = position
            items.Add(this.ParseImportItem())

            if this.TryConsume(Comma).IsNone && this.Current.Kind <> RightParen then
                diagnostics.AddError(DiagnosticCode.ParseError, "Expected ',' between import items.", source.GetLocation(this.Current.Span))

            if position = startPosition && this.Current.Kind <> RightParen && this.Current.Kind <> EndOfFile then
                this.Advance() |> ignore

        this.Expect(RightParen, "Expected ')' to close the import item list.") |> ignore
        List.ofSeq items

    member private this.ParseImportSpec() =
        let moduleSource =
            match this.Current.Kind with
            | StringLiteral ->
                let token = this.Advance()

                let value =
                    match SyntaxFacts.tryDecodeStringLiteral token.Text with
                    | Result.Ok decoded -> decoded
                    | Result.Error message ->
                        diagnostics.AddError(DiagnosticCode.ParseError, message, source.GetLocation(token.Span))
                        SyntaxFacts.trimStringQuotes token.Text

                match SyntaxFacts.tryParseUrlModuleSpecifier value with
                | Result.Ok specifier ->
                    Url specifier
                | Result.Error message ->
                    diagnostics.AddError(DiagnosticCode.ParseError, message, source.GetLocation(token.Span))
                    Url
                        { OriginalText = value
                          BaseUrl = value
                          Pin = None }
            | _ ->
                Dotted(this.ParseDottedName())

        if this.TryConsumeKeyword(Keyword.As).IsSome then
            { Source = moduleSource
              Alias = Some(this.ConsumeName("Expected an alias after 'as'."))
              Selection = QualifiedOnly }
        elif this.TryConsume(Dot).IsSome then
            match this.Current.Kind with
            | Operator when this.Current.Text = "*" ->
                this.Advance() |> ignore

                let selection =
                    if this.TryConsumeKeyword(Keyword.Except).IsSome then
                        AllExcept(this.ParseExceptItems())
                    else
                        All

                { Source = moduleSource
                  Alias = None
                  Selection = selection }
            | LeftParen ->
                { Source = moduleSource
                  Alias = None
                  Selection = Items(this.ParseImportItems()) }
            | _ ->
                match this.TryConsumeTermBindingName() with
                | Some itemName ->
                    { Source = moduleSource
                      Alias = None
                      Selection =
                        Items
                            [
                                { Modifiers = []
                                  Namespace = None
                                  Name = itemName
                                  IncludeConstructors = false
                                  Alias = None }
                            ] }
                | None ->
                    diagnostics.AddError(DiagnosticCode.ParseError, "Expected '*', '(...)', or a singleton item after '.'.", source.GetLocation(this.Current.Span))

                    { Source = moduleSource
                      Alias = None
                      Selection = QualifiedOnly }
        else
            { Source = moduleSource
              Alias = None
              Selection = QualifiedOnly }

    member private this.ParseModifiers() =
        let mutable visibility = None
        let mutable isOpaque = false
        let mutable totalityAssertion = None
        let mutable keepGoing = true

        while keepGoing do
            match this.Current.Kind with
            | Keyword Keyword.Public ->
                visibility <- Some Visibility.Public
                this.Advance() |> ignore
            | Keyword Keyword.Private ->
                visibility <- Some Visibility.Private
                this.Advance() |> ignore
            | Keyword Keyword.Opaque ->
                isOpaque <- true
                this.Advance() |> ignore
            | Keyword Keyword.AssertTerminates
            | Keyword Keyword.AssertTotal ->
                match totalityAssertion with
                | Some _ ->
                    diagnostics.AddError(DiagnosticCode.ParseError, "Multiple totality assertions cannot be applied to the same declaration.", source.GetLocation(this.Current.Span))
                | None ->
                    totalityAssertion <- Some AssertTerminatesAssertion

                this.Advance() |> ignore
            | Keyword Keyword.AssertReducible ->
                match totalityAssertion with
                | Some _ ->
                    diagnostics.AddError(DiagnosticCode.ParseError, "Multiple totality assertions cannot be applied to the same declaration.", source.GetLocation(this.Current.Span))
                | None ->
                    totalityAssertion <- Some AssertReducibleAssertion

                this.Advance() |> ignore
            | _ ->
                keepGoing <- false

        { Visibility = visibility
          IsOpaque = isOpaque
          TotalityAssertion = totalityAssertion }

    member private this.ParseSignature(modifiers: ModifierState) =
        let name = this.ConsumeTermBindingName("Expected a name in the signature declaration.")
        this.Expect(Colon, "Expected ':' in the signature declaration.") |> ignore
        let typeTokens = this.CollectUntilTopLevelBoundary()
        this.ValidateSignatureType(typeTokens)

        SignatureDeclaration
            { Visibility = modifiers.Visibility
              IsOpaque = modifiers.IsOpaque
              Name = name
              TypeTokens = typeTokens }

    member private this.ParseExpectDeclaration() =
        this.ExpectKeyword(Keyword.Expect, "Expected 'expect'.") |> ignore

        match this.Current.Kind with
        | Keyword Keyword.Type ->
            this.Advance() |> ignore
            let nameSpan = this.Current.Span
            let name = this.ConsumeTypeBindingName("Expected a type name after 'expect type'.")

            ExpectDeclarationNode
                (ExpectTypeDeclaration
                    { Name = name
                      HeaderTokens = this.CollectUntilTopLevelBoundary()
                      Span = nameSpan })
        | Keyword Keyword.Trait ->
            this.Advance() |> ignore
            let nameSpan = this.Current.Span
            let name = this.ConsumeName("Expected a trait name after 'expect trait'.")

            ExpectDeclarationNode
                (ExpectTraitDeclaration
                    { Name = name
                      HeaderTokens = this.CollectUntilTopLevelBoundary()
                      Span = nameSpan })
        | Keyword Keyword.Term ->
            this.Advance() |> ignore

            let currentSpan = this.Current.Span
            let name = this.ConsumeTermBindingName("Expected a term name after 'expect term'.")
            this.Expect(Colon, "Expected ':' in the expect term declaration.") |> ignore

            ExpectDeclarationNode
                (ExpectTermDeclaration
                    { Name = name
                      TypeTokens = this.CollectUntilTopLevelBoundary()
                      Span = currentSpan })
        | _ ->
            diagnostics.AddError(DiagnosticCode.ParseError, 
                "Expected 'type', 'trait', or 'term' after 'expect'.",
                source.GetLocation(this.Current.Span)
            )

            ExpectDeclarationNode
                (ExpectTypeDeclaration
                    { Name = "<missing>"
                      HeaderTokens = []
                      Span = this.Current.Span })

    member private this.ParseFixityDeclaration() =
        let kind =
            match this.Current.Kind with
            | Keyword Keyword.Infix ->
                this.Advance() |> ignore

                let associativity =
                    match this.Current.Kind with
                    | Keyword Keyword.Left ->
                        this.Advance() |> ignore
                        LeftAssociative
                    | Keyword Keyword.Right ->
                        this.Advance() |> ignore
                        RightAssociative
                    | _ ->
                        NonAssociative

                Infix associativity
            | Keyword Keyword.Prefix ->
                this.Advance() |> ignore
                Prefix
            | Keyword Keyword.Postfix ->
                this.Advance() |> ignore
                Postfix
            | _ ->
                diagnostics.AddError(DiagnosticCode.ParseError, "Expected a fixity declaration.", source.GetLocation(this.Current.Span))
                Infix NonAssociative

        let precedenceToken =
            this.Expect(IntegerLiteral, "Expected a numeric precedence in the fixity declaration.")

        let precedence =
            match SyntaxFacts.tryParseNumericLiteral precedenceToken.Text with
            | Result.Ok { Literal = SurfaceIntegerLiteral(value, _, None) }
                when value >= System.Numerics.BigInteger(Int32.MinValue)
                     && value <= System.Numerics.BigInteger(Int32.MaxValue) ->
                int value
            | _ ->
                diagnostics.AddError(DiagnosticCode.ParseError, "Expected a valid integer precedence in the fixity declaration.", source.GetLocation(precedenceToken.Span))
                0

        this.Expect(LeftParen, "Expected '(' before the operator token in the fixity declaration.") |> ignore

        let operatorName =
            if this.Current.Kind = Operator then
                this.Advance().Text
            else
                diagnostics.AddError(DiagnosticCode.ParseError, "Expected an operator token in the fixity declaration.", source.GetLocation(this.Current.Span))
                "<missing-operator>"

        this.Expect(RightParen, "Expected ')' after the operator token in the fixity declaration.") |> ignore

        let declaration =
            { Fixity = kind
              Precedence = precedence
              OperatorName = operatorName }

        fixities <- FixityTable.add declaration fixities
        FixityDeclarationNode declaration

    member private this.ParseLetDeclaration(modifiers: ModifierState) =
        this.ExpectKeyword(Keyword.Let, "Expected 'let'.") |> ignore
        this.ParseFunctionLikeDeclaration(modifiers, false, "let")

    member private this.ParsePatternDeclaration(modifiers: ModifierState) =
        this.Advance() |> ignore
        this.ParseFunctionLikeDeclaration(modifiers, true, "pattern")

    member private this.ParseFunctionLikeDeclaration(modifiers: ModifierState, isPattern: bool, keywordText: string) =
        let headerTokens = ResizeArray<Token>()

        let name = this.TryConsumeTermBindingName()

        while (this.Current.Kind <> Equals
               && this.Current.Kind <> Newline
               && this.Current.Kind <> EndOfFile) do
            headerTokens.Add(this.Advance())

        let bodyTokens =
            if this.TryConsume(Equals).IsSome then
                this.CollectUntilTopLevelBoundary()
            else
                diagnostics.AddError(DiagnosticCode.ParseError, $"Expected '=' in the {keywordText} declaration.", source.GetLocation(this.Current.Span))
                []

        let parsedHeader = CoreParsing.parseLetHeader source diagnostics (List.ofSeq headerTokens)
        let parsedBody = CoreParsing.parseExpression fixities source diagnostics bodyTokens

        LetDeclaration
            { Visibility = modifiers.Visibility
              IsOpaque = modifiers.IsOpaque
              TotalityAssertion = modifiers.TotalityAssertion
              IsPattern = isPattern
              Name = name
              Parameters = parsedHeader.Parameters
              HeaderTokens = List.ofSeq headerTokens
              ReturnTypeTokens = parsedHeader.ReturnTypeTokens
              BodyTokens = bodyTokens
              Body = parsedBody }

    member private this.ParseProjectionDeclaration(modifiers: ModifierState) =
        this.ExpectKeyword(Keyword.Projection, "Expected 'projection'.") |> ignore

        if modifiers.IsOpaque then
            diagnostics.AddError(DiagnosticCode.ParseError, "Opacity modifiers do not apply to projection declarations.", source.GetLocation(this.Current.Span))

        let name = this.ConsumeName("Expected a projection name.")
        let headerTokens = ResizeArray<Token>()

        while (this.Current.Kind <> Equals
               && this.Current.Kind <> Newline
               && this.Current.Kind <> EndOfFile) do
            headerTokens.Add(this.Advance())

        let bodyTokens =
            if this.TryConsume(Equals).IsSome then
                this.CollectUntilTopLevelBoundary()
            else
                diagnostics.AddError(DiagnosticCode.ParseError, "Expected '=' in the projection declaration.", source.GetLocation(this.Current.Span))
                []

        let parsedHeader = CoreParsing.parseProjectionHeader source diagnostics (List.ofSeq headerTokens)
        let parsedBody = CoreParsing.parseProjectionBody fixities source diagnostics bodyTokens

        ProjectionDeclarationNode
            { Visibility = modifiers.Visibility
              Name = name
              Binders = parsedHeader.Binders
              ReturnTypeTokens = parsedHeader.ReturnTypeTokens
              BodyTokens = bodyTokens
              Body = parsedBody }

    member private this.ParseIndentedLines() =
        let lines = ResizeArray<Token list>()

        if this.TryConsume(Newline).IsSome then
            match this.TryConsume(Indent) with
            | Some _ ->
                let currentLine = ResizeArray<Token>()
                let mutable nestedIndents = 0
                let mutable parenDepth = 0
                let mutable braceDepth = 0
                let mutable bracketDepth = 0
                let mutable setBraceDepth = 0

                let flushLine () =
                    if currentLine.Count > 0 then
                        lines.Add(List.ofSeq currentLine)
                        currentLine.Clear()

                let lineContinuesIntoIndentedBody () =
                    if currentLine.Count = 0 then
                        false
                    else
                        let lastToken = currentLine[currentLine.Count - 1]
                        lastToken.Kind = Equals

                while not (this.Current.Kind = Dedent && nestedIndents = 0) && this.Current.Kind <> EndOfFile do
                    match this.Current.Kind with
                    | Newline
                        when nestedIndents = 0
                             && parenDepth = 0
                             && braceDepth = 0
                             && bracketDepth = 0
                             && setBraceDepth = 0 ->
                        if lineContinuesIntoIndentedBody () then
                            currentLine.Add(this.Advance())
                        else
                            flushLine ()
                            this.Advance() |> ignore
                    | Indent ->
                        nestedIndents <- nestedIndents + 1
                        currentLine.Add(this.Advance())
                    | Dedent when nestedIndents > 0 ->
                        nestedIndents <- nestedIndents - 1
                        currentLine.Add(this.Advance())
                    | _ ->
                        match this.Current.Kind with
                        | LeftParen -> parenDepth <- parenDepth + 1
                        | RightParen -> parenDepth <- max 0 (parenDepth - 1)
                        | LeftBrace -> braceDepth <- braceDepth + 1
                        | RightBrace -> braceDepth <- max 0 (braceDepth - 1)
                        | LeftBracket -> bracketDepth <- bracketDepth + 1
                        | RightBracket -> bracketDepth <- max 0 (bracketDepth - 1)
                        | LeftEffectRow -> bracketDepth <- bracketDepth + 1
                        | RightEffectRow -> bracketDepth <- max 0 (bracketDepth - 1)
                        | LeftSetBrace -> setBraceDepth <- setBraceDepth + 1
                        | RightSetBrace -> setBraceDepth <- max 0 (setBraceDepth - 1)
                        | _ -> ()

                        currentLine.Add(this.Advance())

                flushLine ()

                if parenDepth > 0 then
                    diagnostics.AddError(DiagnosticCode.ParseError, "Expected ')' to close the declaration block item.", source.GetLocation(this.Current.Span))

                if braceDepth > 0 then
                    diagnostics.AddError(DiagnosticCode.ParseError, "Expected '}' to close the declaration block item.", source.GetLocation(this.Current.Span))

                if bracketDepth > 0 then
                    diagnostics.AddError(DiagnosticCode.ParseError, "Expected ']' to close the declaration block item.", source.GetLocation(this.Current.Span))

                if setBraceDepth > 0 then
                    diagnostics.AddError(DiagnosticCode.ParseError, "Expected '>}' to close the declaration block item.", source.GetLocation(this.Current.Span))

                this.Expect(Dedent, "Expected the declaration block to dedent.") |> ignore
            | None ->
                diagnostics.AddError(DiagnosticCode.ParseError, "Expected an indented block.", source.GetLocation(this.Current.Span))
        else
            let inlineBody = this.CollectUntilTopLevelBoundary()

            if not (List.isEmpty inlineBody) then
                lines.Add(inlineBody)

        List.ofSeq lines

    member private this.ParseDataDeclaration(modifiers: ModifierState) =
        this.ExpectKeyword(Keyword.Data, "Expected 'data'.") |> ignore
        let name = this.ConsumeTypeBindingName("Expected a data type name.")

        let headerTokens = ResizeArray<Token>()

        while (this.Current.Kind <> Equals
               && this.Current.Kind <> Newline
               && this.Current.Kind <> EndOfFile) do
            headerTokens.Add(this.Advance())

        let constructors: DataConstructor list =
            if this.TryConsume(Equals).IsSome then
                this.ParseIndentedLines()
                |> List.map this.ParseConstructor
            else
                []

        DataDeclarationNode
            { Visibility = modifiers.Visibility
              IsOpaque = modifiers.IsOpaque
              Name = name
              HeaderTokens = List.ofSeq headerTokens
              Constructors = constructors }

    member private this.ParseTypeAlias(modifiers: ModifierState) =
        this.ExpectKeyword(Keyword.Type, "Expected 'type'.") |> ignore
        let name = this.ConsumeTypeBindingName("Expected a type alias name.")

        let headerTokens = ResizeArray<Token>()

        while (this.Current.Kind <> Equals
               && this.Current.Kind <> Newline
               && this.Current.Kind <> EndOfFile) do
            headerTokens.Add(this.Advance())

        let bodyTokens =
            if this.TryConsume(Equals).IsSome then
                let parsedBody = this.CollectUntilTopLevelBoundary()
                this.ValidateTypeAliasBody(parsedBody)
                Some parsedBody
            else
                None

        TypeAliasNode
            { Visibility = modifiers.Visibility
              IsOpaque = modifiers.IsOpaque
              TotalityAssertion = None
              Name = name
              HeaderTokens = List.ofSeq headerTokens
              BodyTokens = bodyTokens }

    member private this.ParseTraitDeclaration(modifiers: ModifierState) =
        this.ExpectKeyword(Keyword.Trait, "Expected 'trait'.") |> ignore

        let fullHeaderTokens = ResizeArray<Token>()

        let isConstraintArrowEquals () =
            this.Current.Kind = Equals
            && this.Peek(1).Kind = Operator
            && String.Equals(this.Peek(1).Text, ">", StringComparison.Ordinal)

        while ((this.Current.Kind <> Equals || isConstraintArrowEquals ())
               && this.Current.Kind <> Newline
               && this.Current.Kind <> EndOfFile) do
            fullHeaderTokens.Add(this.Advance())

        let fullHeaderTokens = List.ofSeq fullHeaderTokens

        let splitTraitHeader (tokens: Token list) =
            let tokenArray = tokens |> List.toArray
            let mutable depth = 0
            let mutable headStart = 0
            let mutable index = 0

            while index < tokenArray.Length do
                match tokenArray[index].Kind with
                | LeftParen ->
                    depth <- depth + 1
                | RightParen ->
                    depth <- max 0 (depth - 1)
                | Operator when depth = 0 && String.Equals(tokenArray[index].Text, "=>", StringComparison.Ordinal) ->
                    headStart <- index + 1
                | Equals
                    when depth = 0
                         && index + 1 < tokenArray.Length
                         && tokenArray[index + 1].Kind = Operator
                         && String.Equals(tokenArray[index + 1].Text, ">", StringComparison.Ordinal) ->
                    headStart <- index + 2
                    index <- index + 1
                | _ ->
                    ()

                index <- index + 1

            let headTokens =
                if headStart < tokenArray.Length then
                    tokenArray[headStart..] |> Array.toList
                else
                    []

            let rec collectQualifiedName acc remaining =
                match remaining with
                | token :: dotToken :: rest when Token.isName token && dotToken.Kind = Dot ->
                    collectQualifiedName (SyntaxFacts.trimIdentifierQuotes token.Text :: acc) rest
                | token :: _ when Token.isName token ->
                    List.rev (SyntaxFacts.trimIdentifierQuotes token.Text :: acc)
                | _ ->
                    []

            let rec stripQualifiedName remaining =
                match remaining with
                | token :: dotToken :: rest when Token.isName token && dotToken.Kind = Dot ->
                    stripQualifiedName rest
                | token :: rest when Token.isName token ->
                    rest
                | _ ->
                    remaining

            let traitName, traitHeadTokens =
                match collectQualifiedName [] headTokens with
                | [] ->
                    None, headTokens
                | [ singleName ] ->
                    Some singleName, stripQualifiedName headTokens
                | segments ->
                    Some(SyntaxFacts.moduleNameToText segments), stripQualifiedName headTokens

            traitName, traitHeadTokens

        let tryMemberNameFromTokens (tokens: Token list) =
            match tokens with
            | head :: _ when Token.isName head ->
                Some(SyntaxFacts.trimIdentifierQuotes head.Text)
            | { Kind = LeftParen } :: { Kind = Operator; Text = operatorName } :: { Kind = RightParen } :: _ ->
                Some operatorName
            | _ ->
                None

        let nameOption, headTokens = splitTraitHeader fullHeaderTokens
        let name = nameOption |> Option.defaultValue ""

        if String.IsNullOrWhiteSpace name then
            diagnostics.AddError(DiagnosticCode.ParseError, "Expected a trait head after 'trait'.", source.GetLocation(this.Current.Span))

        let members: TraitMember list =
            if this.TryConsume(Equals).IsSome then
                this.ParseIndentedLines()
                |> List.map (fun lineTokens ->
                    this.ValidateTraitMember(lineTokens)

                    let defaultDefinition =
                        match lineTokens with
                        | head :: _ when Token.isKeyword Keyword.Let head ->
                            this.ParseInstanceMember lineTokens
                        | _ ->
                            None

                    let memberName =
                        match lineTokens with
                        | _ when defaultDefinition.IsSome ->
                            defaultDefinition.Value.Name
                        | _ ->
                            tryMemberNameFromTokens (this.SignificantTokens lineTokens)

                    ({ Name = memberName
                       DefaultDefinition = defaultDefinition
                       Tokens = lineTokens }: TraitMember))
            else
                []

        TraitDeclarationNode
            { Visibility = modifiers.Visibility
              Name = name
              FullHeaderTokens = fullHeaderTokens
              HeaderTokens = headTokens
              Members = members }

    member private _.IsContextualEffectToken(token: Token) =
        token.Kind = Identifier
        && String.Equals(SyntaxFacts.trimIdentifierQuotes token.Text, "effect", StringComparison.Ordinal)

    member private _.TryParseEffectQuantityPrefix(tokens: Token list) =
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

    member private this.ParseEffectOperation(lineTokens: Token list) =
        let significantTokens = this.SignificantTokens lineTokens
        let quantity, remainingTokens =
            match this.TryParseEffectQuantityPrefix significantTokens with
            | Some(quantity, rest) -> Some quantity, rest
            | None -> None, significantTokens

        match remainingTokens with
        | nameToken :: colonToken :: signatureTokens when Token.isName nameToken && colonToken.Kind = Colon ->
            { Name = SyntaxFacts.trimIdentifierQuotes nameToken.Text
              ResumptionQuantity = quantity
              SignatureTokens = signatureTokens }
        | nameToken :: _ ->
            diagnostics.AddError(
                DiagnosticCode.ParseError,
                "Expected an effect operation signature of the form 'op : ...'.",
                source.GetLocation(nameToken.Span)
            )

            { Name = "<missing>"
              ResumptionQuantity = quantity
              SignatureTokens = [] }
        | [] ->
            diagnostics.AddError(
                DiagnosticCode.ParseError,
                "Expected an effect operation signature.",
                source.GetLocation(this.Current.Span)
            )

            { Name = "<missing>"
              ResumptionQuantity = quantity
              SignatureTokens = [] }

    member private this.ParseEffectDeclaration(modifiers: ModifierState) =
        this.Advance() |> ignore
        let name = this.ConsumeName("Expected an effect name.")
        let headerTokens = ResizeArray<Token>()

        while this.Current.Kind <> Equals
              && this.Current.Kind <> Newline
              && this.Current.Kind <> EndOfFile do
            headerTokens.Add(this.Advance())

        let operations =
            if this.TryConsume(Equals).IsSome then
                this.ParseIndentedLines() |> List.map this.ParseEffectOperation
            else
                diagnostics.AddError(DiagnosticCode.ParseError, "Expected '=' in the effect declaration.", source.GetLocation(this.Current.Span))
                []

        EffectDeclarationNode
            { Visibility = modifiers.Visibility
              Name = name
              HeaderTokens = List.ofSeq headerTokens
              Operations = operations }

    member private this.ParseInstanceMember(lineTokens: Token list) : LetDefinition option =
        match lineTokens with
        | letToken :: rest when Token.isKeyword Keyword.Let letToken ->
            let memberTokens = ResizeArray<Token>(rest)
            let headerTokens = ResizeArray<Token>()

            let mutable position = 0

            let current () =
                if position < memberTokens.Count then
                    memberTokens[position]
                else
                    { Kind = EndOfFile
                      Text = ""
                      Span = letToken.Span }

            let advance () =
                let token = current ()

                if position < memberTokens.Count then
                    position <- position + 1

                token

            let tryConsume kind =
                if (current ()).Kind = kind then
                    Some(advance ())
                else
                    None

            let tryConsumeTermBindingName () =
                let token = current ()

                if Token.isName token then
                    advance () |> ignore
                    Some(SyntaxFacts.trimIdentifierQuotes token.Text)
                elif token.Kind = LeftParen && position + 2 < memberTokens.Count && memberTokens[position + 1].Kind = Operator && memberTokens[position + 2].Kind = RightParen then
                    advance () |> ignore
                    let operatorToken = advance ()
                    advance () |> ignore
                    Some operatorToken.Text
                else
                    None

            let name = tryConsumeTermBindingName ()

            while (current ()).Kind <> Equals
                   && (current ()).Kind <> Newline
                   && (current ()).Kind <> EndOfFile do
                headerTokens.Add(advance ())

            let bodyTokens =
                if tryConsume Equals |> Option.isSome then
                    [ while (current ()).Kind <> EndOfFile do
                          yield advance () ]
                else
                    diagnostics.AddError(DiagnosticCode.ParseError, "Expected '=' in the instance member declaration.", source.GetLocation((current ()).Span))
                    []

            let parsedHeader = CoreParsing.parseLetHeader source diagnostics (List.ofSeq headerTokens)
            let parsedBody = CoreParsing.parseExpression fixities source diagnostics bodyTokens

            Some
                { Visibility = None
                  IsOpaque = false
                  TotalityAssertion = None
                  IsPattern = false
                  Name = name
                  Parameters = parsedHeader.Parameters
                  HeaderTokens = List.ofSeq headerTokens
                  ReturnTypeTokens = parsedHeader.ReturnTypeTokens
                  BodyTokens = bodyTokens
                  Body = parsedBody }
        | _ ->
            diagnostics.AddError(DiagnosticCode.ParseError, "Expected an instance member definition starting with 'let'.", source.GetLocation(this.Current.Span))
            None

    member private this.ParseInstanceDeclaration() =
        this.ExpectKeyword(Keyword.Instance, "Expected 'instance'.") |> ignore

        let headerTokens = ResizeArray<Token>()

        let isConstraintArrowEquals () =
            this.Current.Kind = Equals
            && this.Peek(1).Kind = Operator
            && String.Equals(this.Peek(1).Text, ">", StringComparison.Ordinal)

        while ((this.Current.Kind <> Equals || isConstraintArrowEquals ())
               && this.Current.Kind <> Newline
               && this.Current.Kind <> EndOfFile) do
            headerTokens.Add(this.Advance())

        let headerTokens = List.ofSeq headerTokens

        let splitInstanceHeader (tokens: Token list) =
            let tokenArray = tokens |> List.toArray
            let mutable depth = 0
            let mutable headStart = 0
            let mutable index = 0

            while index < tokenArray.Length do
                match tokenArray[index].Kind with
                | LeftParen ->
                    depth <- depth + 1
                | RightParen ->
                    depth <- max 0 (depth - 1)
                | Operator when depth = 0 && String.Equals(tokenArray[index].Text, "=>", StringComparison.Ordinal) ->
                    headStart <- index + 1
                | Equals
                    when depth = 0
                         && index + 1 < tokenArray.Length
                         && tokenArray[index + 1].Kind = Operator
                         && String.Equals(tokenArray[index + 1].Text, ">", StringComparison.Ordinal) ->
                    headStart <- index + 2
                    index <- index + 1
                | _ ->
                    ()

                index <- index + 1

            let fullTokens = tokenArray |> Array.toList

            let headTokens =
                if headStart < tokenArray.Length then
                    tokenArray[headStart..] |> Array.toList
                else
                    []

            let rec collectQualifiedName acc remaining =
                match remaining with
                | token :: dotToken :: rest when Token.isName token && dotToken.Kind = Dot ->
                    collectQualifiedName (SyntaxFacts.trimIdentifierQuotes token.Text :: acc) rest
                | token :: _ when Token.isName token ->
                    List.rev (SyntaxFacts.trimIdentifierQuotes token.Text :: acc)
                | _ ->
                    []

            let rec stripQualifiedName remaining =
                match remaining with
                | token :: dotToken :: rest when Token.isName token && dotToken.Kind = Dot ->
                    stripQualifiedName rest
                | token :: rest when Token.isName token ->
                    rest
                | _ ->
                    remaining

            let traitName, instanceHeadTokens =
                match collectQualifiedName [] headTokens with
                | [] ->
                    None, headTokens
                | [ singleName ] ->
                    Some singleName, stripQualifiedName headTokens
                | segments ->
                    Some(SyntaxFacts.moduleNameToText segments), stripQualifiedName headTokens

            fullTokens, instanceHeadTokens, traitName

        let fullHeaderTokens, headTokens, traitNameOption =
            splitInstanceHeader headerTokens

        let traitName =
            traitNameOption
            |> Option.defaultValue ""

        if String.IsNullOrWhiteSpace traitName then
            diagnostics.AddError(DiagnosticCode.ParseError, "Expected a valid instance head after 'instance'.", source.GetLocation(this.Current.Span))

        let members =
            if this.TryConsume(Equals).IsSome then
                if this.Current.Kind = Newline && this.Peek(1).Kind <> Indent then
                    this.Advance() |> ignore
                    []
                else
                    this.ParseIndentedLines()
                    |> List.choose this.ParseInstanceMember
            else
                diagnostics.AddError(DiagnosticCode.ParseError, "Expected '=' in the instance declaration.", source.GetLocation(this.Current.Span))
                []

        InstanceDeclarationNode
            { TraitName = traitName
              FullHeaderTokens = fullHeaderTokens
              HeaderTokens = headTokens
              Members = members }

    member private this.ParseImportOrExport(isExport: bool) =
        let keyword = if isExport then Keyword.Export else Keyword.Import
        this.ExpectKeyword(keyword, $"Expected '{Keyword.toText keyword}'.") |> ignore

        let specs = ResizeArray<ImportSpec>()
        specs.Add(this.ParseImportSpec())

        while this.TryConsume(Comma).IsSome do
            specs.Add(this.ParseImportSpec())

        let parsedSpecs = List.ofSeq specs

        for declaration in resolveImportedFixities parsedSpecs do
            fixities <- FixityTable.add declaration fixities

        ImportDeclaration(isExport, parsedSpecs)

    member private this.ParseUnknownDeclaration() =
        diagnostics.AddError(
            DiagnosticCode.ParseError,
            "Expected a top-level declaration.",
            source.GetLocation(this.Current.Span)
        )

        UnknownDeclaration(this.CollectUntilTopLevelBoundary())

    member private this.ParseTopLevelDeclaration() =
        let modifiers = this.ParseModifiers()

        match this.Current.Kind with
        | Keyword Keyword.Import -> this.ParseImportOrExport(false)
        | Keyword Keyword.Export -> this.ParseImportOrExport(true)
        | Keyword Keyword.Expect -> this.ParseExpectDeclaration()
        | Keyword Keyword.Projection ->
            if modifiers.TotalityAssertion.IsSome then
                diagnostics.AddError(DiagnosticCode.ParseError, "Totality assertions do not apply to projection declarations.", source.GetLocation(this.Current.Span))

            this.ParseProjectionDeclaration(modifiers)
        | Keyword Keyword.Infix
        | Keyword Keyword.Prefix
        | Keyword Keyword.Postfix -> this.ParseFixityDeclaration()
        | Keyword Keyword.Let -> this.ParseLetDeclaration(modifiers)
        | Identifier when String.Equals(this.Current.Text, "pattern", StringComparison.Ordinal) -> this.ParsePatternDeclaration(modifiers)
        | Keyword Keyword.Data ->
            if modifiers.TotalityAssertion.IsSome then
                diagnostics.AddError(DiagnosticCode.ParseError, "Totality assertions do not apply to data declarations.", source.GetLocation(this.Current.Span))

            this.ParseDataDeclaration(modifiers)
        | Identifier when this.IsContextualEffectToken(this.Current) ->
            if modifiers.IsOpaque || modifiers.TotalityAssertion.IsSome then
                diagnostics.AddError(DiagnosticCode.ParseError, "Opacity and totality assertions do not apply to effect declarations.", source.GetLocation(this.Current.Span))

            this.ParseEffectDeclaration(modifiers)
        | Keyword Keyword.Type ->
            if modifiers.TotalityAssertion.IsSome then
                diagnostics.AddError(DiagnosticCode.ParseError, "Totality assertions currently apply only to term declarations.", source.GetLocation(this.Current.Span))

            this.ParseTypeAlias(modifiers)
        | Keyword Keyword.Trait ->
            if modifiers.TotalityAssertion.IsSome then
                diagnostics.AddError(DiagnosticCode.ParseError, "Totality assertions do not apply to trait declarations.", source.GetLocation(this.Current.Span))

            this.ParseTraitDeclaration(modifiers)
        | Keyword Keyword.Instance ->
            if modifiers.Visibility.IsSome || modifiers.IsOpaque || modifiers.TotalityAssertion.IsSome then
                diagnostics.AddError(DiagnosticCode.ParseError, "Visibility, opacity, and totality modifiers do not apply to instance declarations.", source.GetLocation(this.Current.Span))

            this.ParseInstanceDeclaration()
        | _ when this.IsSignatureStart() ->
            if modifiers.TotalityAssertion.IsSome then
                diagnostics.AddError(DiagnosticCode.ParseError, "Totality assertions do not apply to signature declarations.", source.GetLocation(this.Current.Span))

            this.ParseSignature(modifiers)
        | _ -> this.ParseUnknownDeclaration()

    member this.ParseCompilationUnit() =
        this.SkipLayout()

        while this.Current.Kind = Indent do
            diagnostics.AddError(
                DiagnosticCode.UnexpectedIndentation,
                "Unexpected indentation.",
                source.GetLocation(this.Current.Span)
            )
            this.Advance() |> ignore
            this.SkipLayout()

        let moduleAttributes = ResizeArray<string>()

        while this.Current.Kind = AtSign do
            this.Advance() |> ignore
            moduleAttributes.Add(this.ConsumeName("Expected a module attribute name after '@'."))
            this.SkipLayout()

        let moduleHeader =
            if Token.isKeyword Keyword.Module this.Current then
                this.Advance() |> ignore
                Some(this.ParseDottedName())
            else
                None

        let declarations = ResizeArray<TopLevelDeclaration>()

        while this.Current.Kind <> EndOfFile do
            this.SkipLayout()

            while this.Current.Kind = Indent do
                diagnostics.AddError(
                    DiagnosticCode.UnexpectedIndentation,
                    "Unexpected indentation.",
                    source.GetLocation(this.Current.Span)
                )
                this.Advance() |> ignore
                this.SkipLayout()

            if this.Current.Kind <> EndOfFile then
                declarations.Add(this.ParseTopLevelDeclaration())

        { ModuleAttributes = List.ofSeq moduleAttributes
          ModuleHeader = moduleHeader
          Declarations = List.ofSeq declarations
          Tokens = List.ofArray tokenArray },
        diagnostics.Items

// Parses tokens into documents and maintains the bootstrap/user fixity environment.
module Parser =
    let parseWithImportedFixityResolver initialFixities resolveImportedFixities source tokens =
        let parser = TokenParser(tokens, source, initialFixities, resolveImportedFixities)
        let syntax, diagnostics = parser.ParseCompilationUnit()

        { Syntax = syntax
          Diagnostics = diagnostics }

    let parseWithInitialFixities initialFixities source tokens =
        parseWithImportedFixityResolver initialFixities (fun _ -> []) source tokens

    let private bundledPreludeBootstrapFixities =
        let splitLeadingFixities declarations =
            let rec loop collected remaining =
                match remaining with
                | FixityDeclarationNode declaration :: rest ->
                    loop (declaration :: collected) rest
                | _ ->
                    List.rev collected, remaining

            loop [] declarations

        lazy
            (let source = SourceText.From(BundledPrelude.virtualPath, BundledPrelude.loadText ())
             let lexed = Lexer.tokenize source

             if not (List.isEmpty lexed.Diagnostics) then
                 let diagnostics =
                     lexed.Diagnostics
                     |> List.map (fun diagnostic -> diagnostic.Message)
                     |> String.concat Environment.NewLine

                 invalidOp $"Bundled prelude failed to lex for bootstrap fixity extraction:{Environment.NewLine}{diagnostics}"

             let parsed = parseWithInitialFixities FixityTable.empty source lexed.Tokens

             if not (List.isEmpty parsed.Diagnostics) then
                 let diagnostics =
                     parsed.Diagnostics
                     |> List.map (fun diagnostic -> diagnostic.Message)
                     |> String.concat Environment.NewLine

                 invalidOp $"Bundled prelude failed to parse for bootstrap fixity extraction:{Environment.NewLine}{diagnostics}"

             let leadingFixities, remainingDeclarations =
                 splitLeadingFixities parsed.Syntax.Declarations

             if List.isEmpty leadingFixities then
                 invalidOp "Bundled prelude must begin with bootstrap fixity declarations."

             if remainingDeclarations |> List.exists (function | FixityDeclarationNode _ -> true | _ -> false) then
                 invalidOp "Bundled prelude bootstrap fixity declarations must appear before all other declarations."

             leadingFixities
             |> List.fold (fun table declaration -> FixityTable.add declaration table) FixityTable.empty)

    let parse source tokens =
        parseWithInitialFixities bundledPreludeBootstrapFixities.Value source tokens

    let bootstrapFixities () =
        bundledPreludeBootstrapFixities.Value
