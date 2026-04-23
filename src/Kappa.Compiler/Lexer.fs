namespace Kappa.Compiler

open System
open System.Collections.Generic

type LexResult =
    { Tokens: Token list
      Diagnostics: Diagnostic list }

// Tokenizes source text, tracks layout, and records lexical diagnostics.
module Lexer =
    let private token kind text startIndex =
        { Kind = kind
          Text = text
          Span =
            { Start = startIndex
              Length = text.Length } }

    let private zeroLengthToken kind index =
        token kind "" index

    let private isLineComment (text: string) index =
        index + 1 < text.Length && text[index] = '-' && text[index + 1] = '-'

    let private isBlockCommentStart (text: string) index =
        index + 1 < text.Length && text[index] = '{' && text[index + 1] = '-'

    let private isBlockCommentEnd (text: string) index =
        index + 1 < text.Length && text[index] = '-' && text[index + 1] = '}'

    let private lineContainsCode (lineText: string) startingBlockCommentDepth =
        let mutable depth = startingBlockCommentDepth
        let mutable index = 0
        let mutable foundCode = false

        while index < lineText.Length && not foundCode do
            if depth > 0 then
                if isBlockCommentStart lineText index then
                    depth <- depth + 1
                    index <- index + 2
                elif isBlockCommentEnd lineText index then
                    depth <- depth - 1
                    index <- index + 2
                else
                    index <- index + 1
            else
                match lineText[index] with
                | ' '
                | '\t' ->
                    index <- index + 1
                | '-' when isLineComment lineText index ->
                    index <- lineText.Length
                | '{' when isBlockCommentStart lineText index ->
                    depth <- depth + 1
                    index <- index + 2
                | _ ->
                    foundCode <- true

        foundCode

    let private countLeadingSpaces (source: SourceText) lineIndex (lineText: string) (diagnostics: DiagnosticBag) =
        let mutable spaces = 0
        let mutable index = 0
        let lineStart = source.LineStarts[lineIndex]

        while index < lineText.Length do
            match lineText[index] with
            | ' ' ->
                spaces <- spaces + 1
                index <- index + 1
            | '\t' ->
                diagnostics.AddError("Tabs are not permitted in indentation.", source.GetLocation(TextSpan.FromBounds(lineStart + index, lineStart + index + 1)))
                index <- index + 1
            | _ ->
                index <- lineText.Length

        spaces

    let private addIndentationTokens
        (source: SourceText)
        lineIndex
        indent
        (indentStack: Stack<int>)
        (tokens: ResizeArray<Token>)
        (diagnostics: DiagnosticBag)
        =
        let lineStart = source.LineStarts[lineIndex]
        let currentIndent = indentStack.Peek()

        if indent > currentIndent then
            indentStack.Push(indent)
            tokens.Add(zeroLengthToken Indent lineStart)
        elif indent < currentIndent then
            let mutable keepPopping = indent < indentStack.Peek()

            while keepPopping do
                indentStack.Pop() |> ignore
                tokens.Add(zeroLengthToken Dedent lineStart)
                keepPopping <- indent < indentStack.Peek()

            if indent <> indentStack.Peek() then
                diagnostics.AddError(
                    $"Unexpected indentation level {indent}; expected one of the previous block levels.",
                    source.GetLocation(TextSpan.FromBounds(lineStart, lineStart + indent))
                )

    let private readBacktickIdentifier (lineText: string) index =
        let mutable endIndex = index + 1
        let mutable closed = false

        while endIndex < lineText.Length && not closed do
            if lineText[endIndex] = '`' then
                closed <- true
                endIndex <- endIndex + 1
            else
                endIndex <- endIndex + 1

        endIndex, closed

    let private readIdentifier (lineText: string) index =
        let mutable endIndex = index + 1

        while endIndex < lineText.Length && SyntaxFacts.isIdentifierPart lineText[endIndex] do
            endIndex <- endIndex + 1

        endIndex

    let private readQuotedLiteral (lineText: string) index quoteCharacter =
        let mutable endIndex = index + 1
        let mutable closed = false

        while endIndex < lineText.Length && not closed do
            if lineText[endIndex] = '\\' then
                endIndex <- min lineText.Length (endIndex + 2)
            elif lineText[endIndex] = quoteCharacter then
                endIndex <- endIndex + 1
                closed <- true
            else
                endIndex <- endIndex + 1

        endIndex, closed

    let private tokenizeLine
        (source: SourceText)
        lineIndex
        (lineText: string)
        startingBlockCommentDepth
        startingDelimiterDepth
        (tokens: ResizeArray<Token>)
        (diagnostics: DiagnosticBag)
        =
        let lineStart = source.LineStarts[lineIndex]
        let mutable blockCommentDepth = startingBlockCommentDepth
        let mutable delimiterDepth = startingDelimiterDepth
        let mutable index = 0
        let mutable emittedCode = false

        let emit kind text startOffset =
            emittedCode <- true
            tokens.Add(token kind text (lineStart + startOffset))

        let emitNameToken text startOffset =
            let kind =
                if text = "_" then
                    Underscore
                else
                    match Keyword.tryParse text with
                    | Some keyword -> Keyword keyword
                    | None -> Identifier

            emit kind text startOffset

        let emitQuotedLiteral kind startOffset quoteCharacter unterminatedMessage =
            let endIndex, closed = readQuotedLiteral lineText startOffset quoteCharacter

            if not closed then
                diagnostics.AddError(
                    unterminatedMessage,
                    source.GetLocation(TextSpan.FromBounds(lineStart + startOffset, lineStart + endIndex))
                )

            emit kind (lineText.Substring(startOffset, endIndex - startOffset)) startOffset
            endIndex

        let rec scanPrefixedString prefixText prefixStart prefixEnd =
            emit InterpolatedStringStart prefixText prefixStart

            let mutable currentIndex = prefixEnd + 1
            let mutable segmentStart = currentIndex
            let mutable closed = false

            let emitPendingSegment endIndex =
                if endIndex > segmentStart then
                    emit StringTextSegment (lineText.Substring(segmentStart, endIndex - segmentStart)) segmentStart

            while currentIndex < lineText.Length && not closed do
                match lineText[currentIndex] with
                | '\\' ->
                    currentIndex <- min lineText.Length (currentIndex + 2)
                | '"' ->
                    emitPendingSegment currentIndex
                    emit InterpolatedStringEnd "\"" currentIndex
                    currentIndex <- currentIndex + 1
                    closed <- true
                | '$' when currentIndex + 1 < lineText.Length && lineText[currentIndex + 1] = '{' ->
                    emitPendingSegment currentIndex
                    emit InterpolationStart "${" currentIndex

                    let expressionEnd, terminated = scanInterpolationExpression (currentIndex + 2)

                    if terminated then
                        emit InterpolationEnd "}" expressionEnd
                        currentIndex <- expressionEnd + 1
                        segmentStart <- currentIndex
                    else
                        currentIndex <- expressionEnd
                | '$' when currentIndex + 1 < lineText.Length && lineText[currentIndex + 1] = '`' ->
                    emitPendingSegment currentIndex
                    emit InterpolationStart "$" currentIndex

                    let endIndex = scanSimpleBacktickName (currentIndex + 1)
                    tokens.Add(zeroLengthToken InterpolationEnd (lineStart + endIndex))
                    currentIndex <- endIndex
                    segmentStart <- currentIndex
                | '$' when currentIndex + 1 < lineText.Length && SyntaxFacts.isIdentifierStart lineText[currentIndex + 1] ->
                    emitPendingSegment currentIndex
                    emit InterpolationStart "$" currentIndex

                    let endIndex = scanSimpleName (currentIndex + 1)
                    tokens.Add(zeroLengthToken InterpolationEnd (lineStart + endIndex))
                    currentIndex <- endIndex
                    segmentStart <- currentIndex
                | _ ->
                    currentIndex <- currentIndex + 1

            if not closed then
                diagnostics.AddError(
                    "Unterminated prefixed string literal.",
                    source.GetLocation(TextSpan.FromBounds(lineStart + prefixStart, lineStart + currentIndex))
                )

                emitPendingSegment currentIndex

            currentIndex

        and scanSimpleBacktickName startOffset =
            let endIndex, closed = readBacktickIdentifier lineText startOffset

            if not closed then
                diagnostics.AddError(
                    "Unterminated backtick identifier.",
                    source.GetLocation(TextSpan.FromBounds(lineStart + startOffset, lineStart + endIndex))
                )

            emit Identifier (lineText.Substring(startOffset, endIndex - startOffset)) startOffset
            endIndex

        and scanSimpleName startOffset =
            let endIndex = readIdentifier lineText startOffset
            let text = lineText.Substring(startOffset, endIndex - startOffset)
            emitNameToken text startOffset
            endIndex

        and scanBacktickOrPrefixedString startOffset =
            let endIndex, closed = readBacktickIdentifier lineText startOffset

            if not closed then
                diagnostics.AddError(
                    "Unterminated backtick identifier.",
                    source.GetLocation(TextSpan.FromBounds(lineStart + startOffset, lineStart + endIndex))
                )

            let text = lineText.Substring(startOffset, endIndex - startOffset)

            if closed && endIndex < lineText.Length && lineText[endIndex] = '"' then
                scanPrefixedString text startOffset endIndex
            else
                emit Identifier text startOffset
                endIndex

        and scanNameOrPrefixedString startOffset =
            let endIndex = readIdentifier lineText startOffset
            let text = lineText.Substring(startOffset, endIndex - startOffset)

            if endIndex < lineText.Length && lineText[endIndex] = '"' then
                scanPrefixedString text startOffset endIndex
            else
                emitNameToken text startOffset
                endIndex

        and scanNumber startOffset =
            let mutable endIndex = startOffset + 1

            while endIndex < lineText.Length && Char.IsDigit(lineText[endIndex]) do
                endIndex <- endIndex + 1

            let kind =
                if endIndex + 1 < lineText.Length && lineText[endIndex] = '.' && Char.IsDigit(lineText[endIndex + 1]) then
                    endIndex <- endIndex + 2

                    while endIndex < lineText.Length && Char.IsDigit(lineText[endIndex]) do
                        endIndex <- endIndex + 1

                    FloatLiteral
                else
                    IntegerLiteral

            emit kind (lineText.Substring(startOffset, endIndex - startOffset)) startOffset
            endIndex

        and scanOperator startOffset =
            let mutable endIndex = startOffset + 1

            while endIndex < lineText.Length && SyntaxFacts.isOperatorCharacter lineText[endIndex] do
                endIndex <- endIndex + 1

            emit Operator (lineText.Substring(startOffset, endIndex - startOffset)) startOffset
            endIndex

        and scanInterpolationExpression startOffset =
            let mutable currentIndex = startOffset
            let mutable blockCommentDepth = 0
            let mutable braceDepth = 0
            let mutable setBraceDepth = 0
            let mutable terminated = false

            while currentIndex < lineText.Length && not terminated do
                if blockCommentDepth > 0 then
                    if isBlockCommentStart lineText currentIndex then
                        blockCommentDepth <- blockCommentDepth + 1
                        currentIndex <- currentIndex + 2
                    elif isBlockCommentEnd lineText currentIndex then
                        blockCommentDepth <- blockCommentDepth - 1
                        currentIndex <- currentIndex + 2
                    else
                        currentIndex <- currentIndex + 1
                elif braceDepth = 0 && setBraceDepth = 0 && lineText[currentIndex] = '}' then
                    terminated <- true
                else
                    let current = lineText[currentIndex]

                    match current with
                    | ' ' ->
                        currentIndex <- currentIndex + 1
                    | '\t' ->
                        diagnostics.AddError(
                            "Tabs are not permitted.",
                            source.GetLocation(TextSpan.FromBounds(lineStart + currentIndex, lineStart + currentIndex + 1))
                        )

                        currentIndex <- currentIndex + 1
                    | '-' when isLineComment lineText currentIndex ->
                        currentIndex <- lineText.Length
                    | '{' when isBlockCommentStart lineText currentIndex ->
                        blockCommentDepth <- blockCommentDepth + 1
                        currentIndex <- currentIndex + 2
                    | '{' when currentIndex + 1 < lineText.Length && lineText[currentIndex + 1] = '|' ->
                        emit LeftSetBrace "{|" currentIndex
                        setBraceDepth <- setBraceDepth + 1
                        currentIndex <- currentIndex + 2
                    | '|' when currentIndex + 1 < lineText.Length && lineText[currentIndex + 1] = '}' && setBraceDepth > 0 ->
                        emit RightSetBrace "|}" currentIndex
                        setBraceDepth <- setBraceDepth - 1
                        currentIndex <- currentIndex + 2
                    | '(' ->
                        emit LeftParen "(" currentIndex
                        currentIndex <- currentIndex + 1
                    | ')' ->
                        emit RightParen ")" currentIndex
                        currentIndex <- currentIndex + 1
                    | '[' ->
                        emit LeftBracket "[" currentIndex
                        currentIndex <- currentIndex + 1
                    | ']' ->
                        emit RightBracket "]" currentIndex
                        currentIndex <- currentIndex + 1
                    | '{' ->
                        emit LeftBrace "{" currentIndex
                        braceDepth <- braceDepth + 1
                        currentIndex <- currentIndex + 1
                    | '}' ->
                        emit RightBrace "}" currentIndex
                        braceDepth <- max 0 (braceDepth - 1)
                        currentIndex <- currentIndex + 1
                    | '.' ->
                        emit Dot "." currentIndex
                        currentIndex <- currentIndex + 1
                    | ',' ->
                        emit Comma "," currentIndex
                        currentIndex <- currentIndex + 1
                    | ':' ->
                        if currentIndex + 1 < lineText.Length && lineText[currentIndex + 1] = ':' then
                            emit Operator "::" currentIndex
                            currentIndex <- currentIndex + 2
                        else
                            emit Colon ":" currentIndex
                            currentIndex <- currentIndex + 1
                    | '@' ->
                        emit AtSign "@" currentIndex
                        currentIndex <- currentIndex + 1
                    | '\\' ->
                        emit Backslash "\\" currentIndex
                        currentIndex <- currentIndex + 1
                    | '=' ->
                        if currentIndex + 1 < lineText.Length && lineText[currentIndex + 1] = '=' then
                            emit Operator "==" currentIndex
                            currentIndex <- currentIndex + 2
                        else
                            emit Equals "=" currentIndex
                            currentIndex <- currentIndex + 1
                    | '-' when currentIndex + 1 < lineText.Length && lineText[currentIndex + 1] = '>' ->
                        emit Arrow "->" currentIndex
                        currentIndex <- currentIndex + 2
                    | '"' ->
                        currentIndex <- emitQuotedLiteral StringLiteral currentIndex '"' "Unterminated string literal."
                    | '\'' ->
                        currentIndex <- emitQuotedLiteral CharacterLiteral currentIndex '\'' "Unterminated character literal."
                    | '`' ->
                        currentIndex <- scanBacktickOrPrefixedString currentIndex
                    | _ when Char.IsDigit(current) ->
                        currentIndex <- scanNumber currentIndex
                    | _ when SyntaxFacts.isIdentifierStart current ->
                        currentIndex <- scanNameOrPrefixedString currentIndex
                    | _ when SyntaxFacts.isOperatorCharacter current ->
                        currentIndex <- scanOperator currentIndex
                    | _ ->
                        diagnostics.AddError(
                            $"Unrecognized character '{current}'.",
                            source.GetLocation(TextSpan.FromBounds(lineStart + currentIndex, lineStart + currentIndex + 1))
                        )

                        tokens.Add(token BadToken (string current) (lineStart + currentIndex))
                        currentIndex <- currentIndex + 1

            if blockCommentDepth > 0 || not terminated then
                diagnostics.AddError(
                    "Unterminated string interpolation.",
                    source.GetLocation(TextSpan.FromBounds(lineStart + startOffset, lineStart + currentIndex))
                )

            currentIndex, terminated

        while index < lineText.Length do
            if blockCommentDepth > 0 then
                if isBlockCommentStart lineText index then
                    blockCommentDepth <- blockCommentDepth + 1
                    index <- index + 2
                elif isBlockCommentEnd lineText index then
                    blockCommentDepth <- blockCommentDepth - 1
                    index <- index + 2
                else
                    index <- index + 1
            else
                let current = lineText[index]

                match current with
                | ' ' ->
                    index <- index + 1
                | '\t' ->
                    diagnostics.AddError("Tabs are not permitted.", source.GetLocation(TextSpan.FromBounds(lineStart + index, lineStart + index + 1)))
                    index <- index + 1
                | '-' when isLineComment lineText index ->
                    index <- lineText.Length
                | '{' when isBlockCommentStart lineText index ->
                    blockCommentDepth <- blockCommentDepth + 1
                    index <- index + 2
                | '{' when index + 1 < lineText.Length && lineText[index + 1] = '|' ->
                    emit LeftSetBrace "{|" index
                    delimiterDepth <- delimiterDepth + 1
                    index <- index + 2
                | '|' when index + 1 < lineText.Length && lineText[index + 1] = '}' ->
                    emit RightSetBrace "|}" index
                    delimiterDepth <- max 0 (delimiterDepth - 1)
                    index <- index + 2
                | '(' ->
                    emit LeftParen "(" index
                    delimiterDepth <- delimiterDepth + 1
                    index <- index + 1
                | ')' ->
                    emit RightParen ")" index
                    delimiterDepth <- max 0 (delimiterDepth - 1)
                    index <- index + 1
                | '[' ->
                    emit LeftBracket "[" index
                    delimiterDepth <- delimiterDepth + 1
                    index <- index + 1
                | ']' ->
                    emit RightBracket "]" index
                    delimiterDepth <- max 0 (delimiterDepth - 1)
                    index <- index + 1
                | '{' ->
                    emit LeftBrace "{" index
                    delimiterDepth <- delimiterDepth + 1
                    index <- index + 1
                | '}' ->
                    emit RightBrace "}" index
                    delimiterDepth <- max 0 (delimiterDepth - 1)
                    index <- index + 1
                | '.' ->
                    emit Dot "." index
                    index <- index + 1
                | ',' ->
                    emit Comma "," index
                    index <- index + 1
                | ':' ->
                    if index + 1 < lineText.Length && lineText[index + 1] = ':' then
                        emit Operator "::" index
                        index <- index + 2
                    else
                        emit Colon ":" index
                        index <- index + 1
                | '@' ->
                    emit AtSign "@" index
                    index <- index + 1
                | '\\' ->
                    emit Backslash "\\" index
                    index <- index + 1
                | '=' ->
                    if index + 1 < lineText.Length && lineText[index + 1] = '=' then
                        emit Operator "==" index
                        index <- index + 2
                    else
                        emit Equals "=" index
                        index <- index + 1
                | '-' when index + 1 < lineText.Length && lineText[index + 1] = '>' ->
                    emit Arrow "->" index
                    index <- index + 2
                | '"' ->
                    index <- emitQuotedLiteral StringLiteral index '"' "Unterminated string literal."
                | '\'' ->
                    index <- emitQuotedLiteral CharacterLiteral index '\'' "Unterminated character literal."
                | '`' ->
                    index <- scanBacktickOrPrefixedString index
                | _ when Char.IsDigit(current) ->
                    index <- scanNumber index
                | _ when SyntaxFacts.isIdentifierStart current ->
                    index <- scanNameOrPrefixedString index
                | _ when SyntaxFacts.isOperatorCharacter current ->
                    index <- scanOperator index
                | _ ->
                    diagnostics.AddError(
                        $"Unrecognized character '{current}'.",
                        source.GetLocation(TextSpan.FromBounds(lineStart + index, lineStart + index + 1))
                    )
                    tokens.Add(token BadToken (string current) (lineStart + index))
                    index <- index + 1

        if emittedCode then
            tokens.Add(zeroLengthToken Newline (lineStart + lineText.Length))

        blockCommentDepth, delimiterDepth

    let tokenize (source: SourceText) =
        let diagnostics = DiagnosticBag()
        let tokens = ResizeArray<Token>()
        let indentStack = Stack<int>()
        indentStack.Push(0)

        let mutable blockCommentDepth = 0
        let mutable delimiterDepth = 0

        for lineIndex in 0 .. source.LineCount - 1 do
            let lineText = source.GetLineText(lineIndex)
            let hasCode = lineContainsCode lineText blockCommentDepth

            if hasCode && blockCommentDepth = 0 && delimiterDepth = 0 then
                let indent = countLeadingSpaces source lineIndex lineText diagnostics
                addIndentationTokens source lineIndex indent indentStack tokens diagnostics

            let newBlockDepth, newDelimiterDepth =
                tokenizeLine source lineIndex lineText blockCommentDepth delimiterDepth tokens diagnostics

            blockCommentDepth <- newBlockDepth
            delimiterDepth <- newDelimiterDepth

        if blockCommentDepth > 0 then
            diagnostics.AddError("Unterminated block comment.", source.GetLocation(TextSpan.FromBounds(source.Length, source.Length)))

        while indentStack.Count > 1 do
            indentStack.Pop() |> ignore
            tokens.Add(zeroLengthToken Dedent source.Length)

        tokens.Add(zeroLengthToken EndOfFile source.Length)

        { Tokens = List.ofSeq tokens
          Diagnostics = diagnostics.Items }
