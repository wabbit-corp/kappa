namespace Kappa.Compiler

open System
open System.Collections.Generic

type LexResult =
    { Tokens: Token list
      Diagnostics: Diagnostic list }

// Tokenizes source text, tracks layout, and records lexical diagnostics.
module Lexer =
    type private ScannedStringLiteral =
        { EndIndex: int
          Closed: bool
          IsMultiline: bool }

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
                diagnostics.AddError(
                    DiagnosticFact.lexer (LexerDiagnosticEvidence.TabCharacterNotPermitted true),
                    source.GetLocation(TextSpan.FromBounds(lineStart + index, lineStart + index + 1))
                )
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
                diagnostics.AddError(DiagnosticFact.lexer (LexerDiagnosticEvidence.UnexpectedIndentation indent),
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

    let private startsSyntaxQuote (text: string) index =
        index + 1 < text.Length
        && text[index + 1] = '{'
        && (index + 2 >= text.Length || text[index + 2] <> '\'')

    let private countLeadingHashes (text: string) index =
        let mutable currentIndex = index

        while currentIndex < text.Length && text[currentIndex] = '#' do
            currentIndex <- currentIndex + 1

        currentIndex - index

    let private tryScanStringLiteral (text: string) startIndex =
        let hashCount =
            if startIndex < text.Length && text[startIndex] = '#' then
                countLeadingHashes text startIndex
            else
                0

        let quoteIndex = startIndex + hashCount

        if quoteIndex >= text.Length || text[quoteIndex] <> '"' then
            None
        else
            let isMultiline =
                quoteIndex + 2 < text.Length
                && text[quoteIndex + 1] = '"'
                && text[quoteIndex + 2] = '"'

            if isMultiline then
                let closingDelimiter = "\"\"\"" + String('#', hashCount)
                let closeIndex = text.IndexOf(closingDelimiter, quoteIndex + 3, StringComparison.Ordinal)

                if closeIndex >= 0 then
                    Some
                        { EndIndex = closeIndex + closingDelimiter.Length
                          Closed = true
                          IsMultiline = true }
                else
                    Some
                        { EndIndex = text.Length
                          Closed = false
                          IsMultiline = true }
            elif hashCount > 0 then
                let mutable currentIndex = quoteIndex + 1
                let mutable closed = false

                while currentIndex < text.Length && not closed do
                    if text[currentIndex] = '\r' || text[currentIndex] = '\n' then
                        currentIndex <- text.Length
                    elif text[currentIndex] = '"' then
                        let hashSuffixLength = countLeadingHashes text (currentIndex + 1)

                        if hashSuffixLength = hashCount then
                            currentIndex <- currentIndex + 1 + hashCount
                            closed <- true
                        else
                            currentIndex <- currentIndex + 1
                    else
                        currentIndex <- currentIndex + 1

                Some
                    { EndIndex = currentIndex
                      Closed = closed
                      IsMultiline = false }
            else
                let mutable currentIndex = quoteIndex + 1
                let mutable closed = false

                while currentIndex < text.Length && not closed do
                    if text[currentIndex] = '\r' || text[currentIndex] = '\n' then
                        currentIndex <- text.Length
                    elif text[currentIndex] = '\\' then
                        currentIndex <- min text.Length (currentIndex + 2)
                    elif text[currentIndex] = '"' then
                        currentIndex <- currentIndex + 1
                        closed <- true
                    else
                        currentIndex <- currentIndex + 1

                Some
                    { EndIndex = currentIndex
                      Closed = closed
                      IsMultiline = false }

    let private tokenizeLine
        (source: SourceText)
        lineIndex
        (lineText: string)
        startingBlockCommentDepth
        startingDelimiterDepth
        startOffset
        (tokens: ResizeArray<Token>)
        (diagnostics: DiagnosticBag)
        =
        let lineStart = source.LineStarts[lineIndex]
        let mutable blockCommentDepth = startingBlockCommentDepth
        let mutable delimiterDepth = startingDelimiterDepth
        let mutable index = startOffset
        let mutable emittedCode = false
        let mutable consumedUntilAbsoluteIndex = lineStart

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

        let emitQuotedLiteral kind startOffset quoteCharacter unterminatedDiagnostic =
            let endIndex, closed = readQuotedLiteral lineText startOffset quoteCharacter

            if not closed then
                diagnostics.AddError(
                    unterminatedDiagnostic,
                    source.GetLocation(TextSpan.FromBounds(lineStart + startOffset, lineStart + endIndex))
                )

            emit kind (lineText.Substring(startOffset, endIndex - startOffset)) startOffset
            endIndex

        let emitAbsolute kind text absoluteStartIndex =
            emittedCode <- true
            tokens.Add(token kind text absoluteStartIndex)

        let scanAbsoluteStringLiteral absoluteStartIndex =
            let scanned =
                tryScanStringLiteral source.Content absoluteStartIndex
                |> Option.defaultValue
                    { EndIndex = absoluteStartIndex + 1
                      Closed = false
                      IsMultiline = false }

            if not scanned.Closed then
                diagnostics.AddError(DiagnosticFact.lexer (LexerDiagnosticEvidence.UnterminatedStringLiteral false),
                    source.GetLocation(TextSpan.FromBounds(absoluteStartIndex, scanned.EndIndex))
                )

            emitAbsolute StringLiteral (source.Slice(TextSpan.FromBounds(absoluteStartIndex, scanned.EndIndex))) absoluteStartIndex
            consumedUntilAbsoluteIndex <- max consumedUntilAbsoluteIndex scanned.EndIndex
            scanned.EndIndex

        let scanStringLiteral startOffset =
            let absoluteStartIndex = lineStart + startOffset
            let endIndex = scanAbsoluteStringLiteral absoluteStartIndex

            if endIndex <= lineStart + lineText.Length then
                endIndex - lineStart
            else
                lineText.Length

        let rec scanPrefixedString prefixText prefixStart prefixEnd hashCount =
            let absolutePrefixStart = lineStart + prefixStart
            let absolutePrefixEnd = lineStart + prefixEnd
            let quoteAbsoluteIndex = absolutePrefixEnd + hashCount
            let isMultiline =
                quoteAbsoluteIndex + 2 < source.Length
                && source.Content[quoteAbsoluteIndex] = '"'
                && source.Content[quoteAbsoluteIndex + 1] = '"'
                && source.Content[quoteAbsoluteIndex + 2] = '"'

            let rec scanInterpolationExpressionAbsolute startAbsoluteIndex =
                let text = source.Content
                let mutable currentAbsoluteIndex = startAbsoluteIndex
                let mutable blockCommentDepth = 0
                let mutable braceDepth = 0
                let mutable setBraceDepth = 0
                let mutable terminated = false

                let endOfLineFrom index =
                    let mutable cursor = index

                    while cursor < text.Length && text[cursor] <> '\n' && text[cursor] <> '\r' do
                        cursor <- cursor + 1

                    cursor

                while currentAbsoluteIndex < text.Length && not terminated do
                    if blockCommentDepth > 0 then
                        if isBlockCommentStart text currentAbsoluteIndex then
                            blockCommentDepth <- blockCommentDepth + 1
                            currentAbsoluteIndex <- currentAbsoluteIndex + 2
                        elif isBlockCommentEnd text currentAbsoluteIndex then
                            blockCommentDepth <- blockCommentDepth - 1
                            currentAbsoluteIndex <- currentAbsoluteIndex + 2
                        else
                            currentAbsoluteIndex <- currentAbsoluteIndex + 1
                    elif braceDepth = 0 && setBraceDepth = 0 && text[currentAbsoluteIndex] = '}' then
                        terminated <- true
                    else
                        let current = text[currentAbsoluteIndex]

                        match current with
                        | ' '
                        | '\n'
                        | '\r' ->
                            currentAbsoluteIndex <- currentAbsoluteIndex + 1
                        | '\t' ->
                            diagnostics.AddError(DiagnosticFact.lexer (LexerDiagnosticEvidence.TabCharacterNotPermitted false),
                                source.GetLocation(TextSpan.FromBounds(currentAbsoluteIndex, currentAbsoluteIndex + 1))
                            )

                            currentAbsoluteIndex <- currentAbsoluteIndex + 1
                        | '-' when isLineComment text currentAbsoluteIndex ->
                            currentAbsoluteIndex <- endOfLineFrom currentAbsoluteIndex
                        | '{' when isBlockCommentStart text currentAbsoluteIndex ->
                            blockCommentDepth <- blockCommentDepth + 1
                            currentAbsoluteIndex <- currentAbsoluteIndex + 2
                        | '{' when currentAbsoluteIndex + 1 < text.Length && text[currentAbsoluteIndex + 1] = '|' ->
                            emitAbsolute LeftSetBrace "{|" currentAbsoluteIndex
                            setBraceDepth <- setBraceDepth + 1
                            currentAbsoluteIndex <- currentAbsoluteIndex + 2
                        | '|' when currentAbsoluteIndex + 1 < text.Length && text[currentAbsoluteIndex + 1] = '}' && setBraceDepth > 0 ->
                            emitAbsolute RightSetBrace "|}" currentAbsoluteIndex
                            setBraceDepth <- setBraceDepth - 1
                            currentAbsoluteIndex <- currentAbsoluteIndex + 2
                        | '(' ->
                            emitAbsolute LeftParen "(" currentAbsoluteIndex
                            currentAbsoluteIndex <- currentAbsoluteIndex + 1
                        | ')' ->
                            emitAbsolute RightParen ")" currentAbsoluteIndex
                            currentAbsoluteIndex <- currentAbsoluteIndex + 1
                        | '<' when currentAbsoluteIndex + 1 < text.Length && text[currentAbsoluteIndex + 1] = '[' ->
                            emitAbsolute LeftEffectRow "<[" currentAbsoluteIndex
                            currentAbsoluteIndex <- currentAbsoluteIndex + 2
                        | ']' when currentAbsoluteIndex + 1 < text.Length && text[currentAbsoluteIndex + 1] = '>' ->
                            emitAbsolute RightEffectRow "]>" currentAbsoluteIndex
                            currentAbsoluteIndex <- currentAbsoluteIndex + 2
                        | '[' ->
                            emitAbsolute LeftBracket "[" currentAbsoluteIndex
                            currentAbsoluteIndex <- currentAbsoluteIndex + 1
                        | ']' ->
                            emitAbsolute RightBracket "]" currentAbsoluteIndex
                            currentAbsoluteIndex <- currentAbsoluteIndex + 1
                        | '{' ->
                            emitAbsolute LeftBrace "{" currentAbsoluteIndex
                            braceDepth <- braceDepth + 1
                            currentAbsoluteIndex <- currentAbsoluteIndex + 1
                        | '}' ->
                            emitAbsolute RightBrace "}" currentAbsoluteIndex
                            braceDepth <- max 0 (braceDepth - 1)
                            currentAbsoluteIndex <- currentAbsoluteIndex + 1
                        | '.' when currentAbsoluteIndex + 2 < text.Length
                                 && text[currentAbsoluteIndex + 1] = '.'
                                 && text[currentAbsoluteIndex + 2] = '<' ->
                            emitAbsolute Operator "..<" currentAbsoluteIndex
                            currentAbsoluteIndex <- currentAbsoluteIndex + 3
                        | '.' when currentAbsoluteIndex + 1 < text.Length && text[currentAbsoluteIndex + 1] = '.' ->
                            emitAbsolute Operator ".." currentAbsoluteIndex
                            currentAbsoluteIndex <- currentAbsoluteIndex + 2
                        | '.' ->
                            emitAbsolute Dot "." currentAbsoluteIndex
                            currentAbsoluteIndex <- currentAbsoluteIndex + 1
                        | ',' ->
                            emitAbsolute Comma "," currentAbsoluteIndex
                            currentAbsoluteIndex <- currentAbsoluteIndex + 1
                        | ':' ->
                            if currentAbsoluteIndex + 1 < text.Length && text[currentAbsoluteIndex + 1] = ':' then
                                emitAbsolute Operator "::" currentAbsoluteIndex
                                currentAbsoluteIndex <- currentAbsoluteIndex + 2
                            else
                                emitAbsolute Colon ":" currentAbsoluteIndex
                                currentAbsoluteIndex <- currentAbsoluteIndex + 1
                        | '@' ->
                            emitAbsolute AtSign "@" currentAbsoluteIndex
                            currentAbsoluteIndex <- currentAbsoluteIndex + 1
                        | '\\' ->
                            emitAbsolute Backslash "\\" currentAbsoluteIndex
                            currentAbsoluteIndex <- currentAbsoluteIndex + 1
                        | '=' ->
                            if currentAbsoluteIndex + 1 < text.Length && text[currentAbsoluteIndex + 1] = '=' then
                                emitAbsolute Operator "==" currentAbsoluteIndex
                                currentAbsoluteIndex <- currentAbsoluteIndex + 2
                            else
                                emitAbsolute Equals "=" currentAbsoluteIndex
                                currentAbsoluteIndex <- currentAbsoluteIndex + 1
                        | '-' when currentAbsoluteIndex + 1 < text.Length && text[currentAbsoluteIndex + 1] = '>' ->
                            emitAbsolute Arrow "->" currentAbsoluteIndex
                            currentAbsoluteIndex <- currentAbsoluteIndex + 2
                        | '"' ->
                            currentAbsoluteIndex <- scanAbsoluteStringLiteral currentAbsoluteIndex
                        | '#' when
                            let nestedHashCount = countLeadingHashes text currentAbsoluteIndex
                            currentAbsoluteIndex + nestedHashCount < text.Length && text[currentAbsoluteIndex + nestedHashCount] = '"' ->
                            currentAbsoluteIndex <- scanAbsoluteStringLiteral currentAbsoluteIndex
                        | '\'' ->
                            if startsSyntaxQuote text currentAbsoluteIndex then
                                emitAbsolute Operator "'" currentAbsoluteIndex
                                currentAbsoluteIndex <- currentAbsoluteIndex + 1
                            else
                                let endIndex, closed = readQuotedLiteral text currentAbsoluteIndex '\''

                                if not closed then
                                    diagnostics.AddError(DiagnosticFact.lexer LexerDiagnosticEvidence.UnterminatedCharacterLiteral,
                                        source.GetLocation(TextSpan.FromBounds(currentAbsoluteIndex, endIndex))
                                    )

                                emitAbsolute CharacterLiteral (text.Substring(currentAbsoluteIndex, endIndex - currentAbsoluteIndex)) currentAbsoluteIndex
                                currentAbsoluteIndex <- endIndex
                        | '`' ->
                            let endIndex, closed = readBacktickIdentifier text currentAbsoluteIndex

                            if not closed then
                                diagnostics.AddError(DiagnosticFact.lexer LexerDiagnosticEvidence.UnterminatedBacktickIdentifier,
                                    source.GetLocation(TextSpan.FromBounds(currentAbsoluteIndex, endIndex))
                                )

                            emitAbsolute Identifier (text.Substring(currentAbsoluteIndex, endIndex - currentAbsoluteIndex)) currentAbsoluteIndex
                            currentAbsoluteIndex <- endIndex
                        | _ when Char.IsDigit(current) ->
                            match SyntaxFacts.scanNumericToken text currentAbsoluteIndex with
                            | Some(SyntaxFacts.ValidNumericToken(kind, endIndex)) ->
                                emitAbsolute kind (text.Substring(currentAbsoluteIndex, endIndex - currentAbsoluteIndex)) currentAbsoluteIndex
                                currentAbsoluteIndex <- endIndex
                            | Some(SyntaxFacts.InvalidNumericToken(error, endIndex)) ->
                                diagnostics.AddError(DiagnosticFact.lexer (LexerDiagnosticEvidence.MalformedNumericLiteral error),
                                    source.GetLocation(TextSpan.FromBounds(currentAbsoluteIndex, endIndex))
                                )

                                emitAbsolute BadToken (text.Substring(currentAbsoluteIndex, endIndex - currentAbsoluteIndex)) currentAbsoluteIndex
                                currentAbsoluteIndex <- endIndex
                            | None ->
                                emitAbsolute IntegerLiteral (text.Substring(currentAbsoluteIndex, 1)) currentAbsoluteIndex
                                currentAbsoluteIndex <- currentAbsoluteIndex + 1
                        | _ when SyntaxFacts.isIdentifierStart current ->
                            let endIndex = readIdentifier text currentAbsoluteIndex
                            let identifierText = text.Substring(currentAbsoluteIndex, endIndex - currentAbsoluteIndex)

                            if identifierText = "let" && endIndex < text.Length && text[endIndex] = '?' then
                                emitAbsolute (Keyword Keyword.LetQuestion) "let?" currentAbsoluteIndex
                                currentAbsoluteIndex <- endIndex + 1
                            elif identifierText = "for" && endIndex < text.Length && text[endIndex] = '?' then
                                emitAbsolute (Keyword Keyword.ForQuestion) "for?" currentAbsoluteIndex
                                currentAbsoluteIndex <- endIndex + 1
                            else
                                let kind =
                                    if identifierText = "_" then
                                        Underscore
                                    else
                                        match Keyword.tryParse identifierText with
                                        | Some keyword -> Keyword keyword
                                        | None -> Identifier

                                emitAbsolute kind identifierText currentAbsoluteIndex
                                currentAbsoluteIndex <- endIndex
                        | _ when SyntaxFacts.isOperatorCharacter current ->
                            let mutable endIndex = currentAbsoluteIndex + 1

                            while endIndex < text.Length && SyntaxFacts.isOperatorCharacter text[endIndex] do
                                endIndex <- endIndex + 1

                            emitAbsolute Operator (text.Substring(currentAbsoluteIndex, endIndex - currentAbsoluteIndex)) currentAbsoluteIndex
                            currentAbsoluteIndex <- endIndex
                        | _ ->
                            diagnostics.AddError(DiagnosticFact.lexer (LexerDiagnosticEvidence.UnrecognizedCharacter(string current)),
                                source.GetLocation(TextSpan.FromBounds(currentAbsoluteIndex, currentAbsoluteIndex + 1))
                            )

                            tokens.Add(token BadToken (string current) currentAbsoluteIndex)
                            currentAbsoluteIndex <- currentAbsoluteIndex + 1

                if blockCommentDepth > 0 || not terminated then
                    diagnostics.AddError(DiagnosticFact.lexer LexerDiagnosticEvidence.UnterminatedStringInterpolation,
                        source.GetLocation(TextSpan.FromBounds(startAbsoluteIndex, currentAbsoluteIndex))
                    )

                currentAbsoluteIndex, terminated

            let scanMultilinePrefixedStringAbsolute () =
                let text = source.Content
                let quoteLength = 3
                let openingLength = hashCount + quoteLength
                let openingQuoteStart = absolutePrefixEnd
                let contentStart = openingQuoteStart + openingLength
                let closingDelimiter = "\"\"\"" + String('#', hashCount)
                emitAbsolute InterpolatedStringStart (SyntaxFacts.encodePrefixedStringStart prefixText hashCount) absolutePrefixStart

                let mutable currentAbsoluteIndex = contentStart
                let mutable segmentStart = contentStart
                let mutable closed = false

                let emitPendingSegment endAbsoluteIndex =
                    if endAbsoluteIndex > segmentStart then
                        emitAbsolute
                            StringTextSegment
                            (source.Slice(TextSpan.FromBounds(segmentStart, endAbsoluteIndex)))
                            segmentStart

                while currentAbsoluteIndex < text.Length && not closed do
                    if text.AsSpan(currentAbsoluteIndex).StartsWith(closingDelimiter.AsSpan(), StringComparison.Ordinal) then
                        emitPendingSegment currentAbsoluteIndex
                        emitAbsolute InterpolatedStringEnd closingDelimiter currentAbsoluteIndex
                        currentAbsoluteIndex <- currentAbsoluteIndex + closingDelimiter.Length
                        closed <- true
                    elif hashCount = 0 && currentAbsoluteIndex + 1 < text.Length && text[currentAbsoluteIndex] = '$' && text[currentAbsoluteIndex + 1] = '{' then
                        emitPendingSegment currentAbsoluteIndex
                        emitAbsolute InterpolationStart "${" currentAbsoluteIndex

                        let expressionEnd, terminated = scanInterpolationExpressionAbsolute (currentAbsoluteIndex + 2)

                        if terminated then
                            emitAbsolute InterpolationEnd "}" expressionEnd
                            currentAbsoluteIndex <- expressionEnd + 1
                            segmentStart <- currentAbsoluteIndex
                        else
                            currentAbsoluteIndex <- expressionEnd
                    elif hashCount = 0 && currentAbsoluteIndex + 1 < text.Length && text[currentAbsoluteIndex] = '$' && text[currentAbsoluteIndex + 1] = '`' then
                        emitPendingSegment currentAbsoluteIndex
                        emitAbsolute InterpolationStart "$" currentAbsoluteIndex

                        let endIndex, closedIdentifier = readBacktickIdentifier text (currentAbsoluteIndex + 1)

                        if not closedIdentifier then
                            diagnostics.AddError(DiagnosticFact.lexer LexerDiagnosticEvidence.UnterminatedBacktickIdentifier,
                                source.GetLocation(TextSpan.FromBounds(currentAbsoluteIndex + 1, endIndex))
                            )

                        emitAbsolute Identifier (text.Substring(currentAbsoluteIndex + 1, endIndex - (currentAbsoluteIndex + 1))) (currentAbsoluteIndex + 1)
                        tokens.Add(zeroLengthToken InterpolationEnd endIndex)
                        currentAbsoluteIndex <- endIndex
                        segmentStart <- currentAbsoluteIndex
                    elif hashCount = 0 && currentAbsoluteIndex + 1 < text.Length && text[currentAbsoluteIndex] = '$' && SyntaxFacts.isIdentifierStart text[currentAbsoluteIndex + 1] then
                        emitPendingSegment currentAbsoluteIndex
                        emitAbsolute InterpolationStart "$" currentAbsoluteIndex

                        let endIndex = readIdentifier text (currentAbsoluteIndex + 1)
                        let identifierText = text.Substring(currentAbsoluteIndex + 1, endIndex - (currentAbsoluteIndex + 1))
                        let kind =
                            if identifierText = "_" then Underscore else Identifier

                        emitAbsolute kind identifierText (currentAbsoluteIndex + 1)
                        tokens.Add(zeroLengthToken InterpolationEnd endIndex)
                        currentAbsoluteIndex <- endIndex
                        segmentStart <- currentAbsoluteIndex
                    elif hashCount > 0
                         && currentAbsoluteIndex + hashCount < text.Length
                         && text.Substring(currentAbsoluteIndex, hashCount) = String('#', hashCount)
                         && text[currentAbsoluteIndex + hashCount] = '{' then
                        emitPendingSegment currentAbsoluteIndex
                        emitAbsolute InterpolationStart (String('#', hashCount) + "{") currentAbsoluteIndex

                        let expressionEnd, terminated = scanInterpolationExpressionAbsolute (currentAbsoluteIndex + hashCount + 1)

                        if terminated then
                            emitAbsolute InterpolationEnd "}" expressionEnd
                            currentAbsoluteIndex <- expressionEnd + 1
                            segmentStart <- currentAbsoluteIndex
                        else
                            currentAbsoluteIndex <- expressionEnd
                    elif hashCount = 0 && text[currentAbsoluteIndex] = '\\' then
                        currentAbsoluteIndex <- min text.Length (currentAbsoluteIndex + 2)
                    else
                        currentAbsoluteIndex <- currentAbsoluteIndex + 1

                if not closed then
                    diagnostics.AddError(DiagnosticFact.lexer (LexerDiagnosticEvidence.UnterminatedStringLiteral true),
                        source.GetLocation(TextSpan.FromBounds(absolutePrefixStart, currentAbsoluteIndex))
                    )

                    emitPendingSegment currentAbsoluteIndex

                consumedUntilAbsoluteIndex <- max consumedUntilAbsoluteIndex currentAbsoluteIndex

                if currentAbsoluteIndex <= lineStart + lineText.Length then
                    currentAbsoluteIndex - lineStart
                else
                    lineText.Length

            if isMultiline then
                scanMultilinePrefixedStringAbsolute ()
            else
                emit InterpolatedStringStart (SyntaxFacts.encodePrefixedStringStart prefixText hashCount) prefixStart

                let mutable currentIndex =
                    if hashCount = 0 then
                        prefixEnd + 1
                    else
                        prefixEnd + hashCount + 1
                let mutable segmentStart = currentIndex
                let mutable closed = false

                let emitPendingSegment endIndex =
                    if endIndex > segmentStart then
                        emit StringTextSegment (lineText.Substring(segmentStart, endIndex - segmentStart)) segmentStart

                while currentIndex < lineText.Length && not closed do
                    if hashCount = 0 && lineText[currentIndex] = '\\' then
                        currentIndex <- min lineText.Length (currentIndex + 2)
                    elif hashCount = 0 && lineText[currentIndex] = '"' then
                        emitPendingSegment currentIndex
                        emit InterpolatedStringEnd "\"" currentIndex
                        currentIndex <- currentIndex + 1
                        closed <- true
                    elif hashCount > 0
                         && lineText[currentIndex] = '"'
                         && currentIndex + hashCount < lineText.Length
                         && countLeadingHashes lineText (currentIndex + 1) = hashCount then
                        emitPendingSegment currentIndex
                        emit InterpolatedStringEnd ("\"" + String('#', hashCount)) currentIndex
                        currentIndex <- currentIndex + 1 + hashCount
                        closed <- true
                    elif hashCount = 0 && currentIndex + 1 < lineText.Length && lineText[currentIndex] = '$' && lineText[currentIndex + 1] = '{' then
                        emitPendingSegment currentIndex
                        emit InterpolationStart "${" currentIndex

                        let expressionEnd, terminated = scanInterpolationExpression (currentIndex + 2)

                        if terminated then
                            emit InterpolationEnd "}" expressionEnd
                            currentIndex <- expressionEnd + 1
                            segmentStart <- currentIndex
                        else
                            currentIndex <- expressionEnd
                    elif hashCount = 0 && currentIndex + 1 < lineText.Length && lineText[currentIndex] = '$' && lineText[currentIndex + 1] = '`' then
                        emitPendingSegment currentIndex
                        emit InterpolationStart "$" currentIndex

                        let endIndex = scanSimpleBacktickName (currentIndex + 1)
                        tokens.Add(zeroLengthToken InterpolationEnd (lineStart + endIndex))
                        currentIndex <- endIndex
                        segmentStart <- currentIndex
                    elif hashCount = 0 && currentIndex + 1 < lineText.Length && lineText[currentIndex] = '$' && SyntaxFacts.isIdentifierStart lineText[currentIndex + 1] then
                        emitPendingSegment currentIndex
                        emit InterpolationStart "$" currentIndex

                        let endIndex = scanSimpleName (currentIndex + 1)
                        tokens.Add(zeroLengthToken InterpolationEnd (lineStart + endIndex))
                        currentIndex <- endIndex
                        segmentStart <- currentIndex
                    elif hashCount > 0
                         && currentIndex + hashCount < lineText.Length
                         && lineText.Substring(currentIndex, hashCount) = String('#', hashCount)
                         && lineText[currentIndex + hashCount] = '{' then
                        emitPendingSegment currentIndex
                        emit InterpolationStart (String('#', hashCount) + "{") currentIndex

                        let expressionEnd, terminated = scanInterpolationExpression (currentIndex + hashCount + 1)

                        if terminated then
                            emit InterpolationEnd "}" expressionEnd
                            currentIndex <- expressionEnd + 1
                            segmentStart <- currentIndex
                        else
                            currentIndex <- expressionEnd
                    else
                        currentIndex <- currentIndex + 1

                if not closed then
                    diagnostics.AddError(DiagnosticFact.lexer (LexerDiagnosticEvidence.UnterminatedStringLiteral true),
                        source.GetLocation(TextSpan.FromBounds(lineStart + prefixStart, lineStart + currentIndex))
                    )

                    emitPendingSegment currentIndex

                currentIndex

        and scanSimpleBacktickName startOffset =
            let endIndex, closed = readBacktickIdentifier lineText startOffset

            if not closed then
                diagnostics.AddError(DiagnosticFact.lexer LexerDiagnosticEvidence.UnterminatedBacktickIdentifier,
                    source.GetLocation(TextSpan.FromBounds(lineStart + startOffset, lineStart + endIndex))
                )

            emit Identifier (lineText.Substring(startOffset, endIndex - startOffset)) startOffset
            endIndex

        and scanSimpleName startOffset =
            let endIndex = readIdentifier lineText startOffset
            let text = lineText.Substring(startOffset, endIndex - startOffset)

            if text = "let" && endIndex < lineText.Length && lineText[endIndex] = '?' then
                emit (Keyword Keyword.LetQuestion) "let?" startOffset
                endIndex + 1
            elif text = "for" && endIndex < lineText.Length && lineText[endIndex] = '?' then
                emit (Keyword Keyword.ForQuestion) "for?" startOffset
                endIndex + 1
            else
                emitNameToken text startOffset
                endIndex

        and scanBacktickOrPrefixedString startOffset =
            let endIndex, closed = readBacktickIdentifier lineText startOffset

            if not closed then
                diagnostics.AddError(DiagnosticFact.lexer LexerDiagnosticEvidence.UnterminatedBacktickIdentifier,
                    source.GetLocation(TextSpan.FromBounds(lineStart + startOffset, lineStart + endIndex))
                )

            let text = lineText.Substring(startOffset, endIndex - startOffset)

            let rawHashCount = countLeadingHashes lineText endIndex

            if closed && endIndex < lineText.Length && lineText[endIndex] = '"' then
                scanPrefixedString text startOffset endIndex 0
            elif closed && rawHashCount > 0 && endIndex + rawHashCount < lineText.Length && lineText[endIndex + rawHashCount] = '"' then
                scanPrefixedString text startOffset endIndex rawHashCount
            else
                emit Identifier text startOffset
                endIndex

        and scanNameOrPrefixedString startOffset =
            let endIndex = readIdentifier lineText startOffset
            let text = lineText.Substring(startOffset, endIndex - startOffset)

            if text = "let" && endIndex < lineText.Length && lineText[endIndex] = '?' then
                emit (Keyword Keyword.LetQuestion) "let?" startOffset
                endIndex + 1
            elif text = "for" && endIndex < lineText.Length && lineText[endIndex] = '?' then
                emit (Keyword Keyword.ForQuestion) "for?" startOffset
                endIndex + 1
            elif endIndex < lineText.Length && lineText[endIndex] = '"' then
                scanPrefixedString text startOffset endIndex 0
            elif endIndex < lineText.Length && lineText[endIndex] = '#' then
                let rawHashCount = countLeadingHashes lineText endIndex

                if endIndex + rawHashCount < lineText.Length && lineText[endIndex + rawHashCount] = '"' then
                    scanPrefixedString text startOffset endIndex rawHashCount
                else
                    emitNameToken text startOffset
                    endIndex
            else
                emitNameToken text startOffset
                endIndex

        and scanNumber startOffset =
            match SyntaxFacts.scanNumericToken lineText startOffset with
            | Some(SyntaxFacts.ValidNumericToken(kind, endIndex)) ->
                emit kind (lineText.Substring(startOffset, endIndex - startOffset)) startOffset
                endIndex
            | Some(SyntaxFacts.InvalidNumericToken(error, endIndex)) ->
                diagnostics.AddError(DiagnosticFact.lexer (LexerDiagnosticEvidence.MalformedNumericLiteral error),
                    source.GetLocation(TextSpan.FromBounds(lineStart + startOffset, lineStart + endIndex))
                )

                emit BadToken (lineText.Substring(startOffset, endIndex - startOffset)) startOffset
                endIndex
            | None ->
                emit IntegerLiteral (lineText.Substring(startOffset, 1)) startOffset
                startOffset + 1

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
                        diagnostics.AddError(DiagnosticFact.lexer (LexerDiagnosticEvidence.TabCharacterNotPermitted false),
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
                    | '<' when currentIndex + 1 < lineText.Length && lineText[currentIndex + 1] = '[' ->
                        emit LeftEffectRow "<[" currentIndex
                        currentIndex <- currentIndex + 2
                    | ']' when currentIndex + 1 < lineText.Length && lineText[currentIndex + 1] = '>' ->
                        emit RightEffectRow "]>" currentIndex
                        currentIndex <- currentIndex + 2
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
                    | '.' when currentIndex + 2 < lineText.Length
                             && lineText[currentIndex + 1] = '.'
                             && lineText[currentIndex + 2] = '<' ->
                        emit Operator "..<" currentIndex
                        currentIndex <- currentIndex + 3
                    | '.' when currentIndex + 1 < lineText.Length && lineText[currentIndex + 1] = '.' ->
                        emit Operator ".." currentIndex
                        currentIndex <- currentIndex + 2
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
                        currentIndex <- scanStringLiteral currentIndex
                    | '#' when
                        let hashCount = countLeadingHashes lineText currentIndex
                        currentIndex + hashCount < lineText.Length && lineText[currentIndex + hashCount] = '"' ->
                        currentIndex <- scanStringLiteral currentIndex
                    | '\'' ->
                        if startsSyntaxQuote lineText currentIndex then
                            emit Operator "'" currentIndex
                            currentIndex <- currentIndex + 1
                        else
                            currentIndex <-
                                emitQuotedLiteral
                                    CharacterLiteral
                                    currentIndex
                                    '\''
                                    (DiagnosticFact.lexer LexerDiagnosticEvidence.UnterminatedCharacterLiteral)
                    | '`' ->
                        currentIndex <- scanBacktickOrPrefixedString currentIndex
                    | _ when Char.IsDigit(current) ->
                        currentIndex <- scanNumber currentIndex
                    | _ when SyntaxFacts.isIdentifierStart current ->
                        currentIndex <- scanNameOrPrefixedString currentIndex
                    | _ when SyntaxFacts.isOperatorCharacter current ->
                        currentIndex <- scanOperator currentIndex
                    | _ ->
                        diagnostics.AddError(DiagnosticFact.lexer (LexerDiagnosticEvidence.UnrecognizedCharacter(string current)),
                            source.GetLocation(TextSpan.FromBounds(lineStart + currentIndex, lineStart + currentIndex + 1))
                        )

                        tokens.Add(token BadToken (string current) (lineStart + currentIndex))
                        currentIndex <- currentIndex + 1

            if blockCommentDepth > 0 || not terminated then
                diagnostics.AddError(DiagnosticFact.lexer LexerDiagnosticEvidence.UnterminatedStringInterpolation,
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
                    diagnostics.AddError(
                        DiagnosticFact.lexer (LexerDiagnosticEvidence.TabCharacterNotPermitted false),
                        source.GetLocation(TextSpan.FromBounds(lineStart + index, lineStart + index + 1))
                    )
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
                | '<' when index + 1 < lineText.Length && lineText[index + 1] = '[' ->
                    emit LeftEffectRow "<[" index
                    delimiterDepth <- delimiterDepth + 1
                    index <- index + 2
                | ']' when index + 1 < lineText.Length && lineText[index + 1] = '>' ->
                    emit RightEffectRow "]>" index
                    delimiterDepth <- max 0 (delimiterDepth - 1)
                    index <- index + 2
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
                | '.' when index + 2 < lineText.Length
                         && lineText[index + 1] = '.'
                         && lineText[index + 2] = '<' ->
                    emit Operator "..<" index
                    index <- index + 3
                | '.' when index + 1 < lineText.Length && lineText[index + 1] = '.' ->
                    emit Operator ".." index
                    index <- index + 2
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
                    index <- scanStringLiteral index
                | '#' when
                    let hashCount = countLeadingHashes lineText index
                    index + hashCount < lineText.Length && lineText[index + hashCount] = '"' ->
                    index <- scanStringLiteral index
                | '\'' ->
                    if startsSyntaxQuote lineText index then
                        emit Operator "'" index
                        index <- index + 1
                    else
                        index <-
                            emitQuotedLiteral
                                CharacterLiteral
                                index
                                '\''
                                (DiagnosticFact.lexer LexerDiagnosticEvidence.UnterminatedCharacterLiteral)
                | '`' ->
                    index <- scanBacktickOrPrefixedString index
                | _ when Char.IsDigit(current) ->
                    index <- scanNumber index
                | _ when SyntaxFacts.isIdentifierStart current ->
                    index <- scanNameOrPrefixedString index
                | _ when SyntaxFacts.isOperatorCharacter current ->
                    index <- scanOperator index
                | _ ->
                    diagnostics.AddError(DiagnosticFact.lexer (LexerDiagnosticEvidence.UnrecognizedCharacter(string current)),
                        source.GetLocation(TextSpan.FromBounds(lineStart + index, lineStart + index + 1))
                    )
                    tokens.Add(token BadToken (string current) (lineStart + index))
                    index <- index + 1

        if emittedCode then
            tokens.Add(zeroLengthToken Newline (lineStart + lineText.Length))

        blockCommentDepth, delimiterDepth, consumedUntilAbsoluteIndex

    let tokenize (source: SourceText) =
        let diagnostics = DiagnosticBag()
        let tokens = ResizeArray<Token>()
        let indentStack = Stack<int>()
        indentStack.Push(0)

        let mutable blockCommentDepth = 0
        let mutable delimiterDepth = 0
        let mutable consumedUntilAbsoluteIndex = 0

        for lineIndex in 0 .. source.LineCount - 1 do
            let lineStart = source.LineStarts[lineIndex]
            let lineText = source.GetLineText(lineIndex)
            let startOffset =
                if consumedUntilAbsoluteIndex > lineStart then
                    min lineText.Length (consumedUntilAbsoluteIndex - lineStart)
                else
                    0

            let hasCode =
                startOffset = 0
                && lineContainsCode lineText blockCommentDepth

            if hasCode && blockCommentDepth = 0 && delimiterDepth = 0 then
                let indent = countLeadingSpaces source lineIndex lineText diagnostics
                addIndentationTokens source lineIndex indent indentStack tokens diagnostics

            let newBlockDepth, newDelimiterDepth, newConsumedUntilAbsoluteIndex =
                tokenizeLine source lineIndex lineText blockCommentDepth delimiterDepth startOffset tokens diagnostics

            blockCommentDepth <- newBlockDepth
            delimiterDepth <- newDelimiterDepth
            consumedUntilAbsoluteIndex <- max consumedUntilAbsoluteIndex newConsumedUntilAbsoluteIndex

        if blockCommentDepth > 0 then
            diagnostics.AddError(DiagnosticFact.lexer LexerDiagnosticEvidence.UnterminatedBlockComment,
                source.GetLocation(TextSpan.FromBounds(source.Length, source.Length))
            )

        while indentStack.Count > 1 do
            indentStack.Pop() |> ignore
            tokens.Add(zeroLengthToken Dedent source.Length)

        tokens.Add(zeroLengthToken EndOfFile source.Length)

        { Tokens = List.ofSeq tokens
          Diagnostics = diagnostics.Items }
