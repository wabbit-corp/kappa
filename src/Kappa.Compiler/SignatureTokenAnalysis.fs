namespace Kappa.Compiler

open System

module internal SignatureTokenAnalysis =
    let significantTokens (tokens: Token list) =
        tokens
        |> List.filter (fun token ->
            match token.Kind with
            | Newline
            | Indent
            | Dedent
            | EndOfFile -> false
            | _ -> true)

    let splitTopLevelArrows (tokens: Token list) =
        let tokenArray = significantTokens tokens |> List.toArray
        let segments = ResizeArray<Token list>()
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
            | Arrow when depth = 0 ->
                segments.Add(List.ofSeq current)
                current.Clear()
            | _ ->
                current.Add(token)

        if current.Count > 0 then
            segments.Add(List.ofSeq current)

        segments |> Seq.toList

    let bindingSignatureBodyTokens (tokens: Token list) =
        let tokenArray = significantTokens tokens |> List.toArray
        let mutable parenDepth = 0
        let mutable braceDepth = 0
        let mutable bracketDepth = 0
        let mutable bodyStart = 0

        for index = 0 to tokenArray.Length - 1 do
            match tokenArray[index].Kind with
            | LeftParen -> parenDepth <- parenDepth + 1
            | RightParen -> parenDepth <- max 0 (parenDepth - 1)
            | LeftBrace
            | LeftSetBrace -> braceDepth <- braceDepth + 1
            | RightBrace
            | RightSetBrace -> braceDepth <- max 0 (braceDepth - 1)
            | LeftBracket -> bracketDepth <- bracketDepth + 1
            | RightBracket -> bracketDepth <- max 0 (bracketDepth - 1)
            | Operator when parenDepth = 0 && braceDepth = 0 && bracketDepth = 0 && String.Equals(tokenArray[index].Text, "=>", StringComparison.Ordinal) ->
                bodyStart <- index + 1
            | Equals when parenDepth = 0 && braceDepth = 0 && bracketDepth = 0 && index + 1 < tokenArray.Length ->
                match tokenArray[index + 1] with
                | { Kind = Operator; Text = ">" } ->
                    bodyStart <- index + 2
                | _ ->
                    ()
            | _ ->
                ()

        let stripLeadingForall startIndex =
            if startIndex >= tokenArray.Length then
                startIndex
            else
                match tokenArray[startIndex] with
                | token when Token.isKeyword Keyword.Forall token ->
                    let mutable index = startIndex + 1
                    let mutable parsed = true
                    let mutable foundDot = false
                    let mutable nextIndex = startIndex

                    while parsed && not foundDot && index < tokenArray.Length do
                        match tokenArray[index] with
                        | token when Token.isName token ->
                            index <- index + 1
                        | { Kind = LeftParen } ->
                            let mutable depth = 1
                            let mutable innerIndex = index + 1

                            while depth > 0 && innerIndex < tokenArray.Length do
                                match tokenArray[innerIndex].Kind with
                                | LeftParen -> depth <- depth + 1
                                | RightParen -> depth <- depth - 1
                                | _ -> ()

                                innerIndex <- innerIndex + 1

                            if depth = 0 then
                                index <- innerIndex
                            else
                                parsed <- false
                        | { Kind = Dot } ->
                            foundDot <- true
                            nextIndex <- index + 1
                        | _ ->
                            parsed <- false

                    if parsed && foundDot then nextIndex else startIndex
                | _ ->
                    startIndex

        let bodyStart = stripLeadingForall bodyStart
        List.ofArray tokenArray[bodyStart..]

    let private binderTokens (tokens: Token list) =
        let significant = significantTokens tokens

        match significant with
        | { Kind = LeftParen } :: rest ->
            match List.rev rest with
            | { Kind = RightParen } :: reversedInner -> significantTokens (List.rev reversedInner)
            | _ -> significant
        | _ ->
            significant

    let parameterHasErasedRuntimeQuantity (tokens: Token list) =
        match binderTokens tokens with
        | { Kind = AtSign } :: { Kind = IntegerLiteral; Text = "0" } :: _ -> true
        | { Kind = IntegerLiteral; Text = "0" } :: _ -> true
        | _ -> false

    let runtimeArityFromTypeTokens tokens =
        let segments =
            tokens
            |> bindingSignatureBodyTokens
            |> splitTopLevelArrows
            |> List.filter (List.isEmpty >> not)

        if List.length segments <= 1 then
            0
        else
            segments
            |> List.take (segments.Length - 1)
            |> List.sumBy (fun segment -> if parameterHasErasedRuntimeQuantity segment then 0 else 1)
