namespace Kappa.Compiler

open System
open System.Collections.Generic

// Parses and reasons about textual type signatures used across compiler stages.
module TypeSignatures =
    type TypeExpr =
        | TypeName of string list * TypeExpr list
        | TypeVariable of string
        | TypeArrow of Quantity * TypeExpr * TypeExpr
        | TypeEquality of TypeExpr * TypeExpr
        | TypeCapture of TypeExpr * string list
        | TypeRecord of RecordField list
        | TypeUnion of TypeExpr list

    and RecordField =
        { Name: string
          Quantity: Quantity
          Type: TypeExpr }

    type TraitConstraint =
        { TraitName: string
          Arguments: TypeExpr list }

    type TypeScheme =
        { Forall: string list
          Constraints: TraitConstraint list
          Body: TypeExpr }

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
                    | LeftBrace
                    | LeftSetBrace ->
                        loop (depth + 1) (index + 1)
                    | RightParen
                    | RightBracket
                    | RightBrace
                    | RightSetBrace ->
                        loop (max 0 (depth - 1)) (index + 1)
                    | Colon when depth = 0 ->
                        Some index
                    | _ ->
                        loop depth (index + 1)

            loop 0 0

        member private this.TryParseNamedBinder(tokens: Token list) =
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
                            QuantityOmega, binderTokens

                let typeTokens, nameTokens =
                    match afterQuantity with
                    | head :: rest when Token.isKeyword Keyword.Thunk head ->
                        { head with Kind = Identifier; Text = "Thunk" } :: typeTokens, rest
                    | head :: rest when Token.isKeyword Keyword.Lazy head ->
                        { head with Kind = Identifier; Text = "Need" } :: typeTokens, rest
                    | _ ->
                        typeTokens, afterQuantity

                match nameTokens with
                | [ nameToken ] when Token.isName nameToken ->
                    Some(quantity, SyntaxFacts.trimIdentifierQuotes nameToken.Text, typeTokens)
                | [ { Kind = Underscore } ] ->
                    Some(quantity, "_", typeTokens)
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

                    match this.TryParseNamedBinder innerTokens with
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
            match this.TryParseNamedBinder(tokens) with
            | Some(quantity, name, typeTokens) ->
                let nestedParser = Parser(typeTokens)

                nestedParser.ParseCompleteType()
                |> Option.map (fun fieldType ->
                    { Name = name
                      Quantity = quantity
                      Type = fieldType })
            | None ->
                None

        member private this.TryParseRecordType(innerTokens: Token list) =
            let groups, sawTopLevelComma = this.SplitTopLevelCommaGroups(innerTokens)
            let nonEmptyGroups =
                groups
                |> List.filter (List.isEmpty >> not)

            let fields =
                nonEmptyGroups
                |> List.map this.TryParseRecordField

            if (sawTopLevelComma || List.length nonEmptyGroups = 1) && fields |> List.forall Option.isSome then
                fields
                |> List.choose id
                |> function
                    | [] -> None
                    | parsedFields -> Some(TypeRecord parsedFields)
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

        member private this.ParseQualifiedName() =
            let parseOperatorName () =
                match this.Current, this.Peek(1), this.Peek(2) with
                | Some { Kind = LeftParen }, Some { Kind = Operator; Text = operatorText }, Some { Kind = RightParen } ->
                    this.Advance() |> ignore
                    this.Advance() |> ignore
                    this.Advance() |> ignore
                    Some [ operatorText ]
                | _ ->
                    None

            match parseOperatorName () with
            | Some name -> Some name
            | None ->
                match this.Current with
                | Some token when Token.isName token ->
                    let segments = ResizeArray<string>()
                    segments.Add(SyntaxFacts.trimIdentifierQuotes token.Text)
                    this.Advance() |> ignore

                    let mutable keepReading = true

                    while keepReading do
                        match this.Current, this.Peek(1) with
                        | Some { Kind = Dot }, Some nextToken when Token.isName nextToken ->
                            this.Advance() |> ignore
                            segments.Add(SyntaxFacts.trimIdentifierQuotes nextToken.Text)
                            this.Advance() |> ignore
                        | _ ->
                            keepReading <- false

                    Some(List.ofSeq segments)
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
            | _ ->
                None

        member private this.ParseGroupedOrAtomicType() =
            match this.Current with
            | Some { Kind = LeftParen } ->
                this.Advance() |> ignore

                if this.MatchKind RightParen then
                    Some(TypeName([ "Unit" ], []))
                else
                    match this.ParseType() with
                    | Some inner when this.MatchKind RightParen -> Some inner
                    | _ -> None
            | _ ->
                this.ParseQualifiedName()
                |> Option.map (fun name ->
                    let terminalSegment = name |> List.last
                    let collapsedName = SyntaxFacts.moduleNameToText name

                    if terminalSegment.Length > 0 && Char.IsLower(terminalSegment[0]) then
                        TypeVariable collapsedName
                    else
                        TypeName(name, []))

        member private this.CanStartAtom() =
            match this.Current with
            | Some { Kind = LeftParen } -> true
            | Some token when Token.isName token ->
                not (String.Equals(SyntaxFacts.trimIdentifierQuotes token.Text, "captures", StringComparison.Ordinal))
            | _ -> false

        member private this.ParsePostfix() =
            match this.ParseAtom() |> Option.orElseWith (fun () -> this.ParseGroupedOrAtomicType()) with
            | None -> None
            | Some head ->
                let mutable parsed = head

                while this.MatchOperator("?") do
                    parsed <- TypeName([ "std"; "prelude"; "Option" ], [ parsed ])

                Some parsed

        member private this.TryParseCaptureSuffix() =
            let isCapturesToken token =
                Token.isName token
                && String.Equals(SyntaxFacts.trimIdentifierQuotes token.Text, "captures", StringComparison.Ordinal)

            match this.Current, this.Peek(1) with
            | Some capturesToken, Some { Kind = LeftParen } when isCapturesToken capturesToken ->
                this.Advance() |> ignore
                this.Advance() |> ignore
                let regions = ResizeArray<string>()
                let mutable keepReading = true
                let mutable parsed = true

                while keepReading && parsed do
                    match this.Current with
                    | Some token when Token.isName token ->
                        regions.Add(SyntaxFacts.trimIdentifierQuotes token.Text)
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

                while this.CanStartAtom() do
                    match this.ParsePostfix() with
                    | Some argument -> arguments.Add(argument)
                    | None -> ()

                let withCaptures parsed =
                    match this.TryParseCaptureSuffix() with
                    | Some captures -> TypeCapture(parsed, captures)
                    | None -> parsed

                match head with
                | TypeName(name, existingArguments) ->
                    TypeName(name, existingArguments @ List.ofSeq arguments)
                    |> withCaptures
                    |> Some
                | _ when arguments.Count = 0 ->
                    head |> withCaptures |> Some
                | _ ->
                    None

        member private this.ParseArrow() =
            match this.TryParseBinderArrow() with
            | Some arrow ->
                Some arrow
            | None ->
                match this.ParseApplication() with
                | None -> None
                | Some left when this.MatchKind Arrow ->
                    this.ParseType()
                    |> Option.map (fun right -> TypeArrow(QuantityOmega, left, right))
                | Some left ->
                    Some left

        member this.ParseType() =
            match this.ParseArrow() with
            | Some left when this.MatchKind Equals ->
                this.ParseType()
                |> Option.map (fun right -> TypeEquality(left, right))
            | some ->
                some

        member this.ParseScheme() =
            let parseForall () =
                match this.Current with
                | Some token when Token.isKeyword Keyword.Forall token ->
                    this.Advance() |> ignore

                    let variables = ResizeArray<string>()
                    let mutable keepReading = true

                    while keepReading do
                        match this.Current with
                        | Some token when Token.isName token ->
                            variables.Add(SyntaxFacts.trimIdentifierQuotes token.Text)
                            this.Advance() |> ignore
                        | Some { Kind = LeftParen } ->
                            match this.FindMatchingIndex(LeftParen, RightParen, position) with
                            | Some rightParenIndex ->
                                let binderTokens =
                                    tokenArray[position + 1 .. rightParenIndex - 1]
                                    |> Array.toList

                                position <- rightParenIndex + 1

                                match this.TryParseNamedBinder binderTokens with
                                | Some(_, binderName, _) ->
                                    variables.Add(binderName)
                                | None ->
                                    keepReading <- false
                            | None ->
                                keepReading <- false
                        | Some { Kind = Dot } ->
                            this.Advance() |> ignore
                            keepReading <- false
                        | _ ->
                            keepReading <- false

                    List.ofSeq variables
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
                        { TraitName =
                            match name with
                            | [ singleName ] -> singleName
                            | _ -> SyntaxFacts.moduleNameToText name
                          Arguments = arguments }
                | _ ->
                    None

            let parseConstraints () =
                if not (containsConstraintArrow 0 position) then
                    []
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

                    let splitConstraints =
                        let groups = ResizeArray<Token list>()
                        let currentGroup = ResizeArray<Token>()
                        let mutable nestedDepth = 0

                        for token in constraintTokens do
                            match token.Kind with
                            | LeftParen ->
                                nestedDepth <- nestedDepth + 1
                                if nestedDepth > 1 then
                                    currentGroup.Add(token)
                            | RightParen ->
                                if nestedDepth > 1 then
                                    currentGroup.Add(token)

                                nestedDepth <- max 0 (nestedDepth - 1)
                            | Comma when nestedDepth = 0 ->
                                if currentGroup.Count > 0 then
                                    groups.Add(List.ofSeq currentGroup)
                                    currentGroup.Clear()
                            | _ ->
                                currentGroup.Add(token)

                        if currentGroup.Count > 0 then
                            groups.Add(List.ofSeq currentGroup)

                        groups
                        |> Seq.toList

                    splitConstraints
                    |> List.choose parseConstraint

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

        significant
        |> List.takeWhile (fun token -> token.Kind <> Colon)
        |> List.choose (fun token ->
            if Token.isName token then
                Some(SyntaxFacts.trimIdentifierQuotes token.Text)
            else
                None)

    let private constructorFieldTokens (tokens: Token list) =
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
            match significant with
            | leftToken :: operatorToken :: rightToken :: rest
                when leftToken.Kind = LeftParen && operatorToken.Kind = Operator && rightToken.Kind = RightParen ->
                rest
            | _ :: rest ->
                rest
            | [] ->
                []

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

        groups
        |> Seq.toList
        |> List.choose (fun groupTokens ->
            match groupTokens |> List.tryFindIndex (fun token -> token.Kind = Colon) with
            | Some colonIndex when colonIndex + 1 < groupTokens.Length ->
                Some(groupTokens |> List.skip (colonIndex + 1))
            | _ when not (List.isEmpty groupTokens) ->
                Some groupTokens
            | _ ->
                None)

    let constructorFieldTypes (constructor: DataConstructor) =
        constructorFieldTokens constructor.Tokens
        |> List.choose parseType

    let constructorFieldTokenGroups (constructor: DataConstructor) =
        constructorFieldTokens constructor.Tokens

    let rec applySubstitution substitution typeExpr =
        match typeExpr with
        | TypeVariable name ->
            substitution
            |> Map.tryFind name
            |> Option.defaultValue typeExpr
        | TypeName(name, arguments) ->
            TypeName(name, arguments |> List.map (applySubstitution substitution))
        | TypeArrow(quantity, parameterType, resultType) ->
            TypeArrow(quantity, applySubstitution substitution parameterType, applySubstitution substitution resultType)
        | TypeEquality(left, right) ->
            TypeEquality(applySubstitution substitution left, applySubstitution substitution right)
        | TypeCapture(inner, captures) ->
            TypeCapture(applySubstitution substitution inner, captures)
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

    let applySchemeSubstitution substitution (scheme: TypeScheme) =
        { scheme with
            Constraints = scheme.Constraints |> List.map (applyConstraintSubstitution substitution)
            Body = applySubstitution substitution scheme.Body }

    let private occurs name typeExpr =
        let rec loop current =
            match current with
            | TypeVariable currentName ->
                String.Equals(currentName, name, StringComparison.Ordinal)
            | TypeName(_, arguments) ->
                arguments |> List.exists loop
            | TypeArrow(_, parameterType, resultType) ->
                loop parameterType || loop resultType
            | TypeEquality(left, right) ->
                loop left || loop right
            | TypeCapture(inner, _) ->
                loop inner
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
                | TypeVariable leftName, TypeVariable rightName when String.Equals(leftName, rightName, StringComparison.Ordinal) ->
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
                | TypeArrow(leftQuantity, leftParameter, leftResult), TypeArrow(rightQuantity, rightParameter, rightResult)
                    when leftQuantity = rightQuantity ->
                    unify substitution ((leftParameter, rightParameter) :: (leftResult, rightResult) :: rest)
                | TypeEquality(leftLeft, leftRight), TypeEquality(rightLeft, rightRight) ->
                    unify substitution ((leftLeft, rightLeft) :: (leftRight, rightRight) :: rest)
                | TypeCapture(leftInner, leftCaptures), TypeCapture(rightInner, rightCaptures)
                    when (leftCaptures |> List.distinct |> List.sort) = (rightCaptures |> List.distinct |> List.sort) ->
                    unify substitution ((leftInner, rightInner) :: rest)
                | TypeRecord leftFields, TypeRecord rightFields ->
                    let normalizeFieldList fields =
                        fields
                        |> List.sortBy (fun field -> field.Name)

                    let normalizedLeftFields = normalizeFieldList leftFields
                    let normalizedRightFields = normalizeFieldList rightFields

                    if List.length normalizedLeftFields = List.length normalizedRightFields
                       && List.forall2
                           (fun leftField rightField ->
                               String.Equals(leftField.Name, rightField.Name, StringComparison.Ordinal)
                               && leftField.Quantity = rightField.Quantity)
                           normalizedLeftFields
                           normalizedRightFields then
                        let pendingFields =
                            List.zip normalizedLeftFields normalizedRightFields
                            |> List.map (fun (leftField, rightField) -> leftField.Type, rightField.Type)

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

    let private renameVariables prefix index names =
        names
        |> List.mapi (fun offset name -> name, TypeVariable($"{prefix}{index + offset}"))
        |> Map.ofList

    let instantiate prefix nextId (scheme: TypeScheme) =
        let substitution = renameVariables prefix nextId scheme.Forall
        applySchemeSubstitution substitution scheme

    let private canonicalCaptureSet captures =
        captures
        |> List.distinct
        |> List.sort

    let private tryFieldDependencyReference (fieldNames: Set<string>) (referenceName: string) =
        let direct =
            if Set.contains referenceName fieldNames then
                Some referenceName
            else
                None

        direct
        |> Option.orElseWith (fun () ->
            if referenceName.StartsWith("this.", StringComparison.Ordinal) then
                let suffix = referenceName.Substring("this.".Length)

                if Set.contains suffix fieldNames then
                    Some suffix
                else
                    None
            else
                None)

    let private canonicalRecordFields fields =
        let fieldNames =
            fields
            |> List.map (fun field -> field.Name)
            |> Set.ofList

        let rec collectDependencies typeExpr =
            match typeExpr with
            | TypeVariable name ->
                tryFieldDependencyReference fieldNames name |> Option.toList |> Set.ofList
            | TypeName(_, arguments) ->
                arguments
                |> List.fold (fun state argument -> Set.union state (collectDependencies argument)) Set.empty
            | TypeArrow(_, parameterType, resultType) ->
                Set.union (collectDependencies parameterType) (collectDependencies resultType)
            | TypeEquality(left, right) ->
                Set.union (collectDependencies left) (collectDependencies right)
            | TypeCapture(inner, _) ->
                collectDependencies inner
            | TypeRecord nestedFields ->
                nestedFields
                |> List.fold (fun state field -> Set.union state (collectDependencies field.Type)) Set.empty
            | TypeUnion members ->
                members
                |> List.fold (fun state memberType -> Set.union state (collectDependencies memberType)) Set.empty

        let fieldMap =
            fields
            |> List.map (fun field -> field.Name, field)
            |> Map.ofList

        let dependencyMap =
            fields
            |> List.map (fun field ->
                field.Name, (collectDependencies field.Type |> Set.remove field.Name))
            |> Map.ofList

        let rec loop remaining resolved ordered =
            if Set.isEmpty remaining then
                List.rev ordered
            else
                let ready =
                    remaining
                    |> Set.filter (fun name ->
                        dependencyMap[name] |> Set.forall (fun dependency -> Set.contains dependency resolved))
                    |> Set.toList
                    |> List.sort

                match ready with
                | next :: _ ->
                    loop (Set.remove next remaining) (Set.add next resolved) (fieldMap[next] :: ordered)
                | [] ->
                    let fallback =
                        remaining
                        |> Set.toList
                        |> List.sort
                        |> List.map (fun name -> fieldMap[name])

                    List.rev ordered @ fallback

        loop fieldNames Set.empty []

    let rec private canonicalize typeExpr =
        match typeExpr with
        | TypeVariable _ ->
            typeExpr
        | TypeName(name, arguments) ->
            TypeName(name, arguments |> List.map canonicalize)
        | TypeArrow(quantity, parameterType, resultType) ->
            TypeArrow(quantity, canonicalize parameterType, canonicalize resultType)
        | TypeEquality(left, right) ->
            TypeEquality(canonicalize left, canonicalize right)
        | TypeCapture(inner, captures) ->
            TypeCapture(canonicalize inner, canonicalCaptureSet captures)
        | TypeRecord fields ->
            TypeRecord(
                fields
                |> List.map (fun field ->
                    { field with
                        Type = canonicalize field.Type })
                |> canonicalRecordFields
            )
        | TypeUnion members ->
            TypeUnion(
                members
                |> List.map canonicalize
                |> List.distinct
                |> List.sortBy (sprintf "%A")
            )

    let definitionallyEqual left right =
        canonicalize left = canonicalize right

    let schemeDefinitionallyEqual left right =
        left.Forall = right.Forall
        && List.length left.Constraints = List.length right.Constraints
        && List.forall2
            (fun leftConstraint rightConstraint ->
                String.Equals(leftConstraint.TraitName, rightConstraint.TraitName, StringComparison.Ordinal)
                && List.length leftConstraint.Arguments = List.length rightConstraint.Arguments
                && List.forall2 definitionallyEqual leftConstraint.Arguments rightConstraint.Arguments)
            left.Constraints
            right.Constraints
        && definitionallyEqual left.Body right.Body

    let rec toText typeExpr =
        let rec renderAtom current =
            match current with
            | TypeName(name, []) ->
                SyntaxFacts.moduleNameToText name
            | TypeName(name, arguments) ->
                let argumentText = arguments |> List.map renderAtom |> String.concat " "
                $"{SyntaxFacts.moduleNameToText name} {argumentText}"
            | TypeVariable name ->
                name
            | TypeCapture _
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
            let capturesText = String.concat ", " captures
            $"{renderAtom inner} captures ({capturesText})"
        | TypeRecord fields ->
            let fieldText =
                fields
                |> List.map (fun field ->
                    let quantityText =
                        match field.Quantity with
                        | QuantityOmega -> ""
                        | quantity -> Quantity.toSurfaceText quantity + " "

                    $"{quantityText}{field.Name} : {toText field.Type}")
                |> String.concat ", "

            $"({fieldText})"
        | TypeUnion members ->
            let memberText = members |> List.map toText |> String.concat " | "
            $"(| {memberText} |)"
        | _ ->
            renderAtom typeExpr

    let collectFreeTypeVariables typeExpr =
        let rec loop bound current =
            match current with
            | TypeVariable name when not (Set.contains name bound) ->
                Set.singleton name
            | TypeVariable _ ->
                Set.empty
            | TypeName(_, arguments) ->
                arguments
                |> List.fold (fun state argument -> Set.union state (loop bound argument)) Set.empty
            | TypeArrow(_, parameterType, resultType) ->
                Set.union (loop bound parameterType) (loop bound resultType)
            | TypeEquality(left, right) ->
                Set.union (loop bound left) (loop bound right)
            | TypeCapture(inner, _) ->
                loop bound inner
            | TypeRecord fields ->
                fields
                |> List.fold (fun state field -> Set.union state (loop bound field.Type)) Set.empty
            | TypeUnion members ->
                members
                |> List.fold (fun state memberType -> Set.union state (loop bound memberType)) Set.empty

        loop Set.empty typeExpr
        |> Set.toList
        |> List.sort
