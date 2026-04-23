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

        member private _.TryParseQuantityPrefix(tokens: Token list) =
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
            | head :: rest when Token.isName head ->
                match rest with
                | next :: _ when Token.isName next ->
                    Some(QuantityVariable(SyntaxFacts.trimIdentifierQuotes head.Text), rest)
                | _ ->
                    None
            | _ ->
                None

        member this.ParseCompleteType() =
            match this.ParseType() with
            | Some parsed when this.IsAtEnd ->
                Some parsed
            | _ ->
                None

        member private this.TryParseBinderArrow() =
            let tryParseBinderType (tokens: Token list) =
                let rec findColon depth index =
                    if index >= tokens.Length then
                        None
                    else
                        match tokens[index].Kind with
                        | LeftParen
                        | LeftBracket
                        | LeftBrace
                        | LeftSetBrace ->
                            findColon (depth + 1) (index + 1)
                        | RightParen
                        | RightBracket
                        | RightBrace
                        | RightSetBrace ->
                            findColon (max 0 (depth - 1)) (index + 1)
                        | Colon when depth = 0 ->
                            Some index
                        | _ ->
                            findColon depth (index + 1)

                match findColon 0 0 with
                | Some colonIndex when colonIndex > 0 && colonIndex + 1 < tokens.Length ->
                    let binderTokens = tokens |> List.take colonIndex
                    let typeTokens = tokens |> List.skip (colonIndex + 1)

                    let quantity, nameTokens =
                        match this.TryParseQuantityPrefix binderTokens with
                        | Some(quantity, rest) ->
                            quantity, rest
                        | None ->
                            QuantityOmega, binderTokens

                    match nameTokens with
                    | [ nameToken ] when Token.isName nameToken ->
                        let nestedParser = Parser(typeTokens)

                        nestedParser.ParseCompleteType()
                        |> Option.map (fun parameterType -> quantity, parameterType)
                    | _ ->
                        None
                | _ ->
                    None

            match this.Current with
            | Some { Kind = LeftParen } ->
                let mutable depth = 0
                let mutable index = position
                let mutable closingIndex = None

                while index < tokenArray.Length && closingIndex.IsNone do
                    match tokenArray[index].Kind with
                    | LeftParen ->
                        depth <- depth + 1
                    | RightParen ->
                        depth <- depth - 1

                        if depth = 0 then
                            closingIndex <- Some index
                    | _ ->
                        ()

                    index <- index + 1

                match closingIndex with
                | Some rightParenIndex when rightParenIndex + 1 < tokenArray.Length && tokenArray[rightParenIndex + 1].Kind = Arrow ->
                    let innerTokens =
                        tokenArray[position + 1 .. rightParenIndex - 1]
                        |> Array.toList

                    match tryParseBinderType innerTokens with
                    | Some(quantity, parameterType) ->
                        position <- rightParenIndex + 2

                        this.ParseType()
                        |> Option.map (fun resultType -> TypeArrow(quantity, parameterType, resultType))
                    | None ->
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
                this.Advance() |> ignore

                match this.ParseType() with
                | Some inner when this.MatchKind RightParen -> Some inner
                | _ -> None
            | _ ->
                this.ParseQualifiedName()
                |> Option.map (fun name ->
                    let head =
                        match name with
                        | [ singleName ] -> singleName
                        | _ -> SyntaxFacts.moduleNameToText name

                    if head.Length > 0 && Char.IsLower(head[0]) then
                        TypeVariable head
                    else
                        TypeName(name, []))

        member private this.CanStartAtom() =
            match this.Current with
            | Some { Kind = LeftParen } -> true
            | Some token when Token.isName token -> true
            | _ -> false

        member private this.TryConsumeCaptureSuffix() =
            let isCapturesToken token =
                Token.isName token
                && String.Equals(SyntaxFacts.trimIdentifierQuotes token.Text, "captures", StringComparison.Ordinal)

            match this.Current, this.Peek(1) with
            | Some capturesToken, Some { Kind = LeftParen } when isCapturesToken capturesToken ->
                this.Advance() |> ignore
                this.Advance() |> ignore

                let mutable depth = 1

                while not this.IsAtEnd && depth > 0 do
                    match this.Advance() with
                    | Some { Kind = LeftParen } ->
                        depth <- depth + 1
                    | Some { Kind = RightParen } ->
                        depth <- depth - 1
                    | _ ->
                        ()

                true
            | _ ->
                false

        member private this.ParseApplication() =
            match this.ParseAtom() with
            | None -> None
            | Some head ->
                let arguments = ResizeArray<TypeExpr>()

                while this.CanStartAtom() do
                    match this.ParseAtom() with
                    | Some argument -> arguments.Add(argument)
                    | None -> ()

                match head with
                | TypeName(name, existingArguments) ->
                    let mutable parsed = TypeName(name, existingArguments @ List.ofSeq arguments)

                    while this.TryConsumeCaptureSuffix() do
                        ()

                    Some parsed
                | _ when arguments.Count = 0 ->
                    let mutable parsed = head

                    while this.TryConsumeCaptureSuffix() do
                        ()

                    Some parsed
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

    let definitionallyEqual left right =
        left = right

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
            | TypeArrow _
            | TypeEquality _ ->
                $"({toText current})"

        match typeExpr with
        | TypeArrow(quantity, parameterType, resultType) ->
            match quantity with
            | QuantityOmega ->
                $"{renderAtom parameterType} -> {toText resultType}"
            | _ ->
                $"({Quantity.toSurfaceText quantity} x : {toText parameterType}) -> {toText resultType}"
        | TypeEquality(left, right) ->
            $"{renderAtom left} = {toText right}"
        | _ ->
            renderAtom typeExpr
