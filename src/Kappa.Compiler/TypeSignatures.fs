namespace Kappa.Compiler

open System
open System.Collections.Generic

module TypeSignatures =
    type TypeExpr =
        | TypeName of string list * TypeExpr list
        | TypeVariable of string
        | TypeArrow of TypeExpr * TypeExpr

    type TraitConstraint =
        { TraitName: string
          Argument: TypeExpr }

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

        member private _.IsAtEnd = position >= tokenArray.Length

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
                    Some(TypeName(name, existingArguments @ List.ofSeq arguments))
                | _ when arguments.Count = 0 ->
                    Some head
                | _ ->
                    None

        member this.ParseType() =
            match this.ParseApplication() with
            | None -> None
            | Some left when this.MatchKind Arrow ->
                this.ParseType()
                |> Option.map (fun right -> TypeArrow(left, right))
            | Some left ->
                Some left

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
                | Some(TypeName(name, [ argument ])) ->
                    Some
                        { TraitName =
                            match name with
                            | [ singleName ] -> singleName
                            | _ -> SyntaxFacts.moduleNameToText name
                          Argument = argument }
                | Some(TypeName([ traitName ], [])) ->
                    Some
                        { TraitName = traitName
                          Argument = TypeName([ "Unit" ], []) }
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
        parser.ParseType()

    let parseScheme tokens =
        let parser = Parser(tokens)
        parser.ParseScheme()

    let functionParts (typeExpr: TypeExpr) =
        let rec loop current parameters =
            match current with
            | TypeArrow(parameterType, resultType) ->
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

        let groups = ResizeArray<Token list>()
        let mutable position = 0

        while position < argumentTokens.Length do
            if argumentTokens[position].Kind = LeftParen then
                let current = ResizeArray<Token>()
                let mutable depth = 0
                let mutable keepReading = true

                while keepReading && position < argumentTokens.Length do
                    let token = argumentTokens[position]
                    position <- position + 1

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

                if current.Count > 0 then
                    groups.Add(List.ofSeq current)
            else
                position <- position + 1

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

    let rec applySubstitution substitution typeExpr =
        match typeExpr with
        | TypeVariable name ->
            substitution
            |> Map.tryFind name
            |> Option.defaultValue typeExpr
        | TypeName(name, arguments) ->
            TypeName(name, arguments |> List.map (applySubstitution substitution))
        | TypeArrow(parameterType, resultType) ->
            TypeArrow(applySubstitution substitution parameterType, applySubstitution substitution resultType)

    let applyConstraintSubstitution substitution (constraintInfo: TraitConstraint) =
        { constraintInfo with
            Argument = applySubstitution substitution constraintInfo.Argument }

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
            | TypeArrow(parameterType, resultType) ->
                loop parameterType || loop resultType

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
                | TypeArrow(leftParameter, leftResult), TypeArrow(rightParameter, rightResult) ->
                    unify substitution ((leftParameter, rightParameter) :: (leftResult, rightResult) :: rest)
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
            | TypeArrow _ ->
                $"({toText current})"

        match typeExpr with
        | TypeArrow(parameterType, resultType) ->
            $"{renderAtom parameterType} -> {toText resultType}"
        | _ ->
            renderAtom typeExpr
