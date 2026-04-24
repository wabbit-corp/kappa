namespace Kappa.Compiler

open Kappa.Compiler.ResourceModel

// Extracts quantity and resource-signature information from declarations for the checker.
module internal ResourceCheckingSignatures =
    let private signatureName (moduleName: string list option) bindingName =
        let simple = bindingName

        match moduleName with
        | Some segments -> [ simple; $"{SyntaxFacts.moduleNameToText segments}.{simple}" ]
        | None -> [ simple ]

    let private splitTopLevelArrows (tokens: Token list) =
        let rec loop depth current remaining segments =
            match remaining with
            | [] ->
                List.rev ((List.rev current) :: segments)
            | token :: tail when token.Kind = LeftParen ->
                loop (depth + 1) (token :: current) tail segments
            | token :: tail when token.Kind = RightParen ->
                loop (max 0 (depth - 1)) (token :: current) tail segments
            | token :: tail when token.Kind = Arrow && depth = 0 ->
                loop depth [] tail ((List.rev current) :: segments)
            | token :: tail ->
                loop depth (token :: current) tail segments

        loop 0 [] tokens []

    let private quantityFromSignatureSegment (tokens: Token list) =
        let trimSignificant tokens =
            tokens
            |> List.filter (fun token ->
                match token.Kind with
                | Newline
                | Indent
                | Dedent
                | EndOfFile -> false
                | _ -> true)

        let significant = trimSignificant tokens

        let binderTokens =
            match significant with
            | { Kind = LeftParen } :: rest ->
                match List.rev rest with
                | { Kind = RightParen } :: reversedInner ->
                    reversedInner |> List.rev |> trimSignificant
                | _ ->
                    significant
            | _ ->
                significant

        let binderTokens =
            match binderTokens with
            | { Kind = AtSign } :: rest -> rest
            | _ -> binderTokens

        match binderTokens with
        | token :: _ when token.Text = "&" -> Some(ResourceQuantity.Borrow None)
        | token :: _ when token.Text = "1" -> Some ResourceQuantity.one
        | token :: _ when token.Text = "0" -> Some ResourceQuantity.zero
        | token :: _ when token.Text = "omega" || token.Text = "ω" -> Some ResourceQuantity.omega
        | first :: second :: _ when first.Text = "<=" && second.Text = "1" ->
            Some ResourceQuantity.atMostOne
        | first :: second :: _ when first.Text = ">=" && second.Text = "1" ->
            Some(ResourceQuantity.atLeastOne)
        | _ ->
            None

    let private signatureParameterQuantities tokens =
        let segments =
            splitTopLevelArrows tokens
            |> List.filter (List.isEmpty >> not)

        if List.length segments <= 1 then
            []
        else
            segments
            |> List.take (segments.Length - 1)
            |> List.map quantityFromSignatureSegment

    let private signatureParameterTypeTokens tokens =
        let trimSignificant tokens =
            tokens
            |> List.filter (fun token ->
                match token.Kind with
                | Newline
                | Indent
                | Dedent
                | EndOfFile -> false
                | _ -> true)

        let stripBinderShell tokens =
            match trimSignificant tokens with
            | { Kind = LeftParen } :: rest ->
                match List.rev rest with
                | { Kind = RightParen } :: reversedInner ->
                    reversedInner |> List.rev |> trimSignificant
                | _ ->
                    trimSignificant tokens
            | trimmed ->
                trimmed

        let rec stripQuantityPrefix tokens =
            match tokens with
            | { Kind = AtSign } :: rest ->
                stripQuantityPrefix rest
            | { Kind = IntegerLiteral; Text = "0" } :: rest
            | { Kind = IntegerLiteral; Text = "1" } :: rest ->
                rest
            | { Kind = Operator; Text = "&" } :: rest ->
                rest
            | { Kind = Operator; Text = "<=" } :: { Kind = IntegerLiteral; Text = "1" } :: rest
            | { Kind = Operator; Text = ">=" } :: { Kind = IntegerLiteral; Text = "1" } :: rest ->
                rest
            | head :: rest when Token.isName head && (head.Text = "omega" || head.Text = "ω") ->
                rest
            | other ->
                other

        let parameterType segmentTokens =
            let trimmed =
                segmentTokens
                |> stripBinderShell
                |> stripQuantityPrefix

            match trimmed with
            | nameToken :: colonToken :: typeTokens when (Token.isName nameToken || nameToken.Kind = Underscore) && colonToken.Kind = Colon ->
                Some typeTokens
            | _ when List.isEmpty trimmed ->
                None
            | _ ->
                Some trimmed

        let segments =
            splitTopLevelArrows tokens
            |> List.filter (List.isEmpty >> not)

        if List.length segments <= 1 then
            []
        else
            segments
            |> List.take (segments.Length - 1)
            |> List.map parameterType

    let private signatureParameterInout tokens =
        let trimSignificant tokens =
            tokens
            |> List.filter (fun token ->
                match token.Kind with
                | Newline
                | Indent
                | Dedent
                | EndOfFile -> false
                | _ -> true)

        let stripBinderShell tokens =
            match trimSignificant tokens with
            | { Kind = LeftParen } :: rest ->
                match List.rev rest with
                | { Kind = RightParen } :: reversedInner ->
                    reversedInner |> List.rev |> trimSignificant
                | _ ->
                    trimSignificant tokens
            | trimmed ->
                trimmed

        let segments =
            splitTopLevelArrows tokens
            |> List.filter (List.isEmpty >> not)

        if List.length segments <= 1 then
            []
        else
            segments
            |> List.take (segments.Length - 1)
            |> List.map (fun segmentTokens ->
                match stripBinderShell segmentTokens with
                | { Kind = AtSign } :: rest ->
                    match rest with
                    | token :: _ when Token.isKeyword Keyword.Inout token -> true
                    | _ -> false
                | token :: _ when Token.isKeyword Keyword.Inout token -> true
                | _ -> false)

    let private signatureReturnTypeTokens tokens =
        splitTopLevelArrows tokens
        |> List.filter (List.isEmpty >> not)
        |> List.tryLast

    let private signatureEntries
        (document: ParsedDocument)
        (name: string)
        quantities
        parameterTypes
        returnTypeTokens
        parameterInout
        =
        signatureName document.ModuleName name
        |> List.map (fun signatureName ->
            signatureName,
            { Name = signatureName
              ParameterQuantities = quantities
              ParameterTypeTokens = parameterTypes
              ReturnTypeTokens = returnTypeTokens
              ParameterInout = parameterInout })

    let private mergeParameterQuantities existing next =
        let length = max (List.length existing) (List.length next)

        [ 0 .. length - 1 ]
        |> List.map (fun index ->
            match List.tryItem index next with
            | Some(Some quantity) ->
                Some quantity
            | Some None ->
                List.tryItem index existing |> Option.defaultValue None
            | None ->
                List.tryItem index existing |> Option.defaultValue None)

    let private mergeParameterInout existing next =
        let length = max (List.length existing) (List.length next)

        [ 0 .. length - 1 ]
        |> List.map (fun index ->
            match List.tryItem index next with
            | Some true ->
                true
            | Some false ->
                List.tryItem index existing |> Option.defaultValue false
            | None ->
                List.tryItem index existing |> Option.defaultValue false)

    let private mergeParameterTypes existing next =
        let length = max (List.length existing) (List.length next)

        [ 0 .. length - 1 ]
        |> List.map (fun index ->
            match List.tryItem index next with
            | Some(Some tokens) ->
                Some tokens
            | Some None ->
                List.tryItem index existing |> Option.defaultValue None
            | None ->
                List.tryItem index existing |> Option.defaultValue None)

    let private mergeSignature existing next =
        { next with
            ParameterQuantities =
                mergeParameterQuantities existing.ParameterQuantities next.ParameterQuantities
            ParameterTypeTokens =
                mergeParameterTypes existing.ParameterTypeTokens next.ParameterTypeTokens
            ReturnTypeTokens =
                next.ReturnTypeTokens
                |> Option.orElse existing.ReturnTypeTokens
            ParameterInout =
                mergeParameterInout existing.ParameterInout next.ParameterInout }

    let collectSignatures (documents: ParsedDocument list) =
        documents
        |> List.collect (fun document ->
            document.Syntax.Declarations
            |> List.choose (function
                | SignatureDeclaration declaration ->
                    let quantities = signatureParameterQuantities declaration.TypeTokens
                    let parameterTypes = signatureParameterTypeTokens declaration.TypeTokens
                    let returnTypeTokens = signatureReturnTypeTokens declaration.TypeTokens
                    let parameterInout = signatureParameterInout declaration.TypeTokens
                    Some(signatureEntries document declaration.Name quantities parameterTypes returnTypeTokens parameterInout)
                | ExpectDeclarationNode (ExpectTermDeclaration declaration) ->
                    let quantities = signatureParameterQuantities declaration.TypeTokens
                    let parameterTypes = signatureParameterTypeTokens declaration.TypeTokens
                    let returnTypeTokens = signatureReturnTypeTokens declaration.TypeTokens

                    Some(
                        signatureEntries
                            document
                            declaration.Name
                            quantities
                            parameterTypes
                            returnTypeTokens
                            (quantities |> List.map (fun _ -> false))
                    )
                | LetDeclaration definition ->
                    definition.Name
                    |> Option.map (fun name ->
                        let quantities =
                            definition.Parameters
                            |> List.map (fun (parameter: Parameter) ->
                                if parameter.IsInout then
                                    Some ResourceQuantity.one
                                else
                                    parameter.Quantity |> Option.map ResourceQuantity.ofSurface)

                        let parameterTypes =
                            definition.Parameters
                            |> List.map (fun (parameter: Parameter) -> parameter.TypeTokens)

                        signatureEntries
                            document
                            name
                            quantities
                            parameterTypes
                            definition.ReturnTypeTokens
                            (definition.Parameters |> List.map (fun parameter -> parameter.IsInout)))
                | _ -> None)
            |> List.concat)
        |> List.fold (fun signatures (name, signature) ->
            signatures
            |> Map.change name (fun existing ->
                match existing with
                | Some current -> Some(mergeSignature current signature)
                | None -> Some signature)) Map.empty
