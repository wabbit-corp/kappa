namespace Kappa.Compiler

open Kappa.Compiler.ResourceModel

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
        let significant =
            tokens
            |> List.filter (fun token ->
                match token.Kind with
                | Newline
                | Indent
                | Dedent
                | EndOfFile -> false
                | _ -> true)

        if significant |> List.exists (fun token -> token.Text = "&") then
            Some(ResourceQuantity.Borrow None)
        else
            match significant with
            | token :: _ when token.Text = "1" -> Some ResourceQuantity.one
            | token :: _ when token.Text = "0" -> Some ResourceQuantity.zero
            | token :: _ when token.Text = "omega" -> Some ResourceQuantity.omega
            | _ -> None

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

    let private signatureEntries (document: ParsedDocument) name quantities parameterInout =
        signatureName document.ModuleName name
        |> List.map (fun signatureName ->
            signatureName,
            { Name = signatureName
              ParameterQuantities = quantities
              ParameterInout = parameterInout })

    let collectSignatures (documents: ParsedDocument list) =
        documents
        |> List.collect (fun document ->
            document.Syntax.Declarations
            |> List.choose (function
                | SignatureDeclaration declaration ->
                    let quantities = signatureParameterQuantities declaration.TypeTokens
                    Some(signatureEntries document declaration.Name quantities (quantities |> List.map (fun _ -> false)))
                | ExpectDeclarationNode (ExpectTermDeclaration declaration) ->
                    let quantities = signatureParameterQuantities declaration.TypeTokens
                    Some(signatureEntries document declaration.Name quantities (quantities |> List.map (fun _ -> false)))
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

                        signatureEntries
                            document
                            name
                            quantities
                            (definition.Parameters |> List.map (fun parameter -> parameter.IsInout)))
                | _ -> None)
            |> List.concat)
        |> Map.ofList
