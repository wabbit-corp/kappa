namespace Kappa.Compiler

open System
open Kappa.Compiler.ResourceModel

// Extracts quantity and resource-signature information from declarations for the checker.
module internal ResourceCheckingSignatures =
    let private matchesKnownType knownType nameSegments =
        CompilerKnownSymbols.KnownTypes.matchesName knownType nameSegments

    let private syntheticIdentifierToken text =
        { Kind = Identifier
          Text = text
          Span = { Start = 0; Length = text.Length } }

    let private signatureName (moduleName: string list option) bindingName =
        let simple = bindingName

        match moduleName with
        | Some segments -> [ simple; $"{SyntaxFacts.moduleNameToText segments}.{simple}" ]
        | None -> [ simple ]

    let private quantityFromSignatureSegment (tokens: Token list) =
        let significant = SignatureTokenAnalysis.significantTokens tokens

        let binderTokens =
            match significant with
            | { Kind = LeftParen } :: rest ->
                match List.rev rest with
                | { Kind = RightParen } :: reversedInner ->
                    reversedInner |> List.rev |> SignatureTokenAnalysis.significantTokens
                | _ ->
                    significant
            | _ ->
                significant

        let trimIdentifier token =
            SyntaxFacts.trimIdentifierQuotes token.Text

        match binderTokens with
        | { Kind = AtSign } :: { Kind = IntegerLiteral; Text = "0" } :: _ ->
            Some ResourceQuantity.zero
        | { Kind = AtSign } :: { Kind = IntegerLiteral; Text = "1" } :: _ ->
            Some ResourceQuantity.one
        | { Kind = AtSign } :: { Kind = Operator; Text = "<=" } :: { Kind = IntegerLiteral; Text = "1" } :: _ ->
            Some ResourceQuantity.atMostOne
        | { Kind = AtSign } :: { Kind = Operator; Text = ">=" } :: { Kind = IntegerLiteral; Text = "1" } :: _ ->
            Some(ResourceQuantity.atLeastOne)
        | { Kind = AtSign } :: head :: _ when Token.isName head && (head.Text = "omega" || head.Text = "ω") ->
            Some ResourceQuantity.omega
        | { Kind = AtSign } :: { Kind = Operator; Text = "&" } :: { Kind = LeftBracket } :: regionToken :: { Kind = RightBracket } :: _
            when Token.isName regionToken ->
            Some(ResourceQuantity.Borrow(Some(trimIdentifier regionToken)))
        | { Kind = AtSign } :: { Kind = Operator; Text = "&" } :: _ ->
            Some(ResourceQuantity.Borrow None)
        | { Kind = Operator; Text = "&" } :: { Kind = LeftBracket } :: regionToken :: { Kind = RightBracket } :: _
            when Token.isName regionToken ->
            Some(ResourceQuantity.Borrow(Some(trimIdentifier regionToken)))
        | { Kind = Operator; Text = "&" } :: _ ->
            Some(ResourceQuantity.Borrow None)
        | { Kind = IntegerLiteral; Text = "1" } :: _ ->
            Some ResourceQuantity.one
        | { Kind = IntegerLiteral; Text = "0" } :: _ ->
            Some ResourceQuantity.zero
        | head :: _ when Token.isName head && (head.Text = "omega" || head.Text = "ω") ->
            Some ResourceQuantity.omega
        | { Kind = Operator; Text = "<=" } :: { Kind = IntegerLiteral; Text = "1" } :: _ ->
            Some ResourceQuantity.atMostOne
        | { Kind = Operator; Text = ">=" } :: { Kind = IntegerLiteral; Text = "1" } :: _ ->
            Some(ResourceQuantity.atLeastOne)
        | _ ->
            None

    let private signatureParameterQuantitiesFromScheme (scheme: TypeSignatures.TypeScheme) : ResourceQuantity option list =
        let rec loop (current: TypeSignatures.TypeExpr) : ResourceQuantity list =
            match current with
            | TypeSignatures.TypeCapture(inner, _) ->
                loop inner
            | TypeSignatures.TypeArrow(quantity, _, resultType) ->
                ResourceQuantity.ofSurface quantity :: loop resultType
            | _ ->
                []

        scheme.Body |> loop |> List.map Some

    let private signatureParameterQuantities (scheme: TypeSignatures.TypeScheme option) (tokens: Token list) : ResourceQuantity option list =
        match scheme with
        | Some parsedScheme ->
            signatureParameterQuantitiesFromScheme parsedScheme
        | None ->
            let segments =
                tokens
                |> SignatureTokenAnalysis.bindingSignatureBodyTokens
                |> SignatureTokenAnalysis.splitTopLevelArrows
                |> List.filter (List.isEmpty >> not)

            if List.length segments <= 1 then
                []
            else
                segments
                |> List.take (segments.Length - 1)
                |> List.map quantityFromSignatureSegment

    let private signatureParameterTypeTokens tokens =
        let trimSignificant tokens =
            SignatureTokenAnalysis.significantTokens tokens

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
            | { Kind = Operator; Text = "&" } :: { Kind = LeftBracket } :: regionToken :: { Kind = RightBracket } :: rest
                when Token.isName regionToken ->
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
            tokens
            |> SignatureTokenAnalysis.bindingSignatureBodyTokens
            |> SignatureTokenAnalysis.splitTopLevelArrows
            |> List.filter (List.isEmpty >> not)

        if List.length segments <= 1 then
            []
        else
            segments
            |> List.take (segments.Length - 1)
            |> List.map parameterType

    let private signatureParameterNames tokens =
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
            | { Kind = Operator; Text = "&" } :: { Kind = LeftBracket } :: regionToken :: { Kind = RightBracket } :: rest
                when Token.isName regionToken ->
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

        let parameterName segmentTokens =
            let trimmed =
                segmentTokens
                |> stripBinderShell
                |> stripQuantityPrefix

            match trimmed with
            | token :: colonToken :: _ when token.Kind = Underscore && colonToken.Kind = Colon ->
                None
            | token :: colonToken :: _ when Token.isName token && colonToken.Kind = Colon ->
                Some(SyntaxFacts.trimIdentifierQuotes token.Text)
            | receiverToken :: nameToken :: colonToken :: _
                when Token.isName receiverToken
                     && String.Equals(receiverToken.Text, "this", StringComparison.Ordinal)
                     && Token.isName nameToken
                     && colonToken.Kind = Colon ->
                Some(SyntaxFacts.trimIdentifierQuotes nameToken.Text)
            | receiverToken :: colonToken :: _
                when Token.isName receiverToken
                     && String.Equals(receiverToken.Text, "this", StringComparison.Ordinal)
                     && colonToken.Kind = Colon ->
                Some "this"
            | _ ->
                None

        let segments =
            tokens
            |> SignatureTokenAnalysis.bindingSignatureBodyTokens
            |> SignatureTokenAnalysis.splitTopLevelArrows
            |> List.filter (List.isEmpty >> not)

        if List.length segments <= 1 then
            []
        else
            segments
            |> List.take (segments.Length - 1)
            |> List.map parameterName

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
            tokens
            |> SignatureTokenAnalysis.bindingSignatureBodyTokens
            |> SignatureTokenAnalysis.splitTopLevelArrows
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

    let private signatureParameterImplicit tokens =
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
            tokens
            |> SignatureTokenAnalysis.bindingSignatureBodyTokens
            |> SignatureTokenAnalysis.splitTopLevelArrows
            |> List.filter (List.isEmpty >> not)

        if List.length segments <= 1 then
            []
        else
            segments
            |> List.take (segments.Length - 1)
            |> List.map (fun segmentTokens ->
                match stripBinderShell segmentTokens with
                | { Kind = AtSign } :: _ -> true
                | _ -> false)

    let private signatureReturnTypeTokens tokens =
        tokens
        |> SignatureTokenAnalysis.bindingSignatureBodyTokens
        |> SignatureTokenAnalysis.splitTopLevelArrows
        |> List.filter (List.isEmpty >> not)
        |> List.tryLast

    let private signatureEntries
        (document: ParsedDocument)
        (name: string)
        scheme
        parameterNames
        parameterImplicit
        quantities
        parameterTypes
        returnTypeTokens
        parameterInout
        =
        signatureName document.ModuleName name
        |> List.map (fun signatureName ->
            signatureName,
            { Name = signatureName
              Scheme = scheme
              ParameterNames = parameterNames
              ParameterImplicit = parameterImplicit
              ParameterQuantities = quantities
              ParameterTypeTokens = parameterTypes
              ReturnTypeTokens = returnTypeTokens
              ParameterInout = parameterInout })

    let rec private isCompileTimeDefinitionParameterType typeExpr =
        match typeExpr with
        | TypeSignatures.TypeUniverse _
        | TypeSignatures.TypeIntrinsic _ ->
            true
        | TypeSignatures.TypeName(typeReference, []) when TypeSignatures.TypeReference.segments typeReference = [ "Type" ] ->
            true
        | TypeSignatures.TypeName(typeReference, []) when
            matchesKnownType CompilerKnownSymbols.ConstraintType (TypeSignatures.TypeReference.segments typeReference)
            || matchesKnownType CompilerKnownSymbols.QuantityType (TypeSignatures.TypeReference.segments typeReference)
            || matchesKnownType CompilerKnownSymbols.RegionType (TypeSignatures.TypeReference.segments typeReference)
            || matchesKnownType CompilerKnownSymbols.RecRowType (TypeSignatures.TypeReference.segments typeReference)
            || matchesKnownType CompilerKnownSymbols.VarRowType (TypeSignatures.TypeReference.segments typeReference)
            || matchesKnownType CompilerKnownSymbols.EffRowType (TypeSignatures.TypeReference.segments typeReference)
            || matchesKnownType CompilerKnownSymbols.LabelType (TypeSignatures.TypeReference.segments typeReference)
            || matchesKnownType CompilerKnownSymbols.EffLabelType (TypeSignatures.TypeReference.segments typeReference) ->
            true
        | TypeSignatures.TypeArrow(_, parameterType, resultType) ->
            isCompileTimeDefinitionParameterType parameterType
            && isCompileTimeDefinitionParameterType resultType
        | _ ->
            false

    let private stripLeadingCompileTimeDefinitionParameters (parameters: Parameter list) =
        let rec loop (remainingParameters: Parameter list) =
            match remainingParameters with
            | (parameter: Parameter) :: rest ->
                match parameter.TypeTokens |> Option.bind TypeSignatures.parseType with
                | Some parameterType when isCompileTimeDefinitionParameterType parameterType ->
                    loop rest
                | _ ->
                    remainingParameters
            | [] ->
                []

        loop parameters

    let private constructorSignatureEntries (document: ParsedDocument) (declaration: DataDeclaration) =
        let typeParameterNames = TypeSignatures.collectLeadingTypeParameters declaration.HeaderTokens

        let returnTypeTokens =
            Some(
                syntheticIdentifierToken declaration.Name
                :: (typeParameterNames
                    |> List.map TypeSignatures.TypeVariableName.text
                    |> List.map syntheticIdentifierToken)
            )

        declaration.Constructors
        |> List.map (fun constructor ->
            let quantities, parameterTypes =
                match constructor.Parameters with
                | Some parameters ->
                    parameters
                    |> List.map (fun parameter -> parameter.ParameterQuantity |> Option.map ResourceQuantity.ofSurface)
                    ,
                    (parameters |> List.map (fun parameter -> Some parameter.ParameterTypeTokens))
                | None ->
                    let parameterGroups = TypeSignatures.constructorParameterTokenGroups constructor
                    let parameterTypes = TypeSignatures.constructorFieldTokenGroups constructor
                    parameterGroups |> List.map quantityFromSignatureSegment, parameterTypes |> List.map Some

            let parameterInout = quantities |> List.map (fun _ -> false)
            let parameterNames =
                match constructor.Parameters with
                | Some parameters -> parameters |> List.map (fun parameter -> parameter.ParameterName)
                | None -> parameterTypes |> List.map (fun _ -> None)

            signatureEntries
                document
                constructor.Name
                None
                parameterNames
                (parameterTypes |> List.map (fun _ -> false))
                quantities
                parameterTypes
                returnTypeTokens
                parameterInout)
        |> List.concat

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

    let private mergeParameterImplicit existing next =
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

    let private mergeParameterNames existing next =
        let length = max (List.length existing) (List.length next)

        [ 0 .. length - 1 ]
        |> List.map (fun index ->
            match List.tryItem index next with
            | Some(Some name) ->
                Some name
            | Some None ->
                List.tryItem index existing |> Option.defaultValue None
            | None ->
                List.tryItem index existing |> Option.defaultValue None)

    let private mergeSignature existing next =
        { next with
            Scheme = next.Scheme |> Option.orElse existing.Scheme
            ParameterNames =
                mergeParameterNames existing.ParameterNames next.ParameterNames
            ParameterImplicit =
                mergeParameterImplicit existing.ParameterImplicit next.ParameterImplicit
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
                    let scheme = TypeSignatures.parseScheme declaration.TypeTokens
                    let parameterNames = signatureParameterNames declaration.TypeTokens
                    let parameterImplicit = signatureParameterImplicit declaration.TypeTokens
                    let quantities = signatureParameterQuantities scheme declaration.TypeTokens
                    let parameterTypes = signatureParameterTypeTokens declaration.TypeTokens
                    let returnTypeTokens = signatureReturnTypeTokens declaration.TypeTokens
                    let parameterInout = signatureParameterInout declaration.TypeTokens
                    Some(
                        signatureEntries
                            document
                            declaration.Name
                            scheme
                            parameterNames
                            parameterImplicit
                            quantities
                            parameterTypes
                            returnTypeTokens
                            parameterInout
                    )
                | ExpectDeclarationNode (ExpectTermDeclaration declaration) ->
                    let scheme = TypeSignatures.parseScheme declaration.TypeTokens
                    let parameterNames = signatureParameterNames declaration.TypeTokens
                    let parameterImplicit = signatureParameterImplicit declaration.TypeTokens
                    let quantities = signatureParameterQuantities scheme declaration.TypeTokens
                    let parameterTypes = signatureParameterTypeTokens declaration.TypeTokens
                    let returnTypeTokens = signatureReturnTypeTokens declaration.TypeTokens

                    Some(
                        signatureEntries
                            document
                            declaration.Name
                            scheme
                            parameterNames
                            parameterImplicit
                            quantities
                            parameterTypes
                            returnTypeTokens
                            (quantities |> List.map (fun _ -> false))
                    )
                | LetDeclaration definition ->
                    definition.Name
                    |> Option.map (fun name ->
                        let runtimeParameters =
                            definition.Parameters |> stripLeadingCompileTimeDefinitionParameters

                        let parameterNames =
                            runtimeParameters
                            |> List.map (fun (parameter: Parameter) -> Some parameter.Name)

                        let parameterImplicit =
                            runtimeParameters
                            |> List.map (fun (parameter: Parameter) -> parameter.IsImplicit)

                        let quantities =
                            runtimeParameters
                            |> List.map (fun (parameter: Parameter) ->
                                if parameter.IsInout then
                                    Some ResourceQuantity.one
                                else
                                    parameter.Quantity |> Option.map ResourceQuantity.ofSurface)

                        let parameterTypes =
                            runtimeParameters
                            |> List.map (fun (parameter: Parameter) -> parameter.TypeTokens)

                        signatureEntries
                            document
                            name
                            None
                            parameterNames
                            parameterImplicit
                            quantities
                            parameterTypes
                            definition.ReturnTypeTokens
                            (runtimeParameters |> List.map (fun parameter -> parameter.IsInout)))
                | DataDeclarationNode declaration ->
                    Some(constructorSignatureEntries document declaration)
                | _ -> None)
            |> List.concat)
        |> List.fold (fun signatures (name, signature) ->
            signatures
            |> Map.change name (fun existing ->
                match existing with
                | Some current -> Some(mergeSignature current signature)
                | None -> Some signature)) Map.empty
