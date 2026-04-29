namespace Kappa.Compiler

open System
open System.Security.Cryptography
open System.Text

type EffectSemanticOperation =
    { OperationId: string
      Name: string
      ResumptionQuantity: Quantity option
      SignatureTokens: Token list
      ParameterArity: int }

type EffectSemanticDeclaration =
    { VisibleName: string
      InterfaceId: string
      LabelId: string
      HeaderTokens: Token list
      Operations: EffectSemanticOperation list }

module internal EffectSemantics =
    let private stableId (prefix: string) (seed: string) =
        let bytes: byte array = Encoding.UTF8.GetBytes(seed)
        let hash: byte array = SHA256.HashData(bytes)
        let suffix = Convert.ToHexString(hash.AsSpan(0, 16)).ToLowerInvariant()

        $"{prefix}:{suffix}"

    let private topLevelSeed moduleName declarationName =
        $"top|{moduleName}|{declarationName}"

    let private localSeed identitySeed declarationName =
        $"local|{identitySeed}|{declarationName}"

    let private effectOperationId interfaceId operationName =
        stableId "effect-op" $"{interfaceId}|{operationName}"

    let private effectInterfaceId seed =
        stableId "effect-interface" seed

    let private effectLabelId seed =
        stableId "effect-label" seed

    let private sanitizeScopedEffectId (id: string) =
        id.Replace(":", "_", StringComparison.Ordinal).Replace("-", "_", StringComparison.Ordinal)

    let labelNameSegments (declaration: EffectSemanticDeclaration) =
        [ $"__kappa_effect_label_{sanitizeScopedEffectId declaration.LabelId}" ]

    let interfaceNameSegments (declaration: EffectSemanticDeclaration) =
        [ $"__kappa_effect_interface_{sanitizeScopedEffectId declaration.InterfaceId}" ]

    let private effectOperationParameterArity signatureTokens =
        signatureTokens
        |> TypeSignatures.parseType
        |> Option.map TypeSignatures.functionParts
        |> Option.map (fun (parameterTypes, _) -> List.length parameterTypes)
        |> Option.defaultValue 0

    let ensureTopLevel moduleName (declaration: EffectDeclaration) =
        let seed = topLevelSeed moduleName declaration.Name
        let interfaceId = declaration.EffectInterfaceId |> Option.defaultValue (effectInterfaceId seed)
        let labelId = declaration.EffectLabelId |> Option.defaultValue (effectLabelId seed)

        let operations =
            declaration.Operations
            |> List.map (fun operation ->
                { operation with
                    OperationId =
                        operation.OperationId
                        |> Option.orElseWith (fun () -> Some(effectOperationId interfaceId operation.Name)) })

        { declaration with
            EffectInterfaceId = Some interfaceId
            EffectLabelId = Some labelId
            Operations = operations }

    let ensureLocal identitySeed (declaration: EffectDeclaration) =
        let seed = localSeed identitySeed declaration.Name
        let interfaceId = declaration.EffectInterfaceId |> Option.defaultValue (effectInterfaceId seed)
        let labelId = declaration.EffectLabelId |> Option.defaultValue (effectLabelId seed)

        let operations =
            declaration.Operations
            |> List.map (fun operation ->
                { operation with
                    OperationId =
                        operation.OperationId
                        |> Option.orElseWith (fun () -> Some(effectOperationId interfaceId operation.Name)) })

        { declaration with
            EffectInterfaceId = Some interfaceId
            EffectLabelId = Some labelId
            Operations = operations }

    let toSemantic (declaration: EffectDeclaration) =
        let interfaceId =
            declaration.EffectInterfaceId
            |> Option.defaultWith (fun () ->
                invalidOp $"Effect declaration '{declaration.Name}' is missing an interface identity.")

        let labelId =
            declaration.EffectLabelId
            |> Option.defaultWith (fun () ->
                invalidOp $"Effect declaration '{declaration.Name}' is missing a label identity.")

        { VisibleName = declaration.Name
          InterfaceId = interfaceId
          LabelId = labelId
          HeaderTokens = declaration.HeaderTokens
          Operations =
            declaration.Operations
            |> List.map (fun operation ->
                { OperationId =
                    operation.OperationId
                    |> Option.defaultWith (fun () ->
                        invalidOp $"Effect operation '{declaration.Name}.{operation.Name}' is missing an operation identity.")
                  Name = operation.Name
                  ResumptionQuantity = operation.ResumptionQuantity
                  SignatureTokens = operation.SignatureTokens
                  ParameterArity = effectOperationParameterArity operation.SignatureTokens }) }

    let private rewriteLetDefinition rewriteExpression (definition: LetDefinition) =
        { definition with
            Body = definition.Body |> Option.map rewriteExpression }

    let private rewriteProjectionBody rewriteExpression projectionBody =
        let rec rewrite current =
            match current with
            | ProjectionYield expression ->
                ProjectionYield(rewriteExpression expression)
            | ProjectionIfThenElse(condition, whenTrue, whenFalse) ->
                ProjectionIfThenElse(rewriteExpression condition, rewrite whenTrue, rewrite whenFalse)
            | ProjectionMatch(scrutinee, cases) ->
                ProjectionMatch(
                    rewriteExpression scrutinee,
                    cases
                    |> List.map (fun caseClause ->
                        { caseClause with
                            Guard = caseClause.Guard |> Option.map rewriteExpression
                            Body = rewrite caseClause.Body })
                )
            | ProjectionAccessors clauses ->
                ProjectionAccessors(
                    clauses
                    |> List.map (function
                        | ProjectionGet expression -> ProjectionGet(rewriteExpression expression)
                        | ProjectionInout expression -> ProjectionInout(rewriteExpression expression)
                        | ProjectionSet(parameterName, typeTokens, body) ->
                            ProjectionSet(parameterName, typeTokens, rewriteExpression body)
                        | ProjectionSink expression -> ProjectionSink(rewriteExpression expression))
                )

        rewrite projectionBody

    let private rewriteDataConstructorParameters rewriteExpression parameters =
        parameters
        |> List.map (fun parameter ->
            { parameter with
                DefaultValue = parameter.DefaultValue |> Option.map rewriteExpression })

    let assignDocumentEffectIdentities (document: ParsedDocument) =
        let moduleIdentitySeed =
            document.ModuleName
            |> Option.map SyntaxFacts.moduleNameToText
            |> Option.defaultValue document.Source.FilePath

        let localEffectCounter = ref 0

        let nextLocalEffectSeed () =
            let current = !localEffectCounter
            localEffectCounter := current + 1
            $"{document.Source.FilePath}|local-effect|{current}"

        let rec rewriteExpression expression =
            match expression with
            | LocalScopedEffect(declaration, body) ->
                let rewrittenDeclaration = ensureLocal (nextLocalEffectSeed ()) declaration
                LocalScopedEffect(rewrittenDeclaration, rewriteExpression body)
            | LocalLet(binding, value, body) ->
                LocalLet(binding, rewriteExpression value, rewriteExpression body)
            | LocalSignature(declaration, body) ->
                LocalSignature(declaration, rewriteExpression body)
            | LocalTypeAlias(declaration, body) ->
                LocalTypeAlias(declaration, rewriteExpression body)
            | Lambda(parameters, body) ->
                Lambda(parameters, rewriteExpression body)
            | IfThenElse(condition, whenTrue, whenFalse) ->
                IfThenElse(rewriteExpression condition, rewriteExpression whenTrue, rewriteExpression whenFalse)
            | Handle(isDeep, label, body, returnClause, operationClauses) ->
                let rewriteClause (clause: SurfaceEffectHandlerClause) =
                    { clause with
                        Body = rewriteExpression clause.Body }

                Handle(
                    isDeep,
                    rewriteExpression label,
                    rewriteExpression body,
                    rewriteClause returnClause,
                    operationClauses |> List.map rewriteClause
                )
            | Match(scrutinee, cases) ->
                Match(
                    rewriteExpression scrutinee,
                    cases
                    |> List.map (fun caseClause ->
                        { caseClause with
                            Guard = caseClause.Guard |> Option.map rewriteExpression
                            Body = rewriteExpression caseClause.Body })
                )
            | RecordLiteral fields ->
                RecordLiteral(fields |> List.map (fun field -> { field with Value = rewriteExpression field.Value }))
            | Seal(value, ascriptionTokens) ->
                Seal(rewriteExpression value, ascriptionTokens)
            | RecordUpdate(receiver, fields) ->
                RecordUpdate(receiver |> rewriteExpression, fields |> List.map (fun field -> { field with Value = rewriteExpression field.Value }))
            | MemberAccess(receiver, segments, arguments) ->
                MemberAccess(rewriteExpression receiver, segments, arguments |> List.map rewriteExpression)
            | SafeNavigation(receiver, navigation) ->
                SafeNavigation(rewriteExpression receiver, { navigation with Arguments = navigation.Arguments |> List.map rewriteExpression })
            | TagTest(receiver, constructorName) ->
                TagTest(rewriteExpression receiver, constructorName)
            | Do statements ->
                let rec rewriteDoStatement statement =
                    match statement with
                    | DoLet(binding, value) -> DoLet(binding, rewriteExpression value)
                    | DoLetQuestion(binding, value, failure) ->
                        let rewrittenFailure =
                            failure
                            |> Option.map (fun block ->
                                { block with
                                    Body = block.Body |> List.map rewriteDoStatement })

                        DoLetQuestion(binding, rewriteExpression value, rewrittenFailure)
                    | DoBind(binding, value) -> DoBind(binding, rewriteExpression value)
                    | DoUsing(binding, value) -> DoUsing(binding, rewriteExpression value)
                    | DoVar(name, value) -> DoVar(name, rewriteExpression value)
                    | DoAssign(name, value) -> DoAssign(name, rewriteExpression value)
                    | DoDefer value -> DoDefer(rewriteExpression value)
                    | DoExpression value -> DoExpression(rewriteExpression value)
                    | DoReturn value -> DoReturn(rewriteExpression value)
                    | DoIf(condition, whenTrue, whenFalse) ->
                        DoIf(rewriteExpression condition, whenTrue |> List.map rewriteDoStatement, whenFalse |> List.map rewriteDoStatement)
                    | DoWhile(condition, body) ->
                        DoWhile(rewriteExpression condition, body |> List.map rewriteDoStatement)

                Do(statements |> List.map rewriteDoStatement)
            | MonadicSplice inner ->
                MonadicSplice(rewriteExpression inner)
            | Apply(callee, arguments) ->
                Apply(rewriteExpression callee, arguments |> List.map rewriteExpression)
            | ExplicitImplicitArgument inner ->
                ExplicitImplicitArgument(rewriteExpression inner)
            | NamedApplicationBlock fields ->
                NamedApplicationBlock(fields |> List.map (fun field -> { field with Value = rewriteExpression field.Value }))
            | InoutArgument inner ->
                InoutArgument(rewriteExpression inner)
            | Unary(operatorName, inner) ->
                Unary(operatorName, rewriteExpression inner)
            | Binary(left, operatorName, right) ->
                Binary(rewriteExpression left, operatorName, rewriteExpression right)
            | Elvis(left, right) ->
                Elvis(rewriteExpression left, rewriteExpression right)
            | Comprehension comprehension ->
                Comprehension { comprehension with Lowered = rewriteExpression comprehension.Lowered }
            | PrefixedString(prefix, parts) ->
                PrefixedString(
                    prefix,
                    parts
                    |> List.map (function
                        | StringText text -> StringText text
                        | StringInterpolation(inner, format) -> StringInterpolation(rewriteExpression inner, format))
                )
            | SyntaxQuote inner ->
                SyntaxQuote(rewriteExpression inner)
            | SyntaxSplice inner ->
                SyntaxSplice(rewriteExpression inner)
            | TopLevelSyntaxSplice inner ->
                TopLevelSyntaxSplice(rewriteExpression inner)
            | CodeQuote inner ->
                CodeQuote(rewriteExpression inner)
            | CodeSplice inner ->
                CodeSplice(rewriteExpression inner)
            | Literal _
            | NumericLiteral _
            | Name _
            | KindQualifiedName _ ->
                expression

        let rewriteTopLevelDeclaration declaration =
            match declaration with
            | LetDeclaration definition ->
                LetDeclaration(rewriteLetDefinition rewriteExpression definition)
            | DataDeclarationNode dataDeclaration ->
                DataDeclarationNode(
                    { dataDeclaration with
                        Constructors =
                            dataDeclaration.Constructors
                            |> List.map (fun constructor ->
                                { constructor with
                                    Parameters =
                                        constructor.Parameters
                                        |> Option.map (rewriteDataConstructorParameters rewriteExpression) }) }
                )
            | EffectDeclarationNode declaration ->
                EffectDeclarationNode(ensureTopLevel moduleIdentitySeed declaration)
            | ProjectionDeclarationNode declaration ->
                ProjectionDeclarationNode(
                    { declaration with
                        Body = declaration.Body |> Option.map (rewriteProjectionBody rewriteExpression) }
                )
            | TraitDeclarationNode declaration ->
                TraitDeclarationNode(
                    { declaration with
                        Members =
                            declaration.Members
                            |> List.map (fun memberDeclaration ->
                                { memberDeclaration with
                                    DefaultDefinition =
                                        memberDeclaration.DefaultDefinition
                                        |> Option.map (rewriteLetDefinition rewriteExpression) }) }
                )
            | InstanceDeclarationNode declaration ->
                InstanceDeclarationNode(
                    { declaration with
                        Members = declaration.Members |> List.map (rewriteLetDefinition rewriteExpression) }
                )
            | ImportDeclaration _
            | FixityDeclarationNode _
            | ExpectDeclarationNode _
            | SignatureDeclaration _
            | TypeAliasNode _
            | UnknownDeclaration _ ->
                declaration

        { document with
            Syntax =
                { document.Syntax with
                    Declarations = document.Syntax.Declarations |> List.map rewriteTopLevelDeclaration } }

    let importAs localName (declaration: EffectDeclaration) =
        { declaration with Name = localName }

    let tryFindOperation operationName (declaration: EffectSemanticDeclaration) =
        declaration.Operations
        |> List.tryFind (fun operation -> String.Equals(operation.Name, operationName, StringComparison.Ordinal))

    let defaultResumptionQuantity quantity =
        quantity |> Option.defaultValue QuantityOne

    let quantityPermitsMultipleResumptions quantity =
        match defaultResumptionQuantity quantity with
        | QuantityOmega
        | QuantityAtLeastOne
        | QuantityVariable _ ->
            true
        | QuantityZero
        | QuantityOne
        | QuantityBorrow _
        | QuantityAtMostOne ->
            false
