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
    type ResolvedEffectLabel =
        { Declaration: EffectSemanticDeclaration
          CanonicalExpression: SurfaceExpression }

    type ResolvedEffectOperation =
        { Declaration: EffectSemanticDeclaration
          Operation: EffectSemanticOperation
          BaseExpression: SurfaceExpression
          AppliedArguments: SurfaceExpression list }

    type EffectResolutionCallbacks =
        { TryResolveHandledEffectDeclaration: SurfaceExpression -> EffectSemanticDeclaration option
          TryResolveEffectOperationNamePath: string list -> (EffectSemanticDeclaration * EffectSemanticOperation) option }

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

    let labelExpression (declaration: EffectSemanticDeclaration) =
        KindQualifiedName(EffectLabelKind, labelNameSegments declaration)

    let operationExpression (declaration: EffectSemanticDeclaration) (operation: EffectSemanticOperation) =
        MemberAccess(labelExpression declaration, [ operation.Name ], [])

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
            | TypeSyntaxTokens _ ->
                expression
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

    let tryResolveHandledLabel (callbacks: EffectResolutionCallbacks) expression =
        callbacks.TryResolveHandledEffectDeclaration expression
        |> Option.map (fun declaration ->
            { Declaration = declaration
              CanonicalExpression = labelExpression declaration })

    let tryResolveOperationExpression (callbacks: EffectResolutionCallbacks) expression =
        let rec resolve current =
            match current with
            | Apply(callee, arguments) ->
                resolve callee
                |> Option.map (fun resolved ->
                    { resolved with
                        AppliedArguments = resolved.AppliedArguments @ arguments })
            | Name nameSegments ->
                callbacks.TryResolveEffectOperationNamePath nameSegments
                |> Option.map (fun (declaration, operation) ->
                    { Declaration = declaration
                      Operation = operation
                      BaseExpression = operationExpression declaration operation
                      AppliedArguments = [] })
            | MemberAccess(receiver, [ operationName ], []) ->
                callbacks.TryResolveHandledEffectDeclaration receiver
                |> Option.bind (fun declaration ->
                    tryFindOperation operationName declaration
                    |> Option.map (fun operation ->
                        { Declaration = declaration
                          Operation = operation
                          BaseExpression = operationExpression declaration operation
                          AppliedArguments = [] }))
            | Seal(inner, _) ->
                resolve inner
            | _ ->
                None

        resolve expression

    let collectPatternOperationAliases
        (callbacks: EffectResolutionCallbacks)
        pattern
        value
        =
        let rec collectExpression prefix expression aliases =
            match tryResolveOperationExpression callbacks expression with
            | Some resolved ->
                Map.add prefix resolved aliases
            | None ->
                match expression with
                | RecordLiteral fields ->
                    fields
                    |> List.fold (fun current field -> collectExpression (prefix @ [ field.Name ]) field.Value current) aliases
                | Seal(inner, _) ->
                    collectExpression prefix inner aliases
                | _ ->
                    aliases

        let rec collectPattern currentPattern currentValue aliases =
            match currentPattern with
            | NamePattern name ->
                collectExpression [ name ] currentValue aliases
            | AsPattern(name, inner) ->
                collectPattern inner currentValue (collectExpression [ name ] currentValue aliases)
            | TypedPattern(inner, _) ->
                collectPattern inner currentValue aliases
            | AnonymousRecordPattern(fields, rest) ->
                fields
                |> List.fold (fun current field ->
                    let fieldExpression =
                        match currentValue with
                        | Name segments -> Name(segments @ [ field.Name ])
                        | _ -> MemberAccess(currentValue, [ field.Name ], [])

                    collectPattern field.Pattern fieldExpression current) aliases
                |> fun current ->
                    match rest with
                    | Some(BindRecordPatternRest name) ->
                        collectExpression [ name ] currentValue current
                    | _ ->
                        current
            | ConstructorPattern(_, arguments) ->
                arguments |> List.fold (fun current argument -> collectPattern argument currentValue current) aliases
            | NamedConstructorPattern(_, fields) ->
                fields |> List.fold (fun current field -> collectPattern field.Pattern currentValue current) aliases
            | TuplePattern elements ->
                elements |> List.fold (fun current element -> collectPattern element currentValue current) aliases
            | VariantPattern(BoundVariantPattern(name, _))
            | VariantPattern(RestVariantPattern name) ->
                collectExpression [ name ] currentValue aliases
            | VariantPattern(WildcardVariantPattern _) ->
                aliases
            | OrPattern alternatives ->
                alternatives
                |> List.tryHead
                |> Option.map (fun first -> collectPattern first currentValue aliases)
                |> Option.defaultValue aliases
            | WildcardPattern
            | LiteralPattern _ ->
                aliases

        collectPattern pattern value Map.empty

    let rebuildOperationAliasExpression (resolved: ResolvedEffectOperation) =
        match resolved.AppliedArguments with
        | [] -> resolved.BaseExpression
        | arguments -> Apply(resolved.BaseExpression, arguments)

    let rec rewriteOperationAliasUses
        (callbacks: EffectResolutionCallbacks)
        (aliases: Map<string list, ResolvedEffectOperation>)
        expression
        =
        let shadowsAnyAlias names (activeAliases: Map<string list, ResolvedEffectOperation>) =
            if Set.isEmpty names then
                false
            else
                activeAliases
                |> Map.exists (fun segments _ ->
                    match segments with
                    | root :: _ -> Set.contains root names
                    | [] -> false)

        let withoutShadowedAliases names (activeAliases: Map<string list, ResolvedEffectOperation>) =
            if Set.isEmpty names then
                activeAliases
            else
                activeAliases
                |> Map.filter (fun segments _ ->
                    match segments with
                    | root :: _ -> not (Set.contains root names)
                    | [] -> true)

        let tryLookupNameAlias activeAliases segments =
            activeAliases |> Map.tryFind segments

        let tryLookupMemberAlias activeAliases receiver segments =
            match receiver with
            | Name receiverSegments ->
                activeAliases |> Map.tryFind (receiverSegments @ segments)
            | _ ->
                None

        let rec collectPatternNames pattern =
            match pattern with
            | NamePattern name -> [ name ]
            | TypedPattern(inner, _) -> collectPatternNames inner
            | WildcardPattern
            | LiteralPattern _ -> []
            | ConstructorPattern(_, arguments)
            | TuplePattern arguments -> arguments |> List.collect collectPatternNames
            | NamedConstructorPattern(_, fields) -> fields |> List.collect (fun field -> collectPatternNames field.Pattern)
            | AsPattern(name, inner) -> name :: collectPatternNames inner
            | AnonymousRecordPattern(fields, rest) ->
                let fieldNames = fields |> List.collect (fun field -> collectPatternNames field.Pattern)

                match rest with
                | Some(BindRecordPatternRest name) -> name :: fieldNames
                | _ -> fieldNames
            | VariantPattern(BoundVariantPattern(name, _))
            | VariantPattern(RestVariantPattern name) -> [ name ]
            | VariantPattern(WildcardVariantPattern _) -> []
            | OrPattern alternatives ->
                alternatives
                |> List.tryHead
                |> Option.map collectPatternNames
                |> Option.defaultValue []

        let rec rewrite activeAliases current =
            match current with
            | Name segments ->
                tryLookupNameAlias activeAliases segments
                |> Option.map rebuildOperationAliasExpression
                |> Option.defaultValue current
            | SyntaxQuote inner ->
                SyntaxQuote(rewrite activeAliases inner)
            | SyntaxSplice inner ->
                SyntaxSplice(rewrite activeAliases inner)
            | TopLevelSyntaxSplice inner ->
                TopLevelSyntaxSplice(rewrite activeAliases inner)
            | CodeQuote inner ->
                CodeQuote(rewrite activeAliases inner)
            | CodeSplice inner ->
                CodeSplice(rewrite activeAliases inner)
            | TypeSyntaxTokens _ ->
                current
            | Handle(isDeep, label, body, returnClause, operationClauses) ->
                let rewriteClause (clause: SurfaceEffectHandlerClause) =
                    { clause with Body = rewrite activeAliases clause.Body }

                Handle(
                    isDeep,
                    rewrite activeAliases label,
                    rewrite activeAliases body,
                    rewriteClause returnClause,
                    operationClauses |> List.map rewriteClause
                )
            | LocalLet(binding, value, body) ->
                let rewrittenValue = rewrite activeAliases value
                let shadowedNames = collectPatternNames binding.Pattern |> Set.ofList
                let bodyAliases =
                    withoutShadowedAliases shadowedNames activeAliases
                    |> fun currentAliases ->
                        collectPatternOperationAliases callbacks binding.Pattern rewrittenValue
                        |> Map.fold (fun state segments alias -> Map.add segments alias state) currentAliases

                LocalLet(binding, rewrittenValue, rewrite bodyAliases body)
            | LocalSignature(declaration, body) ->
                LocalSignature(declaration, rewrite (withoutShadowedAliases (Set.singleton declaration.Name) activeAliases) body)
            | LocalTypeAlias(declaration, body) ->
                LocalTypeAlias(declaration, rewrite activeAliases body)
            | LocalScopedEffect(declaration, body) ->
                LocalScopedEffect(declaration, rewrite (withoutShadowedAliases (Set.singleton declaration.Name) activeAliases) body)
            | Lambda(parameters, body) ->
                let shadowedNames = parameters |> List.map (fun parameter -> parameter.Name) |> Set.ofList
                Lambda(parameters, rewrite (withoutShadowedAliases shadowedNames activeAliases) body)
            | IfThenElse(condition, whenTrue, whenFalse) ->
                IfThenElse(rewrite activeAliases condition, rewrite activeAliases whenTrue, rewrite activeAliases whenFalse)
            | Match(scrutinee, cases) ->
                Match(
                    rewrite activeAliases scrutinee,
                    cases
                    |> List.map (fun caseClause ->
                        let shadowedNames = collectPatternNames caseClause.Pattern |> Set.ofList
                        let caseAliases = withoutShadowedAliases shadowedNames activeAliases

                        { caseClause with
                            Guard = caseClause.Guard |> Option.map (rewrite caseAliases)
                            Body = rewrite caseAliases caseClause.Body })
                )
            | RecordLiteral fields ->
                RecordLiteral(fields |> List.map (fun field -> { field with Value = rewrite activeAliases field.Value }))
            | Seal(value, ascriptionTokens) ->
                Seal(rewrite activeAliases value, ascriptionTokens)
            | RecordUpdate(receiver, fields) ->
                RecordUpdate(rewrite activeAliases receiver, fields |> List.map (fun field -> { field with Value = rewrite activeAliases field.Value }))
            | MemberAccess(receiver, segments, arguments) ->
                let rewrittenArguments = arguments |> List.map (rewrite activeAliases)

                match tryLookupMemberAlias activeAliases receiver segments with
                | Some resolved ->
                    if List.isEmpty rewrittenArguments then
                        rebuildOperationAliasExpression resolved
                    else
                        Apply(resolved.BaseExpression, resolved.AppliedArguments @ rewrittenArguments)
                | None ->
                    MemberAccess(rewrite activeAliases receiver, segments, rewrittenArguments)
            | SafeNavigation(receiver, navigation) ->
                SafeNavigation(rewrite activeAliases receiver, { navigation with Arguments = navigation.Arguments |> List.map (rewrite activeAliases) })
            | TagTest(receiver, constructorName) ->
                TagTest(rewrite activeAliases receiver, constructorName)
            | Do statements ->
                let rec rewriteStatements currentAliases remaining =
                    match remaining with
                    | [] -> []
                    | statement :: rest ->
                        match statement with
                        | DoLet(binding, value) ->
                            let rewrittenValue = rewrite currentAliases value
                            let shadowedNames = collectPatternNames binding.Pattern |> Set.ofList
                            let restAliases =
                                withoutShadowedAliases shadowedNames currentAliases
                                |> fun aliasesWithoutShadowing ->
                                    collectPatternOperationAliases callbacks binding.Pattern rewrittenValue
                                    |> Map.fold (fun state segments alias -> Map.add segments alias state) aliasesWithoutShadowing

                            DoLet(binding, rewrittenValue) :: rewriteStatements restAliases rest
                        | DoLetQuestion(binding, value, failure) ->
                            let rewrittenValue = rewrite currentAliases value
                            let rewrittenFailure =
                                failure
                                |> Option.map (fun failureClause ->
                                    let shadowedNames = collectPatternNames failureClause.ResiduePattern.Pattern |> Set.ofList

                                    { failureClause with
                                        Body = rewriteStatements (withoutShadowedAliases shadowedNames currentAliases) failureClause.Body })

                            let shadowedNames = collectPatternNames binding.Pattern |> Set.ofList
                            let restAliases =
                                withoutShadowedAliases shadowedNames currentAliases
                                |> fun aliasesWithoutShadowing ->
                                    collectPatternOperationAliases callbacks binding.Pattern rewrittenValue
                                    |> Map.fold (fun state segments alias -> Map.add segments alias state) aliasesWithoutShadowing

                            DoLetQuestion(binding, rewrittenValue, rewrittenFailure) :: rewriteStatements restAliases rest
                        | DoBind(binding, value) ->
                            let rewrittenValue = rewrite currentAliases value
                            let shadowedNames = collectPatternNames binding.Pattern |> Set.ofList
                            DoBind(binding, rewrittenValue) :: rewriteStatements (withoutShadowedAliases shadowedNames currentAliases) rest
                        | DoUsing(binding, value) ->
                            let rewrittenValue = rewrite currentAliases value
                            let shadowedNames = collectPatternNames binding.Pattern |> Set.ofList
                            DoUsing(binding, rewrittenValue) :: rewriteStatements (withoutShadowedAliases shadowedNames currentAliases) rest
                        | DoVar(name, value) ->
                            DoVar(name, rewrite currentAliases value)
                            :: rewriteStatements (withoutShadowedAliases (Set.singleton name) currentAliases) rest
                        | DoAssign(name, value) ->
                            DoAssign(name, rewrite currentAliases value) :: rewriteStatements currentAliases rest
                        | DoDefer value ->
                            DoDefer(rewrite currentAliases value) :: rewriteStatements currentAliases rest
                        | DoIf(condition, whenTrue, whenFalse) ->
                            DoIf(
                                rewrite currentAliases condition,
                                rewriteStatements currentAliases whenTrue,
                                rewriteStatements currentAliases whenFalse
                            )
                            :: rewriteStatements currentAliases rest
                        | DoWhile(condition, body) ->
                            DoWhile(rewrite currentAliases condition, rewriteStatements currentAliases body)
                            :: rewriteStatements currentAliases rest
                        | DoReturn value ->
                            DoReturn(rewrite currentAliases value) :: rest
                        | DoExpression value ->
                            DoExpression(rewrite currentAliases value) :: rewriteStatements currentAliases rest

                Do(rewriteStatements activeAliases statements)
            | MonadicSplice inner ->
                MonadicSplice(rewrite activeAliases inner)
            | ExplicitImplicitArgument inner ->
                ExplicitImplicitArgument(rewrite activeAliases inner)
            | NamedApplicationBlock fields ->
                NamedApplicationBlock(fields |> List.map (fun field -> { field with Value = rewrite activeAliases field.Value }))
            | Apply(Name segments, arguments) ->
                let rewrittenArguments = arguments |> List.map (rewrite activeAliases)

                match tryLookupNameAlias activeAliases segments with
                | Some resolved ->
                    Apply(resolved.BaseExpression, resolved.AppliedArguments @ rewrittenArguments)
                | None ->
                    Apply(Name segments, rewrittenArguments)
            | Apply(MemberAccess(receiver, segments, []), arguments) ->
                let rewrittenArguments = arguments |> List.map (rewrite activeAliases)

                match tryLookupMemberAlias activeAliases receiver segments with
                | Some resolved ->
                    Apply(resolved.BaseExpression, resolved.AppliedArguments @ rewrittenArguments)
                | None ->
                    Apply(MemberAccess(rewrite activeAliases receiver, segments, []), rewrittenArguments)
            | Apply(callee, arguments) ->
                Apply(rewrite activeAliases callee, arguments |> List.map (rewrite activeAliases))
            | InoutArgument inner ->
                InoutArgument(rewrite activeAliases inner)
            | Unary(operatorName, operand) ->
                Unary(operatorName, rewrite activeAliases operand)
            | Binary(left, operatorName, right) ->
                Binary(rewrite activeAliases left, operatorName, rewrite activeAliases right)
            | Elvis(left, right) ->
                Elvis(rewrite activeAliases left, rewrite activeAliases right)
            | Comprehension comprehension ->
                Comprehension { comprehension with Lowered = rewrite activeAliases comprehension.Lowered }
            | PrefixedString(prefix, parts) ->
                PrefixedString(
                    prefix,
                    parts
                    |> List.map (function
                        | StringText _ as part -> part
                        | StringInterpolation(inner, format) -> StringInterpolation(rewrite activeAliases inner, format))
                )
            | Literal _
            | NumericLiteral _
            | KindQualifiedName _ ->
                current

        if Map.isEmpty aliases then
            expression
        else
            rewrite aliases expression

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
