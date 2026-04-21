namespace Kappa.Compiler

open System

module SurfaceElaboration =
    open TypeSignatures

    type private TraitMemberInfo =
        { Name: string
          Scheme: TypeScheme }

    type private TraitInfo =
        { ModuleName: string
          Name: string
          TypeParameters: string list
          Members: Map<string, TraitMemberInfo> }

    type private InstanceInfo =
        { ModuleName: string
          TraitName: string
          InstanceKey: string
          HeadType: TypeExpr
          Members: Map<string, LetDefinition> }

    type private BindingSchemeInfo =
        { ModuleName: string
          Name: string
          Scheme: TypeScheme }

    type private ModuleSurfaceInfo =
        { BindingSchemes: Map<string, BindingSchemeInfo>
          Traits: Map<string, TraitInfo>
          Instances: InstanceInfo list }

    type private BindingLoweringEnvironment =
        { CurrentModuleName: string
          VisibleBindings: Map<string, BindingSchemeInfo>
          VisibleTraits: Map<string, TraitInfo>
          VisibleInstances: InstanceInfo list
          ConstrainedMembers: Map<string, string * string> }

    let private tokensText (tokens: Token list) =
        tokens
        |> List.map (fun token -> token.Text)
        |> String.concat " "

    let private significantTokens (tokens: Token list) =
        tokens
        |> List.filter (fun token ->
            match token.Kind with
            | Newline
            | Indent
            | Dedent
            | EndOfFile -> false
            | _ -> true)

    let private moduleNameText moduleName =
        moduleName
        |> Option.map SyntaxFacts.moduleNameToText
        |> Option.defaultValue "<unknown>"

    let private declarationName (declaration: TopLevelDeclaration) =
        match declaration with
        | SignatureDeclaration declaration -> Some declaration.Name
        | LetDeclaration declaration -> declaration.Name
        | DataDeclarationNode declaration -> Some declaration.Name
        | TypeAliasNode declaration -> Some declaration.Name
        | TraitDeclarationNode declaration -> Some declaration.Name
        | InstanceDeclarationNode declaration -> Some declaration.TraitName
        | ExpectDeclarationNode (ExpectTypeDeclaration declaration) -> Some declaration.Name
        | ExpectDeclarationNode (ExpectTraitDeclaration declaration) -> Some declaration.Name
        | ExpectDeclarationNode (ExpectTermDeclaration declaration) -> Some declaration.Name
        | _ -> None

    let private declarationOrigin filePath moduleName declaration =
        { FilePath = filePath
          ModuleName = moduleName
          DeclarationName = declarationName declaration
          IntroductionKind = "source" }

    let private syntheticOrigin filePath moduleName declarationName introductionKind =
        { FilePath = filePath
          ModuleName = moduleName
          DeclarationName = Some declarationName
          IntroductionKind = introductionKind }

    let private syntheticLetSource name =
        LetDeclaration
            { Visibility = Some Visibility.Private
              IsOpaque = false
              Name = Some name
              Parameters = []
              HeaderTokens = []
              ReturnTypeTokens = None
              BodyTokens = []
              Body = None }

    let private lowerKCoreParameter (name: string) (typeText: string option) =
        { Name = name
          TypeText = typeText }

    let rec private lowerKCorePattern (pattern: CorePattern) =
        match pattern with
        | WildcardPattern ->
            KCoreWildcardPattern
        | NamePattern name ->
            KCoreNamePattern name
        | LiteralPattern literal ->
            KCoreLiteralPattern literal
        | ConstructorPattern(name, arguments) ->
            KCoreConstructorPattern(name, arguments |> List.map lowerKCorePattern)

    let private unitType =
        TypeName([ "Unit" ], [])

    let private boolType =
        TypeName([ "Bool" ], [])

    let private intType =
        TypeName([ "Int" ], [])

    let private floatType =
        TypeName([ "Float" ], [])

    let private stringType =
        TypeName([ "String" ], [])

    let private charType =
        TypeName([ "Char" ], [])

    let private ioType argumentType =
        TypeName([ "IO" ], [ argumentType ])

    let private refType argumentType =
        TypeName([ "Ref" ], [ argumentType ])

    let private dictionaryType traitName argumentType =
        TypeName([ TraitRuntime.dictionaryTypeName traitName ], [ argumentType ])

    let private unwrapIoType typeExpr =
        match typeExpr with
        | TypeName([ "IO" ], [ inner ]) -> inner
        | other -> other

    let private typeTextOf typeExpr =
        typeExpr
        |> unwrapIoType
        |> TypeSignatures.toText

    let private tryParseParameterType (parameter: Parameter) =
        parameter.TypeTokens |> Option.bind TypeSignatures.parseType

    let private tryParseReturnTypeTokens tokens =
        tokens |> Option.bind TypeSignatures.parseType |> Option.map typeTextOf

    let private tryParseTraitMemberInfo (memberDeclaration: TraitMember) =
        let tokens = significantTokens memberDeclaration.Tokens

        match tokens with
        | head :: colon :: rest when Token.isName head && colon.Kind = Colon ->
            TypeSignatures.parseScheme rest
            |> Option.map (fun scheme ->
                let memberName = SyntaxFacts.trimIdentifierQuotes head.Text

                memberName,
                { Name = memberName
                  Scheme = scheme })
        | _ ->
            None

    let private tryParseInstanceHeadType (declaration: InstanceDeclaration) =
        declaration.HeaderTokens |> TypeSignatures.parseType

    let private buildModuleSurfaceInfo (frontendModule: KFrontIRModule) =
        let moduleName = moduleNameText frontendModule.ModuleIdentity

        let bindingSchemes =
            frontendModule.Declarations
            |> List.choose (function
                | SignatureDeclaration declaration ->
                    TypeSignatures.parseScheme declaration.TypeTokens
                    |> Option.map (fun scheme ->
                        declaration.Name,
                        { ModuleName = moduleName
                          Name = declaration.Name
                          Scheme = scheme })
                | ExpectDeclarationNode (ExpectTermDeclaration declaration) ->
                    TypeSignatures.parseScheme declaration.TypeTokens
                    |> Option.map (fun scheme ->
                        declaration.Name,
                        { ModuleName = moduleName
                          Name = declaration.Name
                          Scheme = scheme })
                | _ ->
                    None)
            |> Map.ofList

        let traits =
            frontendModule.Declarations
            |> List.choose (function
                | TraitDeclarationNode declaration ->
                    let members =
                        declaration.Members
                        |> List.choose tryParseTraitMemberInfo
                        |> Map.ofList

                    Some(
                        declaration.Name,
                        { ModuleName = moduleName
                          Name = declaration.Name
                          TypeParameters = TypeSignatures.collectLeadingTypeParameters declaration.HeaderTokens
                          Members = members }
                    )
                | _ ->
                    None)
            |> Map.ofList

        let instances =
            frontendModule.Declarations
            |> List.choose (function
                | InstanceDeclarationNode declaration ->
                    tryParseInstanceHeadType declaration
                    |> Option.map (fun headType ->
                        { ModuleName = moduleName
                          TraitName = declaration.TraitName
                          InstanceKey = TraitRuntime.instanceKeyFromTokens declaration.HeaderTokens
                          HeadType = headType
                          Members =
                            declaration.Members
                            |> List.choose (fun memberDeclaration ->
                                memberDeclaration.Name
                                |> Option.map (fun memberName -> memberName, memberDeclaration))
                            |> Map.ofList })
                | _ ->
                    None)

        { BindingSchemes = bindingSchemes
          Traits = traits
          Instances = instances }

    let private buildSurfaceIndex (frontendModules: KFrontIRModule list) =
        frontendModules
        |> List.map (fun moduleDump ->
            moduleNameText moduleDump.ModuleIdentity, buildModuleSurfaceInfo moduleDump)
        |> Map.ofList

    let private mergeVisibleBindings (surfaceIndex: Map<string, ModuleSurfaceInfo>) moduleName =
        let preludeBindings =
            surfaceIndex
            |> Map.tryFind Stdlib.PreludeModuleText
            |> Option.map (fun moduleInfo -> moduleInfo.BindingSchemes)
            |> Option.defaultValue Map.empty

        let moduleBindings =
            surfaceIndex
            |> Map.tryFind moduleName
            |> Option.map (fun moduleInfo -> moduleInfo.BindingSchemes)
            |> Option.defaultValue Map.empty

        preludeBindings
        |> Map.fold (fun state name info -> Map.add name info state) moduleBindings

    let private mergeVisibleTraits (surfaceIndex: Map<string, ModuleSurfaceInfo>) moduleName =
        let preludeTraits =
            surfaceIndex
            |> Map.tryFind Stdlib.PreludeModuleText
            |> Option.map (fun moduleInfo -> moduleInfo.Traits)
            |> Option.defaultValue Map.empty

        let moduleTraits =
            surfaceIndex
            |> Map.tryFind moduleName
            |> Option.map (fun moduleInfo -> moduleInfo.Traits)
            |> Option.defaultValue Map.empty

        preludeTraits
        |> Map.fold (fun state name info -> Map.add name info state) moduleTraits

    let private visibleInstances (surfaceIndex: Map<string, ModuleSurfaceInfo>) moduleName =
        surfaceIndex
        |> Map.tryFind moduleName
        |> Option.map (fun moduleInfo -> moduleInfo.Instances)
        |> Option.defaultValue []

    let private unwrapInstantiatedCallType
        (freshCounter: int ref)
        (scheme: TypeScheme)
        (argumentTypes: TypeExpr list)
        =
        let instantiated = TypeSignatures.instantiate "t" freshCounter.Value scheme
        freshCounter.Value <- freshCounter.Value + instantiated.Forall.Length

        let parameterTypes, resultType = TypeSignatures.schemeParts instantiated

        if List.length parameterTypes <> List.length argumentTypes then
            None
        else
            TypeSignatures.tryUnifyMany (List.zip parameterTypes argumentTypes)
            |> Option.map (fun substitution ->
                instantiated
                |> TypeSignatures.applySchemeSubstitution substitution,
                TypeSignatures.applySubstitution substitution resultType)

    let private inferLiteralType literal =
        match literal with
        | LiteralValue.Integer _ -> intType
        | LiteralValue.Float _ -> floatType
        | LiteralValue.String _ -> stringType
        | LiteralValue.Character _ -> charType
        | LiteralValue.Unit -> unitType

    let private lowerBindingReturnType (scheme: TypeScheme option) (fallbackTokens: Token list option) =
        match scheme with
        | Some scheme ->
            let _, resultType = TypeSignatures.schemeParts scheme
            Some(typeTextOf resultType)
        | None ->
            tryParseReturnTypeTokens fallbackTokens

    let private buildBindingParameters
        (scheme: TypeScheme option)
        (parameters: Parameter list)
        =
        match scheme with
        | Some scheme ->
            let parameterTypes, _ = TypeSignatures.schemeParts scheme

            if List.length parameterTypes = List.length parameters then
                List.zip parameters parameterTypes
                |> List.map (fun (parameter, parameterType) ->
                    lowerKCoreParameter parameter.Name (Some(TypeSignatures.toText parameterType)))
            else
                parameters
                |> List.map (fun parameter ->
                    lowerKCoreParameter parameter.Name (parameter.TypeTokens |> Option.map tokensText))
        | None ->
            parameters
            |> List.map (fun parameter ->
                lowerKCoreParameter parameter.Name (parameter.TypeTokens |> Option.map tokensText))

    let private buildLocalTypes
        (scheme: TypeScheme option)
        (parameters: Parameter list)
        =
        let fromScheme =
            match scheme with
            | Some scheme ->
                let parameterTypes, _ = TypeSignatures.schemeParts scheme

                if List.length parameterTypes = List.length parameters then
                    List.zip parameters parameterTypes
                    |> List.map (fun (parameter, parameterType) -> parameter.Name, parameterType)
                else
                    []
            | None ->
                []

        let fromAnnotations =
            parameters
            |> List.choose (fun parameter ->
                tryParseParameterType parameter
                |> Option.map (fun parameterType -> parameter.Name, parameterType))

        fromScheme @ fromAnnotations
        |> List.fold (fun state (name, parameterType) -> Map.add name parameterType state) Map.empty

    let private resolveConstraintInstance
        (environment: BindingLoweringEnvironment)
        (constraintInfo: TraitConstraint)
        =
        environment.VisibleInstances
        |> List.tryPick (fun instanceInfo ->
            if not (String.Equals(instanceInfo.TraitName, constraintInfo.TraitName, StringComparison.Ordinal)) then
                None
            else
                TypeSignatures.tryUnify instanceInfo.HeadType constraintInfo.Argument
                |> Option.map (fun _ -> instanceInfo))

    let private makeSyntheticBindingDeclaration
        (name: string)
        (parameters: KCoreParameter list)
        (returnTypeText: string option)
        (body: KCoreExpression)
        (provenance: KCoreOrigin)
        =
        { Source = syntheticLetSource name
          Binding =
            Some
                { Visibility = Some Visibility.Private
                  IsOpaque = false
                  Name = Some name
                  Parameters = parameters
                  ReturnTypeText = returnTypeText
                  Body = Some body
                  BodyText = Some ""
                  Provenance = provenance }
          Provenance = provenance }

    let private lowerBindingBody
        (environment: BindingLoweringEnvironment)
        (scheme: TypeScheme option)
        (parameters: Parameter list)
        (body: CoreExpression)
        =
        let freshCounter = ref 0
        let doScopeCounter = ref 0
        let usingCounter = ref 0

        let sanitizeInternalName (value: string) =
            if String.IsNullOrWhiteSpace value then
                "resource"
            else
                let characters =
                    value
                    |> Seq.map (fun character ->
                        if Char.IsLetterOrDigit character || character = '_' then
                            character
                        else
                            '_')
                    |> Seq.toArray

                String(characters)

        let freshDoScopeLabel () =
            let label = $"S{doScopeCounter.Value}"
            doScopeCounter.Value <- doScopeCounter.Value + 1
            label

        let freshUsingOwnedName bindingName =
            let index = usingCounter.Value
            usingCounter.Value <- usingCounter.Value + 1
            $"__kappa_using_{sanitizeInternalName bindingName}_{index}"

        let rec inferExpressionType localTypes expression =
            match expression with
            | Literal literal ->
                Some(inferLiteralType literal)
            | Name [ "True" ]
            | Name [ "False" ] ->
                Some boolType
            | Name [ name ] ->
                localTypes
                |> Map.tryFind name
                |> Option.orElseWith (fun () ->
                    environment.VisibleBindings
                    |> Map.tryFind name
                    |> Option.map (fun bindingInfo ->
                        let instantiated, resultType =
                            let instance = TypeSignatures.instantiate "t" freshCounter.Value bindingInfo.Scheme
                            freshCounter.Value <- freshCounter.Value + instance.Forall.Length
                            instance, snd (TypeSignatures.schemeParts instance)

                        let _ = instantiated
                        resultType))
            | Name _ ->
                None
            | Lambda(lambdaParameters, lambdaBody) ->
                let parameterTypes =
                    lambdaParameters
                    |> List.map (fun parameter ->
                        tryParseParameterType parameter
                        |> Option.defaultValue (TypeVariable $"lambda{freshCounter.Value}"))

                let lambdaLocals =
                    List.zip lambdaParameters parameterTypes
                    |> List.fold (fun state (parameter, parameterType) -> Map.add parameter.Name parameterType state) localTypes

                inferExpressionType lambdaLocals lambdaBody
                |> Option.map (fun resultType ->
                    parameterTypes
                    |> List.rev
                    |> List.fold (fun state parameterType -> TypeArrow(parameterType, state)) resultType)
            | IfThenElse(_, whenTrue, whenFalse) ->
                match inferExpressionType localTypes whenTrue, inferExpressionType localTypes whenFalse with
                | Some trueType, Some falseType when trueType = falseType ->
                    Some trueType
                | _ ->
                    None
            | Match(_, cases) ->
                cases
                |> List.choose (fun caseClause -> inferExpressionType localTypes caseClause.Body)
                |> function
                    | first :: rest when rest |> List.forall ((=) first) -> Some first
                    | _ -> None
            | Do statements ->
                inferDoResultType localTypes statements
            | MonadicSplice inner ->
                inferExpressionType localTypes inner |> Option.map unwrapIoType
            | InoutArgument inner ->
                inferExpressionType localTypes inner
            | Apply(Name [ calleeName ], arguments) ->
                let argumentTypes =
                    arguments |> List.map (inferExpressionType localTypes)

                if argumentTypes |> List.exists Option.isNone then
                    None
                else
                    environment.VisibleBindings
                    |> Map.tryFind calleeName
                    |> Option.bind (fun bindingInfo ->
                        unwrapInstantiatedCallType freshCounter bindingInfo.Scheme (argumentTypes |> List.choose id)
                        |> Option.map snd)
            | Apply _ ->
                None
            | Unary("not", _) ->
                Some boolType
            | Unary("negate", operand) ->
                inferExpressionType localTypes operand
            | Unary _ ->
                None
            | Binary(left, ("+" | "-" | "*" | "/"), right) ->
                match inferExpressionType localTypes left, inferExpressionType localTypes right with
                | Some leftType, Some rightType when leftType = intType && rightType = intType ->
                    Some intType
                | Some leftType, Some rightType when leftType = floatType && rightType = floatType ->
                    Some floatType
                | _ ->
                    None
            | Binary(_, ("==" | "!=" | "<" | ">" | "<=" | ">=" | "&&" | "||"), _) ->
                Some boolType
            | Binary(left, operatorName, right) ->
                inferExpressionType localTypes (Apply(Name [ operatorName ], [ left; right ]))
            | PrefixedString _ ->
                Some stringType

        and inferDoResultType localTypes statements =
            let bindPatternName (binding: BindPattern) =
                match binding.Pattern with
                | NamePattern name -> Some name
                | _ -> None

            match statements with
            | [] ->
                Some unitType
            | DoExpression expression :: [] ->
                inferExpressionType localTypes expression |> Option.map unwrapIoType
            | DoExpression _ :: rest ->
                inferDoResultType localTypes rest
            | DoLet(binding, expression) :: rest ->
                let nextLocals =
                    match bindPatternName binding, inferExpressionType localTypes expression with
                    | Some bindingName, Some valueType -> Map.add bindingName valueType localTypes
                    | _ -> localTypes

                inferDoResultType nextLocals rest
            | DoBind(binding, expression) :: rest ->
                let nextLocals =
                    match bindPatternName binding, inferExpressionType localTypes expression with
                    | Some bindingName, Some valueType -> Map.add bindingName (unwrapIoType valueType) localTypes
                    | _ -> localTypes

                inferDoResultType nextLocals rest
            | DoUsing(pattern, expression) :: rest ->
                let nextLocals =
                    match pattern, inferExpressionType localTypes expression with
                    | NamePattern bindingName, Some valueType -> Map.add bindingName (unwrapIoType valueType) localTypes
                    | _ -> localTypes

                inferDoResultType nextLocals rest
            | DoVar(bindingName, expression) :: rest ->
                let nextLocals =
                    match inferExpressionType localTypes expression with
                    | Some valueType -> Map.add bindingName (refType valueType) localTypes
                    | None -> localTypes

                inferDoResultType nextLocals rest
            | DoAssign _ :: rest ->
                inferDoResultType localTypes rest
            | DoWhile _ :: rest ->
                inferDoResultType localTypes rest

        and lowerDoStatements scopeLabel localTypes statements =
            let bindPatternName (binding: BindPattern) =
                match binding.Pattern with
                | NamePattern name -> name
                | _ -> "__pattern"

            match statements with
            | [] ->
                KCoreLiteral LiteralValue.Unit
            | [ DoExpression expression ] ->
                KCoreExecute(lowerExpression localTypes expression)
            | DoExpression expression :: rest ->
                KCoreSequence(KCoreExecute(lowerExpression localTypes expression), lowerDoStatements scopeLabel localTypes rest)
            | DoLet(binding, expression) :: rest ->
                let bindingName = bindPatternName binding
                let loweredValue = lowerExpression localTypes expression

                let nextLocals =
                    match inferExpressionType localTypes expression with
                    | Some valueType -> Map.add bindingName valueType localTypes
                    | None -> localTypes

                KCoreLet(bindingName, loweredValue, lowerDoStatements scopeLabel nextLocals rest)
            | DoBind(binding, expression) :: rest ->
                let bindingName = bindPatternName binding
                let loweredValue = KCoreExecute(lowerExpression localTypes expression)

                let nextLocals =
                    match inferExpressionType localTypes expression with
                    | Some valueType -> Map.add bindingName (unwrapIoType valueType) localTypes
                    | None -> localTypes

                KCoreLet(bindingName, loweredValue, lowerDoStatements scopeLabel nextLocals rest)
            | DoUsing(pattern, expression) :: rest ->
                let bindingName =
                    match pattern with
                    | NamePattern name -> name
                    | _ -> "__using"

                let hiddenOwnedName = freshUsingOwnedName bindingName
                let loweredValue = KCoreExecute(lowerExpression localTypes expression)

                let nextLocals =
                    match inferExpressionType localTypes expression with
                    | Some valueType -> Map.add bindingName (unwrapIoType valueType) localTypes
                    | None -> localTypes

                KCoreLet(
                    hiddenOwnedName,
                    loweredValue,
                    KCoreScheduleExit(
                        scopeLabel,
                        KCoreRelease(None, KCoreName [ "release" ], KCoreName [ hiddenOwnedName ]),
                        KCoreLet(bindingName, KCoreName [ hiddenOwnedName ], lowerDoStatements scopeLabel nextLocals rest)
                    )
                )
            | DoVar(bindingName, expression) :: rest ->
                let loweredValue =
                    KCoreExecute(KCoreApply(KCoreName [ "newRef" ], [ lowerExpression localTypes expression ]))

                let nextLocals =
                    match inferExpressionType localTypes expression with
                    | Some valueType -> Map.add bindingName (refType valueType) localTypes
                    | None -> localTypes

                KCoreLet(bindingName, loweredValue, lowerDoStatements scopeLabel nextLocals rest)
            | DoAssign(bindingName, expression) :: rest ->
                KCoreSequence(
                    KCoreExecute(KCoreApply(KCoreName [ "writeRef" ], [ KCoreName [ bindingName ]; lowerExpression localTypes expression ])),
                    lowerDoStatements scopeLabel localTypes rest
                )
            | DoWhile(condition, body) :: rest ->
                KCoreSequence(
                    KCoreWhile(lowerExpression localTypes condition, lowerDoStatements scopeLabel localTypes body),
                    lowerDoStatements scopeLabel localTypes rest
                )

        and lowerExpression localTypes expression =
            let lowerArguments arguments =
                arguments |> List.map (lowerExpression localTypes)

            match expression with
            | Literal literal ->
                KCoreLiteral literal
            | Name segments ->
                KCoreName segments
            | Lambda(lambdaParameters, lambdaBody) ->
                let lambdaLocals =
                    lambdaParameters
                    |> List.fold (fun state parameter ->
                        match tryParseParameterType parameter with
                        | Some parameterType -> Map.add parameter.Name parameterType state
                        | None -> state) localTypes

                KCoreLambda(
                    lambdaParameters
                    |> List.map (fun parameter ->
                        lowerKCoreParameter parameter.Name (parameter.TypeTokens |> Option.map tokensText)),
                    lowerExpression lambdaLocals lambdaBody
                )
            | IfThenElse(condition, whenTrue, whenFalse) ->
                KCoreIfThenElse(
                    lowerExpression localTypes condition,
                    lowerExpression localTypes whenTrue,
                    lowerExpression localTypes whenFalse
                )
            | Match(scrutinee, cases) ->
                KCoreMatch(
                    lowerExpression localTypes scrutinee,
                    cases
                    |> List.map (fun caseClause ->
                        { Pattern = lowerKCorePattern caseClause.Pattern
                          Body = lowerExpression localTypes caseClause.Body })
                )
            | Do statements ->
                let scopeLabel = freshDoScopeLabel ()
                KCoreDoScope(scopeLabel, lowerDoStatements scopeLabel localTypes statements)
            | MonadicSplice inner ->
                KCoreExecute(lowerExpression localTypes inner)
            | InoutArgument inner ->
                lowerExpression localTypes inner
            | Apply(Name [ calleeName ], arguments)
                when not (Map.containsKey calleeName localTypes)
                     && not (Map.containsKey calleeName environment.VisibleBindings)
                     && environment.ConstrainedMembers.ContainsKey calleeName ->
                let traitName, dictionaryParameterName = environment.ConstrainedMembers[calleeName]

                KCoreTraitCall(
                    traitName,
                    calleeName,
                    KCoreName [ dictionaryParameterName ],
                    lowerArguments arguments
                )
            | Apply(Name [ calleeName ], arguments)
                when environment.VisibleBindings.ContainsKey calleeName
                     && not (List.isEmpty environment.VisibleBindings[calleeName].Scheme.Constraints) ->
                let loweredArguments = lowerArguments arguments
                let argumentTypes = arguments |> List.map (inferExpressionType localTypes)

                let dictionaryArguments =
                    if argumentTypes |> List.exists Option.isNone then
                        []
                    else
                        let bindingInfo = environment.VisibleBindings[calleeName]

                        match unwrapInstantiatedCallType freshCounter bindingInfo.Scheme (argumentTypes |> List.choose id) with
                        | Some(instantiatedScheme, _) ->
                            instantiatedScheme.Constraints
                            |> List.choose (fun constraintInfo ->
                                resolveConstraintInstance environment constraintInfo
                                |> Option.map (fun instanceInfo ->
                                    KCoreDictionaryValue(instanceInfo.ModuleName, instanceInfo.TraitName, instanceInfo.InstanceKey)))
                        | None ->
                            []

                KCoreApply(KCoreName [ calleeName ], dictionaryArguments @ loweredArguments)
            | Apply(callee, arguments) ->
                KCoreApply(lowerExpression localTypes callee, lowerArguments arguments)
            | Unary(operatorName, operand) ->
                KCoreUnary(operatorName, lowerExpression localTypes operand)
            | Binary(left, operatorName, right) ->
                KCoreBinary(lowerExpression localTypes left, operatorName, lowerExpression localTypes right)
            | PrefixedString(prefix, parts) ->
                KCorePrefixedString(
                    prefix,
                    parts
                    |> List.map (function
                        | StringText text -> KCoreStringText text
                        | StringInterpolation inner -> KCoreStringInterpolation(lowerExpression localTypes inner))
                )

        let localTypes =
            buildLocalTypes scheme parameters

        lowerExpression localTypes body

    let private lowerUserBinding
        (environment: BindingLoweringEnvironment)
        (filePath: string)
        (moduleName: string)
        (definition: LetDefinition)
        (scheme: TypeScheme option)
        =
        let constrainedMembers, hiddenParameters =
            match scheme with
            | Some scheme when not (List.isEmpty scheme.Constraints) ->
                let memberEntries =
                    scheme.Constraints
                    |> List.mapi (fun index constraintInfo ->
                        let dictionaryParameterName = $"__kappa_dict_{constraintInfo.TraitName}_{index}"

                        constraintInfo, dictionaryParameterName)

                let constrainedMembers =
                    memberEntries
                    |> List.collect (fun (constraintInfo, dictionaryParameterName) ->
                        environment.VisibleTraits
                        |> Map.tryFind constraintInfo.TraitName
                        |> Option.map (fun traitInfo ->
                            traitInfo.Members
                            |> Map.toList
                            |> List.map (fun (memberName, _) ->
                                memberName, (constraintInfo.TraitName, dictionaryParameterName)))
                        |> Option.defaultValue [])
                    |> Map.ofList

                constrainedMembers, memberEntries
            | _ ->
                Map.empty, []

        let bodyEnvironment =
            { environment with
                ConstrainedMembers = constrainedMembers }

        let hiddenParameters =
            hiddenParameters
            |> List.map (fun (constraintInfo, dictionaryParameterName) ->
                lowerKCoreParameter
                    dictionaryParameterName
                    (Some(TypeSignatures.toText (dictionaryType constraintInfo.TraitName constraintInfo.Argument))))

        let loweredBody =
            definition.Body
            |> Option.map (lowerBindingBody bodyEnvironment scheme definition.Parameters)

        let parameters =
            hiddenParameters @ buildBindingParameters scheme definition.Parameters

        let provenance = declarationOrigin filePath moduleName (LetDeclaration definition)

        { Source = LetDeclaration definition
          Binding =
            Some
                { Visibility = definition.Visibility
                  IsOpaque = definition.IsOpaque
                  Name = definition.Name
                  Parameters = parameters
                  ReturnTypeText = lowerBindingReturnType scheme definition.ReturnTypeTokens
                  Body = loweredBody
                  BodyText = loweredBody |> Option.map (fun _ -> "")
                  Provenance = provenance }
          Provenance = provenance }

    let private synthesizeTraitDispatchBindings
        (filePath: string)
        (moduleName: string)
        (traitInfo: TraitInfo)
        =
        traitInfo.Members
        |> Map.toList
        |> List.map (fun (memberName, memberInfo) ->
            let parameterTypes, resultType = TypeSignatures.schemeParts memberInfo.Scheme
            let dictionaryParameterName = "__kappa_dict"
            let valueParameterNames =
                parameterTypes
                |> List.mapi (fun index _ -> $"arg{index}")

            let parameters =
                lowerKCoreParameter
                    dictionaryParameterName
                    (Some(TypeSignatures.toText (dictionaryType traitInfo.Name memberInfo.Scheme.Body |> function
                        | TypeArrow(parameterType, _) -> parameterType
                        | _ -> TypeVariable "_")))
                :: (List.zip valueParameterNames parameterTypes
                    |> List.map (fun (parameterName, parameterType) ->
                        lowerKCoreParameter parameterName (Some(TypeSignatures.toText parameterType))))

            let dictionaryArgumentType =
                match parameterTypes with
                | parameterType :: _ -> parameterType
                | [] -> unitType

            let body =
                KCoreTraitCall(
                    traitInfo.Name,
                    memberName,
                    KCoreName [ dictionaryParameterName ],
                    valueParameterNames |> List.map (fun parameterName -> KCoreName [ parameterName ])
                )

            let provenance =
                syntheticOrigin
                    filePath
                    moduleName
                    (TraitRuntime.dispatchBindingName traitInfo.Name memberName)
                    "trait-dispatch"

            makeSyntheticBindingDeclaration
                (TraitRuntime.dispatchBindingName traitInfo.Name memberName)
                ({ Name = dictionaryParameterName
                   TypeText = Some(TypeSignatures.toText (dictionaryType traitInfo.Name dictionaryArgumentType)) }
                 :: (List.zip valueParameterNames parameterTypes
                     |> List.map (fun (parameterName, parameterType) ->
                         { Name = parameterName
                           TypeText = Some(TypeSignatures.toText parameterType) })))
                (Some(TypeSignatures.toText resultType))
                body
                provenance)

    let private synthesizeInstanceBindings
        (surfaceIndex: Map<string, ModuleSurfaceInfo>)
        (filePath: string)
        (moduleName: string)
        (instanceInfo: InstanceInfo)
        =
        let traitInfo =
            surfaceIndex[moduleName].Traits[instanceInfo.TraitName]

        let substitution =
            match traitInfo.TypeParameters with
            | [ parameterName ] ->
                Map.ofList [ parameterName, instanceInfo.HeadType ]
            | _ ->
                Map.empty

        let visibleBindings = mergeVisibleBindings surfaceIndex moduleName
        let visibleTraits = mergeVisibleTraits surfaceIndex moduleName

        let environment =
            { CurrentModuleName = moduleName
              VisibleBindings = visibleBindings
              VisibleTraits = visibleTraits
              VisibleInstances = visibleInstances surfaceIndex moduleName
              ConstrainedMembers = Map.empty }

        let memberBindings =
            instanceInfo.Members
            |> Map.toList
            |> List.choose (fun (memberName, definition) ->
                traitInfo.Members
                |> Map.tryFind memberName
                |> Option.map (fun memberInfo ->
                    let specializedScheme =
                        memberInfo.Scheme |> TypeSignatures.applySchemeSubstitution substitution

                    let loweredBody =
                        definition.Body
                        |> Option.map (lowerBindingBody environment (Some specializedScheme) definition.Parameters)

                    let parameters = buildBindingParameters (Some specializedScheme) definition.Parameters

                    let provenance =
                        syntheticOrigin
                            filePath
                            moduleName
                            (TraitRuntime.instanceMemberBindingName instanceInfo.TraitName instanceInfo.InstanceKey memberName)
                            "instance-member"

                    makeSyntheticBindingDeclaration
                        (TraitRuntime.instanceMemberBindingName instanceInfo.TraitName instanceInfo.InstanceKey memberName)
                        parameters
                        (lowerBindingReturnType (Some specializedScheme) definition.ReturnTypeTokens)
                        (loweredBody |> Option.defaultValue (KCoreLiteral LiteralValue.Unit))
                        provenance))

        let dictionaryBinding =
            let provenance =
                syntheticOrigin
                    filePath
                    moduleName
                    (TraitRuntime.instanceDictionaryBindingName instanceInfo.TraitName instanceInfo.InstanceKey)
                    "instance-dictionary"

            makeSyntheticBindingDeclaration
                (TraitRuntime.instanceDictionaryBindingName instanceInfo.TraitName instanceInfo.InstanceKey)
                []
                (Some(TypeSignatures.toText (dictionaryType instanceInfo.TraitName instanceInfo.HeadType)))
                (KCoreDictionaryValue(moduleName, instanceInfo.TraitName, instanceInfo.InstanceKey))
                provenance

        memberBindings @ [ dictionaryBinding ]

    let private lowerModule (surfaceIndex: Map<string, ModuleSurfaceInfo>) (frontendModule: KFrontIRModule) =
        let moduleName = moduleNameText frontendModule.ModuleIdentity
        let moduleInfo = surfaceIndex[moduleName]

        let environment =
            { CurrentModuleName = moduleName
              VisibleBindings = mergeVisibleBindings surfaceIndex moduleName
              VisibleTraits = mergeVisibleTraits surfaceIndex moduleName
              VisibleInstances = visibleInstances surfaceIndex moduleName
              ConstrainedMembers = Map.empty }

        let declarations =
            frontendModule.Declarations
            |> List.collect (fun declaration ->
                let provenance = declarationOrigin frontendModule.FilePath moduleName declaration

                let originalDeclaration =
                    match declaration with
                    | LetDeclaration definition when definition.Name.IsSome ->
                        let scheme =
                            environment.VisibleBindings
                            |> Map.tryFind definition.Name.Value
                            |> Option.map (fun bindingInfo -> bindingInfo.Scheme)

                        [ lowerUserBinding environment frontendModule.FilePath moduleName definition scheme ]
                    | _ ->
                        [ { Source = declaration
                            Binding = None
                            Provenance = provenance } ]

                match declaration with
                | TraitDeclarationNode declaration ->
                    let traitInfo = moduleInfo.Traits[declaration.Name]
                    originalDeclaration @ synthesizeTraitDispatchBindings frontendModule.FilePath moduleName traitInfo
                | InstanceDeclarationNode declaration ->
                    let instanceKey = TraitRuntime.instanceKeyFromTokens declaration.HeaderTokens
                    let instanceInfo =
                        moduleInfo.Instances
                        |> List.find (fun info ->
                            String.Equals(info.TraitName, declaration.TraitName, StringComparison.Ordinal)
                            && String.Equals(info.InstanceKey, instanceKey, StringComparison.Ordinal))

                    originalDeclaration @ synthesizeInstanceBindings surfaceIndex frontendModule.FilePath moduleName instanceInfo
                | _ ->
                    originalDeclaration)

        let intrinsicTerms =
            match frontendModule.ModuleIdentity with
            | Some moduleNameSegments ->
                frontendModule.Declarations
                |> List.choose (function
                    | ExpectDeclarationNode declaration
                        when Stdlib.intrinsicallySatisfiesExpect "interpreter" moduleNameSegments declaration ->
                        match declaration with
                        | ExpectTermDeclaration termDeclaration -> Some termDeclaration.Name
                        | _ -> None
                    | _ ->
                        None)
            | None ->
                []

        { Name = moduleName
          SourceFile = frontendModule.FilePath
          ModuleAttributes = frontendModule.ModuleAttributes
          Imports = frontendModule.Imports
          IntrinsicTerms = intrinsicTerms |> List.distinct |> List.sort
          Declarations = declarations }

    let lowerKCoreModules (backendProfile: string) (frontendModules: KFrontIRModule list) =
        let surfaceIndex = buildSurfaceIndex frontendModules

        frontendModules
        |> List.map (fun frontendModule ->
            let moduleName = moduleNameText frontendModule.ModuleIdentity
            let moduleInfo = surfaceIndex[moduleName]

            let environment =
                { CurrentModuleName = moduleName
                  VisibleBindings = mergeVisibleBindings surfaceIndex moduleName
                  VisibleTraits = mergeVisibleTraits surfaceIndex moduleName
                  VisibleInstances = visibleInstances surfaceIndex moduleName
                  ConstrainedMembers = Map.empty }

            let declarations =
                frontendModule.Declarations
                |> List.collect (fun declaration ->
                    let provenance = declarationOrigin frontendModule.FilePath moduleName declaration

                    let originalDeclaration =
                        match declaration with
                        | LetDeclaration definition when definition.Name.IsSome ->
                            let scheme =
                                environment.VisibleBindings
                                |> Map.tryFind definition.Name.Value
                                |> Option.map (fun bindingInfo -> bindingInfo.Scheme)

                            [ lowerUserBinding environment frontendModule.FilePath moduleName definition scheme ]
                        | _ ->
                            [ { Source = declaration
                                Binding = None
                                Provenance = provenance } ]

                    match declaration with
                    | TraitDeclarationNode declaration ->
                        let traitInfo = moduleInfo.Traits[declaration.Name]
                        originalDeclaration @ synthesizeTraitDispatchBindings frontendModule.FilePath moduleName traitInfo
                    | InstanceDeclarationNode declaration ->
                        let instanceKey = TraitRuntime.instanceKeyFromTokens declaration.HeaderTokens
                        let instanceInfo =
                            moduleInfo.Instances
                            |> List.find (fun info ->
                                String.Equals(info.TraitName, declaration.TraitName, StringComparison.Ordinal)
                                && String.Equals(info.InstanceKey, instanceKey, StringComparison.Ordinal))

                        originalDeclaration @ synthesizeInstanceBindings surfaceIndex frontendModule.FilePath moduleName instanceInfo
                    | _ ->
                        originalDeclaration)

            let intrinsicTerms =
                match frontendModule.ModuleIdentity with
                | Some moduleNameSegments ->
                    frontendModule.Declarations
                    |> List.choose (function
                        | ExpectDeclarationNode declaration
                            when Stdlib.intrinsicallySatisfiesExpect backendProfile moduleNameSegments declaration ->
                            match declaration with
                            | ExpectTermDeclaration termDeclaration -> Some termDeclaration.Name
                            | _ -> None
                        | _ ->
                            None)
                | None ->
                    []

            { Name = moduleName
              SourceFile = frontendModule.FilePath
              ModuleAttributes = frontendModule.ModuleAttributes
              Imports = frontendModule.Imports
              IntrinsicTerms = intrinsicTerms |> List.distinct |> List.sort
              Declarations = declarations })
