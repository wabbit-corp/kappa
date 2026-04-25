namespace Kappa.Compiler

open System

// Lowers KCore into the implementation-defined KRuntimeIR checkpoint.
module internal KRuntimeLowering =
    let private tokensText (tokens: Token list) =
        tokens
        |> List.map (fun token -> token.Text)
        |> String.concat " "

    let private tryParseTypeText text =
        let source = SourceText.From("__runtime_type__.kp", text)
        let lexResult = Lexer.tokenize source

        if not (List.isEmpty lexResult.Diagnostics) then
            None
        else
            TypeSignatures.parseType lexResult.Tokens

    let rec private eraseRuntimeTypeExpr typeExpr =
        match typeExpr with
        | TypeSignatures.TypeLevelLiteral _ ->
            typeExpr
        | TypeSignatures.TypeUniverse None ->
            typeExpr
        | TypeSignatures.TypeUniverse(Some universeExpr) ->
            TypeSignatures.TypeUniverse(Some(eraseRuntimeTypeExpr universeExpr))
        | TypeSignatures.TypeIntrinsic _ ->
            typeExpr
        | TypeSignatures.TypeApply(callee, arguments) ->
            TypeSignatures.TypeApply(eraseRuntimeTypeExpr callee, arguments |> List.map eraseRuntimeTypeExpr)
        | TypeSignatures.TypeLambda(parameterName, parameterSort, body) ->
            TypeSignatures.TypeLambda(parameterName, eraseRuntimeTypeExpr parameterSort, eraseRuntimeTypeExpr body)
        | TypeSignatures.TypeDelay inner ->
            TypeSignatures.TypeDelay(eraseRuntimeTypeExpr inner)
        | TypeSignatures.TypeMemo inner ->
            TypeSignatures.TypeMemo(eraseRuntimeTypeExpr inner)
        | TypeSignatures.TypeForce inner ->
            TypeSignatures.TypeForce(eraseRuntimeTypeExpr inner)
        | TypeSignatures.TypeProject(target, fieldName) ->
            TypeSignatures.TypeProject(eraseRuntimeTypeExpr target, fieldName)
        | TypeSignatures.TypeVariable name ->
            TypeSignatures.TypeVariable name
        | TypeSignatures.TypeName(name, arguments) ->
            TypeSignatures.TypeName(name, arguments |> List.map eraseRuntimeTypeExpr)
        | TypeSignatures.TypeArrow(_, parameterType, resultType) ->
            TypeSignatures.TypeArrow(QuantityOmega, eraseRuntimeTypeExpr parameterType, eraseRuntimeTypeExpr resultType)
        | TypeSignatures.TypeEquality(left, _) ->
            eraseRuntimeTypeExpr left
        | TypeSignatures.TypeCapture(inner, _) ->
            eraseRuntimeTypeExpr inner
        | TypeSignatures.TypeRecord fields ->
            fields
            |> List.choose (fun field ->
                if field.Quantity = QuantityZero then
                    None
                else
                    Some
                        { field with
                            Quantity = QuantityOmega
                            Type = eraseRuntimeTypeExpr field.Type })
            |> TypeSignatures.TypeRecord
        | TypeSignatures.TypeUnion members ->
            TypeSignatures.TypeUnion(members |> List.map eraseRuntimeTypeExpr)

    let private runtimeValueTypeExpr typeExpr =
        match eraseRuntimeTypeExpr typeExpr with
        | TypeSignatures.TypeName([ "IO" ], [ inner ]) -> inner
        | TypeSignatures.TypeName([ "IO" ], [ _; inner ]) -> inner
        | TypeSignatures.TypeName([ "UIO" ], [ inner ]) -> inner
        | other -> other

    let private eraseRuntimeTypeText (text: string) =
        match tryParseTypeText text with
        | Some parsed ->
            parsed |> eraseRuntimeTypeExpr |> TypeSignatures.toText
        | None ->
            text

    let private runtimeValueTypeText (text: string) =
        match tryParseTypeText text with
        | Some parsed ->
            parsed |> runtimeValueTypeExpr |> TypeSignatures.toText
        | None ->
            text

    let private normalizeTypeTokens (tokens: Token list) =
        tokens
        |> TypeSignatures.parseType
        |> Option.map (eraseRuntimeTypeExpr >> TypeSignatures.toText)
        |> Option.defaultValue (tokensText tokens)

    let private significantTokens (tokens: Token list) =
        tokens
        |> List.filter (fun token ->
            match token.Kind with
            | Newline
            | Indent
            | Dedent
            | EndOfFile -> false
            | _ -> true)

    let private splitLeadingTypeArgumentTokens argumentCount tokens =
        let tokens = significantTokens tokens

        let takeAtom remaining =
            match remaining with
            | [] ->
                [], []
            | first :: _ when first.Kind = LeftParen ->
                let tokenArray = remaining |> List.toArray
                let mutable depth = 0
                let mutable index = 0
                let mutable keepReading = true

                while keepReading && index < tokenArray.Length do
                    match tokenArray[index].Kind with
                    | LeftParen ->
                        depth <- depth + 1
                    | RightParen ->
                        depth <- depth - 1

                        if depth = 0 then
                            keepReading <- false
                    | _ ->
                        ()

                    index <- index + 1

                List.ofArray tokenArray[0 .. index - 1], List.ofArray tokenArray[index ..]
            | first :: rest ->
                [ first ], rest

        let rec loop remaining count groups =
            if count <= 1 then
                List.rev (remaining :: groups)
            else
                let group, rest = takeAtom remaining
                loop rest (count - 1) (group :: groups)

        loop tokens (max 1 argumentCount) []

    let private lowerRuntimeParameter (parameter: KCoreParameter) : KRuntimeParameter =
        { Name = parameter.Name
          TypeText = parameter.TypeText |> Option.map eraseRuntimeTypeText }

    let private isCompileTimeParameterType parameterType =
        match parameterType with
        | TypeSignatures.TypeUniverse _
        | TypeSignatures.TypeIntrinsic TypeSignatures.UniverseClassifier
        | TypeSignatures.TypeIntrinsic TypeSignatures.ConstraintClassifier
        | TypeSignatures.TypeIntrinsic TypeSignatures.QuantityClassifier
        | TypeSignatures.TypeIntrinsic TypeSignatures.RegionClassifier
        | TypeSignatures.TypeIntrinsic TypeSignatures.RecRowClassifier
        | TypeSignatures.TypeIntrinsic TypeSignatures.VarRowClassifier
        | TypeSignatures.TypeIntrinsic TypeSignatures.EffRowClassifier
        | TypeSignatures.TypeIntrinsic TypeSignatures.LabelClassifier
        | TypeSignatures.TypeIntrinsic TypeSignatures.EffLabelClassifier
        | TypeSignatures.TypeName([ "Type" ], [])
        | TypeSignatures.TypeName([ "Constraint" ], [])
        | TypeSignatures.TypeName([ "Quantity" ], [])
        | TypeSignatures.TypeName([ "Region" ], [])
        | TypeSignatures.TypeName([ "RecRow" ], [])
        | TypeSignatures.TypeName([ "VarRow" ], [])
        | TypeSignatures.TypeName([ "EffRow" ], [])
        | TypeSignatures.TypeName([ "Label" ], [])
        | TypeSignatures.TypeName([ "EffLabel" ], []) ->
            true
        | _ ->
            false

    let private isCompileTimeKCoreParameter (parameter: KCoreParameter) =
        parameter.Type |> Option.exists isCompileTimeParameterType

    let private filterRuntimeParameters (parameters: KCoreParameter list) =
        parameters |> List.filter (isCompileTimeKCoreParameter >> not)

    let private filterRuntimeArguments (keepMask: bool list) (arguments: KCoreArgument list) =
        if List.length keepMask = List.length arguments then
            List.zip keepMask arguments
            |> List.choose (fun (keep, argument) -> if keep then Some argument else None)
        else
            arguments

    let rec private lowerKRuntimePattern pattern =
        match pattern with
        | KCoreWildcardPattern ->
            KRuntimeWildcardPattern
        | KCoreNamePattern name ->
            KRuntimeNamePattern name
        | KCoreLiteralPattern literal ->
            KRuntimeLiteralPattern literal
        | KCoreConstructorPattern(name, arguments) ->
            KRuntimeConstructorPattern(name, arguments |> List.map lowerKRuntimePattern)
        | KCoreOrPattern alternatives ->
            KRuntimeOrPattern(alternatives |> List.map lowerKRuntimePattern)

    let rec private lowerKRuntimeExitAction runtimeParameterMasks action =
        match action with
        | KCoreDeferred expression ->
            KRuntimeDeferred(lowerKRuntimeExpression runtimeParameterMasks expression)
        | KCoreRelease(resourceTypeText, release, resource) ->
            KRuntimeRelease(
                resourceTypeText,
                lowerKRuntimeExpression runtimeParameterMasks release,
                lowerKRuntimeExpression runtimeParameterMasks resource
            )

    and private lowerKRuntimeExpression runtimeParameterMasks expression =
        match expression with
        | KCoreLiteral literal ->
            KRuntimeLiteral literal
        | KCoreStaticObject _ ->
            KRuntimeLiteral LiteralValue.Unit
        | KCoreName segments ->
            KRuntimeName segments
        | KCoreSyntaxQuote inner
        | KCoreSyntaxSplice inner
        | KCoreTopLevelSyntaxSplice inner
        | KCoreCodeQuote inner
        | KCoreCodeSplice inner ->
            lowerKRuntimeExpression runtimeParameterMasks inner
        | KCoreLambda(parameters, body) ->
            KRuntimeClosure(
                filterRuntimeParameters parameters |> List.map (fun parameter -> parameter.Name),
                lowerKRuntimeExpression runtimeParameterMasks body
            )
        | KCoreIfThenElse(condition, whenTrue, whenFalse) ->
            KRuntimeIfThenElse(
                lowerKRuntimeExpression runtimeParameterMasks condition,
                lowerKRuntimeExpression runtimeParameterMasks whenTrue,
                lowerKRuntimeExpression runtimeParameterMasks whenFalse
            )
        | KCoreMatch(scrutinee, cases) ->
            KRuntimeMatch(
                lowerKRuntimeExpression runtimeParameterMasks scrutinee,
                cases
                |> List.map (fun caseClause ->
                    { Pattern = lowerKRuntimePattern caseClause.Pattern
                      Guard = caseClause.Guard |> Option.map (lowerKRuntimeExpression runtimeParameterMasks)
                      Body = lowerKRuntimeExpression runtimeParameterMasks caseClause.Body })
            )
        | KCoreExecute inner ->
            KRuntimeExecute(lowerKRuntimeExpression runtimeParameterMasks inner)
        | KCoreLet(_, KCoreStaticObject _, body) ->
            lowerKRuntimeExpression runtimeParameterMasks body
        | KCoreLet(bindingName, value, body) ->
            KRuntimeLet(
                bindingName,
                lowerKRuntimeExpression runtimeParameterMasks value,
                lowerKRuntimeExpression runtimeParameterMasks body
            )
        | KCoreDoScope(scopeLabel, body) ->
            KRuntimeDoScope(scopeLabel, lowerKRuntimeExpression runtimeParameterMasks body)
        | KCoreScheduleExit(scopeLabel, action, body) ->
            KRuntimeScheduleExit(
                scopeLabel,
                lowerKRuntimeExitAction runtimeParameterMasks action,
                lowerKRuntimeExpression runtimeParameterMasks body
            )
        | KCoreSequence(first, second) ->
            KRuntimeSequence(
                lowerKRuntimeExpression runtimeParameterMasks first,
                lowerKRuntimeExpression runtimeParameterMasks second
            )
        | KCoreWhile(condition, body) ->
            KRuntimeWhile(
                lowerKRuntimeExpression runtimeParameterMasks condition,
                lowerKRuntimeExpression runtimeParameterMasks body
            )
        | KCoreAppSpine(callee, arguments) ->
            let runtimeArguments =
                match callee with
                | KCoreName [ calleeName ] ->
                    runtimeParameterMasks
                    |> Map.tryFind calleeName
                    |> Option.map (fun keepMask -> filterRuntimeArguments keepMask arguments)
                    |> Option.defaultValue arguments
                | _ ->
                    arguments

            KRuntimeApply(
                lowerKRuntimeExpression runtimeParameterMasks callee,
                runtimeArguments
                |> List.map (fun argument -> lowerKRuntimeExpression runtimeParameterMasks argument.Expression)
            )
        | KCoreDictionaryValue(moduleName, traitName, instanceKey) ->
            KRuntimeDictionaryValue(moduleName, traitName, instanceKey)
        | KCoreTraitCall(traitName, memberName, dictionary, arguments) ->
            KRuntimeTraitCall(
                traitName,
                memberName,
                lowerKRuntimeExpression runtimeParameterMasks dictionary,
                arguments |> List.map (lowerKRuntimeExpression runtimeParameterMasks)
            )
        | KCoreUnary(operatorName, operand) ->
            KRuntimeUnary(operatorName, lowerKRuntimeExpression runtimeParameterMasks operand)
        | KCoreBinary(left, operatorName, right) ->
            KRuntimeBinary(
                lowerKRuntimeExpression runtimeParameterMasks left,
                operatorName,
                lowerKRuntimeExpression runtimeParameterMasks right
            )
        | KCorePrefixedString(prefix, parts) ->
            KRuntimePrefixedString(
                prefix,
                parts
                |> List.map (function
                    | KCoreStringText text -> KRuntimeStringText text
                    | KCoreStringInterpolation inner ->
                        KRuntimeStringInterpolation(lowerKRuntimeExpression runtimeParameterMasks inner))
            )

    let private isPrivateByDefaultAttributes attributes =
        attributes
        |> List.exists (fun attributeName -> String.Equals(attributeName, "PrivateByDefault", StringComparison.Ordinal))

    let private isExportedVisibility moduleAttributes visibility =
        match visibility with
        | Some Visibility.Public -> true
        | Some Visibility.Private -> false
        | None -> not (isPrivateByDefaultAttributes moduleAttributes)

    let private isExportedKCoreBinding (moduleDump: KCoreModule) (binding: KCoreBinding) =
        isExportedVisibility moduleDump.ModuleAttributes binding.Visibility

    let private isExportedDataDeclaration (moduleDump: KCoreModule) (declaration: DataDeclaration) =
        not declaration.IsOpaque
        && isExportedVisibility moduleDump.ModuleAttributes declaration.Visibility

    let private constructorFieldNames (constructor: DataConstructor) =
        match constructor.Parameters with
        | Some parameters ->
            parameters |> List.map (fun parameter -> parameter.ParameterName)
        | None ->
            let significant =
                constructor.Tokens
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
            |> List.map (fun groupTokens ->
                match groupTokens |> List.tryFindIndex (fun token -> token.Kind = Colon) with
                | Some colonIndex when colonIndex > 0 ->
                    match groupTokens |> List.take colonIndex with
                    | [ nameToken ] when Token.isName nameToken ->
                        Some(SyntaxFacts.trimIdentifierQuotes nameToken.Text)
                    | _ ->
                        None
                | _ ->
                    None)

    let private constructorFieldTypeTexts (constructor: DataConstructor) =
        match constructor.Parameters with
        | Some parameters ->
            parameters |> List.map (fun parameter -> normalizeTypeTokens parameter.ParameterTypeTokens)
        | None ->
            let significant = constructor.Tokens |> significantTokens

            match significant with
            | _ :: { Kind = Colon } :: typeTokens ->
                match TypeSignatures.parseScheme typeTokens with
                | Some scheme ->
                    let parameterTypes, _ = TypeSignatures.schemeParts scheme
                    parameterTypes |> List.map TypeSignatures.toText
                | None ->
                    constructor
                    |> TypeSignatures.constructorFieldTokenGroups
                    |> List.map normalizeTypeTokens
            | _ ->
                constructor
                |> TypeSignatures.constructorFieldTokenGroups
                |> List.map normalizeTypeTokens

    let private isCompileTimeOnlyBindingBody body =
        match body with
        | Some(KCoreStaticObject _) -> true
        | _ -> false

    let private tryEtaExpandClosureBinding
        returnTypeText
        body
        : (KRuntimeParameter list * string option * KRuntimeExpression option) option =
        let rec flattenArrows typeExpr =
            match typeExpr with
            | TypeSignatures.TypeArrow(_, parameterType, resultType) ->
                let parameterTypes, finalResult = flattenArrows resultType
                parameterType :: parameterTypes, finalResult
            | other ->
                [], other

        match returnTypeText, body with
        | Some returnTypeText, Some(KRuntimeClosure(parameterNames, closureBody)) ->
            tryParseTypeText returnTypeText
            |> Option.bind (fun returnType ->
                let parameterTypes, resultType = flattenArrows returnType

                if List.length parameterNames = List.length parameterTypes then
                    let etaParameters : KRuntimeParameter list =
                        parameterNames
                        |> List.zip parameterTypes
                        |> List.map (fun (parameterType, parameterName) ->
                            ({ Name = parameterName
                               TypeText = Some(TypeSignatures.toText parameterType |> eraseRuntimeTypeText) }
                             : KRuntimeParameter))

                    Some(
                        etaParameters,
                        Some(TypeSignatures.toText resultType |> runtimeValueTypeText),
                        Some closureBody
                    )
                else
                    None)
        | _ ->
            None

    let private intrinsicRuntimeSignature (declaration: ExpectedTermDeclaration) =
        declaration.TypeTokens
        |> TypeSignatures.parseScheme
        |> Option.map (fun scheme ->
            let parameterTypes, resultType = TypeSignatures.schemeParts scheme

            let parameters: KRuntimeParameter list =
                parameterTypes
                |> List.filter (isCompileTimeParameterType >> not)
                |> List.mapi (fun index parameterType ->
                    { Name = $"arg{index}"
                      TypeText = Some(TypeSignatures.toText parameterType |> eraseRuntimeTypeText) })

            parameters, Some(runtimeValueTypeExpr resultType |> TypeSignatures.toText))

    let lowerKRuntimeModule (coreModule: KCoreModule) : KRuntimeModule =
        let moduleTraitArities =
            coreModule.Declarations
            |> List.choose (fun declaration ->
                match declaration.Source with
                | TraitDeclarationNode traitDeclaration ->
                    Some(
                        traitDeclaration.Name,
                        TypeSignatures.collectLeadingTypeParameters traitDeclaration.HeaderTokens
                        |> List.length
                    )
                | _ ->
                    None)
            |> Map.ofList

        let runtimeParameterMasks =
            coreModule.Declarations
            |> List.choose (fun declaration ->
                declaration.Binding
                |> Option.bind (fun binding ->
                    binding.Name
                    |> Option.map (fun bindingName ->
                        bindingName,
                        (binding.Parameters |> List.map (isCompileTimeKCoreParameter >> not)))))
            |> Map.ofList

        let termBindings : KRuntimeBinding list =
            coreModule.Declarations
            |> List.choose (fun declaration ->
                match declaration.Binding with
                | Some binding when binding.Name.IsSome && not (isCompileTimeOnlyBindingBody binding.Body) ->
                    let loweredBody = binding.Body |> Option.map (lowerKRuntimeExpression runtimeParameterMasks)
                    let loweredParameters = binding.Parameters |> filterRuntimeParameters |> List.map lowerRuntimeParameter
                    let loweredReturnType = binding.ReturnTypeText |> Option.map runtimeValueTypeText
                    let parameters, returnType, body =
                        match loweredParameters with
                        | [] ->
                            tryEtaExpandClosureBinding binding.ReturnTypeText loweredBody
                            |> Option.defaultValue (loweredParameters, loweredReturnType, loweredBody)
                        | _ ->
                            loweredParameters, loweredReturnType, loweredBody

                    Some
                        { Name = binding.Name.Value
                          Parameters = parameters
                          ReturnTypeText = returnType
                          Body = body
                          Intrinsic = false
                          Provenance = binding.Provenance }
                | _ ->
                    None)

        let intrinsicBindings : KRuntimeBinding list =
            let intrinsicSignatures =
                coreModule.Declarations
                |> List.choose (fun declaration ->
                    match declaration.Source with
                    | ExpectDeclarationNode (ExpectTermDeclaration termDeclaration) ->
                        intrinsicRuntimeSignature termDeclaration
                        |> Option.map (fun signature -> termDeclaration.Name, signature)
                    | _ ->
                        None)
                |> Map.ofList

            coreModule.IntrinsicTerms
            |> List.map (fun name ->
                let parameters, returnTypeText =
                    intrinsicSignatures
                    |> Map.tryFind name
                    |> Option.defaultValue ([], None)

                { Name = name
                  Parameters = parameters
                  ReturnTypeText = returnTypeText
                  Body = None
                  Intrinsic = true
                  Provenance =
                    { FilePath = coreModule.SourceFile
                      ModuleName = coreModule.Name
                      DeclarationName = Some name
                      IntroductionKind = "intrinsic" } })

        let dataTypes : KRuntimeDataType list =
            let sourceDataTypes : KRuntimeDataType list =
                coreModule.Declarations
                |> List.choose (fun declaration ->
                    match declaration.Source with
                    | DataDeclarationNode dataDeclaration ->
                        let constructors : KRuntimeConstructor list =
                            dataDeclaration.Constructors
                            |> List.map (fun constructor ->
                                { Name = constructor.Name
                                  Arity = constructor.Arity
                                  TypeName = dataDeclaration.Name
                                  FieldNames = constructorFieldNames constructor
                                  FieldTypeTexts = constructorFieldTypeTexts constructor
                                  Provenance =
                                    { declaration.Provenance with
                                        DeclarationName = Some constructor.Name
                                        IntroductionKind = "constructor" } })

                        Some
                            { Name = dataDeclaration.Name
                              TypeParameters = TypeSignatures.collectLeadingTypeParameters dataDeclaration.HeaderTokens
                              Constructors = constructors }
                    | _ ->
                        None)

            let syntheticDataTypes : KRuntimeDataType list =
                coreModule.SyntheticDataTypes
                |> List.map (fun dataType ->
                    let constructor : KRuntimeConstructor =
                        { Name = dataType.ConstructorName
                          Arity = dataType.FieldNames.Length
                          TypeName = dataType.Name
                          FieldNames = dataType.FieldNames |> List.map Some
                          FieldTypeTexts = dataType.FieldTypeTexts |> List.map eraseRuntimeTypeText
                          Provenance = dataType.Provenance }

                    { Name = dataType.Name
                      TypeParameters = dataType.TypeParameters
                      Constructors = [ constructor ] })

            sourceDataTypes @ syntheticDataTypes

        let traits : KRuntimeTrait list =
            coreModule.Declarations
            |> List.choose (fun declaration ->
                match declaration.Source with
                | TraitDeclarationNode traitDeclaration ->
                    Some
                        { Name = traitDeclaration.Name
                          TypeParameterCount =
                            TypeSignatures.collectLeadingTypeParameters traitDeclaration.HeaderTokens
                            |> List.length }
                | _ ->
                    None)

        let traitInstances : KRuntimeTraitInstance list =
            coreModule.Declarations
            |> List.choose (fun declaration ->
                match declaration.Source with
                | InstanceDeclarationNode instanceDeclaration ->
                    let instanceKey = TraitRuntime.instanceKeyFromTokens instanceDeclaration.HeaderTokens
                    let argumentCount =
                        moduleTraitArities
                        |> Map.tryFind instanceDeclaration.TraitName
                        |> Option.defaultValue 1

                    let headTypeTexts =
                        instanceDeclaration.HeaderTokens
                        |> splitLeadingTypeArgumentTokens argumentCount
                        |> List.map normalizeTypeTokens

                    let memberBindings =
                        instanceDeclaration.Members
                        |> List.choose (fun memberDeclaration ->
                            memberDeclaration.Name
                            |> Option.map (fun memberName ->
                                memberName,
                                TraitRuntime.instanceMemberBindingName
                                    instanceDeclaration.TraitName
                                    instanceKey
                                    memberName))

                    Some
                        { TraitName = instanceDeclaration.TraitName
                          InstanceKey = instanceKey
                          HeadTypeTexts = headTypeTexts
                          MemberBindings = memberBindings }
                | _ ->
                    None)

        let constructors : KRuntimeConstructor list =
            dataTypes
            |> List.collect (fun dataType -> dataType.Constructors)

        let exports =
            coreModule.Declarations
            |> List.collect (fun declaration ->
                let bindingExports =
                    match declaration.Binding with
                    | Some binding when binding.Name.IsSome && isExportedKCoreBinding coreModule binding ->
                        [ binding.Name.Value ]
                    | _ ->
                        []

                let constructorExports =
                    match declaration.Source with
                    | DataDeclarationNode dataDeclaration when isExportedDataDeclaration coreModule dataDeclaration ->
                        dataDeclaration.Constructors |> List.map (fun constructor -> constructor.Name)
                    | _ ->
                        []

                bindingExports @ constructorExports)
            |> List.append coreModule.IntrinsicTerms
            |> List.distinct
            |> List.sort

        ({ Name = coreModule.Name
           SourceFile = coreModule.SourceFile
           Imports = coreModule.Imports
           Exports = exports
           IntrinsicTerms = coreModule.IntrinsicTerms
           DataTypes = dataTypes
           Traits = traits
           TraitInstances = traitInstances
           Constructors = constructors
           Bindings = termBindings @ intrinsicBindings }: KRuntimeModule)
