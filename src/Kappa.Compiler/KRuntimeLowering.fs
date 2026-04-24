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

    let private eraseRuntimeTypeText (text: string) =
        match tryParseTypeText text with
        | Some parsed ->
            parsed |> eraseRuntimeTypeExpr |> TypeSignatures.toText
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

    let rec private lowerKRuntimeExitAction action =
        match action with
        | KCoreDeferred expression ->
            KRuntimeDeferred(lowerKRuntimeExpression expression)
        | KCoreRelease(resourceTypeText, release, resource) ->
            KRuntimeRelease(resourceTypeText, lowerKRuntimeExpression release, lowerKRuntimeExpression resource)

    and private lowerKRuntimeExpression expression =
        match expression with
        | KCoreLiteral literal ->
            KRuntimeLiteral literal
        | KCoreName segments ->
            KRuntimeName segments
        | KCoreLambda(parameters, body) ->
            KRuntimeClosure(parameters |> List.map (fun parameter -> parameter.Name), lowerKRuntimeExpression body)
        | KCoreIfThenElse(condition, whenTrue, whenFalse) ->
            KRuntimeIfThenElse(
                lowerKRuntimeExpression condition,
                lowerKRuntimeExpression whenTrue,
                lowerKRuntimeExpression whenFalse
            )
        | KCoreMatch(scrutinee, cases) ->
            KRuntimeMatch(
                lowerKRuntimeExpression scrutinee,
                cases
                |> List.map (fun caseClause ->
                    { Pattern = lowerKRuntimePattern caseClause.Pattern
                      Guard = caseClause.Guard |> Option.map lowerKRuntimeExpression
                      Body = lowerKRuntimeExpression caseClause.Body })
            )
        | KCoreExecute inner ->
            KRuntimeExecute(lowerKRuntimeExpression inner)
        | KCoreLet(bindingName, value, body) ->
            KRuntimeLet(bindingName, lowerKRuntimeExpression value, lowerKRuntimeExpression body)
        | KCoreDoScope(scopeLabel, body) ->
            KRuntimeDoScope(scopeLabel, lowerKRuntimeExpression body)
        | KCoreScheduleExit(scopeLabel, action, body) ->
            KRuntimeScheduleExit(scopeLabel, lowerKRuntimeExitAction action, lowerKRuntimeExpression body)
        | KCoreSequence(first, second) ->
            KRuntimeSequence(lowerKRuntimeExpression first, lowerKRuntimeExpression second)
        | KCoreWhile(condition, body) ->
            KRuntimeWhile(lowerKRuntimeExpression condition, lowerKRuntimeExpression body)
        | KCoreApply(callee, arguments) ->
            KRuntimeApply(lowerKRuntimeExpression callee, arguments |> List.map lowerKRuntimeExpression)
        | KCoreDictionaryValue(moduleName, traitName, instanceKey) ->
            KRuntimeDictionaryValue(moduleName, traitName, instanceKey)
        | KCoreTraitCall(traitName, memberName, dictionary, arguments) ->
            KRuntimeTraitCall(
                traitName,
                memberName,
                lowerKRuntimeExpression dictionary,
                arguments |> List.map lowerKRuntimeExpression
            )
        | KCoreUnary(operatorName, operand) ->
            KRuntimeUnary(operatorName, lowerKRuntimeExpression operand)
        | KCoreBinary(left, operatorName, right) ->
            KRuntimeBinary(lowerKRuntimeExpression left, operatorName, lowerKRuntimeExpression right)
        | KCorePrefixedString(prefix, parts) ->
            KRuntimePrefixedString(
                prefix,
                parts
                |> List.map (function
                    | KCoreStringText text -> KRuntimeStringText text
                    | KCoreStringInterpolation inner -> KRuntimeStringInterpolation(lowerKRuntimeExpression inner))
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

        let termBindings : KRuntimeBinding list =
            coreModule.Declarations
            |> List.choose (fun declaration ->
                match declaration.Binding with
                | Some binding when binding.Name.IsSome ->
                    Some
                        { Name = binding.Name.Value
                          Parameters = binding.Parameters |> List.map lowerRuntimeParameter
                          ReturnTypeText = binding.ReturnTypeText |> Option.map eraseRuntimeTypeText
                          Body = binding.Body |> Option.map lowerKRuntimeExpression
                          Intrinsic = false
                          Provenance = binding.Provenance }
                | _ ->
                    None)

        let intrinsicBindings : KRuntimeBinding list =
            coreModule.IntrinsicTerms
            |> List.map (fun name ->
                { Name = name
                  Parameters = []
                  ReturnTypeText = None
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
                                  FieldTypeTexts =
                                    constructor
                                    |> TypeSignatures.constructorFieldTokenGroups
                                    |> List.map normalizeTypeTokens
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
            let exportedSourceConstructors =
                dataTypes
                |> List.filter (fun dataType ->
                    coreModule.Declarations
                    |> List.exists (fun declaration ->
                        match declaration.Source with
                        | DataDeclarationNode dataDeclaration ->
                            dataDeclaration.Name = dataType.Name
                            && isExportedDataDeclaration coreModule dataDeclaration
                        | _ ->
                            false))
                |> List.collect (fun dataType -> dataType.Constructors)

            let syntheticConstructors =
                coreModule.SyntheticDataTypes
                |> List.collect (fun dataType ->
                    dataTypes
                    |> List.tryFind (fun runtimeDataType -> runtimeDataType.Name = dataType.Name)
                    |> Option.map (fun runtimeDataType -> runtimeDataType.Constructors)
                    |> Option.defaultValue [])

            exportedSourceConstructors @ syntheticConstructors

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
