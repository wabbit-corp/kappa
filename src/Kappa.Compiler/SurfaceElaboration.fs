namespace Kappa.Compiler

open System
open System.Security.Cryptography
open System.Text

// Elaborates resolved surface modules into KCore while preserving observability metadata.
module SurfaceElaboration =
    open TypeSignatures

    type private TraitMemberInfo =
        { Name: string
          Scheme: TypeScheme
          DefaultDefinition: LetDefinition option }

    type private TraitInfo =
        { ModuleName: string
          Name: string
          TypeParameters: string list
          Members: Map<string, TraitMemberInfo> }

    type private InstanceInfo =
        { ModuleName: string
          TraitName: string
          InstanceKey: string
          Constraints: TraitConstraint list
          HeadTypes: TypeExpr list
          Members: Map<string, LetDefinition> }

    type private TypeAliasInfo =
        { ModuleName: string
          Name: string
          Parameters: string list
          Body: TypeExpr }

    type private BindingSchemeInfo =
        { ModuleName: string
          Name: string
          Scheme: TypeScheme
          ParameterLayouts: Parameter list option }

    type private ProjectionInfo =
        { ModuleName: string
          Name: string
          Binders: ProjectionBinder list
          ReturnType: TypeExpr
          Body: SurfaceProjectionBody option }

    type private ModuleSurfaceInfo =
        { TypeAliases: Map<string, TypeAliasInfo>
          BindingSchemes: Map<string, BindingSchemeInfo>
          Constructors: Map<string, BindingSchemeInfo>
          Projections: Map<string, ProjectionInfo>
          Traits: Map<string, TraitInfo>
          Instances: InstanceInfo list
          Imports: ImportSpec list
          ExportedTerms: Set<string>
          ExportedConstructors: Set<string>
          ExportedTypes: Set<string>
          ExportedTraits: Set<string> }

    type private BindingLoweringEnvironment =
        { CurrentModuleName: string
          VisibleTypeAliases: Map<string, TypeAliasInfo>
          VisibleBindings: Map<string, BindingSchemeInfo>
          VisibleConstructors: Map<string, BindingSchemeInfo>
          VisibleProjections: Map<string, ProjectionInfo>
          VisibleTraits: Map<string, TraitInfo>
          VisibleInstances: InstanceInfo list
          ConstrainedMembers: Map<string, string * string> }

    type private PreparedCallArgument =
        | ExplicitArgument of SurfaceExpression
        | ImplicitArgument of KCoreExpression

    type private PreparedCallParameter =
        { Layout: Parameter option
          ParameterType: TypeExpr
          AssignedArgument: PreparedCallArgument option }

    type private PreparedBindingCall =
        { InstantiatedScheme: TypeScheme
          ResultType: TypeExpr
          Parameters: PreparedCallParameter list
          Arguments: PreparedCallArgument list }

    type private SyntheticRecordLayout =
        { TypeKey: string
          TypeExpr: TypeExpr
          TypeName: string
          ConstructorName: string
          TypeParameters: string list
          Fields: RecordField list
          Provenance: KCoreOrigin }

    let private tokensText (tokens: Token list) =
        tokens
        |> List.map (fun token -> token.Text)
        |> String.concat " "

    let private tryParseTypeText (text: string) =
        let source = SourceText.From("__surface_elaboration_type__.kp", text)
        let lexResult = Lexer.tokenize source

        if not (List.isEmpty lexResult.Diagnostics) then
            None
        else
            TypeSignatures.parseType lexResult.Tokens

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

    let private stableSyntheticSuffix (value: string) =
        let bytes = Encoding.UTF8.GetBytes(value)
        let hash = SHA256.HashData(bytes)

        hash
        |> Seq.take 10
        |> Seq.map (fun value -> value.ToString("x2"))
        |> String.concat ""

    let private syntheticRecordTypeName recordKey =
        "__kappa_record_" + stableSyntheticSuffix recordKey

    let private syntheticRecordConstructorName recordKey =
        syntheticRecordTypeName recordKey

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

    let private lowerKCoreParameter (name: string) (typeText: string option) : KCoreParameter =
        { Name = name
          TypeText = typeText }

    let rec private lowerKCorePattern (pattern: SurfacePattern) =
        match pattern with
        | WildcardPattern ->
            KCoreWildcardPattern
        | NamePattern name ->
            KCoreNamePattern name
        | LiteralPattern literal ->
            KCoreLiteralPattern literal
        | ConstructorPattern(name, arguments) ->
            KCoreConstructorPattern(name, arguments |> List.map lowerKCorePattern)
        | OrPattern alternatives ->
            KCoreOrPattern(alternatives |> List.map lowerKCorePattern)
        | AnonymousRecordPattern _ ->
            KCoreWildcardPattern

    let private collectPatternNames (pattern: SurfacePattern) =
        let rec loop current =
            seq {
                match current with
                | NamePattern name -> yield name
                | ConstructorPattern(_, arguments) ->
                    for argument in arguments do
                        yield! loop argument
                | OrPattern alternatives ->
                    match alternatives with
                    | first :: _ ->
                        yield! loop first
                    | [] ->
                        ()
                | AnonymousRecordPattern fields ->
                    for field in fields do
                        yield! loop field.Pattern
                | WildcardPattern
                | LiteralPattern _ -> ()
            }

        loop pattern |> Seq.toList

    let private isOmegaText text =
        String.Equals(text, "\u03c9", StringComparison.Ordinal)
        || String.Equals(text, "omega", StringComparison.Ordinal)

    let private isQuantityVariableToken (token: Token) =
        if not (Token.isName token) then
            false
        else
            let text = SyntaxFacts.trimIdentifierQuotes token.Text
            not (String.IsNullOrEmpty text) && not (Char.IsUpper text[0])

    let private tryParseQuantityPrefix (tokens: Token list) =
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
        | head :: rest when Token.isName head && isOmegaText head.Text ->
            Some(QuantityOmega, rest)
        | head :: rest when isQuantityVariableToken head ->
            match rest with
            | next :: _ when Token.isName next ->
                Some(QuantityVariable(SyntaxFacts.trimIdentifierQuotes head.Text), rest)
            | _ ->
                None
        | _ ->
            None

    let private stripDelimitedParameterTokens tokens =
        match tokens with
        | { Kind = LeftParen } :: rest when not (List.isEmpty rest) && (List.last rest).Kind = RightParen ->
            Some(false, rest |> List.take (List.length rest - 1))
        | { Kind = LeftBrace } :: rest when not (List.isEmpty rest) && (List.last rest).Kind = RightBrace ->
            Some(true, rest |> List.take (List.length rest - 1))
        | _ ->
            None

    let private tryParseSignatureParameterLayout (tokens: Token list) =
        stripDelimitedParameterTokens tokens
        |> Option.bind (fun (isImplicit, innerTokens) ->
            let isInout, remainingTokens =
                match innerTokens with
                | head :: rest when Token.isKeyword Keyword.Inout head -> true, rest
                | _ -> false, innerTokens

            let quantity, bodyTokens =
                if isInout then
                    Some QuantityOne, remainingTokens
                else
                    match tryParseQuantityPrefix remainingTokens with
                    | Some(parsedQuantity, rest) -> Some parsedQuantity, rest
                    | None -> None, remainingTokens

            match bodyTokens with
            | nameToken :: { Kind = Colon } :: typeTokens when Token.isName nameToken && not (List.isEmpty typeTokens) ->
                Some
                    { Name = SyntaxFacts.trimIdentifierQuotes nameToken.Text
                      TypeTokens = Some typeTokens
                      Quantity = quantity
                      IsImplicit = isImplicit
                      IsInout = isInout }
            | _ ->
                None)

    let private bindingSignatureBodyTokens (tokens: Token list) =
        let tokenArray = significantTokens tokens |> List.toArray
        let mutable parenDepth = 0
        let mutable braceDepth = 0
        let mutable bracketDepth = 0
        let mutable bodyStart = 0

        for index = 0 to tokenArray.Length - 1 do
            match tokenArray[index].Kind with
            | LeftParen -> parenDepth <- parenDepth + 1
            | RightParen -> parenDepth <- max 0 (parenDepth - 1)
            | LeftBrace -> braceDepth <- braceDepth + 1
            | RightBrace -> braceDepth <- max 0 (braceDepth - 1)
            | LeftBracket -> bracketDepth <- bracketDepth + 1
            | RightBracket -> bracketDepth <- max 0 (bracketDepth - 1)
            | Operator when parenDepth = 0 && braceDepth = 0 && bracketDepth = 0 && String.Equals(tokenArray[index].Text, "=>", StringComparison.Ordinal) ->
                bodyStart <- index + 1
            | Equals when parenDepth = 0 && braceDepth = 0 && bracketDepth = 0 && index + 1 < tokenArray.Length ->
                match tokenArray[index + 1] with
                | { Kind = Operator; Text = ">" } ->
                    bodyStart <- index + 2
                | _ ->
                    ()
            | _ ->
                ()

        List.ofArray tokenArray[bodyStart..]

    let private splitSignatureParameterTokens (tokens: Token list) =
        let bodyTokens = bindingSignatureBodyTokens tokens
        let tokenArray = bodyTokens |> List.toArray
        let segments = ResizeArray<Token list>()
        let mutable parenDepth = 0
        let mutable braceDepth = 0
        let mutable bracketDepth = 0
        let mutable segmentStart = 0

        for index = 0 to tokenArray.Length - 1 do
            match tokenArray[index].Kind with
            | LeftParen -> parenDepth <- parenDepth + 1
            | RightParen -> parenDepth <- max 0 (parenDepth - 1)
            | LeftBrace -> braceDepth <- braceDepth + 1
            | RightBrace -> braceDepth <- max 0 (braceDepth - 1)
            | LeftBracket -> bracketDepth <- bracketDepth + 1
            | RightBracket -> bracketDepth <- max 0 (bracketDepth - 1)
            | Arrow when parenDepth = 0 && braceDepth = 0 && bracketDepth = 0 ->
                if index > segmentStart then
                    segments.Add(List.ofArray tokenArray[segmentStart .. index - 1])

                segmentStart <- index + 1
            | _ ->
                ()

        segments |> Seq.toList

    let private mergeParameterLayouts
        (signatureLayouts: Parameter option list option)
        (definitionLayouts: Parameter list option)
        =
        match signatureLayouts, definitionLayouts with
        | Some signatureParameters, Some definitionParameters
            when List.length signatureParameters = List.length definitionParameters ->
            List.zip signatureParameters definitionParameters
            |> List.map (fun (signatureParameter, definitionParameter) ->
                match signatureParameter with
                | Some signatureParameter ->
                    { definitionParameter with
                        Name = signatureParameter.Name
                        TypeTokens = signatureParameter.TypeTokens |> Option.orElse definitionParameter.TypeTokens
                        Quantity = signatureParameter.Quantity |> Option.orElse definitionParameter.Quantity
                        IsImplicit = signatureParameter.IsImplicit
                        IsInout = signatureParameter.IsInout }
                | None ->
                    definitionParameter)
            |> Some
        | Some signatureParameters, None when signatureParameters |> List.forall Option.isSome ->
            signatureParameters |> List.choose id |> Some
        | _ ->
            definitionLayouts

    let private tryResolveVisibleConstructorInfo
        (environment: BindingLoweringEnvironment)
        nameSegments
        =
        match List.rev nameSegments with
        | [] ->
            None
        | constructorName :: reversedModuleSegments ->
            environment.VisibleConstructors
            |> Map.tryFind constructorName
            |> Option.filter (fun constructorInfo ->
                match reversedModuleSegments with
                | [] ->
                    true
                | _ ->
                    String.Equals(
                        constructorInfo.ModuleName,
                        SyntaxFacts.moduleNameToText (List.rev reversedModuleSegments),
                        StringComparison.Ordinal
                    ))

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

    let private dictionaryType traitName argumentTypes =
        TypeName([ TraitRuntime.dictionaryTypeName traitName ], argumentTypes)

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
                  Scheme = scheme
                  DefaultDefinition = memberDeclaration.DefaultDefinition })
        | _ ->
            None

    let private tryParseInstanceHeader (declaration: InstanceDeclaration) =
        declaration.FullHeaderTokens
        |> TypeSignatures.parseScheme
        |> Option.bind (fun scheme ->
            match scheme.Body with
            | TypeName(name, headTypes) ->
                let traitName =
                    match name with
                    | [ singleName ] -> singleName
                    | _ -> SyntaxFacts.moduleNameToText name

                Some(traitName, scheme.Constraints, headTypes)
            | _ ->
                None)

    let private tryParseTypeAliasInfo moduleName (declaration: TypeAlias) =
        if declaration.IsOpaque then
            None
        else
            declaration.BodyTokens
            |> Option.bind TypeSignatures.parseType
            |> Option.map (fun body ->
                declaration.Name,
                { ModuleName = moduleName
                  Name = declaration.Name
                  Parameters = TypeSignatures.collectLeadingTypeParameters declaration.HeaderTokens
                  Body = body })

    let private isPrivateByDefault (frontendModule: KFrontIRModule) =
        frontendModule.ModuleAttributes
        |> List.exists (fun attributeName -> String.Equals(attributeName, "PrivateByDefault", StringComparison.Ordinal))

    let private isExportedVisibility isPrivateByDefault visibility =
        match visibility with
        | Some Visibility.Public -> true
        | Some Visibility.Private -> false
        | None -> not isPrivateByDefault

    let private itemImportsTermName (item: ImportItem) =
        item.Namespace.IsNone || item.Namespace = Some ImportNamespace.Term

    let private itemImportsTypeName (item: ImportItem) =
        item.Namespace.IsNone || item.Namespace = Some ImportNamespace.Type

    let private itemImportsTraitName (item: ImportItem) =
        item.Namespace.IsNone || item.Namespace = Some ImportNamespace.Trait

    let private itemImportsConstructorName (item: ImportItem) =
        item.Namespace = Some ImportNamespace.Constructor

    let private tryParseConstructorScheme (dataDeclaration: DataDeclaration) (constructor: DataConstructor) =
        let typeParameters = TypeSignatures.collectLeadingTypeParameters dataDeclaration.HeaderTokens
        let resultType = TypeName([ dataDeclaration.Name ], typeParameters |> List.map TypeVariable)

        let body =
            constructor
            |> TypeSignatures.constructorFieldTypes
            |> List.rev
            |> List.fold (fun state fieldType -> TypeArrow(QuantityOmega, fieldType, state)) resultType

        Some
            (constructor.Name,
             { ModuleName = ""
               Name = constructor.Name
               Scheme =
                { Forall = typeParameters
                  Constraints = []
                  Body = body }
               ParameterLayouts = None })

    let private selectionImportsExportedName
        (exportedNames: Set<string>)
        itemSelector
        (selection: ImportSelection)
        (name: string)
        =
        let exported = Set.contains name exportedNames

        match selection with
        | QualifiedOnly ->
            false
        | Items items ->
            exported
            && (items
                |> List.exists (fun item ->
                    String.Equals(item.Name, name, StringComparison.Ordinal)
                    && itemSelector item))
        | All ->
            exported
        | AllExcept excludedNames ->
            exported && not (List.contains name excludedNames)

    let private buildModuleSurfaceInfo (frontendModule: KFrontIRModule) =
        let moduleName = moduleNameText frontendModule.ModuleIdentity
        let privateByDefault = isPrivateByDefault frontendModule

        let typeAliases =
            frontendModule.Declarations
            |> List.choose (function
                | TypeAliasNode declaration -> tryParseTypeAliasInfo moduleName declaration
                | _ -> None)
            |> Map.ofList

        let bindingSchemes =
            let bindingDefinitions =
                frontendModule.Declarations
                |> List.choose (function
                    | LetDeclaration definition when definition.Name.IsSome ->
                        Some(definition.Name.Value, definition)
                    | _ ->
                        None)
                |> Map.ofList

            frontendModule.Declarations
            |> List.choose (function
                | SignatureDeclaration declaration ->
                    let signatureParameterLayouts =
                        declaration.TypeTokens
                        |> splitSignatureParameterTokens
                        |> function
                            | [] -> None
                            | parameterTokens -> Some(parameterTokens |> List.map tryParseSignatureParameterLayout)

                    TypeSignatures.parseScheme declaration.TypeTokens
                    |> Option.map (fun scheme ->
                        declaration.Name,
                        { ModuleName = moduleName
                          Name = declaration.Name
                          Scheme = scheme
                          ParameterLayouts =
                            bindingDefinitions
                            |> Map.tryFind declaration.Name
                            |> Option.map (fun definition -> definition.Parameters)
                            |> mergeParameterLayouts signatureParameterLayouts })
                | ExpectDeclarationNode (ExpectTermDeclaration declaration) ->
                    TypeSignatures.parseScheme declaration.TypeTokens
                    |> Option.map (fun scheme ->
                        declaration.Name,
                        { ModuleName = moduleName
                          Name = declaration.Name
                          Scheme = scheme
                          ParameterLayouts = None })
                | _ ->
                    None)
            |> Map.ofList

        let constructors =
            frontendModule.Declarations
            |> List.choose (function
                | DataDeclarationNode dataDeclaration ->
                    dataDeclaration.Constructors
                    |> List.choose (fun constructor ->
                        tryParseConstructorScheme dataDeclaration constructor
                        |> Option.map (fun (name, info) ->
                            name,
                            { info with
                                ModuleName = moduleName }))
                    |> Some
                | _ ->
                    None)
            |> List.concat
            |> Map.ofList

        let projections =
            frontendModule.Declarations
            |> List.choose (function
                | ProjectionDeclarationNode declaration ->
                    TypeSignatures.parseType declaration.ReturnTypeTokens
                    |> Option.map (fun returnType ->
                        declaration.Name,
                        { ModuleName = moduleName
                          Name = declaration.Name
                          Binders = declaration.Binders
                          ReturnType = returnType
                          Body = declaration.Body })
                | _ ->
                    None)
            |> Map.ofList

        let exportedTerms =
            frontendModule.Declarations
            |> List.choose (function
                | SignatureDeclaration declaration when isExportedVisibility privateByDefault declaration.Visibility ->
                    Some declaration.Name
                | LetDeclaration definition when isExportedVisibility privateByDefault definition.Visibility ->
                    definition.Name
                | ProjectionDeclarationNode declaration when isExportedVisibility privateByDefault declaration.Visibility ->
                    Some declaration.Name
                | _ ->
                    None)
            |> Set.ofList

        let exportedTypes =
            frontendModule.Declarations
            |> List.choose (function
                | DataDeclarationNode declaration when isExportedVisibility privateByDefault declaration.Visibility ->
                    Some declaration.Name
                | TypeAliasNode declaration when isExportedVisibility privateByDefault declaration.Visibility ->
                    Some declaration.Name
                | _ ->
                    None)
            |> Set.ofList

        let exportedConstructors =
            frontendModule.Declarations
            |> List.choose (function
                | DataDeclarationNode declaration when isExportedVisibility privateByDefault declaration.Visibility ->
                    Some(declaration.Constructors |> List.map (fun constructor -> constructor.Name))
                | _ ->
                    None)
            |> List.concat
            |> Set.ofList

        let exportedTraits =
            frontendModule.Declarations
            |> List.choose (function
                | TraitDeclarationNode declaration when isExportedVisibility privateByDefault declaration.Visibility ->
                    Some declaration.Name
                | _ ->
                    None)
            |> Set.ofList

        let traits =
            frontendModule.Declarations
            |> List.choose (function
                | TraitDeclarationNode declaration ->
                    let members =
                        declaration.Members
                        |> List.fold (fun state memberDeclaration ->
                            match tryParseTraitMemberInfo memberDeclaration with
                            | Some(memberName, memberInfo) ->
                                let mergedInfo =
                                    match Map.tryFind memberName state with
                                    | Some existingInfo ->
                                        { memberInfo with
                                            DefaultDefinition =
                                                memberInfo.DefaultDefinition
                                                |> Option.orElse existingInfo.DefaultDefinition }
                                    | None ->
                                        memberInfo

                                Map.add memberName mergedInfo state
                            | None ->
                                match memberDeclaration.Name, memberDeclaration.DefaultDefinition with
                                | Some memberName, Some defaultDefinition ->
                                    state
                                    |> Map.change memberName (function
                                        | Some existingInfo ->
                                            Some { existingInfo with DefaultDefinition = Some defaultDefinition }
                                        | None ->
                                            None)
                                | _ ->
                                    state) Map.empty

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
                    tryParseInstanceHeader declaration
                    |> Option.map (fun (traitName, constraints, headTypes) ->
                        { ModuleName = moduleName
                          TraitName = traitName
                          InstanceKey = TraitRuntime.instanceKeyFromTokens declaration.HeaderTokens
                          Constraints = constraints
                          HeadTypes = headTypes
                          Members =
                            declaration.Members
                            |> List.choose (fun memberDeclaration ->
                                memberDeclaration.Name
                                |> Option.map (fun memberName -> memberName, memberDeclaration))
                            |> Map.ofList })
                | _ ->
                    None)

        { TypeAliases = typeAliases
          BindingSchemes = bindingSchemes
          Constructors = constructors
          Projections = projections
          Traits = traits
          Instances = instances
          Imports = frontendModule.Imports
          ExportedTerms = exportedTerms
          ExportedConstructors = exportedConstructors
          ExportedTypes = exportedTypes
          ExportedTraits = exportedTraits }

    let private buildSurfaceIndex (frontendModules: KFrontIRModule list) =
        frontendModules
        |> List.map (fun moduleDump ->
            moduleNameText moduleDump.ModuleIdentity, buildModuleSurfaceInfo moduleDump)
        |> Map.ofList

    let private importedModuleInfos (surfaceIndex: Map<string, ModuleSurfaceInfo>) moduleName =
        surfaceIndex
        |> Map.tryFind moduleName
        |> Option.map (fun moduleInfo ->
            moduleInfo.Imports
            |> List.choose (fun spec ->
                match spec.Source with
                | Url _ ->
                    None
                | Dotted moduleSegments ->
                    let importedModuleName = SyntaxFacts.moduleNameToText moduleSegments
                    surfaceIndex
                    |> Map.tryFind importedModuleName
                    |> Option.map (fun importedModule -> spec, importedModule))
            |> List.distinctBy (fun (_, importedModule) ->
                importedModule.BindingSchemes.Keys |> Seq.toList,
                importedModule.Constructors.Keys |> Seq.toList,
                importedModule.Projections.Keys |> Seq.toList,
                importedModule.Traits.Keys |> Seq.toList,
                importedModule.TypeAliases.Keys |> Seq.toList))
        |> Option.defaultValue []

    let private mergeVisibleBindings (surfaceIndex: Map<string, ModuleSurfaceInfo>) moduleName =
        let importedBindings =
            importedModuleInfos surfaceIndex moduleName
            |> List.fold (fun state (spec, importedModule) ->
                importedModule.BindingSchemes
                |> Map.fold (fun current name info ->
                    if selectionImportsExportedName importedModule.ExportedTerms itemImportsTermName spec.Selection name then
                        Map.add name info current
                    else
                        current) state) Map.empty

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
        |> Map.fold (fun state name info -> Map.add name info state) importedBindings
        |> fun state -> moduleBindings |> Map.fold (fun current name info -> Map.add name info current) state

    let private mergeVisibleConstructors (surfaceIndex: Map<string, ModuleSurfaceInfo>) moduleName =
        let importedConstructors =
            importedModuleInfos surfaceIndex moduleName
            |> List.fold (fun state (spec, importedModule) ->
                importedModule.Constructors
                |> Map.fold (fun current name info ->
                    if
                        selectionImportsExportedName
                            importedModule.ExportedConstructors
                            itemImportsConstructorName
                            spec.Selection
                            name
                    then
                        Map.add name info current
                    else
                        current) state) Map.empty

        let moduleConstructors =
            surfaceIndex
            |> Map.tryFind moduleName
            |> Option.map (fun moduleInfo -> moduleInfo.Constructors)
            |> Option.defaultValue Map.empty

        moduleConstructors
        |> Map.fold (fun state name info -> Map.add name info state) importedConstructors

    let private mergeVisibleProjections (surfaceIndex: Map<string, ModuleSurfaceInfo>) moduleName =
        let importedProjections =
            importedModuleInfos surfaceIndex moduleName
            |> List.fold (fun state (spec, importedModule) ->
                importedModule.Projections
                |> Map.fold (fun current name info ->
                    if selectionImportsExportedName importedModule.ExportedTerms itemImportsTermName spec.Selection name then
                        Map.add name info current
                    else
                        current) state) Map.empty

        let moduleProjections =
            surfaceIndex
            |> Map.tryFind moduleName
            |> Option.map (fun moduleInfo -> moduleInfo.Projections)
            |> Option.defaultValue Map.empty

        moduleProjections
        |> Map.fold (fun state name info -> Map.add name info state) importedProjections

    let private mergeVisibleTypeAliases (surfaceIndex: Map<string, ModuleSurfaceInfo>) moduleName =
        let importedAliases =
            importedModuleInfos surfaceIndex moduleName
            |> List.fold (fun state (spec, importedModule) ->
                importedModule.TypeAliases
                |> Map.fold (fun current name info ->
                    if selectionImportsExportedName importedModule.ExportedTypes itemImportsTypeName spec.Selection name then
                        Map.add name info current
                    else
                        current) state) Map.empty

        let preludeAliases =
            surfaceIndex
            |> Map.tryFind Stdlib.PreludeModuleText
            |> Option.map (fun moduleInfo -> moduleInfo.TypeAliases)
            |> Option.defaultValue Map.empty

        let moduleAliases =
            surfaceIndex
            |> Map.tryFind moduleName
            |> Option.map (fun moduleInfo -> moduleInfo.TypeAliases)
            |> Option.defaultValue Map.empty

        preludeAliases
        |> Map.fold (fun state name info -> Map.add name info state) importedAliases
        |> fun state -> moduleAliases |> Map.fold (fun current name info -> Map.add name info current) state

    let private mergeVisibleTraits (surfaceIndex: Map<string, ModuleSurfaceInfo>) moduleName =
        let importedTraits =
            importedModuleInfos surfaceIndex moduleName
            |> List.fold (fun state (spec, importedModule) ->
                importedModule.Traits
                |> Map.fold (fun current name info ->
                    if selectionImportsExportedName importedModule.ExportedTraits itemImportsTraitName spec.Selection name then
                        Map.add name info current
                    else
                        current) state) Map.empty

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
        |> Map.fold (fun state name info -> Map.add name info state) importedTraits
        |> fun state -> moduleTraits |> Map.fold (fun current name info -> Map.add name info current) state

    let private visibleInstances (surfaceIndex: Map<string, ModuleSurfaceInfo>) moduleName =
        let importedInstances =
            importedModuleInfos surfaceIndex moduleName
            |> List.collect (fun (spec, importedModule) ->
                importedModule.Instances
                |> List.filter (fun instanceInfo ->
                    selectionImportsExportedName importedModule.ExportedTraits itemImportsTraitName spec.Selection instanceInfo.TraitName))

        let moduleInstances =
            surfaceIndex
            |> Map.tryFind moduleName
            |> Option.map (fun moduleInfo -> moduleInfo.Instances)
            |> Option.defaultValue []

        importedInstances @ moduleInstances

    let private normalizeTypeAliases (aliases: Map<string, TypeAliasInfo>) =
        let rec loop visited typeExpr =
            match typeExpr with
            | TypeVariable _ ->
                typeExpr
            | TypeArrow(quantity, parameterType, resultType) ->
                TypeArrow(quantity, loop visited parameterType, loop visited resultType)
            | TypeEquality(left, right) ->
                TypeEquality(loop visited left, loop visited right)
            | TypeCapture(inner, captures) ->
                TypeCapture(loop visited inner, captures)
            | TypeRecord fields ->
                TypeRecord(
                    fields
                    |> List.map (fun field ->
                        { field with
                            Type = loop visited field.Type })
                )
            | TypeName([ name ], arguments) ->
                let normalizedArguments = arguments |> List.map (loop visited)

                match aliases |> Map.tryFind name with
                | Some aliasInfo
                    when not (Set.contains name visited)
                         && List.length aliasInfo.Parameters = List.length normalizedArguments ->
                    let substitution = List.zip aliasInfo.Parameters normalizedArguments |> Map.ofList
                    aliasInfo.Body
                    |> TypeSignatures.applySubstitution substitution
                    |> loop (Set.add name visited)
                | _ ->
                    TypeName([ name ], normalizedArguments)
            | TypeName(name, arguments) ->
                TypeName(name, arguments |> List.map (loop visited))

        loop Set.empty

    let private tryCanonicalRecordType
        (aliases: Map<string, TypeAliasInfo>)
        typeExpr
        =
        match normalizeTypeAliases aliases typeExpr with
        | TypeRecord fields ->
            Some(TypeRecord fields)
        | _ ->
            None

    let rec private eraseSyntheticRecordTypeExpr typeExpr =
        match typeExpr with
        | TypeVariable name ->
            TypeVariable name
        | TypeName(name, arguments) ->
            TypeName(name, arguments |> List.map eraseSyntheticRecordTypeExpr)
        | TypeArrow(_, parameterType, resultType) ->
            TypeArrow(QuantityOmega, eraseSyntheticRecordTypeExpr parameterType, eraseSyntheticRecordTypeExpr resultType)
        | TypeEquality(left, _) ->
            eraseSyntheticRecordTypeExpr left
        | TypeCapture(inner, _) ->
            eraseSyntheticRecordTypeExpr inner
        | TypeRecord fields ->
            fields
            |> List.choose (fun field ->
                if field.Quantity = QuantityZero then
                    None
                else
                    Some
                        { field with
                            Quantity = QuantityOmega
                            Type = eraseSyntheticRecordTypeExpr field.Type })
            |> TypeRecord

    let private ensureSyntheticRecordLayout
        (moduleName: string)
        (filePath: string)
        (layouts: Map<string, SyntheticRecordLayout> ref)
        recordType
        =
        let erasedRecordType = eraseSyntheticRecordTypeExpr recordType
        let recordKey = TypeSignatures.toText erasedRecordType
        let canonicalRecordType = tryParseTypeText recordKey |> Option.defaultValue erasedRecordType

        match Map.tryFind recordKey layouts.Value with
        | Some layout ->
            layout
        | None ->
            let fields =
                match canonicalRecordType with
                | TypeRecord fields -> fields
                | _ -> []

            let typeName = syntheticRecordTypeName recordKey
            let constructorName = syntheticRecordConstructorName recordKey
            let provenance = syntheticOrigin filePath moduleName typeName "synthetic-record"
            let typeParameters = TypeSignatures.collectFreeTypeVariables canonicalRecordType

            let layout =
                { TypeKey = recordKey
                  TypeExpr = canonicalRecordType
                  TypeName = typeName
                  ConstructorName = constructorName
                  TypeParameters = typeParameters
                  Fields = fields
                  Provenance = provenance }

            layouts.Value <- Map.add recordKey layout layouts.Value
            layout

    let private composeTypeSubstitution
        (existing: Map<string, TypeExpr>)
        (next: Map<string, TypeExpr>)
        =
        let rewrittenExisting =
            existing
            |> Map.map (fun _ value -> TypeSignatures.applySubstitution next value)

        next
        |> Map.fold (fun state name value -> Map.add name value state) rewrittenExisting

    let private isCompileTimeArgumentType
        (aliases: Map<string, TypeAliasInfo>)
        parameterType
        =
        match normalizeTypeAliases aliases parameterType with
        | TypeName([ "Type" ], [])
        | TypeName([ "Constraint" ], [])
        | TypeName([ "Quantity" ], [])
        | TypeName([ "Region" ], [])
        | TypeName([ "RecRow" ], [])
        | TypeName([ "Label" ], []) ->
            true
        | _ ->
            false

    let private tryUnifyVisibleTypes
        (aliases: Map<string, TypeAliasInfo>)
        (pairs: (TypeExpr * TypeExpr) list)
        =
        let normalize = normalizeTypeAliases aliases
        pairs
        |> List.map (fun (left, right) -> normalize left, normalize right)
        |> TypeSignatures.tryUnifyMany

    let private extendPatternLocalTypes
        (environment: BindingLoweringEnvironment)
        (freshCounter: int ref)
        (localTypes: Map<string, TypeExpr>)
        expectedType
        pattern
        =
        let rec loop locals expected currentPattern =
            match currentPattern with
            | WildcardPattern
            | LiteralPattern _ ->
                locals
            | NamePattern name ->
                match expected with
                | Some expectedType -> Map.add name expectedType locals
                | None -> locals
            | AnonymousRecordPattern fields ->
                match expected |> Option.bind (tryCanonicalRecordType environment.VisibleTypeAliases) with
                | Some(TypeRecord recordFields) ->
                    fields
                    |> List.fold (fun state field ->
                        let fieldType =
                            recordFields
                            |> List.tryFind (fun recordField -> String.Equals(recordField.Name, field.Name, StringComparison.Ordinal))
                            |> Option.map (fun recordField -> recordField.Type)

                        loop state fieldType field.Pattern) locals
                | _ ->
                    fields |> List.fold (fun state field -> loop state None field.Pattern) locals
            | ConstructorPattern(nameSegments, arguments) ->
                let argumentTypes =
                    match expected, tryResolveVisibleConstructorInfo environment nameSegments with
                    | Some expectedResultType, Some constructorInfo ->
                        let instantiated = TypeSignatures.instantiate "t" freshCounter.Value constructorInfo.Scheme
                        freshCounter.Value <- freshCounter.Value + instantiated.Forall.Length

                        let parameterTypes, constructorResultType = TypeSignatures.schemeParts instantiated

                        if List.length parameterTypes = List.length arguments then
                            match tryUnifyVisibleTypes environment.VisibleTypeAliases [ constructorResultType, expectedResultType ] with
                            | Some substitution ->
                                parameterTypes
                                |> List.map (TypeSignatures.applySubstitution substitution >> Some)
                            | None ->
                                List.replicate arguments.Length None
                        else
                            List.replicate arguments.Length None
                    | _ ->
                        List.replicate arguments.Length None

                List.zip arguments argumentTypes
                |> List.fold (fun state (argumentPattern, argumentType) -> loop state argumentType argumentPattern) locals
            | OrPattern alternatives ->
                match alternatives with
                | first :: _ ->
                    loop locals expected first
                | [] ->
                    locals

        loop localTypes expectedType pattern

    let private unwrapInstantiatedCallType
        (environment: BindingLoweringEnvironment)
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
            tryUnifyVisibleTypes environment.VisibleTypeAliases (List.zip parameterTypes argumentTypes)
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
        (aliases: Map<string, TypeAliasInfo>)
        (scheme: TypeScheme option)
        (parameters: Parameter list)
        =
        match scheme with
        | Some scheme ->
            let parameterTypes, _ = TypeSignatures.schemeParts scheme

            if List.length parameterTypes = List.length parameters then
                List.zip parameters parameterTypes
                |> List.filter (fun (_, parameterType) -> not (isCompileTimeArgumentType aliases parameterType))
                |> List.map (fun (parameter, parameterType) ->
                    lowerKCoreParameter parameter.Name (Some(TypeSignatures.toText parameterType)))
            else
                parameters
                |> List.filter (fun parameter ->
                    parameter.TypeTokens
                    |> Option.bind TypeSignatures.parseType
                    |> Option.map (isCompileTimeArgumentType aliases >> not)
                    |> Option.defaultValue true)
                |> List.map (fun parameter ->
                    lowerKCoreParameter parameter.Name (parameter.TypeTokens |> Option.map tokensText))
        | None ->
            parameters
            |> List.filter (fun parameter ->
                parameter.TypeTokens
                |> Option.bind TypeSignatures.parseType
                |> Option.map (isCompileTimeArgumentType aliases >> not)
                |> Option.defaultValue true)
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

    let private tryOptionPayloadType
        (aliases: Map<string, TypeAliasInfo>)
        (typeExpr: TypeExpr)
        =
        match normalizeTypeAliases aliases typeExpr with
        | TypeName([ "Option" ], [ payloadType ])
        | TypeName([ "std"; "prelude"; "Option" ], [ payloadType ]) ->
            Some payloadType
        | _ ->
            None

    let private tryProjectVisibleRecordType
        (aliases: Map<string, TypeAliasInfo>)
        currentType
        path
        =
        let normalize = normalizeTypeAliases aliases

        let rec loop typeExpr remainingPath =
            match remainingPath, normalize typeExpr with
            | [], normalizedType ->
                Some normalizedType
            | fieldName :: rest, TypeRecord fields ->
                fields
                |> List.tryFind (fun field -> String.Equals(field.Name, fieldName, StringComparison.Ordinal))
                |> Option.bind (fun field -> loop field.Type rest)
            | _ ->
                None

        loop currentType path

    let private instantiateVisibleBindingResultType
        (environment: BindingLoweringEnvironment)
        (freshCounter: int ref)
        name
        =
        environment.VisibleBindings
        |> Map.tryFind name
        |> Option.map (fun bindingInfo ->
            let instance = TypeSignatures.instantiate "t" freshCounter.Value bindingInfo.Scheme
            freshCounter.Value <- freshCounter.Value + instance.Forall.Length
            snd (TypeSignatures.schemeParts instance))

    let private tryInferVisibleAppliedType
        (aliases: Map<string, TypeAliasInfo>)
        (inferArgumentType: SurfaceExpression -> TypeExpr option)
        headType
        arguments
        =
        let normalize = normalizeTypeAliases aliases

        let rec loop currentType remainingArguments =
            match remainingArguments with
            | [] ->
                Some currentType
            | argument :: rest ->
                match normalize currentType with
                | TypeArrow(_, parameterType, resultType) ->
                    inferArgumentType argument
                    |> Option.bind (fun argumentType ->
                        tryUnifyVisibleTypes aliases [ parameterType, argumentType ]
                        |> Option.map (fun substitution ->
                            TypeSignatures.applySubstitution substitution resultType)
                        |> Option.orElseWith (fun () ->
                            if TypeSignatures.definitionallyEqual (normalize parameterType) (normalize argumentType) then
                                Some resultType
                            else
                                None))
                    |> Option.bind (fun nextType -> loop nextType rest)
                | _ ->
                    None

        loop headType arguments

    let private tryInferSafeNavigationMemberType
        (aliases: Map<string, TypeAliasInfo>)
        (inferArgumentType: SurfaceExpression -> TypeExpr option)
        payloadType
        (navigation: SurfaceSafeNavigationMember)
        =
        tryProjectVisibleRecordType aliases payloadType navigation.Segments
        |> Option.bind (fun headType -> tryInferVisibleAppliedType aliases inferArgumentType headType navigation.Arguments)

    let rec private tryParseTypeLikeSurfaceExpression expression =
        match expression with
        | Name segments when not (List.isEmpty segments) ->
            Some(TypeName(segments, []))
        | Apply(head, arguments) ->
            match tryParseTypeLikeSurfaceExpression head, arguments |> List.map tryParseTypeLikeSurfaceExpression with
            | Some(TypeName(name, existingArguments)), parsedArguments when parsedArguments |> List.forall Option.isSome ->
                Some(TypeName(name, existingArguments @ (parsedArguments |> List.choose id)))
            | _ ->
                None
        | _ ->
            None

    let private tryResolveTraitMemberProjection
        (environment: BindingLoweringEnvironment)
        (localTypes: Map<string, TypeExpr>)
        nameSegments
        =
        match nameSegments with
        | receiverName :: [ memberName ] ->
            localTypes
            |> Map.tryFind receiverName
            |> Option.orElseWith (fun () ->
                environment.VisibleBindings
                |> Map.tryFind receiverName
                |> Option.map (fun bindingInfo -> snd (TypeSignatures.schemeParts bindingInfo.Scheme)))
            |> Option.bind (fun receiverType ->
                match normalizeTypeAliases environment.VisibleTypeAliases receiverType with
                | TypeName([ traitName ], arguments) when environment.VisibleTraits.ContainsKey(traitName) ->
                    let traitInfo = environment.VisibleTraits[traitName]

                    if traitInfo.Members.ContainsKey(memberName) then
                        Some(traitInfo, memberName, arguments, receiverName)
                    else
                        None
                | TypeName(qualifiedName, arguments) ->
                    let traitName = SyntaxFacts.moduleNameToText qualifiedName

                    if environment.VisibleTraits.ContainsKey(traitName)
                       && environment.VisibleTraits[traitName].Members.ContainsKey(memberName) then
                        Some(environment.VisibleTraits[traitName], memberName, arguments, receiverName)
                    else
                        None
                | _ ->
                    None)
        | _ ->
            None

    let private resolveConstraintInstance
        (environment: BindingLoweringEnvironment)
        (constraintInfo: TraitConstraint)
        =
        let normalizeConstraint (constraintInfo: TraitConstraint) =
            let normalizedArguments =
                constraintInfo.Arguments
                |> List.map (normalizeTypeAliases environment.VisibleTypeAliases >> TypeSignatures.toText)
                |> String.concat ","

            $"{constraintInfo.TraitName}({normalizedArguments})"

        let rec solve visited (goal: TraitConstraint) =
            let goalKey = normalizeConstraint goal

            if Set.contains goalKey visited then
                None
            else
                let visited = Set.add goalKey visited

                environment.VisibleInstances
                |> List.tryPick (fun instanceInfo ->
                    if not (String.Equals(instanceInfo.TraitName, goal.TraitName, StringComparison.Ordinal)) then
                        None
                    elif List.length instanceInfo.HeadTypes <> List.length goal.Arguments then
                        None
                    else
                        tryUnifyVisibleTypes environment.VisibleTypeAliases (List.zip instanceInfo.HeadTypes goal.Arguments)
                        |> Option.bind (fun substitution ->
                            let instantiatedPremises =
                                instanceInfo.Constraints
                                |> List.map (TypeSignatures.applyConstraintSubstitution substitution)

                            if instantiatedPremises |> List.forall (solve visited >> Option.isSome) then
                                Some instanceInfo
                            else
                                None))

        solve Set.empty constraintInfo

    let private trySynthesizeImplicitArgument
        (environment: BindingLoweringEnvironment)
        parameterType
        =
        match normalizeTypeAliases environment.VisibleTypeAliases parameterType with
        | TypeName([ traitName ], arguments) when environment.VisibleTraits.ContainsKey(traitName) ->
            resolveConstraintInstance environment { TraitName = traitName; Arguments = arguments }
            |> Option.map (fun instanceInfo ->
                KCoreDictionaryValue(instanceInfo.ModuleName, instanceInfo.TraitName, instanceInfo.InstanceKey))
        | TypeName(qualifiedName, arguments) ->
            let traitName = SyntaxFacts.moduleNameToText qualifiedName

            if environment.VisibleTraits.ContainsKey(traitName) then
                resolveConstraintInstance environment { TraitName = traitName; Arguments = arguments }
                |> Option.map (fun instanceInfo ->
                    KCoreDictionaryValue(instanceInfo.ModuleName, instanceInfo.TraitName, instanceInfo.InstanceKey))
            else
                None
        | _ ->
            None

    let private tryPrepareVisibleBindingCall
        (environment: BindingLoweringEnvironment)
        (freshCounter: int ref)
        (bindingInfo: BindingSchemeInfo)
        (inferArgumentType: SurfaceExpression -> TypeExpr option)
        (arguments: SurfaceExpression list)
        =
        let instantiated = TypeSignatures.instantiate "t" freshCounter.Value bindingInfo.Scheme
        freshCounter.Value <- freshCounter.Value + instantiated.Forall.Length

        match bindingInfo.ParameterLayouts with
        | None ->
            let argumentTypes = arguments |> List.map inferArgumentType

            if argumentTypes |> List.exists Option.isNone then
                None
            else
                unwrapInstantiatedCallType environment freshCounter bindingInfo.Scheme (argumentTypes |> List.choose id)
                |> Option.map (fun (instantiatedScheme, resultType) ->
                    { InstantiatedScheme = instantiatedScheme
                      ResultType = resultType
                      Parameters = []
                      Arguments = arguments |> List.map ExplicitArgument })
        | Some parameterLayouts ->
            let parameterTypes, resultType = TypeSignatures.schemeParts instantiated

            if List.length parameterLayouts <> List.length parameterTypes then
                None
            else
                let mutable remainingArguments = arguments
                let mutable substitution = Map.empty<string, TypeExpr>
                let preparedArguments = ResizeArray<PreparedCallArgument>()
                let preparedParameters = ResizeArray<PreparedCallParameter>()
                let mutable success = true

                for parameterLayout, rawParameterType in List.zip parameterLayouts parameterTypes do
                    let parameterType = TypeSignatures.applySubstitution substitution rawParameterType

                    if parameterLayout.IsImplicit then
                        match trySynthesizeImplicitArgument environment parameterType with
                        | Some implicitArgument ->
                            preparedArguments.Add(ImplicitArgument implicitArgument)
                            preparedParameters.Add(
                                { Layout = Some parameterLayout
                                  ParameterType = parameterType
                                  AssignedArgument = Some(ImplicitArgument implicitArgument) }
                            )
                        | None ->
                            success <- false
                    elif isCompileTimeArgumentType environment.VisibleTypeAliases parameterType then
                        match remainingArguments with
                        | nextArgument :: rest ->
                            match tryParseTypeLikeSurfaceExpression nextArgument with
                            | Some explicitTypeArgument ->
                                remainingArguments <- rest
                                substitution <-
                                    composeTypeSubstitution
                                        substitution
                                        (Map.ofList [ parameterLayout.Name, explicitTypeArgument ])
                                preparedParameters.Add(
                                    { Layout = Some parameterLayout
                                      ParameterType = parameterType
                                      AssignedArgument = None }
                                )
                            | None ->
                                success <- false
                        | [] ->
                            success <- false
                    else
                        match remainingArguments with
                        | nextArgument :: rest ->
                            match inferArgumentType nextArgument with
                            | Some argumentType ->
                                match tryUnifyVisibleTypes environment.VisibleTypeAliases [ parameterType, argumentType ] with
                                | Some inferredSubstitution ->
                                    preparedArguments.Add(ExplicitArgument nextArgument)
                                    preparedParameters.Add(
                                        { Layout = Some parameterLayout
                                          ParameterType = parameterType
                                          AssignedArgument = Some(ExplicitArgument nextArgument) }
                                    )
                                    remainingArguments <- rest
                                    substitution <- composeTypeSubstitution substitution inferredSubstitution
                                | None ->
                                    success <- false
                            | None ->
                                success <- false
                        | [] ->
                            success <- false

                if success && List.isEmpty remainingArguments then
                    let instantiatedScheme = TypeSignatures.applySchemeSubstitution substitution instantiated
                    let appliedResultType = TypeSignatures.applySubstitution substitution resultType

                    Some
                        { InstantiatedScheme = instantiatedScheme
                          ResultType = appliedResultType
                          Parameters = List.ofSeq preparedParameters
                          Arguments = List.ofSeq preparedArguments }
                else
                    None

    let private tryPrepareVisibleTraitMemberCall
        (environment: BindingLoweringEnvironment)
        (freshCounter: int ref)
        memberName
        (inferArgumentType: SurfaceExpression -> TypeExpr option)
        (arguments: SurfaceExpression list)
        =
        let candidates =
            environment.VisibleTraits
            |> Map.toList
            |> List.choose (fun (_, traitInfo) ->
                traitInfo.Members
                |> Map.tryFind memberName
                |> Option.bind (fun memberInfo ->
                    let candidateCounter = ref freshCounter.Value

                    let overloadedScheme =
                        { memberInfo.Scheme with
                            Constraints =
                                { TraitName = traitInfo.Name
                                  Arguments = traitInfo.TypeParameters |> List.map TypeVariable }
                                :: memberInfo.Scheme.Constraints }

                    let bindingInfo =
                        { ModuleName = traitInfo.ModuleName
                          Name = memberName
                          Scheme = overloadedScheme
                          ParameterLayouts = None }

                    tryPrepareVisibleBindingCall environment candidateCounter bindingInfo inferArgumentType arguments
                    |> Option.bind (fun preparedCall ->
                        match preparedCall.InstantiatedScheme.Constraints with
                        | owningConstraint :: [] when String.Equals(owningConstraint.TraitName, traitInfo.Name, StringComparison.Ordinal) ->
                            resolveConstraintInstance environment owningConstraint
                            |> Option.map (fun instanceInfo ->
                                traitInfo, memberInfo, preparedCall, instanceInfo, candidateCounter.Value)
                        | _ ->
                            None)))

        match candidates with
        | [ traitInfo, memberInfo, preparedCall, instanceInfo, nextFreshValue ] ->
            freshCounter.Value <- nextFreshValue
            Some(traitInfo, memberInfo, preparedCall, instanceInfo)
        | _ ->
            None


    let rec private inferValidationExpressionType
        (environment: BindingLoweringEnvironment)
        (freshCounter: int ref)
        (localTypes: Map<string, TypeExpr>)
        expression
        =
        match expression with
        | Literal literal ->
            Some(inferLiteralType literal)
        | Name [ "True" ]
        | Name [ "False" ] ->
            Some boolType
        | Name(root :: path) ->
            localTypes
            |> Map.tryFind root
            |> Option.orElseWith (fun () ->
                environment.VisibleBindings
                |> Map.tryFind root
                |> Option.bind (fun bindingInfo ->
                    if List.isEmpty path then
                        tryPrepareVisibleBindingCall
                            environment
                            freshCounter
                            bindingInfo
                            (inferValidationExpressionType environment freshCounter localTypes)
                            []
                        |> Option.map (fun preparedCall -> preparedCall.ResultType)
                        |> Option.orElseWith (fun () -> instantiateVisibleBindingResultType environment freshCounter root)
                    else
                        instantiateVisibleBindingResultType environment freshCounter root))
            |> Option.orElseWith (fun () ->
                environment.VisibleConstructors
                |> Map.tryFind root
                |> Option.bind (fun constructorInfo ->
                    if List.isEmpty path then
                        tryPrepareVisibleBindingCall
                            environment
                            freshCounter
                            constructorInfo
                            (inferValidationExpressionType environment freshCounter localTypes)
                            []
                        |> Option.map (fun preparedCall -> preparedCall.ResultType)
                    else
                        None))
            |> Option.bind (fun rootType -> tryProjectVisibleRecordType environment.VisibleTypeAliases rootType path)
        | Name [] ->
            None
        | LocalLet(binding, value, body) ->
            let bindingNames = collectPatternNames binding.Pattern

            let nextLocals =
                match inferValidationExpressionType environment freshCounter localTypes value with
                | Some valueType ->
                    bindingNames
                    |> List.fold (fun state name -> Map.add name valueType state) localTypes
                | None ->
                    localTypes

            inferValidationExpressionType environment freshCounter nextLocals body
        | Lambda(parameters, body) ->
            let parameterTypes =
                parameters
                |> List.map (fun parameter ->
                    tryParseParameterType parameter
                    |> Option.defaultValue (TypeVariable $"lambda{freshCounter.Value}"))

            let lambdaLocals =
                List.zip parameters parameterTypes
                |> List.fold (fun state (parameter, parameterType) -> Map.add parameter.Name parameterType state) localTypes

            inferValidationExpressionType environment freshCounter lambdaLocals body
            |> Option.map (fun resultType ->
                parameterTypes
                |> List.rev
                |> List.fold (fun state parameterType -> TypeArrow(QuantityOmega, parameterType, state)) resultType)
        | IfThenElse(_, whenTrue, whenFalse) ->
            match
                inferValidationExpressionType environment freshCounter localTypes whenTrue,
                inferValidationExpressionType environment freshCounter localTypes whenFalse
            with
            | Some trueType, Some falseType when trueType = falseType ->
                Some trueType
            | _ ->
                None
        | Match(scrutinee, cases) ->
            let scrutineeType =
                inferValidationExpressionType environment freshCounter localTypes scrutinee

            cases
            |> List.choose (fun caseClause ->
                let caseLocals =
                    extendPatternLocalTypes environment freshCounter localTypes scrutineeType caseClause.Pattern

                inferValidationExpressionType environment freshCounter caseLocals caseClause.Body)
            |> function
                | first :: rest when rest |> List.forall ((=) first) -> Some first
                | _ -> None
        | RecordLiteral fields ->
            let inferredFields =
                fields
                |> List.map (fun field ->
                    inferValidationExpressionType environment freshCounter localTypes field.Value
                    |> Option.map (fun fieldType ->
                        ({ Name = field.Name
                           Quantity = if field.IsImplicit then QuantityZero else QuantityOmega
                           Type = fieldType }: TypeSignatures.RecordField)))

            if inferredFields |> List.forall Option.isSome then
                inferredFields
                |> List.choose id
                |> TypeRecord
                |> Some
            else
                None
        | Seal(value, _) ->
            inferValidationExpressionType environment freshCounter localTypes value
        | RecordUpdate(receiver, _) ->
            inferValidationExpressionType environment freshCounter localTypes receiver
        | SafeNavigation(receiver, navigation) ->
            inferValidationExpressionType environment freshCounter localTypes receiver
            |> Option.bind (fun receiverType ->
                tryOptionPayloadType environment.VisibleTypeAliases receiverType
                |> Option.bind (fun payloadType ->
                    tryInferSafeNavigationMemberType
                        environment.VisibleTypeAliases
                        (inferValidationExpressionType environment freshCounter localTypes)
                        payloadType
                        navigation
                    |> Option.map (fun memberType ->
                        match tryOptionPayloadType environment.VisibleTypeAliases memberType with
                        | Some _ -> memberType
                        | None -> TypeName([ "Option" ], [ memberType ]))))
        | TagTest _ ->
            Some boolType
        | Do statements ->
            inferValidationDoResultType environment freshCounter localTypes statements
        | MonadicSplice inner ->
            inferValidationExpressionType environment freshCounter localTypes inner
            |> Option.map unwrapIoType
        | InoutArgument inner ->
            inferValidationExpressionType environment freshCounter localTypes inner
        | Apply(Name [ calleeName ], arguments) ->
            environment.VisibleBindings
            |> Map.tryFind calleeName
            |> Option.bind (fun bindingInfo ->
                tryPrepareVisibleBindingCall
                    environment
                    freshCounter
                    bindingInfo
                    (inferValidationExpressionType environment freshCounter localTypes)
                    arguments
                |> Option.map (fun preparedCall -> preparedCall.ResultType))
            |> Option.orElseWith (fun () ->
                environment.VisibleConstructors
                |> Map.tryFind calleeName
                |> Option.bind (fun constructorInfo ->
                    tryPrepareVisibleBindingCall
                        environment
                        freshCounter
                        constructorInfo
                        (inferValidationExpressionType environment freshCounter localTypes)
                        arguments
                    |> Option.map (fun preparedCall -> preparedCall.ResultType)))
            |> Option.orElseWith (fun () ->
                tryPrepareVisibleTraitMemberCall
                    environment
                    freshCounter
                    calleeName
                    (inferValidationExpressionType environment freshCounter localTypes)
                    arguments
                |> Option.map (fun (_, _, preparedCall, _) -> preparedCall.ResultType))
        | Apply _ ->
            None
        | Unary("not", _) ->
            Some boolType
        | Unary("negate", operand) ->
            inferValidationExpressionType environment freshCounter localTypes operand
        | Unary _ ->
            None
        | Binary(left, ("+" | "-" | "*" | "/"), right) ->
            match
                inferValidationExpressionType environment freshCounter localTypes left,
                inferValidationExpressionType environment freshCounter localTypes right
            with
            | Some leftType, Some rightType when leftType = intType && rightType = intType ->
                Some intType
            | Some leftType, Some rightType when leftType = floatType && rightType = floatType ->
                Some floatType
            | _ ->
                None
        | Binary(_, ("==" | "!=" | "<" | ">" | "<=" | ">=" | "&&" | "||"), _) ->
            Some boolType
        | Binary(left, operatorName, right) ->
            inferValidationExpressionType environment freshCounter localTypes (Apply(Name [ operatorName ], [ left; right ]))
        | Elvis(left, right) ->
            match
                inferValidationExpressionType environment freshCounter localTypes left,
                inferValidationExpressionType environment freshCounter localTypes right
            with
            | Some leftType, Some defaultType ->
                tryOptionPayloadType environment.VisibleTypeAliases leftType
                |> Option.bind (fun payloadType ->
                    tryUnifyVisibleTypes environment.VisibleTypeAliases [ payloadType, defaultType ]
                    |> Option.map (fun substitution -> TypeSignatures.applySubstitution substitution payloadType)
                    |> Option.orElseWith (fun () ->
                        if
                            TypeSignatures.definitionallyEqual
                                (normalizeTypeAliases environment.VisibleTypeAliases payloadType)
                                (normalizeTypeAliases environment.VisibleTypeAliases defaultType)
                        then
                            Some payloadType
                        else
                            None))
            | _ ->
                None
        | PrefixedString _ ->
            Some stringType

    and private inferValidationDoResultType
        (environment: BindingLoweringEnvironment)
        (freshCounter: int ref)
        (localTypes: Map<string, TypeExpr>)
        statements
        =
        match statements with
        | [] ->
            Some unitType
        | DoExpression expression :: [] ->
            inferValidationExpressionType environment freshCounter localTypes expression
            |> Option.map unwrapIoType
        | DoExpression _ :: rest ->
            inferValidationDoResultType environment freshCounter localTypes rest
        | DoLet(binding, expression) :: rest ->
            let nextLocals =
                inferValidationExpressionType environment freshCounter localTypes expression
                |> Option.map (fun valueType ->
                    extendPatternLocalTypes environment freshCounter localTypes (Some valueType) binding.Pattern)
                |> Option.defaultValue localTypes

            inferValidationDoResultType environment freshCounter nextLocals rest
        | DoLetQuestion(binding, expression, failure) :: rest ->
            let nextLocals =
                inferValidationExpressionType environment freshCounter localTypes expression
                |> Option.map (fun valueType ->
                    extendPatternLocalTypes environment freshCounter localTypes (Some valueType) binding.Pattern)
                |> Option.defaultValue localTypes

            failure
            |> Option.iter (fun failure ->
                inferValidationDoResultType environment freshCounter localTypes failure.Body |> ignore)

            inferValidationDoResultType environment freshCounter nextLocals rest
        | DoBind(binding, expression) :: rest ->
            let nextLocals =
                inferValidationExpressionType environment freshCounter localTypes expression
                |> Option.map (fun valueType ->
                    extendPatternLocalTypes
                        environment
                        freshCounter
                        localTypes
                        (Some(unwrapIoType valueType))
                        binding.Pattern)
                |> Option.defaultValue localTypes

            inferValidationDoResultType environment freshCounter nextLocals rest
        | DoUsing(binding, expression) :: rest ->
            let nextLocals =
                inferValidationExpressionType environment freshCounter localTypes expression
                |> Option.map (fun valueType ->
                    extendPatternLocalTypes
                        environment
                        freshCounter
                        localTypes
                        (Some(unwrapIoType valueType))
                        binding.Pattern)
                |> Option.defaultValue localTypes

            inferValidationDoResultType environment freshCounter nextLocals rest
        | DoVar(bindingName, expression) :: rest ->
            let nextLocals =
                match inferValidationExpressionType environment freshCounter localTypes expression with
                | Some valueType -> Map.add bindingName (refType valueType) localTypes
                | None -> localTypes

            inferValidationDoResultType environment freshCounter nextLocals rest
        | DoAssign _ :: rest ->
            inferValidationDoResultType environment freshCounter localTypes rest
        | DoIf(_, _, _) :: rest ->
            inferValidationDoResultType environment freshCounter localTypes rest
        | DoWhile _ :: rest ->
            inferValidationDoResultType environment freshCounter localTypes rest
        | DoReturn expression :: _ ->
            inferValidationExpressionType environment freshCounter localTypes expression

    let private validateBuiltInExpressionsForBinding
        (environment: BindingLoweringEnvironment)
        (definition: LetDefinition)
        (scheme: TypeScheme option)
        =
        let freshCounter = ref 0
        let localTypes = buildLocalTypes scheme definition.Parameters

        let makeDiagnostic code message =
            { Severity = DiagnosticSeverity.Error
              Code = code
              Stage = Some "KFrontIR"
              Phase = Some(KFrontIRPhase.phaseName CORE_LOWERING)
              Message = message
              Location = None
              RelatedLocations = [] }

        let memberText (navigation: SurfaceSafeNavigationMember) =
            let argumentsText =
                navigation.Arguments
                |> List.map (fun _ -> "<arg>")
                |> String.concat " "

            let segmentsText = String.concat "." navigation.Segments
            match argumentsText with
            | "" -> segmentsText
            | _ -> $"{segmentsText} {argumentsText}"

        let rec validateExpression locals expression =
            let recurse = validateExpression locals

            match expression with
            | Literal _
            | Name _ ->
                []
            | LocalLet(binding, value, body) ->
                let nextLocals =
                    match inferValidationExpressionType environment freshCounter locals value with
                    | Some valueType ->
                        collectPatternNames binding.Pattern
                        |> List.fold (fun state name -> Map.add name valueType state) locals
                    | None ->
                        locals

                recurse value @ validateExpression nextLocals body
            | Lambda(parameters, body) ->
                let nextLocals =
                    parameters
                    |> List.fold (fun state parameter ->
                        match tryParseParameterType parameter with
                        | Some parameterType -> Map.add parameter.Name parameterType state
                        | None -> state) locals

                validateExpression nextLocals body
            | IfThenElse(condition, whenTrue, whenFalse) ->
                recurse condition @ recurse whenTrue @ recurse whenFalse
            | Match(scrutinee, cases) ->
                let caseDiagnostics =
                    cases
                    |> List.collect (fun caseClause ->
                        (caseClause.Guard |> Option.map recurse |> Option.defaultValue [])
                        @ recurse caseClause.Body)

                recurse scrutinee @ caseDiagnostics
            | RecordLiteral fields ->
                fields |> List.collect (fun field -> recurse field.Value)
            | Seal(value, _) ->
                recurse value
            | RecordUpdate(receiver, fields) ->
                recurse receiver @ (fields |> List.collect (fun field -> recurse field.Value))
            | SafeNavigation(receiver, navigation) ->
                let receiverDiagnostics = recurse receiver
                let argumentDiagnostics = navigation.Arguments |> List.collect recurse

                let builtinDiagnostics =
                    match inferValidationExpressionType environment freshCounter locals receiver with
                    | Some receiverType ->
                        match tryOptionPayloadType environment.VisibleTypeAliases receiverType with
                        | Some payloadType ->
                            match
                                tryInferSafeNavigationMemberType
                                    environment.VisibleTypeAliases
                                    (inferValidationExpressionType environment freshCounter locals)
                                    payloadType
                                    navigation
                            with
                            | Some _ ->
                                []
                            | None ->
                                [ makeDiagnostic
                                      DiagnosticCode.SafeNavigationAmbiguous
                                      $"Safe-navigation `?.` requires knowing the result type of '{memberText navigation}' to decide whether to wrap or flatten." ]
                        | None ->
                            [ makeDiagnostic
                                  DiagnosticCode.SafeNavigationReceiverNotOption
                                  "Safe-navigation `?.` requires its receiver to have type `Option T` for some `T`." ]
                    | None ->
                        [ makeDiagnostic
                              DiagnosticCode.SafeNavigationAmbiguous
                              $"Safe-navigation `?.` requires knowing the result type of '{memberText navigation}' to decide whether to wrap or flatten." ]

                receiverDiagnostics @ argumentDiagnostics @ builtinDiagnostics
            | TagTest(receiver, _) ->
                recurse receiver
            | Do statements ->
                validateDoStatements locals statements
            | MonadicSplice inner
            | InoutArgument inner
            | Unary(_, inner) ->
                recurse inner
            | Apply(callee, arguments) ->
                recurse callee @ (arguments |> List.collect recurse)
            | Binary(left, _, right)
            | Elvis(left, right) ->
                let diagnostics = recurse left @ recurse right

                match expression with
                | Elvis(left, _) ->
                    match inferValidationExpressionType environment freshCounter locals left with
                    | Some leftType when tryOptionPayloadType environment.VisibleTypeAliases leftType |> Option.isNone ->
                        diagnostics
                        @ [ makeDiagnostic
                                DiagnosticCode.ElvisReceiverNotOption
                                "Elvis `?:` requires its left-hand side to have type `Option T` for some `T`." ]
                    | _ ->
                        diagnostics
                | _ ->
                    diagnostics
            | PrefixedString(_, parts) ->
                parts
                |> List.collect (function
                    | StringText _ -> []
                    | StringInterpolation inner -> recurse inner)

        and validateDoStatements locals statements =
            match statements with
            | [] ->
                []
            | statement :: rest ->
                match statement with
                | DoLet(binding, expression)
                | DoBind(binding, expression) ->
                    let nextLocals =
                        match inferValidationExpressionType environment freshCounter locals expression with
                        | Some valueType ->
                            collectPatternNames binding.Pattern
                            |> List.fold (fun state name -> Map.add name valueType state) locals
                        | None ->
                            locals

                    validateExpression locals expression @ validateDoStatements nextLocals rest
                | DoLetQuestion(binding, expression, failure) ->
                    let nextLocals =
                        match inferValidationExpressionType environment freshCounter locals expression with
                        | Some valueType ->
                            collectPatternNames binding.Pattern
                            |> List.fold (fun state name -> Map.add name valueType state) locals
                        | None ->
                            locals

                    let failureDiagnostics =
                        match failure with
                        | Some failure -> validateDoStatements locals failure.Body
                        | None -> []

                    validateExpression locals expression
                    @ failureDiagnostics
                    @ validateDoStatements nextLocals rest
                | DoUsing(binding, expression) ->
                    let nextLocals =
                        match binding.Pattern, inferValidationExpressionType environment freshCounter locals expression with
                        | NamePattern bindingName, Some valueType -> Map.add bindingName (unwrapIoType valueType) locals
                        | _ -> locals

                    validateExpression locals expression @ validateDoStatements nextLocals rest
                | DoVar(bindingName, expression) ->
                    let nextLocals =
                        match inferValidationExpressionType environment freshCounter locals expression with
                        | Some valueType -> Map.add bindingName (refType valueType) locals
                        | None -> locals

                    validateExpression locals expression @ validateDoStatements nextLocals rest
                | DoAssign(_, expression)
                | DoExpression expression ->
                    validateExpression locals expression @ validateDoStatements locals rest
                | DoIf(condition, whenTrue, whenFalse) ->
                    validateExpression locals condition
                    @ validateDoStatements locals whenTrue
                    @ validateDoStatements locals whenFalse
                    @ validateDoStatements locals rest
                | DoWhile(condition, body) ->
                    validateExpression locals condition
                    @ validateDoStatements locals body
                    @ validateDoStatements locals rest
                | DoReturn expression ->
                    validateExpression locals expression

        match definition.Body with
        | Some body -> validateExpression localTypes body
        | None -> []

    let validateSurfaceModules (frontendModules: KFrontIRModule list) =
        let surfaceIndex = buildSurfaceIndex frontendModules

        frontendModules
        |> List.collect (fun frontendModule ->
            let moduleName = moduleNameText frontendModule.ModuleIdentity

            let environment =
                { CurrentModuleName = moduleName
                  VisibleTypeAliases = mergeVisibleTypeAliases surfaceIndex moduleName
                  VisibleBindings = mergeVisibleBindings surfaceIndex moduleName
                  VisibleConstructors = mergeVisibleConstructors surfaceIndex moduleName
                  VisibleProjections = mergeVisibleProjections surfaceIndex moduleName
                  VisibleTraits = mergeVisibleTraits surfaceIndex moduleName
                  VisibleInstances = visibleInstances surfaceIndex moduleName
                  ConstrainedMembers = Map.empty }

            frontendModule.Declarations
            |> List.collect (function
                | LetDeclaration definition ->
                    let scheme =
                        definition.Name
                        |> Option.bind (fun name -> Map.tryFind name environment.VisibleBindings)
                        |> Option.map (fun bindingInfo -> bindingInfo.Scheme)

                    validateBuiltInExpressionsForBinding environment definition scheme
                | _ ->
                    []))

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
        (recordLayouts: Map<string, SyntheticRecordLayout> ref)
        (scheme: TypeScheme option)
        (parameters: Parameter list)
        (body: SurfaceExpression)
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

        let freshSyntheticName prefix =
            let index = freshCounter.Value
            freshCounter.Value <- freshCounter.Value + 1
            $"{prefix}{index}"

        let rec inferExpressionType localTypes expression =
            match expression with
            | Literal literal ->
                Some(inferLiteralType literal)
            | Name [ "True" ]
            | Name [ "False" ] ->
                Some boolType
            | Name(root :: path) ->
                localTypes
                |> Map.tryFind root
                |> Option.orElseWith (fun () ->
                    environment.VisibleBindings
                    |> Map.tryFind root
                    |> Option.bind (fun bindingInfo ->
                        if List.isEmpty path then
                            tryPrepareVisibleBindingCall
                                environment
                                freshCounter
                                bindingInfo
                                (inferExpressionType localTypes)
                                []
                            |> Option.map (fun preparedCall -> preparedCall.ResultType)
                            |> Option.orElseWith (fun () -> instantiateVisibleBindingResultType environment freshCounter root)
                        else
                            None))
                |> Option.orElseWith (fun () ->
                    environment.VisibleConstructors
                    |> Map.tryFind root
                    |> Option.bind (fun constructorInfo ->
                        if List.isEmpty path then
                            tryPrepareVisibleBindingCall
                                environment
                                freshCounter
                                constructorInfo
                                (inferExpressionType localTypes)
                                []
                            |> Option.map (fun preparedCall -> preparedCall.ResultType)
                        else
                            None))
                |> Option.bind (fun rootType -> tryProjectVisibleRecordType environment.VisibleTypeAliases rootType path)
            | Name [] ->
                None
            | LocalLet(binding, value, body) ->
                let bindingNames = collectPatternNames binding.Pattern

                let nextLocals =
                    match inferExpressionType localTypes value with
                    | Some valueType ->
                        bindingNames
                        |> List.fold (fun state name -> Map.add name valueType state) localTypes
                    | None -> localTypes

                inferExpressionType nextLocals body
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
                    |> List.fold (fun state parameterType -> TypeArrow(QuantityOmega, parameterType, state)) resultType)
            | IfThenElse(_, whenTrue, whenFalse) ->
                match inferExpressionType localTypes whenTrue, inferExpressionType localTypes whenFalse with
                | Some trueType, Some falseType when trueType = falseType ->
                    Some trueType
                | _ ->
                    None
            | Match(scrutinee, cases) ->
                let scrutineeType = inferExpressionType localTypes scrutinee

                cases
                |> List.choose (fun caseClause ->
                    let caseLocals =
                        extendPatternLocalTypes environment freshCounter localTypes scrutineeType caseClause.Pattern

                    inferExpressionType caseLocals caseClause.Body)
                |> function
                    | first :: rest when rest |> List.forall ((=) first) -> Some first
                    | _ -> None
            | RecordLiteral fields ->
                let inferredFields =
                    fields
                    |> List.map (fun field ->
                        inferExpressionType localTypes field.Value
                        |> Option.map (fun fieldType ->
                            ({ Name = field.Name
                               Quantity = if field.IsImplicit then QuantityZero else QuantityOmega
                               Type = fieldType }: TypeSignatures.RecordField)))

                if inferredFields |> List.forall Option.isSome then
                    inferredFields
                    |> List.choose id
                    |> TypeRecord
                    |> Some
                else
                    None
            | Seal(value, _) ->
                inferExpressionType localTypes value
            | RecordUpdate(receiver, _) ->
                inferExpressionType localTypes receiver
            | SafeNavigation(receiver, navigation) ->
                inferExpressionType localTypes receiver
                |> Option.bind (fun receiverType ->
                    tryOptionPayloadType environment.VisibleTypeAliases receiverType
                    |> Option.bind (fun payloadType ->
                        tryInferSafeNavigationMemberType
                            environment.VisibleTypeAliases
                            (inferExpressionType localTypes)
                            payloadType
                            navigation
                        |> Option.map (fun memberType ->
                            match tryOptionPayloadType environment.VisibleTypeAliases memberType with
                            | Some _ -> memberType
                            | None -> TypeName([ "Option" ], [ memberType ]))))
            | TagTest _ ->
                Some boolType
            | Do statements ->
                inferDoResultType localTypes statements
            | MonadicSplice inner ->
                inferExpressionType localTypes inner |> Option.map unwrapIoType
            | InoutArgument inner ->
                inferExpressionType localTypes inner
            | Apply(Name [ calleeName ], arguments) ->
                environment.VisibleBindings
                |> Map.tryFind calleeName
                |> Option.bind (fun bindingInfo ->
                    tryPrepareVisibleBindingCall
                        environment
                        freshCounter
                        bindingInfo
                        (inferExpressionType localTypes)
                        arguments
                    |> Option.map (fun preparedCall -> preparedCall.ResultType))
                |> Option.orElseWith (fun () ->
                    environment.VisibleConstructors
                    |> Map.tryFind calleeName
                    |> Option.bind (fun constructorInfo ->
                        tryPrepareVisibleBindingCall
                            environment
                            freshCounter
                            constructorInfo
                            (inferExpressionType localTypes)
                            arguments
                        |> Option.map (fun preparedCall -> preparedCall.ResultType)))
                |> Option.orElseWith (fun () ->
                    tryPrepareVisibleTraitMemberCall
                        environment
                        freshCounter
                        calleeName
                        (inferExpressionType localTypes)
                        arguments
                    |> Option.map (fun (_, _, preparedCall, _) -> preparedCall.ResultType))
                |> Option.orElseWith (fun () ->
                    environment.VisibleProjections
                    |> Map.tryFind calleeName
                    |> Option.bind (fun projectionInfo ->
                        tryInferProjectionCallResultType localTypes projectionInfo arguments))
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
            | Elvis(left, right) ->
                match inferExpressionType localTypes left, inferExpressionType localTypes right with
                | Some leftType, Some defaultType ->
                    tryOptionPayloadType environment.VisibleTypeAliases leftType
                    |> Option.bind (fun payloadType ->
                        tryUnifyVisibleTypes environment.VisibleTypeAliases [ payloadType, defaultType ]
                        |> Option.map (fun substitution ->
                            TypeSignatures.applySubstitution substitution payloadType)
                        |> Option.orElseWith (fun () ->
                            if
                                TypeSignatures.definitionallyEqual
                                    (normalizeTypeAliases environment.VisibleTypeAliases payloadType)
                                    (normalizeTypeAliases environment.VisibleTypeAliases defaultType)
                            then
                                Some payloadType
                            else
                                None))
                | _ ->
                    None
            | PrefixedString _ ->
                Some stringType

        and tryInferProjectionCallResultType localTypes (projectionInfo: ProjectionInfo) arguments =
            if List.length projectionInfo.Binders <> List.length arguments then
                None
            else
                let tryUnifyExpected expectedType actualType substitution =
                    let expectedType = TypeSignatures.applySubstitution substitution expectedType

                    tryUnifyVisibleTypes environment.VisibleTypeAliases [ expectedType, actualType ]
                    |> Option.map (composeTypeSubstitution substitution)
                    |> Option.orElseWith (fun () ->
                        if
                            TypeSignatures.definitionallyEqual
                                (normalizeTypeAliases environment.VisibleTypeAliases expectedType)
                                (normalizeTypeAliases environment.VisibleTypeAliases actualType)
                        then
                            Some substitution
                        else
                            None)

                let rec tryInferPlaceExpressionType expression =
                    match expression with
                    | Name(root :: path) ->
                        localTypes
                        |> Map.tryFind root
                        |> Option.bind (fun rootType ->
                            tryProjectVisibleRecordType environment.VisibleTypeAliases rootType path)
                    | Apply(Name [ projectionName ], projectionArguments) ->
                        environment.VisibleProjections
                        |> Map.tryFind projectionName
                        |> Option.bind (fun nestedProjectionInfo ->
                            tryInferProjectionCallResultType localTypes nestedProjectionInfo projectionArguments)
                    | _ ->
                        None

                (Some Map.empty, List.zip projectionInfo.Binders arguments)
                ||> List.fold (fun current (binder, argument) ->
                    current
                    |> Option.bind (fun substitution ->
                        match binder with
                        | ProjectionPlaceBinder placeBinder ->
                            match TypeSignatures.parseType placeBinder.TypeTokens, tryInferPlaceExpressionType argument with
                            | Some expectedType, Some actualType ->
                                tryUnifyExpected expectedType actualType substitution
                            | _ ->
                                None
                        | ProjectionValueBinder valueBinder ->
                            match tryParseParameterType valueBinder, inferExpressionType localTypes argument with
                            | Some expectedType, Some actualType ->
                                tryUnifyExpected expectedType actualType substitution
                            | _ ->
                                None))
                |> Option.map (fun substitution ->
                    TypeSignatures.applySubstitution substitution projectionInfo.ReturnType)

        and inferDoResultType localTypes statements =
            match statements with
            | [] ->
                Some unitType
            | DoExpression expression :: [] ->
                inferExpressionType localTypes expression |> Option.map unwrapIoType
            | DoExpression _ :: rest ->
                inferDoResultType localTypes rest
            | DoLet(binding, expression) :: rest ->
                let nextLocals =
                    inferExpressionType localTypes expression
                    |> Option.map (fun valueType ->
                        extendPatternLocalTypes environment freshCounter localTypes (Some valueType) binding.Pattern)
                    |> Option.defaultValue localTypes

                inferDoResultType nextLocals rest
            | DoLetQuestion(binding, expression, failure) :: rest ->
                let nextLocals =
                    inferExpressionType localTypes expression
                    |> Option.map (fun valueType ->
                        extendPatternLocalTypes environment freshCounter localTypes (Some valueType) binding.Pattern)
                    |> Option.defaultValue localTypes

                failure
                |> Option.iter (fun failure -> inferDoResultType localTypes failure.Body |> ignore)

                inferDoResultType nextLocals rest
            | DoBind(binding, expression) :: rest ->
                let nextLocals =
                    inferExpressionType localTypes expression
                    |> Option.map (fun valueType ->
                        extendPatternLocalTypes
                            environment
                            freshCounter
                            localTypes
                            (Some(unwrapIoType valueType))
                            binding.Pattern)
                    |> Option.defaultValue localTypes

                inferDoResultType nextLocals rest
            | DoUsing(binding, expression) :: rest ->
                let nextLocals =
                    inferExpressionType localTypes expression
                    |> Option.map (fun valueType ->
                        extendPatternLocalTypes
                            environment
                            freshCounter
                            localTypes
                            (Some(unwrapIoType valueType))
                            binding.Pattern)
                    |> Option.defaultValue localTypes

                inferDoResultType nextLocals rest
            | DoVar(bindingName, expression) :: rest ->
                let nextLocals =
                    match inferExpressionType localTypes expression with
                    | Some valueType -> Map.add bindingName (refType valueType) localTypes
                    | None -> localTypes

                inferDoResultType nextLocals rest
            | DoAssign _ :: rest ->
                inferDoResultType localTypes rest
            | DoIf(_, _, _) :: rest ->
                inferDoResultType localTypes rest
            | DoWhile _ :: rest ->
                inferDoResultType localTypes rest
            | DoReturn expression :: _ ->
                inferExpressionType localTypes expression

        and buildUsingReleaseAction localTypes expression hiddenOwnedName =
            let defaultRelease =
                KCoreRelease(None, KCoreName [ "release" ], KCoreName [ hiddenOwnedName ])

            match inferExpressionType localTypes expression with
            | Some(TypeName(monadName, [ resourceType ])) ->
                let monadType = TypeName(monadName, [])

                let releasableConstraint =
                    { TraitName = "Releasable"
                      Arguments = [ monadType; resourceType ] }

                match resolveConstraintInstance environment releasableConstraint with
                | Some instanceInfo ->
                    KCoreRelease(
                        Some(TypeSignatures.toText resourceType),
                        KCoreTraitCall(
                            "Releasable",
                            "release",
                            KCoreDictionaryValue(instanceInfo.ModuleName, instanceInfo.TraitName, instanceInfo.InstanceKey),
                            []
                        ),
                        KCoreName [ hiddenOwnedName ]
                    )
                | None ->
                    defaultRelease
            | _ ->
                defaultRelease

        and buildPlaceWritebackExpression rootName path replacement =
            let rec loop currentSegments remainingPath =
                match remainingPath with
                | [] ->
                    replacement
                | fieldName :: rest ->
                    let nestedValue = loop (currentSegments @ [ fieldName ]) rest

                    RecordUpdate(
                        Name currentSegments,
                        [
                            { Name = fieldName
                              IsImplicit = false
                              Value = nestedValue }
                        ]
                    )

            loop [ rootName ] path

        and tryExpressionPlace expression =
            match expression with
            | Name(root :: path) ->
                Some(root, path)
            | _ ->
                None

        and unwrapRefLikeType typeExpr =
            match normalizeTypeAliases environment.VisibleTypeAliases typeExpr with
            | TypeName([ "Ref" ], [ inner ]) -> Some inner
            | _ -> None

        and substituteProjectionExpression
            (placeArguments: Map<string, string * string list>)
            (valueArguments: Map<string, SurfaceExpression>)
            expression
            =
            let rec loop current =
                match current with
                | Name(root :: path) ->
                    match Map.tryFind root placeArguments with
                    | Some(actualRoot, actualPath) ->
                        Name(actualRoot :: (actualPath @ path))
                    | None ->
                        match Map.tryFind root valueArguments with
                        | Some replacement when List.isEmpty path ->
                            replacement
                        | Some(Name replacementSegments) ->
                            Name(replacementSegments @ path)
                        | _ ->
                            current
                | Name [] ->
                    current
                | LocalLet(binding, value, body) ->
                    LocalLet(binding, loop value, loop body)
                | Lambda(parameters, body) ->
                    Lambda(parameters, loop body)
                | IfThenElse(condition, whenTrue, whenFalse) ->
                    IfThenElse(loop condition, loop whenTrue, loop whenFalse)
                | Match(scrutinee, cases) ->
                    Match(
                        loop scrutinee,
                        cases
                        |> List.map (fun caseClause ->
                            { caseClause with
                                Guard = caseClause.Guard |> Option.map loop
                                Body = loop caseClause.Body })
                    )
                | RecordLiteral fields ->
                    RecordLiteral(fields |> List.map (fun field -> { field with Value = loop field.Value }))
                | Seal(value, ascriptionTokens) ->
                    Seal(loop value, ascriptionTokens)
                | RecordUpdate(receiver, fields) ->
                    RecordUpdate(receiver |> loop, fields |> List.map (fun field -> { field with Value = loop field.Value }))
                | SafeNavigation(receiver, navigation) ->
                    SafeNavigation(receiver |> loop, { navigation with Arguments = navigation.Arguments |> List.map loop })
                | TagTest(receiver, constructorName) ->
                    TagTest(loop receiver, constructorName)
                | Do statements ->
                    let rec substituteDoStatement statement =
                        match statement with
                        | DoLet(binding, value) -> DoLet(binding, loop value)
                        | DoLetQuestion(binding, value, failure) ->
                            DoLetQuestion(
                                binding,
                                loop value,
                                failure
                                |> Option.map (fun failure ->
                                    { failure with
                                        Body = failure.Body |> List.map substituteDoStatement })
                            )
                        | DoBind(binding, value) -> DoBind(binding, loop value)
                        | DoVar(name, value) -> DoVar(name, loop value)
                        | DoAssign(name, value) -> DoAssign(name, loop value)
                        | DoUsing(binding, value) -> DoUsing(binding, loop value)
                        | DoIf(condition, whenTrue, whenFalse) ->
                            DoIf(
                                loop condition,
                                whenTrue |> List.map substituteDoStatement,
                                whenFalse |> List.map substituteDoStatement
                            )
                        | DoWhile(condition, body) ->
                            DoWhile(loop condition, body |> List.map substituteDoStatement)
                        | DoReturn value -> DoReturn(loop value)
                        | DoExpression value -> DoExpression(loop value)

                    Do(statements |> List.map substituteDoStatement)
                | MonadicSplice inner ->
                    MonadicSplice(loop inner)
                | Apply(callee, arguments) ->
                    Apply(loop callee, arguments |> List.map loop)
                | InoutArgument inner ->
                    InoutArgument(loop inner)
                | Unary(operatorName, operand) ->
                    Unary(operatorName, loop operand)
                | Binary(left, operatorName, right) ->
                    Binary(loop left, operatorName, loop right)
                | Elvis(left, right) ->
                    Elvis(loop left, loop right)
                | PrefixedString(prefix, parts) ->
                    PrefixedString(
                        prefix,
                        parts
                        |> List.map (function
                            | StringText text -> StringText text
                            | StringInterpolation inner -> StringInterpolation(loop inner))
                    )
                | Literal _ ->
                    current

            loop expression

        and tryProjectionCallExpression expression =
            match expression with
            | Apply(Name [ projectionName ], projectionArguments) ->
                environment.VisibleProjections
                |> Map.tryFind projectionName
                |> Option.map (fun projectionInfo -> projectionInfo, projectionArguments)
            | _ ->
                None

        and tryProjectionArgumentBindings (projectionInfo: ProjectionInfo) projectionArguments =
            if List.length projectionInfo.Binders <> List.length projectionArguments then
                None
            else
                (Some(Map.empty, Map.empty), List.zip projectionInfo.Binders projectionArguments)
                ||> List.fold (fun state (binder, argument) ->
                    state
                    |> Option.bind (fun (placeArguments, valueArguments) ->
                        match binder with
                        | ProjectionPlaceBinder placeBinder ->
                            tryExpressionPlace argument
                            |> Option.map (fun place ->
                                Map.add placeBinder.Name place placeArguments, valueArguments)
                        | ProjectionValueBinder valueBinder ->
                            Some(placeArguments, Map.add valueBinder.Name argument valueArguments)))

        and lowerPatternBinding
            (localTypes: Map<string, TypeExpr>)
            valueType
            (binding: SurfaceBindPattern)
            valueExpression
            continueWith
            =
            let nextLocals =
                extendPatternLocalTypes environment freshCounter localTypes valueType binding.Pattern

            let loweredBody = continueWith nextLocals

            match binding.Pattern with
            | NamePattern bindingName ->
                KCoreLet(bindingName, valueExpression, loweredBody)
            | _ ->
                let tempBindingName = freshSyntheticName "__kappa_bind_"

                KCoreLet(
                    tempBindingName,
                    valueExpression,
                    KCoreMatch(
                        KCoreName [ tempBindingName ],
                        [
                            { Pattern = lowerPattern nextLocals valueType binding.Pattern
                              Guard = None
                              Body = loweredBody }
                        ]
                    )
                )

        and tryLowerProjectionInoutDoSite
            scopeLabel
            (localTypes: Map<string, TypeExpr>)
            (binding: SurfaceBindPattern option)
            calleeName
            explicitParameters
            targetExplicitIndex
            (projectionInfo: ProjectionInfo)
            projectionArguments
            rest
            isMonadicSite
            =
            tryProjectionArgumentBindings projectionInfo projectionArguments
            |> Option.bind (fun (placeArguments, valueArguments) ->
                let substitute =
                    substituteProjectionExpression placeArguments valueArguments

                let rec lowerProjectionBody bodyLocals body =
                    match body with
                    | ProjectionYield yielded ->
                        let yieldedPlace = substitute yielded

                        match tryExpressionPlace yieldedPlace with
                        | Some _ ->
                            let rewrittenArguments =
                                explicitParameters
                                |> List.map (fun (explicitIndex, _, _, argument) ->
                                    if explicitIndex = targetExplicitIndex then
                                        InoutArgument yieldedPlace
                                    else
                                        argument)

                            tryLowerInoutDoSite
                                scopeLabel
                                bodyLocals
                                binding
                                (Apply(Name [ calleeName ], rewrittenArguments))
                                rest
                                isMonadicSite
                        | None ->
                            None
                    | ProjectionIfThenElse(condition, whenTrue, whenFalse) ->
                        match lowerProjectionBody bodyLocals whenTrue, lowerProjectionBody bodyLocals whenFalse with
                        | Some loweredTrue, Some loweredFalse ->
                            Some(KCoreIfThenElse(lowerExpression bodyLocals (substitute condition), loweredTrue, loweredFalse))
                        | _ ->
                            None
                    | ProjectionMatch(scrutinee, cases) ->
                        let loweredScrutinee = substitute scrutinee
                        let scrutineeType = inferExpressionType bodyLocals loweredScrutinee

                        let loweredCases : KCoreMatchCase option list =
                            cases
                            |> List.map (fun caseClause ->
                                let caseLocals =
                                    extendPatternLocalTypes environment freshCounter bodyLocals scrutineeType caseClause.Pattern

                                lowerProjectionBody caseLocals caseClause.Body
                                |> Option.map (fun loweredBody ->
                                    ({ Pattern = lowerPattern caseLocals scrutineeType caseClause.Pattern
                                       Guard = caseClause.Guard |> Option.map (substitute >> lowerExpression caseLocals)
                                       Body = loweredBody }
                                     : KCoreMatchCase)))

                        if loweredCases |> List.forall Option.isSome then
                            Some(
                                KCoreMatch(
                                    lowerExpression bodyLocals loweredScrutinee,
                                    loweredCases |> List.choose id
                                )
                            )
                        else
                            None

                projectionInfo.Body |> Option.bind (lowerProjectionBody localTypes))

        and tryLowerInoutDoSite
            scopeLabel
            (localTypes: Map<string, TypeExpr>)
            (binding: SurfaceBindPattern option)
            expression
            rest
            isMonadicSite
            =
            match expression with
            | Apply(Name [ calleeName ], arguments) ->
                environment.VisibleBindings
                |> Map.tryFind calleeName
                |> Option.bind (fun bindingInfo ->
                    tryPrepareVisibleBindingCall environment freshCounter bindingInfo (inferExpressionType localTypes) arguments
                    |> Option.bind (fun preparedCall ->
                        let explicitParameters =
                            preparedCall.Parameters
                            |> List.choose (fun parameter ->
                                match parameter.Layout, parameter.AssignedArgument with
                                | Some layout, Some(ExplicitArgument argument) ->
                                    Some(layout, parameter.ParameterType, argument)
                                | _ ->
                                    None)
                            |> List.mapi (fun explicitIndex (layout, parameterType, argument) ->
                                explicitIndex, layout, parameterType, argument)

                        let inoutParameters =
                            explicitParameters
                            |> List.choose (fun (explicitIndex, layout, parameterType, argument) ->
                                match layout.IsInout, argument with
                                | true, InoutArgument inner -> Some(explicitIndex, layout, parameterType, inner)
                                | _ -> None)

                        if List.isEmpty inoutParameters then
                            None
                        else
                            let siteResultType =
                                if isMonadicSite then
                                    unwrapIoType preparedCall.ResultType
                                else
                                    preparedCall.ResultType

                            match tryCanonicalRecordType environment.VisibleTypeAliases siteResultType with
                            | Some(TypeRecord resultFields as resultRecordType) ->
                                let inoutNames =
                                    inoutParameters |> List.map (fun (_, layout, _, _) -> layout.Name) |> Set.ofList

                                let residualFields =
                                    resultFields
                                    |> List.filter (fun field -> not (Set.contains field.Name inoutNames))

                                let projectionInoutParameter =
                                    inoutParameters
                                    |> List.tryPick (fun (explicitIndex, _, _, inner) ->
                                        tryProjectionCallExpression inner
                                        |> Option.map (fun (projectionInfo, projectionArguments) ->
                                            explicitIndex, projectionInfo, projectionArguments))

                                match projectionInoutParameter with
                                | Some(targetExplicitIndex, projectionInfo, projectionArguments) ->
                                    tryLowerProjectionInoutDoSite
                                        scopeLabel
                                        localTypes
                                        binding
                                        calleeName
                                        explicitParameters
                                        targetExplicitIndex
                                        projectionInfo
                                        projectionArguments
                                        rest
                                        isMonadicSite
                                | None ->
                                    let layout =
                                        ensureSyntheticRecordLayout environment.CurrentModuleName "<binding>" recordLayouts resultRecordType

                                    let mutable rewrittenArguments = ResizeArray<SurfaceExpression>()
                                    let mutable callLocalTypes = localTypes
                                    let preludeBindings = ResizeArray<string * TypeExpr * KCoreExpression>()
                                    let restorePlans = ResizeArray<string * string * string list * bool>()

                                    for _, parameterLayout, _, argument in explicitParameters do
                                        match parameterLayout.IsInout, argument with
                                        | true, InoutArgument inner ->
                                            match tryExpressionPlace inner with
                                            | Some(rootName, path) ->
                                                match localTypes |> Map.tryFind rootName |> Option.bind unwrapRefLikeType with
                                                | Some refInnerType ->
                                                    let hiddenRootName = freshSyntheticName $"__kappa_inout_{rootName}_"
                                                    preludeBindings.Add(
                                                        hiddenRootName,
                                                        refInnerType,
                                                        KCoreExecute(KCoreApply(KCoreName [ "readRef" ], [ KCoreName [ rootName ] ]))
                                                    )

                                                    callLocalTypes <- Map.add hiddenRootName refInnerType callLocalTypes
                                                    restorePlans.Add(parameterLayout.Name, rootName, path, true)
                                                    rewrittenArguments.Add(InoutArgument(Name(hiddenRootName :: path)))
                                                | None ->
                                                    restorePlans.Add(parameterLayout.Name, rootName, path, false)
                                                    rewrittenArguments.Add(argument)
                                            | None ->
                                                rewrittenArguments.Add(argument)
                                        | _ ->
                                            rewrittenArguments.Add(argument)

                                    let rewrittenCallExpression =
                                        Apply(Name [ calleeName ], List.ofSeq rewrittenArguments)

                                    let resultBindingName = freshSyntheticName "__kappa_inout_result_"
                                    let fieldBindingNames =
                                        layout.Fields
                                        |> List.map (fun field -> field.Name, freshSyntheticName $"__kappa_inout_field_{field.Name}_")
                                        |> Map.ofList

                                    let pattern =
                                        KCoreConstructorPattern(
                                            [ layout.ConstructorName ],
                                            layout.Fields
                                            |> List.map (fun field -> KCoreNamePattern fieldBindingNames[field.Name])
                                        )

                                    let matchLocalTypes =
                                        let withPrelude =
                                            preludeBindings
                                            |> Seq.fold (fun state (name, fieldType, _) -> Map.add name fieldType state) callLocalTypes

                                        layout.Fields
                                        |> List.fold (fun state field ->
                                            Map.add fieldBindingNames[field.Name] field.Type state) withPrelude

                                    let restLocalTypes =
                                        restorePlans
                                        |> Seq.fold (fun state (formalName, rootName, path, isVarRoot) ->
                                            match resultFields |> List.tryFind (fun field -> String.Equals(field.Name, formalName, StringComparison.Ordinal)) with
                                            | Some field when List.isEmpty path ->
                                                if isVarRoot then
                                                    Map.add rootName (refType field.Type) state
                                                else
                                                    Map.add rootName field.Type state
                                            | _ ->
                                                state) localTypes

                                    let residualResultType =
                                        match residualFields with
                                        | [] -> unitType
                                        | _ -> TypeRecord residualFields

                                    let loweredContinuation =
                                        match binding with
                                        | None ->
                                            if List.isEmpty residualFields then
                                                Some(lowerDoStatements scopeLabel restLocalTypes rest)
                                            else
                                                None
                                        | Some residualBinding ->
                                            let residualValueExpression =
                                                match residualFields with
                                                | [] ->
                                                    KCoreLiteral LiteralValue.Unit
                                                | _ ->
                                                    let residualLayout =
                                                        ensureSyntheticRecordLayout
                                                            environment.CurrentModuleName
                                                            "<binding>"
                                                            recordLayouts
                                                            residualResultType

                                                    KCoreApply(
                                                        KCoreName [ residualLayout.ConstructorName ],
                                                        residualLayout.Fields
                                                        |> List.map (fun field -> KCoreName [ fieldBindingNames[field.Name] ])
                                                    )

                                            Some(
                                                lowerPatternBinding
                                                    restLocalTypes
                                                    (Some residualResultType)
                                                    residualBinding
                                                    residualValueExpression
                                                    (fun nextLocals -> lowerDoStatements scopeLabel nextLocals rest)
                                            )

                                    loweredContinuation
                                    |> Option.map (fun continuationBody ->
                                        let restoredBody =
                                            restorePlans
                                            |> Seq.toList
                                            |> List.rev
                                            |> List.fold (fun current (formalName, rootName, path, isVarRoot) ->
                                                let replacement = Name [ fieldBindingNames[formalName] ]

                                                let rebuiltValue =
                                                    if isVarRoot then
                                                        let hiddenRootName, hiddenRootType =
                                                            preludeBindings
                                                            |> Seq.find (fun (name, _, _) ->
                                                                name.StartsWith($"__kappa_inout_{rootName}_", StringComparison.Ordinal))
                                                            |> fun (name, fieldType, _) -> name, fieldType

                                                        if List.isEmpty path then
                                                            KCoreName [ fieldBindingNames[formalName] ]
                                                        else
                                                            lowerExpression
                                                                (Map.add hiddenRootName hiddenRootType matchLocalTypes)
                                                                (buildPlaceWritebackExpression hiddenRootName path replacement)
                                                    elif List.isEmpty path then
                                                        KCoreName [ fieldBindingNames[formalName] ]
                                                    else
                                                        lowerExpression
                                                            matchLocalTypes
                                                            (buildPlaceWritebackExpression rootName path replacement)

                                                if isVarRoot then
                                                    KCoreSequence(
                                                        KCoreExecute(KCoreApply(KCoreName [ "writeRef" ], [ KCoreName [ rootName ]; rebuiltValue ])),
                                                        current
                                                    )
                                                else
                                                    KCoreLet(rootName, rebuiltValue, current))
                                                continuationBody

                                        let matchBody =
                                            KCoreMatch(
                                                KCoreName [ resultBindingName ],
                                                [
                                                    { Pattern = pattern
                                                      Guard = None
                                                      Body = restoredBody }
                                                ]
                                            )

                                        let callValue =
                                            if isMonadicSite then
                                                KCoreExecute(lowerExpression callLocalTypes rewrittenCallExpression)
                                            else
                                                lowerExpression callLocalTypes rewrittenCallExpression

                                        let callBody =
                                            KCoreLet(
                                                resultBindingName,
                                                callValue,
                                                matchBody
                                            )

                                        preludeBindings
                                        |> Seq.toList
                                        |> List.rev
                                        |> List.fold (fun current (bindingName, _, valueExpression) ->
                                            KCoreLet(bindingName, valueExpression, current)) callBody)
                            | _ ->
                                None))
            | _ ->
                None

        and lowerDoStatements scopeLabel localTypes statements =
            let bindPatternName (binding: SurfaceBindPattern) =
                match binding.Pattern with
                | NamePattern name -> name
                | _ -> "__pattern"

            match statements with
            | [] ->
                KCoreLiteral LiteralValue.Unit
            | [ DoExpression expression ] ->
                match tryLowerInoutDoSite scopeLabel localTypes None expression [] true with
                | Some lowered ->
                    lowered
                | None ->
                    KCoreExecute(lowerExpression localTypes expression)
            | DoExpression expression :: rest ->
                match tryLowerInoutDoSite scopeLabel localTypes None expression rest true with
                | Some lowered ->
                    lowered
                | None ->
                    KCoreSequence(KCoreExecute(lowerExpression localTypes expression), lowerDoStatements scopeLabel localTypes rest)
            | DoLet(binding, expression) :: rest ->
                match tryLowerInoutDoSite scopeLabel localTypes (Some binding) expression rest false with
                | Some lowered ->
                    lowered
                | None ->
                    let loweredValue = lowerExpression localTypes expression
                    let valueType = inferExpressionType localTypes expression

                    lowerPatternBinding
                        localTypes
                        valueType
                        binding
                        loweredValue
                        (fun nextLocals -> lowerDoStatements scopeLabel nextLocals rest)
            | DoLetQuestion(binding, expression, failure) :: rest ->
                let valueType = inferExpressionType localTypes expression
                let successLocals =
                    extendPatternLocalTypes environment freshCounter localTypes valueType binding.Pattern

                let successCase: KCoreMatchCase =
                    { Pattern = lowerPattern localTypes valueType binding.Pattern
                      Guard = None
                      Body = lowerDoStatements scopeLabel successLocals rest }

                let failureCase: KCoreMatchCase =
                    match failure with
                    | Some failure ->
                        let failureLocals =
                            extendPatternLocalTypes environment freshCounter localTypes valueType failure.ResiduePattern.Pattern

                        { Pattern = lowerPattern localTypes valueType failure.ResiduePattern.Pattern
                          Guard = None
                          Body = lowerDoStatements scopeLabel failureLocals failure.Body }
                    | None ->
                        { Pattern = KCoreWildcardPattern
                          Guard = None
                          Body = KCoreExecute(KCoreName [ "empty" ]) }

                KCoreMatch(lowerExpression localTypes expression, [ successCase; failureCase ])
            | DoBind(binding, expression) :: rest ->
                match tryLowerInoutDoSite scopeLabel localTypes (Some binding) expression rest true with
                | Some lowered ->
                    lowered
                | None ->
                    let loweredValue = KCoreExecute(lowerExpression localTypes expression)
                    let valueType = inferExpressionType localTypes expression |> Option.map unwrapIoType

                    lowerPatternBinding
                        localTypes
                        valueType
                        binding
                        loweredValue
                        (fun nextLocals -> lowerDoStatements scopeLabel nextLocals rest)
            | DoUsing(binding, expression) :: rest ->
                let bindingName =
                    match binding.Pattern with
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
                        buildUsingReleaseAction localTypes expression hiddenOwnedName,
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
            | DoIf(condition, whenTrue, whenFalse) :: rest ->
                KCoreSequence(
                    KCoreIfThenElse(
                        lowerExpression localTypes condition,
                        lowerDoStatements scopeLabel localTypes whenTrue,
                        lowerDoStatements scopeLabel localTypes whenFalse
                    ),
                    lowerDoStatements scopeLabel localTypes rest
                )
            | DoWhile(condition, body) :: rest ->
                KCoreSequence(
                    KCoreWhile(lowerExpression localTypes condition, lowerDoStatements scopeLabel localTypes body),
                    lowerDoStatements scopeLabel localTypes rest
                )
            | DoReturn expression :: _ ->
                KCoreExecute(KCoreApply(KCoreName [ "return" ], [ lowerExpression localTypes expression ]))

        and lowerPattern localTypes expectedType pattern =
            match pattern with
            | WildcardPattern ->
                KCoreWildcardPattern
            | NamePattern name ->
                KCoreNamePattern name
            | LiteralPattern literal ->
                KCoreLiteralPattern literal
            | ConstructorPattern(name, arguments) ->
                KCoreConstructorPattern(name, arguments |> List.map (lowerPattern localTypes None))
            | OrPattern alternatives ->
                KCoreOrPattern(alternatives |> List.map (lowerPattern localTypes expectedType))
            | AnonymousRecordPattern fields ->
                match expectedType |> Option.bind (tryCanonicalRecordType environment.VisibleTypeAliases) with
                | Some recordType ->
                    let layout =
                        ensureSyntheticRecordLayout environment.CurrentModuleName "<binding>" recordLayouts recordType

                    let fieldPatterns =
                        fields
                        |> List.map (fun field -> field.Name, field.Pattern)
                        |> Map.ofList

                    KCoreConstructorPattern(
                        [ layout.ConstructorName ],
                        layout.Fields
                        |> List.map (fun field ->
                            fieldPatterns
                            |> Map.tryFind field.Name
                            |> Option.map (lowerPattern localTypes (Some field.Type))
                            |> Option.defaultValue KCoreWildcardPattern)
                    )
                | None ->
                    KCoreWildcardPattern

        and lowerExpression localTypes expression =
            let lowerArguments arguments =
                arguments |> List.map (lowerExpression localTypes)

            let lowerPreparedArguments preparedArguments =
                preparedArguments
                |> List.map (function
                    | ExplicitArgument argument -> lowerExpression localTypes argument
                    | ImplicitArgument implicitArgument -> implicitArgument)

            match expression with
            | Literal literal ->
                KCoreLiteral literal
            | Name [ bindingName ]
                when not (Map.containsKey bindingName localTypes)
                     && environment.VisibleBindings.ContainsKey(bindingName) ->
                let bindingInfo = environment.VisibleBindings[bindingName]
                match
                    tryPrepareVisibleBindingCall environment freshCounter bindingInfo (inferExpressionType localTypes) []
                with
                | Some preparedCall when not (List.isEmpty preparedCall.Arguments) ->
                    KCoreApply(KCoreName [ bindingName ], lowerPreparedArguments preparedCall.Arguments)
                | _ ->
                    KCoreName [ bindingName ]
            | Name segments ->
                KCoreName segments
            | LocalLet(binding, value, body) ->
                let bindingNames = collectPatternNames binding.Pattern

                let nextLocals =
                    match inferExpressionType localTypes value with
                    | Some valueType ->
                        bindingNames
                        |> List.fold (fun state name -> Map.add name valueType state) localTypes
                    | None -> localTypes

                let loweredValue = lowerExpression localTypes value
                let loweredBody = lowerExpression nextLocals body

                match bindingNames with
                | [] ->
                    KCoreLet("__pattern", loweredValue, loweredBody)
                | [ bindingName ] ->
                    KCoreLet(bindingName, loweredValue, loweredBody)
                | bindingName :: rest ->
                    let loweredBindings =
                        List.foldBack
                            (fun name current -> KCoreLet(name, KCoreName [ bindingName ], current))
                            rest
                            loweredBody

                    KCoreLet(
                        "__pattern",
                        loweredValue,
                        KCoreLet(bindingName, KCoreName [ "__pattern" ], loweredBindings)
                    )
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
                let scrutineeType = inferExpressionType localTypes scrutinee

                let loweredCases : KCoreMatchCase list =
                    cases
                    |> List.collect (fun caseClause ->
                        let alternatives =
                            match caseClause.Pattern with
                            | OrPattern options -> options
                            | other -> [ other ]

                        alternatives
                        |> List.map (fun pattern ->
                            let caseLocals =
                                extendPatternLocalTypes environment freshCounter localTypes scrutineeType pattern

                            ({ Pattern = lowerPattern caseLocals scrutineeType pattern
                               Guard = caseClause.Guard |> Option.map (lowerExpression caseLocals)
                               Body = lowerExpression caseLocals caseClause.Body }
                             : KCoreMatchCase)))

                KCoreMatch(
                    lowerExpression localTypes scrutinee,
                    loweredCases
                )
            | RecordLiteral fields ->
                let inferredFields =
                    fields
                    |> List.map (fun field ->
                        inferExpressionType localTypes field.Value
                        |> Option.map (fun fieldType ->
                            { Name = field.Name
                              Quantity = if field.IsImplicit then QuantityZero else QuantityOmega
                              Type = fieldType }))

                if inferredFields |> List.forall Option.isSome then
                    let recordType = TypeRecord(inferredFields |> List.choose id)
                    let layout =
                        ensureSyntheticRecordLayout environment.CurrentModuleName "<binding>" recordLayouts recordType

                    let fieldValues =
                        fields
                        |> List.map (fun field -> field.Name, field.Value)
                        |> Map.ofList

                    KCoreApply(
                        KCoreName [ layout.ConstructorName ],
                        layout.Fields
                        |> List.map (fun field ->
                            fieldValues
                            |> Map.tryFind field.Name
                            |> Option.map (lowerExpression localTypes)
                            |> Option.defaultValue (KCoreLiteral LiteralValue.Unit))
                    )
                else
                    KCoreLiteral LiteralValue.Unit
            | Seal(value, _) ->
                lowerExpression localTypes value
            | RecordUpdate(receiver, fields) ->
                let loweredReceiver = lowerExpression localTypes receiver

                match inferExpressionType localTypes receiver |> Option.bind (tryCanonicalRecordType environment.VisibleTypeAliases) with
                | Some recordType ->
                    let layout =
                        ensureSyntheticRecordLayout environment.CurrentModuleName "<binding>" recordLayouts recordType

                    let boundFields =
                        layout.Fields
                        |> List.map (fun field -> field.Name, freshSyntheticName $"__kappa_record_{field.Name}_")

                    let updatedFields =
                        fields
                        |> List.map (fun field -> field.Name, field.Value)
                        |> Map.ofList

                    let pattern =
                        KCoreConstructorPattern(
                            [ layout.ConstructorName ],
                            boundFields
                            |> List.map (fun (_, bindingName) -> KCoreNamePattern bindingName)
                        )

                    let rebuiltFields =
                        layout.Fields
                        |> List.map (fun field ->
                            updatedFields
                            |> Map.tryFind field.Name
                            |> Option.map (lowerExpression localTypes)
                            |> Option.defaultValue (
                                boundFields
                                |> List.find (fun (name, _) -> String.Equals(name, field.Name, StringComparison.Ordinal))
                                |> snd
                                |> fun bindingName -> KCoreName [ bindingName ]))

                    KCoreMatch(
                        loweredReceiver,
                        [
                            { Pattern = pattern
                              Guard = None
                              Body = KCoreApply(KCoreName [ layout.ConstructorName ], rebuiltFields) }
                        ]
                    )
                | None ->
                    loweredReceiver
            | SafeNavigation(receiver, navigation) ->
                let loweredReceiver = lowerExpression localTypes receiver
                let binderName = freshSyntheticName "__kappa_safe_"

                let loweredMember =
                    let callee = KCoreName(binderName :: navigation.Segments)

                    match navigation.Arguments with
                    | [] ->
                        callee
                    | arguments ->
                        KCoreApply(callee, arguments |> List.map (lowerExpression localTypes))

                let wrapsResult =
                    inferExpressionType localTypes receiver
                    |> Option.bind (fun receiverType ->
                        tryOptionPayloadType environment.VisibleTypeAliases receiverType
                        |> Option.bind (fun payloadType ->
                            tryInferSafeNavigationMemberType
                                environment.VisibleTypeAliases
                                (inferExpressionType localTypes)
                                payloadType
                                navigation))
                    |> Option.bind (tryOptionPayloadType environment.VisibleTypeAliases)
                    |> Option.isNone

                let successBody =
                    if wrapsResult then
                        KCoreApply(KCoreName [ "Some" ], [ loweredMember ])
                    else
                        loweredMember

                KCoreMatch(
                    loweredReceiver,
                    [
                        { Pattern = KCoreConstructorPattern([ "Some" ], [ KCoreNamePattern binderName ])
                          Guard = None
                          Body = successBody }
                        { Pattern = KCoreConstructorPattern([ "None" ], [])
                          Guard = None
                          Body = KCoreName [ "None" ] }
                    ]
                )
            | TagTest(receiver, constructorName) ->
                KCoreBinary(lowerExpression localTypes receiver, "is", KCoreName constructorName)
            | Do statements ->
                let scopeLabel = freshDoScopeLabel ()
                KCoreDoScope(scopeLabel, lowerDoStatements scopeLabel localTypes statements)
            | MonadicSplice inner ->
                KCoreExecute(lowerExpression localTypes inner)
            | InoutArgument inner ->
                lowerExpression localTypes inner
            | Apply(Name nameSegments, arguments) ->
                match tryResolveTraitMemberProjection environment localTypes nameSegments with
                | Some(traitInfo, memberName, _, receiverName) ->
                    KCoreTraitCall(
                        traitInfo.Name,
                        memberName,
                        lowerExpression localTypes (Name [ receiverName ]),
                        lowerArguments arguments
                    )
                | None ->
                    match nameSegments with
                    | [ calleeName ]
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
                    | [ calleeName ] when environment.VisibleBindings.ContainsKey calleeName ->
                        let bindingInfo = environment.VisibleBindings[calleeName]
                        match
                            tryPrepareVisibleBindingCall
                                environment
                                freshCounter
                                bindingInfo
                                (inferExpressionType localTypes)
                                arguments
                        with
                        | Some preparedCall ->
                            let dictionaryArguments =
                                preparedCall.InstantiatedScheme.Constraints
                                |> List.choose (fun constraintInfo ->
                                    resolveConstraintInstance environment constraintInfo
                                    |> Option.map (fun instanceInfo ->
                                        KCoreDictionaryValue(instanceInfo.ModuleName, instanceInfo.TraitName, instanceInfo.InstanceKey)))

                            KCoreApply(
                                KCoreName [ calleeName ],
                                dictionaryArguments @ lowerPreparedArguments preparedCall.Arguments
                            )
                        | None ->
                            KCoreApply(KCoreName [ calleeName ], lowerArguments arguments)
                    | [ calleeName ] ->
                        match
                            tryPrepareVisibleTraitMemberCall
                                environment
                                freshCounter
                                calleeName
                                (inferExpressionType localTypes)
                                arguments
                        with
                        | Some(traitInfo, _, preparedCall, instanceInfo) ->
                            KCoreTraitCall(
                                traitInfo.Name,
                                calleeName,
                                KCoreDictionaryValue(instanceInfo.ModuleName, instanceInfo.TraitName, instanceInfo.InstanceKey),
                                lowerPreparedArguments preparedCall.Arguments
                            )
                        | None ->
                            KCoreApply(KCoreName [ calleeName ], lowerArguments arguments)
                    | _ ->
                        KCoreApply(lowerExpression localTypes (Name nameSegments), lowerArguments arguments)
            | Apply(callee, arguments) ->
                KCoreApply(lowerExpression localTypes callee, lowerArguments arguments)
            | Unary(operatorName, operand) ->
                KCoreUnary(operatorName, lowerExpression localTypes operand)
            | Binary(left, operatorName, right) ->
                KCoreBinary(lowerExpression localTypes left, operatorName, lowerExpression localTypes right)
            | Elvis(left, right) ->
                let binderName = freshSyntheticName "__kappa_elvis_"

                KCoreMatch(
                    lowerExpression localTypes left,
                    [
                        { Pattern = KCoreConstructorPattern([ "Some" ], [ KCoreNamePattern binderName ])
                          Guard = None
                          Body = KCoreName [ binderName ] }
                        { Pattern = KCoreConstructorPattern([ "None" ], [])
                          Guard = None
                          Body = lowerExpression localTypes right }
                    ]
                )
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
        (recordLayouts: Map<string, SyntheticRecordLayout> ref)
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
                    (Some(TypeSignatures.toText (dictionaryType constraintInfo.TraitName constraintInfo.Arguments))))

        let loweredBody =
            definition.Body
            |> Option.map (lowerBindingBody bodyEnvironment recordLayouts scheme definition.Parameters)

        let parameters =
            hiddenParameters @ buildBindingParameters environment.VisibleTypeAliases scheme definition.Parameters

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
            let dictionaryArgumentTypes =
                traitInfo.TypeParameters |> List.map TypeVariable

            let valueParameterNames =
                parameterTypes
                |> List.mapi (fun index _ -> $"arg{index}")

            let parameters =
                lowerKCoreParameter
                    dictionaryParameterName
                    (Some(TypeSignatures.toText (dictionaryType traitInfo.Name dictionaryArgumentTypes)))
                :: (List.zip valueParameterNames parameterTypes
                    |> List.map (fun (parameterName, parameterType) ->
                        lowerKCoreParameter parameterName (Some(TypeSignatures.toText parameterType))))

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
                (lowerKCoreParameter
                    dictionaryParameterName
                    (Some(TypeSignatures.toText (dictionaryType traitInfo.Name dictionaryArgumentTypes)))
                 :: (List.zip valueParameterNames parameterTypes
                     |> List.map (fun (parameterName, parameterType) ->
                         lowerKCoreParameter parameterName (Some(TypeSignatures.toText parameterType)))))
                (Some(TypeSignatures.toText resultType))
                body
                provenance)

    let private synthesizeInstanceBindings
        (surfaceIndex: Map<string, ModuleSurfaceInfo>)
        (recordLayouts: Map<string, SyntheticRecordLayout> ref)
        (filePath: string)
        (moduleName: string)
        (instanceInfo: InstanceInfo)
        =
        let selfDictionaryName = "__kappa_self_dict"

        let rec substituteSelfDictionary expression =
            match expression with
            | KCoreLiteral _ ->
                expression
            | KCoreName [ name ] when String.Equals(name, selfDictionaryName, StringComparison.Ordinal) ->
                KCoreDictionaryValue(moduleName, instanceInfo.TraitName, instanceInfo.InstanceKey)
            | KCoreName _ ->
                expression
            | KCoreLambda(parameters, body) ->
                KCoreLambda(parameters, substituteSelfDictionary body)
            | KCoreIfThenElse(condition, whenTrue, whenFalse) ->
                KCoreIfThenElse(
                    substituteSelfDictionary condition,
                    substituteSelfDictionary whenTrue,
                    substituteSelfDictionary whenFalse
                )
            | KCoreMatch(scrutinee, cases) ->
                KCoreMatch(
                    substituteSelfDictionary scrutinee,
                    cases
                    |> List.map (fun caseClause ->
                        { caseClause with
                            Guard = caseClause.Guard |> Option.map substituteSelfDictionary
                            Body = substituteSelfDictionary caseClause.Body })
                )
            | KCoreExecute inner ->
                KCoreExecute(substituteSelfDictionary inner)
            | KCoreLet(bindingName, value, body) ->
                KCoreLet(bindingName, substituteSelfDictionary value, substituteSelfDictionary body)
            | KCoreDoScope(scopeLabel, body) ->
                KCoreDoScope(scopeLabel, substituteSelfDictionary body)
            | KCoreScheduleExit(scopeLabel, action, body) ->
                let rewrittenAction =
                    match action with
                    | KCoreDeferred deferred ->
                        KCoreDeferred(substituteSelfDictionary deferred)
                    | KCoreRelease(resourceTypeText, release, resource) ->
                        KCoreRelease(resourceTypeText, substituteSelfDictionary release, substituteSelfDictionary resource)

                KCoreScheduleExit(scopeLabel, rewrittenAction, substituteSelfDictionary body)
            | KCoreSequence(first, second) ->
                KCoreSequence(substituteSelfDictionary first, substituteSelfDictionary second)
            | KCoreWhile(condition, body) ->
                KCoreWhile(substituteSelfDictionary condition, substituteSelfDictionary body)
            | KCoreApply(callee, arguments) ->
                KCoreApply(substituteSelfDictionary callee, arguments |> List.map substituteSelfDictionary)
            | KCoreDictionaryValue _ ->
                expression
            | KCoreTraitCall(traitName, memberName, dictionary, arguments) ->
                KCoreTraitCall(
                    traitName,
                    memberName,
                    substituteSelfDictionary dictionary,
                    arguments |> List.map substituteSelfDictionary
                )
            | KCoreUnary(operatorName, operand) ->
                KCoreUnary(operatorName, substituteSelfDictionary operand)
            | KCoreBinary(left, operatorName, right) ->
                KCoreBinary(substituteSelfDictionary left, operatorName, substituteSelfDictionary right)
            | KCorePrefixedString(prefix, parts) ->
                KCorePrefixedString(
                    prefix,
                    parts
                    |> List.map (function
                        | KCoreStringText _ as part -> part
                        | KCoreStringInterpolation inner -> KCoreStringInterpolation(substituteSelfDictionary inner))
                )

        let traitInfo =
            surfaceIndex[moduleName].Traits[instanceInfo.TraitName]

        let substitution =
            if List.length traitInfo.TypeParameters = List.length instanceInfo.HeadTypes then
                List.zip traitInfo.TypeParameters instanceInfo.HeadTypes |> Map.ofList
            else
                Map.empty

        let visibleBindings = mergeVisibleBindings surfaceIndex moduleName
        let visibleTraits = mergeVisibleTraits surfaceIndex moduleName
        let visibleTypeAliases = mergeVisibleTypeAliases surfaceIndex moduleName
        let visibleProjections = mergeVisibleProjections surfaceIndex moduleName

        let environment =
            { CurrentModuleName = moduleName
              VisibleTypeAliases = visibleTypeAliases
              VisibleBindings = visibleBindings
              VisibleConstructors = mergeVisibleConstructors surfaceIndex moduleName
              VisibleProjections = visibleProjections
              VisibleTraits = visibleTraits
              VisibleInstances = visibleInstances surfaceIndex moduleName
              ConstrainedMembers = Map.empty }

        let defaultBodyEnvironment =
            { environment with
                ConstrainedMembers =
                    traitInfo.Members
                    |> Map.toList
                    |> List.map (fun (memberName, _) -> memberName, (instanceInfo.TraitName, selfDictionaryName))
                    |> Map.ofList }

        let memberBindings =
            traitInfo.Members
            |> Map.toList
            |> List.choose (fun (memberName, memberInfo) ->
                let memberDefinition =
                    instanceInfo.Members
                    |> Map.tryFind memberName
                    |> Option.map (fun definition -> definition, false)
                    |> Option.orElseWith (fun () ->
                        memberInfo.DefaultDefinition
                        |> Option.map (fun definition -> definition, true))

                memberDefinition
                |> Option.map (fun (definition, isDefaultDefinition) ->
                    let specializedScheme =
                        memberInfo.Scheme |> TypeSignatures.applySchemeSubstitution substitution

                    let loweredBody =
                        definition.Body
                        |> Option.map (
                            lowerBindingBody
                                (if isDefaultDefinition then defaultBodyEnvironment else environment)
                                recordLayouts
                                (Some specializedScheme)
                                definition.Parameters
                        )
                        |> Option.map (fun body ->
                            if isDefaultDefinition then
                                substituteSelfDictionary body
                            else
                                body)

                    let parameters =
                        buildBindingParameters environment.VisibleTypeAliases (Some specializedScheme) definition.Parameters

                    let provenance =
                        syntheticOrigin
                            filePath
                            moduleName
                            (TraitRuntime.instanceMemberBindingName instanceInfo.TraitName instanceInfo.InstanceKey memberName)
                            (if isDefaultDefinition then "instance-default-member" else "instance-member")

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
                (Some(TypeSignatures.toText (dictionaryType instanceInfo.TraitName instanceInfo.HeadTypes)))
                (KCoreDictionaryValue(moduleName, instanceInfo.TraitName, instanceInfo.InstanceKey))
                provenance

        memberBindings @ [ dictionaryBinding ]

    let lowerKCoreModules (backendProfile: string) (frontendModules: KFrontIRModule list) =
        let surfaceIndex = buildSurfaceIndex frontendModules

        frontendModules
        |> List.map (fun frontendModule ->
            let moduleName = moduleNameText frontendModule.ModuleIdentity
            let moduleInfo = surfaceIndex[moduleName]
            let recordLayouts = ref Map.empty

            let environment =
                { CurrentModuleName = moduleName
                  VisibleTypeAliases = mergeVisibleTypeAliases surfaceIndex moduleName
                  VisibleBindings = mergeVisibleBindings surfaceIndex moduleName
                  VisibleConstructors = mergeVisibleConstructors surfaceIndex moduleName
                  VisibleProjections = mergeVisibleProjections surfaceIndex moduleName
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

                            [ lowerUserBinding environment recordLayouts frontendModule.FilePath moduleName definition scheme ]
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

                        originalDeclaration
                        @ synthesizeInstanceBindings surfaceIndex recordLayouts frontendModule.FilePath moduleName instanceInfo
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
              SyntheticDataTypes =
                recordLayouts.Value
                |> Map.toList
                |> List.map snd
                |> List.sortBy (fun layout -> layout.TypeName)
                |> List.map (fun layout ->
                    { Name = layout.TypeName
                      TypeParameters = layout.TypeParameters
                      ConstructorName = layout.ConstructorName
                      FieldNames = layout.Fields |> List.map (fun field -> field.Name)
                      FieldTypeTexts = layout.Fields |> List.map (fun field -> TypeSignatures.toText field.Type)
                      Provenance = layout.Provenance })
              Ownership = frontendModule.Ownership
              Declarations = declarations })
