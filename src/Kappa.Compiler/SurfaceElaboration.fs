namespace Kappa.Compiler

open System

// Elaborates resolved surface modules into KCore while preserving observability metadata.
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

    type private ModuleSurfaceInfo =
        { TypeAliases: Map<string, TypeAliasInfo>
          BindingSchemes: Map<string, BindingSchemeInfo>
          Traits: Map<string, TraitInfo>
          Instances: InstanceInfo list
          Imports: ImportSpec list
          ExportedTerms: Set<string>
          ExportedTypes: Set<string>
          ExportedTraits: Set<string> }

    type private BindingLoweringEnvironment =
        { CurrentModuleName: string
          VisibleTypeAliases: Map<string, TypeAliasInfo>
          VisibleBindings: Map<string, BindingSchemeInfo>
          VisibleTraits: Map<string, TraitInfo>
          VisibleInstances: InstanceInfo list
          ConstrainedMembers: Map<string, string * string> }

    type private PreparedCallArgument =
        | ExplicitArgument of SurfaceExpression
        | ImplicitArgument of KCoreExpression

    type private PreparedBindingCall =
        { InstantiatedScheme: TypeScheme
          ResultType: TypeExpr
          Arguments: PreparedCallArgument list }

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
                  Scheme = scheme })
        | _ ->
            None

    let private splitLeadingTypeArgumentTokens argumentCount tokens =
        let tokens = significantTokens tokens

        let takeAtom remaining =
            match remaining with
            | [] ->
                [], []
            | first :: rest when first.Kind = LeftParen ->
                let mutable depth = 0
                let mutable index = 0
                let mutable keepReading = true
                let tokenArray = remaining |> List.toArray

                while keepReading && index < tokenArray.Length do
                    match tokenArray[index].Kind with
                    | LeftParen -> depth <- depth + 1
                    | RightParen ->
                        depth <- depth - 1

                        if depth = 0 then
                            keepReading <- false
                    | _ -> ()

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

    let private tryParseInstanceHeadTypes argumentCount (declaration: InstanceDeclaration) =
        let parsed =
            declaration.HeaderTokens
            |> splitLeadingTypeArgumentTokens argumentCount
            |> List.map TypeSignatures.parseType

        if parsed |> List.exists Option.isNone then
            None
        else
            Some(parsed |> List.choose id)

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
                    TypeSignatures.parseScheme declaration.TypeTokens
                    |> Option.map (fun scheme ->
                        declaration.Name,
                        { ModuleName = moduleName
                          Name = declaration.Name
                          Scheme = scheme
                          ParameterLayouts =
                            bindingDefinitions
                            |> Map.tryFind declaration.Name
                            |> Option.map (fun definition -> definition.Parameters) })
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
                    let argumentCount =
                        traits
                        |> Map.tryFind declaration.TraitName
                        |> Option.map (fun traitInfo -> traitInfo.TypeParameters.Length)
                        |> Option.defaultValue 1

                    tryParseInstanceHeadTypes argumentCount declaration
                    |> Option.map (fun headTypes ->
                        { ModuleName = moduleName
                          TraitName = declaration.TraitName
                          InstanceKey = TraitRuntime.instanceKeyFromTokens declaration.HeaderTokens
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
          Traits = traits
          Instances = instances
          Imports = frontendModule.Imports
          ExportedTerms = exportedTerms
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
            |> List.distinctBy (fun (_, importedModule) -> importedModule.BindingSchemes.Keys |> Seq.toList, importedModule.Traits.Keys |> Seq.toList, importedModule.TypeAliases.Keys |> Seq.toList))
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
        environment.VisibleInstances
        |> List.tryPick (fun instanceInfo ->
            if not (String.Equals(instanceInfo.TraitName, constraintInfo.TraitName, StringComparison.Ordinal)) then
                None
            elif List.length instanceInfo.HeadTypes <> List.length constraintInfo.Arguments then
                None
            else
                tryUnifyVisibleTypes environment.VisibleTypeAliases (List.zip instanceInfo.HeadTypes constraintInfo.Arguments)
                |> Option.map (fun _ -> instanceInfo))

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
                      Arguments = arguments |> List.map ExplicitArgument })
        | Some parameterLayouts ->
            let parameterTypes, resultType = TypeSignatures.schemeParts instantiated

            if List.length parameterLayouts <> List.length parameterTypes then
                None
            else
                let mutable remainingArguments = arguments
                let mutable substitution = Map.empty<string, TypeExpr>
                let preparedArguments = ResizeArray<PreparedCallArgument>()
                let mutable success = true

                for parameterLayout, rawParameterType in List.zip parameterLayouts parameterTypes do
                    let parameterType = TypeSignatures.applySubstitution substitution rawParameterType

                    if parameterLayout.IsImplicit then
                        match trySynthesizeImplicitArgument environment parameterType with
                        | Some implicitArgument ->
                            preparedArguments.Add(ImplicitArgument implicitArgument)
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
                          Arguments = List.ofSeq preparedArguments }
                else
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
        | Match(_, cases) ->
            cases
            |> List.choose (fun caseClause -> inferValidationExpressionType environment freshCounter localTypes caseClause.Body)
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
        let bindPatternName (binding: SurfaceBindPattern) =
            match binding.Pattern with
            | NamePattern name -> Some name
            | _ -> None

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
                match bindPatternName binding, inferValidationExpressionType environment freshCounter localTypes expression with
                | Some bindingName, Some valueType -> Map.add bindingName valueType localTypes
                | _ -> localTypes

            inferValidationDoResultType environment freshCounter nextLocals rest
        | DoBind(binding, expression) :: rest ->
            let nextLocals =
                match bindPatternName binding, inferValidationExpressionType environment freshCounter localTypes expression with
                | Some bindingName, Some valueType -> Map.add bindingName (unwrapIoType valueType) localTypes
                | _ -> localTypes

            inferValidationDoResultType environment freshCounter nextLocals rest
        | DoUsing(pattern, expression) :: rest ->
            let nextLocals =
                match pattern, inferValidationExpressionType environment freshCounter localTypes expression with
                | NamePattern bindingName, Some valueType -> Map.add bindingName (unwrapIoType valueType) localTypes
                | _ -> localTypes

            inferValidationDoResultType environment freshCounter nextLocals rest
        | DoVar(bindingName, expression) :: rest ->
            let nextLocals =
                match inferValidationExpressionType environment freshCounter localTypes expression with
                | Some valueType -> Map.add bindingName (refType valueType) localTypes
                | None -> localTypes

            inferValidationDoResultType environment freshCounter nextLocals rest
        | DoAssign _ :: rest ->
            inferValidationDoResultType environment freshCounter localTypes rest
        | DoWhile _ :: rest ->
            inferValidationDoResultType environment freshCounter localTypes rest

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
                | DoUsing(pattern, expression) ->
                    let nextLocals =
                        match pattern, inferValidationExpressionType environment freshCounter locals expression with
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
                | DoWhile(condition, body) ->
                    validateExpression locals condition
                    @ validateDoStatements locals body
                    @ validateDoStatements locals rest

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
            | Match(_, cases) ->
                cases
                |> List.choose (fun caseClause -> inferExpressionType localTypes caseClause.Body)
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

        and inferDoResultType localTypes statements =
            let bindPatternName (binding: SurfaceBindPattern) =
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

        and lowerDoStatements scopeLabel localTypes statements =
            let bindPatternName (binding: SurfaceBindPattern) =
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
            | DoWhile(condition, body) :: rest ->
                KCoreSequence(
                    KCoreWhile(lowerExpression localTypes condition, lowerDoStatements scopeLabel localTypes body),
                    lowerDoStatements scopeLabel localTypes rest
                )

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
                let loweredCases : KCoreMatchCase list =
                    cases
                    |> List.collect (fun caseClause ->
                        let alternatives =
                            match caseClause.Pattern with
                            | OrPattern options -> options
                            | other -> [ other ]

                        alternatives
                        |> List.map (fun pattern ->
                            ({ Pattern = lowerKCorePattern pattern
                               Guard = caseClause.Guard |> Option.map (lowerExpression localTypes)
                               Body = lowerExpression localTypes caseClause.Body }
                             : KCoreMatchCase)))

                KCoreMatch(
                    lowerExpression localTypes scrutinee,
                    loweredCases
                )
            | RecordLiteral _ ->
                KCoreLiteral LiteralValue.Unit
            | RecordUpdate(receiver, _) ->
                lowerExpression localTypes receiver
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
            |> Option.map (lowerBindingBody bodyEnvironment scheme definition.Parameters)

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
        (filePath: string)
        (moduleName: string)
        (instanceInfo: InstanceInfo)
        =
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

        let environment =
            { CurrentModuleName = moduleName
              VisibleTypeAliases = visibleTypeAliases
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

                    let parameters =
                        buildBindingParameters environment.VisibleTypeAliases (Some specializedScheme) definition.Parameters

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

            let environment =
                { CurrentModuleName = moduleName
                  VisibleTypeAliases = mergeVisibleTypeAliases surfaceIndex moduleName
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
              Ownership = frontendModule.Ownership
              Declarations = declarations })
