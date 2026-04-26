namespace Kappa.Compiler

open System
open System.Numerics
open System.Security.Cryptography
open System.Text
open System.Threading

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
          Body: TypeExpr
          TerminationCertified: bool
          ConversionReducible: bool }

    type private TypeFacetInfo =
        { ModuleName: string
          Name: string
          Scheme: TypeScheme }

    type private StaticObjectKind =
        | StaticTypeObject
        | StaticTraitObject
        | StaticEffectLabelObject
        | StaticModuleObject

    type private StaticObjectInfo =
        { ObjectKind: StaticObjectKind
          NameSegments: string list
          Scheme: TypeScheme option }

    type private RecordSurfaceFieldInfo =
        { Name: string
          IsOpaque: bool
          IsImplicit: bool
          TypeTokens: Token list }

    type private RecordSurfaceInfo =
        { Fields: RecordSurfaceFieldInfo list
          RowTail: string option }

    type private BindingSchemeInfo =
        { ModuleName: string
          Name: string
          IsPattern: bool
          Scheme: TypeScheme
          TypeTokens: Token list
          ParameterLayouts: Parameter list option
          ConstructorTypeName: string option
          DefaultArguments: Map<string, SurfaceExpression> }

    type private ProjectionInfo =
        { ModuleName: string
          Name: string
          Binders: ProjectionBinder list
          ReturnType: TypeExpr
          Body: SurfaceProjectionBody option }

    type private ProjectionDescriptorAliasInfo =
        { Projection: ProjectionInfo
          RemainingPlaceBinders: ProjectionPlaceBinder list }

    type private ModuleSurfaceInfo =
        { TypeAliases: Map<string, TypeAliasInfo>
          TypeFacets: Map<string, TypeFacetInfo>
          RecordTypes: Map<string, RecordSurfaceInfo>
          BindingSchemes: Map<string, BindingSchemeInfo>
          Constructors: Map<string, BindingSchemeInfo>
          Projections: Map<string, ProjectionInfo>
          Traits: Map<string, TraitInfo>
          Instances: InstanceInfo list
          Imports: ImportSpec list
          ExportedTerms: Set<string>
          ExportedConstructors: Set<string>
          ExportedConstructorsByType: Map<string, Set<string>>
          ExportedTypes: Set<string>
          ExportedTraits: Set<string> }

    type private BindingLoweringEnvironment =
        { CurrentModuleName: string
          VisibleTypeAliases: Map<string, TypeAliasInfo>
          VisibleTypeFacets: Map<string, TypeFacetInfo>
          VisibleStaticObjects: Map<string, StaticObjectInfo>
          VisibleModules: Set<string>
          VisibleRecordTypes: Map<string, RecordSurfaceInfo>
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

    let private typeTextTokens (text: string) =
        let source = SourceText.From("__surface_elaboration_type__.kp", text)
        Lexer.tokenize source
        |> fun lexResult -> lexResult.Tokens
        |> List.filter (fun token -> token.Kind <> EndOfFile)

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
              TotalityAssertion = None
              IsPattern = false
              Name = Some name
              Parameters = []
              HeaderTokens = []
              ReturnTypeTokens = None
              BodyTokens = []
              Body = None }

    let private lowerKCoreParameter (parameter: Parameter) (parameterType: TypeExpr option) : KCoreParameter =
        { Name = parameter.Name
          Quantity = parameter.Quantity
          IsImplicit = parameter.IsImplicit
          Type = parameterType
          TypeText =
            parameterType
            |> Option.map TypeSignatures.toText
            |> Option.orElseWith (fun () -> parameter.TypeTokens |> Option.map tokensText) }

    let private lowerSyntheticKCoreParameter
        (name: string)
        (quantity: Quantity option)
        (isImplicit: bool)
        (parameterType: TypeExpr option)
        : KCoreParameter =
        { Name = name
          Quantity = quantity
          IsImplicit = isImplicit
          Type = parameterType
          TypeText = parameterType |> Option.map TypeSignatures.toText }

    let private explicitKCoreArgument expression =
        { ArgumentKind = KCoreExplicitArgument
          Expression = expression }

    let private implicitKCoreArgument expression =
        { ArgumentKind = KCoreImplicitArgument
          Expression = expression }

    let private inoutKCoreArgument expression =
        { ArgumentKind = KCoreInoutArgument
          Expression = expression }

    let rec private lowerKCorePattern (pattern: SurfacePattern) =
        match pattern with
        | WildcardPattern ->
            KCoreWildcardPattern
        | NamePattern name ->
            KCoreNamePattern name
        | AsPattern(_, inner) ->
            lowerKCorePattern inner
        | TypedPattern(inner, _) ->
            lowerKCorePattern inner
        | LiteralPattern literal ->
            KCoreLiteralPattern literal
        | ConstructorPattern(name, arguments) ->
            KCoreConstructorPattern(name, arguments |> List.map lowerKCorePattern)
        | NamedConstructorPattern(name, fields) ->
            KCoreConstructorPattern(name, fields |> List.map (fun field -> lowerKCorePattern field.Pattern))
        | TuplePattern _ ->
            KCoreWildcardPattern
        | VariantPattern(BoundVariantPattern(name, _))
        | VariantPattern(RestVariantPattern name) ->
            KCoreNamePattern name
        | VariantPattern(WildcardVariantPattern _) ->
            KCoreWildcardPattern
        | OrPattern alternatives ->
            KCoreOrPattern(alternatives |> List.map lowerKCorePattern)
        | AnonymousRecordPattern _ ->
            KCoreWildcardPattern

    let private collectPatternNames (pattern: SurfacePattern) =
        let rec loop current =
            seq {
                match current with
                | NamePattern name -> yield name
                | AsPattern(name, inner) ->
                    yield name
                    yield! loop inner
                | TypedPattern(inner, _) ->
                    yield! loop inner
                | ConstructorPattern(_, arguments) ->
                    for argument in arguments do
                        yield! loop argument
                | NamedConstructorPattern(_, fields) ->
                    for field in fields do
                        yield! loop field.Pattern
                | TuplePattern elements ->
                    for element in elements do
                        yield! loop element
                | VariantPattern(BoundVariantPattern(name, _))
                | VariantPattern(RestVariantPattern name) ->
                    yield name
                | VariantPattern(WildcardVariantPattern _) ->
                    ()
                | OrPattern alternatives ->
                    match alternatives with
                    | first :: _ ->
                        yield! loop first
                    | [] ->
                        ()
                | AnonymousRecordPattern(fields, rest) ->
                    for field in fields do
                        yield! loop field.Pattern
                    match rest with
                    | Some(BindRecordPatternRest name) -> yield name
                    | _ -> ()
                | WildcardPattern
                | LiteralPattern _ -> ()
            }

        loop pattern |> Seq.toList

    let private isOmegaText text =
        String.Equals(text, "\u03c9", StringComparison.Ordinal)
        || String.Equals(text, "omega", StringComparison.Ordinal)

    let private isQuantityVariableToken (token: Token) =
        if token.Kind <> Identifier then
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

    let private tryParseSuspensionMarker (tokens: Token list) =
        match tokens with
        | head :: rest when Token.isKeyword Keyword.Thunk head ->
            Some({ head with Kind = Identifier; Text = "Thunk" }, rest)
        | head :: rest when Token.isKeyword Keyword.Lazy head ->
            Some({ head with Kind = Identifier; Text = "Need" }, rest)
        | _ ->
            None

    let private splitTopLevelCommaGroups (tokens: Token list) =
        let tokenArray = tokens |> List.toArray
        let groups = ResizeArray<Token list>()
        let current = ResizeArray<Token>()
        let mutable depth = 0

        for token in tokenArray do
            match token.Kind with
            | LeftParen
            | LeftBracket
            | LeftBrace
            | LeftSetBrace ->
                depth <- depth + 1
                current.Add(token)
            | RightParen
            | RightBracket
            | RightBrace
            | RightSetBrace ->
                depth <- max 0 (depth - 1)
                current.Add(token)
            | Comma when depth = 0 ->
                groups.Add(List.ofSeq current)
                current.Clear()
            | _ ->
                current.Add(token)

        if current.Count > 0 then
            groups.Add(List.ofSeq current)

        groups |> Seq.toList

    let private tryFindTopLevelToken predicate (tokens: Token list) =
        let tokenArray = tokens |> List.toArray
        let mutable depth = 0
        let mutable index = 0
        let mutable result = None

        while index < tokenArray.Length && result.IsNone do
            match tokenArray[index].Kind with
            | LeftParen
            | LeftBracket
            | LeftBrace
            | LeftSetBrace ->
                depth <- depth + 1
            | RightParen
            | RightBracket
            | RightBrace
            | RightSetBrace ->
                depth <- max 0 (depth - 1)
            | _ when depth = 0 && predicate tokenArray[index] ->
                result <- Some index
            | _ ->
                ()

            index <- index + 1

        result

    let private stripOuterParens (tokens: Token list) =
        let trimmed = significantTokens tokens

        match trimmed with
        | { Kind = LeftParen } :: rest when not (List.isEmpty rest) && (List.last rest).Kind = RightParen ->
            Some(rest |> List.take (List.length rest - 1))
        | _ ->
            None

    let private tryParseRecordSurfaceField (tokens: Token list) =
        let tokens = significantTokens tokens

        let isOpaque, afterOpaque =
            match tokens with
            | head :: rest when Token.isKeyword Keyword.Opaque head -> true, rest
            | _ -> false, tokens

        let isImplicit, afterImplicit =
            match afterOpaque with
            | { Kind = AtSign } :: rest -> true, rest
            | _ -> false, afterOpaque

        let afterQuantity =
            match tryParseQuantityPrefix afterImplicit with
            | Some(_, rest) -> rest
            | None -> afterImplicit

        let suspensionTypeToken, afterSuspension =
            match tryParseSuspensionMarker afterQuantity with
            | Some(typeToken, rest) -> Some typeToken, rest
            | None -> None, afterQuantity

        tryFindTopLevelToken (fun token -> token.Kind = Colon) afterSuspension
        |> Option.bind (fun colonIndex ->
            let tokenArray = afterSuspension |> List.toArray
            let binderTokens = tokenArray[0 .. colonIndex - 1] |> Array.toList
            let rawTypeTokens = tokenArray[colonIndex + 1 ..] |> Array.toList
            let typeTokens =
                match suspensionTypeToken with
                | Some typeToken -> typeToken :: rawTypeTokens
                | None -> rawTypeTokens

            match binderTokens, typeTokens with
            | [ nameToken ], _ when Token.isName nameToken && not (List.isEmpty typeTokens) ->
                Some
                    { Name = SyntaxFacts.trimIdentifierQuotes nameToken.Text
                      IsOpaque = isOpaque
                      IsImplicit = isImplicit
                      TypeTokens = typeTokens }
            | _ ->
                None)

    let private tryParseRecordSurfaceInfo (tokens: Token list) =
        stripOuterParens tokens
        |> Option.bind (fun innerTokens ->
            let fieldGroups = splitTopLevelCommaGroups innerTokens |> List.filter (List.isEmpty >> not)
            let fields = ResizeArray<RecordSurfaceFieldInfo>()
            let mutable rowTail = None
            let mutable valid = true

            for group in fieldGroups do
                let fieldTokens, tailTokens =
                    match tryFindTopLevelToken (fun token -> token.Kind = Operator && token.Text = "|") group with
                    | Some pipeIndex ->
                        let tokenArray = group |> List.toArray
                        tokenArray[0 .. pipeIndex - 1] |> Array.toList,
                        Some(tokenArray[pipeIndex + 1 ..] |> Array.toList)
                    | None ->
                        group, None

                match tailTokens with
                | Some tokens ->
                    match significantTokens tokens with
                    | [ rowToken ] when Token.isName rowToken ->
                        rowTail <- Some(SyntaxFacts.trimIdentifierQuotes rowToken.Text)
                    | _ ->
                        valid <- false
                | None ->
                    ()

                if not (List.isEmpty (significantTokens fieldTokens)) then
                    match tryParseRecordSurfaceField fieldTokens with
                    | Some field -> fields.Add(field)
                    | None -> valid <- false

            if valid && (fields.Count > 0 || rowTail.IsSome) then
                Some
                    { Fields = List.ofSeq fields
                      RowTail = rowTail }
            else
                None)

    let private recordSurfaceFieldNames (recordInfo: RecordSurfaceInfo) =
        recordInfo.Fields |> List.map (fun field -> field.Name) |> Set.ofList

    let private tokenText (tokens: Token list) =
        tokens
        |> significantTokens
        |> List.map (fun token -> token.Text)
        |> String.concat " "

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

            let suspensionTypeToken, bodyTokens =
                match tryParseSuspensionMarker bodyTokens with
                | Some(typeToken, rest) -> Some typeToken, rest
                | None -> None, bodyTokens

            match bodyTokens with
            | nameToken :: { Kind = Colon } :: typeTokens when Token.isName nameToken && not (List.isEmpty typeTokens) ->
                Some
                    { Name = SyntaxFacts.trimIdentifierQuotes nameToken.Text
                      TypeTokens =
                        Some(suspensionTypeToken |> Option.map (fun typeToken -> typeToken :: typeTokens) |> Option.defaultValue typeTokens)
                      Quantity = quantity
                      IsImplicit = isImplicit
                      IsInout = isInout
                      IsReceiver = false }
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

    let private stripForallOnlyParameterLayouts
        (scheme: TypeScheme)
        (parameterLayouts: Parameter list option)
        =
        let isForallLayoutParameter (parameter: Parameter) =
            parameter.IsImplicit
            && (scheme.Forall |> List.exists (fun binder -> binder.Name = parameter.Name))
            && (parameter.TypeTokens.IsNone
                || (parameter.TypeTokens
                    |> Option.bind TypeSignatures.parseType
                    |> Option.exists (function
                        | TypeUniverse None -> true
                        | TypeName([ "Type" ], []) -> true
                        | _ -> false)))

        parameterLayouts
        |> Option.map (List.filter (isForallLayoutParameter >> not))

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

    let private typeObjectType =
        TypeUniverse None

    let private constraintObjectType =
        TypeIntrinsic ConstraintClassifier

    let private effectLabelObjectType =
        TypeIntrinsic EffLabelClassifier

    let private staticObjectTypeText (info: StaticObjectInfo) =
        info.Scheme
        |> Option.map (fun scheme -> TypeSignatures.toText scheme.Body)

    let private traitFacetScheme (traitInfo: TraitInfo) =
        let body =
            traitInfo.TypeParameters
            |> List.rev
            |> List.fold (fun current _ -> TypeArrow(QuantityOmega, typeObjectType, current)) constraintObjectType

        { Forall = []
          Constraints = []
          Body = body }

    let private typeFacetStaticObject (typeFacetInfo: TypeFacetInfo) =
        { ObjectKind = StaticTypeObject
          NameSegments = [ typeFacetInfo.Name ]
          Scheme = Some typeFacetInfo.Scheme }

    let private scopedEffectTypeStaticObject name =
        { ObjectKind = StaticTypeObject
          NameSegments = [ name ]
          Scheme =
            Some
                { Forall = []
                  Constraints = []
                  Body = typeObjectType } }

    let private scopedEffectLabelStaticObject name =
        { ObjectKind = StaticEffectLabelObject
          NameSegments = [ name ]
          Scheme =
            Some
                { Forall = []
                  Constraints = []
                  Body = effectLabelObjectType } }

    let private traitStaticObject (traitInfo: TraitInfo) =
        { ObjectKind = StaticTraitObject
          NameSegments = [ traitInfo.Name ]
          Scheme = Some(traitFacetScheme traitInfo) }

    let private tryResolveVisibleTypeFacetInfo
        (environment: BindingLoweringEnvironment)
        nameSegments
        =
        match List.rev nameSegments with
        | [] ->
            None
        | typeName :: reversedModuleSegments ->
            environment.VisibleTypeFacets
            |> Map.tryFind typeName
            |> Option.filter (fun typeFacetInfo ->
                match reversedModuleSegments with
                | [] ->
                    true
                | _ ->
                    String.Equals(
                        typeFacetInfo.ModuleName,
                        SyntaxFacts.moduleNameToText (List.rev reversedModuleSegments),
                        StringComparison.Ordinal
                    ))

    let private tryResolveStaticObject
        (environment: BindingLoweringEnvironment)
        expression
        =
        match expression with
        | KindQualifiedName(TypeKind, nameSegments) ->
            tryResolveVisibleTypeFacetInfo environment nameSegments
            |> Option.map typeFacetStaticObject
        | KindQualifiedName(TraitKind, nameSegments) ->
            match List.rev nameSegments with
            | [] -> None
            | traitName :: _ ->
                environment.VisibleTraits
                |> Map.tryFind traitName
                |> Option.map traitStaticObject
        | KindQualifiedName(EffectLabelKind, _) ->
            None
        | KindQualifiedName(ModuleKind, nameSegments) ->
            let moduleName = SyntaxFacts.moduleNameToText nameSegments

            if environment.VisibleModules.Contains moduleName then
                Some
                    { ObjectKind = StaticModuleObject
                      NameSegments = nameSegments
                      Scheme = None }
            else
                None
        | Name nameSegments when not (List.isEmpty nameSegments) ->
            let aliasName = SyntaxFacts.moduleNameToText nameSegments

            environment.VisibleStaticObjects
            |> Map.tryFind aliasName
            |> Option.orElseWith (fun () ->
                match nameSegments with
                | [ root ]
                    when not (environment.VisibleBindings.ContainsKey root)
                         && Option.isNone (tryResolveVisibleConstructorInfo environment [ root ]) ->
                    environment.VisibleTypeFacets
                    |> Map.tryFind root
                    |> Option.map typeFacetStaticObject
                    |> Option.orElseWith (fun () ->
                        environment.VisibleTraits
                        |> Map.tryFind root
                        |> Option.map traitStaticObject)
                | _ ->
                    None)
        | _ ->
            None

    let private tryResolveStaticReceiverObject
        (environment: BindingLoweringEnvironment)
        expression
        =
        match expression with
        | Name nameSegments when not (List.isEmpty nameSegments) ->
            let aliasName = SyntaxFacts.moduleNameToText nameSegments

            environment.VisibleStaticObjects
            |> Map.tryFind aliasName
            |> Option.orElseWith (fun () ->
                match nameSegments with
                | [ root ] ->
                    environment.VisibleTypeFacets
                    |> Map.tryFind root
                    |> Option.map typeFacetStaticObject
                | _ ->
                    None)
        | _ ->
            tryResolveStaticObject environment expression

    let private scopedEffects = AsyncLocal<Map<string, EffectDeclaration> option>()

    let private currentScopedEffects () =
        scopedEffects.Value |> Option.defaultValue Map.empty

    let private currentScopedEffectNames () =
        currentScopedEffects () |> Map.keys |> Set.ofSeq

    let private tryFindScopedEffectDeclaration name =
        currentScopedEffects () |> Map.tryFind name

    let private withScopedEffectDeclaration (declaration: EffectDeclaration) work =
        let saved = currentScopedEffects ()
        scopedEffects.Value <- Some(Map.add declaration.Name declaration saved)

        try
            work ()
        finally
            scopedEffects.Value <- Some saved

    let private tryResolveScopedStaticObject
        (environment: BindingLoweringEnvironment)
        expression
        =
        let scopedEffectNames = currentScopedEffectNames ()

        match expression with
        | KindQualifiedName(TypeKind, [ name ]) when Set.contains name scopedEffectNames ->
            Some(scopedEffectTypeStaticObject name)
        | KindQualifiedName(EffectLabelKind, [ name ]) when Set.contains name scopedEffectNames ->
            Some(scopedEffectLabelStaticObject name)
        | _ ->
            tryResolveStaticObject environment expression

    let private kindSelectorText selector =
        match selector with
        | TypeKind -> "type"
        | TraitKind -> "trait"
        | EffectLabelKind -> "effect-label"
        | ModuleKind -> "module"

    let private staticObjectExpression (staticObject: StaticObjectInfo) =
        let selector =
            match staticObject.ObjectKind with
            | StaticTypeObject -> TypeKind
            | StaticTraitObject -> TraitKind
            | StaticEffectLabelObject -> EffectLabelKind
            | StaticModuleObject -> ModuleKind

        KindQualifiedName(selector, staticObject.NameSegments)

    let private rewriteStaticObjectAliasUse aliasName staticObject expression =
        let aliasIsBoundByPattern pattern =
            collectPatternNames pattern
            |> List.exists (fun name -> String.Equals(name, aliasName, StringComparison.Ordinal))

        let aliasIsBoundByParameters (parameters: Parameter list) =
            parameters
            |> List.exists (fun parameter -> String.Equals(parameter.Name, aliasName, StringComparison.Ordinal))

        let aliasIsBoundByName name =
            String.Equals(name, aliasName, StringComparison.Ordinal)

        let rec rewrite current =
            match current with
            | Name [ root ] when aliasIsBoundByName root ->
                staticObjectExpression staticObject
            | Name(root :: path) when aliasIsBoundByName root ->
                Name(staticObject.NameSegments @ path)
            | Name _ ->
                current
            | SyntaxQuote inner ->
                SyntaxQuote(rewrite inner)
            | SyntaxSplice inner ->
                SyntaxSplice(rewrite inner)
            | TopLevelSyntaxSplice inner ->
                TopLevelSyntaxSplice(rewrite inner)
            | CodeQuote inner ->
                CodeQuote(rewrite inner)
            | CodeSplice inner ->
                CodeSplice(rewrite inner)
            | LocalLet(binding, value, body) ->
                let rewrittenValue = rewrite value

                if aliasIsBoundByPattern binding.Pattern then
                    LocalLet(binding, rewrittenValue, body)
                else
                    LocalLet(binding, rewrittenValue, rewrite body)
            | LocalSignature(declaration, body) ->
                if aliasIsBoundByName declaration.Name then
                    LocalSignature(declaration, body)
                else
                    LocalSignature(declaration, rewrite body)
            | LocalTypeAlias(declaration, body) ->
                LocalTypeAlias(declaration, rewrite body)
            | LocalScopedEffect(declaration, body) ->
                if aliasIsBoundByName declaration.Name then
                    LocalScopedEffect(declaration, body)
                else
                    LocalScopedEffect(declaration, rewrite body)
            | Handle(isDeep, label, body, returnClause, operationClauses) ->
                let rewriteClause (clause: SurfaceEffectHandlerClause) =
                    { clause with
                        Body = rewrite clause.Body }

                Handle(
                    isDeep,
                    rewrite label,
                    rewrite body,
                    rewriteClause returnClause,
                    operationClauses |> List.map rewriteClause
                )
            | Lambda(parameters, body) ->
                if aliasIsBoundByParameters parameters then
                    Lambda(parameters, body)
                else
                    Lambda(parameters, rewrite body)
            | IfThenElse(condition, whenTrue, whenFalse) ->
                IfThenElse(rewrite condition, rewrite whenTrue, rewrite whenFalse)
            | Match(scrutinee, cases) ->
                Match(
                    rewrite scrutinee,
                    cases
                    |> List.map (fun caseClause ->
                        if aliasIsBoundByPattern caseClause.Pattern then
                            caseClause
                        else
                            { caseClause with
                                Guard = caseClause.Guard |> Option.map rewrite
                                Body = rewrite caseClause.Body })
                )
            | RecordLiteral fields ->
                RecordLiteral(fields |> List.map (fun field -> { field with Value = rewrite field.Value }))
            | Seal(value, ascriptionTokens) ->
                Seal(rewrite value, ascriptionTokens)
            | RecordUpdate(receiver, fields) ->
                RecordUpdate(rewrite receiver, fields |> List.map (fun field -> { field with Value = rewrite field.Value }))
            | MemberAccess(receiver, segments, arguments) ->
                MemberAccess(rewrite receiver, segments, arguments |> List.map rewrite)
            | SafeNavigation(receiver, navigation) ->
                SafeNavigation(rewrite receiver, { navigation with Arguments = navigation.Arguments |> List.map rewrite })
            | TagTest(receiver, constructorName) ->
                TagTest(rewrite receiver, constructorName)
            | Do statements ->
                let rec rewriteStatements remaining =
                    match remaining with
                    | [] -> []
                    | statement :: rest ->
                        match statement with
                        | DoLet(binding, value) ->
                            let rewritten = DoLet(binding, rewrite value)
                            if aliasIsBoundByPattern binding.Pattern then
                                rewritten :: rest
                            else
                                rewritten :: rewriteStatements rest
                        | DoLetQuestion(binding, value, failure) ->
                            let rewrittenFailure =
                                failure
                                |> Option.map (fun failureClause ->
                                    if aliasIsBoundByPattern failureClause.ResiduePattern.Pattern then
                                        failureClause
                                    else
                                        { failureClause with Body = rewriteStatements failureClause.Body })

                            let rewritten = DoLetQuestion(binding, rewrite value, rewrittenFailure)
                            if aliasIsBoundByPattern binding.Pattern then
                                rewritten :: rest
                            else
                                rewritten :: rewriteStatements rest
                        | DoBind(binding, value) ->
                            let rewritten = DoBind(binding, rewrite value)
                            if aliasIsBoundByPattern binding.Pattern then
                                rewritten :: rest
                            else
                                rewritten :: rewriteStatements rest
                        | DoUsing(binding, value) ->
                            let rewritten = DoUsing(binding, rewrite value)
                            if aliasIsBoundByPattern binding.Pattern then
                                rewritten :: rest
                            else
                                rewritten :: rewriteStatements rest
                        | DoVar(name, value) ->
                            let rewritten = DoVar(name, rewrite value)
                            if aliasIsBoundByName name then
                                rewritten :: rest
                            else
                                rewritten :: rewriteStatements rest
                        | DoAssign(name, value) ->
                            DoAssign(name, rewrite value) :: rewriteStatements rest
                        | DoDefer value ->
                            DoDefer(rewrite value) :: rewriteStatements rest
                        | DoIf(condition, whenTrue, whenFalse) ->
                            DoIf(condition |> rewrite, whenTrue |> rewriteStatements, whenFalse |> rewriteStatements)
                            :: rewriteStatements rest
                        | DoWhile(condition, body) ->
                            DoWhile(rewrite condition, rewriteStatements body) :: rewriteStatements rest
                        | DoReturn value ->
                            DoReturn(rewrite value) :: rest
                        | DoExpression value ->
                            DoExpression(rewrite value) :: rewriteStatements rest

                Do(rewriteStatements statements)
            | MonadicSplice inner ->
                MonadicSplice(rewrite inner)
            | ExplicitImplicitArgument inner ->
                ExplicitImplicitArgument(rewrite inner)
            | NamedApplicationBlock fields ->
                NamedApplicationBlock(fields |> List.map (fun field -> { field with Value = rewrite field.Value }))
            | Apply(callee, arguments) ->
                Apply(rewrite callee, arguments |> List.map rewrite)
            | InoutArgument inner ->
                InoutArgument(rewrite inner)
            | Unary(operatorName, operand) ->
                Unary(operatorName, rewrite operand)
            | Binary(left, operatorName, right) ->
                Binary(rewrite left, operatorName, rewrite right)
            | Elvis(left, right) ->
                Elvis(rewrite left, rewrite right)
            | PrefixedString(prefix, parts) ->
                PrefixedString(
                    prefix,
                    parts
                    |> List.map (function
                        | StringText _ as part -> part
                        | StringInterpolation(inner, format) -> StringInterpolation(rewrite inner, format))
                )
            | Literal _
            | NumericLiteral _
            | KindQualifiedName _ ->
                current

        rewrite expression

    let private rewriteNamedApplicationAliasUse
        aliasName
        (parameters: Parameter list)
        expression
        =
        let supportedParameters =
            parameters
            |> List.forall (fun parameter -> not parameter.IsImplicit && not parameter.IsInout && not parameter.IsReceiver)

        let tryReorder arguments =
            match supportedParameters, arguments with
            | true, [ NamedApplicationBlock fields ] ->
                let fieldMap =
                    fields
                    |> List.map (fun field -> field.Name, field.Value)
                    |> Map.ofList

                if parameters |> List.forall (fun parameter -> fieldMap.ContainsKey(parameter.Name)) then
                    parameters
                    |> List.map (fun parameter -> fieldMap[parameter.Name])
                    |> Some
                else
                    None
            | _ ->
                None

        let rec rewrite current =
            match current with
            | SyntaxQuote inner ->
                SyntaxQuote(rewrite inner)
            | SyntaxSplice inner ->
                SyntaxSplice(rewrite inner)
            | TopLevelSyntaxSplice inner ->
                TopLevelSyntaxSplice(rewrite inner)
            | CodeQuote inner ->
                CodeQuote(rewrite inner)
            | CodeSplice inner ->
                CodeSplice(rewrite inner)
            | Handle(isDeep, label, body, returnClause, operationClauses) ->
                let rewriteClause (clause: SurfaceEffectHandlerClause) =
                    { clause with
                        Body = rewrite clause.Body }

                Handle(
                    isDeep,
                    rewrite label,
                    rewrite body,
                    rewriteClause returnClause,
                    operationClauses |> List.map rewriteClause
                )
            | Apply(Name [ name ], arguments) when String.Equals(name, aliasName, StringComparison.Ordinal) ->
                match tryReorder arguments with
                | Some reorderedArguments ->
                    Apply(Name [ name ], reorderedArguments |> List.map rewrite)
                | None ->
                    Apply(Name [ name ], arguments |> List.map rewrite)
            | Apply(callee, arguments) ->
                Apply(rewrite callee, arguments |> List.map rewrite)
            | LocalLet(binding, value, body) ->
                let shadowsAlias =
                    collectPatternNames binding.Pattern
                    |> List.exists (fun name -> String.Equals(name, aliasName, StringComparison.Ordinal))

                LocalLet(binding, rewrite value, if shadowsAlias then body else rewrite body)
            | LocalSignature(declaration, body) ->
                if String.Equals(declaration.Name, aliasName, StringComparison.Ordinal) then
                    current
                else
                    LocalSignature(declaration, rewrite body)
            | LocalTypeAlias(declaration, body) ->
                LocalTypeAlias(declaration, rewrite body)
            | LocalScopedEffect(declaration, body) ->
                if String.Equals(declaration.Name, aliasName, StringComparison.Ordinal) then
                    current
                else
                    LocalScopedEffect(declaration, rewrite body)
            | Lambda(parameters, body) ->
                if parameters |> List.exists (fun parameter -> String.Equals(parameter.Name, aliasName, StringComparison.Ordinal)) then
                    current
                else
                    Lambda(parameters, rewrite body)
            | IfThenElse(condition, whenTrue, whenFalse) ->
                IfThenElse(rewrite condition, rewrite whenTrue, rewrite whenFalse)
            | Match(scrutinee, cases) ->
                Match(
                    rewrite scrutinee,
                    cases
                    |> List.map (fun caseClause ->
                        { caseClause with
                            Guard = caseClause.Guard |> Option.map rewrite
                            Body = rewrite caseClause.Body })
                )
            | RecordLiteral fields ->
                RecordLiteral(fields |> List.map (fun field -> { field with Value = rewrite field.Value }))
            | Seal(value, ascriptionTokens) ->
                Seal(rewrite value, ascriptionTokens)
            | RecordUpdate(receiver, fields) ->
                RecordUpdate(rewrite receiver, fields |> List.map (fun field -> { field with Value = rewrite field.Value }))
            | MemberAccess(receiver, segments, arguments) ->
                MemberAccess(rewrite receiver, segments, arguments |> List.map rewrite)
            | SafeNavigation(receiver, navigation) ->
                SafeNavigation(rewrite receiver, { navigation with Arguments = navigation.Arguments |> List.map rewrite })
            | TagTest(receiver, constructorName) ->
                TagTest(rewrite receiver, constructorName)
            | Do statements ->
                Do statements
            | MonadicSplice inner ->
                MonadicSplice(rewrite inner)
            | ExplicitImplicitArgument inner ->
                ExplicitImplicitArgument(rewrite inner)
            | NamedApplicationBlock fields ->
                NamedApplicationBlock(fields |> List.map (fun field -> { field with Value = rewrite field.Value }))
            | InoutArgument inner ->
                InoutArgument(rewrite inner)
            | Unary(operatorName, inner) ->
                Unary(operatorName, rewrite inner)
            | Binary(left, operatorName, right) ->
                Binary(rewrite left, operatorName, rewrite right)
            | Elvis(left, right) ->
                Elvis(rewrite left, rewrite right)
            | PrefixedString(prefix, parts) ->
                PrefixedString(
                    prefix,
                    parts
                    |> List.map (function
                        | StringText text -> StringText text
                        | StringInterpolation(inner, format) -> StringInterpolation(rewrite inner, format))
                )
            | Literal _
            | NumericLiteral _
            | KindQualifiedName _
            | Name _ ->
                current

        rewrite expression

    let private rewriteDirectAliasUse aliasName targetName expression =
        let rec rewrite current =
            match current with
            | SyntaxQuote inner ->
                SyntaxQuote(rewrite inner)
            | SyntaxSplice inner ->
                SyntaxSplice(rewrite inner)
            | TopLevelSyntaxSplice inner ->
                TopLevelSyntaxSplice(rewrite inner)
            | CodeQuote inner ->
                CodeQuote(rewrite inner)
            | CodeSplice inner ->
                CodeSplice(rewrite inner)
            | Handle(isDeep, label, body, returnClause, operationClauses) ->
                let rewriteClause (clause: SurfaceEffectHandlerClause) =
                    { clause with
                        Body = rewrite clause.Body }

                Handle(
                    isDeep,
                    rewrite label,
                    rewrite body,
                    rewriteClause returnClause,
                    operationClauses |> List.map rewriteClause
                )
            | Apply(Name [ name ], arguments) when String.Equals(name, aliasName, StringComparison.Ordinal) ->
                Apply(Name [ targetName ], arguments |> List.map rewrite)
            | Apply(callee, arguments) ->
                Apply(rewrite callee, arguments |> List.map rewrite)
            | LocalLet(binding, value, body) ->
                let shadowsAlias =
                    collectPatternNames binding.Pattern
                    |> List.exists (fun name -> String.Equals(name, aliasName, StringComparison.Ordinal))

                LocalLet(binding, rewrite value, if shadowsAlias then body else rewrite body)
            | LocalSignature(declaration, body) ->
                if String.Equals(declaration.Name, aliasName, StringComparison.Ordinal) then
                    current
                else
                    LocalSignature(declaration, rewrite body)
            | LocalTypeAlias(declaration, body) ->
                LocalTypeAlias(declaration, rewrite body)
            | LocalScopedEffect(declaration, body) ->
                if String.Equals(declaration.Name, aliasName, StringComparison.Ordinal) then
                    current
                else
                    LocalScopedEffect(declaration, rewrite body)
            | Lambda(parameters, body) ->
                if parameters |> List.exists (fun parameter -> String.Equals(parameter.Name, aliasName, StringComparison.Ordinal)) then
                    current
                else
                    Lambda(parameters, rewrite body)
            | IfThenElse(condition, whenTrue, whenFalse) ->
                IfThenElse(rewrite condition, rewrite whenTrue, rewrite whenFalse)
            | Match(scrutinee, cases) ->
                Match(
                    rewrite scrutinee,
                    cases
                    |> List.map (fun caseClause ->
                        { caseClause with
                            Guard = caseClause.Guard |> Option.map rewrite
                            Body = rewrite caseClause.Body })
                )
            | RecordLiteral fields ->
                RecordLiteral(fields |> List.map (fun field -> { field with Value = rewrite field.Value }))
            | Seal(value, ascriptionTokens) ->
                Seal(rewrite value, ascriptionTokens)
            | RecordUpdate(receiver, fields) ->
                RecordUpdate(rewrite receiver, fields |> List.map (fun field -> { field with Value = rewrite field.Value }))
            | MemberAccess(receiver, segments, arguments) ->
                MemberAccess(rewrite receiver, segments, arguments |> List.map rewrite)
            | SafeNavigation(receiver, navigation) ->
                SafeNavigation(rewrite receiver, { navigation with Arguments = navigation.Arguments |> List.map rewrite })
            | TagTest(receiver, constructorName) ->
                TagTest(rewrite receiver, constructorName)
            | Do statements ->
                Do statements
            | MonadicSplice inner ->
                MonadicSplice(rewrite inner)
            | ExplicitImplicitArgument inner ->
                ExplicitImplicitArgument(rewrite inner)
            | NamedApplicationBlock fields ->
                NamedApplicationBlock(fields |> List.map (fun field -> { field with Value = rewrite field.Value }))
            | InoutArgument inner ->
                InoutArgument(rewrite inner)
            | Unary(operatorName, inner) ->
                Unary(operatorName, rewrite inner)
            | Binary(left, operatorName, right) ->
                Binary(rewrite left, operatorName, rewrite right)
            | Elvis(left, right) ->
                Elvis(rewrite left, rewrite right)
            | PrefixedString(prefix, parts) ->
                PrefixedString(
                    prefix,
                    parts
                    |> List.map (function
                        | StringText text -> StringText text
                        | StringInterpolation(inner, format) -> StringInterpolation(rewrite inner, format))
                )
            | Literal _
            | NumericLiteral _
            | KindQualifiedName _
            | Name _ ->
                current

        rewrite expression

    let private rewriteStaticObjectPathAliasUses (aliases: Map<string list, StaticObjectInfo>) expression =
        let sortedAliases =
            aliases
            |> Map.toList
            |> List.sortByDescending (fun (segments, _) -> List.length segments)

        let shadowsAnyAlias names (activeAliases: Map<string list, StaticObjectInfo>) =
            if Set.isEmpty names then
                false
            else
                activeAliases
                |> Map.exists (fun segments _ ->
                    match segments with
                    | root :: _ -> Set.contains root names
                    | [] -> false)

        let withoutShadowedAliases names (activeAliases: Map<string list, StaticObjectInfo>) =
            if Set.isEmpty names then
                activeAliases
            else
                activeAliases
                |> Map.filter (fun segments _ ->
                    match segments with
                    | root :: _ -> not (Set.contains root names)
                    | [] -> true)

        let tryRewriteName activeAliases segments =
            let candidates =
                activeAliases
                |> Map.toList
                |> List.sortByDescending (fun (aliasSegments, _) -> List.length aliasSegments)

            candidates
            |> List.tryPick (fun (aliasSegments, staticObject) ->
                if List.length aliasSegments <= List.length segments
                   && (segments |> List.take (List.length aliasSegments)) = aliasSegments then
                    let rest = segments |> List.skip (List.length aliasSegments)

                    if List.isEmpty rest then
                        Some(staticObjectExpression staticObject)
                    else
                        Some(Name(staticObject.NameSegments @ rest))
                else
                    None)

        let rec rewrite activeAliases current =
            match current with
            | Name segments ->
                tryRewriteName activeAliases segments
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
            | Handle(isDeep, label, body, returnClause, operationClauses) ->
                let rewriteClause (clause: SurfaceEffectHandlerClause) =
                    { clause with
                        Body = rewrite activeAliases clause.Body }

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
                let bodyAliases = withoutShadowedAliases shadowedNames activeAliases

                LocalLet(binding, rewrittenValue, rewrite bodyAliases body)
            | LocalSignature(declaration, body) ->
                LocalSignature(declaration, rewrite (withoutShadowedAliases (Set.singleton declaration.Name) activeAliases) body)
            | LocalTypeAlias(declaration, body) ->
                LocalTypeAlias(declaration, rewrite activeAliases body)
            | LocalScopedEffect(declaration, body) ->
                let bodyAliases = withoutShadowedAliases (Set.singleton declaration.Name) activeAliases
                LocalScopedEffect(declaration, rewrite bodyAliases body)
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
                MemberAccess(rewrite activeAliases receiver, segments, arguments |> List.map (rewrite activeAliases))
            | SafeNavigation(receiver, navigation) ->
                SafeNavigation(rewrite activeAliases receiver, { navigation with Arguments = navigation.Arguments |> List.map (rewrite activeAliases) })
            | TagTest(receiver, constructorName) ->
                TagTest(rewrite activeAliases receiver, constructorName)
            | Do statements ->
                let rec rewriteStatements activeAliases remaining =
                    match remaining with
                    | [] -> []
                    | statement :: rest ->
                        match statement with
                        | DoLet(binding, value) ->
                            let rewritten = DoLet(binding, rewrite activeAliases value)
                            let shadowedNames = collectPatternNames binding.Pattern |> Set.ofList

                            rewritten :: rewriteStatements (withoutShadowedAliases shadowedNames activeAliases) rest
                        | DoLetQuestion(binding, value, failure) ->
                            let rewrittenFailure =
                                failure
                                |> Option.map (fun failureClause ->
                                    let shadowedNames = collectPatternNames failureClause.ResiduePattern.Pattern |> Set.ofList

                                    { failureClause with
                                        Body = rewriteStatements (withoutShadowedAliases shadowedNames activeAliases) failureClause.Body })

                            let rewritten = DoLetQuestion(binding, rewrite activeAliases value, rewrittenFailure)
                            let shadowedNames = collectPatternNames binding.Pattern |> Set.ofList

                            rewritten :: rewriteStatements (withoutShadowedAliases shadowedNames activeAliases) rest
                        | DoBind(binding, value)
                        | DoUsing(binding, value) ->
                            let rewrittenValue = rewrite activeAliases value
                            let rewritten =
                                match statement with
                                | DoBind _ -> DoBind(binding, rewrittenValue)
                                | _ -> DoUsing(binding, rewrittenValue)

                            let shadowedNames = collectPatternNames binding.Pattern |> Set.ofList
                            rewritten :: rewriteStatements (withoutShadowedAliases shadowedNames activeAliases) rest
                        | DoVar(name, value) ->
                            DoVar(name, rewrite activeAliases value)
                            :: rewriteStatements (withoutShadowedAliases (Set.singleton name) activeAliases) rest
                        | DoAssign(name, value) ->
                            DoAssign(name, rewrite activeAliases value) :: rewriteStatements activeAliases rest
                        | DoDefer value ->
                            DoDefer(rewrite activeAliases value) :: rewriteStatements activeAliases rest
                        | DoIf(condition, whenTrue, whenFalse) ->
                            DoIf(
                                rewrite activeAliases condition,
                                rewriteStatements activeAliases whenTrue,
                                rewriteStatements activeAliases whenFalse
                            )
                            :: rewriteStatements activeAliases rest
                        | DoWhile(condition, body) ->
                            DoWhile(rewrite activeAliases condition, rewriteStatements activeAliases body)
                            :: rewriteStatements activeAliases rest
                        | DoReturn value ->
                            DoReturn(rewrite activeAliases value) :: rest
                        | DoExpression value ->
                            DoExpression(rewrite activeAliases value) :: rewriteStatements activeAliases rest

                Do(rewriteStatements activeAliases statements)
            | MonadicSplice inner ->
                MonadicSplice(rewrite activeAliases inner)
            | ExplicitImplicitArgument inner ->
                ExplicitImplicitArgument(rewrite activeAliases inner)
            | NamedApplicationBlock fields ->
                NamedApplicationBlock(fields |> List.map (fun field -> { field with Value = rewrite activeAliases field.Value }))
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

        if List.isEmpty sortedAliases then
            expression
        else
            rewrite aliases expression

    let private unitType =
        TypeName([ "Unit" ], [])

    let private boolType =
        TypeName([ "Bool" ], [])

    let private intType =
        TypeName([ "Int" ], [])

    let private integerType =
        TypeName([ "Integer" ], [])

    let private floatType =
        TypeName([ "Float" ], [])

    let private doubleType =
        TypeName([ "Double" ], [])

    let private stringType =
        TypeName([ "String" ], [])

    let private charType =
        TypeName([ "Char" ], [])

    let private syntaxType argumentType =
        TypeName([ "Syntax" ], [ argumentType ])

    let private codeType argumentType =
        TypeName([ "Code" ], [ argumentType ])

    let private ioType argumentType =
        TypeName([ "IO" ], [ argumentType ])

    let private refType argumentType =
        TypeName([ "Ref" ], [ argumentType ])

    let private dictionaryType traitName argumentTypes =
        TypeName([ TraitRuntime.dictionaryTypeName traitName ], argumentTypes)

    let private unwrapIoType typeExpr =
        match typeExpr with
        | TypeName([ "IO" ], [ inner ]) -> inner
        | TypeName([ "IO" ], [ _; inner ]) -> inner
        | TypeName([ "UIO" ], [ inner ]) -> inner
        | other -> other

    let private unwrapBindPayloadType typeExpr =
        match typeExpr with
        | TypeName(_, [ inner ]) -> inner
        | other -> unwrapIoType other

    let private typeTextOf typeExpr =
        typeExpr
        |> unwrapIoType
        |> TypeSignatures.toText

    let private tryParseParameterType (parameter: Parameter) =
        parameter.TypeTokens |> Option.bind TypeSignatures.parseType

    let private tryParseReturnTypeType tokens =
        tokens |> Option.bind TypeSignatures.parseType

    let private tryParseReturnTypeTokens tokens =
        tryParseReturnTypeType tokens |> Option.map typeTextOf

    let private freshUnknownLocalType (freshCounter: int ref) =
        let name = $"__kappa_local_t{freshCounter.Value}"
        freshCounter.Value <- freshCounter.Value + 1
        TypeVariable name

    let private inferFallbackLocalType (freshCounter: int ref) expression =
        match expression with
        | Lambda(parameters, _) ->
            let resultType = freshUnknownLocalType freshCounter

            parameters
            |> List.rev
            |> List.fold
                (fun current parameter ->
                    let parameterType =
                        tryParseParameterType parameter
                        |> Option.defaultValue (freshUnknownLocalType freshCounter)

                    TypeArrow(parameter.Quantity |> Option.defaultValue QuantityOmega, parameterType, current))
                resultType
        | _ ->
            freshUnknownLocalType freshCounter

    let private tryParseLetDefinitionScheme (definition: LetDefinition) =
        match definition.ReturnTypeTokens |> Option.bind TypeSignatures.parseType with
        | Some returnType ->
            let parameterTypes =
                definition.Parameters
                |> List.map tryParseParameterType

            if parameterTypes |> List.forall Option.isSome then
                let body =
                    List.zip definition.Parameters (parameterTypes |> List.choose id)
                    |> List.rev
                    |> List.fold (fun current (parameter, parameterType) ->
                        TypeArrow(parameter.Quantity |> Option.defaultValue QuantityOmega, parameterType, current)) returnType

                Some
                    { Forall = []
                      Constraints = []
                      Body = body }
            else
                None
        | None ->
            None

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
                  Body = body
                  TerminationCertified = true
                  ConversionReducible = true })

    let private splitHeaderKindTokens (tokens: Token list) =
        let rec loop depth index =
            if index >= tokens.Length then
                tokens, []
            else
                match tokens[index].Kind with
                | LeftParen
                | LeftBracket
                | LeftBrace
                | LeftSetBrace ->
                    loop (depth + 1) (index + 1)
                | RightParen
                | RightBracket
                | RightBrace
                | RightSetBrace ->
                    loop (max 0 (depth - 1)) (index + 1)
                | Colon when depth = 0 ->
                    List.take index tokens, List.skip (index + 1) tokens
                | _ ->
                    loop depth (index + 1)

        loop 0 0

    let private findMatchingRightParen (tokens: Token list) =
        let rec loop depth index =
            if index >= tokens.Length then
                None
            else
                match tokens[index].Kind with
                | LeftParen ->
                    loop (depth + 1) (index + 1)
                | RightParen when depth = 1 ->
                    Some index
                | RightParen ->
                    loop (max 0 (depth - 1)) (index + 1)
                | _ ->
                    loop depth (index + 1)

        loop 0 0

    let private parseBinderTypeOrDefault (binderTokens: Token list) =
        let _, typeTokens = splitHeaderKindTokens binderTokens

        match significantTokens typeTokens with
        | [] -> typeObjectType
        | tokens -> TypeSignatures.parseType tokens |> Option.defaultValue typeObjectType

    let private typeFacetParameterTypes (tokens: Token list) =
        let rec loop remaining acc =
            match significantTokens remaining with
            | [] ->
                List.rev acc
            | head :: rest when Token.isName head ->
                loop rest (typeObjectType :: acc)
            | { Kind = LeftParen } :: _ as current ->
                match findMatchingRightParen current with
                | Some rightParenIndex ->
                    let binderTokens =
                        current
                        |> List.skip 1
                        |> List.take (rightParenIndex - 1)

                    let next =
                        current |> List.skip (rightParenIndex + 1)

                    loop next (parseBinderTypeOrDefault binderTokens :: acc)
                | None ->
                    List.rev acc
            | _ :: rest ->
                loop rest acc

        loop tokens []

    let private buildTypeFacetInfo moduleName name headerTokens =
        let parameterTokens, kindTokens = splitHeaderKindTokens headerTokens

        let resultType =
            match significantTokens kindTokens with
            | [] -> typeObjectType
            | tokens -> TypeSignatures.parseType tokens |> Option.defaultValue typeObjectType

        let body =
            parameterTokens
            |> typeFacetParameterTypes
            |> List.rev
            |> List.fold (fun current parameterType -> TypeArrow(QuantityOmega, parameterType, current)) resultType

        name,
        { ModuleName = moduleName
          Name = name
          Scheme =
            { Forall = []
              Constraints = []
              Body = body } }

    let private isPrivateByDefault (frontendModule: KFrontIRModule) =
        frontendModule.ModuleAttributes
        |> List.exists (fun attributeName -> String.Equals(attributeName, "PrivateByDefault", StringComparison.Ordinal))

    let private moduleHasAttribute (frontendModule: KFrontIRModule) (attributeName: string) =
        frontendModule.ModuleAttributes
        |> List.exists (fun current -> String.Equals(current, attributeName, StringComparison.Ordinal))

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

    let private itemImportsConstructorsOfType typeName (item: ImportItem) =
        item.IncludeConstructors
        && item.Namespace = Some ImportNamespace.Type
        && String.Equals(item.Name, typeName, StringComparison.Ordinal)

    let private importedItemLocalName (item: ImportItem) =
        item.Alias |> Option.defaultValue item.Name

    let private itemExportsTermName (item: ImportItem) =
        item.Namespace.IsNone || item.Namespace = Some ImportNamespace.Term

    let private itemExportsTypeName (item: ImportItem) =
        item.Namespace.IsNone || item.Namespace = Some ImportNamespace.Type

    let private itemExportsTraitName (item: ImportItem) =
        item.Namespace.IsNone || item.Namespace = Some ImportNamespace.Trait

    let private itemExportsConstructorName (item: ImportItem) =
        item.Namespace = Some ImportNamespace.Constructor

    let private tryParseConstructorScheme (dataDeclaration: DataDeclaration) (constructor: DataConstructor) =
        let significantConstructorTokens = significantTokens constructor.Tokens

        let signatureScheme =
            match significantConstructorTokens with
            | _ :: { Kind = Colon } :: typeTokens ->
                TypeSignatures.parseScheme typeTokens
            | _ ->
                None

        let parameterLayouts, defaultArguments =
            match constructor.Parameters with
            | Some parameters when parameters |> List.forall (fun parameter -> parameter.ParameterName.IsSome) ->
                let layouts =
                    parameters
                    |> List.map (fun parameter ->
                        { Name = parameter.ParameterName.Value
                          TypeTokens = Some parameter.ParameterTypeTokens
                          Quantity = parameter.ParameterQuantity
                          IsImplicit = parameter.ParameterIsImplicit
                          IsInout = false
                          IsReceiver = false })

                let defaults =
                    parameters
                    |> List.choose (fun parameter ->
                        match parameter.ParameterName, parameter.DefaultValue with
                        | Some name, Some defaultValue -> Some(name, defaultValue)
                        | _ -> None)
                    |> Map.ofList

                Some layouts, defaults
            | _ ->
                None, Map.empty

        let scheme =
            match signatureScheme with
            | Some scheme ->
                scheme
            | None ->
                let typeParameters = TypeSignatures.collectLeadingTypeParameters dataDeclaration.HeaderTokens
                let resultType = TypeName([ dataDeclaration.Name ], typeParameters |> List.map TypeVariable)

                let body =
                    constructor
                    |> TypeSignatures.constructorFieldTypes
                    |> List.rev
                    |> List.fold (fun state fieldType -> TypeArrow(QuantityOmega, fieldType, state)) resultType

                { Forall = TypeSignatures.inferredTypeForallBinders typeParameters
                  Constraints = []
                  Body = body }

        Some
            (constructor.Name,
             { ModuleName = ""
               Name = constructor.Name
               IsPattern = false
               Scheme = scheme
               TypeTokens = []
               ParameterLayouts = parameterLayouts
               ConstructorTypeName = Some dataDeclaration.Name
               DefaultArguments = defaultArguments })

    let private exceptMatches namespaceName name (item: ExceptItem) =
        String.Equals(item.Name, name, StringComparison.Ordinal)
        && (item.Namespace.IsNone || item.Namespace = Some namespaceName)

    let private selectionImportedName
        namespaceName
        (exportedNames: Set<string>)
        itemSelector
        (selection: ImportSelection)
        (name: string)
        =
        let exported = Set.contains name exportedNames

        match selection with
        | QualifiedOnly ->
            None
        | Items items ->
            if not exported then
                None
            else
                items
                |> List.tryFind (fun item ->
                    String.Equals(item.Name, name, StringComparison.Ordinal)
                    && itemSelector item)
                |> Option.map importedItemLocalName
        | All ->
            if exported then Some name else None
        | AllExcept excludedItems ->
            if exported && not (excludedItems |> List.exists (exceptMatches namespaceName name)) then
                Some name
            else
                None

    let private buildModuleSurfaceInfo (frontendModule: KFrontIRModule) =
        let moduleName = moduleNameText frontendModule.ModuleIdentity
        let privateByDefault = isPrivateByDefault frontendModule

        let typeAliases =
            frontendModule.Declarations
            |> List.choose (function
                | TypeAliasNode declaration -> tryParseTypeAliasInfo moduleName declaration
                | _ -> None)
            |> Map.ofList

        let typeFacets =
            frontendModule.Declarations
            |> List.choose (function
                | DataDeclarationNode declaration ->
                    buildTypeFacetInfo moduleName declaration.Name declaration.HeaderTokens
                    |> Some
                | TypeAliasNode declaration ->
                    buildTypeFacetInfo moduleName declaration.Name declaration.HeaderTokens
                    |> Some
                | _ ->
                    None)
            |> Map.ofList

        let recordTypes =
            frontendModule.Declarations
            |> List.choose (function
                | TypeAliasNode declaration ->
                    declaration.BodyTokens
                    |> Option.bind tryParseRecordSurfaceInfo
                    |> Option.map (fun recordInfo -> declaration.Name, recordInfo)
                | _ ->
                    None)
            |> Map.ofList

        let bindingSchemes =
            let signatureNames =
                frontendModule.Declarations
                |> List.choose (function
                    | SignatureDeclaration declaration -> Some declaration.Name
                    | _ -> None)
                |> Set.ofList

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
                        let parameterLayouts =
                            bindingDefinitions
                            |> Map.tryFind declaration.Name
                            |> Option.map (fun definition -> definition.Parameters)
                            |> mergeParameterLayouts signatureParameterLayouts
                            |> stripForallOnlyParameterLayouts scheme

                        declaration.Name,
                        { ModuleName = moduleName
                          Name = declaration.Name
                          IsPattern =
                            bindingDefinitions
                              |> Map.tryFind declaration.Name
                              |> Option.exists (fun definition -> definition.IsPattern)
                          Scheme = scheme
                          TypeTokens = declaration.TypeTokens
                          ParameterLayouts = parameterLayouts
                          ConstructorTypeName = None
                          DefaultArguments = Map.empty })
                | ExpectDeclarationNode (ExpectTermDeclaration declaration) ->
                    TypeSignatures.parseScheme declaration.TypeTokens
                    |> Option.map (fun scheme ->
                        declaration.Name,
                        { ModuleName = moduleName
                          Name = declaration.Name
                          IsPattern = false
                          Scheme = scheme
                          TypeTokens = declaration.TypeTokens
                          ParameterLayouts = None
                          ConstructorTypeName = None
                          DefaultArguments = Map.empty })
                | LetDeclaration definition
                    when definition.Name.IsSome
                         && not (Set.contains definition.Name.Value signatureNames)
                         && (definition.IsPattern || (definition.Parameters |> List.exists (fun parameter -> parameter.IsReceiver))) ->
                    tryParseLetDefinitionScheme definition
                    |> Option.map (fun scheme ->
                        let parameterLayouts =
                            Some definition.Parameters
                            |> stripForallOnlyParameterLayouts scheme

                        definition.Name.Value,
                        { ModuleName = moduleName
                          Name = definition.Name.Value
                          IsPattern = definition.IsPattern
                          Scheme = scheme
                          TypeTokens = definition.ReturnTypeTokens |> Option.defaultValue []
                          ParameterLayouts = parameterLayouts
                          ConstructorTypeName = None
                          DefaultArguments = Map.empty })
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

        let exportedConstructorsByType =
            frontendModule.Declarations
            |> List.choose (function
                | DataDeclarationNode declaration when isExportedVisibility privateByDefault declaration.Visibility ->
                    Some(
                        declaration.Name,
                        declaration.Constructors |> List.map (fun constructor -> constructor.Name) |> Set.ofList
                    )
                | _ ->
                    None)
            |> Map.ofList

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
          TypeFacets = typeFacets
          RecordTypes = recordTypes
          BindingSchemes = bindingSchemes
          Constructors = constructors
          Projections = projections
          Traits = traits
          Instances = instances
          Imports = frontendModule.Imports
          ExportedTerms = exportedTerms
          ExportedConstructors = exportedConstructors
          ExportedConstructorsByType = exportedConstructorsByType
          ExportedTypes = exportedTypes
          ExportedTraits = exportedTraits }

    let private buildSurfaceIndex (frontendModules: KFrontIRModule list) =
        let directIndex =
            frontendModules
            |> List.map (fun moduleDump ->
                moduleNameText moduleDump.ModuleIdentity, buildModuleSurfaceInfo moduleDump)
            |> Map.ofList

        let moduleGroups =
            frontendModules
            |> List.groupBy (fun moduleDump -> moduleNameText moduleDump.ModuleIdentity)
            |> Map.ofList

        let addReexportedItem (importedModule: ModuleSurfaceInfo) (moduleInfo: ModuleSurfaceInfo) (item: ImportItem) =
            let exportedName = item.Name
            let localName = importedItemLocalName item
            let exportsType = Set.contains exportedName importedModule.ExportedTypes && itemExportsTypeName item
            let exportsTerm = Set.contains exportedName importedModule.ExportedTerms && itemExportsTermName item
            let exportsTrait = Set.contains exportedName importedModule.ExportedTraits && itemExportsTraitName item
            let exportsConstructor = Set.contains exportedName importedModule.ExportedConstructors && itemExportsConstructorName item
            let exportsBundledConstructors = itemImportsConstructorsOfType exportedName item
            let exportsSameSpellingConstructor =
                item.Namespace.IsNone
                && Set.contains exportedName importedModule.ExportedTypes
                && Set.contains exportedName importedModule.ExportedConstructors

            let moduleInfo =
                { moduleInfo with
                    ExportedTerms =
                        if exportsTerm then Set.add localName moduleInfo.ExportedTerms else moduleInfo.ExportedTerms
                    ExportedTypes =
                        if exportsType then Set.add localName moduleInfo.ExportedTypes else moduleInfo.ExportedTypes
                    ExportedTraits =
                        if exportsTrait then Set.add localName moduleInfo.ExportedTraits else moduleInfo.ExportedTraits
                    ExportedConstructors =
                        moduleInfo.ExportedConstructors
                        |> fun constructors ->
                            if exportsConstructor || exportsSameSpellingConstructor then
                                Set.add localName constructors
                            else
                                constructors }

            if exportsBundledConstructors then
                let bundledConstructors =
                    importedModule.ExportedConstructorsByType
                    |> Map.tryFind exportedName
                    |> Option.defaultValue Set.empty

                { moduleInfo with
                    ExportedConstructors = Set.union moduleInfo.ExportedConstructors bundledConstructors
                    ExportedConstructorsByType = Map.add localName bundledConstructors moduleInfo.ExportedConstructorsByType }
            else
                moduleInfo

        let addWildcardReexports (importedModule: ModuleSurfaceInfo) (moduleInfo: ModuleSurfaceInfo) excludedItems =
            let notExcluded namespaceName name = not (excludedItems |> List.exists (exceptMatches namespaceName name))
            let sameSpellingConstructors =
                Set.intersect importedModule.ExportedConstructors importedModule.ExportedTypes
                |> Set.filter (notExcluded ImportNamespace.Constructor)

            { moduleInfo with
                ExportedTerms = Set.union moduleInfo.ExportedTerms (Set.filter (notExcluded ImportNamespace.Term) importedModule.ExportedTerms)
                ExportedTypes = Set.union moduleInfo.ExportedTypes (Set.filter (notExcluded ImportNamespace.Type) importedModule.ExportedTypes)
                ExportedTraits = Set.union moduleInfo.ExportedTraits (Set.filter (notExcluded ImportNamespace.Trait) importedModule.ExportedTraits)
                ExportedConstructors = Set.union moduleInfo.ExportedConstructors sameSpellingConstructors }

        let applyReexportSpec (surfaceIndex: Map<string, ModuleSurfaceInfo>) (moduleInfo: ModuleSurfaceInfo) (spec: ImportSpec) =
            match spec.Source with
            | Url _ ->
                moduleInfo
            | Dotted moduleSegments ->
                let importedModuleName = SyntaxFacts.moduleNameToText moduleSegments

                match Map.tryFind importedModuleName surfaceIndex with
                | None ->
                    moduleInfo
                | Some importedModule ->
                    match spec.Selection with
                    | QualifiedOnly ->
                        moduleInfo
                    | Items items ->
                        items |> List.fold (addReexportedItem importedModule) moduleInfo
                    | All ->
                        addWildcardReexports importedModule moduleInfo []
                    | AllExcept excludedItems ->
                        addWildcardReexports importedModule moduleInfo excludedItems

        let rec saturate surfaceIndex =
            let nextSurfaceIndex =
                moduleGroups
                |> Map.map (fun moduleName moduleDumps ->
                    let directModuleInfo = Map.find moduleName directIndex

                    moduleDumps
                    |> List.collect (fun moduleDump ->
                        moduleDump.Declarations
                        |> List.choose (function
                            | ImportDeclaration (true, specs) -> Some specs
                            | _ -> None)
                        |> List.concat)
                    |> List.fold (applyReexportSpec surfaceIndex) directModuleInfo)

            if nextSurfaceIndex = surfaceIndex then
                surfaceIndex
            else
                saturate nextSurfaceIndex

        saturate directIndex

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
                importedModule.TypeAliases.Keys |> Seq.toList,
                importedModule.TypeFacets.Keys |> Seq.toList,
                importedModule.RecordTypes.Keys |> Seq.toList))
        |> Option.defaultValue []

    let private mergeVisibleBindings (surfaceIndex: Map<string, ModuleSurfaceInfo>) moduleName =
        let importedBindings =
            importedModuleInfos surfaceIndex moduleName
            |> List.fold (fun state (spec, importedModule) ->
                importedModule.BindingSchemes
                |> Map.fold (fun current name info ->
                    match selectionImportedName ImportNamespace.Term importedModule.ExportedTerms itemImportsTermName spec.Selection name with
                    | Some localName -> Map.add localName info current
                    | None -> current) state) Map.empty

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
                    let importedName =
                        match spec.Selection with
                        | QualifiedOnly ->
                            None
                        | All
                        | AllExcept _ ->
                            None
                        | Items items ->
                            if not (Set.contains name importedModule.ExportedConstructors) then
                                None
                            else
                                items
                                |> List.tryPick (fun item ->
                                    if String.Equals(item.Name, name, StringComparison.Ordinal)
                                       && itemImportsConstructorName item then
                                        Some(importedItemLocalName item)
                                    else
                                        match info.ConstructorTypeName with
                                        | Some typeName when itemImportsConstructorsOfType typeName item ->
                                            Some name
                                        | _ ->
                                            None)

                    match importedName with
                    | Some localName -> Map.add localName info current
                    | None -> current) state) Map.empty

        let preludeConstructors =
            surfaceIndex
            |> Map.tryFind Stdlib.PreludeModuleText
            |> Option.map (fun moduleInfo ->
                moduleInfo.Constructors
                |> Map.filter (fun name _ -> List.contains name Stdlib.FixedPreludeConstructors))
            |> Option.defaultValue Map.empty

        let moduleConstructors =
            surfaceIndex
            |> Map.tryFind moduleName
            |> Option.map (fun moduleInfo -> moduleInfo.Constructors)
            |> Option.defaultValue Map.empty

        preludeConstructors
        |> Map.fold (fun state name info -> Map.add name info state) importedConstructors
        |> fun state -> moduleConstructors |> Map.fold (fun current name info -> Map.add name info current) state

    let private mergeVisibleProjections (surfaceIndex: Map<string, ModuleSurfaceInfo>) moduleName =
        let importedProjections =
            importedModuleInfos surfaceIndex moduleName
            |> List.fold (fun state (spec, importedModule) ->
                importedModule.Projections
                |> Map.fold (fun current name info ->
                    match selectionImportedName ImportNamespace.Term importedModule.ExportedTerms itemImportsTermName spec.Selection name with
                    | Some localName -> Map.add localName info current
                    | None -> current) state) Map.empty

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
                    match selectionImportedName ImportNamespace.Type importedModule.ExportedTypes itemImportsTypeName spec.Selection name with
                    | Some localName -> Map.add localName info current
                    | None -> current) state) Map.empty

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

    let private mergeVisibleTypeFacets (surfaceIndex: Map<string, ModuleSurfaceInfo>) moduleName =
        let importedFacets =
            importedModuleInfos surfaceIndex moduleName
            |> List.fold (fun state (spec, importedModule) ->
                importedModule.TypeFacets
                |> Map.fold (fun current name info ->
                    match selectionImportedName ImportNamespace.Type importedModule.ExportedTypes itemImportsTypeName spec.Selection name with
                    | Some localName -> Map.add localName info current
                    | None -> current) state) Map.empty

        let preludeFacets =
            surfaceIndex
            |> Map.tryFind Stdlib.PreludeModuleText
            |> Option.map (fun moduleInfo -> moduleInfo.TypeFacets)
            |> Option.defaultValue Map.empty

        let moduleFacets =
            surfaceIndex
            |> Map.tryFind moduleName
            |> Option.map (fun moduleInfo -> moduleInfo.TypeFacets)
            |> Option.defaultValue Map.empty

        preludeFacets
        |> Map.fold (fun state name info -> Map.add name info state) importedFacets
        |> fun state -> moduleFacets |> Map.fold (fun current name info -> Map.add name info current) state

    let private mergeVisibleModules (surfaceIndex: Map<string, ModuleSurfaceInfo>) moduleName =
        let importedModules =
            surfaceIndex
            |> Map.tryFind moduleName
            |> Option.map (fun moduleInfo ->
                moduleInfo.Imports
                |> List.choose (fun spec ->
                    match spec.Source, spec.Alias, spec.Selection with
                    | Dotted _, Some alias, QualifiedOnly
                    | Url _, Some alias, QualifiedOnly ->
                        Some alias
                    | Dotted moduleSegments, None, QualifiedOnly ->
                        let importedModuleName = SyntaxFacts.moduleNameToText moduleSegments

                        if surfaceIndex.ContainsKey importedModuleName then
                            Some importedModuleName
                        else
                            None
                    | _ ->
                        None))
            |> Option.defaultValue []
            |> Set.ofList

        Set.add moduleName importedModules

    let private mergeVisibleRecordTypes (surfaceIndex: Map<string, ModuleSurfaceInfo>) moduleName =
        let importedRecordTypes =
            importedModuleInfos surfaceIndex moduleName
            |> List.fold (fun state (spec, importedModule) ->
                importedModule.RecordTypes
                |> Map.fold (fun current name info ->
                    match selectionImportedName ImportNamespace.Type importedModule.ExportedTypes itemImportsTypeName spec.Selection name with
                    | Some localName -> Map.add localName info current
                    | None -> current) state) Map.empty

        let moduleRecordTypes =
            surfaceIndex
            |> Map.tryFind moduleName
            |> Option.map (fun moduleInfo -> moduleInfo.RecordTypes)
            |> Option.defaultValue Map.empty

        moduleRecordTypes
        |> Map.fold (fun state name info -> Map.add name info state) importedRecordTypes

    let private mergeVisibleTraits (surfaceIndex: Map<string, ModuleSurfaceInfo>) moduleName =
        let importedTraits =
            importedModuleInfos surfaceIndex moduleName
            |> List.fold (fun state (spec, importedModule) ->
                importedModule.Traits
                |> Map.fold (fun current name info ->
                    match selectionImportedName ImportNamespace.Trait importedModule.ExportedTraits itemImportsTraitName spec.Selection name with
                    | Some localName -> Map.add localName info current
                    | None -> current) state) Map.empty

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
                    selectionImportedName ImportNamespace.Trait importedModule.ExportedTraits itemImportsTraitName spec.Selection instanceInfo.TraitName
                    |> Option.isSome))

        let moduleInstances =
            surfaceIndex
            |> Map.tryFind moduleName
            |> Option.map (fun moduleInfo -> moduleInfo.Instances)
            |> Option.defaultValue []

        importedInstances @ moduleInstances

    let private buildStaticObjectAliasesForModule
        (environment: BindingLoweringEnvironment)
        (declarations: TopLevelDeclaration list)
        =
        let aliasKey segments = SyntaxFacts.moduleNameToText segments

        let transparentSealFields ascriptionTokens =
            match tryParseRecordSurfaceInfo ascriptionTokens with
            | Some recordInfo ->
                recordInfo.Fields
                |> List.filter (fun field -> not field.IsOpaque)
                |> List.map (fun field -> field.Name)
                |> Set.ofList
                |> Some
            | None ->
                match significantTokens ascriptionTokens with
                | [ nameToken ] when Token.isName nameToken ->
                    environment.VisibleRecordTypes
                    |> Map.tryFind (SyntaxFacts.trimIdentifierQuotes nameToken.Text)
                    |> Option.map (fun recordInfo ->
                        recordInfo.Fields
                        |> List.filter (fun field -> not field.IsOpaque)
                        |> List.map (fun field -> field.Name)
                        |> Set.ofList)
                | _ ->
                    None

        let copyNestedAliasesFiltered
            (sourcePrefix: string list)
            (targetPrefix: string list)
            (fieldFilter: string -> bool)
            (aliases: Map<string, StaticObjectInfo>)
            =
            let sourceKey = aliasKey sourcePrefix
            let sourceNestedPrefix = sourceKey + "."

            aliases
            |> Map.toList
            |> List.filter (fun ((key: string), _) ->
                if key.StartsWith(sourceNestedPrefix, StringComparison.Ordinal) then
                    let suffix = key.Substring(sourceNestedPrefix.Length)
                    let firstSegment = suffix.Split('.')[0]

                    fieldFilter firstSegment
                else
                    false)
            |> List.fold (fun (current: Map<string, StaticObjectInfo>) ((key: string), staticObject) ->
                let suffix = key.Substring(sourceNestedPrefix.Length)
                Map.add (aliasKey targetPrefix + "." + suffix) staticObject current) aliases

        let copyNestedAliases sourcePrefix targetPrefix aliases =
            copyNestedAliasesFiltered sourcePrefix targetPrefix (fun _ -> true) aliases

        let rec collectAliases
            (environment: BindingLoweringEnvironment)
            (prefix: string list)
            (expression: SurfaceExpression)
            (aliases: Map<string, StaticObjectInfo>)
            =
            let aliasEnvironment =
                { environment with
                    VisibleStaticObjects = aliases }

            match tryResolveStaticObject aliasEnvironment expression with
            | Some staticObject ->
                let aliases = Map.add (aliasKey prefix) staticObject aliases

                match expression with
                | Name sourcePrefix -> copyNestedAliases sourcePrefix prefix aliases
                | _ -> aliases
            | None ->
                match expression with
                | RecordLiteral fields ->
                    fields
                    |> List.fold (fun current field -> collectAliases environment (prefix @ [ field.Name ]) field.Value current) aliases
                | Seal(value, ascriptionTokens) ->
                    match value, transparentSealFields ascriptionTokens with
                    | Name sourcePrefix, Some transparentFields ->
                        copyNestedAliasesFiltered sourcePrefix prefix (fun fieldName -> Set.contains fieldName transparentFields) aliases
                    | RecordLiteral fields, Some transparentFields ->
                        fields
                        |> List.filter (fun field -> Set.contains field.Name transparentFields)
                        |> List.fold (fun current field -> collectAliases environment (prefix @ [ field.Name ]) field.Value current) aliases
                    | _, Some _ ->
                        aliases
                    | _ ->
                        collectAliases environment prefix value aliases
                | _ ->
                    aliases

        declarations
        |> List.fold (fun aliases declaration ->
            match declaration with
            | LetDeclaration definition when definition.Name.IsSome && definition.Body.IsSome ->
                collectAliases environment [ definition.Name.Value ] definition.Body.Value aliases
            | _ ->
                aliases) Map.empty

    let private normalizeTypeAliases (aliases: Map<string, TypeAliasInfo>) =
        let rec loop visited typeExpr =
            match typeExpr with
            | TypeLevelLiteral _ ->
                typeExpr
            | TypeUniverse None ->
                typeExpr
            | TypeUniverse(Some universeExpr) ->
                TypeUniverse(Some(loop visited universeExpr))
            | TypeIntrinsic _ ->
                typeExpr
            | TypeApply(callee, arguments) ->
                TypeApply(loop visited callee, arguments |> List.map (loop visited))
            | TypeLambda(parameterName, parameterSort, body) ->
                TypeLambda(parameterName, loop visited parameterSort, loop visited body)
            | TypeDelay inner ->
                TypeDelay(loop visited inner)
            | TypeMemo inner ->
                TypeMemo(loop visited inner)
            | TypeForce inner ->
                TypeForce(loop visited inner)
            | TypeProject(target, fieldName) ->
                TypeProject(loop visited target, fieldName)
            | TypeVariable _ ->
                typeExpr
            | TypeArrow(quantity, parameterType, resultType) ->
                TypeArrow(quantity, loop visited parameterType, loop visited resultType)
            | TypeEquality(left, right) ->
                TypeEquality(loop visited left, loop visited right)
            | TypeCapture(inner, captures) ->
                TypeCapture(loop visited inner, captures)
            | TypeEffectRow(entries, tail) ->
                TypeEffectRow(
                    entries
                    |> List.map (fun entry ->
                        { Label = loop visited entry.Label
                          Effect = loop visited entry.Effect }),
                    tail |> Option.map (loop visited)
                )
            | TypeRecord fields ->
                TypeRecord(
                    fields
                    |> List.map (fun field ->
                        { field with
                            Type = loop visited field.Type })
                )
            | TypeUnion members ->
                TypeUnion(members |> List.map (loop visited))
            | TypeName([ name ], arguments) ->
                let normalizedArguments = arguments |> List.map (loop visited)

                match aliases |> Map.tryFind name with
                | Some aliasInfo
                    when not (Set.contains name visited)
                         && aliasInfo.ConversionReducible
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
        | TypeLevelLiteral _ ->
            typeExpr
        | TypeUniverse None ->
            typeExpr
        | TypeUniverse(Some universeExpr) ->
            TypeUniverse(Some(eraseSyntheticRecordTypeExpr universeExpr))
        | TypeIntrinsic _ ->
            typeExpr
        | TypeApply(callee, arguments) ->
            TypeApply(eraseSyntheticRecordTypeExpr callee, arguments |> List.map eraseSyntheticRecordTypeExpr)
        | TypeLambda(parameterName, parameterSort, body) ->
            TypeLambda(parameterName, eraseSyntheticRecordTypeExpr parameterSort, eraseSyntheticRecordTypeExpr body)
        | TypeDelay inner ->
            TypeDelay(eraseSyntheticRecordTypeExpr inner)
        | TypeMemo inner ->
            TypeMemo(eraseSyntheticRecordTypeExpr inner)
        | TypeForce inner ->
            TypeForce(eraseSyntheticRecordTypeExpr inner)
        | TypeProject(target, fieldName) ->
            TypeProject(eraseSyntheticRecordTypeExpr target, fieldName)
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
        | TypeEffectRow(entries, tail) ->
            TypeEffectRow(
                entries
                |> List.map (fun entry ->
                    { Label = eraseSyntheticRecordTypeExpr entry.Label
                      Effect = eraseSyntheticRecordTypeExpr entry.Effect }),
                tail |> Option.map eraseSyntheticRecordTypeExpr
            )
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
        | TypeUnion members ->
            TypeUnion(members |> List.map eraseSyntheticRecordTypeExpr)

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
        | TypeUniverse _
        | TypeIntrinsic UniverseClassifier
        | TypeIntrinsic ConstraintClassifier
        | TypeIntrinsic QuantityClassifier
        | TypeIntrinsic RegionClassifier
        | TypeIntrinsic RecRowClassifier
        | TypeIntrinsic VarRowClassifier
        | TypeIntrinsic EffRowClassifier
        | TypeIntrinsic LabelClassifier
        | TypeIntrinsic EffLabelClassifier
        | TypeName([ "Type" ], [])
        | TypeName([ "Constraint" ], [])
        | TypeName([ "Quantity" ], [])
        | TypeName([ "Region" ], [])
        | TypeName([ "RecRow" ], [])
        | TypeName([ "VarRow" ], [])
        | TypeName([ "EffRow" ], [])
        | TypeName([ "Label" ], [])
        | TypeName([ "EffLabel" ], []) ->
            true
        | _ ->
            false

    let private isReifiedStaticObjectType
        (aliases: Map<string, TypeAliasInfo>)
        typeExpr
        =
        let rec loop current =
            match normalizeTypeAliases aliases current with
            | TypeUniverse _
            | TypeIntrinsic UniverseClassifier
            | TypeIntrinsic ConstraintClassifier
            | TypeIntrinsic QuantityClassifier
            | TypeIntrinsic RegionClassifier
            | TypeIntrinsic RecRowClassifier
            | TypeIntrinsic VarRowClassifier
            | TypeIntrinsic EffRowClassifier
            | TypeIntrinsic LabelClassifier
            | TypeIntrinsic EffLabelClassifier
            | TypeName([ "Type" ], [])
            | TypeName([ "Constraint" ], [])
            | TypeName([ "Quantity" ], [])
            | TypeName([ "Region" ], [])
            | TypeName([ "RecRow" ], [])
            | TypeName([ "VarRow" ], [])
            | TypeName([ "EffRow" ], [])
            | TypeName([ "Label" ], [])
            | TypeName([ "EffLabel" ], []) ->
                true
            | TypeArrow(_, _, resultType) ->
                loop resultType
            | _ ->
                false

        loop typeExpr

    let private tryUnifyVisibleTypes
        (aliases: Map<string, TypeAliasInfo>)
        (pairs: (TypeExpr * TypeExpr) list)
        =
        let normalize = normalizeTypeAliases aliases
        pairs
        |> List.map (fun (left, right) -> normalize left, normalize right)
        |> TypeSignatures.tryUnifyMany

    let private qualifyVisibleTypeNames
        (environment: BindingLoweringEnvironment)
        typeExpr
        =
        let rec loop current =
            match normalizeTypeAliases environment.VisibleTypeAliases current with
            | TypeLevelLiteral _ ->
                current
            | TypeUniverse None ->
                current
            | TypeUniverse(Some universeExpr) ->
                TypeUniverse(Some(loop universeExpr))
            | TypeIntrinsic _ ->
                current
            | TypeApply(callee, arguments) ->
                TypeApply(loop callee, arguments |> List.map loop)
            | TypeLambda(parameterName, parameterSort, body) ->
                TypeLambda(parameterName, loop parameterSort, loop body)
            | TypeDelay inner ->
                TypeDelay(loop inner)
            | TypeMemo inner ->
                TypeMemo(loop inner)
            | TypeForce inner ->
                TypeForce(loop inner)
            | TypeProject(target, fieldName) ->
                TypeProject(loop target, fieldName)
            | TypeName([ name ], arguments) when environment.VisibleTypeFacets.ContainsKey(name) ->
                let typeFacet = environment.VisibleTypeFacets[name]
                let qualifiedName =
                    (typeFacet.ModuleName.Split('.', StringSplitOptions.RemoveEmptyEntries) |> Array.toList) @ [ typeFacet.Name ]

                TypeName(qualifiedName, arguments |> List.map loop)
            | TypeName(name, arguments) ->
                TypeName(name, arguments |> List.map loop)
            | TypeArrow(quantity, parameterType, resultType) ->
                TypeArrow(quantity, loop parameterType, loop resultType)
            | TypeEquality(left, right) ->
                TypeEquality(loop left, loop right)
            | TypeCapture(inner, captures) ->
                TypeCapture(loop inner, captures)
            | TypeEffectRow(entries, tail) ->
                TypeEffectRow(
                    entries
                    |> List.map (fun entry ->
                        { Label = loop entry.Label
                          Effect = loop entry.Effect }),
                    tail |> Option.map loop
                )
            | TypeRecord fields ->
                TypeRecord(
                    fields
                    |> List.map (fun field ->
                        { field with
                            Type = loop field.Type })
                )
            | TypeUnion members ->
                TypeUnion(members |> List.map loop)
            | TypeVariable _ ->
                current

        loop typeExpr

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
            | AsPattern(name, inner) ->
                let locals =
                    match expected with
                    | Some expectedType -> Map.add name expectedType locals
                    | None -> locals

                loop locals expected inner
            | TypedPattern(inner, typeTokens) ->
                let ascribedType =
                    TypeSignatures.parseType typeTokens
                    |> Option.map (qualifyVisibleTypeNames environment)
                    |> Option.orElse expected

                loop locals ascribedType inner
            | TuplePattern elements ->
                let tupleFields =
                    elements
                    |> List.mapi (fun index element ->
                        { Name = $"_{index + 1}"
                          IsImplicit = false
                          Pattern = element })

                loop locals expected (AnonymousRecordPattern(tupleFields, None))
            | AnonymousRecordPattern(fields, rest) ->
                match expected |> Option.bind (tryCanonicalRecordType environment.VisibleTypeAliases) with
                | Some(TypeRecord recordFields) ->
                    let afterFields =
                        fields
                        |> List.fold (fun state field ->
                            let fieldType =
                                recordFields
                                |> List.tryFind (fun recordField -> String.Equals(recordField.Name, field.Name, StringComparison.Ordinal))
                                |> Option.map (fun recordField -> recordField.Type)

                            loop state fieldType field.Pattern) locals

                    match rest with
                    | Some(BindRecordPatternRest name) ->
                        let mentioned =
                            fields |> List.map (fun field -> field.Name) |> Set.ofList

                        let residualFields =
                            recordFields
                            |> List.filter (fun field -> not (Set.contains field.Name mentioned))

                        Map.add name (TypeRecord residualFields) afterFields
                    | _ ->
                        afterFields
                | _ ->
                    let afterFields = fields |> List.fold (fun state field -> loop state None field.Pattern) locals

                    match rest with
                    | Some(BindRecordPatternRest name) ->
                        Map.add name (TypeRecord []) afterFields
                    | _ ->
                        afterFields
            | ConstructorPattern(nameSegments, arguments) ->
                let argumentTypes =
                    match expected, tryResolveVisibleConstructorInfo environment nameSegments with
                    | Some expectedResultType, Some constructorInfo ->
                        let instantiated = TypeSignatures.instantiate "t" freshCounter.Value constructorInfo.Scheme
                        freshCounter.Value <- freshCounter.Value + instantiated.Forall.Length

                        let parameterTypes, constructorResultType = TypeSignatures.schemeParts instantiated

                        if List.length parameterTypes = List.length arguments then
                            match
                                TypeSignatures.tryUnifyMany
                                    [
                                        qualifyVisibleTypeNames environment constructorResultType,
                                        qualifyVisibleTypeNames environment expectedResultType
                                    ]
                            with
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
            | NamedConstructorPattern(nameSegments, fields) ->
                let argumentTypes =
                    match expected, tryResolveVisibleConstructorInfo environment nameSegments with
                    | Some expectedResultType, Some constructorInfo ->
                        let instantiated = TypeSignatures.instantiate "t" freshCounter.Value constructorInfo.Scheme
                        freshCounter.Value <- freshCounter.Value + instantiated.Forall.Length

                        let parameterTypes, constructorResultType = TypeSignatures.schemeParts instantiated

                        let fieldTypeMap =
                            constructorInfo.ParameterLayouts
                            |> Option.map (fun layouts ->
                                layouts
                                |> List.filter (fun parameter -> not parameter.IsImplicit)
                                |> List.map (fun parameter -> parameter.Name)
                                |> List.zip parameterTypes
                                |> List.map (fun (fieldType, fieldName) -> fieldName, fieldType)
                                |> Map.ofList)
                            |> Option.defaultValue Map.empty

                        match
                            TypeSignatures.tryUnifyMany
                                [
                                    qualifyVisibleTypeNames environment constructorResultType,
                                    qualifyVisibleTypeNames environment expectedResultType
                                ]
                        with
                        | Some substitution ->
                            fieldTypeMap
                            |> Map.map (fun _ fieldType -> TypeSignatures.applySubstitution substitution fieldType)
                        | None ->
                            Map.empty
                    | _ ->
                        Map.empty

                fields
                |> List.fold (fun state field ->
                    let fieldType = argumentTypes |> Map.tryFind field.Name
                    loop state fieldType field.Pattern) locals
            | VariantPattern(BoundVariantPattern(name, Some typeTokens)) ->
                match TypeSignatures.parseType typeTokens with
                | Some parsedType ->
                    Map.add name (qualifyVisibleTypeNames environment parsedType) locals
                | None ->
                    locals
            | VariantPattern(BoundVariantPattern(name, None))
            | VariantPattern(RestVariantPattern name) ->
                match expected with
                | Some expectedType -> Map.add name expectedType locals
                | None -> locals
            | VariantPattern(WildcardVariantPattern _) ->
                locals
            | OrPattern alternatives ->
                match alternatives with
                | first :: _ ->
                    loop locals expected first
                | [] ->
                    locals

        loop localTypes expectedType pattern

    let private extendBindingLocalTypes
        (environment: BindingLoweringEnvironment)
        (freshCounter: int ref)
        (localTypes: Map<string, TypeExpr>)
        (binding: SurfaceBindPattern)
        expectedType
        =
        extendPatternLocalTypes environment freshCounter localTypes expectedType binding.Pattern

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

    let private inferSurfaceNumericLiteralType literal =
        match literal with
        | SurfaceIntegerLiteral _ -> intType
        | SurfaceRealLiteral _ -> floatType

    let private lowerBindingReturnTypeExpr
        (scheme: TypeScheme option)
        (parameters: Parameter list)
        (fallbackTokens: Token list option)
        =
        match tryParseReturnTypeType fallbackTokens with
        | Some explicitReturnType ->
            Some explicitReturnType
        | None ->
            match scheme with
            | Some scheme ->
                let resultType =
                    if List.isEmpty parameters then
                        scheme.Body
                    else
                        let _, resultType = TypeSignatures.schemeParts scheme
                        resultType

                Some resultType
            | None ->
                None

    let private lowerBindingReturnType
        (scheme: TypeScheme option)
        (parameters: Parameter list)
        (fallbackTokens: Token list option)
        =
        lowerBindingReturnTypeExpr scheme parameters fallbackTokens
        |> Option.map typeTextOf

    let private buildBindingParameters
        (_aliases: Map<string, TypeAliasInfo>)
        (scheme: TypeScheme option)
        (parameters: Parameter list)
        =
        let coreParameters scheme =
            parameters
            |> List.filter (fun parameter ->
                not (
                    parameter.IsImplicit
                    && parameter.TypeTokens.IsNone
                    && (scheme.Forall |> List.exists (fun binder -> binder.Name = parameter.Name))
                ))

        match scheme with
        | Some scheme ->
            let parameters = coreParameters scheme
            let parameterTypes, _ = TypeSignatures.schemeParts scheme

            if List.length parameterTypes = List.length parameters then
                List.zip parameters parameterTypes
                |> List.map (fun (parameter, parameterType) ->
                    lowerKCoreParameter parameter (Some parameterType))
            else
                parameters |> List.map (fun parameter -> lowerKCoreParameter parameter (parameter.TypeTokens |> Option.bind TypeSignatures.parseType))
        | None ->
            parameters |> List.map (fun parameter -> lowerKCoreParameter parameter (parameter.TypeTokens |> Option.bind TypeSignatures.parseType))

    let private buildLocalTypes
        (scheme: TypeScheme option)
        (parameters: Parameter list)
        =
        let fromScheme =
            match scheme with
            | Some scheme ->
                let parameters =
                    parameters
                    |> List.filter (fun parameter ->
                        not (
                            parameter.IsImplicit
                            && parameter.TypeTokens.IsNone
                            && (scheme.Forall |> List.exists (fun binder -> binder.Name = parameter.Name))
                        ))

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

    let private tryInterpolatedMacroResultType
        (aliases: Map<string, TypeAliasInfo>)
        (typeExpr: TypeExpr)
        =
        let normalize = normalizeTypeAliases aliases

        match normalize typeExpr with
        | TypeName(([ "Dict" ] | [ "std"; "prelude"; "Dict" ]), [ dictionaryConstraint ]) ->
            match normalize dictionaryConstraint with
            | TypeName(([ "InterpolatedMacro" ] | [ "std"; "prelude"; "InterpolatedMacro" ]), [ resultType ]) ->
                Some resultType
            | _ ->
                None
        | TypeName([ dictionaryTypeName ], [ resultType ])
            when String.Equals(dictionaryTypeName, TraitRuntime.dictionaryTypeName "InterpolatedMacro", StringComparison.Ordinal) ->
            Some resultType
        | _ ->
            None

    let private tryInferPrefixedStringMacroResultType
        (environment: BindingLoweringEnvironment)
        (freshCounter: int ref)
        (localTypes: Map<string, TypeExpr>)
        prefix
        =
        localTypes
        |> Map.tryFind prefix
        |> Option.orElseWith (fun () -> instantiateVisibleBindingResultType environment freshCounter prefix)
        |> Option.bind (tryInterpolatedMacroResultType environment.VisibleTypeAliases)

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
        | ExplicitImplicitArgument inner ->
            tryParseTypeLikeSurfaceExpression inner
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

    let private tryReceiverProjection
        (environment: BindingLoweringEnvironment)
        (localTypes: Map<string, TypeExpr>)
        receiverName
        memberName
        =
        let receiverType =
            localTypes
            |> Map.tryFind receiverName
            |> Option.orElseWith (fun () ->
                environment.VisibleBindings
                |> Map.tryFind receiverName
                |> Option.map (fun bindingInfo -> snd (TypeSignatures.schemeParts bindingInfo.Scheme)))
            |> Option.map (normalizeTypeAliases environment.VisibleTypeAliases)

        let binderMatchesReceiverType (binder: ProjectionPlaceBinder) =
            match receiverType, TypeSignatures.parseType binder.TypeTokens with
            | Some actualType, Some expectedType ->
                TypeSignatures.definitionallyEqual
                    actualType
                    (normalizeTypeAliases environment.VisibleTypeAliases expectedType)
            | _ ->
                true

        environment.VisibleProjections
        |> Map.tryFind memberName
        |> Option.bind (fun projectionInfo ->
            let placeBinders =
                projectionInfo.Binders
                |> List.choose (function
                    | ProjectionPlaceBinder binder -> Some binder
                    | ProjectionValueBinder _ -> None)

            match placeBinders with
            | [ binder ] when binder.IsReceiver && binderMatchesReceiverType binder ->
                Some projectionInfo
            | _ ->
                None)

    let private tryBuildReceiverMethodArguments
        (bindingInfo: BindingSchemeInfo)
        receiverExpression
        arguments
        =
        bindingInfo.ParameterLayouts
        |> Option.bind (fun layouts ->
            let receiverParameters =
                layouts
                |> List.mapi (fun index layout -> index, layout)
                |> List.filter (fun (_, layout) -> layout.IsReceiver)

            match receiverParameters with
            | [ receiverIndex, _ ] ->
                let precedingExplicitCount =
                    layouts
                    |> List.take receiverIndex
                    |> List.filter (fun layout -> not layout.IsImplicit)
                    |> List.length

                if List.length arguments >= precedingExplicitCount then
                    let precedingArguments = arguments |> List.take precedingExplicitCount
                    let followingArguments = arguments |> List.skip precedingExplicitCount
                    Some(precedingArguments @ [ receiverExpression ] @ followingArguments)
                else
                    None
            | _ ->
                None)

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

    let private matchesBuiltinNumericTarget
        (aliases: Map<string, TypeAliasInfo>)
        literal
        expectedType
        =
        let isIntegerLiteral =
            match literal with
            | SurfaceIntegerLiteral _ -> true
            | _ -> false

        let isRealLiteral =
            match literal with
            | SurfaceRealLiteral _ -> true
            | _ -> false

        match normalizeTypeAliases aliases expectedType with
        | TypeName([ "Int" ], [])
        | TypeName([ "Integer" ], [])
        | TypeName([ "Nat" ], [])
            when isIntegerLiteral ->
            true
        | TypeName([ "Float" ], [])
        | TypeName([ "Double" ], [])
        | TypeName([ "Real" ], [])
            when isRealLiteral ->
            true
        | _ ->
            false

    let private numericLiteralConstraintName literal =
        match literal with
        | SurfaceIntegerLiteral _ -> "FromInteger"
        | SurfaceRealLiteral _ -> "FromFloat"

    let private tryResolveNumericStaticObjectTarget
        (environment: BindingLoweringEnvironment)
        expectedType
        =
        let rec loop visited current =
            let normalizedCurrent =
                normalizeTypeAliases environment.VisibleTypeAliases current

            let candidateKey = TypeSignatures.toText normalizedCurrent

            if Set.contains candidateKey visited then
                None
            else
                let visited = Set.add candidateKey visited

                match normalizedCurrent with
                | TypeName(nameSegments, []) ->
                    let aliasName = SyntaxFacts.moduleNameToText nameSegments

                    environment.VisibleStaticObjects
                    |> Map.tryFind aliasName
                    |> Option.bind (fun staticObject ->
                        match staticObject.ObjectKind with
                        | StaticTypeObject ->
                            loop visited (TypeName(staticObject.NameSegments, []))
                        | _ ->
                            None)
                    |> Option.orElseWith (fun () ->
                        if Set.isEmpty visited then
                            None
                        else
                            Some normalizedCurrent)
                | _ ->
                    if Set.isEmpty visited then
                        None
                    else
                        Some normalizedCurrent

        loop Set.empty expectedType

    let private tryResolveNumericLiteralContext
        (environment: BindingLoweringEnvironment)
        expectedType
        literal
        =
        let candidateTypes =
            expectedType
            :: (tryResolveNumericStaticObjectTarget environment expectedType |> Option.toList)

        let tryResolveAgainstTarget targetType =
            let normalizedTargetType =
                normalizeTypeAliases environment.VisibleTypeAliases targetType

            if isCompileTimeArgumentType environment.VisibleTypeAliases normalizedTargetType then
                None
            elif matchesBuiltinNumericTarget environment.VisibleTypeAliases literal normalizedTargetType then
                Some(expectedType, None)
            else
                let constraintInfo =
                    { TraitName = numericLiteralConstraintName literal
                      Arguments = [ normalizedTargetType ] }

                resolveConstraintInstance environment constraintInfo
                |> Option.map (fun instanceInfo -> expectedType, Some instanceInfo)

        candidateTypes
        |> List.tryPick tryResolveAgainstTarget

    let private tryInferNumericLiteralTypeFromContext
        (environment: BindingLoweringEnvironment)
        expectedType
        literal
        =
        tryResolveNumericLiteralContext environment expectedType literal
        |> Option.map fst

    let private tryInferNumericExpressionTypeFromContext
        (environment: BindingLoweringEnvironment)
        expectedType
        expression
        =
        match expression with
        | NumericLiteral literal ->
            tryInferNumericLiteralTypeFromContext environment expectedType literal
        | Unary("-", NumericLiteral literal) ->
            tryInferNumericLiteralTypeFromContext environment expectedType literal
        | _ ->
            None

    let private tryLowerNumericLiteralForRuntime literal =
        match literal with
        | SurfaceIntegerLiteral(value, _, _) when value >= BigInteger(Int64.MinValue) && value <= BigInteger(Int64.MaxValue) ->
            Some(KCoreLiteral(LiteralValue.Integer(int64 value)))
        | SurfaceRealLiteral(_, text, _) ->
            match Double.TryParse(text, Globalization.NumberStyles.Float, Globalization.CultureInfo.InvariantCulture) with
            | true, value ->
                Some(KCoreLiteral(LiteralValue.Float value))
            | _ ->
                None
        | _ ->
            None

    let private tryLowerNumericExpressionForRuntime expression =
        match expression with
        | NumericLiteral literal ->
            tryLowerNumericLiteralForRuntime literal
        | Unary("-", NumericLiteral(SurfaceIntegerLiteral(value, text, suffix))) ->
            tryLowerNumericLiteralForRuntime (SurfaceIntegerLiteral(-value, "-" + text, suffix))
        | Unary("-", NumericLiteral(SurfaceRealLiteral(decimalValue, text, suffix))) ->
            tryLowerNumericLiteralForRuntime (SurfaceRealLiteral(decimalValue |> Option.map (~-), "-" + text, suffix))
        | _ ->
            None

    let private trySynthesizeImplicitArgument
        (environment: BindingLoweringEnvironment)
        (tryResolveLocalImplicit: TypeExpr -> KCoreExpression option)
        parameterType
        =
        match tryResolveLocalImplicit parameterType with
        | Some implicitArgument ->
            Some implicitArgument
        | None ->
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

    let private noLocalImplicit (_: TypeExpr) : KCoreExpression option =
        None

    let private tryResolveUniqueLocalImplicitByType
        (environment: BindingLoweringEnvironment)
        (localTypes: Map<string, TypeExpr>)
        parameterType
        =
        let normalizedGoal =
            normalizeTypeAliases environment.VisibleTypeAliases parameterType

        let matches =
            localTypes
            |> Map.toList
            |> List.filter (fun (_, localType) ->
                TypeSignatures.definitionallyEqual
                    (normalizeTypeAliases environment.VisibleTypeAliases localType)
                    normalizedGoal)

        match matches with
        | [ name, _ ] ->
            Some(KCoreName [ name ])
        | _ ->
            None

    let private tryPrepareVisibleBindingCall
        (environment: BindingLoweringEnvironment)
        (freshCounter: int ref)
        (bindingInfo: BindingSchemeInfo)
        (inferArgumentType: SurfaceExpression -> TypeExpr option)
        (tryResolveLocalImplicit: TypeExpr -> KCoreExpression option)
        (arguments: SurfaceExpression list)
        =
        let inferArgumentTypeForParameter parameterType argument =
            tryInferNumericExpressionTypeFromContext environment parameterType argument
            |> Option.orElseWith (fun () -> inferArgumentType argument)

        let instantiated = TypeSignatures.instantiate "t" freshCounter.Value bindingInfo.Scheme
        freshCounter.Value <- freshCounter.Value + instantiated.Forall.Length

        let rec consumeExplicitTypeApplications
            (remainingForall: TypeSignatures.ForallBinder list)
            (remainingArguments: SurfaceExpression list)
            substitution
            =
            match remainingForall, remainingArguments with
            | binder :: restForall, ExplicitImplicitArgument explicitArgument :: restArguments ->
                match tryParseTypeLikeSurfaceExpression explicitArgument with
                | Some explicitTypeArgument ->
                    consumeExplicitTypeApplications
                        restForall
                        restArguments
                        (composeTypeSubstitution substitution (Map.ofList [ binder.Name, explicitTypeArgument ]))
                | None ->
                    None
            | _, _ ->
                Some(substitution, remainingArguments)

        let explicitTypeApplicationsValid, instantiated, arguments =
            match consumeExplicitTypeApplications instantiated.Forall arguments Map.empty with
            | Some(substitution, remainingArguments) ->
                true,
                TypeSignatures.applySchemeSubstitution substitution instantiated,
                remainingArguments
            | None ->
                false,
                instantiated,
                arguments

        let hasApplicationOnlyArgument arguments =
            arguments
            |> List.exists (function
                | ExplicitImplicitArgument _
                | NamedApplicationBlock _ -> true
                | _ -> false)

        let hasDefaultArgument (parameterLayout: Parameter) =
            bindingInfo.DefaultArguments.ContainsKey(parameterLayout.Name)

        match bindingInfo.ParameterLayouts with
        | None ->
            if (not explicitTypeApplicationsValid) || hasApplicationOnlyArgument arguments then
                None
            else
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
            if not explicitTypeApplicationsValid then
                None
            else
                let schemeParameterTypes, resultType = TypeSignatures.schemeParts instantiated

                let alignParameterTypesWithLayouts layouts parameterTypes =
                    let remaining = ref parameterTypes
                    let aligned = ResizeArray<TypeExpr>()
                    let mutable success = true

                    for layout in layouts do
                        match tryParseParameterType layout with
                        | Some layoutType when isCompileTimeArgumentType environment.VisibleTypeAliases layoutType ->
                            aligned.Add(layoutType)

                            match !remaining with
                            | parameterType :: rest
                                when TypeSignatures.definitionallyEqual
                                         (normalizeTypeAliases environment.VisibleTypeAliases layoutType)
                                         (normalizeTypeAliases environment.VisibleTypeAliases parameterType) ->
                                remaining := rest
                            | _ ->
                                ()
                        | _ ->
                            match !remaining with
                            | parameterType :: rest ->
                                aligned.Add(parameterType)
                                remaining := rest
                            | [] ->
                                success <- false

                    if success then
                        Some(List.ofSeq aligned, List.ofSeq !remaining)
                    else
                        None

                match alignParameterTypesWithLayouts parameterLayouts schemeParameterTypes with
                | None ->
                    None
                | Some(parameterTypes, trailingParameterTypes) ->
                    let splitNamedBlock (arguments: SurfaceExpression list) =
                        let indexedNamedBlocks =
                            arguments
                            |> List.mapi (fun index argument ->
                                match argument with
                                | NamedApplicationBlock fields -> Some(index, fields)
                                | _ -> None)
                            |> List.choose id

                        match indexedNamedBlocks with
                        | [] ->
                            Some(arguments, None)
                        | [ index, fields ] when index = List.length arguments - 1 ->
                            Some(arguments |> List.take index, Some fields)
                        | _ ->
                            None

                    match splitNamedBlock arguments with
                    | None ->
                        None
                    | Some(positionalArguments, namedFields) ->
                        let mutable remainingArguments = positionalArguments
                        let mutable substitution = Map.empty<string, TypeExpr>
                        let preparedArguments = ResizeArray<PreparedCallArgument>()
                        let preparedParameters = ResizeArray<PreparedCallParameter>()
                        let remainingParameters = ResizeArray<Parameter * TypeExpr>()
                        let mutable usedNamedFields = Set.empty<string>
                        let mutable success = true
                        let mutable stoppedAtPartialApplication = false

                        let namedFieldCounts =
                            namedFields
                            |> Option.map (fun fields -> fields |> List.countBy (fun field -> field.Name) |> Map.ofList)
                            |> Option.defaultValue Map.empty

                        let tryTakeNamedArgument (parameterLayout: Parameter) =
                            namedFields
                            |> Option.bind (fun fields ->
                                fields
                                |> List.tryFind (fun field -> String.Equals(field.Name, parameterLayout.Name, StringComparison.Ordinal)))
                            |> Option.bind (fun field ->
                                if field.IsImplicit then
                                    None
                                else
                                    usedNamedFields <- Set.add field.Name usedNamedFields
                                    Some field.Value)

                        for parameterLayout, rawParameterType in List.zip parameterLayouts parameterTypes do
                            let parameterType = TypeSignatures.applySubstitution substitution rawParameterType

                            if stoppedAtPartialApplication then
                                remainingParameters.Add(parameterLayout, parameterType)
                            elif parameterLayout.IsImplicit then
                                match remainingArguments with
                                | ExplicitImplicitArgument explicitArgument :: rest ->
                                    if isCompileTimeArgumentType environment.VisibleTypeAliases parameterType then
                                        match tryParseTypeLikeSurfaceExpression explicitArgument with
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
                                    else
                                        match inferArgumentType explicitArgument with
                                        | Some argumentType ->
                                            match tryUnifyVisibleTypes environment.VisibleTypeAliases [ parameterType, argumentType ] with
                                            | Some inferredSubstitution ->
                                                preparedArguments.Add(ExplicitArgument explicitArgument)
                                                preparedParameters.Add(
                                                    { Layout = Some parameterLayout
                                                      ParameterType = parameterType
                                                      AssignedArgument = Some(ExplicitArgument explicitArgument) }
                                                )
                                                remainingArguments <- rest
                                                let termSubstitution =
                                                    tryParseTypeLikeSurfaceExpression explicitArgument
                                                    |> Option.map (fun argumentType -> Map.ofList [ parameterLayout.Name, argumentType ])
                                                    |> Option.defaultValue Map.empty

                                                substitution <-
                                                    composeTypeSubstitution
                                                        (composeTypeSubstitution substitution inferredSubstitution)
                                                        termSubstitution
                                            | None ->
                                                success <- false
                                        | None ->
                                            success <- false
                                | _ ->
                                    match trySynthesizeImplicitArgument environment tryResolveLocalImplicit parameterType with
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
                                | ExplicitImplicitArgument _ :: _ ->
                                    success <- false
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
                                    match tryTakeNamedArgument parameterLayout with
                                    | Some namedArgument ->
                                        match tryParseTypeLikeSurfaceExpression namedArgument with
                                        | Some explicitTypeArgument ->
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
                                    | None ->
                                        if namedFields.IsSome && not (hasDefaultArgument parameterLayout) then
                                            success <- false
                                        else
                                            if namedFields.IsSome then
                                                preparedParameters.Add(
                                                    { Layout = Some parameterLayout
                                                      ParameterType = parameterType
                                                      AssignedArgument = None }
                                                )
                                            else
                                                stoppedAtPartialApplication <- true
                                                remainingParameters.Add(parameterLayout, parameterType)
                            else
                                let explicitArgument =
                                    match remainingArguments with
                                    | ExplicitImplicitArgument _ :: _ ->
                                        success <- false
                                        None
                                    | nextArgument :: rest ->
                                        remainingArguments <- rest
                                        Some nextArgument
                                    | [] ->
                                        match tryTakeNamedArgument parameterLayout with
                                        | Some namedArgument ->
                                            Some namedArgument
                                        | None ->
                                            if namedFields.IsSome && not (hasDefaultArgument parameterLayout) then
                                                success <- false
                                                None
                                            else
                                                if namedFields.IsSome then
                                                    preparedParameters.Add(
                                                        { Layout = Some parameterLayout
                                                          ParameterType = parameterType
                                                          AssignedArgument = None }
                                                    )
                                                else
                                                    stoppedAtPartialApplication <- true
                                                    remainingParameters.Add(parameterLayout, parameterType)
                                                None

                                explicitArgument
                                |> Option.iter (fun nextArgument ->
                                    match inferArgumentTypeForParameter parameterType nextArgument with
                                    | Some argumentType ->
                                        match tryUnifyVisibleTypes environment.VisibleTypeAliases [ parameterType, argumentType ] with
                                        | Some inferredSubstitution ->
                                            preparedArguments.Add(ExplicitArgument nextArgument)
                                            preparedParameters.Add(
                                                { Layout = Some parameterLayout
                                                  ParameterType = parameterType
                                                  AssignedArgument = Some(ExplicitArgument nextArgument) }
                                            )
                                            let termSubstitution =
                                                tryParseTypeLikeSurfaceExpression nextArgument
                                                |> Option.map (fun argumentType -> Map.ofList [ parameterLayout.Name, argumentType ])
                                                |> Option.defaultValue Map.empty

                                            substitution <-
                                                composeTypeSubstitution
                                                    (composeTypeSubstitution substitution inferredSubstitution)
                                                    termSubstitution
                                        | None ->
                                            success <- false
                                    | None ->
                                        success <- false)

                        let namedFieldsValid =
                            namedFieldCounts
                            |> Map.forall (fun _ count -> count = 1)
                            && (namedFields
                                |> Option.map (fun fields ->
                                    fields
                                    |> List.forall (fun field -> Set.contains field.Name usedNamedFields))
                                |> Option.defaultValue true)

                        if success && namedFieldsValid && List.isEmpty remainingArguments then
                            let instantiatedScheme = TypeSignatures.applySchemeSubstitution substitution instantiated
                            let appliedResultType =
                                let loweredRemainingParameters =
                                    remainingParameters
                                    |> Seq.toList
                                    |> List.map (fun (parameterLayout, parameterType) ->
                                        parameterLayout.Quantity |> Option.defaultValue QuantityOmega,
                                        TypeSignatures.applySubstitution substitution parameterType)

                                let trailingParameters =
                                    trailingParameterTypes
                                    |> List.map (fun parameterType ->
                                        QuantityOmega,
                                        TypeSignatures.applySubstitution substitution parameterType)

                                loweredRemainingParameters @ trailingParameters
                                |> List.rev
                                |> List.fold
                                    (fun current (parameterQuantity, parameterType) ->
                                        TypeArrow(parameterQuantity, parameterType, current))
                                    (TypeSignatures.applySubstitution substitution resultType)

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
                          IsPattern = false
                          Scheme = overloadedScheme
                          TypeTokens = []
                          ParameterLayouts = None
                          ConstructorTypeName = None
                          DefaultArguments = Map.empty }

                    tryPrepareVisibleBindingCall environment candidateCounter bindingInfo inferArgumentType noLocalImplicit arguments
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

    let private constructorResultBelongsToType
        (environment: BindingLoweringEnvironment)
        (typeObject: StaticObjectInfo)
        (constructorInfo: BindingSchemeInfo)
        =
        match typeObject.ObjectKind, typeObject.NameSegments with
        | StaticTypeObject, _ ->
            let _, resultType = TypeSignatures.schemeParts constructorInfo.Scheme

            match normalizeTypeAliases environment.VisibleTypeAliases resultType with
            | TypeName(resultName, _) ->
                match List.tryLast resultName, List.tryLast typeObject.NameSegments with
                | Some resultTypeName, Some objectTypeName ->
                    String.Equals(resultTypeName, objectTypeName, StringComparison.Ordinal)
                | _ ->
                    false
            | _ ->
                false
        | _ ->
            false

    let private tryResolveStaticConstructor
        (environment: BindingLoweringEnvironment)
        receiverExpression
        memberName
        =
        tryResolveStaticReceiverObject environment receiverExpression
        |> Option.bind (fun typeObject ->
            environment.VisibleConstructors
            |> Map.tryFind memberName
            |> Option.filter (constructorResultBelongsToType environment typeObject))

    let private tryInferStaticConstructorCall
        (environment: BindingLoweringEnvironment)
        (freshCounter: int ref)
        (inferArgumentType: SurfaceExpression -> TypeExpr option)
        receiverExpression
        memberName
        arguments
        =
        tryResolveStaticConstructor environment receiverExpression memberName
        |> Option.bind (fun constructorInfo ->
            tryPrepareVisibleBindingCall
                environment
                freshCounter
                constructorInfo
                inferArgumentType
                noLocalImplicit
                arguments
            |> Option.map (fun preparedCall -> preparedCall.ResultType))


    let rec private inferValidationExpressionType
        (environment: BindingLoweringEnvironment)
        (freshCounter: int ref)
        (localTypes: Map<string, TypeExpr>)
        expression
        =
        match expression with
        | Literal literal ->
            Some(inferLiteralType literal)
        | NumericLiteral literal ->
            match SurfaceNumericLiteral.suffix literal with
            | Some suffixName ->
                inferValidationExpressionType environment freshCounter localTypes (Apply(Name [ suffixName ], [ NumericLiteral(SurfaceNumericLiteral.withoutSuffix literal) ]))
            | None ->
                Some(inferSurfaceNumericLiteralType literal)
        | SyntaxQuote inner ->
            inferValidationExpressionType environment freshCounter localTypes inner
            |> Option.map syntaxType
        | SyntaxSplice inner ->
            inferValidationExpressionType environment freshCounter localTypes inner
            |> Option.bind (function
                | TypeName([ "Syntax" ], [ innerType ]) -> Some innerType
                | _ -> None)
        | TopLevelSyntaxSplice inner ->
            inferValidationExpressionType environment freshCounter localTypes inner
            |> Option.bind (function
                | TypeName([ "Syntax" ], [ innerType ]) -> Some innerType
                | _ -> None)
        | CodeQuote inner ->
            inferValidationExpressionType environment freshCounter localTypes inner
            |> Option.map codeType
        | CodeSplice inner ->
            inferValidationExpressionType environment freshCounter localTypes inner
            |> Option.bind (function
                | TypeName([ "Code" ], [ innerType ]) -> Some innerType
                | _ -> None)
        | Name [ "True" ]
        | Name [ "False" ] ->
            Some boolType
        | KindQualifiedName _ ->
            tryResolveScopedStaticObject environment expression
            |> Option.bind (fun staticObject -> staticObject.Scheme)
            |> Option.map (fun scheme -> scheme.Body)
        | Name(root :: path) ->
            let ordinaryResolution =
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
                                (tryResolveUniqueLocalImplicitByType environment localTypes)
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
                                (tryResolveUniqueLocalImplicitByType environment localTypes)
                                []
                            |> Option.map (fun preparedCall -> preparedCall.ResultType)
                        else
                            None))
                |> Option.orElseWith (fun () ->
                    tryResolveScopedStaticObject environment (Name [ root ])
                    |> Option.bind (fun staticObject ->
                        if List.isEmpty path then
                            staticObject.Scheme |> Option.map (fun scheme -> scheme.Body)
                        else
                            None))
                |> Option.bind (fun rootType -> tryProjectVisibleRecordType environment.VisibleTypeAliases rootType path)

            match path with
            | [ memberName ] ->
                tryInferStaticConstructorCall
                    environment
                    freshCounter
                    (inferValidationExpressionType environment freshCounter localTypes)
                    (Name [ root ])
                    memberName
                    []
                |> Option.orElse ordinaryResolution
            | _ ->
                ordinaryResolution
        | Name [] ->
            None
        | LocalSignature(_, body) ->
            inferValidationExpressionType environment freshCounter localTypes body
        | LocalTypeAlias(_, body) ->
            inferValidationExpressionType environment freshCounter localTypes body
        | LocalLet(binding, value, body) ->
            let bindingNames = collectPatternNames binding.Pattern

            let nextLocals =
                match inferValidationExpressionType environment freshCounter localTypes value with
                | Some valueType ->
                    extendBindingLocalTypes environment freshCounter localTypes binding (Some valueType)
                | None ->
                    extendBindingLocalTypes
                        environment
                        freshCounter
                        localTypes
                        binding
                        (Some(inferFallbackLocalType freshCounter value))

            let bodyForInference =
                match bindingNames, tryResolveScopedStaticObject environment value with
                | [ aliasName ], Some staticObject ->
                    rewriteStaticObjectAliasUse aliasName staticObject body
                | _ ->
                    body

            inferValidationExpressionType environment freshCounter nextLocals bodyForInference
        | LocalScopedEffect(declaration, body) ->
            withScopedEffectDeclaration declaration (fun () ->
                inferValidationExpressionType environment freshCounter localTypes body)
        | Handle(_, _, _, returnClause, _) ->
            inferValidationExpressionType environment freshCounter localTypes returnClause.Body
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
        | MemberAccess(receiver, segments, arguments) ->
            match segments with
            | [] ->
                inferValidationExpressionType environment freshCounter localTypes receiver
                |> Option.bind (fun receiverType ->
                    tryInferVisibleAppliedType
                        environment.VisibleTypeAliases
                        (inferValidationExpressionType environment freshCounter localTypes)
                        receiverType
                        arguments)
            | [ memberName ] ->
                tryInferStaticConstructorCall
                    environment
                    freshCounter
                    (inferValidationExpressionType environment freshCounter localTypes)
                    receiver
                    memberName
                    arguments
                |> Option.orElseWith (fun () ->
                    environment.VisibleBindings
                    |> Map.tryFind memberName
                    |> Option.bind (fun bindingInfo ->
                        tryBuildReceiverMethodArguments bindingInfo receiver arguments
                        |> Option.bind (fun receiverArguments ->
                            tryPrepareVisibleBindingCall
                                environment
                                freshCounter
                                bindingInfo
                                (inferValidationExpressionType environment freshCounter localTypes)
                                (tryResolveUniqueLocalImplicitByType environment localTypes)
                                receiverArguments
                            |> Option.map (fun preparedCall -> preparedCall.ResultType))))
                |> Option.orElseWith (fun () ->
                    inferValidationExpressionType environment freshCounter localTypes receiver
                    |> Option.bind (fun receiverType ->
                        tryProjectVisibleRecordType environment.VisibleTypeAliases receiverType [ memberName ]
                        |> Option.bind (fun memberType ->
                            tryInferVisibleAppliedType
                                environment.VisibleTypeAliases
                                (inferValidationExpressionType environment freshCounter localTypes)
                                memberType
                                arguments)))
            | memberName :: rest ->
                inferValidationExpressionType
                    environment
                    freshCounter
                    localTypes
                    (MemberAccess(MemberAccess(receiver, [ memberName ], []), rest, arguments))
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
        | ExplicitImplicitArgument inner ->
            inferValidationExpressionType environment freshCounter localTypes inner
        | NamedApplicationBlock _ ->
            None
        | InoutArgument inner ->
            inferValidationExpressionType environment freshCounter localTypes inner
        | Apply(Name [ receiverName; memberName ], arguments) ->
            tryInferStaticConstructorCall
                environment
                freshCounter
                (inferValidationExpressionType environment freshCounter localTypes)
                (Name [ receiverName ])
                memberName
                arguments
            |> Option.orElseWith (fun () ->
                environment.VisibleBindings
                |> Map.tryFind memberName
                |> Option.bind (fun bindingInfo ->
                    tryBuildReceiverMethodArguments bindingInfo (Name [ receiverName ]) arguments
                    |> Option.bind (fun receiverArguments ->
                        tryPrepareVisibleBindingCall
                            environment
                            freshCounter
                            bindingInfo
                            (inferValidationExpressionType environment freshCounter localTypes)
                            (tryResolveUniqueLocalImplicitByType environment localTypes)
                            receiverArguments
                        |> Option.map (fun preparedCall -> preparedCall.ResultType))))
        | Apply(Name [ calleeName ], arguments) ->
            environment.VisibleBindings
            |> Map.tryFind calleeName
            |> Option.bind (fun bindingInfo ->
                tryPrepareVisibleBindingCall
                    environment
                    freshCounter
                    bindingInfo
                    (inferValidationExpressionType environment freshCounter localTypes)
                    (tryResolveUniqueLocalImplicitByType environment localTypes)
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
                        (tryResolveUniqueLocalImplicitByType environment localTypes)
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
        | PrefixedString(prefix, _) ->
            tryInferPrefixedStringMacroResultType environment freshCounter localTypes prefix

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
                        (Some(unwrapBindPayloadType valueType))
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
        | DoDefer _ :: rest ->
            inferValidationDoResultType environment freshCounter localTypes rest
        | DoIf(_, _, _) :: rest ->
            inferValidationDoResultType environment freshCounter localTypes rest
        | DoWhile _ :: rest ->
            inferValidationDoResultType environment freshCounter localTypes rest
        | DoReturn expression :: _ ->
            inferValidationExpressionType environment freshCounter localTypes expression

    let private validateBuiltInExpressionsForBinding
        (environment: BindingLoweringEnvironment)
        (knownSurfaceTermNames: Set<string>)
        (allowUnresolvedCallDiagnostics: bool)
        (definition: LetDefinition)
        (scheme: TypeScheme option)
        =
        let freshCounter = ref 0
        let tupleParameterLocalTypes =
            definition.Parameters
            |> List.collect (fun parameter ->
                parameter.TypeTokens
                |> Option.map significantTokens
                |> Option.map splitTopLevelCommaGroups
                |> Option.map (fun groups ->
                    groups
                    |> List.skip 1
                    |> List.choose (fun group ->
                        match significantTokens group with
                        | nameToken :: colonToken :: typeTokens
                            when Token.isName nameToken && colonToken.Kind = Colon ->
                            typeTokens
                            |> TypeSignatures.parseType
                            |> Option.map (fun fieldType -> SyntaxFacts.trimIdentifierQuotes nameToken.Text, fieldType)
                        | _ ->
                            None))
                |> Option.defaultValue [])

        let recordParameterLocalTypes =
            definition.Parameters
            |> List.collect (fun parameter ->
                parameter.TypeTokens
                |> Option.bind tryParseRecordSurfaceInfo
                |> Option.map (fun recordInfo ->
                    recordInfo.Fields
                    |> List.choose (fun field ->
                        field.TypeTokens
                        |> TypeSignatures.parseType
                        |> Option.map (fun fieldType -> field.Name, fieldType)))
                |> Option.defaultValue [])

        let localTypes =
            (tupleParameterLocalTypes @ recordParameterLocalTypes)
            |> List.fold (fun state (name, fieldType) -> Map.add name fieldType state) (buildLocalTypes scheme definition.Parameters)

        let parameterNames =
            [ definition.Parameters |> List.map (fun parameter -> parameter.Name)
              tupleParameterLocalTypes |> List.map fst
              recordParameterLocalTypes |> List.map fst ]
            |> List.concat
            |> Set.ofList

        let preludeContract = IntrinsicCatalog.bundledPreludeExpectContract ()
        let standardRuntimeNames =
            Set.ofList
                [ "bindModule"; "bindModuleOwned"; "bridgePackageValue"; "bridgePackageOrigin"; "bridgeFailureToCastBlame"
                  "checkedCast"; "checkedCastWith"; "sameDynRep"; "toDyn"; "toDynWith"
                  "Conservative"; "Exact"; "IntoKappa"; "LaterUse"; "Lossy"; "OutOfKappa" ]

        let compilerKnownSurfaceTerms =
            Set.ofList
                [ "thunk"
                  "lazy"
                  "force"
                  "captureBorrow"
                  "withBorrowView"
                  "break"
                  "continue"
                  "fork"
                  "defEqSyntax"
                  "headSymbolSyntax"
                  "sameSymbol" ]

        let visibleTraitMemberNames =
            environment.VisibleTraits
            |> Map.values
            |> Seq.collect (fun traitInfo -> traitInfo.Members.Keys)
            |> Set.ofSeq

        let knownValueNames =
            [ preludeContract.TermNames
              Set.ofList Stdlib.FixedPreludeConstructors
              IntrinsicCatalog.namedIntrinsicTermNames ()
              standardRuntimeNames
              compilerKnownSurfaceTerms
              environment.VisibleBindings |> Map.keys |> Set.ofSeq
              environment.VisibleConstructors |> Map.keys |> Set.ofSeq
              visibleTraitMemberNames
              environment.ConstrainedMembers |> Map.keys |> Set.ofSeq ]
            |> Set.unionMany
            |> Set.remove "<anonymous>"

        let makeDiagnostic code message =
            { Severity = DiagnosticSeverity.Error
              Code = code
              Stage = Some "KFrontIR"
              Phase = Some(KFrontIRPhase.phaseName CORE_LOWERING)
              Message = message
              Location = None
              RelatedLocations = [] }

        let tryInstantiateVisibleBindingType name =
            let instantiate (bindingInfo: BindingSchemeInfo) =
                let probe = ref freshCounter.Value
                let instance = TypeSignatures.instantiate "t" probe.Value bindingInfo.Scheme
                snd (TypeSignatures.schemeParts instance)

            environment.VisibleBindings
            |> Map.tryFind name
            |> Option.map instantiate
            |> Option.orElseWith (fun () ->
                environment.VisibleConstructors
                |> Map.tryFind name
                |> Option.map instantiate)

        let prefixedStringPrefixDiagnostics locals lexicalNames prefix =
            let mismatchMessage =
                $"Prefixed string prefix '{prefix}' must resolve to a term of type 'Dict (InterpolatedMacro t)' for some 't'."

            let prefixType =
                locals
                |> Map.tryFind prefix
                |> Option.orElseWith (fun () -> tryInstantiateVisibleBindingType prefix)

            match prefixType with
            | Some resolvedType ->
                match tryInterpolatedMacroResultType environment.VisibleTypeAliases resolvedType with
                | Some _ -> []
                | None -> [ makeDiagnostic DiagnosticCode.TypeEqualityMismatch mismatchMessage ]
            | None when Set.contains prefix lexicalNames ->
                []
            | None when allowUnresolvedCallDiagnostics && not (Set.contains prefix knownValueNames) ->
                [ makeDiagnostic DiagnosticCode.NameUnresolved $"Name '{prefix}' is not in scope." ]
            | None when Set.contains prefix knownSurfaceTermNames ->
                [ makeDiagnostic DiagnosticCode.TypeEqualityMismatch mismatchMessage ]
            | None ->
                []

        let numericLiteralRangeDiagnostic literal =
            let sourceText = SurfaceNumericLiteral.toSurfaceText literal
            makeDiagnostic DiagnosticCode.NumericLiteralOutOfRange $"Numeric literal '{sourceText}' is outside the currently supported runtime range for its target type."

        let tryNumericLiteralRangeDiagnostic expectedType expression =
            match tryInferNumericExpressionTypeFromContext environment expectedType expression with
            | Some _ when tryLowerNumericExpressionForRuntime expression |> Option.isNone ->
                match expression with
                | NumericLiteral literal ->
                    Some(numericLiteralRangeDiagnostic literal)
                | Unary("-", NumericLiteral(SurfaceIntegerLiteral(value, text, suffix))) ->
                    Some(numericLiteralRangeDiagnostic (SurfaceIntegerLiteral(-value, "-" + text, suffix)))
                | Unary("-", NumericLiteral(SurfaceRealLiteral(decimalValue, text, suffix))) ->
                    Some(numericLiteralRangeDiagnostic (SurfaceRealLiteral(decimalValue |> Option.map (~-), "-" + text, suffix)))
                | _ ->
                    None
            | _ ->
                None

        let isVisibleConstructorName name =
            environment.VisibleConstructors |> Map.containsKey name

        let expressionSelectsOpaqueField expression =
            let tryRecordInfoFromType typeExpr =
                match typeExpr with
                | TypeName(nameSegments, []) ->
                    nameSegments
                    |> List.tryLast
                    |> Option.bind (fun name -> environment.VisibleRecordTypes |> Map.tryFind name)
                    |> Option.orElseWith (fun () ->
                        match normalizeTypeAliases environment.VisibleTypeAliases typeExpr with
                        | TypeRecord fields ->
                            Some
                                { Fields =
                                    fields
                                    |> List.map (fun field ->
                                        { Name = field.Name
                                          IsOpaque = false
                                          IsImplicit = field.Quantity = QuantityZero
                                          TypeTokens = [] })
                                  RowTail = None }
                        | _ ->
                            None)
                | _ ->
                    None

            let rootField expression =
                match expression with
                | Name [ root; fieldName ] ->
                    Some(root, fieldName)
                | MemberAccess(Name [ root ], [ fieldName ], []) ->
                    Some(root, fieldName)
                | _ ->
                    None

            rootField expression
            |> Option.bind (fun (root, fieldName) ->
                environment.VisibleBindings
                |> Map.tryFind root
                |> Option.bind (fun bindingInfo -> tryRecordInfoFromType bindingInfo.Scheme.Body)
                |> Option.bind (fun recordInfo ->
                    recordInfo.Fields
                    |> List.tryFind (fun field -> String.Equals(field.Name, fieldName, StringComparison.Ordinal))))
            |> Option.exists (fun field -> field.IsOpaque)

        let hasProvenStaticObjectIdentity locals expression =
            match expression with
            | KindQualifiedName(TypeKind, [ name ]) when Set.contains name (currentScopedEffectNames ()) ->
                true
            | KindQualifiedName(EffectLabelKind, [ name ]) when Set.contains name (currentScopedEffectNames ()) ->
                true
            | Name(root :: _) when locals |> Map.containsKey root ->
                not (Set.contains root parameterNames)
            | _ ->
                tryResolveStaticReceiverObject environment expression
                |> Option.isSome
                |> function
                    | true -> true
                    | false ->
                        match expression with
                        | Name _ ->
                            false
                        | _ ->
                            not (expressionSelectsOpaqueField expression)
                            && (inferValidationExpressionType environment (ref freshCounter.Value) locals expression
                                |> Option.exists (isReifiedStaticObjectType environment.VisibleTypeAliases))

        let staticConstructorIdentityDiagnostic locals receiver memberName =
            if isVisibleConstructorName memberName
               && not (hasProvenStaticObjectIdentity locals receiver) then
                [ makeDiagnostic DiagnosticCode.StaticObjectUnresolved $"Static constructor '{memberName}' requires a receiver with preserved static-object identity." ]
            else
                []

        let rec staticPatternIdentityDiagnostics locals pattern =
            match pattern with
            | AsPattern(_, inner) ->
                staticPatternIdentityDiagnostics locals inner
            | TypedPattern(inner, _) ->
                staticPatternIdentityDiagnostics locals inner
            | ConstructorPattern(segments, arguments) ->
                let nested = arguments |> List.collect (staticPatternIdentityDiagnostics locals)

                match List.rev segments with
                | memberName :: receiverSegments when not (List.isEmpty receiverSegments) ->
                    let receiver = Name(List.rev receiverSegments)
                    staticConstructorIdentityDiagnostic locals receiver memberName @ nested
                | _ ->
                    nested
            | NamedConstructorPattern(segments, fields) ->
                staticPatternIdentityDiagnostics locals (ConstructorPattern(segments, fields |> List.map (fun field -> field.Pattern)))
            | TuplePattern elements ->
                elements |> List.collect (staticPatternIdentityDiagnostics locals)
            | VariantPattern _ ->
                []
            | OrPattern alternatives ->
                alternatives |> List.collect (staticPatternIdentityDiagnostics locals)
            | AnonymousRecordPattern(fields, _) ->
                fields |> List.collect (fun field -> staticPatternIdentityDiagnostics locals field.Pattern)
            | WildcardPattern
            | NamePattern _
            | LiteralPattern _ ->
                []

        let transparentSealFields ascriptionTokens =
            match tryParseRecordSurfaceInfo ascriptionTokens with
            | Some recordInfo ->
                recordInfo.Fields
                |> List.filter (fun field -> not field.IsOpaque)
                |> List.map (fun field -> field.Name)
                |> Set.ofList
                |> Some
            | None ->
                match significantTokens ascriptionTokens with
                | [ nameToken ] when Token.isName nameToken ->
                    environment.VisibleRecordTypes
                    |> Map.tryFind (SyntaxFacts.trimIdentifierQuotes nameToken.Text)
                    |> Option.map (fun recordInfo ->
                        recordInfo.Fields
                        |> List.filter (fun field -> not field.IsOpaque)
                        |> List.map (fun field -> field.Name)
                        |> Set.ofList)
                | _ ->
                    None

        let aliasesAsVisibleStaticObjects (aliases: Map<string list, StaticObjectInfo>) =
            aliases
            |> Map.fold
                (fun current segments staticObject ->
                    Map.add (SyntaxFacts.moduleNameToText segments) staticObject current)
                environment.VisibleStaticObjects

        let tryResolveWithLocalAliases aliases expression =
            let aliasEnvironment =
                { environment with
                    VisibleStaticObjects = aliasesAsVisibleStaticObjects aliases }

            tryResolveScopedStaticObject aliasEnvironment expression

        let copyVisibleNestedAliases
            (sourcePrefix: string list)
            (targetPrefix: string list)
            fieldFilter
            (aliases: Map<string list, StaticObjectInfo>)
            =
            let sourceKey = SyntaxFacts.moduleNameToText sourcePrefix
            let sourceNestedPrefix = sourceKey + "."

            aliasesAsVisibleStaticObjects aliases
            |> Map.toList
            |> List.filter (fun ((key: string), _) ->
                if key.StartsWith(sourceNestedPrefix, StringComparison.Ordinal) then
                    let suffix = key.Substring(sourceNestedPrefix.Length)
                    let firstSegment = suffix.Split('.')[0]

                    fieldFilter firstSegment
                else
                    false)
            |> List.fold (fun current ((key: string), staticObject) ->
                let suffixSegments =
                    key.Substring(sourceNestedPrefix.Length).Split('.', StringSplitOptions.RemoveEmptyEntries)
                    |> Array.toList

                Map.add (targetPrefix @ suffixSegments) staticObject current) aliases

        let rec collectExpressionStaticAliases prefix expression aliases =
            match tryResolveWithLocalAliases aliases expression with
            | Some staticObject ->
                let aliases = Map.add prefix staticObject aliases

                match expression with
                | Name sourcePrefix -> copyVisibleNestedAliases sourcePrefix prefix (fun _ -> true) aliases
                | _ -> aliases
            | None ->
                match expression with
                | RecordLiteral fields ->
                    fields
                    |> List.fold (fun current field -> collectExpressionStaticAliases (prefix @ [ field.Name ]) field.Value current) aliases
                | Seal(value, ascriptionTokens) ->
                    match value, transparentSealFields ascriptionTokens with
                    | Name sourcePrefix, Some transparentFields ->
                        copyVisibleNestedAliases sourcePrefix prefix (fun fieldName -> Set.contains fieldName transparentFields) aliases
                    | RecordLiteral fields, Some transparentFields ->
                        fields
                        |> List.filter (fun field -> Set.contains field.Name transparentFields)
                        |> List.fold (fun current field -> collectExpressionStaticAliases (prefix @ [ field.Name ]) field.Value current) aliases
                    | _, Some _ ->
                        aliases
                    | _ ->
                        collectExpressionStaticAliases prefix value aliases
                | _ ->
                    aliases

        let rec collectPatternStaticAliases pattern value aliases =
            match pattern with
            | NamePattern name ->
                collectExpressionStaticAliases [ name ] value aliases
            | AsPattern(name, inner) ->
                collectPatternStaticAliases inner value (collectExpressionStaticAliases [ name ] value aliases)
            | TypedPattern(inner, _) ->
                collectPatternStaticAliases inner value aliases
            | AnonymousRecordPattern(fields, rest) ->
                fields
                |> List.fold (fun current field ->
                    let fieldExpression =
                        match value with
                        | Name segments -> Name(segments @ [ field.Name ])
                        | _ -> MemberAccess(value, [ field.Name ], [])

                    collectPatternStaticAliases field.Pattern fieldExpression current) aliases
                |> fun current ->
                    match rest with
                    | Some(BindRecordPatternRest name) ->
                        collectExpressionStaticAliases [ name ] value current
                    | _ ->
                        current
            | ConstructorPattern(_, arguments) ->
                arguments |> List.fold (fun current argument -> collectPatternStaticAliases argument value current) aliases
            | NamedConstructorPattern(_, fields) ->
                fields |> List.fold (fun current field -> collectPatternStaticAliases field.Pattern value current) aliases
            | TuplePattern elements ->
                elements |> List.fold (fun current element -> collectPatternStaticAliases element value current) aliases
            | VariantPattern(BoundVariantPattern(name, _))
            | VariantPattern(RestVariantPattern name) ->
                collectExpressionStaticAliases [ name ] value aliases
            | VariantPattern(WildcardVariantPattern _) ->
                aliases
            | OrPattern alternatives ->
                alternatives
                |> List.tryHead
                |> Option.map (fun first -> collectPatternStaticAliases first value aliases)
                |> Option.defaultValue aliases
            | WildcardPattern
            | LiteralPattern _ ->
                aliases

        let typeHeadName typeExpr =
            match typeExpr with
            | TypeName(nameSegments, _) when not (List.isEmpty nameSegments) -> Some(List.last nameSegments)
            | _ -> None

        let rec finalResultType typeExpr =
            match typeExpr with
            | TypeArrow(_, _, resultType) -> finalResultType resultType
            | other -> other

        let isMonadicType typeExpr =
            match typeHeadName (finalResultType typeExpr) with
            | Some "IO"
            | Some "UIO" -> true
            | _ -> false

        let activePatternResultKind (bindingInfo: BindingSchemeInfo) =
            match typeHeadName (finalResultType bindingInfo.Scheme.Body) with
            | Some "Option" -> Some "Option"
            | Some "Match" -> Some "Match"
            | Some _ -> Some "Total"
            | None -> None

        let activePatternScrutineeQuantity (bindingInfo: BindingSchemeInfo) =
            bindingInfo.ParameterLayouts
            |> Option.bind (fun layouts ->
                layouts
                |> List.filter (fun parameter -> not parameter.IsImplicit)
                |> List.tryLast
                |> Option.bind (fun parameter -> parameter.Quantity))

        let tryActivePatternBinding pattern =
            match pattern with
            | ConstructorPattern(nameSegments, _) when not (List.isEmpty nameSegments) ->
                environment.VisibleBindings
                |> Map.tryFind (List.last nameSegments)
                |> Option.filter (fun bindingInfo -> bindingInfo.IsPattern)
            | _ ->
                None

        let rec validatePatternHead pattern =
            match pattern with
            | AsPattern(_, inner) ->
                validatePatternHead inner
            | TypedPattern(inner, _) ->
                validatePatternHead inner
            | ConstructorPattern(nameSegments, arguments) ->
                let nestedDiagnostics = arguments |> List.collect validatePatternHead
                let headDiagnostics =
                    match nameSegments with
                    | [] -> []
                    | _ ->
                        let headName = List.last nameSegments
                        let isConstructor = environment.VisibleConstructors |> Map.containsKey headName

                        match environment.VisibleBindings |> Map.tryFind headName with
                        | Some bindingInfo when not bindingInfo.IsPattern && not isConstructor ->
                            [ makeDiagnostic DiagnosticCode.ActivePatternInvalid $"Pattern head '{headName}' resolves to an ordinary term, not a constructor or active pattern." ]
                        | _ ->
                            []

                headDiagnostics @ nestedDiagnostics
            | NamedConstructorPattern(nameSegments, fields) ->
                validatePatternHead (ConstructorPattern(nameSegments, fields |> List.map (fun field -> field.Pattern)))
            | TuplePattern elements ->
                elements |> List.collect validatePatternHead
            | VariantPattern _ ->
                []
            | OrPattern alternatives ->
                alternatives |> List.collect validatePatternHead
            | AnonymousRecordPattern(fields, _) ->
                fields |> List.collect (fun field -> validatePatternHead field.Pattern)
            | WildcardPattern
            | NamePattern _
            | LiteralPattern _ ->
                []

        let activePatternLinearityDiagnostics pattern context =
            match tryActivePatternBinding pattern with
            | Some bindingInfo ->
                match activePatternResultKind bindingInfo, activePatternScrutineeQuantity bindingInfo with
                | Some "Option", Some QuantityOne ->
                    [ makeDiagnostic DiagnosticCode.ActivePatternInvalid $"Option-returning active pattern '{bindingInfo.Name}' consumes its scrutinee linearly in a refutable {context}." ]
                | Some "Match", _ when String.Equals(context, "let?", StringComparison.Ordinal) ->
                    [ makeDiagnostic DiagnosticCode.ActivePatternInvalid $"Match-returning active pattern '{bindingInfo.Name}' is not permitted in plain let? destructuring." ]
                | _ ->
                    []
            | None ->
                []

        let memberText (navigation: SurfaceSafeNavigationMember) =
            let argumentsText =
                navigation.Arguments
                |> List.map (fun _ -> "<arg>")
                |> String.concat " "

            let segmentsText = String.concat "." navigation.Segments
            match argumentsText with
            | "" -> segmentsText
            | _ -> $"{segmentsText} {argumentsText}"

        let tryRecordInfoFromTypeName name =
            environment.VisibleRecordTypes |> Map.tryFind name

        let tryRecordInfoFromTypeExpr typeExpr =
            match typeExpr with
            | TypeName(nameSegments, []) ->
                nameSegments
                |> List.tryLast
                |> Option.bind tryRecordInfoFromTypeName
                |> Option.orElseWith (fun () ->
                    match normalizeTypeAliases environment.VisibleTypeAliases typeExpr with
                    | TypeRecord fields ->
                        Some
                            { Fields =
                                fields
                                |> List.map (fun field ->
                                    { Name = field.Name
                                      IsOpaque = false
                                      IsImplicit = field.Quantity = QuantityZero
                                      TypeTokens = [] })
                              RowTail = None }
                    | _ ->
                        None)
            | _ ->
                match normalizeTypeAliases environment.VisibleTypeAliases typeExpr with
                | TypeRecord fields ->
                    Some
                        { Fields =
                            fields
                            |> List.map (fun field ->
                                { Name = field.Name
                                  IsOpaque = false
                                  IsImplicit = field.Quantity = QuantityZero
                                  TypeTokens = [] })
                          RowTail = None }
                | _ ->
                    None

        let tryRecordInfoFromTypeTokens tokens =
            tryParseRecordSurfaceInfo tokens
            |> Option.orElseWith (fun () ->
                match significantTokens tokens with
                | [ nameToken ] when Token.isName nameToken ->
                    tryRecordInfoFromTypeName (SyntaxFacts.trimIdentifierQuotes nameToken.Text)
                | _ ->
                    None)

        let localRecordTypes =
            definition.Parameters
            |> List.choose (fun parameter ->
                parameter.TypeTokens
                |> Option.bind tryRecordInfoFromTypeTokens
                |> Option.map (fun recordInfo -> parameter.Name, recordInfo))
            |> Map.ofList

        let tryRecordInfoForRoot locals root =
            localRecordTypes
            |> Map.tryFind root
            |> Option.orElseWith (fun () ->
                locals
                |> Map.tryFind root
                |> Option.bind tryRecordInfoFromTypeExpr)
            |> Option.orElseWith (fun () ->
                environment.VisibleBindings
                |> Map.tryFind root
                |> Option.bind (fun bindingInfo ->
                    tryRecordInfoFromTypeExpr bindingInfo.Scheme.Body
                    |> Option.orElseWith (fun () -> tryRecordInfoFromTypeTokens bindingInfo.TypeTokens)))

        let tryRecordInfoForExpression locals expression =
            match expression with
            | Name(root :: _) -> tryRecordInfoForRoot locals root
            | _ -> None

        let trueTermType = TypeName([ "True" ], [])
        let falseTermType = TypeName([ "False" ], [])

        let applyRefinements refinements typeExpr =
            TypeSignatures.applySubstitution refinements typeExpr

        let normalizeExpectedType refinements typeExpr =
            typeExpr
            |> applyRefinements refinements
            |> normalizeTypeAliases environment.VisibleTypeAliases

        let rec typeContainsLocalTermVariable locals typeExpr =
            match typeExpr with
            | TypeLevelLiteral _ ->
                false
            | TypeUniverse None ->
                false
            | TypeUniverse(Some universeExpr) ->
                typeContainsLocalTermVariable locals universeExpr
            | TypeIntrinsic _ ->
                false
            | TypeApply(callee, arguments) ->
                typeContainsLocalTermVariable locals callee
                || (arguments |> List.exists (typeContainsLocalTermVariable locals))
            | TypeLambda(_, parameterSort, body) ->
                typeContainsLocalTermVariable locals parameterSort
                || typeContainsLocalTermVariable locals body
            | TypeDelay inner
            | TypeMemo inner
            | TypeForce inner ->
                typeContainsLocalTermVariable locals inner
            | TypeProject(target, _) ->
                typeContainsLocalTermVariable locals target
            | TypeVariable name ->
                Map.containsKey name locals
            | TypeName(_, arguments) ->
                arguments |> List.exists (typeContainsLocalTermVariable locals)
            | TypeArrow(_, parameterType, resultType) ->
                typeContainsLocalTermVariable locals parameterType
                || typeContainsLocalTermVariable locals resultType
            | TypeEquality(left, right) ->
                typeContainsLocalTermVariable locals left
                || typeContainsLocalTermVariable locals right
            | TypeCapture(inner, _) ->
                typeContainsLocalTermVariable locals inner
            | TypeEffectRow(entries, tail) ->
                entries
                |> List.exists (fun entry ->
                    typeContainsLocalTermVariable locals entry.Label
                    || typeContainsLocalTermVariable locals entry.Effect)
                || tail |> Option.exists (typeContainsLocalTermVariable locals)
            | TypeRecord fields ->
                fields |> List.exists (fun field -> typeContainsLocalTermVariable locals field.Type)
            | TypeUnion members ->
                members |> List.exists (typeContainsLocalTermVariable locals)

        let rec typeContainsSuspension typeExpr =
            match normalizeTypeAliases environment.VisibleTypeAliases typeExpr with
            | TypeDelay _
            | TypeMemo _
            | TypeForce _ ->
                true
            | TypeLevelLiteral _ ->
                false
            | TypeUniverse None ->
                false
            | TypeUniverse(Some universeExpr) ->
                typeContainsSuspension universeExpr
            | TypeIntrinsic _ ->
                false
            | TypeApply(callee, arguments) ->
                typeContainsSuspension callee || (arguments |> List.exists typeContainsSuspension)
            | TypeLambda(_, parameterSort, body) ->
                typeContainsSuspension parameterSort || typeContainsSuspension body
            | TypeProject(target, _) ->
                typeContainsSuspension target
            | TypeName(([ "Thunk" ] | [ "std"; "prelude"; "Thunk" ] | [ "Need" ] | [ "std"; "prelude"; "Need" ]), _) ->
                true
            | TypeName(_, arguments) ->
                arguments |> List.exists typeContainsSuspension
            | TypeArrow(_, parameterType, resultType) ->
                typeContainsSuspension parameterType || typeContainsSuspension resultType
            | TypeEquality(left, right) ->
                typeContainsSuspension left || typeContainsSuspension right
            | TypeCapture(inner, _) ->
                typeContainsSuspension inner
            | TypeEffectRow(entries, tail) ->
                entries
                |> List.exists (fun entry ->
                    typeContainsSuspension entry.Label
                    || typeContainsSuspension entry.Effect)
                || tail |> Option.exists typeContainsSuspension
            | TypeRecord fields ->
                fields |> List.exists (fun field -> typeContainsSuspension field.Type)
            | TypeUnion members ->
                members |> List.exists typeContainsSuspension
            | TypeVariable _ ->
                false

        let rec typeContainsUnion typeExpr =
            match normalizeTypeAliases environment.VisibleTypeAliases typeExpr with
            | TypeLevelLiteral _ ->
                false
            | TypeUniverse None ->
                false
            | TypeUniverse(Some universeExpr) ->
                typeContainsUnion universeExpr
            | TypeIntrinsic _ ->
                false
            | TypeApply(callee, arguments) ->
                typeContainsUnion callee || (arguments |> List.exists typeContainsUnion)
            | TypeLambda(_, parameterSort, body) ->
                typeContainsUnion parameterSort || typeContainsUnion body
            | TypeDelay inner
            | TypeMemo inner
            | TypeForce inner ->
                typeContainsUnion inner
            | TypeProject(target, _) ->
                typeContainsUnion target
            | TypeUnion _ ->
                true
            | TypeName(_, arguments) ->
                arguments |> List.exists typeContainsUnion
            | TypeArrow(_, parameterType, resultType) ->
                typeContainsUnion parameterType || typeContainsUnion resultType
            | TypeEquality(left, right) ->
                typeContainsUnion left || typeContainsUnion right
            | TypeCapture(inner, _) ->
                typeContainsUnion inner
            | TypeEffectRow(entries, tail) ->
                entries
                |> List.exists (fun entry ->
                    typeContainsUnion entry.Label
                    || typeContainsUnion entry.Effect)
                || tail |> Option.exists typeContainsUnion
            | TypeRecord fields ->
                fields |> List.exists (fun field -> typeContainsUnion field.Type)
            | TypeVariable _ ->
                false

        let trySuspensionInner typeExpr =
            match normalizeTypeAliases environment.VisibleTypeAliases typeExpr with
            | TypeName(([ "Thunk" ] | [ "std"; "prelude"; "Thunk" ]), [ inner ]) ->
                Some inner
            | TypeName(([ "Need" ] | [ "std"; "prelude"; "Need" ]), [ inner ]) ->
                Some inner
            | _ ->
                None

        let tryUnionMembers typeExpr =
            match normalizeTypeAliases environment.VisibleTypeAliases typeExpr with
            | TypeUnion members -> Some members
            | _ -> None

        let rec expectedTypeAccepts locals refinements expectedType actualType =
            let expectedType = normalizeExpectedType refinements expectedType
            let actualType = normalizeExpectedType refinements actualType

            if TypeSignatures.definitionallyEqual expectedType actualType then
                true
            else
                match trySuspensionInner expectedType with
                | Some innerType ->
                    expectedTypeAccepts locals refinements innerType actualType
                | None ->
                    match expectedType, actualType with
                    | TypeArrow(expectedQuantity, expectedParameter, expectedResult),
                      TypeArrow(actualQuantity, actualParameter, actualResult)
                        when TypeSignatures.definitionallyEqual expectedParameter actualParameter
                             && TypeSignatures.definitionallyEqual expectedResult actualResult
                             && ResourceModel.ResourceQuantity.satisfies
                                    (ResourceModel.ResourceQuantity.ofSurface expectedQuantity)
                                    (ResourceModel.ResourceQuantity.ofSurface actualQuantity) ->
                        true
                    | TypeRecord expectedFields, TypeRecord actualFields ->
                        let actualFieldMap =
                            actualFields
                            |> List.map (fun field -> field.Name, field)
                            |> Map.ofList

                        List.length expectedFields = List.length actualFields
                        && expectedFields
                           |> List.forall (fun expectedField ->
                               actualFieldMap
                               |> Map.tryFind expectedField.Name
                               |> Option.exists (fun actualField ->
                                   expectedField.Quantity = actualField.Quantity
                                   && expectedTypeAccepts locals refinements expectedField.Type actualField.Type))
                    | _ ->
                        match tryUnionMembers expectedType, tryUnionMembers actualType with
                        | Some expectedMembers, Some actualMembers ->
                            actualMembers
                            |> List.forall (fun actualMember ->
                                expectedMembers
                                |> List.exists (fun expectedMember ->
                                    expectedTypeAccepts locals refinements expectedMember actualMember))
                        | Some expectedMembers, None ->
                            expectedMembers
                            |> List.exists (fun expectedMember ->
                                expectedTypeAccepts locals refinements expectedMember actualType)
                        | _ when typeContainsLocalTermVariable locals expectedType
                                 || typeContainsLocalTermVariable locals actualType ->
                            false
                        | _ ->
                            tryUnifyVisibleTypes environment.VisibleTypeAliases [ expectedType, actualType ]
                            |> Option.isSome

        let expectedMismatchDiagnostic locals refinements context expectedType actualType =
            let expectedType = normalizeExpectedType refinements expectedType
            let actualType = normalizeExpectedType refinements actualType

            let message =
                if typeContainsLocalTermVariable locals expectedType
                   || typeContainsLocalTermVariable locals actualType then
                    $"{context} requires equality transport evidence for dependent expected type '{TypeSignatures.toText expectedType}', but the argument has type '{TypeSignatures.toText actualType}'."
                elif
                    match expectedType, actualType with
                    | TypeArrow _, TypeArrow _ -> true
                    | _ -> false
                then
                    $"{context} has incompatible function quantity: expected '{TypeSignatures.toText expectedType}' but found '{TypeSignatures.toText actualType}'."
                elif typeContainsUnion expectedType then
                    $"{context} does not match the expected union type '{TypeSignatures.toText expectedType}'; no valid union injection or widening applies from '{TypeSignatures.toText actualType}'."
                elif typeContainsSuspension expectedType then
                    $"{context} does not match the expected suspension type '{TypeSignatures.toText expectedType}' from expression type '{TypeSignatures.toText actualType}'."
                else
                    $"{context} type mismatch: expected '{TypeSignatures.toText expectedType}' but found '{TypeSignatures.toText actualType}'."

            makeDiagnostic DiagnosticCode.TypeEqualityMismatch message

        let expectedTypeDiagnostics locals refinements context expectedType expression =
            match tryNumericLiteralRangeDiagnostic expectedType expression with
            | Some diagnostic ->
                [ diagnostic ]
            | None ->
                match tryInferNumericExpressionTypeFromContext environment expectedType expression with
                | Some actualType when not (expectedTypeAccepts locals refinements expectedType actualType) ->
                    [ expectedMismatchDiagnostic locals refinements context expectedType actualType ]
                | Some _ ->
                    []
                | None ->
                    match inferValidationExpressionType environment freshCounter locals expression with
                    | Some actualType when not (expectedTypeAccepts locals refinements expectedType actualType) ->
                        [ expectedMismatchDiagnostic locals refinements context expectedType actualType ]
                    | _ ->
                        []

        let fieldTypeReferences (recordInfo: RecordSurfaceInfo) (tokens: Token list) =
            let fieldNames = recordSurfaceFieldNames recordInfo
            let tokenArray = significantTokens tokens |> List.toArray
            let references = ResizeArray<string>()

            for index = 0 to tokenArray.Length - 1 do
                let token = tokenArray[index]

                if Token.isName token then
                    let name = SyntaxFacts.trimIdentifierQuotes token.Text

                    if String.Equals(name, "this", StringComparison.Ordinal)
                       && index + 2 < tokenArray.Length
                       && tokenArray[index + 1].Kind = Dot
                       && Token.isName tokenArray[index + 2] then
                        references.Add(SyntaxFacts.trimIdentifierQuotes tokenArray[index + 2].Text)
                    elif Set.contains name fieldNames then
                        references.Add(name)

            references |> Seq.toList |> List.distinct

        let expressionThisReferences expression =
            let rec loop current =
                seq {
                    match current with
                    | Name("this" :: fieldName :: _) ->
                        yield fieldName
                    | SyntaxQuote inner
                    | SyntaxSplice inner
                    | TopLevelSyntaxSplice inner
                    | CodeQuote inner
                    | CodeSplice inner ->
                        yield! loop inner
                    | LocalLet(_, value, body) ->
                        yield! loop value
                        yield! loop body
                    | LocalSignature(_, body) ->
                        yield! loop body
                    | LocalTypeAlias(_, body) ->
                        yield! loop body
                    | LocalScopedEffect(_, body) ->
                        yield! loop body
                    | Handle(_, label, body, returnClause, operationClauses) ->
                        yield! loop label
                        yield! loop body
                        yield! loop returnClause.Body

                        for clause in operationClauses do
                            yield! loop clause.Body
                    | Lambda(_, body) ->
                        yield! loop body
                    | IfThenElse(condition, whenTrue, whenFalse) ->
                        yield! loop condition
                        yield! loop whenTrue
                        yield! loop whenFalse
                    | Match(scrutinee, cases) ->
                        yield! loop scrutinee
                        for caseClause in cases do
                            match caseClause.Guard with
                            | Some guard -> yield! loop guard
                            | None -> ()
                            yield! loop caseClause.Body
                    | RecordLiteral fields ->
                        for field in fields do
                            yield! loop field.Value
                    | Seal(value, _) ->
                        yield! loop value
                    | RecordUpdate(receiver, fields) ->
                        yield! loop receiver
                        for field in fields do
                            yield! loop field.Value
                    | MemberAccess(receiver, _, arguments) ->
                        yield! loop receiver
                        for argument in arguments do
                            yield! loop argument
                    | SafeNavigation(receiver, navigation) ->
                        yield! loop receiver
                        for argument in navigation.Arguments do
                            yield! loop argument
                    | TagTest(receiver, _) ->
                        yield! loop receiver
                    | Do statements ->
                        for statement in statements do
                            match statement with
                            | DoLet(_, value)
                            | DoBind(_, value)
                            | DoUsing(_, value)
                            | DoVar(_, value)
                            | DoAssign(_, value)
                            | DoDefer value
                            | DoExpression value
                            | DoReturn value ->
                                yield! loop value
                            | DoLetQuestion(_, value, failure) ->
                                yield! loop value
                                match failure with
                                | Some failure ->
                                    for nested in failure.Body do
                                        match nested with
                                        | DoExpression expression -> yield! loop expression
                                        | _ -> ()
                                | None -> ()
                            | DoIf(condition, _, _) ->
                                yield! loop condition
                            | DoWhile(condition, _) ->
                                yield! loop condition
                    | MonadicSplice inner
                    | ExplicitImplicitArgument inner
                    | InoutArgument inner
                    | Unary(_, inner) ->
                        yield! loop inner
                    | NamedApplicationBlock fields ->
                        for field in fields do
                            yield! loop field.Value
                    | Apply(callee, arguments) ->
                        yield! loop callee
                        for argument in arguments do
                            yield! loop argument
                    | Binary(left, _, right)
                    | Elvis(left, right) ->
                        yield! loop left
                        yield! loop right
                    | PrefixedString(_, parts) ->
                        for part in parts do
                            match part with
                            | StringInterpolation(inner, _) -> yield! loop inner
                            | StringText _ -> ()
                    | Literal _
                    | NumericLiteral _
                    | KindQualifiedName _
                    | Name _ ->
                        ()
                }

            loop expression |> Seq.toList |> List.distinct

        let hasCycle (edges: Map<string, Set<string>>) =
            let rec visit visiting visited node =
                if Set.contains node visiting then
                    true
                elif Set.contains node visited then
                    false
                else
                    let nextVisiting = Set.add node visiting
                    let dependencies = edges |> Map.tryFind node |> Option.defaultValue Set.empty

                    dependencies
                    |> Set.exists (visit nextVisiting (Set.add node visited))

            edges |> Map.exists (fun node _ -> visit Set.empty Set.empty node)

        let validateRecordSurfaceInfo (recordInfo: RecordSurfaceInfo) =
            let duplicateDiagnostics =
                recordInfo.Fields
                |> List.countBy (fun field -> field.Name)
                |> List.choose (fun (name, count) ->
                    if count > 1 then
                        Some(makeDiagnostic DiagnosticCode.RecordDuplicateField $"Record field '{name}' is declared more than once.")
                    else
                        None)

            let fieldNames = recordSurfaceFieldNames recordInfo

            let dependencyDiagnostics =
                recordInfo.Fields
                |> List.collect (fun field ->
                    fieldTypeReferences recordInfo field.TypeTokens
                    |> List.choose (fun referencedField ->
                        if Set.contains referencedField fieldNames then
                            None
                        else
                            Some(
                                makeDiagnostic
                                    DiagnosticCode.RecordDependencyInvalid
                                    $"Record field '{field.Name}' depends on field '{referencedField}', which is not in the explicit record telescope."
                            )))

            let cycleDiagnostics =
                let dependencyMap =
                    recordInfo.Fields
                    |> List.map (fun field ->
                        field.Name,
                        fieldTypeReferences recordInfo field.TypeTokens
                        |> List.filter (fun referencedField ->
                            Set.contains referencedField fieldNames
                            && not (String.Equals(referencedField, field.Name, StringComparison.Ordinal)))
                        |> Set.ofList)
                    |> Map.ofList

                if hasCycle dependencyMap then
                    [ makeDiagnostic DiagnosticCode.RecordDependencyCycle "Record type field dependencies must be acyclic." ]
                else
                    []

            duplicateDiagnostics @ dependencyDiagnostics @ cycleDiagnostics

        let pathKey (field: SurfaceRecordUpdateField) =
            field.Path
            |> List.map (fun segment -> if segment.IsImplicit then $"@{segment.Name}" else segment.Name)
            |> String.concat "."

        let isStrictPrefix left right =
            List.length left < List.length right
            && List.forall2
                (fun (leftSegment: SurfaceRecordUpdatePathSegment) (rightSegment: SurfaceRecordUpdatePathSegment) ->
                    leftSegment.IsImplicit = rightSegment.IsImplicit
                    && String.Equals(leftSegment.Name, rightSegment.Name, StringComparison.Ordinal))
                left
                (right |> List.take (List.length left))

        let hasLacksConstraint rowName labelName =
            let parameterHasConstraint =
                definition.Parameters
                |> List.exists (fun parameter ->
                    parameter.TypeTokens
                    |> Option.map tokenText
                    |> Option.exists (fun text -> String.Equals(text, $"LacksRec {rowName} {labelName}", StringComparison.Ordinal)))

            let schemeHasConstraint =
                scheme
                |> Option.exists (fun scheme ->
                    scheme.Constraints
                    |> List.exists (fun constraintInfo ->
                        String.Equals(constraintInfo.TraitName, "LacksRec", StringComparison.Ordinal)
                        && match constraintInfo.Arguments with
                           | [ TypeVariable row; TypeVariable label ] ->
                               String.Equals(row, rowName, StringComparison.Ordinal)
                               && String.Equals(label, labelName, StringComparison.Ordinal)
                           | _ ->
                               false))

            parameterHasConstraint || schemeHasConstraint

        let validateRecordPatch locals receiver fields =
            let ordinaryFields = fields |> List.filter (fun field -> not field.IsExtension && field.Name <> "<missing>")
            let extensionFields = fields |> List.filter (fun field -> field.IsExtension && field.Name <> "<missing>")

            let projectionPatchInfo (field: SurfaceRecordUpdateField) =
                match receiver, field.Path with
                | Name [ receiverName ], [ segment ] ->
                    tryReceiverProjection environment locals receiverName segment.Name
                | _ ->
                    None

            let projectionSupportsSet (projectionInfo: ProjectionInfo) =
                match projectionInfo.Body with
                | Some(ProjectionAccessors clauses) ->
                    let clauseKinds =
                        clauses
                        |> List.map (function
                            | ProjectionGet _ -> "get"
                            | ProjectionInout _ -> "open"
                            | ProjectionSet _ -> "set"
                            | ProjectionSink _ -> "sink")
                        |> Set.ofList

                    Set.contains "set" clauseKinds || Set.contains "open" clauseKinds
                | Some _ ->
                    true
                | None ->
                    false

            let duplicatePathDiagnostics =
                ordinaryFields
                |> List.countBy pathKey
                |> List.choose (fun (path, count) ->
                    if count > 1 then
                        Some(makeDiagnostic DiagnosticCode.RecordPatchDuplicatePath $"Record patch updates path '{path}' more than once.")
                    else
                        None)

            let prefixDiagnostics =
                ordinaryFields
                |> List.allPairs ordinaryFields
                |> List.choose (fun (left, right) ->
                    if isStrictPrefix left.Path right.Path then
                        Some(
                            makeDiagnostic
                                DiagnosticCode.RecordPatchPrefixConflict
                                $"Record patch path '{pathKey left}' is a strict prefix of '{pathKey right}'."
                        )
                    else
                        None)
                |> List.distinctBy (fun diagnostic -> diagnostic.Message)

            let duplicateExtensionDiagnostics =
                extensionFields
                |> List.countBy (fun field -> field.Name)
                |> List.choose (fun (name, count) ->
                    if count > 1 then
                        Some(makeDiagnostic DiagnosticCode.RowExtensionDuplicateLabel $"Row extension label '{name}' appears more than once.")
                    else
                        None)

            let receiverRecordInfo = tryRecordInfoForExpression locals receiver

            let extensionDiagnostics =
                match receiverRecordInfo with
                | Some recordInfo ->
                    let explicitFields = recordSurfaceFieldNames recordInfo

                    extensionFields
                    |> List.choose (fun field ->
                        if Set.contains field.Name explicitFields then
                            Some(makeDiagnostic DiagnosticCode.RowExtensionExistingField $"Row extension label '{field.Name}' already exists in the receiver record.")
                        else
                            match recordInfo.RowTail with
                            | Some rowName when not (hasLacksConstraint rowName field.Name) ->
                                Some(
                                    makeDiagnostic
                                        DiagnosticCode.RowExtensionMissingLacksConstraint
                                        $"Row extension label '{field.Name}' for row '{rowName}' requires a matching LacksRec constraint."
                                )
                            | _ ->
                                None)
                | None ->
                    []

            let unknownPathDiagnostics =
                let rec validatePath (currentRecordInfo: RecordSurfaceInfo) (path: SurfaceRecordUpdatePathSegment list) =
                    match path with
                    | [] ->
                        []
                    | segment :: rest ->
                        match currentRecordInfo.Fields |> List.tryFind (fun (field: RecordSurfaceFieldInfo) -> String.Equals(field.Name, segment.Name, StringComparison.Ordinal)) with
                        | None ->
                            [ makeDiagnostic DiagnosticCode.RecordPatchUnknownPath $"Record patch path contains unknown field '{segment.Name}'." ]
                        | Some field when List.isEmpty rest ->
                            []
                        | Some field ->
                            match tryRecordInfoFromTypeTokens field.TypeTokens with
                            | Some nestedRecordInfo -> validatePath nestedRecordInfo rest
                            | None ->
                                [ makeDiagnostic DiagnosticCode.RecordPatchUnknownPath $"Record patch path continues through non-record field '{field.Name}'." ]

                match receiverRecordInfo with
                | Some recordInfo ->
                    ordinaryFields
                    |> List.collect (fun field ->
                        match projectionPatchInfo field with
                        | Some _ -> []
                        | None -> validatePath recordInfo field.Path)
                | None ->
                    []

            let projectionPatchDiagnostics =
                ordinaryFields
                |> List.choose (fun field ->
                    projectionPatchInfo field
                    |> Option.bind (fun projectionInfo ->
                        if projectionSupportsSet projectionInfo then
                            None
                        else
                            Some(makeDiagnostic DiagnosticCode.ProjectionDefinitionUnsupported "Projection-section update requires an accessor/setter or selector projection.")))

            duplicatePathDiagnostics
            @ prefixDiagnostics
            @ duplicateExtensionDiagnostics
            @ extensionDiagnostics
            @ unknownPathDiagnostics
            @ projectionPatchDiagnostics

        let validateRecordLiteral (fields: SurfaceRecordLiteralField list) =
            let fieldNames = fields |> List.map (fun (field: SurfaceRecordLiteralField) -> field.Name) |> Set.ofList

            let dependencyMap =
                fields
                |> List.map (fun (field: SurfaceRecordLiteralField) ->
                    field.Name,
                    expressionThisReferences field.Value
                    |> List.filter (fun name -> Set.contains name fieldNames && not (String.Equals(name, field.Name, StringComparison.Ordinal)))
                    |> Set.ofList)
                |> Map.ofList

            if hasCycle dependencyMap then
                [ makeDiagnostic DiagnosticCode.RecordDependencyCycle "Record literal field dependencies must be acyclic." ]
            else
                []

        let recordInfoHasOpaqueMembers (recordInfo: RecordSurfaceInfo) =
            recordInfo.Fields |> List.exists (fun (field: RecordSurfaceFieldInfo) -> field.IsOpaque)

        let compactTokenText tokens =
            tokens
            |> significantTokens
            |> List.map (fun token -> token.Text)
            |> String.concat ""

        let fieldMentionsOpaqueMember (recordInfo: RecordSurfaceInfo) (field: RecordSurfaceFieldInfo) =
            let opaqueNames =
                recordInfo.Fields
                |> List.filter (fun (item: RecordSurfaceFieldInfo) -> item.IsOpaque)
                |> List.map (fun item -> item.Name)
                |> Set.ofList

            let compactFieldType = compactTokenText field.TypeTokens

            (fieldTypeReferences recordInfo field.TypeTokens
             |> List.exists (fun name -> Set.contains name opaqueNames))
            || (opaqueNames
                |> Set.exists (fun opaqueName -> compactFieldType.Contains($"this.{opaqueName}", StringComparison.Ordinal)))

        let expectedTypeCompactText =
            definition.ReturnTypeTokens
            |> Option.map compactTokenText
            |> Option.orElseWith (fun () ->
                scheme
                |> Option.map (fun scheme -> TypeSignatures.toText scheme.Body)
                |> Option.map (fun text -> text.Replace(" ", "")))

        let isTransparentCompileTimeField (field: RecordSurfaceFieldInfo) =
            not field.IsOpaque
            &&
                match significantTokens field.TypeTokens with
                | [ typeToken ] when Token.isName typeToken ->
                    let typeName = SyntaxFacts.trimIdentifierQuotes typeToken.Text
                    List.contains typeName [ "Type"; "Constraint"; "Quantity"; "Region"; "RecRow"; "Label" ]
                | _ ->
                    false

        let expressionOpaqueUnfoldingDiagnostic locals expression =
            let expectedText = expectedTypeCompactText |> Option.defaultValue ""

            let checkRootMember root memberName =
                tryRecordInfoForRoot locals root
                |> Option.bind (fun recordInfo ->
                    recordInfo.Fields
                    |> List.tryFind (fun (field: RecordSurfaceFieldInfo) -> String.Equals(field.Name, memberName, StringComparison.Ordinal))
                    |> Option.bind (fun field ->
                        if fieldMentionsOpaqueMember recordInfo field then
                            let opaqueNames =
                                recordInfo.Fields
                                |> List.filter (fun (item: RecordSurfaceFieldInfo) -> item.IsOpaque)
                                |> List.map (fun item -> $"{root}.{item.Name}")

                            if opaqueNames |> List.exists (fun opaqueName -> expectedText.Contains(opaqueName, StringComparison.Ordinal)) then
                                None
                            else
                                Some(makeDiagnostic DiagnosticCode.SealOpaqueUnfolding $"Projection '{root}.{memberName}' would expose a member whose type depends on an opaque package member.")
                        else
                            None))

            match expression with
            | Name(root :: memberName :: _) ->
                checkRootMember root memberName
            | Apply(Name(root :: memberName :: _), _) ->
                checkRootMember root memberName
            | _ ->
                None

        let validateExpectedBody locals body =
            let dropDefinitionParameters typeExpr =
                let rec loop remaining current =
                    if remaining <= 0 then
                        current
                    else
                        match current with
                        | TypeArrow(_, _, resultType) -> loop (remaining - 1) resultType
                        | _ -> current

                loop definition.Parameters.Length typeExpr

            let expectedBodyType =
                definition.ReturnTypeTokens
                |> Option.bind TypeSignatures.parseType
                |> Option.orElseWith (fun () ->
                    scheme
                    |> Option.map (fun scheme -> dropDefinitionParameters scheme.Body))

            let directSignatureLiteralDiagnostic =
                match body, definition.ReturnTypeTokens |> Option.bind tryRecordInfoFromTypeTokens with
                | RecordLiteral _, Some recordInfo when recordInfoHasOpaqueMembers recordInfo ->
                    [ makeDiagnostic DiagnosticCode.SealDirectLiteralForSignature "A record literal cannot be checked directly against a signature type; use 'seal ... as ...'." ]
                | _ ->
                    []

            let sealAscriptionDiagnostics =
                let rec loop expression =
                    match expression with
                    | Seal(value, ascriptionTokens) ->
                        let nested = loop value

                        match tryParseRecordSurfaceInfo ascriptionTokens with
                        | Some recordInfo when recordInfo.RowTail.IsSome ->
                            makeDiagnostic DiagnosticCode.SealOpenRecordAscription "The ascribed type of 'seal' must be a closed record type." :: nested
                        | _ ->
                            nested
                    | LocalLet(_, value, nestedBody) -> loop value @ loop nestedBody
                    | LocalSignature(_, nestedBody) -> loop nestedBody
                    | LocalTypeAlias(_, nestedBody) -> loop nestedBody
                    | LocalScopedEffect(_, nestedBody) -> loop nestedBody
                    | Handle(_, label, nestedBody, returnClause, operationClauses) ->
                        loop label
                        @ loop nestedBody
                        @ loop returnClause.Body
                        @ (operationClauses |> List.collect (fun clause -> loop clause.Body))
                    | Lambda(_, nestedBody) -> loop nestedBody
                    | SyntaxQuote inner
                    | SyntaxSplice inner
                    | TopLevelSyntaxSplice inner
                    | CodeQuote inner
                    | CodeSplice inner -> loop inner
                    | IfThenElse(condition, whenTrue, whenFalse) -> loop condition @ loop whenTrue @ loop whenFalse
                    | Match(scrutinee, cases) ->
                        loop scrutinee
                        @ (cases
                           |> List.collect (fun caseClause ->
                               (caseClause.Guard |> Option.map loop |> Option.defaultValue [])
                               @ loop caseClause.Body))
                    | RecordLiteral fields -> fields |> List.collect (fun field -> loop field.Value)
                    | RecordUpdate(receiver, fields) -> loop receiver @ (fields |> List.collect (fun field -> loop field.Value))
                    | MemberAccess(receiver, _, arguments) -> loop receiver @ (arguments |> List.collect loop)
                    | SafeNavigation(receiver, navigation) -> loop receiver @ (navigation.Arguments |> List.collect loop)
                    | TagTest(receiver, _) -> loop receiver
                    | MonadicSplice inner
                    | ExplicitImplicitArgument inner
                    | InoutArgument inner
                    | Unary(_, inner) -> loop inner
                    | NamedApplicationBlock fields -> fields |> List.collect (fun field -> loop field.Value)
                    | Apply(callee, arguments) -> loop callee @ (arguments |> List.collect loop)
                    | Binary(left, _, right)
                    | Elvis(left, right) -> loop left @ loop right
                    | PrefixedString(_, parts) ->
                        parts
                        |> List.collect (function
                            | StringInterpolation(inner, _) -> loop inner
                            | StringText _ -> [])
                    | Literal _
                    | NumericLiteral _
                    | KindQualifiedName _
                    | Name _
                    | Do _ -> []

                loop body

            let opaqueUnfoldingDiagnostics =
                expressionOpaqueUnfoldingDiagnostic locals body |> Option.toList

            let projectionTypeMismatchDiagnostics =
                match body, expectedTypeCompactText with
                | Name(root :: memberName :: _), Some expectedText ->
                    match tryRecordInfoForRoot locals root with
                    | Some recordInfo ->
                        match recordInfo.Fields |> List.tryFind (fun (field: RecordSurfaceFieldInfo) -> String.Equals(field.Name, memberName, StringComparison.Ordinal)) with
                        | Some field ->
                            if fieldMentionsOpaqueMember recordInfo field then
                                []
                            else
                                let unstableDependencies =
                                    fieldTypeReferences recordInfo field.TypeTokens
                                    |> List.choose (fun dependencyName ->
                                        recordInfo.Fields
                                        |> List.tryFind (fun candidate -> String.Equals(candidate.Name, dependencyName, StringComparison.Ordinal))
                                        |> Option.filter (isTransparentCompileTimeField >> not)
                                        |> Option.map (fun _ -> dependencyName))

                                if
                                    unstableDependencies
                                    |> List.exists (fun dependencyName -> expectedText.Contains($"{root}.{dependencyName}", StringComparison.Ordinal) |> not)
                                then
                                    [ makeDiagnostic DiagnosticCode.TypeEqualityMismatch "Projected field type does not match the declared result type." ]
                                else
                                    []
                        | None ->
                            []
                    | None ->
                        []
                | _ ->
                    []

            let staticObjectResultDiagnostics =
                match expectedBodyType, inferValidationExpressionType environment freshCounter locals body with
                | Some expectedType, Some actualType
                    when isReifiedStaticObjectType environment.VisibleTypeAliases actualType
                         && not (expectedTypeAccepts locals Map.empty expectedType actualType) ->
                    expectedTypeDiagnostics locals Map.empty "Definition body" expectedType body
                | _ ->
                    []

            let constructorAsStaticObjectDiagnostics =
                match expectedBodyType, body with
                | Some expectedType, Name [ root ]
                    when isVisibleConstructorName root
                         && TypeSignatures.definitionallyEqual
                             (normalizeTypeAliases environment.VisibleTypeAliases expectedType)
                             typeObjectType ->
                    [ makeDiagnostic DiagnosticCode.TypeEqualityMismatch "A data constructor term cannot satisfy a Type-valued definition body; use an explicit kind-qualified type object when the type facet is intended." ]
                | _ ->
                    []

            let expectedUnionDiagnostics =
                match expectedBodyType with
                | Some expectedType when typeContainsUnion expectedType ->
                    expectedTypeDiagnostics locals Map.empty "Definition body" expectedType body
                | _ ->
                    []

            let contextualNumericBodyDiagnostics =
                match expectedBodyType, body with
                | Some expectedType, (NumericLiteral _ | Unary("-", NumericLiteral _)) ->
                    expectedTypeDiagnostics locals Map.empty "Definition body" expectedType body
                | Some _, _ ->
                    []
                | None, _ ->
                    []

            directSignatureLiteralDiagnostic
            @ sealAscriptionDiagnostics
            @ opaqueUnfoldingDiagnostics
            @ projectionTypeMismatchDiagnostics
            @ staticObjectResultDiagnostics
            @ constructorAsStaticObjectDiagnostics
            @ expectedUnionDiagnostics
            @ contextualNumericBodyDiagnostics

        let applicationExpectedArgumentDiagnostics locals refinements (bindingInfo: BindingSchemeInfo) arguments =
            let inferArgumentTypeForParameter parameterType argument =
                tryInferNumericExpressionTypeFromContext environment parameterType argument
                |> Option.orElseWith (fun () -> inferValidationExpressionType environment freshCounter locals argument)

            match bindingInfo.ParameterLayouts with
            | None ->
                []
            | Some parameterLayouts ->
                let instantiated = TypeSignatures.instantiate "v" freshCounter.Value bindingInfo.Scheme
                let schemeParameterTypes, _ = TypeSignatures.schemeParts instantiated

                let alignParameterTypesWithLayouts layouts parameterTypes =
                    let remaining = ref parameterTypes
                    let aligned = ResizeArray<TypeExpr>()
                    let mutable success = true

                    for layout in layouts do
                        match tryParseParameterType layout with
                        | Some layoutType when isCompileTimeArgumentType environment.VisibleTypeAliases layoutType ->
                            aligned.Add(layoutType)

                            match !remaining with
                            | parameterType :: rest
                                when TypeSignatures.definitionallyEqual
                                         (normalizeTypeAliases environment.VisibleTypeAliases layoutType)
                                         (normalizeTypeAliases environment.VisibleTypeAliases parameterType) ->
                                remaining := rest
                            | _ ->
                                ()
                        | _ ->
                            match !remaining with
                            | parameterType :: rest ->
                                aligned.Add(parameterType)
                                remaining := rest
                            | [] ->
                                success <- false

                    if success && List.isEmpty !remaining then
                        Some(List.ofSeq aligned)
                    else
                        None

                let hasApplicationOnlyArgument =
                    arguments
                    |> List.exists (function
                        | ExplicitImplicitArgument _
                        | NamedApplicationBlock _ -> true
                        | _ -> false)

                if hasApplicationOnlyArgument then
                    []
                else
                    match alignParameterTypesWithLayouts parameterLayouts schemeParameterTypes with
                    | None ->
                        []
                    | Some parameterTypes ->
                        let remainingArguments = ref arguments
                        let substitution = ref Map.empty<string, TypeExpr>
                        let diagnostics = ResizeArray<Diagnostic>()

                        for parameterLayout, rawParameterType in List.zip parameterLayouts parameterTypes do
                            let parameterType =
                                rawParameterType
                                |> TypeSignatures.applySubstitution !substitution

                            if parameterLayout.IsImplicit then
                                ()
                            elif isCompileTimeArgumentType environment.VisibleTypeAliases parameterType then
                                match !remainingArguments with
                                | nextArgument :: rest ->
                                    remainingArguments := rest

                                    match tryParseTypeLikeSurfaceExpression nextArgument with
                                    | Some typeArgument ->
                                        substitution :=
                                            composeTypeSubstitution
                                                !substitution
                                                (Map.ofList [ parameterLayout.Name, typeArgument ])
                                    | None ->
                                        ()
                                | [] ->
                                    ()
                            else
                                match !remainingArguments with
                                | nextArgument :: rest ->
                                    remainingArguments := rest

                                    match inferArgumentTypeForParameter parameterType nextArgument with
                                    | Some argumentType ->
                                        match tryNumericLiteralRangeDiagnostic parameterType nextArgument with
                                        | Some diagnostic ->
                                            diagnostics.Add(diagnostic)
                                        | None when not (expectedTypeAccepts locals refinements parameterType argumentType) ->
                                            diagnostics.Add(
                                                expectedMismatchDiagnostic
                                                    locals
                                                    refinements
                                                    "Application argument"
                                                    parameterType
                                                    argumentType
                                            )
                                        | None ->
                                            match tryParseTypeLikeSurfaceExpression nextArgument with
                                            | Some termArgument ->
                                                substitution :=
                                                    composeTypeSubstitution
                                                        !substitution
                                                        (Map.ofList [ parameterLayout.Name, termArgument ])
                                            | None ->
                                                match tryUnifyVisibleTypes environment.VisibleTypeAliases [ parameterType, argumentType ] with
                                                | Some inferredSubstitution ->
                                                    substitution :=
                                                        composeTypeSubstitution
                                                            !substitution
                                                            inferredSubstitution
                                                | None ->
                                                    ()
                                    | None ->
                                        ()
                                | [] ->
                                    ()

                        List.ofSeq diagnostics

        let rec validateExpression locals refinements lexicalNames expression =
            let recurse = validateExpression locals refinements lexicalNames

            let simpleClauseBinderName (tokens: Token list) =
                match tokens |> List.filter (fun token -> token.Kind <> Newline && token.Kind <> Indent && token.Kind <> Dedent) with
                | [ token ] when Token.isName token ->
                    Some(SyntaxFacts.trimIdentifierQuotes token.Text)
                | _ ->
                    None

            let clauseBoundNames (clause: SurfaceEffectHandlerClause) =
                let argumentNames =
                    clause.ArgumentTokens
                    |> List.choose simpleClauseBinderName

                match clause.ResumptionName with
                | Some name -> name :: argumentNames
                | None -> argumentNames
                |> Set.ofList

            let rec expressionReferencesName target shadowed current =
                let recurseName = expressionReferencesName target shadowed

                match current with
                | Name [ name ] ->
                    String.Equals(name, target, StringComparison.Ordinal)
                    && not (Set.contains name shadowed)
                | SyntaxQuote inner
                | SyntaxSplice inner
                | TopLevelSyntaxSplice inner
                | CodeQuote inner
                | CodeSplice inner
                | MonadicSplice inner
                | ExplicitImplicitArgument inner
                | InoutArgument inner
                | Unary(_, inner) ->
                    recurseName inner
                | LocalLet(binding, value, body) ->
                    recurseName value
                    || expressionReferencesName target (Set.union shadowed (collectPatternNames binding.Pattern |> Set.ofList)) body
                | LocalSignature(declaration, body) ->
                    if String.Equals(declaration.Name, target, StringComparison.Ordinal) then
                        recurseName body
                    else
                        expressionReferencesName target (Set.add declaration.Name shadowed) body
                | LocalTypeAlias(_, body) ->
                    recurseName body
                | LocalScopedEffect(declaration, body) ->
                    if String.Equals(declaration.Name, target, StringComparison.Ordinal) then
                        false
                    else
                        recurseName body
                | Lambda(parameters, body) ->
                    let shadowedParameters =
                        parameters |> List.map (fun parameter -> parameter.Name) |> Set.ofList |> Set.union shadowed

                    expressionReferencesName target shadowedParameters body
                | Handle(_, label, body, returnClause, operationClauses) ->
                    recurseName label
                    || recurseName body
                    || expressionReferencesName target (Set.union shadowed (clauseBoundNames returnClause)) returnClause.Body
                    || (operationClauses
                        |> List.exists (fun clause ->
                            expressionReferencesName target (Set.union shadowed (clauseBoundNames clause)) clause.Body))
                | IfThenElse(condition, whenTrue, whenFalse) ->
                    recurseName condition || recurseName whenTrue || recurseName whenFalse
                | Match(scrutinee, cases) ->
                    recurseName scrutinee
                    || (cases
                        |> List.exists (fun caseClause ->
                            let caseShadowed = Set.union shadowed (collectPatternNames caseClause.Pattern |> Set.ofList)
                            let guardUses =
                                caseClause.Guard
                                |> Option.exists (expressionReferencesName target caseShadowed)

                            guardUses || expressionReferencesName target caseShadowed caseClause.Body))
                | RecordLiteral fields ->
                    fields |> List.exists (fun field -> recurseName field.Value)
                | Seal(value, _) ->
                    recurseName value
                | RecordUpdate(receiver, fields) ->
                    recurseName receiver || (fields |> List.exists (fun field -> recurseName field.Value))
                | MemberAccess(receiver, _, arguments) ->
                    recurseName receiver || (arguments |> List.exists recurseName)
                | SafeNavigation(receiver, navigation) ->
                    recurseName receiver || (navigation.Arguments |> List.exists recurseName)
                | TagTest(receiver, _) ->
                    recurseName receiver
                | Do statements ->
                    statements
                    |> List.exists (function
                        | DoLet(binding, value)
                        | DoBind(binding, value)
                        | DoUsing(binding, value) ->
                            recurseName value
                            || expressionReferencesName target (Set.union shadowed (collectPatternNames binding.Pattern |> Set.ofList)) (Literal LiteralValue.Unit)
                        | DoLetQuestion(binding, value, failure) ->
                            recurseName value
                            || (failure
                                |> Option.exists (fun failure ->
                                    let failureShadowed =
                                        Set.union shadowed (collectPatternNames failure.ResiduePattern.Pattern |> Set.ofList)

                                    failure.Body
                                    |> List.exists (function
                                        | DoExpression inner
                                        | DoReturn inner
                                        | DoDefer inner
                                        | DoVar(_, inner)
                                        | DoAssign(_, inner) -> expressionReferencesName target failureShadowed inner
                                        | _ -> false)))
                            || expressionReferencesName target (Set.union shadowed (collectPatternNames binding.Pattern |> Set.ofList)) (Literal LiteralValue.Unit)
                        | DoVar(_, value)
                        | DoAssign(_, value)
                        | DoDefer value
                        | DoExpression value
                        | DoReturn value -> recurseName value
                        | DoIf(condition, whenTrue, whenFalse) ->
                            recurseName condition
                            || (whenTrue |> List.exists (function DoExpression inner | DoReturn inner -> recurseName inner | _ -> false))
                            || (whenFalse |> List.exists (function DoExpression inner | DoReturn inner -> recurseName inner | _ -> false))
                        | DoWhile(condition, body) ->
                            recurseName condition
                            || (body |> List.exists (function DoExpression inner | DoReturn inner -> recurseName inner | _ -> false)))
                | Apply(callee, arguments) ->
                    recurseName callee || (arguments |> List.exists recurseName)
                | NamedApplicationBlock fields ->
                    fields |> List.exists (fun field -> recurseName field.Value)
                | Binary(left, _, right)
                | Elvis(left, right) ->
                    recurseName left || recurseName right
                | PrefixedString(_, parts) ->
                    parts
                    |> List.exists (function
                        | StringText _ -> false
                        | StringInterpolation(inner, _) -> recurseName inner)
                | Literal _
                | NumericLiteral _
                | KindQualifiedName _
                | Name _ ->
                    false

            match expression with
            | SyntaxQuote inner
            | SyntaxSplice inner
            | TopLevelSyntaxSplice inner
            | CodeQuote inner
            | CodeSplice inner ->
                recurse inner
            | Handle(_, label, body, returnClause, operationClauses) ->
                let returnClauseLexicalNames =
                    Set.union lexicalNames (clauseBoundNames returnClause)

                let handlerDiagnostics =
                    match label with
                    | Name [ effectName ] ->
                        match tryFindScopedEffectDeclaration effectName with
                        | Some declaration ->
                            operationClauses
                            |> List.collect (fun clause ->
                                match
                                    declaration.Operations
                                    |> List.tryFind (fun operation -> String.Equals(operation.Name, clause.OperationName, StringComparison.Ordinal))
                                with
                                | Some operation ->
                                    match operation.ResumptionQuantity, clause.ResumptionName with
                                    | Some QuantityZero, _
                                    | Some QuantityAtMostOne, _
                                    | None, _ ->
                                        []
                                    | Some _, Some resumptionName
                                        when not (expressionReferencesName resumptionName Set.empty clause.Body) ->
                                        [
                                            makeDiagnostic
                                                DiagnosticCode.QttLinearDrop
                                                $"Handler clause for operation '{clause.OperationName}' must use relevant resumption '{resumptionName}'."
                                        ]
                                    | Some _, _ ->
                                        []
                                | None ->
                                    []
                                )
                        | None ->
                            []
                    | _ ->
                        []

                recurse label
                @ recurse body
                @ validateExpression locals refinements returnClauseLexicalNames returnClause.Body
                @ (operationClauses
                   |> List.collect (fun clause ->
                       let clauseLexicalNames = Set.union lexicalNames (clauseBoundNames clause)
                       validateExpression locals refinements clauseLexicalNames clause.Body))
                @ handlerDiagnostics
            | Literal _ ->
                []
            | NumericLiteral _ ->
                []
            | KindQualifiedName(kind, nameSegments) ->
                match tryResolveScopedStaticObject environment expression with
                | Some _ ->
                    []
                | None ->
                    let kindText = kindSelectorText kind
                    let nameText = SyntaxFacts.moduleNameToText nameSegments

                    [
                        makeDiagnostic
                            DiagnosticCode.StaticObjectUnresolved
                            $"No {kindText} static object named '{nameText}' is visible."
                    ]
            | Name(root :: fieldName :: _) ->
                let missingFieldDiagnostics =
                    match tryReceiverProjection environment locals root fieldName with
                    | Some _ ->
                        []
                    | None ->
                        match tryRecordInfoForRoot locals root with
                        | Some recordInfo
                            when recordInfo.Fields
                                 |> List.exists (fun (field: RecordSurfaceFieldInfo) -> String.Equals(field.Name, fieldName, StringComparison.Ordinal))
                                 |> not ->
                            [ makeDiagnostic DiagnosticCode.RecordProjectionMissingField $"Record type has no field named '{fieldName}'." ]
                        | _ ->
                            []
                let staticMemberDiagnostics =
                    match expression with
                    | Name segments ->
                        match List.rev segments with
                        | memberName :: receiverSegments when not (List.isEmpty receiverSegments) ->
                            staticConstructorIdentityDiagnostic locals (Name(List.rev receiverSegments)) memberName
                        | _ ->
                            []
                    | _ ->
                        []

                missingFieldDiagnostics @ staticMemberDiagnostics
            | Name [ name ]
                when allowUnresolvedCallDiagnostics
                     && not (Map.containsKey name locals)
                     && not (Set.contains name lexicalNames)
                     && not (Set.contains name knownSurfaceTermNames)
                     && not (Set.contains name knownValueNames) ->
                [ makeDiagnostic DiagnosticCode.NameUnresolved $"Name '{name}' is not in scope." ]
            | Name _ ->
                []
            | LocalLet(binding, value, body) ->
                let nextLocals =
                    match inferValidationExpressionType environment freshCounter locals value with
                    | Some valueType ->
                        extendBindingLocalTypes environment freshCounter locals binding (Some valueType)
                    | None ->
                        locals

                let rec rewriteNamedAliasUse aliasName targetName expression =
                    let rewrite = rewriteNamedAliasUse aliasName targetName

                    match expression with
                    | SyntaxQuote inner ->
                        SyntaxQuote(rewrite inner)
                    | SyntaxSplice inner ->
                        SyntaxSplice(rewrite inner)
                    | TopLevelSyntaxSplice inner ->
                        TopLevelSyntaxSplice(rewrite inner)
                    | CodeQuote inner ->
                        CodeQuote(rewrite inner)
                    | CodeSplice inner ->
                        CodeSplice(rewrite inner)
                    | Handle(isDeep, label, body, returnClause, operationClauses) ->
                        let rewriteClause (clause: SurfaceEffectHandlerClause) =
                            { clause with
                                Body = rewrite clause.Body }

                        Handle(
                            isDeep,
                            rewrite label,
                            rewrite body,
                            rewriteClause returnClause,
                            operationClauses |> List.map rewriteClause
                        )
                    | Apply(Name [ name ], arguments) when String.Equals(name, aliasName, StringComparison.Ordinal) ->
                        Apply(Name [ targetName ], arguments |> List.map rewrite)
                    | Apply(callee, arguments) ->
                        Apply(rewrite callee, arguments |> List.map rewrite)
                    | LocalLet(nestedBinding, nestedValue, nestedBody) ->
                        let shadowsAlias =
                            collectPatternNames nestedBinding.Pattern
                            |> List.exists (fun name -> String.Equals(name, aliasName, StringComparison.Ordinal))

                        LocalLet(
                            nestedBinding,
                            rewrite nestedValue,
                            if shadowsAlias then nestedBody else rewrite nestedBody
                        )
                    | LocalSignature(declaration, nestedBody) ->
                        if String.Equals(declaration.Name, aliasName, StringComparison.Ordinal) then
                            expression
                        else
                            LocalSignature(declaration, rewrite nestedBody)
                    | LocalTypeAlias(declaration, nestedBody) ->
                        LocalTypeAlias(declaration, rewrite nestedBody)
                    | LocalScopedEffect(declaration, nestedBody) ->
                        if String.Equals(declaration.Name, aliasName, StringComparison.Ordinal) then
                            expression
                        else
                            LocalScopedEffect(declaration, rewrite nestedBody)
                    | Lambda(parameters, nestedBody) ->
                        if parameters |> List.exists (fun parameter -> String.Equals(parameter.Name, aliasName, StringComparison.Ordinal)) then
                            expression
                        else
                            Lambda(parameters, rewrite nestedBody)
                    | IfThenElse(condition, whenTrue, whenFalse) ->
                        IfThenElse(rewrite condition, rewrite whenTrue, rewrite whenFalse)
                    | Match(scrutinee, cases) ->
                        Match(
                            rewrite scrutinee,
                            cases
                            |> List.map (fun caseClause ->
                                { caseClause with
                                    Guard = caseClause.Guard |> Option.map rewrite
                                    Body = rewrite caseClause.Body })
                        )
                    | RecordLiteral fields ->
                        RecordLiteral(fields |> List.map (fun field -> { field with Value = rewrite field.Value }))
                    | Seal(nestedValue, ascriptionTokens) ->
                        Seal(rewrite nestedValue, ascriptionTokens)
                    | RecordUpdate(receiver, fields) ->
                        RecordUpdate(rewrite receiver, fields |> List.map (fun field -> { field with Value = rewrite field.Value }))
                    | MemberAccess(receiver, segments, arguments) ->
                        MemberAccess(rewrite receiver, segments, arguments |> List.map rewrite)
                    | SafeNavigation(receiver, navigation) ->
                        SafeNavigation(rewrite receiver, { navigation with Arguments = navigation.Arguments |> List.map rewrite })
                    | TagTest(receiver, constructorName) ->
                        TagTest(rewrite receiver, constructorName)
                    | Do statements ->
                        Do statements
                    | MonadicSplice inner ->
                        MonadicSplice(rewrite inner)
                    | ExplicitImplicitArgument inner ->
                        ExplicitImplicitArgument(rewrite inner)
                    | NamedApplicationBlock fields ->
                        NamedApplicationBlock(fields |> List.map (fun field -> { field with Value = rewrite field.Value }))
                    | InoutArgument inner ->
                        InoutArgument(rewrite inner)
                    | Unary(operatorName, inner) ->
                        Unary(operatorName, rewrite inner)
                    | Binary(left, operatorName, right) ->
                        Binary(rewrite left, operatorName, rewrite right)
                    | Elvis(left, right) ->
                        Elvis(rewrite left, rewrite right)
                    | PrefixedString(prefix, parts) ->
                        PrefixedString(
                            prefix,
                            parts
                            |> List.map (function
                                | StringText text -> StringText text
                                | StringInterpolation(inner, format) -> StringInterpolation(rewrite inner, format))
                        )
                    | Literal _
                    | NumericLiteral _
                    | KindQualifiedName _
                    | Name _ ->
                        expression

                let bodyForValidation =
                    match binding.Pattern, value with
                    | NamePattern aliasName, Name [ targetName ]
                        when (environment.VisibleBindings
                              |> Map.tryFind targetName
                              |> Option.orElseWith (fun () -> environment.VisibleConstructors |> Map.tryFind targetName))
                             |> Option.bind (fun bindingInfo -> bindingInfo.ParameterLayouts)
                             |> Option.isSome ->
                        rewriteNamedAliasUse aliasName targetName body
                    | NamePattern aliasName, Lambda(parameters, _) ->
                        rewriteNamedApplicationAliasUse aliasName parameters body
                    | _ ->
                        body

                let localStaticAliases = collectPatternStaticAliases binding.Pattern value Map.empty
                let bodyForValidation = rewriteStaticObjectPathAliasUses localStaticAliases bodyForValidation

                let nextLexicalNames =
                    collectPatternNames binding.Pattern
                    |> Set.ofList
                    |> Set.union lexicalNames

                recurse value @ validateExpression nextLocals refinements nextLexicalNames bodyForValidation
            | LocalSignature(declaration, body) ->
                let nextLocals =
                    declaration.TypeTokens
                    |> TypeSignatures.parseType
                    |> Option.map (fun declarationType -> Map.add declaration.Name declarationType locals)
                    |> Option.defaultValue locals

                validateExpression nextLocals refinements (Set.add declaration.Name lexicalNames) body
            | LocalTypeAlias(declaration, body) ->
                validateExpression locals refinements (Set.add declaration.Name lexicalNames) body
            | LocalScopedEffect(declaration, body) ->
                withScopedEffectDeclaration declaration (fun () ->
                    validateExpression locals refinements (Set.add declaration.Name lexicalNames) body)
            | Lambda(parameters, body) ->
                let nextLocals =
                    parameters
                    |> List.fold (fun state parameter ->
                        match tryParseParameterType parameter with
                        | Some parameterType -> Map.add parameter.Name parameterType state
                        | None -> state) locals

                let nextLexicalNames =
                    parameters
                    |> List.map (fun parameter -> parameter.Name)
                    |> Set.ofList
                    |> Set.union lexicalNames

                validateExpression nextLocals refinements nextLexicalNames body
            | IfThenElse(condition, whenTrue, whenFalse) ->
                let trueRefinements, falseRefinements =
                    match condition with
                    | Name [ conditionName ] when locals |> Map.containsKey conditionName ->
                        Map.add conditionName trueTermType refinements,
                        Map.add conditionName falseTermType refinements
                    | Unary("not", Name [ conditionName ]) when locals |> Map.containsKey conditionName ->
                        Map.add conditionName falseTermType refinements,
                        Map.add conditionName trueTermType refinements
                    | _ ->
                        refinements, refinements

                recurse condition
                @ validateExpression locals trueRefinements lexicalNames whenTrue
                @ validateExpression locals falseRefinements lexicalNames whenFalse
            | Match(scrutinee, cases) ->
                let caseDiagnostics =
                    cases
                    |> List.collect (fun caseClause ->
                        let caseLexicalNames =
                            collectPatternNames caseClause.Pattern
                            |> Set.ofList
                            |> Set.union lexicalNames

                        validatePatternHead caseClause.Pattern
                        @ staticPatternIdentityDiagnostics locals caseClause.Pattern
                        @ activePatternLinearityDiagnostics caseClause.Pattern "match case"
                        @ (caseClause.Guard
                           |> Option.map (validateExpression locals refinements caseLexicalNames)
                           |> Option.defaultValue [])
                        @ validateExpression locals refinements caseLexicalNames caseClause.Body)

                recurse scrutinee @ caseDiagnostics
            | RecordLiteral fields ->
                validateRecordLiteral fields @ (fields |> List.collect (fun field -> recurse field.Value))
            | Seal(value, _) ->
                recurse value
            | RecordUpdate(receiver, fields) ->
                let fieldExpectedDiagnostics =
                    match tryRecordInfoForExpression locals receiver with
                    | Some recordInfo ->
                        fields
                        |> List.collect (fun field ->
                            match field.Path with
                            | [ pathSegment ] ->
                                recordInfo.Fields
                                |> List.tryFind (fun recordField ->
                                    String.Equals(recordField.Name, pathSegment.Name, StringComparison.Ordinal))
                                |> Option.bind (fun recordField ->
                                    recordField.TypeTokens |> TypeSignatures.parseType)
                                |> Option.filter (fun expectedType ->
                                    typeContainsSuspension expectedType || typeContainsUnion expectedType)
                                |> Option.map (fun expectedType ->
                                    expectedTypeDiagnostics
                                        locals
                                        refinements
                                        $"Record update field '{field.Name}'"
                                        expectedType
                                        field.Value)
                                |> Option.defaultValue []
                            | _ ->
                                [])
                    | None ->
                        []

                recurse receiver
                @ (fields |> List.collect (fun field -> recurse field.Value))
                @ validateRecordPatch locals receiver fields
                @ fieldExpectedDiagnostics
            | MemberAccess(receiver, segments, arguments) ->
                let memberDiagnostics =
                    match segments with
                    | [ memberName ] ->
                        let receiverStaticDiagnostics =
                            staticConstructorIdentityDiagnostic locals receiver memberName

                        let receiverMethodDiagnostics =
                            match environment.VisibleBindings |> Map.tryFind memberName |> Option.bind (fun info -> info.ParameterLayouts) with
                            | Some layouts ->
                                let receiverParameters =
                                    layouts
                                    |> List.filter (fun layout -> layout.IsReceiver)

                                let precedingExplicitCount =
                                    layouts
                                    |> List.takeWhile (fun layout -> not layout.IsReceiver)
                                    |> List.filter (fun layout -> not layout.IsImplicit)
                                    |> List.length

                                let requiredExplicitArgumentCount =
                                    layouts
                                    |> List.filter (fun layout -> not layout.IsImplicit)
                                    |> List.length
                                    |> fun count -> max 0 (count - 1)

                                if List.length receiverParameters <> 1 then
                                    [ makeDiagnostic DiagnosticCode.TypeEqualityMismatch $"Member-call sugar for '{memberName}' requires exactly one receiver binder." ]
                                elif List.length arguments < precedingExplicitCount then
                                    [ makeDiagnostic DiagnosticCode.TypeEqualityMismatch $"Member-call sugar for '{memberName}' is missing preceding explicit argument(s)." ]
                                elif List.length arguments < requiredExplicitArgumentCount then
                                    [ makeDiagnostic DiagnosticCode.TypeEqualityMismatch $"Member-call sugar for '{memberName}' is missing explicit argument(s)." ]
                                else
                                    []
                            | _ ->
                                []

                        receiverStaticDiagnostics @ receiverMethodDiagnostics
                    | _ ->
                        []

                recurse receiver @ (arguments |> List.collect recurse) @ memberDiagnostics
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
                validateDoStatements locals refinements lexicalNames statements
            | MonadicSplice inner
            | ExplicitImplicitArgument inner
            | InoutArgument inner
            | Unary(_, inner) ->
                recurse inner
            | NamedApplicationBlock fields ->
                fields |> List.collect (fun field -> recurse field.Value)
            | Apply(callee, arguments) ->
                let argumentDiagnostics =
                    (match callee with
                     | Name [ _ ] -> []
                     | _ -> recurse callee)
                    @ (arguments |> List.collect recurse)

                let hasNamedBlock =
                    arguments
                    |> List.exists (function
                        | NamedApplicationBlock _ -> true
                        | _ -> false)

                let hasExplicitImplicitArgument =
                    arguments
                    |> List.exists (function
                        | ExplicitImplicitArgument _ -> true
                        | _ -> false)

                let localCanSatisfyImplicitParameter (bindingInfo: BindingSchemeInfo) =
                    match bindingInfo.ParameterLayouts with
                    | Some layouts when layouts |> List.exists (fun parameter -> parameter.IsImplicit) ->
                        let parameterTypes, _ = TypeSignatures.schemeParts bindingInfo.Scheme

                        if List.length layouts = List.length parameterTypes then
                            List.zip layouts parameterTypes
                            |> List.exists (fun (layout, parameterType) ->
                                layout.IsImplicit
                                && not (isCompileTimeArgumentType environment.VisibleTypeAliases parameterType)
                                && (locals
                                    |> Map.exists (fun _ localType ->
                                        TypeSignatures.definitionallyEqual
                                            (normalizeTypeAliases environment.VisibleTypeAliases localType)
                                            (normalizeTypeAliases environment.VisibleTypeAliases parameterType))))
                        else
                            false
                    | _ ->
                        false

                let hasUnsatisfiedRuntimeImplicit (bindingInfo: BindingSchemeInfo) =
                    match bindingInfo.ParameterLayouts with
                    | Some layouts ->
                        let parameterTypes, _ = TypeSignatures.schemeParts bindingInfo.Scheme
                        let isTraitConstraintType parameterType =
                            match normalizeTypeAliases environment.VisibleTypeAliases parameterType with
                            | TypeName([ traitName ], _) ->
                                environment.VisibleTraits.ContainsKey(traitName)
                            | TypeName(qualifiedName, _) ->
                                environment.VisibleTraits.ContainsKey(SyntaxFacts.moduleNameToText qualifiedName)
                            | _ ->
                                false

                        List.length layouts = List.length parameterTypes
                        && (List.zip layouts parameterTypes
                            |> List.exists (fun (layout, parameterType) ->
                                layout.IsImplicit
                                && not (isCompileTimeArgumentType environment.VisibleTypeAliases parameterType)
                                && not (isTraitConstraintType parameterType)
                                && not (localCanSatisfyImplicitParameter bindingInfo)))
                    | None ->
                        false

                let hasTooManyArguments (bindingInfo: BindingSchemeInfo) =
                    match bindingInfo.ParameterLayouts with
                    | Some layouts ->
                        let visibleParameterCount =
                            layouts
                            |> List.filter (fun parameter -> not parameter.IsImplicit)
                            |> List.length

                        visibleParameterCount > 0 && arguments.Length > visibleParameterCount
                    | None ->
                        false

                let applicationDiagnostics =
                    match callee with
                    | Name [ calleeName ] ->
                        let bindingInfo =
                            environment.VisibleBindings
                            |> Map.tryFind calleeName
                            |> Option.orElseWith (fun () -> environment.VisibleConstructors |> Map.tryFind calleeName)

                        let resolvesAsTraitMemberCall () =
                            tryPrepareVisibleTraitMemberCall
                                environment
                                (ref freshCounter.Value)
                                calleeName
                                (inferValidationExpressionType environment (ref freshCounter.Value) locals)
                                arguments
                            |> Option.isSome

                        let isKnownVisibleCallee =
                            (Set.contains calleeName lexicalNames)
                            || (Set.contains calleeName knownSurfaceTermNames)
                            || environment.VisibleProjections.ContainsKey(calleeName)
                            || (environment.VisibleTraits |> Map.exists (fun _ traitInfo -> traitInfo.Members.ContainsKey(calleeName)))
                            || environment.ConstrainedMembers.ContainsKey(calleeName)
                            || resolvesAsTraitMemberCall ()

                        match bindingInfo with
                        | Some bindingInfo ->
                            let probeCounter = ref freshCounter.Value
                            let expectedArgumentDiagnostics =
                                applicationExpectedArgumentDiagnostics locals refinements bindingInfo arguments

                            match
                                tryPrepareVisibleBindingCall
                                    environment
                                    probeCounter
                                    bindingInfo
                                    (inferValidationExpressionType environment (ref freshCounter.Value) locals)
                                    (tryResolveUniqueLocalImplicitByType environment locals)
                                    arguments
                            with
                            | Some _ ->
                                expectedArgumentDiagnostics
                            | None when localCanSatisfyImplicitParameter bindingInfo && not hasExplicitImplicitArgument ->
                                expectedArgumentDiagnostics
                            | None
                                when hasNamedBlock
                                     || hasExplicitImplicitArgument
                                     || hasUnsatisfiedRuntimeImplicit bindingInfo
                                     || hasTooManyArguments bindingInfo ->
                                let message =
                                    if hasNamedBlock then
                                        "Named application arguments do not match the callee parameter metadata."
                                    elif hasExplicitImplicitArgument
                                         || (bindingInfo.ParameterLayouts
                                             |> Option.exists (List.exists (fun parameter -> parameter.IsImplicit))) then
                                        "Implicit application argument could not be resolved or does not match the implicit parameter."
                                    else
                                        "Application argument types do not match the callee parameters."

                                expectedArgumentDiagnostics
                                @ [ makeDiagnostic DiagnosticCode.TypeEqualityMismatch message ]
                            | None ->
                                expectedArgumentDiagnostics
                        | None
                            when allowUnresolvedCallDiagnostics
                                 && not isKnownVisibleCallee
                                 && not (String.IsNullOrEmpty(calleeName))
                                 && Char.IsUpper(calleeName[0]) ->
                            [ makeDiagnostic DiagnosticCode.NameUnresolved $"Name '{calleeName}' is not in scope." ]
                        | None when hasNamedBlock ->
                            [ makeDiagnostic DiagnosticCode.TypeEqualityMismatch "Named application requires a callee with preserved parameter metadata." ]
                        | _ ->
                            []
                    | _ when hasNamedBlock ->
                        [ makeDiagnostic DiagnosticCode.TypeEqualityMismatch "Named application requires a callee with preserved parameter metadata." ]
                    | _ ->
                        []

                argumentDiagnostics @ applicationDiagnostics
            | Elvis(left, right) ->
                let diagnostics = recurse left @ recurse right

                match inferValidationExpressionType environment freshCounter locals left with
                | Some leftType when tryOptionPayloadType environment.VisibleTypeAliases leftType |> Option.isNone ->
                    diagnostics
                    @ [ makeDiagnostic
                            DiagnosticCode.ElvisReceiverNotOption
                            "Elvis `?:` requires its left-hand side to have type `Option T` for some `T`." ]
                | _ ->
                    diagnostics
            | Binary(left, operatorName, right) ->
                let diagnostics = recurse left @ recurse right

                match operatorName with
                | "&&"
                | "||" ->
                    let operandDiagnostics operandLabel operand =
                        match inferValidationExpressionType environment freshCounter locals operand with
                        | Some operandType when not (expectedTypeAccepts locals refinements boolType operandType) ->
                            [
                                makeDiagnostic
                                    DiagnosticCode.TypeEqualityMismatch
                                    $"Short-circuit operator {operandLabel} must match the expected suspended Bool operand type, but found '{TypeSignatures.toText operandType}'."
                            ]
                        | _ ->
                            []

                    diagnostics
                    @ operandDiagnostics "left-hand side" left
                    @ operandDiagnostics "right-hand side" right
                | _ ->
                    diagnostics
            | PrefixedString(prefix, parts) ->
                prefixedStringPrefixDiagnostics locals lexicalNames prefix
                @ (parts
                   |> List.collect (function
                       | StringText _ -> []
                       | StringInterpolation(inner, _) -> recurse inner))

        and validateDoStatements locals refinements lexicalNames statements =
            match statements with
            | [] ->
                []
            | statement :: rest ->
                match statement with
                | DoLet(binding, expression) ->
                    let nextLexicalNames =
                        collectPatternNames binding.Pattern
                        |> Set.ofList
                        |> Set.union lexicalNames

                    let nextLocals =
                        match inferValidationExpressionType environment freshCounter locals expression with
                        | Some valueType ->
                            collectPatternNames binding.Pattern
                            |> List.fold (fun state name -> Map.add name valueType state) locals
                        | None ->
                            locals

                    let localStaticAliases = collectPatternStaticAliases binding.Pattern expression Map.empty
                    let restForValidation =
                        match rewriteStaticObjectPathAliasUses localStaticAliases (Do rest) with
                        | Do rewritten -> rewritten
                        | _ -> rest

                    validateExpression locals refinements lexicalNames expression
                    @ validateDoStatements nextLocals refinements nextLexicalNames restForValidation
                | DoBind(binding, expression) ->
                    let nextLexicalNames =
                        collectPatternNames binding.Pattern
                        |> Set.ofList
                        |> Set.union lexicalNames

                    let nextLocals =
                        match inferValidationExpressionType environment freshCounter locals expression with
                        | Some valueType ->
                            collectPatternNames binding.Pattern
                            |> List.fold (fun state name -> Map.add name (unwrapBindPayloadType valueType) state) locals
                        | None ->
                            locals

                    validateExpression locals refinements lexicalNames expression
                    @ validateDoStatements nextLocals refinements nextLexicalNames rest
                | DoLetQuestion(binding, expression, failure) ->
                    let nextLexicalNames =
                        collectPatternNames binding.Pattern
                        |> Set.ofList
                        |> Set.union lexicalNames

                    let nextLocals =
                        match inferValidationExpressionType environment freshCounter locals expression with
                        | Some valueType ->
                            collectPatternNames binding.Pattern
                            |> List.fold (fun state name -> Map.add name valueType state) locals
                        | None ->
                            locals

                    let failureDiagnostics =
                        match failure with
                        | Some failure ->
                            let failureLexicalNames =
                                collectPatternNames failure.ResiduePattern.Pattern
                                |> Set.ofList
                                |> Set.union lexicalNames

                            validateDoStatements locals refinements failureLexicalNames failure.Body
                        | None -> []

                    validateExpression locals refinements lexicalNames expression
                    @ validatePatternHead binding.Pattern
                    @ activePatternLinearityDiagnostics binding.Pattern "let?"
                    @ failureDiagnostics
                    @ validateDoStatements nextLocals refinements nextLexicalNames rest
                | DoUsing(binding, expression) ->
                    let nextLexicalNames =
                        collectPatternNames binding.Pattern
                        |> Set.ofList
                        |> Set.union lexicalNames

                    let nextLocals =
                        match binding.Pattern, inferValidationExpressionType environment freshCounter locals expression with
                        | NamePattern bindingName, Some valueType -> Map.add bindingName (unwrapIoType valueType) locals
                        | _ -> locals

                    validateExpression locals refinements lexicalNames expression
                    @ validateDoStatements nextLocals refinements nextLexicalNames rest
                | DoVar(bindingName, expression) ->
                    let nextLocals =
                        match inferValidationExpressionType environment freshCounter locals expression with
                        | Some valueType -> Map.add bindingName (refType valueType) locals
                        | None -> locals

                    validateExpression locals refinements lexicalNames expression
                    @ validateDoStatements nextLocals refinements (Set.add bindingName lexicalNames) rest
                | DoAssign(_, expression)
                | DoDefer expression
                | DoExpression expression ->
                    validateExpression locals refinements lexicalNames expression
                    @ validateDoStatements locals refinements lexicalNames rest
                | DoIf(condition, whenTrue, whenFalse) ->
                    validateExpression locals refinements lexicalNames condition
                    @ validateDoStatements locals refinements lexicalNames whenTrue
                    @ validateDoStatements locals refinements lexicalNames whenFalse
                    @ validateDoStatements locals refinements lexicalNames rest
                | DoWhile(condition, body) ->
                    validateExpression locals refinements lexicalNames condition
                    @ validateDoStatements locals refinements lexicalNames body
                    @ validateDoStatements locals refinements lexicalNames rest
                | DoReturn expression ->
                    validateExpression locals refinements lexicalNames expression

        let rec validateInoutPlacement insideDo expression =
            let recurse = validateInoutPlacement insideDo

            match expression with
            | InoutArgument inner ->
                let markerDiagnostics =
                    if insideDo then
                        []
                    else
                        [ makeDiagnostic DiagnosticCode.QttInoutMarkerUnexpected "The '~' inout marker is only valid inside do blocks." ]

                markerDiagnostics @ recurse inner
            | SyntaxQuote inner
            | SyntaxSplice inner
            | TopLevelSyntaxSplice inner
            | CodeQuote inner
            | CodeSplice inner ->
                recurse inner
            | Handle(_, label, body, returnClause, operationClauses) ->
                recurse label
                @ recurse body
                @ recurse returnClause.Body
                @ (operationClauses |> List.collect (fun clause -> recurse clause.Body))
            | LocalLet(_, value, body) ->
                recurse value @ recurse body
            | LocalSignature(_, body) ->
                recurse body
            | LocalTypeAlias(_, body) ->
                recurse body
            | LocalScopedEffect(_, body) ->
                recurse body
            | Lambda(_, body) ->
                validateInoutPlacement false body
            | IfThenElse(condition, whenTrue, whenFalse) ->
                recurse condition @ recurse whenTrue @ recurse whenFalse
            | Match(scrutinee, cases) ->
                recurse scrutinee
                @ (cases
                   |> List.collect (fun caseClause ->
                       (caseClause.Guard |> Option.map recurse |> Option.defaultValue [])
                       @ recurse caseClause.Body))
            | RecordLiteral fields ->
                fields |> List.collect (fun field -> recurse field.Value)
            | Seal(value, _) ->
                recurse value
            | RecordUpdate(receiver, fields) ->
                recurse receiver @ (fields |> List.collect (fun field -> recurse field.Value))
            | MemberAccess(receiver, _, arguments) ->
                recurse receiver @ (arguments |> List.collect recurse)
            | SafeNavigation(receiver, navigation) ->
                recurse receiver @ (navigation.Arguments |> List.collect recurse)
            | TagTest(receiver, _) ->
                recurse receiver
            | Do statements ->
                statements |> List.collect validateInoutDoStatement
            | MonadicSplice inner
            | ExplicitImplicitArgument inner
            | Unary(_, inner) ->
                recurse inner
            | NamedApplicationBlock fields ->
                fields |> List.collect (fun field -> recurse field.Value)
            | Apply(callee, arguments) ->
                recurse callee @ (arguments |> List.collect recurse)
            | Binary(left, _, right)
            | Elvis(left, right) ->
                recurse left @ recurse right
            | PrefixedString(_, parts) ->
                parts
                |> List.collect (function
                    | StringText _ -> []
                    | StringInterpolation(inner, _) -> recurse inner)
            | Literal _
            | NumericLiteral _
            | KindQualifiedName _
            | Name _ ->
                []

        and validateInoutDoStatement statement =
            match statement with
            | DoLet(_, expression)
            | DoLetQuestion(_, expression, _)
            | DoBind(_, expression)
            | DoVar(_, expression)
            | DoAssign(_, expression)
            | DoDefer expression
            | DoUsing(_, expression)
            | DoReturn expression
            | DoExpression expression ->
                validateInoutPlacement true expression
            | DoIf(condition, whenTrue, whenFalse) ->
                validateInoutPlacement true condition
                @ (whenTrue |> List.collect validateInoutDoStatement)
                @ (whenFalse |> List.collect validateInoutDoStatement)
            | DoWhile(condition, body) ->
                validateInoutPlacement true condition @ (body |> List.collect validateInoutDoStatement)

        let tryProjectionDescriptorAlias (expression: SurfaceExpression) =
            let tryBuild projectionName suppliedArguments =
                environment.VisibleProjections
                |> Map.tryFind projectionName
                |> Option.bind (fun projectionInfo ->
                    if List.length suppliedArguments >= List.length projectionInfo.Binders then
                        None
                    else
                        let suppliedBinders = projectionInfo.Binders |> List.take (List.length suppliedArguments)
                        let remainingBinders = projectionInfo.Binders |> List.skip (List.length suppliedArguments)

                        let suppliedAreValues =
                            suppliedBinders
                            |> List.forall (function
                                | ProjectionValueBinder _ -> true
                                | ProjectionPlaceBinder _ -> false)

                        let remainingPlaceBinders =
                            remainingBinders
                            |> List.choose (function
                                | ProjectionPlaceBinder binder -> Some binder
                                | ProjectionValueBinder _ -> None)

                        if suppliedAreValues && List.length remainingPlaceBinders = List.length remainingBinders then
                            Some
                                { Projection = projectionInfo
                                  RemainingPlaceBinders = remainingPlaceBinders }
                        else
                            None)

            match expression with
            | Name [ projectionName ] -> tryBuild projectionName []
            | Apply(Name [ projectionName ], suppliedArguments) -> tryBuild projectionName suppliedArguments
            | _ -> None

        let rec isStableDescriptorPlace (expression: SurfaceExpression) =
            match expression with
            | Name(_ :: _) ->
                true
            | Apply(Name [ projectionName ], arguments) ->
                environment.VisibleProjections
                |> Map.tryFind projectionName
                |> Option.exists (fun projectionInfo ->
                    match projectionInfo.Body with
                    | Some(ProjectionAccessors _) ->
                        false
                    | _ ->
                        List.length projectionInfo.Binders = List.length arguments
                        && (List.zip projectionInfo.Binders arguments
                            |> List.forall (fun (binder, argument) ->
                                match binder with
                                | ProjectionPlaceBinder _ -> isStableDescriptorPlace argument
                                | ProjectionValueBinder _ -> true)))
            | _ ->
                false

        let descriptorApplicationDiagnostics (remainingPlaceBinders: ProjectionPlaceBinder list) rootsArgument =
            let rootPackFieldMap (fields: SurfaceRecordLiteralField list) =
                fields
                |> List.map (fun field -> field.Name, field.Value)
                |> Map.ofList

            let unstableRootDiagnostics expressions =
                expressions
                |> List.choose (fun expression ->
                    if isStableDescriptorPlace expression then
                        None
                    else
                        Some(makeDiagnostic DiagnosticCode.ProjectionDefinitionUnsupported "Projector descriptor roots must be stable places or selector-computed places."))

            match remainingPlaceBinders with
            | [] ->
                [ makeDiagnostic DiagnosticCode.ProjectionDefinitionUnsupported "A projector descriptor application must still have at least one root." ]
            | [ binder ] ->
                match rootsArgument with
                | RecordLiteral fields when fields |> List.exists (fun field -> String.Equals(field.Name, binder.Name, StringComparison.Ordinal)) ->
                    let fieldMap = rootPackFieldMap fields
                    let fieldNames = fieldMap |> Map.keys |> Set.ofSeq
                    let expectedNames = Set.singleton binder.Name

                    if fieldNames <> expectedNames then
                        [ makeDiagnostic DiagnosticCode.ProjectionDefinitionUnsupported "Projector descriptor roots pack fields must match the projector root binders exactly." ]
                    else
                        fieldMap |> Map.find binder.Name |> List.singleton |> unstableRootDiagnostics
                | RecordLiteral _ ->
                    [ makeDiagnostic DiagnosticCode.ProjectionDefinitionUnsupported "Projector descriptor roots pack fields must match the projector root binders exactly." ]
                | _ ->
                    unstableRootDiagnostics [ rootsArgument ]
            | binders ->
                match rootsArgument with
                | RecordLiteral fields ->
                    let fieldMap = rootPackFieldMap fields
                    let fieldNames = fieldMap |> Map.keys |> Set.ofSeq
                    let expectedNames = binders |> List.map (fun binder -> binder.Name) |> Set.ofList

                    if fieldNames <> expectedNames then
                        [ makeDiagnostic DiagnosticCode.ProjectionDefinitionUnsupported "Projector descriptor roots pack fields must match the projector root binders exactly." ]
                    else
                        binders
                        |> List.map (fun binder -> fieldMap |> Map.find binder.Name)
                        |> unstableRootDiagnostics
                | _ ->
                    [ makeDiagnostic DiagnosticCode.ProjectionDefinitionUnsupported "Multi-root projector descriptors must be applied to a record literal roots pack." ]

        let fullyAppliedProjectionReturningDescriptorDiagnostic body =
            let returnTypeIsProjector =
                definition.ReturnTypeTokens
                |> Option.bind TypeSignatures.parseType
                |> Option.exists (function
                    | TypeName(([ "Projector" ] | [ "std"; "prelude"; "Projector" ]), _) -> true
                    | _ -> false)

            match returnTypeIsProjector, body with
            | true, Apply(Name [ projectionName ], arguments) ->
                environment.VisibleProjections
                |> Map.tryFind projectionName
                |> Option.filter (fun projectionInfo -> List.length projectionInfo.Binders = List.length arguments)
                |> Option.map (fun _ ->
                    [ makeDiagnostic DiagnosticCode.ProjectionDefinitionUnsupported "A fully applied projection produces the projected focus, not a projector descriptor value." ])
                |> Option.defaultValue []
            | _ ->
                []

        let rec validateProjectionDescriptorApplications (aliases: Map<string, ProjectionDescriptorAliasInfo>) (expression: SurfaceExpression) =
            let recurse = validateProjectionDescriptorApplications aliases

            match expression with
            | SyntaxQuote inner
            | SyntaxSplice inner
            | TopLevelSyntaxSplice inner
            | CodeQuote inner
            | CodeSplice inner ->
                recurse inner
            | Handle(_, label, body, returnClause, operationClauses) ->
                recurse label
                @ recurse body
                @ recurse returnClause.Body
                @ (operationClauses |> List.collect (fun clause -> recurse clause.Body))
            | Apply(Name [ aliasName ], [ rootsArgument ]) ->
                let nested = validateProjectionDescriptorApplications aliases rootsArgument

                match aliases |> Map.tryFind aliasName with
                | Some aliasInfo ->
                    nested @ descriptorApplicationDiagnostics aliasInfo.RemainingPlaceBinders rootsArgument
                | None ->
                    nested
            | Apply(callee, arguments) ->
                recurse callee @ (arguments |> List.collect recurse)
            | LocalLet(binding, value, body) ->
                let valueDiagnostics = recurse value
                let shadowedAliases =
                    collectPatternNames binding.Pattern
                    |> List.fold (fun current name -> Map.remove name current) aliases

                let bodyAliases =
                    match binding.Pattern, tryProjectionDescriptorAlias value with
                    | NamePattern aliasName, Some aliasInfo -> Map.add aliasName aliasInfo shadowedAliases
                    | _ -> shadowedAliases

                valueDiagnostics @ validateProjectionDescriptorApplications bodyAliases body
            | LocalSignature(declaration, body) ->
                validateProjectionDescriptorApplications (Map.remove declaration.Name aliases) body
            | LocalTypeAlias(_, body) ->
                recurse body
            | LocalScopedEffect(_, body) ->
                recurse body
            | Lambda(parameters, body) ->
                let bodyAliases =
                    parameters
                    |> List.fold (fun current parameter -> Map.remove parameter.Name current) aliases

                validateProjectionDescriptorApplications bodyAliases body
            | IfThenElse(condition, whenTrue, whenFalse) ->
                recurse condition @ recurse whenTrue @ recurse whenFalse
            | Match(scrutinee, cases) ->
                recurse scrutinee
                @ (cases
                   |> List.collect (fun caseClause ->
                       (caseClause.Guard |> Option.map recurse |> Option.defaultValue [])
                       @ recurse caseClause.Body))
            | RecordLiteral fields ->
                fields |> List.collect (fun field -> recurse field.Value)
            | Seal(value, _) ->
                recurse value
            | RecordUpdate(receiver, fields) ->
                recurse receiver @ (fields |> List.collect (fun field -> recurse field.Value))
            | MemberAccess(receiver, _, arguments) ->
                recurse receiver @ (arguments |> List.collect recurse)
            | SafeNavigation(receiver, navigation) ->
                recurse receiver @ (navigation.Arguments |> List.collect recurse)
            | TagTest(receiver, _) ->
                recurse receiver
            | Do statements ->
                validateProjectionDescriptorDoStatements aliases statements
            | MonadicSplice inner
            | ExplicitImplicitArgument inner
            | InoutArgument inner
            | Unary(_, inner) ->
                recurse inner
            | NamedApplicationBlock fields ->
                fields |> List.collect (fun field -> recurse field.Value)
            | Binary(left, _, right)
            | Elvis(left, right) ->
                recurse left @ recurse right
            | PrefixedString(_, parts) ->
                parts
                |> List.collect (function
                    | StringText _ -> []
                    | StringInterpolation(inner, _) -> recurse inner)
            | Literal _
            | NumericLiteral _
            | KindQualifiedName _
            | Name _ ->
                []

        and validateProjectionDescriptorDoStatements (aliases: Map<string, ProjectionDescriptorAliasInfo>) (statements: SurfaceDoStatement list) =
            match statements with
            | [] ->
                []
            | statement :: rest ->
                match statement with
                | DoLet(binding, expression)
                | DoBind(binding, expression)
                | DoUsing(binding, expression) ->
                    let nextAliases =
                        collectPatternNames binding.Pattern
                        |> List.fold (fun current name -> Map.remove name current) aliases

                    validateProjectionDescriptorApplications aliases expression
                    @ validateProjectionDescriptorDoStatements nextAliases rest
                | DoLetQuestion(binding, expression, failure) ->
                    let nextAliases =
                        collectPatternNames binding.Pattern
                        |> List.fold (fun current name -> Map.remove name current) aliases

                    let failureDiagnostics =
                        failure
                        |> Option.map (fun block -> validateProjectionDescriptorDoStatements aliases block.Body)
                        |> Option.defaultValue []

                    validateProjectionDescriptorApplications aliases expression
                    @ failureDiagnostics
                    @ validateProjectionDescriptorDoStatements nextAliases rest
                | DoVar(name, expression) ->
                    validateProjectionDescriptorApplications aliases expression
                    @ validateProjectionDescriptorDoStatements (Map.remove name aliases) rest
                | DoAssign(target, expression) ->
                    validateProjectionDescriptorApplications aliases expression
                    @ validateProjectionDescriptorDoStatements aliases rest
                | DoDefer expression
                | DoExpression expression
                | DoReturn expression ->
                    validateProjectionDescriptorApplications aliases expression
                    @ validateProjectionDescriptorDoStatements aliases rest
                | DoIf(condition, whenTrue, whenFalse) ->
                    validateProjectionDescriptorApplications aliases condition
                    @ validateProjectionDescriptorDoStatements aliases whenTrue
                    @ validateProjectionDescriptorDoStatements aliases whenFalse
                    @ validateProjectionDescriptorDoStatements aliases rest
                | DoWhile(condition, body) ->
                    validateProjectionDescriptorApplications aliases condition
                    @ validateProjectionDescriptorDoStatements aliases body
                    @ validateProjectionDescriptorDoStatements aliases rest

        let validateLocalImplicitResolution body =
            let normalizedKey typeExpr =
                normalizeTypeAliases environment.VisibleTypeAliases typeExpr |> TypeSignatures.toText

            let implicitQuantity quantity =
                quantity |> Option.defaultValue QuantityOmega

            let addCandidate name quantity typeExpr scope =
                let key = normalizedKey typeExpr
                let existing = scope |> Map.tryFind key |> Option.defaultValue []
                Map.add key ((name, implicitQuantity quantity) :: existing) scope

            let addImplicitBinding (binding: SurfaceBindPattern) valueType scope =
                if binding.IsImplicit then
                    collectPatternNames binding.Pattern
                    |> List.fold (fun current name -> addCandidate name binding.Quantity valueType current) scope
                else
                    scope

            let addImplicitParameter scope (parameter: Parameter) =
                if parameter.IsImplicit then
                    parameter.TypeTokens
                    |> Option.bind TypeSignatures.parseType
                    |> Option.map (fun parameterType -> addCandidate parameter.Name parameter.Quantity parameterType scope)
                    |> Option.defaultValue scope
                else
                    scope

            let initialScope =
                definition.Parameters
                |> List.fold addImplicitParameter Map.empty

            let resolveCandidates scopes parameterType =
                let key = normalizedKey parameterType

                scopes
                |> List.tryPick (fun scope ->
                    scope
                    |> Map.tryFind key
                    |> Option.filter (fun candidates -> not (List.isEmpty candidates)))

            let implicitDemandDiagnostics inEscapingLambda scopes (bindingInfo: BindingSchemeInfo) arguments =
                match bindingInfo.ParameterLayouts with
                | Some layouts ->
                    let parameterTypes, _ = TypeSignatures.schemeParts bindingInfo.Scheme

                    if List.length layouts <> List.length parameterTypes then
                        []
                    else
                        let hasExplicitImplicitArgument =
                            arguments
                            |> List.exists (function
                                | ExplicitImplicitArgument _ -> true
                                | _ -> false)

                        if hasExplicitImplicitArgument then
                            []
                        else
                            List.zip layouts parameterTypes
                            |> List.collect (fun (layout, parameterType) ->
                                if not layout.IsImplicit
                                   || isCompileTimeArgumentType environment.VisibleTypeAliases parameterType then
                                    []
                                else
                                    match resolveCandidates scopes parameterType with
                                    | None ->
                                        []
                                    | Some candidates when List.length candidates > 1 ->
                                        [ makeDiagnostic DiagnosticCode.TypeEqualityMismatch "Implicit application argument is ambiguous in the nearest lexical implicit scope." ]
                                    | Some [ _, QuantityZero ] ->
                                        [ makeDiagnostic DiagnosticCode.TypeEqualityMismatch "A quantity-0 local implicit value cannot satisfy a runtime implicit parameter." ]
                                    | Some [ _, QuantityBorrow _ ] when inEscapingLambda ->
                                        [ makeDiagnostic DiagnosticCode.QttBorrowEscape "A lambda cannot capture a borrowed implicit value that may escape its borrow scope." ]
                                    | Some _ ->
                                        [])
                | None ->
                    []

            let rec validateExpr inEscapingLambda scopes expression =
                let recurse = validateExpr inEscapingLambda scopes

                match expression with
                | SyntaxQuote inner
                | SyntaxSplice inner
                | TopLevelSyntaxSplice inner
                | CodeQuote inner
                | CodeSplice inner ->
                    recurse inner
                | Handle(_, label, body, returnClause, operationClauses) ->
                    recurse label
                    @ recurse body
                    @ recurse returnClause.Body
                    @ (operationClauses |> List.collect (fun clause -> recurse clause.Body))
                | Apply(Name [ calleeName ], arguments) ->
                    let bindingDiagnostics =
                        environment.VisibleBindings
                        |> Map.tryFind calleeName
                        |> Option.map (fun bindingInfo -> implicitDemandDiagnostics inEscapingLambda scopes bindingInfo arguments)
                        |> Option.defaultValue []

                    bindingDiagnostics @ (arguments |> List.collect recurse)
                | Name [ calleeName ] ->
                    environment.VisibleBindings
                    |> Map.tryFind calleeName
                    |> Option.map (fun bindingInfo -> implicitDemandDiagnostics inEscapingLambda scopes bindingInfo [])
                    |> Option.defaultValue []
                | Apply(callee, arguments) ->
                    recurse callee @ (arguments |> List.collect recurse)
                | LocalSignature(_, nestedBody) ->
                    validateExpr inEscapingLambda scopes nestedBody
                | LocalTypeAlias(_, nestedBody) ->
                    validateExpr inEscapingLambda scopes nestedBody
                | LocalLet(binding, value, nestedBody) ->
                    let valueDiagnostics = recurse value
                    let valueType =
                        binding.TypeTokens
                        |> Option.bind TypeSignatures.parseType
                        |> Option.orElseWith (fun () -> inferValidationExpressionType environment freshCounter Map.empty value)
                    let nestedScopes =
                        valueType
                        |> Option.map (fun inferredType -> (addImplicitBinding binding inferredType Map.empty) :: scopes)
                        |> Option.defaultValue scopes

                    valueDiagnostics @ validateExpr inEscapingLambda nestedScopes nestedBody
                | LocalScopedEffect(_, nestedBody) ->
                    validateExpr inEscapingLambda scopes nestedBody
                | Lambda(parameters, nestedBody) ->
                    let lambdaScope =
                        parameters
                        |> List.fold addImplicitParameter Map.empty

                    validateExpr true (lambdaScope :: scopes) nestedBody
                | IfThenElse(condition, whenTrue, whenFalse) ->
                    recurse condition @ recurse whenTrue @ recurse whenFalse
                | Match(scrutinee, cases) ->
                    recurse scrutinee
                    @ (cases
                       |> List.collect (fun caseClause ->
                           (caseClause.Guard |> Option.map recurse |> Option.defaultValue [])
                           @ recurse caseClause.Body))
                | RecordLiteral fields ->
                    fields |> List.collect (fun field -> recurse field.Value)
                | RecordUpdate(receiver, fields) ->
                    recurse receiver @ (fields |> List.collect (fun field -> recurse field.Value))
                | MemberAccess(receiver, _, arguments) ->
                    recurse receiver @ (arguments |> List.collect recurse)
                | SafeNavigation(receiver, navigation) ->
                    recurse receiver @ (navigation.Arguments |> List.collect recurse)
                | TagTest(receiver, _) ->
                    recurse receiver
                | Do statements ->
                    validateDo inEscapingLambda scopes statements
                | MonadicSplice inner
                | ExplicitImplicitArgument inner
                | InoutArgument inner
                | Unary(_, inner)
                | Seal(inner, _) ->
                    recurse inner
                | NamedApplicationBlock fields ->
                    fields |> List.collect (fun field -> recurse field.Value)
                | Binary(left, _, right)
                | Elvis(left, right) ->
                    recurse left @ recurse right
                | PrefixedString(_, parts) ->
                    parts
                    |> List.collect (function
                        | StringText _ -> []
                        | StringInterpolation(inner, _) -> recurse inner)
                | Literal _
                | NumericLiteral _
                | KindQualifiedName _
                | Name _ ->
                    []

            and validateDo inEscapingLambda scopes statements =
                match statements with
                | [] ->
                    []
                | statement :: rest ->
                    match statement with
                    | DoLet(binding, expression)
                    | DoBind(binding, expression) ->
                        let expressionDiagnostics = validateExpr inEscapingLambda scopes expression
                        let valueType =
                            binding.TypeTokens
                            |> Option.bind TypeSignatures.parseType
                            |> Option.orElseWith (fun () ->
                                inferValidationExpressionType environment freshCounter Map.empty expression
                                |> Option.map (fun typeExpr ->
                                    match statement with
                                    | DoBind _ -> unwrapBindPayloadType typeExpr
                                    | _ -> typeExpr))

                        let restScopes =
                            valueType
                            |> Option.map (fun inferredType -> (addImplicitBinding binding inferredType Map.empty) :: scopes)
                            |> Option.defaultValue scopes

                        expressionDiagnostics @ validateDo inEscapingLambda restScopes rest
                    | DoLetQuestion(binding, expression, failure) ->
                        let expressionDiagnostics = validateExpr inEscapingLambda scopes expression
                        let valueType =
                            binding.TypeTokens
                            |> Option.bind TypeSignatures.parseType
                            |> Option.orElseWith (fun () -> inferValidationExpressionType environment freshCounter Map.empty expression)

                        let restScopes =
                            valueType
                            |> Option.map (fun inferredType -> (addImplicitBinding binding inferredType Map.empty) :: scopes)
                            |> Option.defaultValue scopes

                        let failureDiagnostics =
                            failure
                            |> Option.map (fun block -> validateDo inEscapingLambda scopes block.Body)
                            |> Option.defaultValue []

                        expressionDiagnostics @ failureDiagnostics @ validateDo inEscapingLambda restScopes rest
                    | DoUsing(binding, expression) ->
                        validateExpr inEscapingLambda scopes expression @ validateDo inEscapingLambda scopes rest
                    | DoVar(_, expression)
                    | DoAssign(_, expression)
                    | DoDefer expression
                    | DoExpression expression
                    | DoReturn expression ->
                        validateExpr inEscapingLambda scopes expression @ validateDo inEscapingLambda scopes rest
                    | DoIf(condition, whenTrue, whenFalse) ->
                        validateExpr inEscapingLambda scopes condition
                        @ validateDo inEscapingLambda scopes whenTrue
                        @ validateDo inEscapingLambda scopes whenFalse
                        @ validateDo inEscapingLambda scopes rest
                    | DoWhile(condition, body) ->
                        validateExpr inEscapingLambda scopes condition
                        @ validateDo inEscapingLambda scopes body
                        @ validateDo inEscapingLambda scopes rest

            validateExpr false [ initialScope ] body

        let parameterRecordDiagnostics =
            definition.Parameters
            |> List.collect (fun parameter ->
                parameter.TypeTokens
                |> Option.bind tryRecordInfoFromTypeTokens
                |> Option.map validateRecordSurfaceInfo
                |> Option.defaultValue [])

        let activePatternDeclarationDiagnostics =
            if not definition.IsPattern then
                []
            else
                let explicitParameterCount =
                    definition.Parameters
                    |> List.filter (fun parameter -> not parameter.IsImplicit)
                    |> List.length

                let binderDiagnostics =
                    if explicitParameterCount = 0 then
                        [ makeDiagnostic DiagnosticCode.ActivePatternInvalid "An active pattern declaration must have at least one explicit binder for the scrutinee." ]
                    else
                        []

                let resultDiagnostics =
                    scheme
                    |> Option.map (fun parsedScheme ->
                        if isMonadicType parsedScheme.Body then
                            [ makeDiagnostic DiagnosticCode.ActivePatternInvalid "An active pattern declaration result type must not be monadic." ]
                        else
                            [])
                    |> Option.defaultValue []

                binderDiagnostics @ resultDiagnostics

        parameterRecordDiagnostics
        @ activePatternDeclarationDiagnostics
        @
            match definition.Body with
            | Some body ->
                validateExpression localTypes Map.empty parameterNames body
                @ validateLocalImplicitResolution body
                @ validateExpectedBody localTypes body
                @ validateInoutPlacement false body
                @ validateProjectionDescriptorApplications Map.empty body
                @ fullyAppliedProjectionReturningDescriptorDiagnostic body
            | None -> []

    let validateSurfaceModules (frontendModules: KFrontIRModule list) =
        let surfaceIndex = buildSurfaceIndex frontendModules

        frontendModules
        |> List.collect (fun frontendModule ->
            let moduleName = moduleNameText frontendModule.ModuleIdentity

            let topLevelNames =
                frontendModule.Declarations
                |> List.collect (function
                    | SignatureDeclaration declaration -> [ declaration.Name ]
                    | LetDeclaration declaration -> declaration.Name |> Option.toList
                    | ProjectionDeclarationNode declaration -> [ declaration.Name ]
                    | DataDeclarationNode declaration ->
                        declaration.Name :: (declaration.Constructors |> List.map (fun constructor -> constructor.Name))
                    | TypeAliasNode declaration -> [ declaration.Name ]
                    | TraitDeclarationNode declaration -> [ declaration.Name ]
                    | ExpectDeclarationNode (ExpectTermDeclaration declaration) -> [ declaration.Name ]
                    | ExpectDeclarationNode (ExpectTypeDeclaration declaration) -> [ declaration.Name ]
                    | ExpectDeclarationNode (ExpectTraitDeclaration declaration) -> [ declaration.Name ]
                    | _ -> [])
                |> Set.ofList

            let makeDiagnostic code message =
                { Severity = DiagnosticSeverity.Error
                  Code = code
                  Stage = Some "KFrontIR"
                  Phase = Some(KFrontIRPhase.phaseName CORE_LOWERING)
                  Message = message
                  Location = None
                  RelatedLocations = [] }

            let baseEnvironment =
                { CurrentModuleName = moduleName
                  VisibleTypeAliases = mergeVisibleTypeAliases surfaceIndex moduleName
                  VisibleTypeFacets = mergeVisibleTypeFacets surfaceIndex moduleName
                  VisibleStaticObjects = Map.empty
                  VisibleModules = mergeVisibleModules surfaceIndex moduleName
                  VisibleRecordTypes = mergeVisibleRecordTypes surfaceIndex moduleName
                  VisibleBindings = mergeVisibleBindings surfaceIndex moduleName
                  VisibleConstructors = mergeVisibleConstructors surfaceIndex moduleName
                  VisibleProjections = mergeVisibleProjections surfaceIndex moduleName
                  VisibleTraits = mergeVisibleTraits surfaceIndex moduleName
                  VisibleInstances = visibleInstances surfaceIndex moduleName
                  ConstrainedMembers = Map.empty }

            let environment =
                { baseEnvironment with
                    VisibleStaticObjects = buildStaticObjectAliasesForModule baseEnvironment frontendModule.Declarations }

            let hasNonQualifiedImports =
                frontendModule.Declarations
                |> List.exists (function
                    | ImportDeclaration (_, specs) ->
                        specs
                        |> List.exists (fun spec ->
                            match spec.Selection with
                            | QualifiedOnly -> false
                            | _ -> true)
                    | _ ->
                        false)

            let allowUnresolvedCallDiagnostics =
                not hasNonQualifiedImports
                && not (frontendModule.Diagnostics |> List.exists (fun diagnostic -> diagnostic.Severity = Error))

            let knownSurfaceTermNames =
                let preludeContract = IntrinsicCatalog.bundledPreludeExpectContract ()
                let standardRuntimeNames =
                    Set.ofList
                        [ "bindModule"; "bindModuleOwned"; "bridgePackageValue"; "bridgePackageOrigin"; "bridgeFailureToCastBlame"
                          "checkedCast"; "checkedCastWith"; "sameDynRep"; "toDyn"; "toDynWith"
                          "Conservative"; "Exact"; "IntoKappa"; "LaterUse"; "Lossy"; "OutOfKappa" ]

                let compilerKnownSurfaceTerms =
                    Set.ofList
                        [ "thunk"
                          "lazy"
                          "force"
                          "captureBorrow"
                          "withBorrowView"
                          "fork"
                          "defEqSyntax"
                          "headSymbolSyntax"
                          "sameSymbol" ]

                let visibleTraitMemberNames =
                    environment.VisibleTraits
                    |> Map.values
                    |> Seq.collect (fun traitInfo -> traitInfo.Members.Keys)
                    |> Set.ofSeq

                [ topLevelNames
                  preludeContract.TermNames
                  preludeContract.TypeNames
                  preludeContract.TraitNames
                  Set.ofList Stdlib.FixedPreludeConstructors
                  IntrinsicCatalog.namedIntrinsicTermNames ()
                  standardRuntimeNames
                  compilerKnownSurfaceTerms
                  environment.VisibleBindings |> Map.keys |> Set.ofSeq
                  environment.VisibleConstructors |> Map.keys |> Set.ofSeq
                  environment.VisibleModules
                  environment.VisibleStaticObjects |> Map.keys |> Set.ofSeq
                  environment.VisibleTypeFacets |> Map.keys |> Set.ofSeq
                  environment.VisibleTraits |> Map.keys |> Set.ofSeq
                  visibleTraitMemberNames
                  environment.ConstrainedMembers |> Map.keys |> Set.ofSeq ]
                |> Set.unionMany
                |> Set.remove "<anonymous>"

            let validateProjectionDeclaration (declaration: ProjectionDeclaration) =
                let placeBinders =
                    declaration.Binders
                    |> List.choose (function
                        | ProjectionPlaceBinder binder -> Some binder
                        | ProjectionValueBinder _ -> None)

                let placeBinderNames = placeBinders |> List.map (fun binder -> binder.Name) |> Set.ofList

                let missingPlaceDiagnostics =
                    if List.isEmpty placeBinders then
                        [ makeDiagnostic DiagnosticCode.ProjectionDefinitionUnsupported "A projection definition must declare at least one place binder." ]
                    else
                        []

                let rec selectorYieldDiagnostics body =
                    match body with
                    | ProjectionYield(Name(root :: _)) when Set.contains root placeBinderNames ->
                        []
                    | ProjectionYield(Apply(Name [ projectionName ], _)) ->
                        match environment.VisibleProjections |> Map.tryFind projectionName with
                        | Some nestedProjection ->
                            match nestedProjection.Body with
                            | Some(ProjectionAccessors _) ->
                                [ makeDiagnostic DiagnosticCode.ProjectionDefinitionUnsupported "A selector projection yield must denote a stable place, not an accessor bundle." ]
                            | _ ->
                                []
                        | None ->
                            [ makeDiagnostic DiagnosticCode.ProjectionDefinitionUnsupported "A selector projection yield must denote a stable place." ]
                    | ProjectionYield _ ->
                        [ makeDiagnostic DiagnosticCode.ProjectionDefinitionUnsupported "A selector projection yield must denote a stable place rooted in a place binder." ]
                    | ProjectionIfThenElse(_, whenTrue, whenFalse) ->
                        selectorYieldDiagnostics whenTrue @ selectorYieldDiagnostics whenFalse
                    | ProjectionMatch(_, cases) ->
                        cases |> List.collect (fun caseClause -> selectorYieldDiagnostics caseClause.Body)
                    | ProjectionAccessors _ ->
                        []

                let expandedDiagnostics =
                    if List.isEmpty placeBinders then
                        []
                    else
                        match declaration.Body with
                        | Some(ProjectionAccessors clauses) ->
                            let placeCountDiagnostics =
                                if List.length placeBinders = 1 then
                                    []
                                else
                                    [ makeDiagnostic DiagnosticCode.ProjectionDefinitionUnsupported "An expanded accessor projection must declare exactly one place binder." ]

                            let duplicateDiagnostics =
                                clauses
                                |> List.map (function
                                    | ProjectionGet _ -> "get"
                                    | ProjectionInout _ -> "inout"
                                    | ProjectionSet _ -> "set"
                                    | ProjectionSink _ -> "sink")
                                |> List.countBy id
                                |> List.choose (fun (kind, count) ->
                                    if count > 1 then
                                        Some(makeDiagnostic DiagnosticCode.ProjectionDefinitionUnsupported $"Projection accessor clause '{kind}' is declared more than once.")
                                    else
                                        None)

                            placeCountDiagnostics @ duplicateDiagnostics
                        | Some body ->
                            selectorYieldDiagnostics body
                        | None ->
                            []

                missingPlaceDiagnostics @ expandedDiagnostics

            let duplicateDeclarationDiagnostics =
                let duplicateNames messagePrefix names =
                    names
                    |> List.countBy id
                    |> List.choose (fun (name, count) ->
                        if count > 1 then
                            Some(makeDiagnostic DiagnosticCode.DuplicateDeclaration $"{messagePrefix} '{name}' is declared more than once in module '{moduleName}'.")
                        else
                            None)

                let termDefinitions =
                    let expectedTerms =
                        frontendModule.Declarations
                        |> List.choose (function
                            | ExpectDeclarationNode (ExpectTermDeclaration declaration) -> Some declaration.Name
                            | _ -> None)
                        |> Set.ofList

                    frontendModule.Declarations
                    |> List.choose (function
                        | LetDeclaration definition ->
                            definition.Name
                            |> Option.filter (fun name -> not (Set.contains name expectedTerms))
                        | _ -> None)

                let typeDeclarations =
                    frontendModule.Declarations
                    |> List.choose (function
                        | DataDeclarationNode declaration -> Some declaration.Name
                        | TypeAliasNode declaration -> Some declaration.Name
                        | _ -> None)

                let constructorDeclarations =
                    frontendModule.Declarations
                    |> List.collect (function
                        | DataDeclarationNode declaration ->
                            declaration.Constructors
                            |> List.map (fun constructor -> constructor.Name)
                            |> List.filter (fun name -> not (String.Equals(name, "<anonymous>", StringComparison.Ordinal)))
                        | _ -> [])

                duplicateNames "Term declaration" termDefinitions
                @ duplicateNames "Type declaration" typeDeclarations
                @ duplicateNames "Constructor declaration" constructorDeclarations

            let totalityAssertionDiagnostics =
                let allowAssertTerminates = moduleHasAttribute frontendModule "allow_assert_terminates"
                let allowAssertReducible = moduleHasAttribute frontendModule "allow_assert_reducible"

                frontendModule.Declarations
                |> List.choose (function
                    | LetDeclaration definition ->
                        match definition.TotalityAssertion with
                        | Some AssertTerminatesAssertion when not allowAssertTerminates ->
                            let bindingName = definition.Name |> Option.defaultValue "<anonymous>"
                            Some(makeDiagnostic DiagnosticCode.FrontendValidation $"Declaration '{bindingName}' uses assertTerminates/assertTotal without enabling module attribute 'allow_assert_terminates'.")
                        | Some AssertReducibleAssertion when not allowAssertReducible ->
                            let bindingName = definition.Name |> Option.defaultValue "<anonymous>"
                            Some(makeDiagnostic DiagnosticCode.FrontendValidation $"Declaration '{bindingName}' uses assertReducible without enabling module attribute 'allow_assert_reducible'.")
                        | _ ->
                            None
                    | _ ->
                        None)

            let typeAliasCycleDiagnostics =
                let rec referencedTypeNames typeExpr =
                    seq {
                        match typeExpr with
                        | TypeLevelLiteral _ ->
                            ()
                        | TypeUniverse None ->
                            ()
                        | TypeUniverse(Some universeExpr) ->
                            yield! referencedTypeNames universeExpr
                        | TypeIntrinsic _ ->
                            ()
                        | TypeApply(callee, arguments) ->
                            yield! referencedTypeNames callee

                            for argument in arguments do
                                yield! referencedTypeNames argument
                        | TypeLambda(_, parameterSort, body) ->
                            yield! referencedTypeNames parameterSort
                            yield! referencedTypeNames body
                        | TypeDelay inner
                        | TypeMemo inner
                        | TypeForce inner ->
                            yield! referencedTypeNames inner
                        | TypeProject(target, _) ->
                            yield! referencedTypeNames target
                        | TypeName(name, arguments) ->
                            match name with
                            | [ localName ] -> yield localName
                            | _ -> ()

                            for argument in arguments do
                                yield! referencedTypeNames argument
                        | TypeVariable _ ->
                            ()
                        | TypeArrow(_, parameterType, resultType) ->
                            yield! referencedTypeNames parameterType
                            yield! referencedTypeNames resultType
                        | TypeEquality(left, right) ->
                            yield! referencedTypeNames left
                            yield! referencedTypeNames right
                        | TypeCapture(inner, _) ->
                            yield! referencedTypeNames inner
                        | TypeEffectRow(entries, tail) ->
                            for entry in entries do
                                yield! referencedTypeNames entry.Label
                                yield! referencedTypeNames entry.Effect

                            match tail with
                            | Some tailType -> yield! referencedTypeNames tailType
                            | None -> ()
                        | TypeRecord fields ->
                            for field in fields do
                                yield! referencedTypeNames field.Type
                        | TypeUnion members ->
                            for memberType in members do
                                yield! referencedTypeNames memberType
                    }

                let localAliases =
                    frontendModule.Declarations
                    |> List.choose (function
                        | TypeAliasNode declaration -> tryParseTypeAliasInfo moduleName declaration
                        | _ -> None)
                    |> Map.ofList

                let aliasDependencies =
                    localAliases
                    |> Map.map (fun _ aliasInfo ->
                        referencedTypeNames aliasInfo.Body
                        |> Seq.filter (fun referencedName ->
                            Map.containsKey referencedName localAliases
                            && not (List.contains referencedName aliasInfo.Parameters))
                        |> Set.ofSeq)

                let rec reaches start current visited =
                    aliasDependencies
                    |> Map.tryFind current
                    |> Option.defaultValue Set.empty
                    |> Seq.exists (fun dependency ->
                        String.Equals(dependency, start, StringComparison.Ordinal)
                        || (not (Set.contains dependency visited)
                            && reaches start dependency (Set.add dependency visited)))

                aliasDependencies
                |> Map.keys
                |> Seq.filter (fun aliasName -> reaches aliasName aliasName (Set.singleton aliasName))
                |> Seq.distinct
                |> Seq.map (fun aliasName ->
                    makeDiagnostic DiagnosticCode.RecursiveTypeAlias $"Type alias '{aliasName}' recursively depends on itself.")
                |> Seq.toList

            let malformedConstructorDiagnostics =
                let declarationKeywordNames =
                    Set.ofList [ "data"; "expect"; "export"; "import"; "instance"; "let"; "module"; "opaque"; "private"; "projection"; "public"; "trait"; "type" ]

                let exposesRuntimeTypeField (constructor: DataConstructor) =
                    let exposedFieldTypes =
                        match constructor.Parameters with
                        | Some parameters ->
                            parameters
                            |> List.choose (fun parameter ->
                                if parameter.ParameterIsImplicit || parameter.ParameterQuantity = Some QuantityZero then
                                    None
                                else
                                    TypeSignatures.parseType parameter.ParameterTypeTokens)
                        | None ->
                            TypeSignatures.constructorParameterTokenGroups constructor
                            |> List.choose (fun parameterTokens ->
                                match parameterTokens with
                                | { Kind = AtSign } :: _ ->
                                    None
                                | { Kind = IntegerLiteral; Text = "0" } :: _ ->
                                    None
                                | _ ->
                                    let typeTokens =
                                        match parameterTokens |> List.tryFindIndex (fun token -> token.Kind = Colon) with
                                        | Some colonIndex when colonIndex + 1 < parameterTokens.Length ->
                                            parameterTokens |> List.skip (colonIndex + 1)
                                        | _ ->
                                            parameterTokens

                                    TypeSignatures.parseType typeTokens)

                    exposedFieldTypes
                    |> List.exists (function
                        | TypeUniverse _ -> true
                        | TypeIntrinsic UniverseClassifier -> true
                        | TypeName([ "Type" ], _) -> true
                        | _ -> false)

                let constructorHasMalformedShape (constructor: DataConstructor) =
                    let significant = significantTokens constructor.Tokens

                    let hasUnbalancedDelimiters tokens =
                        let mutable parenDepth = 0
                        let mutable braceDepth = 0
                        let mutable bracketDepth = 0
                        let mutable setBraceDepth = 0

                        for token in tokens do
                            match token.Kind with
                            | LeftParen -> parenDepth <- parenDepth + 1
                            | RightParen -> parenDepth <- max 0 (parenDepth - 1)
                            | LeftBrace -> braceDepth <- braceDepth + 1
                            | RightBrace -> braceDepth <- max 0 (braceDepth - 1)
                            | LeftBracket -> bracketDepth <- bracketDepth + 1
                            | RightBracket -> bracketDepth <- max 0 (bracketDepth - 1)
                            | LeftSetBrace -> setBraceDepth <- setBraceDepth + 1
                            | RightSetBrace -> setBraceDepth <- max 0 (setBraceDepth - 1)
                            | _ -> ()

                        parenDepth <> 0 || braceDepth <> 0 || bracketDepth <> 0 || setBraceDepth <> 0

                    if hasUnbalancedDelimiters significant then
                        false
                    else
                        let stripConstructorName tokens =
                            let isSymbolToken token =
                                match token.Kind with
                                | Operator
                                | Colon
                                | Equals -> true
                                | _ -> false

                            match tokens with
                            | { Kind = LeftParen } :: rest ->
                                let rec loop remaining =
                                    match remaining with
                                    | { Kind = RightParen } :: tail ->
                                        tail
                                    | token :: tail when isSymbolToken token ->
                                        loop tail
                                    | _ ->
                                        tokens

                                loop rest
                            | _nameToken :: rest ->
                                rest
                            | [] ->
                                []

                        let remainingTokens =
                            match significant with
                            | { Kind = Operator; Text = "|" } :: rest -> stripConstructorName rest
                            | _ -> stripConstructorName significant

                        match constructor.Parameters, remainingTokens with
                        | Some _, _ ->
                            false
                        | None, [] ->
                            false
                        | None, { Kind = Colon } :: typeTokens ->
                            TypeSignatures.parseScheme typeTokens |> Option.isNone
                        | None, _ ->
                            List.isEmpty (TypeSignatures.constructorFieldTypes constructor)

                frontendModule.Declarations
                |> List.collect (function
                    | DataDeclarationNode declaration ->
                        declaration.Constructors
                        |> List.choose (fun constructor ->
                            if Set.contains constructor.Name declarationKeywordNames then
                                Some(makeDiagnostic DiagnosticCode.MalformedConstructorDeclaration $"Constructor declaration in data type '{declaration.Name}' starts with declaration keyword '{constructor.Name}'.")
                            elif exposesRuntimeTypeField constructor then
                                Some(makeDiagnostic DiagnosticCode.MalformedConstructorDeclaration $"Constructor '{constructor.Name}' exposes runtime field metadata of type 'Type'.")
                            elif constructorHasMalformedShape constructor then
                                Some(makeDiagnostic DiagnosticCode.MalformedConstructorDeclaration $"Constructor declaration '{constructor.Name}' in data type '{declaration.Name}' is malformed.")
                            else
                                None)
                    | _ ->
                        [])

            let patternDuplicateDiagnostics pattern =
                let duplicateNames current =
                    collectPatternNames current
                    |> List.countBy id
                    |> List.choose (fun (name, count) ->
                        if count > 1 then
                            Some(makeDiagnostic DiagnosticCode.DuplicatePatternBinder $"Pattern binder '{name}' is bound more than once in the same pattern.")
                        else
                            None)

                let rec loop current =
                    match current with
                    | AsPattern(_, inner) ->
                        duplicateNames current @ loop inner
                    | TypedPattern(inner, _) ->
                        duplicateNames current @ loop inner
                    | OrPattern alternatives ->
                        alternatives |> List.collect loop
                    | ConstructorPattern(_, arguments) ->
                        duplicateNames current @ (arguments |> List.collect loop)
                    | NamedConstructorPattern(_, fields) ->
                        duplicateNames current @ (fields |> List.collect (fun field -> loop field.Pattern))
                    | TuplePattern elements ->
                        duplicateNames current @ (elements |> List.collect loop)
                    | VariantPattern _ ->
                        duplicateNames current
                    | AnonymousRecordPattern(fields, _) ->
                        duplicateNames current @ (fields |> List.collect (fun field -> loop field.Pattern))
                    | WildcardPattern
                    | NamePattern _
                    | LiteralPattern _ ->
                        []

                loop pattern |> List.distinctBy (fun diagnostic -> diagnostic.Message)

            let unsignedRecursiveBindingDiagnostics =
                let signatureNames =
                    frontendModule.Declarations
                    |> List.choose (function
                        | SignatureDeclaration declaration -> Some declaration.Name
                        | _ -> None)
                    |> Set.ofList

                let patternNames pattern =
                    collectPatternNames pattern |> Set.ofList

                let parameterNames (parameters: Parameter list) =
                    parameters |> List.map (fun parameter -> parameter.Name) |> Set.ofList

                let rec expressionReferences target shadowed expression =
                    let references = expressionReferences target shadowed

                    match expression with
                    | Name [ name ] ->
                        String.Equals(name, target, StringComparison.Ordinal)
                        && not (Set.contains name shadowed)
                    | SyntaxQuote inner
                    | SyntaxSplice inner
                    | TopLevelSyntaxSplice inner
                    | CodeQuote inner
                    | CodeSplice inner ->
                        references inner
                    | Handle(_, label, body, returnClause, operationClauses) ->
                        references label
                        || references body
                        || references returnClause.Body
                        || (operationClauses |> List.exists (fun clause -> references clause.Body))
                    | Apply(callee, arguments) ->
                        references callee || (arguments |> List.exists references)
                    | LocalLet(binding, value, body) ->
                        references value
                        || expressionReferences target (Set.union shadowed (patternNames binding.Pattern)) body
                    | LocalSignature(declaration, body) ->
                        if String.Equals(declaration.Name, target, StringComparison.Ordinal) then
                            false
                        else
                            expressionReferences target (Set.add declaration.Name shadowed) body
                    | LocalTypeAlias(_, body) ->
                        references body
                    | LocalScopedEffect(_, body) ->
                        references body
                    | Lambda(parameters, body) ->
                        expressionReferences target (Set.union shadowed (parameterNames parameters)) body
                    | IfThenElse(condition, whenTrue, whenFalse) ->
                        references condition || references whenTrue || references whenFalse
                    | Match(scrutinee, cases) ->
                        references scrutinee
                        || (cases
                            |> List.exists (fun caseClause ->
                                let caseShadowed = Set.union shadowed (patternNames caseClause.Pattern)
                                (caseClause.Guard |> Option.exists (expressionReferences target caseShadowed))
                                || expressionReferences target caseShadowed caseClause.Body))
                    | RecordLiteral fields ->
                        fields |> List.exists (fun field -> references field.Value)
                    | Seal(value, _) ->
                        references value
                    | RecordUpdate(receiver, fields) ->
                        references receiver || (fields |> List.exists (fun field -> references field.Value))
                    | MemberAccess(receiver, _, arguments) ->
                        references receiver || (arguments |> List.exists references)
                    | SafeNavigation(receiver, navigation) ->
                        references receiver || (navigation.Arguments |> List.exists references)
                    | TagTest(receiver, _) ->
                        references receiver
                    | Do statements ->
                        doStatementsReference target shadowed statements
                    | MonadicSplice inner
                    | ExplicitImplicitArgument inner
                    | InoutArgument inner
                    | Unary(_, inner) ->
                        references inner
                    | NamedApplicationBlock fields ->
                        fields |> List.exists (fun field -> references field.Value)
                    | Binary(left, _, right)
                    | Elvis(left, right) ->
                        references left || references right
                    | PrefixedString(_, parts) ->
                        parts
                        |> List.exists (function
                            | StringText _ -> false
                            | StringInterpolation(inner, _) -> references inner)
                    | Literal _
                    | NumericLiteral _
                    | KindQualifiedName _
                    | Name _ ->
                        false

                and doStatementsReference target shadowed statements =
                    match statements with
                    | [] ->
                        false
                    | statement :: rest ->
                        match statement with
                        | DoLet(binding, expression)
                        | DoBind(binding, expression)
                        | DoUsing(binding, expression) ->
                            expressionReferences target shadowed expression
                            || doStatementsReference target (Set.union shadowed (patternNames binding.Pattern)) rest
                        | DoLetQuestion(binding, expression, failure) ->
                            let failureReferences =
                                failure
                                |> Option.exists (fun block ->
                                    doStatementsReference target (Set.union shadowed (patternNames block.ResiduePattern.Pattern)) block.Body)

                            expressionReferences target shadowed expression
                            || failureReferences
                            || doStatementsReference target (Set.union shadowed (patternNames binding.Pattern)) rest
                        | DoVar(name, expression) ->
                            expressionReferences target shadowed expression
                            || doStatementsReference target (Set.add name shadowed) rest
                        | DoAssign(_, expression)
                        | DoDefer expression
                        | DoExpression expression
                        | DoReturn expression ->
                            expressionReferences target shadowed expression
                            || doStatementsReference target shadowed rest
                        | DoIf(condition, whenTrue, whenFalse) ->
                            expressionReferences target shadowed condition
                            || doStatementsReference target shadowed whenTrue
                            || doStatementsReference target shadowed whenFalse
                            || doStatementsReference target shadowed rest
                        | DoWhile(condition, body) ->
                            expressionReferences target shadowed condition
                            || doStatementsReference target shadowed body
                            || doStatementsReference target shadowed rest

                frontendModule.Declarations
                |> List.choose (function
                    | LetDeclaration definition
                        when definition.Name.IsSome
                             && definition.Body.IsSome
                             && not (Set.contains definition.Name.Value signatureNames) ->
                        let name = definition.Name.Value
                        let shadowed = parameterNames definition.Parameters

                        if expressionReferences name shadowed definition.Body.Value then
                            Some(makeDiagnostic DiagnosticCode.RecursionRequiresSignature $"Top-level binding '{name}' is recursive but has no preceding signature declaration.")
                        else
                            None
                    | _ ->
                        None)

            let trivialRecursiveCycleDiagnostics =
                frontendModule.Declarations
                |> List.choose (function
                    | LetDeclaration definition
                        when definition.Name.IsSome
                             && definition.Body.IsSome
                             && List.isEmpty definition.Parameters ->
                        let name = definition.Name.Value

                        match definition.Body.Value with
                        | Name [ referencedName ] when String.Equals(referencedName, name, StringComparison.Ordinal) ->
                            Some(
                                makeDiagnostic
                                    DiagnosticCode.RecursionRequiresSignature
                                    $"Recursive cycle for binding '{name}' is not total and must be rejected."
                            )
                        | _ ->
                            None
                    | _ ->
                        None)

            let expressionPatternDiagnostics expression =
                let rec validateExpression current =
                    match current with
                    | SyntaxQuote inner
                    | SyntaxSplice inner
                    | TopLevelSyntaxSplice inner
                    | CodeQuote inner
                    | CodeSplice inner ->
                        validateExpression inner
                    | LocalLet(binding, value, body) ->
                        patternDuplicateDiagnostics binding.Pattern @ validateExpression value @ validateExpression body
                    | LocalSignature(_, body) ->
                        validateExpression body
                    | LocalTypeAlias(_, body) ->
                        validateExpression body
                    | LocalScopedEffect(_, body) ->
                        validateExpression body
                    | Handle(_, label, body, returnClause, operationClauses) ->
                        validateExpression label
                        @ validateExpression body
                        @ validateExpression returnClause.Body
                        @ (operationClauses |> List.collect (fun clause -> validateExpression clause.Body))
                    | Lambda(_, body) ->
                        validateExpression body
                    | IfThenElse(condition, whenTrue, whenFalse) ->
                        validateExpression condition @ validateExpression whenTrue @ validateExpression whenFalse
                    | Match(scrutinee, cases) ->
                        validateExpression scrutinee
                        @ (cases
                           |> List.collect (fun caseClause ->
                               patternDuplicateDiagnostics caseClause.Pattern
                               @ (caseClause.Guard |> Option.map validateExpression |> Option.defaultValue [])
                               @ validateExpression caseClause.Body))
                    | RecordLiteral fields ->
                        fields |> List.collect (fun field -> validateExpression field.Value)
                    | Seal(value, _) ->
                        validateExpression value
                    | RecordUpdate(receiver, fields) ->
                        validateExpression receiver @ (fields |> List.collect (fun field -> validateExpression field.Value))
                    | MemberAccess(receiver, _, arguments) ->
                        validateExpression receiver @ (arguments |> List.collect validateExpression)
                    | SafeNavigation(receiver, navigation) ->
                        validateExpression receiver @ (navigation.Arguments |> List.collect validateExpression)
                    | TagTest(receiver, _) ->
                        validateExpression receiver
                    | Do statements ->
                        validateDo statements
                    | MonadicSplice inner
                    | ExplicitImplicitArgument inner
                    | InoutArgument inner
                    | Unary(_, inner) ->
                        validateExpression inner
                    | NamedApplicationBlock fields ->
                        fields |> List.collect (fun field -> validateExpression field.Value)
                    | Binary(left, _, right)
                    | Elvis(left, right) ->
                        validateExpression left @ validateExpression right
                    | Apply(callee, arguments) ->
                        validateExpression callee @ (arguments |> List.collect validateExpression)
                    | PrefixedString(_, parts) ->
                        parts
                        |> List.collect (function
                            | StringText _ -> []
                            | StringInterpolation(inner, _) -> validateExpression inner)
                    | Literal _
                    | NumericLiteral _
                    | KindQualifiedName _
                    | Name _ ->
                        []

                and validateDo statements =
                    statements
                    |> List.collect (function
                        | DoLet(binding, expression)
                        | DoBind(binding, expression)
                        | DoUsing(binding, expression) ->
                            patternDuplicateDiagnostics binding.Pattern @ validateExpression expression
                        | DoLetQuestion(binding, expression, failure) ->
                            patternDuplicateDiagnostics binding.Pattern
                            @ validateExpression expression
                            @ (failure
                               |> Option.map (fun block ->
                                   patternDuplicateDiagnostics block.ResiduePattern.Pattern @ validateDo block.Body)
                               |> Option.defaultValue [])
                        | DoVar(_, expression)
                        | DoAssign(_, expression)
                        | DoDefer expression
                        | DoExpression expression
                        | DoReturn expression ->
                            validateExpression expression
                        | DoIf(condition, whenTrue, whenFalse) ->
                            validateExpression condition @ validateDo whenTrue @ validateDo whenFalse
                        | DoWhile(condition, body) ->
                            validateExpression condition @ validateDo body)

                validateExpression expression

            let expressionNameResolutionDiagnostics (definition: LetDefinition) expression =
                []

            let constructorDefaultDiagnostics =
                let topLevelNames =
                    frontendModule.Declarations
                    |> List.collect (function
                        | SignatureDeclaration declaration -> [ declaration.Name ]
                        | LetDeclaration declaration -> declaration.Name |> Option.toList
                        | ProjectionDeclarationNode declaration -> [ declaration.Name ]
                        | DataDeclarationNode declaration ->
                            declaration.Name :: (declaration.Constructors |> List.map (fun constructor -> constructor.Name))
                        | TypeAliasNode declaration -> [ declaration.Name ]
                        | TraitDeclarationNode declaration -> [ declaration.Name ]
                        | ExpectDeclarationNode (ExpectTermDeclaration declaration) -> [ declaration.Name ]
                        | ExpectDeclarationNode (ExpectTypeDeclaration declaration) -> [ declaration.Name ]
                        | ExpectDeclarationNode (ExpectTraitDeclaration declaration) -> [ declaration.Name ]
                        | _ -> [])
                    |> Set.ofList

                let baseVisibleNames =
                    let preludeContract = IntrinsicCatalog.bundledPreludeExpectContract ()

                    [ topLevelNames
                      preludeContract.TermNames
                      preludeContract.TypeNames
                      preludeContract.TraitNames
                      Set.ofList Stdlib.FixedPreludeConstructors
                      environment.VisibleBindings |> Map.keys |> Set.ofSeq
                      environment.VisibleConstructors |> Map.keys |> Set.ofSeq
                      environment.VisibleModules
                      environment.VisibleStaticObjects |> Map.keys |> Set.ofSeq
                      environment.VisibleTypeFacets |> Map.keys |> Set.ofSeq
                      environment.VisibleTraits |> Map.keys |> Set.ofSeq ]
                    |> Set.unionMany
                    |> Set.remove "<anonymous>"

                let diagnosticsForDefault parameters index parameterName parameterTypeTokens defaultValue =
                    let earlierParameters = parameters |> List.take index
                    let forbiddenNames =
                        parameters
                        |> List.skip index
                        |> List.choose (fun candidate -> candidate.ParameterName)
                        |> Set.ofList

                    let localTypes =
                        earlierParameters
                        |> List.choose (fun earlier ->
                            match earlier.ParameterName, TypeSignatures.parseType earlier.ParameterTypeTokens with
                            | Some name, Some parameterType -> Some(name, parameterType)
                            | _ -> None)
                        |> Map.ofList

                    let visibleNames =
                        earlierParameters
                        |> List.choose (fun earlier -> earlier.ParameterName)
                        |> Set.ofList
                        |> Set.union baseVisibleNames

                    let nameDiagnostics =
                        ResourceCheckingSurface.expressionNames defaultValue
                        |> Seq.distinct
                        |> Seq.choose (fun referencedName ->
                            if Set.contains referencedName forbiddenNames then
                                Some(
                                    makeDiagnostic
                                        DiagnosticCode.NameUnresolved
                                        $"Default expression for constructor parameter '{parameterName}' cannot reference '{referencedName}' before it is bound."
                                )
                            elif not (Set.contains referencedName visibleNames) then
                                Some(makeDiagnostic DiagnosticCode.NameUnresolved $"Name '{referencedName}' is not in scope.")
                            else
                                None)
                        |> Seq.toList

                    let typeDiagnostics =
                        match TypeSignatures.parseType parameterTypeTokens, inferValidationExpressionType environment (ref 0) localTypes defaultValue with
                        | Some expectedType, Some actualType
                            when not (
                                TypeSignatures.definitionallyEqual
                                    (normalizeTypeAliases environment.VisibleTypeAliases expectedType)
                                    (normalizeTypeAliases environment.VisibleTypeAliases actualType)
                            ) ->
                            [ makeDiagnostic
                                DiagnosticCode.TypeEqualityMismatch
                                $"Default expression for constructor parameter '{parameterName}' has type '{TypeSignatures.toText actualType}', but '{TypeSignatures.toText expectedType}' was expected." ]
                        | Some _, None when List.isEmpty nameDiagnostics ->
                            [ makeDiagnostic
                                DiagnosticCode.TypeEqualityMismatch
                                $"Default expression for constructor parameter '{parameterName}' could not be checked against its declared type." ]
                        | _ ->
                            []

                    nameDiagnostics @ typeDiagnostics

                let diagnosticsForConstructor (constructor: DataConstructor) =
                    match constructor.Parameters with
                    | Some parameters ->
                        parameters
                        |> List.mapi (fun index parameter -> index, parameter)
                        |> List.collect (fun (index, parameter) ->
                            match parameter.ParameterName, parameter.DefaultValue with
                            | Some parameterName, Some defaultValue ->
                                diagnosticsForDefault parameters index parameterName parameter.ParameterTypeTokens defaultValue
                            | _ ->
                                [])
                    | None ->
                        []

                frontendModule.Declarations
                |> List.collect (function
                    | DataDeclarationNode declaration ->
                        declaration.Constructors |> List.collect diagnosticsForConstructor
                    | _ ->
                        [])

            let structuralDiagnostics =
                duplicateDeclarationDiagnostics
                @ totalityAssertionDiagnostics
                @ typeAliasCycleDiagnostics
                @ malformedConstructorDiagnostics
                @ unsignedRecursiveBindingDiagnostics
                @ trivialRecursiveCycleDiagnostics
                @ constructorDefaultDiagnostics

            if not (List.isEmpty typeAliasCycleDiagnostics) then
                structuralDiagnostics
            else
                structuralDiagnostics
                @ (frontendModule.Declarations
                |> List.collect (function
                    | TypeAliasNode declaration ->
                        declaration.BodyTokens
                        |> Option.bind tryParseRecordSurfaceInfo
                        |> Option.map (fun recordInfo ->
                            let duplicateDiagnostics =
                                recordInfo.Fields
                                |> List.countBy (fun field -> field.Name)
                                |> List.choose (fun (name, count) ->
                                    if count > 1 then
                                        Some(makeDiagnostic DiagnosticCode.RecordDuplicateField $"Record field '{name}' is declared more than once.")
                                    else
                                        None)

                            let cycleDiagnostics =
                                let parsedFields =
                                    recordInfo.Fields
                                    |> List.map (fun field ->
                                        parseType field.TypeTokens
                                        |> Option.map (fun parsedType ->
                                            { Name = field.Name
                                              Quantity = QuantityOmega
                                              Type = parsedType }))

                                if parsedFields |> List.forall Option.isSome then
                                    let typeExpr = TypeRecord(parsedFields |> List.choose id)

                                    if hasCyclicRecordDependencies typeExpr then
                                        [ makeDiagnostic DiagnosticCode.RecordDependencyCycle "Record type field dependencies must be acyclic." ]
                                    else
                                        []
                                else
                                    []

                            duplicateDiagnostics @ cycleDiagnostics)
                        |> Option.defaultValue []
                    | LetDeclaration definition ->
                        let scheme =
                            definition.Name
                            |> Option.bind (fun name -> Map.tryFind name environment.VisibleBindings)
                            |> Option.map (fun bindingInfo -> bindingInfo.Scheme)

                        validateBuiltInExpressionsForBinding
                            environment
                            knownSurfaceTermNames
                            allowUnresolvedCallDiagnostics
                            definition
                            scheme
                        @ (definition.Body
                           |> Option.map (fun body ->
                               expressionPatternDiagnostics body
                               @ expressionNameResolutionDiagnostics definition body)
                           |> Option.defaultValue [])
                    | ProjectionDeclarationNode declaration ->
                        validateProjectionDeclaration declaration
                    | _ ->
                        [])))

    let private makeSyntheticBindingDeclaration
        (name: string)
        (parameters: KCoreParameter list)
        (returnType: TypeExpr option)
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
                  ReturnType = returnType
                  ReturnTypeText = returnType |> Option.map typeTextOf
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

        let rec substituteZeroArityLocalTypeAliasInExpression (alias: TypeAlias) expression =
            let substituteTypeTokens tokens =
                let aliasBodyTokens = alias.BodyTokens |> Option.defaultValue []
                let tokenArray = tokens |> List.toArray
                let rewritten = ResizeArray<Token>()

                let isStandaloneAlias index =
                    let token = tokenArray[index]
                    let previousIsDot = index > 0 && tokenArray[index - 1].Kind = Dot
                    let nextIsDot = index + 1 < tokenArray.Length && tokenArray[index + 1].Kind = Dot

                    Token.isName token
                    && String.Equals(SyntaxFacts.trimIdentifierQuotes token.Text, alias.Name, StringComparison.Ordinal)
                    && not previousIsDot
                    && not nextIsDot

                for index = 0 to tokenArray.Length - 1 do
                    if isStandaloneAlias index then
                        rewritten.AddRange(aliasBodyTokens)
                    else
                        rewritten.Add(tokenArray[index])

                List.ofSeq rewritten

            let substituteParameter (parameter: Parameter) =
                { parameter with
                    TypeTokens = parameter.TypeTokens |> Option.map substituteTypeTokens }

            let substituteBinding (binding: SurfaceBindPattern) =
                { binding with
                    TypeTokens = binding.TypeTokens |> Option.map substituteTypeTokens }

            let rec substitute current =
                match current with
                | SyntaxQuote inner ->
                    SyntaxQuote(substitute inner)
                | SyntaxSplice inner ->
                    SyntaxSplice(substitute inner)
                | TopLevelSyntaxSplice inner ->
                    TopLevelSyntaxSplice(substitute inner)
                | CodeQuote inner ->
                    CodeQuote(substitute inner)
                | CodeSplice inner ->
                    CodeSplice(substitute inner)
                | Handle(isDeep, label, body, returnClause, operationClauses) ->
                    let substituteClause (clause: SurfaceEffectHandlerClause) =
                        { clause with
                            Body = substitute clause.Body }

                    Handle(
                        isDeep,
                        substitute label,
                        substitute body,
                        substituteClause returnClause,
                        operationClauses |> List.map substituteClause
                    )
                | LocalLet(binding, value, body) ->
                    LocalLet(substituteBinding binding, substitute value, substitute body)
                | LocalSignature(declaration, body) ->
                    LocalSignature({ declaration with TypeTokens = substituteTypeTokens declaration.TypeTokens }, substitute body)
                | LocalTypeAlias(declaration, body) ->
                    LocalTypeAlias(
                        { declaration with
                            HeaderTokens = substituteTypeTokens declaration.HeaderTokens
                            BodyTokens = declaration.BodyTokens |> Option.map substituteTypeTokens },
                        substitute body
                    )
                | LocalScopedEffect(declaration, body) ->
                    LocalScopedEffect(declaration, substitute body)
                | Lambda(parameters, body) ->
                    Lambda(parameters |> List.map substituteParameter, substitute body)
                | IfThenElse(condition, whenTrue, whenFalse) ->
                    IfThenElse(substitute condition, substitute whenTrue, substitute whenFalse)
                | Match(scrutinee, cases) ->
                    Match(
                        substitute scrutinee,
                        cases
                        |> List.map (fun caseClause ->
                            { caseClause with
                                Guard = caseClause.Guard |> Option.map substitute
                                Body = substitute caseClause.Body })
                    )
                | RecordLiteral fields ->
                    RecordLiteral(fields |> List.map (fun field -> { field with Value = substitute field.Value }))
                | Seal(value, ascriptionTokens) ->
                    Seal(substitute value, substituteTypeTokens ascriptionTokens)
                | RecordUpdate(receiver, fields) ->
                    RecordUpdate(substitute receiver, fields |> List.map (fun field -> { field with Value = substitute field.Value }))
                | MemberAccess(receiver, segments, arguments) ->
                    MemberAccess(substitute receiver, segments, arguments |> List.map substitute)
                | SafeNavigation(receiver, navigation) ->
                    SafeNavigation(substitute receiver, { navigation with Arguments = navigation.Arguments |> List.map substitute })
                | TagTest(receiver, constructorName) ->
                    TagTest(substitute receiver, constructorName)
                | Do statements ->
                    Do(statements |> List.map substituteDoStatement)
                | MonadicSplice inner ->
                    MonadicSplice(substitute inner)
                | Apply(callee, arguments) ->
                    Apply(substitute callee, arguments |> List.map substitute)
                | ExplicitImplicitArgument inner ->
                    ExplicitImplicitArgument(substitute inner)
                | NamedApplicationBlock fields ->
                    NamedApplicationBlock(fields |> List.map (fun field -> { field with Value = substitute field.Value }))
                | InoutArgument inner ->
                    InoutArgument(substitute inner)
                | Unary(operatorName, inner) ->
                    Unary(operatorName, substitute inner)
                | Binary(left, operatorName, right) ->
                    Binary(substitute left, operatorName, substitute right)
                | Elvis(left, right) ->
                    Elvis(substitute left, substitute right)
                | PrefixedString(prefix, parts) ->
                    PrefixedString(
                        prefix,
                        parts
                        |> List.map (function
                            | StringText text -> StringText text
                            | StringInterpolation(inner, format) -> StringInterpolation(substitute inner, format))
                    )
                | Literal _
                | NumericLiteral _
                | KindQualifiedName _
                | Name _ ->
                    current

            and substituteDoStatement statement =
                match statement with
                | DoLet(binding, expression) ->
                    DoLet(substituteBinding binding, substitute expression)
                | DoLetQuestion(binding, expression, failure) ->
                    let substitutedFailure =
                        failure
                        |> Option.map (fun failure ->
                            { failure with
                                ResiduePattern = substituteBinding failure.ResiduePattern
                                Body = failure.Body |> List.map substituteDoStatement })

                    DoLetQuestion(substituteBinding binding, substitute expression, substitutedFailure)
                | DoBind(binding, expression) ->
                    DoBind(substituteBinding binding, substitute expression)
                | DoVar(name, expression) ->
                    DoVar(name, substitute expression)
                | DoAssign(name, expression) ->
                    DoAssign(name, substitute expression)
                | DoUsing(binding, expression) ->
                    DoUsing(substituteBinding binding, substitute expression)
                | DoDefer expression ->
                    DoDefer(substitute expression)
                | DoIf(condition, whenTrue, whenFalse) ->
                    DoIf(substitute condition, whenTrue |> List.map substituteDoStatement, whenFalse |> List.map substituteDoStatement)
                | DoWhile(condition, body) ->
                    DoWhile(substitute condition, body |> List.map substituteDoStatement)
                | DoReturn expression ->
                    DoReturn(substitute expression)
                | DoExpression expression ->
                    DoExpression(substitute expression)

            substitute expression

        let annotateLambdaFromSignature (declaration: BindingSignature) value =
            match TypeSignatures.parseScheme declaration.TypeTokens, value with
            | Some signatureScheme, Lambda(lambdaParameters, lambdaBody) ->
                let parameterTypes, _ = TypeSignatures.schemeParts signatureScheme

                if List.length parameterTypes = List.length lambdaParameters then
                    let annotatedParameters =
                        List.zip lambdaParameters parameterTypes
                        |> List.map (fun (parameter, parameterType) ->
                            if parameter.TypeTokens.IsSome then
                                parameter
                            else
                                { parameter with
                                    TypeTokens = Some(typeTextTokens (TypeSignatures.toText parameterType)) })

                    Lambda(annotatedParameters, lambdaBody)
                else
                    value
            | _ ->
                value

        let rec normalizeLocalDeclarations expression =
            match expression with
            | SyntaxQuote inner ->
                SyntaxQuote(normalizeLocalDeclarations inner)
            | SyntaxSplice inner ->
                SyntaxSplice(normalizeLocalDeclarations inner)
            | TopLevelSyntaxSplice inner ->
                TopLevelSyntaxSplice(normalizeLocalDeclarations inner)
            | CodeQuote inner ->
                CodeQuote(normalizeLocalDeclarations inner)
            | CodeSplice inner ->
                CodeSplice(normalizeLocalDeclarations inner)
            | Handle(isDeep, label, body, returnClause, operationClauses) ->
                let normalizeClause (clause: SurfaceEffectHandlerClause) =
                    { clause with
                        Body = normalizeLocalDeclarations clause.Body }

                Handle(
                    isDeep,
                    normalizeLocalDeclarations label,
                    normalizeLocalDeclarations body,
                    normalizeClause returnClause,
                    operationClauses |> List.map normalizeClause
                )
            | LocalTypeAlias(declaration, body) ->
                match tryParseTypeAliasInfo environment.CurrentModuleName declaration with
                | Some(_, aliasInfo) when List.isEmpty aliasInfo.Parameters ->
                    body
                    |> substituteZeroArityLocalTypeAliasInExpression declaration
                    |> normalizeLocalDeclarations
                | _ ->
                    LocalTypeAlias(declaration, normalizeLocalDeclarations body)
            | LocalSignature(declaration, LocalLet(binding, value, body))
                when binding.Pattern = NamePattern declaration.Name ->
                LocalLet(binding, annotateLambdaFromSignature declaration (normalizeLocalDeclarations value), normalizeLocalDeclarations body)
            | LocalSignature(declaration, body) ->
                LocalSignature(declaration, normalizeLocalDeclarations body)
            | LocalLet(binding, value, body) ->
                LocalLet(binding, normalizeLocalDeclarations value, normalizeLocalDeclarations body)
            | LocalScopedEffect(declaration, body) ->
                LocalScopedEffect(declaration, normalizeLocalDeclarations body)
            | Lambda(parameters, body) ->
                Lambda(parameters, normalizeLocalDeclarations body)
            | IfThenElse(condition, whenTrue, whenFalse) ->
                IfThenElse(
                    normalizeLocalDeclarations condition,
                    normalizeLocalDeclarations whenTrue,
                    normalizeLocalDeclarations whenFalse
                )
            | Match(scrutinee, cases) ->
                Match(
                    normalizeLocalDeclarations scrutinee,
                    cases
                    |> List.map (fun caseClause ->
                        { caseClause with
                            Guard = caseClause.Guard |> Option.map normalizeLocalDeclarations
                            Body = normalizeLocalDeclarations caseClause.Body })
                )
            | RecordLiteral fields ->
                RecordLiteral(fields |> List.map (fun field -> { field with Value = normalizeLocalDeclarations field.Value }))
            | Seal(value, ascriptionTokens) ->
                Seal(normalizeLocalDeclarations value, ascriptionTokens)
            | RecordUpdate(receiver, fields) ->
                RecordUpdate(receiver |> normalizeLocalDeclarations, fields |> List.map (fun field -> { field with Value = normalizeLocalDeclarations field.Value }))
            | MemberAccess(receiver, segments, arguments) ->
                MemberAccess(normalizeLocalDeclarations receiver, segments, arguments |> List.map normalizeLocalDeclarations)
            | SafeNavigation(receiver, navigation) ->
                SafeNavigation(
                    normalizeLocalDeclarations receiver,
                    { navigation with
                        Arguments = navigation.Arguments |> List.map normalizeLocalDeclarations }
                )
            | TagTest(receiver, constructorName) ->
                TagTest(normalizeLocalDeclarations receiver, constructorName)
            | Do statements ->
                Do(statements |> List.map normalizeLocalDoStatement)
            | MonadicSplice inner ->
                MonadicSplice(normalizeLocalDeclarations inner)
            | Apply(callee, arguments) ->
                Apply(normalizeLocalDeclarations callee, arguments |> List.map normalizeLocalDeclarations)
            | ExplicitImplicitArgument inner ->
                ExplicitImplicitArgument(normalizeLocalDeclarations inner)
            | NamedApplicationBlock fields ->
                NamedApplicationBlock(fields |> List.map (fun field -> { field with Value = normalizeLocalDeclarations field.Value }))
            | InoutArgument inner ->
                InoutArgument(normalizeLocalDeclarations inner)
            | Unary(operatorName, inner) ->
                Unary(operatorName, normalizeLocalDeclarations inner)
            | Binary(left, operatorName, right) ->
                Binary(normalizeLocalDeclarations left, operatorName, normalizeLocalDeclarations right)
            | Elvis(left, right) ->
                Elvis(normalizeLocalDeclarations left, normalizeLocalDeclarations right)
            | PrefixedString(prefix, parts) ->
                PrefixedString(
                    prefix,
                    parts
                    |> List.map (function
                        | StringText text -> StringText text
                        | StringInterpolation(inner, format) -> StringInterpolation(normalizeLocalDeclarations inner, format))
                )
            | Literal _
            | NumericLiteral _
            | KindQualifiedName _
            | Name _ ->
                expression

        and normalizeLocalDoStatement statement =
            match statement with
            | DoLet(binding, expression) ->
                DoLet(binding, normalizeLocalDeclarations expression)
            | DoLetQuestion(binding, expression, failure) ->
                let normalizedFailure =
                    failure
                    |> Option.map (fun failure ->
                        { failure with
                            Body = failure.Body |> List.map normalizeLocalDoStatement })

                DoLetQuestion(binding, normalizeLocalDeclarations expression, normalizedFailure)
            | DoBind(binding, expression) ->
                DoBind(binding, normalizeLocalDeclarations expression)
            | DoVar(name, expression) ->
                DoVar(name, normalizeLocalDeclarations expression)
            | DoAssign(name, expression) ->
                DoAssign(name, normalizeLocalDeclarations expression)
            | DoUsing(binding, expression) ->
                DoUsing(binding, normalizeLocalDeclarations expression)
            | DoDefer expression ->
                DoDefer(normalizeLocalDeclarations expression)
            | DoIf(condition, whenTrue, whenFalse) ->
                DoIf(
                    normalizeLocalDeclarations condition,
                    whenTrue |> List.map normalizeLocalDoStatement,
                    whenFalse |> List.map normalizeLocalDoStatement
                )
            | DoWhile(condition, body) ->
                DoWhile(normalizeLocalDeclarations condition, body |> List.map normalizeLocalDoStatement)
            | DoReturn expression ->
                DoReturn(normalizeLocalDeclarations expression)
            | DoExpression expression ->
                DoExpression(normalizeLocalDeclarations expression)

        let body = normalizeLocalDeclarations body

        let rec inferExpressionType localTypes expression =
            match expression with
            | Literal literal ->
                Some(inferLiteralType literal)
            | NumericLiteral literal ->
                match SurfaceNumericLiteral.suffix literal with
                | Some suffixName ->
                    inferExpressionType localTypes (Apply(Name [ suffixName ], [ NumericLiteral(SurfaceNumericLiteral.withoutSuffix literal) ]))
                | None ->
                    Some(inferSurfaceNumericLiteralType literal)
            | SyntaxQuote inner ->
                inferExpressionType localTypes inner
                |> Option.map syntaxType
            | SyntaxSplice inner ->
                inferExpressionType localTypes inner
                |> Option.bind (function
                    | TypeName([ "Syntax" ], [ innerType ]) -> Some innerType
                    | _ -> None)
            | TopLevelSyntaxSplice inner ->
                inferExpressionType localTypes inner
                |> Option.bind (function
                    | TypeName([ "Syntax" ], [ innerType ]) -> Some innerType
                    | _ -> None)
            | CodeQuote inner ->
                inferExpressionType localTypes inner
                |> Option.map codeType
            | CodeSplice inner ->
                inferExpressionType localTypes inner
                |> Option.bind (function
                    | TypeName([ "Code" ], [ innerType ]) -> Some innerType
                    | _ -> None)
            | Name [ "True" ]
            | Name [ "False" ] ->
                Some boolType
            | KindQualifiedName _ ->
                tryResolveScopedStaticObject environment expression
                |> Option.bind (fun staticObject -> staticObject.Scheme)
                |> Option.map (fun scheme -> scheme.Body)
            | Name(root :: path) ->
                let ordinaryResolution =
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
                                    (tryResolveUniqueLocalImplicitByType environment localTypes)
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
                                    (tryResolveUniqueLocalImplicitByType environment localTypes)
                                    []
                                |> Option.map (fun preparedCall -> preparedCall.ResultType)
                            else
                                None))
                    |> Option.orElseWith (fun () ->
                        tryResolveScopedStaticObject environment (Name [ root ])
                        |> Option.bind (fun staticObject ->
                            if List.isEmpty path then
                                staticObject.Scheme |> Option.map (fun scheme -> scheme.Body)
                            else
                                None))
                    |> Option.bind (fun rootType -> tryProjectVisibleRecordType environment.VisibleTypeAliases rootType path)

                match path with
                | [ memberName ] ->
                    tryReceiverProjection environment localTypes root memberName
                    |> Option.map (fun projectionInfo -> projectionInfo.ReturnType)
                    |> Option.orElseWith (fun () ->
                        tryInferStaticConstructorCall
                            environment
                            freshCounter
                            (inferExpressionType localTypes)
                            (Name [ root ])
                            memberName
                            [])
                    |> Option.orElse ordinaryResolution
                | _ ->
                    ordinaryResolution
            | Name [] ->
                None
            | LocalSignature(_, body) ->
                inferExpressionType localTypes body
            | LocalTypeAlias(_, body) ->
                inferExpressionType localTypes body
            | LocalLet(binding, value, body) ->
                let bindingNames = collectPatternNames binding.Pattern

                let nextLocals =
                    match inferExpressionType localTypes value with
                    | Some valueType ->
                        extendBindingLocalTypes environment freshCounter localTypes binding (Some valueType)
                    | None ->
                        extendBindingLocalTypes
                            environment
                            freshCounter
                            localTypes
                            binding
                            (Some(inferFallbackLocalType freshCounter value))

                let bodyForInference =
                    match bindingNames, tryResolveScopedStaticObject environment value with
                    | [ aliasName ], Some staticObject ->
                        rewriteStaticObjectAliasUse aliasName staticObject body
                    | _ ->
                        body

                inferExpressionType nextLocals bodyForInference
            | LocalScopedEffect(declaration, body) ->
                withScopedEffectDeclaration declaration (fun () -> inferExpressionType localTypes body)
            | Handle(_, _, _, returnClause, _) ->
                inferExpressionType localTypes returnClause.Body
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
            | MemberAccess(receiver, segments, arguments) ->
                match segments with
                | [] ->
                    inferExpressionType localTypes receiver
                    |> Option.bind (fun receiverType ->
                        tryInferVisibleAppliedType environment.VisibleTypeAliases (inferExpressionType localTypes) receiverType arguments)
                | [ memberName ] ->
                    tryInferStaticConstructorCall
                        environment
                        freshCounter
                        (inferExpressionType localTypes)
                        receiver
                        memberName
                        arguments
                    |> Option.orElseWith (fun () ->
                        environment.VisibleBindings
                        |> Map.tryFind memberName
                        |> Option.bind (fun bindingInfo ->
                            tryBuildReceiverMethodArguments bindingInfo receiver arguments
                            |> Option.bind (fun receiverArguments ->
                                tryPrepareVisibleBindingCall
                                    environment
                                    freshCounter
                                    bindingInfo
                                    (inferExpressionType localTypes)
                                    (tryResolveUniqueLocalImplicitByType environment localTypes)
                                    receiverArguments
                                |> Option.map (fun preparedCall -> preparedCall.ResultType))))
                    |> Option.orElseWith (fun () ->
                        inferExpressionType localTypes receiver
                        |> Option.bind (fun receiverType ->
                            tryProjectVisibleRecordType environment.VisibleTypeAliases receiverType [ memberName ]
                            |> Option.bind (fun memberType ->
                                tryInferVisibleAppliedType
                                    environment.VisibleTypeAliases
                                    (inferExpressionType localTypes)
                                    memberType
                                    arguments)))
                | memberName :: rest ->
                    inferExpressionType localTypes (MemberAccess(MemberAccess(receiver, [ memberName ], []), rest, arguments))
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
            | ExplicitImplicitArgument inner ->
                inferExpressionType localTypes inner
            | NamedApplicationBlock _ ->
                None
            | InoutArgument inner ->
                inferExpressionType localTypes inner
            | Apply(Name [ receiverName; memberName ], arguments) ->
                tryInferStaticConstructorCall
                    environment
                    freshCounter
                    (inferExpressionType localTypes)
                    (Name [ receiverName ])
                    memberName
                    arguments
                |> Option.orElseWith (fun () ->
                    environment.VisibleBindings
                    |> Map.tryFind memberName
                    |> Option.bind (fun bindingInfo ->
                        tryBuildReceiverMethodArguments bindingInfo (Name [ receiverName ]) arguments
                        |> Option.bind (fun receiverArguments ->
                            tryPrepareVisibleBindingCall
                                environment
                                freshCounter
                                bindingInfo
                                (inferExpressionType localTypes)
                                (tryResolveUniqueLocalImplicitByType environment localTypes)
                                receiverArguments
                            |> Option.map (fun preparedCall -> preparedCall.ResultType))))
            | Apply(Name [ calleeName ], arguments) ->
                environment.VisibleBindings
                |> Map.tryFind calleeName
                |> Option.bind (fun bindingInfo ->
                    tryPrepareVisibleBindingCall
                        environment
                        freshCounter
                        bindingInfo
                        (inferExpressionType localTypes)
                        (tryResolveUniqueLocalImplicitByType environment localTypes)
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
                            (tryResolveUniqueLocalImplicitByType environment localTypes)
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
            | PrefixedString(prefix, _) ->
                tryInferPrefixedStringMacroResultType environment freshCounter localTypes prefix

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
                            (Some(unwrapBindPayloadType valueType))
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
            | DoDefer _ :: rest ->
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
                              IsExtension = false
                              Path =
                                  [ { Name = fieldName
                                      IsImplicit = false } ]
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
                | SyntaxQuote inner ->
                    SyntaxQuote(loop inner)
                | SyntaxSplice inner ->
                    SyntaxSplice(loop inner)
                | TopLevelSyntaxSplice inner ->
                    TopLevelSyntaxSplice(loop inner)
                | CodeQuote inner ->
                    CodeQuote(loop inner)
                | CodeSplice inner ->
                    CodeSplice(loop inner)
                | Handle(isDeep, label, body, returnClause, operationClauses) ->
                    let loopClause (clause: SurfaceEffectHandlerClause) =
                        { clause with
                            Body = loop clause.Body }

                    Handle(
                        isDeep,
                        loop label,
                        loop body,
                        loopClause returnClause,
                        operationClauses |> List.map loopClause
                    )
                | LocalLet(binding, value, body) ->
                    LocalLet(binding, loop value, loop body)
                | LocalSignature(declaration, body) ->
                    LocalSignature(declaration, loop body)
                | LocalTypeAlias(declaration, body) ->
                    LocalTypeAlias(declaration, loop body)
                | LocalScopedEffect(declaration, body) ->
                    LocalScopedEffect(declaration, loop body)
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
                | MemberAccess(receiver, segments, arguments) ->
                    MemberAccess(receiver |> loop, segments, arguments |> List.map loop)
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
                        | DoDefer value -> DoDefer(loop value)
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
                | ExplicitImplicitArgument inner ->
                    ExplicitImplicitArgument(loop inner)
                | NamedApplicationBlock fields ->
                    NamedApplicationBlock(fields |> List.map (fun field -> { field with Value = loop field.Value }))
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
                            | StringInterpolation(inner, format) -> StringInterpolation(loop inner, format))
                    )
                | Literal _
                | NumericLiteral _
                | KindQualifiedName _ ->
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
                    | ProjectionAccessors _ ->
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
                    tryPrepareVisibleBindingCall
                        environment
                        freshCounter
                        bindingInfo
                        (inferExpressionType localTypes)
                        (tryResolveUniqueLocalImplicitByType environment localTypes)
                        arguments
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
                                                        KCoreExecute(
                                                            KCoreAppSpine(
                                                                KCoreName [ "readRef" ],
                                                                [ explicitKCoreArgument (KCoreName [ rootName ]) ]
                                                            )
                                                        )
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

                                                    KCoreAppSpine(
                                                        KCoreName [ residualLayout.ConstructorName ],
                                                        residualLayout.Fields
                                                        |> List.map (fun field ->
                                                            explicitKCoreArgument (KCoreName [ fieldBindingNames[field.Name] ]))
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
                                                        KCoreExecute(
                                                            KCoreAppSpine(
                                                                KCoreName [ "writeRef" ],
                                                                [ explicitKCoreArgument (KCoreName [ rootName ])
                                                                  explicitKCoreArgument rebuiltValue ]
                                                            )
                                                        ),
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
                    let valueType = inferExpressionType localTypes expression |> Option.map unwrapBindPayloadType

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
                    KCoreExecute(
                        KCoreAppSpine(
                            KCoreName [ "newRef" ],
                            [ explicitKCoreArgument (lowerExpression localTypes expression) ]
                        )
                    )

                let nextLocals =
                    match inferExpressionType localTypes expression with
                    | Some valueType -> Map.add bindingName (refType valueType) localTypes
                    | None -> localTypes

                KCoreLet(bindingName, loweredValue, lowerDoStatements scopeLabel nextLocals rest)
            | DoAssign(bindingName, expression) :: rest ->
                KCoreSequence(
                    KCoreExecute(
                        KCoreAppSpine(
                            KCoreName [ "writeRef" ],
                            [ explicitKCoreArgument (KCoreName [ bindingName ])
                              explicitKCoreArgument (lowerExpression localTypes expression) ]
                        )
                    ),
                    lowerDoStatements scopeLabel localTypes rest
                )
            | DoDefer expression :: rest ->
                KCoreScheduleExit(
                    scopeLabel,
                    KCoreDeferred(lowerExpression localTypes expression),
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
                KCoreExecute(
                    KCoreAppSpine(
                        KCoreName [ "return" ],
                        [ explicitKCoreArgument (lowerExpression localTypes expression) ]
                    )
                )

        and lowerPattern localTypes expectedType pattern =
            match pattern with
            | WildcardPattern ->
                KCoreWildcardPattern
            | NamePattern name ->
                KCoreNamePattern name
            | AsPattern(_, inner) ->
                lowerPattern localTypes expectedType inner
            | TypedPattern(inner, typeTokens) ->
                let narrowedExpected =
                    TypeSignatures.parseType typeTokens
                    |> Option.map (qualifyVisibleTypeNames environment)
                    |> Option.orElse expectedType

                lowerPattern localTypes narrowedExpected inner
            | LiteralPattern literal ->
                KCoreLiteralPattern literal
            | ConstructorPattern(name, arguments) ->
                let argumentTypes =
                    match expectedType, tryResolveVisibleConstructorInfo environment name with
                    | Some expectedResultType, Some constructorInfo ->
                        let instantiated = TypeSignatures.instantiate "t" freshCounter.Value constructorInfo.Scheme
                        freshCounter.Value <- freshCounter.Value + instantiated.Forall.Length

                        let parameterTypes, constructorResultType = TypeSignatures.schemeParts instantiated

                        if List.length parameterTypes = List.length arguments then
                            match
                                TypeSignatures.tryUnifyMany
                                    [
                                        qualifyVisibleTypeNames environment constructorResultType,
                                        qualifyVisibleTypeNames environment expectedResultType
                                    ]
                            with
                            | Some substitution ->
                                parameterTypes
                                |> List.map (TypeSignatures.applySubstitution substitution >> Some)
                            | None ->
                                List.replicate arguments.Length None
                        else
                            List.replicate arguments.Length None
                    | _ ->
                        List.replicate arguments.Length None

                KCoreConstructorPattern(
                    name,
                    List.zip arguments argumentTypes
                    |> List.map (fun (argumentPattern, argumentType) -> lowerPattern localTypes argumentType argumentPattern)
                )
            | NamedConstructorPattern(name, fields) ->
                let positionalPatterns =
                    match tryResolveVisibleConstructorInfo environment name with
                    | Some constructorInfo ->
                        let fieldMap =
                            fields
                            |> List.map (fun field -> field.Name, field.Pattern)
                            |> Map.ofList

                        constructorInfo.ParameterLayouts
                        |> Option.map (fun layouts ->
                            layouts
                            |> List.filter (fun parameter -> not parameter.IsImplicit)
                            |> List.map (fun parameter ->
                                fieldMap |> Map.tryFind parameter.Name |> Option.defaultValue WildcardPattern))
                        |> Option.defaultValue (fields |> List.map (fun field -> field.Pattern))
                    | None ->
                        fields |> List.map (fun field -> field.Pattern)

                lowerPattern localTypes expectedType (ConstructorPattern(name, positionalPatterns))
            | TuplePattern elements ->
                let tupleFields =
                    elements
                    |> List.mapi (fun index element ->
                        { Name = $"_{index + 1}"
                          IsImplicit = false
                          Pattern = element })

                lowerPattern localTypes expectedType (AnonymousRecordPattern(tupleFields, None))
            | VariantPattern(BoundVariantPattern(name, _)) ->
                KCoreNamePattern name
            | VariantPattern(RestVariantPattern name) ->
                KCoreNamePattern name
            | VariantPattern(WildcardVariantPattern _) ->
                KCoreWildcardPattern
            | OrPattern alternatives ->
                KCoreOrPattern(alternatives |> List.map (lowerPattern localTypes expectedType))
            | AnonymousRecordPattern(fields, _) ->
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

        and lowerStaticObject staticObject =
            let kind =
                match staticObject.ObjectKind with
                | StaticTypeObject -> KCoreTypeObject
                | StaticTraitObject -> KCoreTraitObject
                | StaticEffectLabelObject -> KCoreEffectLabelObject
                | StaticModuleObject -> KCoreModuleObject

            KCoreStaticObject
                { ObjectKind = kind
                  Name = staticObject.NameSegments
                  Type = staticObjectTypeText staticObject |> Option.bind tryParseTypeText
                  TypeText = staticObjectTypeText staticObject }

        and tryLowerRecordProjectionPath
            localTypes
            receiverValue
            receiverType
            path
            =
            match tryCanonicalRecordType environment.VisibleTypeAliases receiverType, path with
            | Some(TypeRecord fields), fieldName :: rest ->
                let layout =
                    ensureSyntheticRecordLayout environment.CurrentModuleName "<binding>" recordLayouts (TypeRecord fields)

                let boundFields =
                    layout.Fields
                    |> List.map (fun field -> field.Name, freshSyntheticName $"__kappa_record_{field.Name}_")

                match
                    fields |> List.tryFind (fun field -> String.Equals(field.Name, fieldName, StringComparison.Ordinal)),
                    boundFields |> List.tryFind (fun (name, _) -> String.Equals(name, fieldName, StringComparison.Ordinal))
                with
                | Some field, Some(_, bindingName) ->
                    let fieldValue = KCoreName [ bindingName ]

                    let body =
                        match rest with
                        | [] ->
                            fieldValue
                        | _ ->
                            tryLowerRecordProjectionPath
                                (Map.add bindingName field.Type localTypes)
                                fieldValue
                                field.Type
                                rest
                            |> Option.defaultValue fieldValue

                    Some(
                        KCoreMatch(
                            receiverValue,
                            [
                                { Pattern =
                                    KCoreConstructorPattern(
                                        [ layout.ConstructorName ],
                                        boundFields
                                        |> List.map (fun (_, localName) -> KCoreNamePattern localName)
                                    )
                                  Guard = None
                                  Body = body }
                            ]
                        )
                    )
                | _ ->
                    None
            | _ ->
                None

        and lowerExpression localTypes expression =
            lowerExpressionWithExpectedType localTypes None expression

        and lowerExpressionWithExpectedType localTypes expectedType expression =
            let lowerArguments arguments =
                arguments
                |> List.map (function
                    | InoutArgument inner -> inoutKCoreArgument (lowerExpressionWithExpectedType localTypes None inner)
                    | argument -> explicitKCoreArgument (lowerExpressionWithExpectedType localTypes None argument))

            let lowerArgumentExpressions arguments =
                arguments
                |> List.map (function
                    | InoutArgument inner -> lowerExpressionWithExpectedType localTypes None inner
                    | argument -> lowerExpressionWithExpectedType localTypes None argument)

            let lowerPreparedArguments preparedArguments =
                preparedArguments
                |> List.map (function
                    | ExplicitArgument argument -> explicitKCoreArgument (lowerExpressionWithExpectedType localTypes None argument)
                    | ImplicitArgument implicitArgument -> implicitKCoreArgument implicitArgument)

            let lowerPreparedArgumentExpressions preparedArguments =
                preparedArguments
                |> List.map (function
                    | ExplicitArgument argument -> lowerExpressionWithExpectedType localTypes None argument
                    | ImplicitArgument implicitArgument -> implicitArgument)

            let lowerInterpolatedMacroTypeArgument inner =
                match inferExpressionType localTypes inner |> Option.map (normalizeTypeAliases environment.VisibleTypeAliases) with
                | Some(TypeName(nameSegments, [])) ->
                    KCoreStaticObject
                        { ObjectKind = KCoreTypeObject
                          Name = nameSegments
                          Type = Some(TypeName([ "Type" ], []))
                          TypeText = Some "Type" }
                | _ ->
                    KCoreLiteral LiteralValue.Unit

            let interpolatedFragmentConstructor name arguments =
                KCoreAppSpine(KCoreName [ name ], arguments |> List.map explicitKCoreArgument)

            let fragmentListExpression fragments =
                let cons head tail =
                    KCoreAppSpine(
                        KCoreName [ "Cons" ],
                        [ explicitKCoreArgument head
                          explicitKCoreArgument tail ]
                    )

                fragments
                |> List.rev
                |> List.fold (fun tail head -> cons head tail) (KCoreName [ "Nil" ])

            let lowerPrefixedStringFragments parts =
                let flushText pending acc =
                    match pending with
                    | Some text when not (String.IsNullOrEmpty(text)) ->
                        interpolatedFragmentConstructor "Lit" [ KCoreLiteral(LiteralValue.String text) ] :: acc
                    | _ ->
                        acc

                let interpolationFragment inner format =
                    let typeArgument = lowerInterpolatedMacroTypeArgument inner
                    let quoted = KCoreSyntaxQuote(lowerExpressionWithExpectedType localTypes None inner)

                    match format with
                    | None ->
                        interpolatedFragmentConstructor "Interp" [ typeArgument; quoted ]
                    | Some formatText ->
                        interpolatedFragmentConstructor
                            "InterpFmt"
                            [ typeArgument
                              quoted
                              KCoreLiteral(LiteralValue.String formatText) ]

                let rec loop pendingText acc remainingParts =
                    match remainingParts with
                    | [] ->
                        let completed = flushText pendingText acc |> List.rev

                        if List.isEmpty completed then
                            [ interpolatedFragmentConstructor "Lit" [ KCoreLiteral(LiteralValue.String "") ] ]
                        else
                            completed
                    | StringText text :: rest ->
                        let mergedText =
                            pendingText
                            |> Option.map (fun pending -> pending + text)
                            |> Option.defaultValue text

                        loop (Some mergedText) acc rest
                    | StringInterpolation(inner, format) :: rest ->
                        let nextAcc =
                            flushText pendingText acc
                            |> fun fragments -> interpolationFragment inner format :: fragments

                        loop None nextAcc rest

                loop None [] parts

            let lowerPrefixedString prefix parts =
                let fragments = lowerPrefixedStringFragments parts |> fragmentListExpression

                KCoreTopLevelSyntaxSplice(
                    KCoreTraitCall(
                        "InterpolatedMacro",
                        "buildInterpolated",
                        KCoreName [ prefix ],
                        [ fragments ]
                    )
                )

            let rec substituteKCoreNames substitutions current =
                match current with
                | KCoreName [ name ] ->
                    substitutions |> Map.tryFind name |> Option.defaultValue current
                | KCoreName _ ->
                    current
                | KCoreSyntaxQuote inner ->
                    KCoreSyntaxQuote(substituteKCoreNames substitutions inner)
                | KCoreSyntaxSplice inner ->
                    KCoreSyntaxSplice(substituteKCoreNames substitutions inner)
                | KCoreTopLevelSyntaxSplice inner ->
                    KCoreTopLevelSyntaxSplice(substituteKCoreNames substitutions inner)
                | KCoreCodeQuote inner ->
                    KCoreCodeQuote(substituteKCoreNames substitutions inner)
                | KCoreCodeSplice inner ->
                    KCoreCodeSplice(substituteKCoreNames substitutions inner)
                | KCoreLiteral _
                | KCoreDictionaryValue _
                | KCoreStaticObject _ ->
                    current
                | KCoreLambda(parameters, body) ->
                    let nextSubstitutions =
                        parameters
                        |> List.fold (fun state parameter -> Map.remove parameter.Name state) substitutions

                    KCoreLambda(parameters, substituteKCoreNames nextSubstitutions body)
                | KCoreIfThenElse(condition, whenTrue, whenFalse) ->
                    KCoreIfThenElse(
                        substituteKCoreNames substitutions condition,
                        substituteKCoreNames substitutions whenTrue,
                        substituteKCoreNames substitutions whenFalse
                    )
                | KCoreMatch(scrutinee, cases) ->
                    KCoreMatch(
                        substituteKCoreNames substitutions scrutinee,
                        cases
                        |> List.map (fun caseClause ->
                            { Pattern = caseClause.Pattern
                              Guard = caseClause.Guard |> Option.map (substituteKCoreNames substitutions)
                              Body = substituteKCoreNames substitutions caseClause.Body })
                    )
                | KCoreExecute inner ->
                    KCoreExecute(substituteKCoreNames substitutions inner)
                | KCoreLet(bindingName, value, body) ->
                    let nextSubstitutions = Map.remove bindingName substitutions
                    KCoreLet(
                        bindingName,
                        substituteKCoreNames substitutions value,
                        substituteKCoreNames nextSubstitutions body
                    )
                | KCoreDoScope(scopeLabel, body) ->
                    KCoreDoScope(scopeLabel, substituteKCoreNames substitutions body)
                | KCoreScheduleExit(scopeLabel, action, body) ->
                    let loweredAction =
                        match action with
                        | KCoreDeferred deferred -> KCoreDeferred(substituteKCoreNames substitutions deferred)
                        | KCoreRelease(typeText, release, resource) ->
                            KCoreRelease(
                                typeText,
                                substituteKCoreNames substitutions release,
                                substituteKCoreNames substitutions resource
                            )

                    KCoreScheduleExit(scopeLabel, loweredAction, substituteKCoreNames substitutions body)
                | KCoreSequence(first, second) ->
                    KCoreSequence(substituteKCoreNames substitutions first, substituteKCoreNames substitutions second)
                | KCoreWhile(condition, body) ->
                    KCoreWhile(substituteKCoreNames substitutions condition, substituteKCoreNames substitutions body)
                | KCoreAppSpine(callee, arguments) ->
                    KCoreAppSpine(
                        substituteKCoreNames substitutions callee,
                        arguments
                        |> List.map (fun argument ->
                            { argument with
                                Expression = substituteKCoreNames substitutions argument.Expression })
                    )
                | KCoreTraitCall(traitName, memberName, dictionary, arguments) ->
                    KCoreTraitCall(
                        traitName,
                        memberName,
                        substituteKCoreNames substitutions dictionary,
                        arguments |> List.map (substituteKCoreNames substitutions)
                    )
                | KCoreUnary(operatorName, operand) ->
                    KCoreUnary(operatorName, substituteKCoreNames substitutions operand)
                | KCoreBinary(left, operatorName, right) ->
                    KCoreBinary(
                        substituteKCoreNames substitutions left,
                        operatorName,
                        substituteKCoreNames substitutions right
                    )
                | KCorePrefixedString(prefix, parts) ->
                    KCorePrefixedString(
                        prefix,
                        parts
                        |> List.map (function
                            | KCoreStringText text -> KCoreStringText text
                            | KCoreStringInterpolation inner -> KCoreStringInterpolation(substituteKCoreNames substitutions inner))
                    )

            let lowerPreparedBindingCallArguments
                (bindingInfo: BindingSchemeInfo)
                (preparedCall: PreparedBindingCall)
                =
                if List.isEmpty preparedCall.Parameters then
                    lowerPreparedArguments preparedCall.Arguments
                else
                    let rec loop substitutions parameters =
                        match parameters with
                        | [] ->
                            []
                        | parameter :: rest ->
                            let runtimeArgument, nextSubstitutions =
                                match parameter.Layout, parameter.AssignedArgument with
                                | _, Some(ExplicitArgument argument) ->
                                    let loweredArgument = lowerExpressionWithExpectedType localTypes (Some parameter.ParameterType) argument
                                    let substitutions =
                                        match parameter.Layout with
                                        | Some layout -> Map.add layout.Name loweredArgument substitutions
                                        | None -> substitutions

                                    Some(explicitKCoreArgument loweredArgument), substitutions
                                | Some layout, Some(ImplicitArgument implicitArgument) ->
                                    Some(implicitKCoreArgument implicitArgument), Map.add layout.Name implicitArgument substitutions
                                | Some layout, None when bindingInfo.DefaultArguments.ContainsKey(layout.Name) ->
                                    let loweredDefault =
                                        lowerExpressionWithExpectedType localTypes (Some parameter.ParameterType) bindingInfo.DefaultArguments[layout.Name]
                                        |> substituteKCoreNames substitutions

                                    Some(explicitKCoreArgument loweredDefault), Map.add layout.Name loweredDefault substitutions
                                | _ ->
                                    None, substitutions

                            match runtimeArgument with
                            | Some runtimeArgument -> runtimeArgument :: loop nextSubstitutions rest
                            | None -> loop nextSubstitutions rest

                    loop Map.empty preparedCall.Parameters

            match expression with
            | Literal literal ->
                KCoreLiteral literal
            | NumericLiteral literal ->
                match SurfaceNumericLiteral.suffix literal with
                | Some suffixName ->
                    lowerExpressionWithExpectedType
                        localTypes
                        expectedType
                        (Apply(Name [ suffixName ], [ NumericLiteral(SurfaceNumericLiteral.withoutSuffix literal) ]))
                | None ->
                    let contextualType =
                        expectedType
                        |> Option.bind (fun expectedResultType ->
                            tryResolveNumericLiteralContext environment expectedResultType literal)

                    match contextualType with
                    | Some(_, None) ->
                        tryLowerNumericLiteralForRuntime literal
                        |> Option.defaultValue (KCoreLiteral LiteralValue.Unit)
                    | Some(targetType, Some instanceInfo) ->
                        let literalConstraint =
                            { TraitName = numericLiteralConstraintName literal
                              Arguments = [ targetType ] }

                        let semanticArgument =
                            tryLowerNumericLiteralForRuntime literal
                            |> Option.defaultValue (KCoreLiteral LiteralValue.Unit)

                        KCoreTraitCall(
                            literalConstraint.TraitName,
                            (if literalConstraint.TraitName = "FromInteger" then
                                 "fromInteger"
                             else
                                 "fromFloat"),
                            KCoreDictionaryValue(instanceInfo.ModuleName, instanceInfo.TraitName, instanceInfo.InstanceKey),
                            [ semanticArgument ]
                        )
                    | None ->
                        tryLowerNumericLiteralForRuntime literal
                        |> Option.defaultValue (KCoreLiteral LiteralValue.Unit)
            | SyntaxQuote inner ->
                KCoreSyntaxQuote(lowerExpressionWithExpectedType localTypes None inner)
            | SyntaxSplice inner ->
                KCoreSyntaxSplice(lowerExpressionWithExpectedType localTypes None inner)
            | TopLevelSyntaxSplice inner ->
                KCoreTopLevelSyntaxSplice(lowerExpressionWithExpectedType localTypes None inner)
            | CodeQuote inner ->
                KCoreCodeQuote(lowerExpressionWithExpectedType localTypes None inner)
            | CodeSplice inner ->
                KCoreCodeSplice(lowerExpressionWithExpectedType localTypes None inner)
            | Handle(_, _, _, returnClause, _) ->
                lowerExpressionWithExpectedType localTypes expectedType returnClause.Body
            | KindQualifiedName _ ->
                tryResolveScopedStaticObject environment expression
                |> Option.map lowerStaticObject
                |> Option.defaultValue (KCoreLiteral LiteralValue.Unit)
            | Name [ bindingName ]
                when not (Map.containsKey bindingName localTypes)
                     && environment.VisibleBindings.ContainsKey(bindingName) ->
                let bindingInfo = environment.VisibleBindings[bindingName]
                match
                    tryPrepareVisibleBindingCall
                        environment
                        freshCounter
                        bindingInfo
                        (inferExpressionType localTypes)
                        (tryResolveUniqueLocalImplicitByType environment localTypes)
                        []
                with
                | Some preparedCall when not (List.isEmpty preparedCall.Arguments) ->
                    KCoreAppSpine(KCoreName [ bindingName ], lowerPreparedBindingCallArguments bindingInfo preparedCall)
                | _ ->
                    KCoreName [ bindingName ]
            | Name [ bindingName ]
                when not (Map.containsKey bindingName localTypes) ->
                match tryResolveScopedStaticObject environment expression with
                | Some staticObject ->
                    lowerStaticObject staticObject
                | None ->
                    KCoreName [ bindingName ]
            | Name (receiverName :: path) when not (List.isEmpty path) ->
                let loweredReceiver = lowerExpression localTypes (Name [ receiverName ])

                let recordProjection =
                    inferExpressionType localTypes (Name [ receiverName ])
                    |> Option.bind (fun receiverType ->
                        tryLowerRecordProjectionPath localTypes loweredReceiver receiverType path)

                match path with
                | [ memberName ] ->
                    match tryResolveStaticConstructor environment (Name [ receiverName ]) memberName with
                    | Some _ ->
                        KCoreName [ memberName ]
                    | None ->
                        match tryReceiverProjection environment localTypes receiverName memberName with
                        | Some _ ->
                            KCoreAppSpine(KCoreName [ memberName ], [ explicitKCoreArgument loweredReceiver ])
                        | None ->
                            recordProjection
                            |> Option.defaultValue (KCoreName [ receiverName; memberName ])
                | _ ->
                    recordProjection
                    |> Option.defaultValue (KCoreName(receiverName :: path))
            | Name segments ->
                KCoreName segments
            | LocalSignature(_, body) ->
                lowerExpressionWithExpectedType localTypes expectedType body
            | LocalTypeAlias(_, body) ->
                lowerExpressionWithExpectedType localTypes expectedType body
            | LocalLet(binding, value, body) ->
                let bindingNames = collectPatternNames binding.Pattern

                let loweredValue = lowerExpressionWithExpectedType localTypes None value
                let bodyForLowering =
                    match bindingNames, tryResolveScopedStaticObject environment value with
                    | [ aliasName ], Some staticObject ->
                        rewriteStaticObjectAliasUse aliasName staticObject body
                    | [ aliasName ], _ ->
                        match value with
                        | Name [ targetName ]
                            when (environment.VisibleBindings
                                  |> Map.tryFind targetName
                                  |> Option.orElseWith (fun () -> environment.VisibleConstructors |> Map.tryFind targetName))
                                 |> Option.bind (fun bindingInfo -> bindingInfo.ParameterLayouts)
                                 |> Option.isSome ->
                            rewriteDirectAliasUse aliasName targetName body
                        | Lambda(parameters, _) ->
                            rewriteNamedApplicationAliasUse aliasName parameters body
                        | _ ->
                            body
                    | _ ->
                        body

                let valueType = inferExpressionType localTypes value

                lowerPatternBinding
                    localTypes
                    valueType
                    binding
                    loweredValue
                    (fun loweredLocals -> lowerExpression loweredLocals bodyForLowering)
            | LocalScopedEffect(declaration, body) ->
                withScopedEffectDeclaration declaration (fun () -> lowerExpressionWithExpectedType localTypes expectedType body)
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
                        lowerKCoreParameter parameter (tryParseParameterType parameter)),
                    lowerExpressionWithExpectedType lambdaLocals expectedType lambdaBody
                )
            | IfThenElse(condition, whenTrue, whenFalse) ->
                KCoreIfThenElse(
                    lowerExpressionWithExpectedType localTypes None condition,
                    lowerExpressionWithExpectedType localTypes expectedType whenTrue,
                    lowerExpressionWithExpectedType localTypes expectedType whenFalse
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

                    KCoreAppSpine(
                        KCoreName [ layout.ConstructorName ],
                        layout.Fields
                        |> List.map (fun field ->
                            fieldValues
                            |> Map.tryFind field.Name
                            |> Option.map (lowerExpression localTypes >> explicitKCoreArgument)
                            |> Option.defaultValue (explicitKCoreArgument (KCoreLiteral LiteralValue.Unit)))
                    )
                else
                    KCoreLiteral LiteralValue.Unit
            | Seal(value, _) ->
                lowerExpressionWithExpectedType localTypes expectedType value
            | RecordUpdate(receiver, fields) ->
                let loweredReceiver = lowerExpression localTypes receiver

                match inferExpressionType localTypes receiver |> Option.bind (tryCanonicalRecordType environment.VisibleTypeAliases) with
                | Some recordType ->
                    let layout =
                        ensureSyntheticRecordLayout environment.CurrentModuleName "<binding>" recordLayouts recordType

                    let boundFields =
                        layout.Fields
                        |> List.map (fun field -> field.Name, freshSyntheticName $"__kappa_record_{field.Name}_")

                    let receiverSegments =
                        match receiver with
                        | Name segments -> Some segments
                        | _ -> None

                    let ordinaryUpdateValue topLevelName (group: SurfaceRecordUpdateField list) =
                        match group |> List.tryFind (fun field -> List.length field.Path = 1) with
                        | Some directField ->
                            directField.Value
                        | None ->
                            match receiverSegments with
                            | Some segments ->
                                let nestedFields =
                                    group
                                    |> List.choose (fun field ->
                                        match field.Path with
                                        | _ :: child :: rest ->
                                            Some
                                                { field with
                                                    Name = child.Name
                                                    IsImplicit = child.IsImplicit
                                                    IsExtension = false
                                                    Path = child :: rest }
                                        | _ ->
                                            None)

                                RecordUpdate(Name(segments @ [ topLevelName ]), nestedFields)
                            | None ->
                                group
                                |> List.tryHead
                                |> Option.map (fun field -> field.Value)
                                |> Option.defaultValue (Literal LiteralValue.Unit)

                    let updatedFields =
                        fields
                        |> List.filter (fun field -> not field.IsExtension)
                        |> List.groupBy (fun field -> field.Name)
                        |> List.map (fun (name, group) -> name, ordinaryUpdateValue name group)
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
                              Body =
                                KCoreAppSpine(
                                    KCoreName [ layout.ConstructorName ],
                                    rebuiltFields |> List.map explicitKCoreArgument
                                ) }
                        ]
                    )
                | None ->
                    loweredReceiver
            | MemberAccess(receiver, segments, arguments) ->
                match segments with
                | [] ->
                    KCoreAppSpine(lowerExpression localTypes receiver, lowerArguments arguments)
                | [ memberName ] ->
                    match tryResolveStaticConstructor environment receiver memberName with
                    | Some _ ->
                        KCoreAppSpine(KCoreName [ memberName ], lowerArguments arguments)
                    | None ->
                        match environment.VisibleBindings |> Map.tryFind memberName with
                        | Some bindingInfo ->
                            match tryBuildReceiverMethodArguments bindingInfo receiver arguments with
                            | Some receiverArguments ->
                                match
                                    tryPrepareVisibleBindingCall
                                        environment
                                        freshCounter
                                        bindingInfo
                                        (inferExpressionType localTypes)
                                        (tryResolveUniqueLocalImplicitByType environment localTypes)
                                        receiverArguments
                                with
                                | Some preparedCall ->
                                    KCoreAppSpine(KCoreName [ memberName ], lowerPreparedBindingCallArguments bindingInfo preparedCall)
                                | None ->
                                    KCoreAppSpine(KCoreName [ memberName ], lowerArguments receiverArguments)
                            | None ->
                                if List.isEmpty arguments then
                                    KCoreName [ memberName ]
                                else
                                    KCoreAppSpine(KCoreName [ memberName ], lowerArguments arguments)
                        | None ->
                            let loweredReceiver = lowerExpression localTypes receiver

                            match inferExpressionType localTypes receiver with
                            | Some receiverType ->
                                tryLowerRecordProjectionPath localTypes loweredReceiver receiverType [ memberName ]
                                |> Option.map (fun projected ->
                                    if List.isEmpty arguments then
                                        projected
                                    else
                                        KCoreAppSpine(projected, lowerArguments arguments))
                                |> Option.defaultValue (KCoreName [ memberName ])
                            | None ->
                                KCoreName [ memberName ]
                | memberName :: rest ->
                    lowerExpression localTypes (MemberAccess(MemberAccess(receiver, [ memberName ], []), rest, arguments))
            | SafeNavigation(receiver, navigation) ->
                let loweredReceiver = lowerExpression localTypes receiver
                let binderName = freshSyntheticName "__kappa_safe_"

                let loweredMember =
                    let callee = KCoreName(binderName :: navigation.Segments)

                    match navigation.Arguments with
                    | [] ->
                        callee
                    | arguments ->
                        KCoreAppSpine(callee, arguments |> List.map (lowerExpression localTypes >> explicitKCoreArgument))

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
                        KCoreAppSpine(KCoreName [ "Some" ], [ explicitKCoreArgument loweredMember ])
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
                KCoreExecute(lowerExpressionWithExpectedType localTypes None inner)
            | ExplicitImplicitArgument inner ->
                lowerExpressionWithExpectedType localTypes None inner
            | NamedApplicationBlock _ ->
                KCoreLiteral LiteralValue.Unit
            | InoutArgument inner ->
                lowerExpressionWithExpectedType localTypes None inner
            | Apply(Name [ receiverName; memberName ], arguments) ->
                match tryResolveStaticConstructor environment (Name [ receiverName ]) memberName with
                | Some _ ->
                    KCoreAppSpine(KCoreName [ memberName ], lowerArguments arguments)
                | None ->
                    match tryResolveTraitMemberProjection environment localTypes [ receiverName; memberName ] with
                    | Some(traitInfo, resolvedMemberName, _, resolvedReceiverName) ->
                            KCoreTraitCall(
                                traitInfo.Name,
                                resolvedMemberName,
                                lowerExpressionWithExpectedType localTypes None (Name [ resolvedReceiverName ]),
                                lowerArgumentExpressions arguments
                            )
                    | None ->
                        match environment.VisibleBindings |> Map.tryFind memberName with
                        | Some bindingInfo ->
                            match tryBuildReceiverMethodArguments bindingInfo (Name [ receiverName ]) arguments with
                            | Some receiverArguments ->
                                match
                                    tryPrepareVisibleBindingCall
                                        environment
                                        freshCounter
                                        bindingInfo
                                        (inferExpressionType localTypes)
                                        (tryResolveUniqueLocalImplicitByType environment localTypes)
                                        receiverArguments
                                with
                                | Some preparedCall ->
                                    KCoreAppSpine(KCoreName [ memberName ], lowerPreparedBindingCallArguments bindingInfo preparedCall)
                                | None ->
                                    KCoreAppSpine(KCoreName [ memberName ], lowerArguments receiverArguments)
                            | None ->
                                KCoreAppSpine(lowerExpression localTypes (Name [ receiverName; memberName ]), lowerArguments arguments)
                        | None ->
                            KCoreAppSpine(lowerExpression localTypes (Name [ receiverName; memberName ]), lowerArguments arguments)
            | Apply(Name nameSegments, arguments) ->
                match tryResolveTraitMemberProjection environment localTypes nameSegments with
                | Some(traitInfo, memberName, _, receiverName) ->
                    KCoreTraitCall(
                        traitInfo.Name,
                        memberName,
                        lowerExpressionWithExpectedType localTypes None (Name [ receiverName ]),
                        lowerArgumentExpressions arguments
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
                            lowerArgumentExpressions arguments
                        )
                    | [ calleeName ] when environment.VisibleBindings.ContainsKey calleeName ->
                        let bindingInfo = environment.VisibleBindings[calleeName]
                        match
                            tryPrepareVisibleBindingCall
                                environment
                                freshCounter
                                bindingInfo
                                (inferExpressionType localTypes)
                                (tryResolveUniqueLocalImplicitByType environment localTypes)
                                arguments
                        with
                        | Some preparedCall ->
                            let dictionaryArguments =
                                preparedCall.InstantiatedScheme.Constraints
                                |> List.choose (fun constraintInfo ->
                                    resolveConstraintInstance environment constraintInfo
                                    |> Option.map (fun instanceInfo ->
                                        explicitKCoreArgument (
                                            KCoreDictionaryValue(instanceInfo.ModuleName, instanceInfo.TraitName, instanceInfo.InstanceKey)
                                        )))

                            KCoreAppSpine(
                                KCoreName [ calleeName ],
                                dictionaryArguments @ lowerPreparedBindingCallArguments bindingInfo preparedCall
                            )
                        | None ->
                            KCoreAppSpine(KCoreName [ calleeName ], lowerArguments arguments)
                    | [ calleeName ] when environment.VisibleConstructors.ContainsKey calleeName ->
                        let constructorInfo = environment.VisibleConstructors[calleeName]
                        match
                            tryPrepareVisibleBindingCall
                                environment
                                freshCounter
                                constructorInfo
                                (inferExpressionType localTypes)
                                (tryResolveUniqueLocalImplicitByType environment localTypes)
                                arguments
                        with
                        | Some preparedCall ->
                            KCoreAppSpine(
                                KCoreName [ calleeName ],
                                lowerPreparedBindingCallArguments constructorInfo preparedCall
                            )
                        | None ->
                            KCoreAppSpine(KCoreName [ calleeName ], lowerArguments arguments)
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
                                lowerPreparedArgumentExpressions preparedCall.Arguments
                            )
                        | None ->
                            KCoreAppSpine(KCoreName [ calleeName ], lowerArguments arguments)
                    | _ ->
                        KCoreAppSpine(lowerExpression localTypes (Name nameSegments), lowerArguments arguments)
            | Apply(callee, arguments) ->
                        KCoreAppSpine(lowerExpressionWithExpectedType localTypes None callee, lowerArguments arguments)
            | Unary(operatorName, operand) ->
                let operandExpectedType =
                    if operatorName = "-" || operatorName = "negate" then expectedType else None

                KCoreUnary(operatorName, lowerExpressionWithExpectedType localTypes operandExpectedType operand)
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
                lowerPrefixedString prefix parts

        let localTypes =
            buildLocalTypes scheme parameters

        let bodyForLowering =
            match parameters, scheme, body with
            | [], Some scheme, Lambda(lambdaParameters, lambdaBody) ->
                let parameterTypes, _ = TypeSignatures.schemeParts scheme

                if List.length parameterTypes = List.length lambdaParameters then
                    let annotatedParameters =
                        List.zip lambdaParameters parameterTypes
                        |> List.map (fun (parameter, parameterType) ->
                            if parameter.TypeTokens.IsSome then
                                parameter
                            else
                                { parameter with
                                    TypeTokens = Some(typeTextTokens (TypeSignatures.toText parameterType)) })

                    Lambda(annotatedParameters, lambdaBody)
                else
                    body
            | _ ->
                body

        lowerExpressionWithExpectedType localTypes (lowerBindingReturnTypeExpr scheme parameters None) bodyForLowering

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
                lowerSyntheticKCoreParameter
                    dictionaryParameterName
                    None
                    true
                    (Some(dictionaryType constraintInfo.TraitName constraintInfo.Arguments)))

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
                  ReturnType = lowerBindingReturnTypeExpr scheme definition.Parameters definition.ReturnTypeTokens
                  ReturnTypeText = lowerBindingReturnType scheme definition.Parameters definition.ReturnTypeTokens
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
                lowerSyntheticKCoreParameter
                    dictionaryParameterName
                    None
                    false
                    (Some(dictionaryType traitInfo.Name dictionaryArgumentTypes))
                :: (List.zip valueParameterNames parameterTypes
                    |> List.map (fun (parameterName, parameterType) ->
                        lowerSyntheticKCoreParameter parameterName None false (Some parameterType)))

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
                (lowerSyntheticKCoreParameter
                    dictionaryParameterName
                    None
                    false
                    (Some(dictionaryType traitInfo.Name dictionaryArgumentTypes))
                 :: (List.zip valueParameterNames parameterTypes
                     |> List.map (fun (parameterName, parameterType) ->
                         lowerSyntheticKCoreParameter parameterName None false (Some parameterType))))
                (Some resultType)
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
            | KCoreSyntaxQuote inner ->
                KCoreSyntaxQuote(substituteSelfDictionary inner)
            | KCoreSyntaxSplice inner ->
                KCoreSyntaxSplice(substituteSelfDictionary inner)
            | KCoreTopLevelSyntaxSplice inner ->
                KCoreTopLevelSyntaxSplice(substituteSelfDictionary inner)
            | KCoreCodeQuote inner ->
                KCoreCodeQuote(substituteSelfDictionary inner)
            | KCoreCodeSplice inner ->
                KCoreCodeSplice(substituteSelfDictionary inner)
            | KCoreStaticObject _ ->
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
            | KCoreAppSpine(callee, arguments) ->
                KCoreAppSpine(
                    substituteSelfDictionary callee,
                    arguments
                    |> List.map (fun argument ->
                        { argument with
                            Expression = substituteSelfDictionary argument.Expression })
                )
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
            mergeVisibleTraits surfaceIndex moduleName
            |> Map.tryFind instanceInfo.TraitName

        let substitution =
            match traitInfo with
            | Some traitInfo when List.length traitInfo.TypeParameters = List.length instanceInfo.HeadTypes ->
                List.zip traitInfo.TypeParameters instanceInfo.HeadTypes |> Map.ofList
            | _ ->
                Map.empty

        let visibleBindings = mergeVisibleBindings surfaceIndex moduleName
        let visibleTraits = mergeVisibleTraits surfaceIndex moduleName
        let visibleTypeAliases = mergeVisibleTypeAliases surfaceIndex moduleName
        let visibleTypeFacets = mergeVisibleTypeFacets surfaceIndex moduleName
        let visibleProjections = mergeVisibleProjections surfaceIndex moduleName

        let environment =
            { CurrentModuleName = moduleName
              VisibleTypeAliases = visibleTypeAliases
              VisibleTypeFacets = visibleTypeFacets
              VisibleStaticObjects = Map.empty
              VisibleModules = mergeVisibleModules surfaceIndex moduleName
              VisibleRecordTypes = mergeVisibleRecordTypes surfaceIndex moduleName
              VisibleBindings = visibleBindings
              VisibleConstructors = mergeVisibleConstructors surfaceIndex moduleName
              VisibleProjections = visibleProjections
              VisibleTraits = visibleTraits
              VisibleInstances = visibleInstances surfaceIndex moduleName
              ConstrainedMembers = Map.empty }

        let defaultBodyEnvironment =
            { environment with
                ConstrainedMembers =
                    traitInfo
                    |> Option.map (fun traitInfo ->
                        traitInfo.Members
                        |> Map.toList
                        |> List.map (fun (memberName, _) -> memberName, (instanceInfo.TraitName, selfDictionaryName))
                        |> Map.ofList)
                    |> Option.defaultValue Map.empty }

        let memberBindings =
            traitInfo
            |> Option.map (fun traitInfo ->
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
                            (lowerBindingReturnTypeExpr (Some specializedScheme) definition.Parameters definition.ReturnTypeTokens)
                            (loweredBody |> Option.defaultValue (KCoreLiteral LiteralValue.Unit))
                            provenance)))
            |> Option.defaultValue []

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
                (Some(dictionaryType instanceInfo.TraitName instanceInfo.HeadTypes))
                (KCoreDictionaryValue(moduleName, instanceInfo.TraitName, instanceInfo.InstanceKey))
                provenance

        memberBindings @ [ dictionaryBinding ]

    let lowerKCoreModules (backendProfile: string) allowUnsafeConsume (frontendModules: KFrontIRModule list) =
        let surfaceIndex = buildSurfaceIndex frontendModules

        frontendModules
        |> List.map (fun frontendModule ->
            let moduleName = moduleNameText frontendModule.ModuleIdentity
            let moduleInfo = surfaceIndex[moduleName]
            let recordLayouts = ref Map.empty

            let baseEnvironment =
                { CurrentModuleName = moduleName
                  VisibleTypeAliases = mergeVisibleTypeAliases surfaceIndex moduleName
                  VisibleTypeFacets = mergeVisibleTypeFacets surfaceIndex moduleName
                  VisibleStaticObjects = Map.empty
                  VisibleModules = mergeVisibleModules surfaceIndex moduleName
                  VisibleRecordTypes = mergeVisibleRecordTypes surfaceIndex moduleName
                  VisibleBindings = mergeVisibleBindings surfaceIndex moduleName
                  VisibleConstructors = mergeVisibleConstructors surfaceIndex moduleName
                  VisibleProjections = mergeVisibleProjections surfaceIndex moduleName
                  VisibleTraits = mergeVisibleTraits surfaceIndex moduleName
                  VisibleInstances = visibleInstances surfaceIndex moduleName
                  ConstrainedMembers = Map.empty }

            let environment =
                { baseEnvironment with
                    VisibleStaticObjects = buildStaticObjectAliasesForModule baseEnvironment frontendModule.Declarations }

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
                            when Stdlib.intrinsicallySatisfiesExpectForCompilation
                                    backendProfile
                                    allowUnsafeConsume
                                    moduleNameSegments
                                    declaration ->
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
