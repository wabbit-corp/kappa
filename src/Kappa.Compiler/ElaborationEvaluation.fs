namespace Kappa.Compiler

open System
open System.Numerics

module internal ElaborationEvaluation =
    type private ExportInventory =
        { Terms: Set<string>
          Types: Set<string>
          Traits: Set<string>
          Constructors: Set<string> }

    type private SurfaceTermDefinition =
        { ModuleIdentity: ModuleIdentity
          ModuleName: string
          Name: string
          Parameters: Parameter list
          Body: SurfaceExpression option
          IsExpect: bool }

    type private SurfaceTypeDefinition =
        | SurfaceData of moduleIdentity: ModuleIdentity * declaration: DataDeclaration
        | SurfaceAlias of moduleIdentity: ModuleIdentity * declaration: TypeAlias
        | SurfaceEffect of moduleIdentity: ModuleIdentity * declaration: EffectDeclaration
        | SurfaceExpectedType of moduleIdentity: ModuleIdentity * name: string

    type private SurfaceTraitDefinition =
        | SurfaceTrait of moduleIdentity: ModuleIdentity * declaration: TraitDeclaration
        | SurfaceExpectedTrait of moduleIdentity: ModuleIdentity * name: string

    type private SurfaceConstructorDefinition =
        { ModuleIdentity: ModuleIdentity
          ModuleName: string
          ParentTypeName: string
          Constructor: DataConstructor }

    type private ModuleModel =
        { ModuleIdentity: ModuleIdentity
          ModuleName: string
          Terms: Map<string, SurfaceTermDefinition>
          Types: Map<string, SurfaceTypeDefinition>
          Traits: Map<string, SurfaceTraitDefinition>
          Constructors: Map<string, SurfaceConstructorDefinition>
          Instances: InstanceDeclaration list
          Imports: ImportSpec list }

    type private TypeSyntaxValue =
        | ParsedTypeExpr of TypeSignatures.TypeExpr
        | RawTypeTokens of Token list

    type private SyntaxValue =
        { Expression: SurfaceExpression
          CapturedNames: Set<string>
          ScopeModule: ModuleIdentity option }

    type private MetaField =
        { Name: string
          Value: MetaValue }

    and private MetaClosure =
        { ModuleIdentity: ModuleIdentity
          ModuleName: string
          Parameters: Parameter list
          Body: SurfaceExpression
          MetaBindings: Map<string, MetaValue> }

    and private MetaValue =
        | MetaUnit
        | MetaBool of bool
        | MetaString of string
        | MetaInt of int64
        | MetaNat of bigint
        | MetaList of MetaValue list
        | MetaRecord of MetaField list
        | MetaData of constructorName: string * fields: MetaField list
        | MetaSyntax of SyntaxValue
        | MetaTypeSyntax of TypeSyntaxValue
        | MetaTraitRef of moduleName: string * traitName: string
        | MetaConstructor of constructorName: string * parameters: DataConstructorParameter list * appliedArguments: MetaValue list
        | MetaClosure of MetaClosure
        | MetaBuiltin of name: string * appliedArguments: MetaValue list

    type private EvalResult =
        | EvalBlocked
        | EvalFailed of Diagnostic list
        | EvalSucceeded of MetaValue * Diagnostic list

    type private RewriteResult =
        { Expression: SurfaceExpression
          Diagnostics: Diagnostic list }

    type private RewriteContext =
        { Models: Map<ModuleIdentity, ModuleModel>
          Inventories: Map<ModuleIdentity, ExportInventory>
          CurrentModule: ModuleIdentity
          Source: SourceText
          ObjectBinders: Set<string>
          MetaBindings: Map<string, MetaValue> }

    let private emptyInventory : ExportInventory =
        { Terms = Set.empty
          Types = Set.empty
          Traits = Set.empty
          Constructors = Set.empty }

    let private mergeInventory (left: ExportInventory) (right: ExportInventory) : ExportInventory =
        { Terms = Set.union left.Terms right.Terms
          Types = Set.union left.Types right.Types
          Traits = Set.union left.Traits right.Traits
          Constructors = Set.union left.Constructors right.Constructors }

    let private exportInventoryForDocument (document: ParsedDocument) : ExportInventory =
        let privateByDefault =
            document.Syntax.ModuleAttributes
            |> List.exists (fun attributeName -> String.Equals(attributeName, ModuleAttribute.PrivateByDefault, StringComparison.Ordinal))

        let isExported visibility =
            match visibility with
            | Some Visibility.Public -> true
            | Some Visibility.Private -> false
            | None -> not privateByDefault

        document.Syntax.Declarations
        |> List.fold
            (fun (inventory: ExportInventory) declaration ->
                match declaration with
                | SignatureDeclaration declaration when isExported declaration.Visibility ->
                    { inventory with Terms = Set.add declaration.Name inventory.Terms }
                | LetDeclaration definition when isExported definition.Visibility ->
                    match definition.Name with
                    | Some name -> { inventory with Terms = Set.add name inventory.Terms }
                    | None -> inventory
                | ProjectionDeclarationNode declaration when isExported declaration.Visibility ->
                    { inventory with Terms = Set.add declaration.Name inventory.Terms }
                | DataDeclarationNode declaration when isExported declaration.Visibility ->
                    { inventory with
                        Types = Set.add declaration.Name inventory.Types
                        Constructors =
                            declaration.Constructors
                            |> List.fold (fun current constructor -> Set.add constructor.Name current) inventory.Constructors }
                | TypeAliasNode declaration when isExported declaration.Visibility ->
                    { inventory with Types = Set.add declaration.Name inventory.Types }
                | EffectDeclarationNode declaration when isExported declaration.Visibility ->
                    { inventory with Types = Set.add declaration.Name inventory.Types }
                | TraitDeclarationNode declaration when isExported declaration.Visibility ->
                    { inventory with Traits = Set.add declaration.Name inventory.Traits }
                | ExpectDeclarationNode (ExpectTermDeclaration declaration) ->
                    { inventory with Terms = Set.add declaration.Name inventory.Terms }
                | ExpectDeclarationNode (ExpectTypeDeclaration declaration) ->
                    { inventory with Types = Set.add declaration.Name inventory.Types }
                | ExpectDeclarationNode (ExpectTraitDeclaration declaration) ->
                    { inventory with Traits = Set.add declaration.Name inventory.Traits }
                | _ ->
                    inventory)
            emptyInventory

    let private buildInventories (documents: ParsedDocument list) : Map<ModuleIdentity, ExportInventory> =
        documents
        |> List.choose (fun document ->
            document.ModuleIdentity
            |> Option.map (fun moduleIdentity -> moduleIdentity, exportInventoryForDocument document))
        |> List.groupBy fst
        |> List.map (fun (moduleIdentity, entries) ->
            moduleIdentity,
            (entries |> List.map snd |> List.fold mergeInventory emptyInventory))
        |> Map.ofList

    let private typeTokensFromText (text: string) =
        let source = SourceText.From("__kappa_elab_signature__.kp", text)
        let lexResult = Lexer.tokenize source

        lexResult.Tokens
        |> List.filter (fun token ->
            match token.Kind with
            | EndOfFile -> false
            | _ -> true)

    let private leadingForallParameters (signature: BindingSignature option) (parameters: Parameter list) =
        let existingNames = parameters |> List.map (fun parameter -> parameter.Name) |> Set.ofList

        signature
        |> Option.bind (fun currentSignature -> TypeSignatures.parseScheme currentSignature.TypeTokens)
        |> Option.map (fun scheme ->
            scheme.Forall
            |> List.map (fun binder ->
                { Name = binder.Name
                  TypeTokens = Some(typeTokensFromText (TypeSignatures.toText binder.Sort))
                  Quantity = Some binder.Quantity
                  IsImplicit = true
                  IsInout = false
                  IsReceiver = false })
            |> List.filter (fun parameter -> not (Set.contains parameter.Name existingNames)))
        |> Option.defaultValue []

    let private tryParseTopLevelTermDefinition moduleIdentity moduleName (signatures: Map<string, BindingSignature>) declaration =
        match declaration with
        | LetDeclaration definition when definition.Name.IsSome ->
            let leadingParameters =
                signatures
                |> Map.tryFind definition.Name.Value
                |> fun signature -> leadingForallParameters signature definition.Parameters

            Some(
                definition.Name.Value,
                { ModuleIdentity = moduleIdentity
                  ModuleName = moduleName
                  Name = definition.Name.Value
                  Parameters = leadingParameters @ definition.Parameters
                  Body = definition.Body
                  IsExpect = false }
            )
        | ProjectionDeclarationNode declaration ->
            Some(
                declaration.Name,
                { ModuleIdentity = moduleIdentity
                  ModuleName = moduleName
                  Name = declaration.Name
                  Parameters = []
                  Body = None
                  IsExpect = false }
            )
        | ExpectDeclarationNode (ExpectTermDeclaration declaration) ->
            Some(
                declaration.Name,
                { ModuleIdentity = moduleIdentity
                  ModuleName = moduleName
                  Name = declaration.Name
                  Parameters = []
                  Body = None
                  IsExpect = true }
            )
        | _ ->
            None

    let private tryParseTopLevelTypeDefinition moduleIdentity declaration =
        match declaration with
        | DataDeclarationNode declaration -> Some(declaration.Name, SurfaceData(moduleIdentity, declaration))
        | TypeAliasNode declaration -> Some(declaration.Name, SurfaceAlias(moduleIdentity, declaration))
        | EffectDeclarationNode declaration -> Some(declaration.Name, SurfaceEffect(moduleIdentity, declaration))
        | ExpectDeclarationNode (ExpectTypeDeclaration declaration) -> Some(declaration.Name, SurfaceExpectedType(moduleIdentity, declaration.Name))
        | _ -> None

    let private tryParseTopLevelTraitDefinition moduleIdentity declaration =
        match declaration with
        | TraitDeclarationNode declaration -> Some(declaration.Name, SurfaceTrait(moduleIdentity, declaration))
        | ExpectDeclarationNode (ExpectTraitDeclaration declaration) -> Some(declaration.Name, SurfaceExpectedTrait(moduleIdentity, declaration.Name))
        | _ -> None

    let private buildModuleModels (documents: ParsedDocument list) : Map<ModuleIdentity, ModuleModel> =
        documents
        |> List.choose (fun document ->
            document.ModuleIdentity
            |> Option.map (fun moduleIdentity -> moduleIdentity, document))
        |> List.groupBy fst
        |> List.map (fun (moduleIdentity, entries) ->
            let moduleDocuments = entries |> List.map snd
            let moduleName = ModuleIdentity.text moduleIdentity

            let signatures =
                moduleDocuments
                |> List.collect (fun document ->
                    document.Syntax.Declarations
                    |> List.choose (function
                        | SignatureDeclaration declaration -> Some(declaration.Name, declaration)
                        | _ -> None))
                |> Map.ofList

            let terms =
                moduleDocuments
                |> List.collect (fun document ->
                    document.Syntax.Declarations
                    |> List.choose (tryParseTopLevelTermDefinition moduleIdentity moduleName signatures))
                |> Map.ofList

            let types =
                moduleDocuments
                |> List.collect (fun document -> document.Syntax.Declarations |> List.choose (tryParseTopLevelTypeDefinition moduleIdentity))
                |> Map.ofList

            let traits =
                moduleDocuments
                |> List.collect (fun document -> document.Syntax.Declarations |> List.choose (tryParseTopLevelTraitDefinition moduleIdentity))
                |> Map.ofList

            let constructors =
                moduleDocuments
                |> List.collect (fun document ->
                    document.Syntax.Declarations
                    |> List.collect (function
                        | DataDeclarationNode declaration ->
                            declaration.Constructors
                            |> List.map (fun constructor ->
                                constructor.Name,
                                { ModuleIdentity = moduleIdentity
                                  ModuleName = moduleName
                                  ParentTypeName = declaration.Name
                                  Constructor = constructor })
                        | _ -> []))
                |> Map.ofList

            let instances =
                moduleDocuments
                |> List.collect (fun document ->
                    document.Syntax.Declarations
                    |> List.choose (function
                        | InstanceDeclarationNode declaration -> Some declaration
                        | _ -> None))

            let imports = moduleDocuments |> List.collect CompilationFrontend.collectImportSpecs

            moduleIdentity,
            { ModuleIdentity = moduleIdentity
              ModuleName = moduleName
              Terms = terms
              Types = types
              Traits = traits
              Constructors = constructors
              Instances = instances
              Imports = imports })
        |> Map.ofList

    let private defaultLocation (source: SourceText) = source.GetLocation(TextSpan.FromBounds(0, 0))

    let private makeDiagnostic severity fact stage phase source =
        Diagnostics.create severity fact (Some(defaultLocation source)) [] (Some stage) (Some phase)

    let private makeMacroError message source =
        makeDiagnostic Error (DiagnosticFact.macroExpansion (FailElabMessage message)) "macro-expansion" (KFrontIRPhase.phaseName BODY_RESOLVE) source

    let private makeMacroWarning message source =
        makeDiagnostic Warning (DiagnosticFact.macroExpansion (WarnElabMessage message)) "macro-expansion" (KFrontIRPhase.phaseName BODY_RESOLVE) source

    let private makeMacroCodeError code message source =
        makeDiagnostic Error (DiagnosticFact.macroExpansion (FailElabWithCode(code, message))) "macro-expansion" (KFrontIRPhase.phaseName BODY_RESOLVE) source

    let private makeMacroCodeWarning code message source =
        makeDiagnostic Warning (DiagnosticFact.macroExpansion (WarnElabWithCode(code, message))) "macro-expansion" (KFrontIRPhase.phaseName BODY_RESOLVE) source

    let private importedItemLocalName (item: ImportItem) = item.Alias |> Option.defaultValue item.Name

    let private itemImportsTerm (item: ImportItem) =
        match item.Namespace with
        | Some ImportNamespace.Term
        | None -> true
        | _ -> false

    let private itemImportsType (item: ImportItem) =
        match item.Namespace with
        | Some ImportNamespace.Type
        | None -> true
        | _ -> false

    let private itemImportsTrait (item: ImportItem) =
        match item.Namespace with
        | Some ImportNamespace.Trait
        | None -> true
        | _ -> false

    let private itemImportsConstructor (item: ImportItem) =
        match item.Namespace with
        | Some ImportNamespace.Constructor
        | _ -> false

    let private itemImportsConstructorsOfType typeName (item: ImportItem) =
        item.IncludeConstructors
        && item.Namespace = Some ImportNamespace.Type
        && String.Equals(item.Name, typeName, StringComparison.Ordinal)

    let private exceptContains name (items: ExceptItem list) =
        items |> List.exists (fun item -> String.Equals(item.Name, name, StringComparison.Ordinal))

    let private exceptMatches namespaceName name (item: ExceptItem) =
        String.Equals(item.Name, name, StringComparison.Ordinal)
        && (item.Namespace.IsNone || item.Namespace = Some namespaceName)

    let private resolveImportedNames
        (exported: Set<string>)
        (selection: ImportSelection)
        (includeItem: ImportItem -> bool)
        =
        match selection with
        | QualifiedOnly ->
            Map.empty
        | Items items ->
            items
            |> List.choose (fun item ->
                if includeItem item then Some(importedItemLocalName item, item.Name) else None)
            |> Map.ofList
        | All ->
            exported |> Seq.map (fun name -> name, name) |> Map.ofSeq
        | AllExcept excluded ->
            exported
            |> Seq.filter (fun name -> not (exceptContains name excluded))
            |> Seq.map (fun name -> name, name)
            |> Map.ofSeq

    let private resolveImportedTerms (inventory: ExportInventory) (selection: ImportSelection) =
        resolveImportedNames inventory.Terms selection itemImportsTerm

    let private resolveImportedTypes (inventory: ExportInventory) (selection: ImportSelection) =
        resolveImportedNames inventory.Types selection itemImportsType

    let private resolveImportedTraits (inventory: ExportInventory) (selection: ImportSelection) =
        resolveImportedNames inventory.Traits selection itemImportsTrait

    let private selectionImportsConstructorName (importedModel: ModuleModel) (inventory: ExportInventory) (selection: ImportSelection) (name: string) =
        let exportsConstructor =
            importedModel.Constructors.ContainsKey(name)
            && inventory.Constructors.Contains(name)

        let constructorTypeName =
            importedModel.Constructors
            |> Map.tryFind name
            |> Option.map (fun constructorInfo -> constructorInfo.ParentTypeName)

        match selection with
        | QualifiedOnly ->
            false
        | Items items ->
            exportsConstructor
            && (items
                |> List.exists (fun item ->
                    (String.Equals(item.Name, name, StringComparison.Ordinal) && itemImportsConstructor item)
                    || (constructorTypeName |> Option.exists (fun typeName -> itemImportsConstructorsOfType typeName item))))
        | All ->
            false
        | AllExcept excludedItems ->
            exportsConstructor
            && not (excludedItems |> List.exists (exceptMatches ImportNamespace.Constructor name))

    let private resolveVisibleTermReference inventories models moduleIdentity localName =
        let model = Map.find moduleIdentity models

        model.Terms
        |> Map.tryFind localName
        |> Option.map (fun term -> term.ModuleIdentity, term.Name)
        |> Option.orElseWith (fun () ->
            model.Imports
            |> List.tryPick (fun spec ->
                match SymbolicIdentity.moduleIdentityOfSpecifier spec.Source with
                | Some importedModuleIdentity ->
                    inventories
                    |> Map.tryFind importedModuleIdentity
                    |> Option.bind (fun inventory ->
                        resolveImportedTerms inventory spec.Selection
                        |> Map.tryFind localName
                        |> Option.map (fun importedName -> importedModuleIdentity, importedName))
                | None ->
                    None))

    let private resolveVisibleTypeReference inventories models moduleIdentity localName =
        let model = Map.find moduleIdentity models

        model.Types
        |> Map.tryFind localName
        |> Option.map (fun _ -> model.ModuleIdentity, localName)
        |> Option.orElseWith (fun () ->
            model.Imports
            |> List.tryPick (fun spec ->
                match SymbolicIdentity.moduleIdentityOfSpecifier spec.Source with
                | Some importedModuleIdentity ->
                    inventories
                    |> Map.tryFind importedModuleIdentity
                    |> Option.bind (fun inventory ->
                        resolveImportedTypes inventory spec.Selection
                        |> Map.tryFind localName
                        |> Option.map (fun importedName -> importedModuleIdentity, importedName))
                | None ->
                    None))

    let private resolveVisibleTraitReference inventories models moduleIdentity localName =
        let model = Map.find moduleIdentity models

        model.Traits
        |> Map.tryFind localName
        |> Option.map (fun _ -> model.ModuleIdentity, localName)
        |> Option.orElseWith (fun () ->
            model.Imports
            |> List.tryPick (fun spec ->
                match SymbolicIdentity.moduleIdentityOfSpecifier spec.Source with
                | Some importedModuleIdentity ->
                    inventories
                    |> Map.tryFind importedModuleIdentity
                    |> Option.bind (fun inventory ->
                        resolveImportedTraits inventory spec.Selection
                        |> Map.tryFind localName
                        |> Option.map (fun importedName -> importedModuleIdentity, importedName))
                | None ->
                    None))

    let private resolveVisibleConstructorReference inventories models moduleIdentity localName =
        let model = Map.find moduleIdentity models

        model.Constructors
        |> Map.tryFind localName
        |> Option.map (fun constructor -> constructor.ModuleIdentity, constructor.Constructor.Name)
        |> Option.orElseWith (fun () ->
            model.Imports
            |> List.tryPick (fun spec ->
                match SymbolicIdentity.moduleIdentityOfSpecifier spec.Source with
                | Some importedModuleIdentity ->
                    match inventories |> Map.tryFind importedModuleIdentity, models |> Map.tryFind importedModuleIdentity with
                    | Some inventory, Some importedModel when selectionImportsConstructorName importedModel inventory spec.Selection localName ->
                        Some(importedModuleIdentity, localName)
                    | _ ->
                        None
                | None ->
                    None))

    let private collectPatternBoundNames pattern =
        let rec loop current =
            match current with
            | WildcardPattern
            | LiteralPattern _ -> []
            | NamePattern name -> [ name ]
            | AsPattern(name, inner) -> name :: loop inner
            | TypedPattern(inner, _) -> loop inner
            | ConstructorPattern(_, patterns) -> patterns |> List.collect loop
            | NamedConstructorPattern(_, fields) -> fields |> List.collect (fun field -> loop field.Pattern)
            | TuplePattern patterns -> patterns |> List.collect loop
            | VariantPattern _ -> []
            | OrPattern patterns -> patterns |> List.collect loop
            | AnonymousRecordPattern(fields, rest) ->
                let fieldNames = fields |> List.collect (fun field -> loop field.Pattern)
                match rest with
                | Some(BindRecordPatternRest name) -> name :: fieldNames
                | _ -> fieldNames

        loop pattern |> Set.ofList

    let private mergeDiagnostics left right = left @ right

    let private normalizeBuiltinName (builtinName: string) =
        if builtinName.StartsWith("std.deriving.shape.", StringComparison.Ordinal) then
            builtinName.Substring("std.deriving.shape.".Length)
        elif builtinName.StartsWith("std.prelude.", StringComparison.Ordinal) then
            builtinName.Substring("std.prelude.".Length)
        else
            builtinName

    let private metaEquals left right =
        let rec loop a b =
            match a, b with
            | MetaUnit, MetaUnit -> true
            | MetaBool left, MetaBool right -> left = right
            | MetaString left, MetaString right -> left = right
            | MetaInt left, MetaInt right -> left = right
            | MetaNat left, MetaNat right -> left = right
            | MetaList left, MetaList right -> List.length left = List.length right && List.forall2 loop left right
            | MetaRecord leftFields, MetaRecord rightFields ->
                List.length leftFields = List.length rightFields
                && List.forall2 (fun leftField rightField -> String.Equals(leftField.Name, rightField.Name, StringComparison.Ordinal) && loop leftField.Value rightField.Value) leftFields rightFields
            | MetaData(leftName, leftFields), MetaData(rightName, rightFields) ->
                leftName = rightName
                && List.length leftFields = List.length rightFields
                && List.forall2 (fun leftField rightField -> String.Equals(leftField.Name, rightField.Name, StringComparison.Ordinal) && loop leftField.Value rightField.Value) leftFields rightFields
            | _ -> false

        loop left right

    let private tryAsBool value =
        match value with
        | MetaBool value -> Some value
        | _ -> None

    let private tryAsString value =
        match value with
        | MetaString value -> Some value
        | _ -> None

    let rec private tryResolveMetaFieldPath value segments =
        match segments with
        | [] -> Some value
        | segment :: rest ->
            match value with
            | MetaRecord fields
            | MetaData(_, fields) ->
                fields
                |> List.tryFind (fun field -> String.Equals(field.Name, segment, StringComparison.Ordinal))
                |> Option.bind (fun field -> tryResolveMetaFieldPath field.Value rest)
            | _ ->
                None

    let rec private tryParseTypeSyntaxValue value =
        match value with
        | ParsedTypeExpr typeExpr -> Some typeExpr
        | RawTypeTokens tokens -> TypeSignatures.parseType tokens

    let rec private tryParseTypeLikeSurfaceExpression expression =
        match expression with
        | Name segments when not (List.isEmpty segments) -> Some(TypeSignatures.TypeName(segments, []))
        | Apply(head, arguments) ->
            match tryParseTypeLikeSurfaceExpression head, arguments |> List.map tryParseTypeLikeSurfaceExpression with
            | Some(TypeSignatures.TypeName(name, existingArguments)), parsedArguments when parsedArguments |> List.forall Option.isSome ->
                Some(TypeSignatures.TypeName(name, existingArguments @ (parsedArguments |> List.choose id)))
            | _ -> None
        | ExplicitImplicitArgument inner -> tryParseTypeLikeSurfaceExpression inner
        | TypeSyntaxTokens tokens -> TypeSignatures.parseType tokens
        | _ -> None

    let private tryTypeSyntaxFromExpression expression =
        match expression with
        | TypeSyntaxTokens tokens -> Some(RawTypeTokens tokens)
        | _ -> tryParseTypeLikeSurfaceExpression expression |> Option.map ParsedTypeExpr

    let private isOpenRecordTypeTokens (tokens: Token list) =
        let mutable depth = 0
        let mutable foundColon = false
        let mutable foundBar = false

        for token in tokens do
            match token.Kind with
            | LeftParen
            | LeftBracket
            | LeftBrace
            | LeftSetBrace
            | LeftEffectRow -> depth <- depth + 1
            | RightParen
            | RightBracket
            | RightBrace
            | RightSetBrace
            | RightEffectRow -> depth <- max 0 (depth - 1)
            | Colon when depth >= 1 -> foundColon <- true
            | Operator when token.Text = "|" && depth >= 1 -> foundBar <- true
            | _ -> ()

        foundColon && foundBar

    let private valueField name value = { Name = name; Value = value }
    let private metaString text = MetaString text
    let private metaBool value = MetaBool value
    let private metaNat (value: int) = MetaNat(bigint value)

    let private runtimeRelevantType typeExpr =
        match typeExpr with
        | TypeSignatures.TypeUniverse _ -> false
        | TypeSignatures.TypeIntrinsic TypeSignatures.QuantityClassifier
        | TypeSignatures.TypeIntrinsic TypeSignatures.RegionClassifier
        | TypeSignatures.TypeIntrinsic TypeSignatures.ConstraintClassifier
        | TypeSignatures.TypeIntrinsic TypeSignatures.RecRowClassifier
        | TypeSignatures.TypeIntrinsic TypeSignatures.VarRowClassifier
        | TypeSignatures.TypeIntrinsic TypeSignatures.EffRowClassifier
        | TypeSignatures.TypeIntrinsic TypeSignatures.LabelClassifier
        | TypeSignatures.TypeIntrinsic TypeSignatures.EffLabelClassifier -> false
        | _ -> true

    let private syntaxValueWithScope scopeModule expression =
        { Expression = expression
          CapturedNames = Set.empty
          ScopeModule = scopeModule }

    let private emptySyntaxValue expression = syntaxValueWithScope None expression

    let private combineSyntaxScopeModules (scopeModules: ModuleIdentity option list) =
        match scopeModules with
        | [] -> None
        | first :: rest ->
            rest
            |> List.fold
                (fun state next ->
                    match state, next with
                    | Some left, Some right when left = right -> Some left
                    | _ -> None)
                first

    let private typeSyntaxTokensFromText (text: string) =
        let source = SourceText.From("__kappa_elab_type__.kp", text)
        let lexResult = Lexer.tokenize source

        lexResult.Tokens
        |> List.filter (fun token ->
            match token.Kind with
            | EndOfFile -> false
            | _ -> true)

    let private syntaxValueForTypeExpr typeExpr =
        typeExpr
        |> TypeSignatures.toText
        |> typeSyntaxTokensFromText
        |> TypeSyntaxTokens
        |> emptySyntaxValue

    let private shapeFieldData fieldName typeExpr =
        MetaData(
            CompilerKnownSymbols.KnownTypeNames.ShapeField,
            [ valueField "sourceName" MetaUnit
              valueField "renderName" (metaString fieldName)
              valueField "origin" MetaUnit
              valueField "typeOrigin" MetaUnit
              valueField "fieldType" (MetaTypeSyntax(ParsedTypeExpr typeExpr))
              valueField "fieldTypeSyntax" (MetaSyntax(syntaxValueForTypeExpr typeExpr))
              valueField "quantity" MetaUnit
              valueField "implicit" (metaBool false)
              valueField "compileTimeOnly" (metaBool false)
              valueField "runtimeRelevant" (metaBool (runtimeRelevantType typeExpr)) ]
        )

    let private shapeConstructorData typeName tag (constructor: DataConstructor) =
        let fields =
            constructor.Parameters
            |> Option.defaultValue []
            |> List.mapi (fun index parameter ->
                let fieldName = parameter.ParameterName |> Option.defaultValue $"_{index + 1}"
                let fieldType =
                    parameter.ParameterTypeTokens
                    |> TypeSignatures.parseType
                    |> Option.defaultValue (TypeSignatures.knownType CompilerKnownSymbols.UnitType [])
                shapeFieldData fieldName fieldType)

        MetaData(
            CompilerKnownSymbols.KnownTypeNames.ShapeConstructor,
            [ valueField "symbol" (metaString $"{typeName}.{constructor.Name}")
              valueField "sourceName" (metaString constructor.Name)
              valueField "renderName" (metaString constructor.Name)
              valueField "origin" MetaUnit
              valueField "tag" (metaNat tag)
              valueField "fields" (MetaList fields) ]
        )

    let private shapeAdtData visibility kind (typeName: string) (declaration: DataDeclaration) =
        let constructors = declaration.Constructors |> List.mapi (fun index constructor -> shapeConstructorData typeName index constructor)
        MetaData(
            CompilerKnownSymbols.KnownTypeNames.AdtShape,
            [ valueField "symbol" (metaString typeName)
              valueField "sourceName" (metaString typeName)
              valueField "renderName" (metaString typeName)
              valueField "origin" MetaUnit
              valueField "visibility" (MetaData(visibility, []))
              valueField "kind" (MetaData(kind, []))
              valueField "parameters" (MetaList [])
              valueField "constructors" (MetaList constructors) ]
        )

    let private shapeRecordData (fields: TypeSignatures.RecordField list) =
        MetaData(
            CompilerKnownSymbols.KnownTypeNames.RecordShape,
            [ valueField "origin" MetaUnit
              valueField "fields" (MetaList(fields |> List.map (fun field -> shapeFieldData field.Name field.Type))) ]
        )

    let private kindForDataDeclaration (declaration: DataDeclaration) =
        match declaration.Constructors with
        | [ _ ] -> "ProductAdt"
        | constructors ->
            let anyRuntimeFields =
                constructors
                |> List.exists (fun constructor ->
                    constructor.Parameters
                    |> Option.defaultValue []
                    |> List.exists (fun parameter ->
                        parameter.ParameterTypeTokens
                        |> TypeSignatures.parseType
                        |> Option.map runtimeRelevantType
                        |> Option.defaultValue true))

            if anyRuntimeFields then "SumAdt" else "EnumAdt"

    let private tryGetField name fields =
        fields |> List.tryFind (fun field -> String.Equals(field.Name, name, StringComparison.Ordinal)) |> Option.map (fun field -> field.Value)

    let private tryGetDataField name value =
        match value with
        | MetaData(_, fields)
        | MetaRecord fields -> tryGetField name fields
        | _ -> None

    let private tryAsMetaList value =
        match value with
        | MetaList items -> Some items
        | _ -> None

    let private visibleInstancesForModule models inventories moduleIdentity =
        let model = Map.find moduleIdentity models
        let importedInstances =
            model.Imports
            |> List.collect (fun spec ->
                match SymbolicIdentity.moduleIdentityOfSpecifier spec.Source with
                | Some importedModuleIdentity ->
                    models |> Map.tryFind importedModuleIdentity |> Option.map (fun imported -> imported.Instances) |> Option.defaultValue []
                | None ->
                    [])
        model.Instances @ importedInstances

    let private tryParseInstanceHeader (declaration: InstanceDeclaration) =
        declaration.FullHeaderTokens
        |> TypeSignatures.parseScheme
        |> Option.bind (fun scheme ->
            match scheme.Body with
            | TypeSignatures.TypeName(name, headTypes) ->
                let traitName =
                    match name with
                    | [ singleName ] -> singleName
                    | _ -> SyntaxFacts.moduleNameToText name
                Some(traitName, headTypes)
            | _ -> None)

    let private visibleTypeAliasDefinitions models inventories moduleIdentity =
        let moduleModel = Map.find moduleIdentity models

        let localAliases =
            moduleModel.Types
            |> Map.toList
            |> List.choose (fun (name, definition) ->
                match definition with
                | SurfaceAlias(aliasModule, declaration) -> Some(aliasModule, name, declaration)
                | _ -> None)

        let importedAliases =
            moduleModel.Imports
            |> List.collect (fun spec ->
                match SymbolicIdentity.moduleIdentityOfSpecifier spec.Source with
                | Some importedModuleIdentity ->
                    match models |> Map.tryFind importedModuleIdentity, inventories |> Map.tryFind importedModuleIdentity with
                    | Some importedModel, Some inventory ->
                        resolveImportedTypes inventory spec.Selection
                        |> Map.toList
                        |> List.choose (fun (_, importedName) ->
                            match importedModel.Types |> Map.tryFind importedName with
                            | Some(SurfaceAlias(aliasModule, declaration)) -> Some(aliasModule, importedName, declaration)
                            | _ -> None)
                    | _ -> []
                | None ->
                    [])

        localAliases @ importedAliases
        |> List.choose (fun (aliasModule, name, declaration) ->
            declaration.BodyTokens
            |> Option.bind TypeSignatures.parseType
            |> Option.map (fun body -> aliasModule, name, declaration, body))

    let rec private substituteTypeParameters substitution typeExpr =
        match typeExpr with
        | TypeSignatures.TypeVariable name -> Map.tryFind name substitution |> Option.defaultValue typeExpr
        | TypeSignatures.TypeName(name, arguments) ->
            TypeSignatures.TypeName(name, arguments |> List.map (substituteTypeParameters substitution))
        | TypeSignatures.TypeArrow(quantity, parameterType, resultType) ->
            TypeSignatures.TypeArrow(quantity, substituteTypeParameters substitution parameterType, substituteTypeParameters substitution resultType)
        | TypeSignatures.TypeRecord fields ->
            TypeSignatures.TypeRecord(fields |> List.map (fun field -> { field with Type = substituteTypeParameters substitution field.Type }))
        | TypeSignatures.TypeApply(head, arguments) ->
            TypeSignatures.TypeApply(substituteTypeParameters substitution head, arguments |> List.map (substituteTypeParameters substitution))
        | other -> other

    let rec private normalizeVisibleType models inventories moduleIdentity typeSyntax =
        match typeSyntax with
        | ParsedTypeExpr(TypeSignatures.TypeName(name, arguments)) ->
            let localName = SyntaxFacts.moduleNameToText name
            match resolveVisibleTypeReference inventories models moduleIdentity localName with
            | Some(resolvedModule, resolvedName) ->
                match (Map.find resolvedModule models).Types |> Map.tryFind resolvedName with
                | Some(SurfaceAlias(aliasModule, declaration)) when not declaration.IsOpaque ->
                    let parsedArguments = arguments |> List.map ParsedTypeExpr
                    let resolvedAliases = visibleTypeAliasDefinitions models inventories moduleIdentity
                    resolvedAliases
                    |> List.tryFind (fun (candidateModule, candidateName, _, _) -> candidateModule = aliasModule && candidateName = resolvedName)
                    |> Option.map (fun (_, _, declaration, body) ->
                        let parameters = TypeSignatures.collectLeadingTypeParameters declaration.HeaderTokens
                        let substitution = List.zip parameters arguments |> Map.ofList
                        ParsedTypeExpr(substituteTypeParameters substitution body))
                    |> Option.map (normalizeVisibleType models inventories moduleIdentity)
                    |> Option.defaultValue typeSyntax
                | _ -> typeSyntax
            | None -> typeSyntax
        | _ -> typeSyntax

    let private classifyRecordSyntax models inventories moduleIdentity typeSyntax =
        match normalizeVisibleType models inventories moduleIdentity typeSyntax with
        | ParsedTypeExpr(TypeSignatures.TypeRecord fields) -> Some(Result.Ok fields)
        | RawTypeTokens tokens when isOpenRecordTypeTokens tokens -> Some(Result.Error DiagnosticCode.DerivingShapeNotClosedRecord)
        | _ -> None

    let private classifyAdtSyntax models inventories moduleIdentity typeSyntax =
        match normalizeVisibleType models inventories moduleIdentity typeSyntax with
        | ParsedTypeExpr(TypeSignatures.TypeName(name, _)) ->
            let localName = SyntaxFacts.moduleNameToText name
            match resolveVisibleTypeReference inventories models moduleIdentity localName with
            | Some(resolvedModule, resolvedName) ->
                match (Map.find resolvedModule models).Types |> Map.tryFind resolvedName with
                | Some(SurfaceData(dataModule, declaration)) ->
                    if declaration.IsOpaque && dataModule <> moduleIdentity then
                        Result.Error DiagnosticCode.DerivingShapeOpaqueRepresentation
                    else
                        let visibility = if declaration.IsOpaque then "ShapeRepresentationOpaque" else "ShapeRepresentationVisible"
                        Result.Ok(shapeAdtData visibility (kindForDataDeclaration declaration) resolvedName declaration)
                | Some(SurfaceAlias _) -> Result.Error DiagnosticCode.DerivingShapeNotData
                | _ -> Result.Error DiagnosticCode.DerivingShapeNotData
            | None -> Result.Error DiagnosticCode.DerivingShapeNotData
        | ParsedTypeExpr(TypeSignatures.TypeArrow _) -> Result.Error DiagnosticCode.DerivingShapeNotData
        | ParsedTypeExpr(TypeSignatures.TypeRecord _) -> Result.Error DiagnosticCode.DerivingShapeNotData
        | RawTypeTokens _ -> Result.Error DiagnosticCode.DerivingShapeNotData
        | _ -> Result.Error DiagnosticCode.DerivingShapeNotData

    let private makeShapeDiagnostic code message source = makeMacroCodeError code message source

    let private constructorFieldName index (parameter: DataConstructorParameter) =
        parameter.ParameterName |> Option.defaultValue $"_{index + 1}"

    let private finalizeConstructorValue constructorName (parameters: DataConstructorParameter list) (appliedArguments: MetaValue list) =
        match constructorName, appliedArguments with
        | "True", [] -> MetaBool true
        | "False", [] -> MetaBool false
        | "Nil", [] -> MetaList []
        | "::", [ head; MetaList tail ] -> MetaList(head :: tail)
        | _ ->
            MetaData(
                constructorName,
                (parameters, appliedArguments)
                ||> List.zip
                |> List.mapi (fun index (parameter, argumentValue) ->
                    valueField (constructorFieldName index parameter) argumentValue)
            )

    let rec private evalExpression (context: RewriteContext) (expression: SurfaceExpression) : EvalResult =
        let success value diagnostics = EvalSucceeded(value, diagnostics)

        let rec forceValue value diagnostics =
            match value with
            | MetaClosure closure when List.isEmpty closure.Parameters ->
                let nextContext =
                    { context with
                        CurrentModule = closure.ModuleIdentity
                        MetaBindings = closure.MetaBindings }
                match evalExpression nextContext closure.Body with
                | EvalSucceeded(result, resultDiagnostics) -> EvalSucceeded(result, mergeDiagnostics diagnostics resultDiagnostics)
                | EvalFailed resultDiagnostics -> EvalFailed(mergeDiagnostics diagnostics resultDiagnostics)
                | EvalBlocked -> EvalBlocked
            | _ -> EvalSucceeded(value, diagnostics)

        let rec evalQuotedSyntax shadowed current =
            let recurse = evalQuotedSyntax shadowed

            let combineMany
                (items: SurfaceExpression list)
                builder
                =
                let rec loop accExprs accCaptured accDiagnostics remaining =
                    match remaining with
                    | [] -> Some(builder (List.rev accExprs), accCaptured, accDiagnostics)
                    | item :: rest ->
                        match recurse item with
                        | EvalSucceeded(MetaSyntax syntaxValue, diagnostics) ->
                            loop
                                (syntaxValue.Expression :: accExprs)
                                (Set.union accCaptured syntaxValue.CapturedNames)
                                (mergeDiagnostics accDiagnostics diagnostics)
                                rest
                        | _ -> None
                loop [] Set.empty [] items

            match current with
            | SyntaxSplice inner ->
                match evalExpression context inner with
                | EvalSucceeded(MetaSyntax syntaxValue, diagnostics) -> EvalSucceeded(MetaSyntax syntaxValue, diagnostics)
                | EvalFailed diagnostics -> EvalFailed diagnostics
                | _ -> EvalBlocked
            | Name [ name ] when Set.contains name context.ObjectBinders && not (Set.contains name shadowed) ->
                success
                    (MetaSyntax
                        { Expression = current
                          CapturedNames = Set.singleton name
                          ScopeModule = Some context.CurrentModule })
                    []
            | Name _
            | Literal _
            | NumericLiteral _
            | KindQualifiedName _
            | TypeSyntaxTokens _ ->
                success
                    (MetaSyntax
                        { Expression = current
                          CapturedNames = Set.empty
                          ScopeModule = Some context.CurrentModule })
                    []
            | Unary(operatorName, inner) ->
                match recurse inner with
                | EvalSucceeded(MetaSyntax syntaxValue, diagnostics) ->
                    success
                        (MetaSyntax
                            { Expression = Unary(operatorName, syntaxValue.Expression)
                              CapturedNames = syntaxValue.CapturedNames
                              ScopeModule = syntaxValue.ScopeModule })
                        diagnostics
                | other -> other
            | Binary(left, operatorName, right) ->
                match recurse left, recurse right with
                | EvalSucceeded(MetaSyntax leftSyntax, leftDiagnostics), EvalSucceeded(MetaSyntax rightSyntax, rightDiagnostics) ->
                    success
                        (MetaSyntax
                            { Expression = Binary(leftSyntax.Expression, operatorName, rightSyntax.Expression)
                              CapturedNames = Set.union leftSyntax.CapturedNames rightSyntax.CapturedNames
                              ScopeModule = combineSyntaxScopeModules [ leftSyntax.ScopeModule; rightSyntax.ScopeModule ] })
                        (mergeDiagnostics leftDiagnostics rightDiagnostics)
                | EvalFailed diagnostics, _
                | _, EvalFailed diagnostics -> EvalFailed diagnostics
                | _ -> EvalBlocked
            | Apply(callee, arguments) ->
                match recurse callee, combineMany arguments (fun expressions -> expressions) with
                | EvalSucceeded(MetaSyntax calleeSyntax, calleeDiagnostics), Some(argumentExpressions, argumentCaptured, argumentDiagnostics) ->
                    success
                        (MetaSyntax
                            { Expression = Apply(calleeSyntax.Expression, argumentExpressions)
                              CapturedNames = Set.union calleeSyntax.CapturedNames argumentCaptured
                              ScopeModule = calleeSyntax.ScopeModule })
                        (mergeDiagnostics calleeDiagnostics argumentDiagnostics)
                | EvalFailed diagnostics, _ -> EvalFailed diagnostics
                | _ -> EvalBlocked
            | RecordLiteral fields ->
                let rec collect
                    (currentFields: SurfaceRecordLiteralField list)
                    currentCaptured
                    currentDiagnostics
                    (remaining: SurfaceRecordLiteralField list)
                    =
                    match remaining with
                    | [] -> Some(List.rev currentFields, currentCaptured, currentDiagnostics)
                    | (field: SurfaceRecordLiteralField) :: rest ->
                        match recurse field.Value with
                        | EvalSucceeded(MetaSyntax syntaxValue, diagnostics) ->
                            collect
                                ({ field with Value = syntaxValue.Expression } :: currentFields)
                                (Set.union currentCaptured syntaxValue.CapturedNames)
                                (mergeDiagnostics currentDiagnostics diagnostics)
                                rest
                        | _ -> None
                match collect [] Set.empty [] fields with
                | Some(rewrittenFields, capturedNames, diagnostics) ->
                    success
                        (MetaSyntax
                            { Expression = RecordLiteral rewrittenFields
                              CapturedNames = capturedNames
                              ScopeModule = Some context.CurrentModule })
                        diagnostics
                | None -> EvalBlocked
            | Match(scrutinee, cases) ->
                match recurse scrutinee with
                | EvalSucceeded(MetaSyntax scrutineeSyntax, scrutineeDiagnostics) ->
                    let rec collectCases
                        (currentCases: SurfaceMatchCase list)
                        currentCaptured
                        currentDiagnostics
                        (remaining: SurfaceMatchCase list)
                        =
                        match remaining with
                        | [] -> Some(List.rev currentCases, currentCaptured, currentDiagnostics)
                        | (caseClause: SurfaceMatchCase) :: rest ->
                            let nextShadowed = Set.union shadowed (collectPatternBoundNames caseClause.Pattern)
                            match evalQuotedSyntax nextShadowed caseClause.Body with
                            | EvalSucceeded(MetaSyntax bodySyntax, bodyDiagnostics) ->
                                let guardResult =
                                    match caseClause.Guard with
                                    | None -> Some(None, [], Set.empty)
                                    | Some guard ->
                                        match evalQuotedSyntax nextShadowed guard with
                                        | EvalSucceeded(MetaSyntax guardSyntax, guardDiagnostics) -> Some(Some guardSyntax.Expression, guardDiagnostics, guardSyntax.CapturedNames)
                                        | _ -> None
                                match guardResult with
                                | Some(guardExpression, guardDiagnostics, guardCaptured) ->
                                    collectCases
                                        ({ caseClause with Guard = guardExpression; Body = bodySyntax.Expression } :: currentCases)
                                        (Set.unionMany [ currentCaptured; bodySyntax.CapturedNames; guardCaptured ])
                                        (mergeDiagnostics currentDiagnostics (mergeDiagnostics bodyDiagnostics guardDiagnostics))
                                        rest
                                | None -> None
                            | _ -> None
                    match collectCases [] scrutineeSyntax.CapturedNames scrutineeDiagnostics cases with
                    | Some(rewrittenCases, capturedNames, diagnostics) ->
                        success
                            (MetaSyntax
                                { Expression = Match(scrutineeSyntax.Expression, rewrittenCases)
                                  CapturedNames = capturedNames
                                  ScopeModule = scrutineeSyntax.ScopeModule })
                            diagnostics
                    | None -> EvalBlocked
                | _ -> EvalBlocked
            | Lambda(parameters, body) ->
                let parameterNames = parameters |> List.map (fun parameter -> parameter.Name) |> Set.ofList
                match evalQuotedSyntax (Set.union shadowed parameterNames) body with
                | EvalSucceeded(MetaSyntax bodySyntax, diagnostics) ->
                    success
                        (MetaSyntax
                            { Expression = Lambda(parameters, bodySyntax.Expression)
                              CapturedNames = bodySyntax.CapturedNames - parameterNames
                              ScopeModule = bodySyntax.ScopeModule })
                        diagnostics
                | other -> other
            | LocalLet(binding, value, body) ->
                let bindingNames = collectPatternBoundNames binding.Pattern
                match recurse value, evalQuotedSyntax (Set.union shadowed bindingNames) body with
                | EvalSucceeded(MetaSyntax valueSyntax, valueDiagnostics), EvalSucceeded(MetaSyntax bodySyntax, bodyDiagnostics) ->
                    success
                        (MetaSyntax
                            { Expression = LocalLet(binding, valueSyntax.Expression, bodySyntax.Expression)
                              CapturedNames = Set.union valueSyntax.CapturedNames (bodySyntax.CapturedNames - bindingNames)
                              ScopeModule = combineSyntaxScopeModules [ valueSyntax.ScopeModule; bodySyntax.ScopeModule ] })
                        (mergeDiagnostics valueDiagnostics bodyDiagnostics)
                | _ -> EvalBlocked
            | IfThenElse(condition, whenTrue, whenFalse) ->
                match recurse condition, recurse whenTrue, recurse whenFalse with
                | EvalSucceeded(MetaSyntax conditionSyntax, conditionDiagnostics), EvalSucceeded(MetaSyntax trueSyntax, trueDiagnostics), EvalSucceeded(MetaSyntax falseSyntax, falseDiagnostics) ->
                    success
                        (MetaSyntax
                            { Expression = IfThenElse(conditionSyntax.Expression, trueSyntax.Expression, falseSyntax.Expression)
                              CapturedNames = Set.unionMany [ conditionSyntax.CapturedNames; trueSyntax.CapturedNames; falseSyntax.CapturedNames ]
                              ScopeModule =
                                combineSyntaxScopeModules
                                    [ conditionSyntax.ScopeModule
                                      trueSyntax.ScopeModule
                                      falseSyntax.ScopeModule ] })
                        (mergeDiagnostics (mergeDiagnostics conditionDiagnostics trueDiagnostics) falseDiagnostics)
                | _ -> EvalBlocked
            | MemberAccess(receiver, segments, arguments) ->
                match recurse receiver, combineMany arguments (fun expressions -> expressions) with
                | EvalSucceeded(MetaSyntax receiverSyntax, receiverDiagnostics), Some(argumentExpressions, argumentCaptured, argumentDiagnostics) ->
                    success
                        (MetaSyntax
                            { Expression = MemberAccess(receiverSyntax.Expression, segments, argumentExpressions)
                              CapturedNames = Set.union receiverSyntax.CapturedNames argumentCaptured
                              ScopeModule = receiverSyntax.ScopeModule })
                        (mergeDiagnostics receiverDiagnostics argumentDiagnostics)
                | EvalFailed diagnostics, _ -> EvalFailed diagnostics
                | _ -> EvalBlocked
            | ExplicitImplicitArgument inner ->
                match recurse inner with
                | EvalSucceeded(MetaSyntax syntaxValue, diagnostics) ->
                    success
                        (MetaSyntax
                            { Expression = ExplicitImplicitArgument syntaxValue.Expression
                              CapturedNames = syntaxValue.CapturedNames
                              ScopeModule = syntaxValue.ScopeModule })
                        diagnostics
                | _ -> EvalBlocked
            | SyntaxQuote _
            | TopLevelSyntaxSplice _
            | CodeQuote _
            | CodeSplice _
            | MonadicSplice _
            | Do _
            | PrefixedString _
            | Handle _
            | LocalSignature _
            | LocalTypeAlias _
            | LocalScopedEffect _
            | Seal _
            | RecordUpdate _
            | SafeNavigation _
            | TagTest _
            | NamedApplicationBlock _
            | InoutArgument _
            | Elvis _
            | Comprehension _ -> EvalBlocked

        and applyMeta functionValue argumentValue diagnostics =
            match functionValue with
            | MetaClosure closure ->
                match closure.Parameters with
                | [] -> EvalBlocked
                | parameter :: remainingParameters ->
                    let nextBindings = Map.add parameter.Name argumentValue closure.MetaBindings
                    if List.isEmpty remainingParameters then
                        let nextContext =
                            { context with
                                CurrentModule = closure.ModuleIdentity
                                MetaBindings = nextBindings }
                        match evalExpression nextContext closure.Body with
                        | EvalSucceeded(value, bodyDiagnostics) -> forceValue value (mergeDiagnostics diagnostics bodyDiagnostics)
                        | EvalFailed bodyDiagnostics -> EvalFailed(mergeDiagnostics diagnostics bodyDiagnostics)
                        | EvalBlocked -> EvalBlocked
                    else
                        EvalSucceeded(MetaClosure { closure with Parameters = remainingParameters; MetaBindings = nextBindings }, diagnostics)
            | MetaBuiltin(name, appliedArguments) ->
                applyBuiltin context name (appliedArguments @ [ argumentValue ]) diagnostics
            | MetaConstructor(constructorName, parameters, appliedArguments) ->
                let nextArguments = appliedArguments @ [ argumentValue ]

                if List.length nextArguments < List.length parameters then
                    EvalSucceeded(MetaConstructor(constructorName, parameters, nextArguments), diagnostics)
                elif List.length nextArguments = List.length parameters then
                    EvalSucceeded(finalizeConstructorValue constructorName parameters nextArguments, diagnostics)
                else
                    EvalBlocked
            | _ -> EvalBlocked

        match expression with
        | Literal LiteralValue.Unit -> success MetaUnit []
        | Literal(LiteralValue.String text) -> success (MetaString text) []
        | Literal(LiteralValue.Integer value) -> success (MetaInt value) []
        | NumericLiteral(SurfaceIntegerLiteral(value, _, _)) -> success (MetaNat value) []
        | Name [ "True" ] -> success (MetaBool true) []
        | Name [ "False" ] -> success (MetaBool false) []
        | Name [ "Nil" ] -> success (MetaList []) []
        | Name [ "unitSyntax" ] -> success (MetaSyntax(syntaxValueWithScope (Some context.CurrentModule) (Literal LiteralValue.Unit))) []
        | Name [ name ] ->
            match context.MetaBindings |> Map.tryFind name with
            | Some value -> forceValue value []
            | None ->
                match resolveVisibleTermReference context.Inventories context.Models context.CurrentModule name with
                | Some(moduleIdentity, termName) ->
                    let definition = (Map.find moduleIdentity context.Models).Terms[termName]
                    if definition.IsExpect then
                        success (MetaBuiltin($"{definition.ModuleName}.{termName}", [])) []
                    else
                        match definition.Body with
                        | Some body ->
                            forceValue
                                (MetaClosure
                                    { ModuleIdentity = definition.ModuleIdentity
                                      ModuleName = definition.ModuleName
                                      Parameters = definition.Parameters
                                      Body = body
                                      MetaBindings = Map.empty })
                                []
                        | None -> EvalBlocked
                | None ->
                    match resolveVisibleTraitReference context.Inventories context.Models context.CurrentModule name with
                    | Some(moduleIdentity, traitName) ->
                        let traitModuleName = (Map.find moduleIdentity context.Models).ModuleName
                        success (MetaTraitRef(traitModuleName, traitName)) []
                    | None ->
                        match resolveVisibleConstructorReference context.Inventories context.Models context.CurrentModule name with
                        | Some(moduleIdentity, constructorName) ->
                            let constructorDefinition = (Map.find moduleIdentity context.Models).Constructors[constructorName]
                            let parameters = constructorDefinition.Constructor.Parameters |> Option.defaultValue []

                            if List.isEmpty parameters then
                                success (finalizeConstructorValue constructorName parameters []) []
                            else
                                success (MetaConstructor(constructorName, parameters, [])) []
                        | None ->
                            EvalBlocked
        | Name(head :: tail) ->
            match context.MetaBindings |> Map.tryFind head with
            | Some value ->
                match tryResolveMetaFieldPath value tail with
                | Some resolved -> forceValue resolved []
                | None -> EvalBlocked
            | None -> EvalBlocked
        | Name _ -> EvalBlocked
        | TypeSyntaxTokens tokens -> success (MetaTypeSyntax(RawTypeTokens tokens)) []
        | ExplicitImplicitArgument inner ->
            match evalExpression context inner with
            | EvalSucceeded(MetaTypeSyntax typeSyntax, diagnostics) ->
                success (MetaTypeSyntax typeSyntax) diagnostics
            | EvalSucceeded(MetaTraitRef(moduleName, traitName), diagnostics) ->
                success (MetaTraitRef(moduleName, traitName)) diagnostics
            | EvalSucceeded(MetaSyntax syntaxValue, diagnostics) ->
                match tryTypeSyntaxFromExpression syntaxValue.Expression with
                | Some typeSyntax ->
                    success (MetaTypeSyntax typeSyntax) diagnostics
                | None ->
                    EvalBlocked
            | EvalSucceeded(value, diagnostics) ->
                match tryTypeSyntaxFromExpression inner with
                | Some typeSyntax -> success (MetaTypeSyntax typeSyntax) diagnostics
                | None -> success value diagnostics
            | EvalFailed diagnostics ->
                EvalFailed diagnostics
            | EvalBlocked ->
                match tryTypeSyntaxFromExpression inner with
                | Some typeSyntax -> success (MetaTypeSyntax typeSyntax) []
                | None -> EvalBlocked
        | SyntaxQuote inner ->
            match evalQuotedSyntax Set.empty inner with
            | EvalSucceeded(MetaSyntax syntaxValue, diagnostics) -> success (MetaSyntax syntaxValue) diagnostics
            | EvalFailed diagnostics -> EvalFailed diagnostics
            | _ -> EvalBlocked
        | Lambda(parameters, body) ->
            success
                (MetaClosure
                    { ModuleIdentity = context.CurrentModule
                      ModuleName = (Map.find context.CurrentModule context.Models).ModuleName
                      Parameters = parameters
                      Body = body
                      MetaBindings = context.MetaBindings })
                []
        | LocalLet(binding, value, body) ->
            match binding.Pattern with
            | NamePattern name ->
                match evalExpression context value with
                | EvalSucceeded(valueResult, valueDiagnostics) ->
                    let nextContext = { context with MetaBindings = Map.add name valueResult context.MetaBindings }
                    match evalExpression nextContext body with
                    | EvalSucceeded(bodyValue, bodyDiagnostics) -> success bodyValue (mergeDiagnostics valueDiagnostics bodyDiagnostics)
                    | EvalFailed diagnostics -> EvalFailed(mergeDiagnostics valueDiagnostics diagnostics)
                    | EvalBlocked -> EvalBlocked
                | other -> other
            | _ -> EvalBlocked
        | IfThenElse(condition, whenTrue, whenFalse) ->
            match evalExpression context condition with
            | EvalSucceeded(conditionValue, conditionDiagnostics) ->
                match tryAsBool conditionValue with
                | Some true ->
                    match evalExpression context whenTrue with
                    | EvalSucceeded(value, diagnostics) -> success value (mergeDiagnostics conditionDiagnostics diagnostics)
                    | EvalFailed diagnostics -> EvalFailed(mergeDiagnostics conditionDiagnostics diagnostics)
                    | EvalBlocked -> EvalBlocked
                | Some false ->
                    match evalExpression context whenFalse with
                    | EvalSucceeded(value, diagnostics) -> success value (mergeDiagnostics conditionDiagnostics diagnostics)
                    | EvalFailed diagnostics -> EvalFailed(mergeDiagnostics conditionDiagnostics diagnostics)
                    | EvalBlocked -> EvalBlocked
                | None -> EvalBlocked
            | other -> other
        | Match(scrutinee, cases) ->
            let rec tryMatchPattern value (pattern: SurfacePattern) =
                match pattern with
                | WildcardPattern -> Some Map.empty
                | NamePattern name -> Some(Map.ofList [ name, value ])
                | LiteralPattern(LiteralValue.String expected) ->
                    match value with
                    | MetaString actual when String.Equals(expected, actual, StringComparison.Ordinal) -> Some Map.empty
                    | _ -> None
                | LiteralPattern LiteralValue.Unit ->
                    match value with
                    | MetaUnit -> Some Map.empty
                    | _ -> None
                | ConstructorPattern([ "Nil" ], []) ->
                    match value with
                    | MetaList [] -> Some Map.empty
                    | _ -> None
                | ConstructorPattern([ "::" ], [ headPattern; tailPattern ]) ->
                    match value with
                    | MetaList(head :: tail) ->
                        match tryMatchPattern head headPattern, tryMatchPattern (MetaList tail) tailPattern with
                        | Some leftBindings, Some rightBindings -> Some(Map.fold (fun state key current -> Map.add key current state) leftBindings rightBindings)
                        | _ -> None
                    | _ -> None
                | ConstructorPattern([ constructorName ], subPatterns) ->
                    match value with
                    | MetaData(actualName, fields) when actualName = constructorName && List.length fields = List.length subPatterns ->
                        (subPatterns, fields)
                        ||> List.zip
                        |> List.fold
                            (fun state (subPattern, field) ->
                                match state, tryMatchPattern field.Value subPattern with
                                | Some bindings, Some current -> Some(Map.fold (fun accumulator key currentValue -> Map.add key currentValue accumulator) bindings current)
                                | _ -> None)
                            (Some Map.empty)
                    | _ -> None
                | TuplePattern patterns ->
                    match value with
                    | MetaRecord fields when List.length patterns = List.length fields ->
                        (patterns, fields)
                        ||> List.zip
                        |> List.fold
                            (fun state (subPattern, field) ->
                                match state, tryMatchPattern field.Value subPattern with
                                | Some bindings, Some current -> Some(Map.fold (fun accumulator key currentValue -> Map.add key currentValue accumulator) bindings current)
                                | _ -> None)
                            (Some Map.empty)
                    | _ -> None
                | TypedPattern(inner, _) -> tryMatchPattern value inner
                | _ -> None

            match evalExpression context scrutinee with
            | EvalSucceeded(scrutineeValue, scrutineeDiagnostics) ->
                let rec tryCases (remaining: SurfaceMatchCase list) =
                    match remaining with
                    | [] -> EvalBlocked
                    | (caseClause: SurfaceMatchCase) :: rest ->
                        match tryMatchPattern scrutineeValue caseClause.Pattern with
                        | Some bindings ->
                            let nextContext =
                                { context with
                                    MetaBindings = Map.fold (fun state key value -> Map.add key value state) context.MetaBindings bindings }
                            match caseClause.Guard with
                            | Some guard ->
                                match evalExpression nextContext guard with
                                | EvalSucceeded(guardValue, guardDiagnostics) ->
                                    match tryAsBool guardValue with
                                    | Some true ->
                                        match evalExpression nextContext caseClause.Body with
                                        | EvalSucceeded(value, bodyDiagnostics) -> success value (mergeDiagnostics scrutineeDiagnostics (mergeDiagnostics guardDiagnostics bodyDiagnostics))
                                        | EvalFailed diagnostics -> EvalFailed(mergeDiagnostics scrutineeDiagnostics diagnostics)
                                        | EvalBlocked -> EvalBlocked
                                    | Some false -> tryCases rest
                                    | None -> EvalBlocked
                                | other -> other
                            | None ->
                                match evalExpression nextContext caseClause.Body with
                                | EvalSucceeded(value, bodyDiagnostics) -> success value (mergeDiagnostics scrutineeDiagnostics bodyDiagnostics)
                                | EvalFailed diagnostics -> EvalFailed(mergeDiagnostics scrutineeDiagnostics diagnostics)
                                | EvalBlocked -> EvalBlocked
                        | None -> tryCases rest
                tryCases cases
            | other -> other
        | RecordLiteral fields ->
            let rec collect
                (currentFields: MetaField list)
                currentDiagnostics
                (remaining: SurfaceRecordLiteralField list)
                =
                match remaining with
                | [] -> success (MetaRecord(List.rev currentFields)) currentDiagnostics
                | (field: SurfaceRecordLiteralField) :: rest ->
                    match evalExpression context field.Value with
                    | EvalSucceeded(value, diagnostics) -> collect ({ Name = field.Name; Value = value } :: currentFields) (mergeDiagnostics currentDiagnostics diagnostics) rest
                    | EvalFailed diagnostics -> EvalFailed(mergeDiagnostics currentDiagnostics diagnostics)
                    | EvalBlocked -> EvalBlocked
            collect [] [] fields
        | MemberAccess(receiver, segments, arguments) when List.isEmpty arguments ->
            match evalExpression context receiver with
            | EvalSucceeded(receiverValue, diagnostics) ->
                match tryResolveMetaFieldPath receiverValue segments with
                | Some value -> success value diagnostics
                | None -> EvalBlocked
            | other -> other
        | Binary(left, "&&", right) ->
            match evalExpression context left with
            | EvalSucceeded(leftValue, leftDiagnostics) ->
                match tryAsBool leftValue with
                | Some true ->
                    match evalExpression context right with
                    | EvalSucceeded(rightValue, rightDiagnostics) ->
                        match tryAsBool rightValue with
                        | Some rightBool -> success (MetaBool rightBool) (mergeDiagnostics leftDiagnostics rightDiagnostics)
                        | None -> EvalBlocked
                    | EvalFailed diagnostics -> EvalFailed(mergeDiagnostics leftDiagnostics diagnostics)
                    | EvalBlocked -> EvalBlocked
                | Some false -> success (MetaBool false) leftDiagnostics
                | None -> EvalBlocked
            | other -> other
        | Binary(left, "==", right) ->
            match evalExpression context left, evalExpression context right with
            | EvalSucceeded(leftValue, leftDiagnostics), EvalSucceeded(rightValue, rightDiagnostics) ->
                success (MetaBool(metaEquals leftValue rightValue)) (mergeDiagnostics leftDiagnostics rightDiagnostics)
            | EvalFailed diagnostics, _
            | _, EvalFailed diagnostics -> EvalFailed diagnostics
            | _ -> EvalBlocked
        | Binary(head, "::", tail) ->
            match evalExpression context head, evalExpression context tail with
            | EvalSucceeded(headValue, headDiagnostics), EvalSucceeded(MetaList tailValues, tailDiagnostics) ->
                success (MetaList(headValue :: tailValues)) (mergeDiagnostics headDiagnostics tailDiagnostics)
            | _ -> EvalBlocked
        | Apply(callee, arguments) ->
            match evalExpression context callee with
            | EvalSucceeded(calleeValue, calleeDiagnostics) ->
                let rec applyMany currentValue currentDiagnostics remainingArguments =
                    match remainingArguments with
                    | [] -> success currentValue currentDiagnostics
                    | argument :: rest ->
                        match evalExpression context argument with
                        | EvalSucceeded(argumentValue, argumentDiagnostics) ->
                            match applyMeta currentValue argumentValue (mergeDiagnostics currentDiagnostics argumentDiagnostics) with
                            | EvalSucceeded(nextValue, nextDiagnostics) -> applyMany nextValue nextDiagnostics rest
                            | other -> other
                        | EvalFailed diagnostics -> EvalFailed(mergeDiagnostics currentDiagnostics diagnostics)
                        | EvalBlocked -> EvalBlocked
                applyMany calleeValue calleeDiagnostics arguments
            | other -> other
        | Do statements -> evalDoBlock context statements
        | _ -> EvalBlocked

    and private evalDoBlock (context: RewriteContext) (statements: SurfaceDoStatement list) : EvalResult =
        let rec loop currentContext accumulatedDiagnostics remaining =
            match remaining with
            | [] -> EvalBlocked
            | DoReturn expression :: _ ->
                match evalExpression currentContext expression with
                | EvalSucceeded(value, diagnostics) -> EvalSucceeded(value, mergeDiagnostics accumulatedDiagnostics diagnostics)
                | EvalFailed diagnostics -> EvalFailed(mergeDiagnostics accumulatedDiagnostics diagnostics)
                | EvalBlocked -> EvalBlocked
            | DoExpression expression :: [] ->
                match evalExpression currentContext expression with
                | EvalSucceeded(value, diagnostics) -> EvalSucceeded(value, mergeDiagnostics accumulatedDiagnostics diagnostics)
                | EvalFailed diagnostics -> EvalFailed(mergeDiagnostics accumulatedDiagnostics diagnostics)
                | EvalBlocked -> EvalBlocked
            | DoExpression expression :: rest ->
                match evalExpression currentContext expression with
                | EvalSucceeded(_, diagnostics) -> loop currentContext (mergeDiagnostics accumulatedDiagnostics diagnostics) rest
                | EvalFailed diagnostics -> EvalFailed(mergeDiagnostics accumulatedDiagnostics diagnostics)
                | EvalBlocked -> EvalBlocked
            | DoBind(binding, expression) :: rest
            | DoLet(binding, expression) :: rest ->
                match binding.Pattern with
                | NamePattern name ->
                    match evalExpression currentContext expression with
                    | EvalSucceeded(value, diagnostics) ->
                        let nextContext = { currentContext with MetaBindings = Map.add name value currentContext.MetaBindings }
                        loop nextContext (mergeDiagnostics accumulatedDiagnostics diagnostics) rest
                    | EvalFailed diagnostics -> EvalFailed(mergeDiagnostics accumulatedDiagnostics diagnostics)
                    | EvalBlocked -> EvalBlocked
                | WildcardPattern ->
                    match evalExpression currentContext expression with
                    | EvalSucceeded(_, diagnostics) -> loop currentContext (mergeDiagnostics accumulatedDiagnostics diagnostics) rest
                    | EvalFailed diagnostics -> EvalFailed(mergeDiagnostics accumulatedDiagnostics diagnostics)
                    | EvalBlocked -> EvalBlocked
                | _ -> EvalBlocked
            | DoIf(condition, whenTrue, whenFalse) :: rest ->
                match evalExpression currentContext condition with
                | EvalSucceeded(conditionValue, conditionDiagnostics) ->
                    match tryAsBool conditionValue with
                    | Some branchValue ->
                        let branchStatements = if branchValue then whenTrue else whenFalse

                        match loop currentContext [] branchStatements with
                        | EvalSucceeded(_, branchDiagnostics) when not (List.isEmpty rest) ->
                            loop
                                currentContext
                                (mergeDiagnostics accumulatedDiagnostics (mergeDiagnostics conditionDiagnostics branchDiagnostics))
                                rest
                        | EvalSucceeded(branchResult, branchDiagnostics) ->
                            EvalSucceeded(
                                branchResult,
                                mergeDiagnostics accumulatedDiagnostics (mergeDiagnostics conditionDiagnostics branchDiagnostics)
                            )
                        | EvalFailed branchDiagnostics ->
                            EvalFailed(mergeDiagnostics accumulatedDiagnostics (mergeDiagnostics conditionDiagnostics branchDiagnostics))
                        | EvalBlocked ->
                            EvalBlocked
                    | None ->
                        EvalBlocked
                | EvalFailed conditionDiagnostics ->
                    EvalFailed(mergeDiagnostics accumulatedDiagnostics conditionDiagnostics)
                | EvalBlocked ->
                    EvalBlocked
            | _ -> EvalBlocked
        loop context [] statements

    and private applyBuiltin (context: RewriteContext) builtinName appliedArguments diagnostics : EvalResult =
        let builtinSuffix = normalizeBuiltinName builtinName
        let partial () = EvalSucceeded(MetaBuiltin(builtinName, appliedArguments), diagnostics)

        let rec renderSurfaceExpression expression =
            let renderStringPart = function
                | StringText text -> text
                | StringInterpolation(inner, None) -> "${" + renderSurfaceExpression inner + "}"
                | StringInterpolation(inner, Some format) -> "${" + renderSurfaceExpression inner + " : " + format + "}"

            let isTupleFields (fields: SurfaceRecordLiteralField list) =
                fields
                |> List.mapi (fun index field -> field.Name = $"_{index + 1}" && not field.IsImplicit)
                |> List.forall id

            match expression with
            | Literal LiteralValue.Unit -> "()"
            | Literal(LiteralValue.String text) -> "\"" + text + "\""
            | Literal(LiteralValue.Integer value) -> string value
            | Literal(LiteralValue.Float value) -> string value
            | Literal(LiteralValue.Byte value) -> $"b'\\x{int value:X2}'"
            | Literal(LiteralValue.Character text)
            | Literal(LiteralValue.Grapheme text) -> "'" + text + "'"
            | NumericLiteral literal -> SurfaceNumericLiteral.toSurfaceText literal
            | Name segments -> String.concat "." segments
            | KindQualifiedName(_, segments) -> String.concat "." segments
            | TypeSyntaxTokens tokens -> tokens |> List.map (fun token -> token.Text) |> String.concat " "
            | SyntaxQuote inner -> "'{ " + renderSurfaceExpression inner + " }"
            | SyntaxSplice inner -> "${" + renderSurfaceExpression inner + "}"
            | TopLevelSyntaxSplice inner -> "$(" + renderSurfaceExpression inner + ")"
            | CodeQuote inner -> ".< " + renderSurfaceExpression inner + " >."
            | CodeSplice inner -> ".~" + renderSurfaceExpression inner
            | Apply(callee, arguments) ->
                String.concat " " ([ renderSurfaceExpression callee ] @ (arguments |> List.map renderSurfaceExpression))
            | ExplicitImplicitArgument inner -> "@(" + renderSurfaceExpression inner + ")"
            | Unary(operatorName, inner) -> operatorName + renderSurfaceExpression inner
            | Binary(left, operatorName, right) -> renderSurfaceExpression left + " " + operatorName + " " + renderSurfaceExpression right
            | Elvis(left, right) -> renderSurfaceExpression left + " ?: " + renderSurfaceExpression right
            | RecordLiteral fields when isTupleFields fields ->
                "(" + (fields |> List.map (fun field -> renderSurfaceExpression field.Value) |> String.concat ", ") + ")"
            | RecordLiteral fields ->
                "("
                + (fields
                   |> List.map (fun field ->
                       let prefix = if field.IsImplicit then "@" else ""
                       prefix + field.Name + " = " + renderSurfaceExpression field.Value)
                   |> String.concat ", ")
                + ")"
            | MemberAccess(receiver, segments, arguments) ->
                let baseText = renderSurfaceExpression receiver + "." + String.concat "." segments
                if List.isEmpty arguments then baseText else baseText + " " + (arguments |> List.map renderSurfaceExpression |> String.concat " ")
            | PrefixedString(prefix, parts) -> prefix + "\"" + (parts |> List.map renderStringPart |> String.concat "") + "\""
            | _ -> "<syntax>"

        let diagnosticCodeFromText codeText =
            DiagnosticCode.tryParseIdentifier codeText |> Option.defaultValue DiagnosticCode.ElaborationFailed

        let failWith codeText message = EvalFailed [ makeMacroCodeError (diagnosticCodeFromText codeText) message context.Source ]

        let rec applyCallback callback arguments =
            let applyOne functionValue argumentValue callbackDiagnostics =
                match functionValue with
                | MetaClosure closure ->
                    match closure.Parameters with
                    | [] -> EvalBlocked
                    | parameter :: remainingParameters ->
                        let nextBindings = Map.add parameter.Name argumentValue closure.MetaBindings

                        if List.isEmpty remainingParameters then
                            let nextContext =
                                { context with
                                    CurrentModule = closure.ModuleIdentity
                                    MetaBindings = nextBindings }

                            match evalExpression nextContext closure.Body with
                            | EvalSucceeded(value, bodyDiagnostics) ->
                                match value with
                                | MetaClosure forcedClosure when List.isEmpty forcedClosure.Parameters ->
                                    let forcedContext =
                                        { context with
                                            CurrentModule = forcedClosure.ModuleIdentity
                                            MetaBindings = forcedClosure.MetaBindings }

                                    match evalExpression forcedContext forcedClosure.Body with
                                    | EvalSucceeded(forcedValue, forcedDiagnostics) ->
                                        EvalSucceeded(forcedValue, mergeDiagnostics (mergeDiagnostics callbackDiagnostics bodyDiagnostics) forcedDiagnostics)
                                    | EvalFailed forcedDiagnostics ->
                                        EvalFailed(mergeDiagnostics (mergeDiagnostics callbackDiagnostics bodyDiagnostics) forcedDiagnostics)
                                    | EvalBlocked ->
                                        EvalBlocked
                                | _ ->
                                    EvalSucceeded(value, mergeDiagnostics callbackDiagnostics bodyDiagnostics)
                            | EvalFailed bodyDiagnostics ->
                                EvalFailed(mergeDiagnostics callbackDiagnostics bodyDiagnostics)
                            | EvalBlocked ->
                                EvalBlocked
                        else
                            EvalSucceeded(MetaClosure { closure with Parameters = remainingParameters; MetaBindings = nextBindings }, callbackDiagnostics)
                | MetaBuiltin(name, builtinArguments) ->
                    applyBuiltin context name (builtinArguments @ [ argumentValue ]) callbackDiagnostics
                | _ ->
                    EvalBlocked

            match arguments with
            | [] -> EvalSucceeded(callback, diagnostics)
            | argument :: rest ->
                match applyOne callback argument diagnostics with
                | EvalSucceeded(value, _) -> applyCallback value rest
                | other -> other

        match builtinSuffix, appliedArguments with
        | "pure", [ value ] -> EvalSucceeded(value, diagnostics)
        | "failElab", [ MetaString message ] -> EvalFailed [ makeMacroError message context.Source ]
        | "warnElab", [ MetaString message ] -> EvalSucceeded(MetaUnit, mergeDiagnostics diagnostics [ makeMacroWarning message context.Source ])
        | "failElabWith", [ MetaString code; MetaString message; _ ] -> failWith code message
        | "warnElabWith", [ MetaString code; MetaString message; _ ] ->
            EvalSucceeded(MetaUnit, mergeDiagnostics diagnostics [ makeMacroCodeWarning (diagnosticCodeFromText code) message context.Source ])
        | "syntaxOrigin", [ MetaSyntax _ ] ->
            EvalSucceeded(MetaData(CompilerKnownSymbols.KnownTypeNames.SyntaxOrigin, []), diagnostics)
        | "renderSyntax", [ MetaSyntax syntaxValue ] ->
            EvalSucceeded(MetaString(renderSurfaceExpression syntaxValue.Expression), diagnostics)
        | "normalizeSyntax", [ MetaSyntax syntaxValue ] ->
            EvalSucceeded(MetaSyntax syntaxValue, diagnostics)
        | "stringSyntax", [ MetaString text ] ->
            EvalSucceeded(MetaSyntax(syntaxValueWithScope (Some context.CurrentModule) (Literal(LiteralValue.String text))), diagnostics)
        | "natSyntax", [ MetaNat value ] ->
            EvalSucceeded(MetaSyntax(syntaxValueWithScope (Some context.CurrentModule) (NumericLiteral(SurfaceIntegerLiteral(value, value.ToString(), None)))), diagnostics)
        | "boolSyntax", [ MetaBool value ] ->
            let expression = if value then Name [ "True" ] else Name [ "False" ]
            EvalSucceeded(MetaSyntax(syntaxValueWithScope (Some context.CurrentModule) expression), diagnostics)
        | "inspectAdt", [ MetaTypeSyntax typeSyntax; MetaSyntax targetSyntax ] ->
            let resolutionModule = targetSyntax.ScopeModule |> Option.defaultValue context.CurrentModule
            let effectiveTypeSyntax =
                match tryTypeSyntaxFromExpression targetSyntax.Expression with
                | Some resolvedTargetSyntax -> resolvedTargetSyntax
                | None -> typeSyntax

            match classifyAdtSyntax context.Models context.Inventories resolutionModule effectiveTypeSyntax with
            | Result.Ok shape -> EvalSucceeded(shape, diagnostics)
            | Result.Error code -> EvalFailed [ makeShapeDiagnostic code "ADT shape inspection failed." context.Source ]
        | "inspectRecord", [ MetaTypeSyntax typeSyntax; MetaSyntax targetSyntax ] ->
            let resolutionModule = targetSyntax.ScopeModule |> Option.defaultValue context.CurrentModule
            let effectiveTypeSyntax =
                match tryTypeSyntaxFromExpression targetSyntax.Expression with
                | Some resolvedTargetSyntax -> resolvedTargetSyntax
                | None -> typeSyntax

            match classifyRecordSyntax context.Models context.Inventories resolutionModule effectiveTypeSyntax with
            | Some(Result.Ok fields) -> EvalSucceeded(shapeRecordData fields, diagnostics)
            | Some(Result.Error code) -> EvalFailed [ makeShapeDiagnostic code "Record shape inspection failed." context.Source ]
            | None -> EvalFailed [ makeShapeDiagnostic DiagnosticCode.DerivingShapeNotClosedRecord "Record shape inspection failed." context.Source ]
        | "requireRuntimeFieldInstances", [ MetaTraitRef(_, traitName); MetaData(shapeTag, shapeFields) ]
            when String.Equals(shapeTag, CompilerKnownSymbols.KnownTypeNames.AdtShape, StringComparison.Ordinal) ->
            match tryGetField "constructors" shapeFields with
            | Some(MetaList constructors) ->
                let visibleInstances = visibleInstancesForModule context.Models context.Inventories context.CurrentModule
                let instanceMatches fieldType =
                    visibleInstances
                    |> List.exists (fun declaration ->
                        match tryParseInstanceHeader declaration with
                        | Some(candidateTrait, [ candidateType ]) ->
                            let fieldTypeText =
                                match fieldType with
                                | MetaTypeSyntax typeSyntax ->
                                    typeSyntax |> tryParseTypeSyntaxValue |> Option.map TypeSignatures.toText |> Option.defaultValue "<unknown>"
                                | _ ->
                                    "<unknown>"

                            candidateTrait = traitName
                            && TypeSignatures.toText candidateType = fieldTypeText
                        | _ -> false)
                let missingField =
                    constructors
                    |> List.tryPick (fun constructor ->
                        match tryGetDataField "fields" constructor with
                        | Some(MetaList fields) ->
                            fields
                            |> List.tryFind (fun field ->
                                let runtimeRelevant =
                                    tryGetDataField "runtimeRelevant" field |> Option.bind tryAsBool |> Option.defaultValue true
                                let fieldType = tryGetDataField "fieldType" field
                                runtimeRelevant && not (fieldType |> Option.exists instanceMatches))
                        | _ -> None)
                match missingField with
                | Some _ -> EvalFailed [ makeShapeDiagnostic DiagnosticCode.DerivingShapeMissingRuntimeFieldInstance "Missing runtime field instance." context.Source ]
                | None -> EvalSucceeded(MetaUnit, diagnostics)
            | _ -> EvalBlocked
        | "matchAdt", [ MetaData(shapeTag, shapeFields); MetaSyntax scrutineeSyntax; callback ]
            when String.Equals(shapeTag, CompilerKnownSymbols.KnownTypeNames.AdtShape, StringComparison.Ordinal) ->
            match tryGetField "constructors" shapeFields with
            | Some(MetaList constructors) ->
                let buildCase index constructorValue : SurfaceMatchCase option =
                    match constructorValue with
                    | MetaData(constructorTag, constructorFields)
                        when String.Equals(constructorTag, CompilerKnownSymbols.KnownTypeNames.ShapeConstructor, StringComparison.Ordinal) ->
                        let constructorName = tryGetField "renderName" constructorFields |> Option.bind tryAsString |> Option.defaultValue $"Ctor{index}"
                        let fieldValues = tryGetField "fields" constructorFields |> Option.bind tryAsMetaList |> Option.defaultValue []
                        let binderNames = fieldValues |> List.mapi (fun fieldIndex _ -> $"__kappa_macro_{constructorName}_{fieldIndex}")
                        let boundFields =
                            (fieldValues, binderNames)
                            ||> List.zip
                            |> List.map (fun (fieldValue, binderName) ->
                                MetaData(
                                    CompilerKnownSymbols.KnownTypeNames.BoundField,
                                    [ valueField "field" fieldValue
                                      valueField "fieldType" MetaUnit
                                      valueField "term" (MetaSyntax(syntaxValueWithScope scrutineeSyntax.ScopeModule (Name [ binderName ]))) ]))
                        match applyCallback callback [ constructorValue; MetaList boundFields ] with
                        | EvalSucceeded(MetaSyntax bodySyntax, _) ->
                            Some
                                { Pattern = ConstructorPattern([ constructorName ], binderNames |> List.map NamePattern)
                                  Guard = None
                                  Body = bodySyntax.Expression }
                        | _ -> None
                    | _ -> None
                let cases = constructors |> List.mapi buildCase
                if cases |> List.forall Option.isSome then
                    EvalSucceeded(
                        MetaSyntax(
                            syntaxValueWithScope
                                scrutineeSyntax.ScopeModule
                                (Match(scrutineeSyntax.Expression, cases |> List.choose id))
                        ),
                        diagnostics
                    )
                else
                    EvalBlocked
            | _ -> EvalBlocked
        | "matchAdt2", [ MetaData(shapeTag, shapeFields); MetaSyntax leftSyntax; MetaSyntax rightSyntax; onSame; onDifferent ]
            when String.Equals(shapeTag, CompilerKnownSymbols.KnownTypeNames.AdtShape, StringComparison.Ordinal) ->
            match tryGetField "constructors" shapeFields with
            | Some(MetaList constructors) ->
                let buildCase leftCtor rightCtor : SurfaceMatchCase option =
                    match leftCtor, rightCtor with
                    | MetaData(leftTag, leftFields), MetaData(rightTag, rightFields)
                        when String.Equals(leftTag, CompilerKnownSymbols.KnownTypeNames.ShapeConstructor, StringComparison.Ordinal)
                             && String.Equals(rightTag, CompilerKnownSymbols.KnownTypeNames.ShapeConstructor, StringComparison.Ordinal) ->
                        let leftName = tryGetField "renderName" leftFields |> Option.bind tryAsString |> Option.defaultValue "Left"
                        let rightName = tryGetField "renderName" rightFields |> Option.bind tryAsString |> Option.defaultValue "Right"
                        let bodyResult =
                            if leftName = rightName then
                                let shapeFields = tryGetField "fields" leftFields |> Option.bind tryAsMetaList |> Option.defaultValue []
                                let binderPairs =
                                    shapeFields
                                    |> List.mapi (fun index field ->
                                        let leftBinder = $"__kappa_macro_left_{leftName}_{index}"
                                        let rightBinder = $"__kappa_macro_right_{rightName}_{index}"
                                        MetaData(
                                            CompilerKnownSymbols.KnownTypeNames.BoundFieldPair,
                                            [ valueField "field" field
                                              valueField "fieldType" MetaUnit
                                              valueField "left" (MetaSyntax(syntaxValueWithScope leftSyntax.ScopeModule (Name [ leftBinder ])))
                                              valueField "right" (MetaSyntax(syntaxValueWithScope rightSyntax.ScopeModule (Name [ rightBinder ]))) ]))
                                applyCallback onSame [ leftCtor; MetaList binderPairs ]
                            else
                                applyCallback onDifferent [ leftCtor; rightCtor ]
                        match bodyResult with
                        | EvalSucceeded(MetaSyntax bodySyntax, _) ->
                            let leftPattern : SurfacePattern =
                                let fieldCount = tryGetField "fields" leftFields |> Option.bind tryAsMetaList |> Option.map List.length |> Option.defaultValue 0
                                ConstructorPattern([ leftName ], [ for index in 0 .. fieldCount - 1 -> NamePattern $"__kappa_macro_left_{leftName}_{index}" ])
                            let rightPattern : SurfacePattern =
                                let fieldCount = tryGetField "fields" rightFields |> Option.bind tryAsMetaList |> Option.map List.length |> Option.defaultValue 0
                                ConstructorPattern([ rightName ], [ for index in 0 .. fieldCount - 1 -> NamePattern $"__kappa_macro_right_{rightName}_{index}" ])
                            Some { Pattern = TuplePattern [ leftPattern; rightPattern ]; Guard = None; Body = bodySyntax.Expression }
                        | _ -> None
                    | _ -> None
                let cases = [ for leftCtor in constructors do for rightCtor in constructors do buildCase leftCtor rightCtor ]
                if cases |> List.forall Option.isSome then
                    let tupleRecordFields : SurfaceRecordLiteralField list =
                        [ { Name = "_1"; IsImplicit = false; Value = leftSyntax.Expression }
                          { Name = "_2"; IsImplicit = false; Value = rightSyntax.Expression } ]

                    EvalSucceeded(
                        MetaSyntax(
                            syntaxValueWithScope
                                (combineSyntaxScopeModules [ leftSyntax.ScopeModule; rightSyntax.ScopeModule ])
                                (Match(RecordLiteral tupleRecordFields, cases |> List.choose id))
                        ),
                        diagnostics
                    )
                else
                    EvalBlocked
            | _ -> EvalBlocked
        | _ -> partial ()

    let private renameBinderName (counter: int) (name: string) = $"__kappa_hyg_{counter}_{name}"

    let private alphaRenameExpression expression =
        let rec renamePattern counter env (pattern: SurfacePattern) : SurfacePattern * Map<string, string> * int =
            match pattern with
            | WildcardPattern
            | LiteralPattern _
            | VariantPattern _ -> pattern, env, counter
            | NamePattern name ->
                let renamed = renameBinderName counter name
                NamePattern renamed, Map.add name renamed env, counter + 1
            | AsPattern(name, inner) ->
                let renamed = renameBinderName counter name
                let innerPattern, nextEnv, nextCounter = renamePattern (counter + 1) env inner
                AsPattern(renamed, innerPattern), Map.add name renamed nextEnv, nextCounter
            | TypedPattern(inner, tokens) ->
                let innerPattern, nextEnv, nextCounter = renamePattern counter env inner
                TypedPattern(innerPattern, tokens), nextEnv, nextCounter
            | ConstructorPattern(name, patterns) ->
                let rewrittenPatterns, nextEnv, nextCounter = renamePatterns counter env patterns
                ConstructorPattern(name, rewrittenPatterns), nextEnv, nextCounter
            | NamedConstructorPattern(name, fields) ->
                let rec renameFields
                    currentCounter
                    currentEnv
                    (currentFields: SurfaceRecordPatternField list)
                    (remaining: SurfaceRecordPatternField list)
                    =
                    match remaining with
                    | [] -> List.rev currentFields, currentEnv, currentCounter
                    | (field: SurfaceRecordPatternField) :: rest ->
                        let rewrittenPattern, nextEnv, nextCounter = renamePattern currentCounter currentEnv field.Pattern
                        renameFields nextCounter nextEnv ({ field with Pattern = rewrittenPattern } :: currentFields) rest
                let rewrittenFields, nextEnv, nextCounter = renameFields counter env [] fields
                NamedConstructorPattern(name, rewrittenFields), nextEnv, nextCounter
            | TuplePattern patterns ->
                let rewrittenPatterns, nextEnv, nextCounter = renamePatterns counter env patterns
                TuplePattern rewrittenPatterns, nextEnv, nextCounter
            | OrPattern patterns ->
                let rewrittenPatterns, nextEnv, nextCounter = renamePatterns counter env patterns
                OrPattern rewrittenPatterns, nextEnv, nextCounter
            | AnonymousRecordPattern(fields, rest) ->
                let rec renameFields
                    currentCounter
                    currentEnv
                    (currentFields: SurfaceRecordPatternField list)
                    (remaining: SurfaceRecordPatternField list)
                    =
                    match remaining with
                    | [] -> List.rev currentFields, currentEnv, currentCounter
                    | (field: SurfaceRecordPatternField) :: restFields ->
                        let rewrittenPattern, nextEnv, nextCounter = renamePattern currentCounter currentEnv field.Pattern
                        renameFields nextCounter nextEnv ({ field with Pattern = rewrittenPattern } :: currentFields) restFields
                let rewrittenFields, nextEnv, nextCounter = renameFields counter env [] fields
                match rest with
                | Some(BindRecordPatternRest name) ->
                    let renamed = renameBinderName nextCounter name
                    AnonymousRecordPattern(rewrittenFields, Some(BindRecordPatternRest renamed)), Map.add name renamed nextEnv, nextCounter + 1
                | _ -> AnonymousRecordPattern(rewrittenFields, rest), nextEnv, nextCounter
        and renamePatterns counter env (patterns: SurfacePattern list) =
            let rec loop currentCounter currentEnv currentPatterns remaining =
                match remaining with
                | [] -> List.rev currentPatterns, currentEnv, currentCounter
                | pattern :: rest ->
                    let rewrittenPattern, nextEnv, nextCounter = renamePattern currentCounter currentEnv pattern
                    loop nextCounter nextEnv (rewrittenPattern :: currentPatterns) rest
            loop counter env [] patterns
        and renameBindPattern counter env (binding: SurfaceBindPattern) =
            let rewrittenPattern, nextEnv, nextCounter = renamePattern counter env binding.Pattern
            let rewrittenSpans =
                binding.BinderSpans
                |> Map.toList
                |> List.choose (fun (name, spans) -> nextEnv |> Map.tryFind name |> Option.map (fun renamed -> renamed, spans))
                |> Map.ofList
            { binding with Pattern = rewrittenPattern; BinderSpans = rewrittenSpans }, nextEnv, nextCounter
        and renameParameters counter env (parameters: Parameter list) =
            let rec loop
                currentCounter
                currentEnv
                (currentParameters: Parameter list)
                (remaining: Parameter list)
                =
                match remaining with
                | [] -> List.rev currentParameters, currentEnv, currentCounter
                | parameter :: rest ->
                    let renamed = renameBinderName currentCounter parameter.Name
                    loop (currentCounter + 1) (Map.add parameter.Name renamed currentEnv) ({ parameter with Name = renamed } :: currentParameters) rest
            loop counter env [] parameters
        and renameDoStatements counter env statements =
            let rec loop currentCounter currentEnv currentStatements remaining =
                match remaining with
                | [] -> List.rev currentStatements, currentCounter
                | statement :: rest ->
                    let rewrittenStatement, nextEnv, nextCounter =
                        match statement with
                        | DoReturn expression -> DoReturn(renameExpression currentCounter currentEnv expression), currentEnv, currentCounter
                        | DoExpression expression -> DoExpression(renameExpression currentCounter currentEnv expression), currentEnv, currentCounter
                        | DoBind(binding, value) ->
                            let rewrittenValue = renameExpression currentCounter currentEnv value
                            let rewrittenBinding, nextEnv, nextCounter = renameBindPattern currentCounter currentEnv binding
                            DoBind(rewrittenBinding, rewrittenValue), nextEnv, nextCounter
                        | DoLet(binding, value) ->
                            let rewrittenValue = renameExpression currentCounter currentEnv value
                            let rewrittenBinding, nextEnv, nextCounter = renameBindPattern currentCounter currentEnv binding
                            DoLet(rewrittenBinding, rewrittenValue), nextEnv, nextCounter
                        | _ -> statement, currentEnv, currentCounter
                    loop nextCounter nextEnv (rewrittenStatement :: currentStatements) rest
            loop counter env [] statements
        and renameExpression counter env current =
            let recurse = renameExpression counter env
            match current with
            | Name [ name ] -> env |> Map.tryFind name |> Option.map (fun renamed -> Name [ renamed ]) |> Option.defaultValue current
            | SyntaxQuote inner -> SyntaxQuote(renameExpression counter env inner)
            | SyntaxSplice inner -> SyntaxSplice(renameExpression counter env inner)
            | TopLevelSyntaxSplice inner -> TopLevelSyntaxSplice(renameExpression counter env inner)
            | CodeQuote inner -> CodeQuote(renameExpression counter env inner)
            | CodeSplice inner -> CodeSplice(renameExpression counter env inner)
            | ExplicitImplicitArgument inner -> ExplicitImplicitArgument(renameExpression counter env inner)
            | LocalLet(binding, value, body) ->
                let rewrittenValue = renameExpression counter env value
                let rewrittenBinding, nextEnv, nextCounter = renameBindPattern counter env binding
                let rewrittenBody = renameExpression nextCounter nextEnv body
                LocalLet(rewrittenBinding, rewrittenValue, rewrittenBody)
            | Lambda(parameters, body) ->
                let rewrittenParameters, nextEnv, nextCounter = renameParameters counter env parameters
                Lambda(rewrittenParameters, renameExpression nextCounter nextEnv body)
            | IfThenElse(condition, whenTrue, whenFalse) -> IfThenElse(recurse condition, recurse whenTrue, recurse whenFalse)
            | Match(scrutinee, cases) ->
                let rewrittenCases =
                    cases
                    |> List.map (fun (caseClause: SurfaceMatchCase) ->
                        let rewrittenPattern, nextEnv, nextCounter = renamePattern counter env caseClause.Pattern
                        { caseClause with
                            Pattern = rewrittenPattern
                            Guard = caseClause.Guard |> Option.map (renameExpression nextCounter nextEnv)
                            Body = renameExpression nextCounter nextEnv caseClause.Body })
                Match(recurse scrutinee, rewrittenCases)
            | RecordLiteral fields -> RecordLiteral(fields |> List.map (fun field -> { field with Value = recurse field.Value }))
            | MemberAccess(receiver, segments, arguments) -> MemberAccess(recurse receiver, segments, arguments |> List.map recurse)
            | Apply(callee, arguments) -> Apply(recurse callee, arguments |> List.map recurse)
            | Unary(operatorName, inner) -> Unary(operatorName, recurse inner)
            | Binary(left, operatorName, right) -> Binary(recurse left, operatorName, recurse right)
            | Do statements ->
                let rewrittenStatements, _ = renameDoStatements counter env statements
                Do rewrittenStatements
            | _ -> current
        renameExpression 0 Map.empty expression

    let private rewriteSplicesInExpression context expression =
        let rec rewrite currentContext currentExpression =
            let recurse = rewrite currentContext
            match currentExpression with
            | TopLevelSyntaxSplice inner ->
                match evalExpression currentContext inner with
                | EvalSucceeded(MetaSyntax syntaxValue, diagnostics) when Set.isSubset syntaxValue.CapturedNames currentContext.ObjectBinders ->
                    { Expression = alphaRenameExpression syntaxValue.Expression
                      Diagnostics = diagnostics }
                | EvalSucceeded(MetaSyntax syntaxValue, diagnostics) ->
                    { Expression = Literal LiteralValue.Unit
                      Diagnostics =
                        diagnostics
                        @ [ makeDiagnostic
                                Error
                                (DiagnosticFact.macroExpansion
                                    (SyntaxValueEscapesCapturedBindings(Set.toList syntaxValue.CapturedNames)))
                                "macro-expansion"
                                (KFrontIRPhase.phaseName BODY_RESOLVE)
                                currentContext.Source ] }
                | EvalFailed diagnostics -> { Expression = Literal LiteralValue.Unit; Diagnostics = diagnostics }
                | _ -> { Expression = TopLevelSyntaxSplice inner; Diagnostics = [] }
            | LocalLet(binding, value, body) ->
                let rewrittenValue = recurse value
                let bindingNames = collectPatternBoundNames binding.Pattern
                let rewrittenBody = rewrite { currentContext with ObjectBinders = Set.union currentContext.ObjectBinders bindingNames } body
                { Expression = LocalLet(binding, rewrittenValue.Expression, rewrittenBody.Expression)
                  Diagnostics = mergeDiagnostics rewrittenValue.Diagnostics rewrittenBody.Diagnostics }
            | Lambda(parameters, body) ->
                let parameterNames = parameters |> List.map (fun parameter -> parameter.Name) |> Set.ofList
                let rewrittenBody = rewrite { currentContext with ObjectBinders = Set.union currentContext.ObjectBinders parameterNames } body
                { Expression = Lambda(parameters, rewrittenBody.Expression); Diagnostics = rewrittenBody.Diagnostics }
            | IfThenElse(condition, whenTrue, whenFalse) ->
                let rewrittenCondition = recurse condition
                let rewrittenTrue = recurse whenTrue
                let rewrittenFalse = recurse whenFalse
                { Expression = IfThenElse(rewrittenCondition.Expression, rewrittenTrue.Expression, rewrittenFalse.Expression)
                  Diagnostics = mergeDiagnostics rewrittenCondition.Diagnostics (mergeDiagnostics rewrittenTrue.Diagnostics rewrittenFalse.Diagnostics) }
            | Match(scrutinee, cases) ->
                let rewrittenScrutinee = recurse scrutinee
                let rewrittenCases =
                    cases
                    |> List.map (fun caseClause ->
                        let caseBinders = collectPatternBoundNames caseClause.Pattern
                        let caseContext = { currentContext with ObjectBinders = Set.union currentContext.ObjectBinders caseBinders }
                        let rewrittenGuard = caseClause.Guard |> Option.map (rewrite caseContext)
                        let rewrittenBody = rewrite caseContext caseClause.Body
                        { caseClause with Guard = rewrittenGuard |> Option.map (fun guard -> guard.Expression); Body = rewrittenBody.Expression },
                        ((rewrittenGuard |> Option.map (fun guard -> guard.Diagnostics) |> Option.defaultValue []) @ rewrittenBody.Diagnostics))
                { Expression = Match(rewrittenScrutinee.Expression, rewrittenCases |> List.map fst)
                  Diagnostics = mergeDiagnostics rewrittenScrutinee.Diagnostics (rewrittenCases |> List.collect snd) }
            | Apply(callee, arguments) ->
                let rewrittenCallee = recurse callee
                let rewrittenArguments = arguments |> List.map recurse
                { Expression = Apply(rewrittenCallee.Expression, rewrittenArguments |> List.map (fun argument -> argument.Expression))
                  Diagnostics = mergeDiagnostics rewrittenCallee.Diagnostics (rewrittenArguments |> List.collect (fun argument -> argument.Diagnostics)) }
            | SyntaxQuote inner ->
                let rewritten = recurse inner
                { Expression = SyntaxQuote rewritten.Expression; Diagnostics = rewritten.Diagnostics }
            | SyntaxSplice inner ->
                let rewritten = recurse inner
                { Expression = SyntaxSplice rewritten.Expression; Diagnostics = rewritten.Diagnostics }
            | CodeQuote inner ->
                let rewritten = recurse inner
                { Expression = CodeQuote rewritten.Expression; Diagnostics = rewritten.Diagnostics }
            | CodeSplice inner ->
                let rewritten = recurse inner
                { Expression = CodeSplice rewritten.Expression; Diagnostics = rewritten.Diagnostics }
            | ExplicitImplicitArgument inner ->
                let rewritten = recurse inner
                { Expression = ExplicitImplicitArgument rewritten.Expression; Diagnostics = rewritten.Diagnostics }
            | Unary(operatorName, inner) ->
                let rewritten = recurse inner
                { Expression = Unary(operatorName, rewritten.Expression); Diagnostics = rewritten.Diagnostics }
            | Binary(left, operatorName, right) ->
                let rewrittenLeft = recurse left
                let rewrittenRight = recurse right
                { Expression = Binary(rewrittenLeft.Expression, operatorName, rewrittenRight.Expression)
                  Diagnostics = mergeDiagnostics rewrittenLeft.Diagnostics rewrittenRight.Diagnostics }
            | RecordLiteral fields ->
                let rewrittenFields = fields |> List.map (fun field -> let rewritten = recurse field.Value in { field with Value = rewritten.Expression }, rewritten.Diagnostics)
                { Expression = RecordLiteral(rewrittenFields |> List.map fst)
                  Diagnostics = rewrittenFields |> List.collect snd }
            | MemberAccess(receiver, segments, arguments) ->
                let rewrittenReceiver = recurse receiver
                let rewrittenArguments = arguments |> List.map recurse
                { Expression = MemberAccess(rewrittenReceiver.Expression, segments, rewrittenArguments |> List.map (fun argument -> argument.Expression))
                  Diagnostics = mergeDiagnostics rewrittenReceiver.Diagnostics (rewrittenArguments |> List.collect (fun argument -> argument.Diagnostics)) }
            | Do statements ->
                let rec rewriteDoStatements doContext remaining =
                    match remaining with
                    | [] -> [], []
                    | statement :: rest ->
                        let rewrittenStatement, nextContext, statementDiagnostics =
                            match statement with
                            | DoReturn expression -> let rewritten = rewrite doContext expression in DoReturn rewritten.Expression, doContext, rewritten.Diagnostics
                            | DoExpression expression -> let rewritten = rewrite doContext expression in DoExpression rewritten.Expression, doContext, rewritten.Diagnostics
                            | DoBind(binding, expression) ->
                                let rewritten = rewrite doContext expression
                                let binders = collectPatternBoundNames binding.Pattern
                                DoBind(binding, rewritten.Expression), { doContext with ObjectBinders = Set.union doContext.ObjectBinders binders }, rewritten.Diagnostics
                            | DoLet(binding, expression) ->
                                let rewritten = rewrite doContext expression
                                let binders = collectPatternBoundNames binding.Pattern
                                DoLet(binding, rewritten.Expression), { doContext with ObjectBinders = Set.union doContext.ObjectBinders binders }, rewritten.Diagnostics
                            | _ -> statement, doContext, []
                        let rewrittenRest, restDiagnostics = rewriteDoStatements nextContext rest
                        rewrittenStatement :: rewrittenRest, mergeDiagnostics statementDiagnostics restDiagnostics
                let rewrittenStatements, diagnostics = rewriteDoStatements currentContext statements
                { Expression = Do rewrittenStatements; Diagnostics = diagnostics }
            | _ -> { Expression = currentExpression; Diagnostics = [] }
        rewrite context expression

    let private rewriteDeclaration context declaration =
        match declaration with
        | LetDeclaration definition ->
            match definition.Body with
            | Some body ->
                let rewritten = rewriteSplicesInExpression context body
                LetDeclaration { definition with Body = Some rewritten.Expression }, rewritten.Diagnostics
            | None -> declaration, []
        | _ -> declaration, []

    let expandTopLevelSplices (documents: ParsedDocument list) =
        let models = buildModuleModels documents
        let inventories = buildInventories documents

        documents
        |> List.map (fun document ->
            match document.ModuleIdentity with
            | None -> document
            | Some moduleIdentity ->
                let context =
                    { Models = models
                      Inventories = inventories
                      CurrentModule = moduleIdentity
                      Source = document.Source
                      ObjectBinders = Set.empty
                      MetaBindings = Map.empty }
                let rewrittenDeclarations, diagnostics =
                    document.Syntax.Declarations
                    |> List.map (rewriteDeclaration context)
                    |> List.unzip
                { document with
                    Syntax = { document.Syntax with Declarations = rewrittenDeclarations }
                    Diagnostics = document.Diagnostics @ (diagnostics |> List.collect id) })
