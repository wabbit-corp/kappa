namespace Kappa.Compiler

open System
open System.Collections.Generic
open System.IO
open System.Reflection
open System.Reflection.Emit
open System.Runtime.CompilerServices

// Builds and validates the CLR assembly input environment used by IL emission.
module internal IlDotNetBackendInput =
    open IlDotNetBackendModel

    let internal significantTokens (tokens: Token list) =
        SignatureTokenAnalysis.significantTokens tokens

    let internal splitTopLevelArrows (tokens: Token list) =
        SignatureTokenAnalysis.splitTopLevelArrows tokens

    let internal collectParenthesizedTokens (tokens: Token list) =
        let rec loop depth current remaining =
            match remaining with
            | [] ->
                Result.Error "Expected ')' to close the type."
            | token :: tail when token.Kind = LeftParen ->
                loop (depth + 1) (token :: current) tail
            | token :: tail when token.Kind = RightParen && depth = 1 ->
                Result.Ok(List.rev current, tail)
            | token :: tail when token.Kind = RightParen ->
                loop (depth - 1) (token :: current) tail
            | token :: tail ->
                loop depth (token :: current) tail

        match tokens with
        | head :: tail when head.Kind = LeftParen ->
            loop 1 [] tail
        | _ ->
            Result.Error "Expected '(' to start a parenthesized type."

    let internal typeTextTokens (text: string) =
        let rec loop index tokens =
            if index >= text.Length then
                List.rev tokens
            else
                match text[index] with
                | ch when Char.IsWhiteSpace(ch) ->
                    loop (index + 1) tokens
                | '(' ->
                    let token =
                        { Kind = LeftParen
                          Text = "("
                          Span = TextSpan.FromBounds(index, index + 1) }

                    loop (index + 1) (token :: tokens)
                | ')' ->
                    let token =
                        { Kind = RightParen
                          Text = ")"
                          Span = TextSpan.FromBounds(index, index + 1) }

                    loop (index + 1) (token :: tokens)
                | '.' ->
                    let token =
                        { Kind = Dot
                          Text = "."
                          Span = TextSpan.FromBounds(index, index + 1) }

                    loop (index + 1) (token :: tokens)
                | '-' when index + 1 < text.Length && text[index + 1] = '>' ->
                    let token =
                        { Kind = Arrow
                          Text = "->"
                          Span = TextSpan.FromBounds(index, index + 2) }

                    loop (index + 2) (token :: tokens)
                | ch when SyntaxFacts.isIdentifierStart ch ->
                    let mutable endIndex = index + 1

                    while endIndex < text.Length && SyntaxFacts.isIdentifierPart text[endIndex] do
                        endIndex <- endIndex + 1

                    let token =
                        { Kind = Identifier
                          Text = text.Substring(index, endIndex - index)
                          Span = TextSpan.FromBounds(index, endIndex) }

                    loop endIndex (token :: tokens)
                | ch ->
                    let token =
                        { Kind = BadToken
                          Text = string ch
                          Span = TextSpan.FromBounds(index, index + 1) }

                    loop (index + 1) (token :: tokens)

        loop 0 []

    let private hashModuleIdentity = ModuleIdentity.ofSegments CompilerKnownSymbols.KnownModules.Hash
    let internal tryParsePrimitiveTypeIdentity typeIdentity =
        if TypeIdentity.hasTopLevelName preludeModuleIdentity Stdlib.KnownTypeNames.Int typeIdentity
           || TypeIdentity.hasTopLevelName preludeModuleIdentity Stdlib.KnownTypeNames.Nat typeIdentity
           || TypeIdentity.hasTopLevelName preludeModuleIdentity Stdlib.KnownTypeNames.Integer typeIdentity
           || TypeIdentity.hasTopLevelName preludeModuleIdentity Stdlib.KnownTypeNames.Byte typeIdentity
           || TypeIdentity.hasTopLevelName hashModuleIdentity Stdlib.KnownTypeNames.HashCode typeIdentity then
            Some(IlPrimitive IlInt64)
        elif TypeIdentity.hasTopLevelName preludeModuleIdentity Stdlib.KnownTypeNames.Float typeIdentity
             || TypeIdentity.hasTopLevelName preludeModuleIdentity Stdlib.KnownTypeNames.Double typeIdentity
             || TypeIdentity.hasTopLevelName preludeModuleIdentity Stdlib.KnownTypeNames.Real typeIdentity then
            Some(IlPrimitive IlFloat64)
        elif TypeIdentity.hasTopLevelName preludeModuleIdentity Stdlib.KnownTypeNames.Bool typeIdentity then
            Some(IlPrimitive IlBool)
        elif TypeIdentity.hasTopLevelName preludeModuleIdentity Stdlib.KnownTypeNames.String typeIdentity then
            Some(IlPrimitive IlString)
        elif TypeIdentity.hasTopLevelName preludeModuleIdentity Stdlib.KnownTypeNames.Char typeIdentity then
            Some(IlPrimitive IlChar)
        elif TypeIdentity.hasTopLevelName preludeModuleIdentity Stdlib.KnownTypeNames.UnicodeScalar typeIdentity
             || TypeIdentity.hasTopLevelName preludeModuleIdentity Stdlib.KnownTypeNames.Grapheme typeIdentity
             || TypeIdentity.hasTopLevelName preludeModuleIdentity Stdlib.KnownTypeNames.Bytes typeIdentity then
            Some(IlPrimitive IlString)
        else
            None

    let private bundledTypeExports moduleName =
        let bundledPreludeExpectTypes =
            if String.Equals(moduleName, Stdlib.PreludeModuleText, StringComparison.Ordinal) then
                (IntrinsicCatalog.bundledPreludeExpectContract ()).TypeNames
            else
                Set.empty

        let bundledStandardTypes =
            StandardLibraryCatalog.tryTypeNamesText moduleName |> Option.defaultValue Set.empty

        Set.union bundledPreludeExpectTypes bundledStandardTypes

    let private tryResolveBundledBuiltinTypeIdentity name =
        let candidates =
            let preludeCandidates =
                let preludeTypes = (IntrinsicCatalog.bundledPreludeExpectContract ()).TypeNames

                if Set.contains name preludeTypes then
                    [ TypeIdentity.topLevel preludeModuleIdentity name ]
                else
                    []

            let bundledStandardCandidates =
                StandardLibraryCatalog.all
                |> List.choose (fun moduleInfo ->
                    let surfaceInfo = StandardLibraryCatalog.surface moduleInfo

                    if surfaceInfo.Types |> List.exists (fun typeName -> String.Equals(typeName, name, StringComparison.Ordinal)) then
                        Some(TypeIdentity.topLevel surfaceInfo.ModuleIdentity name)
                    else
                        None)

            preludeCandidates @ bundledStandardCandidates

        match candidates |> List.distinct with
        | [ resolved ] -> Some resolved
        | _ -> None

    let internal itemImportsTypeName (item: ImportItem) =
        item.Namespace.IsNone || item.Namespace = Some ImportNamespace.Type

    let internal itemImportsTermName (item: ImportItem) =
        item.Namespace.IsNone || item.Namespace = Some ImportNamespace.Term

    let internal itemImportsConstructorName (item: ImportItem) =
        item.Namespace = Some ImportNamespace.Constructor

    let internal itemImportsConstructorsOfType typeName (item: ImportItem) =
        item.IncludeConstructors
        && item.Namespace = Some ImportNamespace.Type
        && String.Equals(item.Name, typeName, StringComparison.Ordinal)

    let internal exceptMatches namespaceName name (item: ExceptItem) =
        String.Equals(item.Name, name, StringComparison.Ordinal)
        && (item.Namespace.IsNone || item.Namespace = Some namespaceName)

    let internal selectionImportsTypeName selection name =
        match selection with
        | QualifiedOnly ->
            false
        | Items items ->
            items
            |> List.exists (fun item ->
                String.Equals(item.Name, name, StringComparison.Ordinal)
                && itemImportsTypeName item)
        | All ->
            true
        | AllExcept excludedItems ->
            not (excludedItems |> List.exists (exceptMatches ImportNamespace.Type name))

    let internal selectionImportsTermName selection name =
        match selection with
        | QualifiedOnly ->
            false
        | Items items ->
            items
            |> List.exists (fun item ->
                String.Equals(item.Name, name, StringComparison.Ordinal)
                && itemImportsTermName item)
        | All ->
            true
        | AllExcept excludedItems ->
            not (excludedItems |> List.exists (exceptMatches ImportNamespace.Term name))

    let internal selectionImportsConstructorName selection name constructorTypeName =
        match selection with
        | QualifiedOnly ->
            false
        | Items items ->
            items
            |> List.exists (fun item ->
                ((String.Equals(item.Name, name, StringComparison.Ordinal)
                  && itemImportsConstructorName item)
                 || itemImportsConstructorsOfType constructorTypeName item))
        | All
        | AllExcept _ ->
            false

    let internal buildRawDataTypes (modules: ClrAssemblyModule list) =
        modules
        |> List.map (fun moduleDump ->
            let moduleIdentity = ModuleIdentity.ofDottedTextUnchecked moduleDump.Name

            let dataTypes =
                moduleDump.DataTypes
                |> List.map (fun dataType ->
                    let dataTypeIdentity = TypeIdentity.topLevel moduleIdentity dataType.Name

                    let constructors =
                        dataType.Constructors
                        |> List.map (fun constructor ->
                            { Identity =
                                DeclarationIdentity.topLevel moduleIdentity constructor.Name ConstructorDeclaration
                              ResultType = dataTypeIdentity
                              FieldTypeTexts = constructor.FieldTypeTexts
                              Arity = constructor.Arity })

                    let rawDataType: RawDataTypeInfo =
                        { Identity = dataTypeIdentity
                          TypeParameters = dataType.TypeParameters
                          Constructors = constructors
                          ExternalRuntimeTypeName = dataType.ExternalRuntimeTypeName
                          EmittedTypeName = emittedDataTypeName dataTypeIdentity }

                    dataType.Name, rawDataType)
                |> Map.ofList

            moduleDump.Name, dataTypes)
        |> Map.ofList

    let internal buildRawModuleSkeletons (modules: ClrAssemblyModule list) =
        let rawDataTypes = buildRawDataTypes modules

        modules
        |> List.map (fun moduleDump ->
            let moduleIdentity = ModuleIdentity.ofDottedTextUnchecked moduleDump.Name

            let bindings =
                moduleDump.Bindings
                |> List.filter (fun binding -> not binding.Intrinsic)
                |> List.map (fun binding -> binding.Name, binding)
                |> Map.ofList

            let moduleDataTypes =
                rawDataTypes |> Map.tryFind moduleDump.Name |> Option.defaultValue Map.empty

            let typeExports =
                Set.union (moduleDataTypes |> Map.keys |> Set.ofSeq) (bundledTypeExports moduleDump.Name)

            moduleDump.Name,
            { Identity = moduleIdentity
              Imports = moduleDump.Imports
              Exports = moduleDump.Exports |> Set.ofList
              TypeExports = typeExports
              DataTypes = Map.empty
              Constructors = Map.empty
              Bindings = bindings
              EmittedTypeName = Some(emittedModuleTypeName moduleDump.Name) })
        |> Map.ofList,
        rawDataTypes

    let internal tryResolveQualifiedTypeName (rawModules: Map<string, RawModuleInfo>) segments =
        if List.length segments < 2 then
            None
        else
            let moduleName =
                segments |> List.take (segments.Length - 1) |> String.concat "."

            let typeName = List.last segments

            rawModules
            |> Map.tryFind moduleName
            |> Option.bind (fun moduleInfo ->
                if moduleInfo.TypeExports.Contains(typeName) then
                    Some(TypeIdentity.ofDottedTextUnchecked moduleName typeName)
                else
                    None)

    let internal tryResolveImportedTypeName (rawModules: Map<string, RawModuleInfo>) currentModule name =
        let currentModuleInfo = rawModules[currentModule]

        currentModuleInfo.Imports
        |> List.choose (fun spec ->
            match spec.Source with
            | Url _ ->
                None
            | Dotted moduleSegments ->
                let importedModuleName = SyntaxFacts.moduleNameToText moduleSegments

                match rawModules |> Map.tryFind importedModuleName with
                | Some importedModule
                    when selectionImportsTypeName spec.Selection name
                         && importedModule.TypeExports.Contains(name) ->
                    Some(TypeIdentity.ofDottedTextUnchecked importedModuleName name)
                | _ ->
                    None)
        |> List.distinct
        |> function
            | [ resolved ] -> Some resolved
            | _ -> None

    let internal tryResolveTypeName (rawModules: Map<string, RawModuleInfo>) currentModule typeParameters segments =
        match segments with
        | [] ->
            Result.Error "Expected a type name."
        | [ name ] ->
            if Set.contains name typeParameters then
                Result.Ok(IlTypeParameter name)
            elif name.Length > 0 && Char.IsLower(name[0]) then
                Result.Ok(IlTypeParameter name)
            else
                let resolvedIdentity =
                    if String.Equals(name, Stdlib.KnownTypeNames.Unit, StringComparison.Ordinal) then
                        Some(preludeTypeIdentity Stdlib.KnownTypeNames.Unit)
                    elif String.Equals(name, Stdlib.KnownTypeNames.Ref, StringComparison.Ordinal) then
                        Some(preludeTypeIdentity Stdlib.KnownTypeNames.Ref)
                    elif String.Equals(name, Stdlib.KnownTypeNames.IO, StringComparison.Ordinal) then
                        Some(preludeTypeIdentity Stdlib.KnownTypeNames.IO)
                    elif String.Equals(name, Stdlib.KnownTypeNames.Dict, StringComparison.Ordinal) then
                        Some(preludeTypeIdentity Stdlib.KnownTypeNames.Dict)
                    elif isDictionaryTypeName name then
                        Some(preludeTypeIdentity name)
                    else
                        match tryResolveBundledBuiltinTypeIdentity name with
                        | Some identity ->
                            Some identity
                        | None ->
                            let currentModuleInfo = rawModules[currentModule]

                            match currentModuleInfo.TypeExports.Contains(name), tryResolveImportedTypeName rawModules currentModule name with
                            | true, _ ->
                                Some(TypeIdentity.ofDottedTextUnchecked currentModule name)
                            | false, Some identity ->
                                Some identity
                            | false, None ->
                                None

                match resolvedIdentity with
                | Some identity ->
                    match tryParsePrimitiveTypeIdentity identity with
                    | Some primitiveType -> Result.Ok primitiveType
                    | None -> Result.Ok(IlNamed(identity, []))
                | None ->
                    Result.Error $"IL backend could not resolve type '{name}'."
        | _ ->
            match tryResolveQualifiedTypeName rawModules segments with
            | Some identity ->
                match tryParsePrimitiveTypeIdentity identity with
                | Some primitiveType -> Result.Ok primitiveType
                | None -> Result.Ok(IlNamed(identity, []))
            | None ->
                let typeName = String.concat "." segments
                Result.Error $"IL backend could not resolve type '{typeName}'."

    let internal parseType (rawModules: Map<string, RawModuleInfo>) currentModule typeParameters (tokens: Token list) =
        let significant = significantTokens tokens

        let rec lowerTypeExpr (typeExpr: TypeSignatures.TypeExpr) =
            result {
                match typeExpr with
                | TypeSignatures.TypeName(nameSegments, [ constraintExpr ])
                    when nameSegments = [ Stdlib.KnownTypeNames.Dict ]
                         || nameSegments = Stdlib.PreludeModuleName @ [ Stdlib.KnownTypeNames.Dict ] ->
                    match constraintExpr with
                    | TypeSignatures.TypeName(traitNameSegments, argumentExprs) ->
                        let traitName = List.last traitNameSegments

                        let! loweredArguments =
                            argumentExprs
                            |> List.fold
                                (fun stateResult argumentExpr ->
                                    result {
                                        let! collected = stateResult
                                        let! lowered = lowerTypeExpr argumentExpr
                                        return lowered :: collected
                                    })
                                (Result.Ok [])
                            |> Result.map List.rev

                        return dictionaryIlType traitName loweredArguments
                    | _ ->
                        return! Result.Error "IL backend expected Dict to be applied to a concrete trait constraint."
                | TypeSignatures.TypeName(segments, arguments) ->
                    let! headType = tryResolveTypeName rawModules currentModule typeParameters segments

                    let! loweredArguments =
                        arguments
                        |> List.fold
                            (fun stateResult argumentExpr ->
                                result {
                                    let! collected = stateResult
                                    let! lowered = lowerTypeExpr argumentExpr
                                    return lowered :: collected
                                })
                            (Result.Ok [])
                        |> Result.map List.rev

                    match headType, loweredArguments with
                    | IlPrimitive _, [] -> return headType
                    | IlPrimitive _, _ -> return! Result.Error $"Type '{formatIlType headType}' cannot take arguments."
                    | IlTypeParameter _, [] -> return headType
                    | IlTypeParameter _, _ -> return! Result.Error $"Type '{formatIlType headType}' cannot take arguments."
                    | IlNamed(identity, existingArguments), _ when List.isEmpty existingArguments ->
                        return IlNamed(identity, loweredArguments)
                    | IlNamed _, _ ->
                        return! Result.Error $"Type '{formatIlType headType}' cannot take arguments."
                | TypeSignatures.TypeVariable name ->
                    return! tryResolveTypeName rawModules currentModule typeParameters [ name ]
                | _ ->
                    return! Result.Error "IL backend could not lower this type form."
            }

        let rec parseQualifiedName remaining segments =
            match remaining with
            | dotToken :: nextToken :: tail when dotToken.Kind = Dot && Token.isName nextToken ->
                parseQualifiedName tail (SyntaxFacts.trimIdentifierQuotes nextToken.Text :: segments)
            | _ ->
                List.rev segments, remaining

        let rec parseAtom remaining =
            match remaining with
            | [] ->
                Result.Error "Expected a type.", []
            | token :: tail when token.Kind = LeftParen ->
                match collectParenthesizedTokens (token :: tail) with
                | Result.Ok(innerTokens, rest) ->
                    match parseApplication innerTokens with
                    | Result.Ok innerType -> Result.Ok innerType, rest
                    | Result.Error message -> Result.Error message, rest
                | Result.Error message ->
                    Result.Error message, []
            | token :: tail when Token.isName token ->
                let segments, rest = parseQualifiedName tail [ SyntaxFacts.trimIdentifierQuotes token.Text ]

                match tryResolveTypeName rawModules currentModule typeParameters segments with
                | Result.Ok resolvedType -> Result.Ok resolvedType, rest
                | Result.Error message -> Result.Error message, rest
            | token :: tail ->
                Result.Error $"Unexpected token '{token.Text}' in a type.", tail

        and parseApplication tokens =
            let rec gather remaining parsed =
                match remaining with
                | [] ->
                    Result.Ok(List.rev parsed)
                | token :: _ when token.Kind = RightParen ->
                    Result.Ok(List.rev parsed)
                | token :: _ when token.Kind = Arrow ->
                    Result.Ok(List.rev parsed)
                | _ ->
                    let parsedAtom, rest = parseAtom remaining

                    match parsedAtom with
                    | Result.Error message ->
                        Result.Error message
                    | Result.Ok atom ->
                        gather rest (atom :: parsed)

            gather tokens []
            |> Result.bind (function
                | [] ->
                    Result.Error "Expected a type."
                | IlNamed(identity, existingArguments) :: arguments when List.isEmpty existingArguments ->
                    let fullTokens =
                        if List.isEmpty arguments then
                            tokens
                        else
                            significant

                    match TypeSignatures.parseType fullTokens with
                    | Some parsedExpr ->
                        lowerTypeExpr parsedExpr
                    | None ->
                        Result.Ok(IlNamed(identity, arguments))
                | head :: [] ->
                    Result.Ok head
                | head :: _ ->
                    Result.Error $"Type '{formatIlType head}' cannot take arguments.")

        match parseApplication significant with
        | Result.Error message ->
            Result.Error message
        | Result.Ok parsedType ->
            Result.Ok parsedType

    let internal parseTypeText (rawModules: Map<string, RawModuleInfo>) currentModule (text: string) =
        parseType rawModules currentModule Set.empty (typeTextTokens text)

    let internal parseFunctionSignature (rawModules: Map<string, RawModuleInfo>) currentModule (tokens: Token list) =
        splitTopLevelArrows (significantTokens tokens)
        |> List.filter (List.isEmpty >> not)
        |> List.fold
            (fun stateResult segment ->
                result {
                    let! collected = stateResult
                    let! parsedSegment = parseType rawModules currentModule Set.empty segment
                    return parsedSegment :: collected
                })
            (Result.Ok [])
        |> Result.bind (fun reversedSegments ->
            match List.rev reversedSegments with
            | [] ->
                Result.Error "Expected at least one type in the signature."
            | parsedSegments ->
                let parameterTypes = parsedSegments |> List.take (parsedSegments.Length - 1)
                let returnType = List.last parsedSegments
                Result.Ok(parameterTypes, returnType))

    let internal resolveDataTypes (rawModules: Map<string, RawModuleInfo>) (rawDataTypes: Map<string, Map<string, RawDataTypeInfo>>) =
        rawDataTypes
        |> Map.toList
        |> List.fold
            (fun stateResult (moduleName, moduleDataTypes) ->
                result {
                    let! state = stateResult

                    let! resolvedEntries =
                        moduleDataTypes
                        |> Map.toList
                        |> List.fold
                            (fun entriesResult (_, rawDataType) ->
                                result {
                                    let! entries = entriesResult
                                    let typeParameterScope = rawDataType.TypeParameters |> Set.ofList

                                    let! constructors =
                                        rawDataType.Constructors
                                        |> List.fold
                                            (fun constructorsResult rawConstructor ->
                                                result {
                                                    let! constructorsSoFar = constructorsResult

                                                    let! fieldTypes =
                                                        rawConstructor.FieldTypeTexts
                                                        |> List.fold
                                                            (fun fieldResult fieldTypeText ->
                                                                result {
                                                                    let! fields = fieldResult
                                                                    let! fieldType = parseTypeText rawModules moduleName fieldTypeText
                                                                    return fieldType :: fields
                                                                })
                                                            (Result.Ok [])

                                                    let resolvedConstructor =
                                                        { Identity = rawConstructor.Identity
                                                          ResultType = rawConstructor.ResultType
                                                          TypeParameters = rawDataType.TypeParameters
                                                          FieldTypes = List.rev fieldTypes
                                                          EmittedTypeName = emittedConstructorTypeName rawConstructor.Identity }

                                                    return
                                                        (DeclarationIdentity.name rawConstructor.Identity, resolvedConstructor)
                                                        :: constructorsSoFar
                                                })
                                            (Result.Ok [])

                                    let resolvedDataType =
                                        { Identity = rawDataType.Identity
                                          TypeParameters = rawDataType.TypeParameters
                                          Constructors = constructors |> Map.ofList
                                          ExternalRuntimeTypeName = rawDataType.ExternalRuntimeTypeName
                                          EmittedTypeName = rawDataType.EmittedTypeName }: DataTypeInfo

                                    return (TypeIdentity.name rawDataType.Identity, resolvedDataType) :: entries
                                })
                            (Result.Ok [])

                    return state |> Map.add moduleName (resolvedEntries |> Map.ofList)
                })
            (Result.Ok Map.empty)

    let internal attachResolvedDataTypes (rawModules: Map<string, RawModuleInfo>) (resolvedDataTypes: Map<string, Map<string, DataTypeInfo>>) =
        rawModules
        |> Map.map (fun moduleName moduleInfo ->
            let dataTypes = resolvedDataTypes |> Map.tryFind moduleName |> Option.defaultValue Map.empty

            let constructors =
                dataTypes
                |> Map.toList
                |> List.collect (fun (_, dataType) -> dataType.Constructors |> Map.toList)
                |> Map.ofList

            { moduleInfo with
                DataTypes = dataTypes
                Constructors = constructors })

    let rec internal substituteType substitution ilType =
        match ilType with
        | IlPrimitive _ ->
            ilType
        | IlNamed(identity, arguments) ->
            IlNamed(identity, arguments |> List.map (substituteType substitution))
        | IlTypeParameter name ->
            substitution |> Map.tryFind name |> Option.defaultValue ilType

    let rec internal containsTypeParameters ilType =
        match ilType with
        | IlPrimitive _ ->
            false
        | IlTypeParameter _ ->
            true
        | IlNamed(_, arguments) ->
            arguments |> List.exists containsTypeParameters

    let rec internal collectTypeParameters ilType =
        match ilType with
        | IlPrimitive _ ->
            Set.empty
        | IlTypeParameter name ->
            Set.singleton name
        | IlNamed(_, arguments) ->
            arguments
            |> List.fold (fun state argumentType -> Set.union state (collectTypeParameters argumentType)) Set.empty

    let internal containsTypeParametersOutside (allowed: Set<string>) ilType =
        collectTypeParameters ilType |> Set.exists (fun name -> not (allowed.Contains name))

    let internal bindingTypeParameters parameterTypes returnType =
        (returnType :: parameterTypes)
        |> List.fold (fun state ilType -> Set.union state (collectTypeParameters ilType)) Set.empty
        |> Set.toList
        |> List.sort

    let rec internal unifyTypes substitution template actual =
        match template, actual with
        | IlPrimitive left, IlPrimitive right when left = right ->
            Result.Ok substitution
        | IlTypeParameter name, _ ->
            match substitution |> Map.tryFind name with
            | Some existing when existing = actual ->
                Result.Ok substitution
            | Some existing ->
                Result.Error $"IL backend could not unify {formatIlType existing} with {formatIlType actual}."
            | None ->
                Result.Ok(substitution |> Map.add name actual)
        | IlNamed(leftIdentity, leftArguments), IlNamed(rightIdentity, rightArguments)
            when leftIdentity = rightIdentity && List.length leftArguments = List.length rightArguments ->
            List.zip leftArguments rightArguments
            |> List.fold
                (fun stateResult (leftArgument, rightArgument) ->
                    stateResult |> Result.bind (fun state -> unifyTypes state leftArgument rightArgument))
                (Result.Ok substitution)
        | _ ->
            Result.Error $"IL backend could not unify {formatIlType template} with {formatIlType actual}."

    let internal constructorResultType (constructorInfo: ConstructorInfo) =
        IlNamed(constructorInfo.ResultType, constructorInfo.TypeParameters |> List.map IlTypeParameter)

    let internal buildDeclaredBindingLookup (modules: ClrAssemblyModule list) =
        modules
        |> List.collect (fun moduleDump ->
            moduleDump.Bindings
            |> List.map (fun binding ->
                (moduleDump.Name, binding.Name),
                ({ ParameterTypes = binding.Parameters |> List.map (fun parameter -> parameter.TypeText)
                   ReturnType = binding.ReturnTypeText }: DeclaredBindingTexts)))
        |> Map.ofList

    let internal buildTraitInstances (rawModules: Map<string, RawModuleInfo>) (modules: ClrAssemblyModule list) =
        modules
        |> List.fold
            (fun stateResult moduleDump ->
                result {
                    let moduleIdentity = ModuleIdentity.ofDottedTextUnchecked moduleDump.Name

                    let! instances = stateResult

                    let! discoveredInstances =
                        moduleDump.TraitInstances
                        |> List.fold
                            (fun instancesResult instanceDeclaration ->
                                result {
                                    let! collected = instancesResult

                                    let! headTypes =
                                        instanceDeclaration.HeadTypeTexts
                                        |> List.fold
                                            (fun headTypesResult headTypeText ->
                                                result {
                                                    let! parsedHeadTypes = headTypesResult
                                                    let! headType = parseTypeText rawModules moduleDump.Name headTypeText
                                                    return headType :: parsedHeadTypes
                                                })
                                            (Result.Ok [])

                                    let instanceInfo =
                                        { ModuleIdentity = moduleIdentity
                                          Trait =
                                            instanceDeclaration.TraitName.Split('.', StringSplitOptions.RemoveEmptyEntries)
                                            |> Array.toList
                                            |> TypeSignatures.TraitReference.ofSegments
                                          InstanceKey = instanceDeclaration.InstanceKey
                                          HeadTypes = List.rev headTypes
                                          MemberBindings = instanceDeclaration.MemberBindings |> Map.ofList }

                                    return instanceInfo :: collected
                                })
                            (Result.Ok [])

                    return List.rev discoveredInstances @ instances
                })
            (Result.Ok [])

    let internal resolveDeclaredBindingTypes
        (rawModules: Map<string, RawModuleInfo>)
        currentModule
        (declaredTexts: DeclaredBindingTexts)
        : Result<DeclaredBindingTypes, string> =
        result {
            let! parameterTypes =
                if declaredTexts.ParameterTypes |> List.exists Option.isNone then
                    Result.Ok None
                else
                    declaredTexts.ParameterTypes
                    |> List.choose id
                    |> List.fold
                        (fun stateResult parameterTypeText ->
                            result {
                                let! collected = stateResult
                                let! parameterType = parseTypeText rawModules currentModule parameterTypeText
                                return parameterType :: collected
                            })
                        (Result.Ok [])
                    |> Result.map (List.rev >> Some)

            let! returnType =
                match declaredTexts.ReturnType with
                | Some returnTypeText ->
                    parseTypeText rawModules currentModule returnTypeText |> Result.map Some
                | None ->
                    Result.Ok None

            return
                ({ ParameterTypes = parameterTypes
                   ReturnType = returnType }: DeclaredBindingTypes)
        }

    let internal tryFindTraitInstance (environment: EmissionEnvironment) (moduleName: string) (traitName: string) instanceKey =
        let moduleIdentity = ModuleIdentity.ofDottedTextUnchecked moduleName
        let traitReference =
            traitName.Split('.', StringSplitOptions.RemoveEmptyEntries)
            |> Array.toList
            |> TypeSignatures.TraitReference.ofSegments

        environment.TraitInstances
        |> List.tryFind (fun instanceInfo ->
            instanceInfo.ModuleIdentity = moduleIdentity
            && TypeSignatures.TraitReference.matches instanceInfo.Trait traitReference
            && String.Equals(instanceInfo.InstanceKey, instanceKey, StringComparison.Ordinal))

    let internal traitMemberRoutes (environment: EmissionEnvironment) (traitName: string) memberName =
        let traitReference =
            traitName.Split('.', StringSplitOptions.RemoveEmptyEntries)
            |> Array.toList
            |> TypeSignatures.TraitReference.ofSegments

        environment.TraitInstances
        |> List.choose (fun instanceInfo ->
            instanceInfo.MemberBindings
            |> Map.tryFind memberName
            |> Option.bind (fun bindingName ->
                if TypeSignatures.TraitReference.matches instanceInfo.Trait traitReference then
                    Some(instanceInfo, bindingName)
                else
                    None))

    let internal knownIntrinsicNames =
        IntrinsicCatalog.namedIntrinsicTermNames ()

    let internal intrinsicParameterTypes name argumentTypes =
        match name, argumentTypes with
        | ("print" | "println"), [ IlPrimitive IlString ] ->
            Some([ IlPrimitive IlString ], unitIlType)
        | "primitiveIntToString", [ IlPrimitive IlInt64 ] ->
            Some([ IlPrimitive IlInt64 ], IlPrimitive IlString)
        | "unsafeConsume", [ valueType ] ->
            Some([ valueType ], unitIlType)
        | "pure", [ valueType ] ->
            Some([ valueType ], valueType)
        | ("primitiveReadData" | "readData"), [ fileType ] ->
            Some([ fileType ], IlPrimitive IlString)
        | "primitiveCloseFile", [ fileType ] ->
            Some([ fileType ], unitIlType)
        | "newRef", [ valueType ] ->
            Some([ valueType ], refIlType valueType)
        | "readRef", [ IlNamed(typeIdentity, [ valueType ]) ] when TypeIdentity.hasTopLevelName preludeModuleIdentity Stdlib.KnownTypeNames.Ref typeIdentity ->
            Some([ refIlType valueType ], valueType)
        | "writeRef", [ IlNamed(typeIdentity, [ valueType ]); actualValueType ]
            when TypeIdentity.hasTopLevelName preludeModuleIdentity Stdlib.KnownTypeNames.Ref typeIdentity && valueType = actualValueType ->
            Some([ refIlType valueType; valueType ], unitIlType)
        | "not", [ IlPrimitive IlBool ] ->
            Some([ IlPrimitive IlBool ], IlPrimitive IlBool)
        | "negate", [ IlPrimitive IlInt64 ] ->
            Some([ IlPrimitive IlInt64 ], IlPrimitive IlInt64)
        | "negate", [ IlPrimitive IlFloat64 ] ->
            Some([ IlPrimitive IlFloat64 ], IlPrimitive IlFloat64)
        | "and", [ IlPrimitive IlBool; IlPrimitive IlBool ]
        | "or", [ IlPrimitive IlBool; IlPrimitive IlBool ] ->
            Some([ IlPrimitive IlBool; IlPrimitive IlBool ], IlPrimitive IlBool)
        | intrinsicName, [ valueType ] when intrinsicName = KnownPreludeSemantics.BuiltinPreludeShowHelperName ->
            Some([ valueType ], IlPrimitive IlString)
        | intrinsicName, [ leftType; rightType ] when intrinsicName = KnownPreludeSemantics.BuiltinPreludeCompareHelperName && leftType = rightType ->
            Some([ leftType; rightType ], IlNamed(preludeTypeIdentity Stdlib.KnownTypeNames.Ordering, []))
        | _ ->
            None

    let internal tryResolveImportedBinding (modules: Map<string, ModuleSurface<'binding>>) currentModule name =
        let currentModuleInfo = modules[currentModule]

        currentModuleInfo.Imports
        |> List.choose (fun spec ->
            match spec.Source with
            | Url _ ->
                None
            | Dotted moduleSegments ->
                let importedModuleName = SyntaxFacts.moduleNameToText moduleSegments

                match modules |> Map.tryFind importedModuleName with
                | Some importedModule
                    when selectionImportsTermName spec.Selection name
                         && importedModule.Exports.Contains(name) ->
                    importedModule.Bindings
                    |> Map.tryFind name
                    |> Option.map (fun binding -> ModuleIdentity.text importedModule.Identity, binding)
                | _ ->
                    None)
        |> List.distinctBy fst
        |> function
            | [ resolved ] -> Some resolved
            | _ -> None

    let internal tryResolveBinding (modules: Map<string, ModuleSurface<'binding>>) currentModule segments =
        match segments with
        | [] ->
            None
        | [ bindingName ] ->
            let currentModuleInfo = modules[currentModule]

            currentModuleInfo.Bindings
            |> Map.tryFind bindingName
            |> Option.map (fun binding -> currentModule, binding)
            |> Option.orElseWith (fun () -> tryResolveImportedBinding modules currentModule bindingName)
        | _ ->
            let moduleName = segments |> List.take (segments.Length - 1) |> String.concat "."
            let bindingName = List.last segments

            modules
            |> Map.tryFind moduleName
            |> Option.bind (fun moduleInfo -> moduleInfo.Bindings |> Map.tryFind bindingName)
            |> Option.map (fun binding -> moduleName, binding)

    let internal tryDefaultFileType (modules: Map<string, ModuleSurface<'binding>>) currentModule =
        modules
        |> Map.tryFind currentModule
        |> Option.bind (fun moduleInfo ->
            if moduleInfo.DataTypes.ContainsKey("File") then
                Some(namedIlType (TypeIdentity.topLevel moduleInfo.Identity "File") [])
            else
                None)

    let internal tryResolveImportedConstructor (modules: Map<string, ModuleSurface<'binding>>) currentModule name =
        let currentModuleInfo = modules[currentModule]

        currentModuleInfo.Imports
        |> List.choose (fun spec ->
            match spec.Source with
            | Url _ ->
                None
            | Dotted moduleSegments ->
                let importedModuleName = SyntaxFacts.moduleNameToText moduleSegments

                match modules |> Map.tryFind importedModuleName with
                | Some importedModule when importedModule.Exports.Contains(name) ->
                    importedModule.Constructors
                    |> Map.tryFind name
                    |> Option.filter (fun constructorInfo ->
                        selectionImportsConstructorName
                            spec.Selection
                            name
                            (TypeIdentity.name constructorInfo.ResultType))
                    |> Option.map (fun constructorInfo -> ModuleIdentity.text importedModule.Identity, constructorInfo)
                | _ ->
                    None)
        |> List.distinctBy fst
        |> function
            | [ resolved ] -> Some resolved
            | _ -> None

    let internal tryResolveConstructor (modules: Map<string, ModuleSurface<'binding>>) currentModule segments =
        match segments with
        | [] ->
            None
        | [ constructorName ] ->
            let currentModuleInfo = modules[currentModule]

            currentModuleInfo.Constructors
            |> Map.tryFind constructorName
            |> Option.map (fun constructorInfo -> currentModule, constructorInfo)
            |> Option.orElseWith (fun () -> tryResolveImportedConstructor modules currentModule constructorName)
        | _ ->
            let moduleName = segments |> List.take (segments.Length - 1) |> String.concat "."
            let constructorName = List.last segments

            modules
            |> Map.tryFind moduleName
            |> Option.bind (fun moduleInfo -> moduleInfo.Constructors |> Map.tryFind constructorName)
            |> Option.map (fun constructorInfo -> moduleName, constructorInfo)

    let internal inferConstructorTypeFromArguments
        inferExpressionType
        currentModule
        localTypes
        active
        allowedTypeParameters
        expectedType
        expressionArguments
        constructorInfo
        =
        let initialSubstitutionResult =
            match expectedType with
            | Some expected ->
                unifyTypes Map.empty (constructorResultType constructorInfo) expected
            | None ->
                Result.Ok Map.empty

        let argumentTemplates = constructorInfo.FieldTypes

        if List.length argumentTemplates <> List.length expressionArguments then
            Result.Error
                $"IL backend expected constructor '{DeclarationIdentity.name constructorInfo.Identity}' to receive {List.length argumentTemplates} argument(s), but received {List.length expressionArguments}."
        else
            List.zip expressionArguments argumentTemplates
            |> List.fold
                (fun stateResult (argumentExpression, argumentTemplate) ->
                    stateResult
                    |> Result.bind (fun substitution ->
                        let expectedArgumentType =
                            let specialized = substituteType substitution argumentTemplate

                            if containsTypeParametersOutside allowedTypeParameters specialized then
                                None
                            else
                                Some specialized

                        inferExpressionType
                            currentModule
                            localTypes
                            active
                            allowedTypeParameters
                            expectedArgumentType
                            argumentExpression
                        |> Result.bind (fun argumentType -> unifyTypes substitution argumentTemplate argumentType)))
                initialSubstitutionResult
            |> Result.bind (fun substitution ->
                let resultType = substituteType substitution (constructorResultType constructorInfo)

                if containsTypeParametersOutside allowedTypeParameters resultType then
                    Result.Error
                        $"IL backend could not infer concrete type arguments for constructor '{DeclarationIdentity.name constructorInfo.Identity}'."
                else
                    Result.Ok resultType)

    let internal inferBindingTypeFromArguments
        inferExpressionType
        currentModule
        localTypes
        active
        allowedTypeParameters
        expectedType
        expressionArguments
        (bindingInfo: BindingInfo)
        =
        let initialSubstitutionResult =
            match expectedType with
            | Some expected ->
                unifyTypes Map.empty bindingInfo.ReturnType expected
            | None ->
                Result.Ok Map.empty

        if List.length bindingInfo.ParameterTypes <> List.length expressionArguments then
            Result.Error
                $"IL backend expected '{bindingInfo.Binding.Name}' to receive {List.length bindingInfo.ParameterTypes} argument(s), but received {List.length expressionArguments}."
        else
            List.zip expressionArguments bindingInfo.ParameterTypes
            |> List.fold
                (fun stateResult (argumentExpression, (_, argumentTemplate)) ->
                    result {
                        let! substitution = stateResult

                        let expectedArgumentType =
                            let specialized = substituteType substitution argumentTemplate

                            if containsTypeParametersOutside allowedTypeParameters specialized then
                                None
                            else
                                Some specialized

                        let! argumentType =
                            inferExpressionType
                                currentModule
                                localTypes
                                active
                                allowedTypeParameters
                                expectedArgumentType
                                argumentExpression

                        return! unifyTypes substitution argumentTemplate argumentType
                    })
                initialSubstitutionResult
            |> Result.bind (fun substitution ->
                let resultType = substituteType substitution bindingInfo.ReturnType

                if containsTypeParametersOutside allowedTypeParameters resultType then
                    Result.Error
                        $"IL backend could not infer concrete type arguments for '{bindingInfo.Binding.Name}'."
                else
                    Result.Ok(substitution, resultType))
