namespace Kappa.Compiler

open System
open System.Collections.Generic
open System.IO
open System.Reflection
open System.Reflection.Emit

module IlDotNetBackend =
    type private IlPrimitiveType =
        | IlInt64
        | IlFloat64
        | IlBool
        | IlString
        | IlChar

    type private IlType =
        | IlPrimitive of IlPrimitiveType
        | IlNamed of moduleName: string * typeName: string * arguments: IlType list
        | IlTypeParameter of string

    type private RawConstructorInfo =
        { Name: string
          FieldTypeTokens: Token list list
          Arity: int }

    type private RawDataTypeInfo =
        { ModuleName: string
          Name: string
          TypeParameters: string list
          Constructors: RawConstructorInfo list
          EmittedTypeName: string }

    type private ConstructorInfo =
        { ModuleName: string
          Name: string
          TypeName: string
          TypeParameters: string list
          FieldTypes: IlType list
          EmittedTypeName: string }

    type private DataTypeInfo =
        { ModuleName: string
          Name: string
          TypeParameters: string list
          Constructors: Map<string, ConstructorInfo>
          EmittedTypeName: string }

    type private ModuleSurface<'binding> =
        { Name: string
          Imports: ImportSpec list
          Exports: Set<string>
          TypeExports: Set<string>
          DataTypes: Map<string, DataTypeInfo>
          Constructors: Map<string, ConstructorInfo>
          Bindings: Map<string, 'binding>
          EmittedTypeName: string option }

    type private BindingInfo =
        { Binding: KRuntimeBinding
          ParameterTypes: (string * IlType) list
          ReturnType: IlType
          EmittedMethodName: string }

    type private RawModuleInfo = ModuleSurface<KRuntimeBinding>
    type private ModuleInfo = ModuleSurface<BindingInfo>

    type private EmissionEnvironment =
        { Modules: Map<string, ModuleInfo>
          DataTypes: Map<string * string, DataTypeInfo> }

    type private ConstructorEmission =
        { TypeBuilder: TypeBuilder
          GenericParameters: GenericTypeParameterBuilder array
          ConstructorBuilder: ConstructorBuilder
          FieldBuilders: FieldBuilder array }

    type private DataTypeEmission =
        { BaseTypeBuilder: TypeBuilder
          GenericParameters: GenericTypeParameterBuilder array
          BaseConstructor: ConstructorBuilder
          Constructors: Map<string, ConstructorEmission> }

    type private EmissionState =
        { Environment: EmissionEnvironment
          ModuleBuilders: Map<string, TypeBuilder>
          MethodBuilders: Map<string, Map<string, MethodBuilder>>
          DataTypeBuilders: Map<string * string, DataTypeEmission> }

    type private ValueLocation =
        | Argument of int
        | Local of LocalBuilder

    type private LocalValue =
        { Location: ValueLocation
          Type: IlType }

    type private ResultBuilder() =
        member _.Bind(result, binder) = Result.bind binder result
        member _.Return(value) = Result.Ok value
        member _.ReturnFrom(result) = result
        member _.Zero() = Result.Ok()
        member _.Delay(generator) = generator
        member _.Run(generator) = generator()
        member _.Combine(first, second) =
            first |> Result.bind (fun () -> second())

    let private result = ResultBuilder()

    let private aggregateDiagnostics diagnostics =
        diagnostics
        |> List.map (fun diagnostic -> diagnostic.Message)
        |> String.concat Environment.NewLine

    let private sanitizeIdentifier (value: string) =
        let text =
            value
            |> Seq.collect (fun ch ->
                if Char.IsLetterOrDigit(ch) || ch = '_' then
                    Seq.singleton(string ch)
                else
                    Seq.singleton($"_u{int ch:x4}"))
            |> String.concat ""

        if String.IsNullOrWhiteSpace(text) then
            "_"
        elif Char.IsLetter(text[0]) || text[0] = '_' then
            text
        else
            "_" + text

    let emittedModuleTypeName (moduleName: string) =
        let segments =
            moduleName.Split('.', StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries)
            |> Array.map sanitizeIdentifier

        if Array.isEmpty segments then
            invalidOp "Module name cannot be empty."

        "Kappa.Generated." + String.concat "." segments

    let emittedMethodName (bindingName: string) =
        sanitizeIdentifier bindingName

    let private emittedDataTypeName (moduleName: string) (typeName: string) =
        emittedModuleTypeName moduleName + "." + sanitizeIdentifier typeName

    let private emittedConstructorTypeName (moduleName: string) (constructorName: string) =
        emittedModuleTypeName moduleName + "." + sanitizeIdentifier constructorName

    let private primitiveRuntimeType =
        function
        | IlInt64 -> typeof<int64>
        | IlFloat64 -> typeof<double>
        | IlBool -> typeof<bool>
        | IlString -> typeof<string>
        | IlChar -> typeof<char>

    let private primitiveTypeName =
        function
        | IlInt64 -> "Int"
        | IlFloat64 -> "Float"
        | IlBool -> "Bool"
        | IlString -> "String"
        | IlChar -> "Char"

    let rec private formatIlType ilType =
        match ilType with
        | IlPrimitive primitiveType ->
            primitiveTypeName primitiveType
        | IlTypeParameter name ->
            name
        | IlNamed (_, typeName, []) ->
            typeName
        | IlNamed (_, typeName, arguments) ->
            let argumentText = arguments |> List.map formatIlType |> String.concat " "
            $"{typeName} {argumentText}"

    let private significantTokens (tokens: Token list) =
        tokens
        |> List.filter (fun token ->
            match token.Kind with
            | Newline
            | Indent
            | Dedent
            | EndOfFile -> false
            | _ -> true)

    let private parseTypeParameters (headerTokens: Token list) =
        headerTokens
        |> significantTokens
        |> List.takeWhile (fun token -> token.Kind <> Colon)
        |> List.choose (fun token ->
            if Token.isName token then
                Some(SyntaxFacts.trimIdentifierQuotes token.Text)
            else
                None)

    let private splitTopLevelArrows (tokens: Token list) =
        let rec loop depth current remaining segments =
            match remaining with
            | [] ->
                List.rev ((List.rev current) :: segments)
            | token :: tail when token.Kind = LeftParen ->
                loop (depth + 1) (token :: current) tail segments
            | token :: tail when token.Kind = RightParen ->
                loop (max 0 (depth - 1)) (token :: current) tail segments
            | token :: tail when token.Kind = Arrow && depth = 0 ->
                loop depth [] tail ((List.rev current) :: segments)
            | token :: tail ->
                loop depth (token :: current) tail segments

        loop 0 [] tokens []

    let private collectParenthesizedTokens (tokens: Token list) =
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

    let private constructorFieldTypeTokens (tokens: Token list) =
        let significant = significantTokens tokens

        let argumentTokens =
            match significant with
            | leftToken :: operatorToken :: rightToken :: rest
                when leftToken.Kind = LeftParen && operatorToken.Kind = Operator && rightToken.Kind = RightParen ->
                rest
            | _ :: rest ->
                rest
            | [] ->
                []

        let rec splitArguments current remaining groups =
            match remaining with
            | [] ->
                List.rev groups
            | token :: tail when token.Kind = LeftParen ->
                match collectParenthesizedTokens (token :: tail) with
                | Result.Ok(groupTokens, rest) ->
                    splitArguments current rest (groupTokens :: groups)
                | Result.Error _ ->
                    List.rev groups
            | token :: tail ->
                splitArguments current tail ([ token ] :: groups)

        let extractFieldTokens groupTokens =
            let rec findColon depth remaining =
                match remaining with
                | [] -> None
                | token :: tail when token.Kind = LeftParen -> findColon (depth + 1) tail
                | token :: tail when token.Kind = RightParen -> findColon (max 0 (depth - 1)) tail
                | token :: tail when token.Kind = Colon && depth = 0 -> Some tail
                | _ :: tail -> findColon depth tail

            match findColon 0 groupTokens with
            | Some tail -> significantTokens tail
            | None -> significantTokens groupTokens

        splitArguments [] argumentTokens []
        |> List.map extractFieldTokens
        |> List.filter (List.isEmpty >> not)

    let private tryParsePrimitiveTypeName typeName =
        match typeName with
        | "Int" -> Some(IlPrimitive IlInt64)
        | "Float" -> Some(IlPrimitive IlFloat64)
        | "Bool" -> Some(IlPrimitive IlBool)
        | "String" -> Some(IlPrimitive IlString)
        | "Char" -> Some(IlPrimitive IlChar)
        | _ -> None

    let private itemImportsTypeName (item: ImportItem) =
        item.Namespace.IsNone || item.Namespace = Some ImportNamespace.Type

    let private itemImportsTermName (item: ImportItem) =
        item.Namespace.IsNone || item.Namespace = Some ImportNamespace.Term

    let private itemImportsConstructorName (item: ImportItem) =
        item.Namespace = Some ImportNamespace.Constructor

    let private selectionImportsTypeName selection name =
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
        | AllExcept excludedNames ->
            not (List.contains name excludedNames)

    let private selectionImportsTermName selection name =
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
        | AllExcept excludedNames ->
            not (List.contains name excludedNames)

    let private selectionImportsConstructorName selection name =
        match selection with
        | QualifiedOnly ->
            false
        | Items items ->
            items
            |> List.exists (fun item ->
                String.Equals(item.Name, name, StringComparison.Ordinal)
                && itemImportsConstructorName item)
        | All
        | AllExcept _ ->
            false

    let private buildRawDataTypes (workspace: WorkspaceCompilation) =
        workspace.Documents
        |> List.choose (fun document ->
            document.ModuleName
            |> Option.map (fun moduleName ->
                SyntaxFacts.moduleNameToText moduleName, document.Syntax.Declarations))
        |> List.fold
            (fun modules (moduleName, declarations) ->
                let dataTypes =
                    declarations
                    |> List.choose (function
                        | DataDeclarationNode declaration ->
                            let constructors =
                                declaration.Constructors
                                |> List.map (fun constructor ->
                                    { Name = constructor.Name
                                      FieldTypeTokens = constructorFieldTypeTokens constructor.Tokens
                                      Arity = constructor.Arity })

                            let rawDataType: RawDataTypeInfo =
                                { ModuleName = moduleName
                                  Name = declaration.Name
                                  TypeParameters = parseTypeParameters declaration.HeaderTokens
                                  Constructors = constructors
                                  EmittedTypeName = emittedDataTypeName moduleName declaration.Name }

                            Some(declaration.Name, rawDataType)
                        | _ ->
                            None)
                    |> Map.ofList

                modules |> Map.add moduleName dataTypes)
            Map.empty

    let private buildRawModuleSkeletons (workspace: WorkspaceCompilation) =
        let rawDataTypes = buildRawDataTypes workspace

        workspace.KRuntimeIR
        |> List.map (fun moduleDump ->
            let bindings =
                moduleDump.Bindings
                |> List.filter (fun binding -> not binding.Intrinsic)
                |> List.map (fun binding -> binding.Name, binding)
                |> Map.ofList

            let moduleDataTypes =
                rawDataTypes |> Map.tryFind moduleDump.Name |> Option.defaultValue Map.empty

            moduleDump.Name,
            { Name = moduleDump.Name
              Imports = moduleDump.Imports
              Exports = moduleDump.Exports |> Set.ofList
              TypeExports = moduleDataTypes |> Map.keys |> Set.ofSeq
              DataTypes = Map.empty
              Constructors = Map.empty
              Bindings = bindings
              EmittedTypeName = Some(emittedModuleTypeName moduleDump.Name) })
        |> Map.ofList,
        rawDataTypes

    let private tryResolveQualifiedTypeName (rawModules: Map<string, RawModuleInfo>) segments =
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
                    Some(moduleName, typeName)
                else
                    None)

    let private tryResolveImportedTypeName (rawModules: Map<string, RawModuleInfo>) currentModule name =
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
                    Some(importedModuleName, name)
                | _ ->
                    None)
        |> List.distinct
        |> function
            | [ resolved ] -> Some resolved
            | _ -> None

    let private tryResolveTypeName (rawModules: Map<string, RawModuleInfo>) currentModule typeParameters segments =
        match segments with
        | [] ->
            Result.Error "Expected a type name."
        | [ name ] ->
            match tryParsePrimitiveTypeName name with
            | Some primitiveType ->
                Result.Ok primitiveType
            | None when Set.contains name typeParameters ->
                Result.Ok(IlTypeParameter name)
            | None ->
                let currentModuleInfo = rawModules[currentModule]

                match currentModuleInfo.TypeExports.Contains(name), tryResolveImportedTypeName rawModules currentModule name with
                | true, _ ->
                    Result.Ok(IlNamed(currentModule, name, []))
                | false, Some(moduleName, typeName) ->
                    Result.Ok(IlNamed(moduleName, typeName, []))
                | false, None ->
                    Result.Error $"IL backend could not resolve type '{name}'."
        | _ ->
            match tryResolveQualifiedTypeName rawModules segments with
            | Some(moduleName, typeName) ->
                Result.Ok(IlNamed(moduleName, typeName, []))
            | None ->
                let typeName = String.concat "." segments
                Result.Error $"IL backend could not resolve type '{typeName}'."

    let private parseType (rawModules: Map<string, RawModuleInfo>) currentModule typeParameters (tokens: Token list) =
        let significant = significantTokens tokens

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
                | head :: [] ->
                    Result.Ok head
                | IlNamed(moduleName, typeName, existingArguments) :: arguments when List.isEmpty existingArguments ->
                    Result.Ok(IlNamed(moduleName, typeName, arguments))
                | head :: _ ->
                    Result.Error $"Type '{formatIlType head}' cannot take arguments.")

        match parseApplication significant with
        | Result.Error message ->
            Result.Error message
        | Result.Ok parsedType ->
            Result.Ok parsedType

    let private parseFunctionSignature (rawModules: Map<string, RawModuleInfo>) currentModule (tokens: Token list) =
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

    let private resolveDataTypes (rawModules: Map<string, RawModuleInfo>) (rawDataTypes: Map<string, Map<string, RawDataTypeInfo>>) =
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
                                                        rawConstructor.FieldTypeTokens
                                                        |> List.fold
                                                            (fun fieldResult fieldTokens ->
                                                                result {
                                                                    let! fields = fieldResult
                                                                    let! fieldType = parseType rawModules moduleName typeParameterScope fieldTokens
                                                                    return fieldType :: fields
                                                                })
                                                            (Result.Ok [])

                                                    let resolvedConstructor =
                                                        { ModuleName = moduleName
                                                          Name = rawConstructor.Name
                                                          TypeName = rawDataType.Name
                                                          TypeParameters = rawDataType.TypeParameters
                                                          FieldTypes = List.rev fieldTypes
                                                          EmittedTypeName = emittedConstructorTypeName moduleName rawConstructor.Name }

                                                    return (rawConstructor.Name, resolvedConstructor) :: constructorsSoFar
                                                })
                                            (Result.Ok [])

                                    let resolvedDataType =
                                        { ModuleName = moduleName
                                          Name = rawDataType.Name
                                          TypeParameters = rawDataType.TypeParameters
                                          Constructors = constructors |> Map.ofList
                                          EmittedTypeName = rawDataType.EmittedTypeName }: DataTypeInfo

                                    return (rawDataType.Name, resolvedDataType) :: entries
                                })
                            (Result.Ok [])

                    return state |> Map.add moduleName (resolvedEntries |> Map.ofList)
                })
            (Result.Ok Map.empty)

    let private attachResolvedDataTypes (rawModules: Map<string, RawModuleInfo>) (resolvedDataTypes: Map<string, Map<string, DataTypeInfo>>) =
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

    let rec private substituteType substitution ilType =
        match ilType with
        | IlPrimitive _ ->
            ilType
        | IlNamed(moduleName, typeName, arguments) ->
            IlNamed(moduleName, typeName, arguments |> List.map (substituteType substitution))
        | IlTypeParameter name ->
            substitution |> Map.tryFind name |> Option.defaultValue ilType

    let rec private containsTypeParameters ilType =
        match ilType with
        | IlPrimitive _ ->
            false
        | IlTypeParameter _ ->
            true
        | IlNamed(_, _, arguments) ->
            arguments |> List.exists containsTypeParameters

    let rec private unifyTypes substitution template actual =
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
        | IlNamed(leftModule, leftTypeName, leftArguments), IlNamed(rightModule, rightTypeName, rightArguments)
            when String.Equals(leftModule, rightModule, StringComparison.Ordinal)
                 && String.Equals(leftTypeName, rightTypeName, StringComparison.Ordinal)
                 && List.length leftArguments = List.length rightArguments ->
            List.zip leftArguments rightArguments
            |> List.fold
                (fun stateResult (leftArgument, rightArgument) ->
                    stateResult |> Result.bind (fun state -> unifyTypes state leftArgument rightArgument))
                (Result.Ok substitution)
        | _ ->
            Result.Error $"IL backend could not unify {formatIlType template} with {formatIlType actual}."

    let private constructorResultType (constructorInfo: ConstructorInfo) =
        IlNamed(
            constructorInfo.ModuleName,
            constructorInfo.TypeName,
            constructorInfo.TypeParameters |> List.map IlTypeParameter
        )

    let private buildSignatureLookup (workspace: WorkspaceCompilation) =
        workspace.Documents
        |> List.choose (fun document ->
            document.ModuleName
            |> Option.map (fun moduleName ->
                SyntaxFacts.moduleNameToText moduleName, document.Syntax.Declarations))
        |> List.collect (fun (moduleName, declarations) ->
            declarations
            |> List.choose (function
                | SignatureDeclaration declaration ->
                    Some((moduleName, declaration.Name), declaration.TypeTokens)
                | _ ->
                    None))
        |> Map.ofList

    let private tryResolveImportedBinding (modules: Map<string, ModuleSurface<'binding>>) currentModule name =
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
                    importedModule.Bindings |> Map.tryFind name |> Option.map (fun binding -> importedModule.Name, binding)
                | _ ->
                    None)
        |> List.distinctBy fst
        |> function
            | [ resolved ] -> Some resolved
            | _ -> None

    let private tryResolveBinding (modules: Map<string, ModuleSurface<'binding>>) currentModule segments =
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

    let private tryResolveImportedConstructor (modules: Map<string, ModuleSurface<'binding>>) currentModule name =
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
                    when selectionImportsConstructorName spec.Selection name
                         && importedModule.Exports.Contains(name) ->
                    importedModule.Constructors
                    |> Map.tryFind name
                    |> Option.map (fun constructorInfo -> importedModule.Name, constructorInfo)
                | _ ->
                    None)
        |> List.distinctBy fst
        |> function
            | [ resolved ] -> Some resolved
            | _ -> None

    let private tryResolveConstructor (modules: Map<string, ModuleSurface<'binding>>) currentModule segments =
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

    let private inferConstructorTypeFromArguments inferExpressionType currentModule localTypes active expectedType expressionArguments constructorInfo =
        let initialSubstitutionResult =
            match expectedType with
            | Some expected ->
                unifyTypes Map.empty (constructorResultType constructorInfo) expected
            | None ->
                Result.Ok Map.empty

        let argumentTemplates = constructorInfo.FieldTypes

        if List.length argumentTemplates <> List.length expressionArguments then
            Result.Error
                $"IL backend expected constructor '{constructorInfo.Name}' to receive {List.length argumentTemplates} argument(s), but received {List.length expressionArguments}."
        else
            List.zip expressionArguments argumentTemplates
            |> List.fold
                (fun stateResult (argumentExpression, argumentTemplate) ->
                    stateResult
                    |> Result.bind (fun substitution ->
                        let expectedArgumentType =
                            let specialized = substituteType substitution argumentTemplate

                            if containsTypeParameters specialized then
                                None
                            else
                                Some specialized

                        inferExpressionType currentModule localTypes active expectedArgumentType argumentExpression
                        |> Result.bind (fun argumentType -> unifyTypes substitution argumentTemplate argumentType)))
                initialSubstitutionResult
            |> Result.bind (fun substitution ->
                let resultType = substituteType substitution (constructorResultType constructorInfo)

                if containsTypeParameters resultType then
                    Result.Error
                        $"IL backend could not infer concrete type arguments for constructor '{constructorInfo.Name}'."
                else
                    Result.Ok resultType)

    let private buildEnvironment (workspace: WorkspaceCompilation) =
        let rawSkeletons, rawDataTypes = buildRawModuleSkeletons workspace

        resolveDataTypes rawSkeletons rawDataTypes
        |> Result.bind (fun resolvedDataTypes ->
            let rawModules = attachResolvedDataTypes rawSkeletons resolvedDataTypes
            let signatureLookup = buildSignatureLookup workspace
            let cache = Dictionary<string * string, BindingInfo>()

            let tryResolveExplicitSignature currentModule bindingName =
                match signatureLookup |> Map.tryFind (currentModule, bindingName) with
                | Some typeTokens ->
                    parseFunctionSignature rawModules currentModule typeTokens |> Result.map Some
                | None ->
                    Result.Ok None

            let rec inferBindingInfo currentModule bindingName active =
                let cacheKey = currentModule, bindingName

                match cache.TryGetValue(cacheKey) with
                | true, cached ->
                    Result.Ok cached
                | _ when Set.contains cacheKey active ->
                    Result.Error $"IL backend recursive type inference is not implemented yet for '{currentModule}.{bindingName}'."
                | _ ->
                    match rawModules |> Map.tryFind currentModule |> Option.bind (fun moduleInfo -> moduleInfo.Bindings |> Map.tryFind bindingName) with
                    | None ->
                        Result.Error $"IL backend could not resolve binding '{currentModule}.{bindingName}'."
                    | Some binding ->
                        match binding.Body with
                        | None ->
                            Result.Error $"IL backend requires a body for '{currentModule}.{bindingName}'."
                        | Some body ->
                            result {
                                let! explicitSignature = tryResolveExplicitSignature currentModule bindingName

                                match binding.Parameters, explicitSignature with
                                | [], Some(parameterTypes, _) when not (List.isEmpty parameterTypes) ->
                                    return!
                                        Result.Error
                                            $"IL backend expected zero-argument binding '{currentModule}.{bindingName}' to have a zero-argument signature."
                                | [], declaredSignature ->
                                    let declaredReturnType =
                                        declaredSignature |> Option.map snd

                                    let! bodyType =
                                        inferExpressionType currentModule Map.empty (Set.add cacheKey active) declaredReturnType body

                                    match declaredReturnType with
                                    | Some expectedReturnType when expectedReturnType <> bodyType ->
                                        return!
                                            Result.Error
                                                $"IL backend expected '{currentModule}.{bindingName}' to return {formatIlType expectedReturnType}, but the body returns {formatIlType bodyType}."
                                    | _ ->
                                        let info =
                                            { Binding = binding
                                              ParameterTypes = []
                                              ReturnType = declaredReturnType |> Option.defaultValue bodyType
                                              EmittedMethodName = emittedMethodName bindingName }

                                        cache[cacheKey] <- info
                                        return info
                                | parameters, None ->
                                    return!
                                        Result.Error
                                            $"IL backend currently requires an explicit signature for parameterized binding '{currentModule}.{bindingName}'."
                                | parameters, Some(parameterTypes, returnType) ->
                                    if List.length parameters <> List.length parameterTypes then
                                        return!
                                            Result.Error
                                                $"IL backend expected signature for '{currentModule}.{bindingName}' to declare {List.length parameters} parameter type(s), but found {List.length parameterTypes}."

                                    let localTypes =
                                        List.zip parameters parameterTypes |> Map.ofList

                                    let info =
                                        { Binding = binding
                                          ParameterTypes = List.zip parameters parameterTypes
                                          ReturnType = returnType
                                          EmittedMethodName = emittedMethodName bindingName }

                                    cache[cacheKey] <- info

                                    let! bodyType =
                                        inferExpressionType currentModule localTypes (Set.add cacheKey active) (Some returnType) body

                                    if bodyType <> returnType then
                                        cache.Remove(cacheKey) |> ignore

                                        return!
                                            Result.Error
                                                $"IL backend expected '{currentModule}.{bindingName}' to return {formatIlType returnType}, but the body returns {formatIlType bodyType}."

                                    return info
                            }

            and inferPatternBindings currentModule active expectedType pattern =
                match pattern with
                | KRuntimeWildcardPattern ->
                    Result.Ok(Map.empty<string, IlType>)
                | KRuntimeNamePattern name ->
                    Result.Ok(Map.ofList [ name, expectedType ])
                | KRuntimeLiteralPattern literal ->
                    let literalType =
                        match literal with
                        | LiteralValue.Integer _ -> IlPrimitive IlInt64
                        | LiteralValue.Float _ -> IlPrimitive IlFloat64
                        | LiteralValue.String _ -> IlPrimitive IlString
                        | LiteralValue.Character _ -> IlPrimitive IlChar
                        | LiteralValue.Unit -> IlNamed("std.prelude", "Unit", [])

                    if literalType = expectedType then
                        Result.Ok(Map.empty<string, IlType>)
                    else
                        Result.Error
                            $"IL backend cannot match literal of type {formatIlType literalType} against {formatIlType expectedType}."
                | KRuntimeConstructorPattern(nameSegments, argumentPatterns) ->
                    match tryResolveConstructor rawModules currentModule nameSegments with
                    | None ->
                        let patternName = String.concat "." nameSegments
                        Result.Error $"IL backend could not resolve constructor pattern '{patternName}'."
                    | Some(_, constructorInfo) ->
                        let resultTemplate = constructorResultType constructorInfo

                        unifyTypes Map.empty resultTemplate expectedType
                        |> Result.bind (fun substitution ->
                            if List.length argumentPatterns <> List.length constructorInfo.FieldTypes then
                                Result.Error
                                    $"IL backend expected pattern '{constructorInfo.Name}' to receive {List.length constructorInfo.FieldTypes} argument(s), but received {List.length argumentPatterns}."
                            else
                                List.zip argumentPatterns constructorInfo.FieldTypes
                                |> List.fold
                                    (fun stateResult (argumentPattern, fieldTemplate) ->
                                        stateResult
                                        |> Result.bind (fun bindings ->
                                            let fieldType = substituteType substitution fieldTemplate

                                            inferPatternBindings currentModule active fieldType argumentPattern
                                            |> Result.map (fun childBindings ->
                                                Map.fold (fun acc key value -> acc |> Map.add key value) bindings childBindings)))
                                    (Result.Ok(Map.empty<string, IlType>)))

            and inferExpressionType currentModule localTypes active expectedType expression =
                let ensureExpected actualType =
                    match expectedType with
                    | Some expected when expected <> actualType ->
                        Result.Error
                            $"IL backend expected expression of type {formatIlType expected}, but found {formatIlType actualType}."
                    | _ ->
                        Result.Ok actualType

                let inferNamedValue segments =
                    let nameText = String.concat "." segments

                    match segments with
                    | [ "True" ]
                    | [ "False" ] ->
                        ensureExpected (IlPrimitive IlBool)
                    | [ name ] when localTypes |> Map.containsKey name ->
                        ensureExpected localTypes[name]
                    | _ ->
                        match tryResolveBinding rawModules currentModule segments with
                        | Some(targetModule, bindingInfo) when List.isEmpty bindingInfo.Parameters ->
                            inferBindingInfo targetModule bindingInfo.Name active
                            |> Result.bind (fun resolvedBinding -> ensureExpected resolvedBinding.ReturnType)
                        | Some(targetModule, bindingInfo) ->
                            Result.Error $"IL backend does not support function-valued name '{targetModule}.{bindingInfo.Name}' yet."
                        | None ->
                            match tryResolveConstructor rawModules currentModule segments with
                            | Some(_, constructorInfo) when List.isEmpty constructorInfo.FieldTypes ->
                                inferConstructorTypeFromArguments inferExpressionType currentModule localTypes active expectedType [] constructorInfo
                            | Some(targetModule, constructorInfo) ->
                                Result.Error
                                    $"IL backend does not support constructor-valued name '{targetModule}.{constructorInfo.Name}' yet."
                            | None ->
                                let localsText =
                                    localTypes |> Map.toList |> List.map fst |> String.concat ", "

                                Result.Error
                                    $"IL backend could not resolve name '{nameText}'. Locals in scope: [{localsText}]."

                match expression with
                | KRuntimeLiteral(LiteralValue.Integer _) ->
                    ensureExpected (IlPrimitive IlInt64)
                | KRuntimeLiteral(LiteralValue.Float _) ->
                    ensureExpected (IlPrimitive IlFloat64)
                | KRuntimeLiteral(LiteralValue.String _) ->
                    ensureExpected (IlPrimitive IlString)
                | KRuntimeLiteral(LiteralValue.Character _) ->
                    ensureExpected (IlPrimitive IlChar)
                | KRuntimeLiteral LiteralValue.Unit ->
                    Result.Error "IL backend does not emit Unit values yet."
                | KRuntimeName segments ->
                    inferNamedValue segments
                | KRuntimeUnary("-", operand) ->
                    inferExpressionType currentModule localTypes active None operand
                    |> Result.bind (function
                        | IlPrimitive IlInt64 -> ensureExpected (IlPrimitive IlInt64)
                        | IlPrimitive IlFloat64 -> ensureExpected (IlPrimitive IlFloat64)
                        | other -> Result.Error $"Unary '-' is not supported for {formatIlType other}.")
                | KRuntimeUnary(operatorName, _) ->
                    Result.Error $"IL backend does not support unary operator '{operatorName}' yet."
                | KRuntimeBinary(left, operatorName, right) ->
                    let builtinOperators =
                        Set.ofList [ "+"; "-"; "*"; "/"; "=="; "!="; "<"; "<="; ">"; ">="; "&&"; "||" ]

                    if builtinOperators.Contains(operatorName) then
                        let builtinResult =
                            result {
                                let! leftType = inferExpressionType currentModule localTypes active None left
                                let! rightType = inferExpressionType currentModule localTypes active None right

                                return!
                                    match operatorName, leftType, rightType with
                                    | ("+" | "-" | "*" | "/"), IlPrimitive IlInt64, IlPrimitive IlInt64 ->
                                        ensureExpected (IlPrimitive IlInt64)
                                    | ("+" | "-" | "*" | "/"), IlPrimitive IlFloat64, IlPrimitive IlFloat64 ->
                                        ensureExpected (IlPrimitive IlFloat64)
                                    | ("==" | "!="), IlPrimitive IlInt64, IlPrimitive IlInt64 ->
                                        ensureExpected (IlPrimitive IlBool)
                                    | ("==" | "!="), IlPrimitive IlFloat64, IlPrimitive IlFloat64 ->
                                        ensureExpected (IlPrimitive IlBool)
                                    | ("==" | "!="), IlPrimitive IlBool, IlPrimitive IlBool ->
                                        ensureExpected (IlPrimitive IlBool)
                                    | ("==" | "!="), IlPrimitive IlChar, IlPrimitive IlChar ->
                                        ensureExpected (IlPrimitive IlBool)
                                    | ("<" | "<=" | ">" | ">="), IlPrimitive IlInt64, IlPrimitive IlInt64 ->
                                        ensureExpected (IlPrimitive IlBool)
                                    | ("<" | "<=" | ">" | ">="), IlPrimitive IlFloat64, IlPrimitive IlFloat64 ->
                                        ensureExpected (IlPrimitive IlBool)
                                    | ("&&" | "||"), IlPrimitive IlBool, IlPrimitive IlBool ->
                                        ensureExpected (IlPrimitive IlBool)
                                    | _ ->
                                        Result.Error ""
                            }

                        builtinResult
                    else
                        inferExpressionType currentModule localTypes active expectedType (KRuntimeApply(KRuntimeName [ operatorName ], [ left; right ]))
                | KRuntimeIfThenElse(condition, whenTrue, whenFalse) ->
                    result {
                        let! conditionType = inferExpressionType currentModule localTypes active None condition

                        if conditionType <> IlPrimitive IlBool then
                            return! Result.Error "IL backend requires Bool conditions for if expressions."

                        let! trueType = inferExpressionType currentModule localTypes active expectedType whenTrue
                        let! falseType = inferExpressionType currentModule localTypes active (Some trueType) whenFalse

                        if trueType <> falseType then
                            return!
                                Result.Error
                                    $"IL backend requires both if branches to have the same type, but saw {formatIlType trueType} and {formatIlType falseType}."

                        return trueType
                    }
                | KRuntimeMatch(scrutinee, cases) ->
                    result {
                        let! scrutineeType = inferExpressionType currentModule localTypes active None scrutinee

                        let! caseTypes =
                            cases
                            |> List.fold
                                (fun stateResult caseClause ->
                                    result {
                                        let! collected = stateResult
                                        let! patternBindings = inferPatternBindings currentModule active scrutineeType caseClause.Pattern
                                        let extendedLocals =
                                            patternBindings
                                            |> Map.fold (fun locals name value -> locals |> Map.add name value) localTypes

                                        let expectedCaseType =
                                            match collected with
                                            | head :: _ -> Some head
                                            | [] -> expectedType

                                        let! caseType =
                                            inferExpressionType currentModule extendedLocals active expectedCaseType caseClause.Body

                                        return caseType :: collected
                                    })
                                (Result.Ok [])

                        match caseTypes with
                        | [] ->
                            return! Result.Error "IL backend requires at least one match case."
                        | head :: tail ->
                            if tail |> List.exists ((<>) head) then
                                return! Result.Error "IL backend requires all match cases to return the same type."

                            return head
                    }
                | KRuntimeApply(KRuntimeName segments, arguments) ->
                    let nameText = String.concat "." segments
                    let builtinOperators =
                        Set.ofList [ "+"; "-"; "*"; "/"; "=="; "!="; "<"; "<="; ">"; ">="; "&&"; "||" ]

                    match segments, arguments with
                    | [ operatorName ], [ left; right ] when builtinOperators.Contains(operatorName) ->
                        inferExpressionType currentModule localTypes active expectedType (KRuntimeBinary(left, operatorName, right))
                    | _ ->
                        match tryResolveBinding rawModules currentModule segments with
                        | Some(targetModule, bindingInfo) ->
                            inferBindingInfo targetModule bindingInfo.Name active
                            |> Result.bind (fun resolvedBinding ->
                                if List.length resolvedBinding.ParameterTypes <> List.length arguments then
                                    Result.Error
                                        $"IL backend expected '{nameText}' to receive {List.length resolvedBinding.ParameterTypes} argument(s), but received {List.length arguments}."
                                else
                                    List.zip arguments resolvedBinding.ParameterTypes
                                    |> List.fold
                                        (fun stateResult (argumentExpression, (_, parameterType)) ->
                                            stateResult
                                            |> Result.bind (fun () ->
                                                inferExpressionType currentModule localTypes active (Some parameterType) argumentExpression
                                                |> Result.map (fun _ -> ())))
                                        (Result.Ok())
                                    |> Result.bind (fun () -> ensureExpected resolvedBinding.ReturnType))
                        | None ->
                            match tryResolveConstructor rawModules currentModule segments with
                            | Some(_, constructorInfo) ->
                                inferConstructorTypeFromArguments inferExpressionType currentModule localTypes active expectedType arguments constructorInfo
                            | None ->
                                Result.Error $"IL backend could not resolve callee '{nameText}'."
                | KRuntimeApply _ ->
                    Result.Error "IL backend currently supports application only when the callee is a named binding."
                | KRuntimeClosure _ ->
                    Result.Error "IL backend does not support closures yet."
                | KRuntimePrefixedString _ ->
                    Result.Error "IL backend does not support prefixed strings yet."

            rawModules
            |> Map.toList
            |> List.fold
                (fun stateResult (moduleName, rawModule) ->
                    result {
                        let! state = stateResult

                        let! bindingEntries =
                            rawModule.Bindings
                            |> Map.toList
                            |> List.fold
                                (fun bindingResult (bindingName, _) ->
                                    result {
                                        let! entriesSoFar = bindingResult
                                        let! bindingInfo = inferBindingInfo moduleName bindingName Set.empty
                                        return (bindingName, bindingInfo) :: entriesSoFar
                                    })
                                (Result.Ok [])

                        let moduleInfo =
                            { Name = rawModule.Name
                              Imports = rawModule.Imports
                              Exports = rawModule.Exports
                              TypeExports = rawModule.TypeExports
                              DataTypes = rawModule.DataTypes
                              Constructors = rawModule.Constructors
                              Bindings = bindingEntries |> Map.ofList
                              EmittedTypeName = rawModule.EmittedTypeName }

                        return state |> Map.add moduleName moduleInfo
                    })
                (Result.Ok Map.empty)
            |> Result.map (fun modules ->
                let allDataTypes =
                    modules
                    |> Map.toList
                    |> List.collect (fun (moduleName, moduleInfo) ->
                        moduleInfo.DataTypes
                        |> Map.toList
                        |> List.map (fun (typeName, dataType) -> (moduleName, typeName), dataType))
                    |> Map.ofList

                { Modules = modules
                  DataTypes = allDataTypes }))

    let private emitLiteral (il: ILGenerator) literal =
        match literal with
        | LiteralValue.Integer value ->
            il.Emit(OpCodes.Ldc_I8, value)
            Result.Ok()
        | LiteralValue.Float value ->
            il.Emit(OpCodes.Ldc_R8, value)
            Result.Ok()
        | LiteralValue.String value ->
            il.Emit(OpCodes.Ldstr, value)
            Result.Ok()
        | LiteralValue.Character value ->
            il.Emit(OpCodes.Ldc_I4, int value)
            Result.Ok()
        | LiteralValue.Unit ->
            Result.Error "IL backend does not emit Unit values yet."

    let private emitComparisonFromCeq (il: ILGenerator) negate =
        il.Emit(OpCodes.Ceq)

        if negate then
            il.Emit(OpCodes.Ldc_I4_0)
            il.Emit(OpCodes.Ceq)

    let rec private resolveClrType (state: EmissionState) (typeParameters: Map<string, Type>) ilType =
        match ilType with
        | IlPrimitive primitiveType ->
            primitiveRuntimeType primitiveType
        | IlTypeParameter name ->
            typeParameters[name]
        | IlNamed(moduleName, typeName, arguments) ->
            let emission = state.DataTypeBuilders[moduleName, typeName]
            let baseType =
                emission.BaseTypeBuilder :> Type

            if List.isEmpty arguments then
                baseType
            else
                let genericArguments =
                    arguments |> List.map (resolveClrType state typeParameters) |> List.toArray

                baseType.MakeGenericType(genericArguments)

    let private resolveConstructorTypeAndMembers (state: EmissionState) (substitution: Map<string, IlType>) (constructorInfo: ConstructorInfo) =
        let emission = state.DataTypeBuilders[constructorInfo.ModuleName, constructorInfo.TypeName].Constructors[constructorInfo.Name]

        if Array.isEmpty emission.GenericParameters then
            let fields = emission.FieldBuilders |> Array.map (fun fieldBuilder -> fieldBuilder :> FieldInfo)
            emission.TypeBuilder :> Type, emission.ConstructorBuilder :> System.Reflection.ConstructorInfo, fields
        else
            let genericArguments =
                constructorInfo.TypeParameters
                |> List.map (fun name -> resolveClrType state Map.empty substitution[name])
                |> List.toArray

            let constructedType = emission.TypeBuilder.MakeGenericType(genericArguments)
            let constructor = TypeBuilder.GetConstructor(constructedType, emission.ConstructorBuilder)
            let fields = emission.FieldBuilders |> Array.map (fun fieldBuilder -> TypeBuilder.GetField(constructedType, fieldBuilder))
            constructedType, constructor, fields

    let private loadValue (il: ILGenerator) localValue =
        match localValue.Location with
        | Argument argumentIndex ->
            il.Emit(OpCodes.Ldarg, int16 argumentIndex)
        | Local localBuilder ->
            il.Emit(OpCodes.Ldloc, localBuilder)

    let private emitLoadLocal (il: ILGenerator) (localBuilder: LocalBuilder) =
        il.Emit(OpCodes.Ldloc, localBuilder)

    let rec private emitExpression (state: EmissionState) currentModule localValues expectedType (il: ILGenerator) expression =
        let localTypes =
            localValues |> Map.map (fun _ value -> value.Type)

        let rec inferExpressionType currentModule localTypes expectedType expression =
            let modules = state.Environment.Modules

            let rec inferPatternBindings expectedType pattern =
                match pattern with
                | KRuntimeWildcardPattern ->
                    Result.Ok(Map.empty<string, IlType>)
                | KRuntimeNamePattern name ->
                    Result.Ok(Map.ofList [ name, expectedType ])
                | KRuntimeLiteralPattern literal ->
                    let literalType =
                        match literal with
                        | LiteralValue.Integer _ -> IlPrimitive IlInt64
                        | LiteralValue.Float _ -> IlPrimitive IlFloat64
                        | LiteralValue.String _ -> IlPrimitive IlString
                        | LiteralValue.Character _ -> IlPrimitive IlChar
                        | LiteralValue.Unit -> IlNamed("std.prelude", "Unit", [])

                    if literalType = expectedType then
                        Result.Ok(Map.empty<string, IlType>)
                    else
                        Result.Error
                            $"IL backend cannot match literal of type {formatIlType literalType} against {formatIlType expectedType}."
                | KRuntimeConstructorPattern(nameSegments, argumentPatterns) ->
                    match tryResolveConstructor modules currentModule nameSegments with
                    | None ->
                        let patternName = String.concat "." nameSegments
                        Result.Error $"IL backend could not resolve constructor pattern '{patternName}'."
                    | Some(_, constructorInfo) ->
                        unifyTypes Map.empty (constructorResultType constructorInfo) expectedType
                        |> Result.bind (fun substitution ->
                            if List.length argumentPatterns <> List.length constructorInfo.FieldTypes then
                                Result.Error
                                    $"IL backend expected pattern '{constructorInfo.Name}' to receive {List.length constructorInfo.FieldTypes} argument(s), but received {List.length argumentPatterns}."
                            else
                                List.zip argumentPatterns constructorInfo.FieldTypes
                                |> List.fold
                                    (fun stateResult (argumentPattern, fieldTemplate) ->
                                        stateResult
                                        |> Result.bind (fun bindings ->
                                            let fieldType = substituteType substitution fieldTemplate

                                            inferPatternBindings fieldType argumentPattern
                                            |> Result.map (fun childBindings ->
                                                Map.fold (fun acc key value -> acc |> Map.add key value) bindings childBindings)))
                                    (Result.Ok(Map.empty<string, IlType>)))

            let rec infer expression expectedType =
                let ensureExpected actualType =
                    match expectedType with
                    | Some expected when expected <> actualType ->
                        Result.Error
                            $"IL backend expected expression of type {formatIlType expected}, but found {formatIlType actualType}."
                    | _ ->
                        Result.Ok actualType

                match expression with
                | KRuntimeLiteral(LiteralValue.Integer _) ->
                    ensureExpected (IlPrimitive IlInt64)
                | KRuntimeLiteral(LiteralValue.Float _) ->
                    ensureExpected (IlPrimitive IlFloat64)
                | KRuntimeLiteral(LiteralValue.String _) ->
                    ensureExpected (IlPrimitive IlString)
                | KRuntimeLiteral(LiteralValue.Character _) ->
                    ensureExpected (IlPrimitive IlChar)
                | KRuntimeLiteral LiteralValue.Unit ->
                    Result.Error "IL backend does not emit Unit values yet."
                | KRuntimeName [ "True" ]
                | KRuntimeName [ "False" ] ->
                    ensureExpected (IlPrimitive IlBool)
                | KRuntimeName [ name ] when localTypes |> Map.containsKey name ->
                    ensureExpected localTypes[name]
                | KRuntimeName segments ->
                    let nameText = String.concat "." segments

                    match tryResolveBinding modules currentModule segments with
                    | Some(_, bindingInfo) when List.isEmpty bindingInfo.ParameterTypes ->
                        ensureExpected bindingInfo.ReturnType
                    | Some(targetModule, bindingInfo) ->
                        Result.Error
                            $"IL backend does not support function-valued name '{targetModule}.{bindingInfo.Binding.Name}' yet."
                    | None ->
                        match tryResolveConstructor modules currentModule segments with
                        | Some(_, constructorInfo) when List.isEmpty constructorInfo.FieldTypes ->
                            inferConstructorTypeFromArguments
                                (fun _ locals _ expected innerExpression ->
                                    inferBody locals expected innerExpression)
                                currentModule
                                localTypes
                                Set.empty
                                expectedType
                                []
                                constructorInfo
                        | Some(targetModule, constructorInfo) ->
                            Result.Error
                                $"IL backend does not support constructor-valued name '{targetModule}.{constructorInfo.Name}' yet."
                        | None ->
                            let localsText =
                                localTypes |> Map.toList |> List.map fst |> String.concat ", "

                            Result.Error
                                $"IL backend could not resolve name '{nameText}'. Locals in scope: [{localsText}]."
                | KRuntimeUnary("-", operand) ->
                    infer operand None
                    |> Result.bind (function
                        | IlPrimitive IlInt64 -> ensureExpected (IlPrimitive IlInt64)
                        | IlPrimitive IlFloat64 -> ensureExpected (IlPrimitive IlFloat64)
                        | other -> Result.Error $"Unary '-' is not supported for {formatIlType other}.")
                | KRuntimeUnary(operatorName, _) ->
                    Result.Error $"IL backend does not support unary operator '{operatorName}' yet."
                | KRuntimeBinary(left, operatorName, right) ->
                    let builtinOperators =
                        Set.ofList [ "+"; "-"; "*"; "/"; "=="; "!="; "<"; "<="; ">"; ">="; "&&"; "||" ]

                    if builtinOperators.Contains(operatorName) then
                        let builtinResult =
                            result {
                                let! leftType = infer left None
                                let! rightType = infer right None

                                return!
                                    match operatorName, leftType, rightType with
                                    | ("+" | "-" | "*" | "/"), IlPrimitive IlInt64, IlPrimitive IlInt64 ->
                                        ensureExpected (IlPrimitive IlInt64)
                                    | ("+" | "-" | "*" | "/"), IlPrimitive IlFloat64, IlPrimitive IlFloat64 ->
                                        ensureExpected (IlPrimitive IlFloat64)
                                    | ("==" | "!="), IlPrimitive IlInt64, IlPrimitive IlInt64 ->
                                        ensureExpected (IlPrimitive IlBool)
                                    | ("==" | "!="), IlPrimitive IlFloat64, IlPrimitive IlFloat64 ->
                                        ensureExpected (IlPrimitive IlBool)
                                    | ("==" | "!="), IlPrimitive IlBool, IlPrimitive IlBool ->
                                        ensureExpected (IlPrimitive IlBool)
                                    | ("==" | "!="), IlPrimitive IlChar, IlPrimitive IlChar ->
                                        ensureExpected (IlPrimitive IlBool)
                                    | ("<" | "<=" | ">" | ">="), IlPrimitive IlInt64, IlPrimitive IlInt64 ->
                                        ensureExpected (IlPrimitive IlBool)
                                    | ("<" | "<=" | ">" | ">="), IlPrimitive IlFloat64, IlPrimitive IlFloat64 ->
                                        ensureExpected (IlPrimitive IlBool)
                                    | ("&&" | "||"), IlPrimitive IlBool, IlPrimitive IlBool ->
                                        ensureExpected (IlPrimitive IlBool)
                                    | _ ->
                                        Result.Error ""
                            }

                        builtinResult
                    else
                        infer (KRuntimeApply(KRuntimeName [ operatorName ], [ left; right ])) expectedType
                | KRuntimeIfThenElse(condition, whenTrue, whenFalse) ->
                    result {
                        let! conditionType = infer condition None

                        if conditionType <> IlPrimitive IlBool then
                            return! Result.Error "IL backend requires Bool conditions for if expressions."

                        let! trueType = infer whenTrue expectedType
                        let! falseType = infer whenFalse (Some trueType)

                        if trueType <> falseType then
                            return!
                                Result.Error
                                    $"IL backend requires both if branches to have the same type, but saw {formatIlType trueType} and {formatIlType falseType}."

                        return trueType
                    }
                | KRuntimeMatch(scrutinee, cases) ->
                    result {
                        let! scrutineeType = infer scrutinee None

                        let! caseTypes =
                            cases
                            |> List.fold
                                (fun stateResult caseClause ->
                                    result {
                                        let! collected = stateResult
                                        let! patternBindings = inferPatternBindings scrutineeType caseClause.Pattern

                                        let extendedLocals =
                                            patternBindings
                                            |> Map.fold (fun locals name value -> locals |> Map.add name value) localTypes

                                        let expectedCaseType =
                                            match collected with
                                            | head :: _ -> Some head
                                            | [] -> expectedType

                                        let! caseType = inferBody extendedLocals expectedCaseType caseClause.Body
                                        return caseType :: collected
                                    })
                                (Result.Ok [])

                        match caseTypes with
                        | [] ->
                            return! Result.Error "IL backend requires at least one match case."
                        | head :: tail ->
                            if tail |> List.exists ((<>) head) then
                                return! Result.Error "IL backend requires all match cases to return the same type."

                            return head
                    }
                | KRuntimeApply(KRuntimeName segments, arguments) ->
                    let nameText = String.concat "." segments
                    let builtinOperators =
                        Set.ofList [ "+"; "-"; "*"; "/"; "=="; "!="; "<"; "<="; ">"; ">="; "&&"; "||" ]

                    match segments, arguments with
                    | [ operatorName ], [ left; right ] when builtinOperators.Contains(operatorName) ->
                        infer (KRuntimeBinary(left, operatorName, right)) expectedType
                    | _ ->
                        match tryResolveBinding modules currentModule segments with
                        | Some(_, bindingInfo) ->
                            if List.length bindingInfo.ParameterTypes <> List.length arguments then
                                Result.Error
                                    $"IL backend expected '{nameText}' to receive {List.length bindingInfo.ParameterTypes} argument(s), but received {List.length arguments}."
                            else
                                List.zip arguments bindingInfo.ParameterTypes
                                |> List.fold
                                    (fun stateResult (argumentExpression, (_, parameterType)) ->
                                        stateResult
                                        |> Result.bind (fun () ->
                                            infer argumentExpression (Some parameterType) |> Result.map (fun _ -> ())))
                                    (Result.Ok())
                                |> Result.bind (fun () -> ensureExpected bindingInfo.ReturnType)
                        | None ->
                            match tryResolveConstructor modules currentModule segments with
                            | Some(_, constructorInfo) ->
                                inferConstructorTypeFromArguments
                                    (fun _ locals _ expected innerExpression ->
                                        inferBody locals expected innerExpression)
                                    currentModule
                                    localTypes
                                    Set.empty
                                    expectedType
                                    arguments
                                    constructorInfo
                            | None ->
                                Result.Error $"IL backend could not resolve callee '{nameText}'."
                | KRuntimeApply _ ->
                    Result.Error "IL backend currently supports application only when the callee is a named binding."
                | KRuntimeClosure _ ->
                    Result.Error "IL backend does not support closures yet."
                | KRuntimePrefixedString _ ->
                    Result.Error "IL backend does not support prefixed strings yet."

            and inferBody bodyLocals expectedType bodyExpression =
                inferExpressionType currentModule bodyLocals expectedType bodyExpression

            infer expression expectedType

        let resultType =
            inferExpressionType currentModule localTypes expectedType expression

        let rec emitConstructorApplication constructorInfo arguments expectedType =
            result {
                let initialSubstitutionResult =
                    match expectedType with
                    | Some expected ->
                        unifyTypes Map.empty (constructorResultType constructorInfo) expected
                    | None ->
                        Result.Ok Map.empty

                let! substitution, argumentTypes =
                    if List.length arguments <> List.length constructorInfo.FieldTypes then
                        Result.Error
                            $"IL backend expected constructor '{constructorInfo.Name}' to receive {List.length constructorInfo.FieldTypes} argument(s), but received {List.length arguments}."
                    else
                        List.zip arguments constructorInfo.FieldTypes
                        |> List.fold
                            (fun stateResult (argumentExpression, argumentTemplate) ->
                                result {
                                    let! substitution, collectedArgumentTypes = stateResult

                                    let expectedArgumentType =
                                        let specialized = substituteType substitution argumentTemplate

                                        if containsTypeParameters specialized then
                                            None
                                        else
                                            Some specialized

                                    let! argumentType =
                                        inferExpressionType currentModule localTypes expectedArgumentType argumentExpression

                                    let! nextSubstitution =
                                        unifyTypes substitution argumentTemplate argumentType

                                    return nextSubstitution, argumentType :: collectedArgumentTypes
                                })
                            (initialSubstitutionResult |> Result.map (fun substitution -> substitution, []))
                        |> Result.map (fun (substitution, reversedArgumentTypes) -> substitution, List.rev reversedArgumentTypes)

                let resultType = substituteType substitution (constructorResultType constructorInfo)

                if containsTypeParameters resultType then
                    return!
                        Result.Error
                            $"IL backend could not infer concrete type arguments for constructor '{constructorInfo.Name}'."

                let _, constructor, _ = resolveConstructorTypeAndMembers state substitution constructorInfo

                do!
                    List.zip arguments argumentTypes
                    |> List.fold
                        (fun stateResult (argumentExpression, argumentType) ->
                            stateResult
                            |> Result.bind (fun () ->
                                emitExpression state currentModule localValues (Some argumentType) il argumentExpression))
                        (Result.Ok())

                il.Emit(OpCodes.Newobj, constructor)
            }

        let emitBuiltinBinary operatorName left right =
            result {
                let! leftType = inferExpressionType currentModule localTypes None left
                let! rightType = inferExpressionType currentModule localTypes None right
                do! emitExpression state currentModule localValues (Some leftType) il left
                do! emitExpression state currentModule localValues (Some rightType) il right

                match operatorName, leftType, rightType with
                | "+", IlPrimitive IlInt64, IlPrimitive IlInt64 ->
                    il.Emit(OpCodes.Add)
                | "-", IlPrimitive IlInt64, IlPrimitive IlInt64 ->
                    il.Emit(OpCodes.Sub)
                | "*", IlPrimitive IlInt64, IlPrimitive IlInt64 ->
                    il.Emit(OpCodes.Mul)
                | "/", IlPrimitive IlInt64, IlPrimitive IlInt64 ->
                    il.Emit(OpCodes.Div)
                | "+", IlPrimitive IlFloat64, IlPrimitive IlFloat64 ->
                    il.Emit(OpCodes.Add)
                | "-", IlPrimitive IlFloat64, IlPrimitive IlFloat64 ->
                    il.Emit(OpCodes.Sub)
                | "*", IlPrimitive IlFloat64, IlPrimitive IlFloat64 ->
                    il.Emit(OpCodes.Mul)
                | "/", IlPrimitive IlFloat64, IlPrimitive IlFloat64 ->
                    il.Emit(OpCodes.Div)
                | "==", IlPrimitive IlInt64, IlPrimitive IlInt64 ->
                    emitComparisonFromCeq il false
                | "!=", IlPrimitive IlInt64, IlPrimitive IlInt64 ->
                    emitComparisonFromCeq il true
                | "==", IlPrimitive IlFloat64, IlPrimitive IlFloat64 ->
                    emitComparisonFromCeq il false
                | "!=", IlPrimitive IlFloat64, IlPrimitive IlFloat64 ->
                    emitComparisonFromCeq il true
                | "==", IlPrimitive IlBool, IlPrimitive IlBool ->
                    emitComparisonFromCeq il false
                | "!=", IlPrimitive IlBool, IlPrimitive IlBool ->
                    emitComparisonFromCeq il true
                | "==", IlPrimitive IlChar, IlPrimitive IlChar ->
                    emitComparisonFromCeq il false
                | "!=", IlPrimitive IlChar, IlPrimitive IlChar ->
                    emitComparisonFromCeq il true
                | "<", IlPrimitive IlInt64, IlPrimitive IlInt64 ->
                    il.Emit(OpCodes.Clt)
                | "<=", IlPrimitive IlInt64, IlPrimitive IlInt64 ->
                    il.Emit(OpCodes.Cgt)
                    il.Emit(OpCodes.Ldc_I4_0)
                    il.Emit(OpCodes.Ceq)
                | ">", IlPrimitive IlInt64, IlPrimitive IlInt64 ->
                    il.Emit(OpCodes.Cgt)
                | ">=", IlPrimitive IlInt64, IlPrimitive IlInt64 ->
                    il.Emit(OpCodes.Clt)
                    il.Emit(OpCodes.Ldc_I4_0)
                    il.Emit(OpCodes.Ceq)
                | "<", IlPrimitive IlFloat64, IlPrimitive IlFloat64 ->
                    il.Emit(OpCodes.Clt)
                | "<=", IlPrimitive IlFloat64, IlPrimitive IlFloat64 ->
                    il.Emit(OpCodes.Cgt)
                    il.Emit(OpCodes.Ldc_I4_0)
                    il.Emit(OpCodes.Ceq)
                | ">", IlPrimitive IlFloat64, IlPrimitive IlFloat64 ->
                    il.Emit(OpCodes.Cgt)
                | ">=", IlPrimitive IlFloat64, IlPrimitive IlFloat64 ->
                    il.Emit(OpCodes.Clt)
                    il.Emit(OpCodes.Ldc_I4_0)
                    il.Emit(OpCodes.Ceq)
                | _ ->
                    return! Result.Error $"IL backend does not support '{operatorName}' for {formatIlType leftType} and {formatIlType rightType}."
            }

        let rec emitPatternMatch currentScope expectedType pattern valueLocal failureLabel =
            match pattern with
            | KRuntimeWildcardPattern ->
                Result.Ok currentScope
            | KRuntimeNamePattern name ->
                Result.Ok(currentScope |> Map.add name { Location = Local valueLocal; Type = expectedType })
            | KRuntimeLiteralPattern literal ->
                result {
                    emitLoadLocal il valueLocal
                    do! emitLiteral il literal
                    emitComparisonFromCeq il false
                    il.Emit(OpCodes.Brfalse, (failureLabel: Label))
                    return currentScope
                }
            | KRuntimeConstructorPattern(nameSegments, argumentPatterns) ->
                match tryResolveConstructor state.Environment.Modules currentModule nameSegments with
                | None ->
                    let patternName = String.concat "." nameSegments
                    Result.Error $"IL backend could not resolve constructor pattern '{patternName}'."
                | Some(_, constructorInfo) ->
                    unifyTypes Map.empty (constructorResultType constructorInfo) expectedType
                    |> Result.bind (fun substitution ->
                        if List.length argumentPatterns <> List.length constructorInfo.FieldTypes then
                            Result.Error
                                $"IL backend expected pattern '{constructorInfo.Name}' to receive {List.length constructorInfo.FieldTypes} argument(s), but received {List.length argumentPatterns}."
                        else
                            let constructorType, _, fieldInfos = resolveConstructorTypeAndMembers state substitution constructorInfo
                            let castLocal = il.DeclareLocal(constructorType)
                            emitLoadLocal il valueLocal
                            il.Emit(OpCodes.Isinst, constructorType)
                            il.Emit(OpCodes.Stloc, castLocal)
                            il.Emit(OpCodes.Ldloc, castLocal)
                            il.Emit(OpCodes.Brfalse, (failureLabel: Label))

                            List.zip3 argumentPatterns constructorInfo.FieldTypes (fieldInfos |> Array.toList)
                            |> List.fold
                                (fun stateResult (argumentPattern, fieldTemplate, fieldInfo) ->
                                    stateResult
                                    |> Result.bind (fun scope ->
                                        let fieldType = substituteType substitution fieldTemplate
                                        let fieldLocal = il.DeclareLocal(resolveClrType state Map.empty fieldType)
                                        il.Emit(OpCodes.Ldloc, castLocal)
                                        il.Emit(OpCodes.Ldfld, (fieldInfo: FieldInfo))
                                        il.Emit(OpCodes.Stloc, fieldLocal)
                                        emitPatternMatch scope fieldType argumentPattern fieldLocal failureLabel))
                                (Result.Ok currentScope))

        let emitMatch (scrutinee: KRuntimeExpression) (cases: KRuntimeMatchCase list) =
            result {
                let! scrutineeType = inferExpressionType currentModule localTypes None scrutinee
                let! matchType = inferExpressionType currentModule localTypes expectedType (KRuntimeMatch(scrutinee, cases))

                let scrutineeLocal = il.DeclareLocal(resolveClrType state Map.empty scrutineeType)
                let resultLocal = il.DeclareLocal(resolveClrType state Map.empty matchType)
                let endLabel = il.DefineLabel()

                do! emitExpression state currentModule localValues (Some scrutineeType) il scrutinee
                il.Emit(OpCodes.Stloc, scrutineeLocal)

                let rec emitCases (remainingCases: KRuntimeMatchCase list) =
                    match remainingCases with
                    | [] ->
                        let exceptionCtor = typeof<InvalidOperationException>.GetConstructor([| typeof<string> |])
                        il.Emit(OpCodes.Ldstr, "Non-exhaustive match.")
                        il.Emit(OpCodes.Newobj, exceptionCtor)
                        il.Emit(OpCodes.Throw)
                        Result.Ok()
                    | (caseClause: KRuntimeMatchCase) :: rest ->
                        let nextCaseLabel = il.DefineLabel()

                        emitPatternMatch localValues scrutineeType caseClause.Pattern scrutineeLocal nextCaseLabel
                        |> Result.bind (fun caseScope ->
                            emitExpression state currentModule caseScope (Some matchType) il caseClause.Body
                            |> Result.map (fun () ->
                                il.Emit(OpCodes.Stloc, resultLocal)
                                il.Emit(OpCodes.Br, endLabel)
                                il.MarkLabel(nextCaseLabel)))
                        |> Result.bind (fun () -> emitCases rest)

                do! emitCases cases
                il.MarkLabel(endLabel)
                il.Emit(OpCodes.Ldloc, resultLocal)
            }

        match expression with
        | KRuntimeLiteral literal ->
            emitLiteral il literal
        | KRuntimeName [ "True" ] ->
            il.Emit(OpCodes.Ldc_I4_1)
            Result.Ok()
        | KRuntimeName [ "False" ] ->
            il.Emit(OpCodes.Ldc_I4_0)
            Result.Ok()
        | KRuntimeName [ name ] when localValues |> Map.containsKey name ->
            loadValue il localValues[name]
            Result.Ok()
        | KRuntimeName segments ->
            let nameText = String.concat "." segments

            match tryResolveBinding state.Environment.Modules currentModule segments with
            | Some(targetModule, bindingInfo) when List.isEmpty bindingInfo.ParameterTypes ->
                let targetMethod = state.MethodBuilders[targetModule][bindingInfo.Binding.Name]
                il.Emit(OpCodes.Call, targetMethod)
                Result.Ok()
            | Some(targetModule, bindingInfo) ->
                Result.Error $"IL backend does not support function-valued name '{targetModule}.{bindingInfo.Binding.Name}' yet."
            | None ->
                match tryResolveConstructor state.Environment.Modules currentModule segments with
                | Some(_, constructorInfo) when List.isEmpty constructorInfo.FieldTypes ->
                    emitConstructorApplication constructorInfo [] expectedType
                | Some(targetModule, constructorInfo) ->
                    Result.Error $"IL backend does not support constructor-valued name '{targetModule}.{constructorInfo.Name}' yet."
                | None ->
                    let localsText =
                        localValues |> Map.toList |> List.map fst |> String.concat ", "

                    Result.Error $"IL backend could not resolve name '{nameText}'. Locals in scope: [{localsText}]."
        | KRuntimeUnary("-", operand) ->
            result {
                let! operandType = inferExpressionType currentModule localTypes None operand
                do! emitExpression state currentModule localValues (Some operandType) il operand

                match operandType with
                | IlPrimitive IlInt64
                | IlPrimitive IlFloat64 ->
                    il.Emit(OpCodes.Neg)
                | other ->
                    return! Result.Error $"Unary '-' is not supported for {formatIlType other}."
            }
        | KRuntimeUnary(operatorName, _) ->
            Result.Error $"IL backend does not support unary operator '{operatorName}' yet."
        | KRuntimeBinary(left, "&&", right) ->
            let falseLabel = il.DefineLabel()
            let endLabel = il.DefineLabel()

            result {
                do! emitExpression state currentModule localValues (Some(IlPrimitive IlBool)) il left
                il.Emit(OpCodes.Brfalse, falseLabel)
                do! emitExpression state currentModule localValues (Some(IlPrimitive IlBool)) il right
                il.Emit(OpCodes.Br, endLabel)
                il.MarkLabel(falseLabel)
                il.Emit(OpCodes.Ldc_I4_0)
                il.MarkLabel(endLabel)
            }
        | KRuntimeBinary(left, "||", right) ->
            let trueLabel = il.DefineLabel()
            let endLabel = il.DefineLabel()

            result {
                do! emitExpression state currentModule localValues (Some(IlPrimitive IlBool)) il left
                il.Emit(OpCodes.Brtrue, trueLabel)
                do! emitExpression state currentModule localValues (Some(IlPrimitive IlBool)) il right
                il.Emit(OpCodes.Br, endLabel)
                il.MarkLabel(trueLabel)
                il.Emit(OpCodes.Ldc_I4_1)
                il.MarkLabel(endLabel)
            }
        | KRuntimeBinary(left, operatorName, right) ->
            let supportedBuiltinOperators =
                Set.ofList [ "+"; "-"; "*"; "/"; "=="; "!="; "<"; "<="; ">"; ">=" ]

            if supportedBuiltinOperators.Contains(operatorName) then
                emitBuiltinBinary operatorName left right
            else
                emitExpression
                    state
                    currentModule
                    localValues
                    expectedType
                    il
                    (KRuntimeApply(KRuntimeName [ operatorName ], [ left; right ]))
        | KRuntimeIfThenElse(condition, whenTrue, whenFalse) ->
            result {
                let! resultType = inferExpressionType currentModule localTypes expectedType expression
                let falseLabel = il.DefineLabel()
                let endLabel = il.DefineLabel()

                do! emitExpression state currentModule localValues (Some(IlPrimitive IlBool)) il condition
                il.Emit(OpCodes.Brfalse, falseLabel)
                do! emitExpression state currentModule localValues (Some resultType) il whenTrue
                il.Emit(OpCodes.Br, endLabel)
                il.MarkLabel(falseLabel)
                do! emitExpression state currentModule localValues (Some resultType) il whenFalse
                il.MarkLabel(endLabel)
            }
        | KRuntimeMatch(scrutinee, cases) ->
            emitMatch scrutinee cases
        | KRuntimeApply(KRuntimeName segments, arguments) ->
            let nameText = String.concat "." segments
            let builtinOperators =
                Set.ofList [ "+"; "-"; "*"; "/"; "=="; "!="; "<"; "<="; ">"; ">="; "&&"; "||" ]

            match segments, arguments with
            | [ operatorName ], [ left; right ] when builtinOperators.Contains(operatorName) ->
                emitExpression state currentModule localValues expectedType il (KRuntimeBinary(left, operatorName, right))
            | _ ->
                match tryResolveBinding state.Environment.Modules currentModule segments with
                | Some(targetModule, bindingInfo) ->
                    if List.length bindingInfo.ParameterTypes <> List.length arguments then
                        Result.Error
                            $"IL backend expected '{nameText}' to receive {List.length bindingInfo.ParameterTypes} argument(s), but received {List.length arguments}."
                    else
                        List.zip arguments bindingInfo.ParameterTypes
                        |> List.fold
                            (fun stateResult (argumentExpression, (_, parameterType)) ->
                                stateResult
                                |> Result.bind (fun () ->
                                    emitExpression state currentModule localValues (Some parameterType) il argumentExpression))
                            (Result.Ok())
                        |> Result.map (fun () ->
                            let targetMethod = state.MethodBuilders[targetModule][bindingInfo.Binding.Name]
                            il.Emit(OpCodes.Call, targetMethod))
                | None ->
                    match tryResolveConstructor state.Environment.Modules currentModule segments with
                    | Some(_, constructorInfo) ->
                        emitConstructorApplication constructorInfo arguments expectedType
                    | None ->
                        Result.Error $"IL backend could not resolve callee '{nameText}'."
        | KRuntimeApply _ ->
            Result.Error "IL backend currently supports application only when the callee is a named binding."
        | KRuntimeClosure _ ->
            Result.Error "IL backend does not support closures yet."
        | KRuntimePrefixedString _ ->
            Result.Error "IL backend does not support prefixed strings yet."

    let private emitMethodBody (state: EmissionState) (moduleInfo: ModuleInfo) (bindingInfo: BindingInfo) =
        result {
            let methodBuilder = state.MethodBuilders[moduleInfo.Name][bindingInfo.Binding.Name]
            let il = methodBuilder.GetILGenerator()

            let localValues =
                bindingInfo.ParameterTypes
                |> List.mapi (fun index (name, parameterType) ->
                    name,
                    { Location = Argument index
                      Type = parameterType })
                |> Map.ofList

            match bindingInfo.Binding.Body with
            | None ->
                return! Result.Error $"IL backend requires a body for '{moduleInfo.Name}.{bindingInfo.Binding.Name}'."
            | Some body ->
                do! emitExpression state moduleInfo.Name localValues (Some bindingInfo.ReturnType) il body
                il.Emit(OpCodes.Ret)
        }

    let private defineDataTypes (moduleBuilder: ModuleBuilder) (environment: EmissionEnvironment) =
        let baseTypeDefinitions =
            environment.DataTypes
            |> Map.toList
            |> List.map (fun ((moduleName, typeName), dataTypeInfo) ->
                let baseTypeBuilder =
                    moduleBuilder.DefineType(
                        dataTypeInfo.EmittedTypeName,
                        TypeAttributes.Public ||| TypeAttributes.Abstract ||| TypeAttributes.Class
                    )

                let genericParameters =
                    match dataTypeInfo.TypeParameters with
                    | [] -> [||]
                    | parameters -> baseTypeBuilder.DefineGenericParameters(parameters |> List.toArray)

                let baseConstructor =
                    baseTypeBuilder.DefineDefaultConstructor(MethodAttributes.Family)

                ((moduleName, typeName),
                 { BaseTypeBuilder = baseTypeBuilder
                   GenericParameters = genericParameters
                   BaseConstructor = baseConstructor
                   Constructors = Map.empty }))
            |> Map.ofList

        baseTypeDefinitions
        |> Map.map (fun (moduleName, typeName) dataEmission ->
            let dataTypeInfo = environment.DataTypes[moduleName, typeName]

            let genericTypeParameterMap =
                List.zip dataTypeInfo.TypeParameters (dataEmission.GenericParameters |> Array.toList)
                |> List.map (fun (name, parameter) -> name, (parameter :> Type))
                |> Map.ofList

            let constructors =
                dataTypeInfo.Constructors
                |> Map.toList
                |> List.map (fun (constructorName, constructorInfo) ->
                    let constructorTypeBuilder =
                        moduleBuilder.DefineType(
                            constructorInfo.EmittedTypeName,
                            TypeAttributes.Public ||| TypeAttributes.Sealed ||| TypeAttributes.Class
                        )

                    let constructorGenericParameters =
                        match constructorInfo.TypeParameters with
                        | [] -> [||]
                        | parameters -> constructorTypeBuilder.DefineGenericParameters(parameters |> List.toArray)

                    let baseType =
                        if Array.isEmpty constructorGenericParameters then
                            dataEmission.BaseTypeBuilder :> Type
                        else
                            let baseArguments =
                                constructorGenericParameters |> Array.map (fun parameter -> parameter :> Type)

                            (dataEmission.BaseTypeBuilder :> Type).MakeGenericType(baseArguments)

                    constructorTypeBuilder.SetParent(baseType)

                    let fieldBuilders =
                        constructorInfo.FieldTypes
                        |> List.mapi (fun index fieldType ->
                            constructorTypeBuilder.DefineField(
                                $"Item{index + 1}",
                                resolveClrType
                                    { Environment = environment
                                      ModuleBuilders = Map.empty
                                      MethodBuilders = Map.empty
                                      DataTypeBuilders = baseTypeDefinitions }
                                    (if Array.isEmpty constructorGenericParameters then genericTypeParameterMap else
                                        List.zip constructorInfo.TypeParameters (constructorGenericParameters |> Array.toList)
                                        |> List.map (fun (name, parameter) -> name, (parameter :> Type))
                                        |> Map.ofList)
                                    fieldType,
                                FieldAttributes.Public ||| FieldAttributes.InitOnly
                            ))
                        |> List.toArray

                    let constructorParameterTypes =
                        constructorInfo.FieldTypes
                        |> List.map (resolveClrType
                            { Environment = environment
                              ModuleBuilders = Map.empty
                              MethodBuilders = Map.empty
                              DataTypeBuilders = baseTypeDefinitions }
                            (if Array.isEmpty constructorGenericParameters then genericTypeParameterMap else
                                List.zip constructorInfo.TypeParameters (constructorGenericParameters |> Array.toList)
                                |> List.map (fun (name, parameter) -> name, (parameter :> Type))
                                |> Map.ofList))
                        |> List.toArray

                    let constructorBuilder =
                        constructorTypeBuilder.DefineConstructor(
                            MethodAttributes.Public,
                            CallingConventions.Standard,
                            constructorParameterTypes
                        )

                    constructorInfo.FieldTypes
                    |> List.iteri (fun index _ ->
                        constructorBuilder.DefineParameter(index + 1, ParameterAttributes.None, $"item{index + 1}")
                        |> ignore)

                    let constructorIl = constructorBuilder.GetILGenerator()
                    constructorIl.Emit(OpCodes.Ldarg_0)

                    let baseConstructor =
                        if Array.isEmpty constructorGenericParameters then
                            dataEmission.BaseConstructor :> System.Reflection.ConstructorInfo
                        else
                            TypeBuilder.GetConstructor(baseType, dataEmission.BaseConstructor)

                    constructorIl.Emit(OpCodes.Call, baseConstructor)

                    fieldBuilders
                    |> Array.iteri (fun index fieldBuilder ->
                        constructorIl.Emit(OpCodes.Ldarg_0)
                        constructorIl.Emit(OpCodes.Ldarg, index + 1)
                        constructorIl.Emit(OpCodes.Stfld, fieldBuilder))

                    constructorIl.Emit(OpCodes.Ret)

                    constructorName,
                    { TypeBuilder = constructorTypeBuilder
                      GenericParameters = constructorGenericParameters
                      ConstructorBuilder = constructorBuilder
                      FieldBuilders = fieldBuilders })
                |> Map.ofList

            { dataEmission with Constructors = constructors })

    let emitAssemblyArtifact (workspace: WorkspaceCompilation) (outputDirectory: string) =
        if workspace.HasErrors then
            Result.Error $"Cannot emit a CLR assembly for a workspace with diagnostics:{Environment.NewLine}{aggregateDiagnostics workspace.Diagnostics}"
        else
            let verificationDiagnostics = Compilation.verifyCheckpoint workspace "KRuntimeIR"

            if not (List.isEmpty verificationDiagnostics) then
                Result.Error $"Cannot emit malformed KRuntimeIR:{Environment.NewLine}{aggregateDiagnostics verificationDiagnostics}"
            else
                match buildEnvironment workspace with
                | Result.Error message ->
                    Result.Error message
                | Result.Ok environment ->
                    try
                        let resolvedOutputDirectory = Path.GetFullPath(outputDirectory)
                        Directory.CreateDirectory(resolvedOutputDirectory) |> ignore

                        let assemblyName =
                            "Kappa.Generated."
                            + sanitizeIdentifier(Path.GetFileName(resolvedOutputDirectory))

                        let assemblyPath = Path.Combine(resolvedOutputDirectory, $"{assemblyName}.dll")
                        let assemblyBuilder = PersistedAssemblyBuilder(AssemblyName(assemblyName), typeof<obj>.Assembly)
                        let moduleBuilder = assemblyBuilder.DefineDynamicModule(assemblyName)

                        let moduleBuilders =
                            environment.Modules
                            |> Map.toList
                            |> List.map (fun (moduleName, moduleInfo) ->
                                let moduleTypeBuilder =
                                    moduleBuilder.DefineType(
                                        moduleInfo.EmittedTypeName.Value,
                                        TypeAttributes.Public ||| TypeAttributes.Abstract ||| TypeAttributes.Sealed ||| TypeAttributes.Class
                                    )

                                moduleName, moduleTypeBuilder)
                            |> Map.ofList

                        let dataTypeBuilders = defineDataTypes moduleBuilder environment

                        let methodBuilders =
                            environment.Modules
                            |> Map.toList
                            |> List.map (fun (moduleName, moduleInfo) ->
                                let methods =
                                    moduleInfo.Bindings
                                    |> Map.toList
                                    |> List.map (fun (bindingName, bindingInfo) ->
                                        let parameterTypes =
                                            bindingInfo.ParameterTypes
                                            |> List.map (snd >> resolveClrType
                                                { Environment = environment
                                                  ModuleBuilders = moduleBuilders
                                                  MethodBuilders = Map.empty
                                                  DataTypeBuilders = dataTypeBuilders }
                                                Map.empty)
                                            |> List.toArray

                                        let methodBuilder =
                                            moduleBuilders[moduleName].DefineMethod(
                                                bindingInfo.EmittedMethodName,
                                                MethodAttributes.Public ||| MethodAttributes.Static ||| MethodAttributes.HideBySig,
                                                resolveClrType
                                                    { Environment = environment
                                                      ModuleBuilders = moduleBuilders
                                                      MethodBuilders = Map.empty
                                                      DataTypeBuilders = dataTypeBuilders }
                                                    Map.empty
                                                    bindingInfo.ReturnType,
                                                parameterTypes
                                            )

                                        bindingInfo.ParameterTypes
                                        |> List.iteri (fun index (parameterName, _) ->
                                            methodBuilder.DefineParameter(index + 1, ParameterAttributes.None, parameterName)
                                            |> ignore)

                                        bindingName, methodBuilder)
                                    |> Map.ofList

                                moduleName, methods)
                            |> Map.ofList

                        let state =
                            { Environment = environment
                              ModuleBuilders = moduleBuilders
                              MethodBuilders = methodBuilders
                              DataTypeBuilders = dataTypeBuilders }

                        let emissionResult =
                            environment.Modules
                            |> Map.toList
                            |> List.map snd
                            |> List.collect (fun moduleInfo -> moduleInfo.Bindings |> Map.toList |> List.map (fun (_, bindingInfo) -> moduleInfo, bindingInfo))
                            |> List.fold
                                (fun stateResult (moduleInfo, bindingInfo) ->
                                    result {
                                        do! stateResult
                                        do! emitMethodBody state moduleInfo bindingInfo
                                    })
                                (Result.Ok())

                        match emissionResult with
                        | Result.Error message ->
                            Result.Error message
                        | Result.Ok() ->
                            dataTypeBuilders
                            |> Map.iter (fun _ dataEmission -> dataEmission.BaseTypeBuilder.CreateType() |> ignore)

                            dataTypeBuilders
                            |> Map.iter (fun _ dataEmission ->
                                dataEmission.Constructors
                                |> Map.iter (fun _ constructorEmission -> constructorEmission.TypeBuilder.CreateType() |> ignore))

                            moduleBuilders |> Map.iter (fun _ moduleTypeBuilder -> moduleTypeBuilder.CreateType() |> ignore)
                            assemblyBuilder.Save(assemblyPath)

                            Result.Ok
                                { OutputDirectory = resolvedOutputDirectory
                                  AssemblyName = assemblyName
                                  AssemblyFilePath = assemblyPath }
                    with ex ->
                        Result.Error $"IL backend emission failed: {ex.Message}"
