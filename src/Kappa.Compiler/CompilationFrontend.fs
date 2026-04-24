namespace Kappa.Compiler

open System
open System.Collections.Generic
open System.IO

// Builds frontend documents and phase snapshots from source text and bundled imports.
module internal CompilationFrontend =
    type private ModuleExportInventory =
        { Terms: Set<string>
          Types: Set<string>
          Traits: Set<string>
          Constructors: Set<string>
          UnqualifiedBindings: Set<string> }

    let parseBundledPrelude allowUnsafeConsume =
        let source =
            SourceText.From(Stdlib.BundledPreludeVirtualPath, Stdlib.loadBundledPreludeTextForOptions allowUnsafeConsume)
        let lexed = Lexer.tokenize source
        let parsed = Parser.parse source lexed.Tokens

        { Source = source
          InferredModuleName = Some Stdlib.PreludeModuleName
          Syntax = parsed.Syntax
          Diagnostics = lexed.Diagnostics @ parsed.Diagnostics }

    let private isValidModuleSegment (segment: string) =
        not (String.IsNullOrWhiteSpace(segment))
        && SyntaxFacts.isIdentifierStart segment[0]
        && segment |> Seq.forall SyntaxFacts.isIdentifierPart

    let tryInferModuleName (fileSystem: IFileSystem) sourceRoot filePath =
        let fullRoot = fileSystem.GetFullPath(sourceRoot)
        let fullPath = fileSystem.GetFullPath(filePath)

        if not (fullPath.EndsWith(".kp", StringComparison.OrdinalIgnoreCase)) then
            None
        else
            let relativePath = Path.GetRelativePath(fullRoot, fullPath)

            if relativePath.StartsWith("..", StringComparison.Ordinal) then
                None
            else
                let withoutExtension = Path.ChangeExtension(relativePath, null)

                withoutExtension.Split([| '\\'; '/' |], StringSplitOptions.RemoveEmptyEntries)
                |> Array.toList
                |> fun segments ->
                    if segments |> List.forall isValidModuleSegment then Some segments else None

    let private readSource (fileSystem: IFileSystem) filePath =
        let fullPath = fileSystem.GetFullPath(filePath)
        SourceText.From(fullPath, fileSystem.ReadAllText(fullPath))

    let private validateModuleName (options: CompilationOptions) (document: ParsedDocument) =
        let diagnostics = DiagnosticBag()

        match document.InferredModuleName, document.Syntax.ModuleHeader with
        | Some inferred, Some declared when options.PackageMode && inferred <> declared ->
            diagnostics.AddError(
                DiagnosticCode.ModulePathMismatch,
                $"Module header '{SyntaxFacts.moduleNameToText declared}' does not match the path-derived module name '{SyntaxFacts.moduleNameToText inferred}'.",
                document.Source.GetLocation(TextSpan.FromBounds(0, 0)),
                stage = "KFrontIR",
                phase = KFrontIRPhase.phaseName CHECKERS
            )
        | None, None ->
            diagnostics.AddError(
                DiagnosticCode.ModuleNameUnresolved,
                $"Could not derive a Kappa module name from '{document.Source.FilePath}'. Files must live under the source root and end in .kp.",
                document.Source.GetLocation(TextSpan.FromBounds(0, 0)),
                stage = "KFrontIR",
                phase = KFrontIRPhase.phaseName CHECKERS
            )
        | _ ->
            ()

        diagnostics.Items

    let parseFile (options: CompilationOptions) filePath =
        let initialFixities = Parser.bootstrapFixities ()
        let source = readSource options.FileSystem filePath
        let inferredModuleName = tryInferModuleName options.FileSystem options.SourceRoot source.FilePath
        let lexed = Lexer.tokenize source
        let parsed = Parser.parseWithInitialFixities initialFixities source lexed.Tokens

        let document =
            { Source = source
              InferredModuleName = inferredModuleName
              Syntax = parsed.Syntax
              Diagnostics = lexed.Diagnostics @ parsed.Diagnostics }

        { document with
            Diagnostics = document.Diagnostics @ validateModuleName options document }

    let private parseFileWithFixities (options: CompilationOptions) initialFixities filePath =
        let source = readSource options.FileSystem filePath
        let inferredModuleName = tryInferModuleName options.FileSystem options.SourceRoot source.FilePath
        let lexed = Lexer.tokenize source
        let parsed = Parser.parseWithInitialFixities initialFixities source lexed.Tokens

        let document =
            { Source = source
              InferredModuleName = inferredModuleName
              Syntax = parsed.Syntax
              Diagnostics = lexed.Diagnostics @ parsed.Diagnostics }

        { document with
            Diagnostics = document.Diagnostics @ validateModuleName options document }

    let collectInputFiles (options: CompilationOptions) inputs =
        let roots =
            if List.isEmpty inputs then
                [ options.SourceRoot ]
            else
                inputs

        roots
        |> List.collect (fun path ->
            let fullPath = options.FileSystem.GetFullPath(path)

            if options.FileSystem.FileExists(fullPath) then
                [ fullPath ]
            elif options.FileSystem.DirectoryExists(fullPath) then
                options.FileSystem.EnumerateFiles(fullPath, "*.kp", SearchOption.AllDirectories)
                |> Seq.toList
            else
                [])
        |> List.distinct
        |> List.sort

    let collectImportSpecs (document: ParsedDocument) =
        Stdlib.implicitImportsFor document.ModuleName
        @ (document.Syntax.Declarations
        |> List.collect (function
            | ImportDeclaration (_, specs) ->
                specs
            | _ ->
                []))

    let private importedModules (document: ParsedDocument) =
        collectImportSpecs document
        |> List.choose (fun spec ->
            match spec.Source with
            | Dotted name -> Some(SyntaxFacts.moduleNameToText name)
            | Url _ -> None)

    let severityText severity =
        match severity with
        | DiagnosticSeverity.Info -> "info"
        | DiagnosticSeverity.Warning -> "warning"
        | DiagnosticSeverity.Error -> "error"

    let tokenKindText tokenKind =
        match tokenKind with
        | Identifier -> "Identifier"
        | IntegerLiteral -> "IntegerLiteral"
        | FloatLiteral -> "FloatLiteral"
        | StringLiteral -> "StringLiteral"
        | InterpolatedStringStart -> "InterpolatedStringStart"
        | StringTextSegment -> "StringTextSegment"
        | InterpolationStart -> "InterpolationStart"
        | InterpolationEnd -> "InterpolationEnd"
        | InterpolatedStringEnd -> "InterpolatedStringEnd"
        | CharacterLiteral -> "CharacterLiteral"
        | Keyword keyword -> $"Keyword.{Keyword.toText keyword}"
        | Operator -> "Operator"
        | AtSign -> "AtSign"
        | Backslash -> "Backslash"
        | Colon -> "Colon"
        | Comma -> "Comma"
        | Dot -> "Dot"
        | Equals -> "Equals"
        | Arrow -> "Arrow"
        | LeftParen -> "LeftParen"
        | RightParen -> "RightParen"
        | LeftBracket -> "LeftBracket"
        | RightBracket -> "RightBracket"
        | LeftBrace -> "LeftBrace"
        | RightBrace -> "RightBrace"
        | LeftSetBrace -> "LeftSetBrace"
        | RightSetBrace -> "RightSetBrace"
        | Underscore -> "Underscore"
        | Newline -> "Newline"
        | Indent -> "Indent"
        | Dedent -> "Dedent"
        | EndOfFile -> "EndOfFile"
        | BadToken -> "BadToken"

    let tokensText (tokens: Token list) =
        tokens
        |> List.map (fun token -> token.Text)
        |> String.concat " "

    let private moduleSpecifierText moduleSpecifier =
        match moduleSpecifier with
        | Dotted segments -> SyntaxFacts.moduleNameToText segments
        | Url url -> url

    let private importItemModifierText modifier =
        match modifier with
        | Unhide -> "unhide"
        | Clarify -> "clarify"

    let private importNamespaceText importNamespace =
        match importNamespace with
        | ImportNamespace.Term -> "term"
        | ImportNamespace.Type -> "type"
        | ImportNamespace.Trait -> "trait"
        | ImportNamespace.Constructor -> "ctor"

    let private importItemText (item: ImportItem) =
        let parts =
            (item.Modifiers |> List.map importItemModifierText)
            @ [ item.Namespace |> Option.map importNamespaceText |> Option.defaultValue ""
                item.Name ]

        parts
        |> List.filter (String.IsNullOrWhiteSpace >> not)
        |> String.concat " "

    let importSpecText (spec: ImportSpec) =
        let sourceText = moduleSpecifierText spec.Source

        match spec.Alias, spec.Selection with
        | Some alias, QualifiedOnly ->
            $"{sourceText} as {alias}"
        | None, QualifiedOnly ->
            sourceText
        | None, Items items ->
            let itemText =
                items
                |> List.map importItemText
                |> String.concat " + "

            $"{sourceText}.({itemText})"
        | None, All ->
            $"{sourceText}.*"
        | None, AllExcept names ->
            let nameText = String.concat " + " names
            $"{sourceText}.* except ({nameText})"
        | Some alias, _ ->
            $"{sourceText} as {alias}"

    let visibilityText visibility =
        match visibility with
        | Some Visibility.Public -> Some "public"
        | Some Visibility.Private -> Some "private"
        | None -> None

    let expressionText expression =
        let rec renderPattern pattern =
            match pattern with
            | WildcardPattern -> "_"
            | NamePattern name -> name
            | LiteralPattern(LiteralValue.Integer value) -> string value
            | LiteralPattern(LiteralValue.Float value) -> string value
            | LiteralPattern(LiteralValue.String value) -> $"\"{value}\""
            | LiteralPattern(LiteralValue.Character value) -> $"'{value}'"
            | LiteralPattern LiteralValue.Unit -> "()"
            | ConstructorPattern(name, arguments) ->
                let nameText = String.concat "." name

                match arguments with
                | [] ->
                    nameText
                | _ ->
                    let argumentText = arguments |> List.map renderPattern |> String.concat " "
                    $"({nameText} {argumentText})"
            | OrPattern alternatives ->
                alternatives
                |> List.map renderPattern
                |> String.concat " | "
            | AnonymousRecordPattern fields ->
                let fieldText =
                    fields
                    |> List.map (fun field -> $"{field.Name} = {renderPattern field.Pattern}")
                    |> String.concat ", "

                $"({fieldText})"

        let renderBindPattern binding =
            let quantityText =
                binding.Quantity
                |> Option.map (fun quantity -> Quantity.toSurfaceText quantity + " ")
                |> Option.defaultValue ""

            quantityText + renderPattern binding.Pattern

        let rec render current =
            match current with
            | Literal(LiteralValue.Integer value) -> string value
            | Literal(LiteralValue.Float value) -> string value
            | Literal(LiteralValue.String value) -> $"\"{value}\""
            | Literal(LiteralValue.Character value) -> $"'{value}'"
            | Literal LiteralValue.Unit -> "()"
            | Name segments -> String.concat "." segments
            | LocalLet(binding, value, body) ->
                $"(let {renderBindPattern binding} {render value} {render body})"
            | Lambda(parameters, body) ->
                let names = parameters |> List.map (fun parameter -> parameter.Name) |> String.concat " "
                $"(lambda ({names}) {render body})"
            | IfThenElse(condition, whenTrue, whenFalse) ->
                $"(if {render condition} {render whenTrue} {render whenFalse})"
            | Match(scrutinee, cases) ->
                let caseText =
                    cases
                    |> List.map (fun caseClause ->
                        let guardText =
                            caseClause.Guard
                            |> Option.map (fun guard -> $" if {render guard}")
                            |> Option.defaultValue ""

                        $"(case {renderPattern caseClause.Pattern}{guardText} {render caseClause.Body})")
                    |> String.concat " "

                $"(match {render scrutinee} {caseText})"
            | RecordLiteral fields ->
                let fieldText =
                    fields
                    |> List.map (fun field ->
                        let prefix = if field.IsImplicit then "@" else ""
                        $"{prefix}{field.Name} = {render field.Value}")
                    |> String.concat ", "

                $"({fieldText})"
            | Seal(value, ascriptionTokens) ->
                let ascriptionText = ascriptionTokens |> List.map (fun token -> token.Text) |> String.concat " "
                $"(seal {render value} as {ascriptionText})"
            | RecordUpdate(receiver, fields) ->
                let fieldText =
                    fields
                    |> List.map (fun field ->
                        let prefix = if field.IsImplicit then "@" else ""
                        $"{prefix}{field.Name} = {render field.Value}")
                    |> String.concat ", "

                $"(update {render receiver} {{{fieldText}}})"
            | SafeNavigation(receiver, navigation) ->
                let argumentText =
                    navigation.Arguments
                    |> List.map render
                    |> String.concat " "

                let segmentsText = String.concat "." navigation.Segments
                let memberText =
                    match argumentText with
                    | "" -> segmentsText
                    | _ -> $"{segmentsText} {argumentText}"

                $"(safe-nav {render receiver} {memberText})"
            | TagTest(receiver, constructorName) ->
                let constructorText = String.concat "." constructorName
                $"(is {render receiver} {constructorText})"
            | Do statements ->
                let rec renderDoStatement statement =
                    match statement with
                    | DoLet(binding, body) -> $"(let {renderBindPattern binding} {render body})"
                    | DoLetQuestion(binding, body, failure) ->
                        let failureText =
                            match failure with
                            | Some failure ->
                                let bodyText =
                                    failure.Body
                                    |> List.map renderDoStatement
                                    |> String.concat " "

                                $" (else {renderBindPattern failure.ResiduePattern} {bodyText})"
                            | None -> ""

                        $"(let? {renderBindPattern binding} {render body}{failureText})"
                    | DoBind(binding, body) -> $"(<- {renderBindPattern binding} {render body})"
                    | DoVar(name, body) -> $"(var {name} {render body})"
                    | DoAssign(name, body) -> $"(assign {name} {render body})"
                    | DoUsing(binding, body) -> $"(using {renderPattern binding.Pattern} {render body})"
                    | DoIf(condition, whenTrue, whenFalse) ->
                        let trueText =
                            whenTrue
                            |> List.map renderDoStatement
                            |> String.concat " "

                        let falseText =
                            whenFalse
                            |> List.map renderDoStatement
                            |> String.concat " "

                        $"(if {render condition} (do {trueText}) (do {falseText}))"
                    | DoWhile(condition, body) ->
                        let bodyText =
                            body
                            |> List.map renderDoStatement
                            |> String.concat " "

                        $"(while {render condition} {bodyText})"
                    | DoReturn body -> $"(return {render body})"
                    | DoExpression body -> $"(expr {render body})"

                let statementText =
                    statements
                    |> List.map renderDoStatement
                    |> String.concat " "

                $"(do {statementText})"
            | MonadicSplice inner ->
                $"(! {render inner})"
            | Apply(callee, arguments) ->
                let argumentText =
                    arguments
                    |> List.map render
                    |> String.concat " "

                if String.IsNullOrWhiteSpace(argumentText) then
                    $"(apply {render callee})"
                else
                    $"(apply {render callee} {argumentText})"
            | InoutArgument inner ->
                $"(~ {render inner})"
            | Unary(operatorName, operand) ->
                $"({operatorName} {render operand})"
            | Binary(left, operatorName, right) ->
                $"({operatorName} {render left} {render right})"
            | Elvis(left, right) ->
                $"(?: {render left} {render right})"
            | PrefixedString(prefix, parts) ->
                let partText =
                    parts
                    |> List.map (function
                        | StringText text -> $"text:{text}"
                        | StringInterpolation inner -> $"interp:{render inner}")
                    |> String.concat " | "

                $"({prefix}-string {partText})"

        render expression

    let private collectExpectDeclarations (document: ParsedDocument) =
        document.Syntax.Declarations
        |> List.choose (function
            | ExpectDeclarationNode declaration -> Some declaration
            | _ -> None)

    let private collectIntrinsicTerms (backendProfile: string) allowUnsafeConsume (document: ParsedDocument) =
        match document.ModuleName with
        | Some moduleName ->
            document.Syntax.Declarations
            |> List.choose (function
                | ExpectDeclarationNode (ExpectTermDeclaration declaration)
                    when Stdlib.intrinsicallySatisfiesExpectForCompilation
                            backendProfile
                            allowUnsafeConsume
                            moduleName
                            (ExpectTermDeclaration declaration) ->
                    Some declaration.Name
                | _ ->
                    None)
            |> Set.ofList
        | None ->
            Set.empty

    let private isPrivateByDefault (document: ParsedDocument) =
        document.Syntax.ModuleAttributes
        |> List.exists (fun attributeName -> String.Equals(attributeName, "PrivateByDefault", StringComparison.Ordinal))

    let private isExportedVisibility isPrivateByDefault visibility =
        match visibility with
        | Some Visibility.Public -> true
        | Some Visibility.Private -> false
        | None -> not isPrivateByDefault

    let private isExportedDefinition (document: ParsedDocument) (definition: LetDefinition) =
        isExportedVisibility (isPrivateByDefault document) definition.Visibility

    let private exportedSignature (document: ParsedDocument) (signature: BindingSignature) =
        isExportedVisibility (isPrivateByDefault document) signature.Visibility

    let private exportedDataDeclaration (document: ParsedDocument) (declaration: DataDeclaration) =
        isExportedVisibility (isPrivateByDefault document) declaration.Visibility

    let private exportedTypeAlias (document: ParsedDocument) (declaration: TypeAlias) =
        isExportedVisibility (isPrivateByDefault document) declaration.Visibility

    let private exportedTraitDeclaration (document: ParsedDocument) (declaration: TraitDeclaration) =
        isExportedVisibility (isPrivateByDefault document) declaration.Visibility

    let private exportedProjectionDeclaration (document: ParsedDocument) (declaration: ProjectionDeclaration) =
        isExportedVisibility (isPrivateByDefault document) declaration.Visibility

    let private tokenName (token: Token) =
        match token.Kind with
        | Identifier
        | Keyword _ ->
            Some(SyntaxFacts.trimIdentifierQuotes token.Text)
        | Operator ->
            Some token.Text
        | _ ->
            None

    let private findTokenLocation (document: ParsedDocument) name =
        document.Syntax.Tokens
        |> List.tryPick (fun token ->
            tokenName token
            |> Option.filter (fun tokenName -> String.Equals(tokenName, name, StringComparison.Ordinal))
            |> Option.map (fun _ -> document.Source.GetLocation(token.Span)))

    let private buildModuleExportInventory (documents: ParsedDocument list) =
        let emptyInventory =
            { Terms = Set.empty
              Types = Set.empty
              Traits = Set.empty
              Constructors = Set.empty
              UnqualifiedBindings = Set.empty }

        let addDocument inventory (document: ParsedDocument) =
            document.Syntax.Declarations
            |> List.fold (fun current declaration ->
                match declaration with
                | SignatureDeclaration signature when exportedSignature document signature ->
                    { current with
                        Terms = Set.add signature.Name current.Terms
                        UnqualifiedBindings = Set.add signature.Name current.UnqualifiedBindings }
                | LetDeclaration definition when isExportedDefinition document definition ->
                    match definition.Name with
                    | Some name ->
                        { current with
                            Terms = Set.add name current.Terms
                            UnqualifiedBindings = Set.add name current.UnqualifiedBindings }
                    | None ->
                        current
                | ProjectionDeclarationNode declaration when exportedProjectionDeclaration document declaration ->
                    { current with
                        Terms = Set.add declaration.Name current.Terms
                        UnqualifiedBindings = Set.add declaration.Name current.UnqualifiedBindings }
                | DataDeclarationNode declaration when exportedDataDeclaration document declaration ->
                    let constructorNames =
                        declaration.Constructors
                        |> List.map (fun constructor -> constructor.Name)

                    let sameSpellingConstructors, _ =
                        constructorNames
                        |> List.partition (fun name -> String.Equals(name, declaration.Name, StringComparison.Ordinal))

                    { current with
                        Types = Set.add declaration.Name current.Types
                        Constructors = Set.union current.Constructors (Set.ofList constructorNames)
                        UnqualifiedBindings =
                            current.UnqualifiedBindings
                            |> Set.add declaration.Name
                            |> fun bindings ->
                                sameSpellingConstructors
                                |> List.fold (fun state name -> Set.add name state) bindings }
                | TypeAliasNode declaration when exportedTypeAlias document declaration ->
                    { current with
                        Types = Set.add declaration.Name current.Types
                        UnqualifiedBindings = Set.add declaration.Name current.UnqualifiedBindings }
                | TraitDeclarationNode declaration when exportedTraitDeclaration document declaration ->
                    { current with
                        Traits = Set.add declaration.Name current.Traits
                        UnqualifiedBindings = Set.add declaration.Name current.UnqualifiedBindings }
                | _ ->
                    current) inventory

        documents
        |> List.choose (fun document ->
            document.ModuleName
            |> Option.map (fun moduleName -> SyntaxFacts.moduleNameToText moduleName, document))
        |> List.groupBy fst
        |> List.map (fun (moduleNameText, entries) ->
            let syntaxInventory =
                entries
                |> List.map snd
                |> List.fold addDocument emptyInventory

            let inventory =
                if String.Equals(moduleNameText, Stdlib.PreludeModuleText, StringComparison.Ordinal) then
                    let contract = IntrinsicCatalog.bundledPreludeExpectContract ()

                    { syntaxInventory with
                        Terms = Set.union syntaxInventory.Terms contract.TermNames
                        Types = Set.union syntaxInventory.Types contract.TypeNames
                        Traits = Set.union syntaxInventory.Traits contract.TraitNames
                        UnqualifiedBindings =
                            syntaxInventory.UnqualifiedBindings
                            |> Set.union contract.TermNames
                            |> Set.union contract.TypeNames
                            |> Set.union contract.TraitNames
                            |> Set.union (Set.ofList Stdlib.FixedPreludeConstructors) }
                else
                    syntaxInventory

            moduleNameText, inventory)
        |> Map.ofList

    let private importItemExists inventory (item: ImportItem) =
        match item.Namespace with
        | Some ImportNamespace.Term ->
            Set.contains item.Name inventory.Terms
        | Some ImportNamespace.Type ->
            Set.contains item.Name inventory.Types
        | Some ImportNamespace.Trait ->
            Set.contains item.Name inventory.Traits
        | Some ImportNamespace.Constructor ->
            Set.contains item.Name inventory.Constructors
        | None ->
            Set.contains item.Name inventory.UnqualifiedBindings

    let validateImportSelections (documents: ParsedDocument list) =
        let diagnostics = DiagnosticBag()
        let exportInventories = buildModuleExportInventory documents

        for document in documents do
            for declaration in document.Syntax.Declarations do
                match declaration with
                | ImportDeclaration (_, specs) ->
                    for spec in specs do
                        match spec.Source, spec.Selection with
                        | Dotted moduleName, Items items ->
                            let importedModuleName = SyntaxFacts.moduleNameToText moduleName

                            match Map.tryFind importedModuleName exportInventories with
                            | Some inventory ->
                                for item in items do
                                    if not (importItemExists inventory item) then
                                        diagnostics.AddError(
                                            DiagnosticCode.ImportItemNotFound,
                                            $"Import item '{item.Name}' was not found in module '{importedModuleName}'.",
                                            defaultArg (findTokenLocation document item.Name) (document.Source.GetLocation(TextSpan.FromBounds(0, 0))),
                                            stage = "KFrontIR",
                                            phase = KFrontIRPhase.phaseName CHECKERS
                                        )
                            | None ->
                                ()
                        | _ ->
                            ()
                | _ ->
                    ()

        diagnostics.Items

    let private buildModuleFixityInventory (documents: ParsedDocument list) =
        documents
        |> List.choose (fun document ->
            document.ModuleName
            |> Option.map (fun moduleName -> SyntaxFacts.moduleNameToText moduleName, document))
        |> List.groupBy fst
        |> List.map (fun (moduleNameText, entries) ->
            let declarations =
                entries
                |> List.collect (fun (_, document) ->
                    document.Syntax.Declarations
                    |> List.choose (function
                        | FixityDeclarationNode declaration -> Some declaration
                        | _ -> None))

            moduleNameText, declarations)
        |> Map.ofList

    let private selectionImportsFixityName inventory selection name =
        match selection with
        | QualifiedOnly ->
            false
        | Items items ->
            items
            |> List.exists (fun item ->
                String.Equals(item.Name, name, StringComparison.Ordinal)
                && importItemExists inventory item)
        | All ->
            Set.contains name inventory.UnqualifiedBindings
        | AllExcept excludedNames ->
            Set.contains name inventory.UnqualifiedBindings
            && not (excludedNames |> List.exists (fun excluded -> String.Equals(excluded, name, StringComparison.Ordinal)))

    let reparseDocumentsWithImportedFixities (options: CompilationOptions) (documents: ParsedDocument list) =
        let baseFixities = Parser.bootstrapFixities ()
        let exportInventories = buildModuleExportInventory documents
        let moduleFixityInventory = buildModuleFixityInventory documents

        documents
        |> List.map (fun document ->
            let importedFixities =
                document.Syntax.Declarations
                |> List.collect (function
                    | ImportDeclaration (_, specs) -> specs
                    | _ -> [])
                |> List.collect (fun spec ->
                    match spec.Source with
                    | Dotted moduleName ->
                        let moduleNameText = SyntaxFacts.moduleNameToText moduleName

                        match Map.tryFind moduleNameText exportInventories, Map.tryFind moduleNameText moduleFixityInventory with
                        | Some inventory, Some fixities ->
                            fixities
                            |> List.filter (fun declaration ->
                                selectionImportsFixityName inventory spec.Selection declaration.OperatorName)
                        | _ ->
                            []
                    | Url _ ->
                        [])

            let initialFixities =
                importedFixities
                |> List.fold (fun table declaration -> FixityTable.add declaration table) baseFixities

            parseFileWithFixities options initialFixities document.Source.FilePath)

    let declarationKindText (declaration: TopLevelDeclaration) =
        match declaration with
        | ImportDeclaration (true, _) -> "export"
        | ImportDeclaration (false, _) -> "import"
        | FixityDeclarationNode _ -> "fixity"
        | ExpectDeclarationNode _ -> "expect"
        | SignatureDeclaration _ -> "signature"
        | LetDeclaration _ -> "let"
        | ProjectionDeclarationNode _ -> "projection"
        | DataDeclarationNode _ -> "data"
        | TypeAliasNode _ -> "type"
        | TraitDeclarationNode _ -> "trait"
        | InstanceDeclarationNode _ -> "instance"
        | UnknownDeclaration _ -> "unknown"

    let declarationName (declaration: TopLevelDeclaration) =
        match declaration with
        | SignatureDeclaration declaration -> Some declaration.Name
        | LetDeclaration declaration -> declaration.Name
        | ProjectionDeclarationNode declaration -> Some declaration.Name
        | DataDeclarationNode declaration -> Some declaration.Name
        | TypeAliasNode declaration -> Some declaration.Name
        | TraitDeclarationNode declaration -> Some declaration.Name
        | InstanceDeclarationNode declaration -> Some declaration.TraitName
        | ExpectDeclarationNode (ExpectTypeDeclaration declaration) -> Some declaration.Name
        | ExpectDeclarationNode (ExpectTraitDeclaration declaration) -> Some declaration.Name
        | ExpectDeclarationNode (ExpectTermDeclaration declaration) -> Some declaration.Name
        | FixityDeclarationNode declaration -> Some declaration.OperatorName
        | ImportDeclaration _
        | UnknownDeclaration _ -> None

    let declarationIsOpaque (declaration: TopLevelDeclaration) =
        match declaration with
        | SignatureDeclaration declaration -> declaration.IsOpaque
        | LetDeclaration declaration -> declaration.IsOpaque
        | ProjectionDeclarationNode _ -> false
        | DataDeclarationNode declaration -> declaration.IsOpaque
        | TypeAliasNode declaration -> declaration.IsOpaque
        | TraitDeclarationNode _
        | InstanceDeclarationNode _
        | ExpectDeclarationNode _
        | FixityDeclarationNode _
        | ImportDeclaration _
        | UnknownDeclaration _ -> false

    let declarationVisibility (declaration: TopLevelDeclaration) =
        match declaration with
        | SignatureDeclaration declaration -> visibilityText declaration.Visibility
        | LetDeclaration declaration -> visibilityText declaration.Visibility
        | ProjectionDeclarationNode declaration -> visibilityText declaration.Visibility
        | DataDeclarationNode declaration -> visibilityText declaration.Visibility
        | TypeAliasNode declaration -> visibilityText declaration.Visibility
        | TraitDeclarationNode declaration -> visibilityText declaration.Visibility
        | InstanceDeclarationNode _
        | ExpectDeclarationNode _
        | FixityDeclarationNode _
        | ImportDeclaration _
        | UnknownDeclaration _ -> None

    let declarationSummary (declaration: TopLevelDeclaration) =
        match declaration with
        | ImportDeclaration (isExport, specs) ->
            let keyword = if isExport then "export" else "import"
            let specText = specs |> List.map importSpecText |> String.concat " | "
            $"{keyword} {specText}".Trim()
        | FixityDeclarationNode declaration ->
            let fixityText =
                match declaration.Fixity with
                | Infix LeftAssociative -> "infix left"
                | Infix RightAssociative -> "infix right"
                | Infix NonAssociative -> "infix"
                | Prefix -> "prefix"
                | Postfix -> "postfix"

            $"{fixityText} {declaration.Precedence} ({declaration.OperatorName})"
        | ExpectDeclarationNode (ExpectTypeDeclaration declaration) ->
            $"expect type {declaration.Name} {tokensText declaration.HeaderTokens}".Trim()
        | ExpectDeclarationNode (ExpectTraitDeclaration declaration) ->
            $"expect trait {declaration.Name} {tokensText declaration.HeaderTokens}".Trim()
        | ExpectDeclarationNode (ExpectTermDeclaration declaration) ->
            $"expect term {declaration.Name} : {tokensText declaration.TypeTokens}".Trim()
        | SignatureDeclaration declaration ->
            let visibilityPrefix = defaultArg (visibilityText declaration.Visibility) ""
            let opaquePrefix = if declaration.IsOpaque then "opaque " else ""
            $"{visibilityPrefix} {opaquePrefix}signature {declaration.Name} : {tokensText declaration.TypeTokens}".Trim()
        | LetDeclaration declaration ->
            let headerText = tokensText declaration.HeaderTokens
            let bodyText = declaration.Body |> Option.map expressionText |> Option.defaultValue (tokensText declaration.BodyTokens)
            let visibilityPrefix = defaultArg (visibilityText declaration.Visibility) ""
            let opaquePrefix = if declaration.IsOpaque then "opaque " else ""
            let bindingName = defaultArg declaration.Name "<pattern>"
            $"{visibilityPrefix} {opaquePrefix}let {bindingName} {headerText} = {bodyText}".Trim()
        | ProjectionDeclarationNode declaration ->
            let binderText =
                declaration.Binders
                |> List.map (function
                    | ProjectionPlaceBinder binder ->
                        let receiverPrefix = if binder.IsReceiver then "this " else ""
                        $"(place {receiverPrefix}{binder.Name} : {tokensText binder.TypeTokens})"
                    | ProjectionValueBinder binder ->
                        let quantityPrefix =
                            binder.Quantity
                            |> Option.map Quantity.toSurfaceText
                            |> Option.map (fun quantity -> $"{quantity} ")
                            |> Option.defaultValue ""

                        let implicitPrefix = if binder.IsImplicit then "@ " else ""
                        let typeText = binder.TypeTokens |> Option.map tokensText |> Option.defaultValue ""
                        $"({implicitPrefix}{quantityPrefix}{binder.Name} : {typeText})".Trim())
                |> String.concat " "

            let bodyText = tokensText declaration.BodyTokens
            let visibilityPrefix = defaultArg (visibilityText declaration.Visibility) ""
            $"{visibilityPrefix} projection {declaration.Name} {binderText} : {tokensText declaration.ReturnTypeTokens} = {bodyText}".Trim()
        | DataDeclarationNode declaration ->
            let constructors =
                declaration.Constructors
                |> List.map (fun constructor -> constructor.Name)
                |> String.concat " | "

            let visibilityPrefix = defaultArg (visibilityText declaration.Visibility) ""
            let opaquePrefix = if declaration.IsOpaque then "opaque " else ""
            $"{visibilityPrefix} {opaquePrefix}data {declaration.Name} = {constructors}".Trim()
        | TypeAliasNode declaration ->
            let bodyText = declaration.BodyTokens |> Option.map tokensText |> Option.defaultValue ""
            let visibilityPrefix = defaultArg (visibilityText declaration.Visibility) ""
            let opaquePrefix = if declaration.IsOpaque then "opaque " else ""
            $"{visibilityPrefix} {opaquePrefix}type {declaration.Name} = {bodyText}".Trim()
        | TraitDeclarationNode declaration ->
            let members =
                declaration.Members
                |> List.choose (fun memberDeclaration -> memberDeclaration.Name)
                |> String.concat ", "

            let visibilityPrefix = defaultArg (visibilityText declaration.Visibility) ""
            $"{visibilityPrefix} trait {declaration.Name} [{members}]".Trim()
        | InstanceDeclarationNode declaration ->
            let members =
                declaration.Members
                |> List.choose (fun memberDeclaration -> memberDeclaration.Name)
                |> String.concat ", "

            $"instance {tokensText declaration.FullHeaderTokens} [{members}]".Trim()
        | UnknownDeclaration tokens ->
            $"unknown {tokensText tokens}".Trim()

    let declarationTypeText (declaration: TopLevelDeclaration) =
        match declaration with
        | SignatureDeclaration declaration -> Some(tokensText declaration.TypeTokens)
        | LetDeclaration declaration -> declaration.ReturnTypeTokens |> Option.map tokensText
        | ProjectionDeclarationNode declaration -> Some(tokensText declaration.ReturnTypeTokens)
        | ExpectDeclarationNode (ExpectTermDeclaration declaration) -> Some(tokensText declaration.TypeTokens)
        | _ -> None

    let declarationBodyText (declaration: TopLevelDeclaration) =
        match declaration with
        | LetDeclaration declaration ->
            declaration.Body
            |> Option.map expressionText
            |> Option.orElseWith (fun () ->
                if List.isEmpty declaration.BodyTokens then None else Some(tokensText declaration.BodyTokens))
        | ProjectionDeclarationNode declaration ->
            if List.isEmpty declaration.BodyTokens then None else Some(tokensText declaration.BodyTokens)
        | TypeAliasNode declaration -> declaration.BodyTokens |> Option.map tokensText
        | _ -> None

    let declarationConstructors (declaration: TopLevelDeclaration) =
        match declaration with
        | DataDeclarationNode declaration -> declaration.Constructors |> List.map (fun constructor -> constructor.Name)
        | _ -> []

    let declarationMembers (declaration: TopLevelDeclaration) =
        match declaration with
        | TraitDeclarationNode declaration ->
            declaration.Members
            |> List.choose (fun memberDeclaration -> memberDeclaration.Name)
        | InstanceDeclarationNode declaration ->
            declaration.Members
            |> List.choose (fun memberDeclaration -> memberDeclaration.Name)
        | _ -> []

    let moduleNameText moduleName =
        moduleName
        |> Option.map SyntaxFacts.moduleNameToText
        |> Option.defaultValue "<unknown>"

    let declarationOrigin filePath moduleName declaration =
        { FilePath = filePath
          ModuleName = moduleName
          DeclarationName = declarationName declaration
          IntroductionKind = "source" }

    let buildKFrontIRModule ownershipFactsByFile (document: ParsedDocument) =
        { FilePath = document.Source.FilePath
          ModuleHeader = document.Syntax.ModuleHeader
          InferredModuleName = document.InferredModuleName
          ModuleIdentity = document.ModuleName
          ModuleAttributes = document.Syntax.ModuleAttributes
          Imports = collectImportSpecs document
          Tokens = document.Syntax.Tokens
          Declarations = document.Syntax.Declarations
          Diagnostics = document.Diagnostics
          Ownership = Map.tryFind document.Source.FilePath ownershipFactsByFile
          ResolvedPhases = Set.ofList KFrontIRPhase.all }

    let private countOrdinarySatisfactions (moduleDocuments: ParsedDocument list) declaration =
        let declarations =
            moduleDocuments
            |> List.collect (fun document -> document.Syntax.Declarations)

        let matchesExpectation candidate =
            match declaration, candidate with
            | ExpectTypeDeclaration expected, DataDeclarationNode dataDeclaration ->
                String.Equals(expected.Name, dataDeclaration.Name, StringComparison.Ordinal)
            | ExpectTypeDeclaration expected, TypeAliasNode alias ->
                String.Equals(expected.Name, alias.Name, StringComparison.Ordinal)
            | ExpectTraitDeclaration expected, TraitDeclarationNode traitDeclaration ->
                String.Equals(expected.Name, traitDeclaration.Name, StringComparison.Ordinal)
            | ExpectTermDeclaration expected, LetDeclaration definition ->
                definition.Name
                |> Option.exists (fun name -> String.Equals(expected.Name, name, StringComparison.Ordinal))
            | ExpectTermDeclaration expected, ProjectionDeclarationNode declaration ->
                String.Equals(expected.Name, declaration.Name, StringComparison.Ordinal)
            | _ ->
                false

        declarations |> List.filter matchesExpectation |> List.length

    let private describeExpectation declaration =
        match declaration with
        | ExpectTypeDeclaration declaration -> $"type '{declaration.Name}'"
        | ExpectTraitDeclaration declaration -> $"trait '{declaration.Name}'"
        | ExpectTermDeclaration declaration -> $"term '{declaration.Name}'"

    let private spanOfExpectation declaration =
        match declaration with
        | ExpectTypeDeclaration declaration -> declaration.Span
        | ExpectTraitDeclaration declaration -> declaration.Span
        | ExpectTermDeclaration declaration -> declaration.Span

    let validateReflEqualityDeclarations (documents: ParsedDocument list) =
        let diagnostics = DiagnosticBag()

        for document in documents do
            let mutable signaturesByName = Map.empty

            for declaration in document.Syntax.Declarations do
                match declaration with
                | SignatureDeclaration signature ->
                    signaturesByName <- Map.add signature.Name signature.TypeTokens signaturesByName
                | LetDeclaration definition ->
                    match definition.Name, definition.Body with
                    | Some name, Some(Name [ "refl" ]) ->
                        let declaredTypeTokens =
                            definition.ReturnTypeTokens
                            |> Option.orElseWith (fun () -> Map.tryFind name signaturesByName)

                        match declaredTypeTokens |> Option.bind TypeSignatures.parseType with
                        | Some(TypeSignatures.TypeEquality(left, right))
                            when not (TypeSignatures.definitionallyEqual left right) ->
                            diagnostics.AddError(
                                DiagnosticCode.TypeEqualityMismatch,
                                "The proof term 'refl' requires both sides of the equality type to be definitionally equal. Function binder quantities are part of type identity.",
                                findTokenLocation document name |> Option.defaultValue (document.Source.GetLocation(TextSpan.FromBounds(0, 0))),
                                stage = "KFrontIR",
                                phase = KFrontIRPhase.phaseName CHECKERS
                            )
                        | _ ->
                            ()
                    | _ ->
                        ()
                | _ ->
                    ()

        diagnostics.Items

    let validateExpectDeclarations (backendProfile: string) allowUnsafeConsume (documents: ParsedDocument list) =
        let diagnostics = DiagnosticBag()

        let documentsByModule =
            documents
            |> List.choose (fun document ->
                document.ModuleName
                |> Option.map (fun moduleName -> SyntaxFacts.moduleNameToText moduleName, document))
            |> List.groupBy fst
            |> List.map (fun (moduleNameText, entries) -> moduleNameText, entries |> List.map snd)
            |> Map.ofList

        for document in documents do
            match document.ModuleName with
            | Some moduleName ->
                let moduleNameText = SyntaxFacts.moduleNameToText moduleName
                let moduleDocuments = documentsByModule[moduleNameText]

                for declaration in collectExpectDeclarations document do
                    let satisfactionCount =
                        countOrdinarySatisfactions moduleDocuments declaration
                        + if
                              Stdlib.intrinsicallySatisfiesExpectForCompilation
                                  backendProfile
                                  allowUnsafeConsume
                                  moduleName
                                  declaration
                          then
                              1
                          else
                              0

                    if satisfactionCount = 0 then
                        diagnostics.AddError(
                            DiagnosticCode.ExpectUnsatisfied,
                            $"Unsatisfied expect declaration for {describeExpectation declaration}.",
                            document.Source.GetLocation(spanOfExpectation declaration),
                            stage = "KFrontIR",
                            phase = KFrontIRPhase.phaseName CHECKERS
                        )
                    elif satisfactionCount > 1 then
                        diagnostics.AddError(
                            DiagnosticCode.ExpectAmbiguous,
                            $"Multiple satisfactions were found for expected {describeExpectation declaration}.",
                            document.Source.GetLocation(spanOfExpectation declaration),
                            stage = "KFrontIR",
                            phase = KFrontIRPhase.phaseName CHECKERS
                        )
            | None ->
                ()

        diagnostics.Items

    let detectImportCycles (documents: ParsedDocument list) =
        let diagnostics = ResizeArray<Diagnostic>()
        let emitted = HashSet<string>()

        let moduleMap =
            documents
            |> List.choose (fun document ->
                document.ModuleName
                |> Option.map (fun moduleName -> SyntaxFacts.moduleNameToText moduleName, document))
            |> Map.ofList

        let edges =
            moduleMap
            |> Map.map (fun _ document ->
                importedModules document
                |> List.filter (fun moduleName -> moduleMap.ContainsKey(moduleName)))

        let states = Dictionary<string, int>()
        let stack = ResizeArray<string>()

        let rec visit moduleName =
            let state = states.GetValueOrDefault(moduleName, 0)

            match state with
            | 1 ->
                if emitted.Add(moduleName) then
                    let dependency = moduleName
                    let cycleStart = stack |> Seq.findIndex ((=) dependency)
                    let cycle = stack |> Seq.skip cycleStart |> Seq.toList
                    let message = String.concat " -> " (cycle @ [ dependency ])
                    let document = moduleMap[moduleName]

                    diagnostics.Add(
                        { Severity = Error
                          Code = DiagnosticCode.ImportCycle
                          Stage = Some "KFrontIR"
                          Phase = Some(KFrontIRPhase.phaseName CHECKERS)
                          Message = $"Import cycle detected: {message}."
                          Location = Some(document.Source.GetLocation(TextSpan.FromBounds(0, 0)))
                          RelatedLocations = [] }
                    )
            | 2 ->
                ()
            | _ ->
                states[moduleName] <- 1
                stack.Add(moduleName)

                for dependency in edges[moduleName] do
                    visit dependency

                ignore (stack.RemoveAt(stack.Count - 1))
                states[moduleName] <- 2

        for moduleName in moduleMap.Keys do
            visit moduleName

        diagnostics |> Seq.toList
