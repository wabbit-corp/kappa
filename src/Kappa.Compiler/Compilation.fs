namespace Kappa.Compiler

open System
open System.Buffers
open System.IO
open System.Collections.Generic
open System.Text
open System.Text.Json

module Compilation =
    type DumpSourceDocument =
        { FilePath: string
          ModuleIdentity: string option
          Text: string
          LineCount: int }

    type DumpToken =
        { Kind: string
          Text: string }

    type DumpDiagnostic =
        { Code: string
          Stage: string option
          Phase: string option
          Severity: string
          Message: string
          FilePath: string option
          StartLine: int option
          StartColumn: int option
          EndLine: int option
          EndColumn: int option
          RelatedOrigins: DumpRelatedOrigin list }

    and DumpRelatedOrigin =
        { Message: string
          FilePath: string
          StartLine: int
          StartColumn: int
          EndLine: int
          EndColumn: int }

    type DumpOwnershipBinding =
        { Id: string
          Name: string
          Kind: string
          DeclaredQuantity: string option
          InferredDemand: string
          State: string
          PlaceRoot: string
          PlacePath: string list
          BorrowRegionId: string option }

    type DumpOwnershipUse =
        { Id: string
          UseKind: string
          TargetBindingId: string option
          TargetName: string
          PlaceRoot: string
          PlacePath: string list }

    type DumpOwnershipBorrowRegion =
        { Id: string
          ExplicitName: string option
          OwnerScope: string }

    type DumpOwnershipUsingScope =
        { Id: string
          HiddenOwnedBinding: string
          SharedRegionId: string
          HiddenReleaseObligation: string }

    type DumpOwnershipClosure =
        { Id: string
          Name: string option
          CaptureBindingIds: string list
          CaptureNames: string list
          RegionEnvironment: string list
          EscapeStatus: string }

    type DumpOwnershipFacts =
        { Bindings: DumpOwnershipBinding list
          Uses: DumpOwnershipUse list
          BorrowRegions: DumpOwnershipBorrowRegion list
          UsingScopes: DumpOwnershipUsingScope list
          Closures: DumpOwnershipClosure list
          Deferred: string list
          Diagnostics: string list }

    type DumpDeclaration =
        { Kind: string
          Name: string option
          Visibility: string option
          IsOpaque: bool
          Summary: string
          TypeText: string option
          BodyText: string option
          Constructors: string list
          Members: string list }

    type DumpDocument =
        { FilePath: string
          ModuleHeader: string option
          InferredModuleName: string option
          ModuleIdentity: string option
          ModuleAttributes: string list
          Imports: string list
          Tokens: DumpToken list
          Declarations: DumpDeclaration list
          Diagnostics: DumpDiagnostic list
          Ownership: DumpOwnershipFacts option }

    type DumpCoreModule =
        { Name: string
          SourceFile: string
          Imports: string list
          Ownership: DumpOwnershipFacts option
          Declarations: DumpDeclaration list }

    type DumpRuntimeBinding =
        { Name: string
          Parameters: string list
          Body: string
          Intrinsic: bool }

    type DumpRuntimeConstructor =
        { Name: string
          Arity: int
          TypeName: string }

    type DumpRuntimeModule =
        { Name: string
          SourceFile: string
          Exports: string list
          IntrinsicTerms: string list
          Constructors: DumpRuntimeConstructor list
          Bindings: DumpRuntimeBinding list }

    type DumpBackendParameter =
        { Name: string
          Representation: string }

    type DumpBackendCapture =
        { Name: string
          Representation: string }

    type DumpBackendFunction =
        { Name: string
          Parameters: DumpBackendParameter list
          CallingConvention: string
          ReturnRepresentation: string option
          EnvironmentLayout: string option
          Intrinsic: bool
          Exported: bool
          EntryPoint: bool
          ControlForm: string
          Body: string }

    type DumpBackendConstructorLayout =
        { Name: string
          Tag: int
          FieldRepresentations: string list }

    type DumpBackendDataLayout =
        { TypeName: string
          RepresentationClass: string
          TagEncoding: string
          Constructors: DumpBackendConstructorLayout list }

    type DumpBackendEnvironmentLayout =
        { Name: string
          Slots: DumpBackendCapture list }

    type DumpBackendModule =
        { Name: string
          SourceFile: string
          Imports: string list
          Exports: string list
          EntryPoints: string list
          IntrinsicTerms: string list
          DataLayouts: DumpBackendDataLayout list
          EnvironmentLayouts: DumpBackendEnvironmentLayout list
          Functions: DumpBackendFunction list }

    let private compilerImplementationId = "kappa.compiler"
    let private compilerImplementationVersion = "0.1.0"
    let private languageVersion = "draft"

    let private parseBundledPrelude () =
        let source = SourceText.From(Stdlib.BundledPreludeVirtualPath, Stdlib.loadBundledPreludeText ())
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
                $"Module header '{SyntaxFacts.moduleNameToText declared}' does not match the path-derived module name '{SyntaxFacts.moduleNameToText inferred}'.",
                document.Source.GetLocation(TextSpan.FromBounds(0, 0)),
                code = "E_MODULE_PATH_MISMATCH",
                stage = "KFrontIR",
                phase = KFrontIRPhase.phaseName CHECKERS
            )
        | None, None ->
            diagnostics.AddError(
                $"Could not derive a Kappa module name from '{document.Source.FilePath}'. Files must live under the source root and end in .kp.",
                document.Source.GetLocation(TextSpan.FromBounds(0, 0)),
                code = "E_MODULE_NAME_UNRESOLVED",
                stage = "KFrontIR",
                phase = KFrontIRPhase.phaseName CHECKERS
            )
        | _ ->
            ()

        diagnostics.Items

    let private parseFile (options: CompilationOptions) filePath =
        let source = readSource options.FileSystem filePath
        let inferredModuleName = tryInferModuleName options.FileSystem options.SourceRoot source.FilePath
        let lexed = Lexer.tokenize source
        let parsed = Parser.parse source lexed.Tokens

        let document =
            { Source = source
              InferredModuleName = inferredModuleName
              Syntax = parsed.Syntax
              Diagnostics = lexed.Diagnostics @ parsed.Diagnostics }

        { document with
            Diagnostics = document.Diagnostics @ validateModuleName options document }

    let private collectInputFiles (options: CompilationOptions) inputs =
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

    let private collectImportSpecs (document: ParsedDocument) =
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

    let private severityText severity =
        match severity with
        | DiagnosticSeverity.Info -> "info"
        | DiagnosticSeverity.Warning -> "warning"
        | DiagnosticSeverity.Error -> "error"

    let private tokenKindText tokenKind =
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

    let private tokensText (tokens: Token list) =
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

    let private importSpecText (spec: ImportSpec) =
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

    let private visibilityText visibility =
        match visibility with
        | Some Visibility.Public -> Some "public"
        | Some Visibility.Private -> Some "private"
        | None -> None

    let private expressionText expression =
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

        let rec render current =
            match current with
            | Literal(LiteralValue.Integer value) -> string value
            | Literal(LiteralValue.Float value) -> string value
            | Literal(LiteralValue.String value) -> $"\"{value}\""
            | Literal(LiteralValue.Character value) -> $"'{value}'"
            | Literal LiteralValue.Unit -> "()"
            | Name segments -> String.concat "." segments
            | Lambda(parameters, body) ->
                let names = parameters |> List.map (fun parameter -> parameter.Name) |> String.concat " "
                $"(lambda ({names}) {render body})"
            | IfThenElse(condition, whenTrue, whenFalse) ->
                $"(if {render condition} {render whenTrue} {render whenFalse})"
            | Match(scrutinee, cases) ->
                let caseText =
                    cases
                    |> List.map (fun caseClause -> $"(case {renderPattern caseClause.Pattern} {render caseClause.Body})")
                    |> String.concat " "

                $"(match {render scrutinee} {caseText})"
            | Do statements ->
                let renderBindPattern binding =
                    let quantityText =
                        binding.Quantity
                        |> Option.map (fun quantity -> Quantity.toSurfaceText quantity + " ")
                        |> Option.defaultValue ""

                    quantityText + renderPattern binding.Pattern

                let rec renderDoStatement statement =
                    match statement with
                    | DoLet(binding, body) -> $"(let {renderBindPattern binding} {render body})"
                    | DoBind(binding, body) -> $"(<- {renderBindPattern binding} {render body})"
                    | DoVar(name, body) -> $"(var {name} {render body})"
                    | DoAssign(name, body) -> $"(assign {name} {render body})"
                    | DoUsing(pattern, body) -> $"(using {renderPattern pattern} {render body})"
                    | DoWhile(condition, body) ->
                        let bodyText =
                            body
                            |> List.map renderDoStatement
                            |> String.concat " "

                        $"(while {render condition} {bodyText})"
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

    let private collectIntrinsicTerms (backendProfile: string) (document: ParsedDocument) =
        match document.ModuleName with
        | Some moduleName ->
            document.Syntax.Declarations
            |> List.choose (function
                | ExpectDeclarationNode (ExpectTermDeclaration declaration)
                    when Stdlib.intrinsicallySatisfiesExpect backendProfile moduleName (ExpectTermDeclaration declaration) ->
                    Some declaration.Name
                | _ ->
                    None)
            |> Set.ofList
        | None ->
            Set.empty

    let private isPrivateByDefault (document: ParsedDocument) =
        document.Syntax.ModuleAttributes
        |> List.exists (fun attributeName -> String.Equals(attributeName, "PrivateByDefault", StringComparison.Ordinal))

    let private isExportedDefinition (document: ParsedDocument) (definition: LetDefinition) =
        match definition.Visibility with
        | Some Visibility.Public -> true
        | Some Visibility.Private -> false
        | None -> not (isPrivateByDefault document)

    let private declarationKindText (declaration: TopLevelDeclaration) =
        match declaration with
        | ImportDeclaration (true, _) -> "export"
        | ImportDeclaration (false, _) -> "import"
        | FixityDeclarationNode _ -> "fixity"
        | ExpectDeclarationNode _ -> "expect"
        | SignatureDeclaration _ -> "signature"
        | LetDeclaration _ -> "let"
        | DataDeclarationNode _ -> "data"
        | TypeAliasNode _ -> "type"
        | TraitDeclarationNode _ -> "trait"
        | InstanceDeclarationNode _ -> "instance"
        | UnknownDeclaration _ -> "unknown"

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
        | FixityDeclarationNode declaration -> Some declaration.OperatorName
        | ImportDeclaration _
        | UnknownDeclaration _ -> None

    let private declarationIsOpaque (declaration: TopLevelDeclaration) =
        match declaration with
        | SignatureDeclaration declaration -> declaration.IsOpaque
        | LetDeclaration declaration -> declaration.IsOpaque
        | DataDeclarationNode declaration -> declaration.IsOpaque
        | TypeAliasNode declaration -> declaration.IsOpaque
        | TraitDeclarationNode _ 
        | InstanceDeclarationNode _
        | ExpectDeclarationNode _
        | FixityDeclarationNode _
        | ImportDeclaration _
        | UnknownDeclaration _ -> false

    let private declarationVisibility (declaration: TopLevelDeclaration) =
        match declaration with
        | SignatureDeclaration declaration -> visibilityText declaration.Visibility
        | LetDeclaration declaration -> visibilityText declaration.Visibility
        | DataDeclarationNode declaration -> visibilityText declaration.Visibility
        | TypeAliasNode declaration -> visibilityText declaration.Visibility
        | TraitDeclarationNode declaration -> visibilityText declaration.Visibility
        | InstanceDeclarationNode _
        | ExpectDeclarationNode _
        | FixityDeclarationNode _
        | ImportDeclaration _
        | UnknownDeclaration _ -> None

    let private declarationSummary (declaration: TopLevelDeclaration) =
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

            $"instance {declaration.TraitName} {tokensText declaration.HeaderTokens} [{members}]".Trim()
        | UnknownDeclaration tokens ->
            $"unknown {tokensText tokens}".Trim()

    let private declarationTypeText (declaration: TopLevelDeclaration) =
        match declaration with
        | SignatureDeclaration declaration -> Some(tokensText declaration.TypeTokens)
        | LetDeclaration declaration -> declaration.ReturnTypeTokens |> Option.map tokensText
        | ExpectDeclarationNode (ExpectTermDeclaration declaration) -> Some(tokensText declaration.TypeTokens)
        | _ -> None

    let private declarationBodyText (declaration: TopLevelDeclaration) =
        match declaration with
        | LetDeclaration declaration ->
            declaration.Body
            |> Option.map expressionText
            |> Option.orElseWith (fun () ->
                if List.isEmpty declaration.BodyTokens then None else Some(tokensText declaration.BodyTokens))
        | TypeAliasNode declaration -> declaration.BodyTokens |> Option.map tokensText
        | _ -> None

    let private declarationConstructors (declaration: TopLevelDeclaration) =
        match declaration with
        | DataDeclarationNode declaration -> declaration.Constructors |> List.map (fun constructor -> constructor.Name)
        | _ -> []

    let private declarationMembers (declaration: TopLevelDeclaration) =
        match declaration with
        | TraitDeclarationNode declaration ->
            declaration.Members
            |> List.choose (fun memberDeclaration -> memberDeclaration.Name)
        | InstanceDeclarationNode declaration ->
            declaration.Members
            |> List.choose (fun memberDeclaration -> memberDeclaration.Name)
        | _ -> []

    let private moduleNameText moduleName =
        moduleName
        |> Option.map SyntaxFacts.moduleNameToText
        |> Option.defaultValue "<unknown>"

    let private declarationOrigin filePath moduleName declaration =
        { FilePath = filePath
          ModuleName = moduleName
          DeclarationName = declarationName declaration
          IntroductionKind = "source" }

    let private buildKFrontIRModule ownershipFactsByFile (document: ParsedDocument) =
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

    let private dumpDiagnostic (diagnostic: Diagnostic) =
        let dumpRelatedOrigin (related: DiagnosticRelatedLocation) =
            { Message = related.Message
              FilePath = related.Location.FilePath
              StartLine = related.Location.Start.Line
              StartColumn = related.Location.Start.Column
              EndLine = related.Location.End.Line
              EndColumn = related.Location.End.Column }

        { Code = diagnostic.Code
          Stage = diagnostic.Stage
          Phase = diagnostic.Phase
          Severity = severityText diagnostic.Severity
          Message = diagnostic.Message
          FilePath = diagnostic.Location |> Option.map (fun location -> location.FilePath)
          StartLine = diagnostic.Location |> Option.map (fun location -> location.Start.Line)
          StartColumn = diagnostic.Location |> Option.map (fun location -> location.Start.Column)
          EndLine = diagnostic.Location |> Option.map (fun location -> location.End.Line)
          EndColumn = diagnostic.Location |> Option.map (fun location -> location.End.Column)
          RelatedOrigins = diagnostic.RelatedLocations |> List.map dumpRelatedOrigin }

    let private dumpDeclaration (declaration: TopLevelDeclaration) =
        { Kind = declarationKindText declaration
          Name = declarationName declaration
          Visibility = declarationVisibility declaration
          IsOpaque = declarationIsOpaque declaration
          Summary = declarationSummary declaration
          TypeText = declarationTypeText declaration
          BodyText = declarationBodyText declaration
          Constructors = declarationConstructors declaration
          Members = declarationMembers declaration }

    let private dumpToken (token: Token) =
        { Kind = tokenKindText token.Kind
          Text = token.Text }

    let private dumpSourceDocument (document: ParsedDocument) =
        { FilePath = document.Source.FilePath
          ModuleIdentity = document.ModuleName |> Option.map SyntaxFacts.moduleNameToText
          Text = document.Source.Content
          LineCount = document.Source.LineCount }

    let private dumpOwnership (facts: OwnershipFactSet) =
        { Bindings =
            facts.OwnershipBindings
            |> List.map (fun binding ->
                { Id = binding.BindingId
                  Name = binding.BindingName
                  Kind = binding.BindingKind
                  DeclaredQuantity = binding.BindingDeclaredQuantity
                  InferredDemand = binding.BindingInferredDemand
                  State = binding.BindingState
                  PlaceRoot = binding.BindingPlaceRoot
                  PlacePath = binding.BindingPlacePath
                  BorrowRegionId = binding.BindingBorrowRegionId })
          Uses =
            facts.OwnershipUses
            |> List.map (fun useFact ->
                { Id = useFact.UseId
                  UseKind = useFact.UseKindName
                  TargetBindingId = useFact.UseTargetBindingId
                  TargetName = useFact.UseTargetName
                  PlaceRoot = useFact.UsePlaceRoot
                  PlacePath = useFact.UsePlacePath })
          BorrowRegions =
            facts.OwnershipBorrowRegions
            |> List.map (fun region ->
                { Id = region.BorrowRegionId
                  ExplicitName = region.BorrowRegionExplicitName
                  OwnerScope = region.BorrowRegionOwnerScope })
          UsingScopes =
            facts.OwnershipUsingScopes
            |> List.map (fun usingScope ->
                { Id = usingScope.UsingScopeId
                  HiddenOwnedBinding = usingScope.UsingScopeHiddenOwnedBinding
                  SharedRegionId = usingScope.UsingScopeSharedRegionId
                  HiddenReleaseObligation = usingScope.UsingScopeHiddenReleaseObligation })
          Closures =
            facts.OwnershipClosures
            |> List.map (fun closure ->
                { Id = closure.ClosureId
                  Name = closure.ClosureName
                  CaptureBindingIds = closure.ClosureCaptureBindingIds
                  CaptureNames = closure.ClosureCaptureNames
                  RegionEnvironment = closure.ClosureRegionEnvironment
                  EscapeStatus = closure.ClosureEscapeStatus })
          Deferred = facts.OwnershipDeferred
          Diagnostics = facts.OwnershipDiagnostics }

    let private dumpFrontendDocument (document: KFrontIRModule) =
        { FilePath = document.FilePath
          ModuleHeader = document.ModuleHeader |> Option.map SyntaxFacts.moduleNameToText
          InferredModuleName = document.InferredModuleName |> Option.map SyntaxFacts.moduleNameToText
          ModuleIdentity = document.ModuleIdentity |> Option.map SyntaxFacts.moduleNameToText
          ModuleAttributes = document.ModuleAttributes
          Imports = document.Imports |> List.map importSpecText
          Tokens = document.Tokens |> List.map dumpToken
          Declarations = document.Declarations |> List.map dumpDeclaration
          Diagnostics = document.Diagnostics |> List.map dumpDiagnostic
          Ownership = document.Ownership |> Option.map dumpOwnership }

    let private dumpKCoreDeclaration (declaration: KCoreDeclaration) =
        match declaration.Binding, declaration.Source with
        | Some binding, LetDeclaration _ ->
            let visibility = binding.Visibility |> visibilityText
            let visibilityPrefix = defaultArg visibility ""
            let opaquePrefix = if binding.IsOpaque then "opaque " else ""
            let bindingName = defaultArg binding.Name "<pattern>"

            let parameterText =
                binding.Parameters
                |> List.map (fun parameter -> parameter.Name)
                |> String.concat " "

            let signatureText =
                if String.IsNullOrWhiteSpace(parameterText) then
                    bindingName
                else
                    $"{bindingName} {parameterText}"

            let bodyText =
                match binding.BodyText with
                | Some text when not (String.IsNullOrWhiteSpace text) ->
                    Some text
                | _ ->
                    binding.Body |> Option.map IrText.kcoreExpressionText

            let summaryBodyText = bodyText |> Option.defaultValue "<missing>"

            { Kind = declarationKindText declaration.Source
              Name = binding.Name
              Visibility = visibility
              IsOpaque = binding.IsOpaque
              Summary = $"{visibilityPrefix} {opaquePrefix}let {signatureText} = {summaryBodyText}".Trim()
              TypeText = binding.ReturnTypeText
              BodyText = bodyText
              Constructors = []
              Members = [] }
        | _ ->
            dumpDeclaration declaration.Source

    let private dumpCoreModule (moduleDump: KCoreModule) =
        { Name = moduleDump.Name
          SourceFile = moduleDump.SourceFile
          Imports = moduleDump.Imports |> List.map importSpecText
          Ownership = moduleDump.Ownership |> Option.map dumpOwnership
          Declarations = moduleDump.Declarations |> List.map dumpKCoreDeclaration }

    let private dumpRuntimeModule (moduleDump: KRuntimeModule) =
        let constructors =
            moduleDump.Constructors
            |> List.map (fun constructor ->
                { Name = constructor.Name
                  Arity = constructor.Arity
                  TypeName = constructor.TypeName })

        let bindings =
            moduleDump.Bindings
            |> List.map (fun binding ->
                { Name = binding.Name
                  Parameters = binding.Parameters
                  Body =
                    binding.Body
                    |> Option.map IrText.runtimeExpressionText
                    |> Option.defaultValue "<intrinsic>"
                  Intrinsic = binding.Intrinsic })

        { Name = moduleDump.Name
          SourceFile = moduleDump.SourceFile
          Exports = moduleDump.Exports
          IntrinsicTerms = moduleDump.IntrinsicTerms
          Constructors = constructors
          Bindings = bindings }

    let private dumpBackendModule (moduleDump: KBackendModule) =
        let functions =
            moduleDump.Functions
            |> List.map (fun binding ->
                { Name = binding.Name
                  Parameters =
                    binding.Parameters
                    |> List.map (fun parameter ->
                        { Name = parameter.Name
                          Representation = IrText.backendRepresentationText parameter.Representation })
                  CallingConvention = IrText.backendCallingConventionText binding.CallingConvention
                  ReturnRepresentation =
                    binding.ReturnRepresentation
                    |> Option.map IrText.backendRepresentationText
                  EnvironmentLayout = binding.EnvironmentLayout
                  Intrinsic = binding.Intrinsic
                  Exported = binding.Exported
                  EntryPoint = binding.EntryPoint
                  ControlForm =
                    match binding.ControlForm with
                    | StructuredExpression -> "structured-expression"
                  Body =
                    binding.Body
                    |> Option.map IrText.backendExpressionText
                    |> Option.defaultValue "<intrinsic>" })

        let dataLayouts =
            moduleDump.DataLayouts
            |> List.map (fun layout ->
                { TypeName = layout.TypeName
                  RepresentationClass = layout.RepresentationClass
                  TagEncoding = layout.TagEncoding
                  Constructors =
                    layout.Constructors
                    |> List.map (fun constructor ->
                        { Name = constructor.Name
                          Tag = constructor.Tag
                          FieldRepresentations =
                            constructor.FieldRepresentations
                            |> List.map IrText.backendRepresentationText }) })

        let environmentLayouts =
            moduleDump.EnvironmentLayouts
            |> List.map (fun layout ->
                { Name = layout.Name
                  Slots =
                    layout.Slots
                    |> List.map (fun slot ->
                        { Name = slot.Name
                          Representation = IrText.backendRepresentationText slot.Representation }) })

        { Name = moduleDump.Name
          SourceFile = moduleDump.SourceFile
          Imports = moduleDump.Imports |> List.map importSpecText
          Exports = moduleDump.Exports
          EntryPoints = moduleDump.EntryPoints
          IntrinsicTerms = moduleDump.IntrinsicTerms
          DataLayouts = dataLayouts
          EnvironmentLayouts = environmentLayouts
          Functions = functions }

    let private countOrdinarySatisfactions (moduleDocuments: ParsedDocument list) declaration =
        let declarations =
            moduleDocuments
            |> List.collect (fun document -> document.Syntax.Declarations)

        match declaration with
        | ExpectTypeDeclaration declaration ->
            declarations
            |> List.sumBy (function
                | DataDeclarationNode dataDeclaration when String.Equals(dataDeclaration.Name, declaration.Name, StringComparison.Ordinal) -> 1
                | TypeAliasNode typeAlias when String.Equals(typeAlias.Name, declaration.Name, StringComparison.Ordinal) -> 1
                | _ -> 0)
        | ExpectTraitDeclaration declaration ->
            declarations
            |> List.sumBy (function
                | TraitDeclarationNode traitDeclaration when String.Equals(traitDeclaration.Name, declaration.Name, StringComparison.Ordinal) -> 1
                | _ -> 0)
        | ExpectTermDeclaration declaration ->
            declarations
            |> List.sumBy (function
                | LetDeclaration definition when definition.Name = Some declaration.Name -> 1
                | _ -> 0)

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

    let private validateExpectDeclarations (backendProfile: string) (documents: ParsedDocument list) =
        let diagnostics = DiagnosticBag()

        let documentsByModule =
            documents
            |> List.choose (fun document ->
                document.ModuleName
                |> Option.map (fun moduleName -> SyntaxFacts.moduleNameToText moduleName, document))
            |> List.groupBy fst
            |> List.map (fun (moduleName, grouped) -> moduleName, grouped |> List.map snd)
            |> Map.ofList

        for document in documents do
            match document.ModuleName with
            | Some moduleName ->
                let moduleNameText = SyntaxFacts.moduleNameToText moduleName
                let moduleDocuments = documentsByModule[moduleNameText]

                for declaration in collectExpectDeclarations document do
                    let satisfactionCount =
                        countOrdinarySatisfactions moduleDocuments declaration
                        + if Stdlib.intrinsicallySatisfiesExpect backendProfile moduleName declaration then 1 else 0

                    if satisfactionCount = 0 then
                        diagnostics.AddError(
                            $"Unsatisfied expect declaration for {describeExpectation declaration}.",
                            document.Source.GetLocation(spanOfExpectation declaration)
                        )
                    elif satisfactionCount > 1 then
                        diagnostics.AddError(
                            $"Multiple satisfactions were found for expected {describeExpectation declaration}.",
                            document.Source.GetLocation(spanOfExpectation declaration)
                        )
            | None ->
                ()

        diagnostics.Items

    let private detectImportCycles (documents: ParsedDocument list) =
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
                |> List.filter (fun imported -> moduleMap.ContainsKey(imported))
                |> Set.ofList)

        let states = Dictionary<string, int>()
        let stack = ResizeArray<string>()

        let rec visit moduleName =
            states[moduleName] <- 1
            stack.Add(moduleName)

            for dependency in edges[moduleName] do
                match states.TryGetValue(dependency) with
                | true, 1 ->
                    let cycleStart = stack |> Seq.findIndex ((=) dependency)
                    let cycle = stack |> Seq.skip cycleStart |> Seq.toList
                    let message = String.concat " -> " (cycle @ [ dependency ])

                    if emitted.Add(message) then
                        diagnostics.Add(
                            { Severity = Error
                              Code = "E_IMPORT_CYCLE"
                              Stage = Some "KFrontIR"
                              Phase = Some(KFrontIRPhase.phaseName CHECKERS)
                              Message = $"Import cycle detected: {message}"
                              Location = None
                              RelatedLocations = [] }
                        )
                | true, 2 ->
                    ()
                | _ ->
                    visit dependency

            stack.RemoveAt(stack.Count - 1)
            states[moduleName] <- 2

        for moduleName in moduleMap.Keys do
            if not (states.ContainsKey(moduleName)) then
                visit moduleName

        List.ofSeq diagnostics

    let private jsonOptions =
        JsonSerializerOptions(WriteIndented = true, PropertyNamingPolicy = JsonNamingPolicy.CamelCase)

    let private serializeJson value =
        JsonSerializer.Serialize(value, jsonOptions)

    let private sexprEscape (value: string) =
        value
            .Replace("\\", "\\\\")
            .Replace("\"", "\\\"")
            .Replace("\r", "\\r")
            .Replace("\n", "\\n")
            .Replace("\t", "\\t")

    let private sexprString value =
        $"\"{sexprEscape value}\""

    let private sexprAtom name value =
        $"({name} {value})"

    let private sexprStringAtom name value =
        sexprAtom name (sexprString value)

    let private sexprOptionalStringAtom name value =
        match value with
        | Some actual -> sexprStringAtom name actual
        | None -> sexprAtom name "nil"

    let private sexprStringList name values =
        let items =
            values
            |> List.map sexprString
            |> String.concat " "

        if String.IsNullOrWhiteSpace(items) then
            $"({name})"
        else
            $"({name} {items})"

    let private renderDumpRelatedOriginSexpr (related: DumpRelatedOrigin) =
        [
            sexprStringAtom "message" related.Message
            sexprStringAtom "file" related.FilePath
            sexprAtom "start-line" (string related.StartLine)
            sexprAtom "start-column" (string related.StartColumn)
            sexprAtom "end-line" (string related.EndLine)
            sexprAtom "end-column" (string related.EndColumn)
        ]
        |> String.concat " "
        |> fun body -> $"(related-origin {body})"

    let private renderDumpDiagnosticSexpr (diagnostic: DumpDiagnostic) =
        let relatedOrigins =
            diagnostic.RelatedOrigins
            |> List.map renderDumpRelatedOriginSexpr
            |> String.concat " "

        [
            sexprStringAtom "code" diagnostic.Code
            sexprOptionalStringAtom "stage" diagnostic.Stage
            sexprOptionalStringAtom "phase" diagnostic.Phase
            sexprStringAtom "severity" diagnostic.Severity
            sexprStringAtom "message" diagnostic.Message
            sexprOptionalStringAtom "file" diagnostic.FilePath
            diagnostic.StartLine |> Option.map (fun value -> sexprAtom "start-line" (string value)) |> Option.defaultValue (sexprAtom "start-line" "nil")
            diagnostic.StartColumn |> Option.map (fun value -> sexprAtom "start-column" (string value)) |> Option.defaultValue (sexprAtom "start-column" "nil")
            diagnostic.EndLine |> Option.map (fun value -> sexprAtom "end-line" (string value)) |> Option.defaultValue (sexprAtom "end-line" "nil")
            diagnostic.EndColumn |> Option.map (fun value -> sexprAtom "end-column" (string value)) |> Option.defaultValue (sexprAtom "end-column" "nil")
            if String.IsNullOrWhiteSpace(relatedOrigins) then "(related-origins)" else $"(related-origins {relatedOrigins})"
        ]
        |> String.concat " "
        |> fun body -> $"(diagnostic {body})"

    let private renderDumpDeclarationSexpr (declaration: DumpDeclaration) =
        [
            sexprStringAtom "kind" declaration.Kind
            sexprOptionalStringAtom "name" declaration.Name
            sexprOptionalStringAtom "visibility" declaration.Visibility
            sexprAtom "opaque" (if declaration.IsOpaque then "true" else "false")
            sexprStringAtom "summary" declaration.Summary
            declaration.TypeText |> Option.map (sexprStringAtom "type") |> Option.defaultValue (sexprAtom "type" "nil")
            declaration.BodyText |> Option.map (sexprStringAtom "body") |> Option.defaultValue (sexprAtom "body" "nil")
            sexprStringList "constructors" declaration.Constructors
            sexprStringList "members" declaration.Members
        ]
        |> String.concat " "
        |> fun body -> $"(declaration {body})"

    let private renderDumpTokenSexpr (token: DumpToken) =
        let kindAtom = sexprStringAtom "kind" token.Kind
        let textAtom = sexprStringAtom "text" token.Text
        $"(token {kindAtom} {textAtom})"

    let private renderOwnershipBindingSexpr (binding: DumpOwnershipBinding) =
        [
            sexprStringAtom "id" binding.Id
            sexprStringAtom "name" binding.Name
            sexprStringAtom "kind" binding.Kind
            sexprOptionalStringAtom "declared-quantity" binding.DeclaredQuantity
            sexprStringAtom "inferred-demand" binding.InferredDemand
            sexprStringAtom "state" binding.State
            sexprStringAtom "place-root" binding.PlaceRoot
            sexprStringList "place-path" binding.PlacePath
            sexprOptionalStringAtom "borrow-region-id" binding.BorrowRegionId
        ]
        |> String.concat " "
        |> fun body -> $"(binding {body})"

    let private renderOwnershipUseSexpr (useFact: DumpOwnershipUse) =
        [
            sexprStringAtom "id" useFact.Id
            sexprStringAtom "use-kind" useFact.UseKind
            sexprOptionalStringAtom "target-binding-id" useFact.TargetBindingId
            sexprStringAtom "target-name" useFact.TargetName
            sexprStringAtom "place-root" useFact.PlaceRoot
            sexprStringList "place-path" useFact.PlacePath
        ]
        |> String.concat " "
        |> fun body -> $"(use {body})"

    let private renderOwnershipBorrowRegionSexpr (region: DumpOwnershipBorrowRegion) =
        [
            sexprStringAtom "id" region.Id
            sexprOptionalStringAtom "explicit-name" region.ExplicitName
            sexprStringAtom "owner-scope" region.OwnerScope
        ]
        |> String.concat " "
        |> fun body -> $"(borrow-region {body})"

    let private renderOwnershipUsingScopeSexpr (usingScope: DumpOwnershipUsingScope) =
        [
            sexprStringAtom "id" usingScope.Id
            sexprStringAtom "hidden-owned-binding" usingScope.HiddenOwnedBinding
            sexprStringAtom "shared-region-id" usingScope.SharedRegionId
            sexprStringAtom "hidden-release-obligation" usingScope.HiddenReleaseObligation
        ]
        |> String.concat " "
        |> fun body -> $"(using-scope {body})"

    let private renderOwnershipClosureSexpr (closure: DumpOwnershipClosure) =
        [
            sexprStringAtom "id" closure.Id
            sexprOptionalStringAtom "name" closure.Name
            sexprStringList "capture-binding-ids" closure.CaptureBindingIds
            sexprStringList "capture-names" closure.CaptureNames
            sexprStringList "region-environment" closure.RegionEnvironment
            sexprStringAtom "escape-status" closure.EscapeStatus
        ]
        |> String.concat " "
        |> fun body -> $"(closure {body})"

    let private renderOwnershipSexpr (ownership: DumpOwnershipFacts option) =
        match ownership with
        | None ->
            "(ownership (status \"unknown\"))"
        | Some facts ->
            let bindings =
                facts.Bindings
                |> List.map renderOwnershipBindingSexpr
                |> String.concat " "

            let uses =
                facts.Uses
                |> List.map renderOwnershipUseSexpr
                |> String.concat " "

            let borrowRegions =
                facts.BorrowRegions
                |> List.map renderOwnershipBorrowRegionSexpr
                |> String.concat " "

            let usingScopes =
                facts.UsingScopes
                |> List.map renderOwnershipUsingScopeSexpr
                |> String.concat " "

            let closures =
                facts.Closures
                |> List.map renderOwnershipClosureSexpr
                |> String.concat " "

            [
                if String.IsNullOrWhiteSpace(bindings) then "(bindings)" else $"(bindings {bindings})"
                if String.IsNullOrWhiteSpace(uses) then "(uses)" else $"(uses {uses})"
                if String.IsNullOrWhiteSpace(borrowRegions) then "(borrow-regions)" else $"(borrow-regions {borrowRegions})"
                if String.IsNullOrWhiteSpace(usingScopes) then "(using-scopes)" else $"(using-scopes {usingScopes})"
                if String.IsNullOrWhiteSpace(closures) then "(closures)" else $"(closures {closures})"
                sexprStringList "deferred" facts.Deferred
                sexprStringList "diagnostics" facts.Diagnostics
            ]
            |> String.concat " "
            |> fun body -> $"(ownership {body})"

    let private renderDumpDocumentSexpr (document: DumpDocument) =
        [
            sexprStringAtom "file" document.FilePath
            sexprOptionalStringAtom "module-header" document.ModuleHeader
            sexprOptionalStringAtom "inferred-module-name" document.InferredModuleName
            sexprOptionalStringAtom "module-identity" document.ModuleIdentity
            sexprStringList "module-attributes" document.ModuleAttributes
            sexprStringList "imports" document.Imports
            let tokenBody =
                document.Tokens
                |> List.map renderDumpTokenSexpr
                |> String.concat " "

            if String.IsNullOrWhiteSpace(tokenBody) then "(tokens)" else $"(tokens {tokenBody})"
            let declarationBody =
                document.Declarations
                |> List.map renderDumpDeclarationSexpr
                |> String.concat " "

            if String.IsNullOrWhiteSpace(declarationBody) then "(declarations)" else $"(declarations {declarationBody})"
            let diagnosticsBody =
                document.Diagnostics
                |> List.map renderDumpDiagnosticSexpr
                |> String.concat " "

            if String.IsNullOrWhiteSpace(diagnosticsBody) then "(diagnostics)" else $"(diagnostics {diagnosticsBody})"
            renderOwnershipSexpr document.Ownership
        ]
        |> String.concat " "
        |> fun body -> $"(document {body})"

    let private renderDumpSourceDocumentSexpr (document: DumpSourceDocument) =
        [
            sexprStringAtom "file" document.FilePath
            sexprOptionalStringAtom "module-identity" document.ModuleIdentity
            sexprAtom "line-count" (string document.LineCount)
            sexprStringAtom "text" document.Text
        ]
        |> String.concat " "
        |> fun body -> $"(document {body})"

    let private renderDumpCoreModuleSexpr (moduleDump: DumpCoreModule) =
        [
            sexprStringAtom "name" moduleDump.Name
            sexprStringAtom "source-file" moduleDump.SourceFile
            sexprStringList "imports" moduleDump.Imports
            renderOwnershipSexpr moduleDump.Ownership
            let declarationBody =
                moduleDump.Declarations
                |> List.map renderDumpDeclarationSexpr
                |> String.concat " "

            if String.IsNullOrWhiteSpace(declarationBody) then "(declarations)" else $"(declarations {declarationBody})"
        ]
        |> String.concat " "
        |> fun body -> $"(module {body})"

    let private renderDumpRuntimeBindingSexpr (binding: DumpRuntimeBinding) =
        [
            sexprStringAtom "name" binding.Name
            sexprStringList "parameters" binding.Parameters
            sexprStringAtom "body" binding.Body
            sexprAtom "intrinsic" (if binding.Intrinsic then "true" else "false")
        ]
        |> String.concat " "
        |> fun body -> $"(binding {body})"

    let private renderDumpRuntimeConstructorSexpr (constructor: DumpRuntimeConstructor) =
        [
            sexprStringAtom "name" constructor.Name
            sexprAtom "arity" (string constructor.Arity)
            sexprStringAtom "type-name" constructor.TypeName
        ]
        |> String.concat " "
        |> fun body -> $"(constructor {body})"

    let private renderDumpRuntimeModuleSexpr (moduleDump: DumpRuntimeModule) =
        [
            sexprStringAtom "name" moduleDump.Name
            sexprStringAtom "source-file" moduleDump.SourceFile
            sexprStringList "exports" moduleDump.Exports
            sexprStringList "intrinsic-terms" moduleDump.IntrinsicTerms
            let constructorBody =
                moduleDump.Constructors
                |> List.map renderDumpRuntimeConstructorSexpr
                |> String.concat " "

            if String.IsNullOrWhiteSpace(constructorBody) then "(constructors)" else $"(constructors {constructorBody})"
            let bindingBody =
                moduleDump.Bindings
                |> List.map renderDumpRuntimeBindingSexpr
                |> String.concat " "

            if String.IsNullOrWhiteSpace(bindingBody) then "(bindings)" else $"(bindings {bindingBody})"
        ]
        |> String.concat " "
        |> fun body -> $"(module {body})"

    let private renderDumpBackendParameterSexpr (parameter: DumpBackendParameter) =
        [
            sexprStringAtom "name" parameter.Name
            sexprStringAtom "representation" parameter.Representation
        ]
        |> String.concat " "
        |> fun body -> $"(parameter {body})"

    let private renderDumpBackendCaptureSexpr (capture: DumpBackendCapture) =
        [
            sexprStringAtom "name" capture.Name
            sexprStringAtom "representation" capture.Representation
        ]
        |> String.concat " "
        |> fun body -> $"(slot {body})"

    let private renderDumpBackendFunctionSexpr (binding: DumpBackendFunction) =
        [
            sexprStringAtom "name" binding.Name
            let parameterBody =
                binding.Parameters
                |> List.map renderDumpBackendParameterSexpr
                |> String.concat " "

            if String.IsNullOrWhiteSpace(parameterBody) then "(parameters)" else $"(parameters {parameterBody})"
            sexprStringAtom "calling-convention" binding.CallingConvention
            binding.ReturnRepresentation
            |> Option.map (sexprStringAtom "return-representation")
            |> Option.defaultValue (sexprAtom "return-representation" "nil")
            binding.EnvironmentLayout
            |> Option.map (sexprStringAtom "environment-layout")
            |> Option.defaultValue (sexprAtom "environment-layout" "nil")
            sexprAtom "intrinsic" (if binding.Intrinsic then "true" else "false")
            sexprAtom "exported" (if binding.Exported then "true" else "false")
            sexprAtom "entry-point" (if binding.EntryPoint then "true" else "false")
            sexprStringAtom "control-form" binding.ControlForm
            sexprStringAtom "body" binding.Body
        ]
        |> String.concat " "
        |> fun body -> $"(function {body})"

    let private renderDumpBackendConstructorLayoutSexpr (constructor: DumpBackendConstructorLayout) =
        [
            sexprStringAtom "name" constructor.Name
            sexprAtom "tag" (string constructor.Tag)
            sexprStringList "field-representations" constructor.FieldRepresentations
        ]
        |> String.concat " "
        |> fun body -> $"(constructor {body})"

    let private renderDumpBackendDataLayoutSexpr (layout: DumpBackendDataLayout) =
        [
            sexprStringAtom "type-name" layout.TypeName
            sexprStringAtom "representation-class" layout.RepresentationClass
            sexprStringAtom "tag-encoding" layout.TagEncoding
            let constructorBody =
                layout.Constructors
                |> List.map renderDumpBackendConstructorLayoutSexpr
                |> String.concat " "

            if String.IsNullOrWhiteSpace(constructorBody) then "(constructors)" else $"(constructors {constructorBody})"
        ]
        |> String.concat " "
        |> fun body -> $"(data-layout {body})"

    let private renderDumpBackendEnvironmentLayoutSexpr (layout: DumpBackendEnvironmentLayout) =
        [
            sexprStringAtom "name" layout.Name
            let slotBody =
                layout.Slots
                |> List.map renderDumpBackendCaptureSexpr
                |> String.concat " "

            if String.IsNullOrWhiteSpace(slotBody) then "(slots)" else $"(slots {slotBody})"
        ]
        |> String.concat " "
        |> fun body -> $"(environment-layout {body})"

    let private renderDumpBackendModuleSexpr (moduleDump: DumpBackendModule) =
        [
            sexprStringAtom "name" moduleDump.Name
            sexprStringAtom "source-file" moduleDump.SourceFile
            sexprStringList "imports" moduleDump.Imports
            sexprStringList "exports" moduleDump.Exports
            sexprStringList "entry-points" moduleDump.EntryPoints
            sexprStringList "intrinsic-terms" moduleDump.IntrinsicTerms
            let dataLayoutBody =
                moduleDump.DataLayouts
                |> List.map renderDumpBackendDataLayoutSexpr
                |> String.concat " "

            if String.IsNullOrWhiteSpace(dataLayoutBody) then "(data-layouts)" else $"(data-layouts {dataLayoutBody})"
            let environmentLayoutBody =
                moduleDump.EnvironmentLayouts
                |> List.map renderDumpBackendEnvironmentLayoutSexpr
                |> String.concat " "

            if String.IsNullOrWhiteSpace(environmentLayoutBody) then "(environment-layouts)" else $"(environment-layouts {environmentLayoutBody})"
            let functionBody =
                moduleDump.Functions
                |> List.map renderDumpBackendFunctionSexpr
                |> String.concat " "

            if String.IsNullOrWhiteSpace(functionBody) then "(functions)" else $"(functions {functionBody})"
        ]
        |> String.concat " "
        |> fun body -> $"(module {body})"

    let private targetCheckpointNames (workspace: WorkspaceCompilation) =
        Stdlib.targetCheckpointNamesFor workspace.BackendProfile

    let private checkpointContract name kind inputCheckpoint requiredBySpec profileSpecific =
        { Name = name
          CheckpointKind = kind
          InputCheckpoint = inputCheckpoint
          RequiredBySpec = requiredBySpec
          ProfileSpecific = profileSpecific }

    let private frontendCheckpointContracts =
        let rec loop inputCheckpoint phases =
            match phases with
            | [] -> []
            | phase :: remainingPhases ->
                let checkpoint = KFrontIRPhase.checkpointName phase

                checkpointContract checkpoint KFrontIRCheckpoint (Some inputCheckpoint) true false
                :: loop checkpoint remainingPhases

        loop "surface-source" KFrontIRPhase.all

    let private baseCheckpointContracts =
        [
            checkpointContract "surface-source" SurfaceSourceCheckpoint None true false
            yield! frontendCheckpointContracts
            checkpointContract "KCore" KCoreCheckpoint (Some(KFrontIRPhase.checkpointName CORE_LOWERING)) true false
            checkpointContract "KRuntimeIR" ImplementationDefinedCheckpoint (Some "KCore") false false
            checkpointContract "KBackendIR" KBackendIRCheckpoint (Some "KRuntimeIR") true false
        ]

    let private targetCheckpointContracts (workspace: WorkspaceCompilation) =
        targetCheckpointNames workspace
        |> List.map (fun checkpoint ->
            checkpointContract checkpoint TargetLoweringCheckpoint (Some "KBackendIR") true true)

    let private contractsForWorkspace (workspace: WorkspaceCompilation) =
        baseCheckpointContracts
        @ targetCheckpointContracts workspace
        |> List.distinct

    let private checkpointContractFor workspace checkpoint =
        contractsForWorkspace workspace
        |> List.tryFind (fun contract -> String.Equals(contract.Name, checkpoint, StringComparison.Ordinal))

    let private checkpointContractJson workspace checkpoint =
        match checkpointContractFor workspace checkpoint with
        | Some contract ->
            {| name = contract.Name
               kind = CheckpointKind.toPortableName contract.CheckpointKind
               inputCheckpoint = contract.InputCheckpoint |> Option.toObj
               requiredBySpec = contract.RequiredBySpec
               profileSpecific = contract.ProfileSpecific |}
        | None ->
            {| name = checkpoint
               kind = "unknown"
               inputCheckpoint = null
               requiredBySpec = false
               profileSpecific = false |}

    let private renderCheckpointContractSexpr workspace checkpoint =
        let contract =
            checkpointContractFor workspace checkpoint
            |> Option.defaultValue (
                checkpointContract checkpoint ImplementationDefinedCheckpoint None false false
            )

        let inputAtom =
            match contract.InputCheckpoint with
            | Some inputCheckpoint -> sexprStringAtom "input-checkpoint" inputCheckpoint
            | None -> sexprAtom "input-checkpoint" "none"

        [
            sexprStringAtom "name" contract.Name
            sexprStringAtom "kind" (CheckpointKind.toPortableName contract.CheckpointKind)
            inputAtom
            sexprAtom "required-by-spec" (if contract.RequiredBySpec then "true" else "false")
            sexprAtom "profile-specific" (if contract.ProfileSpecific then "true" else "false")
        ]
        |> String.concat " "
        |> fun body -> $"(checkpoint-contract {body})"

    let private buildConfigurationJson workspace =
        {| identity = workspace.BuildConfigurationIdentity
           packageMode = workspace.PackageMode
           backendProfile = workspace.BackendProfile
           deploymentMode = workspace.DeploymentMode
           backendIntrinsicSet = workspace.BackendIntrinsicIdentity
           elaborationAvailableIntrinsicTerms = workspace.ElaborationAvailableIntrinsicTerms |}

    let private metadataJson workspace checkpoint =
        {| schemaVersion = "1"
           languageVersion = languageVersion
           compiler = {| id = compilerImplementationId; version = compilerImplementationVersion |}
           checkpoint = checkpoint
           compilationRoot = workspace.SourceRoot
           backendProfile = workspace.BackendProfile
           backendIntrinsicSet = workspace.BackendIntrinsicIdentity
           elaborationAvailableIntrinsicTerms = workspace.ElaborationAvailableIntrinsicTerms
           buildConfiguration = buildConfigurationJson workspace
           checkpointContract = checkpointContractJson workspace checkpoint |}

    let private metadataSexpr workspace checkpoint =
        let compilerAtom =
            let idAtom = sexprStringAtom "id" compilerImplementationId
            let versionAtom = sexprStringAtom "version" compilerImplementationVersion
            $"(compiler {idAtom} {versionAtom})"

        let buildConfigurationAtom =
            let identityAtom = sexprStringAtom "identity" workspace.BuildConfigurationIdentity
            let packageModeAtom = sexprAtom "package-mode" (if workspace.PackageMode then "true" else "false")
            let backendProfileAtom = sexprStringAtom "backend-profile" workspace.BackendProfile
            let deploymentModeAtom = sexprStringAtom "deployment-mode" workspace.DeploymentMode
            let backendIntrinsicSetAtom = sexprStringAtom "backend-intrinsic-set" workspace.BackendIntrinsicIdentity
            let elaborationAvailableAtom =
                sexprStringList "elaboration-available-intrinsic-terms" workspace.ElaborationAvailableIntrinsicTerms

            $"(build-configuration {identityAtom} {packageModeAtom} {backendProfileAtom} {deploymentModeAtom} {backendIntrinsicSetAtom} {elaborationAvailableAtom})"

        [
            sexprStringAtom "schema-version" "1"
            sexprStringAtom "language-version" languageVersion
            compilerAtom
            sexprStringAtom "checkpoint" checkpoint
            sexprStringAtom "compilation-root" workspace.SourceRoot
            sexprStringAtom "backend-profile" workspace.BackendProfile
            sexprStringAtom "backend-intrinsic-set" workspace.BackendIntrinsicIdentity
            sexprStringList "elaboration-available-intrinsic-terms" workspace.ElaborationAvailableIntrinsicTerms
            buildConfigurationAtom
            renderCheckpointContractSexpr workspace checkpoint
        ]
        |> String.concat " "

    let private emitClrTargetManifest (workspace: WorkspaceCompilation) =
        let verificationDiagnostics = CheckpointVerification.verifyCheckpoint workspace "KBackendIR"

        if not (List.isEmpty verificationDiagnostics) then
            let diagnosticText =
                verificationDiagnostics
                |> List.map (fun diagnostic -> diagnostic.Message)
                |> String.concat Environment.NewLine

            Result.Error $"Cannot emit CLR target manifest from malformed KBackendIR:{Environment.NewLine}{diagnosticText}"
        else
            let sanitizeIdentifier (value: string) =
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

                "Kappa.Generated." + String.concat "." segments

            let clrSymbol moduleName functionName =
                $"{emittedModuleTypeName moduleName}.{sanitizeIdentifier functionName}"

            let functions =
                workspace.KBackendIR
                |> List.collect (fun moduleDump ->
                    moduleDump.Functions
                    |> List.filter (fun functionDump -> not functionDump.Intrinsic)
                    |> List.map (fun functionDump -> moduleDump.Name, functionDump))

            let functionSymbols =
                functions
                |> List.map (fun (moduleName, functionDump) -> clrSymbol moduleName functionDump.Name)
                |> List.sort

            let entrySymbols =
                functions
                |> List.choose (fun (moduleName, functionDump) ->
                    if functionDump.EntryPoint then
                        Some(clrSymbol moduleName functionDump.Name)
                    else
                        None)
                |> List.sort

            Result.Ok
                { ArtifactKind = "clr-assembly"
                  TranslationUnitName = "Kappa.Generated.dll"
                  InputCheckpoint = "KBackendIR"
                  EntrySymbols = entrySymbols
                  FunctionSymbols = functionSymbols
                  SourceText = "" }

    let private tryEmitTargetTranslationUnit (workspace: WorkspaceCompilation) checkpoint =
        match Stdlib.normalizeBackendProfile workspace.BackendProfile, checkpoint with
        | "zig", checkpointName when checkpointName = Stdlib.ZigTargetCheckpointName ->
            ZigCcBackend.emitTranslationUnit workspace
        | ("dotnet" | "dotnet-il"), checkpointName when checkpointName = Stdlib.ClrTargetCheckpointName ->
            emitClrTargetManifest workspace
        | _ ->
            Result.Error $"Unknown checkpoint '{checkpoint}'."

    let private verifyTargetCheckpoint (workspace: WorkspaceCompilation) checkpoint =
        match tryEmitTargetTranslationUnit workspace checkpoint with
        | Result.Ok _ ->
            []
        | Result.Error message ->
            [ { Severity = Error
                Code = "E_TARGET_CHECKPOINT"
                Stage = Some "target-lowering"
                Phase = None
                Message = message
                Location = None
                RelatedLocations = [] } ]

    let private renderTargetCheckpointJson
        (workspace: WorkspaceCompilation)
        (checkpoint: string)
        (translationUnit: NativeTranslationUnit)
        =
        serializeJson
            {| schemaVersion = "1"
               languageVersion = languageVersion
               compiler = {| id = compilerImplementationId; version = compilerImplementationVersion |}
               checkpoint = checkpoint
               compilationRoot = workspace.SourceRoot
               backendProfile = workspace.BackendProfile
               backendIntrinsicSet = workspace.BackendIntrinsicIdentity
               elaborationAvailableIntrinsicTerms = workspace.ElaborationAvailableIntrinsicTerms
               buildConfiguration = buildConfigurationJson workspace
               checkpointContract = checkpointContractJson workspace checkpoint
               artifactKind = translationUnit.ArtifactKind
               inputCheckpoint = translationUnit.InputCheckpoint
               translationUnitName = translationUnit.TranslationUnitName
               entrySymbols = translationUnit.EntrySymbols
               functionSymbols = translationUnit.FunctionSymbols
               sourceText = translationUnit.SourceText
               diagnostics = workspace.Diagnostics |> List.map dumpDiagnostic |}

    let private renderTargetCheckpointSexpr
        (workspace: WorkspaceCompilation)
        (checkpoint: string)
        (translationUnit: NativeTranslationUnit)
        =
        let translationUnitAtom =
            [
                sexprStringAtom "artifact-kind" translationUnit.ArtifactKind
                sexprStringAtom "input-checkpoint" translationUnit.InputCheckpoint
                sexprStringAtom "translation-unit-name" translationUnit.TranslationUnitName
                sexprStringList "entry-symbols" translationUnit.EntrySymbols
                sexprStringList "function-symbols" translationUnit.FunctionSymbols
                sexprStringAtom "source-text" translationUnit.SourceText
            ]
            |> String.concat " "
            |> fun body -> $"(translation-unit {body})"

        let diagnosticsAtom =
            workspace.Diagnostics
            |> List.map dumpDiagnostic
            |> List.map renderDumpDiagnosticSexpr
            |> String.concat " "
            |> fun body -> if String.IsNullOrWhiteSpace(body) then "(diagnostics)" else $"(diagnostics {body})"

        $"(stage-dump {metadataSexpr workspace checkpoint} {translationUnitAtom} {diagnosticsAtom})"

    let private traceStep eventName subject stepName inputCheckpoint outputCheckpoint changedRepresentation verificationAttempted verificationSucceeded =
        { Event = eventName
          Subject = subject
          StepName = stepName
          InputCheckpoint = inputCheckpoint
          OutputCheckpoint = outputCheckpoint
          ChangedRepresentation = changedRepresentation
          VerificationAttempted = verificationAttempted
          VerificationSucceeded = verificationSucceeded }

    let private buildPipelineTrace (workspace: WorkspaceCompilation) =
        let documents =
            workspace.KFrontIR
            |> List.sortBy (fun document -> document.FilePath)

        let frontendCheckpoint = KFrontIRPhase.checkpointName CHECKERS
        let frontendVerified = CheckpointVerification.verifyCheckpoint workspace frontendCheckpoint |> List.isEmpty
        let coreVerified = CheckpointVerification.verifyCheckpoint workspace "KCore" |> List.isEmpty
        let runtimeVerified = CheckpointVerification.verifyCheckpoint workspace "KRuntimeIR" |> List.isEmpty
        let backendVerified = CheckpointVerification.verifyCheckpoint workspace "KBackendIR" |> List.isEmpty

        let phaseTransitions =
            KFrontIRPhase.all
            |> List.pairwise

        let documentSteps =
            documents
            |> List.collect (fun document ->
                let label =
                    document.ModuleIdentity
                    |> Option.map SyntaxFacts.moduleNameToText
                    |> Option.defaultValue document.FilePath

                let parseSteps =
                    [
                        traceStep
                            PipelineTraceEvent.Parse
                            PipelineTraceSubject.File
                            $"parse {document.FilePath}"
                            "surface-source"
                            "surface-source"
                            false
                            false
                            None
                        traceStep
                            PipelineTraceEvent.BuildKFrontIR
                            PipelineTraceSubject.File
                            $"build KFrontIR for {document.FilePath}"
                            "surface-source"
                            (KFrontIRPhase.checkpointName RAW)
                            true
                            false
                            None
                    ]

                let phaseSteps =
                    phaseTransitions
                    |> List.map (fun (fromPhase, toPhase) ->
                        traceStep
                            PipelineTraceEvent.AdvancePhase
                            PipelineTraceSubject.Module
                            $"advance {label} to {KFrontIRPhase.phaseName toPhase}"
                            (KFrontIRPhase.checkpointName fromPhase)
                            (KFrontIRPhase.checkpointName toPhase)
                            true
                            false
                            None)

                let verifySteps =
                    [
                        traceStep
                            PipelineTraceEvent.Verify
                            PipelineTraceSubject.Module
                            $"verify {label} at {frontendCheckpoint}"
                            frontendCheckpoint
                            frontendCheckpoint
                            false
                            true
                            (Some frontendVerified)
                        traceStep
                            PipelineTraceEvent.LowerKCore
                            PipelineTraceSubject.Module
                            $"lower {label} to KCore"
                            (KFrontIRPhase.checkpointName CORE_LOWERING)
                            "KCore"
                            true
                            false
                            None
                        traceStep
                            PipelineTraceEvent.Verify
                            PipelineTraceSubject.KCoreUnit
                            $"verify {label} at KCore"
                            "KCore"
                            "KCore"
                            false
                            true
                            (Some coreVerified)
                        traceStep
                            PipelineTraceEvent.LowerKRuntimeIR
                            PipelineTraceSubject.KCoreUnit
                            $"lower {label} to KRuntimeIR"
                            "KCore"
                            "KRuntimeIR"
                            true
                            false
                            None
                        traceStep
                            PipelineTraceEvent.Verify
                            PipelineTraceSubject.KRuntimeIRUnit
                            $"verify {label} at KRuntimeIR"
                            "KRuntimeIR"
                            "KRuntimeIR"
                            false
                            true
                            (Some runtimeVerified)
                        traceStep
                            PipelineTraceEvent.LowerKBackendIR
                            PipelineTraceSubject.KRuntimeIRUnit
                            $"lower {label} to KBackendIR"
                            "KRuntimeIR"
                            "KBackendIR"
                            true
                            false
                            None
                        traceStep
                            PipelineTraceEvent.Verify
                            PipelineTraceSubject.KBackendIRUnit
                            $"verify {label} at KBackendIR"
                            "KBackendIR"
                            "KBackendIR"
                            false
                            true
                            (Some backendVerified)
                    ]

                parseSteps @ phaseSteps @ verifySteps)

        let targetSteps =
            targetCheckpointNames workspace
            |> List.collect (fun checkpoint ->
                let targetVerified = verifyTargetCheckpoint workspace checkpoint |> List.isEmpty

                [
                    traceStep
                        PipelineTraceEvent.LowerTarget
                        PipelineTraceSubject.TargetUnit
                        $"lower KBackendIR to {checkpoint}"
                        "KBackendIR"
                        checkpoint
                        true
                        false
                        None
                    traceStep
                        PipelineTraceEvent.Verify
                        PipelineTraceSubject.TargetUnit
                        $"verify target at {checkpoint}"
                        checkpoint
                        checkpoint
                        false
                        true
                        (Some targetVerified)
                ])

        documentSteps @ targetSteps

    let private dumpStageJson (workspace: WorkspaceCompilation) checkpoint =
        match checkpoint with
        | "surface-source" ->
            let documents =
                workspace.Documents
                |> List.sortBy (fun document -> document.Source.FilePath)
                |> List.map dumpSourceDocument

            serializeJson
                {| schemaVersion = "1"
                   languageVersion = languageVersion
                   compiler = {| id = compilerImplementationId; version = compilerImplementationVersion |}
                   checkpoint = checkpoint
                   compilationRoot = workspace.SourceRoot
                   backendProfile = workspace.BackendProfile
                   backendIntrinsicSet = workspace.BackendIntrinsicIdentity
                   elaborationAvailableIntrinsicTerms = workspace.ElaborationAvailableIntrinsicTerms
                   buildConfiguration = buildConfigurationJson workspace
                   checkpointContract = checkpointContractJson workspace checkpoint
                   documents = documents
                   diagnostics = workspace.Diagnostics |> List.map dumpDiagnostic |}
        | "KCore" ->
            let modules =
                workspace.KCore
                |> List.sortBy (fun moduleDump -> moduleDump.SourceFile)
                |> List.map dumpCoreModule

            serializeJson
                {| schemaVersion = "1"
                   languageVersion = languageVersion
                   compiler = {| id = compilerImplementationId; version = compilerImplementationVersion |}
                   checkpoint = checkpoint
                   compilationRoot = workspace.SourceRoot
                   backendProfile = workspace.BackendProfile
                   backendIntrinsicSet = workspace.BackendIntrinsicIdentity
                   elaborationAvailableIntrinsicTerms = workspace.ElaborationAvailableIntrinsicTerms
                   buildConfiguration = buildConfigurationJson workspace
                   checkpointContract = checkpointContractJson workspace checkpoint
                   modules = modules
                   diagnostics = workspace.Diagnostics |> List.map dumpDiagnostic |}
        | "KRuntimeIR" ->
            let modules =
                workspace.KRuntimeIR
                |> List.sortBy (fun moduleDump -> moduleDump.SourceFile)
                |> List.map dumpRuntimeModule

            serializeJson
                {| schemaVersion = "1"
                   languageVersion = languageVersion
                   compiler = {| id = compilerImplementationId; version = compilerImplementationVersion |}
                   checkpoint = checkpoint
                   compilationRoot = workspace.SourceRoot
                   backendProfile = workspace.BackendProfile
                   backendIntrinsicSet = workspace.BackendIntrinsicIdentity
                   elaborationAvailableIntrinsicTerms = workspace.ElaborationAvailableIntrinsicTerms
                   buildConfiguration = buildConfigurationJson workspace
                   checkpointContract = checkpointContractJson workspace checkpoint
                   modules = modules
                   diagnostics = workspace.Diagnostics |> List.map dumpDiagnostic |}
        | "KBackendIR" ->
            let modules =
                workspace.KBackendIR
                |> List.sortBy (fun moduleDump -> moduleDump.SourceFile)
                |> List.map dumpBackendModule

            serializeJson
                {| schemaVersion = "1"
                   languageVersion = languageVersion
                   compiler = {| id = compilerImplementationId; version = compilerImplementationVersion |}
                   checkpoint = checkpoint
                   compilationRoot = workspace.SourceRoot
                   backendProfile = workspace.BackendProfile
                   backendIntrinsicSet = workspace.BackendIntrinsicIdentity
                   elaborationAvailableIntrinsicTerms = workspace.ElaborationAvailableIntrinsicTerms
                   buildConfiguration = buildConfigurationJson workspace
                   checkpointContract = checkpointContractJson workspace checkpoint
                   modules = modules
                   diagnostics = workspace.Diagnostics |> List.map dumpDiagnostic |}
        | _ ->
            match CheckpointVerification.tryParseCheckpoint checkpoint with
            | Some(Some phase) ->
                let documents =
                    workspace.KFrontIR
                    |> List.sortBy (fun document -> document.FilePath)
                    |> List.map dumpFrontendDocument

                serializeJson
                    {| schemaVersion = "1"
                       languageVersion = languageVersion
                       compiler = {| id = compilerImplementationId; version = compilerImplementationVersion |}
                       checkpoint = checkpoint
                       phase = KFrontIRPhase.phaseName phase
                       compilationRoot = workspace.SourceRoot
                       backendProfile = workspace.BackendProfile
                       backendIntrinsicSet = workspace.BackendIntrinsicIdentity
                       elaborationAvailableIntrinsicTerms = workspace.ElaborationAvailableIntrinsicTerms
                       buildConfiguration = buildConfigurationJson workspace
                       checkpointContract = checkpointContractJson workspace checkpoint
                       documents = documents
                       diagnostics = workspace.Diagnostics |> List.map dumpDiagnostic |}
            | _ ->
                invalidOp $"Unknown checkpoint '{checkpoint}'."

    let private dumpStageSexpr (workspace: WorkspaceCompilation) checkpoint =
        match checkpoint with
        | "surface-source" ->
            let documents =
                workspace.Documents
                |> List.sortBy (fun document -> document.Source.FilePath)
                |> List.map dumpSourceDocument
                |> List.map renderDumpSourceDocumentSexpr
                |> String.concat " "

            let diagnostics =
                workspace.Diagnostics
                |> List.map dumpDiagnostic
                |> List.map renderDumpDiagnosticSexpr
                |> String.concat " "

            let documentsAtom =
                if String.IsNullOrWhiteSpace(documents) then "(documents)" else $"(documents {documents})"

            let diagnosticsAtom =
                if String.IsNullOrWhiteSpace(diagnostics) then "(diagnostics)" else $"(diagnostics {diagnostics})"

            $"(stage-dump {metadataSexpr workspace checkpoint} {documentsAtom} {diagnosticsAtom})"
        | "KCore" ->
            let modules =
                workspace.KCore
                |> List.sortBy (fun moduleDump -> moduleDump.SourceFile)
                |> List.map dumpCoreModule
                |> List.map renderDumpCoreModuleSexpr
                |> String.concat " "

            let diagnostics =
                workspace.Diagnostics
                |> List.map dumpDiagnostic
                |> List.map renderDumpDiagnosticSexpr
                |> String.concat " "

            let modulesAtom =
                if String.IsNullOrWhiteSpace(modules) then "(modules)" else $"(modules {modules})"

            let diagnosticsAtom =
                if String.IsNullOrWhiteSpace(diagnostics) then "(diagnostics)" else $"(diagnostics {diagnostics})"

            $"(stage-dump {metadataSexpr workspace checkpoint} {modulesAtom} {diagnosticsAtom})"
        | "KRuntimeIR" ->
            let modules =
                workspace.KRuntimeIR
                |> List.sortBy (fun moduleDump -> moduleDump.SourceFile)
                |> List.map dumpRuntimeModule
                |> List.map renderDumpRuntimeModuleSexpr
                |> String.concat " "

            let diagnostics =
                workspace.Diagnostics
                |> List.map dumpDiagnostic
                |> List.map renderDumpDiagnosticSexpr
                |> String.concat " "

            let modulesAtom =
                if String.IsNullOrWhiteSpace(modules) then "(modules)" else $"(modules {modules})"

            let diagnosticsAtom =
                if String.IsNullOrWhiteSpace(diagnostics) then "(diagnostics)" else $"(diagnostics {diagnostics})"

            $"(stage-dump {metadataSexpr workspace checkpoint} {modulesAtom} {diagnosticsAtom})"
        | "KBackendIR" ->
            let modules =
                workspace.KBackendIR
                |> List.sortBy (fun moduleDump -> moduleDump.SourceFile)
                |> List.map dumpBackendModule
                |> List.map renderDumpBackendModuleSexpr
                |> String.concat " "

            let diagnostics =
                workspace.Diagnostics
                |> List.map dumpDiagnostic
                |> List.map renderDumpDiagnosticSexpr
                |> String.concat " "

            let modulesAtom =
                if String.IsNullOrWhiteSpace(modules) then "(modules)" else $"(modules {modules})"

            let diagnosticsAtom =
                if String.IsNullOrWhiteSpace(diagnostics) then "(diagnostics)" else $"(diagnostics {diagnostics})"

            $"(stage-dump {metadataSexpr workspace checkpoint} {modulesAtom} {diagnosticsAtom})"
        | _ ->
            match CheckpointVerification.tryParseCheckpoint checkpoint with
            | Some(Some phase) ->
                let documents =
                    workspace.KFrontIR
                    |> List.sortBy (fun document -> document.FilePath)
                    |> List.map dumpFrontendDocument
                    |> List.map renderDumpDocumentSexpr
                    |> String.concat " "

                let diagnostics =
                    workspace.Diagnostics
                    |> List.map dumpDiagnostic
                    |> List.map renderDumpDiagnosticSexpr
                    |> String.concat " "

                let phaseAtom = sexprStringAtom "phase" (KFrontIRPhase.phaseName phase)

                let documentsAtom =
                    if String.IsNullOrWhiteSpace(documents) then "(documents)" else $"(documents {documents})"

                let diagnosticsAtom =
                    if String.IsNullOrWhiteSpace(diagnostics) then "(diagnostics)" else $"(diagnostics {diagnostics})"

                $"(stage-dump {metadataSexpr workspace checkpoint} {phaseAtom} {documentsAtom} {diagnosticsAtom})"
            | _ ->
                invalidOp $"Unknown checkpoint '{checkpoint}'."

    let private defaultDeploymentModeForBackendProfile backendProfile =
        match Stdlib.normalizeBackendProfile backendProfile with
        | "dotnet"
        | "dotnet-il"
        | "hosted-dotnet" -> "managed"
        | "zig" -> "executable"
        | _ -> "default"

    let private normalizeDeploymentMode backendProfile deploymentMode =
        if
            String.IsNullOrWhiteSpace(deploymentMode)
            || String.Equals(deploymentMode.Trim(), "default", StringComparison.OrdinalIgnoreCase)
        then
            defaultDeploymentModeForBackendProfile backendProfile
        else
            deploymentMode.Trim().ToLowerInvariant()

    let private makeBuildConfigurationIdentity
        packageMode
        backendProfile
        deploymentMode
        backendIntrinsicIdentity
        elaborationAvailableIntrinsicTerms
        =
        let elaborationTerms =
            elaborationAvailableIntrinsicTerms |> String.concat ","

        let packageModeText =
            if packageMode then "true" else "false"

        [
            $"packageMode={packageModeText}"
            $"backendProfile={backendProfile}"
            $"backendIntrinsicSet={backendIntrinsicIdentity}"
            $"deploymentMode={deploymentMode}"
            $"elaborationAvailableIntrinsicTerms=[{elaborationTerms}]"
        ]
        |> String.concat ";"

    let parse (options: CompilationOptions) inputs =
        let normalizedBackendProfile = Stdlib.normalizeBackendProfile options.BackendProfile
        let deploymentMode = normalizeDeploymentMode normalizedBackendProfile options.DeploymentMode
        let backendIntrinsicSet = Stdlib.intrinsicSetForBackendProfile normalizedBackendProfile
        let backendIntrinsicIdentity = backendIntrinsicSet.Identity
        let elaborationAvailableIntrinsicTerms =
            backendIntrinsicSet.ElaborationAvailableTermNames
            |> Set.toList
            |> List.sort
        let buildConfigurationIdentity =
            makeBuildConfigurationIdentity
                options.PackageMode
                normalizedBackendProfile
                deploymentMode
                backendIntrinsicIdentity
                elaborationAvailableIntrinsicTerms

        let analysisSessionIdentity =
            $"sourceRoot={options.SourceRoot};{buildConfigurationIdentity}"

        let userDocuments =
            collectInputFiles options inputs
            |> List.map (parseFile options)

        let documents =
            if userDocuments |> List.exists (fun document -> document.ModuleName = Some Stdlib.PreludeModuleName) then
                userDocuments
            else
                parseBundledPrelude () :: userDocuments

        let frontendDiagnostics =
            (documents |> List.collect (fun document -> document.Diagnostics))
            @ detectImportCycles documents
            @ validateExpectDeclarations normalizedBackendProfile documents

        let resourceCheckResult: ResourceChecking.CheckResult =
            if frontendDiagnostics |> List.exists (fun diagnostic -> diagnostic.Severity = Error) then
                { Diagnostics = []
                  OwnershipFactsByFile = Map.empty }
            else
                ResourceChecking.checkDocumentsWithFacts documents

        let diagnostics =
            frontendDiagnostics @ resourceCheckResult.Diagnostics

        let kFrontIR =
            documents
            |> List.map (buildKFrontIRModule resourceCheckResult.OwnershipFactsByFile)
            |> List.sortBy (fun document -> document.FilePath)

        let kCore =
            SurfaceElaboration.lowerKCoreModules normalizedBackendProfile kFrontIR
            |> List.sortBy (fun moduleDump -> moduleDump.SourceFile)

        let kRuntimeIR =
            kCore
            |> List.map KRuntimeLowering.lowerKRuntimeModule
            |> List.sortBy (fun moduleDump -> moduleDump.SourceFile)

        let kBackendIR =
            KBackendLowering.lowerKBackendModules normalizedBackendProfile kCore kRuntimeIR
            |> List.sortBy (fun moduleDump -> moduleDump.SourceFile)

        let workspace =
            { SourceRoot = options.SourceRoot
              PackageMode = options.PackageMode
              BackendProfile = normalizedBackendProfile
              DeploymentMode = deploymentMode
              BackendIntrinsicIdentity = backendIntrinsicIdentity
              BuildConfigurationIdentity = buildConfigurationIdentity
              AnalysisSessionIdentity = analysisSessionIdentity
              ElaborationAvailableIntrinsicTerms = elaborationAvailableIntrinsicTerms
              Documents = documents
              KFrontIR = kFrontIR
              KCore = kCore
              KRuntimeIR = kRuntimeIR
              KBackendIR = kBackendIR
              Diagnostics = diagnostics
              PipelineTrace = [] }

        { workspace with
            PipelineTrace = buildPipelineTrace workspace }

    let checkpointContracts (workspace: WorkspaceCompilation) =
        contractsForWorkspace workspace

    let availableCheckpoints (workspace: WorkspaceCompilation) =
        checkpointContracts workspace
        |> List.map (fun contract -> contract.Name)

    let verifyCheckpoint (workspace: WorkspaceCompilation) checkpoint =
        if targetCheckpointNames workspace |> List.contains checkpoint then
            verifyTargetCheckpoint workspace checkpoint
        else
            CheckpointVerification.verifyCheckpoint workspace checkpoint

    let verifyAllCheckpoints (workspace: WorkspaceCompilation) =
        checkpointContracts workspace
        |> List.map (fun contract ->
            let diagnostics = verifyCheckpoint workspace contract.Name

            { Checkpoint = contract.Name
              Succeeded = List.isEmpty diagnostics
              Diagnostics = diagnostics })

    let portableRuntimeObligations (_workspace: WorkspaceCompilation) =
        [
            { Name = "tagged-data-layout"
              Owner = KBackendIRGuaranteed
              Description = "KBackendIR fixes tagged ADT layout, constructor tags, and field representation classes before target lowering." }
            { Name = "runtime-calling-convention"
              Owner = KBackendIRGuaranteed
              Description = "KBackendIR functions, closures, calls, and entrypoints carry fixed runtime arity and representation-level calling conventions." }
            { Name = "retained-dictionaries-and-intrinsics"
              Owner = KBackendIRGuaranteed
              Description = "Retained dictionaries and runtime backend intrinsics must have concrete KBackendIR representations or verification rejects the checkpoint." }
            { Name = "memory-management"
              Owner = BackendSpecificRuntime
              Description = "Allocation and collection strategy is backend-specific; target lowering must preserve KBackendIR value and lifetime semantics." }
            { Name = "deterministic-cleanup"
              Owner = DeferredRuntimeObligation
              Description = "Cleanup scopes, defer, and using/finally lowering are deferred until M3 introduces resource tracking and cleanup forms." }
            { Name = "effect-handlers"
              Owner = DeferredRuntimeObligation
              Description = "Handler frames and resumptions are deferred until the post-M3 effect-handler milestone." }
        ]

    let analysisSession (workspace: WorkspaceCompilation) =
        { Identity = workspace.AnalysisSessionIdentity
          SourceRoot = workspace.SourceRoot
          PackageMode = workspace.PackageMode
          BuildConfigurationIdentity = workspace.BuildConfigurationIdentity
          BackendProfile = workspace.BackendProfile
          BackendIntrinsicSet = workspace.BackendIntrinsicIdentity
          DeploymentMode = workspace.DeploymentMode }

    let private queryId queryKind inputKey outputCheckpoint =
        $"{QueryKind.toPortableName queryKind}:{inputKey}->{outputCheckpoint}"

    let private queryRecord
        (workspace: WorkspaceCompilation)
        queryKind
        inputKey
        outputCheckpoint
        requiredPhase
        dependencyIds
        =
        { Id = queryId queryKind inputKey outputCheckpoint
          QueryKind = queryKind
          InputKey = inputKey
          OutputCheckpoint = outputCheckpoint
          RequiredPhase = requiredPhase
          AnalysisSessionIdentity = workspace.AnalysisSessionIdentity
          BuildConfigurationIdentity = workspace.BuildConfigurationIdentity
          BackendProfile = workspace.BackendProfile
          BackendIntrinsicSet = workspace.BackendIntrinsicIdentity
          DependencyIds = dependencyIds }

    let queryPlan (workspace: WorkspaceCompilation) =
        let documents =
            workspace.KFrontIR
            |> List.sortBy (fun document -> document.FilePath)

        let documentQueries =
            documents
            |> List.collect (fun document ->
                let inputKey = document.FilePath
                let parse =
                    queryRecord workspace ParseSourceFileQuery inputKey "surface-source" None []

                let raw =
                    queryRecord
                        workspace
                        BuildKFrontIRQuery
                        inputKey
                        (KFrontIRPhase.checkpointName RAW)
                        (Some RAW)
                        [ parse.Id ]

                let phaseQueries =
                    ((raw, []), KFrontIRPhase.all |> List.tail)
                    ||> List.fold (fun (previous, collected) phase ->
                        let checkpoint = KFrontIRPhase.checkpointName phase

                        let current =
                            queryRecord
                                workspace
                                AdvanceKFrontIRPhaseQuery
                                inputKey
                                checkpoint
                                (Some phase)
                                [ previous.Id ]

                        current, collected @ [ current ])
                    |> snd

                let diagnostics =
                    let checkers =
                        phaseQueries
                        |> List.find (fun query -> query.OutputCheckpoint = KFrontIRPhase.checkpointName CHECKERS)

                    queryRecord
                        workspace
                        ComputeDiagnosticsQuery
                        inputKey
                        $"{KFrontIRPhase.checkpointName CHECKERS}.diagnostics"
                        (Some CHECKERS)
                        [ checkers.Id ]

                let core =
                    let coreLowering =
                        phaseQueries
                        |> List.find (fun query -> query.OutputCheckpoint = KFrontIRPhase.checkpointName CORE_LOWERING)

                    queryRecord workspace LowerKCoreQuery inputKey "KCore" (Some CORE_LOWERING) [ coreLowering.Id ]

                let runtime =
                    queryRecord workspace LowerKRuntimeIRQuery inputKey "KRuntimeIR" None [ core.Id ]

                let backend =
                    queryRecord workspace LowerKBackendIRQuery inputKey "KBackendIR" None [ runtime.Id ]

                [ parse; raw ] @ phaseQueries @ [ diagnostics; core; runtime; backend ])

        let backendDependencies =
            documentQueries
            |> List.filter (fun query -> query.QueryKind = LowerKBackendIRQuery)
            |> List.map (fun query -> query.Id)

        let targetQueries =
            targetCheckpointNames workspace
            |> List.map (fun checkpoint ->
                queryRecord
                    workspace
                    LowerTargetQuery
                    workspace.SourceRoot
                    checkpoint
                    None
                    backendDependencies)

        documentQueries @ targetQueries

    let private fingerprintId fingerprintKind inputKey =
        $"{CompilerFingerprintKind.toPortableName fingerprintKind}:{inputKey}"

    let private fingerprintRecord
        (workspace: WorkspaceCompilation)
        fingerprintKind
        inputKey
        identity
        dependencyFingerprintIds
        =
        { Id = fingerprintId fingerprintKind inputKey
          FingerprintKind = fingerprintKind
          InputKey = inputKey
          Identity = identity
          AnalysisSessionIdentity = workspace.AnalysisSessionIdentity
          BuildConfigurationIdentity = workspace.BuildConfigurationIdentity
          BackendProfile = workspace.BackendProfile
          BackendIntrinsicSet = workspace.BackendIntrinsicIdentity
          DependencyFingerprintIds = dependencyFingerprintIds }

    let private declarationInputKey filePath index declaration =
        let kind = declarationKindText declaration
        let name = declarationName declaration |> Option.defaultValue "<anonymous>"
        $"{filePath}#declaration:{index}:{kind}:{name}"

    let private parameterHeaderIdentity (parameter: Parameter) =
        match parameter.TypeTokens with
        | Some typeTokens -> $"{parameter.Name}:{tokensText typeTokens}"
        | None -> parameter.Name

    let private declarationHeaderIdentity declaration =
        let name = declarationName declaration |> Option.defaultValue "<anonymous>"
        let visibility = declarationVisibility declaration |> Option.defaultValue "<default>"
        let typeText = declarationTypeText declaration |> Option.defaultValue "<none>"

        let commonPrefix =
            [
                $"kind={declarationKindText declaration}"
                $"name={name}"
                $"visibility={visibility}"
                $"opaque={declarationIsOpaque declaration}"
                $"type={typeText}"
            ]
            |> String.concat ";"

        let shape =
            match declaration with
            | ImportDeclaration (isExport, specs) ->
                let keyword = if isExport then "export" else "import"
                let specsText = specs |> List.map importSpecText |> String.concat ","
                $"keyword={keyword};specs=[{specsText}]"
            | FixityDeclarationNode fixity ->
                $"precedence={fixity.Precedence};operator={fixity.OperatorName}"
            | ExpectDeclarationNode (ExpectTypeDeclaration expected) ->
                $"expect=type;header={tokensText expected.HeaderTokens}"
            | ExpectDeclarationNode (ExpectTraitDeclaration expected) ->
                $"expect=trait;header={tokensText expected.HeaderTokens}"
            | ExpectDeclarationNode (ExpectTermDeclaration expected) ->
                $"expect=term;type={tokensText expected.TypeTokens}"
            | SignatureDeclaration signature ->
                $"signatureType={tokensText signature.TypeTokens}"
            | LetDeclaration definition ->
                let parameters =
                    definition.Parameters
                    |> List.map parameterHeaderIdentity
                    |> String.concat ","

                let returnType =
                    definition.ReturnTypeTokens
                    |> Option.map tokensText
                    |> Option.defaultValue "<inferred>"

                $"parameters=[{parameters}];header={tokensText definition.HeaderTokens};return={returnType}"
            | DataDeclarationNode dataDeclaration ->
                let constructors =
                    dataDeclaration.Constructors
                    |> List.map (fun constructor ->
                        $"{constructor.Name}/{constructor.Arity}:{tokensText constructor.Tokens}")
                    |> String.concat ","

                $"header={tokensText dataDeclaration.HeaderTokens};constructors=[{constructors}]"
            | TypeAliasNode alias ->
                $"header={tokensText alias.HeaderTokens}"
            | TraitDeclarationNode traitDeclaration ->
                let members =
                    traitDeclaration.Members
                    |> List.map (fun memberDeclaration ->
                        let memberName = memberDeclaration.Name |> Option.defaultValue "<anonymous>"
                        $"{memberName}:{tokensText memberDeclaration.Tokens}")
                    |> String.concat ","

                $"header={tokensText traitDeclaration.HeaderTokens};members=[{members}]"
            | InstanceDeclarationNode instanceDeclaration ->
                let members =
                    instanceDeclaration.Members
                    |> List.map (fun definition ->
                        let memberName = definition.Name |> Option.defaultValue "<anonymous>"
                        $"{memberName}:{tokensText definition.HeaderTokens}")
                    |> String.concat ","

                $"trait={instanceDeclaration.TraitName};header={tokensText instanceDeclaration.HeaderTokens};members=[{members}]"
            | UnknownDeclaration tokens ->
                $"tokens={tokensText tokens}"

        $"header:{commonPrefix};{shape}"

    let private declarationBodyIdentity declaration =
        let body =
            match declaration with
            | LetDeclaration definition ->
                declarationBodyText declaration
                |> Option.defaultValue (tokensText definition.BodyTokens)
            | TypeAliasNode alias ->
                alias.BodyTokens
                |> Option.map tokensText
                |> Option.defaultValue "<none>"
            | InstanceDeclarationNode instanceDeclaration ->
                instanceDeclaration.Members
                |> List.map (fun definition ->
                    let memberName = definition.Name |> Option.defaultValue "<anonymous>"

                    let bodyText =
                        definition.Body
                        |> Option.map expressionText
                        |> Option.defaultValue (tokensText definition.BodyTokens)

                    $"{memberName}={bodyText}")
                |> String.concat ";"
            | _ ->
                "<none>"

        let name = declarationName declaration |> Option.defaultValue "<anonymous>"
        $"body:kind={declarationKindText declaration};name={name};body={body}"

    let private moduleInterfaceIdentity (frontendModule: KFrontIRModule) =
        let imports =
            frontendModule.Imports
            |> List.map importSpecText
            |> String.concat ","

        let declarations =
            frontendModule.Declarations
            |> List.map declarationHeaderIdentity
            |> String.concat "|"

        $"interface:module={moduleNameText frontendModule.ModuleIdentity};imports=[{imports}];declarations=[{declarations}]"

    let private backendFingerprintIdentity (workspace: WorkspaceCompilation) (backendModule: KBackendModule) =
        let imports =
            backendModule.Imports
            |> List.map importSpecText
            |> String.concat ","

        let layouts =
            backendModule.DataLayouts
            |> List.map (fun layout ->
                let constructors =
                    layout.Constructors
                    |> List.map (fun constructor ->
                        let fieldRepresentations =
                            constructor.FieldRepresentations
                            |> List.map (fun representation -> $"{representation}")
                            |> String.concat ","

                        $"{constructor.Name}/{constructor.Tag}({fieldRepresentations})")
                    |> String.concat ","

                $"{layout.TypeName}:{layout.RepresentationClass}:{layout.TagEncoding}[{constructors}]")
            |> String.concat "|"

        let functions =
            backendModule.Functions
            |> List.map (fun fn ->
                let parameters =
                    fn.Parameters
                    |> List.map (fun parameter -> $"{parameter.Name}:{parameter.Representation}")
                    |> String.concat ","

                let returnRepresentation =
                    fn.ReturnRepresentation
                    |> Option.map (fun representation -> $"{representation}")
                    |> Option.defaultValue "<unit>"

                $"{fn.Name}({parameters}):{returnRepresentation}:intrinsic={fn.Intrinsic}:exported={fn.Exported}:entry={fn.EntryPoint}")
            |> String.concat "|"

        let entryPoints = String.concat "," backendModule.EntryPoints

        [
            $"backend:compiler={compilerImplementationId}/{compilerImplementationVersion}"
            $"profile={workspace.BackendProfile}"
            $"build={workspace.BuildConfigurationIdentity}"
            $"intrinsics={workspace.BackendIntrinsicIdentity}"
            $"module={backendModule.Name}"
            $"imports=[{imports}]"
            $"entryPoints=[{entryPoints}]"
            $"layouts=[{layouts}]"
            $"functions=[{functions}]"
        ]
        |> String.concat ";"

    let compilerFingerprints (workspace: WorkspaceCompilation) =
        let sourceFingerprints =
            workspace.Documents
            |> List.sortBy (fun document -> document.Source.FilePath)
            |> List.map (fun document ->
                let inputKey = document.Source.FilePath

                let identity =
                    [
                        $"source:path={document.Source.FilePath}"
                        $"length={document.Source.Length}"
                        $"lines={document.Source.LineCount}"
                        $"module={moduleNameText document.ModuleName}"
                    ]
                    |> String.concat ";"

                fingerprintRecord workspace SourceFingerprint inputKey identity [])

        let sourceFingerprintByFile =
            sourceFingerprints
            |> List.map (fun fingerprint -> fingerprint.InputKey, fingerprint)
            |> Map.ofList

        let declarationFingerprints =
            workspace.KFrontIR
            |> List.sortBy (fun frontendModule -> frontendModule.FilePath)
            |> List.collect (fun frontendModule ->
                frontendModule.Declarations
                |> List.mapi (fun index declaration ->
                    let inputKey = declarationInputKey frontendModule.FilePath index declaration
                    let sourceDependency = sourceFingerprintByFile[frontendModule.FilePath].Id

                    let header =
                        fingerprintRecord
                            workspace
                            HeaderFingerprint
                            inputKey
                            (declarationHeaderIdentity declaration)
                            [ sourceDependency ]

                    let body =
                        fingerprintRecord
                            workspace
                            BodyFingerprint
                            inputKey
                            (declarationBodyIdentity declaration)
                            [ header.Id ]

                    [ header; body ])
                |> List.concat)

        let declarationFingerprintIdsByFile =
            declarationFingerprints
            |> List.groupBy (fun fingerprint ->
                let marker = "#declaration:"
                let markerIndex = fingerprint.InputKey.IndexOf(marker, StringComparison.Ordinal)

                if markerIndex < 0 then
                    fingerprint.InputKey
                else
                    fingerprint.InputKey.Substring(0, markerIndex))
            |> Map.ofList

        let interfaceFingerprints =
            workspace.KFrontIR
            |> List.sortBy (fun frontendModule -> frontendModule.FilePath)
            |> List.map (fun frontendModule ->
                let sourceDependency = sourceFingerprintByFile[frontendModule.FilePath].Id

                let declarationDependencies =
                    declarationFingerprintIdsByFile
                    |> Map.tryFind frontendModule.FilePath
                    |> Option.defaultValue []
                    |> List.filter (fun fingerprint -> fingerprint.FingerprintKind = HeaderFingerprint)
                    |> List.map (fun fingerprint -> fingerprint.Id)

                fingerprintRecord
                    workspace
                    InterfaceFingerprint
                    frontendModule.FilePath
                    (moduleInterfaceIdentity frontendModule)
                    (sourceDependency :: declarationDependencies))

        let interfaceFingerprintByFile =
            interfaceFingerprints
            |> List.map (fun fingerprint -> fingerprint.InputKey, fingerprint)
            |> Map.ofList

        let bodyFingerprintIdsByFile =
            declarationFingerprints
            |> List.filter (fun fingerprint -> fingerprint.FingerprintKind = BodyFingerprint)
            |> List.groupBy (fun fingerprint ->
                let marker = "#declaration:"
                let markerIndex = fingerprint.InputKey.IndexOf(marker, StringComparison.Ordinal)

                if markerIndex < 0 then
                    fingerprint.InputKey
                else
                    fingerprint.InputKey.Substring(0, markerIndex))
            |> Map.ofList

        let backendFingerprints =
            workspace.KBackendIR
            |> List.sortBy (fun backendModule -> backendModule.SourceFile)
            |> List.map (fun backendModule ->
                let interfaceDependency = interfaceFingerprintByFile[backendModule.SourceFile].Id

                let bodyDependencies =
                    bodyFingerprintIdsByFile
                    |> Map.tryFind backendModule.SourceFile
                    |> Option.defaultValue []
                    |> List.map (fun fingerprint -> fingerprint.Id)

                fingerprintRecord
                    workspace
                    BackendFingerprint
                    backendModule.SourceFile
                    (backendFingerprintIdentity workspace backendModule)
                    (interfaceDependency :: bodyDependencies))

        sourceFingerprints @ declarationFingerprints @ interfaceFingerprints @ backendFingerprints

    let private unitId unitKind inputKey =
        $"{IncrementalUnitKind.toPortableName unitKind}:{inputKey}"

    let private incrementalUnit
        (workspace: WorkspaceCompilation)
        unitKind
        inputKey
        outputCheckpoint
        fingerprintIds
        dependencyUnitIds
        =
        { Id = unitId unitKind inputKey
          UnitKind = unitKind
          InputKey = inputKey
          OutputCheckpoint = outputCheckpoint
          FingerprintIds = fingerprintIds
          DependencyUnitIds = dependencyUnitIds
          AnalysisSessionIdentity = workspace.AnalysisSessionIdentity
          BuildConfigurationIdentity = workspace.BuildConfigurationIdentity
          BackendProfile = workspace.BackendProfile
          BackendIntrinsicSet = workspace.BackendIntrinsicIdentity }

    let incrementalUnits (workspace: WorkspaceCompilation) =
        let fingerprints = compilerFingerprints workspace

        let fingerprintById =
            fingerprints
            |> List.map (fun fingerprint -> fingerprint.Id, fingerprint)
            |> Map.ofList

        let fingerprintIdFor fingerprintKind inputKey =
            fingerprintId fingerprintKind inputKey

        let sourceUnitByFile =
            workspace.Documents
            |> List.sortBy (fun document -> document.Source.FilePath)
            |> List.map (fun document ->
                let inputKey = document.Source.FilePath

                let unit =
                    incrementalUnit
                        workspace
                        SourceFileTextUnit
                        inputKey
                        (Some "surface-source")
                        [ fingerprintIdFor SourceFingerprint inputKey ]
                        []

                inputKey, unit)
            |> Map.ofList

        let importSurfaceUnits =
            workspace.KFrontIR
            |> List.sortBy (fun frontendModule -> frontendModule.FilePath)
            |> List.map (fun frontendModule ->
                incrementalUnit
                    workspace
                    ModuleImportSurfaceUnit
                    frontendModule.FilePath
                    (Some (KFrontIRPhase.checkpointName IMPORTS))
                    [ fingerprintIdFor SourceFingerprint frontendModule.FilePath ]
                    [ sourceUnitByFile[frontendModule.FilePath].Id ])

        let importSurfaceUnitByFile =
            importSurfaceUnits
            |> List.map (fun unit -> unit.InputKey, unit)
            |> Map.ofList

        let declarationUnits =
            workspace.KFrontIR
            |> List.sortBy (fun frontendModule -> frontendModule.FilePath)
            |> List.collect (fun frontendModule ->
                frontendModule.Declarations
                |> List.mapi (fun index declaration ->
                    let inputKey = declarationInputKey frontendModule.FilePath index declaration

                    let headerUnit =
                        incrementalUnit
                            workspace
                            DeclarationHeaderUnit
                            inputKey
                            (Some (KFrontIRPhase.checkpointName IMPLICIT_SIGNATURES))
                            [ fingerprintIdFor HeaderFingerprint inputKey ]
                            [ importSurfaceUnitByFile[frontendModule.FilePath].Id ]

                    let macroExpansionUnit =
                        incrementalUnit
                            workspace
                            MacroExpansionUnit
                            inputKey
                            (Some "macro-expansion")
                            []
                            [ headerUnit.Id ]

                    let bodyUnit =
                        incrementalUnit
                            workspace
                            DeclarationBodyUnit
                            inputKey
                            (Some (KFrontIRPhase.checkpointName BODY_RESOLVE))
                            [ fingerprintIdFor BodyFingerprint inputKey ]
                            [ macroExpansionUnit.Id ]

                    [ headerUnit; macroExpansionUnit; bodyUnit ])
                |> List.concat)

        let unitsByKindAndFile unitKind filePath =
            declarationUnits
            |> List.filter (fun unit ->
                unit.UnitKind = unitKind
                && unit.InputKey.StartsWith(filePath + "#declaration:", StringComparison.Ordinal))

        let moduleInterfaceUnits =
            workspace.KFrontIR
            |> List.sortBy (fun frontendModule -> frontendModule.FilePath)
            |> List.map (fun frontendModule ->
                let headerDependencies =
                    unitsByKindAndFile DeclarationHeaderUnit frontendModule.FilePath
                    |> List.map (fun unit -> unit.Id)

                incrementalUnit
                    workspace
                    ModuleInterfaceUnit
                    frontendModule.FilePath
                    (Some "module-interface")
                    [ fingerprintIdFor InterfaceFingerprint frontendModule.FilePath ]
                    (importSurfaceUnitByFile[frontendModule.FilePath].Id :: headerDependencies))

        let moduleInterfaceUnitByFile =
            moduleInterfaceUnits
            |> List.map (fun unit -> unit.InputKey, unit)
            |> Map.ofList

        let kCoreUnits =
            workspace.KCore
            |> List.sortBy (fun coreModule -> coreModule.SourceFile)
            |> List.map (fun coreModule ->
                let bodyDependencies =
                    unitsByKindAndFile DeclarationBodyUnit coreModule.SourceFile
                    |> List.map (fun unit -> unit.Id)

                incrementalUnit
                    workspace
                    KCoreModuleUnit
                    coreModule.SourceFile
                    (Some "KCore")
                    []
                    (moduleInterfaceUnitByFile[coreModule.SourceFile].Id :: bodyDependencies))

        let kCoreUnitByFile =
            kCoreUnits
            |> List.map (fun unit -> unit.InputKey, unit)
            |> Map.ofList

        let kBackendUnits =
            workspace.KBackendIR
            |> List.sortBy (fun backendModule -> backendModule.SourceFile)
            |> List.map (fun backendModule ->
                incrementalUnit
                    workspace
                    KBackendIRModuleUnit
                    backendModule.SourceFile
                    (Some "KBackendIR")
                    [ fingerprintIdFor BackendFingerprint backendModule.SourceFile ]
                    [ kCoreUnitByFile[backendModule.SourceFile].Id ])

        let kBackendUnitIds =
            kBackendUnits
            |> List.map (fun unit -> unit.Id)

        let backendFingerprintIds =
            fingerprints
            |> List.filter (fun fingerprint -> fingerprint.FingerprintKind = BackendFingerprint)
            |> List.map (fun fingerprint -> fingerprint.Id)

        let targetUnits =
            targetCheckpointNames workspace
            |> List.map (fun checkpoint ->
                let inputKey = $"{workspace.SourceRoot}#target:{checkpoint}"

                incrementalUnit
                    workspace
                    TargetLoweringUnit
                    inputKey
                    (Some checkpoint)
                    backendFingerprintIds
                    kBackendUnitIds)

        let units =
            sourceUnitByFile
            |> Map.toList
            |> List.map snd

        let referencedFingerprintIds =
            units
            @ importSurfaceUnits
            @ declarationUnits
            @ moduleInterfaceUnits
            @ kCoreUnits
            @ kBackendUnits
            @ targetUnits
            |> List.collect (fun unit -> unit.FingerprintIds)

        for fingerprintId in referencedFingerprintIds do
            if not (fingerprintById.ContainsKey fingerprintId) then
                invalidOp $"Internal error: incremental unit references missing compiler fingerprint '{fingerprintId}'."

        units
        @ importSurfaceUnits
        @ declarationUnits
        @ moduleInterfaceUnits
        @ kCoreUnits
        @ kBackendUnits
        @ targetUnits

    let pipelineTrace (workspace: WorkspaceCompilation) =
        workspace.PipelineTrace

    let dumpStage (workspace: WorkspaceCompilation) checkpoint format =
        let available = availableCheckpoints workspace

        if not (available |> List.contains checkpoint) then
            let availableText = String.concat ", " available
            Result.Error $"Unknown checkpoint '{checkpoint}'. Available checkpoints: {availableText}."
        elif targetCheckpointNames workspace |> List.contains checkpoint then
            tryEmitTargetTranslationUnit workspace checkpoint
            |> Result.map (fun translationUnit ->
                match format with
                | StageDumpFormat.Json ->
                    renderTargetCheckpointJson workspace checkpoint translationUnit
                | StageDumpFormat.SExpression ->
                    renderTargetCheckpointSexpr workspace checkpoint translationUnit)
        else
            match format with
            | StageDumpFormat.Json ->
                Result.Ok(dumpStageJson workspace checkpoint)
            | StageDumpFormat.SExpression ->
                Result.Ok(dumpStageSexpr workspace checkpoint)
