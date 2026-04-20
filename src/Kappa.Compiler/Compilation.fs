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
        { Severity: string
          Message: string
          FilePath: string option
          StartLine: int option
          StartColumn: int option
          EndLine: int option
          EndColumn: int option }

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
          Diagnostics: DumpDiagnostic list }

    type DumpCoreModule =
        { Name: string
          SourceFile: string
          Imports: string list
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
                document.Source.GetLocation(TextSpan.FromBounds(0, 0))
            )
        | None, None ->
            diagnostics.AddError(
                $"Could not derive a Kappa module name from '{document.Source.FilePath}'. Files must live under the source root and end in .kp.",
                document.Source.GetLocation(TextSpan.FromBounds(0, 0))
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
                let statementText =
                    statements
                    |> List.map (function
                        | DoLet(name, body) -> $"(let {name} {render body})"
                        | DoBind(name, body) -> $"(<- {name} {render body})"
                        | DoExpression body -> $"(expr {render body})")
                    |> String.concat " "

                $"(do {statementText})"
            | Apply(callee, arguments) ->
                let argumentText =
                    arguments
                    |> List.map render
                    |> String.concat " "

                if String.IsNullOrWhiteSpace(argumentText) then
                    $"(apply {render callee})"
                else
                    $"(apply {render callee} {argumentText})"
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
        | UnknownDeclaration _ -> "unknown"

    let private declarationName (declaration: TopLevelDeclaration) =
        match declaration with
        | SignatureDeclaration declaration -> Some declaration.Name
        | LetDeclaration declaration -> declaration.Name
        | DataDeclarationNode declaration -> Some declaration.Name
        | TypeAliasNode declaration -> Some declaration.Name
        | TraitDeclarationNode declaration -> Some declaration.Name
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

    let private lowerKCoreParameter (parameter: Parameter) =
        { Name = parameter.Name
          TypeText = parameter.TypeTokens |> Option.map tokensText }

    let rec private lowerKCorePattern pattern =
        match pattern with
        | WildcardPattern ->
            KCoreWildcardPattern
        | NamePattern name ->
            KCoreNamePattern name
        | LiteralPattern literal ->
            KCoreLiteralPattern literal
        | ConstructorPattern(name, arguments) ->
            KCoreConstructorPattern(name, arguments |> List.map lowerKCorePattern)

    let rec private desugarDoExpression statements =
        match statements with
        | [] ->
            Literal LiteralValue.Unit
        | [ DoExpression expression ] ->
            expression
        | DoExpression expression :: rest ->
            Apply(Name [ ">>" ], [ expression; desugarDoExpression rest ])
        | DoBind(name, expression) :: rest ->
            Apply(
                Name [ ">>=" ],
                [
                    expression
                    Lambda(
                        [ { Name = name
                            TypeTokens = None } ],
                        desugarDoExpression rest
                    )
                ]
            )
        | DoLet(name, expression) :: rest ->
            Apply(
                Lambda(
                    [ { Name = name
                        TypeTokens = None } ],
                    desugarDoExpression rest
                ),
                [ expression ]
            )

    let rec private lowerKCoreExpression expression =
        match expression with
        | Literal literal ->
            KCoreLiteral literal
        | Name segments ->
            KCoreName segments
        | Lambda(parameters, body) ->
            KCoreLambda(parameters |> List.map lowerKCoreParameter, lowerKCoreExpression body)
        | IfThenElse(condition, whenTrue, whenFalse) ->
            KCoreIfThenElse(
                lowerKCoreExpression condition,
                lowerKCoreExpression whenTrue,
                lowerKCoreExpression whenFalse
            )
        | Match(scrutinee, cases) ->
            KCoreMatch(
                lowerKCoreExpression scrutinee,
                cases
                |> List.map (fun caseClause ->
                    { Pattern = lowerKCorePattern caseClause.Pattern
                      Body = lowerKCoreExpression caseClause.Body })
            )
        | Do statements ->
            lowerKCoreExpression (desugarDoExpression statements)
        | Apply(callee, arguments) ->
            KCoreApply(lowerKCoreExpression callee, arguments |> List.map lowerKCoreExpression)
        | Unary(operatorName, operand) ->
            KCoreUnary(operatorName, lowerKCoreExpression operand)
        | Binary(left, operatorName, right) ->
            KCoreBinary(lowerKCoreExpression left, operatorName, lowerKCoreExpression right)
        | PrefixedString(prefix, parts) ->
            KCorePrefixedString(
                prefix,
                parts
                |> List.map (function
                    | StringText text -> KCoreStringText text
                    | StringInterpolation inner -> KCoreStringInterpolation(lowerKCoreExpression inner))
            )

    let rec private kcorePatternText pattern =
        match pattern with
        | KCoreWildcardPattern -> "_"
        | KCoreNamePattern name -> name
        | KCoreLiteralPattern(LiteralValue.Integer value) -> string value
        | KCoreLiteralPattern(LiteralValue.Float value) -> string value
        | KCoreLiteralPattern(LiteralValue.String value) -> $"\"{value}\""
        | KCoreLiteralPattern(LiteralValue.Character value) -> $"'{value}'"
        | KCoreLiteralPattern LiteralValue.Unit -> "()"
        | KCoreConstructorPattern(name, arguments) ->
            let nameText = String.concat "." name

            match arguments with
            | [] ->
                nameText
            | _ ->
                let argumentText = arguments |> List.map kcorePatternText |> String.concat " "
                $"({nameText} {argumentText})"

    let rec private kcoreExpressionText expression =
        match expression with
        | KCoreLiteral(LiteralValue.Integer value) -> string value
        | KCoreLiteral(LiteralValue.Float value) -> string value
        | KCoreLiteral(LiteralValue.String value) -> $"\"{value}\""
        | KCoreLiteral(LiteralValue.Character value) -> $"'{value}'"
        | KCoreLiteral LiteralValue.Unit -> "()"
        | KCoreName segments -> String.concat "." segments
        | KCoreLambda(parameters, body) ->
            let names = parameters |> List.map (fun parameter -> parameter.Name) |> String.concat " "
            $"(lambda ({names}) {kcoreExpressionText body})"
        | KCoreIfThenElse(condition, whenTrue, whenFalse) ->
            $"(if {kcoreExpressionText condition} {kcoreExpressionText whenTrue} {kcoreExpressionText whenFalse})"
        | KCoreMatch(scrutinee, cases) ->
            let caseText =
                cases
                |> List.map (fun caseClause -> $"(case {kcorePatternText caseClause.Pattern} {kcoreExpressionText caseClause.Body})")
                |> String.concat " "

            $"(match {kcoreExpressionText scrutinee} {caseText})"
        | KCoreApply(callee, arguments) ->
            let argumentText =
                arguments
                |> List.map kcoreExpressionText
                |> String.concat " "

            if String.IsNullOrWhiteSpace(argumentText) then
                $"(apply {kcoreExpressionText callee})"
            else
                $"(apply {kcoreExpressionText callee} {argumentText})"
        | KCoreUnary(operatorName, operand) ->
            $"({operatorName} {kcoreExpressionText operand})"
        | KCoreBinary(left, operatorName, right) ->
            $"({operatorName} {kcoreExpressionText left} {kcoreExpressionText right})"
        | KCorePrefixedString(prefix, parts) ->
            let partText =
                parts
                |> List.map (function
                    | KCoreStringText text -> $"text:{text}"
                    | KCoreStringInterpolation inner -> $"interp:{kcoreExpressionText inner}")
                |> String.concat " | "

            $"({prefix}-string {partText})"

    let private lowerKCoreBinding provenance (definition: LetDefinition) =
        { Visibility = definition.Visibility
          IsOpaque = definition.IsOpaque
          Name = definition.Name
          Parameters = definition.Parameters |> List.map lowerKCoreParameter
          ReturnTypeText = definition.ReturnTypeTokens |> Option.map tokensText
          Body = definition.Body |> Option.map lowerKCoreExpression
          BodyText =
            definition.Body
            |> Option.map (lowerKCoreExpression >> kcoreExpressionText)
            |> Option.orElseWith (fun () ->
                if List.isEmpty definition.BodyTokens then None else Some(tokensText definition.BodyTokens))
          Provenance = provenance }

    let private buildKFrontIRModule (document: ParsedDocument) =
        { FilePath = document.Source.FilePath
          ModuleHeader = document.Syntax.ModuleHeader
          InferredModuleName = document.InferredModuleName
          ModuleIdentity = document.ModuleName
          ModuleAttributes = document.Syntax.ModuleAttributes
          Imports = collectImportSpecs document
          Tokens = document.Syntax.Tokens
          Declarations = document.Syntax.Declarations
          Diagnostics = document.Diagnostics
          ResolvedPhases = Set.ofList KFrontIRPhase.all }

    let private lowerKCoreModule (backendProfile: string) (frontendModule: KFrontIRModule) =
        let moduleName = moduleNameText frontendModule.ModuleIdentity

        let declarations =
            frontendModule.Declarations
            |> List.map (fun declaration ->
                let provenance = declarationOrigin frontendModule.FilePath moduleName declaration

                { Source = declaration
                  Binding =
                    match declaration with
                    | LetDeclaration definition -> Some(lowerKCoreBinding provenance definition)
                    | _ -> None
                  Provenance = provenance })

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
          Declarations = declarations }

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

    let rec private lowerKRuntimeExpression expression =
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
                      Body = lowerKRuntimeExpression caseClause.Body })
            )
        | KCoreApply(callee, arguments) ->
            KRuntimeApply(lowerKRuntimeExpression callee, arguments |> List.map lowerKRuntimeExpression)
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

    let rec private runtimePatternText pattern =
        match pattern with
        | KRuntimeWildcardPattern -> "_"
        | KRuntimeNamePattern name -> name
        | KRuntimeLiteralPattern(LiteralValue.Integer value) -> string value
        | KRuntimeLiteralPattern(LiteralValue.Float value) -> string value
        | KRuntimeLiteralPattern(LiteralValue.String value) -> $"\"{value}\""
        | KRuntimeLiteralPattern(LiteralValue.Character value) -> $"'{value}'"
        | KRuntimeLiteralPattern LiteralValue.Unit -> "()"
        | KRuntimeConstructorPattern(name, arguments) ->
            let nameText = String.concat "." name

            match arguments with
            | [] ->
                nameText
            | _ ->
                let argumentText = arguments |> List.map runtimePatternText |> String.concat " "
                $"({nameText} {argumentText})"

    let rec private runtimeExpressionText expression =
        match expression with
        | KRuntimeLiteral(LiteralValue.Integer value) -> string value
        | KRuntimeLiteral(LiteralValue.Float value) -> string value
        | KRuntimeLiteral(LiteralValue.String value) -> $"\"{value}\""
        | KRuntimeLiteral(LiteralValue.Character value) -> $"'{value}'"
        | KRuntimeLiteral LiteralValue.Unit -> "()"
        | KRuntimeName segments -> String.concat "." segments
        | KRuntimeClosure(parameters, body) ->
            let names = String.concat " " parameters
            $"(closure ({names}) {runtimeExpressionText body})"
        | KRuntimeIfThenElse(condition, whenTrue, whenFalse) ->
            $"(if {runtimeExpressionText condition} {runtimeExpressionText whenTrue} {runtimeExpressionText whenFalse})"
        | KRuntimeMatch(scrutinee, cases) ->
            let caseText =
                cases
                |> List.map (fun caseClause -> $"(case {runtimePatternText caseClause.Pattern} {runtimeExpressionText caseClause.Body})")
                |> String.concat " "

            $"(match {runtimeExpressionText scrutinee} {caseText})"
        | KRuntimeApply(callee, arguments) ->
            let argumentText =
                arguments
                |> List.map runtimeExpressionText
                |> String.concat " "

            if String.IsNullOrWhiteSpace(argumentText) then
                $"(apply {runtimeExpressionText callee})"
            else
                $"(apply {runtimeExpressionText callee} {argumentText})"
        | KRuntimeUnary(operatorName, operand) ->
            $"({operatorName} {runtimeExpressionText operand})"
        | KRuntimeBinary(left, operatorName, right) ->
            $"({operatorName} {runtimeExpressionText left} {runtimeExpressionText right})"
        | KRuntimePrefixedString(prefix, parts) ->
            let partText =
                parts
                |> List.map (function
                    | KRuntimeStringText text -> $"text:{text}"
                    | KRuntimeStringInterpolation inner -> $"interp:{runtimeExpressionText inner}")
                |> String.concat " | "

            $"({prefix}-string {partText})"

    let private backendRepresentationText representation =
        match representation with
        | BackendRepInt64 -> "int64"
        | BackendRepFloat64 -> "float64"
        | BackendRepBoolean -> "bool"
        | BackendRepString -> "string"
        | BackendRepChar -> "char"
        | BackendRepUnit -> "unit"
        | BackendRepTaggedData(moduleName, typeName) -> $"tagged-data:{moduleName}.{typeName}"
        | BackendRepClosure environmentLayout -> $"closure:{environmentLayout}"
        | BackendRepIOAction -> "io-action"
        | BackendRepOpaque(Some label) -> $"opaque:{label}"
        | BackendRepOpaque None -> "opaque"

    let private backendCallingConventionText (convention: KBackendCallingConvention) =
        let parameterText =
            convention.ParameterRepresentations
            |> List.map backendRepresentationText
            |> String.concat ", "

        let resultText =
            convention.ResultRepresentation
            |> Option.map backendRepresentationText
            |> Option.defaultValue "void"

        let dictionaryText =
            if List.isEmpty convention.RetainedDictionaryParameters then
                ""
            else
                let names = convention.RetainedDictionaryParameters |> String.concat ", "
                $" dict=[{names}]"

        $"arity={convention.RuntimeArity} params=[{parameterText}] result={resultText}{dictionaryText}"

    let private backendResolvedNameText resolvedName =
        match resolvedName with
        | BackendLocalName(name, representation) ->
            match representation with
            | Some rep -> $"{name}:{backendRepresentationText rep}"
            | None -> name
        | BackendGlobalBindingName(moduleName, bindingName, representation) ->
            match representation with
            | Some rep -> $"{moduleName}.{bindingName}:{backendRepresentationText rep}"
            | None -> $"{moduleName}.{bindingName}"
        | BackendIntrinsicName(moduleName, bindingName, representation) ->
            match representation with
            | Some rep -> $"intrinsic {moduleName}.{bindingName}:{backendRepresentationText rep}"
            | None -> $"intrinsic {moduleName}.{bindingName}"
        | BackendConstructorName(moduleName, typeName, constructorName, tag, arity, representation) ->
            $"ctor {moduleName}.{typeName}.{constructorName}@{tag}/{arity}:{backendRepresentationText representation}"

    let rec private backendPatternText pattern =
        match pattern with
        | BackendWildcardPattern -> "_"
        | BackendBindPattern binding -> $"{binding.Name}:{backendRepresentationText binding.Representation}"
        | BackendLiteralPattern(LiteralValue.Integer value, representation) ->
            $"{value}:{backendRepresentationText representation}"
        | BackendLiteralPattern(LiteralValue.Float value, representation) ->
            $"{value}:{backendRepresentationText representation}"
        | BackendLiteralPattern(LiteralValue.String value, representation) ->
            $"\"{value}\":{backendRepresentationText representation}"
        | BackendLiteralPattern(LiteralValue.Character value, representation) ->
            $"'{value}':{backendRepresentationText representation}"
        | BackendLiteralPattern(LiteralValue.Unit, representation) ->
            $"():{backendRepresentationText representation}"
        | BackendConstructorPattern(moduleName, typeName, constructorName, tag, fieldPatterns) ->
            let fields =
                fieldPatterns
                |> List.map backendPatternText
                |> String.concat " "

            if String.IsNullOrWhiteSpace(fields) then
                $"({moduleName}.{typeName}.{constructorName}@{tag})"
            else
                $"({moduleName}.{typeName}.{constructorName}@{tag} {fields})"

    let rec private backendExpressionText expression =
        match expression with
        | BackendLiteral(LiteralValue.Integer value, representation) ->
            $"{value}:{backendRepresentationText representation}"
        | BackendLiteral(LiteralValue.Float value, representation) ->
            $"{value}:{backendRepresentationText representation}"
        | BackendLiteral(LiteralValue.String value, representation) ->
            $"\"{value}\":{backendRepresentationText representation}"
        | BackendLiteral(LiteralValue.Character value, representation) ->
            $"'{value}':{backendRepresentationText representation}"
        | BackendLiteral(LiteralValue.Unit, representation) ->
            $"():{backendRepresentationText representation}"
        | BackendName resolvedName ->
            backendResolvedNameText resolvedName
        | BackendClosure(parameters, captures, environmentLayout, body, convention, representation) ->
            let parameterText =
                parameters
                |> List.map (fun parameter -> $"{parameter.Name}:{backendRepresentationText parameter.Representation}")
                |> String.concat " "

            let captureText =
                captures
                |> List.map (fun capture -> $"{capture.Name}:{backendRepresentationText capture.Representation}")
                |> String.concat " "

            $"(closure env={environmentLayout} rep={backendRepresentationText representation} conv=[{backendCallingConventionText convention}] params=({parameterText}) captures=({captureText}) {backendExpressionText body})"
        | BackendIfThenElse(condition, whenTrue, whenFalse, resultRepresentation) ->
            $"(if rep={backendRepresentationText resultRepresentation} {backendExpressionText condition} {backendExpressionText whenTrue} {backendExpressionText whenFalse})"
        | BackendMatch(scrutinee, cases, resultRepresentation) ->
            let caseText =
                cases
                |> List.map (fun caseClause -> $"(case {backendPatternText caseClause.Pattern} {backendExpressionText caseClause.Body})")
                |> String.concat " "

            $"(match rep={backendRepresentationText resultRepresentation} {backendExpressionText scrutinee} {caseText})"
        | BackendCall(callee, arguments, convention, resultRepresentation) ->
            let argumentText =
                arguments
                |> List.map backendExpressionText
                |> String.concat " "

            $"(call rep={backendRepresentationText resultRepresentation} conv=[{backendCallingConventionText convention}] {backendExpressionText callee} {argumentText})"
        | BackendConstructData(moduleName, typeName, constructorName, tag, fields, representation) ->
            let fieldText =
                fields
                |> List.map backendExpressionText
                |> String.concat " "

            $"(construct {moduleName}.{typeName}.{constructorName}@{tag} rep={backendRepresentationText representation} {fieldText})"
        | BackendPrefixedString(prefix, parts, resultRepresentation) ->
            let partText =
                parts
                |> List.map (function
                    | BackendStringText text -> $"text:{text}"
                    | BackendStringInterpolation inner -> $"interp:{backendExpressionText inner}")
                |> String.concat " | "

            $"({prefix}-string rep={backendRepresentationText resultRepresentation} {partText})"

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

    let private lowerKRuntimeModule (coreModule: KCoreModule) =
        let termBindings =
            coreModule.Declarations
            |> List.choose (fun declaration ->
                match declaration.Binding with
                | Some binding when binding.Name.IsSome ->
                    Some
                        { Name = binding.Name.Value
                          Parameters = binding.Parameters |> List.map (fun parameter -> parameter.Name)
                          Body = binding.Body |> Option.map lowerKRuntimeExpression
                          Intrinsic = false
                          Provenance = binding.Provenance }
                | _ ->
                    None)

        let intrinsicBindings =
            coreModule.IntrinsicTerms
            |> List.map (fun name ->
                { Name = name
                  Parameters = []
                  Body = None
                  Intrinsic = true
                  Provenance =
                    { FilePath = coreModule.SourceFile
                      ModuleName = coreModule.Name
                      DeclarationName = Some name
                      IntroductionKind = "intrinsic" } })

        let constructors =
            coreModule.Declarations
            |> List.collect (fun declaration ->
                match declaration.Source with
                | DataDeclarationNode dataDeclaration when isExportedDataDeclaration coreModule dataDeclaration ->
                    dataDeclaration.Constructors
                    |> List.map (fun constructor ->
                        { Name = constructor.Name
                          Arity = constructor.Arity
                          TypeName = dataDeclaration.Name
                          Provenance =
                            { declaration.Provenance with
                                DeclarationName = Some constructor.Name
                                IntroductionKind = "constructor" } })
                | _ ->
                    [])

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

        { Name = coreModule.Name
          SourceFile = coreModule.SourceFile
          Imports = coreModule.Imports
          Exports = exports
          IntrinsicTerms = coreModule.IntrinsicTerms
          Constructors = constructors
          Bindings = termBindings @ intrinsicBindings }

    type private BackendLoweringBindingInfo =
        { ModuleName: string
          Name: string
          Intrinsic: bool
          Arity: int
          ReturnRepresentation: KBackendRepresentationClass option }

    type private BackendLoweringConstructorInfo =
        { ModuleName: string
          TypeName: string
          Name: string
          Tag: int
          Arity: int
          FieldRepresentations: KBackendRepresentationClass list
          Representation: KBackendRepresentationClass
          Provenance: KCoreOrigin }

    type private BackendLoweringContext =
        { RuntimeModules: Map<string, KRuntimeModule>
          CoreModules: Map<string, KCoreModule>
          BindingInfos: Map<string * string, BackendLoweringBindingInfo>
          ConstructorInfos: Map<string * string, BackendLoweringConstructorInfo> }

    let private backendLiteralRepresentation literal =
        match literal with
        | LiteralValue.Integer _ -> BackendRepInt64
        | LiteralValue.Float _ -> BackendRepFloat64
        | LiteralValue.String _ -> BackendRepString
        | LiteralValue.Character _ -> BackendRepChar
        | LiteralValue.Unit -> BackendRepUnit

    let private backendOpaqueRepresentation name =
        BackendRepOpaque name

    let private intrinsicRuntimeArity name =
        match name with
        | "True"
        | "False" ->
            0
        | "not"
        | "negate"
        | "pure"
        | "print"
        | "println"
        | "printInt" ->
            1
        | "and"
        | "or"
        | ">>="
        | ">>" ->
            2
        | _ ->
            0

    let private intrinsicResultRepresentation name =
        match name with
        | "True"
        | "False"
        | "not"
        | "and"
        | "or" ->
            Some BackendRepBoolean
        | "print"
        | "println"
        | "printInt"
        | "pure"
        | ">>="
        | ">>" ->
            Some BackendRepIOAction
        | _ ->
            None

    let private tryBackendRepresentationFromTypeText (typeText: string option) =
        let tryTypeHead (text: string) =
            text.Replace("(", " ")
                .Replace(")", " ")
                .Split([| ' '; '\t' |], StringSplitOptions.RemoveEmptyEntries)
            |> Array.tryHead

        match typeText |> Option.bind tryTypeHead with
        | Some "Int" -> Some BackendRepInt64
        | Some "Float" -> Some BackendRepFloat64
        | Some "Bool" -> Some BackendRepBoolean
        | Some "String" -> Some BackendRepString
        | Some "Char" -> Some BackendRepChar
        | Some "Unit" -> Some BackendRepUnit
        | Some "IO" -> Some BackendRepIOAction
        | Some head -> Some(backendOpaqueRepresentation (Some head))
        | None -> None

    let private mergeBackendRepresentations left right =
        if left = right then
            left
        else
            backendOpaqueRepresentation None

    let rec private inferKCoreExpressionRepresentation expression =
        match expression with
        | KCoreLiteral literal ->
            backendLiteralRepresentation literal
        | KCoreName [ "True" ]
        | KCoreName [ "False" ] ->
            BackendRepBoolean
        | KCoreName _ ->
            backendOpaqueRepresentation None
        | KCoreLambda _ ->
            backendOpaqueRepresentation (Some "Function")
        | KCoreIfThenElse(_, whenTrue, whenFalse) ->
            mergeBackendRepresentations
                (inferKCoreExpressionRepresentation whenTrue)
                (inferKCoreExpressionRepresentation whenFalse)
        | KCoreMatch(_, cases) ->
            match cases with
            | [] -> backendOpaqueRepresentation None
            | firstCase :: rest ->
                rest
                |> List.fold
                    (fun state caseClause ->
                        mergeBackendRepresentations state (inferKCoreExpressionRepresentation caseClause.Body))
                    (inferKCoreExpressionRepresentation firstCase.Body)
        | KCoreApply(KCoreName [ "pure" ], _)
        | KCoreApply(KCoreName [ ">>=" ], _)
        | KCoreApply(KCoreName [ ">>" ], _)
        | KCoreApply(KCoreName [ "print" ], _)
        | KCoreApply(KCoreName [ "println" ], _)
        | KCoreApply(KCoreName [ "printInt" ], _) ->
            BackendRepIOAction
        | KCoreApply _ ->
            backendOpaqueRepresentation None
        | KCoreUnary("not", _) ->
            BackendRepBoolean
        | KCoreUnary("negate", operand) ->
            inferKCoreExpressionRepresentation operand
        | KCoreUnary _ ->
            backendOpaqueRepresentation None
        | KCoreBinary(_, ("&&" | "||" | "==" | "!=" | "<" | ">" | "<=" | ">="), _) ->
            BackendRepBoolean
        | KCoreBinary(left, ("+" | "-" | "*" | "/"), right) ->
            match inferKCoreExpressionRepresentation left, inferKCoreExpressionRepresentation right with
            | BackendRepFloat64, _
            | _, BackendRepFloat64 ->
                BackendRepFloat64
            | BackendRepInt64, BackendRepInt64 ->
                BackendRepInt64
            | _ ->
                backendOpaqueRepresentation None
        | KCoreBinary _ ->
            backendOpaqueRepresentation None
        | KCorePrefixedString _ ->
            BackendRepString

    let private selectionImportsRuntimeTermName selection name =
        match selection with
        | QualifiedOnly ->
            false
        | Items items ->
            items
            |> List.exists (fun item ->
                String.Equals(item.Name, name, StringComparison.Ordinal)
                && (item.Namespace.IsNone || item.Namespace = Some ImportNamespace.Term))
        | All ->
            true
        | AllExcept excludedNames ->
            not (List.contains name excludedNames)

    let private selectionImportsRuntimeConstructorName selection name =
        match selection with
        | QualifiedOnly ->
            false
        | Items items ->
            items
            |> List.exists (fun item ->
                String.Equals(item.Name, name, StringComparison.Ordinal)
                && item.Namespace = Some ImportNamespace.Constructor)
        | All
        | AllExcept _ ->
            false

    let private buildBackendLoweringContext (kCore: KCoreModule list) (kRuntimeIR: KRuntimeModule list) =
        let runtimeModules =
            kRuntimeIR
            |> List.map (fun moduleDump -> moduleDump.Name, moduleDump)
            |> Map.ofList

        let coreModules =
            kCore
            |> List.map (fun moduleDump -> moduleDump.Name, moduleDump)
            |> Map.ofList

        let bindingInfos =
            kRuntimeIR
            |> List.collect (fun runtimeModule ->
                let coreModule = coreModules[runtimeModule.Name]

                runtimeModule.Bindings
                |> List.map (fun binding ->
                    let coreBinding =
                        coreModule.Declarations
                        |> List.tryPick (fun declaration ->
                            match declaration.Binding with
                            | Some coreBinding when coreBinding.Name = Some binding.Name ->
                                Some coreBinding
                            | _ ->
                                None)

                    let returnRepresentation =
                        if binding.Intrinsic then
                            intrinsicResultRepresentation binding.Name
                        else
                            coreBinding
                            |> Option.bind (fun coreBinding ->
                                tryBackendRepresentationFromTypeText coreBinding.ReturnTypeText
                                |> Option.orElseWith (fun () ->
                                    coreBinding.Body |> Option.map inferKCoreExpressionRepresentation))

                    let arity =
                        if binding.Intrinsic then
                            intrinsicRuntimeArity binding.Name
                        else
                            List.length binding.Parameters

                    (runtimeModule.Name, binding.Name),
                    { ModuleName = runtimeModule.Name
                      Name = binding.Name
                      Intrinsic = binding.Intrinsic
                      Arity = arity
                      ReturnRepresentation = returnRepresentation }))
            |> Map.ofList

        let constructorInfos =
            kRuntimeIR
            |> List.collect (fun runtimeModule ->
                runtimeModule.Constructors
                |> List.groupBy (fun constructor -> constructor.TypeName)
                |> List.collect (fun (_, constructors) ->
                    constructors
                    |> List.mapi (fun tag constructor ->
                        (runtimeModule.Name, constructor.Name),
                        { ModuleName = runtimeModule.Name
                          TypeName = constructor.TypeName
                          Name = constructor.Name
                          Tag = tag
                          Arity = constructor.Arity
                          FieldRepresentations = List.replicate constructor.Arity (backendOpaqueRepresentation None)
                          Representation = BackendRepTaggedData(runtimeModule.Name, constructor.TypeName)
                          Provenance = constructor.Provenance })))
            |> Map.ofList

        { RuntimeModules = runtimeModules
          CoreModules = coreModules
          BindingInfos = bindingInfos
          ConstructorInfos = constructorInfos }

    let private lowerKBackendModules (kCore: KCoreModule list) (kRuntimeIR: KRuntimeModule list) =
        let context = buildBackendLoweringContext kCore kRuntimeIR

        let resolveQualifiedRuntimeModule (currentModule: KRuntimeModule) qualifierSegments =
            let qualifierText = SyntaxFacts.moduleNameToText qualifierSegments

            if String.Equals(qualifierText, currentModule.Name, StringComparison.Ordinal) then
                Some currentModule
            else
                context.RuntimeModules
                |> Map.tryFind qualifierText
                |> Option.orElseWith (fun () ->
                    currentModule.Imports
                    |> List.tryPick (fun (spec: ImportSpec) ->
                        match spec.Source, spec.Alias, spec.Selection with
                        | Dotted moduleSegments, Some alias, QualifiedOnly when qualifierSegments = [ alias ] ->
                            context.RuntimeModules |> Map.tryFind (SyntaxFacts.moduleNameToText moduleSegments)
                        | Dotted moduleSegments, None, QualifiedOnly when qualifierSegments = moduleSegments ->
                            context.RuntimeModules |> Map.tryFind (SyntaxFacts.moduleNameToText moduleSegments)
                        | _ ->
                            None))

        let resolveRuntimeName currentModule (locals: Map<string, KBackendRepresentationClass>) segments =
            let bindingNames (moduleDump: KRuntimeModule) =
                moduleDump.Bindings |> List.map (fun binding -> binding.Name) |> Set.ofList

            let constructorNames (moduleDump: KRuntimeModule) =
                moduleDump.Constructors |> List.map (fun constructor -> constructor.Name) |> Set.ofList

            let resolvedNameRepresentation resolvedName =
                match resolvedName with
                | BackendLocalName(_, Some representation) ->
                    representation
                | BackendLocalName _ ->
                    backendOpaqueRepresentation None
                | BackendGlobalBindingName(moduleName, bindingName, _) ->
                    let bindingInfo = context.BindingInfos[moduleName, bindingName]

                    if bindingInfo.Arity = 0 then
                        bindingInfo.ReturnRepresentation |> Option.defaultValue (backendOpaqueRepresentation None)
                    else
                        backendOpaqueRepresentation (Some "Function")
                | BackendIntrinsicName(moduleName, bindingName, _) ->
                    let bindingInfo = context.BindingInfos[moduleName, bindingName]

                    if bindingInfo.Arity = 0 then
                        bindingInfo.ReturnRepresentation |> Option.defaultValue (backendOpaqueRepresentation (Some "Intrinsic"))
                    else
                        backendOpaqueRepresentation (Some "Intrinsic")
                | BackendConstructorName(_, _, _, _, arity, representation) ->
                    if arity = 0 then representation else backendOpaqueRepresentation (Some "Constructor")

            let tryResolveModuleMember (targetModule: KRuntimeModule) memberName =
                if bindingNames targetModule |> Set.contains memberName then
                    let bindingInfo = context.BindingInfos[targetModule.Name, memberName]

                    if bindingInfo.Intrinsic then
                        Some(BackendIntrinsicName(targetModule.Name, memberName, bindingInfo.ReturnRepresentation))
                    else
                        Some(BackendGlobalBindingName(targetModule.Name, memberName, bindingInfo.ReturnRepresentation))
                elif constructorNames targetModule |> Set.contains memberName then
                    let constructorInfo = context.ConstructorInfos[targetModule.Name, memberName]

                    Some(
                        BackendConstructorName(
                            targetModule.Name,
                            constructorInfo.TypeName,
                            constructorInfo.Name,
                            constructorInfo.Tag,
                            constructorInfo.Arity,
                            constructorInfo.Representation
                        )
                    )
                else
                    None

            match segments with
            | [] ->
                Result.Error "empty runtime name"
            | [ name ] when locals.ContainsKey name ->
                let resolved = BackendLocalName(name, Map.tryFind name locals)
                Result.Ok(resolved, resolvedNameRepresentation resolved)
            | [ name ] ->
                let currentBinding =
                    tryResolveModuleMember currentModule name

                match currentBinding with
                | Some resolved ->
                    Result.Ok(resolved, resolvedNameRepresentation resolved)
                | None ->
                    let importedMatches =
                        currentModule.Imports
                        |> List.choose (fun (spec: ImportSpec) ->
                            match spec.Source with
                            | Url _ ->
                                None
                            | Dotted moduleSegments ->
                                let importedModuleName = SyntaxFacts.moduleNameToText moduleSegments

                                match context.RuntimeModules |> Map.tryFind importedModuleName with
                                | Some importedModule when selectionImportsRuntimeTermName spec.Selection name ->
                                    tryResolveModuleMember importedModule name |> Option.map (fun resolved -> importedModuleName, resolved)
                                | Some importedModule when selectionImportsRuntimeConstructorName spec.Selection name ->
                                    tryResolveModuleMember importedModule name |> Option.map (fun resolved -> importedModuleName, resolved)
                                | _ ->
                                    None)
                        |> List.distinctBy fst

                    match importedMatches with
                    | [ _, resolved ] ->
                        Result.Ok(resolved, resolvedNameRepresentation resolved)
                    | _ :: _ :: _ ->
                        Result.Error $"ambiguous runtime name '{name}'"
                    | [] ->
                        Result.Error $"unresolved runtime name '{name}'"
            | _ ->
                let qualifierSegments = segments |> List.take (segments.Length - 1)
                let memberName = List.last segments

                match resolveQualifiedRuntimeModule currentModule qualifierSegments with
                | Some targetModule ->
                    match tryResolveModuleMember targetModule memberName with
                    | Some resolved ->
                        Result.Ok(resolved, resolvedNameRepresentation resolved)
                    | None ->
                        let text = String.concat "." segments
                        Result.Error $"unresolved runtime name '{text}'"
                | None ->
                    Result.Error $"unresolved module qualifier '{SyntaxFacts.moduleNameToText qualifierSegments}'"

        let rec collectClosureCaptures (locals: Set<string>) (bound: Set<string>) expression =
            match expression with
            | KRuntimeLiteral _ ->
                Set.empty
            | KRuntimeName [ name ] when Set.contains name locals && not (Set.contains name bound) ->
                Set.singleton name
            | KRuntimeName _ ->
                Set.empty
            | KRuntimeClosure(parameters, body) ->
                collectClosureCaptures locals (Set.union bound (parameters |> Set.ofList)) body
            | KRuntimeIfThenElse(condition, whenTrue, whenFalse) ->
                collectClosureCaptures locals bound condition
                |> Set.union (collectClosureCaptures locals bound whenTrue)
                |> Set.union (collectClosureCaptures locals bound whenFalse)
            | KRuntimeMatch(scrutinee, cases) ->
                let scrutineeCaptures = collectClosureCaptures locals bound scrutinee

                cases
                |> List.fold
                    (fun state caseClause ->
                        let rec collectPatternBindings pattern =
                            match pattern with
                            | KRuntimeWildcardPattern
                            | KRuntimeLiteralPattern _ ->
                                Set.empty
                            | KRuntimeNamePattern name ->
                                Set.singleton name
                            | KRuntimeConstructorPattern(_, arguments) ->
                                arguments
                                |> List.fold
                                    (fun patternState argumentPattern ->
                                        Set.union patternState (collectPatternBindings argumentPattern))
                                    Set.empty

                        let caseBound = Set.union bound (collectPatternBindings caseClause.Pattern)
                        Set.union state (collectClosureCaptures locals caseBound caseClause.Body))
                    scrutineeCaptures
            | KRuntimeApply(callee, arguments) ->
                arguments
                |> List.fold
                    (fun state argument -> Set.union state (collectClosureCaptures locals bound argument))
                    (collectClosureCaptures locals bound callee)
            | KRuntimeUnary(_, operand) ->
                collectClosureCaptures locals bound operand
            | KRuntimeBinary(left, _, right) ->
                collectClosureCaptures locals bound left
                |> Set.union (collectClosureCaptures locals bound right)
            | KRuntimePrefixedString(_, parts) ->
                parts
                |> List.fold
                    (fun state part ->
                        match part with
                        | KRuntimeStringText _ ->
                            state
                        | KRuntimeStringInterpolation inner ->
                            Set.union state (collectClosureCaptures locals bound inner))
                    Set.empty

        let lowerModule (runtimeModule: KRuntimeModule) : Result<KBackendModule, string> =
            let environmentLayouts = ResizeArray<KBackendEnvironmentLayout>()
            let mutable nextEnvironmentLayoutId = 0

            let rec lowerPattern
                (locals: Map<string, KBackendRepresentationClass>)
                (patternRepresentation: KBackendRepresentationClass)
                (pattern: KRuntimePattern)
                : Result<KBackendPattern * Map<string, KBackendRepresentationClass>, string> =
                match pattern with
                | KRuntimeWildcardPattern ->
                    Result.Ok(BackendWildcardPattern, Map.empty)
                | KRuntimeLiteralPattern literal ->
                    Result.Ok(BackendLiteralPattern(literal, backendLiteralRepresentation literal), Map.empty)
                | KRuntimeNamePattern name ->
                    let binding: KBackendPatternBinding =
                        { Name = name
                          Representation = patternRepresentation }

                    Result.Ok(BackendBindPattern binding, Map.ofList [ name, patternRepresentation ])
                | KRuntimeConstructorPattern(nameSegments, argumentPatterns) ->
                    let constructorText = String.concat "." nameSegments

                    resolveRuntimeName runtimeModule locals nameSegments
                    |> Result.bind (fun (resolvedName, _) ->
                        match resolvedName with
                        | BackendConstructorName(moduleName, typeName, constructorName, tag, _, _) ->
                            let constructorInfo = context.ConstructorInfos[moduleName, constructorName]

                            ((Result.Ok([], Map.empty)), List.zip argumentPatterns constructorInfo.FieldRepresentations)
                            ||> List.fold (fun state (argumentPattern, fieldRepresentation) ->
                                state
                                |> Result.bind (fun (patterns, discoveredLocals) ->
                                    lowerPattern locals fieldRepresentation argumentPattern
                                    |> Result.map (fun (loweredPattern, patternLocals) ->
                                        loweredPattern :: patterns,
                                        Map.fold (fun mapState key value -> Map.add key value mapState) discoveredLocals patternLocals)))
                            |> Result.map (fun (patterns, discoveredLocals) ->
                                BackendConstructorPattern(moduleName, typeName, constructorName, tag, List.rev patterns),
                                discoveredLocals)
                        | _ ->
                            Result.Error $"Pattern '{constructorText}' does not resolve to a constructor.")

            let rec lowerExpression
                (scopeLabel: string)
                (locals: Map<string, KBackendRepresentationClass>)
                (expression: KRuntimeExpression)
                : Result<KBackendExpression * KBackendRepresentationClass, string> =
                match expression with
                | KRuntimeLiteral literal ->
                    let representation = backendLiteralRepresentation literal
                    Result.Ok(BackendLiteral(literal, representation), representation)
                | KRuntimeName segments ->
                    resolveRuntimeName runtimeModule locals segments
                    |> Result.map (fun (resolvedName, representation) ->
                        BackendName resolvedName, representation)
                | KRuntimeClosure(parameters, body) ->
                    let parameterRepresentations: KBackendParameter list =
                        parameters
                        |> List.map (fun name ->
                            { Name = name
                              Representation = backendOpaqueRepresentation None })

                    let parameterLocals =
                        parameterRepresentations
                        |> List.map (fun parameter -> parameter.Name, parameter.Representation)
                        |> Map.ofList

                    let captureNames =
                        collectClosureCaptures (locals |> Map.toSeq |> Seq.map fst |> Set.ofSeq) (parameters |> Set.ofList) body
                        |> Set.toList
                        |> List.sort

                    let captures: KBackendCapture list =
                        captureNames
                        |> List.choose (fun name ->
                            locals
                            |> Map.tryFind name
                            |> Option.map (fun representation ->
                                    { Name = name
                                      Representation = representation }))

                    let environmentLayoutName =
                        nextEnvironmentLayoutId <- nextEnvironmentLayoutId + 1
                        $"{scopeLabel}$env{nextEnvironmentLayoutId}"

                    environmentLayouts.Add
                        { Name = environmentLayoutName
                          Slots = captures }

                    let closureLocals =
                        captures
                        |> List.map (fun capture -> capture.Name, capture.Representation)
                        |> List.append (parameterRepresentations |> List.map (fun parameter -> parameter.Name, parameter.Representation))
                        |> Map.ofList

                    lowerExpression $"{scopeLabel}$closure{nextEnvironmentLayoutId}" closureLocals body
                    |> Result.map (fun (loweredBody, resultRepresentation) ->
                        let convention =
                            { RuntimeArity = List.length parameters
                              ParameterRepresentations = parameterRepresentations |> List.map (fun parameter -> parameter.Representation)
                              ResultRepresentation = Some resultRepresentation
                              RetainedDictionaryParameters = [] }

                        let representation = BackendRepClosure environmentLayoutName

                        BackendClosure(
                            parameterRepresentations,
                            captures,
                            environmentLayoutName,
                            loweredBody,
                            convention,
                            representation
                        ),
                        representation)
                | KRuntimeIfThenElse(condition, whenTrue, whenFalse) ->
                    lowerExpression scopeLabel locals condition
                    |> Result.bind (fun (loweredCondition, _) ->
                        lowerExpression scopeLabel locals whenTrue
                        |> Result.bind (fun (loweredTrue, trueRepresentation) ->
                            lowerExpression scopeLabel locals whenFalse
                            |> Result.map (fun (loweredFalse, falseRepresentation) ->
                                let resultRepresentation =
                                    mergeBackendRepresentations trueRepresentation falseRepresentation

                                BackendIfThenElse(
                                    loweredCondition,
                                    loweredTrue,
                                    loweredFalse,
                                    resultRepresentation
                                ),
                                resultRepresentation)))
                | KRuntimeMatch(scrutinee, cases) ->
                    lowerExpression scopeLabel locals scrutinee
                    |> Result.bind (fun (loweredScrutinee, scrutineeRepresentation) ->
                        ((Result.Ok([], [])), cases)
                        ||> List.fold (fun state caseClause ->
                            state
                            |> Result.bind (fun (loweredCases, caseRepresentations) ->
                                lowerPattern locals scrutineeRepresentation caseClause.Pattern
                                |> Result.bind (fun (loweredPattern, discoveredLocals) ->
                                    let caseLocals =
                                        Map.fold (fun mapState key value -> Map.add key value mapState) locals discoveredLocals

                                    lowerExpression scopeLabel caseLocals caseClause.Body
                                     |> Result.map (fun (loweredBody, bodyRepresentation) ->
                                         { Pattern = loweredPattern
                                           Body = loweredBody }
                                         :: loweredCases,
                                         bodyRepresentation :: caseRepresentations))))
                        |> Result.map (fun (loweredCases, caseRepresentations) ->
                            let resultRepresentation =
                                match List.rev caseRepresentations with
                                | [] ->
                                    backendOpaqueRepresentation None
                                | first :: rest ->
                                    rest |> List.fold mergeBackendRepresentations first

                            BackendMatch(loweredScrutinee, List.rev loweredCases, resultRepresentation),
                            resultRepresentation))
                | KRuntimeApply(callee, arguments) ->
                    let lowerArguments =
                        arguments
                        |> List.fold
                            (fun state argument ->
                                state
                                |> Result.bind (fun (loweredArguments, argumentRepresentations) ->
                                    lowerExpression scopeLabel locals argument
                                    |> Result.map (fun (loweredArgument, argumentRepresentation) ->
                                        loweredArgument :: loweredArguments,
                                        argumentRepresentation :: argumentRepresentations)))
                            (Result.Ok([], []))

                    lowerArguments
                    |> Result.bind (fun (loweredArguments, argumentRepresentations) ->
                        let loweredArguments = List.rev loweredArguments
                        let argumentRepresentations = List.rev argumentRepresentations

                        match callee with
                        | KRuntimeName segments ->
                            resolveRuntimeName runtimeModule locals segments
                            |> Result.bind (fun (resolvedName, _) ->
                                match resolvedName with
                                | BackendConstructorName(moduleName, typeName, constructorName, tag, arity, representation)
                                    when arity = List.length loweredArguments ->
                                    Result.Ok(
                                        BackendConstructData(
                                            moduleName,
                                            typeName,
                                            constructorName,
                                            tag,
                                            loweredArguments,
                                            representation
                                        ),
                                        representation
                                    )
                                | _ ->
                                    let calleeExpression = BackendName resolvedName

                                    let convention =
                                        match resolvedName with
                                        | BackendGlobalBindingName(moduleName, bindingName, _)
                                        | BackendIntrinsicName(moduleName, bindingName, _) ->
                                            let bindingInfo = context.BindingInfos[moduleName, bindingName]

                                            { RuntimeArity = bindingInfo.Arity
                                              ParameterRepresentations = argumentRepresentations
                                              ResultRepresentation = bindingInfo.ReturnRepresentation
                                              RetainedDictionaryParameters = [] }
                                        | BackendConstructorName(_, _, _, _, arity, representation) ->
                                            { RuntimeArity = arity
                                              ParameterRepresentations = argumentRepresentations
                                              ResultRepresentation = Some representation
                                              RetainedDictionaryParameters = [] }
                                        | BackendLocalName _ ->
                                            { RuntimeArity = List.length loweredArguments
                                              ParameterRepresentations = argumentRepresentations
                                              ResultRepresentation = Some(backendOpaqueRepresentation None)
                                              RetainedDictionaryParameters = [] }

                                    let resultRepresentation =
                                        convention.ResultRepresentation |> Option.defaultValue (backendOpaqueRepresentation None)

                                    Result.Ok(BackendCall(calleeExpression, loweredArguments, convention, resultRepresentation), resultRepresentation))
                        | _ ->
                            lowerExpression scopeLabel locals callee
                            |> Result.map (fun (loweredCallee, _) ->
                                let convention =
                                    { RuntimeArity = List.length loweredArguments
                                      ParameterRepresentations = argumentRepresentations
                                      ResultRepresentation = Some(backendOpaqueRepresentation None)
                                      RetainedDictionaryParameters = [] }

                                let resultRepresentation = backendOpaqueRepresentation None
                                BackendCall(loweredCallee, loweredArguments, convention, resultRepresentation), resultRepresentation))
                | KRuntimeUnary(operatorName, operand) ->
                    lowerExpression scopeLabel locals operand
                    |> Result.map (fun (loweredOperand, operandRepresentation) ->
                        let resultRepresentation =
                            match operatorName with
                            | "not" -> BackendRepBoolean
                            | "negate" -> operandRepresentation
                            | _ -> backendOpaqueRepresentation None

                        let callee = BackendName(BackendIntrinsicName(runtimeModule.Name, operatorName, Some resultRepresentation))

                        let convention =
                            { RuntimeArity = 1
                              ParameterRepresentations = [ operandRepresentation ]
                              ResultRepresentation = Some resultRepresentation
                              RetainedDictionaryParameters = [] }

                        BackendCall(callee, [ loweredOperand ], convention, resultRepresentation), resultRepresentation)
                | KRuntimeBinary(left, operatorName, right) ->
                    lowerExpression scopeLabel locals left
                    |> Result.bind (fun (loweredLeft, leftRepresentation) ->
                        lowerExpression scopeLabel locals right
                        |> Result.map (fun (loweredRight, rightRepresentation) ->
                            let resultRepresentation =
                                match operatorName with
                                | "&&"
                                | "||"
                                | "=="
                                | "!="
                                | "<"
                                | ">"
                                | "<="
                                | ">=" ->
                                    BackendRepBoolean
                                | "+" | "-" | "*" | "/" ->
                                    mergeBackendRepresentations leftRepresentation rightRepresentation
                                | _ ->
                                    backendOpaqueRepresentation None

                            let callee = BackendName(BackendIntrinsicName(runtimeModule.Name, operatorName, Some resultRepresentation))

                            let convention =
                                { RuntimeArity = 2
                                  ParameterRepresentations = [ leftRepresentation; rightRepresentation ]
                                  ResultRepresentation = Some resultRepresentation
                                  RetainedDictionaryParameters = [] }

                            BackendCall(callee, [ loweredLeft; loweredRight ], convention, resultRepresentation), resultRepresentation))
                | KRuntimePrefixedString(prefix, parts) ->
                    let rec lowerStringParts parts =
                        match parts with
                        | [] ->
                            Result.Ok []
                        | KRuntimeStringText text :: rest ->
                            lowerStringParts rest
                            |> Result.map (fun loweredRest -> BackendStringText text :: loweredRest)
                        | KRuntimeStringInterpolation inner :: rest ->
                            lowerExpression scopeLabel locals inner
                            |> Result.bind (fun (loweredInner, _) ->
                                lowerStringParts rest
                                |> Result.map (fun loweredRest -> BackendStringInterpolation loweredInner :: loweredRest))

                    lowerStringParts parts
                    |> Result.map (fun loweredParts ->
                        let representation = BackendRepString
                        BackendPrefixedString(prefix, loweredParts, representation), representation)

            let lowerBinding (binding: KRuntimeBinding) =
                let bindingInfo = context.BindingInfos[runtimeModule.Name, binding.Name]
                let exported = List.contains binding.Name runtimeModule.Exports
                let entryPoint = exported && bindingInfo.Arity = 0 && not bindingInfo.Intrinsic

                let parameterRepresentations: KBackendParameter list =
                    if binding.Intrinsic then
                        List.init bindingInfo.Arity (fun _ ->
                            { Name = "_"
                              Representation = backendOpaqueRepresentation None })
                    else
                        match context.CoreModules[runtimeModule.Name].Declarations
                              |> List.tryPick (fun declaration ->
                                  match declaration.Binding with
                                  | Some coreBinding when coreBinding.Name = Some binding.Name -> Some coreBinding
                                  | _ -> None) with
                        | Some coreBinding ->
                            coreBinding.Parameters
                            |> List.map (fun parameter ->
                                { Name = parameter.Name
                                  Representation =
                                    tryBackendRepresentationFromTypeText parameter.TypeText
                                    |> Option.defaultValue (backendOpaqueRepresentation None) })
                        | None ->
                            binding.Parameters
                            |> List.map (fun name ->
                                { Name = name
                                  Representation = backendOpaqueRepresentation None })

                let convention =
                    { RuntimeArity = bindingInfo.Arity
                      ParameterRepresentations = parameterRepresentations |> List.map (fun parameter -> parameter.Representation)
                      ResultRepresentation = bindingInfo.ReturnRepresentation
                      RetainedDictionaryParameters = [] }

                let bodyResult =
                    if binding.Intrinsic then
                        Result.Ok None
                    else
                        let locals =
                            parameterRepresentations
                            |> List.map (fun parameter -> parameter.Name, parameter.Representation)
                            |> Map.ofList

                        match binding.Body with
                        | None ->
                            Result.Ok None
                        | Some body ->
                            lowerExpression $"{runtimeModule.Name}.{binding.Name}" locals body
                            |> Result.map (fun (loweredBody, _) -> Some loweredBody)

                bodyResult
                |> Result.mapError (fun issue ->
                    $"Could not lower runtime binding '{runtimeModule.Name}.{binding.Name}' to KBackendIR: {issue}")
                |> Result.map (fun body ->
                    { Name = binding.Name
                      Parameters = parameterRepresentations
                      CallingConvention = convention
                      ReturnRepresentation = bindingInfo.ReturnRepresentation
                      EnvironmentLayout = None
                      Intrinsic = binding.Intrinsic
                      Exported = exported
                      EntryPoint = entryPoint
                      ControlForm = StructuredExpression
                      Body = body
                      Provenance = binding.Provenance })

            let backendFunctions =
                ((Result.Ok []), runtimeModule.Bindings)
                ||> List.fold (fun state binding ->
                    state
                    |> Result.bind (fun loweredBindings ->
                        lowerBinding binding
                        |> Result.map (fun loweredBinding -> loweredBinding :: loweredBindings)))
                |> Result.map List.rev

            let dataLayouts =
                context.ConstructorInfos
                |> Map.toList
                |> List.choose (fun ((moduleName, _), constructorInfo) ->
                    if String.Equals(moduleName, runtimeModule.Name, StringComparison.Ordinal) then
                        Some constructorInfo
                    else
                        None)
                |> List.groupBy (fun constructorInfo -> constructorInfo.TypeName)
                |> List.map (fun (typeName, constructors) ->
                    { TypeName = typeName
                      RepresentationClass = "tagged-object"
                      TagEncoding = "ordinal"
                      Constructors =
                        constructors
                        |> List.sortBy (fun constructor -> constructor.Tag)
                        |> List.map (fun constructor ->
                            { Name = constructor.Name
                              Tag = constructor.Tag
                              FieldRepresentations = constructor.FieldRepresentations
                              Provenance = constructor.Provenance })
                      Provenance = constructors.Head.Provenance })
                |> List.sortBy (fun layout -> layout.TypeName)

            backendFunctions
            |> Result.map (fun backendFunctions ->
                { Name = runtimeModule.Name
                  SourceFile = runtimeModule.SourceFile
                  Imports = runtimeModule.Imports
                  Exports = runtimeModule.Exports
                  EntryPoints =
                    backendFunctions
                    |> List.filter (fun binding -> binding.EntryPoint)
                    |> List.map (fun binding -> binding.Name)
                  IntrinsicTerms = runtimeModule.IntrinsicTerms
                  DataLayouts = dataLayouts
                  EnvironmentLayouts = environmentLayouts |> Seq.toList
                  Functions = backendFunctions })

        kRuntimeIR
        |> List.choose (fun runtimeModule ->
            match lowerModule runtimeModule with
            | Result.Ok backendModule ->
                Some backendModule
            | Result.Error _ ->
                None)

    let private dumpDiagnostic (diagnostic: Diagnostic) =
        { Severity = severityText diagnostic.Severity
          Message = diagnostic.Message
          FilePath = diagnostic.Location |> Option.map (fun location -> location.FilePath)
          StartLine = diagnostic.Location |> Option.map (fun location -> location.Start.Line)
          StartColumn = diagnostic.Location |> Option.map (fun location -> location.Start.Column)
          EndLine = diagnostic.Location |> Option.map (fun location -> location.End.Line)
          EndColumn = diagnostic.Location |> Option.map (fun location -> location.End.Column) }

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

    let private dumpFrontendDocument (document: KFrontIRModule) =
        { FilePath = document.FilePath
          ModuleHeader = document.ModuleHeader |> Option.map SyntaxFacts.moduleNameToText
          InferredModuleName = document.InferredModuleName |> Option.map SyntaxFacts.moduleNameToText
          ModuleIdentity = document.ModuleIdentity |> Option.map SyntaxFacts.moduleNameToText
          ModuleAttributes = document.ModuleAttributes
          Imports = document.Imports |> List.map importSpecText
          Tokens = document.Tokens |> List.map dumpToken
          Declarations = document.Declarations |> List.map dumpDeclaration
          Diagnostics = document.Diagnostics |> List.map dumpDiagnostic }

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

            let bodyText = binding.BodyText |> Option.defaultValue "<missing>"

            { Kind = declarationKindText declaration.Source
              Name = binding.Name
              Visibility = visibility
              IsOpaque = binding.IsOpaque
              Summary = $"{visibilityPrefix} {opaquePrefix}let {signatureText} = {bodyText}".Trim()
              TypeText = binding.ReturnTypeText
              BodyText = binding.BodyText
              Constructors = []
              Members = [] }
        | _ ->
            dumpDeclaration declaration.Source

    let private dumpCoreModule (moduleDump: KCoreModule) =
        { Name = moduleDump.Name
          SourceFile = moduleDump.SourceFile
          Imports = moduleDump.Imports |> List.map importSpecText
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
                  Body = binding.Body |> Option.map runtimeExpressionText |> Option.defaultValue "<intrinsic>"
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
                          Representation = backendRepresentationText parameter.Representation })
                  CallingConvention = backendCallingConventionText binding.CallingConvention
                  ReturnRepresentation = binding.ReturnRepresentation |> Option.map backendRepresentationText
                  EnvironmentLayout = binding.EnvironmentLayout
                  Intrinsic = binding.Intrinsic
                  Exported = binding.Exported
                  EntryPoint = binding.EntryPoint
                  ControlForm =
                    match binding.ControlForm with
                    | StructuredExpression -> "structured-expression"
                  Body = binding.Body |> Option.map backendExpressionText |> Option.defaultValue "<intrinsic>" })

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
                            constructor.FieldRepresentations |> List.map backendRepresentationText }) })

        let environmentLayouts =
            moduleDump.EnvironmentLayouts
            |> List.map (fun layout ->
                { Name = layout.Name
                  Slots =
                    layout.Slots
                    |> List.map (fun slot ->
                        { Name = slot.Name
                          Representation = backendRepresentationText slot.Representation }) })

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
                              Message = $"Import cycle detected: {message}"
                              Location = None }
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

    let private renderDumpDiagnosticSexpr (diagnostic: DumpDiagnostic) =
        [
            sexprStringAtom "severity" diagnostic.Severity
            sexprStringAtom "message" diagnostic.Message
            sexprOptionalStringAtom "file" diagnostic.FilePath
            diagnostic.StartLine |> Option.map (fun value -> sexprAtom "start-line" (string value)) |> Option.defaultValue (sexprAtom "start-line" "nil")
            diagnostic.StartColumn |> Option.map (fun value -> sexprAtom "start-column" (string value)) |> Option.defaultValue (sexprAtom "start-column" "nil")
            diagnostic.EndLine |> Option.map (fun value -> sexprAtom "end-line" (string value)) |> Option.defaultValue (sexprAtom "end-line" "nil")
            diagnostic.EndColumn |> Option.map (fun value -> sexprAtom "end-column" (string value)) |> Option.defaultValue (sexprAtom "end-column" "nil")
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

    let private metadataJson workspace checkpoint =
        {| schemaVersion = "1"
           languageVersion = languageVersion
           compiler = {| id = compilerImplementationId; version = compilerImplementationVersion |}
           checkpoint = checkpoint
           compilationRoot = workspace.SourceRoot
           backendProfile = workspace.BackendProfile
           backendIntrinsicSet = workspace.BackendIntrinsicIdentity
           elaborationAvailableIntrinsicTerms = workspace.ElaborationAvailableIntrinsicTerms
           buildConfiguration = {| packageMode = workspace.PackageMode |} |}

    let private metadataSexpr workspace checkpoint =
        let compilerAtom =
            let idAtom = sexprStringAtom "id" compilerImplementationId
            let versionAtom = sexprStringAtom "version" compilerImplementationVersion
            $"(compiler {idAtom} {versionAtom})"

        let buildConfigurationAtom =
            let packageModeAtom = sexprAtom "package-mode" (if workspace.PackageMode then "true" else "false")
            $"(build-configuration {packageModeAtom})"

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
        ]
        |> String.concat " "

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
                   buildConfiguration = {| packageMode = workspace.PackageMode |}
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
                   buildConfiguration = {| packageMode = workspace.PackageMode |}
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
                   buildConfiguration = {| packageMode = workspace.PackageMode |}
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
                   buildConfiguration = {| packageMode = workspace.PackageMode |}
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
                       buildConfiguration = {| packageMode = workspace.PackageMode |}
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

    let parse (options: CompilationOptions) inputs =
        let backendIntrinsicSet = Stdlib.intrinsicSetForBackendProfile options.BackendProfile
        let backendIntrinsicIdentity = backendIntrinsicSet.Identity
        let elaborationAvailableIntrinsicTerms =
            backendIntrinsicSet.ElaborationAvailableTermNames
            |> Set.toList
            |> List.sort

        let userDocuments =
            collectInputFiles options inputs
            |> List.map (parseFile options)

        let documents =
            if userDocuments |> List.exists (fun document -> document.ModuleName = Some Stdlib.PreludeModuleName) then
                userDocuments
            else
                parseBundledPrelude () :: userDocuments

        let diagnostics =
            (documents |> List.collect (fun document -> document.Diagnostics))
            @ detectImportCycles documents
            @ validateExpectDeclarations options.BackendProfile documents

        let kFrontIR =
            documents
            |> List.map buildKFrontIRModule
            |> List.sortBy (fun document -> document.FilePath)

        let kCore =
            kFrontIR
            |> List.map (lowerKCoreModule options.BackendProfile)
            |> List.sortBy (fun moduleDump -> moduleDump.SourceFile)

        let kRuntimeIR =
            kCore
            |> List.map lowerKRuntimeModule
            |> List.sortBy (fun moduleDump -> moduleDump.SourceFile)

        let kBackendIR =
            lowerKBackendModules kCore kRuntimeIR
            |> List.sortBy (fun moduleDump -> moduleDump.SourceFile)

        let workspace =
            { SourceRoot = options.SourceRoot
              PackageMode = options.PackageMode
              BackendProfile = options.BackendProfile
              BackendIntrinsicIdentity = backendIntrinsicIdentity
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

    let availableCheckpoints (_: WorkspaceCompilation) =
        CheckpointVerification.availableCheckpointNames

    let verifyCheckpoint (workspace: WorkspaceCompilation) checkpoint =
        CheckpointVerification.verifyCheckpoint workspace checkpoint

    let pipelineTrace (workspace: WorkspaceCompilation) =
        workspace.PipelineTrace

    let dumpStage (workspace: WorkspaceCompilation) checkpoint format =
        if not (CheckpointVerification.availableCheckpointNames |> List.contains checkpoint) then
            let available = String.concat ", " CheckpointVerification.availableCheckpointNames
            Result.Error $"Unknown checkpoint '{checkpoint}'. Available checkpoints: {available}."
        else
            match format with
            | StageDumpFormat.Json ->
                Result.Ok(dumpStageJson workspace checkpoint)
            | StageDumpFormat.SExpression ->
                Result.Ok(dumpStageSexpr workspace checkpoint)
