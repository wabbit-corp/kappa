namespace Kappa.Compiler

open System
open System.Buffers
open System.IO
open System.Collections.Generic
open System.Text
open System.Text.Json

type IFileSystem =
    abstract member GetFullPath: string -> string
    abstract member FileExists: string -> bool
    abstract member DirectoryExists: string -> bool
    abstract member EnumerateFiles: string * string * SearchOption -> seq<string>
    abstract member ReadAllText: string -> string

module FileSystem =
    let defaultImplementation =
        { new IFileSystem with
            member _.GetFullPath(path: string) = Path.GetFullPath(path)
            member _.FileExists(path: string) = File.Exists(path)
            member _.DirectoryExists(path: string) = Directory.Exists(path)

            member _.EnumerateFiles(path: string, searchPattern: string, searchOption: SearchOption) =
                Directory.EnumerateFiles(path, searchPattern, searchOption)

            member _.ReadAllText(path: string) = File.ReadAllText(path) }

type CompilationOptions =
    { SourceRoot: string
      PackageMode: bool
      BackendProfile: string
      FileSystem: IFileSystem }

module CompilationOptions =
    let createWithFileSystem (fileSystem: IFileSystem) sourceRoot =
        { SourceRoot = fileSystem.GetFullPath(sourceRoot)
          PackageMode = true
          BackendProfile = "interpreter"
          FileSystem = fileSystem }

    let create sourceRoot =
        createWithFileSystem FileSystem.defaultImplementation sourceRoot

type ParsedDocument =
    { Source: SourceText
      InferredModuleName: string list option
      Syntax: CompilationUnit
      Diagnostics: Diagnostic list }

    member this.ModuleName =
        match this.Syntax.ModuleHeader with
        | Some moduleName -> Some moduleName
        | None -> this.InferredModuleName

type KFrontIRPhase =
    | RAW
    | IMPORTS
    | DECLARATION_SHAPES
    | HEADER_TYPES
    | STATUS
    | IMPLICIT_SIGNATURES
    | BODY_RESOLVE
    | CHECKERS
    | CORE_LOWERING

module KFrontIRPhase =
    let all =
        [
            RAW
            IMPORTS
            DECLARATION_SHAPES
            HEADER_TYPES
            STATUS
            IMPLICIT_SIGNATURES
            BODY_RESOLVE
            CHECKERS
            CORE_LOWERING
        ]

    let checkpointName phase =
        match phase with
        | RAW -> "KFrontIR.RAW"
        | IMPORTS -> "KFrontIR.IMPORTS"
        | DECLARATION_SHAPES -> "KFrontIR.DECLARATION_SHAPES"
        | HEADER_TYPES -> "KFrontIR.HEADER_TYPES"
        | STATUS -> "KFrontIR.STATUS"
        | IMPLICIT_SIGNATURES -> "KFrontIR.IMPLICIT_SIGNATURES"
        | BODY_RESOLVE -> "KFrontIR.BODY_RESOLVE"
        | CHECKERS -> "KFrontIR.CHECKERS"
        | CORE_LOWERING -> "KFrontIR.CORE_LOWERING"

    let phaseName phase =
        checkpointName phase
        |> fun checkpoint -> checkpoint.Substring("KFrontIR.".Length)

type PipelineTraceEvent =
    | Parse
    | BuildKFrontIR
    | AdvancePhase
    | EmitInterface
    | EvaluateElaboration
    | LowerKCore
    | LowerKBackendIR
    | LowerTarget
    | Reuse
    | Verify

module PipelineTraceEvent =
    let toPortableName eventName =
        match eventName with
        | Parse -> "parse"
        | BuildKFrontIR -> "buildKFrontIR"
        | AdvancePhase -> "advancePhase"
        | EmitInterface -> "emitInterface"
        | EvaluateElaboration -> "evaluateElaboration"
        | LowerKCore -> "lowerKCore"
        | LowerKBackendIR -> "lowerKBackendIR"
        | LowerTarget -> "lowerTarget"
        | Reuse -> "reuse"
        | Verify -> "verify"

type PipelineTraceSubject =
    | File
    | Declaration
    | Module
    | Interface
    | KCoreUnit
    | KBackendIRUnit
    | TargetUnit

module PipelineTraceSubject =
    let toPortableName subject =
        match subject with
        | File -> "file"
        | Declaration -> "declaration"
        | Module -> "module"
        | Interface -> "interface"
        | KCoreUnit -> "KCoreUnit"
        | KBackendIRUnit -> "KBackendIRUnit"
        | TargetUnit -> "targetUnit"

type PipelineTraceStep =
    { Event: PipelineTraceEvent
      Subject: PipelineTraceSubject
      StepName: string
      InputCheckpoint: string
      OutputCheckpoint: string
      ChangedRepresentation: bool
      VerificationAttempted: bool
      VerificationSucceeded: bool option }

type StageDumpFormat =
    | Json
    | SExpression

type KFrontIRModule =
    { FilePath: string
      ModuleHeader: string list option
      InferredModuleName: string list option
      ModuleIdentity: string list option
      ModuleAttributes: string list
      Imports: ImportSpec list
      Tokens: Token list
      Declarations: TopLevelDeclaration list
      Diagnostics: Diagnostic list
      ResolvedPhases: Set<KFrontIRPhase> }

type KCoreOrigin =
    { FilePath: string
      ModuleName: string
      DeclarationName: string option
      IntroductionKind: string }

type KCoreParameter =
    { Name: string
      TypeText: string option }

type KCoreExpression =
    | KCoreLiteral of LiteralValue
    | KCoreName of string list
    | KCoreLambda of KCoreParameter list * KCoreExpression
    | KCoreIfThenElse of KCoreExpression * KCoreExpression * KCoreExpression
    | KCoreMatch of KCoreExpression * KCoreMatchCase list
    | KCoreApply of KCoreExpression * KCoreExpression list
    | KCoreUnary of operatorName: string * KCoreExpression
    | KCoreBinary of KCoreExpression * operatorName: string * KCoreExpression
    | KCorePrefixedString of prefix: string * parts: KCoreStringPart list

and KCoreStringPart =
    | KCoreStringText of string
    | KCoreStringInterpolation of KCoreExpression

and KCorePattern =
    | KCoreWildcardPattern
    | KCoreNamePattern of string
    | KCoreLiteralPattern of LiteralValue
    | KCoreConstructorPattern of string list * KCorePattern list

and KCoreMatchCase =
    { Pattern: KCorePattern
      Body: KCoreExpression }

type KCoreBinding =
    { Visibility: Visibility option
      IsOpaque: bool
      Name: string option
      Parameters: KCoreParameter list
      ReturnTypeText: string option
      Body: KCoreExpression option
      BodyText: string option
      Provenance: KCoreOrigin }

type KCoreDeclaration =
    { Source: TopLevelDeclaration
      Binding: KCoreBinding option
      Provenance: KCoreOrigin }

type KCoreModule =
    { Name: string
      SourceFile: string
      ModuleAttributes: string list
      Imports: ImportSpec list
      IntrinsicTerms: string list
      Declarations: KCoreDeclaration list }

type KBackendExpression =
    | KBackendLiteral of LiteralValue
    | KBackendName of string list
    | KBackendClosure of string list * KBackendExpression
    | KBackendIfThenElse of KBackendExpression * KBackendExpression * KBackendExpression
    | KBackendMatch of KBackendExpression * KBackendMatchCase list
    | KBackendApply of KBackendExpression * KBackendExpression list
    | KBackendUnary of operatorName: string * KBackendExpression
    | KBackendBinary of KBackendExpression * operatorName: string * KBackendExpression
    | KBackendPrefixedString of prefix: string * parts: KBackendStringPart list

and KBackendStringPart =
    | KBackendStringText of string
    | KBackendStringInterpolation of KBackendExpression

and KBackendPattern =
    | KBackendWildcardPattern
    | KBackendNamePattern of string
    | KBackendLiteralPattern of LiteralValue
    | KBackendConstructorPattern of string list * KBackendPattern list

and KBackendMatchCase =
    { Pattern: KBackendPattern
      Body: KBackendExpression }

type KBackendConstructor =
    { Name: string
      Arity: int
      TypeName: string
      Provenance: KCoreOrigin }

type KBackendBinding =
    { Name: string
      Parameters: string list
      Body: KBackendExpression option
      Intrinsic: bool
      Provenance: KCoreOrigin }

type KBackendModule =
    { Name: string
      SourceFile: string
      Imports: ImportSpec list
      Exports: string list
      IntrinsicTerms: string list
      Constructors: KBackendConstructor list
      Bindings: KBackendBinding list }

type WorkspaceCompilation =
    { SourceRoot: string
      PackageMode: bool
      BackendProfile: string
      Documents: ParsedDocument list
      KFrontIR: KFrontIRModule list
      KCore: KCoreModule list
      KBackendIR: KBackendModule list
      Diagnostics: Diagnostic list
      PipelineTrace: PipelineTraceStep list }

    member this.HasErrors =
        this.Diagnostics |> List.exists (fun diagnostic -> diagnostic.Severity = Error)

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

    type DumpBackendBinding =
        { Name: string
          Parameters: string list
          Body: string
          Intrinsic: bool }

    type DumpBackendConstructor =
        { Name: string
          Arity: int
          TypeName: string }

    type DumpBackendModule =
        { Name: string
          SourceFile: string
          Exports: string list
          IntrinsicTerms: string list
          Constructors: DumpBackendConstructor list
          Bindings: DumpBackendBinding list }

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

    let private makeDiagnostic message =
        { Severity = Error
          Message = message
          Location = None }

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

    let rec private lowerKBackendPattern pattern =
        match pattern with
        | KCoreWildcardPattern ->
            KBackendWildcardPattern
        | KCoreNamePattern name ->
            KBackendNamePattern name
        | KCoreLiteralPattern literal ->
            KBackendLiteralPattern literal
        | KCoreConstructorPattern(name, arguments) ->
            KBackendConstructorPattern(name, arguments |> List.map lowerKBackendPattern)

    let rec private lowerKBackendExpression expression =
        match expression with
        | KCoreLiteral literal ->
            KBackendLiteral literal
        | KCoreName segments ->
            KBackendName segments
        | KCoreLambda(parameters, body) ->
            KBackendClosure(parameters |> List.map (fun parameter -> parameter.Name), lowerKBackendExpression body)
        | KCoreIfThenElse(condition, whenTrue, whenFalse) ->
            KBackendIfThenElse(
                lowerKBackendExpression condition,
                lowerKBackendExpression whenTrue,
                lowerKBackendExpression whenFalse
            )
        | KCoreMatch(scrutinee, cases) ->
            KBackendMatch(
                lowerKBackendExpression scrutinee,
                cases
                |> List.map (fun caseClause ->
                    { Pattern = lowerKBackendPattern caseClause.Pattern
                      Body = lowerKBackendExpression caseClause.Body })
            )
        | KCoreApply(callee, arguments) ->
            KBackendApply(lowerKBackendExpression callee, arguments |> List.map lowerKBackendExpression)
        | KCoreUnary(operatorName, operand) ->
            KBackendUnary(operatorName, lowerKBackendExpression operand)
        | KCoreBinary(left, operatorName, right) ->
            KBackendBinary(lowerKBackendExpression left, operatorName, lowerKBackendExpression right)
        | KCorePrefixedString(prefix, parts) ->
            KBackendPrefixedString(
                prefix,
                parts
                |> List.map (function
                    | KCoreStringText text -> KBackendStringText text
                    | KCoreStringInterpolation inner -> KBackendStringInterpolation(lowerKBackendExpression inner))
            )

    let rec private backendPatternText pattern =
        match pattern with
        | KBackendWildcardPattern -> "_"
        | KBackendNamePattern name -> name
        | KBackendLiteralPattern(LiteralValue.Integer value) -> string value
        | KBackendLiteralPattern(LiteralValue.Float value) -> string value
        | KBackendLiteralPattern(LiteralValue.String value) -> $"\"{value}\""
        | KBackendLiteralPattern(LiteralValue.Character value) -> $"'{value}'"
        | KBackendLiteralPattern LiteralValue.Unit -> "()"
        | KBackendConstructorPattern(name, arguments) ->
            let nameText = String.concat "." name

            match arguments with
            | [] ->
                nameText
            | _ ->
                let argumentText = arguments |> List.map backendPatternText |> String.concat " "
                $"({nameText} {argumentText})"

    let rec private backendExpressionText expression =
        match expression with
        | KBackendLiteral(LiteralValue.Integer value) -> string value
        | KBackendLiteral(LiteralValue.Float value) -> string value
        | KBackendLiteral(LiteralValue.String value) -> $"\"{value}\""
        | KBackendLiteral(LiteralValue.Character value) -> $"'{value}'"
        | KBackendLiteral LiteralValue.Unit -> "()"
        | KBackendName segments -> String.concat "." segments
        | KBackendClosure(parameters, body) ->
            let names = String.concat " " parameters
            $"(closure ({names}) {backendExpressionText body})"
        | KBackendIfThenElse(condition, whenTrue, whenFalse) ->
            $"(if {backendExpressionText condition} {backendExpressionText whenTrue} {backendExpressionText whenFalse})"
        | KBackendMatch(scrutinee, cases) ->
            let caseText =
                cases
                |> List.map (fun caseClause -> $"(case {backendPatternText caseClause.Pattern} {backendExpressionText caseClause.Body})")
                |> String.concat " "

            $"(match {backendExpressionText scrutinee} {caseText})"
        | KBackendApply(callee, arguments) ->
            let argumentText =
                arguments
                |> List.map backendExpressionText
                |> String.concat " "

            if String.IsNullOrWhiteSpace(argumentText) then
                $"(apply {backendExpressionText callee})"
            else
                $"(apply {backendExpressionText callee} {argumentText})"
        | KBackendUnary(operatorName, operand) ->
            $"({operatorName} {backendExpressionText operand})"
        | KBackendBinary(left, operatorName, right) ->
            $"({operatorName} {backendExpressionText left} {backendExpressionText right})"
        | KBackendPrefixedString(prefix, parts) ->
            let partText =
                parts
                |> List.map (function
                    | KBackendStringText text -> $"text:{text}"
                    | KBackendStringInterpolation inner -> $"interp:{backendExpressionText inner}")
                |> String.concat " | "

            $"({prefix}-string {partText})"

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

    let private lowerKBackendModule (coreModule: KCoreModule) =
        let termBindings =
            coreModule.Declarations
            |> List.choose (fun declaration ->
                match declaration.Binding with
                | Some binding when binding.Name.IsSome ->
                    Some
                        { Name = binding.Name.Value
                          Parameters = binding.Parameters |> List.map (fun parameter -> parameter.Name)
                          Body = binding.Body |> Option.map lowerKBackendExpression
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

    let private dumpBackendModule (moduleDump: KBackendModule) =
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
                  Body = binding.Body |> Option.map backendExpressionText |> Option.defaultValue "<intrinsic>"
                  Intrinsic = binding.Intrinsic })

        { Name = moduleDump.Name
          SourceFile = moduleDump.SourceFile
          Exports = moduleDump.Exports
          IntrinsicTerms = moduleDump.IntrinsicTerms
          Constructors = constructors
          Bindings = bindings }

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

    let private availableCheckpointNames =
        [
            "surface-source"
            yield! KFrontIRPhase.all |> List.map KFrontIRPhase.checkpointName
            "KCore"
            "KBackendIR"
        ]

    let private tryParseCheckpoint checkpoint =
        if String.Equals(checkpoint, "surface-source", StringComparison.Ordinal) then
            Some None
        else
            KFrontIRPhase.all
            |> List.tryFind (fun phase -> String.Equals(KFrontIRPhase.checkpointName phase, checkpoint, StringComparison.Ordinal))
            |> Option.map Some

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

    let private renderDumpBackendBindingSexpr (binding: DumpBackendBinding) =
        [
            sexprStringAtom "name" binding.Name
            sexprStringList "parameters" binding.Parameters
            sexprStringAtom "body" binding.Body
            sexprAtom "intrinsic" (if binding.Intrinsic then "true" else "false")
        ]
        |> String.concat " "
        |> fun body -> $"(binding {body})"

    let private renderDumpBackendConstructorSexpr (constructor: DumpBackendConstructor) =
        [
            sexprStringAtom "name" constructor.Name
            sexprAtom "arity" (string constructor.Arity)
            sexprStringAtom "type-name" constructor.TypeName
        ]
        |> String.concat " "
        |> fun body -> $"(constructor {body})"

    let private renderDumpBackendModuleSexpr (moduleDump: DumpBackendModule) =
        [
            sexprStringAtom "name" moduleDump.Name
            sexprStringAtom "source-file" moduleDump.SourceFile
            sexprStringList "exports" moduleDump.Exports
            sexprStringList "intrinsic-terms" moduleDump.IntrinsicTerms
            let constructorBody =
                moduleDump.Constructors
                |> List.map renderDumpBackendConstructorSexpr
                |> String.concat " "

            if String.IsNullOrWhiteSpace(constructorBody) then "(constructors)" else $"(constructors {constructorBody})"
            let bindingBody =
                moduleDump.Bindings
                |> List.map renderDumpBackendBindingSexpr
                |> String.concat " "

            if String.IsNullOrWhiteSpace(bindingBody) then "(bindings)" else $"(bindings {bindingBody})"
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
            buildConfigurationAtom
        ]
        |> String.concat " "

    let private verifySurfaceSource (workspace: WorkspaceCompilation) =
        let duplicatePaths =
            workspace.Documents
            |> List.countBy (fun document -> document.Source.FilePath)
            |> List.filter (fun (_, count) -> count > 1)

        [
            for filePath, _ in duplicatePaths do
                yield makeDiagnostic $"Checkpoint 'surface-source' requires unique file identities, but '{filePath}' appeared more than once."

            for document in workspace.Documents do
                if document.Source.LineCount <= 0 then
                    yield makeDiagnostic $"Checkpoint 'surface-source' requires non-empty line tables for '{document.Source.FilePath}'."
        ]

    let private verifyFrontendCheckpoint (workspace: WorkspaceCompilation) checkpoint =
        [
            let duplicatePaths =
                workspace.KFrontIR
                |> List.countBy (fun document -> document.FilePath)
                |> List.filter (fun (_, count) -> count > 1)

            for filePath, _ in duplicatePaths do
                yield makeDiagnostic $"Checkpoint '{checkpoint}' requires unique file identities, but '{filePath}' appeared more than once."

            for document in workspace.KFrontIR do
                match List.tryLast document.Tokens with
                | Some token when token.Kind = EndOfFile -> ()
                | _ ->
                    yield makeDiagnostic $"Checkpoint '{checkpoint}' requires an EOF token for '{document.FilePath}'."
        ]

    let private verifyKCoreCheckpoint (workspace: WorkspaceCompilation) =
        let duplicateModules =
            workspace.KCore
            |> List.map (fun moduleDump -> moduleDump.Name)
            |> List.countBy id
            |> List.filter (fun (_, count) -> count > 1)

        [
            yield! verifyFrontendCheckpoint workspace "KCore"

            for moduleName, _ in duplicateModules do
                yield makeDiagnostic $"Checkpoint 'KCore' requires unique module identities, but '{moduleName}' appeared more than once."
        ]

    let private verifyKBackendIRCheckpoint (workspace: WorkspaceCompilation) =
        let backendModules =
            workspace.KBackendIR
            |> List.sortBy (fun moduleDump -> moduleDump.SourceFile)

        [
            yield! verifyKCoreCheckpoint workspace

            for moduleDump in backendModules do
                let duplicateBindings =
                    moduleDump.Bindings
                    |> List.countBy (fun binding -> binding.Name)
                    |> List.filter (fun (_, count) -> count > 1)

                for bindingName, _ in duplicateBindings do
                    yield makeDiagnostic $"Checkpoint 'KBackendIR' requires unique binding identities within module '{moduleDump.Name}', but '{bindingName}' was duplicated."
        ]

    let verifyCheckpoint (workspace: WorkspaceCompilation) checkpoint =
        match checkpoint with
        | "surface-source" ->
            verifySurfaceSource workspace
        | "KCore" ->
            verifyKCoreCheckpoint workspace
        | "KBackendIR" ->
            verifyKBackendIRCheckpoint workspace
        | _ ->
            match tryParseCheckpoint checkpoint with
            | Some(Some _) ->
                verifyFrontendCheckpoint workspace checkpoint
            | Some None ->
                verifySurfaceSource workspace
            | None ->
                [ makeDiagnostic $"Unknown checkpoint '{checkpoint}'." ]

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
        let frontendVerified = verifyCheckpoint workspace frontendCheckpoint |> List.isEmpty
        let coreVerified = verifyCheckpoint workspace "KCore" |> List.isEmpty
        let backendVerified = verifyCheckpoint workspace "KBackendIR" |> List.isEmpty

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
                    traceStep Parse File $"parse {document.FilePath}" "surface-source" "surface-source" false false None
                    traceStep BuildKFrontIR File $"build KFrontIR for {document.FilePath}" "surface-source" (KFrontIRPhase.checkpointName RAW) true false None
                ]

            let phaseSteps =
                phaseTransitions
                |> List.map (fun (fromPhase, toPhase) ->
                    traceStep
                        AdvancePhase
                        Module
                        $"advance {label} to {KFrontIRPhase.phaseName toPhase}"
                        (KFrontIRPhase.checkpointName fromPhase)
                        (KFrontIRPhase.checkpointName toPhase)
                        true
                        false
                        None)

            let verifySteps =
                [
                    traceStep Verify Module $"verify {label} at {frontendCheckpoint}" frontendCheckpoint frontendCheckpoint false true (Some frontendVerified)
                    traceStep LowerKCore Module $"lower {label} to KCore" (KFrontIRPhase.checkpointName CORE_LOWERING) "KCore" true false None
                    traceStep Verify KCoreUnit $"verify {label} at KCore" "KCore" "KCore" false true (Some coreVerified)
                    traceStep LowerKBackendIR KCoreUnit $"lower {label} to KBackendIR" "KCore" "KBackendIR" true false None
                    traceStep Verify KBackendIRUnit $"verify {label} at KBackendIR" "KBackendIR" "KBackendIR" false true (Some backendVerified)
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
                   buildConfiguration = {| packageMode = workspace.PackageMode |}
                   modules = modules
                   diagnostics = workspace.Diagnostics |> List.map dumpDiagnostic |}
        | _ ->
            match tryParseCheckpoint checkpoint with
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
            match tryParseCheckpoint checkpoint with
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

    let parse options inputs =
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

        let kBackendIR =
            kCore
            |> List.map lowerKBackendModule
            |> List.sortBy (fun moduleDump -> moduleDump.SourceFile)

        let workspace =
            { SourceRoot = options.SourceRoot
              PackageMode = options.PackageMode
              BackendProfile = options.BackendProfile
              Documents = documents
              KFrontIR = kFrontIR
              KCore = kCore
              KBackendIR = kBackendIR
              Diagnostics = diagnostics
              PipelineTrace = [] }

        { workspace with
            PipelineTrace = buildPipelineTrace workspace }

    let availableCheckpoints (_: WorkspaceCompilation) =
        availableCheckpointNames

    let pipelineTrace (workspace: WorkspaceCompilation) =
        workspace.PipelineTrace

    let dumpStage (workspace: WorkspaceCompilation) checkpoint format =
        if not (availableCheckpointNames |> List.contains checkpoint) then
            let available = String.concat ", " availableCheckpointNames
            Result.Error $"Unknown checkpoint '{checkpoint}'. Available checkpoints: {available}."
        else
            match format with
            | StageDumpFormat.Json ->
                Result.Ok(dumpStageJson workspace checkpoint)
            | StageDumpFormat.SExpression ->
                Result.Ok(dumpStageSexpr workspace checkpoint)
