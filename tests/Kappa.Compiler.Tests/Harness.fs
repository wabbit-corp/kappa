// Provides the public test harness facade and assertion interpreter used by compiler tests.
module Harness

open System
open System.IO
open System.Text
open System.Text.RegularExpressions
open HarnessFixtureModel
open HarnessFixtures
open HarnessWorkspace
open Kappa.Compiler
open Xunit

let createSource = HarnessWorkspace.createSource
let lexAndParse = HarnessWorkspace.lexAndParse
let compileInMemoryWorkspace = HarnessWorkspace.compileInMemoryWorkspace
let compileInMemoryWorkspaceWithBackend = HarnessWorkspace.compileInMemoryWorkspaceWithBackend
let evaluateInMemoryBinding = HarnessWorkspace.evaluateInMemoryBinding
let discoverKpFixtureCases = HarnessFixtures.discoverKpFixtureCases

type InMemoryFileSystem = HarnessWorkspace.InMemoryFileSystem
type KpFixtureMode = HarnessFixtures.KpFixtureMode
type KpFixtureDirectiveSource = HarnessFixtures.KpFixtureDirectiveSource
type KpFixtureRelation = HarnessFixtures.KpFixtureRelation
type KpFixtureConfiguration = HarnessFixtures.KpFixtureConfiguration
type KpFixtureAssertion = HarnessFixtures.KpFixtureAssertion
type KpFixtureCase = HarnessFixtures.KpFixtureCase

module KpFixtureConfiguration =
    let defaultValue = HarnessFixtures.KpFixtureConfiguration.defaultValue

let private rootedFilePath = HarnessSupport.rootedFilePath

type ProcessResult = HarnessSupport.ProcessResult
type LoadedManagedAssembly = HarnessSupport.LoadedManagedAssembly

let createScratchDirectory = HarnessSupport.createScratchDirectory
let currentRid = HarnessSupport.currentRid
let writeWorkspaceFiles = HarnessSupport.writeWorkspaceFiles
let executablePath = HarnessSupport.executablePath
let runProcess = HarnessSupport.runProcess
let runProcessWithEnvironment = HarnessSupport.runProcessWithEnvironment
let runProcessWithInput = HarnessSupport.runProcessWithInput
let runProcessWithEnvironmentAndInput = HarnessSupport.runProcessWithEnvironmentAndInput
let repoZigBootstrapCommand = HarnessSupport.repoZigBootstrapCommand
let ensureRepoZigExecutablePath = HarnessSupport.ensureRepoZigExecutablePath
let loadManagedAssembly = HarnessSupport.loadManagedAssembly

let private isLayoutOrSentinelToken (token: Token) =
    match token.Kind with
    | Newline
    | Indent
    | Dedent
    | EndOfFile -> true
    | _ -> false

let private tokenTexts (tokens: Token list) =
    tokens
    |> List.filter (isLayoutOrSentinelToken >> not)
    |> List.map (fun token -> token.Text)

let private formatDiagnostics = HarnessExecution.formatDiagnostics
let private countDiagnosticsBySeverity = HarnessExecution.countDiagnosticsBySeverity
let private normalizeLineEndings = HarnessExecution.normalizeLineEndings
let private normalizeExecutionOutput = HarnessExecution.normalizeExecutionOutput
let private executeBindingWithCapturedOutput = HarnessExecution.executeBindingWithCapturedOutput

let private tokenizeAssertionTypeText (expectedTypeText: string) =
    let source = createSource "__fixture_assertion_type__.kp" expectedTypeText
    let lexed = Lexer.tokenize source

    Assert.Empty(lexed.Diagnostics)
    tokenTexts lexed.Tokens

let private normalizeFixtureText (value: string) =
    value.Trim().ToLowerInvariant()

let private declarationKindText declaration =
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

let private visibilityText visibility =
    match visibility with
    | Some Visibility.Public -> "public"
    | Some Visibility.Private -> "private"
    | None -> ""

let private moduleSpecifierText moduleSpecifier =
    match moduleSpecifier with
    | Dotted segments ->
        SyntaxFacts.moduleNameToText segments
    | Url url ->
        $"\"{url}\""

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

let private declarationDescriptorText declaration =
    let joinParts parts =
        parts
        |> List.filter (String.IsNullOrWhiteSpace >> not)
        |> String.concat " "

    match declaration with
    | ImportDeclaration (true, specs) ->
        joinParts [ "export"; (specs |> List.map importSpecText |> String.concat " | ") ]
    | ImportDeclaration (false, specs) ->
        joinParts [ "import"; (specs |> List.map importSpecText |> String.concat " | ") ]
    | FixityDeclarationNode declaration ->
        let fixityText =
            match declaration.Fixity with
            | Infix _ -> "infix"
            | Prefix -> "prefix"
            | Postfix -> "postfix"

        joinParts [ "fixity"; fixityText; declaration.OperatorName ]
    | ExpectDeclarationNode declaration ->
        match declaration with
        | ExpectTypeDeclaration declaration -> joinParts [ "expect"; "type"; declaration.Name ]
        | ExpectTraitDeclaration declaration -> joinParts [ "expect"; "trait"; declaration.Name ]
        | ExpectTermDeclaration declaration -> joinParts [ "expect"; "term"; declaration.Name ]
    | SignatureDeclaration declaration ->
        joinParts [ visibilityText declaration.Visibility; (if declaration.IsOpaque then "opaque" else ""); "signature"; declaration.Name ]
    | LetDeclaration declaration ->
        joinParts [ visibilityText declaration.Visibility; (if declaration.IsOpaque then "opaque" else ""); "let"; defaultArg declaration.Name "<anonymous>" ]
    | DataDeclarationNode declaration ->
        joinParts [ visibilityText declaration.Visibility; (if declaration.IsOpaque then "opaque" else ""); "data"; declaration.Name ]
    | TypeAliasNode declaration ->
        joinParts [ visibilityText declaration.Visibility; (if declaration.IsOpaque then "opaque" else ""); "type"; declaration.Name ]
    | TraitDeclarationNode declaration ->
        joinParts [ visibilityText declaration.Visibility; "trait"; declaration.Name ]
    | InstanceDeclarationNode declaration ->
        joinParts [ "instance"; declaration.TraitName ]
    | UnknownDeclaration _ ->
        "unknown"

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

let private tryFindDocumentForFilePath (workspace: WorkspaceCompilation) (filePath: string) =
    let normalizedPath = Path.GetFullPath(filePath)

    workspace.Documents
    |> List.tryFind (fun document ->
        String.Equals(document.Source.FilePath, normalizedPath, StringComparison.OrdinalIgnoreCase))

let private lexFixtureFile (filePath: string) =
    let source = createSource filePath (File.ReadAllText(filePath))
    Lexer.tokenize source

let private tryFindDataDeclaration typeName (document: ParsedDocument) =
    document.Syntax.Declarations
    |> List.tryPick (function
        | DataDeclarationNode declaration when String.Equals(declaration.Name, typeName, StringComparison.Ordinal) ->
            Some declaration
        | _ ->
            None)

let private tryFindTraitDeclaration traitName (document: ParsedDocument) =
    document.Syntax.Declarations
    |> List.tryPick (function
        | TraitDeclarationNode declaration when String.Equals(declaration.Name, traitName, StringComparison.Ordinal) ->
            Some declaration
        | _ ->
            None)

let private tryFindLetDefinition bindingName (document: ParsedDocument) =
    document.Syntax.Declarations
    |> List.tryPick (function
        | LetDeclaration declaration when declaration.Name = Some bindingName ->
            Some declaration
        | _ ->
            None)

let rec private fixturePatternText pattern =
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
        | [] -> nameText
        | _ ->
            let argumentText = arguments |> List.map fixturePatternText |> String.concat " "
            $"{nameText} {argumentText}"

let private fixtureBindPatternText (binding: SurfaceBindPattern) =
    let quantityText =
        binding.Quantity
        |> Option.map (fun quantity -> Quantity.toSurfaceText quantity + " ")
        |> Option.defaultValue ""

    quantityText + fixturePatternText binding.Pattern

let private doItemDescriptor statement =
    match statement with
    | DoLet(binding, _) -> $"let {fixtureBindPatternText binding}"
    | DoBind(binding, _) -> $"<- {fixtureBindPatternText binding}"
    | DoUsing(pattern, _) -> $"using {fixturePatternText pattern}"
    | DoVar(name, _) -> $"var {name}"
    | DoAssign(name, _) -> $"assign {name}"
    | DoWhile _ -> "while"
    | DoExpression _ -> "expression"

let private qualifyFixtureBindingTarget (workspace: WorkspaceCompilation) (filePath: string) (target: string) =
    if target.Contains(".", StringComparison.Ordinal) then
        target
    else
        tryFindDocumentForFilePath workspace filePath
        |> Option.bind (fun document ->
            document.ModuleName
            |> Option.map (fun moduleName ->
                $"{SyntaxFacts.moduleNameToText moduleName}.{target}"))
        |> Option.defaultValue target

let private tryFindDeclaredTypeTokensInDocument bindingName (document: ParsedDocument) =
    let signatureTokens =
        document.Syntax.Declarations
        |> List.tryPick (function
            | SignatureDeclaration signature when String.Equals(signature.Name, bindingName, StringComparison.Ordinal) ->
                Some signature.TypeTokens
            | _ ->
                None)

    match signatureTokens with
    | Some tokens ->
        Some(tokenTexts tokens)
    | None ->
        document.Syntax.Declarations
        |> List.tryPick (function
            | LetDeclaration definition when definition.Name = Some bindingName ->
                definition.ReturnTypeTokens
                |> Option.map tokenTexts
            | _ ->
                None)

let private tryFindDeclaredTypeTokens (workspace: WorkspaceCompilation) (currentFilePath: string option) (target: string) =
    let segments =
        target.Split('.', StringSplitOptions.RemoveEmptyEntries)
        |> Array.toList

    match segments with
    | [] ->
        invalidArg (nameof target) "Fixture assertion target cannot be empty."
    | [ bindingName ] ->
        let matches =
            workspace.Documents
            |> List.choose (fun document ->
                tryFindDeclaredTypeTokensInDocument bindingName document
                |> Option.map (fun tokens ->
                    let moduleName =
                        document.ModuleName
                        |> Option.map SyntaxFacts.moduleNameToText
                        |> Option.defaultValue "<unknown>"

                    moduleName, tokens))

        match matches with
        | [] ->
            None
        | _ when currentFilePath.IsSome ->
            let preferredModuleName =
                workspace.Documents
                |> List.tryFind (fun document ->
                    String.Equals(document.Source.FilePath, Path.GetFullPath(currentFilePath.Value), StringComparison.OrdinalIgnoreCase))
                |> Option.bind (fun document -> document.ModuleName |> Option.map SyntaxFacts.moduleNameToText)

            match preferredModuleName with
            | Some moduleName ->
                matches
                |> List.tryPick (fun (candidateModuleName, tokens) ->
                    if String.Equals(candidateModuleName, moduleName, StringComparison.Ordinal) then
                        Some tokens
                    else
                        None)
                |> Option.orElseWith (fun () ->
                    match matches with
                    | [ _, tokens ] ->
                        Some tokens
                    | multiple ->
                        let owners = multiple |> List.map fst |> String.concat ", "
                        invalidOp $"assertType target '{target}' is ambiguous across modules: {owners}.")
            | None ->
                match matches with
                | [ _, tokens ] ->
                    Some tokens
                | multiple ->
                    let owners = multiple |> List.map fst |> String.concat ", "
                    invalidOp $"assertType target '{target}' is ambiguous across modules: {owners}."
        | [ _, tokens ] ->
            Some tokens
        | multiple ->
            let owners = multiple |> List.map fst |> String.concat ", "
            invalidOp $"assertType target '{target}' is ambiguous across modules: {owners}."
    | _ ->
        let moduleName = segments |> List.take (segments.Length - 1) |> SyntaxFacts.moduleNameToText
        let bindingName = List.last segments

        workspace.Documents
        |> List.tryPick (fun document ->
            match document.ModuleName with
            | Some candidate when String.Equals(SyntaxFacts.moduleNameToText candidate, moduleName, StringComparison.Ordinal) ->
                tryFindDeclaredTypeTokensInDocument bindingName document
            | _ ->
                None)

let private requireFixtureDocument (workspace: WorkspaceCompilation) (filePath: string) lineNumber =
    match tryFindDocumentForFilePath workspace filePath with
    | Some document ->
        document
    | None ->
        failwithf "Could not find parsed document for '%s' (%s:%d)." filePath filePath lineNumber

let private requireFixtureDocumentByRelativePath
    (workspace: WorkspaceCompilation)
    (suiteRoot: string)
    (relativePath: string)
    lineNumber
    =
    let fullPath = rootedFilePath suiteRoot relativePath |> Path.GetFullPath

    match tryFindDocumentForFilePath workspace fullPath with
    | Some document ->
        document
    | None ->
        failwithf "Could not find parsed document for '%s' (%s:%d)." relativePath fullPath lineNumber

let private compileFixtureWorkspace = HarnessExecution.compileFixtureWorkspace
let private executeFixtureRun = HarnessExecution.executeFixtureRun

let private compareFixtureRelation relation actual expected =
    match relation with
    | KpFixtureRelation.Equal -> actual = expected
    | KpFixtureRelation.NotEqual -> actual <> expected
    | KpFixtureRelation.LessThan -> actual < expected
    | KpFixtureRelation.LessThanOrEqual -> actual <= expected
    | KpFixtureRelation.GreaterThan -> actual > expected
    | KpFixtureRelation.GreaterThanOrEqual -> actual >= expected

let private evaluateFixtureBinding (workspace: WorkspaceCompilation) (filePath: string) (target: string) =
    let bindingTarget = qualifyFixtureBindingTarget workspace filePath target
    bindingTarget, Interpreter.evaluateBinding workspace bindingTarget

let private executeFixtureBinding (workspace: WorkspaceCompilation) (filePath: string) (target: string) =
    let bindingTarget = qualifyFixtureBindingTarget workspace filePath target
    let result, output = executeBindingWithCapturedOutput workspace bindingTarget
    bindingTarget, result, output

let runKpFixtureCase (fixtureCase: KpFixtureCase) =
    Assert.NotEmpty(fixtureCase.SourceFiles)
    Assert.NotEmpty(fixtureCase.Assertions)

    let workspace = compileFixtureWorkspace fixtureCase

    let expectsDiagnostics =
        fixtureCase.Assertions
        |> List.exists (function
            | AssertErrorCount(expectedCount, _, _) when expectedCount > 0 -> true
            | AssertWarningCount(expectedCount, _, _) when expectedCount > 0 -> true
            | AssertDiagnosticCodes _ -> true
            | AssertDiagnosticAt _ -> true
            | AssertDiagnosticMatch _ -> true
            | _ -> false)

    if not expectsDiagnostics then
        Assert.False(
            workspace.HasErrors,
            $"Fixture '{fixtureCase.Name}' failed to compile:{Environment.NewLine}{formatDiagnostics workspace.Diagnostics}"
        )

    let runResult = lazy (executeFixtureRun fixtureCase workspace)

    let requireRunMode assertionName =
        if fixtureCase.Configuration.Mode <> KpFixtureMode.Run then
            invalidOp $"'{assertionName}' is valid only for fixtures running in mode run."

    for assertion in fixtureCase.Assertions do
        match assertion with
        | AssertNoErrors _ ->
            Assert.Equal(0, countDiagnosticsBySeverity DiagnosticSeverity.Error workspace.Diagnostics)
        | AssertNoWarnings _ ->
            Assert.Equal(0, countDiagnosticsBySeverity DiagnosticSeverity.Warning workspace.Diagnostics)
        | AssertErrorCount(expectedCount, _, _) ->
            Assert.Equal(expectedCount, countDiagnosticsBySeverity DiagnosticSeverity.Error workspace.Diagnostics)
        | AssertWarningCount(expectedCount, _, _) ->
            Assert.Equal(expectedCount, countDiagnosticsBySeverity DiagnosticSeverity.Warning workspace.Diagnostics)
        | AssertDiagnosticCodes(expectedCodes, _, _) ->
            let expected =
                expectedCodes |> List.map normalizeFixtureText

            let actual =
                workspace.Diagnostics
                |> List.map (fun diagnostic -> diagnostic.Code)
                |> List.map normalizeFixtureText

            Assert.Equal<string list>(expected, actual)
        | AssertDiagnosticAt(relativePath, severity, code, expectedLine, expectedColumn, _, lineNumber) ->
            let expectedPath = rootedFilePath fixtureCase.Root relativePath |> Path.GetFullPath
            let normalizedCode = normalizeFixtureText code

            let matches diagnostic =
                match diagnostic.Location with
                | Some location ->
                    diagnostic.Severity = severity
                    && normalizeFixtureText diagnostic.Code = normalizedCode
                    && String.Equals(Path.GetFullPath(location.FilePath), expectedPath, StringComparison.OrdinalIgnoreCase)
                    && location.Start.Line = expectedLine
                    && (expectedColumn |> Option.forall (fun column -> location.Start.Column = column))
                | None ->
                    false

            let expectedColumnText =
                expectedColumn
                |> Option.map (fun column -> $":{column}")
                |> Option.defaultValue ""

            Assert.True(
                workspace.Diagnostics |> List.exists matches,
                $"Could not find diagnostic {severity} {code} at {relativePath}:{expectedLine}{expectedColumnText} ({fixtureCase.Name}:{lineNumber}). Actual diagnostics:{Environment.NewLine}{formatDiagnostics workspace.Diagnostics}"
            )
        | AssertDiagnosticMatch(pattern, _, lineNumber) ->
            let regex =
                try
                    Regex(pattern, RegexOptions.ECMAScript)
                with :? ArgumentException as ex ->
                    invalidOp $"Invalid assertDiagnosticMatch regex '{pattern}' ({fixtureCase.Name}:{lineNumber}): {ex.Message}"

            Assert.True(
                workspace.Diagnostics
                |> List.exists (fun item -> regex.IsMatch(item.Message)),
                $"Could not find diagnostic matching /{pattern}/ ({fixtureCase.Name}:{lineNumber}). Actual diagnostics:{Environment.NewLine}{formatDiagnostics workspace.Diagnostics}"
            )
        | AssertType(target, expectedTypeText, filePath, lineNumber) ->
            let expectedTokens = tokenizeAssertionTypeText expectedTypeText

            match tryFindDeclaredTypeTokens workspace (Some filePath) target with
            | Some actualTokens ->
                Assert.Equal<string list>(expectedTokens, actualTokens)
            | None ->
                failwithf "Could not find a declared top-level type for '%s' (%s:%d)." target filePath lineNumber
        | AssertFileDeclarationKinds(relativePath, expectedKinds, _, lineNumber) ->
            let document = requireFixtureDocumentByRelativePath workspace fixtureCase.Root relativePath lineNumber

            let expected =
                expectedKinds |> List.map normalizeFixtureText

            let actual =
                document.Syntax.Declarations
                |> List.map declarationKindText
                |> List.map normalizeFixtureText

            Assert.Equal<string list>(expected, actual)
        | AssertEval(target, expectedValueText, filePath, lineNumber) ->
            let bindingTarget, evaluation = evaluateFixtureBinding workspace filePath target

            match evaluation with
            | Result.Ok value ->
                Assert.Equal(expectedValueText, RuntimeValue.format value)
            | Result.Error issue ->
                failwithf
                    "Expected '%s' to evaluate to '%s', but evaluation failed with '%s' (%s:%d)."
                    bindingTarget
                    expectedValueText
                    issue.Message
                    filePath
                    lineNumber
        | AssertEvalErrorContains(target, expectedText, filePath, lineNumber) ->
            let bindingTarget, evaluation = evaluateFixtureBinding workspace filePath target

            match evaluation with
            | Result.Ok value ->
                failwithf
                    "Expected '%s' evaluation to fail with a message containing '%s', but got '%s' (%s:%d)."
                    bindingTarget
                    expectedText
                    (RuntimeValue.format value)
                    filePath
                    lineNumber
            | Result.Error issue ->
                Assert.Contains(expectedText, issue.Message, StringComparison.OrdinalIgnoreCase)
        | AssertExecute(target, expectedValueText, filePath, lineNumber) ->
            let bindingTarget, result, _ = executeFixtureBinding workspace filePath target

            match result with
            | Result.Ok value ->
                Assert.Equal(expectedValueText, RuntimeValue.format value)
            | Result.Error issue ->
                failwithf
                    "Expected '%s' execution to produce '%s', but execution failed with '%s' (%s:%d)."
                    bindingTarget
                    expectedValueText
                    issue.Message
                    filePath
                    lineNumber
        | AssertRunStdout(target, expectedOutputText, filePath, lineNumber) ->
            let bindingTarget, result, actualOutput = executeFixtureBinding workspace filePath target

            match result with
            | Result.Ok _ ->
                Assert.Equal(normalizeExecutionOutput expectedOutputText, actualOutput)
            | Result.Error issue ->
                failwithf
                    "Expected '%s' to run and write '%s', but execution failed with '%s' (%s:%d)."
                    bindingTarget
                    expectedOutputText
                    issue.Message
                    filePath
                    lineNumber
        | AssertStdout(expectedOutputText, _, _) ->
            requireRunMode "assertStdout"

            Assert.Equal(
                normalizeLineEndings expectedOutputText,
                runResult.Value.StandardOutput
            )
        | AssertStdoutContains(expectedOutputText, _, _) ->
            requireRunMode "assertStdoutContains"

            Assert.Contains(
                normalizeLineEndings expectedOutputText,
                runResult.Value.StandardOutput,
                StringComparison.Ordinal
            )
        | AssertStderrContains(expectedOutputText, _, _) ->
            requireRunMode "assertStderrContains"

            Assert.Contains(
                normalizeLineEndings expectedOutputText,
                runResult.Value.StandardError,
                StringComparison.Ordinal
            )
        | AssertExitCode(expectedCode, _, _) ->
            requireRunMode "assertExitCode"
            Assert.Equal(expectedCode, runResult.Value.ExitCode)
        | AssertTraceCount(eventName, subjectName, relation, expectedCount, filePath, lineNumber) ->
            let actualCount =
                Compilation.pipelineTrace workspace
                |> List.filter (fun step ->
                    String.Equals(PipelineTraceEvent.toPortableName step.Event, eventName, StringComparison.Ordinal)
                    && String.Equals(PipelineTraceSubject.toPortableName step.Subject, subjectName, StringComparison.Ordinal))
                |> List.length

            Assert.True(
                compareFixtureRelation relation actualCount expectedCount,
                $"Trace count assertion failed for ({eventName}, {subjectName}) in '{fixtureCase.Name}' ({filePath}:{lineNumber}). Actual count: {actualCount}."
            )
        | AssertModule(expectedModuleText, filePath, lineNumber) ->
            let document = requireFixtureDocument workspace filePath lineNumber

            let actualModuleText =
                document.ModuleName
                |> Option.map SyntaxFacts.moduleNameToText
                |> Option.defaultValue "<none>"

            Assert.Equal(expectedModuleText, actualModuleText)
        | AssertModuleAttributes(expectedAttributes, filePath, lineNumber) ->
            let document = requireFixtureDocument workspace filePath lineNumber

            let expected =
                expectedAttributes
                |> List.map normalizeFixtureText

            let actual =
                document.Syntax.ModuleAttributes
                |> List.map normalizeFixtureText

            Assert.Equal<string list>(expected, actual)
        | AssertDeclarationKinds(expectedKinds, filePath, lineNumber) ->
            let document = requireFixtureDocument workspace filePath lineNumber

            let expected =
                expectedKinds |> List.map normalizeFixtureText

            let actual =
                document.Syntax.Declarations
                |> List.map declarationKindText
                |> List.map normalizeFixtureText

            Assert.Equal<string list>(expected, actual)
        | AssertDeclarationDescriptors(expectedDescriptors, filePath, lineNumber) ->
            let document = requireFixtureDocument workspace filePath lineNumber

            let expected =
                expectedDescriptors |> List.map normalizeFixtureText

            let actual =
                document.Syntax.Declarations
                |> List.map declarationDescriptorText
                |> List.map normalizeFixtureText

            Assert.Equal<string list>(expected, actual)
        | AssertParameterQuantities(bindingName, expectedQuantities, filePath, lineNumber) ->
            let document = requireFixtureDocument workspace filePath lineNumber

            match tryFindLetDefinition bindingName document with
            | Some definition ->
                let expected =
                    expectedQuantities |> List.map normalizeFixtureText

                let actual =
                    definition.Parameters
                    |> List.map (fun parameter ->
                        parameter.Quantity
                        |> Option.map Quantity.toSurfaceText
                        |> Option.defaultValue "<default>")
                    |> List.map normalizeFixtureText

                Assert.Equal<string list>(expected, actual)
            | None ->
                failwithf "Could not find let declaration '%s' (%s:%d)." bindingName filePath lineNumber
        | AssertInoutParameters(bindingName, expectedNames, filePath, lineNumber) ->
            let document = requireFixtureDocument workspace filePath lineNumber

            match tryFindLetDefinition bindingName document with
            | Some definition ->
                let expected =
                    expectedNames |> List.map normalizeFixtureText

                let actual =
                    definition.Parameters
                    |> List.filter (fun parameter -> parameter.IsInout)
                    |> List.map (fun parameter -> parameter.Name)
                    |> List.map normalizeFixtureText

                Assert.Equal<string list>(expected, actual)
            | None ->
                failwithf "Could not find let declaration '%s' (%s:%d)." bindingName filePath lineNumber
        | AssertDoItemDescriptors(bindingName, expectedDescriptors, filePath, lineNumber) ->
            let document = requireFixtureDocument workspace filePath lineNumber

            match tryFindLetDefinition bindingName document with
            | Some { Body = Some(Do statements) } ->
                let expected =
                    expectedDescriptors |> List.map normalizeFixtureText

                let actual =
                    statements
                    |> List.map doItemDescriptor
                    |> List.map normalizeFixtureText

                Assert.Equal<string list>(expected, actual)
            | Some _ ->
                failwithf "Let declaration '%s' does not have a do body (%s:%d)." bindingName filePath lineNumber
            | None ->
                failwithf "Could not find let declaration '%s' (%s:%d)." bindingName filePath lineNumber
        | AssertDataConstructors(typeName, expectedConstructors, filePath, lineNumber) ->
            let document = requireFixtureDocument workspace filePath lineNumber

            match tryFindDataDeclaration typeName document with
            | Some declaration ->
                let expected =
                    expectedConstructors |> List.map normalizeFixtureText

                let actual =
                    declaration.Constructors
                    |> List.map (fun constructor -> constructor.Name)
                    |> List.map normalizeFixtureText

                Assert.Equal<string list>(expected, actual)
            | None ->
                failwithf "Could not find data declaration '%s' (%s:%d)." typeName filePath lineNumber
        | AssertTraitMembers(traitName, expectedMembers, filePath, lineNumber) ->
            let document = requireFixtureDocument workspace filePath lineNumber

            match tryFindTraitDeclaration traitName document with
            | Some declaration ->
                let expected =
                    expectedMembers |> List.map normalizeFixtureText

                let actual =
                    declaration.Members
                    |> List.choose (fun memberDeclaration -> memberDeclaration.Name)
                    |> List.map normalizeFixtureText

                Assert.Equal<string list>(expected, actual)
            | None ->
                failwithf "Could not find trait declaration '%s' (%s:%d)." traitName filePath lineNumber
        | AssertContainsTokenKinds(expectedKinds, filePath, lineNumber) ->
            let lexed = lexFixtureFile filePath

            Assert.Empty(lexed.Diagnostics)

            let actualKinds =
                lexed.Tokens
                |> List.map (fun token -> tokenKindText token.Kind)
                |> List.map normalizeFixtureText

            for expectedKind in expectedKinds do
                let normalizedExpectedKind = normalizeFixtureText expectedKind

                Assert.Contains(
                    normalizedExpectedKind,
                    actualKinds
                )
        | AssertContainsTokenTexts(expectedTexts, filePath, lineNumber) ->
            let lexed = lexFixtureFile filePath

            Assert.Empty(lexed.Diagnostics)

            let actualTexts =
                lexed.Tokens
                |> tokenTexts
                |> List.map normalizeFixtureText

            for expectedText in expectedTexts do
                let normalizedExpectedText = normalizeFixtureText expectedText

                Assert.Contains(
                    normalizedExpectedText,
                    actualTexts
                )
