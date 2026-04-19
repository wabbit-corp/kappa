module Harness

open System
open System.Diagnostics
open System.IO
open System.Runtime.InteropServices
open System.Text
open System.Text.RegularExpressions
open Kappa.Compiler
open Xunit

let createSource (filePath: string) (text: string) =
    SourceText.From(filePath, text.Replace("\r\n", "\n"))

let lexAndParse (filePath: string) (text: string) =
    let source = createSource filePath text
    let lexed = Lexer.tokenize source
    let parsed = Parser.parse source lexed.Tokens
    source, lexed, parsed

let private rootPath (rootName: string) =
    Path.GetFullPath(rootName)

let private rootedFilePath (root: string) (filePath: string) =
    let relativePath =
        filePath.Replace('/', Path.DirectorySeparatorChar)
                .Replace('\\', Path.DirectorySeparatorChar)

    Path.Combine(root, relativePath)

type InMemoryFileSystem(files: (string * string) list) =
    let normalize (path: string) = Path.GetFullPath(path)

    let fileMap =
        files
        |> List.map (fun (path, text) -> normalize path, text.Replace("\r\n", "\n"))
        |> Map.ofList

    let directories =
        let rec allParents (path: string) =
            seq {
                let parent = Path.GetDirectoryName(path)

                if not (String.IsNullOrWhiteSpace(parent)) then
                    yield parent
                    yield! allParents parent
            }

        fileMap.Keys
        |> Seq.collect allParents
        |> Set.ofSeq

    let isUnderRoot (root: string) (candidate: string) =
        let trimmedRoot = root.TrimEnd(Path.DirectorySeparatorChar, Path.AltDirectorySeparatorChar)
        let prefix = trimmedRoot + string Path.DirectorySeparatorChar

        String.Equals(candidate, trimmedRoot, StringComparison.OrdinalIgnoreCase)
        || candidate.StartsWith(prefix, StringComparison.OrdinalIgnoreCase)

    interface IFileSystem with
        member _.GetFullPath(path: string) = normalize path

        member _.FileExists(path: string) =
            fileMap.ContainsKey(normalize path)

        member _.DirectoryExists(path: string) =
            directories.Contains(normalize path)

        member _.EnumerateFiles(path: string, searchPattern: string, searchOption: SearchOption) =
            let root = normalize path

            fileMap.Keys
            |> Seq.filter (fun filePath ->
                let fileName = Path.GetFileName(filePath)

                let matchesPattern =
                    match searchPattern with
                    | "*.kp" -> filePath.EndsWith(".kp", StringComparison.OrdinalIgnoreCase)
                    | _ -> String.Equals(fileName, searchPattern, StringComparison.OrdinalIgnoreCase)

                let matchesScope =
                    match searchOption with
                    | SearchOption.TopDirectoryOnly ->
                        String.Equals(Path.GetDirectoryName(filePath), root, StringComparison.OrdinalIgnoreCase)
                    | SearchOption.AllDirectories ->
                        isUnderRoot root filePath
                    | _ ->
                        false

                matchesPattern && matchesScope)

        member _.ReadAllText(path: string) =
            fileMap[normalize path]

let compileInMemoryWorkspace (rootName: string) (files: (string * string) list) =
    let root = rootPath rootName

    let fileSystem =
        InMemoryFileSystem(
            files
            |> List.map (fun (filePath, text) -> rootedFilePath root filePath, text)
        )

    Compilation.parse (CompilationOptions.createWithFileSystem fileSystem root) [ root ]

let evaluateInMemoryBinding (rootName: string) (entryPoint: string) (files: (string * string) list) =
    let workspace = compileInMemoryWorkspace rootName files
    workspace, Interpreter.evaluateBinding workspace entryPoint

type ProcessResult =
    { ExitCode: int
      StandardOutput: string
      StandardError: string }

let private scratchRoot =
    Path.Combine(Path.GetTempPath(), "kappa-tests")

let createScratchDirectory (name: string) =
    Directory.CreateDirectory(scratchRoot) |> ignore

    let safeName =
        name
        |> Seq.map (fun ch ->
            if Char.IsLetterOrDigit(ch) then ch else '-')
        |> Array.ofSeq
        |> System.String

    let directory =
        Path.Combine(scratchRoot, $"{safeName}-{Guid.NewGuid():N}")

    Directory.CreateDirectory(directory).FullName

let currentRid () =
    let suffix =
        match RuntimeInformation.ProcessArchitecture with
        | Architecture.X64 -> "x64"
        | Architecture.Arm64 -> "arm64"
        | Architecture.X86 -> "x86"
        | architecture -> invalidOp $"Unsupported test architecture '{architecture}'."

    if RuntimeInformation.IsOSPlatform(OSPlatform.Windows) then
        $"win-{suffix}"
    elif RuntimeInformation.IsOSPlatform(OSPlatform.Linux) then
        $"linux-{suffix}"
    elif RuntimeInformation.IsOSPlatform(OSPlatform.OSX) then
        $"osx-{suffix}"
    else
        invalidOp "Unsupported test operating system."

let writeWorkspaceFiles (root: string) (files: (string * string) list) =
    for filePath, text in files do
        let fullPath = rootedFilePath root filePath
        let directory = Path.GetDirectoryName(fullPath)

        if not (String.IsNullOrWhiteSpace(directory)) then
            Directory.CreateDirectory(directory) |> ignore

        File.WriteAllText(fullPath, text.Replace("\r\n", "\n"))

let executablePath (directory: string) (baseName: string) =
    if RuntimeInformation.IsOSPlatform(OSPlatform.Windows) then
        Path.Combine(directory, $"{baseName}.exe")
    else
        Path.Combine(directory, baseName)

let runProcess (workingDirectory: string) (fileName: string) (arguments: string) =
    let startInfo = ProcessStartInfo()
    startInfo.WorkingDirectory <- workingDirectory
    startInfo.FileName <- fileName
    startInfo.Arguments <- arguments
    startInfo.UseShellExecute <- false
    startInfo.RedirectStandardOutput <- true
    startInfo.RedirectStandardError <- true

    use child = new Process()
    child.StartInfo <- startInfo

    if not (child.Start()) then
        invalidOp $"Failed to start process '{fileName}'."

    let standardOutput = child.StandardOutput.ReadToEnd()
    let standardError = child.StandardError.ReadToEnd()
    child.WaitForExit()

    { ExitCode = child.ExitCode
      StandardOutput = standardOutput.Replace("\r\n", "\n")
      StandardError = standardError.Replace("\r\n", "\n") }

type KpFixtureAssertion =
    | AssertNoErrors of filePath: string * lineNumber: int
    | AssertNoWarnings of filePath: string * lineNumber: int
    | AssertErrorCount of expectedCount: int * filePath: string * lineNumber: int
    | AssertWarningCount of expectedCount: int * filePath: string * lineNumber: int
    | AssertDiagnosticMatch of regexPattern: string * filePath: string * lineNumber: int
    | AssertType of target: string * expectedTypeText: string * filePath: string * lineNumber: int
    | AssertEval of target: string * expectedValueText: string * filePath: string * lineNumber: int
    | AssertEvalErrorContains of target: string * expectedText: string * filePath: string * lineNumber: int
    | AssertExecute of target: string * expectedValueText: string * filePath: string * lineNumber: int
    | AssertRunStdout of target: string * expectedOutputText: string * filePath: string * lineNumber: int
    | AssertModule of expectedModuleText: string * filePath: string * lineNumber: int
    | AssertModuleAttributes of expectedAttributes: string list * filePath: string * lineNumber: int
    | AssertDeclarationKinds of expectedKinds: string list * filePath: string * lineNumber: int
    | AssertDeclarationDescriptors of expectedDescriptors: string list * filePath: string * lineNumber: int
    | AssertDataConstructors of typeName: string * expectedConstructors: string list * filePath: string * lineNumber: int
    | AssertTraitMembers of traitName: string * expectedMembers: string list * filePath: string * lineNumber: int
    | AssertContainsTokenKinds of expectedKinds: string list * filePath: string * lineNumber: int
    | AssertContainsTokenTexts of expectedTexts: string list * filePath: string * lineNumber: int

type KpFixtureCase =
    { Name: string
      Root: string
      SourceFiles: string list
      Assertions: KpFixtureAssertion list }

    override this.ToString() = this.Name

let private fixturesRoot =
    Path.Combine(__SOURCE_DIRECTORY__, "Fixtures")

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

let private formatDiagnostics (diagnostics: Diagnostic list) =
    diagnostics
    |> List.map (fun diagnostic ->
        let locationText =
            match diagnostic.Location with
            | Some location ->
                $"{location.FilePath}({location.Start.Line},{location.Start.Column})"
            | None ->
                "<unknown>"

        $"{locationText}: {diagnostic.Message}")
    |> String.concat Environment.NewLine

let private countDiagnosticsBySeverity severity (diagnostics: Diagnostic list) =
    diagnostics |> List.filter (fun diagnostic -> diagnostic.Severity = severity) |> List.length

let private parseFixtureList (value: string) =
    value.Split([| ',' |], StringSplitOptions.RemoveEmptyEntries)
    |> Array.map (fun item -> item.Trim())
    |> Array.filter (String.IsNullOrWhiteSpace >> not)
    |> Array.toList

let private parseNonNegativeInt (directiveName: string) (filePath: string) lineNumber (value: string) =
    match Int32.TryParse(value.Trim()) with
    | true, parsed when parsed >= 0 ->
        parsed
    | _ ->
        invalidOp $"{directiveName} expects a non-negative integer ({filePath}:{lineNumber})."

let private parseTargetAndBody (directiveName: string) (filePath: string) lineNumber (directiveBody: string) =
    let targetAndBody =
        directiveBody.Split([| ' '; '\t' |], 2, StringSplitOptions.RemoveEmptyEntries)

    if targetAndBody.Length <> 2 then
        invalidOp $"{directiveName} expects '<target> <value>' ({filePath}:{lineNumber})."

    targetAndBody[0], targetAndBody[1].Trim()

let private decodeAssertionText (directiveName: string) (filePath: string) lineNumber (value: string) =
    let trimmed = value.Trim()

    if trimmed.StartsWith("\"", StringComparison.Ordinal) then
        match SyntaxFacts.tryDecodeStringLiteral trimmed with
        | Result.Ok decoded -> decoded
        | Result.Error message ->
            invalidOp $"{directiveName} expects valid string literal text ({filePath}:{lineNumber}): {message}"
    else
        trimmed

let private normalizeExecutionOutput (text: string) =
    text.Replace("\r\n", "\n").TrimEnd([| '\r'; '\n' |])

let private executeBindingWithCapturedOutput (workspace: WorkspaceCompilation) (entryPoint: string) =
    let builder = StringBuilder()

    let output: RuntimeOutput =
        { Write = fun text -> builder.Append(text) |> ignore
          WriteLine = fun text -> builder.AppendLine(text) |> ignore }

    let result = Interpreter.executeBindingWithOutput workspace output entryPoint

    result, normalizeExecutionOutput (builder.ToString())

let private parseFixtureAssertion (filePath: string) lineNumber (lineText: string) =
    let trimmed = lineText.Trim()

    if trimmed.StartsWith("--!", StringComparison.Ordinal) then
        let body = trimmed.Substring(3).Trim()

        if String.IsNullOrWhiteSpace(body) then
            invalidOp $"Fixture assertion at {filePath}:{lineNumber} is empty."

        let firstSpace = body.IndexOf(' ')
        let directiveName, directiveBody =
            if firstSpace < 0 then
                body, ""
            else
                body.Substring(0, firstSpace), body.Substring(firstSpace + 1).Trim()

        match directiveName with
        | "assertNoErrors" ->
            if not (String.IsNullOrWhiteSpace(directiveBody)) then
                invalidOp $"assertNoErrors does not take arguments ({filePath}:{lineNumber})."

            Some(AssertNoErrors(filePath, lineNumber))
        | "assertNoWarnings" ->
            if not (String.IsNullOrWhiteSpace(directiveBody)) then
                invalidOp $"assertNoWarnings does not take arguments ({filePath}:{lineNumber})."

            Some(AssertNoWarnings(filePath, lineNumber))
        | "assertErrorCount" ->
            Some(AssertErrorCount(parseNonNegativeInt directiveName filePath lineNumber directiveBody, filePath, lineNumber))
        | "assertWarningCount" ->
            Some(AssertWarningCount(parseNonNegativeInt directiveName filePath lineNumber directiveBody, filePath, lineNumber))
        | "assertDiagnosticMatch" ->
            if String.IsNullOrWhiteSpace(directiveBody) then
                invalidOp $"assertDiagnosticMatch expects a regular expression ({filePath}:{lineNumber})."

            Some(AssertDiagnosticMatch(directiveBody, filePath, lineNumber))
        | "assertType" ->
            let targetAndType =
                directiveBody.Split([| ' '; '\t' |], 2, StringSplitOptions.RemoveEmptyEntries)

            if targetAndType.Length <> 2 then
                invalidOp $"assertType expects '<target> <type>' ({filePath}:{lineNumber})."

            Some(AssertType(targetAndType[0], targetAndType[1].Trim(), filePath, lineNumber))
        | "assertEval"
        | "x-assertEval" ->
            let target, expectedValueText = parseTargetAndBody "assertEval" filePath lineNumber directiveBody
            Some(AssertEval(target, expectedValueText, filePath, lineNumber))
        | "assertEvalErrorContains"
        | "x-assertEvalErrorContains" ->
            let target, expectedText = parseTargetAndBody "assertEvalErrorContains" filePath lineNumber directiveBody
            Some(AssertEvalErrorContains(target, expectedText, filePath, lineNumber))
        | "assertExecute" ->
            let target, expectedValueText = parseTargetAndBody "assertExecute" filePath lineNumber directiveBody
            Some(AssertExecute(target, expectedValueText, filePath, lineNumber))
        | "assertRunStdout" ->
            let target, expectedOutputText = parseTargetAndBody "assertRunStdout" filePath lineNumber directiveBody

            Some(
                AssertRunStdout(
                    target,
                    decodeAssertionText "assertRunStdout" filePath lineNumber expectedOutputText,
                    filePath,
                    lineNumber
                )
            )
        | "assertModule"
        | "x-assertModule" ->
            if String.IsNullOrWhiteSpace(directiveBody) then
                invalidOp $"assertModule expects '<module>' ({filePath}:{lineNumber})."

            Some(AssertModule(directiveBody, filePath, lineNumber))
        | "assertModuleAttributes"
        | "x-assertModuleAttributes" ->
            let expectedAttributes = parseFixtureList directiveBody

            if List.isEmpty expectedAttributes then
                invalidOp $"assertModuleAttributes expects a comma-separated list of attributes ({filePath}:{lineNumber})."

            Some(AssertModuleAttributes(expectedAttributes, filePath, lineNumber))
        | "assertDeclKinds" ->
            let expectedKinds = parseFixtureList directiveBody

            if List.isEmpty expectedKinds then
                invalidOp $"assertDeclKinds expects a comma-separated list of declaration kinds ({filePath}:{lineNumber})."

            Some(AssertDeclarationKinds(expectedKinds, filePath, lineNumber))
        | "assertDeclDescriptors"
        | "x-assertDeclDescriptors" ->
            let expectedDescriptors = parseFixtureList directiveBody

            if List.isEmpty expectedDescriptors then
                invalidOp $"assertDeclDescriptors expects a comma-separated list of declaration descriptors ({filePath}:{lineNumber})."

            Some(AssertDeclarationDescriptors(expectedDescriptors, filePath, lineNumber))
        | "assertDataConstructors"
        | "x-assertDataConstructors" ->
            let typeAndConstructors =
                directiveBody.Split([| ' '; '\t' |], 2, StringSplitOptions.RemoveEmptyEntries)

            if typeAndConstructors.Length <> 2 then
                invalidOp $"assertDataConstructors expects '<type> <ctor1, ctor2, ...>' ({filePath}:{lineNumber})."

            let expectedConstructors = parseFixtureList typeAndConstructors[1]

            if List.isEmpty expectedConstructors then
                invalidOp $"assertDataConstructors expects at least one constructor ({filePath}:{lineNumber})."

            Some(AssertDataConstructors(typeAndConstructors[0], expectedConstructors, filePath, lineNumber))
        | "assertTraitMembers"
        | "x-assertTraitMembers" ->
            let traitAndMembers =
                directiveBody.Split([| ' '; '\t' |], 2, StringSplitOptions.RemoveEmptyEntries)

            if traitAndMembers.Length <> 2 then
                invalidOp $"assertTraitMembers expects '<trait> <member1, member2, ...>' ({filePath}:{lineNumber})."

            let expectedMembers = parseFixtureList traitAndMembers[1]

            if List.isEmpty expectedMembers then
                invalidOp $"assertTraitMembers expects at least one member ({filePath}:{lineNumber})."

            Some(AssertTraitMembers(traitAndMembers[0], expectedMembers, filePath, lineNumber))
        | "assertContainsTokenKinds"
        | "x-assertContainsTokenKinds" ->
            let expectedKinds = parseFixtureList directiveBody

            if List.isEmpty expectedKinds then
                invalidOp $"assertContainsTokenKinds expects a comma-separated list of token kinds ({filePath}:{lineNumber})."

            Some(AssertContainsTokenKinds(expectedKinds, filePath, lineNumber))
        | "assertContainsTokenTexts"
        | "x-assertContainsTokenTexts" ->
            let expectedTexts = parseFixtureList directiveBody

            if List.isEmpty expectedTexts then
                invalidOp $"assertContainsTokenTexts expects a comma-separated list of token texts ({filePath}:{lineNumber})."

            Some(AssertContainsTokenTexts(expectedTexts, filePath, lineNumber))
        | other ->
            invalidOp $"Unsupported fixture assertion '{other}' at {filePath}:{lineNumber}."
    else
        None

let private loadFixtureAssertions (filePath: string) =
    File.ReadAllLines(filePath)
    |> Array.mapi (fun index lineText -> parseFixtureAssertion filePath (index + 1) lineText)
    |> Array.choose id
    |> Array.toList

let discoverKpFixtureCases () =
    if not (Directory.Exists(fixturesRoot)) then
        []
    else
        Directory.EnumerateDirectories(fixturesRoot)
        |> Seq.sort
        |> Seq.choose (fun caseDirectory ->
            let sourceFiles =
                Directory.EnumerateFiles(caseDirectory, "*.kp", SearchOption.AllDirectories)
                |> Seq.sort
                |> Seq.toList

            if List.isEmpty sourceFiles then
                None
            else
                let assertions =
                    sourceFiles
                    |> List.collect loadFixtureAssertions

                Some
                    { Name = Path.GetFileName(caseDirectory)
                      Root = caseDirectory
                      SourceFiles = sourceFiles
                      Assertions = assertions })
        |> Seq.toList

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

let runKpFixtureCase (fixtureCase: KpFixtureCase) =
    Assert.NotEmpty(fixtureCase.SourceFiles)
    Assert.NotEmpty(fixtureCase.Assertions)

    let workspace =
        Compilation.parse (CompilationOptions.create fixtureCase.Root) [ fixtureCase.Root ]

    let expectsDiagnostics =
        fixtureCase.Assertions
        |> List.exists (function
            | AssertErrorCount(expectedCount, _, _) when expectedCount > 0 -> true
            | AssertWarningCount(expectedCount, _, _) when expectedCount > 0 -> true
            | AssertDiagnosticMatch _ -> true
            | _ -> false)

    if not expectsDiagnostics then
        Assert.False(
            workspace.HasErrors,
            $"Fixture '{fixtureCase.Name}' failed to compile:{Environment.NewLine}{formatDiagnostics workspace.Diagnostics}"
        )

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
        | AssertEval(target, expectedValueText, filePath, lineNumber) ->
            let bindingTarget = qualifyFixtureBindingTarget workspace filePath target

            match Interpreter.evaluateBinding workspace bindingTarget with
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
            let bindingTarget = qualifyFixtureBindingTarget workspace filePath target

            match Interpreter.evaluateBinding workspace bindingTarget with
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
            let bindingTarget = qualifyFixtureBindingTarget workspace filePath target
            let result, _ = executeBindingWithCapturedOutput workspace bindingTarget

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
            let bindingTarget = qualifyFixtureBindingTarget workspace filePath target
            let result, actualOutput = executeBindingWithCapturedOutput workspace bindingTarget

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
        | AssertModule(expectedModuleText, filePath, lineNumber) ->
            match tryFindDocumentForFilePath workspace filePath with
            | Some document ->
                let actualModuleText =
                    document.ModuleName
                    |> Option.map SyntaxFacts.moduleNameToText
                    |> Option.defaultValue "<none>"

                Assert.Equal(expectedModuleText, actualModuleText)
            | None ->
                failwithf "Could not find parsed document for '%s' (%s:%d)." filePath filePath lineNumber
        | AssertModuleAttributes(expectedAttributes, filePath, lineNumber) ->
            match tryFindDocumentForFilePath workspace filePath with
            | Some document ->
                let expected =
                    expectedAttributes
                    |> List.map normalizeFixtureText

                let actual =
                    document.Syntax.ModuleAttributes
                    |> List.map normalizeFixtureText

                Assert.Equal<string list>(expected, actual)
            | None ->
                failwithf "Could not find parsed document for '%s' (%s:%d)." filePath filePath lineNumber
        | AssertDeclarationKinds(expectedKinds, filePath, lineNumber) ->
            match tryFindDocumentForFilePath workspace filePath with
            | Some document ->
                let expected =
                    expectedKinds |> List.map normalizeFixtureText

                let actual =
                    document.Syntax.Declarations
                    |> List.map declarationKindText
                    |> List.map normalizeFixtureText

                Assert.Equal<string list>(expected, actual)
            | None ->
                failwithf "Could not find parsed document for '%s' (%s:%d)." filePath filePath lineNumber
        | AssertDeclarationDescriptors(expectedDescriptors, filePath, lineNumber) ->
            match tryFindDocumentForFilePath workspace filePath with
            | Some document ->
                let expected =
                    expectedDescriptors |> List.map normalizeFixtureText

                let actual =
                    document.Syntax.Declarations
                    |> List.map declarationDescriptorText
                    |> List.map normalizeFixtureText

                Assert.Equal<string list>(expected, actual)
            | None ->
                failwithf "Could not find parsed document for '%s' (%s:%d)." filePath filePath lineNumber
        | AssertDataConstructors(typeName, expectedConstructors, filePath, lineNumber) ->
            match tryFindDocumentForFilePath workspace filePath with
            | Some document ->
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
            | None ->
                failwithf "Could not find parsed document for '%s' (%s:%d)." filePath filePath lineNumber
        | AssertTraitMembers(traitName, expectedMembers, filePath, lineNumber) ->
            match tryFindDocumentForFilePath workspace filePath with
            | Some document ->
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
            | None ->
                failwithf "Could not find parsed document for '%s' (%s:%d)." filePath filePath lineNumber
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
