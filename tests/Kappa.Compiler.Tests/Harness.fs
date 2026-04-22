module Harness

open System
open System.IO
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

let private rootPath = HarnessSupport.rootPath
let private rootedFilePath = HarnessSupport.rootedFilePath

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

let compileInMemoryWorkspaceWithBackend (rootName: string) (backendProfile: string) (files: (string * string) list) =
    let root = rootPath rootName

    let fileSystem =
        InMemoryFileSystem(
            files
            |> List.map (fun (filePath, text) -> rootedFilePath root filePath, text)
        )

    let options =
        { CompilationOptions.createWithFileSystem fileSystem root with
            BackendProfile = backendProfile }

    Compilation.parse options [ root ]

let evaluateInMemoryBinding (rootName: string) (entryPoint: string) (files: (string * string) list) =
    let workspace = compileInMemoryWorkspace rootName files
    workspace, Interpreter.evaluateBinding workspace entryPoint

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

type KpFixtureMode =
    | Analyze
    | Check
    | Compile
    | Run

type KpFixtureDirectiveSource =
    | KpSourceFile
    | SuiteDirectiveFile

type KpFixtureRelation =
    | Equal
    | NotEqual
    | LessThan
    | LessThanOrEqual
    | GreaterThan
    | GreaterThanOrEqual

type KpFixtureConfiguration =
    { Mode: KpFixtureMode
      PackageMode: bool
      BackendProfile: string
      EntryPoint: string option
      RunArgs: string list
      StdinFile: string option
      DumpFormat: StageDumpFormat }

module KpFixtureConfiguration =
    let defaultValue =
        { Mode = KpFixtureMode.Check
          PackageMode = true
          BackendProfile = "interpreter"
          EntryPoint = None
          RunArgs = []
          StdinFile = None
          DumpFormat = StageDumpFormat.Json }

type KpFixtureAssertion =
    | AssertNoErrors of filePath: string * lineNumber: int
    | AssertNoWarnings of filePath: string * lineNumber: int
    | AssertErrorCount of expectedCount: int * filePath: string * lineNumber: int
    | AssertWarningCount of expectedCount: int * filePath: string * lineNumber: int
    | AssertDiagnosticCodes of expectedCodes: string list * filePath: string * lineNumber: int
    | AssertDiagnosticAt of relativePath: string * severity: DiagnosticSeverity * code: string * expectedLine: int * expectedColumn: int option * filePath: string * lineNumber: int
    | AssertDiagnosticMatch of regexPattern: string * filePath: string * lineNumber: int
    | AssertType of target: string * expectedTypeText: string * filePath: string * lineNumber: int
    | AssertFileDeclarationKinds of relativePath: string * expectedKinds: string list * filePath: string * lineNumber: int
    | AssertEval of target: string * expectedValueText: string * filePath: string * lineNumber: int
    | AssertEvalErrorContains of target: string * expectedText: string * filePath: string * lineNumber: int
    | AssertExecute of target: string * expectedValueText: string * filePath: string * lineNumber: int
    | AssertRunStdout of target: string * expectedOutputText: string * filePath: string * lineNumber: int
    | AssertStdout of expectedOutputText: string * filePath: string * lineNumber: int
    | AssertStdoutContains of expectedOutputText: string * filePath: string * lineNumber: int
    | AssertStderrContains of expectedOutputText: string * filePath: string * lineNumber: int
    | AssertExitCode of expectedCode: int * filePath: string * lineNumber: int
    | AssertTraceCount of eventName: string * subjectName: string * relation: KpFixtureRelation * expectedCount: int * filePath: string * lineNumber: int
    | AssertModule of expectedModuleText: string * filePath: string * lineNumber: int
    | AssertModuleAttributes of expectedAttributes: string list * filePath: string * lineNumber: int
    | AssertDeclarationKinds of expectedKinds: string list * filePath: string * lineNumber: int
    | AssertDeclarationDescriptors of expectedDescriptors: string list * filePath: string * lineNumber: int
    | AssertParameterQuantities of bindingName: string * expectedQuantities: string list * filePath: string * lineNumber: int
    | AssertInoutParameters of bindingName: string * expectedNames: string list * filePath: string * lineNumber: int
    | AssertDoItemDescriptors of bindingName: string * expectedDescriptors: string list * filePath: string * lineNumber: int
    | AssertDataConstructors of typeName: string * expectedConstructors: string list * filePath: string * lineNumber: int
    | AssertTraitMembers of traitName: string * expectedMembers: string list * filePath: string * lineNumber: int
    | AssertContainsTokenKinds of expectedKinds: string list * filePath: string * lineNumber: int
    | AssertContainsTokenTexts of expectedTexts: string list * filePath: string * lineNumber: int

type KpFixtureCase =
    { Name: string
      Root: string
      SourceFiles: string list
      Configuration: KpFixtureConfiguration
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

let private normalizeLineEndings (text: string) =
    text.Replace("\r\n", "\n")

let private normalizeExecutionOutput (text: string) =
    normalizeLineEndings text
    |> fun normalized -> normalized.TrimEnd([| '\r'; '\n' |])

let private parseFixtureMode (filePath: string) lineNumber (value: string) =
    match value.Trim() with
    | "analyze" -> KpFixtureMode.Analyze
    | "check" -> KpFixtureMode.Check
    | "compile" -> KpFixtureMode.Compile
    | "run" -> KpFixtureMode.Run
    | other ->
        invalidOp $"Unsupported fixture mode '{other}' ({filePath}:{lineNumber})."

let private parseFixtureDumpFormat (filePath: string) lineNumber (value: string) =
    match value.Trim() with
    | "json" -> StageDumpFormat.Json
    | "sexpr" -> StageDumpFormat.SExpression
    | other ->
        invalidOp $"Unsupported fixture dump format '{other}' ({filePath}:{lineNumber})."

let private parseFixtureDiagnosticSeverity (filePath: string) lineNumber (value: string) =
    match value.Trim().ToLowerInvariant() with
    | "info" -> DiagnosticSeverity.Info
    | "warning" -> DiagnosticSeverity.Warning
    | "error" -> DiagnosticSeverity.Error
    | other ->
        invalidOp $"Unsupported diagnostic severity '{other}' ({filePath}:{lineNumber})."

let private parseFixtureRelation (filePath: string) lineNumber (value: string) =
    match value.Trim() with
    | "=" -> KpFixtureRelation.Equal
    | "!=" -> KpFixtureRelation.NotEqual
    | "<" -> KpFixtureRelation.LessThan
    | "<=" -> KpFixtureRelation.LessThanOrEqual
    | ">" -> KpFixtureRelation.GreaterThan
    | ">=" -> KpFixtureRelation.GreaterThanOrEqual
    | other ->
        invalidOp $"Unsupported fixture relation '{other}' ({filePath}:{lineNumber})."

let private parseDirectiveHeader (filePath: string) lineNumber (lineText: string) =
    let trimmed = lineText.Trim()

    if trimmed.StartsWith("--!", StringComparison.Ordinal) then
        let body = trimmed.Substring(3).Trim()

        if String.IsNullOrWhiteSpace(body) then
            invalidOp $"Fixture directive at {filePath}:{lineNumber} is empty."

        let firstSpace = body.IndexOf(' ')

        let directiveName, directiveBody =
            if firstSpace < 0 then
                body, ""
            else
                body.Substring(0, firstSpace), body.Substring(firstSpace + 1).Trim()

        Some(directiveName, directiveBody)
    else
        None

let private parseStringLiteralArguments (directiveName: string) (filePath: string) lineNumber (directiveBody: string) =
    let values = ResizeArray<string>()
    let mutable index = 0

    let skipWhitespace () =
        while index < directiveBody.Length && Char.IsWhiteSpace(directiveBody[index]) do
            index <- index + 1

    skipWhitespace ()

    while index < directiveBody.Length do
        if directiveBody[index] <> '"' then
            invalidOp $"{directiveName} expects one or more string literals ({filePath}:{lineNumber})."

        let literalStart = index
        index <- index + 1

        let mutable escaped = false
        let mutable closed = false

        while index < directiveBody.Length && not closed do
            let current = directiveBody[index]

            if escaped then
                escaped <- false
                index <- index + 1
            elif current = '\\' then
                escaped <- true
                index <- index + 1
            elif current = '"' then
                closed <- true
                index <- index + 1
            else
                index <- index + 1

        if not closed then
            invalidOp $"{directiveName} contains an unterminated string literal ({filePath}:{lineNumber})."

        let literalText = directiveBody.Substring(literalStart, index - literalStart)

        match SyntaxFacts.tryDecodeStringLiteral literalText with
        | Result.Ok value ->
            values.Add(value)
        | Result.Error message ->
            invalidOp $"{directiveName} expects valid string literal text ({filePath}:{lineNumber}): {message}"

        skipWhitespace ()

    List.ofSeq values

let private parseSingleStringLiteralArgument (directiveName: string) (filePath: string) lineNumber (directiveBody: string) =
    match parseStringLiteralArguments directiveName filePath lineNumber directiveBody with
    | [ value ] -> value
    | [] ->
        invalidOp $"{directiveName} expects a string literal ({filePath}:{lineNumber})."
    | _ ->
        invalidOp $"{directiveName} expects exactly one string literal ({filePath}:{lineNumber})."

type private KpFixtureDirective =
    | SetMode of KpFixtureMode * filePath: string * lineNumber: int
    | SetPackageMode of packageMode: bool * filePath: string * lineNumber: int
    | SetBackend of backendProfile: string * filePath: string * lineNumber: int
    | SetEntry of entryPoint: string * filePath: string * lineNumber: int
    | SetRunArgs of runArgs: string list * filePath: string * lineNumber: int
    | SetStdinFile of relativePath: string * filePath: string * lineNumber: int
    | SetDumpFormat of dumpFormat: StageDumpFormat * filePath: string * lineNumber: int
    | AssertionDirective of KpFixtureAssertion

let private legacyDirectiveAliases =
    Map.ofList
        [
            "x-assertEval", "assertEval"
            "x-assertEvalErrorContains", "assertEvalErrorContains"
            "x-assertModule", "assertModule"
            "x-assertModuleAttributes", "assertModuleAttributes"
            "x-assertDeclDescriptors", "assertDeclDescriptors"
            "x-assertDataConstructors", "assertDataConstructors"
            "x-assertTraitMembers", "assertTraitMembers"
            "x-assertContainsTokenKinds", "assertContainsTokenKinds"
            "x-assertContainsTokenTexts", "assertContainsTokenTexts"
        ]

let private canonicalizeDirectiveName directiveName =
    legacyDirectiveAliases
    |> Map.tryFind directiveName
    |> Option.defaultValue directiveName

let private executeBindingWithCapturedOutput (workspace: WorkspaceCompilation) (entryPoint: string) =
    let builder = StringBuilder()

    let output: RuntimeOutput =
        { Write = fun text -> builder.Append(text) |> ignore
          WriteLine = fun text -> builder.AppendLine(text) |> ignore }

    let result = Interpreter.executeBindingWithOutput workspace output entryPoint

    result, normalizeExecutionOutput (builder.ToString())

let private parseFixtureDirective (sourceKind: KpFixtureDirectiveSource) (filePath: string) lineNumber (lineText: string) =
    match parseDirectiveHeader filePath lineNumber lineText with
    | None ->
        None
    | Some(rawDirectiveName, directiveBody) ->
        let directiveName = canonicalizeDirectiveName rawDirectiveName

        let ensureNoArguments () =
            if not (String.IsNullOrWhiteSpace(directiveBody)) then
                invalidOp $"{directiveName} does not take arguments ({filePath}:{lineNumber})."

        let ensureSourceFileDirective () =
            if sourceKind <> KpFixtureDirectiveSource.KpSourceFile then
                invalidOp $"{directiveName} is only valid in .kp source files ({filePath}:{lineNumber})."

        let parseRelativePathAndList () =
            let tokens =
                directiveBody.Split([| ' '; '\t' |], 2, StringSplitOptions.RemoveEmptyEntries)

            if tokens.Length <> 2 then
                invalidOp $"{directiveName} expects '<path> <value>' ({filePath}:{lineNumber})."

            let expectedKinds = parseFixtureList tokens[1]

            if List.isEmpty expectedKinds then
                invalidOp $"{directiveName} expects a comma-separated list ({filePath}:{lineNumber})."

            tokens[0], expectedKinds

        let parseTargetAndList directiveDescription =
            let tokens =
                directiveBody.Split([| ' '; '\t' |], 2, StringSplitOptions.RemoveEmptyEntries)

            if tokens.Length <> 2 then
                invalidOp $"{directiveName} expects '<{directiveDescription}> <value-list>' ({filePath}:{lineNumber})."

            let expectedItems = parseFixtureList tokens[1]

            if List.isEmpty expectedItems then
                invalidOp $"{directiveName} expects a comma-separated list ({filePath}:{lineNumber})."

            tokens[0], expectedItems

        match directiveName with
        | "mode" ->
            if String.IsNullOrWhiteSpace(directiveBody) then
                invalidOp $"mode expects one of analyze/check/compile/run ({filePath}:{lineNumber})."

            Some(SetMode(parseFixtureMode filePath lineNumber directiveBody, filePath, lineNumber))
        | "packageMode" ->
            ensureNoArguments ()
            Some(SetPackageMode(true, filePath, lineNumber))
        | "scriptMode" ->
            ensureNoArguments ()
            Some(SetPackageMode(false, filePath, lineNumber))
        | "backend" ->
            if String.IsNullOrWhiteSpace(directiveBody) then
                invalidOp $"backend expects a backend profile name ({filePath}:{lineNumber})."

            Some(SetBackend(directiveBody.Trim(), filePath, lineNumber))
        | "entry" ->
            if String.IsNullOrWhiteSpace(directiveBody) then
                invalidOp $"entry expects a qualified binding name ({filePath}:{lineNumber})."

            Some(SetEntry(directiveBody.Trim(), filePath, lineNumber))
        | "runArgs" ->
            Some(SetRunArgs(parseStringLiteralArguments directiveName filePath lineNumber directiveBody, filePath, lineNumber))
        | "stdinFile" ->
            if String.IsNullOrWhiteSpace(directiveBody) then
                invalidOp $"stdinFile expects a relative path ({filePath}:{lineNumber})."

            Some(SetStdinFile(directiveBody.Trim(), filePath, lineNumber))
        | "dumpFormat" ->
            if String.IsNullOrWhiteSpace(directiveBody) then
                invalidOp $"dumpFormat expects 'json' or 'sexpr' ({filePath}:{lineNumber})."

            Some(SetDumpFormat(parseFixtureDumpFormat filePath lineNumber directiveBody, filePath, lineNumber))
        | "assertNoErrors" ->
            ensureNoArguments ()
            Some(AssertionDirective(AssertNoErrors(filePath, lineNumber)))
        | "assertNoWarnings" ->
            ensureNoArguments ()
            Some(AssertionDirective(AssertNoWarnings(filePath, lineNumber)))
        | "assertErrorCount" ->
            Some(AssertionDirective(AssertErrorCount(parseNonNegativeInt directiveName filePath lineNumber directiveBody, filePath, lineNumber)))
        | "assertWarningCount" ->
            Some(AssertionDirective(AssertWarningCount(parseNonNegativeInt directiveName filePath lineNumber directiveBody, filePath, lineNumber)))
        | "assertDiagnosticMatch" ->
            if String.IsNullOrWhiteSpace(directiveBody) then
                invalidOp $"assertDiagnosticMatch expects a regular expression ({filePath}:{lineNumber})."

            Some(AssertionDirective(AssertDiagnosticMatch(directiveBody, filePath, lineNumber)))
        | "assertDiagnosticCodes" ->
            let expectedCodes = parseFixtureList directiveBody

            if List.isEmpty expectedCodes then
                invalidOp $"assertDiagnosticCodes expects a comma-separated list of diagnostic codes ({filePath}:{lineNumber})."

            Some(AssertionDirective(AssertDiagnosticCodes(expectedCodes, filePath, lineNumber)))
        | "assertDiagnosticAt" ->
            let tokens =
                directiveBody.Split([| ' '; '\t' |], StringSplitOptions.RemoveEmptyEntries)

            if tokens.Length <> 4 && tokens.Length <> 5 then
                invalidOp $"assertDiagnosticAt expects '<path> <severity> <code> <line> [column]' ({filePath}:{lineNumber})."

            let expectedLine = parseNonNegativeInt directiveName filePath lineNumber tokens[3]

            if expectedLine = 0 then
                invalidOp $"assertDiagnosticAt expects a 1-based line number ({filePath}:{lineNumber})."

            let expectedColumn =
                if tokens.Length = 5 then
                    let parsedColumn = parseNonNegativeInt directiveName filePath lineNumber tokens[4]

                    if parsedColumn = 0 then
                        invalidOp $"assertDiagnosticAt expects a 1-based column number ({filePath}:{lineNumber})."

                    Some parsedColumn
                else
                    None

            Some(
                AssertionDirective(
                    AssertDiagnosticAt(
                        tokens[0],
                        parseFixtureDiagnosticSeverity filePath lineNumber tokens[1],
                        tokens[2],
                        expectedLine,
                        expectedColumn,
                        filePath,
                        lineNumber
                    )
                )
            )
        | "assertType" ->
            let targetAndType =
                directiveBody.Split([| ' '; '\t' |], 2, StringSplitOptions.RemoveEmptyEntries)

            if targetAndType.Length <> 2 then
                invalidOp $"assertType expects '<target> <type>' ({filePath}:{lineNumber})."

            Some(AssertionDirective(AssertType(targetAndType[0], targetAndType[1].Trim(), filePath, lineNumber)))
        | "assertFileDeclKinds" ->
            let relativePath, expectedKinds = parseRelativePathAndList ()
            Some(AssertionDirective(AssertFileDeclarationKinds(relativePath, expectedKinds, filePath, lineNumber)))
        | "assertEval" ->
            let target, expectedValueText = parseTargetAndBody "assertEval" filePath lineNumber directiveBody
            Some(AssertionDirective(AssertEval(target, expectedValueText, filePath, lineNumber)))
        | "assertEvalErrorContains" ->
            let target, expectedText = parseTargetAndBody "assertEvalErrorContains" filePath lineNumber directiveBody
            Some(AssertionDirective(AssertEvalErrorContains(target, expectedText, filePath, lineNumber)))
        | "assertExecute" ->
            let target, expectedValueText = parseTargetAndBody "assertExecute" filePath lineNumber directiveBody
            Some(AssertionDirective(AssertExecute(target, expectedValueText, filePath, lineNumber)))
        | "assertRunStdout" ->
            let target, expectedOutputText = parseTargetAndBody "assertRunStdout" filePath lineNumber directiveBody

            Some(
                AssertionDirective(
                    AssertRunStdout(
                        target,
                        decodeAssertionText "assertRunStdout" filePath lineNumber expectedOutputText,
                        filePath,
                        lineNumber
                    )
                )
            )
        | "assertStdout" ->
            Some(
                AssertionDirective(
                    AssertStdout(
                        parseSingleStringLiteralArgument directiveName filePath lineNumber directiveBody,
                        filePath,
                        lineNumber
                    )
                )
            )
        | "assertStdoutContains" ->
            Some(
                AssertionDirective(
                    AssertStdoutContains(
                        parseSingleStringLiteralArgument directiveName filePath lineNumber directiveBody,
                        filePath,
                        lineNumber
                    )
                )
            )
        | "assertStderrContains" ->
            Some(
                AssertionDirective(
                    AssertStderrContains(
                        parseSingleStringLiteralArgument directiveName filePath lineNumber directiveBody,
                        filePath,
                        lineNumber
                    )
                )
            )
        | "assertExitCode" ->
            Some(AssertionDirective(AssertExitCode(parseNonNegativeInt directiveName filePath lineNumber directiveBody, filePath, lineNumber)))
        | "assertTraceCount" ->
            let tokens =
                directiveBody.Split([| ' '; '\t' |], StringSplitOptions.RemoveEmptyEntries)

            if tokens.Length <> 4 then
                invalidOp $"{directiveName} expects '<event> <subject> <relop> <n>' ({filePath}:{lineNumber})."

            Some(
                AssertionDirective(
                    AssertTraceCount(
                        tokens[0],
                        tokens[1],
                        parseFixtureRelation filePath lineNumber tokens[2],
                        parseNonNegativeInt directiveName filePath lineNumber tokens[3],
                        filePath,
                        lineNumber
                    )
                )
            )
        | "assertModule" ->
            if String.IsNullOrWhiteSpace(directiveBody) then
                invalidOp $"assertModule expects '<module>' ({filePath}:{lineNumber})."

            Some(AssertionDirective(AssertModule(directiveBody, filePath, lineNumber)))
        | "assertModuleAttributes" ->
            let expectedAttributes = parseFixtureList directiveBody

            if List.isEmpty expectedAttributes then
                invalidOp $"assertModuleAttributes expects a comma-separated list of attributes ({filePath}:{lineNumber})."

            Some(AssertionDirective(AssertModuleAttributes(expectedAttributes, filePath, lineNumber)))
        | "assertDeclKinds" ->
            ensureSourceFileDirective ()

            let expectedKinds = parseFixtureList directiveBody

            if List.isEmpty expectedKinds then
                invalidOp $"assertDeclKinds expects a comma-separated list of declaration kinds ({filePath}:{lineNumber})."

            Some(AssertionDirective(AssertDeclarationKinds(expectedKinds, filePath, lineNumber)))
        | "assertDeclDescriptors" ->
            ensureSourceFileDirective ()

            let expectedDescriptors = parseFixtureList directiveBody

            if List.isEmpty expectedDescriptors then
                invalidOp $"assertDeclDescriptors expects a comma-separated list of declaration descriptors ({filePath}:{lineNumber})."

            Some(AssertionDirective(AssertDeclarationDescriptors(expectedDescriptors, filePath, lineNumber)))
        | "assertParameterQuantities" ->
            ensureSourceFileDirective ()
            let bindingName, expectedQuantities = parseTargetAndList "binding"
            Some(AssertionDirective(AssertParameterQuantities(bindingName, expectedQuantities, filePath, lineNumber)))
        | "assertInoutParameters" ->
            ensureSourceFileDirective ()
            let bindingName, expectedNames = parseTargetAndList "binding"
            Some(AssertionDirective(AssertInoutParameters(bindingName, expectedNames, filePath, lineNumber)))
        | "assertDoItemDescriptors" ->
            ensureSourceFileDirective ()
            let bindingName, expectedDescriptors = parseTargetAndList "binding"
            Some(AssertionDirective(AssertDoItemDescriptors(bindingName, expectedDescriptors, filePath, lineNumber)))
        | "assertDataConstructors" ->
            ensureSourceFileDirective ()

            let typeAndConstructors =
                directiveBody.Split([| ' '; '\t' |], 2, StringSplitOptions.RemoveEmptyEntries)

            if typeAndConstructors.Length <> 2 then
                invalidOp $"assertDataConstructors expects '<type> <ctor1, ctor2, ...>' ({filePath}:{lineNumber})."

            let expectedConstructors = parseFixtureList typeAndConstructors[1]

            if List.isEmpty expectedConstructors then
                invalidOp $"assertDataConstructors expects at least one constructor ({filePath}:{lineNumber})."

            Some(AssertionDirective(AssertDataConstructors(typeAndConstructors[0], expectedConstructors, filePath, lineNumber)))
        | "assertTraitMembers" ->
            ensureSourceFileDirective ()

            let traitAndMembers =
                directiveBody.Split([| ' '; '\t' |], 2, StringSplitOptions.RemoveEmptyEntries)

            if traitAndMembers.Length <> 2 then
                invalidOp $"assertTraitMembers expects '<trait> <member1, member2, ...>' ({filePath}:{lineNumber})."

            let expectedMembers = parseFixtureList traitAndMembers[1]

            if List.isEmpty expectedMembers then
                invalidOp $"assertTraitMembers expects at least one member ({filePath}:{lineNumber})."

            Some(AssertionDirective(AssertTraitMembers(traitAndMembers[0], expectedMembers, filePath, lineNumber)))
        | "assertContainsTokenKinds" ->
            ensureSourceFileDirective ()

            let expectedKinds = parseFixtureList directiveBody

            if List.isEmpty expectedKinds then
                invalidOp $"assertContainsTokenKinds expects a comma-separated list of token kinds ({filePath}:{lineNumber})."

            Some(AssertionDirective(AssertContainsTokenKinds(expectedKinds, filePath, lineNumber)))
        | "assertContainsTokenTexts" ->
            ensureSourceFileDirective ()

            let expectedTexts = parseFixtureList directiveBody

            if List.isEmpty expectedTexts then
                invalidOp $"assertContainsTokenTexts expects a comma-separated list of token texts ({filePath}:{lineNumber})."

            Some(AssertionDirective(AssertContainsTokenTexts(expectedTexts, filePath, lineNumber)))
        | other ->
            invalidOp $"Unsupported fixture assertion '{other}' at {filePath}:{lineNumber}."

let private loadFixtureDirectives (sourceKind: KpFixtureDirectiveSource) (filePath: string) =
    File.ReadAllLines(filePath)
    |> Array.mapi (fun index lineText ->
        let lineNumber = index + 1
        let trimmed = lineText.Trim()

        if sourceKind = KpFixtureDirectiveSource.SuiteDirectiveFile then
            if String.IsNullOrWhiteSpace(trimmed) || trimmed.StartsWith("--", StringComparison.Ordinal) then
                parseFixtureDirective sourceKind filePath lineNumber lineText
            else
                invalidOp $"Only directive, blank, or comment-only lines are allowed in '{filePath}' ({filePath}:{lineNumber})."
        else
            parseFixtureDirective sourceKind filePath lineNumber lineText)
    |> Array.choose id
    |> Array.toList

type private KpFixtureConfigurationAccumulator =
    { Mode: (KpFixtureMode * string * int) option
      PackageMode: (bool * string * int) option
      BackendProfile: (string * string * int) option
      EntryPoint: (string * string * int) option
      RunArgs: (string list * string * int) option
      StdinFile: (string * string * int) option
      DumpFormat: (StageDumpFormat * string * int) option }

let private emptyFixtureConfigurationAccumulator =
    { Mode = None
      PackageMode = None
      BackendProfile = None
      EntryPoint = None
      RunArgs = None
      StdinFile = None
      DumpFormat = None }

let private mergeFixtureConfigurationValue fieldName existing next =
    match existing with
    | None ->
        Some next
    | Some(existingValue, existingFilePath, existingLineNumber) ->
        let nextValue, nextFilePath, nextLineNumber = next

        if existingValue = nextValue then
            existing
        else
            invalidOp
                $"Conflicting fixture configuration for {fieldName}: '{existingFilePath}:{existingLineNumber}' and '{nextFilePath}:{nextLineNumber}'."

let private buildFixtureConfiguration (directives: KpFixtureDirective list) =
    let accumulator =
        directives
        |> List.fold
            (fun (state: KpFixtureConfigurationAccumulator) directive ->
                match directive with
                | SetMode(mode, filePath, lineNumber) ->
                    { state with
                        Mode = mergeFixtureConfigurationValue "mode" state.Mode (mode, filePath, lineNumber) }
                | SetPackageMode(packageMode, filePath, lineNumber) ->
                    { state with
                        PackageMode =
                            mergeFixtureConfigurationValue
                                "packageMode/scriptMode"
                                state.PackageMode
                                (packageMode, filePath, lineNumber) }
                | SetBackend(backendProfile, filePath, lineNumber) ->
                    { state with
                        BackendProfile =
                            mergeFixtureConfigurationValue "backend" state.BackendProfile (backendProfile, filePath, lineNumber) }
                | SetEntry(entryPoint, filePath, lineNumber) ->
                    { state with
                        EntryPoint = mergeFixtureConfigurationValue "entry" state.EntryPoint (entryPoint, filePath, lineNumber) }
                | SetRunArgs(runArgs, filePath, lineNumber) ->
                    { state with
                        RunArgs = mergeFixtureConfigurationValue "runArgs" state.RunArgs (runArgs, filePath, lineNumber) }
                | SetStdinFile(relativePath, filePath, lineNumber) ->
                    { state with
                        StdinFile = mergeFixtureConfigurationValue "stdinFile" state.StdinFile (relativePath, filePath, lineNumber) }
                | SetDumpFormat(dumpFormat, filePath, lineNumber) ->
                    { state with
                        DumpFormat =
                            mergeFixtureConfigurationValue "dumpFormat" state.DumpFormat (dumpFormat, filePath, lineNumber) }
                | AssertionDirective _ ->
                    state)
            emptyFixtureConfigurationAccumulator

    let configuration =
        { KpFixtureConfiguration.defaultValue with
            Mode = accumulator.Mode |> Option.map (fun (value, _, _) -> value) |> Option.defaultValue KpFixtureConfiguration.defaultValue.Mode
            PackageMode =
                accumulator.PackageMode
                |> Option.map (fun (value, _, _) -> value)
                |> Option.defaultValue KpFixtureConfiguration.defaultValue.PackageMode
            BackendProfile =
                accumulator.BackendProfile
                |> Option.map (fun (value, _, _) -> value)
                |> Option.defaultValue KpFixtureConfiguration.defaultValue.BackendProfile
            EntryPoint = accumulator.EntryPoint |> Option.map (fun (value, _, _) -> value)
            RunArgs = accumulator.RunArgs |> Option.map (fun (value, _, _) -> value) |> Option.defaultValue []
            StdinFile = accumulator.StdinFile |> Option.map (fun (value, _, _) -> value)
            DumpFormat =
                accumulator.DumpFormat
                |> Option.map (fun (value, _, _) -> value)
                |> Option.defaultValue KpFixtureConfiguration.defaultValue.DumpFormat }

    match configuration.Mode with
    | KpFixtureMode.Run ->
        if configuration.EntryPoint.IsNone then
            invalidOp "mode run requires an entry directive."

        configuration
    | _ ->
        if configuration.EntryPoint.IsSome then
            invalidOp "entry is valid only for mode run."

        if not (List.isEmpty configuration.RunArgs) then
            invalidOp "runArgs is valid only for mode run."

        if configuration.StdinFile.IsSome then
            invalidOp "stdinFile is valid only for mode run."

        configuration

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
                let suiteDirectiveFilePath = Path.Combine(caseDirectory, "suite.ktest")

                let directives =
                    (if File.Exists(suiteDirectiveFilePath) then
                         loadFixtureDirectives KpFixtureDirectiveSource.SuiteDirectiveFile suiteDirectiveFilePath
                     else
                         [])
                    @ (sourceFiles |> List.collect (loadFixtureDirectives KpFixtureDirectiveSource.KpSourceFile))

                let configuration = buildFixtureConfiguration directives

                let assertions =
                    directives
                    |> List.choose (function
                        | AssertionDirective assertion -> Some assertion
                        | _ -> None)

                Some
                    { Name = Path.GetFileName(caseDirectory)
                      Root = caseDirectory
                      SourceFiles = sourceFiles
                      Configuration = configuration
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

let private compileFixtureWorkspace (fixtureCase: KpFixtureCase) =
    let options =
        { CompilationOptions.create fixtureCase.Root with
            PackageMode = fixtureCase.Configuration.PackageMode
            BackendProfile = fixtureCase.Configuration.BackendProfile }

    Compilation.parse options [ fixtureCase.Root ]

type private FixtureRunResult =
    { ExitCode: int
      StandardOutput: string
      StandardError: string }

let private executeInterpreterRun (workspace: WorkspaceCompilation) (entryPoint: string) =
    let builder = StringBuilder()

    let output: RuntimeOutput =
        { Write = fun text -> builder.Append(text) |> ignore
          WriteLine = fun text -> builder.AppendLine(text) |> ignore }

    match Interpreter.executeBindingWithOutput workspace output entryPoint with
    | Result.Ok value ->
        if Interpreter.shouldPrintResult value then
            builder.AppendLine(RuntimeValue.format value) |> ignore

        { ExitCode = 0
          StandardOutput = normalizeLineEndings (builder.ToString())
          StandardError = "" }
    | Result.Error issue ->
        { ExitCode = 1
          StandardOutput = normalizeLineEndings (builder.ToString())
          StandardError = normalizeLineEndings issue.Message }

let private readFixtureStandardInput (fixtureCase: KpFixtureCase) =
    fixtureCase.Configuration.StdinFile
    |> Option.map (fun relativePath ->
        let fullPath = rootedFilePath fixtureCase.Root relativePath
        File.ReadAllText(fullPath))

let private renderProcessArguments (arguments: string list) =
    arguments
    |> List.map (fun argument ->
        let escapedArgument =
            argument.Replace("\\", "\\\\").Replace("\"", "\\\"")

        $"\"{escapedArgument}\"")
    |> String.concat " "

let private executeBackendRun (fixtureCase: KpFixtureCase) (workspace: WorkspaceCompilation) (entryPoint: string) =
    let outputDirectory = createScratchDirectory $"fixture-run-{fixtureCase.Name}-{fixtureCase.Configuration.BackendProfile}"
    let renderedArguments = renderProcessArguments fixtureCase.Configuration.RunArgs
    let stdinText = readFixtureStandardInput fixtureCase

    match fixtureCase.Configuration.BackendProfile with
    | "interpreter" ->
        executeInterpreterRun workspace entryPoint
    | "zig" ->
        let artifact =
            match Backend.emitZigArtifact workspace entryPoint outputDirectory with
            | Result.Ok artifact -> artifact
            | Result.Error message -> invalidOp message

        let compileResult =
            runProcessWithEnvironment
                artifact.OutputDirectory
                (ensureRepoZigExecutablePath ())
                $"cc -std=c11 -O0 -o \"{artifact.ExecutableFilePath}\" \"{artifact.SourceFilePath}\""
                []

        if compileResult.ExitCode <> 0 then
            { ExitCode = compileResult.ExitCode
              StandardOutput = compileResult.StandardOutput
              StandardError = compileResult.StandardError }
        else
            let runResult =
                runProcessWithInput artifact.OutputDirectory artifact.ExecutableFilePath renderedArguments stdinText

            { ExitCode = runResult.ExitCode
              StandardOutput = runResult.StandardOutput
              StandardError = runResult.StandardError }
    | "dotnet" ->
        let artifact =
            match Backend.emitDotNetArtifact workspace entryPoint outputDirectory DotNetDeployment.Managed with
            | Result.Ok artifact -> artifact
            | Result.Error message -> invalidOp message

        let runResult =
            runProcessWithInput
                outputDirectory
                "dotnet"
                ($"run --project \"{artifact.ProjectFilePath}\" -c Release"
                 + if String.IsNullOrWhiteSpace(renderedArguments) then "" else $" -- {renderedArguments}")
                stdinText

        { ExitCode = runResult.ExitCode
          StandardOutput = runResult.StandardOutput
          StandardError = runResult.StandardError }
    | other ->
        invalidOp $"mode run does not support backend '{other}' yet."

let private executeFixtureRun (fixtureCase: KpFixtureCase) (workspace: WorkspaceCompilation) =
    match fixtureCase.Configuration.Mode, fixtureCase.Configuration.EntryPoint with
    | KpFixtureMode.Run, Some entryPoint ->
        executeBackendRun fixtureCase workspace entryPoint
    | KpFixtureMode.Run, None ->
        invalidOp $"Fixture '{fixtureCase.Name}' selected mode run but did not configure an entry point."
    | _ ->
        { ExitCode = 0
          StandardOutput = ""
          StandardError = "" }

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
