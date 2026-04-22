module HarnessFixtureParser

open System
open System.IO
open HarnessFixtureModel
open Kappa.Compiler

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

type KpFixtureDirective =
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

let loadFixtureDirectives (sourceKind: KpFixtureDirectiveSource) (filePath: string) =
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

let buildFixtureConfiguration (directives: KpFixtureDirective list) =
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
