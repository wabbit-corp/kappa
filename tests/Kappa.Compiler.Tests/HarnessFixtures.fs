// Parses harness directives and suite configuration files into fixture assertions.
module HarnessFixtureParser

open System
open System.IO
open System.Text
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
        | Result.Error error ->
            let detail = (DiagnosticFact.describe (DiagnosticFact.parserSyntax (InvalidStringLiteral error))).Message
            invalidOp $"{directiveName} expects valid string literal text ({filePath}:{lineNumber}): {detail}"
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

let private parseFixtureDiagnosticCode (filePath: string) lineNumber (value: string) =
    match DiagnosticCode.tryParseIdentifier value with
    | Some code -> code
    | None ->
        invalidOp $"Unsupported diagnostic code '{value}' ({filePath}:{lineNumber})."

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

let private parseFixtureCapability (filePath: string) lineNumber (value: string) =
    match value.Trim() with
    | "pipelineTrace"
    | "incremental"
    | "runTask"
    | "legacyCharAlias"
    | "unicodeSourceWarnings" as capability ->
        capability
    | other ->
        invalidOp $"Unsupported fixture capability '{other}' ({filePath}:{lineNumber})."

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
        | Result.Error error ->
            let detail = (DiagnosticFact.describe (DiagnosticFact.parserSyntax (InvalidStringLiteral error))).Message
            invalidOp $"{directiveName} expects valid string literal text ({filePath}:{lineNumber}): {detail}"

        skipWhitespace ()

    List.ofSeq values

let private parseSingleStringLiteralArgument (directiveName: string) (filePath: string) lineNumber (directiveBody: string) =
    match parseStringLiteralArguments directiveName filePath lineNumber directiveBody with
    | [ value ] -> value
    | [] ->
        invalidOp $"{directiveName} expects a string literal ({filePath}:{lineNumber})."
    | _ ->
        invalidOp $"{directiveName} expects exactly one string literal ({filePath}:{lineNumber})."

let private parseSingleBareArgument (directiveName: string) (filePath: string) lineNumber (directiveBody: string) =
    let trimmed = directiveBody.Trim()

    if String.IsNullOrWhiteSpace(trimmed) then
        invalidOp $"{directiveName} expects a single argument ({filePath}:{lineNumber})."

    if trimmed.Contains(" ") || trimmed.Contains("\t") then
        invalidOp $"{directiveName} expects exactly one argument ({filePath}:{lineNumber})."

    trimmed

type KpFixtureDirective =
    | SetMode of KpFixtureMode * filePath: string * lineNumber: int
    | SetPackageMode of packageMode: bool * filePath: string * lineNumber: int
    | SetBackend of backendProfile: string * filePath: string * lineNumber: int
    | SetAllowUnsafeConsume of filePath: string * lineNumber: int
    | RequireCapability of capability: string * filePath: string * lineNumber: int
    | SetEntry of entryPoint: string * filePath: string * lineNumber: int
    | SetRunArgs of runArgs: string list * filePath: string * lineNumber: int
    | SetStdinFile of relativePath: string * filePath: string * lineNumber: int
    | SetDumpFormat of dumpFormat: StageDumpFormat * filePath: string * lineNumber: int
    | AssertionDirective of KpFixtureAssertion
    | ExtensionAssertionDirective of extensionName: string * KpFixtureAssertion
    | IncrementalAssertionDirective of KpIncrementalFixtureAssertion

let private parseFixtureDirective (sourceKind: KpFixtureDirectiveSource) (filePath: string) lineNumber (lineText: string) =
    match parseDirectiveHeader filePath lineNumber lineText with
    | None ->
        None
    | Some(rawDirectiveName, directiveBody) ->
        let directiveName = rawDirectiveName

        let assertionDirective assertion =
            if rawDirectiveName.StartsWith("x-", StringComparison.Ordinal) then
                ExtensionAssertionDirective(rawDirectiveName, assertion)
            else
                AssertionDirective assertion

        let ensureNoArguments () =
            if not (String.IsNullOrWhiteSpace(directiveBody)) then
                invalidOp $"{directiveName} does not take arguments ({filePath}:{lineNumber})."

        let ensureSourceFileDirective () =
            if sourceKind <> KpFixtureDirectiveSource.KpSourceFile then
                invalidOp $"{directiveName} is only valid in .kp source files ({filePath}:{lineNumber})."

        let ensureIncrementalDirective () =
            if sourceKind <> KpFixtureDirectiveSource.IncrementalDirectiveFile then
                invalidOp $"{directiveName} is only valid in incremental.ktest ({filePath}:{lineNumber})."

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
        | "allowUnsafeConsume"
        | "allow_unsafe_consume" ->
            ensureNoArguments ()
            Some(SetAllowUnsafeConsume(filePath, lineNumber))
        | "requires" ->
            let tokens =
                directiveBody.Split([| ' '; '\t' |], StringSplitOptions.RemoveEmptyEntries)

            match tokens with
            | [| "capability"; capability |] ->
                Some(RequireCapability(parseFixtureCapability filePath lineNumber capability, filePath, lineNumber))
            | _ ->
                invalidOp $"requires expects 'capability <name>' ({filePath}:{lineNumber})."
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
            Some(assertionDirective (AssertNoErrors(filePath, lineNumber)))
        | "assertNoWarnings" ->
            ensureNoArguments ()
            Some(assertionDirective (AssertNoWarnings(filePath, lineNumber)))
        | "assertErrorCount" ->
            Some(assertionDirective (AssertErrorCount(parseNonNegativeInt directiveName filePath lineNumber directiveBody, filePath, lineNumber)))
        | "assertWarningCount" ->
            Some(assertionDirective (AssertWarningCount(parseNonNegativeInt directiveName filePath lineNumber directiveBody, filePath, lineNumber)))
        | "assertDiagnostic" ->
            let tokens =
                directiveBody.Split([| ' '; '\t' |], StringSplitOptions.RemoveEmptyEntries)

            if tokens.Length <> 2 then
                invalidOp $"assertDiagnostic expects '<severity> <code>' ({filePath}:{lineNumber})."

            Some(
                assertionDirective
                    (AssertDiagnostic(
                        parseFixtureDiagnosticSeverity filePath lineNumber tokens[0],
                        parseFixtureDiagnosticCode filePath lineNumber tokens[1],
                        filePath,
                        lineNumber
                    ))
            )
        | "assertDiagnosticNext"
        | "assertDiagnosticHere" ->
            let tokens =
                directiveBody.Split([| ' '; '\t' |], StringSplitOptions.RemoveEmptyEntries)

            if tokens.Length <> 2 then
                invalidOp $"{directiveName} expects '<severity> <code>' ({filePath}:{lineNumber})."

            Some(
                assertionDirective
                    (AssertDiagnosticNext(
                        parseFixtureDiagnosticSeverity filePath lineNumber tokens[0],
                        parseFixtureDiagnosticCode filePath lineNumber tokens[1],
                        filePath,
                        lineNumber
                    ))
            )
        | "assertDiagnosticMatch" ->
            if String.IsNullOrWhiteSpace(directiveBody) then
                invalidOp $"assertDiagnosticMatch expects a regular expression ({filePath}:{lineNumber})."

            Some(assertionDirective (AssertDiagnosticMatch(directiveBody, filePath, lineNumber)))
        | "assertDiagnosticCodes" ->
            let expectedCodes =
                parseFixtureList directiveBody
                |> List.map (parseFixtureDiagnosticCode filePath lineNumber)

            if List.isEmpty expectedCodes then
                invalidOp $"assertDiagnosticCodes expects a comma-separated list of diagnostic codes ({filePath}:{lineNumber})."

            Some(assertionDirective (AssertDiagnosticCodes(expectedCodes, filePath, lineNumber)))
        | "assertDiagnosticAt" ->
            let tokens =
                directiveBody.Split([| ' '; '\t' |], StringSplitOptions.RemoveEmptyEntries)

            if tokens.Length <> 4 && tokens.Length <> 5 && tokens.Length <> 8 then
                invalidOp
                    $"assertDiagnosticAt expects '<path> <severity> <code> <line> [column]' or '<path> <severity> <code> <startLine> <startColumn> - <endLine> <endColumn>' ({filePath}:{lineNumber})."

            let parseRequiredOneBasedInt description token =
                let parsed = parseNonNegativeInt directiveName filePath lineNumber token

                if parsed = 0 then
                    invalidOp $"{directiveName} expects a 1-based {description} ({filePath}:{lineNumber})."

                parsed

            let expectedStartLine = parseRequiredOneBasedInt "line number" tokens[3]

            let expectedStartColumn, expectedEndLine, expectedEndColumn =
                match tokens.Length with
                | 4 ->
                    None, None, None
                | 5 ->
                    Some(parseRequiredOneBasedInt "column number" tokens[4]), None, None
                | 8 ->
                    if not (String.Equals(tokens[5], "-", StringComparison.Ordinal)) then
                        invalidOp
                            $"assertDiagnosticAt exact ranges require '-' between the start and end positions ({filePath}:{lineNumber})."

                    Some(parseRequiredOneBasedInt "start column number" tokens[4]),
                    Some(parseRequiredOneBasedInt "end line number" tokens[6]),
                    Some(parseRequiredOneBasedInt "end column number" tokens[7])
                | _ ->
                    invalidOp $"Unsupported assertDiagnosticAt form ({filePath}:{lineNumber})."

            Some(
                assertionDirective
                    (AssertDiagnosticAt(
                        tokens[0],
                        parseFixtureDiagnosticSeverity filePath lineNumber tokens[1],
                        parseFixtureDiagnosticCode filePath lineNumber tokens[2],
                        expectedStartLine,
                        expectedStartColumn,
                        expectedEndLine,
                        expectedEndColumn,
                        filePath,
                        lineNumber
                    ))
            )
        | "assertDiagnosticExplainExists" ->
            if String.IsNullOrWhiteSpace(directiveBody) then
                invalidOp $"assertDiagnosticExplainExists expects '<code>' ({filePath}:{lineNumber})."

            Some(
                assertionDirective
                    (AssertDiagnosticExplainExists(
                        parseFixtureDiagnosticCode filePath lineNumber directiveBody,
                        filePath,
                        lineNumber
                    ))
            )
        | "assertType" ->
            let targetAndType =
                directiveBody.Split([| ' '; '\t' |], 2, StringSplitOptions.RemoveEmptyEntries)

            if targetAndType.Length <> 2 then
                invalidOp $"assertType expects '<target> <type>' ({filePath}:{lineNumber})."

            Some(assertionDirective (AssertType(targetAndType[0], targetAndType[1].Trim(), filePath, lineNumber)))
        | "assertFileDeclKinds" ->
            let relativePath, expectedKinds = parseRelativePathAndList ()
            Some(assertionDirective (AssertFileDeclarationKinds(relativePath, expectedKinds, filePath, lineNumber)))
        | "assertStageDump" ->
            let tokens =
                directiveBody.Split([| ' '; '\t' |], StringSplitOptions.RemoveEmptyEntries)

            if tokens.Length <> 3 || not (String.Equals(tokens[1], "equals", StringComparison.Ordinal)) then
                invalidOp $"assertStageDump expects '<checkpoint> equals <path>' ({filePath}:{lineNumber})."

            Some(assertionDirective (AssertStageDump(tokens[0], tokens[2], filePath, lineNumber)))
        | "assertEval"
        | "x-assertEval" ->
            let target, expectedValueText = parseTargetAndBody "assertEval" filePath lineNumber directiveBody
            Some(assertionDirective (AssertEval(target, expectedValueText, filePath, lineNumber)))
        | "assertEvalErrorContains"
        | "x-assertEvalErrorContains" ->
            let target, expectedText = parseTargetAndBody "assertEvalErrorContains" filePath lineNumber directiveBody
            Some(assertionDirective (AssertEvalErrorContains(target, expectedText, filePath, lineNumber)))
        | "assertExecute" ->
            let target, expectedValueText = parseTargetAndBody "assertExecute" filePath lineNumber directiveBody
            Some(assertionDirective (AssertExecute(target, expectedValueText, filePath, lineNumber)))
        | "assertRunStdout" ->
            let target, expectedOutputText = parseTargetAndBody "assertRunStdout" filePath lineNumber directiveBody

            Some(
                assertionDirective
                    (AssertRunStdout(
                        target,
                        decodeAssertionText "assertRunStdout" filePath lineNumber expectedOutputText,
                        filePath,
                        lineNumber
                    ))
            )
        | "assertStdout" ->
            Some(
                assertionDirective
                    (AssertStdout(
                        parseSingleStringLiteralArgument directiveName filePath lineNumber directiveBody,
                        filePath,
                        lineNumber
                    ))
            )
        | "assertStdoutFile" ->
            Some(
                assertionDirective
                    (AssertStdoutFile(
                        parseSingleBareArgument directiveName filePath lineNumber directiveBody,
                        filePath,
                        lineNumber
                    ))
            )
        | "assertStdoutContains" ->
            Some(
                assertionDirective
                    (AssertStdoutContains(
                        parseSingleStringLiteralArgument directiveName filePath lineNumber directiveBody,
                        filePath,
                        lineNumber
                    ))
            )
        | "assertStderrContains" ->
            Some(
                assertionDirective
                    (AssertStderrContains(
                        parseSingleStringLiteralArgument directiveName filePath lineNumber directiveBody,
                        filePath,
                        lineNumber
                    ))
            )
        | "assertStderrFile" ->
            Some(
                assertionDirective
                    (AssertStderrFile(
                        parseSingleBareArgument directiveName filePath lineNumber directiveBody,
                        filePath,
                        lineNumber
                    ))
            )
        | "assertExitCode" ->
            Some(assertionDirective (AssertExitCode(parseNonNegativeInt directiveName filePath lineNumber directiveBody, filePath, lineNumber)))
        | "assertTraceCount" ->
            let tokens =
                directiveBody.Split([| ' '; '\t' |], StringSplitOptions.RemoveEmptyEntries)

            if tokens.Length <> 4 then
                invalidOp $"{directiveName} expects '<event> <subject> <relop> <n>' ({filePath}:{lineNumber})."

            Some(
                assertionDirective
                    (AssertTraceCount(
                        tokens[0],
                        tokens[1],
                        parseFixtureRelation filePath lineNumber tokens[2],
                        parseNonNegativeInt directiveName filePath lineNumber tokens[3],
                        filePath,
                        lineNumber
                    ))
            )
        | "assertStepNoErrors" ->
            ensureIncrementalDirective ()

            Some(
                IncrementalAssertionDirective(
                    AssertStepNoErrors(
                        parseNonNegativeInt directiveName filePath lineNumber directiveBody,
                        filePath,
                        lineNumber
                    )
                )
            )
        | "assertStepErrorCount" ->
            ensureIncrementalDirective ()

            let tokens =
                directiveBody.Split([| ' '; '\t' |], StringSplitOptions.RemoveEmptyEntries)

            if tokens.Length <> 2 then
                invalidOp $"{directiveName} expects '<step> <n>' ({filePath}:{lineNumber})."

            Some(
                IncrementalAssertionDirective(
                    AssertStepErrorCount(
                        parseNonNegativeInt directiveName filePath lineNumber tokens[0],
                        parseNonNegativeInt directiveName filePath lineNumber tokens[1],
                        filePath,
                        lineNumber
                    )
                )
            )
        | "assertStepWarningCount" ->
            ensureIncrementalDirective ()

            let tokens =
                directiveBody.Split([| ' '; '\t' |], StringSplitOptions.RemoveEmptyEntries)

            if tokens.Length <> 2 then
                invalidOp $"{directiveName} expects '<step> <n>' ({filePath}:{lineNumber})."

            Some(
                IncrementalAssertionDirective(
                    AssertStepWarningCount(
                        parseNonNegativeInt directiveName filePath lineNumber tokens[0],
                        parseNonNegativeInt directiveName filePath lineNumber tokens[1],
                        filePath,
                        lineNumber
                    )
                )
            )
        | "assertStepTraceCount" ->
            ensureIncrementalDirective ()

            let tokens =
                directiveBody.Split([| ' '; '\t' |], StringSplitOptions.RemoveEmptyEntries)

            if tokens.Length <> 5 then
                invalidOp $"{directiveName} expects '<step> <event> <subject> <relop> <n>' ({filePath}:{lineNumber})."

            Some(
                IncrementalAssertionDirective(
                    AssertStepTraceCount(
                        parseNonNegativeInt directiveName filePath lineNumber tokens[0],
                        tokens[1],
                        tokens[2],
                        parseFixtureRelation filePath lineNumber tokens[3],
                        parseNonNegativeInt directiveName filePath lineNumber tokens[4],
                        filePath,
                        lineNumber
                    )
                )
            )
        | "assertModule"
        | "x-assertModule" ->
            if String.IsNullOrWhiteSpace(directiveBody) then
                invalidOp $"assertModule expects '<module>' ({filePath}:{lineNumber})."

            Some(assertionDirective (AssertModule(directiveBody, filePath, lineNumber)))
        | "assertModuleAttributes"
        | "x-assertModuleAttributes" ->
            let expectedAttributes = parseFixtureList directiveBody

            if List.isEmpty expectedAttributes then
                invalidOp $"assertModuleAttributes expects a comma-separated list of attributes ({filePath}:{lineNumber})."

            Some(assertionDirective (AssertModuleAttributes(expectedAttributes, filePath, lineNumber)))
        | "assertDeclKinds" ->
            ensureSourceFileDirective ()

            let expectedKinds = parseFixtureList directiveBody

            if List.isEmpty expectedKinds then
                invalidOp $"assertDeclKinds expects a comma-separated list of declaration kinds ({filePath}:{lineNumber})."

            Some(assertionDirective (AssertDeclarationKinds(expectedKinds, filePath, lineNumber)))
        | "assertDeclDescriptors"
        | "x-assertDeclDescriptors" ->
            ensureSourceFileDirective ()

            let expectedDescriptors = parseFixtureList directiveBody

            if List.isEmpty expectedDescriptors then
                invalidOp $"assertDeclDescriptors expects a comma-separated list of declaration descriptors ({filePath}:{lineNumber})."

            Some(assertionDirective (AssertDeclarationDescriptors(expectedDescriptors, filePath, lineNumber)))
        | "assertParameterQuantities" ->
            ensureSourceFileDirective ()
            let bindingName, expectedQuantities = parseTargetAndList "binding"
            Some(assertionDirective (AssertParameterQuantities(bindingName, expectedQuantities, filePath, lineNumber)))
        | "assertInoutParameters" ->
            ensureSourceFileDirective ()
            let bindingName, expectedNames = parseTargetAndList "binding"
            Some(assertionDirective (AssertInoutParameters(bindingName, expectedNames, filePath, lineNumber)))
        | "assertDoItemDescriptors" ->
            ensureSourceFileDirective ()
            let bindingName, expectedDescriptors = parseTargetAndList "binding"
            Some(assertionDirective (AssertDoItemDescriptors(bindingName, expectedDescriptors, filePath, lineNumber)))
        | "assertDataConstructors"
        | "x-assertDataConstructors" ->
            ensureSourceFileDirective ()

            let typeAndConstructors =
                directiveBody.Split([| ' '; '\t' |], 2, StringSplitOptions.RemoveEmptyEntries)

            if typeAndConstructors.Length <> 2 then
                invalidOp $"assertDataConstructors expects '<type> <ctor1, ctor2, ...>' ({filePath}:{lineNumber})."

            let expectedConstructors = parseFixtureList typeAndConstructors[1]

            if List.isEmpty expectedConstructors then
                invalidOp $"assertDataConstructors expects at least one constructor ({filePath}:{lineNumber})."

            Some(assertionDirective (AssertDataConstructors(typeAndConstructors[0], expectedConstructors, filePath, lineNumber)))
        | "assertTraitMembers"
        | "x-assertTraitMembers" ->
            ensureSourceFileDirective ()

            let traitAndMembers =
                directiveBody.Split([| ' '; '\t' |], 2, StringSplitOptions.RemoveEmptyEntries)

            if traitAndMembers.Length <> 2 then
                invalidOp $"assertTraitMembers expects '<trait> <member1, member2, ...>' ({filePath}:{lineNumber})."

            let expectedMembers = parseFixtureList traitAndMembers[1]

            if List.isEmpty expectedMembers then
                invalidOp $"assertTraitMembers expects at least one member ({filePath}:{lineNumber})."

            Some(assertionDirective (AssertTraitMembers(traitAndMembers[0], expectedMembers, filePath, lineNumber)))
        | "assertContainsTokenKinds"
        | "x-assertContainsTokenKinds" ->
            ensureSourceFileDirective ()

            let expectedKinds = parseFixtureList directiveBody

            if List.isEmpty expectedKinds then
                invalidOp $"assertContainsTokenKinds expects a comma-separated list of token kinds ({filePath}:{lineNumber})."

            Some(assertionDirective (AssertContainsTokenKinds(expectedKinds, filePath, lineNumber)))
        | "assertContainsTokenTexts"
        | "x-assertContainsTokenTexts" ->
            ensureSourceFileDirective ()

            let expectedTexts = parseFixtureList directiveBody

            if List.isEmpty expectedTexts then
                invalidOp $"assertContainsTokenTexts expects a comma-separated list of token texts ({filePath}:{lineNumber})."

            Some(assertionDirective (AssertContainsTokenTexts(expectedTexts, filePath, lineNumber)))
        | other ->
            invalidOp $"Unsupported fixture assertion '{other}' at {filePath}:{lineNumber}."

let loadFixtureDirectives (sourceKind: KpFixtureDirectiveSource) (filePath: string) =
    let lines =
        match sourceKind with
        | KpFixtureDirectiveSource.SuiteDirectiveFile ->
            File.ReadAllLines(filePath)
        | KpFixtureDirectiveSource.IncrementalDirectiveFile ->
            File.ReadAllLines(filePath)
        | KpFixtureDirectiveSource.KpSourceFile ->
            let utf8Strict = UTF8Encoding(false, true)

            try
                File.ReadAllBytes(filePath)
                |> utf8Strict.GetString
                |> fun text -> text.Replace("\r\n", "\n").Replace('\r', '\n').Split('\n')
            with :? DecoderFallbackException ->
                [||]

    lines
    |> Array.mapi (fun index lineText ->
        let lineNumber = index + 1
        let trimmed = lineText.Trim()

        if sourceKind = KpFixtureDirectiveSource.SuiteDirectiveFile || sourceKind = KpFixtureDirectiveSource.IncrementalDirectiveFile then
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
      AllowUnsafeConsume: (bool * string * int) option
      RequiredCapabilities: Set<string>
      EntryPoint: (string * string * int) option
      RunArgs: (string list * string * int) option
      StdinFile: (string * string * int) option
      DumpFormat: (StageDumpFormat * string * int) option }

let private emptyFixtureConfigurationAccumulator =
    { Mode = None
      PackageMode = None
      BackendProfile = None
      AllowUnsafeConsume = None
      RequiredCapabilities = Set.empty
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
                | SetAllowUnsafeConsume(filePath, lineNumber) ->
                    { state with
                        AllowUnsafeConsume =
                            mergeFixtureConfigurationValue
                                "allowUnsafeConsume"
                                state.AllowUnsafeConsume
                                (true, filePath, lineNumber) }
                | RequireCapability(capability, _, _) ->
                    { state with
                        RequiredCapabilities = Set.add capability state.RequiredCapabilities }
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
                    state
                | ExtensionAssertionDirective _ ->
                    state
                | IncrementalAssertionDirective _ ->
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
            AllowUnsafeConsume =
                accumulator.AllowUnsafeConsume
                |> Option.map (fun (value, _, _) -> value)
                |> Option.defaultValue KpFixtureConfiguration.defaultValue.AllowUnsafeConsume
            RequiredCapabilities = accumulator.RequiredCapabilities
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
