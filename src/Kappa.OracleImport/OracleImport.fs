namespace Kappa.OracleImport

open System
open System.IO
open System.Text
open System.Text.Json
open System.Text.RegularExpressions

type GeneratedFixture =
    { FixtureDirectoryName: string
      MainFileText: string
      SourcePath: string
      AdapterId: string }

type ScanReport =
    { RepoSlug: string
      TotalFiles: int
      Fixtures: GeneratedFixture list
      SkipCounts: (string * int) list }

module OracleImport =
    type private TranslationResult =
        | Generated of GeneratedFixture
        | Skipped of string

    let private normalizeNewlines (text: string) = text.Replace("\r\n", "\n")

    let private regexSingleline pattern =
        Regex(pattern, RegexOptions.Singleline ||| RegexOptions.Multiline ||| RegexOptions.CultureInvariant)

    let private repoLabel (repoSlug: string) =
        match repoSlug with
        | "youtrack-youtrack.jetbrains.com-kt" -> "kotlin"
        | "github-scala-scala3" -> "scala3"
        | _ -> repoSlug

    let private sanitizeSegment (segment: string) =
        let sanitized = Regex.Replace(segment, "[^A-Za-z0-9]+", "_").Trim('_')

        if String.IsNullOrWhiteSpace(sanitized) then
            "part"
        else
            sanitized

    let private fixtureDirectoryName (repoSlug: string) (sourcePath: string) =
        let normalizedSourcePath = sourcePath.Replace('\\', '/')

        let rawParts =
            normalizedSourcePath.Split('/', StringSplitOptions.RemoveEmptyEntries)
            |> Array.toList

        let parts =
            rawParts
            |> List.mapi (fun index part ->
                if index = rawParts.Length - 1 then
                    Path.GetFileNameWithoutExtension(part)
                else
                    part)
            |> List.map sanitizeSegment

        String.concat "." ([ "oracle"; sanitizeSegment (repoLabel repoSlug) ] @ parts)

    let private joinLines (lines: string list) = String.concat "\n" lines

    let private quotedString (text: string) =
        "\"" + text.Replace("\\", "\\\\").Replace("\"", "\\\"") + "\""

    let private quotedKotlinStringAsKappa (text: string) =
        let interpolationPattern = Regex(@"\$(\w+)", RegexOptions.CultureInvariant)
        let matches = interpolationPattern.Matches(text)

        if matches.Count = 0 then
            quotedString text
        else
            let builder = StringBuilder()
            builder.Append("f\"") |> ignore

            let mutable cursor = 0

            for matched in matches |> Seq.cast<Match> do
                let literal = text.Substring(cursor, matched.Index - cursor)
                builder.Append(literal.Replace("\\", "\\\\").Replace("\"", "\\\"")) |> ignore
                builder.Append("${") |> ignore
                builder.Append(matched.Groups[1].Value) |> ignore
                builder.Append("}") |> ignore
                cursor <- matched.Index + matched.Length

            let suffix = text.Substring(cursor)
            builder.Append(suffix.Replace("\\", "\\\\").Replace("\"", "\\\"")) |> ignore
            builder.Append('"') |> ignore
            builder.ToString()

    let private fileEntriesFromManifest (manifestPath: string) =
        if not (File.Exists(manifestPath)) then
            None
        else
            use document = JsonDocument.Parse(File.ReadAllText(manifestPath))

            let entries =
                document.RootElement.GetProperty("files").EnumerateArray()
                |> Seq.choose (fun fileElement ->
                    let mutable property = Unchecked.defaultof<JsonElement>

                    if fileElement.TryGetProperty("source_path", &property) then
                        property.GetString() |> Option.ofObj
                    else
                        None)
                |> Seq.toList

            Some entries

    let private fullSourcePath (testsRoot: string) (sourcePath: string) =
        let relativePath = sourcePath.Replace('/', Path.DirectorySeparatorChar)
        Path.Combine(testsRoot, relativePath)

    let private matchesIncludePrefixes (includePrefixes: string list) (sourcePath: string) =
        match includePrefixes with
        | [] -> true
        | prefixes ->
            prefixes
            |> List.exists (fun prefix -> sourcePath.StartsWith(prefix, StringComparison.Ordinal))

    let private buildKotlinBranchingFixture
        (repoSlug: string)
        (sourcePath: string)
        (adapterId: string)
        (helperName: string)
        (helperParameter: string)
        (helperBody: string)
        (boxCallArgument: string)
        (expectedValue: string)
        (failureText: string)
        (successText: string)
        =
        let helperType =
            if adapterId = "kotlin_box_branching_when" then
                "Int -> Int"
            else
                "Bool -> Int"

        let fixtureText =
            joinLines
                [
                    "module main"
                    ""
                    $"-- Source repo: {repoLabel repoSlug}"
                    $"-- Source path: {sourcePath}"
                    ""
                    $"{helperName} : {helperType}"
                    $"let {helperName} {helperParameter} ="
                    helperBody
                    ""
                    "box : Unit -> String"
                    "let box () = block"
                    $"    let res = {helperName} {boxCallArgument}"
                    $"    if res != {expectedValue} then {quotedKotlinStringAsKappa failureText} else {quotedKotlinStringAsKappa successText}"
                    ""
                    "result : String"
                    "let result = box ()"
                    ""
                    "--! assertNoErrors"
                    $"--! assertType {helperName} {helperType}"
                    "--! assertType box Unit -> String"
                    "--! assertType result String"
                    "--! assertEval result \"OK\""
                ]

        { FixtureDirectoryName = fixtureDirectoryName repoSlug sourcePath
          MainFileText = fixtureText
          SourcePath = sourcePath
          AdapterId = adapterId }

    let private tryTranslateKotlinIfElse (repoSlug: string) (sourcePath: string) (sourceText: string) =
        let pattern =
            regexSingleline
                @"fun\s+(?<helper>[A-Za-z_][A-Za-z0-9_]*)\s*\(\s*(?<param>[A-Za-z_][A-Za-z0-9_]*)\s*:\s*Boolean\s*\)\s*:\s*Int\s*\{\s*if\s*\(\s*(?<cond>[A-Za-z_][A-Za-z0-9_]*)\s*\)\s*return\s+(?<then>-?\d+)\s*else\s*return\s+(?<else>-?\d+)\s*\}\s*fun\s+box\s*\(\s*\)\s*:\s*String\s*\{\s*val\s+(?<resultVar>[A-Za-z_][A-Za-z0-9_]*)\s*=\s*(?<calledHelper>[A-Za-z_][A-Za-z0-9_]*)\(\s*(?<arg>true|false)\s*\)\s*if\s*\(\s*(?<checkedVar>[A-Za-z_][A-Za-z0-9_]*)\s*!=\s*(?<expected>-?\d+)\s*\)\s*return\s+""(?<fail>[^""]*)""\s*return\s+""(?<ok>[^""]*)""\s*\}"

        let matched = pattern.Match(sourceText)

        if not matched.Success then
            None
        else
            let helperName = matched.Groups["helper"].Value
            let helperParameter = matched.Groups["param"].Value
            let conditionName = matched.Groups["cond"].Value
            let calledHelper = matched.Groups["calledHelper"].Value
            let resultVar = matched.Groups["resultVar"].Value
            let checkedVar = matched.Groups["checkedVar"].Value

            if helperName <> calledHelper || resultVar <> checkedVar || helperParameter <> conditionName then
                None
            else
                let thenValue = matched.Groups["then"].Value
                let elseValue = matched.Groups["else"].Value

                let helperBody =
                    [
                        $"    if {helperParameter} then {thenValue} else {elseValue}"
                    ]
                    |> joinLines

                let callArgument =
                    match matched.Groups["arg"].Value with
                    | "true" -> "True"
                    | _ -> "False"

                Some(
                    buildKotlinBranchingFixture
                        repoSlug
                        sourcePath
                        "kotlin_box_branching_if_else"
                        helperName
                        helperParameter
                        helperBody
                        callArgument
                        matched.Groups["expected"].Value
                        matched.Groups["fail"].Value
                        matched.Groups["ok"].Value
                )

    let private tryTranslateKotlinWhen (repoSlug: string) (sourcePath: string) (sourceText: string) =
        let pattern =
            regexSingleline
                @"fun\s+(?<helper>[A-Za-z_][A-Za-z0-9_]*)\s*\(\s*(?<param>[A-Za-z_][A-Za-z0-9_]*)\s*:\s*Int\s*\)\s*:\s*Int\s*\{\s*when\s*\(\s*(?<scrutinee>[A-Za-z_][A-Za-z0-9_]*)\s*\)\s*\{\s*(?<caseValue>-?\d+)\s*->\s*return\s+(?<caseResult>-?\d+)\s*else\s*->\s*return\s+(?<elseResult>-?\d+)\s*\}\s*\}\s*fun\s+box\s*\(\s*\)\s*:\s*String\s*\{\s*val\s+(?<resultVar>[A-Za-z_][A-Za-z0-9_]*)\s*=\s*(?<calledHelper>[A-Za-z_][A-Za-z0-9_]*)\(\s*(?<arg>-?\d+)\s*\)\s*if\s*\(\s*(?<checkedVar>[A-Za-z_][A-Za-z0-9_]*)\s*!=\s*(?<expected>-?\d+)\s*\)\s*return\s+""(?<fail>[^""]*)""\s*return\s+""(?<ok>[^""]*)""\s*\}"

        let matched = pattern.Match(sourceText)

        if not matched.Success then
            None
        else
            let helperName = matched.Groups["helper"].Value
            let helperParameter = matched.Groups["param"].Value
            let scrutinee = matched.Groups["scrutinee"].Value
            let calledHelper = matched.Groups["calledHelper"].Value
            let resultVar = matched.Groups["resultVar"].Value
            let checkedVar = matched.Groups["checkedVar"].Value

            if helperName <> calledHelper || resultVar <> checkedVar || helperParameter <> scrutinee then
                None
            else
                let caseValue = matched.Groups["caseValue"].Value
                let caseResult = matched.Groups["caseResult"].Value
                let elseResult = matched.Groups["elseResult"].Value

                let helperBody =
                    [
                        $"    match {helperParameter}"
                        $"      case {caseValue} -> {caseResult}"
                        $"      case _ -> {elseResult}"
                    ]
                    |> joinLines

                Some(
                    buildKotlinBranchingFixture
                        repoSlug
                        sourcePath
                        "kotlin_box_branching_when"
                        helperName
                        helperParameter
                        helperBody
                        matched.Groups["arg"].Value
                        matched.Groups["expected"].Value
                        matched.Groups["fail"].Value
                        matched.Groups["ok"].Value
                )

    let private tryTranslateKotlinNullElvis (repoSlug: string) (sourcePath: string) (sourceText: string) =
        let pattern = regexSingleline @"fun\s+box\s*\(\s*\)\s*=\s*null\s*\?:\s*null\s*\?:\s*""(?<ok>[^""]*)"""
        let matched = pattern.Match(sourceText)

        if not matched.Success then
            None
        else
            let successText = matched.Groups["ok"].Value

            let fixtureText =
                joinLines
                    [
                        "module main"
                        ""
                        $"-- Source repo: {repoLabel repoSlug}"
                        $"-- Source path: {sourcePath}"
                        ""
                        "import std.prelude.(type Option, ctor None)"
                        ""
                        "leftNone : Option String"
                        "let leftNone = None"
                        ""
                        "rightNone : Option String"
                        "let rightNone = None"
                        ""
                        "box : Unit -> String"
                        $"let box () = (leftNone ?: rightNone) ?: {quotedString successText}"
                        ""
                        "result : String"
                        "let result = box ()"
                        ""
                        "--! assertNoErrors"
                        "--! assertType leftNone Option String"
                        "--! assertType rightNone Option String"
                        "--! assertType box Unit -> String"
                        "--! assertType result String"
                        "--! assertEval result \"OK\""
                    ]

            Some
                { FixtureDirectoryName = fixtureDirectoryName repoSlug sourcePath
                  MainFileText = fixtureText
                  SourcePath = sourcePath
                  AdapterId = "kotlin_box_null_elvis_chain" }

    let private tryTranslateScalaGuardedOption (repoSlug: string) (sourcePath: string) (sourceText: string) =
        let functionPattern =
            regexSingleline
                @"def\s+(?<functionName>[A-Za-z_][A-Za-z0-9_]*)\(\s*(?<parameter>[A-Za-z_][A-Za-z0-9_]*)\s*:\s*Option\[Int\]\s*\)\s*:\s*Int\s*=\s*(?<scrutinee>[A-Za-z_][A-Za-z0-9_]*)\s*match\s*\{\s*case\s+Some\((?<binder>[A-Za-z_][A-Za-z0-9_]*)\)\s*if\s*(?<guard>.*?)\s*=>\s*(?<someValue>[^\r\n]+?)\s*case\s+None\s*=>\s*(?<noneValue>[^\r\n]+?)\s*\}"

        let samplePattern =
            regexSingleline @"println\(\s*(?<calledFunction>[A-Za-z_][A-Za-z0-9_]*)\(\s*Some\((?<sample>-?\d+)\)\s*\)\s*\)"

        let functionMatch = functionPattern.Match(sourceText)
        let sampleMatch = samplePattern.Match(sourceText)

        if not functionMatch.Success || not sampleMatch.Success then
            None
        else
            let functionName = functionMatch.Groups["functionName"].Value
            let parameter = functionMatch.Groups["parameter"].Value
            let scrutinee = functionMatch.Groups["scrutinee"].Value
            let calledFunction = sampleMatch.Groups["calledFunction"].Value

            if functionName <> calledFunction || parameter <> scrutinee then
                None
            else
                let guardText = functionMatch.Groups["guard"].Value.Trim()
                let someValueText = functionMatch.Groups["someValue"].Value.Trim()
                let noneValueText = functionMatch.Groups["noneValue"].Value.Trim()
                let binder = functionMatch.Groups["binder"].Value
                let sample = sampleMatch.Groups["sample"].Value

                let fixtureText =
                    joinLines
                        [
                            "module main"
                            ""
                            $"-- Source repo: {repoLabel repoSlug}"
                            $"-- Source path: {sourcePath}"
                            ""
                            "import std.prelude.(type Option, ctor Some, ctor None)"
                            ""
                            $"{functionName} : Option Int -> Int"
                            $"let {functionName} {parameter} ="
                            $"    match {parameter}"
                            $"      case Some {binder} if {guardText} -> {someValueText}"
                            $"      case None -> {noneValueText}"
                            ""
                            "result : Int"
                            $"let result = {functionName} (Some {sample})"
                            ""
                            "--! assertErrorCount 1"
                            "--! assertDiagnosticMatch exhaustive|non-exhaustive|guard"
                        ]

                Some
                    { FixtureDirectoryName = fixtureDirectoryName repoSlug sourcePath
                      MainFileText = fixtureText
                      SourcePath = sourcePath
                      AdapterId = "scala_option_guard_non_exhaustive" }

    let private tryTranslateKotlin (repoSlug: string) (sourcePath: string) (sourceText: string) =
        let normalizedPath = sourcePath.Replace('\\', '/')

        if not (normalizedPath.StartsWith("compiler/testData/codegen/box/", StringComparison.Ordinal)) then
            Skipped "unsupported_kotlin_path"
        else
            [
                tryTranslateKotlinIfElse repoSlug sourcePath sourceText
                tryTranslateKotlinWhen repoSlug sourcePath sourceText
                tryTranslateKotlinNullElvis repoSlug sourcePath sourceText
            ]
            |> List.tryPick id
            |> function
                | Some fixture -> Generated fixture
                | None -> Skipped "unsupported_kotlin_pattern"

    let private tryTranslateScala (repoSlug: string) (sourcePath: string) (sourceText: string) =
        let normalizedPath = sourcePath.Replace('\\', '/')

        if not (normalizedPath.StartsWith("tests/patmat/", StringComparison.Ordinal)) then
            Skipped "unsupported_scala_path"
        else
            match tryTranslateScalaGuardedOption repoSlug sourcePath sourceText with
            | Some fixture -> Generated fixture
            | None -> Skipped "unsupported_scala_pattern"

    let private translateSourceFile (repoSlug: string) (sourcePath: string) (sourceText: string) =
        match repoSlug with
        | "youtrack-youtrack.jetbrains.com-kt" -> tryTranslateKotlin repoSlug sourcePath sourceText
        | "github-scala-scala3" -> tryTranslateScala repoSlug sourcePath sourceText
        | _ -> Skipped "unsupported_repo"

    let scanRepo (corpusRoot: string) (repoSlug: string) (includePrefixes: string list) (limit: int option) : ScanReport =
        let testsRoot = Path.Combine(corpusRoot, "repos", repoSlug, "tests")
        let manifestPath = Path.Combine(testsRoot, "manifest.json")

        match fileEntriesFromManifest manifestPath with
        | None ->
            { RepoSlug = repoSlug
              TotalFiles = 0
              Fixtures = []
              SkipCounts = [ "missing_tests_manifest", 1 ] }
        | Some allEntries ->
            let filteredEntries =
                allEntries
                |> List.filter (matchesIncludePrefixes includePrefixes)
                |> fun entries ->
                    match limit with
                    | Some maxCount -> entries |> List.truncate maxCount
                    | None -> entries

            let results =
                filteredEntries
                |> List.map (fun sourcePath ->
                    let filePath = fullSourcePath testsRoot sourcePath

                    if not (File.Exists(filePath)) then
                        Skipped "missing_source_file"
                    else
                        let sourceText = normalizeNewlines (File.ReadAllText(filePath))
                        translateSourceFile repoSlug sourcePath sourceText)

            let fixtures =
                results
                |> List.choose (function
                    | Generated fixture -> Some fixture
                    | Skipped _ -> None)

            let skipCounts =
                results
                |> List.choose (function
                    | Generated _ -> None
                    | Skipped reason -> Some reason)
                |> List.countBy id
                |> List.sortBy fst

            { RepoSlug = repoSlug
              TotalFiles = filteredEntries.Length
              Fixtures = fixtures
              SkipCounts = skipCounts }

    let emitFixtures (outputRoot: string) (report: ScanReport) : string list =
        Directory.CreateDirectory(outputRoot) |> ignore

        let writtenDirectories =
            report.Fixtures
            |> List.map (fun fixture ->
                let fixtureDirectory = Path.Combine(outputRoot, fixture.FixtureDirectoryName)
                Directory.CreateDirectory(fixtureDirectory) |> ignore
                File.WriteAllText(Path.Combine(fixtureDirectory, "main.kp"), fixture.MainFileText + "\n")
                fixtureDirectory)

        let summaryPath = Path.Combine(outputRoot, "summary.json")

        use stream = File.Create(summaryPath)
        use writer = new Utf8JsonWriter(stream, JsonWriterOptions(Indented = true))

        writer.WriteStartObject()
        writer.WriteString("repo_slug", report.RepoSlug)
        writer.WriteNumber("total_files", report.TotalFiles)

        writer.WritePropertyName("fixtures")
        writer.WriteStartArray()

        for fixture in report.Fixtures do
            writer.WriteStartObject()
            writer.WriteString("fixture_directory_name", fixture.FixtureDirectoryName)
            writer.WriteString("source_path", fixture.SourcePath)
            writer.WriteString("adapter_id", fixture.AdapterId)
            writer.WriteEndObject()

        writer.WriteEndArray()

        writer.WritePropertyName("skip_counts")
        writer.WriteStartArray()

        for reason, count in report.SkipCounts do
            writer.WriteStartObject()
            writer.WriteString("reason", reason)
            writer.WriteNumber("count", count)
            writer.WriteEndObject()

        writer.WriteEndArray()
        writer.WriteEndObject()
        writer.Flush()

        writtenDirectories
