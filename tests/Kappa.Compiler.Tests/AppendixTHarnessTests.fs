module AppendixTHarnessTests

open System
open System.IO
open System.Text
open System.Text.Json
open Harness
open HarnessFixtureModel
open HarnessFixtureParser
open Kappa.Compiler
open Xunit

let private withScratchFixture name action =
    let root = createScratchDirectory name

    try
        action root
    finally
        if Directory.Exists(root) then
            Directory.Delete(root, true)

let private discoverFixtureCase root =
    let sourceFiles =
        Directory.EnumerateFiles(root, "*.kp", SearchOption.AllDirectories)
        |> Seq.sort
        |> Seq.toList

    let suiteDirectivePath = Path.Combine(root, "suite.ktest")

    let directives =
        (if File.Exists(suiteDirectivePath) then
             loadFixtureDirectives KpFixtureDirectiveSource.SuiteDirectiveFile suiteDirectivePath
         else
             [])
        @ (sourceFiles |> List.collect (loadFixtureDirectives KpFixtureDirectiveSource.KpSourceFile))

    let assertions =
        directives
        |> List.choose (function
            | AssertionDirective assertion -> Some assertion
            | _ -> None)

    { Name = Path.GetFileName(root)
      Root = root
      SourceFiles = sourceFiles
      Configuration = buildFixtureConfiguration directives
      Assertions = assertions }

let private discoverIncrementalFixtureCase root =
    let directives =
        loadFixtureDirectives
            KpFixtureDirectiveSource.IncrementalDirectiveFile
            (Path.Combine(root, "incremental.ktest"))

    let requiredCapabilities =
        directives
        |> List.choose (function
            | RequireCapability(capability, _, _) -> Some capability
            | _ -> None)
        |> Set.ofList

    let assertions =
        directives
        |> List.choose (function
            | IncrementalAssertionDirective assertion -> Some assertion
            | _ -> None)

    let steps =
        Directory.EnumerateDirectories(root, "step*", SearchOption.TopDirectoryOnly)
        |> Seq.sort
        |> Seq.map discoverFixtureCase
        |> Seq.toList

    { Name = Path.GetFileName(root)
      Root = root
      RequiredCapabilities = requiredCapabilities
      Steps = steps
      Assertions = assertions }

let private requireStageDump root checkpoint format =
    let workspace =
        Compilation.parse (CompilationOptions.create root) [ root ]

    Assert.False(workspace.HasErrors, String.concat Environment.NewLine (workspace.Diagnostics |> List.map _.Message))

    match Compilation.dumpStage workspace checkpoint format with
    | Result.Ok text -> text
    | Result.Error message -> failwithf "Expected dump for checkpoint '%s': %s" checkpoint message

let rec private renderJsonWithReorderedObjectKeys (element: JsonElement) =
    match element.ValueKind with
    | JsonValueKind.Object ->
        let properties =
            element.EnumerateObject()
            |> Seq.sortByDescending (fun property -> property.Name)
            |> Seq.map (fun property ->
                $"{JsonSerializer.Serialize(property.Name)}:{renderJsonWithReorderedObjectKeys property.Value}")
            |> String.concat ","

        $"{{{properties}}}"
    | JsonValueKind.Array ->
        let items =
            element.EnumerateArray()
            |> Seq.map renderJsonWithReorderedObjectKeys
            |> String.concat ","

        $"[{items}]"
    | _ ->
        element.GetRawText()

let private reorderJsonObjectKeys (json: string) =
    use document = JsonDocument.Parse(json)
    renderJsonWithReorderedObjectKeys document.RootElement

let private mutateSexprWhitespace (text: string) =
    let builder = StringBuilder()
    let mutable inString = false
    let mutable escaped = false

    for ch in text do
        if inString then
            builder.Append(ch) |> ignore

            if escaped then
                escaped <- false
            elif ch = '\\' then
                escaped <- true
            elif ch = '"' then
                inString <- false
        else
            match ch with
            | '"' ->
                inString <- true
                builder.Append(ch) |> ignore
            | '(' ->
                builder.AppendLine() |> ignore
                builder.Append("  (") |> ignore
            | ')' ->
                builder.Append(')') |> ignore
                builder.AppendLine() |> ignore
            | _ when Char.IsWhiteSpace(ch) ->
                builder.Append("  ") |> ignore
            | _ ->
                builder.Append(ch) |> ignore

    builder.ToString()

[<Fact>]
let ``suite directives accept standard assertStageDump assertions`` () =
    withScratchFixture "appendix-t-stage-dump-parse" (fun root ->
        writeWorkspaceFiles
            root
            [
                "suite.ktest", "--! assertStageDump KCore equals expected.json"
                "main.kp", "module main\nanswer : Int\nlet answer = 42\n"
            ]

        let directives =
            loadFixtureDirectives
                KpFixtureDirectiveSource.SuiteDirectiveFile
                (Path.Combine(root, "suite.ktest"))

        Assert.NotEmpty(directives))

[<Fact>]
let ``fixture harness evaluates assertStageDump with canonical json and sexpr comparison`` () =
    withScratchFixture "appendix-t-stage-dump-run" (fun root ->
        writeWorkspaceFiles
            root
            [
                "suite.ktest", "--! dumpFormat sexpr\n"
                "nested/main.kp",
                "module nested.main\nanswer : Int\nlet answer = 42\n--! assertStageDump KCore equals expected-kcore.json\n--! assertStageDump KCore equals expected-kcore.sexpr\n"
            ]

        let jsonDump = requireStageDump root "KCore" StageDumpFormat.Json
        let sexprDump = requireStageDump root "KCore" StageDumpFormat.SExpression

        writeWorkspaceFiles
            root
            [
                "expected-kcore.json", reorderJsonObjectKeys jsonDump
                "expected-kcore.sexpr", mutateSexprWhitespace sexprDump
            ]

        let fixtureCase = discoverFixtureCase root
        runKpFixtureCase fixtureCase)

[<Fact>]
let ``suite directives accept standard assertStderrFile assertions`` () =
    withScratchFixture "appendix-t-stderr-file-parse" (fun root ->
        writeWorkspaceFiles
            root
            [
                "suite.ktest", "--! assertStderrFile expected-stderr.txt"
                "main.kp", "module main\nmain : Unit -> Unit\nlet main () = ()\n"
            ]

        let directives =
            loadFixtureDirectives
                KpFixtureDirectiveSource.SuiteDirectiveFile
                (Path.Combine(root, "suite.ktest"))

        Assert.NotEmpty(directives))

[<Fact>]
let ``fixture harness evaluates assertStderrFile for interpreter runtime failures`` () =
    withScratchFixture "appendix-t-stderr-file-run" (fun root ->
        writeWorkspaceFiles
            root
            [
                "suite.ktest",
                "--! mode run\n--! backend interpreter\n--! entry main.result\n--! assertExitCode 1\n--! assertStderrFile expected-stderr.txt\n"
                "main.kp", "module main\nlet i0 i1 = 1\nlet result = i0 True i0 0\n"
            ]

        File.WriteAllText(
            Path.Combine(root, "expected-stderr.txt"),
            "Cannot apply 1 as a function.",
            Encoding.UTF8
        )

        let fixtureCase = discoverFixtureCase root
        runKpFixtureCase fixtureCase)

[<Fact>]
let ``incremental directives accept standard step assertions`` () =
    withScratchFixture "appendix-t-incremental-parse" (fun root ->
        writeWorkspaceFiles
            root
            [
                "incremental.ktest",
                "--! assertStepNoErrors 0\n--! assertStepErrorCount 1 1\n--! assertStepWarningCount 1 0\n--! assertStepTraceCount 0 parse file >= 1\n"
                "step0/main.kp", "module main\nanswer : Int\nlet answer = 42\n"
                "step0/suite.ktest", "--! mode check\n--! assertNoErrors\n"
                "step1/main.kp", "module main\nanswer : Int\nlet answer = True\n"
                "step1/suite.ktest", "--! mode check\n--! assertErrorCount 1\n"
            ]

        let directives =
            loadFixtureDirectives
                KpFixtureDirectiveSource.IncrementalDirectiveFile
                (Path.Combine(root, "incremental.ktest"))

        Assert.NotEmpty(directives))

[<Fact>]
let ``incremental fixture runner evaluates standard cross step assertions`` () =
    withScratchFixture "appendix-t-incremental-run" (fun root ->
        writeWorkspaceFiles
            root
            [
                "incremental.ktest",
                "--! assertStepNoErrors 0\n--! assertStepErrorCount 1 1\n--! assertStepWarningCount 1 0\n--! assertStepTraceCount 0 parse file >= 1\n"
                "step0/main.kp", "module main\nanswer : Int\nlet answer = 42\n"
                "step0/suite.ktest", "--! mode check\n--! assertNoErrors\n"
                "step1/main.kp", "module main\nanswer : Int\nlet answer = True\n"
                "step1/suite.ktest", "--! mode check\n--! assertErrorCount 1\n"
            ]

        let fixtureCase = discoverIncrementalFixtureCase root
        runIncrementalKpFixtureCase fixtureCase)
