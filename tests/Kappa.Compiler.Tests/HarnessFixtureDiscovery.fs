// Discovers kp fixture cases and suite-level test inputs from disk.
module HarnessFixtureDiscovery

open System
open System.IO
open HarnessFixtureModel
open HarnessFixtureParser

let private fixturesRoot =
    Path.Combine(__SOURCE_DIRECTORY__, "Fixtures")

let private supportedCapabilities =
    Set.ofList [ "pipelineTrace"; "runTask" ]

let private discoverOrdinaryFixtureCase caseName caseDirectory =
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

        if Set.isSubset configuration.RequiredCapabilities supportedCapabilities then
            Some
                { Name = caseName
                  Root = caseDirectory
                  SourceFiles = sourceFiles
                  Configuration = configuration
                  Assertions = assertions }
        else
            None

let private discoverIncrementalFixtureCase caseName caseDirectory incrementalDirectiveFilePath =
    let directives =
        loadFixtureDirectives KpFixtureDirectiveSource.IncrementalDirectiveFile incrementalDirectiveFilePath

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

    let stepDirectories =
        Directory.EnumerateDirectories(caseDirectory, "step*", SearchOption.TopDirectoryOnly)
        |> Seq.choose (fun stepDirectory ->
            let name = Path.GetFileName(stepDirectory)

            if name.StartsWith("step", StringComparison.Ordinal) then
                match Int32.TryParse(name.Substring(4)) with
                | true, stepIndex -> Some(stepIndex, stepDirectory)
                | _ -> None
            else
                None)
        |> Seq.sortBy fst
        |> Seq.toList

    match stepDirectories with
    | [] ->
        invalidOp $"Incremental fixture '{caseName}' does not contain any step directories."
    | (0, _) :: _ -> ()
    | (firstStepIndex, _) :: _ ->
        invalidOp $"Incremental fixture '{caseName}' must begin at step0 but begins at step{firstStepIndex}."

    stepDirectories
    |> List.pairwise
    |> List.iter (fun ((previousIndex, _), (nextIndex, _)) ->
        if nextIndex <> previousIndex + 1 then
            invalidOp $"Incremental fixture '{caseName}' has non-contiguous steps: step{previousIndex} followed by step{nextIndex}.")

    let steps =
        stepDirectories
        |> List.choose (fun (stepIndex, stepDirectory) ->
            discoverOrdinaryFixtureCase $"{caseName}.step{stepIndex}" stepDirectory)

    if List.length steps <> List.length stepDirectories then
        None
    elif Set.isSubset requiredCapabilities supportedCapabilities then
        Some
            { Name = caseName
              Root = caseDirectory
              RequiredCapabilities = requiredCapabilities
              Steps = steps
              Assertions = assertions }
    else
        None

let discoverKpFixtureCases () =
    if not (Directory.Exists(fixturesRoot)) then
        []
    else
        Directory.EnumerateDirectories(fixturesRoot)
        |> Seq.sort
        |> Seq.choose (fun caseDirectory ->
            let incrementalDirectiveFilePath = Path.Combine(caseDirectory, "incremental.ktest")

            if File.Exists(incrementalDirectiveFilePath) then
                None
            else
                discoverOrdinaryFixtureCase (Path.GetFileName(caseDirectory)) caseDirectory)
        |> Seq.toList

let discoverIncrementalKpFixtureCases () =
    if not (Directory.Exists(fixturesRoot)) then
        []
    else
        Directory.EnumerateDirectories(fixturesRoot)
        |> Seq.sort
        |> Seq.choose (fun caseDirectory ->
            let incrementalDirectiveFilePath = Path.Combine(caseDirectory, "incremental.ktest")

            if File.Exists(incrementalDirectiveFilePath) then
                discoverIncrementalFixtureCase (Path.GetFileName(caseDirectory)) caseDirectory incrementalDirectiveFilePath
            else
                None)
        |> Seq.toList
