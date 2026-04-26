// Discovers kp fixture cases and suite-level test inputs from disk.
module HarnessFixtureDiscovery

open System
open System.IO
open HarnessFixtureModel
open HarnessFixtureParser

let private fixturesRoot =
    Path.Combine(__SOURCE_DIRECTORY__, "Fixtures")

let private supportedCapabilities =
    Set.ofList [ "pipelineTrace"; "incremental"; "runTask" ]

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
                            { Name = Path.GetFileName(caseDirectory)
                              Root = caseDirectory
                              SourceFiles = sourceFiles
                              Configuration = configuration
                              Assertions = assertions }
                    else
                        None)
        |> Seq.toList
