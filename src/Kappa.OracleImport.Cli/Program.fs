open System
open Kappa.OracleImport

type CliOptions =
    { CorpusRoot: string
      RepoSlug: string
      Prefixes: string list
      Limit: int option
      OutputDirectory: string option
      Write: bool }

module private Cli =
    let usage () =
        [
            "oracle-import --corpus-root <path> --repo <slug> [--prefix <path-prefix>]... [--limit <n>] [--output-dir <path>] [--write]"
            ""
            "Examples:"
            "  oracle-import --corpus-root ../datatron/data-scala-issues --repo youtrack-youtrack.jetbrains.com-kt --prefix compiler/testData/codegen/box/branching/"
            "  oracle-import --corpus-root ../datatron/data-scala-issues --repo github-scala-scala3 --prefix tests/patmat/ --limit 20 --write --output-dir ./artifacts/oracle-import"
        ]
        |> String.concat Environment.NewLine

    let parse (argv: string array) : Result<CliOptions, string> =
        let mutable corpusRoot = None
        let mutable repoSlug = None
        let prefixes = ResizeArray<string>()
        let mutable limit = None
        let mutable outputDirectory = None
        let mutable write = false
        let mutable index = 0
        let mutable error = None

        let readRequiredValue flagName =
            if index + 1 >= argv.Length then
                error <- Some $"Missing value after {flagName}."
                None
            else
                let value = argv[index + 1]
                index <- index + 2
                Some value

        while index < argv.Length && error.IsNone do
            match argv[index] with
            | "--corpus-root" ->
                corpusRoot <- readRequiredValue "--corpus-root"
            | "--repo" ->
                repoSlug <- readRequiredValue "--repo"
            | "--prefix" ->
                match readRequiredValue "--prefix" with
                | Some value -> prefixes.Add(value)
                | None -> ()
            | "--limit" ->
                match readRequiredValue "--limit" with
                | Some value ->
                    match Int32.TryParse(value) with
                    | true, parsed when parsed >= 0 -> limit <- Some parsed
                    | _ -> error <- Some "--limit expects a non-negative integer."
                | None -> ()
            | "--output-dir" ->
                outputDirectory <- readRequiredValue "--output-dir"
            | "--write" ->
                write <- true
                index <- index + 1
            | "--help"
            | "-h" ->
                error <- Some(usage ())
                index <- argv.Length
            | other ->
                error <- Some $"Unknown argument '{other}'."

        match error, corpusRoot, repoSlug with
        | Some message, _, _ -> Error message
        | None, Some corpusRootValue, Some repoSlugValue ->
            Ok
                { CorpusRoot = corpusRootValue
                  RepoSlug = repoSlugValue
                  Prefixes = List.ofSeq prefixes
                  Limit = limit
                  OutputDirectory = outputDirectory
                  Write = write }
        | None, None, _ ->
            Error "Missing required --corpus-root."
        | None, _, None ->
            Error "Missing required --repo."

let private printReport (report: ScanReport) =
    printfn "repo: %s" report.RepoSlug
    printfn "scanned files: %d" report.TotalFiles
    printfn "generated fixtures: %d" report.Fixtures.Length

    if List.isEmpty report.Fixtures then
        printfn "fixtures: none"
    else
        printfn "fixtures:"

        for fixture in report.Fixtures do
            printfn "  %s <- %s [%s]" fixture.FixtureDirectoryName fixture.SourcePath fixture.AdapterId

    if List.isEmpty report.SkipCounts then
        printfn "skip counts: none"
    else
        printfn "skip counts:"

        for reason, count in report.SkipCounts do
            printfn "  %s: %d" reason count

[<EntryPoint>]
let main argv =
    match Cli.parse argv with
    | Error message ->
        eprintfn "%s" message
        1
    | Ok options ->
        let report =
            OracleImport.scanRepo options.CorpusRoot options.RepoSlug options.Prefixes options.Limit

        printReport report

        if options.Write then
            let outputDirectory =
                options.OutputDirectory
                |> Option.defaultWith (fun () ->
                    failwith "--write requires --output-dir.")

            let writtenDirectories = OracleImport.emitFixtures outputDirectory report
            printfn "wrote %d fixture directories to %s" writtenDirectories.Length outputDirectory

        0
