#!/usr/bin/env -S dotnet fsi

open System
open System.Diagnostics
open System.IO
open System.Text
open System.Text.RegularExpressions

type GeneratedSample =
    { Index: int
      Source: string }

type ProcessResult =
    { ExitCode: int
      Stdout: string
      Stderr: string }

type Args =
    { Count: int
      Depth: int
      StartIndex: int
      Program: string
      Elpi: string option
      KappaCli: string option
      OutDir: string option
      Validate: bool
      Help: bool }

let repoRoot =
    Path.GetFullPath(Path.Combine(__SOURCE_DIRECTORY__, ".."))

let defaultElpiProgram =
    Path.Combine(repoRoot, "tools", "lambda_prolog", "kappa_generator.elpi")

let localElpi =
    Path.Combine(repoRoot, ".tools", "elpi-switch", "_opam", "bin", "elpi")

let releaseCliDir =
    Path.Combine(repoRoot, "src", "Kappa.Compiler.Cli", "bin", "Release")

let sampleStart = Regex(@"^%%KAPPA-SAMPLE\s+(\d+)\s*$", RegexOptions.Compiled)
let sampleEnd = "%%END-KAPPA-SAMPLE"

let usage =
    """Generate small well-typed Kappa programs from a Lambda Prolog model.

Options:
  --count N          number of programs to emit (default: 8)
  --depth N          maximum synthesis depth (default: 4)
  --start-index N    first generated candidate index (default: 0)
  --program PATH     ELPI program to execute
  --elpi PATH        path to the elpi executable
  --kappa-cli PATH   path to a release Kappa CLI binary used for --validate
  --out-dir PATH     directory for generated fixture cases
  --validate         compile each generated case with the Kappa CLI
  --help             show this help
"""

let expandUser (path: string) =
    if String.IsNullOrWhiteSpace path then
        path
    elif path = "~" then
        Environment.GetFolderPath(Environment.SpecialFolder.UserProfile)
    elif path.StartsWith("~/", StringComparison.Ordinal) then
        Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.UserProfile), path.Substring(2))
    else
        path

let existingFile (path: string) =
    let expanded = expandUser path
    if File.Exists expanded then Some expanded else None

let findOnPath (name: string) =
    let pathValue = Environment.GetEnvironmentVariable("PATH")

    if String.IsNullOrWhiteSpace pathValue then
        None
    else
        pathValue.Split(Path.PathSeparator, StringSplitOptions.RemoveEmptyEntries)
        |> Array.tryPick (fun dir ->
            let candidate = Path.Combine(dir, name)
            if File.Exists candidate then Some candidate else None)

let resolveElpi (explicit: string option) =
    seq {
        match explicit with
        | Some value -> value
        | None -> ()

        match Environment.GetEnvironmentVariable("ELPI") with
        | value when not (String.IsNullOrWhiteSpace value) -> value
        | _ -> ()

        match findOnPath "elpi" with
        | Some value -> value
        | None -> ()

        localElpi
    }
    |> Seq.tryPick existingFile

let resolveKappaCli (explicit: string option) =
    let releaseBinaries =
        if Directory.Exists releaseCliDir then
            Directory.EnumerateFiles(releaseCliDir, "Kappa.Compiler.Cli", SearchOption.AllDirectories)
            |> Seq.filter File.Exists
            |> Seq.sortByDescending (fun path -> File.GetLastWriteTimeUtc path)
            |> Seq.toList
        else
            []

    seq {
        match explicit with
        | Some value -> value
        | None -> ()

        match Environment.GetEnvironmentVariable("KAPPA_CLI") with
        | value when not (String.IsNullOrWhiteSpace value) -> value
        | _ -> ()

        yield! releaseBinaries
    }
    |> Seq.tryPick existingFile

let runProcess (fileName: string) (arguments: string list) =
    let startInfo = ProcessStartInfo()
    startInfo.FileName <- fileName
    startInfo.WorkingDirectory <- repoRoot
    startInfo.RedirectStandardOutput <- true
    startInfo.RedirectStandardError <- true
    startInfo.UseShellExecute <- false

    for argument in arguments do
        startInfo.ArgumentList.Add argument

    use proc = new Process()
    proc.StartInfo <- startInfo

    if not (proc.Start()) then
        failwithf "Could not start process: %s" fileName

    let stdoutTask = proc.StandardOutput.ReadToEndAsync()
    let stderrTask = proc.StandardError.ReadToEndAsync()
    proc.WaitForExit()

    { ExitCode = proc.ExitCode
      Stdout = stdoutTask.Result
      Stderr = stderrTask.Result }

let runElpi (elpi: string) (program: string) (count: int) (depth: int) (startIndex: int) =
    let result =
        runProcess
            elpi
            [ program
              "-exec"
              "emit-samples"
              "--"
              string count
              string depth
              string startIndex ]

    if result.ExitCode <> 0 then
        failwithf
            "ELPI generation failed with exit code %d\nSTDOUT:\n%s\nSTDERR:\n%s"
            result.ExitCode
            result.Stdout
            result.Stderr

    result.Stdout

let splitLines (text: string) =
    let normalized = text.Replace("\r\n", "\n").Replace("\r", "\n")
    let lines = normalized.Split('\n')

    if lines.Length > 0 && lines[lines.Length - 1] = "" then
        lines[0 .. lines.Length - 2]
    else
        lines

let parseElpiSamples (text: string) =
    let samples = ResizeArray<GeneratedSample>()
    let mutable currentIndex: int option = None
    let mutable currentLines: string list = []

    for rawLine in splitLines text do
        let startMatch = sampleStart.Match rawLine

        if startMatch.Success then
            match currentIndex with
            | Some _ -> failwithf "nested sample marker before %s" sampleEnd
            | None ->
                currentIndex <- Some(int startMatch.Groups[1].Value)
                currentLines <- []
        elif rawLine = sampleEnd then
            match currentIndex with
            | None -> failwithf "%s without a sample start" sampleEnd
            | Some index ->
                let source = (String.Join("\n", List.rev currentLines)).TrimEnd() + "\n"
                samples.Add({ Index = index; Source = source })
                currentIndex <- None
                currentLines <- []
        else
            match currentIndex with
            | Some _ -> currentLines <- rawLine :: currentLines
            | None when not (String.IsNullOrWhiteSpace rawLine) ->
                failwithf "unexpected ELPI output outside a sample block: %A" rawLine
            | None -> ()

    match currentIndex with
    | Some index -> failwithf "sample %d was not closed by %s" index sampleEnd
    | None -> samples |> Seq.toList

let writeFixtureCases (samples: GeneratedSample list) (outDir: string) =
    Directory.CreateDirectory(outDir) |> ignore

    samples
    |> List.map (fun sample ->
        let caseDir = Path.Combine(outDir, sprintf "lp_kappa_%03d" sample.Index)
        Directory.CreateDirectory(caseDir) |> ignore
        File.WriteAllText(Path.Combine(caseDir, "main.kp"), sample.Source, UTF8Encoding(false))
        caseDir)

let validateCase (cli: string option) (caseDir: string) =
    let kappaCli = cli |> Option.orElseWith (fun () -> resolveKappaCli None)

    match kappaCli with
    | None ->
        failwith
            "Could not find a release Kappa CLI binary. Build or provide src/Kappa.Compiler.Cli/bin/Release/.../Kappa.Compiler.Cli, or pass --kappa-cli /path/to/Kappa.Compiler.Cli."
    | Some executable ->
        let result =
            runProcess
                executable
                [ "--source-root"
                  caseDir
                  caseDir ]

        if result.ExitCode <> 0 then
            failwithf "Kappa validation failed for %s\nSTDOUT:\n%s\nSTDERR:\n%s" caseDir result.Stdout result.Stderr

let validateCases (caseDirs: string list) (cli: string option) =
    caseDirs |> List.iter (validateCase cli)

let requireValue (optionName: string) (remaining: string list) =
    match remaining with
    | value :: rest -> value, rest
    | [] -> failwithf "%s requires a value" optionName

let parseIntOption (optionName: string) (value: string) =
    match Int32.TryParse value with
    | true, parsed -> parsed
    | false, _ -> failwithf "%s must be an integer: %s" optionName value

let optionValueFromEquals (optionName: string) (argument: string) =
    let prefix = optionName + "="

    if argument.StartsWith(prefix, StringComparison.Ordinal) then
        Some(argument.Substring(prefix.Length))
    else
        None

let parseArgs (argv: string array) =
    let rec loop args parsed =
        match args with
        | [] -> parsed
        | "--help" :: rest -> loop rest { parsed with Help = true }
        | "--validate" :: rest -> loop rest { parsed with Validate = true }
        | argument :: rest when (optionValueFromEquals "--count" argument).IsSome ->
            loop rest { parsed with Count = parseIntOption "--count" (optionValueFromEquals "--count" argument).Value }
        | argument :: rest when (optionValueFromEquals "--depth" argument).IsSome ->
            loop rest { parsed with Depth = parseIntOption "--depth" (optionValueFromEquals "--depth" argument).Value }
        | argument :: rest when (optionValueFromEquals "--start-index" argument).IsSome ->
            loop
                rest
                { parsed with
                    StartIndex = parseIntOption "--start-index" (optionValueFromEquals "--start-index" argument).Value }
        | argument :: rest when (optionValueFromEquals "--program" argument).IsSome ->
            loop rest { parsed with Program = (optionValueFromEquals "--program" argument).Value }
        | argument :: rest when (optionValueFromEquals "--elpi" argument).IsSome ->
            loop rest { parsed with Elpi = Some(optionValueFromEquals "--elpi" argument).Value }
        | argument :: rest when (optionValueFromEquals "--kappa-cli" argument).IsSome ->
            loop rest { parsed with KappaCli = Some(optionValueFromEquals "--kappa-cli" argument).Value }
        | argument :: rest when (optionValueFromEquals "--out-dir" argument).IsSome ->
            loop rest { parsed with OutDir = Some(optionValueFromEquals "--out-dir" argument).Value }
        | "--count" :: rest ->
            let value, rest = requireValue "--count" rest
            loop rest { parsed with Count = parseIntOption "--count" value }
        | "--depth" :: rest ->
            let value, rest = requireValue "--depth" rest
            loop rest { parsed with Depth = parseIntOption "--depth" value }
        | "--start-index" :: rest ->
            let value, rest = requireValue "--start-index" rest
            loop rest { parsed with StartIndex = parseIntOption "--start-index" value }
        | "--program" :: rest ->
            let value, rest = requireValue "--program" rest
            loop rest { parsed with Program = value }
        | "--elpi" :: rest ->
            let value, rest = requireValue "--elpi" rest
            loop rest { parsed with Elpi = Some value }
        | "--kappa-cli" :: rest ->
            let value, rest = requireValue "--kappa-cli" rest
            loop rest { parsed with KappaCli = Some value }
        | "--out-dir" :: rest ->
            let value, rest = requireValue "--out-dir" rest
            loop rest { parsed with OutDir = Some value }
        | option :: _ when option.StartsWith("--", StringComparison.Ordinal) -> failwithf "unrecognized argument: %s" option
        | value :: _ -> failwithf "unexpected positional argument: %s" value

    loop
        (argv |> Array.toList)
        { Count = 8
          Depth = 4
          StartIndex = 0
          Program = defaultElpiProgram
          Elpi = None
          KappaCli = None
          OutDir = None
          Validate = false
          Help = false }

let main argv =
    try
        let args = parseArgs argv

        if args.Help then
            Console.Write usage
            0
        elif args.Count < 1 then
            eprintfn "--count must be at least 1"
            1
        elif args.Depth < 1 then
            eprintfn "--depth must be at least 1"
            1
        elif args.StartIndex < 0 then
            eprintfn "--start-index must be non-negative"
            1
        else
            match resolveElpi args.Elpi with
            | None ->
                eprintfn
                    "Could not find elpi. Install it with the local opam switch under .tools/elpi-switch or pass --elpi /path/to/elpi."
                1
            | Some elpi ->
                let output = runElpi elpi args.Program args.Count args.Depth args.StartIndex
                let samples = parseElpiSamples output

                if samples.Length <> args.Count then
                    eprintfn
                        "ELPI emitted %d samples; expected %d. Lower --count or expand the ELPI candidate set."
                        samples.Length
                        args.Count
                    1
                else
                    match args.OutDir with
                    | None ->
                        Console.Write output
                        0
                    | Some outDir ->
                        let caseDirs = writeFixtureCases samples outDir

                        if args.Validate then
                            validateCases caseDirs (resolveKappaCli args.KappaCli)

                        printfn "wrote %d Lambda Prolog generated Kappa cases to %s" caseDirs.Length outDir
                        0
    with ex ->
        eprintfn "%s" ex.Message
        1

let argv = fsi.CommandLineArgs |> Array.skip 1
Environment.ExitCode <- main argv
