open System
open System.IO
open Kappa.Compiler

type CliOptions =
    { SourceRoot: string
      DumpTokens: bool
      DumpAst: bool
      RunBinding: string option
      Inputs: string list }

module private Cli =
    let usage () =
        [
            "kappac [--source-root <path>] [--dump-tokens] [--dump-ast] [--run <binding>] [inputs...]"
            ""
            "If no input paths are supplied, the compiler scans the source root for *.kp files."
        ]
        |> String.concat Environment.NewLine

    let parse (argv: string array) : Result<CliOptions, string> =
        let inputs = ResizeArray<string>()
        let mutable sourceRoot = Directory.GetCurrentDirectory()
        let mutable dumpTokens = false
        let mutable dumpAst = false
        let mutable runBinding = None
        let mutable index = 0
        let mutable error: string option = None

        while index < argv.Length && error.IsNone do
            match argv[index] with
            | "--source-root" ->
                if index + 1 >= argv.Length then
                    error <- Some "Missing path after --source-root."
                else
                    sourceRoot <- argv[index + 1]
                    index <- index + 2
            | "--dump-tokens" ->
                dumpTokens <- true
                index <- index + 1
            | "--dump-ast" ->
                dumpAst <- true
                index <- index + 1
            | "--run" ->
                if index + 1 >= argv.Length then
                    error <- Some "Missing binding name after --run."
                else
                    runBinding <- Some argv[index + 1]
                    index <- index + 2
            | "--help"
            | "-h" ->
                error <- Some(usage ())
                index <- argv.Length
            | value ->
                inputs.Add(value)
                index <- index + 1

        match error with
        | Some message -> Result.Error message
        | None ->
            Result.Ok
                { SourceRoot = sourceRoot
                  DumpTokens = dumpTokens
                  DumpAst = dumpAst
                  RunBinding = runBinding
                  Inputs = List.ofSeq inputs }

let private severityText severity =
    match severity with
    | DiagnosticSeverity.Info -> "info"
    | DiagnosticSeverity.Warning -> "warning"
    | DiagnosticSeverity.Error -> "error"

let private formatLocation (location: SourceLocation) =
    $"{location.FilePath}({location.Start.Line},{location.Start.Column})"

let private printDiagnostic (diagnostic: Diagnostic) =
    match diagnostic.Location with
    | Some location ->
        printfn "%s: %s: %s" (formatLocation location) (severityText diagnostic.Severity) diagnostic.Message
    | None ->
        printfn "%s: %s" (severityText diagnostic.Severity) diagnostic.Message

let private declarationLabel declaration =
    match declaration with
    | ImportDeclaration (isExport, _) -> if isExport then "export" else "import"
    | FixityDeclarationNode _ -> "fixity"
    | SignatureDeclaration _ -> "signature"
    | LetDeclaration _ -> "let"
    | DataDeclarationNode _ -> "data"
    | TypeAliasNode _ -> "type"
    | TraitDeclarationNode _ -> "trait"
    | UnknownDeclaration _ -> "unknown"

let private printDocumentSummary (document: ParsedDocument) =
    let moduleName =
        document.ModuleName
        |> Option.map SyntaxFacts.moduleNameToText
        |> Option.defaultValue "<unknown>"

    let counts =
        document.Syntax.Declarations
        |> List.countBy declarationLabel
        |> List.sortBy fst
        |> List.map (fun (name, count) -> $"{name}={count}")
        |> String.concat ", "

    printfn "%s -> %s%s" document.Source.FilePath moduleName (if String.IsNullOrWhiteSpace counts then "" else $" ({counts})")

let private printTokens (document: ParsedDocument) =
    printfn ""
    printfn "Tokens for %s" document.Source.FilePath

    for token in document.Syntax.Tokens do
        printfn "  %-14A %s" token.Kind token.Text

let private printAst (document: ParsedDocument) =
    printfn ""
    printfn "Top-level declarations for %s" document.Source.FilePath

    for declaration in document.Syntax.Declarations do
        match declaration with
        | ImportDeclaration (isExport, specs) ->
            let prefix = if isExport then "export" else "import"
            printfn "  %s %d spec(s)" prefix specs.Length
        | FixityDeclarationNode declaration ->
            printfn "  fixity %s (%d)" declaration.OperatorName declaration.Precedence
        | SignatureDeclaration signature ->
            printfn "  signature %s" signature.Name
        | LetDeclaration definition ->
            printfn "  let %s" (defaultArg definition.Name "<pattern>")
        | DataDeclarationNode declaration ->
            printfn "  data %s (%d constructor line(s))" declaration.Name declaration.Constructors.Length
        | TypeAliasNode declaration ->
            printfn "  type %s" declaration.Name
        | TraitDeclarationNode declaration ->
            printfn "  trait %s (%d member line(s))" declaration.Name declaration.Members.Length
        | UnknownDeclaration tokens ->
            printfn "  unknown (%d token(s))" tokens.Length

[<EntryPoint>]
let main argv =
    match Cli.parse argv with
    | Result.Error message ->
        Console.Error.WriteLine(message)
        if message.Contains("kappac [--source-root") then 0 else 1
    | Result.Ok options ->
        let compilationOptions = CompilationOptions.create options.SourceRoot
        let workspace = Compilation.parse compilationOptions options.Inputs

        if List.isEmpty workspace.Documents then
            Console.Error.WriteLine($"No .kp files were found under {compilationOptions.SourceRoot}.")
            1
        else
            let shouldPrintSummaries =
                options.RunBinding.IsNone || options.DumpTokens || options.DumpAst

            if shouldPrintSummaries then
                for document in workspace.Documents do
                    printDocumentSummary document

            if options.DumpTokens then
                workspace.Documents |> List.iter printTokens

            if options.DumpAst then
                workspace.Documents |> List.iter printAst

            if not (List.isEmpty workspace.Diagnostics) then
                printfn ""
                printfn "Diagnostics"
                workspace.Diagnostics |> List.iter printDiagnostic

            if workspace.HasErrors then
                1
            else
                match options.RunBinding with
                | None ->
                    0
                | Some entryPoint ->
                    match Interpreter.evaluateBinding workspace entryPoint with
                    | Result.Ok value ->
                        printfn "%s" (RuntimeValue.format value)
                        0
                    | Result.Error issue ->
                        Console.Error.WriteLine($"runtime error: {issue.Message}")
                        1
