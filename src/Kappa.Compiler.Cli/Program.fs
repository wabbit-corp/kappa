open System
open System.IO
open System.Reflection
open System.Runtime.Loader
open Kappa.Compiler

type CliOptions =
    { SourceRoot: string
      BackendProfile: string
      EmitDirectory: string option
      NativeAot: bool
      DumpTokens: bool
      DumpAst: bool
      DumpStage: string option
      DumpFormat: StageDumpFormat
      PrintTrace: bool
      VerifyCheckpoint: string option
      RunBinding: string option
      Inputs: string list }

// Implements the command-line entry point and wires compiler services to CLI commands.
module private Cli =
    let usage () =
        [
            "kp [--source-root <path>] [--backend <profile>] [--emit-dir <path>] [--native-aot] [--dump-tokens] [--dump-ast] [--dump-stage <checkpoint>] [--dump-format <json|sexpr>] [--trace] [--verify <checkpoint>] [--run <binding>] [inputs...]"
            ""
            "If no input paths are supplied, the compiler scans the source root for *.kp files."
            "Runtime backends: interpreter | dotnet | dotnet-hosted | dotnet-il | zig (alias: zigcc)"
        ]
        |> String.concat Environment.NewLine

    let parse (argv: string array) : Result<CliOptions, string> =
        let inputs = ResizeArray<string>()
        let mutable sourceRoot = Directory.GetCurrentDirectory()
        let mutable backendProfile = "interpreter"
        let mutable emitDirectory = None
        let mutable nativeAot = false
        let mutable dumpTokens = false
        let mutable dumpAst = false
        let mutable dumpStage = None
        let mutable dumpFormat = StageDumpFormat.Json
        let mutable printTrace = false
        let mutable verifyCheckpoint = None
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
            | "--backend" ->
                if index + 1 >= argv.Length then
                    error <- Some "Missing backend profile after --backend."
                else
                    backendProfile <- argv[index + 1]
                    index <- index + 2
            | "--emit-dir" ->
                if index + 1 >= argv.Length then
                    error <- Some "Missing path after --emit-dir."
                else
                    emitDirectory <- Some argv[index + 1]
                    index <- index + 2
            | "--native-aot" ->
                nativeAot <- true
                index <- index + 1
            | "--dump-tokens" ->
                dumpTokens <- true
                index <- index + 1
            | "--dump-ast" ->
                dumpAst <- true
                index <- index + 1
            | "--dump-stage" ->
                if index + 1 >= argv.Length then
                    error <- Some "Missing checkpoint name after --dump-stage."
                else
                    dumpStage <- Some argv[index + 1]
                    index <- index + 2
            | "--dump-format" ->
                if index + 1 >= argv.Length then
                    error <- Some "Missing format after --dump-format."
                else
                    match argv[index + 1].ToLowerInvariant() with
                    | "json" ->
                        dumpFormat <- StageDumpFormat.Json
                        index <- index + 2
                    | "sexpr" ->
                        dumpFormat <- StageDumpFormat.SExpression
                        index <- index + 2
                    | value ->
                        error <- Some $"Unsupported dump format '{value}'. Expected 'json' or 'sexpr'."
            | "--trace" ->
                printTrace <- true
                index <- index + 1
            | "--verify" ->
                if index + 1 >= argv.Length then
                    error <- Some "Missing checkpoint name after --verify."
                else
                    verifyCheckpoint <- Some argv[index + 1]
                    index <- index + 2
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
                  BackendProfile = backendProfile
                  EmitDirectory = emitDirectory
                  NativeAot = nativeAot
                  DumpTokens = dumpTokens
                  DumpAst = dumpAst
                  DumpStage = dumpStage
                  DumpFormat = dumpFormat
                  PrintTrace = printTrace
                  VerifyCheckpoint = verifyCheckpoint
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
    | ExpectDeclarationNode _ -> "expect"
    | SignatureDeclaration _ -> "signature"
    | LetDeclaration _ -> "let"
    | DataDeclarationNode _ -> "data"
    | TypeAliasNode _ -> "type"
    | TraitDeclarationNode _ -> "trait"
    | InstanceDeclarationNode _ -> "instance"
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
        | ExpectDeclarationNode (ExpectTypeDeclaration declaration) ->
            printfn "  expect type %s" declaration.Name
        | ExpectDeclarationNode (ExpectTraitDeclaration declaration) ->
            printfn "  expect trait %s" declaration.Name
        | ExpectDeclarationNode (ExpectTermDeclaration declaration) ->
            printfn "  expect term %s" declaration.Name
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
        | InstanceDeclarationNode declaration ->
            printfn "  instance %s (%d member line(s))" declaration.TraitName declaration.Members.Length
        | UnknownDeclaration tokens ->
            printfn "  unknown (%d token(s))" tokens.Length

let private printTraceStep (step: PipelineTraceStep) =
    let verificationText =
        if not step.VerificationAttempted then
            ""
        else
            match step.VerificationSucceeded with
            | Some true -> " verify=ok"
            | Some false -> " verify=failed"
            | None -> " verify=attempted"

    printfn
        "  %-18s %-12s %s -> %s changed=%b%s"
        (PipelineTraceEvent.toPortableName step.Event)
        (PipelineTraceSubject.toPortableName step.Subject)
        step.InputCheckpoint
        step.OutputCheckpoint
        step.ChangedRepresentation
        verificationText

let private printPipelineTrace workspace =
    printfn ""
    printfn "Pipeline trace"
    workspace
    |> Compilation.pipelineTrace
    |> List.iter printTraceStep

let private printVerification checkpoint diagnostics =
    printfn ""
    printfn "Verification for %s" checkpoint

    if List.isEmpty diagnostics then
        printfn "  ok"
    else
        diagnostics |> List.iter printDiagnostic

let private runProcess = HostSupport.runProcess
let private currentRid = HostSupport.currentRid
let private executablePath = HostSupport.executablePath

let private printProcessFailure (heading: string) (result: HostSupport.ProcessResult) =
    if not (String.IsNullOrWhiteSpace(result.StandardOutput)) then
        Console.Error.WriteLine(result.StandardOutput.TrimEnd())

    if not (String.IsNullOrWhiteSpace(result.StandardError)) then
        Console.Error.WriteLine(result.StandardError.TrimEnd())

    if String.IsNullOrWhiteSpace(result.StandardOutput) && String.IsNullOrWhiteSpace(result.StandardError) then
        Console.Error.WriteLine(heading)

let private runDotNetBackend
    (emitArtifact:
        WorkspaceCompilation -> string -> string -> DotNetDeployment -> Result<DotNetArtifact, string>)
    (workspace: WorkspaceCompilation)
    (entryPoint: string)
    (emitDirectory: string option)
    (nativeAot: bool)
    =
    let outputDirectory =
        emitDirectory
        |> Option.defaultWith (fun () ->
            Path.Combine(Path.GetTempPath(), "kappa-cli", Guid.NewGuid().ToString("N")))

    let deployment =
        if nativeAot then DotNetDeployment.NativeAot else DotNetDeployment.Managed

    match emitArtifact workspace entryPoint outputDirectory deployment with
    | Result.Error message ->
        Console.Error.WriteLine(message)
        1
    | Result.Ok artifact ->
        if nativeAot then
            let rid = currentRid ()
            let publishResult =
                runProcess artifact.ProjectDirectory "dotnet" $"publish \"{artifact.ProjectFilePath}\" -c Release -r {rid}"

            if publishResult.ExitCode <> 0 then
                printProcessFailure "Native AOT publish failed." publishResult
                1
            else
                let publishDirectory =
                    Path.Combine(artifact.ProjectDirectory, "bin", "Release", "net10.0", rid, "publish")

                let executable =
                    executablePath publishDirectory (Path.GetFileNameWithoutExtension(artifact.ProjectFilePath))

                let runResult = runProcess publishDirectory executable ""

                if not (String.IsNullOrWhiteSpace(runResult.StandardOutput)) then
                    Console.Out.Write(runResult.StandardOutput)

                if not (String.IsNullOrWhiteSpace(runResult.StandardError)) then
                    Console.Error.Write(runResult.StandardError)

                runResult.ExitCode
        else
            let buildResult =
                runProcess artifact.ProjectDirectory "dotnet" $"build \"{artifact.ProjectFilePath}\" -c Release"

            if buildResult.ExitCode <> 0 then
                printProcessFailure "Managed dotnet build failed." buildResult
                1
            else
                let outputDirectory =
                    Path.Combine(artifact.ProjectDirectory, "bin", "Release", "net10.0")

                let executable =
                    executablePath outputDirectory (Path.GetFileNameWithoutExtension(artifact.ProjectFilePath))

                let runResult = runProcess outputDirectory executable ""

                if not (String.IsNullOrWhiteSpace(runResult.StandardOutput)) then
                    Console.Out.Write(runResult.StandardOutput)

                if not (String.IsNullOrWhiteSpace(runResult.StandardError)) then
                    Console.Error.Write(runResult.StandardError)

                runResult.ExitCode

let private resolveIlEntryPoint (workspace: WorkspaceCompilation) (entryPoint: string) =
    let segments =
        entryPoint.Split('.', StringSplitOptions.RemoveEmptyEntries)
        |> Array.toList

    let tryMatchBinding moduleName bindingName =
        workspace.KRuntimeIR
        |> List.tryFind (fun (moduleDump: KRuntimeModule) -> String.Equals(moduleDump.Name, moduleName, StringComparison.Ordinal))
        |> Option.bind (fun moduleDump ->
            moduleDump.Bindings
            |> List.tryFind (fun (binding: KRuntimeBinding) ->
                String.Equals(binding.Name, bindingName, StringComparison.Ordinal)
                && not binding.Intrinsic
                && List.isEmpty binding.Parameters))

    match segments with
    | [] ->
        Result.Error "Expected a binding name to run."
    | [ bindingName ] ->
        let matches =
            workspace.KRuntimeIR
            |> List.choose (fun (moduleDump: KRuntimeModule) ->
                moduleDump.Bindings
                |> List.tryFind (fun (binding: KRuntimeBinding) ->
                    String.Equals(binding.Name, bindingName, StringComparison.Ordinal)
                    && not binding.Intrinsic
                    && List.isEmpty binding.Parameters)
                |> Option.map (fun binding -> moduleDump.Name, binding.Name))

        match matches with
        | [] ->
            Result.Error $"No zero-argument binding named '{bindingName}' was found for dotnet-il."
        | [ moduleName, resolvedBindingName ] ->
            Result.Ok(moduleName, resolvedBindingName)
        | _ ->
            Result.Error $"Binding name '{bindingName}' is ambiguous. Use a fully qualified name."
    | _ ->
        let moduleName = segments |> List.take (segments.Length - 1) |> String.concat "."
        let bindingName = List.last segments

        match tryMatchBinding moduleName bindingName with
        | Some _ ->
            Result.Ok(moduleName, bindingName)
        | None ->
            Result.Error $"dotnet-il requires a zero-argument binding named '{bindingName}' in module '{moduleName}'."

let private formatIlValue (value: obj) =
    match value with
    | :? int64 as integerValue -> string integerValue
    | :? double as floatValue -> string floatValue
    | :? bool as boolValue -> if boolValue then "True" else "False"
    | :? string as stringValue -> $"\"{stringValue}\""
    | :? char as characterValue -> $"'{characterValue}'"
    | null -> "()"
    | other -> other.ToString()

let private runIlBackend (workspace: WorkspaceCompilation) (entryPoint: string) (emitDirectory: string option) =
    let outputDirectory =
        emitDirectory
        |> Option.defaultWith (fun () ->
            Path.Combine(Path.GetTempPath(), "kappa-cli-il", Guid.NewGuid().ToString("N")))

    match resolveIlEntryPoint workspace entryPoint with
    | Result.Error message ->
        Console.Error.WriteLine(message)
        1
    | Result.Ok(moduleName, bindingName) ->
        match Backend.emitIlAssemblyArtifact workspace outputDirectory with
        | Result.Error message ->
            Console.Error.WriteLine(message)
            1
        | Result.Ok artifact ->
            let assemblyPath = Path.GetFullPath(artifact.AssemblyFilePath)
            let assembly = AssemblyLoadContext.Default.LoadFromAssemblyPath(assemblyPath)
            let typeName = IlDotNetBackend.emittedModuleTypeName moduleName
            let methodName = IlDotNetBackend.emittedMethodName bindingName
            let moduleType = assembly.GetType(typeName, throwOnError = false, ignoreCase = false)

            if isNull moduleType then
                Console.Error.WriteLine($"dotnet-il could not find emitted type '{typeName}'.")
                1
            else
                let method =
                    moduleType.GetMethod(methodName, BindingFlags.Public ||| BindingFlags.Static)

                if isNull method then
                    Console.Error.WriteLine($"dotnet-il could not find emitted method '{typeName}.{methodName}'.")
                    1
                else
                    let value = method.Invoke(null, [||])
                    Console.Out.WriteLine(formatIlValue value)
                    0

let private resolveZigExecutable () =
    let configuredPath = Environment.GetEnvironmentVariable("KAPPA_ZIG_EXE")

    if String.IsNullOrWhiteSpace(configuredPath) then
        Result.Ok "zig"
    else
        let resolvedPath = configuredPath.Trim()

        if File.Exists(resolvedPath) then
            Result.Ok resolvedPath
        else
            Result.Error $"Configured Zig executable '{resolvedPath}' does not exist. Set KAPPA_ZIG_EXE to a valid zig executable path."

let private runZigBackend
    (workspace: WorkspaceCompilation)
    (entryPoint: string)
    (emitDirectory: string option)
    (nativeAot: bool)
    =
    if nativeAot then
        Console.Error.WriteLine("The zig backend does not use --native-aot. Native code is its default output.")
        1
    else
        let outputDirectory =
            emitDirectory
            |> Option.defaultWith (fun () ->
                Path.Combine(Path.GetTempPath(), "kappa-cli-zig", Guid.NewGuid().ToString("N")))

        match resolveZigExecutable () with
        | Result.Error message ->
            Console.Error.WriteLine(message)
            1
        | Result.Ok zigExecutable ->
            match Backend.emitZigArtifact workspace entryPoint outputDirectory with
            | Result.Error message ->
                Console.Error.WriteLine(message)
                1
            | Result.Ok artifact ->
                try
                    let compileResult =
                        runProcess
                            artifact.OutputDirectory
                            zigExecutable
                            $"cc -std=c11 -O0 -o \"{artifact.ExecutableFilePath}\" \"{artifact.SourceFilePath}\""

                    if compileResult.ExitCode <> 0 then
                        printProcessFailure "zig build failed." compileResult
                        1
                    else
                        let runResult = runProcess artifact.OutputDirectory artifact.ExecutableFilePath ""

                        if not (String.IsNullOrWhiteSpace(runResult.StandardOutput)) then
                            Console.Out.Write(runResult.StandardOutput)

                        if not (String.IsNullOrWhiteSpace(runResult.StandardError)) then
                            Console.Error.Write(runResult.StandardError)

                        runResult.ExitCode
                with ex ->
                    Console.Error.WriteLine($"Could not launch Zig toolchain '{zigExecutable}': {ex.Message}")
                    1

[<EntryPoint>]
let main argv =
    match Cli.parse argv with
    | Result.Error message ->
        Console.Error.WriteLine(message)
        if message.Contains("kp [--source-root") then 0 else 1
    | Result.Ok options ->
        let compilationOptions =
            { CompilationOptions.create options.SourceRoot with
                BackendProfile = options.BackendProfile }

        let workspace = Compilation.parse compilationOptions options.Inputs

        if List.isEmpty workspace.Documents then
            Console.Error.WriteLine($"No .kp files were found under {compilationOptions.SourceRoot}.")
            1
        else
            let explicitObservabilityRequest =
                options.DumpStage.IsSome || options.PrintTrace || options.VerifyCheckpoint.IsSome

            let shouldPrintSummaries =
                (options.RunBinding.IsNone && not explicitObservabilityRequest)
                || options.DumpTokens
                || options.DumpAst

            if shouldPrintSummaries then
                for document in workspace.Documents do
                    printDocumentSummary document

            if options.DumpTokens then
                workspace.Documents |> List.iter printTokens

            if options.DumpAst then
                workspace.Documents |> List.iter printAst

            if options.PrintTrace then
                printPipelineTrace workspace

            let verificationFailed =
                match options.VerifyCheckpoint with
                | Some checkpoint ->
                    let diagnostics = Compilation.verifyCheckpoint workspace checkpoint
                    printVerification checkpoint diagnostics
                    not (List.isEmpty diagnostics)
                | None ->
                    false

            let dumpFailed =
                match options.DumpStage with
                | Some checkpoint ->
                    match Compilation.dumpStage workspace checkpoint options.DumpFormat with
                    | Result.Ok dump ->
                        printfn ""
                        printfn "%s" dump
                        false
                    | Result.Error message ->
                        Console.Error.WriteLine(message)
                        true
                | None ->
                    false

            if not (List.isEmpty workspace.Diagnostics) then
                printfn ""
                printfn "Diagnostics"
                workspace.Diagnostics |> List.iter printDiagnostic

            if workspace.HasErrors || verificationFailed || dumpFailed then
                1
            else
                match options.RunBinding with
                | None ->
                    0
                | Some entryPoint ->
                    match options.BackendProfile.ToLowerInvariant() with
                    | "interpreter" ->
                        match Interpreter.executeBinding workspace entryPoint with
                        | Result.Ok value ->
                            if Interpreter.shouldPrintResult value then
                                printfn "%s" (RuntimeValue.format value)

                            0
                        | Result.Error issue ->
                            Console.Error.WriteLine($"runtime error: {issue.Message}")
                            1
                    | "dotnet" ->
                        runDotNetBackend Backend.emitDotNetArtifact workspace entryPoint options.EmitDirectory options.NativeAot
                    | "dotnet-hosted" ->
                        runDotNetBackend Backend.emitHostedDotNetArtifact workspace entryPoint options.EmitDirectory options.NativeAot
                    | "dotnet-il" ->
                        runIlBackend workspace entryPoint options.EmitDirectory
                    | "zig"
                    | "zigcc" ->
                        runZigBackend workspace entryPoint options.EmitDirectory options.NativeAot
                    | other ->
                        Console.Error.WriteLine($"Unsupported runtime backend '{other}'. Expected interpreter, dotnet, dotnet-hosted, dotnet-il, or zig.")
                        1
