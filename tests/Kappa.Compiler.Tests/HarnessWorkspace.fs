// Builds in-memory workspaces and compiler utility entry points for test code.
module HarnessWorkspace

open System
open System.IO
open Kappa.Compiler

let createSource (filePath: string) (text: string) =
    SourceText.From(filePath, text.Replace("\r\n", "\n"))

let lexAndParse (filePath: string) (text: string) =
    let source = createSource filePath text
    let lexed = Lexer.tokenize source
    let parsed = Parser.parse source lexed.Tokens
    source, lexed, parsed

let private rootPath = HarnessSupport.rootPath
let private rootedFilePath = HarnessSupport.rootedFilePath

type InMemoryFileSystem(files: (string * string) list) =
    let normalize (path: string) = Path.GetFullPath(path)

    let fileMap =
        files
        |> List.map (fun (path, text) -> normalize path, text.Replace("\r\n", "\n"))
        |> Map.ofList

    let directories =
        let rec allParents (path: string) =
            seq {
                let parent = Path.GetDirectoryName(path)

                if not (String.IsNullOrWhiteSpace(parent)) then
                    yield parent
                    yield! allParents parent
            }

        fileMap.Keys
        |> Seq.collect allParents
        |> Set.ofSeq

    let isUnderRoot (root: string) (candidate: string) =
        let trimmedRoot = root.TrimEnd(Path.DirectorySeparatorChar, Path.AltDirectorySeparatorChar)
        let prefix = trimmedRoot + string Path.DirectorySeparatorChar

        String.Equals(candidate, trimmedRoot, StringComparison.OrdinalIgnoreCase)
        || candidate.StartsWith(prefix, StringComparison.OrdinalIgnoreCase)

    interface IFileSystem with
        member _.GetFullPath(path: string) = normalize path

        member _.FileExists(path: string) =
            fileMap.ContainsKey(normalize path)

        member _.DirectoryExists(path: string) =
            directories.Contains(normalize path)

        member _.EnumerateFiles(path: string, searchPattern: string, searchOption: SearchOption) =
            let root = normalize path

            fileMap.Keys
            |> Seq.filter (fun filePath ->
                let fileName = Path.GetFileName(filePath)

                let matchesPattern =
                    match searchPattern with
                    | "*.kp" -> filePath.EndsWith(".kp", StringComparison.OrdinalIgnoreCase)
                    | _ -> String.Equals(fileName, searchPattern, StringComparison.OrdinalIgnoreCase)

                let matchesScope =
                    match searchOption with
                    | SearchOption.TopDirectoryOnly ->
                        String.Equals(Path.GetDirectoryName(filePath), root, StringComparison.OrdinalIgnoreCase)
                    | SearchOption.AllDirectories ->
                        isUnderRoot root filePath
                    | _ ->
                        false

                matchesPattern && matchesScope)

        member _.ReadAllText(path: string) =
            fileMap[normalize path]

let compileInMemoryWorkspace (rootName: string) (files: (string * string) list) =
    let root = rootPath rootName

    let fileSystem =
        InMemoryFileSystem(
            files
            |> List.map (fun (filePath, text) -> rootedFilePath root filePath, text)
        )

    Compilation.parse (CompilationOptions.createWithFileSystem fileSystem root) [ root ]

let compileInMemoryWorkspaceWithBackend (rootName: string) (backendProfile: string) (files: (string * string) list) =
    let root = rootPath rootName

    let fileSystem =
        InMemoryFileSystem(
            files
            |> List.map (fun (filePath, text) -> rootedFilePath root filePath, text)
        )

    let options =
        { CompilationOptions.createWithFileSystem fileSystem root with
            BackendProfile = backendProfile }

    Compilation.parse options [ root ]

let compileInMemoryWorkspaceWithUnsafeConsume (rootName: string) (files: (string * string) list) =
    let root = rootPath rootName

    let fileSystem =
        InMemoryFileSystem(
            files
            |> List.map (fun (filePath, text) -> rootedFilePath root filePath, text)
        )

    let options =
        { CompilationOptions.createWithFileSystem fileSystem root with
            AllowUnsafeConsume = true }

    Compilation.parse options [ root ]

let evaluateInMemoryBinding (rootName: string) (entryPoint: string) (files: (string * string) list) =
    let workspace = compileInMemoryWorkspace rootName files
    workspace, Interpreter.evaluateBinding workspace entryPoint
