namespace Kappa.LSP

open System
open System.IO
open System.Text
open Kappa.Compiler

module Workspace =
    let pathComparison =
        if OperatingSystem.IsWindows() then
            StringComparison.OrdinalIgnoreCase
        else
            StringComparison.Ordinal

    let normalizePath (fileSystem: IFileSystem) (path: string) =
        fileSystem.GetFullPath(path)

    let filePathToUri (filePath: string) =
        Uri(filePath).AbsoluteUri

    let private isWindowsDrivePathWithLeadingSlash (path: string) =
        path.Length >= 3
        && (path[0] = '/' || path[0] = '\\')
        && Char.IsLetter(path[1])
        && path[2] = ':'

    let private normalizeFileUriLocalPath (path: string) =
        let normalizedSlashes = path.Replace('/', Path.DirectorySeparatorChar)

        if isWindowsDrivePathWithLeadingSlash normalizedSlashes then
            normalizedSlashes.Substring(1)
        else
            normalizedSlashes

    let tryUriToFilePath (uriText: string) =
        try
            match Uri.TryCreate(uriText, UriKind.Absolute) with
            | true, uri when uri.IsFile ->
                uri.LocalPath
                |> normalizeFileUriLocalPath
                |> Path.GetFullPath
                |> Some
            | _ -> None
        with _ ->
            None

    let isRootedPublishablePath (filePath: string) =
        Path.IsPathRooted(filePath) && not (filePath.StartsWith("<", StringComparison.Ordinal))

    let rootPathFromWorkspace (rootUri: string option) (workspaceFolderUris: string list) =
        let candidates =
            [ yield! rootUri |> Option.toList
              yield! workspaceFolderUris ]

        candidates |> List.tryPick tryUriToFilePath

type WorkspaceOverlayFileSystem(baseFileSystem: IFileSystem, overlays: Map<string, string>) =
    let normalizedOverlays =
        overlays
        |> Map.toList
        |> List.map (fun (path, text) -> Workspace.normalizePath baseFileSystem path, text.Replace("\r\n", "\n"))
        |> Map.ofList

    let overlayDirectories =
        let rec parentsOf (path: string) =
            seq {
                let parent = Path.GetDirectoryName(path)

                if not (String.IsNullOrWhiteSpace(parent)) then
                    let fullParent = Workspace.normalizePath baseFileSystem parent
                    yield fullParent
                    yield! parentsOf fullParent
            }

        normalizedOverlays.Keys
        |> Seq.collect parentsOf
        |> Set.ofSeq

    let matchesSearchPattern (searchPattern: string) (filePath: string) =
        let fileName = Path.GetFileName(filePath)

        match searchPattern with
        | "*.kp" -> filePath.EndsWith(".kp", StringComparison.OrdinalIgnoreCase)
        | _ -> String.Equals(fileName, searchPattern, StringComparison.OrdinalIgnoreCase)

    let isUnderRoot (root: string) (candidate: string) =
        let trimmedRoot = root.TrimEnd(Path.DirectorySeparatorChar, Path.AltDirectorySeparatorChar)
        let prefix = trimmedRoot + string Path.DirectorySeparatorChar

        String.Equals(candidate, trimmedRoot, Workspace.pathComparison)
        || candidate.StartsWith(prefix, Workspace.pathComparison)

    interface IFileSystem with
        member _.GetFullPath(path: string) = Workspace.normalizePath baseFileSystem path

        member _.FileExists(path: string) =
            let normalized = Workspace.normalizePath baseFileSystem path
            normalizedOverlays.ContainsKey(normalized) || baseFileSystem.FileExists(normalized)

        member _.DirectoryExists(path: string) =
            let normalized = Workspace.normalizePath baseFileSystem path
            overlayDirectories.Contains(normalized) || baseFileSystem.DirectoryExists(normalized)

        member _.EnumerateFiles(path: string, searchPattern: string, searchOption: SearchOption) =
            let normalizedRoot = Workspace.normalizePath baseFileSystem path

            seq {
                yield!
                    baseFileSystem.EnumerateFiles(normalizedRoot, searchPattern, searchOption)
                    |> Seq.map (Workspace.normalizePath baseFileSystem)

                yield! normalizedOverlays.Keys
            }
            |> Seq.filter (matchesSearchPattern searchPattern)
            |> Seq.filter (fun filePath ->
                match searchOption with
                | SearchOption.TopDirectoryOnly ->
                    String.Equals(Path.GetDirectoryName(filePath), normalizedRoot, Workspace.pathComparison)
                | SearchOption.AllDirectories ->
                    isUnderRoot normalizedRoot filePath
                | _ ->
                    false)
            |> Seq.distinct

        member _.ReadAllText(path: string) =
            let normalized = Workspace.normalizePath baseFileSystem path

            match normalizedOverlays |> Map.tryFind normalized with
            | Some text -> text
            | None -> baseFileSystem.ReadAllText(normalized)

        member _.ReadAllBytes(path: string) =
            let normalized = Workspace.normalizePath baseFileSystem path

            match normalizedOverlays |> Map.tryFind normalized with
            | Some text -> Encoding.UTF8.GetBytes(text)
            | None -> baseFileSystem.ReadAllBytes(normalized)
