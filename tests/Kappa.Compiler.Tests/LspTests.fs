module LspTests

open System
open System.IO
open Kappa.Compiler
open Kappa.LSP
open HarnessSupport
open Xunit

let private workspaceRootUri (root: string) =
    Uri(root + string Path.DirectorySeparatorChar).AbsoluteUri

let private fileUri (path: string) =
    Uri(path).AbsoluteUri

[<Fact>]
let ``file URI conversion accepts VS Code Windows drive URI shape`` () =
    let actual = Workspace.tryUriToFilePath "file:///d%3A/ws/kapper"

    if OperatingSystem.IsWindows() then
        Assert.Equal(Some @"d:\ws\kapper", actual |> Option.map (fun path -> path.ToLowerInvariant()))
    else
        Assert.True(actual.IsSome)

[<Fact>]
let ``overlay filesystem shadows on-disk sources during compilation`` () =
    let root = createScratchDirectory "lsp-overlay-shadow"

    writeWorkspaceFiles
        root
        [
            "main.kp",
            [
                "module main"
                "value : Int"
            ]
            |> String.concat "\n"
        ]

    let mainPath = Path.Combine(root, "main.kp")
    let overlayText =
        [
            "module main"
            "value : Int"
            "let value = 1"
        ]
        |> String.concat "\n"

    let overlayFileSystem =
        WorkspaceOverlayFileSystem(
            FileSystem.defaultImplementation,
            Map.ofList [ Path.GetFullPath(mainPath), overlayText ]
        )

    let workspace =
        Compilation.parse
            (CompilationOptions.createWithFileSystem overlayFileSystem root)
            [ root ]

    Assert.False(workspace.HasErrors, sprintf "Expected overlay content to be compiled instead of on-disk content, got %A" workspace.Diagnostics)

[<Fact>]
let ``language server publishes diagnostics for opened buffers and clears them after a valid change`` () =
    let root = createScratchDirectory "lsp-diagnostics-change"

    writeWorkspaceFiles
        root
        [
            "main.kp",
            [
                "module main"
                "value : Int"
                "let value = 1"
            ]
            |> String.concat "\n"
        ]

    let mainPath = Path.Combine(root, "main.kp")
    let mainUri = fileUri mainPath
    let session = LanguageServerSession()

    session.Initialize(Some(workspaceRootUri root), []) |> ignore

    let invalidText =
        [
            "module main"
            "value : Int"
        ]
        |> String.concat "\n"

    let openPublishes = session.DidOpen(mainUri, 1, invalidText)
    let openDiagnostics = openPublishes |> List.find (fun publish -> publish.Uri = mainUri)

    Assert.NotEmpty(openDiagnostics.Diagnostics)
    Assert.Contains(openDiagnostics.Diagnostics, fun diagnostic -> diagnostic.Code = Some "E_SIGNATURE_UNSATISFIED")

    let validText =
        [
            "module main"
            "value : Int"
            "let value = 1"
        ]
        |> String.concat "\n"

    let changedPublishes = session.DidChange(mainUri, 2, validText)
    let changedDiagnostics = changedPublishes |> List.find (fun publish -> publish.Uri = mainUri)

    Assert.Empty(changedDiagnostics.Diagnostics)

[<Fact>]
let ``language server restores on-disk content after a buffer closes`` () =
    let root = createScratchDirectory "lsp-diagnostics-close"

    writeWorkspaceFiles
        root
        [
            "main.kp",
            [
                "module main"
                "value : Int"
                "let value = 1"
            ]
            |> String.concat "\n"
        ]

    let mainPath = Path.Combine(root, "main.kp")
    let mainUri = fileUri mainPath
    let session = LanguageServerSession()

    session.Initialize(Some(workspaceRootUri root), []) |> ignore

    let invalidText =
        [
            "module main"
            "value : Int"
        ]
        |> String.concat "\n"

    session.DidOpen(mainUri, 1, invalidText) |> ignore

    let closePublishes = session.DidClose(mainUri)
    let closeDiagnostics = closePublishes |> List.find (fun publish -> publish.Uri = mainUri)

    Assert.Empty(closeDiagnostics.Diagnostics)

[<Fact>]
let ``language server interprets pure binding at cursor`` () =
    let root = createScratchDirectory "lsp-interpret-pure"

    writeWorkspaceFiles
        root
        [
            "main.kp",
            [
                "module main"
                "value : Int"
                "let value = 40 + 2"
            ]
            |> String.concat "\n"
        ]

    let mainPath = Path.Combine(root, "main.kp")
    let mainUri = fileUri mainPath
    let session = LanguageServerSession()
    let text = File.ReadAllText(mainPath).Replace("\r\n", "\n")

    session.Initialize(Some(workspaceRootUri root), []) |> ignore
    session.DidOpen(mainUri, 1, text) |> ignore

    let result =
        session.InterpretAtPosition(
            mainUri,
            { Line = 2
              Character = 5 }
        )

    Assert.True(result.Success, result.Error |> Option.defaultValue "Expected interpretation to succeed.")
    Assert.Equal(Some "main.value", result.Binding)
    Assert.Equal(Some "pure", result.Mode)
    Assert.Equal(Some "42", result.Value)
    Assert.Equal(None, result.Output)

[<Fact>]
let ``language server runs io binding at cursor to completion`` () =
    let root = createScratchDirectory "lsp-interpret-io"

    writeWorkspaceFiles
        root
        [
            "main.kp",
            [
                "module main"
                "let main : UIO Unit = do"
                "    printlnString \"hello\""
            ]
            |> String.concat "\n"
        ]

    let mainPath = Path.Combine(root, "main.kp")
    let mainUri = fileUri mainPath
    let session = LanguageServerSession()
    let text = File.ReadAllText(mainPath).Replace("\r\n", "\n")

    session.Initialize(Some(workspaceRootUri root), []) |> ignore
    session.DidOpen(mainUri, 1, text) |> ignore

    let result =
        session.InterpretAtPosition(
            mainUri,
            { Line = 2
              Character = 8 }
        )

    Assert.True(result.Success, result.Error |> Option.defaultValue "Expected interpretation to succeed.")
    Assert.Equal(Some "main.main", result.Binding)
    Assert.Equal(Some "io", result.Mode)
    Assert.Equal(None, result.Value)
    Assert.Equal(Some "hello\n", result.Output)

[<Fact>]
let ``language server returns hover information for a top-level symbol`` () =
    let root = createScratchDirectory "lsp-hover-symbol"

    writeWorkspaceFiles
        root
        [
            "main.kp",
            [
                "module main"
                "value : Int"
                "let value = 42"
            ]
            |> String.concat "\n"
        ]

    let mainPath = Path.Combine(root, "main.kp")
    let mainUri = fileUri mainPath
    let session = LanguageServerSession()
    let text = File.ReadAllText(mainPath).Replace("\r\n", "\n")

    session.Initialize(Some(workspaceRootUri root), []) |> ignore
    session.DidOpen(mainUri, 1, text) |> ignore

    let hover =
        session.Hover(
            mainUri,
            { Line = 2
              Character = 5 }
        )

    let value =
        match hover with
        | Some hover -> hover.Contents.Value
        | None -> failwith "Expected hover information for value."

    Assert.Contains("value : Int", value)
    Assert.Contains("kind: value", value)
    Assert.Contains("qualified: `main.value`", value)
    Assert.Contains("module: `main`", value)
    Assert.Contains("origin: `source`", value)
