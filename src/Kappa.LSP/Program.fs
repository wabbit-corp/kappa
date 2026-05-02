open System
open System.IO
open System.Text.Json
open Kappa.LSP

let private usage () =
    "kappa-lsp --stdio"

let private tryGetProperty (name: string) (element: JsonElement) =
    let mutable value = Unchecked.defaultof<JsonElement>

    if element.TryGetProperty(name, &value) then
        Some value
    else
        None

let private tryGetStringProperty name element =
    tryGetProperty name element |> Option.bind (fun value -> value.GetString() |> Option.ofObj)

let private tryGetIntProperty name element =
    tryGetProperty name element
    |> Option.bind (fun value ->
        match value.ValueKind with
        | JsonValueKind.Number ->
            match value.TryGetInt32() with
            | true, number -> Some number
            | _ -> None
        | _ ->
            None)

let private tryGetArrayProperty name element =
    tryGetProperty name element
    |> Option.filter (fun value -> value.ValueKind = JsonValueKind.Array)

let private publishDiagnostics (output: Stream) diagnostics =
    diagnostics
    |> List.iter (fun publish ->
        JsonRpc.writeMessage
            output
            {| jsonrpc = "2.0"
               method = "textDocument/publishDiagnostics"
               ``params`` = publish |})

let private handleInitialize (session: LanguageServerSession) (requestId: JsonElement) (parameters: JsonElement option) =
    let rootUri =
        parameters
        |> Option.bind (tryGetStringProperty "rootUri")

    let workspaceFolderUris =
        parameters
        |> Option.bind (tryGetArrayProperty "workspaceFolders")
        |> Option.map (fun folders ->
            folders.EnumerateArray()
            |> Seq.choose (tryGetStringProperty "uri")
            |> Seq.toList)
        |> Option.defaultValue []

    let result = session.Initialize(rootUri, workspaceFolderUris)

    {| jsonrpc = "2.0"
       id = requestId
       result =
        {| capabilities =
            {| textDocumentSync = 1
               hoverProvider = true
               definitionProvider = false
               referencesProvider = false |}
           serverInfo =
            {| name = result.ServerName
               version = result.ServerVersion |} |} |}

let private extractTextDocument (parameters: JsonElement) =
    parameters
    |> tryGetProperty "textDocument"
    |> Option.defaultWith (fun () -> invalidOp "Missing textDocument payload.")

let private handleDidOpen (session: LanguageServerSession) (parameters: JsonElement) =
    let document = extractTextDocument parameters
    let uri = document.GetProperty("uri").GetString()
    let version = tryGetIntProperty "version" document |> Option.defaultValue 0
    let text = document.GetProperty("text").GetString()

    if isNull uri || isNull text then
        []
    else
        session.DidOpen(uri, version, text)

let private handleDidChange (session: LanguageServerSession) (parameters: JsonElement) =
    let document = extractTextDocument parameters
    let changes =
        parameters
        |> tryGetArrayProperty "contentChanges"
        |> Option.defaultWith (fun () -> invalidOp "Missing contentChanges payload.")
        |> fun values -> values.EnumerateArray() |> Seq.toList

    let uri = document.GetProperty("uri").GetString()
    let version = tryGetIntProperty "version" document |> Option.defaultValue 0

    let latestText =
        changes
        |> List.tryLast
        |> Option.bind (tryGetStringProperty "text")

    match uri, latestText with
    | null, _
    | _, None ->
        []
    | _, Some text ->
        session.DidChange(uri, version, text)

let private handleDidClose (session: LanguageServerSession) (parameters: JsonElement) =
    let document = extractTextDocument parameters
    let uri = document.GetProperty("uri").GetString()

    if isNull uri then
        []
    else
        session.DidClose(uri)

let private parseInterpretAtPositionParams (parameters: JsonElement option) =
    parameters
    |> Option.map (fun value -> JsonSerializer.Deserialize<InterpretAtPositionParams>(value.GetRawText(), JsonRpc.serializerOptions))
    |> Option.bind Option.ofObj
    |> Option.defaultWith (fun () -> invalidOp "Missing kappa/interpretAtPosition parameters.")

let private parseTextDocumentPositionParams (parameters: JsonElement option) =
    parameters
    |> Option.map (fun value -> JsonSerializer.Deserialize<TextDocumentPositionParams>(value.GetRawText(), JsonRpc.serializerOptions))
    |> Option.bind Option.ofObj
    |> Option.defaultWith (fun () -> invalidOp "Missing text document position parameters.")

let private handleRequest (session: LanguageServerSession) (output: Stream) requestId methodName parameters =
    match methodName with
    | "initialize" ->
        JsonRpc.writeMessage output (handleInitialize session requestId parameters)
        true
    | "shutdown" ->
        session.Shutdown()
        let requestIdJson = requestId.GetRawText()
        JsonRpc.writeRawJson output $"{{\"jsonrpc\":\"2.0\",\"id\":{requestIdJson},\"result\":null}}"
        true
    | "textDocument/hover" ->
        let hoverParams = parseTextDocumentPositionParams parameters

        match session.Hover(hoverParams.TextDocument.Uri, hoverParams.Position) with
        | Some hover ->
            JsonRpc.writeMessage
                output
                {| jsonrpc = "2.0"
                   id = requestId
                   result = hover |}
        | None ->
            let requestIdJson = requestId.GetRawText()
            JsonRpc.writeRawJson output $"{{\"jsonrpc\":\"2.0\",\"id\":{requestIdJson},\"result\":null}}"

        true
    | "kappa/interpretAtPosition" ->
        let interpretParams = parseInterpretAtPositionParams parameters
        let result = session.InterpretAtPosition(interpretParams.TextDocument.Uri, interpretParams.Position)

        JsonRpc.writeMessage
            output
            {| jsonrpc = "2.0"
               id = requestId
               result = result |}

        true
    | _ ->
        JsonRpc.writeMessage
            output
            {| jsonrpc = "2.0"
               id = requestId
               error =
                {| code = -32601
                   message = $"Method '{methodName}' is not supported." |} |}

        true

let private handleNotification (session: LanguageServerSession) (output: Stream) methodName parameters =
    let diagnostics =
        match methodName, parameters with
        | "initialized", _ -> []
        | "textDocument/didOpen", Some value -> handleDidOpen session value
        | "textDocument/didChange", Some value -> handleDidChange session value
        | "textDocument/didClose", Some value -> handleDidClose session value
        | "workspace/didChangeWatchedFiles", _ -> session.DidChangeWatchedFiles()
        | "exit", _ -> raise (OperationCanceledException())
        | _ -> []

    publishDiagnostics output diagnostics

[<EntryPoint>]
let main argv =
    if argv.Length <> 1 || argv[0] <> "--stdio" then
        Console.Error.WriteLine(usage ())
        1
    else
        let session = LanguageServerSession()
        use input = Console.OpenStandardInput()
        use output = Console.OpenStandardOutput()

        try
            let mutable running = true

            while running do
                match JsonRpc.tryReadMessage input with
                | None ->
                    running <- false
                | Some(Request(id, methodName, parameters), document) ->
                    use _document = document
                    ignore (handleRequest session output id methodName parameters)
                | Some(Notification(methodName, parameters), document) ->
                    use _document = document

                    try
                        handleNotification session output methodName parameters
                    with :? OperationCanceledException ->
                        running <- false

            session.ExitCode
        with ex ->
            Console.Error.WriteLine(ex.Message)
            1
