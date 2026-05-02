namespace Kappa.LSP

open System
open System.IO
open System.Text
open System.Text.Json
open System.Text.Json.Serialization

type LspPosition =
    { [<JsonPropertyName("line")>]
      Line: int
      [<JsonPropertyName("character")>]
      Character: int }

type LspRange =
    { [<JsonPropertyName("start")>]
      Start: LspPosition
      [<JsonPropertyName("end")>]
      End: LspPosition }

type LspLocation =
    { [<JsonPropertyName("uri")>]
      Uri: string
      [<JsonPropertyName("range")>]
      Range: LspRange }

type LspDiagnosticRelatedInformation =
    { [<JsonPropertyName("location")>]
      Location: LspLocation
      [<JsonPropertyName("message")>]
      Message: string }

type LspDiagnostic =
    { [<JsonPropertyName("range")>]
      Range: LspRange
      [<JsonPropertyName("severity")>]
      Severity: int option
      [<JsonPropertyName("code")>]
      Code: string option
      [<JsonPropertyName("source")>]
      Source: string option
      [<JsonPropertyName("message")>]
      Message: string
      [<JsonPropertyName("relatedInformation")>]
      RelatedInformation: LspDiagnosticRelatedInformation list }

type PublishDiagnosticsParams =
    { [<JsonPropertyName("uri")>]
      Uri: string
      [<JsonPropertyName("version")>]
      Version: int option
      [<JsonPropertyName("diagnostics")>]
      Diagnostics: LspDiagnostic list }

type TextDocumentIdentifier =
    { [<JsonPropertyName("uri")>]
      Uri: string }

type TextDocumentPositionParams =
    { [<JsonPropertyName("textDocument")>]
      TextDocument: TextDocumentIdentifier
      [<JsonPropertyName("position")>]
      Position: LspPosition }

type MarkupContent =
    { [<JsonPropertyName("kind")>]
      Kind: string
      [<JsonPropertyName("value")>]
      Value: string }

type Hover =
    { [<JsonPropertyName("contents")>]
      Contents: MarkupContent
      [<JsonPropertyName("range")>]
      Range: LspRange option }

type InterpretAtPositionTextDocument =
    { [<JsonPropertyName("uri")>]
      Uri: string }

type InterpretAtPositionParams =
    { [<JsonPropertyName("textDocument")>]
      TextDocument: InterpretAtPositionTextDocument
      [<JsonPropertyName("position")>]
      Position: LspPosition }

type InterpretAtPositionResult =
    { [<JsonPropertyName("success")>]
      Success: bool
      [<JsonPropertyName("binding")>]
      Binding: string option
      [<JsonPropertyName("mode")>]
      Mode: string option
      [<JsonPropertyName("value")>]
      Value: string option
      [<JsonPropertyName("output")>]
      Output: string option
      [<JsonPropertyName("error")>]
      Error: string option }

type JsonRpcMessage =
    | Request of id: JsonElement * methodName: string * parameters: JsonElement option
    | Notification of methodName: string * parameters: JsonElement option

module JsonRpc =
    let serializerOptions =
        JsonSerializerOptions(DefaultIgnoreCondition = JsonIgnoreCondition.WhenWritingNull)

    let private tryGetProperty (name: string) (element: JsonElement) =
        let mutable value = Unchecked.defaultof<JsonElement>

        if element.TryGetProperty(name, &value) then
            Some value
        else
            None

    let private readHeaderLine (stream: Stream) =
        let builder = StringBuilder()

        let rec loop () =
            let next = stream.ReadByte()

            if next = -1 then
                if builder.Length = 0 then
                    None
                else
                    invalidOp "Unexpected end of stream while reading JSON-RPC headers."
            else
                let ch = char next

                if ch = '\r' then
                    loop ()
                elif ch = '\n' then
                    Some(builder.ToString())
                else
                    builder.Append(ch) |> ignore
                    loop ()

        loop ()

    let private readHeaders (stream: Stream) =
        let rec loop (headers: Map<string, string>) =
            match readHeaderLine stream with
            | None ->
                None
            | Some "" ->
                Some headers
            | Some line ->
                let separatorIndex = line.IndexOf(':')

                if separatorIndex < 0 then
                    invalidOp $"Malformed JSON-RPC header '{line}'."
                else
                    let name = line.Substring(0, separatorIndex).Trim().ToLowerInvariant()
                    let value = line.Substring(separatorIndex + 1).Trim()
                    loop (headers |> Map.add name value)

        loop Map.empty

    let private readExactBytes (stream: Stream) (count: int) =
        let buffer = Array.zeroCreate<byte> count
        let mutable offset = 0

        while offset < count do
            let read = stream.Read(buffer, offset, count - offset)

            if read = 0 then
                invalidOp "Unexpected end of stream while reading JSON-RPC payload."

            offset <- offset + read

        buffer

    let tryReadMessage (stream: Stream) =
        match readHeaders stream with
        | None ->
            None
        | Some headers ->
            let contentLength =
                headers
                |> Map.tryFind "content-length"
                |> Option.defaultWith (fun () -> invalidOp "Missing Content-Length header.")
                |> int

            let payload = readExactBytes stream contentLength
            let document = JsonDocument.Parse(payload)
            let root = document.RootElement
            let methodName = root.GetProperty("method").GetString()

            if String.IsNullOrWhiteSpace(methodName) then
                invalidOp "JSON-RPC message did not specify a method."

            match tryGetProperty "id" root with
            | Some id -> Some(Request(id, methodName, tryGetProperty "params" root), document)
            | None -> Some(Notification(methodName, tryGetProperty "params" root), document)

    let private writePayload (stream: Stream) (payload: byte array) =
        let header = Encoding.ASCII.GetBytes($"Content-Length: {payload.Length}\r\n\r\n")
        stream.Write(header, 0, header.Length)
        stream.Write(payload, 0, payload.Length)
        stream.Flush()

    let writeMessage (stream: Stream) (value: 'T) =
        let payload = JsonSerializer.SerializeToUtf8Bytes(value, serializerOptions)
        writePayload stream payload

    let writeRawJson (stream: Stream) (jsonText: string) =
        Encoding.UTF8.GetBytes(jsonText)
        |> writePayload stream
