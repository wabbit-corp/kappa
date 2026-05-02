namespace Kappa.LSP

open System
open System.Collections.Generic
open System.IO
open System.Text
open Kappa.Compiler

type InitializeResult =
    { ServerName: string
      ServerVersion: string }

type private OpenDocument =
    { Uri: string
      FilePath: string
      Version: int
      Text: string }

type private BindingAtPosition =
    { Name: string
      IsIO: bool }

type private HoverSymbol =
    { Name: string
      QualifiedName: string
      SymbolKind: string
      TypeText: string option
      Origin: KCoreOrigin }

type LanguageServerSession() =
    let mutable rootPath: string option = None
    let openDocuments = Dictionary<string, OpenDocument>(StringComparer.Ordinal)
    let mutable publishedUris = Set.empty<string>
    let mutable shutdownRequested = false

    let buildEffectiveRootPath () =
        match rootPath with
        | Some path -> Some path
        | None ->
            openDocuments.Values
            |> Seq.tryHead
            |> Option.bind (fun document -> Path.GetDirectoryName(document.FilePath) |> Option.ofObj)

    let currentOverlays () =
        openDocuments.Values
        |> Seq.map (fun document -> document.FilePath, document.Text)
        |> Map.ofSeq

    let lspSeverity severity =
        match severity with
        | DiagnosticSeverity.Error -> Some 1
        | DiagnosticSeverity.Warning -> Some 2
        | DiagnosticSeverity.Info -> Some 3

    let lspPosition (position: LinePosition) =
        { Line = max 0 (position.Line - 1)
          Character = max 0 (position.Column - 1) }

    let lspRange (location: SourceLocation) =
        { Start = lspPosition location.Start
          End = lspPosition location.End }

    let lspRangeFromSpan (source: SourceText) span =
        lspRange (source.GetLocation(span))

    let lspRelatedInformation (related: DiagnosticRelatedLocation) : LspDiagnosticRelatedInformation =
        { Location =
            { Uri = Workspace.filePathToUri related.Location.FilePath
              Range = lspRange related.Location }
          Message = related.Message }

    let lspDiagnostic (diagnostic: Diagnostic) (location: SourceLocation) : LspDiagnostic =
        { Range = lspRange location
          Severity = lspSeverity diagnostic.Severity
          Code = Some(DiagnosticCode.toIdentifier diagnostic.Code)
          Source = Some "kappa"
          Message = diagnostic.Message
          RelatedInformation =
            diagnostic.RelatedLocations
            |> List.filter (fun related -> Workspace.isRootedPublishablePath related.Location.FilePath)
            |> List.map lspRelatedInformation }

    let versionByUri () =
        openDocuments.Values
        |> Seq.map (fun document -> document.Uri, document.Version)
        |> Map.ofSeq

    let publishableDiagnosticBuckets (workspace: WorkspaceCompilation) =
        workspace.Diagnostics
        |> List.choose (fun diagnostic ->
            diagnostic.Location
            |> Option.filter (fun location -> Workspace.isRootedPublishablePath location.FilePath)
            |> Option.map (fun location -> location.FilePath, lspDiagnostic diagnostic location))
        |> List.groupBy fst
        |> List.map (fun (filePath, entries) -> filePath, entries |> List.map snd)
        |> Map.ofList

    let rebuildDiagnostics () =
        match buildEffectiveRootPath () with
        | None ->
            let clearUris = publishedUris |> Set.toList
            publishedUris <- Set.empty

            clearUris
            |> List.map (fun uri ->
                { Uri = uri
                  Version = None
                  Diagnostics = [] })
        | Some effectiveRoot ->
            let overlayFileSystem = WorkspaceOverlayFileSystem(FileSystem.defaultImplementation, currentOverlays ())
            let options = CompilationOptions.createWithFileSystem overlayFileSystem effectiveRoot
            let workspace = Compilation.parse options [ effectiveRoot ]
            let diagnosticsByPath = publishableDiagnosticBuckets workspace
            let openDocumentUris = openDocuments.Values |> Seq.map (fun document -> document.Uri) |> Set.ofSeq
            let currentUris =
                diagnosticsByPath.Keys
                |> Seq.map Workspace.filePathToUri
                |> Set.ofSeq
                |> Set.union openDocumentUris

            let urisToPublish =
                Set.union currentUris publishedUris
                |> Set.toList
                |> List.sort

            let versions = versionByUri ()

            publishedUris <- currentUris

            urisToPublish
            |> List.map (fun uri ->
                let diagnostics =
                    uri
                    |> Workspace.tryUriToFilePath
                    |> Option.bind (fun filePath -> diagnosticsByPath |> Map.tryFind filePath)
                    |> Option.defaultValue []

                { Uri = uri
                  Version = versions |> Map.tryFind uri
                  Diagnostics = diagnostics })

    let currentWorkspace () =
        buildEffectiveRootPath ()
        |> Option.map (fun effectiveRoot ->
            let overlayFileSystem = WorkspaceOverlayFileSystem(FileSystem.defaultImplementation, currentOverlays ())
            let options = CompilationOptions.createWithFileSystem overlayFileSystem effectiveRoot
            Compilation.parse options [ effectiveRoot ])

    let sourceIndexFromPosition (source: SourceText) (position: LspPosition) =
        let boundedLine = Math.Clamp(position.Line, 0, max 0 (source.LineCount - 1))
        let lineStart = source.LineStarts[boundedLine]
        let lineText = source.GetLineText(boundedLine)
        let boundedCharacter = Math.Clamp(position.Character, 0, lineText.Length)
        lineStart + boundedCharacter

    let tokenSpanBounds (source: SourceText) (tokens: Token list) =
        match tokens with
        | [] -> None
        | _ ->
            let firstTokenStart =
                tokens
                |> List.map (fun token -> token.Span.Start)
                |> List.min

            let startLine =
                source.GetLocation(TextSpan.FromBounds(firstTokenStart, firstTokenStart)).Start.Line - 1
                |> fun line -> Math.Clamp(line, 0, max 0 (source.LineCount - 1))

            let startIndex = source.LineStarts[startLine]

            let endIndex =
                tokens
                |> List.map (fun token -> token.Span.End)
                |> List.max

            Some(TextSpan.FromBounds(startIndex, endIndex))

    let letDefinitionSpan source (definition: LetDefinition) =
        definition.HeaderTokens
        @ definition.BodyTokens
        @ (definition.ReturnTypeTokens |> Option.defaultValue [])
        |> tokenSpanBounds source

    let signatureSpan source (signature: BindingSignature) =
        signature.TypeTokens |> tokenSpanBounds source

    let isIoTypeTokens tokens =
        tokens
        |> TypeSignatures.parseScheme
        |> Option.bind (fun scheme ->
            match scheme.Body with
            | TypeSignatures.TypeName(typeReference, arguments)
                when not (List.isEmpty arguments)
                     && TypeSignatures.matchesKnownTypeName CompilerKnownSymbols.IOType typeReference ->
                Some true
            | TypeSignatures.TypeName(typeReference, [ _ ])
                when TypeSignatures.matchesKnownTypeName CompilerKnownSymbols.UIOType typeReference ->
                Some true
            | _ ->
                Some false)
        |> Option.defaultValue false

    let spanContains index (span: TextSpan) =
        index >= span.Start && index <= span.End

    let spanStrictlyContains index (span: TextSpan) =
        index >= span.Start && index < span.End

    let isHoverableToken (token: Token) =
        match token.Kind with
        | Identifier
        | Operator ->
            not (String.IsNullOrWhiteSpace(token.Text))
        | _ ->
            false

    let tryFindTokenAtPosition (workspace: WorkspaceCompilation) uri position : (ParsedDocument * Token) option =
        Workspace.tryUriToFilePath uri
        |> Option.bind (fun filePath ->
            workspace.Documents
            |> List.tryFind (fun document -> String.Equals(document.Source.FilePath, filePath, Workspace.pathComparison)))
        |> Option.bind (fun document ->
            let index = sourceIndexFromPosition document.Source position

            document.Syntax.Tokens
            |> List.filter isHoverableToken
            |> List.tryFind (fun token -> spanStrictlyContains index token.Span)
            |> Option.map (fun token -> document, token))

    let bindingTypeText (binding: KCoreBinding) =
        let parameterTypes =
            binding.Parameters
            |> List.map (fun parameter ->
                let quantityPrefix =
                    parameter.Quantity
                    |> Option.map Quantity.toSurfaceText
                    |> Option.map (fun quantity -> $"{quantity} ")
                    |> Option.defaultValue ""

                parameter.TypeText
                |> Option.orElseWith (fun () -> parameter.Type |> Option.map TypeSignatures.toText)
                |> Option.map (fun typeText -> quantityPrefix + typeText)
                |> Option.defaultValue (quantityPrefix + "?"))

        let returnType =
            binding.ReturnTypeText
            |> Option.orElseWith (fun () -> binding.ReturnType |> Option.map TypeSignatures.toText)

        match parameterTypes, returnType with
        | [], Some returnType -> Some returnType
        | _ :: _, Some returnType -> Some(String.concat " -> " (parameterTypes @ [ returnType ]))
        | [], None -> None
        | _ :: _, None -> Some(String.concat " -> " parameterTypes)

    let hoverSymbols (workspace: WorkspaceCompilation) =
        workspace.KCore
        |> List.collect (fun coreModule ->
            coreModule.Declarations
            |> List.choose (fun declaration ->
                declaration.Binding
                |> Option.bind (fun binding ->
                    binding.Name
                    |> Option.map (fun name ->
                        let kind =
                            if List.isEmpty binding.Parameters then
                                "value"
                            else
                                "function"

                        { Name = name
                          QualifiedName = $"{coreModule.Name}.{name}"
                          SymbolKind = kind
                          TypeText = bindingTypeText binding
                          Origin = binding.Provenance }))))

    let resolveHoverSymbol (workspace: WorkspaceCompilation) (document: ParsedDocument) (token: Token) =
        let name = SyntaxFacts.trimIdentifierQuotes token.Text
        let currentModuleName = document.ModuleName |> Option.map SyntaxFacts.moduleNameToText
        let symbols = hoverSymbols workspace

        let currentModuleMatches =
            currentModuleName
            |> Option.map (fun moduleName ->
                symbols
                |> List.filter (fun symbol -> symbol.Name = name && KCoreOrigin.moduleNameText symbol.Origin = moduleName))
            |> Option.defaultValue []

        match currentModuleMatches with
        | symbol :: _ -> Some symbol
        | [] ->
            let preludeMatches =
                symbols
                |> List.filter (fun symbol -> symbol.Name = name && KCoreOrigin.moduleNameText symbol.Origin = Stdlib.PreludeModuleText)

            match preludeMatches with
            | symbol :: _ -> Some symbol
            | [] ->
                symbols
                |> List.filter (fun symbol -> symbol.Name = name)
                |> List.distinctBy (fun symbol -> symbol.QualifiedName)
                |> function
                    | [ symbol ] -> Some symbol
                    | _ -> None

    let markdownEscape (text: string) =
        text.Replace("\\", "\\\\").Replace("`", "\\`")

    let hoverMarkdown (symbol: HoverSymbol) =
        let signatureLine =
            match symbol.TypeText with
            | Some typeText -> $"{symbol.Name} : {typeText}"
            | None -> symbol.Name

        [
            "```kappa"
            signatureLine
            "```"
            $"kind: {symbol.SymbolKind}"
            $"qualified: `{markdownEscape symbol.QualifiedName}`"
            $"module: `{markdownEscape (KCoreOrigin.moduleNameText symbol.Origin)}`"
            $"origin: `{markdownEscape symbol.Origin.IntroductionKind}`"
            $"file: `{markdownEscape symbol.Origin.FilePath}`"
        ]
        |> String.concat "\n"

    let tryFindBindingAtPosition (workspace: WorkspaceCompilation) uri position =
        Workspace.tryUriToFilePath uri
        |> Option.bind (fun filePath ->
            workspace.Documents
            |> List.tryFind (fun document -> String.Equals(document.Source.FilePath, filePath, Workspace.pathComparison)))
        |> Option.bind (fun document ->
            let index = sourceIndexFromPosition document.Source position

            let signatureTypes =
                document.Syntax.Declarations
                |> List.choose (function
                    | SignatureDeclaration signature -> Some(signature.Name, signature.TypeTokens)
                    | _ -> None)
                |> Map.ofList

            let letCandidates =
                document.Syntax.Declarations
                |> List.choose (function
                    | LetDeclaration definition ->
                        definition.Name
                        |> Option.bind (fun name ->
                            letDefinitionSpan document.Source definition
                            |> Option.filter (spanContains index)
                            |> Option.map (fun span ->
                                let isIO =
                                    definition.ReturnTypeTokens
                                    |> Option.orElseWith (fun () -> signatureTypes |> Map.tryFind name)
                                    |> Option.exists isIoTypeTokens

                                { Name = name
                                  IsIO = isIO },
                                span))
                    | _ -> None)

            let signatureCandidates =
                document.Syntax.Declarations
                |> List.choose (function
                    | SignatureDeclaration signature ->
                        signatureSpan document.Source signature
                        |> Option.filter (spanContains index)
                        |> Option.map (fun span ->
                            { Name = signature.Name
                              IsIO = isIoTypeTokens signature.TypeTokens },
                            span)
                    | _ -> None)

            let candidates =
                letCandidates @ signatureCandidates
                |> List.sortBy (fun (_, span) -> span.Length)

            candidates
            |> List.tryHead
            |> Option.map (fun (candidate, _) ->
                let modulePrefix =
                    document.ModuleName
                    |> Option.map SyntaxFacts.moduleNameToText

                let qualifiedName =
                    match modulePrefix with
                    | Some moduleName -> $"{moduleName}.{candidate.Name}"
                    | None -> candidate.Name

                { candidate with Name = qualifiedName }))

    let captureOutput () =
        let builder = StringBuilder()

        let append (text: string) =
            builder.Append(text) |> ignore

        let appendLine (text: string) =
            builder.AppendLine(text) |> ignore

        { Write = append
          WriteLine = appendLine },
        fun () -> builder.ToString().Replace("\r\n", "\n")

    let successfulInterpret binding mode value output =
        { Success = true
          Binding = Some binding
          Mode = Some mode
          Value = value
          Output =
            if String.IsNullOrEmpty(output) then None else Some output
          Error = None }

    let failedInterpret message =
        { Success = false
          Binding = None
          Mode = None
          Value = None
          Output = None
          Error = Some message }

    member _.Initialize(rootUri: string option, workspaceFolderUris: string list) : InitializeResult =
        rootPath <- Workspace.rootPathFromWorkspace rootUri workspaceFolderUris

        { ServerName = "Kappa.LSP"
          ServerVersion = "0.1.0" }

    member _.DidOpen(uri: string, version: int, text: string) : PublishDiagnosticsParams list =
        match Workspace.tryUriToFilePath uri with
        | Some filePath ->
            openDocuments[uri] <-
                { Uri = uri
                  FilePath = filePath
                  Version = version
                  Text = text.Replace("\r\n", "\n") }

            rebuildDiagnostics ()
        | None ->
            []

    member _.DidChange(uri: string, version: int, text: string) : PublishDiagnosticsParams list =
        match Workspace.tryUriToFilePath uri with
        | Some filePath ->
            openDocuments[uri] <-
                { Uri = uri
                  FilePath = filePath
                  Version = version
                  Text = text.Replace("\r\n", "\n") }

            rebuildDiagnostics ()
        | None ->
            []

    member _.DidClose(uri: string) : PublishDiagnosticsParams list =
        openDocuments.Remove(uri) |> ignore
        rebuildDiagnostics ()

    member _.DidChangeWatchedFiles() : PublishDiagnosticsParams list =
        rebuildDiagnostics ()

    member _.Hover(uri: string, position: LspPosition) : Hover option =
        match currentWorkspace () with
        | None ->
            None
        | Some workspace when workspace.HasErrors ->
            None
        | Some workspace ->
            tryFindTokenAtPosition workspace uri position
            |> Option.bind (fun ((document: ParsedDocument), (token: Token)) ->
                resolveHoverSymbol workspace document token
                |> Option.map (fun symbol ->
                    { Contents =
                        { Kind = "markdown"
                          Value = hoverMarkdown symbol }
                      Range = Some(lspRangeFromSpan document.Source token.Span) }))

    member _.InterpretAtPosition(uri: string, position: LspPosition) : InterpretAtPositionResult =
        match currentWorkspace () with
        | None ->
            failedInterpret "No workspace root is available for interpretation."
        | Some workspace when workspace.HasErrors ->
            failedInterpret "Cannot interpret because the workspace has diagnostics."
        | Some workspace ->
            match tryFindBindingAtPosition workspace uri position with
            | None ->
                failedInterpret "No top-level function or value was found at the cursor."
            | Some binding when binding.IsIO ->
                let output, readOutput = captureOutput ()

                match Interpreter.executeBindingWithOutput workspace output binding.Name with
                | Result.Ok value ->
                    let valueText =
                        if Interpreter.shouldPrintResult value then
                            Some(RuntimeValue.format value)
                        else
                            None

                    successfulInterpret binding.Name "io" valueText (readOutput ())
                | Result.Error issue ->
                    failedInterpret issue.Message
            | Some binding ->
                match Interpreter.evaluateBinding workspace binding.Name with
                | Result.Error issue ->
                    failedInterpret issue.Message
                | Result.Ok(RuntimeValue.IOActionValue _) ->
                    let output, readOutput = captureOutput ()

                    match Interpreter.executeBindingWithOutput workspace output binding.Name with
                    | Result.Ok value ->
                        let valueText =
                            if Interpreter.shouldPrintResult value then
                                Some(RuntimeValue.format value)
                            else
                                None

                        successfulInterpret binding.Name "io" valueText (readOutput ())
                    | Result.Error issue ->
                        failedInterpret issue.Message
                | Result.Ok value ->
                    successfulInterpret binding.Name "pure" (Some(RuntimeValue.format value)) ""

    member _.Shutdown() =
        shutdownRequested <- true

    member _.ExitCode =
        if shutdownRequested then 0 else 1
