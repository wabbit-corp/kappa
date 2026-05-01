namespace Kappa.Compiler

open System

module StandardLibrarySourceParsing =
    let private aggregateDiagnostics diagnostics =
        diagnostics
        |> List.map (fun diagnostic -> diagnostic.Message)
        |> String.concat Environment.NewLine

    let private parseSourceBackedModule (moduleInfo: StandardLibraryCatalog.SourceBackedStandardLibraryModule) : ParsedDocument =
        let source = SourceText.From(moduleInfo.VirtualPath, moduleInfo.LoadText ())
        let lexed = Lexer.tokenize source

        if not (List.isEmpty lexed.Diagnostics) then
            invalidOp
                $"Bundled stdlib module '{moduleInfo.VirtualPath}' failed to lex for standard-library source parsing:{Environment.NewLine}{aggregateDiagnostics lexed.Diagnostics}"

        let parsed = Parser.parseWithInitialFixities FixityTable.empty source lexed.Tokens

        if not (List.isEmpty parsed.Diagnostics) then
            invalidOp
                $"Bundled stdlib module '{moduleInfo.VirtualPath}' failed to parse for standard-library source parsing:{Environment.NewLine}{aggregateDiagnostics parsed.Diagnostics}"

        { Source = source
          InferredModuleName = None
          Syntax = parsed.Syntax
          Diagnostics = parsed.Diagnostics }

    let private tokenizeTypeText context typeText =
        let source = SourceText.From($"__standard_library_source_parsing__.{context}.kp", typeText)
        let lexed = Lexer.tokenize source

        if not (List.isEmpty lexed.Diagnostics) then
            invalidOp
                $"Standard-library type '{context}' failed to lex for standard-library source parsing:{Environment.NewLine}{aggregateDiagnostics lexed.Diagnostics}"

        lexed.Tokens
        |> List.filter (fun token -> token.Kind <> EndOfFile)
        |> SignatureTokenAnalysis.significantTokens

    let private normalizeTypeTokens (tokens: Token list) =
        SignatureTokenAnalysis.significantTokens tokens

    let private parsedSourceBackedDocumentsByIdentity =
        StandardLibraryCatalog.sourceBackedModules
        |> List.map (fun moduleInfo -> moduleInfo.Surface.ModuleIdentity, lazy (parseSourceBackedModule moduleInfo))
        |> Map.ofList

    let trySourceBackedDeclarations moduleName =
        StandardLibraryCatalog.tryFindBySegments moduleName
        |> Option.bind StandardLibraryCatalog.trySourceBacked
        |> Option.map (fun moduleInfo -> parsedSourceBackedDocumentsByIdentity[moduleInfo.Surface.ModuleIdentity].Value.Syntax.Declarations)

    let tryEffectiveIntrinsicTermNames moduleName =
        match StandardLibraryCatalog.tryFindBySegments moduleName with
        | Some(StandardLibraryCatalog.SourceBacked moduleInfo) when Set.isEmpty moduleInfo.Surface.IntrinsicTermNames ->
            parsedSourceBackedDocumentsByIdentity[moduleInfo.Surface.ModuleIdentity].Value.Syntax.Declarations
            |> List.choose (function
                | ExpectDeclarationNode (ExpectTermDeclaration declaration) -> Some declaration.Name
                | _ -> None)
            |> Set.ofList
            |> Some
        | Some moduleInfo ->
            StandardLibraryCatalog.surface moduleInfo |> _.IntrinsicTermNames |> Some
        | None ->
            None

    let tryIntrinsicTermTypeTokens moduleName termName =
        match StandardLibraryCatalog.tryFindBySegments moduleName with
        | Some(StandardLibraryCatalog.SourceBacked moduleInfo) ->
            moduleInfo.Surface.Terms
            |> List.tryFind (fun term -> String.Equals(term.Name, termName, StringComparison.Ordinal))
            |> Option.map (fun term -> tokenizeTypeText term.Name term.TypeText)
            |> Option.orElseWith (fun () ->
                parsedSourceBackedDocumentsByIdentity[moduleInfo.Surface.ModuleIdentity].Value.Syntax.Declarations
                |> List.tryPick (function
                    | ExpectDeclarationNode (ExpectTermDeclaration declaration)
                        when String.Equals(declaration.Name, termName, StringComparison.Ordinal) ->
                        Some(normalizeTypeTokens declaration.TypeTokens)
                    | _ ->
                        None))
        | Some(StandardLibraryCatalog.Synthetic moduleInfo) ->
            moduleInfo.Surface.Terms
            |> List.tryFind (fun term -> String.Equals(term.Name, termName, StringComparison.Ordinal))
            |> Option.map (fun term -> tokenizeTypeText term.Name term.TypeText)
        | None ->
            None

    let preludeBootstrapFixityDeclarations () =
        let declarations =
            trySourceBackedDeclarations CompilerKnownSymbols.KnownModules.Prelude
            |> Option.defaultWith (fun () -> invalidOp "Bundled prelude must be source-backed.")

        let splitLeadingFixities declarations =
            let rec loop collected remaining =
                match remaining with
                | FixityDeclarationNode declaration :: rest ->
                    loop (declaration :: collected) rest
                | _ ->
                    List.rev collected, remaining

            loop [] declarations

        let leadingFixities, remainingDeclarations = splitLeadingFixities declarations

        if List.isEmpty leadingFixities then
            invalidOp "Bundled prelude must begin with bootstrap fixity declarations."

        if remainingDeclarations |> List.exists (function | FixityDeclarationNode _ -> true | _ -> false) then
            invalidOp "Bundled prelude bootstrap fixity declarations must appear before all other declarations."

        leadingFixities
