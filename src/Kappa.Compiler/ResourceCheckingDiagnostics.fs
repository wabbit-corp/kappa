namespace Kappa.Compiler

open System
open Kappa.Compiler.ResourceModel

// Builds source locations and diagnostic payloads for resource-checker results.
module internal ResourceCheckingDiagnostics =
    let linearDropCode = "E_QTT_LINEAR_DROP"
    let linearOveruseCode = "E_QTT_LINEAR_OVERUSE"
    let borrowConsumeCode = "E_QTT_BORROW_CONSUME"
    let borrowEscapeCode = "E_QTT_BORROW_ESCAPE"
    let inoutMarkerRequiredCode = "E_QTT_INOUT_MARKER_REQUIRED"
    let inoutMarkerUnexpectedCode = "E_QTT_INOUT_MARKER_UNEXPECTED"

    let diagnosticLocation (document: ParsedDocument) =
        document.Source.GetLocation(TextSpan.FromBounds(0, 0))

    let private tokenName (token: Token) =
        match token.Kind with
        | Identifier
        | Keyword _ ->
            Some(SyntaxFacts.trimIdentifierQuotes token.Text)
        | Operator ->
            Some token.Text
        | _ ->
            None

    let private tokenMatchesName name (token: Token) =
        tokenName token
        |> Option.exists (fun tokenName -> String.Equals(tokenName, name, StringComparison.Ordinal))

    let private tokenLocation (document: ParsedDocument) (token: Token) =
        document.Source.GetLocation(token.Span)

    let private tokenLine (document: ParsedDocument) (token: Token) =
        (tokenLocation document token).Start.Line

    let private tokensByLine (document: ParsedDocument) =
        document.Syntax.Tokens
        |> List.filter (fun token ->
            match token.Kind with
            | Newline
            | Indent
            | Dedent
            | EndOfFile -> false
            | _ -> true)
        |> List.groupBy (tokenLine document)
        |> Map.ofList

    let private isAssignmentBoundary (token: Token) =
        token.Kind = Equals || (token.Kind = Operator && token.Text = "<-")

    let private binderPrefixTokens lineTokens =
        let rec takeUntilBoundary tokens collected =
            match tokens with
            | [] ->
                List.rev collected
            | token :: _ when isAssignmentBoundary token ->
                List.rev collected
            | token :: rest ->
                takeUntilBoundary rest (token :: collected)

        match lineTokens with
        | first :: _ when Token.isKeyword Keyword.Let first ->
            takeUntilBoundary lineTokens []
        | first :: _ when Token.isKeyword Keyword.Using first ->
            takeUntilBoundary lineTokens []
        | first :: _ when Token.isKeyword Keyword.Var first ->
            takeUntilBoundary lineTokens []
        | _ ->
            []

    let private isBinderToken lineMap (document: ParsedDocument) token =
        let lineTokens =
            lineMap
            |> Map.tryFind (tokenLine document token)
            |> Option.defaultValue []

        binderPrefixTokens lineTokens
        |> List.exists (fun candidate -> candidate.Span = token.Span)

    let private tryFindBinderLocation (document: ParsedDocument) name =
        let lineMap = tokensByLine document

        document.Syntax.Tokens
        |> List.tryPick (fun token ->
            if tokenMatchesName name token && isBinderToken lineMap document token then
                Some(tokenLocation document token)
            else
                None)

    let private useLocations (document: ParsedDocument) name =
        let lineMap = tokensByLine document

        document.Syntax.Tokens
        |> List.choose (fun token ->
            if tokenMatchesName name token && not (isBinderToken lineMap document token) then
                Some(tokenLocation document token)
            else
                None)

    let findUseLocation (document: ParsedDocument) name ordinal =
        useLocations document name
        |> List.tryItem (max 0 (ordinal - 1))

    let private findLastUseLocation (document: ParsedDocument) name =
        useLocations document name
        |> List.tryLast

    let private findUseLocationAfter (document: ParsedDocument) name (origin: SourceLocation) ordinal =
        useLocations document name
        |> List.filter (fun location -> location.Span.Start >= origin.Span.End)
        |> List.tryItem (max 0 (ordinal - 1))

    let addDiagnostic code message location relatedLocations (document: ParsedDocument) (state: ResourceContext) =
        let diagnostic: Diagnostic =
            { Severity = Error
              Code = code
              Stage = Some "KFrontIR"
              Phase = Some(KFrontIRPhase.phaseName BODY_RESOLVE)
              Message = message
              Location = location |> Option.orElseWith (fun () -> Some(diagnosticLocation document))
              RelatedLocations = relatedLocations }

        { state with
            Diagnostics = state.Diagnostics @ [ diagnostic ] }

    let findBindingUseLocation (document: ParsedDocument) (binding: ResourceBinding) ordinal =
        match binding.Origin with
        | Some origin ->
            findUseLocationAfter document binding.Name origin ordinal
            |> Option.orElseWith (fun () -> findUseLocation document binding.Name ordinal)
        | None ->
            findUseLocation document binding.Name ordinal

    let argumentLocation (document: ParsedDocument) argument =
        match argument with
        | Name [ name ]
        | InoutArgument(Name [ name ]) ->
            findLastUseLocation document name
        | _ -> None

    let findBinderLocation (document: ParsedDocument) name =
        tryFindBinderLocation document name
