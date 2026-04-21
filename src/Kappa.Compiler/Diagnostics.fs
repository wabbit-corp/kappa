namespace Kappa.Compiler

type DiagnosticSeverity =
    | Info
    | Warning
    | Error

type Diagnostic =
    { Severity: DiagnosticSeverity
      Code: string
      Stage: string option
      Phase: string option
      Message: string
      Location: SourceLocation option }

module Diagnostic =
    let defaultCode severity =
        match severity with
        | Info -> "I_GENERAL"
        | Warning -> "W_GENERAL"
        | Error -> "E_GENERAL"

type DiagnosticBag() =
    let items = ResizeArray<Diagnostic>()

    member _.Add(
        severity: DiagnosticSeverity,
        message: string,
        ?location: SourceLocation,
        ?code: string,
        ?stage: string,
        ?phase: string
    ) =
        items.Add(
            { Severity = severity
              Code = defaultArg code (Diagnostic.defaultCode severity)
              Stage = stage
              Phase = phase
              Message = message
              Location = location }
        )

    member this.AddInfo(message: string, ?location: SourceLocation, ?code: string, ?stage: string, ?phase: string) =
        this.Add(Info, message, ?location = location, ?code = code, ?stage = stage, ?phase = phase)

    member this.AddWarning(message: string, ?location: SourceLocation, ?code: string, ?stage: string, ?phase: string) =
        this.Add(Warning, message, ?location = location, ?code = code, ?stage = stage, ?phase = phase)

    member this.AddError(message: string, ?location: SourceLocation, ?code: string, ?stage: string, ?phase: string) =
        this.Add(Error, message, ?location = location, ?code = code, ?stage = stage, ?phase = phase)

    member _.AddRange(diagnostics: Diagnostic seq) =
        diagnostics |> Seq.iter items.Add

    member _.Items = List.ofSeq items
    member _.HasErrors = items |> Seq.exists (fun diagnostic -> diagnostic.Severity = Error)
