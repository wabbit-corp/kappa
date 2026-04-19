namespace Kappa.Compiler

type DiagnosticSeverity =
    | Info
    | Warning
    | Error

type Diagnostic =
    { Severity: DiagnosticSeverity
      Message: string
      Location: SourceLocation option }

type DiagnosticBag() =
    let items = ResizeArray<Diagnostic>()

    member _.Add(severity: DiagnosticSeverity, message: string, ?location: SourceLocation) =
        items.Add(
            { Severity = severity
              Message = message
              Location = location }
        )

    member this.AddInfo(message: string, ?location: SourceLocation) =
        this.Add(Info, message, ?location = location)

    member this.AddWarning(message: string, ?location: SourceLocation) =
        this.Add(Warning, message, ?location = location)

    member this.AddError(message: string, ?location: SourceLocation) =
        this.Add(Error, message, ?location = location)

    member _.AddRange(diagnostics: Diagnostic seq) =
        diagnostics |> Seq.iter items.Add

    member _.Items = List.ofSeq items
    member _.HasErrors = items |> Seq.exists (fun diagnostic -> diagnostic.Severity = Error)
