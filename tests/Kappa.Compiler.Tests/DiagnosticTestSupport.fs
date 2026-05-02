module DiagnosticTestSupport

open System
open Kappa.Compiler

let hasDiagnosticCode code (diagnostic: Diagnostic) = diagnostic.Code = code

let diagnosticsText diagnostics =
    diagnostics
    |> List.map (fun diagnostic -> $"{DiagnosticCode.toIdentifier diagnostic.Code}: {diagnostic.Message}")
    |> String.concat Environment.NewLine

let tryFindPayloadText fieldName (diagnostic: Diagnostic) =
    diagnostic.Payload.Fields
    |> List.tryPick (fun field ->
        if field.Name <> fieldName then
            None
        else
            match field.Value with
            | DiagnosticPayloadText value -> Some value
            | DiagnosticPayloadTextList _ -> None)

let tryFindPayloadTextList fieldName (diagnostic: Diagnostic) =
    diagnostic.Payload.Fields
    |> List.tryPick (fun field ->
        if field.Name <> fieldName then
            None
        else
            match field.Value with
            | DiagnosticPayloadText _ -> None
            | DiagnosticPayloadTextList values -> Some values)

let hasPayloadText fieldName expectedValue (diagnostic: Diagnostic) =
    tryFindPayloadText fieldName diagnostic = Some expectedValue

let hasPayloadTextList fieldName expectedValues (diagnostic: Diagnostic) =
    tryFindPayloadTextList fieldName diagnostic = Some expectedValues
