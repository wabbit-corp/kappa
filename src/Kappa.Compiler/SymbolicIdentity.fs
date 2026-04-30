namespace Kappa.Compiler

open System

// Structured symbolic identities used after lexical parsing so semantic phases do not key modules by raw dotted text.
[<StructuralEquality; StructuralComparison>]
type ModuleIdentity = private ModuleIdentity of string list

module ModuleIdentity =
    let ofSegments segments =
        match segments with
        | [] ->
            invalidArg (nameof segments) "Module identities must contain at least one segment."
        | _ ->
            ModuleIdentity segments

    let ofOptionalSegments segments =
        segments |> Option.map ofSegments

    let ofDottedTextUnchecked (text: string) =
        if String.IsNullOrWhiteSpace(text) then
            invalidArg (nameof text) "Module identity text must not be blank."

        text.Split('.', StringSplitOptions.RemoveEmptyEntries)
        |> Array.toList
        |> ofSegments

    let ofModuleSpecifier specifier =
        match specifier with
        | Dotted segments -> Some(ofSegments segments)
        | Url _ -> None

    let segments (ModuleIdentity segments) = segments

    let text identity =
        identity
        |> segments
        |> SyntaxFacts.moduleNameToText

    let asciiCaseFoldKey identity =
        identity
        |> segments
        |> List.map (fun segment -> segment.ToLowerInvariant())
