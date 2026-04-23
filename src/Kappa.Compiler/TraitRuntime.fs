namespace Kappa.Compiler

open System

// Defines naming and runtime conventions for trait dictionaries and instance artifacts.
module TraitRuntime =
    let private sanitizePart (value: string) =
        value
        |> Seq.map (fun ch ->
            if Char.IsLetterOrDigit(ch) || ch = '_' then
                string ch
            else
                sprintf "_u%04x" (int ch))
        |> String.concat ""

    let private nonEmptyPart value =
        let sanitized = sanitizePart value

        if String.IsNullOrWhiteSpace(sanitized) then
            "_"
        else
            sanitized

    let objectTypeName = "__kappa_object"

    let dictionaryTypeName (traitName: string) =
        $"__kappa_dict_{nonEmptyPart traitName}"

    let dispatchBindingName (traitName: string) (memberName: string) =
        $"__kappa_traitcall_{nonEmptyPart traitName}_{nonEmptyPart memberName}"

    let instanceKeyFromTokens (tokens: Token list) =
        tokens
        |> List.map (fun token -> token.Text)
        |> String.concat "_"
        |> nonEmptyPart

    let instanceConstructorName (traitName: string) (instanceKey: string) =
        $"__kappa_dict_ctor_{nonEmptyPart traitName}_{nonEmptyPart instanceKey}"

    let instanceDictionaryBindingName (traitName: string) (instanceKey: string) =
        $"__kappa_instance_{nonEmptyPart traitName}_{nonEmptyPart instanceKey}"

    let instanceMemberBindingName (traitName: string) (instanceKey: string) (memberName: string) =
        $"__kappa_instance_{nonEmptyPart traitName}_{nonEmptyPart instanceKey}_{nonEmptyPart memberName}"

    let isDispatchBindingName (bindingName: string) =
        bindingName.StartsWith("__kappa_traitcall_", StringComparison.Ordinal)

    let tryParseDispatchBindingName (bindingName: string) =
        if not (isDispatchBindingName bindingName) then
            None
        else
            let remainder = bindingName.Substring("__kappa_traitcall_".Length)
            let separatorIndex = remainder.IndexOf('_')

            if separatorIndex <= 0 || separatorIndex >= remainder.Length - 1 then
                None
            else
                Some(remainder.Substring(0, separatorIndex), remainder.Substring(separatorIndex + 1))
