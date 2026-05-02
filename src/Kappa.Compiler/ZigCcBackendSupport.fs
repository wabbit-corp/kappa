namespace Kappa.Compiler

open System
open System.Globalization
open System.IO

// Defines shared zig backend state, naming, and low-level emission helpers.
module internal ZigCcBackendSupport =
    type internal ResultBuilder() =
        member _.Bind(result, binder) = Result.bind binder result
        member _.Return(value) = Result.Ok value
        member _.ReturnFrom(result) = result
        member _.Zero() = Result.Ok()
        member _.Delay(generator) = generator
        member _.Run(generator) = generator()
        member _.Combine(first, second) =
            first |> Result.bind (fun () -> second())

    type internal GeneratedCFunction =
        { Prototype: string
          SupportDefinitions: string list
          Definition: string }

    type internal EmitScope =
        { CurrentModule: string
          Bindings: Map<string, string> }

    type internal EmittedExpression =
        { Statements: string list
          ValueExpression: string }

    type internal GenerationContext =
        { Workspace: WorkspaceCompilation
          Functions: Map<string * string, KBackendFunction>
          FunctionNames: Map<string * string, string>
          DataTypeIds: Map<string * string, string>
          GeneratedClosures: ResizeArray<GeneratedCFunction>
          mutable NextTempId: int
          mutable NextClosureId: int }

    let internal result = ResultBuilder()

    let internal aggregateDiagnostics diagnostics =
        diagnostics
        |> List.map (fun diagnostic -> diagnostic.Message)
        |> String.concat Environment.NewLine

    let internal resultOfOption errorMessage optionValue =
        match optionValue with
        | Some value -> Result.Ok value
        | None -> Result.Error errorMessage

    let internal sanitizeIdentifier (value: string) =
        let text =
            value
            |> Seq.collect (fun ch ->
                if Char.IsLetterOrDigit(ch) || ch = '_' then
                    Seq.singleton(string ch)
                else
                    Seq.singleton($"_u{int ch:x4}"))
            |> String.concat ""

        if String.IsNullOrWhiteSpace(text) then
            "_"
        elif Char.IsLetter(text[0]) || text[0] = '_' then
            text
        else
            "_" + text

    let internal cStringLiteral (value: string) =
        value
        |> Seq.map (fun ch ->
            match ch with
            | '\\' -> "\\\\"
            | '"' -> "\\\""
            | '\r' -> "\\r"
            | '\n' -> "\\n"
            | '\t' -> "\\t"
            | ch when Char.IsControl(ch) -> $"\\x{int ch:x2}"
            | ch -> string ch)
        |> String.concat ""

    let internal indentLines level (lines: string list) =
        let prefix = String.replicate (level * 4) " "
        lines |> List.map (fun line -> prefix + line)

    let internal joinLines (lines: string list) =
        String.concat Environment.NewLine lines

    let internal executablePath = HostSupport.executablePath

    let internal functionName moduleName bindingName =
        $"kappa_module_{sanitizeIdentifier moduleName}_{sanitizeIdentifier bindingName}"

    let internal typeIdName (moduleName: string) (typeName: string) =
        let sanitizeTypeIdPart (value: string) =
            value
            |> Seq.map (fun ch -> if Char.IsLetterOrDigit(ch) then string ch else "_")
            |> String.concat ""
            |> fun text ->
                if String.IsNullOrWhiteSpace(text) then
                    "_"
                elif Char.IsLetter(text[0]) || text[0] = '_' then
                    text
                else
                    "_" + text

        let moduleText =
            moduleName.Split('.', StringSplitOptions.RemoveEmptyEntries)
            |> Array.map sanitizeTypeIdPart
            |> String.concat "_"

        $"KTYPE_{moduleText}_{sanitizeTypeIdPart typeName}"

    let internal traitDispatchFunctionName traitName memberName =
        $"kappa_trait_dispatch_{sanitizeIdentifier traitName}_{sanitizeIdentifier memberName}"

    let internal freshTemp (context: GenerationContext) prefix =
        let value = context.NextTempId
        context.NextTempId <- context.NextTempId + 1
        $"{sanitizeIdentifier prefix}_{value}"

    let internal freshClosureFunctionName (context: GenerationContext) (moduleName: string) =
        let value = context.NextClosureId
        context.NextClosureId <- context.NextClosureId + 1
        $"kappa_closure_{sanitizeIdentifier moduleName}_{value}"

    let internal resolveEntryPoint (workspace: WorkspaceCompilation) (entryPoint: string) =
        let segments =
            entryPoint.Split('.', StringSplitOptions.RemoveEmptyEntries)
            |> Array.toList

        let tryMatchBinding moduleName bindingName =
            workspace.KBackendIR
            |> List.tryFind (fun moduleDump -> String.Equals(moduleDump.Name, moduleName, StringComparison.Ordinal))
            |> Option.bind (fun moduleDump ->
                moduleDump.Functions
                |> List.tryFind (fun binding ->
                    String.Equals(binding.Name, bindingName, StringComparison.Ordinal)
                    && not binding.Intrinsic
                    && List.isEmpty binding.Parameters))

        match segments with
        | [] ->
            Result.Error(DiagnosticFact.ZigBackendEmitterError.message ZigEntryPointBindingNameRequired)
        | [ bindingName ] ->
            let matches =
                workspace.KBackendIR
                |> List.choose (fun moduleDump ->
                    moduleDump.Functions
                    |> List.tryFind (fun binding ->
                        String.Equals(binding.Name, bindingName, StringComparison.Ordinal)
                        && not binding.Intrinsic
                        && List.isEmpty binding.Parameters)
                    |> Option.map (fun binding -> moduleDump.Name, binding.Name))

            match matches with
            | [] ->
                Result.Error(DiagnosticFact.ZigBackendEmitterError.message (ZigEntryPointBindingNotFound bindingName))
            | [ moduleName, resolvedBindingName ] ->
                Result.Ok(moduleName, resolvedBindingName)
            | _ ->
                Result.Error(DiagnosticFact.ZigBackendEmitterError.message (ZigEntryPointBindingAmbiguous bindingName))
        | _ ->
            let moduleName = segments |> List.take (segments.Length - 1) |> String.concat "."
            let bindingName = List.last segments

            match tryMatchBinding moduleName bindingName with
            | Some _ ->
                Result.Ok(moduleName, bindingName)
            | None ->
                Result.Error(
                    DiagnosticFact.ZigBackendEmitterError.message (
                        ZigQualifiedEntryPointBindingNotFound(moduleName, bindingName)
                    )
                )

    let internal buildContext (workspace: WorkspaceCompilation) =
        let functions =
            workspace.KBackendIR
            |> List.collect (fun moduleDump ->
                moduleDump.Functions
                |> List.filter (fun functionDump -> not functionDump.Intrinsic)
                |> List.map (fun functionDump -> (moduleDump.Name, functionDump.Name), functionDump))
            |> Map.ofList

        let functionNames =
            functions
            |> Map.toList
            |> List.map (fun ((moduleName, bindingName), _) ->
                (moduleName, bindingName), functionName moduleName bindingName)
            |> Map.ofList

        let dataTypeIds =
            workspace.KBackendIR
            |> List.collect (fun moduleDump ->
                moduleDump.DataLayouts
                |> List.map (fun layout ->
                    (moduleDump.Name, layout.TypeName), typeIdName moduleDump.Name layout.TypeName))
            |> Map.ofList

        { Workspace = workspace
          Functions = functions
          FunctionNames = functionNames
          DataTypeIds = dataTypeIds
          GeneratedClosures = ResizeArray()
          NextTempId = 0
          NextClosureId = 0 }

    let internal lookupFunction (context: GenerationContext) moduleName bindingName =
        context.Functions |> Map.tryFind (moduleName, bindingName)

    let internal lookupFunctionName (context: GenerationContext) moduleName bindingName =
        context.FunctionNames |> Map.tryFind (moduleName, bindingName)

    let internal lookupTypeId (context: GenerationContext) moduleName typeName =
        context.DataTypeIds
        |> Map.tryFind (moduleName, typeName)
        |> resultOfOption(
            DiagnosticFact.ZigBackendEmitterError.message (ZigRuntimeTypeLayoutResolutionFailed(moduleName, typeName))
        )

    let internal literalToBoxedExpression literal =
        match literal with
        | Integer value ->
            $"kappa_box_int({value}LL)"
        | Float value ->
            let text = value.ToString("R", CultureInfo.InvariantCulture)
            $"kappa_box_float({text})"
        | String value ->
            $"kappa_box_string(\"{cStringLiteral value}\")"
        | Character value ->
            $"kappa_box_string(\"{cStringLiteral value}\")"
        | Grapheme value ->
            $"kappa_box_string(\"{cStringLiteral value}\")"
        | Byte value ->
            $"kappa_box_int({value}LL)"
        | Unit ->
            "kappa_unit()"

    let internal literalGuard literal expression =
        match literal with
        | Integer value ->
            $"kappa_is_int_value({expression}, {value}LL)"
        | Float value ->
            let text = value.ToString("R", CultureInfo.InvariantCulture)
            $"kappa_is_float_value({expression}, {text})"
        | String value ->
            $"kappa_is_string_value({expression}, \"{cStringLiteral value}\")"
        | Character value ->
            $"kappa_is_string_value({expression}, \"{cStringLiteral value}\")"
        | Grapheme value ->
            $"kappa_is_string_value({expression}, \"{cStringLiteral value}\")"
        | Byte value ->
            $"kappa_is_int_value({expression}, {value}LL)"
        | Unit ->
            $"kappa_is_unit({expression})"

    let internal buildArgumentArray (context: GenerationContext) (arguments: string list) =
        if List.isEmpty arguments then
            [], "NULL"
        else
            let arrayName = freshTemp context "args"
            let initializerText = String.concat ", " arguments
            [ $"KValue* {arrayName}[{arguments.Length}] = {{ {initializerText} }};" ], arrayName

    let internal wrapCallResult (context: GenerationContext) prefix callExpression =
        let resultName = freshTemp context prefix
        [ $"KValue* {resultName} = {callExpression};" ], resultName
