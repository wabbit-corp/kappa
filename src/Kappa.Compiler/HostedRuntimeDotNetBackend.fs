namespace Kappa.Compiler

open System
open System.Globalization
open System.IO
open System.Text

// Builds the nonstandard hosted dotnet runner project used for bootstrap-style execution.
module HostedRuntimeDotNetBackend =
    let private csharpString (value: string) =
        let builder = StringBuilder()
        builder.Append('"') |> ignore

        for ch in value do
            match ch with
            | '\\' -> builder.Append("\\\\") |> ignore
            | '"' -> builder.Append("\\\"") |> ignore
            | '\r' -> builder.Append("\\r") |> ignore
            | '\n' -> builder.Append("\\n") |> ignore
            | '\t' -> builder.Append("\\t") |> ignore
            | _ when Char.IsControl(ch) ->
                builder.Append($"\\u{int ch:x4}") |> ignore
            | _ ->
                builder.Append(ch) |> ignore

        builder.Append('"') |> ignore
        builder.ToString()

    let private csharpChar (value: char) =
        match value with
        | '\\' -> "'\\\\'"
        | '\'' -> "'\\''"
        | '\r' -> "'\\r'"
        | '\n' -> "'\\n'"
        | '\t' -> "'\\t'"
        | _ when Char.IsControl(value) -> $"'\\u{int value:x4}'"
        | _ -> $"'{value}'"

    let private emitStringArray (values: string list) =
        match values with
        | [] -> "System.Array.Empty<string>()"
        | _ ->
            values
            |> List.map csharpString
            |> String.concat ", "
            |> fun body -> $"new[] {{ {body} }}"

    let private emitImportNamespace namespaceOption =
        match namespaceOption with
        | None -> "ImportNamespaceKind.None"
        | Some ImportNamespace.Term -> "ImportNamespaceKind.Term"
        | Some ImportNamespace.Type -> "ImportNamespaceKind.Type"
        | Some ImportNamespace.Trait -> "ImportNamespaceKind.Trait"
        | Some ImportNamespace.Constructor -> "ImportNamespaceKind.Constructor"

    let private emitImportItem (item: ImportItem) =
        $"new ImportItem({emitImportNamespace item.Namespace}, {csharpString item.Name})"

    let private emitImportSelection selection =
        match selection with
        | QualifiedOnly ->
            "ImportSelection.QualifiedOnly()"
        | Items items ->
            match items with
            | [] ->
                "ImportSelection.FromItems(System.Array.Empty<ImportItem>())"
            | _ ->
                items
                |> List.map emitImportItem
                |> String.concat ", "
                |> fun body -> $"ImportSelection.FromItems({body})"
        | All ->
            "ImportSelection.All()"
        | AllExcept names ->
            match names with
            | [] ->
                "ImportSelection.AllExcept(System.Array.Empty<string>())"
            | _ ->
                names
                |> emitStringArray
                |> fun body -> $"ImportSelection.AllExcept({body})"

    let private emitModuleSpecifier specifier =
        match specifier with
        | Dotted segments ->
            $"ModuleSpecifier.Dotted({emitStringArray segments})"
        | Url url ->
            $"ModuleSpecifier.FromUrl({csharpString url})"

    let private emitImportSpec (spec: ImportSpec) =
        let aliasCode =
            spec.Alias
            |> Option.map csharpString
            |> Option.defaultValue "null"

        $"new ImportSpec({emitModuleSpecifier spec.Source}, {aliasCode}, {emitImportSelection spec.Selection})"

    let private emitLiteralValue literal =
        match literal with
        | LiteralValue.Integer value ->
            $"LiteralValue.FromInteger({value}L)"
        | LiteralValue.Float value ->
            let text = value.ToString("R", CultureInfo.InvariantCulture)
            $"LiteralValue.FromFloat({text})"
        | LiteralValue.String value ->
            $"LiteralValue.FromString({csharpString value})"
        | LiteralValue.Character value ->
            $"LiteralValue.FromCharacter({csharpChar value})"
        | LiteralValue.Unit ->
            "LiteralValue.Unit()"

    let rec private emitBackendPattern pattern =
        match pattern with
        | KRuntimeWildcardPattern ->
            "Pattern.Wildcard()"
        | KRuntimeNamePattern name ->
            $"Pattern.FromName({csharpString name})"
        | KRuntimeLiteralPattern literal ->
            $"Pattern.FromLiteral({emitLiteralValue literal})"
        | KRuntimeConstructorPattern(name, arguments) ->
            let argumentsCode =
                match arguments with
                | [] -> "System.Array.Empty<Pattern>()"
                | _ ->
                    arguments
                    |> List.map emitBackendPattern
                    |> String.concat ", "
                    |> fun body -> $"new Pattern[] {{ {body} }}"

            $"Pattern.Constructor({emitStringArray name}, {argumentsCode})"

    let rec private emitBackendExpression expression =
        match expression with
        | KRuntimeLiteral literal ->
            $"new LiteralExpression({emitLiteralValue literal})"
        | KRuntimeName segments ->
            $"new NameExpression({emitStringArray segments})"
        | KRuntimeClosure(parameters, body) ->
            $"new ClosureExpression({emitStringArray parameters}, {emitBackendExpression body})"
        | KRuntimeIfThenElse(condition, whenTrue, whenFalse) ->
            $"new IfExpression({emitBackendExpression condition}, {emitBackendExpression whenTrue}, {emitBackendExpression whenFalse})"
        | KRuntimeMatch(scrutinee, cases) ->
            let casesCode =
                match cases with
                | [] -> "System.Array.Empty<MatchCase>()"
                | _ ->
                    cases
                    |> List.map (fun caseClause ->
                        $"new MatchCase({emitBackendPattern caseClause.Pattern}, {emitBackendExpression caseClause.Body})")
                    |> String.concat ", "
                    |> fun body -> $"new MatchCase[] {{ {body} }}"

            $"new MatchExpression({emitBackendExpression scrutinee}, {casesCode})"
        | KRuntimeExecute expression ->
            emitBackendExpression expression
        | KRuntimeLet(bindingName, value, body) ->
            $"new ApplyExpression(new ClosureExpression(new[] {{ {csharpString bindingName} }}, {emitBackendExpression body}), new KExpr[] {{ {emitBackendExpression value} }})"
        | KRuntimeDoScope(_, body) ->
            emitBackendExpression body
        | KRuntimeScheduleExit(_, _, body) ->
            emitBackendExpression body
        | KRuntimeSequence(first, second) ->
            $"new ApplyExpression(new ClosureExpression(new[] {{ \"_\" }}, {emitBackendExpression second}), new KExpr[] {{ {emitBackendExpression first} }})"
        | KRuntimeWhile(_, _) ->
            "new LiteralExpression(LiteralValue.Unit())"
        | KRuntimeApply(callee, arguments) ->
            let argumentsCode =
                match arguments with
                | [] -> "System.Array.Empty<KExpr>()"
                | _ ->
                    arguments
                    |> List.map emitBackendExpression
                    |> String.concat ", "
                    |> fun body -> $"new KExpr[] {{ {body} }}"

            $"new ApplyExpression({emitBackendExpression callee}, {argumentsCode})"
        | KRuntimeDictionaryValue(_, _, _) ->
            "new LiteralExpression(LiteralValue.Unit())"
        | KRuntimeTraitCall(_, _, _, _) ->
            "new LiteralExpression(LiteralValue.Unit())"
        | KRuntimeUnary(operatorName, operand) ->
            $"new UnaryExpression({csharpString operatorName}, {emitBackendExpression operand})"
        | KRuntimeBinary(left, operatorName, right) ->
            $"new BinaryExpression({emitBackendExpression left}, {csharpString operatorName}, {emitBackendExpression right})"
        | KRuntimePrefixedString(prefix, parts) ->
            let partsCode =
                match parts with
                | [] -> "System.Array.Empty<StringPart>()"
                | _ ->
                    parts
                    |> List.map (function
                        | KRuntimeStringText text -> $"StringPart.FromText({csharpString text})"
                        | KRuntimeStringInterpolation inner -> $"StringPart.FromInterpolation({emitBackendExpression inner})")
                    |> String.concat ", "
                    |> fun body -> $"new StringPart[] {{ {body} }}"

            $"new PrefixedStringExpression({csharpString prefix}, {partsCode})"

    let private runtimeSource = BundledBackendSources.loadHostedDotNetRuntime ()

    let private projectSource deployment =
        let publishAotProperty =
            match deployment with
            | Managed -> ""
            | NativeAot -> "    <PublishAot>true</PublishAot>\n"

        [
            "<Project Sdk=\"Microsoft.NET.Sdk\">"
            "  <PropertyGroup>"
            "    <OutputType>Exe</OutputType>"
            "    <TargetFramework>net10.0</TargetFramework>"
            "    <ImplicitUsings>enable</ImplicitUsings>"
            "    <Nullable>enable</Nullable>"
            publishAotProperty.TrimEnd('\n')
            "  </PropertyGroup>"
            "</Project>"
        ]
        |> List.filter (String.IsNullOrWhiteSpace >> not)
        |> String.concat Environment.NewLine

    let private programSource entryPoint =
        [
            "using Kappa.Generated;"
            ""
            "try"
            "{"
            $"    var value = KappaRunner.Run(GeneratedProgram.CreateModules(), {csharpString entryPoint});"
            "    if (KappaRunner.ShouldPrintResult(value))"
            "    {"
            "        Console.WriteLine(KappaRunner.Format(value));"
            "    }"
            "    return 0;"
            "}"
            "catch (RuntimeError error)"
            "{"
            "    Console.Error.WriteLine($\"runtime error: {error.Message}\");"
            "    return 1;"
            "}"
        ]
        |> String.concat Environment.NewLine

    let private appendArray (builder: StringBuilder) (itemType: string) (items: string list) =
        match items with
        | [] ->
            builder.Append($"System.Array.Empty<{itemType}>()") |> ignore
        | _ ->
            builder.Append($"new {itemType}[] {{ ") |> ignore
            items |> List.iteri (fun index item ->
                if index > 0 then
                    builder.Append(", ") |> ignore

                builder.Append(item) |> ignore)
            builder.Append(" }") |> ignore

    let private appendBinding (builder: StringBuilder) (binding: KRuntimeBinding) =
        builder.Append("new BindingSpec(") |> ignore
        builder.Append(csharpString binding.Name).Append(", ") |> ignore
        binding.Parameters |> List.map (fun parameter -> parameter.Name) |> List.map csharpString |> appendArray builder "string"
        builder.Append(", ") |> ignore

        match binding.Body with
        | Some body ->
            builder.Append(emitBackendExpression body) |> ignore
        | None ->
            builder.Append("null") |> ignore

        builder.Append(", ").Append(if binding.Intrinsic then "true" else "false").Append(")") |> ignore

    let private appendConstructor (builder: StringBuilder) (constructor: KRuntimeConstructor) =
        builder.Append("new ConstructorSpec(") |> ignore
        builder.Append(csharpString constructor.Name).Append(", ") |> ignore
        builder.Append(constructor.Arity).Append(", ") |> ignore
        builder.Append(csharpString constructor.TypeName).Append(")") |> ignore

    let private appendModule (builder: StringBuilder) (moduleDump: KRuntimeModule) =
        builder.Append("            [") |> ignore
        builder.Append(csharpString moduleDump.Name).Append("] = new RuntimeModuleSpec(") |> ignore
        builder.Append(csharpString moduleDump.Name).Append(", ") |> ignore

        moduleDump.Imports
        |> List.map emitImportSpec
        |> appendArray builder "ImportSpec"

        builder.Append(", ") |> ignore
        moduleDump.Exports |> List.map csharpString |> appendArray builder "string"
        builder.Append(", ") |> ignore
        moduleDump.IntrinsicTerms |> List.map csharpString |> appendArray builder "string"
        builder.Append(", ") |> ignore
        builder.Append("new ConstructorSpec[] { ") |> ignore
        moduleDump.Constructors
        |> List.iteri (fun index constructor ->
            if index > 0 then
                builder.Append(", ") |> ignore

            appendConstructor builder constructor)

        builder.Append(" }, ") |> ignore

        builder.Append("new BindingSpec[] { ") |> ignore
        moduleDump.Bindings
        |> List.iteri (fun index binding ->
            if index > 0 then
                builder.Append(", ") |> ignore

            appendBinding builder binding)

        builder.Append(" })") |> ignore

    let private generatedSource (workspace: WorkspaceCompilation) =
        let builder = StringBuilder()
        builder.AppendLine("using System;") |> ignore
        builder.AppendLine("using System.Collections.Generic;") |> ignore
        builder.AppendLine() |> ignore
        builder.AppendLine("namespace Kappa.Generated;") |> ignore
        builder.AppendLine() |> ignore
        builder.AppendLine("internal static class GeneratedProgram") |> ignore
        builder.AppendLine("{") |> ignore
        builder.AppendLine("    public static IReadOnlyDictionary<string, RuntimeModuleSpec> CreateModules() =>") |> ignore
        builder.AppendLine("        new Dictionary<string, RuntimeModuleSpec>(StringComparer.Ordinal)") |> ignore
        builder.AppendLine("        {") |> ignore

        workspace.KRuntimeIR
        |> List.sortBy (fun moduleDump -> moduleDump.Name)
        |> List.iteri (fun index moduleDump ->
            if index > 0 then
                builder.AppendLine(",") |> ignore

            appendModule builder moduleDump)

        builder.AppendLine() |> ignore
        builder.AppendLine("        };") |> ignore
        builder.AppendLine("}") |> ignore
        builder.ToString()

    let private resolveEntryPoint (workspace: WorkspaceCompilation) (entryPoint: string) =
        let segments =
            entryPoint.Split('.', StringSplitOptions.RemoveEmptyEntries)
            |> Array.toList

        match segments with
        | [] ->
            Result.Error "Expected a binding name to run."
        | [ bindingName ] ->
            let matches =
                workspace.KRuntimeIR
                |> List.choose (fun moduleDump ->
                    if moduleDump.Bindings |> List.exists (fun binding -> String.Equals(binding.Name, bindingName, StringComparison.Ordinal) && not binding.Intrinsic) then
                        Some(moduleDump.Name, bindingName)
                    else
                        None)

            match matches with
            | [] ->
                Result.Error $"No executable binding named '{bindingName}' was found."
            | [ moduleName, resolvedBindingName ] ->
                Result.Ok(moduleName, resolvedBindingName)
            | _ ->
                Result.Error $"Binding name '{bindingName}' is ambiguous. Use a fully qualified name."
        | _ ->
            let moduleName = segments |> List.take (segments.Length - 1) |> String.concat "."
            let bindingName = List.last segments

            match workspace.KRuntimeIR |> List.tryFind (fun moduleDump -> String.Equals(moduleDump.Name, moduleName, StringComparison.Ordinal)) with
            | Some moduleDump when moduleDump.Bindings |> List.exists (fun binding -> String.Equals(binding.Name, bindingName, StringComparison.Ordinal) && not binding.Intrinsic) ->
                Result.Ok(moduleName, bindingName)
            | Some _ ->
                Result.Error $"Module '{moduleName}' does not define '{bindingName}'."
            | None ->
                Result.Error $"Module '{moduleName}' was not found."

    let private aggregateDiagnostics diagnostics =
        diagnostics
        |> List.map (fun diagnostic -> diagnostic.Message)
        |> String.concat Environment.NewLine

    let emitDotNetArtifact (workspace: WorkspaceCompilation) (entryPoint: string) (outputDirectory: string) (deployment: DotNetDeployment) =
        if workspace.HasErrors then
            Result.Error $"Cannot emit a runnable backend artifact for a workspace with diagnostics:{Environment.NewLine}{aggregateDiagnostics workspace.Diagnostics}"
        else
            let verificationDiagnostics = CheckpointVerification.verifyCheckpoint workspace "KRuntimeIR"

            if not (List.isEmpty verificationDiagnostics) then
                Result.Error $"Cannot emit malformed KRuntimeIR:{Environment.NewLine}{aggregateDiagnostics verificationDiagnostics}"
            else
                match resolveEntryPoint workspace entryPoint with
                | Result.Error message ->
                    Result.Error message
                | Result.Ok _ ->
                    let projectDirectory = Path.GetFullPath(outputDirectory)
                    Directory.CreateDirectory(projectDirectory) |> ignore

                    let projectName = "Kappa.Generated.Runner"
                    let projectFilePath = Path.Combine(projectDirectory, $"{projectName}.csproj")
                    let programFilePath = Path.Combine(projectDirectory, "Program.cs")
                    let runtimeFilePath = Path.Combine(projectDirectory, "KappaRuntime.cs")
                    let generatedFilePath = Path.Combine(projectDirectory, "GeneratedProgram.cs")

                    File.WriteAllText(projectFilePath, projectSource deployment)
                    File.WriteAllText(programFilePath, programSource entryPoint)
                    File.WriteAllText(runtimeFilePath, runtimeSource)
                    File.WriteAllText(generatedFilePath, generatedSource workspace)

                    Result.Ok
                        { ProjectDirectory = projectDirectory
                          ProjectFilePath = projectFilePath
                          ProgramFilePath = programFilePath
                          RuntimeFilePath = runtimeFilePath
                          GeneratedFilePath = generatedFilePath
                          EntryPoint = entryPoint
                          Deployment = deployment }
