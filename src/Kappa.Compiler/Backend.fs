namespace Kappa.Compiler

open System
open System.IO
open Kappa.Runtime

// Routes public backend requests to the appropriate artifact emitter.
module Backend =
    type private ResultBuilder() =
        member _.Bind(result, binder) = Result.bind binder result
        member _.Return(value) = Result.Ok value
        member _.ReturnFrom(result) = result
        member _.Zero() = Result.Ok()
        member _.Delay(generator) = generator
        member _.Run(generator) = generator()
        member _.Combine(first, second) =
            first |> Result.bind (fun () -> second())

    let private result = ResultBuilder()

    let private csharpString (value: string) =
        value.Replace("\\", "\\\\").Replace("\"", "\\\"")

    let private resolveClrEntryPoint (workspace: WorkspaceCompilation) (entryPoint: string) =
        let segments =
            entryPoint.Split('.', StringSplitOptions.RemoveEmptyEntries)
            |> Array.toList

        let tryMatchBinding moduleName bindingName =
            workspace.ClrAssemblyIR
            |> List.tryFind (fun moduleDump -> String.Equals(moduleDump.Name, moduleName, StringComparison.Ordinal))
            |> Option.bind (fun moduleDump ->
                moduleDump.Bindings
                |> List.tryFind (fun binding ->
                    String.Equals(binding.Name, bindingName, StringComparison.Ordinal)
                    && not binding.Intrinsic
                    && List.isEmpty binding.Parameters)
                |> Option.map (fun _ -> ()))

        match segments with
        | [] ->
            Result.Error "Expected a binding name to run."
        | [ bindingName ] ->
            let matches =
                workspace.ClrAssemblyIR
                |> List.choose (fun moduleDump ->
                    moduleDump.Bindings
                    |> List.tryFind (fun binding ->
                        String.Equals(binding.Name, bindingName, StringComparison.Ordinal)
                        && not binding.Intrinsic
                        && List.isEmpty binding.Parameters)
                    |> Option.map (fun binding -> moduleDump.Name, binding.Name))

            match matches with
            | [] ->
                Result.Error $"No zero-argument binding named '{bindingName}' was found for dotnet."
            | [ moduleName, resolvedBindingName ] ->
                Result.Ok(moduleName, resolvedBindingName)
            | _ ->
                Result.Error $"Binding name '{bindingName}' is ambiguous. Use a fully qualified name."
        | _ ->
            let moduleName = segments |> List.take (segments.Length - 1) |> String.concat "."
            let bindingName = List.last segments

            match tryMatchBinding moduleName bindingName with
            | Some _ ->
                Result.Ok(moduleName, bindingName)
            | None ->
                Result.Error $"dotnet requires a zero-argument binding named '{bindingName}' in module '{moduleName}'."

    let private buildClrRunnerProjectText supportFileNames =
        let itemLines =
            supportFileNames
            |> List.distinct
            |> List.map (fun fileName -> $"    <None Include=\"{fileName}\" CopyToOutputDirectory=\"PreserveNewest\" />")
            |> String.concat Environment.NewLine

        $"""<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <OutputType>Exe</OutputType>
    <TargetFramework>net10.0</TargetFramework>
    <ImplicitUsings>enable</ImplicitUsings>
    <Nullable>enable</Nullable>
    <TreatWarningsAsErrors>true</TreatWarningsAsErrors>
  </PropertyGroup>

  <ItemGroup>
{itemLines}
  </ItemGroup>
</Project>
"""

    let private buildClrRunnerProgramText assemblyFileName moduleName bindingName =
        let emittedTypeName = IlDotNetBackend.emittedModuleTypeName moduleName
        let emittedMethodName = IlDotNetBackend.emittedMethodName bindingName

        $"""using System;
using System.IO;
using System.Reflection;

internal static class Program
{{
    private static bool ShouldPrintResult(object? value)
    {{
        return value switch
        {{
            null => false,
            ValueTuple => false,
            _ => true
        }};
    }}

    private static string FormatValue(object? value)
    {{
        return value switch
        {{
            long integerValue => integerValue.ToString(),
            double floatValue => floatValue.ToString(System.Globalization.CultureInfo.InvariantCulture),
            bool boolValue => boolValue ? "True" : "False",
            string stringValue => "\"" + stringValue.Replace("\\", "\\\\").Replace("\"", "\\\"") + "\"",
            char characterValue => "'" + characterValue.ToString() + "'",
            null => "()",
            _ => value.ToString() ?? "()"
        }};
    }}

    public static int Main()
    {{
        try
        {{
            var assemblyPath = Path.Combine(AppContext.BaseDirectory, "{csharpString assemblyFileName}");
            var assembly = Assembly.LoadFrom(assemblyPath);
            var moduleType = assembly.GetType("{csharpString emittedTypeName}", throwOnError: false, ignoreCase: false);

            if (moduleType is null)
            {{
                Console.Error.WriteLine("dotnet could not find emitted type '{csharpString emittedTypeName}'.");
                return 1;
            }}

            var method = moduleType.GetMethod("{csharpString emittedMethodName}", BindingFlags.Public | BindingFlags.Static);

            if (method is null)
            {{
                Console.Error.WriteLine("dotnet could not find emitted method '{csharpString emittedTypeName}.{csharpString emittedMethodName}'.");
                return 1;
            }}

            var value = method.Invoke(null, Array.Empty<object>());

            if (ShouldPrintResult(value))
            {{
                Console.Out.WriteLine(FormatValue(value));
            }}

            return 0;
        }}
        catch (TargetInvocationException ex)
        {{
            Console.Error.WriteLine("runtime error: " + (ex.InnerException?.ToString() ?? ex.ToString()));
            return 1;
        }}
        catch (Exception ex)
        {{
            Console.Error.WriteLine("runtime error: " + ex.ToString());
            return 1;
        }}
    }}
}}
"""

    let emitClrDotNetArtifact
        (workspace: WorkspaceCompilation)
        (entryPoint: string)
        (outputDirectory: string)
        (deployment: DotNetDeployment)
        =
        result {
            if deployment <> DotNetDeployment.Managed then
                return!
                    Result.Error
                        "The CLR-backed dotnet profile currently supports managed builds only."

            let! moduleName, bindingName = resolveClrEntryPoint workspace entryPoint
            let usesEffectRuntime = ClrAssemblyIR.modulesUseEffectRuntime workspace.ClrAssemblyIR

            let! clrAssembly =
                ClrDotNetBackend.emitAssemblyArtifact workspace outputDirectory
                |> Result.mapError (fun message -> $"The CLR-backed dotnet profile could not lower '{entryPoint}': {message}")

            let projectDirectory = Path.GetFullPath(outputDirectory)
            Directory.CreateDirectory(projectDirectory) |> ignore

            let copySupportFile (sourcePath: string) =
                let fileName = Path.GetFileName(sourcePath)
                let destinationPath = Path.Combine(projectDirectory, fileName)

                if not (String.Equals(Path.GetFullPath(sourcePath), destinationPath, StringComparison.OrdinalIgnoreCase)) then
                    File.Copy(sourcePath, destinationPath, true)

                fileName, destinationPath

            let generatedAssemblyFileName, generatedAssemblyPath = copySupportFile clrAssembly.AssemblyFilePath

            let! supportFiles =
                if usesEffectRuntime then
                    let runtimeAssemblyPath = typeof<KappaRuntime>.Assembly.Location

                    if String.IsNullOrWhiteSpace(runtimeAssemblyPath) then
                        Result.Error
                            "The CLR-backed dotnet profile could not locate Kappa.Runtime.dll for effectful lowering."
                    else
                        let runtimeAssemblyFileName, _ = copySupportFile runtimeAssemblyPath
                        Result.Ok [ generatedAssemblyFileName; runtimeAssemblyFileName ]
                else
                    Result.Ok [ generatedAssemblyFileName ]

            let projectFilePath = Path.Combine(projectDirectory, "Kappa.Generated.Runner.csproj")
            let programFilePath = Path.Combine(projectDirectory, "Program.cs")
            File.WriteAllText(projectFilePath, buildClrRunnerProjectText supportFiles)
            File.WriteAllText(programFilePath, buildClrRunnerProgramText generatedAssemblyFileName moduleName bindingName)

            return
                { ProjectDirectory = projectDirectory
                  ProjectFilePath = projectFilePath
                  ProgramFilePath = programFilePath
                  RuntimeFilePath = generatedAssemblyPath
                  GeneratedFilePath = generatedAssemblyPath
                  EntryPoint = entryPoint
                  Deployment = deployment }
        }

    let emitDotNetArtifact
        (workspace: WorkspaceCompilation)
        (entryPoint: string)
        (outputDirectory: string)
        (deployment: DotNetDeployment)
        =
        emitClrDotNetArtifact workspace entryPoint outputDirectory deployment

    let emitIlAssemblyArtifact (workspace: WorkspaceCompilation) (outputDirectory: string) =
        IlDotNetBackend.emitAssemblyArtifact workspace outputDirectory

    let emitZigArtifact (workspace: WorkspaceCompilation) (entryPoint: string) (outputDirectory: string) =
        ZigCcBackend.emitArtifact workspace entryPoint outputDirectory

    let emitZigCcArtifact (workspace: WorkspaceCompilation) (entryPoint: string) (outputDirectory: string) =
        emitZigArtifact workspace entryPoint outputDirectory
