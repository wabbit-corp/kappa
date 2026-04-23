namespace Kappa.Compiler

open System
open System.IO
open System.Reflection

module internal BundledBackendSources =
    let private loadResourceText resourceName =
        lazy
            (let assembly = Assembly.GetExecutingAssembly()
             use stream = assembly.GetManifestResourceStream(resourceName)

             if isNull stream then
                 invalidOp $"Could not load bundled backend resource '{resourceName}'."

             use reader = new StreamReader(stream)
             reader.ReadToEnd().Replace("\r\n", "\n"))

    let private hostedDotNetRuntime =
        loadResourceText "Kappa.Compiler.Runtime.Hosted.KappaRuntime.cs"

    let private zigRuntimeTemplate =
        loadResourceText "Kappa.Compiler.Runtime.Zig.kappa_runtime.c"

    let private zigEntrypointTemplate =
        loadResourceText "Kappa.Compiler.Runtime.Zig.kappa_entrypoint.c"

    let loadHostedDotNetRuntime () = hostedDotNetRuntime.Value

    let renderZigRuntimePrelude typeIdLines preludeBoolTypeId =
        zigRuntimeTemplate.Value
            .Replace("__KAPPA_TYPE_ID_ENUM__", String.concat "\n" typeIdLines)
            .Replace("__KAPPA_PRELUDE_BOOL_TYPE_ID__", preludeBoolTypeId)

    let renderZigEntryPointProgram entryFunctionName =
        zigEntrypointTemplate.Value.Replace("__KAPPA_ENTRY_FUNCTION_NAME__", entryFunctionName)
