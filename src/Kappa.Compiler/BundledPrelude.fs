namespace Kappa.Compiler

open System.IO
open System.Reflection

// Loads the embedded std.prelude source used for bootstrap parsing and implicit imports.
module internal BundledPrelude =
    let private resourceName = "Kappa.Compiler.Stdlib.std.prelude.kp"

    let private bundledText =
        lazy
            (let assembly = Assembly.GetExecutingAssembly()
             use stream = assembly.GetManifestResourceStream(resourceName)

             if isNull stream then
                 invalidOp $"Could not load bundled prelude resource '{resourceName}'."

             use reader = new StreamReader(stream)
             reader.ReadToEnd().Replace("\r\n", "\n"))

    let virtualPath =
        Path.Combine(Path.GetFullPath("__kappa_stdlib__"), "std", "prelude.kp")

    let loadText () = bundledText.Value
