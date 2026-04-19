module IlBackendTests

open System
open System.IO
open System.Reflection
open Kappa.Compiler
open Harness
open Xunit

[<Fact>]
let ``il backend emits a loadable assembly with public static module methods`` () =
    let workspace =
        compileInMemoryWorkspace
            "memory-il-basic-root"
            [
                "main.kp",
                [
                    "module main"
                    "let answer = 40"
                    "let result = answer + 2"
                ]
                |> String.concat "\n"
            ]

    let outputDirectory = createScratchDirectory "il-basic"

    let artifact =
        match Backend.emitIlAssemblyArtifact workspace outputDirectory with
        | Result.Ok artifact -> artifact
        | Result.Error message -> failwith message

    Assert.True(File.Exists(artifact.AssemblyFilePath), $"Expected emitted assembly at '{artifact.AssemblyFilePath}'.")

    use loaded = loadManagedAssembly artifact.AssemblyFilePath

    let moduleType = loaded.Assembly.GetType("Kappa.Generated.main", throwOnError = true, ignoreCase = false)
    let answerMethod = moduleType.GetMethod("answer", BindingFlags.Public ||| BindingFlags.Static)
    let resultMethod = moduleType.GetMethod("result", BindingFlags.Public ||| BindingFlags.Static)

    Assert.NotNull(answerMethod)
    Assert.NotNull(resultMethod)
    Assert.Equal(typeof<int64>, answerMethod.ReturnType)
    Assert.Equal(typeof<int64>, resultMethod.ReturnType)
    Assert.Equal(40L, answerMethod.Invoke(null, [||]) |> unbox<int64>)
    Assert.Equal(42L, resultMethod.Invoke(null, [||]) |> unbox<int64>)

[<Fact>]
let ``il backend emits executable conditional logic as IL`` () =
    let workspace =
        compileInMemoryWorkspace
            "memory-il-conditional-root"
            [
                "main.kp",
                [
                    "module main"
                    "let result = if (21 * 2) == 42 then 1 else 0"
                ]
                |> String.concat "\n"
            ]

    let outputDirectory = createScratchDirectory "il-conditional"

    let artifact =
        match Backend.emitIlAssemblyArtifact workspace outputDirectory with
        | Result.Ok artifact -> artifact
        | Result.Error message -> failwith message

    use loaded = loadManagedAssembly artifact.AssemblyFilePath

    let moduleType = loaded.Assembly.GetType("Kappa.Generated.main", throwOnError = true, ignoreCase = false)
    let resultMethod = moduleType.GetMethod("result", BindingFlags.Public ||| BindingFlags.Static)

    Assert.NotNull(resultMethod)
    Assert.Equal(typeof<int64>, resultMethod.ReturnType)
    Assert.Equal(1L, resultMethod.Invoke(null, [||]) |> unbox<int64>)

[<Fact>]
let ``il backend supports qualified cross module calls for zero argument bindings`` () =
    let workspace =
        compileInMemoryWorkspace
            "memory-il-qualified-root"
            [
                "math.kp",
                [
                    "module math"
                    "let answer = 41"
                ]
                |> String.concat "\n"
                "main.kp",
                [
                    "module main"
                    "let result = math.answer + 1"
                ]
                |> String.concat "\n"
            ]

    let outputDirectory = createScratchDirectory "il-qualified"

    let artifact =
        match Backend.emitIlAssemblyArtifact workspace outputDirectory with
        | Result.Ok artifact -> artifact
        | Result.Error message -> failwith message

    use loaded = loadManagedAssembly artifact.AssemblyFilePath

    let mainType = loaded.Assembly.GetType("Kappa.Generated.main", throwOnError = true, ignoreCase = false)
    let mathType = loaded.Assembly.GetType("Kappa.Generated.math", throwOnError = true, ignoreCase = false)
    let mathAnswer = mathType.GetMethod("answer", BindingFlags.Public ||| BindingFlags.Static)
    let resultMethod = mainType.GetMethod("result", BindingFlags.Public ||| BindingFlags.Static)

    Assert.NotNull(mathAnswer)
    Assert.NotNull(resultMethod)
    Assert.Equal(41L, mathAnswer.Invoke(null, [||]) |> unbox<int64>)
    Assert.Equal(42L, resultMethod.Invoke(null, [||]) |> unbox<int64>)
