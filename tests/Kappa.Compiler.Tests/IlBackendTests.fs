// Covers the internal IL-emitter backend directly.
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
let ``il backend emits parameterized public static methods from explicit signatures`` () =
    let workspace =
        compileInMemoryWorkspace
            "memory-il-parameterized-root"
            [
                "main.kp",
                [
                    "module main"
                    "twice : Int -> Int"
                    "let twice x = x * 2"
                    "let result = twice 21"
                ]
                |> String.concat "\n"
            ]

    let outputDirectory = createScratchDirectory "il-parameterized"

    let artifact =
        match Backend.emitIlAssemblyArtifact workspace outputDirectory with
        | Result.Ok artifact -> artifact
        | Result.Error message -> failwith message

    use loaded = loadManagedAssembly artifact.AssemblyFilePath

    let moduleType = loaded.Assembly.GetType("Kappa.Generated.main", throwOnError = true, ignoreCase = false)
    let twiceMethod = moduleType.GetMethod("twice", BindingFlags.Public ||| BindingFlags.Static)
    let resultMethod = moduleType.GetMethod("result", BindingFlags.Public ||| BindingFlags.Static)
    let parameters = twiceMethod.GetParameters()

    Assert.NotNull(twiceMethod)
    Assert.NotNull(resultMethod)
    Assert.Equal(typeof<int64>, twiceMethod.ReturnType)
    let _ = Assert.Single(parameters)
    Assert.Equal(typeof<int64>, parameters[0].ParameterType)
    Assert.Equal(42L, twiceMethod.Invoke(null, [| box 21L |]) |> unbox<int64>)
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
let ``il backend uses ordinal string equality for equality operators`` () =
    let workspace =
        compileInMemoryWorkspace
            "memory-il-string-equality-root"
            [
                "main.kp",
                [
                    "module main"
                    "same : String -> String -> Bool"
                    "let same a b = a == b"
                    "different : String -> String -> Bool"
                    "let different a b = a != b"
                ]
                |> String.concat "\n"
            ]

    let outputDirectory = createScratchDirectory "il-string-equality"

    let artifact =
        match Backend.emitIlAssemblyArtifact workspace outputDirectory with
        | Result.Ok artifact -> artifact
        | Result.Error message -> failwith message

    use loaded = loadManagedAssembly artifact.AssemblyFilePath

    let moduleType = loaded.Assembly.GetType("Kappa.Generated.main", throwOnError = true, ignoreCase = false)
    let sameMethod = moduleType.GetMethod("same", BindingFlags.Public ||| BindingFlags.Static)
    let differentMethod = moduleType.GetMethod("different", BindingFlags.Public ||| BindingFlags.Static)

    let abc1 = System.String([| 'a'; 'b'; 'c' |])
    let abc2 = System.String([| 'a'; 'b'; 'c' |])
    let abd = System.String([| 'a'; 'b'; 'd' |])

    Assert.False(Object.ReferenceEquals(abc1, abc2), "Expected distinct managed string instances for equality testing.")
    Assert.NotNull(sameMethod)
    Assert.NotNull(differentMethod)
    Assert.Equal(typeof<bool>, sameMethod.ReturnType)
    Assert.Equal(typeof<bool>, differentMethod.ReturnType)
    Assert.True(sameMethod.Invoke(null, [| box abc1; box abc2 |]) |> unbox<bool>)
    Assert.False(sameMethod.Invoke(null, [| box abc1; box abd |]) |> unbox<bool>)
    Assert.False(differentMethod.Invoke(null, [| box abc1; box abc2 |]) |> unbox<bool>)
    Assert.True(differentMethod.Invoke(null, [| box abc1; box abd |]) |> unbox<bool>)

[<Fact>]
let ``il backend matches string literal patterns by content`` () =
    let workspace =
        compileInMemoryWorkspace
            "memory-il-string-pattern-root"
            [
                "main.kp",
                [
                    "module main"
                    "classify : String -> Int"
                    "let classify s ="
                    "    match s"
                    "    case \"abc\" -> 1"
                    "    case _ -> 0"
                ]
                |> String.concat "\n"
            ]

    let outputDirectory = createScratchDirectory "il-string-pattern"

    let artifact =
        match Backend.emitIlAssemblyArtifact workspace outputDirectory with
        | Result.Ok artifact -> artifact
        | Result.Error message -> failwith message

    use loaded = loadManagedAssembly artifact.AssemblyFilePath

    let moduleType = loaded.Assembly.GetType("Kappa.Generated.main", throwOnError = true, ignoreCase = false)
    let classifyMethod = moduleType.GetMethod("classify", BindingFlags.Public ||| BindingFlags.Static)

    let abc = System.String([| 'a'; 'b'; 'c' |])
    let xyz = System.String([| 'x'; 'y'; 'z' |])

    Assert.NotNull(classifyMethod)
    Assert.Equal(typeof<int64>, classifyMethod.ReturnType)
    Assert.Equal(1L, classifyMethod.Invoke(null, [| box abc |]) |> unbox<int64>)
    Assert.Equal(0L, classifyMethod.Invoke(null, [| box xyz |]) |> unbox<int64>)

[<Fact>]
let ``il backend evaluates literal or patterns across all alternatives`` () =
    let workspace =
        compileInMemoryWorkspace
            "memory-il-or-pattern-literals-root"
            [
                "main.kp",
                [
                    "module main"
                    "classify : Int -> Int"
                    "let classify x ="
                    "    match x"
                    "    case 1 | 2 -> 10"
                    "    case _ -> 20"
                    "let result = classify 2"
                ]
                |> String.concat "\n"
            ]

    let outputDirectory = createScratchDirectory "il-or-pattern-literals"

    let artifact =
        match Backend.emitIlAssemblyArtifact workspace outputDirectory with
        | Result.Ok artifact -> artifact
        | Result.Error message -> failwith message

    use loaded = loadManagedAssembly artifact.AssemblyFilePath

    let moduleType = loaded.Assembly.GetType("Kappa.Generated.main", throwOnError = true, ignoreCase = false)
    let classifyMethod = moduleType.GetMethod("classify", BindingFlags.Public ||| BindingFlags.Static)
    let resultMethod = moduleType.GetMethod("result", BindingFlags.Public ||| BindingFlags.Static)

    Assert.NotNull(classifyMethod)
    Assert.NotNull(resultMethod)
    Assert.Equal(typeof<int64>, classifyMethod.ReturnType)
    Assert.Equal(10L, classifyMethod.Invoke(null, [| box 1L |]) |> unbox<int64>)
    Assert.Equal(10L, classifyMethod.Invoke(null, [| box 2L |]) |> unbox<int64>)
    Assert.Equal(20L, classifyMethod.Invoke(null, [| box 3L |]) |> unbox<int64>)
    Assert.Equal(10L, resultMethod.Invoke(null, [||]) |> unbox<int64>)

[<Fact>]
let ``il backend evaluates constructor or patterns with shared binders`` () =
    let workspace =
        compileInMemoryWorkspace
            "memory-il-or-pattern-constructors-root"
            [
                "main.kp",
                [
                    "module main"
                    "data Choice : Type ="
                    "    A Int"
                    "    B Int"
                    "unwrap : Choice -> Int"
                    "let unwrap choice ="
                    "    match choice"
                    "    case A value | B value -> value"
                    "let result = unwrap (B 42)"
                ]
                |> String.concat "\n"
            ]

    let outputDirectory = createScratchDirectory "il-or-pattern-constructors"

    let artifact =
        match Backend.emitIlAssemblyArtifact workspace outputDirectory with
        | Result.Ok artifact -> artifact
        | Result.Error message -> failwith message

    use loaded = loadManagedAssembly artifact.AssemblyFilePath

    let moduleType = loaded.Assembly.GetType("Kappa.Generated.main", throwOnError = true, ignoreCase = false)
    let resultMethod = moduleType.GetMethod("result", BindingFlags.Public ||| BindingFlags.Static)

    Assert.NotNull(resultMethod)
    Assert.Equal(typeof<int64>, resultMethod.ReturnType)
    Assert.Equal(42L, resultMethod.Invoke(null, [||]) |> unbox<int64>)

[<Fact>]
let ``il backend executes direct calls to top level zero capture lambda bindings`` () =
    let workspace =
        compileInMemoryWorkspace
            "memory-il-top-level-zero-capture-lambda-root"
            [
                "main.kp",
                [
                    "module main"
                    "let i0 = \\() -> 42"
                    "result : Int"
                    "let result = i0 ()"
                ]
                |> String.concat "\n"
            ]

    Assert.False(workspace.HasErrors, sprintf "Expected zero-capture lambda binding to compile, got %A" workspace.Diagnostics)

    let outputDirectory = createScratchDirectory "il-top-level-zero-capture-lambda"

    let artifact =
        match Backend.emitIlAssemblyArtifact workspace outputDirectory with
        | Result.Ok artifact -> artifact
        | Result.Error message -> failwith message

    use loaded = loadManagedAssembly artifact.AssemblyFilePath

    let moduleType = loaded.Assembly.GetType("Kappa.Generated.main", throwOnError = true, ignoreCase = false)
    let resultMethod = moduleType.GetMethod("result", BindingFlags.Public ||| BindingFlags.Static)

    Assert.NotNull(resultMethod)
    Assert.Equal(typeof<int64>, resultMethod.ReturnType)
    Assert.Equal(42L, resultMethod.Invoke(null, [||]) |> unbox<int64>)

[<Fact>]
let ``il backend emits callable wrappers for host dotnet type modules`` () =
    let workspace =
        compileInMemoryWorkspaceWithBackend
            "memory-il-host-dotnet-root"
            "dotnet"
            [
                "main.kp",
                [
                    "module main"
                    "import host.dotnet.Kappa.Compiler.TestHost.Sample.(term new, term Echo, term Create)"
                    "result : String"
                    "let result ="
                    "    let fromCtor = new \"wrapped\""
                    "    let _ = Echo fromCtor ()"
                    "    let fromStatic = Create \"ok\""
                    "    Echo fromStatic ()"
                ]
                |> String.concat "\n"
            ]

    Assert.False(workspace.HasErrors, sprintf "Expected host.dotnet wrappers to compile, got %A" workspace.Diagnostics)

    let outputDirectory = createScratchDirectory "il-host-dotnet"

    let artifact =
        match Backend.emitIlAssemblyArtifact workspace outputDirectory with
        | Result.Ok artifact -> artifact
        | Result.Error message -> failwith message

    use loaded = loadManagedAssembly artifact.AssemblyFilePath

    let moduleType = loaded.Assembly.GetType("Kappa.Generated.main", throwOnError = true, ignoreCase = false)
    let resultMethod = moduleType.GetMethod("result", BindingFlags.Public ||| BindingFlags.Static)

    Assert.NotNull(resultMethod)
    Assert.Equal(typeof<string>, resultMethod.ReturnType)
    Assert.Equal("ok", resultMethod.Invoke(null, [||]) :?> string)

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
                    "import math"
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

[<Fact>]
let ``il backend supports qualified cross module calls into parameterized bindings`` () =
    let workspace =
        compileInMemoryWorkspace
            "memory-il-qualified-parameterized-root"
            [
                "math.kp",
                [
                    "module math"
                    "twice : Int -> Int"
                    "let twice value = value * 2"
                ]
                |> String.concat "\n"
                "main.kp",
                [
                    "module main"
                    "import math"
                    "let result = math.twice 21"
                ]
                |> String.concat "\n"
            ]

    let outputDirectory = createScratchDirectory "il-qualified-parameterized"

    let artifact =
        match Backend.emitIlAssemblyArtifact workspace outputDirectory with
        | Result.Ok artifact -> artifact
        | Result.Error message -> failwith message

    use loaded = loadManagedAssembly artifact.AssemblyFilePath

    let mainType = loaded.Assembly.GetType("Kappa.Generated.main", throwOnError = true, ignoreCase = false)
    let mathType = loaded.Assembly.GetType("Kappa.Generated.math", throwOnError = true, ignoreCase = false)
    let twiceMethod = mathType.GetMethod("twice", BindingFlags.Public ||| BindingFlags.Static)
    let resultMethod = mainType.GetMethod("result", BindingFlags.Public ||| BindingFlags.Static)

    Assert.NotNull(twiceMethod)
    Assert.NotNull(resultMethod)
    Assert.Equal(42L, twiceMethod.Invoke(null, [| box 21L |]) |> unbox<int64>)
    Assert.Equal(42L, resultMethod.Invoke(null, [||]) |> unbox<int64>)

[<Fact>]
let ``il backend supports unqualified imported calls into parameterized bindings`` () =
    let workspace =
        compileInMemoryWorkspace
            "memory-il-imported-parameterized-root"
            [
                "math.kp",
                [
                    "module math"
                    "twice : Int -> Int"
                    "let twice value = value * 2"
                ]
                |> String.concat "\n"
                "main.kp",
                [
                    "module main"
                    "import math.*"
                    "let result = twice 21"
                ]
                |> String.concat "\n"
            ]

    let outputDirectory = createScratchDirectory "il-imported-parameterized"

    let artifact =
        match Backend.emitIlAssemblyArtifact workspace outputDirectory with
        | Result.Ok artifact -> artifact
        | Result.Error message -> failwith message

    use loaded = loadManagedAssembly artifact.AssemblyFilePath

    let mainType = loaded.Assembly.GetType("Kappa.Generated.main", throwOnError = true, ignoreCase = false)
    let resultMethod = mainType.GetMethod("result", BindingFlags.Public ||| BindingFlags.Static)

    Assert.NotNull(resultMethod)
    Assert.Equal(42L, resultMethod.Invoke(null, [||]) |> unbox<int64>)

[<Fact>]
let ``il backend supports recursive signed methods over primitive values`` () =
    let workspace =
        compileInMemoryWorkspace
            "memory-il-recursive-root"
            [
                "main.kp",
                [
                    "module main"
                    "sumDown : Int -> Int"
                    "let sumDown n = if n == 0 then 0 else n + sumDown (n - 1)"
                    "let result = sumDown 5"
                ]
                |> String.concat "\n"
            ]

    let outputDirectory = createScratchDirectory "il-recursive"

    let artifact =
        match Backend.emitIlAssemblyArtifact workspace outputDirectory with
        | Result.Ok artifact -> artifact
        | Result.Error message -> failwith message

    use loaded = loadManagedAssembly artifact.AssemblyFilePath

    let moduleType = loaded.Assembly.GetType("Kappa.Generated.main", throwOnError = true, ignoreCase = false)
    let sumDownMethod = moduleType.GetMethod("sumDown", BindingFlags.Public ||| BindingFlags.Static)
    let resultMethod = moduleType.GetMethod("result", BindingFlags.Public ||| BindingFlags.Static)

    Assert.NotNull(sumDownMethod)
    Assert.NotNull(resultMethod)
    Assert.Equal(15L, sumDownMethod.Invoke(null, [| box 5L |]) |> unbox<int64>)
    Assert.Equal(15L, resultMethod.Invoke(null, [||]) |> unbox<int64>)

[<Fact>]
let ``il backend emits generic list adt types for the prelude`` () =
    let workspace =
        compileInMemoryWorkspace
            "memory-il-list-types-root"
            [
                "main.kp",
                [
                    "module main"
                    "empty : List Int"
                    "let empty = Nil"
                    "singleton : List Int"
                    "let singleton = 1 :: Nil"
                ]
                |> String.concat "\n"
            ]

    let outputDirectory = createScratchDirectory "il-list-types"

    let artifact =
        match Backend.emitIlAssemblyArtifact workspace outputDirectory with
        | Result.Ok artifact -> artifact
        | Result.Error message -> failwith message

    use loaded = loadManagedAssembly artifact.AssemblyFilePath

    let moduleType = loaded.Assembly.GetType("Kappa.Generated.main", throwOnError = true, ignoreCase = false)
    let emptyMethod = moduleType.GetMethod("empty", BindingFlags.Public ||| BindingFlags.Static)
    let singletonMethod = moduleType.GetMethod("singleton", BindingFlags.Public ||| BindingFlags.Static)
    let emptyValue = emptyMethod.Invoke(null, [||])
    let singletonValue = singletonMethod.Invoke(null, [||])
    let nilType = emptyValue.GetType()
    let consType = singletonValue.GetType()
    let listType = nilType.BaseType

    Assert.NotNull(moduleType)
    Assert.NotNull(emptyMethod)
    Assert.NotNull(singletonMethod)
    Assert.NotNull(listType)
    Assert.NotNull(nilType)
    Assert.NotNull(consType)
    Assert.True(listType.IsClass)
    Assert.True(listType.IsAbstract)
    Assert.True(listType.IsGenericType)
    Assert.Equal(1, listType.GetGenericArguments().Length)
    Assert.True(nilType.IsGenericType)
    Assert.True(consType.IsGenericType)
    Assert.Equal(listType.GetGenericTypeDefinition(), nilType.GetGenericTypeDefinition().BaseType.GetGenericTypeDefinition())
    Assert.Equal(listType.GetGenericTypeDefinition(), consType.GetGenericTypeDefinition().BaseType.GetGenericTypeDefinition())

[<Fact>]
let ``il backend emits constructors for multi parameter data types when constructors mention only a subset of parameters`` () =
    let workspace =
        compileInMemoryWorkspace
            "memory-il-multi-parameter-data-root"
            [
                "main.kp",
                [
                    "module main"
                    "data Exit e a : Type ="
                    "    Success a"
                    "    Failure e"
                    "let ok : Exit Int Int = Success 1"
                    "let err : Exit Int Int = Failure 2"
                ]
                |> String.concat "\n"
            ]

    let outputDirectory = createScratchDirectory "il-multi-parameter-data"

    let artifact =
        match Backend.emitIlAssemblyArtifact workspace outputDirectory with
        | Result.Ok artifact -> artifact
        | Result.Error message -> failwith message

    use loaded = loadManagedAssembly artifact.AssemblyFilePath

    let moduleType = loaded.Assembly.GetType("Kappa.Generated.main", throwOnError = true, ignoreCase = false)
    let okMethod = moduleType.GetMethod("ok", BindingFlags.Public ||| BindingFlags.Static)
    let errMethod = moduleType.GetMethod("err", BindingFlags.Public ||| BindingFlags.Static)

    Assert.NotNull(okMethod)
    Assert.NotNull(errMethod)
    Assert.NotNull(okMethod.Invoke(null, [||]))
    Assert.NotNull(errMethod.Invoke(null, [||]))

[<Fact>]
let ``il backend evaluates recursive list matches through emitted clr types`` () =
    let workspace =
        compileInMemoryWorkspace
            "memory-il-list-match-root"
            [
                "main.kp",
                [
                    "module main"
                    "sumList : List Int -> Int"
                    "let sumList xs ="
                    "    match xs"
                    "    case Nil -> 0"
                    "    case head :: tail -> head + sumList tail"
                    "let result = sumList (10 :: 20 :: 42 :: Nil)"
                ]
                |> String.concat "\n"
            ]

    let outputDirectory = createScratchDirectory "il-list-match"

    let artifact =
        match Backend.emitIlAssemblyArtifact workspace outputDirectory with
        | Result.Ok artifact -> artifact
        | Result.Error message -> failwith message

    use loaded = loadManagedAssembly artifact.AssemblyFilePath

    let moduleType = loaded.Assembly.GetType("Kappa.Generated.main", throwOnError = true, ignoreCase = false)
    let resultMethod = moduleType.GetMethod("result", BindingFlags.Public ||| BindingFlags.Static)

    Assert.NotNull(resultMethod)
    Assert.Equal(typeof<int64>, resultMethod.ReturnType)
    Assert.Equal(72L, resultMethod.Invoke(null, [||]) |> unbox<int64>)

[<Fact>]
let ``il backend emits explicitly imported constructors from lowered backend state`` () =
    let workspace =
        compileInMemoryWorkspace
            "memory-il-explicit-constructor-root"
            [
                "foo.kp",
                [
                    "module foo"
                    "data Box a : Type ="
                    "    Box a"
                ]
                |> String.concat "\n"
                "main.kp",
                [
                    "module main"
                    "import foo.(type Box(..))"
                    "let result ="
                    "    match Box 42"
                    "    case Box value -> value"
                ]
                |> String.concat "\n"
            ]

    let outputDirectory = createScratchDirectory "il-explicit-constructor"

    let artifact =
        match Backend.emitIlAssemblyArtifact { workspace with Documents = []; KCore = []; KRuntimeIR = [] } outputDirectory with
        | Result.Ok artifact -> artifact
        | Result.Error message -> failwith message

    use loaded = loadManagedAssembly artifact.AssemblyFilePath

    let moduleType = loaded.Assembly.GetType("Kappa.Generated.main", throwOnError = true, ignoreCase = false)
    let resultMethod = moduleType.GetMethod("result", BindingFlags.Public ||| BindingFlags.Static)

    Assert.NotNull(resultMethod)
    Assert.Equal(typeof<int64>, resultMethod.ReturnType)
    Assert.Equal(42L, resultMethod.Invoke(null, [||]) |> unbox<int64>)

[<Fact>]
let ``il backend emission does not depend on frontend documents`` () =
    let workspace =
        compileInMemoryWorkspace
            "memory-il-no-documents-root"
            [
                "main.kp",
                [
                    "module main"
                    "sumList : List Int -> Int"
                    "let sumList xs ="
                    "    match xs"
                    "    case Nil -> 0"
                    "    case head :: tail -> head + sumList tail"
                    "let result = sumList (10 :: 20 :: 42 :: Nil)"
                ]
                |> String.concat "\n"
            ]

    let outputDirectory = createScratchDirectory "il-no-documents"

    let artifact =
        match Backend.emitIlAssemblyArtifact { workspace with Documents = [] } outputDirectory with
        | Result.Ok artifact -> artifact
        | Result.Error message -> failwith message

    use loaded = loadManagedAssembly artifact.AssemblyFilePath

    let moduleType = loaded.Assembly.GetType("Kappa.Generated.main", throwOnError = true, ignoreCase = false)
    let resultMethod = moduleType.GetMethod("result", BindingFlags.Public ||| BindingFlags.Static)

    Assert.NotNull(resultMethod)
    Assert.Equal(72L, resultMethod.Invoke(null, [||]) |> unbox<int64>)
