// Deterministic fuzzball-inspired compiler smoke tests.
module FuzzballInspiredTests

open System
open Kappa.Compiler
open Harness
open Xunit

let private source lines =
    String.concat "\n" lines

let private rootName name =
    "memory-fuzzball-" + name

let private diagnosticText (workspace: WorkspaceCompilation) =
    workspace.Diagnostics
    |> List.map (fun diagnostic -> diagnostic.ToString())
    |> String.concat "\n"

let private compileNoThrow name files =
    let mutable workspace = Unchecked.defaultof<WorkspaceCompilation>

    let ex =
        Record.Exception(fun () ->
            workspace <- compileInMemoryWorkspace (rootName name) files
            Assert.NotEmpty(workspace.Documents))

    Assert.Null(ex)
    workspace

let private assertNoCompilerErrors name files =
    let workspace = compileNoThrow name files
    Assert.False(workspace.HasErrors, diagnosticText workspace)

let private validGeneratedPrograms : obj array seq =
    seq {
        let generatedBinaryProgram operatorName operatorUse expected =
            source
                [
                    "module main"
                    $"infix left 60 ({operatorName})"
                    $"let ({operatorName}) x y = x + y"
                    $"let i0 = {operatorUse}"
                    "let result = i0"
                ],
            expected

        yield
            [|
                box "identifier-canonicalization-lowercase"
                box
                    [
                        "main.kp",
                        source
                            [
                                "module main"
                                "let i0 = 40"
                                "let i1 = 2"
                                "let i2 = i0 + i1"
                                "let result = i2"
                            ]
                    ]
                box "42"
            |]

        yield
            [|
                box "identifier-canonicalization-uppercase-data"
                box
                    [
                        "main.kp",
                        source
                            [
                                "module main"
                                "data I0 : Type ="
                                "    I1 Int"
                                "    I2"
                                "let read i0 ="
                                "    match i0"
                                "      case I1 i1 -> i1"
                                "      case I2 -> 0"
                                "let result = read (I1 42)"
                            ]
                    ]
                box "42"
            |]

        let operatorProgram, expected = generatedBinaryProgram "++" "20 ++ 22" "42"
        yield [| box "operator-fixity-generated-symbol"; box [ "main.kp", operatorProgram ]; box expected |]

        yield
            [|
                box "module-fragments-generated-identifiers"
                box
                    [
                        "math.kp",
                        source
                            [
                                "module math"
                                "let i0 = 40"
                            ]
                        "main.kp",
                        source
                            [
                                "module main"
                                "import math.*"
                                "let i1 = 2"
                                "let result = i0 + i1"
                            ]
                    ]
                box "42"
            |]
    }

let private adversarialCompilerCorpus : obj array seq =
    seq {
        yield
            [|
                box "unbalanced-expression-after-token-drop"
                box
                    [
                        "main.kp",
                        source
                            [
                                "module main"
                                "let i0 = 1 +"
                                "let i1 = ((i0"
                            ]
                    ]
            |]

        yield
            [|
                box "rebalance-brackets-inside-match"
                box
                    [
                        "main.kp",
                        source
                            [
                                "module main"
                                "data I0 : Type ="
                                "    I1 Int"
                                "let i2 x ="
                                "    match x"
                                "      case I1 y -> (y + 1"
                                "      case _ -> 0)"
                            ]
                    ]
            |]

        yield
            [|
                box "keyword-and-import-noise"
                box
                    [
                        "main.kp",
                        source
                            [
                                "module main"
                                "import std.list.(type List(..), ctor (::), trait Eq, term map)"
                                "let `match` = 1"
                                "let i0 = `match` + missing"
                            ]
                    ]
            |]

        yield
            [|
                box "duplicate-and-capitalized-fuzz-identifiers"
                box
                    [
                        "main.kp",
                        source
                            [
                                "module wrong"
                                "data I0 : Type ="
                                "    I0 Int"
                                "data I0 : Type ="
                                "    I1"
                                "let I0 = 42"
                                "let i0 = I0 I0"
                            ]
                    ]
            |]

        yield
            [|
                box "layout-block-token-deletion"
                box
                    [
                        "main.kp",
                        source
                            [
                                "module main"
                                "let i0 ="
                                "    let i1 = 1"
                                "      let i2 = \"unterminated"
                                "    i1 + i2"
                            ]
                    ]
            |]
    }

type ValidGeneratedPrograms() =
    static member Cases = validGeneratedPrograms

type AdversarialCompilerCorpus() =
    static member Cases = adversarialCompilerCorpus

[<Theory>]
[<MemberData(nameof ValidGeneratedPrograms.Cases, MemberType = typeof<ValidGeneratedPrograms>)>]
let ``fuzzball style generated programs compile and evaluate`` name (files: (string * string) list) expected =
    assertNoCompilerErrors name files

    let workspace, result = evaluateInMemoryBinding (rootName (name + "-eval")) "main.result" files
    Assert.False(workspace.HasErrors, diagnosticText workspace)

    match result with
    | Result.Ok value ->
        Assert.Equal(expected, RuntimeValue.format value)
    | Result.Error issue ->
        failwithf "Expected generated program to evaluate, got %s" issue.Message

[<Theory>]
[<MemberData(nameof AdversarialCompilerCorpus.Cases, MemberType = typeof<AdversarialCompilerCorpus>)>]
let ``fuzzball style adversarial programs stay in diagnostics`` name (files: (string * string) list) =
    let workspace = compileNoThrow name files

    Assert.True(
        workspace.HasErrors,
        "Expected adversarial fuzzball-style input to produce diagnostics, but compilation succeeded."
    )
