module SmokeTests

open System
open System.IO
open Kappa.Compiler
open Harness
open Xunit

[<Fact>]
let ``source text reports 1-based line and column positions`` () =
    let source = createSource "memory.kp" "alpha\nbeta\n"
    let location = source.GetLocation(TextSpan.FromBounds(6, 10))

    Assert.Equal(2, location.Start.Line)
    Assert.Equal(1, location.Start.Column)
    Assert.Equal(2, location.End.Line)
    Assert.Equal(5, location.End.Column)

[<Fact>]
let ``lexer emits indent and dedent tokens for indented declarations`` () =
    let sourceText =
        [
            "data Maybe (a : Type) : Type ="
            "    Nothing"
            "    Just a"
        ]
        |> String.concat "\n"

    let _, lexed, _ =
        lexAndParse
            "memory.kp"
            sourceText

    let kinds = lexed.Tokens |> List.map (fun token -> token.Kind)

    Assert.Empty(lexed.Diagnostics)
    Assert.Contains(Indent, kinds)
    Assert.Contains(Dedent, kinds)

[<Fact>]
let ``parser captures import selectors and declaration kinds`` () =
    let sourceText =
        [
            "module demo.hello"
            "import std.list.(type List, ctor Cons)"
            "export std.math.(sin, cos)"
            "let answer = 42"
        ]
        |> String.concat "\n"

    let _, lexed, parsed =
        lexAndParse
            "demo/hello.kp"
            sourceText

    Assert.Empty(lexed.Diagnostics)
    Assert.Empty(parsed.Diagnostics)

    match parsed.Syntax.ModuleHeader with
    | Some [ "demo"; "hello" ] -> ()
    | other -> failwithf "Unexpected module header: %A" other

    match parsed.Syntax.Declarations with
    | [ ImportDeclaration (false, [ importSpec ])
        ImportDeclaration (true, [ exportSpec ])
        LetDeclaration definition ] ->
        match importSpec.Source, importSpec.Selection with
        | Dotted [ "std"; "list" ], Items items ->
            let first = List.item 0 items
            let second = List.item 1 items

            Assert.Equal("List", first.Name)
            Assert.Equal(Some ImportNamespace.Type, first.Namespace)
            Assert.Equal("Cons", second.Name)
            Assert.Equal(Some ImportNamespace.Constructor, second.Namespace)
        | other -> failwithf "Unexpected import declaration: %A" other

        match exportSpec.Source, exportSpec.Selection with
        | Dotted [ "std"; "math" ], Items items ->
            let names = items |> List.map (fun item -> item.Name)
            Assert.Equal<string list>([ "sin"; "cos" ], names)
        | other -> failwithf "Unexpected export declaration: %A" other

        Assert.Equal(Some "answer", definition.Name)
    | other ->
        failwithf "Unexpected top-level declarations: %A" other

[<Fact>]
let ``compilation reports import cycles across modules`` () =
    let root = Path.GetFullPath("memory-cycle-root")
    let moduleASource =
        [
            "module a"
            "import b"
            "let a = 1"
        ]
        |> String.concat "\n"

    let moduleBSource =
        [
            "module b"
            "import a"
            "let b = 2"
        ]
        |> String.concat "\n"

    let fileSystem =
        InMemoryFileSystem(
            [
                Path.Combine(root, "a.kp"), moduleASource
                Path.Combine(root, "b.kp"), moduleBSource
            ]
        )

    let workspace =
        Compilation.parse (CompilationOptions.createWithFileSystem fileSystem root) [ root ]

    Assert.True(workspace.HasErrors, "Expected a cycle diagnostic.")

    let cycleDiagnostic =
        workspace.Diagnostics
        |> List.tryFind (fun diagnostic -> diagnostic.Message.Contains("Import cycle detected"))

    Assert.True(cycleDiagnostic.IsSome, sprintf "Expected an import-cycle diagnostic, got %A" workspace.Diagnostics)

[<Fact>]
let ``parser captures expect declarations including soft keyword names and operator terms`` () =
    let sourceText =
        [
            "module demo.expect"
            "expect type type a"
            "expect trait trait a"
            "expect term (>>=) : IO a -> (a -> IO b) -> IO b"
        ]
        |> String.concat "\n"

    let _, lexed, parsed =
        lexAndParse
            "demo/expect.kp"
            sourceText

    Assert.Empty(lexed.Diagnostics)
    Assert.Empty(parsed.Diagnostics)

    match parsed.Syntax.Declarations with
    | [ ExpectDeclarationNode (ExpectTypeDeclaration typeDeclaration)
        ExpectDeclarationNode (ExpectTraitDeclaration traitDeclaration)
        ExpectDeclarationNode (ExpectTermDeclaration termDeclaration) ] ->
        Assert.Equal("type", typeDeclaration.Name)
        Assert.Equal<string list>([ "a" ], typeDeclaration.HeaderTokens |> List.map (fun token -> token.Text))

        Assert.Equal("trait", traitDeclaration.Name)
        Assert.Equal<string list>([ "a" ], traitDeclaration.HeaderTokens |> List.map (fun token -> token.Text))

        Assert.Equal(">>=", termDeclaration.Name)
        Assert.Equal<string list>(
            [ "IO"; "a"; "->"; "("; "a"; "->"; "IO"; "b"; ")"; "->"; "IO"; "b" ],
            termDeclaration.TypeTokens |> List.map (fun token -> token.Text)
        )
    | other ->
        failwithf "Unexpected declarations: %A" other

[<Fact>]
let ``compilation injects bundled std prelude and satisfies its intrinsic expects`` () =
    let mainSource =
        [
            "module main"
            "flag : Bool"
            "let flag = not False"
        ]
        |> String.concat "\n"

    let workspace =
        compileInMemoryWorkspace
            "memory-stdlib-root"
            [ "main.kp", mainSource ]

    Assert.False(workspace.HasErrors, sprintf "Expected no diagnostics, got %A" workspace.Diagnostics)

    let moduleNames =
        workspace.Documents
        |> List.choose (fun document -> document.ModuleName |> Option.map SyntaxFacts.moduleNameToText)

    Assert.Contains<string>(Stdlib.PreludeModuleText, moduleNames)
    Assert.Contains<string>("main", moduleNames)

    let preludeDocument =
        workspace.Documents
        |> List.find (fun document -> document.ModuleName = Some Stdlib.PreludeModuleName)

    let intrinsicExpect =
        preludeDocument.Syntax.Declarations
        |> List.tryPick (function
            | ExpectDeclarationNode (ExpectTermDeclaration declaration) when declaration.Name = "not" ->
                Some declaration
            | _ ->
                None)

    Assert.True(intrinsicExpect.IsSome, "Expected bundled std.prelude to declare intrinsic term 'not'.")

[<Fact>]
let ``implicit prelude import models the wildcard and constructor subset separately`` () =
    let imports = Stdlib.implicitImportsFor (Some [ "main" ])

    match imports with
    | [ wildcardImport; constructorImport ] ->
        match wildcardImport.Source, wildcardImport.Selection with
        | Dotted moduleName, All ->
            Assert.Equal<string list>(Stdlib.PreludeModuleName, moduleName)
        | other ->
            failwithf "Unexpected implicit wildcard prelude import: %A" other

        match constructorImport.Source, constructorImport.Selection with
        | Dotted moduleName, Items items ->
            Assert.Equal<string list>(Stdlib.PreludeModuleName, moduleName)

            let actualItems =
                items
                |> List.map (fun item -> item.Namespace, item.Name)

            let expectedItems =
                [
                    Some ImportNamespace.Constructor, "True"
                    Some ImportNamespace.Constructor, "False"
                    Some ImportNamespace.Constructor, "None"
                    Some ImportNamespace.Constructor, "Some"
                    Some ImportNamespace.Constructor, "Ok"
                    Some ImportNamespace.Constructor, "Err"
                    Some ImportNamespace.Constructor, "Nil"
                    Some ImportNamespace.Constructor, "::"
                    Some ImportNamespace.Constructor, "LT"
                    Some ImportNamespace.Constructor, "EQ"
                    Some ImportNamespace.Constructor, "GT"
                    Some ImportNamespace.Constructor, "refl"
                ]

            Assert.Equal<(ImportNamespace option * string) list>(expectedItems, actualItems)
        | other ->
            failwithf "Unexpected implicit prelude constructor import: %A" other
    | other ->
        failwithf "Expected two implicit prelude import specs, got %A" other

[<Fact>]
let ``compilation reports unsatisfied expect declarations outside std prelude`` () =
    let mainSource =
        [
            "module main"
            "expect term mystery : Int"
        ]
        |> String.concat "\n"

    let workspace =
        compileInMemoryWorkspace
            "memory-unsatisfied-expect-root"
            [ "main.kp", mainSource ]

    Assert.True(workspace.HasErrors, "Expected an unsatisfied expect diagnostic.")

    let diagnostic =
        workspace.Diagnostics
        |> List.tryFind (fun item -> item.Message.Contains("Unsatisfied expect declaration"))

    Assert.True(diagnostic.IsSome, sprintf "Expected an unsatisfied expect diagnostic, got %A" workspace.Diagnostics)

[<Fact>]
let ``parser treats a following signature as a top level boundary after a let body`` () =
    let sourceText =
        [
            "module demo.boundary"
            "flag : Bool"
            "let flag = not False"
            "message : String"
            "let message = \"ready\""
        ]
        |> String.concat "\n"

    let _, lexed, parsed =
        lexAndParse
            "demo/boundary.kp"
            sourceText

    Assert.Empty(lexed.Diagnostics)
    Assert.Empty(parsed.Diagnostics)

    match parsed.Syntax.Declarations with
    | [ SignatureDeclaration flagSignature
        LetDeclaration flagDefinition
        SignatureDeclaration messageSignature
        LetDeclaration messageDefinition ] ->
        Assert.Equal("flag", flagSignature.Name)
        Assert.Equal(Some "flag", flagDefinition.Name)
        Assert.Equal("message", messageSignature.Name)
        Assert.Equal(Some "message", messageDefinition.Name)
    | other ->
        failwithf "Unexpected declarations: %A" other
