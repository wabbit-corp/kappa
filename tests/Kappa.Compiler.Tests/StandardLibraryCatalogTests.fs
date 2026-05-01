module StandardLibraryCatalogTests

open Kappa.Compiler
open Xunit

let private tokenTexts (tokens: Token list) =
    tokens |> List.map (fun token -> token.Text)

[<Fact>]
let ``standard library catalog distinguishes source-backed and synthetic modules`` () =
    let prelude =
        StandardLibraryCatalog.tryFindBySegments CompilerKnownSymbols.KnownModules.Prelude
        |> Option.defaultWith (fun () -> failwith "expected std.prelude in standard library catalog")

    let unicode =
        StandardLibraryCatalog.tryFindBySegments CompilerKnownSymbols.KnownModules.Unicode
        |> Option.defaultWith (fun () -> failwith "expected std.unicode in standard library catalog")

    Assert.True(StandardLibraryCatalog.isSourceBacked prelude)
    Assert.True(StandardLibraryCatalog.isSynthetic unicode)

[<Fact>]
let ``standard library catalog exposes source-backed module text and paths without string reconstruction call sites`` () =
    let preludeText = StandardLibraryCatalog.loadPreludeText ()
    let preludePath = StandardLibraryCatalog.preludeVirtualPath

    Assert.Contains("module std.prelude", preludeText)
    Assert.EndsWith("std\\prelude.kp", preludePath)

[<Fact>]
let ``standard library catalog unifies bundled and synthetic surface inventories`` () =
    let hashTypeNames =
        StandardLibraryCatalog.tryTypeNames CompilerKnownSymbols.KnownModules.Hash
        |> Option.defaultValue Set.empty

    let unicodeTermType =
        StandardLibraryCatalog.tryTermTypeText CompilerKnownSymbols.KnownModules.Unicode "normalize"

    let testingTerms =
        StandardLibraryCatalog.tryTermNames CompilerKnownSymbols.KnownModules.Testing
        |> Option.defaultValue Set.empty

    Assert.Contains(CompilerKnownSymbols.KnownTypeNames.HashCode, hashTypeNames)
    Assert.Equal(Some "NormalizationForm -> String -> String", unicodeTermType)
    Assert.Contains("failNow", testingTerms)

[<Fact>]
let ``standard library catalog exposes bundled prelude bootstrap fixity declarations`` () =
    let fixities = StandardLibrarySourceParsing.preludeBootstrapFixityDeclarations ()

    Assert.NotEmpty(fixities)
    Assert.Contains(fixities, fun declaration -> declaration.OperatorName = "<|")
    Assert.Contains(fixities, fun declaration -> declaration.OperatorName = "<")

[<Fact>]
let ``standard library catalog resolves intrinsic term type tokens across source and catalog modules`` () =
    let preludeTypeText =
        StandardLibrarySourceParsing.tryIntrinsicTermTypeTokens CompilerKnownSymbols.KnownModules.Prelude "<|"
        |> Option.map tokenTexts

    let derivingShapeTypeText =
        StandardLibrarySourceParsing.tryIntrinsicTermTypeTokens [ "std"; "deriving"; "shape" ] "omitImplicitFieldArgument"
        |> Option.map tokenTexts

    let hashTypeText =
        StandardLibrarySourceParsing.tryIntrinsicTermTypeTokens CompilerKnownSymbols.KnownModules.Hash "hashUnit"
        |> Option.map tokenTexts

    Assert.Equal(Some [ "("; "a"; "->"; "b"; ")"; "->"; "a"; "->"; "b" ], preludeTypeText)
    Assert.Equal(Some [ "ShapeField"; "->"; "FieldArgument" ], derivingShapeTypeText)
    Assert.Equal(Some [ "("; "1"; "state"; ":"; "HashState"; ")"; "->"; "HashState" ], hashTypeText)
