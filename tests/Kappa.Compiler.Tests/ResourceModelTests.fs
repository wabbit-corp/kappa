// Exercises the standalone resource-model helpers used by M3 checking.
module ResourceModelTests

open Kappa.Compiler
open Kappa.Compiler.ResourceModel
open HarnessWorkspace
open Xunit

let private hasDiagnosticCode code (diagnostic: Diagnostic) =
    diagnostic.Code = code

let private assertContainsDiagnostic code (diagnostics: Diagnostic list) =
    Assert.Contains(diagnostics, hasDiagnosticCode code)

let private assertDoesNotContainDiagnostic code (diagnostics: Diagnostic list) =
    Assert.DoesNotContain(diagnostics, hasDiagnosticCode code)

let private mkParameter name quantity =
    { Name = name
      TypeTokens = None
      Quantity = quantity
      IsImplicit = false
      IsInout = false }

let private mkBindPattern name quantity =
    { Pattern = NamePattern name
      Quantity = quantity }

let private mkAnonymousRecordBindPattern quantity fields =
    { Pattern =
        AnonymousRecordPattern(
            fields
            |> List.map (fun (name, patternName) ->
                { Name = name
                  Pattern = NamePattern patternName })
        )
      Quantity = quantity }

let private mkLetDefinition name parameters body =
    { Visibility = None
      IsOpaque = false
      Name = Some name
      Parameters = parameters
      HeaderTokens = []
      ReturnTypeTokens = None
      BodyTokens = []
      Body = Some body }

let private parsedDocumentWithDeclarations filePath text declarations =
    let source, lexed, parsed = lexAndParse filePath text

    { Source = source
      InferredModuleName = Some [ "main" ]
      Syntax =
        { parsed.Syntax with
            ModuleHeader = Some [ "main" ]
            Declarations = declarations }
      Diagnostics = lexed.Diagnostics @ parsed.Diagnostics }

let private parsedDocument filePath text =
    let source, lexed, parsed = lexAndParse filePath text

    { Source = source
      InferredModuleName = Some [ "main" ]
      Syntax =
        { parsed.Syntax with
            ModuleHeader = Some [ "main" ] }
      Diagnostics = lexed.Diagnostics @ parsed.Diagnostics }

[<Fact>]
let ``resource quantities keep borrow mode distinct from intervals`` () =
    let one = ResourceQuantity.ofSurface QuantityOne
    let borrow = ResourceQuantity.ofSurface (QuantityBorrow None)
    let omega = ResourceQuantity.ofSurface QuantityOmega

    Assert.True(ResourceQuantity.isExactOne one)
    Assert.True(ResourceQuantity.isBorrow borrow)
    Assert.False(ResourceQuantity.isBorrow one)
    Assert.False(ResourceQuantity.isInterval borrow)
    Assert.True(ResourceQuantity.requiresUse one)
    Assert.False(ResourceQuantity.requiresUse omega)
    Assert.Equal("1", ResourceQuantity.toSurfaceText one)
    Assert.Equal("&", ResourceQuantity.toSurfaceText borrow)
    Assert.Equal("\u03c9", ResourceQuantity.toSurfaceText omega)

[<Fact>]
let ``resource quantity satisfaction follows interval and borrow rules`` () =
    let zero = ResourceQuantity.ofSurface QuantityZero
    let one = ResourceQuantity.ofSurface QuantityOne
    let omega = ResourceQuantity.ofSurface QuantityOmega
    let borrow = ResourceQuantity.ofSurface (QuantityBorrow None)

    Assert.True(ResourceQuantity.satisfies omega one)
    Assert.True(ResourceQuantity.satisfies omega (ResourceQuantity.exact 2))
    Assert.True(ResourceQuantity.satisfies borrow borrow)
    Assert.True(ResourceQuantity.satisfies borrow omega)
    Assert.False(ResourceQuantity.satisfies one borrow)
    Assert.False(ResourceQuantity.satisfies one (ResourceQuantity.exact 2))
    Assert.False(ResourceQuantity.satisfies borrow one)
    Assert.False(ResourceQuantity.satisfies zero one)

[<Fact>]
let ``resource checker preserves borrowed parameter quantities declared by a separate signature`` () =
    let text =
        [
            "module main"
            ""
            "data Handle : Type ="
            "    Handle Int"
            ""
            "openFile : String -> IO Handle"
            "let openFile name = pure (Handle 1)"
            ""
            "headerLength : (& h : Handle) -> Nat"
            "let headerLength h = 5"
            ""
            "readHeaderSafe : IO Nat"
            "let readHeaderSafe = do"
            "    using h <- openFile \"data.txt\""
            "    pure (headerLength h)"
        ]
        |> String.concat "\n"

    let document = parsedDocument "main.kp" text

    Assert.Empty(document.Diagnostics)

    let result = ResourceChecking.checkDocumentsWithFacts [ document ]

    assertDoesNotContainDiagnostic DiagnosticCode.QttBorrowEscape result.Diagnostics

[<Fact>]
let ``resource checker rejects a returned lambda that captures a borrowed parameter declared by a separate signature`` () =
    let text =
        [
            "module main"
            ""
            "data Box a : Type ="
            "    Box a"
            ""
            "read : (& x : Box Int) -> Int"
            "let read x = 0"
            ""
            "bad : (& x : Box Int) -> (Unit -> Int)"
            "let bad x = \\_ -> read x"
        ]
        |> String.concat "\n"

    let document = parsedDocument "main.kp" text

    Assert.Empty(document.Diagnostics)

    let result = ResourceChecking.checkDocumentsWithFacts [ document ]

    assertContainsDiagnostic DiagnosticCode.QttBorrowEscape result.Diagnostics

[<Fact>]
let ``resource checker rejects overusing a linear parameter captured through a separate signature`` () =
    let text =
        [
            "module main"
            ""
            "data Token : Type ="
            "    Token"
            ""
            "consume : (1 t : Token) -> Unit"
            "let consume t = ()"
            ""
            "twice : (omega f : Unit -> Unit) -> Unit"
            "let twice f = do"
            "    f ()"
            "    f ()"
            ""
            "bad : (1 t : Token) -> Unit"
            "let bad t = twice (\\_ -> consume t)"
        ]
        |> String.concat "\n"

    let document = parsedDocument "main.kp" text

    Assert.Empty(document.Diagnostics)

    let result = ResourceChecking.checkDocumentsWithFacts [ document ]

    assertContainsDiagnostic DiagnosticCode.QttLinearOveruse result.Diagnostics

[<Fact>]
let ``resource checker rejects a runtime closure that captures a quantity-zero parameter declared by a separate signature`` () =
    let text =
        [
            "module main"
            ""
            "bad : (@0 n : Nat) -> Unit -> Nat"
            "let bad n = \\_ -> n"
        ]
        |> String.concat "\n"

    let document = parsedDocument "main.kp" text

    Assert.Empty(document.Diagnostics)

    let result = ResourceChecking.checkDocumentsWithFacts [ document ]

    assertContainsDiagnostic DiagnosticCode.QttErasedRuntimeUse result.Diagnostics

[<Fact>]
let ``resource checker keeps if branch shadow bindings scoped to the branch`` () =
    let text =
        [
            "module main"
            ""
            "let consume (1 owned : File) = ()"
            "let openFile name = pure file"
            "let main = do"
            "    let 1 file = file"
            "    if True then"
            "        shadow"
            "    else"
            "        other"
            "    consume file"
        ]
        |> String.concat "\n"

    let consumeDefinition =
        LetDeclaration(
            mkLetDefinition "consume" [ mkParameter "owned" (Some QuantityOne) ] (Apply(Name [ "owned" ], [ Literal LiteralValue.Unit ]))
        )

    let openFileDefinition =
        LetDeclaration(mkLetDefinition "openFile" [ mkParameter "name" None ] (Name [ "file" ]))

    let mainDefinition =
        LetDeclaration(
            mkLetDefinition
                "main"
                []
                (Do
                    [
                        DoLet(mkBindPattern "file" (Some QuantityOne), Name [ "file" ])
                        DoExpression(
                            IfThenElse(
                                Name [ "True" ],
                                Do
                                    [
                                        DoUsing(NamePattern "file", Apply(Name [ "openFile" ], [ Literal(LiteralValue.String "data.txt") ]))
                                        DoExpression(Apply(Name [ "printInt" ], [ Literal(LiteralValue.Integer 0L) ]))
                                    ],
                                Do [ DoExpression(Apply(Name [ "printInt" ], [ Literal(LiteralValue.Integer 1L) ])) ]
                            )
                        )
                        DoExpression(Apply(Name [ "consume" ], [ Name [ "file" ] ]))
                    ])
        )

    let document =
        parsedDocumentWithDeclarations "main.kp" text [ consumeDefinition; openFileDefinition; mainDefinition ]

    let result = ResourceChecking.checkDocumentsWithFacts [ document ]

    assertDoesNotContainDiagnostic DiagnosticCode.QttLinearDrop result.Diagnostics
    assertDoesNotContainDiagnostic DiagnosticCode.QttBorrowConsume result.Diagnostics

    let ownership = result.OwnershipFactsByFile[document.Source.FilePath]

    let outerBinding =
        ownership.OwnershipBindings
        |> List.find (fun binding ->
            binding.BindingName = "file"
            && binding.BindingDeclaredQuantity = Some "1")

    Assert.Equal("consumed", outerBinding.BindingState)

    Assert.Contains(
        ownership.OwnershipBindings,
        fun binding ->
            binding.BindingName = "file"
            && binding.BindingDeclaredQuantity = Some "&"
            && binding.BindingState = "borrowed"
    )

[<Fact>]
let ``resource checker keeps nested do shadow bindings scoped to the nested block`` () =
    let text =
        [
            "module main"
            ""
            "let consume (1 owned : File) = ()"
            "let openFile name = pure file"
            "let main = do"
            "    let 1 file = file"
            "    do"
            "        shadow"
            "    consume file"
        ]
        |> String.concat "\n"

    let consumeDefinition =
        LetDeclaration(
            mkLetDefinition "consume" [ mkParameter "owned" (Some QuantityOne) ] (Apply(Name [ "owned" ], [ Literal LiteralValue.Unit ]))
        )

    let openFileDefinition =
        LetDeclaration(mkLetDefinition "openFile" [ mkParameter "name" None ] (Name [ "file" ]))

    let mainDefinition =
        LetDeclaration(
            mkLetDefinition
                "main"
                []
                (Do
                    [
                        DoLet(mkBindPattern "file" (Some QuantityOne), Name [ "file" ])
                        DoExpression(
                            Do
                                [
                                    DoUsing(NamePattern "file", Apply(Name [ "openFile" ], [ Literal(LiteralValue.String "data.txt") ]))
                                    DoExpression(Apply(Name [ "printInt" ], [ Literal(LiteralValue.Integer 0L) ]))
                                ]
                        )
                        DoExpression(Apply(Name [ "consume" ], [ Name [ "file" ] ]))
                    ])
        )

    let document =
        parsedDocumentWithDeclarations "main.kp" text [ consumeDefinition; openFileDefinition; mainDefinition ]

    let result = ResourceChecking.checkDocumentsWithFacts [ document ]

    assertDoesNotContainDiagnostic DiagnosticCode.QttLinearDrop result.Diagnostics
    assertDoesNotContainDiagnostic DiagnosticCode.QttBorrowConsume result.Diagnostics

    let ownership = result.OwnershipFactsByFile[document.Source.FilePath]

    let outerBinding =
        ownership.OwnershipBindings
        |> List.find (fun binding ->
            binding.BindingName = "file"
            && binding.BindingDeclaredQuantity = Some "1")

    Assert.Equal("consumed", outerBinding.BindingState)

    Assert.Contains(
        ownership.OwnershipBindings,
        fun binding ->
            binding.BindingName = "file"
            && binding.BindingDeclaredQuantity = Some "&"
            && binding.BindingState = "borrowed"
    )

[<Fact>]
let ``resource checker accounts for a direct local lambda call that consumes a captured linear binding`` () =
    let text =
        [
            "module main"
            ""
            "let consume (1 owned : File) = ()"
            "let main = do"
            "    let 1 file = file"
            "    let use = \\u -> consume file"
            "    use ()"
        ]
        |> String.concat "\n"

    let consumeDefinition =
        LetDeclaration(
            mkLetDefinition "consume" [ mkParameter "owned" (Some QuantityOne) ] (Apply(Name [ "owned" ], [ Literal LiteralValue.Unit ]))
        )

    let mainDefinition =
        LetDeclaration(
            mkLetDefinition
                "main"
                []
                (Do
                    [
                        DoLet(mkBindPattern "file" (Some QuantityOne), Name [ "file" ])
                        DoLet(
                            mkBindPattern "use" None,
                            Lambda(
                                [ mkParameter "u" None ],
                                Apply(Name [ "consume" ], [ Name [ "file" ] ])
                            )
                        )
                        DoExpression(Apply(Name [ "use" ], [ Literal LiteralValue.Unit ]))
                    ])
        )

    let document =
        parsedDocumentWithDeclarations "main.kp" text [ consumeDefinition; mainDefinition ]

    let result = ResourceChecking.checkDocumentsWithFacts [ document ]

    assertDoesNotContainDiagnostic DiagnosticCode.QttLinearDrop result.Diagnostics
    assertDoesNotContainDiagnostic DiagnosticCode.QttLinearOveruse result.Diagnostics

    let ownership = result.OwnershipFactsByFile[document.Source.FilePath]

    let outerBinding =
        ownership.OwnershipBindings
        |> List.find (fun binding ->
            binding.BindingName = "file"
            && binding.BindingDeclaredQuantity = Some "1")

    Assert.Equal("consumed", outerBinding.BindingState)

[<Fact>]
let ``resource checker treats a local lambda that captures a linear binding as a linear value`` () =
    let text =
        [
            "module main"
            ""
            "let consume (1 owned : File) = ()"
            "let main = do"
            "    let 1 file = file"
            "    let use = \\u -> consume file"
            "    use ()"
        ]
        |> String.concat "\n"

    let consumeDefinition =
        LetDeclaration(
            mkLetDefinition "consume" [ mkParameter "owned" (Some QuantityOne) ] (Apply(Name [ "owned" ], [ Literal LiteralValue.Unit ]))
        )

    let mainDefinition =
        LetDeclaration(
            mkLetDefinition
                "main"
                []
                (Do
                    [
                        DoLet(mkBindPattern "file" (Some QuantityOne), Name [ "file" ])
                        DoLet(
                            mkBindPattern "use" None,
                            Lambda(
                                [ mkParameter "u" None ],
                                Apply(Name [ "consume" ], [ Name [ "file" ] ])
                            )
                        )
                        DoExpression(Apply(Name [ "use" ], [ Literal LiteralValue.Unit ]))
                    ])
        )

    let document =
        parsedDocumentWithDeclarations "main.kp" text [ consumeDefinition; mainDefinition ]

    let result = ResourceChecking.checkDocumentsWithFacts [ document ]
    let ownership = result.OwnershipFactsByFile[document.Source.FilePath]

    let closureBinding =
        ownership.OwnershipBindings
        |> List.find (fun binding -> binding.BindingName = "use")

    Assert.Equal(Some "1", closureBinding.BindingDeclaredQuantity)
    Assert.Equal("consumed", closureBinding.BindingState)

    Assert.Contains(
        ownership.OwnershipUses,
        fun useFact -> useFact.UseTargetName = "use" && useFact.UseKindName = "consume"
    )

[<Fact>]
let ``resource checker reports dropping a local lambda that captures a linear binding`` () =
    let text =
        [
            "module main"
            ""
            "let consume (1 owned : File) = ()"
            "let main = do"
            "    let 1 file = file"
            "    let use = \\u -> consume file"
            "    pure ()"
        ]
        |> String.concat "\n"

    let consumeDefinition =
        LetDeclaration(
            mkLetDefinition "consume" [ mkParameter "owned" (Some QuantityOne) ] (Apply(Name [ "owned" ], [ Literal LiteralValue.Unit ]))
        )

    let mainDefinition =
        LetDeclaration(
            mkLetDefinition
                "main"
                []
                (Do
                    [
                        DoLet(mkBindPattern "file" (Some QuantityOne), Name [ "file" ])
                        DoLet(
                            mkBindPattern "use" None,
                            Lambda(
                                [ mkParameter "u" None ],
                                Apply(Name [ "consume" ], [ Name [ "file" ] ])
                            )
                        )
                        DoExpression(Literal LiteralValue.Unit)
                    ])
        )

    let document =
        parsedDocumentWithDeclarations "main.kp" text [ consumeDefinition; mainDefinition ]

    let result = ResourceChecking.checkDocumentsWithFacts [ document ]
    let ownership = result.OwnershipFactsByFile[document.Source.FilePath]

    let closureBinding =
        ownership.OwnershipBindings
        |> List.find (fun binding -> binding.BindingName = "use")

    Assert.Equal(Some "1", closureBinding.BindingDeclaredQuantity)
    Assert.Equal("unconsumed", closureBinding.BindingState)
    assertContainsDiagnostic DiagnosticCode.QttLinearDrop result.Diagnostics

[<Fact>]
let ``resource checker rejects repeated direct local lambda calls that overuse a captured linear binding`` () =
    let text =
        [
            "module main"
            ""
            "let consume (1 owned : File) = ()"
            "let main = do"
            "    let 1 file = file"
            "    let use = \\u -> consume file"
            "    use ()"
            "    use ()"
        ]
        |> String.concat "\n"

    let consumeDefinition =
        LetDeclaration(
            mkLetDefinition "consume" [ mkParameter "owned" (Some QuantityOne) ] (Apply(Name [ "owned" ], [ Literal LiteralValue.Unit ]))
        )

    let mainDefinition =
        LetDeclaration(
            mkLetDefinition
                "main"
                []
                (Do
                    [
                        DoLet(mkBindPattern "file" (Some QuantityOne), Name [ "file" ])
                        DoLet(
                            mkBindPattern "use" None,
                            Lambda(
                                [ mkParameter "u" None ],
                                Apply(Name [ "consume" ], [ Name [ "file" ] ])
                            )
                        )
                        DoExpression(Apply(Name [ "use" ], [ Literal LiteralValue.Unit ]))
                        DoExpression(Apply(Name [ "use" ], [ Literal LiteralValue.Unit ]))
                    ])
        )

    let document =
        parsedDocumentWithDeclarations "main.kp" text [ consumeDefinition; mainDefinition ]

    let result = ResourceChecking.checkDocumentsWithFacts [ document ]

    assertContainsDiagnostic DiagnosticCode.QttLinearOveruse result.Diagnostics

[<Fact>]
let ``resource checker treats a local lambda that captures an at-most-once binding as at-most-once`` () =
    let text =
        [
            "module main"
            ""
            "let main = do"
            "    let <=1 token = token"
            "    let use = \\u -> token"
            "    use ()"
            "    use ()"
        ]
        |> String.concat "\n"

    let mainDefinition =
        LetDeclaration(
            mkLetDefinition
                "main"
                []
                (Do
                    [
                        DoLet(mkBindPattern "token" (Some QuantityAtMostOne), Name [ "token" ])
                        DoLet(
                            mkBindPattern "use" None,
                            Lambda([ mkParameter "u" None ], Name [ "token" ])
                        )
                        DoExpression(Apply(Name [ "use" ], [ Literal LiteralValue.Unit ]))
                        DoExpression(Apply(Name [ "use" ], [ Literal LiteralValue.Unit ]))
                    ])
        )

    let document = parsedDocumentWithDeclarations "main.kp" text [ mainDefinition ]
    let result = ResourceChecking.checkDocumentsWithFacts [ document ]
    let ownership = result.OwnershipFactsByFile[document.Source.FilePath]

    let closureBinding =
        ownership.OwnershipBindings
        |> List.find (fun binding -> binding.BindingName = "use")

    Assert.Equal(Some "<=1", closureBinding.BindingDeclaredQuantity)
    assertContainsDiagnostic DiagnosticCode.QttLinearOveruse result.Diagnostics

[<Fact>]
let ``resource checker treats a local lambda that captures an at-least-once binding as requiring use`` () =
    let text =
        [
            "module main"
            ""
            "let main = do"
            "    let >=1 token = token"
            "    let use = \\u -> token"
            "    pure ()"
        ]
        |> String.concat "\n"

    let mainDefinition =
        LetDeclaration(
            mkLetDefinition
                "main"
                []
                (Do
                    [
                        DoLet(mkBindPattern "token" (Some QuantityAtLeastOne), Name [ "token" ])
                        DoLet(
                            mkBindPattern "use" None,
                            Lambda([ mkParameter "u" None ], Name [ "token" ])
                        )
                        DoExpression(Literal LiteralValue.Unit)
                    ])
        )

    let document = parsedDocumentWithDeclarations "main.kp" text [ mainDefinition ]
    let result = ResourceChecking.checkDocumentsWithFacts [ document ]
    let ownership = result.OwnershipFactsByFile[document.Source.FilePath]

    let closureBinding =
        ownership.OwnershipBindings
        |> List.find (fun binding -> binding.BindingName = "use")

    Assert.Equal(Some ">=1", closureBinding.BindingDeclaredQuantity)
    assertContainsDiagnostic DiagnosticCode.QttLinearDrop result.Diagnostics

[<Fact>]
let ``resource checker allows a local borrowed anonymous record pattern over a non-place expression`` () =
    let text =
        [
            "module main"
            ""
            "let mkPair unit = stub"
            "let demo = hidden"
        ]
        |> String.concat "\n"

    let mkPairDefinition =
        LetDeclaration(mkLetDefinition "mkPair" [ mkParameter "unit" None ] (Name [ "stub" ]))

    let demoDefinition =
        LetDeclaration(
            mkLetDefinition
                "demo"
                []
                (LocalLet(
                    mkAnonymousRecordBindPattern (Some(QuantityBorrow None)) [ "x", "bx"; "y", "by" ],
                    Apply(Name [ "mkPair" ], [ Literal LiteralValue.Unit ]),
                    Binary(Name [ "bx" ], "+", Name [ "by" ])
                ))
        )

    let document =
        parsedDocumentWithDeclarations "main.kp" text [ mkPairDefinition; demoDefinition ]

    let result = ResourceChecking.checkDocumentsWithFacts [ document ]

    assertDoesNotContainDiagnostic DiagnosticCode.QttBorrowEscape result.Diagnostics

    let ownership = result.OwnershipFactsByFile[document.Source.FilePath]

    let bxBinding =
        ownership.OwnershipBindings
        |> List.find (fun binding -> binding.BindingName = "bx")

    let byBinding =
        ownership.OwnershipBindings
        |> List.find (fun binding -> binding.BindingName = "by")

    Assert.Equal(Some "&", bxBinding.BindingDeclaredQuantity)
    Assert.Equal("borrowed", bxBinding.BindingState)
    Assert.Equal(Some "&", byBinding.BindingDeclaredQuantity)
    Assert.Equal("borrowed", byBinding.BindingState)
    Assert.Equal(bxBinding.BindingPlaceRoot, byBinding.BindingPlaceRoot)
    Assert.Equal<string list>([ "x" ], bxBinding.BindingPlacePath)
    Assert.Equal<string list>([ "y" ], byBinding.BindingPlacePath)

    let hiddenRootBinding =
        ownership.OwnershipBindings
        |> List.find (fun binding -> binding.BindingName = bxBinding.BindingPlaceRoot)

    Assert.Equal(Some "1", hiddenRootBinding.BindingDeclaredQuantity)
    Assert.Equal<string list>([], hiddenRootBinding.BindingPlacePath)

[<Fact>]
let ``resource checker rejects a lambda escaping a local borrowed anonymous record pattern`` () =
    let text =
        [
            "module main"
            ""
            "let mkPair unit = stub"
            "let bad = hidden"
        ]
        |> String.concat "\n"

    let mkPairDefinition =
        LetDeclaration(mkLetDefinition "mkPair" [ mkParameter "unit" None ] (Name [ "stub" ]))

    let badDefinition =
        LetDeclaration(
            mkLetDefinition
                "bad"
                []
                (LocalLet(
                    mkAnonymousRecordBindPattern (Some(QuantityBorrow None)) [ "x", "bx" ],
                    Apply(Name [ "mkPair" ], [ Literal LiteralValue.Unit ]),
                    Lambda([ mkParameter "unit" None ], Name [ "bx" ])
                ))
        )

    let document =
        parsedDocumentWithDeclarations "main.kp" text [ mkPairDefinition; badDefinition ]

    let result = ResourceChecking.checkDocumentsWithFacts [ document ]

    assertContainsDiagnostic DiagnosticCode.QttBorrowEscape result.Diagnostics

[<Fact>]
let ``resource checker preserves a named borrow root for local anonymous record pattern aliases`` () =
    let text =
        [
            "module main"
            ""
            "let demo = hidden"
        ]
        |> String.concat "\n"

    let demoDefinition =
        LetDeclaration(
            mkLetDefinition
                "demo"
                []
                (Do
                    [
                        DoLet(mkBindPattern "pair" (Some QuantityOne), Name [ "pair" ])
                        DoExpression(
                            LocalLet(
                                mkAnonymousRecordBindPattern (Some(QuantityBorrow None)) [ "x", "bx"; "y", "by" ],
                                Name [ "pair" ],
                                Binary(Name [ "bx" ], "+", Name [ "by" ])
                            )
                        )
                    ])
        )

    let document = parsedDocumentWithDeclarations "main.kp" text [ demoDefinition ]
    let result = ResourceChecking.checkDocumentsWithFacts [ document ]
    let ownership = result.OwnershipFactsByFile[document.Source.FilePath]

    let bxBinding =
        ownership.OwnershipBindings
        |> List.find (fun binding -> binding.BindingName = "bx")

    let byBinding =
        ownership.OwnershipBindings
        |> List.find (fun binding -> binding.BindingName = "by")

    Assert.Equal("pair", bxBinding.BindingPlaceRoot)
    Assert.Equal<string list>([ "x" ], bxBinding.BindingPlacePath)
    Assert.Equal("pair", byBinding.BindingPlaceRoot)
    Assert.Equal<string list>([ "y" ], byBinding.BindingPlacePath)

[<Fact>]
let ``resource checker rejects reprojecting a consumed linear field path`` () =
    let text =
        [
            "module main"
            ""
            "data Buffer : Type ="
            "    Buffer"
            ""
            "bad : (1 r : (1 buf : Buffer, len : Nat)) -> Buffer"
            "let bad r ="
            "    let old = r.buf"
            "    r.buf"
        ]
        |> String.concat "\n"

    let document = parsedDocument "main.kp" text

    Assert.Empty(document.Diagnostics)

    let result = ResourceChecking.checkDocumentsWithFacts [ document ]

    assertContainsDiagnostic DiagnosticCode.QttLinearOveruse result.Diagnostics

[<Fact>]
let ``resource checker rejects consuming a whole record after moving a linear field`` () =
    let text =
        [
            "module main"
            ""
            "data Buffer : Type ="
            "    Buffer"
            ""
            "consumeWhole : (1 r : (1 buf : Buffer, len : Nat)) -> Unit"
            "let consumeWhole r = ()"
            ""
            "bad : (1 r : (1 buf : Buffer, len : Nat)) -> Unit"
            "let bad r ="
            "    let old = r.buf"
            "    consumeWhole r"
        ]
        |> String.concat "\n"

    let document = parsedDocument "main.kp" text

    Assert.Empty(document.Diagnostics)

    let result = ResourceChecking.checkDocumentsWithFacts [ document ]

    assertContainsDiagnostic DiagnosticCode.QttLinearOveruse result.Diagnostics

[<Fact>]
let ``resource checker allows reading an unrestricted sibling field after moving and consuming a linear field`` () =
    let text =
        [
            "module main"
            ""
            "data Buffer : Type ="
            "    Buffer"
            ""
            "consume : (1 old : Buffer) -> Unit"
            "let consume old = ()"
            ""
            "demo : (1 r : (1 buf : Buffer, len : Nat)) -> Nat"
            "let demo r ="
            "    let old = r.buf"
            "    let used = consume old"
            "    r.len"
        ]
        |> String.concat "\n"

    let document = parsedDocument "main.kp" text

    Assert.Empty(document.Diagnostics)

    let result = ResourceChecking.checkDocumentsWithFacts [ document ]

    assertDoesNotContainDiagnostic DiagnosticCode.QttLinearOveruse result.Diagnostics
    assertDoesNotContainDiagnostic DiagnosticCode.QttLinearDrop result.Diagnostics

[<Fact>]
let ``resource checker allows a consumed record field path to be restored by record update`` () =
    let text =
        [
            "module main"
            ""
            "data Buffer : Type ="
            "    Buffer"
            ""
            "process : (1 old : Buffer) -> Buffer"
            "let process old = old"
            ""
            "repair : (1 r : (1 buf : Buffer, len : Nat)) -> (1 buf : Buffer, len : Nat)"
            "let repair r ="
            "    let old = r.buf"
            "    let new = process old"
            "    r.{ buf = new }"
        ]
        |> String.concat "\n"

    let document = parsedDocument "main.kp" text

    Assert.Empty(document.Diagnostics)

    let result = ResourceChecking.checkDocumentsWithFacts [ document ]

    assertDoesNotContainDiagnostic DiagnosticCode.QttLinearOveruse result.Diagnostics
    assertDoesNotContainDiagnostic DiagnosticCode.QttLinearDrop result.Diagnostics

[<Fact>]
let ``resource checker rejects a record update that omits a dependent field repair`` () =
    let text =
        [
            "module main"
            ""
            "data Byte : Type ="
            "    Byte"
            ""
            "data Array n a : Type ="
            "    Array"
            ""
            "type SizedBuffer = (len : Nat, buffer : Array this.len Byte, checksum : Nat)"
            ""
            "bad : SizedBuffer -> SizedBuffer"
            "let bad buf = buf.{ len = 20 }"
        ]
        |> String.concat "\n"

    let document = parsedDocument "main.kp" text

    Assert.Empty(document.Diagnostics)

    let result = ResourceChecking.checkDocumentsWithFacts [ document ]

    assertContainsDiagnostic DiagnosticCode.TypeEqualityMismatch result.Diagnostics

[<Fact>]
let ``resource checker allows a record update that repairs a dependent field`` () =
    let text =
        [
            "module main"
            ""
            "data Byte : Type ="
            "    Byte"
            ""
            "data Array n a : Type ="
            "    Array"
            ""
            "mkBuffer : (len : Nat) -> Array len Byte"
            "let mkBuffer len = Array"
            ""
            "type SizedBuffer = (len : Nat, buffer : Array this.len Byte, checksum : Nat)"
            ""
            "grow : SizedBuffer -> SizedBuffer"
            "let grow buf ="
            "    buf.{ len = 20, buffer = mkBuffer this.len }"
        ]
        |> String.concat "\n"

    let document = parsedDocument "main.kp" text

    Assert.Empty(document.Diagnostics)

    let result = ResourceChecking.checkDocumentsWithFacts [ document ]

    assertDoesNotContainDiagnostic DiagnosticCode.TypeEqualityMismatch result.Diagnostics

[<Fact>]
let ``resource checker tracks repeated callee-position use of an at-most-once parameter`` () =
    let text =
        [
            "module main"
            ""
            "let invoke (<=1 f : Unit -> Unit) = do"
            "    f ()"
            "    f ()"
        ]
        |> String.concat "\n"

    let invokeDefinition =
        LetDeclaration(
            mkLetDefinition
                "invoke"
                [ mkParameter "f" (Some QuantityAtMostOne) ]
                (Do
                    [
                        DoExpression(Apply(Name [ "f" ], [ Literal LiteralValue.Unit ]))
                        DoExpression(Apply(Name [ "f" ], [ Literal LiteralValue.Unit ]))
                    ])
        )

    let document = parsedDocumentWithDeclarations "main.kp" text [ invokeDefinition ]
    let result = ResourceChecking.checkDocumentsWithFacts [ document ]

    assertContainsDiagnostic DiagnosticCode.QttLinearOveruse result.Diagnostics

[<Fact>]
let ``resource checker tracks repeated argument-position use of an at-most-once binding`` () =
    let text =
        [
            "module main"
            ""
            "let maybeUse (<=1 x : File) = ()"
            "let main = do"
            "    let <=1 file = file"
            "    maybeUse file"
            "    maybeUse file"
        ]
        |> String.concat "\n"

    let maybeUseDefinition =
        LetDeclaration(mkLetDefinition "maybeUse" [ mkParameter "x" (Some QuantityAtMostOne) ] (Literal LiteralValue.Unit))

    let mainDefinition =
        LetDeclaration(
            mkLetDefinition
                "main"
                []
                (Do
                    [
                        DoLet(mkBindPattern "file" (Some QuantityAtMostOne), Name [ "file" ])
                        DoExpression(Apply(Name [ "maybeUse" ], [ Name [ "file" ] ]))
                        DoExpression(Apply(Name [ "maybeUse" ], [ Name [ "file" ] ]))
                    ])
        )

    let document = parsedDocumentWithDeclarations "main.kp" text [ maybeUseDefinition; mainDefinition ]
    let result = ResourceChecking.checkDocumentsWithFacts [ document ]

    assertContainsDiagnostic DiagnosticCode.QttLinearOveruse result.Diagnostics

[<Fact>]
let ``resource checker counts an at-least-once argument use through a function call`` () =
    let text =
        [
            "module main"
            ""
            "let mustUse (>=1 x : File) = ()"
            "let main = do"
            "    let >=1 file = file"
            "    mustUse file"
        ]
        |> String.concat "\n"

    let mustUseDefinition =
        LetDeclaration(mkLetDefinition "mustUse" [ mkParameter "x" (Some QuantityAtLeastOne) ] (Literal LiteralValue.Unit))

    let mainDefinition =
        LetDeclaration(
            mkLetDefinition
                "main"
                []
                (Do
                    [
                        DoLet(mkBindPattern "file" (Some QuantityAtLeastOne), Name [ "file" ])
                        DoExpression(Apply(Name [ "mustUse" ], [ Name [ "file" ] ]))
                    ])
        )

    let document = parsedDocumentWithDeclarations "main.kp" text [ mustUseDefinition; mainDefinition ]
    let result = ResourceChecking.checkDocumentsWithFacts [ document ]

    assertDoesNotContainDiagnostic DiagnosticCode.QttLinearDrop result.Diagnostics

[<Fact>]
let ``resource checker rejects returning a borrowed parameter directly`` () =
    let text =
        [
            "module main"
            ""
            "data File : Type ="
            "    Handle Int"
            ""
            "let leak (& file : File) = file"
        ]
        |> String.concat "\n"

    let leakDefinition =
        LetDeclaration(mkLetDefinition "leak" [ mkParameter "file" (Some(QuantityBorrow None)) ] (Name [ "file" ]))

    let document = parsedDocumentWithDeclarations "main.kp" text [ leakDefinition ]
    let result = ResourceChecking.checkDocumentsWithFacts [ document ]

    assertContainsDiagnostic DiagnosticCode.QttBorrowEscape result.Diagnostics

[<Fact>]
let ``resource checker rejects returning a using borrow as the result of a do block`` () =
    let text =
        [
            "module main"
            ""
            "data File : Type ="
            "    Handle Int"
            ""
            "let openFile name = pure (Handle 1)"
            ""
            "let leak : IO File = do"
            "    using file <- openFile \"data.txt\""
            "    file"
        ]
        |> String.concat "\n"

    let openFileDefinition =
        LetDeclaration(mkLetDefinition "openFile" [ mkParameter "name" None ] (Name [ "file" ]))

    let leakDefinition =
        LetDeclaration(
            mkLetDefinition
                "leak"
                []
                (Do
                    [
                        DoUsing(NamePattern "file", Apply(Name [ "openFile" ], [ Literal(LiteralValue.String "data.txt") ]))
                        DoExpression(Name [ "file" ])
                    ])
        )

    let document = parsedDocumentWithDeclarations "main.kp" text [ openFileDefinition; leakDefinition ]
    let result = ResourceChecking.checkDocumentsWithFacts [ document ]

    assertContainsDiagnostic DiagnosticCode.QttBorrowEscape result.Diagnostics

[<Fact>]
let ``resource checker rejects a local lambda whose borrowed parameter escapes through its result`` () =
    let text =
        [
            "module main"
            ""
            "data File : Type ="
            "    Handle Int"
            ""
            "let main : IO Unit = do"
            "    let bad = \\(& file : File) -> file"
            "    ()"
        ]
        |> String.concat "\n"

    let mainDefinition =
        LetDeclaration(
            mkLetDefinition
                "main"
                []
                (Do
                    [
                        DoLet(
                            mkBindPattern "bad" None,
                            Lambda([ mkParameter "file" (Some(QuantityBorrow None)) ], Name [ "file" ])
                        )
                        DoExpression(Literal LiteralValue.Unit)
                    ])
        )

    let document = parsedDocumentWithDeclarations "main.kp" text [ mainDefinition ]
    let result = ResourceChecking.checkDocumentsWithFacts [ document ]

    assertContainsDiagnostic DiagnosticCode.QttBorrowEscape result.Diagnostics

[<Fact>]
let ``resource checker lets a match constructor pattern bind a linear field for consumption`` () =
    let text =
        [
            "module main"
            ""
            "data File : Type ="
            "    Handle Int"
            ""
            "data Box : Type ="
            "    Box File"
            ""
            "let consume (1 file : File) = ()"
            ""
            "let main : IO Unit = do"
            "    let 1 box = box"
            "    match box"
            "    case Box file -> consume file"
        ]
        |> String.concat "\n"

    let consumeDefinition =
        LetDeclaration(
            mkLetDefinition "consume" [ mkParameter "file" (Some QuantityOne) ] (Apply(Name [ "file" ], [ Literal LiteralValue.Unit ]))
        )

    let mainDefinition =
        LetDeclaration(
            mkLetDefinition
                "main"
                []
                (Do
                    [
                        DoLet(mkBindPattern "box" (Some QuantityOne), Name [ "box" ])
                        DoExpression(
                            Match(
                                Name [ "box" ],
                                [
                                    { Pattern = ConstructorPattern([ "Box" ], [ NamePattern "file" ])
                                      Guard = None
                                      Body = Apply(Name [ "consume" ], [ Name [ "file" ] ]) }
                                ]
                            )
                        )
                    ])
        )

    let document = parsedDocumentWithDeclarations "main.kp" text [ consumeDefinition; mainDefinition ]
    let result = ResourceChecking.checkDocumentsWithFacts [ document ]

    assertDoesNotContainDiagnostic DiagnosticCode.QttLinearDrop result.Diagnostics
    assertDoesNotContainDiagnostic DiagnosticCode.QttLinearOveruse result.Diagnostics

    let ownership = result.OwnershipFactsByFile[document.Source.FilePath]
    Assert.DoesNotContain("match-pattern-resource-checking", ownership.OwnershipDeferred)

[<Fact>]
let ``resource checker propagates borrowed match bindings as borrowed aliases`` () =
    let text =
        [
            "module main"
            ""
            "data File : Type ="
            "    Handle Int"
            ""
            "let consume (1 file : File) = ()"
            ""
            "let main : IO Unit = do"
            "    let & file = Handle 1"
            "    match file"
            "    case alias -> consume alias"
        ]
        |> String.concat "\n"

    let consumeDefinition =
        LetDeclaration(
            mkLetDefinition "consume" [ mkParameter "file" (Some QuantityOne) ] (Apply(Name [ "file" ], [ Literal LiteralValue.Unit ]))
        )

    let mainDefinition =
        LetDeclaration(
            mkLetDefinition
                "main"
                []
                (Do
                    [
                        DoLet(mkBindPattern "file" (Some(QuantityBorrow None)), Name [ "file" ])
                        DoExpression(
                            Match(
                                Name [ "file" ],
                                [
                                    { Pattern = NamePattern "alias"
                                      Guard = None
                                      Body = Apply(Name [ "consume" ], [ Name [ "alias" ] ]) }
                                ]
                            )
                        )
                    ])
        )

    let document = parsedDocumentWithDeclarations "main.kp" text [ consumeDefinition; mainDefinition ]
    let result = ResourceChecking.checkDocumentsWithFacts [ document ]

    assertContainsDiagnostic DiagnosticCode.QttBorrowConsume result.Diagnostics

    let ownership = result.OwnershipFactsByFile[document.Source.FilePath]
    Assert.DoesNotContain("match-pattern-resource-checking", ownership.OwnershipDeferred)

[<Fact>]
let ``resource checker keeps multi-binder match patterns deferred`` () =
    let text =
        [
            "module main"
            ""
            "data File : Type ="
            "    Handle Int"
            ""
            "data Pair : Type ="
            "    Pair File File"
            ""
            "let consume (1 file : File) = ()"
            ""
            "let main : IO Unit = do"
            "    let 1 pair = pair"
            "    match pair"
            "    case Pair left right -> consume left"
        ]
        |> String.concat "\n"

    let consumeDefinition =
        LetDeclaration(
            mkLetDefinition "consume" [ mkParameter "file" (Some QuantityOne) ] (Apply(Name [ "file" ], [ Literal LiteralValue.Unit ]))
        )

    let mainDefinition =
        LetDeclaration(
            mkLetDefinition
                "main"
                []
                (Do
                    [
                        DoLet(mkBindPattern "pair" (Some QuantityOne), Name [ "pair" ])
                        DoExpression(
                            Match(
                                Name [ "pair" ],
                                [
                                    { Pattern = ConstructorPattern([ "Pair" ], [ NamePattern "left"; NamePattern "right" ])
                                      Guard = None
                                      Body = Apply(Name [ "consume" ], [ Name [ "left" ] ]) }
                                ]
                            )
                        )
                    ])
        )

    let document = parsedDocumentWithDeclarations "main.kp" text [ consumeDefinition; mainDefinition ]
    let result = ResourceChecking.checkDocumentsWithFacts [ document ]
    let ownership = result.OwnershipFactsByFile[document.Source.FilePath]

    Assert.Contains("match-pattern-resource-checking", ownership.OwnershipDeferred)

[<Fact>]
let ``resource checker allows a borrowed value to be used in a local non-escaping call result`` () =
    let text =
        [
            "module main"
            ""
            "data Handle : Type ="
            "    Handle Int"
            ""
            "let openFile name = pure (Handle 1)"
            "let headerLength (& h : Handle) = 5"
            ""
            "let main : IO Unit = do"
            "    using h <- openFile \"data.txt\""
            "    printInt (headerLength h)"
        ]
        |> String.concat "\n"

    let openFileDefinition =
        LetDeclaration(mkLetDefinition "openFile" [ mkParameter "name" None ] (Name [ "file" ]))

    let headerLengthDefinition =
        LetDeclaration(mkLetDefinition "headerLength" [ mkParameter "h" (Some(QuantityBorrow None)) ] (Literal(LiteralValue.Integer 5L)))

    let mainDefinition =
        LetDeclaration(
            mkLetDefinition
                "main"
                []
                (Do
                    [
                        DoUsing(NamePattern "h", Apply(Name [ "openFile" ], [ Literal(LiteralValue.String "data.txt") ]))
                        DoExpression(Apply(Name [ "printInt" ], [ Apply(Name [ "headerLength" ], [ Name [ "h" ] ]) ]))
                    ])
        )

    let document =
        parsedDocumentWithDeclarations "main.kp" text [ openFileDefinition; headerLengthDefinition; mainDefinition ]

    let result = ResourceChecking.checkDocumentsWithFacts [ document ]

    assertDoesNotContainDiagnostic DiagnosticCode.QttBorrowEscape result.Diagnostics
