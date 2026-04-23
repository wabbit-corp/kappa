// Exercises the standalone resource-model helpers used by M3 checking.
module ResourceModelTests

open Kappa.Compiler
open Kappa.Compiler.ResourceModel
open HarnessWorkspace
open Xunit

let private mkParameter name quantity =
    { Name = name
      TypeTokens = None
      Quantity = quantity
      IsImplicit = false
      IsInout = false }

let private mkBindPattern name quantity =
    { Pattern = NamePattern name
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

    Assert.DoesNotContain(result.Diagnostics, fun diagnostic -> diagnostic.Code = "E_QTT_LINEAR_DROP")
    Assert.DoesNotContain(result.Diagnostics, fun diagnostic -> diagnostic.Code = "E_QTT_BORROW_CONSUME")

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

    Assert.DoesNotContain(result.Diagnostics, fun diagnostic -> diagnostic.Code = "E_QTT_LINEAR_DROP")
    Assert.DoesNotContain(result.Diagnostics, fun diagnostic -> diagnostic.Code = "E_QTT_BORROW_CONSUME")

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

    Assert.DoesNotContain(result.Diagnostics, fun diagnostic -> diagnostic.Code = "E_QTT_LINEAR_DROP")
    Assert.DoesNotContain(result.Diagnostics, fun diagnostic -> diagnostic.Code = "E_QTT_LINEAR_OVERUSE")

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
    Assert.Contains(result.Diagnostics, fun diagnostic -> diagnostic.Code = "E_QTT_LINEAR_DROP")

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

    Assert.Contains(result.Diagnostics, fun diagnostic -> diagnostic.Code = "E_QTT_LINEAR_OVERUSE")

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
    Assert.Contains(result.Diagnostics, fun diagnostic -> diagnostic.Code = "E_QTT_LINEAR_OVERUSE")

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
    Assert.Contains(result.Diagnostics, fun diagnostic -> diagnostic.Code = "E_QTT_LINEAR_DROP")

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

    Assert.Contains(result.Diagnostics, fun diagnostic -> diagnostic.Code = "E_QTT_LINEAR_OVERUSE")

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

    Assert.Contains(result.Diagnostics, fun diagnostic -> diagnostic.Code = "E_QTT_LINEAR_OVERUSE")

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

    Assert.DoesNotContain(result.Diagnostics, fun diagnostic -> diagnostic.Code = "E_QTT_LINEAR_DROP")

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

    Assert.Contains(result.Diagnostics, fun diagnostic -> diagnostic.Code = "E_QTT_BORROW_ESCAPE")

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

    Assert.Contains(result.Diagnostics, fun diagnostic -> diagnostic.Code = "E_QTT_BORROW_ESCAPE")

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

    Assert.Contains(result.Diagnostics, fun diagnostic -> diagnostic.Code = "E_QTT_BORROW_ESCAPE")
