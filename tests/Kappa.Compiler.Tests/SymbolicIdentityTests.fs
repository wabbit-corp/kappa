module SymbolicIdentityTests

open Kappa.Compiler
open Xunit

[<Fact>]
let ``ModuleIdentity preserves segments and render spelling`` () =
    let identity = ModuleIdentity.ofSegments [ "Std"; "Prelude" ]

    Assert.Equal<string list>([ "Std"; "Prelude" ], ModuleIdentity.segments identity)
    Assert.Equal("Std.Prelude", ModuleIdentity.text identity)

[<Fact>]
let ``ModuleIdentity ASCII case-fold key is segment-wise`` () =
    let identity = ModuleIdentity.ofSegments [ "Std"; "HTTP_2"; "Client1" ]

    Assert.Equal<string list>([ "std"; "http_2"; "client1" ], ModuleIdentity.asciiCaseFoldKey identity)

[<Fact>]
let ``ModuleIdentity maps dotted module specifiers to semantic identities`` () =
    let identity = ModuleIdentity.ofModuleSpecifier (Dotted [ "Std"; "Prelude" ])

    Assert.Equal(Some(ModuleIdentity.ofSegments [ "Std"; "Prelude" ]), identity)

[<Fact>]
let ``ModuleIdentity lifts optional path segments`` () =
    let someIdentity = ModuleIdentity.ofOptionalSegments (Some [ "Std"; "Prelude" ])
    let noneIdentity = ModuleIdentity.ofOptionalSegments None

    Assert.Equal(Some(ModuleIdentity.ofSegments [ "Std"; "Prelude" ]), someIdentity)
    Assert.Equal(None, noneIdentity)

[<Fact>]
let ``ModuleIdentity ignores URL module specifiers`` () =
    let identity =
        ModuleIdentity.ofModuleSpecifier
            (Url
                { OriginalText = "\"https://example.test/std/prelude.kp\""
                  BaseUrl = "https://example.test/std/prelude.kp"
                  Pin = None })

    Assert.Equal(None, identity)

[<Fact>]
let ``TypeIdentity keeps module identity and top-level name structured`` () =
    let moduleIdentity = ModuleIdentity.ofSegments [ "std"; "prelude" ]
    let identity = TypeIdentity.topLevel moduleIdentity "Bool"

    Assert.Equal(moduleIdentity, TypeIdentity.moduleIdentity identity)
    Assert.Equal<string list>([], TypeIdentity.scopePath identity)
    Assert.Equal("Bool", TypeIdentity.name identity)

[<Fact>]
let ``TypeIdentity can represent scoped names without flattening them to text`` () =
    let moduleIdentity = ModuleIdentity.ofSegments [ "Example" ]
    let identity = TypeIdentity.create moduleIdentity [ "Outer"; "Inner" ] "T"

    Assert.Equal(moduleIdentity, TypeIdentity.moduleIdentity identity)
    Assert.Equal<string list>([ "Outer"; "Inner" ], TypeIdentity.scopePath identity)
    Assert.Equal("T", TypeIdentity.name identity)

[<Fact>]
let ``KnownTypes classify bare and prelude spellings to the same symbolic type`` () =
    let bare = CompilerKnownSymbols.KnownTypes.tryClassifyName [ "Option" ]
    let prelude = CompilerKnownSymbols.KnownTypes.tryClassifyName [ "std"; "prelude"; "Option" ]

    Assert.Equal(Some CompilerKnownSymbols.OptionType, bare)
    Assert.Equal(Some CompilerKnownSymbols.OptionType, prelude)

[<Fact>]
let ``TypeSignatures known type helpers construct and match symbolic heads`` () =
    let payload = TypeSignatures.knownType CompilerKnownSymbols.StringType []
    let optionType = TypeSignatures.knownType CompilerKnownSymbols.OptionType [ payload ]

    let arguments = TypeSignatures.tryKnownTypeArguments CompilerKnownSymbols.OptionType optionType

    Assert.Equal(Some [ payload ], arguments)

[<Fact>]
let ``DeclarationIdentity keeps module, scope, name, and kind structured`` () =
    let moduleIdentity = ModuleIdentity.ofSegments [ "std"; "prelude" ]
    let identity =
        DeclarationIdentity.create moduleIdentity [ "Outer"; "Inner" ] "map" TermDeclaration

    Assert.Equal(moduleIdentity, DeclarationIdentity.moduleIdentity identity)
    Assert.Equal<string list>([ "Outer"; "Inner" ], DeclarationIdentity.scopePath identity)
    Assert.Equal("map", DeclarationIdentity.name identity)
    Assert.Equal(TermDeclaration, DeclarationIdentity.kind identity)

[<Fact>]
let ``SemanticObjectIdentity layers reified facets over declaration identity without flattening`` () =
    let declarationIdentity =
        DeclarationIdentity.topLevel (ModuleIdentity.ofSegments [ "std"; "prelude" ]) "Bool" TypeFacetDeclaration

    let semanticIdentity = SemanticObjectIdentity.create declarationIdentity TypeObject

    Assert.Equal(declarationIdentity, SemanticObjectIdentity.declarationIdentity semanticIdentity)
    Assert.Equal(TypeObject, SemanticObjectIdentity.kind semanticIdentity)

[<Fact>]
let ``Effect semantics preserve declaration identity and reified object identities`` () =
    let declarationIdentity =
        DeclarationIdentity.topLevel (ModuleIdentity.ofSegments [ "Example" ]) "State" EffectDeclaration

    let declaration =
        { EffectInterfaceId = Some "effect-interface:test"
          EffectLabelId = Some "effect-label:test"
          Visibility = None
          Name = "State"
          HeaderTokens = []
          Operations =
            [ { OperationId = Some "effect-op:get"
                Name = "get"
                ResumptionQuantity = None
                SignatureTokens = [] } ] }

    let semanticDeclaration = EffectSemantics.toSemantic (Some declarationIdentity) declaration

    Assert.Equal(Some declarationIdentity, semanticDeclaration.Identity)
    Assert.Equal(
        Some(SemanticObjectIdentity.create declarationIdentity TypeObject),
        semanticDeclaration.InterfaceObjectIdentity
    )
    Assert.Equal(
        Some(SemanticObjectIdentity.create declarationIdentity EffectLabelObject),
        semanticDeclaration.LabelObjectIdentity
    )
