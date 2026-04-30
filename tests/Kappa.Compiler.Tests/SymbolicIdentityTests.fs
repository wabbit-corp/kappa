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
let ``ModuleIdentity ignores URL module specifiers`` () =
    let identity =
        ModuleIdentity.ofModuleSpecifier
            (Url
                { OriginalText = "\"https://example.test/std/prelude.kp\""
                  BaseUrl = "https://example.test/std/prelude.kp"
                  Pin = None })

    Assert.Equal(None, identity)
