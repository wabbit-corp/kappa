module FixtureTests

open Harness
open Xunit

type FixtureCaseData() =
    static member Cases : seq<obj array> =
        discoverKpFixtureCases ()
        |> Seq.map (fun fixture -> [| box fixture |])

[<Theory>]
[<MemberData(nameof FixtureCaseData.Cases, MemberType = typeof<FixtureCaseData>)>]
let ``raw kp fixtures satisfy their assertions`` (fixture: KpFixtureCase) =
    let fixtures = discoverKpFixtureCases ()

    Assert.NotEmpty(fixtures)
    runKpFixtureCase fixture
