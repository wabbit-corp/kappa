module FixtureTestSupport

open Harness
open Xunit

let private shardCount = 24

let private allFixtures =
    lazy (discoverKpFixtureCases () |> List.toArray)

let fixtureCasesForShard shardIndex : seq<obj array> =
    allFixtures.Value
    |> Seq.indexed
    |> Seq.choose (fun (index, fixture) ->
        if index % shardCount = shardIndex then
            Some [| box fixture |]
        else
            None)

let runFixtureCase (fixture: KpFixtureCase) =
    Assert.NotEmpty(allFixtures.Value)
    runKpFixtureCase fixture

module FixtureTestsShard0 =
    type FixtureCaseData() =
        static member Cases : seq<obj array> = fixtureCasesForShard 0

    [<Theory>]
    [<MemberData(nameof FixtureCaseData.Cases, MemberType = typeof<FixtureCaseData>)>]
    let ``raw kp fixtures satisfy their assertions shard 0`` (fixture: KpFixtureCase) =
        runFixtureCase fixture

module FixtureTestsShard1 =
    type FixtureCaseData() =
        static member Cases : seq<obj array> = fixtureCasesForShard 1

    [<Theory>]
    [<MemberData(nameof FixtureCaseData.Cases, MemberType = typeof<FixtureCaseData>)>]
    let ``raw kp fixtures satisfy their assertions shard 1`` (fixture: KpFixtureCase) =
        runFixtureCase fixture

module FixtureTestsShard2 =
    type FixtureCaseData() =
        static member Cases : seq<obj array> = fixtureCasesForShard 2

    [<Theory>]
    [<MemberData(nameof FixtureCaseData.Cases, MemberType = typeof<FixtureCaseData>)>]
    let ``raw kp fixtures satisfy their assertions shard 2`` (fixture: KpFixtureCase) =
        runFixtureCase fixture

module FixtureTestsShard3 =
    type FixtureCaseData() =
        static member Cases : seq<obj array> = fixtureCasesForShard 3

    [<Theory>]
    [<MemberData(nameof FixtureCaseData.Cases, MemberType = typeof<FixtureCaseData>)>]
    let ``raw kp fixtures satisfy their assertions shard 3`` (fixture: KpFixtureCase) =
        runFixtureCase fixture

module FixtureTestsShard4 =
    type FixtureCaseData() =
        static member Cases : seq<obj array> = fixtureCasesForShard 4

    [<Theory>]
    [<MemberData(nameof FixtureCaseData.Cases, MemberType = typeof<FixtureCaseData>)>]
    let ``raw kp fixtures satisfy their assertions shard 4`` (fixture: KpFixtureCase) =
        runFixtureCase fixture

module FixtureTestsShard5 =
    type FixtureCaseData() =
        static member Cases : seq<obj array> = fixtureCasesForShard 5

    [<Theory>]
    [<MemberData(nameof FixtureCaseData.Cases, MemberType = typeof<FixtureCaseData>)>]
    let ``raw kp fixtures satisfy their assertions shard 5`` (fixture: KpFixtureCase) =
        runFixtureCase fixture

module FixtureTestsShard6 =
    type FixtureCaseData() =
        static member Cases : seq<obj array> = fixtureCasesForShard 6

    [<Theory>]
    [<MemberData(nameof FixtureCaseData.Cases, MemberType = typeof<FixtureCaseData>)>]
    let ``raw kp fixtures satisfy their assertions shard 6`` (fixture: KpFixtureCase) =
        runFixtureCase fixture

module FixtureTestsShard7 =
    type FixtureCaseData() =
        static member Cases : seq<obj array> = fixtureCasesForShard 7

    [<Theory>]
    [<MemberData(nameof FixtureCaseData.Cases, MemberType = typeof<FixtureCaseData>)>]
    let ``raw kp fixtures satisfy their assertions shard 7`` (fixture: KpFixtureCase) =
        runFixtureCase fixture

module FixtureTestsShard8 =
    type FixtureCaseData() =
        static member Cases : seq<obj array> = fixtureCasesForShard 8

    [<Theory>]
    [<MemberData(nameof FixtureCaseData.Cases, MemberType = typeof<FixtureCaseData>)>]
    let ``raw kp fixtures satisfy their assertions shard 8`` (fixture: KpFixtureCase) =
        runFixtureCase fixture

module FixtureTestsShard9 =
    type FixtureCaseData() =
        static member Cases : seq<obj array> = fixtureCasesForShard 9

    [<Theory>]
    [<MemberData(nameof FixtureCaseData.Cases, MemberType = typeof<FixtureCaseData>)>]
    let ``raw kp fixtures satisfy their assertions shard 9`` (fixture: KpFixtureCase) =
        runFixtureCase fixture

module FixtureTestsShard10 =
    type FixtureCaseData() =
        static member Cases : seq<obj array> = fixtureCasesForShard 10

    [<Theory>]
    [<MemberData(nameof FixtureCaseData.Cases, MemberType = typeof<FixtureCaseData>)>]
    let ``raw kp fixtures satisfy their assertions shard 10`` (fixture: KpFixtureCase) =
        runFixtureCase fixture

module FixtureTestsShard11 =
    type FixtureCaseData() =
        static member Cases : seq<obj array> = fixtureCasesForShard 11

    [<Theory>]
    [<MemberData(nameof FixtureCaseData.Cases, MemberType = typeof<FixtureCaseData>)>]
    let ``raw kp fixtures satisfy their assertions shard 11`` (fixture: KpFixtureCase) =
        runFixtureCase fixture

module FixtureTestsShard12 =
    type FixtureCaseData() =
        static member Cases : seq<obj array> = fixtureCasesForShard 12

    [<Theory>]
    [<MemberData(nameof FixtureCaseData.Cases, MemberType = typeof<FixtureCaseData>)>]
    let ``raw kp fixtures satisfy their assertions shard 12`` (fixture: KpFixtureCase) =
        runFixtureCase fixture

module FixtureTestsShard13 =
    type FixtureCaseData() =
        static member Cases : seq<obj array> = fixtureCasesForShard 13

    [<Theory>]
    [<MemberData(nameof FixtureCaseData.Cases, MemberType = typeof<FixtureCaseData>)>]
    let ``raw kp fixtures satisfy their assertions shard 13`` (fixture: KpFixtureCase) =
        runFixtureCase fixture

module FixtureTestsShard14 =
    type FixtureCaseData() =
        static member Cases : seq<obj array> = fixtureCasesForShard 14

    [<Theory>]
    [<MemberData(nameof FixtureCaseData.Cases, MemberType = typeof<FixtureCaseData>)>]
    let ``raw kp fixtures satisfy their assertions shard 14`` (fixture: KpFixtureCase) =
        runFixtureCase fixture

module FixtureTestsShard15 =
    type FixtureCaseData() =
        static member Cases : seq<obj array> = fixtureCasesForShard 15

    [<Theory>]
    [<MemberData(nameof FixtureCaseData.Cases, MemberType = typeof<FixtureCaseData>)>]
    let ``raw kp fixtures satisfy their assertions shard 15`` (fixture: KpFixtureCase) =
        runFixtureCase fixture

module FixtureTestsShard16 =
    type FixtureCaseData() =
        static member Cases : seq<obj array> = fixtureCasesForShard 16

    [<Theory>]
    [<MemberData(nameof FixtureCaseData.Cases, MemberType = typeof<FixtureCaseData>)>]
    let ``raw kp fixtures satisfy their assertions shard 16`` (fixture: KpFixtureCase) =
        runFixtureCase fixture

module FixtureTestsShard17 =
    type FixtureCaseData() =
        static member Cases : seq<obj array> = fixtureCasesForShard 17

    [<Theory>]
    [<MemberData(nameof FixtureCaseData.Cases, MemberType = typeof<FixtureCaseData>)>]
    let ``raw kp fixtures satisfy their assertions shard 17`` (fixture: KpFixtureCase) =
        runFixtureCase fixture

module FixtureTestsShard18 =
    type FixtureCaseData() =
        static member Cases : seq<obj array> = fixtureCasesForShard 18

    [<Theory>]
    [<MemberData(nameof FixtureCaseData.Cases, MemberType = typeof<FixtureCaseData>)>]
    let ``raw kp fixtures satisfy their assertions shard 18`` (fixture: KpFixtureCase) =
        runFixtureCase fixture

module FixtureTestsShard19 =
    type FixtureCaseData() =
        static member Cases : seq<obj array> = fixtureCasesForShard 19

    [<Theory>]
    [<MemberData(nameof FixtureCaseData.Cases, MemberType = typeof<FixtureCaseData>)>]
    let ``raw kp fixtures satisfy their assertions shard 19`` (fixture: KpFixtureCase) =
        runFixtureCase fixture

module FixtureTestsShard20 =
    type FixtureCaseData() =
        static member Cases : seq<obj array> = fixtureCasesForShard 20

    [<Theory>]
    [<MemberData(nameof FixtureCaseData.Cases, MemberType = typeof<FixtureCaseData>)>]
    let ``raw kp fixtures satisfy their assertions shard 20`` (fixture: KpFixtureCase) =
        runFixtureCase fixture

module FixtureTestsShard21 =
    type FixtureCaseData() =
        static member Cases : seq<obj array> = fixtureCasesForShard 21

    [<Theory>]
    [<MemberData(nameof FixtureCaseData.Cases, MemberType = typeof<FixtureCaseData>)>]
    let ``raw kp fixtures satisfy their assertions shard 21`` (fixture: KpFixtureCase) =
        runFixtureCase fixture

module FixtureTestsShard22 =
    type FixtureCaseData() =
        static member Cases : seq<obj array> = fixtureCasesForShard 22

    [<Theory>]
    [<MemberData(nameof FixtureCaseData.Cases, MemberType = typeof<FixtureCaseData>)>]
    let ``raw kp fixtures satisfy their assertions shard 22`` (fixture: KpFixtureCase) =
        runFixtureCase fixture

module FixtureTestsShard23 =
    type FixtureCaseData() =
        static member Cases : seq<obj array> = fixtureCasesForShard 23

    [<Theory>]
    [<MemberData(nameof FixtureCaseData.Cases, MemberType = typeof<FixtureCaseData>)>]
    let ``raw kp fixtures satisfy their assertions shard 23`` (fixture: KpFixtureCase) =
        runFixtureCase fixture
