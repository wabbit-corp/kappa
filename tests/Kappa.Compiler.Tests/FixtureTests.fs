module FixtureTestSupport

open Harness
open Xunit

let private shardCount = 8

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
