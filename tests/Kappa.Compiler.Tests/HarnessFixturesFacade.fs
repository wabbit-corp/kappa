module HarnessFixtures

type KpFixtureMode = HarnessFixtureModel.KpFixtureMode
type KpFixtureDirectiveSource = HarnessFixtureModel.KpFixtureDirectiveSource
type KpFixtureRelation = HarnessFixtureModel.KpFixtureRelation
type KpFixtureConfiguration = HarnessFixtureModel.KpFixtureConfiguration
type KpFixtureAssertion = HarnessFixtureModel.KpFixtureAssertion
type KpFixtureCase = HarnessFixtureModel.KpFixtureCase

module KpFixtureConfiguration =
    let defaultValue = HarnessFixtureModel.KpFixtureConfiguration.defaultValue

let discoverKpFixtureCases = HarnessFixtureDiscovery.discoverKpFixtureCases
