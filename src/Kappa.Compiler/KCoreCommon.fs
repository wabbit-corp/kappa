namespace Kappa.Compiler

// Common provenance shared by KCore-derived checkpoints.
type KCoreOrigin =
    { FilePath: string
      ModuleIdentity: ModuleIdentity option
      DeclarationName: string option
      IntroductionKind: string }

module KCoreOrigin =
    let moduleNameText (origin: KCoreOrigin) =
        origin.ModuleIdentity
        |> Option.map ModuleIdentity.text
        |> Option.defaultValue "<unknown>"
