namespace Kappa.Compiler

// Common provenance shared by KCore-derived checkpoints.
type KCoreOrigin =
    { FilePath: string
      ModuleName: string
      DeclarationName: string option
      IntroductionKind: string }
