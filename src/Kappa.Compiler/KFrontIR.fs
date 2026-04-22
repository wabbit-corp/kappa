namespace Kappa.Compiler

type KFrontIRModule =
    { FilePath: string
      ModuleHeader: string list option
      InferredModuleName: string list option
      ModuleIdentity: string list option
      ModuleAttributes: string list
      Imports: ImportSpec list
      Tokens: Token list
      Declarations: TopLevelDeclaration list
      Diagnostics: Diagnostic list
      Ownership: OwnershipFactSet option
      ResolvedPhases: Set<KFrontIRPhase> }
