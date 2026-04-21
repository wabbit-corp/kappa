namespace Kappa.Compiler

module ResourceModel =
    type FunctionSignature =
        { Name: string
          ParameterQuantities: Quantity option list }

    type ResourcePlace =
        { Root: string
          Path: string list }

    module ResourcePlace =
        let root name =
            { Root = name
              Path = [] }

    type BorrowRegion =
        { Id: string
          ExplicitName: string option
          OwnerScope: string }

    type ResourceBinding =
        { Id: string
          Name: string
          DeclaredQuantity: Quantity option
          Place: ResourcePlace
          BorrowRegion: BorrowRegion option
          CapturedRegions: Set<string>
          CapturedBindingOrigins: SourceLocation list
          UseMinimum: int
          UseMaximum: int
          CheckLinearDrop: bool
          ClosureFactId: string option
          Origin: SourceLocation option
          FirstConsumeOrigin: SourceLocation option }

    type ResourceContext =
        { ScopeId: string
          Bindings: Map<string, ResourceBinding>
          Diagnostics: Diagnostic list
          Events: OwnershipUseFact list
          BorrowRegions: OwnershipBorrowRegionFact list
          UsingScopes: OwnershipUsingScopeFact list
          Closures: OwnershipClosureFact list
          DeferredFacts: string list
          NextBindingId: int
          NextEventId: int
          NextRegionId: int
          NextUsingScopeId: int
          NextClosureId: int }

    module ResourceContext =
        let empty scopeId =
            { ScopeId = scopeId
              Bindings = Map.empty
              Diagnostics = []
              Events = []
              BorrowRegions = []
              UsingScopes = []
              Closures = []
              DeferredFacts = []
              NextBindingId = 0
              NextEventId = 0
              NextRegionId = 0
              NextUsingScopeId = 0
              NextClosureId = 0 }
