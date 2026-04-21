namespace Kappa.Compiler

module ResourceModel =
    type ResourceQuantity =
        | Interval of minimum: int * maximum: int option
        | Borrow of explicitRegion: string option
        | Variable of name: string

    module ResourceQuantity =
        let zero = Interval(0, Some 0)
        let one = Interval(1, Some 1)
        let omega = Interval(0, None)
        let atMostOne = Interval(0, Some 1)
        let atLeastOne = Interval(1, None)
        let exact count = Interval(count, Some count)

        let ofSurface quantity =
            match quantity with
            | QuantityZero -> zero
            | QuantityOne -> one
            | QuantityBorrow explicitRegion -> Borrow explicitRegion
            | QuantityOmega -> omega
            | QuantityAtMostOne -> atMostOne
            | QuantityAtLeastOne -> atLeastOne
            | QuantityVariable name -> Variable name

        let toSurfaceText quantity =
            match quantity with
            | Interval(0, Some 0) -> "0"
            | Interval(1, Some 1) -> "1"
            | Interval(0, None) -> Quantity.toSurfaceText QuantityOmega
            | Interval(0, Some 1) -> "<=1"
            | Interval(1, None) -> ">=1"
            | Interval(minimum, Some maximum) -> $"[{minimum},{maximum}]"
            | Interval(minimum, None) -> $"[{minimum},inf]"
            | Borrow None -> "&"
            | Borrow(Some explicitRegion) -> $"&[{explicitRegion}]"
            | Variable name -> name

        let isExactOne quantity =
            match quantity with
            | Interval(1, Some 1) -> true
            | _ -> false

        let isBorrow quantity =
            match quantity with
            | Borrow _ -> true
            | _ -> false

        let isInterval quantity =
            match quantity with
            | Interval _ -> true
            | _ -> false

        let private maximumContains capability demand =
            match capability, demand with
            | None, _ -> true
            | Some _, None -> false
            | Some capabilityMaximum, Some demandMaximum -> demandMaximum <= capabilityMaximum

        let satisfies capability demand =
            match capability, demand with
            | Interval(capabilityMinimum, capabilityMaximum), Interval(demandMinimum, demandMaximum) ->
                capabilityMinimum <= demandMinimum && maximumContains capabilityMaximum demandMaximum
            | Borrow _, Borrow _ -> true
            | Borrow _, Interval(0, None) -> true
            | _ -> false

    type FunctionSignature =
        { Name: string
          ParameterQuantities: ResourceQuantity option list
          ParameterInout: bool list }

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
          DeclaredQuantity: ResourceQuantity option
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
