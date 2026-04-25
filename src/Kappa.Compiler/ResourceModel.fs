namespace Kappa.Compiler

// Defines quantities, regions, places, ownership events, and checker state shared across M3 analysis.
module ResourceModel =
    type ResourceBindingKind =
        | LocalBinding
        | ParameterBinding
        | PatternBinding
        | UsingOwnedBinding

    module ResourceBindingKind =
        let toOwnershipKind kind =
            match kind with
            | LocalBinding -> OwnershipBindingKind.Local
            | ParameterBinding -> OwnershipBindingKind.Parameter
            | PatternBinding -> OwnershipBindingKind.Pattern
            | UsingOwnedBinding -> OwnershipBindingKind.UsingOwned

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

        let toOwnershipQuantity quantity =
            match quantity with
            | Interval(minimum, maximum) -> OwnershipQuantity.Interval(minimum, maximum)
            | Borrow explicitRegion -> OwnershipQuantity.Borrow explicitRegion
            | Variable name -> OwnershipQuantity.Variable name

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

        let minimumRequiredUses quantity =
            match quantity with
            | Interval(minimum, _) -> minimum
            | _ -> 0

        let requiresUse quantity =
            minimumRequiredUses quantity > 0

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
          ParameterTypeTokens: Token list option list
          ReturnTypeTokens: Token list option
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

    type BorrowLock =
        { Id: string
          Root: string
          FootprintPaths: string list list
          Origin: SourceLocation option }

    type LocalLambda =
        { Identity: string option
          Parameters: Parameter list
          Body: SurfaceExpression
          CapturedBindings: ResourceBinding list }

    and ResourceBinding =
        { Id: string
          BindingKind: ResourceBindingKind
          Name: string
          DeclaredQuantity: ResourceQuantity option
          Place: ResourcePlace
          ConsumedPaths: string list list
          RecordFieldDependencies: Map<string, Set<string>>
          RecordFieldQuantities: Map<string, ResourceQuantity>
          BorrowRegion: BorrowRegion option
          CapturedRegions: Set<string>
          CapturedBindingOrigins: SourceLocation list
          UseMinimum: int
          UseMaximum: int
          CheckLinearDrop: bool
          ClosureFactId: string option
          LocalLambda: LocalLambda option
          Origin: SourceLocation option
          FirstConsumeOrigin: SourceLocation option }

    type UsingObligation =
        { Id: string
          ScopeId: string
          HiddenOwnedBindingId: string
          HiddenOwnedBindingName: string
          SharedRegionId: string
          SurfaceOrigin: SourceLocation option }

    type ResourceScope =
        { Id: string
          IntroducedBindings: (string * string) list
          IntroducedBorrowLocks: string list
          IntroducedUsingObligations: string list }

    type ResourceContext =
        { ScopeId: string
          ActiveScopes: ResourceScope list
          ActiveBindingIds: Map<string, string list>
          Bindings: Map<string, ResourceBinding>
          BorrowLocks: Map<string, BorrowLock>
          UsingObligations: Map<string, UsingObligation>
          Diagnostics: Diagnostic list
          Events: OwnershipUseFact list
          BorrowRegions: OwnershipBorrowRegionFact list
          UsingScopes: OwnershipUsingScopeFact list
          Closures: OwnershipClosureFact list
          DeferredFacts: OwnershipDeferredFact list
          ActiveLocalLambdaInvocations: string list
          NextScopeId: int
          NextBindingId: int
          NextBorrowLockId: int
          NextEventId: int
          NextRegionId: int
          NextUsingScopeId: int
          NextClosureId: int }

    module ResourceContext =
        let empty scopeId =
            { ScopeId = scopeId
              ActiveScopes =
                [
                    { Id = scopeId
                      IntroducedBindings = []
                      IntroducedBorrowLocks = []
                      IntroducedUsingObligations = [] }
                ]
              ActiveBindingIds = Map.empty
              Bindings = Map.empty
              BorrowLocks = Map.empty
              UsingObligations = Map.empty
              Diagnostics = []
              Events = []
              BorrowRegions = []
              UsingScopes = []
              Closures = []
              DeferredFacts = []
              ActiveLocalLambdaInvocations = []
              NextScopeId = 0
              NextBindingId = 0
              NextBorrowLockId = 0
              NextEventId = 0
              NextRegionId = 0
              NextUsingScopeId = 0
              NextClosureId = 0 }
