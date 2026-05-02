namespace Kappa.Compiler

type ClrAssemblyParameter =
    { Name: string
      Type: TypeSignatures.TypeExpr option }

type ClrAssemblyConstructor =
    { Name: string
      Arity: int
      TypeName: string
      FieldTypes: TypeSignatures.TypeExpr list
      Provenance: KCoreOrigin }

type ClrAssemblyDataType =
    { Name: string
      TypeParameters: string list
      Constructors: ClrAssemblyConstructor list
      ExternalRuntimeTypeName: string option }

type ClrAssemblyTraitInstance =
    { TraitName: string
      InstanceKey: string
      HeadTypes: TypeSignatures.TypeExpr list
      MemberBindings: (string * string) list }

type ClrAssemblyBinding =
    { Name: string
      Parameters: ClrAssemblyParameter list
      ReturnType: TypeSignatures.TypeExpr option
      ExternalBinding: ExternalRuntimeBinding option
      Body: KRuntimeExpression option
      Intrinsic: bool
      Provenance: KCoreOrigin }

type ClrAssemblyModule =
    { Name: string
      SourceFile: string
      Imports: ImportSpec list
      Exports: string list
      IntrinsicTerms: string list
      DataTypes: ClrAssemblyDataType list
      TraitInstances: ClrAssemblyTraitInstance list
      Bindings: ClrAssemblyBinding list }

// Converts KRuntimeIR modules into the managed assembly model consumed by CLR lowering.
[<RequireQualifiedAccess>]
module ClrAssemblyIR =
    let private bindingUsesEffectRuntime (binding: ClrAssemblyBinding) =
        binding.Body |> Option.exists KRuntimeIR.containsEffectRuntimeConstruct

    let modulesUseEffectRuntime (modules: ClrAssemblyModule list) =
        modules
        |> List.exists (fun moduleDump ->
            moduleDump.Bindings |> List.exists bindingUsesEffectRuntime)

    let private ofRuntimeParameter (parameter: KRuntimeParameter) : ClrAssemblyParameter =
        { Name = parameter.Name
          Type = parameter.Type }

    let private ofRuntimeConstructor (constructor: KRuntimeConstructor) : ClrAssemblyConstructor =
        { Name = constructor.Name
          Arity = constructor.Arity
          TypeName = constructor.TypeName
          FieldTypes = constructor.FieldTypes
          Provenance = constructor.Provenance }

    let private ofRuntimeDataType (dataType: KRuntimeDataType) : ClrAssemblyDataType =
        { Name = dataType.Name
          TypeParameters = dataType.TypeParameters
          Constructors = dataType.Constructors |> List.map ofRuntimeConstructor
          ExternalRuntimeTypeName = dataType.ExternalRuntimeTypeName }

    let private ofRuntimeTraitInstance (instanceDeclaration: KRuntimeTraitInstance) : ClrAssemblyTraitInstance =
        { TraitName = instanceDeclaration.TraitName
          InstanceKey = instanceDeclaration.InstanceKey
          HeadTypes = instanceDeclaration.HeadTypes
          MemberBindings = instanceDeclaration.MemberBindings }

    let private ofRuntimeBinding (binding: KRuntimeBinding) : ClrAssemblyBinding =
        { Name = binding.Name
          Parameters = binding.Parameters |> List.map ofRuntimeParameter
          ReturnType = binding.ReturnType
          ExternalBinding = binding.ExternalBinding
          Body = binding.Body
          Intrinsic = binding.Intrinsic
          Provenance = binding.Provenance }

    let requiredExternalAssemblyPaths (modules: ClrAssemblyModule list) =
        modules
        |> List.collect (fun moduleDump ->
            moduleDump.Bindings
            |> List.collect (fun binding ->
                binding.ExternalBinding
                |> Option.map ExternalRuntimeBinding.requiredAssemblyPaths
                |> Option.defaultValue []))
        |> List.distinct

    let ofRuntimeModule (moduleDump: KRuntimeModule) : ClrAssemblyModule =
        { Name = moduleDump.Name
          SourceFile = moduleDump.SourceFile
          Imports = moduleDump.Imports
          Exports = moduleDump.Exports
          IntrinsicTerms = moduleDump.IntrinsicTerms
          DataTypes = moduleDump.DataTypes |> List.map ofRuntimeDataType
          TraitInstances = moduleDump.TraitInstances |> List.map ofRuntimeTraitInstance
          Bindings = moduleDump.Bindings |> List.map ofRuntimeBinding }
