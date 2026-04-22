namespace Kappa.Compiler

type ClrAssemblyParameter =
    { Name: string
      TypeText: string option }

type ClrAssemblyConstructor =
    { Name: string
      Arity: int
      TypeName: string
      FieldTypeTexts: string list
      Provenance: KCoreOrigin }

type ClrAssemblyDataType =
    { Name: string
      TypeParameters: string list
      Constructors: ClrAssemblyConstructor list }

type ClrAssemblyTraitInstance =
    { TraitName: string
      InstanceKey: string
      HeadTypeTexts: string list
      MemberBindings: (string * string) list }

type ClrAssemblyBinding =
    { Name: string
      Parameters: ClrAssemblyParameter list
      ReturnTypeText: string option
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

[<RequireQualifiedAccess>]
module ClrAssemblyIR =
    let private ofRuntimeParameter (parameter: KRuntimeParameter) : ClrAssemblyParameter =
        { Name = parameter.Name
          TypeText = parameter.TypeText }

    let private ofRuntimeConstructor (constructor: KRuntimeConstructor) : ClrAssemblyConstructor =
        { Name = constructor.Name
          Arity = constructor.Arity
          TypeName = constructor.TypeName
          FieldTypeTexts = constructor.FieldTypeTexts
          Provenance = constructor.Provenance }

    let private ofRuntimeDataType (dataType: KRuntimeDataType) : ClrAssemblyDataType =
        { Name = dataType.Name
          TypeParameters = dataType.TypeParameters
          Constructors = dataType.Constructors |> List.map ofRuntimeConstructor }

    let private ofRuntimeTraitInstance (instanceDeclaration: KRuntimeTraitInstance) : ClrAssemblyTraitInstance =
        { TraitName = instanceDeclaration.TraitName
          InstanceKey = instanceDeclaration.InstanceKey
          HeadTypeTexts = instanceDeclaration.HeadTypeTexts
          MemberBindings = instanceDeclaration.MemberBindings }

    let private ofRuntimeBinding (binding: KRuntimeBinding) : ClrAssemblyBinding =
        { Name = binding.Name
          Parameters = binding.Parameters |> List.map ofRuntimeParameter
          ReturnTypeText = binding.ReturnTypeText
          Body = binding.Body
          Intrinsic = binding.Intrinsic
          Provenance = binding.Provenance }

    let ofRuntimeModule (moduleDump: KRuntimeModule) : ClrAssemblyModule =
        { Name = moduleDump.Name
          SourceFile = moduleDump.SourceFile
          Imports = moduleDump.Imports
          Exports = moduleDump.Exports
          IntrinsicTerms = moduleDump.IntrinsicTerms
          DataTypes = moduleDump.DataTypes |> List.map ofRuntimeDataType
          TraitInstances = moduleDump.TraitInstances |> List.map ofRuntimeTraitInstance
          Bindings = moduleDump.Bindings |> List.map ofRuntimeBinding }
