namespace Kappa.Compiler

open System
open System.Collections.Generic
open System.IO
open System.Reflection
open System.Reflection.Emit
open System.Runtime.CompilerServices

// Defines the CLR emitter's internal type, module, and emission-state model.
module internal IlDotNetBackendModel =
    type internal IlPrimitiveType =
        | IlInt64
        | IlFloat64
        | IlBool
        | IlString
        | IlChar

    type internal DataTypeRepresentation =
        | IlClassHierarchy
        | IlClrEnum

    type internal IlType =
        | IlPrimitive of IlPrimitiveType
        | IlNamed of identity: TypeIdentity * arguments: IlType list
        | IlTypeParameter of string

    type internal RawConstructorInfo =
        { Identity: DeclarationIdentity
          ResultType: TypeIdentity
          FieldTypeTexts: string list
          Arity: int
          CaseOrdinal: int }
        member this.Name = DeclarationIdentity.name this.Identity

    type internal RawDataTypeInfo =
        { Identity: TypeIdentity
          TypeParameters: string list
          Constructors: RawConstructorInfo list
          Representation: DataTypeRepresentation
          ExternalRuntimeTypeName: string option
          EmittedTypeName: string }
        member this.ModuleName = TypeIdentity.moduleIdentity this.Identity |> ModuleIdentity.text
        member this.Name = TypeIdentity.name this.Identity

    type internal ConstructorInfo =
        { Identity: DeclarationIdentity
          ResultType: TypeIdentity
          TypeParameters: string list
          FieldTypes: IlType list
          CaseOrdinal: int
          EmittedTypeName: string }
        member this.ModuleName = DeclarationIdentity.moduleIdentity this.Identity |> ModuleIdentity.text
        member this.Name = DeclarationIdentity.name this.Identity
        member this.TypeName = TypeIdentity.name this.ResultType

    type internal DataTypeInfo =
        { Identity: TypeIdentity
          TypeParameters: string list
          Constructors: Map<string, ConstructorInfo>
          Representation: DataTypeRepresentation
          ExternalRuntimeTypeName: string option
          EmittedTypeName: string }
        member this.ModuleName = TypeIdentity.moduleIdentity this.Identity |> ModuleIdentity.text
        member this.Name = TypeIdentity.name this.Identity

    type internal ModuleSurface<'binding> =
        { Identity: ModuleIdentity
          Imports: ImportSpec list
          Exports: Set<string>
          TypeExports: Set<string>
          DataTypes: Map<string, DataTypeInfo>
          Constructors: Map<string, ConstructorInfo>
          Bindings: Map<string, 'binding>
          EmittedTypeName: string option }
        member this.Name = ModuleIdentity.text this.Identity

    type internal BindingInfo =
        { Binding: ClrAssemblyBinding
          ParameterTypes: (string * IlType) list
          ReturnType: IlType
          TypeParameters: string list
          EmittedMethodName: string }

    type internal DeclaredBindingTypes =
        { ParameterTypes: IlType list option
          ReturnType: IlType option }

    type internal DeclaredBindingTexts =
        { ParameterTypes: string option list
          ReturnType: string option }

    type internal TraitInstanceInfo =
        { ModuleIdentity: ModuleIdentity
          Trait: TypeSignatures.TraitReference
          InstanceKey: string
          HeadTypes: IlType list
          MemberBindings: Map<string, string> }
        member this.ModuleName = ModuleIdentity.text this.ModuleIdentity
        member this.TraitName = TypeSignatures.TraitReference.text this.Trait

    type internal RawModuleInfo = ModuleSurface<ClrAssemblyBinding>
    type internal ModuleInfo = ModuleSurface<BindingInfo>

    type internal EmissionEnvironment =
        { Modules: Map<string, ModuleInfo>
          DataTypes: Map<string * string, DataTypeInfo>
          TraitInstances: TraitInstanceInfo list }

    type internal ConstructorEmission =
        { TypeBuilder: TypeBuilder
          GenericParameters: GenericTypeParameterBuilder array
          ConstructorBuilder: ConstructorBuilder
          FieldBuilders: FieldBuilder array }

    type internal EnumCaseEmission =
        { LiteralField: FieldBuilder
          CaseOrdinal: int }

    type internal ClassHierarchyEmission =
        { BaseTypeBuilder: TypeBuilder
          GenericParameters: GenericTypeParameterBuilder array
          BaseConstructor: ConstructorBuilder
          Constructors: Map<string, ConstructorEmission> }

    type internal EnumEmission =
        { EnumBuilder: EnumBuilder
          Cases: Map<string, EnumCaseEmission> }

    type internal DataTypeEmission =
        | ClassHierarchyEmission of ClassHierarchyEmission
        | EnumEmission of EnumEmission

    type internal EmissionState =
        { Environment: EmissionEnvironment
          ModuleBuilders: Map<string, TypeBuilder>
          MethodBuilders: Map<string, Map<string, MethodEmission>>
          DataTypeBuilders: Map<string * string, DataTypeEmission> }

    and internal MethodEmission =
        { Builder: MethodBuilder
          GenericParameters: Map<string, Type> }

    type internal ValueLocation =
        | Argument of int
        | Local of LocalBuilder

    type internal LocalValue =
        { Location: ValueLocation
          Type: IlType }

    type internal ResultBuilder() =
        member _.Bind(result, binder) = Result.bind binder result
        member _.Return(value) = Result.Ok value
        member _.ReturnFrom(result) = result
        member _.Zero() = Result.Ok()
        member _.Delay(generator) = generator
        member _.Run(generator) = generator()
        member _.Combine(first, second) =
            first |> Result.bind (fun () -> second())

    let internal result = ResultBuilder()

    let internal aggregateDiagnostics diagnostics =
        diagnostics
        |> List.map (fun diagnostic -> diagnostic.Message)
        |> String.concat Environment.NewLine

    let internal sanitizeIdentifier (value: string) =
        let text =
            value
            |> Seq.collect (fun ch ->
                if Char.IsLetterOrDigit(ch) || ch = '_' then
                    Seq.singleton(string ch)
                else
                    Seq.singleton($"_u{int ch:x4}"))
            |> String.concat ""

        if String.IsNullOrWhiteSpace(text) then
            "_"
        elif Char.IsLetter(text[0]) || text[0] = '_' then
            text
        else
            "_" + text

    let private emittedModuleTypeNameFromIdentity (moduleIdentity: ModuleIdentity) =
        let segments =
            moduleIdentity
            |> ModuleIdentity.segments
            |> List.map sanitizeIdentifier
            |> List.toArray

        if Array.isEmpty segments then
            invalidOp "Module name cannot be empty."

        "Kappa.Generated." + String.concat "." segments

    let emittedModuleTypeName (moduleName: string) =
        moduleName |> ModuleIdentity.ofDottedTextUnchecked |> emittedModuleTypeNameFromIdentity

    let emittedMethodName (bindingName: string) =
        sanitizeIdentifier bindingName

    let internal emittedDataTypeName (typeIdentity: TypeIdentity) =
        emittedModuleTypeNameFromIdentity (TypeIdentity.moduleIdentity typeIdentity)
        + "."
        + sanitizeIdentifier (TypeIdentity.name typeIdentity)

    let internal emittedConstructorTypeName (constructorIdentity: DeclarationIdentity) =
        emittedModuleTypeNameFromIdentity (DeclarationIdentity.moduleIdentity constructorIdentity)
        + "."
        + sanitizeIdentifier (DeclarationIdentity.name constructorIdentity)

    let internal primitiveRuntimeType =
        function
        | IlInt64 -> typeof<int64>
        | IlFloat64 -> typeof<double>
        | IlBool -> typeof<bool>
        | IlString -> typeof<string>
        | IlChar -> typeof<char>

    let internal primitiveTypeName =
        function
        | IlInt64 -> Stdlib.KnownTypeNames.Int
        | IlFloat64 -> Stdlib.KnownTypeNames.Float
        | IlBool -> Stdlib.KnownTypeNames.Bool
        | IlString -> Stdlib.KnownTypeNames.String
        | IlChar -> Stdlib.KnownTypeNames.Char

    let internal preludeModuleIdentity = Stdlib.PreludeModuleIdentity

    let internal preludeTypeIdentity name =
        TypeIdentity.topLevel preludeModuleIdentity name

    let internal namedIlType typeIdentity arguments =
        IlNamed(typeIdentity, arguments)

    let internal unitIlType =
        IlNamed(preludeTypeIdentity Stdlib.KnownTypeNames.Unit, [])

    let internal refIlType elementType =
        IlNamed(preludeTypeIdentity Stdlib.KnownTypeNames.Ref, [ elementType ])

    let internal dictionaryIlType traitName argumentTypes =
        IlNamed(preludeTypeIdentity (TraitRuntime.dictionaryTypeName traitName), argumentTypes)

    let internal isUnitIlType ilType =
        ilType = unitIlType

    let internal isDictionaryTypeName (typeName: string) =
        typeName.StartsWith("__kappa_dict_", StringComparison.Ordinal)

    let rec internal formatIlType ilType =
        match ilType with
        | IlPrimitive primitiveType ->
            primitiveTypeName primitiveType
        | IlTypeParameter name ->
            name
        | IlNamed (identity, []) ->
            TypeIdentity.name identity
        | IlNamed (identity, arguments) ->
            let argumentText = arguments |> List.map formatIlType |> String.concat " "
            $"{TypeIdentity.name identity} {argumentText}"
