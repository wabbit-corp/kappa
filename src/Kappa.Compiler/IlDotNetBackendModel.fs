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

    type internal IlType =
        | IlPrimitive of IlPrimitiveType
        | IlNamed of moduleName: string * typeName: string * arguments: IlType list
        | IlTypeParameter of string

    type internal RawConstructorInfo =
        { Name: string
          FieldTypeTexts: string list
          Arity: int }

    type internal RawDataTypeInfo =
        { ModuleName: string
          Name: string
          TypeParameters: string list
          Constructors: RawConstructorInfo list
          ExternalRuntimeTypeName: string option
          EmittedTypeName: string }

    type internal ConstructorInfo =
        { ModuleName: string
          Name: string
          TypeName: string
          TypeParameters: string list
          FieldTypes: IlType list
          EmittedTypeName: string }

    type internal DataTypeInfo =
        { ModuleName: string
          Name: string
          TypeParameters: string list
          Constructors: Map<string, ConstructorInfo>
          ExternalRuntimeTypeName: string option
          EmittedTypeName: string }

    type internal ModuleSurface<'binding> =
        { Name: string
          Imports: ImportSpec list
          Exports: Set<string>
          TypeExports: Set<string>
          DataTypes: Map<string, DataTypeInfo>
          Constructors: Map<string, ConstructorInfo>
          Bindings: Map<string, 'binding>
          EmittedTypeName: string option }

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
        { ModuleName: string
          TraitName: string
          InstanceKey: string
          HeadTypes: IlType list
          MemberBindings: Map<string, string> }

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

    type internal DataTypeEmission =
        { BaseTypeBuilder: TypeBuilder
          GenericParameters: GenericTypeParameterBuilder array
          BaseConstructor: ConstructorBuilder
          Constructors: Map<string, ConstructorEmission> }

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

    let emittedModuleTypeName (moduleName: string) =
        let segments =
            moduleName.Split('.', StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries)
            |> Array.map sanitizeIdentifier

        if Array.isEmpty segments then
            invalidOp "Module name cannot be empty."

        "Kappa.Generated." + String.concat "." segments

    let emittedMethodName (bindingName: string) =
        sanitizeIdentifier bindingName

    let internal emittedDataTypeName (moduleName: string) (typeName: string) =
        emittedModuleTypeName moduleName + "." + sanitizeIdentifier typeName

    let internal emittedConstructorTypeName (moduleName: string) (constructorName: string) =
        emittedModuleTypeName moduleName + "." + sanitizeIdentifier constructorName

    let internal primitiveRuntimeType =
        function
        | IlInt64 -> typeof<int64>
        | IlFloat64 -> typeof<double>
        | IlBool -> typeof<bool>
        | IlString -> typeof<string>
        | IlChar -> typeof<char>

    let internal primitiveTypeName =
        function
        | IlInt64 -> "Int"
        | IlFloat64 -> "Float"
        | IlBool -> "Bool"
        | IlString -> "String"
        | IlChar -> "Char"

    let internal unitIlType =
        IlNamed("std.prelude", "Unit", [])

    let internal refIlType elementType =
        IlNamed("std.prelude", "Ref", [ elementType ])

    let internal dictionaryIlType traitName argumentTypes =
        IlNamed("std.prelude", TraitRuntime.dictionaryTypeName traitName, argumentTypes)

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
        | IlNamed (_, typeName, []) ->
            typeName
        | IlNamed (_, typeName, arguments) ->
            let argumentText = arguments |> List.map formatIlType |> String.concat " "
            $"{typeName} {argumentText}"
