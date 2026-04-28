namespace Kappa.Compiler

open System
open System.Collections.Generic
open System.Reflection

// Synthesizes backend-provided host binding module surfaces from CLR metadata.
module HostBindings =
    type HostRoot =
        | HostJvm
        | HostJvmJni
        | HostDotNet
        | HostNative

    type HostModuleScopeKind =
        | ManagedTypeScope

    type HostBindingParameter =
        { Name: string
          TypeText: string
          IsReceiver: bool }

    type HostBindingMember =
        { Name: string
          SourceName: string
          Parameters: HostBindingParameter list
          ReturnTypeText: string }

    type HostTypeExport =
        { Name: string
          ExternalRuntimeTypeName: string }

    type HostModuleDescription =
        { ModuleName: string
          ScopeKind: HostModuleScopeKind
          Types: HostTypeExport list
          Terms: HostBindingMember list }

    type DotNetHostCallable =
        | HostConstructor of ConstructorInfo
        | HostMethod of MethodInfo
        | HostPropertyGetter of MethodInfo

    type HostImportResolution =
        | NotHostModule
        | UnsupportedBackend of rootText: string
        | ModuleNotFound
        | Resolved of HostModuleDescription

    let private hostJvmRoot = [ "host"; "jvm" ]
    let private hostJvmJniRoot = [ "host"; "jvm"; "jni" ]
    let private hostDotNetRoot = [ "host"; "dotnet" ]
    let private hostNativeRoot = [ "host"; "native" ]

    let private startsWithSegments prefix segments =
        List.length segments >= List.length prefix
        && List.forall2 (fun left right -> String.Equals(left, right, StringComparison.Ordinal)) prefix (segments |> List.take prefix.Length)

    let private tryReservedRoot segments =
        if startsWithSegments hostJvmJniRoot segments then
            Some(HostJvmJni, SyntaxFacts.moduleNameToText hostJvmJniRoot)
        elif startsWithSegments hostJvmRoot segments then
            Some(HostJvm, SyntaxFacts.moduleNameToText hostJvmRoot)
        elif startsWithSegments hostDotNetRoot segments then
            Some(HostDotNet, SyntaxFacts.moduleNameToText hostDotNetRoot)
        elif startsWithSegments hostNativeRoot segments then
            Some(HostNative, SyntaxFacts.moduleNameToText hostNativeRoot)
        else
            None

    let isReservedHostModuleName segments =
        tryReservedRoot segments |> Option.isSome

    let private backendProvidesRoot backendProfile hostRoot =
        match Stdlib.normalizeBackendProfile backendProfile, hostRoot with
        | "dotnet", HostDotNet -> true
        | _ -> false

    let private dotNetTypeName segments =
        segments |> List.skip hostDotNetRoot.Length |> String.concat "."

    let private tryFindClrType (fullName: string) =
        Type.GetType(fullName, throwOnError = false, ignoreCase = false)
        |> Option.ofObj
        |> Option.orElseWith (fun () ->
            AppDomain.CurrentDomain.GetAssemblies()
            |> Seq.tryPick (fun assembly ->
                try
                    assembly.GetType(fullName, throwOnError = false, ignoreCase = false) |> Option.ofObj
                with _ ->
                    None))

    let private isSupportedManagedPrimitive (clrType: Type) =
        clrType = typeof<string>
        || clrType = typeof<char>
        || clrType = typeof<bool>
        || clrType = typeof<sbyte>
        || clrType = typeof<byte>
        || clrType = typeof<int16>
        || clrType = typeof<uint16>
        || clrType = typeof<int32>
        || clrType = typeof<uint32>
        || clrType = typeof<int64>
        || clrType = typeof<uint64>
        || clrType = typeof<float32>
        || clrType = typeof<float>

    let private tryMapManagedTypeText (ownerType: Type) (clrType: Type) =
        if clrType = typeof<Void> then
            Some "Unit"
        elif clrType = typeof<string> then
            Some "String"
        elif clrType = typeof<char> then
            Some "Char"
        elif clrType = typeof<bool> then
            Some "Bool"
        elif clrType = typeof<float32> || clrType = typeof<float> then
            Some "Float"
        elif clrType.IsEnum
             || clrType = typeof<sbyte>
             || clrType = typeof<byte>
             || clrType = typeof<int16>
             || clrType = typeof<uint16>
             || clrType = typeof<int32>
             || clrType = typeof<uint32>
             || clrType = typeof<int64>
             || clrType = typeof<uint64> then
            Some "Int"
        elif clrType = ownerType then
            Some(ownerType.Name.Split('`')[0])
        else
            None

    let private isSupportedManagedType (ownerType: Type) (clrType: Type) =
        not clrType.IsByRef
        && not clrType.IsPointer
        && not clrType.IsGenericParameter
        && not clrType.ContainsGenericParameters
        && Option.isSome (tryMapManagedTypeText ownerType clrType)

    let private supportedParameterName (index: int) (parameterInfo: ParameterInfo) =
        if String.IsNullOrWhiteSpace(parameterInfo.Name) then
            $"arg{index + 1}"
        else
            parameterInfo.Name

    let private nullaryUnitParameter =
        { Name = "unit"
          TypeText = "Unit"
          IsReceiver = false }

    let private tryBuildConstructorBinding ownerType (constructorInfo: ConstructorInfo) =
        if constructorInfo.IsPublic
           && not constructorInfo.ContainsGenericParameters
           && constructorInfo.GetParameters() |> Array.forall (fun parameter -> isSupportedManagedType ownerType parameter.ParameterType) then
            let parameters =
                constructorInfo.GetParameters()
                |> Array.toList
                |> List.mapi (fun index parameterInfo ->
                    { Name = supportedParameterName index parameterInfo
                      TypeText = tryMapManagedTypeText ownerType parameterInfo.ParameterType |> Option.get
                      IsReceiver = false })
                |> fun parameters ->
                    if List.isEmpty parameters then
                        [ nullaryUnitParameter ]
                    else
                        parameters

            Some(
                "new",
                { Name = ""
                  SourceName = "new"
                  Parameters = parameters
                  ReturnTypeText = ownerType.Name.Split('`')[0] },
                HostConstructor constructorInfo
            )
        else
            None

    let private tryBuildMethodBinding ownerType (methodInfo: MethodInfo) =
        let parameters = methodInfo.GetParameters()

        let supported =
            methodInfo.IsPublic
            && not methodInfo.IsSpecialName
            && not methodInfo.ContainsGenericParameters
            && isSupportedManagedType ownerType methodInfo.ReturnType
            && parameters |> Array.forall (fun parameter -> isSupportedManagedType ownerType parameter.ParameterType)

        if supported then
            let parameterLayouts =
                (if methodInfo.IsStatic then
                     []
                 else
                     [ { Name = "this"
                         TypeText = ownerType.Name.Split('`')[0]
                         IsReceiver = true } ])
                @ (parameters
                   |> Array.toList
                   |> List.mapi (fun index parameterInfo ->
                       { Name = supportedParameterName index parameterInfo
                         TypeText = tryMapManagedTypeText ownerType parameterInfo.ParameterType |> Option.get
                         IsReceiver = false }))
                |> fun parameters ->
                    if parameters |> List.exists (fun parameter -> not parameter.IsReceiver) then
                        parameters
                    else
                        parameters @ [ nullaryUnitParameter ]

            Some(
                methodInfo.Name,
                { Name = ""
                  SourceName = methodInfo.Name
                  Parameters = parameterLayouts
                  ReturnTypeText = tryMapManagedTypeText ownerType methodInfo.ReturnType |> Option.get },
                HostMethod methodInfo
            )
        else
            None

    let private tryBuildPropertyGetterBinding ownerType (propertyInfo: PropertyInfo) =
        let getter = propertyInfo.GetMethod

        let supported =
            getter <> null
            && getter.IsPublic
            && propertyInfo.GetIndexParameters().Length = 0
            && isSupportedManagedType ownerType propertyInfo.PropertyType

        if supported then
            let parameters =
                if getter.IsStatic then
                    []
                else
                    [ { Name = "this"
                        TypeText = ownerType.Name.Split('`')[0]
                        IsReceiver = true } ]

            Some(
                propertyInfo.Name,
                { Name = ""
                  SourceName = propertyInfo.Name
                  Parameters = parameters
                  ReturnTypeText = tryMapManagedTypeText ownerType propertyInfo.PropertyType |> Option.get },
                HostPropertyGetter getter
            )
        else
            None

    let private signatureKey (callable: DotNetHostCallable) =
        let memberInfoText (memberInfo: MethodBase) =
            let parameterTypes =
                memberInfo.GetParameters()
                |> Array.map (fun parameterInfo -> parameterInfo.ParameterType.AssemblyQualifiedName |> Option.ofObj |> Option.defaultValue parameterInfo.ParameterType.FullName)
                |> String.concat ";"

            match memberInfo with
            | :? MethodInfo as methodInfo ->
                let returnTypeText =
                    methodInfo.ReturnType.AssemblyQualifiedName
                    |> Option.ofObj
                    |> Option.defaultValue methodInfo.ReturnType.FullName

                $"{parameterTypes}->{returnTypeText}"
            | _ ->
                parameterTypes

        match callable with
        | HostConstructor constructorInfo ->
            memberInfoText constructorInfo
        | HostMethod methodInfo ->
            memberInfoText methodInfo
        | HostPropertyGetter getter ->
            memberInfoText getter

    let private applyDeterministicNames rawEntries =
        rawEntries
        |> List.groupBy (fun (sourceName, _, _) -> sourceName)
        |> List.collect (fun (sourceName, entries) ->
            let ordered =
                entries
                |> List.sortBy (fun (_, binding, callable) ->
                    List.length binding.Parameters,
                    signatureKey callable)

            if List.length ordered = 1 then
                ordered
                |> List.map (fun (_, binding, callable) ->
                    { binding with Name = sourceName }, callable)
            else
                ordered
                |> List.groupBy (fun (_, binding, _) -> List.length binding.Parameters)
                |> List.collect (fun (arity, arityEntries) ->
                    if List.length arityEntries = 1 then
                        arityEntries
                        |> List.map (fun (_, binding, callable) ->
                            { binding with Name = $"{sourceName}_arity{arity}" }, callable)
                    else
                        arityEntries
                        |> List.sortBy (fun (_, _, callable) -> signatureKey callable)
                        |> List.mapi (fun index (_, binding, callable) ->
                            { binding with Name = $"{sourceName}_arity{arity}_{index + 1}" }, callable)))

    let private dotNetModuleCache = Dictionary<string, HostModuleDescription option>(StringComparer.Ordinal)
    let private dotNetCallableCache = Dictionary<string, Map<string, DotNetHostCallable>>(StringComparer.Ordinal)
    let private cacheGate = obj ()

    let private buildDotNetTypeModule segments =
        let moduleName = SyntaxFacts.moduleNameToText segments
        let fullTypeName = dotNetTypeName segments

        tryFindClrType fullTypeName
        |> Option.filter (fun clrType -> not clrType.IsGenericTypeDefinition && not clrType.ContainsGenericParameters)
        |> Option.map (fun clrType ->
            let ownTypeName = clrType.Name.Split('`')[0]

            let rawEntries =
                ((clrType.GetConstructors(BindingFlags.Public ||| BindingFlags.Instance)
                  |> Array.choose (tryBuildConstructorBinding clrType)
                  |> Array.toList)
                 @ (clrType.GetMethods(BindingFlags.Public ||| BindingFlags.Instance ||| BindingFlags.Static)
                    |> Array.choose (tryBuildMethodBinding clrType)
                    |> Array.toList)
                 @ (clrType.GetProperties(BindingFlags.Public ||| BindingFlags.Instance ||| BindingFlags.Static)
                    |> Array.choose (tryBuildPropertyGetterBinding clrType)
                    |> Array.toList))

            let namedEntries = applyDeterministicNames rawEntries
            let callableMap = namedEntries |> List.map (fun (binding, callable) -> binding.Name, callable) |> Map.ofList

            lock cacheGate (fun () -> dotNetCallableCache[moduleName] <- callableMap)

            { ModuleName = moduleName
              ScopeKind = ManagedTypeScope
              Types =
                [ { Name = ownTypeName
                    ExternalRuntimeTypeName = clrType.AssemblyQualifiedName } ]
              Terms = namedEntries |> List.map fst })

    let private getOrBuildDotNetTypeModule segments =
        let moduleName = SyntaxFacts.moduleNameToText segments

        lock cacheGate (fun () ->
            match dotNetModuleCache.TryGetValue(moduleName) with
            | true, cached -> cached
            | _ ->
                let built = buildDotNetTypeModule segments
                dotNetModuleCache[moduleName] <- built
                built)

    let tryResolveImport backendProfile moduleSpecifier =
        match moduleSpecifier with
        | Url _ ->
            NotHostModule
        | Dotted segments ->
            match tryReservedRoot segments with
            | None ->
                NotHostModule
            | Some(hostRoot, rootText) when not (backendProvidesRoot backendProfile hostRoot) ->
                UnsupportedBackend rootText
            | Some(HostDotNet, _) ->
                getOrBuildDotNetTypeModule segments
                |> Option.map Resolved
                |> Option.defaultValue ModuleNotFound
            | Some(_, rootText) ->
                UnsupportedBackend rootText

    let collectImportedModules backendProfile (documents: ParsedDocument list) =
        documents
        |> List.collect (fun document ->
            document.Syntax.Declarations
            |> List.collect (function
                | ImportDeclaration (_, specs) -> specs
                | _ -> []))
        |> List.choose (fun spec ->
            match tryResolveImport backendProfile spec.Source with
            | Resolved description -> Some(description.ModuleName, description)
            | _ -> None)
        |> Map.ofList

    let tryResolveDotNetCallable moduleName bindingName =
        lock cacheGate (fun () ->
            match dotNetCallableCache.TryGetValue(moduleName) with
            | true, bindingMap ->
                bindingMap |> Map.tryFind bindingName
            | _ ->
                None)

    let toRuntimeModule (description: HostModuleDescription) =
        let dataTypes : KRuntimeDataType list =
            description.Types
            |> List.map (fun exportedType ->
                { Name = exportedType.Name
                  TypeParameters = []
                  Constructors = []
                  ExternalRuntimeTypeName = Some exportedType.ExternalRuntimeTypeName })

        let bindings : KRuntimeBinding list =
            description.Terms
            |> List.map (fun binding ->
                { Name = binding.Name
                  Parameters =
                    binding.Parameters
                    |> List.map (fun parameter ->
                        { Name = parameter.Name
                          TypeText = Some parameter.TypeText })
                  ReturnTypeText = Some binding.ReturnTypeText
                  Body = None
                  Intrinsic = false
                  Provenance =
                    { FilePath = $"<host:{description.ModuleName}>"
                      ModuleName = description.ModuleName
                      DeclarationName = Some binding.Name
                      IntroductionKind = "host-binding" } })

        { Name = description.ModuleName
          SourceFile = $"<host:{description.ModuleName}>"
          Imports = []
          Exports = description.Terms |> List.map (fun binding -> binding.Name)
          IntrinsicTerms = []
          DataTypes = dataTypes
          Traits = []
          TraitInstances = []
          Constructors = []
          Bindings = bindings }
