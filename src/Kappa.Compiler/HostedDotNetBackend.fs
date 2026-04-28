namespace Kappa.Compiler

open System
open System.Globalization
open System.IO
open System.Reflection
open System.Security

// Emits a managed dotnet artifact by hosting the KRuntimeIR interpreter inside a generated F# project.
module internal HostedDotNetBackend =
    let private compilerAssemblyPath =
        typeof<WorkspaceCompilation>.Assembly.Location

    let requiresHostedRuntime backendProfile (runtimeModules: KRuntimeModule list) =
        String.Equals(Stdlib.normalizeBackendProfile backendProfile, "dotnet", StringComparison.Ordinal)
        && KRuntimeEffectAnalysis.modulesContainEffectRuntime runtimeModules

    let private xmlEscape (text: string) =
        SecurityElement.Escape(text)

    let private fsString (text: string) =
        sprintf "%A" text

    let private fsInt64 (value: int64) =
        string value + "L"

    let private fsBool value =
        if value then "true" else "false"

    let private fsOption render value =
        match value with
        | Some current -> $"Some({render current})"
        | None -> "None"

    let private fsList render items =
        "[ " + (items |> List.map render |> String.concat "; ") + " ]"

    let private fsFloat (value: double) =
        let bits = BitConverter.DoubleToInt64Bits(value)
        $"System.BitConverter.Int64BitsToDouble({fsInt64 bits})"

    let private visibilityToCode =
        function
        | Public -> "Public"
        | Private -> "Private"

    let private quantityToCode =
        function
        | QuantityZero -> "QuantityZero"
        | QuantityOne -> "QuantityOne"
        | QuantityBorrow regionName -> $"QuantityBorrow({fsOption fsString regionName})"
        | QuantityOmega -> "QuantityOmega"
        | QuantityAtMostOne -> "QuantityAtMostOne"
        | QuantityAtLeastOne -> "QuantityAtLeastOne"
        | QuantityVariable name -> $"QuantityVariable({fsString name})"

    let private literalToCode =
        function
        | LiteralValue.Integer value -> $"LiteralValue.Integer {fsInt64 value}"
        | LiteralValue.Float value -> $"LiteralValue.Float ({fsFloat value})"
        | LiteralValue.String value -> $"LiteralValue.String {fsString value}"
        | LiteralValue.Character value -> $"LiteralValue.Character {fsString value}"
        | LiteralValue.Grapheme value -> $"LiteralValue.Grapheme {fsString value}"
        | LiteralValue.Byte value -> $"LiteralValue.Byte {int value}uy"
        | LiteralValue.Unit -> "LiteralValue.Unit"

    let private urlPinToCode =
        function
        | Sha256Pin digest -> $"Sha256Pin({fsString digest})"
        | RefPin reference -> $"RefPin({fsString reference})"

    let private moduleSpecifierToCode =
        function
        | Dotted segments -> $"Dotted {fsList fsString segments}"
        | Url spec ->
            $"Url {{ OriginalText = {fsString spec.OriginalText}; BaseUrl = {fsString spec.BaseUrl}; Pin = {fsOption urlPinToCode spec.Pin} }}"

    let private importNamespaceToCode =
        function
        | Term -> "Term"
        | Type -> "Type"
        | Trait -> "Trait"
        | Constructor -> "Constructor"

    let private importItemModifierToCode =
        function
        | Unhide -> "Unhide"
        | Clarify -> "Clarify"

    let private importItemToCode (item: ImportItem) =
        $"{{ Modifiers = {fsList importItemModifierToCode item.Modifiers}; Namespace = {fsOption importNamespaceToCode item.Namespace}; Name = {fsString item.Name}; IncludeConstructors = {fsBool item.IncludeConstructors}; Alias = {fsOption fsString item.Alias} }}"

    let private exceptItemToCode (item: ExceptItem) =
        $"{{ Namespace = {fsOption importNamespaceToCode item.Namespace}; Name = {fsString item.Name} }}"

    let private importSelectionToCode =
        function
        | QualifiedOnly -> "QualifiedOnly"
        | Items items -> $"Items {fsList importItemToCode items}"
        | All -> "All"
        | AllExcept items -> $"AllExcept {fsList exceptItemToCode items}"

    let private importSpecToCode (spec: ImportSpec) =
        $"{{ Source = {moduleSpecifierToCode spec.Source}; Alias = {fsOption fsString spec.Alias}; Selection = {importSelectionToCode spec.Selection} }}"

    let private originToCode (origin: KCoreOrigin) =
        $"{{ FilePath = {fsString origin.FilePath}; ModuleName = {fsString origin.ModuleName}; DeclarationName = {fsOption fsString origin.DeclarationName}; IntroductionKind = {fsString origin.IntroductionKind} }}"

    let rec private runtimeExpressionToCode expression =
        match expression with
        | KRuntimeLiteral literal ->
            $"KRuntimeLiteral({literalToCode literal})"
        | KRuntimeName segments ->
            $"KRuntimeName({fsList fsString segments})"
        | KRuntimeEffectLabel(labelName, operations) ->
            $"KRuntimeEffectLabel({fsString labelName}, {fsList runtimeEffectOperationToCode operations})"
        | KRuntimeEffectOperation(label, operationName) ->
            $"KRuntimeEffectOperation({runtimeExpressionToCode label}, {fsString operationName})"
        | KRuntimeClosure(parameters, body) ->
            $"KRuntimeClosure({fsList fsString parameters}, {runtimeExpressionToCode body})"
        | KRuntimeIfThenElse(condition, whenTrue, whenFalse) ->
            $"KRuntimeIfThenElse({runtimeExpressionToCode condition}, {runtimeExpressionToCode whenTrue}, {runtimeExpressionToCode whenFalse})"
        | KRuntimeHandle(isDeep, label, body, returnClause, operationClauses) ->
            $"KRuntimeHandle({fsBool isDeep}, {runtimeExpressionToCode label}, {runtimeExpressionToCode body}, {runtimeHandlerClauseToCode returnClause}, {fsList runtimeHandlerClauseToCode operationClauses})"
        | KRuntimeMatch(scrutinee, cases) ->
            $"KRuntimeMatch({runtimeExpressionToCode scrutinee}, {fsList runtimeMatchCaseToCode cases})"
        | KRuntimeExecute inner ->
            $"KRuntimeExecute({runtimeExpressionToCode inner})"
        | KRuntimeLet(bindingName, value, body) ->
            $"KRuntimeLet({fsString bindingName}, {runtimeExpressionToCode value}, {runtimeExpressionToCode body})"
        | KRuntimeDoScope(scopeLabel, body) ->
            $"KRuntimeDoScope({fsString scopeLabel}, {runtimeExpressionToCode body})"
        | KRuntimeScheduleExit(scopeLabel, action, body) ->
            $"KRuntimeScheduleExit({fsString scopeLabel}, {runtimeExitActionToCode action}, {runtimeExpressionToCode body})"
        | KRuntimeSequence(first, second) ->
            $"KRuntimeSequence({runtimeExpressionToCode first}, {runtimeExpressionToCode second})"
        | KRuntimeWhile(condition, body) ->
            $"KRuntimeWhile({runtimeExpressionToCode condition}, {runtimeExpressionToCode body})"
        | KRuntimeApply(callee, arguments) ->
            $"KRuntimeApply({runtimeExpressionToCode callee}, {fsList runtimeExpressionToCode arguments})"
        | KRuntimeDictionaryValue(moduleName, traitName, instanceKey) ->
            $"KRuntimeDictionaryValue({fsString moduleName}, {fsString traitName}, {fsString instanceKey})"
        | KRuntimeTraitCall(traitName, memberName, dictionary, arguments) ->
            $"KRuntimeTraitCall({fsString traitName}, {fsString memberName}, {runtimeExpressionToCode dictionary}, {fsList runtimeExpressionToCode arguments})"
        | KRuntimeUnary(operatorName, operand) ->
            $"KRuntimeUnary({fsString operatorName}, {runtimeExpressionToCode operand})"
        | KRuntimeBinary(left, operatorName, right) ->
            $"KRuntimeBinary({runtimeExpressionToCode left}, {fsString operatorName}, {runtimeExpressionToCode right})"
        | KRuntimePrefixedString(prefix, parts) ->
            $"KRuntimePrefixedString({fsString prefix}, {fsList runtimeStringPartToCode parts})"

    and private runtimeEffectOperationToCode (operation: KRuntimeEffectOperation) =
        $"{{ Name = {fsString operation.Name}; ResumptionQuantity = {fsOption quantityToCode operation.ResumptionQuantity}; ParameterArity = {operation.ParameterArity} }}"

    and private runtimeHandlerArgumentToCode =
        function
        | KRuntimeEffectUnitArgument -> "KRuntimeEffectUnitArgument"
        | KRuntimeEffectWildcardArgument -> "KRuntimeEffectWildcardArgument"
        | KRuntimeEffectNameArgument name -> $"KRuntimeEffectNameArgument({fsString name})"

    and private runtimeHandlerClauseToCode (clause: KRuntimeEffectHandlerClause) =
        $"{{ OperationName = {fsString clause.OperationName}; Arguments = {fsList runtimeHandlerArgumentToCode clause.Arguments}; ResumptionName = {fsOption fsString clause.ResumptionName}; Body = {runtimeExpressionToCode clause.Body} }}"

    and private runtimeExitActionToCode =
        function
        | KRuntimeDeferred expression ->
            $"KRuntimeDeferred({runtimeExpressionToCode expression})"
        | KRuntimeRelease(resourceTypeText, release, resource) ->
            $"KRuntimeRelease({fsOption fsString resourceTypeText}, {runtimeExpressionToCode release}, {runtimeExpressionToCode resource})"

    and private runtimeStringPartToCode =
        function
        | KRuntimeStringText text -> $"KRuntimeStringText({fsString text})"
        | KRuntimeStringInterpolation(expression, format) ->
            $"KRuntimeStringInterpolation({runtimeExpressionToCode expression}, {fsOption fsString format})"

    and private runtimePatternToCode =
        function
        | KRuntimeWildcardPattern -> "KRuntimeWildcardPattern"
        | KRuntimeNamePattern name -> $"KRuntimeNamePattern({fsString name})"
        | KRuntimeLiteralPattern literal -> $"KRuntimeLiteralPattern({literalToCode literal})"
        | KRuntimeConstructorPattern(name, patterns) ->
            $"KRuntimeConstructorPattern({fsList fsString name}, {fsList runtimePatternToCode patterns})"
        | KRuntimeOrPattern patterns ->
            $"KRuntimeOrPattern({fsList runtimePatternToCode patterns})"

    and private runtimeMatchCaseToCode (caseClause: KRuntimeMatchCase) =
        $"{{ Pattern = {runtimePatternToCode caseClause.Pattern}; Guard = {fsOption runtimeExpressionToCode caseClause.Guard}; Body = {runtimeExpressionToCode caseClause.Body} }}"

    let private runtimeParameterToCode (parameter: KRuntimeParameter) =
        $"{{ Name = {fsString parameter.Name}; TypeText = {fsOption fsString parameter.TypeText} }}"

    let private runtimeConstructorToCode (constructor: KRuntimeConstructor) =
        $"{{ Name = {fsString constructor.Name}; Arity = {constructor.Arity}; TypeName = {fsString constructor.TypeName}; FieldNames = {fsList (fsOption fsString) constructor.FieldNames}; FieldTypeTexts = {fsList fsString constructor.FieldTypeTexts}; Provenance = {originToCode constructor.Provenance} }}"

    let private runtimeDataTypeToCode (dataType: KRuntimeDataType) =
        $"{{ Name = {fsString dataType.Name}; TypeParameters = {fsList fsString dataType.TypeParameters}; Constructors = {fsList runtimeConstructorToCode dataType.Constructors}; ExternalRuntimeTypeName = {fsOption fsString dataType.ExternalRuntimeTypeName} }}"

    let private runtimeTraitToCode (traitInfo: KRuntimeTrait) =
        $"{{ Name = {fsString traitInfo.Name}; TypeParameterCount = {traitInfo.TypeParameterCount} }}"

    let private runtimeTraitInstanceToCode (instanceInfo: KRuntimeTraitInstance) =
        let memberBindings =
            instanceInfo.MemberBindings
            |> List.map (fun (memberName, bindingName) -> $"({fsString memberName}, {fsString bindingName})")
            |> String.concat "; "
            |> fun body -> "[ " + body + " ]"

        $"{{ TraitName = {fsString instanceInfo.TraitName}; InstanceKey = {fsString instanceInfo.InstanceKey}; HeadTypeTexts = {fsList fsString instanceInfo.HeadTypeTexts}; MemberBindings = {memberBindings} }}"

    let private runtimeBindingToCode (binding: KRuntimeBinding) =
        $"{{ Name = {fsString binding.Name}; Parameters = {fsList runtimeParameterToCode binding.Parameters}; ReturnTypeText = {fsOption fsString binding.ReturnTypeText}; Body = {fsOption runtimeExpressionToCode binding.Body}; Intrinsic = {fsBool binding.Intrinsic}; Provenance = {originToCode binding.Provenance} }}"

    let private runtimeModuleToCode (runtimeModule: KRuntimeModule) =
        $"{{ Name = {fsString runtimeModule.Name}; SourceFile = {fsString runtimeModule.SourceFile}; Imports = {fsList importSpecToCode runtimeModule.Imports}; Exports = {fsList fsString runtimeModule.Exports}; IntrinsicTerms = {fsList fsString runtimeModule.IntrinsicTerms}; DataTypes = {fsList runtimeDataTypeToCode runtimeModule.DataTypes}; Traits = {fsList runtimeTraitToCode runtimeModule.Traits}; TraitInstances = {fsList runtimeTraitInstanceToCode runtimeModule.TraitInstances}; Constructors = {fsList runtimeConstructorToCode runtimeModule.Constructors}; Bindings = {fsList runtimeBindingToCode runtimeModule.Bindings} }}"

    let private sanitizeAssemblyName (outputDirectory: string) =
        "Kappa.Generated.Hosted."
        + IlDotNetBackendModel.sanitizeIdentifier(Path.GetFileName(Path.GetFullPath(outputDirectory)))

    let private buildProjectText assemblyName compilerReferencePath =
        $"""<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <OutputType>Exe</OutputType>
    <TargetFramework>net10.0</TargetFramework>
    <TreatWarningsAsErrors>true</TreatWarningsAsErrors>
    <AssemblyName>{xmlEscape assemblyName}</AssemblyName>
  </PropertyGroup>

  <ItemGroup>
    <Compile Include="Program.fs" />
  </ItemGroup>

  <ItemGroup>
    <Reference Include="Kappa.Compiler">
      <HintPath>{xmlEscape compilerReferencePath}</HintPath>
      <Private>true</Private>
    </Reference>
  </ItemGroup>
</Project>
"""

    let private buildProgramText entryPoint (runtimeModules: KRuntimeModule list) =
        let modulesText = runtimeModules |> fsList runtimeModuleToCode

        $"""namespace Kappa.Generated

open System
open Kappa.Compiler

module private Generated =
    let entryPoint = {fsString entryPoint}

    let modules : KRuntimeModule list =
        {modulesText}

module Program =
    [<EntryPoint>]
    let main _ =
        let output = RuntimeOutput.console

        match Interpreter.executeRuntimeBindingWithOutput Generated.modules output Generated.entryPoint with
        | Result.Ok value ->
            if Interpreter.shouldPrintResult value then
                Console.Out.WriteLine(RuntimeValue.format value)

            0
        | Result.Error issue ->
            Console.Error.WriteLine("runtime error: " + issue.Message)
            1
"""

    let private aggregateBuildOutput (result: HostSupport.ProcessResult) =
        [ result.StandardOutput; result.StandardError ]
        |> List.filter (String.IsNullOrWhiteSpace >> not)
        |> String.concat Environment.NewLine

    let private resolveHostedEntryPoint (runtimeModules: KRuntimeModule list) (entryPoint: string) =
        let zeroArgBindingsByModule =
            runtimeModules
            |> List.groupBy (fun runtimeModule -> runtimeModule.Name)
            |> List.map (fun (moduleName, fragments) ->
                let bindings =
                    fragments
                    |> List.collect (fun fragment ->
                        fragment.Bindings
                        |> List.filter (fun binding -> not binding.Intrinsic && List.isEmpty binding.Parameters))

                moduleName, bindings)
            |> Map.ofList

        let segments =
            entryPoint.Split('.', StringSplitOptions.RemoveEmptyEntries)
            |> Array.toList

        match segments with
        | [] ->
            Result.Error "Expected a binding name to run."
        | [ bindingName ] ->
            let matches =
                zeroArgBindingsByModule
                |> Map.toList
                |> List.choose (fun (moduleName, bindings) ->
                    bindings
                    |> List.tryFind (fun binding -> String.Equals(binding.Name, bindingName, StringComparison.Ordinal))
                    |> Option.map (fun _ -> moduleName, bindingName))

            match matches with
            | [] ->
                Result.Error $"No zero-argument binding named '{bindingName}' was found for dotnet."
            | [ moduleName, resolvedBindingName ] ->
                Result.Ok(moduleName, resolvedBindingName)
            | _ ->
                Result.Error $"Binding name '{bindingName}' is ambiguous. Use a fully qualified name."
        | _ ->
            let moduleName = segments |> List.take (segments.Length - 1) |> String.concat "."
            let bindingName = List.last segments

            match zeroArgBindingsByModule |> Map.tryFind moduleName with
            | Some bindings when bindings |> List.exists (fun binding -> String.Equals(binding.Name, bindingName, StringComparison.Ordinal)) ->
                Result.Ok(moduleName, bindingName)
            | Some _ ->
                Result.Error $"dotnet requires a zero-argument binding named '{bindingName}' in module '{moduleName}'."
            | None ->
                Result.Error $"dotnet requires a zero-argument binding named '{bindingName}' in module '{moduleName}'."

    let emitArtifact (workspace: WorkspaceCompilation) (entryPoint: string) (outputDirectory: string) (deployment: DotNetDeployment) =
        if deployment <> DotNetDeployment.Managed then
            Result.Error "The hosted dotnet backend currently supports managed builds only."
        else
            resolveHostedEntryPoint workspace.KRuntimeIR entryPoint
            |> Result.bind (fun _ ->
                try
                    let projectDirectory = Path.GetFullPath(outputDirectory)
                    Directory.CreateDirectory(projectDirectory) |> ignore

                    let assemblyName = sanitizeAssemblyName projectDirectory
                    let projectFilePath = Path.Combine(projectDirectory, "Kappa.Generated.Hosted.fsproj")
                    let programFilePath = Path.Combine(projectDirectory, "Program.fs")
                    let generatedFilePath = Path.Combine(projectDirectory, "bin", "Release", "net10.0", $"{assemblyName}.dll")

                    File.WriteAllText(projectFilePath, buildProjectText assemblyName compilerAssemblyPath)
                    File.WriteAllText(programFilePath, buildProgramText entryPoint workspace.KRuntimeIR)

                    let buildResult =
                        HostSupport.runProcess
                            projectDirectory
                            "dotnet"
                            $"build \"{projectFilePath}\" -c Release"

                    if buildResult.ExitCode <> 0 then
                        Result.Error
                            $"The hosted dotnet backend could not build '{entryPoint}':{Environment.NewLine}{aggregateBuildOutput buildResult}"
                    elif not (File.Exists(generatedFilePath)) then
                        Result.Error
                            $"The hosted dotnet backend built '{entryPoint}' but did not produce '{generatedFilePath}'."
                    else
                        Result.Ok
                            { ProjectDirectory = projectDirectory
                              ProjectFilePath = projectFilePath
                              ProgramFilePath = programFilePath
                              RuntimeFilePath = generatedFilePath
                              GeneratedFilePath = generatedFilePath
                              EntryPoint = entryPoint
                              Deployment = deployment }
                with ex ->
                    Result.Error $"The hosted dotnet backend could not prepare '{entryPoint}': {ex.Message}")
