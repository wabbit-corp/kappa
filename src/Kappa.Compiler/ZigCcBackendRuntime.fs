namespace Kappa.Compiler

// Supplies the generated C runtime/prelude scaffolding used by the zig backend.
module internal ZigCcBackendRuntime =
    open ZigCcBackendSupport

    let internal emitTraitDispatchFunctions (context: GenerationContext) =
        let dispatchEntries =
            context.Workspace.Documents
            |> List.collect (fun document ->
                match document.ModuleName with
                | None ->
                    []
                | Some moduleSegments ->
                    let moduleName = SyntaxFacts.moduleNameToText moduleSegments

                    document.Syntax.Declarations
                    |> List.choose (function
                        | InstanceDeclarationNode declaration ->
                            Some(moduleName, declaration)
                        | _ ->
                            None)
                    |> List.collect (fun (instanceModuleName, declaration) ->
                        let instanceKey = TraitRuntime.instanceKeyFromTokens declaration.HeaderTokens

                        declaration.Members
                        |> List.choose (fun memberDeclaration ->
                            memberDeclaration.Name
                            |> Option.bind (fun memberName ->
                                let hiddenBindingName =
                                    TraitRuntime.instanceMemberBindingName declaration.TraitName instanceKey memberName

                                lookupFunctionName context instanceModuleName hiddenBindingName
                                |> Option.map (fun emittedFunctionName ->
                                    declaration.TraitName, memberName, instanceModuleName, instanceKey, emittedFunctionName)))))

        dispatchEntries
        |> List.groupBy (fun (traitName, memberName, _, _, _) -> traitName, memberName)
        |> List.collect (fun ((traitName, memberName), entries) ->
            let functionName = traitDispatchFunctionName traitName memberName
            let missingDictionaryMessage =
                cStringLiteral $"missing dictionary value for trait dispatch {traitName}.{memberName}"

            let missingInstanceMessage =
                cStringLiteral $"no instance available for trait dispatch {traitName}.{memberName}"

            [
                $"static KValue* {functionName}(KValue* dictionary, KValue** args, int argc)"
                "{"
                "    if (dictionary == NULL || dictionary->tag != K_TAG_DICTIONARY)"
                "    {"
                $"        kappa_panic(\"{missingDictionaryMessage}\");"
                "    }"
                "    KValue* dispatch_args[argc + 1];"
                "    dispatch_args[0] = dictionary;"
                "    for (int i = 0; i < argc; ++i)"
                "    {"
                "        dispatch_args[i + 1] = args[i];"
                "    }"
                ""
                yield!
                    entries
                    |> List.collect (fun (_, _, instanceModuleName, instanceKey, emittedFunctionName) ->
                        [
                            $"    if (strcmp(dictionary->as.dictionary_value.module_name, \"{cStringLiteral instanceModuleName}\") == 0"
                            $"        && strcmp(dictionary->as.dictionary_value.instance_key, \"{cStringLiteral instanceKey}\") == 0)"
                            "    {"
                            $"        return {emittedFunctionName}(NULL, dispatch_args, argc + 1);"
                            "    }"
                            ""
                        ])
                $"    kappa_panic(\"{missingInstanceMessage}\");"
                "    return kappa_unit();"
                "}"
                ""
            ])

    let internal emitRuntimePrelude (context: GenerationContext) =
        let preludeBoolTypeId = typeIdName Stdlib.PreludeModuleText "Bool"

        let typeIdLines =
            if Map.isEmpty context.DataTypeIds then
                [ "enum { KTYPE_unused = 0 };" ]
            else
                [ "enum"
                  "{"
                  yield!
                    context.DataTypeIds
                    |> Map.toList
                    |> List.mapi (fun index ((_, _), typeId) -> $"    {typeId} = {index + 1},")
                  "};" ]

        BundledBackendSources.renderZigRuntimePrelude typeIdLines preludeBoolTypeId

    let internal emitEntryPointProgram entryFunctionName =
        BundledBackendSources.renderZigEntryPointProgram entryFunctionName
