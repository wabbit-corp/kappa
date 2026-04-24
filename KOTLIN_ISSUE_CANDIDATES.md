# Kotlin Issue Candidates For Kappa

Mined from:

- `../datatron/data-scala-issues/kotlin-issues.json`

Corpus note: after the 2026-04-24 layout refresh, this file is YouTrack-shaped. Use `idReadable` for issue keys such
as `KT-248`; `id` is an internal YouTrack id like `25-342046` and must not be used as the issue id in this ledger.

Keyword scans completed on 2026-04-23:

- full-corpus scan for `?.`, `?:`, `safe call`, `elvis`, `smart cast`, `smartcast`, `flow typing`, `nullability`, `nullable`, `null check`

Direct reads completed through 2026-04-24:

- `KT-1`, `KT-2`, `KT-44`, `KT-46`, `KT-51`, `KT-52`, `KT-53`, `KT-55`, `KT-56`, `KT-57`, `KT-58`, `KT-59`, `KT-60`, `KT-61`, `KT-62`, `KT-63`, `KT-64`, `KT-65`, `KT-66`, `KT-67`, `KT-68`, `KT-69`, `KT-70`, `KT-71`, `KT-72`, `KT-73`, `KT-74`, `KT-75`, `KT-76`, `KT-77`, `KT-78`, `KT-79`, `KT-80`, `KT-81`, `KT-82`, `KT-83`, `KT-84`, `KT-85`, `KT-86`, `KT-87`, `KT-88`, `KT-89`, `KT-90`, `KT-91`, `KT-92`, `KT-93`, `KT-95`, `KT-97`, `KT-98`, `KT-99`, `KT-100`, `KT-101`, `KT-102`, `KT-103`, `KT-104`, `KT-106`, `KT-107`, `KT-108`, `KT-109`, `KT-110`, `KT-111`, `KT-113`, `KT-114`, `KT-115`, `KT-116`, `KT-117`, `KT-118`, `KT-119`, `KT-120`, `KT-121`, `KT-122`, `KT-123`, `KT-124`, `KT-125`, `KT-126`, `KT-127`, `KT-128`, `KT-129`, `KT-130`, `KT-131`, `KT-132`, `KT-133`, `KT-134`, `KT-135`, `KT-136`, `KT-137`, `KT-138`, `KT-139`, `KT-140`, `KT-141`, `KT-142`, `KT-143`, `KT-144`, `KT-145`, `KT-146`, `KT-147`, `KT-148`, `KT-149`, `KT-150`, `KT-151`, `KT-152`, `KT-153`, `KT-154`, `KT-155`, `KT-156`, `KT-157`, `KT-158`, `KT-159`, `KT-160`, `KT-161`, `KT-162`, `KT-163`, `KT-164`, `KT-165`, `KT-166`, `KT-167`, `KT-168`, `KT-169`, `KT-170`, `KT-171`, `KT-172`, `KT-173`, `KT-174`, `KT-175`, `KT-176`, `KT-177`, `KT-178`, `KT-179`, `KT-180`, `KT-181`, `KT-182`, `KT-183`, `KT-184`, `KT-185`, `KT-186`, `KT-187`, `KT-188`, `KT-189`, `KT-190`, `KT-191`, `KT-192`, `KT-194`, `KT-195`, `KT-196`, `KT-199`, `KT-200`, `KT-201`, `KT-202`, `KT-203`, `KT-204`, `KT-205`, `KT-206`, `KT-207`, `KT-208`, `KT-209`, `KT-210`, `KT-211`, `KT-212`, `KT-213`, `KT-214`, `KT-216`, `KT-218`, `KT-219`, `KT-220`, `KT-221`, `KT-222`, `KT-223`, `KT-224`, `KT-225`, `KT-226`, `KT-227`, `KT-228`, `KT-229`, `KT-230`, `KT-231`, `KT-232`, `KT-233`, `KT-234`, `KT-235`, `KT-237`, `KT-238`, `KT-239`, `KT-240`, `KT-241`, `KT-105`, `KT-247`, `KT-969`, `KT-1154`, `KT-1275`, `KT-1982`, `KT-2127`, `KT-2143`, `KT-2146`, `KT-2164`, `KT-2176`, `KT-2193`, `KT-2212`, `KT-3175`, `KT-3818`, `KT-3899`, `KT-4294`, `KT-4751`, `KT-5228`, `KT-5335`, `KT-6242`, `KT-6313`, `KT-6470`, `KT-7290`, `KT-8135`, `KT-8492`
- `KT-242`, `KT-243`, `KT-244`, `KT-245`, `KT-246`, `KT-248`, `KT-249`, `KT-250`, `KT-251`, `KT-252`, `KT-253`, `KT-254`, `KT-255`, `KT-256`, `KT-257`
- `KT-258`, `KT-259`, `KT-260`, `KT-261`, `KT-262`
- `KT-263`, `KT-264`, `KT-265`, `KT-266`, `KT-267`
- `KT-268`, `KT-270`, `KT-272`
- `KT-273`, `KT-274`, `KT-275`, `KT-276`, `KT-277`
- `KT-278`, `KT-279`, `KT-280`, `KT-281`, `KT-282`
- `KT-283`, `KT-284`, `KT-285`, `KT-286`, `KT-287`
- `KT-288`, `KT-289`, `KT-290`, `KT-291`, `KT-292`
- `KT-293`, `KT-294`, `KT-295`, `KT-296`, `KT-297`
- `KT-298`, `KT-299`, `KT-300`, `KT-301`, `KT-302`
- `KT-303`, `KT-304`, `KT-305`, `KT-306`, `KT-307`
- `KT-308`, `KT-309`, `KT-310`, `KT-311`, `KT-312`
- `KT-313`, `KT-314`, `KT-316`, `KT-317`
- `KT-318`, `KT-319`, `KT-320`, `KT-321`, `KT-322`
- `KT-323`, `KT-324`, `KT-325`, `KT-326`, `KT-327`
- `KT-328`, `KT-329`, `KT-330`, `KT-331`, `KT-332`
- `KT-333`, `KT-334`, `KT-335`, `KT-336`, `KT-337`
- `KT-338`, `KT-339`, `KT-340`, `KT-341`, `KT-342`
- `KT-343`, `KT-344`, `KT-345`, `KT-346`, `KT-347`
- `KT-348`, `KT-349`, `KT-350`, `KT-351`, `KT-352`
- `KT-353`, `KT-354`, `KT-355`, `KT-357`
- `KT-358`, `KT-359`, `KT-360`, `KT-361`, `KT-362`
- `KT-363`, `KT-364`, `KT-366`, `KT-367`
- `KT-368`, `KT-369`, `KT-370`, `KT-371`, `KT-372`
- `KT-374`, `KT-375`, `KT-376`, `KT-377`
- `KT-378`, `KT-379`, `KT-380`, `KT-381`, `KT-382`
- `KT-383`, `KT-384`, `KT-385`, `KT-386`, `KT-387`
- `KT-388`, `KT-389`, `KT-390`, `KT-391`, `KT-392`
- `KT-393`, `KT-394`, `KT-395`, `KT-396`, `KT-397`
- `KT-398`, `KT-399`, `KT-400`, `KT-401`, `KT-402`
- `KT-403`, `KT-404`, `KT-405`, `KT-406`, `KT-407`
- `KT-408`, `KT-409`, `KT-410`, `KT-411`, `KT-412`
- `KT-413`, `KT-415`, `KT-416`, `KT-417`; `KT-414` was absent from the current local corpus snapshot
- `KT-418`, `KT-419`, `KT-421`, `KT-422`; `KT-420` was absent from the current local corpus snapshot
- `KT-423`, `KT-424`, `KT-425`, `KT-426`, `KT-427`
- `KT-428`, `KT-429`, `KT-431`, `KT-432`; `KT-430` was absent from the current local corpus snapshot
- `KT-433`, `KT-434`, `KT-435`, `KT-436`, `KT-437`
- `KT-438`, `KT-439`, `KT-440`, `KT-441`, `KT-442`
- `KT-443`, `KT-444`, `KT-445`, `KT-446`, `KT-447`
- `KT-448`, `KT-449`, `KT-450`, `KT-451`; `KT-452` was absent from the current local corpus snapshot
- `KT-453`, `KT-454`, `KT-455`, `KT-456`, `KT-457`
- `KT-458`, `KT-459`, `KT-460`, `KT-461`, `KT-462`
- `KT-463`, `KT-464`, `KT-465`, `KT-466`, `KT-467`
- `KT-468`, `KT-469`, `KT-470`, `KT-471`, `KT-472`
- `KT-473`, `KT-474`, `KT-475`, `KT-476`; `KT-477` was absent from the current local corpus snapshot
- `KT-478`, `KT-479`, `KT-480`, `KT-481`, `KT-482`
- `KT-483`, `KT-484`, `KT-485`, `KT-486`, `KT-487`
- `KT-489`, `KT-490`, `KT-491`, `KT-492`; `KT-488` was absent from the current local corpus snapshot
- `KT-493`, `KT-494`, `KT-495`, `KT-496`, `KT-497`
- `KT-498`, `KT-500`, `KT-501`, `KT-502`; `KT-499` was absent from the current local corpus snapshot
- `KT-503`, `KT-504`, `KT-505`, `KT-507`; `KT-506` was absent from the current local corpus snapshot
- `KT-508`, `KT-509`, `KT-510`, `KT-511`, `KT-512`
- `KT-513`, `KT-514`, `KT-515`, `KT-516`, `KT-517`
- `KT-518`, `KT-520`, `KT-521`; `KT-519` and `KT-522` were absent from the current local corpus snapshot
- `KT-523`, `KT-524`, `KT-525`, `KT-526`, `KT-527`
- `KT-528`, `KT-529`, `KT-530`, `KT-531`; `KT-532` was absent from the current local corpus snapshot
- `KT-533`, `KT-534`, `KT-535`, `KT-536`, `KT-537`
- `KT-538`, `KT-542`; `KT-539`, `KT-540`, and `KT-541` were absent from the current local corpus snapshot
- `KT-543`, `KT-544`, `KT-545`, `KT-546`, and `KT-547` were absent from the current local corpus snapshot
- `KT-549`, `KT-550`, `KT-551`, `KT-552`; `KT-548` was absent from the current local corpus snapshot
- `KT-553`, `KT-554`, `KT-555`, `KT-556`, `KT-557`
- `KT-559`, `KT-560`, `KT-562`; `KT-558` and `KT-561` were absent from the current local corpus snapshot
- `KT-566`; `KT-563`, `KT-564`, `KT-565`, and `KT-567` were absent from the current local corpus snapshot
- `KT-568`, `KT-569`, `KT-570`, `KT-571`, `KT-572`
- `KT-573`, `KT-574`, `KT-575`, `KT-576`, `KT-577`
- `KT-578`, `KT-579`, `KT-580`, `KT-581`; `KT-582` was absent from the current local corpus snapshot

Purpose: collect Kotlin nullability, safe-navigation, Elvis, and smart-cast issues that map cleanly onto Kappa's
Chapter 7 rules without importing JVM interop or Kotlin's OO-specific surface.

## Strongest Kappa Fits

| source | bucket | spec_refs | fit | current_coverage | fixture_kind | expected_outcome | proposed_fixture_root | status | notes |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| `KT-105` | `optional_chaining` | `§7.1.1.2` | `direct` | `covered` | `single-file` | `positive-check` | `expressions.safe_navigation.positive_flatten` | `imported` | The existing fixture already covers left-associative chaining plus flattening through nested `?.`. |
| `KT-247` | `optional_chaining` | `§7.1.1.2` | `direct` | `covered` | `single-file` | `positive-check` | `expressions.safe_navigation.positive_wrap` | `imported` | The existing fixture already covers the basic wrap case for a non-optional member accessed through `?.`. |
| `KT-125`, `KT-442` | `application_receiver_sugar` | `§2.8.4`, `§7.1.3` | `direct` | `partial` | `single-file` | `positive-run` | `expressions.application.positive_generic_receiver_method_sugar` | `accepted-now` | Staged in `new-tests/`; a generic receiver-marked function should still resolve and infer correctly through dotted method-call sugar. |
| `KT-258`, `KT-262`, `KT-442` | `application_receiver_sugar` | `§2.8.4`, `§7.1.3` | `direct` | `partial` | `single-file` | `positive-run` | `expressions.application.positive_generic_receiver_method_infers_argument` | `accepted-now` | Staged in `new-tests/`; a generic receiver-marked function with additional arguments should infer the container type from the receiver and use that type for later arguments. |
| `KT-146` | `flow_typing_conjunction_rhs` | `§7.1.2A`, `§7.4.1`, `§7.4.2`, `§2.8.3` | `direct` | `partial` | `single-file` | `positive-run` | `expressions.conditionals.positive_conjunction_constructor_projection` | `accepted-now` | Staged in `new-tests/`; the right operand of `&&` is checked only in the success environment of the left operand, so constructor-field projection is valid there and in the resulting `then` body. |
| `KT-2143` | `optional_chaining` | `§7.1.1.2` | `direct` | `covered` | `single-file` | `positive-check` | `expressions.safe_navigation.positive_wrap` | `imported` | This is the concrete annotated-receiver side of the same rule: when the receiver type is known, `?.` should just elaborate. |
| `KT-418`, `KT-431` | `optional_chaining_receiver_type` | `§7.1.1.2` | `direct` | `partial` | `single-file` | `negative-check` | `expressions.safe_navigation.negative_non_optional_receiver` | `accepted-now` | Staged in `new-tests/`; Kappa deliberately requires the prefix of `?.` to have type `Option T`, so safe-navigation on a non-optional receiver, namespace, or module-like receiver is rejected instead of treated as ordinary `.`. |
| `KT-109` | `application_inference` | `§7.1`, `§7.2` | `direct` | `partial` | `single-file` | `positive-run` | `expressions.application.positive_generic_lambda_result_inference` | `accepted-now` | Staged in `new-tests/`; the polymorphic result type of the higher-order call should be inferred from the lambda argument and the call result without explicit type arguments. |
| `KT-549`, `KT-571` | `application_contextual_lambda_inference` | `§7.1.3`, `§7.2` | `direct` | `none` | `single-file` | `positive-check` | `expressions.lambdas.positive_generic_contextual_lambda_inference` | `accepted-now` | Staged in `new-tests/`; a higher-order generic function call should instantiate its type parameter from an ordinary argument and use that same instantiation to contextually type bare lambda binders. |
| `KT-353` | `application_expected_result_inference` | `§7.1.3.1`, `§7.2`, `§7.4.2` | `direct` | `partial` | `single-file` | `positive-check` | `expressions.application.positive_polymorphic_result_from_expected_type` | `accepted-now` | Staged in `new-tests/`; a polymorphic result-only call should instantiate from the expected type in annotated results, lambda bodies, and branch joins. |
| `KT-399` | `application_returned_function_call` | `§7.1`, `§7.1.3`, `§7.3` | `direct` | `partial` | `single-file` | `positive-run` | `expressions.application.positive_polymorphic_returned_function_immediate_application` | `accepted-now` | Staged in `new-tests/`; when a polymorphic call returns a function value, the returned function should remain callable immediately and after rebinding, with explicit or inferred type instantiation. |
| `KT-263` | `function_value_rebinding` | `§7.1`, `§7.1.3` | `direct` | `covered` | `single-file` | `positive-run` | `expressions.application.positive_function_declaration_rebinds_as_value` | `accepted-now` | Staged in `new-tests/`; a named function declaration must be usable as an ordinary function value and may be rebound through a typed local. |
| `KT-328`, `KT-329` | `local_declarations_in_lambdas` | `§6.3.1`, `§7.2.1`, `§14.1.1` | `direct` | `partial` | `single-file` | `positive-run` | `declarations.let_in.positive_local_function_inside_lambda` | `accepted-now` | Staged in `new-tests/`; lambda body suite sugar must admit local signatures and named local function definitions with ordinary lexical capture. |
| `KT-202`, `KT-214` | `named_function_application` | `§7.1.3C` | `direct` | `partial` | `single-file` | `positive-run` / `negative-check` | `expressions.application.positive_named_function_application`, `expressions.application.negative_named_function_application_labels` | `accepted-now` | Staged in `new-tests/`; ordinary function values with named explicit binders support named application, while missing / extra / duplicate labels remain errors. |
| `KT-1154` | `elvis` | `§7.1.2` | `direct` | `partial` | `single-file` | `positive-run` | `expressions.elvis.positive_expression_context` | `accepted-now` | Staged in `new-tests/`; the analogue checks that `?:` keeps the expected result type when used inside a larger arithmetic expression. |
| `KT-5335` | `elvis` | `§7.1.2` | `direct` | `partial` | `single-file` | `positive-run` | `expressions.elvis.positive_expression_context` | `accepted-now` | Kappa's `?:` is specified directly as an `Option` match, so once the fallback has type `T` the whole Elvis expression has type `T`; the staged expression-context fixture covers the important consequence. |
| `KT-257` | `generic_data_runtime_projection` | `§11.1`, `§7.3.4`, `§7.4.1` | `direct` | `partial` | `single-file` | `positive-run` | `types.generics.runtime_positive_nested_generic_payload_projection` | `accepted-now` | Staged in `new-tests/`; nested generic constructor payloads should preserve their value and field type through branch-local constructor refinement. |
| `KT-312` | `generic_optional_return` | `§5.4.9`, `§7.1.2`, `§11.1` | `direct` | `partial` | `single-file` | `negative-check` | `types.generics.negative_optional_generic_return_not_plain` | `accepted-now` | Staged in `new-tests/`; a generic `Option` return must not silently satisfy a plain expected payload type. |
| `KT-313`, `KT-314` | `generic_optional_payload` | `§5.4.9`, `§7.5`, `§11.1` | `direct` | `partial` | `single-file` | `positive-run` | `types.generics.positive_optional_generic_unwrap_with_fallback` | `accepted-now` | Staged in `new-tests/`; matching on `Option a` must return a value at the same instantiated payload type, both for present and fallback branches. |
| `KT-332` | `optional_function_type_sugar` | `§5.4.9` | `direct` | `partial` | `single-file` | `positive-run` | `definitional_equality.positive_optional_function_type_sugar` | `accepted-now` | Staged in `new-tests/`; `(Unit -> Int)?` must parse as `Option (Unit -> Int)`, while `Unit -> Int?` parses as `Unit -> Option Int`. |
| `KT-410`, `KT-538` | `optional_tuple_type_sugar` | `§4.5`, `§5.4.9` | `direct` | `partial` | `single-file` | `positive-run` | `definitional_equality.positive_optional_tuple_type_sugar` | `accepted-now` | Staged in `new-tests/`; tuple types participate in postfix optional type sugar like any other type expression, so `(A, B)?` is `Option (A, B)`, and tuple payloads can be introduced with `Some`. |
| `KT-338` | `flow_typing_nested_declarations` | `§6.3.1`, `§7.4.1`, `§7.4.2` | `direct` | `partial` | `single-file` | `positive-run` | `expressions.conditionals.positive_refinement_visible_in_local_function` | `accepted-now` | Staged in `new-tests/`; constructor-refinement evidence should be available while checking a local function declaration nested in the success branch. |
| `KT-368` | `flow_typing_pattern_condition_order` | `§7.4.1`, `§7.4.2`, `§7.5.1` | `direct` | `partial` | `single-file` | `positive-run` | `expressions.conditionals.positive_pattern_condition_refines_scrutinee` | `accepted-now` | Staged in `new-tests/`; constructor-test and pattern-condition evidence should compose in either order so both the original scrutinee and bound payload remain usable in the success branch. |
| `KT-341` | `for_iterator_protocol` | `§8.7.5`, `§12.2.1` | `direct` | `partial` | `single-file` | `positive-check` | `effects.for_loops.positive_iterator_protocol` | `accepted-now` | Staged in `new-tests/`; `for x in xs do` should accept an initial iterator state implementing the standard `Iterator` protocol. |
| `KT-343`, `KT-521` | `lambda_expected_unit_no_discard` | `§4.5`, `§6.3.1`, `§7.2` | `direct` | `partial` | `single-file` | `negative-check` | `expressions.lambdas.negative_expected_unit_does_not_discard_result` | `accepted-now` | Staged in `new-tests/`; expected `Unit` does not make a lambda discard a non-`Unit` final expression. |
| `KT-351` | `loop_statement_not_pure_expression` | `§6.3.1`, `§8.7.5` | `direct` | `partial` | `single-file` | `negative-check` | `effects.loops.negative_loop_not_pure_expression` | `accepted-now` | Staged in `new-tests/`; `while` / `for` are do-block statements and cannot appear as pure block expressions. |
| `KT-352` | `local_function_value_signature_checked` | `§6.3.1`, `§7.2` | `direct` | `partial` | `single-file` | `negative-check` | `declarations.let_in.negative_local_value_signature_checked` | `accepted-now` | Staged in `new-tests/`; a local value with a function type signature must reject a lambda whose binder type does not match. |
| `KT-264`, `KT-265`, `KT-371`, `KT-372` | `constructor_defaults` | `§11.1`, `§11.1.1` | `direct` | `partial` | `single-file` | `positive-run` / `negative-check` | `data_types.constructor_defaults.positive_named_defaults`, `data_types.constructor_defaults.negative_default_expression_checked`, `data_types.constructor_defaults.negative_default_dependency_order` | `accepted-now` | Staged in `new-tests/`; constructor defaults must be checked in declaration context, may use earlier parameters / in-scope declarations, must reject self or later-parameter references, and are inserted only through named constructor application. |
| `KT-267` | `labeled_lambda_return` | `§8.4`, `§8.4.1`, `§7.2` | `direct` | `partial` | `single-file` | `positive-run` | `effects.return.runtime_positive_labeled_lambda_rebound_as_value` | `accepted-now` | Staged in `new-tests/`; a labeled lambda rebound as a value still admits `return@label` targeting that lambda. |
| `KT-411`, `KT-439` | `labeled_lambda_return_argument` | `§8.4`, `§8.4.1`, `§7.2` | `direct` | `partial` | `single-file` | `positive-run` | `effects.return.runtime_positive_labeled_lambda_argument_return` | `accepted-now` | Staged in `new-tests/`; a labeled lambda supplied as a function argument should type `return@label` against the lambda's result, not against the surrounding application. |
| `KT-413` | `labeled_return_local_function_boundary` | `§8.4`, `§8.4.1`, `§6.3.1` | `direct` | `partial` | `single-file` | `negative-check` | `effects.return.negative_labeled_return_crosses_local_function` | `accepted-now` | Staged in `new-tests/`; `return@outer` must not cross a user-written local-function boundary while resolving a named outer function target. |
| `KT-272` | `constructor_test_rhs` | `§7.3.4` | `direct` | `partial` | `single-file` | `negative-check` | `expressions.conditionals.negative_constructor_test_rhs_must_be_constructor` | `accepted-now` | Staged in `new-tests/`; the RHS of `is` is a constructor name only, not a type expression. |
| `KT-291` | `data_type_object_not_constructor` | `§2.8.6`, `§11.1` | `direct` | `partial` | `single-file` | `negative-check` | `data_types.data_declarations.negative_type_object_is_not_constructor` | `accepted-now` | Staged in `new-tests/`; a data declaration reifies as its type constructor, not as a value constructor. |
| `KT-292` | `empty_data_type` | `§11.1` | `direct` | `partial` | `single-file` | `positive-check` | `data_types.data_declarations.positive_empty_data_type` | `accepted-now` | Staged in `new-tests/`; constructorless data declarations such as `Void`-style types are valid. |
| `KT-195` | `imports_duplicate_alias` | `§2.3.1`, `§2.8.1` | `direct` | `partial` | `single-file` | `negative-check` | `modules.imports.negative_duplicate_alias` | `accepted-now` | Staged in `new-tests/`; two imports in the same scope must not contribute the same module alias spelling as distinct module declarations. |
| `KT-424` | `duplicate_term_declaration` | `§2.8.1` | `direct` | `partial` | `single-file` | `negative-check` | `modules.names.negative_duplicate_term_declaration` | `accepted-now` | Staged in `new-tests/`; one lexical scope may contribute at most one ordinary term declaration for a spelling, so same-scope ordinary overload declarations are rejected. |
| `KT-381`, `KT-382`, `KT-478` | `same_module_fragments` | `§2.1`, `§6.5` | `direct` | `partial` | `directory-suite` | `positive-run` | `modules.files.positive_same_module_fragments` | `accepted-now` | Staged in `new-tests/`; dotted filename fragments with the same path-derived module name should share ordinary declarations and satisfy `expect` declarations across source files. |
| `KT-175` | `flow_typing_stable_alias_transport` | `§7.4.1`, `§7.4.3`, `§2.8.3` | `direct` | `partial` | `single-file` | `positive-run` | `expressions.conditionals.positive_stable_alias_transport` | `accepted-now` | Staged in `new-tests/`; once a simple local alias is tested, constructor-refinement evidence must transport across the alias equality so both the alias and the underlying representative project consistently. |
| `KT-1982` | `flow_typing` | `§7.4.1`, `§7.4.2`, `§2.8.3` | `direct` | `partial` | `single-file` | `positive-run` | `expressions.conditionals.positive_disjunction_constructor_projection` | `accepted-now` | Staged in `new-tests/`; the branch body is rechecked under each successful disjunct so a shared constructor-field projection is valid under `||`. |
| `KT-3899` | `postdominating_flow_refinement` | `§8.2.2A`, `§7.4.1`, `§7.4.3` | `direct` | `partial` | `single-file` | `positive-run` | `effects.do_blocks.positive_postdominating_constructor_refinement` | `accepted-now` | Staged in `new-tests/`; the Kappa analogue uses a terminal `else` branch in a `do` sequence so later do-items are checked under the surviving success facts of the constructor test. |
| `KT-437`, `KT-443`, `KT-493` | `assignment_requires_var_target` | `§8.2` | `direct` | `partial` | `single-file` | `negative-check` | `effects.do_blocks.negative_assignment_requires_var` | `accepted-now` | Staged in `new-tests/`; do-block assignment sugar may target only a mutable variable introduced by `var`, not an immutable local, read-only property analogue, or arbitrary expression. |
| `KT-451` | `character_literal_scalar_count` | `§4.4` | `direct` | `partial` | `single-file` | `negative-check` | `literals.character_literals_char.negative_empty_or_multiple_scalars` | `accepted-now` | Staged in `new-tests/`; character literals must contain exactly one Unicode scalar value and reject empty or multi-scalar contents. |
| `KT-457` | `receiver_marked_explicit_call` | `§2.8.4`, `§7.1.3`, `§7.2` | `direct` | `partial` | `single-file` | `positive-run` | `expressions.application.positive_receiver_method_explicit_call` | `accepted-now` | Staged in `new-tests/`; receiver-marked functions remain ordinary explicit-parameter callables, so dotted method-call sugar must not be the only way to pass the receiver. |
| `KT-458` | `backtick_identifiers` | `§3.1` | `direct` | `partial` | `single-file` | `positive-run` | `lexical.identifiers.positive_backtick_weird_names` | `accepted-now` | Staged in `new-tests/`; backtick-quoted identifiers must support reserved words and weird symbolic spellings as ordinary identifiers. |
| `KT-459`, `KT-465`, `KT-507` | `qualified_constructor_inference` | `§2.8.3`, `§5.2`, `§7.1.3`, `§11.1` | `direct` | `partial` | `directory-suite` | `positive-run` | `types.generics.positive_qualified_constructor_type_inference` | `accepted-now` | Staged in `new-tests/`; qualifying a generic type / constructor through a module path must not disable ordinary type-name resolution or expected-type and argument-based type inference, including when an explicit type argument position contains a function type. |
| `KT-460` | `pure_block_expression` | `§6.3.1` | `direct` | `partial` | `single-file` | `positive-run` | `declarations.let_in.positive_pure_block_expression_value` | `accepted-now` | Staged in `new-tests/`; a `block` expression should introduce local declarations and return the final expression value without requiring an immediately invoked closure. |
| `KT-461` | `character_literal_escape_validation` | `§4.4` | `direct` | `partial` | `single-file` | `negative-check` | `literals.character_literals_char.negative_dangling_escape` | `accepted-now` | Staged in `new-tests/`; a dangling escape in a character literal is ill-formed and must not be accepted as a backslash character. |
| `KT-463` | `character_literal_escape_sequences` | `§4.4`, `§17.2.5` | `direct` | `partial` | `single-file` | `positive-check` | `literals.character_literals_char.positive_escape_sequences` | `accepted-now` | Staged in `new-tests/`; the source issue is mainly crash-on-incomplete-code tooling background, but its valid character escape list maps directly to Kappa's required `Char` escape support. |
| `KT-468` | `module_scope_isolation` | `§2.8.1`, `§2.8.3` | `direct` | `partial` | `directory-suite` | `positive-run` | `modules.names.positive_same_names_in_distinct_modules` | `accepted-now` | Staged in `new-tests/`; same-spelling terms in distinct modules are separate binding groups and should not conflict when selected through their module receivers. |
| `KT-475` | `receiver_projection_sugar` | `§2.8.4`, `§6.1.1` | `direct` | `partial` | `single-file` | `positive-check` | `types.projections.positive_receiver_projection_sugar` | `accepted-now` | Staged in `new-tests/`; a projection rooted at `place this` should elaborate through dotted receiver-projection sugar, matching the extension-property-style access pattern without importing Kotlin property syntax. |
| `KT-476` | `loop_else` | `§8.5.2`, `§8.7.5` | `direct` | `none` | `single-file` | `positive-check` | `effects.loops.positive_loop_else_typechecks` | `accepted-now` | Staged in `new-tests/`; `while ... do ... else do ...` should typecheck as a do-block statement whose `else` block runs only on normal loop completion without a targeted break. |
| `KT-483` | `numeric_literal_token_boundary` | `§4.1.2`, `§4.1.6`, `§7.1.1` | `direct` | `none` | `single-file` | `negative-check` | `literals.numeric_literals.negative_trailing_dot_float` | `accepted-now` | Staged in `new-tests/`; a decimal point belongs to a float literal only when followed by digits, so `1.` is not accepted as a completed floating literal and must not swallow the dotted-form token boundary. |
| `KT-494`, `KT-495` | `numeric_literal_expected_type` | `§4.1.4`, `§4.1.5`, `§7.1.3` | `direct` | `none` | `single-file` | `positive-check` | `literals.numeric_literals.positive_signed_expected_type_and_comparison` | `accepted-now` | Staged in `new-tests/`; signed integer expressions and comparison operands should use the expected non-default numeric type instead of prematurely defaulting the literal before prefix or equality/operator checking. |
| `KT-496`, `KT-517` | `try_finally_finalizer_shape` | `§9.2` | `direct` | `none` | `single-file` | `positive-check` / `negative-check` | `errors.try_except_finally.positive_finally_typechecks`, `errors.try_except_finally.negative_finally_must_be_unit` | `accepted-now` | Staged in `new-tests/`; `finally` is a cleanup action of type `m Unit`, runs around the primary computation, and must not replace the primary result type. |
| `KT-500` | `try_except_handler_result` | `§9.2` | `direct` | `none` | `single-file` | `positive-check` | `errors.try_except.positive_except_result_typechecks` | `accepted-now` | Staged in `new-tests/`; when a `try` computation raises a matching error, the matching `except` handler supplies the whole expression's result rather than a default or zero value. |
| `KT-516` | `nested_elif_desugaring` | `§7.4`, `§7.4.1` | `direct` | `partial` | `single-file` | `positive-check` | `expressions.conditionals.positive_nested_elif_typechecks` | `accepted-now` | Staged in `new-tests/`; `elif` is just nested `else if` sugar, so a chain of branches must typecheck as one conditional expression with a shared result type. |
| `KT-530` | `unit_noop_branch` | `§4.5`, `§7.5` | `direct` | `partial` | `single-file` | `positive-check` | `expressions.match.positive_unit_noop_branch` | `accepted-now` | Staged in `new-tests/`; Kappa requires an explicit `()` branch body for no-op branches rather than accepting an empty branch body. |
| `KT-537` | `map_literal` | `§10.1`, `§10.3.1` | `direct` | `none` | `single-file` | `positive-check` | `collections.literals.positive_map_literal` | `accepted-now` | Staged in `new-tests/`; built-in map literals use `{ key : value, ... }`, and empty `{}` is a map literal whose key/value types come from context. |

## Useful But Secondary Fits

| source | bucket | spec_refs | fit | current_coverage | fixture_kind | expected_outcome | proposed_fixture_root | status | notes |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| `KT-2146` | `flow_typing_negative_constructor_remainder` | `§7.4.1`, `§17.3.1.8` | `indirect` | `none` | `single-file` | `positive-check` | `expressions.conditionals.positive_else_branch_negative_constructor_refinement` | `accepted-later` | This is a spec-clarity item more than an immediate fixture: the current prose is explicit about success-side `HasCtor` projection, but much less explicit about what failure-side `LacksCtor` buys you when only one constructor remains. |
| `KT-2127` | `flow_typing_optional_presence` | `§7.1.1.2`, `§7.1.2`, `§7.4.1`, `§7.4.2` | `indirect` | `none` | `single-file` | `positive-check` | `expressions.conditionals.positive_safe_navigation_condition_implies_present_receiver` | `accepted-later` | This is a plausible future ergonomic extension, but v0.1 does not currently promise that a successful safe-navigation-derived boolean condition refines the receiver to the present case. |
| `KT-8492` | `flow_typing_optional_presence` | `§7.1.1.2`, `§7.1.2`, `§7.4.1`, `§7.4.2` | `indirect` | `none` | `single-file` | `positive-check` | `expressions.conditionals.positive_safe_navigation_condition_implies_present_receiver` | `accepted-later` | Same family as `KT-2127`, just with `?: false` on the condition path. This is tracked in `SPEC_ADDITIONS.md` as an ergonomic candidate rather than as current guaranteed behavior. |
| `KT-3818` | `flow_typing_nullable_subject_after_check` | `§7.3.4`, `§7.4.1`, `§7.4.3` | `indirect` | `covered` | `single-file` | `positive-check` | `expressions.conditionals.positive_constructor_test_projection` | `imported` | The Kappa analogue is the existing success-branch constructor-test projection story. Kotlin spells it as a null check; Kappa spells it as constructor refinement. |

## Additional Read Outcomes From 2026-04-24

- `KT-82`
  - direct cross-wave Kappa fit for Appendix B.2 plus dotted-form precedence; staged as `new-tests/lexical.operator_identifiers_fixity.runtime_positive_pipe_preserves_dotted_forms`.

- `KT-104`
  - direct Kappa fit for the negative side of common-success typing under `||`; staged as `new-tests/expressions.conditionals.negative_disjunction_projection_type_mismatch`.

- `KT-139`
  - direct cross-wave Kappa fit for lambda-local returns; staged as `new-tests/effects.return.runtime_positive_labeled_lambda_local_return`.

- `KT-144`
  - direct cross-wave Kappa fit for the negative side of lambda-local returns; staged as `new-tests/effects.return.negative_bare_return_crosses_anonymous_lambda`.

- `KT-146`
  - direct Kappa fit for flow typing through `&&`; staged as `new-tests/expressions.conditionals.positive_conjunction_constructor_projection`.

- `KT-57`
  - adjacent Kappa fit, but already covered by `tests/Kappa.Compiler.Tests/Fixtures/modules.imports.negative_missing_item`; it belongs to `§2.3` / `§2.3.1`, not to the main Chapter 7 Kotlin wave.

- `KT-58`
  - useful later background for `defer` / `finally` control-flow cleanup under `§8.6` / `§9.2`, but not a Wave 5 flow-typing import.

- `KT-78`
  - same semantic family as ordinary success-path refinement, but the issue itself is only about a redundant-cast warning. Kappa has no corresponding warning surface to import.

- `KT-89`
  - adjacent flow-typing fit, but already represented by the existing disjunctive-condition and or-pattern coverage in `new-tests/expressions.conditionals.positive_disjunction_constructor_projection` and `new-tests/expressions.patterns.positive_or_pattern_shared_binders`.

- `KT-1`, `KT-51`, `KT-56`
  - poor fit: effective-signature rules, overload ambiguity, and inherited-Java-member lookup depend on Kotlin/JVM OO surface rather than Kappa's dictionary/member model.

- `KT-2`, `KT-44`, `KT-46`
  - poor fit: these depend on Kotlin's nullable receiver/operator syntax or receiver-type parser surface, whereas Kappa reserves `?.` only for expression-level safe navigation.

- `KT-53`, `KT-55`, `KT-59`, `KT-63`, `KT-64`, `KT-65`
  - poor fit: class/property/constructor inference and nominal initialization issues do not translate into a clean Kappa fixture.

- `KT-60`, `KT-61`, `KT-62`
  - poor fit: tuple/enum/expression-pattern surface mismatches make these parser or front-end background only for now.

- `KT-52`, `KT-66`, `KT-67`
  - not useful fixture imports: docs, annotation support, and tooling/completion background only.

- `KT-68`, `KT-70`, `KT-72`, `KT-76`
  - not useful fixture imports: Java-compat annotations, array-lowering requests, and compiler-internal refactors rather than source-level Kappa semantics.

- `KT-69`, `KT-77`
  - poor fit: receiver-type parser and extension-surface debates do not translate into Kappa's current declaration and safe-navigation grammar.

- `KT-71`
  - poor fit: Kotlin's proposal to make `continue` jump between `when` branches is a language-specific control-flow design, not a Kappa `match` or loop obligation.

- `KT-73`, `KT-74`
  - poor fit: abstract-class property initialization and class-body grammar are nominal-OO background only.

- `KT-75`, `KT-79`
  - poor fit: the bound-checking requests are phrased in Kotlin's subtype-bound nominal generic surface rather than in a clean Kappa constraint or dependent-application analogue.

- `KT-80`
  - optimization-only background for range matching code generation; not a source-level fixture import.

- `KT-81`
  - poor fit: primary-constructor parameter/property lookup has no clean Kappa analogue.

- `KT-83`, `KT-86`, `KT-88`, `KT-91`, `KT-92`
  - poor fit: delegation syntax, abandoned syntax debates, multiple inheritance, constructor virtual-call policy, and 64-bit collection sizing do not translate into current Kappa surface semantics.

- `KT-84`
  - poor fit: lexer treatment of a Kotlin-specific `!is` token family has no Kappa analogue.

- `KT-85`
  - backend/debugger metadata only, not a source-language fixture import.

- `KT-87`
  - poor fit: nominal accessibility of constructor-promoted properties does not map cleanly onto Kappa's record or local-binding model.

- `KT-90`
  - front-end callee-resolution background only; it does not currently suggest a more precise Kappa fixture than the existing ordinary application coverage.

- `KT-98`, `KT-100`, `KT-101`, `KT-102`
  - poor fit: Java-package visibility, implicit primary-constructor handling, required nominal modifiers, and dropping `new` are nominal or interop syntax questions rather than Kappa semantic imports.

- `KT-99`
  - only broad macro-design background; Kappa already has a separate macro/staging wave, and this issue is too high-level to become a concrete fixture.

- `KT-103`
  - poor fit: Kotlin's statement-level `return` typing in ordinary block-bodied functions does not map cleanly onto Kappa's `return`-in-`do` model.

- `KT-106`, `KT-108`
  - poor fit: both depend on Kotlin's nullable receiver extension / overload surface rather than Kappa's dictionary and dotted-resolution rules.

- `KT-107`
  - syntax-only lambda-parameter sugar, not a semantic fixture import.

- `KT-110`, `KT-111`, `KT-113`
  - poor fit: constructor-promoted backing fields, operator spelling conventions, and Java-static-member interop are nominal or host-interop issues rather than direct Kappa fixtures.

- `KT-114`, `KT-115`, `KT-117`
  - diagnostic or nominal-constructor-policy background only; they do not suggest a new Kappa semantic fixture.

- `KT-116`, `KT-118`, `KT-119`
  - design-note material rather than concrete language-semantic bugs.

- `KT-120`, `KT-122`
  - Java interop / platform-type background only, not a Kappa source-language import.

- `KT-121`
  - poor fit: Kotlin compound-assignment operator conventions do not map directly onto current Kappa surface semantics.

- `KT-123`
  - interpolation-surface detail with no clear Kappa analogue worth a dedicated fixture.

- `KT-124`, `KT-126`, `KT-127`, `KT-128`
  - visibility-policy, standard-library implementation, `when`-specific extension syntax, and trailing-closure/default-argument ergonomics are not clean Kappa fixture imports.

- `KT-129`, `KT-130`, `KT-131`, `KT-133`
  - interop, generic-array construction, nominal initialization, and backend-mapper background only; none translate into a clean Kappa fixture.

- `KT-132`
  - useful ergonomic background rather than a direct current fixture: the request is for a condition-local name-binding form so a success branch can reuse a refined value without forcing a wider-scope temporary.

- `KT-134`, `KT-135`, `KT-137`, `KT-140`, `KT-141`, `KT-142`, `KT-147`, `KT-148`
  - Java/final/enums/typeinfo/trailing-closure/backing-field/background only; these are nominal, interop, or syntax-surface issues rather than clean Kappa imports.

- `KT-136`
  - already represented by ordinary function-value application coverage; the Kotlin bug is a front-end regression on a baseline application case rather than a new Kappa semantic obligation.

- `KT-143`
  - declaration-header / generic-binder-order background only; Kappa already fixes binder scope left-to-right, but this issue does not currently suggest a cleaner Wave 5 fixture than the existing ordered-binder rules.

- `KT-145`
  - weaker sibling of the staged `&&` refinement case and the existing success-branch constructor-projection fixtures; it does not need a separate Kappa test once those are in place.

- `KT-149`
  - poor fit: Kappa's `is` tests constructors, not erased parameterized runtime types, so this Java-generic-is-check request does not translate directly.

- `KT-150`, `KT-151`
  - nominal redeclaration and override-visibility background only; these are OO front-end policy issues rather than clean Kappa imports.

- `KT-152`, `KT-153`
  - raw-null and nullable-type-parameter background only; Kappa models optionality through constructor-based types rather than Kotlin's `null` semantics.

- `KT-154`, `KT-155`, `KT-156`, `KT-157`
  - library-equality, nominal-supertype initialization, `this<Super>` syntax, and class-object constraint background only; these are Kotlin library or OO surface issues rather than clean Kappa imports.

- `KT-158`
  - already represented by ordinary irrefutable `let`-pattern coverage; Kappa supports declaration-site destructuring directly under `§6.1.2`.

- `KT-159`, `KT-161`
  - parser and surface-syntax background only; object-literal punctuation and alternative function-type notation do not translate into Kappa fixture work.

- `KT-160`
  - Java-qualified-name host-generation background only; this is closer to host-binding / backend interop than to the current Kotlin Chapter 7 import wave.

- `KT-162`
  - collection-iteration inference background only; it does not currently suggest a cleaner Kappa import than the existing local binding and loop typing rules.

- `KT-163`
  - nominal `final` policy only, with no direct Kappa analogue worth a dedicated fixture.

- `KT-164`, `KT-165`, `KT-166`, `KT-167`
  - override-property sugar, nullable-receiver editor behavior, class objects, and object-expression background only; these are Kotlin nominal/editor surfaces rather than current Kappa imports.

- `KT-168`
  - generic-upper-bound intersection background only; Kappa expresses this space through explicit constraints and does not need a dedicated Wave 5 fixture here.

- `KT-169`
  - adjacent recursive-group background only: Kappa already accepts mutually recursive groups with explicit preceding signatures, but this Kotlin issue is about omitted-result-type inference for such groups rather than about the accepted SCC shape itself.

- `KT-170`, `KT-171`, `KT-172`, `KT-173`
  - constructor-delegation cycles, receiver-info plumbing, final-upper-bound warnings, and class-object diagnostics are Kotlin nominal/front-end background rather than clean Kappa fixture imports.

- `KT-174`
  - poor fit: nullable extension-receiver refinement depends on Kotlin's extension-method surface and `null` semantics rather than Kappa's constructor-test flow typing.

- `KT-175`
  - direct Kappa fit for stable local aliases under `§7.4.3`; staged as `new-tests/expressions.conditionals.positive_stable_alias_transport`.

- `KT-176`, `KT-177`, `KT-178`
  - block-body return requirements, string-template rename fallout, and anonymous initializers are syntax/nominal background only, not clean Kappa imports.

- `KT-179`, `KT-183`
  - impossible-null-check warnings and `null is T?` semantics are raw-null background only; Kappa models optionality via constructors rather than literal `null`.

- `KT-180`
  - class-object bound validation at call sites is Kotlin nominal/class-object background, not a current Kappa fixture import.

- `KT-181`
  - definite-assignment analysis belongs to a later mutable-variable / initialization pass rather than this Wave 5 flow-typing tranche.

- `KT-182`
  - Java-to-Kotlin type-conversion internals are host-interop background only.

- `KT-184`, `KT-185`, `KT-187`
  - raw-null branch crashes, uninitialized-property rules, and try/catch inference are front-end or nominal background rather than current Kappa imports.

- `KT-186`
  - broad pattern-matching design discussion belongs to the later pattern-language / ergonomics wave rather than this direct Kotlin fixture pass.

- `KT-188`
  - recursive inference through class bodies is nominal initialization background, not a clean Kappa import.

- `KT-189`
  - try/catch exit-shape control-flow plumbing belongs to the later control/finalization wave rather than this Kotlin flow-typing tranche.

- `KT-190`, `KT-191`, `KT-192`
  - character arithmetic, unsigned integers, and enum classes are core-language feature/background requests rather than current Kappa imports.

- `KT-194`
  - plain string-template support is a core surface-language feature request, not a current direct Kappa import.

- `KT-195`
  - direct Kappa fit for duplicate imported alias names in one lexical scope; staged as `new-tests/modules.imports.negative_duplicate_alias`.

- `KT-193`
  - no corresponding issue is present in the current local `kotlin-issues.json` snapshot; keep the gap explicit rather than silently skipping it.

- `KT-196`
  - nominal constructor-parameter / property distinction only; this is Kotlin class-member surface background, not a Kappa import.

- `KT-197`, `KT-198`
  - no corresponding issues are present in the current local `kotlin-issues.json` snapshot; keep the gaps explicit rather than silently skipping them.

- `KT-199`, `KT-200`
  - useful spec-addition material for the portable `Array` prelude surface rather than current fixture imports: Kappa names `Array a` as a standard type but does not yet specify a user-facing constructor or convenience members such as `indices` / `lastIndex`.

- `KT-201`
  - poor fit: nullable extension receivers with ordinary `.` calls depend on Kotlin's extension-method and raw-null model, while Kappa keeps optionality as `Option` plus `?.` / constructor tests.

- `KT-202`
  - direct Kappa fit after the newer spec addition for named function application; staged as `new-tests/expressions.application.positive_named_function_application` and `new-tests/expressions.application.negative_named_function_application_labels`.

- `KT-203`
  - implementation-crash / host-configuration background only; the sample touches safe calls and Elvis, but the actual failure is common-supertype computation without a configured JDK, not a Kappa source-semantics obligation.

- `KT-204`, `KT-205`, `KT-206`
  - enum-superclass initialization, package import from a Kotlin namespace, and recursive anonymous-object initialization are nominal / package / object-surface background rather than clean Kappa imports.

- `KT-207`, `KT-208`
  - property initializer placement and getter-line syntax are Kotlin class/property grammar details with no direct Kappa semantic fixture.

- `KT-209`, `KT-210`, `KT-211`
  - lower-bound polymorphism, usage-site variance, and decidable subtyping are intentional-divergence background: Kappa has no ambient structural subtyping and keeps coercion/subsumption surfaces explicit and bounded.

- `KT-212`
  - broad off-side-rule design background; Kappa's layout-sensitive parsing is already tracked by the GHC/Scala layout wave and the staged explicit-brace-after-layout negative, so no separate fixture is needed from this issue.

- `KT-213`, `KT-216`
  - equality/hash-code generation and interface factoring are nominal runtime-library design background, not current Kappa fixture work.

- `KT-214`
  - partial direct Kappa fit for named ordinary-function application, now staged with `KT-202`; Kotlin's generated data-class `copy` API remains nominal/library background rather than a separate current fixture.

- `KT-215`
  - no corresponding issue is present in the current local `kotlin-issues.json` snapshot; keep the gap explicit rather than silently skipping it.

- `KT-217`
  - no corresponding issue is present in the current local `kotlin-issues.json` snapshot; keep the gap explicit rather than silently skipping it.

- `KT-218`, `KT-220`
  - structural types and recursive variance bounds are intentional-divergence / solver-background material: Kappa has records, packages, row constraints, and explicit dynamic values, but not ambient structural subtyping or wildcard variance.

- `KT-219`
  - Kotlin class-delegation grammar background only; Kappa has no class inheritance / delegation-specifier surface to import.

- `KT-221`
  - direct Kappa fit for the explicit Unit-binder function surface; staged as `new-tests/expressions.lambdas.positive_unit_binder_is_single_argument`.

- `KT-222`, `KT-223`
  - object-literal / class-object grammar-documentation issues are Kotlin object-surface background only, not Kappa fixture imports.

- `KT-224`
  - compiler global-state / duplicated standard-library-scope background only; not a source-level Kappa semantic issue.

- `KT-225`
  - direct semantic family, already represented by constructor-test projection coverage: under a successful constructor test, Kappa permits constructor-field projection from the refined scrutinee.

- `KT-226`
  - poor fit: Kotlin class-method lookup on `Any` has no clean analogue in Kappa's receiver-driven record/dictionary/module lookup.

- `KT-227`
  - adjacent terminal-control-flow background: Kappa already specifies terminal `m Void` branches and `absurd`-based ex falso elaboration in `do`, but this Kotlin issue is about pseudocode/CFG metadata rather than a distinct source rule.

- `KT-228`
  - same intentional divergence as the broader mutable-smart-cast family: Kappa's stable-alias refinement excludes mutable locals / `var`-rooted places from automatic constructor-refinement transport.

- `KT-229`, `KT-230`
  - collection implementation generation and language-project merger discussion are not source-level Kappa semantic imports.

- `KT-231`
  - definite initialization / null-in-non-null-property background only; Kappa has no Kotlin-style uninitialized class property surface.

- `KT-232`
  - backend/codegen crash for a simple class is Kotlin implementation background, not a source-level Kappa fixture.

- `KT-233`, `KT-234`
  - direct semantic family, already represented by Kappa match-exhaustiveness fixtures; when exhaustiveness cannot be proven, a catch-all or equivalent branch is required.

- `KT-235`
  - Kotlin assignment-expression return typing is mutation/operator-surface background; Kappa's current user-facing mutation APIs return explicit `Unit` in the relevant monad rather than exposing Kotlin-style assignment expressions.

- `KT-236`
  - no corresponding issue is present in the current local `kotlin-issues.json` snapshot; keep the gap explicit rather than silently skipping it.

- `KT-237`
  - direct Kappa fit for `Unit` as an ordinary value type and generic type argument; staged as `new-tests/types.unit.positive_unit_as_generic_argument`.

- `KT-238`
  - array indexing / backend verification background; useful as supporting evidence for the portable `Array` API spec-addition note, but not a current Kappa source fixture.

- `KT-239`
  - adjacent safe-navigation fit, already represented by `?.` wrap/flatten coverage; Kotlin's `Int?.plus(Int)` method/operator details do not add a distinct Kappa fixture.

- `KT-240`
  - useful ergonomic background for a future scope-function or pipeline-binding form; tracked in `SPEC_ADDITIONS.md` rather than staged as current behavior.

- `KT-241`
  - iteration-surface background: Kappa has prelude `Iterator` / `IntoQuery` traits, but this Kotlin issue is about Java/Kotlin class `for` desugaring and user-class operator conventions.

- `KT-242`, `KT-246`
  - direct Kappa fit for same-type equality constraints; staged as `new-tests/traits.eq.negative_nullable_equality_requires_same_type`.

- `KT-243`
  - Java static-method / backend codegen crash background only; Kappa's host interop bridge is specified separately and this does not yield a current source-language fixture.

- `KT-244`
  - direct Kappa fit for applying branch refinement while checking a local `let` initializer; staged as `new-tests/expressions.conditionals.positive_refinement_applies_in_let_initializer`.

- `KT-245`
  - Java collection iteration / safe-call backend verification background; adjacent to `KT-241` and `KT-244`, but not a distinct current Kappa fixture.

- `KT-248`
  - cast/backend background only. Kappa now has explicit gradual support in `std.gradual`, but this Kotlin issue is about `as` / `as?` codegen crashes for primitive casts rather than a current Kappa source fixture.

- `KT-249`, `KT-255`
  - inner-class / class-object backend verification background only; Kappa has static members and same-spelling data-family facets, but no Kotlin OO inner-class construction surface.

- `KT-250`
  - nominal superclass-constructor resolution background only; Kappa has no superclass constructor-argument lookup surface.

- `KT-251`
  - accessor/getter-surface background only. It is adjacent to Kappa's accessor-bundle descriptor application, but Kotlin-style property getter declarations and setter type checking are not current Kappa syntax.

- `KT-252`
  - variance/subtyping intentional divergence; Kappa does not have ambient declaration-site `in` / `out` variance and keeps coercion/subsumption surfaces explicit.

- `KT-253`
  - `this` assignment is Kotlin OO/mutation surface background; Kappa has no assignable `this` value.

- `KT-254`
  - self-return typing for generic classes is Kotlin `this` / class-subtyping background rather than a Kappa fixture import.

- `KT-256`
  - recursive secondary constructors are Kotlin class-constructor surface background; Kappa data constructors are ordinary introduction forms rather than user-authored recursive constructor bodies.

- `KT-257`
  - direct Kappa fit for nested generic data payload representation and projection; staged as `new-tests/types.generics.runtime_positive_nested_generic_payload_projection`.

- `KT-258`, `KT-262`
  - direct Kappa fit for generic receiver-method elaboration with additional arguments; staged as `new-tests/expressions.application.positive_generic_receiver_method_infers_argument`.

- `KT-259`, `KT-260`
  - nullable generic cast background only. Kappa's current explicit gradual-cast surface is separate from Kotlin's nullable `as` / `as?` warning and codegen behavior.

- `KT-261`
  - nominal subclass override / dispatch background only; Kappa trait members elaborate through dictionaries rather than Kotlin's class override table.

- `KT-263`
  - direct Kappa fit for resolving a named function declaration as an ordinary first-class function value; staged as `new-tests/expressions.application.positive_function_declaration_rebinds_as_value`.

- `KT-264`
  - direct Kappa fit for checking constructor default expressions against the declared field type and lexical scope; staged as `new-tests/data_types.constructor_defaults.negative_default_expression_checked`.

- `KT-265`
  - direct Kappa fit for named constructor defaults; staged as `new-tests/data_types.constructor_defaults.positive_named_defaults`. Ordinary function defaults remain a separate future ergonomic candidate in `SPEC_ADDITIONS.md` because §7.1.3C explicitly excludes them in v0.1.

- `KT-266`
  - nominal/JVM subtype background only. Kappa has no ambient `String <: Object` subtyping rule, and host object typing belongs to bridge profiles rather than source-level Kappa conformance.

- `KT-267`
  - direct Kappa fit for labeled local return from a lambda assigned to a value; staged as `new-tests/effects.return.runtime_positive_labeled_lambda_rebound_as_value`.

- `KT-268`
  - overload/member-vs-extension resolution background only; Kappa's receiver fallback sugar is not Kotlin's member/extension overload model.

- `KT-269`
  - no corresponding issue is present in the current local `kotlin-issues.json` snapshot; keep the gap explicit rather than silently skipping it.

- `KT-270`
  - numeric cast-surface background only; Kappa has explicit conversions and gradual checked casts rather than Kotlin's `as` casts between primitive numeric types.

- `KT-271`
  - no corresponding issue is present in the current local `kotlin-issues.json` snapshot; keep the gap explicit rather than silently skipping it.

- `KT-272`
  - direct Kappa fit for rejecting non-constructor RHS forms in `is`; staged as `new-tests/expressions.conditionals.negative_constructor_test_rhs_must_be_constructor`.

- `KT-273`
  - backend condition-fact reporting / optimization background only; Kappa's source-level flow facts are already represented by the constructor-refinement and stable-alias fixtures.

- `KT-274`, `KT-276`
  - iteration-library background. `KT-276` is additional supporting evidence for the existing portable `Array` construction / iterator API note in `SPEC_ADDITIONS.md`, but it does not add a distinct current fixture.

- `KT-275`
  - logical-expression backend optimization background only; current Kappa fixtures already cover `&&` / `||` source typing and refinement behavior where needed.

- `KT-277`
  - integer-range codegen optimization background only; no current Kappa source-level range-membership fixture is implied.

- `KT-278`
  - integer-range membership codegen optimization background only; adjacent to `KT-277`, but not a source-level Kappa fixture import.

- `KT-279`, `KT-280`
  - runtime type-information / variance-projection metadata background only; Kappa's variance and host metadata rules are separate from Kotlin's `TypeInfo` implementation surface.

- `KT-281`
  - Kotlin block-bodied definite-return analysis background only. Kappa function bodies are expressions, and `return` is a `do`-scoped control form, so this does not translate into a distinct current fixture.

- `KT-282`
  - nullable extension-function and binary-operator background only. Kappa uses `Option` plus `?.` and receiver-marked functions rather than Kotlin's nullable receiver extension/operator resolution.

- `KT-283`, `KT-284`
  - Kotlin trait backend/static-check background only; Kappa traits elaborate through dictionaries and do not share Kotlin's interface/class construction surface.

- `KT-285`
  - Kotlin delegation backend background only; Kappa has no class delegation surface to import here.

- `KT-286`
  - Kotlin supertype-list validation background only; Kappa has trait constraints and explicit instance/coherence rules rather than class supertype lists.

- `KT-287`
  - direct Kappa fit for inferring constructor type arguments from expected type and supplied values; staged as `new-tests/types.generics.positive_constructor_type_arguments_inferred`.

- `KT-288`, `KT-290`
  - Kotlin trait implementation-class backend background only; no source-level Kappa fixture import.

- `KT-289`
  - Kotlin multiple-inheritance/class-list background only; Kappa has explicit trait constraints and no class inheritance surface.

- `KT-291`
  - direct Kappa fit for keeping data type objects distinct from value constructors; staged as `new-tests/data_types.data_declarations.negative_type_object_is_not_constructor`.

- `KT-292`
  - direct Kappa fit for constructorless data declarations; staged as `new-tests/data_types.data_declarations.positive_empty_data_type`.

- `KT-293`
  - Kotlin function-backed delegation background only; Kappa has explicit functions and dictionaries rather than class delegation syntax.

- `KT-294`
  - malformed Kotlin class/function declaration diagnostic background only; no distinct Kappa source fixture beyond ordinary parser/diagnostic coverage.

- `KT-295`
  - useful ergonomic material for assertion / requirement precondition forms with lazy diagnostic messages; tracked in `SPEC_ADDITIONS.md` rather than staged as current behavior.

- `KT-296`
  - Android/build-system support background only.

- `KT-297`
  - Kotlin trait/JVM overload ambiguity background only; Kappa's trait members elaborate through dictionaries and existing supertrait/coherence fixtures cover the closer obligations.

- `KT-298`
  - cast-surface background only; Kappa's explicit gradual checked-cast surface is separate from Kotlin's `as` / `as?` handling for non-class type operands.

- `KT-299`, `KT-300`
  - user-defined range / nullable range-membership operator background only. This reinforces the existing Array/range prelude ergonomics note, but current Kappa has no Kotlin-style `in` operator over arbitrary `contains` methods to stage.

- `KT-301`
  - intentional divergence / runtime-representation background: Kappa does not implicitly require full runtime type reification for ordinary generic code, and erased compile-time parameters already have explicit quantity/scope rules.

- `KT-302`
  - Kotlin multiple-inherited-implementation background; the closest Kappa obligations are trait coherence and overlap checks already represented by staged solver/coherence fixtures.

- `KT-303`, `KT-304`, `KT-305`
  - Kotlin class-hierarchy / supertype-list / runtime-TypeInfo background only; Kappa does not have nominal class inheritance or Kotlin-style supertype projections.

- `KT-306`
  - Kotlin receiver-lambda `this`-stack background only. Kappa has receiver-marked binders and method-call sugar, but no nested implicit receiver-lambda stack matching this issue's surface.

- `KT-307`
  - Kotlin trait/codegen unresolved-reference background only; the sample combines traits, casts, and Elvis, but the actual failure is not a new Kappa source-level obligation.

- `KT-308`, `KT-309`
  - intentional divergence / Kotlin raw-null bottom-type background. Kappa exposes absence through `Option` / `T?` and terminal computations through `Void` / monadic effects rather than a raw `null : Nothing?` inhabitant.

- `KT-310`
  - diagnostic-background only: "property not function" would be a quality-of-diagnostics pass for applying a non-function value, not a new Kappa semantic rule; tracked in `SPEC_ADDITIONS.md` under standard diagnostic codes.

- `KT-311`
  - unresolved-constructor diagnostic background only; the closest semantic obligation, keeping type objects distinct from value constructors, is already staged from `KT-291`.

- `KT-312`
  - direct Kappa fit for generic optional returns preserving `Option`; staged as `new-tests/types.generics.negative_optional_generic_return_not_plain`.

- `KT-313`, `KT-314`
  - direct Kappa fit for generic optional payload substitution / refinement; staged as `new-tests/types.generics.positive_optional_generic_unwrap_with_fallback`.

- `KT-316`
  - Kotlin trait/class override background only. Kappa trait members and default members elaborate through dictionaries and instances, not through class inheritance / open-member override rules.

- `KT-317`
  - backend representation background only: Kotlin default trait-method bytecode sharing does not create a Kappa source-level fixture.

- `KT-318`
  - Kotlin override erasure / bridge-method background only; Kappa erasure is specified separately and trait members elaborate through dictionaries rather than JVM override slots.

- `KT-319`
  - runtime type-information background only; Kappa's runtime shape / dynamic type evidence rules are explicit and do not import Kotlin inner-class metadata behavior.

- `KT-320`
  - useful ergonomic evidence for finite type-set bounds on generic functions; tracked in `SPEC_ADDITIONS.md` rather than staged as current behavior because Kappa currently expresses the nearby use case with value-level unions or explicit constraints.

- `KT-321`
  - already represented as spec-level implementation guidance: Kappa has deterministic parallel frontend execution rules and runtime profile flags for parallel runtime support, but this does not yield a source fixture.

- `KT-322`
  - Kotlin enum/class inheritance background only; Kappa data constructors and finite variants have no nominal class-inheritance surface to prohibit.

- `KT-323`
  - Kotlin override visibility policy background only; Kappa has module visibility / opacity and dictionary-backed trait instances, not subclass override visibility.

- `KT-324`
  - Kotlin class-delegation visibility background only; Kappa has explicit dictionaries/functions and no delegation-by-member forwarding surface.

- `KT-325`
  - accessor override / delegation background only. Kappa has accessor-bundle descriptors, but no Kotlin-style property accessor overriding or delegation syntax.

- `KT-326`
  - array backend verification background; reinforces the existing portable `Array` construction / indexing / accessor ergonomics note, but does not yield a current Kappa source fixture.

- `KT-327`
  - Kotlin compiler-internal class-object naming background only.

- `KT-328`, `KT-329`
  - direct Kappa fit for local function declarations inside lambda bodies; staged as `new-tests/declarations.let_in.positive_local_function_inside_lambda`.

- `KT-330`
  - adjacent recursive/object-initialization background. Kappa already requires explicit signatures for recursive groups; this Kotlin issue is about retaining partial type information after object-recursion failures rather than a new accepted Kappa source shape.

- `KT-331`
  - Array convenience-member ergonomics evidence; folded into the existing `SPEC_ADDITIONS.md` note for portable `Array` construction / indexing / convenience members.

- `KT-332`
  - direct Kappa fit for optional function type sugar and precedence; staged as `new-tests/definitional_equality.positive_optional_function_type_sugar`.

- `KT-333`
  - intentional divergence / terminology background: Kappa keeps `Unit` as the inhabited singleton and `Void` as the empty type, with both already specified and covered by staged tests.

- `KT-334`, `KT-335`
  - Java import / `Comparable` / `Collections.sort` inference background only; Kappa's host binding and trait constraint surfaces do not import Kotlin/JVM short-name behavior.

- `KT-336`
  - generic host collection constructor inference background only. The closest Kappa source-level obligation, generic constructor type-argument inference, is already staged from `KT-287`.

- `KT-337`
  - useful formatting ergonomics evidence for allowing a line break before a dotted suffix; tracked in `SPEC_ADDITIONS.md` rather than staged as current behavior because §3.4 does not currently list leading dotted suffixes as expression-continuation contexts.

- `KT-338`
  - direct Kappa fit for refinement visibility in nested local declarations; staged as `new-tests/expressions.conditionals.positive_refinement_visible_in_local_function`.

- `KT-339`
  - useful ergonomic evidence for a shorthand for `Unit`-returning function / callback types; tracked in `SPEC_ADDITIONS.md` rather than staged as current behavior.

- `KT-340`
  - Kotlin autocast plus virtual method dispatch background only. Kappa's constructor-refinement and method/dictionary model does not have override-compatible class dispatch ambiguities.

- `KT-341`
  - direct Kappa fit for `for` over an explicit iterator state implementing the standard `Iterator` protocol; staged as `new-tests/effects.for_loops.positive_iterator_protocol`.

- `KT-342`
  - Kotlin project/empty-file redeclaration background only; no current Kappa fixture beyond ordinary empty-module/file handling.

- `KT-343`
  - direct Kappa fit for the negative side of expected-`Unit` lambda checking; staged as `new-tests/expressions.lambdas.negative_expected_unit_does_not_discard_result`.

- `KT-344`
  - mutable closure-capture background only. Kappa's current closure story is capture/quantity inference over immutable bindings and explicit state threading, not Kotlin-style mutable local capture by reference.

- `KT-345`
  - inline-function / breakable-loop annotation background only. Kappa's current abrupt-control rules already keep `break` / `continue` target resolution lexical and do not have a Kotlin-style inline-loopbody surface to import.

- `KT-346`, `KT-347`
  - Array convenience-member ergonomics evidence for membership and sorting helpers; folded into the existing `SPEC_ADDITIONS.md` note for portable `Array` construction / indexing / convenience members.

- `KT-348`
  - Array convenience-member ergonomics evidence for binary search; folded into the existing `SPEC_ADDITIONS.md` note for portable `Array` construction / indexing / convenience members.

- `KT-349`, `KT-350`
  - runtime type-information coverage evidence; tracked in `SPEC_ADDITIONS.md` as a request to clarify which `DynamicType` / `DynRep` instances are portable, especially for tuples and higher-order function values.

- `KT-351`
  - direct Kappa fit for keeping loops out of pure expression/block positions; staged as `new-tests/effects.loops.negative_loop_not_pure_expression`.

- `KT-352`
  - direct Kappa fit for checking local function-value declarations against their local signatures; staged as `new-tests/declarations.let_in.negative_local_value_signature_checked`.

- `KT-353`
  - direct Kappa fit for expected-type-directed instantiation of a polymorphic result; staged as `new-tests/expressions.application.positive_polymorphic_result_from_expected_type`.

- `KT-354`
  - Kotlin inner-class inheritance background only. Kappa has no nested OO class inheritance surface; trait/static member lookup is dictionary/static-object based instead.

- `KT-355`
  - compiler import-pipeline background only. Kappa's current resolve phases make import handling and declaration-shape construction explicit, but this issue does not expose a portable source-level fixture beyond existing import-resolution tests.

- `KT-356`
  - absent from the current local Kotlin corpus snapshot.

- `KT-357`
  - Kotlin inline-function non-local return policy background. The Kappa analogue is already represented by lambda return-boundary fixtures, especially `new-tests/effects.return.negative_bare_return_crosses_anonymous_lambda`.

- `KT-358`
  - redundant safe-navigation diagnostic evidence; tracked in `SPEC_ADDITIONS.md` as a warning / diagnostic hook rather than staged as current required behavior.

- `KT-359`
  - Kotlin mutable-variable / ref-cell lowering background only. Kappa's current mutable state surfaces are explicit `var`, references, `inout`, or monadic state, not implicit wrapping of captured locals for `++` / `+=`.

- `KT-360`
  - Kotlin virtual default-argument overriding background only. Kappa has constructor defaults and named ordinary-function application, but no OO override surface where default values can be changed downstream.

- `KT-361`
  - label-resolver implementation background; the nearest Kappa semantic obligation is already staged under labeled-lambda return and bare-return lambda-boundary fixtures.

- `KT-362`
  - Kotlin visibility policy for smart casts on properties background only. Kappa's stable-alias refinement rules are lexical and syntax-directed, not based on public/internal property visibility.

- `KT-363`
  - standalone compiler / JDK packaging background only; no Kappa language-spec analogue.

- `KT-364`
  - Kotlin root-package namespace syntax background only. Kappa's module/import model already has explicit module paths and does not import Kotlin's `namespace` root-package surface.

- `KT-365`
  - absent from the current local Kotlin corpus snapshot.

- `KT-366`
  - vague frontend resolved-call type-parameter metadata background. The closest source-level obligations are already covered by generic application / expected-result inference fixtures.

- `KT-367`
  - Kotlin parser ambiguity around nullable type patterns and binding patterns. Kappa's `is` right-hand side is constructor-only and the closest semantic rejection is already staged under `new-tests/expressions.conditionals.negative_constructor_test_rhs_must_be_constructor`.

- `KT-368`
  - direct Kappa fit for composing constructor-test and pattern-condition evidence in either condition-clause order; staged as `new-tests/expressions.conditionals.positive_pattern_condition_refines_scrutinee`.

- `KT-369`
  - namespace initialization-order design background only. Kappa's module declarations are not Kotlin namespace property initializers and this does not yield a portable source fixture.

- `KT-370`
  - Kotlin OO override / named-argument design background only. Kappa named ordinary-function application has preserved binder metadata, but no virtual override surface where parameter names can diverge.

- `KT-371`
  - direct Kappa fit for constructor default dependency-order checking; staged as `new-tests/data_types.constructor_defaults.negative_default_dependency_order`.

- `KT-372`
  - direct Kappa fit for non-constant constructor defaults using earlier parameters and in-scope declarations; already represented by `new-tests/data_types.constructor_defaults.positive_named_defaults`.

- `KT-373`
  - absent from the current local Kotlin corpus snapshot.

- `KT-374`
  - ordinary function default-argument ergonomics evidence; folded into the existing `SPEC_ADDITIONS.md` note because Kappa v0.1 deliberately limits defaults to named constructor application.

- `KT-375`
  - Kotlin root `Any` intrinsic background only. Kappa has explicit prelude traits and no ambient `Any.toString` / `Any.equals` object root to import as a current source fixture.

- `KT-376`
  - Array documentation / construction-surface evidence; folded into the existing `SPEC_ADDITIONS.md` note for portable `Array` construction and convenience members.

- `KT-377`
  - Kotlin OO `super` receiver restrictions background only. Kappa has no `super` expression or extension-function receiver model matching this surface.

- `KT-378`, `KT-380`
  - Kotlin `super` documentation / backend implementation background only. Kappa has no current class-inheritance `super` expression surface to import.

- `KT-379`
  - host-callback adapter ergonomics evidence; tracked in `SPEC_ADDITIONS.md` as an explicit bridge/callback adapter candidate rather than staged as current behavior.

- `KT-381`, `KT-382`, `KT-478`
  - direct Kappa fit for same-module source fragments and cross-fragment name resolution / `expect` satisfaction; staged as `new-tests/modules.files.positive_same_module_fragments`.

- `KT-383`
  - Kotlin object-literal / expression-body return-checking background only. Kappa has do-scoped `return` rules and no Kotlin object literal body surface matching this issue.

- `KT-384`
  - abstract-property descriptor / backend-plumbing background only; no Kappa source-language fixture is implied.

- `KT-385`
  - adjacent higher-order receiver / callback inference background, but the failure depends on Kotlin overload sets and extension-function machinery. Kappa's cleaner current obligations are already staged under generic receiver-method inference and Unit-return callback ergonomics notes.

- `KT-386`
  - Java mapping documentation background only; no Kappa semantic import.

- `KT-387`
  - variadic host/backend support evidence; folded into the existing `SPEC_ADDITIONS.md` portable-ABI diagnostics note rather than staged as current source behavior.

- `KT-388`, `KT-389`
  - typed / inferred vararg support evidence; folded into the existing `SPEC_ADDITIONS.md` portable-ABI diagnostics note with `KT-387`. Kappa has fixed-arity curried functions and explicit bridge adapter profiles, so this does not become a current source-level test.

- `KT-390`
  - test-coverage reminder for constants. Kappa already has real fixtures for numeric, string, interpolated string, and character literals under `tests/Kappa.Compiler.Tests/Fixtures/literals.*`, so no new staged test is needed from this vague coverage-ticket body.

- `KT-391`
  - Kotlin/JVM standard-library function-arity generation background only. Kappa's function types are not encoded as a finite generated `Function0`-`Function22` class family.

- `KT-392`
  - import-name ergonomics background only. Kappa module references in import declarations are already path-derived global module references, with no Kotlin-style current-package-relative import fallback to disambiguate.

- `KT-393`
  - broad module-system implementation background. Kappa already has real fixture coverage for module descriptors / path-derived modules, default prelude import behavior, dependencies/imports, visibility, and import item selection, so this vague umbrella ticket does not need a new staged test.

- `KT-394`
  - Kotlin class-object member visibility background only; Kappa has no owning-class/class-object lookup surface matching this issue.

- `KT-395`
  - backend verification crash around infix extension-method call and nullable host call. The closest Kappa source-level precedence obligation is already staged from `KT-82`; this issue does not add a distinct source-semantics fixture.

- `KT-396`
  - function-literal class-descriptor implementation background only. Kappa closure/runtime representation is specified separately and this does not imply a source-level fixture.

- `KT-397`
  - Kotlin generic runtime type-test / missing-primary-constructor diagnostic background. Kappa does not have erased type-parameter `is T` checks or Kotlin primary-constructor lookup, so no current fixture is implied.

- `KT-398`
  - internal property-initializer / function-valued descriptor crash background only. The source sample is the same erased type-parameter runtime-test family as `KT-397`, not a current Kappa fixture.

- `KT-399`
  - direct Kappa fit for immediately applying the function value returned by a polymorphic call; staged as `new-tests/expressions.application.positive_polymorphic_returned_function_immediate_application`.

- `KT-400`
  - Java bean getter/setter property interop background only. Kappa host binding and accessor-bundle rules are explicit and do not import Kotlin's JavaBean property projection surface.

- `KT-401`
  - standalone compiler packaging / `JAVA_HOME` background only, not a language-spec fixture.

- `KT-402`
  - function-literal visitor implementation background only. The closest source-level obligation is ordinary function-literal typing, already represented by existing lambda/application fixtures.

- `KT-403`
  - broad operator-overloading feature request. Kappa already treats symbolic operator identifiers as ordinary term declarations with imported fixity, and reserved tokens such as `?.` / `?:` remain intentionally non-overloadable, so this does not add a distinct fixture.

- `KT-404`
  - inline-function restriction background only. Kappa has no Kotlin-style inline-function parameter mode; recursion, visibility, and function-value escape are governed by ordinary Kappa rules.

- `KT-405`
  - non-local `break` / `continue` through inline function literals background only. Kappa explicitly keeps `break` and `continue` target resolution lexical and disallows crossing user-written lambda or local-function boundaries, so this is intentional divergence rather than a fixture import.

- `KT-406`
  - Java external-constant nullability evidence; folded into the existing `SPEC_ADDITIONS.md` trusted-summary / raw-nullability example note rather than staged as current source behavior.

- `KT-407`
  - script-mode implementation background. Kappa already specifies script mode for module headers, URL imports, macro transcripts, debug defaults, and harness `scriptMode`; this Kotlin issue does not imply a new current fixture.

- `KT-408`
  - Kotlin inner-class-in-generic-class background only. Kappa has local type declarations and generic capture rules, but no nominal inner-class construction surface matching this issue.

- `KT-409`
  - Kotlin cast-and-bind parser feature background only. Kappa has pattern bindings and type ascription, but no `as val` surface to import as current behavior.

- `KT-410`
  - direct Kappa fit for applying postfix optional type sugar to tuple types; staged as `new-tests/definitional_equality.positive_optional_tuple_type_sugar`.

- `KT-411`
  - direct Kappa fit for labeled lambda-local return when the lambda is passed as an argument; staged as `new-tests/effects.return.runtime_positive_labeled_lambda_argument_return`.

- `KT-412`
  - Kotlin namespace/directory mismatch import background only. Kappa package-mode module headers must match path-derived module names, and script-mode differences are already specified explicitly, so this is not a current fixture import.

- `KT-413`
  - direct Kappa fit for labeled return target resolution across local-function boundaries; staged as `new-tests/effects.return.negative_labeled_return_crosses_local_function`.

- `KT-414`
  - absent from the current local Kotlin corpus snapshot.

- `KT-415`
  - Boolean library ergonomics background only. Kappa already has ordinary `Bool` values and function/operator surfaces, so this does not add a distinct current fixture or spec note.

- `KT-416`
  - redundant safe-navigation diagnostic/backend evidence; folded into the `SPEC_ADDITIONS.md` diagnostic-hook note with `KT-358`.

- `KT-417`
  - Array construction-helper ergonomics evidence; folded into the existing `SPEC_ADDITIONS.md` portable `Array` construction and convenience-member note.

- `KT-418`
  - direct but intentionally negative Kappa fit for safe-navigation receiver typing; staged as `new-tests/expressions.safe_navigation.negative_non_optional_receiver` because Kappa requires the prefix of `?.` to be `Option T`.

- `KT-419`
  - Kotlin primary-constructor parameter / property-scope bug background only. Kappa has constructor declarations and default expressions, but no class-body property initialization surface matching this issue.

- `KT-420`
  - absent from the current local Kotlin corpus snapshot.

- `KT-421`
  - Kotlin backing-field and declaration-order initialization background only. Kappa has ordinary record fields and local declaration ordering, but no `$field` backing-field surface to import.

- `KT-422`
  - vararg overload and primitive literal typing evidence; folded into the `SPEC_ADDITIONS.md` portable ABI / variadic diagnostics note rather than staged as current source behavior.

- `KT-423`
  - incomplete-code tooling robustness evidence. Kappa already says tooling queries over syntactically incomplete files should return partial results rather than fail wholesale, so this does not add a current source fixture.

- `KT-424`
  - direct Kappa fit for same-scope duplicate term declarations; staged as `new-tests/modules.names.negative_duplicate_term_declaration`.

- `KT-425`
  - multi-index get/set ergonomics evidence; folded into the existing `SPEC_ADDITIONS.md` portable `Array` construction, indexing, and convenience-member note.

- `KT-426`
  - closure caching / allocation performance background only. Kappa's source semantics do not require a particular closure-object caching strategy.

- `KT-427`
  - `String` convenience-member ergonomics evidence; tracked in `SPEC_ADDITIONS.md` as a portable prelude surface question rather than staged as current required behavior.

- `KT-428`
  - platform intrinsic cast-elimination background only. Kappa's host representations and bridge profiles are specified separately, and this does not add a source-level fixture.

- `KT-429`
  - Kotlin top-level import-placement background only. Kappa permits explicit local imports in block/do scopes and already has targeted import fixtures; this Kotlin restriction does not translate into a current Kappa negative.

- `KT-430`
  - absent from the current local Kotlin corpus snapshot.

- `KT-431`
  - adjacent Kappa fit for safe-navigation receiver typing; already represented by `new-tests/expressions.safe_navigation.negative_non_optional_receiver` because `?.` requires an `Option T` prefix, not a namespace or module receiver.

- `KT-432`
  - Unit-return function-type shorthand ergonomics evidence; folded into the existing `SPEC_ADDITIONS.md` note with `KT-339`.

- `KT-433`
  - named function expressions inside receiver lambdas are already represented by Kappa's local-declaration-in-lambda fixtures and receiver-method inference fixtures; no distinct current fixture is needed.

- `KT-434`
  - backend verification issue around `try` / nullable result lowering. This is not a distinct source-level Kappa obligation beyond the later effects/finalization/backend waves.

- `KT-435`
  - function-valued-expression diagnostic ergonomics evidence; tracked in `SPEC_ADDITIONS.md` as a diagnostic-quality rule for preserving source binder names in application errors.

- `KT-436`
  - function-valued expression support is already represented by returned-function immediate application and generic function-valued expression fixtures, especially `new-tests/expressions.application.positive_polymorphic_returned_function_immediate_application`.

- `KT-437`
  - direct Kappa fit for assignment target validity; staged as `new-tests/effects.do_blocks.negative_assignment_requires_var`.

- `KT-438`
  - first-class property support is already represented by Kappa's real projection/accessor descriptor fixtures under `tests/Kappa.Compiler.Tests/Fixtures/types.projections.*`; no new staged fixture is needed.

- `KT-439`
  - labeled lambda literal as a call argument is already represented by `new-tests/effects.return.runtime_positive_labeled_lambda_argument_return`; no separate label-only fixture is needed.

- `KT-440`
  - unlabeled `return@` shorthand is an intentional divergence. Kappa requires an explicit lambda label `L@` and explicit `return@L`, avoiding an implicit innermost-lambda target.

- `KT-441`
  - overload-plus-integer-literal inference background only. Kappa does not have Kotlin-style same-scope overload sets for ordinary terms, and the closest returned-function inference obligations are already staged.

- `KT-442`
  - receiver-lambda / `with` inference background; Kappa's receiver-marked function and dotted-call inference obligations are already represented by `new-tests/expressions.application.positive_generic_receiver_method_sugar` and `new-tests/expressions.application.positive_generic_receiver_method_infers_argument`.

- `KT-443`
  - adjacent Kappa fit for assignment target validity; already represented by `new-tests/effects.do_blocks.negative_assignment_requires_var` because Kappa assignment sugar may target only mutable `var` cells.

- `KT-444`
  - incomplete-code tooling robustness evidence. Kappa already says tooling queries over syntactically incomplete files should return partial results rather than fail wholesale, so this does not add a current source fixture.

- `KT-445`
  - erased generic runtime-test evidence; folded into the `SPEC_ADDITIONS.md` `DynamicType` / `DynRep` portability note rather than staged as a current `is` fixture, because Kappa constructor tests do not perform deep erased generic type checks.

- `KT-446`
  - String compound-assignment / conversion ergonomics evidence; folded into the portable `String` convenience-member note rather than staged as a current operator fixture.

- `KT-447`
  - erased/reified declaration-modifier design evidence; folded into the `SPEC_ADDITIONS.md` `DynamicType` / `DynRep` portability note because Kappa already separates erased type information from explicit runtime representation values.

- `KT-448`
  - definite-assignment / uninitialized local background only. Kappa local bindings require an initializer in the current grammar, so this Kotlin declaration form does not translate to a current fixture.

- `KT-449`
  - Kotlin class-object-in-inner-class background only. Kappa has local/static declarations and reified facets, but no Kotlin class object surface matching this restriction.

- `KT-450`
  - Kotlin primary-constructor modifier background only. Kappa constructor declarations have a different data-declaration surface and existing constructor-default fixtures cover the closer current obligations.

- `KT-451`
  - direct Kappa fit for invalid character literal scalar count; staged as `new-tests/literals.character_literals_char.negative_empty_or_multiple_scalars`.

- `KT-452`
  - absent from the current local Kotlin corpus snapshot.

- `KT-453`
  - parser-diagnostic ergonomics evidence; folded into the `SPEC_ADDITIONS.md` fix-it / repair-suggestion note because Kappa already specifies partial tooling queries over incomplete files but not standardized parser repair edits.

- `KT-454`
  - arbitrary-expression-label background only. Kappa labels lambdas for `return@label` and effect/do scopes for effect handling and cleanup, but does not currently have Kotlin-style labels on every expression.

- `KT-455`
  - duplicate diagnostic ergonomics evidence; folded into the `SPEC_ADDITIONS.md` cascaded-diagnostic suppression note rather than staged as a definite-assignment test, because Kappa locals require initializers.

- `KT-456`
  - Kotlin getter definite-return background only. Kappa function bodies and accessor descriptors do not have a separate empty getter-body surface that can omit a required result.

- `KT-457`
  - direct Kappa fit for receiver-marked binders as ordinary explicit parameters; staged as `new-tests/expressions.application.positive_receiver_method_explicit_call`.

- `KT-458`
  - direct Kappa fit for backtick-quoted identifiers; staged as `new-tests/lexical.identifiers.positive_backtick_weird_names`.

- `KT-459`
  - direct Kappa fit for qualified generic constructor inference; staged as `new-tests/types.generics.positive_qualified_constructor_type_inference`.

- `KT-460`
  - direct Kappa fit for pure block expressions as value-producing local scopes; staged as `new-tests/declarations.let_in.positive_pure_block_expression_value`.

- `KT-461`
  - direct Kappa fit for character literal escape validation; staged as `new-tests/literals.character_literals_char.negative_dangling_escape`.

- `KT-462`
  - Kotlin property-initialization policy background only. The closest current Kappa rule is already the staged assignment-target negative: assignment sugar may target only explicit mutable `var` cells.

- `KT-463`
  - partial tooling robustness is already a current spec obligation under §17.2.5 and is also represented by the diagnostic-ergonomics notes for `KT-453` / `KT-455`; the concrete valid character escapes are staged as `new-tests/literals.character_literals_char.positive_escape_sequences`.

- `KT-464`
  - Kotlin inner-class / class-object qualified lookup background only. Kappa has module members, data constructors as static members, and reified type-object static-member lookup, but no nested class surface matching this issue.

- `KT-465`
  - Kappa analogue is qualified type-name resolution and qualified generic constructor use; represented by `new-tests/types.generics.positive_qualified_constructor_type_inference`.

- `KT-466`
  - syntax-design background only. Kappa already uses the thin arrow `->` for lambdas, so there is no current semantic fixture to import.

- `KT-467`
  - Kotlin receiver-overload compatibility background only. Kappa's current ordinary term namespace rejects same-scope overload declarations rather than requiring overload compatibility checks over receiver types.

- `KT-468`
  - direct Kappa fit for namespace / module structure isolation; staged as `new-tests/modules.names.positive_same_names_in_distinct_modules`.

- `KT-469`
  - augmented-assignment ergonomics evidence; folded into the `SPEC_ADDITIONS.md` augmented-assignment / increment note rather than staged as current behavior, because Kappa currently keeps mutation explicit through `var`, references, `inout`, and ordinary calls.

- `KT-470`
  - duplicate diagnostic ergonomics evidence; linked to the staged `new-tests/modules.names.negative_duplicate_term_declaration` fixture, whose one-error assertion already checks the Kappa analogue, and folded into the cascaded-diagnostic suppression note.

- `KT-471`
  - user-defined increment ergonomics evidence; folded into the `SPEC_ADDITIONS.md` augmented-assignment / increment note rather than staged as current behavior.

- `KT-472`
  - multi-file namespace-level resolution background. The Kappa analogue is already represented by `new-tests/modules.files.positive_same_module_fragments`, which checks that declarations from path-related module fragments resolve together.

- `KT-473`
  - literal-format ergonomics evidence; folded into the `SPEC_ADDITIONS.md` literal-format annotation note rather than staged as current behavior, because Kappa has numeric suffixes and prefixed interpolated macros but no general annotation-based format contract for arbitrary string literals.

- `KT-474`
  - redeclaration diagnostic ergonomics evidence; folded into the cascaded-diagnostic suppression note and linked to the existing duplicate-term fixture family rather than creating another duplicate.

- `KT-475`
  - direct Kappa fit for receiver-projection sugar as an extension-property analogue; staged as `new-tests/types.projections.positive_receiver_projection_sugar`.

- `KT-476`
  - direct Kappa fit for loop `else`; staged as `new-tests/effects.loops.positive_loop_else_typechecks`.

- `KT-477`
  - absent from the current local corpus snapshot.

- `KT-478`
  - direct same-module / namespace-fragment fit; folded into `new-tests/modules.files.positive_same_module_fragments` with updated provenance rather than adding a duplicate fixture.

- `KT-479`
  - vararg overload specificity background only. Kappa has fixed-arity curried functions and no same-scope ordinary term overload set, so this does not become a current source-level fixture.

- `KT-480`
  - string-template rendering ergonomics evidence; folded into the portable `String` convenience / rendering note in `SPEC_ADDITIONS.md` rather than staged as current behavior.

- `KT-481`
  - identity-equality spec-clarity evidence; folded into `SPEC_ADDITIONS.md` as a question about whether Kappa should expose any general reference / object identity equality primitive.

- `KT-482`
  - direct current-spec point, but already covered by `tests/Kappa.Compiler.Tests/Fixtures/lexical.comments.positive`, whose block comment contains a nested block comment.

- `KT-483`
  - direct Kappa fit for numeric-token boundary handling; staged as `new-tests/literals.numeric_literals.negative_trailing_dot_float`.

- `KT-484`
  - JVM / Java interop object-codegen background only. Kappa backend profiles may generate companion artifacts, but source-level module and static-object semantics do not require a Java-callable singleton class naming fixture.

- `KT-485`
  - Kotlin nominal declaration-conflict background. Kappa explicitly permits distinct declaration kinds with one spelling and handles term-expression admissibility through binding-group lookup; same-scope duplicate ordinary terms are already covered separately.

- `KT-486`
  - extension-property backing-field policy background only. Kappa computed projections and accessor bundles have explicit `get`, `set`, and `sink` clauses, not Kotlin-style backing-field initialization.

- `KT-487`
  - computed-projection shorthand ergonomics evidence; folded into `SPEC_ADDITIONS.md` rather than staged as current behavior because Kappa currently requires explicit `get ->` for expanded-form read-only projections.

- `KT-488`
  - absent from the current local corpus snapshot.

- `KT-489`
  - portable range-surface evidence; folded into the `SPEC_ADDITIONS.md` range note rather than staged as current behavior, because Kappa specifies `Rangeable` conceptually but does not yet pin down scalar-specific range representation families.

- `KT-490`
  - Kotlin class-object constraint / JVM runtime metadata background only. Kappa has reified static objects, traits, and explicit `Rangeable` evidence, but no Kotlin class-object constraint surface or matching JVM metadata parser behavior.

- `KT-491`
  - Kotlin trait-extends-class / diamond-inheritance syntax background only. Kappa trait constraints and supertrait projection are dictionary-based, not nominal class inheritance.

- `KT-492`
  - portable range endpoint evidence; folded into the `SPEC_ADDITIONS.md` range note because Kappa should clarify that any standard inclusive scalar range iteration terminates correctly at maximum endpoints without relying on overflowing successor arithmetic.

- `KT-493`
  - local immutable assignment background; folded into the existing assignment-target fixture row because Kappa already requires assignment sugar to target explicit `var` locals.

- `KT-494`
  - direct Kappa fit for signed numeric-expression target typing; staged as `new-tests/literals.numeric_literals.positive_signed_expected_type_and_comparison`.
  - the unary `+` half is tracked as a spec-addition question rather than a current test because v0.1 only specifies prefix `-`.

- `KT-495`
  - direct Kappa fit for expected-type propagation into integer literals in equality / inequality operands; staged with `KT-494` as `new-tests/literals.numeric_literals.positive_signed_expected_type_and_comparison`.

- `KT-496`
  - direct Kappa fit for `try` / `finally` finalizer typing and backend-lowering robustness; staged as `new-tests/errors.try_except_finally.positive_finally_typechecks` and `new-tests/errors.try_except_finally.negative_finally_must_be_unit`.

- `KT-497`
  - adjacent top-level initializer / `try` background only. Kappa's `try` is monadic and already covered by the new `KT-496` finalizer fixtures for the transferable obligation; the Kotlin namespace/class-inheritance details do not produce a separate current fixture.

- `KT-498`
  - Kotlin/JVM class, extension-function, and unchecked-cast background only. Kappa's gradual and host-bridge casts are explicit and do not import Kotlin's `T : Any` / `Object.getClass` surface.

- `KT-499`
  - absent from the current local corpus snapshot.

- `KT-500`
  - direct Kappa fit for `try` / `except` result semantics; staged as `new-tests/errors.try_except.positive_except_result_typechecks`.

- `KT-501`
  - recursive constructor / JVM verifier background only. Kappa data constructors are introduction forms, and recursive value initialization should be handled through ordinary recursion / termination / effect rules rather than Kotlin class initializer bytecode.

- `KT-502`
  - vararg overload-specificity background only. Kappa has fixed-arity curried functions and no Kotlin-style overload ranking between primitive varargs; existing ABI/vararg notes already track the portable boundary concern.

- `KT-503`
  - short-circuit codegen background; the transferable Kappa obligation is already represented by `tests/Kappa.Compiler.Tests/Fixtures/core_semantics.evaluation.runtime_positive/short_circuit_and.kp` and the staged effects-handler short-circuit root.

- `KT-504`
  - Kotlin class-object setter / private-property backend background only. Kappa has explicit projection/accessor descriptors and no Kotlin companion-object setter generation surface.

- `KT-505`
  - structural delegation ergonomics evidence; folded into `SPEC_ADDITIONS.md` as a possible forwarding helper for signature-shaped APIs rather than staged as current behavior.

- `KT-506`
  - absent from the current local corpus snapshot.

- `KT-507`
  - direct Kappa fit for qualified generic constructor parsing when a type argument is a function type; folded into the existing staged `new-tests/types.generics.positive_qualified_constructor_type_inference` root.

- `KT-508`
  - map indexing plus function-typed values background only. The function-type parsing part is already represented by `KT-507`; Kappa's portable `Map` indexing/update surface is not specified enough for a current fixture.

- `KT-509`
  - constructor-parameter accessor metadata background. Kappa constructor named-parameter metadata and projection/accessor descriptor behavior is already covered by existing constructor/default/projection fixtures; Kotlin property-descriptor internals do not add a distinct fixture.

- `KT-510`
  - Kotlin class backing-field initialization background only. Kappa has no class-body field-initialization surface or assignable `this.field` analogue for this issue.

- `KT-511`
  - generic class inheritance / runtime type-metadata background only. Kappa's runtime type evidence and gradual/bridge metadata surfaces are explicit and do not import Kotlin class-supertype `TypeInfo` parsing behavior.

- `KT-512`
  - user-defined `plusAssign : Unit` ergonomics evidence; folded into the existing augmented-assignment / increment note in `SPEC_ADDITIONS.md` rather than staged as current behavior.

- `KT-513`
  - Kotlin/JVM verify-error background around infix collection mutation inside a range loop. Kappa's closest current obligations are already covered by iterator/loop fixtures and explicit mutation surfaces; this does not add a new portable source rule.

- `KT-514`
  - Java-to-Kotlin resolve / anonymous-object codegen background only. Kappa has local nominal declarations and object identity rules, but no Kotlin object-literal classfile naming surface matching this issue.

- `KT-515`
  - runtime class-token evidence; folded into the `SPEC_ADDITIONS.md` `DynamicType` / `DynRep` portability note because Kappa should clarify when explicit runtime representations may expose host class tokens.

- `KT-516`
  - direct Kappa fit for nested `elif` / `else if` classification; staged as `new-tests/expressions.conditionals.positive_nested_elif_typechecks`.

- `KT-517`
  - direct Kappa fit for `try` / `finally` result typing; already represented by the `KT-496` finalizer tests because Kappa explicitly rejects using the finalizer's last expression as the `try` result.

- `KT-518`
  - adjacent safe-navigation plus constructor-refinement backend background. Kappa's current safe-navigation and branch-refinement obligations are already represented by the safe-navigation wrap/flatten and constructor-test projection fixtures.

- `KT-519`
  - absent from the current local corpus snapshot.

- `KT-520`
  - backend line-number / classfile metadata background only. Kappa diagnostic source-map and target-lowering metadata are important, but this Kotlin classfile `LineNumberTable` bug does not imply a current source-level fixture.

- `KT-521`
  - direct Kappa divergence on `Unit` coercion; already represented by `new-tests/expressions.lambdas.negative_expected_unit_does_not_discard_result`, because Kappa expected-`Unit` contexts do not silently discard non-`Unit` final expressions.

- `KT-522`
  - absent from the current local corpus snapshot.

- `KT-523`
  - receiver-specific computed-projection ergonomics evidence; folded into the computed-projection getter shorthand note in `SPEC_ADDITIONS.md` because Kappa does not currently have Kotlin-style extension-property overload sets by receiver type.

- `KT-524`
  - nullable assertion / unwrap-or-throw background only. Kappa models optionality through `Option` and explicit pattern matching / monadic error surfaces, not Kotlin's `sure()` / `!!` style assertion returning a raw non-null payload.

- `KT-525`
  - test-root / private-access ergonomics evidence; folded into `SPEC_ADDITIONS.md` as a possible package-mode test visibility/profile model rather than a current source fixture.

- `KT-526`
  - Kotlin class-object nested static class background only. Kappa has data constructors as static members and reified type/static objects, but no Kotlin nested class-object lookup surface matching `Foo.Bar`.

- `KT-527`
  - loop-binder shadowing policy background only. Kappa `for` bodies introduce fresh iteration binders scoped to the loop, and existing shadowing fixtures cover lexical local shadowing; the issue's "should this conflict?" question does not imply a new current negative.

- `KT-528`
  - Kotlin/JVM codegen background around local methods, callbacks, Java collection iteration, and source-level collection mutation. Kappa's transferable obligations are already covered by receiver-method, iterator, callback, and loop fixtures.

- `KT-529`
  - Kotlin/JVM verify-error background around higher-order iterator helpers, mutable locals, and Java collection receivers. Kappa's current source-level obligations are already represented by `for` / `Iterator`, function-typed callbacks, and explicit mutation fixtures.

- `KT-530`
  - direct Kappa fit for explicit no-op branch bodies; staged as `new-tests/expressions.match.positive_unit_noop_branch`. Kappa does not import Kotlin's empty-branch syntax; it uses explicit `()`.

- `KT-531`
  - optional-iterator ergonomics evidence; folded into `SPEC_ADDITIONS.md` as a question about whether `Option src` should act like an empty iteration source.

- `KT-532`
  - absent from the current local corpus snapshot.

- `KT-533`
  - Kotlin/JVM classfile generation background around object-literal iterators, callbacks, mutable locals, and Java collections. Kappa's relevant source obligations are already covered by iterator, callback, and loop fixtures.

- `KT-534`
  - Kotlin parser ambiguity around `C <*> = ...` versus `>=` tokenization. Kappa has no Kotlin wildcard generic syntax, and comparison-token handling is already ordinary operator/fixity surface rather than a direct fixture import.

- `KT-535`
  - generic class-object initializer backend background only. Kappa has generic constructors and static members, but no Kotlin companion/class-object initialization surface matching this verifier issue.

- `KT-536`
  - build-tool integration background only. Ant support is outside Kappa source semantics and current fixture planning.

- `KT-537`
  - direct Kappa fit for built-in map literals; staged as `new-tests/collections.literals.positive_map_literal`.

- `KT-538`
  - direct Kappa fit for optional tuple values and postfix optional type sugar on tuple types; already represented by `new-tests/definitional_equality.positive_optional_tuple_type_sugar`.

- `KT-539`, `KT-540`, `KT-541`
  - absent from the current local corpus snapshot.

- `KT-542`
  - diagnostic-test infrastructure background only. Kappa already has Appendix T harness directives and local fixture planning; embedding the Kotlin compiler as a library has no direct Kappa source-semantic analogue.

- `KT-543`, `KT-544`, `KT-545`, `KT-546`, `KT-547`
  - absent from the current local corpus snapshot.

- `KT-548`
  - absent from the current local corpus snapshot.

- `KT-549`
  - direct Kappa fit for contextual typing of bare lambda binders inside higher-order generic calls; staged as `new-tests/expressions.lambdas.positive_generic_contextual_lambda_inference`.

- `KT-550`
  - projection result-type inference ergonomics evidence; folded into `SPEC_ADDITIONS.md` because Kappa v0.1 explicitly requires every projection definition to carry an inline result type.

- `KT-551`
  - compiler-internal overload / resolve-result crash background only. Kappa has no JVM PSI analogue, and the source text does not imply a portable source-level rule beyond ordinary resolver robustness.

- `KT-552`
  - single-expression loop body ergonomics evidence; folded into `SPEC_ADDITIONS.md` rather than staged because current Kappa loop syntax is explicitly `while ... do` / `for ... do` with block-suite bodies.

- `KT-553`
  - Kotlin/JVM object-literal override diagnostic crash background around byte-array iterators and extension properties. Kappa's portable source obligations are already covered by iterator protocol, receiver projection, and loop fixtures.

- `KT-554`
  - object-literal closure-analysis background only. Kappa closure capture and local nominal declarations matter, but Kotlin's object-constructor closure-shape issue does not map to a current source fixture.

- `KT-555`
  - generic invocation parser background only. Kappa does not use Kotlin's angle-bracket function type-argument syntax or star-projection syntax, and optional type sugar is already covered separately.

- `KT-556`
  - augmented-assignment / `plusAssign` ergonomics evidence; folded into the existing `SPEC_ADDITIONS.md` augmented-assignment note rather than staged as current behavior.

- `KT-557`
  - nullable assertion plus redundant safe-call diagnostic evidence; folded into `SPEC_ADDITIONS.md`'s redundant safe-navigation diagnostic note and kept out of current tests because Kappa has no Kotlin `sure()` / `!!` assertion surface.

- `KT-558`, `KT-561`
  - absent from the current local corpus snapshot.

- `KT-559`
  - Kotlin OO inheritance background only. Kappa has no class-subclass `super` call surface or abstract method override table matching this issue; trait and dictionary obligations are covered by the solver/coherence wave instead.

- `KT-560`
  - JVM verify-error background around `while`, mutable locals, and increment lowering. Kappa's portable source obligations for `while`, `var`, and explicit mutation are already represented in loop/mutation fixtures, while increment sugar remains only a future ergonomics note.

- `KT-562`
  - Kotlin parser/lowering ambiguity around treating a standalone brace block as a function literal. Kappa has explicit `block` expressions and explicit `\` lambdas, so this is useful confirmation of the current design but does not imply a new fixture or spec-addition note.

- `KT-563`, `KT-564`, `KT-565`, `KT-567`
  - absent from the current local corpus snapshot.

- `KT-566`
  - direct Kappa fit for for-iteration over an iterator value; already represented by `new-tests/effects.for_loops.positive_iterator_protocol`, whose source trace now includes this issue.

- `KT-568`
  - incomplete object-literal implementation diagnostic background only. Kappa has no object-literal inheritance surface; trait dictionary and instance completeness belong to the solver/coherence wave.

- `KT-569`
  - failed type-inference diagnostic background around an incomplete iterator implementation and higher-order callback. Kappa's semantic obligations are already covered by contextual lambda inference and iterator fixtures; no new current fixture is implied by the Kotlin diagnostic quality issue.

- `KT-570`
  - direct but already-covered import-alias fit. Kappa's `import M as A` rule preserves module identity while binding only the alias for qualified access, and existing real fixtures cover alias qualification.

- `KT-571`
  - direct Kappa fit for contextual typing of a lambda argument in a generic helper call; already represented by `new-tests/expressions.lambdas.positive_generic_contextual_lambda_inference`, whose source trace now includes this issue.

- `KT-572`
  - function-type and lambda-syntax ergonomics evidence; Kappa already uses `->` for function types and lambdas, while the Unit-return callback shorthand subcase is folded into `SPEC_ADDITIONS.md`.

- `KT-573`
  - incomplete-code IDE robustness background only. The sample uses backtick package segments and an unfinished class header, but the actual issue is an assertion during editor analysis rather than a portable Kappa source rule.

- `KT-574`
  - identity-equality ergonomics / standard-library surface evidence; folded into the existing `SPEC_ADDITIONS.md` note about whether Kappa should expose a source-visible `identityEquals`-style primitive.

- `KT-575`
  - increment / class-object mutation ergonomics evidence; folded into the augmented-assignment / increment note in `SPEC_ADDITIONS.md` rather than staged as current behavior.

- `KT-576`
  - loop-body syntax and diagnostic ergonomics evidence; folded into the single-expression loop-body note in `SPEC_ADDITIONS.md` because Kappa currently requires explicit `do`-suite loop bodies.

- `KT-577`
  - direct Kappa fit for rejecting Elvis on a non-optional left operand; staged as `new-tests/expressions.elvis.negative_non_optional_left`.

- `KT-578`
  - hash-based collection constraint evidence; folded into `SPEC_ADDITIONS.md` as a question about whether Kappa should expose a portable `Hash` / `Hashable` trait for hash-oriented collection APIs, while keeping built-in `Set` / `Map` semantics specified by `Eq`.

- `KT-579`
  - array literal / vararg diagnostic background. It reinforces the existing portable `Array` construction and convenience-member note, but Kappa does not currently have Kotlin-style array literal or vararg source syntax to stage.

- `KT-580`
  - array/range/accessor inference evidence; folded into the existing portable `Array` and range-surface notes rather than staged as current behavior because Kappa has not yet pinned down `indices`, `lastIndex`, or scalar range constructors.

- `KT-581`
  - array extension / backend verify-error background; folded into the portable `Array` construction and convenience-member note as additional evidence, not a current source-level fixture.

- `KT-582`
  - absent from the current local corpus snapshot.

- `KT-93`
  - poor fit: nullable arrays, initialization discipline, and collection element nullability are collection-surface design issues rather than a clean Kappa Chapter 7 import.

- `KT-95`
  - not a fixture import: architectural/tooling discussion only.

- `KT-97`
  - poor fit: where-clause syntax for generic constraints is a syntax-design issue; Kappa already expresses constraints through explicit binders and constraint arrows rather than Kotlin-style where-clause placement.

## Poor Fits / Intentional Divergences

| source | why it is not a clean Kappa import |
| --- | --- |
| `KT-969` | Kotlin's complaint is about mutable-local smart casts and diagnostics. Kappa intentionally keeps stable-alias refinement syntax-directed and excludes `var` locals from that rule. |
| `KT-4294` | This is mostly a diagnostic / UX issue around generic calls and smart-cast reporting. Kappa has no corresponding smart-cast warning surface to import. |
| `KT-2164` | Kappa has no Kotlin-style `!!` operator, and the requested post-`!!` refinement is a different ergonomic surface from anything currently specified. |
| `KT-2176` | Same divergence as `KT-2164`, plus the `as` variant asks for statement-like refinement after a cast expression; Kappa does not currently specify that style of refinement. |
| `KT-2193` | This is mostly about Kotlin's OO trait/class dispatch surface. Kappa's trait members elaborate through dictionaries instead, and the current trait-member fixtures already cover the more direct Kappa obligations. |
| `KT-2212` | Another null-check-plus-`!!` flow-analysis request. Kappa's current story is constructor refinement and stable aliases, not post-assertion mutation of local types. |
| `KT-3175` | Same divergence as `KT-969`: Kotlin wants smart casts on mutable locals under a no-write analysis, while Kappa's `§7.4.3` deliberately does not. |
| `KT-4751` | The issue is about a mutable object property and receiver stability, not an immutable local alias. Kappa intentionally limits stable-alias flow facts more tightly than Kotlin's property smart casts. |
| `KT-5228` | Kotlin's `when` null-check sequencing does not translate directly because Kappa's current constructor-projection rule is success-side focused; a faithful analogue would need stronger negative-constructor propagation than the current spec states explicitly. |
| `KT-6313` | This is primarily about `finally` and control-flow analysis interactions. It belongs more to the later control/finalization wave than to the current nullability and flow-typing tranche. |
| `KT-1275` | Kotlin's inferred-type-after-null-check issue depends on null-check smart casts over local variables. Kappa's closest analogue would be a future negative-constructor refinement story rather than a direct current fixture. |
| `KT-6242` | Same family as `KT-1275`, with generic inference layered on top; it does not survive translation cleanly into Kappa's current `Option`/constructor-test rules. |
| `KT-6470` | This is another mutable-local / `!!` flow-analysis request. Kappa intentionally avoids that style of refinement. |
| `KT-7290` | This is a local-expression variant of the safe-navigation-implies-present-receiver family. It is useful background for the ergonomic note, but not a current direct import. |
| `KT-8135` | This is a JVM/backend null-check generation bug around `CHECKCAST`, not a source-level Kappa language-semantic issue. |

## Current Coverage Map

- `tests/Kappa.Compiler.Tests/Fixtures/expressions.safe_navigation.positive_wrap`
  - sources: `KT-247`, `KT-2143`

- `tests/Kappa.Compiler.Tests/Fixtures/expressions.safe_navigation.positive_flatten`
  - source: `KT-105`

- `tests/Kappa.Compiler.Tests/Fixtures/expressions.safe_navigation.negative_ambiguous_generic`
  - source family: Kotlin safe-call inference pressure, but this is a Kappa-specific v0.1 hold rather than a direct Kotlin import

- `new-tests/expressions.safe_navigation.negative_non_optional_receiver`
  - sources: `KT-418`, `KT-431`; safe-navigation requires an `Option T` receiver prefix and must not silently degrade to ordinary dotted projection or namespace/member selection

- `tests/Kappa.Compiler.Tests/Fixtures/expressions.elvis.positive_default`
  - source family: basic Elvis behavior; still useful background for `KT-1154` / `KT-5335`

- `new-tests/expressions.elvis.negative_non_optional_left`
  - source: `KT-577`; `?:` requires a left operand of type `Option T` and must reject a plain value left operand

- `tests/Kappa.Compiler.Tests/Fixtures/expressions.conditionals.positive_constructor_test_projection`
  - source family: Kotlin smart-cast / tag-test projection; adjacent source `KT-225`

- `tests/Kappa.Compiler.Tests/Fixtures/expressions.match.runtime_negative_non_exhaustive`
  - adjacent sources: `KT-233`, `KT-234`; when exhaustiveness cannot be proven, a missing catch-all remains a rejected match

- `tests/Kappa.Compiler.Tests/Fixtures/expressions.application.positive/function_value_application`
  - adjacent source: `KT-136`; ordinary invocation of a function-valued parameter is already covered, so the Kotlin issue is tracked as front-end regression background rather than as a new Kappa gap

- `tests/Kappa.Compiler.Tests/Fixtures/modules.imports.negative_missing_item`
  - adjacent source: `KT-57` (already covered; tracked here so the early Kotlin import-resolution read is not lost even though it sits outside the main Chapter 7 wave)

- `tests/Kappa.Compiler.Tests/Fixtures/modules.imports.positive/main_alias.kp`
  - source: `KT-570`; module import aliases should preserve the imported module identity while binding only the alias for qualified access

- `new-tests/lexical.operator_identifiers_fixity.runtime_positive_pipe_preserves_dotted_forms`
  - adjacent source: `KT-82`; the pipe operator must leave the dotted right-hand side intact, so `(makeOps 22).adder` is formed before the pipe applies its left operand

- `new-tests/expressions.conditionals.negative_disjunction_projection_type_mismatch`
  - source: `KT-104`; a projection under `||` is invalid when the successful alternatives induce different types for the projected field

- `new-tests/expressions.application.positive_generic_lambda_result_inference`
  - source: `KT-109`; a polymorphic higher-order application should infer its result type from a lambda argument without explicit type application

- `new-tests/expressions.lambdas.positive_generic_contextual_lambda_inference`
  - sources: `KT-549`, `KT-571`; a higher-order generic function call should instantiate its type parameter from a value argument and contextually type bare lambda binders from that instantiation

- `new-tests/expressions.application.positive_polymorphic_result_from_expected_type`
  - source: `KT-353`; expected types from declaration signatures, lambda results, and branch joins should instantiate result-only polymorphic calls

- `new-tests/expressions.application.positive_polymorphic_returned_function_immediate_application`
  - source: `KT-399`; a function value returned by a polymorphic call should remain directly callable at the same application site and after rebinding

- `new-tests/expressions.application.positive_function_declaration_rebinds_as_value`
  - source: `KT-263`; a named function declaration should resolve as an ordinary first-class function value when rebound through a typed local

- `new-tests/declarations.let_in.positive_local_function_inside_lambda`
  - sources: `KT-328`, `KT-329`; lambda body suite sugar should admit local signatures and named local function definitions with ordinary lexical capture

- `new-tests/expressions.application.positive_named_function_application`
  - sources: `KT-202`, `KT-214`; ordinary function values with named explicit binders should support out-of-order and punned named application under Spec §7.1.3C

- `new-tests/expressions.application.negative_named_function_application_labels`
  - sources: `KT-202`, `KT-214`; named ordinary-function application must reject missing, extra, and duplicate labels under Spec §7.1.3C

- `new-tests/expressions.lambdas.positive_unit_binder_is_single_argument`
  - source: `KT-221`; Kappa's `()` lambda/function binder is one explicit `Unit` argument, not a degenerate binderless function

- `new-tests/expressions.lambdas.negative_expected_unit_does_not_discard_result`
  - source: `KT-343`; assigning a lambda to `Unit -> Unit` must not silently discard a non-`Unit` final expression

- `new-tests/effects.loops.negative_loop_not_pure_expression`
  - source: `KT-351`; loop forms are do-block statements and must not be accepted as pure block items or pure expressions

- `new-tests/declarations.let_in.negative_local_value_signature_checked`
  - source: `KT-352`; a local function-value definition must be checked against its local signature inside an enclosing function body

- `new-tests/types.unit.positive_unit_as_generic_argument`
  - source: `KT-237`; `Unit` must remain an ordinary type argument and value type, not a special non-value result marker

- `new-tests/types.generics.runtime_positive_nested_generic_payload_projection`
  - source: `KT-257`; nested generic constructor payloads should preserve their runtime value and projected payload type through branch-local constructor refinement

- `new-tests/types.generics.negative_optional_generic_return_not_plain`
  - source: `KT-312`; a generic optional return must remain `Option (Box Int)` and must not be accepted where a plain `Box Int` is expected

- `new-tests/types.generics.positive_optional_generic_unwrap_with_fallback`
  - sources: `KT-313`, `KT-314`; matching on `Option a` should preserve the instantiated payload type in both present and fallback branches

- `new-tests/definitional_equality.positive_optional_function_type_sugar`
  - source: `KT-332`; optional type sugar around function types should respect parentheses and postfix precedence

- `new-tests/definitional_equality.positive_optional_tuple_type_sugar`
  - sources: `KT-410`, `KT-538`; postfix optional type sugar should apply to tuple types exactly as to any other type expression, and tuple payloads can be introduced through `Some`

- `new-tests/types.generics.positive_constructor_type_arguments_inferred`
  - source: `KT-287`; generic constructor type arguments should be inferred from the expected type and supplied constructor arguments without explicit type application

- `new-tests/data_types.data_declarations.negative_type_object_is_not_constructor`
  - source: `KT-291`; a data type object's reified term facet is a compile-time type constructor, not a runtime value constructor

- `new-tests/data_types.data_declarations.positive_empty_data_type`
  - source: `KT-292`; constructorless data declarations are valid source declarations

- `new-tests/data_types.constructor_defaults.positive_named_defaults`
  - sources: `KT-265`, `KT-372`; named constructor application should insert checked non-constant defaults and preserve default metadata through a local constructor binding

- `new-tests/data_types.constructor_defaults.negative_default_expression_checked`
  - source: `KT-264`; constructor default expressions must be checked in the declaration context and rejected for unresolved names or type mismatches

- `new-tests/data_types.constructor_defaults.negative_default_dependency_order`
  - source: `KT-371`; constructor default expressions must not refer to the parameter being defaulted or later constructor parameters

- `new-tests/effects.return.runtime_positive_labeled_lambda_rebound_as_value`
  - source: `KT-267`; `return@label` should target a labeled lambda even when that lambda is rebound as an ordinary value

- `new-tests/expressions.conditionals.negative_constructor_test_rhs_must_be_constructor`
  - source: `KT-272`; `is` accepts a constructor name on the right-hand side, not a type name or nullable-type-like expression

- `new-tests/traits.eq.negative_nullable_equality_requires_same_type`
  - sources: `KT-242`, `KT-246`; `Eq`-based `==` requires both operands to have one shared type, so `Option Int == Int` and `Option C == Option D` are rejected

- `new-tests/expressions.conditionals.positive_refinement_applies_in_let_initializer`
  - source: `KT-244`; constructor-refinement facts from a branch condition must be available while checking local initializers inside that branch

- `new-tests/expressions.conditionals.positive_refinement_visible_in_local_function`
  - source: `KT-338`; constructor-refinement facts should also be available while checking a nested local function declaration in the success branch

- `new-tests/expressions.conditionals.positive_pattern_condition_refines_scrutinee`
  - source: `KT-368`; constructor-test and pattern-condition evidence should compose in either condition-clause order

- `new-tests/expressions.application.positive_generic_receiver_method_sugar`
  - sources: `KT-125`, `KT-442`; a generic receiver-marked function should elaborate correctly through dotted method-call sugar

- `new-tests/expressions.application.positive_generic_receiver_method_infers_argument`
  - sources: `KT-258`, `KT-262`, `KT-442`; a generic receiver-marked function should infer its container type from the receiver and apply that same type to later explicit arguments

- `new-tests/effects.return.runtime_positive_labeled_lambda_local_return`
  - source: `KT-139`; a labeled lambda should admit a local `return@label` that exits that lambda rather than forcing an outer-function return

- `new-tests/effects.return.runtime_positive_labeled_lambda_argument_return`
  - sources: `KT-411`, `KT-439`; a labeled lambda passed as an argument should type `return@label` against the lambda result type

- `new-tests/effects.return.negative_labeled_return_crosses_local_function`
  - source: `KT-413`; labeled return target resolution must not cross a user-written local-function boundary

- `new-tests/effects.return.negative_bare_return_crosses_anonymous_lambda`
  - source: `KT-144`; bare `return` must not silently jump across a user-written anonymous lambda boundary

- `new-tests/expressions.conditionals.positive_conjunction_constructor_projection`
  - source: `KT-146`; the right operand of `&&` should be checked under the success facts of the left operand, so constructor-field projection is valid there

- `new-tests/expressions.patterns.positive_or_pattern_shared_binders`
  - adjacent source: `KT-89`; multiple successful alternatives in one branch should preserve the shared binder/refinement environment

- `new-tests/expressions.elvis.positive_expression_context`
  - source: `KT-1154`, with the same result-typing consequence relevant to `KT-5335`

- `new-tests/expressions.conditionals.positive_disjunction_constructor_projection`
  - source: `KT-1982`

- `new-tests/effects.do_blocks.positive_postdominating_constructor_refinement`
  - source: `KT-3899`

- `new-tests/effects.do_blocks.negative_assignment_requires_var`
  - sources: `KT-437`, `KT-443`; do-block assignment sugar must reject immutable locals, read-only property analogues, and other non-`var` targets

- `new-tests/literals.character_literals_char.negative_empty_or_multiple_scalars`
  - source: `KT-451`; a character literal must contain exactly one Unicode scalar value

- `new-tests/expressions.application.positive_receiver_method_explicit_call`
  - source: `KT-457`; a receiver-marked function should be callable both by ordinary explicit receiver argument and by dotted method-call sugar

- `new-tests/lexical.identifiers.positive_backtick_weird_names`
  - source: `KT-458`; backtick-quoted identifiers should support reserved words and weird symbolic spellings as ordinary names

- `new-tests/types.generics.positive_qualified_constructor_type_inference`
  - sources: `KT-459`, `KT-465`; module qualification of a generic type and constructor should preserve type-name resolution plus expected-type and argument-based inference

- `new-tests/declarations.let_in.positive_pure_block_expression_value`
  - source: `KT-460`; `block` should provide a value-producing local scope without needing closure allocation or invocation

- `new-tests/literals.character_literals_char.negative_dangling_escape`
  - source: `KT-461`; a dangling escape must not form a valid `Char` literal

- `new-tests/literals.character_literals_char.positive_escape_sequences`
  - source: `KT-463`; required `Char` escape sequences such as newline, tab, carriage return, backslash, quote, and Unicode escapes should typecheck as `Char`

- `new-tests/literals.numeric_literals.negative_trailing_dot_float`
  - source: `KT-483`; a trailing dot must not be accepted as part of a completed floating-point literal

- `new-tests/effects.for_loops.positive_iterator_protocol`
  - sources: `KT-341`, `KT-566`; `for x in xs do` should accept an iterator state that implements the standard `Iterator` protocol

- `new-tests/expressions.conditionals.positive_stable_alias_transport`
  - source: `KT-175`; testing a simple local alias must refine the underlying representative too, so both spellings project consistently in the success branch

- `new-tests/modules.imports.negative_duplicate_alias`
  - source: `KT-195`; two imports in one scope must not introduce the same alias spelling as distinct module declarations

- `new-tests/modules.names.negative_duplicate_term_declaration`
  - sources: `KT-424`, `KT-470`; adjacent diagnostic source: `KT-474`; same-scope ordinary term declarations with the same spelling must be rejected once rather than treated as overloads or reported as duplicated cascades

- `new-tests/modules.names.positive_same_names_in_distinct_modules`
  - source: `KT-468`; same-spelling declarations in distinct modules should not conflict when selected through their module receivers

- `new-tests/modules.files.positive_same_module_fragments`
  - sources: `KT-381`, `KT-382`, `KT-478`; adjacent source: `KT-472`; files with the same path-derived module name but different dotted fragment suffixes should contribute to one module, so base fragments can use ordinary definitions and `expect` implementations from companion fragments

- `new-tests/types.projections.positive_receiver_projection_sugar`
  - source: `KT-475`; receiver projection sugar should typecheck as a first-class Kappa analogue of extension-property-style access

- `new-tests/effects.loops.positive_loop_else_typechecks`
  - source: `KT-476`; `while ... do ... else do ...` should typecheck in a do block and follow the current loop-else spec shape

- `new-tests/declarations.let_in.positive_pattern_bound_local_capture`
  - adjacent source: `KT-158`; ordinary local `let` destructuring is already staged via an irrefutable record pattern that binds names for later local declarations

- `tests/Kappa.Compiler.Tests/Fixtures/declarations.totality.positive_mutual_recursion_hidden_phase`
  - adjacent source: `KT-169`; Kappa’s accepted mutual-recursion shape is already covered when the group has explicit signatures, so the Kotlin issue is tracked as inference/background rather than as a new gap

- `tests/Kappa.Compiler.Tests/Fixtures/literals.*`
  - adjacent source: `KT-390`; the Kotlin issue is a coverage reminder rather than a concrete semantic bug, and Kappa already has real fixtures for numeric, string, interpolated string, and character literal families

- `tests/Kappa.Compiler.Tests/Fixtures/modules.*`
  - adjacent source: `KT-393`; the Kotlin issue is a broad umbrella for modules, default imports, dependencies, and visibility, all of which are already represented by targeted Kappa module/import/prelude/visibility fixture families

- `Spec.md` script-mode rules and Appendix T `scriptMode`
  - adjacent source: `KT-407`; Kappa already has script-mode semantics, so this source issue is tracked as implementation background rather than a missing spec point

- `tests/Kappa.Compiler.Tests/Fixtures/lexical.comments.positive`
  - source: `KT-482`; nested block comments are already covered in the real fixture suite under Spec §3.3

- `tests/Kappa.Compiler.Tests/Fixtures/core_semantics.evaluation.runtime_positive/short_circuit_and.kp`
  - source: `KT-503`; `&&` must preserve short-circuit evaluation so the right operand is not evaluated when the left operand is false.

- `new-tests/types.generics.positive_qualified_constructor_type_inference`
  - sources: `KT-459`, `KT-465`, `KT-507`; qualified generic constructor calls should still parse and infer when the instantiated payload type includes a function type.

- `new-tests/literals.numeric_literals.positive_signed_expected_type_and_comparison`
  - sources: `KT-494`, `KT-495`; signed integer expressions and equality / inequality operands should preserve the demanded numeric type rather than forcing premature defaulting.

- `new-tests/errors.try_except_finally.positive_finally_typechecks`
  - sources: `KT-496`, `KT-517`; a `finally` action of type `m Unit` typechecks around a primary `try` computation without changing the primary result type.

- `new-tests/errors.try_except_finally.negative_finally_must_be_unit`
  - sources: `KT-496`, `KT-517`; a non-`Unit` `finally` action is rejected instead of being treated as the result of the `try`.

- `new-tests/errors.try_except.positive_except_result_typechecks`
  - source: `KT-500`; a matching `except` handler supplies the result type and value of the whole `try` expression.

- `new-tests/expressions.conditionals.positive_nested_elif_typechecks`
  - source: `KT-516`; `elif` chains desugar to nested `else if` conditionals and preserve a single branch result type.

- `new-tests/expressions.lambdas.negative_expected_unit_does_not_discard_result`
  - sources: `KT-343`, `KT-521`; expected `Unit` does not make a lambda discard a non-`Unit` final expression.

- `new-tests/expressions.match.positive_unit_noop_branch`
  - source: `KT-530`; no-op match branches are written explicitly as `()` and all branches still share the same `Unit` result type.

- `new-tests/collections.literals.positive_map_literal`
  - source: `KT-537`; built-in map literals use `{ key : value, ... }`, and empty `{}` is a map literal checked from context.

## Next Focused Pass

- `KT-583`
- `KT-584`
- `KT-585`
- `KT-586`
- `KT-587`

These are the next exact IDs to read if Wave 5 resumes immediately after the current staged batch.
