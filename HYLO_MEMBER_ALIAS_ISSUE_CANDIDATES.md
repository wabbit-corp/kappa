# Hylo Member And Alias Issue Candidates For Kappa

Mined from `../datatron/data-scala-issues/hylo-issues.json` on 2026-04-23.

Search coverage for this note:

- focused follow-up scan for `typealias`, `associated type`, `generic arguments`, `receiver`, `member`, `tuple labels`,
  `extension`, `conformance`, `lookup`
- direct body reads so far:
  - `hylo#629`
  - `hylo#674`
  - `hylo#819`
  - `hylo#921`
  - `hylo#936`
  - `hylo#955`
  - `hylo#1042`
  - `hylo#1398`
  - `hylo#1550`
  - `hylo#1582`

Purpose: track Hylo issues that map not to Kappa ownership, but to existing Kappa rules for default trait members,
transparent type aliases, local generic-context capture, associated static members, and receiver/member resolution.

## Accepted Now

| source | bucket | spec_refs | fit | current_coverage | fixture_kind | expected_outcome | proposed_fixture_root | status | notes |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| `hylo#674`, `hylo#629` | `local_generic_context_capture` | `§6.3.1`, `§11.3`, `§14.1.1` | `indirect` | `partial` | `single-file` | `positive-check` | `declarations.let_in.positive_local_type_alias_captures_outer_type_parameter` | `accepted-now` | Staged in `new-tests/`; Kappa has no extension declarations, but the shared semantic obligation is that a nested declaration keeps referring to the outer generic parameter rather than inventing a fresh type variable during elaboration. |
| `hylo#819` | `traits_members_default_resolution` | `§12.2.1` | `direct` | `partial` | `single-file` | `positive-check` | `traits.members.positive_default_member_uses_required_member` | `accepted-now` | Staged in `new-tests/`; a default member body should resolve another member through the same synthesized trait evidence instead of losing the receiver context. |
| `hylo#936`, `hylo#955` | `type_alias_transparency` | `§5.5.10`, `§14.3` | `direct` | `partial` | `single-file` | `positive-check` | `data_types.type_aliases.positive_alias_preserves_member_projection` | `accepted-now` | Staged in `new-tests/`; transparent aliases should preserve ordinary member projection, including through an alias chain. |
| `hylo#1042` | `associated_static_members_alias_resolution` | `§12.2.1`, `§12.3`, `§7.3.3` | `direct` | `partial` | `single-file` | `positive-check` | `traits.instances.positive_associated_member_from_resolved_dictionary` | `accepted-now` | Already staged elsewhere in `new-tests/`, and the premise-dependent associated-member case is already in the real fixture suite. |
| `hylo#1398` | `associated_static_member_projection_through_alias` | `§11.3`, `§12.2.1` | `direct` | `partial` | `single-file` | `positive-check` | `traits.instances.positive_associated_member_projection_through_alias` | `accepted-now` | Staged in `new-tests/`; a transparent alias parameterized by explicit trait evidence should preserve projected associated static members in type position. |
| `hylo#1582` | `traits_associated_static_member_namespaces` | `§12.2.1`, `§12.3` | `direct` | `partial` | `single-file` | `positive-check` | `traits.instances.positive_same_spelling_associated_members_across_traits` | `accepted-now` | Staged in `new-tests/`; distinct trait dictionaries may expose associated static members with the same spelling, and each projection must stay receiver-scoped. |

## Accepted Later / Hold

- None from the current direct-read set.

## Rejected / Poor Fits For Now

- `hylo#921`
  - likely belongs to a later explicit-type-instantiation pass rather than this member/alias batch; it is weaker than the
    already staged default-member, alias, and associated-member roots.

- `hylo#1550`
  - mostly about Hylo's extension-binding machinery. Kappa has no extension declarations, so the architectural fix does
    not transfer directly even though some of the surrounding alias and receiver obligations do.

## Recommended Pull Order

1. merged `hylo#674` + `hylo#629`
2. `hylo#819`
3. merged `hylo#936` + `hylo#955`
4. `hylo#1398`
5. `hylo#1582`
6. `hylo#1042`

That order matches current Kappa needs: capture of outer generic context first, then default-member resolution, then
transparent alias/member projection, then associated-member projection through an alias, then same-spelling associated
members across traits, and only after that the more associated-member-specific alias-resolution case that is already
mostly covered.
