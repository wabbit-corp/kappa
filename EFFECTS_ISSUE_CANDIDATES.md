# Effects / Handlers Issue Candidates For Kappa

Mined from:

- `../datatron/data-scala-issues/koka-issues.json`
- `../datatron/data-scala-issues/effekt-issues.json`
- `../datatron/data-scala-issues/unison-issues.json`

Search coverage for this note:

- keyword scan across the full corpora for `handler`, `resume`, `resumption`, `finally`, `finalizer`, `effect`, `ability`, `capability`, `short-circuit`
- direct body reads so far:
  - `koka#129`, `koka#360`, `koka#564`, `koka#649`
  - `effekt#50`, `effekt#548`, `effekt#861`, `effekt#971`
  - `unison#374`, `unison#696`, `unison#697`, `unison#979`, `unison#1010`

Purpose: identify effect-handler issues that map cleanly onto Kappa's existing handler, continuation, capture, and
finalization semantics, while separating strong direct fits from lower-level runtime or source-language-specific cases.

## Accepted Now

| source | bucket | spec_refs | fit | current_coverage | fixture_kind | expected_outcome | proposed_fixture_root | status | notes |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| `koka#129` | `effects_handlers_control` | `§7.1.2A`, `§8.1.9.1`, `§14.8` | `direct` | `partial` | `single-file` | `positive-run` | `effects.handlers.runtime_positive_short_circuit_preserved_under_handler` | `accepted-now` | Staged in `new-tests/`. Short-circuit boolean semantics must survive handler elaboration; a handler must not force evaluation of a branch that the short-circuit rule already excludes. |
| `koka#360` | `effects_handlers_control` | `§8.1.11`, `§8.7.2`, `§14.8.8` | `direct` | `partial` | `single-file` | `positive-run` | `effects.handlers.runtime_positive_non_resuming_operation_runs_finalizer` | `accepted-now` | Staged in `new-tests/`. Pending `defer` / `finally` obligations inside the captured segment must still run when an intercepted operation does not resume. |
| `effekt#50` | `effects_handlers_control` | `§8.1.8`, `§14.8.5`, `§17.3.1.7` | `direct` | `partial` | `single-file` | `negative-diagnostic` | `effects.handlers.negative_first_class_capability_escape` | `accepted-now` | Staged in `new-tests/`. A captured handler capability must not escape its delimiter through a first-class function value. |
| `effekt#548` | `effects_handlers_control` | `§8.1.8`, `§14.8.5`, `§17.3.1.7` | `direct` | `partial` | `single-file` | `negative-diagnostic` | `effects.handlers.negative_higher_order_capability_escape` | `accepted-now` | Staged in `new-tests/`. Same core obligation as `#50`, but through a higher-order API boundary instead of a directly returned closure. |
| `koka#649` | `effects_handlers_control` | `§8.1.8`, `§14.8.5`, `§17.3.1.7` | `direct` | `partial` | `single-file` | `negative-diagnostic` | `effects.handlers.negative_resumption_escape` | `accepted-now` | Staged in `new-tests/`. The repro checks that an operation that would resume is not allowed to smuggle the captured handler capability outward through `Choice`-scoped control flow. |
| `unison#979` | `effects_handlers_control` | `§8.1.9`, `§8.1.10` | `direct` | `partial` | `single-file` | `negative-diagnostic` | `effects.handlers.negative_handler_clause_for_wrong_effect_label` | `accepted-now` | Staged in `new-tests/`. A handler for one effect label must not be allowed to match or discharge requests from a different label. |
| `unison#1010` | `effects_handlers_control` | `§7.5`, `§8.1.9`, `§8.1.10`, `§14.8.9` | `direct` | `partial` | `single-file` | `positive-run` | `effects.handlers.runtime_positive_mutually_exclusive_handler_cases_order_invariant` | `accepted-now` | Staged in `new-tests/`. Once handler dispatch alternatives are mutually exclusive, behavior should not change merely because their source order changed. |

## Accepted Later / Hold

| source | bucket | spec_refs | fit | current_coverage | fixture_kind | expected_outcome | proposed_fixture_root | status | notes |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| `effekt#861` | `effects_handlers_control` | `§8.1.8.1`, `§14.8.6`, `§17.4` | `direct` | `none` | `stage-dump` | `trace` | `effects.handlers.trace_positive_multi_resume_optimization_preserves_semantics` | `accepted-later` | High-value optimizer guardrail. It belongs in the effects wave, but it likely needs a backend-toggle or trace-oriented harness shape instead of an ordinary source-only runtime fixture. |
| `effekt#971` | `effects_handlers_control` | `§8.1.8.1`, `§14.8.6`, `§17.4` | `direct` | `none` | `stage-dump` | `trace` | `effects.handlers.trace_positive_block_argument_scope_preserved_under_multi_resume_lowering` | `accepted-later` | Same broad Kappa obligation as `#861`, but specifically about optimizer / normalizer scoping under multiple resumption. Better merged into the later backend-lowering tranche. |
| `unison#374` | `effects_handlers_control` | `§8.1.10`, `§14.8.9`, `§17.3.1.7` | `indirect` | `none` | `single-file` | `positive-run` | `effects.handlers.runtime_positive_stacked_handlers_preserve_ambient_ability` | `accepted-later` | Interesting deep-handler / ability-plumbing case, but the clean Kappa analogue needs more care so it does not accidentally become a language-specific delayed-computation fixture instead of a handler fixture. |
| `unison#697` | `effects_handlers_control` | `§8.1.9`, `§8.1.10`, `§14.8.9` | `indirect` | `none` | `single-file` | `positive-run` | `effects.handlers.runtime_positive_proxy_handler_reinstalls_effect` | `accepted-later` | Good proxy/rehandling pattern, but it should wait until we decide whether the first direct batch is shallow-only, deep-only, or mixed. |
| `unison#696` | `effects_handlers_control` | `§8.1.9`, `§8.1.10`, `§14.8.9` | `indirect` | `none` | `single-file` | `negative-diagnostic` | `effects.handlers.negative_ambiguous_same_head_effect_use` | `hold-spec` | Related to the lexical-selection story, but the exact Kappa surface analogue for “same head type at once” is not yet pinned down cleanly enough to claim a direct fixture. Keep as spec / diagnostic follow-up rather than staging prematurely. |

## Rejected / Poor Fits For Now

- `koka#564`
  - poor fit for now: the issue is framed around `raw ctl`, queued first-class resumptions, and a scheduler-specific repro. Kappa's user-facing handler surface is higher-level, so this is better treated as indirect background evidence rather than a first fixture import.

## Recommended Pull Order

1. `koka#129`
2. `koka#360`
3. `unison#979`
4. `effekt#50` and `effekt#548`
5. `unison#1010`
6. `koka#649`
7. `effekt#861` and `effekt#971`
8. `unison#374` and `unison#697`

That order keeps the first effects tranche focused on source-visible semantic laws first: short-circuiting, finalization,
basic handler typing, and capability escape. The optimizer and more implementation-shaped cases can wait for the later
backend-oriented pass.
