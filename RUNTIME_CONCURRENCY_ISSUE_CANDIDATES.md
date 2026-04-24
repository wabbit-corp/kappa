# Runtime / Concurrency Issue Candidates

Purpose: track non-Kotlin issues, PRs, docs, and upstream tests relevant to Kappa's `IO`, fibers, structured
concurrency, interruption, supervision, cleanup, promises, blocking work, atomics, and runtime capability semantics.

Primary spec areas:

- §1.1 Boundary honesty and runtime capabilities as values
- §2.7C Standard atomic support module `std.atomic`
- §2.7D Standard supervision support module `std.supervisor`
- §8.1 Runtime computations, exits, fibers, timers, scopes, monitors, and STM
- §8.6-§8.7 `defer`, `using`, finalization, and do-scope unwinding
- Chapter 9 error handling and finalization
- §14.8 runtime semantics and mandatory cleanup waits
- §17.5 runtime capability declarations
- §17.13 foreign-call interruption classification and blocking routing

This note is intentionally separate from `EFFECTS_ISSUE_CANDIDATES.md`. The effects note covers handler/resumption
semantics; this note covers runtime-owned concurrency and cleanup observables.

## Search Coverage Ledger

| Repo | Manifest / artifact paths | Search coverage | Direct reads | PR / test coverage | Next pass |
| --- | --- | --- | --- | --- | --- |
| `chapel` | `repos/github-chapel-lang-chapel/manifest.json`, `issues.json`, `pulls.json`, `tests/manifest.json` | issue search: `fiber`, `task`, `thread`, `async`, `await`, `cancel`, `interrupt`, `mask`, `supervisor`, `scope`, `defer`, `finally`, `cleanup`, `resource`, `atomic`, `race`, `timeout`, `scheduler`, `stm`; manifest counts: 7,432 issues, 21,212 PRs, 324 docs, 48,656 tests | `#5608`, `#5751`, `#6316`, `#6334` | no PR/test reads yet | read Chapel tests around `begin`, `sync`, `coforall`, `deinit`, and task-private variables; then PRs for `#5608` / `#6334` if linked |
| `ponyc` | `repos/github-ponylang-ponyc/manifest.json`, `issues.json`, `pulls.json`, `tests/manifest.json` | issue search: same runtime regex plus `actor`; manifest counts: 2,120 issues, 3,052 PRs, 1 doc, 548 tests | `#34`, `#205`, `#224`, `#344`, `#350` | no PR/test reads yet | read tests around finalizers, actor monitoring, scheduler response, and stdio blocking behavior |
| `encore` | `repos/github-parapluu-encore/manifest.json`, `issues.json`, `pulls.json`, `tests/manifest.json` | issue search: same runtime regex plus `future`, `active object`, `get`, `blocking`; manifest counts: 361 issues, 519 PRs, 1 doc, 1,087 tests | `#184`, `#274`, `#412`, `#429`, `#758`, `#802`, `#880` | no PR/test reads yet | read tests around futures, active objects, `get`, `await`, finalisers, and task module behavior |
| `elixir` | `repos/github-elixir-lang-elixir/manifest.json`, legacy `../../elixir-issues.json`, `pulls.json`, `tests/manifest.json` | issue search: same runtime regex plus `gen_server`, `supervisor`, `assert_receive`; manifest counts: 5,962 issues, 9,303 PRs, 1 doc, 350 tests | `#392`, `#849`, `#869`, `#982`, `#1566`, `#1724` | no PR/test reads yet | read ExUnit / supervisor / receive tests; separate test-framework ergonomics from Kappa runtime semantics |
| `inko` | `repos/github-inko-lang-inko/manifest.json`, `issues.json`, `pulls.json`, `docs/manifest.json`, `tests/manifest.json` | issue search: `process`, `async`, `recover`, `throw`, `panic`, `fiber`, `actor`, `timeout`, `scheduler`, `drop`, `resource`, `future`, `task`, `thread`, `cancel`, `interrupt`; manifest counts: 884 issues, 73 PRs, 112 docs, 293 tests | `#110`, `#112`, `#113`, `#213`, `#216`, `#254`, `#256`, `#278`, `#316`, `#321`, `#332`, `#339` | harvested test path scan found `tests/std/test/compiler/test_async_await.inko`, `tests/std/test/compiler/test_drop.inko`, `tests/std/test/std/test_process.inko`, `tests/std/test/std/test_signal.inko`, `tests/std/test/std/test_sync.inko`, `tests/std/test/std/test_io.inko`, `tests/std/fixtures/fmt/async_await/input.inko`, and escape/drop diagnostics fixtures; files not read yet | read Inko `test_async_await`, `test_process`, `test_drop`, `test_sync`, and PRs `#378`, `#380`, `#397`, `#406`, `#449`, `#490`, `#495` |

## Staged Tests Added

No runtime/concurrency fixtures have been staged from this note yet. The first batch should wait until we decide whether
the current runtime surface in the local implementation can parse the standard `fork`, `await`, `Scope`, `Monitor`,
`timeout`, `race`, `defer`, and `using` examples from Spec.md.

## Candidate Records

### `chapel#5608`: explicit synchronization for `begin` tasks that capture outer variables

- URL: https://github.com/chapel-lang/chapel/issues/5608
- Artifact path: `repos/github-chapel-lang-chapel/issues.json`
- Artifact state: `issue-only`
- Bucket: `runtime_concurrency_resources`
- Spec refs: §8.1.3, §8.1.3D, §14.8.4, §14.8.4C
- Fit: `accepted-later`
- Current coverage: `none`
- Proposed fixture root: `effects.fibers.negative_child_captures_borrow_without_structured_join`
- Expected outcome: `negative-diagnostic` or `runtime-negative`
- Notes: Kappa's `fork` is structured by default and child shutdown/finalizer completion is part of scope exit. A child
  must not retain a borrow or lexical resource past the enclosing scope unless the lifetime is represented by an explicit
  scope, owned package, or other resource value.

### `chapel#6334`: task startup / teardown code

- URL: https://github.com/chapel-lang/chapel/issues/6334
- Artifact path: `repos/github-chapel-lang-chapel/issues.json`
- Artifact state: `issue-only`
- Bucket: `runtime_concurrency_resources`
- Spec refs: §8.1.3, §8.6, §8.7, §14.8.4C
- Fit: `accepted-later`
- Current coverage: `partial`
- Proposed fixture root: `effects.fibers.runtime_positive_child_task_finalizer_runs_on_scope_shutdown`
- Expected outcome: `positive-run`
- Notes: This maps directly to Kappa's requirement that finalizers and release actions attached to runtime work are
  source-visible obligations, not best-effort host cleanup. Existing staged effects tests cover finalizers under
  handlers, but not child-fiber or per-task cleanup.

### `chapel#5751`: libraries should clean up using module-level deinit

- URL: https://github.com/chapel-lang/chapel/issues/5751
- Artifact path: `repos/github-chapel-lang-chapel/issues.json`
- Artifact state: `issue-only`
- Bucket: `runtime_concurrency_resources`
- Spec refs: §8.7, §8.1.11, §17.7.7A
- Fit: `accepted-later`
- Current coverage: `partial`
- Proposed fixture root: `effects.resources.runtime_positive_releasable_resource_released_on_scope_exit`
- Expected outcome: `positive-run`
- Notes: Kappa has `Releasable`, `using`, `acquireRelease`, and `BridgePackage` release rules. This issue is evidence
  for a resource-release fixture, not for adding host-style module finalizers as a language feature.

### `ponyc#344`: `_final` is not always called

- URL: https://github.com/ponylang/ponyc/issues/344
- Artifact path: `repos/github-ponylang-ponyc/issues.json`
- Artifact state: `issue-only`
- Bucket: `runtime_concurrency_resources`
- Spec refs: §8.1.3, §8.7, §14.8.3C, §14.8.4C
- Fit: `accepted-later`
- Current coverage: `partial`
- Proposed fixture root: `effects.fibers.runtime_positive_finalizer_runs_before_interrupt_returns`
- Expected outcome: `positive-run`
- Notes: Kappa must not rely on prompt host GC finalizers for source-level release. `interrupt`, `timeout`, `race`, and
  scope shutdown wait for finalizers before reporting completion.

### `ponyc#350`: Erlang-style monitoring

- URL: https://github.com/ponylang/ponyc/issues/350
- Artifact path: `repos/github-ponylang-ponyc/issues.json`
- Artifact state: `issue-only`
- Bucket: `runtime_concurrency_resources`
- Spec refs: §8.1.3D, §2.7D, §14.8.4C
- Fit: `accepted-later`
- Current coverage: `none`
- Proposed fixture root: `effects.fibers.runtime_positive_monitor_observes_completion_without_ownership`
- Expected outcome: `positive-run`
- Notes: Kappa's `monitor` observes a fiber's terminal `Exit` without making it a child and without changing ownership or
  shutdown behavior. This is a clean monitor/supervision analogue.

### `ponyc#205`: runtime leaves stdio in nonblocking mode

- URL: https://github.com/ponylang/ponyc/issues/205
- Artifact path: `repos/github-ponylang-ponyc/issues.json`
- Artifact state: `issue-only`
- Bucket: `runtime_concurrency_resources`
- Spec refs: §17.13, §17.5
- Fit: `accepted-later`
- Current coverage: `none`
- Proposed fixture root: `runtime.blocking.runtime_positive_blocking_runtime_does_not_mutate_host_stdio_flags`
- Expected outcome: `runtime-positive` or backend conformance test
- Notes: Backend implementation choices for nonblocking I/O must not leak as ambient host-state mutation after Kappa
  runtime exit. This is probably a backend conformance test, not a source-level compiler fixture.

### `encore#184`: premature termination

- URL: https://github.com/parapluu/encore/issues/184
- Artifact path: `repos/github-parapluu-encore/issues.json`
- Artifact state: `issue-only`
- Bucket: `runtime_concurrency_resources`
- Spec refs: §8.1.3, §8.1.3D, §14.8.4C
- Fit: `accepted-later`
- Current coverage: `none`
- Proposed fixture root: `effects.fibers.runtime_negative_child_work_not_dropped_as_success`
- Expected outcome: `runtime-positive` with completion trace
- Notes: A structured computation must not report success while child work that is required for the source result is still
  live or has been silently dropped.

### `encore#412` / `encore#429`: future chaining and tracing futures

- URLs: https://github.com/parapluu/encore/issues/412, https://github.com/parapluu/encore/issues/429
- Artifact path: `repos/github-parapluu-encore/issues.json`
- Artifact state: `issue-only`
- Bucket: `runtime_concurrency_resources`
- Spec refs: §8.1.3, §8.1.3D, §14.8.3C, §14.8.4C
- Fit: `accepted-later`
- Current coverage: `none`
- Proposed fixture root: `effects.promises.runtime_positive_chained_future_retains_result_until_observed`
- Expected outcome: `positive-run`
- Notes: Promise/fiber results and chained continuations are runtime roots until they are no longer source-observable. Host
  GC must not make completion, failure, or later observation nondeterministic.

### `encore#758`: segmentation fault when blocking on futures

- URL: https://github.com/parapluu/encore/issues/758
- Artifact path: `repos/github-parapluu-encore/issues.json`
- Artifact state: `issue-only`
- Bucket: `runtime_concurrency_resources`
- Spec refs: §8.1.3, §8.1.3A, §17.13
- Fit: `accepted-later`
- Current coverage: `none`
- Proposed fixture root: `effects.fibers.runtime_positive_blocking_join_preserves_scheduler_context`
- Expected outcome: `positive-run`
- Notes: Blocking on a future/fiber must preserve runtime context and scheduler invariants. If blocking may monopolize a
  worker, it belongs in the runtime blocking lane rather than as an unsafe direct wait.

### `encore#802`: synchronization on multiple futures missing

- URL: https://github.com/parapluu/encore/issues/802
- Artifact path: `repos/github-parapluu-encore/issues.json`
- Artifact state: `issue-only`
- Bucket: `runtime_concurrency_resources`
- Spec refs: §8.1.3, §8.1.3B, §14.8.4C
- Fit: `hold-spec`
- Current coverage: `none`
- Proposed fixture root: `effects.fibers.runtime_positive_await_all_preserves_order_and_cause_composition`
- Expected outcome: `positive-run`
- Notes: Kappa specifies `await`, `join`, `race`, and supervision, but does not currently specify a collection-oriented
  `awaitAll` / `joinAll` combinator. Track as a possible standard-library ergonomic addition.

### `elixir#869` / `elixir#982`: application / process startup failure should stop dependent tests

- URLs: https://github.com/elixir-lang/elixir/issues/869, https://github.com/elixir-lang/elixir/issues/982
- Artifact path: legacy `elixir-issues.json`
- Artifact state: `issue-only`
- Bucket: `runtime_concurrency_resources`
- Spec refs: §8.1.2, §8.1.3D, §2.7D, §14.8.4C
- Fit: `accepted-later`
- Current coverage: `none`
- Proposed fixture root: `effects.supervisor.runtime_negative_child_start_failure_not_silent_success`
- Expected outcome: `runtime-negative`
- Notes: Kappa supervisors and scopes must surface child startup failure as `Exit` / `Cause` information and must not
  continue dependent work as if startup succeeded.

### `elixir#1566`: interrupt from shell control sequence is ignored

- URL: https://github.com/elixir-lang/elixir/issues/1566
- Artifact path: legacy `elixir-issues.json`
- Artifact state: `issue-only`
- Bucket: `runtime_concurrency_resources`
- Spec refs: §8.1.2, §8.1.3, §14.8.4C
- Fit: `accepted-later`
- Current coverage: `none`
- Proposed fixture root: `effects.fibers.runtime_positive_external_interrupt_uses_external_tag`
- Expected outcome: `runtime-positive`
- Notes: Host- or runtime-originated interruption not attributable to a Kappa fiber maps to `InterruptTag.External`.
  Shell/REPL delivery is not itself Kappa semantics, but the interruption classification is.

### `elixir#1724`: receive assertion passes with bound pattern variable

- URL: https://github.com/elixir-lang/elixir/issues/1724
- Artifact path: legacy `elixir-issues.json`
- Artifact state: `issue-only`
- Bucket: `conditionals_match_patterns`
- Spec refs: §7.6, §8.1.3D
- Fit: `accepted-later`
- Current coverage: `none`
- Proposed fixture root: `effects.mailbox.runtime_negative_receive_pattern_uses_existing_binding`
- Expected outcome: `runtime-negative`
- Notes: This is only partially runtime-shaped. The stronger Kappa fit is pattern semantics: a receive-like operation must
  respect existing bindings rather than rebinding them silently in an assertion pattern.

### `inko#110`: interrupting / rescheduling processes

- URL: https://github.com/inko-lang/inko/issues/110
- Artifact path: `repos/github-inko-lang-inko/issues.json`
- Artifact state: `issue-only`
- Bucket: `runtime_concurrency_resources`
- Spec refs: §8.1.3A, §8.1.3, §17.5
- Fit: `accepted-later`
- Current coverage: `none`
- Proposed fixture root: `effects.fibers.runtime_positive_long_running_fiber_reaches_safe_points`
- Expected outcome: `positive-run` or runtime trace
- Notes: Kappa requires weak fairness and implementation-defined safe points for long-running execution. This is a
  direct runtime-scheduler analogue, but it needs a runtime harness rather than a typechecker fixture.

### `inko#112`: receive wait prevents VM shutdown after another process errors

- URL: https://github.com/inko-lang/inko/issues/112
- Artifact path: `repos/github-inko-lang-inko/issues.json`
- Artifact state: `issue-only`
- Bucket: `runtime_concurrency_resources`
- Spec refs: §8.1.2, §8.1.3D, §14.8.4C
- Fit: `accepted-later`
- Current coverage: `none`
- Proposed fixture root: `effects.mailbox.runtime_negative_receive_wait_does_not_hide_runtime_failure`
- Expected outcome: `runtime-negative`
- Notes: Waiting for a receive-like operation must not block the runtime from observing another process/fiber failure and
  shutting down the relevant scope. This overlaps with `elixir#982`.

### `inko#213`: scope panics to processes, not programs

- URL: https://github.com/inko-lang/inko/issues/213
- Artifact path: `repos/github-inko-lang-inko/issues.json`
- Artifact state: `issue-only`
- Bucket: `runtime_concurrency_resources`
- Spec refs: §8.1.2, §8.1.3D, §2.7D, §14.8.4C
- Fit: `accepted-later`
- Current coverage: `none`
- Proposed fixture root: `effects.supervisor.runtime_positive_child_defect_scoped_to_child_exit`
- Expected outcome: `positive-run`
- Notes: Kappa distinguishes typed failure, interruption, and defects. A child fiber defect is observed through `Exit` /
  `Cause` and supervisor policy rather than automatically killing unrelated fibers.

### `inko#216`: deferred blocks during VM exit

- URL: https://github.com/inko-lang/inko/issues/216
- Artifact path: `repos/github-inko-lang-inko/issues.json`
- Artifact state: `issue-only`
- Bucket: `runtime_concurrency_resources`
- Spec refs: §8.7, §14.8.4C, §17.14
- Fit: `accepted-later`
- Current coverage: `partial`
- Proposed fixture root: `effects.fibers.runtime_positive_scope_shutdown_runs_child_defers_before_exit`
- Expected outcome: `positive-run`
- Notes: Kappa makes finalizer waits mandatory for structured interruption, `timeout`, `race`, and scope shutdown. Abrupt
  host process termination remains a separate backend limitation, but Kappa runtime-initiated shutdown must run source
  cleanup obligations where control remains recoverable.

### `inko#254`: receive-waiting process dropped when its handle is unreachable

- URL: https://github.com/inko-lang/inko/issues/254
- Artifact path: `repos/github-inko-lang-inko/issues.json`
- Artifact state: `issue-only`
- Bucket: `runtime_concurrency_resources`
- Spec refs: §8.1.3, §8.1.3D, §14.8.3C
- Fit: `accepted-later`
- Current coverage: `none`
- Proposed fixture root: `effects.fibers.runtime_positive_dropping_handle_does_not_terminate_structured_child`
- Expected outcome: `positive-run`
- Notes: Kappa fiber handles are observation/control values, not the only roots that keep structured child execution
  alive. Handle reachability and host GC cannot redefine child lifetime.

### `inko#278` / `inko#316`: blocking-operation thread pools

- URLs: https://github.com/inko-lang/inko/issues/278, https://github.com/inko-lang/inko/issues/316
- Artifact path: `repos/github-inko-lang-inko/issues.json`
- Artifact state: `issue-only`
- Bucket: `runtime_concurrency_resources`
- Spec refs: §8.1.3A, §17.13, §17.5
- Fit: `accepted-later`
- Current coverage: `none`
- Proposed fixture root: `runtime.blocking.runtime_positive_blocking_work_does_not_starve_runnable_fibers`
- Expected outcome: `runtime-positive` or backend conformance test
- Notes: Kappa permits different blocking-work implementations, but a backend must not execute blocking work in a way that
  starves unrelated runnable fibers or silently weakens interruption classification.

### `inko#321`: dropped values may escape from destructors

- URL: https://github.com/inko-lang/inko/issues/321
- Artifact path: `repos/github-inko-lang-inko/issues.json`
- Artifact state: `issue-only`
- Bucket: `quantities_borrowing`
- Spec refs: §5.1.6, §8.7, §14.8.4C
- Fit: `accepted-later`
- Current coverage: `partial`
- Proposed fixture root: `types.universes.quantities.qtt.negative_released_self_escape_from_finalizer`
- Expected outcome: `negative-diagnostic`
- Notes: This is a crossover with ownership: cleanup code must not reintroduce a released value into live state. Keep it
  visible here because it is specifically destructor/finalizer-shaped, but the fixture probably belongs in the ownership
  wave.

## Spec Additions To Consider

- Consider a standard `awaitAll` / `joinAll` / `collectAll` library combinator for a collection of fibers or promises.
  It should specify result ordering, cancellation behavior, finalizer waits, and `Cause` composition.
- Consider a backend conformance fixture category for host-process state that runtime initialization must restore or must
  not mutate ambiently, such as stdio blocking flags.
