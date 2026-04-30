I reviewed the compiler observability, incremental metadata, checkpoint contracts, verification, dumps, and trace plumbing in the uploaded tree. I could not run the .NET test suite because this container does not have `dotnet` installed, because apparently civilization remains a patchwork of missing runtimes and prayers. This is a static review.

## Verdict

The observability layer is **useful as a public-facing descriptive model**, but it is **not yet sound as an incremental compilation contract**.

The compiler now exposes named checkpoints, checkpoint contracts, stage dumps, target manifests, a pipeline trace, query records, fingerprints, and incremental units. That is good infrastructure. But several of the “incremental” pieces are too weak or too synthetic to safely drive cache reuse. Right now, it looks more like a well-labelled control panel than an actual reactor.

## Major findings

### 1. `SourceFingerprint` is unsound: it does not include source text

In `CompilationMetadata.compilerFingerprints`, the source fingerprint identity is built from:

```fsharp
source:path=...
length=...
lines=...
module=...
```

See `src/Kappa.Compiler/CompilationMetadata.fs:382-398`.

That violates the spec’s own requirement that `SourceFingerprint` be determined by “the source text and any source-level metadata that affects parsing or module identity” in `Spec.md:20996-20998`.

Two different files with the same path, same length, same line count, and same inferred module name will produce the same source fingerprint. For example, `let answer = 41` and `let answer = 42` have the same length and line count. Yes, human beings built an incremental compiler fingerprint that can miss the number changing. Very brave.

**Fix:** include a deterministic content digest, preferably SHA-256 of `document.Source.Content`, plus normalized file identity and any parser-affecting metadata.

```fsharp
let sourceContentHash = sha256Hex document.Source.Content

let identity =
    [
        $"source:path={document.Source.FilePath}"
        $"contentSha256={sourceContentHash}"
        $"length={document.Source.Length}"
        $"lines={document.Source.LineCount}"
        $"module={moduleNameText document.ModuleName}"
    ]
    |> String.concat ";"
```

Better still, make these identities canonical structured records and hash the canonical serialization.

---

### 2. Query, fingerprint, and incremental unit IDs are not scoped by analysis session or build configuration

The ID constructors are dangerously small:

```fsharp
let private queryId queryKind inputKey outputCheckpoint =
    $"{QueryKind.toPortableName queryKind}:{inputKey}->{outputCheckpoint}"
```

`src/Kappa.Compiler/CompilationMetadata.fs:43-44`

```fsharp
let private fingerprintId fingerprintKind inputKey =
    $"{CompilerFingerprintKind.toPortableName fingerprintKind}:{inputKey}"
```

`src/Kappa.Compiler/CompilationMetadata.fs:149-150`

```fsharp
let private unitId unitKind inputKey =
    $"{IncrementalUnitKind.toPortableName unitKind}:{inputKey}"
```

`src/Kappa.Compiler/CompilationMetadata.fs:505-506`

The records carry `AnalysisSessionIdentity`, `BuildConfigurationIdentity`, backend profile, and intrinsic set, but the IDs themselves do not. If a cache, trace viewer, or external tool treats IDs as stable keys, the same source file compiled under `interpreter`, `zig`, and `dotnet` can collide.

The tests check that those fields exist on records, but they do not verify that IDs are collision-safe across sessions or backend profiles. The wallpaper is tasteful, but the wall is load-bearing cardboard.

**Fix:** either:

1. include session/build/profile/compiler-version in the ID, or
2. rename these IDs to local display keys and add separate globally stable keys.

For real caching, I would use something like:

```text
query:<analysisSessionDigest>:<queryKind>:<inputKeyDigest>:<outputCheckpoint>
fingerprint:<analysisSessionDigest>:<kind>:<inputKeyDigest>
unit:<analysisSessionDigest>:<unitKind>:<inputKeyDigest>
```

Also include the compiler implementation version in the analysis/build identity. The backend fingerprint includes `compilerImplementationId/version` at `CompilationMetadata.fs:369-374`, but source/header/body/interface fingerprints and the analysis session do not.

---

### 3. Import/interface dependency tracking is missing from the incremental graph

The spec says downstream ordinary compilation depends on imported module-interface units, not imported implementation bodies, in `Spec.md:20981-20982`. It also requires query dependencies to record the queries and fingerprints actually used, in `Spec.md:23171-23177`.

The current metadata does not model this.

`ModuleImportSurfaceUnit` depends only on the source file text unit:

```fsharp
[ sourceUnitByFile[frontendModule.FilePath].Id ]
```

`src/Kappa.Compiler/CompilationMetadata.fs:556-566`

`DeclarationHeaderUnit` depends only on the local import-surface unit:

```fsharp
[ importSurfaceUnitByFile[frontendModule.FilePath].Id ]
```

`src/Kappa.Compiler/CompilationMetadata.fs:581-588`

`ModuleInterfaceUnit` depends on the local import-surface unit and local declaration headers:

```fsharp
(importSurfaceUnitByFile[frontendModule.FilePath].Id :: headerDependencies)
```

`src/Kappa.Compiler/CompilationMetadata.fs:617-631`

There is no dependency from an importing module to the imported module’s interface fingerprint/unit. So if module `B` imports module `A`, and `A` changes its exported signature, the metadata does not force `B`’s import resolution, body resolution, diagnostics, KCore lowering, or backend lowering to invalidate.

**Fix:** build a canonical import graph from resolved import specs and add dependencies:

```text
B.import-surface -> A.module-interface
B.declaration-header -> effective import environment fingerprints
B.declaration-body -> B.header + effective import environment + imported interfaces
B.KCore -> B.module-interface + B.body units + imported interface fingerprints
```

Also make query records reflect those same dependencies. Right now the `queryPlan` is mostly a linear per-file fantasy, and fantasies are traditionally poor cache invalidation strategies.

---

### 4. Host and standard backend modules are omitted from incremental units and target dependencies

`KRuntimeIR` includes lowered user modules plus generated host-binding modules and standard modules:

```fsharp
loweredModules
@ (hostBindingModules |> Map.values |> Seq.map HostBindings.toRuntimeModule |> Seq.toList)
@ (StandardModules.all |> List.map StandardModules.toRuntimeModule)
```

`src/Kappa.Compiler/Compilation.fs:130-137`

But backend fingerprints are only produced for backend modules whose `SourceFile` is present in frontend interface fingerprints:

```fsharp
workspace.KBackendIR
|> List.filter (fun backendModule -> interfaceFingerprintByFile.ContainsKey(backendModule.SourceFile))
```

`src/Kappa.Compiler/CompilationMetadata.fs:483-501`

And backend incremental units are similarly filtered through `kCoreUnitByFile`:

```fsharp
workspace.KBackendIR
|> List.filter (fun backendModule -> kCoreUnitByFile.ContainsKey(backendModule.SourceFile))
```

`src/Kappa.Compiler/CompilationMetadata.fs:659-670`

Then target units depend only on those backend fingerprints and backend units:

```fsharp
fingerprintIds = backendFingerprintIds
dependencyUnitIds = kBackendUnitIds
```

`src/Kappa.Compiler/CompilationMetadata.fs:681-692`

So generated standard modules and host binding modules can affect emitted target code, but are not modeled as target-lowering dependencies. That is a serious invalidation hole.

**Fix:** create explicit units/fingerprints for:

```text
StandardRuntimeModuleUnit
HostBindingModuleUnit
RuntimeIntrinsicSetUnit
BackendRuntimeSupportUnit
```

Then make `KBackendIRModuleUnit` and `TargetLoweringUnit` depend on them.

---

### 5. The pipeline trace is synthesized, not an actual execution trace

The spec says the trace records the “actual steps executed, in order” in `Spec.md:21142-21145`.

The implementation builds the trace after compilation from the final document list and target list:

```fsharp
let buildPipelineTrace
    (documents: KFrontIRModule list)
    (targetCheckpoints: string list)
    (verification: VerificationSummary)
```

`src/Kappa.Compiler/CompilationTrace.fs:23-26`

It unconditionally emits parse, build, every frontend phase transition, KCore lowering, KRuntimeIR lowering, KBackendIR lowering, verification steps, and target steps for each document.

That is not an execution trace. It is a reconstructed idealized plan. It cannot express skipped work, lazy query execution, cache reuse, failures before later phases, batching, or actual ordering. The `Reuse` trace event exists in `CompilationPipeline.fs:260-271`, but it is never emitted anywhere. A “reuse” event that never appears is the sort of optimism normally reserved for startup pitch decks.

**Fix:** collect trace events at the point each pipeline/query action actually runs. For now, rename this to something like `plannedPipelineTrace` if it remains synthetic. For real incremental mode, emit:

```text
cacheProbe
reuse
invalidate
execute
verify
skip
```

with stable unit/query IDs and dependency reasons.

---

### 6. Target checkpoint verification is inconsistent between Zig and .NET

Zig target emission rejects workspaces with diagnostics:

```fsharp
if workspace.HasErrors then
    Result.Error ...
```

`src/Kappa.Compiler/ZigCcBackendArtifact.fs:12-24`

But the CLR target manifest path only verifies `KBackendIR`:

```fsharp
let verificationDiagnostics = CheckpointVerification.verifyCheckpoint workspace "KBackendIR"
```

`src/Kappa.Compiler/CompilationCheckpoints.fs:72-78`

It does not check `workspace.HasErrors`.

That means `verifyTargetCheckpoint` can behave differently for `zig.c` and `dotnet.clr` on an erroneous workspace. Worse, `KBackendIR` verification does not necessarily mean the full workspace is semantically valid. The target checkpoint contract should not depend on which backend happened to remember to check diagnostics.

**Fix:** normalize target checkpoint verification. Either all target checkpoints fail when `workspace.HasErrors`, or all distinguish “checkpoint representation valid” from “artifact emission permitted.” Right now those two meanings are blurred.

I would split:

```fsharp
verifyTargetCheckpointManifest : WorkspaceCompilation -> checkpoint -> Diagnostic list
canEmitTargetArtifact : WorkspaceCompilation -> checkpoint -> Diagnostic list
```

---

### 7. CLI `--verify` silently skips verification when the workspace has errors

In the CLI:

```fsharp
match options.VerifyCheckpoint with
| Some checkpoint ->
    if workspace.HasErrors then
        false
    else
        let diagnostics = Compilation.verifyCheckpoint workspace checkpoint
        ...
```

`src/Kappa.Compiler.Cli/Program.fs:536-545`

So an explicit verification request can do nothing and not report that it did nothing. A command-line tool silently declining the command it was given is the sort of user experience that makes people invent shell aliases with profanity in them.

**Fix:** print a clear “verification skipped because workspace has errors” diagnostic, or run checkpoint verification anyway and report both source diagnostics and checkpoint diagnostics.

---

### 8. Checkpoint verification diagnostics do not satisfy the “related node/source origin” requirement

The spec says failed verification diagnostics must identify at least one related node, symbol, block, edge, or source origin, in `Spec.md:21155-21161`.

But all checkpoint diagnostics are created with:

```fsharp
Location = None
RelatedLocations = []
```

`src/Kappa.Compiler/CheckpointVerification.fs:7-14`

Many messages include textual labels, but they do not attach structured locations or related nodes. That is fine for humans reading terminal output, not fine for an observability contract.

**Fix:** change `makeDiagnostic` to accept optional location and related locations. Thread provenance/origin data into verification failures wherever available.

---

### 9. Stage dump diagnostics are not consistently checkpoint-local

Frontend phase dumps use `snapshot.Diagnostics`:

```fsharp
diagnostics = snapshot.Diagnostics |> List.map dumpDiagnostic
```

`src/Kappa.Compiler/CompilationDump.fs:1466-1480`

But `surface-source`, `KCore`, `KRuntimeIR`, and `KBackendIR` dumps use `workspace.Diagnostics`:

```fsharp
diagnostics = workspace.Diagnostics |> List.map dumpDiagnostic
```

Examples:
`surface-source`: `CompilationDump.fs:1382-1395`
`KCore`: `CompilationDump.fs:1402-1415`
`KRuntimeIR`: `CompilationDump.fs:1422-1435`
`KBackendIR`: `CompilationDump.fs:1442-1455`

That makes early checkpoint dumps include later/global diagnostics. For stage dumps, “diagnostics attached to the snapshot” should mean diagnostics meaningful at that checkpoint, not every complaint the compiler developed later after wandering through the house opening drawers.

**Fix:** provide checkpoint-specific diagnostic projection for every checkpoint, not just frontend phases.

---

### 10. `KRuntimeIR` is marked not profile-specific, but it appears profile-influenced

The contract says:

```fsharp
checkpointContract "KRuntimeIR" ImplementationDefinedCheckpoint (Some "KCore") false false
```

`src/Kappa.Compiler/CompilationCheckpoints.fs:34`

The last `false` means not profile-specific.

But `KRuntimeIR` construction includes host binding modules collected using the normalized backend profile:

```fsharp
let hostBindingModules =
    HostBindings.collectImportedModules normalizedBackendProfile documents
```

`src/Kappa.Compiler/Compilation.fs:94-96`

Then those modules are added to `KRuntimeIR` at `Compilation.fs:130-137`.

If the runtime IR can differ by backend profile, the contract metadata should say so. If it cannot differ, the profile argument should not be able to affect it. At present the metadata and construction disagree, or at least leave enough ambiguity to host a small committee.

**Fix:** either mark `KRuntimeIR` as profile-specific, or split the profile-neutral runtime IR from profile-specific host/runtime augmentation.

---

## Medium and low-priority issues

### Declaration keys are index-based

Declaration input keys include the declaration index:

```fsharp
$"{filePath}#declaration:{index}:{kind}:{name}"
```

`src/Kappa.Compiler/CompilationMetadata.fs:169-172`

This is stable enough for a toy example, but it is hostile to reuse. Insert one declaration at the top of a file and every subsequent declaration key shifts. Correctness may survive, but incremental performance takes a theatrical swan dive.

Prefer a stable declaration identity based on resolved symbol identity, source span, declaration kind, and spelling, with a tie-breaker only for true ambiguity.

---

### Macro expansion units have no fingerprints

`MacroExpansionUnit` is emitted with an empty fingerprint list:

```fsharp
(Some "macro-expansion")
[]
[ headerUnit.Id ]
```

`src/Kappa.Compiler/CompilationMetadata.fs:590-598`

If macros exist or are coming, this is not enough. Macro expansion must depend on macro definitions, expansion environment, imported interfaces, compiler version, and any transcript/config inputs.

---

### `queryPlan` is too linear

Each file’s query chain is mostly:

```text
parse -> RAW -> phase -> phase -> ... -> diagnostics -> KCore -> KRuntimeIR -> KBackendIR
```

`src/Kappa.Compiler/CompilationMetadata.fs:65-147`

That is useful as a visual sketch, but it does not represent actual semantic dependencies, import dependencies, trait solving dependencies, macro expansion dependencies, host binding dependencies, or backend runtime dependencies.

If the query metadata is meant for observability only, rename it accordingly. If it is meant for incremental reuse, it needs real dependency capture.

---

### The public API is better than the CLI

The public API exposes:

```fsharp
checkpointContracts
availableCheckpoints
verifyAllCheckpoints
portableRuntimeObligations
analysisSession
queryPlan
compilerFingerprints
incrementalUnits
pipelineTrace
dumpStage
```

`src/Kappa.Compiler/Compilation.fs:224-278`

The CLI exposes `--dump-stage`, `--trace`, and `--verify`, but not checkpoint listing, verify-each, query plan, fingerprints, incremental units, or runtime obligations. The spec asks for user-visible or API-visible, so this is not fatal, but it weakens the practical tooling story.

---

### JSON and S-expression metadata rendering is duplicated

There is a `metadataJson` helper at `CompilationDump.fs:1278-1289`, but the JSON dump cases inline the same metadata repeatedly instead of using it. The S-expression path uses `metadataSexpr`.

This is a maintenance trap. One future field will be added to one path and not the other, because entropy is the only project manager who never misses a meeting.

---

## What is good

The design has some solid pieces:

1. Checkpoint contracts are explicit and backend target checkpoints are profile-driven. See `CompilationCheckpoints.fs:29-46`.

2. Stage dumps include schema version, language version, compiler identity, checkpoint name, build configuration, backend profile, backend intrinsic set, and checkpoint contract metadata. See `CompilationDump.fs:1278-1346`.

3. Frontend snapshots correctly gate ownership facts until `BODY_RESOLVE`. See `CompilationSnapshots.fs:22-43`.

4. KBackendIR verification is materially stronger than the frontend checks. It validates erased metadata leaks, effect construct lowering, function uniqueness, layout references, calling convention arity, entrypoint/export consistency, intrinsic body rules, and resolved backend names. See `CheckpointVerification.fs:480-837`.

5. The tests cover the public surface enough to prevent accidental deletion of the observability APIs. The issue is not absence of tests, it is that the current tests mostly prove that metadata exists, not that it is invalidation-sound. A wonderfully bureaucratic achievement.

## Tests I would add next

Add tests that attack the metadata contract directly:

```text
1. SourceFingerprint changes when source content changes with identical path, length, line count, and module name.

2. Query/fingerprint/unit IDs differ across backend profiles and build configurations.

3. Module B imports module A. Changing A’s exported signature changes B’s relevant import/interface dependencies.

4. Changing only A’s private body does not invalidate B’s ordinary downstream units.

5. TargetLoweringUnit depends on generated standard modules and host binding modules.

6. `dotnet.clr` and `zig.c` target verification behave consistently on a workspace with source errors.

7. Pipeline trace emits `reuse` when an unchanged unit is reused in an incremental step suite.

8. Verification diagnostics contain at least one structured source location or related node when provenance exists.

9. JSON and sexpr dumps expose equivalent metadata for every checkpoint.

10. Declaration insertion does not destroy cache keys for unrelated following declarations.
```

## Recommended triage

Fix these first:

1. **Make fingerprints content- and config-sound.** Start with `SourceFingerprint`, compiler version, and profile/session scoping.

2. **Add imported interface dependencies.** Without this, incremental correctness is mostly pretend.

3. **Add generated runtime/host/standard module units.** Target artifacts depend on them.

4. **Replace synthetic trace with actual event recording, or rename it honestly.**

5. **Normalize target checkpoint verification across backends.**

After that, improve verification diagnostics and CLI exposure.

Final judgment: the observability/checkpoint story is architecturally pointed in the right direction, but the incremental metadata is not yet trustworthy as a cache invalidation model. It is acceptable as a smoke-testable public metadata layer; it is not acceptable as the thing that decides whether stale compiler output gets reused.
