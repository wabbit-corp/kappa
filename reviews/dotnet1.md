I reviewed the .NET backend statically. I could **not** run the test suite because this container has no `dotnet` CLI installed, so this is a source review rather than a build-verified report. Humanity continues bravely: a .NET backend reviewed in an environment without .NET.

## Executive summary

The .NET backend is structurally promising, but there are several correctness traps. The highest-risk issues are:

1. **`host.dotnet` bindings depend on process-local compiler state and are not self-contained.**
2. **`--native-aot` is effectively advertised but rejected or structurally incompatible.**
3. **String literal pattern matching uses IL `ceq`, which gives reference equality, not string equality.**
4. **Or-patterns silently compile as only the first alternative.**
5. **`Char` / grapheme / Unicode scalar representation is inconsistent across backend layers.**
6. **Some advertised intrinsics, especially `printlnString`, are missing in the IL backend/runtime path.**
7. **Generic ADT construction inside generic methods appears broken or over-restricted.**
8. **The `dotnet-il` runner prints unit values, unlike the managed runner.**
9. **Process execution for `dotnet build/run/publish` can deadlock on large stdout/stderr.**

The first four are the ones I would treat as “fix before trusting real programs.”

---

# Findings

## 1. `host.dotnet` bindings are not self-contained

**Severity:** High
**Confidence:** High

The compiler resolves host .NET callables through a process-global cache in:

```text
src/Kappa.Compiler/HostBindings.fs
```

The emitter later depends on that cache when generating method bodies for imported host .NET functions:

```text
src/Kappa.Compiler/IlDotNetBackendEmit.fs
```

The problem is that the generated `ClrAssemblyIR` does not appear to carry the exact host callable identity. It relies on state accumulated earlier in the same compiler process.

That creates two related problems:

### A. Emission depends on hidden compiler process state

If `ClrAssemblyIR` is loaded/emitted independently, or reused from a checkpoint, host bindings may fail because the emitter no longer has the `ConstructorInfo` / `MethodInfo` cache populated.

That is a nasty backend smell: the IR says “I am enough to emit,” while secretly needing a side table lurking in a global cache like a tax loophole.

### B. Generated runner project does not copy host dependency assemblies

The generated runner project in `Backend.fs` only copies the emitted Kappa assembly:

```text
Kappa.Generated.Runner.csproj
```

It does not reference or copy assemblies needed by host .NET bindings.

The current tests likely mask this because they load and invoke the generated assembly inside the same test process, where the host test assembly is already loaded. The CLI runner is a different process. That means a program using `host.dotnet` may compile, then fail at runtime when the generated assembly tries to resolve external host types.

### Suggested fix

Persist host-callable identity into the backend IR. For each host .NET binding, store enough to re-resolve it during emission/runtime:

```fsharp
type ClrHostCallable =
    | StaticMethod of declaringTypeAssemblyQualifiedName: string * methodName: string * parameterTypes: string list
    | InstanceMethod of declaringTypeAssemblyQualifiedName: string * methodName: string * parameterTypes: string list
    | Constructor of declaringTypeAssemblyQualifiedName: string * parameterTypes: string list
```

Then either:

1. Emit direct member references by resolving from this durable metadata during assembly emission.
2. Add required host assemblies to the generated runner project as references with `HintPath`.
3. Or, at minimum, copy required assemblies beside the generated assembly.

The current design is test-friendly but deployment-hostile. Naturally, this is exactly where compilers like to betray people.

---

## 2. `--native-aot` is present but effectively unsupported

**Severity:** High
**Confidence:** High

There is a `NativeAot` deployment mode and the CLI has code paths for `dotnet publish`, but the managed .NET backend rejects native AOT before it gets that far.

Relevant places:

```text
src/Kappa.Compiler/BackendTypes.fs
src/Kappa.Compiler.Cli/Program.fs
src/Kappa.Compiler/Backend.fs
```

In `Backend.fs`, `emitClrDotNetArtifact` rejects anything except `Managed`:

```fsharp
match deployment with
| DotNetDeployment.Managed -> Ok ()
| DotNetDeployment.NativeAot ->
    Error "The CLR assembly backend currently supports only managed execution; NativeAOT is not yet implemented"
```

But the CLI still exposes a native-AOT path for the `dotnet` backend.

There is another deeper issue: the generated runner invokes the Kappa assembly using reflection:

```csharp
Assembly.LoadFrom(...)
GetType(...)
GetMethod(...)
Invoke(...)
```

That is not a good NativeAOT shape. NativeAOT and trimming tend to punish reflection-heavy late-bound code unless you add careful rooting metadata. The current runner is dynamically loaded and reflective, so even if the earlier explicit rejection were removed, this would remain fragile.

### Suggested fix

Choose one of these:

### Option A: remove/disable NativeAOT for this backend

Fail at CLI argument validation with a direct message:

```text
--native-aot is not supported for backend dotnet. Use backend dotnet-il managed output, or omit --native-aot.
```

### Option B: implement a NativeAOT-compatible runner

Generate a runner project that statically references the emitted assembly and directly calls the entrypoint. That means no `Assembly.LoadFrom`, no `MethodInfo.Invoke`.

For example, instead of:

```csharp
var assembly = Assembly.LoadFrom("foo.dll");
var type = assembly.GetType("Kappa.Generated.Main");
var method = type.GetMethod("main");
var result = method.Invoke(null, null);
```

generate:

```csharp
var result = Kappa.Generated.Main.main();
```

This requires the generated assembly to be referenced by the runner project, not copied as inert cargo.

---

## 3. String literal pattern matching uses reference equality

**Severity:** High
**Confidence:** High

Pattern matching on literals is emitted using `ceq`:

```text
src/Kappa.Compiler/IlDotNetBackendEmit.fs
```

The relevant shape is:

```fsharp
| KRuntimeLiteralPattern literal ->
    emitExpression state il inputExpr
    emitLiteral il literal
    il.Emit(OpCodes.Ceq)
```

For primitive numeric values, `ceq` is fine. For strings, it is not. On reference types, `ceq` checks object identity. String interning may accidentally make some cases pass, because apparently even bugs deserve camouflage.

So a match like this is semantically suspect:

```kappa
match x with
| "hello" -> ...
| _ -> ...
```

If `x` and `"hello"` are different string instances with the same content, this can fail.

There is a related gap in binary equality. The backend handles equality for int, float, bool, and char-like primitive cases, but I did not see string equality handled in `emitBuiltinBinary`.

### Suggested fix

For string equality in both pattern matching and `==` / `!=`, emit one of:

```csharp
string.Equals(left, right, StringComparison.Ordinal)
```

or:

```csharp
string.op_Equality(left, right)
```

In IL terms, when the compared type is `System.String`, do not emit `ceq`; call a string equality method.

For pattern matching, the lowering needs type-aware literal comparison:

```fsharp
match literal, inferredInputType with
| KRuntimeStringLiteral _, IlPrimitive IlString ->
    emitExpression state il inputExpr
    emitLiteral il literal
    il.EmitCall(OpCodes.Call, stringEqualsMethod, null)
| _ ->
    emitExpression state il inputExpr
    emitLiteral il literal
    il.Emit(OpCodes.Ceq)
```

Use ordinal semantics unless the language explicitly wants culture-sensitive equality, which would be a bold way to make equality depend on the user’s locale and mood.

---

## 4. Or-patterns silently ignore every alternative after the first

**Severity:** High
**Confidence:** High

Both the typing and emission paths treat `KRuntimeOrPattern` as if only the first alternative exists.

In emission:

```text
src/Kappa.Compiler/IlDotNetBackendEmit.fs
```

The shape is effectively:

```fsharp
| KRuntimeOrPattern alternatives ->
    match alternatives with
    | first :: _ -> emitPatternMatch state il inputExpr first successLabel failureLabel boundLocals
    | [] -> il.Emit(OpCodes.Br, failureLabel)
```

The typing path has the same “first alternative only” behavior.

That means:

```kappa
match x with
| A | B -> ...
```

compiles as approximately:

```kappa
match x with
| A -> ...
```

This is worse than a missing feature because it gives silently wrong semantics.

### Suggested fix

Either implement or-patterns properly or reject them before IL emission.

Proper implementation needs:

1. Try each alternative.
2. Branch to a shared success label on match.
3. Fall through to the next alternative on failure.
4. Ensure all alternatives bind the same names with compatible types.

Pseudo-shape:

```fsharp
let success = il.DefineLabel()
let nextAlt = il.DefineLabel()

for alt in alternatives do
    let altFailure = il.DefineLabel()
    emitPatternMatch state il inputExpr alt success altFailure boundLocals
    il.MarkLabel altFailure

il.Emit(OpCodes.Br, failureLabel)
il.MarkLabel success
```

But that is only safe if binding environments line up. If alternative `A(x)` binds `x` and alternative `B(y)` binds `y`, the compiler must reject it or normalize bindings. Do not quietly choose one. Quiet compilers are how people get production mysteries.

---

## 5. `Char`, Unicode scalar, grapheme, and byte representation is inconsistent

**Severity:** Medium-High
**Confidence:** Medium-High

There are conflicting representations across the backend.

Examples:

```text
src/Kappa.Compiler/IlDotNetBackendInput.fs
```

maps:

```fsharp
"Char" -> IlPrimitive IlChar
```

And:

```text
src/Kappa.Compiler/HostBindings.fs
```

maps CLR `typeof<char>` to Kappa `"Char"`.

But in:

```text
src/Kappa.Compiler/IlDotNetBackendEmit.fs
```

character and grapheme literals are emitted/inferred as strings:

```fsharp
| KRuntimeLiteral.Character value -> il.Emit(OpCodes.Ldstr, string value)
| KRuntimeLiteral.Grapheme value -> il.Emit(OpCodes.Ldstr, value)
```

And the backend lowering appears to map `Char`, `UnicodeScalar`, and `Grapheme` to string-like backend representations.

So depending on whether a `Char` comes from a literal, a frontend type name, a host binding, or lowered backend representation, it may be treated as either:

```text
System.Char
```

or:

```text
System.String
```

That is a backend boundary bug waiting to happen.

### Example failure mode

A host binding exposes:

```csharp
static bool IsLetter(char c)
```

The importer maps `char` to Kappa `Char`, which the IL backend parses as `IlChar`.

But a Kappa character literal may be emitted as `System.String`.

So calling:

```kappa
isLetter 'a'
```

can easily become a type mismatch or invalid IL, depending on the path.

### Suggested fix

Pick one canonical representation and enforce it everywhere.

Possible designs:

### Option A: Kappa `Char` is a Unicode scalar encoded as `string`

Then:

* Do not map Kappa `Char` directly to CLR `System.Char`.
* Host boundary conversion must convert one-scalar strings to `char` only where safe.
* Reject host `char` interop unless explicit conversion exists.

### Option B: Kappa `Char` is CLR `char`

Then:

* Emit character literals with `Ldc_I4` and conversion to `char`, not `Ldstr`.
* Keep grapheme as `string`.
* Be very explicit that grapheme and char are different.

Given that the language appears to distinguish `Char`, `UnicodeScalar`, and `Grapheme`, treating all of them as casual strings in one place and `char` in another is not sustainable.

---

## 6. `printlnString` is declared but missing in the IL backend path

**Severity:** Medium
**Confidence:** High

The prelude declares:

```kappa
println
printlnString
print
printInt
printString
```

And the intrinsic catalog includes `printlnString`.

But the IL backend intrinsic table appears to include:

```text
print
println
printString
printInt
...
```

without `printlnString`.

Relevant files:

```text
src/Kappa.Compiler/Stdlib/std/prelude.kp
src/Kappa.Compiler/IntrinsicCatalog.fs
src/Kappa.Compiler/IlDotNetBackendInput.fs
src/Kappa.Compiler/IlDotNetBackendEmit.fs
src/Kappa.Compiler/Runtime/Hosted/KappaRuntime.cs
```

The hosted runtime also appears to include `print`, `println`, and `printInt`, but not consistently `printString` / `printlnString`.

### Suggested fix

Normalize the intrinsic list across all layers:

```fsharp
"print"         : string -> Unit
"println"       : string -> Unit
"printString"   : string -> Unit
"printlnString" : string -> Unit
"printInt"      : int64 -> Unit
```

Then implement:

```fsharp
printString     -> Console.Write(string)
printlnString   -> Console.WriteLine(string)
```

Also decide whether `println` and `printlnString` are aliases. If yes, encode that explicitly instead of hoping every backend independently reaches enlightenment.

---

## 7. Generic ADT construction inside generic methods appears over-restricted

**Severity:** Medium-High
**Confidence:** Medium

The backend has support for generic methods and generic ADT types, but constructor resolution appears to reject cases where constructor result types still contain type parameters.

Relevant areas:

```text
src/Kappa.Compiler/IlDotNetBackendInput.fs
src/Kappa.Compiler/IlDotNetBackendEmit.fs
```

The suspicious path is constructor inference/resolution for generic constructors. A program like this is the kind of thing I would expect to fail or be mishandled:

```kappa
type List a =
  | Nil
  | Cons a (List a)

let singleton x =
  Cons x Nil
```

The result type is `List a`, where `a` is a method-level generic parameter. The emitter has generic method support, but constructor resolution seems to expect fully concrete type arguments too early.

I saw code that rejects constructor result types containing type parameters, and another path where constructor type resolution uses an empty type-parameter map while resolving substitutions. That makes generic constructor use inside generic methods brittle.

### Suggested fix

Thread method-level generic parameters through constructor resolution.

In other words, when resolving:

```fsharp
Cons : a -> List a -> List a
```

inside:

```fsharp
singleton<'a> : 'a -> List<'a>
```

the backend should allow `IlTypeParameter "a"` to resolve to the current method generic parameter builder/type, not reject it for failing to become concrete.

The fix likely belongs in both:

1. Constructor type inference from arguments.
2. CLR constructor/member resolution for generic ADTs.

Add tests for:

```kappa
let singleton x = Cons x Nil
let pair x y = Pair x y
let mapOption f option = ...
```

Do not only test already-concrete `List Int` cases. Those are training wheels.

---

## 8. `dotnet-il` prints unit results, unlike the managed runner

**Severity:** Medium
**Confidence:** High

The generated managed runner suppresses printing for:

```csharp
null
System.ValueTuple
```

But the `dotnet-il` CLI path appears to always do:

```fsharp
Console.Out.WriteLine(formatIlValue value)
```

So an entrypoint returning unit will print something like:

```text
()
```

or whatever the boxed `ValueTuple` formatting path returns.

That creates inconsistent behavior between:

```text
--backend dotnet
```

and:

```text
--backend dotnet-il
```

### Suggested fix

Centralize result-printing policy.

For example:

```fsharp
let shouldPrintIlResult (value: obj) =
    not (isNull value) &&
    value.GetType() <> typeof<ValueTuple>
```

Then use the same policy in both generated C# runner and F# CLI runner.

---

## 9. `dotnet build/run/publish` process execution can deadlock

**Severity:** Medium
**Confidence:** Medium-High

The CLI process helper reads stdout fully, then stderr fully, then waits for exit.

Relevant file:

```text
src/Kappa.Compiler.Cli/Program.fs
```

The shape appears to be:

```fsharp
let stdout = p.StandardOutput.ReadToEnd()
let stderr = p.StandardError.ReadToEnd()
p.WaitForExit()
```

This can deadlock when the child process writes enough to stderr while the parent is blocked reading stdout, or vice versa. `dotnet build` and `dotnet publish` can absolutely produce enough output to make this exciting in the worst possible way.

### Suggested fix

Read stdout and stderr asynchronously:

```fsharp
let stdoutTask = p.StandardOutput.ReadToEndAsync()
let stderrTask = p.StandardError.ReadToEndAsync()

p.WaitForExit()

let stdout = stdoutTask.Result
let stderr = stderrTask.Result
```

Or use event-based async reads.

This is not exclusively a .NET backend bug, but it directly affects the .NET backend because it shells out to `dotnet`.

---

## 10. Entry-point resolution uses `KBackendIR`, not the emitted CLR IR

**Severity:** Low-Medium
**Confidence:** Medium

The managed backend resolves the entrypoint from:

```text
workspace.KBackendIR
```

but emits from:

```text
workspace.ClrAssemblyIR
```

That is probably fine if the pipeline always constructs these in lockstep. But it means the emitted assembly and selected entrypoint can drift if the workspace is reused, cached, transformed, or partially constructed.

Relevant path:

```text
src/Kappa.Compiler/Backend.fs
```

The function `resolveClrEntryPoint` searches `KBackendIR`, then the backend emits `workspace.ClrAssemblyIR`.

### Suggested fix

Resolve the entrypoint from the actual CLR assembly model being emitted, or validate that the selected KBackend binding maps to a specific CLR method that exists in `ClrAssemblyIR`.

This is lower priority, but compilers should avoid “trust me, bro” invariants when a cheap assertion would do.

---

# Test coverage gaps I would add

The existing tests cover several useful cases: simple functions, conditionals, cross-module calls, recursive int functions, generic list ADTs, constructor imports, and host-dotnet wrappers. But the risky .NET backend behaviors above need targeted tests.

I would add these before refactoring heavily:

## 1. String pattern matching

```kappa
let classify s =
  match s with
  | "abc" -> 1
  | _ -> 0
```

Test with a dynamically constructed string, not only a literal:

```csharp
new string(new[] { 'a', 'b', 'c' })
```

Expected:

```text
1
```

This catches accidental reference equality.

---

## 2. String equality

```kappa
let same a b = a == b
```

Invoke with two non-identical string instances containing the same content.

Expected:

```text
true
```

---

## 3. Or-pattern semantics

```kappa
let f x =
  match x with
  | 1 | 2 -> 10
  | _ -> 20
```

Expected:

```text
f 1 = 10
f 2 = 10
f 3 = 20
```

Right now I would expect `f 2` to fail semantically if the current emission path is used.

---

## 4. `printlnString`

```kappa
let main = printlnString "hello"
```

Expected output:

```text
hello
```

and no extra unit result.

---

## 5. Generic constructor inside generic function

```kappa
type List a =
  | Nil
  | Cons a (List a)

let singleton x = Cons x Nil
```

Then instantiate with at least:

```text
Int
String
```

This catches method-level generic constructor resolution bugs.

---

## 6. Host .NET binding through generated runner process

Do not just load the generated assembly in the test process. Actually generate the runner project and execute it in a fresh process.

That test should reference a host .NET method from a separate assembly and verify the generated project either copies or references that assembly correctly.

Current in-process tests are likely flattering the backend. Flattery is what bugs wear to court.

---

## 7. `dotnet` vs `dotnet-il` unit result behavior

Entrypoint:

```kappa
let main = println "hello"
```

Expected output for both backends:

```text
hello
```

Not:

```text
hello
()
```

---

# Suggested patch order

I would fix these in this order:

1. **Reject or implement unsupported semantics explicitly**

   * Or-patterns should either work or hard-fail.
   * NativeAOT should either be removed from this path or made real.

2. **Fix equality**

   * String pattern matching.
   * String binary equality/inequality.

3. **Normalize intrinsic support**

   * `print`, `println`, `printString`, `printlnString`, `printInt`.
   * Same behavior across runtime, IL backend, and prelude.

4. **Make host .NET bindings durable**

   * Remove dependency on process-global callable cache during emission.
   * Store host member identity in IR.
   * Reference/copy host assemblies in generated projects.

5. **Resolve `Char` representation**

   * Decide whether Kappa `Char` is CLR `char` or string.
   * Make literals, type parsing, host interop, and lowering agree.

6. **Fix generic ADT constructor resolution**

   * Allow method-level generic type parameters to flow into constructor resolution.

7. **Clean up behavior parity**

   * Unit result printing.
   * Process async stdout/stderr reading.
   * Entry-point validation against emitted CLR IR.

---

# Bottom line

The .NET backend is not merely “missing polish.” It has several semantic bugs that can produce wrong programs while looking successful, which is the compiler equivalent of smiling while handing someone a live grenade.

The most urgent fixes are:

```text
or-patterns
string equality / string literal matching
host.dotnet self-contained emission
NativeAOT CLI/backend mismatch
```

After that, clean up intrinsics and type representation. The current test suite is decent for happy-path IL generation, but it does not yet pressure the places where .NET backends usually rot: reflection, host assembly resolution, generic metadata, string equality, and cross-process execution.
