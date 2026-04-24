# Kappa Language Specification

![Temporary Logo](https://espunisinjapan.com/wp-content/uploads/2023/12/kappa-1024x1024.jpg)

> **Status:** Draft.  
> **Scope:** Core language, core syntax, and core semantics.

---

<!-- design -->
## 1. Design Principles

Kappa is a small, statically typed, dependently typed language. The primary design constraints:

- **Explicit is better than implicit.**
- **Simple is better than complex.**
- **Readability counts.**
- **There should be one – and preferably only one – obvious way to do it.**
- **Minimize unnecessary punctuation and syntax noise.**
- Default stance: **totality, purity, parametricity** are desirable and encouraged.
- Non-strictness is represented by explicit suspension types and suspension terms, not by a distinct family of core
  arrow types. Surface non-strict binder syntax is only sugar over those suspension types.



---

<!-- modules -->
## 2. Modules, Imports, Prelude, and Resolution

<!-- modules.files -->
### 2.1 Modules and files

- The module name of a source file is determined from its file path relative to a source root. For example:
  - `std/base.kp` → module `std.base`
  - `user/orders/list.kp` → module `user.orders.list`

Normative mapping:
* Let the file path be `P` and the source root be `R`.
* `P` must be under `R` and must end in the extension ".kp", otherwise it is not a source file.
* Let `S` be the relative path from `R` to `P`, with the ".kp" suffix removed.
* Path separators are normalized such that '\' is treated as '/'.
* Write `S = dir1/dir2/.../dirm/base[.frag1[.frag2 ... [ .fragn ]...]]`, where `m >= 0`.
* Each directory segment `diri`, the basename segment `base`, and each optional fragment segment `fragj` must match the
  identifier regex `[A-Za-z_][A-Za-z0-9_]*`, otherwise it is a compile-time error.
* The path-derived module name is `dir1.dir2....dirm.base`.
* Optional fragment segments are not part of the module name. Thus:
  * `std/base.kp`             → module `std.base`
  * `main.kp`                 → module `main`
  * `main.win32.kp`           → module `main`
  * `std/base.posix.debug.kp` → module `std.base`
* Source files whose path-derived module names are equal are fragments of the same module.
* How a build selects, combines, or conditions such fragments is implementation-defined, except where this specification
  explicitly relies on that mechanism (for example §6.5).
* Module name segments are case-sensitive.
* Implementations MUST reject a compilation unit that contains two source files whose path-derived module names are
  equal after case-folding but differ in case.
* For the comparison in this rule, implementations MUST compare module names after converting each module-name segment
  to lowercase ASCII. Because path-derived module-name segments are restricted to ASCII letters, digits, and `_`, no
  Unicode normalization is required.
* The diagnostic for a case-fold collision MUST identify all colliding files.
* If an implementation needs a deterministic representative spelling for diagnostics or internal bookkeeping, it MUST
  choose the file whose normalized relative path from its source root is lexicographically smallest by Unicode
  scalar-value order.

Modules may have an explicit top-level module header:

```kappa
@PrivateByDefault module std.base
```

Grammar:

```text
moduleHeader ::= { '@' Ident }* 'module' modPath
```

Rules:

* A source file may contain at most one module header.
* A module header may be preceded by zero or more module attributes of the form: @Ident
* Multiple module attributes are applied left-to-right.
* An unknown module attribute is a compile-time error unless the implementation explicitly documents it.
* If a module header is present:
  * It must appear before any non-comment, non-whitespace token other than the leading module attributes.
  * In package mode, it MUST match the path-derived module name (compile-time error otherwise).
  * In script mode, it MAY differ; if it differs, the header name becomes the module name of the file.
    Effective module names are then subject to the same fragment, duplicate, and case-folding rules as path-derived
    module names.
* If no module header is present, the module name is always the path-derived module name.

Standard module attributes:

* `@PrivateByDefault`: all top-level named items are private unless explicitly marked `public`.

<!-- modules.acyclic_imports -->
### 2.2 Acyclic imports

- The module dependency graph formed by `import` statements and by any `export M...` statement that references another
  module **must be acyclic**.
- Implementations must reject programs with cyclic module dependencies.

<!-- modules.imports -->
### 2.3 Imports

Imports are explicit; however, implementations provide an implicit prelude interface (declarations only) that is in
scope by default, as defined in §2.6.

Valid import forms:

```kappa
-- Import the module only (qualified access only)
import std.math

-- Import module with alias
import std.math as math

-- Import specific names
import std.math.(sin, cos, pi)
import std.math.sin              -- sugar for import std.math.(sin)
import std.prelude.((>>=))      -- extra parentheses are required because `(>>=)` is a `nameRef`
import std.list.(ctor (::))
import std.list.(type List(..))

-- Import all public names
import std.math.*

-- Import all public names except some
import std.math.* except (sin, pi)
import std.math.* except (term sin, type pi, ctor Cons)

-- Import from a URL module
import "https://example.com/lib".*
import "https://example.com/lib".(foo, bar)
import "https://example.com/lib".* except (unsafe, debug)

-- Import a URL module for qualified access only
import "https://example.com/lib" as lib
```

An `import` statement may contain multiple import specifications separated by commas:

```kappa
import std.base.(math, io), std.text.*
import std.math as math, std.io as io
import "https://example.com/lib".*, std.base.(term println)
```

Rule:

Syntactically, import parses as: `import importSpec (',' importSpec)*`.

Each `importSpec` is one of the "Valid import forms" shown above. More precisely:

```text
moduleRef   ::= modPath | string_literal
nameRef     ::= ident | backtick_ident | '(' operator_token ')'
importSpec  ::= moduleRef
              | moduleRef 'as' ident
              | moduleRef '.(' importItem (',' importItem)* ')'
              | moduleRef '.*'
              | moduleRef '.*' 'except' '(' exceptItem (',' exceptItem)* ')'
importItem  ::= { 'unhide' | 'clarify' }* [kindSelector] nameRef [ctorAll]
kindSelector ::= 'term' | 'type' | 'trait' | 'ctor'
ctorAll     ::= '(..)'
exceptItem  ::= [kindSelector] nameRef
```

Surface singleton sugar:

* `import M.x` is surface sugar for a singleton import item.
* The parser MUST accept the surface form `moduleRef '.' nameRef` even though it is not part of the core `importSpec`
  grammar above.

Semantic disambiguation of bare dotted import forms:

* The bare form `import A.B.C` is ambiguous between:
  * module-only import of module `A.B.C`, and
  * singleton-item import of item `C` from module `A.B`.
* Disambiguation is performed after module discovery, not by pure longest-match parsing.
* If only one interpretation is resolvable, it is chosen.
* If both interpretations are resolvable, the form is a compile-time error.
* To force singleton-item import, write `import A.B.(C)`.
* To force module import, use any module-only form that cannot denote an item import, such as:
  * `import A.B.C as X`
  * `import A.B.C.*`
  * `import A.B.C.* except (...)`
* URL module references are never extended by dotted module-path continuation after the closing string literal, so:
  * `import "url".x` is always singleton-item import sugar for `import "url".(x)`.

Constraints:

* A dotted module path is `modSeg ("." modSeg)*`, where `modSeg` is defined in §3.1.
* URL imports use a **string literal** and are treated as modules by the implementation.
* In `... except (x, y)` each item may be unqualified or kind-qualified as `term x`, `type y`, `trait C`, or `ctor
  K`.
* An unqualified `except` item excludes every imported binding of that name that the wildcard form would otherwise
  introduce.
* A qualified `except` item excludes only that kind.

Imports are **not re-exported** by default (see `export` below).

Module aliases and qualification:

* `import M` brings the module name `M` into scope for qualified access (e.g. `M.x`) and, under §2.8.5, as a reified
  module value.
* `import M as A` brings only the alias `A` into scope for qualified access (e.g. `A.x`) and, under §2.8.5, as a
  reified module value. The name `M` is not brought into scope by this form.
* Selective imports (`import M.(...)`, `import M.*`, and `import M.* except (...)`) do not bring `M` into scope for
  qualified access or as a reified module value. To enable either, use a separate `import M` (or `import M as M`).
* `import "url" as A` brings only the alias `A` into scope for qualified access and as a reified module value. The
  string literal itself never becomes a qualifier.


<!-- modules.imports.item_kind_selectors -->
#### 2.3.1 Import item kind selectors

Kappa resolves names through lexical binding groups (§2.8), not through
separate shadowing namespaces. The selectors `term`, `type`, `trait`,
and `ctor` on import and export items are therefore kind selectors.

```kappa
import std.list.(type List)
import std.list.(type List(..))
import std.eq.(trait Eq)
import std.base.(term println)
import std.person.(ctor Person)
```

Grammar:

```text
importItem   ::= { 'unhide' | 'clarify' }* [kindSelector] nameRef [ctorAll]
kindSelector ::= 'term' | 'type' | 'trait' | 'ctor'
ctorAll      ::= '(..)'
exceptItem   ::= [kindSelector] nameRef
```

Rules:

* `term` selects an ordinary term declaration, including active patterns.
* `type` selects a type constructor or type alias.
* `trait` selects a trait constructor.
* `ctor` selects a data constructor.
* `type T(..)` imports the type `T` together with all constructors declared by `T`.

Unqualified import of a spelling:

* `import M.(x)` imports every exported declaration of spelling `x` except
  distinct-spelling constructors.
* A same-spelling data family is imported as one binding group containing
  its type facet and its same-spelling constructor facet (§2.8.2).
* Distinct-spelling constructors are not imported unqualified by default.
  Use `ctor K` or `type T(..)`.

Wildcard imports:

* `import M.*` imports all exported binding groups except
  distinct-spelling constructors.
* `import M.* except (...)` removes declarations after wildcard expansion.
  An unqualified item `x` removes every imported declaration of spelling `x`.
  A qualified item such as `type x`, `trait C`, or `ctor K` removes only
  that kind.

Shadowing and lookup:

* Imported declarations participate in ordinary lexical lookup and dotted
  receiver lookup exactly like local declarations (§2.8).
* A closer admissible declaration may shadow an imported declaration at a
  use site.

The same grammar and rules apply to `export` items in §2.4.

<!-- modules.imports.item_kind_selectors.unhide_clarify_import_items -->
##### 2.3.1.1 `unhide` and `clarify` import items

Import items may be prefixed by the modifiers:

* unhide
* clarify

```kappa
import std.rope.(unhide normalizeWorker)
import std.rope.(clarify Rope)

import std.rope.(unhide term normalizeWorker)
import std.rope.(clarify type Rope)
import std.rope.(unhide clarify term normalizeWorker)
```

An import item has the shape:

```
importItem ::= { 'unhide' | 'clarify' }* [kindSelector] nameRef
kindSelector ::= 'term' | 'type' | 'trait' | 'ctor'
```

Rules:

* `unhide` and `clarify` are unsafe/debug import modifiers. Their detailed semantics and build gating are specified in
  §16.2-§16.3.
* In brief, `unhide` requests access to a private imported item, and `clarify` requests that an imported opaque item be
  treated as transparent for definitional equality in the importing module.
* `unhide` and `clarify` may be combined for the same item. Their order in the surface syntax is not semantically
  significant.
* Repeating the same modifier within one import item is a compile-time error.

<!-- modules.imports.url_imports_pinning_reproducibility -->
#### 2.3.2 URL imports, pinning, and reproducibility

URL imports are intended for scripts and quick one-offs, but Kappa also supports reproducible package-mode builds when
each imported URL is tied to immutable content.

<!-- modules.imports.url_module_specifiers -->
#### URL module specifiers

A URL import specifier is a string literal. It may be **unpinned** or **pinned**.

Pinned URL specifiers use a `#` fragment within the string literal to carry the pin:

```kappa
import "https://example.com/lib#sha256:0123abcd...".*
import "https://example.com/lib#ref:v1.2.3".(foo, bar)
```

Rules:

* The portion before the first `#` is the base URL.
* The portion after the first `#` is the pin string.
* Pin formats are:
  * `sha256:<hex>` where `<hex>` is lowercase or uppercase hex. Implementations MUST accept both cases on input, but
    MUST canonicalize the digest to lowercase before computing module identity, lockfile entries, and verification. This
    names immutable content directly.
  * `ref:<text>` where `<text>` is an implementation-defined reference (e.g. a tag or commit id). A `ref:` pin is not,
    by itself, an immutability guarantee; the referenced name may move.
* Module identity rules:
  * The module identity of a URL import is always `(baseURL, digest)`.
  * For `sha256:` pins, `digest` is the canonical lowercase form of the digest named directly by the import specifier.
  * For `ref:` pins, the implementation MUST resolve the ref to immutable content (for example, a `sha256` digest)
    before assigning module identity.
  * In package mode, the resolved digest for a `ref:` pin MUST be recorded in a lockfile or equivalent build artifact.
  * In script mode, if only a `ref:` pin is present, the implementation MUST resolve it immediately, record the digest
    in a transient lock or equivalent run-local state, and treat that digest as the module identity for the current run.
    On first resolution for that run, the implementation MUST emit a warning containing the resolved digest, and
    implementations SHOULD provide a flag to write the transient lock to disk. Without such a persisted lock, subsequent
    runs are not guaranteed reproducible.
* Verification:
  * In package mode, after fetching a URL, the implementation MUST compute the SHA-256 digest of the retrieved content
    and compare it to the expected digest.
  * For a `sha256:` pin, the expected digest is the canonical lowercase form of the digest named directly by the import
    specifier.
  * For a `ref:` pin in package mode, the expected digest is the resolved immutable digest recorded in the lockfile or
    equivalent build artifact.
  * For a `ref:` pin in script mode, the expected digest for the current run is the resolved immutable digest recorded
    in the transient lock or equivalent run-local state.
  * A digest mismatch is a compile-time error.
  * For `ref:` pins in package mode, subsequent builds MUST verify against the recorded resolved digest and MUST NOT
    re-resolve the ref as part of ordinary reproducible compilation.
  * In script mode, if a `ref:` pin resolves to a different digest than it did in a previous run, the implementation
    MUST emit a diagnostic the first time that change is observed.

<!-- modules.imports.compilation_modes -->
#### Compilation modes

Implementations must support (at least) two compilation modes:

* **Script mode**:
  * Unpinned URL imports are permitted.
  * Implementations may cache fetched content.
  * Builds are not guaranteed reproducible.
* **Package mode**:
  * Unpinned URL imports are a compile-time error.
  * Only pinned URL imports are permitted.
  * Package-mode builds MUST be reproducible with respect to URL imports.
  * A `sha256:` pin satisfies this requirement directly.
  * A `ref:` pin is permitted only if the toolchain resolves it to immutable content and records that resolution in a
    lockfile or equivalent build artifact consulted by subsequent package-mode builds.
  * If the toolchain cannot supply such an immutable recorded resolution for a `ref:` pin, package-mode compilation is a
    compile-time error.
  * For reproducibility, module identity is the pair `(baseURL, digest)`, not the raw ref text.

How a toolchain selects "script mode" vs "package mode" is implementation-defined (e.g. a compiler flag, a package
manifest, a file directive, etc.).

<!-- modules.imports.host_binding_modules -->
#### 2.3.3 Host binding modules

Implementations MAY provide backend-specific host binding modules under the reserved module roots:

* `host.jvm`
* `host.dotnet`
* `host.native`

Implementations targeting the JVM MAY additionally reserve the subroot `host.jvm.jni` for JVM-specific JNI and
Invocation-API interop. Because that subroot lies under `host.jvm`, a source-defined module whose effective module name
is exactly `host.jvm.jni` or begins with `host.jvm.jni.` is also a compile-time error.

A source-defined module whose effective module name is exactly one of the reserved host roots above, or begins with one
of those roots followed by `.`, is a compile-time error.

A host binding module is an ordinary module for import, export, name resolution, hashing, interface browsing, and
tooling, but it is supplied from host metadata or ABI descriptions rather than from user-written Kappa source text.

Examples:

```kappa
import host.jvm.java.util.ArrayList as JArrayList
import host.dotnet.System.Text.StringBuilder as StringBuilder
import host.native.sqlite3 as sqlite
import host.jvm.jni.JNIEnv as JNIEnv
```

Rules:

* `host.jvm.<path>` denotes a JVM managed-host-binding scope derived from classfiles, JARs, the module path, or
  equivalent JVM metadata.
* `host.dotnet.<path>` denotes a CLR managed-host-binding scope derived from assembly metadata or equivalent CLR
  metadata.
* `host.native.<path>` denotes a native-ABI host-binding scope derived from an implementation-documented ABI
  description, such as headers, module maps, symbol lists, shim declarations, definition files, or another equivalent
  binding description.
* `host.jvm.jni.<path>` denotes a JVM-specific native-interop scope for JNI or Invocation-API bindings.
* A host binding module MAY denote a package/namespace, a type/class, a library/header set, a JNI namespace, or another
  implementation-documented host scope. The chosen scope kind MUST be recorded in the module interface artifact.
* Host binding modules are backend-specific. A module that directly imports `host.jvm...` is valid only when compiled
  for a backend profile that provides `host.jvm`, and similarly for `host.dotnet...`.
* A module that directly imports `host.native...` is valid only when compiled for a backend profile that provides
  `host.native`. Such a profile MAY be `zig`, `jvm`, `dotnet`, or another implementation-documented profile that
  provides a native-ABI adapter realization under §17.7.
* In package mode, every host binding module used by a build MUST be backed by a pinned host-source identity recorded in
  a lockfile or equivalent build artifact.
* A conforming implementation MAY realize a host binding module as a generated source module, a generated module
  interface artifact, a virtual module, or another observationally equivalent representation.

<!-- modules.exports -->
### 2.4 Exports (re-exporting imports)

Kappa does not implicitly re-export imports. To re-export an imported module or imported names, use `export`.

`export` statements are **top-level only**, and may appear anywhere among other top-level statements.

Valid export forms mirror import forms:

```kappa
-- Re-export the module only (qualified access for downstream users)
export std.math
-- Re-export module with alias
export std.math as math
-- Re-export specific names
export std.math.(sin, cos, pi)
export std.math.sin             -- sugar for export std.math.(sin)
export std.list.(type List(..))
-- Re-export all public names
export std.math.*
-- Re-export all public names except some
export std.math.* except (sin, pi)
export std.math.* except (type T)
-- Re-export from a URL module
export "https://example.com/lib".*
export "https://example.com/lib".(foo, bar)
export "https://example.com/lib".* except (unsafe, debug)
-- Re-export a URL module for qualified access only
export "https://example.com/lib" as lib
```

Syntactically, `export` parses as:

```text
export exportSpec (',' exportSpec)*
```

where `exportSpec` is the corresponding `importSpec` form of §2.3 with `import` replaced by `export`.

Surface singleton sugar:

* `export M.x` is surface sugar for `export M.(x)`.

Semantic disambiguation of bare dotted export forms:

* The same semantic disambiguation rule as §2.3 applies.
* Thus `export A.B.C` is:
  * module re-export of `A.B.C` if only that interpretation is resolvable,
  * singleton-item re-export of `C` from `A.B` if only that interpretation is resolvable,
  * and a compile-time error if both interpretations are resolvable.
* To force singleton-item re-export, write `export A.B.(C)`.
* To force module re-export, use a module-only form such as `export A.B.C as X` or `export A.B.C.*`.

The `except` form uses the same `exceptItem` grammar as §2.3. In particular, `export M.(type T(..))` re-exports the type
`T` together with all of its constructors.

Rules:

* An `export M...` statement is valid whether the module `M` is imported in the same file or not.
* `export M...` introduces the same module dependency as the corresponding `import M...` would.
* `export` only affects **re-exporting definitions**. Export default: without `@PrivateByDefault`, every top-level item
  in the current module is exported unless marked `private`. With `@PrivateByDefault`, every top-level item is private
  unless marked `public`.
* `export` does not apply to trait instances. Instances are not re-exported as ordinary items; instance search over them
  is defined separately in §12.3.
* An item imported via `unhide` or `clarify` MUST NOT be re-exported. Attempting to apply `export` to such an item is a
  compile-time error.

<!-- modules.visibility_opacity_private_opaque -->
### 2.5 Visibility and opacity (private, opaque)

Export default:

* Without `@PrivateByDefault`, every ordinary top-level named item in the current module is exported unless marked
  `private`.
* With `@PrivateByDefault`, every ordinary top-level named item in the current module is private unless marked `public`.

In a module without `@PrivateByDefault`, an explicit `public` modifier is a no-op; implementations may flag it as a
warning.

Trait instances are not ordinary exported items. `public` / `private` do not apply to instances; their participation in
downstream instance search is determined by the instance environment rules in §12.3, not by the module's export surface.

This section defines two orthogonal controls:

1. Visibility (whether a name is exported at all)
2. Transparency (whether definitional equality may unfold a definition outside its defining module)

<!-- modules.visibility_opacity_private_opaque.private -->
#### 2.5.1 private (visibility)

A top-level declaration or definition may be prefixed with private.

Examples:

```
private let helper = ...
private type Internal = ...
private data Node a = ...
```

Rules:

* A private item is not part of the module's export interface.
* Outside the defining module, a private item is not name-resolvable except when it appears as the target of an `unhide`
  or `clarify` import item in a compilation unit where the corresponding build flag is enabled. It cannot be imported by
  ordinary import forms.
* An importing module may access a private item only via an explicit unhide import item, and only if the compilation
  unit has enabled that unsafe/debug facility under §16.2-§16.3.

private affects only downstream visibility; it does not affect visibility within the defining module.

<!-- modules.visibility_opacity_private_opaque.opaque -->
#### 2.5.2 opaque (definitional transparency)

A top-level term definition or type alias definition may be prefixed with opaque.

Examples:

```
opaque let normalize : Syntax Type -> Syntax Type = ...
opaque type Id a = a
```

Rules:

* An opaque item remains exported (unless also private).
* Outside the defining module, an opaque item's definitional equation is not available for definitional equality (delta
  reduction does not unfold it). The item is treated as an opaque constant at its declared type.
* Inside the defining module, the definition remains available normally.

An importing module may request to treat an opaque item as transparent via an explicit clarify import item, and only if
the compilation unit has enabled that unsafe/debug facility under §16.2-§16.3.

<!-- modules.visibility_opacity_private_opaque.data -->
#### 2.5.3 opaque data (representation hiding)

A data declaration may be prefixed with opaque:

```
opaque data Rope a : Type =
    Leaf (xs : Array a)
    Node (l : Rope a) (r : Rope a) (size : Nat)
```

Rules:

* The type constructor Rope is exported (unless also private).
* The constructors of Rope are not exported. Outside the defining module, Rope's constructors are not name-resolvable,
  including through dotted static-member selection on the type (e.g. Rope.Node is not available).
* Pattern matching on Rope constructors outside the defining module is therefore not possible unless the importing
  module explicitly clarifies Rope.

Clarify on an opaque data type makes its constructors available both for construction and for pattern matching in the
importing module, provided the compilation artifact contains their definitions.

Exported trait instances for an opaque data type do not by themselves violate opacity. Such an instance may be
handwritten or derived in the defining module, where the representation is available. Outside the defining module,
however, using that instance exposes only the trait interface at the abstract type: it does not make constructors
name-resolvable, does not permit pattern matching on the hidden representation, and does not make the representation
available to definitional equality. Opacity constrains name resolution and definitional transparency, not all
observational behavior; an exported trait such as `Eq` or `Show` may still reveal whatever information its declared API
itself reveals.

<!-- modules.visibility_opacity_private_opaque.modifier_combinations -->
#### 2.5.4 Modifier combinations

private and opaque may be combined:

```
private opaque let x = ...
```

In such cases private controls visibility; opaque controls transparency when the item is accessed via escape hatch
mechanisms.

<!-- modules.prelude -->
### 2.6 Prelude

<!-- modules.prelude.implicit_import_behavior -->
#### 2.6.1 Implicit import behavior

Although Kappa has explicit imports, implementations provide an implicit **prelude interface** that is in scope by
default.

Normative rule:

* Each source file is processed as if it began with an implicit import of a prelude module:

  ```kappa
  import std.prelude.*
  ```

* In addition, each source file receives an implementation-inserted unqualified constructor import for the following
  fixed subset of prelude constructors:

  ```text
  True, False,
  None, Some,
  Ok, Err,
  Nil, (::),
  LT, EQ, GT,
  refl
  ```

  This has the effect of an implicit prelude constructor import for that exact subset only. It is the only exception to
  the rule that `*` does not import constructors unqualified.

* `std.prelude` is an ordinary module for language-semantics purposes.
  In particular:
  * its exported names participate in name resolution exactly like those of any other imported module;
  * its transparent definitions and transparent type aliases participate in elaboration, normalization, definitional
    equality, hashing, and separate compilation exactly like those of any other imported module;
  * its opaque and private items obey the ordinary visibility and transparency rules; and
  * an implementation MAY realize some prelude names as intrinsics or primitives, but this does not change their
    ordinary module semantics at the source-language level.

The exact contents of std.prelude are implementation-defined, but it must include:
* any declarations required by surface syntax (e.g. Bool and the meanings of True/False),
* the conventional prefixed-string handlers `f`, `re`, `b`, and `type` if prefixed strings are supported,
* fixity declarations for any operator tokens that the implementation expects to parse "out of the box"
(e.g. `+`, `*`, `==`, `and`, `or`, `..`, `..<`), consistent with infix gating (§3.5.3).

<!-- modules.prelude.contents -->
#### 2.6.2 Normative minimum contents

Implementations MUST provide a prelude module `std.prelude` that is implicitly imported (§2.6) and exports at least the
following:

In addition to the ordinary exports listed below, implementations MUST provide the compile-time row and label
declarations referenced by §§5.3.1-5.3.2 (`Label`, `EffLabel`, `ContainsRec`, `LacksRec`, `ContainsVar`, `LacksVar`,
`ContainsEff`, `LacksEff`, and `SplitEff`) whenever those names are user-nameable in source programs.

Types (type namespace):
```
Unit, Void, Bool, Char, String, Int, Nat, Integer, Float, Double, Real, Bytes, Ordering, SyntaxFragment,
Query a, RawComprehension a, ComprehensionPlan a,
Option a, Result e a, List a, Array a, Set a, Map k v,
Res a r, Match a r, Dec p, Dict c,
IO e a, UIO a, Fiber e a, FiberId, Exit e a, Cause e, InterruptTag, InterruptCause, DefectTag, DefectInfo,
Scope, Monitor e a, FiberRef a, Promise e a,
STM a, TVar a,
Duration, Instant, TimeoutError, RaceResult a b,
Regex, (=), Thunk a, Need a
```

`Integer`, `Double`, and `Real` are ordinary user-facing numeric types exported by `std.prelude`.
`Dict c` is the standard explicit dictionary reification type for constraints.

* `Integer` is an arbitrary-precision integer type.
* `Double` is the IEEE-754 binary64 floating-point type.
* `Real` is an arbitrary-precision real/decimal type for exact numeric programming.
* `Float` is the standard prelude alias of `Double`.

Constructors (constructor namespace):
```
Bool.True, Bool.False,
Ordering.LT, Ordering.EQ, Ordering.GT,
Option.None, Option.Some,
Result.Ok, Result.Err,
List.Nil, List.(::),
Res.(:&),
Match.Hit, Match.Miss,
Dec.Yes, Dec.No,
Exit.Success, Exit.Failure,
Cause.Fail, Cause.Interrupt, Cause.Defect, Cause.Both, Cause.Then,
InterruptTag.Requested, InterruptTag.ScopeShutdown, InterruptTag.TimedOut,
InterruptTag.RaceLost, InterruptTag.External, InterruptTag.Custom,
InterruptCause.InterruptCause,
DefectTag.Panic, DefectTag.AssertionFailed, DefectTag.ArithmeticFault,
DefectTag.StackOverflow, DefectTag.OutOfMemory, DefectTag.HostFailure,
DefectTag.ForeignContractViolation, DefectTag.UnhandledChildFailure,
DefectTag.OtherDefect,
DefectInfo.DefectInfo,
TimeoutError.Timeout,
RaceResult.LeftWins, RaceResult.RightWins,
(=).refl,
SyntaxFragment.Lit, SyntaxFragment.Interp, SyntaxFragment.InterpFmt,
Unit.Unit
```

Implicitly imported unqualified constructor subset (§2.6):
```
True, False,
None, Some,
Ok, Err,
Nil, (::),
LT, EQ, GT,
refl
```

Terms (term namespace):
```
pure, (>>=), (>>), (|>), (<|),
not, and, or, force, (&&), (||),
empty, (<|>), orElse,
negate,
absurd,
subst, sym, trans, cong,
floatEq,
runPure,
sandbox, unsandbox,
fork, forkDaemon, await, join, interrupt, interruptFork, interruptAs, interruptForkAs,
fiberId, currentFiberId, getFiberLabel, setFiberLabel, locallyFiberLabel,
cede, blocking,
poll, uninterruptible, mask, ensuring, acquireRelease,
newScope, withScope, forkIn, shutdownScope,
monitor, awaitMonitor, demonitor,
newFiberRef, getFiberRef, setFiberRef, locallyFiberRef,
newPromise, awaitPromiseExit, awaitPromise, completePromise,
nowMonotonic, sleepFor, sleepUntil, timeout, race,
atomically, newTVar, readTVar, writeTVar, retry, check,
f, re, b, type,
println, print
```

Several listed terms, such as `pure`, `(>>=)`, `empty`, `(<|>)`, and `orElse`, are overloaded member names induced by
the corresponding prelude traits (§12.2.1).

`floatEq : Float -> Float -> Bool` compares floating-point values using IEEE numeric equality (so `NaN` is never equal
and `+0.0` equals `-0.0`). It is not the default `(==)` for `Float`.

Traits (constraint namespace, restricted to trait):
```
Equiv, Eq, Ord, Show, Shareable,
Functor, Applicative, Monad, Alternative,
Foldable, Traversable, Filterable, FilterMap, Monoid, Iterator, InterpolatedMacro,
IntoQuery, FromComprehensionPlan, FromComprehensionRaw,
FromInteger, FromFloat, FromString,
MonadError, MonadFinally, MonadResource, MonadRef, Releasable
```

Canonical declarations:

```kappa
data Unit : Type =
    Unit

data Void : Type

data Bool : Type =
    True
    False

type Float = Double

data Ordering : Type =
    LT
    EQ
    GT

data List (a : Type) : Type =
    Nil
    (::) (head : a) (tail : List a)

data Res (a : Type) (r : Type) : Type =
    (:&) (value : a) (1 resource : r)

data Match (a : Type) (r : Type) : Type =
    Hit a
    Miss r

data Dec (p : Type) : Type =
    Yes p
    No (p -> Void)

Dict : Constraint -> Type

expect data IO (e : Type) (a : Type) : Type
type UIO (a : Type) = IO Void a

expect data Fiber (e : Type) (a : Type) : Type
expect data FiberId : Type
expect data Scope : Type
expect data Monitor (e : Type) (a : Type) : Type
expect data FiberRef (a : Type) : Type
expect data Promise (e : Type) (a : Type) : Type

expect data STM (a : Type) : Type
expect data TVar (a : Type) : Type
expect data Duration : Type
expect data Instant : Type

data InterruptTag : Type =
    Requested
    ScopeShutdown
    TimedOut
    RaceLost
    External
    Custom String

data InterruptCause : Type =
    InterruptCause (tag : InterruptTag) (by : Option FiberId)

data DefectTag : Type =
    Panic
    AssertionFailed
    ArithmeticFault
    StackOverflow
    OutOfMemory
    HostFailure
    ForeignContractViolation
    UnhandledChildFailure
    OtherDefect String

data DefectInfo : Type =
    DefectInfo (tag : DefectTag) (message : Option String)

data Exit (e : Type) (a : Type) : Type =
    Success a
    Failure (Cause e)

data Cause (e : Type) : Type =
    Fail e
    Interrupt InterruptCause
    Defect DefectInfo
    Both (Cause e) (Cause e)
    Then (Cause e) (Cause e)

data TimeoutError : Type =
    Timeout

data RaceResult (a : Type) (b : Type) : Type =
    LeftWins a
    RightWins b

data (=) (@0 a : Type) (x : a) : a -> Type =
    refl : x = x

absurd :
    forall (@0 a : Type).
    Void -> a

subst :
    forall (@0 a : Type) (@0 P : a -> Type) (@0 x : a) (@0 y : a).
    (@0 p : x = y) -> P x -> P y

fiberId :
    forall (e : Type) (a : Type).
    Fiber e a -> UIO FiberId

currentFiberId :
    UIO FiberId

getFiberLabel :
    UIO (Option String)

setFiberLabel :
    Option String -> UIO Unit

locallyFiberLabel :
    forall (e : Type) (a : Type).
    Option String -> IO e a -> IO e a

interruptAs :
    forall (e : Type) (a : Type).
    InterruptCause -> Fiber e a -> UIO Unit

interruptForkAs :
    forall (e : Type) (a : Type).
    InterruptCause -> Fiber e a -> UIO Unit

cede :
    UIO Unit

blocking :
    forall (e : Type) (a : Type).
    IO e a -> IO e a

newScope :
    UIO Scope

withScope :
    forall (e : Type) (a : Type).
    (Scope -> IO e a) -> IO e a

forkIn :
    forall (e : Type) (a : Type).
    Scope -> IO e a -> UIO (Fiber e a)

shutdownScope :
    Scope -> UIO Unit

monitor :
    forall (e : Type) (a : Type).
    Fiber e a -> UIO (Monitor e a)

awaitMonitor :
    forall (e : Type) (a : Type).
    Monitor e a -> UIO (Exit e a)

demonitor :
    forall (e : Type) (a : Type).
    Monitor e a -> UIO Unit

newFiberRef :
    forall (a : Type).
    a -> UIO (FiberRef a)

getFiberRef :
    forall (a : Type).
    FiberRef a -> UIO a

setFiberRef :
    forall (a : Type).
    FiberRef a -> a -> UIO Unit

locallyFiberRef :
    forall (e : Type) (a : Type) (b : Type).
    FiberRef a -> a -> IO e b -> IO e b

newPromise :
    forall (e : Type) (a : Type).
    UIO (Promise e a)

awaitPromiseExit :
    forall (e : Type) (a : Type).
    Promise e a -> UIO (Exit e a)

awaitPromise :
    forall (e : Type) (a : Type).
    Promise e a -> IO e a

completePromise :
    forall (e : Type) (a : Type).
    Promise e a -> Exit e a -> UIO Bool

nowMonotonic :
    UIO Instant

sleepFor :
    Duration -> UIO Unit

sleepUntil :
    Instant -> UIO Unit

timeout :
    forall (e : Type) (a : Type).
    Duration -> IO e a -> IO (| TimeoutError | e |) a

race :
    forall (e : Type) (a : Type) (f : Type) (b : Type).
    IO e a -> IO f b -> IO (| e | f |) (RaceResult a b)

trait Alternative (f : Type -> Type) =
    empty : f a
    (<|>) : f a -> f a -> f a

    let orElse : f a -> f a -> f a =
        (<|>)

trait Iterator (it : Type) =
    Item : Type
    next : (1 this : it) -> Option (item : Item, rest : it)

Query : Type -> Type
RawComprehension : Type -> Type
ComprehensionPlan : Type -> Type

trait IntoQuery (src : Type) =
    Item : Type
    toQuery : src -> Query Item

trait FromComprehensionPlan (c : Type) =
    Item : Type
    fromComprehensionPlan : ComprehensionPlan Item -> Syntax c

trait FromComprehensionRaw (c : Type) =
    Item : Type
    fromComprehensionRaw : RawComprehension Item -> Syntax c
```

The comprehension sink hooks `fromComprehensionPlan` and `fromComprehensionRaw` are elaboration-time hooks.

They are not ordinary runtime conversion functions.

When the compiler selects one of these hooks under §10.9, it evaluates that hook during elaboration under the same
elaboration-time evaluator and restrictions as §5.8.6.

The returned `Syntax c` is then elaborated at the original comprehension site, using the same lexical context,
expected-type information, name-resolution, implicit-insertion, visibility, opacity, `unhide`, and `clarify` rules
that would apply to an ordinary splice at that site.

Primitive suspension types:

```kappa
Thunk : Type -> Type
Need  : Type -> Type
```

`Thunk a` is a non-memoized suspension of a computation producing `a`.
`Need a` is a memoized suspension of a computation producing `a`.

The prelude additionally provides:

```kappa
trait Shareable (a : Type)
```

`Shareable a` classifies values that may be safely cached and reused by `Need`.

The prelude also provides a forcing operator `force` with the following primitive typing cases:

```kappa
force :
    forall (@0 a : Type).
    Thunk a -> a

force :
    forall (@0 a : Type).
    Need a -> a
```

Here `force` denotes one prelude term name with these primitive typing cases, not two separate same-kind top-level
declarations.

The canonical short-circuit operators are ordinary prelude definitions:

```kappa
(&&) : Bool -> Thunk Bool -> Bool
let (&&) lhs rhs =
    if lhs then force rhs else False

(||) : Bool -> Thunk Bool -> Bool
let (||) lhs rhs =
    if lhs then True else force rhs
```

Thus `&&` and `||` are ordinary terms. They are not special evaluation forms.

`IO e a` is the standard runtime computation type.

* The parameter `e` classifies expected, recoverable failures.
* Interruption and defects are not represented by `e`; they are tracked by `Cause e` and `Exit e a`.
* `UIO a` is the conventional alias for computations that do not fail with an expected typed error.
* `STM a` is the standard software-transactional-memory computation type.
* `TVar a` is the standard transactional variable type.
* `Fiber e a` is the standard lightweight runtime thread handle.
* `FiberId` is the standard source-visible runtime fiber identity type.
* `InterruptTag` classifies the reason for runtime interruption.
* `InterruptCause` is the standard structured interruption payload carried by `Cause.Interrupt`.
* `InterruptTag.Custom s` is the standard library-extensible interruption tag for user-defined cancellation reasons.
* `DefectTag` classifies the portable minimum defect vocabulary of the runtime.
* `DefectInfo` carries the portable defect tag plus an optional diagnostic message.
  Implementations MAY retain richer diagnostic data out of band.
* `Scope` is the standard explicit supervision-scope handle.
* `Monitor e a` is the standard one-way fiber-termination observation handle.
* `FiberRef a` is the standard fiber-local dynamically scoped cell.
* `Promise e a` is the standard one-shot completion cell carrying `Exit e a`.
* `Instant` and `Duration` are the standard monotonic-time runtime types.
* `TimeoutError` is the standard expected error used by `timeout`.
* `RaceResult a b` is the standard result type returned by `race`.
* `blocking` requests backend-supported blocking-lane execution for a computation without changing its observable
  success, failure, interruption, or defect semantics.

`Void`, `absurd`, and `Dec` are the standard proof-oriented prelude basics:

* `Void` is the empty type.
* `absurd` is ex falso elimination from `Void`.
* `Dec p` packages a decision procedure for proposition `p`, carrying either positive evidence `Yes p` or a refutation
  `No (p -> Void)`.

`Res a r` is the standard prelude wrapper for threading a linear resource `r` alongside an ordinary value `a`.

The standard prelude definition of `Applicative`, and the fact that `Monad` refines `Applicative` in `std.prelude`, are
specified in §12.1 (with Appendix G providing the `ApplicativeDo` amendment that relies on that relationship).

Instances: All canonical instances of the above traits for the above types (e.g. coherent evidence for `Equiv Int`, `Eq
Int`, `Ord Int`, `Show String`, `Monad (IO e)`, `MonadError (IO e)`, `MonadFinally (IO e)`, `MonadResource (IO e)`,
`MonadRef (IO e)`, `Functor STM`, `Applicative STM`, `Monad STM`, `Alternative STM`, `Functor (Eff r)`, `Applicative
(Eff r)`, `Monad (Eff r)`, ...).

`Eq Float` and `Eq Double` use raw IEEE-754 bit equality. Use `floatEq` for numeric equality.

Fixities: Implementations MUST provide fixity declarations, or fixed built-in parsing precedences for reserved
operator-like syntax. The following minimum table is normative:

```text
(==) (~=) (/=) (<) (<=) (>) (>=) : infix 40
(&&)                             : infix right 30
(||)                             : infix right 20
(..) (..<)                       : infix 45
(::) (++)                        : infix right 50
(+) (-)                          : infix left 60
(*) (/) (%)                      : infix left 70
(-)                              : prefix 80
(:&)                             : infix right 4
(|>)                             : infix left 1
(|>=)                            : infix left 1   -- if provided
(<|)                             : infix right 0
?.                               : left-associative precedence 100
?:                               : right-associative precedence 2
```

`(~=)` MUST have the same precedence and associativity as `(==)`.

User fixity declarations for the bare `?` token, including `postfix (?)`, do not affect the reserved tokens `?.` and
`?:`; those tokens always use their built-in parsing behavior (§3.5.3, §7.1.1.2, §7.1.2).

The monadic splice form `!` is reserved surface syntax (§5.7.1), not an ordinary operator token, and therefore is not
controlled by prelude fixity declarations.

Names not in this list MAY be exported by `std.prelude`, but user code SHOULD NOT rely on their presence.

The standard prelude provisions for `|>`, `<|`, and the optional `|>=` operator are specified in Appendix B.

<!-- modules.ffi -->
### 2.7 Standard FFI support module `std.ffi`

Implementations MUST provide a standard module `std.ffi`. It is not implicitly imported.

Types (type namespace):

```text
I8, I16, I32, I64, Isize,
U8, U16, U32, U64, Usize,
F32, F64,
RawPtr,
OpaqueHandle
```

Rules:

* `I8`..`I64` and `U8`..`U64` are exact-width two's-complement integer types.
* `Isize` and `Usize` are signed and unsigned machine-word types matching the pointer width of the selected native ABI.
* `F32` is IEEE-754 binary32.
* `F64` is IEEE-754 binary64.
* `F64` MAY be represented identically to `Double`, but remains the exact-ABI scalar name used by `std.ffi`.
* `RawPtr` denotes an untyped native address. The language core provides no implicit dereference or pointer arithmetic.
  Such operations, if exposed, are library- or backend-defined.
* `OpaqueHandle` denotes an opaque foreign resource handle. Its concrete runtime representation is backend-specific.
* The exact ABI meaning of these types is normative for raw native host bindings under §17.7.
* `std.ffi` MAY additionally export implementation-defined adapter types and helper terms, provided the types above are
  available exactly as named.

<!-- modules.gradual -->
### 2.7A Standard gradual support module `std.gradual`

Implementations MUST provide a standard module `std.gradual`. It is not implicitly imported.

Types (type namespace):

```text
Dyn,
DynRep a,
CastBlame
```

Traits (constraint namespace, restricted to trait):

```text
DynamicType a
```

Terms (term namespace):

```text
toDynWith,
checkedCastWith,
sameDynRep,
toDyn,
checkedCast
```

Canonical declarations:

```kappa
expect data Dyn : Type
expect data DynRep (@0 a : Type) : Type
expect data CastBlame : Type

trait DynamicType (a : Type) =
    dynRep : DynRep a

toDynWith :
    forall (@0 a : Type).
    DynRep a -> a -> Dyn

checkedCastWith :
    forall (@0 a : Type).
    DynRep a -> Dyn -> Result CastBlame a

sameDynRep :
    forall (@0 a : Type) (@0 b : Type).
    DynRep a -> DynRep b -> Dec (a = b)

toDyn :
    forall (@0 a : Type).
    (@_ : DynamicType a) -> a -> Dyn

checkedCast :
    forall (@0 a : Type).
    (@_ : DynamicType a) -> Dyn -> Result CastBlame a
```

Rules:

* `Dyn` is the standard explicit dynamic-value type.
* `DynRep a` is a runtime representation sufficient to validate values at type `a`.
* `DynamicType a` is coherent evidence that such a runtime representation is available.
* `toDyn` and `checkedCast` are convenience forms that obtain the representation from `DynamicType`.
* This section introduces explicit dynamic values only. It does not by itself introduce an ambient unknown type,
  consistency-based subtyping, or implicit cast insertion among arbitrary non-`Dyn` types.

<!-- modules.bridge -->
### 2.7B Standard bridge-contract support module `std.bridge`

Implementations MUST provide a standard module `std.bridge`.
It is not implicitly imported.

Types (type namespace):

```text
BridgeContract sig
```

Traits (constraint namespace, restricted to trait):

```text
BridgeBindable sig
BridgeHandle h
```

Terms (term namespace):

```text
bindModule
```

Canonical declarations:

```kappa
expect data BridgeContract (@0 sig : Type) : Type

trait BridgeBindable (sig : Type) =
    bridgeContract : BridgeContract sig

trait BridgeHandle (h : Type) =
    Error : Type

    bindModuleWith :
        forall (r : Region) (@0 sig : Type).
        BridgeContract sig ->
        (&[r] this : h) ->
        String ->
        IO this.Error (sig captures (r))

bindModule :
    forall (h : Type) (r : Region) (@0 sig : Type).
    (@H : BridgeHandle h) ->
    (@B : BridgeBindable sig) ->
    (&[r] handle : h) ->
    String ->
    IO H.Error (sig captures (r))

let bindModule @H @B handle name =
    H.bindModuleWith B.bridgeContract handle name
```

Rules:

* `BridgeContract sig` is a runtime representation of the foreign
  boundary contract for a value or package surface of type `sig`.
* The type argument `sig` is compile-time only.
  Runtime binding depends on the contract value, not on the erased type
  argument alone.
* `BridgeBindable sig` provides the contract used for runtime binding.
* `bindModuleWith` binds a runtime bridge handle to a foreign module,
  package, namespace, object, or another implementation-documented
  bridge-visible surface selected by the supplied string.
* A successful bind yields an ordinary Kappa value of type
  `sig captures (r)`.
* If `sig` is a signature type or another package-like surface, dotted
  selection from the resulting value uses the ordinary package-member
  rules of §2.8.3.
* A failed bind MUST fail in the surrounding `IO`.
  It MUST NOT silently fabricate a value of type `sig`.
* A conforming implementation MAY provide `BridgeBindable` only for a
  restricted class of record, signature, or other bridge-bindable
  surfaces.
* `BridgeHandle` does not itself specify how a handle is created or
  released.
  Bridge-specific modules provide startup/configuration APIs and SHOULD
  also provide `Releasable` instances for their handle types.

---

<!-- modules.names -->
### 2.8 Names, binding groups, and dotted lookup

Kappa uses one lexical lookup system organized by spelling. A lexical
scope maps each spelling to a binding group. A binding group may carry
several declaration kinds. Declaration kinds control contextual
admissibility; they are not independent shadowing namespaces.

Declaration kinds:

* `term`
* `type`
* `trait`
* `ctor`
* `module`
* `effect-label`

Record field labels are structural labels, not ordinary unqualified
lexical bindings.

Effect-label declarations arise from effect-row syntax, from binders or
package members of compile-time type `EffLabel`, and from the canonical
self labels of `scoped effect` declarations.

Effect-label identifiers are used in `EffRow`-kinded rows, handlers, and
effect-operation selection.

<!-- modules.names.lexical_lookup -->
#### 2.8.1 Ordinary lexical lookup

Unqualified lookup of spelling `x` proceeds by contextual admissibility:

1. Determine the declaration kinds admissible at the use site.
2. Search lexical scopes from innermost to outermost.
3. At each scope, inspect the binding group for spelling `x`, if any.
4. Filter that binding group to declarations whose kinds are admissible
   at the use site.
5. If the filtered set is empty, continue outward.
6. If the filtered set contains exactly one declaration, choose it.
7. If the filtered set contains more than one declaration, apply §2.8.2
   when the ambiguity is solely the same-spelling data-family pairing;
   otherwise the use is ambiguous.

Consequences:

* A nearer binding group containing no admissible declaration does not
  block an outer admissible declaration of the same spelling.
* A nearer admissible declaration shadows outer admissible declarations
  of the same spelling.

Binding-group formation:

* Within one lexical scope, a spelling may contribute at most one
  ordinary declaration of a given kind.
* Distinct kinds with the same spelling may coexist in one binding group.
* A `data` declaration may additionally contribute the same-spelling
  constructor facet described in §2.8.2.

Contextual admissibility:

* term-expression position admits `term`, `ctor`, and `module`;
* type position admits `type`;
* constraint position admits `trait`;
* pattern-head position admits `ctor` and terms declared with `pattern`;
* effect-label position admits `effect-label`.

Receiver positions for dotted forms use §2.8.3 instead of this ordinary
rule.

<!-- modules.names.same_spelling_data_families_static_members -->
#### 2.8.2 Same-spelling data families and static members

A `data` declaration may introduce a type facet and a constructor facet
with the same spelling.

Example:

```kappa
data Person : Type =
    Person (name : String) (age : Int)
```

Rules:

* The type facet and same-spelling constructor facet form one
  same-spelling data-family binding group.
* At an unqualified use site:
  * type positions select the type facet;
  * term-expression positions select the constructor facet;
  * pattern-head positions select the constructor facet.
* Import and export preserve this pairing.
* Distinct-spelling constructors are ordinary `ctor` declarations in
  their enclosing binding groups.
* Constructors are also static members of their enclosing data type for
  dotted selection. Thus `Option.Some`, `Vec.Cons`, and `Person.Person`
  are ordinary static-member selections under §2.8.3.

<!-- modules.names.dotted_name_resolution_dotted -->
#### 2.8.3 Dotted name resolution (`.`)

The tokens `.` and `?.` form left-associative member chains.

The form `lhs.{ ... }` is parsed as a record patch.

Within such a form:

* an item using `=` is an ordinary field update (§5.5.5),
* an item using `:=` is a row-extension field (§5.5.6).

A single `lhs.{ ... }` form may contain either kind or both. Projection-section updates are the `=`-forms described in
§5.5.5. Record-patch forms do not participate in dotted name resolution.

Safe-navigation `lhs?.rhs` desugars before ordinary dotted resolution
runs on the corresponding chain body (§7.1.1.2).

Ordinary dotted resolution is receiver-driven and uses nearest-successful
receiver lookup.

For a dotted chain `base.s1.s2 ... sn`:

1. Search lexical scopes from innermost to outermost for binding groups
   spelled `base`.
2. In each such binding group, consider receiver candidates of kinds:
   * `module`,
   * `type` (including the type facet of a same-spelling data family),
   * `effect-label`,
   * `term`,
   * `ctor`.
3. For each candidate in that binding group, attempt to resolve the full
   remaining chain `.s1.s2 ... sn`.
4. If no candidate in the current binding group yields a well-formed
   full chain, continue outward.
5. If exactly one candidate in the nearest successful binding group
   yields a well-formed full chain, choose it.
6. If more than one candidate in the nearest successful binding group
   yields a well-formed full chain, the dotted form is ambiguous.
7. If no binding group yields a well-formed full chain, the dotted form
   is ill-formed.

After the receiver candidate is fixed, each member step is resolved as
exactly one of:

* module member selection;
* static member selection on a type;
* effect-label operation selection;
* record, package, or dictionary member projection;
* constructor-field projection under positive tag refinement;
* method-call or receiver-projection sugar.

Method-call and receiver-projection sugar are attempted only if no
ordinary member-selection form succeeds for that member step.

Receiver forms:

* If the receiver denotes a module declaration or reified module value,
  `recv.name` selects the exported member `name` of that module.
* If the receiver denotes a type or the type facet of a same-spelling
  data family, `recv.name` selects a static member of that type. For
  data types, constructors are static members.
* If the receiver denotes an effect label, `recv.name` selects an
  operation declared by the effect interface carried at that label in
  the current effect row.
* If the receiver denotes a record, package, or explicit dictionary
  value, `recv.name` selects an ordinary member of that value.
* If the local refinement context proves that the receiver has
  constructor `C` and `C` declares a named field `name`, `recv.name` is
  constructor-field projection.
* Otherwise, fallback sugar of §2.8.4 may apply.

<!-- modules.names.method_call_receiver_projection_sugar -->
#### 2.8.4 Method-call and receiver-projection sugar

Method-call and receiver-projection sugar are fallback forms. A member
step `recv.name` becomes fallback sugar only after ordinary member
selection for that member step has failed.

Eligibility:

* Resolve spelling `name` by the ordinary lexical rule of §2.8.1 in
  term-expression position.
* Let `G` be the nearest binding group containing at least one
  admissible term named `name`.
* From `G`, keep only:
  * ordinary callable terms whose elaborated type has exactly one
    explicit receiver-marked binder; and
  * projection definitions whose declaration has exactly one
    receiver-marked `place` binder.
* If exactly one eligible callable remains, use it.
* If more than one eligible callable remains, the member step is
  ambiguous.
* If none remain, the dotted form is ill-formed.

Elaboration:

* If the chosen callable is an ordinary term `f`, `recv.name` elaborates
  by inserting `recv` at `f`'s unique receiver-marked explicit binder.
* If additional application arguments follow in the same maximal
  application site, the whole site is elaborated as one direct call to
  `f` with `recv` inserted at that binder position.
* If the chosen callable is a projection definition, the same rule
  applies using its unique receiver-marked `place` binder, and the
  result is then treated as an ordinary fully applied projection call
  for §§5.1.7.2 and 8.8.
* The inserted receiver must typecheck against the chosen receiver
  binder after solving any preceding implicit or explicit binders by
  ordinary application-site elaboration.

<!-- modules.names.module_declarations_reified_module_values -->
#### 2.8.5 Module declarations and reified module values

An `import M` or `import M as A` introduces a lexical declaration of kind
`module` with spelling `M` or `A`.

Rules:

* A module declaration participates in ordinary lexical lookup and in
  the dotted receiver lookup of §2.8.3.
* A module declaration may also be used as a pure module value where a
  term expression is expected.
* Ordinary qualified access and projection from a reified module value
  must agree: `M.x` and, after `let m = M`, `m.x` denote the same member
  whenever both are well-formed.

Additional rule:

* A bridge-bound foreign surface returned at runtime under §17.7.7 is not
  a declaration of kind `module` merely because it supports dotted
  member access.
  It is an ordinary value, typically a package value, and dotted access
  on it is governed by the ordinary receiver rules of §2.8.3.



<!-- lexical -->
## 3. Lexical Structure

<!-- lexical.identifiers -->
### 3.1 Identifiers

* Standard identifiers:

  ```text
  [A-Za-z_][A-Za-z0-9_]*
  ```

* Backtick-quoted identifiers (for reserved words or weird names):

  ```kappa
  let `class` = 1
  let `λ` = 42
  ```

Backtick identifiers are ordinary identifiers and participate in the
lexical lookup rules of §2.8.

Grammar amendment:

```text
modSeg  ::= ident | backtick_ident
modPath ::= modSeg ('.' modSeg)*
```

In a source-written module path, a backticked segment contributes its unquoted text as the module-name segment.

Path-derived module names remain constrained by §2.1. In particular, a package-mode module header must still match the
path-derived module name exactly, so a path-derived segment never requires backticks.

<!-- lexical.keywords -->
### 3.2 Keywords

Non-exhaustive but important list of soft keywords and contextual reserved tokens:

```text
let, let?, in, if, then, elif, else,
match, case, is, impossible,
try, except, finally,
data, type, trait, module,
import, export, as, except,
do, block, return, open,
forall, exists,
captures, thunk, lazy, force,
assertTotal, decreases, structural, expect, instance, derive, effect, handle, deep, with, scoped, pattern,
projection, place,
infix, postfix, prefix, left, right,
var, while, break, continue, using, inout,
yield, for, for?, group, by, distinct, order, skip, take, top, join, left, asc, desc,
public, private, opaque, seal, unhide, clarify,
?., ?:
```

Effect-row surface syntax (§5.3.2) introduces no additional keywords; it uses reserved punctuation tokens instead.

Keywords are **soft** (contextual) keywords:

`captures` is a soft keyword used only in type positions for the capture-annotation form of §5.1.6.1.

* The lexer recognizes the keyword tokens, but implementations must permit their use as ordinary identifiers in contexts
  where a keyword is not syntactically expected.
* Example: `let type = 42` is permitted (where `type` is a term name), while `type Foo = ...` uses `type` as a keyword.

<!-- lexical.comments -->
### 3.3 Comments

* Line comments:

  ```kappa
  -- this is a comment
  ```

* Block comments:

  ```kappa
  {- this is
     a block comment -}
  ```

Comments can nest, i.e. block comments may contain other block comments.

<!-- lexical.whitespace_indentation_continuation -->
### 3.4 Whitespace, indentation, and continuation

Kappa uses a Python-style significant indentation rule.

Lexical model:

* The lexer emits `NEWLINE`, `INDENT`, and `DEDENT` tokens.
* Indentation is measured in spaces. Tabs are a compile-time error. Implementations MUST NOT offer a flag that silently
  converts tabs to spaces. A diagnostic MUST point to the first tab character.
* Inside `()`, `[]`, `{}`, `{| |}`, and `<[ ]>`, the lexer does not emit `INDENT`/`DEDENT` (or the parser may
  intentionally ignore those). Newlines inside these delimiters are still emitted. Specific syntactic forms (notably
  comprehensions, §10) may treat `NEWLINE` as a clause separator.

Blank lines and comment-only lines:
* Blank lines and comment-only lines do not affect indentation and do not produce `INDENT`/`DEDENT` changes.

Statement boundaries:

* At base indentation level, `NEWLINE` terminates a statement unless the parser is in a continuation context.

Continuation contexts:

A `NEWLINE` followed by an `INDENT` continues the current syntactic construct when the `NEWLINE` occurs immediately
after a token that syntactically requires a following expression, including (non-exhaustive):

* `=`, `->`, `then`, `elif`, `else`, `in`
* the introducers `do`, `match`, `try`, `handle`, `case`, `except`, `finally`
* after `:` when parsing type annotations or map entries
* after an operator token in infix position

In these cases, the indented lines form a continuation of the expression rather than starting a new statement.

Block introducers:

Certain keywords introduce blocks, meaning they require one or more statements at a greater indentation level,
including:

* `do`
* `block`
* `match` (its `case` clauses)
* `handle` (its `case` clauses)
* `try` / `try match` (its `except` and optional `finally`)
* `trait`, `data`, `effect`, and local `let ... in` bindings
* comprehension clause blocks (when written vertically)

All lines belonging to the same block must share the same indentation level (modulo continuation contexts).

Trailing commas:
* Wherever the grammar admits a comma-separated list (e.g. tuples, record fields, import/export item lists), an optional
  trailing comma is permitted.

<!-- lexical.operator_identifiers_fixity -->
### 3.5 Operator identifiers and fixity

Kappa supports symbolic operator identifiers (e.g. `+`, `*`, `==`, `..`), which are ordinary `term` declarations.

<!-- lexical.operator_identifiers_fixity.operator_tokens -->
#### 3.5.1 Operator tokens

An operator token is an implementation-defined sequence of non-alphanumeric, non-whitespace characters not otherwise
forming a reserved token (such as `->`, string delimiters, comment delimiters, etc.). Common operator tokens include
`+`, `*`, `==`, `&&`, `||`, `..`, `..<`.

Operators may be used as ordinary function names by parenthesizing:

```kappa
let add = (+)
let x = (+) 1 2
```

Special rule for `(-)`:

* When `(-)` occurs with no left operand, prefix fixity applies.
* When `(-)` occurs between two expressions, infix fixity applies.
* A parenthesized `(-)` alone denotes the prefix function.

<!-- lexical.operator_identifiers_fixity.operator_tokens.operator_sections -->
##### 3.5.1.1 Operator sections

A parenthesized expression containing a single binary operator and a single operand denotes an operator section,
following the Haskell 2010 convention:

```kappa
(op e)    -- right section: (+ 1) ≡ \__x -> __x + 1
(e op)    -- left section:  (1 +) ≡ \__x -> 1 + __x
(op)      -- bare operator: the curried function
```

Normative desugaring:

* `(op e)` (right section) desugars to `\__x -> __x op e`, equivalently `flip (op) e`.
* `(e op)` (left section) desugars to `\__x -> e op __x`, equivalently `(op) e`.
* `(op)` remains the curried operator function.

In these desugarings, `__x` denotes a fresh binder not equal to any user-written name in scope.

Exception for prefixes and suffixes:

* `(op e)` is parsed as unary prefix operator application if a matching `prefix` fixity for `op` is in scope at that
  occurrence; otherwise, if a matching infix fixity is in scope, it is a right section.
* `(e op)` is parsed as unary postfix operator application if a matching `postfix` fixity for `op` is in scope at that
  occurrence; otherwise, if a matching infix fixity is in scope, it is a left section.

Precedence rule for section disambiguation:

* When resolving `(op e)` as a right section or `(e op)` as a left section, the operand `e` is parsed with a precedence
  floor of `prec(op) + 1`.
* If a matching `prefix` or `postfix` fixity applies at that occurrence, the form is parsed instead as unary application
  rather than as a section.
* Any lower- or equal-precedence operator usage within `e` must therefore be parenthesized explicitly when the form is
  intended as a section.

Reserved punctuation tokens: The following tokens are reserved by the surface syntax and are not operator tokens:
* `->`, `<-`
* `=`, `:`
* `.`, `@`, `~`
* `<[`, `]>`
* `?.`, `?:`
* `|` (reserved for variant types and or-patterns)
* comment and block-comment delimiters (`--`, `{-`, `-}`)
* string and character delimiters

Token recognition uses longest-match-first.

* `?.`, `?:`, `let?`, and `for?` are recognized as single tokens in preference to their single-character / bare-keyword
  decompositions, regardless of user fixity declarations for `?`.
* After `?.` and `?:` are considered, a `?` immediately followed by an identifier start is recognized as a named-hole
  token.
* `~=` is recognized as an ordinary operator token in preference to standalone `~`, so the `Equiv` operator remains
  available even though `~` itself is reserved for `inout` call sites (§8.8).
* `<[` and `]>` are recognized as single reserved tokens in preference to `<` plus `[` and `]` plus `>`.
* In particular, `?.` is recognized before `?` plus `.`, `?:` is recognized before `?` plus `:`, and a standalone `?`
  remains available as an ordinary operator token (for example, a user-defined postfix operator per §3.5.2).

<!-- lexical.operator_identifiers_fixity.declarations -->
#### 3.5.2 Fixity declarations

Fixity declarations control parsing of operator use.

Supported forms:

```kappa
-- infix (non-associative)
infix 50 (==)

-- infix left/right associativity
infix left  60 (+)
infix right 40 (::)

-- prefix and postfix
prefix  80 (-)
postfix 90 (?)
```

Rules:

* A fixity declaration binds a precedence level (an integer; recommended range `0..100`), and a kind (infix, infix left,
  infix right, prefix, postfix) to one operator token.
* Fixities are **block-scoped** and apply from the point of declaration onward.
* A top-level fixity declaration for operator `op` is exported iff the name `op` is exported under §2.5. The exported
  fixity for `op` is the fixity that is in scope at the end of the module, i.e. the last top-level fixity declaration
  for `op`.
* Exported fixity declarations are imported only together with the corresponding operator name. An unqualified fixity
  enters scope only if that operator itself is imported unqualified. Merely referencing a module interface, or importing
  unrelated names from that module, does not bring other unqualified operator fixities into scope. Imported fixities
  take effect from the point of the import onward.
* Module-only imports do not dump unrelated unqualified fixities into scope.
* If the language later adds qualified operator syntax, qualified fixities must be defined separately for that syntax;
  ordinary unqualified fixity import does not imply any qualified parsing rule.
* Fixity declarations apply when parsing operator tokens in both term expressions and type expressions wherever operator
  parsing is supported by the grammar. (They do not change the meaning of reserved punctuation used by other syntactic
  forms, such as ->.)


<!-- lexical.operator_identifiers_fixity.infix_gating -->
#### 3.5.3 Infix gating

An operator token may only be used in an operator position if a matching fixity is in scope at that source location:

* infix position requires an `infix...` fixity in scope
* prefix position requires a `prefix` fixity in scope
* postfix position requires a `postfix` fixity in scope
* operator sections `(op e)` and `(e op)` require a matching infix fixity in scope, except where §3.5.1.1 specifies that
  the form is taken instead as unary prefix or postfix application

The reserved tokens `?.` and `?:` are always parsed with their built-in precedence and associativity (tightest
left-associative for `?.`; precedence `2`, right-associative for `?:`), even if a user fixity declaration for the bare
`?` token is in scope. The bare `?` token remains available for user-defined operators.

If no fixity is in scope, the operator may still be used as a normal name via `(op)`.


---

<!-- literals -->
## 4. Literals

<!-- literals.numeric_literals -->
### 4.1 Numeric literals

<!-- literals.numeric_literals.integers -->
#### 4.1.1 Integers

Forms:

* Decimal: `0`, `1`, `42`, `9_223_372_036_854_775_807`
* Hex: `0xFF`, `0xDEAD_BEEF`
* Octal: `0o755`, `0o1_2_3`
* Binary: `0b1010`, `0b1_0_1_0`

Underscores `_` are allowed between digits for readability; they have no semantic effect.

<!-- literals.numeric_literals.floating_point_float -->
#### 4.1.2 Floating-point (`Float`)

Decimal floating-point numbers:

* `3.14`
* `10.2e-2`
* `6.022_140_857E23`

General form:

* `[digits] '.' [digits] [exponent]`
* `[digits] [exponent]` where exponent is `e` or `E` followed by an optional sign and digits.

<!-- literals.numeric_literals.float_double_semantics -->
#### 4.1.3 `Float` / `Double` semantics

`Double` is IEEE-754 binary64 (a.k.a. "double precision"). `Float` is the standard prelude alias of `Double`, so the
semantics below apply equally to both names.

* Ordering on `Float` is a **total order** (no NaN weirdness leaking into `Ord`-like operations).
  * Intuitively, it follows IEEE-754 totalOrder semantics: all values are comparable, including NaNs.
* The default equality for `Float` (including the default `Eq Float` instance) is **raw-bit equality**:
  * two floats are equal iff their IEEE-754 bit patterns are equal.
  * This means (not exhaustively)
    * `+0.0` is not equal to `-0.0`
    * `NaN == NaN` may be `True` if the payload bits are identical
* The canonical `Eq Float` instance uses raw-bit equality. Consequently, propositional equality on `Float` in user code
  coincides with equality of the underlying IEEE-754 bit pattern, not IEEE numeric equality.
* `floatEq : Float -> Float -> Bool` is provided for IEEE "numeric" equality (the common `==` in many languages), where
  `NaN` is never equal and `+0.0` equals `-0.0`.

<!-- literals.numeric_literals.sign -->
#### 4.1.4 Sign

`-` is a **unary operator**, not part of the literal.

* `-123` is parsed as `negate 123`
* `-3.14e2` is `negate (3.14e2)`

Operator precedence rules determine association (see expressions).

<!-- literals.numeric_literals.typing -->
#### 4.1.5 Literal typing (normative)

This section applies to numeric literals **without** a suffix. If a numeric literal has a suffix, §4.1.6 takes priority
instead.

**Integer literals (without suffix)**: An integer literal `n` has elaborated type `T` where `T` is a type for which an
instance `FromInteger T` is in scope (§2.6.2). The literal elaborates to `FromInteger.fromInteger @T n` where `n :
Integer` is the integer value denoted by the literal.

The trait is (conceptually):
```
trait FromInteger (t : Type) =
  fromInteger : Integer -> t
```
where `Integer` is the ordinary arbitrary-precision integer type exported by `std.prelude`.

Float literals (without suffix): Analogous: `FromFloat`, with `fromFloat : Double -> t`, where `Double` is the ordinary
IEEE-754 binary64 floating-point type exported by `std.prelude`.

String literals (non-prefixed): Analogous: `FromString`, with `fromString : String -> t`. `String` itself has an
identity `FromString` instance.

Char literals: Char literals have type `Char`.

Defaulting: If elaboration cannot determine a unique `T` for a numeric literal from context, the compiler defaults `T`
as follows:
- integer literal → `Int`
- float literal   → `Float`
- string literal  → `String`

This default is applied after all type inference completes on the enclosing declaration; it is a compile-time error if
defaulting would cause an otherwise-satisfied constraint to become unsatisfiable.

<!-- literals.numeric_literals.numeric_literal_suffixes -->
#### 4.1.6 Numeric literal suffixes

After a numeric literal, an identifier suffix with no intervening whitespace is permitted:

```text
numeric_literal ::= digits [ '.' digits ] [ exponent ] [ suffix ]
suffix          ::= identifier
```

Suffix priority and desugaring:

If a numeric literal has a suffix `suf`, elaboration is `suf litVal`, where `litVal : Integer` for integer literals and
`litVal : Double` for float literals. The suffix path takes priority over the `FromInteger` / `FromFloat` elaboration of
§4.1.5.

* `12px` elaborates to `px n`, where `n : Integer` is the value denoted by the literal `12`.
* `3.14rad` elaborates to `rad d`, where `d : Double` is the value denoted by the literal `3.14`.
* Hex, octal, and binary integer literals may also take suffixes, e.g. `0xFFu32` and `0b1010nat`, with the same
  integer-literal desugaring.

Resolution rules:

* The suffix identifier MUST resolve to a term in scope at the use site.
* For an integer literal suffix, that term MUST have a function type whose argument type is `Integer`.
* For a float literal suffix, that term MUST have a function type whose argument type is `Double`.
* If the user prefers typeclass-mediated construction, they may define a term such as `px = FromInteger.fromInteger
  @Length`; then `12px` works by ordinary name resolution.
* If the suffix identifier is not in scope, compilation fails with a compile-time error that identifies the missing
  suffix name.
* If the suffix name resolves but does not have a compatible function type, compilation fails with a type error at the
  literal.

Suffix-qualified literals are not subject to the `FromInteger` / `FromFloat` defaulting rule of §4.1.5. The suffix
determines the result type.

<!-- literals.boolean_literals -->
### 4.2 Boolean literals

* `True`
* `False`

<!-- literals.string_literals -->
### 4.3 String literals

<!-- literals.string_literals.basic_strings -->
#### 4.3.1 Basic strings

* Double-quoted:

  ```kappa
  "hello"
  "hello\nworld"
  "hello \"world\""
  ```

Common escapes:

* `\n`, `\t`, `\r`, `\\`, `\"`, `\b`, `\uXXXX` etc. Exact set is implementation-defined but should be Unicode-aware.

Single-quoted literals are reserved for `Char` literals (§4.4). Single-quoted **string** literals are not valid in v0.1.

<!-- literals.string_literals.raw_strings -->
#### 4.3.2 Raw strings

Kappa provides a true lexical raw-string form. Raw strings do not process escape sequences and do not treat `$name` or
`${...}` as interpolation syntax.

Surface forms:

```kappa
#"C:\tmp\file.txt"#
##"she said "hello""##
#"\d+\s+\w+"#
```

Grammar (schematic):

```text
hashes           ::= '#' | '##' | '###' | ...
rawStringLiteral ::= hashes '"' rawChar* '"' hashes
```

Rules:

* The number of trailing `#` characters must exactly match the number of leading `#` characters.
* The content of a raw string is the literal source text between the delimiters, with no escape decoding.
* A backslash has no special meaning in a raw string.
* A dollar sign has no special meaning in a raw string.
* The earliest closing delimiter with the matching number of `#` characters terminates the raw string.

<!-- literals.string_literals.multiline_strings_fixed_dedent -->
#### 4.3.3 Multiline strings and fixed dedent

Triple-quoted forms:

```kappa
let s : String = """
    This is a multiline string.
    Indentation is stripped by a fixed rule.
"""

let raw : String = #"""
    This is a raw multiline string.
    Backslashes stay literal: \n
"""#
```

Multiline forms exist for both ordinary and raw strings. The raw multiline form uses the same hash-count matching rule
as §4.3.2:

```text
multilineStringLiteral    ::= '"""' multiBody '"""'
rawMultilineStringLiteral ::= hashes '"""' multiBody '"""' hashes
```

Normative content rule:

* The content of a multiline literal is first taken as the characters between the opening and closing delimiters.
* If the first character of that content is a newline, that newline is removed.
* Let `I` be the exact sequence of leading spaces on the line containing the closing delimiter, immediately before that
  delimiter.
* For each remaining content line:
  * if the line is empty, it remains empty;
  * if the line contains only spaces, remove up to `|I|` leading spaces;
  * otherwise, the line MUST begin with `I`; that prefix is removed.
* If a non-blank content line does not begin with `I`, the literal is ill-formed.
* This fixed dedent step occurs before ordinary escape decoding and before prefixed-string interpolation processing.
* No other indentation normalization is permitted.

This makes the closing-delimiter line the single source of truth for dedent.

<!-- literals.string_literals.prefixed_strings -->
#### 4.3.4 Prefixed strings

A **prefixed string literal** has the form:

```kappa
prefix"..."
prefix"""..."""
prefix#"..."#
prefix#"""..."""#
```

Where `prefix` is an identifier (including backtick identifiers, and including soft keywords used as ordinary
identifiers per §3.2) immediately followed by the opening delimiter, with no intervening whitespace.
It is resolved by ordinary term name resolution at the use site; it has no special parser status beyond introducing
this literal form.

Let the **hash count** of the literal be the number of `#` characters in its delimiter.

* An ordinary prefixed string has hash count `0`.
* A raw prefixed string has hash count `n > 0` and uses the same delimiter-matching and multiline-dedent rules as
  §§4.3.2-4.3.3.

Prefixed strings elaborate through one uniform fragment pipeline. There are no built-in special cases for formatting,
SQL, regex, bytes, or any other DSL.

<!-- literals.string_literals.prefixed_strings.interpolation_syntax -->
##### Interpolation syntax

Ordinary prefixed strings (hash count `0`) support:

* `$name`, which is sugar for `${name}`
* `${expr}`
* `${expr : fmt}`

A literal `$` in an ordinary prefixed string may be written as `\$`.

Raw prefixed strings (hash count `n > 0`) support interpolation using an opener with the **same hash count**:

* when `n = 1`, interpolation uses `#{expr}` and `#{expr : fmt}`
* when `n = 2`, interpolation uses `##{expr}` and `##{expr : fmt}`
* in general, interpolation uses `#...#{expr}` and `#...#{expr : fmt}` with exactly `n` `#` characters before `{`

Rules:

* `$name` sugar is recognized only in ordinary prefixed strings.
* In a raw prefixed string, `$` has no special meaning.
* In a raw prefixed string with hash count `n`, a `#`-sequence begins interpolation only when it is exactly `n`
  `#` characters immediately followed by `{`.
* Any shorter or longer `#`-sequence is ordinary literal text.
* To include the interpolation opener literally in raw text, choose a larger hash count for the surrounding literal.

Examples:

```kappa
f"hello $name"
sql#"select * from users where id = #{userId:param}"#
re##"\b##{word}\b"##
type#"Vec #{n} Int"#
```

<!-- literals.string_literals.prefixed_strings.interpolation_parsing -->
##### Interpolation parsing

The expression part of an interpolation is parsed using ordinary Kappa expression parsing.

Nested parentheses, brackets, braces, strings, character literals, comments, and syntax quotes inside the interpolation
expression are handled exactly as in ordinary source code.

A top-level `:` inside an interpolation begins a format specifier.

Here "top-level" means not nested inside parentheses, brackets, braces, strings, character literals, comments, or
syntax quotes inside that interpolation.

Consequently:

* `${expr : fmt}` and `#{expr : fmt}` attach the format specifier `fmt` to `expr`.
* If the interpolation expression itself requires a top-level `:`, that expression MUST be parenthesized.

The format specifier is the exact source text between that top-level `:` and the closing interpolation delimiter, after:

* multiline dedent of §4.3.3, and
* removing one optional ASCII space immediately after `:` and one optional ASCII space immediately before the closing
  interpolation delimiter.

The compiler does not interpret the contents of the format specifier.

<!-- literals.string_literals.prefixed_strings.resolution_typing -->
##### Resolution and typing

* Let `prefix` resolve to a term `p` in scope at the use site.
* Any prefixed string literal requires `p` to resolve to a term of type `Dict (InterpolatedMacro t)` for some result
  type `t`.
* If resolution fails, or if the resolved term does not have a compatible type, compilation fails with a compile-time
  error.

The prelude provides:

```kappa
data SyntaxFragment : Type =
    Lit       (s : String)
    Interp    (@t : Type) (e : Syntax t)
    InterpFmt (@t : Type) (e : Syntax t) (fmt : String)

trait InterpolatedMacro (t : Type) =
    buildInterpolated : List SyntaxFragment -> Syntax t
```

<!-- literals.string_literals.prefixed_strings.fragment_construction_rules -->
##### Fragment construction rules

* Literal segments are formed after applying the multiline dedent rule of §4.3.3.
* In ordinary prefixed strings, literal segments are then decoded using the ordinary string escape rules of §4.3.1.
* In raw prefixed strings, literal segments are taken verbatim after dedent, with no escape decoding.
* Interpolation expressions are elaborated and quoted hygienically as `Syntax` values in the lexical environment of the
  literal occurrence.
* The fragment list passed to `buildInterpolated` MUST preserve source order.
* Adjacent `Lit` fragments MUST be merged.
* Empty `Lit` fragments MUST be omitted, except that a completely empty prefixed string is represented as `[Lit ""]`.

<!-- literals.string_literals.prefixed_strings.semantics -->
##### Semantics

A non-interpolated prefixed string:

```kappa
p"raw"
```

elaborates to:

```kappa
$(p.buildInterpolated [Lit "raw"])
```

An interpolated prefixed string:

```kappa
p"pre${x}post"
```

elaborates to:

```kappa
$(p.buildInterpolated [Lit "pre", Interp @Tx '{ x }, Lit "post"])
```

where `Tx` is the inferred type of `x`.

A formatted interpolation:

```kappa
p"pre${x : %04d}post"
```

elaborates to:

```kappa
$(p.buildInterpolated [Lit "pre", InterpFmt @Tx '{ x } "%04d", Lit "post"])
```

A raw interpolated prefixed string:

```kappa
p#"pre#{x}post"#
```

elaborates to:

```kappa
$(p.buildInterpolated [Lit "pre", Interp @Tx '{ x }, Lit "post"])
```

A raw formatted interpolation:

```kappa
p#"pre#{x : ident}post"#
```

elaborates to:

```kappa
$(p.buildInterpolated [Lit "pre", InterpFmt @Tx '{ x } "ident", Lit "post"])
```

There is no implementation-defined rewrite of formatted interpolation.
Any meaning of the format specifier is determined entirely by `p.buildInterpolated`.

The standard `f`, `re`, `b`, and `type` prefixes supplied by `std.prelude` are ordinary terms participating in this mechanism.
They receive no special parser treatment.

<!-- literals.string_literals.type_prefix_handler -->
#### 4.3.5 Conventional type-prefix handler

There is no built-in `type"..."`
intrinsic.

```kappa
type : Dict (InterpolatedMacro Type)
```

Because keywords are soft (§3.2), `type"..."`, `type"""..."""`, and their raw forms are parsed as prefixed-string
literals whenever a declaration keyword is not syntactically expected.

Accordingly, `type"..."` follows §4.3.4 exactly and elaborates through `buildInterpolated` like any other prefix.

Implementations MAY provide additional ordinary type-producing prefixed-string handlers.

<!-- literals.character_literals_char -->
### 4.4 Character literals (`Char`)

A character literal is a single-quoted Unicode scalar value:

```kappa
'a'
'λ'
'🙂'
'\n'
'\u03BB'
```

Rules:
* A Char literal denotes a value of type Char.
* The literal must contain exactly one Unicode scalar value (one code point excluding surrogates).
  * Grapheme clusters consisting of multiple code points are not a single Char.
* Escape sequences are the same as for strings (at minimum \n, \t, \r, \\, \', \uXXXX, and \u{...} if supported).
* Char ordering and ranges:
  * Char has an Ord-like ordering given by numeric scalar value order.
  * Range enumeration over Char (e.g. 'a' .. 'z') enumerates by increasing scalar value.

<!-- literals.unit_tuples -->
### 4.5 Unit and tuples

* Unit: `()`
  * Unit type:
    * The unit type is `Unit` (provided by the prelude).
    * Canonically, the prelude defines:
      ```kappa
      data Unit : Type =
          Unit
      ```
    * `()` is surface syntax sugar for the unique value `Unit`.
    * In type position, `()` is surface syntax sugar for the type `Unit`.
    * Through type scope, the constructor is accessible as `Unit.Unit`.
    * The zero-field closed structural record is identified with `Unit`; there is no distinct empty-record type or
      empty-record value in v0.1. Accordingly, the would-be empty record written `()` is definitionally equal to `Unit`
      in type position and to the unique `Unit` value in term position.
* Tuple value:
  ```kappa
  (1, "two", 3.0)
  (42,)          -- single-element tuple
  ```
* Tuple type:
  ```kappa
  (Int, String, Float)
  (Int,)         -- single-element tuple type
  ```

`(x)` alone is **grouping**, not a tuple.

---

<!-- types -->
## 5. Types, Universes, and Records

<!-- types.universes -->
### 5.1 Universes

Kappa is dependently typed with a stratified universe hierarchy.

* There is an infinite family of universes: `Type0`, `Type1`, `Type2`, ...
* Universe typing:

    * `Type0 : Type1`
    * `Type1 : Type2`
    * in general, `Type u : Type (u+1)` at the meta-level.

Intrinsic compile-time types:

```text
Universe   : Type0
Quantity   : Type0
Region     : Type0
Constraint : Type0
```

The intrinsic compile-time row and label types `RecRow`, `VarRow`, `EffRow`, `Label`, and `EffLabel` are introduced in
§5.3.1 and each inhabit `Type0`.

`Type u` is a primitive universe family indexed by `u : Universe`. It is not specified as ordinary first-order function
application, even though `Universe` itself is a type.

Surface syntax:

* `Type0`, `Type1`, `Type2`, ... denote fixed universe levels.
* `Type` is **universe-polymorphic** surface syntax:
    * each occurrence of bare `Type` introduces a fresh universe level metavariable (implementation-defined notation,
      e.g. `?u`), meaning "some universe level inferred by the compiler".
* Advanced code may bind an explicit universe variable and reuse it:

  ```kappa
  forall (u : Universe) (a : Type u) (b : Type u). ...
  ```

  In `Type u`, the identifier `u` is a user-bound variable of the intrinsic compile-time type `Universe`. This is the
  surface mechanism for forcing multiple occurrences to live in the same universe level.
* `*` is syntactic sugar for `Type`.

<!-- types.universes.cumulativity -->
#### 5.1.1 Cumulativity

Universes are **cumulative**:

* if `u ≤ v`, then `Type u` may be used where `Type v` is expected.

Implementations may realize this as an implicit coercion/subtyping rule between universes.

<!-- types.universes.inference_sketch -->
#### 5.1.2 Universe inference (sketch)

Elaboration generates constraints on universe metavariables (e.g. `?u ≤ ?v`, `?u < ?v`) from typing. Explicit universe
variables introduced by `forall (u : Universe)` or an ordinary binder `(u : Universe)` participate in the same
constraint solving. Occurrences of `Type u` therefore share exactly the same user-written universe variable rather than
introducing fresh metavariables. If the constraints are unsatisfiable, compilation fails. Unconstrained universe
metavariables may be generalized at top-level (implementation-defined).

<!-- types.universes.constraints_dictionaries -->
#### 5.1.3 Constraints and dictionaries

`Universe`, `Quantity`, `Region`, `Constraint`, `RecRow`, `VarRow`, `EffRow`, `Label`, and `EffLabel` are intrinsic
compile-time types, not extra meta-level sorts.

Terms of these intrinsic compile-time types may be used in ordinary explicit binders, record fields, package members,
projections, existential witnesses, and local bindings wherever the surrounding surface form otherwise permits them.

This permission is normative and unconditional subject only to the ordinary grammar and typing rules of the surrounding
form.

In particular, a record field, package member, local binding, or existential witness MAY have type:

* `Universe`
* `Quantity`
* `Region`
* `Constraint`
* `RecRow`
* `VarRow`
* `EffRow`
* `Label`
* `EffLabel`

The fact that such values are compile-time only affects erasure under §§5.1.4 and 14.4. It does not remove their
ordinary first-class status for binding, projection, packaging, sealing, opening, equality, or storage inside records
and packages.

Examples of well-formed source types therefore include:

```kappa
(ρ : Region, thunk : ((Unit -> Int) captures (ρ)))
(@ρ : Region, q : Quantity, u : Universe)
exists (ρ : Region). ((Unit -> Int) captures (ρ))
```

Packaging or storing a compile-time value such as a `Region` does not by itself discharge any region-escape obligation.
The ordinary skolem-escape rules of §5.1.6 still apply.

This section does not by itself add arbitrary new surface constructors or eliminators for raw inhabitants of `Universe`,
`Quantity`, or `Region`. Surface v0.1 provides only the forms explicitly specified elsewhere. KCore and elaboration,
however, MAY carry such terms as ordinary compile-time bindable values.

Intended use:

* `Universe` classifies universe levels that may appear in explicit forms such as `Type u`.
* `Quantity` classifies exact usage intervals and the borrow mode `&`. It remains reserved for ownership and usage
  accounting only; see §5.1.5.1.
* `Region` classifies explicit borrow lifetimes that may be named in surface types when a borrow relationship must cross
  an interface boundary.
* `Constraint` classifies constraint descriptors such as `Eq Int`, `Monad (IO e)`, or `ContainsRec r l T`.

Constraint descriptors and coherent evidence are distinct:

* A trait declaration `trait Tr ... = ...` introduces a trait constructor `Tr` whose fully applied applications are
  terms of type `Constraint`.
* A concrete constraint descriptor such as `Eq Int`, `Monad (IO e)`, or `ContainsRec r l T` is not coherent evidence
  by itself.
* Concrete constraint descriptors may appear:
  * in implicit binders `(@x : C)`,
  * in constraint arrows `C => T`,
  * in instance heads, and
  * as arguments to built-ins that abstract over constraints.

Built-in reification:

```kappa
Dict : Constraint -> Type
```

`Dict C` is the explicit dictionary type corresponding to the concrete constraint descriptor `C`.

Usage rules:

* A value of a concrete constraint descriptor `C` may only be bound implicitly.
* An explicit parameter, field, or result may not have concrete type `C`; use `Dict C`.
* There is an implicit coercion from coherent evidence `ev : C` to `Dict C`.
* There is no coercion from `Dict C` to `C`.
* `Dict C` never participates in implicit resolution.
* Coherent constraint evidence is proof-irrelevant for typechecking and coherence.
* The runtime representation of constraint evidence is implementation-defined.
* Explicit `Dict C` values are ordinary runtime values unless eliminated by specialization, inlining, or dead-code
  erasure.
* Implicit evidence may be erased when the implementation proves that it is unused after elaboration.
* A binder may range over `Constraint` itself, e.g. `(c : Constraint) -> ...`; this quantifies over constraint
  descriptors and does not introduce coherent evidence for `c`.

<!-- types.universes.erasure_elaboration_time -->
#### 5.1.4 Erasure and elaboration time

Kappa distinguishes computational values from compile-time values.

Compile-time values are:

* inhabitants of the intrinsic compile-time types `Universe`, `Quantity`, `Region`, `Constraint`, `RecRow`, `VarRow`,
  `EffRow`, `Label`, and `EffLabel`;
* universe terms appearing in `Type u`;
* inhabitants of the elaboration-time reflection types `CoreCtx`, `Symbol`, `Core Γ t`, and `CoreEq x y` of §5.8.5;
* inhabitants of the comprehension reflection types `RawComprehension a` and `ComprehensionPlan a` of §5.8.5;
* inhabitants of the elaboration-time action type `Elab a` and of the elaboration-time goal record `ElabGoal` of
  §5.8.7;
* inhabitants of compile-time function spaces built entirely from such types and from `Type u`.

Compile-time values are ordinary terms for binding, projection, packaging, sealing, opening, and definitional equality,
but Kappa does not require implicit runtime reflection over them.

Runtime erasure is governed as follows:

* For computational binders and fields, runtime erasure is governed by quantities (§5.1.5, §14.4), subject to the
  special treatment of coherent constraint evidence and `Dict` values in §5.1.3.
* For compile-time binders, fields, arguments, and package members, erasure is by classifier rather than by written
  quantity: they are compile-time only and are erased unless preserved by an explicit reified runtime carrier such as
  `Dict C` or another representation type supplied by the implementation or libraries.
* Implementations may provide library mechanisms to reify type information or other compile-time information explicitly
  when needed, but there is no implicit runtime reflection.

<!-- types.universes.erasure_elaboration_time.compile_time_bindings_fields -->
##### 5.1.4.1 Compile-time bindings and fields

A binder, record field, or package member is compile-time if its annotation elaborates to one of the intrinsic
compile-time types of §5.1.3, to `Type u`, to one of the elaboration-time reflection types of §5.8.5, to
`RawComprehension a` or `ComprehensionPlan a` of §5.8.5, to `Elab a` or `ElabGoal` of §5.8.7, or to a compile-time
function space built from such types.

Compile-time bindings and fields:

* may carry any quantity annotation otherwise admitted by the surrounding grammar;
* may be named, rebound, projected, packaged, sealed, opened, and returned;
* participate in ordinary source-level typing, definitional equality, hashing, and interface identity; and
* are erased according to §§5.1.4 and 14.4 unless preserved by an explicit reified carrier.

Writing `let y = x` where `x` is compile-time simply binds `y` to the same compile-time value.

Packaging or rebinding a compile-time value does not discharge region escape. If a compile-time value mentions a fresh
anonymous rigid region introduced by a local borrow, the ordinary skolem-escape rule of §5.1.6 still applies.

<!-- types.universes.quantities -->
#### 5.1.5 Quantities

A quantity denotes either a closed interval of permitted exact uses over `ℕ∞`, where `∞` is allowed as an upper bound,
or the special borrow quantity `&`.

Surface quantities in v0.1 are:

```text
0      ≜ [0,0]     -- Erased (computationally irrelevant)
1      ≜ [1,1]     -- Owned (exactly once, consumes the resource)
&                  -- Borrowed (computationally relevant, non-consuming)
ω      ≜ [0,∞]     -- Unrestricted (zero or more times)
<=1    ≜ [0,1]     -- At most once
>=1    ≜ [1,∞]     -- At least once
```

Implementations may use arbitrary closed intervals internally during inference for interval quantities, but user-written
quantity syntax in v0.1 is limited to the six forms above. The borrow quantity `&` is a distinct mode and is not itself
an interval.

Quantity variables and polymorphism:

A function may be abstract over its quantity obligations by quantifying over the `Quantity` sort.

Syntax:

```kappa
forall (q : Quantity). T
```

Within the scope of `q`, it may be used as the quantity annotation on a binder.

Example:

```kappa
apply : forall (q : Quantity) (a : Type) (b : Type).
        (q fn : a -> b) -> (q x : a) -> b
```

Capability vs demand:

For interval quantities `q1` and `q2`, `q1 ⊆ q2` means the usage permitted by `q1` is contained within the usage
permitted by `q2`.

A capability `q_cap` satisfies a demand `q_dem` (written `q_cap ⊑ q_dem`) by the following exhaustive rules:

* If `q_cap` and `q_dem` are both interval quantities, then `q_cap ⊑ q_dem` iff `q_dem ⊆ q_cap`.
* `& ⊑ &`.
* `& ⊑ ω`.
* All other cases involving `&` are false.

These rules govern quantity satisfaction only. They do not themselves introduce a borrow. Borrow introduction for
contexts that demand `&` is specified separately below.

The symbol `⊑` is used instead of `≤` to avoid confusion with numeric or subset ordering.

For the six surface quantities of v0.1, this yields the following complete table:

```text
q_cap ⊑ q_dem |  0   1   <=1  >=1   ω    &
--------------+------------------------------
0             | yes no  no   no    no   no
1             | no  yes no   no    no   no
<=1           | yes yes yes  no    no   no
>=1           | no  yes no   yes   no   no
ω             | yes yes yes  yes   yes  no
&             | no  no  no   no    yes  yes
```

Equivalently:

* `1 ⋢ &` and `& ⋢ 1`: owned and borrowed are distinct obligations. An owned resource cannot be permanently coerced into
  a borrow, and a borrow cannot satisfy a consuming `@1` obligation.
* `ω ⋢ &`, `<=1 ⋢ &`, `>=1 ⋢ &`, and `0 ⋢ &`: an interval capability does not satisfy a borrowed demand.

Examples:

* `ω ⊑ 1`, because `[1,1] ⊆ [0,∞]`
* `ω ⊑ <=1`, because `[0,1] ⊆ [0,∞]`
* `>=1 ⊑ 1`, because `[1,1] ⊆ [1,∞]`
* `ω ⊑ >=1`, because `[1,∞] ⊆ [0,∞]`
* `& ⊑ ω`
* `& ⊑ &`
* `1 ⋢ &`
* `& ⋢ 1`

Addition, multiplication, and control-flow join are defined on interval quantities:

`[a,b] + [c,d] = [a+c, b+d]`

`[a,b] · [c,d] = [a·c, b·d]`

`[a,b] ⊔ [c,d] = [min(a,c), max(b,d)]`

Default quantity when omitted: `ω`.

Quantities on compile-time binders and fields:

* A quantity annotation may appear on a binder or field whose annotation elaborates to a compile-time type in the sense
  of §5.1.4.1.
* Such a quantity is not rejected merely because the underlying value is compile-time only.
* Erasure of the underlying bound value is still governed by §§5.1.4 and 14.4 rather than by the written quantity alone.

Typing discipline:

* Elaboration infers a usage interval for ordinary binders and a borrow obligation for `&`-annotated binders.
* A binder annotated with an interval quantity `q` is accepted iff the inferred demand interval is contained within the
  declared demand `q`.
* A binder annotated with `&` is accepted iff all uses are non-consuming and the binder does not escape the borrow scope
  described in §5.1.6.
* Sequential composition adds interval usages.
* Alternative control-flow paths merge interval usages with `⊔`.
* Pattern components add usages structurally.
* Or-pattern alternatives merge binder usages with `⊔`.

Borrow binders may optionally name a region variable already in scope:

```kappa
(&[s] x : T)
(@&[s] x : T)
```

where `s : Region`.

A bare borrowed binder `(& x : T)` or `(@& x : T)` introduces a fresh anonymous rigid region local to the enclosing
scope. The explicit forms are needed only when a borrow relationship must cross an interface boundary such as a
top-level export.

Borrow introduction at borrow-demanding positions:

If a context demands borrowed quantity (`(& x : T)` for an explicit binder, or `(@& x : T)` for an implicit binder), and
the supplied expression is a borrowable place expression (§5.1.7.2) of type `T`, the compiler may insert a temporary
borrow.

This is a separate elaboration rule. It does not modify the quantity-satisfaction relation `⊑`.

Borrowable stable expressions are expressions that elaborate to stable places under §5.1.7.1, together with any fresh
hidden temporaries introduced by elaboration.

In v0.1 this includes at least:

* variables;
* record projections from stable places;
* constructor-field projections from stable places; and
* fresh hidden temporaries introduced by elaboration.

At function application, this rule behaves as follows:

* If the borrowed argument is rooted at a value currently held at quantity `@1`, the callee is invoked with a borrow of
  that root, the caller's `@1` obligation is suspended for the duration of the call, and that obligation is restored
  when the call returns.
* If the borrowed argument is rooted at a value currently held at quantity `<=1`, `>=1`, or `ω`, the compiler inserts a
  temporary read-only borrow of that stable root for the duration of the call. No permanent quantity coercion is
  performed.

For projections:

* If the borrowed argument is a record projection path such as `r.f` or `r.f1.f2`, the temporary borrow applies to that
  projected path rather than automatically to the entire root record `r`, subject to the disjoint-path rules of §5.1.7.
* If the borrowed argument is a constructor-field projection from a borrowable root, the temporary borrow applies to
  that projected field path.
* If the borrowed argument is a fully applied projection call under §6.1.1, the temporary borrow applies to the
  dynamically selected yielded stable place. Static overlap and admissibility use the static footprint summary of
  §5.1.7.2.
* If the compiler cannot identify a stable borrowable root and projection path for the argument expression, it MAY
  conservatively reject borrow introduction for that expression.

Typing rule (non-escape of borrowed parameters):

* If a parameter is `(& x : T)` or `(@& x : T)`, then for typing purposes `x` is assigned a fresh anonymous rigid region
  variable `rho` local to the callee.
* If a parameter is `(&[s] x : T)` or `(@&[s] x : T)`, then `x` is tracked under the already bound explicit region
  variable `s : Region`.
* The borrowed view of `x`, and any value, closure, or reference whose type depends on that borrowed view, is treated as
  mentioning the chosen region.
* The result type of the callee must not mention any fresh anonymous region introduced for that call. It may mention
  only region variables that are already explicitly in scope, such as `s : Region`.

The temporary borrow created by borrow introduction ends exactly when the demanding context finishes. At function
application, this is exactly when the call returns. Consequently a callee may not store such a temporary anonymous
borrow in its result; only explicitly scoped regions may cross that boundary.

<!-- types.universes.quantities.are_ownership -->
##### 5.1.5.1 Quantities are ownership-only

The `Quantity` sort is reserved for ownership and usage accounting.

In particular, quantities do not express information-flow labels, privacy or sensitivity grades, scheduling budgets,
energy or cost annotations, or effect grades. Such analyses, if supported by a future version of Kappa, belong to a
distinct modal/coeffect layer.

Consequences:

* the surface quantity forms of §5.1.5 remain the only user-written quantity forms in v0.1;
* the quantity-satisfaction relation `⊑` ranges only over interval quantities and the borrow mode `&`;
* borrow introduction, borrow lifetimes, and path-sensitive borrowing continue to be governed only by §§5.1.5-5.1.7;
* quantity-governed erasure continues to be governed by §§5.1.3-5.1.7 and §14.4.

<!-- types.universes.quantities.reserved_modal_coeffect_extension_lane -->
##### 5.1.5.2 Reserved modal/coeffect extension lane

A future revision of Kappa MAY standardize one or more graded modal or coeffect systems.
For the purposes of this specification, such a system is called a **modal extension**.

A modal extension MUST be distinct from `Quantity`.

A conforming modal-extension specification MUST define all of the following:

* the source-visible type former(s), computation former(s), binder form(s), or other surface constructs by which the
  extension is written;
* the compile-time classifier type(s), if grades of that extension are user-nameable in source;
* the extension's own admissibility, ordering, combination, and branch-join rules for its grades;
* the finite predicate vocabulary emitted during elaboration;
* whether any solved evidence is compile-time only or has a user-visible runtime carrier;
* the extension-specific interaction, if any, with effect rows, modules, opaqueness, separate compilation, and
  definitional equality; and
* the diagnostic requirements for contradictory or unsolved obligations.

A modal extension MUST NOT:

* add new user-written quantity literals;
* reinterpret `Quantity`, `⊑`, borrow introduction, borrow lifetimes, path-sensitive borrowing, or quantity-governed
  erasure;
* require the core quantity checker to invoke a general-purpose solver for programs that do not use that extension's
  surface forms; or
* redefine effect-row membership, effect-row equality, row weakening, or `SplitEff`.

Orthogonality:

* quantity checking continues to determine only consumption, borrowing, erasure, and path-sensitive ownership;
* effect rows continue to determine only which effects may occur; and
* a modal extension determines only the extension-specific property that it introduces.

Examples of candidate future modalities include information-flow labels, privacy/sensitivity grades, energy or cost
budgets, and scheduling budgets.

v0.1 defines no user-visible modal extension.

User-defined grade algebras:

* A conforming v0.1 implementation MUST NOT expose arbitrary user-defined grade algebras or user-defined solver
  theories as a portable language feature.
* An implementation MAY experiment with such facilities under implementation-defined nonportable flags, but those
  facilities are outside conformance unless and until a later revision standardizes them.

<!-- types.universes.quantities.modal_extension_contract -->
##### 5.1.5.3 Modal extension contract

For any enabled modal extension, elaboration behaves as if each checking judgment returns both:

* the ordinary ownership result `Q`, decided by §§5.1.5-5.1.7; and
* a finite set `Φ` of extension-specific modality predicates.

Operational rules:

* `Q` is decided by the syntax-directed ownership rules and MUST NOT depend on modality solving.
* `Φ` is accumulated during checking and is solved only after body resolution, in `MODAL_SOLVE` (§17.2.2).
* Sequential composition combines quantity usage by the ordinary quantity rules and combines modality obligations by
  conjunction.
* Alternative control-flow paths combine quantity usage by the ordinary join rules. Any modality-specific join or
  compatibility requirement is emitted separately as additional predicates in `Φ`.
* If an extension introduces more than one grade domain or predicate class, those domains remain distinct unless that
  extension explicitly defines a product construction.

Failure conditions:

* contradictory modality obligations are compile-time errors;
* unsolved modality obligations at the end of `MODAL_SOLVE` are compile-time errors; and
* such errors MUST identify at least the primary source origin of each contributing obligation and the modal extension
  responsible for it.

An ordinary source program that uses no surface form of an enabled modal extension MUST typecheck exactly as if that
extension were absent, except for implementation-defined diagnostics or tooling metadata that report the extension as
inactive.

<!-- types.universes.borrow_lifetimes_escape_prevention -->
#### 5.1.6 Borrow lifetimes and escape prevention

A value bound at quantity `&` is tethered to the lexical scope of the underlying stable root from which the borrowed
view was introduced via a fresh rigid region variable `rho` (a skolem constant). If that root was owned at quantity
`@1`, the borrow temporarily suspends that ownership obligation. If the root was held at `<=1`, `>=1`, or `ω`, the
borrow is read-only for the lifetime of `rho`.

Anonymous and explicit regions:

* A bare borrowed binder introduces an anonymous rigid region `rho`. This is the default and remains the ordinary
  local-programming case.
* An explicit borrowed binder `&[s]` uses a user-written region variable `s : Region` already in scope.
* Anonymous regions are internal elaboration artifacts only. Explicit region variables are surface-visible only when a
  borrow dependency must cross an interface boundary.

Hidden-root introduction for borrowed local bindings:

If a borrowed local binding has the form `let & pat = expr` and `expr` is not already a borrowable place expression
(§5.1.7.2), elaboration first introduces a fresh hidden temporary `__tmp` scoped exactly to the body of that local
binding (or, in a `do` block, to the remaining do-items). That hidden temporary becomes the borrow root for `pat`.

Conceptually:

```kappa
let & pat = expr in body
```

behaves as if elaborated to:

```kappa
let 1 __tmp = expr
let & pat = __tmp
in body
```

where `__tmp` is fresh and inaccessible to user code. The rigid region introduced for `pat` is scoped to that same body
or remaining do-sequence.

Region tracking during elaboration:

Every elaborated term carries an implicit region environment: the finite set of rigid region variables mentioned by its
free variables and closure captures. This environment is computed locally during typechecking:

* When a bare `&`-binder is introduced (`(& x : T)` or `using pat <- ...`), a fresh rigid `rho` is created and added to
  the region environment of the bound names.
* When an explicit `&[s]` binder is introduced, the bound names are tracked under the already in-scope region variable
  `s`.
* When a closure `\binders -> body` is elaborated, its region environment is the union of:
  * the region environments of all free variables captured from the surrounding scope, and
  * any regions mentioned in the types of the binders.
* A function type `(q x : A) -> B` is elaborated with the implicit side condition that the arrow is valid only in scopes
  where all regions mentioned by `A`, `B`, and the closure's hidden environment (when the term is a closure value) are
  still live.

Escape rule (skolem escape):

A value, including a closure, whose region environment mentions a rigid `rho` may not escape the lexical scope that
introduced `rho`. Escape points include:

* `return` from the function or lambda that introduced the borrow,
* assignment to a `var` declared in an outer scope,
* passing the value as an argument to a function whose parameter does not itself mention `rho` (that is, the parameter
  is not known to be downward-only with respect to `rho`),
* storing the value inside another data structure or closure that would outlive `rho`.

If elaboration attempts any of the above, it produces a skolem-escape error naming the offending `rho` and the construct
that tried to escape it. This check is performed locally during elaboration of the enclosing block; no whole-program
analysis is required, because `rho` is rigid and its scope is statically known.

The same rule applies to first-class borrowed-view values of type `BorrowView ρ A`.

Such a value may be stored in a record or package, returned from a function, or passed as an argument only when the
resulting type continues to mention `ρ` explicitly, or when `ρ` itself is packaged as an explicit witness. Packaging
does not discharge skolem escape.

Downward-only closures:

A closure that captures only `&` variables whose regions are still live in the current scope may be passed to
higher-order functions, provided those functions' parameters are themselves elaborated with compatible region
environments. The typechecker uses the same style of local region-environment compatibility checking that it already
applies to quantities.

Interaction with `using`:

The `using pat <- acquire` form introduces exactly one rigid `rho` for the entire protected scope. Operationally,
`using` performs a primitive internal split of the acquired owned resource into:

* a hidden exit obligation attached to the current `do`-scope frame and consumed by `Releasable.release` when that scope
  unwinds, and
* the borrowed bindings exposed through `pat` within the protected body.

This split is primitive to `using`; it is not modeled as ordinary closure capture by `defer`. Any closure created inside
that scope that captures names from `pat` inherits `rho` and therefore cannot escape the `using` block, or any
surrounding `do`-scope whose unwinding would have to outlive that borrow. This is what makes `using` sound without
explicit lifetime syntax. If `bracket` is provided, it is sound only when implemented with equivalent protected-scope
machinery rather than a naive `defer (release res)` source-level expansion.

<!-- types.universes.borrow_lifetimes_escape_prevention.capture_annotated_types -->
##### 5.1.6.1 Capture-annotated types

Kappa provides an optional postfix capture annotation on value types:

```text
typeCapture ::= typeApp [ 'captures' '(' regionRef (',' regionRef)* ')' ]
typeArrow   ::= typeCapture ('->' typeArrow)?
regionRef   ::= ident
```

This grammar amends the type grammar at the layer between type application and arrows. `captures (...)` is a postfix
type former.

`T captures (s1, ..., sn)` is well-formed only if each `si` resolves to an explicit binder `si : Region` already in
scope.

Meaning:

* A value of type `T captures (s1, ..., sn)` has ordinary value type `T`.
* In addition, its hidden region environment must be contained in the finite set `{s1, ..., sn}`.
* `captures (...)` is compile-time only and is erased under §§14.4 and 17.4.
* The listed regions form a set:
  * duplicates are a compile-time error;
  * order is not semantically significant; and
  * canonical rendering orders them by first binding occurrence in the surrounding explicit `Region` telescope.
* The annotation may be written on any value type, not only on function types. This is necessary because a non-function
  value may hide closures or other region-carrying values inside records, packages, or abstract data.
* A user-written capture annotation is an upper bound. Elaboration accepts a value at type `T captures (s̄)` only if the
  inferred hidden region environment of that value is contained in `{s̄}`.
* When `captures (...)` is omitted in source, elaboration may infer the minimal capture set required for the value.
* Anonymous rigid regions introduced locally are never written in a capture annotation. If a value would require an
  anonymous rigid region to appear in such an annotation, the skolem-escape rule rejects that value instead.

Precedence:

* `captures (...)` binds looser than type application and tighter than `->`.
* Therefore `Boxed a captures (s)` means `(Boxed a) captures (s)`.
* To annotate a function value itself rather than only its codomain, parentheses are required:
  `((A -> B) captures (s))`.

Structural capture inference:

The inferred hidden region environment of an elaborated value is defined structurally.

* A variable, package-member projection, or other neutral value inherits the capture annotation already present in its
  elaborated type.
* A lambda or local function value has the capture set inferred from its hidden region environment under §7.2.1.
* A handler-bound resumption value introduced by §§8.1.9-8.1.10 has the capture set of the captured continuation
  boundary of §14.8.5, restricted to explicit region variables that may appear in surface types.
* If that capture set would require an anonymous rigid region to escape, the ordinary skolem-escape rule rejects the
  value.
* A runtime composite value, including a tuple, record, constructor application, variant injection, or sealed package,
  has the union of the capture sets of its runtime-relevant constituents.
* The result of `if`, `match`, or other branching expression forms has the union of the capture sets of all reachable
  normal-result branches.
* This union is canonicalized by duplicate removal and the ordering rule of §5.1.6.1.

Compile-time-only constituents do not contribute to runtime capture annotations.

Capture-set subsumption:

Let `S` and `S'` be canonical capture sets over explicit region variables.
If a value elaborates at type `T captures S` and `S ⊆ S'`, then it may be checked against demanded type
`T captures S'`.

This is a typing / elaboration rule only.
It is not part of definitional equality.
Definitional equality of capture annotations remains exact as specified in §14.3.

Examples:

```kappa
makeGetter :
    forall (s : Region) (a : Type).
    (&[s] x : Box a) -> ((Unit -> a) captures (s))

packGetter :
    forall (s : Region) (a : Type).
    (&[s] x : Box a) -> ((get : (Unit -> a) captures (s)))
```

<!-- types.universes.disjoint_path_borrowing_records -->
#### 5.1.7 Disjoint path borrowing for records

Borrow tracking for records is path-sensitive. A borrow of a record field does not automatically lock the entire record.

Definitions:

* A **borrow root** is a stable variable, parameter, or hidden temporary from which the borrowed view is projected. In
  v0.1, borrow introduction is defined for roots currently held at `1`, `<=1`, `>=1`, `ω`, or `&`. Hidden temporaries
  introduced by elaboration of borrowed local bindings (`let & pat = expr`) are borrow roots for the scope of that
  binding's body or remaining do-items.
* A **record path** is a borrow root followed by zero or more field labels: `r`, `r.x`, `r.pos.x`, and so on.
* The **footprint** of a borrowed path is the portion of the root record that is considered locked by that borrow for
  ownership-checking purposes.

Footprint rules:

* Borrowing the whole record `r` has footprint `r`; it overlaps every subpath of that same root.
* For any record field whose declared type has no sibling-field dependencies, borrowing `r.f` has footprint exactly the
  projected subpath `r.f` (recursively for longer paths such as `r.f.g`).
* For a dependent record, borrowing `r.f` has footprint:
  * the projected path `r.f`, and
  * the dependency closure of `f` in the record's field-dependency graph (§5.5.1.1), re-rooted under `r`.
  In other words, if the type of field `f` depends on sibling field `g` directly or transitively, then borrowing `r.f`
  also locks `r.g` for the duration of that borrow. This applies equally to closed records and to the explicit field
  telescope of an open record.

Overlap and disjointness:

* Two borrowed record paths are **disjoint** iff:
  * they have different borrow roots, or
  * they have the same borrow root and their footprints are disjoint.
* Otherwise they **overlap**.
* Because `&` is a non-consuming shared borrow in v0.1, overlapping borrowed paths may coexist with each other. The
  important restriction is against consuming the root or any overlapping subpath while such borrows are live.

Consequences:

* A live borrow of `r.f` forbids any operation requiring `@1` ownership of `r` as a whole, or of any path overlapping
  the footprint of `r.f`, until that borrow ends.
* Disjoint sibling borrows are allowed simultaneously. Thus independent fields such as `r.x` and `r.y` may both be
  borrowed at once.
* Dependent fields are not disjoint merely because their labels differ. If `buffer : Array len Byte`, then borrowing
  `r.buffer` overlaps with `r.len`.
* Borrowing through a record pattern is decomposed pathwise. For example, `let & (x = bx, y = by) = r` introduces
  borrows of `r.x` and `r.y`, not one undifferentiated borrow of all of `r`, except that dependency closure may enlarge
  either footprint for dependent records.
* If elaboration introduces a fresh binder as a borrowed alias of an existing record path, that binder does not create a
  new borrow root. Instead, it inherits the same underlying borrow root and path-sensitive footprint as the source path,
  and any further projection from that binder is interpreted pathwise as a projection from the same underlying source
  path. This applies in particular to compiler-introduced binders such as the successful-payload binder of
  safe-navigation (`?.`) in Section 7.1.1.2.
* When the compiler cannot identify a stable record root and field path, it MAY conservatively treat the borrow as one
  of the whole expression.

This path-sensitive rule is specified only for record fields in v0.1. Other aggregate structures may be treated
conservatively as whole-value borrows unless a later section defines finer-grained splitting for them.

<!-- types.universes.disjoint_path_borrowing_records.stable_places_place_preserving_bindings -->
##### 5.1.7.1 Stable places and place-preserving bindings

A stable place is the semantic notion of an addressable immutable subvalue used by borrowing, path-sensitive
consumption, and `inout` rewriting.

A stable place consists of:

* a root binder; and
* a path of zero or more selections from that root.

The path components admitted in v0.1 are:

* record-field selections; and
* constructor-field selections made available by the current refinement context.

Source forms that elaborate to stable places in v0.1 are:

* a variable name `x`, yielding the root place `x`;
* a record projection `e.f` when `e` elaborates to a stable place `p`, yielding the place `p.f`;
* a constructor-field projection `e.f` when `e` elaborates to a stable place `p` and the current refinement context
  makes that constructor field available, yielding the place `p.f`; and
* a fresh hidden temporary introduced by elaboration.

A `var`-bound name does not itself denote a stable place value root for pure elaboration, because under §8.5.1 it
denotes a `Ref`. A surface construct that admits a `var`-rooted place, such as `~x` or `~x.f`, first elaborates by
reading the current contents of that `Ref` into a fresh hidden temporary root and then forming the corresponding stable
place from that temporary.

An ordinary value binding does not preserve place identity. If `e` elaborates to a stable place and the user writes
`let y = e`, then `y` is bound to the value currently read from that place, not to an alias of the place itself.

Place identity is preserved only by constructs that explicitly say so, including:

* borrowed bindings `let & pat = e`;
* `using`-introduced borrowed bindings;
* compiler-generated success binders for `?.`; and
* `inout` rewrite-site temporaries.

If elaboration introduces a fresh binder as an alias of an existing stable place, that binder carries the same
underlying root and path. Projecting from the alias extends that same path rather than creating a fresh root.

<!-- types.universes.disjoint_path_borrowing_records.computed_place_expressions -->
##### 5.1.7.2 Computed place expressions

A place expression is either:

* a stable place under §5.1.7.1; or
* a fully applied call to a `projection` definition (§6.1.1).

A fully applied projection call is not a first-class runtime reference. It denotes a computed selection of one stable
place rooted in one of the `place` parameters of the called projection definition.

After substituting actual arguments for formal parameters, each reachable `yield` in the projection body determines one
yielded stable-place alternative. Every yielded alternative of a projection definition must be rooted in one of the
declaration's `place` binders.

Static footprint summary:

* The static footprint of a stable place is its ordinary footprint under §5.1.7.
* The static footprint of a fully applied projection call is the union of the ordinary footprints of all its yielded
  stable-place alternatives, regardless of which `place` binder each alternative is rooted in, after applying the
  dependency-closure rule of §5.1.7 to each alternative.
* Borrow-overlap checks, disjointness checks, and `~` admissibility checks use this static footprint summary.
* Runtime projector elimination uses only the single yielded stable place selected by evaluation of the projection body,
  together with the corresponding root-pack rebuild when a fill or open operation is requested.

Use restrictions:

* A fully applied projection call may appear in:
  * a borrow-demanding position under §5.1.5;
  * a `~` argument under §8.8; and
  * an ordinary value-demanding position.
* In a non-consuming ordinary value-demanding position, the projection call elaborates through the projector
  elimination of §17.3.1.3 and behaves as `ReadProjector proj pack`.
* A fully applied projection call may also appear in a consuming ordinary value-demanding position.
* In that case, the projection call elaborates through the projector elimination of §17.3.1.3 and behaves as
  `MoveProjector proj pack`.
* In a borrow-demanding position, the call elaborates through `BorrowProjector proj pack`.
* Under `~`, the call elaborates through `OpenProjector proj pack`.
* The post-expression path state is the branchwise join of the leaf path states; in v0.1 a conforming implementation MAY
  conservatively take this join to be the union of consumed footprints per root.
* Consequently, a path may be considered unavailable after the expression even if it is consumed only on some dynamic
  branches.

Why ordinary source code need not write capture annotations everywhere:

The hidden region environment is inferred during elaboration. For local code it may remain implicit.
The `captures (...)` form exists so that values whose hidden region environment matters across an explicit type boundary
can state that fact.
If the inferred capture set is empty, no capture annotation is written in the elaborated type.

This is analogous to how `runST` in §8.5.3 uses rank-2 polymorphism to prevent `STRef s` escape without requiring the
user to write region variables on every arrow.

Module interfaces and separate compilation:

* A top-level definition is well-formed for export only if every escaping hidden region is either:
  * discharged before export, or
  * expressible using explicit `Region` binders already present in the exported type.
* The exported type recorded in the module interface is the fully elaborated type, including any inferred non-empty
  `captures (...)` annotations required by §5.1.6.1.
* There is no separate hidden region-environment summary outside the exported type.
* Downstream modules perform escape checking against those capture-annotated exported types.
* Ordinary module interfaces never expose anonymous rigid region variables. Consequently, cross-module higher-order APIs
  may abstract over borrowed captures only through explicit `Region` binders together with `captures (...)`, rather than
  by laundering hidden local regions across the module boundary.

Example:

```kappa
makeGetter :
    forall (s : Region) (a : Type).
    (&[s] x : Box a) -> ((Unit -> a) captures (s))
```

The returned closure has ordinary function shape `Unit -> a`, but its value type additionally records that its hidden
region environment may mention `s`. A caller may use or store that closure only where `s` is still live. At a call site,
an explicit region variable such as `s` may be instantiated by a caller-local rigid region `rho`.

<!-- types.universes.disjoint_path_borrowing_records.first_class_projector_descriptors_borrowed_views_opened_places -->
##### 5.1.7.3 First-class projector descriptors, borrowed views, and opened places

Kappa provides the following built-in type formers:

```kappa
Projector  : Type -> Type -> Type
BorrowView : Region -> Type -> Type
Zipper     : Type -> Type -> Type -> Type
```

Meaning:

* `Projector roots focus` is a first-class pure descriptor of a place-selection computation whose place arguments have
  shape `roots` and whose selected focus has type `focus`.
* `BorrowView ρ A` is a first-class borrowed view of `A` valid under region `ρ`.
* `Zipper whole focus replace` packages an opened owned focus together with a linear filler back to `whole`.

Canonical declaration of `Zipper`:

```kappa
data Zipper (whole : Type) (focus : Type) (replace : Type) : Type =
    Zipper (focus : focus) (1 fill : replace -> whole)
```

Kappa provides the following built-in operations:

```kappa
captureBorrow :
    forall (ρ : Region) (A : Type).
    (&[ρ] x : A) -> BorrowView ρ A

withBorrowView :
    forall (ρ : Region) (A : Type) (R : Type).
    BorrowView ρ A -> (((&[ρ] x : A) -> R)) -> R

composeProjector :
    forall (roots : Type) (mid : Type) (focus : Type).
    Projector roots mid -> Projector mid focus -> Projector roots focus
```

Rules:

* `Projector`, `BorrowView`, and `Zipper` are ordinary first-class types.
* Values of those types may be bound, stored in records, packaged, sealed, opened, returned, and projected exactly
  like other values, subject to the ordinary quantity and escape rules.
* `captureBorrow` does not extend the lifetime of a borrow. It merely reifies an already-valid borrowed view as an
  ordinary value whose type explicitly mentions `ρ`.
* `withBorrowView v k` checks `k` in a context where its parameter is an ordinary borrowed binder under `ρ`; it is the
  elimination form for a first-class borrowed view.
* `composeProjector` composes projector descriptors only. It does not itself read, move, borrow, or fill any place.

<!-- types.functions -->
### 5.2 Function types

* Arrow types are **right-associative**:

  ```kappa
  A -> B -> C    ==    A -> (B -> C)
  ```

* Dependent arrow forms:

  ```kappa
  (0 x : A) -> B
  (1 x : A) -> B
  (& x : A) -> B
  (&[s] x : A) -> B   -- where s is a bound variable of type Region
  (q x : A) -> B      -- where q is a bound variable of type Quantity
  (thunk x : A) -> B
  (lazy x : A) -> B
  (x : A) -> B        -- defaults to ω
  ```

  which denotes a function whose result type may depend on `x`.

* Non-dependent arrow is sugar:

  ```kappa
  A -> B    ==    (_ : A) -> B
  ```

A function value type may be capture-annotated under §5.1.6.1. For example:

```kappa
((A -> B) captures (s))
(((q x : A) -> B) captures (s1, s2))
```

Such an annotation describes the hidden region environment of the function value itself; it does not change the binder
types of that arrow.

<!-- types.functions.suspension_types_strict_binder_sugar -->
### 5.2.1 Suspension types and non-strict binder sugar

Kappa has a single ordinary dependent function space.

Non-strictness is expressed by suspension types, not by a distinct family of arrow types.

Primitive suspension types:

```kappa
Thunk : Type -> Type
Need  : Type -> Type
```

Meaning:

* `Thunk A` is a by-name suspension of `A`. Each `force` may re-evaluate the suspended body.
* `Need A` is a by-need suspension of `A`. The first `force` evaluates and memoizes the suspended body; later forces
  return the cached value.

Surface binder sugar:

```kappa
(thunk x : A)
(lazy x : A)
(q thunk x : A)
(q lazy x : A)
```

desugars to:

```kappa
(x : Thunk A)
(x : Need A)
(q x : Thunk A)
(q x : Need A)
```

respectively.

This sugar does not introduce a distinct core binder kind and does not change function-type identity beyond the
ordinary appearance of `Thunk A` or `Need A` in the binder type.

Introduction of `Need A` suspensions requires `Shareable A` and obeys the capture restriction of §7.2.2.

<!-- types.quantification -->
### 5.3 Universal quantification

`forall` is syntactic sugar over Pi-types:

```kappa
forall a. T        ==   (@0 a : Type) -> T
forall (a : S). T  ==   (@0 a : S) -> T
```

This includes quantification over the intrinsic compile-time types `Quantity`, `Region`, `Universe`, `Constraint`,
`RecRow`, `VarRow`, `EffRow`, `Label`, and `EffLabel`, e.g. `forall (q : Quantity). T`, `forall (s : Region). T`,
`forall (u : Universe). T`, and `forall (r : EffRow). T`.

Examples:

```kappa
forall a. a -> Int
forall (u : Universe) (a : Type u) (b : Type u). (a -> b) -> a -> b
forall (n : Nat). Vec n Int -> Int
forall (s : Region). (&[s] buf : Buffer) -> Unit -> Int
forall (r : EffRow) (l : EffLabel). SplitEff r l E r' => ...
forall (row : RecRow) (lab : Label). ContainsRec row lab T => ...
```

<!-- types.quantification.rows_labels -->
#### 5.3.1 Rows and labels

Kappa has three intrinsic compile-time row types:

```text
RecRow   : Type0   -- record rows
VarRow   : Type0   -- union/variant rows
EffRow   : Type0   -- effect rows
```

Rows of different row types never unify.

Record labels inhabit the intrinsic compile-time type `Label : Type0`.
Effect labels inhabit the intrinsic compile-time type `EffLabel : Type0`.
Effect interfaces are ordinary named constructors of declaration kind `type`.

Kappa provides the following row-constraint constructors:

```kappa
ContainsRec : RecRow -> Label -> Type -> Constraint
LacksRec    : RecRow -> Label -> Constraint

ContainsVar : VarRow -> Type -> Constraint
LacksVar    : VarRow -> Type -> Constraint

ContainsEff : EffRow -> EffLabel -> Type -> Constraint
LacksEff    : EffRow -> EffLabel -> Constraint
SplitEff    : EffRow -> EffLabel -> Type -> EffRow -> Constraint
```

Surface syntax for `EffRow` values is given in §5.3.2.

In v0.1, `RecRow` and `EffRow` are finite maps with unique labels, while `VarRow` is a finite duplicate-free collection
of member types. Row equality is modulo permutation after row normalization.

A row variable must have one of the types `RecRow`, `VarRow`, or `EffRow`. Row variables of different row types are
never interchangeable.

<!-- types.quantification.effect_row_syntax -->
#### 5.3.2 Effect-row surface syntax

Effect rows are written:

```kappa
<[ ]>                               -- empty effect row
<[label1 : E1, label2 : E2]>        -- closed
<[label1 : E1, label2 : E2 | r]>    -- open, r : EffRow
```

Rules:

* Effect-row labels are identifiers in the effect-label namespace.
* A row entry `<[l : E | r]>` associates the effect label `l` with the effect interface `E`.
* Labels in a single effect row must be unique.
* Equality of effect rows is modulo permutation after row normalization.
* An open row of the form `<[l : E | r]>` carries the implicit uniqueness side condition `LacksEff r l`.
* `SplitEff r l E r'` holds iff `r` normalizes to `<[l : E | r']>`.

<!-- types.quantification.implicit_declaration_universalization -->
#### 5.3.3 Implicit declaration-level universalization

Kappa supports implicit universalization of free lowercase identifiers at declaration sites.

Header parameters:

* In the parameter list of a `trait`, `data`, `type`, or `effect` declaration, an unannotated standard-identifier
  parameter introduces a declaration binder.
* If that parameter's first character is an ASCII lowercase letter, its annotation is inferred from its uses in the
  declaration.
* If the annotation is otherwise unconstrained, it defaults to `Type`.
* An unannotated parameter whose first character is not an ASCII lowercase letter defaults to `Type`.

Free lowercase identifiers in signatures and heads:

* After accounting for explicit binders, declaration parameters, and enclosing lexical bindings, any remaining free
  standard identifier whose first character is an ASCII lowercase letter is implicitly universally quantified when it
  appears in:
  * an ordinary term signature, or in the parameter / result annotations of a named `let` definition;
  * a trait member declaration or default-member signature;
  * an effect operation signature;
  * a GADT-style constructor signature;
  * an instance premise or instance head.
* Backtick identifiers and operator tokens are never implicitly universalized by this rule.
* The introduced binders are ordered by first left-to-right occurrence.
* Each introduced binder's annotation is inferred from its uses in the corresponding signature or head.
* If the annotation is otherwise unconstrained, it defaults to `Type`.
* For signatures, elaboration is as if the declaration were prefixed by an explicit outer `forall`.
* For instances, elaboration is as if the entire instance declaration were abstracted over those binders before its
  premises and head are checked.
* Outside the sites listed above, ordinary unbound-identifier rules apply.

Metavariable discipline:

* An unresolved lowercase identifier at one of the sites above is never treated as a unification metavariable that may
  escape through interfaces or separate-compilation artifacts.
* It is either implicitly generalized by this rule or the program is rejected as containing an unresolved identifier.

Examples:

```kappa
trait Functor f =
    map : (a -> b) -> f a -> f b
```

elaborates as if:

```kappa
trait Functor (f : Type -> Type) =
    map : forall (a : Type) (b : Type). (a -> b) -> f a -> f b
```

```kappa
trait Alternative (f : Type -> Type) =
    empty : f a
```

elaborates `empty` as if:

```kappa
empty : forall (a : Type). f a
```

```kappa
instance Eq a => Eq (Option a) =
    ...
```

elaborates as if abstracted by:

```kappa
forall (a : Type).
```

<!-- types.unions -->
### 5.4 Union types

Kappa supports union types (also called variant types in this context) via the `(| ... |)` syntax. A union type denotes
a value that belongs to *exactly one* of the listed types.

```kappa
type U = (| Int | String | Error |)
type OptionalInt = (| Unit | Int |)
```

<!-- types.unions.formation -->
#### 5.4.1 Union formation

```text
unionType ::= '(|' type ('|' type)* '|)'
            | '(|' type ('|' type)* '|' rowVar '|)'
```

```kappa
Variant : VarRow -> Type
```

Conceptually, a union type elaborates to `Variant r` where `r : VarRow` is a row whose entries are member types rather
than labelled entries.

Each variant member type `T` has a canonical member identity `MemberId(T)`.

`MemberId(T)` is computed from the fully elaborated member type using the same canonical defining-module view used for
Hard Hashes (§15.1.2): it ignores downstream `clarify`, and it is stable across separately compiled modules and
artifacts.

Row equality for `VarRow`, duplicate removal, `ContainsVar`, `LacksVar`, and residual-row matching are all defined in
terms of `MemberId`, not in terms of importer-local transparency or importer-local `clarify`.

Row equality for `VarRow` is modulo permutation and duplicate removal by `MemberId`.
Canonical order is the lexicographic order of the canonical serialized `MemberId` values.
This canonical order is used for row normalization and definitional equality, not for assigning row-local runtime tag
ordinals.

<!-- types.unions.introduction_injection -->
#### 5.4.2 Introduction (injection)

```kappa
(| 42 : Int |)           -- explicit type annotation
(| "hello" |)            -- type inferred when unambiguous
(| someValue |)          -- same
```

Typing rule:

* `(| e : T |) : Variant r` if `ContainsVar r T` holds.
* For an open union row, injection is also permitted when the residual row can admit `T`, i.e. when the appropriate
  `LacksVar` side condition holds.
* The inferred form `(| e |)` is valid only when the injected type is unambiguously determined from context.

<!-- types.unions.expected_type_directed_injection_widening -->
#### 5.4.3 Expected-type-directed injection and widening

Union injection and widening are expected-type-directed elaboration rules, not an ambient subtyping relation over the
whole expression language.

Injection from an ordinary member type:

* If the expected type is `Variant r`, the expression being elaborated has type `T`, and `ContainsVar r T` holds,
  elaboration may insert the corresponding union injection.
* For an open union row, expected-type-directed injection is also permitted when the residual row can admit `T`, i.e.
  when the appropriate `LacksVar` side condition holds.

Widening from one union to another:

* If the expected type is `Variant r₂`, the expression being elaborated has type `Variant r₁`, and every member type of
  `r₁` also appears in `r₂`, elaboration may insert a widening coercion from `Variant r₁` to `Variant r₂`.

Otherwise no implicit union injection or widening is inserted.

When elaboration inserts such a widening, it is zero-cost at runtime: no payload rewrite or row-relative retagging is
performed. The widened value reuses the same runtime tag identity for each member type (§14.5).

<!-- types.unions.elimination_match -->
#### 5.4.4 Elimination (`match`)

```kappa
match u
  case (| x : Int |)     -> x + 1
  case (| s |)           -> String.length s
  case (| e : Error |)   -> handleError e
```

For an open union, the residual row is matched with dedicated syntax:

```kappa
match uOpen
  case (| x : Int |)     -> handleInt x
  case (| ..rest |)      -> default rest
```

All branches must have the same result type.

Exhaustiveness:

* For a closed union, the match must be exhaustive.
* For an open union `(| ... | r |)`, exhaustiveness requires either an ordinary catch-all pattern `_` or a residual-row
  pattern `case (| ..rest |) -> ...`.
* In a residual-row branch for scrutinee type `(| ... | r |)`, the binder introduced by `..rest` has type `Variant r`.

<!-- types.unions.variant_row_polymorphism -->
#### 5.4.5 Variant row polymorphism

A function can be polymorphic over the residual row:

```kappa
project : forall (r : VarRow) (a : Type).
          LacksVar r a => (| a | r |) -> Option a

let project u =
    match u
    case (| x : a |)   -> Some x
    case (| ..rest |)  -> None
```

The constraint `LacksVar r a` ensures `a` is not already present in the row, preventing ambiguous overlaps at use sites.

<!-- types.unions.nullary_singleton_arms -->
#### 5.4.6 Nullary / singleton arms

```kappa
data Missing : Type =
    Missing

type Optional a = (| Missing | a |)
```

`Missing` here is an ordinary singleton type declared separately. If a dedicated nullary branch name is desired, it must
first be introduced as an ordinary singleton type (or `Unit` may be used when no dedicated name is needed). There is no
separate tag namespace: every arm of a union is a type.

<!-- types.unions.pattern_grammar_addition_7_6_1 -->
#### 5.4.7 Pattern grammar (addition to §7.6.1)

```text
variantPat ::= '(|' ident ':' type '|)'
             | '(|' ident '|)'
             | '(|' '_' ':' type '|)'
             | '(|' '..' ident '|)'
```

The inferred form `(| x |)` is valid only when the scrutinee type is a singleton union `(| T |)` or when the member type
can be unambiguously determined from context. It is never interpreted as the residual-row case. Residual-row matching
must be written explicitly as `(| ..rest |)`.

<!-- types.unions.interaction_gadts_indexed_types -->
#### 5.4.8 Interaction with GADTs / indexed types

Union types may be indexed. An injection such as `(| v : Vec n a |)` is valid when the expected union type contains `Vec
n a` among its member types. Exhaustiveness and reachability are determined by index unification exactly as in §7.5.1.

<!-- types.unions.optional_type_sugar -->
#### 5.4.9 Optional-type sugar

For any type expression `T`, the postfix form `T?` is syntactic sugar for `Option T`, where `Option` is the prelude type
of §2.6.2.

Grammar (amends the type grammar):

```text
typeAtom     ::= ... existing atoms ...
typePostfix  ::= typeAtom ('?')*
typeApp      ::= typePostfix typePostfix*
typeCapture  ::= typeApp [ 'captures' '(' regionRef (',' regionRef)* ')' ]
typeArrow    ::= typeCapture ('->' typeArrow)?
```

Postfix `?` binds tighter than type application. Therefore:

Because `?` is part of `typePostfix` and `typeApp` is a sequence of `typePostfix`s, postfix `?` always binds tighter
than juxtaposition (type application). Thus `List Int?` parses as `List (Option Int)`, while `(List Int)?` requires
explicit parentheses.

* `Int?` = `Option Int`
* `List Int?` = `List (Option Int)`
* `(List Int)?` = `Option (List Int)`
* `Int??` = `Option (Option Int)` (no flattening; nesting is preserved)
* `Int? -> Bool` = `Option Int -> Bool`
* `forall a. a?` = `forall a. Option a`

Postfix `?` at the type level is built-in and is not subject to user fixity declarations. A user `postfix N (?)` fixity
declaration applies only at term positions; it has no effect in type positions.

<!-- types.records -->
### 5.5 Records (named tuples)

Records are the single "struct-like" construct in Kappa.

<!-- types.records.syntax -->
#### 5.5.1 Record syntax

Record types are written:

```kappa
(ℓ₁ : T₁, ℓ₂ : T₂, ..., ℓₙ : Tₙ)             -- closed
(ℓ₁ : T₁, ..., ℓₙ : Tₙ | r)                  -- open (row-polymorphic, r : RecRow)

(x : Int, y : Int)
(name : String, age : Int)
(value : Int, array : Array Int)
(1 data : Bytes, & file : File, len : Nat)
(len : Nat, buf : Array this.len Byte | r)
```

Field declaration grammar:

```text
recordFieldDecl ::= [ 'opaque' ] [ '@' ] [quantity] [ 'thunk' | 'lazy' ] ident ':' type
```

Field-suspension sugar:

* `thunk name : T` is sugar for `name : Thunk T`.
* `lazy name : T` is sugar for `name : Need T`.
* `q thunk name : T` is sugar for `q name : Thunk T`.
* `q lazy name : T` is sugar for `q name : Need T`.

This sugar is purely structural and preserves the correspondence between record fields and function binders.

The annotation after `:` may elaborate either to an ordinary computational type or to a compile-time type in the sense
of §5.1.4.1.

A field whose annotation elaborates to a compile-time type is a compile-time member.

Compile-time members:

* may carry any quantity annotation admitted by the general field syntax;
* may be transparent or `opaque`;
* participate in dependency analysis, lawful reordering, projection, sealing, and existential packaging exactly as
  ordinary fields do; and
* are erased at runtime according to §§5.1.4 and 14.4 unless preserved by an explicit reified carrier.

When both `@` and an explicit quantity are present, the surface spelling is `@q name : T`, not `q @name : T`.

Opaque members:

* A field marked `opaque` is an opaque compile-time member.
* A record type containing one or more opaque members is a signature type.
* An opaque member may be referenced by later field types via `this.label` exactly as an ordinary earlier field may.
* Opaqueness affects introduction, projection transparency, and sealing (§5.5.10). It does not change dependency
  analysis, lawful reordering, or canonical field order.

* One-element record type: `(x : T,)`
* The zero-field closed record has no distinct surface syntax in v0.1; it is identified with `Unit` / `()`.
* If a field quantity is omitted, it defaults to `ω`.
* Closed record types may be dependent telescopes of named fields.
* Field order in a closed record type is not semantically significant.
* In a closed record type, a sibling-field reference may be written explicitly as `this.label`.
* A bare identifier `label` in a sibling-reference position is shorthand for `this.label` only when that reading is
  unambiguous; otherwise the user MUST write `this.label`.
* During elaboration of a closed record type, the compiler constructs the field-dependency graph and performs a
  topological sort. If the dependency graph contains a cycle, the record type is ill-formed and compilation fails.
* The resulting dependency-respecting order is the canonical order used for definitional equality (§5.5.1.1, §14.6).
* Open record types use the same telescopic discipline for their explicitly listed fields. An explicit field type in
  `(f1 : T1, ..., fn : Tn | r)` may refer to other explicitly listed fields via `this.label`, or via a bare label when
  that shorthand is unambiguous, subject to the same acyclicity requirement as a closed record type.
* The residual row `r` is not in scope in field types. No explicit field type in an open record may refer to `r` or to
  any field that would come only from the residual row.
* Semantically, the residual row is appended after the explicit telescope. Implementations may model that row tail in
  the core as abstract over the explicit fields, but this dependency is not surface-addressable through the identifier
  `r`.
* Whole-record use is constrained by field quantities. In particular, if any field is declared at quantity `1`, then the
  record as a whole is treated as a linear resource for whole-record use until those linear paths have been consumed or
  restored.

<!-- types.records.syntax.record_field_order_lawful_reorderings -->
##### 5.5.1.1 Record field order and lawful reorderings

Record types are **order-insensitive up to lawful reorderings**.

Closed record *values* use the same dependency-graph discipline as closed record *types*. A record literal `(f1 = e1, f2
= e2, ...)` may be written in any order. During elaboration, the compiler builds a field-dependency graph using both:

* the dependencies induced by the corresponding field types, and
* the dependencies induced by explicit `this.label` references and by bare identifiers that resolve as sibling-field
  shorthand in the same literal.

It then computes a dependency-respecting topological order and performs name resolution, typechecking, and evaluation in
that reordered sequence. If the resulting graph contains a cycle, the record literal is ill-formed and compilation
fails. After elaboration, the resulting value is definitionally equal to its canonically reordered form.

Well-formedness:

* In a record type `(f1 : T1, f2 : T2, ..., fn : Tn)` or `(f1 : T1, f2 : T2, ..., fn : Tn | r)`, each explicit field
  type `Ti` may refer only to explicitly listed sibling fields, using `this.label` or the unambiguous bare-label
  shorthand.
* Elaboration constructs the dependency graph induced by those explicit field references and computes a
  dependency-respecting topological order.
* A record type is well-formed iff that dependency graph is acyclic.
* In an open record type, the residual row `r` is not available for reference from explicit field types, and no hidden
  dependency from `r` back into the explicit telescope is assumed during this check.
* The same dependency graph is used by the record-update rules of §5.5.5, and its type-level component is reused when
  elaborating record values.

Intuition: record types form a dependent telescope. Fields may be reordered when the reorder does not violate
dependencies.

More precisely:

* A reordering is **lawful** iff every field appears **after** all fields that its type depends on (a
  dependency-respecting permutation).
* Canonicalization chooses the dependency-respecting topological order; ties are broken as specified in §14.6.
* Two record types are definitionally equal iff they have the same set of fields (by name) and their canonical forms
  align such that corresponding field quantities are identical and corresponding field types are definitionally equal.

Implementations may insert dependency-respecting reordering coercions implicitly where needed.

<!-- types.records.formation -->
#### 5.5.2 Record formation

Closed record types have exactly the listed fields and may be dependent telescopes.

Open record types have the listed fields plus any fields supplied by the residual row `r : RecRow`. Their explicitly
listed fields may also form a dependent telescope, provided the dependency graph over those explicit fields is acyclic.

Restrictions for open records:

* dependencies among the explicitly listed fields are governed by the same acyclic sibling-dependency graph as in
  §5.5.1.1,
* the residual row `r` may not be referenced in explicit field types, and
* no explicit field type may rely on any field that would be supplied only by `r`.

Record types and values are usable as Σ-types (dependent pairs), subject to the lawful-reordering rule above.

<!-- types.records.values -->
#### 5.5.3 Record values

```kappa
(ℓ₁ = e₁, ℓ₂ = e₂, ..., ℓₙ = eₙ)

(x = 1, y = 2)
(name = "Bob", age = 33)
(name, age = 33)    -- punning: sugar for (name = name, age = 33)
(x = 1,)           -- one-field record value
(len = 10, buffer = mkBuffer this.len, checksum = checksumOf this.buffer)
```

Each `eᵢ` has type `Tᵢ` with dependencies substituted according to the dependency-respecting order chosen during
elaboration (§5.5.1.1). Each field also carries the quantity determined by the chosen record type; if that quantity is
not fixed by annotation or context, it defaults to `ω`.

Field order in the source literal is not semantically significant. The compiler builds the field-dependency graph for
the literal, performs a topological sort to obtain a dependency-respecting order, and then resolves names, typechecks
fields, and evaluates field expressions in that reordered sequence.

After elaboration, the resulting record value is definitionally equal to its canonical field-order normalization
(§14.6).

Well-formedness:

* In a record value `(f1 = e1, f2 = e2, ...)`, sibling references may be written explicitly as `this.label`. A bare
  identifier `label` is shorthand for `this.label` only when that reading is unambiguous; otherwise `this.label` is
  required. The compiler reorders the fields to a dependency-respecting sequence before name resolution and evaluation.
* If the combined dependency graph for the literal, including expression-level references, contains a cycle, the literal
  is a compile-time error.
* Record field punning is permitted: in a record value, an entry written as just `label` is treated as `label = label`.
* In such a punned entry, `label` resolves in the surrounding scope before reordering, and the resolved term must have
  the field type required for that label.
* A record literal may be checked directly against an ordinary record type.
* Expected-type-directed suspension insertion:
  * When a record literal is checked against an expected record type, and a field is declared with type `Thunk T` or
    `Need T`, the supplied field expression is elaborated by the same expected-type-directed suspension rule as
    §7.1.3.1.
  * If the field expression already checks against the demanded suspension type without inserting a new suspension at
    that same field position, it is used unchanged.
  * Otherwise, if it checks against `T`, it is elaborated as `thunk expr` or `lazy expr` respectively.
* A record literal MUST NOT be checked directly against a signature type (that is, a record type containing one or more
  opaque members).
* Introduction of a signature value uses `seal ... as ...` (§5.5.10).

<!-- types.records.field_projection -->
#### 5.5.4 Field projection

```kappa
rec.ℓ
```

Projects field `ℓ` from a record whose row is `r : RecRow`. Well-typed iff `ContainsRec r ℓ T` for some `T` (the
inferred result type).

The projection yields the field value at the field's declared quantity.

Place preservation:

* If `rec` elaborates to a stable place `p` under §5.1.7.1, then the projection `rec.ℓ` also elaborates to a stable
  place, namely `p.ℓ`, before ordinary quantity checking.
* If constructor-tag refinement makes a constructor field available at that source site, the analogous constructor-field
  place is formed.
* An ordinary value binding of `rec.ℓ` still binds the projected value, not an alias to the place, unless the
  surrounding construct explicitly preserves place identity under §5.1.7.1.

Compile-time member selection:

* If the selected field is compile-time in the sense of §5.1.4.1, the same projection form `rec.ℓ` may appear in any
  syntactic position compatible with the field's compile-time type, regardless of the field's written quantity.
* This includes:
  * type position for members such as `T : Type` or `F : Type -> Type`,
  * quantity position for members such as `Q : Quantity`,
  * region position for members such as `S : Region`,
  * row or label position for members such as `R : RecRow` or `L : Label`, and
  * associated/package projections such as `d.Item`, `d.Q`, or `m.Set`.
* For signature types, compile-time member selection additionally obeys the opaqueness rules of §5.5.10.

When a projection such as `rec.ℓ` is used in a context that demands a borrowed argument `(& _ : T)`, the implicit borrow
applies to that field path rather than automatically to the whole record, subject to the disjoint-path borrowing rules
of §5.1.7.

Path-sensitive consumption:

* If the projected field is declared at quantity `1`, then projecting `rec.ℓ` from an owned record consumes only the
  path `rec.ℓ`. The root record `rec` enters a partially consumed state.
* Sibling paths such as `rec.other` remain accessible at their own declared quantities, subject to any dependency-based
  overlap already imposed by §5.1.7.
* A partially consumed record may not be consumed as a whole at quantity `1`, nor matched against a pattern or passed to
  a function that demands full owned access to the original record.
* A partially consumed record may still be projected through its remaining unconsumed paths, updated to restore consumed
  paths (§5.5.5), or borrowed, in which case the borrow covers only the remaining unconsumed portion of the record.
* Re-projecting a field path already consumed at quantity `1` is a compile-time error unless that path has first been
  restored by record update.

This partially consumed state is an elaboration-time path-usage property. It does not require a distinct runtime
representation.

Examples:

```kappa
let p : (x : Int, y : Int) = (x = 1, y = 2)
let a = p.x       -- 1
let b = p.y       -- 2

let r : (1 buf : Buffer, len : Nat) = ...
let oldBuf = r.buf
let n = r.len
-- `r.buf` is now consumed, but `r.len` remains available
```

<!-- types.records.update -->
#### 5.5.5 Record update

Records are immutable values. A `lhs.{ ... }` form may update selected existing fields and, when `:=` items are
present, may also extend an open row in the same surface form.

```kappa
let p : (x : Int, y : Int) = (x = 1, y = 2)
let p2 = p.{ y = 99 }          -- (x = 1, y = 99)
```

Syntax:

```text
recordPatch ::= expr '.{' patchItem (',' patchItem)* '}'
patchItem   ::= ordinaryUpdateField | projectionUpdateField | extensionField
ordinaryUpdateField ::= [ '@' ] ident '=' expr
projectionUpdateField ::= projectionSection '=' expr
extensionField ::= ident ':=' expr
```

Rules:

* A record patch must contain at least one item.
* A record patch containing only ordinary update fields is a record update.
* A record patch containing only extension fields is a pure row extension (§5.5.6).
* A record patch containing at least one ordinary update field and at least one extension field is a mixed
  update/extension form.
* If a record field is declared as an implicit field `@label : T`, it may be explicitly updated with surface syntax
  `@label = expr`.
* Unlike record literals and constructor applications, field punning is not permitted in record patches. Every patch
  item MUST provide an explicit right-hand side. Writing `r.{ x }` as shorthand for either `r.{ x = x }` or
  `r.{ x := x }` is a compile-time error.
* No label may appear more than once among the ordinary update fields and extension fields of one record patch.

Here `projectionSection` is the section form of §7.1.1.1.

Ordinary field update:

Normative elaboration:

Let the scrutinee have record type `R`, and let `R` have canonical dependency-respecting field order `(g1, g2, ..., gn)`
as determined by §5.5.1.1.

Then a pure ordinary update

```kappa
r.{ f1 = e1, ..., fk = ek }
```

elaborates to a full record literal in that canonical order:

* if `gi` is explicitly updated, the elaborated literal uses the supplied expression for `gi`;
  implicit fields are updated with surface syntax `@gi = expr`,
* if `gi` is omitted, the elaborated literal uses the projection `r.gi`, including omitted implicit fields.

The resulting full record literal is then typechecked against the original record type `R` using the ordinary
dependent-record rules of §5.5.1.1 and §5.5.3.

The same expected-type-directed suspension insertion rule applies to explicitly supplied update expressions when the
updated field type is `Thunk T` or `Need T`:

* if the supplied expression already checks against the demanded suspension type without inserting a new suspension at
  that same update position, it is used unchanged;
* otherwise, if it checks against `T`, it is elaborated as `thunk expr` or `lazy expr` respectively.

A conforming implementation MAY realize the same semantics via one or more `FillPlace` operations of §17.3.1.1, or an
observationally equivalent internal representation, provided the resulting typing and definitional-equality behavior is
the same as the full-record reconstruction specified above.

Consequences:

* There is no separate direct-vs-transitive dependency rule for updates. Whether an omitted field remains valid is
  determined solely by ordinary typechecking of the elaborated full record literal.
* If an omitted field depends on updated fields and the copied projection `r.field` no longer has the required type
  after substitution of the updated values, the update is rejected.
* An implicit field may be repaired explicitly in the same update. For example, `r.{ id = 2, @ok = newProof }` is valid
  when `newProof` has the type required by the updated record.
* If an omitted field path is currently in the consumed state of the scrutinee, the implicit filler `r.field` is
  ill-typed; that field must therefore be supplied explicitly.

A bare update item `name = rhs` is always an ordinary field update. A computed receiver-relative update must be written
using a projection section, as in `(.name args...) = rhs`.

Field references inside supplied ordinary update expressions:

* `this` refers to the evolving updated record prefix in canonical dependency-respecting order, not to the original
  record `r`.
* Therefore a supplied expression may refer to any earlier field in that canonical order, whether that earlier field
  came from an explicit update or from an implicit copied projection `r.field`.
* A bare identifier `label` is shorthand for `this.label` only when that reading is unambiguous; otherwise `this.label`
  is required.

Mixed ordinary update + row extension:

A mixed update/extension form

```kappa
r.{ f1 = e1, ..., fk = ek, x1 := e'1, ..., xm := e'm }
```

where the labels `f1 ... fk` and `x1 ... xm` are pairwise distinct, elaborates as:

```kappa
(r.{ f1 = e1, ..., fk = ek }).{ x1 := e'1, ..., xm := e'm }
```

Rules:

* The separation into the `=` part and the `:=` part is by item kind, not by source order.
* Accordingly, the relative surface order between ordinary update fields and extension fields is not semantically
  significant.
* The `=` part uses the ordinary update rules of this subsection.
* The `:=` part uses the ordinary row-extension rules of §5.5.6.
* A mixed form does not change the meaning of extension-field expressions: they are elaborated exactly as they would be
  in the corresponding explicit chained form above.
* A conforming implementation MAY realize the mixed form directly, provided the resulting typing, dependency checking,
  path reconstitution, and definitional-equality behavior is the same as the chained elaboration above.

Projection-section update:

* A field of the form `(.member args...) = rhs` inside `lhs.{ ... }` is a projection-section update.
* It is not an ordinary field update.
* The section body is resolved exactly as if it had been written as a dotted form on the outer receiver `lhs`.
* After receiver insertion and ordinary resolution, the resulting form must denote either:
  * a stable place rooted in `lhs`, or
  * a fully applied projection call whose yielded alternatives are all rooted in the receiver inserted for `lhs`.
* The replacement expression `rhs` is checked against the selected type of that place or projection.
* The whole update expression has the same type as `lhs`.
* In v0.1, a projection-section update form may contain at most one `projectionUpdateField`.
* In v0.1, a `projectionUpdateField` MUST NOT appear in the same `lhs.{ ... }` form as any ordinary update field or any
  extension field.

Source order:

* The surface order of record-patch items is not semantically significant.
* For mixed update/extension forms, normative elaboration first applies all ordinary update fields and then all
  extension fields, as specified above.
* For pure ordinary updates, elaboration uses the canonical dependency-respecting order of the record type.

Reconstitution:

* If `r.fi` was previously consumed at quantity `1` and `r.{ fi = expr }` successfully typechecks, then the path `r.fi`
  is unconsumed again in the resulting record value.
* A record regains full whole-record capabilities when all previously consumed paths have been restored.

**Examples:**

```kappa
type SizedBuffer = (len : Nat, buffer : Array this.len Byte, checksum : Nat)

let buf : SizedBuffer = (len = 10, buffer = mkBuffer 10, checksum = 42)

let buf2 = buf.{ len = 20 }
-- elaborates to:
-- (len = 20, buffer = buf.buffer, checksum = buf.checksum)
-- rejected because `buf.buffer` has type `Array 10 Byte`, not `Array 20 Byte`

let buf3 = buf.{ len = 20, buffer = mkBuffer this.len }
-- OK; `this.len` refers to the updated prefix, so the buffer is checked
-- against `Array 20 Byte`

let buf4 = buf.{ checksum = 99 }
-- OK; omitted fields are copied from `buf`

let pos : (id : Int, @ok : id > 0) = ...
let pos2 = pos.{ id = 2, @ok = newProof }
-- OK when `newProof` proves `2 > 0`

let pos3 = pos.{ id = 2 }
-- rejected if the copied implicit field `pos.ok` no longer has the type
-- required by the updated record

let myRec : (1 buf : Buffer, len : Nat) = ...
let extractedBuf = myRec.buf
let newBuf = process extractedBuf
let restoredRec = myRec.{ buf = newBuf }
-- `restoredRec` is whole again; both `buf` and `len` are available
```

<!-- types.records.row_extension -->
#### 5.5.6 Row extension (new)

```kappa
rec.{ ℓ := e }
```

`rec.{ ℓ := e }` is row extension.

Syntax:

```text
extensionField ::= ident ':=' expr
```

Rules:

* A pure row extension is a `lhs.{ ... }` form containing at least one extension field and no ordinary update fields.
* A mixed update/extension form is a `lhs.{ ... }` form containing at least one extension field and at least one
  ordinary update field. Its elaboration is specified in §5.5.5.
* For each extension field `ℓ := e`:
  * if the receiver has a closed record type and already contains field `ℓ`, it is a compile-time error; use update
    syntax `ℓ = ...`;
  * if the receiver has type `(fields | r)`, then that extension field is well-typed only if `LacksRec r ℓ` holds and
    `ℓ` is not among the explicitly listed `fields`.

Result type:

* A pure row extension

  ```kappa
  rec.{ ℓ1 := e1, ..., ℓm := em }
  ```

  has type `(fields, ℓ1 : typeof e1, ..., ℓm : typeof em | r)`, normalized by the ordinary record canonicalization
  rules.
* A mixed update/extension form has the type obtained by first applying its ordinary update fields under §5.5.5 and
  then applying the same row-extension rule to its extension fields.

<!-- types.records.row_polymorphism -->
#### 5.5.7 Record row polymorphism

A function `(name : String | r) -> String` is polymorphic in the residual row `r : RecRow` with the implicit constraint
`LacksRec r name`.

<!-- types.records.function_parameters -->
#### 5.5.8 Records as function parameters

A function can take a **record argument**:

```kappa
let f (x : A, y : B) : R =
    ... x ... y ...
```

This is sugar for:

```kappa
let f (arg : (x : A, y : B)) : R =
    let x = arg.x
        y = arg.y
    in ...
```

* The function type is `(x : A, y : B) -> R`. It is **not** curried.
* Calls use record values:

  ```kappa
  let r = (x = 1, y = True)
  let z1 = f r
  let z2 = f (x = 1, y = True)
  ```

For **curried** multi-argument functions, use separate binders:

```kappa
let g (x : A) (y : B) : R = ...
-- type: A -> B -> R
```

<!-- types.records.implicit_record_fields_transparent_unpacking -->
#### 5.5.9 Implicit record fields and transparent unpacking

Record types may declare implicit fields using the `@` prefix. This mechanism formalizes dependent pairs, packed
existentials, and refined types.

Syntax and formation:

```kappa
(id : Int, @ok : id > 0)
(a : Type, @tc : Eq a, value : a)
```

* A field declared as `@label : T` is an implicit field.
* Sort restriction:
  * The type `T` of an implicit field must elaborate either to:
    * a concrete constraint descriptor `C : Constraint`,
    * a compile-time type in the sense of §5.1.4.1, or
    * a boolean proposition accepted by the coercion rule of §5.6.2 (so a bare boolean field type `@p : b` elaborates to
      `@p : b = True`).
  * Ordinary computational data types such as `String`, `Bytes`, or `IO e a` cannot be marked as implicit record fields.
* Implicit record fields are the record-field analogue of implicit binders. Accordingly, §5.1.3's prohibition on
  explicit fields of constraint type does not apply to fields marked with `@`.
* Quantity:
  * Implicit record fields follow the same defaulting rule as implicit binders (§7.3).
  * If the field type elaborates to a concrete constraint descriptor or to a compile-time type of §5.1.4.1, the default
    quantity when omitted is `0`. Erasure of the field value is still governed by §§5.1.4 and 14.4.

Introduction (construction):

When constructing a record literal, the user may omit any field marked with `@`.

* The compiler attempts to synthesize each omitted implicit field using the ordinary elaboration machinery and, when
  applicable, the implicit resolution procedure of §7.3.3 at the point of construction.
* If neither elaboration nor implicit resolution can determine the field, compilation fails with the unsolved implicit
  goal.
* A user may explicitly provide an implicit field using `@label = expr` syntax, for example:

  ```kappa
  (id = 1, @ok = assumePositive 1)
  ```

* Explicitly supplied implicit fields participate in the same dependency-respecting reordering as ordinary record
  fields.

Elimination (existential unpacking):

When a record value containing implicit fields is bound in a local scope, its implicit fields are automatically unpacked
and added to the local implicit resolution context.

1. Direct binding:

   If a record value is bound to a simple identifier, its implicit projections become available as local implicit
   evidence.

   ```kappa
   let pair : (a : Type, @tc : Show a, val : a) = getHiddenValue ()

   -- pair.tc : Show pair.a is now in the implicit context.
   -- The following call therefore succeeds without manual evidence routing:
   println pair.val
   ```

2. Pattern matching / destructuring:

   If a record is destructured in a `let` binding, `match` case, or function parameter, the implicit fields associated
   with the matched record are injected into the local implicit context for the scope of the bound components.

   ```kappa
   let getSafeInt : Unit -> (v : Int, @p : v > 0) = ...

   let (v = x) = getSafeInt ()

   -- The context now contains an implicit assumption:
   -- @_ : x > 0
   ```

   If the implicit field itself is explicitly bound in the pattern, for example `(@p = myProof)`, the bound name
   `myProof` is added to the local implicit context. If the implicit field is omitted from the pattern, the compiler
   instead introduces an anonymous implicit assumption corresponding to the projection.

The local implicit context of §7.3.3 includes such unpacked implicit record-field projections.

Interaction with `export` and interfaces:

Functions returning records with implicit fields act as transparent dependent packages. The caller need not manually
route the hidden type, dictionary, or proof; after binding the returned record, the unpacked implicit context may
satisfy subsequent implicit obligations automatically.

This mechanism does not itself create a sealing or opaqueness boundary: omitted implicit fields remain ordinary record
members and are not hidden from projection or definitional equality. Sealed opaqueness is provided instead by
`seal ... as ...` (§5.5.10).

<!-- types.records.sealed_packages_opaque_members -->
#### 5.5.10 Sealed packages and opaque members

A signature type is a closed record type whose explicit field telescope contains one or more opaque members.

Syntax:

```text
sealExpr ::= 'seal' expr 'as' type
```

Grammar amendment:

```text
atom ::= ... | sealExpr
```

`seal e as S` is an atomic expression for purposes of application and dotted forms.

Examples:

```kappa
type SetSig : Type =
    (opaque Set : Type -> Type,
     singleton : forall (a : Type). a -> this.Set a,
     union     : forall (a : Type). this.Set a -> this.Set a -> this.Set a)

let M : SetSig =
    seal
        (Set = RBTreeSet,
         singleton = rbSingleton,
         union = rbUnion)
    as SetSig
```

Well-formedness:

* The ascribed type `S` of `seal e as S` must elaborate to a closed record type.
* `seal` is permitted whether or not `S` contains opaque members.
* If `S` has no opaque members, `seal` acts as explicit interface ascription and field hiding.
* If `S` does contain opaque members, `seal` is the introduction form for values of type `S`.

Signature matching:

Let `S` have canonical explicit field order `(f1 : T1, ..., fn : Tn)` together with an opaqueness flag for each field.

`seal e as S` is well-typed iff `e` provides at least the fields `f1 ... fn` and the following hold in canonical order:

* for each field `fi`, the projection `e.fi` is well-typed at the declared field type `Ti` after substituting
  `this.fj := e.fj` for every earlier field `fj` with `j < i`;
* if `fi` is a transparent compile-time field of `S`, then its defining equation must be available at the seal site;
* if `fi` is opaque in `S`, no defining equation is required to remain available after sealing;
* any fields of `e` not mentioned by `S` are hidden by the seal.

Result type:

* `seal e as S : S`.

Meaning of the resulting package:

Operationally and for KCore purposes, a sealed package carries:

* runtime members for the non-erased fields of `S`;
* manifest compile-time equations for the transparent compile-time fields of `S`; and
* opacity metadata for the opaque fields of `S`, together with any runtime representation required by their declared
  quantities.

Projection and use:

* If `p : S` and `S` contains a field `f`, then `p.f` is well-typed at the declared field type of `f`, with `this`
  interpreted as `p`.
* If `f` is opaque, `p.f` is nameable and may appear in later types and in client code, but its defining equation is
  hidden across the seal.
* If `f` is a transparent compile-time field, `p.f` behaves as a manifest compile-time member and participates in
  definitional equality according to §14.3.
* If `f` is a non-erased term field, `p.f` is an ordinary term projection. Its runtime behavior is ordinary field
  selection. Its compile-time transparency across the seal is governed by §14.3.
* The receiver of package-member selection may be any pure term of signature type. It need not be a syntactic variable
  or a special stable path.
* This does not introduce implicit dereference through `var`; a `var`-bound name still denotes the underlying `Ref`
  object (§8.5.1), so selecting package members through mutable state requires an explicit read in the ordinary way.

Equality and non-generativity:

* `seal` is pure and non-generative. It does not create fresh nominal identities by itself.
* If `e1` and `e2` are definitionally equal, then `seal e1 as S` and `seal e2 as S` are definitionally equal.
* If `e : S`, then `seal e as S` is definitionally equal to `e`.
* Member selection is an ordinary dependent elimination form. Equality of receivers propagates through it by the
  existing equality machinery of §5.6.1.
* Therefore ordinary aliasing preserves member identity. If `let m = p`, then `m.f` and `p.f` denote the same member up
  to ordinary definitional equality.

No implicit subtyping:

* Kappa has no ambient structural subtyping for records or signatures.
* Hiding fields or opaqueness manifest definitions occurs only through explicit `seal ... as ...`.

<!-- types.records.anonymous_existential_packages_exists -->
#### 5.5.11 Anonymous existential packages (`exists`)

The `exists` surface form is anonymous sealed-package sugar. It does not introduce a distinct KCore type former.
Instead it elaborates to the sealed-package machinery of §5.5.10.

Type syntax:

```text
existsType   ::= 'exists' existsBinder+ '.' type
existsBinder ::= ident
               | '(' ident ':' type ')'
```

Grammar (amends the type grammar):

```text
quantifiedType ::= forallType
                 | existsType
                 | typeArrow
```

`exists a. T` is sugar for `exists (a : Type). T`.

Multiple binders associate to the right and may depend on earlier binders, exactly as for `forall`.

Surface-name restrictions:

* Within one `exists` type, witness binder names must be pairwise distinct.
* A witness binder name MUST NOT be `value`, because `value` is the fixed payload-field name used by this sugar.

Canonical elaboration of existential types:

A surface existential type

```kappa
exists (a1 : S1) ... (an : Sn). T
```

elaborates to the closed signature type

```text
(opaque 0 ⟨wit1⟩ : S1,
 ...
 opaque 0 ⟨witn⟩ : Sn[⟨wit1⟩/a1, ..., ⟨wit(n-1)⟩/a(n-1)],
 value : T[⟨wit1⟩/a1, ..., ⟨witn⟩/an])
```

where `⟨wit1⟩ ... ⟨witn⟩` are fresh implementation-internal member labels that are not source-addressable.

Consequences:

* Source binder names in `exists` are binders only; they are not public field labels.
* Alpha-equivalent existential types are equivalent because elaboration uses the same anonymous internal witness-member
  shape.
* There is no special case for record-valued bodies: `exists (a : Type). (x : a, y : a)` elaborates to a package with
  one payload field `value`, not to a widened record with fields `x` and `y` at the outer level.
* The only ordinary source-visible member guaranteed by this sugar is `value`.

Erasure and witness quantity:

* Each existential witness elaborates to an `opaque 0` member.
* Therefore existential witnesses are quantity-`0` values and are erased at runtime under the ordinary erasure rules of
  §14.4, except where the existing special treatment of constraint evidence in §5.1.3 requires retention.
* This means `exists` packages anonymous quantity-`0` witnesses together with a payload.
* The fixed witness quantity used by this surface sugar is a property of `exists` itself. It does not restrict ordinary
  user-written compile-time members, which may carry any quantity annotation allowed by §5.1.4.1.
* Transparent dependent records / Σ already remain available for user-written packages that should stay transparently
  projectable rather than sealed.

Introduction:

A value of existential type is introduced by ordinary sealing:

```kappa
let p : exists (a : Type). Option a =
    seal
        (a = Int,
         value = Some 1)
    as exists (a : Type). Option a
```

```kappa
let mkBorrowedGetter :
    forall (ρ : Region).
    (&[ρ] x : Box Int) ->
    exists (ρ' : Region). (ρWitness : Region, get : BorrowView ρ' (Box Int))
```

For purposes of surface signature matching only, `seal e as exists (a1 : S1) ... (an : Sn). T` checks `e` against the
anonymous existential surface view

```text
(opaque 0 a1 : S1,
 ...
 opaque 0 an : Sn,
 value : T)
```

with the ordinary dependency substitutions induced by the binder order.

After checking, the witness field names `a1 ... an` are rewritten to the canonical internal witness labels
`⟨wit1⟩ ... ⟨witn⟩`. Those source names do not survive as ordinary projection labels outside the seal.

Existential binders may range over any type admitted by §5.1.3, including `Region`, `Quantity`, `Universe`, row types,
and label types.

Projection:

* If `p` has existential type, `p.value` is an ordinary payload projection.
* The typechecker may represent the type of `p.value` internally using the hidden witness selections carried by `p`.
* Source code that needs stable names for those hidden witnesses must use `open ... as exists ...` below.

Unpacking:

```text
openExistsExpr ::= 'open' expr 'as' 'exists' '(' ident (',' ident)* ')' '.' pattern 'in' expr
```

Grammar amendment:

```text
expr ::= ... | openExistsExpr
```

`open ... as exists ... in ...` has the same precedence class as other binding expressions such as `let ... in`.

Examples:

```kappa
let size =
    open p as exists (a). x in
        match x
        case Option.None   -> 0
        case Option.Some _ -> 1
```

Rules:

* In `open e as exists (a1, ..., an). pat in body`, the expression `e` must have a type definitionally equal to
  `exists (b1 : S1) ... (bn : Sn). T`.
* The number of witness binders in the `open` form must equal the number of existential binders in the type of `e`.
* The witness binders `a1 ... an` must be pairwise distinct.
* No witness binder introduced by the `open` form may also be bound by `pat`.
* `pat` must be irrefutable for the payload type `T` after substituting the locally bound witnesses.
* The binders `a1 ... an` are introduced in `body` as ordinary quantity-`0` local bindings at sorts/types
  `S1 ... Sn`.
* The names bound by `pat` are introduced in `body` exactly as in an ordinary irrefutable `let` binding.
* Any implicit-field unpacking induced by binding `pat` occurs exactly as specified in §5.5.9.

Normative elaboration:

`open e as exists (a1, ..., an). pat in body` behaves as if elaborated to:

```kappa
let __pkg = e
let 0 a1 = __pkg.⟨wit1⟩
...
let 0 an = __pkg.⟨witn⟩
let pat = __pkg.value
in body
```

where `__pkg` is fresh and the witness selections are the same hidden members determined by the elaborated existential
type of `e`.

Relationship to `seal` and equality:

Because `exists` is anonymous-package sugar over §5.5.10:

* it is pure and non-generative;
* equality of receivers propagates to equality of the hidden witnesses and of `.value` by the ordinary equality rules
  for projections and seals;
* ordinary aliasing preserves existential identity.


<!-- types.propositions -->
### 5.6 Propositions via booleans and propositional equality

Kappa supports propositional equality as a built-in inductive family.

<!-- types.propositions.propositional_equality_type_positions -->
#### 5.6.1 Propositional equality (`=`) in type positions

The propositional equality type `x = y` is a built-in inductive family. It is exported by `std.prelude` as if by:

```kappa
data (=) (@0 a : Type) (x : a) : a -> Type =
    refl : x = x
```

Formation:

* If `A : Type` and `x : A` and `y : A`, then `(x = y) : Type`.

Introduction:

* `refl` is the canonical reflexivity proof and inhabits `x = x`.

Definitional equality interaction:

* If `x` and `y` are definitionally equal, then `refl` may be used at type `x = y`.

Eliminator (`subst`):

```kappa
subst :
    forall (@0 a : Type) (@0 P : a -> Type) (@0 x : a) (@0 y : a).
    (@0 p : x = y) -> P x -> P y
```

Normative behavior:

* `subst` is a primitive.
* When the proof argument is definitionally `refl`, `subst refl v` reduces to `v`.
* The proof argument `p` has quantity `0`. Equality evidence is erased at runtime and exists only to guide the
  typechecker.

Derived combinators:

```kappa
let sym (@0 a : Type) (@0 x : a) (@0 y : a) (@0 p : x = y) : y = x =
    subst @a @(\i -> i = x) @x @y p refl

let trans (@0 a : Type) (@0 x : a) (@0 y : a) (@0 z : a)
          (@0 p : x = y) (@0 q : y = z) : x = z =
    subst @a @(\i -> x = i) @y @z q p

let cong (@0 a : Type) (@0 b : Type) (@0 f : a -> b)
         (@0 x : a) (@0 y : a) (@0 p : x = y) : f x = f y =
    subst @a @(\i -> f x = f i) @x @y p refl
```

Pattern matching:

* Matching on `refl` performs equality refinement by index unification. In a branch

  ```kappa
  match p
    case refl -> body
  ```

  the typechecker treats the two sides of the equality as definitionally equal within `body`.

<!-- types.propositions.boolean_type_coercion_type_positions -->
#### 5.6.2 Boolean-to-type coercion in type positions

Boolean expressions are not implicitly coerced to types in arbitrary type positions.

The coercion

```kappa
b  ⟼  b = True
```

applies only in the following forms:

* as the annotation of an implicit binder `(@x : b)`, which elaborates to `(@0 x : b = True)`;
* on the left of `b => T`, which elaborates to `(@0 _ : b = True) -> T`;
* as the declared type of a record or tuple field, where the field type is itself a bare boolean expression, e.g. `ok :
  id == 1`, which elaborates to `ok : (id == 1) = True`.

In all other type positions, users MUST write `(b = True)` explicitly.

This preserves lightweight boolean-proposition sugar for implicit evidence and dependent records/tuples while avoiding
accidental coercions elsewhere in the type language.

Examples:

```kappa
let r : (id : Int, ok : id == 1) = (id = 2, ok = _)
-- parses as: ok : (id == 1) = True
```

To express a "false branch" proposition explicitly, use `b = False`.

<!-- types.splicing -->
### 5.7 Monadic splicing (`!`)

The prefix operator `!` is used only within a `do` block as a monadic splice.

Compile-time term and type generation use `Syntax` and top-level `$(...)` splicing (§5.8.2), not `!`.

<!-- types.splicing.term_level_splice_monadic_contexts -->
#### 5.7.1 Term-level `!` in monadic contexts

Within a `do` block (§8), `!e` runs the monadic computation `e` and yields its result in an expression context.

Example:

```kappa
let main : UIO Unit = do
    let x = !readInt
    let y = !readInt
    println (x + y)
```

`!` is valid only inside an expression that is being elaborated as part of the nearest enclosing `do` block.
It does not cross lambda, local-function, quote, or type boundaries.
If `!` appears inside a nested lambda or local function, that nested body must itself contain an enclosing `do`;
otherwise the occurrence is ill-formed.

Normative elaboration order:

* Before translating `!`, the compiler first lowers ordinary surface expression sugar inside the containing do-item to
  the core expression fragment used for expression elaboration.
  In particular, guarded cases, short-circuit boolean lowering, and similar constructs are lowered first.
* Fix a `do` block whose monad is `m`.
  Define a translation `Splice_m[e]` that turns an expression `e` containing zero or more occurrences of `!` into a
  monadic computation of type `m A`, where `A` is the ordinary value type of `e` after removing the `!` markers.

The translation is defined recursively and preserves Kappa's ordinary left-to-right evaluation order:

* Pure atom `a` with no evaluated subexpressions:
  `Splice_m[a] = pure a`

* Splice:
  `Splice_m[!e] = e`

* Application spine `e0 e1 ... en`:
  translate `e0, e1, ..., en` left-to-right, bind their results to fresh temporaries `t0, t1, ..., tn`, and return:
  `pure (t0 t1 ... tn)`

* Tuple, record, list, set, and map literals:
  translate element or field expressions in the language's ordinary left-to-right evaluation order, bind fresh
  temporaries in that same order, and return the corresponding pure literal built from those temporaries.

* Pure projection or other non-short-circuiting expression form:
  translate subexpressions in ordinary left-to-right order and rebuild the pure form from the resulting temporaries.

* Conditional:
  `Splice_m[if c then t else f] = Splice_m[c] >>= \c' -> if c' then Splice_m[t] else Splice_m[f]`

* Match:
  `Splice_m[match s case p1 -> e1 ... case pn -> en] =
     Splice_m[s] >>= \s' ->
       match s'
       case p1 -> Splice_m[e1]
       ...
       case pn -> Splice_m[en]`

This translation is normative.
Implementations may realize it by an equivalent extraction algorithm, but they MUST preserve all of the following:

* left-to-right evaluation order of translated subexpressions;
* exactly-once evaluation of each splice operand;
* branch-local sequencing for `if` and `match`, so that splices in untaken branches are not executed.

Use in do-items:

* If a do-item expression contains one or more occurrences of `!`, that expression is first translated by `Splice_m[-]`.
* The resulting monadic computation is then elaborated by the ordinary do-item rule for that position.
* Consequently:
  * a final do-item expression containing `!` contributes the translated computation as the block result;
  * `let pat = expr` inside `do` may contain `!`, in which case the translated computation is sequenced first and `pat`
    is then bound to its value;
  * any other do-item form that syntactically contains an expression subterm uses the same translation on that subterm
    before applying its ordinary elaboration rule.

<!-- types.macros -->
### 5.8 Hygienic syntax values and macros

<!-- types.macros.syntax_type_quote_syntax -->
#### 5.8.1 `Syntax` type and quote syntax

Kappa provides a built-in type family for inspectable, hygienic syntax values:

```kappa
Syntax : Type -> Type
```

If `expr : t`, then:

```kappa
'{ expr }
```

has type `Syntax t`.

Splice syntax inside syntax quotes:

```kappa
${ s }
```

requires `s : Syntax t` and inserts the quoted sub-expression into the surrounding quote.

Note: `${...}` inside a quoted block is distinct from the string-interpolation `${...}` syntax of §4.3.4. The two forms
occur in disjoint lexical contexts (strings vs. quotes).

<!-- types.macros.top_level_syntax_splicing -->
#### 5.8.2 Top-level syntax splicing

Outside quoted blocks, `$(s)` is an elaboration-time splice in term and type positions.

* In a term position expecting type `t`, `$(s)` requires either `s : Syntax t` or `s : Elab (Syntax t)`.
* In a type position, `$(s)` requires either `s : Syntax Type` or `s : Elab (Syntax Type)`.
* `$(s)` consumes `Syntax`, or an `Elab` action producing `Syntax`, not `Core` or `Code`. Staged code is executed only
  via `runCode` on `ClosedCode` (§5.9.6). Semantic reflection values of §5.8.5 must first be reified back to `Syntax`
  before they can be spliced.
* If `s : Elab (Syntax t)`, the `Elab` action is executed at the splice site and its resulting `Syntax t` is spliced as
  if that syntax had been supplied directly.
* `$(...)` and `reifyCore` are the only normative re-entry points from elaboration-time reflection to object-language
  code. Implementations MUST NOT provide a generic splice or coercion from arbitrary compile-time values, rows, labels,
  constraints, lexical contexts, or exact-equality witnesses directly into ordinary runtime terms or types.
* `$(s)` is the only elaboration-time splice form for `Syntax`.
* `!` remains reserved for the monadic splice form inside `do` blocks (§5.7.1) and is not a synonym for `$(...)`.

<!-- types.macros.functions -->
#### 5.8.3 Macros as ordinary functions

A macro is an ordinary elaboration-time function whose result type is `Syntax t` or `Elab (Syntax t)`:

```kappa
myMacro : Syntax Int -> Syntax Int
let myMacro e = '{ ${e} + 1 }

let x = $(myMacro '{ 10 })
```

There is no separate macro declaration kind or shadowing regime; macros are ordinary term definitions invoked through elaboration-time splicing.

A macro MAY inspect and transform code either at the surface level through `Syntax` or at the semantic level through the
reflection API of §5.8.5. Semantic reflection does not introduce a second user-visible source language or a separate
macro lookup space; it is an elaboration-time interface for reasoning about ordinary Kappa terms, types, rows, labels,
and constraints after elaboration.

<!-- types.macros.hygiene -->
#### 5.8.4 Hygiene

Syntax quotes are hygienic.

* Quoted binders are hygienic.
* Spliced syntax preserves its own binding structure.
* Generated binders are alpha-renamed as needed to avoid capture.

<!-- types.macros.surface_syntax_inspection_and_origins -->
#### 5.8.4.1 Surface syntax inspection and origins

Surface macros need a stable way to analyze `Syntax` itself, not only elaborated `Core`.

A conforming implementation MUST provide either:

* quotation-pattern matching on `Syntax t`, or
* an observationally equivalent stable public `Syntax` inspection API,

sufficient to distinguish the surface constructs of v0.1 that may arise before semantic elaboration.

At minimum, this coverage includes:

* identifiers and operator names;
* literals and prefixed-string fragments;
* application spines;
* lambdas, lets, `if`, and `match`;
* explicit and implicit arguments;
* type ascriptions;
* records, packages, projections, and field punning;
* variant injections and residual-row syntax;
* quotes, splices, and nested macro syntax;
* comprehensions before normalization; and
* source / synthetic origin metadata.

Implementations MUST additionally provide elaboration-time operations equivalent to:

```kappa
SyntaxOrigin : Type

syntaxOrigin :
    forall (@0 t : Type).
    Syntax t -> SyntaxOrigin

withSyntaxOrigin :
    forall (@0 t : Type).
    SyntaxOrigin -> Syntax t -> Syntax t
```

Normative meaning:

* `syntaxOrigin s` returns the current origin information of `s`, corresponding to the source-origin / synthetic-origin
  model of §17.2.1.
* `withSyntaxOrigin o s` returns syntax observationally equal to `s` except that diagnostics, navigation, and rendered
  output treat `o` as the nearest origin of the resulting syntax.
* Surface syntax inspection MUST preserve hygiene and MUST NOT expose mutable parser internals, unstable node IDs, or
  implementation-defined memory identities as part of the portable contract.

<!-- types.macros.reflection_api -->
#### 5.8.5 Reflection API

Implementations MUST provide two reflection tiers:

1. surface reflection over `Syntax t`, preserving user-written structure, quoting, splicing, and hygiene; and
2. semantic reflection over elaborated core terms in explicit lexical contexts.

The semantic tier is an internal elaboration-time meta-language for reasoning about ordinary Kappa code.
It is not a second user-visible source language.

Built-in elaboration-time reflection types:

```kappa
CoreCtx : Type
Core    : CoreCtx -> Type -> Type
CoreEq  :
    forall (@0 Γ : CoreCtx) (@0 t : Type).
    Core Γ t -> Core Γ t -> Type
Symbol  : Type
```

`Core Γ t` denotes an elaborated, well-scoped core term of object-language type `t` in lexical context `Γ`.

`CoreEq x y` denotes macro-level exact equality between reflected core terms `x` and `y` of the same object-language
type in the same lexical context.

`Core` applies uniformly to reflected terms, reflected types, reflected rows, reflected labels, and reflected
constraints. For example:

* `Core Γ Int` is a reflected term of type `Int`;
* `Core Γ Type` is a reflected type expression;
* `Core Γ RecRow`, `Core Γ VarRow`, and `Core Γ EffRow` are reflected row expressions;
* `Core Γ Constraint` is a reflected constraint expression.

`CoreCtx`, `Core Γ t`, `CoreEq x y`, and `Symbol` are compile-time reflection types. Values of these types are
elaboration-time only, are erased, and MUST NOT be required at runtime or by `runCode`.

Implementations MUST provide elaboration-time operations equivalent to:

```kappa
asCore :
    forall (@0 t : Type).
    Syntax t -> exists (Γ : CoreCtx). Core Γ t

asCoreIn :
    forall (@0 Γ : CoreCtx) (@0 t : Type).
    Syntax t -> Core Γ t

reifyCore :
    forall (@0 Γ : CoreCtx) (@0 t : Type).
    Core Γ t -> Syntax t

inferType :
    forall (@0 Γ : CoreCtx) (@0 t : Type).
    Core Γ t -> Core Γ Type

whnf :
    forall (@0 Γ : CoreCtx) (@0 t : Type).
    Core Γ t -> Core Γ t

normalize :
    forall (@0 Γ : CoreCtx) (@0 t : Type).
    Core Γ t -> Core Γ t

proveDefEq :
    forall (@0 Γ : CoreCtx) (@0 t : Type).
    (x : Core Γ t) -> (y : Core Γ t) -> CoreEq x y

defEq :
    forall (@0 Γ : CoreCtx) (@0 a : Type) (@0 b : Type).
    Core Γ a -> Core Γ b -> Bool

reflCoreEq :
    forall (@0 Γ : CoreCtx) (@0 t : Type) (@0 x : Core Γ t).
    CoreEq x x

symCoreEq :
    forall (@0 Γ : CoreCtx) (@0 t : Type) (@0 x : Core Γ t) (@0 y : Core Γ t).
    CoreEq x y -> CoreEq y x

transCoreEq :
    forall (@0 Γ : CoreCtx) (@0 t : Type)
           (@0 x : Core Γ t) (@0 y : Core Γ t) (@0 z : Core Γ t).
    CoreEq x y -> CoreEq y z -> CoreEq x z

substCoreEq :
    forall (@0 Γ : CoreCtx) (@0 t : Type)
           (@0 P : Core Γ t -> Type)
           (@0 x : Core Γ t) (@0 y : Core Γ t).
    CoreEq x y -> P x -> P y

headSymbol :
    forall (@0 Γ : CoreCtx) (@0 t : Type).
    Core Γ t -> Option Symbol

sameSymbol :
    Symbol -> Symbol -> Bool
```

Normative meaning:

* `asCore` elaborates and typechecks its input using the ordinary elaborator and returns the resulting lexical context
  together with the elaborated core term.
* `asCoreIn` elaborates and typechecks its input using exactly the supplied lexical context `Γ`. If the syntax value is
  ill-scoped in `Γ`, or if ordinary elaboration at that source site would require a different lexical context,
  compilation fails with an elaboration-time error.
* `reifyCore` reifies a core term back to hygienic `Syntax`. Consequently `$(reifyCore e)` is the canonical way to
  splice the result of semantic reflection.
* `inferType`, `whnf`, `normalize`, `defEq`, and `proveDefEq` use the same elaboration, normalization, and
  definitional-equality machinery that ordinary Kappa elaboration would use at that source site.
* `proveDefEq x y` succeeds iff the elaborator judges the reflected core terms `x` and `y` definitionally equal under
  the ordinary reduction rules and the current visibility / opacity environment. Otherwise it is an elaboration-time
  error.
* `defEq` is the Boolean convenience query. For reflected terms of the same object-language type and lexical context,
  `defEq x y = True` iff `proveDefEq x y` would succeed.
* `reflCoreEq`, `symCoreEq`, `transCoreEq`, and `substCoreEq` are the witness-level exact-equality operations for
  semantic reflection.
* `substCoreEq reflCoreEq v` reduces to `v`.
* `headSymbol` returns `Some s` only when the weak-head-normal form of the given core term has a global declaration
  head. It returns `None` for variables, binders, locals, literals, and other non-global heads.
* `sameSymbol` compares resolved declaration identity, not spelling. Module aliases, re-export paths, import style, and
  local qualification do not affect symbol identity.

Exact equality for macros:

* `CoreEq` and `proveDefEq` are the witness-bearing macro-level exact-equality facilities.
* They are distinct from propositional equality (`=`), `Equiv`, and any user-defined comparison function.
* `defEq`, `proveDefEq`, `whnf`, `normalize`, and `substCoreEq` MUST obey the current module's ordinary visibility,
  opacity, `unhide`, and `clarify` rules. In particular, an opaque definition remains opaque unless ordinary elaboration
  at that source site could unfold it.

Shared-context discipline:

* The context witness returned by `asCore` is the lexical context of the reflected term.
* `asCoreIn` allows additional syntax values to be elaborated against an already-known lexical context witness.
* Semantic reflection operations MUST preserve scope. No API operation may construct an ill-scoped `Core Γ t`.
* Operations that inspect or introduce binders MUST make the induced context extension explicit in their types or in the
  returned existential package.
* Reification preserves binding structure up to hygiene-preserving alpha-renaming.

Mandatory structural coverage:

Implementations MUST additionally provide a typed constructor / destructor API sufficient to inspect and construct
reflected core terms that may arise from elaboration of v0.1 surface programs. At minimum this coverage includes:

* variables and local binders;
* global references;
* explicit and implicit application;
* lambda and let;
* Π-types and Σ-types;
* records, package members, and projections;
* data constructors and constructor-field projections;
* `match`;
* universes and equality;
* variant injections and residual rows;
* record, variant, and effect rows.

This constructor / destructor API MUST be scope-safe and typed. Ill-scoped or ill-typed reflected core terms MUST be
unrepresentable through the public reflection interface.

The reflection API MUST additionally provide an elaboration-time operation equivalent to:

```kappa
lowerComprehension :
    forall (a : Type).
    RawComprehension a -> ComprehensionPlan a
```

`RawComprehension a` is a stable compile-time reflection type for user-written comprehensions.
It is not an alias for the implementation's internal `Syntax`, `KSyntax`, or KFrontIR node types.

A conforming implementation MUST provide a stable public inspection API for `RawComprehension`
sufficient to observe, at minimum:

* the optional carrier prefix, if any,
* the delimiter kind (`[ ... ]`, `{| ... |}`, or `{ ... }`),
* the clause sequence in source order,
* clause kinds (`for`, `for?`, `let`, `let?`, `if`, `join`, `left join`, `group by`, `order by`, `skip`, `take`,
  `distinct`, `distinct by`),
* binder patterns and guards,
* the yield form (element yield vs key/value yield),
* and any map-conflict clause.

`ComprehensionPlan a` is a stable normalized compile-time reflection type.

A conforming implementation MUST provide a stable public inspection API for `ComprehensionPlan`
sufficient to observe, construct, and decompose, at minimum, normalized plan nodes equivalent to:

* a singleton empty-row source;
* source introduction from `IntoQuery.toQuery`;
* row extension by an irrefutable generator;
* refutable generator filtering / filter-map;
* row-local `let`;
* refutable row-local `let?`;
* row filtering;
* ordering;
* paging (`skip`, `take`);
* row deduplication (`distinct`);
* keyed deduplication (`distinct by`);
* inner join;
* left join;
* grouping;
* final element projection;
* final key/value projection;
* terminal collection kind:
  * list-like element collection,
  * set-like element collection,
  * or key/value collection;
* and any terminal map-conflict policy.

Two implementations MAY use different internal representations, but any public reflection API for
`ComprehensionPlan` MUST be rich enough to recover the normalized semantics above without reparsing source text
or inspecting compiler-private IR.

`lowerComprehension` performs the normative lowering of §10.10.

The reflection API MUST additionally provide elaboration-time convenience operations equivalent to:

```kappa
renderSyntax :
    forall (@0 t : Type).
    Syntax t -> String

typeOfSyntax :
    forall (@0 t : Type).
    Syntax t -> Syntax Type

whnfSyntax :
    forall (@0 t : Type).
    Syntax t -> Syntax t

normalizeSyntax :
    forall (@0 t : Type).
    Syntax t -> Syntax t

defEqSyntax :
    forall (@0 a : Type) (@0 b : Type).
    Syntax a -> Syntax b -> Bool

headSymbolSyntax :
    forall (@0 t : Type).
    Syntax t -> Option Symbol
```

Normative meaning:

* `renderSyntax` returns a deterministic, human-oriented rendering of the given syntax value. The exact pretty-printing
  style is implementation-defined, but the rendering MUST preserve binding structure and hygiene up to alpha-renaming.
* `typeOfSyntax s` elaborates `s` at the current call site, infers its type using the ordinary elaborator, and reifies
  that type back to `Syntax Type`.
* `whnfSyntax s` elaborates `s` at the current call site, computes weak-head normal form using the ordinary
  normalization machinery, and reifies the result back to `Syntax`.
* `normalizeSyntax s` elaborates `s` at the current call site, computes full normalization using the ordinary
  normalization machinery, and reifies the result back to `Syntax`.
* `defEqSyntax s1 s2` elaborates both syntax values at the current call site and returns the same Boolean result that
  `defEq` would return for the resulting reflected cores.
* `headSymbolSyntax s` elaborates `s` at the current call site and returns the same result that `headSymbol` would
  return for the resulting reflected core.
* These convenience operations are pure elaboration-time queries. They MUST obey the same name-resolution,
  implicit-insertion, visibility, opacity, `unhide`, and `clarify` rules as the underlying reflection operations at the
  call site.

<!-- types.macros.restrictions_macro_effects -->
#### 5.8.6 Restrictions on macro effects

Macros, and any helper functions they invoke during elaboration-time execution, run under the elaboration-time
evaluator.

* Macros MUST be total.
* Macros MUST be deterministic with respect to their explicit inputs and any evaluator-supplied input transcript: the
  same arguments and the same transcript MUST produce the same `Syntax`.
* In package mode (§2.3.2), the elaboration-time evaluator provides no `IO` capability. Any attempt to perform
  observable side effects (including file-system access, network access, mutable global state, or equivalent
  implementation-defined effects) is a compile-time error.
* In script mode, the evaluator MAY provide a restricted elaboration-time `IO` capability. Effects performed there are
  permitted but discouraged, and are not guaranteed to be deterministic or reproducible across compilations.
* When script-mode macro execution observes external inputs, the evaluator MUST record a macro input transcript
  sufficient to identify those observations. At minimum, this transcript includes any evaluator-exposed environment or
  configuration values consulted by the macro, together with the contents or cryptographic digests of files, network
  resources, and other external data read during elaboration. This transcript is part of the hashing input described in
  §15.
* Semantic reflection queries over `Core` are pure elaboration-time queries. They MUST use the same name-resolution,
  implicit-insertion, visibility, opacity, and definitional-equality rules as ordinary elaboration at the call site.

**Termination checking.** Macro definitions are subject to the same termination checking rules as ordinary top-level
definitions (§6.4), including the unsafe/debug `assertTotal` escape hatch of §16.4. The elaboration-time evaluator uses
the same termination checker as the rest of the language. A macro accepted only via `assertTotal` is still required to
be deterministic with respect to its inputs and the macro input transcript. Non-termination of a macro during
elaboration is a compiler error. Implementations MUST detect and abort (e.g. via a deterministic step limit or
equivalent) rather than hanging indefinitely.

Successful completion of a macro or `Elab` action under the elaboration-time evaluator does not by itself establish
termination certification for any definition and does not change δ-reduction eligibility under §14.3.

If macro execution is aborted because an implementation-defined step, fuel, or resource limit is exceeded, that is a
compile-time error. It MUST NOT be treated either as proof of non-termination or as proof of termination.

These restrictions are enforced by the elaboration-time evaluator (§17.3.3).
A macro or splice that violates them is a compile-time error.

<!-- types.macros.typed_elaboration_actions_elab -->
#### 5.8.7 Typed elaboration actions (`Elab`)

Kappa provides a built-in elaboration-time effect for context-sensitive macros and DSL elaborators:

```kappa
Elab : Type -> Type

type ElabGoal : Type =
    (ctx : CoreCtx,
     expected : Option (Core this.ctx Type))

currentGoal :
    Elab ElabGoal

failElab :
    forall (@0 a : Type).
    String -> Elab a

warnElab :
    String -> Elab Unit

asCoreGoal :
    forall (@0 g : ElabGoal) (@0 t : Type).
    Syntax t -> Elab (Core g.ctx t)

withExpectedGoal :
    forall (@0 g : ElabGoal) (@0 a : Type).
    Core g.ctx Type -> Elab a -> Elab a

ensureDefEqGoal :
    forall (@0 g : ElabGoal) (@0 a : Type) (@0 b : Type).
    Core g.ctx a -> Core g.ctx b -> Elab Unit
```

Implementations MUST provide coherent evidence for `Functor Elab`, `Applicative Elab`, and `Monad Elab`.

Rules:

* `Elab` actions run only during elaboration.
* `currentGoal.ctx` is the lexical context that ordinary elaboration would use at the splice site.
* `currentGoal.expected` is the expected type at the splice site when one is available; otherwise it is `None`.
* `warnElab` emits a deterministic elaboration-time warning associated with the current source or synthetic-origin chain.
  Warnings emitted by `Elab` are diagnostics only; they do not affect typing, normalization, hashing, or interface
  identity except through their recorded origins.
* `asCoreGoal g s` elaborates `s` in the lexical context `g.ctx`. If `g.expected = Some T`, the implementation MUST use
  the same expected-type-directed elaboration that ordinary source elaboration would use at that site.
* `withExpectedGoal g T act` executes `act` with the same lexical context as `g` and with expected type overridden to
  `Some T` for the dynamic extent of `act`.
* `ensureDefEqGoal g x y` succeeds iff the ordinary elaborator judges `x` and `y` definitionally equal in `g.ctx`
  under the current visibility / opacity environment; otherwise it fails with an elaboration-time error.
* `Elab` actions MAY use the reflection operations of §5.8.5, including `asCoreIn`, `asCoreGoal`, `inferType`, `whnf`,
  `normalize`, `proveDefEq`, `defEq`, and `reifyCore`.
* `Elab` is subject to the same determinism, transcript, visibility, opacity, implicit-resolution, package-mode effect,
  and termination restrictions as other elaboration-time execution in §5.8.6.
* `Elab` does not provide a second re-entry path into object code. It re-enters object-language code only by producing
  `Syntax` for `$(...)`.
* `Elab` values are compile-time only, are erased, and MUST NOT be required at runtime or by `runCode`.

<!-- types.macros.output_round_tripping -->
#### 5.8.7.1 Output round-tripping

A conforming implementation intended for editor, diagnostic, or interface-rendering use MUST provide an observationally
equivalent registration mechanism for user-extensible output recovery.

At minimum, the mechanism MUST support:

* unexpanders, which attempt to reverse specific macro expansions or notation expansions by rewriting reified `Syntax`;
* delaborators, which translate elaborated `Core` back into more user-written `Syntax` when ordinary reification would
  otherwise expose compiler-generated structure.

Rules:

* output recovery affects only rendering;
* it MUST NOT change typing, normalization, definitional equality, hashing, coherence, or separate-compilation
  identity;
* it MUST preserve hygiene and source / synthetic origin chains as far as the recovered surface form permits;
* when output recovery is disabled or no recovery succeeds, the implementation MUST still render a faithful raw form
  derived from ordinary `Syntax` or `Core`; and
* canonical interface views, hovers, error messages, and proof states MAY consult these recovery hooks, but the
  recovered form MUST remain observationally equivalent to the underlying elaborated term.

<!-- types.staging -->
### 5.9 Staged code values

<!-- types.staging.code_closed_code -->
#### 5.9.1 `Code` and `ClosedCode`

Kappa provides separate built-in types for generative staged code:

```kappa
Code       : Type -> Type
ClosedCode : Type -> Type
```

`Code t` is typed staged code. It is generative and is not inspectable as ordinary syntax. Use `Syntax t` for
inspectable macro ASTs.

<!-- types.staging.quotation_escape -->
#### 5.9.2 Quotation and escape

MetaOCaml-style quotation syntax constructs staged code:

```kappa
.< expr >.
```

If `expr : t`, then `.< expr >. : Code t`.

Escape inside code quotes:

```kappa
.~c
```

requires `c : Code t` and splices the staged subterm into the surrounding `Code` quote.

<!-- types.staging.stage_persistence -->
#### 5.9.3 Cross-stage persistence

Cross-stage persistence is lift-based:

```kappa
trait Lift (a : Type) =
    liftCode : a -> Code a
```

A present-stage value may occur inside `.< ... >.` only if it is inserted by `liftCode`, either explicitly or via
elaboration of a simple variable occurrence.

<!-- types.staging.closing_staged_code -->
#### 5.9.4 Closing staged code

Implementations MUST provide:

```kappa
closeCode :
    forall (@0 t : Type).
    Code t -> Option (ClosedCode t)
```

`closeCode c` returns `Some cc` iff `c` is closed under the same scope-safety discipline required by `runCode`.
Otherwise it returns `None`.

If a quotation `.< expr >.` is statically known to be closed, implementations MAY elaborate it directly as
`ClosedCode t` when that type is expected.

<!-- types.staging.let_insertion_sharing -->
#### 5.9.5 Let insertion and sharing

Implementations MUST provide:

```kappa
genlet :
    forall (@0 t : Type).
    Code t -> Code t
```

Normative meaning:

* `genlet e` requests that the generated term represented by `e` be bound exactly once by a fresh generated `let` in
  the nearest enclosing generated code context that contains all of its uses.
* Repeated splices of the same `genlet` result within that enclosing generated code context MUST reuse the same
  generated binder.
* `genlet` preserves typing and observational semantics, modulo administrative let-binding.
* `genlet` MUST preserve scope safety and MUST fail with a stage error if no valid insertion point exists.

Example:

```kappa
let shared = genlet .< expensive n >.
let c = .< .~shared + .~shared >.

-- behaves like code for:
-- let __x = expensive n
-- __x + __x
```

<!-- types.staging.running_staged_code -->
#### 5.9.6 Running staged code

Staged code may be executed only when closed:

```kappa
runCode : ClosedCode t -> UIO t
```

Implementations MUST reject, or fail before execution on, any attempt to run non-closed code.

<!-- types.staging.scope_safety -->
#### 5.9.7 Scope safety

Implementations MUST prevent scope extrusion for `Code`.

* A splice that would produce ill-scoped code is a stage error.
* Implementations MAY detect this statically or with a generation-time scope-extrusion check.
* `closeCode` MUST succeed only for code values that satisfy this discipline, and `genlet` MUST NOT be usable to bypass
  it.

<!-- types.staging.reserved_extension_lane_contextual_open_code -->
#### 5.9.8 Reserved extension lane: contextual open code

v0.1 standardizes closed / closable staged code through `Code`, `ClosedCode`, `closeCode`, `genlet`, and `runCode`.
It does not standardize a user-visible contextual open-code calculus with explicit context variables.

A future revision MAY add such a calculus, provided it preserves the scope-safety, staging, and separate-compilation
invariants of this chapter.

A conforming v0.1 implementation MUST NOT expose nonportable contextual-open-code features as if they were part of the
portable `Code` contract.

<!-- types.gradual -->
### 5.10 Dynamic values, runtime representations, and checked boundaries

Kappa v0.1 provides explicit dynamic values through `std.gradual`.

This section is not an ambient gradual typing extension.

<!-- types.gradual.runtime_representations_operational_meaning -->
#### 5.10.1 Runtime representations and operational meaning

Rules:

* A value of type `DynRep a` is an ordinary runtime value. The type argument `a` is compile-time only, but the
  representation value itself is not.
* The type argument of `DynRep a` or `DynamicType a` is erased by classifier under §§5.1.4 and 14.4. Runtime checking
  therefore depends on the value of the representation, not on the erased type argument.
* A conforming implementation MUST behave as if a dynamic value stores both a runtime representation and a payload:

  ```text
  Dyn  ≃  exists (@0 a : Type). (rep : DynRep a, value : a)
  ```

* `toDynWith rep x` constructs such a package.
* `checkedCastWith repTarget d` compares `repTarget` with the stored representation of `d` using `sameDynRep`.
* If `sameDynRep repTarget repStored = Yes p`, `checkedCastWith` returns `Result.Ok` of the stored payload transported
  along `p`.
* If `sameDynRep repTarget repStored = No _`, `checkedCastWith` returns `Result.Err`.
* The implicit evidence used by `toDyn` and `checkedCast` is runtime-relevant only through its `dynRep` member. A
  conforming implementation MAY specialize or erase that evidence only after producing the required runtime
  representation.

<!-- types.gradual.dynamically_representable_types -->
#### 5.10.2 Dynamically representable types

A type `a` is dynamically representable iff a value of type `DynRep a` is available.

Portable minimum:

* Implementations SHOULD provide `DynamicType` instances at least for:
  * `Unit`, `Bool`, `Char`, `String`, `Bytes`, `Int`, `Integer`, `Float`, `Double`, and any implementation-documented
    primitive scalar types with first-order runtime representations;
  * `Option a`, `Result e a`, `List a`, and `Array a` when their argument types are dynamically representable;
  * closed record types whose field types are dynamically representable;
  * closed variant / union types whose member types are dynamically representable;
  * `std.ffi.OpaqueHandle`; and
  * imported raw host reference types exposed as opaque raw-binding types.

Portable exclusions:

* The portable minimum does not require `DynamicType` for arbitrary function types, open-row records or variants, trait
  constraints, `Dict` values, fibers, TVars, handlers, resumption values, or types whose runtime classification depends
  on erased proofs, erased indices, anonymous regions, or other compile-time-only data.
* A library or implementation MAY provide `DynRep` values for indexed, refined, or higher-order types only when it also
  provides an explicit runtime witness scheme that justifies `sameDynRep` and `checkedCastWith` for those values.

<!-- types.gradual.dynamic_values_compile_time_positions -->
#### 5.10.3 Dynamic values in compile-time positions

Rules:

* `Dyn` values are ordinary runtime data.
* A value of type `Dyn` MUST NOT be used in type position, universe position, quantity position, region position, row
  position, label position, effect-label position, or trait / constraint position except through an explicit checked
  boundary that produces an ordinary value at the demanded type.
* `Dyn` does not participate in definitional equality with any non-`Dyn` type.
* This section introduces no new convertibility rule between `Dyn` and non-`Dyn` types.

<!-- types.gradual.checked_cast_failures_blame -->
#### 5.10.4 Checked-cast failures and blame

Rules:

* `CastBlame` is opaque.
* A failed `checkedCastWith` or `checkedCast` MUST identify at least the demanded target representation.
* When boundary or source-origin information is available, an implementation SHOULD attach that information to the
  `CastBlame` value.
* A checked cast MUST NOT silently coerce a failed cast to a value of the demanded target type.

<!-- types.gradual.future_ambient_graduality -->
#### 5.10.5 Relationship to future ambient graduality

Rules:

* This section introduces only explicit dynamic values and checked boundaries.
* It does not introduce a surface unknown type, implicit consistency-based coercions, or gradual equality.
* Any future extension that does so MUST specify:
  * the precision / consistency relation used by the static system;
  * the runtime cast semantics for higher-order values;
  * the compile-time normalization strategy used when imprecise terms occur in dependent indices or proofs; and
  * the interaction with propositional equality.


---

<!-- declarations -->
## 6. Declarations and Definitions

<!-- declarations.terms -->
### 6.1 Term declarations vs definitions

There are four distinct forms:

Modifier rules:

* `private`: item is not exported (see §2.5.1).
* `public`: item is exported. It is only meaningful when the module has `@PrivateByDefault`; otherwise `public` is a
  no-op and implementations may flag it as a warning.
* `opaque`: see §2.5.2.
* These modifiers apply to ordinary declarations and definitions in this section, not to `instance` declarations
  (§12.3).

`public` and `private` are mutually exclusive.

1. **Declaration (signature only):**

   ```kappa
   [public|private] [opaque] name : Type
   ```

   This introduces a type for `name` without providing a definition. Commonly used:

    * for trait members inside `trait` bodies,
    * as top-level signatures before a `let` definition.

2. **Definition (term binding):**

   ```kappa
   [public|private] [opaque] let pat : Type = expr
   [public|private] [opaque] let pat = expr
   [public|private] [opaque] let name (x : A) (y : B) : R = body
   [public|private] [opaque] let name (inout x : A) (y : B) : R = body
   [public|private] [opaque] let name (x : A) (y : B) : R decreases measure = body
   [public|private] [opaque] let name (x : A) (y : B) : R decreases structural x = body
   [public|private] [opaque] let name (x : A, y : B) : R = body
   [public|private] [opaque] let name (x : A, y : B) : R decreases measure = body
   ```

   Any term definition must begin with `let`, except inside `let ... in` (see below).

   Decreases clause:

   Any named function definition may optionally include a `decreases` clause immediately before `=`:

   ```kappa
   let name binders... : R decreases measure = body
   let name binders... : R decreases structural x = body
   ```

   * `decreases` supplies the user-visible leading component(s) of the termination argument; its semantics are defined
     in §6.4.4.
   * If `decreases` is omitted, termination inference is attempted under §6.4.2.
   * If a `decreases` clause is present on a non-recursive definition, it has no effect.

   Pattern bindings:

   In `let pat = expr`, `pat` is a pattern (§7.6).

   `pat` must be irrefutable at the type of expr (definition below). If it is refutable, it is a compile-time error.
   (Use match / try match / comprehension refutable forms instead.)

   Top-level restriction:

   A top-level pattern binding is never treated as recursive and does not participate in the "preceding signature
   enables recursion" rule.

   Modifiers:

    * private controls export visibility (§2.5.1).
    * opaque controls definitional transparency across modules (§2.5.2).
    * Modifiers are top-level only. Block-scoped declarations inside `block` or `do` are always lexically private and
      transparent; `public`, `private`, and `opaque` are not permitted there.
    * A named-function parameter may also be written `(inout x : A)`; this is surface sugar specified in §8.8 rather
      than a core binder form.
    * Apart from `inout`, named-function parameter binders use the same binder forms as lambda binders (§7.2). This
      includes wildcard binders such as `_`, `(_ : A)`, and `(q _ : A)`, the Unit binder `()`, and receiver-marked
      binders such as `(this : T)`, `(q this : T)`, `(this x : T)`, and `(q this x : T)`.
    * If a definition has the form `let name = \binders -> body` or `let name : T = \binders -> body`, where `name` is a
      simple binder, the lambda is treated as a named function body named `name` for `return` target resolution (§8.4).
      This applies both at top level and for local `let` bindings.
    * In a named `let` definition, the body after `=` may be written as an indented pure block suite, which elaborates
      to `block ...` as specified in §6.3.1.

3. **Projection definition:**

   ```kappa
   [public|private] projection name binders... : Type = projectionBody
   ```

   A `projection` definition introduces a computed place selector. Its detailed rules are given in §6.1.1.

4. **Pattern definition (active pattern):**

   ```kappa
   [public|private] pattern name binders... (scrutinee : A) : R = expr
   ```

   * A `pattern` definition is a top-level term definition that also marks `name` as eligible for pattern-head use under
     §7.7.
   * The final explicit binder is the scrutinee consumed when the pattern is matched; preceding explicit binders are
     pattern arguments.
   * Active patterns are ordinary `term` declarations and are imported/exported as ordinary terms.
   * Active patterns are pure functions; `R` must not be a monadic type.

<!-- declarations.terms.projection_definitions -->
#### 6.1.1 Projection definitions

A projection definition names a pure computed place selector rooted in one or more place parameters.

A projection definition contributes two same-spelling facets to the binding group of its declared name:

* a **projection facet**, used by computed-place elaboration under §§2.8.4, 5.1.7.2, 5.5.5, 8.8, and 17.3.1.3; and
* a **term facet**, denoting a first-class projector descriptor value.

Ordinary unqualified term lookup selects the term facet.
Receiver-projection sugar, computed-place elaboration, `~`, and projection-section update select the projection facet.

Example:

```kappa
projection focusedBuffer (place ed : Editor) : Buffer =
    if ed.focused == LeftPane then
        yield ed.left
    else
        yield ed.right

projection longer (place x : String) (place y : String) : String =
    if size x >= size y then
        yield x
    else
        yield y

projection degrees (place this : Angle) : Float =
    yield this.radians
```

Grammar:

```text
projectionDecl   ::= [public|private] 'projection' ident projectionBinder+ ':' type '=' projectionBody
projectionBinder ::= '(' 'place' ident ':' type ')'
                   | '(' 'place' 'this' ':' type ')'
                   | '(' 'place' 'this' ident ':' type ')'
                   | ordinary function binder except `inout`
projectionBody   ::= 'yield' expr
                   | 'if' expr 'then' projectionBody 'else' projectionBody
                   | 'match' expr NEWLINE INDENT projectionCase+ DEDENT
projectionCase   ::= 'case' pattern ['if' expr] '->' projectionBody
```

Rules:

* A projection definition is top-level only.
* `opaque` does not apply to projection definitions.
* A projection definition must contain one or more `place` binders.
* A `place` binder may be receiver-marked using `(place this : T)` or `(place this x : T)`. Such a binder is still a
  `place` binder, and it participates in dotted receiver-projection sugar under §2.8.4.
* All other binders are ordinary explicit or implicit function binders except `inout`. They may be erased and may be
  dependent.
* A projection definition is pure; its declared result type must not be monadic.
* Each reachable path of the body must end in exactly one `yield`.
* All reachable `yield` operands must have the declared result type.
* The operand of each `yield` must elaborate to either:
  * a stable place rooted in one of the declaration's `place` binders; or
  * a fully applied call to another projection definition whose yielded alternatives are themselves rooted in one of
    those same binders.
* A `place` binder may be inspected by ordinary pure expressions in the body, but may be yielded only as a place
  expression, not stored or returned as a first-class value.
* Selector-observation rule:
  * In selector positions, a `place` binder (or any stable subprojection rooted in that binder) is observed through the
    scoped borrow primitive of §17.3.1.2 rather than by consuming the selected place.
  * Concretely, any non-`yield` use of such a place is elaborated as if the implementation had introduced a fresh
    scoped borrow of that place for the dynamic extent of the containing selector expression.
* A consuming use of a `place` binder inside selector code is ill-formed.
  * Therefore selector code may inspect candidate places in order to decide which place to `yield`, but it may not move
    from them while making that decision.
* A projection definition must be fully applied at every use site. Partial application of a projection definition is
  ill-formed.

Descriptor value:

Let the `place` binders of a projection definition, in declaration order, be:

```text
(place p1 : S1) ... (place pn : Sn)
```

Let `Roots` be the closed record type:

```text
(p1 : S1, ..., pn : Sn)
```

Let the remaining ordinary binders of the declaration elaborate to the Pi telescope `Δ`.
Let `T` be the declared result type of the projection.

Then the declared name additionally denotes an ordinary term constant of type:

```text
Δ -> Projector Roots T
```

with the evident simplification when `Δ` is empty.

Consequences:

* `let foo = degrees` is well-formed and binds the projector descriptor value introduced by the declaration `degrees`.
* A projector descriptor may be stored in a record or package, returned from a function, or passed as an ordinary
  argument.
* The descriptor value is pure and contains no live place or borrow by itself.
* The full-application restriction applies to the projection facet. The ordinary term facet follows the ordinary
  term-application rules over `Δ`.

Elaboration of a fully applied projection call:

* the ordinary non-`place` binders are applied in the ordinary way to obtain a projector descriptor value
  `proj : Projector Roots T`;
* the actual `place` arguments are assembled into an internal place pack having the record shape `Roots`; and
* the surrounding demand then chooses the corresponding projector eliminator of §17.3.1.3.

<!-- declarations.terms.irrefutable_patterns_let_bindings -->
#### 6.1.2 Irrefutable patterns (for `let` bindings)

A pattern `pat` is **irrefutable** for a scrutinee type `T` iff matching `pat` against any value of type `T` cannot
fail.

The following patterns are irrefutable when well-typed:

* Wildcard `_`
* Binder `x`
* As-pattern `x@p` where `p` is irrefutable
* Typed pattern `(p : U)` where `p` is irrefutable and `T` is definitionally equal to `U`
* Tuple patterns `(p1, ..., pn)` where each `pi` is irrefutable and the scrutinee type is a tuple type of arity `n`
* Anonymous record patterns `(f1 = p1, ..., fk = pk)`, `(f1 = p1, ..., fk = pk, ..)`, or `(f1 = p1, ..., fk = pk,
  ..rest)` where each `pi` is irrefutable and the scrutinee type is a record type containing fields `f1..fk`. Extra
  fields are allowed; they are either ignored (`..`) or captured in the residual record binder `rest`. If `..rest` is
  present, `rest` is itself irrefutable because it is bound to the residual record as a whole.

Constructor patterns are refutable for `let` bindings unless the compiler can prove the scrutinee type has exactly one
constructor at the binding site (via definitional equality / index unification). Only in that
provable-single-constructor case may a constructor pattern be treated as irrefutable.

Implementations must reject any `let pat = expr` binding whose `pat` is not irrefutable for the inferred/annotated type
of `expr`.

<!-- declarations.signatures -->
### 6.2 Top-level signatures + definitions

Common forms:

```kappa
let foo (x : Int) : Int = x + 1

foo : Int -> Int
let foo x = x + 1
```

Rules:

* Every exported top-level `let` definition MUST have an explicit top-level signature.
* For purposes of this rule, export status is determined after applying `@PrivateByDefault`, `public`, and `private`.
* A top-level `let` definition has an explicit top-level signature iff either:
  * it is preceded by a separate signature declaration `name : T`, or
  * every binder in the definition has an explicit type annotation and the definition has an explicit result-type
    annotation.
* Parameter-type annotations without an explicit result type, or an explicit result type with one or more untyped
  binders, do not satisfy this rule.
* A top-level non-exported, non-recursive definition MAY omit its signature and rely on inference.
* A top-level simple value may omit its type when the previous rule permits inference.
* Recursive and mutually recursive groups remain governed by §6.4: each member of such a group MUST have a preceding
  separate top-level signature declaration.
* The language does not designate the separate-signature form as canonical for non-recursive definitions; inline
  annotations remain equally standard when they satisfy the explicit-signature rule above.

Additional rules:

* For a top-level simple-name value binding of the form `let name : T = expr`, the annotation `: T` itself counts as an
  explicit top-level signature and satisfies this section.
* The alternative "every binder in the definition has an explicit type annotation and the definition has an explicit
  result-type annotation" applies only to named function-definition forms.
* A top-level pattern binding whose binder is not a single simple name MUST NOT be exported.
* Consequently, the explicit-signature requirement of this section applies only to exported top-level `let` definitions
  whose binder is a single simple name.

<!-- declarations.let_in -->
### 6.3 `let ... in` expression

Local bindings can be written using a `let ... in` expression:

```kappa
let result =
    let
        a = 1
        b : Int = a + 2
    in
        a + b
```

Grammar:

* `let` followed by one or more bindings at a greater indentation.

  * Binding-pattern grammar (used by local `let` bindings, monadic
  `let <-` bindings, and `using`):

  ```text
  bindPat             ::= [quantity] pattern
  implicitLocalBinder ::= '(' '@' [quantity] ident ':' type ')'
  localBind           ::= 'let' bindPat [':' type] '=' expr
                        | 'let' implicitLocalBinder '=' expr
  monadBind           ::= 'let' bindPat '<-' expr
                        | 'let' implicitLocalBinder '<-' expr
  usingBind           ::= 'using' pattern '<-' expr
  ```

  Because `using` always binds its pattern at borrowed quantity `&`, explicit quantity markers are not permitted in
  `usingBind`.

* Each ordinary pattern binding is:

  ```kappa
  bindPat [ : Type ] = expr
  ```

* Followed by `in` and a single expression.

`let ... in` is itself an expression; it can appear anywhere an expression is allowed.

Refutable bindings (`let?`) are permitted only inside `do` blocks (§8.2.1) and comprehensions (§10.4.1). They are
rejected in ordinary `let ... in` expressions.

Pattern bindings in `let ... in`:

Implicit local bindings:

* `let (@q x : T) = expr` introduces the ordinary term variable `x : T` and additionally places `x` in the local
  implicit context of §7.3.3 from the binding site onward.
* `let (@q x : T) <- expr` is the monadic analogue inside `do`.
* These forms are lexically scoped only. They are not instance declarations, do not affect the module export surface,
  and do not participate in global instance search outside their lexical scope.
* Their runtime semantics are otherwise exactly those of the corresponding ordinary local binding or monadic bind.
* In v0.1 an implicit local binder binds only a simple identifier. Pattern implicit local binders are not supported.
* Because the default-quantity rule for implicit binders is given by §7.3, ordinary runtime values SHOULD typically use
  an explicit quantity such as `@ω`.

Typing:

* `let (@q x : T) = expr` is well-typed iff `expr` checks against `T`.
* `let (@q x : T) <- expr` is well-typed in a `do` block iff `expr` has type `m A` for the enclosing monad `m` and `A`
  is definitionally equal to `T`.
* In either form, `x` is introduced as an ordinary term variable of type `T` and is additionally added to the local
  implicit context of §7.3.3 from the binding site onward.
* Quantity checking for `x` uses the ordinary rules for an implicit binder annotated with quantity `q`.

* In `bindPat`, a quantity annotation applies uniformly to every variable bound by the underlying pattern.
* `let & pat = expr` is legal and binds every variable introduced by `pat` at borrowed quantity `&`. It introduces
  borrowed views of the underlying value, is used by the normative desugaring of `using` (§8.7.4), and is subject to the
  borrow-escape rules of §5.1.6. If `expr` is not already a borrowable place expression (§5.1.7.2), elaboration first
  introduces a fresh hidden temporary root scoped to the body:

  ```kappa
  let 1 __tmp = expr
  let & pat = __tmp
  in body
  ```

  where `__tmp` is fresh, inaccessible to user code, and serves as the borrow root for the duration of `body`.
* Each binding `bindPat [ : Type ] = expr` must use an irrefutable underlying pattern (§6.1.2).
* Names bound by `bindPat` are in scope in subsequent bindings and in the `in` body.
* If a type annotation is provided (`bindPat : T = expr`), `expr` is checked against `T` before destructuring.
* A later binding may introduce a name already in scope; the new binding shadows the old one in subsequent bindings and
  in the `in` body.
* If a shadowed name carries a live linear obligation (`@1`), the right-hand side of the shadowing binding must consume
  that obligation exactly once. This permits ergonomic linear state threading without requiring users to invent `x1`,
  `x2`, `x3`, ...

Refutable form:

A `let?` binding is a *refutable* binding. The form `let? pat = expr` matches `expr` against `pat`. If the match
succeeds, variables from `pat` are bound. If the match fails, the enclosing construct's semantics determine the outcome:

* In a comprehension clause, the current row is dropped.
* In a `do` block, the binding requires `Alternative m` (or equivalent short-circuiting structure); on failure, the
  block short-circuits via `empty` (see §8.2.1).
* In ordinary `let ... in`, `let?` is not permitted. Use `match` instead.

Because the plain `let? pat = expr` form discards the refutation residue, it is permitted only when the values rejected
by `pat` are droppable under the enclosing construct's rule. In particular, Kappa rejects plain `let?` when refutation
could discard an owned value carrying a positive lower-bound usage obligation. Inside `do`, the `let? ... else ...`
form of §8.2.1 binds the failure residue explicitly instead of discarding it.

For an open variant scrutinee of type `(| ... | r |)`, the plain form is rejected unless the available row constraints
prove that every case admitted by the residual row `r` is droppable. In the absence of such proof, the compiler MUST
conservatively reject the plain form.

Elaboration (schematic):

* If `bindPat` carries quantity `&`, the binding is elaborated as a borrowed binding of the underlying pattern `pat0`
  against a borrowable place expression (§5.1.7.2), with each variable introduced by `pat0` bound at borrowed quantity
  `&`. If `e` is not already a borrowable place expression (§5.1.7.2), elaboration first introduces the fresh hidden
  root described above and then performs that borrowed binding against the hidden root.
* If `bindPat` does not carry quantity `&`, then `let bindPat = e in body` elaborates as `match e; case pat0 -> body`,
  where `pat0` is the underlying pattern of `bindPat`. This is only permitted because that underlying pattern is
  required to be irrefutable.

<!-- declarations.let_in.pure_block_expressions -->
#### 6.3.1 Pure block expressions

A pure block is an expression that introduces sequential local scope without monadic control flow.

Example:

```kappa
let f (i : Int) : Int =
    block
        data T : Type =
            T (@0 p : i = 0)

        let x = i + 1
        x + 2
```

Grammar (schematic):

```text
blockExpr     ::= 'block' NEWLINE INDENT pureBlockItem* expr DEDENT
pureBlockItem ::= localLetItem
                | localSignature
                | localNamedDef
                | localDataDecl
                | localTypeDecl
                | localTraitDecl
                | localEffectDecl
                | localInstanceDecl
                | localDeriveDecl
                | localImportDecl
                | localFixityDecl
```

Here `localLetItem` is the irrefutable local-binding form of §6.3; `localSignature` and `localNamedDef` are the local
analogues of the signature and named-`let` forms of §6.1-§6.2; and the remaining items use the same surface syntax as
their top-level counterparts, minus the top-level-only forms and modifiers listed below.

`localEffectDecl` is the local-only `scoped effect` form of §6.3.1.1. It uses the same surface syntax as an `effect`
declaration of §8.1.2, except that it MUST be prefixed by `scoped` and MUST NOT carry `public` or `private`.

Typical pure-block items therefore include forms such as:

```kappa
foo : A -> B
let foo x = ...
data T : Type = ...
type Alias = ...
trait C a = ...
instance C T = ...
derive Eq T
import std.math.(sin)
infix left 50 (++)
```

Rules:

* `block` is an expression. It may appear wherever an ordinary expression may appear and is parsed as an atomic
  expression for purposes of application and postfix forms.
* Items are scoped sequentially and apply from their declaration onward.
* The final expression determines the value of the block.
* Non-final items in a pure block are declarations or definitions only.
* A pure block may contain:
  * irrefutable local `let` bindings,
  * local signatures and named `let` definitions,
  * local `data`, `type`, `trait`, `scoped effect`, `instance`, and `derive` declarations,
  * local `import` and fixity declarations.
* `block` and `do` are the two block-scope constructs of the language. They share the local declaration forms above.
  `do` additionally admits the effectful items of §8.2.
* The following forms remain top-level only: `module`, `export`, `expect`, bare `effect`, `pattern`, and the modifiers
  `public`, `private`, and `opaque`.
* The local-only modifier `scoped` is permitted as specified in §6.3.1.1.
* Block-scoped declarations are always lexically private and transparent.
* Monadic or control-flow items such as `<-`, `let?`, `var`, `while`, `for`, `defer`, `using`, `return`, `break`, and
  `continue` are not permitted in a pure block. Use an explicit `do` block for those forms.

Indented-suite sugar:

* In a named `let` definition, if the body after `=` is an indented suite of `pureBlockItem*` followed by a final
  expression, it elaborates to an explicit `block`.
* In a lambda, if the body after `->` has the same shape, it likewise elaborates to an explicit `block`.

Local recursion:

* A local named binding is non-recursive by default.
* A local named binding (or mutually recursive local group) may be recursive only if each recursively referenced name
  has a preceding local signature declaration in the same block scope.
* Within such a local definition body, the declared local name(s) are in scope exactly as in the top-level recursion
  rule of §6.4.

<!-- declarations.let_in.pure_block_expressions.scoped_local_nominal_effect_declarations -->
##### 6.3.1.1 Scoped local nominal and effect declarations

Within a block scope (`block` or `do`), the local-only modifier `scoped` may prefix:

* `data`
* `type`
* `trait`
* `effect`

Examples:

```kappa
block
    scoped data Token : Type =
        Token

    scoped trait Keyed a =
        key : a -> String

    scoped effect Choice =
        ω choose : Unit -> Bool

    ...
```

Rules:

* `scoped` is permitted only inside `block` or `do`.
* `scoped` is not permitted on top-level declarations.
* Ordinary local `data`, `type`, and `trait` declarations without `scoped` retain the escape-capable behavior of
  §14.1.1.
* Bare `effect` remains top-level only. The only local effect form is `scoped effect`.
* Each `block` and `do` scope behaves as if elaborated under a fresh hidden nominal-scope token. The corresponding
  KCore form is specified in §17.3.1.5A.
* A scoped local declaration captures that hidden nominal-scope token in addition to its ordinary closure-converted
  free variables.
* Consequently, a scoped local family, or any transparent type, row, effect row, compile-time term, dictionary, or
  computation that still mentions it after ordinary elaboration, MUST NOT escape the lexical block scope.
* Such entities MAY cross a boundary only when the exposed interface omits the scoped local family entirely, for
  example through `seal ... as ...` with a signature that does not mention it.
* Distinct dynamic evaluations do not create fresh scoped-family identities; scoped locals remain declaration-site
  semantic families under §14.1.1.

Scoped effects:

* `scoped effect E = ...` introduces a local effect-interface constructor `E` of declaration kind `type`.
* It additionally introduces a canonical self label `E` of declaration kind `effect-label` for the same lexical scope.
* The self label is admissible in effect-row syntax, in `handle E expr with ...`, and in effect-operation selection
  `E.op`.
* Accordingly, the row entry for a scoped effect uses the same spelling in the two declaration kinds, for example
  `<[E : E | r]>`.
* The local effect-interface constructor, its canonical self label, and all operations declared in the effect body are
  scoped by the same hidden nominal-scope token.

<!-- declarations.totality -->
### 6.4 Totality and unfolding

Kappa is total by default.

Normative rule:

* A transparent definition may participate in definitional equality only if it is **termination-certified**.

In this section, “definition” includes transparent term definitions, transparent type aliases, and any other
transparent compile-time definitions whose bodies may be unfolded by δ-reduction.

Clarification:

* “total by default” in v1 applies to transparent definitions and elaboration-time computations that may affect
  typechecking, not to all runtime computations.
* Effectful runtime computations, including `while` and `for` inside `do`, may diverge.
* Such runtime divergence MUST NOT by itself make any definition eligible for δ-reduction.

<!-- declarations.totality.termination_certified_definitions -->
#### 6.4.1 Termination-certified definitions

A definition is **termination-certified** iff one of the following holds:

1. the termination checker accepts it directly under §§6.4.2, 6.4.3, and 6.4.4; or
2. it was admitted via `assertTotal`, and the implementation later separately verifies its termination and records it as
   termination-certified.

A definition accepted solely via `assertTotal` is **not** termination-certified by default.

<!-- declarations.totality.portable_conformance_boundary -->
#### 6.4.2 Portable conformance boundary

Termination checking is conservative: an implementation MAY reject a terminating recursive definition unless this section
requires acceptance.

Conformance for recursion is defined by:

1. **Soundness.**
   If an implementation accepts a recursive strongly-connected component (SCC) as termination-certified, the accepted
   termination argument MUST be well-founded.

2. **Minimum acceptance.**
   A conforming implementation MUST accept the recursive definitions and SCCs required by §§6.4.2A, 6.4.2B, 6.4.3,
   and 6.4.4.

The internal search strategy by which an implementation discovers a termination argument is NOT part of portable v1
semantics, except where that strategy is surfaced through diagnostics or tooling (§17.2.4-§17.2.5).

<!-- declarations.totality.structural_descent -->
#### 6.4.2A Structural descent

For portable v1, the structural-smaller relation `<ₛ` is the least transitive relation generated by:

1. if pattern matching on a value `v` binds a constructor argument `u`, then `u <ₛ v`;
2. if `u <ₛ v`, then `C (..., u, ...) <ₛ C (..., v, ...)` provided:
   * the same constructor `C` is used on both sides;
   * corresponding non-distinguished arguments are definitionally equal; and
   * the enclosing terms have the same type.

A structural parameter MUST be an explicit parameter of inductive type, or of a type belonging to a mutually inductive
family.

A recursive definition or recursive SCC is accepted by structural recursion iff, after elaboration of pattern matching,
transparent local alias expansion, and transparent projection reduction rooted in the structural parameter(s), every
recursive call strictly decreases the chosen structural parameter tuple under the lexicographic lifting of `<ₛ`.

<!-- declarations.totality.portable_well_founded_measures -->
#### 6.4.2B Portable well-founded measures

The portable user-visible measure forms of v1 are:

* a term of type `Nat`; or
* a tuple whose components are all of type `Nat`.

A bare `Nat` measure is treated as a 1-tuple. Tuples are ordered lexicographically by the ordinary `<` relation on
`Nat`.

A recursive SCC is accepted by a measure-based termination argument only if every member of the SCC is checked against a
visible measure of the same tuple arity and every recursive call strictly decreases that visible measure.

<!-- declarations.totality.recommended_inference_strategies -->
#### 6.4.2C Recommended inference strategies

Implementations SHOULD, for ergonomics, attempt one or more of the following when no explicit `decreases` clause is
written:

* structural-parameter inference;
* semantic structural descent after simplification of transparent local aliases and transparent projections;
* inference of `Nat`-valued or lexicographic `Nat`-tuple measures;
* the canonical hidden-phase scheme of §6.4.3 for mutually recursive SCCs;
* size-change termination over the SCC call graph; and
* synthesis of linear-lexicographic combinations of primitive `Nat`-valued measures.

These are implementation techniques, not additional portable conformance requirements.

<!-- declarations.totality.hidden_phase_components -->
#### 6.4.3 Hidden phase components

A mutually recursive SCC may use a hidden phase component as the final component of its effective lexicographic
measure.

For portable v1, the hidden phase component is canonical:

* let the SCC members be ordered by deterministic source-origin order:
  * first by normalized source path,
  * then by increasing source start offset within that file;
* if the SCC has size `n`, the phase of the `i`th member in that order is `n - 1 - i`.

The effective measure of a recursive definition in that SCC is its visible measure, with the hidden phase appended as
the final component.

Consequences:

* a recursive call may preserve every earlier visible measure component only if it targets a callee with strictly
  smaller phase;
* because phase is finite, every recursive cycle must strictly decrease an earlier visible measure component at least
  once;
* the hidden phase component is not written in source code and does not appear in user-visible types.

<!-- declarations.totality.explicit_decreases -->
#### 6.4.4 Explicit `decreases`

Any named function definition may optionally include exactly one `decreases` clause immediately before `=`.

Forms:

```kappa
decreases e
decreases structural x
```

Rules:

* `decreases e` is well-formed only when `e` elaborates to `Nat` or to a tuple whose components are all `Nat`.
* `decreases structural x` is well-formed only when `x` names an explicit parameter of inductive type.
* If any member of a recursive SCC has an explicit `decreases` clause, every member of that SCC MUST have one.
* Within one recursive SCC, the explicit `decreases` clauses MUST all be of one kind:
  * all `decreases e` clauses, with the same visible tuple arity; or
  * all `decreases structural x` clauses.
* For a measure-based SCC, the explicit `decreases` clauses define the visible measure referenced by §6.4.2B.
* For a structural SCC, the explicit `decreases structural x` clauses select the structural parameter(s) referenced by
  §6.4.2A.
* The only hidden component permitted by portable v1 semantics is the canonical hidden phase component of §6.4.3.
* If a `decreases` clause is present on a non-recursive definition, it has no effect.
* `decreases` does not change runtime behavior. It constrains only the termination argument used by elaboration.

Minimum portable acceptance:

* Without explicit `decreases`, a conforming implementation MUST accept:
  * direct structural recursion on a pattern-bound subterm of one inductive parameter;
  * `Nat` countdowns where the recursive call uses a predecessor-equivalent argument in a branch where positivity is
    provable;
  * mutually recursive definitions accepted by the canonical hidden-phase scheme together with one of the two preceding
    cases.
* With explicit `decreases`, a conforming implementation MUST accept any recursive definition or SCC satisfying
  §§6.4.2A, 6.4.2B, 6.4.3, and 6.4.4.

<!-- declarations.totality.internal_recursion_introduced_elaboration -->
#### 6.4.5 Internal recursion introduced by elaboration

Elaboration may introduce internal recursive helpers required by normative desugarings.

Such helpers are admissible only if either:

* the helper itself is termination-certified under the same rules as user-written definitions; or
* the helper is lowered to an internal primitive or fixpoint representation that is not unfolded by definitional
  equality.

Desugaring is not a loophole for smuggling uncertified recursion into conversion.

<!-- declarations.totality.relationship_assert_total -->
#### 6.4.6 Relationship to `assertTotal`

`assertTotal` suppresses termination checking for acceptance only.

It does not, by itself:

* establish termination certification,
* make a definition unfoldable for definitional equality,
* or discharge any obligation created by §§6.4.2, 6.4.3, 6.4.4, and 6.4.5.

Recursion policy:

There is no `let rec`.

A top-level binding is permitted to be recursive only if it has a preceding top-level signature declaration.

A mutually recursive top-level group is permitted only if every member of the group has a preceding top-level signature
declaration.

An inline annotation on a recursive definition does not by itself introduce recursion; the recursive scope is determined
from the preceding signature declarations.

<!-- declarations.expect -->
### 6.5 `expect` declarations

An `expect` declaration introduces a name and its type/signature, but does not provide a definition in the current file.

`expect` declarations are **top-level only**.

Grammar:

```text
expectDecl ::= [public|private] 'expect' expectKind ...
```

Forms:

```kappa
expect type Int
expect type String
expect type Float

expect trait Eq a
expect term toFloat : (this : Int) -> Float
expect term print   : (this : String) -> UIO Unit
```

Rules:
* expect may prefix a type, trait, or a term signature `(term name : Type)`.
* An expect declaration contributes to name resolution as if it were an ordinary declaration.
* Expect declarations participate in the module export rules in §2.5.4. In particular, under `@PrivateByDefault`, an
  expect declaration is not exported unless marked `public`.
* Each expected item must be satisfied exactly once by the compilation unit selected for the build, either by:
  * a matching ordinary definition in another source file fragment of the same module (implementation-defined), or
  * a backend intrinsic supplied by the selected backend profile (§17.6).

Errors:
* If an expect is not satisfied, it is a compile-time error.
* If multiple satisfying definitions exist for a single expect, it is a compile-time error.
* The satisfying definition must match the expected signature (up to definitional equality).

Implementations may support selecting module fragments by target or other build conditions.
For module-identity purposes, fragment suffixes are ignored exactly as specified in §2.1.
Thus files such as `main.kp` and `main.win32.kp` are fragments of the same module `main`.
In such systems, `expect` declarations in common fragments are satisfied by definitions in the selected companion
fragments.

---

<!-- expressions -->
## 7. Expressions

<!-- expressions.application -->
### 7.1 Variables and application

* Variable usage: `x`, `foo`, `map`, etc.

* Surface application syntax is left-associative for parsing:

  ```kappa
  f x y      ==   (f x) y
  ```

This left-associative parse does not determine the semantic application unit used by elaboration.
After name resolution and ordinary surface desugaring of dotted forms, the compiler elaborates each maximal
application site as one ordered application spine against the callee's Pi telescope (§7.1.3, §17.3.1).

* Application may involve any expression on the left-hand side:

  ```kappa
  (f g) x
  (if cond then f else g) x
  ```

Grammar:

```text
applicationArg  ::= expr
                  | '@' expr
applicationExpr ::= atom applicationArg*
```

An application argument of the form `@e` is an explicit implicit-argument application. It may be used only to discharge
an implicit binder of the callee. If the next binder expected by the callee is explicit rather than implicit, or if the
callee has no remaining implicit binder at that application site, the application is ill-formed.

A maximal application site is the head expression of an `applicationExpr` together with its ordered `applicationArg*`
after any enclosing dotted-form resolution has determined the actual callee and any inserted receiver argument.

<!-- expressions.application.dotted_forms -->
#### 7.1.1 Dotted forms (`.` / `?.`): qualification, static members, projection, method sugar

The `.` token is used for:

* module qualification (`std.math.sin`)
* static member selection on a type (`Vec.Cons`)
* record or package member projection (`p.x`, `d.name`, `d.(==)`, `d.Name` in any syntactic position compatible with the
  projected compile-time member)
* projection sections and receiver-projection sections (`(.field)`, `(.field1.field2)`, `(.degrees)`, `(.at i)`)
* method-call and receiver-projection sugar (`x.show`, `a.degrees`)
* record patch (`r.{ field = expr, extra := expr2, ... }`)
* projection-section update (`r.{ (.member args...) = expr }`)

The `?.` token is an additional dotted-form operator performing safe-navigation projection over `Option`:

* safe-navigation member access (`e?.member`)

The right-hand side of `?.` is restricted to member-access forms only. In v0.1 these are:

* record or package member projection,
* constructor-field projection,
* method-call and receiver-projection sugar.

Forms such as module qualification, static member selection on a type, projection sections and receiver-projection
sections, and record patch forms are ordinary dotted forms but are not reachable through `?.`.

<!-- expressions.application.dotted_forms.projection_sections_receiver_projection_sections -->
##### 7.1.1.1 Projection sections and receiver-projection sections

```kappa
(.field)                 -- \__x -> __x.field
(.field1.field2)         -- \__x -> __x.field1.field2
(.degrees)               -- \__x -> __x.degrees
(.at i)                  -- \__x -> __x.at i
```

Formation rule:

* A parenthesized expression whose content begins with `.` is a section form over a fresh receiver binder.
* The body after the leading `.` is resolved exactly as if it had been written as a dotted form on a fresh receiver
  `__x`.
* Thus:
  * `(.field1.field2)` elaborates to `\__x -> __x.field1.field2`.
  * `(.name args...)` elaborates to `\__x -> __x.name args...`, where `name args...` is resolved using the ordinary
    dotted-form and receiver-sugar rules of §§2.8.3-2.8.4.
* The binder `__x` is fresh.
* A section is well-formed only if the corresponding dotted form on `__x` is well-formed.
* Module qualification, static member selection on a type, record update, and row extension are not admitted inside a
  section body.
* A section whose body resolves to a fully applied receiver projection counts as a fully applied projection call after
  insertion of the fresh receiver.

Projection syntax also permits parenthesized operator members:

```kappa
d.(==)
d.(<=)
```

Such forms are permitted where dotted member projection is otherwise valid.

<!-- expressions.application.dotted_forms.safe_navigation_safe_navigation -->
##### 7.1.1.2 Safe-navigation (`?.`)

Let a *chain* be a maximal sequence of dotted-form suffixes attached to an atomic expression. Schematically:

```text
chainExpr   ::= atom chainSuffix*
chainSuffix ::= '.'  member applicationArg*
              | '?.' safeMember applicationArg*
```

Here `member` stands for any right-hand side admitted by ordinary dotted forms, `safeMember` stands for a right-hand
side admitted by safe-navigation member access (record projection, constructor-field projection, explicit dictionary
member projection, method-call sugar, or receiver-projection sugar), and `applicationArg*` stands for any explicit
application suffixes that follow that member access.

A chain is *plain* if it contains no `?.` suffix, and *safe* if it contains at least one.

Desugaring of safe chains:

For a chain `C` containing at least one `?.`, let the leftmost `?.` split `C` into:

* prefix `P`: the atom and all suffixes preceding the leftmost `?.`
* residual `R`: the suffixes from the leftmost `?.` onward, with its leading `?.` replaced by `.`

Let `body = desugar(__x ⟨R⟩)` recursively, where `__x` is a fresh binder typed as the payload of `P`'s type. Here `⟨P⟩`
denotes the elaboration of the prefix `P`, and `__x ⟨R⟩` denotes the expression formed by attaching the residual
suffixes `R` to the fresh binder `__x`.

Elaboration dispatches on the type of `body`:

* If `body : Option U` for some `U`, elaborate as
  ```kappa
  match ⟨P⟩
  case Option.Some __x -> body
  case Option.None     -> Option.None
  ```
  yielding `Option U`.
* Otherwise, if `body : U` where `U` is not of the form `Option _`, elaborate as
  ```kappa
  match ⟨P⟩
  case Option.Some __x -> Option.Some body
  case Option.None     -> Option.None
  ```
  yielding `Option U`.

Recursion terminates because each step strictly reduces the count of `?.` tokens. The prefix `P` must have type `Option
T` for some `T`; otherwise the chain is ill-typed.

> **Design Note – Safe-navigation and Type Inference**
>
> The `?.` operator deliberately chooses between wrapping and flattening
> based on whether the member access returns an `Option`. This choice is
> made during elaboration.
>
> As a result, `?.` does **not** work in fully generic code without type
> annotations. For example:
>
> ```kappa
> let getB x = x?.b     -- ERROR: ambiguous safe-navigation
> ```
>
> This is a deliberate design decision. We prioritize predictable behavior
> and simplicity over perfect inference in generic contexts.
>
> If you need to write generic accessors, either:
> * add explicit type annotations, or
> * use `>>=` / `|>=` (or a custom operator) for explicit control.
>
> We may revisit this in the future if the ergonomics prove too painful,
> but for v0.1 we accept this limitation.

Dispatch is performed after elaborating the prefix and residual in the following order:

* First elaborate the prefix `P`.
* Then elaborate the residual `R` under a fresh binder `__x : T`, where `P : Option T`.

If the residual type is still an unsolved metavariable after checking the enclosing declaration's other constraints,
compilation fails with:

```text
Safe-navigation `?.` requires knowing the result type of the member access
to decide whether to wrap or flatten.

The type of `b` is still unsolved at this point.

To fix this, either:
  • Add a type annotation on the parameter or the function:
      let getB (x : MyType) : Option Int = x?.b
  • Use explicit monadic composition instead:
      x?.b >>= id
      -- or
      x |>= (.b)
  • Or accept that you must annotate when using `?.` in generic code.
```

The compiler MUST NOT default to either the flattening or non-flattening branch in this situation.

Typing:

* `P` must have type `Option T` for some `T`.
* The residual body is typechecked under a fresh binder `__x : T` using the ordinary rules for dotted forms (§7.1.1,
  §2.8.3).
* If `P` is itself a borrowed view, then the success-branch binder `__x` is treated as a borrowed alias of the payload
  rather than as a fresh owned value. It inherits the same rigid borrow region and the same underlying borrow root as
  `P`, and further projections from `__x` are interpreted pathwise through that same source path under the disjoint-path
  rules of §5.1.7. Thus nested safe-navigation such as `a?.b?.c` preserves the correct overlap/disjointness behavior of
  the original borrowed path.
* If the residual body has type `U` where `U` is not of the form `Option _`, then the safe chain has type `Option U`.
* If the residual body has type `Option U`, then the safe chain has type `Option U`.

Precedence:

`?.` has the same precedence and associativity as `.`: tightest and left-associative. It is a reserved token and cannot
be redefined by user fixity declarations.

Worked examples:

Let `a : Option A`, and let the `A` / `B` / `C` members below be projections or method calls.

Case 1 - all plain fields (`A.b : B`, `B.c : C`):

```kappa
a?.b         =>  match a
                  case Option.Some __x -> Option.Some (__x.b)
                  case Option.None     -> Option.None           : Option B

a?.b.c       =>  match a
                  case Option.Some __x -> Option.Some (__x.b.c)
                  case Option.None     -> Option.None           : Option C
```

Writing `a?.b?.c` under these types is ill-typed: `__x.b : B` is not `Option _`.

Case 2 - all optional fields (`A.b : Option B`, `B.c : Option C`):

```kappa
a?.b         =>  match a
                  case Option.Some __x -> __x.b
                  case Option.None     -> Option.None           : Option B

a?.b?.c      =>  match a
                  case Option.Some __x ->
                      match __x.b
                      case Option.Some __y -> __y.c
                      case Option.None     -> Option.None
                  case Option.None     -> Option.None           : Option C
```

Writing `a?.b.c` gives:

```kappa
match a
  case Option.Some __x -> __x.b.c
  case Option.None     -> Option.None
```

when `__x.b.c : Option C`, i.e. the inner `.c` is itself a projection on `Option B` and is ill-typed unless a method
named `c` exists on `Option B`. In practice you use `?.` at each optional step.

Case 3 - mixed (`A.b : B`, `B.c : Option C`):

```kappa
a?.b.c       =>  match a
                  case Option.Some __x -> __x.b.c
                  case Option.None     -> Option.None           : Option C
```

Case 4 - mixed (`A.b : Option B`, `B.c : C`):

```kappa
a?.b?.c      =>  match a
                  case Option.Some __x ->
                      match __x.b
                      case Option.Some __y -> Option.Some (__y.c)
                      case Option.None     -> Option.None
                  case Option.None     -> Option.None           : Option C
```

In all cases, a chain of `?.` produces an un-nested `Option U`, matching Kotlin-style optional chaining.

Idioms for flattening:

To avoid nested `Option` in chains of `Option`-returning fields, use `>>=` explicitly (or the optional typed pipe `|>=`,
Appendix B.3):

```kappa
a >>= (.b) >>= (.c)
a |>= (.b) |>= (.c)
```

<!-- expressions.application.elvis -->
#### 7.1.2 Elvis operator (`?:`)

For `e : Option T` and `d : T`, the expression `e ?: d` desugars to:

```kappa
match e
  case Option.Some __x -> __x
  case Option.None     -> d
```

This form is specified directly and does not depend on any library helper name.

`?:` is right-associative at low precedence (`2`, above `|>` at `1` and below comparison / arithmetic operators). It is
a reserved token and cannot be redefined by user fixity declarations.

<!-- expressions.application.short_circuit -->
#### 7.1.2A Short-circuit boolean operators

The operator tokens `&&` and `||` are the conventional infix spellings of the ordinary prelude terms of §2.6.2.

Surface typing:

* `a && b : Bool` iff `a : Bool` and `b : Bool`.
* `a || b : Bool` iff `a : Bool` and `b : Bool`.

Elaboration and behavior:

* `a && b` elaborates as ordinary application of the prelude term `(&&)`.
* `a || b` elaborates as ordinary application of the prelude term `(||)`.
* Because those terms have canonical definitions over `Thunk Bool`, and because application uses the expected-type-
  directed suspension insertion rule of §7.1.3.1, the observable behavior is as if:

  ```kappa
  a && b  ≡  if a then b else False
  a || b  ≡  if a then True else b
  ```

* The right operand is therefore evaluated only when required by these equations.

A conforming implementation MUST preserve this short-circuit evaluation-count behavior even when it lowers `&&` and `||`
differently internally.

The spellings `&&` and `||` remain ordinary terms rather than special evaluation forms. The prelude names `and` and
`or` likewise remain ordinary terms; they do not by themselves receive special flow-sensitive treatment.

<!-- expressions.application.subsumption -->
#### 7.1.3 Application-boundary subsumption

Kappa does not support deep subtyping. However, to maintain ecosystem coherence between linear, borrowed, and
unrestricted contexts, Kappa implements a localized subsumption rule at function application boundaries.

<!-- expressions.application.subsumption.spine_pipeline -->
##### 7.1.3.1 Application-spine elaboration pipeline

To resolve a maximal application site whose head elaborates to a callee `f`, the compiler MUST elaborate the whole site
as one ordered application spine.

Conceptually, the compiler repeatedly exposes the next Pi-telescope segment of the current callee and aligns the next
remaining surface argument against the next binder in that telescope. Accepted arguments are substituted through later
binder types before elaboration continues.

Processing proceeds left-to-right.

For each next binder of the current callee:

* If the binder is implicit:
  * if the next remaining surface argument is of the form `@e`, consume it and typecheck `e` against that binder;
  * otherwise synthesize the argument by the implicit-resolution procedure of §7.3.3;
  * if neither succeeds, the application is ill-typed.

* If the binder is explicit:
  * if the next remaining surface argument is of the form `@e`, the application is ill-formed;
  * if no remaining ordinary surface argument exists, elaboration stops and the result is a partial application over the
    residual telescope;
  * otherwise let `e` be the next remaining ordinary surface argument and apply the explicit-argument pipeline below.

Explicit-argument pipeline for binder `(q_dem x : T_dem)` and supplied argument `e`:

0. **Expected-type-directed suspension insertion.**

   Let the next explicit binder demand type `T_dem`.

   * If `T_dem` is definitionally equal to `Thunk T` for some `T`, first try checking whether `e` already satisfies the
     demanded suspension type without inserting a new suspension at this same binder position.
     * If so, pass the resulting suspension value unchanged.
     * Otherwise, if `e` checks against `T`, elaborate the argument as `thunk e`.
   * If `T_dem` is definitionally equal to `Need T` for some `T`, first try checking whether `e` already satisfies the
     demanded suspension type without inserting a new suspension at this same binder position.
     * If so, pass the resulting suspension value unchanged.
     * Otherwise, if `e` checks against `T`, and `lazy e` is well-formed under §§5.2.1 and 7.2.2, elaborate the
       argument as `lazy e`.
   * If either branch above succeeds, the argument is accepted.
   * This is an expected-type-directed elaboration rule, analogous in spirit to the expected-type-directed variant
     injection rule of §5.4.3. It does not introduce a distinct family of arrow types.

1. **Outermost borrow introduction (auto-referencing).**

   If `q_dem` is exactly a borrowed quantity binder (`&` or an explicitly scoped borrowed binder such as `&[s]`), the
   compiler first checks whether `e` is a borrowable place expression under §5.1.7.2.

   * If so, elaboration first considers the temporary-borrow form of `e` and typechecks that borrowed argument against
     `T_dem`.
   * If this borrowed check succeeds, the argument is accepted and no interval-quantity subsumption step is attempted
     for that argument.
   * If `e` is not a borrowable place expression, or if the temporary-borrow form does not typecheck, elaboration
     continues to Step 2.

2. **Exact unification and ordinary quantity satisfaction.**

   Elaborate `e` against `T_dem`, obtaining an argument type `T_cap` and an available capability `q_cap` for the
   supplied expression.

   * If `T_cap` unifies exactly with `T_dem`, the compiler checks the satisfaction relation `q_cap ⊑ q_dem` as defined
     in §5.1.5.
   * If `q_cap ⊑ q_dem` holds, the argument is accepted.
   * If `q_cap ⊑ q_dem` remains undecidable because one or both quantities still contain unsolved quantity metavariables
     after processing surrounding constraints, compilation fails with an unsolved quantity-metavariable error rather
     than defaulting to any case.
   * If `T_cap` does not unify exactly with `T_dem`, continue to Step 3.

3. **Arrow subsumption (higher-order coercion).**

   If exact unification failed solely because `T_cap` and `T_dem` are both function types whose outermost binder
   quantities differ, the compiler MAY attempt outermost-arrow subsumption:

   * Let `T_dem = (q_inner_dem y : A) -> B`.
   * Let `T_cap = (q_inner_cap y : A) -> B`.
   * The parameter domain `A` and result type `B` must match up to definitional equality; only the outermost arrow
     quantity mismatch is tolerated by this step.
   * The compiler then checks both:
     * the contravariant satisfaction condition `q_inner_dem ⊑ q_inner_cap`, and
     * the ordinary outer argument-slot capability check `q_cap ⊑ q_dem` using the available capability `q_cap` of the
       supplied expression itself, exactly as in Step 2.
   * An inserted coercion or eta-expansion adjusts only the exposed function type of the argument. It does not increase
     the permitted usage of the closure value itself.
   * If both conditions hold, the argument is accepted and the compiler MUST elaborate `e` by inserting an internal
     coercion or eta-expansion whose exposed binder quantity matches the demanded type `T_dem`.
   * If either condition remains undecidable after surrounding constraints are processed, compilation fails with an
     unsolved quantity-metavariable error.
   * If either condition does not hold, the application is a compile-time error.

After an explicit or implicit argument is accepted, the compiler substitutes it through later binder types and continues
with the next binder in the same application spine.

A temporary borrow introduced while elaborating one argument of a maximal application site remains live until
elaboration of that same maximal application site finishes. It MUST NOT end merely because the implementation
internally materializes a unary prefix while processing later arguments of the same source call.

This subsumption applies only to the outermost arrow of the supplied argument at the point where that argument is
checked. It does not apply recursively inside type constructors. For example, `List ((ω x : A) -> B)` is never
implicitly coercible to `List ((1 x : A) -> B)`.

Borrow introduction for arguments demanded at quantity `&` is not an interval-quantity case of `⊑`. It is handled by
Step 1 above, via the borrow-introduction rule of §5.1.5.

The expected-type-directed suspension insertion rule applies uniformly whether the callee was written with surface sugar
such as `(thunk x : A)` / `(lazy x : A)` or with the explicit types `Thunk A` / `Need A`.

<!-- expressions.application.equality_based_subsumption_automatic_transport_insertion -->
#### 7.1.4 Equality-based subsumption (automatic transport insertion)

When checking an expression `e` against an expected type `T`, if inference yields type `S` and `S` is not
definitionally equal to `T`, the elaborator MAY attempt one equality-based transport insertion.

Operationally:

1. If `S` and `T` are syntactically the same up to replacing one subterm `u` with `v` in a type context `K[□]`, i.e.
   `S = K[u]` and `T = K[v]`, continue. Otherwise this rule does not apply.
2. Attempt to synthesize, via the implicit-resolution procedure of §7.3.3, an erased proof `p : u = v` or `p : v = u`.
3. If successful, elaborate `e` by inserting a transport through `subst` with motive `P = \z -> K[z]`.
   * If `p : u = v`, elaborate as `subst p e`.
   * If `p : v = u`, elaborate as `subst (sym p) e`.

Rules:

* This rule performs at most one rewrite step. It does not recursively search for chains of equalities or for multiple
  independently replaced subterms.
* If no such proof is synthesized, expected-type checking fails as usual.
* This is an elaboration rule only. It does not extend definitional equality.
* At an application boundary, this rule may be attempted only after Steps 1-3 of §7.1.3.1 fail to accept the supplied
  argument directly.


<!-- expressions.lambdas -->
### 7.2 Lambdas

Lambda syntax uses backslash and `->`, optionally preceded by a label:

```kappa
\ x -> x
\ x y z -> f x y z
\ x (y : Int) z -> x + y + z
\ (x : Int) -> x
\ (x : Int) (y : Int) -> x + y
\ (@t : Type) (x : t) -> x
\ (@0 t : Type) (x : t) -> x
\ (1 x : Res) -> consume x
\() -> True
exit@\ x -> x
```

Grammar:

```text
lambda  ::= [label '@'] '\' binders '->' expr
binders ::= binder+
binder  ::= ident                            -- inferred type, quantity ω
          | '_'                              -- anonymous explicit binder, inferred type, quantity ω
          | '(' ')'                          -- anonymous explicit Unit binder, quantity ω
          | '(' ident ':' type ')'           -- explicit type, quantity ω
          | '(' '_' ':' type ')'             -- anonymous explicit binder, explicit type, quantity ω
          | '(' quantity ident ':' type ')'  -- explicit type and quantity
          | '(' quantity '_' ':' type ')'    -- anonymous explicit binder, explicit type and quantity
          | '(' 'thunk' ident ':' type ')'   -- suspension sugar, quantity ω
          | '(' 'thunk' '_' ':' type ')'     -- anonymous suspension binder, quantity ω
          | '(' quantity 'thunk' ident ':' type ')' -- suspension sugar, explicit quantity
          | '(' quantity 'thunk' '_' ':' type ')'   -- anonymous suspension binder, explicit quantity
          | '(' 'lazy' ident ':' type ')'    -- memoized suspension sugar, quantity ω
          | '(' 'lazy' '_' ':' type ')'      -- anonymous memoized suspension binder, quantity ω
          | '(' quantity 'lazy' ident ':' type ')' -- memoized suspension sugar, explicit quantity
          | '(' quantity 'lazy' '_' ':' type ')'   -- anonymous memoized suspension binder, explicit quantity
          | '(' 'this' ':' type ')'          -- receiver binder, local name `this`
          | '(' quantity 'this' ':' type ')' -- receiver binder, explicit quantity
          | '(' 'this' ident ':' type ')'    -- receiver binder, local name `ident`
          | '(' quantity 'this' ident ':' type ')' -- receiver binder, explicit quantity and local name
          | '(' '@' binder_body ')'         -- implicit binder (as before)
```

Rules:

* A bare binder `x` or `_` is equivalent to `(x : _)` or `(_ : _)` respectively, with inferred type and default
  quantity `ω`.
* The binder form `()` denotes one explicit anonymous binder of type `Unit` and default quantity `ω`.
  `\() -> e` is equivalent to `\(_ : Unit) -> e`.
* The dedicated form `()` is only sugar for the default-quantity explicit `Unit` binder. To write an anonymous
  `Unit` binder with an explicit quantity, use `(_ : Unit)` or `(q _ : Unit)`.
* When `_` appears in lambda-binder position, it denotes the wildcard-binder form above, not an ordinary identifier.
* Multiple bare binders may be juxtaposed: `\x _ z -> e` ≡ `\(x : _) (_ : _) (z : _) -> e`.
* Mixing styles is permitted: `\x (_ : Int) z -> e`.
* A wildcard binder introduces an explicit parameter but no source-level term name. It is elaborated as if the binder
  were replaced by a fresh internal identifier not equal to any user-written name.
* Distinct wildcard binders in the same lambda denote distinct parameters.
* `_` in lambda-binder position is not an expression hole and does not enable placeholder-abstraction shorthand.
* The suspension-marked forms `(thunk x : A)`, `(thunk _ : A)`, `(lazy x : A)`, `(lazy _ : A)`, `(q thunk x : A)`,
  `(q thunk _ : A)`, `(q lazy x : A)`, and `(q lazy _ : A)` are the ordinary function-binder sugar of §5.2.1.
* Receiver-marked binders are explicit binders. `(this : T)` binds the receiver locally as `this`, while `(this x : T)`
  marks the binder as the receiver and binds it locally as `x`. The quantity-annotated forms `(q this : T)` and `(q this
  x : T)` are the corresponding receiver-marked variants with explicit quantity `q`.
* Method-call and receiver-projection sugar (§2.8.4) consult the receiver marker, not the local binder name chosen
  inside the function body.
* A leading label `L@` labels the lambda body for `return@L` (§8.4.1).
* Parentheses are required for typed, quantity-annotated, suspension-marked, receiver-marked, implicit, and Unit
  binders `()`; they are not required for bare identifier or bare wildcard binders.
* A lambda must contain at least one binder; there is no point-free "degenerate" lambda.
  The Unit binder `()` counts as one binder; it does not denote a binderless lambda.
* The body after `->` may be either a single expression or an indented pure block suite, which elaborates to `block ...`
  under §6.3.1.
* Multiple parameters are curried:

  ```kappa
  \ x (y : B) -> e    :   A -> B -> T
  ```

<!-- expressions.lambdas.closure_capture_quantities -->
#### 7.2.1 Closure capture and quantities

A lambda expression forms a closure over its free variables. The permitted usage of the resulting closure value in its
surrounding context is constrained by the strictest computationally relevant resource that the closure captures.

General rule:

* Elaboration infers a usage quantity or borrow-scoped obligation for the lambda value itself from the uses of its free
  variables in `body`, under the quantity rules of §5.1.5.
* A surrounding binder or context may use the closure only at a quantity compatible with that inferred obligation.

Special cases:

* Linear contamination: if the lambda captures one or more variables bound with quantity `1`, the resulting closure
  value must itself be treated as a quantity-`1` resource in the surrounding context. It cannot be used in a context
  that would permit unrestricted (`ω`) reuse.
* Borrowed capture: if the lambda captures one or more variables bound with quantity `&`, elaboration infers the hidden
  region environment of the resulting closure value.
* A closure may be checked against a type `T captures (s̄)` only if that inferred environment is contained in `{s̄}`.
* A closure whose inferred environment mentions only explicit region variables already in scope may escape under those
  same explicit binders and capture annotations.
* A closure whose inferred environment would require any anonymous local borrow region to escape is rejected by the
  skolem-escape rule of §5.1.6.
* Erased safety: if the lambda captures variables bound with quantity `0`, those variables MUST NOT appear in
  computationally relevant (`1`, `&`, `<=1`, `>=1`, or `ω`) positions inside `body`. Capturing quantity-`0` variables
  imposes no restriction on the multiplicity of the closure itself.
* Unrestricted closures: a closure may be used unrestrictedly (`ω`) only if every computationally relevant captured
  variable is itself unrestricted. Capturing only unrestricted (`ω`) and/or erased (`0`) variables is sufficient for
  this case.

<!-- expressions.lambdas.suspension_expressions_forcing -->
#### 7.2.2 Suspension expressions and forcing

Kappa provides two suspension forms:

```kappa
thunk expr
lazy expr
```

and one eliminator:

```kappa
force expr
```

Grammar:

```text
thunkExpr ::= 'thunk' expr
lazyExpr  ::= 'lazy' expr
forceExpr ::= 'force' expr
```

Typing:

* If `expr : A`, then `thunk expr : Thunk A`.
* If `expr : A`, `Shareable A` is available implicitly, and the capture restriction below is satisfied, then
  `lazy expr : Need A`.
* If `expr : Thunk A`, then `force expr : A`.
* If `expr : Need A`, then `force expr : A`.

Capture restriction for `lazy`:

* `lazy expr` is permitted only when every computationally relevant captured free variable of `expr` is unrestricted and
  is not a borrowed view.
* Compile-time-only captures are always permitted.
* This rule ensures that memoization does not silently duplicate linear or borrowed state.

Operational meaning:

* `thunk expr` does not evaluate `expr` at construction time.
* `lazy expr` does not evaluate `expr` at construction time.
* `force (thunk expr)` evaluates `expr` each time it is forced.
* `force (lazy expr)` evaluates `expr` at most once and memoizes the result.

Control-flow boundary:

* `thunk expr` and `lazy expr` behave as user-written closure boundaries for `return`, `break`, and `continue`
  resolution.

<!-- expressions.implicit_parameters -->
### 7.3 Implicit parameters (`@`)

Implicit binders:

```kappa
\ (@t : Type) (x : t) -> x
```

* `@` marks an implicit parameter.
* Call sites may supply implicit arguments explicitly using the application form `@e` of §7.1 (e.g. `f @T x`). Such an
  argument may discharge only an implicit binder.
* Trait-instance resolution is specified (see Traits), and may be used to fill implicit constraint parameters.

The default quantity of an implicit binder `(@x : T)` is determined by the classification of `T` alone; it does not
consult trait or instance search.

If `T` is a concrete constraint descriptor `C : Constraint`, or if `T` is a compile-time type in the sense of §5.1.4.1,
the default is `0`. Otherwise, the default is the unrestricted quantity `ω`.

Users may override the default by writing the quantity explicitly, e.g. `(@0 x : T)`, `(@& x : T)`, `(@&[s] x : T)`, or
`(@ω x : T)`.

Implicit arguments in Kappa are lexical, not dynamic. After elaboration, omitting an implicit argument is equivalent to
passing an ordinary in-scope term chosen by §7.3.3. Rebinding an implicit value in an inner lexical scope shadows outer
candidates of the same type; there is no dynamic rebinding mechanism.

Example:

```kappa
type Io = (print : String -> UIO Unit)

log : (@ω io : Io) -> String -> UIO Unit
let log (@ω io : Io) (msg : String) : UIO Unit =
    io.print msg

main : UIO Unit
let main =
    do
        let (@ω io : Io) = stdIo
        log "hello"
```

<!-- expressions.implicit_parameters.constraint_sugar -->
#### 7.3.1 Constraint sugar

A constraint arrow

```kappa
C => T
```

is syntactic sugar for an implicit parameter:

```kappa
(@_ : C) -> T
```

where `C : Constraint`.

As a special case for boolean proposition sugar (§5.6.2), if the left side is a `Bool` expression `b`, then

```kappa
b => T
```

elaborates to:

```kappa
(@0 _ : b = True) -> T
```
Multiple constraints associate to the right: `C1 => C2 => T` is equivalent to `(@_ : C1) -> (@_ : C2) -> T`, with each
`Ci : Constraint`.

`=>` is reserved for actual constraints, except for the boolean-proposition special case above. For ordinary proofs or
other implicit values of type `P : Type`, write the implicit binder explicitly.

<!-- expressions.implicit_parameters.expression_holes_named_holes -->
#### 7.3.2 Expression holes (`_` and named holes)

In expression position, `_` denotes a fresh anonymous typed hole.

This hole meaning applies only in expression position. In lambda-binder position, and in other parameter-binder
positions that reuse §7.2, `_` denotes the wildcard binder of §7.2.

A named typed hole has the surface form:

```text
?ident
```

Semantics:

* A hole introduces an elaboration metavariable rather than immediately requiring synthesis to succeed.
* `_` always introduces a fresh hole.
* Repeated occurrences of the same named hole within one enclosing declaration or local right-hand side refer to the
  same metavariable.
* Repeated occurrences of the same named hole MUST elaborate at definitionally equal expected types; otherwise
  compilation fails.
* Named holes do not escape their enclosing declaration or local right-hand side.

Completion rule:

* If a hole is solved during ordinary elaboration, the solved term is used.
* If any hole remains unsolved at the end of elaboration of the enclosing declaration, compilation fails.

Diagnostics for each unsolved hole MUST report at least:

* the hole name, or an implementation-generated identifier for `_`;
* the expected type;
* the local explicit context;
* relevant local implicit assumptions and constraint goals; and
* the source span of each occurrence.

If the expected type of an unsolved hole has inhabitance summary `Contractible`, the diagnostic SHOULD include the
unique inhabitant. If it has summary `Finite n` and `n` does not exceed an implementation-defined completion threshold,
the diagnostic SHOULD include all inhabitants or the corresponding completion candidates in a deterministic order.

Kappa does not provide placeholder-abstraction sugar based on holes in v0.1. Users should write an ordinary lambda
explicitly when abstraction is intended, or use existing section forms such as operator sections (§3.5.1.1) and
projection sections (§7.1.1).

<!-- expressions.implicit_parameters.implicit_resolution_instance_proof_search -->
#### 7.3.3 Implicit resolution (instance/proof search)

When elaborating an application, if the callee expects an implicit argument `(@x : G)` and the call site does not supply
one, the compiler attempts to synthesize a term of type `G`.

Resolution proceeds in this order:

1. **Local implicit context**: search lexical implicit bindings from innermost scope to outermost scope. The local
   implicit context includes:
   * implicit binders introduced by `(@x : T)` parameters, and
   * local implicit values introduced by `let (@q x : T) = ...` and `let (@q x : T) <- ...`, and
   * implicit record-field projections unpacked from bound records with implicit fields (§5.5.9),
   * explicit erased branch evidence introduced by control flow, including boolean equality evidence and constructor
     refinement evidence (§7.4.1, §7.5.4, and any later branch form that introduces such evidence),
   * local instance declarations in scope, and
   * for an in-scope implicit value or dictionary `d : Tr args`, coherent evidence obtained by projecting any declared
     supertraits of `Tr args` (§12.1.1).

   At the first lexical scope level containing one or more candidates whose types are definitionally equal to `G`:
   * if exactly one candidate is present, use it;
   * if more than one candidate is present, the implicit goal is ambiguous and compilation fails.

   Search does not continue to outer lexical levels once such a level is found.

   Ordinary implicit-value search never consults imported top-level term bindings or exported module bindings. Only the
   local implicit context is searched for non-`Constraint` goals.

2. **Trait instance resolution**: if `G` is a trait constraint (e.g. `Eq Int`) and step 1 did not yield a unique local
   candidate, attempt to resolve an instance from the instance environment using the algorithm of §12.3.1.

3. **Boolean proposition normalization**:
    * If `G` is `b = True` and `b` normalizes (by definitional equality) to `True`, synthesize `refl`.
    * If `G` is `b = False` and `b` normalizes (by definitional equality) to `False`, synthesize `refl`.

4. **Equality reflection from `==`**: If `G` is `x = y`, and the implicit context contains `p : (x == y) = True`, and an
   implicit `Eq A` is available, elaborate using:

   ```kappa
   Eq.eqSound x y p
   ```

   Equality reflection from `(x == y) = True` is sound because it uses the `eqSound` proof carried by the resolved `Eq`
   dictionary. This is not a trusted compiler axiom. The `Eq` dictionary must therefore provide well-typed `eqSound` and
   `eqComplete` members. No additional restriction on hand-written `Eq` instances is required. The compiler never
   synthesizes `x = y` from `(x ~= y) = True`. Only `Eq` supports equality reflection; `Equiv` does not.

If all steps fail, compilation fails with an error indicating the unsolved implicit goal `G`.

<!-- expressions.implicit_parameters.implicit_resolution_instance_proof_search.witness_summon_library_helpers -->
##### 7.3.3.1 `witness` and `summon` as library helpers (not syntax)

Implicit resolution can be invoked using ordinary functions with implicit parameters. Conventional helpers are:

```kappa
witness : (t : Type) -> (@v : t) -> t
let witness t @v = v

summon : (c : Constraint) -> (@v : c) -> Dict c
let summon c @v = v
```

Examples (using §5.6 coercion):

```kappa
-- inside a context where (x > 0) = True is available implicitly:
let p : (x > 0) = witness (x > 0)

-- retrieving an instance dictionary:
let eqInt : Dict (Eq Int) = summon (Eq Int)
```

<!-- expressions.implicit_parameters.constructor_tag_test_expression_is -->
#### 7.3.4 Constructor-tag test expression (`is`)

Kappa provides a general constructor-tag test expression:

```kappa
e is C
e is Type.C
```

Grammar:

```text
tagTestExpr ::= expr 'is' ctor
ctor        ::= ctorName | typeName '.' ctorName
```

Rules:

* `is` is a reserved, non-associative infix form.
* Its fixed precedence is the same as the comparison operators of `std.prelude` (the same precedence as `(==)` and
  friends).
* Chaining without parentheses is a parse error. Thus `a is C is D` is ill-formed.
* The right-hand side of `is` is a single constructor name only. No subpatterns, arguments, binders, or or-alternatives
  are permitted.
* `e is C` has type `Bool`.
* `e` must have a type whose head is a `data` type or another constructor-based built-in type such as `Bool`.
* `C` must be a constructor of the scrutinee type. If it is not, the program is ill-formed with the same kind of error
  as an equivalent `match`.

Dynamic semantics:

* Evaluate `e` exactly once to a value `v`.
* The result is `True` iff the top-level constructor of `v` is `C`; otherwise the result is `False`.

<!-- expressions.conditionals -->
### 7.4 Conditionals

`if` is an expression.

```text
ifExpr     ::= 'if' condList 'then' expr ('elif' condList 'then' expr)* 'else' expr
condList   ::= condClause (',' condClause)*
condClause ::= expr
             | 'let' pattern '=' expr
```

Clause forms:

* A plain expression clause `e` is a boolean condition. It must have type `Bool`.
  This includes constructor-tag test expressions `e is C` from §7.3.4.
* A pattern clause `let pat = e` is a refutable pattern condition. It succeeds iff matching `e` against `pat` succeeds.

Scope and evaluation:

* Outside `do`, `if` must have a final `else`.
* `elif` is sugar for `else if`.
* All branches must have the same type.
* Clauses are elaborated and evaluated in source order.
* Later clauses are checked in the environment produced by earlier successful clauses.
* The `then` branch is checked in the success environment of the full condition list.
* The `else` branch is checked in the failure environment of the first failing clause.

Inside a `do` block, an `if` without `else` is allowed as sugar (see §8).

<!-- expressions.conditionals.flow_sensitive_branch_evidence_atomic_conditions -->
#### 7.4.1 Flow-sensitive branch evidence for atomic conditions

Boolean clauses introduce boolean assumptions into the implicit context.

```kappa
if cond then e1 else e2
```

For a plain boolean clause, the success continuation is checked under additional erased implicit evidence:

```kappa
@p : cond = True
```

and the failure continuation is checked under additional erased implicit evidence:

```kappa
@p : cond = False
```

When such a plain boolean clause is syntactically a constructor-tag test `e is C`, the same branch additionally carries
constructor-refinement evidence:

* success continuation:

  ```kappa
  @p : HasCtor e ⟨C⟩
  ```

* failure continuation:

  ```kappa
  @p : LacksCtor e ⟨C⟩
  ```

where `⟨C⟩ : CtorTag` is the internal constructor-tag constant of §17.3.1.8.

The success-side constructor evidence MUST be strong enough that:

* a subsequent `match e` may treat non-`C` cases as unreachable; and
* if `C` declares named explicit parameters, dotted projection `e.field` may refer to those named constructor parameters
  via constructor-field projection (§2.8.3).

The success evidence additionally includes any index equalities forced by constructor `C`, exactly as in the
corresponding constructor branch of `match` (§7.5.1A).

Pattern clauses introduce bindings but no negative residual fact. The success continuation is checked under the
bindings and refinements introduced by matching `pat` against `e`; the failure continuation receives no additional
negative evidence beyond the ordinary control-flow fact that the pattern clause failed.

These erased assumptions participate in implicit resolution (§7.3.3).

`elif` introduces no separate rule beyond its desugaring to nested `else if`.

<!-- expressions.conditionals.flow_typing_through_short_circuit_short_circuit_not -->
#### 7.4.2 Flow typing through `&&`, `||`, and `not`

Flow typing for plain boolean clauses is defined over ordinary boolean syntax.

A flow-sensitive condition position is:

* a plain boolean clause in an `if` condition list;
* the guard of `case pat if guard`;
* the condition of `while` when that condition is a pure `Bool` expression; and
* any later construct explicitly defined in terms of the success or failure environment of a boolean condition.

In such a position, when the boolean expression contains `&&`, `||`, or `not`, the implementation MUST behave as if the
following equations were applied recursively before the branch-evidence rules of §7.4.1:

```kappa
if a && b then t else f
≡ if a then
      if b then t else f
  else
      f

if a || b then t else f
≡ if a then
      t
  else
      if b then t else f

if not a then t else f
≡ if a then f else t
```

Rules:

* The right operand of `&&` is checked only in the success environment of the left operand.
* The right operand of `||` is checked only in the failure environment of the left operand.
* The `not` equation applies only when the outermost condition form is an application of the term `not` to one argument
  and that application has type `Bool`.
* These equations apply recursively. In particular, `a` and `b` may themselves contain `&&`, `||`, `not`, constructor
  tests, stable aliases, or any other boolean-valued expressions.
* Branch bodies may therefore be typechecked multiple times under different branch-local facts when a condition contains
  `||` or nested combinations.
* A conforming implementation MAY share those bodies or lower them differently internally, provided branch-local
  refinement and runtime evaluation count are observationally equivalent to the equations above.

Common success-body typing under `||`:

Because `if a || b then t else f` is checked as `if a then t else if b then t else f`, the branch body `t` is
typechecked separately under each success environment induced by `a` and `b`.

Consequently, a constructor-field projection in `t` is well-formed when each success environment independently proves a
constructor that declares that field, and the resulting type of the projection is definitionally equal in each such
environment.

Example:

```kappa
data Payload : Type =
    IntBox (value : Int)
    NatBox (value : Int)
    Empty

let readValue (p : Payload) : Int =
    if p is IntBox || p is NatBox then p.value else 0
```

This is accepted because the `then` body is checked once under `HasCtor p IntBox` and once under `HasCtor p NatBox`,
and `p.value` has type `Int` in both checks.

No disjunctive constructor fact is introduced beyond the ordinary branchwise checking.

<!-- expressions.conditionals.stable_aliases_transport_refinement_evidence -->
#### 7.4.3 Stable aliases and transport of refinement evidence

Flow typing is defined over stable scrutinee representatives rather than over raw surface spelling.

Definitions:

* A stable scrutinee is:
  * a stable place under §5.1.7.1,
  * a fresh hidden scrutinee term introduced by elaboration of a control-flow form, or
  * a simple local alias of another stable scrutinee as defined below.

* A simple local alias binding has one of the forms:

  ```kappa
  let x = s
  let x : T = s
  ```

  where:
  * `x` is a simple identifier,
  * `s` is a stable scrutinee,
  * the binding is pure,
  * `x` is not a `var`, and
  * ordinary quantity checking judges that forming the binding does not consume `s`.

A simple local alias binding introduces erased local equality evidence:

```kappa
@alias : x = s
```

and records `x` as a stable alias of the same scrutinee representative for flow-typing purposes.

Consequences:

* Any refinement evidence attached to a stable scrutinee may be transported across in-scope alias equalities introduced
  by this section.
* In particular, boolean branch evidence, `HasCtor`, and `LacksCtor` may be used through any current stable alias of the
  refined scrutinee.
* Conversely, if an alias is the tested expression, the refinement applies to the underlying representative and
  therefore to all current aliases of that representative.

Invalidation:

A stable-alias relation introduced by this section ceases to be available after any of the following:

* rebinding or shadowing of the alias name;
* assignment to a `var` from whose read the alias was derived;
* a consuming use that makes the aliased value or path unavailable; or
* any path-state change that invalidates the original stable place under the ownership rules.

This section affects only flow typing and refinement transport. It does not change the place-identity rule of
§5.1.7.1: an ordinary value binding remains not a place alias for borrowing or `inout`.


<!-- expressions.match -->
### 7.5 `match` expressions

Pattern matching expression:

```kappa
match expr
  case pattern1 if guard1 -> expr1
  case pattern2           -> expr2
...
```

* `match` is an expression.
* Suggested formatting: examples in this specification indent `case` clauses by two spaces relative to the `match`
  head. This is a readability convention only. A conforming parser MUST accept both aligned and indented `case`
  clauses.
* Each ordinary `case` has:

    * a **pattern**,
    * an optional `if guard` (a boolean expression),
    * a result expression after `->`.
* A `match` may additionally end with a final `case impossible` as specified in §7.5.2A.
* All branches must have the same type.

**Exhaustiveness:**

* For **closed** types (e.g. ADTs, `Bool`), `match` must be exhaustive; missing cases are a compile-time error.
* For open/infinite domains (e.g. `Int`), you must include a catch-all (`_`) or similar; otherwise it's an error.

<!-- expressions.match.exhaustiveness_indexed_types_gadts -->
#### 7.5.1 Exhaustiveness with indexed types (GADTs)

For indexed/"GADT-style" types, pattern matching refines the scrutinee's indices and may render some constructor cases
impossible.

Implementations should:

* attempt to use definitional equality / unification of indices to detect unreachable cases;
* after ordinary branch refinement, MAY compute the inhabitance summary of the refined case type under §17.3.6;
* if that summary is `Empty`, treat the case as unreachable and therefore not required for exhaustiveness;
* otherwise (if coverage still cannot be established) require an explicit catch-all (`_`) or an explicit user-written
  case structure that proves impossibility.

As user-facing ways to state and check unreachability, a branch may use `-> impossible` (§7.5.2) and a `match` may end
with `case impossible` (§7.5.2A). Such forms are valid only when the compiler can verify the corresponding remainder is
unreachable.

Optional strengthening rules from §17.3.6 MAY improve diagnostics, but acceptance of a `match` MUST NOT depend on any
rule outside the required structural fragment of that section.

<!-- expressions.match.index_refinement_constructor_knowledge -->
#### 7.5.1A Index refinement from constructor knowledge

Positive constructor knowledge refines not only the outer constructor tag but also any equalities on indices or
parameters forced by that constructor declaration.

Rules:

* In a constructor branch of `match`, the branch-local environment includes all index equalities and parameter
  refinements forced by the matched constructor.
* The same index refinement is introduced by `if e is C then ... else ...`.
* The same index refinement is introduced by any refinement-predicate clause whose branch fact is `e is C`.
* These equalities participate in definitional equality, reachability, and implicit resolution within the refined
  branch.
* Accordingly, constructor tests on indexed families may narrow associated indices whenever the constructor declaration
  forces those indices to specific values or shapes.

<!-- expressions.match.discriminating_erased_scrutinees -->
#### 7.5.1B Discriminating erased scrutinees

A constructor test, constructor-field projection, or pattern match whose discriminating scrutinee is available only at
quantity `0` is permitted only when the scrutinee's constructor shape is already forced by non-erased information.

Rules:

* If the scrutinee is available only at quantity `0`, discrimination is well-formed only when the implementation can
  prove from definitional equality, index unification, or already-available refinement evidence that the tested
  constructor is the only possible constructor.
* Otherwise, matching or testing that scrutinee is a compile-time error.
* This rule applies equally to `if e is C`, `match e`, constructor-field projection made possible by refinement, and any
  future surface form that inspects constructor shape.

<!-- expressions.match.impossible_unreachable_branch_bodies -->
#### 7.5.2 `impossible` (unreachable branch bodies)

`impossible` is a special expression used to mark an unreachable branch.

Form:

```kappa
match e
  case pat -> impossible
```

Typing rule:
* A branch body `impossible` is accepted only if the compiler can prove that the corresponding case is unreachable using
  definitional equality, index unification, and the required structural inhabitance rules of §17.3.6.
* In particular, if the refined case type has inhabitance summary `Empty`, the branch is unreachable.
* If the compiler cannot prove the case unreachable, it is a compile-time error.

Meaning:
* If accepted, `impossible` may be given any result type required by the surrounding match.
* At runtime, an `impossible` branch must never be executed; implementations may compile it to a trap.

<!-- expressions.match.case_impossible_empty_remainder -->
#### 7.5.2A `case impossible` (empty remainder)

A `match` may end with an explicit empty-remainder case:

```kappa
match e
  case impossible
```

or after one or more ordinary cases:

```kappa
match e
  case p1 -> e1
  case p2 if g2 -> e2
  case impossible
```

Grammar addition:

```text
matchCase ::= 'case' pattern ['if' expr] '->' expr
            | 'case' 'impossible'
```

Rules:

* `case impossible` may appear only as the final case of a `match`.
* `case impossible` introduces no binders and has no guard.
* It is accepted only if, after accounting for the preceding cases and guards, the remaining uncovered remainder of the
  scrutinee is unreachable using definitional equality, index unification, and the required structural inhabitance rules
  of §17.3.6.
* In particular, if that uncovered remainder has inhabitance summary `Empty`, the clause is accepted.
* Otherwise it is a compile-time error.

Meaning:

* If accepted, `case impossible` contributes no reachable runtime-success path.
* A `match` ending in `case impossible` is exhaustive exactly when that remaining uncovered remainder is unreachable.
* Implementations may compile an accepted `case impossible` to no code or to a trap.

<!-- expressions.match.boolean_matches_introduce_assumptions -->
#### 7.5.3 Boolean matches introduce assumptions

When the scrutinee has type `Bool` and a case pattern is the boolean literal `True` or `False`, the corresponding branch
is typechecked with an implicit assumption:

```kappa
match b
  case True  -> eT   -- eT checked under  @p : b = True
  case False -> eF   -- eF checked under  @p : b = False
```

This applies equally to `try match` success/error branches when the matched value is a `Bool`.

<!-- expressions.match.constructor_branches_introduce_explicit_refinement_evidence -->
#### 7.5.4 Constructor branches introduce explicit refinement evidence

When a `match` scrutinee `s` is checked against a top-level constructor branch headed by constructor `C`, that branch is
typechecked under additional erased implicit evidence:

```kappa
@p : HasCtor s ⟨C⟩
```

where `⟨C⟩ : CtorTag` is the internal constructor-tag constant of §17.3.1.8.

Residual negative evidence:

* If a branch is a top-level unguarded catch-all branch reached only after one or more preceding top-level unguarded
  constructor branches on the same scrutinee have failed, that branch MAY additionally be checked under the
  corresponding negative evidence:

  ```kappa
  @p1 : LacksCtor s ⟨C1⟩
  ...
  @pn : LacksCtor s ⟨Cn⟩
  ```

  for the excluded constructors `C1 ... Cn`.

* Guarded branches do not contribute negative evidence past the guard boundary, because guard failure does not exclude
  the constructor itself.

This evidence is erased and exists only for typechecking, refinement, reachability, and elaboration. It does not affect
runtime representation.

<!-- expressions.match.narrowing_is_consuming -->
#### 7.5.5 Narrowing is non-consuming

Constructor discrimination is observational only.

Rules:

* Evaluating a scrutinee for `if e is C` or `match e` does not by itself consume the scrutinee, even when that scrutinee
  is available at quantity `1`.
* For typing, constructor discrimination behaves as if elaboration borrowed the scrutinee only for branch selection.
* Each branch is checked independently with the original scrutinee still available at its original quantity, refined by
  that branch's evidence.
* A selected branch may then consume the refined scrutinee or its refined components according to the ordinary quantity
  rules.
* Consumptions in different branches are not accumulated against one another. They are checked branchwise and joined
  only through the ordinary control-flow rules after the branching construct.
* If the discriminated scrutinee is itself a borrowed view or a borrowed alias of a stable place, the refinement applies
  to the same underlying root/path and remains valid only for the same borrow lifetime.
* Constructor-field projections made available by such a refinement are borrowed projections of that same underlying
  source path, not copied values.

<!-- expressions.match.guarded_cases_introduce_guard_success_evidence -->
#### 7.5.6 Guarded cases introduce guard-success evidence

In a guarded case

```kappa
case pat if guard -> body
```

the guard is checked after `pat` has matched and under the bindings and refinement evidence introduced by `pat`.

Rules:

* `guard` must have type `Bool`.

* `body` is checked under:

  * the bindings and refinement evidence introduced by the successful match of `pat`; and
  * additional erased implicit evidence

    ```kappa
    @p : guard = True
    ```

* A guarded case is a flow-sensitive condition position for purposes of §7.4.2. If `guard` contains `&&`, `||`, or
  `not`, the body is checked as if that guard were recursively lowered by §7.4.2 before the guard-success evidence of
  this subsection is assigned.

* Guard failure does not by itself exclude `pat` from subsequent cases. It excludes only the conjunction of `pat` and
  `guard`.

* Therefore guarded branches do not contribute negative constructor evidence past the guard boundary unless some
  separate preceding rule already provides it.

<!-- expressions.patterns -->
### 7.6 Patterns (overview)

Patterns are used in `match`, `try match`, `for` generators, and refutable forms (§10.4.1).

<!-- expressions.patterns.forms -->
#### 7.6.1 Pattern forms

* Wildcard: `_`
* Binder: `x`
* Literal: numeric, string, `True`, `False`, `()`
* As-pattern: `name@pat`
* Constructor patterns:
  * prefix: `Just x`
  * qualified/type-scoped: `Option.Some x`, `Vec.Cons h t`
  * infix constructor patterns are permitted when the constructor is an operator with fixity in scope (e.g. `x :: xs`)
* Tuple patterns:
  ```kappa
  (p1, p2)
  (p1, p2, p3)
  (p,)              -- one-tuple
  ```
* Anonymous record patterns:
  ```kappa
  (x = px, y = py)
  (name = n, ..rest)
  (x = px, y = py, ..)
  (x = px,)         -- one-field record pattern
  (@p = proof, x = px)
  ```
  Grammar:
  ```text
  recordPat      ::= '(' recordPatField (',' recordPatField)* [',' recordPatRest] [','] ')'
                   | '(' recordPatRest [','] ')'
  recordPatField ::= ['@'] ident '=' pattern
  recordPatRest  ::= '..' | '..' ident
  ```
  Omitted implicit fields of the scrutinee record type may be left out; if present, they are unpacked into the local
  implicit context as specified by §5.5.9. Additional rules:
  * At most one record-rest item may appear in a record pattern.
  * If present, the record-rest item must appear last.
  * A bare `..` discards the residual record.
  * If the scrutinee has type `(f1 : T1, ..., fk : Tk, g1 : U1, ..., gm : Um | r)` and the pattern explicitly mentions
    only fields `f1..fk`, then:
    * without a record-rest item, the remaining fields `g1..gm` and the open row `r` are ignored as before;
    * with `..rest`, the binder `rest` has type `(g1 : U1, ..., gm : Um | r)`, normalized by the ordinary record
      canonicalization rules.
  * If the scrutinee record is closed and no fields remain, then the type bound by `..rest` is the zero-field closed
    record, definitionally equal to `Unit`.
  * Record-rest syntax applies only to anonymous record patterns. Tuple patterns do not gain a `..` form from this rule.
* Active pattern applications:
  ```kappa
  RegexMatch emailRegex (user :: domain :: Nil)
  TreeView (NonEmpty left k v right)
  ```
  Here the head must resolve to a definition declared with the `pattern` keyword (§7.7). Any arguments before the final
  parenthesized subpattern are ordinary term arguments; the final parenthesized part is an ordinary Kappa pattern
  matched against the active pattern's result view.
* Constructor patterns with named arguments ("named record patterns"):
  ```kappa
  User { name = n, age = a }
  ```
  (also works for GADT constructors whose signatures contain explicit binders) Field punning is permitted:
  ```kappa
  User { name, age }     -- sugar for: User { name = name, age = age }
  ```
* Typed patterns (type ascription inside patterns):
  ```kappa
  (p : T)
  ```
* Union / variant patterns:
  ```kappa
  (| x : T |)
  (| x |)
  (| _ : T |)
  (| ..rest |)
  ```
  Grammar:
  ```text
  variantPat ::= '(|' ident ':' type '|)'
               | '(|' ident '|)'
               | '(|' '_' ':' type '|)'
               | '(|' '..' ident '|)'
  ```
  The inferred form `(| x |)` is valid only when the scrutinee type is a singleton union or when the member type is
  otherwise unambiguous from context (§5.4.7). It is never the residual-row case; that form is written explicitly as `(|
  ..rest |)`.

* Pattern alternatives (or-patterns):
  ```kappa
  p1 | p2 | p3
  ```
  Meaning: try `p1`; if it fails, try `p2`; etc.

<!-- expressions.patterns.scoping_validity -->
#### 7.6.2 Scoping and validity

* Names bound by a pattern are in scope in:
  * the pattern guard (if present),
  * the branch/body expression,
  * subsequent clauses (for comprehensions).
* Duplicate binders within the same pattern are an error (e.g. `(x, x)`).
* A pattern match proceeds left-to-right; nested patterns are permitted.
* In a prefix pattern application, the head must resolve either to a data constructor or to a function explicitly
  declared with the `pattern` keyword. An ordinary function is not permitted in pattern-head position.

<!-- expressions.patterns.validity -->
#### 7.6.3 Or-pattern validity (`|`)

In `p1 | p2 | ... | pn`:

* Each alternative must bind the **same set of names**.
* Each bound name must have definitionally equal types across alternatives (under the scrutinee type and any index
  refinements), and corresponding binders must carry identical quantities across alternatives.
* If these conditions do not hold, the pattern is ill-formed.

Or-patterns are tried left-to-right at runtime.

<!-- expressions.active_patterns -->
### 7.7 Active patterns (view patterns)

Active patterns allow a module to encapsulate the internal representation of an `opaque data` type while still exposing
an ergonomic pattern-matching interface. An active pattern is a pure function that acts as a custom pattern constructor
during `match`, `try match`, or `let?` destructuring.

<!-- expressions.active_patterns.declaration_grammatical_restrictions -->
#### 7.7.1 Declaration and grammatical restrictions

Active patterns are declared using the `pattern` keyword:

```text
patternDecl ::= [public|private] 'pattern' ident binder* ':' resultType '=' expr
```

where `binder` is an ordinary function binder and the last explicit binder is the scrutinee binder.

Example declarations:

```kappa
public pattern RegexMatch (pattern : Regex) (& input : String) : Option (List String) =
    internalExecute pattern input

public pattern ConsumeToken (expected : Token) (1 buffer : Buffer) : Match ExtractData Buffer =
    ...
```

Rules:

* A `pattern` declaration defines an ordinary pure term-level function.
* A `pattern` declaration must contain at least one explicit binder, since the last explicit binder is the scrutinee.
* The final explicit parameter is the scrutinee. When the pattern is used in a pattern position, the scrutinee being
  matched is passed to this parameter automatically.
* Any preceding explicit parameters are pattern arguments and must be supplied at the usage site.
* The `pattern` keyword alters elaboration only for pattern-head use. In expression position, the declared name behaves
  like an ordinary function.
* A `pattern` declaration is ill-formed if its result type is monadic.
* For pattern-head use in v0.1, the result type must elaborate to one of:
  * `Option T` for some `T`,
  * `Match a r` for some `a` and `r`,
  * a `data` type, or
  * a variant type `(| ... |)`.

This restriction prevents pattern syntax from collapsing into arbitrary function application or equality tests.

Usage form:

```kappa
PatternName arg1 ... argN (subpattern)
```

The arguments `arg1 ... argN` are ordinary term arguments. The final parenthesized `subpattern` is an ordinary Kappa
pattern matched against the view returned by the active pattern.

<!-- expressions.active_patterns.partial_active_patterns_option_t -->
#### 7.7.2 Partial active patterns (`Option T`)

If an active pattern returns `Option T`, it is a partial active pattern. It may succeed with payload `T` or fail and
fall through.

Matching rule:

* To match `P a1 ... an (pat)` against scrutinee `s`, evaluate `P a1 ... an s`.
* If the result is `Option.Some v`, continue by matching `pat` against `v`.
* If the result is `Option.None`, the pattern match fails.
* If `pat` itself fails, the whole active-pattern match fails.

Example:

```kappa
match userInput
  case RegexMatch emailRegex (username :: domain :: Nil) ->
      "Domain is " ++ domain
  case RegexMatch phoneRegex (areaCode :: number :: Nil) ->
      "Valid phone: " ++ areaCode
```

Conceptually, the first case behaves like:

```kappa
match (RegexMatch emailRegex userInput)
  case Option.Some groups ->
      match groups
        case username :: domain :: Nil -> "Domain is " ++ domain
  case Option.None ->
      -- case failure; continue to the next outer case
```

Linearity restriction:

* An `Option`-returning active pattern is intended for borrowed (`&`) or unrestricted (`ω`) scrutinees.
* If such a pattern consumes its scrutinee at quantity `@1`, then failure would consume the resource with no residue.
  Therefore the compiler MUST reject use of an `Option`-returning active pattern in any refutable context where failure
  would require fallthrough.
* In particular, `let? P ... = expr` is ill-formed when `P` consumes the scrutinee linearly.

<!-- expressions.active_patterns.linear_fallthrough_threadable_patterns_match_r -->
#### 7.7.3 Linear fallthrough and threadable patterns (`Match a r`)

To support zero-copy parsing and other state-machine-style matching on owned resources, Kappa uses the prelude type:

```kappa
data Match (a : Type) (r : Type) : Type =
    Hit a
    Miss r
```

If an active pattern consumes a scrutinee at quantity `@1` and may fail, it MUST return `Match a r` in order to permit
fallthrough while preserving ownership.

Declaration example:

```kappa
public pattern ConsumeToken (expected : Token) (1 buffer : Buffer) : Match ExtractData Buffer =
    ...
```

Normative threading rule for `match` / `try match`:

When a `match` block over scrutinee `s` uses one or more `Match`-returning active-pattern cases in order, elaboration
threads the `Miss` residue into the subsequent case.

Surface form:

```kappa
match buffer
  case ConsumeToken Header (data) -> ...
  case ConsumeToken Body (data)   -> ...
  case _ -> ...
```

Conceptual elaboration:

```kappa
match (ConsumeToken Header buffer)
  case Match.Hit data -> ...
  case Match.Miss __buf_1 ->
      match (ConsumeToken Body __buf_1)
        case Match.Hit data -> ...
        case Match.Miss __buf_2 ->
            match __buf_2
              case _ -> ...
```

Rules:

* In a threaded sequence, the residue type `r` of each `Match a r` must be definitionally equal to the scrutinee type
  expected by the next case.
* A non-active-pattern case following a `Match`-returning active pattern matches directly against the threaded residue.
* `Match`-returning active patterns are not permitted in the plain `let? pat = expr` form, because that form provides no
  continuation branch to receive the `Miss` residue.
* Because active patterns are arbitrary functions, the exhaustiveness checker treats `Match`-returning active-pattern
  cases as refutable. A threaded `match` / `try match` sequence using such cases is exhaustive only if the threaded
  chain is terminated by an unconditionally irrefutable final case, such as `_` or another total pattern, that consumes
  the final `Miss` residue.

<!-- expressions.active_patterns.match_finite_local_control_protocol -->
#### 7.7.3A `Match` as a finite local control protocol

`Match a r` is the source-level semantic representation of a two-arm finite local control protocol.

Interpretation:

* `Hit a` is the success arm.
* `Miss r` is the residue / fallthrough arm.

Rules:

* The explicit `Match` data type and the threading rules of §7.7.3 remain the normative source semantics.
* An implementation MAY lower a computation of type `Match a r`, together with its immediately surrounding case
  analysis or threaded active-pattern chain, to an internal two-arm multi-return protocol, join-point graph, or other
  observationally equivalent backend form.
* Such a lowering is permitted only when it preserves:
  * single evaluation of the scrutinee and of any explicit active-pattern arguments;
  * left-to-right case order;
  * exact residue threading on failure; and
  * the ownership behavior of the explicit `Hit` and `Miss` constructors.
* This lowering is internal only. It does not introduce first-class return points, does not change source typing, and is
  not part of Kappa's portable ABI.

<!-- expressions.active_patterns.total_active_patterns_adts_variants -->
#### 7.7.4 Total active patterns (ADTs and variants)

If an active pattern returns a `data` type or a variant type, it is a total active pattern. Its subpattern is matched
against that returned view in the ordinary way.

Example declaration:

```kappa
public data TreeViewResult k v : Type =
    Empty
    NonEmpty (TreeMap k v) k v (TreeMap k v)

public pattern TreeView (t : TreeMap k v) : TreeViewResult k v =
    if internalIsEmpty t then Empty
    else NonEmpty (internalLeft t) (internalKey t) (internalVal t) (internalRight t)
```

Usage:

```kappa
match myTree
  case TreeView (Empty) ->
      "Tree is empty!"
  case TreeView (NonEmpty left k v right) ->
      "Root is " ++ show k
```

Semantics:

* Matching `TreeView (pat)` against scrutinee `t` means evaluating `TreeView t` and then matching `pat` against the
  resulting `TreeViewResult`.
* The subpattern may use any ordinary pattern valid at the result type, including constructor patterns, record patterns,
  variant patterns, and nested active patterns.

Exhaustiveness:

* If all cases for a given scrutinee are headed by the same total active pattern `P` with definitionally equal pattern
  arguments, then exhaustiveness checking is reduced to exhaustiveness of the subpatterns against `P`'s result type.
* Thus the `TreeView (Empty)` / `TreeView (NonEmpty ...)` pair above is exhaustive exactly when `Empty` and `NonEmpty
  ...` exhaust `TreeViewResult`.

<!-- expressions.active_patterns.interaction_qtt_linearity_borrowing -->
#### 7.7.5 Interaction with QTT (linearity and borrowing)

Active patterns are functions, so their scrutinee parameters obey the ordinary quantity rules of §5.1.5.

1. Region-tied borrowed views:
   * If an active pattern takes a borrowed scrutinee `(& t : A)`, then the payload bound by a successful match is tied
     to the same rigid borrow region `rho` described in §5.1.6.
   * Bound values obtained from such a match are treated as borrowed projections of the scrutinee and therefore may not
     escape that borrow region.

2. Total consumption:
   * If a total active pattern consumes its scrutinee at quantity `@1`, then successful matching consumes that resource
     permanently.
   * The payload variables bound in the branch represent the owned components yielded by the view.

3. Zero-cost view elision:
   * If a total active pattern merely re-exposes an opaque representation through a transparent `data` / variant view,
     implementations SHOULD optimize away any intermediate allocation when inlining or specialization makes that
     possible.

---

<!-- effects -->
## 8. Effects, `do` Blocks, and Control Flow

<!-- effects.monadic_core -->
### 8.1 Semantic computation interface

Kappa uses a small monadic semantic interface at the level of source desugaring and core equations. For a monad `m`, we
assume:

```kappa
pure  : forall a. a -> m a
(>>=) : forall a b. m a -> (a -> m b) -> m b
(>>)  : forall a b. m a -> m b -> m b
```

(Names may live in traits; here it's a conceptual interface.)

This subsection is a statement about the abstract interface used by `do`, `try`, `using`, handlers, and related source
semantics. It is not a claim that the full runtime footprint of `IO`, fibers, STM, timers, or handlers is minimal.

<!-- effects.monadic_core.io_runtime_computations -->
#### 8.1.1 `IO` and runtime computations

Kappa provides a standard runtime computation type:

```kappa
IO : Type -> Type -> Type
type UIO (a : Type) = IO Void a
```

`IO e a` classifies a runtime computation that may:

* terminate normally with a value of type `a`;
* fail with an expected typed error of type `e`;
* be interrupted; or
* terminate with an unchecked defect.

Only expected failures are represented by `e`. Interruption and defects are tracked separately and are not part of the
typed error parameter.

For every error type `e : Type`, the type constructor `IO e` participates in the standard prelude interfaces:

```kappa
Functor (IO e)
Applicative (IO e)
Monad (IO e)
MonadError (IO e)
MonadFinally (IO e)
MonadResource (IO e)
MonadRef (IO e)
```

The observable semantics of `IO` are defined by Kappa rather than by any host runtime. A backend MAY realize `IO` using
host threads, virtual threads, tasks, promises, coroutines, event loops, or other mechanisms, but MUST NOT let host
behavior redefine Kappa interruption, finalization, fiber lifetime, or failure classification.

<!-- effects.monadic_core.exits_causes_typed_failure_interruption_defects -->
#### 8.1.2 Exits, causes, typed failure, interruption, and defects

The runtime outcome of an `IO` computation is described by:

```kappa
data Exit (e : Type) (a : Type) : Type =
    Success a
    Failure (Cause e)

data Cause (e : Type) : Type =
    Fail e
    Interrupt InterruptCause
    Defect DefectInfo
    Both (Cause e) (Cause e)
    Then (Cause e) (Cause e)
```

Meaning:

* `Fail e` represents an expected, typed failure.
* `Interrupt ...` represents runtime interruption / cancellation.
* `Defect ...` represents an unchecked runtime failure.
* `Both` represents parallel cause composition.
* `Then` represents sequential cause composition, including unwinding and finalization sequencing.

The portable core standardizes interruption metadata.

`Cause.Interrupt c` carries a structured `InterruptCause` value rather than opaque backend-only interruption state.

For `InterruptCause`:

* `tag` classifies why the interruption occurred.
* `by` records the initiating fiber when one is semantically known.
* `InterruptTag.Custom s` permits library-defined interruption reasons without changing the core meaning of
  interruption.

Built-in runtime operations use the following standard interrupt tags:

* direct `interrupt` / `interruptFork` use `Requested`;
* scope shutdown, including implicit structured child shutdown on scope exit, uses `ScopeShutdown`;
* `timeout` uses `TimedOut`;
* interruption of the losing branch of `race` uses `RaceLost`;
* host- or runtime-originated interruption not attributable to a specific Kappa fiber uses `External`.

The standard runtime observation functions are:

```kappa
sandbox   :
    forall (e : Type) (a : Type).
    IO e a -> IO (Cause e) a

unsandbox :
    forall (e : Type) (a : Type).
    IO (Cause e) a -> IO e a
```

For the standard runtime carrier `IO e`:

* `throwError` and `catchError` operate only on the `Fail e` branch.
* Interruption and defects are not caught by `catchError`.
* `sandbox` exposes the full `Cause e` in the typed error channel.
* `unsandbox` reverses that exposure.
* Sandboxed code MAY inspect `InterruptCause` to discriminate among standard interruption reasons or library-defined
  `Custom` interruption tags.

<!-- effects.monadic_core.portable_defect_classes -->
#### 8.1.2A Portable defect classes

The portable core standardizes a minimum defect vocabulary through `DefectTag` and `DefectInfo`.

Required mappings:

* An uncaught host exception, host panic, or equivalent foreign-runtime failure that crosses into Kappa execution
  without explicit typed-error translation becomes `Defect (DefectInfo HostFailure msg)`.
* A failed foreign boundary contract, failed higher-order wrapper obligation, or callback-surface contract failure
  becomes `Defect (DefectInfo ForeignContractViolation msg)`.
* An attached child fiber that terminates with unacknowledged typed failure or defect and remains unacknowledged when
  its enclosing scope exits becomes `Defect (DefectInfo UnhandledChildFailure msg)`.
* A backend-detected assertion failure or explicit runtime panic in Kappa implementation code becomes
  `Defect (DefectInfo Panic msg)` or `Defect (DefectInfo AssertionFailed msg)`.
* A backend-detected arithmetic trap during Kappa execution SHOULD use `ArithmeticFault`.

Resource exhaustion:

* When the backend can recover control to Kappa after stack exhaustion or heap/resource exhaustion, it SHOULD classify
  those failures respectively as `StackOverflow` or `OutOfMemory`.
* When the backend cannot recover control to Kappa after such exhaustion, abrupt process termination is permitted.
  Portable source code MUST NOT rely on catching or recovering from that case.

`message` is diagnostic text only. Implementations MAY additionally retain stack traces, host cause chains, source
origins, or richer defect payloads outside the portable `DefectInfo` value.

<!-- effects.monadic_core.fibers_structured_concurrency -->
#### 8.1.3 Fibers and structured concurrency

A fiber is a lightweight implementation-managed Kappa thread. Fibers are semantic runtime entities of Kappa. They are
not identified with host threads, host tasks, or host promises.

The standard fiber operations are:

```kappa
fork          :
    forall (e : Type) (a : Type).
    IO e a -> UIO (Fiber e a)

forkDaemon    :
    forall (e : Type) (a : Type).
    IO e a -> UIO (Fiber e a)

await         :
    forall (e : Type) (a : Type).
    Fiber e a -> UIO (Exit e a)

join          :
    forall (e : Type) (a : Type).
    Fiber e a -> IO e a

interrupt     :
    forall (e : Type) (a : Type).
    Fiber e a -> UIO Unit

interruptFork :
    forall (e : Type) (a : Type).
    Fiber e a -> UIO Unit

interruptAs   :
    forall (e : Type) (a : Type).
    InterruptCause -> Fiber e a -> UIO Unit

interruptForkAs :
    forall (e : Type) (a : Type).
    InterruptCause -> Fiber e a -> UIO Unit
```

Semantics:

* `fork` creates a child fiber attached to the current structured runtime scope.
* `forkDaemon` creates a child fiber not attached to the current structured runtime scope.
* `await` waits until the target fiber terminates and returns its terminal `Exit`.
* `join` waits until the target fiber terminates and then:
  * returns the value on `Success a`,
  * fails with `e` on `Failure (Fail e)`,
  * otherwise terminates the caller with the same non-typed runtime cause.
* `interrupt` requests interruption of the target fiber and completes only after the target fiber has terminated and all
  of its finalizers have run.
* `interruptFork` requests interruption of the target fiber but does not wait for termination.
* `interruptAs cause fiber` requests interruption of the target fiber with explicit structured interrupt metadata and
  completes only after the target fiber has terminated and all of its finalizers have run.
* `interruptForkAs cause fiber` requests interruption of the target fiber with explicit structured interrupt metadata
  but does not wait for termination.
* `interrupt fiber` behaves as `interruptAs (InterruptCause Requested (Some id)) fiber`, where `id` is the current
  caller fiber id.
* `interruptFork fiber` behaves as `interruptForkAs (InterruptCause Requested (Some id)) fiber`, where `id` is the
  current caller fiber id.

`fork` is structured by default. Portable source semantics do not provide arbitrary cross-fiber asynchronous exception
injection. Portable cross-fiber asynchronous failure is limited to interruption.

<!-- effects.monadic_core.scheduler_fairness_yielding_preemption -->
#### 8.1.3A Scheduler fairness, yielding, and preemption

The standard scheduler-yield operation is:

```kappa
cede :
    UIO Unit
```

Semantics:

* `cede` yields the current fiber to the scheduler.
* A conforming implementation MUST provide weak fairness among runnable fibers within the same runtime agent.
* In particular, a fiber that remains continuously runnable across infinitely many scheduler decisions MUST NOT be
  postponed forever.
* A conforming implementation MUST therefore behave as if long-running pure execution contains implementation-defined
  safe points at which:
  * pending interruption may become observable, and
  * scheduler fairness may be restored.
* Such safe points MAY be realized by asynchronous preemption, loop-backedge checks, reduction budgets, signal checks,
  or another observationally equivalent mechanism.
* Outside masked regions, an implementation-defined safe point MAY also serve as an interruption point.
* `cede` itself is always both a scheduler-yield point and an interruption point.

Blocking foreign work:

* A backend MUST ensure that blocking foreign work does not permanently monopolize the scheduler resources required by
  unrelated runnable fibers.
* A backend MAY realize this by offloading blocking work, compensating worker creation, host-runtime support, or
  another observationally equivalent mechanism.

<!-- effects.monadic_core.time_timers_deadlines_racing -->
#### 8.1.3B Time, timers, deadlines, and racing

Kappa provides monotonic time and timer primitives:

```kappa
nowMonotonic :
    UIO Instant

sleepFor :
    Duration -> UIO Unit

sleepUntil :
    Instant -> UIO Unit

timeout :
    forall (e : Type) (a : Type).
    Duration -> IO e a -> IO (| TimeoutError | e |) a

race :
    forall (e : Type) (a : Type) (f : Type) (b : Type).
    IO e a -> IO f b -> IO (| e | f |) (RaceResult a b)
```

Semantics:

* `Instant` and `Duration` are monotonic runtime time values. They are not wall-clock calendar values.
* `nowMonotonic` returns the current monotonic instant of the runtime agent.
* `sleepFor d` suspends the current fiber for duration `d`, unless interrupted first.
* `sleepUntil t` suspends the current fiber until instant `t`, unless interrupted first.
* `sleepFor` and `sleepUntil` are interruptible and do not block an entire runtime agent merely by parking one fiber.
* `sleepFor d` with a nonpositive duration returns immediately.
* `sleepUntil t` returns immediately when `t` is not later than the current monotonic instant.

`timeout`:

* `timeout d io` runs `io` and a monotonic timer of duration `d`.
* If `d` is nonpositive, `timeout d io` behaves as if the timer fires before the first ordinary step of `io`.
  An implementation MAY therefore avoid starting `io` at all.
* If `io` completes first:
  * success yields success,
  * typed failure yields typed failure,
  * interruption or defect propagates as the corresponding non-typed runtime cause.
* If the timer fires first:
  * the running computation is interrupted using interrupt tag `TimedOut`,
  * the combinator waits until that interrupted computation has terminated and all of its finalizers have run,
  * and the result is the typed failure `Fail Timeout`.
* If completion of `io` and timer expiry become simultaneously observable with no source-level prior winner,
  completion of `io` wins.

`race`:

* `race left right` runs both computations concurrently.
* If `left` completes first with value `a`, the result is `Success (LeftWins a)`.
* If `right` completes first with value `b`, the result is `Success (RightWins b)`.
* If one side completes first with typed failure `e`, the result is `Failure (Fail e)` in the combined error type.
* If one side completes first with interruption or defect, that non-typed runtime cause wins.
* When either side wins, the losing side is interrupted using interrupt tag `RaceLost`.
* `race` completes only after the losing side has terminated and all of its finalizers have run.
* If both sides become simultaneously observable as completed with no source-level prior winner, the left side wins.

<!-- effects.monadic_core.fiber_local_state -->
#### 8.1.3C Fiber-local state

Kappa provides fiber-local dynamically scoped state:

```kappa
newFiberRef :
    forall (a : Type).
    a -> UIO (FiberRef a)

getFiberRef :
    forall (a : Type).
    FiberRef a -> UIO a

setFiberRef :
    forall (a : Type).
    FiberRef a -> a -> UIO Unit

locallyFiberRef :
    forall (e : Type) (a : Type) (b : Type).
    FiberRef a -> a -> IO e b -> IO e b
```

Semantics:

* Each fiber carries a logically private mapping from `FiberRef` cells to current values.
* `newFiberRef init` creates a new fiber-local cell with initial value `init`.
* `getFiberRef ref` reads the current fiber's value for `ref`.
* `setFiberRef ref value` updates only the current fiber's value for `ref`.
* When a child fiber is created by `fork`, `forkDaemon`, or `forkIn`, the child inherits a snapshot copy of the parent
  fiber's currently visible `FiberRef` values.
* After fork, parent and child updates are independent.
* `locallyFiberRef ref value body` installs `value` for the dynamic extent of `body` and restores the previous value
  on:
  * normal completion,
  * typed failure,
  * interruption, and
  * defect.
* The restoration performed by `locallyFiberRef` behaves as a masked finalizer.

<!-- effects.monadic_core.explicit_scopes_monitors -->
#### 8.1.3D Explicit scopes and monitors

In addition to the implicit lexical supervision scopes of `fork`, Kappa provides explicit runtime scopes and monitors:

```kappa
newScope :
    UIO Scope

withScope :
    forall (e : Type) (a : Type).
    (Scope -> IO e a) -> IO e a

forkIn :
    forall (e : Type) (a : Type).
    Scope -> IO e a -> UIO (Fiber e a)

shutdownScope :
    Scope -> UIO Unit

monitor :
    forall (e : Type) (a : Type).
    Fiber e a -> UIO (Monitor e a)

awaitMonitor :
    forall (e : Type) (a : Type).
    Monitor e a -> UIO (Exit e a)

demonitor :
    forall (e : Type) (a : Type).
    Monitor e a -> UIO Unit
```

Semantics:

* `newScope` creates an explicit supervision scope.
* `withScope use` creates a fresh explicit supervision scope, evaluates `use scope`, and on exit shuts down that scope
  as if by `shutdownScope`.
* The shutdown performed by `withScope` behaves as masked finalization attached to the dynamic extent of `use`.
* `withScope` is the preferred structured constructor for explicit scopes.
  `newScope` / `shutdownScope` are the low-level primitives.
* `forkIn scope io` creates a child fiber attached to `scope`.
* `shutdownScope scope`:
  * interrupts every still-live fiber attached to `scope`,
  * waits until each such fiber has terminated and all of its finalizers have run,
  * and is idempotent.
* The interruption sent by `shutdownScope`, and by implicit structured child shutdown on supervision-scope exit, uses
  interrupt tag `ScopeShutdown`.
* `shutdownScope` does not implicitly shut down fibers attached to unrelated scopes.
* A monitor is one-way termination observation.
* `monitor fiber` creates a monitor for `fiber`.
* `awaitMonitor mon` waits until the monitored fiber terminates and returns its terminal `Exit`.
* `demonitor mon` removes the observation handle.
* Monitoring a fiber does not:
  * make the monitored fiber a child of the monitoring fiber,
  * cause termination of the monitored fiber when the monitor is dropped,
  * or cause termination of the monitoring fiber when the monitored fiber fails.
* If `awaitMonitor` is called after the monitored fiber has already terminated, it returns immediately with the
  recorded terminal `Exit`.

Note:

The portable core runtime standardizes lexical supervision, explicit scopes, monitors, promises, timers, interruption,
masking, and `STM`.

Higher-level service supervision policies such as one-for-one, one-for-all, rest-for-one, restart intensity windows,
child-start ordering, and reverse-order shutdown are intentionally specified as standard-library constructions over
those primitives rather than as core source semantics.

<!-- effects.monadic_core.one_shot_promises -->
#### 8.1.3E One-shot promises

Kappa provides one-shot promise cells carrying terminal `Exit` values:

```kappa
newPromise :
    forall (e : Type) (a : Type).
    UIO (Promise e a)

awaitPromiseExit :
    forall (e : Type) (a : Type).
    Promise e a -> UIO (Exit e a)

awaitPromise :
    forall (e : Type) (a : Type).
    Promise e a -> IO e a

completePromise :
    forall (e : Type) (a : Type).
    Promise e a -> Exit e a -> UIO Bool
```

Semantics:

* `newPromise` creates a fresh uncompleted one-shot promise.
* `completePromise promise exit` attempts to complete `promise` with `exit`.
* The first successful completion returns `True`.
* Any later completion attempt returns `False` and leaves the original completion unchanged.
* `awaitPromiseExit promise` waits until `promise` is completed and returns the stored `Exit`.
* `awaitPromise promise` waits until `promise` is completed and then:
  * returns the value on `Success a`,
  * fails with `e` on `Failure (Fail e)`,
  * otherwise terminates the waiter with the same non-typed runtime cause.
* `awaitPromiseExit` and `awaitPromise` are interruptible waits.
* Completing a promise wakes all waiters observing that promise.

<!-- effects.monadic_core.fiber_identity_labels -->
#### 8.1.3F Fiber identity and labels

Kappa provides portable source-visible fiber identity and diagnostic labels:

```kappa
fiberId :
    forall (e : Type) (a : Type).
    Fiber e a -> UIO FiberId

currentFiberId :
    UIO FiberId

getFiberLabel :
    UIO (Option String)

setFiberLabel :
    Option String -> UIO Unit

locallyFiberLabel :
    forall (e : Type) (a : Type).
    Option String -> IO e a -> IO e a
```

Semantics:

* Every created fiber has a unique `FiberId` within one program execution.
* `fiberId fiber` returns that fiber's identity.
* `currentFiberId` returns the identity of the current fiber.
* A fiber label is diagnostic metadata associated with one fiber.
* `getFiberLabel` reads the current fiber's label.
* `setFiberLabel label` replaces the current fiber's label.
  `None` clears the label.
* `locallyFiberLabel label body` installs `label` for the dynamic extent of `body` and restores the previous label on:
  * normal completion,
  * typed failure,
  * interruption, and
  * defect.
* The restoration performed by `locallyFiberLabel` behaves as masked finalization.
* Fiber labels are not inherited implicitly by child fibers.
  Child fibers begin unlabeled unless user code labels them explicitly.
* Fiber labels do not affect typed success, typed failure, interruption, defects, scheduling fairness, or resource
  semantics.
  They exist only for designated observability facilities.

<!-- effects.monadic_core.blocking_regions -->
#### 8.1.3G Blocking regions

Kappa provides a source-level blocking bridge:

```kappa
blocking :
    forall (e : Type) (a : Type).
    IO e a -> IO e a
```

Semantics:

* `blocking body` executes `body` through the backend's blocking-work bridge.
* `blocking` preserves the typed success, typed failure, interruption, and defect behavior of `body`.
* `blocking` exists to prevent blocking work from monopolizing scheduler resources required by unrelated runnable fibers.
* `blocking` does not by itself make an operation cancellable.
  Cancellation of foreign calls inside `blocking body` still obeys the foreign-call classification rules of §17.13.
* Using `blocking` requires backend capability `rt-blocking`.
  A backend that lacks `rt-blocking` MUST reject a program or deployment mode that requires `blocking`.

The recommended portable shape for a blocking foreign call is therefore `blocking rawCall` or `fork (blocking rawCall)`,
not silent scheduler monopolization.

<!-- effects.monadic_core.interruption_masking -->
#### 8.1.4 Interruption and masking

Interruption is modeled as an asynchronous request rather than immediate forced termination.

The standard masking operations are:

```kappa
poll :
    UIO Unit

uninterruptible :
    forall (e : Type) (a : Type).
    IO e a -> IO e a

mask :
    forall (e : Type) (a : Type).
    ((restore : forall (x : Type). IO e x -> IO e x) -> IO e a) -> IO e a

ensuring :
    forall (e : Type) (a : Type).
    IO e a -> IO e Unit -> IO e a

acquireRelease :
    forall (e : Type) (r : Type) (a : Type).
    IO e r ->
    ((1 resource : r) -> IO e Unit) ->
    ((& resource : r) -> IO e a) ->
    IO e a
```

Semantics:

* Each fiber carries at most one pending interruption request for portable source semantics.
* If multiple interruption requests arrive before delivery, the first request determines the portable `InterruptCause`;
  later requests MAY be recorded diagnostically but do not change the delivered portable cause.
* An interruption request becomes observable only at an interruption point or at explicit `poll`.
* Outside masked regions, every runtime suspension point is an interruption point.
* `poll` is an explicit interruption checkpoint. If the current fiber has a pending interruption request, `poll`
  delivers it immediately, including from within `mask` or `uninterruptible`. Otherwise `poll` returns `Unit`.
* `uninterruptible body` suppresses interruption delivery while `body` executes, except that explicit `poll` may still
  deliver an already pending interruption. A pending interruption request is otherwise re-checked when `body` leaves the
  uninterruptible region.
* `mask f` suppresses interruption while `f` executes. The `restore` function re-enables interruption for a selected
  subcomputation.
* Once a pending interruption has been delivered to the current fiber, that pending request is consumed.
  Code that recovers from `Cause.Interrupt` through `sandbox` or another explicit `Exit`-level observation continues
  with no pending interruption unless a later request arrives.
* Finalizers installed by `ensuring`, `acquireRelease`, `defer`, `using`, scope unwinding, and runtime cleanup run in
  masked state.
* `acquireRelease acquire release use` provides a protected resource scope: the resource remains owned by the scope
  itself, while `use` receives only a borrowed view.

A backend MAY use host async exceptions or equivalent host mechanisms internally to realize interruption, but no such
mechanism is part of portable source semantics.

<!-- effects.monadic_core.stm -->
#### 8.1.5 STM

Kappa provides software transactional memory:

```kappa
STM  : Type -> Type
TVar : Type -> Type
```

with the standard operations:

```kappa
newTVar :
    forall (a : Type).
    a -> STM (TVar a)

readTVar :
    forall (a : Type).
    TVar a -> STM a

writeTVar :
    forall (a : Type).
    TVar a -> a -> STM Unit

retry :
    forall (a : Type).
    STM a

check :
    Bool -> STM Unit

atomically :
    forall (a : Type).
    STM a -> UIO a
```

`STM` participates in the standard prelude interfaces:

```kappa
Functor STM
Applicative STM
Monad STM
Alternative STM
```

For `STM`, the `Alternative` operations denote transactional choice:

* `empty` is `retry`;
* `<|>` / `orElse` runs the right branch only when the left branch retries.

Semantics:

* An `STM` computation is isolated and optimistic.
* `atomically` executes a transaction with serializable semantics relative to other `atomically` executions in the same
  TVar domain.
* `retry` aborts the current transaction attempt and suspends the current fiber until one of the TVars read by that
  attempt is changed.
* `check b` is equivalent to `if b then pure () else retry`.
* Aborted transactions expose no partial writes.
* If interruption is requested before a transaction commits, the current attempt is abandoned and the interruption is
  taken at the next interruption point outside the commit.
* Once transaction commit has begun, commit is uninterruptible.

<!-- effects.monadic_core.eff_effect_rows -->
#### 8.1.6 `Eff` and effect rows

Kappa provides a standard algebraic-effect computation type:

```kappa
Eff : EffRow -> Type -> Type
```

Effect rows describe only handleable algebraic effects. Ordinary implicit values are not effect-row entries.

For every effect row `r : EffRow`, the type constructor `Eff r` participates in the standard prelude interfaces:

```kappa
Functor (Eff r)
Applicative (Eff r)
Monad (Eff r)
```

An `Eff r a` computation may perform only the effects described by `r` and, when it terminates normally, produces a
value of type `a`.

`Eff` is distinct from `IO`.

* `Eff` models handleable algebraic effects only.
* `IO` models runtime execution, typed failure, interruption, defects, fibers, and STM.
* Runtime facilities of `IO` are not themselves tracked by `EffRow` unless user code reifies them as ordinary algebraic
  effects explicitly.

Row weakening:

* If `r1 ⊆ r2`, then `Eff r1 a` is implicitly coercible to `Eff r2 a`.
* This coercion is effect-row weakening. It is zero-cost at runtime and preserves the computation's behavior.

Fully handled computations:

```kappa
runPure : Eff <[ ]> a -> a
```

`runPure` eliminates an `Eff` computation only when its effect row is empty. A computation may be passed to `runPure`
only after all effects have been handled or otherwise eliminated.

In particular, `runPure` may eliminate `Eff <[ ]> (IO e a)` to `IO e a`; no additional eliminator is required for that
case.

Effect rows are orthogonal to the reserved modal/coeffect extension lane of §5.1.5.2. `EffRow` classifies which
effects may occur. A future effect-grade extension, if any, would classify some separate quantitative or policy
property of computations and MUST be layered beside `EffRow` rather than by changing row membership, row equality, or
`SplitEff`.

<!-- effects.monadic_core.effect_declarations -->
#### 8.1.7 `effect` declarations

An `effect` declaration introduces a named effect interface constructor. Operations within the effect may be annotated
with a resumption quantity, which dictates how the corresponding handler continuation may be used.

Example:

```kappa
[public|private] effect State (s : Type) =
    1 get : Unit -> s
    1 put : s -> Unit

[public|private] effect Search =
    ω choice : Unit -> Bool
```

Rules:

* An ordinary `effect` declaration is top-level only.
* The local form `scoped effect` is permitted only inside `block` or `do` scopes under §6.3.1.1.
* `public` and `private` apply only to top-level `effect` declarations.
* `opaque` does not apply to `effect` declarations.
* Every effect declaration introduces an effect-interface constructor of declaration kind `type`.
* Effect labels are separate identifiers of declaration kind `effect-label`.
* Effect-row syntax may introduce effect-label identifiers.
* A binder or package member of compile-time type `EffLabel` introduces an effect-label identifier that is admissible in
  effect-row syntax, in `handle label expr with ...`, and in effect-operation selection `label.op`, subject to the
  ordinary scope rules for that binder.
* A `scoped effect` declaration additionally introduces a canonical self label of the same spelling in declaration kind
  `effect-label` for the same lexical scope.
* A row entry `<[l : E | r]>` associates the effect label `l` with the effect interface `E`.
* Each operation signature in the body is an operation declaration of the effect interface.
* An operation declaration may be prefixed with a quantity `q`. If omitted, the resumption quantity defaults to `1`.
* A resumption quantity MUST be an interval quantity of §5.1.5.
  The borrowed quantity `&` is not permitted on effect operations.
* This quantity governs how many times the handler may use the resumption continuation value supplied to handlers of
  that operation.
* Because a resumption is captured control state rather than a borrowable place, there is no borrowed-resumption mode in
  this specification.
* Operation names declared inside an `effect` declaration do not contribute ordinary unqualified `term` declarations. They are
  selected via `label.op` (§2.8.3), and are additionally available within a handler for that label when handlers are
  specified.
* Operation signatures may be arbitrary dependently typed function types after elaborating any outer `forall`s. This
  includes multiple explicit or implicit parameters, quantity annotations on binders (defaulting to `ω`), and dependent
  result types.
* There is no requirement that an operation have the simple form `A -> B`. The only restrictions are:
  * the signature must elaborate to a Pi-type (function type),
  * an operation may not perform the effect it declares, meaning its declared result type must not itself mention the
    handled effect label.
* The restriction that an operation's declared result type must not mention the handled effect label forbids only direct
  dependence on the same handled label of that operation. It does not forbid result types that mention other effect
  labels, abstract effect-row variables, or explicitly quantified / existentially packaged `EffLabel` and `EffRow`
  witnesses.

<!-- effects.monadic_core.effect_declarations.resumption_quantity_interpretation -->
#### 8.1.7A Interpretation of resumption quantities

The declared resumption quantity `q` of an operation quantifies the handler-bound resumption value `k` later introduced
by §§8.1.9-8.1.10.

Accordingly:

* `q` is the quantity of the binding `k` itself.
* `q` is not the quantity of the resume payload accepted by `k`.
* The resume payload binder remains quantity `1`.
* A multi-shot operation therefore permits multiple uses of the same resumption value `k`, while each individual
  application of `k` still consumes exactly one payload of the operation result type.

There is no surface or KCore form in this specification for a borrowed resumption payload or a borrowed resumption
value.

<!-- effects.monadic_core.effect_labels.identity_and_handler_matching -->
#### 8.1.7B Effect-label identity and handler matching

Effect labels denote handler selectors.

An operation occurrence `label.op args` is associated with the exact effect-label value `label` at that source site.

A handler `handle label expr with ...` or `deep handle label expr with ...` intercepts only operations raised to that
same effect-label value.

Handler matching is by effect-label identity, not by source spelling and not merely by effect-interface name.

Accordingly:

* two distinct effect-label values may carry the same effect interface and still be handled independently;
* the nearest dynamically enclosing matching handler for the exact effect-label value is selected at runtime; and
* passing an `EffLabel` binder or package member to another function explicitly delegates authority to perform
  operations at the handler for that label.

This subsection defines surface semantics only. KCore realization is given by §17.3.1.5.

<!-- effects.monadic_core.effect_application_linear_soundness -->
#### 8.1.8 Effect application and linear soundness

In this section, the declared resumption quantity `q` of an operation always ranges over interval quantities only.

Because Kappa enforces quantitative resource tracking, the capability of an effect handler to duplicate execution via a
multi-shot continuation is restricted by the linear environment at the operation site.

When a computation invokes an effect operation `label.op args`, and the declared resumption quantity of `op` is `q`, the
evaluation context from that operation site to the end of the nearest dynamically enclosing matching handler for that
exact effect-label value is reified by the compiler as a captured resumption value whose permitted uses are constrained
by `q`.

Call-site capture rule:

* The computation from the `op` call site to the end of the handled block MUST be valid when the reified continuation is
  treated as a captured resumption value permitted to be used according to `q`.
* This check uses the ordinary closure-capture and borrow rules of §§5.1.5, 5.1.6, and 7.2.1.
* In particular, if `q` permits more than one use of the continuation, such as `ω` or `>=1`, then the continuation MUST
  NOT capture any live variables with quantity `1` or `&` from the surrounding lexical environment.
* If the compiler detects that an operation with a multi-shot resumption quantity is invoked in a scope where live
  linear or borrowed resources would be captured by the continuation, compilation fails with a quantity error at the
  operation site.

This rule guarantees that multi-shot continuations cannot clone linear resources or extend borrow lifetimes unsoundly.

<!-- effects.monadic_core.effect_application_linear_soundness.repeated_resumption_is_fresh_control_re_entry -->
##### 8.1.8.1 Repeated resumption is fresh control re-entry

When a resumption value `k` is used more than once under a resumption quantity that permits multiple uses, each
application of `k` denotes a fresh dynamic re-entry of the suspended computation suffix captured at the operation site.

Normative consequences:

* the captured continuation includes the control context from the operation site to the corresponding handler boundary,
  including active loop / return targets and the currently active `do`-scope frames in that segment;
* each application of `k` starts from the same captured control point;
* the dynamic `do`-scope frames and pending exit-action stacks in that captured segment are cloned logically per
  application;
* exit actions registered in those frames therefore run independently for each application if that application exits the
  corresponding scope;
* general heap state is not rolled back by continuation reuse. Ordinary unrestricted values and references reachable
  from the captured environment remain shared unless the program explicitly copies them;
* effects, allocations, and `var` declarations that occur after the resumption point are performed anew on each
  application of `k`.

Accordingly, multi-shot resumption duplicates captured control state, not the entire ambient store.

If a pending exit action or captured local binding would carry live quantity-`1` or borrowed state in a way that makes
such duplication unsound, the program is already rejected by the call-site capture rule of §8.1.8.

<!-- effects.monadic_core.effect_application_linear_soundness.control_flow_linearity -->
##### 8.1.8.2 Control-flow linearity of captured resumptions

The call-site capture rule of §8.1.8 is a control-flow linearity obligation.

It is distinct from ordinary binder-quantity satisfaction under §§5.1.5-5.1.7.

Operationally:

* the declared resumption quantity `q` determines how many times the resumption value may be used;
* the compiler must additionally verify that the captured dynamic suffix is admissible for that many uses;
* if `q` permits at most one use, the captured suffix may contain owned or borrowed state subject to the ordinary
  quantity and region rules; and
* if `q` permits more than one use, every runtime-relevant captured value in that suffix must be unrestricted and free
  of borrow-lifetime obligations.

Compile-time-only captured values erased under §14.4 do not by themselves make a multi-shot resumption ill-formed.

A conforming implementation MAY discharge this obligation via closure-capture checking plus region checking, or via an
equivalent dedicated control-flow analysis, provided the accepted programs are the same.

<!-- effects.monadic_core.shallow_handlers -->
#### 8.1.9 Shallow handlers

Kappa provides shallow handlers. A shallow handler intercepts operations for one effect label at a time and eliminates
an `Eff` computation into a single target carrier `m`.

Syntax:

```kappa
handle label expr with
  case return x -> e_ret
  case op1 x1 ... xn k -> e1
  case op2 y1 ... ym k -> e2
...
```

Here `label` is an effect label. The handled computation `expr` may be any expression, including a `do` block written
immediately after the label. Inside the handler clauses, operation names of the handled effect are available
unqualified.

Typing:

Suppose `expr : Eff r_all a` and `SplitEff r_all label E r` is solvable. Equivalently, up to row normalization, `expr`
has type `Eff <[label : E | r]> a`.

Suppose every clause body in the handler is checked in the same target carrier `m : Type -> Type`.

Suppose the effect interface `E` declares an operation `q op` whose signature (after elaborating any outer `forall`s)
is:

```kappa
op : Π (x₁ : A₁) ... (xₙ : Aₙ). B
```

Then the handler is well-typed iff:

* there is exactly one return clause of the form `case return x -> e_ret`,
* there is exactly one operation clause for each operation declared by `E`,
* there are no extra operation clauses,
* in the return clause, `x : a` and `e_ret : m b`,
* in an operation clause `case op x₁ ... xₙ k -> e`, each `xᵢ` is bound at type `Aᵢ` with previous parameters
  substituted into later parameter types and into `B`,
* in that clause, the resumption `k` is itself bound at quantity `q`,
* the resumption `k` has type `(1 _ : B) -> Eff <[label : E | r]> a`, where `B` is instantiated by the clause binders,
* The function shape written above is the ordinary surface shape of `k`.
  Its elaborated type may additionally carry an inferred capture annotation under §5.1.6.1.
  Returning, storing, or otherwise escaping `k` is well-typed only when that capture set is expressible and permitted
  at the escape site.
* and the clause body `e` has type `m b`.

At source level, `k v` remains ordinary application syntax. Its KCore realization is resumption application under
§17.3.1.5 rather than ordinary function application.

The bound `k` is a captured resumption value, not a source-visible return arm or join continuation.
Any join-point or multi-return realization applies only to finite local control surrounding that resumption, as
constrained by §14.8.6A and §17.4.7A.

The whole handler has type:

```kappa
m b
```

All clauses of one handler MUST elaborate to the same target carrier `m`. `m` may be `Eff r`, `IO e`, or any other
well-typed computation carrier.

Semantics:

* On pure completion of `expr`, the `return` clause is executed.
* On an operation at the handled label, the matching operation clause is executed.
* Applying `k` resumes the suspended computation from the operation site in the original unhandled carrier
  `Eff <[label : E | r]> a`.
* `k` may be resumed only in ways permitted by its declared resumption quantity `q`. If the clause returns without
  resuming a non-escaping `k`, the captured segment is abandoned according to §14.8.8A.
* A bound resumption whose captured segment contains pending exit actions must not escape the clause; see §14.8.8A.
* The current handler is not automatically reinstalled around `k`. To continue handling the same label after resumption,
  the handler body must explicitly handle the resumed computation again.
* Operations at labels other than `label` propagate outward unchanged inside the resumed computation.

When `m = Eff r`, this rule coincides with the ordinary algebraic-effect elimination behavior.

<!-- effects.monadic_core.shallow_handlers.kcore_realization_interaction_abrupt_completion -->
##### 8.1.9.1 KCore realization and interaction with abrupt completion

The control model of shallow handlers is realized in KCore by the effect-operation and handler kernel of §17.3.1.5.

In an ordinary effect context, a shallow handler consumes a computation of type:

```text
Eff <[label : E | r]> a
```

and produces a computation in a single target carrier:

```text
m b
```

When a shallow handler appears inside the completion-carrying elaboration of §8.7, the handled computation is instead:

```text
Eff <[label : E | r]> (Completion(RetCtx, a))
```

and the handler result is:

```text
m (Completion(RetCtx, b))
```

In this completion-carrying case:

* the surface clause `case return x -> e_ret` handles only the `Normal x` case;
* `Break`, `Continue`, and `Return[...]` completions propagate outward unchanged unless user-written code explicitly
  matches on them after elaboration; and
* a captured shallow resumption has type:

  ```text
  (1 _ : B) -> Eff <[label : E | r]> (Completion(RetCtx, a))
  ```

This does not introduce a second control system. It is ordinary composition of the handler kernel with the
completion-and-scope kernel.

<!-- effects.monadic_core.deep_handlers_deep_handle -->
#### 8.1.10 Deep handlers (`deep handle`)

Deep handlers automatically reinstall themselves around their resumptions. They eliminate an `Eff` computation into a
single target carrier `m`.

Syntax:

```kappa
deep handle label expr with
  case return x -> e_ret
  case op1 x1 ... xn k -> e1
  case op2 y1 ... ym k -> e2
...
```

The handled computation `expr` may be any expression, including a `do` block written immediately after the label.

Typing:

Suppose `expr : Eff r_all a` and `SplitEff r_all label E r` is solvable.

Suppose every clause body in the handler is checked in the same target carrier `m : Type -> Type`.

Suppose the effect interface `E` declares an operation `q op` whose signature (after elaborating any outer `forall`s)
is:

```kappa
op : Π (x₁ : A₁) ... (xₙ : Aₙ). B
```

Then the corresponding clause

```kappa
case op x₁ ... xₙ k -> e
```

is typed as follows:

* each `xᵢ` is bound at type `Aᵢ` with the usual substitution of earlier parameters into later ones and into `B`,
* the continuation `k` is itself bound at quantity `q`,
* `k` has type `(1 _ : B) -> m b`,
* The function shape written above is the ordinary surface shape of `k`.
  Its elaborated type may additionally carry an inferred capture annotation under §5.1.6.1.
  Returning, storing, or otherwise escaping `k` is well-typed only when that capture set is expressible and permitted
  at the escape site.
* and the clause body `e` has type `m b`.

The return clause has `x : a` and body `e_ret : m b`.

The whole deep handler has type:

```kappa
m b
```

Normative desugaring:

```kappa
let __go __comp =
    handle label __comp with
      case return x -> e_ret
      case op1 x1 ... xn __k_shallow ->
          let q k = \(1 res : B) -> __go (__k_shallow res)
          e1
in
    __go expr
```

where `q` and `B` are the declared resumption quantity and instantiated result type of `op1`, and where `__go` and
`__k_shallow` are fresh identifiers inaccessible to user code.

Semantics:

* On normal completion of a handled branch, the `return` clause `e_ret` is evaluated in the lexical context of the
  handler itself.
* The bound `k` is a captured resumption value, not a source-visible return arm or join continuation.
  Any join-point or multi-return realization applies only to finite local control surrounding that resumption, as
  constrained by §14.8.6A and §17.4.7A.
* Applying `k` resumes the suspended computation from the operation site and immediately reinstalls the same deep
  handler around that resumed computation.
* `k` may be resumed only in ways permitted by its declared quantity `q`. If the clause returns without resuming a
  non-escaping `k`, the captured segment is abandoned according to §14.8.8A.
* A bound resumption whose captured segment contains pending exit actions must not escape the clause; see §14.8.8A.
* Operations at labels other than `label` propagate outward unchanged.
* If the deep resumption `k` is used more than once under its declared resumption quantity, each use behaves as a fresh
  application of the corresponding shallow resumption followed by fresh reinstallation of the same deep handler.

<!-- effects.monadic_core.deep_handlers.kcore_realization_interaction_abrupt_completion -->
##### 8.1.10.1 Deep-handler KCore realization and interaction with abrupt completion

The control model of deep handlers is realized by the shallow-handler kernel of §17.3.1.5 together with the recursive
driver specified in §8.1.10.

In an ordinary effect context, a deep handler consumes a computation of type:

```text
Eff <[label : E | r]> a
```

and produces a computation in a single target carrier:

```text
m b
```

When a deep handler appears inside the completion-carrying elaboration of §8.7, the handled computation is instead:

```text
Eff <[label : E | r]> (Completion(RetCtx, a))
```

and the handler result is:

```text
m (Completion(RetCtx, b))
```

In this completion-carrying case:

* the surface clause `case return x -> e_ret` handles only the `Normal x` case;
* `Break`, `Continue`, and `Return[...]` completions propagate outward unchanged unless user-written code explicitly
  matches on them after elaboration;
* the recursive driver `__go` of §8.1.10 has type:

  ```text
  Eff <[label : E | r]> (Completion(RetCtx, a)) -> m (Completion(RetCtx, b))
  ```

* a captured deep resumption has type:

  ```text
  (1 _ : B) -> m (Completion(RetCtx, b))
  ```

This does not introduce a second control system. It is ordinary composition of the shallow-handler kernel, the deep-
handler recursive driver, and the completion-and-scope kernel.

<!-- effects.monadic_core.surface_control_boundary -->
#### 8.1.10A Surface control boundary

Kappa v1 does not provide raw undelimited or prompt-delimited continuation operators such as `call/cc`, `shift`,
`reset`, `control`, or `prompt`.

The only portable source-level captured-control values are the handler-bound resumptions `k` introduced by
§§8.1.9-8.1.10.

Kappa v1 also does not add a dedicated source-level multi-return-call syntax.
Finite local control at source level is expressed through ordinary branching, `Completion`, `Match`, projections,
and handlers.
Backend-local multi-return / join-point lowering is permitted by §17.4.7A but is not surface syntax.

Implementations MAY realize handlers using such operators internally, but those operators are not part of portable
source syntax, module interfaces, or the portability contract of this specification.

<!-- effects.monadic_core.monad_finally -->
#### 8.1.11 `MonadFinally`

Kappa provides a trait for monads that support reliable finalization:

```kappa
trait MonadFinally (m : Type -> Type) =
    finally : forall a. m a -> m Unit -> m a
```

* `finally ma final` runs `ma`, then always runs `final`, even if `ma` produces a monadic error or abrupt control flow
  propagates via the completion-carrying elaboration of §8.7.

`MonadError m` refines `MonadFinally m`. Any `MonadError` instance MUST also determine a `MonadFinally m` instance for
the same `m`, either explicitly or via a default implementation compatible with `catchError` and the unwinding rules of
§8.7.2.

`MonadResource m` refines `MonadFinally m`.

For `IO e`, finalizers and releases additionally obey the masking and interruption rules of §§8.1.4 and 14.8.

<!-- effects.monadic_core.monad_finally.laws -->
##### 8.1.11.1 Laws

All instances of `MonadFinally` MUST satisfy the following laws, where `≡` denotes observational equivalence in the
monad `m`.

Laws for `finally`:

```kappa
finally : forall a. m a -> m Unit -> m a
```

1. Right unit:

   ```kappa
   finally ma (pure ()) ≡ ma
   ```

2. Associativity of finalizer sequencing:

   ```kappa
   finally (finally ma f1) f2 ≡ finally ma (do
       f1
       f2)
   ```

3. Finalizer always runs:
   * `final` is executed exactly once whenever `ma` completes normally or exits via the completion-carrying
     abrupt-control behavior of §8.7.
   * In particular, this includes monadic errors when `MonadError m` is present, and `return`, `break`, or `continue`
     when `finally` is used through `defer` or `try ... finally`.
   * If `ma` diverges, no guarantee is made that `final` runs.

4. Error behavior when `MonadError m` is present:
   * If `ma = throwError e` and `final` completes normally, then:

     ```kappa
     finally (throwError e) final ≡ do
         final
         throwError e
     ```

   * If `ma` produces error `e1` and `final` produces error `e2`, the observable result is `throwError e1`. An
     implementation MAY attach `e2` to `e1` as suppressed finalizer information for diagnostics or debugging, or MAY
     discard `e2`, but it MUST NOT replace `e1` as the ordinary propagated error seen by portable source programs.

If an instance `MonadResource m` is provided, its `bracket` method MUST also satisfy the following laws:

```kappa
bracket :
    forall a b.
    (1 acquire : m a) ->
    (1 release : (1 r : a) -> m Unit) ->
    (1 use : (& r : a) -> m b) ->
    m b
```

1. Protected-scope law:
   * `bracket acquire release use` is observationally equivalent to a single protected resource scope that:
     * runs `acquire` to obtain an owned resource `r`,
     * exposes only a borrowed view of `r` to `use`, and
     * guarantees that `release r` is attached to scope exit rather than expressed by ordinary closure capture.

2. Resource is always released:
   * `release r` is executed exactly once on every exit path from `bracket`, including normal completion of `use`,
     monadic error from `use` (when `MonadError m` is present), and abrupt control flow propagating through the
     protected scope.

3. No use after release:
   * The borrowed value passed to `use` must not escape the `bracket` scope. This is enforced by the borrow and region
     rules of §5.1.6.

4. Acquire before use:
   * `acquire` completes successfully before `use` is called.

<!-- effects.do_blocks -->
### 8.2 `do` blocks

A `do` block sequences effectful actions. In the general case it elaborates monadically; Appendix G permits
applicative-only elaboration for certain dependency-free blocks:

```kappa
do
    let x <- action1
    action2
    let y <- action3 x
    finalExpr
```

Desugars (schematically, in the general monadic case) to:

```kappa
action1 >>= \x ->
action2 >>
action3 x >>= \y ->
finalExpr
```

Where the block's type is some `m T`.

Valid do-items inside `do`:

* **Bind (`<-`) **:

  ```kappa
  let bindPat <- expr
  ```

  This form is **monadic bind**, introducing new bindings from `bindPat`.

  Typing and well-formedness:
  * `expr` is expected to have type `m A` where `m` is the enclosing `do`-block's monad.
  * The underlying pattern of `bindPat` MUST be irrefutable for type `A` (§6.1.2). If it is refutable, it is a
    compile-time error.
  * Any quantity annotation on `bindPat` applies uniformly to every variable introduced by that pattern (§6.3).

  The alternative form `let (@q x : T) <- expr` binds the simple variable `x` from the monadic result and additionally
  places `x` in the local implicit context from the binding site onward. Its typing is specified in §6.3. Its runtime
  semantics are otherwise the same as an ordinary monadic bind.

  Linear state threading and shadowing:
  * A binding form in a `do` block may introduce a name already in scope; the new binding shadows the old one for all
    subsequent do-items.
  * If the shadowed binding carries a live linear obligation (`@1`), the right-hand side of the shadowing binding must
    consume that obligation exactly once.
  * This rule applies to monadic binds, pure local `let` bindings, and other do-local binding forms that introduce
    names.

  Example:

  ```kappa
  do
      let 1 ch <- connect
      let 1 ch <- send ch 42
      disconnect ch
  ```

* **Assign (`<-`)**:

  ```kappa
  x <- expr
  ```

  * Here, `x` must resolve to a mutable variable introduced by `var`.
  * This form is syntactic sugar for updating the reference with an effectful result.

  Typing and Elaboration:
  * If `x` has type `Ref A` and `expr` has type `m A`, this elaborates to:
    ```kappa
    let __tmp <- expr
    writeRef x __tmp
    ```

* **Pure assignment (`=`)**:

  ```kappa
  x = expr
  ```

  * Here, `x` must resolve to a mutable variable introduced by `var`.
  * This form is syntactic sugar for updating the reference with a pure result.

  Typing and Elaboration:
  * If `x` has type `Ref A` and `expr` has type `A`, this elaborates to:
    ```kappa
    writeRef x expr
    ```

* **Expression item**:

  ```kappa
  expr           -- expr : m a
  ```

  * If `expr` is followed by a later reachable do-item, its value is discarded.
  This requires `Monad m` for the enclosing block at that point. Desugaring sketch: `expr` in such a position behaves
  like `expr >>= \_ -> pure ()`.
  * If normal execution reaches `expr` and no later do-item is reached, its value is the block's result and is not
    discarded.

* **Resource-scoped bind (`using`)**:
    ```kappa
    using pat <- expr
    ```
  
    This form evaluates `expr` to acquire a linear resource, safely binding it for the duration of the `do` block while
    guaranteeing its release on exit. It is a first-class protected-scope construct in the `do` elaboration model: the
    owned resource is attached to scope exit, and only a borrowed view is exposed to user code. It is not a pure alias
    and is not defined by source-level rewriting to `defer`.

    Quantity rule:
    * `using` always binds its pattern at quantity `&`.
    * Explicit quantity markers are not part of `using` syntax.

    It requires an implicit instance:

    ```kappa
    trait Releasable (m : Type -> Type) (a : Type) =
        release : (1 resource : a) -> m Unit
    ```

    Typing and well-formedness:

    * `expr` is expected to have type `m A`, yielding an owned value at quantity `1`.
    * The underlying pattern of `bindPat` is bound in the subsequent do-items at quantity `&`.
    * If that underlying pattern is a destructuring pattern, each name it binds is a borrowed projection of the same
      hidden owned resource.
    * The hidden owned resource is not available as an ordinary term after the `using` item. Its `@1` obligation is
      carried only by the protected-scope exit machinery of §8.7.
    * Because `using` introduces borrowed bindings, user code may inspect them and pass them to other borrowed-accepting
      code, but may not consume them or let them escape their borrow scope.
    * If a later value or closure, including one introduced by an implicit local binder, closes over bindings from
      `pat`, it inherits the same region obligations and may not escape the protected scope except as permitted by
      §5.1.6.
    The prelude minimum requires a trait providing at least this operation; this section uses the name `Releasable`.

    Normative elaboration is given in §8.7.4.

* **Local definition**:

  ```kappa
  let bindPat = expr   -- pure local binding inside the `do` body
  ```

  Rules:
    * The underlying pattern of `bindPat` must be irrefutable (§6.1.2). Refutable patterns are not permitted in do-local
      bindings.
    * Any quantity annotation on `bindPat` applies uniformly to every variable introduced by that pattern (§6.3).
    * If `bindPat` carries quantity `&` and `expr` is not already a borrowable place expression (§5.1.7.2), elaboration
      introduces a fresh hidden temporary root scoped to the remaining do-items, exactly as specified in §6.3 and
      §5.1.6.
    * Names bound by `bindPat` are in scope in subsequent do-items in the `do` block.

  The alternative form `let (@q x : T) = expr` is permitted in a `do` block with the same meaning and typing as in §6.3.

* **Refutable local binding (`let?`)**:

  ```kappa
  let? pat = expr
  let? pat = expr else residuePat -> failExpr
  ```

  This form matches the pure expression `expr` against `pat`. If the match succeeds, pattern variables are bound in
  subsequent do-items. In the plain form, if the match fails, the current `do`-block path short-circuits via `empty`.
  In the `else` form, failure instead binds the unmatched cases to `residuePat` and evaluates `failExpr`.

  Typing and well-formedness:
    * `expr` is checked as a pure expression.
    * `pat` may be refutable.
    * The plain `let? pat = expr` form requires `Alternative m` (or equivalent structure providing `empty`).
    * The plain form is ill-formed unless the compiler can prove that every value discarded on match failure is
      droppable. For this rule, quantities `0`, `&`, `<=1`, and `ω` are droppable, while quantities with a positive
      owned lower bound, such as `1` and `>=1`, are not.
    * The `else` form checks the pair of cases `pat` and `residuePat` as a disjoint, jointly exhaustive split of the
      scrutinee type, using the same overlap and coverage machinery as `match`.
    * In the `else` form, variables bound by `residuePat` are in scope only in `failExpr`, and subsequent do-items are
      not executed on that failure branch.

* **Control-flow sugar** (loops, `if`, `return`, `defer`, etc.) described below.

* **Shared block-scope declarations**: A `do` block is also a block scope in the sense of §6.3.1. From declaration
  onward, it may contain the same local declaration items as `block`: local signatures, named `let` definitions, local
  `data` / `type` / `trait` / `scoped effect` / `instance` / `derive` declarations, and local `import` / fixity
  declarations. Their sequential scoping and elaboration rules are specified in §§6.3.1, 6.3.1.1, and 14.1.1.

Control-flow statements:
  * `break` and `continue` are statements, not expressions. They are valid only inside loop bodies (§8.5).
  * `return e` and `return@L e` are statements, not expressions. They may appear only as do-items inside `do` blocks,
    and only when those `do` blocks lie within the body of a named function, method, or lambda (§8.4).
  * `break` and `continue` may not cross user-written lambda or local-function boundaries. They target only loops within
    the same user-written function or lambda body in which they syntactically occur. Compiler-generated closures
    introduced solely by desugaring of loops, comprehensions, or protected-resource elaboration of `using` are
    transparent to this restriction.
  * `return` target resolution is defined by §8.4. Bare `return` targets the nearest enclosing named function or method;
    `return@L` may target that named function or method by name, or a lambda labeled `L`.

<!-- effects.do_blocks.refutable_local_binding_let -->
#### 8.2.1 Refutable local binding (`let?`)

Within a `do` block, Kappa provides two refutable local binding forms:

```kappa
let? pat = expr
let? pat = expr else residuePat -> failExpr
```

Semantics:

* `expr` is evaluated as a pure expression and matched against `pat`.
* On success, the names bound by `pat` are brought into scope for the subsequent do-items of the `do` block.
* In the plain form, failure short-circuits the current `do`-block path via `empty`.
* In the `else` form, failure instead matches the refutation residue against `residuePat` and evaluates `failExpr`.
  Names bound by `residuePat` are in scope only in `failExpr`, and later do-items are not executed on that branch.

Typing and well-formedness:

* The plain form requires `Alternative m` (or equivalent short-circuiting structure) for the enclosing monad.
* The plain form is permitted only when every value discarded by refutation is droppable. The compiler MUST reject the
  binding if it cannot prove this.
* For this rule, values are droppable exactly when all discarded components carry only droppable quantities:
  `0`, `&`, `<=1`, or `ω`. A discarded component with a positive owned lower-bound obligation, such as quantity `1` or
  `>=1`, makes the plain form ill-formed.
* For an open variant scrutinee of type `(| ... | r |)`, the plain form is permitted only when available row
  constraints prove that every residual-row case in `r` is droppable. Because an unconstrained residual row may hide a
  non-droppable owned case, the compiler MUST conservatively reject the plain form when such proof is unavailable.
* This check is performed on the unmatched residue of `pat`, not merely on whether the matched branch contains linear
  data. For example, matching `Option.Some` against `Option (1 File)` is permitted because the failure case `None`
  carries no owned linear obligation, while matching `Ok` against `Result (1 File) (1 ErrorToken)` is rejected because
  the failure case would discard `Err` carrying `ErrorToken`.
* In the `else` form, the pair of patterns `pat` and `residuePat` is checked as a disjoint, jointly exhaustive split of
  the scrutinee type, using the ordinary overlap and coverage rules of `match`.
* The `else` form does not require `Alternative m` merely for refutation, because failure does not go through `empty`.
  Instead, the failure arm is terminal and is accepted only in one of the following two forms:
  * `failExpr` is an explicit abrupt-control do-item (`return`, `break`, or `continue`), which is elaborated by the
    ordinary abrupt-control rules of §8.4 and §8.5.
  * `failExpr` elaborates as an expression branch of type `m Void`. This includes terms such as `empty`, `throwError
    err`, or an explicit nested `do` block whose normal result type is `Void`.
  In the `m Void` case, elaboration converts the impossible normal result to the surrounding `do` result type by
  ex-falso using `absurd`.
* Because `residuePat` explicitly binds the failure cases, those cases may carry linear owned obligations; ordinary
  linearity rules then require the variables bound by `residuePat` to be consumed appropriately within `failExpr`.

Schematic elaboration:

* Plain `let? pat = expr; rest` behaves like:

  ```kappa
  match expr
  case pat -> do
      rest
  case _   -> empty
  ```

* `let? pat = expr else residuePat -> failExpr; rest` behaves like:

  ```kappa
  match expr
  case pat        -> do
      rest
  case residuePat -> failExpr
  ```

  where the failure arm is elaborated terminally:

  * if `failExpr` is an explicit abrupt-control do-item, it uses the ordinary abrupt-control elaboration;
  * otherwise `failExpr` must have type `m Void`, and its branch is elaborated as:

    ```kappa
    bindC (lift failExpr) (\v -> pure (Normal (absurd v)))
    ```

<!-- effects.do_blocks.do_item_sequences_block_result -->
#### 8.2.2 Do-item sequences and block result

A `do` block is a non-empty sequence of **do-items**.

A do-item may be:

* monadic bind (`let bindPat <- expr`)
* mutable assignment (`x <- expr` or `x = expr`)
* pure local binding (`let bindPat = expr`)
* refutable local binding (`let? pat = expr` or `let? pat = expr else residuePat -> failExpr`)
* monadic expression item (`expr`)
* resource-scoped bind (`using pat <- expr`)
* `defer`
* `return`
* `break`
* `continue`
* loop sugar
* conditional / other control-flow block sugar
* shared block-scope declarations / definitions (local signatures, named `let` definitions, `data`, `type`, `trait`,
  `scoped effect`, `instance`, `derive`, and scoped fixity/import items)

Rules:

* Abrupt-control items such as `return`, `break`, and `continue` may appear anywhere in the sequence.
* If execution reaches the end of the block normally, the last reached do-item must provide the block result.
* A do-item that is reached only on unreachable paths need not provide a normal result.
* Unreachable trailing do-items are permitted, though implementations may warn.

Typing:

* A do-item that is followed by a later reachable do-item is sequenced for effects; if it produces a value, that value
  is discarded.
* A do-item that is the last reached item on a normal path determines the block result on that path.
* Abrupt-control items need not produce a normal result on paths where they are taken.

<!-- effects.do_blocks.post_dominating_flow_facts -->
#### 8.2.2A Post-dominating flow facts

In a sequential `do`-item context, refinement facts may survive past a control-flow split when every alternative that
would invalidate those facts is terminal.

Definition:

A branch is terminal with respect to following do-items iff it cannot complete with a `Normal` completion that reaches
those following do-items, according to the completion model of §8.7.

This includes at least:

* `return`,
* `break`,
* `continue`,
* `impossible`, and
* any branch accepted in the `m Void` form used by §8.2.1.

Rules:

* After a do-level conditional

  ```kappa
  if cond then thenExpr else elseExpr
  ```

  followed by later do-items:

* if `cond` contains `&&`, `||`, or `not`, these rules apply after the recursive lowering of §7.4.2;

* if `elseExpr` is terminal and `thenExpr` can continue, the later do-items are checked under the success facts of the
  continuing branch of that lowered conditional;

* if `thenExpr` is terminal and `elseExpr` can continue, the later do-items are checked under the failure facts of the
  continuing branch of that lowered conditional;

* if both branches can continue, this subsection introduces no additional postcondition facts.

* The atomic branch facts introduced by such a lowered conditional are exactly those of §7.4.1 together with the alias
  transport of §7.4.3.

* Rebinding, assignment, consuming use, or path-state invalidation discards any surviving fact whose stable subject is
  no longer available under §7.4.3.

<!-- effects.do_blocks.labeled_do_blocks_labeled_control_flow -->
#### 8.2.3 Labeled `do` blocks and labeled control flow

Labels:
* A label has the form `label@` and may prefix any block-introducing construct (e.g. `do`, `try`, `match`, loops).
* Lambda labels use the same `label@` prefix and are specified in §8.4.1.
* Labels are in scope within the labeled block.

Targets:
* `break@label` may target a labeled loop only (§8.5). Targeting a label that does not name a loop is a compile-time
  error.
* `continue@label` may target a labeled loop only. Targeting a label that does not name a loop is a compile-time error.
* `defer@label e` may target a labeled `do`-scope (§8.6). Targeting a label that does not name a `do`-scope is a
  compile-time error.
* `return@label e` is defined separately in §8.4. It may target only a named function or method named `label`, or a
  lambda carrying label `label`.

Targeting a label that does not name an appropriate construct is a compile-time error.

Label resolution:
* Labels are resolved lexically by searching outward from the use site.
* `break@L` / `continue@L` resolve to the nearest enclosing labeled loop with label `L`. This search is confined to the
  current user-written function or lambda body; it does not cross user-written lambda or local-function boundaries.
  Compiler-generated closures introduced solely by desugaring of loops, comprehensions, or protected-resource
  elaboration of `using` are transparent to this search.
* `defer@L` resolves to the nearest enclosing labeled `do`-scope with label `L`.
  This search is confined to the current user-written function or lambda body; it does not cross user-written lambda or
  local-function boundaries. Compiler-generated closures introduced solely by desugaring of loops, comprehensions, or
  protected-resource elaboration of `using` are transparent to this search.
* If no suitable labeled construct is found, it is a compile-time error.
* If multiple labeled constructs with the same label occur at the same lexical nesting level, it is a compile-time
  error. (Shadowing by nested labels is permitted.)

Semantics follow Kotlin-style labeled control flow:

* `break@label` exits the labeled loop.
* `continue@label` targets the labeled loop.
* `defer@label e` schedules `e` to run when the labeled `do`-scope exits. If several inner `do`-scopes exit first, `e`
  remains pending and runs only when the targeted labeled scope itself is unwound (§8.7.2.1).

<!-- effects.do_blocks.simple_desugaring_applicability -->
#### 8.2.4 Simple desugaring applicability

The schematic monadic desugaring in §8.2 (into `>>=` / `>>` chains) is valid only for `do` blocks that do not use any
of:

* `break` / `continue`,
* `return`,
* `defer` / `using`,
* `while` / `for` (since those introduce additional control structure and nested do-scopes).

For dependency-free `do` blocks satisfying the conditions of Appendix G, implementations MAY instead elaborate
independent groups using only `Applicative` operations such as `liftA2` / `<*>`, provided the result is observationally
equivalent to the monadic desugaring.

When any of the above features are present, the normative semantics and elaboration model is specified in §8.7.
Implementations MAY still generate code equivalent to the §8.2 sketch, but only if it is observationally equivalent to
the §8.7 model.

<!-- effects.do_blocks.do_local_declarations -->
#### 8.2.5 Do-local declarations

A `do` block is a block scope. Local declarations that appear as do-items obey the same sequential scoping rules as pure
block items under §6.3.1.

Rules:

* A do-local declaration comes into scope from its declaration site onward within that `do` block.
* A do-local instance participates in implicit resolution only within its lexical scope (and nested block scopes inside
  it).
* Closure conversion of local declarations, lexical identity of local nominal declarations, admissibility of local
  instances, and escape behavior are specified uniformly by §14.1.1.

Abrupt control flow affects which do-items are executed, but it does not change the lexical scoping rules of
declarations that were already in scope.

<!-- effects.do_if_without_else -->
### 8.3 `if` without `else` in `do`

Inside `do` only, you may write:

```kappa
do
    if cond then
        stmt1
        stmt2
```

and likewise:

```kappa
do
    if expr is C then
        body
```

A branch body written as an indented suite of sequenced do-items elaborates as a nested `do` block. Thus the first
example above is first understood as:

```kappa
do
    if cond then do
        stmt1
        stmt2
```

and then desugars to:

```kappa
do
    if cond then do
        stmt1
        stmt2
    else
        pure ()
```

So `if` remains an expression; the missing `else` is implicitly `pure ()` in the monad. The nested branch `do` block
is a fresh dynamic do-scope, so any `defer`, `using`, or other do-scope obligations introduced in that branch unwind
on branch exit and do not outlive the branch's lexical accessibility. If a value or resource must remain accessible
after the `if`, the user must make that explicit in the branch result (for example with `Option`). For the tag-test
form, no bindings escape the test.

<!-- effects.return -->
### 8.4 `return`

`return e` is a control-flow statement that exits a target function, method, or labeled lambda, producing `e` as its
result.

Validity:
* `return` is a statement, not an expression.
* `return` may appear only as a do-item inside a `do` block.
* `return` is valid only when that `do` block lies inside the body of a named function, method, or lambda.

For purposes of bare `return`, a direct lambda definition of the form `let name = \binders -> body` or `let name : T =
\binders -> body` counts as a named function named `name` (§6.1).

Target resolution:

* **Bare `return e`** targets the nearest enclosing **named function or method body**, searching outward from the use
  site. This includes a direct lambda definition that inherits its binder name from `let name = ...` per §6.1. Bare
  `return` does NOT target other anonymous lambdas.
  * If one or more user-written lambda boundaries lie between the `return` site and the nearest enclosing named
    function, inherited-name lambda, or method, bare `return` is a compile-time error. The diagnostic MUST identify the
    intervening user-written lambda boundary and suggest either labeling the lambda (§8.4.1) or refactoring to avoid the
    lambda.
  * If no named function, inherited-name lambda, or method encloses the `return` site, it is a compile-time error.
  * Compiler-generated closures introduced solely by desugaring of loops, comprehensions, or protected-resource
    elaboration of `using` are transparent to this resolution rule.

* **Labeled `return@L e`** targets the nearest enclosing construct named or labeled `L` within the current
  user-written function or lambda body:
  * a named function or method whose name is `L`, or
  * a direct lambda definition whose inherited name is `L`, or
  * a lambda carrying the label `L` (§8.4.1).
  * Resolution searches outward lexically from the use site, but is confined to the current user-written function or
    lambda body; it does not cross user-written lambda or local-function boundaries.
  * Compiler-generated closures introduced solely by desugaring of loops, comprehensions, or protected-resource
    elaboration of `using` are transparent to this rule.
  * If no construct with label `L` is found, it is a compile-time error.

* `return` does not target `do` blocks, `if` / `match` branches, loops, or `try` expressions. Those constructs are
  transparent to `return` and propagate the `Return` completion outward (§8.7.4).

Typing:
* A `return` statement may appear only as a do-item inside a `do` block.
* Let the target have result type `m R`, where `m` is the monad of the enclosing `do`-scope containing the `return`.
  Then `e` must have type `R`. `return e` exits that target with result `pure e`.
* A labeled `return@L e` is typed against the type of the construct labeled `L`, not of any surrounding construct.

Restrictions (carried over from prior §8.4):
* `return` MUST NOT appear within a `defer` action (§8.6) or `finally` block (§9.2, §9.3) when it would target the
  enclosing `do`-scope, `try`, or surrounding control-flow context. Returns inside nested lambdas or local functions are
  permitted when they target those nested constructs. Any disallowed case is a compile-time error.

Interaction with `defer` / `finally`:
* Executing `return e` exits the target. All enclosing dynamic `do`-scopes within the target are exited first, and their
  deferred actions run per §8.6 and §8.7. Scopes outside the target are not unwound.

<!-- effects.return.lambda_labels -->
#### 8.4.1 Lambda labels

A lambda may carry an explicit label written before the backslash:

```kappa
L@\ x y -> body
```

Rules:
* `L` is an identifier acting as a label on the lambda's body.
* `return@L e` inside `body` exits the labeled lambda, producing `e`.
* `break@L` and `continue@L` do not apply to lambda labels; those target loops only (§8.5).
* A lambda label does not affect resolution of bare `return`, which continues to target the nearest enclosing named
  function or method (§8.4).
* Labels follow normal lexical scoping and may be shadowed by inner labels with the same name.

<!-- effects.loops -->
### 8.5 Loops and mutable variables (`var`, `while`, `for` in `do`)

These are **syntactic sugar** over monadic operations.

* `while cond do body`:

  ```kappa
  while cond do
      stmts
  ```

  Typing of `cond`:
  * `cond` may have type `Bool` or type `m Bool` where `m` is the enclosing `do`-block's monad.
  * If `cond : Bool`, it is implicitly lifted to `pure cond : m Bool` for the purpose of desugaring.
  * When the condition is a pure `Bool` expression, it is a flow-sensitive condition position for purposes of
    §§7.4.1-7.4.3 and §8.2.2A. When the condition is monadic (`m Bool`), no decomposition of that monadic test is
    performed beyond the ordinary branch facts of the final boolean result.

  Desugaring (schematic): `while cond do body` elaborates to an internal recursive loop in the monad. (The core language
  supports recursion even though surface Kappa has no `let rec`.)

* `for` in `do`:

  ```kappa
  for x in xs do
      stmts
  ```

  Portable semantics use left-to-right iteration over `xs`. For simple loops with no early exit, `return`, or loop
  `else`, an implementation may elaborate through `Foldable`/`Traversable`-style traversal. When early exit, `return`,
  or loop `else` is involved, the portable semantics are given by the standard prelude `Iterator` protocol:

  ```kappa
  trait Iterator (it : Type) =
      Item : Type
      next : (1 this : it) -> Option (item : Item, rest : it)
  ```

  In that case the expression `xs` is the initial iterator state, and each iteration obtains the next element by
  repeated application of `next`.

* `break` and `continue`:
    * `break` and `continue` are valid only within the body of a `while ... do ...` or `for ... do ...` loop. Using them
      outside a loop body is a compile-time error.
    * `break` and `continue` may not cross user-written lambda or local-function boundaries. They target only loops
      within the same user-written function or lambda body in which they syntactically occur. Compiler-generated
      closures introduced solely by loop, comprehension, or protected-resource elaboration of `using` are transparent to
      this restriction.
    * Unlabeled `break` / `continue` target the nearest enclosing loop in that same body.
    * Labeled `break@L` / `continue@L` target the loop labeled `L@` in that same body. If `L` does not name such an
      enclosing loop, it is an error.
    * The precise semantics (including interaction with `defer` and `finally`) are specified in §8.7.

<!-- effects.loops.mutable_variables_var_semantics -->
#### 8.5.1 Mutable variables (`var`) - normative semantics

A `var` declaration inside a `do` block of monad `m` requires an implicit instance:

```kappa
trait MonadRef (m : Type -> Type) =
    Ref : Type -> Type
    newRef   : a -> m (Ref a)
    readRef  : Ref a -> m a
    writeRef : Ref a -> a -> m Unit
```

Elaboration of:

```kappa
var x = e
```

introduces a fresh reference cell:

```kappa
let x <- newRef e
```

**Uniform Reference Semantics:** A `var`-bound identifier `x` uniformly denotes the underlying `Ref` object in all
expression contexts, regardless of lexical depth. There is no implicit auto-dereferencing.

* To read the value, users must explicitly evaluate the read action (e.g., `let val <- readRef x` or using monadic
  splice `!(readRef x)`).
* To write to the value, users may use the explicit `writeRef x val`, or the statement-level assignment sugar (`x = val`
  and `x <- expr`) defined in §8.2.

**Scope and Closure Capture:**
* The name `x` introduced by `var x = e` is lexically scoped to the enclosing `do` block.
* Because `x` strictly denotes the `Ref` itself, capturing `x` in a closure naturally captures the reference. Subsequent
  invocations of the closure observe mutations to that `Ref`, and mutations performed through that `Ref` are visible
  outside it.
* Escaping: It is entirely legal for a closure capturing a `Ref` (or the `Ref` itself) to escape the `do` block,
  provided the region discipline of the enclosing monad allows it (§8.5.3). If the enclosing monad is unrestricted
  (e.g., `IO`), the `Ref` escapes to the heap and behaves as standard shared mutable state.

**Examples:**

```kappa
do
    var x = 0

    let f = \() -> do
        let curr = !(readRef x)
        pure curr
    -- f : Unit -> m Int

    let g = \n -> do
        x = n  -- Valid sugar for: writeRef x n
    -- g : Int -> m Unit

    let pureClosure = \() -> x
    -- ERROR if used where an Int is expected; pureClosure returns a `Ref Int`.
```

A `var`-bound identifier may not appear in type positions or elaboration-time macro positions.

<!-- effects.loops.else -->
#### 8.5.2 Loop `else`

`for` and `while` may optionally include an `else` block:

```kappa
for x in xs do
    body
else do
    onNoBreak
```

Semantics:

* The `else` block runs iff the loop completes normally (i.e. the loop condition becomes false for `while`, or the
  iteration is exhausted for `for`) and no `break` targeting that loop is executed.
* If the loop is exited due to:
    * a `break` targeting that loop, OR
    * any abrupt control that exits the loop without normal completion (e.g. `break@outer` propagating outward, or
      `return`) then the loop's `else` block is skipped.

<!-- effects.loops.scoped_regions_escape_prevention -->
#### 8.5.3 Scoped Regions and Escape Prevention

When mutable state must be guaranteed not to escape a specific scope (e.g. for pure computations that use local mutation
internally), implementations or libraries MUST enforce this via the type system, rather than relying on `var` syntax.

The canonical mechanism is a region-parameterized monad, structurally similar to Haskell's `ST` monad.

Implementations are encouraged to provide such a region-parameterized monad in `std.prelude` or `std.effect` following
the pattern below. The names `ST`, `STRef`, and `runST` are illustrative rather than built-in; an implementation may
choose different names provided it offers the same type-level escape-prevention structure.

```kappa
-- A monad parameterized by a ghost region type `s`
expect data ST (s : Type) (a : Type) : Type

-- The reference type is also tied to the region `s`
expect data STRef (s : Type) (a : Type) : Type

-- ST forms a MonadRef
instance MonadRef (ST s) =
    let Ref = STRef s
    let newRef = ...
```

Escape Prevention via Rank-2 Polymorphism:

To run a scoped computation purely, the runner function requires the computation to be polymorphic over the region `s`.

```kappa
runST : forall a. (forall (s : Type). ST s a) -> a
```

Interaction with `var`:

If a user writes `var x = 1` inside an `ST s` block, `x` is backed by an `STRef s Int`.

If the user attempts to return a closure capturing `x` (e.g. `pure (\() -> readRef x)`), the inferred return type of the
block becomes `ST s (Unit -> ST s Int)`.

Because the return type `a` mentions the region parameter `s`, it violates the `forall (s : Type)` constraint of
`runST`. The compiler rejects this as a standard type-checking error (a rigid-variable / skolem escape error), thereby
proving that no reference, and no closure capturing a reference, can outlive the `runST` block.

Implementation Note on `ST` Allocation:

Because the type system guarantees that no `STRef s` can escape the `runST` block that instantiated the region `s`,
implementations MAY implement the `ST` monad using a bump-pointer arena allocator tied to the `runST` frame. When
`runST` returns, the entire arena is popped off the stack, guaranteeing zero-cost deallocation without garbage
collection.

<!-- effects.defer -->
### 8.6 `defer`

Inside a `do`-scope, `defer e` schedules `e` to run when exiting that `do`-scope (normal completion, `return`,
exception, `break`, or `continue` that exits the scope).

```kappa
do
    let file <- open path "r"
    defer file.close
    let data <- file.read
    ...
```

A `do`-scope is introduced by:

* an explicit `do` block
* the body of `while ... do ...`
* the body of `for ... do ...`
* `else do` blocks attached to loops
* the `then` or `else` branch of an `if` inside `do` when that branch is written as a sequenced do-item suite per
  §8.3; such a branch elaborates as a nested `do` block

`try`/`except`/`finally` clause bodies are expressions. To use do-scope statements (`defer`, `<-`, loops), write an
explicit `do` block.

Typing and linearity:

`defer` requires an implicit `MonadFinally m` instance for the enclosing monad `m`.

The expression `e` is typechecked as a monadic computation in the enclosing monad:

```kappa
m Unit
```

At the level of the scheduled action, `defer e` corresponds to attaching `e` as the finalizer of a trivial computation:

```kappa
finally (pure ()) e
```

Within a `do`-item sequence, the completion-aware elaboration wraps the remainder of the scope with `finally` as
specified in §8.7.4. The strong guarantee that deferred actions run exactly once on every exit path (normal completion,
`return`, `break`, `continue`, or propagated error) is therefore provided by `MonadFinally`, not by a separate
surface-level control operator.

Semantics:

* When control exits a `do`-scope for any reason (normal completion, `return`, a `break`/`continue` that exits the
  scope, or a monadic error that propagates out of the scope), the deferred actions are executed in LIFO order.

* Deferred actions are executed sequentially in the enclosing monad.

* All deferred actions in the scope are attempted, even if some deferred actions propagate monadic errors.

* If one or more deferred actions propagate monadic errors during unwinding, the first error determined by §8.7.2
  becomes the propagated error for the scope. Later deferred-action errors do not replace it, though implementations may
  retain them as suppressed diagnostic information. Any propagated error abandons the original normal / abrupt
  completion record for the scope.

* `defer@label e` schedules `e` in the deferred-action stack of the labeled enclosing `do`-scope. If that labeled scope
  is outer to the current one, `e` remains pending across exits of inner scopes and runs only when the targeted scope
  itself exits (§8.7.2.1).

Restrictions:
* The deferred action `e` MUST NOT contain any abrupt control flow (`return`, `break`, or `continue`) that targets any
  scope outside the deferred action itself. This includes the enclosing `do`-scope and any more distant labeled loop,
  lambda, function, or `do`-scope.
* Occurrences inside a nested lambda or local function are permitted only when they target a construct inside that
  nested body rather than escaping the deferred action.
* Since `finally` is elaborated via `defer` (§9.2), the same restriction applies to `finally` blocks. Attempting to
  jump out of a `defer` or `finally` block is a compile-time error.

Implementation may desugar this to a bracket/finalizer mechanism; spec only fixes the ordering and guarantee of
execution on exit.

<!-- effects.elaboration_model -->
### 8.7 Normative elaboration model for `do` blocks, loops, and abrupt control

This section defines the normative meaning of `do` blocks with loops, `break` / `continue`, `return`, and `defer` /
`using`. Implementations may compile using jumps, CPS, internal exceptions, or other techniques, but MUST be
observationally equivalent to the model below.

<!-- effects.elaboration_model.abrupt_completion_records -->
#### 8.7.1 Abrupt completion records

The control model of this section is realized in KCore by the explicit completion-and-scope kernel of §17.3.1.4.

For exposition, we continue to write the resulting control family as a completion record parameterized by the in-scope
return-target context.

Let:

```
RetCtx = [(L1 : R1), (L2 : R2), ..., (Ln : Rn)]
```

be the finite list of return targets currently in scope, where each `Lk` is a resolved return label and each `Rk` is
that target's result type.

Then:

```
Completion(RetCtx, A) =
  Normal   A
| Break    Label
| Continue Label
| Return[L1] R1
| Return[L2] R2
| ...
| Return[Ln] Rn
```

This `Completion(RetCtx, A)` datatype is a normative KCore-observable control family.
Implementations MAY realize it via exceptions, CPS, explicit stacks, or other equivalent machinery, provided the
resulting behavior is observationally equivalent to the unwinding and target-resolution rules specified in this section.

Where:

* `A` is the "normal" result type of the construct (often `Unit` for statements).
* `RetCtx` is the target-indexed return context for the current elaboration point.
* Each `Return[Lk]` is a distinct compiler-generated control constructor for the return target labeled `Lk`.

This is equivalent to generating one internal control constructor per in-scope return target, rather than globally
parameterizing the whole completion model by a single return payload type.

The `DoScope` and `ScheduleExit` forms of §17.3.1.4 induce a dynamic stack of active `do`-scope frames:

```text
ExitAction(m) =
    Deferred (m Unit)
  | Release[A] ((1 resource : A) -> m Unit) (1 resource : A)

ScopeFrame(m) = { scopeLabel : DoScopeLabel, exitActions : List (ExitAction(m)) }
ScopeStack(m) = [ScopeFrame(m)]     -- innermost frame first
```

Each active `do`-scope contributes one frame. A labeled `do` block contributes a frame whose `scopeLabel` is the
resolved surface label. An unlabeled `do` block contributes a fresh implicit scope label that is not nameable by surface
syntax but still distinguishes that frame for the unwinding model.

<!-- effects.elaboration_model.completion_finite_local_control_protocol -->
#### 8.7.1A `Completion` as a finite local control protocol

`Completion(RetCtx, A)` is not a surface syntax form.
It is the normative semantic control family for the finite abrupt-control alternatives in scope at the current
elaboration point.

Interpretation:

* `Normal A` is the ordinary fallthrough arm.
* `Break L` and `Continue L` are loop-target arms.
* Each `Return[Lk] Rk` is a distinct return-target arm.

Rules:

* The explicit `Completion` family and the propagation / consumption rules of this section remain the normative source
  semantics.
* An implementation MAY lower a completion-carrying region to an internal multi-return protocol or equivalent join-point
  graph whose arms are in fixed bijection with these constructors.
* Such a lowering is permitted only when it preserves:
  * the target-resolution rules of §§8.4-8.5;
  * the unwinding and exit-action rules of §8.7.2;
  * single evaluation of the completed computation; and
  * the requirement that non-matching `Break`, `Continue`, and `Return[...]` alternatives propagate outward unchanged.
* This lowering is internal only. It does not introduce source-visible return-point syntax, does not make continuations
  first-class, and does not alter the type-theoretic role of `Completion`.

<!-- effects.elaboration_model.do_scope_exit_actions -->
#### 8.7.2 Dynamic `do`-scope exit and exit actions

Each dynamic execution of a `do`-scope contributes one frame to the active `ScopeStack(m)`. That frame contains a LIFO
stack of exit actions. An exit action is either:

* `Deferred d`, where `d : m Unit`, or
* `Release[A] rel resource`, carrying an owned resource together with the release function that must consume it at scope
  exit.

Ordinary `defer` pushes `Deferred d` onto the innermost frame. `defer@L` pushes `Deferred d` onto the nearest enclosing
frame whose `scopeLabel` is `L`. `using` pushes a `Release[A] rel resource` action onto the current frame. Unlike
ordinary `defer`, this release action owns the hidden `@1` resource directly and does not arise from closure capture.

When control exits a `do`-scope boundary for any reason, all exit actions stored in that frame are executed exactly once
in LIFO order. If the same completion continues outward across further `do`-scope boundaries, those outer frames are
unwound in turn, from inner to outer. Normatively, implementations must behave as if each `defer` attached a finalizer
to its target scope frame via `MonadFinally`, and each `using` attached a hidden release obligation to that same frame.

The detailed error-propagation algorithm below applies only when an implicit `MonadError m` is in scope. When
`MonadError m` is not in scope, `finally` simply sequences the finalizer after the main action using ordinary monadic
composition over the completion-carrying result.

Let the scope's exit-action stack (top-first) be: `a1, a2, ..., an`.

Let the scope body either:
* return a completion record `c : Completion(RetCtx, A)`, or
* propagate an initial monadic error `e_primary`.

Assume an implicit `MonadError m` instance with associated error type `E = MonadError.Error m`.

The compiler evaluates the exit-action stack exactly as follows:

1. Execute `a1` through `an` in strict sequential LIFO order.
   * If `ai = Deferred d`, run `d`.
   * If `ai = Release[A] rel resource`, run `rel resource`.
2. Execution continuity: if an exit action propagates a monadic error, the unwinder MUST NOT short-circuit. It must
   catch or suspend that error and proceed to execute the next exit action.
3. Primary error tracking: the unwinder maintains `primary : Option E`.
   * If the body returned `c`, initialize `primary = None`.
   * If the body propagated `e_primary`, initialize `primary = Some e_primary`.
4. Whenever an exit action propagates an error `ei`:
   * If `primary = None`, set `primary = Some ei`.
   * If `primary = Some e`, retain `e` as the propagated error. The implementation MAY attach `ei` to `e` as
     suppressed-finalizer information for diagnostics or debugging, or MAY discard `ei`.
5. After all exit actions have run:
   * If `primary = Some e`, propagate the tracked monadic error `e` outward, abandoning any original completion record.
   * If `primary = None`, propagate the original completion record `c`.
6. The propagated error is therefore the first monadic error encountered in the unwinding sequence: `e_primary` if the
   body already failed, otherwise the first exit-action error observed in LIFO order. Later exit-action errors MUST NOT
   replace that primary error, though implementations MAY retain them as suppressed diagnostic information.

Linearity note: because the unwinder executes every exit action before propagating the selected error, all quantity-`1`
deferred obligations and all hidden owned-resource release obligations are consumed even when the scope exits by error.

<!-- effects.elaboration_model.do_scope_exit_actions.scope_unwinding_targeted_defer -->
##### 8.7.2.1 Cross-scope unwinding and targeted `defer`

Suppose a completion record or propagated monadic error leaves the innermost active `do`-scope and continues outward
across the sequence of scope frames `F1, F2, ..., Fk`, listed from innermost crossed frame to outermost crossed frame,
before it is finally consumed by its target construct.

The unwinder applies the single-frame algorithm of §8.7.2 to `F1`, then to `F2`, and so on through `Fk`.

Observable ordering:

* Within a single scope frame, exit actions run in strict LIFO order.
* Across multiple exited scope frames, every exit action in an inner exited frame runs before any exit action in an
  outer exited frame.

Therefore, if `break@L`, `continue@L`, or `return@L` exits several nested `do`-scopes at once, every exited scope's exit
actions run exactly once in this order before the control completion is consumed by the loop, lambda, function, or other
construct that targets it.

A deferred action scheduled by `defer@L` into an outer frame remains pending while inner frames exit. It runs only when
the target frame `L` itself is unwound. If `L` names the current `do`-scope, `defer@L e` is observationally equivalent
to ordinary `defer e`.

The in-flight unwinding sequence cannot itself be replaced by a new abrupt control transfer originating from a deferred
action. By the restriction of §8.6, a deferred or `finally` action may complete normally or propagate a monadic error,
but it cannot `return`, `break`, or `continue` to any scope outside that action. Therefore the crossed frames `F1 ...
Fk` are always unwound in the specified inner-to-outer order, subject only to the primary-error selection rules of
§8.7.2.

When `MonadError m` is present, primary-error selection across multiple exited scope frames is defined by repeated
application of §8.7.2. Equivalently, the propagated error is the first error encountered in the actual inner-to-outer
unwinding sequence, while later finalizer errors may be suppressed or discarded but do not replace it.

<!-- effects.elaboration_model.completion_aware_sequencing -->
#### 8.7.3 Completion-aware sequencing (schematic helpers over the KCore kernel)

For an enclosing monad `m`, we write computations of the form `m (Completion(RetCtx, A))`.

Using the explicit `Completion` constructors of §17.3.1.4, the following helpers are definable in KCore or an
observationally equivalent internal representation:

* Lifting a normal monadic computation:

  `lift : m A -> m (Completion(RetCtx, A))`

  `lift act` runs `act` and returns `Normal` of its result.

* Completion bind:

  `bindC : m (Completion(RetCtx, A)) -> (A -> m (Completion(RetCtx, B))) -> m (Completion(RetCtx, B))`

  `bindC ma k` runs `ma` and:
    * if it returns `Normal a`, runs `k a`,
    * otherwise propagates `Break` / `Continue` / any `Return[...]` unchanged.

Sequencing is `thenC x y = bindC x (\_ -> y)`.

<!-- effects.elaboration_model.do_item_sequences -->
#### 8.7.4 Elaboration of `do`-item sequences

A `do` block is elaborated in a context that records the resolved labels and result types of the enclosing named
function or method, inherited-name lambdas (§6.1), and any enclosing labeled lambdas that may be targeted by `return`.
This finite mapping is the current `RetCtx`.

Whenever a do-item expression contains one or more occurrences of `!`, the compiler MUST first translate that expression
by §5.7.1 and then apply the corresponding do-item rule to the resulting monadic computation.

For a do-scope body consisting of a non-empty sequence of do-items, elaboration yields:

* For a final do-item `item` that may complete normally with `A`: `⟦item⟧ : m (Completion(RetCtx, A))`

* For a sequence `item; rest` where `rest` ultimately produces `A`: `⟦item; rest⟧ : m (Completion(RetCtx, A))`

The cases below define `⟦...⟧` for arbitrary `A`.

<!-- effects.elaboration_model.result_producing_item -->
#### Result-producing item

* Expression item `final` where `final : m A`, and where normal execution reaching `final` means no later do-item is
  reached:

  `⟦final⟧ = lift final`

<!-- effects.elaboration_model.sequenced_items -->
#### Sequenced items

* Expression item `act` where `act : m A`:

  `⟦act; rest⟧ = thenC (lift act) ⟦rest⟧`

* Monadic bind `let bindPat <- act` where `act : m A` and the underlying pattern `pat0` of `bindPat` is irrefutable for
  `A` (§8.2):

  `⟦let bindPat <- act; rest⟧ = bindC (lift act) (\tmp -> ⟦rest⟧)` with `pat0` bound to `tmp` under the quantity
  annotation, if any, carried by `bindPat`.

* Refutable local binding `let? pat = expr`:

  `⟦let? pat = expr; rest⟧ = bindC (lift matched) (\tmp -> ⟦rest⟧)` where

  ```kappa
  matched =
      match expr
      case tmp@pat -> pure tmp
      case _   -> empty
  ```

  where `tmp` is fresh. This case requires `Alternative m` and is permitted only when the discarded refutation residue
  is droppable under §8.2.1.

* Refutable local binding with explicit failure arm `let? pat = expr else residuePat -> failExpr`:

  ```kappa
  ⟦let? pat = expr else residuePat -> failExpr; rest⟧ =
      match expr
      case pat        -> ⟦rest⟧
      case residuePat -> termElse(failExpr)
  ```

  where `pat` and `residuePat` are checked as a disjoint, jointly exhaustive split of the scrutinee type, and
  `termElse(failExpr)` denotes terminal failure-arm elaboration:

  * if `failExpr` is an explicit abrupt-control do-item, `termElse(failExpr)` is its ordinary abrupt-control
    elaboration;
  * otherwise `failExpr` must have type `m Void`, and

    ```kappa
    termElse(failExpr) = bindC (lift failExpr) (\v -> pure (Normal (absurd v)))
    ```

* Pure local binding `let bindPat = expr`:

  If `bindPat` carries quantity `&`, elaboration performs a borrowed binding of the underlying pattern `pat0` against a
  borrowable place expression (§5.1.7.2) and continues with `rest`. If `expr` is not already a borrowable place
  expression (§5.1.7.2), elaboration first introduces a fresh hidden temporary root:

  ```kappa
  let 1 __tmp = expr
  let & pat0 = __tmp
  rest
  ```

  where `pat0` is the underlying pattern of `bindPat` and `__tmp` is fresh and inaccessible to user code.

  If `bindPat` does not carry quantity `&`, it elaborates as a pure binding of the underlying pattern of `bindPat`,
  introducing each bound variable at the quantity carried by `bindPat`, and continues with `rest`.

<!-- effects.elaboration_model.defer_using -->
#### `defer` and `using`

Let `S_current` be the resolved label of the current dynamic `do`-scope.

* `defer d; rest`:

  `⟦defer d; rest⟧ = ScheduleExit S_current (Deferred d) ⟦rest⟧`

* `defer@L d; rest`:

  `⟦defer@L d; rest⟧ = ScheduleExit L (Deferred d) ⟦rest⟧`

  If `L` names an outer `do`-scope, the deferred action remains attached to that outer scope and is not run merely
  because inner scopes exit.

* `using pat0 <- acquire; rest`:

  elaborates by protected-scope splitting, not by ordinary closure capture:

  1. Run `acquire : m A` to obtain an owned resource `__res : A` at quantity `@1`.
  2. Select `rel = Releasable.release` from the implicit `Releasable m A` evidence.
  3. Introduce `pat0` as the ordinary borrowed binding of the protected resource against the hidden stable root `__res`.
  4. Attach `Release[A] rel __res` to the current scope with `ScheduleExit`.
  5. Continue with `rest`.

  Schematically:

  ```kappa
  ⟦using pat0 <- acquire; rest⟧ =
      bindC (lift acquire) (\__res ->
          ScheduleExit S_current (Release[A] rel __res)
              (⟦rest⟧ under the borrowed binding of pat0 against __res))
  ```

Any `return`, `break`, or `continue` inside `rest` therefore becomes an ordinary `Completion(...)` payload that
`DoScope` ferries outward after executing the scheduled exit action exactly once.

<!-- effects.elaboration_model.elaboration_return -->
#### Elaboration of `return`

* `return` elaborates as:

  `⟦return e⟧      = pure (Return[L_current] e)`

  `⟦return@L e⟧    = pure (Return[L] e)`

  where `L_current` is the resolved label of the nearest enclosing named function, inherited-name lambda (§6.1), or
  method at the use site (§8.4), and `Return[L]` denotes the control constructor associated with label `L` in the
  current `RetCtx`.

* A target construct with label `L_tgt` consumes a `Return[L_tgt] r` completion. On match, the construct produces `pure
  r` in its monad and terminates. On non-match, the construct propagates the completion outward unchanged, unwinding
  `defer` / `finally` in the usual way.

* Target existence is guaranteed by §8.4's compile-time resolution rules: every well-typed `Return[L] r` completion has
  a matching enclosing target on every dynamic execution path.

<!-- effects.elaboration_model.break_continue -->
#### `break` / `continue`

Within a loop body, each `break` / `continue` targets a specific loop label:

* Resolution is lexical within the current user-written function or lambda body and MUST NOT cross a user-written lambda
  or local-function boundary.
* Compiler-generated closures introduced solely by desugaring of loops or comprehensions are transparent to this
  resolution.
* Unlabeled targets the nearest enclosing loop in that same body.
* Labeled targets the named enclosing loop in that same body.

Elaboration:

* `⟦break⟧ = pure (Break L)`
* `⟦continue⟧ = pure (Continue L)`

where `L` is the resolved target label.

<!-- effects.elaboration_model.loop_elaboration -->
#### 8.7.5 Normative elaboration of loops

Loops are statements within `do` and elaborate to `m (Completion(RetCtx, Unit))`.

<!-- effects.elaboration_model.while_loops -->
#### While loops

For:

```kappa
while cond do
  body
else do
  onNoBreak
```

Let `L` be the loop's label (explicit or implicit). Let `cond` be either `Bool` or `m Bool` (§8.5).


The meaning is:

* Repeatedly evaluate `cond`:
    * If false: the loop completed normally; run `onNoBreak` (if present) and return `Normal ()`.
    * If true: execute one iteration by entering a fresh dynamic `do`-scope for `body` (so its defers run on
      `continue`/`break`/`return`/error).
* If the iteration completes with:
    * `Normal ()` or `Continue L`: begin the next iteration.
    * `Break L`: exit the loop immediately with `Normal ()`, and SKIP the `else` block.
    * `Continue L'` or `Break L'` where `L' ≠ L`: propagate outward unchanged.
    * `Return[L_ret] r`: propagate outward unchanged.

The loop `else` block runs iff the loop completes by condition becoming false, and no `Break L` occurs.

If the `else do` block is present, it is executed as a fresh dynamic `do`-scope (so any `defer` inside it runs on exit
as usual).

<!-- effects.elaboration_model.loops -->
#### For loops

For:

```kappa
for x in xs do
    body
else do
    onNoBreak
```

Let `L` be the loop label (explicit or implicit). The loop iterates `xs` in its iteration order.

Each iteration executes `body` in a fresh dynamic `do`-scope.

When early-exit behavior is required (as classified in §8.7.6), the portable iteration order and exhaustion behavior are
those of repeated application of the standard `Iterator.next` protocol to the current iterator state, starting from
`xs`.

Completion behavior matches `while` above:

* `Continue L` continues with the next element.
* `Break L` exits the loop and skips `else`.
* Mismatched labels propagate outward.
* Any `Return[L_ret] r` propagates outward.

The loop `else` block runs iff iteration is exhausted and no `Break L` occurs.

<!-- effects.elaboration_model.for_loop_desugaring -->
#### 8.7.6 Minimal desugaring requirements for `for` loops

Implementations MUST NOT require stronger iteration capabilities than necessary:

* If the loop body contains no `break`/`continue` whose resolved target is this loop or any enclosing loop, AND contains
  no `return` statement, AND the loop has no `else` block, the implementation MUST accept a desugaring equivalent to a
  `Foldable`/`Traversable` left-to-right traversal (e.g. `foldlM` / `traverse_`) and MUST NOT require the standard
  `Iterator` protocol. In particular, such a loop MUST typecheck when `xs` supplies only the iteration capabilities
  needed for that traversal (for example `Foldable`, or `Foldable` plus `Functor` for helper combinators), and the
  implementation MUST NOT require `xs` itself to have a `Monad` instance.

* If the loop body contains a `break`/`continue` that can exit the loop early (as above), OR contains `return`, OR the
  loop has an `else` block, then early-exit behavior is required. The implementation MUST use (or behave equivalently
  to) the standard `Iterator` protocol:

  ```kappa
  trait Iterator (it : Type) =
      Item : Type
      next : (1 this : it) -> Option (item : Item, rest : it)
  ```

  The loop expression `xs` is the initial iterator state. Repeatedly calling `next` yields either:
  * `Option.Some (item = x, rest = xs')`, in which case one loop iteration executes with `x` bound by the loop pattern
    and `xs'` becomes the next iterator state; or
  * `Option.None`, in which case the iterator is exhausted.

  The observable semantics MUST match §8.7.5: remaining elements are not advanced once `break`, `return`, propagated
  abrupt control, or normal exhaustion decides the loop outcome. Any compiler-generated closures or callbacks used to
  realize this behavior are transparent to the abrupt-control boundary rules of §8.4 and §8.5.

<!-- effects.inout -->
### 8.8 `inout` parameters

`inout` is purely surface-level linear-state-threading sugar. It does not extend Kappa's core type theory.

<!-- effects.inout.meaning_inout -->
#### 8.8.1 Meaning of `inout`

When the user writes:

```kappa
let read (inout f : File Open) (n : Nat) : IO (data : Bytes, 1 f : File Open)
```

the compiler elaborates the parameter binder to an ordinary linear binder:

```kappa
let read (1 f : File Open) (n : Nat) : IO (data : Bytes, 1 f : File Open)
```

with the additional `inout` well-formedness condition that the result record contains a quantity-`1` field named `f`.
That returned field is the threaded linear successor of the parameter `f`, expressed using the ordinary
quantity-annotated record-field machinery of §5.5. Its type need not be definitionally equal to the input type; this
allows typestate transitions while preserving the same local variable name at call sites.

`inout`:

* forces quantity `@1` on the parameter;
* requires a matching quantity-`1` field of the same name in the result record, after peeling any enclosing monad
  required by the binding form;
* enables `~` call-site rebinding sugar inside `do` blocks;
* does not change the underlying pure-vs-monadic shape of the call.

In-place mutation guarantee (as-if rule):

Because an `inout` parameter consumes a value linearly and returns its threaded successor under the same formal name,
implementations SHOULD optimize this pattern to zero-copy in-place mutation when that is safe. The observable semantics
remain pure linear state threading.

<!-- effects.inout.syntax -->
#### 8.8.2 `inout` syntax

Declaration syntax:

```kappa
let read (inout f : File Open) (n : Nat) : IO (data : Bytes, 1 f : File Open)
```

Call-site syntax inside `do`:

```kappa
do
    let (data = bytes) <- read ~file 1024
    step ~weights ~bias
```

`inout` is permitted only on explicit parameters of named functions and methods. It is not permitted in lambdas or
ordinary function types; those use the elaborated `@1` binder form directly.

<!-- effects.inout.call_site_marker -->
#### 8.8.3 Call-site marker `~`

`~` marks an argument supplied to an `inout` parameter.

Rules:

* `~` is valid only inside a `do` block, and only on arguments of the maximal application site (§7.1.3) that forms a
  `do`-item rewrite site.
* `~` may be applied only to:
  * a stable place as defined in §5.1.7.1; or
  * a parenthesized, fully applied projection call under §6.1.1.
* Parentheses are required around a projection call under `~`, for example `~(focusedBuffer editor)`.
* If the source place is rooted at a `var`-bound name, elaboration first reads the current contents of that `Ref` into a
  fresh hidden temporary root and then proceeds on that temporary root.
* A given stable place, or a given projection call occurrence, may appear in at most one `~` argument within a single
  application.
* For a projection call, static disjointness checking uses the static footprint summary of §5.1.7.2.
* A `~place` argument is well-formed if and only if the compiler can statically resolve the callee at that application
  site to a Pi-telescope in which the corresponding explicit parameter:
  * has quantity `@1`,
  * has a known formal binder name `N`, and
  * the function's return type (after peeling the enclosing monad required by the binding form) is a record type
    containing a quantity-`1` field named `N`.
* The binder name `N` must come from the resolved interface of the callee (for example, an ordinary
  declaration/definition, an explicit function type annotation, or a resolved trait member declaration). If overload
  resolution, implicit resolution, type inference, or opacity prevents the compiler from recovering such a stable formal
  name, `~` is rejected.
* Every explicitly supplied parameter satisfying the above `inout`-compatibility condition must be written with `~`.
* A surface call to an `inout` parameter without `~` is a compile-time error.
* `~` has no other term-level meaning in Kappa.

Diagnostic requirement:

* If the callee's corresponding formal parameter has a known name `N` but the return record does not contain a
  quantity-`1` field `N`, the diagnostic SHOULD say that `~place` cannot be used because the return record does not
  contain a quantity-`1` field matching formal parameter name `N`.
* If the compiler cannot recover a stable formal name for the corresponding parameter, the diagnostic SHOULD say that
  `~place` cannot be used because the callee does not expose an `inout`-compatible formal parameter at that position.

<!-- effects.inout.desugaring -->
#### 8.8.4 Normative desugaring (type-directed)

A maximal application site containing one or more `~` arguments is rewritten before the ordinary `do`-item elaboration
of §8.7.4.

For a surface form:

```kappa
let pat <- func ~x1 ... ~xk args
```

or the pure analogue `let pat = func ~x1 ... ~xk args`, elaboration proceeds as follows:

1. Resolve the callee's signature for the maximal application site and determine the ordered list of
   `inout`-compatible formal parameter names `p1 ... pk` corresponding to the marked arguments in that resolved
   application spine. This step succeeds only when each marked argument position satisfies the admissibility rule of
   §8.8.3: quantity `@1`, stable formal name, place expression, and matching quantity-`1` return-record field.
2. Elaborate each marked argument as follows:

   * If the marked argument is a stable place, proceed exactly as in the stable-place case of this section.
   * If the marked argument is a fully applied projection call, elaboration first constructs the projector descriptor
     value `proj` and the internal place pack `pack` for that call, then elaborates the marked argument as
     `OpenProjector proj pack`.
   * The callee receives the `focus` component of the resulting zipper.
   * On return, the returned successor is written back by applying the zipper's linear `fill` component.
   * If the zipper rebuilds a root pack containing more than one root field, the rebuilt pack is then scattered back to
     the corresponding actual stable roots in declaration order.
3. Determine the returned record type `R`:
   * for `let pat <- ...`, the call must have type `m R`;
   * for `let pat = ...`, the call must have type `R`.
4. Check that `R` is a record type containing fields `p1 ... pk`. Those fields are the threaded successors of the
   `inout` parameters and must be declared at quantity `1`. Let the residual record be the record obtained from `R` by
   removing those `inout` fields.
5. Elaborate the returned `inout` fields through fresh temporaries `__p1_tmp ... __pk_tmp` so that write-back does not
   interfere with residual pattern matching.
6. For each marked argument, matched with formal parameter `pᵢ`, elaborate the returned field `__pᵢ_tmp` back through
   the corresponding write-back target:

   * If the marked argument elaborated as a stable place `Pᵢ`, and `Pᵢ` is the whole place `x`, where `x` is an
     ordinary immutable local binding, rebind `x` from `__pᵢ_tmp`, shadowing the previous binding exactly as in
     ordinary linear state threading.
   * If the marked argument elaborated as a stable place `Pᵢ`, and `Pᵢ` is a proper subplace of an ordinary immutable
     local binding `x`, rebuild the root by filling `Pᵢ` with `__pᵢ_tmp`, then rebind `x` to the rebuilt root.
   * If the marked argument elaborated as a stable place `Pᵢ` originating from a `var`-bound root, rebuild the hidden
     temporary root by filling `Pᵢ` with `__pᵢ_tmp` and then write the rebuilt root back to the corresponding `Ref`.
   * In KCore these stable-place cases behave as `FillPlace Pᵢ __pᵢ_tmp` together with ordinary rebinding or
     `writeRef`, or as an observationally equivalent internal form (§17.3.1.1).
   * If the marked argument elaborated as `OpenProjector proj pack`, the restoration is the write-back performed by the
     zipper fill obtained from `OpenProjector`. If that fill rebuilds a root pack containing more than one root field,
     the rebuilt pack is then scattered back to the corresponding actual stable roots in declaration order.
   * Because place filling is observationally equivalent to nested record updates, it is subject to the dependent-record
     update rules of §5.5.5. If the typestate transition of the restored `inout` path invalidates the type of any
     omitted sibling field in the rebuilt record, the `inout` application is a compile-time error. The user must unpack
     the record, perform the call, and manually reconstruct the dependent record, explicitly supplying any affected
     sibling fields.
   * When several `~` arguments are present, these restorations occur left-to-right in the order of the marked
     arguments.
7. Match the user-written pattern `pat` against the residual result:
   * The residual result is always treated as a record. There is no implicit coercion from a one-field residual record
     `(label : T)` to the payload type `T`.
   * If the residual record is non-empty, `pat` must therefore be an anonymous record pattern compatible with the
     residual fields.
   * If the residual record is empty (definitionally `Unit`), then `pat` must be `_` or `()`, or the binding may be
     omitted entirely via the bare-statement form below.
   * `pat` MUST NOT bind any name that also appears as a `~` argument in the same application.

Schematic behavior:

```kappa
-- Surface
let (data = bytes) <- read ~file 1024

-- behaves as if
let (f = __f_tmp, data = bytes) <- read file 1024
let file = __f_tmp
```

```kappa
-- Surface
let (data = bytesRead) <- read ~file 1024

-- behaves as if
let (f = __f_tmp, data = bytesRead) <- read file 1024
let file = __f_tmp
```

```kappa
-- Surface
let (data = bytes) <- read ~myRec.file 1024

-- behaves as if
let (f = __f_tmp, data = bytes) <- read myRec.file 1024
let myRec = myRec.{ file = __f_tmp }
```

The returned `inout` fields are rebound using the callee's formal parameter names, not the caller's local names. Thus
`~file` passed to formal parameter `f` is rebound from returned field `f`, and `~myRec.file` restores the local path
`myRec.file` from that same returned field `f`.

If the caller-side `file` is a `var`, the corresponding schematic write-back is instead:

```kappa
let (f = __f_tmp, data = bytes) <- read file 1024
file = __f_tmp
```

so the mutable place remains in scope and receives the threaded successor.

Bare-statement form:

```kappa
step ~weights ~bias
```

is permitted only inside `do`, only for a monadic call, and only when the residual result is empty. It elaborates as the
corresponding internal monadic bind that rebinds the returned `inout` fields and ignores the empty residual.

The `~` marker does not insert or remove monadic structure. The user still chooses between `let ... =` and `let ... <-`
at the surface.

Because anonymous record patterns in Kappa use parentheses rather than braces, the explicit residual-binding form is:

```kappa
let (data = bytes) <- read ~file 1024
```

<!-- effects.inout.examples -->
#### 8.8.5 Examples

Single-field residual result:

```kappa
let (data = bytes) <- read ~file 1024
-- bytes : Bytes
-- file is rebound from the returned field `f`
```

Explicit residual record binding:

```kappa
let (data = bytesRead) <- read ~file 1024
```

Interaction with `var`:

```kappa
do
    var file = initialFile
    let (data = bytes) <- read ~file 1024
    -- `file` is still the mutable variable; its contents were updated
    -- from the returned field `f`
```

Path-sensitive reconstitution:

```kappa
do
    let (data = bytes) <- read ~session.file 1024
    -- `session.file` was consumed for the call and then restored from the
    -- returned field `f`
```

Rejected higher-order shape mismatch:

```kappa
let bad : (1 a : Int) -> IO (b : Int)

do
    let (b = y) <- bad ~x
    -- ERROR: cannot use `~x`; return record does not contain a
    -- quantity-`1` field matching the formal parameter name `a`
```

Typestate transition:

```kappa
let close (inout f : File Open) : IO (1 f : File Closed)

do
    close ~file
    -- file now refers to File Closed
```

Handler-style state threading:

```kappa
let runState (inout st : s) comp : Eff r (result : a, 1 st : s) =
    let handlerObj =
        deep handle state comp with
          case return x -> \st0 -> pure (result = x, st = st0)
          case get () k -> \st0 -> k st0 st0
          case put st1 k -> \_ -> k () st1
    in
        handlerObj st

do
    let (result = x) <- runState ~st comp
```

This works because `inout` elaborates to the same linear record-threading shape already used by stateful handlers:
consume the current state, produce a residual record that includes the threaded successor under the same field name, and
rebind that field at the call site via `~`.

Multiple `inout` parameters with empty residual:

```kappa
step ~weights ~bias
```

<!-- effects.inout.restrictions -->
#### 8.8.6 Restrictions

* `inout` call syntax is valid only inside `do` blocks.
* `~` may be applied only to stable places: variables, or record projection paths rooted at variables or `var`-bound
  mutable places.
* Each `inout` parameter name must have a matching quantity-`1` field of the same name in the result record.
* `inout` is permitted only on explicit parameters of named functions and methods, not on lambda binders or in ordinary
  function types.
* If the residual result has multiple fields, the user must bind it with an anonymous record pattern naming those
  residual fields.
* The user-written residual pattern must not bind any name also used as a `~` argument in the same application.

---

<!-- errors -->
## 9. Errors and `try match`

<!-- errors.monad_error -->
### 9.1 `MonadError`

Error-aware monads are described via a trait like:

```kappa
trait MonadFinally m => MonadError (m : Type -> Type) =
    Error : Type
    throwError : Error -> m a
    catchError : m a -> (Error -> m a) -> m a
```

The prelude minimum requires a trait providing at least these operations and associated type; this section uses the name
`MonadError`. Any `MonadError m` instance MUST also determine a `MonadFinally m` instance for the same `m`.

When both a primary computation and one or more finalizers fail during unwinding, the primary error is the one
propagated by ordinary `throwError` / `catchError` semantics. Later finalizer errors do not change that ordinary
propagated error. Implementations MAY attach such secondary errors to the primary error as suppressed diagnostic
information, or MAY discard them, but this behavior is implementation-defined and is not part of portable pattern
matching over `Error`.

For the standard runtime carrier `IO e`, the associated `Error` type of `MonadError (IO e)` is exactly `e`.

For `IO e`:

* `throwError` and `catchError` operate only on the `Fail e` branch of `Cause e`;
* interruption and defects are not part of `MonadError.Error (IO e)`;
* interruption and defects are therefore not caught by `catchError`;
* code that must observe or recover from interruption or defects MUST use `sandbox`, `await`, `Exit`, or `Cause`
  inspection explicitly.

<!-- errors.try_except_finally -->
### 9.2 `try` / `except` / `finally`

`try` handles errors for monadic computations.

Syntax:

```kappa
try expr
  except pat1 if guard1 -> handler1
  except pat2           -> handler2
  finally               -> finalizer
```

Rules:

* `expr` has type `m a`, where `m` supports error handling (conceptually `MonadError m`).
* Suggested formatting: examples in this specification indent `except` and `finally` clauses by two spaces relative to
  the `try` head. This is a readability convention only. A conforming parser MUST accept both aligned and indented
  clause forms.
* Each `except` handler must produce type `m a`.
* `finally` (optional) is a monadic action of type `m Unit` that runs whenever control exits the `try` expression,
  including:
    * normal success,
    * an error that is handled by an `except`,
    * an error that propagates out of the `try`,
    * a `break`/`continue` propagating through the `try`,
    * a `return` propagating through the `try`.
* Let `E = MonadError.Error m`. The error type `E` must be a closed data type or a closed variant type.
* `except` clauses are checked for exhaustiveness exactly as an ordinary `match` over `E`.
* The `finally` clause is implemented via `defer` and therefore requires `MonadFinally m`.

Semantics sketch:

1. Evaluate `expr`.
2. If `expr` succeeds, the result is the `try` result.
3. If `expr` raises an error, run the first matching `except` handler and use its result.
4. If `finally` is absent, the result of the `try` expression is the result produced by steps 1-3.
5. If `finally` is present, the whole form elaborates to:

   ```kappa
   do
       defer finalizer
       try expr
         except pat1 if guard1 -> handler1
         except pat2           -> handler2
   ```

   where the inner `try` is the same construct with no `finally` clause.
6. The unwinder of §8.7.2 therefore applies: `finalizer` always runs, and if both the primary computation and
   `finalizer` raise errors, the result is `throwError primary`. The `finalizer` error may be attached to `primary` as
   suppressed diagnostic information, or may be discarded, but it does not replace `primary` as the ordinary propagated
   error.

Restrictions:
* The `finally` clause is elaborated as `defer`; the restrictions of §8.6 apply.

Control-flow interaction:
* `except` handlers match only monadic error values. They do not intercept `break`, `continue`, or `return`. Such
  control flow propagates outward normally, but `finally` still runs as specified above.

<!-- errors.try_match -->
### 9.3 `try match`

`try match` is syntactic sugar that combines a monadic bind + `match` on success with `try` error handling.

Normative desugaring:

```kappa
try match expr
  case p1 if g1 -> s1
  case p2       -> s2
...
  except q1 if h1 -> e1
  except q2       -> e2
...
  finally         -> fin
```

Suggested formatting:

* Examples in this specification indent success-side `case` clauses, `except` clauses, and `finally` by two spaces
  relative to the `try match` head.
* This is a readability convention only. A conforming parser MUST accept both aligned and indented clause forms.

desugars to:

```kappa
try
    do
        let __tmp <- expr
        match __tmp
          case p1 if g1 -> s1
          case p2       -> s2
        ...
  except q1 if h1 -> e1
  except q2       -> e2
...
  finally          -> fin
```

(where `__tmp` is a fresh name).

Therefore:

* error handling and `finally` behavior are exactly those of `try` (§9.2), with `finally` realized via `defer`,
* success-side exhaustiveness is exactly that of `match` (§7.5),
* error-side exhaustiveness is exactly that of `try`'s `except` clauses (§9.2).

---

<!-- errors.raise -->
### 9.4 `raise`

`raise err` is sugar for throwing an error in the current error monad:

* In any context with an implicit `MonadError m`, if `err : MonadError.Error m` then `raise err : m a` desugars to
  `throwError err`.

`raise` is permitted anywhere an expression of type `m a` is expected (commonly inside `except` handlers).

<!-- errors.monad_resource -->
### 9.5 `MonadResource` and scoped allocation

To support resource-safe allocation outside of explicit `do`-block syntax, the prelude provides `MonadResource`. It
refines `MonadFinally` with a scoped-allocation combinator that guarantees release even when the body exits by the
ordinary `do`-scope unwinding behavior of §8.7.

```kappa
trait MonadFinally m => MonadResource (m : Type -> Type) =
    bracket :
        forall (a : Type) (b : Type).
        (1 acquire : m a) ->
        (1 release : (1 r : a) -> m Unit) ->
        (1 use : (& r : a) -> m b) ->
        m b
```

The prelude minimum requires a trait providing at least this operation; this section uses the name `MonadResource`.
Because `bracket` must retain ownership of the acquired resource for the eventual `release` while simultaneously
exposing only a borrowed view to `use`, it is not in general expressible in surface v0.1 by the naive source-level
expansion:

```kappa
do
    let 1 res <- acquire
    defer (release res)
    use res
```

That expansion would move `res` into the deferred closure before the borrowed call to `use`. Accordingly, an
implementation that provides `bracket` MUST realize it using protected-scope machinery equivalent to `using`, whether as
a library primitive, a recognized intrinsic, or an elaboration to the same internal resource-scope representation.

Typing and borrow-checking note:

* A correct implementation of `bracket` keeps the owned resource in hidden protected-scope state until exit, while
  giving `use` only a borrowed view.
* The borrowed value passed to `use` remains subject to the region and escape rules of §5.1.6.

As a result, `bracket` and `using` realize the same core discipline: acquire an owned resource, permit only borrowed use
within the protected region, and ensure the owned resource is eventually consumed by `release`.

<!-- collections -->
## 10. Collections, Ranges, and Comprehensions

<!-- collections.literals -->
### 10.1 Built-in collection literals

* **List:**

  ```kappa
  [1, 2, 3]
  [True, False]
  []
  ```

* **Set:**

  ```kappa
  {|1, 2, 3|}
  {| |}       -- empty set
  ```

* **Map:**

  ```kappa
  { "a": 1, "b": 2 }
  {}                     -- empty map
  ```

Empty `{}` denotes an empty map; empty set uses `{| |}`.

<!-- collections.ranges -->
### 10.2 Range operators

`..` and `..<` are ordinary operator identifiers whose fixities are expected to be provided by `std.prelude`.

They are defined conceptually in terms of a trait such as:

```kappa
trait Rangeable (v : Type) =
    Range : Type
    range : (from : v) -> (to : v) -> (exclusive : Bool) -> Range
```

Prelude implementation:

* `from .. to`  ≡  `range from to False`
* `from ..< to` ≡  `range from to True`
* For coherent `Rangeable v` evidence, the result type is `Rangeable.Range v`.

<!-- collections.comprehensions -->
### 10.3 Comprehensions: general shape

There are three primary comprehension carriers:

* List comprehension: `[ ... ]`
* Set comprehension: `{| ... |}`
* Map comprehension: `{ ... }`

General structure:

```kappa
[   clauses..., yield valueExpr ]
{|  clauses..., yield valueExpr |}
{   clauses..., yield keyExpr : valueExpr }
```

Clause separators:

* Within comprehensions, clauses may be separated by newlines or commas.
* Commas are purely a separator (equivalent to a newline at the same layout nesting).

Top-level clause-boundary rule:

* Within a comprehension, a `NEWLINE` acts as a clause separator only when:
  * the parser is at comprehension clause depth 0,
  * the `NEWLINE` is not inside nested parentheses, brackets, braces, `{| |}`, strings, character literals, comments,
    or quotes,
  * and the preceding token does not place the parser in an ordinary continuation context under §3.4.

Otherwise the `NEWLINE` is treated as part of the surrounding expression syntax rather than as a clause boundary.

Example:

```kappa
[ for x in xs, let y = x + 1, if y > 2, yield y ]
```

The **clauses** appear before `yield` and may include:

* `for` / `for?` clauses
* `let` / `let?` clauses
* `if` filters
* `order by ...`
* `skip n`, `take n`
* `distinct` / `distinct by expr`
* `group by expr` with aggregations
* `join` / `left join` (joins)
* (all optional and composable)

<!-- collections.comprehensions.parsing_comprehension_literal -->
#### 10.3.1 Parsing: comprehension vs literal

A bracketed form may denote either a literal or a comprehension:

* `[e1, e2, ...]` is a list literal; `[ clauses..., yield valueExpr ]` is a list comprehension.
* `{|e1, e2, ...|}` is a set literal; `{| clauses..., yield valueExpr |}` is a set comprehension.
* `{ k1 : v1, ... }` is a map literal; `{ clauses..., yield keyExpr : valueExpr }` is a map comprehension.

Comprehension detection:

* After reading the opening delimiter (`[`, `{|`, or `{`), the parser skips whitespace and comments.
* A bracketed form is parsed as a comprehension iff the first non-comment token after the opener is one of:

  ```text
  yield, for, for?
  ```

  Otherwise the form is parsed as a literal.
* No additional lookahead is used for this disambiguation.
* `yield`, `for`, and `for?` are the only clause forms that may begin a comprehension.
* All other clause forms require an existing row stream and therefore cannot appear first. In particular, `let`, `let?`,
  `if`, `order`, `skip`, `take`, `distinct`, `group`, `join`, and `left join` may appear only after an initial `for` /
  `for?` clause or after a deliberately introduced future syntax that establishes an initial row stream.

This rule applies equally when the bracketed form is prefixed with a custom carrier (§10.9).

Escape hatch:

* To write a literal whose first element/key is an identifier that would otherwise be an initial comprehension token,
  parenthesize it or use a backtick identifier. Examples:

  ```kappa
  [ (yield) ]
  [ `yield` ]
  [ (for) ]
  {| (for) |}
  ```


<!-- collections.comprehensions.encounter_order_order_sensitive_clauses -->
#### 10.3.2 Encounter order and order-sensitive clauses

Comprehensions process elements in an encounter order induced by generator enumeration and preceding clauses.

For multiple `for` clauses, encounter order is the lexicographic order induced by left-to-right nesting: for each row
produced so far, the next `for` enumerates its collection and extends the row stream in that enumeration order. (This
corresponds to the standard `bind`/`flatMap` desugaring in §10.10.)

A comprehension pipeline is in one of two orderedness states:

* **Ordered**: the encounter order is specified by the semantics.
* **Unordered**: the encounter order is unspecified.

Rules for built-in sources:

* Iterating a `List` (and other sequence types with specified iteration order) produces an Ordered stream.
* Iterating a `Set` (`{| ... |}` and values of the built-in set type) produces an Unordered stream.
* Iterating a `Map` (`{ ... }` and values of the built-in map type) produces an Unordered stream.

Order propagation:

* `for` preserves Ordered iff the input is Ordered and the iterated collection's iteration order is specified; otherwise
  it produces Unordered.
* `let` clauses and `if` filters preserve the current orderedness.
* `order by ...` sets orderedness to Ordered.
* `distinct` / `distinct by` preserve Ordered when the input is Ordered; otherwise the result is Unordered.
* `group by` sets orderedness to Unordered. Consequently, `skip` / `take` after `distinct` remain well-formed only if
  the pipeline was already Ordered, whereas after `group by` a later `order by` is required before `skip` or `take` may
  be used (§10.6.2).
* Refutable forms preserve orderedness the same way filtering does:
  * `for? pat in xs` preserves Ordered iff `xs` is iterated in a specified order and the incoming stream is Ordered.
  * `let? pat = e` preserves the current orderedness (it's row-local and either keeps or drops the row).
* `join` / `left join` orderedness follows their normative desugaring in §10.8 (i.e. behaves like adding a `for tmp in
  ...` plus filters).

Order-sensitive clauses:

* `skip n` and `take n` are well-formed only when the pipeline is Ordered at that point; otherwise it is a compile-time
  error.

When orderedness is Unordered, any semantics that would rely on relative order (for example, choosing between equal
`order by` keys, or resolving "keep first/keep last" conflicts) is unspecified with respect to those ties.

Determinism:

* Unordered means that the language does not guarantee a particular user-visible encounter order.
* Unordered does not, by itself, authorize nondeterministic behavior.
* In package mode, for a fixed program, fixed inputs, and fixed implementation configuration, representative choice and
  conflict resolution on unordered pipelines MUST still be deterministic.
* Any truly random or statistically sampled behavior requires an explicit library or future language feature. It MUST
  NOT arise solely from the unordered status of a pipeline.


<!-- collections.clauses -->
### 10.4 `for` and `let` clauses

Examples:

```kappa
[
    for x in [1, 2, 3, 4, 5]
    let doubled = x * 2
    if doubled > 5
    yield doubled
]

{|  -- set
    for item in [1, 2, 2, 3, 4]
    if item > 1
    yield item
|}

{   -- map
    for x in [1, 2, 3]
    let value = x * x + x
    yield x : value
}
```

* `for pat in collection` binds elements of a collection.

    * `pat` is a pattern (§7.6).
    * In `for pat in collection`, `pat` must be **irrefutable** for the element type of `collection` (§6.1.2). If it is
      refutable, it is a compile-time error; use `for? pat in collection` (§10.4.1) instead.

* `let pat = expr` creates derived values within the comprehension.

    * `pat` must be irrefutable (§6.1.2).
    * For refutable matching, use `let? pat = expr` (§10.4.1).

* `if condition` filters out rows where `condition` is `False`.
  Within the remainder of the comprehension after an `if condition` clause, typechecking proceeds under an implicit
  assumption `@p : condition = True` for the current row (analogous to §7.4.1).
  If `condition` is syntactically a constructor-tag test `expr is C`, the same success-side constructor-refinement
  evidence as §7.4.1 is also in scope for later clauses.


Map iteration:

* Iterating a map yields entries as a record `(key : K, value : V)`. This enables destructuring via record patterns, for
  example:

  ```kappa
  { for (key = k, value = v) in myMap, yield k : v }
  { for (key = k) in myMap, yield k : 1 }
  ```


<!-- collections.clauses.refutable_patterns_comprehensions -->
#### 10.4.1 Refutable patterns in comprehensions

Comprehensions support refutable generators and refutable bindings:

**Refutable generator.** `for? pat in collection` iterates `collection` and keeps only elements matching `pat`.
Variables bound by `pat` are in scope in subsequent clauses. This form is ill-formed if dropping a non-matching element
would discard any component carrying a positive owned lower-bound obligation.

**Refutable binding.** `let? pat = expr` matches `expr` against `pat`. On success, pattern variables are bound and the
comprehension continues. On failure, the current row is dropped. This form is ill-formed if the discarded failure
residue would carry any positive owned lower-bound obligation.

For both forms, discarded values are droppable only when all discarded components carry quantities `0`, `&`, `<=1`, or
`ω`. Quantities `1` and `>=1` are not droppable here.

For an open variant element or scrutinee type `(| ... | r |)`, a refutable comprehension clause is permitted only when
the available row constraints prove that every case admitted by the residual row `r` is droppable. In the absence of
such proof, the compiler MUST conservatively reject the clause.

Normative lowering per §10.10 uses query-level refutable filtering / filter-map over the current row environment. The
carrier-specific trait factoring is not part of the surface semantics.

<!-- collections.map_comprehensions -->
### 10.5 Map comprehensions and `:` vs type ascription

In a **map comprehension**, `yield keyExpr : valueExpr` is always interpreted as a **key/value pair**, not a type
ascription.

* List/set comprehensions:

  ```kappa
  [ yield x : Int ]  -- type ascription
  ```

* Map comprehensions:

  ```kappa
  { for x in xs, yield x : x + 1 }  -- key/value
  ```

  If you need type ascription inside a map comprehension, parenthesise:
    
  ```kappa
  { for x in xs, yield (x : KeyType) : (x + 1 : ValType) }
  ```

  It may be followed by an optional map-conflict clause:

  ```kappa
  yield keyExpr : valueExpr
  on conflict ...
  ```

  The optional `on conflict` clause is permitted only in map comprehensions, must appear **after** `yield`, and must be
  the **final** clause in the comprehension.

  Duplicate keys in map comprehensions:
  * If a map comprehension produces the same key multiple times, the conflict policy determines the outcome (§10.5.1).
  * If no `on conflict` clause is present, the default policy is `keep last` in the comprehension's encounter order.

<!-- collections.map_comprehensions.map_key_conflicts_conflict -->
#### 10.5.1 Map key conflicts (`on conflict`)

A map comprehension may specify a key-conflict policy using a final post-`yield` clause:

```kappa
{
    clauses...
    yield k : v
    on conflict keep last
}

{
    clauses...
    yield k : v
    on conflict keep first
}

{
    clauses...
    yield k : v
    on conflict combine using Concat
}

{
    clauses...
    yield k : v
    on conflict combine with (\old new -> old + new)
}
```

Rules:

* `on conflict ...` is permitted only in map comprehensions.
* `on conflict ...` may appear at most once.
* `on conflict ...` MUST appear after the `yield` clause.
* `on conflict ...` MUST be the final clause in the comprehension.

Semantics:

* When multiple yielded pairs produce keys that are equal (by an `Eq`-like equality on keys), the conflict policy
  determines the final value associated with that key.

Policies:

* `keep last`: the value from the last encountered pair wins.
* `keep first`: the value from the first encountered pair wins.
* `combine using Wrapper`: values are combined by wrapping them with `Wrapper` and folding using the implied monoid
  (same wrapper-fold-unwrap mechanism as `group by`).
* `combine with f`: values are combined by left-folding `f` over the encountered values for the key.

Order note:

* For `keep first`, `keep last`, `combine using`, and `combine with`, results may depend on encounter order. If
  orderedness is Unordered at the point where pairs are produced, the outcome is unspecified with respect to ties
  (§10.3.2).

<!-- collections.ordering -->
### 10.6 Ordering, paging, distinct

<!-- collections.ordering.order -->
#### 10.6.1 `order by`

Syntax:

```kappa
order by expr
order by asc expr
order by desc expr
order by (asc expr1, desc expr2, expr3)
```

* Requires an `Ord`-like trait for the involved key types.
* Applies sorting to the current carrier.

Stability:
* `order by` is a stable sort: elements that compare equal under the ordering keys preserve their relative order from
  the input iteration order.

Tie behavior on unordered inputs:

* Stability is with respect to the **current encounter order**.
* If the current orderedness is Unordered (§10.3.2), then the relative order of elements with equal ordering keys is
  unspecified.

Evaluation:
* In `order by ...`, each ordering key expression is evaluated exactly once per row. Implementations may cache keys for
  sorting.

<!-- collections.ordering.skip_take -->
#### 10.6.2 `skip` and `take`

`skip` and `take` are order-sensitive clauses:

* `skip n` discards the first `n` rows in the current encounter order.
* `take n` limits the result to the first `n` rows in the current encounter order.

Well-formedness:

* `skip` and `take` are permitted only when the pipeline is Ordered at that point (§10.3.2). Using `skip` or `take` when
  orderedness is Unordered is a compile-time error.

Notes:

* After `group by` orderedness is Unordered until another `order by` is applied.
* After `distinct`, orderedness is preserved only if the input was Ordered.

<!-- collections.ordering.distinct_distinct -->
#### 10.6.3 `distinct` / `distinct by`

`distinct` and `distinct by` operate on the current row environment at the clause site.

* For `distinct`, the deduplication key is the current row record `Row(Γ)` of §10.10.2.
* For `distinct by keyExpr`, the deduplication key is `keyExpr` evaluated in that same row environment.


```kappa
distinct
distinct by keyExpr
```

* `distinct` keeps unique rows based on equality of `Row(Γ)` under the applicable `Eq` instance.
* `distinct by keyExpr` keeps the first encountered row for each unique deduplication key `keyExpr`.
* Requires an `Eq`-like trait for the value used to determine uniqueness. Hashing may be used as an optimization
  (implementation-defined).

Representative choice:

* `distinct` / `distinct by` preserves the **first encountered** representative in the current iteration order.
* If the carrier's iteration order is unspecified, the chosen representative is unspecified as well.

<!-- collections.grouping -->
### 10.7 Grouping

Grouping syntax:

```kappa
[
    for product in products
    group by product.category {
        items = [product] using Concat
        count = 1 using Sum
    } into g
    yield (g.key, g.count)
]
```

Form:

* `group by keyExpr { agg1, agg2, ... } into name`

Aggregate field form:

* `field = valueExpr using Wrapper`
* `field = valueExpr` (optional; aggregates directly using `Monoid` on the type of `valueExpr`)

Well-formedness:

* `Wrapper` must resolve to a constructor whose type is of the form `T -> W`, where `W` is a data type with exactly one
  constructor and that constructor has exactly one field (a "newtype-like" wrapper). Otherwise `using Wrapper` is a
  compile-time error.
* `using Wrapper` is well-formed only if there is an in-scope instance `Monoid W`, where `W` is the result type of
  `Wrapper`. Otherwise `using Wrapper` is a compile-time error.
* The prelude provides `Monoid` instances for the standard wrappers used for aggregation, such as `Sum`, `Product`, and
  `Concat`.

Rules:

* `keyExpr` is evaluated on each incoming row to produce the group key.
* The bound name `name` is a record containing:

    * `key` (always present), and
    * one field per aggregate declaration in the aggregation block.
* The aggregation block may not bind names; it contains only aggregate field declarations.
* Aggregate fields may refer to names bound by earlier comprehension clauses.
* Aggregate fields may not refer to other aggregate fields (compile-time error).

`key` is special:

* The group record always contains a field `key`.
* Declaring an aggregate field named `key` is a compile-time error.

Semantics (normative):

* Grouping partitions the incoming rows by the value of `keyExpr`.
* For each group key, aggregates are computed over the rows in that group.

Aggregate semantics with `using`:

* Let `valueExpr : T`.
* Let `Wrapper` resolve to a constructor `Wrapper : T -> W`.
* Let there be an implicit monoid instance for `W` (implementation-defined trait factoring, but conceptually: `empty :
  W` and `append : W -> W -> W`).

Then the aggregate value is computed as:

1. For each row in the group, compute `w = Wrapper (valueExpr)`.
2. Fold the `w` values using the monoid: `acc = fold append empty ws`.
3. Unwrap `acc` by pattern matching on the single-constructor wrapper.

Aggregate semantics without `using`:

* If `using Wrapper` is omitted and `valueExpr : T`, then the aggregate field is well-typed iff an implicit `Monoid T`
  instance is available.
* In that case, the result type of the aggregate field is exactly `T`.
* The aggregate value is computed by evaluating `valueExpr` once per row in the group and folding `append` over those
  per-row results starting from `empty`.
* There is no implementation-defined default monoid for this case.

Encounter order after grouping:

* The output of `group by` has Unordered encounter order (§10.3.2) until another `order by` is applied.

Implementation model (permitted):

* Implementations may compute grouping via single-pass hash aggregation, then continue the pipeline on the grouped rows.
  The exact strategy is not semantically observable except through the Unordered/Ordered rules above.

<!-- collections.joins -->
### 10.8 Joins

Syntax (example):

```kappa
[
    for c in customers
    join o in orders on o.customerId == c.id
    if c.region == "North" && o.amount > 100
    yield (c.name, o.id, o.amount)
]
```

* `join pat in collection on condition`:
  * iterates `collection`,
  * matches each element against `pat`,
  * and keeps pairings for which `condition` holds.

Left join example (using group join + flattening):

```kappa
[
    for c in customers
    left join o in orders on o.customerId == c.id into customerOrders
    yield (customer = c, orders = customerOrders)
]
```

* `left join pat in collection on condition into name`:
  * iterates `collection`,
  * matches each element against `pat`,
  * keeps the elements for which `condition` holds,
  * and binds `name` to the first-class query of matching elements for the current outer row.

Normative lowering:

* `join pat in xs on cond` is observationally equivalent to:

  ```kappa
  for tmp in xs
  let? pat = tmp
  if cond
  ```

  where `cond` is evaluated in the scope where the bindings introduced by `pat` are in scope.

* `left join pat in xs on cond into name` is observationally equivalent to introducing:

  ```kappa
  let name = Query [
      for tmp in xs
      let? pat = tmp
      if cond
      yield tmp
  ]
  ```

  The bindings introduced by `pat` are not in scope after the `left join`; only `name` is.

Type and scope rules:

* In `join`, the bindings introduced by `pat` are in scope in later clauses exactly as for other successful row
  extensions.
* In `left join ... into name`, `name` is in scope in later clauses and has type `Query t`, where `t` is the item type
  yielded by the normalized inner matching query.
* The encounter order and ordered/unordered status of `name` are those of the matching inner query produced for the
  current outer row.

Implementations MAY lower joins to dedicated normalized join operators or to observationally equivalent combinations of
normalized query operators, provided the semantics above are preserved.

<!-- collections.carriers -->
### 10.9 Carrier prefixes, first-class queries, and custom sinks

A comprehension form may optionally be prefixed by a carrier type expression:

```kappa
Array [ clauses..., yield valueExpr ]
Query [ clauses..., yield valueExpr ]
Tensor (n, m) { clauses..., yield keyExpr : valueExpr }
```

Carrier-prefix rule:

* The optional prefix is parsed as a type expression.
* Its free variables, if any, must already be in scope.
* After ordinary type inference, the comprehension has some fully applied result type `c : Type`.

Built-in defaults when the prefix is omitted:

* `[ ... ]` defaults to the built-in list collector.
* `{| ... |}` defaults to the built-in set collector.
* `{ ... }` defaults to the built-in map collector.

Carrier selection:

1. If an instance `FromComprehensionRaw c` is available, the compiler constructs a `RawComprehension item` for the
   comprehension, evaluates `fromComprehensionRaw` during elaboration, and elaborates the resulting `Syntax c` at the
   comprehension site.
2. Otherwise, if an instance `FromComprehensionPlan c` is available, the compiler constructs the normalized
   `ComprehensionPlan item` of §10.10, evaluates `fromComprehensionPlan` during elaboration, and elaborates the
   resulting `Syntax c` at the comprehension site.
3. If both are available, `FromComprehensionRaw` is preferred.
4. If neither is available, the comprehension is ill-formed.

The selected hook is an elaboration-time hook and is subject to the restrictions of §5.8.6.

The associated type `Item` of the selected sink instance determines the yielded item type of the normalized plan.

Examples:

* `Array [ ... ]` may use `FromComprehensionPlan (Array a)`.
* `Query [ ... ]` may use `FromComprehensionPlan (Query a)`.
* `Map k v { ... }` may use `FromComprehensionPlan (Map k v)` with `Item = (key : k, value : v)`.
* `Tensor (n, m) { ... }` is an illustrative example of a custom sink.
  This section does not by itself standardize dense tensor semantics, index domains, shape inference, or tensor-specific
  reduction behavior.

Raw custom sinks are intended for query providers, relational backends, and other advanced carriers that need access to
the original clause structure. Normalized sinks are intended for ordinary collection builders and backends that are
satisfied by the normalized plan.

<!-- collections.lowering -->
### 10.10 Normative lowering: sources, query core, and collection

Normative lowering proceeds in two stages:

1. clause lowering to a normalized query pipeline `Query row`,
2. terminal projection and collection via `ComprehensionPlan`.

<!-- collections.lowering.sources -->
#### 10.10.1 Sources

A generator source is any expression whose type `src` has an implicit instance `IntoQuery src`.

If `IntoQuery src` provides associated item type `a`, then:

```kappa
toQuery : src -> Query a
```

is the semantic source of rows for `for ... in ...` clauses.

Built-in source obligations:

* Implementations MUST provide standard `IntoQuery` instances for:
  * `List a`,
  * `Array a`,
  * the built-in set type,
  * map iteration as specified by §10.4,
  * the canonical range result type of §10.2,
  * and `Query a` itself.

* `IntoQuery (Query a)` MUST behave as identity on element streams.

* A conforming implementation MAY provide additional `IntoQuery` instances for implementation-defined sources, but those
  additional instances MUST NOT change the semantics of the required built-in instances.

<!-- collections.lowering.row_environment -->
#### 10.10.2 Row environment

Before `yield`, a comprehension is elaborated as a pipeline over a current row environment.

At any clause site, let `Γ` be the set of currently bound names in scope from preceding clauses.
The corresponding row type `Row(Γ)` is the canonical record type whose fields are those names and whose field order is
the canonical dependency-respecting record order of §14.6.

All clause semantics before `yield` are defined in terms of transformations on `Query (Row(Γ))`.

Consequences:

* `distinct` operates on the full current row environment at its clause site.
* `distinct by keyExpr` operates on keys computed from the current row environment.
* `order by`, `group by`, `join`, and filters likewise operate on the current row environment.

<!-- collections.lowering.initial_row_stream -->
#### 10.10.2A Initial row stream

A comprehension with no pre-yield clause sequence begins from a singleton empty-row stream.

Formally:

* before any non-yield clause is processed, the initial pipeline is `Query (Row(∅))`;
* `Row(∅)` is the zero-field closed record type; under §4.5 it is identified with `Unit`.

Consequences:

* `[ yield valueExpr ]` is well-formed and lowers from a singleton `Query Unit`;
* `{| yield valueExpr |}` is well-formed and lowers from a singleton `Query Unit`;
* `{ yield keyExpr : valueExpr }` is well-formed and lowers from a singleton `Query Unit`.

In these forms, `valueExpr`, `keyExpr`, and `valueExpr` are elaborated in the empty row environment.

<!-- collections.lowering.clause_lowering -->
#### 10.10.3 Clause lowering

The compiler MUST lower the clause sequence to behavior observationally equivalent to the following normalized query
algebra:

* source introduction from `IntoQuery.toQuery`,
* row extension,
* row-local mapping,
* flat-mapping,
* filtering,
* refutable filtering / filter-map,
* ordering,
* paging,
* distinct-by-key,
* join,
* left join,
* grouping.

The exact concrete representation of `Query` is implementation-defined, but a conforming implementation MUST provide a
stable public interface or data representation sufficient to realize these semantics and to support
`FromComprehensionPlan`.

Required clause behavior:

1. `for pat in src`

   * obtain `toQuery src`,
   * require `pat` to be irrefutable for the source item type,
   * extend the current row stream by binding `pat`.

2. `for? pat in src`

   * obtain `toQuery src`,
   * match each source item against `pat`,
   * keep only matching items,
   * reject the clause when dropping a non-matching item would violate the droppability rule of §10.4.1.

3. `let pat = expr`

   * evaluate `expr` once per incoming row,
   * require `pat` to be irrefutable,
   * extend the current row with the bound names.

4. `let? pat = expr`

   * evaluate `expr` once per incoming row,
   * keep only rows for which `pat` matches,
   * reject the clause when dropping the failure residue would violate the droppability rule of §10.4.1.

5. `if cond`

   * filter rows by `cond`,
   * evaluate `cond` at most once per incoming row,
   * typecheck subsequent clauses under the success assumption `cond = True`.

6. `if expr is C`

   * filter rows by the top-level constructor test,
   * evaluate `expr` at most once per incoming row,
   * typecheck subsequent clauses under the same positive constructor refinement as §7.4.1.

7. `join ...` and `left join ...`

   * lower to normalized join operators, or to observationally equivalent combinations of `toQuery`, refutable
     matching, and filtering,
   * preserving the semantics of §10.8.

8. `group by ...`

   * lower to a normalized grouping operator, or to an observationally equivalent implementation,
   * preserving the semantics of §10.7.

9. `distinct`

   * deduplicate on the current row record `Row(Γ)`,
   * preserving the first encountered representative.

10. `distinct by keyExpr`

    * evaluate `keyExpr` exactly once per row,
    * deduplicate by that key,
    * preserving the first encountered representative.

11. `order by`, `skip`, and `take`

    * preserve the ordered/unordered rules of §10.3.2 and §10.6.

<!-- collections.lowering.yield_terminal_plan -->
#### 10.10.4 Yield and terminal plan

After all non-yield clauses are lowered, the comprehension performs a final projection.

For list-like and set-like comprehensions:

* `yield valueExpr` produces a normalized item stream `Query a`.

For map-like comprehensions:

* `yield keyExpr : valueExpr` produces a normalized item stream
  `Query (key : k, value : v)`.

The final result of `lowerComprehension` is a `ComprehensionPlan item` carrying:

* the normalized item query, and
* terminal collection metadata:
  * list-like element collection,
  * set-like element collection,
  * or key/value collection together with any `on conflict` policy.

Map-conflict policy is terminal collection metadata, not part of the general `Query` algebra.

<!-- collections.lowering.collection_first_class_queries -->
#### 10.10.5 Collection and first-class queries

Collection is performed only after clause lowering.

* Built-in list, set, and map comprehensions use the corresponding built-in collectors.
* A prefixed carrier uses the selection rule of §10.9.

`Query [ ... ]` is the standard first-class query form for element-stream comprehensions.

Rules:

* `Query [ clauses..., yield valueExpr ]` is well-formed and returns the normalized `Query item`.
* `Query {| ... |}` is ill-formed in v1.
* `Query { ... }` is ill-formed in v1.
* No collector is permitted to silently discard terminal collection metadata.
* A carrier that wishes to represent set-like or key/value query semantics as first-class values MUST do so through its
  own explicit carrier type rather than by reusing `Query` with metadata loss.

<!-- collections.lowering.performance_if_rule -->
#### 10.10.6 Performance and as-if rule

The normative semantics are defined as if the compiler constructs `RawComprehension`, lowers it to
`ComprehensionPlan`, and then invokes the selected collector.

A conforming implementation need not materialize any of these intermediate values when doing so is unnecessary.

In particular, an implementation MAY:

* fuse source conversion, clause lowering, optimization, and collection,
* avoid materializing intermediate row records,
* avoid materializing an intermediate `Query` value when the normalized plan does not escape,
* lower directly to loops, iterators, relational algebra, SQL, or target-specific kernels,

provided the observable behavior is the same as the normative semantics above.

A first-class `Query` value need be materialized only when it escapes as a user-visible value.

Within a comprehension, implementations MUST preserve the following evaluation-count guarantees (as-if rules):

* `let` clause right-hand sides are evaluated at most once per incoming row.
* `if` filter conditions are evaluated at most once per incoming row.
* In `order by`, each ordering key expression is evaluated exactly once per row.
* In `distinct by`, the key expression is evaluated exactly once per row.
* In `group by`:
  * `keyExpr` is evaluated exactly once per incoming row.
  * each aggregate `valueExpr` is evaluated exactly once per incoming row per aggregate field.

<!-- data_types -->
## 11. Algebraic Data Types and Type Aliases

<!-- data_types.data_declarations -->
### 11.1 `data` declarations

General form:

```kappa
[public|private] [opaque] data Name (params...) : TypeK =
    Constructor1 arg1 arg2 ...
    Constructor2 ...
    ...
```

Examples:

```kappa
data Maybe (a : Type) : Type =
    Nothing
    Just a

data Result (e : Type) (a : Type) : Type =
    Ok a
    Err e
```

Constructors may declare named parameters. Two equivalent forms are supported:

```kappa
data User : Type =
    User (name : String) (age : Int)     -- recommended form

data Point : Type =
    Point { x : Int, y : Int }           -- record-style form (equivalent)
```

Grammar addition (extends the existing constructor declaration):

```text
constructorDecl ::= ident [ ctorBinder+ ]
ctorBinder      ::= ident
                  | '(' ident ':' type ')'
                  | '(' quantity ident ':' type ')'
                  | '(' 'thunk' ident ':' type ')'
                  | '(' quantity 'thunk' ident ':' type ')'
                  | '(' 'lazy' ident ':' type ')'
                  | '(' quantity 'lazy' ident ':' type ')'
                  | '(' '@' binder_body ')'
                  | '{' fieldDecl (',' fieldDecl)* '}'
fieldDecl       ::= [quantity] [ 'thunk' | 'lazy' ] ident ':' type
```

In a constructor declaration, the record-style binder form `{ f1 : T1, ..., fn : Tn }` is syntactic sugar for the
sequence of named binders `(f1 : T1) ... (fn : Tn)`. It does not introduce a single record-typed constructor argument.
The `binder_body` nonterminal is the same one used by implicit binders in §7.3, so constructor parameters may use the
ordinary quantity and implicit machinery, for example `(@0 p : P)`. Receiver-marked binders and `inout` are not
permitted in constructor declarations.

Constructor binders use the same suspension sugar as ordinary function binders and record fields:

* `(thunk x : A)` is sugar for `(x : Thunk A)`,
* `(lazy x : A)` is sugar for `(x : Need A)`.

No distinct constructor-level evaluation strategy exists in the core.

**Named parameters in GADT-style constructors.** When a constructor is declared using the full GADT form (`C : Π … →
R`), any explicit binders that appear in the declared signature (e.g. `(head : a)`, `(tail : Vec n a)`) are treated as
**named parameters** for the purposes of named-argument application and named patterns. Thus both of the following are
valid and equivalent:

```kappa
VCons { head = h, tail = t }
VCons h t
```

The same applies to patterns:

```kappa
case VCons { head = h, tail = t } -> ...
```

The names are taken directly from the binders written in the GADT signature; no separate `(name : Type)` form is
required in the `data` header. This unifies the named-argument sugar across both simple and GADT constructor
declarations.

Rules:
* All parameters of a single constructor must use the same style (all positional/named-parenthesized or all
  record-style). Mixing styles inside one constructor is a compile-time error.
* Parameter names are purely for application sugar (§11.1.1). They do not affect runtime representation, pattern
  matching, or the constructor's type.
* Positional application (`User "Bob" 33`) remains valid for every constructor.
* Named application (`User { name = "Bob", age = 33 }`) becomes available precisely when the constructor was declared
  with named parameters.
* The compiler SHOULD emit a soft diagnostic (warning or hint) when a constructor is declared without parameter names,
  encouraging the named form for improved ergonomics.

* Constructors contribute `ctor` declarations to their binding groups and
  are also static members of their enclosing data type under §2.8.2-§2.8.3.
* Parameters contribute declaration binders.
* Annotated parameters contribute exactly the written binder.
* Unannotated parameters are interpreted by §5.3.3.
  In particular, `data Maybe a : Type = ...` elaborates with `a : Type`, while a declaration may infer a more specific
  annotation for an unannotated lowercase parameter from use.
* If `opaque` is present on a data declaration, constructors are not exported (§2.5.3).

<!-- data_types.data_declarations.constructor_application_named_arguments_c -->
#### 11.1.1 Constructor application with named arguments (`C { ... }`)

Constructors may be applied using a record-like named-argument syntax when their explicit parameters have names
(declared via either of the forms in §11.1).

```kappa
User { name = "Bob", age = 33 }
User { name, age }                    -- punning
Vec.VCons { head = h, tail = t }
```

Rules:
* `C { ... }` is valid only if `C` resolves to a constructor whose explicit argument binders have names.
* Each field label in `{ ... }` must correspond to a distinct explicit constructor argument name.
* Field punning is permitted: a field label written without `= expr` is treated as `label = label`.
* In such a punned field, `label` must resolve to a term in scope whose type matches the corresponding constructor
  field.
* Duplicate labels are an error.
* Missing required labels are an error.
* Extra labels not present in the constructor's argument list are an error.
* Field order is not semantically significant. Implementations elaborate the application in the constructor's argument
  order (or any dependency-respecting order if later arguments depend on earlier ones).
* Expected-type-directed suspension insertion applies to constructor arguments exactly as in ordinary application:
  * if a constructor parameter type is `Thunk T` or `Need T`, a supplied argument that already checks against the
    demanded suspension type without inserting a new suspension at that same argument position is used unchanged;
  * otherwise, if the supplied argument checks against `T`, it is elaborated as `thunk expr` or `lazy expr`
    respectively.

Elaboration sketch:

If `C` has explicit parameters `(f1 : A1) -> (f2 : A2[f1]) -> ... -> R`, then:

```
C { f1 = e1, f2 = e2, ... }
```

elaborates to:

```
C e1 e2 ...
```

with implicit arguments resolved as usual (§7.3).

<!-- data_types.data_declarations.naming_diagnostics -->
#### 11.1.1A Naming diagnostics for lowercase data and constructor names

Capitalization is not semantically significant for `data` declarations. However, lowercase-initial identifiers are used
by convention for variable-like names and by §5.3.3 for implicit declaration-level universalization. Therefore:

* Implementations MUST emit a warning for each user-written `data` type name or constructor name whose spelling, after
  removing surrounding backticks if any, begins with an ASCII lowercase letter.
* The warning applies only to non-operator names.
* The warning applies separately to the declared data type name and to each constructor name.
* This diagnostic class is warning, not error.
* The diagnostic SHOULD suggest capitalizing the first character when that transformation produces a valid identifier
  spelling.

<!-- data_types.gadts -->
### 11.2 GADT-style constructors

GADT-style constructors are written with an explicit result type after `:`:

```kappa
data Vec (n : Nat) (a : Type) : Type =
    VNil  : Vec 0 a
    VCons : (head : a) -> (tail : Vec n a) -> Vec (n + 1) a
```

They support the full range of constructor features, including the named-argument sugar described in §11.1 (the binders
in the Pi signature become the named parameters). Exhaustiveness checking and pattern matching treat GADT constructors
identically to ordinary constructors once indices are unified.

Free lowercase identifiers in a GADT-style constructor signature that are not already bound by the data header or by
explicit constructor binders are implicitly universalized per §5.3.3.

Note: Indexed data together with quantity-`1` arguments are sufficient to encode typestate and session-style protocols
as ordinary libraries. A state transition consumes a value at one index and produces a value at a new index;
non-transition operations may instead be expressed over borrowed parameters (`(& x : T state)`), preserving the current
index without consuming the underlying resource.

<!-- data_types.type_aliases -->
### 11.3 Type aliases

Type aliases use `type`:

```kappa
type Foo (x : Int) : Type = Int
type Id (a : Type) = a
opaque type Id (a : Type) = a
private type Internal = ...
```

Unified declaration principle:

* The `type` keyword is declaration-kind-directed surface sugar over the ordinary named declaration / definition forms.
* In particular, `type T` is sugar for the abstract declaration `T : Type0`.
* Likewise, `type T = RHS` is sugar for a definition of `T` at a universe type, with `RHS` parsed in type position.
* This does not introduce a second declaration mechanism; it is only specialized surface syntax for `type` declarations.

* Parameters may be annotated.
* Unannotated parameters are interpreted by §5.3.3.
  In particular, `type Id a = a` elaborates as `type Id (a : Type) = a`.
* `type Name ...` with no `= ...` defines an abstract type whose implementation may be provided elsewhere
  (implementation-defined).
* If `opaque` is present on a type alias, the right-hand side is not unfolded by definitional equality outside the
  defining module (§2.5.2).

Type aliases vs type-level definitions:

Because Kappa is dependently typed, types are terms.

* `type T ... = RHS` is surface syntax for a type-level definition whose right-hand side is parsed in a type context.
* An equivalent form is a term definition with a universe type, e.g. `let T : Type = RHS`, when `RHS` is intended to be
  a type expression. Implementations may treat these as equivalent after elaboration.

<!-- data_types.well_formedness -->
### 11.4 Well-formedness of `data` declarations

Every `data` declaration MUST satisfy strict positivity.

Definitions:

* Every previously accepted `data` type, and every built-in type constructor treated as strictly positive, carries a
  **parameter-positivity signature** marking each parameter as either positive or non-positive for later
  strict-positivity checking.

* Positivity checking unfolds all transparent local type aliases before reasoning about occurrences. Imported
  transparent aliases are unfolded as well when their bodies are available in the imported interface. If an imported
  transparent alias's body is not available, it is not admissible for positivity reasoning.

* A type former `F` is **admissible** in a strictly positive position iff `F` is one of:
  * a previously defined `data` type that has been accepted as strictly positive and whose parameter-positivity
    signature is known,
  * a built-in type constructor declared as strictly positive together with its parameter-positivity signature (such as
    `List`, `Option`, or `Array`), or
  * an imported opaque or otherwise abstract type constructor whose interface explicitly carries a parameter-positivity
    signature, or
  * a function type whose domain does not mention the recursive type being checked.

* An imported opaque or abstract type constructor whose interface does not carry a parameter-positivity signature is not
  admissible for positivity reasoning. Such a constructor may still be used in ordinary types, but it MUST NOT be
  treated as preserving positivity during strict-positivity checking.

* An occurrence of the type being defined `T` is in a **strictly positive** position in a constructor argument type if:
  * it is the argument type itself (possibly with parameters/indices), or
  * the argument type is `X -> U` where `T` does not occur in `X` and `T` occurs strictly positively in `U`, or
  * the argument type is a dependent function `(x : X) -> U` with the same condition and `T` does not occur in `X`, or
  * the argument type is an application `F A1 ... An` where `F` is admissible in a strictly positive position and `T`
    occurs strictly positively only in those `Ai` whose corresponding parameter of `F` is marked positive in `F`'s
    parameter-positivity signature.

* A `data` declaration is **strictly positive** iff every occurrence of the defined type constructor in every
  constructor's argument types is in a strictly positive position.

* When a `data` declaration `data F p1 ... pn = ...` is accepted, implementations MUST record a parameter-positivity
  signature for `F`. Parameter `pi` is marked positive only if every occurrence of `pi` in the constructor argument
  types of `F` is itself in a strictly positive position. Later strict-positivity checks MUST consult this recorded
  signature rather than treating all arguments of `F` as positive.

* For a mutually recursive group of `data` declarations, implementations MUST compute the parameter-positivity
  signatures of the whole group simultaneously by fixed-point iteration over the group:
  * initialize every parameter of every type in the group as non-positive;
  * repeatedly recompute the signatures using the current signatures of the whole group until a fixed point is reached;
  * after convergence, reject the group if any type in the group still has a non-strictly-positive occurrence in any
    constructor argument type.

Implementations MUST reject non-strictly-positive `data` declarations.

Examples rejected:

```kappa
data Bad = MkBad (Bad -> Bad)           -- Bad in negative position
data Rose a = Node a ((Rose a -> a) -> Rose a)   -- negative occurrence
```

Examples accepted:

```kappa
data Tree a = Leaf | Branch (Tree a) a (Tree a)
data Rose a = Node a (List (Rose a))
```

---

<!-- traits -->
## 12. Traits (Typeclasses)

Traits describe typeclasses / interfaces.

<!-- traits.headers -->
### 12.1 Trait headers

Trait declarations use Haskell/Idris-style headers, optionally with a supertrait context:

```kappa
trait Eq a =
    ...

trait Eq a => Ord a =
    compare : a -> a -> Ordering

trait Applicative f =
    ...

trait Applicative f => Monad f =
    (>>=) : f a -> (a -> f b) -> f b

trait Equiv (a : Type) =
    (~=) : a -> a -> Bool

    equivRefl :
        (x : a) -> ((x ~= x) = True)

    equivSym :
        (x : a) -> (y : a) ->
        ((x ~= y) = True -> (y ~= x) = True)

    equivTrans :
        (x : a) -> (y : a) -> (z : a) ->
        ((x ~= y) = True -> (y ~= z) = True -> (x ~= z) = True)

trait Eq (a : Type) =
    (==) : a -> a -> Bool
    eqSound    : (x : a) -> (y : a) -> ((x == y) = True  -> x = y)
    eqComplete : (x : a) -> (y : a) -> (x = y -> (x == y) = True)

trait Functor (f : Type -> Type) =
    ...

trait Applicative (f : Type -> Type) =
    pure    : a -> f a
    liftA2  : (a -> b -> c) -> f a -> f b -> f c

    let (<*>) : f (a -> b) -> f a -> f b =
        \ff fa -> liftA2 (\f x -> f x) ff fa

trait Applicative (m : Type -> Type) => Monad (m : Type -> Type) =
    ...
```

Fully applied trait applications have type `Constraint`, not `Type` (§5.1.3).

`Eq` is reflective decidable equality. An `Eq` instance participates in equality reflection only because `eqSound` and
`eqComplete` are required members whose proof terms must typecheck. No additional restriction on hand-written instances
is required beyond those required members.

`Equiv` is a non-reflecting boolean equivalence relation. The compiler MUST NOT derive `x = y` from `(x ~= y) = True`;
only proof-carrying `Eq` participates in equality reflection (§7.3.3).

In `std.prelude`, `Ord` refines `Eq`, not `Equiv`: any `Ord a` instance MUST also supply an `Eq a` instance for the same
`a`, and its equality classes MUST agree with `(==)`.

Sugar:

```kappa
trait Eq a = ...           -- Eq (a : Type)
trait Eq a => Ord a = ...
trait Marker a b = ...     -- both default to Type
```

Trait header parameters follow §5.3.3. In particular, an unannotated parameter defaults to `Type` when otherwise
unconstrained and may be inferred at a more specific annotation from use.

In `std.prelude`, `Ord` is declared with supertrait `Eq`, not `Equiv`, and its equality classes MUST agree with `(==)`.

In `std.prelude`, `Monad` is declared with supertrait `Applicative`.

<!-- traits.headers.supertraits -->
#### 12.1.1 Supertraits

A trait declaration may have a supertrait context:

```text
trait C1, ..., Cn => Tr args = ...
```

Meaning:

* every dictionary for `Tr args` contains coherent evidence for `C1` through `Cn`,
* supertrait evidence is available inside the trait body,
* inside local implicit resolution, an in-scope dictionary for `Tr args` may be projected to any of its declared
  supertraits.

Supertrait projection affects local implicit resolution only. It does not introduce additional global instance
candidates. Global instance search remains based only on directly declared instances.

Example:

```kappa
trait Functor f =
    map : (a -> b) -> f a -> f b
```

<!-- traits.members -->
### 12.2 Trait members

Inside a `trait`:

* **Required member (no default):**

  ```kappa
  equals : a -> a -> Bool
  ```

* **Member with default implementation:**

  ```kappa
  let notEquals : a -> a -> Bool =
      \x y -> not (equals x y)
  ```

Members in traits share the same two forms as ordinary bindings:

* Declaration: `name : Type` (required).
* Definition: `let name : Type = expr` (default).

In addition, trait bodies may declare associated static members as specified in §12.2.1.

Free lowercase identifiers in trait member signatures are implicitly universalized per §5.3.3. Thus declarations such
as `map : (a -> b) -> f a -> f b` and `empty : f a` are well-formed without explicit `forall`.

<!-- traits.members.overloaded_member_names -->
#### 12.2.1 Overloaded member names

For each term member `m : τ` declared in `trait Tr params`, let `u1 ... un` be the binders implicitly universalized
from `τ` by §5.3.3, with inferred annotations `U1 ... Un`. Elaboration introduces an overloaded term name of the
shape:

```kappa
m : forall params. forall (u1 : U1) ... (un : Un). Tr params => τ
```

If no such binders are introduced, this reduces to the simpler `forall params. Tr params => τ` shape.

A bare occurrence of `m` elaborates to projection from synthesized implicit evidence for `Tr params`.

An explicit dictionary projection such as `d.m` or `d.(op)` is the unsugared form.

Associated static members:

* Trait bodies use the same unified declaration syntax as the rest of the language.
* A declaration of the form

  ```kappa
  [opaque] Name : S
  ```

  inside a trait body declares:

* a term member when `S` elaborates to an ordinary value type; or
* an associated static member when `S` elaborates to a compile-time type in the sense of §5.1.4.1.
* There is no trait-specific declaration form for associated static members.
  `opaque` has its ordinary meaning here.
* Associated static members do not induce term-level overloaded names.
* Associated static members are projected from an explicit dictionary binder or value using ordinary member selection.
  For example, from:

  ```kappa
  trait Iterator (it : Type) =
      Item : Type
      next : (1 this : it) -> Option (item : this.Item, rest : it)
  ```

  one may write:

  ```kappa
  foo : (@ c : Type) -> (@ It : Iterator c) -> It.Item
  ```

* Associated static members may have any compile-time type of §5.1.4.1, including `Type`, `Type u`, `Universe`,
  `Quantity`, `Region`, `RecRow`, `VarRow`, `EffRow`, `Label`, `EffLabel`, and compile-time function spaces built from
  them.

* There is no separate trait-scoped projection mechanism of the form `Trait.Name args`; ordinary projection from an
  explicit or implicit dictionary remains the core mechanism.

Explicit dictionaries:

For a dictionary `d : Dict (Tr args)`:

* term members are projected as `d.name` or `d.(op)`,
* associated static members are projected as `d.Name` in any syntactic position compatible with their compile-time type.

<!-- traits.instances -->
### 12.3 Instances

Instances provide implementations of traits for specific types.

Syntax (Haskell-style):

```kappa
instance Eq a => Eq (Option a) =
    (==) : Option a -> Option a -> Bool
    let (==) this that =
        match (this, that)
        case (Option.None, Option.None) -> True
        case (Option.Some x, Option.Some y) -> x == y
        case _ -> False
        
trait Iterator (it : Type) =
    Item : Type
    next : (1 this : it) -> Option (item : Item, rest : it)

instance Iterator (List a) =
    let Item = a
    let next this = ...
```

Free lowercase identifiers in instance premises and the instance head are implicitly universalized per §5.3.3. Thus
`instance Eq a => Eq (Option a) = ...` is well-formed without an explicit outer binder for `a`.

`std.prelude` also provides the canonical bridge from reflective equality to non-reflecting equivalence:

```kappa
instance Eq a => Equiv a =
    let (~=) = (==)

    let equivRefl x =
        Eq.eqComplete x x refl

    let equivSym x y p =
        Eq.eqComplete y x (sym (Eq.eqSound x y p))

    let equivTrans x y z p q =
        Eq.eqComplete x z (trans (Eq.eqSound x y p) (Eq.eqSound y z q))
```

Here `refl` denotes the canonical reflexivity proof of `=`, and `sym` and `trans` are the standard
propositional-equality combinators definable from `=`.

An instance declaration elaborates to a dictionary value of type `Dict (Tr args)`, where `Tr args` is the instance head
constraint. It also contributes coherent implicit evidence for the constraint `Tr args`. This elaborated dictionary is
not an ordinary exported top-level item; it participates in instance search according to the rules below.

Rules:

* `public` and `private` do not apply to instance declarations. An instance is not part of the module's ordinary export
  interface.
* `export` does not re-export instances as ordinary items.
* Top-level instance declarations from any module in the compilation unit's module closure contribute to instance
  resolution. This remains true whether or not the defining module exports ordinary names, whether or not another module
  re-exports it, and whether or not any lexical import brings its names into scope.
* Instance declarations are top-level by default.
* Local instances are permitted only in block scopes under the rules of §6.3.1 and §14.1.1. In particular, a local
  instance MUST mention at least one trait or type declared in the same block scope or an enclosing local block scope,
  participates in implicit resolution only within that lexical scope, and does not become part of the global instance
  environment outside it. An explicit dictionary value produced from such an instance may still escape as an ordinary
  term.
* Instance coherence is defined exclusively by Section 15.2.1. Overlap is harmless only when that section deems the
  candidate implementations equivalent; otherwise the program is rejected.
* Orphan instances are permitted. An instance is an orphan when neither the trait definition nor the type constructor
  being instantiated is defined in the same module as the instance declaration.
* Implementations SHOULD emit a warning for each orphan instance declaration.
* For a URL-imported module, the same-module test above is determined by the module identity rules of §2.3.2. In
  particular, for both `sha256:` and `ref:` pins, the relevant identity is the base URL together with the resolved
  immutable digest of the imported content. Two URL modules with different such identities are distinct modules even if
  they share the same base URL.

Instance environment:

Instance visibility is global over the compilation unit's module closure.

Ordinary imports affect name resolution only. Ordinary imports do not enable, disable, or otherwise influence instance
search.

If a module is in the compilation unit's module closure, all of its top-level instances participate in instance search,
regardless of whether any names from that module are imported.

This is a global program property, not an import-side effect. Adding a dependency may introduce new instance candidates
and therefore new ambiguity or coherence failures. This is by design.

<!-- traits.instances.resolution_algorithm -->
#### 12.3.1 Instance resolution algorithm

When solving a trait constraint goal `Tr args`, instance resolution proceeds as follows:

1. **Local implicit context first.** For goals of trait-constraint form `Tr args`, perform step 1 of the
   implicit-resolution procedure of §7.3.3. If that step yields a unique local candidate, use it. If that step yields
   ambiguity, compilation fails. Only if that step yields no local candidate does instance resolution continue to step 2
   below.

2. **Then global instances** Collect all top-level instance declarations in the compilation unit's module closure whose
   instance heads unify with the goal.

3. **Recursive premise solving** For each collected candidate, solve its premises recursively using this same algorithm.

4. **Discard failing candidates** Any candidate whose premises cannot be solved is discarded.

5. **Coherence check on surviving candidates**
   * If zero candidates survive, the implicit goal is unsolved.
   * If one candidate survives, use it.
   * If multiple candidates survive, compare their instantiated candidate dictionaries according to §15.2.1:
     * compare Easy Hashes first,
     * if needed compare Hard Hashes,
     * if all surviving instantiated candidate dictionaries have the same canonical Hard Hash, accept and pick any one,
     * otherwise reject the program as incoherent.

<!-- traits.instances.search_termination -->
#### 12.3.2 Instance search termination

An instance declaration is accepted only if each of its premises is structurally smaller than its head under a
well-founded metric on constructor depth and variable occurrence.

A conforming implementation may use any check at least as strict as the following Paterson-style condition:

* no premise may be identical to the head;
* for every type variable, the number of occurrences of that variable in a premise must not exceed its number of
  occurrences in the head;
* the total constructor-and-variable size of each premise must be strictly smaller than that of the head.

Implementations MAY use a stricter termination check.

<!-- traits.proof_irrelevance -->
### 12.4 Proof irrelevance

Kappa does not provide a user-space `Prop` trait or a separate proposition universe in v0.1.

Rules:
* Ordinary proof types remain ordinary inhabitants of `Type`.
* For ordinary proofs, proof irrelevance is expressed by quantity `0`: a proof bound as `(@0 p : P)` or `(0 p : P)` is
  computationally irrelevant and erased according to Sections 5.1.5 and 14.4.
* Ordinary terms of type `P : Type` are **not** treated as definitionally equal merely because they witness the same
  proposition. Definitional equality remains the core conversion relation of Section 14.3 and does not consult trait
  resolution or any user-space proof-irrelevance marker.
* Constraint evidence for trait applications `Tr args : Constraint` is separately proof-irrelevant for typechecking and
  coherence by the rules of Section 5.1.3. This is a property of the `Constraint` sort, not of an ordinary trait over
  `Type`.

Examples:

```kappa
transport :
    (@0 p : x = y) -> Vec x a -> Vec y a

assumePositive :
    (n : Nat) -> (@0 pf : n > 0) -> SafeNat n
```

<!-- traits.deriving -->
### 12.5 Deriving

Deriving generates instances automatically for eligible declarations.

Minimal form:

```kappa
derive Eq Foo
```

Deriving is implementation-defined in v0.1, but v1.0 should at least support deriving `Eq` and `Show`-like traits for
standard algebraic data types.

For `Eq` specifically:

* `derive Eq T` is permitted only if the compiler can synthesize both `eqSound` and `eqComplete` for `T`.
* If either proof cannot be synthesized, deriving `Eq` fails.
* If `T` is declared as `opaque data`, any deriving algorithm that inspects the constructors or fields of `T` MUST do so
  only in the defining module of `T`, where that representation is available. The resulting derived dictionary is still
  an instance of the abstract type `T`; outside the defining module it may be used only through the trait interface and
  does not grant constructor access, pattern-matching access, or additional definitional transparency for `T`.
* A `derive` declaration that appears in a block scope generates a local instance. Its visibility, closure-conversion
  behavior, and admissibility rules are exactly those of a handwritten local instance under §14.1.1.

---

<!-- names_cross_reference -->
## 13. Cross-reference: names, binding groups, and dotted lookup

The normative rules for names, binding groups, dotted lookup, method-call / receiver-projection sugar, and reified module values are specified in §2.8. This chapter remains only as a continuity pointer for older citations.

<!-- core_semantics -->
## 14. Core Semantics

This chapter defines evaluation, elaboration, definitional equality, and erasure at a level sufficient to make the
language's typechecking and runtime behavior unambiguous.

<!-- core_semantics.elaboration -->
### 14.1 Elaboration overview

Kappa source code is first translated into KFrontIR, the source-preserving frontend IR of §17.2, and is then elaborated
to KCore, the fully resolved core language of §17.3.

Elaboration performs (non-exhaustive):

* layout processing (`INDENT`/`DEDENT`) and parsing using fixities in scope,
* construction and phase resolution of KFrontIR (§17.2),
* name resolution across lexical binding groups, declaration kinds,
  same-spelling data families, and nearest-successful receiver lookup,
* implicit argument insertion (§7.3),
* construction of maximal application sites and their alignment with resolved Pi telescopes (§7.1.3, §17.3.1),
* quantity checking and borrow checking under §§5.1.5-5.1.7 using the syntax-directed ownership rules of the core
  language,
* generation of any predicates required by an enabled modal/coeffect extension of §5.1.5.2, followed by
  modality-specific solving,
* insertion of coercions required by:
    * expected-type-directed variant injections and widenings (§5.4.3),
    * lawful record reorderings (§5.5.1.1),
* signature matching, manifest recording, and opaqueness checking for `seal ... as ...`, and construction of reified
  module values (§5.5.10, §2.8.5),
* desugaring of:
    * pure `block` expressions and indented pure block suites to sequential local scope with closure-converted local
      declarations (§6.3.1, §14.1.1),
    * `do` blocks, abrupt control, and exit-action scheduling to the structured completion-and-scope kernel (§8.2,
      §8.7, §17.3.1.4),
    * shallow handlers to the structured effect-operation and handler kernel (§8.1.9, §14.8, §17.3.1.5),
    * deep handlers to recursive drivers over the shallow-handler kernel (§8.1.10, §14.8, §17.3.1.5),
    * lowering of boolean and constructor-refinement control flow to explicit erased branch evidence (§7.4.1, §7.5.4,
      §17.3.1.8),
    * comprehensions to combinator pipelines (§10.10),
    * existential-package sugar and unpacking (`exists`, `open ... as exists ...`) to anonymous sealed packages and
      ordinary local bindings (§5.5.11),
    * `try match` and `try` to error-handling combinators (§9),
    * method-call sugar (§2.8.4),
* termination checking (§6.4),
* optional erasure planning (§14.4).

Elaboration must be terminating.

<!-- core_semantics.elaboration.local_declarations -->
#### 14.1.1 Elaboration of local declarations

A local declaration inside a block scope (`block` or `do`) elaborates by closure conversion over the free variables it
captures from the surrounding lexical scope.

Closure conversion:

* Let `Gamma` be the free variables of the local declaration drawn from the surrounding lexical environment.
* Elaboration introduces an internal declaration abstracted over `Gamma`.
* Within the user-written block, a bare occurrence of the local name is shorthand for that internal declaration applied
  to the current values or evidence of `Gamma`.
* This applies uniformly to local `data`, `type`, `trait`, `scoped effect`, `instance`, local signatures and named
  `let` definitions, and instances generated by `derive`.

Scoped local declarations:

* Ordinary local `data`, `type`, and `trait` declarations are closure-converted over the ordinary free-variable capture
  set `Gamma` and retain the escape behavior described below.
* A local declaration marked `scoped` is closure-converted over `(σ, Gamma)`, where `σ` is the hidden nominal-scope
  token of the nearest enclosing block scope as specified by §§6.3.1.1 and 17.3.1.5A.
* A `scoped effect` declaration likewise closure-converts both the local effect-interface constructor and its canonical
  self label over `(σ, Gamma)`.

Lexical identity:

* A local nominal declaration has one identity per declaration site, not per dynamic evaluation of the enclosing
  function or block.
* Distinct dynamic evaluations instantiate the same local declaration family at different captured arguments.
* Implementations MUST NOT treat local `data`, `trait`, or `scoped effect` declarations as runtime-fresh generative
  names.

Escaped local nominal families:

* Closure conversion of a local nominal declaration defines a local nominal family abstracted over the captured
  arguments Γ of that declaration.
* The family has one canonical family identity per declaration site, not per dynamic evaluation.
* Distinct dynamic evaluations instantiate the same local nominal family at different captured arguments.
* An occurrence of a local nominal outside its defining block is represented semantically as an application of that
  family identity to the corresponding captured arguments chosen by closure conversion.
* Two such occurrences denote the same escaped local nominal application iff:
  * they refer to the same local nominal family identity; and
  * their corresponding captured arguments are definitionally equal.
* If an escaping type, compile-time term, transparent dependent package, or sealed package mentions a local nominal
  family, that family becomes part of the exported semantic meaning and is subject to the interface-artifact and
  semantic-identity rules of §§17.1.9 and 17.3.4.1.
* A local nominal family may appear in an exported interface only if its captured arguments are representable in the
  interface artifact. Otherwise export is a compile-time error in the defining module.

Interaction with continuations and handlers:

* Capturing a local declaration inside a shallow or deep resumption is closure capture of the same semantic kind as
  capturing it inside an ordinary closure.
* Multi-shot resumption duplicates captured control state, not local nominal family identity.
* Therefore reusing a continuation MUST NOT create fresh local nominal families; the family identity remains the
  declaration-site identity defined above.
* Any escaped occurrence of a local nominal family arising through a resumed computation is represented as an
  application of that family identity to its closure-converted captured arguments, exactly as for any other escaping
  local nominal family.
* Interface representability and exported semantic identity of such escaped local nominal families remain governed by
  the ordinary rules of this subsection together with §§14.8, 17.1.9, and 17.3.4.1.

Escaping results:

* No separate syntactic prohibition prevents a local type, trait, or alias from appearing in a value that leaves the
  block.
* A value escapes exactly when the elaborated term remains well-typed after closure conversion.
* In particular, local type constructors may escape through `Type`-valued results, transparent dependent packages, and
  sealed packages of §5.5.10, provided:
  * the resulting elaborated interface type is well-typed; and
  * every escaped local nominal family mentioned by that interface is representable under §§17.1.9 and 17.3.4.1.

Scoped escape restriction:

* Because nominal-scope tokens are not interface-representable (§17.3.4.1), a transparent escape of a scoped local
  family, or of any elaborated type or compile-time term that still mentions it after ordinary transparent expansion, is
  a compile-time error in the defining block.
* Opaque escape is permitted only when the exposed interface omits the scoped local family and its captured
  nominal-scope token.

Local instances:

* A local instance participates in implicit resolution only within the lexical scope of the block where it is declared,
  together with any nested block scopes inside it.
* A local instance never becomes part of the global top-level instance environment.
* A local instance declaration is permitted only if its instance head mentions at least one trait or type declared in
  the same block scope or an enclosing local block scope.
* An explicit dictionary value may escape as an ordinary term, but the originating local instance does not contribute to
  implicit resolution outside its lexical scope.

Sequential scope:

* Local declarations apply from the declaration onward within the enclosing block scope.
* If a local declaration depends on an earlier local declaration, it captures the closure-converted form of that earlier
  declaration just as it would capture any other free variable.

<!-- core_semantics.evaluation -->
### 14.2 Dynamic semantics (runtime evaluation)

Runtime evaluation is strict call-by-value with left-to-right evaluation order, except where evaluation is explicitly
delayed by the suspension forms of §7.2.2.

Evaluation order:

* Function application evaluates the function expression, then the argument expression, then applies.
* Record/tuple literals evaluate fields/elements left-to-right.
* Record update evaluates the scrutinee record, then evaluates updated field expressions left-to-right.
* `seal e as S` evaluates `e` and constructs a sealed package value whose runtime representation contains exactly the
  non-erased members required by `S`, or an observationally equivalent implementation-defined representation.
* Projection of a non-erased member from a sealed package evaluates by selecting that stored runtime member.
* Quantity-`0` members of a sealed package participate only at compile time unless explicitly reified by library code or
  backend intrinsics.
* `thunk e` and `lazy e` do not evaluate `e` at construction time; `force e` evaluates according to the suspension
  rules of §7.2.2.
* `if` evaluates the condition, then evaluates exactly one branch.
* `match` evaluates the scrutinee once, then tests cases top-to-bottom; guards are evaluated only after the pattern
  matches.

`do` blocks:

* `do` blocks are elaboration-time sugar to monadic operations (`pure`, `bind`, sequencing).
* Evaluation order follows the desugared monadic structure.

<!-- core_semantics.definitional_equality -->
### 14.3 Definitional equality

Definitional equality (also called conversion) is the equality relation used by the typechecker.

Definitional equality is the smallest congruence containing the following reductions (subject to opacity rules):

* β-reduction: `(\(x : A) -> e) v  ↦  e[x := v]`
* δ-reduction: unfolding of transparent **termination-certified** definitions and transparent type aliases whose bodies
  are available and not marked opaque at the use site. Definitions accepted solely via `assertTotal` (§16.4) do not
  unfold for definitional equality unless the implementation has separately verified them and recorded them as
  termination-certified. Outside the defining module, opaque definitions and opaque type aliases do not unfold unless
  the importing module has explicitly clarified them (§2.5.2).
* ι-reduction: reducing `match` on known constructors/literals.
* η-equality (definitional):
    * Functions: `f ≡ (\(x : A) -> f x)` when `x` is not free in `f`.
    * Records: a record is definitionally equal to a record reconstructed from its projections (field-wise η), up to
      lawful reorderings.
    * The zero-field closed record is definitionally equal to `Unit`.
* capture annotations: `T captures (s̄1)` and `U captures (s̄2)` are definitionally equal iff `T ≡ U` and the canonical
  capture sets are equal.

Capture-set canonicalization removes duplicates and orders region variables by first binding occurrence in the
surrounding explicit `Region` telescope.

Quantities in definitional equality:

Using meta-notation `([q] x : A) -> B` for a binder of quantity `q`, two binder types `([q1] x :A1) -> B1` and `([q2] x
:A2) -> B2` are definitionally equal iff `q1 = q2`, `A1 ≡ A2`, and `B1 ≡ B2`.

Quantities are part of a function type's identity.

Definitional equality is strictly invariant with respect to quantities.

In particular, eta-expansion must preserve binder quantities; quantity-mismatched eta-expansions are not definitionally
equal.

Definitional equality additionally includes suspension reduction:

* `force (thunk e)  ↦  e`
* `force (lazy e)   ↦  e`

Equivalently, after surface desugaring, KCore normalizes `Force (Delay e)` and `Force (Memo e)` to `e`.

Because `(thunk x : A)` and `(lazy x : A)` are only surface sugar for `(x : Thunk A)` and `(x : Need A)`, they do not
introduce distinct core binder identities.

The quantity subsumption of §7.1.3 is implemented by elaboration-time insertion of an eta-expansion (or equivalent
coercion term). It is not part of definitional equality and does NOT alter the underlying `≡` relation used by the core
dependent typechecker.

Modal/coeffect evidence and definitional equality:

* Solved evidence introduced by `MODAL_SOLVE` is not part of the definitional equality of ordinary terms or types.
* Such evidence is compile-time only and proof-irrelevant unless the corresponding modal extension explicitly introduces
  a user-visible runtime carrier.
* A modal extension may affect definitional equality only through its own explicit type or computation formers; it MUST
  NOT change the definitional equality of `Quantity`, ordinary arrows, records, effect rows, or ordinary terms that do
  not use that extension.
* Unsolved modality predicates are never reified as neutral terms participating in `≡`.

Normalization used by the typechecker may be fuel-bounded for performance, but must be sound: it must not claim
definitional equality unless it holds in the mathematical relation above.

Definitional equality also includes canonical normalization of:

* optional-type sugar: `T?` normalizes to `Option T` for all `T`,
* union / variant rows: normalize member types to a canonical order with duplicate removal for elaboration and
  definitional equality; runtime tagging uses stable member-type identities instead of row-local ordinals (§5.4, §14.5),
* record types/values: normalize to a canonical dependency-respecting field order (§5.5.1.1, §14.6),
* existential-package sugar: normalize surface `exists (a1 : S1) ... (an : Sn). T` to the canonical
  anonymous-package signature form of §5.5.11, using implementation-internal witness-member labels and the fixed
  payload label `value`;
* seal congruence: if `e1 ≡ e2`, then `seal e1 as S ≡ seal e2 as S`;
* seal idempotence: if `e : S`, then `seal e as S ≡ e`;
* projection congruence: if `p1 ≡ p2`, then `p1.f ≡ p2.f` whenever both projections are well-typed;
* transparent static-member unfolding: for a transparent compile-time field `f` of a signature `S`, the projection
  `(seal e as S).f` δ-reduces to the manifest equation recorded for `f` by the seal;
* opaque-member opacity: if `f` is opaque in `S`, the projection `(seal e as S).f` does not δ-reduce across the seal;
* projection of a non-erased field through a seal is not required to reduce for definitional equality. It remains a
  well-typed neutral elimination form whose runtime behavior is ordinary projection.

<!-- core_semantics.definitional_equality.literal_normalization -->
#### 14.3.1 Literal normalization

For any type `T` with a `FromInteger T` instance whose `fromInteger` is a total transparent function, `fromInteger @T n`
with `n : Integer` reduces by δ to a canonical value of `T` during definitional-equality checking. In particular,
numeric literals appearing as type indices are normalized before comparison.

<!-- core_semantics.erasure -->
### 14.4 Erasure

Intrinsic compile-time values, captured-region annotations, universes, `Type u` terms, quantities, regions, constraint
descriptors, rows, labels, proof terms used only for compile-time reasoning, and erased indices do not require implicit
runtime representation.

The runtime representation of a term is obtained by deleting:

* all computational binders and fields whose quantity is `0`, together with the corresponding erased arguments at
  applications, except for retained coherent constraint evidence and explicit `Dict` values governed by §5.1.3;
* all capture-annotation structure introduced by `captures (...)`; and
* all binders, fields, arguments, and package members whose values are compile-time values in the sense of §5.1.4.1,
  together with the internal nominal-scope tokens introduced by elaboration of §6.3.1.1, regardless of their written
  quantity annotation, unless preserved by an explicit reified runtime carrier.

For computational binders and fields, quantities other than `0` are runtime-relevant and remain in the runtime term.
For compile-time binders, fields, arguments, and package members, and for internal nominal-scope tokens introduced by
elaboration of §6.3.1.1, runtime relevance is determined by classifier rather than by written quantity.

This erasure is sound: runtime behavior of the computational fragment is preserved.

The compile-time fragment and the runtime fragment are largely disjoint machinery.
The compile-time fragment is primarily the typechecker, elaborator, and normalization engine, while the runtime fragment
contains computational values together with any evidence or metadata that has been explicitly reified.

Lowering from KCore to KBackendIR occurs only after the erasure obligations of this section have been discharged
(§17.4).

<!-- core_semantics.variant_runtime -->
### 14.5 Union / variant runtime representation

A value of type `Variant r` is represented at runtime as a tagged payload.

Runtime tags are not assigned by row-local ordinal position. Instead, each member type `T` of a union carries a stable
runtime tag identity `TagId(T)` derived from the canonical elaborated form of `T`.

Requirements on `TagId`:

* If `MemberId(T₁) = MemberId(T₂)`, then `TagId(T₁) = TagId(T₂)`.
* If `MemberId(T₁) ≠ MemberId(T₂)`, then `TagId(T₁) ≠ TagId(T₂)`.
* `TagId(T)` is independent of which particular row `r` currently contains `T`.
* `TagId(T)` must be stable across separately compiled modules and artifacts. A conforming implementation SHOULD derive
  `TagId(T)` from the same canonical normalization-and-hash procedure used for Hard Hashes (§15.1.2), applied to the
  fully elaborated member type `T`. If the concrete runtime tag uses a fixed-width truncation or other compact encoding
  of that identity, the implementation MUST resolve any resulting collisions deterministically so that the injectivity
  requirements above continue to hold. Consequently, two separately compiled modules that agree on the canonical Hard
  Hash of the elaborated member type `T` MUST assign the same `TagId(T)`.

Implementations MAY realize `TagId` using deterministic content-addressed identities, interned canonical-type
identifiers, or another injective encoding of canonical member types. The observable requirement is stable member-type
identity, not any particular concrete tag bit-pattern. `TagId` is therefore insensitive to importer-local `clarify` and
other local transparency changes that do not alter canonical member identity.

Injection:

* `(| v : T |) : Variant r` stores `TagId(T)` together with payload `v`.

Elimination:

* `match` on a variant inspects the runtime tag identity and dispatches to the matching branch.

Consequences:

* When elaboration inserts a widening from `Variant r₁` to `Variant r₂` where `r₁ ⊆ r₂`, that coercion is true
  zero-cost: the runtime representation is unchanged.
* Canonical row ordering remains relevant for row normalization and definitional equality, but not for runtime tag
  assignment.

<!-- core_semantics.record_canonicalization -->
### 14.6 Record canonicalization

Record types and values normalize to a canonical dependency-respecting field order.

Canonical order:

* A record field may depend only on earlier fields.
* The canonical ordering is the dependency-respecting topological order; ties are broken by the Unicode scalar-value
  lexicographic order of field names.
* Two record types are definitionally equal iff their canonical forms are field-wise definitionally equal, including
  identical field quantities.

Record update:

* Record update is typechecked using definitional equality as specified in §5.5.5.

<!-- core_semantics.mutable_state_elision -->
### 14.7 Elision of mutable state

State elision and allocation elision:

Implementations MAY lower `newRef`, `readRef`, and `writeRef` operations to local stack allocations, register
assignments, or SSA form, completely eliding the heap-allocated `Ref` representation, provided the compiler can prove
that the `Ref` does not escape the current stack frame.

In particular, if a `Ref` introduced by `var` is never captured by a closure that outlives the function, and is never
returned or passed to a function that retains it, the implementation SHOULD NOT heap-allocate the reference.

This is an as-if optimization: the observable behavior of the program MUST remain the same as if the `Ref` had been
represented explicitly.

<!-- core_semantics.runtime_model -->
### 14.8 Runtime model for `IO`, fibers, interruption, STM, handlers, and resumptions

This section constrains backend implementations of `IO`, `handle`, and `deep handle`. The concrete representation is
implementation-defined, but the observable behavior below is mandatory.

<!-- core_semantics.runtime_model.io_is_kappa_owned_runtime_semantics -->
#### 14.8.1 `IO` is Kappa-owned runtime semantics

`IO` semantics are defined by Kappa source semantics and this chapter.

A backend MAY realize `IO` using host threads, virtual threads, tasks, promises, coroutines, event loops, exceptions,
or other host mechanisms, but those mechanisms are implementation techniques only.

In particular, the following are defined by Kappa rather than by the host:

* typed failure vs interruption vs defect classification;
* fiber lifetime and `fork` / `forkDaemon` behavior;
* interruption delivery and masking;
* finalizer execution and unwinding order; and
* STM serializability and retry behavior.

<!-- core_semantics.runtime_model.fibers_supervision_scopes -->
#### 14.8.2 Fibers and supervision scopes

Every `IO` computation executes in a fiber.

A fiber is a lightweight implementation-managed runtime thread. A conforming implementation MAY multiplex many fibers
over fewer host execution resources.

Every fiber has a root supervision scope.

For structured concurrency, the implementation MUST behave as if evaluation of an `IO` `do` block introduces a nested
supervision scope. Under this model:

* `fork` attaches the created child fiber to the innermost current supervision scope;
* `forkDaemon` does not attach the created child fiber to that scope;
* exiting a supervision scope interrupts every still-live attached child fiber and waits until each such child has
  terminated before the enclosing scope's own deferred actions and release actions run.

This ordering is mandatory: child-fiber termination, including child finalizers, completes before parent-scope release
of resources owned by that scope.

If mandatory scope shutdown of one or more child fibers produces non-success runtime causes, those causes participate in
the enclosing scope's sequential cause composition.

<!-- core_semantics.runtime_model.interruption_delivery_masks -->
#### 14.8.3 Interruption delivery and masks

Interruption is modeled as an asynchronous request.

An implementation MUST deliver interruption only at interruption points.

At minimum, the following are interruption points outside masked regions:

* runtime suspension points;
* waiting operations such as `await` and `join`;
* parking during `STM.retry`;
* any explicit `poll`.

Masked execution suppresses interruption delivery but records the pending request. When masked execution leaves the
masked region, the pending request becomes observable at the next interruption point.

Finalizers and release actions run in masked state.

Portable source semantics expose only interruption. Arbitrary cross-fiber asynchronous exception injection is not part
of the portable subset.

<!-- core_semantics.runtime_model.scheduler_fairness_safe_points_timers -->
#### 14.8.3A Scheduler fairness, yielding, safe points, and timers

A conforming implementation MUST provide weak fairness among runnable fibers within one runtime agent.

At minimum:

* a fiber that remains continuously runnable across infinitely many scheduler decisions MUST eventually be scheduled;
* `cede` returns the current fiber to runnable state and permits another runnable fiber to be chosen before that fiber
  is resumed;
* the implementation MUST behave as if long-running pure execution contains implementation-defined safe points at which:
  * pending interruption may be observed, and
  * scheduler fairness may be restored;
* these safe points MAY be realized by asynchronous preemption, loop-backedge checks, reduction budgets, signal
  checks, or another observationally equivalent mechanism;
* `sleepFor` and `sleepUntil` park only the current fiber, not an entire runtime agent;
* timer delivery is based on monotonic time rather than wall-clock time.

A backend MUST ensure that blocking foreign work does not permanently monopolize scheduler resources required by
unrelated runnable fibers.

<!-- core_semantics.runtime_model.fiber_local_state_explicit_scopes_monitors_promises -->
#### 14.8.3B Fiber-local state, explicit scopes, monitors, and promises

Fiber-local state:

* each fiber carries a logically private mapping from `FiberRef` cells to current values;
* child-fiber creation copies the parent's current `FiberRef` values at fork time;
* later parent and child updates are independent;
* restoration performed by `locallyFiberRef` behaves as masked finalization.

Explicit scopes:

* `forkIn scope io` attaches the created child fiber to the explicit supervision scope `scope`;
* `shutdownScope scope` interrupts all still-live fibers attached to `scope` and waits for their termination and
  finalizers before returning;
* `shutdownScope` is idempotent.

Monitors:

* a monitor is one-way observation of a target fiber's terminal `Exit`;
* creating, awaiting, or dropping a monitor does not create parent/child supervision in either direction.

Promises:

* a promise is single-assignment;
* the first successful `completePromise` fixes the promise's terminal `Exit`;
* later completion attempts leave that stored `Exit` unchanged;
* waiting on a completed promise returns immediately with the stored result.

<!-- core_semantics.runtime_model.fiber_identities_labels_reachability_gc -->
#### 14.8.3C Fiber identities, labels, handle reachability, and GC non-observability

Fiber identity and labels:

* every created fiber has a unique `FiberId` within one program execution;
* the identity reported by the runtime for a fiber MUST agree with the values returned by `fiberId` and
  `currentFiberId`;
* `setFiberLabel` and `locallyFiberLabel` mutate only the current fiber's diagnostic label;
* child fibers do not inherit labels implicitly;
* labels are diagnostic metadata only and do not alter source-level computation semantics other than through designated
  observability facilities.

Structured interrupt metadata:

* direct `interrupt` / `interruptFork` use interrupt tag `Requested`;
* `shutdownScope`, and implicit structured child shutdown on supervision-scope exit, use interrupt tag
  `ScopeShutdown`;
* `timeout` uses interrupt tag `TimedOut` for the interrupted computation;
* interruption of the losing branch of `race` uses interrupt tag `RaceLost`;
* host- or runtime-originated interruption not attributable to a specific Kappa fiber uses interrupt tag `External`;
* when a definite initiating Kappa fiber exists, that fiber's `FiberId` MUST be recorded in the `by` field of the
  resulting `InterruptCause`;
* otherwise the `by` field is `None`.

Timer and race winner selection:

* if completion of `io` and timer expiry become simultaneously observable for `timeout` with no source-level prior
  winner, completion of `io` wins;
* if both branches of `race` become simultaneously observable as completed with no source-level prior winner, the left
  branch wins.

Handle reachability and host GC:

* dropping the last reachable ordinary handle to a live `Fiber` MUST NOT interrupt, await, daemonize, or otherwise
  alter that fiber;
* dropping the last reachable ordinary handle to a live `Scope` MUST NOT shut down that scope;
* dropping the last reachable ordinary handle to a `Promise`, `Monitor`, `TVar`, or `FiberRef` MUST NOT by itself
  complete, detach, cancel, wake, or otherwise change the behavior of the underlying runtime entity;
* host garbage collection and host finalizers MAY reclaim unreachable runtime storage only when doing so is
  observationally equivalent to continued existence;
* source-level interruption, shutdown, release, wakeup, and finalization semantics MUST NOT depend on prompt host-GC
  finalization.

<!-- core_semantics.runtime_model.stm_runtime_obligations -->
#### 14.8.4 STM runtime obligations

A conforming implementation MUST realize `atomically` with serializable semantics relative to all `atomically`
executions in the same TVar domain.

At minimum:

* each transaction attempt observes a consistent snapshot for the TVars it reads;
* writes become visible only on successful commit;
* `retry` parks the fiber until one or more TVars read by the current attempt have changed;
* transactional choice (`orElse`) runs its right operand only when the left operand retries;
* once commit has begun, commit is uninterruptible.

A backend that lacks shared-memory parallel execution MAY still satisfy this section for a single runtime agent,
provided it preserves the same observable semantics for fibers in that agent.

<!-- core_semantics.runtime_model.memory_visibility_synchronization -->
#### 14.8.4A Memory visibility and synchronization

The portable inter-fiber memory-visibility guarantees of the runtime are defined by happens-before edges.

At minimum, the following introduce happens-before:

* evaluation that occurs in a parent fiber before `fork`, `forkDaemon`, or `forkIn` publishes inherited fiber-local
  state to the child before the child's first step;
* termination of a fiber happens-before any successful return from `await`, `join`, `interrupt`, `shutdownScope`, or
  `awaitMonitor` that observes that termination;
* the first successful `completePromise` on a promise happens-before every resumed or returning `awaitPromiseExit` or
  `awaitPromise` that observes that completion;
* successful `STM` commit happens-before any later transaction attempt that observes writes from that commit.

Ordinary refs introduced by `var`, and ordinary mutable cells provided through `MonadRef (IO e)`, are not portable
cross-fiber synchronization primitives by themselves.

Consequently:

* unsynchronized concurrent read/write or write/write races on such ordinary refs have no portable inter-fiber ordering
  beyond the happens-before edges above;
* portable cross-fiber coordination MUST use `await` / `join`, scopes / monitors, promises, interruption, `STM`, or a
  backend-specific atomic facility documented outside the portable subset.

<!-- core_semantics.runtime_model.runtime_tracing_diagnostics -->
#### 14.8.4B Runtime tracing and diagnostics

A conforming implementation MUST provide an implementation-defined debugging, profiling, tracing, dump, or equivalent
runtime-observability facility.

At minimum, that facility MUST be able to report, for each live fiber:

* a stable fiber identity;
* current supervision scope identity;
* parent fiber identity when one exists;
* fiber status (`running`, `runnable`, `parked`, `done`, or an observationally equivalent classification);
* current masking state;
* whether interruption is pending;
* current wait reason when parked, if any;
* any implementation-provided user label, if any; and
* terminal `Exit` information for completed fibers retained by the facility.

The facility MUST additionally be able to report scheduler-level counts of runnable and parked fibers per runtime
agent.

The stable fiber identity reported by this facility MUST agree with the values observed through `fiberId` and
`currentFiberId`.

Any label installed through `setFiberLabel` or `locallyFiberLabel` MUST be the user label reported for that fiber.

The exact API shape and output format are implementation-defined, but they MUST be deterministic for a fixed runtime
state.

<!-- core_semantics.runtime_model.captured_continuation_boundary -->
#### 14.8.5 Captured continuation boundary

When evaluation of `handle label expr with ...` or `deep handle label expr with ...` intercepts an operation at the
handled label, the captured continuation is the dynamic computation suffix from the operation site to the nearest
dynamically enclosing matching handler for that exact effect-label value.

The captured continuation includes:

* the evaluation context between the operation site and that handler;
* the loop, match, and return-target context in that segment;
* the active `do`-scope frames in that segment together with their current exit-action stacks;
* any fiber-local dynamic control state in that segment whose semantics affect subsequent execution, including the
  current interruption-mask state under §14.8.3;
* any intervening handler frames for other labels in that segment.

Resuming a captured continuation re-enters under the same interruption-mask state that held at the capture point, until
the resumed computation changes that state by ordinary execution.

It does not include:

* frames outside the matching handler boundary;
* compile-time-only entities erased under §14.4.

<!-- core_semantics.runtime_model.one_shot_multi_shot_realization -->
#### 14.8.6 One-shot and multi-shot realization

Let an operation declaration carry resumption quantity `q` as interpreted by §8.1.7A.

* If `q` permits at most one use of the resumption value, an implementation MAY represent the captured continuation
  destructively.
* If `q` permits more than one use, the implementation MUST provide persistent continuation behavior: every use of the
  resumption value must behave as if it resumed from the same captured control state.
* Valid implementation techniques include CPS conversion, heap-allocated frames, stack copying, segmented stacks,
  defunctionalized state machines, or any equivalent strategy.

The representation chosen is not observable except through the semantics fixed by this chapter.

<!-- core_semantics.runtime_model.tail_resumptive_clauses -->
#### 14.8.6A Tail-resumptive clauses

A handler clause is tail-resumptive when every normal control-flow path:

* performs exactly one resumption of the bound resumption value;
* performs that resumption in tail position with respect to the clause body; and
* neither stores, escapes, nor duplicates the resumption value before that tail resumption.

A conforming implementation MAY realize a tail-resumptive clause without general resumption capture, using an
observationally equivalent in-place transfer, skip-frame, join-point, or equivalent strategy.

Such a realization MUST preserve:

* exact effect-label matching;
* the shallow versus deep difference in handler reinstallation;
* all `defer`, `using`, and exit-action obligations of the captured segment; and
* the one-shot versus multi-shot behavior required by this chapter.

When such a realization uses a join point or finite local control protocol, that protocol optimizes only the
tail-resumptive transfer surrounding the resumption.
The resumption value itself remains governed by §§14.8.5-14.8.9 and is not reclassified as a finite local control
protocol of §17.4.7A.

<!-- core_semantics.runtime_model.store_interaction -->
#### 14.8.7 Store interaction

Continuation reuse does not imply global store rollback.

* Effects and allocations performed before the operation site remain performed.
* Ordinary heap objects and references reachable from the captured environment remain shared in the ordinary way.
* A `var` or `newRef` allocation executed before the operation site therefore remains the same object across multiple
  uses of the same resumption value.
* Effects and allocations performed after the resumption point occur anew on each use of the resumption value.

This rule applies equally to shallow and deep handlers.

<!-- core_semantics.runtime_model.exit_actions_resumption_reuse -->
#### 14.8.8 Exit actions under resumption reuse

Because active `do`-scope frames are part of the captured continuation boundary under §14.8.5, their exit-action
stacks are logically copied with the continuation state.

Therefore:

* a pending `defer` registered before the operation site but inside the captured segment is pending independently in
  each resumed execution;
* a `using`-introduced release obligation inside the captured segment is likewise pending independently in each resumed
  execution;
* each resumed execution unwinds and consumes only its own copy of those exit actions.

If such copying would duplicate live linear or borrowed resources unsoundly, the call-site capture rule of §8.1.8
rejects the program.

<!-- core_semantics.runtime_model.abandonment_and_escape_of_unused_resumptions -->
#### 14.8.8A Abandonment and escape of unused resumptions

A handler clause may complete without resuming a bound resumption `k`.

If `k` does not escape the operation clause, this abandons the captured continuation segment immediately.

Abandonment semantics:

* the captured `do`-scope frames of that segment are unwound exactly once;
* their pending exit actions execute in the same masked LIFO order as ordinary scope exit; and
* abandonment does not roll back general heap state or effects that occurred before the operation site.

Escape restriction:

* a resumption value whose captured segment contains pending exit actions MUST NOT escape the operation clause; and
* if the implementation detects such an escape, compilation fails. A conforming implementation MAY conservatively reject
  when it cannot prove that the captured segment is free of pending exit actions.

For multi-shot resumptions, each logical clone of the captured segment carries its own pending exit-action obligations.
Abandoning one clone does not discharge obligations of any other clone.

<!-- core_semantics.runtime_model.deep_handler_reinstallation -->
#### 14.8.9 Deep handler reinstallation

A deep handler behaves as if it were lowered to a shallow handler plus an internal recursive driver as specified in
§8.1.10.

Each use of a deep resumption first resumes the captured shallow continuation and then reinstalls the same deep handler
around the resumed computation.

If the deep resumption is multi-shot, this reinstallation occurs afresh for each use.

<!-- identity -->
## 15. Content-Addressed Identity

Kappa provides a built-in, content-addressed hashing system with two tiers.

Hashes are defined over KCore after macro execution and full elaboration. They are not defined over KFrontIR.
The semantic hashes of this chapter are not, by themselves, the required incremental invalidation keys of an
implementation; compiler fingerprints for that purpose are defined separately in §17.1.2.
Compiler observability artifacts such as stage dumps, pipeline traces, and reproducer bundles are not part of those
hashing inputs.

In package mode, hashes are deterministic functions of the source and dependency graph, because package-mode macros have
no `IO` capability (§5.8.6). Canonical hashes are computed from the perspective of the defining module, using that
module's own transparency rules.

In script mode, when elaboration-time macros observe external inputs, hashes are deterministic only relative to the
recorded macro input transcript of §5.8.6.

Hashes are defined over the KCore term or module after implicit resolution and execution of all elaboration-time macros
via `$(...)`; they do not apply to pre-expansion surface syntax.

<!-- identity.hash_system -->
### 15.1 The Two-Tier Hash System

Every top-level definition (term, type alias, trait, instance, `data` declaration, and similar declarations) has an
**Easy Hash**. A corresponding **Hard Hash** is defined for the same KCore term or module and is computed on demand and
cached.

<!-- identity.hash_system.easy_hash_structural_identity_hash -->
#### 15.1.1 Easy Hash (Structural Identity Hash)

The Easy Hash is always computed.

Its input is the tuple:

```text
(KCore term or module, macro input transcript)
```

where the macro input transcript is empty in package mode and in any compilation that performs no external macro
observations.

The KCore component is produced after:

* exhaustive β/η/ι reduction,
* full unfolding of type aliases,
* literal normalization (including `FromInteger`, `FromFloat`, and similar literal elaboration forms),
* no unfolding of ordinary function bodies or instance method bodies,
* De Bruijn indexing, canonical serialization, and hashing.

In script mode, the transcript component records the observable macro inputs that may have affected elaboration,
including evaluator-exposed environment/configuration values and the contents or exact digests of external resources
consulted.

Uses of the Easy Hash include:

* distribution and package identity,
* artifact caching,
* quick structural lookups,
* fallback when the Hard Hash is unavailable.

<!-- identity.hash_system.hard_hash_semantic_equivalence_hash -->
#### 15.1.2 Hard Hash (Semantic Equivalence Hash)

The Hard Hash is computed on demand and cached, keyed by Easy Hash.

It is computed by full normalization of the definition, including:

* all Easy-Hash normalization steps,
* complete δ-unfolding of transparent definitions reachable from the definition being normalized,
* unfolding of `assertTotal` definitions only when the implementation has separately verified them and recorded them as
  termination-certified (§16.4),
* respect for `opaque` and `private opaque` as defined from the perspective of the defining module: inside the defining
  module, local definitions are available normally (§2.5.2); when normalization reaches a definition imported from
  another module, it uses the transparency recorded in that imported definition's canonical artifact. A downstream
  `clarify` import item does not make an imported opaque definition unfoldable for hashing,
* memoization, fuel, or an equivalent implementation technique sufficient to guarantee that the normalization procedure
  itself terminates.

The resulting fully normalized form is De Bruijn-indexed, canonically serialized, and hashed.

The canonical Hard Hash of a definition is computed using the defining module's own transparency rules only, ignoring
downstream `clarify`. For coherence checking, implementations MUST compare these canonical hashes, not hashes recomputed
under the importing module's view. Coherence comparisons are performed on instantiated candidate dictionaries, not
merely on raw instance declarations. Those comparisons use the canonical instance-artifact hashes exported by the
defining modules.

The Easy Hash and Hard Hash of a definition are immutable properties of the compiled artifact produced by its defining
module. A downstream `clarify` import item MAY affect local typechecking and definitional equality in the importing
module, but it MUST NOT alter or recompute the canonical hashes recorded for the imported definition.

Because the Hard Hash is computed from an elaborated definition already parameterized by the macro input transcript, it
is likewise deterministic only relative to that transcript in script mode.

Caching contract (normative):

* Implementations MUST provide a persistent cache mapping `Easy Hash -> Hard Hash` or `Easy Hash -> Canonical Normal
  Form`.
* The cache MAY additionally record failure states such as "normalization failed" or "not yet computed".
* The cache MAY be global, per-project, or distributed.
* Once computed for a given elaborated definition, the Hard Hash is stable and reusable across compilations.

If an implementation cannot complete Hard-Hash normalization, it MUST treat the Hard Hash as unavailable rather than
emitting a non-canonical approximation. In package mode, if a Hard Hash is required for a coherence comparison, that
Hard Hash MUST be available from the defining artifact or computable on demand; otherwise package-mode compilation is a
hard error.

<!-- identity.hash_system.hash_stability_canonicalization -->
#### 15.1.3 Hash Stability and Canonicalization

Both hashes MUST be stable across compilations of the same source, modulo changes in dependencies and in the macro input
transcript when script-mode macro I/O is enabled.

In particular:

* the Easy Hash is canonical for the structural-baseline normalization of §15.1.1,
* the Hard Hash is canonical for the full normalization of §15.1.2,
* two semantically equivalent definitions that fully normalize to the same canonical normal form MUST produce the same
  Hard Hash.
* In package mode, reproducibility claims are absolute with respect to macro execution.
* In script mode, reproducibility claims are only relative to a fixed macro input transcript.

<!-- identity.hash_uses -->
### 15.2 Uses of the Hash System

<!-- identity.hash_uses.instance_coherence -->
#### 15.2.1 Instance Coherence

For any fully-applied, ground trait constraint `Tr args`, at most one distinct **semantic** implementation may exist in
the compilation unit's module closure. Coherence is checked on the surviving instantiated candidate dictionaries for
that goal, not merely on raw instance declarations.

In particular, overlap decisions are based on the canonical instance-artifact hashes exported by the defining modules.
Those hashes are computed in the defining modules' own transparency environments, never in the importer's local
`clarify` view. Downstream `clarify` MAY affect local typechecking, but it MUST NOT alter the hashes used for coherence.
In package mode, whenever a Hard Hash is needed for such a comparison, it MUST be available from the defining artifact
or computable on demand; otherwise compilation is a hard error.

Coherence check algorithm (normative):

1. Let the comparison set be the surviving instantiated candidate dictionaries produced by the instance-resolution
   algorithm of §12.3.1.
2. Compute or retrieve the canonical Easy Hash of each instantiated candidate dictionary from the defining module's
   exported instance artifact.
3. Group candidates by Easy Hash. Within each Easy-Hash group, all members are considered identical (harmless overlap).
   The compiler may arbitrarily pick one for code generation.
4. For candidates with different Easy Hashes:
   * if `coherence_mode = "structural"`, the compiler MUST treat the differing Easy Hashes as distinct implementations
     and reject the overlap;
   * if `coherence_mode = "semantic"`, the compiler MUST obtain the Hard Hash for each candidate in the comparison set,
     computing it on first need and reusing any previously cached canonical Hard Hash keyed by Easy Hash when valid; it
     then compares those Hard Hashes and rejects the program if any required Hard Hash cannot be obtained;
   * if `coherence_mode = "semantic-if-available"`, the compiler MUST attempt to compute the Hard Hash for each
     candidate in the comparison set;
   * under either semantic mode, if the Hard Hashes match, treat the candidates as semantically equivalent and therefore
     as harmless overlap;
   * under either semantic mode, if the Hard Hashes differ, the program violates coherence and the compiler MUST reject
     it;
   * under `semantic-if-available`, if one or more required Hard Hashes cannot be computed, the compiler MUST
     conservatively treat the differing Easy Hashes as distinct implementations and reject the overlap, while also
     emitting a warning that coherence was checked using structural identity only.

Hard-Hash policy for coherence:

* The coherence policy is controlled by `coherence_mode = "structural" | "semantic" | "semantic-if-available"`.
* In **script mode**, the default is `semantic-if-available`.
* In **package mode** (the default for reproducible builds), the compiler MUST behave as if `coherence_mode =
  "semantic"` for any coherence check that reaches the "different Easy Hash" branch above, and MUST compute Hard Hashes
  for coherence comparisons on first need and persist them in the artifact cache keyed by Easy Hash. Subsequent builds
  MAY reuse a cached canonical Hard Hash without recomputing normalization, provided the defining module's source and
  its transitive dependencies (tracked by Easy Hash) are unchanged. If any required Hard Hash normalization fails or
  otherwise cannot be completed, compilation is a hard error in package mode; the implementation MUST NOT fall back to
  structural-only coherence checking there.
* A compiler flag or manifest setting MAY request a different mode in script mode for debugging or performance work.
* Implementations MAY reject `coherence_mode = "structural"` or `coherence_mode = "semantic-if-available"` in package
  mode; if they do not reject such a request, they MUST still apply the package-mode rule above.

This rule guarantees that:

* two instances that are structurally identical after Easy-Hash normalization are always accepted,
* two instances that are extensionally equal after full normalization are also accepted when the Hard Hash is available,
* two instances that are not extensionally equal are rejected whenever the required Hard-Hash comparison is available,
* in script mode under `semantic-if-available`, if the Hard Hash is unavailable, structurally different instances are
  conservatively rejected with a warning.

<!-- identity.hash_uses.fast_path_definitional_equality -->
#### 15.2.2 Fast-Path Definitional Equality

When checking definitional equality (`t1 ≡ t2`) between two top-level definitions, or between two closed transparent
subterms for which Hard Hashes are available, the compiler MAY first compare their Hard Hashes.

* If both Hard Hashes are available and equal, the compiler MAY conclude that the terms are definitionally equal without
  further conversion checking.
* If the Hard Hashes differ, or one or both Hard Hashes are unavailable, the compiler MUST fall back to ordinary
  definitional-equality checking (§14.3).

This is an optimization only; §14.3 remains the normative definition of definitional equality.

Restriction:

* The Hard-Hash fast path may be taken only when both definitions are fully transparent at the use site. An `opaque`
  definition may not be treated as definitionally equal via Hard Hash unless the current compilation context has
  clarified it.
* This use-site transparency check affects only whether the fast path may be applied locally. It does not modify either
  definition's canonical Easy Hash or Hard Hash.

<!-- identity.hash_uses.macro_elaboration_time_caching -->
#### 15.2.3 Macro and Elaboration-Time Caching

A macro invocation of the form `$(macro '{ args... })` MAY be cached only under a key that includes, in addition to:

* the Hard Hash of the macro definition,
* the tuple of Hard Hashes of the argument syntax, and
* the digest of the macro input transcript,

a fingerprint of the call-site semantic environment sufficient to determine the result of any semantic reflection
performed during expansion.

At minimum, this fingerprint includes any dependency-tracked semantic query results, imported interface artifacts,
visibility / opacity state, and `unhide` / `clarify` effects consulted while elaborating reflected syntax or answering
semantic reflection queries.

An implementation MAY realize this requirement by keying macro-result caches through the ordinary dependency-tracked
query system of §17.2 rather than by materializing one standalone cache-key tuple.

When the cache key matches a previous invocation, the compiler MAY reuse the previously elaborated result rather than
re-executing the macro body.

In package mode, the transcript component is empty. In script mode, this caching is sound only when the transcript
digest reflects the external inputs observed during macro execution (§5.8.6).

This mechanism supports:

* deterministic macro caching across files and compilations,
* safe incremental recompilation of macro-heavy code.

<!-- identity.hash_uses.future_recommended_uses -->
#### 15.2.4 Future / Recommended Uses (non-normative)

The two-tier hash system is designed to support additional applications, including:

* incremental compilation by detecting unchanged definitions via Easy Hash,
* proof and instance-dictionary caching,
* cross-module specialization and inlining decisions,
* reproducible-build verification by comparing Hard Hashes of final artifacts.

---

<!-- unsafe_debug -->
## 16. Unsafe and Debug Facilities

Kappa includes a small set of facilities intended for debugging, experimentation, trusted bootstrapping, and other
non-portable work.

<!-- unsafe_debug.classification -->
### 16.1 Classification and portable subset

The safe portable subset of Kappa excludes:

* `unhide`,
* `clarify`,
* `assertTotal`,
* the standard debug-introspection module `std.debug` of §16.6.

Programs that use any of these facilities are outside the safe portable subset.

These facilities remain part of the language specification, but they are classified as unsafe/debug features rather than
ordinary portable surface forms.

The compiler observability facilities of §17.1.3-§17.1.7 are ordinary required tooling facilities.
They are not classified as unsafe/debug language features merely because they expose intermediate compiler
representations.

<!-- unsafe_debug.build_gating -->
### 16.2 Build-level gating

The legacy per-module attributes `@AllowUnhiding` and `@AllowClarification` are not part of the language. These
facilities are gated at the build-configuration level rather than per module.

A compilation unit's manifest or build configuration (implementation-defined: package manifest, compiler flag, or
project file) specifies:

```text
allow_unhiding            : Bool
allow_clarify             : Bool
allow_assert_total        : Bool
allow_debug_introspection : Bool
```

Defaults:

* In package mode, all four default to `false`.
* In script mode, implementations MAY default any or all of them to `true` for experimentation. Implementations SHOULD
  document the actual defaults they choose.

Violations are compile-time errors. Diagnostics for such errors MUST identify both:

* the offending `unhide` / `clarify` import item, `assertTotal` declaration, or `std.debug` import / use, and
* the build setting (`allow_unhiding`, `allow_clarify`, `allow_assert_total`, or `allow_debug_introspection`) that
  disallows it.

<!-- unsafe_debug.visibility_escapes -->
### 16.3 `unhide` and `clarify`

Surface syntax for these import-item modifiers is defined in §2.3.1.1.

Semantics:

* `unhide` imports a name that is marked `private` in the imported module, as if it were exported for that importing
  module.
* `clarify` requests that an imported `opaque` item be treated as transparent for definitional equality in the importing
  module.
* `unhide` and `clarify` may be combined for the same import item. Their order in the surface syntax is not semantically
  significant.
* `clarify` affects only the importing module. It does not change the imported module, does not modify the canonical
  hashes recorded in the imported artifact, and is not re-exported implicitly.
* If the required build setting is disabled under §16.2, use of `unhide` or `clarify` is a compile-time error.
* If the compiler does not have access to the requested private definition or to the definitional content of an opaque
  item (for example because separate compilation artifacts omit bodies or constructors), `unhide` / `clarify` is a
  compile-time error.

<!-- unsafe_debug.assert_total -->
### 16.4 `assertTotal`

`assertTotal` indicates that the programmer asserts a definition is total even if the compiler cannot prove it.

Syntax:

```kappa
assertTotal let f : T = ...
```

Meaning:

* `assertTotal` does not change runtime semantics.
* `assertTotal` is a trust boundary: it admits code that the compiler cannot justify as terminating.

Unfolding / definitional equality:

* A definition accepted **solely** via `assertTotal` is **not** termination-certified by default and therefore does not
  unfold for definitional equality (§6.4.1, §14.3) unless the implementation separately verifies its termination and
  records it as termination-certified.

  "Records it" is normative: under separate compilation, the module/interface artifact must carry (at minimum) a stable
  bit indicating whether each transparent definition is termination-certified and therefore eligible for δ-reduction. If
  that bit is absent, the definition must be treated as not termination-certified for unfolding purposes, even if a body
  is present.

Interaction with `decreases`:

* An explicit `decreases` clause (§6.4.4) is the preferred fallback when inference under §6.4.2 is insufficient for a
  recursive definition.
* If `assertTotal` is used anyway, the implementation MAY still attempt to verify termination (including using the
  provided `decreases` measure). If verification succeeds, the definition may be recorded as termination-certified;
  otherwise it remains ineligible for unfolding in definitional equality.

Gating:

Code accepted solely via `assertTotal` is in the unsafe/debug surface:

* It is a compile-time error unless the build enables `allow_assert_total` (§16.2).

<!-- unsafe_debug.backend_escapes -->
### 16.5 Backend-specific surface escapes

The safe portable subset of Kappa excludes any implementation-defined surface syntax or API for directly embedding
backend-specific code, backend-specific syntax fragments, or backend-specific intermediate representations.

If an implementation provides such a facility:

* it MUST document the facility as backend-specific;
* it MUST NOT require the facility for ordinary portable Kappa code;
* it MUST require explicit build-level or command-line enablement in package mode; and
* the compiler MUST NOT claim portable validation for the embedded backend-specific fragment beyond whatever
  validation is explicitly documented for that facility.

<!-- unsafe_debug.std_debug -->
### 16.6 Standard debug-introspection module `std.debug`

When `allow_debug_introspection` is enabled, implementations MUST provide a standard module `std.debug`.

No new surface syntax or keyword is introduced for this facility. The intended use is an ordinary import, for example:

```kappa
import std.debug as debug
```

The alias `debug` may then be used for ordinary qualified access and, under §2.8.5, as a reified module value.

If `allow_debug_introspection` is false, importing `std.debug` is a compile-time error.

`std.debug` exports at least:

```kappa
data DebugReason : Type =
    Opaque
    FunctionValue
    ForeignValue
    Cyclic
    DepthLimit
    SizeLimit
    Unsupported
    Unavailable

data DebugEq : Type =
    Equal
    Different
    Unknown DebugReason

data DebugField : Type =
    DebugField (label : Option String) (value : DebugTree)

data DebugTree : Type =
    Scalar      (text : String)
    Node        (tag : String) (fields : List DebugField)
    OpaqueValue (reason : DebugReason)

inspect :
    forall (@0 a : Type).
    (& x : a) -> DebugTree

render :
    forall (@0 a : Type).
    (& x : a) -> String

compare :
    forall (@0 a : Type).
    (& x : a) -> (& y : a) -> DebugEq
```

Normative meaning:

* `inspect` returns a structured debug view of `x`.
* For transparent data, records, sealed packages, and variants whose representation is available at the call site,
  implementations SHOULD expose constructor and field structure recursively.
* `render` returns a human-oriented rendering of `x`. It MAY be derived from `inspect`.
* `compare` is a debug comparison only. It MUST NOT participate in implicit resolution, equality reflection,
  definitional equality, instance search, or coherence.
* `std.debug` does not synthesize or bind ordinary trait evidence. In particular, importing or using `std.debug` MUST
  NOT make a missing implicit goal of type `Show a`, `Eq a`, or any other ordinary trait constraint become solvable.
* Implementations MAY consult already-available `Show a` or `Eq a` evidence when computing `render` or `compare`, but
  only if that evidence was already independently resolvable at the call site.
* If ordinary elaboration at the call site could not inspect a value's representation because of visibility or opacity,
  `inspect` MUST return `OpaqueValue Opaque` for the hidden portion.
* In that same situation, `compare` MUST return `Unknown Opaque` unless the implementation can determine the result
  without exposing hidden representation.
* For functions, foreign values, backend-specific runtime objects, cyclic structures not fully traversed, or cases
  stopped by implementation limits, `inspect` MUST use `OpaqueValue reason` and `compare` MUST use `Unknown reason` with
  the corresponding `DebugReason`.
* Returning `Equal` or `Different` has no semantic effect beyond this debug facility.
* The exact text produced by `render` and the exact `tag` strings used in `DebugTree` are implementation-defined unless
  otherwise documented by the implementation.
* Use of `std.debug` is outside the safe portable subset of §16.1.

Example:

```kappa
import std.debug as debug

let main : UIO Unit = do
    println (debug.render value)
    println (show (debug.compare x y))
```

---

<!-- compiler -->
## 17. Compilation pipeline and backend profiles

This chapter defines the normative implementation pipeline and the backend profiles standardized by this specification.

<!-- compiler.pipeline -->
### 17.1 Pipeline and artifacts

A conforming implementation processes each compilation unit through the following conceptual stages:

1. surface source
2. KFrontIR
3. KCore
4. KBackendIR
5. target-profile lowering
6. final artifact

An implementation MAY fuse stages, skip physically materializing them, or internally refactor them, provided all
observable behavior, hashing inputs, and diagnostics are as if the stages above had occurred exactly as specified.

The selected backend profile is implementation-defined. The selected backend profile, together with any
backend-intrinsic set it activates, is part of the effective build configuration.

A compilation that depends on backend intrinsics or backend-specific `expect` satisfaction is valid only when those
requirements are met by the chosen backend profile.

<!-- compiler.pipeline.incremental_units -->
#### 17.1.1 Incremental units

A conforming implementation MUST behave as if compilation is decomposed into memoizable incremental units.

At minimum, the following conceptual units exist:

* source-file text units;
* module import-surface units;
* declaration-header units;
* declaration-body units;
* macro-expansion units;
* module-interface units;
* KCore declaration or module units;
* KBackendIR declaration or module units; and
* target-lowering units.

An implementation MAY choose finer-grained units, but it MUST NOT behave as if every source edit invalidates the entire
compilation unit unless the edit in fact changes every required upstream unit.

Downstream ordinary compilation depends on imported module-interface units, not on imported implementation bodies,
except where this specification explicitly permits otherwise (for example the unsafe/debug facilities of §16.3).

<!-- compiler.pipeline.compiler_fingerprints -->
#### 17.1.2 Compiler fingerprints

For incremental invalidation, a conforming implementation MUST define deterministic compiler fingerprints distinct from
the semantic hashes of §15.

Compiler fingerprints are implementation-defined digests or equivalent stable change detectors used to decide reuse and
invalidation.
They are not semantic identities and need not coincide with Easy Hash or Hard Hash.

At minimum, the implementation MUST behave as if the following fingerprints exist:

* `SourceFingerprint`:
  determined by the source text and any source-level metadata that affects parsing or module identity.

* `HeaderFingerprint`:
  determined by the parts of a declaration needed for `DECLARATION_SHAPES`, `HEADER_TYPES`, `STATUS`, and
  `IMPLICIT_SIGNATURES`.

* `InterfaceFingerprint`:
  determined by the externally visible interface of a module or declaration, including:
  * exported binding groups by spelling and declaration kind,
  * importable fixity declarations,
  * visibility and opacity classification,
  * exported signatures,
  * associated-type and effect-operation signatures where applicable,
  * instance heads and any interface-visible coherence metadata, and
  * any transparent definitional content required by downstream ordinary definitional equality.

* `BodyFingerprint`:
  determined by the non-interface body content of a declaration after the declaration header is fixed.

* `BackendFingerprint(profile)`:
  determined by all inputs relevant to KBackendIR lowering and target lowering for the selected backend profile,
  including the effective build configuration, backend-intrinsic set, representation-selection inputs, and relevant
  imported interface fingerprints.

Rules:

* A body-only change that leaves `InterfaceFingerprint` unchanged MUST NOT by itself invalidate downstream modules that
  depend only on the module's ordinary interface.
* A change that alters `InterfaceFingerprint` MUST invalidate downstream results that depend on that interface from the
  earliest affected phase.
* A change that alters only `BodyFingerprint` MAY preserve previously computed header-, interface-, and
  downstream-import results.

InterfaceFingerprint intentionally tracks exported binding-group exposure as well as semantic object identities.

Therefore a rename, move, or re-export change that preserves the semantic object identity of the affected definition may
still change `InterfaceFingerprint` if the exported binding groups or declaration-kind exposure change.

Such a change need not by itself invalidate cached semantic objects keyed only by semantic object identity, but it does
invalidate any result that depends on the affected interface surface.

The semantic hashes of §15 remain normative for semantic identity, coherence, and related uses.
They MUST NOT be assumed to be the sole or primary invalidation keys of a conforming implementation.

<!-- compiler.pipeline.compiler_observability -->
#### 17.1.3 Compiler observability

A conforming implementation MUST provide user-visible or API-visible compiler observability facilities.

At minimum, the implementation MUST support:

* requesting a snapshot at any named conceptual checkpoint of the compilation pipeline;
* serializing that snapshot in machine-readable form;
* verifying the invariants expected at that checkpoint;
* listing the actual sequence of compilation steps executed for the current request; and
* stopping compilation before or after a named checkpoint.

The term **stage dump** in this chapter denotes a serialization of the representation at a checkpoint together with the
metadata required to understand it.

A stage dump is not required to be a tree.
In particular, KBackendIR dumps may be graph-structured.

<!-- compiler.pipeline.named_checkpoints -->
#### 17.1.4 Named checkpoints

A conforming implementation MUST behave as if the following named checkpoints exist:

* `surface-source`;
* `KFrontIR.RAW`;
* each monotone KFrontIR phase of §17.2.2;
* `KCore`;
* `KBackendIR`; and
* each exposed post-KBackendIR target-lowering checkpoint.

An implementation MAY fuse internal passes or omit physical materialization of a checkpoint, provided it can still
produce a stage dump observationally equivalent to the representation that would exist at that checkpoint.

For representations that are not themselves IR-shaped, such as a final native executable or classfile bundle, the
implementation MUST provide an artifact-manifest dump rather than pretending that an AST exists there.

<!-- compiler.pipeline.machine_readable_dump_formats -->
#### 17.1.5 Machine-readable dump formats

Implementations MUST support at least two machine-readable serializations for every required stage dump:

* `json`
* `sexpr`

Implementations MAY support additional formats.

The `json` and `sexpr` serializations for the same checkpoint MUST carry equivalent semantic information.

Neither format is required to round-trip through the source parser.
They are observability formats, not source syntax.

Every stage dump MUST be self-describing and versioned.
At minimum it MUST identify:

* dump format and dump-schema version;
* language version;
* compiler implementation identifier and version;
* checkpoint name;
* module identity or other compilation-root identity;
* selected backend profile and backend-intrinsic set;
* effective build-configuration digest or equivalent summary;
* the dumped nodes, symbols, edges, and annotations relevant to that checkpoint;
* source origins and synthetic-origin chains; and
* any diagnostics attached to the snapshot.

All cross references within a stage dump MUST use explicit IDs present in that dump.

A stage dump MUST be deterministic for fixed inputs, effective build configuration, and implementation version.

Implementations SHOULD additionally provide a canonicalized dump mode that omits, redacts, or deterministically
renumbers unstable implementation-generated identifiers such as local variable IDs, temporary binder IDs, or other
non-semantic internal numbering.

This canonicalized mode is intended for diff-friendly testing and debugging and MUST NOT remove or alter semantic
information.

In particular, its content and ordering MUST NOT depend on:

* heap addresses;
* hash-table iteration order;
* non-deterministic traversal order; or
* thread scheduling.

Where source order is semantically relevant, it MUST be preserved.
Where source order is not semantically relevant, ordering MUST be canonical and implementation-defined.

If the current compilation context does not possess private or opaque definitional content, the dump MUST NOT synthesize
or reveal that content.
Any unavailable, omitted, or redacted content MUST be represented explicitly as such rather than silently disappearing.

Sensitive transcript or configuration values MAY be redacted by default.
If redacted, the dump MUST carry an explicit redaction marker and a stable digest or equivalent placeholder in place of
the redacted value.

Stage dumps, pipeline traces, and reproducer bundles are observability artifacts only.
They are not inputs to the hashing rules of §15.

<!-- compiler.pipeline.traces_checkpoint_verification -->
#### 17.1.6 Pipeline traces and checkpoint verification

A conforming implementation MUST expose a pipeline trace for each compilation request.

A pipeline trace records the actual steps executed, in order, together with their checkpoint boundaries.

Each trace step MUST identify at least:

* a stable step name;
* its input and output checkpoint names;
* whether the step changed the representation; and
* whether verification was attempted and whether it succeeded.

An implementation SHOULD additionally record elapsed time and other implementation-defined metrics.

An implementation MUST provide a way to verify the representation at any named checkpoint.

If verification fails, the resulting diagnostic MUST identify:

* the violated invariant;
* the checkpoint at which it was checked; and
* at least one related node, symbol, block, edge, or source origin.

An implementation SHOULD support `verify-each` behavior, or an equivalent facility, that verifies every checkpoint
crossed by a compilation request.

An implementation SHOULD support before-and-after dumps for each transformation step that changes the representation.

<!-- compiler.pipeline.reproducer_bundles -->
#### 17.1.7 Reproducer bundles

A conforming implementation MUST provide a mode that emits a reproducer bundle when compilation fails due to an internal
compiler error or backend-lowering failure.

A reproducer bundle MUST contain enough information to debug the failure without consulting the compiler's transient
in-memory state.

At minimum it MUST include:

* the selected compilation roots or their stable identities;
* the effective build configuration;
* the selected backend profile and backend-intrinsic set;
* the pipeline trace up to the point of failure;
* the last successfully verified stage dump, if any;
* the failing checkpoint and any immediately available diagnostics; and
* compiler implementation identifier and version.

A reproducer bundle SHOULD additionally include the partially constructed representation at the point of failure when
one exists.

A reproducer bundle MAY redact sensitive external data under the rules of §17.1.5.

<!-- compiler.pipeline.compilation_tasks -->
#### 17.1.8 Compilation tasks

A conforming implementation MAY expose any number of user-facing commands, APIs, or build actions, but it MUST behave as
if the following conceptual tasks exist:

* `analyze`:
  parse source, construct KFrontIR, and resolve only the phases required by the request.
  `analyze` produces no KCore, KBackendIR, or target artifact unless the request itself requires them.

* `check`:
  verify the selected compilation roots through KCore construction and all semantic checks required by this
  specification.
  `check` need not perform KBackendIR lowering or target lowering.

* `emit-interface`:
  produce the separate-compilation interface artifacts of §17.1.9 and, on request, the canonical interface view of
  §17.1.9.1, or observationally equivalent in-memory results.

* `compile`:
  lower checked roots through KBackendIR and target-profile lowering to a final artifact.

* `run`:
  execute checked roots using one of the execution strategies of §17.1.10.

The names of these tasks in any particular toolchain are implementation-defined.

<!-- compiler.pipeline.separate_compilation_artifacts -->
#### 17.1.9 Separate-compilation artifacts

For separate compilation, a module MAY be represented by:

* source files;
* a module interface artifact; and
* a module implementation artifact.

A module interface artifact is backend-independent and MUST be sufficient for ordinary downstream compilation without
consulting the defining module's source text.

A module interface artifact MUST record at least:

* the module identity and dependency identities required by §§2.1, 2.2, 2.3, 2.3.2, and 2.3.3;
* the exported binding groups by spelling and declaration kind, including importable fixity declarations;
* visibility and opacity classification of exported ordinary items;
* the fully elaborated signatures of exported terms, including:
  * any user-written or inferred `captures (...)` annotations of §5.1.6.1,
  * ordered binder metadata needed for downstream application-site elaboration, including binder names, explicitness,
    quantities, receiver markers, and any `inout`-relevant formal-parameter information, and
  * any interface-visible classification relevant to use sites, such as pattern-head eligibility under §7.7;
  Capture annotations recorded in a module interface artifact are part of the exported type itself. A conforming
  implementation MUST NOT instead require a second hidden region-summary channel outside the exported signature to make
  ordinary downstream escape checking sound.
* for each exported projection definition, enough metadata to perform downstream projection-call elaboration and
  admissibility checking, including:
  * projection-vs-ordinary-term classification,
  * place-binder positions and any receiver marker,
  * the normalized selector tree or observationally equivalent lowering summary needed for §17.3.1.3,
  * and the static footprint summary required by §§5.1.7.2 and 8.8;
* the signatures of exported types, traits, constructors, associated static members, effect interfaces, and effect
  operations, insofar as those entities are available to downstream code;
* any escaped local nominal families referenced by exported signatures, exported compile-time members, or transparent
  definitional content, together with:
  * their canonical family identities under §17.3.4.1,
  * their nominal kind,
  * their closure-converted capture telescope, and
  * any transparency / manifest-definitional metadata needed for downstream definitional equality;
* any applications of such escaped local nominal families that occur in exported types or compile-time terms, recorded
  by family identity together with the corresponding interface-representable captured arguments;
* top-level instance heads and any metadata required for instance search and coherence under §§12.3 and 15.2.1;
* the hashes or equivalent identity data required by §15 for exported definitions, instances, and escaped local nominal
  families recorded by the interface artifact;
* for a host binding module, the pinned host-source identity, binding-generator identity, host scope kind, and any
  implementation-documented adapter-mode identity, marshalling profile, deployment prerequisite, native-library
  identity, or trusted binding-summary identities required by the exported surface under §17.7;
* for an exported raw foreign binding, the binder metadata required for downstream application-site elaboration,
  including any receiver marker, overload-disambiguation spelling, nullability classification used to obtain the
  exported signature, and any interface-visible foreign-call classification, calling-convention classification, or
  adapter-visible call-state-capture classification;
* enough definitional content for exported transparent items to support downstream definitional equality;
* for each exported transparent recursive definition, whether it is termination-certified and, if so, the termination
  kind (`structural`, `well-founded`, `size-change`, or `verified-assertTotal`), together with either:
  * the canonical visible `decreases` measure, or
  * an equivalent stable termination-certificate payload sufficient for downstream unfolding decisions, hashing, and
    reproducible separate compilation;
* enough metadata to reconstruct the reified-module view of §2.8.5, including the kind-tagged exported-member surface
  and the opaque-vs-transparent classification needed for local qualified access and module-value projection.

For imported definitions, ordinary downstream compilation MUST use the recorded termination-certification status from the
module interface artifact as the source of truth for δ-reduction eligibility. It MUST NOT require re-running
termination inference on imported bodies in ordinary compilation mode.

An implementation MAY offer an explicit re-verification or paranoid mode that rechecks recorded certificates. If such a
recheck is requested and disagrees with the recorded certification, compilation fails.

If an exported signature, exported compile-time member, or transparent definitional equation would mention a local
nominal family that cannot be represented using the preceding interface-artifact data, the module is ill-formed for
export and compilation fails in the defining module.

A module interface artifact MUST distinguish the semantic object identity of an exported object from the current
binding-group entries that expose it.

A conforming implementation MUST permit browsing and querying module interface artifacts, or semantic-object-store
entries derived from them, without reparsing original source text.

A module interface artifact MAY omit:

* the bodies of private items;
* the definitional content of opaque items;
* implementation-private host metadata beyond the identities and interface-visible classifications required above;
* target-specific code.

If a requested `unhide` or `clarify` operation would require private or opaque definitional content that is not present
in the available artifacts, compilation fails as specified in §16.3.

A module implementation artifact MAY contain target-specific code, backend-specific auxiliary data, implementation-
private bodies, or any combination thereof.
It MAY embed the interface artifact or refer to it by stable identity.

Ordinary downstream compilation MUST NOT require access to a target-specific implementation artifact.

<!-- compiler.pipeline.separate_compilation_artifacts.interface_view -->
##### 17.1.9.1 Canonical interface view

Because ordinary downstream compilation depends on module interface artifacts rather than implementation bodies, a
conforming implementation MUST provide a canonical human-readable rendering of each module interface artifact.

The canonical interface view is a source-like rendering derived from the module interface artifact itself rather than
reparsed from the original source text. It is an observability and tooling artifact only; it does not change name
resolution, hashing, or separate-compilation semantics.

At minimum, the canonical interface view MUST include:

* the module identity;
* for a host binding module, the pinned host-source identity, binding-generator identity, declared host scope kind, and
  any adapter mode fixed by the exported surface;
* exported binding groups by spelling and declaration kind;
* importable fixity declarations;
* visibility and opacity classification;
* exported signatures of terms, rendered after elaboration of any inferred non-empty `captures (...)` annotations,
  together with the corresponding binder metadata needed for downstream application-site elaboration and any
  interface-visible classification relevant to use sites, such as pattern-head eligibility under §7.7;
  The canonical interface view MUST render any non-empty inferred `captures (...)` annotation explicitly, even if it was
  omitted in the original source.
* deterministic overload-disambiguation spellings and any interface-visible foreign-call, calling-convention, or
  adapter-mode classifications for exported raw foreign bindings, when present;
* exported signatures of types, traits, constructors, associated static members, effect interfaces, and effect
  operations, insofar as those entities are available to downstream code;
* exported instance heads and any interface-visible coherence metadata;
* for each exported transparent recursive definition, whether it is termination-certified, its termination kind, and
  any canonical visible `decreases` clause or equivalent stable certificate summary recorded by the interface artifact;
* deterministic rendered names or references for any escaped local nominal families needed to explain exported types,
  compile-time members, or transparent definitional content, together with enough information to recover their family
  identities and captured-argument applications under §17.3.4.1;
* re-exports introduced by `export`; and
* enough transparent definitional content to explain what downstream ordinary definitional equality may unfold.

It MUST omit:

* private bodies;
* the hidden representation of opaque items;
* implementation-only helper declarations that are not part of the module interface artifact; and
* target-specific code.

The rendering MUST be deterministic for fixed inputs and SHOULD use a source-like syntax close to ordinary Kappa
declarations.

<!-- compiler.pipeline.execution_strategies -->
#### 17.1.10 Execution strategies

A `run` task executes checked roots under the effective build configuration and selected backend profile.

A conforming implementation MAY realize `run` by:

* interpreting KBackendIR or an observationally equivalent runtime form;
* JIT-compiling from KBackendIR or an observationally equivalent runtime form and then executing the result; or
* compiling to a final backend artifact and launching that artifact.

If a selected execution strategy cannot realize the required backend intrinsics, foreign bindings, or runtime facilities
of the chosen backend profile, `run` MUST fail before execution begins.

For the same selected roots, build configuration, dependency closure, and backend profile, `run` MUST be observationally
equivalent to `compile` followed by execution of the resulting artifact.

<!-- compiler.pipeline.full_fidelity_concrete_syntax_ksyntax -->
#### 17.1.11 Full-fidelity concrete syntax (`KSyntax`)

A conforming implementation MUST behave as if a full-fidelity concrete syntax representation `KSyntax` exists beneath or
alongside KFrontIR.

`KSyntax` preserves every token and every piece of trivia, including at least:

* comments,
* whitespace,
* delimiters,
* exact lexical spelling of tokens, and
* the original source ordering of concrete syntax.

For unchanged subtrees, `KSyntax` round-trips to the exact original source text.

KFrontIR MAY be constructed from `KSyntax`, fused with it, or reconstructed from it on demand, provided observable
behavior is as if `KSyntax` existed.

`KSyntax` is purely syntactic:

* names, symbols, types, and other semantic facts are not part of `KSyntax` except as external references; and
* parse errors may be represented by error or missing syntax nodes in the same error-tolerant style required of
  KFrontIR.

Implementations intended for editor, formatter, or refactoring use SHOULD preserve and reuse unchanged `KSyntax`
subtrees across localized edits.

<!-- compiler.kfrontir -->
### 17.2 KFrontIR

KFrontIR is the source-preserving, symbol-bearing, error-tolerant frontend representation of Kappa, constructed from
`KSyntax` or an observationally equivalent full-fidelity syntax representation.

Normative role:

* KFrontIR is the representation used for incremental compilation, editor tooling, lazy semantic analysis, and
  source-based diagnostics.
* KFrontIR preserves user-written syntactic structure closely enough to support navigation, completion, refactoring, and
  diagnostic positioning.
* KFrontIR is not the source of truth for definitional equality, normalization, or the hashing rules of §15. KCore is
  the source of truth for those tasks.
* A conforming implementation MAY fuse the parser AST and KFrontIR, or omit materialization of KFrontIR entirely,
  provided the observable behavior is as if a representation satisfying this section existed.

<!-- compiler.kfrontir.structural_invariants -->
#### 17.2.1 Structural invariants

Every user-written declaration, expression, pattern, import item, export item, type reference, and record field in
source has a corresponding KFrontIR node or source-anchor record.

Each KFrontIR node carries either:

* a source origin identifying its file and source range, or
* a synthetic origin identifying a generated node together with the originating user-written construct from which it was
  derived.

Generated nodes may arise from:

* implicit prelude imports,
* desugarings,
* inserted coercions,
* macro expansion,
* elaboration-time string/type parsing,
* or other implementation-defined frontend rewrites.

If a generated node corresponds to a user-written construct, the mapping from the generated node back to that construct
MUST be retained for diagnostics and navigation.

For comprehensions, KFrontIR MUST preserve the original clause structure, clause order, delimiter kind, and any
custom-carrier prefix until `CORE_LOWERING`.

Desugared or normalized pipeline nodes MAY be added, but:

* the original comprehension node, or an observationally equivalent source-linked representation, MUST remain available
  for tooling and for construction of `RawComprehension`; and
* the normalized comprehension representation required to construct `ComprehensionPlan` MUST be recoverable without
  reparsing source text.

KFrontIR distinguishes source references from resolved semantic objects:

* a type-reference node preserves the user-written type syntax and may additionally carry a resolved semantic type once
  the relevant phase has run;
* an expression node preserves source structure and may additionally carry resolved type information, expected type
  information, flow facts, and other semantic annotations once available;
* a pattern node preserves source structure and may additionally carry refutability, binding, and reachability
  information once available.

Within one analysis session:

* every declaration node has a unique declaration symbol;
* every resolved reference targets declaration symbols rather than raw syntax nodes;
* declaration symbols for unchanged declarations are stable for the lifetime of that analysis session.

Cross-session symbol identity is implementation-defined. Clients MUST NOT compare symbols from different analysis
sessions for identity.

<!-- compiler.kfrontir.resolve_phases -->
#### 17.2.2 Resolve phases

KFrontIR phases are monotone. If phase `B` follows phase `A`, any node resolved to phase `B` is also resolved to phase
`A`.

A conforming implementation MUST behave as if the following phases exist:

* `RAW`: parser translation, source anchors, no semantic resolution.
* `IMPORTS`: module header handling, implicit prelude insertion, import/export parsing, semantic dotted-form
  disambiguation, and fixity-environment setup.
* `DECLARATION_SHAPES`: binding-group population, declaration-symbol creation, raw declaration headers, binder lists, local
  nominal identities, and raw block-scope declaration structure.
* `HEADER_TYPES`: explicit declaration-header types, supertypes, associated static members, effect-operation headers,
  record field quantities, record dependency graphs, and other header-level type information.
* `STATUS`: visibility, opacity, export status, unsafe/debug gating, instance admissibility, and modifier legality.
* `IMPLICIT_SIGNATURES`: inference of declaration result types or initializer types only for declarations whose omitted
  signatures are required by the current query, interface materialization, or a dependent declaration body.
* `BODY_RESOLVE`: call resolution, type inference, implicit-argument insertion, quantity checking, region checking, flow
  facts, pattern reachability, handler typing, body-local declaration resolution, and generation of any modality
  predicates required by an enabled modal/coeffect extension.
* `MODAL_SOLVE`: solving of predicates emitted during `BODY_RESOLVE` by any enabled modal/coeffect extension of
  §5.1.5.2, together with attachment of solved evidence where required. This phase is a no-op when no such extension is
  in use.
* `CHECKERS`: diagnostic production from resolved KFrontIR and any stored resolution errors.
* `CORE_LOWERING`: lowering of resolved KFrontIR to KCore.

An implementation MAY subdivide or fuse these phases, provided it preserves the monotone-phase invariant and the
observable results of queries, diagnostics, and KCore construction.

<!-- compiler.kfrontir.lazy_resolution_local_declarations_expansion -->
#### 17.2.3 Lazy resolution, local declarations, and expansion

In batch compilation, phases are typically executed sequentially over the whole compilation unit.

In tooling or incremental mode, phases MAY be executed lazily, per file, per declaration, or per query. A query MUST
resolve only the minimum phase required to answer it.

If a query is given an element directly and promises that element at phase `P`, clients may rely on that element being
resolved to `P`. Any declaration reached indirectly through a symbol, type, scope, provider, or imported interface may
still be at an earlier phase.

Implementations MUST therefore provide symbol- and query-level accessors that may trigger the minimum additional
resolution needed to answer the query without requiring whole-project `BODY_RESOLVE`.

Local declarations introduced by `block` or `do` are phased on demand. When `BODY_RESOLVE` first encounters a local
declaration whose header has not yet been prepared, the implementation behaves as if the prerequisite phases from
`DECLARATION_SHAPES` through `STATUS`, and `HEADER_TYPES` where needed, are run for that declaration and its lexical
container before the enclosing body continues.

A `$(...)` splice, a prefixed-string elaboration such as `type"..."`, or other elaboration-time expansion MAY likewise be delayed until a phase first
requires the expanded form. The implementation MUST preserve:

* the source anchor of the original user-written construct, and
* a synthetic-origin mapping for the expanded subtree.

<!-- compiler.kfrontir.error_tolerance_diagnostics -->
#### 17.2.4 Error tolerance and diagnostics

KFrontIR MUST be error-tolerant.

It may contain:

* missing nodes,
* error nodes,
* unresolved references,
* placeholder expressions,
* unknown type references,
* or other implementation-defined partial semantic nodes

corresponding to incomplete or invalid source programs.

Resolution is conceptually side-effect-free:

* it enriches KFrontIR with semantic information;
* it may record resolution failures inside KFrontIR nodes or in implementation-defined side tables associated with those
  nodes;
* it does not directly emit user diagnostics.

Diagnostics are produced in `CHECKERS`, or on demand from information that would be available by `CHECKERS`, and are
always attached to source origins from KFrontIR.

A conforming implementation MUST additionally support machine-readable diagnostic output in JSON.

Such diagnostic records MUST identify at least:

* diagnostic code;
* severity;
* stage or phase;
* primary origin; and
* any related origins.

A diagnostic code MUST be a stable, human-readable identifier that is unique within the implementation's diagnostic
space, and it MUST NOT consist solely of digits.
Implementations SHOULD use readable identifier forms such as `E_IMPORT_CYCLE`, `E_TYPE_MISMATCH`, or
`W_UNUSED_BINDING` rather than bare numeric codes.

When termination checking fails, the diagnostic MUST identify the recursive SCC, the recursive call edge or edges that
failed, and at least one candidate explicit `decreases` clause when the implementation can synthesize one.

If a termination-check failure is caused by an implementation limit rather than by a proved failure of the supplied
termination argument, the diagnostic MUST identify that fact explicitly.

When a standardized modal/coeffect extension is enabled and modality solving fails, the diagnostic MUST identify:

* the extension name;
* the contradictory or unsolved predicate class;
* the primary origin of each contributing obligation; and
* any related origins used to explain the conflict.

Tooling queries over syntactically incomplete files are valid. Where possible, they MUST return partial results rather
than failing wholesale merely because the surrounding file is incomplete.

<!-- compiler.kfrontir.tooling_facing_queries -->
#### 17.2.5 Tooling-facing queries

An implementation intended for editor, language-server, or refactoring use SHOULD expose queries over KFrontIR or an
equivalent abstraction.

Such queries SHOULD support at least:

* resolution of the declaration or symbol at a source position;
* navigation from reference to declaration;
* expression type and type-reference resolution at a source position;
* expected type, expected quantity, expected effect row, and any expected modality obligations where those are
  well-defined at a source position;
* emitted, solved, or unsolved modality predicates for a declaration or source position when a standardized
  modal/coeffect extension is enabled;
* diagnostics for a file or declaration, even when the file is syntactically incomplete;
* for a recursive definition, the recursive SCC, the inferred termination strategy, the canonical visible measure (if
  any), any hidden phase components, and the recursive call edges together with the proved decrease relation on each
  edge;
* inhabitance summary of the expected type or refined scrutinee type at a source position;
* enumeration of inhabitants when the summary is `Contractible` or `Finite n` and `n` does not exceed an
  implementation-defined completion threshold;
* completion candidate enumeration at a source position;
* hole-goal queries that report the expected type, local explicit context, local implicit context, and outstanding
  constraint goals for a named or anonymous hole;
* deterministic case-split generation for a binder or hole at a source position;
* missing-clause generation driven by current coverage and totality information;
* hole refinement by a user-supplied candidate term, returning either a source-preserving edit or a precise rejection
  diagnostic;
* lemma extraction from a hole or local goal, returning a source-preserving proposed declaration;
* proof / term search for a hole under the current local context, imported interfaces, and available semantic object
  store, returning deterministic candidates or edits;
* find-usages and rename keyed by declaration-symbol identity;
* type-directed search across the current project, imported interfaces, and any available semantic object store;
* lookup, navigation, and find-usages by semantic object identity after name resolution;
* browsing of interface artifacts or semantic object stores without reparsing source text;
* retrieving the canonical interface view of a module or semantic object-store entry without reparsing source text; and
* source-to-source refactoring that preserves unchanged concrete syntax and trivia where possible.

API names, packaging, and transport are implementation-defined.

An implementation that exposes editor, language-server, refactoring, or interactive-analysis features MUST use the same
dependency-tracked semantic query system for tooling and batch compilation, or an observationally equivalent slice of
that same system.

Tooling results MUST be computed from KFrontIR, KCore, module interfaces, semantic object identities, compiler
fingerprints, and query dependencies as defined in this chapter, not from a separately maintained semantic model that
may diverge from `analyze`, `check`, `emit-interface`, or `compile`.

Such an implementation MAY maintain presentation-level indexes or caches for latency, but it MUST NOT maintain a
second, independently authoritative semantic frontend whose accepted programs, name resolution, typing, effect checking,
instance resolution, or refactoring legality can differ from batch compilation.

Purely presentation-level services such as syntax coloring, formatting, outline grouping, or snippet expansion MAY use
lighter-weight mechanisms, provided they do not claim semantic authority.

<!-- compiler.kfrontir.analysis_sessions_invalidation -->
#### 17.2.6 Analysis sessions and invalidation

An analysis session is the tooling-time environment within which KFrontIR queries are valid.

An analysis session is parameterized at least by:

* the source roots and dependency closure,
* package mode vs script mode,
* the effective build configuration, including unsafe/debug flags,
* the selected backend profile and backend intrinsic set,
* and any macro-input-transcript state relevant to script-mode analysis.

Symbols, query results, and cached semantic objects are valid only within the analysis session that created them.

When any input to the session changes, the implementation MUST invalidate all affected cached results from the earliest
affected phase onward.

At minimum:

* a pure body-only change MAY preserve earlier phase results such as `DECLARATION_SHAPES`, `HEADER_TYPES`, and `STATUS`
  for unaffected declarations;
* a change to a declaration header, explicit type, supertype, import/export surface, build flag, backend profile,
  backend intrinsic set, or dependency interface MUST invalidate dependent results from the earliest phase whose inputs
  changed;
* a change to a pinned host-source identity, binding-generator version, host metadata input, native ABI description,
  selected adapter mode, marshalling profile, deployment prerequisite, generated companion artifact, trusted binding
  summary, or shim artifact MUST invalidate any generated host-binding surface, adapter realization, and all
  downstream phase results that depended on it;
* a change to macro-observed external input MUST invalidate any expansion, cached query result, or downstream phase
  result that depended on that input.

The implementation MUST determine affected results by consulting recorded query dependencies and compiler fingerprints,
not merely by coarse file-level invalidation, unless the implementation can prove that such coarse invalidation is
equivalent for the affected request.

<!-- compiler.kfrontir.query_model -->
#### 17.2.7 Query model

A conforming implementation MUST behave as if frontend analysis, elaboration, KCore construction, KBackendIR
construction, and target lowering are computed by dependency-tracked queries.

A query is identified by:

* a stable query kind;
* a stable input key;
* the effective build configuration;
* the selected backend profile and backend-intrinsic set when relevant; and
* the analysis session or compilation session in which the query is evaluated.

Query kinds are implementation-defined, but the implementation MUST behave as if at least the following conceptual
queries exist:

* parse source file;
* build KFrontIR for source file;
* advance node or declaration to phase `P`;
* compute declaration header information;
* compute declaration body resolution;
* solve modality predicates for a declaration or module when an enabled modal/coeffect extension emits any;
* compute module interface;
* compute inhabitance summary for a KCore type, declaration result type, or pattern-refined case type;
* resolve host binding module scope and compute its host-source identity;
* build or load a raw host binding surface from host metadata, a native ABI description, or a JVM-specific JNI binding
  description, together with any trusted binding summaries that affect its exported interface;
* select backend adapter mode and adapter-visible binding metadata for a raw `host.native` surface when the selected
  backend profile provides one or more adapter realizations under §17.7;
* build or load any generated companion fragments, registration tables, source-generated interop declarations, shim
  artifacts, or equivalent adapter realizations required by the selected adapter mode or by a `host.jvm.jni` binding;
* lower declaration or module to KCore;
* evaluate elaboration-time expansion or normalization request;
* lower declaration or module to KBackendIR;
* lower KBackendIR to target artifact unit.

Query rules:

* Query evaluation is conceptually pure.
  A query result depends only on its explicit inputs and on the results of other queries it demands.
* A query MUST record dependency edges to the queries and fingerprints it actually used.
* A query result MAY be memoized and reused only while all recorded dependencies remain valid.
* If a dependency changes, the cached query result is invalid from the earliest affected phase onward.
* Hidden ambient mutable state MUST NOT affect query results except insofar as it is itself modeled as an explicit query
  input or recorded transcript input.

Cycle handling:

* If query demand would create a cycle that is not already ruled out by source-language well-formedness, the
  implementation MUST detect it and report a compiler diagnostic or internal-compiler diagnostic rather than deadlocking
  or spinning indefinitely.
* The diagnostic SHOULD identify the query cycle in terms of declarations, modules, or phases when possible.

<!-- compiler.kfrontir.parallel_execution_determinism -->
#### 17.2.8 Parallel execution and determinism

A conforming implementation MAY evaluate ready queries in parallel.

A query is ready when all of its prerequisite query inputs and fingerprints are available.

Parallel execution rules:

* Query evaluation MUST be observationally deterministic.
* For fixed source inputs, dependency closure, build configuration, backend profile, backend-intrinsic set, and
  implementation version:
  * successful query results,
  * emitted interfaces,
  * KCore,
  * KBackendIR,
  * final artifacts,
  * and diagnostics
  MUST be independent of worker count, task interleaving, or scheduling policy.

In particular:

* no query may observe a partially published result of another query;
* publication of memoized query results MUST be atomic from the viewpoint of other queries;
* if several diagnostics are produced concurrently, their externally visible ordering MUST be canonical and
  deterministic;
* if several dumpable checkpoints are produced concurrently, their externally visible ordering and IDs MUST be canonical
  and deterministic.

A conforming implementation SHOULD exploit parallelism at least across:

* modules whose imported interfaces are already available and whose dependency constraints permit concurrent work;
* declarations within a module once their required header information and imported interfaces are available;
* target-lowering units whose KBackendIR inputs are ready.

An implementation MAY decline to parallelize any of these cases, but it MUST NOT introduce a semantic dependence on
serial evaluation order where the source language does not require one.

<!-- compiler.kfrontir.cancellation -->
#### 17.2.9 Cancellation

A conforming implementation MAY support cancellation of analysis, checking, elaboration, lowering, or target-lowering
requests.

If cancellation is supported:

* cancellation MUST occur only at query boundaries or at implementation-defined safe points within a query;
* a cancelled request MUST NOT publish a partially formed query result as if it were complete;
* already completed and verified subquery results MAY remain memoized and available for reuse by later requests;
* cancellation MUST NOT corrupt memo tables, persistent caches, or stage dumps already published as complete.

Cancellation behavior is otherwise implementation-defined.

<!-- compiler.kfrontir.fine_grained_source_reuse -->
#### 17.2.10 Fine-grained source reuse

An implementation intended for editor or language-server use SHOULD preserve unchanged KFrontIR subtrees, declaration
symbols, and source origins across localized source edits when doing so is sound.

In particular, a localized edit SHOULD NOT by itself require rebuilding unaffected files, unrelated declarations in the
same file, or unchanged syntactic subtrees whose containing incremental units remain valid.

This is a performance recommendation rather than a semantic requirement.

<!-- compiler.kfrontir.phase_snapshots -->
#### 17.2.11 KFrontIR phase snapshots

A KFrontIR stage dump at phase `P` MUST preserve the error-tolerant and partially resolved nature of the representation.

It MUST NOT silently normalize away:

* missing nodes;
* error nodes;
* unresolved references;
* placeholder expressions;
* unknown type references; or
* other partial semantic nodes present at that phase.

A KFrontIR dump MUST expose, for each relevant node:

* its source origin or synthetic origin;
* the highest phase reached by that node;
* declaration-symbol identity where applicable;
* resolved-reference targets where available;
* semantic annotations available by that phase; and
* unresolved obligations still pending at that phase, if any.

If a tooling query forces lazy phase advancement, an implementation MUST be able to dump both the pre-query and
post-query KFrontIR snapshots, or observationally equivalent reconstructions thereof.

<!-- compiler.kfrontir.whole_compilation_phase_order -->
#### 17.2.12 Whole-compilation phase order

Because the module dependency graph is acyclic (§2.2), whole-compilation phase order is constrained by module
topological order.

A module is **interface-ready** when the implementation has produced the module interface artifact of §17.1.9, or an
observationally equivalent in-memory representation.

A conforming implementation MUST behave as if batch compilation proceeds as follows:

1. discover the source files and module dependency graph;
2. construct `RAW` KFrontIR for every source file;
3. for each module in topological order, resolve enough of KFrontIR to make that module interface-ready.
   This includes at least `IMPORTS`, `DECLARATION_SHAPES`, `HEADER_TYPES`, `STATUS`, and only the portion of
   `IMPLICIT_SIGNATURES` required to materialize exported signatures, importable fixities, instance heads, and exported
   transparent definitional content.

   For exported top-level `let` definitions, interface readiness MUST depend on the explicit signatures required by
   §6.2, not on inferring missing exported types from bodies. Consequently, once imported interfaces and declaration
   headers are available, an implementation SHOULD make declaration-body queries for distinct top-level definitions
   independently schedulable whenever they are not part of the same explicit recursive group and share no other
   recorded dependencies.

   If an enabled modal/coeffect extension contributes interface-visible obligations, evidence, or classifications,
   interface readiness for the affected exports includes any required `MODAL_SOLVE` work for those exports.
4. downstream modules may consult only imported interfaces for ordinary import resolution, downstream typechecking,
   instance search, and definitional equality.
   Imported implementation artifacts or source bodies are not required, except for same-module multi-fragment
   compilation or the unsafe/debug facilities of §16.3;
5. once a module's required imports are interface-ready, `BODY_RESOLVE`, `MODAL_SOLVE` when applicable, `CHECKERS`, and
   `CORE_LOWERING` for that module may proceed in topological order or lazily per declaration;
6. KBackendIR lowering and target-profile lowering occur only after KCore exists for the chosen compilation roots.

An implementation MAY pipeline or parallelize these steps internally, provided the resulting behavior is as if the
ordering above had been respected.

<!-- compiler.kcore -->
### 17.3 KCore

KCore is the canonical fully resolved, post-macro core language obtained from KFrontIR after the `CORE_LOWERING` phase
of §17.2.2.

Normative role:

* KCore is the source of truth for:
  * typechecking,
  * normalization,
  * definitional equality,
  * instance-resolution results,
  * elaboration-time evaluation,
  * and the hashing rules of §15.
* KCore no longer preserves direct source structure for tooling purposes; those duties belong to KFrontIR.
* The "KCore term or module" referenced in §15 is exactly the representation defined by this section.

KCore is after:

* macro expansion;
* layout resolution and parsing;
* name resolution;
* implicit-argument insertion;
* dictionary insertion and associated-type selection;
* desugaring of:
  * `block`,
  * `do`,
  * `try`,
  * comprehensions,
  * method-call sugar,
  * expected-type-directed variant injections and widenings,
  * and lawful record reorderings;
* closure conversion of local declarations per §14.1.1.

KCore retains all compile-time structure needed by the source semantics. In particular, KCore may contain:

* intrinsic compile-time types and their inhabitants, including `Universe`, `Quantity`, `Region`, `Constraint`, row
  types, label types, universe terms, and `Type u`;
* explicit binder quantities, explicit regions, and explicit implicit binders;
* explicit capture-annotated value types over explicit `Region` binders;
* proof terms and equality evidence;
* branch assumptions and refinements introduced by §§7.4.1, 7.5.3, and 10.4.1 as explicit local proof/evidence
  bindings or equivalent refined case contexts, not merely frontend-only side facts;
* explicit handler forms, resumption quantities, completion constructors, `DoScope` frames, and exit-action scheduling
  for cleanup;
* explicit application spines aligned with Pi telescopes;
* primitive suspension type formers `Thunk` and `Need`, together with explicit suspension introduction and forcing;
* explicit stable places, scoped place borrows, first-class projector descriptors, first-class borrowed-view values, and
  opened-place packages;
* surface `projection` definitions lower to ordinary projector descriptor constants;
* surface fully applied projection calls lower to projector eliminators plus ordinary control flow, rather than
  disappearing into ad-hoc inlining alone;
* explicit `seal` nodes, manifest compile-time members, opaque compile-time members, and package/member projections;
* surface `exists` and `open ... as exists ...` do not survive as distinct KCore forms; they elaborate to ordinary
  `seal`, package projections, and local bindings over implementation-internal witness members (§5.5.11);
* explicit modality-predicate evidence introduced by any enabled modal/coeffect extension;
* local nominal family identities and their captured-argument applications as determined by §14.1.1.

KCore contains no unresolved:

* parsing obligations,
* name-resolution obligations,
* overloading obligations,
* instance-search obligations,
* unresolved modality predicates,
* or type-inference obligations.

The ownership rules of `Quantity` are fully decided before KCore is constructed. Any modal/coeffect information present
in KCore appears only as separate evidence introduced by `MODAL_SOLVE`; it does not alter the meaning of quantity
terms, quantity satisfaction, or borrow checking.

Any modality-predicate evidence introduced by an enabled modal/coeffect extension is compile-time only and
proof-irrelevant for typechecking unless that extension explicitly introduces a user-visible runtime carrier. Such
evidence is erased before KBackendIR and does not participate in definitional equality of ordinary terms or types,
except through the extension's own specified type formers.

<!-- compiler.kcore.application_spines -->
#### 17.3.1 Application spines and Pi telescopes

KCore applications are defined against ordered Pi telescopes rather than by appeal to arbitrary unary prefixes.

A conforming implementation MUST behave as if KCore contains an application form:

```text
AppSpine fn [a1, ..., ak]
```

An `AppSpine` represents one elaborated source application site. Its meaning is the ordinary left-to-right
application of `fn` to the ordered argument list, but the spine is a single KCore node for purposes of binder
alignment, dependent substitution, borrow-lifetime boundaries, and any other application-site elaboration rule
defined by this specification.

Typechecking of an `AppSpine` proceeds by repeatedly exposing the next Pi-telescope segment of the current callee,
aligning the next argument against the next binder, substituting accepted arguments through later binder types, and
continuing until the spine is exhausted or the current term ceases to have function type.

Rules:

* If the spine is exhausted while the current term still has function type, the result is a partial application over the
  residual telescope.
* If the current term ceases to have function type while arguments remain in the spine, the application is ill-typed.
* Implicit-argument insertion required by §7.3 is completed before KCore is formed, so an `AppSpine` contains the
  already elaborated ordered arguments actually applied to the callee.
* Borrow introduction, quantity satisfaction, outermost-arrow subsumption, receiver insertion, and `inout`
  rewrite-site checking are all defined over `AppSpine`, not over implementation-specific unary prefixes.
* A temporary borrow introduced while elaborating one argument of a source-level maximal application site remains live
  until elaboration of the corresponding `AppSpine` node finishes. It MUST NOT end merely because the implementation
  internally materializes a unary prefix while processing later arguments of the same source call.
* An implementation MAY store applications internally in another representation, but it MUST preserve the observable
  behavior of `AppSpine` with respect to dependent substitution, implicit-argument discharge, quantity checking, borrow
  lifetimes, receiver insertion, and `inout` rewriting.
* KCore normalization of an `AppSpine` is by ordinary left-to-right beta-reduction or an observationally equivalent
  strategy.

<!-- compiler.kcore.suspensions -->
#### 17.3.1A KCore suspensions

A conforming implementation MUST behave as if KCore contains the primitive type formers:

```text
Thunk : Type -> Type
Need  : Type -> Type
```

and primitive term forms:

```text
Delay : A -> Thunk A
Memo  : A -> Need A
Force : Thunk A -> A
Force : Need A  -> A
```

Here `Force` denotes one primitive eliminator family with these typing cases, not two unrelated primitive names.

Meaning:

* `Delay e` creates a non-memoized suspension of `e`.
* `Memo e` creates a memoized suspension of `e`.
* `Force` eliminates either suspension form.

Runtime meaning:

* `Force (Delay e)` evaluates `e` each time.
* `Force (Memo e)` evaluates `e` at most once and reuses the cached value thereafter.

Well-formedness:

* `Memo e` is permitted only when the surface `lazy`-formation restrictions of §§5.2.1 and 7.2.2 hold.
* `Delay e` and `Memo e` do not introduce a distinct family of arrow types and do not change Pi-binder identity.
* Surface `thunk expr`, `lazy expr`, and `force expr` elaborate to `Delay`, `Memo`, and `Force` respectively, or to an
  observationally equivalent internal representation.

<!-- compiler.kcore.application_spines.places_path_operations -->
##### 17.3.1.1 KCore places and path operations

A conforming implementation MUST behave as if KCore contains a place category:

```text
Place ::= PVar ident
        | PField Place label
        | PCtorField Place label
```

`Place` is not an ordinary first-class runtime value. It is a KCore addressing form used by elaboration, ownership
tracking, and explainability.

KCore additionally behaves as if it contains the following pure structural operations over places:

```text
ReadPlace : Place -> Term
MovePlace : Place -> Term
FillPlace : Place -> Term -> Term
OpenPlace : Place -> Term
```

Meaning:

* `ReadPlace p` yields the current value at `p` without by itself consuming the root.
* `MovePlace p` yields the current value at `p` and, for ownership purposes, consumes exactly the addressed path.
* `FillPlace p v` yields a rebuilt value of the root type of `p` in which the addressed subplace is replaced by `v`.
  `FillPlace` is pure structural rebuilding; it does not mutate storage.
* `FillPlace (PVar x) v` is observationally equivalent to `v`.
* `OpenPlace p` yields a value of type `Zipper R A A` when `p` selects a subvalue of type `A` inside a root of type `R`.
* `OpenPlace p` is observationally equivalent to:
  * moving the selected path `p` to obtain the focused value; and
  * packaging the corresponding write-back function that fills the same path.

Typing:

* If `p` selects a subvalue of type `A` inside a root of type `R`, then `ReadPlace p : A` and `MovePlace p : A`.
* `FillPlace p v` is well-typed when `v` has the type required for that selected subplace under the ordinary
  dependent-record / constructor-field rules; its result type is `R`.
* For record paths, the typing side conditions of `FillPlace` are the same as those of rebuilding the corresponding root
  by nested record updates under §5.5.5.
* If `p` selects a subvalue of type `A` inside a root of type `R`, then `OpenPlace p : Zipper R A A`.

Admissibility and path-state effects:

* `ReadPlace p` is well-formed only when the current path state grants non-consuming observation of `p`.
* `MovePlace p` is well-formed only when the current path state grants consuming access to `p`; after elaboration of
  that node, the addressed path is marked consumed.
* `FillPlace p v` is well-formed only when `p` is an admissible write-back target under the current path state; after
  elaboration of that node, the addressed path is restored.
* `OpenPlace p` is well-formed only when `MovePlace p` would be well-formed.
* After elaboration of that node, the addressed path is treated as consumed until the linear `fill` component of the
  resulting zipper is used or discarded under an ordinary consuming context.
* `ReadPlace` MUST NOT be used to observe a path in a way that would duplicate a linearly available subvalue.
  Non-consuming observation of such a path must instead elaborate through `WithBorrowPlace` or an observationally
  equivalent internal representation.

A `var`-bound `Ref` is not itself a `Place`. Surface constructs that admit `var`-rooted paths elaborate by an explicit
`readRef` to a fresh hidden root, then ordinary `MovePlace` / `FillPlace` on that root, followed by `writeRef` when
write-back is required.

Path-sensitive field consumption, record update filling, safe-navigation borrowed-alias propagation, and `inout`
write-back are defined in terms of these place forms or an observationally equivalent internal representation.

<!-- compiler.kcore.application_spines.scoped_borrowing -->
##### 17.3.1.2 Scoped borrowing of places

A conforming implementation MUST behave as if KCore contains a scoped borrow eliminator:

```text
WithBorrowPlace p as (ρ, x) in e
```

where:

* `p` is a stable place selecting a value of type `A`;
* `ρ : Region` is a rigid region variable fresh for the form unless supplied by the elaboration context;
* `x` is available in `e` as a borrowed alias of `p` under region `ρ`.

Operationally:

* if `p` is rooted at quantity `1`, ownership of the addressed path is suspended for the dynamic extent of `e` and
  restored on exit;
* otherwise the borrow is read-only for the dynamic extent of `e`.

Typing and escape:

* The body `e` is checked in a context where `x` is available at borrowed quantity and where `ρ` is in scope.
* Values whose types mention `ρ` may not escape the scope of the form unless `ρ` was already explicit in the surrounding
  interface.
* Exiting the body restores the caller's ownership obligations for the borrowed place.

Surface borrow introduction, borrowed local bindings, borrowed `?.` aliases, and `using`-introduced borrowed bindings
elaborate through this form or an observationally equivalent internal representation.

<!-- compiler.kcore.application_spines.projection_lowering -->
<!-- compiler.kcore.application_spines.projector_elimination -->
##### 17.3.1.3 Projector elimination

A conforming implementation MUST behave as if KCore contains the following projector eliminators:

```text
ReadProjector   proj pack
MoveProjector   proj pack
OpenProjector   proj pack
FillProjector   proj pack v
BorrowProjector proj pack as (ρ, x) in e
```

where:

* `proj : Projector Roots A`;
* `pack` is an internal place pack whose record shape matches `Roots`; and
* each field of `pack` is a stable place selecting a value of the corresponding field type of `Roots`.

The internal place pack is not a surface runtime value. It is a KCore application artifact pairing a first-class
projector descriptor with the actual stable places supplied to its `place` binders.

Meaning:

* `ReadProjector proj pack` yields the selected focus as a non-consuming read.
* `MoveProjector proj pack` yields the selected focus as a consuming move and updates path state exactly as if the
  dynamically selected leaf place had been moved directly.
* `OpenProjector proj pack` yields a `Zipper Roots A A` by opening the dynamically selected leaf place and packaging the
  corresponding write-back hole in root-pack form.
* `FillProjector proj pack v` rebuilds the root pack by filling the dynamically selected leaf place with `v`.
* `BorrowProjector proj pack as (ρ, x) in e` behaves like `WithBorrowPlace` for the dynamically selected leaf place.

Typing and admissibility:

* `ReadProjector` is admissible only when each possible yielded leaf of `proj` is admissible for `ReadPlace` under the
  current path state.
* `MoveProjector` and `OpenProjector` are admissible only when each possible yielded leaf of `proj` is admissible for
  `MovePlace` under the current path state.
* `FillProjector` is admissible only when the selected leaf is an admissible write-back target under the current path
  state and `v` has the declared focus type.
* `BorrowProjector` introduces a rigid region exactly as `WithBorrowPlace` does, and values mentioning that region obey
  the same escape rules.

Projection descriptors are pure values. Application of a projector descriptor does not mutate storage. All rebuilding is
expressed through the returned root pack or zipper fill function.

A fully applied surface projection call does not survive as a distinct KCore form.

Instead, after ordinary dependent substitution of non-`place` arguments and construction of the internal place pack for
the actual `place` arguments:

* in an ordinary non-consuming value-demanding position, the call elaborates as `ReadProjector proj pack`;
* in an ordinary consuming value-demanding position, the call elaborates as `MoveProjector proj pack`;
* in a borrow-demanding position, the call elaborates as `BorrowProjector proj pack as (ρ, x) in e`;
* under `~`, the call elaborates as `OpenProjector proj pack`, followed by the ordinary zipper-threading rewrite of
  §8.8;
* in a projection-section update, the call elaborates as `FillProjector proj pack newValue`.

If the internal place pack contains more than one root field, the rebuilt root pack produced by `FillProjector` or by
the `fill` component of `OpenProjector` is then scattered back to the corresponding actual stable roots in declaration
order. This scattering is pure structural rebuilding only.

<!-- compiler.kcore.application_spines.projection_section_update_lowering -->
##### 17.3.1.3A Projection-section update lowering

A conforming implementation MUST behave as if the surface expression

```kappa
lhs.{ (.member args...) = rhs }
```

does not survive as a distinct KCore form.

Instead, elaboration proceeds as follows:

1. Elaborate `lhs` once to a fresh hidden temporary `__root`.
2. Elaborate `rhs` once to a fresh hidden temporary `__new`.
3. Resolve `(.member args...)` exactly as a section body applied to `__root`, i.e. as the dotted form
   `__root.member args...`.
4. Require the resolved form to denote either:
   * a stable place rooted in `__root`, or
   * a fully applied projection call whose yielded stable-place alternatives are all rooted in `__root`.
5. If the resolved form is a stable place `p`, elaborate as `FillPlace p __new`.
6. If the resolved form is a fully applied projection call, elaborate it to its projector descriptor value `proj` and
   internal place pack `pack`, then elaborate the update as `FillProjector proj pack __new`.
7. If the resulting root pack contains more than one root field, scatter the rebuilt fields back to the corresponding
   actual stable roots in declaration order.

Consequences:

* `lhs` is evaluated exactly once.
* `rhs` is evaluated exactly once.
* No new runtime reference primitive is introduced.
* Projection-section update is therefore surface sugar over existing place and projector primitives.

<!-- compiler.kcore.application_spines.completion_kernel -->
##### 17.3.1.4 KCore completion and do-scope kernel

A conforming implementation MUST behave as if KCore contains an explicit completion-and-scope kernel for the control
features of Chapter 8.

For a fixed return-target context

```text
RetCtx = [(L1 : R1), (L2 : R2), ..., (Ln : Rn)]
```

KCore contains an ordinary completion family:

```text
Completion(RetCtx, A) =
    Normal A
  | Break Label
  | Continue Label
  | Return[L1] R1
  | Return[L2] R2
  | ...
  | Return[Ln] Rn
```

KCore also contains an ordinary exit-action family:

```text
ExitAction(m) =
    Deferred (m Unit)
  | Release[A] ((1 resource : A) -> m Unit) (1 resource : A)
```

KCore additionally behaves as if it contains the following structured control forms:

```text
DoScope      : DoScopeLabel -> m (Completion(RetCtx, A)) -> m (Completion(RetCtx, A))
ScheduleExit : DoScopeLabel -> ExitAction(m) -> m (Completion(RetCtx, A)) -> m (Completion(RetCtx, A))
```

The labels appearing in `DoScope` and `ScheduleExit` are already the resolved scope labels of Chapter 8. KCore contains
no unresolved label-lookup obligation.

Meaning:

* `DoScope L body` executes `body` with a fresh active scope frame labeled `L`.
* When `body` returns a completion or propagates a monadic error, the exit actions attached to that frame are executed
  exactly once in LIFO order according to §8.7.2.
* After unwinding that frame, `DoScope` propagates the resulting monadic error or completion outward unchanged.
* `ScheduleExit L act body` attaches `act` to the nearest dynamically enclosing active `DoScope` frame whose label is
  `L`, then evaluates `body`. The immediate completion behavior of `body` is otherwise unchanged.
* Surface `defer` and `defer@L` elaborate through `ScheduleExit` with a `Deferred` action.
* Surface `using` elaborates through `ScheduleExit` with a `Release` action together with the ordinary borrowed binding
  of the protected value described in §§5.1.6 and 8.7.4.
* `return`, `break`, and `continue` elaborate to ordinary constructors of `Completion(RetCtx, A)` rather than to
  distinct KCore statement forms.
* Named functions, labeled lambdas, and loops consume only the `Return[...]`, `Break`, and `Continue` constructors
  targeted at themselves and propagate all other completion constructors outward unchanged.

This subsection defines KCore structure only. A conforming implementation MAY realize these forms by CPS, exceptions,
explicit frame objects, or other equivalent internal machinery, provided the observable behavior is the same as this
kernel.

<!-- compiler.kcore.application_spines.effect_handler_kernel -->
##### 17.3.1.5 KCore effect-operation and handler kernel

A conforming implementation MUST behave as if KCore contains an explicit effect-operation and shallow-handler kernel for
the effect features of §8.1 and the runtime constraints of §14.8.

For a handled label `label`, effect interface `E`, surrounding effect row `r_all`, residual effect row `r`, and target
carrier `m : Type -> Type` such that `SplitEff r_all label E r` is solvable, KCore additionally behaves as if it
contains:

```text
OpCall        : EffLabel -> OpSymbol -> ArgSpine -> Eff r_all B
HandleShallow : EffLabel -> HandlerSpec(label, E, r_all, r, m, A, B) -> Eff r_all A -> m B

Resumption : Type -> Type -> Type
Resume     : Π (X : Type) (Y : Type). Resumption X Y -> (1 _ : X) -> Y
```

where:

```text
HandlerSpec(label, E, r_all, r, m, A, B) =
  { onReturn : A -> m B
  , onOp[op_i] :
        Π (x1 : A1) ... (xn : An).
          (q_i k : Resumption B_i (Eff r_all A)) ->
          m B
      for each declared operation
      q_i op_i : Π (x1 : A1) ... (xn : An). B_i
      of E
  }
```

`OpSymbol` denotes the resolved semantic identity of an operation declared by `E`. `ArgSpine` is the fully elaborated
argument spine for that operation.

Meaning:

* `OpCall label op args` performs the resolved operation `op` at effect label `label`.
* The nearest dynamically enclosing `HandleShallow` for that exact effect-label value intercepts that `OpCall`.
* In the matching operation clause, the bound continuation `k` is a primitive KCore resumption object representing the
  captured computation suffix from the `OpCall` site to that handler boundary, exactly as constrained by §14.8.
* Applying `Resume k` to a resume payload resumes that captured suffix.
* The resumption-use annotation of `k` is the declared resumption quantity `q_i` of the intercepted operation and is
  enforced by §§8.1.7A, 8.1.7B, and 8.1.8 rather than by ordinary binder-quantity satisfaction alone.
* The resume payload accepted by `Resume k` is always bound at quantity `1`. Multi-shotness therefore reuses or
  duplicates the resumption value `k`, not a single resume payload.
* `HandleShallow` handles only the selected label and eliminates into the single target carrier `m`. Operations at all
  other labels propagate outward unchanged inside resumed computations.
* A `HandleShallow` return clause is applied only when the handled computation completes normally.
* If the handled computation is itself completion-carrying, for example of type `Eff r_all (Completion(RetCtx, A))`,
  then propagation of `Break`, `Continue`, and `Return[...]` is determined by the surface elaboration rules of
  §§8.1.9.1 and 8.1.10.1 rather than by any extra KCore control primitive.
* The captured continuation boundary includes any active `DoScope` frames inside that dynamic segment, so exit actions
  are cloned or consumed exactly as specified by §§8.1.8.1, 8.7.2, and 14.8.

Surface `handle label expr with ...` elaborates through `HandleShallow`.

Surface `deep handle label expr with ...` does not require a distinct KCore primitive. It elaborates to an internal
recursive driver over `HandleShallow` as specified in §8.1.10.

This subsection defines KCore structure only. A conforming implementation MAY realize these forms by CPS, exceptions,
heap-allocated frames, stack copying, segmented stacks, defunctionalized state machines, or other equivalent internal
machinery, provided the observable behavior is the same as this kernel and the runtime constraints of §14.8.

<!-- compiler.kcore.application_spines.kcore_nominal_scope_binders -->
##### 17.3.1.5A KCore nominal-scope binders

A conforming implementation MUST behave as if KCore contains an internal compile-time type:

```text
NomScope : Type0
```

and a compile-time-only binder:

```text
FreshNomScope σ in e
```

where `σ : NomScope` is rigid within `e`.

Rules:

* `FreshNomScope` is lexical, not runtime-generative.
* Distinct `FreshNomScope` binder occurrences introduce distinct rigid nominal-scope tokens.
* Distinct dynamic evaluations of the same enclosing function, lambda, or block do not create distinct nominal-scope
  tokens.
* Each source `block` and each source `do` scope elaborates as if wrapped in a fresh `FreshNomScope` binder.
  Implementations MAY omit the binder when no `scoped` local declaration in that scope depends on it.
* A scoped local declaration of §6.3.1.1 captures the nearest enclosing nominal-scope token as part of its
  closure-converted capture telescope.
* `NomScope` values are compile-time only and are erased before KBackendIR.

<!-- compiler.kcore.application_spines.intrinsic_compile_time_types -->
##### 17.3.1.6 KCore intrinsic compile-time types

A conforming implementation MUST behave as if KCore supports ordinary binding, projection, application, sealing, and
packaging of inhabitants of the intrinsic compile-time types of §5.1.3 and of the elaboration-time reflection types of
§5.8.5.

Rules:

* `Universe`, `Quantity`, `Region`, `Constraint`, `RecRow`, `VarRow`, `EffRow`, `Label`, `EffLabel`, and the internal
  nominal-scope type `NomScope` are ordinary compile-time types in KCore.
* `CoreCtx`, `Symbol`, `Core Γ t`, and `CoreEq x y` are ordinary elaboration-time-only compile-time types in KCore.
* `Type u` is a primitive universe family indexed by `u : Universe`; it is not required to be represented as ordinary
  first-order application.
* `Core Γ t` values are well-scoped and well-typed by construction. Public reflection operations MUST preserve that
  invariant.
* Compile-time binders, fields, and package members may carry any quantity annotation otherwise admitted by source or
  KCore.
* Erasure of compile-time terms is classifier-based under §§5.1.4 and 14.4.
* Raw terms of type `Constraint` are descriptors only. Coherent evidence for a concrete descriptor `C` is separate and
  is reified explicitly by `Dict C`.
* The reflection operations of §5.8.5 MAY be realized as KCore intrinsics or as observationally equivalent
  elaboration-time evaluator primitives.
* Reflection values MUST NOT survive lowering to KBackendIR except through ordinary reification to `Syntax` followed by
  ordinary elaboration.

<!-- compiler.kcore.application_spines.capture_annotated_types -->
##### 17.3.1.7 KCore capture-annotated types

A conforming implementation MUST behave as if KCore contains an explicit capture-annotation former:

```text
Captures(s1, ..., sn, T)
```

written in this specification as:

```text
T captures (s1, ..., sn)
```

where each `si : Region` is already in scope.

Meaning:

* `T captures (s̄)` classifies values of ordinary value type `T` whose hidden region environment is contained in the set
  `{s̄}`.
* The annotation is compile-time only and is erased before KBackendIR.
* The empty capture set is represented explicitly in KCore, though surface syntax may omit it by inference.
* Capture annotations are part of elaborated exported types and replace any separate hidden region-environment summary
  in interface artifacts.
* KCore closure conversion, package construction, and interface emission propagate these annotations structurally by
  taking unions of hidden region environments and checking containment where an explicit annotation is written.
* When a source-visible handler clause binds a resumption `k`, any ordinary function-shaped source type used to
  describe that `k` carries the capture annotation induced by the captured continuation boundary of §14.8.5.
* KCore typing, interface emission, and escape checking MUST propagate that annotation in the same way they propagate
  capture annotations for closures.
* Anonymous rigid regions never appear in `Captures(...)`; any attempt to form such an annotation is rejected earlier by
  skolem-escape checking.

A conforming implementation MAY realize this with another internal representation, provided ordinary downstream
typechecking, interface emission, definitional equality, and erasure are observationally equivalent to the rules above.

<!-- compiler.kcore.application_spines.branch_refinement_evidence -->
##### 17.3.1.8 KCore branch and refinement evidence

A conforming implementation MUST behave as if KCore contains explicit erased evidence for branch-local boolean and
constructor refinements.

Internal compile-time type and proof families:

```text
CtorTag   : Type
HasCtor   : forall (@0 a : Type). a -> CtorTag -> Type
LacksCtor : forall (@0 a : Type). a -> CtorTag -> Type
```

Every constructor declaration `C` determines a canonical internal tag constant:

```text
⟨C⟩ : CtorTag
```

Rules:

* `HasCtor v ⟨C⟩` is proof-irrelevant erased evidence that the top-level constructor of `v` is `C`.
* `LacksCtor v ⟨C⟩` is proof-irrelevant erased evidence that the top-level constructor of `v` is not `C`.
* `HasCtor` evidence may be introduced only by successful elaboration of a constructor test or constructor branch.
* `LacksCtor` evidence may be introduced only by the failing branch of a constructor test or by a residual branch after
  excluding one or more top-level constructor branches as specified in §7.5.4.
* Refinement evidence is stable under equality transport. If the local erased context contains `@p : x = y`, then the
  implementation MUST behave as if `HasCtor x t` and `HasCtor y t` are inter-derivable, and likewise `LacksCtor x t`
  and `LacksCtor y t`.
* A stable-alias binding of §7.4.3 MAY therefore be realized purely by erased equality evidence plus ordinary transport;
  no separate runtime representation is required.
* Branch refinement is observationally non-consuming. Introduction or use of branch-local refinement evidence does not
  by itself discharge any quantity obligation of the refined subject.
* These proof terms are compile-time only and are erased before KBackendIR.
* Constructor-field projection, reachability, and branch-local refinement consume this evidence; they do not depend on
  hidden implementation-defined refinement state.

Boolean branch evidence continues to use ordinary propositional equality:

* `if b then t else f` introduces branch-local erased evidence `b = True` / `b = False`.
* `match b` on boolean constructors behaves likewise.

<!-- compiler.kcore.provenance_explainability -->
#### 17.3.2 KCore provenance and explainability

Every synthetic KCore node introduced during elaboration MUST carry provenance sufficient to explain why it exists.

At minimum, provenance MUST relate each synthetic KCore node to:

* one or more KFrontIR origins; and
* an introduction kind.

Introduction kinds are implementation-defined, but MUST distinguish at least:

* desugaring;
* macro expansion;
* inserted implicit argument or dictionary;
* inserted coercion or widening;
* borrow introduction;
* record reordering;
* place lowering / path restoration;
* projector elimination;
* projection-section update lowering;
* branch-refinement lowering;
* completion-kernel lowering;
* multi-return / join-point lowering;
* effect-operation lowering;
* handler-kernel lowering;
* deep-handler driver insertion;
* exit-action scheduling;
* safe-navigation or section lowering; and
* local-declaration closure conversion.

A KCore dump MUST expose this provenance.

An implementation SHOULD provide an explain query that, given a source span or KCore node, returns the provenance chain
from KFrontIR through KCore.

<!-- compiler.kcore.elaboration_time_evaluation -->
#### 17.3.3 Elaboration-time evaluation

The elaboration-time evaluator operates on KCore or an observationally equivalent internal representation.

It is used for:

* macro execution;
* elaboration of prefixed-string handlers such as `type"..."`;
* compile-time reflection over `Syntax`;
* semantic reflection over `Core`;
* any other elaboration-time computation required by this specification.

Elaboration-time evaluation is backend-independent except insofar as the effective build configuration exposes
elaboration-available backend intrinsics under §17.6.1.

If elaboration-time evaluation encounters:

* an unsatisfied backend intrinsic,
* a backend intrinsic that is not elaboration-available, or
* a foreign capability with no compile-time meaning,

then compilation fails.

The evaluator MAY memoize, partially normalize, or reuse cached results, provided the observable result is the same as
semantic elaboration-time evaluation under the restrictions of §5.8.6.

<!-- compiler.kcore.semantic_object_identities -->
#### 17.3.4 Semantic object identities

After `CORE_LOWERING`, each semantic object that may be imported, browsed, documented, cached, compiled, or mentioned by
a module interface artifact has a canonical semantic object identity.

Semantic objects include at least:

* term definitions and declarations;
* type definitions and aliases;
* constructors;
* traits and associated static members;
* effect interfaces and effect operations;
* instances;
* escaped local nominal families that appear in exported signatures, exported compile-time members, transparent
  definitional content, or other interface-visible semantic artifacts; and
* any other implementation-defined top-level semantic artifact that is imported, browsed, or separately cached.

A semantic object identity is distinct from:

* user-visible names,
* binding-group entries,
* source file paths, and
* source locations.

Names are metadata that bind human-facing paths to semantic object identities.

A rename, move, or re-export change that preserves the semantic object identity of an object does not change that
object's identity, though it may change source-level name resolution outcomes and interface fingerprints where exported
bindings change.

An implementation MAY realize semantic object identities using the hashes of §15 or another stable identity scheme whose
equality is at least as fine as KCore semantic identity.

Tooling and caches that operate after name resolution SHOULD prefer semantic object identity to source spelling when
tracking references, rename targets, usages, deduplication, and compiled artifact reuse.

<!-- compiler.kcore.semantic_object_identities.escaped_local_nominal_family_identities -->
##### 17.3.4.1 Escaped local nominal family identities

An escaped local nominal family is a local nominal declaration from §14.1.1 whose family identity, or an application of
that family, appears in a module interface artifact.

Canonical family identity:

Each escaped local nominal family has a canonical family identity determined by:

* the semantic object identity of the enclosing top-level semantic object after closure conversion;
* the stable declaration-site path of the local declaration within that enclosing object; and
* the nominal kind of the declaration (`data`, `type`, `trait`, `effect`, constructor family, or other
  implementation-defined nominal kind).

Rules:

* Renaming local binders, reformatting, or changing unrelated declarations does not change the family identity.
* Changing the enclosing top-level semantic object, the declaration-site path, or the nominal kind does change the
  family identity.
* Distinct dynamic evaluations do not create fresh family identities.

Applications:

* A reference to an escaped local nominal in an exported type or compile-time term is represented as an application of
  the family identity to the closure-converted captured arguments.
* Equality of two escaped local nominal applications is determined by:
  * equality of their family identities; and
  * definitional equality of their corresponding captured arguments.

Interface representability:

* A value of type `NomScope` is never interface-representable.
* Accordingly, any would-be exported application of a scoped local family that captures a `NomScope` token is a
  compile-time error unless the exported interface eliminates or hides that family entirely.
* A captured argument of an escaped local nominal family is interface-representable iff it can itself be encoded by the
  module interface artifact using the ordinary exported type language, compile-time terms, semantic object identities,
  and transparent definitional content available under §17.1.9.
* If an escaped local nominal family would require a captured argument that is not interface-representable, export is a
  compile-time error in the defining module.

An implementation MAY realize escaped local nominal family identities using hashes or another stable identity scheme,
provided equality is at least as fine as the semantic equality above.

<!-- compiler.kcore.semantic_object_stores -->
#### 17.3.5 Semantic object stores

A conforming implementation MAY persist semantic objects in a structured semantic object store or codebase.

If provided, such a store is keyed by semantic object identity together with any required compiler fingerprints,
backend profile, backend-intrinsic set, and implementation version information.

Rules:

* semantic objects and the human-facing names that refer to them MUST be stored separately or be separately recoverable;
* multiple names MAY refer to the same semantic object identity;
* deleting, moving, or renaming a name binding does not by itself require duplicating the underlying semantic object;
* browsing and querying the store MUST NOT require reparsing original source text; and
* the store MAY be append-only, garbage-collected, versioned, or otherwise implementation-defined.

A module-interface artifact of §17.1.9 MAY be materialized directly from such a store, embedded within it, or
reconstructed from it on demand.

<!-- compiler.kcore.inhabitance_summaries -->
#### 17.3.6 Inhabitance summaries

Implementations MUST behave as if KCore exposes a conservative inhabitance-summary query on types and on case-refined
types.

```text
InhabitanceSummary ::= Empty
                     | Contractible
                     | Finite nat
                     | Unknown
```

Canonicalization:

* `Finite 0` is identical to `Empty`.
* `Finite 1` is identical to `Contractible`.
* `Finite n` is used only for `n >= 2`.

Meaning:

* `Empty` means the analyzed type has no inhabitants.
* `Contractible` means the analyzed type is known to have a unique inhabitant within the observational distinctions
  relevant to the current compilation phase.
* `Finite n` means the analyzed type has exactly `n` inhabitants.
* `Unknown` means the implementation does not establish a stronger fact.

Required structural fragment:

* The required fragment is the normalized closed first-order algebraic fragment of KCore.
* At minimum, implementations MUST compute exact summaries for:
  * `Void`, `Unit`, `Bool`, and any closed finite nullary-constructor data type;
  * closed sums and closed variants, by exact addition of already summarized arms;
  * closed nondependent products and nondependent records, by exact multiplication of already summarized components;
  * closed dependent record telescopes are not part of the required exact fragment unless each later field summary is
    determined structurally from earlier singleton refinements without leaving the required fragment; otherwise the
    required result is `Unknown`;
  * refined case fibers whose emptiness or singleton-ness already follows from ordinary branch refinement, definitional
    equality, and index unification;
  * runtime-relevant shapes obtained after erasing compile-time-only and quantity-`0` constituents when the query is
    requested for representation selection.
* For function types, recursive types, higher-rank polymorphism, open rows, opaque definitions outside the current
  transparency environment, and any other unsupported shape, the required result is `Unknown`.

Termination and conservatism:

* The required fragment MUST be implemented by a terminating structural procedure over normalized KCore.
* The implementation MUST NOT return `Empty`, `Contractible`, or `Finite n` unless that result is correct.
* `Unknown` is always permitted.

Permitted strengthening:

* An implementation MAY apply additional sound rules to return a stronger summary than the required fragment would
  produce.
* Such rules MAY use bounded normalization, bounded rewriting, bounded finite-domain instantiation, variance tests,
  representable/container decompositions, Yoneda-style simplifications, or similar techniques.
* Exceeding any implementation-defined bound, encountering exact-cardinality arithmetic overflow, or reaching an
  unsupported proof obligation during such strengthening MUST yield `Unknown`, not a compile-time error, unless some
  other rule of this specification already requires rejection of the program.

Additional restriction:

* An implementation MUST NOT conclude `Contractible` merely because a type is proposition-shaped or is commonly used as
  proof. Ordinary inhabitants of `P : Type` remain observationally distinct unless uniqueness is established within the
  supported fragment. Only coherent `Constraint` evidence follows the built-in proof-irrelevance rule of §5.1.3.

Source-acceptance stability:

* Acceptance of `match` exhaustiveness and `impossible` branches MUST NOT rely on any strengthening rule beyond the
  required structural fragment.
* Optional strengthening rules MAY affect diagnostics, completion, optimization, and code generation, but they MUST
  remain conservative.

<!-- compiler.kbackendir -->
### 17.4 KBackendIR

KBackendIR is the target-neutral runtime IR obtained by erasing KCore and choosing runtime representations.

Normative role:

* KBackendIR is the last backend-independent representation.
* Every standardized backend profile lowers from KBackendIR, or from an internal representation observationally
  equivalent to it.

KBackendIR is after:

* all erasure mandated by §14.4;
* discharge of quantity and region obligations;
* representation selection for:
  * closures,
  * data,
  * variants,
  * records,
  * dictionaries,
  * refs,
  * fibers,
  * supervision scopes,
  * TVars,
  * STM journals,
  * handlers,
  * resumptions,
  * strings,
  * bytes,
  * arrays,
  * maps,
  * sets,
  * and numeric types;
* lowering of source-level abrupt control to explicit runtime control and cleanup operations.

KBackendIR contains only runtime-relevant constructs. In particular, it contains:

* runtime values and runtime type tags;
* functions, closures, environments, and calls;
* basic blocks and explicit control transfer, or an observationally equivalent structured-control form;
* heap allocation and field access;
* explicit tagged-data and tagged-variant construction / elimination;
* explicit mutable-cell operations;
* explicit fiber handles, fiber-id values, fiber-label state, supervision scopes, monitor handles,
  fiber-local-state cells, promise objects, timer registrations / timer queues, TVars, STM journals,
  handler frames, resumption objects, cleanup-scope operations, and error-propagation operations.

The following do not appear in KBackendIR except insofar as they are explicitly reified by a library or backend
intrinsic:

* intrinsic compile-time types and their inhabitants, including universes, `Type u`, row terms, label terms, raw
  `Constraint` descriptors, and anonymous or explicit region variables;
* nominal-scope tokens introduced by `FreshNomScope`;
* capture-annotation structure introduced by `captures (...)`;
* quantity-`0` computational binders or fields;
* proof terms whose only purpose is compile-time reasoning;
* erased indices of dependent data.

<!-- compiler.kbackendir.conceptual_lowering_stages -->
#### 17.4.1 Conceptual lowering stages

A conforming implementation MUST behave as if lowering from KCore to KBackendIR performs the following conceptual
stages:

* `ERASURE`:
  eliminate or discharge compile-time-only intrinsic type inhabitants, quantity-`0` computational binders, proof-only
  terms, and erased indices in accordance with §14.4.

* `INHABITANCE`:
  compute inhabitance summaries for runtime-relevant algebraic shapes, refined case fibers that survive to runtime,
  handler/resumption argument shapes, and other types consulted by representation selection. Results are conservative and
  may be `Unknown`.

* `CONTROL_LOWERING`:
  lower source-level abrupt control, completion-carrying control, handlers, resumptions, `defer`, `using`, `try`, and
  related constructs to explicit runtime control and cleanup operations.

* `LOCAL_CONTROL_PROTOCOL_LOWERING`:
  lower finite non-escaping local control protocols such as `Completion`, `Match`,
  projection/projector-elimination control, and implementation-generated boolean /
  constructor-refinement / case-split joins to internal arm graphs, join points, or
  multi-return form as permitted by §17.4.7A.

* `ENVIRONMENT_LOWERING`:
  closure-convert lambdas and local declarations, and choose runtime environment layouts for captured values.

* `REPRESENTATION_SELECTION`:
  choose runtime representation classes, layouts, and tag encodings for data, variants, records, dictionaries,
  numerics, strings, bytes, collections, refs, fibers, fiber-id values, fiber-label state, supervision scopes,
  monitor handles, fiber-local-state cells, promises, timer registrations / timer queues, TVars, STM journals,
  handlers, and resumption objects.

* `CALL_LOWERING`:
  fix runtime calling conventions, retained-dictionary passing, entrypoint signatures, and ABI-neutral parameter/result
  conventions.

* `RUNTIME_CONTROL_FORM`:
  produce basic blocks and explicit control transfer, or an observationally equivalent structured runtime form.

An implementation MAY fuse, split, reorder, or partially overlap these conceptual stages, provided the resulting
KBackendIR is observationally equivalent to one produced by the staged lowering above.

<!-- compiler.kbackendir.uses_inhabitance_summaries -->
#### 17.4.1A Uses of inhabitance summaries

A conforming implementation MAY use `Empty`, `Contractible`, and small exact `Finite n` summaries for
runtime-representation choices and dead-code elimination.

Permitted uses include:

* eliminating branches whose refined runtime-relevant type is `Empty`;
* omitting storage for contractible record fields, constructor payloads, variant payloads, or residual results, provided
  all source-language observations behave as if the unique value were present;
* choosing compact layouts for small finite enums or variants;
* for `Variant` / union layouts, any compact finite representation MUST preserve the stable member-identity rules of the
  variant typing and runtime-representation sections; in particular, an implementation MUST NOT make row-local tag
  ordinals observable and MUST NOT require retagging for zero-cost widening;
* simplifying handler or resumption calling conventions when an argument or residual result is contractible.

`Unknown` carries no semantic information and MUST NOT be treated as `Empty`, `Contractible`, or finite.

These optimizations MUST NOT change any externally specified data layout, calling convention, or FFI surface promised
by a backend profile.

<!-- compiler.kbackendir.lowering_legality_checkpoints -->
#### 17.4.2 Lowering legality checkpoints

At or before publication of KBackendIR, the implementation MUST validate that:

* every declaration and body selected for runtime lowering is fully resolved;
* no unerased `Type`, universe term, row term, anonymous region, quantity-`0` binder, or proof-only term remains unless
  it has been explicitly reified as an ordinary runtime value;
* every call, closure, and entrypoint has a fixed runtime arity and runtime calling convention;
* handler, resumption, cleanup, and error-propagation structures satisfy the runtime obligations of Chapters 8, 9, and
  14;
* scheduler, timer, promise, fiber-identity, fiber-label, and synchronization structures satisfy the runtime
  obligations of Chapters 8, 14, and 17;
* any internal multi-return or join-point lowering of `Completion`, `Match`, projection/projector-elimination control,
  or implementation-generated boolean / constructor-refinement / case-split control satisfies §17.4.7A;
* data, variant, and record layout choices are fixed consistently with §§14.5-14.6;
* retained dictionaries and backend intrinsics have concrete runtime representations, or else compilation is rejected
  before target lowering;
* the compiled roots and exported runtime entrypoints are explicit.

Failure of any of these checks is a compile-time error.
A conforming implementation MUST NOT silently pass malformed or partially lowered runtime IR to target-profile lowering.

<!-- compiler.kbackendir.graph_dumps_legality_witnesses -->
#### 17.4.3 KBackendIR graph dumps and legality witnesses

A KBackendIR stage dump MUST be graph-capable.

At minimum it MUST represent:

* functions and entrypoints;
* basic blocks, control regions, or an observationally equivalent control graph;
* control-flow edges or equivalent successor structure;
* if internal multi-return or join-point lowering is used, the corresponding arm graph together with its mapping to
  the originating control alternatives, including:
  * source-level `Completion` / `Match` alternatives when those are the source representation;
  * projection yielded-leaf alternatives for projector-elimination control; and
  * success / failure or case-split alternatives for implementation-generated boolean or constructor-refinement join
    points;
* if the implementation classifies a lowered transfer as tail, super-tail, or semi-tail under §17.4.7A, the dump
  SHOULD expose that classification;
* values and def-use or binding-use relationships;
* fiber handles, fiber identities, fiber-label state, supervision scopes, monitor handles, fiber-local-state cells,
  promise objects, timer registrations / timer queues, TVars, STM journals, handler frames, resumption objects,
  cleanup scopes, and error-propagation structure;
* selected runtime representation classes and calling-convention facts relevant to debugging; and
* provenance links back to KCore.

If the implementation internally uses a structured form rather than explicit basic blocks, the dump MUST still expose
enough information to reconstruct control flow and unwinding shape for debugging purposes.

The KBackendIR verifier MUST be user-invokable.

A successful verification MAY be recorded in the dump as a verifier stamp or equivalent witness.

<!-- compiler.kbackendir.post_kbackendir_target_lowering_dumps -->
#### 17.4.4 Post-KBackendIR target-lowering dumps

If a backend profile exposes post-KBackendIR intermediate forms, each such form MUST participate in the same
observability model:

* checkpoint naming;
* dumpability;
* version identification;
* provenance back to KBackendIR; and
* verifier access where the form has a verifier.

If the post-KBackendIR form is not naturally serialized as `json` or `sexpr`, the implementation MUST still provide a
`json` and `sexpr` manifest describing that form, its identity, and its relation to the surrounding checkpoints.

<!-- compiler.kbackendir.interpretation -->
#### 17.4.5 KBackendIR interpretation

An implementation MAY provide a KBackendIR interpreter for script execution, REPL use, tests, or debugging.

If it does:

* the interpreter executes only KBackendIR that has passed the legality checks of §17.4.2;
* interpreted execution is governed by the same selected backend profile and backend-intrinsic set as compiled
  execution;
* interpreted execution MUST be observationally equivalent to compiled execution under that same configuration.

A KBackendIR interpreter MAY retain extra source metadata for debugging, profiling, or diagnostics, provided that
metadata does not change program behavior.

<!-- compiler.kbackendir.representation_classes_specialization -->
#### 17.4.6 Representation classes and specialization

Implementations MAY monomorphize, dictionary-pass, closure-convert, or use any hybrid strategy consistent with
KBackendIR semantics.

If a declaration is specialized, specialization MUST be keyed only by runtime-relevant representation.

Erased indices, quantity-`0` arguments, proofs, row arguments, region arguments, and other compile-time-only
distinctions MUST NOT by themselves force distinct runtime specializations.

Two KCore terms whose erased calling convention and chosen runtime representation classes are definitionally equal MAY
share a single KBackendIR body.

A change that does not alter the erased calling convention, selected runtime representation classes, or other
`BackendFingerprint(profile)` inputs SHOULD preserve previously computed KBackendIR and target-lowering results where
possible.

<!-- compiler.kbackendir.runtime_calling_convention -->
#### 17.4.7 Runtime calling convention

At KBackendIR boundaries:

* quantity-`0` parameters are absent;
* implicit parameters erased by §14.4 are absent;
* retained dictionaries appear as ordinary runtime parameters;
* owned, borrowed, and unrestricted source binders do not induce distinct runtime calling conventions by themselves once
  the program has passed the quantity and region checks, unless a backend explicitly exposes a backend-specific ABI for
  them.
* internal multi-return / join-point arms, when used, are backend-local control channels only and are not part of the
  portable cross-module or foreign calling convention;

This section does not change source typing. It specifies only the backend-neutral runtime view after elaboration.

<!-- compiler.kbackendir.internal_multi_return_control_protocol_lowering -->
#### 17.4.7A Internal multi-return control-protocol lowering

A conforming implementation MAY lower a finite local control protocol to an internal multi-return calling convention or
to an observationally equivalent join-point representation.

Eligible protocols include at least:

* completion-carrying control over `Completion(RetCtx, A)`;
* residue-threading control over `Match a r`;
* projection/projector-elimination control whose leaves terminate in the place or projector eliminators of
  §§17.3.1.1 and 17.3.1.3;
* implementation-generated boolean, constructor-refinement, or case-split join points, including lowering of ordinary
  `if`, `match`, and union/data-constructor dispatch when no unnecessary intermediate boolean or tag value is
  materialized;

Rules:

* Each arm of the lowered protocol is internal and has a fixed correspondence to one source-level alternative.
* The payload carried by an arm, if any, is exactly the payload of its corresponding source-level alternative.
* The lowering MUST preserve the typing, evaluation order, and ownership behavior of the source-level form from which it
  was derived.
* The lowering MUST NOT expose first-class return points, first-class join continuations, or backend-local arm
  identities at source level.
* A captured handler resumption of §§8.1.8-8.1.10 is not itself a finite local control protocol of this subsection.
  It remains a captured resumption value governed by §§14.8.5-14.8.9 and §17.3.1.5.
  Only finite local control that occurs inside, around, or after such a resumption may be lowered under this
  subsection.
* The lowering MUST NOT change the observability of `defer`, `using`, handler unwinding, resumption reuse, or error
  propagation.
* If a lowered transfer introduces no new local arm handlers and preserves every still-live caller arm by references to
  existing caller arms, the implementation MAY realize it as a tail transfer.
* If a lowered transfer introduces no new local arm handlers and preserves only a strict subset of the still-live caller
  arms, the implementation MAY realize it as a super-tail transfer.
  An implementation MAY realize such a transfer by dropping discarded dynamic control state before transfer, but only
  when every discarded arm is unreachable or every unwind obligation associated with it has already been discharged.
* A lowered transfer that installs one or more new local arm handlers while also preserving one or more caller arms is a
  semi-tail transfer.
* Permutation or duplication of preserved caller arms does not by itself disqualify a tail, super-tail, or semi-tail
  realization, provided the observable control protocol remains that of the source semantics.
* If a lowered protocol is used inside a multi-shot resumption, each reuse MUST still satisfy §§8.1.8.1 and 14.8:
  control state inside the captured segment is logically cloned per resumption application, while general heap state
  remains shared unless the program copies it explicitly.

These terms name implementation strategies only. They do not add new surface syntax or new source typing rules.

<!-- compiler.kbackendir.incremental_reuse_across_lowering_stages -->
#### 17.4.8 Incremental reuse across lowering stages

A conforming implementation MAY persist and reuse previously verified results of KCore construction, KBackendIR
construction, and target lowering.

Reuse rules:

* A KCore unit MAY be reused when:
  * its own required frontend-query results remain valid, and
  * all recorded imported-interface dependencies remain valid.

* A KBackendIR unit MAY be reused when:
  * its originating KCore unit remains valid,
  * its representation-selection inputs remain valid, and
  * its `BackendFingerprint(profile)` remains unchanged.

* A target-lowered unit MAY be reused when:
  * its originating KBackendIR unit remains valid, and
  * all target-profile-specific lowering inputs remain unchanged.

An implementation MUST NOT reuse a cached result whose recorded dependencies are invalid.

A successful reuse is observationally equivalent to recomputing the unit from its inputs under the current
implementation version and effective build configuration.

A conforming implementation SHOULD support persistent caches across compiler-process restarts for at least:

* module-interface units,
* KCore units,
* KBackendIR units, and
* target-lowering units,

provided the reuse conditions above are satisfied.

<!-- compiler.kbackendir.implementation_defined_intermediate_forms -->
#### 17.4.9 Implementation-defined intermediate forms

An implementation MAY introduce additional named intermediate representations between KCore, KBackendIR, and target
lowering.

Such intermediate forms are implementation-defined and are not part of the portable subset unless standardized
elsewhere in this specification.

If an implementation exposes such an intermediate form through stage dumps, pipeline traces, or debugging APIs, it MUST
also expose:

* a stable checkpoint or step name for that form;
* a machine-readable dump for that form;
* a verifier or legality check for that form when such a notion exists; and
* provenance relating that form to the adjacent standardized checkpoints.

Implementations SHOULD avoid introducing an additional intermediate form unless it has a distinct semantic,
optimization, representation, or deployment purpose.

<!-- compiler.runtime -->
### 17.5 Portable runtime obligations

A backend MUST preserve all observable source semantics of Chapters 4, 8, 9, 10, 14, 15, and 16.

In particular, a backend MUST preserve:

* floating-point equality and ordering as specified by §4.1.3;
* `Char` as a Unicode scalar value;
* the distinction between `String` and `Bytes`;
* the stable member-type tag identity requirements of §14.5;
* the record-canonicalization and path-sensitive consumption rules of §§5.5 and 14.6;
* the evaluation-count guarantees of §10.10;
* the cleanup, `defer`, `using`, error, and abrupt-control rules of §§8.6-8.7 and §9;
* the shallow/deep-handler and resumption rules of §§8.1.8-8.1.10.

Host-runtime features such as garbage collection, exceptions, stack unwinding, coroutines, JIT compilation, AOT
compilation, or dynamic linking are implementation techniques only. They do not redefine Kappa semantics.

<!-- compiler.runtime.memory_management -->
#### 17.5.1 Memory management

Memory-management strategy is implementation-defined.

A backend MAY use tracing garbage collection, reference counting, arena allocation, stack allocation, region-like
runtime disciplines, manual runtime-managed heaps, or any hybrid thereof, provided the observable behavior of the
program remains that of the source semantics.

In particular:

* `defer`, `using`, and `MonadFinally` obligations are language-level semantics and MUST NOT be weakened into host
  finalizer behavior;
* no backend may require prompt garbage-collection finalization to realize source-level resource release.

<!-- compiler.intrinsics -->
### 17.6 Backend intrinsics and `expect`

A backend profile MAY provide intrinsic definitions that satisfy `expect` declarations of §6.5.

A backend intrinsic is one of:

* a definition whose body is modeled directly in KCore;
* a primitive introduced at or after KBackendIR lowering; or
* a target-platform binding provided by the selected backend profile.

Rules:

* A backend intrinsic MUST satisfy the expected signature exactly up to definitional equality.
* A backend intrinsic MUST have a stable implementation identity recorded in the build artifact. Any hashing, caching,
  or coherence decision that depends on that intrinsic MUST include that identity in its effective input.
* If the selected backend profile does not supply a required intrinsic, the corresponding `expect` remains unsatisfied
  and compilation fails.

<!-- compiler.intrinsics.elaboration_available_backend_intrinsics -->
#### 17.6.1 Elaboration-available backend intrinsics

A backend intrinsic MAY additionally be classified as **elaboration-available**.

Only elaboration-available backend intrinsics may be called, unfolded, or otherwise demanded by the elaboration-time
evaluator of §17.3.3.

Whether a backend intrinsic is elaboration-available is part of the backend-intrinsic set and therefore part of the
effective build configuration and analysis-session identity.

If elaboration-time evaluation reaches a backend intrinsic that is not classified as elaboration-available, compilation
fails.

<!-- compiler.intrinsics.frontend_backend_extension_boundary -->
#### 17.6.2 Frontend/backend extension boundary

Elaboration-time and frontend extensions operate on `KSyntax`, KFrontIR, or KCore.

Backend extensions operate at or after KBackendIR.

Rules:

* an elaboration-time or frontend extension MUST NOT require direct dependence on implementation-defined
  target-lowering internals;
* a backend extension MUST NOT redefine source semantics or bypass the legality checkpoints required for KCore or
  KBackendIR;
* if a backend extension is visible during elaboration, it must be visible only through elaboration-available backend
  intrinsics under §17.6.1;
* an implementation SHOULD avoid maintaining separate unrelated extension models for tooling and batch compilation when
  the same dependency-tracked query and lowering engine can be reused; and
* any implementation-defined extension point that crosses this boundary MUST document which checkpoint is its input and
  which invariants it may rely on.

<!-- compiler.intrinsics.host_binding_intrinsics -->
#### 17.6.3 Host binding intrinsics

A backend profile that provides `host.jvm`, `host.jvm.jni`, `host.dotnet`, or `host.native` MAY satisfy exported
declarations of those modules using backend intrinsics, generated companion fragments, generated registration tables,
generated native or managed shim artifacts, or another observationally equivalent mechanism.

Rules:

* Each raw host binding declaration MUST have a stable implementation identity derived at least from:
  * the pinned host-source identity,
  * the selected host member identity,
  * the binding-generator identity, and
  * any implementation-documented adapter mode, marshalling, calling-convention, deployment prerequisite,
    trusted-binding-summary identity, or companion-artifact recipe that affects semantics.
* Unless explicitly classified as elaboration-available under §17.6.1, a raw host binding intrinsic is runtime-only.
* A backend MAY generate hidden companion classes, source-generated interop declarations, registration tables, headers,
  shim source, native helper objects, or other auxiliary artifacts needed to realize a host binding. Such artifacts are
  part of the host-binding realization and need not be user-authored source code.
* A backend MUST reject a host binding whose required adapter mode, calling convention, marshalling, callback bridge,
  blocking classification, trusted summary contract, deployment prerequisite, or runtime service it cannot realize under
  the selected backend profile or deployment mode.

<!-- compiler.ffi -->
### 17.7 Foreign interop and portable foreign ABI

A conforming implementation MAY expose backend-specific host bindings and portable foreign-ABI adapters.

<!-- compiler.ffi.foreign_abi -->
#### 17.7.1 Portable foreign ABI

A conforming implementation MAY expose foreign-ABI adapters. The portable subset of such an ABI is defined after
elaboration and erasure.

Portable ABI rules:

* portable ABI functions are first-order after erasure;
* implicit parameters, coherent constraint-evidence parameters, quantity-`0` parameters, and proof-only parameters are
  erased and are not ABI-visible;
* resumption values, handler frames, cleanup frames, local nominal declarations, and anonymous borrow regions are not
  portable ABI values;
* fiber handles, fiber ids, supervision scopes, monitor handles, promises, fiber-local-state cells, and TVars are not
  portable ABI values;
* direct borrowed parameters and direct borrowed results are not part of the portable subset;
* borrow-like or resource-like foreign interfaces in the portable subset MUST be represented using owned values or
  opaque resource handles.

Additional portable ABI rule:

* `std.gradual.Dyn` and `std.gradual.DynRep` are not part of the portable subset unless the selected foreign-ABI adapter
  documents a stable portable encoding for them.

Opaque resource handles are ordinary runtime values from the perspective of KBackendIR. Their concrete representation is
backend-specific.

A raw host binding module is not, by itself, part of the portable subset merely because it is expressed using ordinary
Kappa syntax. Portability requires an exported surface that also obeys the rules of this subsection.

<!-- compiler.ffi.raw_host_binding_surfaces -->
#### 17.7.2 Raw host binding surfaces

A host binding module MAY expose a mechanically generated raw surface derived from host metadata, a native ABI
description, or another implementation-documented binding description.

If an implementation provides a refined surface for host binding module `M`, the mechanically generated raw surface MUST
remain available as submodule `M.Raw`.

Rules:

* The raw surface is ordinary Kappa surface syntax and ordinary module interface data. It is not a separate declaration
  language.
* Imported host reference types, imported host class/interface types, and imported managed object types MUST be exposed
  as opaque exported types unless a user-written overlay intentionally re-exposes a more specific representation.
* Imported native pointer-like values in raw native bindings SHOULD use `std.ffi.RawPtr` or an opaque handle type.
* Imported native scalars whose ABI meaning is exact-width or pointer-width MUST use the corresponding `std.ffi` scalar
  types.
* A raw `host.native` surface denotes the native ABI itself rather than a particular backend realization of that ABI.
  Different backend profiles MAY realize the same raw `host.native` surface through different adapter modes under
  §17.7.2.1 without changing its source-level spelling.
* A raw `host.jvm.jni` surface is backend-specific to the `jvm` profile and MAY expose JNI reference types, interface
  pointers, or registration surfaces using opaque exported types, `RawPtr`, `OpaqueHandle`, or another
  implementation-documented equivalent representation.
* When a host type has generic arity that the implementation can preserve soundly in the selected backend profile, the
  raw surface MAY expose it as an ordinary Kappa type constructor of the same arity. Otherwise the implementation MUST
  expose an erased raw surface rather than inventing unsupported phantom precision.
* Instance methods SHOULD be exposed as ordinary terms with exactly one receiver-marked explicit binder so that the
  dotted-call rules of §2.8.4 apply.
* Static members remain ordinary static members of the imported host type module.
* Constructors SHOULD be exposed as ordinary terms named `new` when unambiguous. When overloading requires
  disambiguation, the implementation MUST assign deterministic additional spellings.
* If multiple host members with the same source spelling are exported into one Kappa module, the implementation MUST
  assign deterministic exported spellings to those overloads. The unsuffixed base spelling MAY be used only when
  exactly one exported member of that source spelling remains after filtering and overlay application.
* A raw binding that requires variadics, layout-sensitive structures, callbacks, function pointers, or other ABI
  features that the selected backend profile cannot represent soundly under the selected adapter mode MUST be rejected or
  require an explicit shim or trusted binding summary rather than guessed.

<!-- compiler.ffi.native_abi_adapter_modes -->
#### 17.7.2.1 Native-ABI adapter modes

A `host.native` surface is realized by a backend profile through an adapter mode.

The standard adapter mode names are:

* `native.direct`
* `jvm.ffm`
* `jvm.jna`
* `jvm.jni-shim`
* `dotnet.libraryimport`
* `dotnet.dllimport`

Rules:

* `native.direct` is the direct native-code realization used by the `zig` profile or another native backend profile.
* `jvm.ffm`, `jvm.jna`, and `jvm.jni-shim` are JVM realizations of ordinary native-ABI bindings.
* `dotnet.libraryimport` and `dotnet.dllimport` are CLR realizations of ordinary native-ABI bindings.
* Adapter-mode selection is implementation-documented. It MAY be chosen by build configuration, manifest data,
  surface-sugar attributes, trusted binding summaries, or another equivalent mechanism.
* If incompatible adapter modes are requested for the same binding by multiple selection mechanisms, compilation fails.
* Adapter-mode selection is part of the effective build configuration and of the host-binding implementation identity
  whenever it affects exported signatures, runtime semantics, or deployment requirements.
* If a selected adapter mode requires generated companion classes, generated interop declarations, registration tables,
  shim source, headers, native helper objects, or another auxiliary artifact, those artifacts are part of the binding
  realization under §§17.2.6 and 17.6.3.
* A selected adapter mode MAY additionally provide implementation-documented call-state capture facilities, such as
  `errno` capture, last-error capture, or equivalent host status snapshots, but such facilities do not by themselves
  determine a Kappa typed error channel.
* Adapter modes do not by themselves justify a more precise Kappa type than that assigned by §§17.7.2-17.7.3. More
  precise result typing, error typing, ownership, or callback semantics still require a refined overlay, trusted binding
  summary, or shim.
* A conforming implementation MAY expose ordinary source-level sugar to select adapter modes, provided that sugar
  elaborates to the ordinary host-binding, binding-summary, and overlay mechanisms of this chapter.

<!-- compiler.ffi.conservative_typing_minimal_effect_inference -->
#### 17.7.3 Conservative typing and minimal effect inference

The raw surface of a host binding module MUST be conservative.

Rules:

* A raw foreign declaration MAY be given a pure result type only when the implementation can prove, or a trusted binding
  summary explicitly states, that the underlying host operation is total, nonblocking, nonthrowing, and observably
  side-effect free under the selected backend profile.
* If that condition is not met, the raw declaration MUST be placed in `UIO`.
* A raw foreign declaration MUST NOT be assigned a non-`Void` typed error channel solely from bytecode, IL, metadata,
  headers, symbol information, declared host exception lists, status-code macros, last-error flags, or similar host
  metadata. Raw host failures, host exceptions, status codes, null sentinels, and similar host conventions do not by
  themselves determine a Kappa error type.
* A more precise result type `IO e a` requires an explicit refined overlay or a trusted binding summary that names the
  error type and the translation contract.
* For managed reference-typed parameters and results, the generated type is:
  * `T` only when host metadata proves non-nullability;
  * `Option T` when host metadata proves nullability; and
  * `Option T` when nullability is unknown.
* For native raw bindings, out-parameters, status-code returns, sentinel returns, adapter-captured call-state snapshots,
  and other ABI-level conventions MAY be exposed directly in the raw surface. Such shapes are backend-specific and are
  not portable merely because they are expressible in Kappa syntax.
* Every raw foreign declaration MUST carry foreign-call classification metadata as defined by §17.13:
  `nonblocking`, `blocking`, or `blocking-cancellable`.
* A backend that lacks the runtime capability required by that classification, including `rt-blocking` when relevant,
  MUST reject the declaration or deployment mode rather than silently weakening its behavior.

Additional rules:

* A raw foreign declaration MAY use `std.gradual.Dyn` for parameters or results when host metadata or bridge metadata
  does not determine a sounder Kappa type and the selected backend can retain enough runtime information to support
  later `checkedCast`, marshaling, or overlay validation.
* A raw foreign declaration MUST NOT expose a precise Kappa function, callback, package, record, or dictionary surface
  for a higher-order foreign value unless the selected backend/profile provides the higher-order monitoring required by
  §17.7.4B.
* If such higher-order monitoring is unavailable, the raw surface MUST expose the value as `std.gradual.Dyn`, as an
  opaque imported host type, or as another less precise raw shape.

<!-- compiler.ffi.precise_overlays_trusted_binding_summaries_shims -->
#### 17.7.4 Precise overlays, trusted binding summaries, and shims

A refined foreign surface is an ordinary Kappa module that wraps a raw host binding module and MAY rely on a trusted
binding summary or a user-provided shim.

A trusted binding summary is implementation-documented binding metadata associated with a raw host binding declaration or
with the generator inputs of a host binding module.

Rules:

* A refined overlay MAY rename raw bindings, hide overload spellings, strengthen nullability, translate status codes,
  catch and translate host exceptions, consult adapter-captured call-state, and expose more precise result types,
  including `IO e a`.
* A refined overlay that exposes `IO e a` MUST make the error translation contract explicit, either:
  * by implementing the translation in Kappa source, or
  * by delegating to a shim or trusted binding summary whose documented contract provides that translation.
* A trusted binding summary MAY refine raw typing only by adding facts not recoverable soundly from host metadata alone,
  such as purity classification, blocking classification, nullability, ownership or handle discipline, status-code
  meaning, sentinel meaning, exception-to-error translation, call-state capture, marshalling policy, string or struct
  layout policy, callback ABI, thread-attachment policy, JNI reference discipline, arena or lifetime policy, or
  deployment prerequisites.
* A trusted binding summary that permits a raw declaration to be typed as pure MUST state that the underlying host
  operation is total, nonblocking, nonthrowing, and observably side-effect free under the selected backend profile.
* A trusted binding summary that permits a declaration to be exposed as `IO e a` MUST name:
  * the error type `e`,
  * the triggering host conditions,
  * the translation from those conditions to `e`, and
  * which remaining host failure modes, if any, remain defects or interruption-pending runtime failures rather than
    typed errors.
* A trusted binding summary that exposes callback, handle, arena-lifetime, or thread-affinity behavior MUST name the
  ownership, creation, release, and thread-affinity contract relevant to that binding.
* The identity of every trusted binding summary that affects exported signatures or runtime semantics MUST be recorded in
  the effective build inputs, host binding implementation identity, and relevant interface artifacts.
* A shim MAY be provided as Kappa source, as target-host source, as native source, or as another
  implementation-documented separately compiled artifact that the selected backend can compile and link.
* Overlays, trusted binding summaries, and shims participate in ordinary hashing, interface artifacts, semantic object
  identity, and tooling exactly like other modules and artifacts.
* This specification does not require a distinct `foreign import` declaration form. An implementation MAY provide such a
  form as surface sugar only if it elaborates to the ordinary mechanisms of this section and preserves their semantics.
  Such sugar MAY include precise foreign type declarations, adapter-mode selection, entry-point naming, library lookup,
  marshalling directives, callback declarations, or JNI-specific policy annotations.

<!-- compiler.ffi.boundary_contracts_blame -->
#### 17.7.4A Boundary contracts, dependent checks, and blame

A refined overlay that exposes a type more precise than its raw foreign surface MUST do so through an explicit boundary
contract.

A boundary contract specifies, for each wrapped parameter or result:

* the raw foreign shape being accepted or produced;
* the exposed Kappa type;
* any marshaling performed between the foreign representation and the Kappa representation;
* the dynamic checks, if any, used to justify the exposed type; and
* whether those checks are Exact or Conservative.

Definitions:

* Exact: the boundary check is sound and complete with respect to the exposed invariant.
* Conservative: the boundary check is sound but not necessarily complete.

Rules:

* Unless an overlay or trusted binding summary explicitly states otherwise, a foreign boundary contract is Conservative.
* A refined overlay MUST NOT claim a more precise dependent, indexed, or refinement type unless it also specifies how
  the additional invariant is enforced at the boundary.
* Boundary enforcement MAY be a checked identity on representation, or it MAY marshal the foreign representation into an
  observationally equivalent Kappa value before validation.
* A foreign boundary contract is not required to preserve raw representation identity. When representation-preserving
  validation is impossible or inappropriate, marshaling is the normative mechanism.
* For indexed or refinement-rich types, the contract MAY compute runtime indices, propagate them through subsequent
  checks, and reject values that fail the demanded invariant.
* A failed boundary contract MUST produce `CastBlame` or an observationally equivalent diagnostic payload that
  identifies at least:
  * the boundary identity or source origin when available;
  * the direction of failure (`into Kappa`, `out of Kappa`, or `later higher-order use`); and
  * the demanded exposed type.
* A conforming implementation SHOULD preserve robust boundary behavior: after a foreign component has successfully
  crossed a boundary claiming type `T`, later runtime type failures attributable solely to hidden wrapper mismatch
  SHOULD not occur; subsequent failures SHOULD arise only from surrounding context misuse or from later explicit
  boundary checks.

<!-- compiler.ffi.higher_order_boundaries -->
#### 17.7.4B Higher-order boundaries

A higher-order boundary is a boundary whose correctness depends on later invocation, projection, callback interaction,
or other delayed use rather than on one immediate value inspection.

Examples include:

* functions and callbacks;
* callable objects;
* iterators or streams that yield later values;
* records or packages whose members are exposed as callable or stateful interfaces; and
* imported objects whose methods are exposed through receiver-marked terms.

Rules:

* A one-shot head-shape test is not sufficient justification for exposing a precise Kappa higher-order type.
* A conforming implementation that exposes a precise higher-order Kappa surface MUST enforce that surface with deep
  boundary monitoring that checks arguments, results, and observable member uses at the time of invocation or
  projection.
* Such monitoring MUST NOT reject a higher-order value solely because some potential future use would fail.
* An implementation that cannot provide such deep monitoring MUST expose the value as `std.gradual.Dyn`, as an opaque
  imported host type, or through a less precise portable facade.
* For the portable foreign ABI of §17.7.1, higher-order values remain outside the portable subset unless a later
  subsection explicitly states otherwise.

<!-- compiler.ffi.backend_specificity_portability_layering -->
#### 17.7.5 Backend specificity and portability layering

Rules:

* A module that directly imports `host.jvm...`, `host.dotnet...`, `host.native...`, or `host.jvm.jni...` is backend-
  specific to the corresponding provided host root.
* `host.native...` remains backend-specific even when more than one backend profile provides a realization of the same
  raw surface under different adapter modes.
* Portable libraries SHOULD expose backend-neutral surfaces using `expect` declarations, selected module fragments, or
  other ordinary Kappa modules that delegate to host bindings only in backend-specific fragments.
* A host binding module itself is not part of the portable subset unless the exported surface also satisfies the
  portable ABI restrictions of §17.7.1.
* In particular, direct `std.ffi.RawPtr` use, direct borrowed foreign values, raw out-parameter conventions, runtime-
  only host object types, JNI reference types, direct `JNIEnv`-style interfaces, and backend-specific blocking bridges
  are outside the portable subset unless wrapped by a portable facade.

**Design note:** The intended ergonomic layering is:

1. mechanically generated raw managed bindings in `host.jvm....Raw` and `host.dotnet....Raw`,
2. mechanically generated raw native-ABI bindings in `host.native....Raw`, realized per backend by adapter mode,
3. JVM-specific raw JNI bindings in `host.jvm.jni....Raw` when the user truly needs JNI rather than ordinary native ABI,
4. refined overlay modules with explicit Kappa types and error mapping, and
5. portable facades built with `expect` plus backend-selected fragments.

This keeps direct managed-host import and direct native-ABI import ergonomic without making backend-specific surfaces
part of the portable ABI.

Python, Ruby, and Perl interop SHOULD normally use the runtime bridge
lane of §17.7.7 unless the implementation provides a reproducible static
binding-generation story that better fits the use case.

<!-- compiler.ffi.bridge_based_interop_external_runtimes -->
#### 17.7.6 Bridge-based interop and external runtimes

Interop realized primarily through process, IPC, RPC, or embedding bridges is not a host binding module unless the
implementation documents an observationally equivalent host-metadata model.

Examples include bridges centered on CPython, Ruby, Perl, remote services, subprocess protocols, or host embedding
layers whose observable contract is mediated by serialization, callbacks, or message exchange rather than by direct host
metadata.

Rules:

* Such interop MAY be exposed through ordinary modules, foreign-ABI adapters, refined overlays, trusted binding
  summaries, or shims.
* It need not use the reserved module roots `host.jvm`, `host.dotnet`, or `host.native`.
* Any precision claim made by such a bridge is governed by the raw-surface conservatism, trusted-summary and overlay,
  boundary-contract, and higher-order-boundary rules of §§17.7.3, 17.7.4, 17.7.4A, and 17.7.4B.
* A process, IPC, RPC, or embedding bridge is not part of the portable foreign subset merely because its Kappa-facing
  API is expressed in ordinary Kappa syntax.

<!-- compiler.ffi.dynamic_bridge_handles -->
#### 17.7.7 Dynamic bridge handles and runtime-bound packages

A conforming implementation MAY provide bridge libraries for foreign
runtimes reached through process start, IPC, RPC, embedding adapters, or
other message-based or hosted bridges.

Dynamic bridge handles are the normative mechanism when user code must
control bridge startup and policy explicitly.

Examples (illustrative):

```kappa
type NumpySig : Type =
    (array : List Int -> IO PyError PyArray)

do
    using py <- python.start cfg
    let np <- std.bridge.bindModule @NumpySig py "numpy"
    let xs <- np.array [0, 1, 2]
```

Rules:

* A runtime bridge handle is an ordinary runtime value.
  It is not a module declaration and is not introduced by `import`.
* Creation of a runtime bridge handle is ordinary term-level code.
  The policy for process creation, executable-path selection,
  virtual-environment selection, transport, authentication,
  environment variables, sandboxing, timeout policy, and teardown is
  determined by the selected bridge library and by user code, not by the
  static module system.
* A runtime bridge bind performed through
  `std.bridge.BridgeHandle.bindModuleWith`,
  `std.bridge.bindModule`, or an observationally equivalent library API:
  * does not add a module dependency edge,
  * does not affect the import graph of §§2.2-2.3,
  * does not create a lexical declaration of kind `module`, and
  * does not contribute a source module interface artifact.
* Instead, a successful bind yields an ordinary Kappa value.
* If the requested surface type is a signature type or another
  package-like surface, the resulting value is used through the ordinary
  package-member rules of §2.8.3.
  For example, if `np` is the result of a successful bind, then
  `np.array` is ordinary package-member selection rather than qualified
  module lookup.
* A runtime bridge bind MUST be checked and enforced by an explicit
  `BridgeContract sig` value or an observationally equivalent runtime
  contract.
  The erased type argument `sig` alone is never sufficient to determine
  runtime validation.
* Missing members, wrong arities, marshalling failures, representation
  mismatches, contract violations, and later higher-order boundary
  misuse MUST surface as bridge failure in the surrounding monad rather
  than as silent fabrication of a value of the requested type.
* When the returned value closes over a borrowed bridge handle
  `(&[r] h : H)`, the returned value MUST carry capture annotation
  `captures (r)` unless the implementation can prove an equivalent
  explicit ownership scheme that prevents borrow escape.
* Bridge-specific startup libraries SHOULD provide `Releasable` or
  `MonadResource` support for their handle types so that bridge lifetime
  is lexically controlled.
* Runtime-created bridge handles and values obtained from them are not
  part of the frontend query model or module-interface identity unless a
  separate static facility explicitly records a corresponding schema,
  transcript, or generator input as build input.
* This subsection is orthogonal to static host-binding modules.
  Static host-binding modules remain appropriate for build-time metadata
  import; dynamic bridge handles are appropriate when user code must
  choose connection startup and policy at runtime.
* Implementations MAY additionally provide raw dynamic binding APIs that
  expose `std.gradual.Dyn` or another documented dynamic surface when no
  richer contract is requested.
* An implementation MAY provide a surface sugar resembling a runtime
  import only if that sugar elaborates to the ordinary bridge-handle
  mechanism of this subsection and does not participate in the static
  import graph, module interfaces, or module-dependency cycle checks.

<!-- compiler.zig -->
### 17.8 Native backend profile (`zig`)

A conforming implementation MAY provide the native backend profile `zig`.

The `zig` profile uses the Zig toolchain as its final native build and link layer.

Intermediate internal forms are not standardized. An implementation MAY lower KBackendIR to object code directly, to C,
to Zig source, to LLVM IR, or to another internal form, provided the final artifacts and observable behavior are those
of this profile.

Artifact kinds:

* executable;
* object file;
* static library;
* shared library.

Rules:

* the backend MUST define a deterministic mapping from the implementation's target-selection mechanism to the final Zig
  target used for code generation or linking;
* exported foreign-entry functions MUST use an implementation-documented native ABI;
* the backend runtime needed to realize handlers, closures, dictionaries, strings, bytes, big numerics, collections, and
  resumption objects may be implemented in Zig, in another systems language, or partly by compiler-generated code.

The native profile's observable semantics are those of Kappa, not of any intermediate textual form chosen by the
compiler.

<!-- compiler.zig.native_host_binding_modules -->
#### 17.8.1 Native host binding modules

The `zig` profile MAY provide `host.native` modules.

Rules:

* The `zig` profile realizes `host.native` through the standard adapter mode `native.direct`, unless an
  observationally equivalent implementation-documented native adapter mode is selected.
* The host-source identity of a `host.native` module MUST include the ABI-description inputs used to generate its raw
  surface, such as header digests, module-map digests, symbol-list digests, definition-file digests, shim-source
  digests, target triple, calling convention, and the identity of any linked native library required by the binding.
* A raw `host.native` surface SHOULD use `std.ffi` exact-width scalars, `RawPtr`, and opaque handles for ABI-visible
  native values.
* If the implementation cannot derive a sound raw surface from native metadata alone, it MAY require a user-provided
  shim or explicit binding description rather than guessing.
* A conforming implementation MAY additionally require allowlisted headers, wrapper headers, module maps, or equivalent
  curated binding inputs rather than importing an unconstrained ambient native namespace.
* The raw `host.native` surface used by the `zig` profile MAY also be reused by other backend profiles that provide
  `host.native` through different adapter modes. Such reuse does not by itself make those bindings part of the portable
  ABI.

<!-- compiler.jvm -->
### 17.9 JVM backend profile

A conforming implementation MAY provide the `jvm` backend profile.

Artifact kinds:

* class files;
* JARs or equivalent classfile bundles;
* implementation-defined generated companion classes, registration tables, headers, shim sources, or native helper
  artifacts required by selected `host.native` adapter modes or `host.jvm.jni` bindings.

Rules:

* the backend MUST lower KBackendIR to JVM-compatible artifacts whose externally loadable units conform to the JVM
  classfile format;
* the backend MAY realize closures, dictionaries, and trampolines via synthetic classes, `invokedynamic`, method
  handles, or equivalent JVM mechanisms;
* the backend MAY use JVM exceptions or `try/finally` as implementation techniques for Kappa abrupt control and cleanup,
  but the observable behavior MUST remain that of §§8.6-8.7 and §9;
* the backend MAY rely on host garbage collection for ordinary heap objects, but source-level resource release remains
  governed by Kappa semantics, not by host finalization behavior;
* the backend MAY generate hidden companion classes, registration tables, headers, shim source, or native helper
  artifacts needed to realize `host.native` or `host.jvm.jni` bindings; those artifacts are implementation details and
  are not part of the portable Kappa ABI.

A JVM adapter layer MAY additionally expose Java-friendly wrappers around portable Kappa exports, but those wrappers are
not part of the portable Kappa ABI.

<!-- compiler.jvm.host_binding_modules -->
#### 17.9.1 JVM managed host binding modules

The `jvm` profile MAY provide `host.jvm` modules.

Rules:

* The host-source identity of a `host.jvm` module MUST include the immutable identities of the classfiles, JARs, or
  equivalent JVM metadata units from which the binding was derived.
* A raw `host.jvm` surface MAY use classfile signatures, generic signatures, annotations, and other JVM metadata to
  build its exported interface.
* Checked vs unchecked JVM exceptions do not, by themselves, determine Kappa typed error channels for raw bindings.
* Nullability MAY be inferred from implementation-documented JVM metadata. In the absence of proof of non-nullability,
  the raw surface MUST use the conservative rules of §17.7.3.

<!-- compiler.jvm.native_interop_adapters_and_jni_modules -->
#### 17.9.2 JVM native-ABI adapters and JNI modules

The `jvm` profile MAY provide `host.native` modules realized through one or more of the standard adapter modes:

* `jvm.ffm`
* `jvm.jna`
* `jvm.jni-shim`

The `jvm` profile MAY additionally provide `host.jvm.jni` modules for JNI-specific interop.

Rules:

* `jvm.ffm` is the preferred native-ABI adapter mode for ordinary foreign libraries when the selected deployment mode
  supports it.
* `jvm.jna` is a compatibility native-ABI adapter mode for ordinary foreign libraries.
* `jvm.jni-shim` is a native-ABI adapter mode in which the implementation MAY generate hidden Java declarations,
  registration tables, headers, or native shim artifacts needed to realize a `host.native` binding. The user need not
  author a manual Java bridge.
* If more than one JVM native-ABI adapter mode is available and the build configuration does not choose one, the
  implementation SHOULD prefer `jvm.ffm` over `jvm.jna` and `jvm.jni-shim`.
* `host.jvm.jni` is reserved for JNI-specific and Invocation-API-specific bindings, including `JNIEnv`-mediated
  operations, native-method registration, load/unload hooks, thread attach/detach, reference-management primitives, and
  JVM embedding or startup APIs.
* A raw `host.jvm.jni` surface SHOULD expose JNI reference-like values as opaque handles or equivalent
  implementation-documented raw types unless a refined overlay intentionally bridges them to imported managed host
  types.
* A trusted binding summary or shim for a `jvm.ffm`, `jvm.jna`, `jvm.jni-shim`, or `host.jvm.jni` binding MAY specify
  callback ABI, local-vs-global reference discipline, exception-checking conventions, thread-attachment requirements,
  arena or lifetime policy, and any load-time or deployment-time prerequisites.
* If the selected JVM deployment mode cannot realize the required native-access prerequisite, adapter mode, callback
  bridge, thread-attachment policy, blocking classification, or generated companion artifact of the selected binding,
  the implementation MUST reject that binding or deployment mode rather than silently weakening semantics.

<!-- compiler.dotnet -->
### 17.10 CLR backend profile (`dotnet`)

A conforming implementation MAY provide the `dotnet` backend profile.

Artifact kinds:

* assemblies containing CIL and metadata;
* implementation-defined publish outputs derived from such assemblies;
* native outputs produced by optional Native AOT publish mode;
* implementation-defined generated interop companion artifacts required by selected `host.native` adapter modes.

Rules:

* the backend MUST lower KBackendIR to CLR-compatible artifacts;
* the ordinary managed form of this profile targets assemblies containing CIL and metadata;
* the backend MAY additionally offer a Native AOT publish mode. Native AOT is a deployment mode of the `dotnet`
  profile, not a separate Kappa semantic profile;
* the backend MAY use CLR exceptions, delegates, runtime services, source-generated interop declarations, or host
  garbage collection as implementation techniques, but the observable behavior MUST remain that of Kappa source
  semantics;
* the backend MAY generate companion interop declarations, marshalling helpers, or equivalent auxiliary artifacts needed
  to realize `host.native` bindings; those artifacts are implementation details and are not part of the portable Kappa
  ABI.

If a program or library depends on runtime features unavailable under the selected CLR deployment mode, such as a
particular Native AOT mode, the implementation MUST reject that deployment with a compile-time or publish-time
diagnostic rather than silently weakening semantics.

<!-- compiler.dotnet.clr_host_binding_modules -->
#### 17.10.1 CLR managed host binding modules

The `dotnet` profile MAY provide `host.dotnet` modules.

Rules:

* The host-source identity of a `host.dotnet` module MUST include the immutable identities of the assemblies or
  equivalent CLR metadata units from which the binding was derived.
* A raw `host.dotnet` surface MAY use ECMA-335 metadata, generic-parameter data, attributes, and nullable-reference
  metadata to build its exported interface.
* When metadata-only assemblies or reference assemblies are available and are observationally sufficient for interface
  generation, the implementation SHOULD permit them to serve as the source of the raw exported interface.
* CLR exceptions do not, by themselves, determine Kappa typed error channels for raw bindings.
* In the absence of proof of non-nullability from CLR metadata, the raw surface MUST use the conservative rules of
  §17.7.3.
* If the selected deployment mode, including any Native AOT mode, cannot realize the required marshalling or runtime
  services for a given `host.dotnet` binding, the implementation MUST reject that binding or deployment mode rather than
  silently weakening semantics.

<!-- compiler.dotnet.native_interop_adapters -->
#### 17.10.2 CLR native-ABI adapters

The `dotnet` profile MAY provide `host.native` modules realized through one or more of the standard adapter modes:

* `dotnet.libraryimport`
* `dotnet.dllimport`

Rules:

* `dotnet.libraryimport` is the preferred native-ABI adapter mode for ordinary foreign libraries when the selected
  deployment mode supports it.
* `dotnet.dllimport` is a compatibility native-ABI adapter mode for ordinary foreign libraries.
* If more than one CLR native-ABI adapter mode is available and the build configuration does not choose one, the
  implementation SHOULD prefer `dotnet.libraryimport` over `dotnet.dllimport`.
* The user need not author C# bridge code merely to make a `host.native` binding callable from Kappa. The
  implementation MAY generate companion declarations, marshalling helpers, or equivalent auxiliary artifacts as needed.
* A trusted binding summary or shim for a `dotnet.libraryimport` or `dotnet.dllimport` binding MAY specify entry-point
  naming, charset, string or struct marshalling, last-error capture, custom marshaller use, callback ABI, ownership or
  handle discipline, and any Native AOT or deployment-mode prerequisites.
* If the selected CLR deployment mode, including any Native AOT mode, cannot realize the required adapter mode,
  generated marshalling, callback bridge, last-error capture, custom marshaller contract, or runtime service, the
  implementation MUST reject that binding or deployment mode rather than silently weakening semantics.

<!-- compiler.wasm -->
### 17.11 WebAssembly backend profile (`wasm`)

A conforming implementation MAY provide the `wasm` backend profile.

The `wasm` profile has two subprofiles:

* `wasm-core`
* `wasm-component`

An implementation MAY provide either or both.

<!-- compiler.wasm.core -->
#### 17.11.1 `wasm-core`

`wasm-core` targets the core WebAssembly module model.

Rules:

* the backend MUST lower KBackendIR to a core WebAssembly module plus any required embedder imports;
* all interaction with the environment MUST occur through imports or other embedder-defined mechanisms consistent with
  the WebAssembly embedding model;
* the feature set used by the generated module is implementation-defined and MUST be recorded in the build artifact;
* the representation of heap-managed Kappa objects is implementation-defined. A backend MAY use linear memory, host
  references, or a hybrid design, provided the resulting module remains valid for the chosen WebAssembly feature set and
  preserves Kappa semantics.

<!-- compiler.wasm.component -->
#### 17.11.2 `wasm-component`

`wasm-component` targets the WebAssembly Component Model.

Rules:

* the backend MUST define component interfaces in WIT or in an observationally equivalent canonical form;
* when Kappa exports opaque resource handles through this profile, the backend MAY map them to WIT resources;
* when the backend uses WIT resources, owned vs borrowed Kappa handle adapters MUST respect the ownership discipline
  documented for that interface.

Floating-point restriction:

* Plain WIT `f32` and `f64` do not preserve exact IEEE-754 NaN bit patterns across interface boundaries.
* Therefore a Kappa export or import whose observable behavior depends on raw-bit `Eq Float` / `Eq Double` semantics
  MUST NOT be exposed as plain WIT `f32` / `f64` in the portable subset.
* Such values MUST instead be adapted through an implementation-documented lossless representation, such as integer bit
  patterns or byte buffers, or else the interface MUST be marked backend-specific.

<!-- compiler.js -->
### 17.12 ECMAScript backend profile (`js`)

A conforming implementation MAY provide the `js` backend profile.

Artifact kinds:

* ES modules;
* CommonJS bundles;
* implementation-defined package outputs derived from JavaScript artifacts.

Rules:

* the backend MUST lower KBackendIR to ECMAScript-compatible artifacts;
* the backend MAY realize closures, dictionaries, handlers, fibers, and resumptions via functions, promises,
  microtasks, generators, async functions, worker APIs, or equivalent JavaScript mechanisms;
* the backend MUST preserve Kappa source semantics for interruption, finalization, structured concurrency, and STM;
* the backend MUST NOT silently weaken runtime semantics merely because the selected JavaScript host is single-agent,
  worker-based, or event-loop based.
* when realizing Kappa fibers atop a single-agent JavaScript event loop, the backend MUST still preserve the
  scheduler-fairness, timer, promise, and interruption-observation obligations of §§8.1 and 14.8 within that agent;

If the selected JavaScript deployment target lacks one or more required runtime capabilities of §17.13, compilation is a
hard error for programs that require those capabilities.

<!-- compiler.capabilities -->
### 17.13 Runtime capability profiles

Every backend profile MUST declare a runtime capability set.

The standard runtime capability names are:

* `rt-core`
* `rt-parallel`
* `rt-shared-stm`
* `rt-blocking`

Meaning:

* `rt-core`:
  * `IO e a`,
  * typed failures, causes, exits,
  * structured interrupt metadata,
  * fibers,
  * source-visible fiber identities and labels,
  * structured interruption,
  * masking,
  * scheduler fairness and implementation-defined safe-point delivery,
  * monotonic timers,
  * promises,
  * fiber-local state,
  * explicit supervision scopes and monitors,
  * finalizers,
  * the handle-reachability / GC-nonobservability guarantees of §14.8.3C,
  * the portable synchronization guarantees of §14.8.4A,
  * and single-agent STM semantics.

* `rt-parallel`:
  * execution of runnable fibers on more than one host execution resource at a time.

* `rt-shared-stm`:
  * TVars whose observable semantics remain valid across parallel workers or host execution agents.

* `rt-blocking`:
  * backend-supported blocking bridges for foreign calls together with interruption classification.

Capability rules:

* A backend that lacks a required capability MUST reject the affected program or deployment mode rather than silently
  weakening semantics.
* A backend advertising `rt-core` MUST satisfy the scheduler-fairness, timer, promise, scope/monitor,
  fiber-local-state, fiber-identity/label, interrupt-metadata, handle-reachability / GC-nonobservability,
  and memory-visibility obligations of §§8.1 and 14.8.
  It MUST NOT expose only a syntactic surface while silently weakening those runtime semantics.
* Absence of `rt-parallel` does not make `fork` invalid. It means only that concurrency need not execute on more than
  one host execution resource simultaneously.
* Absence of `rt-shared-stm` does not remove `STM` from the language. It restricts `STM` to a single runtime agent.
* Absence of `rt-blocking` makes blocking foreign-call bridges unavailable in the portable subset.

Recommended backend declarations:

* `zig`, `jvm`, and managed `dotnet` SHOULD advertise:
  * `rt-core`,
  * `rt-parallel`,
  * `rt-shared-stm`,
  * `rt-blocking`.

* `wasm-core`, `wasm-component`, and `js` MUST advertise `rt-core`.
  They MAY additionally advertise `rt-parallel`, `rt-shared-stm`, or `rt-blocking` only when the selected embedder or
  deployment configuration actually provides them.

For the standard target families covered by this specification, `zig`, `jvm`, managed `dotnet`, `wasm-core`,
`wasm-component`, and `js` SHOULD all support the full `rt-core` surface, including timers and promises.

Foreign-call interruption classification:

A foreign call that may suspend a host execution resource is classified as one of:

* `nonblocking`
* `blocking`
* `blocking-cancellable`

Rules:

* If a fiber is interrupted while executing a `nonblocking` call, ordinary interruption rules apply at the next
  interruption point.
* If a fiber is interrupted while executing a `blocking` call, interruption becomes pending and is taken when the call
  returns.
* If a fiber is interrupted while executing a `blocking-cancellable` call, the runtime MUST attempt backend-specific
  cancellation and then obey ordinary interruption semantics.
* A backend that cannot realize the required classification MUST reject that foreign declaration or deployment rather
  than silently pretending to support it.

<!-- compiler.conformance -->
### 17.14 Backend conformance

A backend profile is conforming iff every accepted program, when compiled under that profile, behaves observationally as
required by this specification.

In particular:

* the implementation MAY fuse KCore, KBackendIR, and target lowering into fewer internal passes, provided hashing
  inputs, diagnostics, and observable execution remain as if the conceptual stages had been formed;
* the implementation MAY perform any optimization that preserves the semantics of §§14.2-14.7 and the backend
  obligations of this chapter;
* no backend may use a target-platform limitation as justification for silently changing Kappa program meaning.

---

<!-- appendices.pipe_operators -->
## Appendix B. Pipe operators

<!-- appendices.pipe_operators.prelude_provisions -->
### B.1 Prelude provisions

`std.prelude` exports:

```kappa
(|>) : a -> (a -> b) -> b
let x |> f = f x
infix left 1 (|>)

(<|) : (a -> b) -> a -> b
let f <| x = f x
infix right 0 (<|)
```

<!-- appendices.pipe_operators.interaction_other_operators -->
### B.2 Interaction with other operators

* `|>` has low precedence (`1`), below ordinary comparison and arithmetic operators and below any user-defined operator
  with higher precedence. Thus tighter-binding operator expressions on the right of `|>` group there first.
* `<|` has precedence `0` and is right-associative, so `f <| g <| x ≡ f (g x)`.
* Pipe operators are ordinary infix operators. They do not alter the parsing or elaboration of dotted forms, method-call
  sugar, or receiver-projection sugar (§2.8.4). In particular, `x |> obj.method` parses as `x |> (obj.method)`; the
  pipe does not retarget method-call or receiver-projection sugar onto `x`.

<!-- appendices.pipe_operators.optional_typed_pipe -->
### B.3 Optional: typed pipe

Implementations MAY additionally provide a monadic pipe:

```kappa
(|>=) : m a -> (a -> m b) -> m b
let (|>=) = (>>=)
infix left 1 (|>=)
```

This is merely a renamed `>>=` with pipe-compatible precedence for stream-style monadic code.

<!-- appendices.applicativedo -->
## Appendix G. ApplicativeDo

This appendix amends §8.2.

<!-- appendices.applicativedo.capability -->
### G.1 Capability

`std.prelude` provides:

```kappa
trait Applicative (f : Type -> Type) =
    pure    : a -> f a
    liftA2  : (a -> b -> c) -> f a -> f b -> f c

    let (<*>) : f (a -> b) -> f a -> f b =
        \ff fa -> liftA2 (\f x -> f x) ff fa
```

`Monad` refines `Applicative`: any `Monad m` instance MUST also determine an `Applicative m` instance for the same `m`.

<!-- appendices.applicativedo.applicative_do_blocks -->
### G.2 Applicative-only `do` blocks

A `do` block may be elaborated using only `Applicative` (not `Monad`) when all of the following hold:

* Every do-item is one of:
  * `let pat <- expr` with irrefutable `pat` (§8.2),
  * a pure `let pat = expr` binding, or
  * the final expression.
* No binding's right-hand side refers to any name introduced by an earlier `<-` binding in the same `do` block.
* The block contains no control-flow features: no `if`, `match`, `while`, `for`, `defer`, `using`, `break`, `continue`,
  `return`, `let?`, or `try`.

For example:

```kappa
do
    let x1 <- e1
    let x2 <- e2
    let y  = f x1 x2
    final_expr
```

may elaborate to:

```kappa
liftA2 (\x1 x2 -> let y = f x1 x2 in final_expr) e1 e2
```

generalizing to n-ary independent groups via repeated `liftA2` / `<*>`.

<!-- appendices.applicativedo.mixed_blocks -->
### G.3 Mixed blocks

If some `<-` binds are independent and others are dependent on earlier `<-` results, the compiler SHOULD partition the
block into maximal independent groups elaborated with `liftA2` / `<*>`, and then combine those groups with `>>=`.

This is an as-if optimization: the resulting program MUST remain observationally equivalent to the monadic desugaring of
§8.2 / §8.7.

<!-- appendices.flow_typing_checklist -->
## Appendix H. Flow-typing checklist (non-normative)

A conforming implementation SHOULD evaluate the flow-typing subsystem against a stable benchmark such as If-T.

For Kappa v1, the intended support profile is:

* branch-local positive and negative narrowing;
* stable-alias transport;
* reachability-based propagation past terminal `do` branches;
* loop-body and post-loop propagation for pure `while` boolean conditions;
* checked one-way and two-way refinement predicates;
* linearity-aware, non-consuming narrowing;
* reborrow-respecting narrowing of borrowed scrutinees; and
* constructor and index refinement.

A conforming implementation MAY deliberately omit more expensive global analyses, such as backward fixpoint narrowing,
provided that omission is documented together with the rationale and the observable conservatism it introduces.

<!-- appendices.modal_coeffects -->
## Appendix M. Reserved modal/coeffect architecture (non-normative)

This appendix is explanatory.
The normative rules are in §§5.1.5.2-5.1.5.3, §14.1, §14.3, §17.2.2, and §17.3.

A future implementation may structure checking around judgments of the form:

```text
Γ ⊢ e ⇒ A ▷ Q ; Φ
Γ ⊢ e ⇐ A ▷ Q ; Φ
```

where:

* `Q` is the ordinary quantity result of §§5.1.5-5.1.7.
* `Φ` is a finite set of modality predicates emitted only by enabled modal/coeffect extensions.

Reference output shape:

```text
TcOut =
  { type       : Type
  , qtyUse     : QtySummary
  , modalPreds : List ModalPred
  , subst      : Subst
  }
```

Schematic combination rules:

```text
checkSeq(e1, e2):
  o1 = check e1
  o2 = check e2
  return
    { type       = o2.type
    , qtyUse     = o1.qtyUse + o2.qtyUse
    , modalPreds = o1.modalPreds ++ o2.modalPreds
    , subst      = o2.subst ∘ o1.subst
    }

checkIf(c, t, e):
  oc = check c
  ot = check t
  oe = check e
  return
    { type       = joinType(ot.type, oe.type)
    , qtyUse     = oc.qtyUse + (ot.qtyUse ⊔ oe.qtyUse)
    , modalPreds = oc.modalPreds ++ ot.modalPreds ++ oe.modalPreds ++ branchModalJoin(ot, oe)
    , subst      = mergeSubst(ot.subst, oe.subst)
    }

checkApp(f, a):
  of = synth f
  oa = check a against next binder
  return
    { type       = resultType(of.type, oa.term)
    , qtyUse     = of.qtyUse + oa.qtyUse
    , modalPreds = of.modalPreds ++ oa.modalPreds ++ appModalObligations(of, oa)
    , subst      = oa.subst ∘ of.subst
    }
```

Operational intent:

* `Q` is syntax-directed and never requires a general solver.
* `Φ` is extension-specific and is discharged separately by `MODAL_SOLVE`.
* Effects remain orthogonal: rows classify which effects may occur; modality predicates classify some additional graded
  or policy property.
* A future extension should therefore be implemented as "existing checker + extra predicates", not by generalizing
  `Quantity`, `⊑`, borrow introduction, or borrow lifetimes.

<!-- appendices.local_control_lowering_examples -->
## Appendix N. Local control lowering examples (non-normative)

This appendix is explanatory.
It illustrates the permitted backend-local finite-control lowerings of §§7.7.3A, 8.1.9, 8.1.10, 8.1.10A, 14.8.6A, and
17.4.7A.

<!-- appendices.local_control_lowering_examples.match_parser_chain -->
### N.1 `Match` parser chain to a finite local arm graph

Source parser chain:

```kappa
match input
  case ParseHeader -> 
      match parseBody input
        case Match.Hit body ->
            use body
        case Match.Miss rest ->
            recover rest
  case Match.Miss rest ->
      recover rest
```

The normative source semantics remain the explicit `Match` constructors and nested case analysis.

An implementation MAY lower the surrounding chain to an internal finite local control protocol whose arms correspond to:

* `headerHit`;
* `bodyHit`;
* `bodyMiss`; and
* `outerMiss`.

Schematic arm graph:

```text
entry
  -> headerHit
  -> outerMiss(rest)

headerHit
  -> bodyHit(body)
  -> bodyMiss(rest)

bodyHit(body)
  -> return(use body)

bodyMiss(rest)
  -> return(recover rest)

outerMiss(rest)
  -> return(recover rest)
```

Required preserved properties:

* the source scrutinee and parser calls are still evaluated exactly once in left-to-right order;
* residue threading remains exact;
* `recover` is still reached only through the source failure arms; and
* no first-class multi-return value becomes visible at source level.

This is exactly the kind of finite local protocol permitted by §17.4.7A: it is non-escaping, bounded, and equivalent
to the explicit `Match` constructors at the source level.

<!-- appendices.local_control_lowering_examples.tail_resumptive_handler -->
### N.2 Tail-resumptive handler clause to in-place transfer

Consider a shallow handler whose operation clause immediately tail-resumes:

```kappa
handle Log io with
  case return x -> pure x
  case log msg k -> k Unit
```

The source semantics still bind `k` as a captured resumption value under §8.1.9.

Because the clause:

* performs exactly one resumption,
* performs it in tail position, and
* neither stores nor duplicates `k`,

it is tail-resumptive in the sense of §14.8.6A.

A conforming implementation MAY therefore realize the clause without full general resumption capture, using an
observationally equivalent in-place transfer or join-point handoff:

```text
on log(msg):
  discard current clause frame
  jump directly to resumed suffix with payload Unit
```

This optimization is valid only if it preserves:

* the exact handled effect label;
* the shallow-handler rule that the handler is not automatically reinstalled around the resumed suffix;
* the current interruption-mask state and other captured control state required by §14.8.5; and
* all pending `defer`, `using`, and cleanup obligations in the captured segment.

If the clause instead duplicated `k`, stored it, or resumed it under multiple branches, the implementation would have
to fall back to the ordinary resumption realization rules of §§14.8.5-14.8.9.

<!-- appendices.test_harness -->
## Appendix T. Standard test harness

<!-- appendices.test_harness.scope_status -->
### T.1 Scope and status

This appendix defines the **standard Kappa test harness**.

The standard harness is a portable specification for compiler and language tests.
It covers:

* source-embedded assertions;
* multi-file compilation suites;
* backend- and mode-selected tests;
* stage-dump and pipeline-trace assertions; and
* incremental multi-step test suites.

An implementation that claims support for the standard Kappa test harness MUST accept the syntax and satisfy the
behavior specified in this appendix.

Implementations MAY support additional nonstandard directives.
Such directives are outside the portable subset.

This appendix does **not** define a standard `xfail`, `todo`, or similar expected-failure escape hatch.

<!-- appendices.test_harness.test_forms -->
### T.2 Test forms

The standard harness supports three test forms:

1. **Single-file inline test**
   A `.kp` source file containing one or more test directives.

2. **Directory suite**
   A directory containing one or more `.kp` source files and optionally a file named `suite.ktest`.

3. **Incremental step suite**
   A directory containing subdirectories named `step0`, `step1`, `step2`, and so on, each of which is itself a
   complete suite root.
   The incremental suite MAY additionally contain a file named `incremental.ktest` for cross-step assertions.

In a single-file inline test, the compilation root is that file.
The suite root for resolving relative paths is the containing directory of that file, and that directory is also the
source root for module-path derivation.

In a directory suite, the compilation roots are all `.kp` files under the suite root, recursively, interpreted relative
to that suite root as the source root for module-path derivation.

In an incremental step suite, each step directory is compiled as a complete suite root in numeric order, and the
harness preserves any compiler caches or reusable session state permitted by Chapter 17 across those steps.

<!-- appendices.test_harness.directive_syntax -->
### T.3 Directive syntax

A **test directive line** is a line comment whose first non-whitespace characters are `--!`.

In `.kp` source files only, the harness also recognizes an **inline diagnostic marker** of the form `--!!` after
ordinary source text on the same line.

Grammar:

```text
testDirectiveLine ::= ws? '--!' ws? testDirective
inlineDiagnosticMarker ::= sourceText ws? '--!!' ws diagnosticCode (ws diagnosticCode)*
testDirective     ::= directiveName [ws directiveBody]?
directiveName     ::= ident | extensionDirectiveName
extensionDirectiveName ::= 'x-' ident ('.' ident)*
directiveBody     ::= <the remainder of the line, excluding the line break>
diagnosticCode    ::= <a non-whitespace diagnostic code token>
```

Rules:

* Test directives are recognized only in line comments of the form `--!`.
* Inline diagnostic markers are recognized only in `.kp` source files, and only when `--!!` appears after ordinary
  source text on the same line.
* Block comments do not introduce test directives.
* Test directives are consumed by the harness before or alongside normal compilation.
  They remain comments for the language itself and do not affect ordinary source semantics.
* In a `.kp` source file, a directive applies to the suite containing that file.
  Directives that are file-relative, such as `assertDeclKinds`, apply to the containing file.
* In `suite.ktest` or `incremental.ktest`, each line MUST be a directive line, a blank line, or a comment-only line
  beginning with `--`.
* Directive names beginning with `x-` are implementation-defined extensions and are outside the portable subset.
  If a test uses one or more such directives that the harness does not support, the test result is **unsupported**.
  A portable harness MUST NOT silently ignore unsupported extension directives.
* Any unknown standard directive, malformed directive, or ill-typed directive argument is a harness error.
* Whenever this appendix uses `<stringLiteral>` in a directive, it means the non-interpolated subset of the ordinary
  Kappa string-literal grammar.
  Interpolated string literals are not permitted in test directives.

All directive files and golden files are UTF-8.

<!-- appendices.test_harness.configuration_directives -->
### T.4 Configuration directives

Configuration directives set up the current test case.

Supported standard configuration directives are:

```text
mode analyze
mode check
mode compile
mode run

packageMode
scriptMode

backend <profile>

entry <qualifiedName>
runArgs <stringLiteral>...
stdinFile <path>

dumpFormat json
dumpFormat sexpr

requires backend <profile>
requires mode package
requires mode script
requires capability <name>
```

Rules:

* If no `mode` is specified, the default is `check`.
* If neither `packageMode` nor `scriptMode` is specified, the default is `packageMode`.
* `backend <profile>` selects the backend profile for `compile` and `run` tests.
* Portable tests using `mode compile` or `mode run` SHOULD specify `backend <profile>`.
* `entry <qualifiedName>` is valid only for `mode run`.
  It names the program entrypoint to execute.
* `runArgs <stringLiteral>...` is valid only for `mode run`.
  Each argument is parsed using the directive `<stringLiteral>` rules from §T.3 and supplied as one command-line
  argument or equivalent run-task argument in the specified order.
  If `runArgs` is omitted, the default argument list is empty.
* `stdinFile <path>` is valid only for `mode run`.
  `<path>` is relative to the suite root.
  The contents of the named file are supplied to the program's standard input as raw bytes.
  If `stdinFile` is omitted, the default standard input is empty.
* `dumpFormat json` and `dumpFormat sexpr` select the default stage-dump format for assertions whose expected-file path
  does not itself determine the format.
  If no `dumpFormat` is specified, the default is `json`.
* `requires ...` directives are preconditions.
  If any required condition is not met, the test result is **unsupported**, not failed.

Portable capability names are:

* `stageDumps`
* `pipelineTrace`
* `incremental`
* `runTask`

Additional capability names are implementation-defined.

If the selected `mode`, backend profile, or requirements are mutually inconsistent, the test is ill-formed.

<!-- appendices.test_harness.assertion_directives -->
### T.5 Assertion directives

Unless otherwise stated, assertions are evaluated after the selected test mode has completed.

A test fails if any standard assertion is unsatisfied.

<!-- appendices.test_harness.assertion_directives.diagnostic_assertions -->
#### T.5.1 Diagnostic assertions

Supported directives:

```text
assertNoErrors
assertNoWarnings
assertErrorCount <n>
assertWarningCount <n>
assertDiagnostic <severity> <code>
assertDiagnosticNext <severity> <code>
assertDiagnosticAt <path> <severity> <code> <line>
assertDiagnosticAt <path> <severity> <code> <line> <column>
assertDiagnosticAt <path> <severity> <code> <startLine> <startColumn> - <endLine> <endColumn>
assertDiagnosticMatch <regex>
```

where `severity` is one of:

```text
error
warning
note
info
```

In this appendix, `<code>` denotes a diagnostic code as defined by §17.2.4.
Purely numeric codes are not valid standard-harness diagnostic codes.

Rules:

* `assertNoErrors` succeeds iff the test produces no diagnostics of severity `error`.
* `assertNoWarnings` succeeds iff the test produces no diagnostics of severity `warning`.
* `assertErrorCount n` succeeds iff exactly `n` diagnostics of severity `error` are produced.
* `assertWarningCount n` succeeds iff exactly `n` diagnostics of severity `warning` are produced.
* `assertDiagnostic severity code` succeeds iff at least one diagnostic with that severity and diagnostic code is
  produced.
* `assertDiagnosticNext severity code` is valid only in `.kp` source files.
  It applies to the first following line in the same file that is nonblank, not comment-only, and not itself a
  directive line.
  It succeeds iff at least one matching diagnostic has its primary origin in that file and begins on that target line,
  regardless of column.
* A harness MAY additionally accept `assertDiagnosticHere severity code` as a deprecated alias of
  `assertDiagnosticNext severity code`.
* `assertDiagnosticAt path severity code line` requires the primary origin of the matching diagnostic to begin on the
  given 1-based line in the file `path`, where `path` is relative to the suite root.
* `assertDiagnosticAt path severity code line column` additionally requires the primary origin to begin at the given
  1-based line and 1-based column.
* `assertDiagnosticAt path severity code startLine startColumn - endLine endColumn` requires the primary origin range to
  have exactly the given 1-based start and end positions.
* `assertDiagnosticMatch regex` succeeds iff at least one emitted diagnostic has a primary human-readable message text
  that matches `regex`.
  Here `regex` is the remainder of the directive body and is interpreted as an ECMAScript-style regular expression.
* In a `.kp` source file, an inline marker `--!! E001` at the end of a source line is the same-line counterpart of
  `assertDiagnosticNext error E001`.
  It succeeds iff at least one matching diagnostic has its primary origin in that file and begins on the marked source
  line, regardless of column.
  If several diagnostic codes follow one `--!!` marker, it expands to one same-line assertion per code.

The code-based diagnostic assertions `assertDiagnostic`, `assertDiagnosticNext`, `assertDiagnosticAt`, and `--!!` are
matched by severity and diagnostic code, not by the full human-readable message text.
`assertDiagnosticMatch` is available for suites that want an additional message-text check.

<!-- appendices.test_harness.assertion_directives.type_declaration_shape_assertions -->
#### T.5.2 Type and declaration-shape assertions

Supported directives:

```text
assertType <name> <typeExpr>
assertDeclKinds <kindList>
assertFileDeclKinds <path> <kindList>
```

where `kindList` is a comma-separated list of declaration kinds drawn from:

```text
import
export
fixity
signature
let
data
type
trait
instance
derive
effect
pattern
expect
```

Rules for `assertType`:

* `<name>` is resolved by ordinary name resolution in the test suite.
* If `<name>` is ambiguous, the assertion is ill-formed unless the name is sufficiently qualified to resolve uniquely.
* `<typeExpr>` is parsed as a type expression in the ordinary type grammar.
* The assertion succeeds iff the resolved declaration has a type and that type is definitionally equal to the parsed
  expected type.

Rules for `assertDeclKinds`:

* `assertDeclKinds` applies to the containing `.kp` file.
* It compares the sequence of top-level items in that file, in source order, after any optional module header and module
  attributes.
* Comments, blank lines, and test directives are ignored.

Rules for `assertFileDeclKinds`:

* It is the directory-suite form of `assertDeclKinds`.
* `<path>` is relative to the suite root.

In both declaration-kind assertions:

* a separate signature and a following `let` definition count as two items;
* the optional module header does not count as a declaration kind;
* imports, exports, and fixity declarations do count when present.

<!-- appendices.test_harness.assertion_directives.stage_dump_assertions -->
#### T.5.3 Stage-dump assertions

Supported directives:

```text
assertStageDump <checkpoint> equals <path>
```

Rules:

* `<checkpoint>` must name a valid compiler checkpoint under Chapter 17.
* `<path>` is relative to the suite root and names the expected golden file.
* The harness requests a stage dump for that checkpoint in the format determined as follows:
  * if `<path>` ends in `.json`, the requested format is JSON;
  * if `<path>` ends in `.sexpr`, the requested format is S-expression;
  * otherwise, the requested format is the suite's selected `dumpFormat`.
* Therefore one suite MAY assert both JSON and S-expression dumps by using golden files with the corresponding
  extensions.
* If the effective format is JSON, the expected file MUST contain JSON.
* If the effective format is S-expression, the expected file MUST contain an S-expression serialization.

Comparison semantics:

* JSON dumps are compared after parsing and canonicalization.
  Object-key order is ignored.
* S-expression dumps are compared after parsing and canonicalization.
* Implementations MUST NOT compare stage dumps as raw bytes alone for the purpose of this assertion.

<!-- appendices.test_harness.assertion_directives.run_assertions -->
#### T.5.4 Run assertions

Supported directives:

```text
assertStdout <stringLiteral>
assertStdoutContains <stringLiteral>
assertStderrContains <stringLiteral>
assertStdoutFile <path>
assertStderrFile <path>
assertExitCode <n>
```

Rules:

* These directives are valid only for `mode run`.
* The asserted execution is the one obtained after applying any configured `entry`, `runArgs`, and `stdinFile`
  directives from §T.4.
* In `assertStdout <stringLiteral>`, `assertStdoutContains <stringLiteral>`, and `assertStderrContains <stringLiteral>`,
  the argument is parsed using the directive `<stringLiteral>` rules from §T.3.
* `<path>` is relative to the suite root.
* The expected files are UTF-8 text files.
* Comparison normalizes line endings to LF before comparison.
* `assertStdout s` succeeds iff normalized standard output is exactly equal to the normalized string value `s`.
* `assertStdoutContains s` succeeds iff normalized standard output contains the normalized string value `s` as a
  substring.
* `assertStderrContains s` succeeds iff normalized standard error contains the normalized string value `s` as a
  substring.
* `assertExitCode n` compares the process or task exit code observed by the harness.
* For large or frequently changing outputs, `assertStdoutFile` and `assertStderrFile` remain the preferred golden-file
  forms.

<!-- appendices.test_harness.assertion_directives.trace_assertions -->
#### T.5.5 Trace assertions

Supported directives:

```text
assertTraceCount <event> <subject> <relop> <n>
```

where `relop` is one of:

```text
=  !=  <  <=  >  >=
```

Portable `event` names are:

```text
parse
buildKFrontIR
advancePhase
emitInterface
lowerKCore
evaluateElaboration
lowerKBackendIR
lowerTarget
reuse
verify
```

Portable `subject` names are:

```text
file
declaration
module
interface
KCoreUnit
KBackendIRUnit
targetUnit
```

Rules:

* This assertion is evaluated against the portable view of the pipeline trace of the current test execution.
* The portable view retains only trace steps whose event and subject classifications both use the portable names listed
  above.
* Each retained trace step contributes exactly one count to its `(event, subject)` pair.
* Implementations MAY record richer traces internally, but retries, helper passes, cache probes, batching choices, and
  other implementation-defined micro-steps MUST NOT create additional portable counts.
* The assertion succeeds iff the resulting count satisfies the specified relation.

<!-- appendices.test_harness.suite_level_behavior -->
### T.6 Suite-level behavior

Within a directory suite:

* configuration directives in `suite.ktest` apply to the whole suite;
* file-relative assertions written inline in a `.kp` file apply to that file;
* suite-wide assertions such as `assertType`, `assertStageDump`, and diagnostic assertions may appear inline or in
  `suite.ktest`.

If the same configuration key is specified more than once with different values, the suite is ill-formed.

<!-- appendices.test_harness.incremental_step_suites -->
### T.7 Incremental step suites

An incremental step suite is a directory with subdirectories named `step0`, `step1`, `step2`, and so on.

Rules:

* Step numbering MUST begin at `step0` and be contiguous.
* Each step directory is a complete suite root and may contain its own `.kp` files and optional `suite.ktest`.
* The harness executes steps in numeric order.
* Between steps, the harness preserves any caches, query results, module interfaces, KCore units, KBackendIR units, and
  target-lowering units that the compiler may legally reuse under Chapter 17.
* Assertions inside a step apply only to that step.

Cross-step assertions MAY appear in `incremental.ktest` using:

```text
assertStepNoErrors <step>
assertStepErrorCount <step> <n>
assertStepWarningCount <step> <n>
assertStepTraceCount <step> <event> <subject> <relop> <n>
```

Rules:

* `<step>` is a non-negative integer naming a step index.
* `assertStepTraceCount` is evaluated against the pipeline trace produced for that step.
  It uses the same portable trace-count semantics as `assertTraceCount`.
* The intended use of step-trace assertions is to test incremental reuse and invalidation behavior, for example reuse of
  interfaces, KCore units, KBackendIR units, and target-lowering units.

Each step is interpreted as a complete source snapshot, not as a textual patch.

<!-- appendices.test_harness.result_classification -->
### T.8 Result classification

A standard harness reports one of the following outcomes for each test:

* `pass`
* `fail`
* `unsupported`
* `harnessError`

Rules:

* `pass` means all standard assertions succeeded.
* `fail` means at least one standard assertion failed.
* `unsupported` means one or more `requires ...` preconditions were not satisfied, or the test used one or more
  extension directives unsupported by this harness.
* `harnessError` means the test itself was malformed, for example because of an unknown standard directive, invalid
  directive syntax, or an unreadable required golden file.

<!-- appendices.test_harness.determinism -->
### T.9 Determinism

For fixed source inputs, suite structure, build configuration, backend profile, and implementation version, the standard
harness result MUST be deterministic.

In particular:

* assertion outcomes;
* canonical stage-dump comparisons;
* diagnostic-code matching; and
* trace-count assertions

MUST NOT depend on worker count, scheduling order, hash-table iteration order, or platform-specific line-ending
conventions.

<!-- appendices.test_harness.example -->
### T.10 Example

The following is a valid inline test:

```kappa
module main

choose : Bool -> Int -> Int -> Int
let choose flag left right = if flag then left else right

message : String
let message = "hello"

--! assertNoErrors
--! assertType choose Bool -> Int -> Int -> Int
--! assertType message String
--! assertDeclKinds signature, let, signature, let
```
