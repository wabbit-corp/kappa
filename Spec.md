# Kappa Language Specification

![Temporary Logo](https://espunisinjapan.com/wp-content/uploads/2023/12/kappa-1024x1024.jpg)

> **Status:** Draft.  
> **Scope:** Core language, core syntax, and core semantics.

---

## 1. Design Principles

Kappa is a small, statically typed, dependently typed language. The primary design constraints:

- **Explicit is better than implicit.**
- **Simple is better than complex.**
- **Readability counts.**
- **There should be one – and preferably only one – obvious way to do it.**
- **Minimize unnecessary punctuation and syntax noise.**
- Default stance: **totality, purity, parametricity** are desirable and encouraged.



---

## 2. Modules, Files, and Acyclicity

### 2.1 Modules and files

- The module name of a source file is determined from its file path relative to a source root. For example:
  - `std/base.kp` → module `std.base`
  - `user/orders/list.kp` → module `user.orders.list`

Normative mapping:
* Let the file path be `P` and the source root be `R`.
* `P` must be under `R` and must end in the extension ".kp", otherwise it is not a source file.
* Let `S` be the relative path from `R` to `P`, with the ".kp" suffix removed.
* Path separators are normalized such that '\' is treated as '/'.
* Let `S = seg1/seg2/.../segn`.
* Each `segi` must match the identifier regex `[A-Za-z_][A-Za-z0-9_]*`, otherwise it is a compile-time error.
* Module name segments are case-sensitive. Implementations MUST reject a compilation unit that contains two source files
  whose path-derived module names are equal after case-folding but differ in case. For the comparison in this rule,
  implementations MUST compare module names after converting each segment to lowercase ASCII. Because path-derived
  segments are restricted to ASCII letters, digits, and `_`, no Unicode normalization is required. The canonical
  spelling is the spelling that appears in the first source file encountered during compilation. The original spelling
  is preserved for diagnostics and for the module header check below.
* The module name is `seg1.seg2. ... .segn` (join the segments with a single `.`).

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
  * In script mode, it MAY differ; if it differs, the header name becomes the module name of the file. The compiler MUST
    reject the program if another source file in the same compilation unit declares the same header name.
* If no module header is present, the module name is always the path-derived module name.

Standard module attributes:

* `@PrivateByDefault`: all top-level named items are private unless explicitly marked `public`.

### 2.2 Acyclic imports

- The module dependency graph formed by `import` statements and by any `export M...` statement that references another
  module **must be acyclic**.
- Implementations must reject programs with cyclic module dependencies.

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
              | moduleRef '.' nameRef
              | moduleRef '.(' importItem (',' importItem)* ')'
              | moduleRef '.*'
              | moduleRef '.*' 'except' '(' exceptItem (',' exceptItem)* ')'
importItem  ::= { 'unhide' | 'clarify' }* [namespace] nameRef [ctorAll]
namespace   ::= 'term' | 'type' | 'trait' | 'ctor'
ctorAll     ::= '(..)'
exceptItem  ::= [namespace] nameRef
```

The singleton form `import M.x` is sugar for `import M.(x)`, where `x` is a single `nameRef`.

Disambiguation rule (normative):

* The parser resolves dotted import forms using **longest-match** for `modPath`.
* It first attempts to consume the longest valid module path.
* Only if the remaining tokens after that path form a valid `'.' nameRef`, `'.(' ... ')'`, `'.*'`, or `'.* except
  (...)'` suffix does it treat the final segment as an import item rather than part of the module path.
* This rule is applied before any other interpretation of the dotted form.
* To force singleton-item import, write `import M.(x)`.

Constraints:

* `std.math` is a dotted module path (`ident("." ident)*`).
* URL imports use a **string literal** and are treated as modules by the implementation.
* In `... except (x, y)` each item may be unqualified or namespace-qualified as `term x`, `type y`, `trait C`, or `ctor
  K`.
* An unqualified `except` item excludes every imported binding of that name that the wildcard form would otherwise
  introduce.
* A qualified `except` item excludes only that namespace entry.

Imports are **not re-exported** by default (see `export` below).

Module aliases and qualification:

* `import M` brings the module name `M` into scope for qualified access (e.g. `M.x`).
* `import M as A` brings only the alias `A` into scope for qualified access (e.g. `A.x`). The name `M` is not brought
  into scope by this form.
* Selective imports (`import M.(...)`, `import M.*`, and `import M.* except (...)`) do not bring `M` into scope for
  qualified access. To enable qualified access, use a separate `import M` (or `import M as M`).
* `import "url" as A` brings only the alias `A` into scope for qualified access. The string literal itself never becomes
  a qualifier.


#### 2.3.1 Import item qualifiers (namespaces)

Kappa has multiple namespaces (§13). Import items may optionally specify which namespace (or subset) the item is
imported into:
```kappa
import std.list.(type List)
import std.list.(type List(..))
import std.eq.(trait Eq)
import std.base.(term println)
```

Valid qualifiers are:

* `term`  (term namespace)
* `type`  (type namespace)
* `trait` (constraint namespace; restricted to traits)
* `ctor`  (constructor namespace)

The bulk-constructor suffix `(..)` may appear only on an item explicitly qualified with `type`. Thus `type T(..)` is
valid, while `T(..)` and `ctor T(..)` are ill-formed.

If no qualifier is given (`import M.(x)` or its singleton sugar `import M.x`), the `nameRef` `x` is imported into every
namespace in which `M` exports that name **except the constructor namespace**. If this results in ambiguity at a use
site, that use is an error unless disambiguated (by qualification, expected kind/type, or explicit import qualifier).

Constructors:
* To import constructors unqualified, the import item must be explicitly qualified with `ctor`.
* The only built-in exception is the implicit prelude import of §2.6, which additionally brings a fixed prelude
  constructor subset into scope unqualified.
* Otherwise constructors are not imported as unqualified names by default and must be accessed through type scope
  (§13.2) or an explicit `ctor` import.
* As a concise bulk form, `type T(..)` imports `T` into the type namespace and all constructors of `T` into the
  constructor namespace as unqualified names. This form is valid only when `T` resolves to a constructor-bearing data
  type whose constructors are available to the importing module.

If `ctor x` is imported, the `nameRef` `x` becomes available as an unqualified constructor name in patterns and
expressions (subject to ambiguity rules). If `type T(..)` is imported, each constructor of `T` becomes available as an
unqualified constructor name in patterns and expressions (subject to ambiguity rules).

#### 2.3.1.1 `unhide` and `clarify` import items

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
importItem ::= { 'unhide' | 'clarify' }* [namespace] nameRef
namespace ::= 'term' | 'type' | 'trait' | 'ctor'
```

Rules:

* `unhide` and `clarify` are unsafe/debug import modifiers. Their detailed semantics and build gating are specified in
  §16.2-§16.3.
* In brief, `unhide` requests access to a private imported item, and `clarify` requests that an imported opaque item be
  treated as transparent for definitional equality in the importing module.
* `unhide` and `clarify` may be combined for the same item. Their order in the surface syntax is not semantically
  significant.
* Repeating the same modifier within one import item is a compile-time error.

### 2.3.2 URL imports, pinning, and reproducibility

URL imports are intended for scripts and quick one-offs, but Kappa also supports reproducible package-mode builds when
each imported URL is tied to immutable content.

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

where `exportSpec` is the corresponding `importSpec` form of §2.3 with `import` replaced by `export`. The singleton form
`export M.x` is sugar for `export M.(x)`, where `x` is a single `nameRef`, and `except` uses the same `exceptItem`
grammar as §2.3. In particular, `export M.(type T(..))` re-exports the type `T` together with all of its constructors.
The same longest-match dotted-path disambiguation rule as §2.3 applies to `exportSpec`.

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
  including through type scope selection (e.g. Rope.Node is not available).
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

#### 2.5.4 Modifier combinations

private and opaque may be combined:

```
private opaque let x = ...
```

In such cases private controls visibility; opaque controls transparency when the item is accessed via escape hatch
mechanisms.

### 2.6 Prelude interface (implicit, declarations only)

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

The exact contents of std.prelude are implementation-defined, but it must include:
* any declarations required by surface syntax (e.g. Bool and the meanings of True/False),
* the conventional prefixed-string handlers `f`, `re`, and `b` if prefixed strings are supported,
* fixity declarations for any operator tokens that the implementation expects to parse "out of the box"
(e.g. `+`, `*`, `==`, `and`, `or`, `..`, `..<`), consistent with infix gating (§3.5.3).

### 2.7 Prelude contents (normative minimum)

Implementations MUST provide a prelude module `std.prelude` that is implicitly imported (§2.6) and exports at least the
following:

In addition to the ordinary exports listed below, implementations MUST provide the compile-time row and label
declarations referenced by §§5.3.1-5.3.2 (`Label`, `EffLabel`, `ContainsRec`, `LacksRec`, `ContainsVar`, `LacksVar`,
`ContainsEff`, `LacksEff`, and `SplitEff`) whenever those names are user-nameable in source programs.

Types (type namespace):
```
Unit, Void, Bool, Char, String, Int, Nat, Integer, Float, Double, Real, Bytes, Ordering, SyntaxFragment,
Option a, Result e a, List a, Array a, Set a, Map k v,
Res a r, Match a r, Dec p, IO a, Eff r a, Regex, (=)
```

`Integer`, `Double`, and `Real` are ordinary user-facing numeric types exported by `std.prelude`.

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
(=).refl,
SyntaxFragment.Lit, SyntaxFragment.Interp,
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
not, and, or,
empty, (<|>), orElse,
negate,
absurd,
subst, sym, trans, cong,
floatEq,
runPure,
f, re, b,              -- conventional prefixed-string handlers
println, print
```

Several listed terms, such as `pure`, `(>>=)`, `empty`, `(<|>)`, and `orElse`, are overloaded member names induced by
the corresponding prelude traits (§12.2.1).

`floatEq : Float -> Float -> Bool` compares floating-point values using IEEE numeric equality (so `NaN` is never equal
and `+0.0` equals `-0.0`). It is not the default `(==)` for `Float`.

Traits (constraint namespace, restricted to trait):
```
Equiv, Eq, Ord, Show,
Functor, Applicative, Monad, Alternative,
Foldable, Traversable, Filterable, FilterMap, Monoid, Iterator, InterpolatedMacro,
FromComprehension,
FromInteger, FromFloat, FromString, FromStringType,
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

data (=) (@0 a : Type) (x : a) : a -> Type =
    refl : x = x

absurd :
    forall (@0 a : Type).
    Void -> a

subst :
    forall (@0 a : Type) (@0 P : a -> Type) (@0 x : a) (@0 y : a).
    (@0 p : x = y) -> P x -> P y

trait Alternative (f : Type -> Type) =
    empty : f a
    (<|>) : f a -> f a -> f a

    let orElse : f a -> f a -> f a =
        (<|>)

trait Iterator (it : Type) =
    Item : Type
    next : (1 this : it) -> Option (item : Item, rest : it)
```

`Void`, `absurd`, and `Dec` are the standard proof-oriented prelude basics:

* `Void` is the empty type.
* `absurd` is ex falso elimination from `Void`.
* `Dec p` packages a decision procedure for proposition `p`, carrying either positive evidence `Yes p` or a refutation
  `No (p -> Void)`.

`Res a r` is the standard prelude wrapper for threading a linear resource `r` alongside an ordinary value `a`.

The standard prelude definition of `Applicative`, and the fact that `Monad` refines `Applicative` in `std.prelude`, are
specified in §12.1 (with Appendix G providing the `ApplicativeDo` amendment that relies on that relationship).

Instances: All canonical instances of the above traits for the above types (e.g. coherent evidence for `Equiv Int`, `Eq
Int`, `Ord Int`, `Show String`, `Monad IO`, `MonadRef IO`, `Functor (Eff r)`, `Applicative (Eff r)`, `Monad (Eff r)`,
...).

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

---

## 3. Lexical Structure

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

  Backtick identifiers are part of the term namespace.

### 3.2 Keywords

Non-exhaustive but important list of soft keywords and contextual reserved tokens:

```text
let, let?, in, if, then, elif, else,
match, case, is, impossible,
try, except, finally,
data, type, trait, module,
import, export, as, except,
do, block, return,
forall,
assertTotal, expect, instance, derive, effect, handle, deep, pattern,
infix, postfix, prefix, left, right,
var, while, break, continue, using, inout,
yield, for, for?, group, by, distinct, order, skip, take, top, join, left, asc, desc,
public, private, opaque, unhide, clarify,
?., ?:
```

Effect-row surface syntax (§5.3.2) introduces no additional keywords; it uses reserved punctuation tokens instead.

Keywords are **soft** (contextual) keywords:

* The lexer recognizes the keyword tokens, but implementations must permit their use as ordinary identifiers in contexts
  where a keyword is not syntactically expected.
* Example: `let type = 42` is permitted (where `type` is a term name), while `type Foo = ...` uses `type` as a keyword.

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

### 3.5 Operator identifiers and fixity

Kappa supports symbolic operator identifiers (e.g. `+`, `*`, `==`, `..`), which live in the term namespace.

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

#### 3.5.1.1 Operator sections

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
* `~=` is recognized as an ordinary operator token in preference to standalone `~`, so the `Equiv` operator remains
  available even though `~` itself is reserved for `inout` call sites (§8.8).
* `<[` and `]>` are recognized as single reserved tokens in preference to `<` plus `[` and `]` plus `>`.
* In particular, `?.` is recognized before `?` plus `.`, `?:` is recognized before `?` plus `:`, and a standalone `?`
  remains available as an ordinary operator token (for example, a user-defined postfix operator per §3.5.2).

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

## 4. Literals

### 4.1 Numeric literals

#### 4.1.1 Integers

Forms:

* Decimal: `0`, `1`, `42`, `9_223_372_036_854_775_807`
* Hex: `0xFF`, `0xDEAD_BEEF`
* Octal: `0o755`, `0o1_2_3`
* Binary: `0b1010`, `0b1_0_1_0`

Underscores `_` are allowed between digits for readability; they have no semantic effect.

#### 4.1.2 Floating-point (`Float`)

Decimal floating-point numbers:

* `3.14`
* `10.2e-2`
* `6.022_140_857E23`

General form:

* `[digits] '.' [digits] [exponent]`
* `[digits] [exponent]` where exponent is `e` or `E` followed by an optional sign and digits.

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

#### 4.1.4 Sign

`-` is a **unary operator**, not part of the literal.

* `-123` is parsed as `negate 123`
* `-3.14e2` is `negate (3.14e2)`

Operator precedence rules determine association (see expressions).

#### 4.1.5 Literal typing (normative)

This section applies to numeric literals **without** a suffix. If a numeric literal has a suffix, §4.1.6 takes priority
instead.

**Integer literals (without suffix)**: An integer literal `n` has elaborated type `T` where `T` is a type for which an
instance `FromInteger T` is in scope (§2.7). The literal elaborates to `FromInteger.fromInteger @T n` where `n :
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

### 4.2 Boolean literals

* `True`
* `False`

### 4.3 String literals

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

#### 4.3.4 Prefixed strings


A **prefixed string literal** has the form:

```kappa
prefix"..."
prefix"""..."""
prefix#"..."#
prefix#"""..."""#
```

Where `prefix` is an identifier (including backtick identifiers). It is resolved by ordinary term name resolution at the
use site; it has no special parser status beyond introducing this literal form.

The raw prefixed forms permit any positive number of `#` characters, with the same matching rule as §4.3.2. For example:

```kappa
re#"\d+\s+\w+"#
sql##"select "#col" from t"##
```

Ordinary prefixed strings support interpolation:
 * `$name` inserts a variable name and is sugar for `${name}`.
 * `${expr}` inserts an arbitrary expression.
 * `${expr : fmt}` inserts expr using format string fmt (format mini-language is implementation-defined).

A literal `$` may be written as `\$`.

Raw prefixed strings do not support interpolation. In a raw prefixed string, `$`, `${`, and `\` are ordinary characters
in the literal payload.

Resolution and typing:

* Let `prefix` resolve to a term `p` in scope at the use site.
* Any prefixed string literal requires `p` to resolve to a term of type `Dict (InterpolatedMacro t)` for some result
  type `t`.
* If resolution fails, or if the resolved term does not have a compatible type, compilation fails with a compile-time
  error.

The prelude provides:

```kappa
data SyntaxFragment : Type =
    Lit    (s : String)
    Interp (@t : Type) (e : Syntax t)

trait InterpolatedMacro (t : Type) =
    buildInterpolated : List SyntaxFragment -> Syntax t

trait FromComprehension (f : Type -> Type) =
    fromComprehension : forall a. Syntax (f a) -> f a
```

**Note**: The `Syntax` value passed to `fromComprehension` is the result of applying the normative desugaring rules from
§10.10. Implementations of `FromComprehension` may inspect this `Syntax` (via the reflection API of §5.8.5) to perform
custom optimizations or code generation.

Semantics:

* A non-interpolated prefixed string `p"raw"` elaborates to:

  ```kappa
  $(p.buildInterpolated [Lit "raw"])
  ```

* An interpolated prefixed string `p"pre${x}post"` elaborates to:

  ```kappa
  $(p.buildInterpolated [Lit "pre", Interp @Tx '{ x }, Lit "post"])
  ```

  where `p : Dict (InterpolatedMacro t)` and `Tx` is the inferred type of `x`.
* A formatted interpolation `${expr : fmt}` is first rewritten to an implementation-defined ordinary expression and then
  contributes an `Interp` fragment for that rewritten expression.
* A raw prefixed string such as `p#"..."#` elaborates to:

  ```kappa
  $(p.buildInterpolated [Lit "..."])
  ```

  where the `Lit` payload is the exact raw contents after applying the multiline dedent rule of §4.3.3 when the raw form
  is triple-quoted.
* Prefixed DSL strings therefore elaborate uniformly through `Syntax`, allowing macro implementations to inspect the
  `Syntax` payloads and produce typed outputs such as parameterized SQL or typed HTML.

The standard `f`, `re`, and `b` prefixes supplied by `std.prelude` are ordinary terms participating in this mechanism.
They receive no special parser treatment.

#### 4.3.5 Type-level string parse

Kappa provides a built-in intrinsic:

```kappa
type"..."
```

This form is valid only in a type position. It elaborates at compile time to the type produced by a `FromStringType`
parser.

```kappa
trait FromStringType =
    parseType : String -> Type
```

Typing and elaboration rules:

* `type"..."` requires an implicit `FromStringType` in scope.
* The elaborated type is obtained by evaluating `FromStringType.parseType` on the literal contents during elaboration.
* Implementations MUST evaluate `parseType` under the same elaboration-time restrictions as §5.8.6.

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

## 5. Types, Universes, and Records

### 5.1 Universes

Kappa is dependently typed with a stratified universe hierarchy.

* There is an infinite family of universes: `Type0`, `Type1`, `Type2`, ...
* Universe typing:

    * `Type0 : Type1`
    * `Type1 : Type2`
    * in general, `Type u : Type (u+1)` at the meta-level.

Surface syntax:

* `Type0`, `Type1`, `Type2`, ... denote fixed universe levels.
* `Type` is **universe-polymorphic** surface syntax:
    * each occurrence of bare `Type` introduces a fresh universe level metavariable (implementation-defined notation,
      e.g. `?u`), meaning "some universe level inferred by the compiler".
* Advanced code may bind an explicit universe variable and reuse it:

  ```kappa
  forall (u : Universe) (a : Type u) (b : Type u). ...
  ```

  In `Type u`, the identifier `u` is a user-bound variable of the built-in sort `Universe`. This is the surface
  mechanism for forcing multiple occurrences to live in the same universe level.
* `*` is syntactic sugar for `Type`.

#### 5.1.1 Cumulativity

Universes are **cumulative**:

* if `u ≤ v`, then `Type u` may be used where `Type v` is expected.

Implementations may realize this as an implicit coercion/subtyping rule between universes.

#### 5.1.2 Universe inference (sketch)

Elaboration generates constraints on universe metavariables (e.g. `?u ≤ ?v`, `?u < ?v`) from typing. Explicit universe
variables introduced by `forall (u : Universe)` or an ordinary binder `(u : Universe)` participate in the same
constraint solving. Occurrences of `Type u` therefore share exactly the same user-written universe variable rather than
introducing fresh metavariables. If the constraints are unsatisfiable, compilation fails. Unconstrained universe
metavariables may be generalized at top-level (implementation-defined).

### 5.1.3 Constraints and dictionaries

In addition to the universe hierarchy `Type0`, `Type1`, ... and the sort `Constraint`, Kappa includes the built-in sorts
`Universe`, `Quantity`, and `Region`.

* `Universe` classifies universe levels that may appear in explicit forms such as `Type u`.
* `Universe` is a valid target for universal quantification.
* In v0.1, the intended surface use of a `Universe` variable is as the argument of `Type`; ordinary computation on
  universe levels is not otherwise specified.

* `Quantity` classifies exact usage intervals and the borrow mode `&`.
* `Quantity` is a valid target for universal quantification.
* `Region` classifies explicit borrow lifetimes that may be named in surface types when a borrow relationship must cross
  an interface boundary.
* `Region` is a valid target for universal quantification.

Kappa also has a separate sort `Constraint`.

* A trait declaration `trait Tr ... = ...` introduces a trait constructor `Tr` whose fully applied applications have
  sort `Constraint`.
* A constraint is not a `Type`.
* Constraints may appear:
  * in implicit binders `(@x : C)`,
  * in constraint arrows `C => T`,
  * in instance heads, and
  * as arguments to built-ins that abstract over constraints.

Built-in reification:

```kappa
Dict : Constraint -> Type
```

`Dict C` is the explicit dictionary type corresponding to the constraint `C`.

Usage rules:

* A value of a concrete constraint `C` may only be bound implicitly.
* An explicit parameter, field, or result may not have type `C`; use `Dict C`.
* There is an implicit coercion from coherent evidence `ev : C` to `Dict C`.
* There is no coercion from `Dict C` to `C`.
* `Dict C` never participates in implicit resolution.
* Coherent constraint evidence is proof-irrelevant for typechecking and coherence.
* The runtime representation of constraint evidence is implementation-defined.
* Explicit `Dict C` values are ordinary runtime values unless eliminated by specialization, inlining, or dead-code
  erasure.
* Implicit evidence may be erased when the implementation proves that it is unused after elaboration.
* A binder may range over constraints themselves, e.g. `(c : Constraint) -> ...`; this quantifies over constraints and
  does not introduce an explicit value of type `c`.

### 5.1.4 Erasure and elaboration time

`Type` and universe terms are compile-time entities. Kappa does not require runtime type information.

* Runtime erasure is governed by quantities (§5.1.5, §14.4), subject to the special treatment of constraint evidence and
  `Dict` values in §5.1.3: quantity-0 binders and their corresponding arguments are erased unless elaboration requires
  retaining evidence to realize trait-member selection.
* Implementations may provide library mechanisms to reify type information explicitly when needed (e.g. `Dict C`, quoted
  representations), but there is no implicit runtime reflection.

### 5.1.5 Quantities

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
the supplied expression is a borrowable stable expression of type `T`, the compiler may insert a temporary borrow.

This is a separate elaboration rule. It does not modify the quantity-satisfaction relation `⊑`.

Borrowable stable expressions in v0.1 are at least:

* variables,
* record projections from borrowable roots,
* constructor-field projections from borrowable roots, and
* explicit hidden temporaries introduced by elaboration.

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

### 5.1.6 Borrow lifetimes and escape prevention

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

If a borrowed local binding has the form `let & pat = expr` and `expr` is not already a borrowable stable expression,
elaboration first introduces a fresh hidden temporary `__tmp` scoped exactly to the body of that local binding (or, in a
`do` block, to the remaining do-items). That hidden temporary becomes the borrow root for `pat`.

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

### 5.1.7 Disjoint path borrowing for records

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

Why this does not require explicit region annotations on arrows:

The region environment is an elaboration-time property, like the quantity constraints of §5.1.5 and the implicit
evidence of `Constraint` sorts. It is erased after typechecking and does not affect runtime representation. For
region-closed definitions, the user-written surface type `Unit -> IO Int` therefore remains unchanged; the extra
information lives only in the compiler's internal representation of the term during elaboration. If elaboration would
leave a rigid region variable free in the interface type or hidden closure environment of a value that escapes its
introducing scope, the program is rejected rather than exported with an incomplete type.

This is analogous to how `runST` in §8.5.3 uses rank-2 polymorphism to prevent `STRef s` escape without requiring the
user to write region variables on every arrow.

Module interfaces and separate compilation:

* A top-level definition is well-formed for export only if every region mentioned by its elaborated interface or hidden
  closure environment is either:
  * a fresh anonymous region that is fully discharged before export, or
  * an explicit region variable bound in the exported type, typically by `forall (s : Region)`.
* Therefore a definition may return or export a closure capturing a borrowed value only when that closure's hidden
  region environment mentions explicit region variables already bound in the exported type. Any remaining anonymous
  `rho` still causes a skolem-escape error in the defining module.
* Module interfaces record the ordinary exported type together with the hidden region-environment summary parameterized
  by any explicit `Region` binders. Downstream modules use that summary during escape checking even though surface arrow
  syntax remains unchanged.
* Ordinary module interfaces never expose anonymous rigid region variables. Consequently, cross-module higher-order APIs
  may abstract over borrowed captures only through explicit `Region` binders rather than by laundering hidden local
  regions across the module boundary.

Example:

```kappa
makeGetter :
    forall (s : Region) (a : Type).
    (&[s] x : Box a) -> Unit -> a
```

The returned closure has surface type `Unit -> a`, but its hidden region environment mentions `s`. A caller may use or
store that closure only within the same region `s`; attempting to let it escape farther is a standard skolem-escape
error. At a call site, an explicit region variable such as `s` may be instantiated by a caller-local rigid region `rho`.
This is the mechanism by which exported region-polymorphic APIs accept ordinary local borrows without exposing anonymous
regions in source syntax.

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
  (&[s] x : A) -> B   -- where s is a bound variable of sort Region
  (q x : A) -> B     -- where q is a bound variable of sort Quantity
  (x : A) -> B       -- defaults to ω
  ```

  which denotes a function whose result type may depend on `x`.

* Non-dependent arrow is sugar:

  ```kappa
  A -> B    ==    (_ : A) -> B
  ```

### 5.3 Universal quantification

`forall` is syntactic sugar over Pi-types:

```kappa
forall a. T        ==   (@0 a : Type) -> T
forall (a : S). T  ==   (@0 a : S) -> T
```

This includes quantification over the built-in sorts `Quantity` and `Region`, and `Universe`, e.g. `forall (q :
Quantity). T`, `forall (s : Region). T`, and `forall (u : Universe). T`.

Examples:

```kappa
forall a. a -> Int
forall (u : Universe) (a : Type u) (b : Type u). (a -> b) -> a -> b
forall (n : Nat). Vec n Int -> Int
forall (s : Region). (&[s] buf : Buffer) -> Unit -> Int
```

### 5.3.1 Rows and labels

Kappa has separate row kinds:

```text
RecRow   -- record rows
VarRow   -- union/variant rows
EffRow   -- effect rows
```

Rows of different kinds never unify.

Record labels are identifiers in the label namespace. Effect labels are identifiers in the effect-label namespace.
Effect interfaces are ordinary named constructors in the type namespace.

The implementation provides compile-time label domains `Label` and `EffLabel` classifying those namespaces, together
with kind-specific row constraints:

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

A row variable must have one of the kinds `RecRow`, `VarRow`, or `EffRow`. Row variables of different kinds are never
interchangeable.

### 5.3.2 Effect-row surface syntax

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

### 5.4 Union types

Kappa supports union types (also called variant types in this context) via the `(| ... |)` syntax. A union type denotes
a value that belongs to *exactly one* of the listed types.

```kappa
type U = (| Int | String | Error |)
type OptionalInt = (| Unit | Int |)
```

#### 5.4.1 Formation

```text
unionType ::= '(|' type ('|' type)* '|)'
            | '(|' type ('|' type)* '|' rowVar '|)'
```

```kappa
Variant : VarRow -> Type
```

Conceptually, a union type elaborates to `Variant r` where `r : VarRow` is a row whose entries are member types rather
than labelled entries.

Row equality for `VarRow` is modulo permutation and duplicate removal. Canonical order is the lexicographic order of the
string representation of the member types. This canonical order is used for row normalization and definitional equality,
not for assigning row-local runtime tag ordinals.

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

#### 5.4.5 Row polymorphism

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

#### 5.4.6 Nullary / singleton arms

```kappa
data Missing : Type =
    Missing

type Optional a = (| Missing | a |)
```

`Missing` here is an ordinary singleton type declared separately. If a dedicated nullary branch name is desired, it must
first be introduced as an ordinary singleton type (or `Unit` may be used when no dedicated name is needed). There is no
separate tag namespace: every arm of a union is a type.

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

#### 5.4.8 Interaction with GADTs / indexed types

Union types may be indexed. An injection such as `(| v : Vec n a |)` is valid when the expected union type contains `Vec
n a` among its member types. Exhaustiveness and reachability are determined by index unification exactly as in §7.5.1.

### 5.4.9 Optional-type sugar

For any type expression `T`, the postfix form `T?` is syntactic sugar for `Option T`, where `Option` is the prelude type
of §2.7.

Grammar (amends the type grammar):

```text
typeAtom     ::= ... existing atoms ...
typePostfix  ::= typeAtom ('?')*
typeApp      ::= typePostfix typePostfix*
typeArrow    ::= typeApp ('->' typeArrow)?
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

### 5.5 Records (named tuples)

Records are the single "struct-like" construct in Kappa.

#### 5.5.1 Syntax

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
recordFieldDecl ::= [quantity] ident ':' type
```

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

#### 5.5.1.1 Record field order and lawful reorderings

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

#### 5.5.2 Formation

Closed record types have exactly the listed fields and may be dependent telescopes.

Open record types have the listed fields plus any fields supplied by the residual row `r : RecRow`. Their explicitly
listed fields may also form a dependent telescope, provided the dependency graph over those explicit fields is acyclic.

Restrictions for open records:

* dependencies among the explicitly listed fields are governed by the same acyclic sibling-dependency graph as in
  §5.5.1.1,
* the residual row `r` may not be referenced in explicit field types, and
* no explicit field type may rely on any field that would be supplied only by `r`.

Record types and values are usable as Σ-types (dependent pairs), subject to the lawful-reordering rule above.

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

#### 5.5.4 Field projection

```kappa
rec.ℓ
```

Projects field `ℓ` from a record whose row is `r : RecRow`. Well-typed iff `ContainsRec r ℓ T` for some `T` (the
inferred result type).

The projection yields the field value at the field's declared quantity.

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

#### 5.5.5 Record update

Records are immutable values. Record update produces a new record with selected fields replaced:

```kappa
let p : (x : Int, y : Int) = (x = 1, y = 2)
let p2 = p.{ y = 99 }          -- (x = 1, y = 99)
```

Syntax:

```text
recordUpdate ::= expr '.{' updateField (',' updateField)* '}'
updateField  ::= [ '@' ] ident '=' expr
```

A record update must contain only `=` fields. A record update containing `:=` is ill-formed.
If a record field is declared as an implicit field `@label : T`, it may be explicitly updated with surface syntax
`@label = expr`.
Unlike record literals and constructor applications, field punning is not permitted in record updates. Every update
field MUST provide an explicit right-hand side. Writing `r.{ x }` as shorthand for `r.{ x = x }` is a compile-time
error.

Normative elaboration:

Let the scrutinee have record type `R`, and let `R` have canonical dependency-respecting field order `(g1, g2, ..., gn)`
as determined by §5.5.1.1.

Then a record update

```kappa
r.{ f1 = e1, ..., fk = ek }
```

elaborates to a full record literal in that canonical order:

* if `gi` is explicitly updated, the elaborated literal uses the supplied expression for `gi`;
  implicit fields are updated with surface syntax `@gi = expr`,
* if `gi` is omitted, the elaborated literal uses the projection `r.gi`, including omitted implicit fields.

The resulting full record literal is then typechecked against the original record type `R` using the ordinary
dependent-record rules of §5.5.1.1 and §5.5.3.

Consequences:

* There is no separate direct-vs-transitive dependency rule for updates. Whether an omitted field remains valid is
  determined solely by ordinary typechecking of the elaborated full record literal.
* If an omitted field depends on updated fields and the copied projection `r.field` no longer has the required type
  after substitution of the updated values, the update is rejected.
* An implicit field may be repaired explicitly in the same update. For example, `r.{ id = 2, @ok = newProof }` is valid
  when `newProof` has the type required by the updated record.
* If an omitted field path is currently in the consumed state of the scrutinee, the implicit filler `r.field` is
  ill-typed; that field must therefore be supplied explicitly.

Field references inside supplied update expressions:

* `this` refers to the evolving updated record prefix in canonical dependency-respecting order, not to the original
  record `r`.
* Therefore a supplied expression may refer to any earlier field in that canonical order, whether that earlier field
  came from an explicit update or from an implicit copied projection `r.field`.
* A bare identifier `label` is shorthand for `this.label` only when that reading is unambiguous; otherwise `this.label`
  is required.

Source order:

* The surface order of update fields is not semantically significant. Elaboration uses the canonical
  dependency-respecting order of the record type.

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

#### 5.5.6 Row extension (new)

```kappa
rec.{ ℓ := e }
```

`rec.{ ℓ := e }` is row extension.

Syntax:

```text
rowExtension   ::= expr '.{' extensionField (',' extensionField)* '}'
extensionField ::= ident ':=' expr
```

A row extension must contain at least one `:=` field and must contain only `:=` fields. A row extension containing `=`
is ill-formed.

Typing:

* If `rec` has a closed record type and already contains field `ℓ`, row extension is a compile-time error; use update
  syntax `ℓ = ...`.
* If `rec` has type `(fields | r)`, then row extension is well-typed only if `LacksRec r ℓ` holds and `ℓ` is not among
  the explicitly listed `fields`.

Result type:

* The result has type `(fields, ℓ : typeof e | r)`, normalized by the ordinary record canonicalization rules.

#### 5.5.7 Row polymorphism

A function `(name : String | r) -> String` is polymorphic in the residual row `r : RecRow` with the implicit constraint
`LacksRec r name`.

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

#### 5.5.9 Implicit record fields and existential unpacking

Record types may declare implicit fields using the `@` prefix. This mechanism formalizes dependent pairs, packed
existentials, and refined types.

Syntax and formation:

```kappa
(id : Int, @ok : id > 0)
(a : Type, @tc : Eq a, value : a)
```

* A field declared as `@label : T` is an implicit field.
* Sort restriction:
  * The type `T` of an implicit field must elaborate either to `Constraint`, to some universe `Type u`, or to a boolean
    proposition accepted by the coercion rule of §5.6.2 (so a bare boolean field type `@p : b` elaborates to `@p : b =
    True`).
  * Ordinary term-level data types such as `String`, `Bytes`, or `IO a` cannot be marked as implicit record fields.
* Implicit record fields are the record-field analogue of implicit binders. Accordingly, §5.1.3's prohibition on
  explicit fields of constraint type does not apply to fields marked with `@`.
* Quantity:
  * Implicit record fields follow the same defaulting rule as implicit binders (§7.3).
  * If the field type elaborates to `Constraint` or to `Type u`, the field defaults to quantity `0` and is erased at
    runtime.

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

Functions returning records with implicit fields act as existential packages. The caller need not manually route the
hidden type, dictionary, or proof; after binding the returned record, the unpacked implicit context may satisfy
subsequent implicit obligations automatically.


### 5.6 Propositions via booleans and propositional equality

Kappa supports propositional equality as a built-in inductive family.

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

### 5.7 Monadic splicing (`!`)

The prefix operator `!` is used only within a `do` block as a monadic splice.

Compile-time term and type generation use `Syntax` and top-level `$(...)` splicing (§5.8.2), not `!`.

#### 5.7.1 Term-level `!` in monadic contexts

Within a `do` block (§8), `!e` runs the monadic computation `e` and yields its result in an expression context.

Example:

```kappa
let main : IO Unit = do
let x = !readInt
let y = !readInt
println (x + y)
```

Desugaring sketch:

* Each `!e` introduces an implicit bind to a fresh temporary and uses that temporary at the `!e` site.

### 5.8 Hygienic syntax values and macros

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

#### 5.8.2 Top-level syntax splicing

Outside quoted blocks, `$(s)` is an elaboration-time splice in term and type positions.

* In a term position expecting type `t`, `$(s)` requires `s : Syntax t`.
* In a type position, `$(s)` requires `s : Syntax Type`.
* `$(s)` consumes `Syntax`, not `Code`. Staged code is executed only via `runCode` on `ClosedCode` (§5.9.4).
* `$(s)` is the only elaboration-time splice form for `Syntax`.
* `!` remains reserved for the monadic splice form inside `do` blocks (§5.7.1) and is not a synonym for `$(...)`.

#### 5.8.3 Macros as ordinary functions

A macro is an ordinary elaboration-time function whose result type is `Syntax t`:

```kappa
myMacro : Syntax Int -> Syntax Int
let myMacro e = '{ ${e} + 1 }

let x = $(myMacro '{ 10 })
```

There is no separate macro namespace; macros are ordinary term definitions invoked through elaboration-time splicing.

#### 5.8.4 Hygiene

Syntax quotes are hygienic.

* Quoted binders are hygienic.
* Spliced syntax preserves its own binding structure.
* Generated binders are alpha-renamed as needed to avoid capture.

#### 5.8.5 Reflection API

Implementations MUST provide a typed reflection API sufficient to inspect and construct syntax values.

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

**Termination checking.** Macro definitions are subject to the same termination checking rules as ordinary top-level
definitions (§6.4), including the unsafe/debug `assertTotal` escape hatch of §16.4. The elaboration-time evaluator uses
the same termination checker as the rest of the language. A macro accepted only via `assertTotal` is still required to
be deterministic with respect to its inputs and the macro input transcript. Non-termination of a macro during
elaboration is a compiler error (the implementation may hang, time out, or report a diagnostic).

These restrictions are enforced by the elaboration-time evaluator (§17.3.2).
A macro or splice that violates them is a compile-time error.

### 5.9 Staged code values

#### 5.9.1 `Code` and `ClosedCode`

Kappa provides separate built-in types for generative staged code:

```kappa
Code       : Type -> Type
ClosedCode : Type -> Type
```

`Code t` is typed staged code. It is generative and is not inspectable as ordinary syntax. Use `Syntax t` for
inspectable macro ASTs.

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

#### 5.9.3 Cross-stage persistence

Cross-stage persistence is lift-based:

```kappa
trait Lift (a : Type) =
    liftCode : a -> Code a
```

A present-stage value may occur inside `.< ... >.` only if it is inserted by `liftCode`, either explicitly or via
elaboration of a simple variable occurrence.

#### 5.9.4 Running staged code

Staged code may be executed only when closed:

```kappa
runCode : ClosedCode t -> IO t
```

Implementations MUST reject, or fail before execution on, any attempt to run non-closed code.

#### 5.9.5 Scope safety

Implementations MUST prevent scope extrusion for `Code`.

* A splice that would produce ill-scoped code is a stage error.
* Implementations MAY detect this statically or with a generation-time scope-extrusion check.


---

## 6. Declarations and Definitions

### 6.1 Term declarations vs definitions

There are three distinct forms:

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
   [public|private] [opaque] let name (x : A, y : B) : R = body
   ```

   Any term definition must begin with `let`, except inside `let ... in` (see below).
   
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
    * Apart from `inout`, named-function parameter binders use the same binder forms as lambda binders (§7.2), including
      receiver-marked binders such as `(this : T)`, `(q this : T)`, `(this x : T)`, and `(q this x : T)`.
    * If a definition has the form `let name = \binders -> body` or `let name : T = \binders -> body`, where `name` is a
      simple binder, the lambda is treated as a named function body named `name` for `return` target resolution (§8.4).
      This applies both at top level and for local `let` bindings.
    * In a named `let` definition, the body after `=` may be written as an indented pure block suite, which elaborates
      to `block ...` as specified in §6.3.1.

3. **Pattern definition (active pattern):**

   ```kappa
   [public|private] pattern name binders... (scrutinee : A) : R = expr
   ```

   * A `pattern` definition is a top-level term definition that also marks `name` as eligible for pattern-head use under
     §7.7.
   * The final explicit binder is the scrutinee consumed when the pattern is matched; preceding explicit binders are
     pattern arguments.
   * Active patterns live in the term namespace and are imported/exported as ordinary terms.
   * Active patterns are pure functions; `R` must not be a monadic type.

#### 6.1.1 Irrefutable patterns (for `let` bindings)

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

### 6.2 Top-level signatures + definitions

Common forms:

```kappa
let foo (x : Int) : Int = x + 1

foo : Int -> Int
let foo x = x + 1
```

* The language does not designate the separate-signature form as canonical.
* Inline annotations on a `let` definition are equally standard and are often clearer for short functions.
* If an implementation requires a top-level function annotation, that requirement may be satisfied either by a separate
  signature or by an inline annotation on the definition.
* For **simple values**, `let x = 42` may omit the type and rely on inference.

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
  bindPat   ::= [quantity] pattern
  localBind ::= 'let' bindPat [':' type] '=' expr
  monadBind ::= 'let' bindPat '<-' expr
  usingBind ::= 'using' pattern '<-' expr
  ```

  Because `using` always binds its pattern at borrowed quantity `&`, explicit quantity markers are not permitted in
  `usingBind`.

* Each binding is:

  ```kappa
  bindPat [ : Type ] = expr
  ```

* Followed by `in` and a single expression.

`let ... in` is itself an expression; it can appear anywhere an expression is allowed.

Refutable bindings (`let?`) are permitted only inside `do` blocks (§8.2.1) and comprehensions (§10.4.1). They are
rejected in ordinary `let ... in` expressions.

Pattern bindings in `let ... in`:

* In `bindPat`, a quantity annotation applies uniformly to every variable bound by the underlying pattern.
* `let & pat = expr` is legal and binds every variable introduced by `pat` at borrowed quantity `&`. It introduces
  borrowed views of the underlying value, is used by the normative desugaring of `using` (§8.7.4), and is subject to the
  borrow-escape rules of §5.1.6. If `expr` is not already a borrowable stable expression, elaboration first introduces a
  fresh hidden temporary root scoped to the body:

  ```kappa
  let 1 __tmp = expr
  let & pat = __tmp
  in body
  ```

  where `__tmp` is fresh, inaccessible to user code, and serves as the borrow root for the duration of `body`.
* Each binding `bindPat [ : Type ] = expr` must use an irrefutable underlying pattern (§6.1.1).
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

Elaboration (schematic):

* If `bindPat` carries quantity `&`, the binding is elaborated as a borrowed binding of the underlying pattern `pat0`
  against a borrowable stable root expression, with each variable introduced by `pat0` bound at borrowed quantity `&`.
  If `e` is not already a borrowable stable expression, elaboration first introduces the fresh hidden root described
  above and then performs that borrowed binding against the hidden root.
* If `bindPat` does not carry quantity `&`, then `let bindPat = e in body` elaborates as `match e; case pat0 -> body`,
  where `pat0` is the underlying pattern of `bindPat`. This is only permitted because that underlying pattern is
  required to be irrefutable.

### 6.3.1 Pure block expressions

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
                | localInstanceDecl
                | localDeriveDecl
                | localImportDecl
                | localFixityDecl
```

Here `localLetItem` is the irrefutable local-binding form of §6.3; `localSignature` and `localNamedDef` are the local
analogues of the signature and named-`let` forms of §6.1-§6.2; and the remaining items use the same surface syntax as
their top-level counterparts, minus the top-level-only forms and modifiers listed below.

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
  * local `data`, `type`, `trait`, `instance`, and `derive` declarations,
  * local `import` and fixity declarations.
* `block` and `do` are the two block-scope constructs of the language. They share the local declaration forms above.
  `do` additionally admits the effectful items of §8.2.
* The following forms remain top-level only: `module`, `export`, `expect`, `effect`, `pattern`, and the modifiers
  `public`, `private`, and `opaque`.
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

### 6.4 Totality and unfolding

Kappa is total by default.

Normative rule:

* All definitions that may participate in typechecking via definitional equality must be terminating.

Termination checking:

* Recursive definitions are permitted only when the termination checker accepts them (e.g. structural recursion), or via
  the unsafe/debug escape hatch `assertTotal` as specified in §16.4 and gated by §16.2.
* Elaboration may also introduce internal recursive helpers required by normative desugarings. Such helpers are not user
  definitions and do not require surface `assertTotal`. They are admissible only when the relevant desugaring section
  provides a semantic argument that the recursion is structurally decreasing, or when the implementation lowers the
  construct directly to an internal primitive with observationally equivalent behavior.
* The surface declaration form `assertTotal let ...` and its detailed unsafe/debug semantics are specified in §16.4.

Recursion policy:

There is no `let rec`.

A top-level binding is permitted to be (mutually) recursive only if it has a preceding top-level signature declaration.

```kappa
even : Nat -> Bool
odd  : Nat -> Bool

let even n = ...
let odd  n = ...
```

Within such a definition body, the declared name(s) are in scope.

### 6.5 `expect` declarations

An `expect` declaration introduces a name and its type/signature, but does not provide a definition in the current file.

`expect` declarations are **top-level only**.

Forms:

```kappa
expect type Int
expect type String
expect type Float

expect trait Eq a
expect term toFloat : (this : Int) -> Float
expect term print   : (this : String) -> IO Unit
```

Rules:
* expect may prefix a type, trait, or a term signature `(term name : Type)`.
* An expect declaration contributes to name resolution as if it were an ordinary declaration.
* Each expected item must be satisfied exactly once by the compilation unit selected for the build, either by:
  * a matching ordinary definition in another source file fragment of the same module (implementation-defined), or
  * a backend intrinsic supplied by the selected backend profile (§17.6).

Errors:
* If an expect is not satisfied, it is a compile-time error.
* If multiple satisfying definitions exist for a single expect, it is a compile-time error.
* The satisfying definition must match the expected signature (up to definitional equality).

Implementations may support selecting module "fragments" by target (e.g. `main.kp` + `main.win32.kp`). In such systems,
`expect` declarations in common fragments are satisfied by definitions in the selected target fragments.

---

## 7. Expressions

### 7.1 Variables and application

* Variable usage: `x`, `foo`, `map`, etc.

* Function application is left-associative:

  ```kappa
  f x y      ==   (f x) y
  ```

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

### 7.1.1 Dotted forms (`.` / `?.`): qualification, type scope, projection, method sugar

The `.` token is used for:

* module qualification (`std.math.sin`)
* type scope selection (`Vec.Cons`)
* record field projection (`p.x`)
* explicit dictionary member projection (`d.name`, `d.(==)`, `d.Name` in type position for associated types)
* projection sections (`(.field)`, `(.field1.field2)`)
* method-call sugar (`x.show`)
* record update (`r.{ field = expr, ... }`)

The `?.` token is an additional dotted-form operator performing safe-navigation projection over `Option`:

* safe-navigation member access (`e?.member`)

The right-hand side of `?.` is restricted to member-access forms only. In v0.1 these are:

* record field projection,
* constructor-field projection,
* explicit dictionary member projection, and
* method-call sugar.

Forms such as module qualification, type scope selection, projection sections, record update, and row extension are
ordinary dotted forms but are not reachable through `?.`.

#### 7.1.1.1 Projection sections

```kappa
(.field)                 -- \__x -> __x.field
(.field1.field2)         -- \__x -> __x.field1.field2
```

Formation rule:

* A parenthesized expression whose sole content is a dot followed by one or more field names is a **projection
  section**.
* `(.field1.field2 ... fieldN)` elaborates to `\__x -> __x.field1.field2 ... fieldN`.
* The binder `__x` is fresh.
* The result type is inferred by checking the field chain against the elaborated argument type of the fresh binder.

Projection syntax also permits parenthesized operator members:

```kappa
d.(==)
d.(<=)
```

Such forms are permitted where dotted member projection is otherwise valid.

Resolution of dotted forms is defined in §13.1. Method-call sugar is defined in §13.1.1 and supplies `lhs` to the unique
receiver-marked explicit binder, which may appear in any argument position.

#### 7.1.1.2 Safe-navigation (`?.`)

Let a *chain* be a maximal sequence of dotted-form suffixes attached to an atomic expression. Schematically:

```text
chainExpr   ::= atom chainSuffix*
chainSuffix ::= '.'  member applicationArg*
              | '?.' safeMember applicationArg*
```

Here `member` stands for any right-hand side admitted by ordinary dotted forms, `safeMember` stands for a right-hand
side admitted by safe-navigation member access (record projection, constructor-field projection, explicit dictionary
member projection, or method-call sugar), and `applicationArg*` stands for any explicit application suffixes that follow
that member access.

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
  §13.1).
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

### 7.1.2 Elvis operator (`?:`)

For `e : Option T` and `d : T`, the expression `e ?: d` desugars to:

```kappa
match e
case Option.Some __x -> __x
case Option.None     -> d
```

This form is specified directly and does not depend on any library helper name.

`?:` is right-associative at low precedence (`2`, above `|>` at `1` and below comparison / arithmetic operators). It is
a reserved token and cannot be redefined by user fixity declarations.

### 7.1.3 Application-boundary subsumption

Kappa does not support deep subtyping. However, to maintain ecosystem coherence between linear, borrowed, and
unrestricted contexts, Kappa implements a localized subsumption rule at function application boundaries.

#### 7.1.3.1 Application elaboration pipeline

To resolve an application `f e`, let the next explicit parameter expected by `f` be `(q_dem x : T_dem)`, and let `e`
be the supplied argument expression. The compiler MUST apply the following steps in strict sequence:

1. **Outermost borrow introduction (auto-referencing).**

   If `q_dem` is exactly a borrowed quantity binder (`&` or an explicitly scoped borrowed binder such as `&[s]`), the
   compiler first checks whether `e` is a borrowable stable expression under §5.1.5.

   * If so, elaboration first considers the temporary-borrow form of `e` and typechecks that borrowed argument against
     `T_dem`.
   * If this borrowed check succeeds, the application is well-typed for that argument and no interval-quantity
     subsumption step is attempted.
   * If `e` is not a borrowable stable expression, or if the temporary-borrow form does not typecheck, elaboration
     continues to Step 2.

2. **Exact unification and ordinary quantity satisfaction.**

   Elaborate `e` against `T_dem`, obtaining an argument type `T_cap` and an available capability `q_cap` for the
   supplied expression.

   * If `T_cap` unifies exactly with `T_dem`, the compiler checks the satisfaction relation `q_cap ⊑ q_dem` as defined
     in §5.1.5.
   * If `q_cap ⊑ q_dem` holds, the application is well-typed.
   * If `q_cap ⊑ q_dem` remains undecidable because one or both quantities still contain unsolved quantity
     metavariables after processing surrounding constraints, compilation fails with an unsolved
     quantity-metavariable error rather than defaulting to any case.
   * If `T_cap` does not unify exactly with `T_dem`, continue to Step 3.

3. **Arrow subsumption (higher-order coercion).**

   If exact unification failed solely because `T_cap` and `T_dem` are both function types whose outermost binder
   quantities differ, the compiler MAY attempt outermost-arrow subsumption:

   * Let `T_dem = (q_inner_dem y : A) -> B`.
   * Let `T_cap = (q_inner_cap y : A) -> B`.
   * The parameter domain `A` and result type `B` must match up to definitional equality; only the outermost arrow
     quantity mismatch is tolerated by this step.
   * The compiler then checks the contravariant satisfaction condition `q_inner_dem ⊑ q_inner_cap`.
   * If that condition holds, the application is well-typed and the compiler MUST elaborate `e` by inserting an
     internal coercion or eta-expansion whose exposed binder quantity matches the demanded type `T_dem`.
   * If the condition is undecidable after surrounding constraints are processed, compilation fails with an unsolved
     quantity-metavariable error.
   * If the condition does not hold, the application is a compile-time error.

If none of the three steps succeeds, the application is ill-typed.

This subsumption applies only to the outermost arrow at the point of application. It does not apply recursively inside
type constructors. For example, `List ((ω x : A) -> B)` is never implicitly coercible to `List ((1 x : A) -> B)`.

Borrow introduction for arguments demanded at quantity `&` is not an interval-quantity case of `⊑`. It is handled by
Step 1 above, via the borrow-introduction rule of §5.1.5, which may insert a temporary borrow of a borrowable stable
expression even though no interval capability satisfies `&` in the `⊑` table.


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
exit@\ x -> x
```

Grammar:

```text
lambda  ::= [label '@'] '\' binders '->' expr
binders ::= binder+
binder  ::= ident                            -- inferred type, quantity ω
          | '(' ident ':' type ')'          -- explicit type, quantity ω
          | '(' quantity ident ':' type ')' -- explicit type and quantity
          | '(' 'this' ':' type ')'         -- receiver binder, local name `this`
          | '(' quantity 'this' ':' type ')' -- receiver binder, explicit quantity
          | '(' 'this' ident ':' type ')'   -- receiver binder, local name `ident`
          | '(' quantity 'this' ident ':' type ')' -- receiver binder, explicit quantity and local name
          | '(' '@' binder_body ')'         -- implicit binder (as before)
```

Rules:

* A bare-identifier binder `x` is equivalent to `(x : _)` with inferred type and default quantity `ω`.
* Multiple bare binders may be juxtaposed: `\x y z -> e` ≡ `\(x : _) (y : _) (z : _) -> e`.
* Mixing styles is permitted: `\x (y : Int) z -> e`.
* Receiver-marked binders are explicit binders. `(this : T)` binds the receiver locally as `this`, while `(this x : T)`
  marks the binder as the receiver and binds it locally as `x`. The quantity-annotated forms `(q this : T)` and `(q this
  x : T)` are the corresponding receiver-marked variants with explicit quantity `q`.
* Method-call sugar (§13.1.1) consults the receiver marker, not the local binder name chosen inside the function body.
* A leading label `L@` labels the lambda body for `return@L` (§8.4.1).
* Parentheses are required for typed, quantity-annotated, and implicit binders; they are not required for bare
  identifier binders.
* A lambda must contain at least one binder; there is no point-free "degenerate" lambda.
* The body after `->` may be either a single expression or an indented pure block suite, which elaborates to `block ...`
  under §6.3.1.
* Multiple parameters are curried:

  ```kappa
  \ x (y : B) -> e    :   A -> B -> T
  ```

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
* Borrowed capture: if the lambda captures one or more variables bound with quantity `&`, the resulting closure value is
  tied to the same borrow lifetime. A closure whose hidden region environment mentions only explicit region variables
  already in scope may be returned or exported under those same explicit binders; a closure mentioning any anonymous
  local borrow region must not escape the scope that introduced it (§5.1.6). If a lambda captures an `&` variable and
  its inferred type would allow an anonymous borrow region to escape, elaboration fails with "borrowed capture escapes".
* Erased safety: if the lambda captures variables bound with quantity `0`, those variables MUST NOT appear in
  computationally relevant (`1`, `&`, `<=1`, `>=1`, or `ω`) positions inside `body`. Capturing quantity-`0` variables
  imposes no restriction on the multiplicity of the closure itself.
* Unrestricted closures: a closure may be used unrestrictedly (`ω`) only if every computationally relevant captured
  variable is itself unrestricted. Capturing only unrestricted (`ω`) and/or erased (`0`) variables is sufficient for
  this case.

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

If `T` has sort `Constraint`, the default is `0`. If `T` elaborates to `Type u` for some universe term `u`, the default
is `0`. Otherwise, the default is the unrestricted quantity `ω`.

Users may override the default by writing the quantity explicitly, e.g. `(@0 x : T)`, `(@& x : T)`, `(@&[s] x : T)`, or
`(@ω x : T)`.

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

### 7.3.2 Expression holes (`_`)

In expression position, `_` denotes a **typed hole**.

* The compiler attempts to infer a term that fills the hole.
* If the hole cannot be inferred, compilation fails with an error that reports the expected type and available context.

Kappa does not provide placeholder-abstraction sugar based on `_` in v0.1. Users should write an ordinary lambda
explicitly when abstraction is intended, or use existing section forms such as operator sections (§3.5.1.1) and
projection sections (§7.1.1).

### 7.3.3 Implicit resolution (instance/proof search)

When elaborating an application, if the callee expects an implicit argument `(@x : G)` and the call site does not supply
one, the compiler attempts to synthesize a term of type `G`.

Resolution proceeds in this order:

1. **Local implicit context**: if there is an in-scope implicit value whose type is definitionally equal to `G`, use it.
   The local implicit context includes:
   * implicit binders introduced by `(@x : T)` parameters, and
   * implicit record-field projections unpacked from bound records with implicit fields (§5.5.9),
   * implicit assumptions introduced by control flow (see §7.4.1, §7.5.3, §10.4.1).
   * For an in-scope implicit value or dictionary `d : Tr args`, coherent evidence obtained by projecting any declared
     supertraits of `Tr args` (§12.1.1).

2. **Trait instance resolution**: if `G` is a trait constraint (e.g. `Eq Int`), attempt to resolve an instance from the
   instance environment using the algorithm of §12.3.1.

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

#### 7.3.3.1 `witness` and `summon` as library helpers (not syntax)

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

### 7.4 Conditionals

`if` is an **expression**:

```text
ifExpr ::= 'if' expr 'then' expr ('elif' ... | 'else' expr)
         | 'if' expr 'is' ctor 'then' expr ('elif' ... | 'else' expr)

ctor   ::= ctorName
         | typeName '.' ctorName
```

In an `if e is C` test, the right-hand side of `is` is a single constructor name only. No sub-patterns, no arguments, no
binders, no or-alternatives, no view patterns, and no literals other than constructor-like built-ins such as `True`,
`False`, and `()`. For richer matching, or for access to unnamed constructor payloads, use `match`.

```kappa
let sign : String =
    if x > 0 then
        "positive"
    elif x < 0 then
        "negative"
    else
        "zero"
```

Rules:

* Outside `do`, `if` must have a final `else`.
* `elif` is sugar for `else if`.
* All branches must have the same type.
* In the boolean form `if cond then t else f`, `cond` must have type `Bool`.
* In the tag-test form `if e is C then t else f`:
  * `e` must have a type whose head is a `data` type, or a built-in type whose elimination is constructor-based (such as
    `Bool`).
  * `C` must be a constructor of the scrutinee type. If it is not, the program is ill-formed with the same kind of error
    as an equivalent `match`.
  * The test introduces no fresh user-visible term bindings.
  * The success branch flow-refines the scrutinee expression `e` to the constructor-refined view determined by `C`; the
    failure branch refines `e` negatively to exclude `C` (formalized in §7.4.1).

Semantics:

* `if e is C then t else f` evaluates `e` to a value `v`. If the top-level constructor of `v` is `C`, the expression
  yields `t`; otherwise it yields `f`.
* `elif e' is C' then ...` chains in the obvious way.

Inside a `do` block, an `if` without `else` is allowed as sugar (see §8).

### 7.4.1 Branch assumptions

Boolean conditionals introduce boolean assumptions into the implicit context:

```kappa
if cond then e1 else e2
```

Typechecking rules:

* `cond` must have type `Bool`.
* `e1` is typechecked with an additional implicit assumption:
  * `@p : cond = True`
* `e2` is typechecked with an additional implicit assumption:
  * `@p : cond = False`

For chained conditionals:

```kappa
if c1 then e1
elif c2 then e2
elif c3 then e3
else e4
```

* `e1` is checked under `@p : c1 = True`.
* `e2` is checked under `@p : c1 = False` and `@p : c2 = True`.
* `e3` is checked under `@p : c1 = False`, `@p : c2 = False`, and `@p : c3 = True`.
* `e4` is checked under `@p : c1 = False`, `@p : c2 = False`, and `@p : c3 = False`.

These assumptions participate in implicit resolution (§7.3.3). In particular, `witness (c2)` within the `elif c2` branch
can retrieve a proof of `c2 = True` (via §5.6 coercion).

Tag-test conditionals introduce constructor-tag assumptions and branch-local flow refinement:

```kappa
if e is C then t else f
```

Typechecking rules:

* `t` is checked under an implementation-defined implicit assumption that the top-level constructor of `e` is `C`.
* `f` is checked under the corresponding negative assumption that the top-level constructor of `e` is not `C`.
* These assumptions introduce no new user-visible term bindings, but they do refine the original scrutinee expression
  `e` for branch-local typechecking.
* In the success branch, the refinement MUST be strong enough that:
  * a subsequent `match e` may treat non-`C` cases as unreachable, and
  * if `C` declares named explicit parameters, dotted projection `e.field` may refer to those named constructor
    parameters via constructor-field projection (§13.1).
* If `C` has only positional / unnamed arguments, `if e is C` still does not expose them directly; use `match` to bind
  them.
* In the failure branch, the refinement MUST be strong enough that a subsequent `match e` may treat the `C` case as
  unreachable.
* In mixed `elif` chains, branch assumptions accumulate in the obvious way: earlier failed tests contribute negative
  assumptions, and the selected `elif ... is C` branch additionally contributes its positive assumption.


### 7.5 `match` expressions

Pattern matching expression:

```kappa
match expr
case pattern1 if guard1 -> expr1
case pattern2           -> expr2
...
```

* `match` is an expression.
* Each `case` has:

    * a **pattern**,
    * an optional `if guard` (a boolean expression),
    * a result expression after `->`.
* All branches must have the same type.

**Exhaustiveness:**

* For **closed** types (e.g. ADTs, `Bool`), `match` must be exhaustive; missing cases are a compile-time error.
* For open/infinite domains (e.g. `Int`), you must include a catch-all (`_`) or similar; otherwise it's an error.

#### 7.5.1 Exhaustiveness with indexed types (GADTs)

For indexed/"GADT-style" types, pattern matching refines the scrutinee's indices and may render some constructor cases
impossible.

Implementations should:

* attempt to use definitional equality / unification of indices to detect unreachable cases,
* treat unreachable cases as not required for exhaustiveness,
* when matching on an equality proof and encountering a branch `case refl -> ...`, perform index unification for that
  branch so the two sides of the equality are treated as definitionally equal within the branch body,
* otherwise (if coverage cannot be established) require an explicit catch-all (`_`) or an explicit user-written case
  structure that proves impossibility.

As a user-facing way to state and check unreachability, a branch may use `-> impossible` (§7.5.2). Such a branch is
valid only when the compiler can verify the case is unreachable.

#### 7.5.2 `impossible` (unreachable branch bodies)

`impossible` is a special expression used to mark an unreachable branch.

Form:

```kappa
match e
case pat -> impossible
```

Typing rule:
* A branch body `impossible` is accepted only if the compiler can prove that the corresponding case is unreachable,
using definitional equality and index unification (the same machinery used for detecting unreachable constructor cases
in Section 7.5.1).
* If the compiler cannot prove the case unreachable, it is a compile-time error.

Meaning:
* If accepted, `impossible` may be given any result type required by the surrounding match.
* At runtime, an `impossible` branch must never be executed; implementations may compile it to a trap.

### 7.5.3 Boolean matches introduce assumptions

When the scrutinee has type `Bool` and a case pattern is the boolean literal `True` or `False`, the corresponding branch
is typechecked with an implicit assumption:

```kappa
match b
case True  -> eT   -- eT checked under  @p : b = True
case False -> eF   -- eF checked under  @p : b = False
```

This applies equally to `try match` success/error branches when the matched value is a `Bool`.

### 7.6 Patterns (overview)

Patterns are used in `match`, `try match`, `for` generators, and refutable forms (§10.4.1).

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

#### 7.6.2 Scoping and validity

* Names bound by a pattern are in scope in:
  * the pattern guard (if present),
  * the branch/body expression,
  * subsequent clauses (for comprehensions).
* Duplicate binders within the same pattern are an error (e.g. `(x, x)`).
* A pattern match proceeds left-to-right; nested patterns are permitted.
* In a prefix pattern application, the head must resolve either to a data constructor or to a function explicitly
  declared with the `pattern` keyword. An ordinary function is not permitted in pattern-head position.

#### 7.6.3 Or-pattern validity (`|`)

In `p1 | p2 | ... | pn`:

* Each alternative must bind the **same set of names**.
* Each bound name must have definitionally equal types across alternatives (under the scrutinee type and any index
  refinements), and corresponding binders must carry identical quantities across alternatives.
* If these conditions do not hold, the pattern is ill-formed.

Or-patterns are tried left-to-right at runtime.

### 7.7 Active patterns (view patterns)

Active patterns allow a module to encapsulate the internal representation of an `opaque data` type while still exposing
an ergonomic pattern-matching interface. An active pattern is a pure function that acts as a custom pattern constructor
during `match`, `try match`, or `let?` destructuring.

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

## 8. Effects, `do` Blocks, and Control Flow

### 8.1 Monadic core

Kappa uses a minimal monadic core at the semantic level. For a monad `m`, we assume:

```kappa
pure  : forall a. a -> m a
(>>=) : forall a b. m a -> (a -> m b) -> m b
(>>)  : forall a b. m a -> m b -> m b
```

(Names may live in traits; here it's a conceptual interface.)

#### 8.1.1 `Eff` and effect rows

Kappa provides a standard effect-row-indexed computation type:

```kappa
Eff : EffRow -> Type -> Type
```

For every effect row `r : EffRow`, the type constructor `Eff r` participates in the standard prelude interfaces:

```kappa
Functor (Eff r)
Applicative (Eff r)
Monad (Eff r)
```

An `Eff r a` computation may perform only the effects described by `r` and, when it terminates normally, produces a
value of type `a`.

Row weakening:

* If `r1 ⊆ r2`, then `Eff r1 a` is implicitly coercible to `Eff r2 a`.
* This coercion is effect-row weakening. It is zero-cost at runtime and preserves the computation's behavior.

Fully handled computations:

```kappa
runPure : Eff <[ ]> a -> a
```

`runPure` eliminates an `Eff` computation only when its effect row is empty. A computation may be passed to `runPure`
only after all effects have been handled or otherwise eliminated.

#### 8.1.2 `effect` declarations

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

* `effect` declarations are top-level only.
* `public` and `private` apply in the same way as for other ordinary top-level declarations.
* `opaque` does not apply to `effect` declarations.
* An `effect` declaration introduces an effect-interface constructor in the type namespace.
* Effect labels are separate identifiers in the effect-label namespace and are introduced by effect-row syntax.
* A row entry `<[l : E | r]>` associates the effect label `l` with the effect interface `E`.
* Each operation signature in the body is an operation declaration of the effect interface.
* An operation declaration may be prefixed with a quantity `q`. If omitted, the resumption quantity defaults to `1`.
  This quantity governs the permitted usage of the resumption continuation value supplied to handlers of that operation.
  For interval quantities, it controls how many times the handler may use the continuation; for `&`, it grants borrowed
  non-consuming use subject to the ordinary borrow rules.
* Operation names declared inside an `effect` declaration are not brought into the global term namespace. They are
  selected via `label.op` (§13.1), and are additionally available within a handler for that label when handlers are
  specified.
* Operation signatures may be arbitrary dependently typed function types after elaborating any outer `forall`s. This
  includes multiple explicit or implicit parameters, quantity annotations on binders (defaulting to `ω`), and dependent
  result types.
* There is no requirement that an operation have the simple form `A -> B`. The only restrictions are:
  * the signature must elaborate to a Pi-type (function type),
  * an operation may not perform the effect it declares, meaning its declared result type must not itself mention the
    handled effect label.

#### 8.1.3 Effect application and linear soundness

Because Kappa enforces quantitative resource tracking, the capability of an effect handler to duplicate execution via a
multi-shot continuation is restricted by the linear environment at the operation site.

When a computation invokes an effect operation `label.op args`, and the declared resumption quantity of `op` is `q`, the
evaluation context from that operation site to the end of the nearest enclosing handler for `label` is reified by the
compiler as a continuation value whose usage is checked against `q`.

Call-site capture rule:

* The computation from the `op` call site to the end of the handled block MUST be valid when the reified continuation is
  treated as a closure usable at quantity `q`.
* This check uses the ordinary closure-capture and borrow rules of §§5.1.5, 5.1.6, and 7.2.1.
* In particular, if `q` permits more than one use of the continuation, such as `ω` or `>=1`, then the continuation MUST
  NOT capture any live variables with quantity `1` or `&` from the surrounding lexical environment.
* If the compiler detects that an operation with a multi-shot resumption quantity is invoked in a scope where live
  linear or borrowed resources would be captured by the continuation, compilation fails with a quantity error at the
  operation site.

This rule guarantees that multi-shot continuations cannot clone linear resources or extend borrow lifetimes unsoundly.

#### 8.1.3.1 Repeated resumption is fresh control re-entry

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
such duplication unsound, the program is already rejected by the call-site capture rule of Section 8.1.3.

#### 8.1.4 Shallow handlers

Kappa provides shallow handlers. A shallow handler intercepts operations for one effect label at a time and yields
resumptions that continue in the original unhandled computation type.

Syntax:

```kappa
handle label in expr
case return x -> e_ret
case op1 x1 ... xn k -> e1
case op2 y1 ... ym k -> e2
...
```

Here `label` is an effect label. Inside the handler clauses, operation names of the handled effect are available
unqualified.

Typing:

Suppose `expr : Eff r_all a` and `SplitEff r_all label E r` is solvable. Equivalently, up to row normalization, `expr`
has type `Eff <[label : E | r]> a`.

Suppose the effect interface `E` declares an operation `q op` whose signature (after elaborating any outer `forall`s)
is:

```kappa
op : Π (x₁ : A₁) ... (xₙ : Aₙ). B
```

(where the Π-telescope may contain implicit binders, quantity annotations, and dependencies between the parameters and
the result `B`).

Then the handler is well-typed iff:

* there is exactly one return clause of the form `case return x -> e_ret`,
* there is exactly one operation clause for each operation declared by `E`,
* there are no extra operation clauses,
* in the return clause, `x : a` and `e_ret : Eff r b`,
* in an operation clause `case op x₁ ... xₙ k -> e`, each `xᵢ` is bound at type `Aᵢ` with previous parameters
  substituted into later parameter types and into `B`,
* in that clause, the resumption `k` is itself bound at quantity `q`,
* the resumption `k` has type `(1 _ : B) -> Eff <[label : E | r]> a`, where `B` is instantiated by the clause binders,
* and the clause body `e` has type `Eff r b`.

The "exactly one operation clause per declared operation" and "no extra operation clauses" requirements remain
unchanged.

The whole handler has type:

```kappa
Eff r b
```

Semantics:

* On pure completion of `expr`, the `return` clause is executed.
* On an operation at the handled label, the matching operation clause is executed.
* Applying `k` resumes the suspended computation from the operation site.
* `k` must be used according to its declared resumption quantity `q`. If `q = 1`, invoking it more than once is a
  compile-time quantity error inside the handler. If `q` permits more than one use, such as `ω` or `>=1`, multiple
  invocations are permitted, subject to the call-site capture rule of §8.1.3.
* The current handler is not automatically reinstalled around `k`. To continue handling the same label after resumption,
  the handler body must explicitly handle the resumed computation again.
* Operations at labels other than `label` propagate outward unchanged.

Example:

```kappa
effect State (s : Type) =
    1 get : Unit -> s
    1 put : s -> Unit

runState :
    forall (r : EffRow) (s : Type) (a : Type).
    (init : s) -> Eff <[state : State s | r]> a -> Eff r (a, s)

let runState init comp =
    let go (st : s) (m : Eff <[state : State s | r]> a) : Eff r (a, s) =
        handle state in m
        case return x -> pure (x, st)
        case get () k -> go st  (k st)
        case put st' k -> go st' (k ())
    in
        go init comp

effect ResourceManager =
    1 acquire : (name : String) -> (1 size : Nat) -> Resource name
    1 release : (name : String) -> (1 r : Resource name) -> Unit

handle rm in comp
case return x -> ...
case acquire name size k ->
    let r = ... in
    k r   -- k : (1 _ : Resource name) -> Eff <[rm : ResourceManager | rest]> a
case release name r k ->
    ...; k ()
```

#### 8.1.5 Deep handlers (`deep handle`)

Deep handlers automatically reinstall themselves around their resumptions. They are surface sugar over shallow handlers
plus explicit recursion.

Syntax:

```kappa
deep handle label in expr
case return x -> e_ret
case op1 x1 ... xn k -> e1
case op2 y1 ... ym k -> e2
...
```

Typing:

Suppose `expr : Eff r_all a` and `SplitEff r_all label E r` is solvable.

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
* `k` has type `(1 _ : B) -> Eff r b`,
* and the clause body `e` has type `Eff r b`.

Thus a deep continuation already returns to the target monad of the whole handler rather than to the original unhandled
computation type.

Normative desugaring:

```kappa
let __go __comp =
    handle label in __comp
    case return x -> e_ret
    case op1 x1 ... xn __k_shallow ->
        let q k = \(1 res : B) -> __go (__k_shallow res)
        e1
in
    __go expr
```

where `q` and `B` are the declared resumption quantity and instantiated result type of `op1`, and where `__go` and
`__k_shallow` are fresh identifiers inaccessible to user code.

Totality of the generated recursion:

* The helper `__go` above is an elaborator-internal recursive function, not a user definition.
* It MUST NOT be rejected merely because a surface-level termination checker cannot see an ordinary structural argument
  over source syntax.
* A conforming implementation must justify this recursion by one of the following equivalent strategies:
  * prove internally that each recursive call processes a proper resumed subtree of the handled computation, or
  * lower `deep handle` directly to an internal primitive or handler form with the same observable behavior as the
    schematic recursion above.
* This mechanism is not a hidden use of user-facing `assertTotal`. It is trusted elaborator infrastructure of the same
  kind as the internal recursive loop used for `while` (§8.5).

Semantics:

* On normal completion of a handled branch, the `return` clause `e_ret` is evaluated in the lexical context of the
  handler itself, not in the original call-site context of the computation being handled.
* Consequently, any linear or borrowed resources captured by the handler and used by `e_ret` are checked and consumed
  according to the ordinary closure-capture rules. This is sound because each completed branch reaches the `return`
  clause at most once.
* Applying `k` resumes the suspended computation from the operation site and immediately reinstalls the same deep
  handler around that resumed computation.
* `k` must still be used according to its declared quantity `q`.
* Operations at labels other than `label` propagate outward unchanged.
* If the deep resumption `k` is used more than once under its declared resumption quantity, each use behaves as a fresh
  application of the corresponding shallow resumption followed by fresh reinstallation of the same deep handler. The
  independence and shared-store rules are exactly those of Section 8.1.3.1.

Stateful deep-handler example:

```kappa
let runState (init : s) comp =
    let handlerObj =
        deep handle state in comp
        case return x -> \st -> pure (x, st)
        case get () k -> \st -> k st st
        case put st' k -> \st -> k () st'
    in
        handlerObj init
```

Because `inout` is purely linear record-threading sugar (§8.8), the same state-runner pattern can also be exposed
through an `inout` parameter on a named function, with no change to the underlying handler semantics.

#### 8.1.6 `MonadFinally`

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

##### 8.1.6.1 Laws

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
  * The underlying pattern of `bindPat` MUST be irrefutable for type `A` (§6.1.1). If it is refutable, it is a
    compile-time error.
  * Any quantity annotation on `bindPat` applies uniformly to every variable introduced by that pattern (§6.3).

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
    The prelude minimum requires a trait providing at least this operation; this section uses the name `Releasable`.

    Normative elaboration is given in §8.7.4.

* **Local definition**:

  ```kappa
  let bindPat = expr   -- pure local binding inside the `do` body
  ```

  Rules:
    * The underlying pattern of `bindPat` must be irrefutable (§6.1.1). Refutable patterns are not permitted in do-local
      bindings.
    * Any quantity annotation on `bindPat` applies uniformly to every variable introduced by that pattern (§6.3).
    * If `bindPat` carries quantity `&` and `expr` is not already a borrowable stable expression, elaboration introduces
      a fresh hidden temporary root scoped to the remaining do-items, exactly as specified in §6.3 and §5.1.6.
    * Names bound by `bindPat` are in scope in subsequent do-items in the `do` block.

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
  `data` / `type` / `trait` / `instance` / `derive` declarations, and local `import` / fixity declarations. Their
  sequential scoping and elaboration rules are specified in §6.3.1 and §14.1.1.

Control-flow statements:
  * `break` and `continue` are statements, not expressions. They are valid only inside loop bodies (§8.5).
  * `return e` and `return@L e` are statements, not expressions. They are valid only inside the body of a named
    function, method, or lambda (§8.4).
  * `break` and `continue` may not cross user-written lambda or local-function boundaries. They target only loops within
    the same user-written function or lambda body in which they syntactically occur. Compiler-generated closures
    introduced solely by desugaring of loops, comprehensions, or protected-resource elaboration of `using` are
    transparent to this restriction.
  * `return` target resolution is defined by §8.4. Bare `return` targets the nearest enclosing named function or method;
    `return@L` may target that named function or method by name, or a lambda labeled `L`.

### 8.2.1 Refutable local binding (`let?`)

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
* This check is performed on the unmatched residue of `pat`, not merely on whether the matched branch contains linear
  data. For example, matching `Option.Some` against `Option (1 File)` is permitted because the failure case `None`
  carries no owned linear obligation, while matching `Ok` against `Result (1 File) (1 ErrorToken)` is rejected because
  the failure case would discard `Err` carrying `ErrorToken`.
* In the `else` form, the pair of patterns `pat` and `residuePat` is checked as a disjoint, jointly exhaustive split of
  the scrutinee type, using the ordinary overlap and coverage rules of `match`.
* The `else` form does not require `Alternative m` merely for refutation, because failure does not go through `empty`.
  Instead, `failExpr` is typechecked as the terminal failure branch of the surrounding `do` block, against the
  surrounding `do` result type.
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

### 8.2.2 Do-item sequences and block result

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
  `instance`, `derive`, and scoped fixity/import items)

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

### 8.2.3 Labeled `do` blocks and labeled control flow

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
* If no suitable labeled construct is found, it is a compile-time error.
* If multiple labeled constructs with the same label occur at the same lexical nesting level, it is a compile-time
  error. (Shadowing by nested labels is permitted.)

Semantics follow Kotlin-style labeled control flow:

* `break@label` exits the labeled loop.
* `continue@label` targets the labeled loop.
* `defer@label e` schedules `e` to run when the labeled `do`-scope exits. If several inner `do`-scopes exit first, `e`
  remains pending and runs only when the targeted labeled scope itself is unwound (§8.7.2.1).

### 8.2.4 Simple desugaring applicability

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

### 8.2.5 Do-local declarations

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

### 8.4 `return`

`return e` is a control-flow statement that exits a target function, method, or labeled lambda, producing `e` as its
result.

Validity:
* `return` is a statement, not an expression.
* `return` is valid only inside the body of a named function, method, or lambda.

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

* **Labeled `return@L e`** targets the construct labeled `L`:
  * a named function or method whose name is `L`, or
  * a direct lambda definition whose inherited name is `L`, or
  * a lambda carrying the label `L` (§8.4.1).
  * Resolution searches outward lexically from the use site. Lambda boundaries do not block labeled `return` to a lambda
    labeled `L` or to a named function or method named `L`, provided `L` is in scope at the use site.
  * If no construct with label `L` is found, it is a compile-time error.

* `return` does not target `do` blocks, `if` / `match` branches, loops, or `try` expressions. Those constructs are
  transparent to `return` and propagate the `Return` completion outward (§8.7.4).

Typing:
* Let the target have result type `m R` (or `R` for a pure, non-monadic target). Then `e` must have type `R`. `return e`
  produces `pure e` in the target's monad and exits that target.
* A labeled `return@L e` is typed against the type of the construct labeled `L`, not of any surrounding construct.

Restrictions (carried over from prior §8.4):
* `return` MUST NOT appear within a `defer` action (§8.6) or `finally` block (§9.2, §9.3) when it would target the
  enclosing `do`-scope, `try`, or surrounding control-flow context. Returns inside nested lambdas or local functions are
  permitted when they target those nested constructs. Any disallowed case is a compile-time error.

Interaction with `defer` / `finally`:
* Executing `return e` exits the target. All enclosing dynamic `do`-scopes within the target are exited first, and their
  deferred actions run per §8.6 and §8.7. Scopes outside the target are not unwound.

### 8.4.1 Lambda labels

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

### 8.5.1 Mutable variables (`var`) - normative semantics

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

### 8.5.2 Loop `else`

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

### 8.5.3 Scoped Regions and Escape Prevention

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

## 8.7 Normative elaboration model for `do` blocks, loops, and abrupt control

This section defines the normative meaning of `do` blocks with loops, `break` / `continue`, `return`, and `defer` /
`using`. Implementations may compile using jumps, CPS, internal exceptions, or other techniques, but MUST be
observationally equivalent to the model below.

### 8.7.1 Abrupt completion records (meta-level)

For specification purposes, we model execution of a `do`-scope using a family of *completion records* parameterized by
the in-scope return-target context.

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

This `Completion(RetCtx, A)` datatype is a normative meta-level model of the observable control-flow behavior only.
Implementations MAY realize it via exceptions, CPS, explicit stacks, or other equivalent machinery, provided the
resulting behavior is observationally equivalent to the unwinding and target-resolution rules specified in this section.

Where:

* `A` is the "normal" result type of the construct (often `Unit` for statements).
* `RetCtx` is the target-indexed return context for the current elaboration point.
* Each `Return[Lk]` is a distinct compiler-generated control constructor for the return target labeled `Lk`.

This is equivalent to generating one internal control constructor per in-scope return target, rather than globally
parameterizing the whole completion model by a single return payload type.

In addition to the completion record, execution carries a dynamic stack of active `do`-scopes:

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

### 8.7.2 Dynamic `do`-scope exit and exit actions

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

#### 8.7.2.1 Cross-scope unwinding and targeted `defer`

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

### 8.7.3 Completion-aware sequencing (meta-level)

For an enclosing monad `m`, we write computations of the form `m (Completion(RetCtx, A))`.

Define the following meta-level operators (expressible in core via `>>=` and `match` in the compiler IR):

* Lifting a normal monadic computation:

  `lift : m A -> m (Completion(RetCtx, A))`

  `lift act` runs `act` and returns `Normal` of its result.

* Completion bind:

  `bindC : m (Completion(RetCtx, A)) -> (A -> m (Completion(RetCtx, B))) -> m (Completion(RetCtx, B))`

  `bindC ma k` runs `ma` and:
    * if it returns `Normal a`, runs `k a`,
    * otherwise propagates `Break` / `Continue` / any `Return[...]` unchanged.

Sequencing is `thenC x y = bindC x (\_ -> y)`.

### 8.7.4 Elaboration of `do`-item sequences

A `do` block is elaborated in a context that records the resolved labels and result types of the enclosing named
function or method, inherited-name lambdas (§6.1), and any enclosing labeled lambdas that may be targeted by `return`.
This finite mapping is the current `RetCtx`.

For a do-scope body consisting of a non-empty sequence of do-items, elaboration yields:

* For a final do-item `item` that may complete normally with `A`: `⟦item⟧ : m (Completion(RetCtx, A))`

* For a sequence `item; rest` where `rest` ultimately produces `A`: `⟦item; rest⟧ : m (Completion(RetCtx, A))`

The cases below define `⟦...⟧` for arbitrary `A`.

#### Result-producing item

* Expression item `final` where `final : m A`, and where normal execution reaching `final` means no later do-item is
  reached:

  `⟦final⟧ = lift final`

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
      case residuePat -> ⟦failExpr⟧
  ```

  where `pat` and `residuePat` are checked as a disjoint, jointly exhaustive split of the scrutinee type, and
  `⟦failExpr⟧` denotes elaboration of the failure arm as a terminal branch at the surrounding `do` result type.

* Pure local binding `let bindPat = expr`:

  If `bindPat` carries quantity `&`, elaboration performs a borrowed binding of the underlying pattern `pat0` against a
  borrowable stable root and continues with `rest`. If `expr` is not already a borrowable stable expression, elaboration
  first introduces a fresh hidden temporary root:

  ```kappa
  let 1 __tmp = expr
  let & pat0 = __tmp
  rest
  ```

  where `pat0` is the underlying pattern of `bindPat` and `__tmp` is fresh and inaccessible to user code.

  If `bindPat` does not carry quantity `&`, it elaborates as a pure binding of the underlying pattern of `bindPat`,
  introducing each bound variable at the quantity carried by `bindPat`, and continues with `rest`.

#### `defer` and `using`

* `defer d; rest`:

  `⟦defer d; rest⟧ = finally ⟦rest⟧ d`

  where `finally` is the `MonadFinally` method for the enclosing monad `m`, instantiated at the completion result type
  of `⟦rest⟧`. Nested `defer`s therefore unwind in LIFO order. When an implicit `MonadError m` is also available, nested
  finalizers select the propagated error according to §8.7.2: the first error wins, and later finalizer errors may be
  suppressed or discarded.

* `defer@L d; rest`:

  `⟦defer@L d; rest⟧ = scheduleDeferred(L, d, ⟦rest⟧)`

  where `scheduleDeferred` is the meta-level operation that pushes `d` onto the exit-action stack of the resolved
  `do`-scope frame labeled `L` in the dynamic `ScopeStack(m)` of §8.7.2.1, without otherwise changing the immediate
  completion behavior of `⟦rest⟧`. If `L` is the current `do`-scope's label, this is observationally equivalent to
  `finally ⟦rest⟧ d`.

* `using pat0 <- acquire; rest`:

  elaborates by protected-scope splitting, not by ordinary `defer` closure capture:

  1. Run `acquire : m A` to obtain an owned resource `__res : A` at quantity `@1`.
  2. Select `rel = Releasable.release` from the implicit `Releasable m A` evidence.
  3. Push the exit action `Release[A] rel __res` onto the current `do`-scope frame.
  4. Elaborate `rest` under borrowed bindings `let & pat0 = __res`.

  After step 3, `__res` is no longer available as an ordinary user-visible term. Its owned `@1` obligation lives only in
  the scheduled release action attached to the current scope frame. The borrowed bindings introduced by `pat0` remain
  valid only within `rest` and are subject to the borrow-escape rules of §5.1.6.

  If `pat0` is a destructuring pattern, each name bound by `pat0` is a borrowed projection of the same hidden owned
  resource represented by the scheduled `Release[A] rel __res` action.

  This rule is primitive to `using`. It is NOT elaborated through `defer (rel __res)` or through a user-visible lambda,
  because ordinary closure capture would move `__res` before the borrowed view is introduced and would therefore violate
  the linearity rules of §§5.1.5 and 7.2.1.

  Implementations MAY lower this primitive to backend-supported `try`/`finally`, state-machine code, RAII-style cleanup,
  or other equivalent machinery, but MUST preserve the ownership split and unwinding behavior specified here.

  Equivalent `bracket` view:

  If a correct `MonadResource` implementation is available, the protected resource-scope rule above is observationally
  equivalent to:

  ```kappa
  bracket acquire Releasable.release (\(& pat0) ->
      ⟦rest⟧
  )
  ```

  where the generated closure returns `m (Completion(RetCtx, A))` for the surrounding `do`-scope result type `A`. Any
  `return`, `break`, or `continue` inside `rest` therefore becomes an ordinary `Completion(...)` payload that `bracket`
  ferries outward after executing `release` exactly once. This equivalence relies on the compiler-generated closure
  being transparent to abrupt-control target resolution, as specified in §§8.2.3, 8.4, and 8.5.

#### `return`

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

### 8.7.5 Normative elaboration of loops

Loops are statements within `do` and elaborate to `m (Completion(RetCtx, Unit))`.

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

### 8.7.6 Minimal desugaring requirements for `for` loops

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

### 8.8 `inout` parameters

`inout` is purely surface-level linear-state-threading sugar. It does not extend Kappa's core type theory.

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

#### 8.8.2 Syntax

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

#### 8.8.3 Call-site marker `~`

`~` marks an argument supplied to an `inout` parameter.

Rules:

* `~` is valid only inside a `do` block, and only on arguments of the top-level application that forms a `do`-item
  rewrite site.
* `~` may be applied only to a stable place:
  * a variable name, or
  * a record projection path rooted at a variable or `var`-bound place, such as `rec.field` or `rec.inner.field`.
* A given stable place may appear in at most one `~` argument within a single application.
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

#### 8.8.4 Normative desugaring (type-directed)

Applications containing one or more `~` arguments are rewritten before the ordinary `do`-item elaboration of §8.7.4.

For a surface form:

```kappa
let pat <- func ~x1 ... ~xk args
```

or the pure analogue `let pat = func ~x1 ... ~xk args`, elaboration proceeds as follows:

1. Resolve the callee's signature and determine the ordered list of `inout`-compatible formal parameter names `p1 ...
   pk` corresponding to the `~` arguments. This step succeeds only when each marked argument position satisfies the
   admissibility rule of §8.8.3: quantity `@1`, stable formal name, stable place, and matching quantity-`1`
   return-record field.
2. Remove the `~` markers and pass the underlying place expressions normally. If a marked argument is `~root.path`, the
   argument passed to the callee is the projection `root.path`, whose quantity-`1` use consumes exactly that path under
   §5.5.4.
3. Determine the returned record type `R`:
   * for `let pat <- ...`, the call must have type `m R`;
   * for `let pat = ...`, the call must have type `R`.
4. Check that `R` is a record type containing fields `p1 ... pk`. Those fields are the threaded successors of the
   `inout` parameters and must be declared at quantity `1`. Let the residual record be the record obtained from `R` by
   removing those `inout` fields.
5. Elaborate the returned `inout` fields through fresh temporaries `__p1_tmp ... __pk_tmp` so that write-back does not
   interfere with residual pattern matching.
6. For each marked place, matched with formal parameter `pi`, perform a path-sensitive restoration using `__pi_tmp`:
   * If the place is an immutable local variable `x`, rebind `x` from `__pi_tmp`, shadowing the previous binding exactly
     as in ordinary linear state threading.
   * If the place is a projection path `root.f1 ... fn`, rebuild that path by nested record updates ending in the
     replacement `__pi_tmp`. The outermost `root` is then:
     * rebound if `root` is an ordinary immutable local binding, or
     * assigned if `root` denotes a mutable place introduced by `var`.
   * Because path-sensitive restoration elaborates to nested record updates, it is subject to the dependent-record
     update rules of §5.5.5. If the typestate transition of the restored `inout` path invalidates the type of any
     omitted sibling field in the rebuilt record, the `inout` application is a compile-time error. The user must unpack
     the record, perform the call, and manually reconstruct the dependent record, explicitly supplying any affected
     sibling fields.
   * Since the call arguments themselves are still ordinary expressions, a `var`-bound root is passed by reading its
     current contents per §8.5.1 rather than by passing the underlying `Ref`.
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
        deep handle state in comp
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

## 9. Errors and `try match`

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

### 9.4 `raise`

`raise err` is sugar for throwing an error in the current error monad:

* In any context with an implicit `MonadError m`, if `err : MonadError.Error m` then `raise err : m a` desugars to
  `throwError err`.

`raise` is permitted anywhere an expression of type `m a` is expected (commonly inside `except` handlers).

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

## 10. Collections, Ranges, and Comprehensions

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

### 10.3.1 Parsing: comprehension vs literal

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


### 10.3.2 Encounter order and order-sensitive clauses

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
    * In `for pat in collection`, `pat` must be **irrefutable** for the element type of `collection` (§6.1.1). If it is
      refutable, it is a compile-time error; use `for? pat in collection` (§10.4.1) instead.

* `let pat = expr` creates derived values within the comprehension.

    * `pat` must be irrefutable (§6.1.1).
    * For refutable matching, use `let? pat = expr` (§10.4.1).

* `if condition` filters out rows where the condition is `False`. Within the remainder of the comprehension after an `if
  condition` clause, typechecking proceeds under an implicit assumption `@p : condition = True` for the current row
  (analogous to §7.4.1).
* `if expr is C` filters out rows whose value of `expr` does not have top-level constructor `C`. It introduces no
  bindings. Within subsequent clauses, typechecking proceeds under the same success-side constructor-tag assumption and
  flow refinement as §7.4.1 for the current row. If `C` has named explicit parameters, later clauses may use
  constructor-field projection on `expr`; unnamed payloads still require `let?` or `match`.


Map iteration:

* Iterating a map yields entries as a record `(key : K, value : V)`. This enables destructuring via record patterns, for
  example:

  ```kappa
  { for (key = k, value = v) in myMap, yield k : v }
  { for (key = k) in myMap, yield k : 1 }
  ```


### 10.4.1 Refutable patterns in comprehensions

Comprehensions support refutable generators and refutable bindings:

**Refutable generator.** `for? pat in collection` iterates `collection` and keeps only elements matching `pat`.
Variables bound by `pat` are in scope in subsequent clauses. This form is ill-formed if dropping a non-matching element
would discard any component carrying a positive owned lower-bound obligation.

**Refutable binding.** `let? pat = expr` matches `expr` against `pat`. On success, pattern variables are bound and the
comprehension continues. On failure, the current row is dropped. This form is ill-formed if the discarded failure
residue would carry any positive owned lower-bound obligation.

For both forms, discarded values are droppable only when all discarded components carry quantities `0`, `&`, `<=1`, or
`ω`. Quantities `1` and `>=1` are not droppable here.

Desugaring per §10.10: via `filterMap` when `FilterMap f` is available; otherwise via `bind` + `empty`.

Tag-test filter:

```kappa
if expr is C
if expr is Type.C
```

This filters the current row stream by testing whether the top-level constructor of `expr` is `C`.

Rules:

* The right-hand side of `is` is a single constructor name only, with the same restriction as §7.4.
* The clause introduces no bindings.
* If the tag test succeeds, the current row is kept; otherwise the current row is dropped.
* Users who need bindings from a refutable match should write `let? pat = expr` directly.

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

### 10.5.1 Map key conflicts (`on conflict`)

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

### 10.6 Ordering, paging, distinct

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

#### 10.6.3 `distinct` / `distinct by`

`distinct` and `distinct by` operate on the current row environment at the clause site.

* For `distinct`, the deduplication key is the record of all currently bound names in scope, with fields ordered by
  Unicode scalar lexicographic order of the binder names.
* For `distinct by keyExpr`, the deduplication key is `keyExpr` evaluated in that environment.


```kappa
distinct
distinct by keyExpr
```

* `distinct` keeps unique rows based on the deduplication key being the full current row environment record. Two rows
  are distinct iff their deduplication-key projections differ under `Eq`.
* `distinct by keyExpr` keeps the first encountered row for each unique deduplication key `keyExpr`.
* Requires an `Eq`-like trait for the value used to determine uniqueness. Hashing may be used as an optimization
  (implementation-defined).

Representative choice:

* `distinct` / `distinct by` preserves the **first encountered** representative in the current iteration order.
* If the carrier's iteration order is unspecified, the chosen representative is unspecified as well.

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

    * Iterates over `collection` and matches each element against `pat`.
    * For each `c` from the outer `for`, pairs `c` with elements where `condition` holds.

Left join example (using group join + flattening):

```kappa
[
    for c in customers
    left join o in orders on o.customerId == c.id into customer_orders
    for order in customer_orders.defaultIfEmpty(defaultOrder c)
    yield (c.name, order.totalAmount)
]
```

Desugaring (normative; the compiler is free to optimize):
* `join pat in xs on cond` is sugar for: `for tmp in xs, let? pat = tmp, if cond` where `cond` is evaluated in the scope
  where `pat` bindings are in scope.

* `left join pat in xs on cond into name` is sugar for binding `name` to the list of matching elements: `let name = [
  for tmp in xs, let? pat = tmp, if cond, yield tmp ]` The bindings introduced by `pat` are not in scope after the `left
  join`; only `name` is.

### 10.9 Custom carriers

Any comprehension form can be prefixed with a custom carrier type constructor:

```kappa
MyCustomList [ clauses..., yield valueExpr ]
MyCustomSet  {| clauses..., yield valueExpr |}
MyCustomMap  { clauses..., yield keyExpr : valueExpr }
```

This feature is supported by the `FromComprehension` trait (defined in §2.7). The compiler desugars the comprehension
using the rules in §10.10 and then invokes `fromComprehension` on the carrier with the resulting `Syntax` value.

Carriers are not required to implement every possible clause combination — they only need to accept the final desugared
form. This keeps the interface minimal while still allowing sophisticated carriers (e.g. database query builders,
streaming libraries, or effectful collection types) to intercept the entire pipeline.

### 10.10 Comprehension desugaring and required traits

Comprehensions desugar to a pipeline of combinators. The compiler must choose a desugaring that is well-typed and must
not require stronger trait constraints than necessary for the given comprehension shape.

We refer to a carrier type constructor `f : Type -> Type`.

Required combinators (conceptual traits):

* `Functor f` provides `map : (a -> b) -> f a -> f b`
* `Applicative f` provides `pure : a -> f a` and `liftA2 : (a -> b -> c) -> f a -> f b -> f c` (with `<*>` derivable
  from `liftA2`)
* `Monad f` provides `bind : f a -> (a -> f b) -> f b`
* `Filterable f` provides `filter : (a -> Bool) -> f a -> f a`
* `Alternative f` provides `empty : f a` and `(<|>) : f a -> f a -> f a` (with `orElse` as an alias)
* `FilterMap f` provides `filterMap : (a -> Option b) -> f a -> f b` (dropping elements that map to `None`).


(Exact names and trait factoring are not mandated, but the semantics correspond to these operations.)

Desugaring rules (list/set forms shown; map form analogous):

1. Yield-only:

   `[ yield e ]` desugars to `pure e` and requires `Applicative f`.

2. Single generator without refutation:

   `[ for x in xs, yield e ]` desugars to `map (\x -> e) xs` and requires `Functor f`.

3. Filters without refutation:

   If the comprehension contains `if cond` clauses but no refutable generators/bindings, it prefers `filter` when
   available:

   `[ for x in xs, if cond, yield e ]` desugars to `map (\x -> e) (filter (\x -> cond) xs)` and requires `Filterable f`
   and `Functor f`.

   If `Filterable f` is not available, it may desugar using `bind` and `empty` (requiring `Monad f` and `Alternative
   f`).

4. Multiple generators and/or dependency between generators:

   `[ for x in xs, for y in ys(x), yield e ]` desugars using `bind` and requires `Monad f`. (A later generator `ys(x)`
   that depends on earlier bound variables forces `bind`.)

5. Refutable generator (`for?`) and refutable let (`let?`):

   These forms are permitted only when the values discarded on refutation are droppable under §10.4.1.

   If `FilterMap f` is available, refutation desugars via `filterMap` and requires only `FilterMap f` (plus whatever is
   needed by surrounding clauses).

   Otherwise, refutation desugars using `bind` plus a match that produces `empty` when refutation fails, requiring
   `Monad f` and `Alternative f`.

6. Tag-test filter (`if e is C`):

   This is an ordinary filter clause, not a refutable binding form. It evaluates `e` at most once per incoming row,
   keeps rows whose top-level constructor is `C`, drops the others, and introduces no bindings.

The above is normative: implementations may produce equivalent code, but must preserve these semantics and constraint
minimality. The lambdas shown in these desugaring rules are schematic meta-notation. Compiler-generated closures
introduced solely to realize the comprehension pipeline are transparent to the abrupt-control boundary rules of §8.4 and
§8.5. Custom carriers receive the fully desugared pipeline as `Syntax` via the `FromComprehension.fromComprehension`
method. The compiler is not required to expose intermediate clause representations to carriers.

### 10.10.1 Evaluation-count guarantees

Within a comprehension, implementations must preserve the following evaluation-count guarantees (as-if rules):

* `let` clause right-hand sides are evaluated at most once per incoming row.
* `if` filter conditions are evaluated at most once per incoming row.
* In `order by`, each ordering key expression is evaluated exactly once per row.
* In `distinct by`, the key expression is evaluated exactly once per row.
* In `group by`:
    * `keyExpr` is evaluated exactly once per incoming row.
    * each aggregate `valueExpr` is evaluated exactly once per incoming row per aggregate field.

Implementations may cache computed keys and intermediate aggregate wrappers to satisfy these guarantees.

## 11. Algebraic Data Types and Type Aliases

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
                  | '(' '@' binder_body ')'
                  | '{' fieldDecl (',' fieldDecl)* '}'
fieldDecl       ::= [quantity] ident ':' type
```

In a constructor declaration, the record-style binder form `{ f1 : T1, ..., fn : Tn }` is syntactic sugar for the
sequence of named binders `(f1 : T1) ... (fn : Tn)`. It does not introduce a single record-typed constructor argument.
The `binder_body` nonterminal is the same one used by implicit binders in §7.3, so constructor parameters may use the
ordinary quantity and implicit machinery, for example `(@0 p : P)`. Receiver-marked binders and `inout` are not
permitted in constructor declarations.

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

* Constructors live in the **constructor namespace**.
* Parameters live in the **type namespace** (with sugar `a` ≡ `(a : Type)`) unless specified otherwise.
* If `opaque` is present on a data declaration, constructors are not exported (§2.5.3).

### 11.1.1 Constructor application with named arguments (`C { ... }`)

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

Note: Indexed data together with quantity-`1` arguments are sufficient to encode typestate and session-style protocols
as ordinary libraries. A state transition consumes a value at one index and produces a value at a new index;
non-transition operations may instead be expressed over borrowed parameters (`(& x : T state)`), preserving the current
index without consuming the underlying resource.

### 11.3 Type aliases

Type aliases use `type`:

```kappa
type Foo (x : Int) : Type = Int
type Id (a : Type) = a
opaque type Id (a : Type) = a
private type Internal = ...
```

* Parameters may be annotated; sugar: `type Id a = a` ≡ `type Id (a : Type) = a`.
* `type Name ...` with no `= ...` defines an abstract type whose implementation may be provided elsewhere
  (implementation-defined).
* If `opaque` is present on a type alias, the right-hand side is not unfolded by definitional equality outside the
  defining module (§2.5.2).

Type aliases vs type-level definitions:

Because Kappa is dependently typed, types are terms.

* `type T ... = RHS` is surface syntax for a type-level definition whose right-hand side is parsed in a type context.
* An equivalent form is a term definition with a universe type, e.g. `let T : Type = RHS`, when `RHS` is intended to be
  a type expression. Implementations may treat these as equivalent after elaboration.

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

## 12. Traits (Typeclasses)

Traits describe typeclasses / interfaces.

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

Fully applied trait applications have sort `Constraint`, not `Type` (§5.1.3).

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

Trait parameters default to kind `Type` when unannotated, but implementations may infer higher kinds from usage.

In `std.prelude`, `Ord` is declared with supertrait `Eq`, not `Equiv`, and its equality classes MUST agree with `(==)`.

In `std.prelude`, `Monad` is declared with supertrait `Applicative`.

### 12.1.1 Supertraits

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

#### 12.2.1 Overloaded member names

For each term member `m : τ` declared in `trait Tr params`, elaboration introduces an overloaded term name of the shape:

```kappa
m : forall params. Tr params => τ
```

A bare occurrence of `m` elaborates to projection from synthesized implicit evidence for `Tr params`.

An explicit dictionary projection such as `d.m` or `d.(op)` is the unsugared form.

Associated type members do not induce term-level overloaded names.

Associated type members:

* A member declaration of the form `Name : Type` is an associated type member.
* Inside the defining trait body, such a member may be referred to by its bare name `Name`.
* An associated type member is an associated type component of the dictionary for that trait. It is projected only
  through an explicit or implicit dictionary binder. For example, from:

  ```kappa
  trait Iterator (it : Type) =
      Item : Type
      next : (1 this : it) -> Option (item : Item, rest : it)
  ```

  one may write:

  ```kappa
  foo : (@ c : Type) -> (@ It : Iterator c) -> It.Item
  ```
* There is no separate trait-scoped projection mechanism of the form `Trait.Name args` in the core language.

Explicit dictionaries:

For a dictionary `d : Dict (Tr args)`:

* term members are projected as `d.name` or `d.(op)`,
* associated type members are projected as `d.Name` in type position.

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

### 12.3.1 Instance resolution algorithm

When solving a trait constraint goal `Tr args`, instance resolution proceeds as follows:

1. **Local implicit context first** Search local implicit values, unpacked implicit record fields, and any local
   instance declarations in scope first, innermost scope first. If this yields a unique solution, use it.

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

### 12.3.2 Instance search termination

An instance declaration is accepted only if each of its premises is structurally smaller than its head under a
well-founded metric on constructor depth and variable occurrence.

A conforming implementation may use any check at least as strict as the following Paterson-style condition:

* no premise may be identical to the head;
* for every type variable, the number of occurrences of that variable in a premise must not exceed its number of
  occurrences in the head;
* the total constructor-and-variable size of each premise must be strictly smaller than that of the head.

Implementations MAY use a stricter termination check.

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

## 13. Namespaces

Kappa uses multiple namespaces:

1. **Type namespace**:

    * Type constructors (`Maybe`, `Vec`, `Person`).
    * Type aliases (`type Id a = a`).
    * Effect-interface constructors (`State`, `Reader`, `Console`).

2. **Constraint namespace**:

    * Trait constructors (`Eq`, `Functor`, `Monad`).

3. **Term namespace**:

    * Value bindings (`let x = ...`).
    * Functions (`let foo = ...`).

4. **Constructor namespace**:
 * Data constructors (`Just`, `Nothing`, `VCons`).

5. **Label namespace**:
 * Record labels.

6. **Effect-label namespace**:
 * Effect labels introduced by effect-row syntax and used in `EffRow`-kinded rows and effect-operation selection.

7. **Module namespace**:
 * Module qualifiers introduced by import M / import M as A.
 * Module names are not terms and cannot be used as values.

Rules:

* Names may be reused across namespaces:

  ```kappa
  data Person : Type =
      Person (name : String) (age : Int)

  -- Person (type) and Person (constructor) do not clash.
  ```

* Within a single namespace at the same scope, duplicate definitions are a compile-time error.
* Imports may bring names into the term, type, constraint, and constructor namespaces, and may bring module qualifiers
  into the module namespace. The label namespace is populated by record field syntax; the effect-label namespace is
  populated by effect-row syntax (§5.3.2). Effect declarations populate the type namespace with effect-interface
  constructors.
* Effect operation names are not added to the global term namespace. They are selected through effect labels using
  `label.op` (§13.1).
* Constructor names are not imported as unqualified names unless explicitly imported with the `ctor` qualifier (§2.3.1).
  Constructors remain accessible via type scope selection (§13.2).

### 13.1 Dotted name resolution (`.`)

The `.` token is used for:

* **Module qualification:** `std.math.sin`
* **Type scope selection:** `Vec.Cons`
* **Effect-label operation selection:** `state.get`
* **Record field projection:** `p.x`
* **Constructor-field projection under positive tag refinement:** `e.payload`
* **Safe-navigation member access:** `e?.member`, desugared per §7.1.1.2
* **Explicit dictionary member projection:** `d.name`, `d.(==)`, and `d.Name` (in type position for associated types)
* **Method-call sugar:** `x.show`
* **Record update / row extension:** (`r.{ y = 99 }`, `r.{ z := 0 }`)

Effect labels inside effect-row syntax `<[ ... ]>` are not dotted forms. They are resolved directly in the effect-label
namespace described in §§5.3.1-5.3.2.

The form `lhs.{ ... }` is parsed as record update (§5.5.5) when its fields use `=`, or as row extension (§5.5.6) when
its fields use `:=`. Mixed forms are ill-formed. These forms do not participate in dotted name resolution.
Safe-navigation `lhs?.rhs` desugars before ordinary dotted-form disambiguation runs on the corresponding lambda body.

Resolution of dotted forms proceeds by attempting the following interpretations:

1. Module qualification
2. Type scope selection
3. Effect-label operation selection
4. Record projection
5. Constructor-field projection under positive tag refinement
6. Explicit dictionary member projection
7. Method-call sugar

Method-call sugar is considered only if the dotted form is not already well-formed as a record projection,
constructor-field projection, or explicit dictionary member projection at that use site. Therefore a record field,
constructor field, or explicit dictionary member takes precedence over method-call sugar of the same name.

Effect-label operation selection:

* A dotted form `l.op` denotes effect-label operation selection when `ContainsEff r l E` is solvable and the effect
  interface `E` declares an operation `op : Π (x₁ : A₁) ... . B`.
* In that case, `l.op` elaborates to a term of type `Π (x₁ : A₁) ... . Eff r B`, preserving the implicit/explicit,
  quantity, and dependency structure of the original operation signature.
* The effect row `r` is determined from the surrounding computation type and other available type information. If it
  remains unsolved after ordinary propagation, elaboration fails with an unsolved-metavariable error.
* Operation names are not brought into the global term namespace. They are available only via `label.op` and, when
  handlers are specified, within a handler for that label.

Effect-label example:

```kappa
tick : Eff <[state : State Nat | r]> Unit
let tick = do
    let n <- state.get ()
    state.put (n + 1)
```

Constructor-field projection under positive tag refinement:

* Suppose the current branch or comprehension row carries a positive tag-test refinement proving that the receiver
  expression `lhs` has top-level constructor `C` (as introduced by §7.4.1 or §10.4).
* If `C` declares a named explicit parameter `field`, then `lhs.field` is well-formed as a constructor-field projection.
* Such a projection has the type of that constructor parameter after the usual substitution of the scrutinee's indices
  and earlier constructor arguments.
* Operationally, `lhs.field` behaves as if the compiler had matched `lhs` against `C`, extracted the named parameter
  `field`, and treated all non-`C` branches as `impossible` under the current refinement.
* If `C` does not declare a named explicit parameter `field`, this interpretation does not apply. Users must use `match`
  to access unnamed constructor payloads.

Constructor-field example:

```kappa
data Reply : Type =
    Ok  { value : Int }
    Err { message : String }

let render (r : Reply) : String =
    if r is Reply.Err then
        r.message
    else
        "ok"
```

Disambiguation rule:

* The compiler attempts interpretations that are well-formed at the use site.
* If exactly one interpretation is well-formed, it is chosen.
* If more than one interpretation is well-formed, the use is ambiguous and is a compile-time error unless disambiguated
  (e.g. by explicit qualification/aliasing, explicit import qualifiers, or type ascription that forces a unique
  interpretation).

#### 13.1.1 Method-call sugar

Method-call sugar allows writing:

```
lhs.name
```

as a section (a function value) that supplies lhs as the receiver argument of name, even when the receiver argument is
not the first explicit parameter.

Eligibility:

A term name is eligible for method-call sugar iff:

1. the dotted form has not already resolved as record projection or constructor-field projection or explicit dictionary
   member projection under §13.1,
2. name resolves to a term f in scope, and
3. the elaborated type of f is a Pi-telescope with exactly one explicit receiver-marked binder as defined in §7.2.

If there is no explicit receiver-marked binder, or there is more than one, method-call sugar does not apply.

Elaboration / desugaring:

Let f be the resolved term for name. Let f have elaborated type:

```
f : Π p1. Π p2. ... Π pn. R
```

where `pk` is the unique explicit receiver-marked binder, binding the local receiver variable `rk` at type `Tk`.

Tk may depend on earlier binders p1..p(k-1).

To elaborate lhs.name:

1. Elaborate lhs to a term e with type E.

2. Instantiate binders before this: Introduce fresh metavariables (placeholders) for the binders p1..p(k-1), respecting
   their kinds/types. Let Tk' be Tk with p1..p(k-1) replaced by these metavariables.

3. Receiver unification: Require that E is definitionally equal to Tk' (via unification / definitional equality). This
   step may solve some or all metavariables for p1..p(k-1).

   If unification fails, lhs.name is not well-formed as method-call sugar.

   If unification succeeds but leaves any metavariable unsolved that affects the required type of the receiver position,
   lhs.name is a compile-time error (the receiver e has a fixed type and cannot typecheck for multiple incompatible
   choices).

4. Build the method section: Construct a lambda over the remaining binders p_i for i ≠ k that are not fixed by
   unification, preserving:

    * their relative order (the order induced by p1..pn with pk removed),
    * their binder names,
    * their implicitness (implicit binders remain implicit),
    * their quantities,
    * and their dependent types after substituting the solved metavariables and substituting `rk := e` in later binder
      types.

   The body of the lambda is an application of f with arguments supplied in the original order, inserting e at the
   receiver position.

   Intuitively, lhs.name elaborates to:

   ```
   \ (p1') (p2') ... (p(k-1)') (p(k+1)') ... (pn') ->
       f p1' p2' ... p(k-1)' e p(k+1)' ... pn'
   ```

   where any binder fixed by unification is not abstracted and is instead substituted directly.

Correctness note:

For any arguments supplied to the resulting lambda, beta-reduction yields an application of f in which lhs has been
placed exactly at the this binder position. Therefore lhs.name is definitionally equal to the corresponding eta-expanded
form of applying f with lhs as the this argument, subject to the substitutions described above.

### 13.2 Constructors via type scope

Constructors are accessible through their type constructor:

```kappa
Option.None
Option.Some 1
Vec.Cons head tail
```

Except for the fixed implicit prelude constructor subset of §2.6, unqualified constructor names are not imported by
default. If a module imports a type constructor, its constructors are available via that type's scope.

As a concise unqualified bulk form, `import M.(type T(..))` imports the type constructor `T` and all of its constructors
unqualified. Likewise `export M.(type T(..))` re-exports the type and all of its constructors.

## 14. Core Semantics

This chapter defines evaluation, elaboration, definitional equality, and erasure at a level sufficient to make the
language's typechecking and runtime behavior unambiguous.

### 14.1 Elaboration overview

Kappa source code is first translated into KFrontIR, the source-preserving frontend IR of §17.2, and is then elaborated
to KCore, the fully resolved core language of §17.3.

Elaboration performs (non-exhaustive):

* layout processing (`INDENT`/`DEDENT`) and parsing using fixities in scope,
* construction and phase resolution of KFrontIR (§17.2),
* name resolution across namespaces (term/type/constraint/ctor/module),
* implicit argument insertion (§7.3),
* insertion of coercions required by:
    * expected-type-directed variant injections and widenings (§5.4.3),
    * lawful record reorderings (§5.5.1.1),
* desugaring of:
    * pure `block` expressions and indented pure block suites to sequential local scope with closure-converted local
      declarations (§6.3.1, §14.1.1),
    * `do` blocks and control-flow sugar to monadic core (§8.2, §8.7),
    * comprehensions to combinator pipelines (§10.10),
    * `try match` and `try` to error-handling combinators (§9),
    * method-call sugar (§13.1),
* termination checking (§6.4),
* optional erasure planning (§14.4).

Elaboration must be terminating.

### 14.1.1 Elaboration of local declarations

A local declaration inside a block scope (`block` or `do`) elaborates by closure conversion over the free variables it
captures from the surrounding lexical scope.

Closure conversion:

* Let `Gamma` be the free variables of the local declaration drawn from the surrounding lexical environment.
* Elaboration introduces an internal declaration abstracted over `Gamma`.
* Within the user-written block, a bare occurrence of the local name is shorthand for that internal declaration applied
  to the current values or evidence of `Gamma`.
* This applies uniformly to local `data`, `type`, `trait`, `instance`, local signatures and named `let` definitions, and
  instances generated by `derive`.

Lexical identity:

* A local nominal declaration has one identity per declaration site, not per dynamic evaluation of the enclosing
  function or block.
* Distinct dynamic evaluations instantiate the same local declaration family at different captured arguments.
* Implementations MUST NOT treat local `data` or `trait` declarations as runtime-fresh generative names.

Escaping results:

* No separate syntactic prohibition prevents a local type, trait, or alias from appearing in a value that leaves the
  block.
* A value escapes exactly when the elaborated term remains well-typed after closure conversion.
* In particular, local type constructors may escape through `Type`-valued results and existential-style packages,
  provided the resulting elaborated interface type is well-typed.

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

### 14.2 Dynamic semantics (runtime evaluation)

Runtime evaluation is strict call-by-value with left-to-right evaluation order.

Evaluation order:

* Function application evaluates the function expression, then the argument expression, then applies.
* Record/tuple literals evaluate fields/elements left-to-right.
* Record update evaluates the scrutinee record, then evaluates updated field expressions left-to-right.
* `if` evaluates the condition, then evaluates exactly one branch.
* `match` evaluates the scrutinee once, then tests cases top-to-bottom; guards are evaluated only after the pattern
  matches.

`do` blocks:

* `do` blocks are elaboration-time sugar to monadic operations (`pure`, `bind`, sequencing).
* Evaluation order follows the desugared monadic structure.

### 14.3 Definitional equality

Definitional equality (also called conversion) is the equality relation used by the typechecker.

Definitional equality is the smallest congruence containing the following reductions (subject to opacity rules):

* β-reduction: `(\(x : A) -> e) v  ↦  e[x := v]`
* δ-reduction: unfolding of transparent terminating definitions and transparent type aliases whose bodies are available
  and not marked opaque at the use site. Definitions accepted solely via `assertTotal` (§16.4) do not unfold for
  definitional equality unless the implementation has separately verified them as terminating. Outside the defining
  module, opaque definitions and opaque type aliases do not unfold unless the importing module has explicitly clarified
  them (§2.5.2).
* ι-reduction: reducing `match` on known constructors/literals.
* η-equality (definitional):
    * Functions: `f ≡ (\(x : A) -> f x)` when `x` is not free in `f`.
    * Records: a record is definitionally equal to a record reconstructed from its projections (field-wise η), up to
      lawful reorderings.
    * The zero-field closed record is definitionally equal to `Unit`.

Quantities in definitional equality:

Using meta-notation `([q] x : A) -> B` for a binder of quantity `q`, two binder types `([q1] x :A1) -> B1` and `([q2] x
:A2) -> B2` are definitionally equal iff `q1 = q2`, `A1 ≡ A2`, and `B1 ≡ B2`.

Quantities are part of a function type's identity.

Definitional equality is strictly invariant with respect to quantities.

In particular, eta-expansion must preserve binder quantities; quantity-mismatched eta-expansions are not definitionally
equal.

The quantity subsumption of §7.1.3 is implemented by elaboration-time insertion of an eta-expansion (or equivalent
coercion term). It is not part of definitional equality and does NOT alter the underlying `≡` relation used by the core
dependent typechecker.

Normalization used by the typechecker may be fuel-bounded for performance, but must be sound: it must not claim
definitional equality unless it holds in the mathematical relation above.

Definitional equality also includes canonical normalization of:

* optional-type sugar: `T?` normalizes to `Option T` for all `T`,
* union / variant rows: normalize member types to a canonical order with duplicate removal for elaboration and
  definitional equality; runtime tagging uses stable member-type identities instead of row-local ordinals (§5.4, §14.5),
* record types/values: normalize to a canonical dependency-respecting field order (§5.5.1.1, §14.6).

### 14.3.1 Literal normalization

For any type `T` with a `FromInteger T` instance whose `fromInteger` is a total transparent function, `fromInteger @T n`
with `n : Integer` reduces by δ to a canonical value of `T` during definitional-equality checking. In particular,
numeric literals appearing as type indices are normalized before comparison.

### 14.4 Erasure

Types and universe terms are compile-time only and do not require runtime representation.

Every binder at quantity `0` is erased at runtime, except where elaboration retains constraint evidence as permitted by
§5.1.3. This applies equally to record fields declared at quantity `0`.

The runtime representation of a term is obtained by deleting all quantity-0 binders and their corresponding arguments at
applications, except for retained constraint evidence and explicit `Dict` values governed by §5.1.3.

All quantities other than `0` are computationally relevant and remain in the runtime term.

This erasure is sound: runtime behavior of ω-use terms is preserved.

The quantity-0 fragment and the runtime fragment are largely disjoint machinery: the quantity-0 fragment is primarily
the typechecker, while the runtime fragment contains all non-zero quantities together with any constraint-evidence
representation retained after elaboration under §5.1.3.

Lowering from KCore to KBackendIR occurs only after the erasure obligations of this section have been discharged
(§17.4).

### 14.5 Union / variant runtime representation

A value of type `Variant r` is represented at runtime as a tagged payload.

Runtime tags are not assigned by row-local ordinal position. Instead, each member type `T` of a union carries a stable
runtime tag identity `TagId(T)` derived from the canonical elaborated form of `T`.

Requirements on `TagId`:

* If `T₁` and `T₂` are definitionally equal member types, then `TagId(T₁) = TagId(T₂)`.
* If `T₁` and `T₂` are not definitionally equal member types, then `TagId(T₁) ≠ TagId(T₂)`.
* `TagId(T)` is independent of which particular row `r` currently contains `T`.
* `TagId(T)` must be stable across separately compiled modules and artifacts. A conforming implementation SHOULD derive
  `TagId(T)` from the same canonical normalization-and-hash procedure used for Hard Hashes (§15.1.2), applied to the
  fully elaborated member type `T`. If the concrete runtime tag uses a fixed-width truncation or other compact encoding
  of that identity, the implementation MUST resolve any resulting collisions deterministically so that the injectivity
  requirements above continue to hold. Consequently, two separately compiled modules that agree on the canonical Hard
  Hash of the elaborated member type `T` MUST assign the same `TagId(T)`.

Implementations MAY realize `TagId` using deterministic content-addressed identities, interned canonical-type
identifiers, or another injective encoding of canonical member types. The observable requirement is stable member-type
identity, not any particular concrete tag bit-pattern.

Injection:

* `(| v : T |) : Variant r` stores `TagId(T)` together with payload `v`.

Elimination:

* `match` on a variant inspects the runtime tag identity and dispatches to the matching branch.

Consequences:

* When elaboration inserts a widening from `Variant r₁` to `Variant r₂` where `r₁ ⊆ r₂`, that coercion is true
  zero-cost: the runtime representation is unchanged.
* Canonical row ordering remains relevant for row normalization and definitional equality, but not for runtime tag
  assignment.

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

### 14.7 Elision of mutable state

State elision and allocation elision:

Implementations MAY lower `newRef`, `readRef`, and `writeRef` operations to local stack allocations, register
assignments, or SSA form, completely eliding the heap-allocated `Ref` representation, provided the compiler can prove
that the `Ref` does not escape the current stack frame.

In particular, if a `Ref` introduced by `var` is never captured by a closure that outlives the function, and is never
returned or passed to a function that retains it, the implementation SHOULD NOT heap-allocate the reference.

This is an as-if optimization: the observable behavior of the program MUST remain the same as if the `Ref` had been
represented explicitly.

### 14.8 Runtime model for handlers and resumptions

This section constrains backend implementations of `handle` and `deep handle`. The concrete representation is
implementation-defined, but the observable behavior below is mandatory.

#### 14.8.1 Captured continuation boundary

When evaluation of `handle label in expr` or `deep handle label in expr` intercepts an operation at the handled label,
the captured continuation is the dynamic computation suffix from the operation site to the nearest enclosing matching
handler for that label.

The captured continuation includes:

* the evaluation context between the operation site and that handler;
* the loop, match, and return-target context in that segment;
* the active `do`-scope frames in that segment together with their current exit-action stacks;
* any intervening handler frames for other labels in that segment.

It does not include:

* frames outside the matching handler boundary;
* compile-time-only entities erased under Section 14.4.

#### 14.8.2 One-shot and multi-shot realization

Let an operation declaration carry resumption quantity `q` under Section 8.1.2.

* If `q` permits at most one use of the resumption value, an implementation MAY represent the captured continuation
  destructively.
* If `q` permits more than one use, the implementation MUST provide persistent continuation behavior: every use of the
  resumption value must behave as if it resumed from the same captured control state.
* Valid implementation techniques include CPS conversion, heap-allocated frames, stack copying, segmented stacks,
  defunctionalized state machines, or any equivalent strategy.

The representation chosen is not observable except through the semantics fixed by this chapter.

#### 14.8.3 Store interaction

Continuation reuse does not imply global store rollback.

* Effects and allocations performed before the operation site remain performed.
* Ordinary heap objects and references reachable from the captured environment remain shared in the ordinary way.
* A `var` or `newRef` allocation executed before the operation site therefore remains the same object across multiple
  uses of the same resumption value.
* Effects and allocations performed after the resumption point occur anew on each use of the resumption value.

This rule applies equally to shallow and deep handlers.

#### 14.8.4 Exit actions under resumption reuse

Because active `do`-scope frames are part of the captured continuation boundary under Section 14.8.1, their exit-action
stacks are logically copied with the continuation state.

Therefore:

* a pending `defer` registered before the operation site but inside the captured segment is pending independently in
  each resumed execution;
* a `using`-introduced release obligation inside the captured segment is likewise pending independently in each resumed
  execution;
* each resumed execution unwinds and consumes only its own copy of those exit actions.

If such copying would duplicate live linear or borrowed resources unsoundly, the call-site capture rule of Section 8.1.3
rejects the program.

#### 14.8.5 Deep handler reinstallation

A deep handler behaves as if it were lowered to a shallow handler plus an internal recursive driver as specified in
Section 8.1.5.

Each use of a deep resumption first resumes the captured shallow continuation and then reinstalls the same deep handler
around the resumed computation.

If the deep resumption is multi-shot, this reinstallation occurs afresh for each use.

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

### 15.1 The Two-Tier Hash System

Every top-level definition (term, type alias, trait, instance, `data` declaration, and similar declarations) has an
**Easy Hash**. A corresponding **Hard Hash** is defined for the same KCore term or module and is computed on demand and
cached.

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

#### 15.1.2 Hard Hash (Semantic Equivalence Hash)

The Hard Hash is computed on demand and cached, keyed by Easy Hash.

It is computed by full normalization of the definition, including:

* all Easy-Hash normalization steps,
* complete δ-unfolding of transparent definitions reachable from the definition being normalized,
* unfolding of `assertTotal` definitions only when the implementation has separately verified termination and recorded
  them as safe to unfold (§16.4),
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

### 15.2 Uses of the Hash System

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

#### 15.2.3 Macro and Elaboration-Time Caching

A macro invocation of the form `$(macro '{ args... })` MAY be cached using the tuple:

```text
(Hard Hash of the macro definition,
 tuple of Hard Hashes of the argument syntax,
 digest of the macro input transcript)
```

When the cache key matches a previous invocation, the compiler MAY reuse the previously elaborated result rather than
re-executing the macro body.

In package mode, the transcript component is empty. In script mode, this caching is sound only when the transcript
digest reflects the external inputs observed during macro execution (§5.8.6).

This mechanism supports:

* deterministic macro caching across files and compilations,
* safe incremental recompilation of macro-heavy code.

#### 15.2.4 Future / Recommended Uses (non-normative)

The two-tier hash system is designed to support additional applications, including:

* incremental compilation by detecting unchanged definitions via Easy Hash,
* proof and instance-dictionary caching,
* cross-module specialization and inlining decisions,
* reproducible-build verification by comparing Hard Hashes of final artifacts.

---

## 16. Unsafe and Debug Facilities

Kappa includes a small set of facilities intended for debugging, experimentation, trusted bootstrapping, and other
non-portable work.

### 16.1 Classification and portable subset

The safe portable subset of Kappa excludes:

* `unhide`,
* `clarify`,
* `assertTotal`.

Programs that use any of these facilities are outside the safe portable subset.

These facilities remain part of the language specification, but they are classified as unsafe/debug features rather than
ordinary portable surface forms.

The compiler observability facilities of §17.1.3-§17.1.7 are ordinary required tooling facilities.
They are not classified as unsafe/debug language features merely because they expose intermediate compiler
representations.

### 16.2 Build-level gating

The legacy per-module attributes `@AllowUnhiding` and `@AllowClarification` are not part of the language. These
facilities are gated at the build-configuration level rather than per module.

A compilation unit's manifest or build configuration (implementation-defined: package manifest, compiler flag, or
project file) specifies:

```text
allow_unhiding     : Bool
allow_clarify      : Bool
allow_assert_total : Bool
```

Defaults:

* In package mode, all three default to `false`.
* In script mode, implementations MAY default any or all of them to `true` for experimentation. Implementations SHOULD
  document the actual defaults they choose.

Violations are compile-time errors. Diagnostics for such errors MUST identify both:

* the offending `unhide` / `clarify` import item or `assertTotal` declaration, and
* the build setting (`allow_unhiding`, `allow_clarify`, or `allow_assert_total`) that disallows it.

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

### 16.4 `assertTotal`

Surface syntax:

```kappa
assertTotal let f : T = ...
```

`assertTotal` is an explicit trust boundary.

Semantics:

* `assertTotal` indicates that the programmer asserts a definition is total even if the compiler cannot prove it.
* `assertTotal` suppresses termination checking for acceptance only.
* A definition accepted solely via `assertTotal` is treated as opaque to definitional equality unless the implementation
  separately verifies its termination and records it as safe to unfold.
* `assertTotal` does not change the runtime semantics of the program.
* `assertTotal` is unsafe and is usable only when enabled by the `allow_assert_total` setting of §16.2.

---

## 17. Compilation pipeline and backend profiles

This chapter defines the normative implementation pipeline and the backend profiles standardized by this specification.

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

### 17.1.1 Incremental units

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

### 17.1.2 Compiler fingerprints

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
  * exported names by namespace,
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

The semantic hashes of §15 remain normative for semantic identity, coherence, and related uses.
They MUST NOT be assumed to be the sole or primary invalidation keys of a conforming implementation.

### 17.1.3 Compiler observability

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

### 17.1.4 Named checkpoints

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

### 17.1.5 Machine-readable dump formats

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

### 17.1.6 Pipeline traces and checkpoint verification

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

### 17.1.7 Reproducer bundles

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
  produce the separate-compilation interface artifacts of §17.1.9, or an observationally equivalent in-memory result.

* `compile`:
  lower checked roots through KBackendIR and target-profile lowering to a final artifact.

* `run`:
  execute checked roots using one of the execution strategies of §17.1.10.

The names of these tasks in any particular toolchain are implementation-defined.

#### 17.1.9 Separate-compilation artifacts

For separate compilation, a module MAY be represented by:

* source files;
* a module interface artifact; and
* a module implementation artifact.

A module interface artifact is backend-independent and MUST be sufficient for ordinary downstream compilation without
consulting the defining module's source text.

A module interface artifact MUST record at least:

* the module identity and dependency identities required by §§2.1-2.3.2;
* the exported surface by namespace, including importable fixity declarations;
* visibility and opacity classification of exported ordinary items;
* the signatures of exported terms, types, traits, constructors, associated types, effect interfaces, and effect
  operations, insofar as those entities are available to downstream code;
* top-level instance heads and any metadata required for instance search and coherence under §§12.3 and 15.2.1;
* the hashes or equivalent identity data required by §15 for exported definitions and instances;
* enough definitional content for exported transparent items to support downstream definitional equality.

A module interface artifact MAY omit:

* the bodies of private items;
* the definitional content of opaque items;
* target-specific code.

If a requested `unhide` or `clarify` operation would require private or opaque definitional content that is not present
in the available artifacts, compilation fails as specified in §16.3.

A module implementation artifact MAY contain target-specific code, backend-specific auxiliary data, implementation-
private bodies, or any combination thereof.
It MAY embed the interface artifact or refer to it by stable identity.

Ordinary downstream compilation MUST NOT require access to a target-specific implementation artifact.

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

### 17.2 KFrontIR

KFrontIR is the source-preserving, symbol-bearing, error-tolerant frontend representation of Kappa.

Normative role:

* KFrontIR is the representation used for incremental compilation, editor tooling, lazy semantic analysis, and
  source-based diagnostics.
* KFrontIR preserves user-written syntactic structure closely enough to support navigation, completion, refactoring, and
  diagnostic positioning.
* KFrontIR is not the source of truth for definitional equality, normalization, or the hashing rules of §15. KCore is
  the source of truth for those tasks.
* A conforming implementation MAY fuse the parser AST and KFrontIR, or omit materialization of KFrontIR entirely,
  provided the observable behavior is as if a representation satisfying this section existed.

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

#### 17.2.2 Resolve phases

KFrontIR phases are monotone. If phase `B` follows phase `A`, any node resolved to phase `B` is also resolved to phase
`A`.

A conforming implementation MUST behave as if the following phases exist:

* `RAW`: parser translation, source anchors, no semantic resolution.
* `IMPORTS`: module header handling, implicit prelude insertion, import/export parsing, longest-match dotted splitting,
  and fixity-environment setup.
* `DECLARATION_SHAPES`: namespace population, declaration-symbol creation, raw declaration headers, binder lists, local
  nominal identities, and raw block-scope declaration structure.
* `HEADER_TYPES`: explicit declaration-header types, supertypes, associated types, effect-operation headers, record
  field quantities, record dependency graphs, and other header-level type information.
* `STATUS`: visibility, opacity, export status, unsafe/debug gating, instance admissibility, and modifier legality.
* `IMPLICIT_SIGNATURES`: inference of declaration result types or initializer types when later resolution requires a
  signature that was omitted in source.
* `BODY_RESOLVE`: call resolution, type inference, implicit-argument insertion, quantity checking, region checking, flow
  facts, pattern reachability, handler typing, and body-local declaration resolution.
* `CHECKERS`: diagnostic production from resolved KFrontIR and any stored resolution errors.
* `CORE_LOWERING`: lowering of resolved KFrontIR to KCore.

An implementation MAY subdivide or fuse these phases, provided it preserves the monotone-phase invariant and the
observable results of queries, diagnostics, and KCore construction.

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

A `$(...)` splice, `type"..."` parse, or other elaboration-time expansion MAY likewise be delayed until a phase first
requires the expanded form. The implementation MUST preserve:

* the source anchor of the original user-written construct, and
* a synthetic-origin mapping for the expanded subtree.

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

Tooling queries over syntactically incomplete files are valid. Where possible, they MUST return partial results rather
than failing wholesale merely because the surrounding file is incomplete.

#### 17.2.5 Tooling-facing queries

An implementation intended for editor, language-server, or refactoring use SHOULD expose queries over KFrontIR or an
equivalent abstraction.

Such queries SHOULD support at least:

* resolution of the declaration or symbol at a source position;
* navigation from reference to declaration;
* expression type and type-reference resolution at a source position;
* expected type, expected quantity, and expected effect row where those are well-defined at a source position;
* diagnostics for a file or declaration, even when the file is syntactically incomplete;
* completion candidate enumeration at a source position;
* find-usages and rename keyed by declaration-symbol identity.

API names, packaging, and transport are implementation-defined.

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
* a change to macro-observed external input MUST invalidate any expansion, cached query result, or downstream phase
  result that depended on that input.

The implementation MUST determine affected results by consulting recorded query dependencies and compiler fingerprints,
not merely by coarse file-level invalidation, unless the implementation can prove that such coarse invalidation is
equivalent for the affected request.

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
* compute module interface;
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

#### 17.2.9 Cancellation

A conforming implementation MAY support cancellation of analysis, checking, elaboration, lowering, or target-lowering
requests.

If cancellation is supported:

* cancellation MUST occur only at query boundaries or at implementation-defined safe points within a query;
* a cancelled request MUST NOT publish a partially formed query result as if it were complete;
* already completed and verified subquery results MAY remain memoized and available for reuse by later requests;
* cancellation MUST NOT corrupt memo tables, persistent caches, or stage dumps already published as complete.

Cancellation behavior is otherwise implementation-defined.

#### 17.2.10 Fine-grained source reuse

An implementation intended for editor or language-server use SHOULD preserve unchanged KFrontIR subtrees, declaration
symbols, and source origins across localized source edits when doing so is sound.

In particular, a localized edit SHOULD NOT by itself require rebuilding unaffected files, unrelated declarations in the
same file, or unchanged syntactic subtrees whose containing incremental units remain valid.

This is a performance recommendation rather than a semantic requirement.

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

#### 17.2.12 Whole-compilation phase order

Because the module dependency graph is acyclic (§2.2), whole-compilation phase order is constrained by module
topological order.

A module is **interface-ready** when the implementation has produced the module interface artifact of §17.1.9, or an
observationally equivalent in-memory representation.

A conforming implementation MUST behave as if batch compilation proceeds as follows:

1. discover the source files and module dependency graph;
2. construct `RAW` KFrontIR for every source file;
3. for each module in topological order, resolve enough of KFrontIR to make that module interface-ready.
   This includes at least `IMPORTS`, `DECLARATION_SHAPES`, `HEADER_TYPES`, `STATUS`, `IMPLICIT_SIGNATURES`, and any
   additional resolution or `CORE_LOWERING` needed to materialize exported signatures, importable fixities, instance
   heads, and exported transparent definitional content;
4. downstream modules may consult only imported interfaces for ordinary import resolution, downstream typechecking,
   instance search, and definitional equality.
   Imported implementation artifacts or source bodies are not required, except for same-module multi-fragment
   compilation or the unsafe/debug facilities of §16.3;
5. once a module's required imports are interface-ready, `BODY_RESOLVE`, `CHECKERS`, and `CORE_LOWERING` for that
   module may proceed in topological order or lazily per declaration;
6. KBackendIR lowering and target-profile lowering occur only after KCore exists for the chosen compilation roots.

An implementation MAY pipeline or parallelize these steps internally, provided the resulting behavior is as if the
ordering above had been respected.

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

* `Type`, universes, `Constraint`, `Quantity`, `Region`, rows, and labels;
* explicit binder quantities, explicit regions, and explicit implicit binders;
* proof terms and equality evidence;
* explicit handler forms, resumption quantities, and completion-carrying control structure;
* local nominal identities as determined by §14.1.1.

KCore contains no unresolved:

* parsing obligations,
* name-resolution obligations,
* overloading obligations,
* instance-search obligations,
* or type-inference obligations.

#### 17.3.1 KCore provenance and explainability

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
* safe-navigation or section lowering; and
* local-declaration closure conversion.

A KCore dump MUST expose this provenance.

An implementation SHOULD provide an explain query that, given a source span or KCore node, returns the provenance chain
from KFrontIR through KCore.

#### 17.3.2 Elaboration-time evaluation

The elaboration-time evaluator operates on KCore or an observationally equivalent internal representation.

It is used for:

* macro execution;
* `type"..."` parsing;
* compile-time reflection over `Syntax`;
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
* explicit handler frames, resumption objects, cleanup-scope operations, and error-propagation operations.

The following do not appear in KBackendIR except insofar as they are explicitly reified by a library or backend
intrinsic:

* universes and `Type`;
* row terms and row constraints;
* `Constraint`-sorted evidence erased under §5.1.3 and §14.4;
* anonymous or explicit region variables;
* quantity-`0` binders or fields;
* proof terms whose only purpose is compile-time reasoning;
* erased indices of dependent data.

### 17.4.1 Conceptual lowering stages

A conforming implementation MUST behave as if lowering from KCore to KBackendIR performs the following conceptual
stages:

* `ERASURE`:
  eliminate or discharge runtime-irrelevant universes, rows, regions, quantity-`0` binders, proof-only terms, and
  erased indices in accordance with §14.4.

* `CONTROL_LOWERING`:
  lower source-level abrupt control, completion-carrying control, handlers, resumptions, `defer`, `using`, `try`, and
  related constructs to explicit runtime control and cleanup operations.

* `ENVIRONMENT_LOWERING`:
  closure-convert lambdas and local declarations, and choose runtime environment layouts for captured values.

* `REPRESENTATION_SELECTION`:
  choose runtime representation classes, layouts, and tag encodings for data, variants, records, dictionaries,
  numerics, strings, bytes, collections, refs, handlers, and resumption objects.

* `CALL_LOWERING`:
  fix runtime calling conventions, retained-dictionary passing, entrypoint signatures, and ABI-neutral parameter/result
  conventions.

* `RUNTIME_CONTROL_FORM`:
  produce basic blocks and explicit control transfer, or an observationally equivalent structured runtime form.

An implementation MAY fuse, split, reorder, or partially overlap these conceptual stages, provided the resulting
KBackendIR is observationally equivalent to one produced by the staged lowering above.

### 17.4.2 Lowering legality checkpoints

At or before publication of KBackendIR, the implementation MUST validate that:

* every declaration and body selected for runtime lowering is fully resolved;
* no unerased `Type`, universe term, row term, anonymous region, quantity-`0` binder, or proof-only term remains unless
  it has been explicitly reified as an ordinary runtime value;
* every call, closure, and entrypoint has a fixed runtime arity and runtime calling convention;
* handler, resumption, cleanup, and error-propagation structures satisfy the runtime obligations of Chapters 8, 9, and
  14;
* data, variant, and record layout choices are fixed consistently with §§14.5-14.6;
* retained dictionaries and backend intrinsics have concrete runtime representations, or else compilation is rejected
  before target lowering;
* the compiled roots and exported runtime entrypoints are explicit.

Failure of any of these checks is a compile-time error.
A conforming implementation MUST NOT silently pass malformed or partially lowered runtime IR to target-profile lowering.

### 17.4.3 KBackendIR graph dumps and legality witnesses

A KBackendIR stage dump MUST be graph-capable.

At minimum it MUST represent:

* functions and entrypoints;
* basic blocks, control regions, or an observationally equivalent control graph;
* control-flow edges or equivalent successor structure;
* values and def-use or binding-use relationships;
* handler frames, resumption objects, cleanup scopes, and error-propagation structure;
* selected runtime representation classes and calling-convention facts relevant to debugging; and
* provenance links back to KCore.

If the implementation internally uses a structured form rather than explicit basic blocks, the dump MUST still expose
enough information to reconstruct control flow and unwinding shape for debugging purposes.

The KBackendIR verifier MUST be user-invokable.

A successful verification MAY be recorded in the dump as a verifier stamp or equivalent witness.

### 17.4.4 Post-KBackendIR target-lowering dumps

If a backend profile exposes post-KBackendIR intermediate forms, each such form MUST participate in the same
observability model:

* checkpoint naming;
* dumpability;
* version identification;
* provenance back to KBackendIR; and
* verifier access where the form has a verifier.

If the post-KBackendIR form is not naturally serialized as `json` or `sexpr`, the implementation MUST still provide a
`json` and `sexpr` manifest describing that form, its identity, and its relation to the surrounding checkpoints.

### 17.4.5 KBackendIR interpretation

An implementation MAY provide a KBackendIR interpreter for script execution, REPL use, tests, or debugging.

If it does:

* the interpreter executes only KBackendIR that has passed the legality checks of §17.4.2;
* interpreted execution is governed by the same selected backend profile and backend-intrinsic set as compiled
  execution;
* interpreted execution MUST be observationally equivalent to compiled execution under that same configuration.

A KBackendIR interpreter MAY retain extra source metadata for debugging, profiling, or diagnostics, provided that
metadata does not change program behavior.

### 17.4.6 Representation classes and specialization

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

### 17.4.7 Runtime calling convention

At KBackendIR boundaries:

* quantity-`0` parameters are absent;
* implicit parameters erased by §14.4 are absent;
* retained dictionaries appear as ordinary runtime parameters;
* owned, borrowed, and unrestricted source binders do not induce distinct runtime calling conventions by themselves once
  the program has passed the quantity and region checks, unless a backend explicitly exposes a backend-specific ABI for
  them.

This section does not change source typing. It specifies only the backend-neutral runtime view after elaboration.

### 17.4.8 Incremental reuse across lowering stages

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

### 17.5 Portable runtime obligations

A backend MUST preserve all observable source semantics of Chapters 4, 8, 9, 10, 14, 15, and 16.

In particular, a backend MUST preserve:

* floating-point equality and ordering as specified by §4.1.3;
* `Char` as a Unicode scalar value;
* the distinction between `String` and `Bytes`;
* the stable member-type tag identity requirements of §14.5;
* the record-canonicalization and path-sensitive consumption rules of §§5.5 and 14.6;
* the evaluation-count guarantees of §10.10.1;
* the cleanup, `defer`, `using`, error, and abrupt-control rules of §§8.6-8.7 and §9;
* the shallow/deep-handler and resumption rules of §§8.1.3-8.1.5.

Host-runtime features such as garbage collection, exceptions, stack unwinding, coroutines, JIT compilation, AOT
compilation, or dynamic linking are implementation techniques only. They do not redefine Kappa semantics.

### 17.5.1 Memory management

Memory-management strategy is implementation-defined.

A backend MAY use tracing garbage collection, reference counting, arena allocation, stack allocation, region-like
runtime disciplines, manual runtime-managed heaps, or any hybrid thereof, provided the observable behavior of the
program remains that of the source semantics.

In particular:

* `defer`, `using`, and `MonadFinally` obligations are language-level semantics and MUST NOT be weakened into host
  finalizer behavior;
* no backend may require prompt garbage-collection finalization to realize source-level resource release.

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

#### 17.6.1 Elaboration-available backend intrinsics

A backend intrinsic MAY additionally be classified as **elaboration-available**.

Only elaboration-available backend intrinsics may be called, unfolded, or otherwise demanded by the elaboration-time
evaluator of §17.3.2.

Whether a backend intrinsic is elaboration-available is part of the backend-intrinsic set and therefore part of the
effective build configuration and analysis-session identity.

If elaboration-time evaluation reaches a backend intrinsic that is not classified as elaboration-available, compilation
fails.

### 17.7 Portable foreign ABI

A conforming implementation MAY expose foreign-ABI adapters. The portable subset of such an ABI is defined after
elaboration and erasure.

Portable ABI rules:

* portable ABI functions are first-order after erasure;
* implicit parameters, `Constraint`-sorted parameters, quantity-`0` parameters, and proof-only parameters are erased and
  are not ABI-visible;
* resumption values, handler frames, cleanup frames, local nominal declarations, and anonymous borrow regions are not
  portable ABI values;
* direct borrowed parameters and direct borrowed results are not part of the portable subset;
* borrow-like or resource-like foreign interfaces in the portable subset MUST be represented using owned values or
  opaque resource handles.

Opaque resource handles are ordinary runtime values from the perspective of KBackendIR. Their concrete representation is
backend-specific.

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

### 17.9 JVM backend profile

A conforming implementation MAY provide the `jvm` backend profile.

Artifact kinds:

* class files;
* JARs or equivalent classfile bundles.

Rules:

* the backend MUST lower KBackendIR to JVM-compatible artifacts whose externally loadable units conform to the JVM
  classfile format;
* the backend MAY realize closures, dictionaries, and trampolines via synthetic classes, `invokedynamic`, method
  handles, or equivalent JVM mechanisms;
* the backend MAY use JVM exceptions or `try/finally` as implementation techniques for Kappa abrupt control and cleanup,
  but the observable behavior MUST remain that of §§8.6-8.7 and §9;
* the backend MAY rely on host garbage collection for ordinary heap objects, but source-level resource release remains
  governed by Kappa semantics, not by host finalization behavior.

A JVM adapter layer MAY additionally expose Java-friendly wrappers around portable Kappa exports, but those wrappers are
not part of the portable Kappa ABI.

### 17.10 CLR backend profile (`dotnet`)

A conforming implementation MAY provide the `dotnet` backend profile.

Artifact kinds:

* assemblies containing CIL and metadata;
* implementation-defined publish outputs derived from such assemblies;
* native outputs produced by optional Native AOT publish mode.

Rules:

* the backend MUST lower KBackendIR to CLR-compatible artifacts;
* the ordinary managed form of this profile targets assemblies containing CIL and metadata;
* the backend MAY additionally offer a Native AOT publish mode. Native AOT is a deployment mode of the `dotnet` profile,
  not a separate Kappa semantic profile;
* the backend MAY use CLR exceptions, delegates, runtime services, or host garbage collection as implementation
  techniques, but the observable behavior MUST remain that of Kappa source semantics.

If a program or library depends on runtime features unavailable under the selected CLR deployment mode, such as a
particular Native AOT mode, the implementation MUST reject that deployment with a compile-time or publish-time
diagnostic rather than silently weakening semantics.

### 17.11 WebAssembly backend profile (`wasm`)

A conforming implementation MAY provide the `wasm` backend profile.

The `wasm` profile has two subprofiles:

* `wasm-core`
* `wasm-component`

An implementation MAY provide either or both.

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

### 17.12 Backend conformance

A backend profile is conforming iff every accepted program, when compiled under that profile, behaves observationally as
required by this specification.

In particular:

* the implementation MAY fuse KCore, KBackendIR, and target lowering into fewer internal passes, provided hashing
  inputs, diagnostics, and observable execution remain as if the conceptual stages had been formed;
* the implementation MAY perform any optimization that preserves the semantics of §§14.2-14.7 and the backend
  obligations of this chapter;
* no backend may use a target-platform limitation as justification for silently changing Kappa program meaning.

---

## Appendix B. Pipe operators

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

### B.2 Interaction with other operators

* `|>` has low precedence (`1`), below ordinary comparison and arithmetic operators and below any user-defined operator
  with higher precedence. Thus tighter-binding operator expressions on the right of `|>` group there first.
* `<|` has precedence `0` and is right-associative, so `f <| g <| x ≡ f (g x)`.
* Pipe operators are ordinary infix operators. They do not alter the parsing or elaboration of dotted forms or
  method-call sugar (§13.1.1). In particular, `x |> obj.method` parses as `x |> (obj.method)`; the pipe does not
  retarget method-call sugar onto `x`.

### B.3 Optional: typed pipe

Implementations MAY additionally provide a monadic pipe:

```kappa
(|>=) : m a -> (a -> m b) -> m b
let (|>=) = (>>=)
infix left 1 (|>=)
```

This is merely a renamed `>>=` with pipe-compatible precedence for stream-style monadic code.

## Appendix G. ApplicativeDo

This appendix amends §8.2.

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

### G.3 Mixed blocks

If some `<-` binds are independent and others are dependent on earlier `<-` results, the compiler SHOULD partition the
block into maximal independent groups elaborated with `liftA2` / `<*>`, and then combine those groups with `>>=`.

This is an as-if optimization: the resulting program MUST remain observationally equivalent to the monadic desugaring of
§8.2 / §8.7.

## Appendix T. Standard test harness

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

### T.5 Assertion directives

Unless otherwise stated, assertions are evaluated after the selected test mode has completed.

A test fails if any standard assertion is unsatisfied.

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

### T.6 Suite-level behavior

Within a directory suite:

* configuration directives in `suite.ktest` apply to the whole suite;
* file-relative assertions written inline in a `.kp` file apply to that file;
* suite-wide assertions such as `assertType`, `assertStageDump`, and diagnostic assertions may appear inline or in
  `suite.ktest`.

If the same configuration key is specified more than once with different values, the suite is ill-formed.

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
