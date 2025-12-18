# Kappa Language Specification

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

- The **module name** is derived from the file path (Python-style). For example:
  - `std/base.kp` → module `std.base`
  - `user/orders/list.kp` → module `user.orders.list`
- The mapping is implementation-defined but must be consistent.

Implementations may also support an explicit top-level module header:

```kappa
module std.base
```

Rules:

* If a module header is present, it must match the implementation’s path-derived module name in “package mode”.
* In “script mode”, implementations may permit a module header that does not match the path-derived name (implementation-defined).

### 2.2 Acyclic imports

- The module dependency graph formed by `import` statements **must be acyclic**.
- Implementations must reject programs with cyclic module dependencies.

### 2.3 Imports

Imports are explicit; however, implementations provide an implicit prelude interface (declarations only) that is in scope by default (§2.5).

Valid import forms:

```kappa
-- Import the module only (qualified access only)
import std.math

-- Import module with alias
import std.math as math

-- Import specific names
import std.math.(sin, cos, pi)

-- Import all public names
import std.math.*

-- Import all public names except some
import std.math.* except (sin, pi)

-- Import from a URL module
import "https://example.com/lib".*
import "https://example.com/lib".(foo, bar)
import "https://example.com/lib".* except (unsafe, debug)
```

An `import` statement may contain multiple import specifications separated by commas:

```kappa
import std.base.(math, io), std.text.*
import std.math as math, std.io as io
import "https://example.com/lib".*, std.base.(term println)
```

Rule:

Syntactically, import parses as: `import importSpec (',' importSpec)*`.

Each `importSpec` is one of the “Valid import forms” shown above.

Constraints:

* `std.math` is a dotted module path (`ident("." ident)*`).
* URL imports use a **string literal** and are treated as modules by the implementation.
* In `... except (a, b)`, the names are **unqualified identifiers** referring to exports of that module.

Imports are **not re-exported** by default (see `export` below).

#### 2.3.1 Import item qualifiers (namespaces)

Kappa has multiple namespaces (§13). Import items may optionally specify which namespace (or subset) the item is imported into:
`kappa
import std.list.(type List)
import std.eq.(trait Eq)
import std.base.(term println)
`

Valid qualifiers are:

* `term`  (term namespace)
* `type`  (type namespace)
* `trait` (type namespace; restricted to traits)
* `ctor`  (constructor namespace)

If no qualifier is given (`import M.X`), `X` is imported into any namespace(s) in which `M` exports `X`. If this results in ambiguity at a use site, that use is an error unless disambiguated (by qualification, expected kind/type, or explicit import qualifier).

Constructors are not imported as unqualified names by default. Constructors can be accessed through type scope (§13.2).

If `ctor X` is imported, `X` becomes available as an unqualified constructor name in patterns and expressions (subject to ambiguity rules).

### 2.3.1 URL imports, pinning, and reproducibility

URL imports are intended for scripts and quick one-offs, but Kappa also supports reproducible builds.

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
  * `sha256:<hex>` where `<hex>` is lowercase or uppercase hex (implementation must accept both).
  * `ref:<text>` where `<text>` is an implementation-defined reference (e.g. a tag or commit id).
    * The **module identity** of a pinned URL import includes both the base URL and the pin string.

#### Compilation modes

Implementations must support (at least) two compilation modes:

* **Script mode**:
  * Unpinned URL imports are permitted.
  * Implementations may cache fetched content.
  * Builds are not guaranteed reproducible.
* **Package mode**:
  * Unpinned URL imports are a compile-time error.
  * Only pinned URL imports are permitted.
  * Module identity must include the pin string.

How a toolchain selects “script mode” vs “package mode” is implementation-defined (e.g. a compiler flag, a package manifest, a file directive, etc.).

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
-- Re-export all public names
export std.math.*
-- Re-export all public names except some
export std.math.* except (sin, pi)
-- Re-export from a URL module
export "https://example.com/lib".*
export "https://example.com/lib".(foo, bar)
export "https://example.com/lib".* except (unsafe, debug)
```

Rules:

* An `export M...` statement is only valid if the module `M` is imported in the same file (directly or via a previous `import` statement).
* `export` only affects **re-exporting imported definitions**. All top-level definitions in the current module are exported by default

### 2.X Hiding exports (`hide`)

By default, all top-level definitions in the current module are exported.
`hide` statements remove items from the module’s export interface.
`hide` affects only downstream visibility; it does not affect visibility within the defining module.

`hide` statements are **top-level only** and may appear anywhere. Their effect is order-independent.

Forms:

```kappa
hide Foo

hide term foo
hide type Foo
hide trait Eq

hide ctor Foo
hide ctor Foo.Bar

hide view Foo

hide alias Bar
```

Meaning:
* `hide Foo` hides any exported item named `Foo` in any namespace. 
  If Foo is a type/trait, this additionally hides its exported constructors and its exported view (pattern matching interface).
* `hide term x` hides the exported term x.
* `hide type T` hides the exported type constructor / type alias T.
* `hide trait T` is like hide type T, but requires that T is a trait.
* `hide ctor T` hides all exported constructors of type T (both construction and pattern usage) while still exporting T itself.
* `hide ctor T.C` hides a specific constructor C of type T.
* `hide view T` hides the pattern-matching view of T’s constructors (constructors may remain usable as terms).
  Pattern matching may only mention constructors whose views are visible at the match site.
* `hide alias T` applies to type aliases type T = ... and hides the alias’s definitional equation from downstream modules.
  Outside the defining module, T is treated as an abstract type of the same kind and does not unfold during definitional equality.

### 2.6 Prelude interface (implicit, declarations only)

Although Kappa has explicit imports, implementations provide an implicit **prelude interface** that is in scope by default.

Normative rule:

* Each source file is processed as if it began with an implicit import of a prelude module:

  ```kappa
  import std.prelude.*
  ```

The exact contents of std.prelude are implementation-defined, but it must include:
* any declarations required by surface syntax (e.g. Bool and the meanings of True/False),
* fixity declarations for any operator tokens that the implementation expects to parse “out of the box”
(e.g. `+`, `*`, `==`, `and`, `or`, `..`, `..<`), consistent with infix gating (§3.5.3).
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

Non-exhaustive but important list:

```text
let, in, if, then, elif, else,
match, case, is,
try, except, finally,
data, type, trait,
import, export, as, except,
do, return,
forall,
partial, instance, derive, 
infix, postfix, prefix, left, right,
var, while, break, continue, using,
yield, for, group, by, distinct, order, skip, take, top, join, left, asc, desc
```

Keywords are **soft** (contextual) keywords:

* The lexer recognizes the keyword tokens, but implementations must permit their use as ordinary identifiers in contexts where a keyword is not syntactically expected.
* Example: `let type = 42` is permitted (where `type` is a term name), while `type Foo = ...` uses `type` as a keyword.

### 3.3 Comments

* Line comments:

  ```kappa
  // this is a comment
  ```

* Block comments:

  ```kappa
  /* this is
     a block comment */
  ```

Comments do not nest (implementation may choose to support nesting, but spec does not require it).

### 3.4 Whitespace and indentation

Kappa uses a **significant indentation** (“offside”) rule similar to Haskell/Python.

* Blocks are introduced by certain keywords (`do`, `match`, `try match`, `trait`, `data`, `let ... in`, comprehensions).
* A block consists of one or more lines **indented more** than the introducer.
* All lines in a block must share the same indentation level (modulo continuation rules, which implementations may refine).

Semicolons `;` are optional statement separators:

* Newline at the base indentation level terminates a statement.

* `;` may terminate a statement early, allowing multiple statements on one line:

  ```kappa
  let x = 1; let y = 2
  ```

* Semicolons are never required.

There are **no general-purpose brace blocks**. Braces do not create statement blocks.

* `{ ... }` is reserved for **map literals** and **map comprehensions** (§10), and for **record update** / **named record patterns** when braces follow an expression or type name (§5.5, §7.6).

Blank lines and comment-only lines do not change indentation, but they are still logically line breaks and must not interfere with subsequent layout.

### 3.5 Operator identifiers and fixity

Kappa supports symbolic operator identifiers (e.g. `+`, `*`, `==`, `..`), which live in the term namespace.

#### 3.5.1 Operator tokens

An operator token is an implementation-defined sequence of non-alphanumeric, non-whitespace characters not otherwise forming a reserved token (such as `->`, string delimiters, comment delimiters, etc.). Common operator tokens include `+`, `*`, `==`, `&&`, `||`, `..`, `..<`.

Operators may be used as ordinary function names by parenthesizing:

```kappa
let add = (+)
let x = (+) 1 2
```

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

* A fixity declaration binds a precedence level (an integer; recommended range `0..100`), and
  a kind (infix, infix left, infix right, prefix, postfix) to one operator token.
* Fixities are **block-scoped** and apply from the point of declaration onward.
* `import`/`from` may bring fixities into scope as part of the imported module interface.
* Fixity declarations apply when parsing operator tokens in both term expressions and type expressions wherever operator parsing is supported by the grammar.
  (They do not change the meaning of reserved punctuation used by other syntactic forms, such as ->.)


#### 3.5.3 Infix gating

An operator token may only be used in an operator position if a matching fixity is in scope at that source location:

* infix position requires an `infix...` fixity in scope
* prefix position requires a `prefix` fixity in scope
* postfix position requires a `postfix` fixity in scope

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

#### 4.1.3 `Float` semantics

`Float` is IEEE-754 binary64 (a.k.a. “double precision”).

* Ordering on `Float` is a **total order** (no NaN weirdness leaking into `Ord`-like operations).
  * Intuitively, it follows IEEE-754 totalOrder semantics: all values are comparable, including NaNs.
* The default equality for `Float` (including the default `Eq Float` instance) is **raw-bit equality**:
  * two floats are equal iff their IEEE-754 bit patterns are equal.
  * This means (not exhaustively)
    * `+0.0` is not equal to `-0.0`
    * `NaN == NaN` may be `True` if the payload bits are identical
* A standard-library function is provided for IEEE “numeric” equality (the common `==` in many languages), 
  where NaN is never equal and `+0.0` equals `-0.0`.

#### 4.1.3 Sign

`-` is a **unary operator**, not part of the literal.

* `-123` is parsed as `negate 123`
* `-3.14e2` is `negate (3.14e2)`

Operator precedence rules determine association (see expressions).

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

Single-quoted `'...'` literals are **not valid** in v0.1.

#### 4.3.2 Multiline strings

Triple-quoted strings:

```kappa
let s : String = """
    This is a multiline string.
    Indentation inside the quotes is preserved.
"""
```

Implementations may normalize leading indentation, but the spec assumes the raw content (minus the delimiters) is used.

#### 4.3.3 Prefixed strings


A **prefixed string literal** has the form:

```kappa
prefix"..."
prefix"""..."""
```

Where prefix is an identifier (including backtick identifiers).

All prefixed strings support interpolation:
 * `$name` inserts a variable name.
 * `${expr}` inserts an arbitrary expression.
 * `${expr : fmt}` inserts expr using format string fmt (format mini-language is implementation-defined).

A literal `$` may be written as `\$`.

Semantic sketch:

* `f"..."` → value of type `String`.
  Desugars to a call to an intrinsic (e.g. `__f_string(rawBody)`).
* `re"..."` → value of type `Regex`.
  After interpolation, the resulting string must be a valid regex.
* `b"..."` → value of type `Bytes` (or `Array UInt8` in a minimal core).
  After interpolation, the string is encoded as UTF-8 to produce bytes.

Only prefixes `f`, `re`, and `b` are special in v0.1. Other `foo"..."` forms are invalid or treated as IDENT+STRING without interpolation, at the implementation’s discretion.

Name resolution rule:

`prefix` must resolve to a term in scope at the use site. If it does not, the program is ill-formed.

### 4.4 Unit and tuples

* Unit: `()`

* Tuple value:

  ```kappa
  (1, "two", 3.0)
  (42,)          -- single-element tuple
  ```

* Tuple type:

  ```kappa
  (Int, String, Real)
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
    * in general, `Typeu : Type(u+1)`.

* `Type` is **universe-polymorphic** surface syntax:
    * each occurrence of `Type` introduces a fresh universe level metavariable (implementation-defined notation, e.g. `?u`),
      meaning “some universe level inferred by the compiler”.
* `*` is syntactic sugar for `Type`.

#### 5.1.1 Cumulativity

Universes are **cumulative**:

* if `u ≤ v`, then `Typeu` may be used where `Typev` is expected.

Implementations may realize this as an implicit coercion/subtyping rule between universes.

#### 5.1.2 Universe inference (sketch)

Elaboration generates constraints on universe metavariables (e.g. `?u ≤ ?v`, `?u < ?v`) from typing.
If the constraints are unsatisfiable, compilation fails.
Unconstrained universe metavariables may be generalized at top-level (implementation-defined).

### 5.1.3 Erasure and elaboration time

`Type` and universe terms are compile-time entities. Kappa does not require runtime type information.

* Proofs and type-level computation may be erased at runtime.
* Implementations may provide library mechanisms to reify type information explicitly when needed (e.g. explicit dictionaries, quoted representations), but there is no implicit runtime reflection.

### 5.2 Function types

* Arrow types are **right-associative**:

  ```kappa
  A -> B -> C    ==    A -> (B -> C)
  ```

* Dependent arrow form:

  ```kappa
  (x : A) -> B
  ```

  which denotes a function whose result type may depend on `x`.

* Non-dependent arrow is sugar:

  ```kappa
  A -> B    ==    (_ : A) -> B
  ```

### 5.3 Universal quantification

`forall` is syntactic sugar over Pi-types:

```kappa
forall a. T                ==   (@a : Type) -> T
forall (a : S). T          ==   (@a : S) -> T
```

Examples:

```kappa
forall a. a -> Int
forall (n : Nat). Vec n Int -> Int
```

### 5.4 Union types

Union types may be written as:

```kappa
Int | String | Bool
```

#### 5.4.1 Semantics (restricted union subtyping)

Union types provide **restricted subtyping**:

* For any types `A` and `B`, values of type `A` may be used where `A | B` is expected.
* More generally, `A` is a subtype of `A | B | ...`.
* There is no general-purpose structural subtyping.

#### 5.4.2 Union equivalences and coercions

Union types are treated as:

* **Associative:** `A | (B | C)` is equivalent to `(A | B) | C`
* **Commutative (up to implicit coercion):** `A | B` can be coerced to `B | A`
* **Idempotent:** `A | A` is equivalent to `A`

Implementations should normalize union types to a canonical form for comparison and error reporting (e.g. flatten, deduplicate, stable order). This does not imply any distributive law under type constructors.

#### 5.4.3 Injection and elimination

* **Injection is implicit:** if `e : A` then `e : A | B` is permitted without explicit tagging.
* Elimination is by `match` (§7.5) and related pattern-test forms (e.g. `is`).

Elimination:

* A union value must be eliminated via `match` (or another construct defined in terms of `match`).
* Implementations must reject programs that attempt to treat `A | B` as `A` without a proof (e.g. without a `match`).

### 5.5 Records (named tuples)

Records are the single “struct-like” construct in Kappa.

#### 5.5.1 Record types

Syntax:

```kappa
(x : Int, y : Int)
(name : String, age : Int)
(value : Int, array : Array Int)
```

* One-element record type: `(x : T,)`
* Records form a dependent telescope of named fields. Field order is not semantically significant except for 
  dependencies: a field type may only refer to earlier fields in some dependency-respecting ordering.

#### 5.5.1.1 Record field order and lawful reorderings

Record types are **order-insensitive up to lawful reorderings**.

Well-formedness:

* In a record type `(f1 : T1, f2 : T2, ...)`, each field type `Ti` may refer only to fields that are earlier in the written order.
  (Equivalently: the written order must already be dependency-respecting.)
* References to later fields in the same record type are a compile-time error.

Intuition: record types form a dependent telescope. Fields may be reordered when the reorder does not violate dependencies.

More precisely:

* A field type may refer to earlier fields in the same record type.
* A reordering is **lawful** iff every field appears **after** all fields that its type depends on (a dependency-respecting permutation).
* Two record types are definitionally equal iff they have the same set of fields (by name) and there exists a lawful reordering that aligns them such that corresponding field types are definitionally equal.

Implementations may insert dependency-respecting reordering coercions implicitly where needed.

#### 5.5.2 Record values

Syntax:

```kappa
(x = 1, y = 2)
(name = "Bob", age = 33)
(x = 1,)           -- one-field record value
```

Access:

```kappa
let p : (x : Int, y : Int) = (x = 1, y = 2)
let a = p.x       -- 1
let b = p.y       -- 2
```

Well-formedness:

* In a record value `(f1 = e1, f2 = e2, ...)`, each expression `ei` may refer only to fields that are earlier in the written order.
* References to later fields in the same record literal are a compile-time error.

Record values are order-insensitive. Field order in a record literal does not affect its meaning.

Record types and values are usable as Σ-types (dependent pairs), subject to the lawful-reordering rule above.

#### 5.5.3 Records as function parameters

A function can take a **record argument**:

```kappa
let f (x : A, y : B) : R =
    ... x ... y ...
```

This is sugar for:

```kappa
let f (arg : (x : A, y : B)) : R =
    let a = arg.x
        b = arg.y
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

#### 5.5.4 Record update

Records are immutable values. Record update produces a new record with selected fields replaced:

`kappa
let p : (x : Int, y : Int) = (x = 1, y = 2)
let p2 = p { y = 99 }          -- (x = 1, y = 99)
`

Rules:

* `r { field = expr, ... }` is an expression.
* The update expression must mention only fields present in `r`’s record type.
* Field expressions are evaluated left-to-right.
* Updated fields may depend on earlier fields (including updated ones) according to the record telescope rules.

Implementations may support dotted update paths as sugar:

```kappa
-- Optional sugar (implementation-defined in v0.1; recommended for v1.0):
-- r { address.street = "Main" }
```


### 5.6 Propositions over booleans

Kappa provides a minimal bridge from runtime booleans to propositions usable in types.

#### 5.6.1 `IsTrue` and `IsFalse`

The language defines two built-in type families:

```kappa
IsTrue  : Bool -> Type
IsFalse : Bool -> Type
```

Intuition:

* `IsTrue b` is the proposition “`b` holds” (i.e. evaluates to `True`).
* `IsFalse b` is the proposition “`b` does not hold” (i.e. evaluates to `False`).

Canonical inhabitants:

* There exists a canonical term (name implementation-defined) inhabiting `IsTrue True`.
* There exists a canonical term (name implementation-defined) inhabiting `IsFalse False`.
* There are no constructors for `IsTrue False` or `IsFalse True`.

Proof terms of `IsTrue`/`IsFalse` are compile-time relevant and may be erased at runtime.

#### 5.6.2 Boolean-to-type coercion in type positions

In any position where a `Type` is expected, an expression `b` of type `Bool` is implicitly coerced to:

```kappa
IsTrue b
```

This enables dependent “proof fields” without introducing a separate proposition syntax:

```kappa
let r : (id : Int, ok : id == 1) = (id = 2, ok = _)
-- parses as: ok : IsTrue (id == 1)
```

This coercion applies to:

* type annotations (`e : T`)
* binder types (`(x : T) -> ...`)
* record field types (`(x : T, ...)`)
* any explicit argument position expecting a value of type `Type`

### 5.7 Elaboration-time evaluation and splicing (`!`)

Kappa allows sequencing computations in expression positions using the prefix operator `!`.

#### 5.7.1 Term-level `!` in monadic contexts

Within a `do` block (§8), `!e` runs the monadic computation `e` and yields its result in an expression context.

Example:

```kappa
let main : IO () = do
let x = !readInt
let y = !readInt
println (x + y)
```

Desugaring sketch:

* Each `!e` introduces an implicit bind to a fresh temporary and uses that temporary at the `!e` site.

#### 5.7.2 Type-level `!` (type providers / compile-time evaluation)

In type positions, implementations may evaluate `!e` during elaboration to produce a compile-time value used for typechecking (e.g. for type providers).

Example shape:

`kappa
let readCSV (path : String) : (!typeCSV path).Row = ...
`

Rules:

* `!e` in a type position triggers elaboration-time evaluation of `e` (possibly with caching).
* The available effects and determinism requirements of elaboration-time evaluation are implementation-defined (and may depend on compilation mode).


---

## 6. Declarations and Definitions

### 6.1 Term declarations vs definitions

There are two distinct forms:

1. **Declaration (signature only):**

   ```kappa
   name : Type
   ```

   This introduces a type for `name` without providing a definition. Commonly used:

    * for trait members inside `trait` bodies,
    * as top-level signatures before a `let` definition.

2. **Definition (term binding):**

   ```kappa
   let name : Type = expr
   let name = expr
   let name (x : A) (y : B) : R = body
   let name (x : A, y : B) : R = body
   ```

   Any new term-level binding must begin with `let`, except inside `let ... in` (see below).

### 6.2 Top-level signatures + definitions

Canonical style:

```kappa
foo : Int -> Int
let foo x = x + 1
```

* For **functions**, a top-level type annotation is strongly recommended and may be required by implementations.
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

* Each binding is:

  ```kappa
  name [ : Type ] = expr
  ```

* Followed by `in` and a single expression.

`let ... in` is itself an expression; it can appear anywhere an expression is allowed.

### 6.4 Totality, unfolding, and `partial`

Kappa is total by default for definitions that participate in typechecking via definitional equality.

* A **total** definition may be unfolded during typechecking (delta-reduction), subject to implementation limits.
* A **partial** definition is treated as **opaque** for definitional equality: it is not unfolded by the typechecker.

Syntax:

`kappa
partial let diverge : Int = diverge
`

Recursion policy:

* There is no `let rec`.
* A top-level binding is permitted to be (mutually) recursive **only if** it has a preceding top-level signature declaration.

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
  * a compiler/back-end intrinsic.

Errors:
* If an expect is not satisfied, it is a compile-time error.
* If multiple satisfying definitions exist for a single expect, it is a compile-time error.
* The satisfying definition must match the expected signature (up to definitional equality).

Implementations may support selecting module “fragments” by target (e.g. `main.kp` + `main.win32.kp`). 
In such systems, `expect` declarations in common fragments are satisfied by definitions in the selected target fragments.

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
  
### 7.1.1 Dotted forms (`.`): qualification, type scope, projection, method sugar

The `.` token is used for:

* module qualification (`std.math.sin`)
* type scope selection (`Vec.Cons`)
* record field projection (`p.x`)
* method-call sugar (`x.show`)

Resolution of `lhs.name` is defined in §13.1.


### 7.2 Lambdas

Lambda syntax uses backslash and `->`:

```kappa
\ (x : Int) -> x
\ (x : Int) (y : Int) -> x + y
\ (@t : Type) (x : t) -> x
```

* Parentheses around parameters are required.
* Multiple parameters are curried:

  ```kappa
  \ (x : A) (y : B) -> e    :   A -> B -> T
  ```

### 7.3 Implicit parameters (`@`)

Implicit binders:

```kappa
\ (@t : Type) (x : t) -> x
```

* `@` marks an implicit parameter.
* Call sites may supply implicit arguments explicitly (e.g. `f @T x`).
* Trait-instance resolution is specified (see Traits), and may be used to fill implicit trait parameters.

#### 7.3.1 Constraint sugar.
A constraint arrow `C => T` is syntactic sugar for an implicit parameter: `(@_ : C) -> T`.
Multiple constraints associate to the right: `C1 => C2 => T ≡ (@_ : C1) -> (@_ : C2) -> T`.

### 7.3.2 Expression holes (`_`)

In expression position, `_` denotes a **typed hole**.

* The compiler attempts to infer a term that fills the hole.
* If the hole cannot be inferred, compilation fails with an error that reports the expected type and available context.

### 7.3.3 Implicit resolution (instance/proof search)

When elaborating an application, if the callee expects an implicit argument `(@x : G)` and the call site does not supply one, the compiler attempts to synthesize a term of type `G`.

Resolution proceeds in this order:

1. **Local implicit context**: if there is an in-scope implicit value whose type is definitionally equal to `G`, use it.
   The local implicit context includes:
   * implicit binders introduced by `(@x : T)` parameters, and
   * implicit assumptions introduced by control flow (see §7.4.1, §7.5.1, §10.4.1, §8.5.2).

2. **Trait instance resolution**: if `G` is a trait application (e.g. `Eq Int`), attempt to resolve an instance from the instance environment (coherent, non-overlapping in v1.0).

3. **Boolean proposition normalization**:
   * If `G` is `IsTrue b` and `b` normalizes (by definitional equality) to `True`, synthesize the canonical inhabitant of `IsTrue True`.
   * If `G` is `IsFalse b` and `b` normalizes (by definitional equality) to `False`, synthesize the canonical inhabitant of `IsFalse False`.

If all steps fail, compilation fails with an error indicating the unsolved implicit goal `G`.

#### 7.3.3.1 `summon` as a library function (not syntax)

Implicit resolution can be invoked using an ordinary function with an implicit parameter. A conventional helper is:

```kappa
summon : (t : Type) -> (@v : t) -> t
let summon t @v = v
```

Examples (using §5.6 coercion):

```kappa
-- inside a context where IsTrue (x > 0) is available implicitly:
let p : (x > 0) = summon (x > 0)

-- retrieving an instance dictionary:
let eqInt : Eq Int = summon (Eq Int)
```

### 7.4 Conditionals

`if` is an **expression**:

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

Inside a `do` block, an `if` without `else` is allowed as sugar (see §8).

### 7.4.1 Boolean branch assumptions

Conditionals introduce boolean assumptions into the implicit context:

```kappa
if cond then e1 else e2
```

Typechecking rules:

* `cond` must have type `Bool`.
* `e1` is typechecked with an additional implicit assumption:
  * `@p : IsTrue cond`
* `e2` is typechecked with an additional implicit assumption:
  * `@p : IsFalse cond`

For chained conditionals:

```kappa
if c1 then e1
elif c2 then e2
elif c3 then e3
else e4
```

* `e1` is checked under `IsTrue c1`.
* `e2` is checked under `IsFalse c1` and `IsTrue c2`.
* `e3` is checked under `IsFalse c1`, `IsFalse c2`, and `IsTrue c3`.
* `e4` is checked under `IsFalse c1`, `IsFalse c2`, and `IsFalse c3`.

These assumptions participate in implicit resolution (§7.3.2). In particular, `summon (c2)` within the `elif c2` branch can retrieve a proof of `IsTrue c2` (via §5.6 coercion).


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
* For open/infinite domains (e.g. `Int`), you must include a catch-all (`_`) or similar; otherwise it’s an error.

#### 7.5.2 Exhaustiveness with indexed types (GADTs)

For indexed/“GADT-style” types, pattern matching refines the scrutinee’s indices and may render some constructor cases impossible.

Implementations should:

* attempt to use definitional equality / unification of indices to detect unreachable cases,
* treat unreachable cases as not required for exhaustiveness,
* otherwise (if coverage cannot be established) require an explicit catch-all (`_`) or an explicit user-written case structure that proves impossibility.

### 7.5.1 Boolean matches introduce assumptions

When the scrutinee has type `Bool` and a case pattern is the boolean literal `True` or `False`, the corresponding branch is typechecked with an implicit assumption:

```kappa
match b
case True  -> eT   -- eT checked under  @p : IsTrue  b
case False -> eF   -- eF checked under  @p : IsFalse b
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
  (x = px,)         -- one-field record pattern
  ```
* Named record patterns:
  ```kappa
  User { name = n, age = a }
  ```
  Field punning is permitted:
  ```kappa
  User { name, age }     -- sugar for: User { name = name, age = age }
  ```
* Typed patterns (type ascription inside patterns):
  ```kappa
  (p : T)
  ```

#### 7.6.2 Scoping and validity

* Names bound by a pattern are in scope in:
  * the pattern guard (if present),
  * the branch/body expression,
  * subsequent clauses (for comprehensions).
* Duplicate binders within the same pattern are an error (e.g. `(x, x)`).
* A pattern match proceeds left-to-right; nested patterns are permitted.

---

## 8. Effects, `do` Blocks, and Control Flow

### 8.1 Monadic core

Kappa uses a minimal monadic core at the semantic level. For a monad `m`, we assume:

```kappa
pure  : forall a. a -> m a
(>>=) : forall a b. m a -> (a -> m b) -> m b
(>>)  : forall a b. m a -> m b -> m b
```

(Names may live in traits; here it’s a conceptual interface.)

### 8.2 `do` blocks

A `do` block sequences monadic actions:

```kappa
do
    let x <- action1
    action2
    let y <- action3 x
    finalExpr
```

Desugars (schematically) to:

```kappa
action1 >>= \x ->
action2 >>
action3 x >>= \y ->
finalExpr
```

Where the block’s type is some `m T`.

Valid statements inside `do`:

* **Bind (`<-`) **:

  ```kappa
  let pat <- expr
  ```

  This form is **monadic bind**, introducing new bindings from `pat`.

  Typing:

  * `expr` is expected to have type `m A`.

* **Assign (`<-`) **:

  ```kappa
  x <- expr
  ```

  * Here, `x` must be an existing mutable variable declared by `var x = ...`.
  * This form updates the mutable variable `x` with the result of `expr`.

  Typing:

  * `expr` is expected to have type `m A`, where `x` has type `A`.

* Pure assignment (assign to an existing `var` only):
    ```kappa
    x = expr
    ```
    
    * Here, `x` must be an existing mutable variable declared by `var x = ...`.
    * This form updates the mutable variable `x` with the pure result of `expr`.
    
    Typing:
    
    * `expr` is expected to have type `A`, where `x` has type `A`.

* **Expression statement**:

  ```kappa
  expr           -- expr : m Unit (or value coerced to m Unit)
  ```

* **Resource-scoped bind (`using`)**:
    ```kappa
    using pat <- expr
    ```
  
    This form is like `let pat <- expr`, but ensures that resources acquired by `expr` are released when the `do` block exits.

    It desugars to a `defer` mechanism (see §8.6).

* **Local definition**:

  ```kappa
  let x = expr   -- pure local binding inside the `do` body
  ```

* **Control-flow sugar** (loops, `if`, `return`, `defer`, etc.) described below.

* **Local declarations and scoping**:

`import`/`from`, fixity declarations (`infix`, `postfix`, `prefix`), and (optionally) local `data`/`type`/`trait` declarations are permitted inside a `do` block.
Their effects are scoped to the enclosing `do` block and apply from the declaration onward.

### 8.2.1 Labeled `do` blocks and labeled control flow

A `do` block may be labeled:

```kappa
outer@do
  ...
  break @outer
```

Control flow keywords may target a label:

`kappa
break@outer
defer@outer cleanup
`

Semantics follow Kotlin-style labeled control flow:

* `break@label` exits the labeled `do` block or loop.
* `continue@label` targets loops only.
* `defer@label e` schedules `e` to run when the labeled `do` block exits.


### 8.3 `if` without `else` in `do`

Inside `do` only, you may write:

```kappa
do
    if cond then
        stmt1
        stmt2
```

Desugars to:

```kappa
do
    if cond then
        stmt1
        stmt2
    else
        pure ()
```

So `if` remains an expression; the missing `else` is implicitly `pure ()` in the monad.

### 8.4 `return`

`return e` is a **control-flow keyword**, not the same as `pure`.

* Valid only inside a function body.
* Semantics: immediately terminates the current function and returns `e` as its result.
* Example:

  ```kappa
  let foo (x : Int) : IO Int = do
      if x < 0 then
          return 0
      let y = x + 1
      if y > 100 then
          return 100
      pure y
  ```

Implementations may implement this via CPS or an effect; spec treats it as function-level early exit.

### 8.5 Loops and mutable variables (`var`, `while`, `for` in `do`)

These are **syntactic sugar** over monadic operations.

* `var x = e` declares a mutable variable within the `do` block, backed by some reference type (e.g. `Ref Int`). Example:

  ```kappa
  do
      var x = 1
      x = x + 1
      ...
  ```

* `while cond do body`:

  ```kappa
  while cond do
      stmts
  ```

  Desugars to a recursive function in the monad:

  ```kappa
  let rec loop () =
      cond >>= \c ->
      if c then
          body >>
          loop ()
      else
          pure ()
  in loop ()
  ```

  (Exact desugaring is left to implementations; the invariant is: repeated evaluation until condition fails.)

* `for` in `do`:

  ```kappa
  for x in xs do
      stmts
  ```

  Desugars to iteration over `xs` in the monad, e.g. via a `Foldable`/`Traversable`-like trait.

* `break` and `continue`:

    * Valid only inside loops.
    * `break` exits the nearest loop.
    * `continue` jumps to the next iteration of the nearest loop.
    * Desugaring typically uses additional control effects; spec only requires their usual loop semantics.

### 8.5.1 Loop `else`

`for` and `while` may optionally include an `else` block:

```kappa
for x in xs do
    body
else do
    onNoBreak
```

Semantics:

* The `else` block runs iff the loop completes normally (i.e. no `break` is executed).
* If the loop exits via `break`, the `else` block is skipped.

### 8.6 `defer`

Inside `do`, `defer e` schedules `e` to be run when the enclosing `do` block is exited (normal or via `return`/exception):

```kappa
do
    file <- open path "r"
    defer file.close
    data <- file.read
    ...
```

Semantics:

* `e` must be a monadic action (e.g. `IO Unit`).
* `defer` actions run in **LIFO order** when exiting the `do`.

Implementation may desugar this to a bracket/finalizer mechanism; spec only fixes the ordering and guarantee of execution on exit.

---

## 9. Errors and `try match`

### 9.1 `MonadError`

Error-aware monads are described via a trait like:

```kappa
trait MonadError (m : Type -> Type) (e : Type) =
    throwError : e -> m a
    catchError : m a -> (e -> m a) -> m a
```

(Exact trait name and methods are not mandated in v0.1; this is conceptual.)

### 9.2 `try` / `except` / `finally`

`try` handles errors for monadic computations.

Syntax:

`kappa
try expr
except pat1 if guard1 -> handler1
except pat2           -> handler2
finally               -> finalizer
`

Rules:

* `expr` has type `m a`, where `m` supports error handling (conceptually `MonadError m e`).
* Each `except` handler must produce type `m a`.
* `finally` (optional) is a monadic action of type `m Unit` that always runs after success or error handling but before `try` returns.
* `except` clauses must be exhaustive over the error type `e` (with `_` allowed).

Semantics sketch:

1. Evaluate `expr`.
2. If `expr` succeeds, the result is the `try` result.
3. If `expr` raises an error, run the first matching `except` handler and use its result.
4. If `finally` exists, run it and discard its result.

### 9.3 `try match`

`try match` combines error handling and pattern matching.

Syntax:

```kappa
try match expr
case successPattern1 if guard1 -> successExpr1
case successPattern2           -> successExpr2
except errPattern1 if guardE1  -> errorExpr1
except errPattern2             -> errorExpr2
finally                        -> finalExpr
```

* `expr` has type `m a`, where `m` is a monad with an error type `e` (i.e. an instance of `MonadError m e`).
* `case` clauses pattern-match on the **successful result** `a`.
* `except` clauses pattern-match on the **error value** `e`.
* `finally` (optional) is a monadic action of type `m Unit` that always runs after success or error handling but before the whole `try match` expression returns.

Semantics sketch:

1. Evaluate `expr`.
2. On success (value `v : a`):

    * Run the first matching `case`-branch on `v`.
3. On error (value `err : e`):

    * Run the first matching `except`-branch on `err`.
4. If a `finally` clause exists, run it and discard its result.
5. The result of `try match` is the result of the taken `case`/`except` branch.

`try match` must be exhaustive on both the success and error sides (with `_` allowed as a catch-all).

---

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

### 10.2 Range literals

Syntax:

```kappa
from .. to        -- inclusive
from ..< to       -- end-exclusive
```

Examples:

```kappa
1 .. 10
1 ..< 10
'a' .. 'z'        -- if character-like types exist
```

Ranges rely on a trait like:

```kappa
trait Rangeable (v : Type) (t : Type) =
    range : (from : v) -> (to : v) -> (exclusive : Bool) -> t
```

Desugaring:

* `from .. to`  ≡  `range from to False`
* `from ..< to` ≡  `range from to True`

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

* `for` clauses
* `let` clauses
* `if` filters
* `order by ...`
* `skip n`, `take n`
* `distinct` / `distinct by expr`
* `group by expr` with aggregations
* `top n by ...`
* `join` / `left join` (joins)
* (all optional and composable)

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

* `for x in collection` binds elements of a collection.
* `let` creates derived values within the comprehension.
* `if condition` filters out elements where the condition is `False`.

### 10.4.1 Refutable patterns in comprehensions

Comprehensions support refutable generators and matches:

* **Refutable generator**:

  ```kappa
  for pat in? collection
  ```

  This iterates `collection` and keeps only elements that match `pat`. Variables bound by `pat` are available in later clauses.

* **Refutable let-binding**:

  ```kappa
  let pat ?= expr
  ```

  If `expr` matches `pat`, bind pattern variables and continue. Otherwise, the current element is dropped (as if filtered out).

* **Type/pattern test**:

  ```kappa
  if expr is pat
  ```

  Evaluates to `True` iff `expr` matches `pat`.

### 10.5 Map comprehensions and `:` vs type ascription

In a **map comprehension**, `yield keyExpr : valueExpr` is always interpreted as a **key/value pair**, not a type ascription.

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

#### 10.6.2 `skip` and `take`

```kappa
skip 5
take 10
```

* `skip n` discards the first `n` elements.
* `take n` limits the result to `n` elements.

`skip` and `take` are defined for ordered carriers (like lists or ordered streams). After `group by` or `distinct`, order may be unspecified until another `order by` is applied.

#### 10.6.3 `distinct` / `distinct by`

```kappa
distinct
distinct by keyExpr
```

* `distinct` keeps unique elements based on their entire value.
* `distinct by keyExpr` keeps the first element for each unique key `keyExpr`.
* Requires `Eq` and `Hash`-like traits for the type used to determine uniqueness.

Representative choice:

* `distinct` / `distinct by` preserves the **first encountered** representative in the current iteration order.
* If the carrier’s iteration order is unspecified, the chosen representative is unspecified as well.

### 10.7 Grouping

Grouping syntax:

```kappa
[
    for product in products
    group by product.category {
        key   = product.category          -- group key
        items = [product] using Concat    -- aggregated items via monoid
        count = 1 using Sum               -- aggregated count via monoid
    } into category_group
    yield (category_group.key, category_group.count)
]
```

Conceptual semantics:

* `group by expr { fields... } into name`:

    * `expr` defines the grouping key.
    * The block defines **aggregated fields** using monoidal folds:

        * `items = [product] using Concat` means “fold elements into `items` using the `Concat` monoid”.
        * `count = 1 using Sum` means “sum 1 for each item”.
    * `into category_group` binds a record with fields `key`, `items`, `count`, etc.

* After grouping, iteration order is unspecified until re-ordered.

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

Precise desugaring is left to the implementation; the semantics are those of inner/left joins over collections.

### 10.9 `top n by` sugar

Top-N sugar:

```kappa
[
    for player in players
    top 5 by (desc player.score, asc player.name)
    yield player.name
]
```

Is sugar for:

```kappa
[
    for player in players
    order by (desc player.score, asc player.name)
    take 5
    yield player.name
]
```

### 10.10 Custom carriers

Any comprehension form can be prefixed with a custom carrier:

```kappa
MyCustomList [ ... comprehension ... ]
MyCustomSet {| ... comprehension ... |}
MyCustomMap { ... comprehension ... }
```

This relies on a trait (conceptually `FromComprehension`) that defines how to build the carrier from a generic comprehension pipeline.

---

## 11. Algebraic Data Types and Type Aliases

### 11.1 `data` declarations

General form:

```kappa
data Name (params...) : TypeK =
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

* Constructors live in the **constructor namespace**.
* Parameters live in the **type namespace** (with sugar `a` ≡ `(a : Type)`) unless specified otherwise.

### 11.2 GADT-style constructors

Kappa supports GADTs by allowing constructor signatures with explicit result types:

```kappa
data Vec (n : Nat) (a : Type) : Type =
    VNil  : Vec 0 a
    VCons : (head : a) -> (tail : Vec n a) -> Vec (n + 1) a
```

Constructors can mix simple positional args and full GADT signatures if desired, but using `:` is the canonical GADT form.

### 11.3 Type aliases

Type aliases use `type`:

```kappa
type Foo (x : Int) : Type = Int
type Id (a : Type) = a
```

* Parameters may be annotated; sugar: `type Id a = a` ≡ `type Id (a : Type) = a`.
* `type Name ...` with no `= ...` defines an abstract type whose implementation may be provided elsewhere (implementation-defined).

---

## 12. Traits (Typeclasses)

Traits describe typeclasses / interfaces.

### 12.1 Trait headers

Canonical form:

```kappa
trait Eq (a : Type) =
    ...

trait Functor (f : Type -> Type) =
    ...

trait Monad (m : Type -> Type) =
    ...
```

Sugar:

```kappa
trait Eq a = ...           -- Eq (a : Type)
trait Marker a b = ...     -- both default to Type
```

Trait parameters default to kind `Type` when unannotated, but implementations may infer higher kinds from usage.

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
```

An instance declaration elaborates to a term definition (a dictionary value) whose type is the corresponding trait application.
Instance resolution searches these dictionary values (subject to coherence/non-overlap rules).

Rules:

* Instance declarations are top-level by default.
* Implementations may permit local instances only when the trait or the type constructor being instantiated is local to the same block (no “escaping” instances).
* Instances are **coherent** and **non-overlapping**:
  * for any trait application, at most one instance may apply.
* Orphan instances are disallowed:
  * an instance must be defined in the same module as the trait definition or the type constructor being instantiated (implementation-defined for URL imports and module identity).

### 12.X Traits as propositions (`Prop`)

Kappa provides a built-in marker trait:

```kappa
trait Prop (t : Type)
```

`Prop t` indicates that t is treated as a proposition type for the purposes of implicit resolution.

Rules:
* For any trait application `Tr args`, the compiler can synthesize an implicit instance of `Prop (Tr args)`. 
* Trait instance resolution is coherent:
  * for any fully-applied, ground trait type `Tr args`, there is at most one instance candidate available to instance search. 
  * if multiple candidates could apply, it is a compile-time error. 
  * This is a compiler-enforced property; it does not require a separate “Prop universe”.

### 12.4 Deriving

Deriving generates instances automatically for eligible declarations.

Minimal form:

```kappa
derive Eq Foo
```

Deriving is implementation-defined in v0.1, but v1.0 should at least support deriving `Eq` and `Show`-like traits for standard algebraic data types.

---

## 13. Namespaces

Kappa uses multiple namespaces:

1. **Type namespace**:

    * Type constructors (`Maybe`, `Vec`, `Person`).
    * Type aliases (`type Id a = a`).
    * Traits (`Eq`, `Functor`, `Monad`).

2. **Term namespace**:

    * Value bindings (`let x = ...`).
    * Functions (`let foo = ...`).

3. **Constructor namespace**:

    * Data constructors (`Just`, `Nothing`, `VCons`).

Rules:

* Names may be reused across namespaces:

  ```kappa
  data Person : Type =
      Person (name : String) (age : Int)

  -- Person (type) and Person (constructor) do not clash.
  ```

* Within a single namespace at the same scope, duplicate definitions are a compile-time error.

* Imports bring all three namespaces into the importing module, subject to the import form used.

### 13.1 Dotted name resolution (`.`)

The `.` token is used for:

* **Module qualification:** `std.math.sin`
* **Type scope selection:** `Vec.Cons`
* **Record field projection:** `p.x`
* **Method-call sugar:** `x.show`

Resolution of `lhs.name` proceeds in this order:

1. **Module qualification**
   * If `lhs` resolves to a module alias or module path, then `lhs.name` is a qualified lookup in that module (across namespaces as appropriate to context).

2. **Type scope selection**
   * If `lhs` resolves to a type constructor `T`, then `T.name` looks up constructors and other type-scoped names exported by `T`.

3. **Record projection**
   * If `lhs` is a term and its type is a record type containing a field named `name`, then `lhs.name` is record projection.

4. **Method-call sugar**
   * Otherwise, if `name` resolves to a function eligible for method sugar, then `lhs.name` desugars to `name lhs`.

Ambiguity:

* If more than one interpretation typechecks without additional information, it is a compile-time error unless disambiguated (e.g. by qualification, explicit type ascription, or explicit import qualifiers).

#### 13.1.1 Method-call sugar eligibility.
A function name is eligible for method-call sugar iff its first explicit binder is named `this`.
If `lhs.name` resolves by method-call sugar, it desugars to `name lhs`.
(If name additionally expects implicit parameters, they are resolved as normal implicit arguments after desugaring.)


### 13.2 Constructors via type scope

Constructors are accessible through their type constructor:

```kappa
Option.None
Option.Some 1
Vec.Cons head tail
```

Unqualified constructor names are not imported by default. If a module imports a type constructor, its constructors are available via that type’s scope.
