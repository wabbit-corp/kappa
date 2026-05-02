# Kappa Spec Tour

This directory is a small spec-oriented Kappa package meant to be read with
`examples/spec-tour` as the source root.

It is broader than `examples/demo/hello.kp` and is intended to show the shape
of the language described in `Spec.md`, especially:

- multi-file modules and imports
- kind-qualified imports
- type aliases and record types
- same-spelling data families
- traits, instances, implicit evidence, and member projection
- recursion and pattern matching
- block expressions and local signatures
- `do` blocks
- `using`, borrowed parameters, and `inout`
- scoped effects and handlers
- query/comprehension syntax and prefix macros

Modules:

- `model.data`
  Core domain model plus recursive helpers and trait instances.
- `service.reporting`
  Query/comprehension and interpolated-prefix examples.
- `showcase.main`
  A small entry module that ties the other pieces together.
- `showcase.resources`
  Resource-oriented `do`-block syntax with `using` and `inout`.
- `showcase.effects`
  Scoped effects, effect rows, and a handler.
- `dsl.sql`
  A tiny DSL surface used by the query example.

This is a tour, not a claim that every advanced feature shown here is already
implemented on every backend/profile in the current compiler.
