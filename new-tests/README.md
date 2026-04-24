# New Tests

This directory is a staging area for spec-derived tests that are not yet wired into the normal fixture discovery under `tests/Kappa.Compiler.Tests/Fixtures`.

Rules for files placed here:

- Keep directory names aligned with the eventual fixture-root style used by the main suite.
- Prefer ordinary Appendix T directives so a staged test can be moved with minimal editing.
- Keep one primary behavior per directory.
- Include provenance comments when the test came from an imported issue family.

Promotion rule:

- Once the relevant parser, elaboration, and harness support exist, move the directory into `tests/Kappa.Compiler.Tests/Fixtures` and update the live checklist in `ISSUE_IMPORT_PLAN.md`.
