# CHANGELOG

## [Unreleased]

### Changed

- Consolidated planning documents: deleted RELEASE.md, transformed ROADMAP.md into comprehensive release plan to v1.0 (19 releases), refactored TODO.md to focus on current milestone (v0.2.0) and tech debt

## [0.1.0] - 2025-11-09

### Added

- PEP8-focused formatter with both full-document and range formatting handlers, plus regression coverage via the formatter integration suite
    - `d9229d9`
    - `4a70e7a`
- Parser and analyzer foundations: preserved call targets, analyzer crate extraction, walker refactor, and expanded type-annotation parser fixtures
    - `e85bf3d`
    - `1b9f476`
    - `006b095`
    - `8098357`
- Initial Hindley-Milner type system surfaces including constant-expression evaluation, pattern-matching support, and HM integration tests
    - `60ecfdf`
    - `8892460`
- Static analysis and linting flows exposed through the CLI, including the `beacon analyze`/`beacon lint` commands and a `--version` flag
    - `117facb`
    - `5df3647`
    - `d867e32`
- End-to-end testing harnesses for parser and static analysis along with reorganized contributor documentation
    - `a282559`

### Changed

- Reorganized documentation to reflect the new testing strategy and roadmap, keeping `docs/internal` tasks aligned with delivered features.
- Analyzer architecture split into explicit modules (parser, analyzer, server) to simplify future CLI integration.

### Fixed

- Corrected contravariant unification symmetry in the type checker, eliminating spurious diagnostics for generics with explicit variance annotations
    - `615206f`
