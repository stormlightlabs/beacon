# CHANGELOG

## [Unreleased]

### Added

### Changed

### Fixed

## [0.5.0] - 2025-12-01

### Added

- `cac2496`
    - Typeshed stdlib integration with automated build-time embedding
    - Method resolution through inheritance chain with overload support
    - Structural subtyping checks using class metadata
    - Type conversion to structural representations for protocol satisfaction
    - Tracing instrumentation throughout type checker and analyzer
    - Criterion benchmarks for type inference, LSP handlers, parser, and stub resolution
    - Comprehensive architecture documentation for typechecker, static analyzer, formatter, linter, LSP server, and typeshed integration

### Changed

- `cac2496`
    - Migrated from custom stubs to embedded typeshed stdlib stubs
    - Enhanced constraint solver with improved method resolution capabilities
    - Improved TypeVar registry handling with protocol bounds support
    - Reorganized development documentation into separate component guides

### Fixed

## [0.4.0] - 2025-11-21

### Added

- Type checking mode configuration system with three enforcement levels: strict, balanced, and relaxed modes
    - `15a014c`
- Strict mode rejecting implicit Any types in function signatures, requiring explicit class attribute annotations, and prohibiting bare except clauses
    - `9c3e6f8`
- Balanced mode with implicit Any warnings for gradual typing adoption
    - `e496f14`
- Relaxed mode for minimal enforcement in exploratory development
    - `ebc3626`
- Annotation coverage validation with mode-aware diagnostics
    - `60f5805`
- Per-file mode override capability via inline directives
    - `15a014c`
- LSP status integration displaying active type checking mode
    - `15a014c`
- Unit tests for diagnostics and hover features
    - `cf58b42`

### Changed

- Enhanced type_map population for function parameters and return types to improve LSP features
    - `e496f14`
- Reorganized diagnostic codes documentation for better maintainability
    - `7df336c`

### Fixed

## [0.3.0] - 2025-11-19

### Added

- Pattern matching type narrowing with guard support: match statements now properly narrow types in guarded cases using existing type predicate infrastructure
    - `a15a33d`
- TypeVar bounds and constraints validation: constraint solver now validates bounds and constraints when instantiating TypeVars
    - `9e7fc3c`
- Generator/AsyncGenerator/Coroutine mixed variance support: proper handling of invariant, covariant, and contravariant type parameters in async generators
    - `74b0193`
- Protocol inheritance checking: transitive protocol inheritance now properly validated across complex inheritance hierarchies
    - `74b0193`
- Comprehensive constant expression evaluator supporting all Python comparison operators (==, !=, <, <=, >, >=, is, is not, in, not in), dict/set literals, and parenthesized expressions for guard analysis
    - `a15a33d`
- Guard-aware pattern exhaustiveness checking: patterns with guards no longer incorrectly contribute to exhaustiveness coverage
    - `a15a33d`
- Stress tests for deeply nested generics and multiple inheritance scenarios
    - `74b0193`

### Changed

- Improved type display formatting in diagnostics for better readability
    - `74b0193`
- Better error recovery for partial type inference failures across the type checker
    - `74b0193`

### Fixed

- Pattern matching type inference edge cases with Python-specific semantics:
    - bool now correctly treated as subtype of int in pattern matching (case int() matches bool)
    - Inheritance checking for class patterns now uses ClassRegistry
    - Built-in type handling in pattern narrowing corrected
    - `dcab864`
- None pattern exhaustiveness bug: case None is now properly recognized in union exhaustiveness checking
    - `dcab864`
- Union type simplification now removes redundant types correctly
    - `dcab864`
- Recursive type handling improvements including ForAll unification and recursive classes
    - `dcab864`
- Variance unification corner cases in complex generic hierarchies and multi-parameter generic types
    - `1ba8c08`
- Generic type inference with multiple constraints now handles TypeVar bounds correctly
    - `1ba8c08`
- Protocol satisfaction checking with variance: protocols now respect variance annotations during subtyping checks
    - `1ba8c08`

## [0.2.1] - 2025-11-14

<!-- TODO: Fill out v0.2.1 release notes -->

## [0.2.0] - 2025-11-14

<!-- TODO: Fill out v0.2.0 release notes -->

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
