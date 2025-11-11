# End-to-End Test Suite

This document describes the e2e test strategy for Beacon's static analysis and linting capabilities.
Tests follow the formatter pattern: realistic fixtures run end-to-end through the analysis pipeline with inline assertions.

## Test Organization

### Analyzer Crate Tests (`crates/analyzer`)

Unit tests for core analysis logic without LSP dependencies:

- CFG construction and reachability analysis
- Data flow analysis (use-before-def, unused variables, unreachable code)
- Linter rule validation (individual rule tests)
- Pattern matching and exhaustiveness

### Server Crate Tests (`crates/server/tests`)

Integration tests requiring LSP infrastructure:

- Full document analysis through `DocumentManager`
- Diagnostic generation and formatting
- Multi-file analysis scenarios
- LSP feature integration (code actions, fixes)

### HM Type Checker Tests (`crates/server/tests`)

Integration tests for Hindley-Milner type inference:

- `hm_type_checker_tests.rs` - Comprehensive type inference test suite
- Fixtures in `crates/server/tests/hm_fixtures/`
    - `functional_pipeline.py` - Higher-order functions, composition, map/filter
    - `typed_api.py` - Protocol, TypedDict, TypeVar bounds, structural typing
    - `async_ctx.py` - Async context managers, coroutines, async generators
    - `errors/occurs_check.py` - Self-referential types, infinite type detection
    - `errors/variance_failure.py` - Contravariance violations in callable types
    - `variance/covariant.py` - Covariant return types and immutable containers
    - `variance/contravariant.py` - Contravariant function parameters
    - `variance/invariant.py` - Invariant mutable containers
    - `variance/typevar_annotations.py` - Generic variance annotations

All 18 HM tests currently pass, covering:

- Generalization and polymorphic instantiation
- Protocol-based structural typing
- Async/await type inference
- Variance checking (covariant, contravariant, invariant positions)
- Error detection (occurs check, variance violations)

## Test Files

### Analyzer Unit Tests

Located in `crates/analyzer/src/`:

- Inline module tests within source files
- Test CFG building, data flow, and linting independently
- Fast, isolated unit tests without fixtures

### Server Integration Tests

Located in `crates/server/tests/`:

1. `static_analysis_tests.rs`
   - Loads project fixtures via `DocumentManager`
   - Runs CFG/data-flow/lint analysis
   - Asserts on diagnostic counts and messages

2. `linter_tests.rs`
   - Loads fixtures through `DocumentManager`
   - Runs lint walker
   - Asserts on diagnostic strings, severities, and ranges
   - Tests autofix code paths

### Fixture Directories

Located under `samples/e2e/`:

- `analysis/cfg_zoo/` - Control flow edge cases (branching, loops, exceptions)
- `analysis/web_api/` - FastAPI service with async patterns
- `analysis/packages/` - Multi-module import hierarchy
- `lint/web_api/` - FastAPI + aiohttp linting
- `lint/data_pipeline/` - Pandas/NumPy patterns
- `lint/mixed_quality/` - Intentionally messy script for rule coverage

## Coverage Highlights

### Static Analysis

- CFG reachability and block counts
- Data flow diagnostics (use-before-def, hoisting)
- Unreachable code detection
- Unused variable tracking

### Linting

- Rule stability (unused imports, async misuse, walrus patterns)
- Autofix verification (in-memory fixes + re-lint)
- Severity levels and message formatting

## Running Tests

```bash
# Analyzer unit tests
cargo test --package beacon-analyzer

# Server integration tests (all)
cargo test --package beacon-lsp

# Specific test suites
cargo test --package beacon-lsp --test static_analysis_tests
cargo test --package beacon-lsp --test linter_tests
cargo test --package beacon-lsp --test hm_type_checker_tests
```

## Test Statistics

### Skipped Tests (Analyzer)

The following tests are skipped with TODO comments indicating missing functionality:

1. `test_nested_functions_break_continue` - Linter correctly resets scope for nested functions
2. `test_is_not_literal` - Parser may not generate IsNot as single CompareOperator

## Known Gaps

- No automated dot-graph comparison for CFG visualization
- Autofix verification currently optional in linter tests
- Fixture directories need population (marked in TODOs)

## Guidelines

1. Keep assertions textual rather than snapshot blobs for clarity
2. Mirror formatter regression style: expect concrete strings/ranges
3. Share fixtures between formatter/analysis/linter suites where useful
4. Sort diagnostics deterministically for easier assertions
5. Test module-level analysis separately from function-level

## Next Steps

- [ ] Create helper `assert_diagnostics` util to compare `(rule_id, message, span)` tuples
- [ ] Populate missing fixture directories for integration tests
- [ ] Add CFG visualization comparison when needed
- [ ] Implement autofix test scaffolding for linter
- [ ] Investigate parser output for "is not" expressions

## TODO

### HM Type Checker

- [ ] Fixtures for row polymorphism and record types
- [ ] Bivariant variance for special cases (rare in Python)
- [ ] Variance checking for Protocol definitions
- [ ] Variance inference for generic classes without explicit annotations
- [ ] Symbol table integration for `get_symbol_type` to look up types by identifier name (stub exists, tests use pattern matching instead)
