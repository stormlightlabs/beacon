# Beacon Current Work & Tech Debt

Current milestone tasks and technical debt tracking. All integration tests are tracked in [e2e_tests.md](./e2e_tests.md).

See [ROADMAP.md](./ROADMAP.md) for the full release plan to v1.0.

## Cross-File Static Analysis (v0.6.0)

**Priority**: Current (2025-12-01) milestone focus

### Workspace-Level CFG & Data Flow

- [x] Cross-module CFG construction
- [x] Handle circular dependencies in CFG gracefully (SCC detection in CallGraph.reachable_functions)
- [x] Cross-file reachability analysis
- [x] Transitive type propagation across module boundaries
- [x] Taint analysis across file boundaries
- [ ] Implement Cross-Module CFG Linking (resolve import stubs to FunctionIds to enable true circular dependency detection)

### Enhanced Import/Export Analysis

- [x] Extend ImportDependencyTracker to track all symbol definitions (not just imports)
- [x] Build workspace symbol table with full module resolution
- [x] Inconsistent export detection (`__all__` mismatches)
- [x] Conflicting stub definitions across files
- [x] Star import resolution (`from foo import *`)
- [x] Unused import detection across workspace

### Cross-File LSP Features

- [x] Cross-file goto definition for all symbols (beyond imports)
- [x] Cross-file rename support
- [x] Multi-file refactoring infrastructure
    - [x] Generic refactoring framework
    - [x] Extract Function refactoring
    - [x] Move Symbol refactoring
- [x] Inline Function refactoring
- [x] Change Signature refactoring
- [x] Extract Variable refactoring

### Diagnostics & Validation

- [x] Cross-file diagnostics for import/export issues
    - [x] Invalid symbol imports (importing non-existent symbols from valid modules)
    - [x] Private symbol import warnings
    - [x] Re-export chain validation
- [x] Type mismatch diagnostics across module boundaries
    - [x] Argument count validation for imported functions
    - [x] Type checking for literal arguments against stub signatures
    - [x] Infrastructure for stdlib function validation
    - [x] Extend to user-defined functions by analyzing source modules
- [x] Cross-file dead code detection
    - [x] Added UnusedExport rule (BEA033)
    - [x] Implemented workspace-level reachability analysis
    - [x] Entry point detection (module init, **all**, imported symbols)
    - [x] Integration with diagnostics pipeline
    - [x] Fixed symbol lookup in diagnostic generation (search parent scope)

### Performance & Testing

- [x] Performance benchmarks for multi-file analysis
- [x] Memory profiling for workspace-wide analysis (Documented in debug.md)
- [x] Integration tests for cross-file CFG
- [ ] Integration tests for cross-file taint analysis (Added ignored test template; depends on Cross-Module CFG Linking)
- [x] Integration tests for Extract Function refactoring (Type inference checks disabled; requires Analyzer test env fix)
- [x] Integration tests for Move Symbol refactoring across files
- [x] Integration tests for refactoring with workspace dependency updates

### CLI Updates for Workspace Diagnostics

Recent work added comprehensive cross-file diagnostics (BEA031-BEA033, invalid/private symbol imports, re-export validation, cross-module type checking, magic method validation). These are available in LSP via DiagnosticProvider but CLI `lint` and `analyze` commands don't expose them.

**Current State**:

- `beacon lint` - Single-file linter only (BEA001-BEA033 basic rules)
- `beacon analyze` - Single-file linter + data flow (no cross-file analysis)
- `beacon debug diagnostics` - Full DiagnosticProvider with all cross-file checks (debug builds only)

**Tasks**:

- [x] Ensure all workspace initialization logic works in `debug diagnostics`
- [x] Enhance `analyze package` and `analyze project` implementations
    - [x] Implement workspace-level analysis using DiagnosticProvider
    - [x] Add cross-file diagnostics to package/project analysis
    - [x] Support workspace symbol resolution and import validation
- [x] Update CLI documentation
    - [x] Document new `debug diagnostics` features
    - [x] Add examples showing cross-file diagnostic detection
    - [x] Add cross-file diagnostic examples (BEA031-BEA033)

## Linter Tech Debt

**Priority**: v0.8.0 focus

### BEA022: UnusedIndirectAssignment

Issues with augmented assignments and nested scope tracking:

- [ ] Fix: Augmented assignments (`x += 1`) not tracked as assignments
- [ ] Fix: Nested function scope tracking for complex cases
- [ ] Unskip: `test_global_with_augmented_assignment`
- [ ] Unskip: `test_global_and_nonlocal_in_same_function`

### BEA023: ForwardAnnotationSyntaxError

Complex annotation parsing issues:

- [ ] Fix: Complex nested generic types produce false positives
- [ ] Fix: String quote handling in annotations
- [ ] Fix: Callable syntax validation incomplete
- [ ] Fix: Identifier validation too basic (only checks numeric start)
- [ ] Unskip: `test_annotation_nested_generics`
- [ ] Unskip: `test_annotation_complex_nested_types`

### Autofix Infrastructure

- [ ] Design autofix API for lint rules
- [ ] Implement autofix for simple rules (UnusedImport, RedundantPass)
- [ ] Add autofix tests
- [ ] Document autofix capabilities

### Per-Rule Configuration

- [ ] Design rule configuration schema
- [ ] Implement per-rule enable/disable
- [ ] Implement per-rule severity settings
- [ ] Add configuration tests
- [ ] Document rule configuration

## Snippet Engine (v.0.11.0)

### Core Infrastructure

- [ ] Design snippet definition schema (JSON/YAML)
- [ ] Implement snippet registry and loader
- [ ] Context matcher for scope-aware filtering
- [ ] Placeholder resolver using HM type inference
- [ ] LSP CompletionItem integration with insertTextFormat=Snippet

### Built-in Snippets

- [ ] Control flow: if/elif/else, for, while, match
- [ ] Exception handling: try/except/finally, with statements
- [ ] Function templates with type annotations
- [ ] Class templates (regular, dataclass, Protocol, TypedDict)
- [ ] Decorator templates (@property, @classmethod, @staticmethod, @overload)
- [ ] Type guard patterns and TypeIs/TypeGuard
- [ ] Common patterns: list comprehensions, dict comprehensions, generator expressions

### Advanced Features

- [ ] Dynamic placeholder generation based on inferred types
- [ ] Multi-cursor snippet expansion
- [ ] User-defined snippet support (custom snippets.json)
- [ ] Context-aware filtering (scope type, imports, surrounding code)
- [ ] Snippet variable expansion ($0, ${1:default}, $TM_SELECTED_TEXT)

### Testing & Integration

- [ ] Unit tests for snippet registry and matching
- [ ] Integration tests with VS Code
- [ ] Integration tests with Neovim/nvim-cmp
- [ ] Documentation and examples
- [ ] Performance benchmarks for snippet filtering

## Static Analysis Tech Debt

**Note**: Cross-file analysis tasks moved to [Cross-File Static Analysis (v0.6.0)](#cross-file-static-analysis-v060) section above

## Infrastructure Tech Debt

### Logging

See [Logging](#logging) section below for detailed logging standards.

#### Local Dev

- [x] Add detailed `debug!` logs for parsing, AST construction, type inference, symbol resolution
- [x] Add `info!` logs for file analysis start/finish, workspace detection, initialization
- [x] Log all protocol events (`textDocument/*`, `workspace/*`) using `debug!` and `trace!`
- [ ] Detailed symbol resolution tracing (partial coverage)
- [ ] Per-module diagnostic generation logging

#### LSP Client

- [ ] Use `client.log_message` for background diagnostic information
- [ ] Use `client.show_message` only for visible user warnings or fatal issues
- [ ] Keep protocol logs separate from user-facing notifications
- [ ] Respect client capabilities: only emit verbose logs if client requests `trace.server` or developer mode

#### Release

- [ ] Default to `RUST_LOG=warn` and disable `trace`/`debug` logging
- [ ] Strip log lines containing file paths, source code, or symbol names
- [ ] Keep only essential logs (initialization, version, fatal errors)
- [ ] Provide runtime flag (`LSP_LOG_LEVEL`) for fine-grained control

### Configuration

- [x] Configuration options (`mode`, `pythonVersion`, `stubPaths`, `decoratorStubs`)
- [x] Config hot-reload without restart
- [x] Type checking modes (strict/balanced/relaxed) - implemented but not enforced
- [ ] Formatting configuration validation
- [ ] Per-rule linter configuration (see Linter Tech Debt)

### Performance

- [ ] Parallelization for large projects (v0.13.0)
- [ ] StubCache LRU eviction tuning
- [ ] Memory profiling for large codebases
- [ ] CPU profiling and hotspot fixes

### Testing

- [ ] Split analyzer and LSP e2e tests per e2e_tests.md
- [ ] Add performance regression tests
- [ ] Fuzzing for parser and analyzer
- [ ] Stress testing with large codebases
- [ ] Debug Analyzer type resolution in integration test environment (to enable strict type checks in extract function tests)

## Logging

### Local Dev

#### System-wide Observability

- [x] Add detailed `debug!` logs for parsing, AST construction, type inference, and symbol resolution.
- [x] Add `info!` logs for file analysis start/finish, workspace detection, and initialization success.
- [x] Log all protocol events (`textDocument/*`, `workspace/*`, etc.) using `debug!` and `trace!`.
- [ ] Detailed symbol resolution tracing (partial coverage)
- [ ] Per-module diagnostic generation

### LSP Client

- [ ] Use `client.log_message` for background diagnostic information.
- [ ] Use `client.show_message` only for visible user warnings or fatal issues.
- [ ] Keep protocol logs separate from user-facing notifications.
- [ ] When snippet support or advanced features fail, emit a concise `window/logMessage` instead of dumping trace data.
- [ ] Respect client capabilities: only emit verbose logs if the client requests `trace.server` or developer mode.

### Release

- [ ] Default to `RUST_LOG=warn` (or lower) and disable all `trace`/`debug` logging.
- [ ] Strip any log lines containing file paths, source code, or symbol names.
- [ ] Keep only essential logs:
    - Initialization success/failure
    - Version and build hash
    - Fatal errors and crash reports
- [ ] Continue using `window/logMessage` for high-level, safe summaries ("Server initialized", "Analysis failed")
- [ ] Provide a runtime flag or env var (`LSP_LOG_LEVEL`) for fine-grained control in deployments.
