# Beacon Current Work & Tech Debt

Current milestone tasks and technical debt tracking. All integration tests are tracked in [e2e_tests.md](./e2e_tests.md).

See [ROADMAP.md](./ROADMAP.md) for the full release plan to v1.0.

## Type Checking Modes (v0.4.0)

Implement strict/balanced/loose mode enforcement with mode-aware diagnostic filtering.

### Configuration & Infrastructure

- [x] Design mode configuration schema (strict/balanced/loose)
- [x] Add `mode` field to workspace/file configuration with validation
- [x] Implement per-file mode override mechanism (e.g., `# beacon: mode=strict`)
- [x] LSP integration: show current mode in status/diagnostics
- [x] Mode detection and validation in config loader
- [x] Integration with existing type checker context
- [x] Add sample `beacon.toml` to sample workspace (`samples/`)

### Annotation Coverage and Type Inference Validation

- [x] Walk AST to find annotated assignments and parameters
- [x] Look up inferred types from `result.type_map` for annotated nodes
- [x] Compare user annotations with inferred types
- [x] Generate mode-aware diagnostics (error/warning/silent based on strict/balanced/loose)
- [x] Detect missing or partial annotations for coverage warnings

### Strict Mode Implementation

- [x] Reject implicit Any types in function signatures
- [x] Require explicit return type annotations on functions
- [x] Require explicit parameter type annotations (no inference)
- [x] Error on unannotated class attributes
- [x] Reject bare `except:` clauses

### Balanced Mode Implementation

- [x] Type inference with warnings on ambiguous cases
- [x] Warn on implicit Any from missing annotations
- [x] Suggest type annotations where inference is uncertain
- [x] Allow gradual typing (mixed annotated/unannotated code)
- [x] Improve type inference to populate type_map for simple function parameters and return types

### Loose Mode Implementation

- [ ] Permissive type inference (current default behavior)
- [ ] Minimal warnings for missing annotations
- [ ] Allow implicit Any without errors or warnings
- [ ] Maximum flexibility for gradual adoption

### Diagnostic Filtering & Categorization

- [x] Mode-aware diagnostic severity mapping (error vs warning vs silent)
- [x] Filter type diagnostics based on current mode
- [x] Diagnostic categorization by mode level (strict-only, balanced-only, etc.)
- [x] Preserve existing diagnostic codes (BEA0xx) with mode context
- [x] New diagnostic codes: ANN011 (parameter implicit Any), ANN012 (return type implicit Any)

## Typeshed Integration (v0.5.0)

- [ ] Implement [typeshed-stdlib-mirror](https://github.com/stormlightlabs/typeshed-stdlib-mirror) integration
    - [ ] Version-aware stub selection (3.8, 3.9, 3.10, 3.11, 3.12+)
- [ ] Stub loader with merge strategy (custom stubs override typeshed)
- [ ] Incremental stub updates without breaking analysis
- [ ] Stub cache invalidation on Python version change
- [ ] Documentation for stub sources and update process
- [ ] Tests for version-specific stub behavior

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

**Priority**: v0.6.0 focus

### Cross-File Analysis

- [ ] Extend ImportDependencyTracker to track symbol-level dependencies
- [ ] Build workspace symbol table with module resolution
- [ ] Cross-file reachability analysis
- [ ] Transitive type propagation across module boundaries
- [ ] Inconsistent export detection (`__all__` mismatches)
- [ ] Conflicting stub definitions across files

### Symbol Table

- [ ] Symbol reference tracking for unused detection improvements
- [ ] Better handling of star imports (`from foo import *`)
- [ ] Qualified name resolution across modules

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
- [x] Type checking modes (strict/balanced/loose) - implemented but not enforced
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

## Parking Lot/Deferred

- From 0.2.1
    - [ ] Add performance benchmarks for cache improvements
- From 0.4.0
    - [ ] Performance benchmarks (ensure modes don't add overhead)
    - [ ] Implement signature validation in DiagnosticProvider
