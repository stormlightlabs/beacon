# Beacon Roadmap

Strategic milestones for delivering a Hindley-Milner type system and LSP server for Python.

## LSP Features

- [x] Signature help (`textDocument/signatureHelp`) - function calls provide parameter lists, overloads, and hints
- [ ] Code actions / quick-fixes (`textDocument/codeAction`) - given a range or diagnostic,
      the server returns actionable fixes (insert import, refactor, etc.)
    - [ ] Auto-import / import management (`textDocument/codeAction`)
- [ ] Workspace/folder support (`workspace/didChangeWorkspaceFolders`):
      Allow the server to handle multiple folders/projects in a single workspace.

### Hover & Diagnostics

- [ ] Diagnostics (errors/warnings) (`textDocument/publishDiagnostics`) (notification):
      The server sends diagnostics (syntax errors, type errors, warnings) as they are found.
- [ ] "Did you mean" suggestions and annotation fixes
- [x] Code actions for refactoring (insert annotations, adjust Optional)

### Snippet Engine

Provide intelligent code snippet insertion for common Python idioms and patterns,
with context-aware suggestions and LSP completion integration.

#### Core Snippet Library

- [ ] Pythonic patterns (list/dict/set comprehensions, generator expressions)
- [ ] Context managers (with statements, custom context managers)
- [ ] Decorators (property, staticmethod, classmethod, custom decorators)
- [ ] Exception handling (try/except/finally, context-specific handlers)
- [ ] File operations (open with context manager, pathlib patterns)
- [ ] Iteration patterns (enumerate, zip, itertools idioms)
- [ ] Class definitions (dataclass, attrs, property patterns)
- [ ] Function signatures (typing annotations, *args/**kwargs patterns)
- [ ] Async patterns (async/await, async context managers, async generators)
- [ ] Testing patterns (pytest fixtures, parametrize, mock patterns)
- [ ] Type annotations (Optional, Union, Generic, Protocol)
- [ ] Common algorithms (binary search, memoization, sorting patterns)

#### LSP Integration

- [ ] Snippet completion provider (`textDocument/completion` with snippet kind)
- [ ] Snippet placeholder navigation (tab stops, nested placeholders)
- [ ] Context-aware snippet filtering (only show relevant snippets)
- [ ] Snippet preview and documentation in completion items
- [ ] Multi-cursor snippet insertion
- [ ] Snippet variable resolution ($TM_FILENAME, $LINE_COMMENT, etc.)

#### Context Awareness

- [ ] Detect current scope (module, class, function) for relevant snippets
- [ ] Infer types from context to suggest appropriate snippets
- [ ] Filter by available imports (suggest snippets using imported modules)
- [ ] Consider indentation level and surrounding code
- [ ] Detect incomplete patterns and suggest completions
- [ ] Integration with type inference for typed snippets

#### Snippet Configuration

- [ ] User-defined custom snippets (per-workspace and global)
- [ ] Snippet priority and ranking
- [ ] Enable/disable built-in snippet categories
- [ ] Snippet trigger customization (prefix, regex, keyword)
- [ ] Snippet expansion keybindings
- [ ] Import auto-insertion for snippet dependencies

#### Quality & Usability

- [ ] Snippet validation (ensure syntactic correctness)
- [ ] Real-world usage examples in snippet documentation
- [ ] Snippet search and discovery interface
- [ ] PEP8-compliant snippet formatting
- [ ] Snippet testing framework (ensure all snippets are valid)

### PEP8 Formatting

Implement a PEP8-compliant code formatter with LSP integration, offering both document-wide
and range-based formatting with configurable style options.

#### Core Formatting Engine

- [ ] Whitespace normalization (indentation, line spacing, trailing whitespace)
- [ ] Line length enforcement and intelligent line wrapping
- [ ] Import statement formatting and sorting (PEP8 ordering: stdlib, third-party, local)
- [ ] String quote normalization (single vs double quotes)
- [ ] Operator spacing (binary, unary, comparison operators)
- [ ] Trailing comma insertion for multi-line structures
- [ ] Blank line rules (top-level functions, classes, methods)
- [ ] Comment preservation and formatting

#### LSP Protocol Support

- [ ] Full document formatting (`textDocument/formatting`)
- [ ] Range-based formatting (`textDocument/rangeFormatting`)
- [ ] Format-on-save integration via `willSaveWaitUntil`
- [ ] Format-on-paste support
- [ ] Formatting provider registration and capabilities

#### Configuration Options

- [ ] Line length limit (default: 88 for Black compatibility, 79 for strict PEP8)
- [ ] Indentation size (spaces, default: 4)
- [ ] Maximum blank lines (top-level, within functions)
- [ ] String quote style (single, double, preserve)
- [ ] Trailing comma behavior (always, multiline, never)
- [ ] Import sorting strategy (PEP8, isort-compatible)
- [ ] Line continuation style (backslash vs implicit)
- [ ] Compatibility mode (Black, autopep8, strict PEP8)

#### Performance & Quality

- [ ] Incremental formatting (only changed regions)
- [ ] Idempotency testing (format(format(code)) == format(code))
- [ ] Benchmark formatting speed on large files (target: <500ms for 5000 lines)
- [ ] AST preservation verification (no semantic changes)
- [ ] Regression test suite with real-world Python codebases

## Integration Test Milestone

The integration plan mirrors the formatter regression style: inline fixtures, direct
assertions, and cargo tests committed to the repo. We’ll tackle suites in the
following order so each layer builds on the previous one.

1. **Parser E2E Harness**
   - Implement `parser_e2e.rs` per `docs/internal/e2e_parser_tests.md`.
   - Seed fixtures for advanced syntax, typing-heavy code, and pattern matching.
2. **Configuration Flow Coverage**
   - Use the parser harness to exercise config load + hot-reload scenarios formerly tracked in TODO.
   - Ensure `beacon.toml` / workspace settings round-trips are covered.
3. **Static Analysis E2E**
   - Add `crates/analyzer/tests/analyzer_tests.rs` & `crates/server/tests/static_analysis_tests.rs`
     as described in `docs/internal/e2e_analysis_tests.md`.
   - Focus on CFG/data-flow diagnostics over multi-file fixtures.
4. **Hindley–Milner Solver E2E**
   - Follow `docs/internal/e2e_hm_tests.md` to assert on inferred types and solver diagnostics.
5. **Linter E2E**
   - Build `linter_e2e.rs` according to `docs/internal/e2e_linter_tests.md`, verifying lint messages and autofixes.
6. **Completion Flow Validation**
   - After linter coverage, add integration tests for snippet/ completion pipelines (configuration + completion tasks moved from TODO).

## Infrastructure

### Static Analysis & Linting

- [ ] Linter rule engine (see TODO.md for BEA rule codes)
- [ ] Symbol reference tracking for unused detection
- [ ] Suppression support (`# type: ignore`, `# noqa:`)
- [ ] Per-rule configuration and severity

### Incrementality & Caching

- [ ] Constraint slicing by strongly connected components
- [ ] Disk cache for analysis results
- [ ] Parallelization for large projects
- [ ] StubCache LRU eviction (currently simple HashMap)

### Configuration & Ergonomics

- [x] Configuration options (`mode`, `pythonVersion`, `stubPaths`, `decoratorStubs`)
- [x] Config hot-reload without restart
- [x] Type checking modes (strict/balanced/loose)
- [ ] Formatting configuration (see PEP8 Formatting section)
- [x] Contributor documentation
- [ ] Configurable verbosity for inlay hints

### Performance & Testing

- [ ] Benchmarks for cold/warm runs and large projects
- [ ] Memory and CPU profiling
- [ ] Golden tests for inference outputs
- [ ] Property tests for unifier correctness
- [ ] Corpus harness (compare against MyPy/Pyright)
- [ ] Fuzzing for parser and solver

## Parking Lot

### Incremental Re-analysis

- [ ] Implement proper node-to-scope mapping (currently includes all nodes)
- [ ] Track scope dependencies for transitive invalidation
- [ ] Selective re-analysis: only re-analyze changed scopes (requires refactoring walker)
- [ ] Add scope boundary detection in position_map filtering

### Core Type System

- [ ] Add more comprehensive stdlib stubs (os, sys, pathlib, etc.)
- [ ] Implement type checking mode awareness (strict/balanced/loose)
- [ ] Auto-generate stubs from Python runtime introspection
- [ ] Pull stubs from typeshed repository
