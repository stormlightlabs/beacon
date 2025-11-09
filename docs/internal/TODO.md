# Beacon LSP Implementation Plan

Implementation details and mod-specific tasks.

## LSP Features

**Files:** `crates/server/src/features/diagnostics.rs`, `crates/server/src/features/code_actions.rs`

### Quick Fixes

- [ ] Move pattern before subsuming pattern (PM002)

### Configuration

- [x] Make interpreter path configurable (via `beacon.python.interpreterPath`)
- [x] Configuration file support (beacon.toml, pyproject.toml)
- [x] LSP configuration via workspace settings
- [x] Config hot-reload support
- [x] Diagnostic severity configuration
- [x] Inlay hints configuration implementation
- [ ] Integration tests for configuration flow (config loading and hot-reload)

## Snippet Engine

**Files:** `crates/server/src/snippets/mod.rs`, `crates/server/src/snippets/library.rs`, `crates/server/src/snippets/context.rs`, `crates/server/src/features/snippet_completion.rs`

### Snippet Infrastructure

- [ ] Create snippet module structure (`crates/server/src/snippets/`)
- [ ] Define `Snippet` struct (trigger, body, description, placeholders, scope)
- [ ] Implement snippet parser for LSP snippet syntax ($1, ${2:default}, $0)
- [ ] Build snippet registry (load, query, filter snippets)
- [ ] Define snippet categories (control-flow, types, testing, async, etc.)

### Built-in Snippet Library

- [ ] Control flow snippets:
    - [ ] `if`, `elif`, `else` with proper indentation
    - [ ] `for` loop with enumerate/range patterns
    - [ ] `while` loop with condition
    - [ ] `match`/`case` statement (Python 3.10+)
- [ ] Comprehension snippets:
    - [ ] List comprehension with filter
    - [ ] Dict comprehension
    - [ ] Set comprehension
    - [ ] Generator expression
- [ ] Context manager snippets:
    - [ ] `with open()` for file operations
    - [ ] Custom context manager class
    - [ ] `@contextmanager` decorator pattern
    - [ ] Multiple context managers
- [ ] Exception handling snippets:
    - [ ] `try`/`except`/`finally` block
    - [ ] `try`/`except` with specific exception types
    - [ ] Exception re-raising with context
    - [ ] Custom exception class definition
- [ ] Class definition snippets:
    - [ ] Basic class with `__init__`
    - [ ] `@dataclass` definition
    - [ ] `@dataclass` with default values and types
    - [ ] Property getter/setter pattern
    - [ ] Abstract base class with `@abstractmethod`
    - [ ] Protocol definition (structural typing)
- [ ] Function snippets:
    - [ ] Function with type annotations
    - [ ] Function with `*args` and `**kwargs`
    - [ ] Generator function with `yield`
    - [ ] Async function with proper signature
    - [ ] Lambda with type hint
- [ ] Decorator snippets:
    - [ ] Simple decorator function
    - [ ] Decorator with arguments
    - [ ] Class decorator
    - [ ] `@property`, `@staticmethod`, `@classmethod`
    - [ ] Functools decorators (`@lru_cache`, `@wraps`)
- [ ] Type annotation snippets:
    - [ ] `Optional[T]` type
    - [ ] `Union[T1, T2]` type
    - [ ] `List[T]`, `Dict[K, V]`, `Set[T]`, `Tuple[T, ...]`
    - [ ] `Callable[[Args], Return]`
    - [ ] Generic type variable definition
    - [ ] TypedDict definition
- [ ] Async/await snippets:
    - [ ] `async def` function
    - [ ] `await` expression with error handling
    - [ ] `async with` context manager
    - [ ] `async for` loop
    - [ ] `asyncio.gather` pattern
- [ ] Testing snippets:
    - [ ] pytest test function
    - [ ] pytest fixture
    - [ ] `@pytest.mark.parametrize`
    - [ ] Mock/patch pattern
    - [ ] Async test function
- [ ] Import snippets:
    - [ ] Conditional import (try/except ImportError)
    - [ ] `if TYPE_CHECKING:` block
    - [ ] Common stdlib imports (pathlib, typing, collections)
- [ ] File I/O snippets:
    - [ ] Read file with pathlib
    - [ ] Write file with context manager
    - [ ] JSON load/dump
    - [ ] CSV reader/writer

### LSP Completion Integration

- [ ] Register snippet completion provider
- [ ] Implement `textDocument/completion` handler for snippets
- [ ] Set `CompletionItem.kind` to `Snippet`
- [ ] Populate `CompletionItem.insertText` with snippet body
- [ ] Set `CompletionItem.insertTextFormat` to `Snippet`
- [ ] Add snippet documentation to `CompletionItem.documentation`
- [ ] Implement snippet ranking/sorting in completion list
- [ ] Support client snippet capabilities detection
- [ ] Handle clients without snippet support (fallback to plain text)

### Context-Aware Filtering

- [ ] Detect cursor position scope (module, class, function, block)
- [ ] Filter snippets by scope:
    - [ ] Module-level snippets (imports, class definitions)
    - [ ] Class-level snippets (methods, properties)
    - [ ] Function-level snippets (control flow, expressions)
- [ ] Parse surrounding code for context hints
- [ ] Check available imports and suggest relevant snippets
- [ ] Detect incomplete patterns (e.g., `with` without colon)
- [ ] Integration with type inference:
    - [ ] Suggest typed snippets based on inferred types
    - [ ] Fill placeholder defaults with type information
- [ ] Respect indentation level for snippet insertion

### Snippet Placeholders & Navigation

- [ ] Parse placeholder syntax ($1, ${2:default}, ${3|choice1,choice2|})
- [ ] Implement tab stop ordering ($0 for final position)
- [ ] Support nested placeholders
- [ ] Variable substitution:
    - [ ] `$TM_FILENAME` - current file name
    - [ ] `$TM_SELECTED_TEXT` - selected text
    - [ ] `$CLIPBOARD` - clipboard content
    - [ ] Custom variables based on context
- [ ] Transform placeholders with regex (${1/pattern/replacement/})
- [ ] Choice placeholders (dropdown selection)

### User-Defined Snippets

- [ ] Snippet definition format (JSON or TOML)
- [ ] Load snippets from workspace config (`.beacon/snippets/`)
- [ ] Load snippets from user config (`~/.config/beacon/snippets/`)
- [ ] Snippet validation on load (syntax, scope, triggers)
- [ ] Snippet priority system (user > workspace > built-in)
- [ ] Hot-reload snippets on config change
- [ ] Snippet conflict detection (duplicate triggers)
- [ ] Import dependency specification for snippets

### Smart Snippet Features

- [ ] Auto-import insertion for snippet dependencies
- [ ] Snippet body formatting (apply PEP8 rules)
- [ ] Conditional snippet parts based on context
- [ ] Snippet chaining (one snippet triggers another)
- [ ] Multi-cursor snippet expansion
- [ ] Snippet macros for dynamic content generation
- [ ] Snippet templates with file-level metadata

### Configuration

- [ ] Add `beacon.snippets.enabled` setting
- [ ] Add `beacon.snippets.userSnippetsPath` setting
- [ ] Add `beacon.snippets.categories` setting (enable/disable categories)
- [ ] Add `beacon.snippets.showDocumentation` setting
- [ ] Add `beacon.snippets.autoImport` setting
- [ ] Add `beacon.snippets.formatOnInsert` setting
- [ ] Add `beacon.snippets.triggerCharacters` setting
- [ ] Snippet ranking configuration (frequency, recency, alphabetical)

### Testing & Validation

- [ ] Unit tests for snippet parsing and placeholder extraction
- [ ] Unit tests for snippet filtering by scope
- [ ] Integration tests with LSP completion
- [ ] Validate all built-in snippets are syntactically correct
- [ ] Test snippet expansion with various placeholder types
- [ ] Test context-aware snippet filtering
- [ ] Test user-defined snippet loading and priority
- [ ] Test snippet formatting integration
- [ ] Performance tests (snippet lookup should be <5ms)

### Documentation & Discovery

- [ ] Document all built-in snippets with examples
- [ ] Create snippet authoring guide for users
- [ ] Snippet browsing command (list all available snippets)
- [ ] Snippet search by keyword/description
- [ ] Snippet usage analytics (track most-used snippets)
- [ ] Example snippet library in documentation

## PEP8 Formatting

**Files:** `crates/server/src/formatting/mod.rs`, `crates/server/src/formatting/rules.rs`, `crates/server/src/formatting/config.rs`, `crates/server/src/features/formatting.rs`

### Core Formatter Infrastructure

- [x] Core formatter infrastructure implemented (module structure, config, token stream, context tracking, and state machine)

### Whitespace & Indentation Rules

- [x] Implemented whitespace and indentation rules (indentation normalization, trailing whitespace removal, blank line management, operator spacing, delimiter spacing)
- [x] Created `docs/src/format/overview.md` and `docs/src/format/whitespace.md`

### Line Length & Wrapping

- [x] Implemented line length calculation with Unicode width support
- [x] Smart line breaking algorithm with prioritized break points (commas, operators, brackets)
- [x] Wrapping strategies for function calls, parameters, collections, and binary expressions
- [x] Created `docs/src/format/print-width.md`

### Import Formatting

Implemented PEP8-compliant import sorting with automatic grouping (stdlib/third-party/local), alphabetical ordering, multi-line wrapping, and deduplication.

### String & Comment Formatting

Implemented intelligent string quote normalization with escape-avoidance, docstring formatting, and comment spacing while preserving special directives (type: ignore, noqa, etc.).

### Structural Formatting

Implemented trailing comma handling, decorator spacing, type annotation rules, lambda wrapping, dictionary value indentation, comprehension wrapping strategies, and context-aware blank line management for classes and functions.

### LSP Integration

Implemented `textDocument/formatting` and `textDocument/rangeFormatting` handlers with proper LSP capabilities registration, error recovery, and configuration integration through the workspace config.

- [ ] Implement `textDocument/willSaveWaitUntil` for format-on-save
- [ ] Implement `textDocument/onTypeFormatting` for automatic formatting as user types (e.g., after typing `:` in function definitions)
- [ ] Support `# beacon: disable` comments to skip formatting for specific regions
- [ ] Add performance monitoring to track formatting duration and emit warnings for slow operations (threshold-based)
- [ ] Implement configurable timeout for large files to prevent LSP hangs

### Configuration Integration

Added `FormatterConfig` to main LSP `Config` with full LSP/TOML support for all formatting options (line length, indent size, quote style, trailing commas, blank lines, import sorting, compatibility mode), hot-reload support via `did_change_configuration`, and comprehensive tests for JSON/TOML loading.

### Testing & Validation

- [ ] Unit tests for each formatting rule (whitespace, indentation, imports, etc.)
- [ ] Idempotency tests (format(format(x)) == format(x))
- [ ] AST preservation tests (parse(format(code)) == parse(code) modulo whitespace)
- [ ] Integration tests with LSP protocol
- [ ] Range formatting tests (partial document formatting)
- [ ] Performance benchmarks:
    - [ ] Small files (<100 lines): <10ms
    - [ ] Medium files (100-1000 lines): <100ms
    - [ ] Large files (1000-5000 lines): <500ms
- [ ] Regression test suite with real-world Python files
- [ ] Test compatibility with Black/autopep8 outputs
- [ ] Error handling tests (malformed code, incomplete parsing)

### Optimization

- [ ] Incremental formatting (cache unchanged regions)
- [ ] Parallel formatting for multiple files
- [ ] Lazy token stream evaluation
- [ ] Format buffer pooling (reduce allocations)
- [ ] Short-circuit for already-formatted code detection

### Static Analysis & Linting

**Files:** `crates/server/src/analysis/linter.rs`, `crates/server/src/analysis/rules/mod.rs`, `crates/server/src/features/diagnostics.rs`

See [Linter Rules](#linter-rules) section below for BEA code implementation status.

### Infrastructure

- [ ] Suppression support (`# type: ignore`, `# noqa:`)
- [ ] Rule configuration (per-rule enable/disable, severity)
- [ ] Symbol table integration for unused detection

### Incremental Re-analysis

- [ ] Benchmark incremental performance (target: <100ms)

### Cross-File Diagnostics

**Files:** `crates/server/src/workspace.rs`, `crates/server/src/features/diagnostics.rs`

- [ ] Inconsistent symbol exports (`__all__` mismatches)
- [ ] Conflicting stub definitions

## Linter Rules

Implementation status for BEA diagnostic codes. See `docs/src/lsp/lint_rules.md` for full rule documentation.

### Needs Symbol Table Integration

- [ ] BEA022: UnusedIndirectAssignment (blocked: requires Global/Nonlocal AST nodes)

### Needs Expression Evaluation

- [ ] BEA023: ForwardAnnotationSyntaxError - Needs Python expression parser for annotation validation
- [ ] BEA024: MultiValueRepeatedKeyLiteral - Requires dict key evaluation
- [ ] BEA029: RedundantPass - Requires separate pass

### Other

- [ ] BEA004: YieldOutsideFunction - Track yield/yield-from in visit_node
- [ ] BEA014: TStringMissingPlaceholders - Template string support
- [ ] BEA025: PercentFormatInvalidFormat - Validate % format syntax

### Improve Caching Granularity

Current limitations from `analysis/mod.rs`:

- Scope cache implementation is basic (extracts all nodes, no real filtering)
- No proper node-to-scope mapping
- No dependency tracking between scopes
- Cache invalidation is wholesale per document

Tasks:

- [ ] Implement proper node-to-scope mapping during constraint generation
- [ ] Track inter-scope dependencies (e.g., function calls, imports)
- [ ] Selective invalidation: only reanalyze changed scopes and dependents
- [ ] Benchmark incremental analysis with large files (target: <50ms for single-scope change)

## Logging

### Local Dev

#### Core

- [x] Use `tracing`, `tracing-subscriber`, and `tracing-appender` for structured, rotating logs.
    - Configure daily rotation (`rolling::daily("logs", "lsp.log")`) and JSON or text output (`fmt::layer().json()`).
    - Include timestamps, log levels, and targets for clarity.
- [x] Initialize the logger with `RUST_LOG=trace` for full verbosity.
- [x] Write logs to both `stderr` (for immediate visibility) and to a rotating file (`logs/lsp.log`).
- [x] Enable JSON-RPC tracing via client setting (`"trace.server": "verbose"` in VS Code).
- [x] Implement `panic::set_hook` to capture unhandled panics and write them to the log file.
- [x] Document developer usage (`RUST_LOG=trace cargo run`, or `lsp logs --follow` for live view) in `CONTRIBUTING.md`.

#### CLI Command

- [x] Watches the active log file (`logs/lsp.log` or a path from `$LSP_LOG_PATH`).
- [x] Continuously prints appended lines to stdout with optional filtering by level or module.
- [x] Supports colorized log levels (`INFO`, `WARN`, `ERROR`) and timestamps.

```bash
$ beacon debug logs --[f]ollow
2025-11-08T12:15:42Z [INFO] Server initialized (v0.1.0)
2025-11-08T12:15:43Z [DEBUG] Analyzing file: /workspace/main.py
2025-11-08T12:15:44Z [TRACE] Inferred type: int -> str
```

#### Observability

- [x] Add detailed `debug!` logs for parsing, AST construction, type inference, and symbol resolution.
- [x] Add `info!` logs for file analysis start/finish, workspace detection, and initialization success.
- [x] Log all protocol events (`textDocument/*`, `workspace/*`, etc.) using `debug!` and `trace!`.
- [ ] Detailed symbol resolution tracing (partial coverage)
- [ ] Per-module diagnostic generation
- [ ] Stub cache operations and introspection queries

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
- [ ] Continue using `window/logMessage` for high-level, safe summaries (“Server initialized”, “Analysis failed”).
- [ ] Optionally maintain sanitized rotating logs (`tracing-appender`), capped by size.
- [ ] Add `--version` or `status` CLI command to print build metadata (version, commit hash, platform).
- [ ] Provide a runtime flag or env var (`LSP_LOG_LEVEL`) for fine-grained control in deployments.
