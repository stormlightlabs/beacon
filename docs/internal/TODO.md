# Beacon LSP Implementation Plan

Implementation details and mod-specific tasks.

All integration tests (parser, config, analyzer, HM, linter, completion, formatting, LSP) are tracked in [e2e_test](./e2e_tests.md)

## LSP Features

**Files:** `crates/server/src/features/diagnostics.rs`, `crates/server/src/features/code_actions.rs`

### Quick Fixes

- [ ] Move pattern before subsuming pattern (PM002)

### Configuration

- Implements full LSP configuration support, including interpreter path, config file parsing, workspace settings, hot-reload, and diagnostic and inlay-hint customization—with one remaining quick-fix rule pending.

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

See `docs/internal/e2e_tests.md` for comprehensive snippet integration test tasks.

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

- Core formatter infrastructure implemented (module structure, config, token stream, context tracking, and state machine)

### Whitespace, Indentation & Line-Length

- Implements comprehensive formatting rules for whitespace, indentation, and line wrapping with Unicode-aware width calculations, smart breakpoints, and detailed documentation for all formatting behaviors.

### Import Formatting

Implemented PEP8-compliant import sorting with automatic grouping (stdlib/third-party/local), alphabetical ordering, multi-line wrapping, and deduplication.

### String & Comment Formatting

Implemented intelligent string quote normalization with escape-avoidance, docstring formatting, and comment spacing while preserving special directives (type: ignore, noqa, etc.).

### Structural Formatting

Implemented trailing comma handling, decorator spacing, type annotation rules, lambda wrapping, dictionary value indentation, comprehension wrapping strategies, and context-aware blank line management for classes and functions.

### LSP Integration

Implemented `textDocument/formatting` and `textDocument/rangeFormatting` handlers with proper LSP capabilities registration, error recovery, and configuration integration through the workspace config.

- [x] Implement `textDocument/willSaveWaitUntil` for format-on-save
- [x] Implement `textDocument/onTypeFormatting` for automatic formatting as user types (e.g., after typing `:` in function definitions)
- [ ] Support `# beacon: disable` comments to skip formatting for specific regions
- [ ] Add performance monitoring to track formatting duration and emit warnings for slow operations (threshold-based)
- [ ] Implement configurable timeout for large files to prevent LSP hangs

Added `FormatterConfig` to main LSP `Config` with full LSP/TOML support for all formatting options (line length, indent size, quote style, trailing commas, blank lines, import sorting, compatibility mode), hot-reload support via `did_change_configuration`, and comprehensive tests for JSON/TOML loading.

### Testing & Validation

See `docs/internal/e2e_tests.md` for formatter integration test tasks including performance benchmarks, compatibility testing, and error handling.

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
    - [ ] Create `annotation_parser.rs` module or extend const_eval
    - [ ] Parse string annotations as Python expressions using tree-sitter
    - [ ] Validate bracket matching, identifier syntax, and type expression structure
    - [ ] Handle generic types: `List[int]`, `Dict[str, Any]`, `Optional[T]`
    - [ ] Test malformed annotations: `"List[int"`, `"Dict[str,,"`, `"123InvalidName"`
    - [ ] Implement BEA023 diagnostic emission with syntax error details

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

## Logging

### Local Dev

#### Core

Sets up a structured, rotating logging system using `tracing` with full verbosity, panic capture, JSON-RPC tracing, and developer tooling for live log viewing and documentation in `CONTRIBUTING.md`.

#### CLI Command

Implements a live log-watching CLI that streams and colorizes log output from the active file with optional filtering by level or module.

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
- [ ] Continue using `window/logMessage` for high-level, safe summaries ("Server initialized", "Analysis failed")
- [ ] Provide a runtime flag or env var (`LSP_LOG_LEVEL`) for fine-grained control in deployments.
