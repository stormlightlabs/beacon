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
- [ ] Formatting options

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
