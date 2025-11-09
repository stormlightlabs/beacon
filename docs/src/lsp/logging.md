# Logging and Observability

Beacon uses structured logging via the `tracing` ecosystem to provide comprehensive observability for both development and production environments.

## Architecture

The logging infrastructure is built on three components:

1. **Core Logging Module** (`beacon-core::logging`) - Centralized configuration and initialization
2. **LSP Server Instrumentation** - Protocol events, file analysis, and type inference logging
3. **CLI Logs Command** - Real-time log viewing and filtering

## Local Development

### Running with Logs

Start the LSP server with full tracing enabled:

```sh
RUST_LOG=trace cargo run --bin beacon-lsp
```

Logs are written to two destinations:

- **File**: `logs/lsp.log` (daily rotating, persistent)
- **stderr**: Immediate console output during development

### Log Levels

Set the log level using the `RUST_LOG` environment variable:

```sh
# Only errors and panics
RUST_LOG=error cargo run --bin beacon-lsp

# Warnings and errors
RUST_LOG=warn cargo run --bin beacon-lsp

# High-level events (default for releases)
RUST_LOG=info cargo run --bin beacon-lsp

# Detailed operation logs (recommended for development)
RUST_LOG=debug cargo run --bin beacon-lsp

# Full verbosity including protocol messages
RUST_LOG=trace cargo run --bin beacon-lsp
```

### Module-Specific Filtering

Target specific modules for detailed logging:

```sh
# Debug level for LSP, trace for analysis
RUST_LOG=beacon_lsp=debug,beacon_lsp::analysis=trace cargo run --bin beacon-lsp

# Trace constraint generation only
RUST_LOG=beacon_constraint=trace cargo run --bin beacon-lsp
```

## Watching Logs

### CLI Logs Command

View logs in real-time using the debug logs command:

```sh
# Show all current logs
beacon debug logs

# Follow mode - continuously watch for new entries
beacon debug logs --follow

# Filter by pattern (regex supported)
beacon debug logs --follow --filter "ERROR|WARN"

# Filter to specific module
beacon debug logs --follow --filter "analysis"

# Use custom log file
beacon debug logs --follow --path /custom/path/to/log.txt
```

### Log Output Format

Logs are colorized by level for easy scanning:

- **ERROR** - Bright red
- **WARN** - Yellow
- **INFO** - White
- **DEBUG** - Cyan
- **TRACE** - Dimmed

Example output:

```log
2025-11-08T12:15:42Z [INFO] beacon_lsp: Starting Beacon LSP server version="0.1.0"
2025-11-08T12:15:43Z [DEBUG] beacon_lsp::backend: Received initialize request root_uri=Some(file:///workspace)
2025-11-08T12:15:44Z [INFO] beacon_lsp::analysis: Starting analysis uri="file:///workspace/main.py"
2025-11-08T12:15:44Z [DEBUG] beacon_lsp::analysis: Generating constraints uri="file:///workspace/main.py"
2025-11-08T12:15:45Z [INFO] beacon_lsp::analysis: Analysis completed uri="file:///workspace/main.py" duration_ms=142 type_count=87 error_count=0
```

## What Gets Logged

### Protocol Events

All LSP protocol requests and notifications are logged at appropriate levels:

- `initialize`, `shutdown` - INFO
- `textDocument/didOpen`, `didChange`, `didClose` - INFO
- `workspace/didChangeConfiguration` - INFO
- Diagnostics publishing - DEBUG
- Feature requests (hover, completion, etc.) - TRACE

### File Analysis

The analysis pipeline logs key stages:

1. **Analysis Start** (INFO) - URI, timestamp
2. **Document Retrieval** (DEBUG) - Version, source length, scope count
3. **Cache Hit/Miss** (DEBUG) - Whether cached results are used
4. **Constraint Generation** (DEBUG) - Number of constraints generated
5. **Solver Execution** (DEBUG) - Constraint count, solver invocation
6. **Solver Completion** (INFO) - Type error count
7. **Analysis Completion** (INFO) - Duration, type count, error count

Example analysis sequence:

```log
INFO  Starting analysis uri="file:///app/main.py"
DEBUG Retrieved document data uri="file:///app/main.py" version=3 source_length=1247 scopes=8
DEBUG Generating constraints uri="file:///app/main.py"
DEBUG Constraints generated, starting solver uri="file:///app/main.py" constraint_count=142
INFO  Constraint solving completed uri="file:///app/main.py" type_error_count=0
INFO  Analysis completed uri="file:///app/main.py" version=3 duration_ms=89 type_count=142 error_count=0
```

### Type Inference

Constraint generation and solving steps are logged:

- Constraint count per file
- Solver initialization
- Type errors encountered
- Substitution application

### Workspace Operations

Multi-file workspace operations:

- Workspace indexing start/completion
- Dependency updates
- Module invalidation
- Re-analysis of affected modules

### Configuration Changes

Configuration loading and hot-reload:

- Config file discovery
- Validation warnings
- Runtime updates
- Mode changes (strict/balanced/loose)

## Error Handling

### Panic Logging

Panics are automatically captured and logged before termination:

```rust
PANIC at src/analysis/mod.rs:245:12: Type variable unification failed unexpectedly
```

The panic hook logs:

- Panic message
- File location (file:line:column)
- Payload details

### Error Propagation

Errors are logged at appropriate points with context:

```log
ERROR Failed to open document uri="file:///invalid.py" error="File not found"
ERROR Failed to update document uri="file:///app.py" error="Invalid UTF-8 in document"
```

## Environment Variables

### `RUST_LOG`

Controls log level filtering using the `env_filter` syntax:

```sh
# Global level
RUST_LOG=debug

# Per-module levels
RUST_LOG=info,beacon_lsp::analysis=trace

# Multiple modules
RUST_LOG=beacon_lsp=debug,beacon_constraint=trace,tower_lsp=warn
```

### `LSP_LOG_PATH`

Override the default log file location:

```sh
LSP_LOG_PATH=/var/log/beacon/custom.log cargo run --bin beacon-lsp
```

Default: `logs/lsp.log`

## Output Formats

### Text (Default)

Human-readable format with timestamps, levels, and targets:

```log
2025-11-08T12:15:42.123Z INFO beacon_lsp::backend: Server initialized version="0.1.0"
```

### JSON

Structured JSON output for machine parsing (configurable via `LogFormat::Json`):

```json
{
  "timestamp": "2025-11-08T12:15:42.123Z",
  "level": "INFO",
  "target": "beacon_lsp::backend",
  "message": "Server initialized",
  "version": "0.1.0"
}
```

## Release

In production, logging defaults to WARNING level:

- Minimal performance overhead
- Only errors and warnings logged
- File rotation prevents unbounded growth
- Daily rotation with date-stamped files

### Log Rotation

Logs automatically rotate daily:

- Current log: `logs/lsp.log`
- Rotated logs: `logs/lsp.log.2025-11-07`, `logs/lsp.log.2025-11-06`, etc.

## Coverage Gaps

The following areas have limited or no logging coverage (documented in roadmap):

- Detailed symbol resolution tracing
- Per-module diagnostic generation
- Configuration hot-reload events (currently limited)
- WebSocket/TCP transport logging (when using `--tcp`)
- Stub cache operations and introspection queries
