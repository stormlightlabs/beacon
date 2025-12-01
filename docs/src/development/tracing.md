# Tracing & Observability

Beacon uses the [tracing](https://github.com/tokio-rs/tracing) ecosystem for structured logging and instrumentation.

## Log Levels

- **error**: Critical failures that prevent functionality
- **warn**: Recoverable issues requiring attention
- **info**: High-level lifecycle events and milestones
- **debug**: Detailed operational information
- **trace**: Fine-grained execution details

## Enabling Logs

Set `RUST_LOG` environment variable:

```bash
# All debug logs
RUST_LOG=debug beacon lsp

# Module-specific logging
RUST_LOG=beacon_lsp=debug,beacon_core=info beacon lsp

# Trace-level for specific module
RUST_LOG=beacon_lsp::analysis=trace beacon lsp
```

## Instrumentation Points

### LSP Lifecycle

Key events in `backend.rs`:

- Server initialization and configuration loading
- Document lifecycle (open, change, close)
- Shutdown requests

### Document Processing

In `document.rs`:

- Mode directive detection from file comments
- Parse and reparse operations

### Analysis Pipeline

Stages in `analysis/mod.rs`:

- Constraint generation
- Type inference execution
- Analysis result caching

### Diagnostic Generation

Each diagnostic phase in `features/diagnostics.rs`:

- Parse errors
- Linter diagnostics
- Type checking errors
- Unsafe Any warnings
- Import validation
- Variance checking

### Caching

Cache operations across `cache.rs`:

- Hit/miss tracking for type, introspection, and analysis caches
- Invalidation events
- Import dependency tracking

### Workspace Operations

In `workspace.rs`:

- File discovery and indexing
- Stub loading from typeshed and configured paths
- Dependency graph construction

## Handler Instrumentation

LSP request handlers use `#[tracing::instrument]` macro for automatic span creation:

```rust
#[tracing::instrument(skip(self), level = "debug")]
async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
    // Automatically logs entry/exit with debug level
}
```
