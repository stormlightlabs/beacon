# LSP Formatting Integration

Beacon LSP provides PEP8-compliant code formatting through the Language Server Protocol.

## Overview

The formatting feature provider implements two LSP capabilities:

- `textDocument/formatting`: Format an entire document
- `textDocument/rangeFormatting`: Format a selected range within a document

## Architecture

### FormattingProvider

Located in `crates/server/src/features/formatting.rs`, the `FormattingProvider` handles all formatting requests.

```rust
pub struct FormattingProvider {
    documents: DocumentManager,
}
```

The provider:

1. Retrieves document content from the `DocumentManager`
2. Creates a `Formatter` with the current configuration
3. Applies formatting rules to the content
4. Returns LSP `TextEdit` objects describing the changes

### Integration with Backend

The `Backend` struct registers formatting capabilities during initialization:

```rust
document_formatting_provider: Some(OneOf::Left(true)),
document_range_formatting_provider: Some(OneOf::Left(true)),
```

Formatting requests are routed through the backend's handler methods:

```rust
async fn formatting(&self, params: DocumentFormattingParams) -> Result<Option<Vec<TextEdit>>>
async fn range_formatting(&self, params: DocumentRangeFormattingParams) -> Result<Option<Vec<TextEdit>>>
```

## textDocument/formatting

Full document formatting request.

### Request

```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "method": "textDocument/formatting",
  "params": {
    "textDocument": {
      "uri": "file:///path/to/file.py"
    },
    "options": {
      "tabSize": 4,
      "insertSpaces": true
    }
  }
}
```

### Response

Returns a list of `TextEdit` objects or `null` if already formatted:

```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "result": [
    {
      "range": {
        "start": {"line": 0, "character": 0},
        "end": {"line": 100, "character": 0}
      },
      "newText": "formatted code here..."
    }
  ]
}
```

### Implementation Details

1. **Configuration Access**: Reads `workspace.config.formatting`
2. **Content Retrieval**: Gets full document text via `DocumentManager`
3. **Formatting**: Calls `formatter.format_range()` with entire document range
4. **Already Formatted Check**: Returns `None` if formatted equals original
5. **Error Recovery**: Returns `None` on formatting errors (logged)

## textDocument/rangeFormatting

Format only a selected range within a document.

### Request

```json
{
  "jsonrpc": "2.0",
  "id": 2,
  "method": "textDocument/rangeFormatting",
  "params": {
    "textDocument": {
      "uri": "file:///path/to/file.py"
    },
    "range": {
      "start": {"line": 10, "character": 0},
      "end": {"line": 20, "character": 0}
    },
    "options": {
      "tabSize": 4,
      "insertSpaces": true
    }
  }
}
```

### Current Behavior

The current implementation formats the entire document. Proper range formatting requires more sophisticated AST node selection to ensure syntactic validity when formatting partial code.

## Configuration Integration

Formatting behavior is controlled by the `formatting` section of the LSP configuration.

### Reading Configuration

```rust
let workspace = self.workspace.read().await;
let config = &workspace.config.formatting;
```

Configuration is passed to the `Formatter`:

```rust
let formatter = Formatter::new(config.clone(), parser);
```

### Hot Reload

Configuration changes trigger `did_change_configuration`, which updates the workspace config. The next formatting request will use the new settings.

## Error Handling

The formatter gracefully handles errors:

1. **Document Not Found**: Returns `None` (no-op)
2. **Parse Errors**: Logs error and returns `None` (preserves original)
3. **Formatting Failures**: Logs error and returns `None` (safe fallback)

All errors are logged with tracing:

```rust
tracing::error!(?uri, ?e, "Failed to format document");
```

## Performance Considerations

### Already-Formatted Detection

Before returning edits, the provider checks if formatting changed the content:

```rust
if formatted == content {
    tracing::debug!(?uri, "Document already formatted");
    return None;
}
```

This avoids unnecessary TextEdit applications in the client.

## Testing

The formatting provider includes comprehensive tests:

### Unit Tests

```rust
#[test]
fn test_format_document_nonexistent()      // Missing document handling
fn test_format_range_nonexistent()         // Missing document handling
fn test_format_document_already_formatted() // Already-formatted detection
```

### Integration Tests

Backend tests verify capability advertisement:

```rust
#[tokio::test]
async fn test_formatting_capability_advertised()
```

## Client Configuration

LSP clients can configure formatting in their settings:

### VS Code

```json
{
  "beacon.formatting.enabled": true,
  "beacon.formatting.lineLength": 88,
  "beacon.formatting.quoteStyle": "double"
}
```

### Neovim

```lua
require('lspconfig').beacon.setup({
  settings = {
    beacon = {
      formatting = {
        enabled = true,
        lineLength = 88,
        quoteStyle = "double"
      }
    }
  }
})
```

## Logging

Formatting operations emit debug and error logs:

```text
DEBUG Formatting document uri=file:///test.py
DEBUG Document already formatted uri=file:///test.py
ERROR Failed to format document uri=file:///test.py error="parse error"
```

Set `RUST_LOG=beacon_lsp::features::formatting=debug` for detailed formatting logs.
