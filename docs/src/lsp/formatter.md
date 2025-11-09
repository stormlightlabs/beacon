# LSP Formatting Integration

Beacon LSP provides PEP8-compliant code formatting through the Language Server Protocol.

## Overview

The formatting feature provider implements four LSP capabilities:

- `textDocument/formatting`: Format an entire document
- `textDocument/rangeFormatting`: Format a selected range within a document
- `textDocument/onTypeFormatting`: Automatically format code as the user types
- `textDocument/willSaveWaitUntil`: Format document before saving

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
document_on_type_formatting_provider: Some(DocumentOnTypeFormattingOptions {
    first_trigger_character: ":".to_string(),
    more_trigger_character: Some(vec!["\n".to_string()]),
}),
```

Formatting requests are routed through the backend's handler methods:

```rust
async fn formatting(&self, params: DocumentFormattingParams) -> Result<Option<Vec<TextEdit>>>
async fn range_formatting(&self, params: DocumentRangeFormattingParams) -> Result<Option<Vec<TextEdit>>>
async fn on_type_formatting(&self, params: DocumentOnTypeFormattingParams) -> Result<Option<Vec<TextEdit>>>
async fn will_save_wait_until(&self, params: WillSaveTextDocumentParams) -> Result<Option<Vec<TextEdit>>>
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

The implementation filters AST nodes to include only those within the specified range, ensuring syntactic validity when formatting partial code.

## textDocument/onTypeFormatting

Automatically formats code as the user types, triggered by specific characters.

### Trigger Characters

- `:` - After typing a colon in function definitions, class definitions, or control flow statements
- `\n` - After pressing Enter (newline)

### Request

```json
{
  "jsonrpc": "2.0",
  "id": 3,
  "method": "textDocument/onTypeFormatting",
  "params": {
    "textDocument": {
      "uri": "file:///path/to/file.py"
    },
    "position": {"line": 5, "character": 10},
    "ch": ":",
    "options": {
      "tabSize": 4,
      "insertSpaces": true
    }
  }
}
```

### Response

Returns a list of `TextEdit` objects or `null` if no formatting needed:

```json
{
  "jsonrpc": "2.0",
  "id": 3,
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

1. **Context-Aware Formatting**: Formats the current line and surrounding context (1 line before, 1 line after)
2. **Trigger Character Detection**: Checks which character triggered the formatting
3. **Smart Range Selection**: Uses `format_range()` to format only the relevant section
4. **Already-Formatted Check**: Returns `None` if no changes needed

### Behavior

When a user types `:` after a function definition:

```python
def foo():    # <- After typing ':', formats this line
```

The formatter applies proper spacing and indentation rules to the affected area.

## textDocument/willSaveWaitUntil

Formats the entire document before saving, allowing the server to provide edits that will be applied before the save completes.

### Request

```json
{
  "jsonrpc": "2.0",
  "id": 4,
  "method": "textDocument/willSaveWaitUntil",
  "params": {
    "textDocument": {
      "uri": "file:///path/to/file.py"
    },
    "reason": 1
  }
}
```

### Save Reasons

- `1` - Manual save (Ctrl+S / Cmd+S)
- `2` - AfterDelay (auto-save)
- `3` - FocusOut (save when focus lost)

### Response

Returns a list of `TextEdit` objects to apply before saving:

```json
{
  "jsonrpc": "2.0",
  "id": 4,
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

1. **Save Reason Filtering**: Only formats on manual saves (`TextSaveReason::MANUAL`)
2. **Skip Auto-Save**: Returns `None` for auto-save and focus-out saves to avoid interrupting workflow
3. **Full Document Format**: Calls `format_range()` with the entire document range
4. **Synchronous Application**: Edits are applied before the save completes

### Configuration

To enable format-on-save in your LSP client:

#### VS Code

```json
{
  "editor.formatOnSave": true,
  "[python]": {
    "editor.defaultFormatter": "beacon-lsp"
  }
}
```

#### Neovim

```lua
vim.api.nvim_create_autocmd("BufWritePre", {
  pattern = "*.py",
  callback = function()
    vim.lsp.buf.format({ async = false })
  end,
})
```

### Logging

Format-on-save operations emit specific logs:

```text
DEBUG Format-on-save triggered uri=file:///test.py
DEBUG Skipping format-on-save (not manual save) uri=file:///test.py reason=AfterDelay
DEBUG Format-on-save completed successfully uri=file:///test.py
```

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

The formatting provider includes comprehensive tests in `crates/server/tests/formatting_tests.rs`:

### Unit Tests

Formatting rule tests verify correct behavior:

- `test_whitespace_normalization` - Operator and assignment spacing
- `test_indentation_normalization` - Consistent indentation levels
- `test_trailing_whitespace_removal` - Removes trailing spaces
- `test_blank_line_management` - Proper spacing between functions/classes
- `test_operator_spacing` - Spaces around binary operators
- `test_delimiter_spacing` - Spaces after commas
- `test_line_length_wrapping` - Line wrapping at configured length
- `test_import_sorting_stdlib` - Alphabetical import ordering
- `test_import_grouping` - Stdlib/third-party/local grouping
- `test_from_import_multiline` - Multi-line import formatting

### Idempotency Tests

Verify `format(format(x)) == format(x)`:

- `test_idempotency_simple_function` - Simple function definitions
- `test_idempotency_class_definition` - Class definitions with methods
- `test_idempotency_complex_expressions` - Complex arithmetic expressions
- `test_idempotency_imports` - Import statements
- `test_idempotency_nested_structures` - Nested if/else blocks

### AST Preservation Tests

Ensure formatting doesn't change program semantics:

- `test_ast_preservation_simple` - Simple assignments
- `test_ast_preservation_function` - Function definitions
- `test_ast_preservation_class` - Class definitions
- `test_ast_preservation_expressions` - Binary operations

### Range Formatting Tests

Verify partial document formatting:

- `test_range_formatting_single_function` - Format one function in a file
- `test_range_formatting_partial_class` - Format specific class method
- `test_range_formatting_nested_blocks` - Format nested control structures
- `test_range_formatting_preserves_outside_range` - Code outside range unchanged

### LSP Feature Tests

Integration tests for new LSP features:

- `test_format_on_save` - `willSaveWaitUntil` handler
- `test_on_type_formatting` - `onTypeFormatting` handler

### Running Tests

```bash
cargo test -p beacon-lsp --test formatting_tests
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
