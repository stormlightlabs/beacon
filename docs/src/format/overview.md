# Formatting Overview

Beacon provides built-in PEP8-compliant Python code formatting capabilities through its language server.
The formatter is designed to produce consistent, readable code while respecting configuration preferences.

## Design Principles

The formatter follows these core principles:

**PEP8 Compliance**: Adheres to Python Enhancement Proposal 8 style guidelines by default, with configurable options for compatibility with Black and autopep8.

**AST-Based**: Operates on the abstract syntax tree rather than raw text, ensuring formatting preserves semantic meaning and handles edge cases correctly.

**Configurable**: Supports workspace and project-level configuration through beacon.toml or pyproject.toml files.

**Incremental**: Designed to format code efficiently, with support for both full-file and range formatting.

## Formatting Pipeline

The formatter operates in four stages:

1. **Parsing**: Source code is parsed into an AST using the Beacon parser
2. **Token Generation**: AST nodes are converted into a stream of formatting tokens
3. **Rule Application**: Formatting rules are applied based on context and configuration
4. **Output Generation**: Formatted code is written with proper whitespace and indentation

## Key Features

### Whitespace and Indentation

- Normalizes indentation to 4 spaces (configurable)
- Removes trailing whitespace
- Manages blank lines between definitions and statements
- Controls whitespace around operators, commas, and colons

See [Whitespace](./whitespace.md) for detailed formatting rules.

### Line Length Management

- Enforces maximum line length (default: 88 characters, matching Black)
- Smart line breaking at appropriate boundaries
- Handles multi-byte Unicode characters correctly
- Preserves user line breaks when under the limit

See [Print Width](./print-width.md) for line length handling.

### Structural Formatting

- Function call and definition parameter wrapping
- Collection literal formatting (lists, dicts, sets, tuples)
- Binary expression breaking
- Import statement organization and sorting

See [Structure](./structure.md) and [Imports](./imports.md) for structural rules.

### Suppression Comments

The formatter respects suppression directives:

- `# fmt: skip` - Skip formatting for a single line
- `# fmt: off` / `# fmt: on` - Disable formatting for regions

See [Suppressions](./suppressions.md) for complete documentation on formatter, linter, and type checker suppressions.

## Configuration

Formatting behavior is controlled through settings:

```toml
[formatting]
enabled = true
lineLength = 88
indentSize = 4
quoteStyle = "double"
trailingCommas = "multiline"
maxBlankLines = 2
importSorting = "pep8"
compatibilityMode = "black"
```

See the [Configuration](../configuration.md) documentation for complete details.

## LSP Integration

The formatter integrates with the Language Server Protocol through:

- `textDocument/formatting`: Format entire document
- `textDocument/rangeFormatting`: Format selected range
- `textDocument/willSaveWaitUntil`: Format on save

## Compatibility

The formatter provides compatibility modes for popular formatters:

- **Black**: 88-character line length, minimal configuration
- **autopep8**: 79-character line length, conservative formatting
- **PEP8**: Strict adherence to style guide recommendations
