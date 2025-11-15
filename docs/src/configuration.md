# Configuration

Beacon LSP can be configured through TOML files for standalone usage or through your editor's settings when using an extension.

## Configuration Files

Beacon searches for configuration in the following order:

1. `beacon.toml` in your workspace root
2. `[tool.beacon]` section in `pyproject.toml`

If multiple configuration files are found, `beacon.toml` takes precedence.

## TOML Structure

Beacon configuration uses TOML sections to organize related settings:

```toml
[type_checking]
mode = "balanced"

[python]
version = "3.12"
stub_paths = ["stubs", "typings"]

[workspace]
source_roots = ["src", "lib"]
exclude_patterns = ["**/venv/**"]

[inlay_hints]
enable = true
variable_types = true

[diagnostics]
unresolved_imports = "warning"
circular_imports = "warning"

[formatting]
enabled = true
line_length = 88
quote_style = "double"
trailing_commas = "multiline"

[advanced]
incremental = true
cache_size = 100
```

## Configuration Options

### Type Checking

Configure type checking behavior under the `[type_checking]` section.

#### `mode`

Type checking strictness mode. Controls how the type checker handles annotation mismatches and inference.

- **Type:** `string`
- **Default:** `"balanced"`
- **Values:**
    - `"strict"`: Annotation mismatches are hard errors with strict enforcement
    - `"balanced"`: Annotation mismatches are diagnostics with quick fixes, but inference proceeds
    - `"loose"`: Annotations supply bounds but can be overridden by inference

```toml
[type_checking]
mode = "balanced"
```

### Python

Configure Python-specific settings under the `[python]` section.

#### `version`

Target Python version for feature support (e.g., pattern matching in 3.10+, PEP 695 syntax in 3.12+).

- **Type:** `string`
- **Default:** `"3.12"`
- **Values:** `"3.9"`, `"3.10"`, `"3.11"`, `"3.12"`, `"3.13"`

```toml
[python]
version = "3.12"
```

#### `stub_paths`

Additional paths to search for .pyi stub files.

- **Type:** `array of strings`
- **Default:** `["stubs"]`

```toml
[python]
stub_paths = ["stubs", "typings", "~/.local/share/python-stubs"]
```

### Workspace

Configure workspace settings under the `[workspace]` section.

#### `source_roots`

Source roots for module resolution in addition to workspace root.

- **Type:** `array of strings`
- **Default:** `[]`

```toml
[workspace]
source_roots = ["src", "lib"]
```

#### `exclude_patterns`

Glob patterns to exclude from workspace scanning.

- **Type:** `array of strings`
- **Default:** `[]`

```toml
[workspace]
exclude_patterns = ["**/venv/**", "**/.venv/**", "**/node_modules/**"]
```

### Inlay Hints

#### `inlay_hints.enable`

Master toggle for all inlay hints.

- **Type:** `boolean`
- **Default:** `true`

```toml
[inlay_hints]
enable = true
```

#### `inlay_hints.variable_types`

Show inlay hints for inferred variable types on assignments without explicit type annotations.

- **Type:** `boolean`
- **Default:** `true`

```toml
[inlay_hints]
variable_types = true
```

#### `inlay_hints.function_return_types`

Show inlay hints for inferred function return types on functions without explicit return type annotations.

- **Type:** `boolean`
- **Default:** `true`

```toml
[inlay_hints]
function_return_types = true
```

#### `inlay_hints.parameter_names`

Show inlay hints for parameter names in function calls to improve readability.

- **Type:** `boolean`
- **Default:** `false`

```toml
[inlay_hints]
parameter_names = false
```

### Diagnostics

Configure diagnostic severity levels under the `[diagnostics]` section.

#### `unresolved_imports`

Diagnostic severity level for imports that cannot be resolved.

- **Type:** `string`
- **Default:** `"warning"`
- **Values:** `"error"`, `"warning"`, `"info"`

```toml
[diagnostics]
unresolved_imports = "warning"
```

#### `circular_imports`

Diagnostic severity level for circular import dependencies.

- **Type:** `string`
- **Default:** `"warning"`
- **Values:** `"error"`, `"warning"`, `"info"`

```toml
[diagnostics]
circular_imports = "warning"
```

### Formatting

Configure code formatting behavior under the `[formatting]` section. Beacon provides PEP8-compliant formatting through the LSP.

#### `formatting.enabled`

Master toggle for code formatting.

- **Type:** `boolean`
- **Default:** `true`

```toml
[formatting]
enabled = true
```

#### `formatting.line_length`

Maximum line length before wrapping.

- **Type:** `integer`
- **Default:** `88` (Black-compatible)
- **Range:** `20-200`

```toml
[formatting]
line_length = 88
```

#### `formatting.indent_size`

Number of spaces per indentation level.

- **Type:** `integer`
- **Default:** `4`
- **Range:** `2-8`

```toml
[formatting]
indent_size = 4
```

#### `formatting.quote_style`

String quote style preference.

- **Type:** `string`
- **Default:** `"double"`
- **Values:**
    - `"single"`: Use single quotes for strings
    - `"double"`: Use double quotes for strings
    - `"preserve"`: Keep existing quote style

```toml
[formatting]
quote_style = "double"
```

#### `formatting.trailing_commas`

Trailing comma behavior in multi-line structures.

- **Type:** `string`
- **Default:** `"multiline"`
- **Values:**
    - `"always"`: Add trailing commas to all multi-line structures
    - `"multiline"`: Add trailing commas only to multi-line nested structures
    - `"never"`: Never add trailing commas

```toml
[formatting]
trailing_commas = "multiline"
```

#### `formatting.max_blank_lines`

Maximum consecutive blank lines allowed.

- **Type:** `integer`
- **Default:** `2`
- **Range:** `0-5`

```toml
[formatting]
max_blank_lines = 2
```

#### `formatting.import_sorting`

Import statement sorting style.

- **Type:** `string`
- **Default:** `"pep8"`
- **Values:**
    - `"pep8"`: stdlib, third-party, local
    - `"isort"`: isort-compatible sorting
    - `"off"`: Disable import sorting

```toml
[formatting]
import_sorting = "pep8"
```

#### `formatting.compatibility_mode`

Compatibility with other Python formatters.

- **Type:** `string`
- **Default:** `"black"`
- **Values:**
    - `"black"`: Black formatter compatibility (88 char line length)
    - `"autopep8"`: autopep8 compatibility (79 char line length)
    - `"pep8"`: Strict PEP8 (79 char line length)

```toml
[formatting]
compatibility_mode = "black"
```

#### `formatting.use_tabs`

Use tabs instead of spaces for indentation (not recommended).

- **Type:** `boolean`
- **Default:** `false`

```toml
[formatting]
use_tabs = false
```

#### `formatting.normalize_docstring_quotes`

Normalize quotes in docstrings to match quote_style.

- **Type:** `boolean`
- **Default:** `true`

```toml
[formatting]
normalize_docstring_quotes = true
```

#### `formatting.spaces_around_operators`

Add spaces around binary operators.

- **Type:** `boolean`
- **Default:** `true`

```toml
[formatting]
spaces_around_operators = true
```

#### `formatting.blank_line_before_class`

Add blank lines before class definitions.

- **Type:** `boolean`
- **Default:** `true`

```toml
[formatting]
blank_line_before_class = true
```

#### `formatting.blank_line_before_function`

Add blank lines before function definitions.

- **Type:** `boolean`
- **Default:** `true`

```toml
[formatting]
blank_line_before_function = true
```

### Advanced Options

Configure advanced performance and analysis settings under the `[advanced]` section.

#### `max_any_depth`

Maximum depth for Any type propagation before elevating diagnostics. Higher values are more permissive.

- **Type:** `integer`
- **Default:** `3`
- **Range:** `0-10`

```toml
[advanced]
max_any_depth = 3
```

#### `incremental`

Enable incremental type checking for faster re-analysis.

- **Type:** `boolean`
- **Default:** `true`

```toml
[advanced]
incremental = true
```

#### `workspace_analysis`

Enable workspace-wide analysis and cross-file type checking.

- **Type:** `boolean`
- **Default:** `true`

```toml
[advanced]
workspace_analysis = true
```

#### `enable_caching`

Enable multi-layer caching of parse trees, type inference results, and formatting outputs. Caching dramatically improves performance for incremental edits and repeated operations.

Beacon uses four cache layers:

- **TypeCache**: Node-level type inference (capacity: 100)
- **ScopeCache**: Scope-level analysis with content hashing (capacity: 200)
- **AnalysisCache**: Document-level analysis artifacts (capacity: 50)
- **IntrospectionCache**: Persistent Python introspection (capacity: 1000)

When enabled, Beacon automatically invalidates stale cache entries when documents change. Scope-level content hashing ensures only modified scopes are re-analyzed.

- **Type:** `boolean`
- **Default:** `true`

```toml
[advanced]
enable_caching = true
```

For technical details on cache architecture and invalidation strategies, see [Caching](./lsp/caching.md).

#### `cache_size`

Maximum number of documents to cache in the document-level analysis cache. Higher values improve performance for large workspaces at the cost of memory usage.

- **Type:** `integer`
- **Default:** `100`
- **Range:** `0-1000`

```toml
[advanced]
cache_size = 100
```

## Example Configurations

### Basic Configuration (beacon.toml)

```toml
[type_checking]
mode = "strict"

[python]
version = "3.12"

[diagnostics]
unresolved_imports = "error"
circular_imports = "warning"
```

### Advanced Configuration (beacon.toml)

```toml
[type_checking]
mode = "balanced"

[python]
version = "3.13"
stub_paths = ["stubs", "typings"]

[workspace]
source_roots = ["src", "lib"]
exclude_patterns = ["**/venv/**", "**/.venv/**", "**/build/**"]

[inlay_hints]
enable = true
variable_types = true
function_return_types = true
parameter_names = false

[diagnostics]
unresolved_imports = "warning"
circular_imports = "info"

[formatting]
enabled = true
line_length = 100
indent_size = 4
quote_style = "double"
trailing_commas = "multiline"
import_sorting = "pep8"

[advanced]
max_any_depth = 5
incremental = true
workspace_analysis = true
enable_caching = true
cache_size = 200
```

### Using pyproject.toml

```toml
[tool.beacon.type_checking]
mode = "strict"

[tool.beacon.python]
version = "3.12"
stub_paths = ["stubs", "typings"]

[tool.beacon.workspace]
source_roots = ["src"]
exclude_patterns = ["**/venv/**", "**/.venv/**"]

[tool.beacon.diagnostics]
unresolved_imports = "error"

[tool.beacon.formatting]
enabled = true
line_length = 88
quote_style = "double"
trailing_commas = "multiline"
```

## Configuration Precedence

When using Beacon with an editor extension (e.g., VSCode), configuration is merged in the following order (later sources override earlier ones):

1. **Default values** - Built-in defaults
2. **TOML file** - `beacon.toml` or `pyproject.toml`
3. **Editor settings** - VSCode settings, Zed settings, Neovim config, etc.

This allows you to set project-wide defaults in TOML while still being able to override specific settings through your editor.
