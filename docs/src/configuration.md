# Configuration

Beacon LSP can be configured through TOML files for standalone usage or through your editor's settings when using an extension.

## Configuration Files

Beacon searches for configuration in the following order:

1. `beacon.toml` in your workspace root
2. `[tool.beacon]` section in `pyproject.toml`

If multiple configuration files are found, `beacon.toml` takes precedence.

## Configuration Options

### Type Checking

#### `mode`

Type checking strictness mode. Controls how the type checker handles annotation mismatches and inference.

- **Type:** `string`
- **Default:** `"balanced"`
- **Values:**
    - `"strict"`: Annotation mismatches are hard errors with strict enforcement
    - `"balanced"`: Annotation mismatches are diagnostics with quick fixes, but inference proceeds
    - `"loose"`: Annotations supply bounds but can be overridden by inference

```toml
mode = "balanced"
```

#### `python_version`

Target Python version for feature support (e.g., pattern matching in 3.10+, PEP 695 syntax in 3.12+).

- **Type:** `string`
- **Default:** `"3.12"`
- **Values:** `"3.9"`, `"3.10"`, `"3.11"`, `"3.12"`, `"3.13"`

```toml
python_version = "3.12"
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

#### `unresolved_import_severity`

Diagnostic severity level for imports that cannot be resolved.

- **Type:** `string`
- **Default:** `"warning"`
- **Values:** `"error"`, `"warning"`, `"info"`

```toml
unresolved_import_severity = "warning"
```

#### `circular_import_severity`

Diagnostic severity level for circular import dependencies.

- **Type:** `string`
- **Default:** `"warning"`
- **Values:** `"error"`, `"warning"`, `"info"`

```toml
circular_import_severity = "warning"
```

### Advanced Options

#### `max_any_depth`

Maximum depth for Any type propagation before elevating diagnostics. Higher values are more permissive.

- **Type:** `integer`
- **Default:** `3`
- **Range:** `0-10`

```toml
max_any_depth = 3
```

#### `incremental`

Enable incremental type checking for faster re-analysis.

- **Type:** `boolean`
- **Default:** `true`

```toml
incremental = true
```

#### `workspace_analysis`

Enable workspace-wide analysis and cross-file type checking.

- **Type:** `boolean`
- **Default:** `true`

```toml
workspace_analysis = true
```

#### `enable_caching`

Enable caching of parse trees and type inference results.

- **Type:** `boolean`
- **Default:** `true`

```toml
enable_caching = true
```

#### `cache_size`

Maximum number of documents to cache for faster analysis.

- **Type:** `integer`
- **Default:** `100`
- **Range:** `0-1000`

```toml
cache_size = 100
```

#### `stub_paths`

Additional paths to search for .pyi stub files.

- **Type:** `array of strings`
- **Default:** `["stubs"]`

```toml
stub_paths = ["stubs", "typings", "~/.local/share/python-stubs"]
```

#### `source_roots`

Source roots for module resolution in addition to workspace root.

- **Type:** `array of strings`
- **Default:** `[]`

```toml
source_roots = ["src", "lib"]
```

#### `exclude_patterns`

Glob patterns to exclude from workspace scanning.

- **Type:** `array of strings`
- **Default:** `[]`

```toml
exclude_patterns = ["**/venv/**", "**/.venv/**", "**/node_modules/**"]
```

## Example Configurations

### Basic Configuration (beacon.toml)

```toml
mode = "strict"
python_version = "3.12"
unresolved_import_severity = "error"
circular_import_severity = "warning"
```

### Advanced Configuration (beacon.toml)

```toml
# Type checking configuration
mode = "balanced"
python_version = "3.13"
max_any_depth = 5

# Inlay hints
[inlay_hints]
enable = true
variable_types = true
function_return_types = true
parameter_names = false

# Diagnostic severity
unresolved_import_severity = "warning"
circular_import_severity = "info"

# Performance settings
incremental = true
workspace_analysis = true
enable_caching = true
cache_size = 200

# Paths
stub_paths = ["stubs", "typings"]
source_roots = ["src", "lib"]
exclude_patterns = ["**/venv/**", "**/.venv/**", "**/build/**"]
```

### Using pyproject.toml

```toml
[tool.beacon]
mode = "strict"
python_version = "3.12"
unresolved_import_severity = "error"
stub_paths = ["stubs", "typings"]
source_roots = ["src"]
exclude_patterns = ["**/venv/**", "**/.venv/**"]
```

## Configuration Precedence

When using Beacon with an editor extension (e.g., VSCode), configuration is merged in the following order (later sources override earlier ones):

1. **Default values** - Built-in defaults
2. **TOML file** - `beacon.toml` or `pyproject.toml`
3. **Editor settings** - VSCode settings, Zed settings, Neovim config, etc.

This allows you to set project-wide defaults in TOML while still being able to override specific settings through your editor.
