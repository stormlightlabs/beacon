# Type Checking

The `typecheck` command performs Hindley-Milner type inference on Python code and reports type errors.

## Usage

```sh
beacon-cli typecheck [OPTIONS] [FILE]
```

## Options

- `-f, --format <FORMAT>` - Output format (human, json, compact) [default: human]
- `-w, --workspace` - Analyze all files in workspace (follow imports)

## Output Formats

### Human (Default)

Human-readable output with context and visual pointers:

```sh
$ beacon-cli typecheck example.py
Found 1 type error(s):

Error 1: Cannot unify types: Int ~ Str (line 3, col 5)
  --> example.py:3:5
   |
 3 | z = x + y
   |     ^
```

### JSON

Machine-readable JSON format for tooling integration:

```sh
$ beacon-cli typecheck --format json example.py
{
  "errors": [
    {
      "error": "Cannot unify types: Int ~ Str",
      "line": 3,
      "col": 5,
      "end_line": null,
      "end_col": null
    }
  ],
  "error_count": 1
}
```

### Compact

Single-line format compatible with editor quickfix lists:

```sh
$ beacon-cli typecheck --format compact example.py
example.py:3:5: Cannot unify types: Int ~ Str
```

## Examples

### Check a single file

```sh
beacon-cli typecheck src/main.py
```

### Check with JSON output for CI

```sh
beacon-cli typecheck --format json src/*.py > type-errors.json
```

### Check from stdin

```sh
cat src/main.py | beacon-cli typecheck
```

## Exit Codes

- `0` - No type errors found
- `1` - Type errors found or analysis failed

## Workspace Mode

The `--workspace` flag (currently TODO) will follow imports and analyze all Python files in the project:

```sh
beacon-cli typecheck --workspace src/main.py
```

This performs multi-file analysis, tracking types across module boundaries.
