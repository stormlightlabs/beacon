# Type Checking

The `typecheck` command performs Hindley-Milner type inference on Python code and reports type errors.

## Usage

```sh
beacon typecheck [OPTIONS] [PATHS]...
```

Accepts:

- Single file: `beacon typecheck file.py`
- Multiple files: `beacon typecheck file1.py file2.py file3.py`
- Directory: `beacon typecheck src/` (recursively finds all .py files)
- Stdin: `beacon typecheck` (reads from stdin)

## Options

- `-f, --format <FORMAT>` - Output format (human, json, compact) [default: human]

## Output Formats

### Human (Default)

Human-readable output with context and visual pointers:

```sh
$ beacon typecheck example.py
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
$ beacon typecheck --format json example.py
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
$ beacon typecheck --format compact example.py
example.py:3:5: Cannot unify types: Int ~ Str
```

## Examples

### Check a single file

```sh
beacon typecheck src/main.py
```

### Check multiple files

```sh
beacon typecheck src/main.py src/utils.py tests/test_main.py
```

### Check all files in a directory

```sh
beacon typecheck src/
```

### Check with JSON output for CI

```sh
beacon typecheck --format json src/ > type-errors.json
```

### Check from stdin

```sh
cat src/main.py | beacon typecheck
```

## Exit Codes

- `0` - No type errors found
- `1` - Type errors found or analysis failed

## Directory Traversal

When a directory is provided, the command:

- Recursively discovers all `.py` files
- Respects `.gitignore` rules
- Excludes common patterns: `__pycache__/`, `*.pyc`, `.pytest_cache/`, `.mypy_cache/`, `.ruff_cache/`, `venv/`, `.venv/`, `env/`, `.env/`
