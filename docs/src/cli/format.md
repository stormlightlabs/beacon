# Formatter CLI

The `format` command exposes Beacon's Python formatter without having to spin up the language server.
It is helpful for debugging formatter behaviour against files in
`fixtures/workspace/`.

## Usage

```sh
beacon format [OPTIONS] [PATHS]...
```

Accepts:

- Single file: `beacon format file.py`
- Multiple files: `beacon format file1.py file2.py file3.py`
- Directory: `beacon format src/` (recursively finds all .py files)
- Stdin: `beacon format` (reads from stdin)

### Options

| Flag              | Description                                                                     |
| ----------------- | ------------------------------------------------------------------------------- |
| `--write`         | Overwrite files with formatted output.                                          |
| `--check`         | Exit with a non-zero status if formatting would change the input.               |
| `--output <PATH>` | Write formatted output to a different file (only works with single file input). |

`--write` conflicts with both `--check` and `--output` to prevent accidental combinations.

## Examples

### Format a single file and display to terminal

```sh
beacon format fixtures/workspace/app/dynamic.py
```

### Format file in-place

```sh
beacon format fixtures/workspace/app/dynamic.py --write
```

### Format all files in a directory

```sh
beacon format src/ --write
```

### Format multiple specific files

```sh
beacon format src/main.py src/utils.py tests/test_main.py --write
```

### Check formatting in CI

```sh
beacon format src/ --check
```

### Write formatted output to a different file

```sh
beacon format fixtures/workspace/app/dynamic.py --output /tmp/dynamic.py
```

## Directory Traversal

When a directory is provided, the command:

- Recursively discovers all `.py` files
- Respects `.gitignore` rules
- Excludes common patterns: `__pycache__/`, `*.pyc`, `.pytest_cache/`, `.mypy_cache/`, `.ruff_cache/`, `venv/`, `.venv/`, `env/`, `.env/`

## Suppression Comments

The formatter respects suppression directives in your code:

```python
# Skip formatting for a single line
x=1+2  # fmt: skip

# Skip formatting for a region
# fmt: off
unformatted=code
# fmt: on
```

See [Formatter Suppressions](../format/suppressions.md) for complete documentation.

## Exit Codes

- `0` - All files are formatted correctly (or formatting succeeded)
- `1` - Formatting would change files (with `--check`) or formatting failed
