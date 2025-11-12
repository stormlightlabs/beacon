# Formatter CLI

The `format` command exposes Beacon's Python formatter without having to spin up the language server.
It is helpful for debugging formatter behaviour (for example, while comparing
`samples/capabilities_support.py` against the generated `samples/capabilities_support_formatted.py`).

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
beacon format samples/capabilities_support.py
```

### Format file in-place

```sh
beacon format samples/capabilities_support.py --write
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
beacon format samples/capabilities_support.py --output samples/capabilities_support_formatted.py
```

## Directory Traversal

When a directory is provided, the command:

- Recursively discovers all `.py` files
- Respects `.gitignore` rules
- Excludes common patterns: `__pycache__/`, `*.pyc`, `.pytest_cache/`, `.mypy_cache/`, `.ruff_cache/`, `venv/`, `.venv/`, `env/`, `.env/`

## Exit Codes

- `0` - All files are formatted correctly (or formatting succeeded)
- `1` - Formatting would change files (with `--check`) or formatting failed
