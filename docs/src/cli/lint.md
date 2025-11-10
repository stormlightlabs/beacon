# Linting

The `lint` command runs the Beacon linter on Python code to detect common coding issues, style violations, and potential bugs.

## Usage

Lint a Python file:

```sh
beacon lint example.py
```

Read from stdin:

```sh
cat example.py | beacon lint
```

## Examples

### Detecting Unused Imports and Variable Redefinition

```python
# test.py
import os

def greet(name):
    return f'Hello {name}'

def unused_function():
    x = 1
    x = 2  # Redefined before being used
    return x
```

```sh
beacon lint test.py
```

Output:

```sh
✗ 2 issues found in test.py

▸ test.py:1:1 [BEA015]
  'os' imported but never used
  1 import os
    ^

▸ test.py:8:5 [BEA018]
  'x' is redefined before being used
  8     x = 2
        ^
```

### Clean Code - No Issues

```python
# clean.py
def add(x, y):
    return x + y

result = add(1, 2)
print(result)
```

```sh
$ beacon lint clean.py
✓ No issues found
```

## Output Formats

### Human-Readable (Default)

Shows issues with context and line numbers (default format):

```sh
$ beacon lint test.py
✗ 2 issues found in test.py

▸ test.py:1:1 [BEA015]
  'os' imported but never used
  1 import os
    ^

▸ test.py:8:5 [BEA018]
  'x' is redefined before being used
  8     x = 2
        ^
```

### JSON Format

Machine-readable output for CI/CD integration:

```sh
beacon lint test.py --format json
```

Output:

```json
[
  {
    "rule": "UnusedImport",
    "message": "'os' imported but never used",
    "filename": "test.py",
    "line": 1,
    "col": 1
  },
  {
    "rule": "RedefinedWhileUnused",
    "message": "'x' is redefined before being used",
    "filename": "test.py",
    "line": 8,
    "col": 5
  }
]
```

### Compact Format

Single-line format compatible with many editors:

```sh
$ beacon lint test.py --format compact
test.py:1:1: [BEA015] 'os' imported but never used
test.py:8:5: [BEA018] 'x' is redefined before being used
```

## Lint Rules

The linter implements PyFlakes-style rules (BEA001-BEA030):

- Undefined variables
- Unused imports and variables
- Syntax errors in specific contexts
- Potential bugs (assert on tuple, is vs ==, etc.)
- Code style issues

For a complete list of rules, see the [Lint Rules](../lsp/lint_rules.md) documentation.

## Exit Codes

- `0` - No issues found
- `1` - Issues found

This makes it easy to use in CI/CD pipelines:

```sh
beacon lint src/**/*.py || exit 1
```

## Notes

The linter does not fix issues automatically. It only reports them.
