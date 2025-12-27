# Static Analysis

The `analyze` command runs static analysis on Python code, including linting and data flow analysis.

## Targets

### File Analysis

Analyze an entire file:

```sh
beacon analyze file ./src/myapp/core.py
```

### Function Analysis

Analyze a specific function in a file:

```sh
beacon analyze function ./src/myapp/core.py:process_data
```

### Class Analysis

Analyze a specific class in a file:

```sh
beacon analyze class ./src/myapp/models.py:User
```

### Package Analysis

Analyze an entire package directory (recursively), resolving imports within the package:

```sh
beacon analyze package ./src/myapp
```

This command will find all Python files in the directory and run diagnostics with workspace context.

### Project Analysis

Analyze an entire project workspace (multiple packages), resolving cross-package dependencies:

```sh
beacon analyze project .
```

This is useful for analyzing monorepos or projects with multiple top-level packages. It sets the working directory as the workspace root.

## Options

### Output Format

Control the output format:

```sh
# Human-readable output (default)
beacon analyze file main.py --format human

# JSON output for machine processing
beacon analyze file main.py --format json

# Compact single-line format (file:line:col)
beacon analyze file main.py --format compact
```

### Analysis Filters

Run specific analyses:

```sh
# Only run linter
beacon analyze file main.py --lint-only

# Only run data flow analysis
beacon analyze file main.py --dataflow-only
```

### Visualization

Show additional information:

```sh
# Show control flow graph visualization (TODO)
beacon analyze file main.py --show-cfg

# Show inferred types (TODO)
beacon analyze file main.py --show-types
```

## Examples

### Analyze a Complete File

```python
# calculator.py
import os

def greet(name):
    return f'Hello {name}'

def unused_function():
    x = 1
    x = 2
    return x

class Calculator:
    def add(self, a, b):
        return a + b
```

```sh
$ beacon analyze file calculator.py
✗ 2 issues found in calculator.py

▸ calculator.py:1:1 [BEA015]
  'os' imported but never used
  1 import os
    ^

▸ calculator.py:8:5 [BEA018]
  'x' is redefined before being used
  8     x = 2
        ^
```

### Analyze a Specific Function

```sh
$ beacon analyze function calculator.py:greet
✗ 1 issues found in calculator.py

▸ calculator.py:1:1 [BEA015]
  'os' imported but never used
  1 import os
    ^
```

### Analyze a Specific Class

```sh
$ beacon analyze class calculator.py:Calculator
✗ 1 issues found in calculator.py

▸ calculator.py:1:1 [BEA015]
  'os' imported but never used
  1 import os
    ^
```

### Lint-Only Mode

Run only linting without data flow analysis:

```sh
beacon analyze file main.py --lint-only
```

### JSON Output

Machine-readable output for tooling integration:

```sh
beacon analyze class models.py:User --format json
```
