# Suppression/Ignore Comments

Beacon supports suppression/ignore comments to selectively disable the formatter, linter, and type checker on specific lines or regions of code.

## Formatter Suppressions

Control when the formatter should skip code sections.

### Single Line: `# fmt: skip`

Skip formatting for a single statement or line.

```python
# This line will be formatted normally
x = 1

# This line preserves exact spacing
y=2+3  # fmt: skip

# Back to normal formatting
z = 4
```

### Region: `# fmt: off` / `# fmt: on`

Disable formatting for entire code blocks.

```python
# Normal formatting applies here
formatted_dict = {"key": "value"}

# fmt: off
unformatted_dict={"key":"value","no":"spaces"}
complex_expression=1+2+3+4+5+6+7+8+9
# fmt: on

# Back to formatted code
back_to_normal = {"properly": "formatted"}
```

- Multiple `# fmt: off`/`# fmt: on` pairs allowed in the same file
- Unclosed `# fmt: off` preserves formatting to end of file
- The directive lines themselves are preserved as-is

### Alignment Preservation

Common use case: preserving column alignment in matrices or tables.

```python
# Normal list formatting
matrix = [
    [1, 2, 3],
    [4, 5, 6],
]

# fmt: off
# Preserve column alignment
aligned_matrix = [
    [1,    2,    3],
    [100,  200,  300],
    [10,   20,   30],
]
# fmt: on
```

## Linter Suppressions

Suppress specific linter warnings or all warnings on a line.

### Suppress All Warnings: `# noqa`

Disable all linter checks for a line.

```python
x = 1  # noqa
```

### Suppress Specific Rules: `# noqa: CODE`

Disable specific linter rules by code.

```python
# Suppress unused import warning
import os  # noqa: BEA015

# Suppress unused variable warning
result = expensive_computation()  # noqa: BEA016

# Suppress multiple specific rules
break  # noqa: BEA005, BEA010
```

### Multiple Rules

Separate multiple rule codes with commas:

```python
# Suppress both undefined name and unused variable
x = undefined_variable  # noqa: BEA001, BEA016
```

### Case Insensitive

Rule codes are case-insensitive:

```python
x = 1  # noqa: bea016  # Same as BEA016
```

## Type Checker Suppressions

Suppress type checking errors.

### Suppress All Type Errors: `# type: ignore`

Disable all type checking for a line.

```python
x: int = "string"  # type: ignore
```

### Suppress Specific Error: `# type: ignore[code]`

Disable specific type error categories.

```python
# Suppress only assignment type errors
value: str = 42  # type: ignore[assignment]

# Suppress multiple error types
result: int = some_function()  # type: ignore[assignment, call-arg]
```

Common type error codes:

- `assignment` - Type mismatch in assignment
- `arg-type` - Incorrect argument type
- `return-value` - Return type mismatch
- `call-arg` - Function call argument errors
- `attr-defined` - Attribute not defined

## Combining Suppression/Ignore Comments

Multiple suppression types can be used on the same line in any order

```python
# Suppress both type checker and linter
x: int = "string"  # type: ignore  # noqa: BEA016

# Formatter skip with linter suppression
y=2+3  # fmt: skip  # noqa: BEA020
```

```python
z = value  # noqa: BEA001  # type: ignore
# Same as:
z = value  # type: ignore  # noqa: BEA001
```

## Quick Reference

| Comment                | Scope        | Applies To              | Example                                    |
| ---------------------- | ------------ | ----------------------- | ------------------------------------------ |
| `# fmt: skip`          | Single line  | Formatter               | `x=1  # fmt: skip`                         |
| `# fmt: off`           | Start region | Formatter               | See examples above                         |
| `# fmt: on`            | End region   | Formatter               | See examples above                         |
| `# noqa`               | Single line  | All linter rules        | `x=1  # noqa`                              |
| `# noqa: CODE`         | Single line  | Specific linter rule(s) | `import os  # noqa: BEA015`                |
| `# type: ignore`       | Single line  | All type errors         | `x: int = "s"  # type: ignore`             |
| `# type: ignore[code]` | Single line  | Specific type error(s)  | `x: int = "s"  # type: ignore[assignment]` |

## See Also

- [Linter Rules](../lsp/lint_rules.md) - Complete list of BEA rule codes
- [Type Checking](../lsp/type_checking.md) - Type system documentation
- [Formatter Configuration](../configuration.md) - Global formatter settings
