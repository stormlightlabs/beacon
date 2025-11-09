# Structural Formatting

Structural formatting rules control the layout of Python constructs beyond basic whitespace and indentation.

## Trailing Commas

Trailing commas in multi-line structures improve git diffs and make adding items easier.

### Configuration

Controlled by `beacon.formatting.trailingCommas`:

- `always`: Add trailing commas to all multi-line structures
- `multiline`: Add trailing commas only to multi-line nested structures (default)
- `never`: Never add trailing commas

### Behavior

The formatter determines whether to add a trailing comma based on:

1. The trailing comma configuration setting
2. Whether the structure spans multiple lines
3. The nesting depth of the structure

For `multiline` mode, trailing commas are added when both conditions are met:

- The structure is multi-line
- The structure is nested (inside parentheses, brackets, or braces)

### Examples

```python
# multiline mode
items = [
    "first",
    "second",
    "third",  # trailing comma added (nested and multiline)
]

func(
    arg1,
    arg2,  # trailing comma added
)

# Top-level, single-line: no trailing comma
top_level = ["a", "b", "c"]
```

## Dictionary Formatting

Dictionary formatting includes key-value spacing and multi-line alignment.

### Value Indentation

For multi-line dictionaries, the formatter calculates appropriate indentation:

- **Nested dictionaries**: Use base indentation + 1 level
- **Inline dictionaries**: Align values with key width + 2 spaces

```python
# Nested multi-line
config = {
    "key": "value",
    "nested": {
        "inner": "data",
    },
}

# Inline alignment
options = {"short": "val", "longer_key": "val"}
```

## Comprehensions

List, dict, set, and generator comprehensions are formatted based on length.

### Wrapping Strategy

The formatter chooses between horizontal and vertical layout:

- **Horizontal**: Entire comprehension fits on one line
- **Vertical**: Comprehension exceeds available line space

```python
# Horizontal (fits on one line)
squares = [x**2 for x in range(10)]

# Vertical (too long)
result = [
    transform(item)
    for item in collection
    if predicate(item)
]
```

## Lambda Expressions

Lambda expressions wrap to multiple lines when they exceed the line length limit.

### Wrapping Decision

Determined by: `current_column + lambda_width > line_length`

```python
# Short lambda: stays on one line
square = lambda x: x**2

# Long lambda: may need refactoring to def
complex = lambda x, y, z: (
    some_complex_calculation(x, y, z)
)
```

## Decorators

Decorators are formatted with one decorator per line and proper spacing.

### Rules

1. Each decorator on its own line
2. Decorators aligned at the same indentation as the function/class
3. @ symbol normalized (added if missing)
4. No blank lines between consecutive decorators

```python
@property
@lru_cache(maxsize=128)
def expensive_computation(self):
    return result
```

## Class Definitions

Class definitions follow PEP 8 spacing conventions.

### Blank Lines

Controlled by `beacon.formatting.blankLineBeforeClass`:

- **Top-level classes**: 2 blank lines before (when enabled)
- **Nested classes**: 1 blank line before

```python
# Module-level


class TopLevelClass:
    pass


class AnotherTopLevelClass:

    class NestedClass:
        pass
```

## Function Definitions

Function definitions use similar spacing rules to classes.

### Blank Lines

Controlled by `beacon.formatting.blankLineBeforeFunction`:

- **Top-level functions**: 2 blank lines before (when enabled)
- **Methods**: 1 blank line before

```python
# Module-level


def top_level_function():
    pass


class MyClass:

    def method_one(self):
        pass

    def method_two(self):
        pass
```

## Type Annotations

Type annotation spacing follows PEP 8 guidelines.

### Spacing Rules

Returns tuple `(before, after)` for colon spacing:

- **Variable annotations**: No space before colon, one space after
- **Return annotations**: Space before and after `->`

```python
# Variable annotations
name: str = "value"
count: int = 42

# Function annotations
def greet(name: str, age: int) -> str:
    return f"Hello {name}"
```

## Implementation

Structural formatting rules are implemented in `FormattingRules`:

- `should_add_trailing_comma()`: Determines trailing comma insertion
- `format_decorator()`: Normalizes decorator syntax
- `type_annotation_spacing()`: Returns spacing tuple
- `should_wrap_lambda()`: Decides lambda wrapping
- `dict_value_indent()`: Calculates dictionary value indentation
- `comprehension_wrapping_strategy()`: Returns wrapping strategy
- `blank_lines_for_class()`: Returns required blank lines for classes
- `blank_lines_for_function()`: Returns required blank lines for functions

All rules are context-aware and respect the current indentation level and nesting depth.
