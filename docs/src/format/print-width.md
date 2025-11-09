# Line Length and Wrapping

Beacon enforces configurable line length limits and provides smart line breaking for long statements.

## Line Length

The default maximum line length is 88 characters, matching Black's default.

### Configuration

Set line length via `formatting.lineLength`:

```toml
[formatting]
lineLength = 88  # Black default

# Or for strict PEP8
lineLength = 79
```

### Unicode Width Calculation

Line length is calculated using Unicode display width, not byte count.

```python
# This emoji counts as 2 characters wide
message = "Status: ✅"
```

## Line Breaking

When a line exceeds the configured length, Beacon breaks it the following boundaries:

### Break Points

Lines can break at these locations:

**Commas**: Highest priority break point

```python
# Before
result = function(very_long_arg1, very_long_arg2, very_long_arg3, very_long_arg4)

# After
result = function(
    very_long_arg1,
    very_long_arg2,
    very_long_arg3,
    very_long_arg4
)
```

**Binary Operators**: Secondary break point

```python
# Before
total = first_value + second_value + third_value + fourth_value

# After (when nested)
total = (
    first_value
    + second_value
    + third_value
    + fourth_value
)
```

**Opening Brackets**: When deeply nested

```python
# Multiple levels of nesting
data = {
    'key': [
        item1,
        item2
    ]
}
```

### Preserving User Breaks

If your manually inserted line breaks keep the code under the limit, they are preserved:

```python
# This will not be reformatted if under line limit
result = function(
    arg1, arg2
)
```

## Wrapping Strategies

Different constructs use different wrapping strategies.

### Function Calls

Function calls use one of three strategies based on argument width:

**Horizontal**: All arguments on one line when they fit

```python
result = function(arg1, arg2, arg3)
```

**Vertical**: One argument per line when arguments are long

```python
result = function(
    very_long_argument_name_1,
    very_long_argument_name_2,
    very_long_argument_name_3
)
```

**Mixed**: Multiple arguments per line for medium-length arguments

```python
result = function(
    arg1, arg2,
    arg3, arg4,
    arg5
)
```

### Function Definitions

Function parameters wrap similarly to function calls:

```python
def long_function_name(
    parameter1: str,
    parameter2: int,
    parameter3: bool = False
) -> None:
    pass
```

### Hanging Indents

Parameters can align with the opening delimiter:

```python
result = function(argument1,
                  argument2,
                  argument3)
```

Or use a consistent indent level:

```python
result = function(
    argument1,
    argument2,
    argument3
)
```

Beacon prefers consistent indent levels for clarity.

### Collection Literals

Collections wrap to vertical layout when they exceed line length:

**Lists and Tuples**:

```python
items = [
    'first',
    'second',
    'third'
]
```

**Dictionaries**:

```python
mapping = {
    'key1': 'value1',
    'key2': 'value2',
    'key3': 'value3'
}
```

**Sets**:

```python
unique = {
    item1,
    item2,
    item3
}
```

### Binary Expressions

Long binary expressions break before operators:

```python
result = (
    condition1
    and condition2
    or condition3
)
```

### Parenthesized Continuations

Python's implicit line continuation inside parentheses is preferred over backslashes:

```python
# Preferred
total = (
    first_value
    + second_value
    + third_value
)

# Avoid
total = first_value \
    + second_value \
    + third_value
```

## Trailing Commas

Beacon adds trailing commas in multi-line structures when `formatting.trailingCommas` is set appropriately:

```toml
[formatting]
trailingCommas = "multiline"  # Add in multi-line structures (default)
# trailingCommas = "always"   # Always add
# trailingCommas = "never"    # Never add
```

With `multiline` setting:

```python
items = [
    'first',
    'second',
    'third',  # Trailing comma added
]
```

Benefits of trailing commas:

- Cleaner diffs when adding/removing items
- Prevents forgetting commas when reordering
- Consistent formatting

## Context-Aware Breaking

Breaking decisions consider context:

**Inside Strings and Comments**: Never break

```python
# This string won't be broken even if it's very long
message = "This is a very long string that exceeds the line length limit"
```

**Nested Constructs**: Allow breaking at higher nesting levels

```python
result = outer(
    inner(
        arg1,
        arg2
    )
)
```

**Statement Boundaries**: Prefer breaking between statements

```python
# Break between statements
first = calculate_first()
second = calculate_second()

# Rather than within a statement
first = calculate_first(); second = calculate_second()
```

## Configuration Summary

Related configuration options:

```toml
[formatting]
lineLength = 88                     # Maximum line length
trailingCommas = "multiline"        # Trailing comma strategy
compatibilityMode = "black"         # Affects wrapping decisions
```
