# Whitespace and Indentation

This document describes Beacon's whitespace and indentation formatting rules.

## Indentation

Beacon normalizes indentation according to PEP8 guidelines.

### Indent Size

Default indentation is 4 spaces per level:

```python
def example():
    if condition:
        do_something()
```

Configure indentation via `formatting.indentSize`:

```toml
[formatting]
indentSize = 2  # Use 2 spaces
```

### Tabs vs Spaces

Spaces are strongly recommended and used by default. Tabs can be enabled but are not PEP8-compliant:

```toml
[formatting]
useTabs = true  # Not recommended
```

## Trailing Whitespace

All trailing whitespace is removed from lines:

```python
# Before
def foo():
    return 42

# After
def foo():
    return 42
```

This applies to all lines, including blank lines.

## Blank Lines

Beacon manages blank lines according to PEP8 conventions.

### Top-Level Definitions

Two blank lines separate top-level class and function definitions:

```python
def first_function():
    pass


def second_function():
    pass


class MyClass:
    pass
```

Configure via `formatting.blankLineBeforeClass` and `formatting.blankLineBeforeFunction`:

```toml
[formatting]
blankLineBeforeClass = true
blankLineBeforeFunction = true
```

### Method Definitions

One blank line separates methods within a class:

```python
class Example:
    def first_method(self):
        pass

    def second_method(self):
        pass
```

### Maximum Consecutive Blank Lines

By default, at most 2 consecutive blank lines are allowed:

```python
# Before
def foo():
    pass




def bar():
    pass

# After
def foo():
    pass


def bar():
    pass
```

Configure via `formatting.maxBlankLines`:

```toml
[formatting]
maxBlankLines = 1  # Allow only 1 blank line
```

## Operators

Whitespace around operators depends on the operator type.

### Binary Operators

Single space on both sides of binary operators when `formatting.spacesAroundOperators` is enabled (default):

```python
# Arithmetic
result = x + y
quotient = a / b
power = base ** exponent

# Comparison
if value == expected:
    pass

# Logical
condition = flag and other_flag
```

### Unary Operators

No space between unary operator and operand:

```python
negative = -value
inverted = ~bits
boolean = not flag
```

### Assignment Operators

Single space around assignment operators:

```python
x = 10
count += 1
value *= 2
```

## Delimiters

### Parentheses, Brackets, Braces

No whitespace immediately inside delimiters:

```python
# Correct
function(arg1, arg2)
items = [1, 2, 3]
mapping = {'key': 'value'}

# Incorrect
function( arg1, arg2 )
items = [ 1, 2, 3 ]
mapping = { 'key': 'value' }
```

### Commas

No space before comma, single space after:

```python
# Correct
items = [1, 2, 3]
function(a, b, c)

# Incorrect
items = [1 ,2 ,3]
function(a,b,c)
```

### Colons

In dictionaries and slices, no space before colon, single space after:

```python
# Dictionary
mapping = {'key': 'value', 'other': 'data'}

# Slice
subset = items[start:end]
every_other = items[::2]
```

In function annotations, no space before colon, single space after:

```python
def greet(name: str) -> str:
    return f"Hello, {name}"
```

In class inheritance and control flow, no space before colon:

```python
class Child(Parent):
    pass

if condition:
    pass
```

## Comments

### Inline Comments

Inline comments have two spaces before the hash and one space after:

```python
x = x + 1  # Increment
```

### Block Comments

Block comments start at the beginning of a line or at the current indentation level:

```python
# This is a block comment
# spanning multiple lines
def function():
    # Indented block comment
    pass
```

## Configuration Summary

Related configuration options:

```toml
[formatting]
indentSize = 4                      # Spaces per indent level
useTabs = false                     # Use spaces, not tabs
maxBlankLines = 2                   # Maximum consecutive blank lines
spacesAroundOperators = true        # Add spaces around binary operators
blankLineBeforeClass = true         # 2 blank lines before top-level classes
blankLineBeforeFunction = true      # 2 blank lines before top-level functions
```
