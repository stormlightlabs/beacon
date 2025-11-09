# String and Comment Formatting

Beacon's formatter provides intelligent string quote normalization and comment formatting while preserving special directives and avoiding unnecessary escaping.

## String Quote Normalization

The formatter can normalize string quotes according to your preferred style:

### Quote Styles

- **Double quotes** (default) - Converts strings to use `"`
- **Single quotes** - Converts strings to use `'`
- **Preserve** - Keeps original quote style

### Smart Escaping Avoidance

The formatter intelligently avoids quote normalization when it would introduce escaping:

```python
# Configuration: quote_style = "double"

# Would require escaping, so preserved
'He said "hello" to me'

# No quotes inside, normalized
'simple string' → "simple string"
```

### Prefixed Strings

String prefixes (r, f, rf, etc.) are preserved during normalization:

```python
# Configuration: quote_style = "double"
r'raw string' → r"raw string"
f'formatted {x}' → f"formatted {x}"
rf'raw formatted' → rf"raw formatted"
```

## Docstring Formatting

Triple-quoted strings (docstrings) receive special handling:

### Quote Normalization

Docstrings are normalized to the configured quote style unless they contain the target quote sequence:

```python
# Configuration: quote_style = "double"
'''Single quoted docstring''' → """Single quoted docstring"""

# Contains target quotes, preserved
'''String with """quotes""" inside'''
```

### Indentation

Multi-line docstrings maintain consistent indentation:

```python
def example():
    """
    This is a docstring with
    properly normalized indentation
    across all lines
    """
```

## Comment Formatting

Comments are formatted for consistency while preserving special directives.

### Standard Comments

Regular comments are formatted with a single space after the `#`:

```python
#comment → # comment
#  multiple   spaces → # multiple   spaces
```

### Inline Comments

Inline comments (on the same line as code) are preceded by two spaces:

```python
x = 1  # inline comment
```

### Special Directives

Tool-specific comments are preserved exactly as written:

- `# type: ignore` - Type checking suppressions
- `# noqa` - Linting suppressions
- `# pylint:`, `# mypy:`, `# flake8:` - Tool-specific directives
- `# fmt: off/on`, `# black:` - Formatter control

```python
x = very_long_line()  # type: ignore  # Preserved exactly
```

### Block Comments

Multi-line block comments at module level may be surrounded by blank lines for better separation.

## Configuration

String and comment formatting respects these settings:

- `beacon.formatting.quoteStyle` - Quote normalization style (default: "double")
- `beacon.formatting.normalizeDocstringQuotes` - Apply quote normalization to docstrings (default: true)

## Examples

### Before Formatting

```python
def greet(name):
    '''Say hello'''  #function docstring
    message='Hello, ' + name  #create greeting
    return message
```

### After Formatting

```python
def greet(name):
    """Say hello"""  # function docstring
    message = "Hello, " + name  # create greeting
    return message
```
