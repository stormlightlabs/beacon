# Type Checking Modes

Beacon supports three type checking modes that let you balance type safety with development flexibility: strict, balanced, and loose.

## Configuration

Set the mode in your `beacon.toml` (see [config documentatation](./configuration.md) for more information):

```toml
[type_checking]
mode = "balanced"  # or "strict" or "loose"
```

Override the mode for specific files using a comment directive at the top of the file:

```python
# beacon: mode=strict

def calculate(x: int, y: int) -> int:
    return x + y
```

## Mode Comparison

| Feature | Strict | Balanced | Loose |
|---------|--------|----------|-------|
| Annotation mismatches | Error | Warning | Hint |
| Missing annotations | Error | Warning | Silent |
| Implicit Any | Error | Warning | Silent |
| Best for | New projects, type-safe libraries | Gradual adoption, existing codebases | Legacy code, exploratory development |

## Strict Mode

Enforces complete type annotation coverage with **no type inference allowed**. All function parameters and return types must have explicit type annotations.

Characteristics:

- All annotation mismatches are errors
- **All function parameters must have explicit type annotations** (ANN007)
- **All function return types must have explicit type annotations** (ANN008)
- **All class attributes must have explicit type annotations** (ANN009)
- Missing annotations are treated as implicit `Any`, which is forbidden in strict mode
- Type inference is not allowed as a substitute for explicit annotations
- Best for greenfield projects, type-safe libraries, and critical components

Example:

```python
# beacon: mode=strict

# ✓ Valid - fully annotated
def process(data: list[int]) -> int:
    total: int = sum(data)
    return total

# ✗ Error - missing return type annotation (ANN008)
# Even though the return type could be inferred as int
def calculate(x: int, y: int):
    return x + y

# ✗ Error - parameter 'first' missing annotation (ANN007)
# Strict mode requires explicit annotations, no inference
def format_name(first, last: str) -> str:
    return f"{first} {last}"

# ✗ Error - both parameters and return type missing (ANN007, ANN008)
# Strict mode requires all annotations, even when types could be inferred
def add(x, y):
    return x + y

# Class attributes also require explicit annotations in strict mode
class Config:
    # ✗ Error - class attribute missing annotation (ANN009)
    host = "localhost"

    # ✓ Valid - annotated class attribute
    port: int = 8080
```

## Balanced Mode

Provides helpful warnings while allowing gradual type annotation adoption.

Characteristics:

- Annotation mismatches are warnings
- Missing annotations are warnings
- Allows mixing annotated and unannotated code
- Ideal for incrementally adding types to existing projects

Example:

```python
# beacon: mode=balanced

# ✓ No warnings - fully annotated
def process(data: list[int]) -> int:
    return sum(data)

# ⚠ Warning - missing return type annotation (ANN006)
def calculate(x: int, y: int):
    return x + y

# ⚠ Warning - missing parameter annotation (ANN004)
def format_name(first, last: str) -> str:
    return f"{first} {last}"
```

## Loose Mode

Minimally intrusive type checking focused on explicit mismatches.

Characteristics:

- Only explicit annotation mismatches produce hints
- Missing annotations are silent
- Maximum flexibility for exploration and legacy code
- Useful for initial type system adoption

Example:

```python
# beacon: mode=loose

# ✓ No diagnostics
def process(data):
    return sum(data)

# ℹ Hint only - annotation doesn't match inference (ANN001)
def calculate(x: int, y: int) -> str:  # Returns int, not str
    return x + y

# ✓ No diagnostics - missing annotations are allowed
def format_name(first, last):
    return f"{first} {last}"
```

## Annotation Coverage Diagnostics

Beacon validates type annotations against inferred types and reports missing annotations based on the active mode.

### Diagnostic Codes

| Code | Description | Strict | Balanced | Loose |
|------|-------------|--------|----------|-------|
| ANN001 | Annotation mismatch on assignments | Error | Warning | Hint |
| ANN002 | Missing annotation on assignments | Error | Warning | Silent |
| ANN003 | Parameter annotation mismatch | Error | Warning | Hint |
| ANN004 | Missing parameter annotation (inferred type is concrete) | - | Warning | Silent |
| ANN005 | Return type annotation mismatch | Error | Warning | Hint |
| ANN006 | Missing return type annotation (inferred type is concrete) | - | Warning | Silent |
| ANN007 | Parameter missing annotation (implicit Any) | Error | - | - |
| ANN008 | Return type missing annotation (implicit Any) | Error | - | - |
| ANN009 | Class attribute missing annotation | Error | - | - |

**Note:** In strict mode, all missing parameter and return type annotations are treated as implicit `Any` and reported as ANN007/ANN008 errors. Class attributes without annotations trigger ANN009 errors. In balanced mode, only parameters/returns with concrete inferred types (not Any) generate ANN004/ANN006 warnings.

### Smart Filtering

Beacon skips diagnostics in cases where type inference is incomplete or trivial:

- Types inferred as `Any` (inference couldn't determine a specific type)
- Types containing unresolved type variables
- Function return types inferred as `None` (procedures without explicit returns)

Example:

```python
# beacon: mode=strict

# ✓ No diagnostic - inferred as Any (incomplete inference)
x = get_dynamic_value()

# ✓ No diagnostic - procedure with no return
def log_message(msg: str) -> None:
    print(msg)

# ✗ Error (ANN006) - returns concrete type without annotation
def add(x: int, y: int):
    return x + y  # Inferred as int
```
