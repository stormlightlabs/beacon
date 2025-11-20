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

| Feature                          | Strict  | Balanced  | Loose   |
| -------------------------------- | ------- | --------- | ------- |
| Annotation mismatches            | Error   | Warning   | Hint    |
| Missing annotations (inferred)   | Error   | Warning   | Silent  |
| Implicit Any                     | Error   | Warning   | Silent  |
| Bare except clauses              | Error   | Allowed   | Allowed |
| Class attribute annotations      | Required| Optional  | Optional|

## Strict Mode

Enforces complete type annotation coverage with **no type inference allowed**. All function parameters and return types must have explicit type annotations.

Characteristics:

- All annotation mismatches are errors
- **All function parameters must have explicit type annotations** (ANN007)
- **All function return types must have explicit type annotations** (ANN008)
- **All class attributes must have explicit type annotations** (ANN009)
- **Bare `except:` clauses are forbidden** (ANN010)
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

# Exception handling requires specific exception types
def process() -> int:
    try:
        return risky_operation()
    except:  # ✗ Error - bare except not allowed (ANN010)
        return -1

def safe_process() -> int:
    try:
        return risky_operation()
    except (ValueError, TypeError):  # ✓ Valid - specific exception types
        return -1
```

## Balanced Mode

Provides helpful warnings while allowing gradual type annotation adoption. Distinguishes between concrete inferred types and implicit Any to guide annotation efforts.

Characteristics:

- Annotation mismatches are warnings (not errors)
- Missing annotations with concrete inferred types trigger warnings showing the inferred type
- Implicit Any types (unresolvable inference) trigger warnings to identify ambiguous cases
- Allows mixing annotated and unannotated code (gradual typing)
- Ideal for incrementally adding types to existing projects

Example:

```python
# beacon: mode=balanced

# ✓ No warnings - fully annotated
def process(data: list[int]) -> int:
    return sum(data)

# ⚠ Warning ANN006 - missing return type annotation (inferred as int)
# Suggestion includes the inferred type to guide annotation
def calculate(x: int, y: int):
    return x + y

# ⚠ Warning ANN004 - parameter 'first' missing annotation (inferred as str)
# Type can be inferred from usage context
def format_name(first, last: str) -> str:
    return f"{first} {last}"

# ⚠ Warning ANN011 - parameter 'data' has implicit Any type
# ⚠ Warning ANN012 - return type is implicit Any
# Type inference couldn't determine concrete types
def process_unknown(data, options):
    return data

# ⚠ Warning ANN011 - parameter 'b' has implicit Any type
# Gradual typing: warns only on unannotated parameter
def mixed_params(a: int, b, c: int) -> int:
    return a + b + c
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

| Code   | Description                                                | Strict | Balanced | Loose  |
| ------ | ---------------------------------------------------------- | ------ | -------- | ------ |
| ANN001 | Annotation mismatch on assignments                         | Error  | Warning  | Hint   |
| ANN002 | Missing annotation on assignments                          | Error  | Warning  | Silent |
| ANN003 | Parameter annotation mismatch                              | Error  | Warning  | Hint   |
| ANN004 | Missing parameter annotation (inferred type is concrete)   | -      | Warning  | Silent |
| ANN005 | Return type annotation mismatch                            | Error  | Warning  | Hint   |
| ANN006 | Missing return type annotation (inferred type is concrete) | -      | Warning  | Silent |
| ANN007 | Parameter missing annotation (strict mode)                 | Error  | -        | -      |
| ANN008 | Return type missing annotation (strict mode)               | Error  | -        | -      |
| ANN009 | Class attribute missing annotation                         | Error  | -        | -      |
| ANN010 | Bare except clause without exception type                  | Error  | -        | -      |
| ANN011 | Parameter has implicit Any type                            | -      | Warning  | -      |
| ANN012 | Return type has implicit Any type                          | -      | Warning  | -      |

**Strict Mode:** All missing parameter and return type annotations trigger ANN007/ANN008 errors respectively.
Class attributes without annotations trigger ANN009 errors. Bare except clauses trigger ANN010 errors.

**Balanced Mode:** Distinguishes between concrete inferred types and implicit Any:

- ANN004/ANN006: Missing annotations where type inference determined a concrete type (warns with suggested type)
- ANN011/ANN012: Missing annotations where type inference resulted in implicit Any (warns about ambiguity)

See complete [diagnostic codes](./lsp/diagnostic_codes.md) documentation for more information

### Type Inference and Implicit Any

After constraint solving, Beacon finalizes any unresolved type variables as `Any` enabling balanced mode to distinguish between:

1. **Concrete inferred types**: Type inference successfully determined a specific type (int, str, list, etc.)
2. **Implicit Any**: Type inference couldn't resolve to a concrete type due to insufficient context
3. **Active type variables**: Still in the inference process (no diagnostic yet)

Diagnostic behavior:

- **Strict mode**: All missing annotations are errors (ANN007/ANN008), regardless of inference
- **Balanced mode**: Warns on both concrete inferred types (ANN004/ANN006) and implicit Any (ANN011/ANN012)
- **Loose mode**: Silent on missing annotations, only hints on explicit mismatches
- **NoneType returns**: No diagnostic for procedures with implicit None return (void functions)

Example:

```python
# beacon: mode=balanced

# ⚠ Warning ANN004/ANN006 - concrete inferred type (int)
# Suggestion shows the inferred type
def add(x: int, y: int):
    return x + y  # Inferred as int

# ⚠ Warning ANN011/ANN012 - implicit Any
# Type inference couldn't determine concrete type
def process(data):
    return transform(data)  # Unknown transform behavior

# ✓ No diagnostic - procedure with None return
def log_message(msg: str):
    print(msg)  # Inferred as None, no warning needed
```
