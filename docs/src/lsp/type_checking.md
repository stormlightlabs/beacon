# Type Checking

Beacon provides powerful static type checking for Python code, combining the rigor of Hindley-Milner type inference with the flexibility needed for Python's dynamic features.

## Suppressing Type Errors

Type checking errors can be suppressed using inline comments:

```python
x: int = "string"  # type: ignore
value: str = 42  # type: ignore[assignment]  # Suppress specific error type
```

See [Suppressions](../format/suppressions.md) for complete documentation on suppression comments.

## Type System Philosophy

Beacon's type checker is designed with a core principle: **context-aware strictness**. It maintains strong type safety for genuinely unsafe operations while being permissive for common, safe Python patterns.

### Design Goals

1. **High Signal-to-Noise Ratio**: Report errors that matter, not false positives from valid Python code
2. **Catch Real Bugs**: Focus on type mismatches that lead to runtime errors
3. **Support Gradual Typing**: Work seamlessly with both annotated and unannotated code
4. **Python-First Semantics**: Understand Python idioms rather than forcing ML-style patterns

## Union and Optional Types

### How Union Types Work

Union types represent values that can be one of several types. Beacon treats union types using **subtyping semantics** rather than strict structural equality.

```python
# This is valid - None is a member of Optional[int]
def get_value() -> int | None:
    return None  # No error

# Union members work naturally
x: int | str = 42  # int is a subtype of int | str
y: int | str = "hello"  # str is a subtype of int | str
```

### Optional Types

`Optional[T]` is syntactic sugar for `Union[T, None]`. Beacon understands that None is a valid value for Optional types without requiring explicit checks:

```python
from typing import Optional

def process(value: Optional[str]) -> None:
    # Assigning None to Optional is always valid
    result: Optional[str] = None  # No error
```

### Type Narrowing

While union types are permissive for assignment, **accessing attributes or calling methods requires narrowing**:

```python
def process(value: int | None) -> int:
    # Error: None doesn't have __add__
    return value + 1

def process_safe(value: int | None) -> int:
    if value is None:
        return 0
    # value is narrowed to int here
    return value + 1  # OK
```

Beacon provides several narrowing mechanisms:

1. **None Checks**: `if x is None` / `if x is not None`
2. **isinstance() Guards**: `if isinstance(x, int)`
3. **Truthiness**: `if x` narrows away None and falsy values
4. **Type Guards**: User-defined type guard functions
5. **Match Statements**: Pattern matching with exhaustiveness checking

## Subtyping vs Unification

Beacon's type checker uses two complementary mechanisms:

### Unification (Strict)

Used for non-union types. Requires structural equality:

```python
x: int = 42
y: str = x  # Error: cannot unify int with str
```

### Subtyping (Flexible)

Used when union types are involved. Checks semantic compatibility:

```python
x: int = 42
y: int | str = x  # OK: int <: int | str

z: int | str | None = None  # OK: None <: int | str | None
```

This hybrid approach provides:

- **Strictness where it matters**: Direct type mismatches are caught
- **Flexibility for unions**: Common patterns like Optional work naturally

## Type Inference

Beacon infers types even without annotations:

```python
def add(x, y):
    return x + y
# Inferred type: (int, int) -> int or (str, str) -> str
# (overloaded based on usage)

numbers = [1, 2, 3]
# Inferred type: list[int]
```

### Value Restriction

Beacon applies the value restriction to prevent unsafe generalization:

```python
empty_list = []  # Type: list[Never] - cannot generalize
# Must provide type hint for empty collections:
numbers: list[int] = []  # Type: list[int]
```

## Special Types

### Any

`Any` is the escape hatch for truly dynamic code. It unifies with all types without errors:

```python
from typing import Any

def dynamic_operation(x: Any) -> Any:
    return x.anything()  # No type checking
```

Use `Any` sparingly - it disables type checking for that value.

### Never

`Never` represents impossible values or code paths that never return:

```python
def unreachable() -> Never:
    raise RuntimeError("Never returns")

def example(x: int) -> int:
    if x < 0:
        unreachable()
    return x  # Type checker knows we only reach here if x >= 0
```

### Top (⊤)

Top is the supertype of all types. It appears in generic bounds and protocol definitions but is rarely used directly.

## Flow-Sensitive Type Narrowing

Beacon tracks type information through control flow:

```python
def process(x: int | str | None) -> int:
    if x is None:
        return 0
    # x: int | str here

    if isinstance(x, int):
        return x
    # x: str here

    return len(x)  # OK: x is definitely str
```

This works with:

- If statements
- While loops
- Match statements
- Try-except blocks
- Boolean operators (`and`, `or`)

### Exhaustiveness Checking

Match statements and if-elif chains are checked for exhaustiveness:

```python
def handle(x: bool) -> str:
    match x:
        case True:
            return "yes"
        case False:
            return "no"
    # OK: all cases covered

def incomplete(x: int | str) -> str:
    if isinstance(x, int):
        return "number"
    # Warning: str case not handled
```

## Generic Types

Beacon supports generic types with type parameters:

```python
from typing import TypeVar, Generic

T = TypeVar('T')

class Box(Generic[T]):
    def __init__(self, value: T) -> None:
        self.value = value

    def get(self) -> T:
        return self.value

# Type inference works:
int_box = Box(42)  # Box[int]
str_box = Box("hello")  # Box[str]
```

## Protocols

Beacon supports structural typing through protocols:

```python
from typing import Protocol

class Drawable(Protocol):
    def draw(self) -> None: ...

def render(obj: Drawable) -> None:
    obj.draw()  # OK if obj has draw() method

class Circle:
    def draw(self) -> None:
        print("drawing circle")

render(Circle())  # OK: Circle satisfies Drawable protocol
```

## Common Patterns

### Optional Chaining

```python
from typing import Optional

def get_name(user: Optional[dict]) -> Optional[str]:
    if user is None:
        return None
    return user.get("name")  # Type checker knows user is dict
```

### Union Type Discrimination

```python
def process(value: int | list[int]) -> int:
    if isinstance(value, int):
        return value
    return sum(value)  # value is list[int] here
```

### Type Guard Functions

```python
from typing import TypeGuard

def is_str_list(val: list) -> TypeGuard[list[str]]:
    return all(isinstance(x, str) for x in val)

def process(items: list[int | str]) -> None:
    if is_str_list(items):
        # items: list[str] here
        print(",".join(items))  # OK
```

## Error Messages

Beacon provides context-aware error messages:

- **String/Int Mixing**: Suggests explicit conversion
- **None Errors**: Explains Optional types and None checks
- **Union Errors**: Shows which union branches failed and why
- **Collection Mismatches**: Identifies list vs dict vs tuple confusion

Error messages focus on actionable fixes rather than type theory jargon.

## Configuration

Type checking strictness can be controlled via `beacon.toml`:

```toml
[analysis]
# Warn when Any is used (default: false)
warn-on-any = true

# Strict mode: disallow implicit Any (default: false)
strict = false

# Report unused variables (default: true)
unused-variables = true
```

## Best Practices

1. **Use Optional for nullable values**: `Optional[T]` is clearer than `T | None` for function signatures
2. **Narrow before use**: Check for None before accessing attributes
3. **Leverage type guards**: Create reusable type narrowing functions
4. **Avoid Any**: Use Union types or Protocol types for flexibility
5. **Add type hints to empty collections**: Help inference with `list[int]()` instead of `[]`
6. **Trust the type checker**: If it says a path is unreachable, it probably is

## When Type Checking Fails You

Sometimes the type checker can't infer what you know is true. Use these escape hatches:

```python
from typing import cast, Any

# cast: Assert a type without runtime check
value = cast(int, get_dynamic_value())

# Any: Disable type checking
dynamic: Any = get_unknown_type()

# Type ignore comment (use sparingly)
result = complex_operation()  # type: ignore

# Assert narrowing
x: int | None = get_value()
assert x is not None
# x: int here (type checker understands assert)
```

Use these sparingly and document why the type checker needs help.
