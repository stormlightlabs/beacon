# beacon: mode=balanced
"""
Comprehensive tests for balanced mode type checking.

Balanced mode characteristics:
1. Warn on implicit Any from missing annotations (ANN011, ANN012)
2. Warn on missing annotations when types can be inferred (ANN004, ANN006)
3. Warn on annotation mismatches (not errors like strict mode)
4. Allow gradual typing (mixed annotated/unannotated code)
5. No required class attribute annotations (unlike strict mode)
6. Bare except clauses allowed (unlike strict mode)
"""

# ============================================================================
# Implicit Any Warnings
# ============================================================================

# WARNING: Should trigger ANN011 for both parameters (implicit Any)
# WARNING: Should trigger ANN012 for return type (implicit Any)
def process_unknown(data, options):
    """
    When type inference results in Any, balanced mode warns.
    This helps identify truly ambiguous cases where annotations would help.
    """
    return data


# WARNING: Should trigger ANN011 for parameter (implicit Any)
def handle_dynamic(value):
    """Single parameter with implicit Any."""
    print(value)


# ============================================================================
# Missing Annotations with Inferred Types
# ============================================================================

# WARNING: Should trigger ANN004 for both parameters (inferred as int)
# WARNING: Should trigger ANN006 for return type (inferred as int)
def add(x, y):
    """
    Type can be inferred from usage, but annotation is missing.
    Balanced mode suggests the inferred type in the warning.
    """
    return x + y


# WARNING: Should trigger ANN004 for parameter (inferred as list)
# WARNING: Should trigger ANN006 for return type (inferred as int)
def sum_list(items):
    """Return type and parameter type can be inferred."""
    total = 0
    for item in items:
        total += item
    return total


# WARNING: Should trigger ANN004 for parameter (inferred as str)
# WARNING: Should trigger ANN006 for return type (inferred as str)
def greet(name):
    """Clear str inference from f-string usage."""
    return f"Hello, {name}!"


# OK: Fully annotated function (no warnings)
def multiply(x: int, y: int) -> int:
    """Properly annotated functions don't generate warnings."""
    return x * y


# ============================================================================
# Gradual Typing (Mixed Annotated/Unannotated)
# ============================================================================

# WARNING: Should trigger ANN004 for parameter 'b' (inferred as int)
# OK: Parameters 'a' and 'c' are annotated
# OK: Return type is annotated
def mixed_params(a: int, b, c: int) -> int:
    """
    Balanced mode allows gradual typing.
    Only warns on unannotated parameter 'b'.
    """
    return a + b + c


# OK: Parameter annotated
# WARNING: Should trigger ANN012 for return type (implicit Any)
def partial_annotation(value: str):
    """Parameter annotated but return type missing and is Any."""
    print(value)


# WARNING: Should trigger ANN004 for parameter (inferred as int)
# OK: Return type annotated
def another_partial(count) -> str:
    """Return type annotated but parameter missing."""
    return f"Count: {count}"


# ============================================================================
# Annotation Mismatches (Warnings, Not Errors)
# ============================================================================

# WARNING: Should trigger ANN003 for parameter mismatch (annotated str, inferred int)
# WARNING: Should trigger ANN005 for return type mismatch (annotated str, inferred int)
def wrong_annotations(x: str) -> str:
    """
    Annotation doesn't match inferred type.
    In balanced mode, this is a warning (not an error like strict mode).
    """
    return x + 1


# WARNING: Should trigger ANN003 for parameter mismatch
def process_data(items: str):
    """Parameter annotated as str but inferred as list."""
    results = []
    for item in items:
        results.append(item * 2)
    return results


# ============================================================================
# Class Methods
# ============================================================================

class Calculator:
    """Test balanced mode behavior with class methods."""

    # WARNING: Should trigger ANN004 for 'x' and 'y' (inferred as int)
    # WARNING: Should trigger ANN006 for return type (inferred as int)
    def add(self, x, y):
        """Instance method without annotations."""
        return x + y

    # OK: Fully annotated method
    def subtract(self, x: int, y: int) -> int:
        """Instance method with full annotations."""
        return x - y

    # WARNING: Should trigger ANN004 for 'x' and 'y' (inferred as int)
    @staticmethod
    def multiply_static(x, y):
        """Static method missing annotations."""
        return x * y

    # WARNING: Should trigger ANN011 for parameter 'value' (implicit Any)
    @classmethod
    def from_value(cls, value):
        """Class method with implicit Any parameter."""
        instance = cls()
        return instance


# ============================================================================
# Class Attributes (No Warnings Required in Balanced Mode)
# ============================================================================

class Configuration:
    """
    In balanced mode, class attributes don't require annotations.
    This is different from strict mode which requires them.
    """

    # OK: No warning required in balanced mode
    host = "localhost"

    # OK: No warning required in balanced mode
    port = 8080

    # OK: Annotated class attribute (explicit style)
    timeout: int = 30

    # OK: No warning required in balanced mode
    debug_mode = True

    def __init__(self, name: str) -> None:
        # OK: Instance attributes don't require annotations
        self.name = name
        self.instance_value = 100


# ============================================================================
# Nested Functions
# ============================================================================

# OK: Outer function is fully annotated
def outer(x: int) -> int:
    """Outer function with annotations."""

    # WARNING: Should trigger ANN004 for parameter 'y' (inferred as int)
    # WARNING: Should trigger ANN006 for return type (inferred as int)
    def inner(y):
        """Inner function without annotations."""
        return x + y

    return inner(10)


# ============================================================================
# Variadic Parameters
# ============================================================================

# WARNING: Should trigger ANN011 for *args (implicit Any)
# WARNING: Should trigger ANN011 for **kwargs (implicit Any)
# OK: Parameter 'x' and return type are annotated
def variadic(x: int, *args, **kwargs) -> int:
    """Variadic parameters without annotations."""
    return x + len(args) + len(kwargs)


# OK: Fully annotated variadic function
def variadic_annotated(x: int, *args: int, **kwargs: str) -> int:
    """Variadic parameters with annotations."""
    return x + len(args) + len(kwargs)


# ============================================================================
# Async Functions
# ============================================================================

# WARNING: Should trigger ANN011 for parameter (implicit Any)
# WARNING: Should trigger ANN012 for return type (implicit Any)
async def fetch_data(url):
    """Async function without annotations."""
    return f"Data from {url}"


# OK: Async function with annotations
async def fetch_json(url: str) -> dict:
    """Async function with full annotations."""
    return {"url": url}


# ============================================================================
# Generator Functions
# ============================================================================

# WARNING: Should trigger ANN004 for parameter (inferred as int)
# WARNING: Should trigger ANN006 for return type (should be Generator[int, None, None])
def count_up(n):
    """Generator without annotations."""
    for i in range(n):
        yield i


# ============================================================================
# Functions with Default Values
# ============================================================================

# WARNING: Should trigger ANN004 for parameter (inferred as int)
# OK: Return type annotated
def with_default(value=42) -> int:
    """Default value doesn't substitute for annotation."""
    return value + 1


# OK: Parameter and return type annotated
def with_default_annotated(value: int = 42) -> int:
    """Default value with explicit annotation."""
    return value + 1


# ============================================================================
# Exception Handling (Bare Except Allowed in Balanced Mode)
# ============================================================================

# OK: Bare except clause is allowed in balanced mode
# (Unlike strict mode which requires specific exception types)
def risky_operation_bare_except() -> int:
    """Bare except clause is allowed in balanced mode."""
    try:
        return 1 / 0
    except:
        return -1


# OK: Specific exception type is also allowed
def risky_operation_specific() -> int:
    """Specific exception types are allowed."""
    try:
        return 1 / 0
    except ZeroDivisionError:
        return -1


# OK: Mixed specific and bare except handlers are allowed
def mixed_exception_handlers() -> int:
    """Mixed specific and bare except handlers."""
    try:
        return 1 / 0
    except ValueError:
        return 0
    except TypeError:
        return 1
    except:
        # OK: Bare except allowed in balanced mode
        return -1


# ============================================================================
# Lambda Functions
# ============================================================================

# Lambdas typically can't have annotations in Python < 3.10 syntax
# Balanced mode handles them gracefully
square = lambda x: x * x  # Type inference may apply here
identity = lambda x: x


# ============================================================================
# Context Managers
# ============================================================================

class Resource:
    """Example context manager."""

    # WARNING: Should trigger ANN011 for parameter (implicit Any)
    def __init__(self, name):
        self.name = name

    def __enter__(self):
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        return False


# ============================================================================
# Decorators
# ============================================================================

# WARNING: Should trigger ANN011 for parameter 'func' (implicit Any)
# WARNING: Should trigger ANN012 for return type (implicit Any)
def simple_decorator(func):
    """Decorator without annotations."""
    def wrapper(*args, **kwargs):
        print(f"Calling {func.__name__}")
        return func(*args, **kwargs)
    return wrapper


@simple_decorator
def decorated_function(x: int) -> int:
    """Function using decorator."""
    return x * 2


# ============================================================================
# Type Inference Edge Cases
# ============================================================================

# WARNING: Should trigger ANN006 for return type (inferred as None)
# OK: Parameter annotated
def print_message(msg: str):
    """Implicit None return type (no explicit return)."""
    print(msg)


# WARNING: Should trigger ANN004 for parameter (inferred as list)
# WARNING: Should trigger ANN006 for return type (inferred based on conditional)
def get_first(items):
    """Return type inference with conditionals."""
    if items:
        return items[0]
    return None


# OK: Explicit None annotation (no warning)
def explicit_none(msg: str) -> None:
    """Explicit None is fine."""
    print(msg)
