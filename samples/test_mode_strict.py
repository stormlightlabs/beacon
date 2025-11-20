# beacon: mode=strict
"""
Comprehensive tests for strict mode type checking.

Strict mode requirements:
1. All function parameters MUST have explicit type annotations (ANN007)
2. All function return types MUST have explicit type annotations (ANN008)
3. No type inference is allowed as a substitute for explicit annotations
"""

# ============================================================================
# Basic Function Annotation Tests
# ============================================================================

# ERROR: Should trigger ANN007 for both parameters (x, y)
# ERROR: Should trigger ANN008 for missing return type
def add(x, y):
    """No annotations at all."""
    return x + y


# ERROR: Should trigger ANN007 for parameter (items)
# ERROR: Should trigger ANN008 for missing return type
def process_data(items):
    """Even though we could infer types, strict mode requires explicit annotations."""
    results = []
    for item in items:
        results.append(item * 2)
    return results


# ERROR: Should trigger ANN007 for parameter (data)
# ERROR: Should trigger ANN008 for missing return type
def transform(data):
    """Both parameter and return type missing."""
    return data.upper()


# OK: Fully annotated function
def multiply(x: int, y: int) -> int:
    """Properly annotated - no errors."""
    return x * y


# OK: Explicit None return type annotation
def print_message(msg: str) -> None:
    """None is an explicit annotation - this is OK."""
    print(msg)


# ERROR: Should trigger ANN007 for parameter (value)
# Return type is annotated, so no ANN008
def format_output(value) -> str:
    """Parameter missing annotation even though return type is present."""
    return str(value)


# ERROR: Should trigger ANN008 for missing return type
# Parameters are annotated, so no ANN007
def get_first(items: list):
    """Return type missing annotation even though parameters are present."""
    if items:
        return items[0]
    return None


# ============================================================================
# Strict Mode: Require Explicit Annotations (No Inference)
# ============================================================================

# ERROR: Should trigger ANN007 for parameter (items)
# ERROR: Should trigger ANN008 for return type
# Even though type could be inferred from usage (list operations),
# strict mode does not allow inference - explicit annotations required
def sum_list(items):
    """
    Tests that strict mode requires explicit annotations
    even when types could be inferred from usage.
    """
    total = 0
    for item in items:
        total += item
    return total


# ERROR: Should trigger ANN007 for parameter (name)
# ERROR: Should trigger ANN008 for return type
# Even though return type is clearly str from the f-string,
# strict mode requires explicit annotation
def greet(name):
    """Return type can be inferred as str, but strict mode requires annotation."""
    return f"Hello, {name}!"


# ============================================================================
# Mixed Parameter Annotations
# ============================================================================

# ERROR: Should trigger ANN007 for parameters 'b' and 'c'
# OK: Parameter 'a' is annotated
# Return type is annotated, so no ANN008
def mixed_params(a: int, b, c) -> int:
    """Some parameters annotated, others not."""
    return a + b + c


# ============================================================================
# Functions with Default Values
# ============================================================================

# ERROR: Should trigger ANN007 for parameter (value)
# Default value doesn't substitute for explicit type annotation
def with_default(value=42) -> int:
    """Default value doesn't count as type annotation in strict mode."""
    return value + 1


# ============================================================================
# Class Methods
# ============================================================================

class Calculator:
    """Test class to verify strict mode applies to methods."""

    # ERROR: Should trigger ANN007 for 'x' and 'y'
    # ERROR: Should trigger ANN008 for return type
    def add(self, x, y):
        """Instance method without annotations."""
        return x + y

    # OK: Fully annotated method
    def subtract(self, x: int, y: int) -> int:
        """Instance method with full annotations."""
        return x - y

    # ERROR: Should trigger ANN008 for return type
    # Parameters are annotated, so no ANN007
    @staticmethod
    def multiply_static(x: int, y: int):
        """Static method missing return annotation."""
        return x * y

    # ERROR: Should trigger ANN007 for parameter (x)
    # Return type is annotated, so no ANN008
    @classmethod
    def from_value(cls, x) -> 'Calculator':
        """Class method with parameter missing annotation."""
        return cls()


# ============================================================================
# Nested Functions
# ============================================================================

# OK: Outer function is fully annotated
def outer(x: int) -> int:
    """Outer function with annotations."""

    # ERROR: Should trigger ANN007 for parameter (y)
    # ERROR: Should trigger ANN008 for return type
    def inner(y):
        """Inner function without annotations."""
        return x + y

    return inner(10)


# ============================================================================
# Variadic Parameters
# ============================================================================

# ERROR: Should trigger ANN007 for *args and **kwargs
# OK: Parameter 'x' and return type are annotated
def variadic(x: int, *args, **kwargs) -> int:
    """Variadic parameters need annotations in strict mode."""
    return x + len(args) + len(kwargs)


# ============================================================================
# Async Functions
# ============================================================================

# ERROR: Should trigger ANN007 for parameter (url)
# ERROR: Should trigger ANN008 for return type
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

# ERROR: Should trigger ANN007 for parameter (n)
# ERROR: Should trigger ANN008 for return type (should annotate as Generator)
def count_up(n):
    """Generator without annotations."""
    for i in range(n):
        yield i


# ============================================================================
# Class Attributes
# ============================================================================

class Configuration:
    """Test class attribute annotation requirements."""

    # ERROR: Should trigger ANN009 - class attribute without annotation
    host = "localhost"

    # ERROR: Should trigger ANN009 - class attribute without annotation
    port = 8080

    # OK: Class attribute with annotation
    timeout: int = 30

    # ERROR: Should trigger ANN009 - class attribute without annotation
    debug_mode = True

    def __init__(self, name: str) -> None:
        # OK: Instance attributes in methods don't require annotations
        # (only class-level attributes require ANN009 in strict mode)
        self.name = name
        self.instance_value = 100


class DataClass:
    """Another example with mixed attribute styles."""

    # ERROR: Should trigger ANN009
    count = 0

    # OK: Annotated class attribute
    name: str = "default"

    # ERROR: Should trigger ANN009
    items = []

    @classmethod
    def create(cls, name: str) -> 'DataClass':
        """OK: Fully annotated class method."""
        instance = cls()
        instance.name = name  # Instance attribute assignment
        return instance


# ============================================================================
# Exception Handling
# ============================================================================

# ERROR: Should trigger ANN010 for bare except clause
def risky_operation_bare_except() -> int:
    """Bare except clause is not allowed in strict mode."""
    try:
        return 1 / 0
    except:
        return -1


# OK: Specific exception type is allowed
def risky_operation_specific() -> int:
    """Specific exception types are allowed."""
    try:
        return 1 / 0
    except ZeroDivisionError:
        return -1


# OK: Multiple specific exception types in tuple
def risky_operation_tuple() -> int:
    """Multiple exception types in tuple form are allowed."""
    try:
        return int("not a number")
    except (ValueError, TypeError):
        return -1


# ERROR: Should trigger ANN010 for bare except
# OK for specific exception handlers
def mixed_exception_handlers() -> int:
    """Mixed specific and bare except handlers."""
    try:
        return 1 / 0
    except ValueError:
        return 0
    except TypeError:
        return 1
    except:
        # ERROR: Bare except not allowed in strict mode
        return -1
