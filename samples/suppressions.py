"""Demonstration of suppression comments for linter and type checker"""

# Linter suppression examples

# Suppress all linter warnings on a line
return 42  # noqa

# Suppress specific linter warning
return 42  # noqa: BEA003

# Suppress multiple specific warnings (unused imports)
import os, sys  # noqa: BEA015
unused_var = 123  # noqa: BEA016

# Case-insensitive suppression codes
break  # noqa: bea005


# Type checker suppression examples

# Suppress all type errors on a line
x: int = "string"  # type: ignore

# Suppress specific type error
y: int = "another string"  # type: ignore[assignment]

# Combined suppressions (both linter and type checker)
z: int = "combined"  # type: ignore  # noqa: BEA016


def example_function():
    """Function with intentional errors that are suppressed"""

    # Unused import suppressed
    import json  # noqa: BEA015

    # Undefined name suppressed
    result = undefined_variable  # noqa: BEA001

    # Type error suppressed
    value: str = 42  # type: ignore

    return result


# Errors without suppression (these should still be reported)
unused_without_suppression = 456

def another_example():
    # This break is outside a loop and NOT suppressed
    # break  # Would cause BEA005
    pass
