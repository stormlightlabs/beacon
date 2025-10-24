"""Test file for default parameter values"""


# Simple default value
def greet(name="World"):
    return f"Hello, {name}!"


# Typed parameter with default
def multiply(x: int = 10, y: int = 5) -> int:
    return x * y


# Mixed parameters
def process(data, scale: float = 1.0, offset=0):
    return data * scale + offset


# Complex default values
def create_list(items=None):
    if items is None:
        items = []
    return items


# Default with identifier reference
MAX_SIZE = 100


def initialize(size=MAX_SIZE, name: str = "default"):
    return {"size": size, "name": name}


# All parameter types together
def advanced(
    required, only_type: int, optional="default", typed: str = "value", *args, **kwargs
):
    pass
