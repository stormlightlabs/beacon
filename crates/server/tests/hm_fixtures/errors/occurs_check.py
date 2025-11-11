"""
Fixtures that should trigger occurs check failures.
These tests verify that the type checker correctly detects infinite types.
"""


def self_referential(x):
    """This should fail occurs check if we try to infer x = list[x]"""
    if isinstance(x, list):
        return [x]
    return x


def circular_lambda():
    """Circular reference in lambda - should trigger occurs check"""
    f = lambda x: [f]
    return f


def recursive_list():
    """Building a type that references itself"""
    x = []
    x.append(x)
    return x
