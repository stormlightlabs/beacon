"""
Functional programming pipeline fixtures for testing HM type inference.
Tests higher-order functions, composition, and generalization.
"""

from typing import Callable, TypeVar

T = TypeVar('T')
U = TypeVar('U')
V = TypeVar('V')


def identity(x: T) -> T:
    """Identity function - should infer forall T. T -> T"""
    return x


def compose(f: Callable[[U], V], g: Callable[[T], U]) -> Callable[[T], V]:
    """Function composition - should infer forall T, U, V. (U -> V) -> (T -> U) -> T -> V"""
    def composed(x: T) -> V:
        return f(g(x))
    return composed


def map_list(f: Callable[[T], U], xs: list[T]) -> list[U]:
    """Map function - should infer forall T, U. (T -> U) -> list[T] -> list[U]"""
    return [f(x) for x in xs]


def filter_list(predicate: Callable[[T], bool], xs: list[T]) -> list[T]:
    """Filter function - should infer forall T. (T -> bool) -> list[T] -> list[T]"""
    return [x for x in xs if predicate(x)]


def pipe(x: T, f: Callable[[T], U], g: Callable[[U], V]) -> V:
    """Pipeline operator - should infer forall T, U, V. T -> (T -> U) -> (U -> V) -> V"""
    return g(f(x))


# Test generalization
twice = lambda x: x + x
numbers = [1, 2, 3, 4, 5]
doubled = map_list(twice, numbers)

# Test composition
str_len = lambda s: len(s)
is_long = lambda n: n > 5
check_long_str = compose(is_long, str_len)
result = check_long_str("hello world")

# Test identity
x = identity(42)
y = identity("hello")
z = identity([1, 2, 3])
