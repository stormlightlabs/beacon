"""Generic function and class contract fixture."""

from __future__ import annotations

from collections.abc import Callable
from typing import Concatenate, Generic, ParamSpec, Protocol, TypeVar, TypeVarTuple, Unpack


class SupportsLessThan(Protocol):
    def __lt__(self, other: object) -> bool:
        ...


T = TypeVar("T")
U = TypeVar("U")
ComparableT = TypeVar("ComparableT", bound=SupportsLessThan)
ScalarT = TypeVar("ScalarT", int, float)
T_co = TypeVar("T_co", covariant=True)
T_contra = TypeVar("T_contra", contravariant=True)
P = ParamSpec("P")
Ts = TypeVarTuple("Ts")


class Producer(Generic[T_co]):
    def __init__(self, value: T_co) -> None:
        self.value = value

    def get(self) -> T_co:
        return self.value


class Consumer(Generic[T_contra]):
    def put(self, value: T_contra) -> None:
        _ = value


class Pair(Generic[T, U]):
    def __init__(self, first: T, second: U) -> None:
        self.first = first
        self.second = second

    def swap(self) -> Pair[U, T]:
        return Pair(self.second, self.first)


class VariadicTuple(Generic[Unpack[Ts]]):
    def __init__(self, *items: Unpack[Ts]) -> None:
        pass


def minimum(left: ComparableT, right: ComparableT) -> ComparableT:
    if right < left:
        return right
    return left


def add(left: ScalarT, right: ScalarT) -> ScalarT:
    return left + right


def preserve_call(func: Callable[P, T]) -> Callable[P, T]:
    return func


def bind_first(func: Callable[Concatenate[T, P], U], first: T) -> Callable[P, U]:
    def wrapper(*args: P.args, **kwargs: P.kwargs) -> U:
        return func(first, *args, **kwargs)

    return wrapper


def stringify_pair(left: int, right: str) -> str:
    return f"{left}:{right}"


saved_stringify = preserve_call(stringify_pair)
bound_stringify = bind_first(stringify_pair, 1)
number_pair = Pair(1, "one")
swapped_pair = number_pair.swap()
int_producer = Producer(1)
object_consumer = Consumer()
variadic = VariadicTuple()
minimum_int = minimum(1, 2)
added_float = add(1.0, 2.0)
