"""Protocol contract fixture for inheritance, generics, callbacks, and misses."""

from __future__ import annotations

from typing import Callable, Generic, Protocol, TypeVar

T = TypeVar("T")
T_co = TypeVar("T_co", covariant=True)


class Named(Protocol):
    name: str


class Readable(Protocol[T_co]):
    def read(self) -> T_co:
        ...


class Closeable(Protocol):
    def close(self) -> None:
        ...


class Resource(Readable[str], Closeable, Protocol):
    pass


class Callback(Protocol[T]):
    def __call__(self, value: T) -> str:
        ...


class User:
    name = "Ada"


class FileResource:
    def read(self) -> str:
        return "data"

    def close(self) -> None:
        pass


class IntCallback:
    def __call__(self, value: int) -> str:
        return str(value)


class Box(Generic[T]):
    def __init__(self, value: T) -> None:
        self.value = value

    def read(self) -> T:
        return self.value


def load(resource: Resource) -> str:
    value = resource.read()
    resource.close()
    return value


def invoke(callback: Callback[int]) -> str:
    return callback(1)


def use_reader(reader: Readable[T]) -> T:
    return reader.read()


loaded = load(FileResource())
called = invoke(IntCallback())
read_int = use_reader(Box(1))
