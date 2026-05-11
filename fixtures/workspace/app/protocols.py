from __future__ import annotations

from typing import Generic, Protocol, TypeVar

T = TypeVar("T")


class SupportsClose(Protocol):
    def close(self) -> None:
        ...


class JsonSink(Protocol[T]):
    def write(self, item: T) -> None:
        ...


class MemorySink(Generic[T]):
    def __init__(self) -> None:
        self.items: list[T] = []

    def write(self, item: T) -> None:
        self.items.append(item)

    def close(self) -> None:
        self.items.clear()
