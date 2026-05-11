"""Workspace-wide examples for LSP diagnostics and feature tests."""

from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path
from typing import Generic, Protocol, TypeVar


T = TypeVar("T")


class DataProvider(Protocol[T]):
    def get(self, key: str) -> T: ...

    def put(self, key: str, value: T) -> None: ...


@dataclass
class CacheEntry(Generic[T]):
    key: str
    value: T
    hits: int = 0

    def touch(self) -> None:
        self.hits += 1


class MemoryProvider(Generic[T]):
    def __init__(self) -> None:
        self._items: dict[str, T] = {}

    def get(self, key: str) -> T:
        return self._items[key]

    def put(self, key: str, value: T) -> None:
        self._items[key] = value


def use_provider(provider: DataProvider[str]) -> str:
    provider.put("name", "Beacon")
    return provider.get("name").lower()


def wrong_return_type() -> int:
    return "not an int"


def use_before_definition() -> int:
    result = later_value + 1
    later_value = 41
    return result


def unreachable_after_return(flag: bool) -> int:
    if flag:
        return 1
        dead_value = 2
    return 0


def rename_target(path: Path) -> str:
    content = path.read_text(encoding="utf-8")
    return content.strip()
