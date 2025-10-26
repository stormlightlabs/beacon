"""Shared helpers for exercising Beacon LSP features.

This module mixes dataclasses, protocols, and generics so hover, go-to-definition,
and workspace-symbol requests have meaningful targets.
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import Callable, Generic, Iterable, Protocol, TypeVar

T = TypeVar("T")


@dataclass
class CacheEntry(Generic[T]):
    """Simple cache entry with an eager hit counter."""

    key: str
    value: T
    hits: int = 0

    def record_hit(self) -> None:
        self.hits += 1


class DataProvider(Protocol[T]):
    """Protocol consumed by the main sample to demonstrate structural typing."""

    name: str

    def load(self) -> Iterable[T]: ...

    def describe(self, item: T) -> str: ...


class InMemoryProvider(Generic[T]):
    """Basic provider that pulls data from a callable."""

    def __init__(self, name: str, loader: Callable[[], Iterable[T]]):
        self.name = name
        self._loader = loader

    def load(self) -> list[T]:
        return list(self._loader())

    def describe(self, item: T) -> str:
        return f"{self.name}: {item}"


SERVICE_REGISTRY: dict[str, DataProvider[object]] = {}


def register_provider(name: str, provider: DataProvider[object]) -> None:
    """Register providers so workspace symbol search and references have data."""

    SERVICE_REGISTRY[name] = provider


def prime_cache(provider: DataProvider[T]) -> list[CacheEntry[T]]:
    """Builds ready-to-hover cache entries from any provider."""

    entries = [
        CacheEntry(key=str(idx), value=item) for idx, item in enumerate(provider.load())
    ]
    for entry in entries:
        entry.record_hit()
    return entries


__all__ = [
    "CacheEntry",
    "DataProvider",
    "InMemoryProvider",
    "SERVICE_REGISTRY",
    "prime_cache",
    "register_provider",
]
