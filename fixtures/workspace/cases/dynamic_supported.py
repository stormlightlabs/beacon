from __future__ import annotations

from typing import Any


class AttributeBag:
    known: int = 1

    def __getattr__(self, name: str) -> Any:
        return f"dynamic:{name}"


def read_known_attr(bag: AttributeBag) -> int:
    return getattr(bag, "known")


def guarded_attr(bag: AttributeBag) -> bool:
    return hasattr(bag, "known")


def precise_neighbor(value: int) -> int:
    return value + 1
