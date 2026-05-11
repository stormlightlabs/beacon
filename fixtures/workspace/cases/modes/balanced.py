"""Balanced-mode diagnostic fixture."""

# beacon: mode=balanced

from __future__ import annotations


def maybe_dynamic(value: object) -> object:
    if hasattr(value, "run"):
        return value.run()
    return value


def optional_flow(value: int | None) -> int:
    if value is None:
        value = 0
    return value + 1
