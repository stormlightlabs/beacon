"""Suppression directive coverage for diagnostics and formatting."""

from __future__ import annotations


def unused_suppressed() -> None:
    ignored = 1  # type: ignore[ANN002, unused-variable]
    also_ignored = "x"  # type: ignore[ANN002]
    _ = also_ignored  # type: ignore[ANN002]


def beacon_suppression(value: object) -> int:
    # beacon: ignore[unknown-attribute]
    return value.missing_attribute


def inline_type_ignore(value: object) -> str:
    return value.upper()  # type: ignore[attr-defined]


def block_suppression() -> None:
    # beacon: disable=ANN002, unused-variable
    local_value = 42
    another_value = local_value + 1
    # beacon: enable=ANN002, unused-variable
    _ = another_value  # type: ignore[ANN002]
