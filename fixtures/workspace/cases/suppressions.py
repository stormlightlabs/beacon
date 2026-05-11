"""Suppression directive coverage for diagnostics and formatting."""

from __future__ import annotations


def unused_suppressed() -> None:
    ignored = 1  # noqa: F841
    also_ignored = "x"  # type: ignore[assignment]
    _ = also_ignored


def beacon_suppression(value: object) -> int:
    # beacon: ignore[unknown-attribute]
    return value.missing_attribute


def inline_type_ignore(value: object) -> str:
    return value.upper()  # type: ignore[attr-defined]


def block_suppression() -> None:
    # beacon: disable=unused-variable
    local_value = 42
    another_value = local_value + 1
    # beacon: enable=unused-variable
    _ = another_value
