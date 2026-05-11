"""Utility module for CLI fixture imports."""

from __future__ import annotations


def normalize_name(value: str) -> str:
    return value.strip().replace(" ", "-").lower()


def parse_count(value: str, default: int = 0) -> int:
    try:
        return int(value)
    except ValueError:
        return default
