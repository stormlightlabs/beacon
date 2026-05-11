"""Strict-mode diagnostic fixture."""

# beacon: mode=strict

from __future__ import annotations


def implicit_any(value):
    return value.missing


def suspicious_none(value: str | None) -> str:
    return value.upper()
