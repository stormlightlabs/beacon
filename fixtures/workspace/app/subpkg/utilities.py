from __future__ import annotations


def public_label(value: str) -> str:
    return value.upper()


def _private_label(value: str) -> str:
    return f"_{value}"
