"""Negative call diagnostics for parameter metadata."""

from __future__ import annotations


def pos_only(user_id: int, /, label: str) -> str:
    return f"{user_id}:{label}"


def kw_only(*, required: str, optional: int = 1) -> str:
    return f"{required}:{optional}"


bad_positional_only = pos_only(user_id=1, label="admin")
bad_missing_keyword_only = kw_only()
