"""Any narrowing contract for Milestone 4."""

from __future__ import annotations

from typing import Any


def is_text_guard(value: object) -> TypeGuard[str]:
    return isinstance(value, str)


def is_text_typeis(value: object) -> TypeIs[str]:
    return isinstance(value, str)


def isinstance_any(value: Any) -> Any:
    before = value
    if isinstance(value, str):
        true_branch = value
        result = value.upper()
    else:
        false_branch = value
        result = value
    after = value
    return result


def none_any(value: Any) -> Any:
    if value is None:
        true_none = value
        result = "none"
    else:
        false_none = value
        result = value
    after_none = value
    return result


def typeguard_any(value: Any) -> Any:
    if is_text_guard(value):
        guard_true = value
        result = value.upper()
    else:
        guard_false = value
        result = value
    guard_after = value
    return result


def typeis_any(value: Any) -> Any:
    if is_text_typeis(value):
        typeis_true = value
        result = value.upper()
    else:
        typeis_false = value
        result = value
    typeis_after = value
    return result
