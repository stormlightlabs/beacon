"""Type narrowing scenarios from the former sample tests."""

from __future__ import annotations

from collections.abc import Sequence
from typing import Any, TypeGuard


def is_str_list(value: list[object]) -> TypeGuard[list[str]]:
    return all(isinstance(item, str) for item in value)


def use_type_guard(value: list[object]) -> str:
    if is_str_list(value):
        return ",".join(value)
    return "mixed"


def optional_guard(value: str | None) -> str:
    if value is None:
        return "missing"
    return value.upper()


def truthy_guard(value: str | list[str] | None) -> int:
    if value:
        return len(value)
    return 0


def isinstance_chain(value: object) -> str:
    if isinstance(value, str):
        return value.upper()
    if isinstance(value, int):
        return str(value + 1)
    if isinstance(value, Sequence):
        return f"sequence:{len(value)}"
    return "unknown"


def comparison_guard(value: int | str) -> str:
    if isinstance(value, int) and value > 10:
        return "large"
    if value == 0:
        return "zero"
    if value in (1, 2, 3):
        return "small"
    return str(value)


def hasattr_guard(value: object) -> str:
    if hasattr(value, "name"):
        return str(value.name)
    return "anonymous"


def callable_guard(value: object) -> object:
    if callable(value):
        return value()
    return value


def assert_guard(value: str | None) -> str:
    assert value is not None
    return value


def constant_branches(flag: bool) -> int:
    if True:
        result = 1
    else:
        result = 2

    if False:
        result = 3

    return result if flag else 0


def any_boundary(value: Any) -> str:
    if isinstance(value, str):
        return value
    return repr(value)
