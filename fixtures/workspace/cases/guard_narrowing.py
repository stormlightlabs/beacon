"""Guard narrowing coverage for Milestone 4."""

from __future__ import annotations

from collections.abc import Callable
from typing import TypeGuard


class Animal:
    pass


class Dog(Animal):
    pass


class Cat(Animal):
    pass


def is_string(value: object) -> TypeGuard[str]:
    return isinstance(value, str)


def isinstance_guard(value: int | str | None) -> str:
    if isinstance(value, (int, str)):
        return str(value)
    return "none"


def issubclass_guard(cls: type[Animal]) -> str:
    if issubclass(cls, Dog):
        return "dog"
    return "animal"


def exact_type_guard(value: int | bool | str) -> str:
    if type(value) is bool:
        return "bool"
    if type(value) == int:
        return "int"
    return "str"


def none_guard(value: str | None) -> str:
    if value is None:
        return "missing"
    return str(value)


def truthiness_guard(value: str | list[str] | None) -> int:
    if value:
        return len(value)
    return 0


def membership_guard(value: str) -> str:
    if value in {"red", "green", "blue"}:
        return "known"
    if value not in {"black", "white"}:
        return "custom"
    return "neutral"


def hasattr_guard(value: object) -> str:
    if hasattr(value, "name"):
        return "named"
    return "anonymous"


def callable_guard(value: Callable[[], str] | str) -> str:
    if callable(value):
        return value()
    return value


def assertion_guard(value: str | None) -> str:
    assert value is not None
    return str(value)


def user_defined_guard(value: object) -> str:
    if is_string(value):
        return value
    return "not string"
