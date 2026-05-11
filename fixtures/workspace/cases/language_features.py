"""General Python syntax and flow-control coverage for parser smoke tests."""

from __future__ import annotations

import os
from collections.abc import Iterable
from typing import Optional


PI = 3.14159
DEBUG = True


def traced(func):
    def wrapper(*args: object, **kwargs: object) -> object:
        return func(*args, **kwargs)

    return wrapper


@traced
def greet(name: str = "World") -> str:
    return f"Hello, {name}!"


class Calculator:
    def __init__(self, initial: int = 0) -> None:
        self.value = initial

    def add(self, x: int) -> int:
        self.value += x
        return self.value

    def multiply(self, factor: int = 2) -> int:
        self.value *= factor
        return self.value


def control_flow(x: int) -> str:
    if x > 0:
        return "positive"
    if x < 0:
        return "negative"
    return "zero"


def loops(items: Iterable[int]) -> list[int]:
    result: list[int] = []
    for item in items:
        if item % 2 == 0:
            continue
        result.append(item * 2)
    else:
        result.append(0)
    return result


def with_statement(path: str) -> str:
    with open(path, encoding="utf-8") as handle:
        return handle.read()


def comprehensions(values: list[int]) -> tuple[list[int], set[int], dict[int, int]]:
    squares = [x * x for x in values if x > 0]
    unique = {x for x in values if x % 2 == 0}
    mapping = {x: x * 2 for x in values}
    return squares, unique, mapping


def walrus(values: list[str]) -> list[str]:
    return [line for value in values if (line := value.strip())]


def lambda_and_closure(seed: int) -> int:
    total = seed

    def add(value: int) -> None:
        nonlocal total
        total += value

    double = lambda value: value * 2
    add(double(3))
    return total


def match_statement(value: object) -> str:
    match value:
        case {"kind": "user", "name": str(name)}:
            return f"user:{name}"
        case [first, *rest] if rest:
            return f"list:{first}"
        case int() | float():
            return "number"
        case None:
            return "none"
        case _:
            return "unknown"


def operator_expressions(a: int, b: int) -> dict[str, int | bool | None]:
    return {
        "add": a + b,
        "subtract": a - b,
        "multiply": a * b,
        "divide": a // b if b else None,
        "and": a and b,
        "or": a or b,
        "compare": a < b,
    }


class DataProcessor:
    def __init__(self, data: list[dict[str, object]]) -> None:
        self.data = data
        self.processed: list[dict[str, object]] = []

    def filter_by_key(self, key: str, value: object) -> list[dict[str, object]]:
        return [item for item in self.data if item.get(key) == value]

    def process_nested(self) -> None:
        def transform(item: dict[str, object]) -> dict[str, object]:
            transformed = item.copy()
            transformed["processed"] = True
            return transformed

        self.processed = [transform(item) for item in self.data]

    def get_optional(self, index: int) -> Optional[dict[str, object]]:
        if 0 <= index < len(self.processed):
            return self.processed[index]
        return None


def default_parameters(
    required: str,
    optional: str = "default",
    count: int = 10,
    enabled: bool = True,
) -> dict[str, object]:
    return {
        "required": required,
        "optional": optional,
        "count": count,
        "enabled": enabled,
        "cwd": os.getcwd(),
    }


def simple_compares(x: int, y: int) -> list[bool]:
    return [
        x == y,
        x != y,
        x < y,
        x <= y,
        x > y,
        x >= y,
        x in [1, 2, 3],
        x not in [4, 5, 6],
    ]
