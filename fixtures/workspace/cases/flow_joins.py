"""Flow join narrowing coverage for Milestone 4."""

from __future__ import annotations


def branch_join(value: int | str | None) -> str:
    before = value
    if value is None:
        inside_none = value
        result = "missing"
    elif isinstance(value, int):
        inside_int = value
        result = str(value + 1)
    else:
        inside_str = value
        result = value.upper()
    after = value
    return result


def loop_join(values: list[int | None]) -> int:
    total = 0
    current: int | None = None
    for item in values:
        current = item
        if current is None:
            continue
        narrowed = current
        total = total + narrowed
    after_loop = current
    return total


def try_finally_join(value: int | str) -> str:
    before_try = value
    try:
        if isinstance(value, int):
            inside_try = value
            result = str(value + 1)
        else:
            inside_else = value
            result = value.upper()
    finally:
        after_finally = value
    return result


def match_join(value: int | str | None) -> str:
    before_match = value
    match value:
        case None:
            in_none = value
            label = "none"
        case int() as number:
            in_int = number
            label = str(number + 1)
        case str() as text:
            in_str = text
            label = text.upper()
    after_match = value
    return label
