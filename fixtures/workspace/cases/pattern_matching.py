"""Pattern matching narrowing."""

from __future__ import annotations

from dataclasses import dataclass
from typing import Literal


@dataclass
class Point:
    x: int
    y: int


def literal_patterns(value: Literal["red", "green", "blue"]) -> int:
    match value:
        case "red":
            return 1
        case "green":
            return 2
        case "blue":
            return 3


def singleton_patterns(value: object) -> str:
    match value:
        case None:
            return "none"
        case True:
            return "true"
        case False:
            return "false"
        case _:
            return "other"


def sequence_and_star_patterns(value: object) -> str:
    match value:
        case []:
            return "empty"
        case [single]:
            return f"single:{single}"
        case [first, *rest]:
            return f"many:{first}:{len(rest)}"
        case _:
            return "other"


def mapping_patterns(value: object) -> str:
    match value:
        case {"kind": "user", "id": int(user_id), **extra}:
            return f"user:{user_id}:{len(extra)}"
        case {"kind": "system"}:
            return "system"
        case _:
            return "unknown"


def class_patterns(value: object) -> str:
    match value:
        case Point(0, 0):
            return "origin"
        case Point(x, y) if x == y:
            return "diagonal"
        case Point(x=x, y=y):
            return f"point:{x}:{y}"
        case _:
            return "other"


def or_as_patterns(value: object) -> str:
    match value:
        case ("add" | "sub") as op, int(left), int(right):
            return f"{op}:{left}:{right}"
        case _:
            return "invalid"


def guarded_pattern(value: object) -> str:
    match value:
        case int(number) if number > 0:
            return "positive"
        case int():
            return "integer"
        case _:
            return "other"


def exhaustive_literal_pattern(value: Literal["yes", "no"]) -> bool:
    match value:
        case "yes":
            return True
        case "no":
            return False


def unreachable_pattern(value: int) -> str:
    match value:
        case int():
            return "int"
        case 1:
            return "never"
        case _:
            return "unknown"
