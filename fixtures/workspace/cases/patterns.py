"""Structural pattern matching coverage."""

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


def sequence_patterns(value: object) -> str:
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


def unreachable_pattern(value: int) -> str:
    match value:
        case int():
            return "int"
        case 1:
            return "never"
        case _:
            return "unknown"
