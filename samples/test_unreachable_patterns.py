# PM002: Unreachable Pattern Quick Fix Samples
#
# This file demonstrates unreachable patterns in match statements.
# The LSP provides two quick fixes for PM002:
# 1. Move pattern before subsuming pattern (preferred)
# 2. Remove unreachable pattern

from typing import Union


# Example 1: Wildcard pattern before specific patterns
def example_wildcard_before_specific(value: int | str) -> str:
    match value:
        case _:  # This subsumes all patterns below
            return "wildcard"
        case int():  # PM002: Unreachable - subsumed by wildcard above
            return "integer"
        case str():  # PM002: Unreachable - subsumed by wildcard above
            return "string"


# Example 2: General pattern before specific pattern
def example_general_before_specific(value: int | None) -> str:
    match value:
        case int():  # Matches all integers
            return "integer"
        case 42:  # PM002: Unreachable - subsumed by int() above
            return "forty-two"
        case None:
            return "none"


# Example 3: Multiple unreachable patterns
def example_multiple_unreachable(value: int) -> str:
    match value:
        case _:  # Catches everything
            return "wildcard"
        case 1:  # PM002: Unreachable
            return "one"
        case 2:  # PM002: Unreachable
            return "two"
        case int():  # PM002: Unreachable
            return "integer"


# Example 4: Union type with overlapping patterns
def example_union_overlap(value: str | int) -> str:
    match value:
        case str() | int():  # Catches both types
            return "string or int"
        case str():  # PM002: Unreachable - already covered above
            return "string"


# Example 5: Nested type patterns
class Point:
    x: int
    y: int


class Point3D:
    x: int
    y: int
    z: int


def example_nested_types(value: Point | Point3D) -> str:
    match value:
        case Point():  # Matches Point (but not Point3D)
            return "point"
        case Point3D():
            return "point3d"


# Example 6: Correct ordering (no PM002)
def example_correct_ordering(value: int | str | None) -> str:
    match value:
        case 42:  # Specific pattern first
            return "forty-two"
        case int():  # General pattern after specific
            return "integer"
        case str():
            return "string"
        case None:
            return "none"
        case _:  # Wildcard last
            return "other"


# Example 7: Boolean patterns
def example_boolean_patterns(value: bool | int) -> str:
    match value:
        case int():  # Matches all integers (bools are ints in Python)
            return "integer"
        case True:  # PM002: Unreachable - bools are ints
            return "true"
        case False:  # PM002: Unreachable - bools are ints
            return "false"


# Example 8: List patterns
def example_list_patterns(value: list[int]) -> str:
    match value:
        case [*_]:  # Matches any list
            return "any list"
        case []:  # PM002: Unreachable - covered by [*_]
            return "empty"
        case [1, 2, 3]:  # PM002: Unreachable - covered by [*_]
            return "one-two-three"
