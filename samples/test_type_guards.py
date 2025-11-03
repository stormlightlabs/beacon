"""Test user-defined type guards with TypeGuard and TypeIs"""

from typing import TypeGuard, TypeIs


def is_str(val: object) -> TypeGuard[str]:
    """Type guard function that narrows to str"""
    return isinstance(val, str)


def is_int(val: object) -> TypeIs[int]:
    """Type guard function using TypeIs that asserts exact type"""
    return isinstance(val, int)


def is_str_list(val: list[object]) -> TypeGuard[list[str]]:
    """Type guard for list[str]"""
    return all(isinstance(x, str) for x in val)


# Test 1: Basic TypeGuard narrowing
def test_basic_type_guard():
    x: int | str = get_value()

    if is_str(x):
        # x should be narrowed to str here
        x.upper()  # Should not error
    else:
        # x should be int here
        x.bit_length()  # Should not error


# Test 2: TypeIs narrowing
def test_type_is():
    x: int | str | None = get_optional_value()

    if is_int(x):
        # x should be narrowed to int
        x.bit_length()  # Should not error


# Test 3: Complex type guard with lists
def test_list_type_guard():
    my_list: list[object] = get_list()

    if is_str_list(my_list):
        # my_list should be narrowed to list[str]
        for item in my_list:
            item.upper()  # Should not error


# Test 4: Type guard in elif
def test_type_guard_elif():
    x: int | str | None = get_value_or_none()

    if x is None:
        pass
    elif is_str(x):
        # x should be str here
        x.lower()
    else:
        # x should be int here
        x.bit_length()


# Test 5: Negated type guard
def test_negated_type_guard():
    x: int | str = get_int_or_str()

    if not is_str(x):
        # x should be int here (remainder after excluding str)
        x.bit_length()


# Helper functions for tests
def get_value() -> int | str:
    return 42


def get_optional_value() -> int | str | None:
    return None


def get_list() -> list[object]:
    return []


def get_value_or_none() -> int | str | None:
    return None


def get_int_or_str() -> int | str:
    return 42
