"""
Simple test for guard narrowing without relying on method calls
Tests that type narrowing happens correctly by checking assignability
"""


def test_guard_none_narrowing():
    """Guard with is not None should narrow type"""
    value: int | None = get_optional_int()

    match value:
        case x if x is not None:
            # x should be narrowed to int (None removed)
            # This assignment should work because x is int
            num: int = x
        case _:
            pass


def test_guard_isinstance_narrowing():
    """Guard with isinstance should narrow type"""
    value: int | str = get_value()

    match value:
        case x if isinstance(x, int):
            # x should be narrowed to int
            num: int = x
        case x if isinstance(x, str):
            # x should be narrowed to str
            text: str = x


def test_pattern_with_guard_narrowing():
    """Pattern narrows, then guard narrows further"""
    value: int | str | None = get_optional_value()

    match value:
        case int(x):
            # x is narrowed to int by pattern
            num: int = x
        case str(s):
            # s is narrowed to str by pattern
            text: str = s


def test_guard_truthiness():
    """Truthiness guard should narrow Optional types"""
    value: str | None = get_optional_str()

    match value:
        case s if s:
            # s should be narrowed to str (None removed)
            text: str = s


def test_combined_pattern_and_guard():
    """Pattern + guard narrowing together"""
    value: int | None = get_optional_int()

    match value:
        case x if x is not None:
            # x should be narrowed to int
            num: int = x


# Helper functions
def get_optional_int() -> int | None:
    ...


def get_value() -> int | str:
    ...


def get_optional_value() -> int | str | None:
    ...


def get_optional_str() -> str | None:
    ...
