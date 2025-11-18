"""
Clean test for guard narrowing - demonstrates successful type narrowing
"""


def test_guard_none_narrowing():
    """Guard with is not None should narrow type"""
    value: int | None = get_optional_int()

    match value:
        case x if x is not None:
            # x should be narrowed to int (None removed)
            num: int = x  # This should pass - x is narrowed to int
        case None:
            pass


def test_pattern_narrows_first():
    """Pattern narrows, guard doesn't change anything"""
    value: int | str | None = get_optional_value()

    match value:
        case int(x):
            # x is narrowed to int by pattern
            num: int = x  # This should pass
        case str(s):
            # s is narrowed to str by pattern
            text: str = s  # This should pass
        case None:
            pass


def test_guard_truthiness():
    """Truthiness guard should narrow Optional types"""
    value: str | None = get_optional_str()

    match value:
        case s if s:
            # s should be narrowed to str (None removed)
            text: str = s  # This should pass
        case _:
            pass


def test_guard_with_pattern_binding():
    """Guard narrows a pattern-bound variable"""
    pair: tuple[int | None, int | None] = get_pair()

    match pair:
        case (x, y) if x is not None:
            # x should be narrowed from int | None to int
            num: int = x  # This should pass
            # y is still int | None
        case _:
            pass


# Helper functions
def get_optional_int() -> int | None:
    ...


def get_optional_value() -> int | str | None:
    ...


def get_optional_str() -> str | None:
    ...


def get_pair() -> tuple[int | None, int | None]:
    ...
