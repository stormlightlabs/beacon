"""
Comprehensive test cases for type narrowing in match statements with guards

Tests various guard patterns:
1. isinstance checks
2. None checks (is/is not None)
3. Truthiness checks
4. Negation
5. Compound expressions (and/or)
6. Multiple variables in guards
7. Combined pattern + guard narrowing
"""


def test_guard_none_narrowing():
    """Guard with is not None should narrow type"""
    value: int | None = get_optional_int()

    match value:
        case x if x is not None:
            # x should be narrowed to int (None removed)
            num: int = x
        case None:
            pass


def test_guard_isinstance_narrowing():
    """Guard with isinstance check should narrow type"""
    value: int | str | float = get_value()

    match value:
        case x if isinstance(x, int):
            # x should be narrowed to int by the guard
            _ = x.bit_length()
        case x if isinstance(x, str):
            # x should be narrowed to str by the guard
            _ = x.upper()
        case _:
            pass


def test_pattern_narrows_first():
    """Pattern narrows, guard doesn't change anything"""
    value: int | str | None = get_optional_value()

    match value:
        case int(x):
            # x is narrowed to int by pattern
            num: int = x
        case str(s):
            # s is narrowed to str by pattern
            text: str = s
        case None:
            pass


def test_guard_truthiness():
    """Truthiness guard should narrow Optional types"""
    value: str | None = get_optional_str()

    match value:
        case s if s:
            # s should be narrowed to str (None removed)
            text: str = s
        case _:
            pass


def test_guard_with_pattern_binding():
    """Guard narrows a pattern-bound variable"""
    pair: tuple[int | None, int | None] = get_pair()

    match value:
        case (x, y) if x is not None:
            # x should be narrowed from int | None to int
            num: int = x
            # y is still int | None
        case _:
            pass


def test_guard_negation_narrowing():
    """Guard with negation should apply inverse narrowing"""
    value: int | None = get_optional_int()

    match value:
        case x if not (x is None):
            # x should be narrowed to int (None removed)
            _ = x.bit_length()
        case _:
            pass


def test_pattern_and_guard_combined():
    """Pattern narrows first, then guard narrows further"""
    value: int | str | None = get_mixed_optional()

    match value:
        case int(x) if x is not None:
            # Pattern narrows to int, guard is redundant but shouldn't break
            # x is already int (can't be None), so guard doesn't change type
            _ = x.bit_length()
        case str(s) if s:
            # Pattern narrows to str, guard checks truthiness
            # s is str (and non-empty if guard narrows empty strings)
            _ = s.upper()
        case _:
            pass


def test_guard_with_list_pattern_binding():
    """Guard should narrow pattern-bound variables"""
    values: list[int | str | None] = get_list()

    match values:
        case [first, *rest] if first is not None:
            # first is narrowed by guard from int | str | None to int | str
            pass
        case _:
            pass


def test_guard_isinstance_with_tuple():
    """Guard with isinstance tuple should create union"""
    value: int | str | float | bool = get_multi_type()

    match value:
        case x if isinstance(x, (int, str)):
            # x should be narrowed to int | str
            pass
        case _:
            pass


def test_guard_with_class_pattern():
    """Guard should work with class patterns"""

    class Point:
        x: int | None
        y: int | None

    point: Point | None = get_point()

    match point:
        case Point(x, y) if x is not None:
            # point is narrowed to Point by pattern
            # x is narrowed from int | None to int by guard
            _ = x.bit_length()
        case _:
            pass


def test_guard_comparison_no_narrowing():
    """Guards with value comparisons shouldn't cause type narrowing"""
    value: int | str = get_value()

    match value:
        case int(x) if x > 0:
            # x is int from pattern
            # x > 0 is a value check, not a type check
            _ = x.bit_length()
        case str(s) if len(s) > 5:
            # s is str from pattern
            # len(s) > 5 is a value check
            _ = s.upper()


def test_guard_with_attribute_access():
    """Guard with attribute access should extract variables correctly"""

    class Container:
        value: int | None

    container: Container | None = get_container()

    match container:
        case Container() if container is not None:
            # container is narrowed to Container by pattern
            # Guard is redundant but shouldn't break
            pass
        case _:
            pass


def test_guard_with_multiple_variables():
    """Guard referencing multiple variables should narrow all of them"""
    pair: tuple[int | None, str | None] = get_pair()

    match pair:
        case (x, y) if x is not None and y is not None:
            # Both x and y should be narrowed (None removed)
            # x: int, y: str
            _ = x.bit_length()
            _ = y.upper()
        case _:
            pass


def test_nested_match_with_guards():
    """Nested match statements with guards should maintain narrowing"""
    outer: int | str | None = get_mixed_optional()

    match outer:
        case int(x) if x is not None:
            inner: float | None = get_optional_float()
            match inner:
                case y if y is not None:
                    # x is still int here
                    # y is narrowed to float
                    _ = x.bit_length()
                    _ = y.as_integer_ratio()
                case _:
                    pass
        case _:
            pass


def test_guard_with_user_defined_type_guard():
    """Guard with user-defined type guard function"""

    def is_positive(x: int) -> bool:
        return x > 0

    value: int | str = get_value()

    match value:
        case int(x) if is_positive(x):
            # x is int from pattern
            # is_positive is a value check, not a type guard
            _ = x.bit_length()
        case _:
            pass


def test_guard_or_pattern_narrowing():
    """Guard with OR patterns should work correctly"""
    value: int | str | float | None = get_multi_optional()

    match value:
        case (int() | float()) as num if num is not None:
            # num is int | float from OR pattern
            # Guard removes None (which can't be in int | float anyway)
            _ = abs(num)
        case _:
            pass


def test_guard_with_walrus_operator():
    """Guard using walrus operator should narrow the bound variable"""
    value: int | str | None = get_mixed_optional()

    match value:
        case x if (y := x) is not None:
            # Both x and y should be narrowed to int | str (None removed)
            pass
        case _:
            pass


# Helper functions
def get_value() -> int | str | float:
    ...


def get_optional_int() -> int | None:
    ...


def get_optional_str() -> str | None:
    ...


def get_mixed_optional() -> int | str | None:
    ...


def get_optional_value() -> int | str | None:
    ...


def get_list() -> list[int | str | None]:
    ...


def get_multi_type() -> int | str | float | bool:
    ...


def get_point():
    ...


def get_container():
    ...


def get_pair() -> tuple[int | None, str | None]:
    ...


def get_optional_float() -> float | None:
    ...


def get_multi_optional() -> int | str | float | None:
    ...
