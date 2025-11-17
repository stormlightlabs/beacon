# Test comparison operations in match guards

def test_equality_guard(x: int) -> str:
    """Test == in guard"""
    match x:
        case n if n == 5:
            return "five"
        case n if n == 10:
            return "ten"
        case _:
            return "other"


def test_not_equal_guard(x: int) -> str:
    """Test != in guard"""
    match x:
        case n if n != 0:
            return "non-zero"
        case _:
            return "zero"


def test_less_than_guard(x: int) -> str:
    """Test < in guard"""
    match x:
        case n if n < 0:
            return "negative"
        case n if n < 10:
            return "small"
        case _:
            return "large"


def test_less_or_equal_guard(x: int) -> str:
    """Test <= in guard"""
    match x:
        case n if n <= 0:
            return "non-positive"
        case _:
            return "positive"


def test_greater_than_guard(x: int) -> str:
    """Test > in guard"""
    match x:
        case n if n > 100:
            return "large"
        case n if n > 0:
            return "positive"
        case _:
            return "non-positive"


def test_greater_or_equal_guard(x: int) -> str:
    """Test >= in guard"""
    match x:
        case n if n >= 0:
            return "non-negative"
        case _:
            return "negative"


def test_is_none_guard(x: int | None) -> str:
    """Test is None in guard"""
    match x:
        case n if n is None:
            return "none"
        case _:
            return "value"


def test_is_not_guard(x: int | None) -> str:
    """Test is not in guard"""
    match x:
        case n if n is not None:
            return "has value"
        case _:
            return "none"


def test_in_guard(x: int) -> str:
    """Test in operator in guard"""
    match x:
        case n if n in [1, 2, 3]:
            return "small"
        case n if n in [10, 20, 30]:
            return "medium"
        case _:
            return "other"


def test_not_in_guard(x: int) -> str:
    """Test not in operator in guard"""
    match x:
        case n if n not in [0, 1]:
            return "not binary"
        case _:
            return "binary"


def test_chained_comparison_guard(x: int) -> str:
    """Test chained comparisons in guard"""
    match x:
        case n if 0 <= n < 10:
            return "single digit"
        case n if 10 <= n < 100:
            return "double digit"
        case _:
            return "other"


def test_string_comparison_guard(s: str) -> str:
    """Test string comparison in guard"""
    match s:
        case x if x == "hello":
            return "greeting"
        case x if x < "m":
            return "first half"
        case _:
            return "second half"


def test_boolean_guard(x: int) -> str:
    """Test boolean comparison in guard"""
    match x:
        case n if n == True:  # True == 1
            return "one"
        case n if n == False:  # False == 0
            return "zero"
        case _:
            return "other"


def test_mixed_numeric_guard(x: int) -> str:
    """Test mixed int/float comparison in guard"""
    match x:
        case n if n == 5.0:
            return "five"
        case n if n < 2.5:
            return "small"
        case _:
            return "other"


def test_string_in_guard(s: str) -> str:
    """Test substring membership in guard"""
    match s:
        case x if "test" in x:
            return "contains test"
        case _:
            return "no test"
