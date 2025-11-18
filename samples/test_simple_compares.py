# Simple tests for comparison operations

def test_equality() -> bool:
    """Test == operator"""
    return 5 == 5


def test_not_equal() -> bool:
    """Test != operator"""
    return 5 != 3


def test_less_than() -> bool:
    """Test < operator"""
    return 3 < 5


def test_less_or_equal() -> bool:
    """Test <= operator"""
    return 5 <= 5


def test_greater_than() -> bool:
    """Test > operator"""
    return 10 > 5


def test_greater_or_equal() -> bool:
    """Test >= operator"""
    return 5 >= 5


def test_is_none() -> bool:
    """Test is operator"""
    return None is None


def test_is_not() -> bool:
    """Test is not operator"""
    return 5 is not None


def test_in_list() -> bool:
    """Test in operator"""
    return 2 in [1, 2, 3]


def test_not_in_list() -> bool:
    """Test not in operator"""
    return 5 not in [1, 2, 3]


def test_chained() -> bool:
    """Test chained comparison"""
    return 1 < 2 < 3


def test_string_in() -> bool:
    """Test string membership"""
    return "ab" in "abcd"


def test_mixed_numeric() -> bool:
    """Test mixed int/float"""
    return 5 == 5.0
