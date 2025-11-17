# Test dict and set literal evaluation in constant expressions

def test_dict_in_match(x: dict[str, int]) -> str:
    """Test dict literals in match patterns"""
    match x:
        case {"a": 1, "b": 2}:
            return "matched dict"
        case {}:
            return "empty dict"
        case _:
            return "other"


def test_set_in_match(x: set[int]) -> str:
    """Test set literals in match patterns"""
    match x:
        case {1, 2, 3}:
            return "matched set"
        case _:
            return "other"


def test_dict_with_expressions(value: int) -> str:
    """Test dict with constant expressions as keys"""
    d = {
        -1: "negative one",
        2 + 3: "five",
        10 // 2: "also five",
    }
    match value:
        case -1 if value in d:
            return "found negative one"
        case 5 if value in d:
            return "found five"
        case _:
            return "not found"


def test_set_deduplication() -> set[int]:
    """Test that set literals deduplicate"""
    s: set[int] = {1, 2, 1, 3, 2}  # Should be {1, 2, 3}
    return s


def test_dict_key_membership(key: str) -> bool:
    """Test 'in' operator with dict"""
    config = {
        "host": "localhost",
        "port": 8080,
        "debug": True,
    }
    return key in config


def test_set_membership(item: int) -> bool:
    """Test 'in' operator with set"""
    valid_codes = {200, 201, 204}
    return item in valid_codes


def test_empty_dict_bool() -> bool:
    """Test truthiness of empty dict"""
    empty = {}
    if not empty:
        return True
    return False


def test_empty_set_bool() -> bool:
    """Test truthiness of empty set"""
    # Note: set() in real Python, but we test AST representation
    empty: set[int] = set()
    if not empty:
        return True
    return False


def test_dict_equality() -> bool:
    """Test dict equality is order-independent"""
    d1 = {"a": 1, "b": 2}
    d2 = {"b": 2, "a": 1}
    return d1 == d2


def test_set_equality() -> bool:
    """Test set equality is order-independent"""
    s1 = {1, 2, 3}
    s2 = {3, 1, 2}
    return s1 == s2


def test_nested_structures() -> dict[tuple[int, int], str]:
    """Test tuples as dict keys"""
    coordinates = {
        (0, 0): "origin",
        (1, 0): "right",
        (0, 1): "up",
    }
    return coordinates


def test_mixed_type_keys() -> dict[str | int, str]:
    """Test dict with mixed type keys"""
    mixed = {
        "name": "value",
        42: "answer",
        -1: "negative",
    }
    return mixed
