# Test constant expression evaluation


def test_unary_minus(x: int) -> str:
    """Test unary minus in patterns"""
    match x:
        case -1:
            return "negative one"
        case -42:
            return "negative forty-two"
        case 0:
            return "zero"
        case _:
            return "other"


def test_unary_plus(x: int) -> int:
    """Test unary plus operation"""
    y = +5
    z = +(-3)
    return y + z


def test_not_operation() -> bool:
    """Test NOT in constant expressions"""
    a = not True
    b = not False
    return a or b


def test_bitwise_invert() -> int:
    """Test bitwise invert"""
    x = ~0
    y = ~42
    return x + y


def test_arithmetic() -> int:
    """Test arithmetic operations"""
    a = 2 + 3
    b = 10 - 4
    c = 6 * 7
    d = 15 // 3
    e = 10 % 3
    f = 2 ** 10
    return a + b + c + d + e + f


def test_string_concat() -> str:
    """Test string concatenation"""
    s = "hello" + " " + "world"
    return s


def test_string_repeat() -> str:
    """Test string repetition"""
    s = "ab" * 3
    return s


def test_float_ops() -> float:
    """Test float operations"""
    a = 3.14 + 2.86
    b = 10.5 - 3.5
    c = 2.0 * 3.5
    d = 7.0 / 2.0
    return a + b + c + d


def test_mixed_arithmetic() -> float:
    """Test mixed int/float arithmetic"""
    a = 5 + 2.5
    b = 10.0 - 3
    c = 2 * 3.14
    return a + b + c


def test_bitwise() -> int:
    """Test bitwise operations"""
    a = 12 & 10  # AND
    b = 12 | 10  # OR
    c = 12 ^ 10  # XOR
    d = 1 << 3   # Left shift
    e = 16 >> 2  # Right shift
    return a + b + c + d + e


def test_boolean_ops() -> bool:
    """Test boolean operations"""
    a = True and False
    b = True or False
    return a or b


def test_negative_literal(x: int) -> str:
    """Test that negative literals work in match"""
    match x:
        case -100:
            return "negative hundred"
        case -10:
            return "negative ten"
        case -1:
            return "negative one"
        case 0:
            return "zero"
        case 1:
            return "one"
        case _:
            return "other"


def test_complex() -> int:
    """Test complex nested expression"""
    result = -(1 + 2) + (3 * 4) - (10 // 2)
    return result


def test_safe_arithmetic() -> int:
    """Test safe arithmetic with reasonable values"""
    x = 1000 * 1000
    y = x + 500
    z = y - 250
    return z


def test_arithmetic_patterns(x: int) -> str:
    """Test arithmetic constant expressions in patterns"""
    match x:
        case -1:  # Unary minus
            return "negative one"
        case 2:
            return "two"
        case 3:
            return "three"
        case _:
            return "other"


def test_string_patterns(s: str) -> str:
    """Test string constant expressions"""
    match s:
        case "hello":
            return "greeting"
        case "":
            return "empty"
        case _:
            return "other"


def test_tuple_patterns(t: tuple[int, int] | tuple[()]) -> str:
    """Test tuple literal patterns"""
    match t:
        case (1, 2):
            return "one-two"
        case (3, 4):
            return "three-four"
        case ():
            return "empty"
        case _:
            return "other"


def test_complex_expressions(x: int) -> str:
    """Test that complex expressions in literals are evaluated"""
    y = -42  # Unary minus
    z = 2 ** 3  # Power operation

    match x:
        case -42:  # Should match y
            return "negative"
        case 8:  # Should match z (2**3)
            return "power of two"
        case _:
            return "other"


def test_nested_match(value: int | str) -> str:
    """Test nested patterns with const expressions"""
    match value:
        case -1:
            return "neg one"
        case 0:
            return "zero"
        case "":
            return "empty string"
        case "test":
            return "test string"
        case _:
            return "other"


def test_edge_cases(x: float) -> str:
    """Test float patterns"""
    match x:
        case 0.0:
            return "zero"
        case 3.14:
            return "pi-ish"
        case -1.5:
            return "negative"
        case _:
            return "other"


def test_dict_keys() -> dict[int, str]:
    """Test constant expressions in dict keys"""
    d: dict[int, str] = {
        -1: "neg one",
        5: "five",
        8: "eight",
    }
    return d


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
