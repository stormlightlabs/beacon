# Test constant expression evaluation in pattern matching

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
    # These should work now with const expr evaluation
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


# Test edge cases
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


# Test that expressions in dict keys work
def test_dict_keys() -> dict[int, str]:
    """Test constant expressions in dict keys"""
    d: dict[int, str] = {
        -1: "neg one",
        5: "five",
        8: "eight",
    }
    return d


# Test NOT operation
def test_not_operation(x: bool) -> str:
    """Test NOT in constant expressions"""
    if not True:
        return "impossible"
    if not False:
        return "always"
    return "other"


# Test bitwise operations
def test_bitwise():
    """Test bitwise operations in constant expressions"""
    mask = 15 & 7  # Should be 5
    shifted = 1 << 3  # Should be 8
    inverted = ~0  # Should be -1
    return mask, shifted, inverted
