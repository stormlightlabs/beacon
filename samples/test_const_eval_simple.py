# Test constant expression evaluation - simple cases

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


# Test complex nested expression
def test_complex() -> int:
    """Test complex nested expression"""
    result = -(1 + 2) + (3 * 4) - (10 // 2)
    return result


# Test that overflow is handled
def test_safe_arithmetic() -> int:
    """Test safe arithmetic with reasonable values"""
    x = 1000 * 1000
    y = x + 500
    z = y - 250
    return z
