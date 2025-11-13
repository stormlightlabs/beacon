# Test cases for pattern matching validation
# These should NOT produce HM010 (PatternTypeMismatch) errors

def test_int_union(value: int | str) -> str:
    """case int() should match the int part of int | str"""
    match value:
        case int():
            return "integer"
        case str():
            return "string"

def test_str_union(value: str | None) -> str:
    """case str() should match the str part of str | None"""
    match value:
        case str():
            return "string"
        case None:
            return "none"

def test_int_simple(value: int) -> str:
    """case int() should match int type"""
    match value:
        case int():
            return "integer"

def test_bool_union(value: int | bool) -> str:
    """case bool() should match the bool part of int | bool"""
    match value:
        case bool():
            return "boolean"
        case int():
            return "integer"

def test_float_union(value: float | int) -> str:
    """case float() should match the float part of float | int"""
    match value:
        case float():
            return "float"
        case int():
            return "integer"
