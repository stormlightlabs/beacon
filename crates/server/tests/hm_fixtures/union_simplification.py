"""Test cases for union type simplification with subtype elimination."""

from typing import Union, Never

# Test 1: Union with Never should eliminate Never
def test_union_with_never(x: Union[int, Never]) -> Union[int, Never]:
    return x

# Test 2: Union with Any should simplify to Any
def test_union_with_any(x: Union[int, str]) -> Union[int, str]:
    return x

# Test 3: Nested unions should flatten and simplify
def test_nested_unions(x: Union[Union[int, str], Union[bool, float]]) -> Union[int, str, bool, float]:
    return x

# Test 4: Duplicate types should be removed
def test_duplicate_types(x: Union[int, int, str, str]) -> Union[int, str]:
    return x

# Test 5: Optional simplification
def test_optional_int(x: Union[int, None]) -> Union[int, None]:
    return x

# Test 6: Complex nested case
def test_complex_nested(x: Union[Union[int, Never], Union[str, int]]) -> Union[int, str]:
    return x
