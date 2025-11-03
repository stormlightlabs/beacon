"""
Test cases for exhaustiveness checking

This file tests type elimination tracking and remaining type computation
through successive narrowing in if-elif-else chains.
"""


def test_basic_exhaustiveness():
    """Test that successive narrowing computes correct remaining types"""
    x: int | str | None = get_value()

    if x is None:
        # x is None here
        pass
    elif isinstance(x, int):
        # x is int here
        result = x + 10  # Should not error
    else:
        # x should be narrowed to str (only remaining type)
        result = x.upper()  # Should not error


def test_three_way_exhaustive_split():
    """Test that three-way split properly narrows to remaining type"""
    value: int | str | None = get_value()

    if value is None:
        none_result = "was none"
    elif isinstance(value, int):
        int_result = value.bit_length()
    else:
        # value is str here (only remaining option)
        str_result = value.strip()


def test_partial_exhaustiveness():
    """Test partial exhaustiveness with multiple remaining types"""
    x: int | str | float | None = get_complex_value()

    if x is None:
        pass
    elif isinstance(x, int):
        pass
    else:
        # x should be str | float here
        # Both should be acceptable
        pass


def test_exhaustiveness_with_isinstance_union():
    """Test exhaustiveness when isinstance checks multiple types"""
    x: int | str | float = get_numeric_or_string()

    if isinstance(x, (int, float)):
        # x is int | float
        numeric = abs(x)
    else:
        # x is str (only remaining)
        text = x.upper()


def test_nested_exhaustiveness():
    """Test exhaustiveness in nested if statements"""
    x: int | str | None = get_value()

    if x is not None:
        # x is int | str here
        if isinstance(x, int):
            # x is int
            result = x + 1
        else:
            # x is str (only remaining)
            result = x.upper()


def test_exhaustiveness_with_truthiness():
    """Test exhaustiveness using truthiness checks"""
    x: str | None = get_optional_string()

    if x:
        # x is str (truthy, excludes None)
        upper = x.upper()
    else:
        # x is None (only remaining falsy option)
        pass


def test_elif_chain_exhaustiveness():
    """Test long elif chain with exhaustive coverage"""
    status: int = get_status_code()

    if status == 200:
        message = "OK"
    elif status == 404:
        message = "Not Found"
    elif status == 500:
        message = "Server Error"
    else:
        # status is any other int
        message = f"Unknown: {status}"


def test_union_with_many_types():
    """Test exhaustiveness with larger union"""
    value: int | str | float | bool | None = get_diverse_value()

    if value is None:
        pass
    elif isinstance(value, bool):
        # Note: bool check must come before int since bool is subtype of int in Python
        pass
    elif isinstance(value, int):
        pass
    elif isinstance(value, float):
        pass
    else:
        # value is str (only remaining)
        result = value.upper()


def test_isinstance_with_remaining_computation():
    """Test that isinstance correctly eliminates checked types"""
    x: int | str | float | None = get_complex_value()

    if isinstance(x, int):
        int_op = x + 1
    elif isinstance(x, str):
        str_op = x.upper()
    else:
        # x should be float | None
        # Both should be acceptable
        pass


def test_exhaustiveness_preserves_narrowing():
    """Test that narrowing within branches still works after exhaustiveness"""
    x: int | str | None = get_value()

    if x is None:
        pass
    elif isinstance(x, int):
        # Further narrowing within this branch
        if x > 0:
            positive = x
        else:
            non_positive = x
    else:
        # x is str
        if x:
            non_empty = x.upper()


def test_match_integration_with_exhaustiveness():
    """Test that pattern matching works with exhaustiveness tracking"""
    value: int | str = get_value_no_none()

    match value:
        case int(x):
            # x is int
            result = x + 1
        case str(s):
            # s is str
            result = s.upper()


def test_multiple_variables_exhaustiveness():
    """Test exhaustiveness tracking for multiple variables"""
    x: int | str | None = get_value()
    y: float | bool | None = get_other_value()

    if x is None:
        x_result = "none"
    elif isinstance(x, int):
        x_result = x + 1
    else:
        x_result = x.upper()  # x is str

    if y is None:
        y_result = "none"
    elif isinstance(y, float):
        y_result = y * 2
    else:
        y_result = not y  # y is bool


def test_exhaustiveness_with_complex_predicate():
    """Test exhaustiveness with complex boolean predicates"""
    x: int | str | None = get_value()

    if x is not None and isinstance(x, int):
        # x is int
        result = x + 1
    elif x is not None and isinstance(x, str):
        # x is str
        result = x.upper()
    else:
        # x is None
        pass


# Helper functions
def get_value() -> int | str | None:
    ...

def get_complex_value() -> int | str | float | None:
    ...

def get_numeric_or_string() -> int | str | float:
    ...

def get_optional_string() -> str | None:
    ...

def get_status_code() -> int:
    ...

def get_diverse_value() -> int | str | float | bool | None:
    ...

def get_value_no_none() -> int | str:
    ...

def get_other_value() -> float | bool | None:
    ...
