"""
Test cases for integrating pattern matching with flow-sensitive narrowing

We ensure that:
1. Pattern matches create narrowing scopes
2. Pattern bindings are tracked in control flow context
3. Subsequent narrowing within match case bodies works correctly
4. Nested patterns properly refine types
"""


def test_basic_pattern_narrowing():
    """Test that pattern matching narrows the subject variable type"""
    value: int | str = get_value()

    match value:
        case int(x):
            # value should be narrowed to int here
            # x is bound as int
            result = value + 10  # Should not error
            _ = x.bit_length()   # Should not error
        case str(s):
            # value should be narrowed to str here
            # s is bound as str
            result = value.upper()  # Should not error
            _ = s.strip()           # Should not error


def test_pattern_with_subsequent_narrowing():
    """Test that narrowing works after pattern matching"""
    value: int | str | None = get_optional_value()

    match value:
        case int(x):
            # x is narrowed to int by the pattern
            # Further narrowing should work within this case
            if x > 0:
                # x is still int, and we can use int-specific operations
                positive = x.bit_length()
        case str(s):
            # s is narrowed to str by the pattern
            if s:  # Truthiness check
                # s is still str (non-empty)
                uppercase = s.upper()
        case None:
            # value is None here
            pass


def test_class_pattern_narrowing():
    """Test that class patterns narrow to the specific class type"""

    class Point:
        x: int
        y: int

    class Circle:
        radius: float

    shape: Point | Circle = get_shape()

    match shape:
        case Point(x, y):
            # shape should be narrowed to Point
            # x and y are bound as int
            coords = (x, y)
            _ = shape.x  # Should not error - shape is Point
        case Circle(r):
            # shape should be narrowed to Circle
            # r is bound as float
            area = 3.14 * r * r
            _ = shape.radius  # Should not error - shape is Circle


def test_sequence_pattern_narrowing():
    """Test that sequence patterns narrow to sequence types"""
    value: list[int] | str = get_sequence()

    match value:
        case [first, *rest]:
            # value is narrowed to list[int]
            # first is int, rest is list[int]
            _ = first + 1
            _ = rest.append(42)
        case str():
            # value is narrowed to str
            _ = value.upper()


def test_mapping_pattern_narrowing():
    """Test that mapping patterns narrow to dict types"""
    data: dict[str, int] | list[int] = get_data()

    match data:
        case {"key": value}:
            # data is narrowed to dict[str, int]
            # value is bound as int
            _ = value + 1
            _ = data.keys()  # Should not error
        case [first, *rest]:
            # data is narrowed to list[int]
            _ = data.append(42)  # Should not error


def test_nested_pattern_narrowing():
    """Test that nested patterns properly refine types"""

    class Container:
        value: int | str

    obj: Container | None = get_container()

    match obj:
        case Container(value=int(x)):
            # obj is narrowed to Container
            # value within obj is narrowed to int
            # x is bound as int
            _ = obj.value + 10  # obj.value should be int
            _ = x.bit_length()
        case Container(value=str(s)):
            # obj is narrowed to Container
            # value within obj is narrowed to str
            # s is bound as str
            _ = obj.value.upper()  # obj.value should be str
            _ = s.strip()
        case None:
            pass


def test_pattern_with_guard_and_narrowing():
    """Test that guards and narrowing work together"""
    value: int | str | None = get_value_optional()

    match value:
        case int(x) if x > 0:
            # x is int and we know x > 0 from the guard
            positive = x.bit_length()
        case str(s) if s:
            # s is str and non-empty from the guard
            uppercase = s.upper()
        case _:
            pass


def test_or_pattern_narrowing():
    """Test that OR patterns create union types correctly"""
    value: int | str | float = get_numeric_or_string()

    match value:
        case int() | float():
            # value is narrowed to int | float
            # We should be able to use numeric operations common to both
            _ = abs(value)
        case str():
            # value is narrowed to str
            _ = value.upper()


def test_as_pattern_narrowing():
    """Test that AS patterns capture the narrowed type"""
    value: int | str = get_value()

    match value:
        case int() as num:
            # num should have type int
            _ = num.bit_length()
        case str() as text:
            # text should have type str
            _ = text.upper()


def test_pattern_bindings_persist():
    """Test that pattern bindings are available throughout the case body"""
    data: list[int] = [1, 2, 3, 4, 5]

    match data:
        case [first, second, *rest]:
            # All bindings should be available and properly typed
            _ = first + second
            _ = rest.append(6)

            # And we should be able to do further narrowing on these bindings
            if first > 0:
                positive_first = first


# Helper functions for type annotations
def get_value() -> int | str:
    ...

def get_optional_value() -> int | str | None:
    ...

def get_shape():
    ...

def get_sequence() -> list[int] | str:
    ...

def get_data() -> dict[str, int] | list[int]:
    ...

def get_container():
    ...

def get_value_optional() -> int | str | None:
    ...

def get_numeric_or_string() -> int | str | float:
    ...
