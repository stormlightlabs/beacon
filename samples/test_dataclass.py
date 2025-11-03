"""Test file for @dataclass decorator support."""

from dataclasses import dataclass


@dataclass
class Point:
    """A simple 2D point with x and y coordinates."""
    x: int
    y: int


@dataclass
class Person:
    """A person with name, age, and optional email."""
    name: str
    age: int
    email: str = "unknown@example.com"


@dataclass
class Rectangle:
    """A rectangle defined by two points."""
    top_left: Point
    bottom_right: Point
    color: str = "black"


# Test instantiation with correct types
def test_dataclass_instantiation():
    # Should work: correct types
    p1 = Point(10, 20)
    p2 = Point(x=5, y=15)

    # Field access should work
    x_coord = p1.x
    y_coord = p2.y

    # Should work: Person with all required fields
    person1 = Person("Alice", 30)
    person2 = Person("Bob", 25, "bob@example.com")

    # Field access
    name = person1.name
    age = person2.age
    email = person1.email

    # Should work: nested dataclasses
    rect = Rectangle(Point(0, 0), Point(100, 100))
    rect2 = Rectangle(Point(10, 10), Point(50, 50), "red")

    # Field access on nested
    top_x = rect.top_left.x
    color = rect2.color

    return p1, person1, rect


# Test type errors
def test_dataclass_type_errors():
    # Should error: wrong type for x (str instead of int)
    bad_point1 = Point("10", 20)

    # Should error: wrong type for y (float instead of int)
    bad_point2 = Point(10, 20.5)

    # Should error: missing required argument
    bad_person1 = Person("Alice")

    # Should error: wrong type for age (str instead of int)
    bad_person2 = Person("Bob", "25")

    # Should error: wrong nested type
    bad_rect = Rectangle("not a point", Point(10, 10))

    return bad_point1, bad_person1, bad_rect


# Test field access type inference
def test_field_access():
    p = Point(5, 10)

    # x and y should be inferred as int
    x = p.x
    y = p.y

    # Should work: int operations
    sum_coords = x + y

    # Should error: string operations on int
    upper_x = x.upper()

    return sum_coords


# Test dataclass with explicit __init__ (should not be overridden)
@dataclass
class CustomInit:
    """Dataclass with custom __init__."""
    value: int

    def __init__(self, value: int, multiplier: int = 2):
        """Custom init that multiplies the value."""
        self.value = value * multiplier


def test_custom_init():
    # Should use custom __init__ signature
    c = CustomInit(10)
    c2 = CustomInit(10, 3)

    return c, c2


if __name__ == "__main__":
    test_dataclass_instantiation()
    test_dataclass_type_errors()
    test_field_access()
    test_custom_init()
