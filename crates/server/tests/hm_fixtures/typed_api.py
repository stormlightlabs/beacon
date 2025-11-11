"""
Typed API fixtures for testing Protocol, TypedDict, and TypeVar bounds.
Tests structural typing and bounded polymorphism.
"""

from typing import Protocol, TypedDict, TypeVar


class Drawable(Protocol):
    """Protocol for drawable objects"""
    def draw(self) -> str:
        ...


class Circle:
    """Concrete implementation of Drawable"""
    def __init__(self, radius: float):
        self.radius = radius

    def draw(self) -> str:
        return f"Circle(radius={self.radius})"


class Rectangle:
    """Another concrete implementation of Drawable"""
    def __init__(self, width: float, height: float):
        self.width = width
        self.height = height

    def draw(self) -> str:
        return f"Rectangle({self.width}x{self.height})"


def render(shape: Drawable) -> str:
    """Function accepting any Drawable - tests Protocol"""
    return shape.draw()


# TypedDict tests
class UserDict(TypedDict):
    name: str
    age: int
    email: str


def create_user(name: str, age: int, email: str) -> UserDict:
    """Create a typed dict"""
    return {"name": name, "age": age, "email": email}


def get_user_name(user: UserDict) -> str:
    """Extract name from typed dict"""
    return user["name"]


# Bounded TypeVar tests
class Comparable(Protocol):
    def __lt__(self, other) -> bool:
        ...


T_Comparable = TypeVar('T_Comparable', bound=Comparable)


def max_value(x: T_Comparable, y: T_Comparable) -> T_Comparable:
    """Generic max with comparable bound"""
    return x if x > y else y


# Test cases
circle = Circle(5.0)
rect = Rectangle(10.0, 20.0)
circle_str = render(circle)
rect_str = render(rect)

user = create_user("Alice", 30, "alice@example.com")
name = get_user_name(user)

max_int = max_value(10, 20)
max_str = max_value("apple", "banana")
