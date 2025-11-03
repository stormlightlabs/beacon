"""Test file for Enum support."""

from enum import Enum, IntEnum, StrEnum, auto


class Color(Enum):
    """Basic enum with explicit values."""
    RED = 1
    GREEN = 2
    BLUE = 3


class Status(Enum):
    """Enum using auto() for automatic value assignment."""
    PENDING = auto()
    RUNNING = auto()
    COMPLETED = auto()
    FAILED = auto()


class Priority(IntEnum):
    """IntEnum where members are also integers."""
    LOW = 1
    MEDIUM = 2
    HIGH = 3
    URGENT = 4


class LogLevel(StrEnum):
    """StrEnum where members are also strings."""
    DEBUG = "debug"
    INFO = "info"
    WARNING = "warning"
    ERROR = "error"


# Test enum member access
def test_enum_access():
    # Member access should return enum type
    red = Color.RED
    green = Color.GREEN
    blue = Color.BLUE

    # Should work: enum member has correct type
    color: Color = Color.RED

    # Enum member comparison
    if red == Color.RED:
        print("Correct color")

    # Status enum
    status = Status.PENDING
    running = Status.RUNNING

    # Priority (IntEnum)
    prio = Priority.HIGH
    low = Priority.LOW

    # LogLevel (StrEnum)
    level = LogLevel.ERROR
    debug = LogLevel.DEBUG

    return color, status, prio, level


# Test type errors
def test_enum_type_errors():
    # Should error: assigning enum member to wrong type
    color_as_int: int = Color.RED

    # Should error: comparing enum to non-enum
    is_one = Color.RED == 1

    # Should error: accessing non-existent member
    invalid = Color.YELLOW

    # Should error: wrong enum type
    wrong_enum: Status = Color.RED

    return color_as_int, invalid


# Test enum in function parameters
def process_color(color: Color) -> str:
    """Process a color enum."""
    if color == Color.RED:
        return "red color"
    elif color == Color.GREEN:
        return "green color"
    else:
        return "other color"


def process_status(status: Status) -> bool:
    """Check if status is complete."""
    return status == Status.COMPLETED


def test_enum_parameters():
    # Should work: passing correct enum type
    result1 = process_color(Color.RED)
    result2 = process_color(Color.BLUE)

    is_done = process_status(Status.COMPLETED)
    is_running = process_status(Status.RUNNING)

    # Should error: passing wrong type
    bad_result1 = process_color(1)
    bad_result2 = process_color("red")
    bad_result3 = process_color(Status.PENDING)

    bad_status = process_status(Color.RED)

    return result1, is_done


# Test enum with methods
class Direction(Enum):
    """Enum with methods."""
    NORTH = "N"
    SOUTH = "S"
    EAST = "E"
    WEST = "W"

    def opposite(self) -> "Direction":
        """Get the opposite direction."""
        if self == Direction.NORTH:
            return Direction.SOUTH
        elif self == Direction.SOUTH:
            return Direction.NORTH
        elif self == Direction.EAST:
            return Direction.WEST
        else:
            return Direction.EAST


def test_enum_methods():
    north = Direction.NORTH
    south = north.opposite()

    # Should work: method returns correct type
    direction: Direction = Direction.EAST.opposite()

    return south, direction


# Test IntEnum operations
def test_int_enum():
    high = Priority.HIGH
    medium = Priority.MEDIUM

    # IntEnum should support int operations
    is_greater = high > medium
    sum_prio = high + medium

    # But type should still be IntEnum
    prio: Priority = Priority.HIGH

    return is_greater, sum_prio


if __name__ == "__main__":
    test_enum_access()
    test_enum_type_errors()
    test_enum_parameters()
    test_enum_methods()
    test_int_enum()
