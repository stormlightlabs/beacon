"""Enumeration types.

This stub provides Enum classes and utilities."""

from typing import TypeVar, Generic, Any, Iterator

_T = TypeVar("_T")

class Enum:
    """Base class for creating enumerated constants."""

    name: str
    value: Any

    def __init__(self, value: Any) -> None: ...

    @classmethod
    def _missing_(cls, value: object) -> Any: ...

    def __repr__(self) -> str: ...
    def __str__(self) -> str: ...
    def __hash__(self) -> int: ...
    def __eq__(self, other: object) -> bool: ...
    def __ne__(self, other: object) -> bool: ...

class IntEnum(int, Enum):
    """Enum where members are also integers."""
    value: int

class StrEnum(str, Enum):
    """Enum where members are also strings."""
    value: str

class Flag(Enum):
    """Support for flags (bitwise operations)."""

    def __or__(self, other: Flag) -> Flag: ...
    def __and__(self, other: Flag) -> Flag: ...
    def __xor__(self, other: Flag) -> Flag: ...
    def __invert__(self) -> Flag: ...

class IntFlag(int, Flag):
    """Flag where members are also integers."""
    value: int

class auto:
    """Automatic value assignment for enum members."""

    def __init__(self) -> None: ...
    value: int

def unique(enumeration: type[_T]) -> type[_T]:
    """Decorator ensuring unique enum values."""
    ...

class EnumMeta(type):
    """Metaclass for Enum."""

    def __iter__(cls) -> Iterator[Enum]: ...
    def __len__(cls) -> int: ...
    def __contains__(cls, member: object) -> bool: ...
    def __getitem__(cls, name: str) -> Enum: ...
