"""Dataclass decorator and utilities.

This stub provides the dataclass decorator and field function."""

from typing import TypeVar, Any, Callable, overload, Generic

_T = TypeVar("_T")

@overload
def dataclass(cls: type[_T]) -> type[_T]: ...
@overload
def dataclass(
    *,
    init: bool = True,
    repr: bool = True,
    eq: bool = True,
    order: bool = False,
    unsafe_hash: bool = False,
    frozen: bool = False,
    match_args: bool = True,
    kw_only: bool = False,
    slots: bool = False
) -> Callable[[type[_T]], type[_T]]: ...

def field(
    *,
    default: Any = ...,
    default_factory: Callable[[], Any] = ...,
    init: bool = True,
    repr: bool = True,
    hash: bool | None = None,
    compare: bool = True,
    metadata: dict[str, Any] | None = None,
    kw_only: bool = False
) -> Any: ...

class Field:
    """Field metadata for dataclass fields."""
    name: str
    type: type
    default: Any
    default_factory: Callable[[], Any]
    init: bool
    repr: bool
    hash: bool | None
    compare: bool
    metadata: dict[str, Any]
    kw_only: bool

class InitVar(Generic[_T]):
    """Init-only variable for dataclass __init__."""
    ...

def fields(class_or_instance: Any) -> tuple[Field, ...]:
    """Return tuple of Field objects for a dataclass."""
    ...

def asdict(instance: Any, *, dict_factory: type = dict) -> dict[str, Any]:
    """Convert dataclass instance to a dictionary."""
    ...

def astuple(instance: Any, *, tuple_factory: type = tuple) -> tuple[Any, ...]:
    """Convert dataclass instance to a tuple."""
    ...

def make_dataclass(
    cls_name: str,
    fields: list[str | tuple[str, type] | tuple[str, type, Any]],
    *,
    bases: tuple[type, ...] = (),
    namespace: dict[str, Any] | None = None,
    init: bool = True,
    repr: bool = True,
    eq: bool = True,
    order: bool = False,
    unsafe_hash: bool = False,
    frozen: bool = False,
    match_args: bool = True,
    kw_only: bool = False,
    slots: bool = False
) -> type:
    """Dynamically create a dataclass."""
    ...

def is_dataclass(obj: Any) -> bool:
    """Check if an object is a dataclass."""
    ...

class FrozenInstanceError(AttributeError):
    """Exception raised when trying to modify a frozen dataclass."""
    ...
