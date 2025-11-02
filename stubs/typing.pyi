"""Type hints and typing utilities for Python.

This stub provides core typing constructs used throughout Python code."""

from typing import TypeVar, Generic

_T = TypeVar("_T")
_KT = TypeVar("_KT")
_VT = TypeVar("_VT")
_T_co = TypeVar("_T_co", covariant=True)
_T_contra = TypeVar("_T_contra", contravariant=True)

class Any:
    """Special type indicating an unconstrained type."""
    ...

def cast(typ: type[_T], val: object) -> _T:
    """Cast a value to a type."""
    ...

class Optional(Generic[_T]):
    """Optional type alias for Union[T, None]."""
    ...

class Union:
    """Union type for multiple possible types."""
    ...

class List(Generic[_T]):
    """Generic list type."""
    ...

class Dict(Generic[_KT, _VT]):
    """Generic dictionary type."""
    ...

class Set(Generic[_T]):
    """Generic set type."""
    ...

class Tuple(Generic[_T]):
    """Generic tuple type."""
    ...

class Callable:
    """Callable type for functions."""
    ...

class Type(Generic[_T]):
    """Type of a class."""
    ...

class ClassVar(Generic[_T]):
    """Class variable annotation."""
    ...

class Final(Generic[_T]):
    """Final annotation - cannot be overridden."""
    ...

class Literal:
    """Literal type for specific values."""
    ...

class Protocol:
    """Base class for protocol classes."""
    ...

class TypedDict:
    """Base class for typed dictionaries."""
    ...

class NamedTuple:
    """Base class for named tuples."""
    ...

class Generator(Generic[_T, _T_contra, _T_co]):
    """Generator type."""
    def __iter__(self) -> Generator[_T, _T_contra, _T_co]: ...
    def __next__(self) -> _T: ...
    def send(self, value: _T_contra) -> _T: ...

class AsyncGenerator(Generic[_T, _T_contra]):
    """Async generator type."""
    def __aiter__(self) -> AsyncGenerator[_T, _T_contra]: ...
    def __anext__(self) -> _T: ...
    def asend(self, value: _T_contra) -> _T: ...

class Awaitable(Generic[_T]):
    """Awaitable protocol for async/await."""
    def __await__(self) -> Generator[Any, None, _T]: ...

class Coroutine(Generic[_T, _T_contra, _T_co]):
    """Coroutine type."""
    def __await__(self) -> Generator[_T, _T_contra, _T_co]: ...
    def send(self, value: _T_contra) -> _T: ...

class AsyncIterable(Generic[_T]):
    """Async iterable protocol."""
    def __aiter__(self) -> AsyncIterator[_T]: ...

class AsyncIterator(Generic[_T]):
    """Async iterator protocol."""
    def __anext__(self) -> _T: ...
    def __aiter__(self) -> AsyncIterator[_T]: ...

class Iterable(Generic[_T]):
    """Iterable protocol."""
    def __iter__(self) -> Iterator[_T]: ...

class Iterator(Generic[_T]):
    """Iterator protocol."""
    def __next__(self) -> _T: ...
    def __iter__(self) -> Iterator[_T]: ...

class Sequence(Generic[_T]):
    """Sequence protocol."""
    def __getitem__(self, index: int) -> _T: ...
    def __len__(self) -> int: ...

class Mapping(Generic[_KT, _VT]):
    """Mapping protocol."""
    def __getitem__(self, key: _KT) -> _VT: ...
    def __len__(self) -> int: ...

def overload(func: _T) -> _T:
    """Decorator for function overloads."""
    ...

class NoReturn:
    """Type for functions that never return."""
    ...

def assert_type(val: _T, typ: type[_T]) -> _T:
    """Assert that a value has a specific type."""
    ...

def reveal_type(obj: _T) -> _T:
    """Reveal the type of an expression."""
    ...
