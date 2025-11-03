from typing import Callable, Iterable, Protocol, TypeVar, Generic

T = TypeVar("T")


class CacheEntry(Generic[T]):
    key: str
    value: T
    hits: int

    def __init__(self, key: str, value: T, hits: int = ...) -> None: ...

    def record_hit(self) -> None: ...


class DataProvider(Protocol[T]):
    name: str

    def load(self) -> Iterable[T]: ...

    def describe(self, item: T) -> str: ...


class InMemoryProvider(DataProvider[T], Generic[T]):
    name: str

    def __init__(self, name: str, loader: Callable[[], Iterable[T]]) -> None: ...

    def load(self) -> list[T]: ...

    def describe(self, item: T) -> str: ...


SERVICE_REGISTRY: dict[str, DataProvider[object]]

def register_provider(name: str, provider: DataProvider[object]) -> None: ...

def prime_cache(provider: DataProvider[T]) -> list[CacheEntry[T]]: ...
