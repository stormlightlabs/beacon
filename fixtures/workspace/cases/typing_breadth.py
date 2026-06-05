"""Typing-breadth contract fixture for v1 checker smoke coverage."""

from __future__ import annotations

from collections.abc import AsyncIterator, Callable, Iterator
from contextlib import asynccontextmanager, contextmanager
from dataclasses import dataclass, field
from enum import Enum
from typing import ClassVar, Concatenate, Final, Generic, ParamSpec, Protocol, TypeVar, TypeVarTuple, Unpack, overload

T = TypeVar("T")
U = TypeVar("U")
NumberT = TypeVar("NumberT", int, float)
P = ParamSpec("P")
Ts = TypeVarTuple("Ts")


class Status(Enum):
    PENDING = "pending"
    DONE = "done"


class SupportsName(Protocol):
    name: str


class Reader(Protocol[T]):
    def read(self) -> T:
        ...


class Callback(Protocol[P, T]):
    def __call__(self, *args: P.args, **kwargs: P.kwargs) -> T:
        ...


@dataclass(frozen=True)
class User:
    id: int
    name: str
    tags: list[str] = field(default_factory=list)
    kind: ClassVar[str]
    max_tags: Final[int]

    @property
    def display_name(self) -> str:
        return self.name.title()


class Box(Generic[T]):
    def __init__(self, value: T) -> None:
        self.value = value

    def get(self) -> T:
        return self.value


class Packed(Generic[Unpack[Ts]]):
    values: tuple[Unpack[Ts]]

    def __init__(self, *values: Unpack[Ts]) -> None:
        self.values = values


def identity(value: T) -> T:
    return value


def choose(left: T, right: T | None = None) -> T:
    if right is None:
        return left
    return right


def add_numbers(left: NumberT, right: NumberT) -> NumberT:
    return left + right


@overload
def stringify(value: int) -> str:
    ...


def stringify(value: int) -> str:
    return str(value)


def with_user(func: Callable[Concatenate[User, P], T]) -> Callable[P, T]:
    def wrapper(*args: P.args, **kwargs: P.kwargs) -> T:
        return func(User(id=1, name="system"), *args, **kwargs)

    return wrapper


@with_user
def label(user: User, prefix: str) -> str:
    return f"{prefix}:{user.display_name}"


def read_name(reader: Reader[User]) -> str:
    return reader.read().name


def containers(user: User) -> tuple[list[str], dict[str, int], set[Status]]:
    names = user.tags
    counts = {tag: len(tag) for tag in user.tags}
    statuses = {Status.PENDING, Status.DONE}
    return names, counts, statuses


def unpack_pair(pair: tuple[T, U]) -> U:
    _, value = pair
    return value


async def fetch_user(name: str) -> User:
    return User(id=1, name=name)


async def stream_users(names: list[str]) -> AsyncIterator[User]:
    for name in names:
        yield await fetch_user(name)


def user_names(users: list[User]) -> Iterator[str]:
    for user in users:
        yield user.name


@contextmanager
def open_box(value: T) -> Iterator[Box[T]]:
    yield Box(value)


@asynccontextmanager
async def open_user(name: str) -> AsyncIterator[User]:
    yield await fetch_user(name)


def operator_results(user: User) -> tuple[bool, str, int]:
    return bool(user.tags), user.name + "!", len(user.tags)


int_value = identity(1)
str_value = identity("one")
boxed_user = Box(User(id=1, name="Ada"))
packed = Packed()
labelled = label("user")
