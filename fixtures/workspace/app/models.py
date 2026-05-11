from __future__ import annotations

from dataclasses import dataclass, field
from enum import Enum
from typing import ClassVar, Final, Generic, Iterable, TypeVar, overload

T = TypeVar("T")

DEFAULT_LIMIT: Final = 100


class JobStatus(Enum):
    QUEUED = "queued"
    RUNNING = "running"
    DONE = "done"


@dataclass(frozen=True)
class User:
    id: int
    name: str
    tags: list[str] = field(default_factory=list)
    kind: ClassVar[str] = "user"


@dataclass
class Job(Generic[T]):
    owner: User
    payload: T
    status: JobStatus = JobStatus.QUEUED


def identity(value: T) -> T:
    return value


def first(values: Iterable[T], fallback: T) -> T:
    for value in values:
        return value
    return fallback


@overload
def normalize_name(value: str) -> str:
    ...


@overload
def normalize_name(value: None) -> None:
    ...


def normalize_name(value: str | None) -> str | None:
    if value is None:
        return None
    return value.strip().title()
