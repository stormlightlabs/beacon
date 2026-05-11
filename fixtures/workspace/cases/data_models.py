"""Dataclass, enum, and container-model fixtures."""

from __future__ import annotations

from dataclasses import dataclass, field
from enum import Enum, IntEnum, StrEnum, auto
from typing import ClassVar, Final, TypedDict


class Status(StrEnum):
    NEW = "new"
    RUNNING = "running"
    DONE = "done"


class Priority(IntEnum):
    LOW = 1
    NORMAL = 2
    HIGH = 3


class TokenKind(Enum):
    IDENTIFIER = auto()
    NUMBER = auto()
    STRING = auto()


class Payload(TypedDict):
    name: str
    count: int
    status: Status


@dataclass(slots=True)
class Job:
    id: int
    name: str
    status: Status = Status.NEW
    tags: list[str] = field(default_factory=list)
    queue: ClassVar[str] = "default"
    max_retries: Final[int] = 3

    @property
    def is_done(self) -> bool:
        return self.status is Status.DONE


def describe_payload(payload: Payload) -> str:
    return f"{payload['name']}:{payload['count']}:{payload['status']}"


def next_priority(priority: Priority) -> Priority:
    match priority:
        case Priority.LOW:
            return Priority.NORMAL
        case Priority.NORMAL:
            return Priority.HIGH
        case Priority.HIGH:
            return Priority.HIGH
