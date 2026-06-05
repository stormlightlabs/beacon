"""Data-model fixture for dataclasses, transforms, enums, TypedDict, and decorators."""

from __future__ import annotations

from dataclasses import dataclass, field
from enum import Enum
from typing import Any, ClassVar, Final, TypedDict, cast, dataclass_transform, override


@dataclass_transform()
def model(cls: Any) -> Any:
    return cls


class Config(TypedDict):
    name: str
    retries: int


class State(Enum):
    READY = "ready"
    DONE = "done"


@dataclass(slots=True)
class Job:
    id: int
    name: str
    config: Config
    tags: list[str] = field(default_factory=list)
    queue: ClassVar[str] = "default"
    max_retries: Final[int] = 3

    @property
    def label(self) -> str:
        return f"{self.id}:{self.name}"


class BaseFormatter:
    def format(self, job: Job) -> str:
        return job.name


class LabelFormatter(BaseFormatter):
    @override
    def format(self, job: Job) -> str:
        return job.label


@model
class DynamicModel:
    name: str


def build_config(name: str) -> Config:
    return cast(Config, {"name": name, "retries": 3})


def summarize(job: Job, state: State) -> str:
    formatter = LabelFormatter()
    return f"{formatter.format(job)}:{state.value}:{job.config['retries']}"


config = build_config("sync")
job = Job(id=1, name="sync", config=config)
summary = summarize(job, State.READY)
