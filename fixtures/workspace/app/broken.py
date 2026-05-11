# beacon: mode=strict

from __future__ import annotations

from .models import Job, User
from .missing import MissingThing


def bad_assignment() -> int:
    return "not an int"


def bad_import_use(value: MissingThing) -> None:
    print(value)


def missing_attribute(job: Job[str]) -> str:
    return job.owner.email


def wrong_constructor() -> User:
    return User(id="bad", name=123)
