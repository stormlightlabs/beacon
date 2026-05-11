from __future__ import annotations

from collections.abc import Callable
from typing import Any, TypeGuard

from .models import Job, JobStatus, User, first, normalize_name
from .protocols import JsonSink, SupportsClose
from .subpkg.reexports import public_label
from slugify import slugify


def is_user(value: object) -> TypeGuard[User]:
    return isinstance(value, User)


def load_owner(value: object) -> User | None:
    if is_user(value):
        return value
    return None


def build_summary(job: Job[str]) -> str:
    name = normalize_name(job.owner.name)
    fallback = first(job.owner.tags, "untagged")
    return f"{name}:{fallback}:{public_label(job.status.value)}"


def close_all(resources: list[SupportsClose]) -> None:
    for resource in resources:
        resource.close()


def write_all(sink: JsonSink[dict[str, Any]], jobs: list[Job[str]]) -> None:
    for job in jobs:
        sink.write({"owner": slugify(job.owner.name), "payload": job.payload})


def status_handler(status: JobStatus) -> Callable[[Job[str]], bool]:
    def matches(job: Job[str]) -> bool:
        return job.status is status

    return matches
