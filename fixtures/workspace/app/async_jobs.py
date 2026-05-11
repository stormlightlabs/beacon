from __future__ import annotations

from collections.abc import AsyncIterator, Iterator
from contextlib import asynccontextmanager, contextmanager

from .models import Job, User


async def fetch_job(owner: User) -> Job[str]:
    return Job(owner=owner, payload="ready")


async def job_stream(owner: User) -> AsyncIterator[Job[str]]:
    yield await fetch_job(owner)


def payloads(jobs: list[Job[str]]) -> Iterator[str]:
    for job in jobs:
        yield job.payload


@contextmanager
def open_user(name: str) -> Iterator[User]:
    yield User(id=1, name=name)


@asynccontextmanager
async def open_job(owner: User) -> AsyncIterator[Job[str]]:
    yield await fetch_job(owner)
