"""Async, generator, coroutine, and context-manager inference fixture."""

from __future__ import annotations

from collections.abc import AsyncIterator, Iterator
from contextlib import asynccontextmanager, contextmanager
from dataclasses import dataclass


@dataclass
class Record:
    name: str


async def fetch_record(name: str) -> Record:
    return Record(name=name)


async def stream_records(names: list[str]) -> AsyncIterator[Record]:
    for name in names:
        yield await fetch_record(name)


def record_names(records: list[Record]) -> Iterator[str]:
    for record in records:
        yield record.name


@contextmanager
def open_record(name: str) -> Iterator[Record]:
    yield Record(name=name)


@asynccontextmanager
async def open_async_record(name: str) -> AsyncIterator[Record]:
    yield await fetch_record(name)


async def collect_first(name: str) -> str:
    record = await fetch_record(name)
    return record.name
