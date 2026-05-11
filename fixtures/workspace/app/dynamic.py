from __future__ import annotations

from typing import Any


class DynamicRecord:
    def __init__(self) -> None:
        self.name = "record"

    def __getattr__(self, name: str) -> Any:
        return f"dynamic:{name}"


def read_dynamic(record: DynamicRecord, key: str) -> Any:
    return getattr(record, key)


def monkey_patch(record: DynamicRecord) -> None:
    record.extra = 42
