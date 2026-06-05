"""Negative protocol fixture with stable user-defined protocol mismatch."""

from __future__ import annotations

from typing import Protocol


class Closeable(Protocol):
    def close(self) -> None:
        ...


class MissingClose:
    pass


def close_it(resource: Closeable) -> None:
    resource.close()


close_it(MissingClose())
