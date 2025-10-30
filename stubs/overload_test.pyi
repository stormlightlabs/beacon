"""Test stub file for @overload functionality."""

from typing import overload

class Converter:
    """A class with overloaded methods for testing."""

    @overload
    def convert(self, value: int) -> str: ...

    @overload
    def convert(self, value: str) -> int: ...

    def convert(self, value): ...

    @overload
    def process(self, x: int, y: int) -> int: ...

    @overload
    def process(self, x: str, y: str) -> str: ...

    def process(self, x, y): ...
