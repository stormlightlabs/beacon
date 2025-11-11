"""
Async context manager fixtures for testing async/await type inference.
Tests coroutine types, async context managers, and async generators.
"""

from typing import AsyncIterator


class AsyncResource:
    """Async context manager for testing"""
    def __init__(self, name: str):
        self.name = name
        self.active = False

    async def __aenter__(self):
        self.active = True
        return self

    async def __aexit__(self, exc_type, exc_val, exc_tb):
        self.active = False
        return False

    async def fetch(self) -> str:
        """Async method returning string"""
        return f"Data from {self.name}"


async def process_resource(name: str) -> str:
    """Test async context manager usage"""
    async with AsyncResource(name) as resource:
        data = await resource.fetch()
        return data


async def async_generator(n: int) -> AsyncIterator[int]:
    """Async generator for testing"""
    for i in range(n):
        yield i


async def consume_generator(n: int) -> list[int]:
    """Test async generator consumption"""
    results = []
    async for item in async_generator(n):
        results.append(item)
    return results


async def chained_async() -> int:
    """Test chained async calls"""
    data = await process_resource("test")
    length = len(data)
    return length
