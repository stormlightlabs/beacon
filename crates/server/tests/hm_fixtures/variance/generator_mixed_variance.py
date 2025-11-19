"""
Test Generator, AsyncGenerator, and Coroutine with mixed variance.

Generator[YieldType, SendType, ReturnType]:
  - YieldType: covariant
  - SendType: contravariant
  - ReturnType: covariant

AsyncGenerator[YieldType, SendType]:
  - YieldType: covariant
  - SendType: contravariant

Coroutine[YieldType, SendType, ReturnType]:
  - YieldType: covariant
  - SendType: contravariant
  - ReturnType: covariant
"""

from typing import Generator, AsyncGenerator, Coroutine


class Animal:
    def speak(self) -> str:
        return "..."


class Dog(Animal):
    def bark(self) -> str:
        return "woof"


# Generator tests
def animal_generator() -> Generator[Animal, Dog, str]:
    """Yields Animal (covariant), receives Dog (contravariant), returns str (covariant)."""
    received = yield Animal()
    return "done"


def dog_generator() -> Generator[Dog, Animal, str]:
    """Yields Dog, receives Animal, returns str."""
    received = yield Dog()
    return "done"


def use_animal_generator(gen: Generator[Animal, Dog, str]) -> str:
    """Consumer expects Generator[Animal, Dog, str]."""
    return "ok"


# This should work: Dog-yielding generator is assignable to Animal-yielding position (covariance)
result1 = use_animal_generator(dog_generator())

# This should work: Generator[Dog, Animal, str] is a subtype of Generator[Animal, Dog, str]
# because Dog <: Animal (covariant in yield), Animal >: Dog (contravariant in send)
animal_gen: Generator[Animal, Dog, str] = dog_generator()


# AsyncGenerator tests
async def animal_async_gen() -> AsyncGenerator[Animal, Dog]:
    """Yields Animal (covariant), receives Dog (contravariant)."""
    received = yield Animal()


async def dog_async_gen() -> AsyncGenerator[Dog, Animal]:
    """Yields Dog, receives Animal."""
    received = yield Dog()


async def use_animal_async_gen(gen: AsyncGenerator[Animal, Dog]) -> None:
    """Consumer expects AsyncGenerator[Animal, Dog]."""
    pass


# This should work: Dog-yielding async generator is assignable to Animal-yielding position
async def test_async_gen() -> None:
    await use_animal_async_gen(dog_async_gen())


# Coroutine tests
async def animal_coroutine() -> Coroutine[Animal, Dog, str]:
    """Similar mixed variance as Generator."""
    return "done"


async def dog_coroutine() -> Coroutine[Dog, Animal, str]:
    """Yields Dog, receives Animal, returns str."""
    return "done"


async def use_animal_coroutine(coro: Coroutine[Animal, Dog, str]) -> str:
    """Consumer expects Coroutine[Animal, Dog, str]."""
    return "ok"


# This should work: Dog-yielding coroutine is assignable to Animal-yielding position
async def test_coroutine() -> None:
    result = await use_animal_coroutine(dog_coroutine())
