"""
Test TypeVar variance annotations.
Demonstrates covariant and contravariant TypeVars in generic classes.
"""

from typing import TypeVar, Generic


class Animal:
    def speak(self) -> str:
        return "..."


class Dog(Animal):
    def speak(self) -> str:
        return "Woof"

    def fetch(self) -> str:
        return "Fetching"


class Cat(Animal):
    def speak(self) -> str:
        return "Meow"


# Define variance-annotated TypeVars
T_Co = TypeVar('T_Co', covariant=True)
T_Contra = TypeVar('T_Contra', contravariant=True)
T = TypeVar('T')  # invariant by default


# Producer: covariant (only produces values, never consumes)
class Producer(Generic[T_Co]):
    def __init__(self, value: T_Co) -> None:
        self._value = value

    def get(self) -> T_Co:
        return self._value


# Consumer: contravariant (only consumes values, never produces)
class Consumer(Generic[T_Contra]):
    def process(self, item: T_Contra) -> None:
        pass


# Storage: invariant (both reads and writes)
class Storage(Generic[T]):
    def __init__(self) -> None:
        self._items: list[T] = []

    def add(self, item: T) -> None:
        self._items.append(item)

    def get(self) -> T:
        return self._items[0]


# Test covariant TypeVar
dog_producer: Producer[Dog] = Producer(Dog())
animal_producer: Producer[Animal] = dog_producer  # OK: covariant


# Test contravariant TypeVar
animal_consumer: Consumer[Animal] = Consumer()
dog_consumer: Consumer[Dog] = animal_consumer  # OK: contravariant


# Test invariant TypeVar
dog_storage: Storage[Dog] = Storage()
# animal_storage: Storage[Animal] = dog_storage  # ERROR: invariant


# Test with functions
def use_animal_producer(p: Producer[Animal]) -> Animal:
    return p.get()


use_animal_producer(dog_producer)  # OK: Producer[Dog] <: Producer[Animal] (covariant)


def use_dog_consumer(c: Consumer[Dog]) -> None:
    c.process(Dog())


use_dog_consumer(animal_consumer)  # OK: Consumer[Animal] <: Consumer[Dog] (contravariant)


# Test nested variance
nested_producer: Producer[Producer[Dog]] = Producer(dog_producer)
nested_animal: Producer[Producer[Animal]] = nested_producer  # OK: nested covariance
