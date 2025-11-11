"""
Test covariant return types and immutable containers.
Covariance: If Dog <: Animal, then Container[Dog] <: Container[Animal]
"""

from typing import Callable


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


# Test covariant return types
def get_dog() -> Dog:
    return Dog()


def get_animal() -> Animal:
    # OK: Dog <: Animal, and return types are covariant
    return get_dog()


# Test covariant containers (tuple is covariant)
dogs: tuple[Dog, ...] = (Dog(),)
animals: tuple[Animal, ...] = dogs  # OK: tuple is covariant


# Test function with covariant return type
def animal_producer() -> Animal:
    return Dog()  # OK: Dog <: Animal


# Test nested covariant types
nested_dogs: tuple[tuple[Dog, ...], ...] = ((Dog(),),)
nested_animals: tuple[tuple[Animal, ...], ...] = nested_dogs  # OK: nested covariance
