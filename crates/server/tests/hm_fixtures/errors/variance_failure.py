"""
Fixtures that should trigger variance errors.
Tests covariance/contravariance violations.
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


def process_animal_function(f: Callable[[Animal], str]) -> str:
    """Expects a function that works on any Animal"""
    dog = Dog()
    return f(dog)


def dog_specific_function(dog: Dog) -> str:
    """This function is more specific - requires Dog"""
    return dog.fetch()


# This should fail: dog_specific_function is Callable[[Dog], str]
# but we need Callable[[Animal], str]
# Dog is a subtype of Animal, but Callable is contravariant in its argument
result = process_animal_function(dog_specific_function)
