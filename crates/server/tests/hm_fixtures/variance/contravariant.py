"""
Test contravariant function parameters.
Contravariance: If Dog <: Animal, then Callable[[Animal], R] <: Callable[[Dog], R]
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


# Test contravariant function parameters
def process_animal(f: Callable[[Animal], None]) -> None:
    """Accepts a function that works on any Animal"""
    f(Dog())


def handle_dog(d: Dog) -> None:
    """More specific: requires Dog"""
    d.fetch()


# ERROR: contravariance violation
# handle_dog is Callable[[Dog], None]
# but we need Callable[[Animal], None]
process_animal(handle_dog)  # Should error: Dog-specific function in Animal position


def handle_any_animal(a: Animal) -> None:
    """Generic: works on any Animal"""
    a.speak()


# OK: contravariant
process_animal(handle_any_animal)  # OK: Animal handler in Animal position


# Test with return type (covariant)
def process_with_return(f: Callable[[Animal], Dog]) -> Dog:
    """Parameters contravariant, return type covariant"""
    return f(Dog())


def animal_to_dog(a: Animal) -> Dog:
    return Dog()


process_with_return(animal_to_dog)  # OK: exact match


def dog_to_animal(d: Dog) -> Animal:
    return d


# Should error: parameter variance violation
process_with_return(dog_to_animal)
