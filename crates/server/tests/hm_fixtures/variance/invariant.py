"""
Test invariant mutable containers.
Invariance: list[Dog] is not a subtype of list[Animal], even though Dog <: Animal
"""

from typing import List


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


# Test invariant mutable containers
dogs: List[Dog] = [Dog()]

# ERROR: list is invariant
# If this were allowed, we could do:
#   animals.append(Cat())  # Cat is an Animal
#   dog = dogs[0]  # But dogs list now contains a Cat!
animals: List[Animal] = dogs  # Should error: invariance violation


# Test with dict (also invariant)
dog_dict: dict[str, Dog] = {"buddy": Dog()}
animal_dict: dict[str, Animal] = dog_dict  # Should error: dict is invariant


# Test with set (also invariant)
dog_set: set[Dog] = {Dog()}
animal_set: set[Animal] = dog_set  # Should error: set is invariant


# OK: exact type match
dogs2: List[Dog] = [Dog()]
dogs3: List[Dog] = dogs2  # OK: same type


# OK: create new list with correct type
animals_copy: List[Animal] = [Dog(), Cat()]  # OK: elements are Animals
