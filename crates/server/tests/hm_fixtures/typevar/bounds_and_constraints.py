"""
Test TypeVar bounds and constraints in Hindley-Milner type inference.

This tests the implementation of:
- TypeVar bounds: T = TypeVar('T', bound=BaseClass)
- TypeVar constraints: T = TypeVar('T', int, str)
"""

from typing import TypeVar, Generic

# Test 1: TypeVar with bound
class Animal:
    def speak(self) -> str:
        return "..."

class Dog(Animal):
    def speak(self) -> str:
        return "Woof"
    def fetch(self) -> str:
        return "Fetching"

T = TypeVar('T', bound=Animal)

def get_sound(animal: T) -> str:
    return animal.speak()

# OK: Dog is a subtype of Animal
dog = Dog()
sound1: str = get_sound(dog)

# OK: Animal satisfies its own bound
animal = Animal()
sound2: str = get_sound(animal)

# ERROR: int is not a subtype of Animal
# num: str = get_sound(42)  # Should error: TypeVar bound violation


# Test 2: TypeVar with multiple constraints
U = TypeVar('U', int, str)

def double(value: U) -> U:
    """Only works with int or str, not subtypes."""
    if isinstance(value, int):
        return value * 2  # type: ignore
    else:
        return value * 2  # type: ignore

# OK: int is in constraints
result1: int = double(5)

# OK: str is in constraints
result2: str = double("hello")

# ERROR: bool is not in constraints (even though bool is subtype of int in Python)
# result3 = double(True)  # Should error: TypeVar constraint violation

# ERROR: float is not in constraints
# result4 = double(3.14)  # Should error: TypeVar constraint violation


# Test 3: TypeVar with bound in generic class
V = TypeVar('V', bound=Animal)

class AnimalShelter(Generic[V]):
    def __init__(self) -> None:
        self.animals: list[V] = []

    def add(self, animal: V) -> None:
        self.animals.append(animal)

    def get_all_sounds(self) -> list[str]:
        return [a.speak() for a in self.animals]

# OK: Dog shelter
dog_shelter: AnimalShelter[Dog] = AnimalShelter()
dog_shelter.add(Dog())

# OK: Animal shelter
animal_shelter: AnimalShelter[Animal] = AnimalShelter()
animal_shelter.add(Animal())

# ERROR: int does not satisfy bound
# int_shelter: AnimalShelter[int] = AnimalShelter()  # Should error


# Test 4: Multiple TypeVars with different constraints
K = TypeVar('K', int, str)
V2 = TypeVar('V2', bound=Animal)

class Registry(Generic[K, V2]):
    def __init__(self) -> None:
        self.items: dict[K, V2] = {}

    def register(self, key: K, value: V2) -> None:
        self.items[key] = value

# OK: str key, Dog value
reg1: Registry[str, Dog] = Registry()
reg1.register("buddy", Dog())

# OK: int key, Animal value
reg2: Registry[int, Animal] = Registry()
reg2.register(1, Animal())

# ERROR: bool key violates K constraints
# reg3: Registry[bool, Dog] = Registry()  # Should error

# ERROR: int value violates V2 bound
# reg4: Registry[str, int] = Registry()  # Should error


# Test 5: TypeVar without bounds or constraints (accepts any type)
W = TypeVar('W')

def identity(x: W) -> W:
    return x

# All of these should work
id1: int = identity(42)
id2: str = identity("hello")
id3: Dog = identity(Dog())
id4: list[int] = identity([1, 2, 3])
