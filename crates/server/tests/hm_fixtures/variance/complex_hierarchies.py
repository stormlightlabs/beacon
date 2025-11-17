"""
Test complex generic hierarchies and multi-parameter variance.

This tests the fix for variance unification corner cases in complex generic hierarchies.
Focuses on multi-parameter types like Dict and nested generic containers.
"""

from typing import TypeVar

class Animal:
    def speak(self) -> str:
        return "..."

class Dog(Animal):
    def speak(self) -> str:
        return "Woof"
    def fetch(self) -> str:
        return "Fetching"

# Test multi-parameter type variance (Dict)
dog_dict: dict[str, Dog] = {"buddy": Dog()}
# ERROR: dict is invariant in both parameters
animal_dict: dict[str, Animal] = dog_dict  # Should error: invariance violation

# Test multi-parameter type variance (both parameters)
dog_dog_dict: dict[Dog, Dog] = {Dog(): Dog()}
# ERROR: dict is invariant in key parameter too
animal_dog_dict: dict[Animal, Dog] = dog_dog_dict  # Should error: key invariance

# Test dict with same types (OK)
str_int_dict1: dict[str, int] = {"a": 1}
str_int_dict2: dict[str, int] = str_int_dict1  # OK: same type

# Test deeply nested lists (invariant containers)
deep_list: list[list[list[int]]] = [[[1, 2]], [[3, 4]]]
same_deep_list: list[list[list[int]]] = deep_list  # OK: same type
# ERROR: nested list with different element type
deep_str_list: list[list[list[str]]] = deep_list  # Should error: invariance

# Test dict with nested generics
dict_with_list: dict[str, list[int]] = {"nums": [1, 2, 3]}
same_dict: dict[str, list[int]] = dict_with_list  # OK: same type

# Test dict value variance with nested types
dict_list_int: dict[str, list[int]] = {"nums": [1]}
# ERROR: list[int] != list[str] (invariant)
dict_list_str: dict[str, list[str]] = dict_list_int  # Should error: value type mismatch
