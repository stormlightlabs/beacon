# Edge case 3: Protocol checking when implementing class is also generic
# e.g., Generic[T] satisfying Protocol[T]

from typing import Protocol, TypeVar, Generic

T = TypeVar('T')
T_co = TypeVar('T_co', covariant=True)

class Container(Protocol[T_co]):
    def get(self) -> T_co:
        ...

# Generic class that satisfies Container protocol
class Box(Generic[T]):
    def __init__(self, value: T) -> None:
        self._value = value

    def get(self) -> T:
        return self._value

def process_container(container: Container[str]) -> str:
    return container.get()

# This should work: Box[str] satisfies Container[str]
box = Box("hello")
result = process_container(box)  # Should not error
