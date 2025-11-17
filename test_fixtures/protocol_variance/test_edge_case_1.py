# Edge case 1: Protocol class instances not satisfying protocol types with type arguments
# e.g., MyClass() not satisfying MyProtocol[str]

from typing import Protocol, TypeVar

T_co = TypeVar('T_co', covariant=True)

class ReadOnly(Protocol[T_co]):
    def get(self) -> T_co:
        ...

# Concrete class that implements ReadOnly protocol with str
class MyStringReader:
    def get(self) -> str:
        return "hello"

def process_reader(reader: ReadOnly[str]) -> str:
    return reader.get()

# This should work: MyStringReader() satisfies ReadOnly[str]
reader = MyStringReader()
result = process_reader(reader)  # Should not error
