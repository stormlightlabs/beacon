# Edge case 2: Type parameter inference from method signatures
# Need better tracking of which methods use which type variables

from typing import Protocol, TypeVar

T = TypeVar('T')
U = TypeVar('U')

class TwoParam(Protocol[T, U]):
    def method_with_t(self, x: T) -> None:
        ...

    def method_with_u(self) -> U:
        ...

# Concrete class that should satisfy TwoParam[int, str]
class MyTwoParam:
    def method_with_t(self, x: int) -> None:
        pass

    def method_with_u(self) -> str:
        return "result"

def process_two_param(obj: TwoParam[int, str]) -> str:
    obj.method_with_t(42)
    return obj.method_with_u()

# This should work
obj = MyTwoParam()
result = process_two_param(obj)  # Should not error
