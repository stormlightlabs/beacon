# Edge case 4: Covariant/contravariant/invariant protocol params

from typing import Protocol, TypeVar

T_co = TypeVar('T_co', covariant=True)
T_contra = TypeVar('T_contra', contravariant=True)
T = TypeVar('T')

# Covariant protocol
class Producer(Protocol[T_co]):
    def produce(self) -> T_co:
        ...

# Contravariant protocol
class Consumer(Protocol[T_contra]):
    def consume(self, value: T_contra) -> None:
        ...

# Invariant protocol
class Storage(Protocol[T]):
    def get(self) -> T:
        ...
    def set(self, value: T) -> None:
        ...

class IntProducer:
    def produce(self) -> int:
        return 42

class ObjectConsumer:
    def consume(self, value: object) -> None:
        pass

class StrStorage:
    def get(self) -> str:
        return "hello"

    def set(self, value: str) -> None:
        pass

# Covariant: int is a subtype of object, so Producer[int] should be compatible with Producer[object]
def use_producer(p: Producer[object]) -> object:
    return p.produce()

producer = IntProducer()
result = use_producer(producer)  # Should work: Producer[int] <: Producer[object]

# Contravariant: int is a subtype of object, so Consumer[object] should be compatible with Consumer[int]
def use_consumer(c: Consumer[int]) -> None:
    c.consume(42)

consumer = ObjectConsumer()
use_consumer(consumer)  # Should work: Consumer[object] <: Consumer[int]

# Invariant: Storage[str] should only accept exactly str, not supertype or subtype
def use_storage(s: Storage[str]) -> str:
    s.set("test")
    return s.get()

storage = StrStorage()
result2 = use_storage(storage)  # Should work: exact match
