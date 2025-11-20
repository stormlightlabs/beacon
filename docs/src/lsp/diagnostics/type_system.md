# Type System Diagnostics

Type system diagnostics originate from Hindley–Milner inference, call checking, and generic validation.

## HM001 – `TypeMismatch` {#hm001}

### Example

```py
def add(flag: bool) -> int:
    return flag + "!"  # int vs. str cannot unify
```

### Guidance

Beacon’s Hindley–Milner engine reports HM001 when two types cannot unify (see [Subtyping vs Unification](../type_checking.md#subtyping-vs-unification)).
Convert or narrow the values so the operands share a compatible type.

## HM002 – `OccursCheckFailed` {#hm002}

### Example

```py
def self_apply(f):
    return f(f)  # Requires f: T -> T, but T would have to contain itself
```

### Guidance

Occurs-check failures indicate an infinite recursive type.
Refactor so values are not applied to themselves without a wrapper type, or introduce generics that break the cycle.
See [Type Checking](../type_checking.md#type-system-philosophy) for how recursive types are limited.

## HM003 – `UndefinedTypeVar` {#hm003}

### Example

```py
def use_unknown(x: U) -> U:  # U was never declared via TypeVar
    return x
```

### Guidance

Declare every type variable with `TypeVar` before referencing it: `U = TypeVar("U")`.
The generics workflow is covered in [Type Checking](../type_checking.md#type-inference).

## HM004 – `KindMismatch` {#hm004}

### Example

```py
ids: dict[str] = {}  # dict expects two type arguments
```

### Guidance

Provide the correct number of arguments for each generic (`dict[str, int]`).
Beacon enforces kind arity to avoid ambiguous instantiations.
See [Type Checking](../type_checking.md#special-types).

## HM005 – `InfiniteType` {#hm005}

### Example

```py
def paradox(x):
    return x(paradox)  # Leads to an infinite type when inferred
```

### Guidance

Infinite type errors usually stem from higher-order functions that apply un-annotated callables to themselves.
Add annotations to break the cycle or restructure the algorithm so a value is not required to contain itself.

## HM006 – `ProtocolNotSatisfied` {#hm006}

### Example

```py
from typing import Iterable

def consume(xs: Iterable[str]) -> None:
    for item in xs:
        print(item.upper())

consume(10)  # int does not satisfy Iterable[str]
```

### Guidance

Ensure call arguments implement the required protocol slots or convert them first (wrap values in iterables, implement `__iter__`, etc.).
Protocol behavior is described in [Type Checking](../type_checking.md#type-system-philosophy).

## HM008 – `ArgumentCountMismatch` {#hm008}

### Example

```py
def pair(a: int, b: int) -> None:
    ...

pair(1)  # Missing second positional argument
```

### Guidance

Match the declared arity (positional + keyword-only + variadic).
Add or remove arguments, or update the function signature.
This follows the call constraint rules in [Type Checking](../type_checking.md#type-system-philosophy).

## HM009 – `ArgumentTypeMismatch` {#hm009}

### Example

```py
def square(x: int) -> int:
    return x * x

square("ten")  # Argument type mismatch
```

### Guidance

Convert arguments to the expected type or adjust the signature to accept a broader type.
Beacon pinpoints the offending parameter in the diagnostic.

## HM011 – `KeywordArgumentError` {#hm011}

### Example

```py
def connect(host: str, *, ssl: bool) -> None:
    ...

connect("db", secure=True)  # Unknown keyword `secure`
```

### Guidance

Use valid keyword names, avoid duplicates, and respect positional-only/keyword-only markers.
Adjust the call site or function signature accordingly.

## HM012 – `GenericTypeError` {#hm012}

### Example

```py
def capture() -> int:
    cache = []
    def inner():
        cache.append(inner)
        return inner(cache)  # Triggers a generic HM012 error about unsafe recursion
```

### Guidance

HM012 is a catch-all for rare Hindley–Milner failures (value restriction violations, unsupported constructs).
Inspect the message for context, add annotations to guide inference, or refactor towards supported patterns.
See [Type Checking](../type_checking.md#type-system-philosophy).
