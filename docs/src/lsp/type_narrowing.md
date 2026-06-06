# Type Narrowing

Type narrowing refines a value after a guard proves more about it. Beacon uses
narrowing for Optional values, unions, and pattern matching without requiring a
cast in common cases.

```python
def length(value: str | None) -> int:
    if value is None:
        return 0

    return len(value)  # value is str here
```

## Guards Beacon understands

Beacon narrows values through common Python control flow:

- `isinstance(value, T)` and tuple forms such as `isinstance(value, (A, B))`;
- `issubclass(cls, T)` for class objects;
- `type(value) is T` and `type(value) == T`;
- `value is None`, `value is not None`, and singleton checks;
- truthiness checks for optionals, literals, and common containers;
- `hasattr(value, "name")` when the attribute name is literal;
- `callable(value)`;
- `assert`, `raise`, `return`, `break`, and `continue`;
- user-defined `TypeGuard` and `TypeIs` functions;
- `match` patterns and case guards.

## Branches and joins

A guard narrows the branch where it applies. When branches join, Beacon combines
the possible types again.

```python
def parse(value: int | str) -> int:
    if isinstance(value, str):
        value = int(value)

    return value  # value is int after the join
```

Reassignment updates the narrowed type. Narrowing does not pass through aliases,
attributes, or subscripts unless Beacon can prove the relationship is stable.

## Pattern matching

Beacon uses match statements for type refinement and diagnostics:

```python
def describe(value: int | str) -> str:
    match value:
        case int():
            return "integer"
        case str():
            return "string"
```

Beacon supports literals, singletons, sequences, mappings, class patterns, OR
patterns, AS patterns, star patterns, and wildcards. It can report unreachable
patterns and, when it has enough type information, non-exhaustive matches.

## Any and unknown values

`Any` remains an escape hatch. A guard can give useful precision inside one
branch, but Beacon does not keep that precision after the branch ends.

```python
from typing import Any


def use(value: Any) -> None:
    if isinstance(value, str):
        value.upper()  # treated as str in this branch

    value.anything()  # value is Any again
```

Negative branches for `Any` and unknown values stay `Any` or unknown. Beacon does
not infer complement types such as "Any except str".

## When narrowing is not enough

If Beacon cannot prove a relationship, make the type explicit:

- add an annotation at the assignment or function boundary;
- use a small `TypeGuard` helper;
- use `typing.cast` at the narrowest point;
- replace reflective code with a protocol or wrapper function.
