# Typing Surface

Beacon covers common typed Python: annotations, generics, decorators, data
models, protocols, async code, and dynamic boundaries. When Beacon cannot model a
feature precisely, it reports a diagnostic or treats the value as `Any` or
unknown.

## Core inference

Beacon infers local types for variables, functions, lambdas, comprehensions,
nested functions, calls, and returns. Add annotations where an empty collection,
mixed branch, or dynamic value would otherwise be ambiguous.

```python
items: list[int] = []
items.append(1)
```

## Functions and callables

Beacon checks positional, keyword, defaulted, keyword-only, variadic, and bound
method calls. It also understands callable annotations, overloads, callback
protocols, and decorators that preserve signatures.

Decorators that may replace a function or class are dynamic boundaries unless
Beacon recognizes them as transparent.

## Generics

Beacon supports generic functions and classes, including common `TypeVar` use,
bounds, constraints, variance, generic methods, class methods, static methods,
and constructors. Newer typing forms depend on the configured Python version.

Supported typing features include:

- `TypeVar`
- `ParamSpec` and `Concatenate`
- variadic tuple-style generics
- PEP 695 type-parameter syntax when enabled by the target version

## Classes and data models

Beacon models common class patterns:

- dataclasses and generated initializers;
- enums and enum-member literals;
- `TypedDict` required and optional keys;
- properties and descriptors;
- `ClassVar`, `Final`, and override checks;
- magic methods used by operators, indexing, iteration, context managers, and
  truthiness.

Annotate public attributes and constructor parameters when class state is split
across branches.

## Protocols

Protocols use structural typing: a value satisfies a protocol when it has the
required attributes and methods. Beacon checks normal, generic, inherited, and
runtime-checkable protocols where their static behavior is well defined.

```python
from typing import Protocol


class Closable(Protocol):
    def close(self) -> None: ...


def shutdown(resource: Closable) -> None:
    resource.close()
```

## Containers, literals, and operators

Beacon understands common containers and literal values:

- `list`, `dict`, `set`, `tuple`, named tuples, and unpacking;
- string, number, boolean, enum, and singleton literals;
- unions and optionals;
- operator result types and unsupported-operator diagnostics.

For empty or heterogeneous containers, add an annotation that shows the intended
shape.

## Async and generators

Beacon checks async functions, awaitable results, generators, async generators,
`yield from`, and sync or async context managers. Public generator and
context-manager APIs usually produce clearer caller errors with annotations.

## Dynamic boundaries

Python features such as monkey-patching, non-literal reflection, dynamic imports,
`eval`, `exec`, and runtime class mutation can hide type information. Beacon
reports these boundaries when they affect checked code. See
[Dynamic Python Diagnostics](./diagnostics/dynamic.md).
