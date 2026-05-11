# Typing Breadth Spec

This spec defines the v1 typing surface that must be represented in
`fixtures/workspace/` and focused checker tests.

## Goal

Beacon v1 should handle common typed Python without requiring users to avoid
normal annotations, generic APIs, decorators, or data-model patterns.

Tests should assert either an inferred type, a stable diagnostic, or an
explicit `Any`/unknown boundary.

## Core Inference

- Local inference for variables, functions, lambdas, comprehensions, and
  nested functions.
- Polymorphic helper functions and value restriction/generalization behavior.
- Callable constraints for positional, keyword, defaulted, keyword-only,
  variadic, and bound receiver calls.
- Return inference for explicit returns, implicit `None`, and mixed branches.

## Generics

- Generic functions and classes.
- TypeVar bounds, constraints, variance, and defaults.
- ParamSpec and Concatenate for callback-preserving decorators.
- TypeVarTuple and unpacked tuple-like generics.
- PEP 695 type parameter syntax where the configured Python version allows it.
- Generic methods, classmethods, staticmethods, and constructors.

## Callable And Overload Rules

- Overload selection for positional, keyword, defaulted, variadic, and union
  arguments.
- Overload implementation compatibility.
- Overlap checks where v1 supports them.
- Callable protocols and callback protocols.
- Decorators that preserve signatures and decorators that intentionally erase
  or transform them.

## Class And Data Model

- Dataclasses, including defaults, default factories, frozen fields,
  keyword-only fields, `InitVar`, `ClassVar`, and generated `__init__`.
- Dataclass transform behavior where v1 supports it.
- Enums and literal enum-member behavior.
- TypedDict required, optional, readonly, extra, update, and key-access cases.
- Properties, descriptors, class variables, `Final`, override checks, and
  inherited members.
- Common magic methods used by operators, indexing, iteration, context
  managers, and truthiness.

## Protocols

- Structural conformance for attributes and methods.
- Generic protocols, inherited protocols, recursive protocols where supported,
  and negative cases with missing or incompatible members.
- Runtime-checkable protocols where behavior differs from static structural
  checks.

## Containers, Literals, And Operators

- Lists, dicts, sets, tuples, named tuples, sequence unpacking, and starred
  unpacking.
- Literal strings, numbers, booleans, enum literals, and singleton values.
- Union and optional simplification.
- Operator result types and unsupported-operator diagnostics.

## Async And Generators

- Async functions and awaitable return behavior.
- Generators, async generators, send/yield/return variance, and `yield from`.
- Sync and async context managers, including generic exit types.

## Test Requirements

- Every v1-supported category above needs at least one workspace case and one
  focused regression case when the behavior is too small or noisy for the
  shared fixture.
- Negative cases must assert diagnostic code, severity, message fragment, and
  span.
- Positive cases should assert inferred type, hover text, or call resolution
  where the type is part of the v1 contract.
