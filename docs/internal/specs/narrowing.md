# Narrowing Spec

This spec defines v1 narrowing and related data-flow expectations.

## Goal

Beacon should narrow values through ordinary Python control flow while keeping
HM inference consistent across branches and joins.

Each narrowing case must assert the narrowed type, the joined type after the
branch, or a stable diagnostic when the expression is unsupported.

## Guard Sources

- `isinstance`, including tuple arguments and union input types.
- `issubclass` and class-object narrowing.
- `type(x) is T`, `type(x) == T`, and negative forms.
- `x is None`, `x is not None`, singleton checks, and enum-member checks.
- Truthiness and falsiness for optionals, literals, containers, and custom
  `__bool__` or `__len__` where supported.
- Membership checks with `in` and `not in`.
- `hasattr` and attribute-existence checks where supported.
- `callable`, `assert`, `raise`, `return`, `break`, and `continue`.
- User-defined `TypeGuard` and `TypeIs`.
- Pattern matching guards and structural patterns.

## Scope And Assignment Rules

- Narrowing must respect local, nonlocal, global, class, and comprehension
  scopes.
- Reassignment invalidates or updates the narrowed type.
- Aliases should narrow only when the analysis can prove the alias relation.
- Attribute and subscript narrowing should be supported only when the contract
  says the place is stable.
- Narrowing must not leak out of branches where the predicate did not apply.

## Pattern Matching

- Literal, singleton, sequence, mapping, class, OR, AS, star, and wildcard
  patterns.
- Exhaustiveness and unreachable pattern diagnostics where supported.
- Pattern-bound variable types inside the matching case.
- Joins after a match statement.

## Joins And Reachability

- If/elif/else joins.
- Loop joins, including break and continue edges.
- Try/except/else/finally joins.
- Constant branch evaluation for `True`, `False`, simple comparisons, and
  statically known platform/version branches where supported.
- Unreachable code and use-before-definition diagnostics.

## Any And Unknown

`Any` and unknown values should not produce durable fake precision.

Guards may provide temporary branch-local precision for usability. For example,
`if isinstance(x, str)` may treat `x: Any` as `str` in the true branch, and
`x is None` may treat it as `None` in that branch. The original value remains
`Any` outside the branch.

Negative branches for `Any` and unknown values stay `Any`/unknown. Beacon does
not infer complement types such as `Any except str`.

Joins involving `Any` should resolve to `Any` unless an explicit annotation or
assignment imposes a narrower type. Unknown values follow the same shape but may
also surface unresolved or unsafe-use diagnostics depending on mode.

Tests must assert true-branch precision, false-branch `Any`/unknown behavior,
and post-join `Any`/unknown behavior.

## Test Requirements

- `fixtures/workspace/` should include representative narrowing flows used by
  CLI and LSP assertions.
- Focused tests should cover guard-specific edge cases.
- Assertions should include inferred type before the guard, inside each branch,
  and after the join when that type is part of the contract.
