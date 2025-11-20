# Variance Diagnostics

Variance diagnostics describe when mutable containers or position constraints break covariance/contravariance rules.

## HM014 – `VarianceError` {#hm014}

### Example

```py
pets: list[object] = ["dog", "cat"]  # list is invariant
specific_pets: list[str] = pets  # HM014: cannot assign list[str] to list[object]
```

### Guidance

Respect variance constraints.
Mutable containers are invariant, so consider using immutable collections (`tuple[str, ...]`) or widening the source type.
The diagnostic message includes targeted advice per position (in/out).
See [Type Checking](../type_checking.md#type-system-philosophy).
