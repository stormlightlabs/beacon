# Patterns Diagnostics

Pattern diagnostics describe exhaustiveness and reachability issues detected in structural pattern matching.

## PM001 – `PatternNonExhaustive` {#pm001}

### Example

```py
def handle(flag: bool) -> str:
    match flag:
        case True:
            return "y"
```

No `False` case triggers PM001.

### Guidance

Add the missing cases (`case False:` or `case _:`).
Exhaustiveness checking is covered in [Pattern Matching Support](../static_analysis.md#pattern-matching-support).

## PM002 – `PatternUnreachable` {#pm002}

### Example

```py
match value:
    case _:
        return 0
    case 1:
        return value  # Unreachable after wildcard case
```

### Guidance

Reorder or delete subsumed patterns so every case is reachable.
See [Pattern Matching Support](../static_analysis.md#pattern-matching-support).
