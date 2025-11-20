# Scope Diagnostics

Scope diagnostics call out variable shadowing inside nested scopes.

## shadowed-variable – `ShadowedVariable` {#shadowed-variable}

### Example

```py
token = "outer"

def handler():
    token = "inner"  # Shadows outer variable
```

### Guidance

Rename inner variables or move logic closer to usage to avoid surprising shadowing.
The static analyzer describes its scope walk in [Control & Data Flow](../static_analysis.md#control--data-flow).
