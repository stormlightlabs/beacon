# Data Flow Diagnostics

Data-flow diagnostics track unreachable blocks and variable usage ordering issues.

## unreachable-code – `UnreachableCode` {#unreachable-code}

### Example

```py
def foo():
    return 42
    print("never runs")  # Unreachable
```

### Guidance

Remove or refactor unreachable statements.
Diagnostics carry the `UNNECESSARY` tag so editors can gray out the code.
Pipeline details sit in [Control & Data Flow](../static_analysis.md#control--data-flow).

## unused-variable – `UnusedVariable` {#unused-variable}

### Example

```py
def process():
    result = compute()  # Never read later
```

### Guidance

Use the variable, prefix with `_` to mark as intentionally unused, or delete it.
See [Control & Data Flow](../static_analysis.md#control--data-flow) for how Beacon tracks reads/writes.

## use-before-def – `UseBeforeDef` {#use-before-def}

### Example

```py
def build():
    print(total)
    total = 10  # total read before assignment in this scope
```

### Guidance

Reorder statements so assignments precede reads, or mark outer-scope variables as `nonlocal`/`global` when appropriate.
Data-flow analysis is described in [Control & Data Flow](../static_analysis.md#control--data-flow).
