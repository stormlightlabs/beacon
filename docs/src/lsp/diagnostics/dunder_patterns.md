# Dunder Patterns Diagnostics

Beacon flags special-casing guidance for dunder blocks and entry-point guards to keep symbol metadata accurate.

## DUNDER_INFO – `EntryPointGuard` {#dunder_info}

### Example

```py
if __name__ == "__main__":
    run_cli()
```

### Guidance

This informational hint makes entry-point guards easier to spot. No action needed.
The behavior is described in [Semantic Enhancements](../feature_providers.md#semantic-enhancements).

## DUNDER001 – `MagicMethodOutOfScope` {#dunder001}

### Example

```py
def __str__():
    return "oops"  # Should live inside a class
```

### Guidance

Define magic methods inside classes (`class Foo:\n    def __str__(self) -> str: ...`). This keeps symbol metadata consistent with Python semantics.
See [Semantic Enhancements](../feature_providers.md#semantic-enhancements) for background on how Beacon tracks dunders.
