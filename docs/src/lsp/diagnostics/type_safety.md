# Type Safety Diagnostics

Diagnostics in this category highlight when inference collapses to `Any` and reduces overall type safety.

## ANY001 – `UnsafeAnyUsage` {#any001}

### Example

```py
from typing import Any

payload: Any = fetch_config()
payload["timeout"]  # ANY001 – inference lost precision once `Any` appeared
```

### Guidance

Beacon warns when unchecked `Any` values flow through the type map (see [Special Types](../type_checking.md#special-types)).
Replace `Any` with a precise annotation, cast the value after runtime checks, or refactor APIs so that callers receive concrete types.
