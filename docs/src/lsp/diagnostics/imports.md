# Imports Diagnostics

Import diagnostics enumerate issues with module resolution, missing files, and configurable severities for unresolved imports.

## circular-import – `CircularImport` {#circular-import}

### Example

```py
# module_a.py
from module_b import helper

# module_b.py
from module_a import helper  # completes a cycle
```

### Guidance

Break the cycle by moving shared code into a third module, deferring imports inside functions, or rethinking module boundaries
Severity comes from `[diagnostics.circular_imports]` in [Configuration](../../configuration.md#diagnostics).

## missing-module – `MissingModule` {#missing-module}

### Example

```py
import backend.plugins.payments  # File or package missing from workspace/stubs
```

### Guidance

Add the module to the workspace, fix typos, or adjust your import path.
Beacon reports missing modules as errors because runtime execution would fail immediately.

## unresolved-import – `UnresolvedImport` {#unresolved-import}

### Example

```py
from services import codecs  # services module exists, codecs submodule does not
```

### Guidance

Fix the module path, add missing files, or install the dependency.
Severity is controlled by `[diagnostics.unresolved_imports]` in [Configuration](../../configuration.md#diagnostics).
