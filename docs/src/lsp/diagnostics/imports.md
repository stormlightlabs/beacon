# Imports Diagnostics

Import diagnostics cover module resolution, missing files, and configurable
severities for unresolved imports. For resolution, exports, and stubs, see
[Imports and Stubs](../imports_and_stubs.md).

## circular-import – `CircularImport` {#circular-import}

### Example

```py
# module_a.py
from module_b import helper

# module_b.py
from module_a import helper  # completes a cycle
```

### Guidance

Break the cycle by moving shared code into a third module or deferring one
side of the import inside a function.

Severity comes from `[diagnostics.circular_imports]` in
[Configuration](../../configuration.md#diagnostics).

## missing-module – `MissingModule` {#missing-module}

### Example

```py
import backend.plugins.payments  # File or package missing from workspace/stubs
```

### Guidance

Add the module to the workspace, fix typos, or adjust your import path.
Beacon reports missing modules as errors because Python would fail at runtime.

## unresolved-import – `UnresolvedImport` {#unresolved-import}

### Example

```py
from services import codecs  # services module exists, codecs submodule does not
```

### Guidance

Fix the module path, add missing files, or install the dependency.
Severity is controlled by `[diagnostics.unresolved_imports]` in
[Configuration](../../configuration.md#diagnostics).
