# Imports and Stubs

Beacon resolves imports from source files, package roots, stubs, and project
configuration. The same model feeds diagnostics, hover, definition lookup,
completion, and rename.

Use this page when Beacon cannot find a module, chooses the wrong definition, or
shows a type that appears to come from a stub.

## Supported import forms

Beacon handles common Python imports:

- `import package.module`
- `import package.module as alias`
- `from package import name`
- `from package import name as alias`
- relative imports such as `from . import sibling` and `from ..core import api`
- imports inside functions
- imports guarded by `if TYPE_CHECKING`
- star imports with known exports
- re-exports through `__init__.py`

Circular imports are allowed. During a cycle, Beacon may have partial
information. If the cycle hides a name, move shared code to another module or
defer one side of the import.

## Module roots

Beacon searches configured source roots and workspace package roots. A typical
`src` layout looks like this:

```text
project/
  beacon.toml
  src/
    package/
      __init__.py
      api.py
  tests/
    test_api.py
```

If your project uses custom source roots or stub directories, add them to the
configuration used by both the CLI and language server.

## Exports and star imports

Beacon treats these names as public exports:

- names listed in a static `__all__` literal;
- names imported and re-exported by package `__init__.py` files;
- public names that do not start with `_` when no `__all__` exists.

Runtime `__all__` changes are dynamic. Beacon reports them when they affect
checked code. Prefer a static list:

```python
from .api import Client, connect

__all__ = ["Client", "connect"]
```

## Stub precedence

When source and stubs both exist, Beacon uses the most specific typed view it can
find. Workspace files and configured stub paths should describe your project
before bundled or third-party fallbacks are used.

Stubs affect diagnostics, hover text, go-to-definition, completion, and call
checking. If a symbol looks wrong in the editor, check whether a `.pyi` file or
custom stub path shadows the implementation.

## Invalidation

The language server refreshes import data when you change:

- an imported file;
- an exported symbol;
- `__all__`;
- a `.pyi` file;
- source roots or stub paths;
- file names or package structure.

If CLI and editor results differ, confirm both use the same configuration. Then
restart the language server if the editor still shows old data.

## Related diagnostics

See [Import Diagnostics](./diagnostics/imports.md) for missing modules,
unresolved imports, and circular imports.
