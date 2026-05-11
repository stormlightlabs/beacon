# Imports And Stubs Spec

This spec defines v1 import, module, stub, and workspace expectations.

## Goal

Beacon should resolve normal Python workspaces predictably and keep source,
stub, CLI, and LSP behavior aligned.

## Module Roots

- Workspace root packages with `__init__.py`.
- `src` and configured source roots.
- Namespace-like roots where supported.
- Subpackages and sibling modules.
- Relative imports at each supported package depth.
- Multi-root workspaces after the single-root contract is stable.

## Import Forms

- `import module` and `import module as alias`.
- `from module import name` and aliases.
- Relative `from .` and `from ..` imports.
- Star imports.
- Imports inside functions and guarded imports.
- `if TYPE_CHECKING` imports.
- Private imports and diagnostics.
- Missing modules and missing imported symbols.
- Circular imports with graceful partial information.

## Exports

- `__all__` lists and simple constants.
- Re-export chains through package and subpackage `__init__.py` files.
- Star import export filtering.
- Private-name filtering.
- Broken re-export diagnostics.

## Stub Precedence

Precedence must be tested for:

- workspace `.py` source;
- workspace `.pyi` stubs;
- custom `stubPaths`;
- bundled stdlib stubs;
- site-packages when supported;
- partial stubs and missing stubs.

The selected source of truth should be visible in diagnostics, hover,
definition, and completion behavior where applicable.

## Invalidation

LSP and workspace tests must cover updates when:

- an imported file changes;
- an exported symbol is added, removed, or renamed;
- `__all__` changes;
- a `.pyi` file changes;
- a config file changes source roots or stub paths;
- a file is created, renamed, or deleted.

## Test Requirements

- `fixtures/workspace/` should include package roots, subpackages, stubs,
  broken imports, private imports, star imports, and re-exports.
- CLI and LSP diagnostics must agree for the same fixture state.
- Reference, rename, completion, and auto-import tests should include imported
  and re-exported symbols.
