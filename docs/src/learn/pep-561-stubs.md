# Distributing and Packaging Python Type Information (`.pyi`/stubs)

## Abstract

**PEP 561** establishes a standardized method for **distributing and packaging type information** in Python.
It builds upon [PEP 484](./pep-484-585.md), addressing the problem of how type information for bboth inline and in separate stub files.
Stubs can be discovered, packaged, and used by type checkers across environments.

This allows:

- Package maintainers to declare their code as typed,
- Third parties to publish independent stub packages, and
- Type checkers to resolve imports consistently across mixed environments.

## Background

Prior to PEP 561:

- There was **no consistent way** to distribute typing information with Python packages.
- Stub files had to be manually placed in `MYPYPATH` or equivalent.
- Community stubs were collected centrally in [Typeshed](https://github.com/python/typeshed), which became a scalability bottleneck.

The goals are:

1. To use existing packaging infrastructure (distutils/setuptools).
2. To provide clear markers for type-aware packages.
3. To define resolution rules so that tools like mypy, pyright, or pylance can locate and prioritize type information uniformly

> PEP 561 recognizes three models: inline-typed, stub-typed, and third-party stub-only packages.

## Packaging Type Information

### Inline

Inline-typed packages must include a **marker file** named `py.typed` inside the package root.

Example setup:

```python
setup(
    name="foopkg",
    packages=["foopkg"],
    package_data={"foopkg": ["py.typed"]},
)
```

This file signals to type checkers that the package and all its submodules are typed.
For **namespace packages** (PEP 420), the marker should be placed in submodules to avoid conflicts.

### Stub-Only

- Stub-only packages contain `.pyi` files without any runtime code.
- Naming convention: `foopkg-stubs` provides types for `foopkg`.
- `py.typed` is **not required** for these packages.
- Version compatibility should be expressed in dependencies (e.g. via `install_requires`).

Example layout:

```sh
shapes-stubs/
└── polygons/
    ├── pentagon/__init__.pyi
    └── hexagon/__init__.pyi
```

### Partial Stubs

Partial stubs (incomplete libraries) must include `partial\n` inside `py.typed`.

These instruct type checkers to:

- Merge the stub directory with the runtime or typeshed directory.
- Continue searching through later steps in the resolution order.

## Module Resolution Order

Type checkers must resolve type information using the following **ordered search path**:

| Priority | Source                        | Description                                          |
| -------- | ----------------------------- | ---------------------------------------------------- |
| 1        | Manual stubs / MYPYPATH       | User-specified patches override all.                 |
| 2        | User code                     | The project’s own files.                             |
| 3        | Stub packages (`*-stubs`)     | Distributed stubs take precedence over inline types. |
| 4        | `py.typed` packages           | Inline or bundled types inside installed packages.   |
| 5        | Typeshed                      | Fallback for stdlib and untyped third-party libs.    |

If a stub-only namespace package lacks a desired module, type checkers continue searching through the inline and typeshed steps.
When checking against another Python version, the checker must look up that version’s `site-packages` path.

## Conventions

### Library Interface

When `py.typed` is present:

- All `.py` and `.pyi` files are considered importable.
- Files beginning with `_` are private.
- Public symbols are controlled via `__all__`.

Valid `__all__` idioms include:

```python
__all__ = ['a', 'b']
__all__ += submodule.__all__
__all__.extend(['c', 'd'])
```

These restrictions allow static determination of public exports by type checkers.

### Imports and Re-Exports

Certain import forms signal that an imported symbol should be **re-exported** as part of the module’s public interface:

```python
import X as X            # re-export X
from Y import X as X     # re-export X
from Y import *          # re-exports __all__ or all public symbols
```

All other imports are private by default.

## Implementation and Tooling

- `mypy` implements full PEP 561 resolution, allowing users to inspect installed package metadata (`py.typed`, stub presence, etc.).
- Tools like **pyright**, **pylance**, and **Pytype** adopt the same ordering and conventions.
- Example repositories include:

    - [`typed_package`](https://github.com/emmatyping/pep-561)
    - [`stub_package`](https://github.com/emmatyping/stub-package)
    - [`numpy-stubs`](https://github.com/numpy/numpy-stubs)

This design remains **fully backward compatible**, requiring no changes to Python’s runtime or packaging systems.
