# Dynamic Python Diagnostics

Beacon reports dynamic Python boundaries when runtime behavior can change names,
types, imports, or class layout in ways static analysis cannot model precisely.

## DYN001 – `DynamicBoundary` {#dyn001}

Emitted when code crosses a dynamic boundary that may hide type-relevant
behavior. Severity is mode-aware:

- strict: error
- balanced: warning
- relaxed: hint

Common triggers include:

- `setattr` / `delattr` monkey-patching;
- runtime `__class__` or `__bases__` mutation;
- custom metaclasses and `type(name, bases, namespace)` class creation;
- non-literal `getattr(obj, name)` reflection;
- `__import__` or `importlib.import_module` dynamic imports;
- custom import hooks and `sys.path` mutation;
- `eval`, `exec`, or `compile` generated code;
- decorators that may replace functions or classes;
- runtime `__all__` mutation.

```python
import sys


def load(name: str):
    return __import__(name)  # DYN001


def patch(obj: object) -> None:
    setattr(obj, "field", 1)  # DYN001


sys.meta_path.append(object())  # DYN001
```

Supported patterns such as literal `getattr(obj, "name")`, `hasattr`, and an
annotated `__getattr__` method act as explicit fallback boundaries and do not
produce this diagnostic by themselves.

Fix by replacing dynamic behavior with ordinary imports, declared attributes,
stubs, protocols, or typed wrapper functions where possible. If the dynamic
boundary is intentional, isolate it and annotate the result as `Any`.
