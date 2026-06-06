# Dynamic Python Spec

This spec defines how v1 handles dynamic Python features.

## Goal

Beacon should be explicit when Python behavior is too dynamic for precise
static analysis. Each dynamic case must choose one outcome:

- infer a concrete type;
- emit a stable diagnostic;
- cross an explicit `Any`/unknown boundary.

Silent crashes, placeholder output, and inconsistent CLI/LSP behavior are not
allowed.

## Dynamic Cases

- Monkey-patching modules, classes, instances, and functions.
- Metaclasses and dynamic class creation.
- Module, class, and instance `__getattr__`.
- `getattr`, `setattr`, `delattr`, and reflective lookup.
- Dynamic imports through `__import__` and `importlib`.
- Custom import hooks and path mutation.
- `exec`, `eval`, and generated code.
- Decorators that replace functions or classes.
- Runtime mutation of `__all__`, `__bases__`, and `__class__`.

## Outcome Rules

Concrete inference is allowed when Beacon can prove the target and resulting
type from local source, stubs, or a supported builtin pattern.

A diagnostic is required when the code appears type-relevant and Beacon cannot
model it without hiding a likely error. The diagnostic must include code,
severity, message fragment, and span.

`Any` or unknown fallback is acceptable at a documented boundary. The fallback
must not erase unrelated precise types, and strict mode may raise severity for
unsafe propagation.

## V1 Fallback Contract

- Monkey-patching via `setattr`/`delattr`, module/class hierarchy mutation, or
  direct `__class__`/`__bases__` mutation emits `DYN001` and the mutated
  value is treated as crossing an `Any`/unknown boundary.
- Custom metaclasses and dynamic class creation with `type(name, bases, dict)`
  emit `DYN001`; the class object itself is not treated as a precise
  static class beyond locally declared members.
- Annotated `__getattr__` is accepted as an explicit fallback boundary. Literal
  `getattr(obj, "name")` and `hasattr(obj, "name")` are supported patterns;
  non-literal reflective lookup emits `DYN001` and falls back to
  `Any`/unknown.
- Dynamic imports via `__import__` or `importlib.import_module`, custom import
  hooks, and runtime `sys.path` mutation emit `DYN001`; imported module
  values are `Any`/unknown unless a normal import path resolves separately.
- `eval`, `exec`, and `compile` emit `DYN001`; generated code does not
  add declarations to the static environment.
- Decorators outside the supported transparent set may replace functions or
  classes and emit `DYN001`.
- Runtime `__all__` mutation emits `DYN001`; only static `__all__`
  literals participate in export resolution.

## Mode Behavior

- Strict mode should make unsafe dynamic boundaries visible.
- Balanced mode should warn when dynamic behavior affects checked code.
- Relaxed mode may allow more `Any` or unknown propagation, but it must remain
  deterministic and tested.

## Test Requirements

- `fixtures/workspace/` should include one module for supported dynamic
  patterns and one for unsupported or fallback behavior.
- CLI and LSP diagnostics must match.
- Tests should assert whether each case is concrete, diagnostic, or fallback.
- Hover and completion should avoid presenting fallback types as precise facts.
