# Beacon Diagnostic Codes

Beacon’s [Diagnostic provider](./feature_providers.md#diagnostics) combines parser feedback, Hindley–Milner type errors, annotation coverage checks, control/data-flow analysis, and workspace import resolution into a single stream of LSP diagnostics.

This guide lists every diagnostic code emitted by that pipeline so you can interpret squiggles quickly and trace them back to the subsystem described
in [Type Checking](./type_checking.md), [Static Analyzer](./static_analysis.md), and [Type Checking Modes](../type-checking-modes.md).

LSP severity for imports (circular vs. unresolved) remains configurable under `[diagnostics]` as documented in [Configuration](../configuration.md#diagnostics).
To temporarily suppress any diagnostic, use the mechanisms described in [Suppressions](../format/suppressions.md).

---

**Legend:**

- &#9888; = Warning
- &#10005; = Error
- &#9432; = Info/Hints

Note that  per-mode rows show the icon used in strict / balanced / loose order

| Code                                      | Name                              | Level                               | Category        | Description                                                           |
| ----------------------------------------- | --------------------------------- | ----------------------------------- | --------------- | --------------------------------------------------------------------- |
| [ANY001](#any001)                         | `UnsafeAnyUsage`                  | &#9888;                             | Type Safety     | Deep inference found an `Any` value, reducing type safety.            |
| [ANN001](#ann001)                         | `AnnotationMismatch`              | &#10005;&nbsp;&#9888;&nbsp;&#9432;  | Annotations     | Declared annotation disagrees with the inferred type.                 |
| [ANN002](#ann002)                         | `MissingVariableAnnotation`       | &#10005; &#9888;                    | Annotations     | Assignment lacks an annotation in strict/balanced modes.              |
| [ANN003](#ann003)                         | `ParameterAnnotationMismatch`     | &#10005; &#9888; &#9432;            | Annotations     | Parameter annotation conflicts with inferred usage.                   |
| [ANN004](#ann004)                         | `MissingParameterAnnotation`      | &#10005; &#9888;                    | Annotations     | Parameter missing annotation when inference is precise.               |
| [ANN005](#ann005)                         | `ReturnAnnotationMismatch`        | &#10005; &#9888; &#9432;            | Annotations     | Function return annotation disagrees with inference.                  |
| [ANN006](#ann006)                         | `MissingReturnAnnotation`         | &#10005; &#9888;                    | Annotations     | Function lacks return annotation when inference is concrete.          |
| [ANN007](#ann007)                         | `ImplicitAnyParameter`            | &#10005;                            | Annotations     | Strict mode forbids implicit `Any` on parameters.                     |
| [ANN008](#ann008)                         | `ImplicitAnyReturn`               | &#10005;                            | Annotations     | Strict mode forbids implicit `Any` return types.                      |
| [ANN009](#ann009)                         | `MissingClassAttributeAnnotation` | &#10005;                            | Annotations     | Strict mode requires explicit annotations on class attributes.        |
| [DUNDER_INFO](#dunder_info)               | `EntryPointGuard`                 | &#9432;                             | Dunder Patterns | Highlights `if __name__ == "__main__":` guard blocks.                 |
| [DUNDER001](#dunder001)                   | `MagicMethodOutOfScope`           | &#9888;                             | Dunder Patterns | Magic methods defined outside a class.                                |
| [HM001](#hm001)                           | `TypeMismatch`                    | &#10005;                            | Type System     | Hindley–Milner could not unify two types.                             |
| [HM002](#hm002)                           | `OccursCheckFailed`               | &#10005;                            | Type System     | Recursive type variable detected (infinite type).                     |
| [HM003](#hm003)                           | `UndefinedTypeVar`                | &#10005;                            | Type System     | Referenced type variable was never declared.                          |
| [HM004](#hm004)                           | `KindMismatch`                    | &#10005;                            | Type System     | Wrong number of type arguments supplied to a generic.                 |
| [HM005](#hm005)                           | `InfiniteType`                    | &#10005;                            | Type System     | Inference produced a non-terminating type (self-referential).         |
| [HM006](#hm006)                           | `ProtocolNotSatisfied`            | &#10005;                            | Type System     | Value fails to implement the required protocol methods.               |
| [HM007](#hm007)                           | `AttributeNotFound`               | &#10005;                            | Attributes      | Attribute or method does not exist on the receiver type.              |
| [HM008](#hm008)                           | `ArgumentCountMismatch`           | &#10005;                            | Type System     | Call site passes too many or too few arguments.                       |
| [HM009](#hm009)                           | `ArgumentTypeMismatch`            | &#10005;                            | Type System     | Argument type incompatible with the parameter type.                   |
| [HM010](#hm010)                           | `PatternTypeMismatch`             | &#10005;                            | Pattern Typing  | Match/case pattern cannot match the subject type.                     |
| [HM011](#hm011)                           | `KeywordArgumentError`            | &#10005;                            | Type System     | Unknown or duplicate keyword arguments in a call.                     |
| [HM012](#hm012)                           | `GenericTypeError`                | &#10005;                            | Type System     | Catch-all Hindley–Milner error (value restriction, etc.).             |
| [HM013](#hm013)                           | `PatternStructureMismatch`        | &#10005;                            | Pattern Typing  | Pattern shape (mapping, class, sequence) differs from subject.        |
| [HM014](#hm014)                           | `VarianceError`                   | &#10005;                            | Variance        | Invariant/covariant/contravariant constraint violated.                |
| [MODE_INFO](#mode_info)                   | `TypeCheckingMode`                | &#9432;                             | Mode            | Reminder showing which type-checking mode produced diagnostics.       |
| [PM001](#pm001)                           | `PatternNonExhaustive`            | &#10005;                            | Patterns        | Match statement fails to cover every possible case.                   |
| [PM002](#pm002)                           | `PatternUnreachable`              | &#10005;                            | Patterns        | Later pattern is shadowed by an earlier one.                          |
| [circular-import](#circular-import)       | `CircularImport`                  | &#10005; &#9888; &#9432;            | Imports         | Module participates in an import cycle (severity comes from config).  |
| [missing-module](#missing-module)         | `MissingModule`                   | &#10005;                            | Imports         | Referenced module is absent from the workspace/stubs.                 |
| [shadowed-variable](#shadowed-variable)   | `ShadowedVariable`                | &#9888;                             | Scope           | Inner scope reuses a name that already exists in an outer scope.      |
| [undefined-variable](#undefined-variable) | `UndefinedVariable`               | &#10005;                            | Name Resolution | Name used before being defined anywhere.                              |
| [unresolved-import](#unresolved-import)   | `UnresolvedImport`                | &#10005; &#9888; &#9432;            | Imports         | Import target cannot be resolved (severity configurable).             |
| [unreachable-code](#unreachable-code)     | `UnreachableCode`                 | &#9888;                             | Data Flow       | Code after `return`, `raise`, or `break` never executes.              |
| [unused-variable](#unused-variable)       | `UnusedVariable`                  | &#9432;                             | Data Flow       | Variable assigned but never read.                                     |
| [use-before-def](#use-before-def)         | `UseBeforeDef`                    | &#10005;                            | Data Flow       | Variable read before it is assigned in the current scope.             |

## Diagnostics

### ANY001

#### Example

```py
from typing import Any

payload: Any = fetch_config()
payload["timeout"]  # ANY001 – inference lost precision once `Any` appeared
```

#### Guidance

Beacon warns when unchecked `Any` values flow through the type map (see [Special Types](./type_checking.md#special-types)).
Replace `Any` with a precise annotation, cast the value after runtime checks, or refactor APIs so that callers receive concrete types.

### ANN001

#### Example

```py
value: int = "stale"  # Annotated as int, inferred as str
```

#### Guidance

Annotation/inference mismatches inherit their severity from the active mode (strict → error, balanced → warning, loose → hint).
Align the annotation with real usage, or change the code so the inferred type matches.
See [Type Checking](./type_checking.md#type-inference) and [Type Checking Modes](../type-checking-modes.md) for the inference rules.

### ANN002

#### Example

```py
# beacon: mode=strict
profile = load_profile()  # Missing annotation (ANN002)
```

#### Guidance

Strict/balanced modes expect assignments with concrete inferred types to be annotated.
Add the appropriate annotation (`profile: Profile = load_profile()`), or downgrade the file to loose mode if intentional (see [Type Checking Modes](../type-checking-modes.md)).

### ANN003

#### Example

```py
def greet(name: str) -> str:
    return name + 1  # name inferred as int due to arithmetic
```

#### Guidance

Parameter annotations must agree with how the function body uses the value.
Update the annotation or refactor the body to respect it.
Details about how inference follows parameter usage live in [Type Checking](./type_checking.md#type-inference).

### ANN004

#### Example

```py
def send_email(address):
    ...
```

Balanced/strict modes infer `address: str` (or similar) and emit ANN004.

#### Guidance

Add explicit parameter annotations whenever inference is concrete: `def send_email(address: str) -> None:`.
Loose mode skips this check entirely (see [Type Checking Modes](../type-checking-modes.md)).

### ANN005

#### Example

```py
def parity(flag: bool) -> bool:
    return "odd"  # Return annotation mismatch
```

#### Guidance

Ensure return annotations reflect every path.
Either return the annotated type or adjust the annotation.
See [Type Checking](./type_checking.md#type-system-philosophy) for how Beacon treats return types.

### ANN006

#### Example

```py
def total(values):
    return sum(values)
```

Balanced/strict modes infer a concrete return type (e.g., `int`) and require `-> int`.

#### Guidance

Add return annotations when inference is precise and not `Any`/`None`: `def total(values: list[int]) -> int:`.
Loose mode suppresses this requirement (see [Type Checking Modes](../type-checking-modes.md)).

### ANN007

#### Example

```py
# beacon: mode=strict
def transform(data):
    return data.strip()
```

#### Guidance

Strict mode disallows implicit `Any` on parameters even when inference could deduce a type.
Add annotations for every parameter (`data: str`). Balanced/loose modes emit ANN004 instead or skip the check entirely.
Review [Type Checking Modes](../type-checking-modes.md) for severity rules.

### ANN008

#### Example

```py
# beacon: mode=strict
def make_id():
    return uuid.uuid4().hex  # Implicit Any return type
```

#### Guidance

Strict mode requires explicit return annotations on every function.
Provide the exact type (`-> str`) or loosen the file mode if you intentionally rely on inference.
See [Type Checking Modes](../type-checking-modes.md) for override syntax.

### ANN009

#### Example

```py
# beacon: mode=strict
class Configuration:
    host = "localhost"  # Missing type annotation
    port: int = 8080  # OK: Has annotation
```

#### Guidance

Strict mode requires explicit type annotations on all class attributes.
Add the annotation (`host: str = "localhost"`) or use balanced/loose mode if gradual typing is preferred.
Note that instance attributes (assigned in `__init__` or other methods) are not subject to this check—only class-level attributes defined directly in the class body.
See [Type Checking Modes](../type-checking-modes.md) for mode configuration.

### DUNDER_INFO

#### Example

```py
if __name__ == "__main__":
    run_cli()
```

#### Guidance

This informational hint makes entry-point guards easier to spot. No action needed.
The behavior is described in [Semantic Enhancements](./feature_providers.md#semantic-enhancements).

### DUNDER001

#### Example

```py
def __str__():
    return "oops"  # Should live inside a class
```

#### Guidance

Define magic methods inside classes (`class Foo:\n    def __str__(self) -> str: ...`). This keeps symbol metadata consistent with Python semantics.
See [Semantic Enhancements](./feature_providers.md#semantic-enhancements) for background on how Beacon tracks dunders.

### HM001

#### Example

```py
def add(flag: bool) -> int:
    return flag + "!"  # int vs. str cannot unify
```

#### Guidance

Beacon’s Hindley–Milner engine reports HM001 when two types cannot unify (see [Subtyping vs Unification](./type_checking.md#subtyping-vs-unification)).
Convert or narrow the values so the operands share a compatible type.

### HM002

#### Example

```py
def self_apply(f):
    return f(f)  # Requires f: T -> T, but T would have to contain itself
```

#### Guidance

Occurs-check failures indicate an infinite recursive type.
Refactor so values are not applied to themselves without a wrapper type, or introduce generics that break the cycle.
See [Type Checking](./type_checking.md#type-system-philosophy) for how recursive types are limited.

### HM003

#### Example

```py
def use_unknown(x: U) -> U:  # U was never declared via TypeVar
    return x
```

#### Guidance

Declare every type variable with `TypeVar` before referencing it: `U = TypeVar("U")`.
The generics workflow is covered in [Type Checking](./type_checking.md#type-inference).

### HM004

#### Example

```py
ids: dict[str] = {}  # dict expects two type arguments
```

#### Guidance

Provide the correct number of arguments for each generic (`dict[str, int]`).
Beacon enforces kind arity to avoid ambiguous instantiations.
See [Type Checking](./type_checking.md#special-types).

### HM005

#### Example

```py
def paradox(x):
    return x(paradox)  # Leads to an infinite type when inferred
```

#### Guidance

Infinite type errors usually stem from higher-order functions that apply un-annotated callables to themselves.
Add annotations to break the cycle or restructure the algorithm so a value is not required to contain itself.

### HM006

#### Example

```py
from typing import Iterable

def consume(xs: Iterable[str]) -> None:
    for item in xs:
        print(item.upper())

consume(10)  # int does not satisfy Iterable[str]
```

#### Guidance

Ensure call arguments implement the required protocol slots or convert them first (wrap values in iterables, implement `__iter__`, etc.).
Protocol behavior is described in [Type Checking](./type_checking.md#type-system-philosophy).

### HM007

#### Example

```py
count = 10
count.splitlines()  # Attribute does not exist on int
```

#### Guidance

The analyzer could not find the attribute on the receiver type.
Narrow the type, convert the value, or fix typos.
Beacon adds contextual hints (e.g., “splitlines is a string method”).
See [Type Checking](./type_checking.md#type-inference) for attribute resolution notes.

### HM008

#### Example

```py
def pair(a: int, b: int) -> None:
    ...

pair(1)  # Missing second positional argument
```

#### Guidance

Match the declared arity (positional + keyword-only + variadic).
Add or remove arguments, or update the function signature.
This follows the call constraint rules in [Type Checking](./type_checking.md#type-system-philosophy).

### HM009

#### Example

```py
def square(x: int) -> int:
    return x * x

square("ten")  # Argument type mismatch
```

#### Guidance

Convert arguments to the expected type or adjust the signature to accept a broader type.
Beacon pinpoints the offending parameter in the diagnostic.

### HM010

#### Example

```py
def parse(match_obj):
    match match_obj:
        case (x, y):  # HM010 if match_obj is inferred as str
            ...
```

#### Guidance

Ensure match subjects and patterns agree (use tuples with tuple subjects, mappings with dicts, etc.).
Pattern typing is detailed in [Pattern Matching Support](./static_analysis.md#pattern-matching-support).

### HM011

#### Example

```py
def connect(host: str, *, ssl: bool) -> None:
    ...

connect("db", secure=True)  # Unknown keyword `secure`
```

#### Guidance

Use valid keyword names, avoid duplicates, and respect positional-only/keyword-only markers.
Adjust the call site or function signature accordingly.

### HM012

#### Example

```py
def capture() -> int:
    cache = []
    def inner():
        cache.append(inner)
        return inner(cache)  # Triggers a generic HM012 error about unsafe recursion
```

#### Guidance

HM012 is a catch-all for rare Hindley–Milner failures (value restriction violations, unsupported constructs).
Inspect the message for context, add annotations to guide inference, or refactor towards supported patterns.
See [Type Checking](./type_checking.md#type-system-philosophy).

### HM013

#### Example

```py
def report(event):
    match event:
        case {"kind": kind, "meta": {"user": user}}:
            ...
```

If `event` is inferred as a tuple or class, the mapping pattern structure mismatches.

#### Guidance

Use patterns whose structure matches the subject (mappings for dicts, class patterns for dataclasses, etc.).
Details live in [Pattern Matching Support](./static_analysis.md#pattern-matching-support).

### HM014

#### Example

```py
pets: list[object] = ["dog", "cat"]  # list is invariant
specific_pets: list[str] = pets  # HM014: cannot assign list[str] to list[object]
```

#### Guidance

Respect variance constraints.
Mutable containers are invariant, so consider using immutable collections (`tuple[str, ...]`) or widening the source type.
The diagnostic message includes targeted advice per position (in/out).
See [Type Checking](./type_checking.md#type-system-philosophy).

### MODE_INFO

#### Example

```text
Type checking mode: balanced (workspace default) - ...
```

#### Guidance

Beacon appends this hint whenever diagnostics appear so you know whether strict/balanced/loose rules applied.
Use `# beacon: mode=strict` (etc.) to override as described in [Type Checking Modes](../type-checking-modes.md).

### PM001

#### Example

```py
def handle(flag: bool) -> str:
    match flag:
        case True:
            return "y"
```

No `False` case triggers PM001.

#### Guidance

Add the missing cases (`case False:` or `case _:`).
Exhaustiveness checking is covered in [Pattern Matching Support](./static_analysis.md#pattern-matching-support).

### PM002

#### Example

```py
match value:
    case _:
        return 0
    case 1:
        return value  # Unreachable after wildcard case
```

#### Guidance

Reorder or delete subsumed patterns so every case is reachable.
See [Pattern Matching Support](./static_analysis.md#pattern-matching-support).

### circular-import

#### Example

```py
# module_a.py
from module_b import helper

# module_b.py
from module_a import helper  # completes a cycle
```

#### Guidance

Break the cycle by moving shared code into a third module, deferring imports inside functions, or rethinking module boundaries
Severity comes from `[diagnostics.circular_imports]` in [Configuration](../configuration.md#diagnostics).

### missing-module

#### Example

```py
import backend.plugins.payments  # File or package missing from workspace/stubs
```

#### Guidance

Add the module to the workspace, fix typos, or adjust your import path.
Beacon reports missing modules as errors because runtime execution would fail immediately.

### shadowed-variable

#### Example

```py
token = "outer"

def handler():
    token = "inner"  # Shadows outer variable
```

#### Guidance

Rename inner variables or move logic closer to usage to avoid surprising shadowing.
The static analyzer describes its scope walk in [Control & Data Flow](./static_analysis.md#control--data-flow).

### undefined-variable

#### Example

```py
print(total)  # `total` never defined
```

#### Guidance

Define the name, import it, or limit the scope where it’s used.
Unlike `use-before-def`, this check runs at the file level via `Analyzer::find_unbound_variables` (see [Static Analyzer](./static_analysis.md)).

### unresolved-import

#### Example

```py
from services import codecs  # services module exists, codecs submodule does not
```

#### Guidance

Fix the module path, add missing files, or install the dependency.
Severity is controlled by `[diagnostics.unresolved_imports]` in [Configuration](../configuration.md#diagnostics).

### unreachable-code

#### Example

```py
def foo():
    return 42
    print("never runs")  # Unreachable
```

#### Guidance

Remove or refactor unreachable statements.
Diagnostics carry the `UNNECESSARY` tag so editors can gray out the code.
Pipeline details sit in [Control & Data Flow](./static_analysis.md#control--data-flow).

### unused-variable

#### Example

```py
def process():
    result = compute()  # Never read later
```

#### Guidance

Use the variable, prefix with `_` to mark as intentionally unused, or delete it.
See [Control & Data Flow](./static_analysis.md#control--data-flow) for how Beacon tracks reads/writes.

### use-before-def

#### Example

```py
def build():
    print(total)
    total = 10  # total read before assignment in this scope
```

#### Guidance

Reorder statements so assignments precede reads, or mark outer-scope variables as `nonlocal`/`global` when appropriate.
Data-flow analysis is described in [Control & Data Flow](./static_analysis.md#control--data-flow).
