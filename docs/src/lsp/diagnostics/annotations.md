# Annotations Diagnostics

Annotation diagnostics cover mismatches between declared types and inferred usage along with mode-specific requirements for annotations.

## ANN001 – `AnnotationMismatch` {#ann001}

### Example

```py
value: int = "stale"  # Annotated as int, inferred as str
```

### Guidance

Annotation/inference mismatches inherit their severity from the active mode (strict → error, balanced → warning, loose → hint).
Align the annotation with real usage, or change the code so the inferred type matches.
See [Type Checking](../type_checking.md#type-inference) and [Type Checking Modes](../../type-checking-modes.md) for the inference rules.

## ANN002 – `MissingVariableAnnotation` {#ann002}

### Example

```py
# beacon: mode=strict
profile = load_profile()  # Missing annotation (ANN002)
```

### Guidance

Strict/balanced modes expect assignments with concrete inferred types to be annotated.
Add the appropriate annotation (`profile: Profile = load_profile()`), or downgrade the file to loose mode if intentional (see [Type Checking Modes](../../type-checking-modes.md)).

## ANN003 – `ParameterAnnotationMismatch` {#ann003}

### Example

```py
def greet(name: str) -> str:
    return name + 1  # name inferred as int due to arithmetic
```

### Guidance

Parameter annotations must agree with how the function body uses the value.
Update the annotation or refactor the body to respect it.
Details about how inference follows parameter usage live in [Type Checking](../type_checking.md#type-inference).

## ANN004 – `MissingParameterAnnotation` {#ann004}

### Example

```py
def send_email(address):
    ...
```

Balanced/strict modes infer `address: str` (or similar) and emit ANN004.

### Guidance

Add explicit parameter annotations whenever inference is concrete: `def send_email(address: str) -> None:`.
Loose mode skips this check entirely (see [Type Checking Modes](../../type-checking-modes.md)).

## ANN005 – `ReturnAnnotationMismatch` {#ann005}

### Example

```py
def parity(flag: bool) -> bool:
    return "odd"  # Return annotation mismatch
```

### Guidance

Ensure return annotations reflect every path.
Either return the annotated type or adjust the annotation.
See [Type Checking](../type_checking.md#type-system-philosophy) for how Beacon treats return types.

## ANN006 – `MissingReturnAnnotation` {#ann006}

### Example

```py
def total(values):
    return sum(values)
```

Balanced/strict modes infer a concrete return type (e.g., `int`) and require `-> int`.

### Guidance

Add return annotations when inference is precise and not `Any`/`None`: `def total(values: list[int]) -> int:`.
Loose mode suppresses this requirement (see [Type Checking Modes](../../type-checking-modes.md)).

## ANN007 – `ImplicitAnyParameter` {#ann007}

### Example

```py
# beacon: mode=strict
def transform(data):
    return data.strip()
```

### Guidance

Strict mode disallows implicit `Any` on parameters even when inference could deduce a type.
Add annotations for every parameter (`data: str`). Balanced/loose modes emit ANN004 instead or skip the check entirely.
Review [Type Checking Modes](../../type-checking-modes.md) for severity rules.

## ANN008 – `ImplicitAnyReturn` {#ann008}

### Example

```py
# beacon: mode=strict
def make_id():
    return uuid.uuid4().hex  # Implicit Any return type
```

### Guidance

Strict mode requires explicit return annotations on every function.
Provide the exact type (`-> str`) or loosen the file mode if you intentionally rely on inference.
See [Type Checking Modes](../../type-checking-modes.md) for override syntax.

## ANN009 – `MissingClassAttributeAnnotation` {#ann009}

### Example

```py
# beacon: mode=strict
class Configuration:
    host = "localhost"  # Missing type annotation
    port: int = 8080  # OK: Has annotation
```

### Guidance

Strict mode requires explicit type annotations on all class attributes.
Add the annotation (`host: str = "localhost"`) or use balanced/loose mode if gradual typing is preferred.
Note that instance attributes (assigned in `__init__` or other methods) are not subject to this check—only class-level attributes defined directly in the class body.
See [Type Checking Modes](../../type-checking-modes.md) for mode configuration.

## ANN010 – `BareExceptClause` {#ann010}

### Example

```py
# beacon: mode=strict
def process_data():
    try:
        result = risky_operation()
    except:  # ANN010: Bare except clause not allowed
        handle_error()
```

### Guidance

Strict mode requires specific exception types in `except` clauses to prevent catching system exceptions like `KeyboardInterrupt` or `SystemExit` unintentionally.
Replace bare `except:` with specific exception types:

```py
# Good: Specific exception type
except ValueError:
    ...

# Good: Multiple exception types
except (ValueError, TypeError):
    ...

# Good: Catch most exceptions but not system ones
except Exception:
    ...
```

Balanced and loose modes allow bare except clauses for gradual adoption.
See [Type Checking Modes](../../type-checking-modes.md) for mode configuration.

## ANN011 – `ParameterImplicitAny` {#ann011}

### Example

```py
# beacon: mode=balanced
def process_unknown(data, options):
    return data  # ANN011: 'data' and 'options' have implicit Any type
```

### Guidance

Balanced mode distinguishes between concrete inferred types (which trigger ANN004 with type suggestions) and implicit Any (which triggers ANN011).
When type inference cannot determine a concrete type due to insufficient context, parameters are finalized as `Any` and this warning is emitted.

Add type annotations to clarify the intended types:

```py
# Good: Explicit annotations remove ambiguity
def process_unknown(data: dict[str, Any], options: dict[str, str]) -> dict[str, Any]:
    return data
```

This diagnostic helps identify truly ambiguous cases where annotations provide the most value.
Strict mode reports all missing parameter annotations as ANN007 errors instead.
See [Type Checking Modes](../../type-checking-modes.md) for inference behavior.

## ANN012 – `ReturnImplicitAny` {#ann012}

### Example

```py
# beacon: mode=balanced
def handle_dynamic(value):
    print(value)  # ANN012: Return type is implicit Any
```

### Guidance

When a function's return type cannot be inferred to a concrete type, balanced mode warns with ANN012.
This differs from ANN006, which fires when inference determines a concrete type but the annotation is missing.

Add an explicit return type annotation:

```py
# Good: Explicit return type
def handle_dynamic(value: Any) -> None:
    print(value)
```

For functions with implicit Any returns, consider whether:

- The return type should be `None` (procedures)
- You need to add annotations to parameters to enable better inference
- The function genuinely needs `-> Any` due to dynamic behavior

Strict mode reports all missing return annotations as ANN008 errors instead.
See [Type Checking Modes](../../type-checking-modes.md) for the distinction between concrete inference and implicit Any.
