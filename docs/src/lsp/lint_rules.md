# Beacon Linter

The Beacon Rule Engine is a modular static analysis system powering diagnostics in Beacon.

It's foundationally a pure Rust implementation of PyFlakes.

---

**Legend:** &#9888; = Warning &#10005; = Error &#9432; = Info

| Code              | Name / RuleKind                         | Level    | Category  | Description                                                           |
| ----------------- | --------------------------------------- | -------- | --------- | --------------------------------------------------------------------- |
| [BEA001](#bea001) | `UndefinedName`                         | &#10005; | Naming    | Variable or function used before being defined.                       |
| [BEA002](#bea002) | `DuplicateArgument`                     | &#10005; | Functions | Duplicate parameter names in a function definition.                   |
| [BEA003](#bea003) | `ReturnOutsideFunction`                 | &#10005; | Flow      | `return` statement outside of a function or method body.              |
| [BEA004](#bea004) | `YieldOutsideFunction`                  | &#10005; | Flow      | `yield` or `yield from` used outside a function context.              |
| [BEA005](#bea005) | `BreakOutsideLoop`                      | &#10005; | Flow      | `break` used outside a `for`/`while` loop.                            |
| [BEA006](#bea006) | `ContinueOutsideLoop`                   | &#10005; | Flow      | `continue` used outside a `for`/`while` loop.                         |
| [BEA007](#bea007) | `DefaultExceptNotLast`                  | &#9888;  | Exception | A bare `except:` is not the final exception handler in a `try` block. |
| [BEA008](#bea008) | `RaiseNotImplemented`                   | &#9888;  | Semantics | Using `raise NotImplemented` instead of `raise NotImplementedError`.  |
| [BEA009](#bea009) | `TwoStarredExpressions`                 | &#10005; | Syntax    | Two or more `*` unpacking expressions in assignment.                  |
| [BEA010](#bea010) | `TooManyExpressionsInStarredAssignment` | &#10005; | Syntax    | Too many expressions when unpacking into a starred target.            |
| [BEA011](#bea011) | `IfTuple`                               | &#9888;  | Logic     | A tuple literal used as an `if` condition — always `True`.            |
| [BEA012](#bea012) | `AssertTuple`                           | &#9888;  | Logic     | Assertion always true due to tuple literal.                           |
| [BEA013](#bea013) | `FStringMissingPlaceholders`            | &#9888;  | Strings   | f-string declared but contains no `{}` placeholders.                  |
| [BEA014](#bea014) | `TStringMissingPlaceholders`            | &#9888;  | Strings   | t-string declared but contains no placeholders.                       |
| [BEA015](#bea015) | `UnusedImport`                          | &#9888;  | Symbols   | Import is never used within the file.                                 |
| [BEA016](#bea016) | `UnusedVariable`                        | &#9888;  | Symbols   | Local variable assigned but never used.                               |
| [BEA017](#bea017) | `UnusedAnnotation`                      | &#9888;  | Symbols   | Annotated variable never referenced.                                  |
| [BEA018](#bea018) | `RedefinedWhileUnused`                  | &#9888;  | Naming    | Variable redefined before original was used.                          |
| [BEA019](#bea019) | `ImportShadowedByLoopVar`               | &#9888;  | Scope     | Import name shadowed by a loop variable.                              |
| [BEA020](#bea020) | `ImportStarNotPermitted`                | &#10005; | Imports   | `from module import *` used inside a function or class.               |
| [BEA021](#bea021) | `ImportStarUsed`                        | &#9888;  | Imports   | `import *` prevents detection of undefined names.                     |
| [BEA022](#bea022) | `UnusedIndirectAssignment`              | &#9888;  | Naming    | Global or nonlocal declared but never reassigned.                     |
| [BEA023](#bea023) | `ForwardAnnotationSyntaxError`          | &#10005; | Typing    | Syntax error in forward type annotation.                              |
| [BEA024](#bea024) | `MultiValueRepeatedKeyLiteral`          | &#9888;  | Dict      | Dictionary literal repeats key with different values.                 |
| [BEA025](#bea025) | `PercentFormatInvalidFormat`            | &#9888;  | Strings   | Invalid `%` format string.                                            |
| [BEA026](#bea026) | `IsLiteral`                             | &#9888;  | Logic     | Comparing constants with `is` or `is not` instead of `==`/`!=`.       |
| [BEA027](#bea027) | `DefaultExceptNotLast`                  | &#9888;  | Exception | Bare `except:` must appear last.                                      |
| [BEA028](#bea028) | `UnreachableCode`                       | &#9888;  | Flow      | Code after a `return`, `raise`, or `break` is never executed.         |
| [BEA029](#bea029) | `RedundantPass`                         | &#9432;  | Cleanup   | `pass` used in a block that already has content.                      |
| [BEA030](#bea030) | `EmptyExcept`                           | &#9888;  | Exception | `except:` with no handling code (silent failure).                     |

## Rules

### BEA001

#### Example

`print(foo)` before `foo` is defined.

#### Fix

Define the variable before use or fix the typo.

### BEA002

#### Example

```py
def f(x, x):
    pass
```

#### Fix

Rename one of the parameters.

### BEA003

#### Example

Top-level `return 5` in a module.

#### Fix

Remove or move inside a function.

### BEA004

#### Example

`yield x` at module scope.

#### Fix

Wrap in a generator function.

### BEA005

#### Example

`break` in global scope or in a function without loop.

#### Fix

Remove or restructure the code to include a loop.

### BEA006

#### Example

`continue` in a function with no loop.

#### Fix

Remove or replace with control flow logic.

### BEA007

#### Example

`except:` followed by `except ValueError:`

#### Fix

Move the `except:` block to the end of the `try`.

### BEA008

#### Example

`raise NotImplemented`

#### Fix

Replace with `raise NotImplementedError`.

### BEA009

#### Example

`a, *b, *c = d`

#### Fix

Only one starred target is allowed.

### BEA010

#### Example

`a, b, c, d = (1, 2, 3)`

#### Fix

Adjust unpacking count.

### BEA011

#### Example

```py
if (x,):
    ...
```

#### Fix

Remove accidental comma or rewrite condition.

### BEA012

#### Example

`assert (x, y)`

#### Fix

Remove parentheses or fix expression.

### BEA013

#### Example

`f"Hello world"`

#### Fix

Remove the `f` prefix if unnecessary.

### BEA014

#### Example

`t"foo"`

#### Fix

Remove the `t` prefix if unused.

### BEA015

#### Example

`import os` not referenced.

#### Fix

Remove the unused import.

### BEA016

#### Example

`x = 10` never referenced again.

#### Fix

Remove assignment or prefix with `_` if intentional.

### BEA017

#### Example

`x: int` declared but unused.

#### Fix

Remove or use variable.

### BEA018

#### Example

`x = 1; x = 2` without reading `x`.

#### Fix

Remove unused definition.

### BEA019

#### Example

```py
import os
for os in range(3):
    ...
```

#### Fix

Rename loop variable.

### BEA020

#### Example

```py
def f():
    from math import *
```

#### Fix

Move import to module level.

### BEA021

#### Example

`from os import *`

#### Fix

Replace with explicit imports.

### BEA022

#### Example

`global foo` never used.

#### Fix

Remove redundant declaration.

### BEA023

#### Example

`def f() -> "List[int": ...`

#### Fix

Fix or quote properly.

### BEA024

#### Example

`{'a': 1, 'a': 2}`

#### Fix

Merge or remove duplicate keys.

### BEA025

#### Example

`"%q" % 3`

#### Fix

Correct format specifier.

### BEA026

#### Example

`x is 5`

#### Fix

Use `==`/`!=`.

### BEA027

#### Example

As above.

#### Fix

Reorder exception handlers.

### BEA028

#### Example

`return 5; print("unreachable")`

#### Fix

Remove or refactor code.

### BEA029

#### Example

```py
def f():
    pass
    return 1
```

#### Fix

Remove redundant `pass`.

### BEA030

#### Example

```py
try:
    ...
except:
    pass
```

#### Fix

Handle exception or remove block.

## Planned

| Name                                | Kind                      | Category  | Severity | Rationale                                                                                                                                                                              |
| ----------------------------------- | ------------------------- | --------- | -------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Mutable Default Argument            | `MutableDefaultArgument`  | Semantic  | &#10005; | Detect functions that use a mutable object (e.g., `list`, `dict`, `set`) as a default argument.                                                                                        |
| Return in Finally                   | `ReturnInFinally`         | Flow      | &#10005; | Catch a `return`, `break`, or `continue` inside a `finally` block: this often suppresses the original exception and leads to subtle bugs.                                              |
| For-Else Without Break              | `ForElseWithoutBreak`     | Flow      | &#9888;  | The `for ... else` construct where the `else` never executes a `break` is confusing and often mis-used. If you have `else:` on a loop but never `break`, you may signal confusing logic. |
| Wrong Exception Caught              | `BroadExceptionCaught`    | Exception | &#9888;  | Catching too broad exceptions (e.g., `except Exception:` or `except:`) instead of specific types can hide bugs. You already have empty except; this expands to overly broad catching.  |
| Inconsistent Return Types           | `InconsistentReturnTypes` | Function  | &#9888;  | A function that returns different types on different paths (e.g., `return int` in one branch, `return None` in another) may lead to consuming code bugs especially if not annotated.   |
| Index / Key Errors Likely           | `UnsafeIndexOrKeyAccess`  | Data      | &#9888;  | Detect patterns that likely lead to `IndexError` or `KeyError`, e.g., accessing list/dict without checking length/keys, especially inside loops.                                       |
| Unused Coroutine / Async Function   | `UnusedCoroutine`         | Symbol    | &#9888;  | In async code: a `async def` function is defined but neither awaited nor returned anywhere — likely a bug.                                                                             |
| Resource Leak / Unclosed Descriptor | `UnclosedResource`        | Symbol    | &#9888;  | Detect file or network resource opened (e.g., `open(...)`) without being closed or managed via context manager (`with`).                                                                 |
| Logging Format String Errors        | `LoggingFormatError`      | String    | &#9888;  | Using `%` or f-string incorrectly in logging calls (e.g., logging format mismatches number of placeholders) can cause runtime exceptions or silent failures.                           |
| Comparison to None Using == / !=    | `NoneComparison`          | Logic     | &#9888;  | Discourage `== None` or `!= None` in favor of `is None` / `is not None`.                                                                                                               |
