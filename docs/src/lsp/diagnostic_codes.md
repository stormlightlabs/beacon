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

Note that per-mode rows show the icon used in strict / balanced / relaxed order

| Code                                                                      | Name                              | Level                              | Category        | Description                                                          |
| ------------------------------------------------------------------------- | --------------------------------- | ---------------------------------- | --------------- | -------------------------------------------------------------------- |
| [ANY001](./diagnostics/type_safety.md#any001)                             | `UnsafeAnyUsage`                  | &#9888;                            | Type Safety     | Deep inference found an `Any` value, reducing type safety.           |
| [ANN001](./diagnostics/annotations.md#ann001)                             | `AnnotationMismatch`              | &#10005;&nbsp;&#9888;&nbsp;&#9432; | Annotations     | Declared annotation disagrees with the inferred type.                |
| [ANN002](./diagnostics/annotations.md#ann002)                             | `MissingVariableAnnotation`       | &#10005; &#9888;                   | Annotations     | Assignment lacks an annotation in strict/balanced modes.             |
| [ANN003](./diagnostics/annotations.md#ann003)                             | `ParameterAnnotationMismatch`     | &#10005; &#9888; &#9432;           | Annotations     | Parameter annotation conflicts with inferred usage.                  |
| [ANN004](./diagnostics/annotations.md#ann004)                             | `MissingParameterAnnotation`      | &#10005; &#9888;                   | Annotations     | Parameter missing annotation when inference is precise.              |
| [ANN005](./diagnostics/annotations.md#ann005)                             | `ReturnAnnotationMismatch`        | &#10005; &#9888; &#9432;           | Annotations     | Function return annotation disagrees with inference.                 |
| [ANN006](./diagnostics/annotations.md#ann006)                             | `MissingReturnAnnotation`         | &#10005; &#9888;                   | Annotations     | Function lacks return annotation when inference is concrete.         |
| [ANN007](./diagnostics/annotations.md#ann007)                             | `ImplicitAnyParameter`            | &#10005;                           | Annotations     | Strict mode forbids implicit `Any` on parameters.                    |
| [ANN008](./diagnostics/annotations.md#ann008)                             | `ImplicitAnyReturn`               | &#10005;                           | Annotations     | Strict mode forbids implicit `Any` return types.                     |
| [ANN009](./diagnostics/annotations.md#ann009)                             | `MissingClassAttributeAnnotation` | &#10005;                           | Annotations     | Strict mode requires explicit annotations on class attributes.       |
| [ANN010](./diagnostics/annotations.md#ann010)                             | `BareExceptClause`                | &#10005;                           | Annotations     | Strict mode forbids bare `except:` clauses without exception types.  |
| [ANN011](./diagnostics/annotations.md#ann011)                             | `ParameterImplicitAny`            | &#9888;                            | Annotations     | Balanced mode warns when parameter type resolves to implicit `Any`.  |
| [ANN012](./diagnostics/annotations.md#ann012)                             | `ReturnImplicitAny`               | &#9888;                            | Annotations     | Balanced mode warns when return type resolves to implicit `Any`.     |
| [DUNDER_INFO](./diagnostics/dunder_patterns.md#dunder_info)               | `EntryPointGuard`                 | &#9432;                            | Dunder Patterns | Highlights `if __name__ == "__main__":` guard blocks.                |
| [DUNDER001](./diagnostics/dunder_patterns.md#dunder001)                   | `MagicMethodOutOfScope`           | &#9888;                            | Dunder Patterns | Magic methods defined outside a class.                               |
| [HM001](./diagnostics/type_system.md#hm001)                               | `TypeMismatch`                    | &#10005;                           | Type System     | Hindley–Milner could not unify two types.                            |
| [HM002](./diagnostics/type_system.md#hm002)                               | `OccursCheckFailed`               | &#10005;                           | Type System     | Recursive type variable detected (infinite type).                    |
| [HM003](./diagnostics/type_system.md#hm003)                               | `UndefinedTypeVar`                | &#10005;                           | Type System     | Referenced type variable was never declared.                         |
| [HM004](./diagnostics/type_system.md#hm004)                               | `KindMismatch`                    | &#10005;                           | Type System     | Wrong number of type arguments supplied to a generic.                |
| [HM005](./diagnostics/type_system.md#hm005)                               | `InfiniteType`                    | &#10005;                           | Type System     | Inference produced a non-terminating type (self-referential).        |
| [HM006](./diagnostics/type_system.md#hm006)                               | `ProtocolNotSatisfied`            | &#10005;                           | Type System     | Value fails to implement the required protocol methods.              |
| [HM007](./diagnostics/attributes.md#hm007)                                | `AttributeNotFound`               | &#10005;                           | Attributes      | Attribute or method does not exist on the receiver type.             |
| [HM008](./diagnostics/type_system.md#hm008)                               | `ArgumentCountMismatch`           | &#10005;                           | Type System     | Call site passes too many or too few arguments.                      |
| [HM009](./diagnostics/type_system.md#hm009)                               | `ArgumentTypeMismatch`            | &#10005;                           | Type System     | Argument type incompatible with the parameter type.                  |
| [HM010](./diagnostics/pattern_typing.md#hm010)                            | `PatternTypeMismatch`             | &#10005;                           | Pattern Typing  | Match/case pattern cannot match the subject type.                    |
| [HM011](./diagnostics/type_system.md#hm011)                               | `KeywordArgumentError`            | &#10005;                           | Type System     | Unknown or duplicate keyword arguments in a call.                    |
| [HM012](./diagnostics/type_system.md#hm012)                               | `GenericTypeError`                | &#10005;                           | Type System     | Catch-all Hindley–Milner error (value restriction, etc.).            |
| [HM013](./diagnostics/pattern_typing.md#hm013)                            | `PatternStructureMismatch`        | &#10005;                           | Pattern Typing  | Pattern shape (mapping, class, sequence) differs from subject.       |
| [HM014](./diagnostics/variance.md#hm014)                                  | `VarianceError`                   | &#10005;                           | Variance        | Invariant/covariant/contravariant constraint violated.               |
| [MODE_INFO](./diagnostics/mode.md#mode_info)                              | `TypeCheckingMode`                | &#9432;                            | Mode            | Reminder showing which type-checking mode produced diagnostics.      |
| [PM001](./diagnostics/patterns.md#pm001)                                  | `PatternNonExhaustive`            | &#10005;                           | Patterns        | Match statement fails to cover every possible case.                  |
| [PM002](./diagnostics/patterns.md#pm002)                                  | `PatternUnreachable`              | &#10005;                           | Patterns        | Later pattern is shadowed by an earlier one.                         |
| [circular-import](./diagnostics/imports.md#circular-import)               | `CircularImport`                  | &#10005; &#9888; &#9432;           | Imports         | Module participates in an import cycle (severity comes from config). |
| [missing-module](./diagnostics/imports.md#missing-module)                 | `MissingModule`                   | &#10005;                           | Imports         | Referenced module is absent from the workspace/stubs.                |
| [shadowed-variable](./diagnostics/scope.md#shadowed-variable)             | `ShadowedVariable`                | &#9888;                            | Scope           | Inner scope reuses a name that already exists in an outer scope.     |
| [undefined-variable](./diagnostics/name_resolution.md#undefined-variable) | `UndefinedVariable`               | &#10005;                           | Name Resolution | Name used before being defined anywhere.                             |
| [unresolved-import](./diagnostics/imports.md#unresolved-import)           | `UnresolvedImport`                | &#10005; &#9888; &#9432;           | Imports         | Import target cannot be resolved (severity configurable).            |
| [unreachable-code](./diagnostics/data_flow.md#unreachable-code)           | `UnreachableCode`                 | &#9888;                            | Data Flow       | Code after `return`, `raise`, or `break` never executes.             |
| [unused-variable](./diagnostics/data_flow.md#unused-variable)             | `UnusedVariable`                  | &#9432;                            | Data Flow       | Variable assigned but never read.                                    |
| [use-before-def](./diagnostics/data_flow.md#use-before-def)               | `UseBeforeDef`                    | &#10005;                           | Data Flow       | Variable read before it is assigned in the current scope.            |

## Diagnostics by Category

- [Type Safety Diagnostics](./diagnostics/type_safety.md)
- [Annotation Diagnostics](./diagnostics/annotations.md)
- [Dunder Pattern Diagnostics](./diagnostics/dunder_patterns.md)
- [Type System Diagnostics](./diagnostics/type_system.md)
- [Attribute Diagnostics](./diagnostics/attributes.md)
- [Pattern Typing Diagnostics](./diagnostics/pattern_typing.md)
- [Variance Diagnostics](./diagnostics/variance.md)
- [Mode Diagnostics](./diagnostics/mode.md)
- [Pattern Diagnostics](./diagnostics/patterns.md)
- [Import Diagnostics](./diagnostics/imports.md)
- [Scope Diagnostics](./diagnostics/scope.md)
- [Name Resolution Diagnostics](./diagnostics/name_resolution.md)
- [Data-Flow Diagnostics](./diagnostics/data_flow.md)
