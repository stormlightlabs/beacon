# Parser Test Suite

Parser integration tests mimic the formatter regression style: embed realistic
Python snippets, run them through `PythonParser`, and assert directly on the
resulting AST strings/fields. No golden files or CI-only harness is required.

## Test Files

1. `parser_tests.rs`
   - Calls `PythonParser::parse`/`to_ast` per fixture.
   - Uses inline assertions (`assert!(ast_string.contains(...))`, `assert_eq!(tuple.is_parenthesized, true)`) just like the formatter tests.
2. Fixture sources (inline or under `samples/e2e/parser/`)
   - `advanced_syntax.py` (walrus, match/case, comprehensions).
   - `typing_heavy.py` (`TypeVar`, generics, subscripts).
   - `pattern_matching.py` (nested patterns, guards).

## Coverage Highlights

- Tuple destructuring with `is_parenthesized`.
- Lambda parameters and nested named expressions.
- Modern syntax (3.8–3.12) plus compatibility constructs that the formatter also exercises.

## Running Tests

```bash
cargo test --package beacon-parser --test parser_e2e
```

## Known Gaps

None - all identified gaps have been resolved.

## Core Test Cases Checklist

### Basic Expressions

- [x] Simple literals (strings, numbers, booleans, None)
- [x] Binary operations (arithmetic, comparison, logical)
- [x] Unary operations (negation, bitwise)
- [x] Parenthesized expressions

### Tuples and Collections

- [x] Tuple with parentheses (is_parenthesized = true)
- [x] Tuple without parentheses (is_parenthesized = false)
- [x] Tuple destructuring in assignments
- [x] List and dict literals

### Functions and Lambdas

- [x] Lambda with single parameter
- [x] Lambda with multiple parameters
- [x] Lambda with default arguments
- [x] Nested lambdas

### Modern Syntax (3.8+)

- [x] Walrus operator (named expressions)
- [x] Walrus in comprehensions
- [x] Walrus in conditionals
- [x] Nested walrus expressions

### Pattern Matching (3.10+)

- [x] Basic match statement
- [x] Pattern matching with guards
- [x] Nested patterns
- [x] Multiple match cases

### Type Annotations

- [x] Function parameter annotations
- [x] Return type annotations
- [x] Generic types with subscripts
- [x] TypeVar declarations

### Comprehensions

- [x] List comprehension
- [x] Dict comprehension
- [x] Set comprehension
- [x] Generator expression
- [x] Nested comprehensions

### Control Flow

- [x] If-elif-else chains
- [x] While loops
- [x] For loops with multiple targets
- [x] Try-except-finally blocks

### Call Chains and Method Chaining

- [x] Attribute call chains (e.g., `obj.method1().method2()`)
- [x] Function call chains
- [x] Calls with lambda arguments
- [x] Deeply nested calls
- [x] Mixed attribute and call chains

## Guidelines / Next Steps

1. Keep tests deterministic and lightweight—mirror formatter regression assertions.
2. Prefer several focused fixtures over a single mega file.
3. TODOs:
   - [ ] Add helper utilities for formatting AST nodes into readable strings.
   - [ ] Seed fixtures listed above with concrete assertions.
