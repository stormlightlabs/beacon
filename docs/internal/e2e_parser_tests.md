# Parser Test Suite

Parser integration tests mimic the formatter regression style: embed realistic
Python snippets, run them through `PythonParser`, and assert directly on the
resulting AST strings/fields. No golden files or CI-only harness is required.

## Test Files

1. `parser_e2e.rs`
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

- Attribute call chains (`compose(lambda…, lambda…)`) are still flattened; emit nested `Call` + `Lambda` nodes in a follow-up.

## Guidelines / Next Steps

1. Keep tests deterministic and lightweight—mirror formatter regression assertions.
2. Prefer several focused fixtures over a single mega file.
3. TODOs:
   - [ ] Add helper utilities for formatting AST nodes into readable strings.
   - [ ] Seed fixtures listed above with concrete assertions.
