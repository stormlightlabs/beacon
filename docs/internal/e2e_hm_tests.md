# Hindley–Milner Test Suite

HM integration tests should feel like the formatter regressions: standalone Rust
tests feed real programs through the solver and assert directly on the inferred
types/diagnostics—no snapshot harness or CI-only job required.

## Test Files

1. `hm_e2e.rs`
   - Parses fixtures, builds constraint graphs, runs the HM solver.
   - Uses direct assertions on inferred type strings and diagnostic text.
2. Fixtures (`samples/e2e/hm/`)
   - `functional_pipeline.py` (higher-order functions).
   - `typed_api.py` (`Protocol`, `TypedDict`, `TypeVar` bounds).
   - `async_ctx.py` (async managers).
   - `errors/` (occurs-check, variance failures).

## Coverage Highlights

- Ensures generalization works (compare type strings per symbol).
- Verifies solver diagnostics include source spans and messages.
- Couples parser metadata (tuples, lambda params) with constraint generation.

## Running Tests

```bash
cargo test --package beacon-lsp --test hm_e2e
```

## Known Gaps

- Constraint graph pretty-printer still minimal; assertions currently focus on type strings + diagnostics.

## Guidelines / Next Steps

1. Keep assertions readable (`assert_eq!(ty("compose"), "Callable[[...], ...]")`).
2. Build up fixtures incrementally and reuse them in formatter/analyzer regressions where helpful.
3. TODOs:
   - [ ] Implement helper functions to fetch inferred types by symbol.
   - [ ] Populate fixture set described above.
