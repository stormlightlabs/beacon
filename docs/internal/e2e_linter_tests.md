# Linter Test Suite

Linter integration tests mirror the formatter suite: realistic fixtures run end
to end, and tests assert on emitted diagnostics/fixes inline. No golden outputs
or CI-only harnesses are needed.

## Test Files

1. `linter_e2e.rs`
   - Loads fixtures through `DocumentManager`.
   - Runs the lint walker and asserts on diagnostic strings, severities, and ranges.
2. Fixture folders (`samples/e2e/lint/`)
   - `web_api/` (FastAPI + aiohttp).
   - `data_pipeline/` (Pandas/NumPy).
   - `mixed_quality/` (intentionally messy script).

## Coverage Highlights

- Validates rule stability: unused imports, async misuse, walrus patterns, etc.
- Exercises autofix code paths by applying fixes in-memory and re-running lint.

## Running Tests

```bash
cargo test --package beacon-lsp --test linter_e2e
```

## Known Gaps

- Autofix verification currently optional—add structured helpers when fixes expand.

## Guidelines / Next Steps

1. Keep diagnostics sorted deterministically for easier assertions.
2. Share fixtures with formatter/analysis suites where useful.
3. TODOs:
   - [ ] Add helper `assert_lints` util to compare `(rule_id, message, span)` tuples.
   - [ ] Populate fixture directories listed above.
