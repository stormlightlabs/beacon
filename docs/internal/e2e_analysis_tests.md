# Static Analysis Test Suite

These tests behave like the formatter suite: fixtures run end-to-end through the
analysis pipeline, and assertions check diagnostics/Cfg stats inline without
golden artifacts or CI-only harnesses.

## Test Files

1. `analysis_e2e.rs`
   - Loads project fixtures via `DocumentManager`.
   - Runs CFG/data-flow/lint analysis, asserting on counts and diagnostic strings.
2. Fixture directories under `samples/e2e/analysis/`
   - `cfg_zoo/` (branching/control-flow edge cases).
   - `web_api/` (FastAPI service with async patterns).
   - `packages/` (multi-module import hierarchy).

## Coverage Highlights

- CFG reachability / block counts.
- Data-flow diagnostics (use-before-def, hoisting).
- Lint messages that formatter tests also care about (async context, walrus use).

## Running Tests

```bash
cargo test --package beacon-lsp --test analysis_e2e
```

## Known Gaps

- No automated dot-graph comparison yet; keep assertions textual for now.

## Guidelines / Next Steps

1. Mirror formatter regressions: expect concrete strings/ranges rather than snapshot blobs.
2. Share fixtures with LSP tests when possible.
3. TODOs:
   - [ ] Define helper to compare sorted diagnostics per file.
   - [ ] Add fixtures described above with precise assertions.
