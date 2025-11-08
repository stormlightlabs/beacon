# Contributing to Beacon

Thanks for helping make Beacon better! This document walks through the expectations, tooling, and workflows we follow when working on this collosal project.

## Community expectations

- Be respectful and collaborative. Assume good intent, respond with empathy, and seek consensus.
- Discuss sizeable changes in an issue or discussion thread before you code so we can align on scope and architecture.
- By submitting code or documentation you agree that your contribution is released under the [MIT License](LICENSE).

## How you can help

- **Bug reports** – include version, platform, Python sample, expected vs. actual behaviour, and logs/diagnostics (`beacon-cli … --format json` helps).
- **Feature proposals/requests** – outline the problem and why it belongs in Beacon.
- **Code contributions** – improvements across Rust crates (`crates/*`), the CLI, LSP server, or editor packages in `pkg/`.
- **Docs & samples** – expand `docs/src`, add walkthroughs, or provide new programs under `samples/` that highlight tricky typing scenarios.

### Filing issues

- Provide the Beacon version (`beacon-cli --version`), OS, Python version, and reproduction steps.
- Mention whether the problem surfaces in CLI, LSP, or a specific editor plugin.

## Prerequisites

- Rust stable (latest toolchain via `rustup toolchain install stable`) and Cargo.
- `pnpm` (Node.js ≥ 18) for the editor packages and shared dev tooling defined in `pnpm-workspace.yaml`.
- `mdbook` for documentation changes (`cargo install mdbook`), optional unless you touch `docs/`.
- Python 3.12+ if you want to exercise the CLI or reproduce user reports locally.

## Local setup

1. Fork and clone the repository, then add the upstream remote so you can rebase easily.
2. Install dependencies once:

   ```sh
   pnpm install            # respects pnpm-workspace.yaml
   cargo fetch             # primes the cargo cache for offline builds
   ```

3. Verify things build:

   ```sh
   just build
   just test
   ```

4. When iterating on the VS Code extension (or other editor packages), work inside the corresponding `pkg/<editor>` directory and rely on the scripts in its `package.json`.

## Development workflow

### Branching and issues

- Keep feature branches small and focused. Rebase on `main` instead of merging to keep history clean.
- Reference the issue you are addressing in your commit messages and PR description.
- If work touches multiple areas (e.g., core + VS Code client), split the changes into logical commits so reviewers can reason about each step.

### Rust crates

- Run formatters and lints locally:

  ```sh
  cargo fmt --all
  cargo clippy --all-targets --all-features -- -D warnings
  ```

- Tests should cover new business logic. Prefer unit tests inside each crate.
- Use the helpers in the `justfile` (`just test-parser`, `just test-cli`, `just lint`) to scope your runs.
- Keep an eye on `[workspace.lints.clippy]` in `Cargo.toml`; Clippy denies (e.g., `bool_comparison`, `mem_forget`) will fail CI.
- `lsp-types` is intentionally pinned (see `Cargo.toml`). Chat with maintainers before updating it because downstream protocol compatibility matters.

### VSCode (`pkg/vscode`)

- Install JS/TS deps once with `pnpm --filter <package> install`.
- Build and lint before committing:

  ```sh
  pnpm --filter vscode compile
  pnpm --filter vscode lint
  ```

- Follow the TypeScript configuration in `pkg/vscode/tsconfig.json` and formatting rules in `pkg/vscode/dprint.json`.
- When you change the LSP surface, update the relevant editor bindings and documentation (README & `extensions/` in `docs`)

### Documentation (`docs/`)

Sources live in `docs/src`. Run `mdbook serve docs --open` for local previews or `mdbook build docs` in CI mode.
This is optional but helps validate links in the SUMMARY.md file. Be sure to keep diagrams in `docs/book` up to date when architecture shifts, and cross-link to reference material in `README.md` when possible.

### Coverage & debugging

- CI enforces coverage via Codecov. To reproduce locally:

  ```sh
  just coverage-lcov     # produces lcov.info consumed by CI
  just coverage          # opens an HTML report via cargo-llvm-cov
  ```

- Use `just demo` or `beacon-cli debug <subcommand>` to capture ASTs, constraints, and solver output when explaining changes in your PR.

### Logging

See [docs](./docs/src/lsp/logging.md)

## Pull request checklist

- [ ] Discussed large design changes beforehand.
- [ ] Code builds (`cargo build --workspace`) and tests pass (`cargo test --workspace`).
- [ ] `cargo fmt`, `cargo clippy`, and any editor-package lints all succeed.
- [ ] Added or updated tests plus documentation (README snippets, docs, samples) when behaviour changes.
- [ ] Included reproduction steps or sample programs for bug fixes under `samples/` when helpful.
- [ ] Confirmed `pnpm lint`, `pnpm compile`, and `mdbook build docs` (if touched) pass.
- [ ] Verified that generated files (coverage artifacts, target/, node_modules/) are not committed.

## Questions?

Open a GitHub discussion or reach out via the issue tracker. Let's talk through unblocking your contributions, talk through design trade-offs, or finding a good starter issue.
