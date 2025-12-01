# Benchmarking

Beacon uses [Criterion](https://github.com/bheisler/criterion.rs) for performance benchmarking across critical paths.

## Running Benchmarks

Execute all benchmarks:

```bash
cargo bench
```

Run specific benchmark suite:

```bash
cargo bench --bench type_inference
cargo bench --bench parser_benchmark
cargo bench --bench lsp_handlers
```

Criterion generates HTML reports in `target/criterion/` with detailed statistics, plots, and regression detection.

## Benchmark Suites

### Type Inference (`beacon-core`)

Located at `crates/core/benches/type_inference.rs`, this suite tests the core type system:

- **Simple Unification**: Concrete types and type variables
- **Generic Types**: Lists, dictionaries with varying complexity
- **Function Types**: Simple, multi-argument, and generic functions
- **Nested Types**: Lists nested to varying depths
- **Substitution**: Composition and application with different sizes

### Parser Performance (`beacon-server`)

Located at `crates/server/benches/parser_benchmark.rs`, tests parse operations:

- **File Size Scaling**: Small, medium, and large Python files
- **AST Construction**: Parse tree to AST transformation
- **Incremental Reparsing**: Performance of incremental updates
- **Symbol Tables**: Generation across different file sizes

### LSP Handlers (`beacon-server`)

Located at `crates/server/benches/lsp_handlers.rs`, measures LSP operation latency:

- **Hover**: Variable and function hover info generation
- **Completion**: Attribute completion and in-body completion
- **Go to Definition**: Navigation for variables, functions, and classes
- **Combined Operations**: Parse + hover to measure end-to-end cost

## Adding Benchmarks

Create new benchmark files in `crates/{crate}/benches/` and register in `Cargo.toml`:

```toml
[[bench]]
name = "my_benchmark"
harness = false
```

Use Criterion's parametric benchmarks to test performance across different input sizes or scenarios.

## Interpreting Results

Criterion provides:

- Throughput and iteration time statistics
- Confidence intervals
- Regression detection against previous runs
- Visual plots in HTML reports

Monitor the benchmark reports to catch performance regressions during development.
