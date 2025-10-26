# Static Analyzer

Beacon's language server leans on a modular static-analysis stack housed in `crates/server/src/analysis`.
The subsystem ingests a parsed document, infers types, builds control-flow graphs, and produces diagnostics that drive editor features like hovers and squiggles.
The sections below highlight the moving pieces without diving into implementation minutiae.

## Pipeline Overview

`Analyzer::analyze` is the high-level orchestration point:

1. Grab a consistent AST + symbol table snapshot from the `DocumentManager`.
2. Walk the tree with a `TypeEnvironment` to emit lightweight constraints that describe how expressions relate to each other.
3. Invoke the shared `beacon_core` unifier to solve those constraints, capturing any mismatches as `TypeErrorInfo`.
4. Build function-level control-flow graphs and run data-flow passes to uncover use-before-def, unreachable code, and unused symbols.
5. Package the inputs, inferred data, and diagnostics into an `AnalysisResult`, which the cache stores per URI for quick repeat lookups.

Hover/type-at-position still rely on a future `type_map`, but the analyzer already produces the substitution data required to implement it.

## Type Inference in Brief

`type_env.rs` supplies the Hindley–Milner style environment that powers constraint generation.
It seeds built-in symbols, hydrates annotations, and hands out fresh type variables whenever the AST does not provide one.
Each visit to a `FunctionDef`, assignment, call, or control-flow node updates the environment and records the relationships that must hold; the actual solving is deferred so the analyzer can collect all obligations before touching the unifier.
This keeps the pass linear, side-effect free, and easy to extend with new AST constructs.

Once constraints reach `solve_constraints`, they are unified in order. Successful unifications compose into a substitution map, while failures persist with span metadata so editor clients can render precise diagnostics.
Attribute constraints are stubbed out for now, leaving room for structural typing or row-polymorphic records later.

## Control & Data Flow

`cfg.rs` and `data_flow.rs` provide the structural analyses that complement pure typing:

- The CFG builder splits a function body into `BasicBlock`s linked by typed edges (normal flow, branch outcomes, loop exits, exception edges, etc.), mirroring Python semantics closely enough for downstream passes to reason about reachability.
- The data-flow analyzer consumes that graph plus the original AST slice to flag common hygiene issues: variables read before assignment, code that cannot execute, and symbols that never get used.
Results surface through `DataFlowResult` and end up in the final `AnalysisResult`.

This layered approach lets the LSP report both type-level and flow-level problems in a single request, keeping feedback tight while avoiding duplicate walks of the AST.

## Diagnostics, Utilities, and Future Work

Beyond inference and CFG analysis, the module exposes helpers for locating unbound identifiers, invalidating cached results when documents change, and bridging between symbol-table scopes and LSP positions.
Outstanding work includes filling the `type_map` for hover support, tightening attribute/type-guard handling, and scaling CFG/data-flow analysis to whole modules.
The current design purposefully isolates each responsibility so new passes (e.g., constant propagation) can slot in without reworking the rest of the stack.
