# Static Analyzer

Beacon's language server leans on a modular static-analysis stack housed in `crates/server/src/analysis`.
The subsystem ingests a parsed document, infers types, builds control-flow graphs, runs pattern exhaustiveness checks, and produces diagnostics that drive editor features like hovers and squiggles.

## Pipeline Overview

`Analyzer::analyze` is the high-level orchestration point:

1. Grab a consistent AST + symbol table snapshot from the `DocumentManager`.
2. Check analysis cache for a previously computed result at this document version.
3. Extract scopes and check scope-level cache for incremental analysis opportunities.
4. Walk the tree to emit lightweight constraints describing how expressions relate (equality, calls, attributes, protocols, patterns).
5. Build a class registry containing metadata for all class definitions (fields, methods, protocols, inheritance).
6. Invoke the shared `beacon_core` unifier to solve constraints, capturing any mismatches as `TypeErrorInfo`.
7. Apply the resulting substitution to refine all inferred types in the type map.
8. Build function-level control-flow graphs and run data-flow passes to uncover use-before-def, unreachable code, and unused symbols.
9. Package the inputs, inferred data, and diagnostics into an `AnalysisResult`, caching at both scope and document level for quick repeat lookups.

The analyzer produces a `type_map` linking AST node IDs to inferred types and a `position_map` linking source positions to nodes, enabling hover and type-at-position queries.

## Type Inference in Brief

`type_env.rs` supplies the Hindley–Milner style environment that powers constraint generation.
It seeds built-in symbols, hydrates annotations, and hands out fresh type variables whenever the AST does not provide one.
Each visit to a `FunctionDef`, assignment, call, or control-flow node updates the environment and records the relationships that must hold; the actual solving is deferred so the analyzer can collect all obligations before touching the unifier.
This keeps the pass linear, side-effect free, and easy to extend with new AST constructs.

The constraint system supports multiple relationship types:

- **Equal**: Type equality constraints (t1 ~ t2)
- **Call**: Function application with argument and return types
- **HasAttr**: Attribute access with method binding and inheritance resolution
- **Protocol**: Structural conformance checks for both built-in protocols (Iterable, Iterator, Sequence, AsyncIterable, AsyncIterator, Awaitable) and user-defined Protocol classes
- **MatchPattern**: Pattern matching with binding extraction
- **PatternExhaustive**: Exhaustiveness checking for match statements
- **PatternReachable**: Reachability checking to detect unreachable patterns

Once constraints reach `solve_constraints`, they are unified in order. Successful unifications compose into a substitution map, while failures persist with span metadata so editor clients can render precise diagnostics.
The class registry enables attribute resolution with full inheritance support, overload resolution for methods decorated with `@overload`, and structural protocol checking for user-defined Protocol classes.

## Control & Data Flow

`cfg.rs` and `data_flow.rs` provide the structural analyses that complement pure typing:

- The CFG builder splits a function body into `BasicBlock`s linked by typed edges (normal flow, branch outcomes, loop exits, exception edges, etc.), mirroring Python semantics closely enough for downstream passes to reason about reachability.
- The data-flow analyzer consumes that graph plus the original AST slice to flag common hygiene issues: variables read before assignment, code that cannot execute, and symbols that never get used.
Results surface through `DataFlowResult` and end up in the final `AnalysisResult`.

This layered approach lets the LSP report both type-level and flow-level problems in a single request, keeping feedback tight while avoiding duplicate walks of the AST.

## Class Metadata & Method Resolution

The `class_metadata` module tracks comprehensive information about class definitions:

- **Fields**: Inferred from assignments in `__init__` and class body
- **Methods**: Including support for overload sets via `@overload` decorator
- **Special methods**: `__init__` and `__new__` signatures for constructor checking
- **Decorators**: `@property`, `@classmethod`, `@staticmethod` tracking
- **Protocols**: Marks classes inheriting from `typing.Protocol` for structural conformance checking
- **Inheritance**: Base class tracking with method resolution order for attribute lookup

Method types can be either single signatures or overload sets. When resolving a method call, the analyzer attempts to match argument types against overload signatures before falling back to the implementation signature.

## Pattern Matching Support

The `pattern` and `exhaustiveness` modules provide comprehensive pattern matching analysis:

- Type checking for all pattern forms (literal, capture, wildcard, sequence, mapping, class, OR, AS)
- Exhaustiveness checking to ensure match statements cover all cases
- Reachability checking to detect unreachable patterns subsumed by earlier cases
- Binding extraction to track variables introduced by patterns

This enables diagnostics like PM001 (non-exhaustive match) and PM002 (unreachable pattern).

## Linting & Additional Diagnostics

The `linter` and `rules` modules implement static checks beyond type correctness.
Many BEA-series diagnostic codes are implemented, with others awaiting parser or symbol table enhancements.
See the table of linting [rules](./lint_rules.md) for details.

## Utilities & Future Work

Beyond inference and CFG analysis, the module exposes helpers for locating unbound identifiers, invalidating cached results when documents change, and bridging between symbol-table scopes and LSP positions.

Outstanding work includes scaling CFG/data-flow analysis to whole modules, improving incremental re-analysis granularity, and extracting the constraint solver into a standalone crate.
