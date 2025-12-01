# Static Analyzer

The static analyzer performs control flow and data flow analysis to detect code quality issues beyond type errors. It builds a Control Flow Graph and runs analyses to find unreachable code, use-before-definition errors, and unused variables.

## How It Works

The analyzer operates in three phases:

1. Build Control Flow Graph from the AST
2. Perform data flow analyses on the CFG
3. Generate diagnostics from analysis results

The CFG captures all possible execution paths through a function, including normal flow, exception handling, loops, and early returns.

```text
                                    ┌─────────────────────┐
                                    │        AST          │
                                    └──────────┬──────────┘
                                               │
                                               ▼
                                    ┌─────────────────────┐
                                    │    CFG Builder      │
                                    └──────────┬──────────┘
                                               │
                                               ▼
                                    ┌─────────────────────┐
                                    │ Control Flow Graph  │
                                    └──────────┬──────────┘
                                               │
                                               ▼
                                    ┌─────────────────────┐
                                    │ Data Flow Analyzer  │
                                    └──────────┬──────────┘
                                               │
                                               ▼
                        ┌──────────────────────┴──────────────────────┐
                        │          Analysis Type                      │
                        └─┬─────────────────┬──────────────────────┬──┘
                          │                 │                      │
              ┌───────────▼──────┐  ┌───────▼────────┐  ┌──────────▼──────────┐
              │   Reachability   │  │    Use-Def     │  │     Liveness        │
              └───────────┬──────┘  └───────┬────────┘  └─────────┬───────────┘
                          │                 │                     │
                          │    ┌────────────▼─────────┐           │
                          └────►      Diagnostics     ◄───────────┘
                               └──────────────────────┘
```

### Control Flow Graph

Each function is converted into a CFG with basic blocks and edges:

- Basic blocks contain sequential statements
- Edges represent control flow with kinds: Normal, True, False, Exception, Break, Continue, Finally
- Entry and exit blocks mark function boundaries
- Loop headers have back edges for iteration

```text
                    ┌─────────────────┐
                    │  Entry Block    │
                    └────────┬────────┘
                             │
                             ▼
                    ┌────────────────┐
                ┌───┤  If Statement  ├───┐
                │   └────────────────┘   │
           True │                        │ False
                ▼                        ▼
         ┌────────────┐           ┌────────────┐
         │ Then Block │           │ Else Block │
         └──────┬─────┘           └──────┬─────┘
                │                        │
                └────────┬───────────────┘
                         ▼
                  ┌─────────────┐
                  │ Merge Block │
                  └──────┬──────┘
                         │
                         ▼
                  ┌─────────────┐
              ┌───┤ While Loop  ├──┐
              │   └─────────────┘  │
         True │          ▲         │ False
              │          │         │
              ▼          │         ▼
         ┌─────────┐     │    ┌──────────┐
         │Loop Body├─────┘    │Exit Block│
         │         ├──────────►          │
         └─────────┘  Break   └──────────┘
     Normal│     │Continue
           └─────┘
```

### CFG Construction

The builder walks the AST recursively:

- If/Elif/Else creates test blocks with True/False edges to branches, merging at a common successor
- For/While loops create headers with back edges from the body and break edges to exit
- Try/Except creates exception edges from try blocks to each handler
- Return/Raise statements jump to the function exit, passing through finally blocks if present
- Break/Continue statements jump to the appropriate loop boundary

Context tracking maintains loop depth and finally block stacks to generate correct edges.

### Data Flow Analyses

Use-Before-Definition detection performs forward analysis, tracking which variables are defined at each point. Any use of an undefined variable generates a diagnostic.

Unreachable code detection marks blocks reachable from the entry via depth-first search. Unreachable blocks produce warnings.

Unused variable detection tracks variable definitions and uses. Variables defined but never read generate warnings.

Hoisting analysis collects function and class definitions that are available before their textual position, matching Python's hoisting semantics.

## Limitations

CFG construction is function-scoped only. Module-level control flow graphs are not yet implemented, limiting whole-program analyses.

Exception flow is simplified and doesn't track exception types through the CFG. All exception handlers are treated uniformly.

Generator functions and async/await constructs have limited support. Yield points and async boundaries don't create proper CFG edges.

Class method CFGs don't track inheritance or method resolution order, which limits cross-method data flow analysis.

## Key Files

```sh
crates/
├── analyzer/src/
│   ├── cfg.rs           # Control Flow Graph construction
│   └── data_flow.rs     # Data flow analyses
└── server/src/
    └── analysis/mod.rs  # Analysis orchestration
```
