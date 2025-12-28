# QA Checklist

**Focus**: Correctness of output (type checking, analysis, linting, formatting, span positioning).

## Findings (Dec 27 2025)

### Correctness Verified

- **Enum Support**: `test_enum.py` correctly identifies invalid member access, type mismatches, and `IntEnum`/`StrEnum` behaviors.
- **Reachability**: `test_exhaustiveness.py` correctly identifies dead code and unreachable branches in standard if/else chains.

### Bugs Identified

- **Type System**: `str` type is not treated as `Iterable` (found in `test_mode_balanced.py`).
- **Analysis**: Exhaustiveness verification fails to account for some match guards, leading to false positive warnings (found in `test_guard_narrowing.py`).

- **Diagnostics**:
    - Spans are sometimes positioned incorrectly (e.g., squiggles under wrong part of expression).
    - Single-character spans (excessively small) do not accurately communicate the scope of the problem.

## Core Feature Checklist

### 1. LSP & Lifecycle

- [ ] **Document Sync**: Open/Edit/Save triggers incremental updates without crash.
- [ ] **Workspace**: Multi-file workspace indexes correctly; cross-file references work.
- [ ] **Config**: Interpreter detection works; per-folder config overrides apply.

### 2. Type Checking & Inference

- [ ] **Basics**: Function/variable annotations enforced; inferred types shown in hover.
- [ ] **Generics**: `List[int]`, `Dict[str, int]` enforced; variance checks passed.
- [ ] **Protocols**: Structural compatibility checks work; `Iterable`, `Sequence` etc.
- [ ] **Refinement**: `isinstance`, `None` checks, and match statements narrow types correctly.

### 3. Static Analysis

- [ ] **Reachability**: Unreachable code after `return`/`raise` is flagged.
- [ ] **Data Flow**: Use-before-def and unused variables detected across scopes.
- [ ] **Imports**: Circular imports detected; relative vs absolute imports resolve correctly.

### 4. Diagnostics & Linting

- [ ] **Parser Errors**: Syntax errors mapped to correct lines/cols.
- [ ] **Linter Rules**: Verify sample of BEA001-BEA030 rules fire appropriately.
- [ ] **Spans**: **CRITICAL**: Verify diagnostic squiggles cover the full relevant expression, not just one char.

### 5. IDE Features

- [ ] **Navigation**: Go-to-Definition and Find References work across files.
- [ ] **Rename**: Renaming symbol updates all references (and only valid references).
- [ ] **Completion**: Context-aware recommendations for attributes, locals, and imports.
- [ ] **Hover**: documentation and type info displayed for symbols.

### 6. Performance

- [ ] **Large Files**: Editing >1k LOC file remains responsive.
- [ ] **Cold Start**: Initial indexing of sample workspace < 5s.
