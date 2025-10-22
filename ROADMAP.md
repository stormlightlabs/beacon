# Roadmap

Building a Robust Hindley–Milner Type System for Python in Rust (with Tree-sitter + lsp-types)

## Project Layout

- `crates/cli`: batch checker & CI integration.
- `crates/constraints`: generation rules & constraints IR, effect/region placeholders.
- `crates/core`: type representations, substitutions, unifier, generalization, kinding.
- `crates/parser`: Tree-sitter bindings, Python grammar hooks, CST->AST adapters.
- `crates/server`: LSP server (uses `lsp-types`), incremental orchestration, features.

---

- `crates/solver`: unification, constraint solving, protocol/structural entailment.
- `crates/index`: symbol tables, module/type cache, stub ingestion (`.pyi`, `typing`).
- `crates/test_support`: golden tests, fixtures, corpus harness.

## Tree-sitter

### Goals

- Fast, incremental parsing with lossless positions.
- Map Tree-sitter nodes -> typed AST suited for constraint generation.
- Preserve trivia for precise diagnostics, ranges, and code actions.

### Tasks

1. Bind grammar: Use `tree-sitter-python` via Rust bindings. Expose node kinds (functions, classes, assignments, calls, comprehensions, pattern matching, async, f-strings).
2. CST -> AST layer: Build a small, typed AST tailored for type inference (normalize desugaring):
   - Desugar `with`, comprehensions, walrus operator, pattern match, and decorators into core forms when feasible.
   - Recognize `typing` syntax in annotations: `list[int]`, `type[T]`, `Callable`, `Literal`, `TypeVar`, `Self`, `Protocol`.
3. Name resolution (prepass):
   - Scope builder: module -> class -> function -> block.
   - Track imports/aliases (`import x as y`, `from m import a as b`).
   - Associate decorators with targets; capture `__init__` -> constructor shaping.
4. Incremental updates: Maintain Tree-sitter edit sessions; invalidate affected AST subtrees and constraint slices by byte ranges.

## Core Type System

### Base HM Types

- **Type variables**: `α, β, ...`
- **Monotypes**: variables, constructors (`int`, `str`, `bool`, `float`, `NoneType`), function arrows `τ -> τ`.
- **Polytypes**: `∀α. τ` with let-generalization only at non-expansive bindings (value restriction variant).
- **Kinds** for higher-kinds (generics over types): `*`, `* -> *`, etc. Needed for `list[T]`, `dict[K,V]`.

### Extensions

- **Products/Records:** row-polymorphic records for objects/instances: `{ x: τx | ρ }`.
- **Structural constraints / Protocols:** entailment `τ ⊢ has_attr "len": int -> int`. Use row-polymorphism or dictionary passing style for protocols.
- **Union & Optional:** `τ | σ`, `Optional[τ] ≡ τ | None`.
- **Literals:** refine basics with literal types (e.g., `Literal["GET"]`).
- **Parametric classes:** generic `class Box[T]: ...`, variance markers (default invariant).
- **Overloads:** choose most specific arrow compatible with call site; fallback to inferred principal type when safe.
- **Effects (future):** mark unknown side effects & reflection as widening to `Any` in loose mode.

### Annotation Interaction

- Treat annotations as constraints/hints, not ground truth (configurable):
    - **Strict:** annotation mismatches are errors.
    - **Balanced:** mismatches are diagnostics w/ quick fixes; inference still proceeds.
    - **Loose:** annotations supply upper/lower bounds but can be overridden.

### Any, Top, Bottom

- `Any` (dynamic escape hatch), `⊤` (unknown supertype), `⊥` (never).
- Prefer gradual semantics: `Any` erases constraints where it flows; annotate diagnostics where it infects boundaries.

## Constraint Generation (Algorithm)

Given environment Γ and expression `e`:

- **Variables:** `x : instantiate(Γ(x))`.
- **Literals:** ground types: `int`, `str`, `bool`, `NoneType`, `float`, `complex`, `bytes`.
- **Lambda / def:** fresh vars for params & result; generate constraints from body; arrow type.
- **Let / assignment:**
    - `x = e`: infer `τ`, generalize to `σ = gen(Γ, τ)` if non-expansive (no effectful/dynamic RHS), else keep monotype.
    - Tuple/list destructuring: unify pattern shape with RHS type.
- **Call:** `f e1 … en` produces constraints that `type(f)` is compatible with callable of arity `n`; resolve overloads via most specific.
- **Attribute access:** `e.a` -> constraint `e` has field `"a" : α`. With classes, integrate nominal info; with protocols, use structural entailment.
- **Indexing / subscripts:** `e[i]` -> `e : SupportsGetItem[τi, τo]`; for builtins, compile to known signatures (`list[T] -> (int)->T`, `dict[K,V] -> (K)->V`).
- **Comprehensions:** desugar to loops; propagate iterator protocols: `__iter__ : () -> Iterator[T]`.
- **Pattern matching (PEP 634):** generate constraints from pattern structure (mapping, sequence, class patterns).
- **Decorators:** treat as transforms on function/class type: if annotation for decorator is known `(A->B) -> (A->B)` or similar, apply; else conservative widening.

### Example

```python
def id(x):
    return x
y = id(41)
```

- `id : α -> α` (generalized at `def`)
- `y : int` via call constraint `α ≡ int`.

## Unification & Solving

- **Occurs-check**, substitution maps, union-find acceleration.
- **Row-unification** for records: enable open records `{a: τa | ρ}` to model objects + duck typing.
- **Entailment engine** for protocol satisfaction: solve `HasAttr(τ, "a", τa)` by:
    - If nominal class known -> consult class MRO & attributes.
    - Else if row contains `"a"` -> unify.
    - Else generate a deferred constraint resolved after more info arrives (important for incremental + multi-file).
- **Value restriction:** generalize only if RHS is non-expansive (literal, lambda, constructor, pure builtin). Configurable for Python ergonomics.
- **Performance:**
    - Arena-allocated types, persistent substitutions, interning of names, **hash-consing** for sharing.
    - Constraint slices keyed by AST node IDs for incremental re-solve.

## Feature Coverage Plan

- **Functions & closures**: capture environment types; infer `*args`/`**kwargs` via tuple/dict rows.
- **Classes & instances**: class body produces a nominal type scheme; instances become records with nominal tag for MRO lookup.
- **`__init__` shaping**: constructor arrow `(Self, *args, **kwargs) -> None`; attributes assigned in `__init__` materialize on `Self`.
- **Properties / descriptors**: treat `@property` as attribute with getter/setter arrows.
- **Magic methods**: predefined typeclasses/traits (e.g., `__iter__`, `__len__`, `__enter__/__exit__`, numeric ops).
- **Modules & imports**: module type is a record of exported names; cache & stub ingestion from `.pyi`.
- **Decorators**: as above; maintain a small library of known decorators (`functools.wraps`, `dataclasses.dataclass`, `classmethod`, `staticmethod`).
- **Generators & async**:
    - `def f() -> Generator[Y, S, R]` or inferred: returns `Generator[Yield=Y, Send=S, Return=R]`.
    - `async def` returns `Coroutine[Any, Any, R]` or `Awaitable[R]`.
- **Pattern matching**: cover sequence, mapping, class patterns; guard expressions; capture variable bindings types.
- **Typing interop**: `TypeVar`, `ParamSpec`, `TypeVarTuple` (progressively), `Protocol`, `TypedDict`, `NewType`, `Literal`, `Final`, `ClassVar`, `Self`.
- **Dynamic escape hatches**: `getattr`, `setattr`, reflection -> configurable diagnostics; widen types as necessary.

## LSP (with `lsp-types`)

Implement an LSP server (e.g., with `tower-lsp` or manual) but **use `lsp-types`** for request/response models.

### Tasks

- **Diagnostics:** type errors, unsafe `Any` flows, unknown attributes, mismatch with annotation, unbound names, unification failures.
- **Hovers:** principal types (generalized/polymorphic), instantiated types at the site, protocol evidence.
- **Go to Definition / Type Definition / References:** based on index and symbol tables.
- **Inlay hints:** parameter names, inferred types for let-bindings, generics on call sites.
- **Code actions:**
    - Insert type annotations (from inferred types).
    - Convert `None` to `Optional[T]`.
    - Add missing `__iter__` / protocol stubs.
    - Replace `Any` with inferred concrete types where possible.
    - Create `.pyi` stub from module.
- **Semantic tokens:** highlight based on type roles (type vars, generics, protocols).
- **Workspace features:** project-wide check, caching, watch mode, stub discovery.

**Incrementality:**

- On `textDocument/didChange`, reparse with Tree-sitter edits, re-generate constraints only for changed subtrees, re-solve affected SCCs in the project graph, publish minimal diagnostics.

## Data Structures

### Types & Kinds

```rust
enum Kind { Star, Arrow(Box<Kind>, Box<Kind>) }

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
struct Tv(u32); // type variable id

enum TypeCtor { Int, Str, Bool, Float, NoneType, Bytes, List, Dict, Set, Tuple, Function, Class, Module, /* ... */ }

enum Type {
    Var(Tv),
    App(Box<Type>, Box<Type>),        // for higher-kinded app, e.g., List T
    Con(TypeCtor),                    // ground constructors
    Fun(Vec<Type>, Option<Type>, Box<Type>), // params, varargs/kwargs flattened later, result
    Record(Row),                      // row-polymorphic records
    Union(Box<Type>, Box<Type>),
    Any,
    Top,
    Bottom,
    ForAll(Vec<Tv>, Box<Type>),
}

struct Row { fields: FxHashMap<String, Type>, tail: Option<Tv> }
```

### Constraints

```rust
enum Constraint {
    Equal(Type, Type),
    HasAttr(Type, String, Type),
    Call(Type, Vec<Type>, Option<Type>, Type), // f(args, kwargs?) -> ret
    Subtype(Type, Type),        // optional, if enabling subtyping-lite
    InstanceOfProtocol(Type, ProtocolId),
    Conj(Vec<Constraint>),
}
```

### Substitutions & Solver

```rust
struct Subst(FxHashMap<Tv, Type>);

impl Subst {
    fn apply(&self, t: &Type) -> Type { /* ... */ }
    fn compose(self, other: Subst) -> Subst { /* ... */ }
}

struct Solver {
    subst: Subst,
    // protocol tables, class MRO maps, row unifier, deferred queues...
}
```

## Incremental & Project-Wide Analysis

- **Graph:** Modules form a dependency graph; analyze in SCC order.
- **Slices:** Each AST node yields a “constraint slice”; edits invalidate slices.
- **Caches:**
    - **Type cache by node id** (for hover/inlay without re-solve).
    - **Substitution snapshots** per slice version.
    - **Stub cache** for `.pyi` + compiled signatures of builtins.
- **Persistence:** optional on-disk cache keyed by file hash & interpreter version.

## Common Patterns

### Patterns for Success

- **Row-polymorphism** for Python objects to model attribute growth while keeping inference tractable.
- **Protocol entailment** as constraints rather than global subtyping.
- **Value restriction** to avoid unsound polymorphism with mutation/dynamics.
- **Local first, global cautiously:** infer inside functions/classes robustly; widen at module boundaries if required.
- **Degradation paths:** When reflection detected, switch to conservative types with precise diagnostics (actionable hints).

### Influential Systems & Ideas

- **Algorithm W** (classic HM), **Damas–Milner**.
- **HM(X)**: HM with constrained types; a good fit for adding protocols.
- **OutsideIn(X)** (GHC): type classes + constraints solving strategy (blueprint for protocol entailment).
- **Typed Racket** & **Gradual Typing** literature (how to co-exist with dynamic code).
- **TypeScript** structural typing & inference strategies (for protocols/records).
- **MyPy** / **Pyright** internals (stubs, protocol support, incremental analysis).
- **Row-polymorphism** (e.g., Rémy’s rows, OCaml object types).
- **Effect systems** (for future: marking dynamic/reflection as effects).

## Error Model & UX

- **Clear primary errors** (where mismatch surfaces) and secondary notes (blame sites: def vs call).
- **Explain polymorphism**: show generalized types vs instantiations in hovers.
- **Actionable suggestions**: code actions to annotate, add missing attributes, or narrow unions with `isinstance` checks.
- **Flow-sensitivity**: basic refinement after guards (`if x is None: ... else: x: T`) for nicer UX, even if core is HM.

## Terminal Syntax Highlighting

Adding syntax highlighting to CLI output significantly improves readability of diagnostics, AST dumps, and type information.
We will layer this functionality gradually, starting with minimal styling and moving toward Tree-sitter–powered highlighting of Python code in error snippets.

### Basic Styling

- Use a lightweight crate such as [`owo-colors`](https://docs.rs/owo-colors) to colorize diagnostics:
    - **Error labels**: bright red, bold.
    - **Warnings**: yellow.
    - **Notes/help text**: dim blue.
    - **File paths & line/col**: dim gray.
- Add helper functions in a small `ui` module:

  ```rust
  use owo_colors::OwoColorize;

  pub fn error_label(msg: &str) -> String {
      format!("{}", msg.red().bold())
  }

  pub fn type_var(name: &str) -> String {
      format!("{}", name.magenta())
  }
  ```

- Centralize all style decisions for easy toggling (e.g. disable in CI with `NO_COLOR`).

### Pretty-Printing Types and Constraints

- Implement custom `Display` for `Type`, `Constraint`, and `AST` structures.
- Use consistent color mapping:
    - Type variables -> magenta.
    - Ground types (`int`, `str`, etc.) -> cyan.
    - Function arrows (`->`) -> dim gray.
    - Constraints keywords (`HasAttr`, `Equal`) -> bold green.
- Example:

  ```text
  head : (Sizable) -> int
  id   : ∀α. α -> α
  ```

### Highlighting Python in Diagnostics

- Integrate [`tree-sitter-highlight`](https://crates.io/crates/tree-sitter-highlight).
- Reuse the same tree-sitter Python grammar already used for parsing.
- Define a highlight query file (e.g. `queries/highlights.scm`) mapping Python syntax nodes to scopes:

    ```scheme
    (identifier) @variable
    (string) @string
    (number) @number
    (function_definition name: (identifier) @function)
    (class_definition name: (identifier) @type)
    ```

- Pipe highlight events from `tree-sitter-highlight` through a **scope-to-color map** (implemented with `owo-colors`).
- Apply highlighting only in code snippets shown in diagnostics:

  ```text
  error[HM001]: type mismatch
    --> src/example.py:3:12
     |
   3 | y = head(42)
     |            ^^ expected Sizable, found int
     |
     = note: `int` does not implement `__len__`
  ```

  Here:

    - `head` is cyan (function).
    - `42` is yellow (number).
    - `y` is bold white (identifier).

- **Inline highlighting in `ast` / `types` commands**: show snippets of Python code alongside inferred types, both styled.
- **Configurable themes**: allow users to switch themes (light/dark) by loading alternate highlight queries or color maps.
- **Fallback modes**: if `tree-sitter-highlight` fails (e.g. malformed syntax), fall back to plain color categories or even disable colors.

## Milestones

1. **Parsing**
   - ✓ Tree-sitter hooked up; CST->AST with comprehensive Python node support
   - ✓ Syntax highlighting with configurable color schemes
   - ✓ CLI interface (parse, highlight, check commands)
   - Name resolution (scope builder: module -> class -> function -> block)
   - Module graph (imports, dependencies, module cache)
   - LSP "hello" (open/close, hover stub)

2. **Core Type System**
   - ✓ Types, Kinds, TypeCtor enums with comprehensive trait implementations
   - ✓ Substitution system with composition and recursive application
   - ✓ Unification algorithm with occurs check, union types, record types
   - ✓ Row-polymorphic records for object modeling
   - Algorithm W constraint generation (needs name resolution)
   - Let-generalization with value restriction
   - Diagnostics + basic hover/inlay

3. **Constraint Generation**
   - Algorithm W implementation for expressions
   - Variables, literals, functions, calls, tuples, lists, dicts
   - Constraint IR and generation rules
   - Integration with parser AST and type system

4. **Records & Attributes**
   - Row-polymorphic records, `HasAttr`, attribute reads/writes, object construction via `__init__`.
   - Basic protocol entailment (manually authored builtins).

5. **Typing Interop**
   - Parse and honor annotations (PEP 484/695 subset), `Optional`, `Union`, `TypeVar`, `Generic`.
   - Annotated mismatch reporting & code actions (insert annotation).

6. **Collections & Iteration**
   - Iterator/Iterable protocols, comprehensions, `with`/context managers.
   - Inlay hints polish.

7. **Classes & Overloads**
   - Overloads resolution, properties/staticmethod/classmethod, dataclasses.
   - `.pyi` stub ingestion; module types.

8. **Async & Generators**
   - `async`, `await`, `yield`, return type derivation for coroutines/generators.

9. **Pattern Matching**
   - PEP 634 constraints; exhaustiveness diagnostics (basic).

10. **Incrementality**
    - Constraint slicing, SCC rebuilds, parallelism, on-disk caches.

11. **Ergonomics**
    - Strict/balanced/loose modes; analysis of dynamic features; rich code actions.

12. **Hardening**
    - Large-repo benchmarks; memory profiling; stability & crash-free guarantee.

## Testing

- **Golden tests** for inference: input + expected principal types / diagnostics.
- **Property tests**: unification idempotence, substitution correctness, principal type invariants.
- **Corpus**: real-world snippets (stdlib, small OSS); compare against MyPy/Pyright diagnostics for sanity.
- **Performance benches**: cold vs warm, single-file edits, large module graphs.
- **Fuzzing**: random ASTs within Python grammar subset to stress unifier/solver.

## Example

```python
from typing import Protocol, TypeVar

T = TypeVar("T")

class Sizable(Protocol):
    def __len__(self) -> int: ...

def head(x: Sizable) -> int:
    return len(x)

def id(x: T) -> T:
    return x

xs = [1,2,3]
y = head(xs)       # infers int via __len__: list[int] satisfies Sizable
z = id(xs)         # generalizes id: ∀α. α->α, instantiates at list[int]
```

**Constraints (abridged):**

- `list[int] ⊢ HasAttr "__len__" : () -> int`
- `head : Sizable -> int`
- `id : ∀α. α -> α` generalized at `def`

**Diagnostics (if mismatch):**

- If `head(42)`: report `int` doesn’t satisfy `HasAttr "__len__" : () -> int` with a note suggesting `from typing import Sized` or `Sizable`.

## Risks

- **Dynamic features explosion:** cap unsoundness with modes; emit diagnostics when widening to `Any`.
- **Decorator complexity:** start with a catalog of known decorators; allow user-provided stubs.
- **Protocol completeness:** bootstrap with stdlib/builtins; allow `.pyi` ingestion & caching.
- **Performance cliffs:** adopt incremental slices early; arena + interning; avoid quadratic unifications via union-find & memoization.

## Deliverables

- [ ] `tree-sitter` parser + CST->AST with stable node IDs & ranges.
- [ ] Type core + solver with tests (unifier, substitution, generalization).
- [ ] Constraint generator covering core Python constructs.
- [ ] Name resolution & symbol tables for variable binding tracking.
- [ ] Row-polymorphic records & protocol entailment.
- [ ] LSP server (using `lsp-types`): diagnostics, hovers, inlay hints, code actions.
- [ ] Typing interop (PEP 484/695 subset) + stub ingestion.
- [ ] Benchmarks + corpus tests + fuzzing harness.
- [ ] Documentation: user guide (modes), developer guide (constraints), contribution guide.

### Appendix A: LSP Messages (with `lsp-types`)

- **Publish Diagnostics** (`textDocument/publishDiagnostics`): ranges via Tree-sitter byte offsets -> UTF-16 positions.
- **Hover** (`textDocument/hover`): Markdown signature with principal & instantiated type.
- **Inlay Hints** (`textDocument/inlayHint`): inferred variable/parameter types.
- **Code Action** (`textDocument/codeAction`): fixes to insert annotations or wrap in `Optional[...]`.
- **Workspace Symbol / Document Symbol**: from index; include type info in detail.

### Appendix B: Config Options

- `mode`: `"strict" | "balanced" | "loose"`.
- `pythonVersion`: `3.10+` target (enable/disable pattern matching features).
- `stubPaths`: extra `.pyi` roots.
- `maxAnyDepth`: cap infection depth for `Any` before elevating diagnostics.
- `decoratorStubs`: user-defined decorator typings.
