# Debug Tools

Debug commands provide low-level inspection of Beacon's parsing and type inference internals. These tools are only available in debug builds.

## Availability

Debug commands are compiled only in debug builds:

```sh
# Build in debug mode (includes debug commands)
cargo build

# Build in release mode (excludes debug commands)
cargo build --release
```

## Commands

### Tree-sitter CST

Display the concrete syntax tree from tree-sitter:

```sh
beacon-cli debug tree [OPTIONS] [FILE]
```

Options:

- `--json` - Output in JSON format

Example output (default S-expression style):

```clojure
Tree-sitter CST:
(module [0, 0] - [3, 0]
  (expression_statement [0, 0] - [0, 6]
    (assignment
      left: (identifier [0, 0] - [0, 1])
      right: (integer [0, 4] - [0, 6]))))
```

JSON output:

```sh
beacon-cli debug tree --json example.py
```

### AST with Types

Show Beacon's AST with inferred types:

```sh
beacon-cli debug ast [OPTIONS] [FILE]
```

Options:

- `--format <FORMAT>` - Output format (tree, json) [default: tree]

Example:

```sh
$ beacon-cli debug ast example.py
AST with inferred types:

Type mappings: 15 nodes
Position mappings: 12 positions

Type errors: 0

Node types:
  Node 1: Int
  Node 2: (Int, Int) -> Int
  Node 3: Int
  ...
```

### Constraints

Display generated type constraints:

```sh
$ beacon-cli debug constraints [FILE]

Generated 23 constraints:

▸  Equal (12 instances)
  1. Equal(τ1, Int)
  2. Equal(τ2, (Int, Int) -> Int)
  3. Equal(τ3, Int)
  ... and 9 more

▸  Call (5 instances)
  1. Call(τ2, [τ1, τ1], {}, τ4)
  2. Call(print, [τ4], {}, τ5)
  ... and 3 more

▸  HasAttr (6 instances)
  1. HasAttr(τ6, "append", τ7)
  2. HasAttr(τ6, "extend", τ8)
  ... and 4 more
```

### Unification

Show unification trace (TODO):

```sh
beacon-cli debug unify [FILE]
```

### Diagnostics

Run comprehensive diagnostics (parse errors, lint issues, type errors, static analysis) on Python files:

```sh
beacon debug diagnostics [OPTIONS] <PATHS>...
```

Accepts:

- Single file: `beacon debug diagnostics file.py`
- Multiple files: `beacon debug diagnostics file1.py file2.py file3.py`
- Directory: `beacon debug diagnostics src/` (recursively finds all .py files)

Options:

- `-f, --format <FORMAT>` - Output format (human, json, compact) [default: human]

Example output (human format):

```sh
$ beacon debug diagnostics src/

⚡ Running comprehensive diagnostics on 5 file(s)...

✓ 0 Parse Errors

✗ 3 Lint Issues
  ▸ src/main.py:5:1 [BEA015] 'os' imported but never used
    5 import os
      ~
  ▸ src/utils.py:10:5 [BEA018] 'x' is redefined before being used
    10     x = 2
           ~
  ▸ src/helper.py:3:1 [BEA015] 'sys' imported but never used
    3 import sys
      ~

✗ 2 Type Errors
  ▸ src/main.py:12:9 Cannot unify types: Int ~ Str
    12     z = x + y
               ~
  ▸ src/utils.py:20:5 Undefined type variable: τ5
    20     result = unknown_func()
           ~

Summary: 5 total issue(s) found
```

JSON output:

```sh
beacon debug diagnostics --format json src/ > diagnostics.json
```

Compact output (for editor integration):

```sh
beacon debug diagnostics --format compact src/
src/main.py:5:1: [BEA015] 'os' imported but never used
src/utils.py:10:5: [BEA018] 'x' is redefined before being used
src/main.py:12:9: [TYPE] Cannot unify types: Int ~ Str
```
