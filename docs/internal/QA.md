
# QA Checklist for Existing Features

Last Updated: 2025-11-05

## LSP Document Lifecycle

- **Test:** Open a Python file, make edits, save, and close
- **Expected:** No crashes, incremental updates reflected, diagnostics refresh on save

- **Test:** Open workspace with multiple Python files
- **Expected:** All files indexed, cross-file references work

## Diagnostics

- **Test:** Write code with parse errors (e.g., `def foo(`), type errors (e.g., `x: int = "str"`), and unbound variables
- **Expected:** Diagnostics appear with appropriate severity (error/warning), positions match the issues

- **Test:** Create circular imports between two modules
- **Expected:** Circular import diagnostic appears, specifies the cycle

- **Test:** Import non-existent module or use unsafe Any types
- **Expected:** Unresolved import error and unsafe Any warnings appear

- **Test:** Trigger linting rules (unused imports, undefined names, duplicate arguments, etc.)
- **Expected:** All 30+ BEA linting rules fire appropriately with clear messages

## Hover

- **Test:** Hover over variables, function parameters, class attributes, builtin types
- **Expected:** Type information displays correctly, includes documentation for builtins

- **Test:** Hover over dunder methods like `__init__`, `__str__`
- **Expected:** Method signature and documentation appear

## Completion

- **Test:** Type `.` after an object and trigger completion
- **Expected:** Context-aware suggestions appear (methods, attributes based on type)

- **Test:** Trigger completion in various contexts (imports, function calls, assignments)
- **Expected:** Relevant suggestions with type information

## Navigation

- **Test:** Use "Go to Definition" on function calls, variable references, class names
- **Expected:** Navigates to the definition location, works across files

- **Test:** Use "Find References" on a symbol used in multiple places
- **Expected:** Lists all usages across workspace with file/line information

- **Test:** Place cursor on a symbol and use "Document Highlight"
- **Expected:** All occurrences in current file are highlighted

## Rename

- **Test:** Rename a function used in multiple files
- **Expected:** All references updated across workspace, no broken references

- **Test:** Rename a parameter or local variable
- **Expected:** Only updates within scope, doesn't affect other scopes

## Inlay Hints

- **Test:** Open file with uninferred types
- **Expected:** Inlay hints display inferred types inline for parameters and return values

## Symbols

- **Test:** Open document symbols view
- **Expected:** Hierarchical outline showing classes, functions, variables

- **Test:** Search workspace symbols for a class or function name
- **Expected:** Results show matching symbols across all files with locations

## Semantic Tokens

- **Test:** Open Python file with various constructs (classes, functions, keywords)
- **Expected:** Advanced syntax highlighting applies correctly, distinguishes types from variables

## Code Actions

- **Test:** Create unused variable or import
- **Expected:** Code action quick fix appears to remove unused code

- **Test:** Use value that could be None in unsafe context
- **Expected:** Code action suggests wrapping with Optional

## Type Inference

- **Test:** Write function without type annotations, use it with various argument types
- **Expected:** Type inference determines correct polymorphic type, shows in hover

- **Test:** Create union types (`x: int | str`), use in function parameters
- **Expected:** Type checking enforces union constraints, allows valid branch operations

- **Test:** Use generic types like `List[int]`, `Dict[str, int]`
- **Expected:** Type applications work correctly, enforce element types

- **Test:** Use Optional types, assign None or valid value
- **Expected:** None compatibility works, non-None assignments type check

## Subtyping

- **Test:** Assign union type to broader union (e.g., `x: int | str` to `y: int | str | float`)
- **Expected:** Subtyping check passes

- **Test:** Assign incompatible types
- **Expected:** Type error with clear message

## Class Analysis

- **Test:** Define class with `__init__`, methods, properties
- **Expected:** Method resolution works, `self` types correctly inferred

- **Test:** Use inheritance with method overrides
- **Expected:** Inherited methods accessible, overrides type check correctly

- **Test:** Use `@property`, `@classmethod`, `@staticmethod` decorators
- **Expected:** Decorators recognized, method types adjusted appropriately

- **Test:** Define and use `@overload` functions
- **Expected:** Overload resolution picks correct signature based on arguments

## Pattern Matching

- **Test:** Use pattern matching with various patterns (literal, capture, sequence, mapping, class)
- **Expected:** Pattern types check correctly, bindings inferred

- **Test:** Write non-exhaustive pattern match
- **Expected:** Warning about missing cases

- **Test:** Write pattern with unreachable branches
- **Expected:** Unreachable code warning

## Control Flow Analysis

- **Test:** Write unreachable code after return/break/continue
- **Expected:** Unreachable code warning

- **Test:** Use variable before definition
- **Expected:** Use-before-def diagnostic

- **Test:** Define unused variables
- **Expected:** Unused variable warning

## Import Resolution

- **Test:** Import standard library modules (`os`, `sys`, `pathlib`)
- **Expected:** Imports resolve, completion/hover work for imported symbols

- **Test:** Import local modules with relative imports
- **Expected:** Module resolution works, cross-file type checking active

- **Test:** Create import cycle and observe behavior
- **Expected:** Graceful handling, diagnostic reports the cycle

## Linting Rules Coverage

- **Test:** Trigger each BEA rule category systematically
    - BEA001-BEA005: Undefined names, duplicate args, flow control
    - BEA006-BEA010: Return/yield/break/continue context errors
    - BEA011-BEA015: Exception handling issues
    - BEA016-BEA020: String format issues
    - BEA021-BEA025: Logic issues (if tuple, assert tuple, is literal)
    - BEA026-BEA030: Import/unused symbol issues
- **Expected:** Each rule fires with appropriate diagnostic message and severity

## Incremental Updates

- **Test:** Make edit in one file, observe diagnostics in dependent files
- **Expected:** Dependent files re-analyzed, diagnostics update without full restart

- **Test:** Fix type error and save
- **Expected:** Diagnostic clears immediately

## Performance

- **Test:** Open large workspace (>100 files)
- **Expected:** Initial indexing completes within reasonable time, no UI freeze

- **Test:** Make rapid edits in large file
- **Expected:** Incremental updates are responsive, no lag in diagnostics

## Folding Range

- **Test:** Open Python file with functions and classes
- **Expected:** Folding indicators appear for function bodies, class bodies, and method bodies

- **Test:** Use code folding on function with multiline body
- **Expected:** Function body collapses, leaving only the function signature visible

- **Test:** Create file with if/elif/else blocks, for/while loops, try/except/finally
- **Expected:** Folding ranges available for each control flow block

- **Test:** Write nested structures (class with methods, function with nested if statements)
- **Expected:** Folding works at each nesting level independently

- **Test:** Create import block with multiple consecutive import statements
- **Expected:** Import group folds as a single unit with "imports" folding kind

- **Test:** Use match/case statements with multiple cases
- **Expected:** Entire match block and individual case blocks have folding ranges

- **Test:** Create with statement containing multiple lines
- **Expected:** With block body is foldable

- **Test:** Write single-line function or empty function body
- **Expected:** Reasonable folding behavior (single statement may or may not fold)

## Configuration

- **Test:** Verify Python interpreter detection works
- **Expected:** Correct interpreter found and used for introspection
