# PEP8

## Philosophy

### Purpose

Provide coding conventions for the Python standard library, to enhance readability and consistency[^pep].

### Underlying principle

"Code is read much more often than it is written."

### Consistency matters

Within a project > within a module > within a function.

### Exceptions permitted

When strictly following the guideline reduces clarity or conflicts with surrounding code.

## Encoding

Use UTF-8 encoding for source files (in the core distribution).

Avoid non-ASCII identifiers in standard library modules; if used, limit noisy Unicode characters.

## Layout

### Indentation

Use 4 spaces per indentation level. Tabs are strongly discouraged. Never mix tabs and spaces.

### Line Length

Preferred maximum: **79 characters** for code.

For long blocks of text (comments/docstrings): ~72 characters.

### Blank Lines and Vertical Whitespace

Insert blank lines to separate top-level functions and classes, and within classes to separate method groups.

Avoid extraneous blank lines within code structure.

### Imports

Imports at top of file, after module docstring and before module globals/constants.

Group imports in the following order:

1. Standard library imports
2. Related third-party imports
3. Local application/library-specific imports
   Insert a blank line between each group.

Absolute imports preferred; explicit relative imports acceptable for intra-package use.

Wildcard imports (`from module import *`) should be avoided except in rare cases (e.g., to publish a public API).

## Whitespace

Avoid extra spaces in the following contexts:

- Immediately inside parentheses, brackets or braces.
- Between a trailing comma and a closing bracket.
- Before a comma, semicolon, or colon.
- More than one space around an assignment operator to align multiple statements (alignment discouraged)

### Usage

```python
# Correct:
spam(ham[1], {eggs: 2})

# Avoid:
spam( ham[ 1 ], { eggs: 2 } )
```

## Comments

Good comments improve readability, explain _why_, not _how_.

Use full sentences, capitalize first word, leave a space after the `#`.

Inline comments should be used sparingly and separated by at least two spaces from the statement.

Block comments should align with code indentation and be separated by blank lines where appropriate.

### Docstrings

Use triple-quoted strings for modules, functions, classes.

The first line should be a short summary; following lines provide more detail if necessary.

For conventions specific to docstrings see PEP 257 – Docstring Conventions.

## Naming Conventions

| Kind                   | Convention                                                                          |
| ---------------------- | ----------------------------------------------------------------------------------- |
| Modules                | Short, **lowercase**, may use underscores                                           |
| Packages               | All-lowercase, preferably no underscores                                            |
| Classes                | Use **CapWords** (CamelCase) convention                                             |
| Exceptions             | Typically CapWords                                                                  |
| Functions and methods  | Lowercase with underscores (snake_case)                                             |
| Variables              | Use lowercase_with_underscores                                                      |
| Constants              | All UPPERCASE_WITH_UNDERSCORES                                                      |
| Private identifiers    | One leading underscore `_private`; name mangling via \_\_double_leading_underscore. |
| Type Vars (in generics)| CapWords                                                                            |

Avoid single character names like `l`, `O`, `I` (they are easily confused with `1` and `0`).

## Recommendations

Avoid pointless object wrappers, redundant code; prefer simple, explicit approaches. This matches the ethos "explicit is better than implicit" from [The Zen of Python](https://peps.python.org/pep-0020/).

When offering interfaces, design them so it is difficult to misuse them (i.e., "avoid programming errors").

Avoid using mutable default arguments in functions.

In comparisons to singletons (e.g., `None`), use `is` or `is not` rather than equality operators.

## Exceptions to the Rules

The style guide states that while adherence is recommended, there are legitimate cases for deviation.

Reasons to deviate:

- Strict adherence would reduce readability in context.
- Code must remain consistent with surrounding non-PEP8 code (especially legacy).
- The code predates the rule and rewriting it isn’t justified.

## Tooling

Tools exist to help enforce or auto-format code to PEP 8 style (e.g., linters, auto-formatters).

Using such tools helps maintain style consistency especially on teams or open-source projects.

## Summary

1. Readability and consistency are the primary goals.
2. Follow conventions: 4 spaces, line length ~79 chars, snake_case for functions/variables, CapWords for classes, uppercase for constants.
3. Imports at top, grouped logically.
4. Whitespace matters—used meaningfully, not decoratively.
5. Use comments and docstrings effectively: explain _why_, not _how_.
6. Be pragmatic: if strictly following every rule makes things worse, depart in favour of clarity.
7. Use automation tools to assist but don’t treat the guide as dogma—interpret intelligently.

[^pep]: <https://peps.python.org/pep-0008/> "PEP 8 – Style Guide for Python Code"
