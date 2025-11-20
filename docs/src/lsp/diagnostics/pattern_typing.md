# Pattern Typing Diagnostics

Pattern typing diagnostics focus on structural mismatches that arise during match statement analysis.

## HM010 – `PatternTypeMismatch` {#hm010}

### Example

```py
def parse(match_obj):
    match match_obj:
        case (x, y):  # HM010 if match_obj is inferred as str
            ...
```

### Guidance

Ensure match subjects and patterns agree (use tuples with tuple subjects, mappings with dicts, etc.).
Pattern typing is detailed in [Pattern Matching Support](../static_analysis.md#pattern-matching-support).

## HM013 – `PatternStructureMismatch` {#hm013}

### Example

```py
def report(event):
    match event:
        case {"kind": kind, "meta": {"user": user}}:
            ...
```

If `event` is inferred as a tuple or class, the mapping pattern structure mismatches.

### Guidance

Use patterns whose structure matches the subject (mappings for dicts, class patterns for dataclasses, etc.).
Details live in [Pattern Matching Support](../static_analysis.md#pattern-matching-support).
