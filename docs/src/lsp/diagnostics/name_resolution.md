# Name Resolution Diagnostics

Name resolution diagnostics highlight names that were never defined anywhere in the file or workspace.

## undefined-variable – `UndefinedVariable` {#undefined-variable}

### Example

```py
print(total)  # `total` never defined
```

### Guidance

Define the name, import it, or limit the scope where it’s used.
Unlike `use-before-def`, this check runs at the file level via `Analyzer::find_unbound_variables` (see [Static Analyzer](../static_analysis.md)).
