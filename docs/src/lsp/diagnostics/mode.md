# Mode Diagnostics

Mode diagnostics are informational hints emitted when Beacon reports which type-checking mode produced a set of issues.

## MODE_INFO – `TypeCheckingMode` {#mode_info}

### Example

```text
Type checking mode: balanced (workspace default) - ...
```

### Guidance

Beacon appends this hint whenever diagnostics appear so you know whether strict/balanced/loose rules applied.
Use `# beacon: mode=strict` (etc.) to override as described in [Type Checking Modes](../../type-checking-modes.md).
