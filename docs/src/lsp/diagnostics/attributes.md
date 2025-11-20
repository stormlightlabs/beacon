# Attributes Diagnostics

Attribute diagnostics explain when a receiver type does not define the attribute being accessed.

## HM007 – `AttributeNotFound` {#hm007}

### Example

```py
count = 10
count.splitlines()  # Attribute does not exist on int
```

### Guidance

The analyzer could not find the attribute on the receiver type.
Narrow the type, convert the value, or fix typos.
Beacon adds contextual hints (e.g., “splitlines is a string method”).
See [Type Checking](../type_checking.md#type-inference) for attribute resolution notes.
