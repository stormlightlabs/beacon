# Formatter CLI

The `format` command exposes Beacon's Python formatter without having to spin up the language server.  
It is helpful for debugging formatter behaviour (for example, while comparing
`samples/capabilities_support.py` against the generated `samples/capabilities_support_formatted.py`).

## Usage

```sh
beacon-cli format [OPTIONS] [FILE]
```

- Reads from the specified `FILE`. If omitted, source is read from `stdin`.
- Writes formatted output to `stdout` by default.

### Options

| Flag | Description |
| --- | --- |
| `--write` | Overwrite the given file with formatted output. Requires `FILE`. |
| `--check` | Exit with a non-zero status if formatting would change the input. |
| `--output <PATH>` | Write formatted output to a different file instead of `stdout`. |

`--write` conflicts with both `--check` and `--output` to prevent accidental combinations.

## Examples

Format a module and show the result in the terminal:

```sh
beacon-cli format samples/capabilities_support.py
```

Overwrite the file in-place:

```sh
beacon-cli format samples/capabilities_support.py --write
```

Guard against accidental diffs in CI:

```sh
beacon-cli format samples/capabilities_support.py --check
```

Write the formatted output to a side file:

```sh
beacon-cli format samples/capabilities_support.py --output samples/capabilities_support_formatted.py
```
