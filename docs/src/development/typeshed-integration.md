# Typeshed Integration

Beacon integrates Python standard library type stubs from the official [python/typeshed](https://github.com/python/typeshed) repository. The stubs are embedded at build time using a git submodule, providing version-controlled, reproducible type information for the analyzer.

## Current Version

Typeshed stubs are tracked as a git submodule at `typeshed/`. To check the current version:

```sh
cd typeshed
git log -1 --format='%H %ci %s'
```

The submodule points to [stormlightlabs/typeshed-stdlib-mirror](https://github.com/stormlightlabs/typeshed-stdlib-mirror), which provides a flattened mirror of typeshed's `stdlib` and `_typeshed` directories.

## Stub Lookup Architecture

Beacon uses a layered stub resolution system with the following priority order:

1. **Manual stubs** - Configured via `config.stub_paths` (highest priority)
2. **Stub packages** - Directories matching `*-stubs` pattern
3. **Inline stubs** - `.pyi` files located alongside `.py` files
4. **Typeshed stubs** - Embedded stdlib stubs (fallback)

Builtins are loaded upfront during initialization. Other modules are loaded on-demand during constraint generation when imports are encountered.

## Updating Typeshed

The typeshed submodule can be updated to pull in newer stub definitions from upstream.

### Check Available Updates

View recent commits from python/typeshed:

```sh
cd typeshed
./scripts/metadata.sh --limit 10
```

Options for filtering commits:

- `--since DATE` - Show commits after date (YYYY-MM-DD)
- `--until DATE` - Show commits before date (YYYY-MM-DD)
- `--author NAME` - Filter by GitHub username or email
- `--grep PATTERN` - Search commit messages
- `--sha-only` - Output only commit SHAs

### Update to Specific Version

Fetch stubs from a specific python/typeshed commit:

```sh
cd typeshed
./scripts/fetch.sh <commit-sha>
```

This fetches the specified commit, flattens the `stdlib` and `_typeshed` directories into `stubs/`, and creates `COMMIT.txt` with metadata.

### Update to Latest

Fetch and commit the latest typeshed version:

```sh
cd typeshed
./scripts/metadata.sh --limit 1 --sha-only | xargs ./scripts/fetch.sh
./scripts/commit.sh
cd ..
git add typeshed
git commit -m "chore: update typeshed submodule"
```

### Manual Commit

After fetching stubs, commit changes manually:

```sh
cd typeshed
git add stubs COMMIT.txt
git commit -m "Bump typeshed stdlib to <commit-sha>"
git push
cd ..
git add typeshed
git commit -m "chore: update typeshed submodule"
```

## Build Integration

Typeshed stubs are embedded into the Beacon binary at compile time. The build process:

1. Reads stub files from `typeshed/stubs/` directory
2. Embeds them using Rust's `include_str!` macro
3. Makes stubs available via `get_embedded_stub(module_name)` API

No runtime network access or file system dependency is required for stdlib type information.

## Custom Beacon Stubs

Beacon-specific stubs that extend or override standard library behavior are kept in `crates/server/stubs/`:

- `capabilities_support.pyi` - Beacon-specific protocol definitions

These stubs have higher priority than embedded typeshed stubs due to the layered lookup system.

## Testing

Stub resolution is tested through:

- **Unit tests** - Verify layered stub lookup and module resolution
- **Integration tests** - Validate stdlib type checking with typeshed stubs
- **Analyzer tests** - Check method resolution through inheritance chains

Test fixtures that require stub files are located in `crates/server/tests/fixtures/`.

## Related Documentation

- [PEP 561 - Distributing and Packaging Type Information](../learn/pep-561-stubs.md)
- [Development Quick Start](./overview.md)
- [Testing Strategy](../lsp/testing.md)
