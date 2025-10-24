# PyDoc Retrieval

The language server enriches hover and completion items for third-party Python packages by executing a short-lived Python subprocess to read real docstrings and signatures from the user's environment.

## Interpreter Discovery

`find_python_interpreter` in `crates/server/src/interpreter.rs` walks common virtual environment managers (Poetry, Pipenv, uv) before falling back to `python` on the `PATH`.
Each probe shells out (`poetry env info -p`, `pipenv --venv`, `uv python find`) and returns the interpreter inside the virtual environment when successful.
The search runs per workspace and only logs at `debug` level on success. Missing tools or failures are tolerated—only a final `warn!` is emitted if no interpreter can be located.
Interpreter lookups currently rely on external commands and inherit their environment; this will eventually be an explicit path via LSP settings.

## Introspection Flow

When a hover needs documentation for `module.symbol`, we call `introspect` in `crates/server/src/introspection.rs` with the discovered interpreter.
`introspect` constructs a tiny Python script that imports the target module, fetches the attribute, and prints two sentinel sections: `SIGSTART` (signature) and `DOCSTART` (docstring).
The async path spawns `tokio::process::Command`, while `introspect_sync` uses `std::process::Command`.
Both share parsing logic via `parse_introspection_output`.
The script uses `inspect.signature` and `inspect.getdoc`, so it respects docstring inheritance and returns cleaned whitespace.
Failures to inspect still return whatever data is available.

## Parsing and Error Handling

Results are parsed by scanning for the sentinel lines and trimming the sections, yielding an `IntrospectionResult { signature, docstring }`.
Timeouts (3 seconds) protect the async path from hanging interpreters.
Other errors—missing module, attribute, or import failure—come back as `IntrospectionError::ExecutionFailed` with the stderr payload for debugging.
We log subprocess stderr on failure but avoid surfacing internal exceptions directly to the client.

## Testing Guarantees

Unit tests cover the parser, confirm the generated script embeds the sentinels, and run best-effort smoke tests against standard library symbols when a Python interpreter is available.
Tests skip gracefully if Python cannot be located, keeping CI green on machines without Python.
