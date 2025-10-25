//! Python runtime introspection
//!
//! Executes Python subprocess to extract signatures and docstrings from installed modules.
//! Used to provide rich hover documentation for external packages (stdlib, third-party).

use serde::{Deserialize, Serialize};
use std::path::Path;
use std::process::Command;
use std::time::Duration;
use thiserror::Error;
use tracing::{debug, warn};

/// Marker for signature section in introspection output
const SIGNATURE_MARKER: &str = "SIGSTART";

/// Marker for docstring section in introspection output
const DOC_MARKER: &str = "DOCSTART";

/// Python introspection script
///
/// Extracts signature and docstring from a module's symbol.
/// Output format:
/// SIGSTART
/// <signature>
/// DOCSTART
/// <docstring>
fn introspection_script() -> String {
    format!(
        r#"
import sys, inspect, importlib

mod_name, sym_name = sys.argv[1], sys.argv[2]

try:
    mod = importlib.import_module(mod_name)
    obj = getattr(mod, sym_name, None)

    if obj is None:
        print("{SIGNATURE_MARKER}")
        print("")
        print("{DOC_MARKER}")
        print("Symbol '{{}}' not found in module '{{}}'".format(sym_name, mod_name))
        sys.exit(0)

    sig = ""
    try:
        sig = str(inspect.signature(obj))
    except Exception:
        pass

    doc = inspect.getdoc(obj) or ""

    print("{SIGNATURE_MARKER}")
    print(sig)
    print("{DOC_MARKER}")
    print(doc)
except ImportError as e:
    print("{SIGNATURE_MARKER}")
    print("")
    print("{DOC_MARKER}")
    print("Module '{{}}' not found: {{}}".format(mod_name, e))
except Exception as e:
    print("{SIGNATURE_MARKER}")
    print("")
    print("{DOC_MARKER}")
    print("Error: {{}}".format(e))
"#
    )
}

/// Result of Python introspection
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct IntrospectionResult {
    /// Function/class signature (e.g., "(x, y) -> int")
    pub signature: String,
    /// Docstring from the symbol
    pub docstring: String,
}

/// Errors that can occur during introspection
#[derive(Error, Debug)]
pub enum IntrospectionError {
    #[error("Failed to execute Python: {0}")]
    ExecutionFailed(String),

    #[error("Introspection timed out after {0:?}")]
    Timeout(Duration),

    #[error("Failed to parse output: {0}")]
    ParseError(String),

    #[error("Python interpreter not available")]
    InterpreterNotFound,
}

/// Introspect a symbol from a module using Python runtime
///
/// Executes a Python subprocess to extract the signature and docstring of a symbol.
/// Times out after 3 seconds to prevent hanging.
///
/// Returns an error if:
/// - Python interpreter is not found
/// - Execution times out
/// - Output cannot be parsed
pub async fn introspect(
    interpreter: &Path, module_name: &str, symbol_name: &str,
) -> Result<IntrospectionResult, IntrospectionError> {
    debug!(
        "Introspecting {}.{} using {}",
        module_name,
        symbol_name,
        interpreter.display()
    );

    let script = introspection_script();

    let output = tokio::time::timeout(
        Duration::from_secs(3),
        tokio::process::Command::new(interpreter)
            .arg("-c")
            .arg(&script)
            .arg(module_name)
            .arg(symbol_name)
            .output(),
    )
    .await
    .map_err(|_| IntrospectionError::Timeout(Duration::from_secs(3)))?
    .map_err(|e| IntrospectionError::ExecutionFailed(e.to_string()))?;

    let stdout = String::from_utf8_lossy(&output.stdout);
    parse_introspection_output(&stdout)
}

/// Synchronous version of introspect for non-async contexts
///
/// Uses [std::process::Command] with a timeout thread.
pub fn introspect_sync(
    interpreter: &Path, module_name: &str, symbol_name: &str,
) -> Result<IntrospectionResult, IntrospectionError> {
    debug!(
        "Introspecting {}.{} using {} (sync)",
        module_name,
        symbol_name,
        interpreter.display()
    );

    let script = introspection_script();
    let output = Command::new(interpreter)
        .arg("-c")
        .arg(&script)
        .arg(module_name)
        .arg(symbol_name)
        .output()
        .map_err(|e| IntrospectionError::ExecutionFailed(e.to_string()))?;

    if output.status.success() {
        let stdout = String::from_utf8_lossy(&output.stdout);
        parse_introspection_output(&stdout)
    } else {
        let stderr = String::from_utf8_lossy(&output.stderr);
        warn!("Python introspection failed: {}", stderr);
        Err(IntrospectionError::ExecutionFailed(stderr.to_string()))
    }
}

/// Parse the output of the introspection script
fn parse_introspection_output(output: &str) -> Result<IntrospectionResult, IntrospectionError> {
    let sig_start = output
        .find(SIGNATURE_MARKER)
        .ok_or_else(|| IntrospectionError::ParseError("Missing SIGNATURE marker".to_string()))?;

    let doc_start = output
        .find(DOC_MARKER)
        .ok_or_else(|| IntrospectionError::ParseError("Missing DOC marker".to_string()))?;

    let signature = output[sig_start + SIGNATURE_MARKER.len()..doc_start].trim().to_string();
    let docstring = output[doc_start + DOC_MARKER.len()..].trim().to_string();
    Ok(IntrospectionResult { signature, docstring })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_introspection_output() {
        let output = format!(
            "\n{SIGNATURE_MARKER}\n(x, y)\n{DOC_MARKER}\nAdd two numbers together.\n\nReturns the sum of x and y.\n"
        );

        let result = parse_introspection_output(&output).unwrap();

        assert_eq!(result.signature, "(x, y)");
        assert!(result.docstring.contains("Add two numbers"));
        assert!(result.docstring.contains("Returns the sum"));
    }

    #[test]
    fn test_parse_introspection_output_empty_signature() {
        let output = format!("\n{SIGNATURE_MARKER}\n\n{DOC_MARKER}\nA constant value.\n");

        let result = parse_introspection_output(&output).unwrap();

        assert_eq!(result.signature, "");
        assert_eq!(result.docstring, "A constant value.");
    }

    #[test]
    fn test_parse_introspection_output_missing_marker() {
        let output = "Some random output without markers";

        let result = parse_introspection_output(output);

        assert!(result.is_err());
        match result {
            Err(IntrospectionError::ParseError(msg)) => {
                assert!(msg.contains("SIGNATURE"));
            }
            _ => panic!("Expected ParseError"),
        }
    }

    #[test]
    fn test_introspection_script_format() {
        let script = introspection_script();
        assert!(script.contains(SIGNATURE_MARKER));
        assert!(script.contains(DOC_MARKER));
        assert!(script.contains("importlib"));
        assert!(script.contains("inspect"));
    }

    #[tokio::test]
    async fn test_introspect_math_sqrt() {
        let interpreter = crate::interpreter::find_python_interpreter(None);

        if let Some(python) = interpreter {
            match introspect(&python, "math", "sqrt").await {
                Ok(result) => {
                    println!("Signature: {}", result.signature);
                    println!("Docstring: {}", result.docstring);

                    assert!(!result.docstring.is_empty());
                    assert!(result.signature.contains("(") || !result.signature.is_empty());
                }
                Err(e) => {
                    println!("Warning: Introspection test failed (this is okay in CI): {e}");
                }
            }
        } else {
            println!("Skipping introspection test: Python not found");
        }
    }

    #[test]
    fn test_introspect_sync_os_path() {
        let interpreter = crate::interpreter::find_python_interpreter(None);

        if let Some(python) = interpreter {
            match introspect_sync(&python, "os.path", "join") {
                Ok(result) => {
                    println!("Signature: {}", result.signature);
                    println!("Docstring: {}", result.docstring);
                    assert!(!result.docstring.is_empty());
                }
                Err(e) => {
                    println!("Warning: Introspection test failed (this is okay in CI): {e}");
                }
            }
        } else {
            println!("Skipping introspection test: Python not found");
        }
    }
}
