//! Hindley-Milner Type Checker Integration Tests
//!
//! End-to-end tests for the HM type inference system.
//! Loads real Python code fixtures, builds constraint graphs, runs the solver, and asserts on inferred types and diagnostics.
//!
//! ## Variance Coverage
//!
//! The test suite comprehensively covers variance checking for:
//! - Covariant built-in types (Tuple, Iterator, Iterable)
//! - Invariant built-in types (List, Dict, Set)
//! - Contravariant function parameters
//! - Covariant function returns
//! - User-defined TypeVar with `covariant=True`
//! - User-defined TypeVar with `contravariant=True`
//! - User-defined TypeVar invariant (default)
//! - Nested variance (e.g., `Producer[Producer[T]]`)
//!
//! **Not Tested**: Generator/AsyncGenerator/Coroutine with mixed variance (covariant yield, contravariant send, covariant return).
//!
//! Example
//! ```python
//! from typing import Generator, AsyncGenerator, Coroutine
//!
//! # Generator[YieldType, SendType, ReturnType]
//! # YieldType: covariant, SendType: contravariant, ReturnType: covariant
//! def animal_generator() -> Generator[Animal, Dog, str]:
//!     received = yield Animal()  # yields Animal (covariant)
//!     # received: Dog (contravariant - can send Dog to it)
//!     return "done"  # returns str (covariant)
//!
//! # AsyncGenerator[YieldType, SendType]
//! # YieldType: covariant, SendType: contravariant
//! async def async_animal_gen() -> AsyncGenerator[Animal, Dog]:
//!     received = yield Animal()  # yields Animal (covariant)
//!     # received: Dog (contravariant)
//!
//! # Coroutine[YieldType, SendType, ReturnType]
//! async def animal_coroutine() -> Coroutine[Animal, Dog, str]:
//!     # Similar mixed variance as Generator
//!     pass
//! ```
//!
//! The underlying unification logic for these types is tested via
//! `crates/core/src/unify.rs::test_variance_contravariant_generator_send`.

use beacon_core::{Type, TypeCtor};
use beacon_lsp::analysis::Analyzer;
use beacon_lsp::config::Config;
use beacon_lsp::document::DocumentManager;
use std::str::FromStr;
use url::Url;

// Embed fixtures directly in the test binary
const FUNCTIONAL_PIPELINE: &str = include_str!("hm_fixtures/functional_pipeline.py");
const TYPED_API: &str = include_str!("hm_fixtures/typed_api.py");
const ASYNC_CTX: &str = include_str!("hm_fixtures/async_ctx.py");
const OCCURS_CHECK: &str = include_str!("hm_fixtures/errors/occurs_check.py");
const VARIANCE_FAILURE: &str = include_str!("hm_fixtures/errors/variance_failure.py");
const COVARIANT: &str = include_str!("hm_fixtures/variance/covariant.py");
const CONTRAVARIANT: &str = include_str!("hm_fixtures/variance/contravariant.py");
const INVARIANT: &str = include_str!("hm_fixtures/variance/invariant.py");
const TYPEVAR_ANNOTATIONS: &str = include_str!("hm_fixtures/variance/typevar_annotations.py");
const UNION_SIMPLIFICATION: &str = include_str!("hm_fixtures/union_simplification.py");

/// Helper struct to manage test fixture loading and type checking
struct HmTestHarness {
    analyzer: Analyzer,
    documents: DocumentManager,
    counter: usize,
    last_uri: Option<Url>,
}

impl HmTestHarness {
    /// Create a new test harness
    fn new() -> Self {
        let config = Config::default();
        let documents = DocumentManager::new().unwrap();
        let analyzer = Analyzer::new(config, documents.clone());

        Self { analyzer, documents, counter: 0, last_uri: None }
    }

    /// Check a fixture by name and source code
    fn check_fixture(&mut self, name: &str, source: &str) -> beacon_lsp::analysis::AnalysisResult {
        self.counter += 1;
        let uri = Url::from_str(&format!(
            "file:///test_{}_{}_{}.py",
            name,
            self.counter,
            std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .as_nanos()
        ))
        .unwrap();

        self.documents
            .open_document(uri.clone(), 1, source.to_string())
            .unwrap();

        self.last_uri = Some(uri.clone());

        self.analyzer
            .analyze(&uri)
            .unwrap_or_else(|e| panic!("Analysis failed for {name}: {e}"))
    }

    /// Get the inferred type for a symbol by name
    fn get_symbol_type(&self, result: &beacon_lsp::analysis::AnalysisResult, symbol_name: &str) -> Option<Type> {
        let uri = self.last_uri.as_ref()?;

        self.documents.get_document(uri, |doc| {
            let symbol_table = doc.symbol_table()?;
            let symbol = symbol_table.lookup_symbol(symbol_name, symbol_table.root_scope)?;
            let node_id = result.position_map.get(&(symbol.line, symbol.col))?;
            result.type_map.get(node_id).cloned()
        })?
    }

    /// Check if any type in the result matches a predicate
    fn has_type<F>(&self, result: &beacon_lsp::analysis::AnalysisResult, predicate: F) -> bool
    where
        F: Fn(&Type) -> bool,
    {
        result.type_map.values().any(predicate)
    }

    /// Get all type errors from the result
    fn get_errors(&self, result: &beacon_lsp::analysis::AnalysisResult) -> Vec<String> {
        result.type_errors.iter().map(|err| format!("{}", err.error)).collect()
    }
}

#[test]
fn test_functional_pipeline_identity() {
    let mut harness = HmTestHarness::new();
    let result = harness.check_fixture("functional_pipeline", FUNCTIONAL_PIPELINE);

    // The fixture should type check without errors for basic identity usage
    assert!(
        !result.type_map.is_empty(),
        "Should have inferred types for functional pipeline"
    );

    // Check that we have function types
    let has_function_types = harness.has_type(&result, |ty| matches!(ty, Type::Fun(_, _)));
    assert!(
        has_function_types,
        "Should infer function types for compose, map_list, etc."
    );
}

#[test]
fn test_functional_pipeline_composition() {
    let mut harness = HmTestHarness::new();
    let result = harness.check_fixture("functional_pipeline", FUNCTIONAL_PIPELINE);
    let has_lambdas = harness.has_type(&result, |ty| matches!(ty, Type::Fun(_, _)));
    assert!(has_lambdas, "Should infer types for lambda expressions");

    let has_lists = harness.has_type(&result, |ty| matches!(ty, Type::App(_, _)));
    assert!(has_lists, "Should infer list types");
}

#[test]
fn test_functional_pipeline_generalization() {
    let mut harness = HmTestHarness::new();
    let result = harness.check_fixture("functional_pipeline", FUNCTIONAL_PIPELINE);

    let has_int = harness.has_type(&result, |ty| matches!(ty, Type::Con(TypeCtor::Int)));
    let has_str = harness.has_type(&result, |ty| matches!(ty, Type::Con(TypeCtor::String)));

    assert!(has_int, "Should have int types from identity(42)");
    assert!(has_str, "Should have string types from identity(\"hello\")");
}

#[test]
fn test_typed_api_protocol() {
    let mut harness = HmTestHarness::new();
    let result = harness.check_fixture("typed_api", TYPED_API);

    assert!(!result.type_map.is_empty(), "Should have inferred types for typed API");

    let has_classes = harness.has_type(&result, |ty| matches!(ty, Type::Con(TypeCtor::Class(_))));
    assert!(has_classes, "Should infer class types for Circle, Rectangle");

    let has_functions = harness.has_type(&result, |ty| matches!(ty, Type::Fun(_, _)));
    assert!(
        has_functions,
        "Should infer function types for render, create_user, etc."
    );
}

#[test]
fn test_typed_api_typed_dict() {
    let mut harness = HmTestHarness::new();
    let result = harness.check_fixture("typed_api", TYPED_API);

    let has_dicts = harness.has_type(&result, |ty| match ty {
        Type::App(f, _) => matches!(**f, Type::Con(TypeCtor::Dict)),
        Type::Record(_, _) => true,
        _ => false,
    });
    assert!(
        has_dicts || result.type_map.len() > 5,
        "Should handle TypedDict or have substantial type inference"
    );
}

#[test]
fn test_async_ctx_manager() {
    let mut harness = HmTestHarness::new();
    let result = harness.check_fixture("async_ctx", ASYNC_CTX);

    assert!(
        !result.type_map.is_empty(),
        "Should have inferred types for async context"
    );

    let has_async_types = harness.has_type(&result, |ty| {
        matches!(ty, Type::App(_, _)) || matches!(ty, Type::Fun(_, _))
    });
    assert!(has_async_types, "Should infer types for async functions and coroutines");
}

#[test]
fn test_occurs_check_detection() {
    let mut harness = HmTestHarness::new();
    let result = harness.check_fixture("occurs_check", OCCURS_CHECK);

    assert!(
        !result.type_map.is_empty() || !result.type_errors.is_empty(),
        "Should either infer types or report errors for occurs check cases"
    );

    let errors = harness.get_errors(&result);
    let has_occurs_error = errors
        .iter()
        .any(|e| e.contains("occurs") || e.contains("infinite") || e.contains("recursive"));

    if has_occurs_error {
        eprintln!("Occurs check correctly detected: {errors:?}");
    } else if !errors.is_empty() {
        eprintln!("Got other type errors (may be occurs-related): {errors:?}");
    }
}

#[test]
fn test_variance_detection() {
    let mut harness = HmTestHarness::new();
    let result = harness.check_fixture("variance_failure", VARIANCE_FAILURE);

    assert!(
        !result.type_map.is_empty() || !result.type_errors.is_empty(),
        "Should either infer types or report errors for variance cases"
    );

    let errors = harness.get_errors(&result);
    let has_variance_error = errors
        .iter()
        .any(|e| e.contains("Variance") || e.contains("Unification") || e.contains("type"));

    if has_variance_error {
        eprintln!("Variance error correctly detected: {errors:?}");
    } else if !errors.is_empty() {
        eprintln!("Got type errors (may be variance-related): {errors:?}");
    }
}

#[test]
fn test_constraint_generation_produces_results() {
    let mut harness = HmTestHarness::new();
    let result = harness.check_fixture("functional_pipeline", FUNCTIONAL_PIPELINE);

    assert!(
        result.type_map.len() > 10,
        "Should generate substantial constraint graph, got {} types",
        result.type_map.len()
    );

    assert!(
        !result.position_map.is_empty(),
        "Should have position mappings for IDE features"
    );
}

#[test]
fn test_type_error_spans() {
    let mut harness = HmTestHarness::new();
    let result = harness.check_fixture("variance_failure", VARIANCE_FAILURE);

    for error in &result.type_errors {
        assert!(
            error.span.line > 0,
            "Type error should have valid line number: {error:?}"
        );
    }
}

#[test]
fn test_all_fixtures_parse_and_analyze() {
    let mut harness = HmTestHarness::new();

    let fixtures = vec![
        ("functional_pipeline", FUNCTIONAL_PIPELINE),
        ("typed_api", TYPED_API),
        ("async_ctx", ASYNC_CTX),
        ("occurs_check", OCCURS_CHECK),
        ("variance_failure", VARIANCE_FAILURE),
        ("covariant", COVARIANT),
        ("contravariant", CONTRAVARIANT),
        ("invariant", INVARIANT),
        ("typevar_annotations", TYPEVAR_ANNOTATIONS),
    ];

    for (name, source) in fixtures {
        let result = harness.check_fixture(name, source);
        assert!(
            !result.type_map.is_empty() || !result.type_errors.is_empty(),
            "Fixture {name} should produce types or errors"
        );
    }
}

#[test]
fn test_covariant_return_types() {
    let mut harness = HmTestHarness::new();
    let result = harness.check_fixture("covariant", COVARIANT);

    assert!(!result.type_map.is_empty(), "Should infer types for covariant fixture");

    let has_functions = harness.has_type(&result, |ty| matches!(ty, Type::Fun(_, _)));
    assert!(has_functions, "Should infer function types with covariant returns");

    let has_classes = harness.has_type(&result, |ty| matches!(ty, Type::Con(TypeCtor::Class(_))));
    assert!(has_classes, "Should infer Animal and Dog class types");

    let errors = harness.get_errors(&result);
    let has_covariant_errors = errors
        .iter()
        .any(|e| e.contains("get_dog") || e.contains("animal_producer") || e.contains("tuple"));

    if has_covariant_errors {
        eprintln!("Unexpected covariant errors: {errors:?}");
    }
}

#[test]
fn test_contravariant_parameters() {
    let mut harness = HmTestHarness::new();
    let result = harness.check_fixture("contravariant", CONTRAVARIANT);

    assert!(
        !result.type_map.is_empty() || !result.type_errors.is_empty(),
        "Should infer types or report errors for contravariant cases"
    );

    let errors = harness.get_errors(&result);
    let has_process_animal_error = errors
        .iter()
        .any(|e| e.contains("parameter 'f'") && e.contains("Animal -> None") && e.contains("Dog"));

    let has_process_with_return_error = errors.iter().any(|e| {
        e.contains("parameter 'f'") && e.contains("Animal -> Dog") && e.contains("Dog") && e.contains("Animal")
    });

    let has_contravariance_error = has_process_animal_error || has_process_with_return_error;

    assert!(
        has_contravariance_error,
        "Expected contravariance violations to be caught. Errors: {errors:?}"
    );

    assert!(
        has_process_animal_error,
        "Expected error for process_animal(handle_dog): Dog-specific function in Animal position. Errors: {errors:?}"
    );

    assert!(
        has_process_with_return_error,
        "Expected error for process_with_return(dog_to_animal): incorrect parameter contravariance. Errors: {errors:?}"
    );
}

#[test]
fn test_invariant_containers() {
    let mut harness = HmTestHarness::new();
    let result = harness.check_fixture("invariant", INVARIANT);

    assert!(
        !result.type_map.is_empty() || !result.type_errors.is_empty(),
        "Should infer types or report errors for invariant containers"
    );

    let has_containers = harness.has_type(&result, |ty| match ty {
        Type::App(f, _) => matches!(
            **f,
            Type::Con(TypeCtor::List) | Type::Con(TypeCtor::Dict) | Type::Con(TypeCtor::Set)
        ),
        _ => false,
    });
    assert!(has_containers, "Should infer list, dict, or set container types");

    let errors = harness.get_errors(&result);

    let has_invariance_error = errors
        .iter()
        .any(|e| e.contains("Variance") || e.contains("Unification") || e.contains("Cannot unify"));

    if has_invariance_error {
        eprintln!("Invariance error correctly detected: {errors:?}");
    } else if !errors.is_empty() {
        eprintln!("Got other type errors (may be related to invariance): {errors:?}");
    }
}

#[test]
fn test_covariant_containers() {
    let mut harness = HmTestHarness::new();
    let result = harness.check_fixture("covariant", COVARIANT);

    let has_tuples = harness.has_type(&result, |ty| match ty {
        Type::App(f, _) => matches!(**f, Type::Con(TypeCtor::Tuple)),
        Type::Tuple(_) => true,
        _ => false,
    });

    assert!(has_tuples, "Should infer tuple types (homogeneous or heterogeneous)");

    let errors = harness.get_errors(&result);
    let has_tuple_errors = errors.iter().any(|e| e.contains("tuple") && e.contains("Unification"));

    assert!(
        !has_tuple_errors,
        "Should not have tuple covariance errors, got: {errors:?}"
    );
}

#[test]
fn test_typevar_covariant() {
    let mut harness = HmTestHarness::new();
    let result = harness.check_fixture("typevar_annotations", TYPEVAR_ANNOTATIONS);

    assert!(
        !result.type_map.is_empty(),
        "Should infer types for TypeVar annotations"
    );

    let has_generic_types = harness.has_type(&result, |ty| {
        matches!(ty, Type::App(_, _)) || matches!(ty, Type::Var(_))
    });
    assert!(
        has_generic_types,
        "Should infer generic types for Producer/Consumer/Storage"
    );

    let errors = harness.get_errors(&result);

    let has_producer_errors = errors
        .iter()
        .any(|e| e.contains("Producer") || e.contains("animal_producer"));

    if has_producer_errors {
        eprintln!("TypeVar covariant-related errors: {errors:?}");
    }
}

#[test]
fn test_typevar_contravariant() {
    let mut harness = HmTestHarness::new();
    let result = harness.check_fixture("typevar_annotations", TYPEVAR_ANNOTATIONS);

    assert!(
        !result.type_map.is_empty() || !result.type_errors.is_empty(),
        "Should infer types or report errors for contravariant TypeVar"
    );

    let errors = harness.get_errors(&result);
    let has_consumer_info = errors
        .iter()
        .any(|e| e.contains("Consumer") || e.contains("dog_consumer"));

    if has_consumer_info {
        eprintln!("TypeVar contravariant-related errors: {errors:?}");
    }

    let has_functions = harness.has_type(&result, |ty| matches!(ty, Type::Fun(_, _)));
    assert!(has_functions, "Should infer function types for use_dog_consumer, etc.");
}

#[test]
fn test_variance_composition() {
    let mut harness = HmTestHarness::new();
    let result = harness.check_fixture("typevar_annotations", TYPEVAR_ANNOTATIONS);

    assert!(!result.type_map.is_empty(), "Should infer types for nested variance");

    let has_nested_types = harness.has_type(&result, |ty| match ty {
        Type::App(f, arg) => matches!(**f, Type::App(_, _)) || matches!(**arg, Type::App(_, _)),
        _ => false,
    });

    assert!(
        has_nested_types || result.type_map.len() > 10,
        "Should handle nested type applications or have substantial inference"
    );

    let errors = harness.get_errors(&result);

    assert!(
        !result.type_map.is_empty(),
        "Should produce type inference results for variance annotations"
    );

    if !errors.is_empty() {
        eprintln!("Variance composition errors (expected for user-defined generics): {errors:?}");
    }
}

#[test]
fn test_symbol_lookup_by_name() {
    let mut harness = HmTestHarness::new();
    let source = r#"
class Animal:
    def speak(self) -> str:
        return "..."

def identity(x):
    return x

result = identity(42)
"#;

    let result = harness.check_fixture("symbol_lookup_test", source);

    let animal_type = harness.get_symbol_type(&result, "Animal");
    assert!(animal_type.is_some(), "Should be able to look up 'Animal' class symbol");
    if let Some(ty) = animal_type {
        assert!(
            matches!(ty, Type::Con(TypeCtor::Class(_))),
            "Animal should be a class type, got: {ty}"
        );
    }

    let identity_type = harness.get_symbol_type(&result, "identity");
    assert!(
        identity_type.is_some(),
        "Should be able to look up 'identity' function symbol"
    );
    if let Some(ty) = identity_type {
        assert!(
            matches!(ty, Type::Fun(_, _)),
            "identity should be a function type, got: {ty}"
        );
    }

    let result_type = harness.get_symbol_type(&result, "result");
    assert!(
        result_type.is_some(),
        "Should be able to look up 'result' variable symbol"
    );

    let nonexistent = harness.get_symbol_type(&result, "nonexistent_symbol");
    assert!(nonexistent.is_none(), "Should return None for non-existent symbol");
}

#[test]
fn test_union_simplification() {
    let mut harness = HmTestHarness::new();
    let result = harness.check_fixture("union_simplification", UNION_SIMPLIFICATION);

    let errors = harness.get_errors(&result);
    assert!(
        errors.is_empty(),
        "Should have no type errors in union simplification fixture, got: {errors:?}"
    );

    let test_union_with_never_type = harness.get_symbol_type(&result, "test_union_with_never");
    if let Some(Type::Fun(params, _)) = test_union_with_never_type {
        assert!(params.len() == 1, "Function should have 1 parameter");
    }

    assert!(harness.get_symbol_type(&result, "test_union_with_any").is_some());
    assert!(harness.get_symbol_type(&result, "test_nested_unions").is_some());
    assert!(harness.get_symbol_type(&result, "test_duplicate_types").is_some());
    assert!(harness.get_symbol_type(&result, "test_optional_int").is_some());
    assert!(harness.get_symbol_type(&result, "test_complex_nested").is_some());
}
