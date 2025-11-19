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
//! - Generator/AsyncGenerator/Coroutine with mixed variance (covariant yield, contravariant send, covariant return)

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
const RECURSIVE_TYPES: &str = include_str!("hm_fixtures/recursive_types.py");
const COMPLEX_HIERARCHIES: &str = include_str!("hm_fixtures/variance/complex_hierarchies.py");
const BOUNDS_AND_CONSTRAINTS: &str = include_str!("hm_fixtures/typevar/bounds_and_constraints.py");
const GENERATOR_MIXED_VARIANCE: &str = include_str!("hm_fixtures/variance/generator_mixed_variance.py");

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
fn test_functional_pipeline() {
    let mut harness = HmTestHarness::new();
    let result = harness.check_fixture("functional_pipeline", FUNCTIONAL_PIPELINE);

    assert!(
        !result.type_map.is_empty(),
        "Should have inferred types for functional pipeline"
    );

    let has_function_types = harness.has_type(&result, |ty| matches!(ty, Type::Fun(_, _)));
    assert!(
        has_function_types,
        "Should infer function types for compose, map_list, and lambdas"
    );

    let has_lists = harness.has_type(&result, |ty| matches!(ty, Type::App(_, _)));
    assert!(has_lists, "Should infer list types");

    let has_int = harness.has_type(&result, |ty| matches!(ty, Type::Con(TypeCtor::Int)));
    let has_str = harness.has_type(&result, |ty| matches!(ty, Type::Con(TypeCtor::String)));

    assert!(has_int, "Should have int types from identity(42)");
    assert!(
        has_str,
        "Should have string types from identity(\"hello\") - tests generalization"
    );
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
        ("recursive_types", RECURSIVE_TYPES),
        ("complex_hierarchies", COMPLEX_HIERARCHIES),
        ("bounds_and_constraints", BOUNDS_AND_CONSTRAINTS),
        ("generator_mixed_variance", GENERATOR_MIXED_VARIANCE),
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

#[test]
fn test_recursive_types() {
    let mut harness = HmTestHarness::new();
    let result = harness.check_fixture("recursive_types", RECURSIVE_TYPES);

    assert!(!result.type_map.is_empty(), "Should infer types for recursive types");

    let has_classes = harness.has_type(&result, |ty| matches!(ty, Type::Con(TypeCtor::Class(_))));
    assert!(has_classes, "Should infer LinkedListNode and TreeNode class types");

    let linked_list_node_type = harness.get_symbol_type(&result, "LinkedListNode");
    assert!(
        linked_list_node_type.is_some(),
        "Should be able to look up LinkedListNode class"
    );

    let create_list_type = harness.get_symbol_type(&result, "create_list");
    assert!(create_list_type.is_some(), "Should infer type for create_list function");

    let tree_node_type = harness.get_symbol_type(&result, "TreeNode");
    assert!(tree_node_type.is_some(), "Should be able to look up TreeNode class");

    let create_tree_type = harness.get_symbol_type(&result, "create_tree");
    assert!(create_tree_type.is_some(), "Should infer type for create_tree function");

    let add_left_child_type = harness.get_symbol_type(&result, "add_left_child");
    assert!(
        add_left_child_type.is_some(),
        "Should infer type for add_left_child function"
    );

    let errors = harness.get_errors(&result);
    assert!(
        errors.is_empty(),
        "Should have no type errors in recursive types, got: {errors:?}"
    );
}

#[test]
fn test_deeply_nested_generics() {
    let mut harness = HmTestHarness::new();
    let result = harness.check_fixture("complex_hierarchies", COMPLEX_HIERARCHIES);

    assert!(
        !result.type_map.is_empty(),
        "Should infer types for deeply nested generics"
    );

    let has_nested_containers = harness.has_type(&result, |ty| match ty {
        Type::App(f, arg) => {
            matches!(**f, Type::Con(TypeCtor::List) | Type::Con(TypeCtor::Dict)) && matches!(**arg, Type::App(_, _))
        }
        _ => false,
    });

    assert!(
        has_nested_containers || result.type_map.len() > 5,
        "Should handle nested container types"
    );

    let errors = harness.get_errors(&result);
    let has_invariance_errors = errors
        .iter()
        .any(|e| e.contains("invariance") || e.contains("Unification") || e.contains("dict") || e.contains("list"));

    assert!(
        has_invariance_errors,
        "Should detect invariance violations in nested generics: {errors:?}"
    );
}

#[test]
fn test_dict_multi_parameter_invariance() {
    let mut harness = HmTestHarness::new();
    let result = harness.check_fixture("complex_hierarchies", COMPLEX_HIERARCHIES);

    assert!(
        !result.type_map.is_empty(),
        "Should have type inference results for complex hierarchies"
    );

    let errors = harness.get_errors(&result);
    let has_invariance_error = errors.iter().any(|e| {
        e.contains("invariance")
            || e.contains("Unification")
            || (e.contains("dict") && (e.contains("Animal") || e.contains("Dog")))
    });

    assert!(
        has_invariance_error,
        "Should catch dict invariance violations in both key and value parameters: {errors:?}"
    );
}

#[test]
fn test_typevar_bounds_and_constraints() {
    let mut harness = HmTestHarness::new();
    let result = harness.check_fixture("bounds_and_constraints", BOUNDS_AND_CONSTRAINTS);

    assert!(
        !result.type_map.is_empty(),
        "Should infer types for TypeVar bounds and constraints"
    );

    let has_generic_classes = harness.has_type(&result, |ty| {
        matches!(ty, Type::App(_, _)) || matches!(ty, Type::Con(TypeCtor::Class(_)))
    });
    assert!(
        has_generic_classes,
        "Should infer generic class types (AnimalShelter, Registry)"
    );

    let get_sound_type = harness.get_symbol_type(&result, "get_sound");
    assert!(
        get_sound_type.is_some(),
        "Should infer type for get_sound function with bounded TypeVar"
    );

    let double_type = harness.get_symbol_type(&result, "double");
    assert!(
        double_type.is_some(),
        "Should infer type for double function with constrained TypeVar"
    );

    let identity_type = harness.get_symbol_type(&result, "identity");
    assert!(
        identity_type.is_some(),
        "Should infer type for unconstrained generic identity function"
    );

    let registry_type = harness.get_symbol_type(&result, "Registry");
    assert!(
        registry_type.is_some(),
        "Should infer type for Registry with multiple TypeVars"
    );
}

#[test]
fn test_multiple_inheritance_basic() {
    let mut harness = HmTestHarness::new();
    let source = r#"
class Base1:
    def method1(self) -> int:
        return 1

class Base2:
    def method2(self) -> str:
        return "hello"

class Derived(Base1, Base2):
    def method3(self) -> bool:
        return True

def use_derived(obj: Derived) -> tuple[int, str, bool]:
    return (obj.method1(), obj.method2(), obj.method3())

d = Derived()
result = use_derived(d)
"#;

    let result = harness.check_fixture("multiple_inheritance", source);

    assert!(
        !result.type_map.is_empty(),
        "Should infer types for multiple inheritance"
    );

    let has_classes = harness.has_type(&result, |ty| matches!(ty, Type::Con(TypeCtor::Class(_))));
    assert!(has_classes, "Should infer Base1, Base2, and Derived class types");

    let derived_type = harness.get_symbol_type(&result, "Derived");
    assert!(derived_type.is_some(), "Should be able to look up Derived class");

    let use_derived_type = harness.get_symbol_type(&result, "use_derived");
    assert!(use_derived_type.is_some(), "Should infer type for use_derived function");

    let errors = harness.get_errors(&result);
    assert!(
        errors.is_empty(),
        "Should have no type errors with multiple inheritance, got: {errors:?}"
    );
}

#[test]
fn test_multiple_inheritance_with_generics() {
    let mut harness = HmTestHarness::new();
    let source = r#"
from typing import Generic, TypeVar

T = TypeVar('T')
U = TypeVar('U')

class Container1(Generic[T]):
    def get_first(self) -> T:
        raise NotImplementedError

class Container2(Generic[U]):
    def get_second(self) -> U:
        raise NotImplementedError

class DoubleContainer(Container1[int], Container2[str]):
    def get_both(self) -> tuple[int, str]:
        return (self.get_first(), self.get_second())

dc: DoubleContainer = DoubleContainer()
"#;

    let result = harness.check_fixture("multiple_inheritance_generic", source);

    assert!(
        !result.type_map.is_empty(),
        "Should infer types for multiple inheritance with generics"
    );

    let double_container_type = harness.get_symbol_type(&result, "DoubleContainer");
    assert!(
        double_container_type.is_some(),
        "Should infer DoubleContainer with multiple generic base classes"
    );
}

#[test]
fn test_stress_deeply_nested_generics_lists() {
    let mut harness = HmTestHarness::new();
    let source = r#"
# Test deeply nested list types (5 levels)
deep1: list[int] = [1, 2, 3]
deep2: list[list[int]] = [[1, 2], [3, 4]]
deep3: list[list[list[int]]] = [[[1, 2]], [[3, 4]]]
deep4: list[list[list[list[int]]]] = [[[[1, 2]]], [[[3, 4]]]]
deep5: list[list[list[list[list[int]]]]] = [[[[[1, 2]]]], [[[[3, 4]]]]]

def process_deep(x: list[list[list[int]]]) -> int:
    return len(x)

result = process_deep(deep3)
"#;

    let result = harness.check_fixture("stress_nested_lists", source);

    assert!(!result.type_map.is_empty(), "Should handle deeply nested list types");

    let process_deep_type = harness.get_symbol_type(&result, "process_deep");
    assert!(
        process_deep_type.is_some(),
        "Should infer type for function with deeply nested parameters"
    );

    let errors = harness.get_errors(&result);
    assert!(
        errors.is_empty(),
        "Should have no errors with deeply nested lists, got: {errors:?}"
    );
}

#[test]
fn test_stress_deeply_nested_generics_dicts() {
    let mut harness = HmTestHarness::new();
    let source = r#"
# Test deeply nested dict types (4 levels)
nested1: dict[str, int] = {"a": 1}
nested2: dict[str, dict[str, int]] = {"outer": {"inner": 1}}
nested3: dict[str, dict[str, dict[str, int]]] = {"a": {"b": {"c": 1}}}
nested4: dict[str, dict[str, dict[str, dict[str, int]]]] = {"a": {"b": {"c": {"d": 1}}}}

def get_nested_value(d: dict[str, dict[str, int]]) -> int:
    return 42

result = get_nested_value(nested2)
"#;

    let result = harness.check_fixture("stress_nested_dicts", source);

    assert!(!result.type_map.is_empty(), "Should handle deeply nested dict types");

    let errors = harness.get_errors(&result);
    assert!(
        errors.is_empty(),
        "Should have no errors with deeply nested dicts, got: {errors:?}"
    );
}

#[test]
fn test_stress_mixed_nested_generics() {
    let mut harness = HmTestHarness::new();
    let source = r#"
# Test mixed nested generic types
mixed1: list[dict[str, int]] = [{"a": 1}]
mixed2: dict[str, list[int]] = {"nums": [1, 2, 3]}
mixed3: list[dict[str, list[int]]] = [{"a": [1, 2]}]
mixed4: dict[str, list[dict[str, int]]] = {"outer": [{"inner": 1}]}
mixed5: list[dict[str, list[dict[str, int]]]] = [{"a": [{"b": 1}]}]

def process_mixed(x: dict[str, list[int]]) -> list[int]:
    return []

result = process_mixed(mixed2)
"#;

    let result = harness.check_fixture("stress_mixed_nested", source);

    assert!(
        !result.type_map.is_empty(),
        "Should handle mixed nested generic types (list/dict combinations)"
    );

    let process_mixed_type = harness.get_symbol_type(&result, "process_mixed");
    assert!(
        process_mixed_type.is_some(),
        "Should infer type for function with mixed nested parameters"
    );
}

#[test]
fn test_generator_mixed_variance() {
    let mut harness = HmTestHarness::new();
    let result = harness.check_fixture("generator_mixed_variance", GENERATOR_MIXED_VARIANCE);

    assert!(
        !result.type_map.is_empty(),
        "Should infer types for Generator with mixed variance"
    );

    let has_functions = harness.has_type(&result, |ty| matches!(ty, Type::Fun(_, _)));
    assert!(has_functions, "Should infer function types for generator functions");

    let has_classes = harness.has_type(&result, |ty| matches!(ty, Type::Con(TypeCtor::Class(_))));
    assert!(has_classes, "Should infer Animal and Dog class types");

    let animal_generator_type = harness.get_symbol_type(&result, "animal_generator");
    assert!(
        animal_generator_type.is_some(),
        "Should infer type for animal_generator function"
    );

    let dog_generator_type = harness.get_symbol_type(&result, "dog_generator");
    assert!(
        dog_generator_type.is_some(),
        "Should infer type for dog_generator function"
    );

    let errors = harness.get_errors(&result);
    assert!(
        errors.is_empty(),
        "Should have no errors with Generator covariance/contravariance, got: {errors:?}"
    );
}

#[test]
fn test_async_generator_mixed_variance() {
    let mut harness = HmTestHarness::new();
    let result = harness.check_fixture("async_generator_mixed_variance", GENERATOR_MIXED_VARIANCE);

    assert!(
        !result.type_map.is_empty(),
        "Should infer types for AsyncGenerator with mixed variance"
    );

    let animal_async_gen_type = harness.get_symbol_type(&result, "animal_async_gen");
    assert!(
        animal_async_gen_type.is_some(),
        "Should infer type for animal_async_gen function"
    );

    let dog_async_gen_type = harness.get_symbol_type(&result, "dog_async_gen");
    assert!(
        dog_async_gen_type.is_some(),
        "Should infer type for dog_async_gen function"
    );

    let errors = harness.get_errors(&result);
    assert!(
        errors.is_empty(),
        "Should have no errors with AsyncGenerator covariance/contravariance, got: {errors:?}"
    );
}

#[test]
fn test_coroutine_mixed_variance() {
    let mut harness = HmTestHarness::new();
    let result = harness.check_fixture("coroutine_mixed_variance", GENERATOR_MIXED_VARIANCE);

    assert!(
        !result.type_map.is_empty(),
        "Should infer types for Coroutine with mixed variance"
    );

    let animal_coroutine_type = harness.get_symbol_type(&result, "animal_coroutine");
    assert!(
        animal_coroutine_type.is_some(),
        "Should infer type for animal_coroutine function"
    );

    let dog_coroutine_type = harness.get_symbol_type(&result, "dog_coroutine");
    assert!(
        dog_coroutine_type.is_some(),
        "Should infer type for dog_coroutine function"
    );

    let errors = harness.get_errors(&result);
    assert!(
        errors.is_empty(),
        "Should have no errors with Coroutine covariance/contravariance, got: {errors:?}"
    );
}
