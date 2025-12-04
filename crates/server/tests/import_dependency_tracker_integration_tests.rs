//! Integration tests for ImportDependencyTracker
//!
//! Verifies that the tracker correctly identifies files that need re-analysis when imported symbols change.

use beacon_lsp::cache::{CacheManager, ImportDependencyTracker};
use std::sync::Arc;
use url::Url;

/// Helper to create file URIs from simple paths
/// TODO: move to shared helper module ([beacon_core]?)
fn file_uri(path: &str) -> Url {
    Url::parse(&format!("file:///{path}")).unwrap()
}

#[test]
fn test_single_import_tracking() {
    let mut tracker = ImportDependencyTracker::new();
    let module = file_uri("src/utils.py");
    let importer = file_uri("src/main.py");

    tracker.add_import(&module, "helper_function", &importer);

    let importers = tracker.get_symbol_importers(&module, "helper_function");
    assert_eq!(importers.len(), 1);
    assert!(importers.contains(&importer));
}

#[test]
fn test_multiple_files_importing_same_symbol() {
    let mut tracker = ImportDependencyTracker::new();
    let module = file_uri("src/models.py");
    let importer1 = file_uri("src/views.py");
    let importer2 = file_uri("src/controllers.py");
    let importer3 = file_uri("src/tests.py");

    tracker.add_import(&module, "User", &importer1);
    tracker.add_import(&module, "User", &importer2);
    tracker.add_import(&module, "User", &importer3);

    let importers = tracker.get_symbol_importers(&module, "User");
    assert_eq!(importers.len(), 3);
    assert!(importers.contains(&importer1));
    assert!(importers.contains(&importer2));
    assert!(importers.contains(&importer3));
}

#[test]
fn test_multiple_symbols_from_same_module() {
    let mut tracker = ImportDependencyTracker::new();
    let module = file_uri("src/database.py");
    let importer = file_uri("src/app.py");

    tracker.add_import(&module, "connect", &importer);
    tracker.add_import(&module, "disconnect", &importer);
    tracker.add_import(&module, "execute_query", &importer);

    let connect_importers = tracker.get_symbol_importers(&module, "connect");
    let disconnect_importers = tracker.get_symbol_importers(&module, "disconnect");
    let query_importers = tracker.get_symbol_importers(&module, "execute_query");

    assert_eq!(connect_importers.len(), 1);
    assert_eq!(disconnect_importers.len(), 1);
    assert_eq!(query_importers.len(), 1);
    assert!(connect_importers.contains(&importer));
    assert!(disconnect_importers.contains(&importer));
    assert!(query_importers.contains(&importer));
}

#[test]
fn test_clear_imports_from_file() {
    let mut tracker = ImportDependencyTracker::new();
    let module1 = file_uri("src/utils.py");
    let module2 = file_uri("src/helpers.py");
    let importer = file_uri("src/main.py");

    tracker.add_import(&module1, "func_a", &importer);
    tracker.add_import(&module2, "func_b", &importer);

    assert_eq!(tracker.get_symbol_importers(&module1, "func_a").len(), 1);
    assert_eq!(tracker.get_symbol_importers(&module2, "func_b").len(), 1);

    tracker.clear_imports_from(&importer);

    assert_eq!(tracker.get_symbol_importers(&module1, "func_a").len(), 0);
    assert_eq!(tracker.get_symbol_importers(&module2, "func_b").len(), 0);
}

#[test]
fn test_clear_imports_preserves_other_importers() {
    let mut tracker = ImportDependencyTracker::new();
    let module = file_uri("src/models.py");
    let importer1 = file_uri("src/views.py");
    let importer2 = file_uri("src/controllers.py");

    tracker.add_import(&module, "User", &importer1);
    tracker.add_import(&module, "User", &importer2);

    assert_eq!(tracker.get_symbol_importers(&module, "User").len(), 2);

    tracker.clear_imports_from(&importer1);

    let importers = tracker.get_symbol_importers(&module, "User");
    assert_eq!(importers.len(), 1);
    assert!(importers.contains(&importer2));
    assert!(!importers.contains(&importer1));
}

#[test]
fn test_get_nonexistent_symbol() {
    let tracker = ImportDependencyTracker::new();
    let module = file_uri("src/utils.py");

    let importers = tracker.get_symbol_importers(&module, "nonexistent_symbol");
    assert_eq!(importers.len(), 0);
}

#[test]
fn test_duplicate_import_tracking() {
    let mut tracker = ImportDependencyTracker::new();
    let module = file_uri("src/utils.py");
    let importer = file_uri("src/main.py");

    tracker.add_import(&module, "helper", &importer);
    tracker.add_import(&module, "helper", &importer);
    tracker.add_import(&module, "helper", &importer);

    let importers = tracker.get_symbol_importers(&module, "helper");
    assert_eq!(importers.len(), 1);
}

#[test]
fn test_clear_all_imports() {
    let mut tracker = ImportDependencyTracker::new();
    let module1 = file_uri("src/utils.py");
    let module2 = file_uri("src/helpers.py");
    let importer1 = file_uri("src/main.py");
    let importer2 = file_uri("src/app.py");

    tracker.add_import(&module1, "func_a", &importer1);
    tracker.add_import(&module2, "func_b", &importer2);

    assert_eq!(tracker.get_symbol_importers(&module1, "func_a").len(), 1);
    assert_eq!(tracker.get_symbol_importers(&module2, "func_b").len(), 1);

    tracker.clear();

    assert_eq!(tracker.get_symbol_importers(&module1, "func_a").len(), 0);
    assert_eq!(tracker.get_symbol_importers(&module2, "func_b").len(), 0);
}

#[test]
fn test_diamond_dependency_pattern() {
    let mut tracker = ImportDependencyTracker::new();
    let base = file_uri("src/base.py");
    let left = file_uri("src/left.py");
    let right = file_uri("src/right.py");
    let top = file_uri("src/top.py");

    tracker.add_import(&base, "BaseClass", &left);
    tracker.add_import(&base, "BaseClass", &right);
    tracker.add_import(&left, "LeftClass", &top);
    tracker.add_import(&right, "RightClass", &top);

    let base_importers = tracker.get_symbol_importers(&base, "BaseClass");
    assert_eq!(base_importers.len(), 2);
    assert!(base_importers.contains(&left));
    assert!(base_importers.contains(&right));

    let left_importers = tracker.get_symbol_importers(&left, "LeftClass");
    assert_eq!(left_importers.len(), 1);
    assert!(left_importers.contains(&top));

    let right_importers = tracker.get_symbol_importers(&right, "RightClass");
    assert_eq!(right_importers.len(), 1);
    assert!(right_importers.contains(&top));
}

#[test]
fn test_cache_manager_integration() {
    let manager = CacheManager::new();
    let module = file_uri("src/models.py");
    let importer1 = file_uri("src/views.py");
    let importer2 = file_uri("src/controllers.py");

    manager.record_import(&module, "User", &importer1);
    manager.record_import(&module, "User", &importer2);
    manager.record_import(&module, "Post", &importer1);

    let user_importers = manager.get_symbol_importers(&module, "User");
    assert_eq!(user_importers.len(), 2);

    let post_importers = manager.get_symbol_importers(&module, "Post");
    assert_eq!(post_importers.len(), 1);

    manager.invalidate_document(&importer1);

    let user_importers_after = manager.get_symbol_importers(&module, "User");
    assert_eq!(user_importers_after.len(), 1);
    assert!(user_importers_after.contains(&importer2));

    let post_importers_after = manager.get_symbol_importers(&module, "Post");
    assert_eq!(post_importers_after.len(), 0);
}

#[test]
fn test_re_analysis_workflow() {
    let manager = CacheManager::new();
    let module = file_uri("src/utils.py");
    let importer = file_uri("src/main.py");

    manager.record_import(&module, "helper", &importer);
    manager.record_import(&module, "calculate", &importer);

    let importers_before = manager.get_symbol_importers(&module, "helper");
    assert_eq!(importers_before.len(), 1);

    manager.invalidate_document(&importer);

    manager.record_import(&module, "helper", &importer);
    manager.record_import(&module, "new_function", &importer);

    let helper_importers = manager.get_symbol_importers(&module, "helper");
    let calculate_importers = manager.get_symbol_importers(&module, "calculate");
    let new_func_importers = manager.get_symbol_importers(&module, "new_function");

    assert_eq!(helper_importers.len(), 1);
    assert_eq!(calculate_importers.len(), 0);
    assert_eq!(new_func_importers.len(), 1);
}

#[test]
fn test_large_scale_imports() {
    let mut tracker = ImportDependencyTracker::new();
    let module = file_uri("src/api.py");

    for i in 0..100 {
        let importer = file_uri(&format!("src/client_{i}.py"));
        tracker.add_import(&module, "ApiClient", &importer);
        tracker.add_import(&module, "Request", &importer);
    }

    let api_client_importers = tracker.get_symbol_importers(&module, "ApiClient");
    let request_importers = tracker.get_symbol_importers(&module, "Request");

    assert_eq!(api_client_importers.len(), 100);
    assert_eq!(request_importers.len(), 100);

    let first_importer = file_uri("src/client_0.py");
    tracker.clear_imports_from(&first_importer);

    let api_client_after = tracker.get_symbol_importers(&module, "ApiClient");
    let request_after = tracker.get_symbol_importers(&module, "Request");

    assert_eq!(api_client_after.len(), 99);
    assert_eq!(request_after.len(), 99);
    assert!(!api_client_after.contains(&first_importer));
}

#[test]
fn test_wildcard_import_simulation() {
    let mut tracker = ImportDependencyTracker::new();
    let module = file_uri("src/utils.py");
    let importer = file_uri("src/main.py");

    let symbols = vec!["func_a", "func_b", "func_c", "CONSTANT_X", "HelperClass"];

    for symbol in &symbols {
        tracker.add_import(&module, symbol, &importer);
    }

    for symbol in &symbols {
        let importers = tracker.get_symbol_importers(&module, symbol);
        assert_eq!(importers.len(), 1);
        assert!(importers.contains(&importer));
    }

    tracker.clear_imports_from(&importer);

    for symbol in &symbols {
        let importers = tracker.get_symbol_importers(&module, symbol);
        assert_eq!(importers.len(), 0);
    }
}

#[test]
fn test_selective_invalidation_with_imports() {
    let manager = CacheManager::new();
    let module = file_uri("src/models.py");
    let importer1 = file_uri("src/views.py");
    let importer2 = file_uri("src/tests.py");

    manager.record_import(&module, "User", &importer1);
    manager.record_import(&module, "User", &importer2);

    let importers_set = manager.get_symbol_importers(&module, "User");
    assert_eq!(importers_set.len(), 2);

    manager.invalidate_document(&importer1);

    let importers_after = manager.get_symbol_importers(&module, "User");
    assert_eq!(importers_after.len(), 1);
    assert!(importers_after.contains(&importer2));
}

#[test]
fn test_concurrent_symbol_tracking() {
    use std::sync::RwLock;
    use std::thread;

    let tracker = Arc::new(RwLock::new(ImportDependencyTracker::new()));
    let module = file_uri("src/shared.py");

    let mut handles = vec![];

    for i in 0..10 {
        let tracker_clone = Arc::clone(&tracker);
        let module_clone = module.clone();

        let handle = thread::spawn(move || {
            let importer = file_uri(&format!("src/thread_{i}.py"));
            tracker_clone
                .write()
                .unwrap()
                .add_import(&module_clone, "SharedClass", &importer);
        });

        handles.push(handle);
    }

    for handle in handles {
        handle.join().unwrap();
    }

    let importers = tracker.read().unwrap().get_symbol_importers(&module, "SharedClass");
    assert_eq!(importers.len(), 10);
}

#[test]
fn test_symbol_name_uniqueness_across_modules() {
    let mut tracker = ImportDependencyTracker::new();
    let module1 = file_uri("src/utils.py");
    let module2 = file_uri("src/helpers.py");
    let importer1 = file_uri("src/app.py");
    let importer2 = file_uri("src/main.py");

    tracker.add_import(&module1, "helper", &importer1);
    tracker.add_import(&module2, "helper", &importer2);

    let module1_importers = tracker.get_symbol_importers(&module1, "helper");
    let module2_importers = tracker.get_symbol_importers(&module2, "helper");

    assert_eq!(module1_importers.len(), 1);
    assert_eq!(module2_importers.len(), 1);
    assert!(module1_importers.contains(&importer1));
    assert!(module2_importers.contains(&importer2));
    assert!(!module1_importers.contains(&importer2));
    assert!(!module2_importers.contains(&importer1));
}

#[test]
fn test_incremental_import_updates() {
    let mut tracker = ImportDependencyTracker::new();
    let module = file_uri("src/api.py");
    let importer = file_uri("src/client.py");

    tracker.add_import(&module, "get", &importer);
    assert_eq!(tracker.get_symbol_importers(&module, "get").len(), 1);

    tracker.add_import(&module, "post", &importer);
    assert_eq!(tracker.get_symbol_importers(&module, "post").len(), 1);

    tracker.add_import(&module, "delete", &importer);
    assert_eq!(tracker.get_symbol_importers(&module, "delete").len(), 1);

    tracker.clear_imports_from(&importer);

    assert_eq!(tracker.get_symbol_importers(&module, "get").len(), 0);
    assert_eq!(tracker.get_symbol_importers(&module, "post").len(), 0);
    assert_eq!(tracker.get_symbol_importers(&module, "delete").len(), 0);
}

#[test]
fn test_cache_manager_clear_all_imports() {
    let manager = CacheManager::new();
    let module1 = file_uri("src/models.py");
    let module2 = file_uri("src/views.py");
    let importer1 = file_uri("src/app.py");
    let importer2 = file_uri("src/main.py");

    manager.record_import(&module1, "User", &importer1);
    manager.record_import(&module2, "UserView", &importer2);

    assert_eq!(manager.get_symbol_importers(&module1, "User").len(), 1);
    assert_eq!(manager.get_symbol_importers(&module2, "UserView").len(), 1);

    manager.clear_all();

    assert_eq!(manager.get_symbol_importers(&module1, "User").len(), 0);
    assert_eq!(manager.get_symbol_importers(&module2, "UserView").len(), 0);
}

#[test]
fn test_mixed_import_operations() {
    let mut tracker = ImportDependencyTracker::new();
    let base_module = file_uri("src/base.py");
    let util_module = file_uri("src/utils.py");
    let app = file_uri("src/app.py");
    let test = file_uri("src/test.py");

    tracker.add_import(&base_module, "BaseClass", &app);
    tracker.add_import(&util_module, "helper", &app);
    tracker.add_import(&base_module, "BaseClass", &test);

    assert_eq!(tracker.get_symbol_importers(&base_module, "BaseClass").len(), 2);
    assert_eq!(tracker.get_symbol_importers(&util_module, "helper").len(), 1);

    tracker.clear_imports_from(&app);

    assert_eq!(tracker.get_symbol_importers(&base_module, "BaseClass").len(), 1);
    assert_eq!(tracker.get_symbol_importers(&util_module, "helper").len(), 0);

    tracker.add_import(&base_module, "NewClass", &app);
    tracker.add_import(&util_module, "calculate", &app);

    assert_eq!(tracker.get_symbol_importers(&base_module, "NewClass").len(), 1);
    assert_eq!(tracker.get_symbol_importers(&util_module, "calculate").len(), 1);
}

#[test]
fn test_star_import_expansion_with_all() {
    let mut tracker = ImportDependencyTracker::new();
    let module = file_uri("src/utils.py");
    let importer = file_uri("src/app.py");

    tracker.add_import(&module, "func1", &importer);
    tracker.add_import(&module, "Class1", &importer);

    assert_eq!(tracker.get_symbol_importers(&module, "func1").len(), 1);
    assert_eq!(tracker.get_symbol_importers(&module, "Class1").len(), 1);
    assert_eq!(tracker.get_symbol_importers(&module, "_private").len(), 0);
}

#[test]
fn test_star_import_expansion_without_all() {
    let mut tracker = ImportDependencyTracker::new();
    let module = file_uri("src/helpers.py");
    let importer = file_uri("src/main.py");

    tracker.add_import(&module, "public_func", &importer);
    tracker.add_import(&module, "PublicClass", &importer);
    tracker.add_import(&module, "PUBLIC_VAR", &importer);

    assert_eq!(tracker.get_symbol_importers(&module, "public_func").len(), 1);
    assert_eq!(tracker.get_symbol_importers(&module, "PublicClass").len(), 1);
    assert_eq!(tracker.get_symbol_importers(&module, "PUBLIC_VAR").len(), 1);
    assert_eq!(tracker.get_symbol_importers(&module, "_private_func").len(), 0);
}

#[test]
fn test_multiple_files_star_import_same_module() {
    let mut tracker = ImportDependencyTracker::new();
    let module = file_uri("src/constants.py");
    let importer1 = file_uri("src/app.py");
    let importer2 = file_uri("src/tests.py");
    let importer3 = file_uri("src/cli.py");

    for symbol in ["MAX_SIZE", "DEFAULT_TIMEOUT", "VERSION"] {
        tracker.add_import(&module, symbol, &importer1);
        tracker.add_import(&module, symbol, &importer2);
        tracker.add_import(&module, symbol, &importer3);
    }

    assert_eq!(tracker.get_symbol_importers(&module, "MAX_SIZE").len(), 3);
    assert_eq!(tracker.get_symbol_importers(&module, "DEFAULT_TIMEOUT").len(), 3);
    assert_eq!(tracker.get_symbol_importers(&module, "VERSION").len(), 3);
}

#[test]
fn test_star_import_invalidation() {
    let mut tracker = ImportDependencyTracker::new();
    let module = file_uri("src/utils.py");
    let importer = file_uri("src/app.py");

    tracker.add_import(&module, "func1", &importer);
    tracker.add_import(&module, "func2", &importer);

    assert_eq!(tracker.get_symbol_importers(&module, "func1").len(), 1);
    assert_eq!(tracker.get_symbol_importers(&module, "func2").len(), 1);

    tracker.clear_imports_from(&importer);

    assert_eq!(tracker.get_symbol_importers(&module, "func1").len(), 0);
    assert_eq!(tracker.get_symbol_importers(&module, "func2").len(), 0);

    tracker.add_import(&module, "func1", &importer);
    tracker.add_import(&module, "func3", &importer);

    assert_eq!(tracker.get_symbol_importers(&module, "func1").len(), 1);
    assert_eq!(tracker.get_symbol_importers(&module, "func2").len(), 0);
    assert_eq!(tracker.get_symbol_importers(&module, "func3").len(), 1);
}
