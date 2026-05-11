use super::cache::StubFile;
use super::class_registry::extract_stub_classes_into_registry;
use super::typevars::{StubTypeContext, collect_stub_type_vars};

use beacon_core::{
    ClassRegistry, TypeVarConstraintRegistry,
    errors::{AnalysisError, Result},
};
use once_cell::sync::Lazy;

static STDLIB_REGISTRIES: Lazy<(ClassRegistry, TypeVarConstraintRegistry)> = Lazy::new(|| {
    let mut class_registry = ClassRegistry::new();
    let mut typevar_registry = TypeVarConstraintRegistry::new();

    for module_name in crate::EMBEDDED_STDLIB_MODULES.iter().copied() {
        if let Some(stub) = crate::get_embedded_stub(module_name)
            && let Err(e) = load_stub_into_registry(&stub, &mut class_registry, &mut typevar_registry)
        {
            eprintln!("Warning: Failed to load stdlib stub '{module_name}': {e:?}");
        }
    }

    (class_registry, typevar_registry)
});

/// Create a new ClassRegistry pre-populated with stdlib classes
pub fn new_class_registry_with_stdlib() -> ClassRegistry {
    STDLIB_REGISTRIES.0.clone()
}

/// Create a new TypeVarConstraintRegistry pre-populated with stdlib type variables
pub fn new_typevar_registry_with_stdlib() -> TypeVarConstraintRegistry {
    STDLIB_REGISTRIES.1.clone()
}

pub fn load_stub_into_registry(
    stub: &StubFile, class_registry: &mut ClassRegistry, typevar_registry: &mut TypeVarConstraintRegistry,
) -> Result<()> {
    let content = match &stub.content {
        Some(embedded_content) => embedded_content.clone(),
        None => std::fs::read_to_string(&stub.path).map_err(AnalysisError::from)?,
    };

    let mut parser = beacon_parser::PythonParser::new()?;
    let (ast, _symbol_table) = parser.parse_and_resolve(&content)?;
    let mut ctx = StubTypeContext::new();
    collect_stub_type_vars(&ast, &mut ctx);

    for type_var_name in ctx.type_vars.clone() {
        let type_var = ctx.get_or_create_type_var(&type_var_name);

        if let Some(bound) = ctx.get_bound(&type_var_name) {
            typevar_registry.set_bound(type_var.id, bound.clone());
        }
        if let Some(constraints) = ctx.get_constraints(&type_var_name) {
            typevar_registry.set_constraints(type_var.id, constraints.clone());
        }
    }

    extract_stub_classes_into_registry(&ast, class_registry, &mut ctx);
    Ok(())
}
