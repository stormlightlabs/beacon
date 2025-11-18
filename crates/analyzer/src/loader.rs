use beacon_core::{AnnotationParser, TypeCtor, TypeVar, TypeVarGen, Variance};
use beacon_core::{
    ClassMetadata, ClassRegistry, MethodType, Type,
    errors::{AnalysisError, Result},
};
use beacon_parser::AstNode;
use rustc_hash::{FxHashMap, FxHashSet};
use std::collections::HashMap;
use std::path::PathBuf;

/// Cache for parsed stub files
#[derive(Default)]
pub struct StubCache {
    cache: FxHashMap<String, StubFile>,
}

impl StubCache {
    pub fn new() -> Self {
        Self::default()
    }

    /// Get a stub from the cache
    pub fn get(&self, module_name: &str) -> Option<&StubFile> {
        self.cache.get(module_name)
    }

    /// Insert a stub into the cache
    pub fn insert(&mut self, module_name: String, stub: StubFile) {
        self.cache.insert(module_name, stub);
    }

    /// Check if a stub exists in the cache
    pub fn contains(&self, module_name: &str) -> bool {
        self.cache.contains_key(module_name)
    }

    /// Get the number of stubs in the cache
    pub fn len(&self) -> usize {
        self.cache.len()
    }

    /// Check if the cache is empty
    pub fn is_empty(&self) -> bool {
        self.cache.is_empty()
    }
}

/// Parsed stub file (.pyi)
#[derive(Debug, Clone)]
pub struct StubFile {
    /// Module name
    pub module: String,
    /// File path to the stub
    pub path: PathBuf,
    /// Exported symbols and their types
    pub exports: FxHashMap<String, Type>,
    /// Whether this is a partial stub
    pub is_partial: bool,
    /// Re-exported modules (from X import Y as Y)
    pub reexports: Vec<String>,
    /// __all__ declaration if present
    pub all_exports: Option<Vec<String>>,
    /// Embedded content for built-in stubs (avoids filesystem access)
    pub content: Option<String>,
}

#[derive(Debug)]
pub(crate) struct MethodInfo {
    pub params: Vec<(String, Type)>,
    pub return_type: Type,
    pub decorators: Vec<String>,
    pub is_overload: bool,
}

#[derive(Debug)]
pub struct StubTypeContext {
    type_vars: FxHashSet<String>,
    var_map: HashMap<String, TypeVar>,
    var_gen: TypeVarGen,
    /// Maps TypeVar names to their variance (covariant, contravariant, invariant)
    var_variance: HashMap<String, Variance>,
    /// Maps TypeVar names to their bound types (if any)
    var_bounds: HashMap<String, Type>,
    /// Maps TypeVar names to their constraint types (if any)
    var_constraints: HashMap<String, Vec<Type>>,
}

impl StubTypeContext {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn register_type_var(&mut self, name: &str, variance: Variance, bound: Option<Type>, constraints: Vec<Type>) {
        self.type_vars.insert(name.to_string());
        self.var_variance.insert(name.to_string(), variance);
        if let Some(bound_ty) = bound {
            self.var_bounds.insert(name.to_string(), bound_ty);
        }
        if !constraints.is_empty() {
            self.var_constraints.insert(name.to_string(), constraints);
        }
    }

    pub fn is_type_var(&self, name: &str) -> bool {
        self.type_vars.contains(name)
    }

    pub fn get_or_create_type_var(&mut self, name: &str) -> TypeVar {
        if let Some(tv) = self.var_map.get(name) {
            tv.clone()
        } else {
            let variance = self.var_variance.get(name).copied().unwrap_or(Variance::Invariant);
            let tv = TypeVar::with_variance(self.var_gen.fresh().id, Some(name.to_string()), variance);
            self.var_map.insert(name.to_string(), tv.clone());
            tv
        }
    }

    pub fn get_bound(&self, name: &str) -> Option<&Type> {
        self.var_bounds.get(name)
    }

    pub fn get_constraints(&self, name: &str) -> Option<&Vec<Type>> {
        self.var_constraints.get(name)
    }
}

impl Default for StubTypeContext {
    fn default() -> Self {
        Self {
            type_vars: FxHashSet::default(),
            var_map: HashMap::default(),
            var_gen: TypeVarGen::new(),
            var_variance: HashMap::default(),
            var_bounds: HashMap::default(),
            var_constraints: HashMap::default(),
        }
    }
}

pub fn collect_stub_type_vars(node: &AstNode, ctx: &mut StubTypeContext) {
    match node {
        AstNode::Module { body, .. } => {
            for stmt in body {
                collect_stub_type_vars(stmt, ctx);
            }
        }
        AstNode::Assignment { target, value, .. } => {
            if let AstNode::Call { function, args, keywords, .. } = value.as_ref() {
                let function_name = function.function_to_string();
                if function_name == "TypeVar" || function_name.ends_with(".TypeVar") {
                    let target_str = target.target_to_string();
                    let (variance, bound, constraints) = extract_typevar_metadata_from_stub(args, keywords, ctx);
                    ctx.register_type_var(&target_str, variance, bound, constraints);
                }
            }
        }
        AstNode::ClassDef { body, .. } => {
            for stmt in body {
                collect_stub_type_vars(stmt, ctx);
            }
        }
        _ => {}
    }
}

/// Extract variance, bounds, and constraints from TypeVar calls in stub files.
///
/// Parses:
/// - Variance: `covariant=True` and `contravariant=True` keyword arguments
/// - Bound: `bound=Animal` keyword argument
/// - Constraints: Positional arguments after the name: `TypeVar('T', int, str)`
///
/// Examples:
/// - `TypeVar('T')` → (Invariant, None, [])
/// - `TypeVar('T_Co', covariant=True)` → (Covariant, None, [])
/// - `TypeVar('T', bound=Animal)` → (Invariant, Some(Animal), [])
/// - `TypeVar('T', int, str)` → (Invariant, None, [int, str])
fn extract_typevar_metadata_from_stub(
    args: &[AstNode], keywords: &[(String, AstNode)], ctx: &mut StubTypeContext,
) -> (Variance, Option<Type>, Vec<Type>) {
    let mut covariant = false;
    let mut contravariant = false;
    let mut bound = None;

    for (key, value) in keywords {
        match key.as_str() {
            "covariant" => {
                covariant = is_true_literal(value);
            }
            "contravariant" => {
                contravariant = is_true_literal(value);
            }
            "bound" => {
                bound = parse_typevar_bound_from_stub(value, ctx);
            }
            _ => {}
        }
    }

    let variance = match (covariant, contravariant) {
        (true, false) => Variance::Covariant,
        (false, true) => Variance::Contravariant,
        _ => Variance::Invariant,
    };

    let mut constraints = Vec::new();
    if args.len() > 1 {
        for constraint_node in &args[1..] {
            if let Some(constraint_ty) = parse_typevar_constraint_from_stub(constraint_node, ctx) {
                constraints.push(constraint_ty);
            }
        }
    }

    (variance, bound, constraints)
}

/// Parse a TypeVar bound from a stub AST node
fn parse_typevar_bound_from_stub(node: &AstNode, ctx: &mut StubTypeContext) -> Option<Type> {
    match node {
        AstNode::Identifier { name, .. } => parse_type_annotation(name, ctx),
        AstNode::Subscript { value, slice, .. } => {
            let base_name = match value.as_ref() {
                AstNode::Identifier { name, .. } => name.as_str(),
                _ => return None,
            };

            let slice_str = node_to_annotation_string(slice);
            let full_annotation = format!("{base_name}[{slice_str}]");
            parse_type_annotation(&full_annotation, ctx)
        }
        _ => None,
    }
}

/// Parse a TypeVar constraint from a stub AST node
fn parse_typevar_constraint_from_stub(node: &AstNode, ctx: &mut StubTypeContext) -> Option<Type> {
    match node {
        AstNode::Identifier { name, .. } => parse_type_annotation(name, ctx),
        AstNode::Subscript { value, slice, .. } => {
            let base_name = match value.as_ref() {
                AstNode::Identifier { name, .. } => name.as_str(),
                _ => return None,
            };

            let slice_str = node_to_annotation_string(slice);
            let full_annotation = format!("{base_name}[{slice_str}]");
            parse_type_annotation(&full_annotation, ctx)
        }
        _ => None,
    }
}

/// Convert an AST node to an annotation string (for complex type expressions)
fn node_to_annotation_string(node: &AstNode) -> String {
    match node {
        AstNode::Identifier { name, .. } => name.clone(),
        AstNode::Subscript { value, slice, .. } => {
            let base = node_to_annotation_string(value);
            let slice_str = node_to_annotation_string(slice);
            format!("{base}[{slice_str}]")
        }
        AstNode::Tuple { elements, .. } => {
            let parts: Vec<String> = elements.iter().map(node_to_annotation_string).collect();
            parts.join(", ")
        }
        _ => "Any".to_string(),
    }
}

/// Check if an AST node is a True literal
fn is_true_literal(node: &AstNode) -> bool {
    matches!(
        node,
        AstNode::Literal { value: beacon_parser::LiteralValue::Boolean(true), .. }
    )
}

fn collect_type_vars_from_type(ty: &Type, acc: &mut FxHashSet<String>) {
    match ty {
        Type::Con(TypeCtor::TypeVariable(name)) => {
            acc.insert(name.clone());
        }
        Type::Var(tv) => {
            if let Some(hint) = &tv.hint {
                acc.insert(hint.clone());
            }
        }
        Type::App(t1, t2) => {
            collect_type_vars_from_type(t1, acc);
            collect_type_vars_from_type(t2, acc);
        }
        Type::Fun(params, ret) => {
            for param in params {
                collect_type_vars_from_type(&param.1, acc);
            }
            collect_type_vars_from_type(ret, acc);
        }
        Type::ForAll(_, inner) => collect_type_vars_from_type(inner, acc),
        Type::Union(types) | Type::Intersection(types) => {
            for ty in types {
                collect_type_vars_from_type(ty, acc);
            }
        }
        Type::Record(fields, _) => {
            for (_, field_ty) in fields {
                collect_type_vars_from_type(field_ty, acc);
            }
        }
        Type::BoundMethod(receiver, _, method) => {
            collect_type_vars_from_type(receiver, acc);
            collect_type_vars_from_type(method, acc);
        }
        _ => {}
    }
}

fn collect_type_vars_from_metadata(metadata: &ClassMetadata) -> FxHashSet<String> {
    let mut vars = FxHashSet::default();

    for ty in metadata.fields.values() {
        collect_type_vars_from_type(ty, &mut vars);
    }

    for ty in metadata.properties.values() {
        collect_type_vars_from_type(ty, &mut vars);
    }

    for ty in metadata.classmethods.values() {
        collect_type_vars_from_type(ty, &mut vars);
    }

    for ty in metadata.staticmethods.values() {
        collect_type_vars_from_type(ty, &mut vars);
    }

    for method in metadata.methods.values() {
        match method {
            MethodType::Single(ty) => collect_type_vars_from_type(ty, &mut vars),
            MethodType::Overloaded(overload_set) => {
                for sig in &overload_set.signatures {
                    collect_type_vars_from_type(sig, &mut vars);
                }
                if let Some(implementation) = &overload_set.implementation {
                    collect_type_vars_from_type(implementation, &mut vars);
                }
            }
        }
    }

    if let Some(init_ty) = &metadata.init_type {
        collect_type_vars_from_type(init_ty, &mut vars);
    }

    if let Some(new_ty) = &metadata.new_type {
        collect_type_vars_from_type(new_ty, &mut vars);
    }

    vars
}

/// Extract class metadata from stub AST and register them in the ClassRegistry
pub fn extract_stub_classes_into_registry(
    node: &AstNode, class_registry: &mut ClassRegistry, ctx: &mut StubTypeContext,
    base_map: &std::collections::HashMap<String, Vec<String>>,
) {
    match node {
        AstNode::Module { body, .. } => {
            for stmt in body {
                extract_stub_classes_into_registry(stmt, class_registry, ctx, base_map);
            }
        }
        AstNode::ClassDef { name, bases, metaclass, body, .. } => {
            let mut metadata = ClassMetadata::new(name.clone());
            let resolved_bases =
                if bases.is_empty() { base_map.get(name).cloned().unwrap_or_default() } else { bases.clone() };

            let is_protocol = resolved_bases.iter().any(|base| {
                base == "Protocol"
                    || base.starts_with("Protocol[")
                    || base.ends_with(".Protocol")
                    || base == "typing.Protocol"
                    || base.starts_with("typing.Protocol[")
            });
            metadata.set_protocol(is_protocol);

            for base in &resolved_bases {
                metadata.add_base_class(base.clone());

                if let Some(params_str) = extract_generic_params(base) {
                    let type_params: Vec<String> = params_str.split(',').map(|s| s.trim().to_string()).collect();
                    for param in &type_params {
                        ctx.register_type_var(param, Variance::Invariant, None, vec![]);
                    }

                    let is_generic_base = base.starts_with("Generic[") && base.ends_with(']');
                    if is_generic_base || metadata.type_params.is_empty() {
                        metadata.set_type_params(type_params);
                    }
                }
            }

            if let Some(meta) = metaclass {
                metadata.set_metaclass(meta.clone());
            }

            process_class_methods(body, &mut metadata, ctx);

            if metadata.type_params.is_empty() {
                let inferred = collect_type_vars_from_metadata(&metadata);
                if !inferred.is_empty() {
                    let mut params: Vec<String> = inferred.into_iter().collect();
                    params.sort();
                    metadata.set_type_params(params.clone());
                    for param in params {
                        ctx.register_type_var(&param, Variance::Invariant, None, vec![]);
                    }
                }
            }

            if !metadata.type_params.is_empty() {
                let vars: Vec<TypeVar> = metadata
                    .type_params
                    .iter()
                    .map(|param| ctx.get_or_create_type_var(param))
                    .collect();
                metadata.set_type_param_vars(vars);
            }

            class_registry.register_class(name.clone(), metadata);
        }
        _ => {}
    }
}

/// Process class methods, grouping overloads together
///
/// Handles @overload decorators by collecting multiple signatures for the same method name.
/// Functions with @overload are signature declarations only; the function without @overload
/// (if present) becomes the implementation signature.
pub fn process_class_methods(body: &[AstNode], metadata: &mut ClassMetadata, ctx: &mut StubTypeContext) {
    let methods_by_name = collect_methods(body, metadata, ctx);

    for (method_name, method_infos) in methods_by_name {
        handle_method_group(method_name, method_infos, metadata);
    }
}

fn collect_methods(
    body: &[AstNode], metadata: &mut ClassMetadata, ctx: &mut StubTypeContext,
) -> HashMap<String, Vec<MethodInfo>> {
    let mut methods_by_name: HashMap<String, Vec<MethodInfo>> = HashMap::new();

    for stmt in body {
        match stmt {
            AstNode::AnnotatedAssignment { target, type_annotation, .. } => {
                add_field_from_annotation(metadata, ctx, target, type_annotation);
            }
            AstNode::FunctionDef { name, args, return_type, decorators, .. } => {
                let info = build_method_info(args, return_type, decorators, ctx);
                methods_by_name.entry(name.clone()).or_default().push(info);
            }
            _ => {}
        }
    }

    methods_by_name
}

fn add_field_from_annotation(
    metadata: &mut ClassMetadata, ctx: &mut StubTypeContext, target: &AstNode, type_annotation: &str,
) {
    if let Some(attr_type) = parse_type_annotation(type_annotation, ctx) {
        metadata.add_field(target.target_to_string(), attr_type);
    }
}

fn build_method_info(
    params: &[beacon_parser::Parameter], return_type: &Option<String>, decorators: &[String], ctx: &mut StubTypeContext,
) -> MethodInfo {
    let mut param_list: Vec<(String, Type)> = Vec::new();
    let has_self = params.first().map(|p| p.name == "self").unwrap_or(false);
    let has_cls = params.first().map(|p| p.name == "cls").unwrap_or(false);

    if has_self {
        param_list.push(("self".to_string(), Type::any()));
    } else if has_cls {
        param_list.push(("cls".to_string(), Type::any()));
    }

    let start_idx = if has_self || has_cls { 1 } else { 0 };
    for param in params.iter().skip(start_idx) {
        if let Some(ann) = &param.type_annotation {
            if let Some(ty) = parse_type_annotation(ann, ctx) {
                param_list.push((param.name.clone(), ty));
            }
        }
    }

    let ret_type = return_type
        .as_ref()
        .and_then(|ann| parse_type_annotation(ann, ctx))
        .unwrap_or_else(Type::any);
    let is_overload = decorators.iter().any(|d| d == "overload");

    MethodInfo { params: param_list, return_type: ret_type, decorators: decorators.to_vec(), is_overload }
}

fn handle_method_group(method_name: String, method_infos: Vec<MethodInfo>, metadata: &mut ClassMetadata) {
    if method_infos.is_empty() {
        return;
    }

    if handle_special_method(&method_name, &method_infos, metadata) {
        return;
    }

    let decorator_flags = MethodDecoratorFlags::from(&method_infos[0].decorators);

    if decorator_flags.property {
        metadata.add_property(method_name, method_infos[0].return_type.clone());
        return;
    }

    let has_overloads = method_infos.iter().any(|info| info.is_overload) && method_infos.len() > 1;

    if has_overloads {
        handle_overloaded_method(method_name, method_infos, decorator_flags, metadata);
    } else {
        register_non_overloaded_method(method_name, &method_infos[0], decorator_flags, metadata);
    }
}

fn handle_special_method(method_name: &str, method_infos: &[MethodInfo], metadata: &mut ClassMetadata) -> bool {
    if method_name == "__init__" {
        if let Some(info) = method_infos.first() {
            let func_type = Type::fun(info.params.clone(), info.return_type.clone());
            metadata.set_init_type(func_type);
        }
        return true;
    }

    if method_name == "__new__" {
        if let Some(info) = method_infos.first() {
            let func_type = Type::fun(info.params.clone(), info.return_type.clone());
            metadata.set_new_type(func_type);
        }
        return true;
    }

    false
}

fn handle_overloaded_method(
    method_name: String, method_infos: Vec<MethodInfo>, decorator_flags: MethodDecoratorFlags,
    metadata: &mut ClassMetadata,
) {
    let overload_set = build_overload_set(method_infos, decorator_flags.staticmethod);

    if decorator_flags.staticmethod {
        if let Some(impl_type) = overload_set.implementation.clone() {
            metadata.add_staticmethod(method_name, impl_type);
        } else if let Some(first_sig) = overload_set.signatures.first() {
            metadata.add_staticmethod(method_name, first_sig.clone());
        }
    } else if decorator_flags.classmethod {
        if let Some(impl_type) = overload_set.implementation.clone() {
            metadata.add_classmethod(method_name, impl_type);
        } else if let Some(first_sig) = overload_set.signatures.first() {
            metadata.add_classmethod(method_name, first_sig.clone());
        }
    } else {
        metadata.add_overloaded_method(method_name, overload_set);
    }
}

fn build_overload_set(method_infos: Vec<MethodInfo>, treat_as_static: bool) -> beacon_core::OverloadSet {
    let mut overload_sigs = Vec::new();
    let mut implementation = None;

    for info in method_infos {
        let params = adjust_params_for_static(&info.params, treat_as_static);
        let func_type = Type::fun(params, info.return_type.clone());

        if info.is_overload {
            overload_sigs.push(func_type);
        } else {
            implementation = Some(func_type);
        }
    }

    beacon_core::OverloadSet { signatures: overload_sigs, implementation }
}

fn register_non_overloaded_method(
    method_name: String, method_info: &MethodInfo, decorator_flags: MethodDecoratorFlags, metadata: &mut ClassMetadata,
) {
    let params = adjust_params_for_static(&method_info.params, decorator_flags.staticmethod);
    let func_type = Type::fun(params, method_info.return_type.clone());

    if decorator_flags.staticmethod {
        metadata.add_staticmethod(method_name, func_type);
    } else if decorator_flags.classmethod {
        metadata.add_classmethod(method_name, func_type);
    } else {
        metadata.add_method(method_name, func_type);
    }
}

fn adjust_params_for_static(params: &[(String, Type)], is_static: bool) -> Vec<(String, Type)> {
    if is_static && !params.is_empty() {
        params.iter().skip(1).cloned().collect()
    } else {
        params.to_vec()
    }
}

#[derive(Clone, Copy)]
struct MethodDecoratorFlags {
    property: bool,
    staticmethod: bool,
    classmethod: bool,
}

impl MethodDecoratorFlags {
    fn from(decorators: &[String]) -> Self {
        Self {
            property: decorators.iter().any(|d| d == "property"),
            staticmethod: decorators.iter().any(|d| d == "staticmethod"),
            classmethod: decorators.iter().any(|d| d == "classmethod"),
        }
    }
}

/// Extract the content between brackets in a generic type annotation
/// e.g., "list[str]" → Some("str"), "dict[str, int]" → Some("str, int")
fn extract_generic_params(annotation: &str) -> Option<String> {
    let start = annotation.find('[')?;
    let end = annotation.rfind(']')?;
    if end > start { Some(annotation[start + 1..end].to_string()) } else { None }
}

/// Parse comma-separated generic parameters, respecting nested brackets
/// e.g., "str, int" → [Type::string(), Type::int()]
/// e.g., "dict[str, int], list[str]" → [Type::App(...), Type::App(...)]
fn parse_generic_params(params_str: &str, ctx: &mut StubTypeContext) -> Vec<beacon_core::Type> {
    let mut types = Vec::new();
    let mut current = String::new();
    let mut bracket_depth = 0;

    for ch in params_str.chars() {
        match ch {
            '[' => {
                bracket_depth += 1;
                current.push(ch);
            }
            ']' => {
                bracket_depth -= 1;
                current.push(ch);
            }
            ',' if bracket_depth == 0 => {
                if let Some(ty) = parse_type_annotation(current.trim(), ctx) {
                    types.push(ty);
                }
                current.clear();
            }
            _ => {
                current.push(ch);
            }
        }
    }

    if !current.is_empty() {
        if let Some(ty) = parse_type_annotation(current.trim(), ctx) {
            types.push(ty);
        }
    }

    types
}

/// Parse a type annotation string into a Type
/// This is a simplified version for stub parsing
pub fn parse_type_annotation(annotation: &str, ctx: &mut StubTypeContext) -> Option<beacon_core::Type> {
    let annotation = annotation.trim();

    if ctx.is_type_var(annotation) {
        return Some(beacon_core::Type::Con(TypeCtor::TypeVariable(annotation.to_string())));
    }

    let parser = AnnotationParser::new();
    if let Ok(parsed) = parser.parse(annotation) {
        return Some(convert_type_vars(parsed, ctx));
    }

    if annotation.contains('|') {
        let types = annotation
            .split('|')
            .map(|s| s.trim())
            .filter_map(|p| parse_type_annotation(p, ctx))
            .collect();
        return Some(Type::Union(types));
    }

    if let Some(idx) = annotation.find('[') {
        let base = &annotation[..idx];

        if let Some(params_str) = extract_generic_params(annotation) {
            let type_params = parse_generic_params(&params_str, ctx);

            if type_params.is_empty() {
                let base_type = match base {
                    "list" => beacon_core::Type::Con(TypeCtor::List),
                    "dict" => beacon_core::Type::Con(TypeCtor::Dict),
                    "set" => beacon_core::Type::Con(TypeCtor::Set),
                    "tuple" => beacon_core::Type::Con(TypeCtor::Tuple),
                    _ => beacon_core::Type::Con(TypeCtor::Class(base.to_string())),
                };
                return Some(base_type);
            }

            match base {
                "list" | "set" | "tuple" => {
                    let elem_ty = type_params.first()?;
                    let ctor = match base {
                        "list" => TypeCtor::List,
                        "set" => TypeCtor::Set,
                        "tuple" => TypeCtor::Tuple,
                        _ => unreachable!(),
                    };
                    return Some(beacon_core::Type::App(
                        Box::new(beacon_core::Type::Con(ctor)),
                        Box::new(elem_ty.clone()),
                    ));
                }
                "dict" => {
                    if type_params.len() >= 2 {
                        let key_ty = &type_params[0];
                        let val_ty = &type_params[1];
                        return Some(beacon_core::Type::App(
                            Box::new(beacon_core::Type::App(
                                Box::new(beacon_core::Type::Con(TypeCtor::Dict)),
                                Box::new(key_ty.clone()),
                            )),
                            Box::new(val_ty.clone()),
                        ));
                    } else {
                        let key_ty = &type_params[0];
                        return Some(beacon_core::Type::App(
                            Box::new(beacon_core::Type::App(
                                Box::new(beacon_core::Type::Con(TypeCtor::Dict)),
                                Box::new(key_ty.clone()),
                            )),
                            Box::new(beacon_core::Type::any()),
                        ));
                    }
                }
                _ => {
                    let mut result = beacon_core::Type::Con(TypeCtor::Class(base.to_string()));
                    for param in type_params {
                        result = beacon_core::Type::App(Box::new(result), Box::new(param));
                    }
                    return Some(result);
                }
            }
        }
    }

    match annotation {
        "int" => Some(beacon_core::Type::Con(TypeCtor::Int)),
        "float" => Some(beacon_core::Type::Con(TypeCtor::Float)),
        "str" => Some(beacon_core::Type::Con(TypeCtor::String)),
        "bool" => Some(beacon_core::Type::Con(TypeCtor::Bool)),
        "None" => Some(beacon_core::Type::Con(TypeCtor::NoneType)),
        "NoneType" => Some(beacon_core::Type::Con(TypeCtor::NoneType)),
        _ => Some(beacon_core::Type::Con(TypeCtor::Class(annotation.to_string()))),
    }
}

fn convert_type_vars(ty: beacon_core::Type, ctx: &mut StubTypeContext) -> beacon_core::Type {
    match ty {
        beacon_core::Type::Con(TypeCtor::Class(name)) if ctx.is_type_var(&name) => {
            beacon_core::Type::Var(ctx.get_or_create_type_var(&name))
        }
        beacon_core::Type::App(base, arg) => beacon_core::Type::App(
            Box::new(convert_type_vars(*base, ctx)),
            Box::new(convert_type_vars(*arg, ctx)),
        ),
        beacon_core::Type::Fun(params, ret) => beacon_core::Type::Fun(
            params.into_iter().map(|p| (p.0, convert_type_vars(p.1, ctx))).collect(),
            Box::new(convert_type_vars(*ret, ctx)),
        ),
        beacon_core::Type::Union(types) => {
            beacon_core::Type::union(types.into_iter().map(|t| convert_type_vars(t, ctx)).collect())
        }
        beacon_core::Type::Intersection(types) => {
            beacon_core::Type::intersection(types.into_iter().map(|t| convert_type_vars(t, ctx)).collect())
        }
        beacon_core::Type::ForAll(vars, inner) => {
            beacon_core::Type::ForAll(vars, Box::new(convert_type_vars(*inner, ctx)))
        }
        beacon_core::Type::Record(fields, row) => beacon_core::Type::Record(
            fields
                .into_iter()
                .map(|(name, t)| (name, convert_type_vars(t, ctx)))
                .collect(),
            row,
        ),
        other => other,
    }
}

pub fn load_stub_into_registry(
    stub: &StubFile, class_registry: &mut ClassRegistry, typevar_registry: &mut beacon_core::TypeVarConstraintRegistry,
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

    let base_map = parse_class_bases(&content);
    extract_stub_classes_into_registry(&ast, class_registry, &mut ctx, &base_map);
    Ok(())
}

fn parse_class_bases(source: &str) -> std::collections::HashMap<String, Vec<String>> {
    let mut map = std::collections::HashMap::new();

    for line in source.lines() {
        let trimmed = line.trim_start();
        if trimmed.starts_with("class ") {
            if let Some(rest) = trimmed.strip_prefix("class ") {
                if let Some((name_part, _)) = rest.split_once(':') {
                    let name_part = name_part.trim();
                    if let Some(idx) = name_part.find('(') {
                        let name = name_part[..idx].trim();
                        if let Some(end_idx) = name_part.rfind(')') {
                            let bases_str = &name_part[idx + 1..end_idx];
                            let bases: Vec<String> = bases_str
                                .split(',')
                                .map(|s| s.trim().to_string())
                                .filter(|s| !s.is_empty())
                                .collect();
                            map.insert(name.to_string(), bases);
                        }
                    }
                }
            }
        }
    }

    map
}

#[cfg(test)]
mod tests {
    use super::StubFile;
    use super::*;

    use beacon_core::{ClassRegistry, MethodType, Type, TypeCtor};
    use rustc_hash::FxHashMap;
    use std::path::PathBuf;

    fn new_ctx() -> StubTypeContext {
        StubTypeContext::new()
    }

    #[test]
    fn test_parse_list_with_generic_param() {
        let mut ctx = new_ctx();
        let result = parse_type_annotation("list[str]", &mut ctx).unwrap();

        match &result {
            Type::App(ctor, elem) => {
                assert!(matches!(**ctor, Type::Con(TypeCtor::List)));
                assert!(matches!(**elem, Type::Con(TypeCtor::String)));
            }
            _ => panic!("Expected Type::App for list[str], got {result:?}"),
        }
    }

    #[test]
    fn test_parse_dict_with_generic_params() {
        let mut ctx = new_ctx();
        let result = parse_type_annotation("dict[str, int]", &mut ctx).unwrap();

        match &result {
            Type::App(outer, val_ty) => {
                assert!(matches!(**val_ty, Type::Con(TypeCtor::Int)));
                match &**outer {
                    Type::App(inner_ctor, key_ty) => {
                        assert!(matches!(**inner_ctor, Type::Con(TypeCtor::Dict)));
                        assert!(matches!(**key_ty, Type::Con(TypeCtor::String)));
                    }
                    _ => panic!("Expected nested Type::App for dict key type"),
                }
            }
            _ => panic!("Expected Type::App for dict[str, int], got {result:?}"),
        }
    }

    #[test]
    fn test_parse_set_with_generic_param() {
        let mut ctx = new_ctx();
        let result = parse_type_annotation("set[int]", &mut ctx).unwrap();

        match &result {
            Type::App(ctor, elem) => {
                assert!(matches!(**ctor, Type::Con(TypeCtor::Set)));
                assert!(matches!(**elem, Type::Con(TypeCtor::Int)));
            }
            _ => panic!("Expected Type::App for set[int], got {result:?}"),
        }
    }

    #[test]
    fn test_parse_tuple_with_generic_param() {
        let mut ctx = new_ctx();
        let result = parse_type_annotation("tuple[str]", &mut ctx).unwrap();

        match &result {
            Type::App(ctor, elem) => {
                assert!(matches!(**ctor, Type::Con(TypeCtor::Tuple)));
                assert!(matches!(**elem, Type::Con(TypeCtor::String)));
            }
            _ => panic!("Expected Type::App for tuple[str], got {result:?}"),
        }
    }

    #[test]
    fn test_parse_nested_generic() {
        let mut ctx = new_ctx();
        let result = parse_type_annotation("list[dict[str, int]]", &mut ctx).unwrap();

        match &result {
            Type::App(list_ctor, dict_ty) => {
                assert!(matches!(**list_ctor, Type::Con(TypeCtor::List)));

                match &**dict_ty {
                    Type::App(outer, val_ty) => {
                        assert!(matches!(**val_ty, Type::Con(TypeCtor::Int)));
                        match &**outer {
                            Type::App(dict_ctor, key_ty) => {
                                assert!(matches!(**dict_ctor, Type::Con(TypeCtor::Dict)));
                                assert!(matches!(**key_ty, Type::Con(TypeCtor::String)));
                            }
                            _ => panic!("Expected nested Type::App for dict"),
                        }
                    }
                    _ => panic!("Expected Type::App for dict element type"),
                }
            }
            _ => panic!("Expected Type::App for list[dict[str, int]], got {result:?}"),
        }
    }

    #[test]
    fn test_parse_union_with_generics() {
        let mut ctx = new_ctx();
        let result = parse_type_annotation("list[str] | None", &mut ctx).unwrap();

        match &result {
            Type::Union(types) => {
                assert_eq!(types.len(), 2);

                let has_list = types.iter().any(|t| {
                    matches!(
                        t,
                        Type::App(ctor, elem)
                            if matches!(**ctor, Type::Con(TypeCtor::List))
                                && matches!(**elem, Type::Con(TypeCtor::String))
                    )
                });
                let has_none = types.iter().any(|t| matches!(t, Type::Con(TypeCtor::NoneType)));

                assert!(has_list, "Expected list[str] in union");
                assert!(has_none, "Expected None in union");
            }
            _ => panic!("Expected Union type, got {result:?}"),
        }
    }

    #[test]
    fn test_extract_generic_params() {
        assert_eq!(extract_generic_params("list[str]"), Some("str".to_string()));
        assert_eq!(extract_generic_params("dict[str, int]"), Some("str, int".to_string()));
        assert_eq!(
            extract_generic_params("list[dict[str, int]]"),
            Some("dict[str, int]".to_string())
        );
        assert_eq!(extract_generic_params("list"), None);
    }

    #[test]
    fn test_parse_generic_params_simple() {
        let mut ctx = new_ctx();
        let params = parse_generic_params("str", &mut ctx);
        assert_eq!(params.len(), 1);
        assert!(matches!(params[0], Type::Con(TypeCtor::String)));
    }

    #[test]
    fn test_parse_generic_params_multiple() {
        let mut ctx = new_ctx();
        let params = parse_generic_params("str, int", &mut ctx);
        assert_eq!(params.len(), 2);
        assert!(matches!(params[0], Type::Con(TypeCtor::String)));
        assert!(matches!(params[1], Type::Con(TypeCtor::Int)));
    }

    #[test]
    fn test_parse_generic_params_nested() {
        let mut ctx = new_ctx();
        let params = parse_generic_params("dict[str, int], list[str]", &mut ctx);
        assert_eq!(params.len(), 2);

        match &params[0] {
            Type::App(outer, val_ty) => {
                assert!(matches!(**val_ty, Type::Con(TypeCtor::Int)));
                match &**outer {
                    Type::App(dict_ctor, key_ty) => {
                        assert!(matches!(**dict_ctor, Type::Con(TypeCtor::Dict)));
                        assert!(matches!(**key_ty, Type::Con(TypeCtor::String)));
                    }
                    _ => panic!("Expected nested Type::App for dict"),
                }
            }
            _ => panic!("Expected Type::App for dict parameter"),
        }

        match &params[1] {
            Type::App(ctor, elem) => {
                assert!(matches!(**ctor, Type::Con(TypeCtor::List)));
                assert!(matches!(**elem, Type::Con(TypeCtor::String)));
            }
            _ => panic!("Expected Type::App for list parameter"),
        }
    }

    #[test]
    fn test_stub_loading_list_methods() {
        use super::StubFile;
        use beacon_core::{ClassRegistry, MethodType};
        use rustc_hash::FxHashMap;
        use std::path::PathBuf;

        let stub_content = r#"
from typing import TypeVar, Generic

_T = TypeVar("_T")

class list(Generic[_T]):
    def append(self, item: _T) -> None: ...
    def pop(self) -> _T: ...
"#;

        let stub = StubFile {
            module: "builtins".to_string(),
            path: PathBuf::from("test.pyi"),
            exports: FxHashMap::default(),
            is_partial: false,
            reexports: Vec::new(),
            all_exports: None,
            content: Some(stub_content.to_string()),
        };

        let mut class_registry = ClassRegistry::new();
        let mut typevar_registry = beacon_core::TypeVarConstraintRegistry::new();
        load_stub_into_registry(&stub, &mut class_registry, &mut typevar_registry).unwrap();

        let list_class = class_registry.get_class("list");
        assert!(list_class.is_some(), "list class should be registered");

        let list_metadata = list_class.unwrap();

        let append_method = list_metadata.methods.get("append");
        assert!(append_method.is_some(), "append method should exist");

        if let Some(append_ty) = append_method {
            match append_ty {
                MethodType::Single(Type::Fun(params, _)) => {
                    assert_eq!(
                        params.len(),
                        2,
                        "append should have 2 parameters (self + item), got {}: {:?}",
                        params.len(),
                        params
                    );
                }
                _ => panic!("append should be a single function type, got {append_ty:?}"),
            }
        }

        let pop_method = list_metadata.methods.get("pop");
        assert!(pop_method.is_some(), "pop method should exist");
    }

    #[test]
    fn test_stub_loading_real_builtins() {
        use super::StubFile;
        use beacon_core::ClassRegistry;
        use rustc_hash::FxHashMap;
        use std::path::PathBuf;

        let stub_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .unwrap()
            .parent()
            .unwrap()
            .join("stubs/builtins.pyi");

        let stub_content = std::fs::read_to_string(&stub_path).unwrap();

        let stub = StubFile {
            module: "builtins".to_string(),
            path: stub_path,
            exports: FxHashMap::default(),
            is_partial: false,
            reexports: Vec::new(),
            all_exports: None,
            content: Some(stub_content),
        };

        let mut class_registry = ClassRegistry::new();
        let mut typevar_registry = beacon_core::TypeVarConstraintRegistry::new();
        load_stub_into_registry(&stub, &mut class_registry, &mut typevar_registry).unwrap();

        let list_class = class_registry.get_class("list");
        assert!(list_class.is_some(), "list class should be registered in builtins.pyi");

        let list_metadata = list_class.unwrap();

        let append_method = list_metadata.methods.get("append");
        assert!(append_method.is_some(), "append method should exist in builtins.pyi");
    }

    #[test]
    fn test_stub_loading_typing_module() {
        let stub_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .unwrap()
            .parent()
            .unwrap()
            .join("stubs/typing.pyi");

        let stub_content = std::fs::read_to_string(&stub_path).unwrap();

        let stub = StubFile {
            module: "typing".to_string(),
            path: stub_path,
            exports: FxHashMap::default(),
            is_partial: false,
            reexports: Vec::new(),
            all_exports: None,
            content: Some(stub_content),
        };

        let mut class_registry = ClassRegistry::new();
        let mut typevar_registry = beacon_core::TypeVarConstraintRegistry::new();
        load_stub_into_registry(&stub, &mut class_registry, &mut typevar_registry).unwrap();

        let generator_class = class_registry.get_class("Generator");
        assert!(
            generator_class.is_some(),
            "Generator class should be registered in typing.pyi"
        );

        let generator_metadata = generator_class.unwrap();
        assert!(
            generator_metadata.methods.contains_key("__iter__"),
            "Generator should have __iter__ method"
        );
        assert!(
            generator_metadata.methods.contains_key("__next__"),
            "Generator should have __next__ method"
        );
        assert!(
            generator_metadata.methods.contains_key("send"),
            "Generator should have send method"
        );

        let iterator_class = class_registry.get_class("Iterator");
        assert!(
            iterator_class.is_some(),
            "Iterator class should be registered in typing.pyi"
        );

        let iterator_metadata = iterator_class.unwrap();
        assert!(
            iterator_metadata.methods.contains_key("__next__"),
            "Iterator should have __next__ method"
        );
        assert!(
            iterator_metadata.methods.contains_key("__iter__"),
            "Iterator should have __iter__ method"
        );

        let iterable_class = class_registry.get_class("Iterable");
        assert!(
            iterable_class.is_some(),
            "Iterable class should be registered in typing.pyi"
        );

        let iterable_metadata = iterable_class.unwrap();
        assert!(
            iterable_metadata.methods.contains_key("__iter__"),
            "Iterable should have __iter__ method"
        );
    }

    #[test]
    fn test_capabilities_support_stub_generics() {
        let stub_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .unwrap()
            .parent()
            .unwrap()
            .join("stubs/capabilities_support.pyi");

        let stub_content = std::fs::read_to_string(&stub_path).unwrap();

        let stub = StubFile {
            module: "capabilities_support".to_string(),
            path: stub_path,
            exports: FxHashMap::default(),
            is_partial: false,
            reexports: Vec::new(),
            all_exports: None,
            content: Some(stub_content),
        };

        let mut class_registry = ClassRegistry::new();
        let mut typevar_registry = beacon_core::TypeVarConstraintRegistry::new();
        load_stub_into_registry(&stub, &mut class_registry, &mut typevar_registry).unwrap();

        let provider_meta = class_registry
            .get_class("DataProvider")
            .expect("DataProvider should be registered");
        assert_eq!(provider_meta.type_params, vec!["T".to_string()]);

        let load_method = provider_meta
            .lookup_method_type("load")
            .expect("DataProvider.load should exist");

        if let MethodType::Single(Type::Fun(params, ret)) = load_method {
            assert!(!params.is_empty(), "load should include self parameter");
            assert!(matches!(params[0].1, Type::Con(TypeCtor::Any)));

            match ret.as_ref() {
                Type::App(iter_ctor, item_ty) => {
                    assert!(matches!(**iter_ctor, Type::Con(TypeCtor::Class(_))));
                    assert!(matches!(**item_ty, Type::Con(TypeCtor::TypeVariable(ref var)) if var == "T"));
                }
                other => panic!("Expected Iterable[T] return type, got {other:?}"),
            }
        } else {
            panic!("DataProvider.load should have a single method signature");
        }

        let provider_impl = class_registry
            .get_class("InMemoryProvider")
            .expect("InMemoryProvider should be registered");
        assert!(provider_impl.type_params.contains(&"T".to_string()));
    }

    #[test]
    fn test_typing_protocol_types_loaded() {
        let stub_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .unwrap()
            .parent()
            .unwrap()
            .join("stubs/typing.pyi");

        let stub_content = std::fs::read_to_string(&stub_path).unwrap();

        let stub = StubFile {
            module: "typing".to_string(),
            path: stub_path,
            exports: FxHashMap::default(),
            is_partial: false,
            reexports: Vec::new(),
            all_exports: None,
            content: Some(stub_content),
        };

        let mut class_registry = ClassRegistry::new();
        let mut typevar_registry = beacon_core::TypeVarConstraintRegistry::new();
        load_stub_into_registry(&stub, &mut class_registry, &mut typevar_registry).unwrap();

        let protocol_types = vec![
            "Generator",
            "Iterator",
            "Iterable",
            "AsyncGenerator",
            "AsyncIterator",
            "AsyncIterable",
            "Sequence",
            "Mapping",
        ];

        for protocol_name in protocol_types {
            let protocol_class = class_registry.get_class(protocol_name);
            assert!(
                protocol_class.is_some(),
                "{protocol_name} class should be registered in typing.pyi"
            );
        }
    }

    #[test]
    fn test_typevar_with_bound() {
        let stub_content = r#"
from typing import TypeVar

class Animal: ...

T = TypeVar("T", bound=Animal)
"#;

        let stub = StubFile {
            module: "test_bound".to_string(),
            path: PathBuf::from("test_bound.pyi"),
            exports: FxHashMap::default(),
            is_partial: false,
            reexports: Vec::new(),
            all_exports: None,
            content: Some(stub_content.to_string()),
        };

        let mut class_registry = ClassRegistry::new();
        let mut typevar_registry = beacon_core::TypeVarConstraintRegistry::new();
        load_stub_into_registry(&stub, &mut class_registry, &mut typevar_registry).unwrap();

        let mut parser = beacon_parser::PythonParser::new().unwrap();
        let (ast, _) = parser.parse_and_resolve(stub_content).unwrap();
        let mut ctx = StubTypeContext::new();
        collect_stub_type_vars(&ast, &mut ctx);

        assert!(ctx.is_type_var("T"), "T should be registered as a TypeVar");

        let bound = ctx.get_bound("T");
        assert!(bound.is_some(), "T should have a bound");

        if let Some(bound_ty) = bound {
            assert!(
                matches!(bound_ty, Type::Con(TypeCtor::Class(name)) if name == "Animal"),
                "T should be bound to Animal, got {bound_ty:?}"
            );
        }
    }

    #[test]
    fn test_typevar_with_constraints() {
        let stub_content = r#"
from typing import TypeVar

T = TypeVar("T", int, str)
"#;

        let stub = StubFile {
            module: "test_constraints".to_string(),
            path: PathBuf::from("test_constraints.pyi"),
            exports: FxHashMap::default(),
            is_partial: false,
            reexports: Vec::new(),
            all_exports: None,
            content: Some(stub_content.to_string()),
        };

        let mut class_registry = ClassRegistry::new();
        let mut typevar_registry = beacon_core::TypeVarConstraintRegistry::new();
        load_stub_into_registry(&stub, &mut class_registry, &mut typevar_registry).unwrap();

        let mut parser = beacon_parser::PythonParser::new().unwrap();
        let (ast, _) = parser.parse_and_resolve(stub_content).unwrap();
        let mut ctx = StubTypeContext::new();
        collect_stub_type_vars(&ast, &mut ctx);

        assert!(ctx.is_type_var("T"), "T should be registered as a TypeVar");

        let constraints = ctx.get_constraints("T");
        assert!(constraints.is_some(), "T should have constraints");

        if let Some(constraint_types) = constraints {
            assert_eq!(constraint_types.len(), 2, "T should have 2 constraints");
            assert!(
                constraint_types.iter().any(|t| matches!(t, Type::Con(TypeCtor::Int))),
                "T should have int as a constraint"
            );
            assert!(
                constraint_types
                    .iter()
                    .any(|t| matches!(t, Type::Con(TypeCtor::String))),
                "T should have str as a constraint"
            );
        }
    }

    #[test]
    fn test_typevar_with_bound_and_variance() {
        let stub_content = r#"
from typing import TypeVar

class Animal: ...

T_Co = TypeVar("T_Co", bound=Animal, covariant=True)
"#;

        let stub = StubFile {
            module: "test_bound_variance".to_string(),
            path: PathBuf::from("test_bound_variance.pyi"),
            exports: FxHashMap::default(),
            is_partial: false,
            reexports: Vec::new(),
            all_exports: None,
            content: Some(stub_content.to_string()),
        };

        let mut class_registry = ClassRegistry::new();
        let mut typevar_registry = beacon_core::TypeVarConstraintRegistry::new();
        load_stub_into_registry(&stub, &mut class_registry, &mut typevar_registry).unwrap();

        let mut parser = beacon_parser::PythonParser::new().unwrap();
        let (ast, _) = parser.parse_and_resolve(stub_content).unwrap();
        let mut ctx = StubTypeContext::new();
        collect_stub_type_vars(&ast, &mut ctx);

        assert!(ctx.is_type_var("T_Co"), "T_Co should be registered as a TypeVar");

        let bound = ctx.get_bound("T_Co");
        assert!(bound.is_some(), "T_Co should have a bound");

        let type_var = ctx.get_or_create_type_var("T_Co");
        assert_eq!(type_var.variance, Variance::Covariant, "T_Co should be covariant");
    }

    #[test]
    fn test_typevar_registry_populated_from_stub() {
        let stub_content = r#"
from typing import TypeVar

class Animal: ...

T = TypeVar("T", bound=Animal)
U = TypeVar("U", int, str)
"#;

        let stub = StubFile {
            module: "test_registry".to_string(),
            path: PathBuf::from("test_registry.pyi"),
            exports: FxHashMap::default(),
            is_partial: false,
            reexports: Vec::new(),
            all_exports: None,
            content: Some(stub_content.to_string()),
        };

        let mut class_registry = ClassRegistry::new();
        let mut typevar_registry = beacon_core::TypeVarConstraintRegistry::new();
        load_stub_into_registry(&stub, &mut class_registry, &mut typevar_registry).unwrap();

        assert!(
            typevar_registry.bound_count() >= 1,
            "Registry should have at least 1 bound"
        );
        assert!(
            typevar_registry.constraint_count() >= 1,
            "Registry should have at least 1 constraint"
        );
    }
}
