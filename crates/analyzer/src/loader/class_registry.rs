use super::annotations::{extract_generic_params, parse_type_annotation};
use super::typevars::StubTypeContext;

use beacon_core::{
    ClassMetadata, ClassRegistry, FunctionParam, FunctionParamKind, MethodType, Type, TypeCtor, TypeVar, Variance,
};
use beacon_parser::{AstNode, ParameterKind};
use rustc_hash::FxHashSet;
use std::collections::HashMap;

#[derive(Debug)]
pub(crate) struct MethodInfo {
    pub params: Vec<FunctionParam>,
    pub return_type: Type,
    pub decorators: Vec<String>,
    pub is_overload: bool,
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
        Type::FunWithParams(params, ret) => {
            for param in params {
                collect_type_vars_from_type(&param.ty, acc);
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
) {
    match node {
        AstNode::Module { body, .. } => {
            for stmt in body {
                extract_stub_classes_into_registry(stmt, class_registry, ctx);
            }
        }
        AstNode::ClassDef { name, bases, metaclass, body, .. } => {
            let mut metadata = ClassMetadata::new(name.clone());
            let resolved_bases = bases.clone();

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
        metadata.add_field(target.target_display(), attr_type);
    }
}

fn build_method_info(
    params: &[beacon_parser::Parameter], return_type: &Option<String>, decorators: &[String], ctx: &mut StubTypeContext,
) -> MethodInfo {
    let mut param_list: Vec<FunctionParam> = Vec::new();
    let has_self = params.first().map(|p| p.name == "self").unwrap_or(false);
    let has_cls = params.first().map(|p| p.name == "cls").unwrap_or(false);

    if has_self {
        param_list.push(FunctionParam::new("self", Type::any()));
    } else if has_cls {
        param_list.push(FunctionParam::new("cls", Type::any()));
    }

    let start_idx = if has_self || has_cls { 1 } else { 0 };
    for param in params.iter().skip(start_idx) {
        if let Some(ann) = &param.type_annotation
            && let Some(ty) = parse_type_annotation(ann, ctx)
        {
            param_list.push(FunctionParam::with_metadata(
                param.name.trim_start_matches('*').to_string(),
                ty,
                function_param_kind(param.kind),
                param.default_value.is_some(),
            ));
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
            let func_type = Type::fun_with_params(info.params.clone(), info.return_type.clone());
            metadata.set_init_type(func_type);
        }
        return true;
    }

    if method_name == "__new__" {
        if let Some(info) = method_infos.first() {
            let func_type = Type::fun_with_params(info.params.clone(), info.return_type.clone());
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
        let func_type = Type::fun_with_params(params, info.return_type.clone());

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
    let func_type = Type::fun_with_params(params, method_info.return_type.clone());

    if decorator_flags.staticmethod {
        metadata.add_staticmethod(method_name, func_type);
    } else if decorator_flags.classmethod {
        metadata.add_classmethod(method_name, func_type);
    } else {
        metadata.add_method(method_name, func_type);
    }
}

fn adjust_params_for_static(params: &[FunctionParam], is_static: bool) -> Vec<FunctionParam> {
    if is_static && !params.is_empty() {
        params.iter().skip(1).cloned().collect()
    } else {
        params.to_vec()
    }
}

fn function_param_kind(kind: ParameterKind) -> FunctionParamKind {
    match kind {
        ParameterKind::PositionalOnly => FunctionParamKind::PositionalOnly,
        ParameterKind::PositionalOrKeyword => FunctionParamKind::PositionalOrKeyword,
        ParameterKind::VarArgs => FunctionParamKind::VarArgs,
        ParameterKind::KeywordOnly => FunctionParamKind::KeywordOnly,
        ParameterKind::KwArgs => FunctionParamKind::KwArgs,
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
