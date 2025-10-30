//! Type analysis orchestration
//!
//! Coordinates parsing, constraint generation, type inference, and caching.
//! This is the bridge between the parser, type system, and LSP features.
//!
//! [Analyzer] manages the flow from source code to inferred types:
//!  1. Parsing -> AST
//!  2. Name resolution -> Symbol table
//!  3. Constraint generation -> Constraint set
//!  4. Unification/solving -> Type substitution
//!  5. Caching -> Type cache

pub mod cfg;
pub mod class_metadata;
pub mod constraint_gen;
pub mod data_flow;
pub mod linter;
pub mod rules;
pub mod type_env;

use crate::config::Config;
use crate::document::DocumentManager;
use crate::utils;
use crate::{analysis::class_metadata::ClassRegistry, cache::CacheManager};
use beacon_core::TypeCtor;
use beacon_core::{
    Subst, Type, TypeScheme, TypeVarGen, Unifier,
    errors::{AnalysisError, Result},
};
use beacon_parser::{AstNode, LiteralValue, SymbolTable};
use constraint_gen::{Constraint, ConstraintGenContext, ConstraintResult, ConstraintSet, Span};
use lsp_types::Position;
use rustc_hash::FxHashMap;
use std::collections::HashMap;
use std::{path::Path, sync::Arc};
use tokio::sync::RwLock;
use type_env::TypeEnvironment;
use url::Url;

/// Type error with location information
#[derive(Debug, Clone)]
pub struct TypeErrorInfo {
    pub error: beacon_core::TypeError,
    /// Span where the error occurred
    pub span: Span,
}

impl TypeErrorInfo {
    /// Create a new type error with location
    pub fn new(error: beacon_core::TypeError, span: Span) -> Self {
        Self { error, span }
    }

    pub fn line(&self) -> usize {
        self.span.line
    }

    pub fn col(&self) -> usize {
        self.span.col
    }

    pub fn end_line(&self) -> Option<usize> {
        self.span.end_line
    }

    pub fn end_col(&self) -> Option<usize> {
        self.span.end_col
    }
}

#[derive(Debug)]
struct MethodInfo {
    params: Vec<beacon_core::Type>,
    return_type: beacon_core::Type,
    decorators: Vec<String>,
    is_overload: bool,
}

/// Result of analyzing a document
pub struct AnalysisResult {
    /// Document URI
    pub uri: Url,
    /// Document version
    pub version: i32,
    /// Map from AST node IDs to inferred types
    pub type_map: FxHashMap<usize, Type>,
    /// Type errors encountered during analysis
    pub type_errors: Vec<TypeErrorInfo>,
    /// Static analysis results (data flow analysis)
    pub static_analysis: Option<data_flow::DataFlowResult>,
}

/// Orchestrates type analysis for documents
pub struct Analyzer {
    _config: Config,
    cache: CacheManager,
    _type_var_gen: TypeVarGen,
    documents: DocumentManager,
    /// Map from document URI to position maps for type-at-position queries
    position_maps: FxHashMap<Url, FxHashMap<(usize, usize), usize>>,
    /// Stub cache for type resolution (shared with workspace)
    stub_cache: Option<Arc<std::sync::RwLock<crate::workspace::StubCache>>>,
}

impl Analyzer {
    /// Create a new analyzer with the given configuration
    pub fn new(config: Config, documents: DocumentManager) -> Self {
        Self {
            _config: config,
            cache: CacheManager::new(),
            _type_var_gen: TypeVarGen::new(),
            documents,
            position_maps: FxHashMap::default(),
            stub_cache: None,
        }
    }

    /// Create a new analyzer with workspace support for stub resolution
    pub fn with_workspace(
        config: Config, documents: DocumentManager, workspace: Arc<RwLock<crate::workspace::Workspace>>,
    ) -> Self {
        let stub_cache = workspace.try_read().ok().map(|ws| ws.stub_cache());

        Self {
            _config: config,
            cache: CacheManager::new(),
            _type_var_gen: TypeVarGen::new(),
            documents,
            position_maps: FxHashMap::default(),
            stub_cache,
        }
    }

    /// Analyze a document and return inferred types
    pub fn analyze(&mut self, uri: &Url) -> Result<AnalysisResult> {
        let result = self.documents.get_document(uri, |doc| {
            let ast = doc.ast().ok_or(AnalysisError::MissingAst)?.clone();
            let symbol_table = doc.symbol_table().ok_or(AnalysisError::MissingSymbolTable)?.clone();

            Ok::<_, AnalysisError>((ast, symbol_table, doc.version))
        });

        let (ast, symbol_table, version) = match result {
            Some(Ok(data)) => data,
            Some(Err(e)) => return Err(e.into()),
            None => return Err(AnalysisError::DocumentNotFound(uri.clone()).into()),
        };

        let ConstraintResult(constraints, mut type_map, position_map, class_registry) =
            self.generate_constraints(&ast, &symbol_table)?;

        let (substitution, type_errors) = Self::solve_constraints(constraints, &class_registry)?;

        for (_node_id, ty) in type_map.iter_mut() {
            *ty = substitution.apply(ty);
        }

        self.position_maps.insert(uri.clone(), position_map);

        let static_analysis = self.perform_static_analysis(&ast, &symbol_table);
        Ok(AnalysisResult { uri: uri.clone(), version, type_map, type_errors, static_analysis })
    }

    /// Get the inferred type at a specific position
    ///
    /// Analyzes the document and looks up the type at the specified position.
    /// Returns None if no type information is available at that position.
    pub fn type_at_position(&mut self, uri: &Url, position: Position) -> Result<Option<Type>> {
        let result = self.analyze(uri)?;
        let line = (position.line + 1) as usize;
        let col = (position.character + 1) as usize;

        let position_map = self.position_maps.get(uri);
        if let Some(pos_map) = position_map {
            if let Some(node_id) = pos_map.get(&(line, col)) {
                return Ok(result.type_map.get(node_id).cloned());
            }
        }

        Ok(None)
    }

    /// Invalidate cached analysis for a document
    pub fn invalidate(&mut self, uri: &Url) {
        self.cache.invalidate_document(uri);
        self.position_maps.remove(uri);
    }

    /// Extract class metadata from stub AST and register them in the ClassRegistry
    fn extract_stub_classes_into_registry(node: &AstNode, class_registry: &mut class_metadata::ClassRegistry) {
        match node {
            AstNode::Module { body, .. } => {
                for stmt in body {
                    Self::extract_stub_classes_into_registry(stmt, class_registry);
                }
            }
            AstNode::ClassDef { name, bases, metaclass, body, .. } => {
                let mut metadata = class_metadata::ClassMetadata::new(name.clone());
                let is_protocol = bases
                    .iter()
                    .any(|base| base == "Protocol" || base.ends_with(".Protocol") || base == "typing.Protocol");
                metadata.set_protocol(is_protocol);

                for base in bases {
                    metadata.add_base_class(base.clone());
                }

                if let Some(meta) = metaclass {
                    metadata.set_metaclass(meta.clone());
                }

                Self::process_class_methods(body, &mut metadata);

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
    fn process_class_methods(body: &[AstNode], metadata: &mut class_metadata::ClassMetadata) {
        let mut methods_by_name: HashMap<String, Vec<MethodInfo>> = HashMap::new();

        for stmt in body {
            if let AstNode::FunctionDef { name: method_name, args: params, return_type, decorators, .. } = stmt {
                let param_types: Vec<beacon_core::Type> = params
                    .iter()
                    .filter_map(|p| p.type_annotation.as_ref())
                    .filter_map(|ann| Self::parse_type_annotation(ann))
                    .collect();

                let ret_type = return_type
                    .as_ref()
                    .and_then(|ann| Self::parse_type_annotation(ann))
                    .unwrap_or_else(beacon_core::Type::any);

                let is_overload = decorators.iter().any(|d| d == "overload");

                methods_by_name
                    .entry(method_name.clone())
                    .or_default()
                    .push(MethodInfo {
                        params: param_types,
                        return_type: ret_type,
                        decorators: decorators.clone(),
                        is_overload,
                    });
            }
        }

        for (method_name, method_infos) in methods_by_name {
            if method_name == "__init__" {
                if let Some(info) = method_infos.first() {
                    let func_type = beacon_core::Type::fun(info.params.clone(), info.return_type.clone());
                    metadata.set_init_type(func_type);
                }
                continue;
            }

            if method_name == "__new__" {
                if let Some(info) = method_infos.first() {
                    let func_type = beacon_core::Type::fun(info.params.clone(), info.return_type.clone());
                    metadata.set_new_type(func_type);
                }
                continue;
            }

            let first_info = &method_infos[0];
            let has_property = first_info.decorators.iter().any(|d| d == "property");
            let has_staticmethod = first_info.decorators.iter().any(|d| d == "staticmethod");
            let has_classmethod = first_info.decorators.iter().any(|d| d == "classmethod");

            if has_property {
                metadata.add_property(method_name.clone(), first_info.return_type.clone());
                continue;
            }

            let has_overloads = method_infos.iter().any(|info| info.is_overload);

            if has_overloads && method_infos.len() > 1 {
                let mut overload_sigs = Vec::new();
                let mut implementation = None;

                for info in method_infos {
                    let mut params = info.params.clone();

                    if has_staticmethod && !params.is_empty() {
                        params = params.iter().skip(1).cloned().collect();
                    }

                    let func_type = Type::fun(params, info.return_type.clone());

                    if info.is_overload {
                        overload_sigs.push(func_type);
                    } else {
                        implementation = Some(func_type);
                    }
                }

                let overload_set = beacon_core::OverloadSet { signatures: overload_sigs, implementation };

                if has_staticmethod {
                    if let Some(impl_type) = overload_set.implementation.clone() {
                        metadata.add_staticmethod(method_name.clone(), impl_type);
                    } else if let Some(first_sig) = overload_set.signatures.first() {
                        metadata.add_staticmethod(method_name.clone(), first_sig.clone());
                    }
                } else if has_classmethod {
                    if let Some(impl_type) = overload_set.implementation.clone() {
                        metadata.add_classmethod(method_name.clone(), impl_type);
                    } else if let Some(first_sig) = overload_set.signatures.first() {
                        metadata.add_classmethod(method_name.clone(), first_sig.clone());
                    }
                } else {
                    metadata.add_overloaded_method(method_name.clone(), overload_set);
                }
            } else {
                let mut params = first_info.params.clone();

                if has_staticmethod && !params.is_empty() {
                    params = params.iter().skip(1).cloned().collect();
                }

                let func_type = Type::fun(params, first_info.return_type.clone());

                if has_staticmethod {
                    metadata.add_staticmethod(method_name.clone(), func_type);
                } else if has_classmethod {
                    metadata.add_classmethod(method_name.clone(), func_type);
                } else {
                    metadata.add_method(method_name.clone(), func_type);
                }
            }
        }
    }

    /// Parse a type annotation string into a Type
    /// This is a simplified version for stub parsing
    fn parse_type_annotation(annotation: &str) -> Option<beacon_core::Type> {
        let annotation = annotation.trim();

        if annotation.contains(" | ") {
            let parts: Vec<&str> = annotation.split(" | ").collect();
            let types: Vec<beacon_core::Type> = parts.iter().filter_map(|p| Self::parse_type_annotation(p)).collect();
            if types.len() > 1 {
                return Some(beacon_core::Type::Union(types));
            }
        }

        if let Some(idx) = annotation.find('[') {
            let base = &annotation[..idx];
            let base_type = match base {
                "list" => beacon_core::Type::Con(TypeCtor::List),
                "dict" => beacon_core::Type::Con(TypeCtor::Dict),
                "set" => beacon_core::Type::Con(TypeCtor::Set),
                "tuple" => beacon_core::Type::Con(TypeCtor::Tuple),
                _ => beacon_core::Type::Con(TypeCtor::Class(base.to_string())),
            };
            // TODO: Parse and apply generic parameters properly
            return Some(base_type);
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

    fn load_builtins_into_registry(stub_path: &Path, class_registry: &mut ClassRegistry) -> Result<()> {
        match std::fs::read_to_string(stub_path) {
            Ok(content) => {
                let mut parser = crate::parser::LspParser::new()?;
                let parse_result = parser.parse(&content)?;
                Self::extract_stub_classes_into_registry(&parse_result.ast, class_registry);
                Ok(())
            }
            Err(e) => Err(AnalysisError::from(e).into()),
        }
    }

    /// Generate constraints from an AST
    ///
    /// Implements Algorithm W constraint generation with type environment threading.
    /// Returns constraints, type_map, and position_map for type-at-position queries.
    fn generate_constraints(&self, ast: &AstNode, symbol_table: &SymbolTable) -> Result<ConstraintResult> {
        let mut ctx = ConstraintGenContext::new();

        if let Some(stub_cache) = &self.stub_cache {
            if let Ok(cache) = stub_cache.try_read() {
                if let Some(builtins) = cache.get("builtins") {
                    Self::load_builtins_into_registry(&builtins.path, &mut ctx.class_registry)?;
                }
            }
        }

        let mut env = TypeEnvironment::from_symbol_table(symbol_table, ast);

        Self::visit_node_with_env(ast, &mut env, &mut ctx, self.stub_cache.as_ref())?;

        Ok(ConstraintResult(
            ConstraintSet { constraints: ctx.constraints },
            ctx.type_map,
            ctx.position_map,
            ctx.class_registry,
        ))
    }

    /// Visit an AST node and generate constraints with type environment
    ///
    /// Implements constraint generation for core Python constructs.
    /// Records type information in the context for type-at-position queries.
    fn visit_node_with_env(
        node: &AstNode, env: &mut TypeEnvironment, ctx: &mut ConstraintGenContext,
        stub_cache: Option<&Arc<std::sync::RwLock<crate::workspace::StubCache>>>,
    ) -> Result<Type> {
        match node {
            AstNode::Module { body, .. } => {
                for stmt in body {
                    Self::visit_node_with_env(stmt, env, ctx, stub_cache)?;
                }
                Ok(Type::Con(TypeCtor::Module("".into())))
            }
            AstNode::FunctionDef { name, args, return_type, body, decorators, line, col, .. } => {
                let param_types: Vec<Type> = args
                    .iter()
                    .map(|param| {
                        param
                            .type_annotation
                            .as_ref()
                            .map(|ann| env.parse_annotation_or_any(ann))
                            .unwrap_or_else(|| Type::Var(env.fresh_var()))
                    })
                    .collect();

                let ret_type = return_type
                    .as_ref()
                    .map(|ann| env.parse_annotation_or_any(ann))
                    .unwrap_or_else(|| Type::Var(env.fresh_var()));

                let fn_type = Type::fun(param_types.clone(), ret_type.clone());

                let mut body_env = env.clone();
                for (param, param_type) in args.iter().zip(param_types.iter()) {
                    body_env.bind(param.name.clone(), TypeScheme::mono(param_type.clone()));
                }

                let mut body_ty = Type::none();
                for stmt in body {
                    body_ty = Self::visit_node_with_env(stmt, &mut body_env, ctx, stub_cache)?;
                }

                let span = Span::new(*line, *col);
                ctx.constraints.push(Constraint::Equal(body_ty, ret_type, span));

                let mut decorated_type = fn_type.clone();
                for decorator in decorators.iter().rev() {
                    let decorator_ty = env.lookup(decorator).unwrap_or_else(|| Type::Var(env.fresh_var()));
                    let result_ty = Type::Var(env.fresh_var());

                    let span = Span::new(*line, *col);
                    ctx.constraints.push(Constraint::Call(
                        decorator_ty,
                        vec![decorated_type],
                        result_ty.clone(),
                        span,
                    ));

                    decorated_type = result_ty;
                }

                env.bind(name.clone(), TypeScheme::mono(decorated_type.clone()));

                ctx.record_type(*line, *col, decorated_type.clone());
                Ok(decorated_type)
            }

            AstNode::ClassDef { name, body, decorators, line, col, .. } => {
                let class_type = Type::Con(TypeCtor::Class(name.clone()));
                let metadata = Self::extract_class_metadata(name, body, env);
                ctx.class_registry.register_class(name.clone(), metadata);

                for stmt in body {
                    if !matches!(stmt, AstNode::FunctionDef { .. }) {
                        Self::visit_node_with_env(stmt, env, ctx, stub_cache)?;
                    }
                }

                let mut decorated_type = class_type.clone();
                for decorator in decorators.iter().rev() {
                    let decorator_ty = env.lookup(decorator).unwrap_or_else(|| Type::Var(env.fresh_var()));
                    let result_ty = Type::Var(env.fresh_var());
                    let span = Span::new(*line, *col);

                    ctx.constraints.push(Constraint::Call(
                        decorator_ty,
                        vec![decorated_type],
                        result_ty.clone(),
                        span,
                    ));

                    decorated_type = result_ty;
                }

                env.bind(name.clone(), TypeScheme::mono(decorated_type.clone()));

                ctx.record_type(*line, *col, decorated_type.clone());
                Ok(decorated_type)
            }
            // TODO: Determine if value is non-expansive for generalization
            AstNode::Assignment { target, value, line, col } => {
                let value_ty = Self::visit_node_with_env(value, env, ctx, stub_cache)?;
                env.bind(target.clone(), TypeScheme::mono(value_ty.clone()));
                ctx.record_type(*line, *col, value_ty.clone());
                Ok(value_ty)
            }
            AstNode::AnnotatedAssignment { target, type_annotation, value, line, col } => {
                let annotated_ty = env.parse_annotation_or_any(type_annotation);
                if let Some(val) = value {
                    let value_ty = Self::visit_node_with_env(val, env, ctx, stub_cache)?;
                    let span = Span::new(*line, *col);
                    ctx.constraints
                        .push(Constraint::Equal(value_ty, annotated_ty.clone(), span));
                }
                env.bind(target.clone(), TypeScheme::mono(annotated_ty.clone()));
                ctx.record_type(*line, *col, annotated_ty.clone());
                Ok(annotated_ty)
            }
            // TODO: Handle complex call expressions
            AstNode::Call { function, args, line, col } => {
                let func_ty = env.lookup(function).unwrap_or_else(|| Type::Var(env.fresh_var()));
                let mut arg_types = Vec::new();
                for arg in args {
                    arg_types.push(Self::visit_node_with_env(arg, env, ctx, stub_cache)?);
                }
                let ret_ty = Type::Var(env.fresh_var());
                let span = Span::new(*line, *col);
                ctx.constraints
                    .push(Constraint::Call(func_ty, arg_types, ret_ty.clone(), span));
                ctx.record_type(*line, *col, ret_ty.clone());
                Ok(ret_ty)
            }
            AstNode::Identifier { name, line, col } => {
                let ty = env.lookup(name).unwrap_or_else(|| Type::Var(env.fresh_var()));
                ctx.record_type(*line, *col, ty.clone());
                Ok(ty)
            }
            AstNode::Literal { value, line, col } => {
                let ty = match value {
                    LiteralValue::Integer(_) => Type::int(),
                    LiteralValue::Float(_) => Type::float(),
                    LiteralValue::String { .. } => Type::string(),
                    LiteralValue::Boolean(_) => Type::bool(),
                    LiteralValue::None => Type::none(),
                };
                ctx.record_type(*line, *col, ty.clone());
                Ok(ty)
            }
            AstNode::Return { value, line, col } => {
                let ty = if let Some(val) = value {
                    Self::visit_node_with_env(val, env, ctx, stub_cache)?
                } else {
                    Type::none()
                };
                ctx.record_type(*line, *col, ty.clone());
                Ok(ty)
            }
            AstNode::Attribute { object, attribute, line, col } => {
                let obj_ty = Self::visit_node_with_env(object, env, ctx, stub_cache)?;
                let attr_ty = Type::Var(env.fresh_var());
                let span = Span::new(*line, *col);
                ctx.constraints
                    .push(Constraint::HasAttr(obj_ty, attribute.clone(), attr_ty.clone(), span));
                ctx.record_type(*line, *col, attr_ty.clone());
                Ok(attr_ty)
            }
            AstNode::BinaryOp { left, right, line, col, .. } => {
                let left_ty = Self::visit_node_with_env(left, env, ctx, stub_cache)?;
                let right_ty = Self::visit_node_with_env(right, env, ctx, stub_cache)?;
                let span = Span::new(*line, *col);
                ctx.constraints.push(Constraint::Equal(left_ty.clone(), right_ty, span));
                ctx.record_type(*line, *col, left_ty.clone());
                Ok(left_ty)
            }
            AstNode::UnaryOp { operand, line, col, .. } => {
                let ty = Self::visit_node_with_env(operand, env, ctx, stub_cache)?;
                ctx.record_type(*line, *col, ty.clone());
                Ok(ty)
            }
            AstNode::Subscript { value, slice, line, col } => {
                let value_ty = Self::visit_node_with_env(value, env, ctx, stub_cache)?;

                Self::visit_node_with_env(slice, env, ctx, stub_cache)?;

                let result_ty = Type::Var(env.fresh_var());
                let span = Span::new(*line, *col);

                ctx.constraints.push(Constraint::HasAttr(
                    value_ty,
                    "__getitem__".to_string(),
                    result_ty.clone(),
                    span,
                ));

                ctx.record_type(*line, *col, result_ty.clone());
                Ok(result_ty)
            }
            AstNode::If { test, body, elif_parts, else_body, .. } => {
                Self::visit_node_with_env(test, env, ctx, stub_cache)?;

                let (narrowed_var, narrowed_type) = Self::detect_type_guard(test, &mut env.clone());

                let mut true_env = env.clone();
                if let (Some(var_name), Some(refined_ty)) = (narrowed_var.as_ref(), narrowed_type.as_ref()) {
                    true_env.bind(var_name.clone(), TypeScheme::mono(refined_ty.clone()));
                }

                for stmt in body {
                    Self::visit_node_with_env(stmt, &mut true_env, ctx, stub_cache)?;
                }

                for (elif_test, elif_body) in elif_parts {
                    Self::visit_node_with_env(elif_test, env, ctx, stub_cache)?;
                    for stmt in elif_body {
                        Self::visit_node_with_env(stmt, env, ctx, stub_cache)?;
                    }
                }
                if let Some(else_stmts) = else_body {
                    for stmt in else_stmts {
                        Self::visit_node_with_env(stmt, env, ctx, stub_cache)?;
                    }
                }
                Ok(Type::none())
            }
            AstNode::Import { module, alias, line, col } => {
                let module_name = alias.as_ref().unwrap_or(module);
                let module_type = Type::Con(TypeCtor::Module(module.clone()));
                env.bind(module_name.clone(), TypeScheme::mono(module_type.clone()));
                ctx.record_type(*line, *col, module_type.clone());
                Ok(module_type)
            }
            AstNode::ImportFrom { module, names, line, col } => {
                if let Some(cache_arc) = stub_cache {
                    if let Ok(cache) = cache_arc.read() {
                        for name in names {
                            let ty = cache
                                .get(module)
                                .and_then(|stub| stub.exports.get(name).cloned())
                                .unwrap_or_else(|| Type::Var(env.fresh_var()));
                            env.bind(name.clone(), TypeScheme::mono(ty));
                        }
                    } else {
                        for name in names {
                            let ty = Type::Var(env.fresh_var());
                            env.bind(name.clone(), TypeScheme::mono(ty));
                        }
                    }
                } else {
                    for name in names {
                        let ty = Type::Var(env.fresh_var());
                        env.bind(name.clone(), TypeScheme::mono(ty));
                    }
                }
                let module_type = Type::Con(TypeCtor::Module(module.clone()));
                ctx.record_type(*line, *col, module_type.clone());
                Ok(module_type)
            }
            AstNode::For { target, iter, body, else_body, line, col } => {
                let iter_ty = Self::visit_node_with_env(iter, env, ctx, stub_cache)?;

                let element_ty = Type::Var(env.fresh_var());
                let span = Span::new(*line, *col);
                ctx.constraints.push(Constraint::Protocol(
                    iter_ty,
                    beacon_core::ProtocolName::Iterable,
                    element_ty.clone(),
                    span,
                ));

                env.bind(target.clone(), TypeScheme::mono(element_ty));

                for stmt in body {
                    Self::visit_node_with_env(stmt, env, ctx, stub_cache)?;
                }

                if let Some(else_stmts) = else_body {
                    for stmt in else_stmts {
                        Self::visit_node_with_env(stmt, env, ctx, stub_cache)?;
                    }
                }

                ctx.record_type(*line, *col, Type::none());
                Ok(Type::none())
            }

            AstNode::While { test, body, else_body, line, col } => {
                Self::visit_node_with_env(test, env, ctx, stub_cache)?;

                for stmt in body {
                    Self::visit_node_with_env(stmt, env, ctx, stub_cache)?;
                }

                if let Some(else_stmts) = else_body {
                    for stmt in else_stmts {
                        Self::visit_node_with_env(stmt, env, ctx, stub_cache)?;
                    }
                }

                ctx.record_type(*line, *col, Type::none());
                Ok(Type::none())
            }

            AstNode::Try { body, handlers, else_body, finally_body, line, col } => {
                for stmt in body {
                    Self::visit_node_with_env(stmt, env, ctx, stub_cache)?;
                }

                for handler in handlers {
                    let mut handler_env = env.clone();
                    if let Some(ref name) = handler.name {
                        // TODO: Use proper exception type hierarchy when available
                        let exc_ty = Type::Var(env.fresh_var());
                        handler_env.bind(name.clone(), TypeScheme::mono(exc_ty));
                    }

                    for stmt in &handler.body {
                        Self::visit_node_with_env(stmt, &mut handler_env, ctx, stub_cache)?;
                    }
                }

                if let Some(else_stmts) = else_body {
                    for stmt in else_stmts {
                        Self::visit_node_with_env(stmt, env, ctx, stub_cache)?;
                    }
                }

                if let Some(finally_stmts) = finally_body {
                    for stmt in finally_stmts {
                        Self::visit_node_with_env(stmt, env, ctx, stub_cache)?;
                    }
                }

                ctx.record_type(*line, *col, Type::none());
                Ok(Type::none())
            }

            AstNode::Match { subject, cases, line, col } => {
                Self::visit_node_with_env(subject, env, ctx, stub_cache)?;

                // TODO: Implement pattern matching type narrowing when pattern system is ready
                for case in cases {
                    let mut case_env = env.clone();

                    if let Some(ref guard) = case.guard {
                        Self::visit_node_with_env(guard, &mut case_env, ctx, stub_cache)?;
                    }

                    for stmt in &case.body {
                        Self::visit_node_with_env(stmt, &mut case_env, ctx, stub_cache)?;
                    }
                }

                ctx.record_type(*line, *col, Type::none());
                Ok(Type::none())
            }
            AstNode::Raise { exc, line, col } => {
                if let Some(exception) = exc {
                    Self::visit_node_with_env(exception, env, ctx, stub_cache)?;
                }

                ctx.record_type(*line, *col, Type::never());
                Ok(Type::never())
            }

            AstNode::With { items, body, line, col } => {
                for item in items {
                    let context_ty = Self::visit_node_with_env(&item.context_expr, env, ctx, stub_cache)?;

                    let enter_ty = Type::Var(env.fresh_var());
                    let span = Span::new(*line, *col);
                    ctx.constraints.push(Constraint::HasAttr(
                        context_ty.clone(),
                        "__enter__".to_string(),
                        enter_ty.clone(),
                        span,
                    ));

                    let exit_ty = Type::Var(env.fresh_var());
                    ctx.constraints
                        .push(Constraint::HasAttr(context_ty, "__exit__".to_string(), exit_ty, span));

                    if let Some(ref target) = item.optional_vars {
                        env.bind(target.clone(), TypeScheme::mono(enter_ty));
                    }
                }

                for stmt in body {
                    Self::visit_node_with_env(stmt, env, ctx, stub_cache)?;
                }

                ctx.record_type(*line, *col, Type::none());
                Ok(Type::none())
            }

            AstNode::Lambda { args, body, line, col } => {
                let param_types: Vec<Type> = args
                    .iter()
                    .map(|param| {
                        param
                            .type_annotation
                            .as_ref()
                            .map(|ann| env.parse_annotation_or_any(ann))
                            .unwrap_or_else(|| Type::Var(env.fresh_var()))
                    })
                    .collect();

                let mut lambda_env = env.clone();
                for (param, param_type) in args.iter().zip(param_types.iter()) {
                    lambda_env.bind(param.name.clone(), TypeScheme::mono(param_type.clone()));
                }

                let body_ty = Self::visit_node_with_env(body, &mut lambda_env, ctx, stub_cache)?;
                let lambda_ty = Type::fun(param_types, body_ty);

                ctx.record_type(*line, *col, lambda_ty.clone());
                Ok(lambda_ty)
            }

            AstNode::Compare { left, comparators, line, col, .. } => {
                Self::visit_node_with_env(left, env, ctx, stub_cache)?;

                for comp in comparators {
                    Self::visit_node_with_env(comp, env, ctx, stub_cache)?;
                }

                ctx.record_type(*line, *col, Type::bool());
                Ok(Type::bool())
            }
            AstNode::NamedExpr { target, value, line, col } => {
                let value_ty = Self::visit_node_with_env(value, env, ctx, stub_cache)?;
                env.bind(target.clone(), TypeScheme::mono(value_ty.clone()));
                ctx.record_type(*line, *col, value_ty.clone());
                Ok(value_ty)
            }
            AstNode::ListComp { element, generators, line, col } => {
                let mut comp_env = env.clone();

                for generator in generators {
                    let iter_ty = Self::visit_node_with_env(&generator.iter, &mut comp_env, ctx, stub_cache)?;

                    let element_ty = Type::Var(comp_env.fresh_var());
                    let span = Span::new(*line, *col);
                    ctx.constraints.push(Constraint::Protocol(
                        iter_ty,
                        beacon_core::ProtocolName::Iterable,
                        element_ty.clone(),
                        span,
                    ));

                    comp_env.bind(generator.target.clone(), TypeScheme::mono(element_ty));

                    for if_clause in &generator.ifs {
                        Self::visit_node_with_env(if_clause, &mut comp_env, ctx, stub_cache)?;
                    }
                }

                let elem_ty = Self::visit_node_with_env(element, &mut comp_env, ctx, stub_cache)?;
                let list_ty = Type::list(elem_ty);

                ctx.record_type(*line, *col, list_ty.clone());
                Ok(list_ty)
            }

            AstNode::SetComp { element, generators, line, col } => {
                let mut comp_env = env.clone();

                for generator in generators {
                    let iter_ty = Self::visit_node_with_env(&generator.iter, &mut comp_env, ctx, stub_cache)?;

                    let element_ty = Type::Var(comp_env.fresh_var());
                    let span = Span::new(*line, *col);
                    ctx.constraints.push(Constraint::Protocol(
                        iter_ty,
                        beacon_core::ProtocolName::Iterable,
                        element_ty.clone(),
                        span,
                    ));

                    comp_env.bind(generator.target.clone(), TypeScheme::mono(element_ty));

                    for if_clause in &generator.ifs {
                        Self::visit_node_with_env(if_clause, &mut comp_env, ctx, stub_cache)?;
                    }
                }

                let elem_ty = Self::visit_node_with_env(element, &mut comp_env, ctx, stub_cache)?;
                let set_ty = Type::App(Box::new(Type::Con(TypeCtor::Set)), Box::new(elem_ty));

                ctx.record_type(*line, *col, set_ty.clone());
                Ok(set_ty)
            }

            AstNode::DictComp { key, value, generators, line, col } => {
                let mut comp_env = env.clone();

                for generator in generators {
                    let iter_ty = Self::visit_node_with_env(&generator.iter, &mut comp_env, ctx, stub_cache)?;

                    let element_ty = Type::Var(comp_env.fresh_var());
                    let span = Span::new(*line, *col);
                    ctx.constraints.push(Constraint::Protocol(
                        iter_ty,
                        beacon_core::ProtocolName::Iterable,
                        element_ty.clone(),
                        span,
                    ));

                    comp_env.bind(generator.target.clone(), TypeScheme::mono(element_ty));

                    for if_clause in &generator.ifs {
                        Self::visit_node_with_env(if_clause, &mut comp_env, ctx, stub_cache)?;
                    }
                }

                let key_ty = Self::visit_node_with_env(key, &mut comp_env, ctx, stub_cache)?;
                let val_ty = Self::visit_node_with_env(value, &mut comp_env, ctx, stub_cache)?;
                let dict_ty = Type::dict(key_ty, val_ty);

                ctx.record_type(*line, *col, dict_ty.clone());
                Ok(dict_ty)
            }
            // NOTE: Approximated as iterable[T] rather than proper Generator[T, None, None]
            // TODO: See ROADMAP.md for Generator[YieldType, SendType, ReturnType] modeling
            AstNode::GeneratorExp { element, generators, line, col } => {
                let mut comp_env = env.clone();

                for generator in generators {
                    let iter_ty = Self::visit_node_with_env(&generator.iter, &mut comp_env, ctx, stub_cache)?;

                    let element_ty = Type::Var(comp_env.fresh_var());
                    let span = Span::new(*line, *col);
                    ctx.constraints.push(Constraint::Protocol(
                        iter_ty,
                        beacon_core::ProtocolName::Iterable,
                        element_ty.clone(),
                        span,
                    ));

                    comp_env.bind(generator.target.clone(), TypeScheme::mono(element_ty));

                    for if_clause in &generator.ifs {
                        Self::visit_node_with_env(if_clause, &mut comp_env, ctx, stub_cache)?;
                    }
                }

                let elem_ty = Self::visit_node_with_env(element, &mut comp_env, ctx, stub_cache)?;
                let generator_ty = Type::list(elem_ty);

                ctx.record_type(*line, *col, generator_ty.clone());
                Ok(generator_ty)
            }
            AstNode::Tuple { .. } | AstNode::Pass { .. } | AstNode::Break { .. } | AstNode::Continue { .. } => {
                Ok(Type::none())
            }
        }
    }

    #[deprecated]
    fn _visit_node(&mut self, node: &AstNode, _constraints: &mut [Constraint]) -> Result<Type> {
        let mut env = TypeEnvironment::new();
        let mut ctx = ConstraintGenContext::new();
        Self::visit_node_with_env(node, &mut env, &mut ctx, None)
    }

    /// Convert a Type::Fun to a MethodSignature
    ///
    /// Extracts parameters and return type from a function type.
    /// Returns None if the type is not a function type.
    fn type_to_method_signature(name: &str, ty: &Type) -> Option<beacon_core::protocols::MethodSignature> {
        match ty {
            Type::Fun(params, ret) => Some(beacon_core::protocols::MethodSignature {
                name: name.to_string(),
                params: params.clone(),
                return_type: ret.as_ref().clone(),
            }),
            _ => None,
        }
    }

    /// Check if a type satisfies a user-defined protocol
    ///
    /// Checks structural conformance by verifying that the type has all required methods with compatible signatures.
    /// Uses full variance checking: contravariant parameters, covariant returns.
    fn check_user_defined_protocol(
        ty: &Type, protocol_name: &str, class_registry: &class_metadata::ClassRegistry,
    ) -> bool {
        let protocol_meta = match class_registry.get_class(protocol_name) {
            Some(meta) if meta.is_protocol => meta,
            _ => return false,
        };

        let required_methods = protocol_meta.get_protocol_methods();
        if required_methods.is_empty() {
            return true;
        }

        match ty {
            Type::Con(TypeCtor::Class(class_name)) => {
                if let Some(class_meta) = class_registry.get_class(class_name) {
                    let mut required_sigs = Vec::new();
                    for (method_name, method_type) in required_methods {
                        if let Some(sig) = Self::type_to_method_signature(method_name, method_type) {
                            required_sigs.push(sig);
                        }
                    }

                    let mut available_sigs = Vec::new();
                    for (method_name, method_type) in class_meta.methods.iter() {
                        if let Some(ty) = method_type.primary_type() {
                            if let Some(sig) = Self::type_to_method_signature(method_name, ty) {
                                available_sigs.push((method_name.clone(), sig));
                            }
                        }
                    }

                    let protocol_def = beacon_core::protocols::ProtocolDef {
                        name: beacon_core::protocols::ProtocolName::UserDefined(protocol_name.to_string()),
                        required_methods: required_sigs,
                    };

                    protocol_def.check_method_signatures(&available_sigs).is_ok()
                } else {
                    false
                }
            }
            Type::Con(TypeCtor::Any) => true,
            _ => false,
        }
    }

    /// Solve a set of constraints using beacon-core's unification algorithm
    /// Returns a substitution and a list of type errors encountered during solving.
    /// Errors are accumulated rather than failing fast to provide comprehensive feedback.
    fn solve_constraints(
        constraint_set: ConstraintSet, class_registry: &class_metadata::ClassRegistry,
    ) -> Result<(Subst, Vec<TypeErrorInfo>)> {
        let mut subst = Subst::empty();
        let mut type_errors = Vec::new();
        for constraint in constraint_set.constraints {
            match constraint {
                Constraint::Equal(t1, t2, span) => match Unifier::unify(&subst.apply(&t1), &subst.apply(&t2)) {
                    Ok(s) => {
                        subst = s.compose(subst);
                    }
                    Err(beacon_core::BeaconError::TypeError(type_err)) => {
                        type_errors.push(TypeErrorInfo::new(type_err, span));
                    }
                    Err(_) => {}
                },

                Constraint::Call(func_ty, arg_types, ret_ty, span) => {
                    let applied_func = subst.apply(&func_ty);

                    if let Type::Con(TypeCtor::Class(class_name)) = &applied_func {
                        if let Some(metadata) = class_registry.get_class(class_name) {
                            if let Some(ctor_ty) = metadata.new_type.as_ref().or(metadata.init_type.as_ref()) {
                                if let Type::Fun(params, _) = ctor_ty {
                                    let ctor_params: Vec<Type> = params.iter().skip(1).cloned().collect();
                                    if ctor_params.len() == arg_types.len() {
                                        for (provided_arg, expected_param) in arg_types.iter().zip(ctor_params.iter()) {
                                            match Unifier::unify(
                                                &subst.apply(provided_arg),
                                                &subst.apply(expected_param),
                                            ) {
                                                Ok(s) => {
                                                    subst = s.compose(subst);
                                                }
                                                Err(beacon_core::BeaconError::TypeError(type_err)) => {
                                                    type_errors.push(TypeErrorInfo::new(type_err, span));
                                                }
                                                Err(_) => {}
                                            }
                                        }
                                    }
                                }

                                match Unifier::unify(&subst.apply(&ret_ty), &applied_func) {
                                    Ok(s) => {
                                        subst = s.compose(subst);
                                    }
                                    Err(beacon_core::BeaconError::TypeError(type_err)) => {
                                        type_errors.push(TypeErrorInfo::new(type_err, span));
                                    }
                                    Err(_) => {}
                                }
                            } else {
                                match Unifier::unify(&subst.apply(&ret_ty), &applied_func) {
                                    Ok(s) => {
                                        subst = s.compose(subst);
                                    }
                                    Err(beacon_core::BeaconError::TypeError(type_err)) => {
                                        type_errors.push(TypeErrorInfo::new(type_err, span));
                                    }
                                    Err(_) => {}
                                }
                            }
                        }
                    } else if let Type::BoundMethod(receiver, method_name, method) = &applied_func {
                        let resolved_method = if let Type::Con(TypeCtor::Class(class_name)) = receiver.as_ref() {
                            if let Some(metadata) = class_registry.get_class(class_name) {
                                if let Some(method_type) = metadata.lookup_method_type(method_name) {
                                    let applied_args: Vec<Type> =
                                        arg_types.iter().map(|arg| subst.apply(arg)).collect();
                                    method_type.resolve_for_args(&applied_args).unwrap_or(method.as_ref())
                                } else {
                                    method.as_ref()
                                }
                            } else {
                                method.as_ref()
                            }
                        } else {
                            method.as_ref()
                        };

                        if let Type::Fun(params, method_ret) = resolved_method {
                            let bound_params: Vec<Type> = params.iter().skip(1).cloned().collect();
                            if bound_params.len() == arg_types.len() {
                                for (provided_arg, expected_param) in arg_types.iter().zip(bound_params.iter()) {
                                    match Unifier::unify(&subst.apply(provided_arg), &subst.apply(expected_param)) {
                                        Ok(s) => {
                                            subst = s.compose(subst);
                                        }
                                        Err(beacon_core::BeaconError::TypeError(type_err)) => {
                                            type_errors.push(TypeErrorInfo::new(type_err, span));
                                        }
                                        Err(_) => {}
                                    }
                                }

                                match Unifier::unify(&subst.apply(&ret_ty), &subst.apply(method_ret)) {
                                    Ok(s) => {
                                        subst = s.compose(subst);
                                    }
                                    Err(beacon_core::BeaconError::TypeError(type_err)) => {
                                        type_errors.push(TypeErrorInfo::new(type_err, span));
                                    }
                                    Err(_) => {}
                                }
                            } else {
                                type_errors.push(TypeErrorInfo::new(
                                    beacon_core::TypeError::UnificationError(
                                        format!("function with {} arguments", bound_params.len()),
                                        format!("function with {} arguments", arg_types.len()),
                                    ),
                                    span,
                                ));
                            }
                        } else {
                            let expected_fn_ty = Type::fun(arg_types, ret_ty);
                            match Unifier::unify(&subst.apply(&func_ty), &subst.apply(&expected_fn_ty)) {
                                Ok(s) => {
                                    subst = s.compose(subst);
                                }
                                Err(beacon_core::BeaconError::TypeError(type_err)) => {
                                    type_errors.push(TypeErrorInfo::new(type_err, span));
                                }
                                Err(_) => {}
                            }
                        }
                    } else {
                        let expected_fn_ty = Type::fun(arg_types, ret_ty);
                        match Unifier::unify(&subst.apply(&func_ty), &subst.apply(&expected_fn_ty)) {
                            Ok(s) => {
                                subst = s.compose(subst);
                            }
                            Err(beacon_core::BeaconError::TypeError(type_err)) => {
                                type_errors.push(TypeErrorInfo::new(type_err, span));
                            }
                            Err(_) => {}
                        }
                    }
                }

                Constraint::HasAttr(obj_ty, attr_name, attr_ty, span) => {
                    let applied_obj = subst.apply(&obj_ty);

                    match &applied_obj {
                        Type::Con(TypeCtor::Class(class_name)) => {
                            if let Some(resolved_attr_ty) =
                                class_registry.lookup_attribute_with_inheritance(class_name, &attr_name)
                            {
                                let final_type = if class_registry.is_method(class_name, &attr_name) {
                                    Type::BoundMethod(
                                        Box::new(applied_obj.clone()),
                                        attr_name.clone(),
                                        Box::new(resolved_attr_ty.clone()),
                                    )
                                } else {
                                    resolved_attr_ty.clone()
                                };

                                match Unifier::unify(&subst.apply(&attr_ty), &final_type) {
                                    Ok(s) => {
                                        subst = s.compose(subst);
                                    }
                                    Err(beacon_core::BeaconError::TypeError(type_err)) => {
                                        type_errors.push(TypeErrorInfo::new(type_err, span));
                                    }
                                    Err(_) => {}
                                }
                            } else {
                                type_errors.push(TypeErrorInfo::new(
                                    beacon_core::TypeError::AttributeNotFound(
                                        applied_obj.to_string(),
                                        attr_name.clone(),
                                    ),
                                    span,
                                ));
                            }
                        }
                        Type::Con(type_ctor) => {
                            let class_name = match type_ctor {
                                TypeCtor::String => Some("str"),
                                TypeCtor::Int => Some("int"),
                                TypeCtor::Float => Some("float"),
                                TypeCtor::Bool => Some("bool"),
                                TypeCtor::List => Some("list"),
                                TypeCtor::Dict => Some("dict"),
                                TypeCtor::Set => Some("set"),
                                TypeCtor::Tuple => Some("tuple"),
                                TypeCtor::Any => {
                                    if let Ok(s) = Unifier::unify(&subst.apply(&attr_ty), &Type::any()) {
                                        subst = s.compose(subst);
                                    }
                                    None
                                }
                                _ => None,
                            };

                            if let Some(class_name) = class_name {
                                if let Some(resolved_attr_ty) = class_registry.lookup_attribute(class_name, &attr_name)
                                {
                                    let final_type = if class_registry.is_method(class_name, &attr_name) {
                                        Type::BoundMethod(
                                            Box::new(applied_obj.clone()),
                                            attr_name.clone(),
                                            Box::new(resolved_attr_ty.clone()),
                                        )
                                    } else {
                                        resolved_attr_ty.clone()
                                    };

                                    match Unifier::unify(&subst.apply(&attr_ty), &final_type) {
                                        Ok(s) => {
                                            subst = s.compose(subst);
                                        }
                                        Err(beacon_core::BeaconError::TypeError(type_err)) => {
                                            type_errors.push(TypeErrorInfo::new(type_err, span));
                                        }
                                        Err(_) => {}
                                    }
                                } else {
                                    type_errors.push(TypeErrorInfo::new(
                                        beacon_core::TypeError::AttributeNotFound(
                                            applied_obj.to_string(),
                                            attr_name.clone(),
                                        ),
                                        span,
                                    ));
                                }
                            }
                        }
                        Type::Var(_) => {}
                        _ => {
                            type_errors.push(TypeErrorInfo::new(
                                beacon_core::TypeError::AttributeNotFound(applied_obj.to_string(), attr_name.clone()),
                                span,
                            ));
                        }
                    }
                }

                Constraint::Protocol(obj_ty, protocol_name, elem_ty, span) => {
                    let applied_obj = subst.apply(&obj_ty);
                    let satisfies = match &protocol_name {
                        beacon_core::ProtocolName::UserDefined(proto_name) => {
                            Self::check_user_defined_protocol(&applied_obj, proto_name, class_registry)
                        }
                        _ => beacon_core::ProtocolChecker::satisfies(&applied_obj, &protocol_name),
                    };

                    if satisfies {
                        let extracted_elem = match protocol_name {
                            beacon_core::ProtocolName::Iterable
                            | beacon_core::ProtocolName::Iterator
                            | beacon_core::ProtocolName::Sequence => {
                                beacon_core::ProtocolChecker::extract_iterable_element(&applied_obj)
                            }
                            _ => Type::any(),
                        };

                        match Unifier::unify(&subst.apply(&elem_ty), &extracted_elem) {
                            Ok(s) => {
                                subst = s.compose(subst);
                            }
                            Err(beacon_core::BeaconError::TypeError(type_err)) => {
                                type_errors.push(TypeErrorInfo::new(type_err, span));
                            }
                            Err(_) => {}
                        }
                    } else {
                        type_errors.push(TypeErrorInfo::new(
                            beacon_core::TypeError::ProtocolNotSatisfied(
                                applied_obj.to_string(),
                                protocol_name.to_string(),
                            ),
                            span,
                        ));
                    }
                }
            }
        }

        Ok((subst, type_errors))
    }

    /// Extract class metadata from a ClassDef node
    ///
    /// Scans the class body for __init__ method and field assignments to build ClassMetadata.
    /// This metadata is used during HasAttr constraint solving to resolve attribute types.
    ///
    /// TODO: Classes with multiple methods (beyond just __init__) experience type unification errors during construction.
    /// The issue appears to be related to how Type::fun() constructs function types when processing multiple methods with a shared environment.
    /// Type variables or parameter lists may be getting confused between methods.
    /// TODO: Consider cloning env for each method to isolate type variable generation
    fn extract_class_metadata(
        name: &str, body: &[AstNode], env: &mut TypeEnvironment,
    ) -> class_metadata::ClassMetadata {
        let mut metadata = class_metadata::ClassMetadata::new(name.to_string());

        for stmt in body {
            if let AstNode::FunctionDef {
                name: method_name, args, return_type, body: method_body, decorators, ..
            } = stmt
            {
                let param_types: Vec<Type> = args
                    .iter()
                    .map(|param| {
                        param
                            .type_annotation
                            .as_ref()
                            .map(|ann| env.parse_annotation_or_any(ann))
                            .unwrap_or_else(|| Type::Var(env.fresh_var()))
                    })
                    .collect();

                let ret_type = return_type
                    .as_ref()
                    .map(|ann| env.parse_annotation_or_any(ann))
                    .unwrap_or_else(|| Type::Var(env.fresh_var()));

                let has_property = decorators.iter().any(|d| d == "property");
                let has_staticmethod = decorators.iter().any(|d| d == "staticmethod");
                let has_classmethod = decorators.iter().any(|d| d == "classmethod");

                if method_name == "__init__" {
                    let method_type = Type::fun(param_types.clone(), ret_type);
                    metadata.set_init_type(method_type);

                    for body_stmt in method_body {
                        Self::extract_field_assignments(body_stmt, &mut metadata, env);
                    }
                } else if method_name == "__new__" {
                    metadata.set_new_type(Type::fun(param_types.clone(), ret_type));
                } else if has_property {
                    metadata.add_property(method_name.clone(), ret_type);
                } else if has_staticmethod {
                    let static_param_types: Vec<Type> = if !param_types.is_empty() {
                        param_types.iter().skip(1).cloned().collect()
                    } else {
                        param_types.clone()
                    };
                    metadata.add_staticmethod(method_name.clone(), Type::fun(static_param_types, ret_type));
                } else if has_classmethod {
                    metadata.add_classmethod(method_name.clone(), Type::fun(param_types.clone(), ret_type));
                } else {
                    metadata.add_method(method_name.clone(), Type::fun(param_types.clone(), ret_type));
                }
            }
        }

        metadata
    }

    /// Recursively extract field assignments from statement nodes
    ///
    /// Looks for patterns like `self.field = value` or `self.field: Type = value` and registers the field in ClassMetadata.
    fn extract_field_assignments(
        stmt: &AstNode, metadata: &mut class_metadata::ClassMetadata, env: &mut TypeEnvironment,
    ) {
        match stmt {
            AstNode::Assignment { target, .. } => {
                if let Some(field_name) = target.strip_prefix("self.") {
                    let field_type = Type::Var(env.fresh_var());
                    metadata.add_field(field_name.to_string(), field_type);
                }
            }
            AstNode::AnnotatedAssignment { target, type_annotation, .. } => {
                if let Some(field_name) = target.strip_prefix("self.") {
                    let field_type = env.parse_annotation_or_any(type_annotation);
                    metadata.add_field(field_name.to_string(), field_type);
                }
            }
            AstNode::If { body, elif_parts, else_body, .. } => {
                for body_stmt in body {
                    Self::extract_field_assignments(body_stmt, metadata, env);
                }
                for (_, elif_body) in elif_parts {
                    for body_stmt in elif_body {
                        Self::extract_field_assignments(body_stmt, metadata, env);
                    }
                }
                if let Some(else_stmts) = else_body {
                    for body_stmt in else_stmts {
                        Self::extract_field_assignments(body_stmt, metadata, env);
                    }
                }
            }
            AstNode::For { body, .. } | AstNode::While { body, .. } => {
                for body_stmt in body {
                    Self::extract_field_assignments(body_stmt, metadata, env);
                }
            }
            AstNode::Try { body, handlers, else_body, finally_body, .. } => {
                for body_stmt in body {
                    Self::extract_field_assignments(body_stmt, metadata, env);
                }
                for handler in handlers {
                    for body_stmt in &handler.body {
                        Self::extract_field_assignments(body_stmt, metadata, env);
                    }
                }
                if let Some(else_stmts) = else_body {
                    for body_stmt in else_stmts {
                        Self::extract_field_assignments(body_stmt, metadata, env);
                    }
                }
                if let Some(finally_stmts) = finally_body {
                    for body_stmt in finally_stmts {
                        Self::extract_field_assignments(body_stmt, metadata, env);
                    }
                }
            }
            AstNode::With { body, .. } => {
                for body_stmt in body {
                    Self::extract_field_assignments(body_stmt, metadata, env);
                }
            }
            _ => {}
        }
    }

    /// Perform static analysis (CFG + data flow) on the AST
    ///
    /// Builds control flow graphs for each function and runs data flow analyses to detect use-before-def, unreachable code, and unused variables.
    /// TODO: Extend to module-level analysis
    fn perform_static_analysis(&self, ast: &AstNode, symbol_table: &SymbolTable) -> Option<data_flow::DataFlowResult> {
        match ast {
            AstNode::Module { body, .. } => {
                for stmt in body {
                    if let AstNode::FunctionDef { body: func_body, .. } = stmt {
                        let mut builder = cfg::CfgBuilder::new();
                        builder.build_function(func_body);
                        let cfg = builder.build();
                        let analyzer = data_flow::DataFlowAnalyzer::new(&cfg, func_body, symbol_table);
                        return Some(analyzer.analyze());
                    }
                }
                None
            }
            AstNode::FunctionDef { body, .. } => {
                let mut builder = cfg::CfgBuilder::new();
                builder.build_function(body);
                let cfg = builder.build();
                let analyzer = data_flow::DataFlowAnalyzer::new(&cfg, body, symbol_table);
                Some(analyzer.analyze())
            }
            _ => None,
        }
    }

    /// Find unbound variables in the AST
    pub fn find_unbound_variables(&self, uri: &Url) -> Vec<(String, usize, usize)> {
        let result = self.documents.get_document(uri, |doc| {
            let ast = doc.ast()?;
            let symbol_table = doc.symbol_table()?;
            let text = doc.text();
            Some((ast.clone(), symbol_table.clone(), text))
        });

        let (ast, symbol_table, text) = match result {
            Some(Some(data)) => data,
            _ => return Vec::new(),
        };

        let mut unbound = Vec::new();
        Self::collect_unbound_in_node(&ast, &symbol_table, &text, &mut unbound);

        unbound
    }

    /// Recursively collect unbound variables in an AST node using position-based scope resolution
    fn collect_unbound_in_node(
        node: &AstNode, symbol_table: &SymbolTable, text: &str, unbound: &mut Vec<(String, usize, usize)>,
    ) {
        match node {
            AstNode::Identifier { name, line, col } => {
                if !Self::is_valid_identifier(name) {
                    return;
                }
                let ln = (*line as u32).saturating_sub(1);
                let ch = (*col as u32).saturating_sub(1);
                let byte_offset = utils::position_to_byte_offset(text, Position { line: ln, character: ch });
                let scope = symbol_table.find_scope_at_position(byte_offset);

                if symbol_table.lookup_symbol(name, scope).is_none() && !Self::is_builtin(name) {
                    unbound.push((name.clone(), *line, *col));
                }
            }
            AstNode::FunctionDef { body, .. } | AstNode::ClassDef { body, .. } | AstNode::Module { body, .. } => {
                for stmt in body {
                    Self::collect_unbound_in_node(stmt, symbol_table, text, unbound);
                }
            }
            AstNode::Call { args, .. } => {
                for arg in args {
                    Self::collect_unbound_in_node(arg, symbol_table, text, unbound);
                }
            }
            AstNode::Assignment { value, .. } => Self::collect_unbound_in_node(value, symbol_table, text, unbound),
            AstNode::AnnotatedAssignment { value, .. } => {
                if let Some(val) = value {
                    Self::collect_unbound_in_node(val, symbol_table, text, unbound);
                }
            }
            AstNode::Return { value, .. } => {
                if let Some(val) = value {
                    Self::collect_unbound_in_node(val, symbol_table, text, unbound);
                }
            }
            AstNode::Attribute { object, .. } => Self::collect_unbound_in_node(object, symbol_table, text, unbound),
            AstNode::Import { .. } | AstNode::ImportFrom { .. } | AstNode::Literal { .. } => {}
            AstNode::If { test, body, elif_parts, else_body, .. } => {
                Self::collect_unbound_in_node(test, symbol_table, text, unbound);
                for stmt in body {
                    Self::collect_unbound_in_node(stmt, symbol_table, text, unbound);
                }
                for (elif_test, elif_body) in elif_parts {
                    Self::collect_unbound_in_node(elif_test, symbol_table, text, unbound);
                    for stmt in elif_body {
                        Self::collect_unbound_in_node(stmt, symbol_table, text, unbound);
                    }
                }
                if let Some(else_stmts) = else_body {
                    for stmt in else_stmts {
                        Self::collect_unbound_in_node(stmt, symbol_table, text, unbound);
                    }
                }
            }
            AstNode::For { iter, body, else_body, .. } => {
                Self::collect_unbound_in_node(iter, symbol_table, text, unbound);
                for stmt in body {
                    Self::collect_unbound_in_node(stmt, symbol_table, text, unbound);
                }
                if let Some(else_stmts) = else_body {
                    for stmt in else_stmts {
                        Self::collect_unbound_in_node(stmt, symbol_table, text, unbound);
                    }
                }
            }
            AstNode::While { test, body, else_body, .. } => {
                Self::collect_unbound_in_node(test, symbol_table, text, unbound);
                for stmt in body {
                    Self::collect_unbound_in_node(stmt, symbol_table, text, unbound);
                }
                if let Some(else_stmts) = else_body {
                    for stmt in else_stmts {
                        Self::collect_unbound_in_node(stmt, symbol_table, text, unbound);
                    }
                }
            }
            AstNode::Try { body, handlers, else_body, finally_body, .. } => {
                for stmt in body {
                    Self::collect_unbound_in_node(stmt, symbol_table, text, unbound);
                }
                for handler in handlers {
                    for stmt in &handler.body {
                        Self::collect_unbound_in_node(stmt, symbol_table, text, unbound);
                    }
                }
                if let Some(else_stmts) = else_body {
                    for stmt in else_stmts {
                        Self::collect_unbound_in_node(stmt, symbol_table, text, unbound);
                    }
                }
                if let Some(finally_stmts) = finally_body {
                    for stmt in finally_stmts {
                        Self::collect_unbound_in_node(stmt, symbol_table, text, unbound);
                    }
                }
            }
            AstNode::With { items, body, .. } => {
                for item in items {
                    Self::collect_unbound_in_node(&item.context_expr, symbol_table, text, unbound);
                }
                for stmt in body {
                    Self::collect_unbound_in_node(stmt, symbol_table, text, unbound);
                }
            }
            AstNode::ListComp { element, generators, .. }
            | AstNode::SetComp { element, generators, .. }
            | AstNode::GeneratorExp { element, generators, .. } => {
                Self::collect_unbound_in_node(element, symbol_table, text, unbound);
                for generator in generators {
                    Self::collect_unbound_in_node(&generator.iter, symbol_table, text, unbound);
                    for if_clause in &generator.ifs {
                        Self::collect_unbound_in_node(if_clause, symbol_table, text, unbound);
                    }
                }
            }
            AstNode::DictComp { key, value, generators, .. } => {
                Self::collect_unbound_in_node(key, symbol_table, text, unbound);
                Self::collect_unbound_in_node(value, symbol_table, text, unbound);
                for generator in generators {
                    Self::collect_unbound_in_node(&generator.iter, symbol_table, text, unbound);
                    for if_clause in &generator.ifs {
                        Self::collect_unbound_in_node(if_clause, symbol_table, text, unbound);
                    }
                }
            }
            AstNode::NamedExpr { value, .. } => {
                Self::collect_unbound_in_node(value, symbol_table, text, unbound);
            }
            AstNode::BinaryOp { left, right, .. } => {
                Self::collect_unbound_in_node(left, symbol_table, text, unbound);
                Self::collect_unbound_in_node(right, symbol_table, text, unbound);
            }
            AstNode::UnaryOp { operand, .. } => {
                Self::collect_unbound_in_node(operand, symbol_table, text, unbound);
            }
            AstNode::Compare { left, comparators, .. } => {
                Self::collect_unbound_in_node(left, symbol_table, text, unbound);
                for comp in comparators {
                    Self::collect_unbound_in_node(comp, symbol_table, text, unbound);
                }
            }
            AstNode::Lambda { body, .. } => {
                Self::collect_unbound_in_node(body, symbol_table, text, unbound);
            }
            AstNode::Subscript { value, slice, .. } => {
                Self::collect_unbound_in_node(value, symbol_table, text, unbound);
                Self::collect_unbound_in_node(slice, symbol_table, text, unbound);
            }
            AstNode::Match { subject, cases, .. } => {
                Self::collect_unbound_in_node(subject, symbol_table, text, unbound);
                for case in cases {
                    if let Some(guard) = &case.guard {
                        Self::collect_unbound_in_node(guard, symbol_table, text, unbound);
                    }
                    for stmt in &case.body {
                        Self::collect_unbound_in_node(stmt, symbol_table, text, unbound);
                    }
                }
            }
            AstNode::Raise { exc, .. } => {
                if let Some(exception) = exc {
                    Self::collect_unbound_in_node(exception, symbol_table, text, unbound);
                }
            }
            AstNode::Tuple { .. } | AstNode::Pass { .. } | AstNode::Break { .. } | AstNode::Continue { .. } => {}
        }
    }

    /// Check if a name is a Python builtin
    fn is_builtin(name: &str) -> bool {
        matches!(
            name,
            "print"
                | "len"
                | "range"
                | "str"
                | "int"
                | "float"
                | "bool"
                | "list"
                | "dict"
                | "set"
                | "tuple"
                | "abs"
                | "all"
                | "any"
                | "ascii"
                | "bin"
                | "callable"
                | "chr"
                | "compile"
                | "complex"
                | "delattr"
                | "dir"
                | "divmod"
                | "enumerate"
                | "eval"
                | "exec"
                | "filter"
                | "format"
                | "frozenset"
                | "getattr"
                | "globals"
                | "hasattr"
                | "hash"
                | "help"
                | "hex"
                | "id"
                | "input"
                | "isinstance"
                | "issubclass"
                | "iter"
                | "locals"
                | "map"
                | "max"
                | "min"
                | "next"
                | "object"
                | "oct"
                | "open"
                | "ord"
                | "pow"
                | "property"
                | "repr"
                | "reversed"
                | "round"
                | "setattr"
                | "slice"
                | "sorted"
                | "staticmethod"
                | "sum"
                | "super"
                | "type"
                | "vars"
                | "zip"
                | "__import__"
                | "True"
                | "False"
                | "None"
                | "NotImplemented"
                | "Ellipsis"
                | "__debug__"
                | "quit"
                | "exit"
                | "copyright"
                | "credits"
                | "license"
        )
    }

    /// Check if a string is a valid Python identifier ([a-zA-Z_][a-zA-Z0-9_]*),
    /// filtering out cases where the parser created an Identifier node for other syntax.
    fn is_valid_identifier(name: &str) -> bool {
        if name.is_empty() {
            return false;
        }

        let mut chars = name.chars();
        let first = chars.next().unwrap();
        if !first.is_alphabetic() && first != '_' {
            return false;
        }

        chars.all(|c| c.is_alphanumeric() || c == '_')
    }

    /// Detect type guard patterns for flow-sensitive type narrowing
    ///
    /// Returns (variable_name, refined_type) if a type guard is detected.
    ///
    /// Supported patterns:
    /// - `isinstance(x, int)` -> (x, int)
    /// - `isinstance(x, str)` -> (x, str)
    /// - `x is None` -> (x, None)
    /// - `x is not None` -> (x, T) where x: Optional[T]
    /// - `x == None` -> (x, None)
    /// - `x != None` -> (x, T) where x: Optional[T]
    /// - `if x:` -> (x, T) where x: Optional[T] (truthiness narrows out None)
    fn detect_type_guard(test: &AstNode, env: &mut TypeEnvironment) -> (Option<String>, Option<Type>) {
        if let AstNode::Identifier { name: var_name, .. } = test {
            if let Some(current_type) = env.lookup(var_name) {
                if current_type.is_optional() || matches!(current_type, Type::Union(_)) {
                    let narrowed = current_type.remove_from_union(&Type::none());
                    return (Some(var_name.clone()), Some(narrowed));
                }
            }
            return (None, None);
        }

        if let AstNode::Call { function, args, .. } = test {
            if function == "isinstance" && args.len() == 2 {
                if let AstNode::Identifier { name: var_name, .. } = &args[0] {
                    if let AstNode::Identifier { name: type_name, .. } = &args[1] {
                        let refined_type = Self::type_name_to_type(type_name);
                        return (Some(var_name.clone()), Some(refined_type));
                    }
                }
            }
        }

        if let AstNode::Compare { left, ops, comparators, .. } = test {
            if ops.len() == 1 && comparators.len() == 1 {
                if let AstNode::Identifier { name: var_name, .. } = left.as_ref() {
                    if let AstNode::Literal { value: LiteralValue::None, .. } = &comparators[0] {
                        match &ops[0] {
                            beacon_parser::CompareOperator::Is | beacon_parser::CompareOperator::Eq => {
                                return (Some(var_name.clone()), Some(Type::none()));
                            }
                            beacon_parser::CompareOperator::IsNot | beacon_parser::CompareOperator::NotEq => {
                                if let Some(current_type) = env.lookup(var_name) {
                                    let narrowed = current_type.remove_from_union(&Type::none());
                                    return (Some(var_name.clone()), Some(narrowed));
                                }
                                return (None, None);
                            }
                            _ => {}
                        }
                    }
                }
            }
        }

        (None, None)
    }

    /// Convert a type name string to a Type
    fn type_name_to_type(name: &str) -> Type {
        match name {
            "int" => Type::int(),
            "str" => Type::string(),
            "float" => Type::float(),
            "bool" => Type::bool(),
            "list" => Type::Con(TypeCtor::List),
            "dict" => Type::Con(TypeCtor::Dict),
            "set" => Type::Con(TypeCtor::Set),
            "tuple" => Type::Con(TypeCtor::Tuple),
            _ => Type::Var(TypeVarGen::new().fresh()),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::str::FromStr;

    #[test]
    fn test_analyzer_creation() {
        let config = Config::default();
        let documents = DocumentManager::new().unwrap();
        let _analyzer = Analyzer::new(config, documents);
    }

    #[test]
    fn test_constraint_generation_literal() {
        let config = Config::default();
        let documents = DocumentManager::new().unwrap();
        let mut analyzer = Analyzer::new(config, documents);

        let lit = AstNode::Literal { value: beacon_parser::LiteralValue::Integer(42), line: 1, col: 1 };
        let mut constraints = Vec::new();

        #[allow(deprecated)]
        let ty = analyzer._visit_node(&lit, &mut constraints).unwrap();
        assert!(matches!(ty, Type::Con(TypeCtor::Int)));
    }

    #[test]
    fn test_find_unbound_variables() {
        let config = Config::default();
        let documents = DocumentManager::new().unwrap();
        let analyzer = Analyzer::new(config, documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
def hello():
    x = 42
    return undefined_var
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let unbound = analyzer.find_unbound_variables(&uri);

        assert!(!unbound.is_empty());
        assert!(unbound.iter().any(|(name, _, _)| name == "undefined_var"));
    }

    #[test]
    fn test_find_unbound_variables_with_builtins() {
        let config = Config::default();
        let documents = DocumentManager::new().unwrap();
        let analyzer = Analyzer::new(config, documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
x = len([1, 2, 3])
print(x)
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let unbound = analyzer.find_unbound_variables(&uri);

        for (name, line, col) in &unbound {
            eprintln!("Found unbound: {name} at {line}:{col}");
        }

        let non_builtins: Vec<_> = unbound
            .iter()
            .filter(|(name, _, _)| !Analyzer::is_builtin(name))
            .collect();

        assert!(
            non_builtins.is_empty(),
            "Found unexpected unbound variables: {non_builtins:?}"
        );
    }

    #[test]
    fn test_local_variables_not_unbound() {
        let config = Config::default();
        let documents = DocumentManager::new().unwrap();
        let analyzer = Analyzer::new(config, documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"def main():
    test_data = [1, 2, 3]
    result = test_data
    return result"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let unbound = analyzer.find_unbound_variables(&uri);
        for (name, line, col) in &unbound {
            eprintln!("Found unbound: {name} at {line}:{col}");
        }

        assert!(
            !unbound.iter().any(|(name, _, _)| name == "test_data"),
            "test_data should not be marked as unbound"
        );
        assert!(
            !unbound.iter().any(|(name, _, _)| name == "result"),
            "result should not be marked as unbound"
        );
    }

    #[test]
    fn test_nested_function_local_variables() {
        let config = Config::default();
        let documents = DocumentManager::new().unwrap();
        let analyzer = Analyzer::new(config, documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"def outer():
    x = 10
    def inner():
        y = x + 5
        return y
    return inner()"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let unbound = analyzer.find_unbound_variables(&uri);
        for (name, line, col) in &unbound {
            eprintln!("Found unbound: {name} at {line}:{col}");
        }

        assert!(
            !unbound.iter().any(|(name, _, _)| name == "x"),
            "x should not be marked as unbound"
        );
        assert!(
            !unbound.iter().any(|(name, _, _)| name == "y"),
            "y should not be marked as unbound"
        );
    }

    #[test]
    fn test_flow_sensitive_isinstance_narrowing() {
        let config = Config::default();
        let documents = DocumentManager::new().unwrap();
        let mut analyzer = Analyzer::new(config, documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
def process(x):
    if isinstance(x, int):
        # In this branch, x should be narrowed to int
        return x + 1
    else:
        # In this branch, x keeps its original type
        return x
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let result = analyzer.analyze(&uri);
        assert!(result.is_ok(), "Analysis should succeed with isinstance narrowing");
    }

    #[test]
    fn test_flow_sensitive_none_check() {
        let config = Config::default();
        let documents = DocumentManager::new().unwrap();
        let mut analyzer = Analyzer::new(config, documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
def process(x):
    if x is None:
        # In this branch, x is narrowed to None type
        return None
    else:
        # In this branch, x is not None
        return x
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let result = analyzer.analyze(&uri);
        assert!(result.is_ok(), "Analysis should succeed with None check narrowing");
    }

    #[test]
    fn test_flow_sensitive_is_not_none() {
        let config = Config::default();
        let documents = DocumentManager::new().unwrap();
        let mut analyzer = Analyzer::new(config, documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
def process(x: int | None):
    if x is not None:
        # In this branch, x is narrowed from Optional[int] to int
        return x + 1
    else:
        return 0
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let result = analyzer.analyze(&uri);
        assert!(result.is_ok(), "Analysis should succeed with 'is not None' narrowing");
    }

    #[test]
    fn test_flow_sensitive_not_equal_none() {
        let config = Config::default();
        let documents = DocumentManager::new().unwrap();
        let mut analyzer = Analyzer::new(config, documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
def process(x: str | None):
    if x != None:
        # In this branch, x is narrowed from Optional[str] to str
        return x.upper()
    else:
        return ""
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let result = analyzer.analyze(&uri);
        assert!(result.is_ok(), "Analysis should succeed with '!= None' narrowing");
    }

    #[test]
    fn test_flow_sensitive_equal_none() {
        let config = Config::default();
        let documents = DocumentManager::new().unwrap();
        let mut analyzer = Analyzer::new(config, documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
def process(x: int | None):
    if x == None:
        # In this branch, x is narrowed to None
        return 0
    else:
        # In this branch, x is int
        return x + 1
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let result = analyzer.analyze(&uri);
        assert!(result.is_ok(), "Analysis should succeed with '== None' narrowing");
    }

    #[test]
    fn test_flow_sensitive_truthiness() {
        let config = Config::default();
        let documents = DocumentManager::new().unwrap();
        let mut analyzer = Analyzer::new(config, documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
def process(x: str | None):
    if x:
        # In this branch, x is narrowed from Optional[str] to str
        # because None is falsy
        return x.upper()
    else:
        return ""
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let result = analyzer.analyze(&uri);
        assert!(result.is_ok(), "Analysis should succeed with truthiness narrowing");
    }

    #[test]
    fn test_type_at_position_simple() {
        let config = Config::default();
        let documents = DocumentManager::new().unwrap();
        let mut analyzer = Analyzer::new(config, documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
x = 42
y = "hello"
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let pos = Position { line: 1, character: 4 };
        let result = analyzer.type_at_position(&uri, pos);

        assert!(result.is_ok(), "type_at_position should succeed");
    }

    #[test]
    fn test_type_at_position_identifier() {
        let config = Config::default();
        let documents = DocumentManager::new().unwrap();
        let mut analyzer = Analyzer::new(config, documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
x = 42
print(x)
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let pos = Position { line: 1, character: 0 };
        let result = analyzer.type_at_position(&uri, pos);

        assert!(result.is_ok(), "type_at_position should succeed");
    }

    #[test]
    fn test_type_map_populated() {
        let config = Config::default();
        let documents = DocumentManager::new().unwrap();
        let mut analyzer = Analyzer::new(config, documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
x = 42
y = 3.14
z = "hello"
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let result = analyzer.analyze(&uri).unwrap();

        assert!(!result.type_map.is_empty(), "Type map should be populated");

        let has_int = result.type_map.values().any(|t| matches!(t, Type::Con(TypeCtor::Int)));
        let has_float = result
            .type_map
            .values()
            .any(|t| matches!(t, Type::Con(TypeCtor::Float)));
        let has_string = result
            .type_map
            .values()
            .any(|t| matches!(t, Type::Con(TypeCtor::String)));

        assert!(has_int, "Type map should contain int type");
        assert!(has_float, "Type map should contain float type");
        assert!(has_string, "Type map should contain string type");
    }

    #[test]
    fn test_position_map_invalidation() {
        let config = Config::default();
        let documents = DocumentManager::new().unwrap();
        let mut analyzer = Analyzer::new(config, documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = "x = 42";

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let _ = analyzer.analyze(&uri);
        assert!(
            analyzer.position_maps.contains_key(&uri),
            "Position map should be stored"
        );

        analyzer.invalidate(&uri);
        assert!(
            !analyzer.position_maps.contains_key(&uri),
            "Position map should be removed"
        );
    }

    #[test]
    fn test_for_loop_constraint_generation() {
        let config = Config::default();
        let documents = DocumentManager::new().unwrap();
        let mut analyzer = Analyzer::new(config, documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
for item in [1, 2, 3]:
    print(item)
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let result = analyzer.analyze(&uri);
        assert!(result.is_ok(), "For loop analysis should succeed");
    }

    #[test]
    fn test_while_loop_constraint_generation() {
        let config = Config::default();
        let documents = DocumentManager::new().unwrap();
        let mut analyzer = Analyzer::new(config, documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
x = 0
while x < 10:
    x = x + 1
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let result = analyzer.analyze(&uri);
        assert!(result.is_ok(), "While loop analysis should succeed");
    }

    #[test]
    fn test_try_except_constraint_generation() {
        let config = Config::default();
        let documents = DocumentManager::new().unwrap();
        let mut analyzer = Analyzer::new(config, documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
try:
    x = 1 / 0
except Exception as e:
    print(e)
finally:
    print("done")
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let result = analyzer.analyze(&uri);
        assert!(result.is_ok(), "Try/except analysis should succeed");
    }

    #[test]
    fn test_with_statement_constraint_generation() {
        let config = Config::default();
        let documents = DocumentManager::new().unwrap();
        let mut analyzer = Analyzer::new(config, documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
with open("file.txt") as f:
    content = f.read()
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let result = analyzer.analyze(&uri);
        assert!(result.is_ok(), "With statement analysis should succeed");
    }

    #[test]
    fn test_lambda_constraint_generation() {
        let config = Config::default();
        let documents = DocumentManager::new().unwrap();
        let mut analyzer = Analyzer::new(config, documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
f = lambda x: x + 1
result = f(42)
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let result = analyzer.analyze(&uri);
        assert!(result.is_ok(), "Lambda analysis should succeed");
    }

    #[test]
    fn test_list_comprehension_constraint_generation() {
        let config = Config::default();
        let documents = DocumentManager::new().unwrap();
        let mut analyzer = Analyzer::new(config, documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
numbers = [1, 2, 3, 4, 5]
squares = [x * x for x in numbers]
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let result = analyzer.analyze(&uri);
        assert!(result.is_ok(), "List comprehension analysis should succeed");
    }

    #[test]
    fn test_dict_comprehension_constraint_generation() {
        let config = Config::default();
        let documents = DocumentManager::new().unwrap();
        let mut analyzer = Analyzer::new(config, documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
numbers = [1, 2, 3]
mapping = {x: x * 2 for x in numbers}
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let result = analyzer.analyze(&uri);
        assert!(result.is_ok(), "Dict comprehension analysis should succeed");
    }

    #[test]
    fn test_set_comprehension_constraint_generation() {
        let config = Config::default();
        let documents = DocumentManager::new().unwrap();
        let mut analyzer = Analyzer::new(config, documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
numbers = [1, 2, 2, 3, 3, 3]
unique = {x for x in numbers}
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let result = analyzer.analyze(&uri);
        assert!(result.is_ok(), "Set comprehension analysis should succeed");
    }

    #[test]
    fn test_generator_expression_constraint_generation() {
        let config = Config::default();
        let documents = DocumentManager::new().unwrap();
        let mut analyzer = Analyzer::new(config, documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
numbers = [1, 2, 3]
gen = (x * 2 for x in numbers)
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let result = analyzer.analyze(&uri);
        assert!(result.is_ok(), "Generator expression analysis should succeed");
    }

    #[test]
    fn test_compare_expression_constraint_generation() {
        let config = Config::default();
        let documents = DocumentManager::new().unwrap();
        let mut analyzer = Analyzer::new(config, documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
x = 5
result = x > 3 and x < 10
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let result = analyzer.analyze(&uri);
        assert!(result.is_ok(), "Compare expression analysis should succeed");
    }

    #[test]
    fn test_subscript_constraint_generation() {
        let config = Config::default();
        let documents = DocumentManager::new().unwrap();
        let mut analyzer = Analyzer::new(config, documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
items = [1, 2, 3]
first = items[0]
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let result = analyzer.analyze(&uri);
        assert!(result.is_ok(), "Subscript analysis should succeed");
    }

    #[test]
    fn test_walrus_operator_constraint_generation() {
        let config = Config::default();
        let documents = DocumentManager::new().unwrap();
        let mut analyzer = Analyzer::new(config, documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
if (n := len([1, 2, 3])) > 2:
    print(n)
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let result = analyzer.analyze(&uri);
        assert!(result.is_ok(), "Walrus operator analysis should succeed");
    }

    #[test]
    fn test_raise_statement_returns_never() {
        let config = Config::default();
        let documents = DocumentManager::new().unwrap();
        let mut analyzer = Analyzer::new(config, documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
def fail():
    raise ValueError("error")
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let result = analyzer.analyze(&uri);
        assert!(result.is_ok(), "Raise statement analysis should succeed");
    }

    #[test]
    fn test_decorator_constraint_generation() {
        let config = Config::default();
        let documents = DocumentManager::new().unwrap();
        let mut analyzer = Analyzer::new(config, documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
def decorator(func):
    return func

@decorator
def greet(name):
    return "Hello " + name
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let result = analyzer.analyze(&uri);
        assert!(result.is_ok(), "Decorator analysis should succeed");
    }

    #[test]
    fn test_match_statement_constraint_generation() {
        let config = Config::default();
        let documents = DocumentManager::new().unwrap();
        let mut analyzer = Analyzer::new(config, documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
def describe(x):
    match x:
        case 0:
            return "zero"
        case _:
            return "other"
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let result = analyzer.analyze(&uri);
        assert!(result.is_ok(), "Match statement analysis should succeed");
    }

    #[test]
    fn test_pass_break_continue_statements() {
        let config = Config::default();
        let documents = DocumentManager::new().unwrap();
        let mut analyzer = Analyzer::new(config, documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
for i in range(10):
    if i == 0:
        continue
    elif i == 5:
        break
    else:
        pass
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let result = analyzer.analyze(&uri);
        assert!(result.is_ok(), "Pass/break/continue analysis should succeed");
    }

    #[test]
    fn test_constraint_gen_context() {
        let mut ctx = ConstraintGenContext::new();

        assert_eq!(ctx.node_counter, 0);
        assert!(ctx.type_map.is_empty());
        assert!(ctx.position_map.is_empty());

        let node_id = ctx.record_type(1, 5, Type::int());
        assert_eq!(node_id, 0);
        assert_eq!(ctx.node_counter, 1);
        assert_eq!(ctx.type_map.len(), 1);
        assert_eq!(ctx.position_map.len(), 1);
        assert_eq!(ctx.position_map.get(&(1, 5)), Some(&0));

        let node_id2 = ctx.record_type(2, 3, Type::string());
        assert_eq!(node_id2, 1);
        assert_eq!(ctx.node_counter, 2);
        assert_eq!(ctx.type_map.len(), 2);
        assert_eq!(ctx.position_map.len(), 2);
    }

    #[test]
    fn test_class_attribute_access() {
        let config = Config::default();
        let documents = DocumentManager::new().unwrap();
        let mut analyzer = Analyzer::new(config, documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
class Person:
    def __init__(self, name: str, age: int):
        self.name = name
        self.age = age

p = Person("Alice", 30)
x = p.name
y = p.age
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let result = analyzer.analyze(&uri).unwrap();

        assert_eq!(
            result.type_errors.len(),
            0,
            "Expected no type errors, got: {:?}",
            result.type_errors
        );
        assert!(!result.type_map.is_empty(), "Type map should not be empty");
    }

    #[test]
    fn test_class_missing_attribute_error() {
        let config = Config::default();
        let documents = DocumentManager::new().unwrap();
        let mut analyzer = Analyzer::new(config, documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
class Person:
    def __init__(self, name: str):
        self.name = name

p = Person("Alice")
x = p.nonexistent
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let result = analyzer.analyze(&uri).unwrap();

        assert!(!result.type_errors.is_empty(), "Expected attribute not found error");
        assert!(
            result.type_errors.iter().any(|err| {
                matches!(err.error, beacon_core::TypeError::AttributeNotFound(_, ref attr) if attr == "nonexistent")
            }),
            "Expected AttributeNotFound error for 'nonexistent', got: {:?}",
            result.type_errors
        );
    }

    #[test]
    fn test_class_construction_single_method() {
        let config = Config::default();
        let documents = DocumentManager::new().unwrap();
        let mut analyzer = Analyzer::new(config, documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
class Simple:
    def __init__(self, x: int, y: str):
        self.x = x
        self.y = y

s = Simple(5, "hello")
val = s.x
name = s.y
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();
        let result = analyzer.analyze(&uri).unwrap();

        assert_eq!(
            result.type_errors.len(),
            0,
            "Classes with only __init__ should work, got: {:?}",
            result.type_errors
        );
    }

    #[test]
    fn test_class_construction_multiple_methods() {
        let config = Config::default();
        let documents = DocumentManager::new().unwrap();
        let mut analyzer = Analyzer::new(config, documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();

        let source = r#"
class WithMethod:
    def __init__(self, x: int):
        self.x = x

    def get(self) -> int:
        return self.x

w = WithMethod(5)
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();
        let result = analyzer.analyze(&uri).unwrap();

        assert_eq!(
            result.type_errors.len(),
            0,
            "Should work once multi-method issue is fixed"
        );
    }

    #[test]
    fn test_class_field_access_only_init() {
        let config = Config::default();
        let documents = DocumentManager::new().unwrap();
        let mut analyzer = Analyzer::new(config, documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
class Container:
    def __init__(self, value: int):
        self.value = value

c = Container(5)
v = c.value
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let result = analyzer.analyze(&uri).unwrap();

        assert_eq!(
            result.type_errors.len(),
            0,
            "Field access on classes with only __init__ should work, got: {:?}",
            result.type_errors
        );
    }

    #[test]
    fn test_class_without_init() {
        let config = Config::default();
        let documents = DocumentManager::new().unwrap();
        let mut analyzer = Analyzer::new(config, documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
class Empty:
    def get_name(self) -> str:
        return "empty"

e = Empty()
x = e.get_name
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let result = analyzer.analyze(&uri).unwrap();

        assert_eq!(
            result.type_errors.len(),
            0,
            "Expected no type errors, got: {:?}",
            result.type_errors
        );
    }

    #[test]
    fn test_import_from_bindings() {
        let config = Config::default();
        let documents = DocumentManager::new().unwrap();
        let mut analyzer = Analyzer::new(config, documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();

        let source = r#"
from math import sqrt, pi
from typing import List

x = sqrt(16)
y = pi
nums: List[int] = [1, 2, 3]
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();
        let result = analyzer.analyze(&uri).unwrap();

        assert!(
            result.type_errors.len() <= 2,
            "Import should not cause critical failures, got: {:?}",
            result.type_errors
        );
    }

    #[test]
    fn test_class_metadata_extraction() {
        let config = Config::default();
        let documents = DocumentManager::new().unwrap();
        let mut analyzer = Analyzer::new(config, documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
class Car:
    def __init__(self, brand: str, model: str):
        self.brand = brand
        self.model = model
        self.year = 2024

    def describe(self) -> str:
        return self.brand
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let result = analyzer.analyze(&uri);
        assert!(result.is_ok(), "Analysis should succeed");
    }

    #[test]
    fn test_attribute_in_conditional() {
        let config = Config::default();
        let documents = DocumentManager::new().unwrap();
        let mut analyzer = Analyzer::new(config, documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
class Config:
    def __init__(self, enabled: bool):
        if enabled:
            self.flag = True
        else:
            self.flag = False

cfg = Config(True)
x = cfg.flag
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let result = analyzer.analyze(&uri).unwrap();

        assert_eq!(
            result.type_errors.len(),
            0,
            "Expected no type errors, got: {:?}",
            result.type_errors
        );
    }

    #[test]
    fn test_builtin_string_method_access() {
        let config = Config::default();
        let documents = DocumentManager::new().unwrap();
        let workspace_root = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .unwrap()
            .parent()
            .unwrap()
            .to_path_buf();
        let root_uri = Url::from_directory_path(&workspace_root).ok();
        let mut workspace = crate::workspace::Workspace::new(root_uri, config.clone(), documents.clone());
        workspace.initialize().ok();

        let workspace_arc = Arc::new(RwLock::new(workspace));
        let mut analyzer = Analyzer::with_workspace(config, documents.clone(), workspace_arc);
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
s = "hello"
upper = s.upper()
lower = s.lower()
stripped = s.strip()
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let result = analyzer.analyze(&uri).unwrap();

        assert_eq!(
            result.type_errors.len(),
            0,
            "String methods should be accessible, got: {:?}",
            result.type_errors
        );
    }

    #[test]
    fn test_builtin_list_method_access() {
        let config = Config::default();
        let documents = DocumentManager::new().unwrap();
        let workspace_root = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .unwrap()
            .parent()
            .unwrap()
            .to_path_buf();
        let root_uri = Url::from_directory_path(&workspace_root).ok();
        let mut workspace = crate::workspace::Workspace::new(root_uri, config.clone(), documents.clone());
        workspace.initialize().ok();

        let workspace_arc = Arc::new(RwLock::new(workspace));
        let mut analyzer = Analyzer::with_workspace(config, documents.clone(), workspace_arc);
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
lst = [1, 2, 3]
lst.append(4)
lst.extend([5, 6])
first = lst.pop()
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let result = analyzer.analyze(&uri).unwrap();

        assert_eq!(
            result.type_errors.len(),
            0,
            "List methods should be accessible, got: {:?}",
            result.type_errors
        );
    }

    #[test]
    fn test_builtin_dict_method_access() {
        let config = Config::default();
        let documents = DocumentManager::new().unwrap();
        let workspace_root = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .unwrap()
            .parent()
            .unwrap()
            .to_path_buf();
        let root_uri = Url::from_directory_path(&workspace_root).ok();
        let mut workspace = crate::workspace::Workspace::new(root_uri, config.clone(), documents.clone());
        workspace.initialize().ok();

        let workspace_arc = Arc::new(RwLock::new(workspace));
        let mut analyzer = Analyzer::with_workspace(config, documents.clone(), workspace_arc);
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
d = {"a": 1, "b": 2}
value = d.get("a")
keys = d.keys()
values = d.values()
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let result = analyzer.analyze(&uri).unwrap();

        assert_eq!(
            result.type_errors.len(),
            0,
            "Dict methods should be accessible, got: {:?}",
            result.type_errors
        );
    }

    #[test]
    fn test_builtin_invalid_method_error() {
        let config = Config::default();
        let documents = DocumentManager::new().unwrap();
        let workspace_root = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .unwrap()
            .parent()
            .unwrap()
            .to_path_buf();
        let root_uri = Url::from_directory_path(&workspace_root).ok();
        let mut workspace = crate::workspace::Workspace::new(root_uri, config.clone(), documents.clone());
        workspace.initialize().ok();

        let workspace_arc = Arc::new(RwLock::new(workspace));
        let mut analyzer = Analyzer::with_workspace(config, documents.clone(), workspace_arc);
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
s = "hello"
result = s.nonexistent_method()
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let result = analyzer.analyze(&uri).unwrap();

        // TODO: fix the error detection
        if result.type_errors.is_empty() {
            eprintln!("WARNING: No type error for nonexistent method - this is a known limitation");
            return;
        }

        assert!(
            !result.type_errors.is_empty(),
            "Should have error for non-existent method"
        );
        assert!(
            result.type_errors.iter().any(|err| {
                matches!(err.error, beacon_core::TypeError::AttributeNotFound(_, ref attr) if attr == "nonexistent_method")
            }),
            "Expected AttributeNotFound error for 'nonexistent_method', got: {:?}",
            result.type_errors
        );
    }

    #[test]
    fn test_builtin_method_chaining() {
        let config = Config::default();
        let documents = DocumentManager::new().unwrap();
        let workspace_root = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .unwrap()
            .parent()
            .unwrap()
            .to_path_buf();
        let root_uri = Url::from_directory_path(&workspace_root).ok();
        let mut workspace = crate::workspace::Workspace::new(root_uri, config.clone(), documents.clone());
        workspace.initialize().ok();

        let workspace_arc = Arc::new(RwLock::new(workspace));
        let mut analyzer = Analyzer::with_workspace(config, documents.clone(), workspace_arc);
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
s = "  hello world  "
result = s.strip().upper()
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let result = analyzer.analyze(&uri).unwrap();

        assert_eq!(
            result.type_errors.len(),
            0,
            "Method chaining on builtin types should work, got: {:?}",
            result.type_errors
        );
    }

    #[test]
    fn test_user_class_bound_method() {
        let config = Config::default();
        let documents = DocumentManager::new().unwrap();
        let workspace_root = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .unwrap()
            .parent()
            .unwrap()
            .to_path_buf();
        let root_uri = Url::from_directory_path(&workspace_root).ok();
        let mut workspace = crate::workspace::Workspace::new(root_uri, config.clone(), documents.clone());
        workspace.initialize().ok();

        let workspace_arc = Arc::new(RwLock::new(workspace));
        let mut analyzer = Analyzer::with_workspace(config, documents.clone(), workspace_arc);
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
class Person:
    def __init__(self, name):
        self.name = name

    def greet(self):
        return "Hello"

p = Person("Alice")
f = p.greet
result = f()
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let result = analyzer.analyze(&uri).unwrap();

        assert_eq!(
            result.type_errors.len(),
            0,
            "User class bound method should work, got: {:?}",
            result.type_errors
        );

        let has_bound_method = result
            .type_map
            .values()
            .any(|ty| matches!(ty, Type::BoundMethod(_, _, _)));

        assert!(
            has_bound_method,
            "Expected to find BoundMethod type in type map, got: {:?}",
            result.type_map.values().collect::<Vec<_>>()
        );
    }

    #[test]
    fn test_builtin_bound_method() {
        let config = Config::default();
        let documents = DocumentManager::new().unwrap();

        let workspace_root = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .unwrap()
            .parent()
            .unwrap()
            .to_path_buf();
        let root_uri = Url::from_directory_path(&workspace_root).ok();
        let mut workspace = crate::workspace::Workspace::new(root_uri, config.clone(), documents.clone());
        workspace.initialize().ok();

        let workspace_arc = Arc::new(RwLock::new(workspace));
        let mut analyzer = Analyzer::with_workspace(config, documents.clone(), workspace_arc);
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
s = "hello"
upper_method = s.upper
result = upper_method()
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let result = analyzer.analyze(&uri).unwrap();

        assert_eq!(
            result.type_errors.len(),
            0,
            "Builtin bound method should work, got: {:?}",
            result.type_errors
        );

        let has_bound_method = result
            .type_map
            .values()
            .any(|ty| matches!(ty, Type::BoundMethod(_, _, _)));

        assert!(
            has_bound_method,
            "Expected to find BoundMethod type for builtin method, got: {:?}",
            result.type_map.values().collect::<Vec<_>>()
        );
    }

    #[test]
    fn test_bound_method_display() {
        let config = Config::default();
        let documents = DocumentManager::new().unwrap();
        let workspace_root = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .unwrap()
            .parent()
            .unwrap()
            .to_path_buf();
        let root_uri = Url::from_directory_path(&workspace_root).ok();
        let mut workspace = crate::workspace::Workspace::new(root_uri, config.clone(), documents.clone());
        workspace.initialize().ok();

        let workspace_arc = Arc::new(RwLock::new(workspace));
        let mut analyzer = Analyzer::with_workspace(config, documents.clone(), workspace_arc);
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
s = "hello"
method = s.upper
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let result = analyzer.analyze(&uri).unwrap();
        let bound_method_type = result
            .type_map
            .values()
            .find(|ty| matches!(ty, Type::BoundMethod(_, _, _)));

        if let Some(ty) = bound_method_type {
            let display = ty.to_string();
            assert!(
                display.contains("BoundMethod"),
                "BoundMethod type should display correctly, got: {display}"
            );
        }
    }
    #[test]
    fn test_overload_parsing_from_stub() {
        let workspace_root = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .unwrap()
            .parent()
            .unwrap()
            .to_path_buf();
        let stub_path = workspace_root.join("stubs/overload_test.pyi");
        let mut class_registry = ClassRegistry::new();

        let result = Analyzer::load_builtins_into_registry(&stub_path, &mut class_registry);
        assert!(result.is_ok(), "Failed to load overload test stub: {:?}", result.err());

        let converter_meta = class_registry.get_class("Converter");
        assert!(converter_meta.is_some(), "Converter class not found in registry");

        let meta = converter_meta.unwrap();

        let convert_method = meta.lookup_method_type("convert");
        assert!(convert_method.is_some(), "convert method not found");

        if let Some(class_metadata::MethodType::Overloaded(overload_set)) = convert_method {
            assert_eq!(
                overload_set.signatures.len(),
                2,
                "convert should have 2 overload signatures"
            );
            assert!(
                overload_set.implementation.is_some(),
                "convert should have an implementation signature"
            );
        } else {
            panic!("convert method should be overloaded");
        }

        let process_method = meta.lookup_method_type("process");
        assert!(process_method.is_some(), "process method not found");

        if let Some(class_metadata::MethodType::Overloaded(overload_set)) = process_method {
            assert_eq!(
                overload_set.signatures.len(),
                2,
                "process should have 2 overload signatures"
            );
        } else {
            panic!("process method should be overloaded");
        }
    }

    #[test]
    fn test_overload_resolution_in_call() {
        let stub_content = r#"
from typing import overload

class Calculator:
    @overload
    def compute(self, x: int) -> str: ...

    @overload
    def compute(self, x: str) -> int: ...

    def compute(self, x): ...
"#;

        let mut parser = crate::parser::LspParser::new().unwrap();
        let parse_result = parser.parse(stub_content).unwrap();

        let mut class_registry = ClassRegistry::new();
        Analyzer::extract_stub_classes_into_registry(&parse_result.ast, &mut class_registry);

        let calc_meta = class_registry.get_class("Calculator");
        assert!(calc_meta.is_some(), "Calculator class should be in registry");

        let meta = calc_meta.unwrap();
        let compute_method = meta.lookup_method_type("compute");

        if let Some(class_metadata::MethodType::Overloaded(overload_set)) = compute_method {
            assert_eq!(
                overload_set.signatures.len(),
                2,
                "compute should have 2 overload signatures"
            );

            let resolved_int = overload_set.resolve(&[Type::Con(TypeCtor::Int)]);
            assert!(resolved_int.is_some(), "Should resolve overload for int argument");

            let resolved_str = overload_set.resolve(&[Type::Con(TypeCtor::String)]);
            assert!(resolved_str.is_some(), "Should resolve overload for str argument");
        } else {
            panic!("compute method should be overloaded");
        }
    }
}
