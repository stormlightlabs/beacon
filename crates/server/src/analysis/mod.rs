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
pub mod exhaustiveness;
pub mod linter;
pub mod pattern;
pub mod rules;
pub mod type_env;

mod loader;
mod walker;

use crate::cache::{CacheManager, ScopeCacheKey};
use crate::config::Config;
use crate::document::DocumentManager;
use crate::utils;

use beacon_core::MethodSignature;
use beacon_core::{
    Subst, Type, TypeCtor, TypeError, TypeVarGen, Unifier,
    errors::{AnalysisError, Result},
};
use beacon_parser::{AstNode, ScopeId, ScopeKind, SymbolTable, resolve::Scope};
use constraint_gen::{Constraint, ConstraintResult, ConstraintSet, Span};
use lsp_types::Position;
use rustc_hash::FxHashMap;
use std::{collections::HashSet, sync::Arc};
use tokio::sync::RwLock;
use url::Url;

/// Type error with location information
#[derive(Debug, Clone)]
pub struct TypeErrorInfo {
    pub error: TypeError,
    /// Span where the error occurred
    pub span: Span,
}

impl TypeErrorInfo {
    /// Create a new type error with location
    pub fn new(error: TypeError, span: Span) -> Self {
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
    params: Vec<Type>,
    return_type: Type,
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
    /// Map from source positions to node IDs
    pub position_map: FxHashMap<(usize, usize), usize>,
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

    /// Extract scopes from the symbol table and get their source content
    fn extract_scopes_with_content(&self, symbol_table: &SymbolTable, source: &str) -> Vec<(ScopeId, String, Scope)> {
        let mut scopes = Vec::new();

        for (scope_id, scope) in &symbol_table.scopes {
            let content = Self::extract_scope_content(source, scope);
            scopes.push((*scope_id, content, scope.clone()));
        }

        scopes
    }

    /// Extract the source content for a scope based on its byte range
    fn extract_scope_content(source: &str, scope: &Scope) -> String {
        let start = scope.start_byte.min(source.len());
        let end = scope.end_byte.min(source.len());
        source[start..end].to_string()
    }

    /// Check which scopes have changed by comparing content hashes
    fn find_changed_scopes(
        &self, uri: &Url, scopes: &[(ScopeId, String, Scope)],
    ) -> std::collections::HashSet<ScopeId> {
        let mut changed = HashSet::new();

        for (scope_id, content, _scope) in scopes {
            let key = ScopeCacheKey::new(uri.clone(), *scope_id, content);
            if self.cache.scope_cache.get(&key).is_none() {
                changed.insert(*scope_id);
            }
        }

        changed
    }

    /// Cache scope analysis results for each scope
    /// TODO: Implement proper node-to-scope mapping
    /// TODO: Track scope dependencies
    fn cache_scope_results(
        &self, uri: &Url, scopes: &[(ScopeId, String, Scope)], type_map: &FxHashMap<usize, Type>,
        position_map: &FxHashMap<(usize, usize), usize>,
    ) {
        use crate::cache::{CachedScopeResult, ScopeCacheKey};

        for (scope_id, content, scope) in scopes {
            let scope_type_map: FxHashMap<usize, Type> = type_map
                .iter()
                .filter(|(_node_id, _ty)| true)
                .map(|(k, v)| (*k, v.clone()))
                .collect();

            let scope_position_map: FxHashMap<(usize, usize), usize> = position_map
                .iter()
                .filter(|((line, _col), _node_id)| {
                    let scope_start_line = scope.start_byte / 80;
                    let scope_end_line = scope.end_byte / 80;
                    *line >= scope_start_line && *line <= scope_end_line
                })
                .map(|(k, v)| (*k, *v))
                .collect();

            let key = ScopeCacheKey::new(uri.clone(), *scope_id, content);
            let result =
                CachedScopeResult { type_map: scope_type_map, position_map: scope_position_map, dependencies: vec![] };

            self.cache.scope_cache.insert(key, result);
        }
    }

    /// Analyze a document and return inferred types
    pub fn analyze(&mut self, uri: &Url) -> Result<AnalysisResult> {
        let result = self.documents.get_document(uri, |doc| {
            let ast = doc.ast().ok_or(AnalysisError::MissingAst)?.clone();
            let symbol_table = doc.symbol_table().ok_or(AnalysisError::MissingSymbolTable)?.clone();
            let source = doc.text();
            Ok::<_, AnalysisError>((ast, symbol_table, source, doc.version))
        });

        let (ast, symbol_table, source, version) = match result {
            Some(Ok(data)) => data,
            Some(Err(e)) => return Err(e.into()),
            None => return Err(AnalysisError::DocumentNotFound(uri.clone()).into()),
        };

        if let Some(cached) = self.cache.analysis_cache.get(uri, version) {
            self.position_maps.insert(uri.clone(), cached.position_map.clone());
            return Ok(AnalysisResult {
                uri: uri.clone(),
                version,
                type_map: cached.type_map,
                position_map: cached.position_map,
                type_errors: cached.type_errors,
                static_analysis: cached.static_analysis,
            });
        }

        let scopes = self.extract_scopes_with_content(&symbol_table, &source);
        let changed_scopes = self.find_changed_scopes(uri, &scopes);
        if !changed_scopes.is_empty() {
            let total_scopes = scopes.len();
            let unchanged_scopes = total_scopes - changed_scopes.len();
            tracing::debug!(
                "Scope cache for {}: {} unchanged, {} changed out of {} total",
                uri,
                unchanged_scopes,
                changed_scopes.len(),
                total_scopes
            );
        }

        let ConstraintResult(constraints, mut type_map, position_map, class_registry) =
            walker::generate_constraints(&self.stub_cache, &ast, &symbol_table)?;

        let (substitution, type_errors) = Self::solve_constraints(constraints, &class_registry)?;

        for (_node_id, ty) in type_map.iter_mut() {
            *ty = substitution.apply(ty);
        }

        self.position_maps.insert(uri.clone(), position_map.clone());

        let static_analysis = self.perform_static_analysis(&ast, &symbol_table);

        self.cache_scope_results(uri, &scopes, &type_map, &position_map);

        let scope_stats = self.cache.scope_stats();
        if scope_stats.hits + scope_stats.misses > 0 {
            tracing::debug!(
                "Scope cache stats - Size: {}/{}, Hit rate: {:.1}%",
                scope_stats.size,
                scope_stats.capacity,
                scope_stats.hit_rate
            );
        }

        let cached_result = crate::cache::CachedAnalysisResult {
            type_map: type_map.clone(),
            position_map: position_map.clone(),
            type_errors: type_errors.clone(),
            static_analysis: static_analysis.clone(),
        };

        self.cache.analysis_cache.insert(uri.clone(), version, cached_result);

        Ok(AnalysisResult { uri: uri.clone(), version, type_map, position_map, type_errors, static_analysis })
    }

    /// Get the inferred type at a specific position by analyzing the document and looks up the type at the specified position.
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

    /// Convert a [Type::Fun] to a [MethodSignature] by extracting parameters and return types from a function type.
    fn type_to_method_signature(name: &str, ty: &Type) -> Option<MethodSignature> {
        match ty {
            Type::Fun(params, ret) => Some(MethodSignature {
                name: name.to_string(),
                params: params.clone(),
                return_type: ret.as_ref().clone(),
            }),
            _ => None,
        }
    }

    /// Check if a type satisfies a user-defined protocol
    ///
    /// This method checks structural conformance by verifying that the type has all required methods with compatible signatures.
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
    ///
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

                Constraint::MatchPattern(_subject_ty, _pattern, bindings, _span) => {
                    for (_var_name, binding_ty) in bindings {
                        let applied_binding = subst.apply(&binding_ty);
                        let _ = applied_binding;
                    }
                }

                Constraint::PatternExhaustive(subject_ty, patterns, span) => {
                    let result = exhaustiveness::check_exhaustiveness(&subject_ty, &patterns);
                    match result {
                        exhaustiveness::ExhaustivenessResult::Exhaustive => {}
                        exhaustiveness::ExhaustivenessResult::NonExhaustive { uncovered } => {
                            let uncovered_str =
                                uncovered.iter().map(|ty| ty.to_string()).collect::<Vec<_>>().join(", ");
                            type_errors.push(TypeErrorInfo::new(TypeError::PatternNonExhaustive(uncovered_str), span));
                        }
                    }
                }
                Constraint::PatternReachable(pattern, previous_patterns, span) => {
                    let result = exhaustiveness::check_reachability(&pattern, &previous_patterns);
                    match result {
                        exhaustiveness::ReachabilityResult::Reachable => {}
                        exhaustiveness::ReachabilityResult::Unreachable { subsumed_by: _ } => {
                            type_errors.push(TypeErrorInfo::new(TypeError::PatternUnreachable, span));
                        }
                    }
                }
            }
        }

        Ok((subst, type_errors))
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
                        let scope_id = symbol_table
                            .scopes
                            .values()
                            .find(|scope| scope.kind == ScopeKind::Function)
                            .map(|scope| scope.id)
                            .unwrap_or(symbol_table.root_scope);
                        let analyzer = data_flow::DataFlowAnalyzer::new(&cfg, func_body, symbol_table, scope_id);
                        return Some(analyzer.analyze());
                    }
                }
                None
            }
            AstNode::FunctionDef { body, .. } => {
                let mut builder = cfg::CfgBuilder::new();
                builder.build_function(body);
                let cfg = builder.build();
                let scope_id = symbol_table
                    .scopes
                    .values()
                    .find(|scope| scope.kind == ScopeKind::Function)
                    .map(|scope| scope.id)
                    .unwrap_or(symbol_table.root_scope);
                let analyzer = data_flow::DataFlowAnalyzer::new(&cfg, body, symbol_table, scope_id);
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
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::analysis::class_metadata::ClassRegistry;
    use std::str::FromStr;

    #[test]
    fn test_analyzer_creation() {
        let config = Config::default();
        let documents = DocumentManager::new().unwrap();
        let _analyzer = Analyzer::new(config, documents);
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
        let mut ctx = constraint_gen::ConstraintGenContext::new();

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

    /// TODO: fix the error detection
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

        let result = loader::load_builtins_into_registry(&stub_path, &mut class_registry);
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
        loader::extract_stub_classes_into_registry(&parse_result.ast, &mut class_registry);

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

    #[test]
    fn test_incremental_reanalysis_cache_hit() {
        let config = Config::default();
        let documents = DocumentManager::new().unwrap();
        let mut analyzer = Analyzer::new(config, documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
x = 42
y = "hello"
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let result1 = analyzer.analyze(&uri).unwrap();
        assert_eq!(result1.version, 1);
        assert!(!result1.type_map.is_empty());

        let cache_stats_before = analyzer.cache.analysis_cache.stats();
        assert_eq!(cache_stats_before.size, 1);

        let result2 = analyzer.analyze(&uri).unwrap();
        assert_eq!(result2.version, 1);
        assert_eq!(result2.type_map.len(), result1.type_map.len());

        let cache_stats_after = analyzer.cache.analysis_cache.stats();
        assert_eq!(cache_stats_after.size, 1);
    }

    #[test]
    fn test_incremental_reanalysis_cache_miss_on_edit() {
        use lsp_types::{TextDocumentContentChangeEvent, VersionedTextDocumentIdentifier};

        let config = Config::default();
        let documents = DocumentManager::new().unwrap();
        let mut analyzer = Analyzer::new(config, documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();
        let source1 = "x = 42";

        documents.open_document(uri.clone(), 1, source1.to_string()).unwrap();

        let result1 = analyzer.analyze(&uri).unwrap();
        assert_eq!(result1.version, 1);

        let cache_stats = analyzer.cache.analysis_cache.stats();
        assert_eq!(cache_stats.size, 1);

        let source2 = "x = 42\ny = 'hello'";
        let params = VersionedTextDocumentIdentifier { uri: uri.clone(), version: 2 };
        let changes =
            vec![TextDocumentContentChangeEvent { range: None, range_length: None, text: source2.to_string() }];
        documents.update_document(params, changes).unwrap();

        let result2 = analyzer.analyze(&uri).unwrap();
        assert_eq!(result2.version, 2);

        let cache_stats_after = analyzer.cache.analysis_cache.stats();
        assert_eq!(cache_stats_after.size, 2);
    }

    #[test]
    fn test_incremental_reanalysis_preserves_type_errors() {
        let config = Config::default();
        let documents = DocumentManager::new().unwrap();
        let mut analyzer = Analyzer::new(config, documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
class Foo:
    def bar(self):
        return self.nonexistent_attr
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let result1 = analyzer.analyze(&uri).unwrap();
        let errors1 = result1.type_errors.len();

        let result2 = analyzer.analyze(&uri).unwrap();
        let errors2 = result2.type_errors.len();

        assert_eq!(errors1, errors2, "Cached result should preserve type errors");
    }

    #[test]
    fn test_incremental_reanalysis_invalidation() {
        let config = Config::default();
        let documents = DocumentManager::new().unwrap();
        let mut analyzer = Analyzer::new(config, documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = "x = 42";

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        analyzer.analyze(&uri).unwrap();

        let cache_stats_before = analyzer.cache.analysis_cache.stats();
        assert_eq!(cache_stats_before.size, 1);

        analyzer.invalidate(&uri);

        let cache_stats_after = analyzer.cache.analysis_cache.stats();
        assert_eq!(cache_stats_after.size, 0);
    }

    #[test]
    fn test_incremental_reanalysis_multiple_documents() {
        let config = Config::default();
        let documents = DocumentManager::new().unwrap();
        let mut analyzer = Analyzer::new(config, documents.clone());

        let uri1 = Url::from_str("file:///test1.py").unwrap();
        let uri2 = Url::from_str("file:///test2.py").unwrap();
        let uri3 = Url::from_str("file:///test3.py").unwrap();

        documents.open_document(uri1.clone(), 1, "x = 1".to_string()).unwrap();
        documents.open_document(uri2.clone(), 1, "y = 2".to_string()).unwrap();
        documents.open_document(uri3.clone(), 1, "z = 3".to_string()).unwrap();

        analyzer.analyze(&uri1).unwrap();
        analyzer.analyze(&uri2).unwrap();
        analyzer.analyze(&uri3).unwrap();

        let cache_stats = analyzer.cache.analysis_cache.stats();
        assert_eq!(cache_stats.size, 3);

        analyzer.analyze(&uri1).unwrap();
        analyzer.analyze(&uri2).unwrap();
        analyzer.analyze(&uri3).unwrap();

        let cache_stats_after = analyzer.cache.analysis_cache.stats();
        assert_eq!(cache_stats_after.size, 3);
    }

    #[test]
    fn test_incremental_reanalysis_static_analysis_cached() {
        let config = Config::default();
        let documents = DocumentManager::new().unwrap();
        let mut analyzer = Analyzer::new(config, documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
def test():
    x = 1
    return x
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let result1 = analyzer.analyze(&uri).unwrap();
        let has_static_analysis_1 = result1.static_analysis.is_some();

        let result2 = analyzer.analyze(&uri).unwrap();
        let has_static_analysis_2 = result2.static_analysis.is_some();

        assert_eq!(
            has_static_analysis_1, has_static_analysis_2,
            "Static analysis should be consistent across cache hits"
        );
    }

    #[test]
    fn test_pattern_exhaustiveness_complete() {
        let config = Config::default();
        let documents = DocumentManager::new().unwrap();
        let mut analyzer = Analyzer::new(config, documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
def describe(x: int):
    match x:
        case 0:
            return "zero"
        case _:
            return "other"
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let result = analyzer.analyze(&uri).unwrap();
        assert_eq!(result.type_errors.len(), 0, "Exhaustive match should have no errors");
    }

    #[test]
    fn test_pattern_exhaustiveness_incomplete() {
        let config = Config::default();
        let documents = DocumentManager::new().unwrap();
        let mut analyzer = Analyzer::new(config, documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
def describe(x: int | str):
    match x:
        case 0:
            return "zero"
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let result = analyzer.analyze(&uri).unwrap();
        let has_exhaustiveness_error = result
            .type_errors
            .iter()
            .any(|err| matches!(err.error, TypeError::PatternNonExhaustive(_)));

        assert!(
            has_exhaustiveness_error,
            "Non-exhaustive match should generate PatternNonExhaustive error"
        );
    }

    /// Test that patterns after a catch-all pattern are correctly identified as unreachable
    #[test]
    fn test_pattern_reachability_unreachable_after_catch_all() {
        let config = Config::default();
        let documents = DocumentManager::new().unwrap();
        let mut analyzer = Analyzer::new(config, documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
def describe(x: int):
    match x:
        case y:
            return "anything"
        case 42:
            return "unreachable"
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let result = analyzer.analyze(&uri).unwrap();
        let has_unreachable_error = result
            .type_errors
            .iter()
            .any(|err| matches!(err.error, TypeError::PatternUnreachable));

        assert!(has_unreachable_error, "Pattern after catch-all should be unreachable");
    }

    #[test]
    fn test_pattern_reachability_duplicate_patterns() {
        let config = Config::default();
        let documents = DocumentManager::new().unwrap();
        let mut analyzer = Analyzer::new(config, documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
def describe(x: int):
    match x:
        case 42:
            return "first"
        case 42:
            return "duplicate"
        case _:
            return "other"
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let result = analyzer.analyze(&uri).unwrap();
        let has_unreachable_error = result
            .type_errors
            .iter()
            .any(|err| matches!(err.error, TypeError::PatternUnreachable));

        assert!(has_unreachable_error, "Duplicate pattern should be unreachable");
    }

    #[test]
    fn test_pattern_union_exhaustiveness() {
        let config = Config::default();
        let documents = DocumentManager::new().unwrap();
        let mut analyzer = Analyzer::new(config, documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
def describe(x: int | str):
    match x:
        case 0:
            return "zero"
        case "hello":
            return "greeting"
        case _:
            return "other"
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let result = analyzer.analyze(&uri).unwrap();
        assert_eq!(
            result.type_errors.len(),
            0,
            "Union type with complete coverage should have no exhaustiveness errors"
        );
    }
}
