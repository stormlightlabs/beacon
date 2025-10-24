//! Type analysis orchestration
//!
//! Coordinates parsing, constraint generation, type inference, and caching.
//! This is the bridge between the parser, type system, and LSP features.
//!
//! [`Analyzer`] manages the flow from source code to inferred types:
//!  1. Parsing -> AST
//!  2. Name resolution -> Symbol table
//!  3. Constraint generation -> Constraint set
//!  4. Unification/solving -> Type substitution
//!  5. Caching -> Type cache

use crate::cache::CacheManager;
use crate::config::Config;
use crate::document::DocumentManager;
use crate::utils;
use beacon_core::{
    Type, TypeVar, TypeVarGen,
    errors::{AnalysisError, Result},
};
use beacon_parser::{AstNode, LiteralValue, SymbolTable};
use lsp_types::Position;
use rustc_hash::FxHashMap;
use url::Url;

/// Orchestrates type analysis for documents
pub struct Analyzer {
    _config: Config,
    cache: CacheManager,
    type_var_gen: TypeVarGen,
    documents: DocumentManager,
}

impl Analyzer {
    /// Create a new analyzer with the given configuration
    pub fn new(config: Config, documents: DocumentManager) -> Self {
        Self { _config: config, cache: CacheManager::new(), type_var_gen: TypeVarGen::new(), documents }
    }

    /// Analyze a document and return inferred types
    ///
    /// TODO: Implement full analysis pipeline
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

        let constraints = self.generate_constraints(&ast, &symbol_table)?;

        // Solve constraints to get type substitution
        // TODO: Implement constraint solving
        let _substitution = self.solve_constraints(constraints)?;

        // Build type map for all nodes
        // TODO: Walk AST and apply substitution to build type map
        let type_map = FxHashMap::default();

        // TODO: Extract type errors from constraint solving
        let type_errors = Vec::new();

        Ok(AnalysisResult { uri: uri.clone(), version, type_map, type_errors })
    }

    /// Get the inferred type at a specific position
    ///
    /// TODO: Implement position-to-node lookup and type retrieval
    /// TODO: Convert position to node_id and check cache
    /// TODO: Look up type for node at position
    pub fn type_at_position(&mut self, uri: &Url, _position: Position) -> Result<Option<Type>> {
        let _result = self.analyze(uri)?;
        Ok(None)
    }

    /// Invalidate cached analysis for a document
    pub fn invalidate(&mut self, uri: &Url) {
        self.cache.invalidate_document(uri);
    }

    /// Generate constraints from an AST
    ///
    /// TODO: Implement Algorithm W constraint generation: see ROADMAP.md "Constraint Generation (Algorithm)" section
    fn generate_constraints(&mut self, ast: &AstNode, _symbol_table: &SymbolTable) -> Result<ConstraintSet> {
        let mut constraints = Vec::new();

        // TODO: Walk AST and generate constraints based on node type
        self.visit_node(ast, &mut constraints)?;

        Ok(ConstraintSet { constraints })
    }

    /// Visit an AST node and generate constraints
    ///
    /// TODO: Implement for all AST node types per ROADMAP
    fn visit_node(&mut self, node: &AstNode, _constraints: &mut Vec<Constraint>) -> Result<Type> {
        match node {
            // TODO: Generate constraints for module body
            AstNode::Module { body: _, .. } => Ok(Type::Con(beacon_core::TypeCtor::Module("".into()))),
            // TODO: Generate function type constraints
            // - Fresh type vars for parameters
            // - Generate constraints from body
            // - Generalize if non-expansive
            AstNode::FunctionDef { .. } => Ok(Type::Var(self.type_var_gen.fresh())),
            // TODO: Generate class type constraints
            // - Create nominal type
            // - Generate constraints from methods
            AstNode::ClassDef { .. } => Ok(Type::Con(beacon_core::TypeCtor::Class("".into()))),
            // TODO: Generate assignment constraints
            // - Infer RHS type
            // - Generalize if non-expansive
            AstNode::Assignment { .. } => Ok(Type::Var(self.type_var_gen.fresh())),
            AstNode::AnnotatedAssignment { .. } => Ok(Type::Var(self.type_var_gen.fresh())),
            // TODO: Generate call constraints
            // - Function must have arrow type
            // - Unify parameter types with arguments
            AstNode::Call { .. } => Ok(Type::Var(self.type_var_gen.fresh())),
            // TODO: Look up in environment/symbol table
            // - Instantiate polytype
            AstNode::Identifier { .. } => Ok(Type::Var(self.type_var_gen.fresh())),
            AstNode::Literal { value, .. } => Ok(match value {
                LiteralValue::Integer(_) => Type::Con(beacon_core::TypeCtor::Int),
                LiteralValue::Float(_) => Type::Con(beacon_core::TypeCtor::Float),
                LiteralValue::String(_) => Type::Con(beacon_core::TypeCtor::String),
                LiteralValue::Boolean(_) => Type::Con(beacon_core::TypeCtor::Bool),
                LiteralValue::None => Type::Con(beacon_core::TypeCtor::NoneType),
            }),
            // TODO: Generate return type constraint
            AstNode::Return { .. } => Ok(Type::Var(self.type_var_gen.fresh())),
            AstNode::Import { .. } | AstNode::ImportFrom { .. } => {
                Ok(Type::Con(beacon_core::TypeCtor::Module("".into())))
            }
            // TODO: Proper attribute type checking
            AstNode::Attribute { object, .. } => self.visit_node(object, _constraints),
        }
    }

    /// Solve a set of constraints using beacon-core's unification algorithm
    ///
    /// TODO: Implement unification and constraint solving
    /// TODO: Implement constraint solving using [`beacon_core::Unifier`]
    fn solve_constraints(&mut self, _constraints: ConstraintSet) -> Result<Substitution> {
        Ok(Substitution::empty())
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
        self.collect_unbound_in_node(&ast, &symbol_table, &text, &mut unbound);

        unbound
    }

    /// Recursively collect unbound variables in an AST node using position-based scope resolution
    fn collect_unbound_in_node(
        &self, node: &AstNode, symbol_table: &SymbolTable, text: &str, unbound: &mut Vec<(String, usize, usize)>,
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
                    self.collect_unbound_in_node(stmt, symbol_table, text, unbound);
                }
            }
            AstNode::Call { args, .. } => {
                for arg in args {
                    self.collect_unbound_in_node(arg, symbol_table, text, unbound);
                }
            }
            AstNode::Assignment { value, .. } => self.collect_unbound_in_node(value, symbol_table, text, unbound),
            AstNode::AnnotatedAssignment { value, .. } => {
                if let Some(val) = value {
                    self.collect_unbound_in_node(val, symbol_table, text, unbound);
                }
            }
            AstNode::Return { value, .. } => {
                if let Some(val) = value {
                    self.collect_unbound_in_node(val, symbol_table, text, unbound);
                }
            }
            AstNode::Attribute { object, .. } => self.collect_unbound_in_node(object, symbol_table, text, unbound),
            AstNode::Import { .. } | AstNode::ImportFrom { .. } | AstNode::Literal { .. } => {}
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

    /// Check if a string is a valid Python identifier ([a-zA-Z_][a-zA-Z0-9_]*), filtering out cases
    /// where the parser created an Identifier node for other syntax.
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

/// Type error with location information
#[derive(Debug, Clone)]
pub struct TypeErrorInfo {
    pub error: beacon_core::TypeError,
    /// Line number (1-indexed)
    pub line: usize,
    /// Column number (1-indexed)
    pub col: usize,
    /// Optional end position for range
    pub end_line: Option<usize>,
    pub end_col: Option<usize>,
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
}

/// Set of type constraints
///
/// TODO: Use [`beacon_core`] constraint types when available
pub struct ConstraintSet {
    pub constraints: Vec<Constraint>,
}

/// Type constraint
///
/// TODO: Align with [`beacon_core`] constraint representation
#[derive(Debug, Clone)]
pub enum Constraint {
    /// Type equality constraint: t1 ~ t2
    Equal(Type, Type),

    /// HasAttr constraint: τ has attribute "name" : τ'
    HasAttr(Type, String, Type),

    /// Call constraint: f(args) -> ret
    Call(Type, Vec<Type>, Type),
    // TODO: Add more constraint types per ROADMAP
}

/// Type substitution
///
/// TODO: Use [`beacon_core::Subst`] when ready
pub struct Substitution {
    _map: FxHashMap<TypeVar, Type>,
}

impl Substitution {
    pub fn empty() -> Self {
        Self { _map: FxHashMap::default() }
    }

    /// TODO: Implement substitution application
    pub fn _apply(&self, _ty: &Type) -> Type {
        // Apply substitution to a type
        todo!()
    }

    /// TODO: Implement substitution composition
    pub fn _compose(&self, _other: &Substitution) -> Substitution {
        todo!()
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
        let ty = analyzer.visit_node(&lit, &mut constraints).unwrap();

        assert!(matches!(ty, Type::Con(beacon_core::TypeCtor::Int)));
    }

    #[test]
    fn test_substitution_empty() {
        let subst = Substitution::empty();
        assert_eq!(subst._map.len(), 0);
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
            eprintln!("Found unbound: {} at {}:{}", name, line, col);
        }

        let non_builtins: Vec<_> = unbound
            .iter()
            .filter(|(name, _, _)| !Analyzer::is_builtin(name))
            .collect();

        assert!(
            non_builtins.is_empty(),
            "Found unexpected unbound variables: {:?}",
            non_builtins
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
            eprintln!("Found unbound: {} at {}:{}", name, line, col);
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
            eprintln!("Found unbound: {} at {}:{}", name, line, col);
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
}
