//! Type Environment
//!
//! Manages the mapping from symbols (variables, functions, classes) to their types.
//! Integrates with the symbol table for scope-aware type lookups.
//!
//! The type environment stores:
//! - Variable bindings → Type schemes
//! - Function signatures → Function types
//! - Class definitions → Class types
//! - Type variable scopes

use beacon_core::{AnnotationParser, Subst, Type, TypeScheme, TypeVar, TypeVarGen, Unifier};
use beacon_parser::{AstNode, Parameter, SymbolTable};
use rustc_hash::FxHashMap;

/// Type environment for a module
///
/// Maps identifiers to their type schemes, allowing polymorphic types to be instantiated with fresh type variables when looked up.
#[derive(Debug, Clone)]
pub struct TypeEnvironment {
    /// Symbol name → Type scheme
    bindings: FxHashMap<String, TypeScheme>,
    /// Type variable generator for fresh variables
    var_gen: TypeVarGen,
    /// Annotation parser for parsing type hints
    annotation_parser: AnnotationParser,
    /// Generator/coroutine parameters (yield_type, send_type, return_type) when inside a generator/coroutine
    generator_params: Option<(Type, Type, Type)>,
    /// Expected return type for the current function (used to constrain return statements)
    expected_return_type: Option<Type>,
}

impl TypeEnvironment {
    /// Create a new type environment
    pub fn new() -> Self {
        let mut env = Self {
            bindings: FxHashMap::default(),
            var_gen: TypeVarGen::new(),
            annotation_parser: AnnotationParser::new(),
            generator_params: None,
            expected_return_type: None,
        };

        env.add_builtins();
        env
    }

    /// Add built-in function types
    fn add_builtins(&mut self) {
        self.bindings.insert(
            "print".to_string(),
            TypeScheme::mono(Type::fun(vec![Type::any()], Type::none())),
        );

        let tv = self.var_gen.fresh();
        self.bindings.insert(
            "len".to_string(),
            TypeScheme::new(vec![tv.clone()], Type::fun(vec![Type::Var(tv)], Type::int())),
        );

        self.bindings.insert(
            "range".to_string(),
            TypeScheme::mono(Type::fun(vec![Type::int()], Type::list(Type::int()))),
        );

        self.bindings.insert(
            "str".to_string(),
            TypeScheme::mono(Type::fun(vec![Type::any()], Type::string())),
        );

        self.bindings.insert(
            "int".to_string(),
            TypeScheme::mono(Type::fun(vec![Type::any()], Type::int())),
        );

        self.bindings.insert(
            "float".to_string(),
            TypeScheme::mono(Type::fun(vec![Type::any()], Type::float())),
        );

        self.bindings.insert(
            "bool".to_string(),
            TypeScheme::mono(Type::fun(vec![Type::any()], Type::bool())),
        );

        let elem_tv = self.var_gen.fresh();
        self.bindings.insert(
            "list".to_string(),
            TypeScheme::new(
                vec![elem_tv.clone()],
                Type::fun(vec![Type::Var(elem_tv.clone())], Type::list(Type::Var(elem_tv))),
            ),
        );

        let key_tv = self.var_gen.fresh();
        let val_tv = self.var_gen.fresh();
        self.bindings.insert(
            "dict".to_string(),
            TypeScheme::new(
                vec![key_tv.clone(), val_tv.clone()],
                Type::fun(vec![], Type::dict(Type::Var(key_tv), Type::Var(val_tv))),
            ),
        );

        let set_tv = self.var_gen.fresh();
        self.bindings.insert(
            "set".to_string(),
            TypeScheme::new(
                vec![set_tv.clone()],
                Type::fun(
                    vec![],
                    Type::App(
                        Box::new(Type::Con(beacon_core::TypeCtor::Set)),
                        Box::new(Type::Var(set_tv)),
                    ),
                ),
            ),
        );

        self.bindings.insert(
            "isinstance".to_string(),
            TypeScheme::mono(Type::fun(vec![Type::any(), Type::any()], Type::bool())),
        );
    }

    /// Bind a name to a type scheme
    pub fn bind(&mut self, name: String, scheme: TypeScheme) {
        self.bindings.insert(name, scheme);
    }

    /// Lookup a name and instantiate its type scheme
    pub fn lookup(&mut self, name: &str) -> Option<Type> {
        let scheme = self.bindings.get(name)?.clone();
        Some(self.instantiate(&scheme))
    }

    /// Instantiate a type scheme by replacing quantified variables with fresh type variables
    pub fn instantiate(&mut self, scheme: &TypeScheme) -> Type {
        if scheme.quantified_vars.is_empty() {
            return scheme.ty.clone();
        }

        let mut subst = Subst::empty();
        for tv in &scheme.quantified_vars {
            subst.insert(tv.clone(), Type::Var(self.var_gen.fresh()));
        }

        subst.apply(&scheme.ty)
    }

    /// Generalize a type into a type scheme by quantifing over free type variables not in the environment
    pub fn generalize(&self, ty: Type) -> TypeScheme {
        let env_vars = self.free_vars();
        TypeScheme::generalize(ty, &env_vars)
    }

    /// Get all free type variables in the environment
    fn free_vars(&self) -> FxHashMap<TypeVar, ()> {
        let mut vars = FxHashMap::default();
        for scheme in self.bindings.values() {
            vars.extend(scheme.ty.free_vars());
        }
        vars
    }

    /// Generate a fresh type variable
    pub fn fresh_var(&mut self) -> TypeVar {
        self.var_gen.fresh()
    }

    /// Generate a fresh type variable with a hint
    pub fn fresh_named(&mut self, hint: &str) -> TypeVar {
        self.var_gen.fresh_named(hint)
    }

    /// Parse a type annotation string into a Type
    pub fn parse_annotation(&self, annotation: &str) -> beacon_core::Result<Type> {
        self.annotation_parser.parse(annotation)
    }

    /// Parse a type annotation, returning Any if parsing fails
    pub fn parse_annotation_or_any(&self, annotation: &str) -> Type {
        self.annotation_parser.parse_or_any(annotation)
    }

    /// Extract type from function parameters
    pub fn function_param_types(&self, params: &[Parameter]) -> Vec<Type> {
        params
            .iter()
            .map(|param| {
                param
                    .type_annotation
                    .as_ref()
                    .map(|ann| self.parse_annotation_or_any(ann))
                    .unwrap_or(Type::any())
            })
            .collect()
    }

    /// Extract return type from function definition
    pub fn function_return_type(&self, return_annotation: Option<&String>) -> Type {
        return_annotation
            .map(|ann| self.parse_annotation_or_any(ann))
            .unwrap_or(Type::any())
    }

    /// Build type environment from symbol table using type annotations where available.
    pub fn from_symbol_table(symbol_table: &SymbolTable, ast: &AstNode) -> Self {
        let mut env = Self::new();
        env.populate_from_ast(ast);
        let _ = symbol_table;
        env
    }

    /// Populate environment from AST by walking and extracting type information
    fn populate_from_ast(&mut self, node: &AstNode) {
        match node {
            AstNode::Module { body, .. } => {
                for stmt in body {
                    self.populate_from_ast(stmt);
                }
            }
            AstNode::FunctionDef { name, args, return_type, body, .. } => {
                let param_types = self.function_param_types(args);
                let ret_type = self.function_return_type(return_type.as_ref());
                let fn_type = Type::fun(param_types, ret_type);
                self.bind(name.clone(), TypeScheme::mono(fn_type));

                for stmt in body {
                    self.populate_from_ast(stmt);
                }
            }
            AstNode::ClassDef { name, body, .. } => {
                let class_type = Type::Con(beacon_core::TypeCtor::Class(name.clone()));
                self.bind(name.clone(), TypeScheme::mono(class_type));

                for stmt in body {
                    self.populate_from_ast(stmt);
                }
            }
            AstNode::AnnotatedAssignment { target, type_annotation, .. } => {
                let ty = self.parse_annotation_or_any(type_annotation);
                self.bind(target.clone(), TypeScheme::mono(ty));
            }
            AstNode::If { body, elif_parts, else_body, .. } => {
                for stmt in body {
                    self.populate_from_ast(stmt);
                }
                for (_, elif_body) in elif_parts {
                    for stmt in elif_body {
                        self.populate_from_ast(stmt);
                    }
                }
                if let Some(else_stmts) = else_body {
                    for stmt in else_stmts {
                        self.populate_from_ast(stmt);
                    }
                }
            }
            AstNode::For { body, else_body, .. } | AstNode::While { body, else_body, .. } => {
                for stmt in body {
                    self.populate_from_ast(stmt);
                }
                if let Some(else_stmts) = else_body {
                    for stmt in else_stmts {
                        self.populate_from_ast(stmt);
                    }
                }
            }
            AstNode::Try { body, handlers, else_body, finally_body, .. } => {
                for stmt in body {
                    self.populate_from_ast(stmt);
                }
                for handler in handlers {
                    for stmt in &handler.body {
                        self.populate_from_ast(stmt);
                    }
                }
                if let Some(else_stmts) = else_body {
                    for stmt in else_stmts {
                        self.populate_from_ast(stmt);
                    }
                }
                if let Some(finally_stmts) = finally_body {
                    for stmt in finally_stmts {
                        self.populate_from_ast(stmt);
                    }
                }
            }
            AstNode::With { body, .. } => {
                for stmt in body {
                    self.populate_from_ast(stmt);
                }
            }
            _ => {}
        }
    }

    /// Unify two types and return the resulting substitution
    pub fn unify(&self, t1: &Type, t2: &Type) -> beacon_core::Result<Subst> {
        Unifier::unify(t1, t2)
    }

    /// Set the generator parameters (yield_type, send_type, return_type) for the current scope.
    pub fn set_generator_params(&mut self, yield_ty: Type, send_ty: Type, return_ty: Type) {
        self.generator_params = Some((yield_ty, send_ty, return_ty));
    }

    /// Get the generator parameters if we're inside a generator/coroutine function.
    pub fn get_generator_params(&self) -> Option<&(Type, Type, Type)> {
        self.generator_params.as_ref()
    }

    /// Clear the generator parameters when exiting a generator/coroutine function.
    pub fn clear_generator_params(&mut self) {
        self.generator_params = None;
    }

    /// Set the expected return type for the current function
    pub fn set_expected_return_type(&mut self, return_ty: Type) {
        self.expected_return_type = Some(return_ty);
    }

    /// Get the expected return type if we're inside a function
    pub fn get_expected_return_type(&self) -> Option<&Type> {
        self.expected_return_type.as_ref()
    }

    /// Clear the expected return type when exiting a function
    pub fn clear_expected_return_type(&mut self) {
        self.expected_return_type = None;
    }
}

impl Default for TypeEnvironment {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use beacon_parser::NameResolver;

    #[test]
    fn test_type_env_creation() {
        let env = TypeEnvironment::new();
        assert!(!env.bindings.is_empty());
    }

    #[test]
    fn test_builtin_lookup() {
        let mut env = TypeEnvironment::new();
        let print_type = env.lookup("print").unwrap();

        match print_type {
            Type::Fun(args, ret) => {
                assert_eq!(args.len(), 1);
                assert_eq!(ret.as_ref(), &Type::none());
            }
            _ => panic!("Expected function type for print"),
        }
    }

    #[test]
    fn test_bind_and_lookup() {
        let mut env = TypeEnvironment::new();
        env.bind("x".to_string(), TypeScheme::mono(Type::int()));

        let x_type = env.lookup("x").unwrap();
        assert_eq!(x_type, Type::int());
    }

    #[test]
    fn test_instantiation() {
        let mut env = TypeEnvironment::new();
        let tv = env.fresh_var();
        let scheme = TypeScheme::new(vec![tv.clone()], Type::fun(vec![Type::Var(tv.clone())], Type::Var(tv)));

        let inst1 = env.instantiate(&scheme);
        let inst2 = env.instantiate(&scheme);

        match (&inst1, &inst2) {
            (Type::Fun(args1, ret1), Type::Fun(args2, ret2)) => {
                assert_eq!(args1[0], *ret1.as_ref());
                assert_eq!(args2[0], *ret2.as_ref());
                assert_ne!(args1[0], args2[0]);
            }
            _ => panic!("Expected function types"),
        }
    }

    #[test]
    fn test_generalization() {
        let mut env = TypeEnvironment::new();
        let tv = env.fresh_var();
        let ty = Type::fun(vec![Type::Var(tv.clone())], Type::Var(tv.clone()));
        let scheme = env.generalize(ty.clone());
        assert_eq!(scheme.quantified_vars.len(), 1);
        assert_eq!(scheme.quantified_vars[0], tv);
    }

    #[test]
    fn test_parse_annotation() {
        let env = TypeEnvironment::new();
        let ty = env.parse_annotation("list[int]").unwrap();
        assert_eq!(ty, Type::list(Type::int()));
    }

    #[test]
    fn test_function_param_types() {
        let env = TypeEnvironment::new();
        let params = vec![
            Parameter {
                name: "x".to_string(),
                line: 1,
                col: 1,
                type_annotation: Some("int".to_string()),
                default_value: None,
            },
            Parameter {
                name: "y".to_string(),
                line: 1,
                col: 2,
                type_annotation: Some("str".to_string()),
                default_value: None,
            },
        ];

        let types = env.function_param_types(&params);
        assert_eq!(types, vec![Type::int(), Type::string()]);
    }

    #[test]
    fn test_from_symbol_table_with_function() {
        let source = "def foo(x: int) -> str:\n    return str(x)".to_string();
        let mut resolver = NameResolver::new(source);

        let ast = AstNode::Module {
            body: vec![AstNode::FunctionDef {
                name: "foo".to_string(),
                args: vec![Parameter {
                    name: "x".to_string(),
                    line: 1,
                    col: 9,
                    type_annotation: Some("int".to_string()),
                    default_value: None,
                }],
                body: vec![],
                docstring: None,
                return_type: Some("str".to_string()),
                decorators: Vec::new(),
                line: 1,
                col: 1,
                is_async: false,
            }],
            docstring: None,
        };

        let _ = resolver.resolve(&ast);
        let mut env = TypeEnvironment::from_symbol_table(&resolver.symbol_table, &ast);

        let foo_type = env.lookup("foo").unwrap();
        match foo_type {
            Type::Fun(args, ret) => {
                assert_eq!(args.len(), 1);
                assert_eq!(args[0], Type::int());
                assert_eq!(ret.as_ref(), &Type::string());
            }
            _ => panic!("Expected function type"),
        }
    }

    #[test]
    fn test_polymorphic_len_builtin() {
        let mut env = TypeEnvironment::new();

        let len1 = env.lookup("len").unwrap();
        let len2 = env.lookup("len").unwrap();

        match (&len1, &len2) {
            (Type::Fun(args1, ret1), Type::Fun(args2, ret2)) => {
                assert_eq!(ret1.as_ref(), &Type::int());
                assert_eq!(ret2.as_ref(), &Type::int());
                assert_ne!(args1[0], args2[0]);
            }
            _ => panic!("Expected function types"),
        }
    }
}
