use rustc_hash::FxHashMap;
use std::fmt;

/// Kinds classify types in the type system
/// Star (*) is the kind of proper types like Int, String
/// Arrow kinds (K1 → K2) are for type constructors like List, Dict
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Kind {
    /// Kind of proper types (*)
    Star,
    /// Kind of type constructors (K1 → K2)
    Arrow(Box<Kind>, Box<Kind>),
}

impl fmt::Display for Kind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Kind::Star => write!(f, "*"),
            Kind::Arrow(k1, k2) => write!(f, "{} → {}", k1, k2),
        }
    }
}

impl Kind {
    /// Create a kind for a type constructor that takes n arguments
    /// e.g., arity(2) creates * → * → *
    pub fn arity(n: usize) -> Self {
        if n == 0 {
            Kind::Star
        } else {
            let mut kind = Kind::Star;
            for _ in 0..n {
                kind = Kind::Arrow(Box::new(Kind::Star), Box::new(kind));
            }
            kind
        }
    }
}

/// Type variable with optional hint name for better error messages
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct TypeVar {
    pub id: u32,
    pub hint: Option<String>,
}

impl TypeVar {
    pub fn new(id: u32) -> Self {
        Self { id, hint: None }
    }

    pub fn named(id: u32, hint: &str) -> Self {
        Self {
            id,
            hint: Some(hint.to_string()),
        }
    }
}

impl fmt::Display for TypeVar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.hint {
            Some(hint) => write!(f, "'{}{}", hint, self.id),
            None => write!(f, "'t{}", self.id),
        }
    }
}

impl TypeVar {
    pub fn as_u32(&self) -> u32 {
        self.id
    }
}

// Constructor function for easier creation
impl From<u32> for TypeVar {
    fn from(id: u32) -> Self {
        TypeVar::new(id)
    }
}

/// Type constructors for built-in and user-defined types
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum TypeCtor {
    // Built-in scalar types
    Int,
    Float,
    String,
    Bool,
    NoneType,

    // Collection types
    List,
    Dict,
    Set,
    Tuple,

    // Function type constructor
    Function,

    // Special types
    Any,
    // Bottom type
    Never,

    // User-defined types (classes)
    Class(String),
    Module(String),
}

impl fmt::Display for TypeCtor {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TypeCtor::Int => write!(f, "int"),
            TypeCtor::Float => write!(f, "float"),
            TypeCtor::String => write!(f, "str"),
            TypeCtor::Bool => write!(f, "bool"),
            TypeCtor::NoneType => write!(f, "None"),
            TypeCtor::List => write!(f, "list"),
            TypeCtor::Dict => write!(f, "dict"),
            TypeCtor::Set => write!(f, "set"),
            TypeCtor::Tuple => write!(f, "tuple"),
            TypeCtor::Function => write!(f, "function"),
            TypeCtor::Any => write!(f, "Any"),
            TypeCtor::Never => write!(f, "Never"),
            TypeCtor::Class(name) => write!(f, "{}", name),
            TypeCtor::Module(name) => write!(f, "module<{}>", name),
        }
    }
}

impl TypeCtor {
    /// Get the kind of a type constructor
    pub fn kind(&self) -> Kind {
        match self {
            // Proper types have kind *
            TypeCtor::Int
            | TypeCtor::Float
            | TypeCtor::String
            | TypeCtor::Bool
            | TypeCtor::NoneType
            | TypeCtor::Any
            | TypeCtor::Never => Kind::Star,

            // Type constructors
            TypeCtor::List | TypeCtor::Set => Kind::arity(1), // * → *
            TypeCtor::Dict => Kind::arity(2),                 // * → * → *
            TypeCtor::Tuple => Kind::Star,                    // Special case: can be 0-ary
            TypeCtor::Function => Kind::arity(2),             // * → * → * (simplified)

            // User-defined types are proper types
            TypeCtor::Class(_) | TypeCtor::Module(_) => Kind::Star,
        }
    }

    /// Check if this type constructor represents a built-in type
    pub fn is_builtin(&self) -> bool {
        matches!(
            self,
            TypeCtor::Int
                | TypeCtor::Float
                | TypeCtor::String
                | TypeCtor::Bool
                | TypeCtor::NoneType
                | TypeCtor::List
                | TypeCtor::Dict
                | TypeCtor::Set
                | TypeCtor::Tuple
                | TypeCtor::Function
                | TypeCtor::Any
                | TypeCtor::Never
        )
    }
}

/// Core type representation for the Hindley-Milner type system
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Type {
    /// Type variable
    Var(TypeVar),

    /// Type constructor (0-ary application)
    Con(TypeCtor),

    /// Type application (higher-kinded types)
    App(Box<Type>, Box<Type>),

    /// Function types (syntactic sugar for Type application)
    Fun(Vec<Type>, Box<Type>), // args -> return

    /// Forall quantification (type schemes)
    ForAll(Vec<TypeVar>, Box<Type>),

    /// Union types (for Python's Union[A, B])
    Union(Vec<Type>),

    /// Record types for objects/classes (row polymorphism)
    Record(Vec<(String, Type)>, Option<TypeVar>), // fields, row variable
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Var(tv) => write!(f, "{}", tv),
            Type::Con(tc) => write!(f, "{}", tc),
            Type::App(t1, t2) => match t1.as_ref() {
                Type::Con(TypeCtor::List) => write!(f, "list[{}]", t2),
                Type::Con(TypeCtor::Set) => write!(f, "set[{}]", t2),
                Type::App(inner, key) if matches!(inner.as_ref(), Type::Con(TypeCtor::Dict)) => {
                    write!(f, "dict[{}, {}]", key, t2)
                }
                _ => write!(f, "({} {})", t1, t2),
            },
            Type::Fun(args, ret) => {
                if args.is_empty() {
                    write!(f, "() -> {}", ret)
                } else if args.len() == 1 {
                    write!(f, "{} -> {}", args[0], ret)
                } else {
                    write!(
                        f,
                        "({}) -> {}",
                        args.iter()
                            .map(|t| t.to_string())
                            .collect::<Vec<_>>()
                            .join(", "),
                        ret
                    )
                }
            }
            Type::ForAll(tvs, t) => {
                if tvs.is_empty() {
                    write!(f, "{}", t)
                } else {
                    write!(
                        f,
                        "∀{}. {}",
                        tvs.iter()
                            .map(|tv| tv.to_string())
                            .collect::<Vec<_>>()
                            .join(" "),
                        t
                    )
                }
            }
            Type::Union(types) => {
                write!(
                    f,
                    "{}",
                    types
                        .iter()
                        .map(|t| t.to_string())
                        .collect::<Vec<_>>()
                        .join(" | ")
                )
            }
            Type::Record(fields, row_var) => {
                let field_strs: Vec<String> = fields
                    .iter()
                    .map(|(k, v)| format!("{}: {}", k, v))
                    .collect();
                match row_var {
                    Some(rv) => write!(f, "{{ {} | {} }}", field_strs.join(", "), rv),
                    None => write!(f, "{{ {} }}", field_strs.join(", ")),
                }
            }
        }
    }
}

impl Type {
    /// Create commonly used types
    pub fn int() -> Self {
        Type::Con(TypeCtor::Int)
    }

    pub fn float() -> Self {
        Type::Con(TypeCtor::Float)
    }

    pub fn string() -> Self {
        Type::Con(TypeCtor::String)
    }

    pub fn bool() -> Self {
        Type::Con(TypeCtor::Bool)
    }

    pub fn none() -> Self {
        Type::Con(TypeCtor::NoneType)
    }

    pub fn any() -> Self {
        Type::Con(TypeCtor::Any)
    }

    pub fn never() -> Self {
        Type::Con(TypeCtor::Never)
    }

    /// Create a list type
    pub fn list(element_type: Type) -> Self {
        Type::App(Box::new(Type::Con(TypeCtor::List)), Box::new(element_type))
    }

    /// Create a dict type
    pub fn dict(key_type: Type, value_type: Type) -> Self {
        Type::App(
            Box::new(Type::App(
                Box::new(Type::Con(TypeCtor::Dict)),
                Box::new(key_type),
            )),
            Box::new(value_type),
        )
    }

    /// Create a function type
    pub fn fun(args: Vec<Type>, ret: Type) -> Self {
        Type::Fun(args, Box::new(ret))
    }

    /// Create a union type, flattening nested unions, removing duplications,
    /// and sorting for canonical form
    pub fn union(mut types: Vec<Type>) -> Self {
        if types.len() == 1 {
            return types.pop().unwrap();
        }

        let mut flattened = Vec::new();
        for t in types {
            match t {
                Type::Union(inner) => flattened.extend(inner),
                other => flattened.push(other),
            }
        }

        flattened.sort();
        flattened.dedup();

        if flattened.len() == 1 {
            flattened.pop().unwrap()
        } else {
            Type::Union(flattened)
        }
    }

    /// Create Optional[T] which is Union[T, None]
    pub fn optional(t: Type) -> Self {
        Type::union(vec![t, Type::none()])
    }

    /// Get all free type variables in this type
    pub fn free_vars(&self) -> FxHashMap<TypeVar, ()> {
        let mut vars = FxHashMap::default();
        self.collect_free_vars(&mut vars, &FxHashMap::default());
        vars
    }

    fn collect_free_vars(&self, vars: &mut FxHashMap<TypeVar, ()>, bound: &FxHashMap<TypeVar, ()>) {
        match self {
            Type::Var(tv) => {
                if !bound.contains_key(tv) {
                    vars.insert(tv.clone(), ());
                }
            }
            Type::Con(_) => {}
            Type::App(t1, t2) => {
                t1.collect_free_vars(vars, bound);
                t2.collect_free_vars(vars, bound);
            }
            Type::Fun(args, ret) => {
                for arg in args {
                    arg.collect_free_vars(vars, bound);
                }
                ret.collect_free_vars(vars, bound);
            }
            Type::ForAll(tvs, t) => {
                let mut new_bound = bound.clone();
                for tv in tvs {
                    new_bound.insert(tv.clone(), ());
                }
                t.collect_free_vars(vars, &new_bound);
            }
            Type::Union(types) => {
                for t in types {
                    t.collect_free_vars(vars, bound);
                }
            }
            Type::Record(fields, row_var) => {
                for (_, t) in fields {
                    t.collect_free_vars(vars, bound);
                }
                if let Some(rv) = row_var {
                    if !bound.contains_key(rv) {
                        vars.insert(rv.clone(), ());
                    }
                }
            }
        }
    }
}

/// Type schemes represent polymorphic types
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeScheme {
    pub quantified_vars: Vec<TypeVar>,
    pub ty: Type,
}

impl TypeScheme {
    pub fn new(quantified_vars: Vec<TypeVar>, ty: Type) -> Self {
        Self {
            quantified_vars,
            ty,
        }
    }

    /// Create a monomorphic type scheme (no quantified variables)
    pub fn mono(ty: Type) -> Self {
        Self::new(Vec::new(), ty)
    }

    /// Generalize a type into a type scheme by quantifying over free variables
    pub fn generalize(ty: Type, env_vars: &FxHashMap<TypeVar, ()>) -> Self {
        let free_vars = ty.free_vars();
        let quantified: Vec<TypeVar> = free_vars
            .keys()
            .filter(|tv| !env_vars.contains_key(tv))
            .cloned()
            .collect();

        Self::new(quantified, ty)
    }
}

impl fmt::Display for TypeScheme {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.quantified_vars.is_empty() {
            write!(f, "{}", self.ty)
        } else {
            write!(
                f,
                "∀{}. {}",
                self.quantified_vars
                    .iter()
                    .map(|tv| tv.to_string())
                    .collect::<Vec<_>>()
                    .join(" "),
                self.ty
            )
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_kind_arity() {
        assert_eq!(Kind::arity(0), Kind::Star);
        assert_eq!(
            Kind::arity(1),
            Kind::Arrow(Box::new(Kind::Star), Box::new(Kind::Star))
        );
        assert_eq!(
            Kind::arity(2),
            Kind::Arrow(
                Box::new(Kind::Star),
                Box::new(Kind::Arrow(Box::new(Kind::Star), Box::new(Kind::Star)))
            )
        );
    }

    #[test]
    fn test_type_var_display() {
        let tv1 = TypeVar::new(0);
        let tv2 = TypeVar::named(1, "alpha");
        assert_eq!(tv1.to_string(), "'t0");
        assert_eq!(tv2.to_string(), "'alpha1");
    }

    #[test]
    fn test_type_constructors() {
        assert_eq!(Type::int(), Type::Con(TypeCtor::Int));
        assert_eq!(Type::string(), Type::Con(TypeCtor::String));

        let list_int = Type::list(Type::int());
        assert_eq!(
            list_int,
            Type::App(Box::new(Type::Con(TypeCtor::List)), Box::new(Type::int()))
        );
    }

    #[test]
    fn test_function_types() {
        let fun_type = Type::fun(vec![Type::int(), Type::string()], Type::bool());
        assert_eq!(
            fun_type,
            Type::Fun(vec![Type::int(), Type::string()], Box::new(Type::bool()))
        );
        assert_eq!(fun_type.to_string(), "(int, str) -> bool");
    }

    #[test]
    fn test_union_types() {
        let union = Type::union(vec![Type::int(), Type::string()]);
        if let Type::Union(types) = union {
            assert_eq!(types.len(), 2);
            assert!(types.contains(&Type::int()));
            assert!(types.contains(&Type::string()));
        } else {
            panic!("Expected union type");
        }
    }

    #[test]
    fn test_optional_type() {
        let opt_int = Type::optional(Type::int());
        if let Type::Union(types) = opt_int {
            assert_eq!(types.len(), 2);
            assert!(types.contains(&Type::int()));
            assert!(types.contains(&Type::none()));
        } else {
            panic!("Expected union type for optional");
        }
    }

    #[test]
    fn test_free_vars() {
        let tv1 = TypeVar::new(0);
        let tv2 = TypeVar::new(1);
        let t = Type::fun(vec![Type::Var(tv1.clone())], Type::Var(tv2.clone()));

        let free_vars = t.free_vars();
        assert_eq!(free_vars.len(), 2);
        assert!(free_vars.contains_key(&tv1));
        assert!(free_vars.contains_key(&tv2));
    }

    #[test]
    fn test_type_scheme_generalization() {
        let tv1 = TypeVar::new(0);
        let tv2 = TypeVar::new(1);
        let ty = Type::fun(vec![Type::Var(tv1.clone())], Type::Var(tv1.clone()));

        let mut env_vars = FxHashMap::default();
        env_vars.insert(tv2, ()); // tv2 is bound in environment

        let scheme = TypeScheme::generalize(ty.clone(), &env_vars);
        assert_eq!(scheme.quantified_vars.len(), 1);
        assert_eq!(scheme.quantified_vars[0], tv1);
        assert_eq!(scheme.ty, ty);
    }

    #[test]
    fn test_type_display() {
        assert_eq!(Type::int().to_string(), "int");
        assert_eq!(Type::list(Type::string()).to_string(), "list[str]");
        assert_eq!(
            Type::dict(Type::string(), Type::int()).to_string(),
            "dict[str, int]"
        );

        let fun = Type::fun(vec![Type::int()], Type::bool());
        assert_eq!(fun.to_string(), "int -> bool");
    }

    #[test]
    fn test_type_constructor_kinds() {
        assert_eq!(TypeCtor::Int.kind(), Kind::Star);
        assert_eq!(TypeCtor::List.kind(), Kind::arity(1));
        assert_eq!(TypeCtor::Dict.kind(), Kind::arity(2));
    }
}
