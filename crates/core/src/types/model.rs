//! Core type model.

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Kind {
    /// Kind of proper types (*)
    Star,
    /// Kind of type constructors (K1 -> K2)
    Arrow(Box<Kind>, Box<Kind>),
}

impl std::fmt::Display for Kind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Kind::Star => write!(f, "*"),
            Kind::Arrow(k1, k2) => write!(f, "{k1} -> {k2}"),
        }
    }
}

impl Kind {
    /// Create a kind for a type constructor that takes n arguments
    /// e.g., arity(2) creates * -> * -> *
    pub fn arity(n: usize) -> Self {
        match n {
            0 => Kind::Star,
            _ => (0..n).fold(Kind::Star, |acc, _| Kind::Arrow(Box::new(Kind::Star), Box::new(acc))),
        }
    }
}

/// Variance describes how subtyping relationships are preserved or reversed through type constructors.
///
/// - **Covariant**: If `Dog <: Animal`, then `Container[Dog] <: Container[Animal]`
///   Used for immutable containers, function return types, producer contexts
/// - **Contravariant**: If `Dog <: Animal`, then `Container[Animal] <: Container[Dog]`
///   Used for function parameters, consumer contexts
/// - **Invariant**: No subtyping relationship preserved
///   Used for mutable containers, read-write contexts
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Default)]
pub enum Variance {
    /// Covariant position (e.g., return types, immutable containers)
    Covariant,
    /// Contravariant position (e.g., function parameters)
    Contravariant,
    /// Invariant position (e.g., mutable containers)
    #[default]
    Invariant,
}

impl std::fmt::Display for Variance {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Variance::Covariant => write!(f, "covariant"),
            Variance::Contravariant => write!(f, "contravariant"),
            Variance::Invariant => write!(f, "invariant"),
        }
    }
}

/// Type variable with optional hint name for better error messages
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct TypeVar {
    pub id: u32,
    pub hint: Option<String>,
    pub variance: Variance,
}

impl TypeVar {
    pub fn new(id: u32) -> Self {
        Self { id, hint: None, variance: Variance::Invariant }
    }

    pub fn named(id: u32, hint: &str) -> Self {
        Self { id, hint: Some(hint.to_string()), variance: Variance::Invariant }
    }

    pub fn with_variance(id: u32, hint: Option<String>, variance: Variance) -> Self {
        Self { id, hint, variance }
    }
}

impl std::fmt::Display for TypeVar {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
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

impl From<u32> for TypeVar {
    fn from(id: u32) -> Self {
        TypeVar::new(id)
    }
}

/// Literal values for singleton types (Literal[True], Literal[42], etc.)
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum LiteralType {
    Int(i64),
    Bool(bool),
    String(String),
    None,
}

impl std::fmt::Display for LiteralType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LiteralType::Int(n) => write!(f, "Literal[{n}]"),
            LiteralType::Bool(b) => write!(f, "Literal[{}]", if *b { "True" } else { "False" }),
            LiteralType::String(s) => write!(f, "Literal[{s:?}]"),
            LiteralType::None => write!(f, "Literal[None]"),
        }
    }
}

/// Type constructors for built-in and user-defined types
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum TypeCtor {
    Int,
    Float,
    String,
    Bool,
    NoneType,
    List,
    Dict,
    Set,
    Tuple,
    Function,
    /// Gradual typing opt-out type - user explicitly asked for dynamic behavior.
    Any,
    /// Internal unknown type - analyzer could not determine a precise type.
    Unknown,
    /// Top of type lattice - supertype of all types
    Top,
    /// Bottom of type lattice - subtype of all types (uninhabited)
    Never,
    /// Type variable (from Generic[T] or bare T in annotations)
    TypeVariable(String),
    /// Generic base class (parameters stored via Type::App)
    Generic,
    /// Protocol (parameters stored via Type::App, name for named protocols, variance for type params)
    Protocol(Option<String>, Vec<Variance>),
    Class(String),
    Module(String),
    /// Literal singleton type (Literal[True], Literal[42], etc.)
    Literal(LiteralType),
    /// Iterator type: Iterator[ElementType]
    Iterator,
    /// Iterable type: Iterable[ElementType]
    Iterable,
    /// Generator type: Generator[YieldType, SendType, ReturnType]
    Generator,
    /// Async generator type: AsyncGenerator[YieldType, SendType]
    AsyncGenerator,
    /// Coroutine type: Coroutine[YieldType, SendType, ReturnType]
    Coroutine,
}

impl std::fmt::Display for TypeCtor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
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
            TypeCtor::Unknown => write!(f, "Unknown"),
            TypeCtor::Top => write!(f, "Top"),
            TypeCtor::Never => write!(f, "Never"),
            TypeCtor::TypeVariable(name) => write!(f, "{name}"),
            TypeCtor::Generic => write!(f, "Generic"),
            TypeCtor::Protocol(name, _variances) => match name {
                Some(protocol_name) => write!(f, "Protocol<{protocol_name}>"),
                None => write!(f, "Protocol"),
            },
            TypeCtor::Class(name) => write!(f, "{name}"),
            TypeCtor::Module(name) => write!(f, "module<{name}>"),
            TypeCtor::Literal(lit) => write!(f, "{lit}"),
            TypeCtor::Iterator => write!(f, "Iterator"),
            TypeCtor::Iterable => write!(f, "Iterable"),
            TypeCtor::Generator => write!(f, "Generator"),
            TypeCtor::AsyncGenerator => write!(f, "AsyncGenerator"),
            TypeCtor::Coroutine => write!(f, "Coroutine"),
        }
    }
}

impl TypeCtor {
    /// Get the kind of a type constructor
    pub fn kind(&self) -> Kind {
        match self {
            TypeCtor::Int
            | TypeCtor::Float
            | TypeCtor::String
            | TypeCtor::Bool
            | TypeCtor::NoneType
            | TypeCtor::Any
            | TypeCtor::Unknown
            | TypeCtor::Top
            | TypeCtor::Never
            | TypeCtor::TypeVariable(_)
            | TypeCtor::Literal(_) => Kind::Star,
            // * -> *
            TypeCtor::List | TypeCtor::Set => Kind::arity(1),
            // * -> * -> *
            TypeCtor::Dict => Kind::arity(2),
            // Special case: can be 0-ary
            TypeCtor::Tuple => Kind::Star,
            // * -> * -> * (simplified)
            TypeCtor::Function => Kind::arity(2),
            // * -> * (Iterator[ElementType])
            TypeCtor::Iterator => Kind::arity(1),
            TypeCtor::Iterable => Kind::arity(1),
            // * -> * -> * -> * (Generator[YieldType, SendType, ReturnType])
            TypeCtor::Generator => Kind::arity(3),
            // * -> * -> * (AsyncGenerator[YieldType, SendType])
            TypeCtor::AsyncGenerator => Kind::arity(2),
            // * -> * -> * -> * (Coroutine[YieldType, SendType, ReturnType])
            TypeCtor::Coroutine => Kind::arity(3),
            TypeCtor::Generic | TypeCtor::Protocol(_, _) => Kind::Star,
            TypeCtor::Class(_) | TypeCtor::Module(_) => Kind::Star,
        }
    }

    /// Get the variance of a type constructor's type parameter(s).
    ///
    /// For type constructors with multiple parameters (e.g., Dict, Callable),
    /// this returns the variance of the first parameter. Use specific methods
    /// for multi-parameter variance checking.
    ///
    /// - Immutable containers (tuple, frozenset) are covariant
    /// - Mutable containers (list, dict, set) are invariant
    /// - Callable parameters are contravariant, returns are covariant
    /// - Iterators/Iterables are covariant (read-only)
    /// - Protocols use the variance specified in their type parameters
    pub fn variance(&self, param_index: usize) -> Variance {
        match self {
            TypeCtor::Tuple | TypeCtor::Iterator | TypeCtor::Iterable => Variance::Covariant,
            TypeCtor::List | TypeCtor::Set => Variance::Invariant,
            TypeCtor::Dict => Variance::Invariant,
            TypeCtor::Function => {
                if param_index == 0 {
                    Variance::Contravariant
                } else {
                    Variance::Covariant
                }
            }
            TypeCtor::Generator => match param_index {
                0 => Variance::Covariant,
                1 => Variance::Contravariant,
                2 => Variance::Covariant,
                _ => Variance::Invariant,
            },
            TypeCtor::AsyncGenerator => match param_index {
                0 => Variance::Covariant,
                1 => Variance::Contravariant,
                _ => Variance::Invariant,
            },
            TypeCtor::Coroutine => match param_index {
                0 => Variance::Covariant,
                1 => Variance::Contravariant,
                2 => Variance::Covariant,
                _ => Variance::Invariant,
            },
            TypeCtor::Protocol(_name, variances) => variances.get(param_index).copied().unwrap_or(Variance::Invariant),
            _ => Variance::Invariant,
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
                | TypeCtor::Unknown
                | TypeCtor::Top
                | TypeCtor::Never
                | TypeCtor::TypeVariable(_)
                | TypeCtor::Generic
                | TypeCtor::Protocol(_, _)
                | TypeCtor::Literal(_)
                | TypeCtor::Generator
                | TypeCtor::AsyncGenerator
                | TypeCtor::Coroutine
        )
    }

    /// Get the base type of a literal (e.g., Literal[42] -> int)
    pub fn base_type(&self) -> Option<TypeCtor> {
        match self {
            TypeCtor::Literal(LiteralType::Int(_)) => Some(TypeCtor::Int),
            TypeCtor::Literal(LiteralType::Bool(_)) => Some(TypeCtor::Bool),
            TypeCtor::Literal(LiteralType::String(_)) => Some(TypeCtor::String),
            TypeCtor::Literal(LiteralType::None) => Some(TypeCtor::NoneType),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum FunctionParamKind {
    PositionalOnly,
    PositionalOrKeyword,
    VarArgs,
    KeywordOnly,
    KwArgs,
}

impl std::fmt::Display for FunctionParamKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FunctionParamKind::PositionalOnly => write!(f, "positional-only"),
            FunctionParamKind::PositionalOrKeyword => write!(f, "positional-or-keyword"),
            FunctionParamKind::VarArgs => write!(f, "varargs"),
            FunctionParamKind::KeywordOnly => write!(f, "keyword-only"),
            FunctionParamKind::KwArgs => write!(f, "kwargs"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct FunctionParam {
    pub name: String,
    pub ty: Type,
    pub kind: FunctionParamKind,
    pub has_default: bool,
}

impl FunctionParam {
    pub fn new(name: impl Into<String>, ty: Type) -> Self {
        Self { name: name.into(), ty, kind: FunctionParamKind::PositionalOrKeyword, has_default: false }
    }

    pub fn with_metadata(name: impl Into<String>, ty: Type, kind: FunctionParamKind, has_default: bool) -> Self {
        Self { name: name.into(), ty, kind, has_default }
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
    /// args -> return
    /// Each parameter is a (name, type) pair where name is used for keyword argument matching
    Fun(Vec<(String, Type)>, Box<Type>),
    /// Function type with explicit Python parameter metadata.
    FunWithParams(Vec<FunctionParam>, Box<Type>),
    /// Forall quantification (type schemes)
    ForAll(Vec<TypeVar>, Box<Type>),
    /// Union types (for Python's Union[A, B])
    Union(Vec<Type>),
    /// Intersection types (for Protocol1 & Protocol2)
    /// A type satisfying Intersection[A, B] must satisfy both A and B
    Intersection(Vec<Type>),
    /// Record types for objects/classes (row polymorphism)
    /// fields, row variable
    Record(Vec<(String, Type)>, Option<TypeVar>),
    /// Bound method type (receiver_type, method_name, method_type) represents a method bound to an instance.
    /// The method name enables overload resolution during function calls
    ///
    /// e.g., for `f = obj.method` where obj: MyClass and method: (self, int) -> str stores BoundMethod(MyClass, "method", (self, int) -> str)
    BoundMethod(Box<Type>, String, Box<Type>),
    /// Heterogeneous tuple type: tuple[T1, T2, ..., Tn]
    /// For homogeneous tuples, use Type::App(Type::Con(TypeCtor::Tuple), element_type)
    Tuple(Vec<Type>),
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Var(tv) => write!(f, "{tv}"),
            Type::Con(tc) => write!(f, "{tc}"),
            Type::App(t1, t2) => match t1.as_ref() {
                Type::Con(TypeCtor::List) => write!(f, "list[{t2}]"),
                Type::Con(TypeCtor::Set) => write!(f, "set[{t2}]"),
                Type::App(inner, key) if matches!(inner.as_ref(), Type::Con(TypeCtor::Dict)) => {
                    write!(f, "dict[{key}, {t2}]")
                }
                _ => write!(f, "({t1} {t2})"),
            },
            Type::Fun(args, ret) => {
                if args.is_empty() {
                    write!(f, "() -> {ret}")
                } else {
                    let all_auto_generated = args.iter().all(|(name, _)| {
                        name.is_empty() || (name.starts_with('_') && name[1..].chars().all(|c| c.is_ascii_digit()))
                    });

                    if all_auto_generated {
                        if args.len() == 1 {
                            write!(f, "{} -> {}", args[0].1, ret)
                        } else {
                            write!(
                                f,
                                "({}) -> {}",
                                args.iter().map(|(_, t)| t.to_string()).collect::<Vec<_>>().join(", "),
                                ret
                            )
                        }
                    } else if args.len() == 1 {
                        write!(f, "({}: {}) -> {}", args[0].0, args[0].1, ret)
                    } else {
                        write!(
                            f,
                            "({}) -> {}",
                            args.iter()
                                .map(|(name, t)| format!("{name}: {t}"))
                                .collect::<Vec<_>>()
                                .join(", "),
                            ret
                        )
                    }
                }
            }
            Type::FunWithParams(params, ret) => {
                if params.is_empty() {
                    write!(f, "() -> {ret}")
                } else {
                    write!(
                        f,
                        "({}) -> {}",
                        params
                            .iter()
                            .map(|param| {
                                let prefix = match param.kind {
                                    FunctionParamKind::VarArgs => "*",
                                    FunctionParamKind::KwArgs => "**",
                                    _ => "",
                                };
                                let default = if param.has_default { " = ..." } else { "" };
                                format!("{prefix}{}: {}{default}", param.name, param.ty)
                            })
                            .collect::<Vec<_>>()
                            .join(", "),
                        ret
                    )
                }
            }
            Type::ForAll(tvs, t) => {
                if tvs.is_empty() {
                    write!(f, "{t}")
                } else {
                    write!(
                        f,
                        "∀{}. {}",
                        tvs.iter().map(|tv| tv.to_string()).collect::<Vec<_>>().join(" "),
                        t
                    )
                }
            }
            Type::Union(types) => {
                write!(
                    f,
                    "{}",
                    types.iter().map(|t| t.to_string()).collect::<Vec<_>>().join(" | ")
                )
            }
            Type::Intersection(types) => {
                write!(
                    f,
                    "{}",
                    types.iter().map(|t| t.to_string()).collect::<Vec<_>>().join(" & ")
                )
            }
            Type::Record(fields, row_var) => {
                let field_strs: Vec<String> = fields.iter().map(|(k, v)| format!("{k}: {v}")).collect();
                match row_var {
                    Some(rv) => write!(f, "{{ {} | {} }}", field_strs.join(", "), rv),
                    None => write!(f, "{{ {} }}", field_strs.join(", ")),
                }
            }
            Type::BoundMethod(receiver, method_name, method) => {
                write!(f, "BoundMethod[{receiver}, {method_name}, {method}]")
            }
            Type::Tuple(types) => {
                write!(
                    f,
                    "tuple[{}]",
                    types.iter().map(|t| t.to_string()).collect::<Vec<_>>().join(", ")
                )
            }
        }
    }
}
