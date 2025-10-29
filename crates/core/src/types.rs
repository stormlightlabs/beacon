//! Type System Implementation for Beacon
//!
//! This module implements a Hindley-Milner type system with extensions for Python,
//! including row-polymorphic records, union types, and gradual typing support.
//!
//! ## Type Hierarchy
//!
//! The type system distinguishes three special types for gradual typing and lattice semantics:
//!
//! - **`Any`**: Gradual typing "unknown" type that unifies with everything. Used when type
//!   information is unavailable or when interfacing with untyped code. Acts as both a
//!   supertype and subtype in the gradual typing system.
//!
//! - **`Top`**: The top of the type lattice - a supertype of all types. Unlike `Any`, `Top`
//!   only unifies with itself and type variables. It represents "any value" in the type
//!   hierarchy.
//!
//! - **`Never`**: The bottom of the type lattice - a subtype of all types. Represents
//!   uninhabited types (e.g., functions that never return). Only unifies with itself and
//!   type variables.
//!
//! ## Kind System
//!
//! Types are classified by kinds:
//!
//! - `*` (Star): The kind of proper types (e.g., `int`, `str`, `list[int]`)
//! - `* -> *`: The kind of unary type constructors (e.g., `list`, `set`)
//! - `* -> * -> *`: The kind of binary type constructors (e.g., `dict`)
//!
//! Type applications must be well-kinded: applying a type constructor `F :: k1 -> k2` to
//! an argument `A :: k1` produces a result of kind `k2`.
//!
//! ## Row-Polymorphic Records
//!
//! Records support row polymorphism for flexible structural typing:
//!
//! ```text
//! { x: int, y: str }           -- Closed record
//! { x: int | r }                -- Open record with row variable r
//! ```
//!
//! Row variables allow record extension and flexible matching:
//!
//! ```text
//! { x: int | r } ~ { x: int, y: str }
//! // Unifies r with { y: str }
//! ```
//!
//! ## Value Restriction
//!
//! To ensure soundness, generalization follows the value restriction: only non-expansive
//! expressions can be generalized. In Python:
//!
//! **Non-expansive (safe to generalize):**
//! - Literals: `42`, `"hello"`, `True`, `None`
//! - Lambda expressions: `lambda x: x`
//! - Variable references
//! - Constructor applications with non-expansive arguments
//!
//! **Expansive (not generalized):**
//! - Function calls: `f()`
//! - Method calls: `obj.method()`
//! - Attribute access: `obj.attr`
//! - Most other computations
//!
//! ## Example
//!
//! ```
//! use beacon_core::{Type, TypeScheme, TypeVar, TypeVarGen};
//! use rustc_hash::FxHashMap;
//!
//! fn example() {
//!     // Create the identity function type: 'a -> 'a
//!     let mut var_gen = TypeVarGen::new();
//!     let tv_a = var_gen.fresh();
//!     let identity_type = Type::fun(
//!         vec![Type::Var(tv_a.clone())],
//!         Type::Var(tv_a.clone())
//!     );
//!
//!     // Generalize it to a type scheme: ∀'a. 'a -> 'a
//!     let env = FxHashMap::default();
//!     let identity_scheme = TypeScheme::generalize(identity_type, &env);
//!
//!     assert_eq!(identity_scheme.quantified_vars.len(), 1);
//! }
//! ```

use rustc_hash::FxHashMap;
use std::{fmt, ops::Not};

/// Kinds classify types in the type system
/// Star (*) is the kind of proper types like Int, String
/// Arrow kinds (K1 -> K2) are for type constructors like List, Dict
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Kind {
    /// Kind of proper types (*)
    Star,
    /// Kind of type constructors (K1 -> K2)
    Arrow(Box<Kind>, Box<Kind>),
}

impl fmt::Display for Kind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
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
        Self { id, hint: Some(hint.to_string()) }
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
    /// Gradual typing "unknown" type - acts as both supertype and subtype
    Any,
    /// Top of type lattice - supertype of all types
    Top,
    /// Bottom of type lattice - subtype of all types (uninhabited)
    Never,
    /// Type variable (from Generic[T] or bare T in annotations)
    TypeVariable(String),
    /// Generic base class (parameters stored via Type::App)
    Generic,
    /// Protocol (parameters stored via Type::App, name for named protocols)
    Protocol(Option<String>),
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
            TypeCtor::Top => write!(f, "Top"),
            TypeCtor::Never => write!(f, "Never"),
            TypeCtor::TypeVariable(name) => write!(f, "{name}"),
            TypeCtor::Generic => write!(f, "Generic"),
            TypeCtor::Protocol(name) => {
                if let Some(protocol_name) = name {
                    write!(f, "Protocol<{protocol_name}>")
                } else {
                    write!(f, "Protocol")
                }
            }
            TypeCtor::Class(name) => write!(f, "{name}"),
            TypeCtor::Module(name) => write!(f, "module<{name}>"),
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
            | TypeCtor::Top
            | TypeCtor::Never
            | TypeCtor::TypeVariable(_) => Kind::Star,
            // * -> *
            TypeCtor::List | TypeCtor::Set => Kind::arity(1),
            // * -> * -> *
            TypeCtor::Dict => Kind::arity(2),
            // Special case: can be 0-ary
            TypeCtor::Tuple => Kind::Star,
            // * -> * -> * (simplified)
            TypeCtor::Function => Kind::arity(2),
            // Generic and Protocol are just markers, kind is Star
            TypeCtor::Generic | TypeCtor::Protocol(_) => Kind::Star,
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
                | TypeCtor::Top
                | TypeCtor::Never
                | TypeCtor::TypeVariable(_)
                | TypeCtor::Generic
                | TypeCtor::Protocol(_)
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
    /// args -> return
    Fun(Vec<Type>, Box<Type>),
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
    /// Bound method type (receiver_type, method_type)
    /// Represents a method bound to an instance
    /// e.g., for `f = obj.method` where obj: MyClass and method: (self, int) -> str
    /// stores BoundMethod(MyClass, (self, int) -> str)
    BoundMethod(Box<Type>, Box<Type>),
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
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
                } else if args.len() == 1 {
                    write!(f, "{} -> {}", args[0], ret)
                } else {
                    write!(
                        f,
                        "({}) -> {}",
                        args.iter().map(|t| t.to_string()).collect::<Vec<_>>().join(", "),
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
            Type::BoundMethod(receiver, method) => {
                write!(f, "BoundMethod[{receiver}, {method}]")
            }
        }
    }
}

impl Type {
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

    pub fn top() -> Self {
        Type::Con(TypeCtor::Top)
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
            Box::new(Type::App(Box::new(Type::Con(TypeCtor::Dict)), Box::new(key_type))),
            Box::new(value_type),
        )
    }

    /// Create a function type
    pub fn fun(args: Vec<Type>, ret: Type) -> Self {
        Type::Fun(args, Box::new(ret))
    }

    /// Create a union type, flattening nested unions, removing duplications, and sorting for canonical form
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

        if flattened.len() == 1 { flattened.pop().unwrap() } else { Type::Union(flattened) }
    }

    /// Create an intersection type, flattening nested intersections, removing duplications, and sorting for canonical form
    ///
    /// Intersection types represent values that satisfy multiple type constraints simultaneously.
    /// For protocols, `Intersection[Protocol1, Protocol2]` means a type must satisfy both protocols.
    ///
    /// # Examples
    /// ```
    /// use beacon_core::Type;
    ///
    /// let proto1 = Type::Con(beacon_core::TypeCtor::Protocol(Some("Iterable".to_string())));
    /// let proto2 = Type::Con(beacon_core::TypeCtor::Protocol(Some("Sized".to_string())));
    /// let both = Type::intersection(vec![proto1, proto2]); // Must be both Iterable and Sized
    /// ```
    pub fn intersection(mut types: Vec<Type>) -> Self {
        if types.len() == 1 {
            return types.pop().unwrap();
        }

        let mut flattened = Vec::new();
        for t in types {
            match t {
                Type::Intersection(inner) => flattened.extend(inner),
                other => flattened.push(other),
            }
        }

        flattened.sort();
        flattened.dedup();

        if flattened.len() == 1 { flattened.pop().unwrap() } else { Type::Intersection(flattened) }
    }

    /// Create Optional[T] which is Union[T, None]
    pub fn optional(t: Type) -> Self {
        Type::union(vec![t, Type::none()])
    }

    /// Check if this type is Optional[T] (i.e., Union[T, None])
    ///
    /// Returns true if the type is a union containing exactly None and one other type.
    pub fn is_optional(&self) -> bool {
        match self {
            Type::Union(types) => types.len() == 2 && types.iter().any(|t| matches!(t, Type::Con(TypeCtor::NoneType))),
            _ => false,
        }
    }

    /// Extract T from Optional[T] (Union[T, None])
    ///
    /// Returns Some(T) if this is Optional[T], None otherwise.
    pub fn unwrap_optional(&self) -> Option<Type> {
        match self {
            Type::Union(types) if types.len() == 2 => {
                let none_idx = types.iter().position(|t| matches!(t, Type::Con(TypeCtor::NoneType)))?;
                let other_idx = 1 - none_idx;
                Some(types[other_idx].clone())
            }
            _ => None,
        }
    }

    /// Remove a type from a union
    ///
    /// Returns a new type with the specified type removed from the union.
    /// If removing the type leaves a single type, returns that type unwrapped.
    /// If the type is not a union, returns self unchanged.
    ///
    /// # Examples
    /// ```
    /// use beacon_core::Type;
    /// let opt_int = Type::optional(Type::int()); // Union[int, None]
    /// let just_int = opt_int.remove_from_union(&Type::none()); // int
    /// ```
    pub fn remove_from_union(&self, to_remove: &Type) -> Type {
        match self {
            Type::Union(types) => {
                let filtered: Vec<Type> = types.iter().filter(|t| *t != to_remove).cloned().collect();
                if filtered.is_empty() {
                    Type::never()
                } else if filtered.len() == 1 {
                    filtered.into_iter().next().unwrap()
                } else {
                    Type::Union(filtered)
                }
            }
            other => {
                if other == to_remove {
                    Type::never()
                } else {
                    other.clone()
                }
            }
        }
    }

    /// Get all free type variables in this type
    pub fn free_vars(&self) -> FxHashMap<TypeVar, ()> {
        let mut vars = FxHashMap::default();
        self.collect_free_vars(&mut vars, &FxHashMap::default());
        vars
    }

    /// Compute the kind of this type
    ///
    /// Returns an error if the type is ill-kinded (e.g., `Int[String]`)
    pub fn kind_of(&self) -> Result<Kind, String> {
        match self {
            Type::Var(_) => Ok(Kind::Star),
            Type::Con(tc) => Ok(tc.kind()),

            Type::App(f, a) => {
                let f_kind = f.kind_of()?;
                let a_kind = a.kind_of()?;

                match f_kind {
                    Kind::Arrow(k1, k2) => {
                        if *k1 == a_kind {
                            Ok(*k2)
                        } else {
                            Err(format!(
                                "Kind mismatch in type application: expected {k1}, found {a_kind}"
                            ))
                        }
                    }
                    Kind::Star => Err(format!("Cannot apply type {f} :: * to argument {a}")),
                }
            }
            Type::Fun(_, _) => Ok(Kind::Star),
            Type::ForAll(_, t) => t.kind_of(),
            Type::Union(_) => Ok(Kind::Star),
            Type::Intersection(_) => Ok(Kind::Star),
            Type::Record(_, _) => Ok(Kind::Star),
            Type::BoundMethod(_, _) => Ok(Kind::Star),
        }
    }

    /// Check if this type is well-kinded
    ///
    /// A type is well-kinded if:
    /// 1. All type applications are kind-correct
    /// 2. All subterms are well-kinded
    /// 3. The type has kind * (is a proper type)
    pub fn check_well_kinded(&self) -> Result<(), String> {
        let kind = self.kind_of()?;
        match kind {
            Kind::Star => Ok(()),
            _ => Err(format!("Type {self} has kind {kind} but expected kind *")),
        }
    }

    fn collect_free_vars(&self, vars: &mut FxHashMap<TypeVar, ()>, bound: &FxHashMap<TypeVar, ()>) {
        match self {
            Type::Var(tv) => {
                if bound.contains_key(tv).not() {
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
            Type::Intersection(types) => {
                for t in types {
                    t.collect_free_vars(vars, bound);
                }
            }
            Type::Record(fields, row_var) => {
                for (_, t) in fields {
                    t.collect_free_vars(vars, bound);
                }
                if let Some(rv) = row_var {
                    if bound.contains_key(rv).not() {
                        vars.insert(rv.clone(), ());
                    }
                }
            }
            Type::BoundMethod(receiver, method) => {
                receiver.collect_free_vars(vars, bound);
                method.collect_free_vars(vars, bound);
            }
        }
    }

    /// Check if this type is a subtype of another type.
    ///
    /// Implements structural subtyping with variance rules:
    /// - Never is a subtype of all types (bottom type)
    /// - Top is a supertype of all types (top type)
    /// - Any is a supertype of all types (gradual typing) but not a universal subtype
    /// - Function types are contravariant in parameters, covariant in return
    /// - Type constructors require structural equality
    /// - Union types: T <: Union[A, B] if T <: A or T <: B
    /// - Union types: Union[A, B] <: T if both A <: T and B <: T
    ///
    /// Note on Any: For gradual typing, Any acts as a supertype (T <: Any for all T), enabling
    /// flexible parameter types. However, Any is not a subtype of other types, which ensures
    /// proper variance checking for protocol method signatures.
    ///
    /// # Examples
    /// ```
    /// use beacon_core::Type;
    ///
    /// // Never is subtype of everything
    /// assert!(Type::never().is_subtype_of(&Type::int()));
    ///
    /// // Everything is subtype of Top
    /// assert!(Type::int().is_subtype_of(&Type::top()));
    ///
    /// // Function parameter contravariance
    /// let f1 = Type::fun(vec![Type::int()], Type::string());
    /// let f2 = Type::fun(vec![Type::any()], Type::string());
    /// assert!(f2.is_subtype_of(&f1)); // Can substitute f2 for f1
    /// ```
    pub fn is_subtype_of(&self, other: &Type) -> bool {
        match (self, other) {
            (Type::Con(TypeCtor::Never), _) => return true,
            (_, Type::Con(TypeCtor::Top)) | (_, Type::Con(TypeCtor::Any)) => return true,
            (_, Type::Con(TypeCtor::Never)) => return false,
            (Type::Con(TypeCtor::Top), _) => return false,
            (Type::Con(TypeCtor::Any), _) => return false,
            _ => {}
        }

        if self == other {
            return true;
        }

        match (self, other) {
            // Union[A, B] <: T if both A <: T and B <: T
            (Type::Union(xs), _) => return xs.iter().all(|x| x.is_subtype_of(other)),
            // T <: Union[A, B] if T <: A or T <: B
            (_, Type::Union(ys)) => return ys.iter().any(|y| self.is_subtype_of(y)),
            _ => {}
        }

        match (self, other) {
            // Intersection[A, B, ...] <: T if ANY component is a subtype of T
            // Rationale: Intersection is more specific than any of its components
            (Type::Intersection(xs), _) => return xs.iter().any(|x| x.is_subtype_of(other)),
            // T <: Intersection[A, B, ...] if T is a subtype of ALL components
            // Rationale: T must satisfy all requirements in the intersection
            (_, Type::Intersection(ys)) => return ys.iter().all(|y| self.is_subtype_of(y)),
            _ => {}
        }

        if let (Type::Fun(self_params, self_ret), Type::Fun(other_params, other_ret)) = (self, other) {
            if self_params.len() != other_params.len() {
                return false;
            }

            let params_ok = self_params.iter().zip(other_params).all(|(s, o)| o.is_subtype_of(s));
            if !params_ok {
                return false;
            }

            return self_ret.is_subtype_of(other_ret);
        }

        if let (Type::App(self_ctor, self_arg), Type::App(other_ctor, other_arg)) = (self, other) {
            return self_ctor.is_subtype_of(other_ctor) && self_arg.is_subtype_of(other_arg);
        }

        if let (Type::Record(self_fields, _), Type::Record(other_fields, _)) = (self, other) {
            for (k_other, t_other) in other_fields {
                match self_fields.iter().find(|(k, _)| k == k_other) {
                    Some((_, t_self)) if t_self.is_subtype_of(t_other) => continue,
                    _ => return false,
                }
            }
            return true;
        }

        if let (Type::BoundMethod(self_recv, self_meth), Type::BoundMethod(other_recv, other_meth)) = (self, other) {
            return self_recv.is_subtype_of(other_recv) && self_meth.is_subtype_of(other_meth);
        }

        if let (Type::ForAll(_, self_body), Type::ForAll(_, other_body)) = (self, other) {
            return self_body.is_subtype_of(other_body);
        }

        false
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
        Self { quantified_vars, ty }
    }

    /// Create a monomorphic type scheme (no quantified variables)
    pub fn mono(ty: Type) -> Self {
        Self::new(Vec::new(), ty)
    }

    /// Generalize a type into a type scheme by quantifying over free variables
    ///
    /// This respects the value restriction: if `is_non_expansive` is false,
    /// the type is not generalized (returns a monomorphic scheme).
    pub fn generalize(ty: Type, env_vars: &FxHashMap<TypeVar, ()>) -> Self {
        Self::generalize_with_restriction(ty, env_vars, true)
    }

    /// Generalize a type with explicit control over value restriction
    ///
    /// ## Arguments
    /// * `ty` - The type to generalize
    /// * `env_vars` - Variables bound in the environment (not generalized)
    /// * `is_non_expansive` - Whether the expression is non-expansive (value restriction)
    ///
    /// If `is_non_expansive` is false, returns a monomorphic scheme (no generalization).
    /// This implements the value restriction to ensure soundness.
    pub fn generalize_with_restriction(ty: Type, env_vars: &FxHashMap<TypeVar, ()>, is_non_expansive: bool) -> Self {
        if !is_non_expansive {
            return Self::mono(ty);
        }

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
        assert_eq!(Kind::arity(1), Kind::Arrow(Box::new(Kind::Star), Box::new(Kind::Star)));
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
        assert_eq!(Type::dict(Type::string(), Type::int()).to_string(), "dict[str, int]");

        let fun = Type::fun(vec![Type::int()], Type::bool());
        assert_eq!(fun.to_string(), "int -> bool");
    }

    #[test]
    fn test_type_constructor_kinds() {
        assert_eq!(TypeCtor::Int.kind(), Kind::Star);
        assert_eq!(TypeCtor::List.kind(), Kind::arity(1));
        assert_eq!(TypeCtor::Dict.kind(), Kind::arity(2));
    }

    #[test]
    fn test_kind_display() {
        assert_eq!(Kind::Star.to_string(), "*");
        assert_eq!(
            Kind::Arrow(Box::new(Kind::Star), Box::new(Kind::Star)).to_string(),
            "* -> *"
        );
        assert_eq!(Kind::arity(2).to_string(), "* -> * -> *");
    }

    #[test]
    fn test_type_var_display_with_hint() {
        let tv_no_hint = TypeVar::new(5);
        let tv_with_hint = TypeVar::named(5, "alpha");

        assert_eq!(tv_no_hint.to_string(), "'t5");
        assert_eq!(tv_with_hint.to_string(), "'alpha5");
    }

    #[test]
    fn test_type_constructor_display() {
        assert_eq!(TypeCtor::Int.to_string(), "int");
        assert_eq!(TypeCtor::Float.to_string(), "float");
        assert_eq!(TypeCtor::String.to_string(), "str");
        assert_eq!(TypeCtor::Bool.to_string(), "bool");
        assert_eq!(TypeCtor::NoneType.to_string(), "None");
        assert_eq!(TypeCtor::Any.to_string(), "Any");
        assert_eq!(TypeCtor::Never.to_string(), "Never");
        assert_eq!(TypeCtor::Class("MyClass".to_string()).to_string(), "MyClass");
        assert_eq!(TypeCtor::Module("os".to_string()).to_string(), "module<os>");
    }

    #[test]
    fn test_complex_type_app_display() {
        let nested_list = Type::list(Type::list(Type::int()));
        assert_eq!(nested_list.to_string(), "list[list[int]]");

        let dict_str_list = Type::dict(Type::string(), Type::list(Type::int()));
        assert_eq!(dict_str_list.to_string(), "dict[str, list[int]]");
    }

    #[test]
    fn test_function_type_display() {
        let no_args = Type::fun(vec![], Type::int());
        assert_eq!(no_args.to_string(), "() -> int");

        let one_arg = Type::fun(vec![Type::string()], Type::bool());
        assert_eq!(one_arg.to_string(), "str -> bool");

        let multi_args = Type::fun(vec![Type::int(), Type::string(), Type::bool()], Type::none());
        assert_eq!(multi_args.to_string(), "(int, str, bool) -> None");
    }

    #[test]
    fn test_forall_display() {
        let tv1 = TypeVar::new(0);
        let tv2 = TypeVar::new(1);

        let simple_forall = Type::ForAll(vec![tv1.clone()], Box::new(Type::Var(tv1.clone())));
        assert_eq!(simple_forall.to_string(), "∀'t0. 't0");

        let multi_forall = Type::ForAll(
            vec![tv1.clone(), tv2.clone()],
            Box::new(Type::fun(vec![Type::Var(tv1)], Type::Var(tv2))),
        );
        assert_eq!(multi_forall.to_string(), "∀'t0 't1. 't0 -> 't1");
    }

    #[test]
    fn test_union_display() {
        let simple_union = Type::union(vec![Type::int(), Type::string()]);
        assert_eq!(simple_union.to_string(), "int | str");

        let multi_union = Type::union(vec![Type::int(), Type::string(), Type::bool(), Type::none()]);
        let display_str = multi_union.to_string();
        assert!(display_str.contains("int"));
        assert!(display_str.contains("str"));
        assert!(display_str.contains("bool"));
        assert!(display_str.contains("None"));
    }

    #[test]
    fn test_record_display() {
        let simple_record = Type::Record(
            vec![("x".to_string(), Type::int()), ("y".to_string(), Type::string())],
            None,
        );
        assert_eq!(simple_record.to_string(), "{ x: int, y: str }");

        let row_var = TypeVar::new(0);
        let record_with_row = Type::Record(vec![("x".to_string(), Type::int())], Some(row_var));
        assert_eq!(record_with_row.to_string(), "{ x: int | 't0 }");

        let empty_record = Type::Record(vec![], None);
        assert_eq!(empty_record.to_string(), "{  }");
    }

    #[test]
    fn test_type_scheme_display() {
        let tv = TypeVar::new(0);

        let mono_scheme = TypeScheme::mono(Type::int());
        assert_eq!(mono_scheme.to_string(), "int");

        let poly_scheme = TypeScheme::new(vec![tv.clone()], Type::fun(vec![Type::Var(tv.clone())], Type::Var(tv)));
        assert_eq!(poly_scheme.to_string(), "∀'t0. 't0 -> 't0");
    }

    #[test]
    fn test_set_type_display() {
        let set_int = Type::App(Box::new(Type::Con(TypeCtor::Set)), Box::new(Type::int()));
        assert_eq!(set_int.to_string(), "set[int]");

        let set_str = Type::App(Box::new(Type::Con(TypeCtor::Set)), Box::new(Type::string()));
        assert_eq!(set_str.to_string(), "set[str]");

        let nested_set = Type::App(Box::new(Type::Con(TypeCtor::Set)), Box::new(Type::list(Type::bool())));
        assert_eq!(nested_set.to_string(), "set[list[bool]]");
    }

    #[test]
    fn test_generic_type_app_display() {
        let tv = TypeVar::new(0);
        let generic_app = Type::App(Box::new(Type::Var(tv)), Box::new(Type::int()));
        assert_eq!(generic_app.to_string(), "('t0 int)");

        let fun_ctor = Type::Con(TypeCtor::Function);
        let fun_app = Type::App(Box::new(fun_ctor), Box::new(Type::string()));
        assert_eq!(fun_app.to_string(), "(function str)");
    }

    #[test]
    fn test_top_type_constructor() {
        let top = Type::top();
        assert_eq!(top, Type::Con(TypeCtor::Top));
        assert_eq!(top.to_string(), "Top");
        assert_eq!(TypeCtor::Top.kind(), Kind::Star);
        assert!(TypeCtor::Top.is_builtin());
    }

    #[test]
    fn test_any_type_constructor() {
        let any = Type::any();
        assert_eq!(any, Type::Con(TypeCtor::Any));
        assert_eq!(any.to_string(), "Any");
        assert_eq!(TypeCtor::Any.kind(), Kind::Star);
        assert!(TypeCtor::Any.is_builtin());
    }

    #[test]
    fn test_never_type_constructor() {
        let never = Type::never();
        assert_eq!(never, Type::Con(TypeCtor::Never));
        assert_eq!(never.to_string(), "Never");
        assert_eq!(TypeCtor::Never.kind(), Kind::Star);
        assert!(TypeCtor::Never.is_builtin());
    }

    #[test]
    fn test_top_any_never_are_distinct() {
        let top = Type::top();
        let any = Type::any();
        let never = Type::never();

        assert_ne!(top, any);
        assert_ne!(top, never);
        assert_ne!(any, never);
    }

    #[test]
    fn test_lattice_types_have_no_free_vars() {
        assert_eq!(Type::top().free_vars().len(), 0);
        assert_eq!(Type::any().free_vars().len(), 0);
        assert_eq!(Type::never().free_vars().len(), 0);
    }

    #[test]
    fn test_well_kinded_simple_types() {
        assert!(Type::int().check_well_kinded().is_ok());
        assert!(Type::string().check_well_kinded().is_ok());
        assert!(Type::bool().check_well_kinded().is_ok());
        assert!(Type::any().check_well_kinded().is_ok());
        assert!(Type::top().check_well_kinded().is_ok());
        assert!(Type::never().check_well_kinded().is_ok());
    }

    #[test]
    fn test_well_kinded_type_applications() {
        let list_int = Type::list(Type::int());
        assert!(list_int.check_well_kinded().is_ok());
        assert_eq!(list_int.kind_of().unwrap(), Kind::Star);

        let dict_str_int = Type::dict(Type::string(), Type::int());
        assert!(dict_str_int.check_well_kinded().is_ok());
        assert_eq!(dict_str_int.kind_of().unwrap(), Kind::Star);

        let nested_list = Type::list(Type::list(Type::int()));
        assert!(nested_list.check_well_kinded().is_ok());
    }

    #[test]
    fn test_ill_kinded_type_applications() {
        let int_ctor = Type::Con(TypeCtor::Int);
        let ill_kinded = Type::App(Box::new(int_ctor), Box::new(Type::string()));
        assert!(ill_kinded.check_well_kinded().is_err());

        let unapplied_list = Type::Con(TypeCtor::List);
        assert!(unapplied_list.check_well_kinded().is_err());

        let dict_ctor = Type::Con(TypeCtor::Dict);
        let partial_dict = Type::App(Box::new(dict_ctor), Box::new(Type::int()));
        assert!(partial_dict.check_well_kinded().is_err());
    }

    #[test]
    fn test_kind_of_type_constructors() {
        assert_eq!(Type::Con(TypeCtor::Int).kind_of().unwrap(), Kind::Star);
        assert_eq!(Type::Con(TypeCtor::List).kind_of().unwrap(), Kind::arity(1));
        assert_eq!(Type::Con(TypeCtor::Dict).kind_of().unwrap(), Kind::arity(2));
    }

    #[test]
    fn test_kind_of_function_types() {
        let fun_type = Type::fun(vec![Type::int(), Type::string()], Type::bool());
        assert_eq!(fun_type.kind_of().unwrap(), Kind::Star);
        assert!(fun_type.check_well_kinded().is_ok());
    }

    #[test]
    fn test_kind_of_union_types() {
        let union = Type::union(vec![Type::int(), Type::string()]);
        assert_eq!(union.kind_of().unwrap(), Kind::Star);
        assert!(union.check_well_kinded().is_ok());
    }

    #[test]
    fn test_kind_of_record_types() {
        let record = Type::Record(vec![("x".to_string(), Type::int())], None);
        assert_eq!(record.kind_of().unwrap(), Kind::Star);
        assert!(record.check_well_kinded().is_ok());
    }

    #[test]
    fn test_kind_of_forall_types() {
        let tv = TypeVar::new(0);
        let forall = Type::ForAll(vec![tv.clone()], Box::new(Type::Var(tv)));
        assert_eq!(forall.kind_of().unwrap(), Kind::Star);
        assert!(forall.check_well_kinded().is_ok());
    }

    #[test]
    fn test_kind_mismatch_in_application() {
        let list_ctor = Type::Con(TypeCtor::List);
        let dict_ctor = Type::Con(TypeCtor::Dict);
        let mismatched = Type::App(Box::new(list_ctor), Box::new(dict_ctor));

        let result = mismatched.kind_of();
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("Kind mismatch"));
    }

    #[test]
    fn test_union_normalization_flattens_nested_unions() {
        let inner_union = Type::union(vec![Type::string(), Type::bool()]);
        let outer_union = Type::union(vec![Type::int(), inner_union]);

        match outer_union {
            Type::Union(types) => {
                assert_eq!(types.len(), 3);
                assert!(types.contains(&Type::bool()));
                assert!(types.contains(&Type::int()));
                assert!(types.contains(&Type::string()));
            }
            _ => panic!("Expected union type"),
        }
    }

    #[test]
    fn test_union_normalization_removes_duplicates() {
        let union = Type::union(vec![Type::int(), Type::string(), Type::int(), Type::string()]);

        match union {
            Type::Union(types) => {
                assert_eq!(types.len(), 2);
                assert!(types.contains(&Type::int()));
                assert!(types.contains(&Type::string()));
            }
            _ => panic!("Expected union type"),
        }
    }

    #[test]
    fn test_union_normalization_is_sorted() {
        let union = Type::union(vec![Type::string(), Type::bool(), Type::int()]);

        match union {
            Type::Union(types) => {
                for i in 1..types.len() {
                    assert!(types[i - 1] <= types[i], "Union types should be sorted");
                }
            }
            _ => panic!("Expected union type"),
        }
    }

    #[test]
    fn test_union_normalization_idempotence() {
        let union1 = Type::union(vec![Type::int(), Type::string()]);
        let union2 = Type::union(vec![Type::int(), Type::string()]);

        assert_eq!(union1, union2);

        if let Type::Union(ref types1) = union1 {
            let union3 = Type::union(types1.clone());
            assert_eq!(union1, union3);
        }
    }

    #[test]
    fn test_union_single_element_unwraps() {
        let union = Type::union(vec![Type::int()]);
        assert_eq!(union, Type::int());
    }

    #[test]
    fn test_optional_is_normalized_union() {
        let opt = Type::optional(Type::int());

        match opt {
            Type::Union(types) => {
                assert_eq!(types.len(), 2);
                assert!(types.contains(&Type::int()));
                assert!(types.contains(&Type::none()));
            }
            _ => panic!("Expected union for optional"),
        }
    }

    #[test]
    fn test_is_optional() {
        let opt = Type::optional(Type::int());
        assert!(opt.is_optional());

        let not_opt = Type::int();
        assert!(!not_opt.is_optional());

        let union = Type::union(vec![Type::int(), Type::string()]);
        assert!(!union.is_optional());

        let triple_union = Type::union(vec![Type::int(), Type::string(), Type::none()]);
        assert!(!triple_union.is_optional());
    }

    #[test]
    fn test_unwrap_optional() {
        let opt_int = Type::optional(Type::int());
        assert_eq!(opt_int.unwrap_optional(), Some(Type::int()));

        let opt_str = Type::optional(Type::string());
        assert_eq!(opt_str.unwrap_optional(), Some(Type::string()));

        let not_opt = Type::int();
        assert_eq!(not_opt.unwrap_optional(), None);

        let union = Type::union(vec![Type::int(), Type::string()]);
        assert_eq!(union.unwrap_optional(), None);
    }

    #[test]
    fn test_remove_from_union_basic() {
        let opt_int = Type::optional(Type::int());
        let just_int = opt_int.remove_from_union(&Type::none());
        assert_eq!(just_int, Type::int());
    }

    #[test]
    fn test_remove_from_union_multi() {
        let union = Type::union(vec![Type::int(), Type::string(), Type::bool()]);
        let without_str = union.remove_from_union(&Type::string());

        match without_str {
            Type::Union(types) => {
                assert_eq!(types.len(), 2);
                assert!(types.contains(&Type::int()));
                assert!(types.contains(&Type::bool()));
            }
            _ => panic!("Expected union type"),
        }
    }

    #[test]
    fn test_remove_from_union_non_union() {
        let int_type = Type::int();
        let result = int_type.remove_from_union(&Type::string());
        assert_eq!(result, Type::int());

        let result2 = int_type.remove_from_union(&Type::int());
        assert_eq!(result2, Type::never());
    }

    #[test]
    fn test_remove_from_union_all_elements() {
        let union = Type::union(vec![Type::int(), Type::string()]);
        let without_int = union.remove_from_union(&Type::int());
        let without_both = without_int.remove_from_union(&Type::string());
        assert_eq!(without_both, Type::never());
    }

    #[test]
    fn test_type_scheme_generalization_basic() {
        let tv = TypeVar::new(0);
        let ty = Type::fun(vec![Type::Var(tv.clone())], Type::Var(tv.clone()));

        let env_vars = FxHashMap::default();
        let scheme = TypeScheme::generalize(ty.clone(), &env_vars);

        assert_eq!(scheme.quantified_vars.len(), 1);
        assert_eq!(scheme.quantified_vars[0], tv);
        assert_eq!(scheme.ty, ty);
    }

    #[test]
    fn test_type_scheme_generalization_respects_environment() {
        let tv_a = TypeVar::new(0);
        let tv_b = TypeVar::new(1);
        let ty = Type::fun(vec![Type::Var(tv_a.clone())], Type::Var(tv_b.clone()));

        let mut env_vars = FxHashMap::default();
        env_vars.insert(tv_a, ()); // 'a is bound in environment

        let scheme = TypeScheme::generalize(ty, &env_vars);

        assert_eq!(scheme.quantified_vars.len(), 1);
        assert_eq!(scheme.quantified_vars[0], tv_b);
    }

    #[test]
    fn test_type_scheme_monomorphic() {
        let ty = Type::int();
        let scheme = TypeScheme::mono(ty.clone());

        assert_eq!(scheme.quantified_vars.len(), 0);
        assert_eq!(scheme.ty, ty);
    }

    #[test]
    fn test_type_scheme_display_polymorphic() {
        let tv = TypeVar::new(0);
        let ty = Type::fun(vec![Type::Var(tv.clone())], Type::Var(tv.clone()));
        let scheme = TypeScheme::new(vec![tv], ty);

        let display = scheme.to_string();
        assert!(display.contains("∀"));
        assert!(display.contains("'t0"));
    }

    #[test]
    fn test_type_scheme_display_monomorphic() {
        let scheme = TypeScheme::mono(Type::int());
        let display = scheme.to_string();
        assert!(!display.contains("∀"));
        assert_eq!(display, "int");
    }

    #[test]
    fn test_generalization_with_complex_types() {
        let tv = TypeVar::new(5);
        let ty = Type::list(Type::Var(tv.clone()));

        let env_vars = FxHashMap::default();
        let scheme = TypeScheme::generalize(ty, &env_vars);

        assert_eq!(scheme.quantified_vars.len(), 1);
        assert_eq!(scheme.quantified_vars[0], tv);
    }

    #[test]
    fn test_generalization_with_multiple_vars() {
        let tv_a = TypeVar::new(10);
        let tv_b = TypeVar::new(11);
        let ty = Type::dict(Type::Var(tv_a.clone()), Type::Var(tv_b.clone()));

        let env_vars = FxHashMap::default();
        let scheme = TypeScheme::generalize(ty, &env_vars);

        assert_eq!(scheme.quantified_vars.len(), 2);
        assert!(scheme.quantified_vars.contains(&tv_a));
        assert!(scheme.quantified_vars.contains(&tv_b));
    }

    #[test]
    fn test_generalization_excludes_concrete_types() {
        let tv = TypeVar::new(20);
        let ty = Type::fun(vec![Type::int()], Type::Var(tv.clone()));

        let env_vars = FxHashMap::default();
        let scheme = TypeScheme::generalize(ty, &env_vars);

        assert_eq!(scheme.quantified_vars.len(), 1);
        assert_eq!(scheme.quantified_vars[0], tv);
    }

    #[test]
    fn test_value_restriction_prevents_generalization() {
        let tv = TypeVar::new(30);
        let ty = Type::fun(vec![Type::Var(tv.clone())], Type::Var(tv.clone()));

        let env_vars = FxHashMap::default();
        let scheme_non_expansive = TypeScheme::generalize_with_restriction(ty.clone(), &env_vars, true);

        assert_eq!(scheme_non_expansive.quantified_vars.len(), 1);
        assert_eq!(scheme_non_expansive.quantified_vars[0], tv);

        let scheme_expansive = TypeScheme::generalize_with_restriction(ty.clone(), &env_vars, false);

        assert_eq!(scheme_expansive.quantified_vars.len(), 0);
        assert_eq!(scheme_expansive.ty, ty);
    }

    #[test]
    fn test_value_restriction_with_free_vars() {
        let tv_a = TypeVar::new(40);
        let tv_b = TypeVar::new(41);
        let ty = Type::fun(vec![Type::Var(tv_a.clone())], Type::Var(tv_b.clone()));

        let env_vars = FxHashMap::default();

        let non_exp_scheme = TypeScheme::generalize_with_restriction(ty.clone(), &env_vars, true);
        assert_eq!(non_exp_scheme.quantified_vars.len(), 2);

        let exp_scheme = TypeScheme::generalize_with_restriction(ty.clone(), &env_vars, false);
        assert_eq!(exp_scheme.quantified_vars.len(), 0);
        assert_eq!(exp_scheme.ty, ty);
    }

    #[test]
    fn test_default_generalize_assumes_non_expansive() {
        let tv = TypeVar::new(50);
        let ty = Type::Var(tv.clone());

        let env_vars = FxHashMap::default();
        let scheme1 = TypeScheme::generalize(ty.clone(), &env_vars);
        let scheme2 = TypeScheme::generalize_with_restriction(ty, &env_vars, true);

        assert_eq!(scheme1.quantified_vars, scheme2.quantified_vars);
        assert_eq!(scheme1.ty, scheme2.ty);
    }

    #[test]
    fn test_subtype_reflexivity() {
        assert!(Type::int().is_subtype_of(&Type::int()));
        assert!(Type::string().is_subtype_of(&Type::string()));
        let list_int = Type::list(Type::int());
        assert!(list_int.is_subtype_of(&list_int));
    }

    /// Any acts as a supertype (for gradual typing) but not as a universal subtype
    /// This enables proper variance checking for protocol method signatures
    #[test]
    fn test_subtype_any_as_supertype() {
        assert!(Type::int().is_subtype_of(&Type::any()));
        assert!(Type::string().is_subtype_of(&Type::any()));
        assert!(Type::list(Type::int()).is_subtype_of(&Type::any()));
        assert!(Type::never().is_subtype_of(&Type::any()));

        assert!(!Type::any().is_subtype_of(&Type::int()));
        assert!(!Type::any().is_subtype_of(&Type::string()));

        assert!(Type::any().is_subtype_of(&Type::any()));
        assert!(Type::any().is_subtype_of(&Type::top()));
    }

    #[test]
    fn test_subtype_never_is_bottom() {
        assert!(Type::never().is_subtype_of(&Type::int()));
        assert!(Type::never().is_subtype_of(&Type::string()));
        assert!(Type::never().is_subtype_of(&Type::any()));
        assert!(Type::never().is_subtype_of(&Type::top()));

        assert!(!Type::int().is_subtype_of(&Type::never()));
        assert!(!Type::string().is_subtype_of(&Type::never()));
    }

    #[test]
    fn test_subtype_top_is_top() {
        assert!(Type::int().is_subtype_of(&Type::top()));
        assert!(Type::string().is_subtype_of(&Type::top()));
        assert!(Type::list(Type::int()).is_subtype_of(&Type::top()));
        assert!(Type::never().is_subtype_of(&Type::top()));

        assert!(!Type::top().is_subtype_of(&Type::int()));
        assert!(!Type::top().is_subtype_of(&Type::string()));
    }

    #[test]
    fn test_subtype_concrete_types_not_related() {
        assert!(!Type::int().is_subtype_of(&Type::string()));
        assert!(!Type::string().is_subtype_of(&Type::bool()));
        assert!(!Type::bool().is_subtype_of(&Type::int()));
    }

    #[test]
    fn test_subtype_union_covariance() {
        let int_or_str = Type::union(vec![Type::int(), Type::string()]);

        assert!(Type::int().is_subtype_of(&int_or_str));
        assert!(Type::string().is_subtype_of(&int_or_str));
        assert!(!Type::bool().is_subtype_of(&int_or_str));
        assert!(!int_or_str.is_subtype_of(&Type::int()));

        let int_str_bool = Type::union(vec![Type::int(), Type::string(), Type::bool()]);
        assert!(int_or_str.is_subtype_of(&int_str_bool));
    }

    /// (Any -> str) <: (int -> str)
    /// A function accepting Any can substitute for one accepting int
    /// (int -> str) </: (Any -> str)
    /// A function accepting only int cannot substitute for one accepting Any
    #[test]
    fn test_subtype_function_contravariance() {
        let f_any_str = Type::fun(vec![Type::any()], Type::string());
        let f_int_str = Type::fun(vec![Type::int()], Type::string());

        assert!(f_any_str.is_subtype_of(&f_int_str));
        assert!(!f_int_str.is_subtype_of(&f_any_str));
    }

    #[test]
    fn test_subtype_function_return_covariance() {
        let f_int_never = Type::fun(vec![Type::int()], Type::never());
        let f_int_str = Type::fun(vec![Type::int()], Type::string());
        assert!(f_int_never.is_subtype_of(&f_int_str));

        let f_int_top = Type::fun(vec![Type::int()], Type::top());
        assert!(!f_int_top.is_subtype_of(&f_int_str));
    }

    #[test]
    fn test_subtype_function_arity_mismatch() {
        let f_one_arg = Type::fun(vec![Type::int()], Type::string());
        let f_two_args = Type::fun(vec![Type::int(), Type::int()], Type::string());

        assert!(!f_one_arg.is_subtype_of(&f_two_args));
        assert!(!f_two_args.is_subtype_of(&f_one_arg));
    }

    #[test]
    fn test_subtype_function_complex_variance() {
        let inner1 = Type::fun(vec![Type::int()], Type::string());
        let inner2 = Type::fun(vec![Type::any()], Type::string());
        let outer1 = Type::fun(vec![inner1.clone()], Type::bool());
        let outer2 = Type::fun(vec![inner2.clone()], Type::bool());

        assert!(inner2.is_subtype_of(&inner1));
        assert!(!inner1.is_subtype_of(&inner2));
        assert!(outer1.is_subtype_of(&outer2));
        assert!(!outer2.is_subtype_of(&outer1));
    }

    #[test]
    fn test_subtype_type_application() {
        let list_int = Type::list(Type::int());
        let list_str = Type::list(Type::string());

        assert!(list_int.is_subtype_of(&list_int));
        assert!(!list_int.is_subtype_of(&list_str));
        assert!(!list_str.is_subtype_of(&list_int));
    }

    #[test]
    fn test_subtype_record_structural() {
        let r1 = Type::Record(
            vec![("x".to_string(), Type::int()), ("y".to_string(), Type::string())],
            None,
        );
        let r2 = Type::Record(vec![("x".to_string(), Type::int())], None);

        assert!(r1.is_subtype_of(&r2));
        assert!(!r2.is_subtype_of(&r1));
    }

    #[test]
    fn test_subtype_record_field_types() {
        let r1 = Type::Record(vec![("x".to_string(), Type::int())], None);
        let r2 = Type::Record(vec![("x".to_string(), Type::string())], None);

        assert!(!r1.is_subtype_of(&r2));
        assert!(!r2.is_subtype_of(&r1));
    }

    #[test]
    fn test_subtype_record_with_top() {
        let r_int = Type::Record(vec![("x".to_string(), Type::int())], None);
        let r_top = Type::Record(vec![("x".to_string(), Type::top())], None);

        assert!(r_int.is_subtype_of(&r_top));
        assert!(!r_top.is_subtype_of(&r_int));
    }

    #[test]
    fn test_subtype_bound_method() {
        let bm1 = Type::BoundMethod(Box::new(Type::int()), Box::new(Type::fun(vec![], Type::string())));
        let bm2 = Type::BoundMethod(Box::new(Type::int()), Box::new(Type::fun(vec![], Type::string())));
        let bm3 = Type::BoundMethod(Box::new(Type::string()), Box::new(Type::fun(vec![], Type::string())));

        assert!(bm1.is_subtype_of(&bm2));
        assert!(!bm1.is_subtype_of(&bm3));
    }

    #[test]
    fn test_intersection_normalization_flattens_nested() {
        let inner = Type::intersection(vec![Type::string(), Type::bool()]);
        let outer = Type::intersection(vec![Type::int(), inner]);

        match outer {
            Type::Intersection(types) => {
                assert_eq!(types.len(), 3);
                assert!(types.contains(&Type::bool()));
                assert!(types.contains(&Type::int()));
                assert!(types.contains(&Type::string()));
            }
            _ => panic!("Expected intersection type"),
        }
    }

    #[test]
    fn test_intersection_normalization_removes_duplicates() {
        let intersection = Type::intersection(vec![Type::int(), Type::string(), Type::int(), Type::string()]);

        match intersection {
            Type::Intersection(types) => {
                assert_eq!(types.len(), 2);
                assert!(types.contains(&Type::int()));
                assert!(types.contains(&Type::string()));
            }
            _ => panic!("Expected intersection type"),
        }
    }

    #[test]
    fn test_intersection_normalization_is_sorted() {
        let intersection = Type::intersection(vec![Type::string(), Type::bool(), Type::int()]);

        match intersection {
            Type::Intersection(types) => {
                for i in 1..types.len() {
                    assert!(types[i - 1] <= types[i], "Intersection types should be sorted");
                }
            }
            _ => panic!("Expected intersection type"),
        }
    }

    #[test]
    fn test_intersection_single_element_unwraps() {
        let intersection = Type::intersection(vec![Type::int()]);
        assert_eq!(intersection, Type::int());
    }

    #[test]
    fn test_intersection_display() {
        let intersection = Type::intersection(vec![Type::int(), Type::string()]);
        let display = intersection.to_string();
        assert!(display.contains("int"));
        assert!(display.contains("str"));
        assert!(display.contains(" & "));
    }

    /// T <: Intersection[A, B] means T <: A and T <: B
    #[test]
    fn test_subtype_intersection_all_must_satisfy() {
        let intersection = Type::intersection(vec![Type::int(), Type::string()]);

        assert!(!Type::int().is_subtype_of(&intersection));
        assert!(Type::never().is_subtype_of(&intersection));
    }

    /// Intersection[A, B] is more specific than either A or B, so it's a subtype of both
    #[test]
    fn test_subtype_intersection_to_single_type() {
        let proto1 = Type::Con(TypeCtor::Protocol(Some("Iterable".to_string())));
        let proto2 = Type::Con(TypeCtor::Protocol(Some("Sized".to_string())));
        let intersection = Type::intersection(vec![proto1.clone(), proto2.clone()]);

        assert!(intersection.is_subtype_of(&proto1));
        assert!(intersection.is_subtype_of(&proto2));
        assert!(intersection.is_subtype_of(&Type::any()));
        assert!(intersection.is_subtype_of(&Type::top()));
    }

    #[test]
    fn test_subtype_intersection_with_protocols() {
        let proto1 = Type::Con(TypeCtor::Protocol(Some("Iterable".to_string())));
        let proto2 = Type::Con(TypeCtor::Protocol(Some("Sized".to_string())));
        let both = Type::intersection(vec![proto1.clone(), proto2.clone()]);

        assert!(both.is_subtype_of(&proto1));
        assert!(both.is_subtype_of(&proto2));
    }

    #[test]
    fn test_intersection_kind() {
        let intersection = Type::intersection(vec![Type::int(), Type::string()]);
        assert_eq!(intersection.kind_of().unwrap(), Kind::Star);
    }
}
