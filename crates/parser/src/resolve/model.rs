use rustc_hash::FxHashMap;

/// Builtin dunder variables available at module level
pub static BUILTIN_DUNDERS: &[&str] = &["__name__", "__file__", "__doc__", "__package__"];

/// Magic methods that can be defined in classes
pub static MAGIC_METHODS: &[&str] = &[
    "__init__",
    "__new__",
    "__repr__",
    "__str__",
    "__len__",
    "__eq__",
    "__lt__",
    "__le__",
    "__gt__",
    "__ge__",
    "__ne__",
    "__getitem__",
    "__setitem__",
    "__delitem__",
    "__enter__",
    "__exit__",
    "__iter__",
    "__next__",
    "__call__",
    "__add__",
    "__sub__",
    "__mul__",
    "__truediv__",
    "__floordiv__",
    "__mod__",
    "__pow__",
    "__neg__",
    "__pos__",
    "__abs__",
    "__bool__",
    "__copy__",
    "__deepcopy__",
    "__sizeof__",
    "__bytes__",
    "__format__",
    "__complex__",
    "__int__",
    "__float__",
    "__round__",
    "__index__",
    "__hash__",
    "__getattr__",
    "__setattr__",
    "__delattr__",
    "__getattribute__",
    "__dir__",
    "__radd__",
    "__rand__",
    "__ior__",
    "__imul__",
    "__reduce_ex__",
    "__reduce__",
    "__getnewargs__",
];

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum SymbolKind {
    Variable,
    Function,
    Class,
    Parameter,
    Import,
    MagicMethod,
    BuiltinVar,
}

impl SymbolKind {
    pub fn icon(&self) -> &'static str {
        match self {
            SymbolKind::Variable => "◆",
            SymbolKind::Function => "λ",
            SymbolKind::Class => "●",
            SymbolKind::Parameter => "▲",
            SymbolKind::Import => "↳",
            SymbolKind::BuiltinVar => "⧉",
            SymbolKind::MagicMethod => "★",
        }
    }

    pub fn name(&self) -> &'static str {
        match self {
            SymbolKind::Variable => "variable",
            SymbolKind::Function => "function",
            SymbolKind::Class => "class",
            SymbolKind::Parameter => "parameter",
            SymbolKind::Import => "import",
            _ => "dunder",
        }
    }
}

/// Kind of reference to a symbol
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ReferenceKind {
    Read,
    Write,
}

/// A reference to a symbol at a specific location
#[derive(Debug, Clone)]
pub struct SymbolReference {
    pub line: usize,
    pub col: usize,
    pub end_col: usize,
    pub kind: ReferenceKind,
}

/// Information about a symbol definition
#[derive(Debug, Clone)]
pub struct Symbol {
    pub name: String,
    pub kind: SymbolKind,
    pub line: usize,
    pub col: usize,
    pub end_col: usize,
    pub scope_id: ScopeId,
    pub docstring: Option<String>,
    /// References to this symbol (reads and writes)
    pub references: Vec<SymbolReference>,
}

/// Unique identifier for scopes
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ScopeId(pub(crate) u32);

impl ScopeId {
    /// Create a new ScopeId for testing purposes
    ///
    /// In production, ScopeIds are created internally by SymbolTable.create_scope().
    /// This constructor is primarily for testing and cache key creation.
    #[cfg(test)]
    pub fn new(id: u32) -> Self {
        Self(id)
    }

    /// Create a ScopeId from a raw u32 value (for cache and testing)
    ///
    /// SAFETY: This should only be used in controlled contexts like caching
    /// where you need to reconstruct a ScopeId from a stored value.
    pub fn from_raw(id: u32) -> Self {
        Self(id)
    }
}

/// Types of scopes in Python
#[derive(Debug, Clone, PartialEq)]
pub enum ScopeKind {
    Module,
    Function,
    Class,
    Block,
}

/// Represents a scope with its symbols and nested scopes
#[derive(Debug, Clone)]
pub struct Scope {
    pub id: ScopeId,
    pub kind: ScopeKind,
    pub parent: Option<ScopeId>,
    pub symbols: FxHashMap<String, Symbol>,
    pub children: Vec<ScopeId>,
    /// Start byte offset of this scope in the source
    pub start_byte: usize,
    /// End byte offset of this scope in the source
    pub end_byte: usize,
}
