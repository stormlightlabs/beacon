use beacon_core::{ParseError, Result};
use tree_sitter::{Parser, Tree};

mod ast;
mod cst;
pub mod docstring;
pub mod highlight;
mod literal_parse;
pub mod resolve;
pub mod rst;

pub use ast::{
    AstNode, BinaryOperator, CompareOperator, Comprehension, ExceptHandler, ImportName, LiteralValue, MatchCase,
    Parameter, Pattern, UnaryOperator, WithItem,
};
pub use docstring::{DocstringStyle, ParsedDocstring, parse as parse_docstring};
pub use highlight::PythonHighlighter;
pub use resolve::{
    BUILTIN_DUNDERS, MAGIC_METHODS, NameResolver, ReferenceKind, Scope, ScopeId, ScopeKind, Symbol, SymbolKind,
    SymbolReference, SymbolTable,
};

/// Python parser using [tree_sitter]
pub struct PythonParser {
    parser: Parser,
}

/// Represents a parsed Python source file
pub struct ParsedFile {
    pub tree: Tree,
    pub source: String,
}

impl PythonParser {
    pub fn new() -> Result<Self> {
        let language = tree_sitter_python::LANGUAGE;
        let mut parser = Parser::new();
        parser
            .set_language(&language.into())
            .map_err(|e| ParseError::TreeSitterError(e.to_string()))?;

        Ok(PythonParser { parser })
    }

    /// Parse Python source code into a tree
    pub fn parse(&mut self, source: &str) -> Result<ParsedFile> {
        let tree = self
            .parser
            .parse(source, None)
            .ok_or_else(|| ParseError::TreeSitterError("Failed to parse source".to_string()))?;

        Ok(ParsedFile { tree, source: source.to_string() })
    }

    /// Convert tree-sitter CST to our AST
    pub fn to_ast(&self, parsed: &ParsedFile) -> Result<AstNode> {
        let root_node = parsed.tree.root_node();
        self.node_to_ast(root_node, &parsed.source)
    }

    /// Debug helper to print tree structure
    pub fn debug_tree(&self, parsed: &ParsedFile) -> String {
        let root_node = parsed.tree.root_node();
        Self::debug_node(root_node, &parsed.source, 0)
    }

    /// Perform name resolution on an AST and return a symbol table
    pub fn resolve_names(&self, ast: &AstNode, source: &str) -> Result<SymbolTable> {
        let mut resolver = NameResolver::new(source.to_string());
        resolver.resolve(ast)?;
        Ok(resolver.symbol_table)
    }

    pub fn parse_and_resolve(&mut self, source: &str) -> Result<(AstNode, SymbolTable)> {
        let parsed = self.parse(source)?;
        let ast = self.to_ast(&parsed)?;
        let symbol_table = self
            .resolve_names(&ast, source)
            .map_err(|e| ParseError::TreeSitterError(format!("Name resolution failed: {e}")))?;
        Ok((ast, symbol_table))
    }
}

impl Default for PythonParser {
    fn default() -> Self {
        Self::new().expect("Failed to create Python parser")
    }
}

#[cfg(test)]
mod tests;
