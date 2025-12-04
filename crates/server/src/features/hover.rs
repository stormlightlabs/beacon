//! Hover information provider
//!
//! Displays type information, signatures, and documentation when hovering over identifiers and expressions.

use crate::cache::IntrospectionCache;
use crate::document::DocumentManager;
use crate::features::{builtin_docs, dunders};
use crate::introspection::IntrospectionResult;
use crate::parser;
use crate::{analysis::Analyzer, introspection};
use beacon_core::{Type, TypeCtor};
use beacon_parser::{AstNode, SymbolKind, parse_docstring};
use lsp_types::{Hover, HoverContents, HoverParams, MarkupContent, MarkupKind, Position};
use std::{ops::Not, path::PathBuf};
use url::Url;

pub struct HoverProvider {
    documents: DocumentManager,
    interpreter_path: Option<PathBuf>,
    introspection_cache: IntrospectionCache,
}

impl HoverProvider {
    pub fn new(documents: DocumentManager) -> Self {
        Self { documents, interpreter_path: None, introspection_cache: IntrospectionCache::default() }
    }

    /// Create a new hover provider with introspection support
    pub fn with_introspection(
        documents: DocumentManager, interpreter_path: Option<PathBuf>, introspection_cache: IntrospectionCache,
    ) -> Self {
        Self { documents, interpreter_path, introspection_cache }
    }

    /// Provide hover information at a position
    pub fn hover(&self, params: HoverParams, analyzer: &mut Analyzer) -> Option<Hover> {
        let uri = params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;
        match self.hover_symbol(&uri, position) {
            Some(hover) => Some(hover),
            None => {
                let ty = analyzer.type_at_position(&uri, position).ok()??;
                let contents = HoverContents::Markup(self.format_type_hover(&ty));
                Some(Hover { contents, range: None })
            }
        }
    }

    /// Get hover information for a symbol
    fn hover_symbol(&self, uri: &Url, position: Position) -> Option<Hover> {
        self.documents.get_document(uri, |doc| {
            let tree = doc.tree()?;
            let text = doc.text();
            let symbol_table = doc.symbol_table()?;
            let ast = doc.ast()?;
            let p = parser::LspParser::new().ok()?;
            let node = p.node_at_position(tree, &text, position)?;

            match node.kind() {
                "identifier" => {
                    let identifier_text = node.utf8_text(text.as_bytes()).ok()?;

                    if identifier_text.starts_with("__") && identifier_text.ends_with("__") {
                        if let Some(dunder_info) = dunders::get_dunder_info(identifier_text) {
                            return Some(Hover {
                                contents: HoverContents::Markup(self.format_dunder_hover(dunder_info)),
                                range: None,
                            });
                        }
                    }

                    if builtin_docs::is_builtin_type(identifier_text) {
                        return Some(Hover {
                            contents: HoverContents::Markup(self.format_builtin_type_hover(identifier_text)),
                            range: None,
                        });
                    }

                    let symbol = symbol_table.lookup_symbol(identifier_text, symbol_table.root_scope)?;
                    let content = match symbol.kind {
                        SymbolKind::Function => {
                            self.format_function_hover(identifier_text, ast, symbol.line, symbol.docstring.as_deref())
                        }
                        SymbolKind::Class => {
                            self.format_class_hover(identifier_text, ast, symbol.line, symbol.docstring.as_deref())
                        }
                        SymbolKind::Variable => {
                            self.format_variable_hover(identifier_text, &symbol.kind, symbol.line, symbol.col)
                        }
                        SymbolKind::Parameter => self.format_parameter_hover(identifier_text, symbol.line, symbol.col),
                        SymbolKind::Import => self.format_import_hover(identifier_text, ast, symbol.line),
                        SymbolKind::BuiltinVar => self.format_builtin_var_hover(identifier_text),
                        SymbolKind::MagicMethod => self.format_magic_method_hover(identifier_text),
                    };

                    Some(Hover { contents: HoverContents::Markup(content), range: None })
                }
                _ => None,
            }
        })?
    }

    /// Format hover for a function with signature display
    fn format_function_hover(
        &self, name: &str, ast: &AstNode, def_line: usize, docstring: Option<&str>,
    ) -> MarkupContent {
        let sig = Self::extract_function_signature(ast, name).unwrap_or_else(|| format!("def {name}(...)"));
        let mut value = format!("```python\n{sig}\n```\n\n**Function** defined at line {def_line}");

        if let Some(doc) = docstring {
            let parsed = parse_docstring(doc);
            let rendered = parsed.to_markdown();
            if rendered.trim().is_empty().not() {
                value.push_str("\n\n---\n\n");
                value.push_str(rendered.trim());
            }
        } else if let Some(generated) = Self::generate_docstring_from_ast(ast, name) {
            value.push_str("\n\n---\n\n");
            value.push_str(&generated);
            value.push_str("\n\n*(Documentation generated from type annotations)*");
        }

        MarkupContent { kind: MarkupKind::Markdown, value }
    }

    /// Format hover for a class
    fn format_class_hover(
        &self, name: &str, _ast: &AstNode, def_line: usize, docstring: Option<&str>,
    ) -> MarkupContent {
        let mut value = format!("```python\nclass {name}\n```\n\n**Class** defined at line {def_line}");

        if let Some(doc) = docstring {
            let parsed = parse_docstring(doc);
            let rendered = parsed.to_markdown();
            if rendered.trim().is_empty().not() {
                value.push_str("\n\n---\n\n");
                value.push_str(rendered.trim());
            }
        }

        MarkupContent { kind: MarkupKind::Markdown, value }
    }

    /// Format hover for a variable
    fn format_variable_hover(&self, name: &str, _kind: &SymbolKind, def_line: usize, _def_col: usize) -> MarkupContent {
        MarkupContent {
            kind: MarkupKind::Markdown,
            value: format!("**Variable** `{name}`\n\nDefined at line {def_line}"),
        }
    }

    fn format_parameter_hover(&self, name: &str, def_line: usize, _def_col: usize) -> MarkupContent {
        MarkupContent {
            kind: MarkupKind::Markdown,
            value: format!("**Parameter** `{name}`\n\nDefined at line {def_line}"),
        }
    }

    /// Format hover for a builtin dunder variable
    fn format_builtin_var_hover(&self, name: &str) -> MarkupContent {
        if let Some(info) = dunders::get_dunder_info(name) {
            self.format_dunder_hover(info)
        } else {
            MarkupContent { kind: MarkupKind::Markdown, value: format!("**Builtin Variable** `{name}`") }
        }
    }

    /// Format hover for a magic method
    fn format_magic_method_hover(&self, name: &str) -> MarkupContent {
        if let Some(info) = dunders::get_dunder_info(name) {
            self.format_dunder_hover(info)
        } else {
            MarkupContent { kind: MarkupKind::Markdown, value: format!("**Magic Method** `{name}`") }
        }
    }

    /// Format hover for a dunder with full documentation
    fn format_dunder_hover(&self, info: &dunders::DunderInfo) -> MarkupContent {
        let kind_label = if info.category == "method" { "Magic Method" } else { "Builtin Variable" };

        let value = format!(
            "```python\n{}\n```\n\n**{}**\n\n{}\n\n[Python Documentation]({})",
            info.name, kind_label, info.doc, info.link
        );

        MarkupContent { kind: MarkupKind::Markdown, value }
    }

    /// Format hover for a built-in type with hybrid documentation approach by first checking
    /// embedded builtin_docs, then falls back to Python introspection
    fn format_builtin_type_hover(&self, type_name: &str) -> MarkupContent {
        if let Some(doc) = builtin_docs::get_builtin_doc(type_name) {
            let mut value = format!(
                "```python\n{}\n```\n\n**Built-in Type**\n\n{}",
                doc.name, doc.description
            );

            if let Some(ref details) = doc.details {
                value.push_str("\n\n");
                value.push_str(details);
            }

            if let Some(ref link) = doc.doc_link {
                value.push_str(&format!("\n\n[Python Documentation]({link})"));
            }

            return MarkupContent { kind: MarkupKind::Markdown, value };
        }

        if let Some(ref python) = self.interpreter_path {
            if let Some(cached) = self.introspection_cache.get("builtins", type_name) {
                let mut value = format!("```python\n{type_name}\n```\n\n**Built-in Type**");

                if !cached.docstring.is_empty() {
                    let parsed = parse_docstring(&cached.docstring);
                    let rendered = parsed.to_markdown();
                    if !rendered.trim().is_empty() {
                        value.push_str("\n\n---\n\n");
                        value.push_str(rendered.trim());
                    }
                }

                return MarkupContent { kind: MarkupKind::Markdown, value };
            }

            match introspection::introspect_sync(python, "builtins", type_name) {
                Ok(result) => {
                    self.introspection_cache
                        .insert("builtins".to_string(), type_name.to_string(), result.clone());

                    let mut value = format!("```python\n{type_name}\n```\n\n**Built-in Type**");

                    if !result.docstring.is_empty() {
                        let parsed = parse_docstring(&result.docstring);
                        let rendered = parsed.to_markdown();
                        if !rendered.trim().is_empty() {
                            value.push_str("\n\n---\n\n");
                            value.push_str(rendered.trim());
                        }
                    }

                    return MarkupContent { kind: MarkupKind::Markdown, value };
                }
                Err(e) => {
                    tracing::warn!("Introspection failed for built-in type {}: {}", type_name, e);
                }
            }
        }

        MarkupContent { kind: MarkupKind::Markdown, value: format!("```python\n{type_name}\n```\n\n**Built-in Type**") }
    }

    fn format_import_hover(&self, name: &str, ast: &AstNode, line: usize) -> MarkupContent {
        if let Some((module, symbol)) = Self::find_import_info(ast, line, name) {
            if let Some(ref python) = self.interpreter_path {
                let module_doc = if let Some(cached) = self.introspection_cache.get(&module, "__module__") {
                    Some(cached.docstring.clone())
                } else {
                    match introspection::introspect_module_sync(python, &module) {
                        Ok(result) => {
                            self.introspection_cache
                                .insert(module.clone(), "__module__".to_string(), result.clone());
                            Some(result.docstring)
                        }
                        Err(e) => {
                            tracing::debug!("Module introspection failed for {}: {}", module, e);
                            None
                        }
                    }
                };

                if let Some(cached) = self.introspection_cache.get(&module, &symbol) {
                    return self.format_introspection_result_with_module(
                        name,
                        &module,
                        &symbol,
                        &cached,
                        module_doc.as_deref(),
                    );
                }

                match introspection::introspect_sync(python, &module, &symbol) {
                    Ok(result) => {
                        self.introspection_cache
                            .insert(module.clone(), symbol.clone(), result.clone());

                        return self.format_introspection_result_with_module(
                            name,
                            &module,
                            &symbol,
                            &result,
                            module_doc.as_deref(),
                        );
                    }
                    Err(e) => {
                        tracing::warn!("Introspection failed for {}.{}: {}", module, symbol, e);

                        if let Some(doc) = module_doc {
                            if !doc.trim().is_empty() {
                                let mut value = format!(
                                    "**Import** `{name}` from module `{module}`\n\n**Module documentation:**\n\n"
                                );
                                let parsed = parse_docstring(&doc);
                                let rendered = parsed.to_markdown();
                                if !rendered.trim().is_empty() {
                                    value.push_str(rendered.trim());
                                }
                                return MarkupContent { kind: MarkupKind::Markdown, value };
                            }
                        }
                    }
                }
            }

            return MarkupContent {
                kind: MarkupKind::Markdown,
                value: format!("**Import** `{name}` from module `{module}`"),
            };
        }

        MarkupContent { kind: MarkupKind::Markdown, value: format!("**Import** `{name}`") }
    }

    /// Find import information from AST
    fn find_import_info(node: &AstNode, target_line: usize, symbol_name: &str) -> Option<(String, String)> {
        match node {
            AstNode::ImportFrom { module, names, line, .. } if *line == target_line => {
                if names.iter().any(|n| n.name == symbol_name) {
                    Some((module.clone(), symbol_name.to_string()))
                } else {
                    None
                }
            }
            AstNode::Module { body, .. } | AstNode::FunctionDef { body, .. } | AstNode::ClassDef { body, .. } => {
                for stmt in body {
                    if let Some(info) = Self::find_import_info(stmt, target_line, symbol_name) {
                        return Some(info);
                    }
                }
                None
            }
            AstNode::Import { .. } => None,
            _ => None,
        }
    }

    /// Format introspection result as hover content with optional module documentation
    /// FIXME: simplify this signature
    fn format_introspection_result_with_module(
        &self, symbol_name: &str, module_name: &str, _full_symbol: &str, result: &IntrospectionResult,
        module_doc: Option<&str>,
    ) -> MarkupContent {
        let mut value = String::new();

        if result.signature.is_empty().not() {
            value.push_str(&format!("```python\n{}{}\n```\n\n", symbol_name, result.signature));
        } else {
            value.push_str(&format!("```python\n{symbol_name}\n```\n\n"));
        }

        value.push_str(&format!("**Imported from** `{module_name}`\n\n"));

        if result.docstring.is_empty().not() {
            let parsed = parse_docstring(&result.docstring);
            let rendered = parsed.to_markdown();
            if rendered.trim().is_empty().not() {
                value.push_str("---\n\n");
                value.push_str(rendered.trim());
            }
        }

        if let Some(mod_doc) = module_doc {
            if !mod_doc.trim().is_empty() {
                let parsed = parse_docstring(mod_doc);
                let rendered = parsed.to_markdown();
                if !rendered.trim().is_empty() {
                    value.push_str("\n\n---\n\n**Module documentation:**\n\n");
                    value.push_str(rendered.trim());
                }
            }
        }

        MarkupContent { kind: MarkupKind::Markdown, value }
    }

    /// Extract detailed function signature including type annotations and default values
    fn extract_function_signature(node: &AstNode, name: &str) -> Option<String> {
        match node {
            AstNode::FunctionDef { name: fn_name, args, return_type, .. } if fn_name == name => {
                let params: Vec<String> = args
                    .iter()
                    .map(|p| {
                        let mut param = p.name.clone();

                        if let Some(ref type_ann) = p.type_annotation {
                            param.push_str(": ");
                            param.push_str(type_ann);
                        }

                        if p.default_value.is_some() {
                            param.push_str(" = ...");
                        }

                        param
                    })
                    .collect();

                let mut signature = format!("def {}({})", name, params.join(", "));
                if let Some(ret_type) = return_type {
                    signature.push_str(" -> ");
                    signature.push_str(ret_type);
                }

                Some(signature)
            }
            AstNode::Module { body, .. } | AstNode::ClassDef { body, .. } => {
                for stmt in body {
                    let sig = Self::extract_function_signature(stmt, name);
                    if sig.is_some() {
                        return sig;
                    }
                }
                None
            }
            _ => None,
        }
    }

    /// Generate documentation from type annotations in AST
    ///
    /// Extracts parameter type annotations and return type annotations to create basic documentation when no docstring is present.
    fn generate_docstring_from_ast(node: &AstNode, name: &str) -> Option<String> {
        match node {
            AstNode::FunctionDef { name: fn_name, args, return_type, .. } if fn_name == name => {
                let mut doc_parts = Vec::new();
                let typed_params: Vec<_> = args.iter().filter(|p| p.type_annotation.is_some()).collect();

                if !typed_params.is_empty() {
                    doc_parts.push("**Parameters:**\n".to_string());
                    for param in typed_params {
                        if let Some(type_ann) = &param.type_annotation {
                            doc_parts.push(format!("- `{}` ({})", param.name, type_ann));
                        }
                    }
                }

                if let Some(ret_type) = return_type {
                    if !doc_parts.is_empty() {
                        doc_parts.push(String::new());
                    }
                    doc_parts.push(format!("**Returns:** `{ret_type}`"));
                }

                if doc_parts.is_empty() { None } else { Some(doc_parts.join("\n")) }
            }
            AstNode::Module { body, .. } | AstNode::ClassDef { body, .. } => {
                for stmt in body {
                    let generated = Self::generate_docstring_from_ast(stmt, name);
                    if generated.is_some() {
                        return generated;
                    }
                }
                None
            }
            _ => None,
        }
    }

    /// Provides rich formatting with type information on hover
    ///
    /// Attempts to enrich type information with docstrings for classes and other named types
    fn format_type_hover(&self, ty: &beacon_core::Type) -> MarkupContent {
        let type_str = self.format_type(ty);
        let mut value = format!("```python\n{type_str}\n```\n\n**Inferred type**");

        if let Some(docstring) = self.get_type_docstring(ty) {
            if !docstring.trim().is_empty() {
                let parsed = parse_docstring(&docstring);
                let rendered = parsed.to_markdown();
                if !rendered.trim().is_empty() {
                    value.push_str("\n\n---\n\n");
                    value.push_str(rendered.trim());
                }
            }
        }

        MarkupContent { kind: MarkupKind::Markdown, value }
    }

    /// Attempt to get docstring for a type by looking it up in the symbol table or via [introspection]
    fn get_type_docstring(&self, ty: &beacon_core::Type) -> Option<String> {
        match ty {
            Type::Con(TypeCtor::Class(class_name)) => {
                let class_doc = self.find_symbol_docstring(class_name, beacon_parser::SymbolKind::Class);
                if class_doc.is_some() {
                    return class_doc;
                }

                if let Some(ref python) = self.interpreter_path {
                    if let Some(cached) = self.introspection_cache.get("builtins", class_name) {
                        return Some(cached.docstring.clone());
                    }

                    if let Ok(result) = introspection::introspect_sync(python, "builtins", class_name) {
                        self.introspection_cache
                            .insert("builtins".to_string(), class_name.clone(), result.clone());
                        return Some(result.docstring);
                    }
                }

                None
            }
            _ => None,
        }
    }

    /// Find a symbol's docstring by searching through document symbol tables
    fn find_symbol_docstring(&self, symbol_name: &str, kind: beacon_parser::SymbolKind) -> Option<String> {
        for uri in self.documents.all_documents() {
            if let Some(doc) = self.documents.get_document(&uri, |doc| {
                let symbol_table = doc.symbol_table()?;
                let symbol = symbol_table.lookup_symbol(symbol_name, symbol_table.root_scope)?;

                if symbol.kind == kind { symbol.docstring.clone() } else { None }
            }) {
                if doc.is_some() {
                    return doc;
                }
            }
        }

        None
    }

    /// Format a type as a string using [std::fmt::Display] trait
    ///
    /// Uses the Display implementation from beacon-core which provides:
    /// - Pretty-printed type syntax (e.g., "list[int]" not "App(List, Int)")
    /// - Function signatures (e.g., "(int, str) -> bool")
    /// - Union types (e.g., "int | str")
    /// - Type variables with hints (e.g., "'alpha0")
    fn format_type(&self, ty: &beacon_core::Type) -> String {
        ty.to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use beacon_core::{Type, TypeCtor};
    use beacon_parser::Parameter;
    use std::str::FromStr;

    #[test]
    fn test_format_type_simple() {
        let documents = DocumentManager::new().unwrap();
        let provider = HoverProvider::new(documents);

        assert_eq!(provider.format_type(&Type::Con(TypeCtor::Int)), "int");
        assert_eq!(provider.format_type(&Type::Con(TypeCtor::String)), "str");
        assert_eq!(provider.format_type(&Type::Con(TypeCtor::Bool)), "bool");
        assert_eq!(provider.format_type(&Type::Con(TypeCtor::NoneType)), "None");
    }

    #[test]
    fn test_format_type_list() {
        let documents = DocumentManager::new().unwrap();
        let provider = HoverProvider::new(documents);

        let list_int = Type::list(Type::Con(TypeCtor::Int));
        assert_eq!(provider.format_type(&list_int), "list[int]");

        let list_str = Type::list(Type::Con(TypeCtor::String));
        assert_eq!(provider.format_type(&list_str), "list[str]");
    }

    #[test]
    fn test_format_type_dict() {
        let documents = DocumentManager::new().unwrap();
        let provider = HoverProvider::new(documents);
        let dict_str_int = Type::dict(Type::Con(TypeCtor::String), Type::Con(TypeCtor::Int));
        assert_eq!(provider.format_type(&dict_str_int), "dict[str, int]");
    }

    #[test]
    fn test_format_type_function() {
        let documents = DocumentManager::new().unwrap();
        let provider = HoverProvider::new(documents);
        let fun1 = Type::fun(
            vec![(String::new(), Type::Con(TypeCtor::Int))],
            Type::Con(TypeCtor::String),
        );
        assert_eq!(provider.format_type(&fun1), "int -> str");

        let fun2 = Type::fun(
            vec![
                (String::new(), Type::Con(TypeCtor::Int)),
                (String::new(), Type::Con(TypeCtor::String)),
            ],
            Type::Con(TypeCtor::Bool),
        );
        assert_eq!(provider.format_type(&fun2), "(int, str) -> bool");

        let fun3 = Type::fun(vec![], Type::Con(TypeCtor::Int));
        assert_eq!(provider.format_type(&fun3), "() -> int");
    }

    #[test]
    fn test_format_type_union() {
        let documents = DocumentManager::new().unwrap();
        let provider = HoverProvider::new(documents);

        let union = Type::union(vec![Type::Con(TypeCtor::Int), Type::Con(TypeCtor::String)]);
        let formatted = provider.format_type(&union);
        assert!(formatted.contains("int"));
        assert!(formatted.contains("str"));
        assert!(formatted.contains("|"));
    }

    #[test]
    fn test_format_type_optional() {
        let documents = DocumentManager::new().unwrap();
        let provider = HoverProvider::new(documents);
        let optional_int = Type::optional(Type::Con(TypeCtor::Int));
        let formatted = provider.format_type(&optional_int);
        assert!(formatted.contains("int"));
        assert!(formatted.contains("None"));
    }

    #[test]
    fn test_format_type_hover() {
        let documents = DocumentManager::new().unwrap();
        let provider = HoverProvider::new(documents);
        let ty = Type::Con(TypeCtor::Int);
        let hover = provider.format_type_hover(&ty);
        assert_eq!(hover.kind, MarkupKind::Markdown);
        assert!(hover.value.contains("```python"));
        assert!(hover.value.contains("int"));
        assert!(hover.value.contains("Inferred type"));
    }

    #[test]
    fn test_format_function_hover() {
        let documents = DocumentManager::new().unwrap();
        let provider = HoverProvider::new(documents);
        let ast = AstNode::Module {
            body: vec![AstNode::FunctionDef {
                name: "test_func".to_string(),
                args: vec![
                    Parameter {
                        name: "x".to_string(),
                        line: 1,
                        col: 15,
                        type_annotation: None,
                        default_value: None,
                        end_col: 1,
                        end_line: 16,
                    },
                    Parameter {
                        name: "y".to_string(),
                        line: 1,
                        col: 18,
                        type_annotation: None,
                        default_value: None,
                        end_col: 1,
                        end_line: 19,
                    },
                ],
                body: vec![],
                line: 1,
                col: 1,
                docstring: None,
                return_type: None,
                decorators: Vec::new(),
                is_async: false,
                end_line: 1,
                end_col: 1,
            }],
            docstring: None,
        };
        let content = provider.format_function_hover("test_func", &ast, 1, None);

        assert_eq!(content.kind, MarkupKind::Markdown);
        assert!(content.value.contains("def test_func"));
        assert!(content.value.contains("Function"));
        assert!(content.value.contains("line 1"));
    }

    #[test]
    fn test_format_class_hover() {
        let documents = DocumentManager::new().unwrap();
        let provider = HoverProvider::new(documents);
        let ast = beacon_parser::AstNode::Module { body: vec![], docstring: None };
        let content = provider.format_class_hover("MyClass", &ast, 5, None);
        assert_eq!(content.kind, MarkupKind::Markdown);
        assert!(content.value.contains("class MyClass"));
        assert!(content.value.contains("Class"));
        assert!(content.value.contains("line 5"));
    }

    #[test]
    fn test_format_variable_hover() {
        let documents = DocumentManager::new().unwrap();
        let provider = HoverProvider::new(documents);
        let content = provider.format_variable_hover("my_var", &beacon_parser::SymbolKind::Variable, 10, 5);
        assert_eq!(content.kind, MarkupKind::Markdown);
        assert!(content.value.contains("my_var"));
        assert!(content.value.contains("Variable"));
        assert!(content.value.contains("line 10"));
    }

    #[test]
    fn test_format_parameter_hover() {
        let documents = DocumentManager::new().unwrap();
        let provider = HoverProvider::new(documents);
        let content = provider.format_parameter_hover("param", 3, 8);
        assert_eq!(content.kind, MarkupKind::Markdown);
        assert!(content.value.contains("param"));
        assert!(content.value.contains("Parameter"));
        assert!(content.value.contains("line 3"));
    }

    #[test]
    fn test_format_import_hover() {
        let documents = DocumentManager::new().unwrap();
        let provider = HoverProvider::new(documents);

        let ast = beacon_parser::AstNode::Module {
            body: vec![beacon_parser::AstNode::ImportFrom {
                module: "os".to_string(),
                names: vec![beacon_parser::ImportName {
                    name: "path".to_string(),
                    line: 1,
                    col: 0,
                    end_line: 1,
                    end_col: 4,
                }],
                line: 1,
                col: 0,
                end_line: 1,
                end_col: 1,
            }],
            docstring: None,
        };

        let content = provider.format_import_hover("path", &ast, 1);

        assert_eq!(content.kind, MarkupKind::Markdown);
        assert!(content.value.contains("path"));
        assert!(content.value.contains("Import"));
    }

    #[test]
    fn test_hover_provider_creation() {
        let documents = DocumentManager::new().unwrap();
        let _ = HoverProvider::new(documents);
    }

    #[test]
    fn test_hover_on_imported_symbol() {
        let documents = DocumentManager::new().unwrap();
        let interpreter = crate::interpreter::find_python_interpreter(None);

        if interpreter.is_none() {
            println!("Skipping test - Python not found");
            return;
        }

        let cache = crate::cache::IntrospectionCache::new(None);
        let provider = HoverProvider::with_introspection(documents.clone(), interpreter, cache);

        let config = crate::config::Config::default();
        let mut analyzer = crate::analysis::Analyzer::new(config, documents.clone());

        let uri = Url::from_str("file:///test.py").unwrap();
        let source = "from math import sqrt\nresult = sqrt(16)";
        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let params = HoverParams {
            text_document_position_params: lsp_types::TextDocumentPositionParams {
                text_document: lsp_types::TextDocumentIdentifier { uri: uri.clone() },
                position: Position { line: 0, character: 17 },
            },
            work_done_progress_params: lsp_types::WorkDoneProgressParams::default(),
        };

        let result = provider.hover(params, &mut analyzer);

        if let Some(hover) = result {
            match hover.contents {
                HoverContents::Markup(content) => {
                    println!("Hover content: {}", content.value);
                    assert!(content.value.contains("math") || content.value.contains("Import"));
                }
                _ => panic!("Expected Markup content"),
            }
        }
    }

    #[test]
    fn test_format_type_nested_complex() {
        let documents = DocumentManager::new().unwrap();
        let provider = HoverProvider::new(documents);
        let dict_type = Type::dict(Type::Con(TypeCtor::String), Type::Con(TypeCtor::Int));
        let list_dict = Type::list(dict_type);
        let formatted = provider.format_type(&list_dict);
        assert!(formatted.contains("list"));
        assert!(formatted.contains("dict"));
        assert!(formatted.contains("str"));
        assert!(formatted.contains("int"));
    }

    #[test]
    fn test_format_type_type_variables() {
        let documents = DocumentManager::new().unwrap();
        let provider = HoverProvider::new(documents);
        let tv = beacon_core::TypeVar::named(0, "T");
        let formatted = provider.format_type(&Type::Var(tv));
        assert!(formatted.contains("T"));
    }

    #[test]
    fn test_format_type_record() {
        let documents = DocumentManager::new().unwrap();
        let provider = HoverProvider::new(documents);
        let formatted = provider.format_type(&Type::Record(
            vec![
                ("x".to_string(), Type::Con(TypeCtor::Int)),
                ("y".to_string(), Type::Con(TypeCtor::String)),
            ],
            None,
        ));
        assert!(formatted.contains("x"));
        assert!(formatted.contains("y"));
    }

    #[test]
    fn test_format_type_any() {
        let documents = DocumentManager::new().unwrap();
        let provider = HoverProvider::new(documents);
        assert_eq!(provider.format_type(&Type::Con(TypeCtor::Any)), "Any");
    }

    #[test]
    fn test_format_type_never() {
        let documents = DocumentManager::new().unwrap();
        let provider = HoverProvider::new(documents);
        assert_eq!(provider.format_type(&Type::Con(TypeCtor::Never)), "Never");
    }

    #[test]
    fn test_hover_with_type_at_position() {
        let documents = DocumentManager::new().unwrap();
        let provider = HoverProvider::new(documents.clone());
        let config = crate::config::Config::default();
        let mut analyzer = crate::analysis::Analyzer::new(config, documents.clone());

        let uri = Url::from_str("file:///test.py").unwrap();
        let source = "x = 42";
        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let params = HoverParams {
            text_document_position_params: lsp_types::TextDocumentPositionParams {
                text_document: lsp_types::TextDocumentIdentifier { uri: uri.clone() },
                position: Position { line: 0, character: 0 },
            },
            work_done_progress_params: lsp_types::WorkDoneProgressParams::default(),
        };

        let _result = provider.hover(params, &mut analyzer);
    }

    #[test]
    fn test_hover_with_function_docstring() {
        let documents = DocumentManager::new().unwrap();
        let provider = HoverProvider::new(documents.clone());
        let config = crate::config::Config::default();
        let mut analyzer = crate::analysis::Analyzer::new(config, documents.clone());

        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"def greet(name):
    """Say hello to someone."""
    return f"Hello {name}"

greet("world")"#;
        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let params = HoverParams {
            text_document_position_params: lsp_types::TextDocumentPositionParams {
                text_document: lsp_types::TextDocumentIdentifier { uri: uri.clone() },
                position: Position { line: 4, character: 0 },
            },
            work_done_progress_params: lsp_types::WorkDoneProgressParams::default(),
        };

        let result = provider.hover(params, &mut analyzer);
        assert!(result.is_some());

        let hover = result.unwrap();
        match hover.contents {
            HoverContents::Markup(content) => {
                assert!(content.value.contains("def greet(name)"));
                assert!(content.value.contains("Say hello to someone."));
                assert!(content.value.contains("Function"));
            }
            _ => panic!("Expected Markup content"),
        }
    }

    #[test]
    fn test_hover_with_class_docstring() {
        let documents = DocumentManager::new().unwrap();
        let provider = HoverProvider::new(documents.clone());
        let config = crate::config::Config::default();
        let mut analyzer = crate::analysis::Analyzer::new(config, documents.clone());

        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"class Person:
    """A person class with name and age."""
    def __init__(self, name):
        self.name = name

p = Person("Alice")"#;
        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let params = HoverParams {
            text_document_position_params: lsp_types::TextDocumentPositionParams {
                text_document: lsp_types::TextDocumentIdentifier { uri: uri.clone() },
                position: Position { line: 5, character: 4 },
            },
            work_done_progress_params: lsp_types::WorkDoneProgressParams::default(),
        };

        let result = provider.hover(params, &mut analyzer);
        assert!(result.is_some());

        let hover = result.unwrap();
        match hover.contents {
            HoverContents::Markup(content) => {
                assert!(content.value.contains("class Person"));
                assert!(content.value.contains("A person class with name and age."));
                assert!(content.value.contains("Class"));
            }
            _ => panic!("Expected Markup content"),
        }
    }

    #[test]
    fn test_format_type_forall() {
        let documents = DocumentManager::new().unwrap();
        let provider = HoverProvider::new(documents);

        let tv = beacon_core::TypeVar::new(0);
        let inner = Type::Fun(
            vec![(String::new(), Type::Var(tv.clone()))],
            Box::new(Type::Var(tv.clone())),
        );
        let forall = Type::ForAll(vec![tv], Box::new(inner));

        let formatted = provider.format_type(&forall);
        assert!(formatted.contains("∀") || formatted.contains("'t"));
    }

    #[test]
    fn test_format_type_app() {
        let documents = DocumentManager::new().unwrap();
        let provider = HoverProvider::new(documents);

        let app = Type::App(
            Box::new(Type::Con(TypeCtor::Class("Result".to_string()))),
            Box::new(Type::Con(TypeCtor::Int)),
        );

        let _formatted = provider.format_type(&app);
    }

    #[test]
    fn test_find_symbol_docstring_function() {
        let documents = DocumentManager::new().unwrap();
        let provider = HoverProvider::new(documents.clone());

        let uri = Url::from_str("file:///test_doc.py").unwrap();
        let source = r#"def calculate(x):
    """Calculate a value."""
    return x * 2
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let result = provider.find_symbol_docstring("calculate", beacon_parser::SymbolKind::Function);

        assert!(result.is_some());
        let docstring = result.unwrap();
        assert_eq!(docstring, "Calculate a value.");
    }

    #[test]
    fn test_find_symbol_docstring_class() {
        let documents = DocumentManager::new().unwrap();
        let provider = HoverProvider::new(documents.clone());

        let uri = Url::from_str("file:///test_class.py").unwrap();
        let source = r#"class Calculator:
    """A calculator class."""
    pass
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let result = provider.find_symbol_docstring("Calculator", beacon_parser::SymbolKind::Class);

        assert!(result.is_some());
        let docstring = result.unwrap();
        assert_eq!(docstring, "A calculator class.");
    }

    #[test]
    fn test_find_symbol_docstring_not_found() {
        let documents = DocumentManager::new().unwrap();
        let provider = HoverProvider::new(documents.clone());

        let uri = Url::from_str("file:///test_missing.py").unwrap();
        let source = r#"def other_function():
    pass
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let result = provider.find_symbol_docstring("nonexistent", beacon_parser::SymbolKind::Function);
        assert!(result.is_none());
    }

    #[test]
    fn test_find_symbol_docstring_wrong_kind() {
        let documents = DocumentManager::new().unwrap();
        let provider = HoverProvider::new(documents.clone());

        let uri = Url::from_str("file:///test_wrong_kind.py").unwrap();
        let source = r#"def my_func():
    """A function."""
    pass
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let result = provider.find_symbol_docstring("my_func", beacon_parser::SymbolKind::Class);
        assert!(result.is_none());
    }

    #[test]
    fn test_find_symbol_docstring_no_docstring() {
        let documents = DocumentManager::new().unwrap();
        let provider = HoverProvider::new(documents.clone());

        let uri = Url::from_str("file:///test_no_doc.py").unwrap();
        let source = r#"def no_doc_func():
    pass
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let result = provider.find_symbol_docstring("no_doc_func", beacon_parser::SymbolKind::Function);
        assert!(result.is_none());
    }

    #[test]
    fn test_find_symbol_docstring_multiple_documents() {
        let documents = DocumentManager::new().unwrap();
        let provider = HoverProvider::new(documents.clone());

        let uri1 = Url::from_str("file:///test1.py").unwrap();
        let source1 = r#"def func1():
    """First function."""
    pass
"#;

        let uri2 = Url::from_str("file:///test2.py").unwrap();
        let source2 = r#"def target_func():
    """Target function in second file."""
    pass
"#;

        documents.open_document(uri1.clone(), 1, source1.to_string()).unwrap();
        documents.open_document(uri2.clone(), 1, source2.to_string()).unwrap();

        let result = provider.find_symbol_docstring("target_func", beacon_parser::SymbolKind::Function);

        assert!(result.is_some());
        let docstring = result.unwrap();
        assert_eq!(docstring, "Target function in second file.");
    }

    #[test]
    fn test_find_symbol_docstring_variable() {
        let documents = DocumentManager::new().unwrap();
        let provider = HoverProvider::new(documents.clone());

        let uri = Url::from_str("file:///test_var.py").unwrap();
        let source = "x = 42\n";

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let result = provider.find_symbol_docstring("x", beacon_parser::SymbolKind::Variable);
        assert!(result.is_none());
    }

    #[test]
    fn test_format_builtin_var_hover_with_dunder_info() {
        let documents = DocumentManager::new().unwrap();
        let provider = HoverProvider::new(documents);

        let content = provider.format_builtin_var_hover("__name__");

        assert_eq!(content.kind, MarkupKind::Markdown);
        assert!(content.value.contains("__name__"));
        assert!(content.value.contains("```python"));
    }

    #[test]
    fn test_format_builtin_var_hover_without_dunder_info() {
        let documents = DocumentManager::new().unwrap();
        let provider = HoverProvider::new(documents);

        let content = provider.format_builtin_var_hover("some_custom_builtin");

        assert_eq!(content.kind, MarkupKind::Markdown);
        assert!(content.value.contains("Builtin Variable"));
        assert!(content.value.contains("some_custom_builtin"));
    }

    #[test]
    fn test_format_builtin_var_hover_file() {
        let documents = DocumentManager::new().unwrap();
        let provider = HoverProvider::new(documents);

        let content = provider.format_builtin_var_hover("__file__");

        assert_eq!(content.kind, MarkupKind::Markdown);
        assert!(content.value.contains("__file__"));
        assert!(content.value.contains("```python"));
    }

    #[test]
    fn test_format_magic_method_hover_with_dunder_info() {
        let documents = DocumentManager::new().unwrap();
        let provider = HoverProvider::new(documents);

        let content = provider.format_magic_method_hover("__init__");

        assert_eq!(content.kind, MarkupKind::Markdown);
        assert!(content.value.contains("__init__"));
        assert!(content.value.contains("```python"));
        assert!(content.value.contains("Magic Method"));
    }

    #[test]
    fn test_format_magic_method_hover_str() {
        let documents = DocumentManager::new().unwrap();
        let provider = HoverProvider::new(documents);

        let content = provider.format_magic_method_hover("__str__");

        assert_eq!(content.kind, MarkupKind::Markdown);
        assert!(content.value.contains("__str__"));
        assert!(content.value.contains("Magic Method"));
    }

    #[test]
    fn test_format_magic_method_hover_without_dunder_info() {
        let documents = DocumentManager::new().unwrap();
        let provider = HoverProvider::new(documents);

        let content = provider.format_magic_method_hover("__custom_magic__");

        assert_eq!(content.kind, MarkupKind::Markdown);
        assert!(content.value.contains("Magic Method"));
        assert!(content.value.contains("__custom_magic__"));
    }

    #[test]
    fn test_format_dunder_hover_structure() {
        let documents = DocumentManager::new().unwrap();
        let provider = HoverProvider::new(documents);

        let dunder_info = crate::features::dunders::DunderInfo {
            name: "__test__".to_string(),
            category: "method".to_string(),
            doc: "Test documentation".to_string(),
            link: "https://example.com".to_string(),
        };

        let content = provider.format_dunder_hover(&dunder_info);

        assert_eq!(content.kind, MarkupKind::Markdown);
        assert!(content.value.contains("```python"));
        assert!(content.value.contains("__test__"));
        assert!(content.value.contains("Magic Method"));
        assert!(content.value.contains("Test documentation"));
        assert!(content.value.contains("Python Documentation"));
        assert!(content.value.contains("https://example.com"));
    }

    #[test]
    fn test_format_dunder_hover_builtin_variable_category() {
        let documents = DocumentManager::new().unwrap();
        let provider = HoverProvider::new(documents);

        let dunder_info = crate::features::dunders::DunderInfo {
            name: "__name__".to_string(),
            category: "variable".to_string(),
            doc: "Module name".to_string(),
            link: "https://docs.python.org/3/reference/import.html#name__".to_string(),
        };

        let content = provider.format_dunder_hover(&dunder_info);

        assert_eq!(content.kind, MarkupKind::Markdown);
        assert!(content.value.contains("Builtin Variable"));
        assert!(!content.value.contains("Magic Method"));
        assert!(content.value.contains("__name__"));
        assert!(content.value.contains("Module name"));
    }

    #[test]
    fn test_format_dunder_hover_method_category() {
        let documents = DocumentManager::new().unwrap();
        let provider = HoverProvider::new(documents);

        let dunder_info = crate::features::dunders::DunderInfo {
            name: "__init__".to_string(),
            category: "method".to_string(),
            doc: "Initialize instance".to_string(),
            link: "https://docs.python.org/3/reference/datamodel.html#object.__init__".to_string(),
        };

        let content = provider.format_dunder_hover(&dunder_info);

        assert_eq!(content.kind, MarkupKind::Markdown);
        assert!(content.value.contains("Magic Method"));
        assert!(!content.value.contains("Builtin Variable"));
    }

    #[test]
    fn test_format_builtin_type_hover_with_embedded_docs() {
        let documents = DocumentManager::new().unwrap();
        let provider = HoverProvider::new(documents);

        let content = provider.format_builtin_type_hover("int");

        assert_eq!(content.kind, MarkupKind::Markdown);
        assert!(content.value.contains("```python"));
        assert!(content.value.contains("int"));
        assert!(content.value.contains("Built-in Type"));
    }

    #[test]
    fn test_format_builtin_type_hover_str() {
        let documents = DocumentManager::new().unwrap();
        let provider = HoverProvider::new(documents);

        let content = provider.format_builtin_type_hover("str");

        assert_eq!(content.kind, MarkupKind::Markdown);
        assert!(content.value.contains("str"));
        assert!(content.value.contains("Built-in Type"));
    }

    #[test]
    fn test_format_builtin_type_hover_list() {
        let documents = DocumentManager::new().unwrap();
        let provider = HoverProvider::new(documents);

        let content = provider.format_builtin_type_hover("list");

        assert_eq!(content.kind, MarkupKind::Markdown);
        assert!(content.value.contains("list"));
        assert!(content.value.contains("Built-in Type"));
    }

    #[test]
    fn test_format_builtin_type_hover_dict() {
        let documents = DocumentManager::new().unwrap();
        let provider = HoverProvider::new(documents);

        let content = provider.format_builtin_type_hover("dict");

        assert_eq!(content.kind, MarkupKind::Markdown);
        assert!(content.value.contains("dict"));
        assert!(content.value.contains("Built-in Type"));
    }

    #[test]
    fn test_format_builtin_type_hover_bool() {
        let documents = DocumentManager::new().unwrap();
        let provider = HoverProvider::new(documents);

        let content = provider.format_builtin_type_hover("bool");

        assert_eq!(content.kind, MarkupKind::Markdown);
        assert!(content.value.contains("bool"));
        assert!(content.value.contains("Built-in Type"));
    }

    #[test]
    fn test_format_builtin_type_hover_float() {
        let documents = DocumentManager::new().unwrap();
        let provider = HoverProvider::new(documents);

        let content = provider.format_builtin_type_hover("float");

        assert_eq!(content.kind, MarkupKind::Markdown);
        assert!(content.value.contains("float"));
        assert!(content.value.contains("Built-in Type"));
    }

    #[test]
    fn test_format_builtin_type_hover_tuple() {
        let documents = DocumentManager::new().unwrap();
        let provider = HoverProvider::new(documents);

        let content = provider.format_builtin_type_hover("tuple");

        assert_eq!(content.kind, MarkupKind::Markdown);
        assert!(content.value.contains("tuple"));
        assert!(content.value.contains("Built-in Type"));
    }

    #[test]
    fn test_format_builtin_type_hover_set() {
        let documents = DocumentManager::new().unwrap();
        let provider = HoverProvider::new(documents);

        let content = provider.format_builtin_type_hover("set");

        assert_eq!(content.kind, MarkupKind::Markdown);
        assert!(content.value.contains("set"));
        assert!(content.value.contains("Built-in Type"));
    }

    #[test]
    fn test_format_builtin_type_hover_unknown_type() {
        let documents = DocumentManager::new().unwrap();
        let provider = HoverProvider::new(documents);

        let content = provider.format_builtin_type_hover("UnknownType");

        assert_eq!(content.kind, MarkupKind::Markdown);
        assert!(content.value.contains("UnknownType"));
        assert!(content.value.contains("Built-in Type"));
        assert!(content.value.contains("```python"));
    }

    #[test]
    fn test_format_builtin_type_hover_without_introspection() {
        let documents = DocumentManager::new().unwrap();
        let provider = HoverProvider::new(documents);

        let content = provider.format_builtin_type_hover("CustomBuiltin");

        assert_eq!(content.kind, MarkupKind::Markdown);
        assert!(content.value.contains("CustomBuiltin"));
        assert!(content.value.contains("Built-in Type"));
    }

    #[test]
    fn test_format_builtin_type_hover_with_doc_link() {
        let documents = DocumentManager::new().unwrap();
        let provider = HoverProvider::new(documents);

        let content = provider.format_builtin_type_hover("str");

        assert_eq!(content.kind, MarkupKind::Markdown);
        if content.value.contains("Python Documentation") {
            assert!(content.value.contains("http"));
        }
    }
}
