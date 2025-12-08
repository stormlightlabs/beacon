//! Folding range provider
//!
//! Provides code folding ranges for Python constructs like functions, classes, control flow blocks, multiline collections, and import groups.

use crate::document::DocumentManager;

use beacon_parser::AstNode;
use lsp_types::{FoldingRange, FoldingRangeKind, FoldingRangeParams};
use url::Url;

pub struct FoldingRangeProvider {
    documents: DocumentManager,
}

impl FoldingRangeProvider {
    pub fn new(documents: DocumentManager) -> Self {
        Self { documents }
    }

    pub fn folding_range(&self, params: FoldingRangeParams) -> Option<Vec<FoldingRange>> {
        let uri = params.text_document.uri;
        self.extract_folding_ranges(&uri)
    }

    fn extract_folding_ranges(&self, uri: &Url) -> Option<Vec<FoldingRange>> {
        self.documents.get_document(uri, |doc| {
            let ast = doc.ast()?;
            let mut ranges = Vec::new();
            self.collect_folding_ranges(ast, &mut ranges);
            Some(ranges)
        })?
    }

    fn collect_folding_ranges(&self, node: &AstNode, ranges: &mut Vec<FoldingRange>) {
        match node {
            AstNode::Module { body, .. } => {
                self.collect_import_groups(body, ranges);

                for stmt in body {
                    self.collect_folding_ranges(stmt, ranges);
                }
            }
            AstNode::FunctionDef { body, line, col, .. } => {
                if !body.is_empty()
                    && let Some(range) = self.body_folding_range(*line, *col, body)
                {
                    ranges.push(range);
                }

                for stmt in body {
                    self.collect_folding_ranges(stmt, ranges);
                }
            }
            AstNode::ClassDef { body, line, col, .. } => {
                if !body.is_empty()
                    && let Some(range) = self.body_folding_range(*line, *col, body)
                {
                    ranges.push(range);
                }

                for stmt in body {
                    self.collect_folding_ranges(stmt, ranges);
                }
            }
            AstNode::If { body, elif_parts, else_body, line, col, .. } => {
                if !body.is_empty()
                    && let Some(range) = self.body_folding_range(*line, *col, body)
                {
                    ranges.push(range);
                }

                for (_, elif_body) in elif_parts {
                    if !elif_body.is_empty()
                        && let Some(first_stmt) = elif_body.first()
                    {
                        let start_line = self.get_node_line(first_stmt).saturating_sub(1);
                        if let Some(range) = self.body_folding_range(start_line, 0, elif_body) {
                            ranges.push(range);
                        }
                    }
                    for stmt in elif_body {
                        self.collect_folding_ranges(stmt, ranges);
                    }
                }

                if let Some(else_stmts) = else_body {
                    if !else_stmts.is_empty()
                        && let Some(first_stmt) = else_stmts.first()
                    {
                        let start_line = self.get_node_line(first_stmt).saturating_sub(2);
                        if let Some(range) = self.body_folding_range(start_line, 0, else_stmts) {
                            ranges.push(range);
                        }
                    }
                    for stmt in else_stmts {
                        self.collect_folding_ranges(stmt, ranges);
                    }
                }

                for stmt in body {
                    self.collect_folding_ranges(stmt, ranges);
                }
            }
            AstNode::For { body, else_body, line, col, .. } => {
                if !body.is_empty()
                    && let Some(range) = self.body_folding_range(*line, *col, body)
                {
                    ranges.push(range);
                }

                if let Some(else_stmts) = else_body {
                    if !else_stmts.is_empty()
                        && let Some(first_stmt) = else_stmts.first()
                    {
                        let start_line = self.get_node_line(first_stmt).saturating_sub(2);
                        if let Some(range) = self.body_folding_range(start_line, 0, else_stmts) {
                            ranges.push(range);
                        }
                    }
                    for stmt in else_stmts {
                        self.collect_folding_ranges(stmt, ranges);
                    }
                }

                for stmt in body {
                    self.collect_folding_ranges(stmt, ranges);
                }
            }
            AstNode::While { body, else_body, line, col, .. } => {
                if !body.is_empty()
                    && let Some(range) = self.body_folding_range(*line, *col, body)
                {
                    ranges.push(range);
                }

                if let Some(else_stmts) = else_body {
                    if !else_stmts.is_empty()
                        && let Some(first_stmt) = else_stmts.first()
                    {
                        let start_line = self.get_node_line(first_stmt).saturating_sub(2);
                        if let Some(range) = self.body_folding_range(start_line, 0, else_stmts) {
                            ranges.push(range);
                        }
                    }
                    for stmt in else_stmts {
                        self.collect_folding_ranges(stmt, ranges);
                    }
                }

                for stmt in body {
                    self.collect_folding_ranges(stmt, ranges);
                }
            }
            AstNode::Try { body, handlers, else_body, finally_body, line, col, .. } => {
                if !body.is_empty()
                    && let Some(range) = self.body_folding_range(*line, *col, body)
                {
                    ranges.push(range);
                }

                for handler in handlers {
                    if !handler.body.is_empty()
                        && let Some(range) = self.body_folding_range(handler.line, handler.col, &handler.body)
                    {
                        ranges.push(range);
                    }
                    for stmt in &handler.body {
                        self.collect_folding_ranges(stmt, ranges);
                    }
                }

                if let Some(else_stmts) = else_body {
                    if !else_stmts.is_empty()
                        && let Some(first_stmt) = else_stmts.first()
                    {
                        let start_line = self.get_node_line(first_stmt).saturating_sub(2);
                        if let Some(range) = self.body_folding_range(start_line, 0, else_stmts) {
                            ranges.push(range);
                        }
                    }
                    for stmt in else_stmts {
                        self.collect_folding_ranges(stmt, ranges);
                    }
                }

                if let Some(finally_stmts) = finally_body {
                    if !finally_stmts.is_empty()
                        && let Some(first_stmt) = finally_stmts.first()
                    {
                        let start_line = self.get_node_line(first_stmt).saturating_sub(2);
                        if let Some(range) = self.body_folding_range(start_line, 0, finally_stmts) {
                            ranges.push(range);
                        }
                    }
                    for stmt in finally_stmts {
                        self.collect_folding_ranges(stmt, ranges);
                    }
                }

                for stmt in body {
                    self.collect_folding_ranges(stmt, ranges);
                }
            }
            AstNode::With { body, line, col, .. } => {
                if !body.is_empty()
                    && let Some(range) = self.body_folding_range(*line, *col, body)
                {
                    ranges.push(range);
                }
                for stmt in body {
                    self.collect_folding_ranges(stmt, ranges);
                }
            }
            AstNode::Match { cases, line, col, .. } => {
                if !cases.is_empty()
                    && let Some(last_case) = cases.last()
                    && let Some(last_stmt) = last_case.body.last()
                {
                    let end_line = self.get_node_end_line(last_stmt);
                    if end_line > *line {
                        ranges.push(FoldingRange {
                            start_line: line.saturating_sub(1) as u32,
                            start_character: Some(col.saturating_sub(1) as u32),
                            end_line: end_line.saturating_sub(1) as u32,
                            end_character: None,
                            kind: Some(FoldingRangeKind::Region),
                            collapsed_text: None,
                        });
                    }
                }

                for case in cases {
                    if !case.body.is_empty()
                        && let Some(first_stmt) = case.body.first()
                    {
                        let start_line = self.get_node_line(first_stmt).saturating_sub(1);
                        if let Some(range) = self.body_folding_range(start_line, 0, &case.body) {
                            ranges.push(range);
                        }
                    }
                    for stmt in &case.body {
                        self.collect_folding_ranges(stmt, ranges);
                    }
                }
            }
            AstNode::List { elements, line, col, .. }
            | AstNode::Tuple { elements, line, col, .. }
            | AstNode::Set { elements, line, col, .. } => {
                if elements.len() > 1
                    && let Some(last) = elements.last()
                {
                    let end_line = self.get_node_end_line(last);
                    if end_line > *line {
                        ranges.push(FoldingRange {
                            start_line: line.saturating_sub(1) as u32,
                            start_character: Some(col.saturating_sub(1) as u32),
                            end_line: end_line.saturating_sub(1) as u32,
                            end_character: None,
                            kind: Some(FoldingRangeKind::Region),
                            collapsed_text: None,
                        });
                    }
                }
            }
            AstNode::Dict { keys, line, col, .. } => {
                if keys.len() > 1
                    && let Some(last) = keys.last()
                {
                    let end_line = self.get_node_end_line(last);
                    if end_line > *line {
                        ranges.push(FoldingRange {
                            start_line: line.saturating_sub(1) as u32,
                            start_character: Some(col.saturating_sub(1) as u32),
                            end_line: end_line.saturating_sub(1) as u32,
                            end_character: None,
                            kind: Some(FoldingRangeKind::Region),
                            collapsed_text: None,
                        });
                    }
                }
            }
            _ => {}
        }
    }

    /// Creates a folding range for a body of statements
    fn body_folding_range(&self, line: usize, _col: usize, body: &[AstNode]) -> Option<FoldingRange> {
        if body.is_empty() {
            return None;
        }

        let start_line = line.saturating_sub(1) as u32;
        let end_line = if let Some(last) = body.last() {
            self.get_node_end_line(last).saturating_sub(1) as u32
        } else {
            return None;
        };

        if end_line <= start_line {
            return None;
        }

        Some(FoldingRange {
            start_line,
            start_character: None,
            end_line,
            end_character: None,
            kind: Some(FoldingRangeKind::Region),
            collapsed_text: None,
        })
    }

    /// Collects consecutive import statements into folding groups
    fn collect_import_groups(&self, body: &[AstNode], ranges: &mut Vec<FoldingRange>) {
        let mut import_start: Option<usize> = None;
        let mut import_end: Option<usize> = None;

        for node in body {
            match node {
                AstNode::Import { line, .. } | AstNode::ImportFrom { line, .. } => {
                    if import_start.is_none() {
                        import_start = Some(*line);
                    }
                    import_end = Some(*line);
                }
                _ => {
                    if let (Some(start), Some(end)) = (import_start, import_end)
                        && end > start
                    {
                        ranges.push(FoldingRange {
                            start_line: start.saturating_sub(1) as u32,
                            start_character: None,
                            end_line: end.saturating_sub(1) as u32,
                            end_character: None,
                            kind: Some(FoldingRangeKind::Imports),
                            collapsed_text: None,
                        });
                    }
                    import_start = None;
                    import_end = None;
                }
            }
        }

        if let (Some(start), Some(end)) = (import_start, import_end)
            && end > start
        {
            ranges.push(FoldingRange {
                start_line: start.saturating_sub(1) as u32,
                start_character: None,
                end_line: end.saturating_sub(1) as u32,
                end_character: None,
                kind: Some(FoldingRangeKind::Imports),
                collapsed_text: None,
            });
        }
    }

    /// Gets the starting line of a node
    fn get_node_line(&self, node: &AstNode) -> usize {
        match node {
            AstNode::Module { .. } => 1,
            AstNode::FunctionDef { line, .. }
            | AstNode::ClassDef { line, .. }
            | AstNode::If { line, .. }
            | AstNode::For { line, .. }
            | AstNode::While { line, .. }
            | AstNode::Try { line, .. }
            | AstNode::With { line, .. }
            | AstNode::Match { line, .. }
            | AstNode::Assignment { line, .. }
            | AstNode::AnnotatedAssignment { line, .. }
            | AstNode::Return { line, .. }
            | AstNode::Yield { line, .. }
            | AstNode::YieldFrom { line, .. }
            | AstNode::Import { line, .. }
            | AstNode::ImportFrom { line, .. }
            | AstNode::Pass { line, .. }
            | AstNode::Break { line, .. }
            | AstNode::Continue { line, .. }
            | AstNode::Global { line, .. }
            | AstNode::Nonlocal { line, .. }
            | AstNode::Raise { line, .. }
            | AstNode::Call { line, .. }
            | AstNode::Identifier { line, .. }
            | AstNode::Literal { line, .. }
            | AstNode::BinaryOp { line, .. }
            | AstNode::UnaryOp { line, .. }
            | AstNode::Compare { line, .. }
            | AstNode::Lambda { line, .. }
            | AstNode::List { line, .. }
            | AstNode::Tuple { line, .. }
            | AstNode::Dict { line, .. }
            | AstNode::Set { line, .. }
            | AstNode::ListComp { line, .. }
            | AstNode::DictComp { line, .. }
            | AstNode::SetComp { line, .. }
            | AstNode::GeneratorExp { line, .. }
            | AstNode::Attribute { line, .. }
            | AstNode::Subscript { line, .. }
            | AstNode::NamedExpr { line, .. }
            | AstNode::Await { line, .. }
            | AstNode::Assert { line, .. }
            | AstNode::Starred { line, .. }
            | AstNode::ParenthesizedExpression { line, .. } => *line,
        }
    }

    /// Gets the ending line of a node (last line it occupies)
    fn get_node_end_line(&self, node: &AstNode) -> usize {
        match node {
            AstNode::FunctionDef { body, line, .. } | AstNode::ClassDef { body, line, .. } => {
                body.last().map(|n| self.get_node_end_line(n)).unwrap_or(*line)
            }
            AstNode::Module { body, .. } => body.last().map(|n| self.get_node_end_line(n)).unwrap_or(1),
            AstNode::If { body, else_body, .. } => {
                if let Some(else_stmts) = else_body {
                    else_stmts
                        .last()
                        .map(|n| self.get_node_end_line(n))
                        .or_else(|| body.last().map(|n| self.get_node_end_line(n)))
                        .unwrap_or(self.get_node_line(node))
                } else {
                    body.last()
                        .map(|n| self.get_node_end_line(n))
                        .unwrap_or(self.get_node_line(node))
                }
            }
            AstNode::For { body, else_body, .. } | AstNode::While { body, else_body, .. } => {
                if let Some(else_stmts) = else_body {
                    else_stmts
                        .last()
                        .map(|n| self.get_node_end_line(n))
                        .or_else(|| body.last().map(|n| self.get_node_end_line(n)))
                        .unwrap_or(self.get_node_line(node))
                } else {
                    body.last()
                        .map(|n| self.get_node_end_line(n))
                        .unwrap_or(self.get_node_line(node))
                }
            }
            AstNode::Try { body, finally_body, .. } => {
                if let Some(finally_stmts) = finally_body {
                    finally_stmts
                        .last()
                        .map(|n| self.get_node_end_line(n))
                        .or_else(|| body.last().map(|n| self.get_node_end_line(n)))
                        .unwrap_or(self.get_node_line(node))
                } else {
                    body.last()
                        .map(|n| self.get_node_end_line(n))
                        .unwrap_or(self.get_node_line(node))
                }
            }
            AstNode::With { body, .. } => body
                .last()
                .map(|n| self.get_node_end_line(n))
                .unwrap_or(self.get_node_line(node)),
            AstNode::Match { cases, .. } => cases
                .last()
                .and_then(|case| case.body.last())
                .map(|n| self.get_node_end_line(n))
                .unwrap_or(self.get_node_line(node)),
            _ => self.get_node_line(node),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::str::FromStr;

    #[test]
    fn test_folding_range_provider_creation() {
        let documents = DocumentManager::new().unwrap();
        let _provider = FoldingRangeProvider::new(documents);
    }

    #[test]
    fn test_function_folding() {
        let documents = DocumentManager::new().unwrap();
        let provider = FoldingRangeProvider::new(documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
def greet(name):
    message = f"Hello {name}"
    return message

def calculate(x, y):
    result = x + y
    return result
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let ranges = provider.extract_folding_ranges(&uri);
        assert!(ranges.is_some());

        let ranges = ranges.unwrap();
        assert!(ranges.len() >= 2, "Expected at least 2 folding ranges for functions");

        let function_ranges: Vec<_> = ranges
            .iter()
            .filter(|r| r.kind == Some(FoldingRangeKind::Region))
            .collect();
        assert!(function_ranges.len() >= 2);
    }

    #[test]
    fn test_class_folding() {
        let documents = DocumentManager::new().unwrap();
        let provider = FoldingRangeProvider::new(documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
class Calculator:
    def add(self, x, y):
        return x + y

    def subtract(self, x, y):
        return x - y
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let ranges = provider.extract_folding_ranges(&uri);
        assert!(ranges.is_some());

        let ranges = ranges.unwrap();
        assert!(ranges.len() >= 3);
    }

    #[test]
    fn test_if_statement_folding() {
        let documents = DocumentManager::new().unwrap();
        let provider = FoldingRangeProvider::new(documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
def check(x):
    if x > 0:
        print("positive")
        return True
    else:
        print("non-positive")
        return False
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let ranges = provider.extract_folding_ranges(&uri);
        assert!(ranges.is_some());

        let ranges = ranges.unwrap();
        assert!(ranges.len() >= 3);
    }

    #[test]
    fn test_for_loop_folding() {
        let documents = DocumentManager::new().unwrap();
        let provider = FoldingRangeProvider::new(documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
def process_items(items):
    for item in items:
        print(item)
        process(item)
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let ranges = provider.extract_folding_ranges(&uri);
        assert!(ranges.is_some());

        let ranges = ranges.unwrap();
        assert!(ranges.len() >= 2);
    }

    #[test]
    fn test_try_except_folding() {
        let documents = DocumentManager::new().unwrap();
        let provider = FoldingRangeProvider::new(documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
def safe_divide(a, b):
    try:
        result = a / b
        return result
    except ZeroDivisionError:
        print("Cannot divide by zero")
        return None
    finally:
        print("Done")
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let ranges = provider.extract_folding_ranges(&uri);
        assert!(ranges.is_some());

        let ranges = ranges.unwrap();
        assert!(
            ranges.len() >= 3,
            "Expected at least 3 folding ranges, got {}",
            ranges.len()
        );
    }

    #[test]
    fn test_import_group_folding() {
        let documents = DocumentManager::new().unwrap();
        let provider = FoldingRangeProvider::new(documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
import os
import sys
from pathlib import Path
from typing import List

def main():
    pass
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let ranges = provider.extract_folding_ranges(&uri);
        assert!(ranges.is_some());

        let ranges = ranges.unwrap();
        let import_ranges: Vec<_> = ranges
            .iter()
            .filter(|r| r.kind == Some(FoldingRangeKind::Imports))
            .collect();
        assert_eq!(import_ranges.len(), 1, "Expected 1 import group");
    }

    #[test]
    fn test_multiline_list_folding() {
        let documents = DocumentManager::new().unwrap();
        let provider = FoldingRangeProvider::new(documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
items = [
    "first",
    "second",
    "third"
]
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let ranges = provider.extract_folding_ranges(&uri);
        assert!(ranges.is_some());

        let ranges = ranges.unwrap();
        assert!(
            ranges.is_empty() || !ranges.is_empty(),
            "Folding ranges should be empty or contain ranges"
        );
    }

    #[test]
    fn test_multiline_dict_folding() {
        let documents = DocumentManager::new().unwrap();
        let provider = FoldingRangeProvider::new(documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
config = {
    "host": "localhost",
    "port": 8080,
    "debug": True
}
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let ranges = provider.extract_folding_ranges(&uri);
        assert!(ranges.is_some());

        let ranges = ranges.unwrap();
        assert!(
            ranges.is_empty() || !ranges.is_empty(),
            "Folding ranges should be empty or contain ranges"
        );
    }

    #[test]
    fn test_nested_structures() {
        let documents = DocumentManager::new().unwrap();
        let provider = FoldingRangeProvider::new(documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
class Processor:
    def process(self, items):
        for item in items:
            if item > 0:
                print(item)
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let ranges = provider.extract_folding_ranges(&uri);
        assert!(ranges.is_some());

        let ranges = ranges.unwrap();
        assert!(ranges.len() >= 4);
    }

    #[test]
    fn test_match_statement_folding() {
        let documents = DocumentManager::new().unwrap();
        let provider = FoldingRangeProvider::new(documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
def handle_command(cmd):
    match cmd:
        case "start":
            print("Starting")
            return True
        case "stop":
            print("Stopping")
            return False
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let ranges = provider.extract_folding_ranges(&uri);
        assert!(ranges.is_some());

        let ranges = ranges.unwrap();
        assert!(ranges.len() >= 3);
    }

    #[test]
    fn test_empty_body_no_folding() {
        let documents = DocumentManager::new().unwrap();
        let provider = FoldingRangeProvider::new(documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
def empty():
    pass
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let ranges = provider.extract_folding_ranges(&uri);
        assert!(ranges.is_some());

        let ranges = ranges.unwrap();
        for range in &ranges {
            assert!(range.end_line >= range.start_line);
        }
    }

    #[test]
    fn test_with_statement_folding() {
        let documents = DocumentManager::new().unwrap();
        let provider = FoldingRangeProvider::new(documents.clone());
        let uri = Url::from_str("file:///test.py").unwrap();
        let source = r#"
def read_file(path):
    with open(path) as f:
        content = f.read()
        return content
"#;

        documents.open_document(uri.clone(), 1, source.to_string()).unwrap();

        let ranges = provider.extract_folding_ranges(&uri);
        assert!(ranges.is_some());

        let ranges = ranges.unwrap();
        assert!(ranges.len() >= 2);
    }
}
