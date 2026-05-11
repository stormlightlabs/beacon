use crate::rules::RuleKind;

use beacon_parser::{AstNode, LiteralValue, ReferenceKind, SymbolKind};

use super::Linter;

impl<'a> Linter<'a> {
    pub(super) fn extract_all_exports(node: &AstNode) -> Option<Vec<String>> {
        match node {
            AstNode::Module { body, .. } => {
                for stmt in body {
                    if let AstNode::Assignment { target, value, .. } = stmt {
                        let target_name = target.target_display();
                        if target_name == "__all__" {
                            return Self::extract_list_literals(value);
                        }
                    }
                }
                None
            }
            _ => None,
        }
    }

    pub(super) fn extract_list_literals(node: &AstNode) -> Option<Vec<String>> {
        match node {
            AstNode::List { elements, .. } | AstNode::Tuple { elements, .. } => {
                let mut exports = Vec::new();
                for elem in elements {
                    if let AstNode::Literal { value: LiteralValue::String { value, .. }, .. } = elem {
                        exports.push(value.clone());
                    }
                }
                Some(exports)
            }
            _ => None,
        }
    }

    pub(super) fn check_symbol_table_rules(&mut self) {
        self.check_unused_imports();
        self.check_unused_annotations();
        self.check_redefined_while_unused();
    }

    pub(super) fn check_unused_imports(&mut self) {
        const FUTURE_IMPORTS: &[&str] = &["annotations", "barry_as_FLUFL"];

        for scope in self.symbol_table.scopes.values() {
            for symbol in scope.symbols.values() {
                if symbol.kind != SymbolKind::Import {
                    continue;
                }

                if symbol.name.starts_with('_') {
                    continue;
                }

                if FUTURE_IMPORTS.contains(&symbol.name.as_str()) {
                    continue;
                }

                let has_read = symbol.references.iter().any(|r| r.kind == ReferenceKind::Read);

                if !has_read {
                    if let Some(ref all_exports) = self.all_exports
                        && all_exports.contains(&symbol.name)
                    {
                        continue;
                    }

                    self.report(
                        RuleKind::UnusedImport,
                        format!("'{}' imported but never used", symbol.name),
                        symbol.line,
                        symbol.col,
                        symbol.end_col,
                    );
                }
            }
        }
    }

    pub(super) fn check_unused_annotations(&mut self) {
        for scope in self.symbol_table.scopes.values() {
            if scope.kind == beacon_parser::ScopeKind::Class {
                let is_dataclass = self.ctx.dataclass_scopes.values().any(|&v| v);
                let is_protocol = self.ctx.protocol_scopes.values().any(|&v| v);

                if is_dataclass || is_protocol {
                    continue;
                }
            }

            for symbol in scope.symbols.values() {
                if symbol.kind != SymbolKind::Variable {
                    continue;
                }

                if symbol.name.starts_with('_') {
                    continue;
                }

                let has_read = symbol.references.iter().any(|r| r.kind == ReferenceKind::Read);
                let has_write = symbol.references.iter().any(|r| r.kind == ReferenceKind::Write);

                if !has_read && !has_write {
                    self.report(
                        RuleKind::UnusedAnnotation,
                        format!("Annotated variable '{}' is never used", symbol.name),
                        symbol.line,
                        symbol.col,
                        symbol.end_col,
                    );
                }
            }
        }
    }

    pub(super) fn check_redefined_while_unused(&mut self) {
        for scope in self.symbol_table.scopes.values() {
            for symbol in scope.symbols.values() {
                if symbol.kind != SymbolKind::Variable {
                    continue;
                }

                if symbol.name.starts_with('_') {
                    continue;
                }

                let writes: Vec<_> = symbol
                    .references
                    .iter()
                    .filter(|r| r.kind == ReferenceKind::Write)
                    .collect();

                if writes.len() < 2 {
                    continue;
                }

                for i in 1..writes.len() {
                    let prev_write = writes[i - 1];
                    let curr_write = writes[i];

                    let has_read_between = symbol.references.iter().any(|r| {
                        r.kind == ReferenceKind::Read
                            && ((r.line > prev_write.line) || (r.line == prev_write.line && r.col > prev_write.col))
                            && ((r.line < curr_write.line) || (r.line == curr_write.line && r.col < curr_write.col))
                    });

                    if !has_read_between {
                        self.report(
                            RuleKind::RedefinedWhileUnused,
                            format!("'{}' is redefined before being used", symbol.name),
                            curr_write.line,
                            curr_write.col,
                            curr_write.end_col,
                        );
                        break;
                    }
                }
            }
        }
    }
}
