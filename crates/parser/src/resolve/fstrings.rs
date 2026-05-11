use beacon_core::Result;

use super::{NameResolver, ReferenceKind};

impl NameResolver {
    pub(crate) fn track_fstring_references(&mut self, template: &str, line: usize, col: usize) -> Result<()> {
        let mut chars = template.chars().peekable();

        while let Some(ch) = chars.next() {
            if ch == '{' {
                if chars.peek() == Some(&'{') {
                    chars.next();
                    continue;
                }

                let mut expr = String::new();
                let mut brace_depth = 1;

                for ch in chars.by_ref() {
                    if ch == '{' {
                        brace_depth += 1;
                        expr.push(ch);
                    } else if ch == '}' {
                        brace_depth -= 1;
                        if brace_depth == 0 {
                            break;
                        }
                        expr.push(ch);
                    } else {
                        expr.push(ch);
                    }
                }

                self.extract_fstring_identifiers(&expr, line, col)?;
            }
        }

        Ok(())
    }

    pub(crate) fn extract_fstring_identifiers(&mut self, expr: &str, line: usize, col: usize) -> Result<()> {
        let expr_ = if let Some(idx) = expr.find(':') { &expr[..idx] } else { expr }.trim();
        let expr = if let Some(idx) = expr_.find('!') { &expr_[..idx] } else { expr_ }.trim();

        if expr.is_empty() {
            return Ok(());
        }

        let base_id = if let Some(idx) = expr.find(['.', '[', '(']) { &expr[..idx] } else { expr }.trim();

        if !base_id.is_empty() && (base_id.chars().next().unwrap().is_alphabetic() || base_id.starts_with('_')) {
            let byte_offset = self.line_col_to_byte_offset(line, col);
            let scope = self.symbol_table.find_scope_at_position(byte_offset);
            let end_col = col + base_id.len();
            self.symbol_table
                .add_reference(base_id, scope, line, col, end_col, ReferenceKind::Read);
        }

        Ok(())
    }
}
