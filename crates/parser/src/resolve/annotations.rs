use beacon_core::Result;

use super::{NameResolver, ReferenceKind};

impl NameResolver {
    pub(crate) fn track_type_annotation_references(&mut self, type_ann: &str, line: usize, col: usize) -> Result<()> {
        const BUILTINS: &[&str] = &[
            "int",
            "str",
            "bool",
            "float",
            "complex",
            "bytes",
            "bytearray",
            "list",
            "tuple",
            "dict",
            "set",
            "frozenset",
            "object",
            "type",
            "None",
            "NoneType",
        ];

        let mut current_word = String::new();
        let mut in_string = false;
        let mut string_char = '\0';
        let mut current_line = line;
        let mut current_col = col;
        let mut word_start_line = line;
        let mut word_start_col = col;

        let flush_word = |resolver: &mut NameResolver,
                          word: &mut String,
                          start_line: usize,
                          start_col: usize|
         -> Result<()> {
            if word.is_empty() {
                return Ok(());
            }

            if !word.chars().next().unwrap().is_ascii_digit() && !BUILTINS.contains(&word.as_str()) {
                let byte_offset = resolver.line_col_to_byte_offset(start_line, start_col);
                let scope = resolver.symbol_table.find_scope_at_position(byte_offset);
                let end_col = start_col + word.chars().count();
                resolver
                    .symbol_table
                    .add_reference(word, scope, start_line, start_col, end_col, ReferenceKind::Read);
            }

            word.clear();
            Ok(())
        };

        for ch in type_ann.chars() {
            if ch == '\n' {
                if !in_string {
                    flush_word(self, &mut current_word, word_start_line, word_start_col)?;
                }
                current_line += 1;
                current_col = 1;
                continue;
            }

            match ch {
                '"' | '\'' if !in_string => {
                    in_string = true;
                    string_char = ch;
                }
                '"' | '\'' if in_string && ch == string_char => {
                    in_string = false;
                }
                'a'..='z' | 'A'..='Z' | '0'..='9' | '_' if !in_string => {
                    if current_word.is_empty() {
                        word_start_line = current_line;
                        word_start_col = current_col;
                    }
                    current_word.push(ch);
                }
                _ if !in_string => {
                    flush_word(self, &mut current_word, word_start_line, word_start_col)?;
                }
                _ => {}
            }

            if ch != '\n' {
                current_col += 1;
            }
        }

        flush_word(self, &mut current_word, word_start_line, word_start_col)?;

        Ok(())
    }
}
