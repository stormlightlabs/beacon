use super::NameResolver;

impl NameResolver {
    pub(crate) fn compute_decorator_spans(
        &self, start_line: usize, decorators: &[String],
    ) -> Vec<Option<(usize, usize, usize)>> {
        if decorators.is_empty() {
            return Vec::new();
        }

        let mut spans = vec![None; decorators.len()];
        if start_line == 0 {
            return spans;
        }

        let lines: Vec<&str> = self.source.lines().collect();
        let mut search_line = start_line.saturating_sub(1);

        for idx in (0..decorators.len()).rev() {
            let decorator = &decorators[idx];
            while search_line > 0 {
                if let Some(line_text) = lines.get(search_line - 1)
                    && let Some((col, end_col)) = Self::decorator_columns(line_text, decorator)
                {
                    spans[idx] = Some((search_line, col, end_col));
                    if search_line == 1 {
                        break;
                    }
                    search_line -= 1;
                    break;
                }

                if search_line == 1 {
                    break;
                }
                search_line -= 1;
            }
        }

        spans
    }

    pub(crate) fn decorator_columns(line_text: &str, decorator: &str) -> Option<(usize, usize)> {
        let trimmed = line_text.trim_start_matches([' ', '\t']);
        if !trimmed.starts_with('@') {
            return None;
        }

        let after_at = &trimmed[1..];
        let trimmed_after = after_at.trim_start_matches([' ', '\t']);
        if !trimmed_after.starts_with(decorator) {
            return None;
        }

        let decorator_char_len = decorator.chars().count();
        let mut chars_iter = trimmed_after.chars();
        for _ in 0..decorator_char_len {
            chars_iter.next();
        }
        if let Some(next_char) = chars_iter.next()
            && (next_char == '_' || next_char.is_alphanumeric())
        {
            return None;
        }

        let leading_ws_bytes = line_text.len() - trimmed.len();
        let spaces_after_at = after_at.len() - trimmed_after.len();
        let start_byte = leading_ws_bytes + 1 + spaces_after_at;
        if start_byte > line_text.len() {
            return None;
        }

        let start_col = line_text[..start_byte].chars().count() + 1;
        let end_col = start_col + decorator_char_len;
        Some((start_col, end_col))
    }

    pub(crate) fn line_col_to_byte_offset(&self, line: usize, col: usize) -> usize {
        crate::line_col_to_byte_offset_lossy(&self.source, line, col)
    }
}
