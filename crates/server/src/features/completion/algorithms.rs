//! Completion scoring and matching algorithms
//!
//! This module provides various string similarity algorithms for ranking and filtering
//! code completions. All algorithms implement the `StringSimilarity` trait to provide
//! a consistent interface.

/// Trait for string similarity algorithms used in completion ranking.
///
/// Implementations should return a normalized score between 0.0 and 1.0,
/// where 1.0 indicates identical strings and 0.0 indicates maximum dissimilarity.
pub trait StringSimilarity {
    /// Calculate similarity score between source (typed text) and target (completion candidate).
    ///
    /// # Arguments
    /// * `source` - The text typed by the user (query)
    /// * `target` - The completion candidate to score
    ///
    /// # Returns
    /// Similarity score between 0.0 (completely different) and 1.0 (identical)
    fn similarity(&self, source: &str, target: &str) -> f64;
}

/// Levenshtein edit distance algorithm.
///
/// Calculates the minimum number of single-character edits (insertions, deletions,
/// or substitutions) required to transform one string into another. The distance
/// is converted to a similarity score between 0.0 and 1.0.
///
/// # Algorithm
/// Uses dynamic programming with O(m*n) time complexity and O(min(m,n)) space complexity,
/// where m and n are the lengths of the input strings.
pub struct LevenshteinDistance {
    /// Whether to perform case-insensitive matching
    case_insensitive: bool,
}

impl LevenshteinDistance {
    /// Create a new Levenshtein distance calculator with case-sensitive matching
    pub fn new() -> Self {
        Self { case_insensitive: false }
    }

    /// Create a new Levenshtein distance calculator with case-insensitive matching
    pub fn case_insensitive() -> Self {
        Self { case_insensitive: true }
    }

    /// Calculate the raw edit distance between two strings
    fn distance(&self, a: &str, b: &str) -> usize {
        let a = if self.case_insensitive { a.to_lowercase() } else { a.to_string() };
        let b = if self.case_insensitive { b.to_lowercase() } else { b.to_string() };

        let a_chars: Vec<char> = a.chars().collect();
        let b_chars: Vec<char> = b.chars().collect();

        let a_len = a_chars.len();
        let b_len = b_chars.len();

        if a_len == 0 {
            return b_len;
        }
        if b_len == 0 {
            return a_len;
        }

        // Use space-optimized version with two rows
        let mut prev_row: Vec<usize> = (0..=b_len).collect();
        let mut curr_row: Vec<usize> = vec![0; b_len + 1];

        for i in 1..=a_len {
            curr_row[0] = i;

            for j in 1..=b_len {
                let cost = if a_chars[i - 1] == b_chars[j - 1] { 0 } else { 1 };

                curr_row[j] = (prev_row[j] + 1) // deletion
                    .min(curr_row[j - 1] + 1) // insertion
                    .min(prev_row[j - 1] + cost); // substitution
            }

            std::mem::swap(&mut prev_row, &mut curr_row);
        }

        prev_row[b_len]
    }
}

impl Default for LevenshteinDistance {
    fn default() -> Self {
        Self::new()
    }
}

impl StringSimilarity for LevenshteinDistance {
    fn similarity(&self, source: &str, target: &str) -> f64 {
        let distance = self.distance(source, target);
        let max_len = source.len().max(target.len());

        if max_len == 0 {
            return 1.0;
        }

        1.0 - (distance as f64 / max_len as f64)
    }
}

/// Sorensen-Dice coefficient algorithm.
///
/// Measures similarity based on bigrams (pairs of adjacent characters).
/// The coefficient ranges from 0.0 (no common bigrams) to 1.0 (identical strings).
///
/// # Formula
/// `coefficient = 2 * |common_bigrams| / (|bigrams_a| + |bigrams_b|)`
///
/// # Advantages
/// - More tolerant of character transpositions than Levenshtein
/// - Better for matching similar but reordered strings
/// - Less sensitive to string length differences
pub struct SorensenDiceCoefficient {
    /// Whether to perform case-insensitive matching
    case_insensitive: bool,
}

impl SorensenDiceCoefficient {
    /// Create a new Sorensen-Dice calculator with case-sensitive matching
    pub fn new() -> Self {
        Self { case_insensitive: false }
    }

    /// Create a new Sorensen-Dice calculator with case-insensitive matching
    pub fn case_insensitive() -> Self {
        Self { case_insensitive: true }
    }

    /// Extract bigrams from a string
    fn bigrams(&self, s: &str) -> Vec<String> {
        let s = if self.case_insensitive { s.to_lowercase() } else { s.to_string() };

        let chars: Vec<char> = s.chars().collect();
        if chars.len() < 2 {
            return Vec::new();
        }

        chars.windows(2).map(|w| format!("{}{}", w[0], w[1])).collect()
    }
}

impl Default for SorensenDiceCoefficient {
    fn default() -> Self {
        Self::new()
    }
}

impl StringSimilarity for SorensenDiceCoefficient {
    fn similarity(&self, source: &str, target: &str) -> f64 {
        let source_bigrams = self.bigrams(source);
        let target_bigrams = self.bigrams(target);

        if source_bigrams.is_empty() && target_bigrams.is_empty() {
            return 1.0;
        }
        if source_bigrams.is_empty() || target_bigrams.is_empty() {
            return 0.0;
        }

        // Count common bigrams
        let mut source_counts = std::collections::HashMap::new();
        for bigram in source_bigrams.iter() {
            *source_counts.entry(bigram).or_insert(0) += 1;
        }

        let mut common = 0;
        for bigram in target_bigrams.iter() {
            if let Some(count) = source_counts.get_mut(&bigram) {
                if *count > 0 {
                    common += 1;
                    *count -= 1;
                }
            }
        }

        (2.0 * common as f64) / (source_bigrams.len() + target_bigrams.len()) as f64
    }
}

/// Prefix matching with position-aware scoring.
///
/// Provides efficient prefix matching with bonus scoring for:
/// - Exact prefix matches (highest score)
/// - Case-insensitive matches
/// - Word boundary matches (camelCase, snake_case)
/// - Earlier match positions
pub struct PrefixMatcher {
    /// Whether to perform case-insensitive matching
    case_insensitive: bool,
}

impl PrefixMatcher {
    /// Create a new prefix matcher with case-sensitive matching
    pub fn new() -> Self {
        Self { case_insensitive: false }
    }

    /// Create a new prefix matcher with case-insensitive matching
    pub fn case_insensitive() -> Self {
        Self { case_insensitive: true }
    }

    /// Check if target starts with source, respecting case sensitivity
    pub fn matches(&self, source: &str, target: &str) -> bool {
        if self.case_insensitive {
            target.to_lowercase().starts_with(&source.to_lowercase())
        } else {
            target.starts_with(source)
        }
    }

    /// Check for camelCase or snake_case word boundary match
    ///
    /// Examples:
    /// - "gTW" matches "getTimeWindow" (camelCase initials)
    /// - "g_t" matches "get_time_window" (snake_case)
    fn word_boundary_match(&self, source: &str, target: &str) -> bool {
        let source = if self.case_insensitive { source.to_lowercase() } else { source.to_string() };

        let source_chars: Vec<char> = source.chars().collect();
        let target_chars: Vec<char> = target.chars().collect();

        if source_chars.is_empty() {
            return true;
        }

        let mut source_idx = 0;
        let mut target_idx = 0;

        while source_idx < source_chars.len() && target_idx < target_chars.len() {
            let target_char = if self.case_insensitive {
                target_chars[target_idx].to_ascii_lowercase()
            } else {
                target_chars[target_idx]
            };

            if source_chars[source_idx] == target_char {
                source_idx += 1;
                target_idx += 1;
            } else {
                // Look for next word boundary (capital letter or underscore)
                target_idx += 1;
                while target_idx < target_chars.len() {
                    let ch = target_chars[target_idx];
                    if ch.is_uppercase() || ch == '_' {
                        break;
                    }
                    target_idx += 1;
                }
            }
        }

        source_idx == source_chars.len()
    }
}

impl Default for PrefixMatcher {
    fn default() -> Self {
        Self::new()
    }
}

impl StringSimilarity for PrefixMatcher {
    fn similarity(&self, source: &str, target: &str) -> f64 {
        if source.is_empty() {
            return 1.0;
        }
        if target.is_empty() {
            return 0.0;
        }

        // Exact prefix match gets highest score
        if self.matches(source, target) {
            // Longer matches relative to target length score higher
            let prefix_ratio = source.len() as f64 / target.len() as f64;
            return 0.9 + (prefix_ratio * 0.1); // Range: 0.9 to 1.0
        }

        // Word boundary match gets medium-high score
        if self.word_boundary_match(source, target) {
            return 0.7;
        }

        0.0
    }
}

/// Fuzzy matcher combining multiple similarity algorithms.
///
/// Uses weighted combination of Levenshtein distance and Sorensen-Dice coefficient
/// to provide robust fuzzy matching that handles both character-level edits and
/// structural similarity.
///
/// # Default Weights
/// - Levenshtein: 0.4 (handles typos and small edits)
/// - Sorensen-Dice: 0.6 (handles character reordering and longer sequences)
pub struct FuzzyMatcher {
    levenshtein: LevenshteinDistance,
    sorensen_dice: SorensenDiceCoefficient,
    levenshtein_weight: f64,
    sorensen_dice_weight: f64,
    /// Minimum similarity threshold (0.0 to 1.0) for a match
    threshold: f64,
}

impl FuzzyMatcher {
    /// Create a new fuzzy matcher with default weights
    pub fn new() -> Self {
        Self::default()
    }

    /// Create a fuzzy matcher with custom weights
    ///
    /// # Arguments
    /// * `levenshtein_weight` - Weight for Levenshtein distance (typically 0.3-0.5)
    /// * `sorensen_dice_weight` - Weight for Sorensen-Dice coefficient (typically 0.5-0.7)
    /// * `threshold` - Minimum similarity score to consider a match (0.0-1.0)
    pub fn _with_weights(levenshtein_weight: f64, sorensen_dice_weight: f64, threshold: f64) -> Self {
        Self {
            levenshtein: LevenshteinDistance::case_insensitive(),
            sorensen_dice: SorensenDiceCoefficient::case_insensitive(),
            levenshtein_weight,
            sorensen_dice_weight,
            threshold,
        }
    }

    /// Check if source fuzzy matches target above the threshold
    pub fn _matches(&self, source: &str, target: &str) -> bool {
        self.similarity(source, target) >= self.threshold
    }

    /// Get the configured threshold
    pub fn threshold(&self) -> f64 {
        self.threshold
    }
}

impl Default for FuzzyMatcher {
    fn default() -> Self {
        Self {
            levenshtein: LevenshteinDistance::case_insensitive(),
            sorensen_dice: SorensenDiceCoefficient::case_insensitive(),
            levenshtein_weight: 0.4,
            sorensen_dice_weight: 0.6,
            threshold: 0.3,
        }
    }
}

impl StringSimilarity for FuzzyMatcher {
    fn similarity(&self, source: &str, target: &str) -> f64 {
        let lev_score = self.levenshtein.similarity(source, target);
        let dice_score = self.sorensen_dice.similarity(source, target);
        let total_weight = self.levenshtein_weight + self.sorensen_dice_weight;
        (lev_score * self.levenshtein_weight + dice_score * self.sorensen_dice_weight) / total_weight
    }
}

/// Rocchio-inspired relevance scoring for completions.
///
/// Simplified implementation without persistent user feedback. Scores completions based on:
/// - Name similarity to query (α weight)
/// - Scope proximity (β weight) - symbols in closer scopes rank higher
/// - Symbol type relevance (β weight) - certain types may be preferred
///
/// Traditional Rocchio: Q_new = α*Q + β*(Σ relevant/|R|) - γ*(Σ non_relevant/|NR|)
///
/// Our adaptation: score = α * name_similarity + β * relevance_features where relevance_features = scope_proximity + type_bonus
pub struct RocchioScorer {
    /// Weight for name similarity (alpha in Rocchio)
    name_weight: f64,
    /// Weight for relevance features (beta in Rocchio)
    relevance_weight: f64,
    /// Underlying similarity algorithm for name matching
    similarity_algo: Box<dyn StringSimilarity + Send + Sync>,
}

impl RocchioScorer {
    /// Create a new Rocchio scorer with default weights
    ///
    /// Default: 60% name similarity, 40% relevance features
    pub fn new() -> Self {
        Self::default()
    }

    /// Create a Rocchio scorer with custom weights
    pub fn _with_weights(name_weight: f64, relevance_weight: f64) -> Self {
        Self { name_weight, relevance_weight, similarity_algo: Box::new(FuzzyMatcher::new()) }
    }

    /// Create a Rocchio scorer with a custom similarity algorithm
    pub fn _with_similarity(similarity_algo: Box<dyn StringSimilarity + Send + Sync>) -> Self {
        Self { name_weight: 0.6, relevance_weight: 0.4, similarity_algo }
    }

    /// Score a completion with relevance features
    ///
    /// # Arguments
    /// * `source` - The typed query
    /// * `target` - The completion candidate name
    /// * `scope_distance` - How many scopes away (0 = same scope, 1 = parent, etc.)
    /// * `type_bonus` - Bonus for symbol type (e.g., 1.0 for functions, 0.5 for variables)
    pub fn score_with_context(&self, source: &str, target: &str, scope_distance: usize, type_bonus: f64) -> f64 {
        let name_similarity = self.similarity_algo.similarity(source, target);
        let scope_proximity = (1.0 - (scope_distance as f64 * 0.2)).max(0.0);
        let relevance_features = (scope_proximity + type_bonus.min(1.0)) / 2.0;
        let total_weight = self.name_weight + self.relevance_weight;
        (name_similarity * self.name_weight + relevance_features * self.relevance_weight) / total_weight
    }
}

impl Default for RocchioScorer {
    fn default() -> Self {
        Self { name_weight: 0.6, relevance_weight: 0.4, similarity_algo: Box::new(FuzzyMatcher::new()) }
    }
}

impl StringSimilarity for RocchioScorer {
    fn similarity(&self, source: &str, target: &str) -> f64 {
        self.similarity_algo.similarity(source, target)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_levenshtein_identical() {
        let lev = LevenshteinDistance::new();
        assert_eq!(lev.similarity("hello", "hello"), 1.0);
    }

    #[test]
    fn test_levenshtein_empty() {
        let lev = LevenshteinDistance::new();
        assert_eq!(lev.similarity("", ""), 1.0);
        assert_eq!(lev.similarity("hello", ""), 0.0);
        assert_eq!(lev.similarity("", "hello"), 0.0);
    }

    #[test]
    fn test_levenshtein_single_edit() {
        let lev = LevenshteinDistance::new();
        assert_eq!(lev.similarity("hello", "hallo"), 0.8);
    }

    #[test]
    fn test_levenshtein_case_insensitive() {
        let lev = LevenshteinDistance::case_insensitive();
        assert_eq!(lev.similarity("Hello", "hello"), 1.0);
        assert_eq!(lev.similarity("HELLO", "hello"), 1.0);
    }

    #[test]
    fn test_levenshtein_completely_different() {
        let lev = LevenshteinDistance::new();
        // "abc" -> "xyz" requires 3 substitutions, max_len = 3
        assert_eq!(lev.similarity("abc", "xyz"), 0.0);
    }

    #[test]
    fn test_sorensen_dice_identical() {
        let dice = SorensenDiceCoefficient::new();
        assert_eq!(dice.similarity("hello", "hello"), 1.0);
    }

    #[test]
    fn test_sorensen_dice_empty() {
        let dice = SorensenDiceCoefficient::new();
        assert_eq!(dice.similarity("", ""), 1.0);
        assert_eq!(dice.similarity("hello", ""), 0.0);
        assert_eq!(dice.similarity("", "hello"), 0.0);
    }

    #[test]
    fn test_sorensen_dice_partial_match() {
        let dice = SorensenDiceCoefficient::new();
        // "night" has bigrams: [ni, ig, gh, ht]
        // "nacht" has bigrams: [na, ac, ch, ht]
        // Common: [ht] = 1
        // Coefficient = 2*1 / (4+4) = 0.25
        assert_eq!(dice.similarity("night", "nacht"), 0.25);
    }

    #[test]
    fn test_sorensen_dice_case_insensitive() {
        let dice = SorensenDiceCoefficient::case_insensitive();
        assert_eq!(dice.similarity("Hello", "hello"), 1.0);
    }

    #[test]
    fn test_prefix_matcher_exact() {
        let prefix = PrefixMatcher::new();
        let score = prefix.similarity("hel", "hello");
        assert!((0.9..=1.0).contains(&score));
    }

    #[test]
    fn test_prefix_matcher_no_match() {
        let prefix = PrefixMatcher::new();
        assert_eq!(prefix.similarity("xyz", "hello"), 0.0);
    }

    #[test]
    fn test_prefix_matcher_case_insensitive() {
        let prefix = PrefixMatcher::case_insensitive();
        let score = prefix.similarity("HEL", "hello");
        assert!(score >= 0.9);
    }

    #[test]
    fn test_prefix_matcher_empty_source() {
        let prefix = PrefixMatcher::new();
        assert_eq!(prefix.similarity("", "hello"), 1.0);
    }

    #[test]
    fn test_prefix_matcher_word_boundary() {
        let prefix = PrefixMatcher::case_insensitive();
        assert!(prefix.similarity("gtw", "getTimeWindow") > 0.5);
    }

    #[test]
    fn test_fuzzy_matcher_exact() {
        let fuzzy = FuzzyMatcher::new();
        let score = fuzzy.similarity("hello", "hello");
        assert!(score > 0.9);
    }

    #[test]
    fn test_fuzzy_matcher_typo() {
        let fuzzy = FuzzyMatcher::new();
        let score = fuzzy.similarity("helo", "hello");
        assert!(score > 0.5);
    }

    #[test]
    fn test_fuzzy_matcher_matches() {
        let fuzzy = FuzzyMatcher::new();
        assert!(fuzzy._matches("hello", "hello"));
        assert!(fuzzy._matches("helo", "hello"));
        assert!(!fuzzy._matches("xyz", "hello"));
    }

    #[test]
    fn test_rocchio_scorer_with_context() {
        let rocchio = RocchioScorer::new();
        let score1 = rocchio.score_with_context("getPr", "getPrice", 0, 1.0);
        let score2 = rocchio.score_with_context("getPr", "getPrice", 2, 0.5);
        assert!(score1 > score2);
    }

    #[test]
    fn test_rocchio_scorer_name_similarity() {
        let rocchio = RocchioScorer::new();
        let score1 = rocchio.score_with_context("get", "getData", 0, 1.0);
        let score2 = rocchio.score_with_context("get", "xyz", 0, 1.0);
        assert!(score1 > score2);
    }

    #[test]
    fn test_rocchio_scorer_scope_distance() {
        let rocchio = RocchioScorer::new();
        let score_close = rocchio.score_with_context("foo", "foobar", 0, 0.5);
        let score_far = rocchio.score_with_context("foo", "foobar", 3, 0.5);

        assert!(score_close > score_far);
    }
}
