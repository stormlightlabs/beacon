//! Python literal parsing helpers.

use beacon_core::{ParseError, Result};

/// Parse Python float literals, handling underscores and leading zeros.
pub(crate) fn parse_python_float(text: &str) -> Result<f64> {
    let cleaned = text.replace('_', "");

    Ok(cleaned
        .parse::<f64>()
        .map_err(|e| ParseError::TreeSitterError(format!("Invalid float literal: {e}")))?)
}

/// Parse Python integer literals including hex (0x), octal (0o), binary (0b), and decimal.
///
/// Python supports arbitrary precision integers, but Beacon's AST stores i64.
/// Values outside i64 range clamp so parsing can continue.
pub(crate) fn parse_python_integer(text: &str) -> i64 {
    let cleaned = text.replace('_', "");

    if let Some(hex_str) = cleaned.strip_prefix("0x").or_else(|| cleaned.strip_prefix("0X")) {
        i64::from_str_radix(hex_str, 16).unwrap_or(i64::MAX)
    } else if let Some(oct_str) = cleaned.strip_prefix("0o").or_else(|| cleaned.strip_prefix("0O")) {
        i64::from_str_radix(oct_str, 8).unwrap_or(i64::MAX)
    } else if let Some(bin_str) = cleaned.strip_prefix("0b").or_else(|| cleaned.strip_prefix("0B")) {
        i64::from_str_radix(bin_str, 2).unwrap_or(i64::MAX)
    } else {
        cleaned
            .parse::<i64>()
            .unwrap_or_else(|_| if cleaned.starts_with('-') { i64::MIN } else { i64::MAX })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_integer_radices_and_underscores() {
        assert_eq!(parse_python_integer("1_000"), 1000);
        assert_eq!(parse_python_integer("0xff"), 255);
        assert_eq!(parse_python_integer("0o10"), 8);
        assert_eq!(parse_python_integer("0b1010"), 10);
    }

    #[test]
    fn clamps_out_of_range_integers() {
        assert_eq!(parse_python_integer("999999999999999999999999999999"), i64::MAX);
        assert_eq!(parse_python_integer("-999999999999999999999999999999"), i64::MIN);
    }

    #[test]
    fn parses_float_underscores_and_exponents() {
        assert_eq!(parse_python_float("1_000.5").unwrap(), 1000.5);
        assert_eq!(parse_python_float("1e3").unwrap(), 1000.0);
    }
}
