//! Privacy-preserving telemetry event helpers for LSP instrumentation.
//!
//! Telemetry is disabled by default at the transport layer, but these event
//! shapes keep the server-side contract explicit.
//!
//! Release-safe events include timing and cache metadata without source snippets,
//! absolute paths, or symbol names.

use serde::{Deserialize, Serialize};
use std::path::Path;
use std::time::Duration;
use url::Url;

/// Release-safe telemetry event emitted by editor-facing LSP features.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct TelemetryEvent {
    /// Stable event name, for example `lsp.analysis.timing`.
    pub name: &'static str,
    /// Optional feature area associated with the event.
    pub feature: Option<&'static str>,
    /// Duration in milliseconds for timing events.
    pub duration_ms: Option<u128>,
    /// Cache hit state for cache behavior events.
    pub cache_hit: Option<bool>,
    /// Non-sensitive cache entry count.
    pub cache_size: Option<usize>,
    /// Crash/error category without panic payloads, source, paths, or symbols.
    pub error_kind: Option<String>,
    /// Sanitized file extension, e.g. `py`; never a full path.
    pub file_extension: Option<String>,
}

impl TelemetryEvent {
    /// Timing event for a feature handler.
    pub fn timing(feature: &'static str, duration: Duration, uri: Option<&Url>) -> Self {
        Self {
            name: "lsp.feature.timing",
            feature: Some(feature),
            duration_ms: Some(duration.as_millis()),
            cache_hit: None,
            cache_size: None,
            error_kind: None,
            file_extension: uri.and_then(sanitized_extension),
        }
    }

    /// Cache behavior event.
    pub fn cache(feature: &'static str, hit: bool, size: usize) -> Self {
        Self {
            name: "lsp.cache.access",
            feature: Some(feature),
            duration_ms: None,
            cache_hit: Some(hit),
            cache_size: Some(size),
            error_kind: None,
            file_extension: None,
        }
    }

    /// Crash event that deliberately excludes panic payloads and locations.
    pub fn crash(kind: impl Into<String>) -> Self {
        Self {
            name: "lsp.crash",
            feature: None,
            duration_ms: None,
            cache_hit: None,
            cache_size: None,
            error_kind: Some(sanitize_error_kind(kind)),
            file_extension: None,
        }
    }

    /// Feature error event that includes the category, not raw user content.
    pub fn feature_error(feature: &'static str, error_kind: impl Into<String>, uri: Option<&Url>) -> Self {
        Self {
            name: "lsp.feature.error",
            feature: Some(feature),
            duration_ms: None,
            cache_hit: None,
            cache_size: None,
            error_kind: Some(sanitize_error_kind(error_kind)),
            file_extension: uri.and_then(sanitized_extension),
        }
    }
}

fn sanitized_extension(uri: &Url) -> Option<String> {
    let path = uri.to_file_path().ok()?;
    path.extension().and_then(|ext| ext.to_str()).map(str::to_string)
}

fn sanitize_error_kind(error_kind: impl Into<String>) -> String {
    let raw = error_kind.into();
    let lower = raw.to_ascii_lowercase();

    if lower.contains("panic") || lower.contains("crash") {
        "crash".to_string()
    } else if lower.contains("parse") {
        "parse_error".to_string()
    } else if lower.contains("cache") {
        "cache_error".to_string()
    } else if lower.contains("timeout") || lower.contains("timed out") {
        "timeout".to_string()
    } else if lower.contains("inspect") || lower.contains("introspect") {
        "inspection_error".to_string()
    } else if Path::new(&raw).components().count() > 1 || lower.contains('/') || lower.contains('\\') {
        "path_error".to_string()
    } else {
        "feature_error".to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn release_payload(event: &TelemetryEvent) -> String {
        serde_json::to_string(event).expect("telemetry event should serialize")
    }

    #[test]
    fn timing_events_include_duration_without_absolute_paths_or_symbols() {
        let uri = Url::from_file_path("/Users/owais/secret/project/module.py").expect("file URI");
        let payload = release_payload(&TelemetryEvent::timing("hover", Duration::from_millis(42), Some(&uri)));

        assert!(payload.contains("lsp.feature.timing"));
        assert!(payload.contains("duration_ms"));
        assert!(payload.contains("\"file_extension\":\"py\""));
        assert!(!payload.contains("/Users/owais"));
        assert!(!payload.contains("module.py"));
        assert!(!payload.contains("secret"));
    }

    #[test]
    fn cache_events_capture_hit_state_and_size() {
        let payload = release_payload(&TelemetryEvent::cache("analysis", true, 12));

        assert!(payload.contains("lsp.cache.access"));
        assert!(payload.contains("\"cache_hit\":true"));
        assert!(payload.contains("\"cache_size\":12"));
    }

    #[test]
    fn crash_and_feature_error_events_omit_payload_paths_and_symbol_names() {
        let uri = Url::from_file_path("/tmp/workspace/private.py").expect("file URI");
        let crash_payload = release_payload(&TelemetryEvent::crash(
            "/tmp/workspace/private.py: crashed while resolving CustomerToken",
        ));
        let error_payload = release_payload(&TelemetryEvent::feature_error(
            "completion",
            "failed to inspect CustomerToken source = 'secret'",
            Some(&uri),
        ));

        for payload in [crash_payload, error_payload] {
            assert!(!payload.contains("/tmp/workspace"));
            assert!(!payload.contains("private.py"));
            assert!(!payload.contains("CustomerToken"));
            assert!(!payload.contains("secret"));
        }
    }
}
