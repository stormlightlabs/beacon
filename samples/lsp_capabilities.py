"""Beacon LSP capability playground.

Open this file when testing the editor plugin to try the current feature set from
`crates/server`:

- Hover & completions: inspect `TelemetryEvent`, `CacheEntry`, or members on `cache`.
- Go to definition / workspace symbols: jump into `capabilities_support.py`.
- Diagnostics: see `inconsistent_ratio`, `latency_budget`, `impossible_branch`,
  `count_active`, and `highlight_shadowing`.
- Rename & references: rename `pipeline_id` arguments to watch cross-file updates.
- Semantic tokens & inlay hints: check pattern matching, enums, and TypeVar usage.
"""

from __future__ import annotations

from dataclasses import dataclass
from enum import Enum, auto
from pathlib import Path
from typing import Iterable, Iterator

from capabilities_support import (
    CacheEntry,
    DataProvider,
    InMemoryProvider,
    SERVICE_REGISTRY,
    prime_cache,
    register_provider,
)


class Severity(Enum):
    """Severity levels used by `TelemetryEvent`."""

    INFO = auto()
    WARNING = auto()
    ERROR = auto()


@dataclass
class TelemetryEvent:
    """Structured event emitted by the fake pipeline runner."""

    pipeline_id: str
    payload: dict[str, object]
    severity: Severity = Severity.INFO

    def as_dict(self) -> dict[str, object]:
        return {"pipeline_id": self.pipeline_id, **self.payload}


GLOBAL_PIPELINES = ["ingest", "fanout", "alerts"]
latest_pipeline = "bootstrap"


def bootstrap_registry() -> None:
    """Populate the provider registry so workspace navigation has data."""

    register_provider(
        "ingest",
        InMemoryProvider("ingest", lambda: [{"latency_ms": 120}, {"latency_ms": 95}]),
    )
    register_provider(
        "fanout",
        InMemoryProvider("fanout", lambda: [{"latency_ms": 30}, {"latency_ms": 28}]),
    )


def build_dashboard(pipeline_id: str) -> list[CacheEntry[TelemetryEvent]]:
    """Showcases go-to-definition (jumps into support module) and hover data."""

    provider = SERVICE_REGISTRY.get(pipeline_id)
    if provider is None:
        provider = InMemoryProvider(pipeline_id, lambda: [{"latency_ms": 0}])
        register_provider(pipeline_id, provider)

    def telemetry_loader() -> Iterable[TelemetryEvent]:
        for payload in provider.load():
            yield TelemetryEvent(pipeline_id=pipeline_id, payload=dict(payload))

    telemetry_provider: DataProvider[TelemetryEvent] = InMemoryProvider(
        f"{pipeline_id}-telemetry", telemetry_loader
    )
    return prime_cache(telemetry_provider)


def inconsistent_ratio(success: int, total: int) -> float:
    """Intentionally returns the wrong type to trigger HM diagnostics."""

    if total == 0:
        return "no data available"  # Beacon should surface a type mismatch here.
    return success / total


def latency_budget(eager_refresh: bool) -> int:
    """Reads a variable before assignment to trigger use-before-def diagnostics."""

    if eager_refresh:
        return budget_ms + 5  # `budget_ms` is defined later on purpose.
    budget_ms = 50
    return budget_ms


def impossible_branch(pipeline_id: str) -> str:
    """Contains unreachable code for the static analyzer."""

    raise RuntimeError(f"{pipeline_id} cannot continue")
    return pipeline_id.upper()  # Beacon should mark this line unreachable.


def count_active(events: list[TelemetryEvent]) -> int:
    """Creates an unused variable so code actions can offer quick fixes."""

    newest_event = events[-1] if events else None  # unused on purpose
    return sum(1 for event in events if event.severity is not Severity.ERROR)


def highlight_shadowing(events: list[TelemetryEvent]) -> list[str]:
    """Reuses `latest_pipeline` inside a closure to trigger shadowing diagnostics."""

    labels: list[str] = []

    def format_label(event: TelemetryEvent) -> str:
        latest_pipeline = event.pipeline_id  # shadows the module variable on purpose
        return f"{latest_pipeline}:{event.severity.name}"

    for event in events:
        labels.append(format_label(event))
    return labels


def route_event(event: TelemetryEvent) -> str:
    """Structural pattern matching showcases semantic tokens + hover info."""

    match event.severity:
        case Severity.ERROR:
            return "pagerduty"
        case Severity.WARNING if event.payload.get("latency_ms", 0) > 100:
            return "slack"
        case _:
            return "metrics"


def hydrate_from_disk(path: Path) -> Iterator[TelemetryEvent]:
    """Reads newline-delimited payloads to exercise completions on stdlib types."""

    if not path.exists():
        path.write_text(
            '{"pipeline_id": "ingest", "severity": "INFO"}\n', encoding="utf-8"
        )

    for line in path.read_text(encoding="utf-8").splitlines():
        pipeline_id = "ingest"
        payload: dict[str, object] = {"raw": line.strip()}
        yield TelemetryEvent(pipeline_id=pipeline_id, payload=payload)


def rename_target(pipeline_id: str) -> str:
    """Use rename on `pipeline_id` to verify references update across scopes."""

    return f"{pipeline_id}-{pipeline_id.count('-')}"


def main() -> None:
    bootstrap_registry()
    cache = build_dashboard("ingest")
    events = [entry.value for entry in cache]
    print(count_active(events))
    print(route_event(events[0]))


if __name__ == "__main__":
    main()
