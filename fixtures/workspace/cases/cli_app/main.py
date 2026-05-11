"""Entrypoint-like module for CLI smoke coverage."""

from __future__ import annotations

from .utils import normalize_name, parse_count


def main(argv: list[str] | None = None) -> int:
    args = argv or []
    name = normalize_name(args[0]) if args else "beacon"
    count = parse_count(args[1], default=1) if len(args) > 1 else 1
    print(f"{name}:{count}")
    return 0
