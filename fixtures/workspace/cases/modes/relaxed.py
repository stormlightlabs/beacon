"""Relaxed-mode diagnostic fixture."""

# beacon: mode=relaxed

from __future__ import annotations


def legacy_dynamic(value):
    value.extra = 1
    return value


def loose_container(items):
    return [item.id for item in items]
