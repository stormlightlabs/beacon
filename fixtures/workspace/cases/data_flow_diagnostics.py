"""Data-flow diagnostics for Milestone 4."""

from __future__ import annotations


def data_flow_diagnostics() -> int:
    result = later_value + 1
    later_value = 41
    unused_local = 10
    if False:
        constant_dead = 1
    return result
    dead_after_return = 2
