from __future__ import annotations

from typing import Any
import sys


class Meta(type):
    pass


class WithMeta(metaclass=Meta):
    pass


class Mutable:
    pass


def replacement(fn):
    def wrapper(*args: Any, **kwargs: Any) -> Any:
        return fn(*args, **kwargs)

    return wrapper


@replacement
def decorated(value: int) -> int:
    return value


def dynamic_lookup(obj: object, name: str) -> Any:
    return getattr(obj, name)


def mutate_runtime(obj: object) -> None:
    setattr(obj, "extra", 1)
    delattr(obj, "missing")


def execute_generated(code: str) -> Any:
    exec(code)
    return eval(code)


def load_dynamic(module_name: str) -> Any:
    return __import__(module_name)


def install_import_hook(hook: object) -> None:
    sys.meta_path.append(hook)
    sys.path.append("/tmp/generated")


def make_class() -> type:
    return type("Generated", (), {"flag": True})


Mutable.__bases__ = (object,)
Mutable.__class__ = type
__all__ = ["WithMeta"]
__all__.append("decorated")
