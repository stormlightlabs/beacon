from __future__ import annotations

import json as json_module
from typing import TYPE_CHECKING

from .local_source import LocalSource
from .star_source import *
from .subpkg import tool
from .subpkg.reexport_chain import renamed_tool
from ..app.models import User

if TYPE_CHECKING:
    from .type_only import TypeOnly


class PublicApi:
    def build(self) -> LocalSource:
        return LocalSource("api")


def make_api() -> PublicApi:
    return PublicApi()


def use_imports(user: User) -> str:
    local = LocalSource(user.name)
    return json_module.dumps({"name": local.name, "tool": tool(), "renamed": renamed_tool()})
