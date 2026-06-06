from .local_source import LocalSource as PublicLocalSource
from .star_source import STAR_VALUE, star_func

exported_value = STAR_VALUE

__all__ = ["PublicLocalSource", "STAR_VALUE", "star_func", "exported_value"]
