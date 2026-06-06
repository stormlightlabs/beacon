__all__ = ["STAR_VALUE", "star_func"]

STAR_VALUE = 7
HIDDEN_PUBLIC = "not exported by star when __all__ is present"
_PRIVATE_VALUE = "private"


def star_func() -> int:
    return STAR_VALUE
