"""Formatter suppression directive coverage."""

# fmt: off
def preserved( x:int,y:int )->int:
    return x+y
# fmt: on
def formatted(value: str) -> str:
    return value.strip()
# beacon-fmt: off
class PreserveBeaconFormat:
 def method( self )->None:
  pass
# beacon-fmt: on
