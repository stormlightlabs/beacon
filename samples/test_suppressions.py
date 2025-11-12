"""Test file to verify suppression behavior"""

# These should NOT be reported (suppressed)
return 42  # noqa: BEA003
import unused_module  # noqa: BEA015

# This SHOULD be reported (not suppressed)
import another_unused_module
