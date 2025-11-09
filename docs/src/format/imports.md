# Import Formatting

Beacon's formatter provides PEP8-compliant import sorting and formatting with intelligent grouping and deduplication.

## Import Groups

Imports are automatically organized into three groups following PEP8 style:

1. **Standard library imports** - Python's built-in modules (os, sys, json, etc.)
2. **Third-party imports** - External packages (numpy, django, requests, etc.)
3. **Local imports** - Relative imports from your project (., .., .models, etc.)

Each group is separated by a blank line for clarity.

## Sorting Within Groups

Within each group, imports are sorted alphabetically by module name:

- Simple `import` statements are sorted before `from` imports
- Multiple names in `from` imports are alphabetically sorted
- Duplicate imports are automatically removed

## Multi-line Imports

When `from` imports exceed the configured line length, they are automatically wrapped:

```python
# Short enough for one line
from os import environ, path

# Exceeds line length - uses multi-line format
from collections import (
    Counter,
    OrderedDict,
    defaultdict,
    namedtuple,
)
```

## Standard Library Detection

Beacon includes a comprehensive list of Python standard library modules for accurate categorization.
Third-party packages are automatically identified when they don't match known stdlib modules.

## Configuration

Import formatting respects these configuration options:

- `beacon.formatting.lineLength` - Controls when to wrap multi-line imports
- `beacon.formatting.importSorting` - Set to `pep8` for standard sorting (default)

## Example

Input:

```python
from numpy import array
import sys
from .models import User
import os
from django.db import models
```

Output:

```python
import os
import sys

from django.db import models
from numpy import array

from .models import User
```
