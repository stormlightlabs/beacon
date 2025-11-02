"""Object-oriented filesystem paths.

This stub provides Path classes for filesystem operations."""

from typing import Iterator, Any
from os import PathLike

class PurePath:
    """Base class for path manipulation without I/O."""

    parts: tuple[str, ...]
    drive: str
    root: str
    anchor: str
    name: str
    suffix: str
    suffixes: list[str]
    stem: str

    def __init__(self, *pathsegments: str | PathLike) -> None: ...

    def __str__(self) -> str: ...
    def __repr__(self) -> str: ...
    def __hash__(self) -> int: ...
    def __eq__(self, other: object) -> bool: ...
    def __lt__(self, other: PurePath) -> bool: ...
    def __truediv__(self, other: str | PurePath) -> PurePath: ...

    def as_posix(self) -> str: ...
    def as_uri(self) -> str: ...
    def is_absolute(self) -> bool: ...
    def is_relative_to(self, other: str | PurePath) -> bool: ...
    def is_reserved(self) -> bool: ...

    def joinpath(self, *other: str | PurePath) -> PurePath: ...
    def match(self, pattern: str) -> bool: ...
    def relative_to(self, other: str | PurePath) -> PurePath: ...
    def with_name(self, name: str) -> PurePath: ...
    def with_stem(self, stem: str) -> PurePath: ...
    def with_suffix(self, suffix: str) -> PurePath: ...

class PurePosixPath(PurePath):
    """Pure path for POSIX systems."""
    ...

class PureWindowsPath(PurePath):
    """Pure path for Windows systems."""
    ...

class Path(PurePath):
    """Concrete path for filesystem operations."""

    def __new__(cls, *args: str | PathLike, **kwargs: Any) -> Path: ...

    def cwd(self) -> Path:
        """Return current working directory."""
        ...

    def home(self) -> Path:
        """Return home directory."""
        ...

    def stat(self) -> Any:
        """Return stat info."""
        ...

    def exists(self) -> bool:
        """Check if path exists."""
        ...

    def is_dir(self) -> bool:
        """Check if path is a directory."""
        ...

    def is_file(self) -> bool:
        """Check if path is a file."""
        ...

    def is_symlink(self) -> bool:
        """Check if path is a symlink."""
        ...

    def is_mount(self) -> bool:
        """Check if path is a mount point."""
        ...

    def iterdir(self) -> Iterator[Path]:
        """Iterate over directory contents."""
        ...

    def glob(self, pattern: str) -> Iterator[Path]:
        """Glob pattern matching."""
        ...

    def rglob(self, pattern: str) -> Iterator[Path]:
        """Recursive glob pattern matching."""
        ...

    def absolute(self) -> Path:
        """Return absolute path."""
        ...

    def resolve(self, strict: bool = False) -> Path:
        """Resolve to absolute path."""
        ...

    def expanduser(self) -> Path:
        """Expand ~ in path."""
        ...

    def read_text(self, encoding: str | None = None, errors: str | None = None) -> str:
        """Read file as text."""
        ...

    def read_bytes(self) -> bytes:
        """Read file as bytes."""
        ...

    def write_text(self, data: str, encoding: str | None = None, errors: str | None = None) -> int:
        """Write text to file."""
        ...

    def write_bytes(self, data: bytes) -> int:
        """Write bytes to file."""
        ...

    def mkdir(self, mode: int = ..., parents: bool = False, exist_ok: bool = False) -> None:
        """Create directory."""
        ...

    def rmdir(self) -> None:
        """Remove directory."""
        ...

    def unlink(self, missing_ok: bool = False) -> None:
        """Remove file."""
        ...

    def rename(self, target: str | PurePath) -> Path:
        """Rename file or directory."""
        ...

    def replace(self, target: str | PurePath) -> Path:
        """Replace file or directory."""
        ...

    def symlink_to(self, target: str | Path, target_is_directory: bool = False) -> None:
        """Create symbolic link."""
        ...

    def touch(self, mode: int = ..., exist_ok: bool = True) -> None:
        """Create file or update timestamp."""
        ...

    def open(
        self,
        mode: str = "r",
        buffering: int = -1,
        encoding: str | None = None,
        errors: str | None = None,
        newline: str | None = None
    ) -> Any:
        """Open file."""
        ...

class PosixPath(Path, PurePosixPath):
    """Concrete path for POSIX systems."""
    ...

class WindowsPath(Path, PureWindowsPath):
    """Concrete path for Windows systems."""
    ...
