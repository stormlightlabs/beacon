"""Operating system interfaces.

This stub provides OS-related functions and types."""

from typing import Any, Mapping, Sequence

# Environment variables
environ: dict[str, str]

# Path manipulation module
class path:
    """Path manipulation functions."""

    @staticmethod
    def join(path: str, *paths: str) -> str:
        """Join path components."""
        ...

    @staticmethod
    def split(path: str) -> tuple[str, str]:
        """Split path into directory and file."""
        ...

    @staticmethod
    def dirname(path: str) -> str:
        """Return directory name."""
        ...

    @staticmethod
    def basename(path: str) -> str:
        """Return base name."""
        ...

    @staticmethod
    def exists(path: str) -> bool:
        """Check if path exists."""
        ...

    @staticmethod
    def isfile(path: str) -> bool:
        """Check if path is a file."""
        ...

    @staticmethod
    def isdir(path: str) -> bool:
        """Check if path is a directory."""
        ...

    @staticmethod
    def abspath(path: str) -> str:
        """Return absolute path."""
        ...

    @staticmethod
    def relpath(path: str, start: str | None = None) -> str:
        """Return relative path."""
        ...

    @staticmethod
    def normpath(path: str) -> str:
        """Normalize path."""
        ...

    @staticmethod
    def expanduser(path: str) -> str:
        """Expand ~ in path."""
        ...

# File system operations
def getcwd() -> str:
    """Get current working directory."""
    ...

def chdir(path: str) -> None:
    """Change current working directory."""
    ...

def listdir(path: str = ".") -> list[str]:
    """List directory contents."""
    ...

def makedirs(name: str, mode: int = ..., exist_ok: bool = False) -> None:
    """Create directories recursively."""
    ...

def mkdir(path: str, mode: int = ...) -> None:
    """Create a directory."""
    ...

def remove(path: str) -> None:
    """Remove a file."""
    ...

def rmdir(path: str) -> None:
    """Remove a directory."""
    ...

def rename(src: str, dst: str) -> None:
    """Rename a file or directory."""
    ...

# Process operations
def getenv(key: str, default: str | None = None) -> str | None:
    """Get environment variable."""
    ...

def putenv(key: str, value: str) -> None:
    """Set environment variable."""
    ...

def system(command: str) -> int:
    """Execute command in system shell."""
    ...

# File descriptor operations
def open(path: str, flags: int, mode: int = ...) -> int:
    """Open a file descriptor."""
    ...

def close(fd: int) -> None:
    """Close a file descriptor."""
    ...

def read(fd: int, n: int) -> bytes:
    """Read from file descriptor."""
    ...

def write(fd: int, data: bytes) -> int:
    """Write to file descriptor."""
    ...

# Constants
O_RDONLY: int
O_WRONLY: int
O_RDWR: int
O_CREAT: int
O_TRUNC: int
O_APPEND: int

# Process information
def getpid() -> int:
    """Get process ID."""
    ...

def getuid() -> int:
    """Get user ID."""
    ...

def getgid() -> int:
    """Get group ID."""
    ...

# Stat result
class stat_result:
    """Result of stat operation."""
    st_mode: int
    st_ino: int
    st_dev: int
    st_nlink: int
    st_uid: int
    st_gid: int
    st_size: int
    st_atime: float
    st_mtime: float
    st_ctime: float

def stat(path: str) -> stat_result:
    """Get file status."""
    ...

# Walking directories
def walk(top: str, topdown: bool = True, onerror: Any = None, followlinks: bool = False) -> list[tuple[str, list[str], list[str]]]:
    """Walk directory tree."""
    ...
