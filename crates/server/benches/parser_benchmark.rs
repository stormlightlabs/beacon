use beacon_lsp::parser::LspParser;
use criterion::{BenchmarkId, Criterion, criterion_group, criterion_main};

const SMALL_FILE: &str = r#"
def add(x: int, y: int) -> int:
    return x + y

result = add(10, 20)
"#;

const MEDIUM_FILE: &str = r#"
from typing import TypeVar, Generic, Protocol, Callable

T = TypeVar('T')
U = TypeVar('U')

class Container(Generic[T]):
    def __init__(self, value: T):
        self.value = value

    def get(self) -> T:
        return self.value

    def map(self, f: Callable[[T], U]) -> 'Container[U]':
        return Container(f(self.value))

class Drawable(Protocol):
    def draw(self) -> str:
        ...

class Circle:
    def __init__(self, radius: float):
        self.radius = radius

    def draw(self) -> str:
        return f"Circle(radius={self.radius})"

def render(shape: Drawable) -> str:
    return shape.draw()

# Usage
container = Container(42)
doubled = container.map(lambda x: x * 2)
circle = Circle(5.0)
output = render(circle)
"#;

const LARGE_FILE: &str = r#"
"""
Large fixture file for parser benchmarking.
Tests parsing performance on realistic code with classes, functions, and type annotations.
"""

from typing import (
    TypeVar, Generic, Protocol, Callable, Optional, Union,
    List, Dict, Set, Tuple, Any, cast, overload
)
from abc import ABC, abstractmethod
from dataclasses import dataclass
from enum import Enum

T = TypeVar('T')
U = TypeVar('U')
V = TypeVar('V')
K = TypeVar('K')

class Status(Enum):
    """Status enumeration"""
    PENDING = "pending"
    ACTIVE = "active"
    COMPLETED = "completed"
    FAILED = "failed"

@dataclass
class User:
    """User data class"""
    id: int
    name: str
    email: str
    age: Optional[int] = None

    def validate(self) -> bool:
        return len(self.name) > 0 and "@" in self.email

class Repository(Generic[T], ABC):
    """Abstract repository pattern"""

    @abstractmethod
    def get(self, id: int) -> Optional[T]:
        """Get entity by ID"""
        pass

    @abstractmethod
    def save(self, entity: T) -> None:
        """Save entity"""
        pass

    @abstractmethod
    def delete(self, id: int) -> bool:
        """Delete entity by ID"""
        pass

    @abstractmethod
    def list(self, offset: int = 0, limit: int = 100) -> List[T]:
        """List entities with pagination"""
        pass

class InMemoryRepository(Repository[T]):
    """In-memory implementation of repository"""

    def __init__(self):
        self.storage: Dict[int, T] = {}
        self.next_id: int = 1

    def get(self, id: int) -> Optional[T]:
        return self.storage.get(id)

    def save(self, entity: T) -> None:
        if not hasattr(entity, 'id'):
            setattr(entity, 'id', self.next_id)
            self.next_id += 1
        self.storage[entity.id] = entity

    def delete(self, id: int) -> bool:
        if id in self.storage:
            del self.storage[id]
            return True
        return False

    def list(self, offset: int = 0, limit: int = 100) -> List[T]:
        items = list(self.storage.values())
        return items[offset:offset + limit]

class Service(Generic[T]):
    """Generic service layer"""

    def __init__(self, repository: Repository[T]):
        self.repository = repository

    def create(self, entity: T) -> T:
        self.repository.save(entity)
        return entity

    def update(self, id: int, entity: T) -> Optional[T]:
        existing = self.repository.get(id)
        if existing:
            self.repository.save(entity)
            return entity
        return None

    def remove(self, id: int) -> bool:
        return self.repository.delete(id)

    def find_all(self, page: int = 0, page_size: int = 10) -> List[T]:
        offset = page * page_size
        return self.repository.list(offset, page_size)

def map_list(f: Callable[[T], U], xs: List[T]) -> List[U]:
    """Functional map"""
    return [f(x) for x in xs]

def filter_list(predicate: Callable[[T], bool], xs: List[T]) -> List[T]:
    """Functional filter"""
    return [x for x in xs if predicate(x)]

def compose(f: Callable[[U], V], g: Callable[[T], U]) -> Callable[[T], V]:
    """Function composition"""
    def composed(x: T) -> V:
        return f(g(x))
    return composed

class Pipeline(Generic[T]):
    """Functional pipeline builder"""

    def __init__(self, initial: T):
        self.value = initial

    def then(self, f: Callable[[T], U]) -> 'Pipeline[U]':
        return Pipeline(f(self.value))

    def get(self) -> T:
        return self.value

# Usage examples
user_repo = InMemoryRepository[User]()
user_service = Service(user_repo)

user1 = User(1, "Alice", "alice@example.com", 30)
user2 = User(2, "Bob", "bob@example.com", 25)
user3 = User(3, "Charlie", "charlie@example.com")

user_service.create(user1)
user_service.create(user2)
user_service.create(user3)

all_users = user_service.find_all()
adults = filter_list(lambda u: u.age is not None and u.age >= 18, all_users)
names = map_list(lambda u: u.name, adults)

pipeline_result = (
    Pipeline(10)
    .then(lambda x: x * 2)
    .then(lambda x: x + 5)
    .then(lambda x: str(x))
    .get()
)
"#;

/// Benchmark parsing small files
fn bench_parse_small_file(c: &mut Criterion) {
    c.bench_function("parse_small_file", |b| {
        b.iter(|| {
            let mut parser = LspParser::new().unwrap();
            parser.parse(std::hint::black_box(SMALL_FILE))
        })
    });
}

/// Benchmark parsing medium files
fn bench_parse_medium_file(c: &mut Criterion) {
    c.bench_function("parse_medium_file", |b| {
        b.iter(|| {
            let mut parser = LspParser::new().unwrap();
            parser.parse(std::hint::black_box(MEDIUM_FILE))
        })
    });
}

/// Benchmark parsing large files
fn bench_parse_large_file(c: &mut Criterion) {
    c.bench_function("parse_large_file", |b| {
        b.iter(|| {
            let mut parser = LspParser::new().unwrap();
            parser.parse(std::hint::black_box(LARGE_FILE))
        })
    });
}

/// Benchmark AST construction for different file sizes
fn bench_ast_construction(c: &mut Criterion) {
    let mut group = c.benchmark_group("ast_construction");

    let files = vec![("small", SMALL_FILE), ("medium", MEDIUM_FILE), ("large", LARGE_FILE)];

    for (name, content) in files {
        group.bench_with_input(BenchmarkId::from_parameter(name), &content, |b, content| {
            b.iter(|| {
                let mut parser = LspParser::new().unwrap();
                let result = parser.parse(std::hint::black_box(content)).unwrap();
                std::hint::black_box(result.ast)
            })
        });
    }

    group.finish();
}

/// Benchmark incremental reparsing
fn bench_incremental_reparse(c: &mut Criterion) {
    let mut group = c.benchmark_group("incremental_reparse");
    let mut parser = LspParser::new().unwrap();

    let _initial_result = parser.parse(MEDIUM_FILE).unwrap();
    let modified = format!("{}\n\nresult2 = add(30, 40)\n", MEDIUM_FILE);

    group.bench_function("small_edit", |b| {
        b.iter(|| {
            let mut parser = LspParser::new().unwrap();
            parser.parse(std::hint::black_box(&modified))
        })
    });

    group.finish();
}

/// Benchmark symbol table generation
fn bench_symbol_table_generation(c: &mut Criterion) {
    let mut group = c.benchmark_group("symbol_table");

    let files = vec![("small", SMALL_FILE), ("medium", MEDIUM_FILE), ("large", LARGE_FILE)];

    for (name, content) in files {
        group.bench_with_input(BenchmarkId::from_parameter(name), &content, |b, content| {
            b.iter(|| {
                let mut parser = LspParser::new().unwrap();
                let result = parser.parse(std::hint::black_box(content)).unwrap();
                std::hint::black_box(result.symbol_table)
            })
        });
    }

    group.finish();
}

criterion_group!(
    benches,
    bench_parse_small_file,
    bench_parse_medium_file,
    bench_parse_large_file,
    bench_ast_construction,
    bench_incremental_reparse,
    bench_symbol_table_generation
);

criterion_main!(benches);
