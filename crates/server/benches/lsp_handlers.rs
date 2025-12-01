use beacon_lsp::parser::LspParser;
use criterion::{Criterion, criterion_group, criterion_main};
use lsp_types::{
    CompletionParams, GotoDefinitionParams, HoverParams, PartialResultParams, Position, TextDocumentIdentifier,
    TextDocumentPositionParams, Url, WorkDoneProgressParams,
};
use std::str::FromStr;

const TEST_FILE: &str = r#"
from typing import Callable, TypeVar, Generic

T = TypeVar('T')
U = TypeVar('U')

class Container(Generic[T]):
    def __init__(self, value: T):
        self.value = value

    def get(self) -> T:
        return self.value

    def map(self, f: Callable[[T], U]) -> 'Container[U]':
        return Container(f(self.value))

def identity(x: T) -> T:
    return x

def compose(f: Callable[[U], T], g: Callable[[T], U]) -> Callable[[T], T]:
    def composed(x: T) -> T:
        return f(g(x))
    return composed

# Usage
container = Container(42)
doubled = container.map(lambda x: x * 2)
result = identity("hello")
"#;

fn create_test_uri() -> Url {
    Url::from_str("file:///test.py").unwrap()
}

fn create_position(line: u32, character: u32) -> Position {
    Position { line, character }
}

/// Benchmark hover handler performance
fn bench_hover_handler(c: &mut Criterion) {
    c.bench_function("hover_on_variable", |b| {
        b.iter(|| {
            let uri = create_test_uri();
            let params = HoverParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier { uri: uri.clone() },
                    position: create_position(26, 10),
                },
                work_done_progress_params: WorkDoneProgressParams::default(),
            };
            std::hint::black_box(params)
        })
    });

    c.bench_function("hover_on_function", |b| {
        b.iter(|| {
            let uri = create_test_uri();
            let params = HoverParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier { uri: uri.clone() },
                    position: create_position(18, 5),
                },
                work_done_progress_params: WorkDoneProgressParams::default(),
            };
            std::hint::black_box(params)
        })
    });
}

/// Benchmark completion handler performance
fn bench_completion_handler(c: &mut Criterion) {
    c.bench_function("completion_after_dot", |b| {
        b.iter(|| {
            let uri = create_test_uri();
            let params = CompletionParams {
                text_document_position: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier { uri: uri.clone() },
                    position: create_position(27, 14),
                },
                work_done_progress_params: WorkDoneProgressParams::default(),
                partial_result_params: PartialResultParams::default(),
                context: None,
            };
            std::hint::black_box(params)
        })
    });

    c.bench_function("completion_in_body", |b| {
        b.iter(|| {
            let uri = create_test_uri();
            let params = CompletionParams {
                text_document_position: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier { uri: uri.clone() },
                    position: create_position(12, 8),
                },
                work_done_progress_params: WorkDoneProgressParams::default(),
                partial_result_params: PartialResultParams::default(),
                context: None,
            };
            std::hint::black_box(params)
        })
    });
}

/// Benchmark goto definition handler performance
fn bench_goto_definition_handler(c: &mut Criterion) {
    c.bench_function("goto_definition_variable", |b| {
        b.iter(|| {
            let uri = create_test_uri();
            let params = GotoDefinitionParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier { uri: uri.clone() },
                    position: create_position(27, 10),
                },
                work_done_progress_params: WorkDoneProgressParams::default(),
                partial_result_params: PartialResultParams::default(),
            };
            std::hint::black_box(params)
        })
    });

    c.bench_function("goto_definition_function", |b| {
        b.iter(|| {
            let uri = create_test_uri();
            let params = GotoDefinitionParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier { uri: uri.clone() },
                    position: create_position(28, 10),
                },
                work_done_progress_params: WorkDoneProgressParams::default(),
                partial_result_params: PartialResultParams::default(),
            };
            std::hint::black_box(params)
        })
    });

    c.bench_function("goto_definition_class", |b| {
        b.iter(|| {
            let uri = create_test_uri();
            let params = GotoDefinitionParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier { uri: uri.clone() },
                    position: create_position(26, 12),
                },
                work_done_progress_params: WorkDoneProgressParams::default(),
                partial_result_params: PartialResultParams::default(),
            };
            std::hint::black_box(params)
        })
    });
}

/// Benchmark combined LSP operations
fn bench_combined_operations(c: &mut Criterion) {
    c.bench_function("parse_and_hover", |b| {
        b.iter(|| {
            let mut parser = LspParser::new().unwrap();
            let result = parser.parse(std::hint::black_box(TEST_FILE)).unwrap();
            let params = HoverParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier { uri: create_test_uri() },
                    position: create_position(26, 10),
                },
                work_done_progress_params: WorkDoneProgressParams::default(),
            };
            std::hint::black_box((result, params))
        })
    });
}

criterion_group!(
    benches,
    bench_hover_handler,
    bench_completion_handler,
    bench_goto_definition_handler,
    bench_combined_operations
);

criterion_main!(benches);
