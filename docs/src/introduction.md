# Introduction

Beacon is an experimental Python type checker and developer experience platform written in Rust.
This documentation set describes the architecture, design decisions, and research that power the project. Whether you are contributing to the [codebase](https://github.com/stormlightlabs/beacon), evaluating the language server, or exploring the type system, start here to orient yourself.

## What You’ll Find

- **LSP Overview**: A deep dive into our Language Server Protocol implementation, including its goals, building blocks, and feature set.
- **Type System Research**: Summaries of the academic and practical references influencing Beacon’s approach to Hindley–Milner inference, gradual typing, and structural subtyping.
- **Contributor Guides (planned)**: Setup instructions, style guidelines, and workflows for building and testing Beacon.

## Project Vision

Beacon aims to combine precise type checking with interactive tooling that stays responsive for everyday Python development. The project embraces:

- Fast feedback loops enabled by incremental analysis.
- Interoperability with modern editors via LSP.
- A pragmatic blend of theoretical rigor and implementable engineering.

## Getting Started

1. **Clone the repository** and install Rust 1.70+ (stable).
2. Run `cargo check` from the workspace root to verify the build.
3. Launch the LSP server with `cargo run -p beacon-lsp` or integrate with an editor using the provided configuration (see the LSP chapter).
4. Browse the documentation sidebar for in-depth topics.

## Contributing

We welcome pull requests and discussions. To get involved:

- Review open issues
- Read the upcoming contributor guide (work in progress).
- Join the conversation in our community channels (details to be added).

Beacon is evolving quickly; expect iteration, experimentation, and plenty of opportunities to help shape the future of type checking for Python.
