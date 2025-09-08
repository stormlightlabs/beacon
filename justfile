# Beacon CLI Development Justfile

# Run all tests
test:
    cargo test --workspace

# Run tests with coverage report
coverage:
    cargo llvm-cov --all-features --workspace --html

# Run tests with lcov output for codecov
coverage-lcov:
    cargo llvm-cov --all-features --workspace --lcov --output-path lcov.info

# Format code
fmt:
    cargo fmt --all

# Lint code
lint:
    cargo clippy --all-targets --all-features -- -D warnings

# Build all packages
build:
    cargo build --workspace

# Build release
build-release:
    cargo build --release --workspace

# Clean build artifacts
clean:
    cargo clean

# Install cargo-llvm-cov for coverage
install-coverage:
    cargo install cargo-llvm-cov

# Run the CLI with example file
demo:
    cargo run --package beacon-cli -- highlight test.py

# Run parser tests specifically
test-parser:
    cargo test --package beacon-parser

# Run CLI tests specifically
test-cli:
    cargo test --package beacon-cli

# Check coverage percentage
coverage-check:
    cargo llvm-cov --all-features --workspace --summary-only

# Generate coverage in JSON format
coverage-json:
    cargo llvm-cov --all-features --workspace --json --output-path coverage.json

# Generate coverage in XML format
coverage-xml:
    cargo llvm-cov --all-features --workspace --cobertura --output-path coverage.xml
