# epp-codegen — AngelScript DevStructs code generator
# Run `just` (or `just --list`) to see available recipes.

# Default: show available recipes
default:
    @just --list

# Install the release binary into ~/.cargo/bin
install:
    cargo install --path .

# Install with debug symbols / release profile locked to this workspace
install-locked:
    cargo install --path . --locked

# Uninstall the installed binary
uninstall:
    cargo uninstall epp-codegen

# Debug build
build:
    cargo build

# Release build
release:
    cargo build --release

# Type-check without producing a binary
check:
    cargo check

# Run the golden-fixture test suite (byte-parity against tests/fixtures)
test:
    cargo test

# Generate DevStructs from an xtoml spec: `just gen spec-gamecamera.xtoml`
gen spec:
    cargo run --release -- "{{spec}}"

# Generate using the debug binary (faster to build): `just gen-debug spec-gamecamera.xtoml`
gen-debug spec:
    cargo run -- "{{spec}}"

# Regenerate all fixture outputs to stdout for inspection (does not overwrite .expected.as)
gen-all:
    #!/usr/bin/env bash
    set -euo pipefail
    cargo build --release
    find tests/fixtures -name '*.xtoml' | sort | while read -r f; do
        echo "==> $f"
        ./target/release/epp-codegen "$f"
    done

# Lint with clippy (warnings as errors)
lint:
    cargo clippy --all-targets -- -D warnings

# Format the code
fmt:
    cargo fmt

# Check formatting without modifying files
fmt-check:
    cargo fmt -- --check

# Remove build artifacts
clean:
    cargo clean
