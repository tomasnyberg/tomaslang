# Repository Guidelines

## Project Structure & Module Organization
Source lives in `src/` with the REPL entry in `src/main.rs`, the bytecode VM in `src/vm.rs`, the compiler and scanner in `src/compiler.rs` and `src/scanner.rs`, and chunk/bytecode helpers in `src/chunk.rs`. Inline unit tests sit at the bottom of those files. End-to-end language fixtures are in `integtests/` (primary) and `integtests/debug/` (debug-flavored); each `.cigg` file embeds `// EXPECT` comments that define expected stdout/stderr. `integration_tests.py` drives those fixtures against the built interpreter. Feature notes live in `features.md`.

## Build, Test, and Development Commands
- `just -l` shows all tasks; prefer `just` targets over memorizing cargo flags.
- `just run` builds and starts the debug REPL with tracing features enabled; `just run-release` runs the optimized build.
- `just test` runs unit + integration coverage (builds twice with debug flags, then executes `integration_tests.py` and its debug variant).
- `just test-unit` / `just test-integration` for narrower cycles. Integration tests assume `./target/debug/tomaslang` exists; run `cargo build` once if missing.
- `just build-cigg` installs a release binary to `/usr/local/bin/cigg` (uses `sudo`); keep this for local installs, not CI.

## Coding Style & Naming Conventions
Rust 2021 defaults: 4-space indents, `rustfmt` before sending changes. Modules and functions are `snake_case`; types/enums/traits are `PascalCase`; constants `SCREAMING_SNAKE_CASE`. Keep modules cohesive (scanner/token logic in `scanner.rs`, bytecode/runtime state in `vm.rs`, compiler stages in `compiler.rs`). Favor small helpers over long functions and add succinct comments when behavior is non-obvious.

## Testing Guidelines
Always prefer integration tests over unit tests as they are very cheap. Integration fixtures must declare outputs with `// EXPECT <stdout>` or `// EXPECT ERR <stderr>`; for multiline stdout, wrap expectations between `// EXPECT MULTI` and `// EXPECT MULTI END`. After adding fixtures, run `just test-integration` (or `python3 integration_tests.py`) and ensure both normal and debug suites pass when invoking `just test`.

