# Run tests
test:
  cargo test

# Run the dev build (with debug info)
run:
  cargo run --features debug_trace_execution

# Run the release build (no debug info)
run-release:
  cargo run --release
