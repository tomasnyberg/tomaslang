# Run tests (both unit and integration)
test:
  cargo test && python3 integration_tests.py

# Run unit tests
test-unit:
  cargo test

# Run integration tests
test-integration:
  python3 integration_tests.py

# Run the dev build (with debug info)
run:
  cargo run --features debug_trace_execution

# Run the release build (no debug info)
run-release:
  cargo run --release
