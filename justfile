# Run tests (both unit and integration)
test:
  cargo build && cargo test --features debug_disassemble,debug_trace_execution && \
  python3 integration_tests.py && \
  cargo build --features debug_trace_execution,debug_disassemble > /dev/null 2>&1 && \
  python3 integration_tests.py --debugintegration

# Run unit tests
test-unit:
  cargo test

# Run integration tests
test-integration:
  python3 integration_tests.py

# Run the dev build (with debug info)
run:
  cargo run --features debug_trace_execution,debug_disassemble

# Run the release build (no debug info)
run-release:
  cargo run --release

runfile filename:
  cargo run --features debug_trace_execution,debug_disassemble -- {{filename}}

runfile-release filename:
  cargo run --release -- {{filename}}
