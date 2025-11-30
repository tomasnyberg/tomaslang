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

# Interpret a file (with debug info)
runfile filename:
  cargo run --features debug_trace_execution,debug_disassemble -- {{filename}}

# Interpret a file (no debug info)
runfile-release filename:
  cargo run --release -- {{filename}}

# Build the release binary (put it in /usr/bin/cigg (!))
build-cigg:
  cargo build --release
  sudo cp ./target/release/tomaslang /usr/local/bin/cigg

# Build the debug binary (put it in /usr/bin/cigg-debug (!))
build-cigg-debug:
  cargo build --features debug_trace_execution,debug_disassemble
  sudo cp ./target/debug/tomaslang /usr/bin/cigg-debug
