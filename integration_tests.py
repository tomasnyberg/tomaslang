import subprocess
import os

if not os.path.exists("./target/debug/tomaslang"):
    print("binary not found, please run `cargo build` first")
    exit(1)

for filename in os.listdir("integtests"):
    with open("integtests/" + filename, "r") as file:
        expected_line = file.readline().strip()
        if not expected_line.startswith("// EXPECT"):
            print(f"integ test {filename} does not have an expected line")
            exit(1)
        expected = expected_line.split("// EXPECT ")[1]
        expected = expected.replace("\\n", "\n")
    result = subprocess.run(
        ["./target/debug/tomaslang", "./integtests/" + filename], stdout=subprocess.PIPE, text=True)
    result = result.stdout.strip()
    if result != expected:
        print(f"integ test {filename} failed")
        print("Expected:")
        print(expected)
        print("Got:")
        print(result)
        exit(1)
    else:
        print(f"integ test {filename} \033[32mpassed\033[0m")
