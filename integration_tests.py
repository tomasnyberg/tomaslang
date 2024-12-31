import subprocess
import os
import sys

if not os.path.exists("./target/debug/tomaslang"):
    print("binary not found, please run `cargo build` first")
    exit(1)


def find_expecteds(file):  # Find the expected output from the file
    result = ["", ""]  # stdout, stderr
    multiline = False
    for line in open(file, "r").readlines():
        if multiline:
            if line.startswith("// EXPECT MULTI END"):
                result[0] = result[0].strip()
                multiline = False
                continue
            result[0] += line.rstrip() + "\n"
            continue
        if line.startswith("// EXPECT MULTI"):
            multiline = True
            continue
        if line.startswith("// EXPECT"):
            idx = 0
            latterpart = line.split("// EXPECT ")[1]
            if latterpart.startswith("ERR"):
                latterpart = latterpart.split("ERR ")[1]
                idx = 1
            result[idx] = latterpart.strip().replace("\\n", "\n")
    if multiline:
        print("Unterminated multiline expected output in file " + file)
        exit(1)
    if result == ["", ""]:
        print("No expected output found in file " + file)
        exit(1)
    return result


def run_file(filename):
    result = subprocess.run(
        ["./target/debug/tomaslang", filename], stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
    stdout = result.stdout.strip()
    stderr = result.stderr.strip()
    return stdout, stderr


def find_diff(expected, result):
    line = 0
    for c in range(min(len(expected), len(result))):
        if expected[c] == "\n":
            line += 1
        if expected[c] != result[c]:
            print("Diff at character", c, "Expected:",
                  repr(expected[c]), "Got:", repr(result[c]))
            print("Line:", line)
            return


def verify_output(expecteds, outs, filename):
    for expected, result, output_type in zip(expecteds, outs, ["stdout", "stderr"]):
        if expected != "" and result != expected:
            print(f"integ test {filename} \033[31mfailed\033[0m")
            print(f"Expected {output_type}:")
            print(expected)
            print(f"Got {output_type}:")
            print(result)
            find_diff(expected, result)
            break
    else:
        print(f"integ test {filename} \033[32mpassed\033[0m")


def normal_integration_tests():
    for filename in os.listdir("integtests"):
        if os.path.isdir("integtests/" + filename):
            continue
        expecteds = find_expecteds("integtests/" + filename)
        stdout, stderr = run_file("integtests/" + filename)
        verify_output(expecteds, [stdout, stderr], filename)


def debug_integration_tests():
    base = "integtests/debug/"
    for filename in os.listdir(base):
        expecteds = find_expecteds(base + filename)
        stdout, stderr = run_file(base + filename)
        verify_output(expecteds, [stdout, stderr], filename)


if __name__ == "__main__":
    if len(sys.argv) == 2 and sys.argv[1] == "--debugintegration":
        debug_integration_tests()
        exit(0)
    normal_integration_tests()
