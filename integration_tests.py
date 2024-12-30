import subprocess
import os

if not os.path.exists("./target/debug/tomaslang"):
    print("binary not found, please run `cargo build` first")
    exit(1)


# Parse the // EXPECT and //EXPECT ERR lines in integ test files
def find_expecteds(file):
    result = ["", ""]  # stdout, stderr
    for line in open(file, "r").readlines():
        if line.startswith("// EXPECT"):
            idx = 0
            latterpart = line.split("// EXPECT ")[1]
            if latterpart.startswith("ERR"):
                latterpart = latterpart.split("ERR ")[1]
                idx = 1
            result[idx] = latterpart.strip().replace("\\n", "\n")
    if result == ["", ""]:
        print("No expected output found in file " + file)
        exit(1)
    return result


def run_file(filename):
    result = subprocess.run(
        ["./target/debug/tomaslang", "./integtests/" + filename], stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
    stdout = result.stdout.strip()
    stderr = result.stderr.strip()
    return stdout, stderr


for filename in os.listdir("integtests"):
    expecteds = find_expecteds("integtests/" + filename)
    stdout, stderr = run_file(filename)
    for output_type, expected, result in zip(["stdout", "stderr"], expecteds, [stdout, stderr]):
        if expected != "" and result != expected:
            print(f"integ test {filename} \033[31mfailed\033[0m")
            print(f"Expected {output_type}:")
            print(expected)
            print(f"Got {output_type}:")
            print(result)
            break
    else:
        print(f"integ test {filename} \033[32mpassed\033[0m")
