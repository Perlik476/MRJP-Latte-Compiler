
import os
import subprocess
from tqdm import tqdm

test_latte = "./latc_llvm"
latte_dirs = [
    "tests/lattests/good",
    "tests/lattests/extensions/arrays1",
    "tests/lattests/extensions/struct",
    "tests/mrjp-tests-good/good",
    "tests/my-tests"
]
latte_dirs_bad = [
    "tests/lattests/bad",
    "tests/mrjp-tests/bad"
]


errs = 0
files = [f"{d}/{f}" for d in latte_dirs for f in os.listdir(d) if f.endswith(".lat")]
files = sorted(files)
for file in tqdm(files):
    result = subprocess.run([test_latte, file], stdout=subprocess.DEVNULL, stderr=subprocess.PIPE)
    if result.returncode != 0:
        print(f"Error compiling {file}")
        print(result.stderr.decode("utf-8"))
        errs += 1
        continue

    os.chmod(file.replace(".lat", ".bc"), 0o777)
    if os.path.exists(file.replace(".lat", ".input")):
        with open(file.replace(".lat", ".input"), "r") as f:
            result = subprocess.run([file.replace(".lat", ".bc")], stdin=f, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    else:
        result = subprocess.run([file.replace(".lat", ".bc")], stdout=subprocess.PIPE, stderr=subprocess.PIPE)

    if os.path.exists(file.replace(".lat", ".exit_code")):
        with open(file.replace(".lat", ".exit_code"), "r") as f:
            exit_code_expected = int(f.read())
        if result.returncode != exit_code_expected:
            print(f"Error processing {file}, expected exit code: {exit_code_expected}, got: {result.returncode}")
            errs += 1
            continue
    elif result.returncode != 0:
        print(f"Error processing {file}, expected exit code: 0, got: {result.returncode}")
        errs += 1
    else:
        with open(file.replace(".lat", ".output"), "r") as f:
            expected = f.read()
        if result.stdout.decode("utf-8") != expected:
            print(f"Error processing {file}")
            # print the result of diff on the output and expected
            output = result.stdout.decode("utf-8")
            diff = subprocess.run(["diff", "-u", file.replace(".lat", ".output"), "-"], input=output.encode("utf-8"), stdout=subprocess.PIPE)
            print(diff.stdout.decode("utf-8"))
            errs += 1

    os.remove(file.replace(".lat", ".ll"))
    os.remove(file.replace(".lat", ".bc"))
    
print(f"Errors: {errs} out of {len(files)} files")
max_errors = 73
if errs > max_errors:
    print(f"Too many errors, max: {max_errors}")
    exit(1)

    
errs = 0
files = [f"{d}/{f}" for d in latte_dirs_bad for f in os.listdir(d) if f.endswith(".lat")]
files = sorted(files)
for file in tqdm(files):
    result = subprocess.run([test_latte, file], stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    if result.returncode == 0:
        print(f"File {file} compiled successfully, expected error")
        errs += 1
        try:
            os.remove(file.replace(".lat", ".ll"))
            os.remove(file.replace(".lat", ".bc"))
        except:
            pass

print(f"Errors: {errs} out of {len(files)} files")