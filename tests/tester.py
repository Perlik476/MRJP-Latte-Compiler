
import os
import subprocess
from tqdm import tqdm

test_latte = "./latc_llvm"
latte_dirs = [
    # "tests/lattests/good",
    # "tests/lattests/extensions/arrays1",
    # "tests/lattests/extensions/struct",
    "tests/mrjp-tests-good/good",
]

errs = 0
all = 0
files = [f"{d}/{f}" for d in latte_dirs for f in os.listdir(d) if f.endswith(".lat")]
files = sorted(files)
for file in tqdm(files):
    result = subprocess.run([test_latte, file], stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    if result.returncode != 0:
        print(f"Error processing {file}")
        errs += 1
    else:
        # if exists file with .input extension, use it as input
        if os.path.exists(file.replace(".lat", ".input")):
            with open(file.replace(".lat", ".input"), "r") as f:
                result = subprocess.run(["./out.bc"], stdin=f, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        else:
            result = subprocess.run(["./out.bc"], stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        with open(file.replace(".lat", ".output"), "r") as f:
            expected = f.read()
        if result.stdout.decode("utf-8") != expected:
            print(f"Error processing {file}")
            print(f"Expected: {expected}")
            print(f"Got: {result.stdout.decode('utf-8')}")
            errs += 1
    
print(f"Errors: {errs} out of {len(files)} files")

print("All files processed successfully")
