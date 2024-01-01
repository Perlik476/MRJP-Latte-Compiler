
import os
import subprocess
from tqdm import tqdm

# latte_dir = "tests/lattests/good"
# latte_dir = "tests/lattests/extensions/arrays1"
latte_dir = "tests/lattests/extensions/struct"
# latte_dir = "tests/mrjp-tests-good/good"
# latte_dir = "tests/mrjp-tests-benek/gr5"
test_latte = "./latc_llvm"

errs = 0
all = 0
files = sorted([f for f in os.listdir(latte_dir) if f.endswith(".lat")])
for file in tqdm(files):
    result = subprocess.run([test_latte, os.path.join(latte_dir, file)], stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    if result.returncode != 0:
        print(f"Error processing {file}")
        errs += 1
    else:
        # if exists file with .input extension, use it as input
        if os.path.exists(os.path.join(latte_dir, file.replace(".lat", ".input"))):
            with open(os.path.join(latte_dir, file.replace(".lat", ".input")), "r") as f:
                result = subprocess.run(["./out.bc"], stdin=f, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        else:
            result = subprocess.run(["./out.bc"], stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        with open(os.path.join(latte_dir, file.replace(".lat", ".output")), "r") as f:
            expected = f.read()
        if result.stdout.decode("utf-8") != expected:
            print(f"Error processing {file}")
            print(f"Expected: {expected}")
            print(f"Got: {result.stdout.decode('utf-8')}")
            errs += 1
    
print(f"Errors: {errs} out of {len(files)} files")

print("All files processed successfully")
