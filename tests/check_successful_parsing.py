
import os
import subprocess
from tqdm import tqdm

# latte_dir = "tests/lattests/good"
latte_dir = "tests/mrjp-tests-temp/good"
test_latte = "./Frontend"

errs = 0
for file in tqdm([f for f in os.listdir(latte_dir) if f.endswith(".lat")]):
    result = subprocess.run([test_latte, os.path.join(latte_dir, file)], stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    if result.returncode != 0:
        print(f"Error processing {file}")
        errs += 1
        # exit(1)
print(f"Errors: {errs}")

print("All files processed successfully")
