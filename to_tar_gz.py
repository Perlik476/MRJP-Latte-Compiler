import tarfile


dirs = [
    "src",
    "lib",
]

files = [
    "Makefile",
    "README.md",
]

name = "latte-mateusz-perlik.tar.gz"

# pack files to tar.gz
with tarfile.open(name, "w:gz") as tar:
    for d in dirs:
        tar.add(d)
    for f in files:
        tar.add(f)