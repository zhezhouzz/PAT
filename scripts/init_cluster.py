#!/usr/bin/env python3
"""
Cluster initialization script.

Run this once after starting the MariaDB Galera cluster and before
running any MonkeyDB benchmarks.

Usage:
    python3 scripts/init_cluster.py
"""

import subprocess
import sys
import os

def repo_root():
    return os.path.dirname(os.path.dirname(os.path.abspath(__file__)))

def main():
    root = repo_root()
    exe = os.path.join(root, "main.exe")
    if not os.path.isfile(exe):
        print(f"[!] Binary not found: {exe}", file=sys.stderr)
        print("    Build the project first with: dune build --profile release && cp _build/default/bin/main.exe main.exe", file=sys.stderr)
        sys.exit(1)

    print("[*] Initializing cluster ...")
    subprocess.run([exe, "sample-syn", "warmup", "1"], cwd=root,
                   stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    print("[+] Cluster initialization complete.")

if __name__ == "__main__":
    main()
