#!/usr/bin/env python3
"""
Start the Clouseau Docker cluster.

Usage:
    python3 scripts/start_cluster.py            # full stack (galera + clouseau + adminer)
    python3 scripts/start_cluster.py --db-only  # 3 MariaDB Galera nodes only (run repo locally)

Options:
    --db-only        Start only the 3 Galera nodes (no clouseau, no adminer)
    --no-adminer     Full stack minus adminer
    --compose FILE   Path to compose file (default: compose.yaml next to this script's repo root)
    --timeout SEC    Seconds to wait for galera1 to sync (default: 120)
"""

import argparse
import subprocess
import sys
import time
import os

READY_MARKER = "WSREP: Synchronized with group, ready for connections"


def repo_root():
    """Return the repository root (parent of this script's directory)."""
    return os.path.dirname(os.path.dirname(os.path.abspath(__file__)))


def compose_cmd(compose_file):
    return ["docker", "compose", "-f", compose_file]


def run(cmd, **kwargs):
    print(f"  $ {' '.join(cmd)}")
    subprocess.run(cmd, check=True, **kwargs)


def start_service(compose_file, *services):
    run(compose_cmd(compose_file) + ["up", "-d"] + list(services))


def wait_for_node(compose_file, service, timeout):
    """Stream service logs until the sync marker appears or timeout is reached."""
    print(f"\n[*] Waiting for {service} to sync (timeout={timeout}s) ...")
    print(f"    Looking for: '{READY_MARKER}'\n")

    deadline = time.time() + timeout
    proc = subprocess.Popen(
        compose_cmd(compose_file) + ["logs", "-f", service],
        stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT,
        text=True,
    )

    try:
        for line in proc.stdout:
            print(f"    {service} | {line}", end="")
            if READY_MARKER in line:
                print(f"\n[+] {service} is ready!")
                return True
            if time.time() > deadline:
                print(f"\n[!] Timed out waiting for {service} after {timeout}s.")
                return False
    finally:
        proc.terminate()
        proc.wait()

    return False


def main():
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--db-only", action="store_true",
                        help="Start only the 3 MariaDB Galera nodes (for running the repo locally)")
    parser.add_argument("--no-adminer", action="store_true",
                        help="Start full stack minus the Adminer web UI")
    parser.add_argument("--compose", metavar="FILE",
                        default=os.path.join(repo_root(), "compose.yaml"),
                        help="Path to the Docker Compose file (default: <repo-root>/compose.yaml)")
    parser.add_argument("--timeout", metavar="SEC", type=int, default=120,
                        help="Seconds to wait for galera1 to become ready (default: 120)")
    args = parser.parse_args()

    compose_file = args.compose
    if not os.path.isfile(compose_file):
        print(f"[!] Compose file not found: {compose_file}", file=sys.stderr)
        sys.exit(1)

    print(f"[*] Using compose file: {compose_file}")

    # Step 1 — bootstrap node
    print("\n[*] Starting galera1 (bootstrap node) ...")
    start_service(compose_file, "galera1")

    # Step 2 — wait for galera1
    ready = wait_for_node(compose_file, "galera1", args.timeout)
    if not ready:
        print("[!] galera1 did not reach ready state. Check logs with: docker logs galera1", file=sys.stderr)
        sys.exit(1)

    # Step 3 — remaining Galera nodes
    print("\n[*] Starting galera2 and galera3 ...")
    start_service(compose_file, "galera2", "galera3")

    for node in ("galera2", "galera3"):
        ready = wait_for_node(compose_file, node, args.timeout)
        if not ready:
            print(f"[!] {node} did not reach ready state. Check logs with: docker logs {node}", file=sys.stderr)
            sys.exit(1)

    if args.db_only:
        print("\n[+] DB-only mode: skipping clouseau and adminer.")
        print("    Galera cluster is up on ports 3307 (galera1), 3308 (galera2), 3309 (galera3).")
        print("    Connect with: mysql -h 127.0.0.1 -P 3307 -u root -prootpass")
        return

    # Step 4 — clouseau
    print("\n[*] Starting clouseau ...")
    start_service(compose_file, "clouseau")

    # Step 5 — adminer (optional)
    if not args.no_adminer:
        print("\n[*] Starting adminer ...")
        start_service(compose_file, "adminer")
        print("    Adminer UI: http://localhost:8080/  (server: 127.0.0.1:3307, user: root, pass: rootpass)")

    print("\n[+] Cluster is up. Open a shell in clouseau with:")
    print("    docker compose exec clouseau bash")
    print("\n    To shut down and remove volumes:")
    print("    docker compose down -v")


if __name__ == "__main__":
    main()
