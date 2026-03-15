#!/usr/bin/env bash
# start.sh — Start the full Clouseau artifact environment (Galera cluster + Clouseau container)
# Run from the repository root: bash scripts/start.sh
set -e

echo "[*] Starting Galera node 1..."
docker compose up galera1 -d

echo "[*] Waiting for Galera node 1 to be ready (this may take ~30s)..."
until docker logs galera1 2>&1 | grep -q "Synchronized with group, ready for connections"; do
  sleep 5
  printf "    still waiting...\n"
done
echo "[+] Galera node 1 is ready."

echo "[*] Starting Galera nodes 2 & 3 and the Clouseau container..."
docker compose up galera2 galera3 clouseau -d

echo "[*] Waiting for containers to settle..."
sleep 8

echo "[*] Initializing the cluster (warmup run)..."
docker compose exec clouseau python3 scripts/init_cluster.py

echo ""
echo "[+] Environment is ready. Open a shell with:"
echo "    docker compose exec clouseau bash"
