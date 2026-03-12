# Artifact Guide: Trace-Guided Synthesis of Effectful Test Generators

This is the artifact for the PLDI 2026 paper *Trace-Guided Synthesis of Effectful Test
Generators*. The tool is called **Clouseau** and is implemented in approximately 14K lines
of OCaml. It automatically synthesizes effectful test generators (schedulers) for
property-based testing, guided by traces that witness property violations.

The artifact supports reproduction of:
- **Table 1** — ADT & QCheck benchmarks (14 benchmarks)
- **Table 2** — P language benchmarks (11 benchmarks)

---

## Getting Started Guide

**Recommended hardware:** 8 GB RAM, 8 GB free disk space.

**Requirements:** Docker version 20.10.23 or later, Docker Compose v2.

**Tested on:** Linux, Intel Core i7, 64 GB RAM.

### Using the Pre-Built Docker Image

Pull from Docker Hub:

```
$ docker pull clouseau2026/clouseau:pldi-2026
```

Or load from the provided archive:

```
$ docker load < clouseau:pldi-2026.tar.gz
```

### Building the Docker Image (Optional)

```
$ docker build . --tag clouseau2026/clouseau:pldi-2026
```

> **Note:** Building the image requires compiling Z3 from source via the `z3` opam package,
> which may require up to 32 GB of RAM on some systems. If the build fails due to memory
> pressure, increase Docker's memory limit or use the pre-built image instead.

### Running Clouseau (ADT benchmarks only)

For ADT and P language benchmarks (no database required):

```
$ docker run -it -m="8g" clouseau2026/clouseau:pldi-2026
```

Once inside the container, verify the tool works:

```
$ ./main.exe --help
```

You should see the Clouseau help message listing available commands.

### Running Clouseau with MariaDB (for MonkeyDB benchmarks)

The MonkeyDB benchmarks (Shopping, Courseware, Twitter, Smallbank) require a
three-node MariaDB Galera cluster. A `compose.yaml` at the root of this repository
sets up everything — the Galera cluster and the Clouseau container — with a single
command.

**Step 1 — Start the first Galera node and wait for it to be ready:**

```
$ docker compose up galera1 -d
$ docker logs -f galera1
```

Wait until you see a line like:

```
WSREP: Synchronized with group, ready for connections
```

**Step 2 — Start the remaining nodes and the Clouseau container:**

```
$ docker compose up galera2 galera3 clouseau -d
```

**Step 3 — Open an interactive shell in the Clouseau container:**

```
$ docker compose exec clouseau bash
```

The Galera nodes listen on host ports `3307`, `3308`, and `3309` (user `root`,
password `rootpass`). The Clouseau container uses `network_mode: host` so it can
reach them at `127.0.0.1` on those ports.

**Optional — web UI via Adminer:**

```
$ docker compose up adminer -d
```

Then open `http://localhost:8080/` in your browser (server: `127.0.0.1:3307`,
leave database blank or enter `mysql`).

**Shutting down:**

```
$ docker compose down -v
```

The `-v` flag removes the Galera data volumes for a clean restart.

---

## Step-by-Step Instructions

### Artifact Structure

Key directories and files:

| Path | Description |
|------|-------------|
| `bin/main.ml` | Entry point |
| `synthesis/` | Core synthesis engine |
| `interpreter/` | Trace interpreter / runtime |
| `benchmarks/ADT/` | ADT specs for Table 1 |
| `benchmarks/MonkeyDB/` | MonkeyDB (database) specs for Table 1 |
| `benchmarks/BackendMariaDB/` | MariaDB backend implementation |
| `benchmarks/Database/`, `benchmarks/Firewall/`, … | P language specs for Table 2 |
| `penv/` | Synthesized P programs (output) |
| `poriginal/` | Baseline P programs (random) |
| `script/run_adt.py` | Script to reproduce Table 1 |
| `script/run_bench.py` | Script to reproduce Table 2 |
| `stat/` | Statistics output files (JSON) |
| `meta-config.json` | Tool configuration |

---

### Reproducing Table 1 (ADT & QCheck Benchmarks)

**Benchmarks:** Stack, HashTable, Filesystem, Graph, NFA, IFCStore, IFCAdd, IFCLoad,
DeBruijn1, DeBruijn2, Shopping, Courseware, Twitter, Smallbank

> **Note:** Shopping, Courseware, Twitter, and Smallbank are MonkeyDB benchmarks that
> connect to the MariaDB Galera cluster during the `runsyn` step. Start the cluster
> (see above) before running Steps 2–3 for those benchmarks.

All steps are run from `/home/clouseau` inside the container.

**Step 1 — Synthesis (approximately X minutes):**

```
$ python3 script/run_adt.py syn
```

**Step 2 — Run synthesized generators (200 runs each):**

```
$ python3 script/run_adt.py runsyn
```

**Step 3 — Run random (QCheck) baseline:**

```
$ python3 script/run_adt.py runrandom
```

**Step 4 — Display Table 1 as LaTeX:**

```
$ python3 script/run_adt.py table1
```

---

### Reproducing Table 2 (P Language Benchmarks)

**Benchmarks:** Database, Firewall, RingLeaderElection, EspressoMachine, BankServer,
Simplified2PC, HeartBeat, ChainReplication, Paxos, Raft, Kermit2PCModel

**Step 1 — Synthesis + compile to P (approximately X minutes):**

```
$ python3 script/run_bench.py syn
```

**Step 2 — Run synthesized schedulers (500 runs each):**

```
$ python3 script/run_bench.py runsyn
```

**Step 3 — Run random baseline:**

```
$ python3 script/run_bench.py runrandom
```

**Step 4 — Display Table 2 as LaTeX:**

```
$ python3 script/run_bench.py table2
```

---

### Detailed Usage of Clouseau

**Synthesis — run the synthesizer on a single benchmark:**

```
$ ./main.exe do-syn GOAL_NAME SPEC_FILE
```

Example:

```
$ ./main.exe do-syn stack benchmarks/ADT/stack_spec.ml
```

Output is written to `output/GOAL_NAME.scm`.

**Run synthesized generator N times:**

```
$ ./main.exe sample-syn GOAL_NAME N
```

Example (`stack`, 100 runs):

```
$ ./main.exe sample-syn stack 100
```

**Run random (QCheck) generator N times:**

```
$ ./main.exe sample-random GOAL_NAME N
```

Example:

```
$ ./main.exe sample-random stack 100
```

**Compile synthesized generator to P:**

```
$ ./main.exe compile-to-p TASK_NAME BENCH_NAME
```

---

### MariaDB Isolation Level Tests

The MariaDB backend supports four isolation levels (weak → strong):
`ReadUncommitted`, `ReadCommitted`, `Causal` (via Galera), and `Serializable`.

Several standalone test programs are provided:

```
$ ./main.exe test-stuck ReadUncommitted
$ ./main.exe test-dirty-read ReadUncommitted
$ ./main.exe test-non-repeatable-read ReadUncommitted
$ ./main.exe test-causal Causal
$ ./main.exe test-db ReadCommitted
```

These require the MariaDB Galera cluster to be running (see above).

---

### Configuration of Clouseau (`meta-config.json`)

| Field | Description |
|-------|-------------|
| `mode` | `"Debug"` or `"Release"` |
| `log_tags` | Controls debug output (e.g., `result`, `eval`) |
| `bool_options` | Synthesis and display flags |
| `prover_timeout_bound` | Z3 timeout in milliseconds (default `1999`) |
| `prim_path` | Paths to built-in automata and P templates |
