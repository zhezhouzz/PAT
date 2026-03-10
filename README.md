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

**Requirements:** Docker version 20.10.23 or later.

**Tested on:** Linux, Intel Core i7, 64 GB RAM.

### Using the Pre-Built Docker Image

Pull from Docker Hub:

```
$ docker pull clouseau26/clouseau:pldi-2026
```

Or load from the provided archive:

```
$ docker load < clouseau:pldi-2026.tar.gz
```

### Building the Docker Image (Optional)

```
$ docker build . --tag clouseau26/clouseau:pldi-2026
```

> **Note:** Building the image requires compiling Z3 from source via the `z3` opam package,
> which may require up to 32 GB of RAM on some systems. If the build fails due to memory
> pressure, increase Docker's memory limit or use the pre-built image instead.

### Running the Docker Image

```
$ docker run -it -m="8g" clouseau26/clouseau:pldi-2026
```

Once inside the container, build the tool (if not already built) and verify it works:

```
$ dune build --profile release && cp _build/default/bin/main.exe main.exe
$ ./main.exe --help
```

You should see the Clouseau help message listing available commands.

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

All steps are run from `/home/clouseau` inside the container.

**Step 1 — Synthesis (approximately X minutes):**

```
$ python3 script/run_adt.py syn
```

**Step 2 — Run synthesized generators (500 runs each):**

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
$ dune exec -- bin/main.exe do-syn GOAL_NAME SPEC_FILE
```

Example:

```
$ dune exec -- bin/main.exe do-syn stack benchmarks/ADT/stack_spec.ml
```

Output is written to `output/GOAL_NAME.scm`.

**Run synthesized generator N times:**

```
$ dune exec -- bin/main.exe test-eval GOAL_NAME N
```

Example (`stack`, 100 runs):

```
$ dune exec -- bin/main.exe test-eval stack 100
```

**Run random (QCheck) generator N times:**

```
$ dune exec -- bin/main.exe test-random GOAL_NAME N
```

Example:

```
$ dune exec -- bin/main.exe test-random stack 100
```

**Compile synthesized generator to P:**

```
$ dune exec -- bin/main.exe compile-to-p TASK_NAME BENCH_NAME
```

---

### Configuration of Clouseau (`meta-config.json`)

| Field | Description |
|-------|-------------|
| `mode` | `"Debug"` or `"Release"` |
| `log_tags` | Controls debug output (e.g., `result`, `eval`) |
| `bool_options` | Synthesis and display flags |
| `prover_timeout_bound` | Z3 timeout in milliseconds (default `1999`) |
| `prim_path` | Paths to built-in automata and P templates |
