# Artifact Guide: Trace-Guided Synthesis of Effectful Test Generators

This is the artifact for the PLDI 2026 paper *Trace-Guided Synthesis of Effectful Test
Generators*. The tool is called **Clouseau** and is implemented in approximately 14K lines
of OCaml. It automatically synthesizes effectful test generators (schedulers) for
property-based testing, guided by traces that witness property violations.

The artifact supports reproduction of:
- **Table 1** — ADT & QCheck benchmarks (14 benchmarks)
- **Table 2** — P language benchmarks (11 benchmarks)

---

# 1. Quick Get Started

## 1.1 Requirements

- Docker version 20.10.23 or later, Docker Compose v2
- 8 GB RAM, 8 GB free disk space (recommended)
- Tested on: Linux, Intel Core i7, 64 GB RAM

## 1.2 Pull or Build the Docker Image

**Option A — Pull the pre-built image (recommended):**

```
$ docker pull clouseau2026/clouseau:pldi-2026
```

Or load from the provided archive:

```
$ docker load < clouseau2026-clouseau-pldi-2026.tar.gz
```

**Option B — Build locally (optional):**

```
$ docker build . --tag clouseau2026/clouseau:pldi-2026
```

> **Note:** Building requires compiling Z3 from source, which may need up to 32 GB of
> RAM. If the build fails due to memory pressure, use the pre-built image instead.

## 1.3 Start the Environment

We provide a script that starts the full environment — the three-node MariaDB Galera
cluster and the Clouseau container — and initializes the database in one step. Run it
from the repository root:

```
$ bash scripts/start.sh
```

The script performs the following steps automatically:
1. Starts Galera node 1 and waits until it is ready
2. Starts Galera nodes 2 & 3 and the Clouseau container
3. Runs `scripts/init_cluster.py` inside the container to warm up the cluster

Once it completes, open an interactive shell in the Clouseau container:

```
$ docker compose exec clouseau bash
```

Verify the tool works:

```
$ ./main.exe --help
```

**Shutting down:**

```
$ docker compose down -v
```

The `-v` flag removes the Galera data volumes for a clean restart.

---

> **Optional — Manual startup (if you prefer not to use the script):**
>
> **Step 1 — Start the first Galera node and wait for it to be ready:**
> ```
> $ docker compose up galera1 -d
> $ docker logs -f galera1
> ```
> Wait until you see: `WSREP: Synchronized with group, ready for connections`
>
> **Step 2 — Start the remaining nodes and the Clouseau container:**
> ```
> $ docker compose up galera2 galera3 clouseau -d
> ```
>
> **Step 3 — Open an interactive shell:**
> ```
> $ docker compose exec clouseau bash
> ```
>
> **Step 4 — Initialize the cluster (required once before any MonkeyDB benchmark run):**
> ```
> $ python3 scripts/init_cluster.py
> ```
>
> The Galera nodes listen on host ports `3307`, `3308`, and `3309` (user `root`,
> password `rootpass`). An Adminer web UI is optionally available via
> `docker compose up adminer -d` at `http://localhost:8080/`.

---

## 1.4 Pretty Printing

To display a synthesized generator in human-readable form:

```
$ ./main.exe show-term output/GOAL_NAME.scm
```

Example — after synthesizing the `stack` benchmark (see §2.3.1):

```
$ ./main.exe show-term output/stack.scm
```

This prints the synthesized Clouseau DSL program in a formatted, readable layout.

---

# 2. Step-by-Step Instructions

## 2.1 Artifact Structure

| Path | Description |
|------|-------------|
| `bin/main.ml` | Entry point |
| `synthesis/` | Core synthesis engine |
| `interpreter/` | Trace interpreter / runtime |
| `benchmarks/OCamlBench/` | ADT specs for Table 1 |
| `benchmarks/MonkeyDB/` | MonkeyDB (database) specs for Table 1 |
| `benchmarks/BackendMariaDB/` | MariaDB backend implementation |
| `benchmarks/PBench/` | P language specs for Table 2 |
| `penv/` | Synthesized P programs (output) |
| `poriginal/` | Baseline P programs (random) |
| `scripts/run_ocaml_bench.py` | Script to reproduce Table 1 |
| `scripts/run_p_bench.py` | Script to reproduce Table 2 |
| `scripts/start.sh` | One-shot environment startup script |
| `stat/` | Statistics output files (JSON) |
| `output/` | Synthesized generator files (`.scm`) |
| `synRuntime/` | Built-in automata and P language templates |
| `meta-config.json` | Tool configuration |

---

## 2.2 Comprehensive Scripts

All steps below are run from `/home/clouseau` inside the container.

### 2.2.1 Reproducing Table 1 (ADT & QCheck Benchmarks)

**Benchmarks:** Stack, HashTable, Filesystem, Graph, NFA, IFCStore, IFCAdd, IFCLoad,
DeBruijn1, DeBruijn2, Shopping, Courseware, Twitter, Smallbank

**Step 1 — Synthesis:**

```
$ python3 scripts/run_ocaml_bench.py syn
```

**Step 2 — Run synthesized generators (200 runs each):**

```
$ python3 scripts/run_ocaml_bench.py runsyn
```

**Step 3 — Run random (QCheck) baseline:**

```
$ python3 scripts/run_ocaml_bench.py runrandom
```

**Step 4 — Print Table 1 as LaTeX:**

```
$ python3 scripts/run_ocaml_bench.py table1
```

### 2.2.2 Reproducing Table 2 (P Language Benchmarks)

**Benchmarks:** Database, Firewall, RingLeaderElection, EspressoMachine, BankServer,
Simplified2PC, HeartBeat, ChainReplication, Paxos, Raft, Kermit2PCModel

**Step 1 — Synthesis + compile to P:**

```
$ python3 scripts/run_p_bench.py syn
```

**Step 2 — Run synthesized schedulers (500 runs each):**

```
$ python3 scripts/run_p_bench.py runsyn
```

**Step 3 — Run random baseline:**

```
$ python3 scripts/run_p_bench.py runrandom
```

**Step 4 — Print Table 2 as LaTeX:**

```
$ python3 scripts/run_p_bench.py table2
```

---

## 2.3 Detailed Steps

All commands accept an optional `-config PATH` flag to use an alternate configuration
file (default: `meta-config.json`).

### 2.3.1 Synthesis

Run the synthesizer on a single benchmark:

```
$ ./main.exe do-syn GOAL_NAME SPEC_FILE N
```

| Argument | Description |
|----------|-------------|
| `GOAL_NAME` | The synthesis goal name (must match the `[@goal]` annotation in the spec file) |
| `SPEC_FILE` | Path to the `.ml` spec file |
| `N` | Number of expected synthesis candidates |

Output is written to `output/GOAL_NAME.scm`.

Example:

```
$ ./main.exe do-syn stack benchmarks/OCamlBench/stack_spec.ml 1
```

### 2.3.2 Running the Synthesized Generator

Run the synthesized generator `N` times and report the bug-detection rate:

```
$ ./main.exe sample-syn GOAL_NAME N
```

Loads `output/GOAL_NAME.scm` and executes the synthesized test generator against the
system under test, reporting the fraction of runs that expose a violation.

Example:

```
$ ./main.exe sample-syn stack 200
```

### 2.3.3 Running the Random Baseline

Run the random (QCheck-style) baseline generator for `N` seconds of wall-clock time:

```
$ ./main.exe sample-random GOAL_NAME N
```

Example:

```
$ ./main.exe sample-random stack 200
```

### 2.3.4 Compiling a Synthesized Generator to P

After synthesis, compile the result to a P language scheduler:

```
$ ./main.exe compile-to-p TASK_NAME BENCH_NAME
```

| Argument | Description |
|----------|-------------|
| `TASK_NAME` | The task identifier, e.g. `p_database` |
| `BENCH_NAME` | The P benchmark directory name, e.g. `Database` |

The compiled P file is written to `penv/BENCH_NAME/PSyn/SynClient.p`.

Example:

```
$ ./main.exe compile-to-p p_database Database
```

---

## 2.4 Input File Formats

Each benchmark is specified in a single `.ml` file using Clouseau's OCaml-embedded DSL.
A spec file has three sections: basic typing declarations, uHAT specifications, and a
goal declaration.

### 2.4.1 Syntax

The core syntactic objects used in spec files are **Symbolic Regular Expressions
(SREs)**, which describe sets of event traces:

```
SRE  H, F, A  ::=
    allA                   -- (anything)*  matches any trace (including empty)
  | anyA                   -- matches any single event
  | ε                      -- empty trace
  | Op φ                   -- single event of type Op whose payload satisfies φ
  | A ; A                  -- sequence
  | A ∨ A                  -- union
  | A ∧ A                  -- intersection
  | starA A                -- Kleene star  (A*)
  | anyA - A               -- difference: any event not matching A
  | •                      -- wildcard event (used in ghost event position)
```

Payload qualifiers `φ` are Boolean expressions over the event's field names and
bound variables:

```
Qualifier  φ  ::=
    true | false
  | field == expr
  | field != expr
  | field < expr  |  field <= expr
  | φ && φ  |  φ || φ  |  not φ
```

A **uHAT** (Underapproximate Hoare Automata Type) for an operation has the form:

```
uHAT  ::=  ( H, Op φ, F )
```

where `H` is the history SRE (what must have happened before), `Op φ` is the current
event, and `F` is the future SRE (what must follow).

**Ghost variables** introduce existentially quantified witnesses that relate values
across events:

```
?l:(x = (true : [%v: τ]))    -- binds ghost variable x of type τ
```

### 2.4.2 Basic Typing Declarations

Declare the effectful API of the system under test. Each operation is annotated with
its role:

| Annotation | Role |
|------------|------|
| `[@@gen]` | Generator effect — the test generator actively invokes this operation |
| `[@@obs]` | Observation effect — a synchronous response received from the SUT |
| `[@@obsRecv]` | Asynchronous observation — used in P language benchmarks |

The payload type is an anonymous record `< field : type; ... >`. Example:

```ocaml
val pushReq  : < elem : int >  [@@gen]
val popReq   : < >             [@@gen]
val popResp  : < elem : int >  [@@obs]
val isEmptyResp : < isEmpty : bool > [@@obs]
```

### 2.4.3 uHAT Specifications

Each declared operation is given a uHAT specification:

```ocaml
(* pushReq: at any history, a PushReq carrying x requires
   a subsequent PopReq somewhere in the future *)
let pushReq ?l:(x = (true : [%v: int])) =
  ( allA,
    PushReq (elem == x),
    (allA; PopReq true; allA) )

(* popReq: at any history, a PopReq must eventually be answered *)
let popReq =
  ( allA,
    PopReq true,
    (PopResp true; allA) )

(* popResp: a PopResp can only occur after a sequence that has no
   intervening PushReq *)
let popResp = (allA, PopResp true, starA (anyA - PushReq true))
```

Ghost variables (`?l:(x = ...)`) allow the history `H` and future `F` of a uHAT to
share data values with the current event's payload.

For operations with multiple possible behaviors, list uHAT cases as an array:

```ocaml
let readReq =
  [|
    (fun (x : int) ->
      ( (allA; WriteReq (va == x); starA (anyA - WriteReq true)),
        ReadReq true,
        [| ReadRsp (va == x && st) |] ));
    (starA (anyA - WriteReq true), ReadReq true, [| ReadRsp (not st) |]);
  |]
```

### 2.4.4 Goal Declaration

The synthesis goal is an SRE describing the *bad trace* (the property violation to
expose), annotated with `[@goal]`:

```ocaml
let[@goal] stack (y : int) =
  allA;
  PushReq (elem == y);
  starA (anyA - PopResp (elem == y));
  IsEmptyResp (isEmpty == true)
```

This asserts that there exists a trace in which value `y` is pushed but then reported
missing by `isEmpty`. Clouseau synthesizes a generator whose executions are guaranteed
to produce such a trace whenever the SUT has the corresponding bug.

The goal takes typed parameters (here `y : int`) that are treated as existentially
quantified witnesses across the SRE.

### 2.4.5 P Language Spec Files

P benchmark specs follow the same structure but use `[@@obsRecv]` for asynchronous
responses, and write the future SRE as an array of alternatives `[| F1; F2; ... |]`
to model the nondeterministic responses a P state machine may return. Example
(`benchmarks/PBench/p_database_spec.ml`):

```ocaml
val readReq : < >                     [@@gen]
val readRsp : < va : int; st : bool > [@@obsRecv]
val writeReq : < va : int >           [@@gen]
val writeRsp : < va : int >           [@@obsRecv]

let readReq =
  [|
    (fun (x : int) ->
      ( (allA; WriteReq (va == x); starA (anyA - WriteReq true)),
        ReadReq true,
        [| ReadRsp (va == x && st) |] ));
    (starA (anyA - WriteReq true), ReadReq true, [| ReadRsp (not st) |]);
  |]

let[@goal] p_database (x : int) (y : int) =
  allA;
  WriteRsp (va == x);
  starA (anyA - WriteRsp true);
  ReadRsp (va == y && (not (x == y)) && st);
  allA
```

### 2.4.6 Output Files

Synthesized generators are serialized as S-expressions and written to
`output/GOAL_NAME.scm`. These files are consumed by `sample-syn`, `compile-to-p`, and
`show-term`. Statistics for each benchmark run are saved as JSON under `stat/`.
