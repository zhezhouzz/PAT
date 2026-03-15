# Artifact Guide: Trace-Guided Synthesis of Effectful Test Generators

This is the artifact for the PLDI 2026 paper *Trace-Guided Synthesis of Effectful Test
Generators*. The tool is called **Clouseau** and is implemented in approximately 14K lines
of OCaml. It automatically synthesizes effectful test generators (schedulers) for
property-based testing, guided by traces that witness property violations.

The artifact supports reproduction of:
- **Table 1** — ADT & QCheck benchmarks (14 benchmarks)
- **Table 2** — P language benchmarks (11 benchmarks)

---

## Requirements

**Hardware:** 8 GB RAM, 8 GB free disk space recommended. All benchmarks were tested on
Linux, Intel Core i7, 64 GB RAM.

**Software:** Docker version 20.10.23 or later, Docker Compose v2.

---

## Running the Docker Image

### Using the Pre-Built Image

Pull from Docker Hub:

```
$ docker pull clouseau2026/clouseau:pldi-2026
```

Or load from the provided archive:

```
$ docker load < clouseau2026-clouseau-pldi-2026.tar.gz
```

Start an interactive shell (ADT and P benchmarks only, no database required):

```
$ docker run -it -m="8g" clouseau2026/clouseau:pldi-2026
```

Verify the tool works:

```
$ ./main.exe --help
```

You should see the Clouseau help message listing all available commands.

### Building the Image Locally (Optional)

```
$ docker build . --tag clouseau2026/clouseau:pldi-2026
```

> **Note:** Building requires compiling Z3 from source via the `z3` opam package, which
> may require up to 32 GB of RAM. If the build fails due to memory pressure, use the
> pre-built image instead.

### Running with MariaDB (MonkeyDB Benchmarks)

The MonkeyDB benchmarks (Shopping, Courseware, Twitter, Smallbank) require a three-node
MariaDB Galera cluster. The `compose.yaml` at the root of this repository sets up the
full environment with a single command.

**Step 1 — Start the first Galera node and wait for it to be ready:**

```
$ docker compose up galera1 -d
$ docker logs -f galera1
```

Wait until you see:

```
WSREP: Synchronized with group, ready for connections
```

**Step 2 — Start the remaining nodes and the Clouseau container:**

```
$ docker compose up galera2 galera3 clouseau -d
```

**Step 3 — Open an interactive shell:**

```
$ docker compose exec clouseau bash
```

**Step 4 — Initialize the cluster (required once before first run):**

```
$ python3 scripts/init_cluster.py
```

**Shutting down:**

```
$ docker compose down -v
```

The `-v` flag removes Galera data volumes for a clean restart.

The Galera nodes listen on host ports `3307`, `3308`, `3309` (user `root`, password
`rootpass`). An Adminer web UI is optionally available:

```
$ docker compose up adminer -d
```

Then open `http://localhost:8080/` (server: `127.0.0.1:3307`).

---

## Pretty Printing

To display a previously synthesized generator in human-readable form:

```
$ ./main.exe show-term output/GOAL_NAME.scm
```

Example:

```
$ ./main.exe show-term output/stack.scm
```

This prints the synthesized Clouseau DSL program in a formatted, readable layout.

---

## Comprehensive Scripts

All steps below are run from `/home/clouseau` inside the container.

### Reproducing Table 1 (ADT & QCheck Benchmarks)

**Benchmarks:** Stack, HashTable, Filesystem, Graph, NFA, IFCStore, IFCAdd, IFCLoad,
DeBruijn1, DeBruijn2, Shopping, Courseware, Twitter, Smallbank

> **Note:** Shopping, Courseware, Twitter, and Smallbank are MonkeyDB benchmarks that
> require the MariaDB Galera cluster. Start the cluster and run the cluster
> initialization warm-up (see above) before running Steps 2–3 for those benchmarks.

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

**Step 4 — Display Table 1 as LaTeX:**

```
$ python3 scripts/run_ocaml_bench.py table1
```

### Reproducing Table 2 (P Language Benchmarks)

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

**Step 4 — Display Table 2 as LaTeX:**

```
$ python3 scripts/run_p_bench.py table2
```

---

## Detailed Usage

All commands take an optional `-config` flag to specify an alternate configuration file
(default: `meta-config.json`).

### Synthesis

Run the synthesizer on a single benchmark:

```
$ ./main.exe do-syn GOAL_NAME SPEC_FILE N
```

- `GOAL_NAME` — the name of the synthesis goal (must match the `[@goal]` annotation in
  the spec file)
- `SPEC_FILE` — path to the `.ml` spec file
- `N` — expected number of synthesis candidates

Output is written to `output/GOAL_NAME.scm`.

Example:

```
$ ./main.exe do-syn stack benchmarks/OCamlBench/stack_spec.ml 1
```

### Running the Synthesized Generator

Run the synthesized generator `N` times and report the bug-detection rate:

```
$ ./main.exe sample-syn GOAL_NAME N
```

Example (`stack`, 200 runs):

```
$ ./main.exe sample-syn stack 200
```

This loads `output/GOAL_NAME.scm` and executes the synthesized test generator against
the system under test, reporting the fraction of runs that expose a violation.

### Running the Random Baseline

Run the random (QCheck-style) baseline generator for `N` seconds of wall-clock time:

```
$ ./main.exe sample-random GOAL_NAME N
```

Example:

```
$ ./main.exe sample-random stack 200
```

### Compiling a Synthesized Generator to P

After synthesis, compile the resulting generator to a P language scheduler:

```
$ ./main.exe compile-to-p TASK_NAME BENCH_NAME
```

- `TASK_NAME` — the task identifier (e.g., `p_database`)
- `BENCH_NAME` — the P benchmark directory name (e.g., `Database`)

The compiled P file is written to `penv/BENCH_NAME/PSyn/SynClient.p`.

Example:

```
$ ./main.exe compile-to-p p_database Database
```

### MariaDB Isolation Level Tests

These commands test the MariaDB backend under various isolation levels and require the
Galera cluster to be running.

```
$ ./main.exe test-stuck ReadUncommitted
$ ./main.exe test-dirty-read ReadUncommitted
$ ./main.exe test-non-repeatable-read ReadUncommitted
$ ./main.exe test-causal Causal
$ ./main.exe test-db ReadCommitted
```

Supported isolation levels: `ReadUncommitted`, `ReadCommitted`, `Causal`, `Serializable`.

---

## Artifact Structure

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
| `stat/` | Statistics output files (JSON) |
| `output/` | Synthesized generator files (`.scm`) |
| `synRuntime/` | Built-in automata and P templates |
| `meta-config.json` | Tool configuration |

---

## Configuration

Clouseau is configured via `meta-config.json` at the repository root. An alternate
config file can be passed to any command with `-config PATH`.

```json
{
    "mode": ["Debug"],
    "max_printing_size": 300,
    "log_tags": ["eval", "result", "never-use"],
    "bool_options": [...],
    "prover_timeout_bound": 1999,
    "prim_path": { ... }
}
```

| Field | Type | Description |
|-------|------|-------------|
| `mode` | `"Debug"` or `"Release"` | Controls verbosity of internal output. `"Release"` suppresses most diagnostic printing. |
| `max_printing_size` | int | Maximum number of nodes to print in a symbolic regex or term before truncating. Useful for keeping output readable on large benchmarks. |
| `log_tags` | list of strings | Selects which diagnostic channels to enable. See below for the full list. |
| `bool_options` | list of `[name, bool]` pairs | Fine-grained feature flags for display and synthesis behavior. See below. |
| `prover_timeout_bound` | int (milliseconds) | Per-query timeout for Z3 SMT calls. Default `1999` ms. Increase this on slow machines if you observe premature `Timeout` results. |
| `prim_path` | object | Paths to built-in runtime files. These should not need to change unless you relocate `synRuntime/`. |

### Log Tags

Add any of the following strings to `log_tags` to enable the corresponding diagnostic
output:

| Tag | Description |
|-----|-------------|
| `"result"` | Print the final synthesis result and statistics |
| `"eval"` | Print interpreter evaluation steps |
| `"ntypecheck"` | Print normalization and type-checking steps |
| `"plan"` | Print the synthesis search plan |
| `"queries"` | Print each Z3 SMT query before it is dispatched |
| `"model"` | Print Z3 satisfying models |
| `"z3encode"` | Print the raw Z3 encoding of queries |
| `"parsing"` | Print the parsed AST of each spec file |
| `"desymbolic"` | Print the desymbolization of SREs |
| `"unification"` | Print unification steps during type inference |

The special tag `"never-use"` acts as a sentinel and has no effect; it can be left in
the list safely.

### Bool Options

| Option | Default | Description |
|--------|---------|-------------|
| `show_type_infer_pre_judgement` | `false` | Print pre-condition judgements during type inference |
| `show_type_infer_constant_judgement` | `false` | Print constant typing judgements |
| `show_type_infer_variable_judgement` | `false` | Print variable typing judgements |
| `show_var_type_in_prop` | `false` | Annotate variables with their types in printed propositions |
| `show_var_type_in_lit` | `false` | Annotate variables with their types in printed literals |
| `show_var_type_in_term` | `false` | Annotate variables with their types in printed terms |
| `show_record_type_feilds` | `false` | Print field types in record type annotations |
| `show_sevent_fds` | `false` | Print field names in symbolic event payloads |
| `pause_during_synthesis` | `false` | Pause and wait for a keypress between synthesis iterations (useful for interactive debugging) |
| `add_kstar_during_synthesis` | `false` | Allow the synthesizer to insert Kleene-star constructors during search |
| `if_sort_record` | `false` | Canonicalize record field order before printing |
| `instantiate_poly_type_var_in_smt` | `false` | Eagerly instantiate polymorphic type variables when encoding to SMT |

### Prim Path

| Field | Description |
|-------|-------------|
| `predefined_path` | Path to the built-in automata primitives file (`synRuntime/automata/builtin.s`) |
| `axioms_path` | Path to additional axioms (leave empty to use defaults) |
| `templates_path` | Path to additional synthesis templates (leave empty to use defaults) |
| `p_header_template_path` | Path to the P language header template used during `compile-to-p` |
| `p_client_template_path` | Path to the P language client template used during `compile-to-p` |

---

## Input File Formats

### Spec Files (`.ml`)

Each benchmark is specified in a single `.ml` file using Clouseau's OCaml-embedded DSL.
A spec file has three sections:

**1. Basic Typing Declarations**

Declare the effectful operations of the system under test. Each operation is annotated
with `[@@gen]` (a generator effect — the test generator actively invokes this) or
`[@@obs]` / `[@@obsRecv]` (an observation effect — the response received from the SUT).

```ocaml
val pushReq : < elem : int > [@@gen]
val popReq  : < >            [@@gen]
val popResp : < elem : int > [@@obs]
```

The payload type is written as an anonymous record `< field : type; ... >`. For P
language benchmarks, response events use `[@@obsRecv]` to indicate asynchronous
receive.

**2. uHAT Specifications**

Each operation declared above is given a uHAT (Underapproximate Hoare Automata Type)
specification. A uHAT has the form:

```
(H, op φ, F)
```

where:
- `H` is a **history SRE** — a symbolic regular expression constraining the trace of
  events that must have occurred *before* this operation executes. It describes the
  dependency context required for the operation to be meaningful.
- `op φ` is the **current event** with payload qualifier `φ`.
- `F` is a **future SRE** — constrains the trace of events that *must follow* this
  event. For generator effects this is typically `allA` (unconstrained); for
  asynchronous operations it specifies the required response event.

SRE primitives:
| Expression | Meaning |
|------------|---------|
| `allA` | Any trace (including empty) — `(anything)*` |
| `anyA` | Any single event |
| `starA e` | Kleene star: zero or more occurrences of `e` |
| `e1; e2` | Sequence: `e1` followed by `e2` |
| `anyA - e` | Any event except those matching `e` |
| `Op φ` | A single event of type `Op` whose payload satisfies qualifier `φ` |

Example — the `pushReq` uHAT says: at any history, a `PushReq` carrying value `x`
must be followed eventually by a `PopReq`, then anything:

```ocaml
let pushReq ?l:(x = (true : [%v: int])) =
  ( allA,
    PushReq (elem == x),
    (allA; PopReq true; allA) )
```

Ghost variables (written `?l:(x = (true : [%v: τ]))`) introduce existentially
quantified witnesses that allow the history and future SREs to share data values
across events.

**3. Goal Declaration**

The synthesis goal is a single SRE (the *safety property* to violate) annotated with
`[@goal]`:

```ocaml
let[@goal] stack (y : int) =
  allA;
  PushReq (elem == y);
  starA (anyA - PopResp (elem == y));
  IsEmptyResp (isEmpty == true)
```

This SRE describes the *bad trace* the synthesizer targets: a trace in which `y` is
pushed but later reported missing by `isEmpty`. The synthesizer produces a generator
whose executions are guaranteed to contain traces matching this SRE whenever the SUT
has the corresponding bug.

### P Language Spec Files

P benchmark specs follow the same structure but use `[@@obsRecv]` for asynchronous
responses and write the future SRE as an array of alternatives `[| F1; F2; ... |]`,
reflecting the nondeterministic responses a P machine may return:

```ocaml
val readReq : < >                   [@@gen]
val readRsp : < va : int; st : bool > [@@obsRecv]

let readReq =
  [|
    (fun (x : int) ->
      ( (allA; WriteReq (va == x); starA (anyA - WriteReq true)),
        ReadReq true,
        [| ReadRsp (va == x && st) |] ));
    (starA (anyA - WriteReq true), ReadReq true, [| ReadRsp (not st) |]);
  |]
```

The outer array lists multiple uHAT cases for the same operation — the synthesizer
treats these as a disjunction, choosing whichever case applies at runtime.

### Output Files

Synthesized generators are serialized as S-expressions and written to
`output/GOAL_NAME.scm`. These files are consumed by `sample-syn` and `compile-to-p`.
Statistics for each run are saved as JSON under `stat/`.
