# Artifact Guide: Trace-Guided Synthesis of Effectful Test Generators

This is the artifact for the PLDI 2026 paper *Trace-Guided Synthesis of Effectful Test
Generators*. The tool is called **Clouseau** and is implemented in approximately 10K lines
of OCaml. It automatically synthesizes effectful test generators (schedulers) for
property-based testing, guided by traces that witness property violations.

The artifact supports reproduction of:
- **Table 1** — Using Clouseau to synthesize effectful test generators in OCaml. (16 benchmarks)
- **Table 2** — P language benchmarks (10 benchmarks)

---

# 1. Quick Get Started

We recommend machines have at least 8 GB of memory and 8 GB of hard disk space available when building and running Docker images. All benchmarks were tested on a Linux machine having Intel i7-8700 CPU @ 3.20GHz with 64GB of RAM. The estimated execution time in the rest of the document also fits this setting.

## 1.1 Requirements

This artifact is built as a Docker image. Before proceeding, ensure
Docker is installed. (On *nix, `sudo docker run hello-world` will test
your installation.) If Docker is not installed, install it via the
[official installation guide](https://docs.docker.com/get-docker/). This guide was tested using Docker version `27.5.1`, but any contemporary Docker version is expected to work.

The MonkeyDB benchmarks use a three-node MariaDB cluster organized via [Galera](https://galeracluster.com/). Before starting the environment, pull the MariaDB Galera image: `docker pull bitnamilegacy/mariadb-galera:latest`. The original `bitnami/mariadb-galera` image is no longer maintained by Bitnami; we use the legacy image `bitnamilegacy/mariadb-galera` instead.

Docker Compose is required for running the MariaDB cluster. If you installed Docker Desktop (Windows, macOS), Docker Compose is included by default. On Linux, install it via your package manager (e.g., `apt install docker-compose-plugin` on Debian/Ubuntu) or follow the [official Compose installation guide](https://docs.docker.com/compose/install/). Verify with `docker compose version`.

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

> **Note:** Although our tool doesn't have large memory usage, building the docker image needs more than `32GB` RAM available. This memory usage requirement comes from the installation of the SMT solver `z3` (https://github.com/Z3Prover/z3). When the RAM limit of Docker (by default, it is `8GB` on Mac, no limit on Linux machine) is lower than `32GB`, the installation of `z3` will be killed and the `docker build` will fail.
> The memory error can be fixed by increasing the RAM limit in Docker; you can find instructions for doing so on Mac here: (https://docs.docker.com/desktop/settings/mac/#resources), for Windows here: (https://docs.docker.com/desktop/settings/windows/#resources), and for Linux here: (https://docs.docker.com/desktop/settings/linux/#resources). The pre-built docker image is built on a Linux machine having Intel i7-8700 CPU @ 3.20GHz with `64GB` of RAM, it took `30` min to build.


## 1.3 Start the Environment

We provide a script that starts the full environment — the three-node MariaDB Galera
cluster and the Clouseau container — and initializes the database in one step. Run it
from the repository root:

```
$ bash scripts/start_cluster.sh
```

The script performs the following steps automatically:
1. Starts Galera node 1 and waits until it is ready
2. Starts Galera nodes 2 & 3 and the Clouseau container
3. Runs `scripts/init_cluster.py` inside the container to warm up the cluster

Once it completes, verify the tool works:

```
$ docker compose exec clouseau ./main.exe --help
```

> **Tip:** For an interactive session inside the container, run:
> ```
> $ docker compose exec clouseau bash
> ```

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
> $ docker compose exec clouseau python3 scripts/init_cluster.py
> ```
>
> The Galera nodes listen on host ports `3307`, `3308`, and `3309` (user `root`,
> password `rootpass`). An Adminer web UI is optionally available via
> `docker compose up adminer -d` at `http://localhost:8080/`.

---

## 1.4 Pretty Printing

As another way to verify the tool operating successfull, the following command pretty prints the content of given files, which may contains the definition of pure and effectful operators, axioms, refinement types and global properties (the syntax of our input file can be found in [§3.2 Input File Formats](#32-input-file-formats)).:

```
$ docker compose exec clouseau ./main.exe show-spec benchmarks/OCamlBench/stack_spec.ml
```

The command will print the following specifications:

```
val ==: 'a . 'a -> 'a -> bool
gen pushReq: _record
gen initStackReq: _record
gen popReq: _record
obs popResp: _record
gen isEmptyReq: _record
obs isEmptyResp: _record
rty pushReq:
  (x:int) → [.*][⟨pushReq | elem == x⟩][.*•⟨popReq⟩•.*]
rty initStackReq:
  [.*][⟨initStackReq⟩][.*]
rty popReq:
  [.*][⟨popReq⟩][⟨popResp⟩•.*]
rty popResp:
  [.*][⟨popResp⟩][(.\⟨pushReq⟩)*]
rty isEmptyReq:
  [.*][⟨isEmptyReq⟩][⟨isEmptyResp⟩•.*]
rty isEmptyResp:
  (z:bool) → [.*][⟨isEmptyResp | isEmpty == z⟩][.*]
goal:
  stack: ∃(y: int)..*•⟨pushReq | elem == y⟩•(.\⟨popResp | elem == y⟩)*•⟨isEmptyResp | isEmpty == true⟩
```

---

# 2. Step-by-Step Instructions

In this section, we provide the instructions to evaluate our artifact.

## 2.1 Artifact Structure

This sub-section gives a brief overview of the files in this artifact.

| Path | Description |
|------|-------------|
| `bin/main.ml` | Entry point |
| `synthesis/` | Core synthesis engine |
| `interpreter/` | Trace interpreter / runtime |
| `benchmarks/OCamlBench/` | Ocaml related specs for Table 1 |
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

## 2.2 Running Benchmarks of Clouseau

In this section, we discuss the scripts that display the tables in the evaluation section of the paper.

### 2.2.1 Comprehensive Scripts to Reproducing Table 1 (QCheck and MonkeyDB Benchmarks)

**Benchmarks:** Stack, HashTable, Filesystem, Graph, NFA, IFCStore, IFCAdd, IFCLoad,
DeBruijn1, DeBruijn2, Shopping, Courseware, Twitter, Smallbank

```
$ docker compose exec clouseau python3 scripts/run_ocaml_bench.py
```

This runs synthesis, then samples synthesized generators, then
runs the random (QCheck) baseline, and finally prints
Table 1 as LaTeX.

**Common flags:**

| Flag | Default | Description |
|------|---------|-------------|
| `-t T` | — | Override time limit in seconds for both `runsyn` and `runrandom`, default is `1800` seconds |
| `-b B1,B2,...` | all | Run a subset of benchmarks by name |

### 2.2.2 Comprehensive Scripts to Reproducing Table 2 (P Language Benchmarks)

**Benchmarks:** Database, Firewall, RingLeaderElection, BankServer, Simplified2PC,
HeartBeat, ChainReplication, Paxos, Raft, AnonReadAtomicity

```
$ docker compose exec clouseau python3 scripts/run_p_bench.py
```

This runs synthesis, compiles to P, runs synthesized schedulers,
runs the random and manually-written baselines, and prints Table 2 as LaTeX.

**Common flags:** same as Table 1 above (`-t`, `-b`).

---

## 2.3 Detailed Steps

The comprehensive scripts above can also be invoked step-by-step using subcommands.
Each step description below shows both the script command and the underlying
`main.exe` calls it performs.

All `main.exe` commands accept an optional `-config PATH` flag to use an alternate
configuration file (default: `meta-config.json`).

### 2.3.1 Table 1 Step-by-Step

**Step 1 — Synthesis**

```
$ docker compose exec clouseau python3 scripts/run_ocaml_bench.py syn
```

For each benchmark, this runs:

```
$ docker compose exec clouseau ./main.exe do-syn GOAL_NAME SPEC_FILE NUM_CANDIDATE
```

Output is written to `output/GOAL_NAME.scm`. Example for a single benchmark:

```
$ docker compose exec clouseau ./main.exe do-syn stack benchmarks/OCamlBench/stack_spec.ml 1
```

**Step 2 — Run synthesized generators**

```
$ docker compose exec clouseau python3 scripts/run_ocaml_bench.py runsyn
```

For each benchmark, this runs:

```
$ docker compose exec clouseau ./main.exe sample-syn GOAL_NAME COUNT TIME_SEC
```

Where `COUNT` is the sample count (0 = no limit) and `TIME_SEC` is the time bound in
seconds (0 = no limit). Use `-n N` to override the count, `-t T` to override the
time limit.

**Step 3 — Run random (QCheck) baseline**

```
$ docker compose exec clouseau python3 scripts/run_ocaml_bench.py runrandom
```

For each benchmark, this runs:

```
$ docker compose exec clouseau ./main.exe sample-random GOAL_NAME COUNT TIME_SEC
```

The default time limit for the random baseline is 1800 seconds per benchmark. Use
`-t T` to override.

**Step 4 — Print Table 1 as LaTeX**

```
$ docker compose exec clouseau python3 scripts/run_ocaml_bench.py table1
```

### 2.3.2 Table 2 Step-by-Step

**Step 1 — Synthesis + compile to P**

```
$ docker compose exec clouseau python3 scripts/run_p_bench.py syn
```

For each benchmark, this first synthesizes:

```
$ docker compose exec clouseau ./main.exe do-syn GOAL_NAME SPEC_FILE NUM_CANDIDATE
```

Then compiles the result to a P scheduler:

```
$ docker compose exec clouseau ./main.exe compile-to-p GOAL_NAME BENCHNAME
```

The compiled P file is written to `penv/BENCHNAME/PSyn/SynClient.p`.

**Step 2 — Run synthesized schedulers**

```
$ docker compose exec clouseau python3 scripts/run_p_bench.py runsyn
```

Runs each P benchmark under `penv/BENCHNAME/` using the synthesized scheduler.

**Step 3 — Run random P baseline**

```
$ docker compose exec clouseau python3 scripts/run_p_bench.py runrandom
```

Runs each P benchmark under `poriginal/BENCHNAME/` using the original random
scheduler. Used as the baseline for benchmarks *without* a manually-written
scheduler. 

**Step 4 — Run manual P baseline**

```
$ docker compose exec clouseau python3 scripts/run_p_bench.py rundefault
```

Runs each P benchmark under `poriginal/BENCHNAME/` using the manually-written
default scheduler (mode `Manual`). Used as the baseline for:
EspressoMachine, BankServer, Simplified2PC, HeartBeat, ChainReplication, Paxos,
AnonReadAtomicity.

Both Step 3 and Step 4 must be run before printing Table 2, since each benchmark
uses whichever baseline is applicable.

**Step 5 — Print Table 2 as LaTeX**

```
$ docker compose exec clouseau python3 scripts/run_p_bench.py table2
```

---

# 3. Configuration of Clouseau

All commands of **Clouseau** take a universal configuration file (`meta-config.json`) in JSON format. The main fields are:

- **`max_printing_size`** — maximum size for printed output (default: 300).
- **`log_tags`** — list of enabled log tags. Uncomment a tag to show its debug output. Examples:
  - `eval`, `eval_io` — interpreter evaluation traces
  - `ntypecheck` — type checking
  - `synthesis`, `refine`, `compile`, `simp`, `recursion` — synthesis pipeline
  - `qc`, `DB`, `pbackend` — benchmark-related output
  - `result` — synthesis/check results
- **`bool_options`** — toggles for detailed output:
  - `show_type_infer_*_judgement` — typing judgements
  - `show_var_type_in_prop/lit/term` — variable types in formulas
  - `pause_during_synthesis`, `add_kstar_during_synthesis` — synthesis behavior
- **`prover_timeout_bound`** — SMT/Z3 timeout in seconds.

## 3.1 Input File Formats

Each benchmark is specified in a single `.ml` file using Clouseau's OCaml-embedded DSL
(files are conventionally named `*_spec.ml`). The DSL reuses the OCaml parser and maps
OCaml constructs to internal Clouseau AST nodes. A spec file has five sections:

1. **Pure Operators** — uninterpreted functions and axioms used in logic formulas
2. **Effectful Operations** — declarations of generator requests and system observations
3. **Refinement Types & uHAT Specifications** — behavioral specs for each operation
4. **Global Property (Goal)** — the bad trace the synthesizer targets

### 3.1.1 Pure Operators and Axioms

Pure operators are uninterpreted functions used in payload qualifiers. Declare them
with a `val` binding:

```ocaml
val ( == ) : 'a. 'a -> 'a -> bool
val is_root : Path.t -> bool
val parent  : Path.t -> Path.t
```

Axioms state First-Order Logic facts about these operators. The sugared form
universally quantifies over the named argument:

```ocaml
let[@axiom] isFileOrDir (b : Byte.t) = isFile b || isDir b
let[@axiom] isDirNotFile (b : Byte.t) = not (isFile b && isDir b)
let[@axiom] parent_is_root (p : Path.t) = iff (p == parent p) (is_root p)
```

The desugared form uses `[@forall]`:

```ocaml
let[@axiom] name = fun ((x : int) [@forall]) -> PROP
```

### 3.1.2 Propositions (Qualifiers)

Payload qualifiers `φ` are Boolean expressions used to constrain event fields and
refinement types:

```
LIT  ::=  true | false | INT | VAR | OP VAR ...

PROP ::=
    LIT
  | PROP "&&" PROP  |  PROP "||" PROP  |  "not" PROP
  | "implies" PROP PROP  |  "iff" PROP PROP
  | "fun" "((" NAME " [@forall]) :" TYPE ") ->" PROP   -- ∀NAME:TYPE. PROP
```

Examples: `x > 0`, `isFile b || isDir b`, `implies (x > 0) (y > 0)`

### 3.1.3 Effectful Operations (Events)

Declare the effectful API of the system under test. Each operation is annotated with
its role:

| Annotation | Role |
|------------|------|
| `[@@gen]` | Generator request — the test generator actively invokes this |
| `[@@obs]` | System observation — a synchronous response from the SUT |
| `[@@obsRecv]` | Asynchronous observation — used in P language benchmarks |

The payload type is an OCaml object type `< field : type; ... >`. An empty record
`< >` represents `unit`.

```ocaml
val pushReq     : < elem : int >      [@@gen]
val popReq      : < >                 [@@gen]
val popResp     : < elem : int >      [@@obs]
val isEmptyResp : < isEmpty : bool >  [@@obs]
```

### 3.1.4 Refinement Types

Refinement types constrain the values of arguments in uHAT specifications.

**Base refinement type** `{v: T | φ(v)}`:

```ocaml
(PROP : [%v: TYPE])

(true : [%v: int])          (* any integer *)
(v > 0 : [%v: int])         (* positive integers *)
(is_root v : [%v: Path.t])  (* root paths *)
```

**Arrow type** — binds an argument name for use in subsequent refinements:

```ocaml
fun ?l:(ARG = BASE_RTY) -> BODY_RTY

fun ?l:(x = (true : [%v: int])) -> ...   (* x is a ghost int, used in H/F *)
```

**Ghost arrow type** — like an arrow type but the argument is a pure specification
variable with no runtime counterpart:

```ocaml
fun (g : int) -> ...
```

**Intersection type** — specifies multiple disjoint behaviors for one operation,
written as an OCaml array:

```ocaml
[| RTY1; RTY2; ... |]
```

Example (two cases for `createReq` based on whether the parent path is root):

```ocaml
let createReq =
  [|
    (fun ?l:(p = (is_root (parent v) : [%v: Path.t])) -> ...);
    (fun ?l:(p = (not (is_root v)   : [%v: Path.t])) -> ...);
  |]
```

### 3.1.5 Symbolic Regex (SRE) Syntax

History `H` and future `F` components of a uHAT are **Symbolic Regular Expressions**
over events:

| Expression | Meaning |
|------------|---------|
| `allA` | `.*` — any trace (including empty) |
| `anyA` | `.` — any single event |
| `epsilonA` | `ε` — the empty trace |
| `emptyA` | `∅` — the empty language |
| `Op φ` | Single event of type `Op` whose payload satisfies `φ` |
| `A ; B` | Concatenation (sequence) |
| `A \|\| B` | Union (`A ∪ B`) |
| `A && B` | Intersection (`A ∩ B`) |
| `A - B` | Difference (`A \ B`) |
| `starA A` | Kleene star (`A*`) |
| `not A` | Complement |
| `repeat N A` | Exactly `N` repetitions of `A` |
| `range [| a1; a2; ... |]` | Matches any one of the atomic events |
| `parallel [| a1; a2; ... |]` | Matches any interleaving of the atoms |

**Atomic event predicates** — written as `EventName (qualifier)`:

```ocaml
PushReq (elem == x)   (* PushReq event where elem field equals x *)
PopReq true           (* any PopReq event *)
WriteRsp (va == x && st)
```

### 3.1.6 uHAT Specifications

Each declared operation is given a uHAT of the form `(H, Op φ, F)` where `H` is the
history SRE, `Op φ` is the current event, and `F` is the future SRE:

```ocaml
let NAME ARGS = (HISTORY_REGEX, ATOMIC_EVENT, FUTURE_REGEX)
```

Arrow-type arguments bind ghost variables that can appear in all three components:

```ocaml
(* pushReq: a PushReq carrying x must eventually be followed by a PopReq *)
let pushReq ?l:(x = (true : [%v: int])) =
  ( allA,
    PushReq (elem == x),
    (allA; PopReq true; allA) )

(* popReq: a PopReq must be answered by a PopResp *)
let popReq =
  ( allA,
    PopReq true,
    (PopResp true; allA) )

(* popResp: after a PopResp, no PushReq can occur *)
let popResp = (allA, PopResp true, starA (anyA - PushReq true))
```

For operations with multiple behaviors, use an intersection type (array of cases):

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

### 3.1.7 Goal Declaration

The synthesis goal is an SRE annotated with `[@goal]` that describes the *bad trace*
(the property violation to expose). Goals take typed ghost parameters that are
existentially quantified over the SRE:

```ocaml
let[@goal] NAME (GHOST_ARGS) = REGEX
```

**Important:** the goal SRE describes a *violation*, not a correct behavior. Clouseau
synthesizes a generator guaranteed to produce traces matching this SRE whenever the
SUT has the corresponding bug.

```ocaml
(* A trace in which y is pushed but the stack later reports empty without
   ever popping y — this witnesses a "lost element" bug *)
let[@goal] stack (y : int) =
  allA;
  PushReq (elem == y);
  starA (anyA - PopResp (elem == y));
  IsEmptyResp (isEmpty == true)
```

### 3.1.8 P Language Spec Files

P benchmark specs follow the same structure but use `[@@obsRecv]` for asynchronous
responses. The future SRE in each uHAT case is written as an array `[| F1; F2; ... |]`
to model nondeterministic P state machine responses:

```ocaml
val readReq  : < >                     [@@gen]
val readRsp  : < va : int; st : bool > [@@obsRecv]
val writeReq : < va : int >            [@@gen]
val writeRsp : < va : int >            [@@obsRecv]

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

### 3.1.9 Syntax Summary

| Concept | OCaml Syntax | Notes |
|---------|--------------|-------|
| Pure operator | `val (op) : type` | Uninterpreted function |
| Axiom | `let[@axiom] name (x:T) = prop` | Universally quantified FOL fact |
| Event (gen) | `val name : <...> [@@gen]` | Generator request |
| Event (obs) | `val name : <...> [@@obs]` | Synchronous observation |
| Event (async) | `val name : <...> [@@obsRecv]` | Asynchronous observation (P) |
| Base refinement | `(φ : [%v: T])` | `{v:T \| φ}` |
| Arrow type | `fun ?l:(x = rty) -> body` | Binds ghost variable `x` |
| Ghost arrow | `fun (x : T) -> body` | Pure spec variable |
| Intersection | `[| rty1; rty2; ... |]` | Multiple behaviors |
| uHAT | `let name args = (H, Op φ, F)` | Core spec triple |
| Goal | `let[@goal] name (args) = regex` | Bad trace to synthesize toward |

### 3.1.10 Output Files

Synthesized generators are serialized as S-expressions and written to
`output/GOAL_NAME.scm`. These files are consumed by `sample-syn`, `compile-to-p`, and
`show-term`. Statistics for each benchmark run are saved as JSON under `stat/`.
