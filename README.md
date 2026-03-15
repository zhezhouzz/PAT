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

To display a synthesized generator in human-readable form:

```
$ docker compose exec clouseau ./main.exe show-term output/GOAL_NAME.scm
```

Example — after synthesizing the `stack` benchmark (see §2.3.1):

```
$ docker compose exec clouseau ./main.exe show-term output/stack.scm
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

Running the scripts without arguments executes all steps in order and prints the
final table. This is the easiest way to reproduce the paper results.

### 2.2.1 Reproducing Table 1 (ADT & QCheck Benchmarks)

**Benchmarks:** Stack, HashTable, Filesystem, Graph, NFA, IFCStore, IFCAdd, IFCLoad,
DeBruijn1, DeBruijn2, Shopping, Courseware, Twitter, Smallbank

```
$ docker compose exec clouseau python3 scripts/run_ocaml_bench.py
```

### 2.2.2 Reproducing Table 2 (P Language Benchmarks)

**Benchmarks:** Database, Firewall, RingLeaderElection, BankServer, Simplified2PC,
HeartBeat, ChainReplication, Paxos, Raft, AnonReadAtomicity

```
$ docker compose exec clouseau python3 scripts/run_p_bench.py
```

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
$ docker compose exec clouseau ./main.exe do-syn GOAL_NAME benchmarks/OCamlBench/SPEC_FILE 1
```

Output is written to `output/GOAL_NAME.scm`. Example for a single benchmark:

```
$ docker compose exec clouseau ./main.exe do-syn stack benchmarks/OCamlBench/stack_spec.ml 1
```

**Step 2 — Run synthesized generators (200 runs each)**

```
$ docker compose exec clouseau python3 scripts/run_ocaml_bench.py runsyn
```

For each benchmark, this runs:

```
$ docker compose exec clouseau ./main.exe sample-syn GOAL_NAME 200
```

**Step 3 — Run random (QCheck) baseline**

```
$ docker compose exec clouseau python3 scripts/run_ocaml_bench.py runrandom
```

For each benchmark, this runs:

```
$ docker compose exec clouseau ./main.exe sample-random GOAL_NAME 1800
```

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
$ docker compose exec clouseau ./main.exe do-syn p_BENCHNAME benchmarks/PBench/p_BENCHNAME_spec.ml 1
```

Then compiles the result to a P scheduler:

```
$ docker compose exec clouseau ./main.exe compile-to-p p_BENCHNAME BENCHNAME
```

The compiled P file is written to `penv/BENCHNAME/PSyn/SynClient.p`.

**Step 2 — Run synthesized schedulers (500 runs each)**

```
$ docker compose exec clouseau python3 scripts/run_p_bench.py runsyn
```

Runs each P benchmark under `penv/BENCHNAME/` using the synthesized scheduler.

**Step 3 — Run random P baseline**

```
$ docker compose exec clouseau python3 scripts/run_p_bench.py runrandom
```

Runs each P benchmark under `poriginal/BENCHNAME/` using the original random
scheduler. Used as the baseline for benchmarks without a manually-written scheduler.

**Step 4 — Run default (manual) P baseline**

```
$ docker compose exec clouseau python3 scripts/run_p_bench.py rundefault
```

Runs each P benchmark under `poriginal/BENCHNAME/` using the manually-written
default scheduler (mode `Manual`). Used as the baseline for benchmarks in
`manual_baseline_benchmarks`: EspressoMachine, BankServer, Simplified2PC, HeartBeat,
ChainReplication, Paxos, AnonReadAtomicity. Both Step 3 and Step 4 must be run
before printing Table 2, as each benchmark uses whichever baseline is applicable.

**Step 5 — Print Table 2 as LaTeX**

```
$ docker compose exec clouseau python3 scripts/run_p_bench.py table2
```

---

## 2.4 Input File Formats

Each benchmark is specified in a single `.ml` file using Clouseau's OCaml-embedded DSL
(files are conventionally named `*_spec.ml`). The DSL reuses the OCaml parser and maps
OCaml constructs to internal Clouseau AST nodes. A spec file has five sections:

1. **Pure Operators** — uninterpreted functions and axioms used in logic formulas
2. **Effectful Operations** — declarations of generator requests and system observations
3. **Refinement Types & uHAT Specifications** — behavioral specs for each operation
4. **Global Property (Goal)** — the bad trace the synthesizer targets

### 2.4.1 Pure Operators and Axioms

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

### 2.4.2 Propositions (Qualifiers)

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

### 2.4.3 Effectful Operations (Events)

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

### 2.4.4 Refinement Types

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

### 2.4.5 Symbolic Regex (SRE) Syntax

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

### 2.4.6 uHAT Specifications

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

### 2.4.7 Goal Declaration

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

### 2.4.8 P Language Spec Files

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

### 2.4.9 Syntax Summary

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

### 2.4.10 Output Files

Synthesized generators are serialized as S-expressions and written to
`output/GOAL_NAME.scm`. These files are consumed by `sample-syn`, `compile-to-p`, and
`show-term`. Statistics for each benchmark run are saved as JSON under `stat/`.
