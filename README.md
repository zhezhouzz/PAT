# P PBT
Property-Based Testing For P

# Install

The easiest way to install the dependencies is via [OPAM](https://opam.ocaml.org/doc/Install.html).

```
  opam init --auto-setup
  opam update
  opam switch create YOUR_NAME ocaml-base-compiler.5.2.0
  eval $(opam env)
  opam install dune core core_unix yojson conf-c++ conf-python qcheck ocolor dolog ocamlbuild z3 ppx_deriving_yojson menhirLib menhir spectrum
```

Then download the dependent libraries `zutils` and `AutomataLibrary`, and install them:

```
    cd zutils
    opam install .
    cd AutomataLibrary
    opam install .
```

Then compile this repo:

```
    dune build
```

# Run Synthesizer

```
dune exec -- bin/main.exe do-syn [GOAL_NAME] [SPEC_FILE]
```

For example,
```
    mkdir output
    dune exec -- bin/main.exe do-syn stack benchmarks/ADT/stack_spec.ml
```

This command will store the synthesized programs as output/stack.scm

# Run Synthesized Programs

Run synthesized programs `N` times (e.g., 100):

```
    dune exec -- bin/main.exe test-eval stack 100
```
# Run QuickCheck Programs

Run random test programs `N` times (e.g., 100):

```
    dune exec -- bin/main.exe test-random stack 100
```

Note: benchmark names are hardcoded in `bin/commands/cre.ml`.

# Run Benchmarks

```
    python3 script/run2025.py [NAME] [NUM_TEST]
```

The script pauses for input between phases (synthesis → evaluation → random); press Enter when prompted.

Currently supported:

+ ADT: `graph`, `nfa`, `stlc`, `stack`, `filesystem`, `ifc_store`, `ifc_add`, `ifc_load`.

+ Database (Causal and Read Committed): `cart`. TODO: `courseware`, `twitter`, `treiber_stack`, `OTLP`.

+ P Language (legacy code needs updates to work with the current version).

# Database

To run MonkeyDB benchmarks, set up a MariaDB Galera cluster.

## MariaDB setup

We use Docker to run three MariaDB instances on different ports.

- Download the image from `https://hub.docker.com/r/bitnami/mariadb-galera`.

- Use Docker Compose (`https://github.com/docker/compose`) to start the cluster (e.g., under the `docker` directory that contains a `docker/compose.yaml`).
    + Start the first node named `galera1` (`-d` runs in the background):
```
    docker compose up galera1 -d 
```

    + Wait until the node is ready to accept connections. Check logs of the detached container:

```
    docker logs -f galera1
```

    Proceed once you see a line like `WSREP: Synchronized with group, ready for connections`.

    + Then start the other nodes (`galera2` and `galera3`):

```
    docker compose up galera2 galera3 -d 
```

    + Connect to MariaDB from the command line (requires the MariaDB client):

```
    mysql -h 127.0.0.1 -P 3308 -uroot -prootpass
```

    The nodes listen on ports `3307`, `3308`, and `3309`; the user is `root`, password `rootpass` (see `docker/compose.yaml` for details).

    + (Optional) Use a web UI via Adminer. First start `adminer`:

```
    docker compose up adminer -d 
``` 

    Then open `http://localhost:8080/` in your browser to access the database.

    + If something fails, clear everything except `docker/compose.yaml` under the `docker` directory, then run `docker compose down -v` to shut down and clean volumes, and try again.

## MariaDB options and tests

A MariaDB-backed, MonkeyDB-like layer lives in `benchmarks/BackendMariaDB/backendMariaDB.ml`.

We support four isolation levels (weak → strong): `ReadUncommitted`, `ReadCommitted`, `Causal` (via Galera), and `Serializable`. InnoDB prevents write/write conflicts: two concurrent transactions cannot both write the same row. If a synthesized schedule violates these policies, the database may deadlock or raise errors.

We provide several simple test programs in `benchmarks/BackendMariaDB/backendMariaDB.ml`:

+ `test_stuck`: causes a write/write conflict.
+ `test_dirty_read`: reads uncommitted changes.
+ `test_non_repeatable_read`: reads different results within one transaction.
+ `test_causal`: violates causal consistency.
+ `test_cart`: a possible run of the `cart` benchmark.

Try them, for example:

```
dune exec -- bin/main.exe test-non-repeatable-read ReadUncommitted
```

These commands are also defined in `bin/commands/cre.ml`.

## Next Steps

To extend and evaluate the system, follow these steps:

1. **Set up MariaDB and run the `cart_rc` benchmark.**
   - Ensure your MariaDB cluster is running as described above.
   - Use the provided scripts and commands to execute the `cart_rc` benchmark and verify correct operation.

2. **Add and evaluate additional benchmarks: `courseware`, `twitter`, `treiber_stack`, and `smallbank`.**
   - For each benchmark:
     - **Design the database schema:** Define tables and relationships to represent the application's data model.
     - **Implement handlers:** Write the code to process transactions and operations for the benchmark.
     - **Write the specification:** Formalize the application APIs and isolation requirements for the benchmark (isolation requirements are similar with `cart`).
     - **Develop a random test generator:** Create a generator to produce randomized sequences of operations in asynchronize style.
     - **Run experiments:** Execute the benchmark under different isolation levels and collect results.