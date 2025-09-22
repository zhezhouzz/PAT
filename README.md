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

Currently supported:

+ ADT: `graph`, `nfa`, `stlc`, `stack`, `filesystem`, `ifc_store`, `ifc_add`, `ifc_load`.

+ Database (Causal and Read Committed): `cart`. TODO: `courseware`, `twitter`, `treiber_stack`, `OTLP`.

+ P Language (legacy code needs updates to work with the current version).
