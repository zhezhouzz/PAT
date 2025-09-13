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

The download the dependent library: https://github.com/OCamlRefinementType/zutils, and https://github.com/OCamlRefinementType/AutomataLibrary/tree/main  then install it.

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
    mkdir output
    dune exec -- bin/main.exe do-syn stack benchmarks/ADT/stack_spec.ml
```

This command will store the synthesized programs as output/stack.scm

# Run Synthesized Programs

```
    dune exec -- bin/main.exe test-eval stack
```
# Run QuickCheck Programs

```
    dune exec -- bin/main.exe test-random stack
```

Now the benchmark names are hardcoded in the file `bin/commands/cre.ml`.

# Run Benchmarks

```
    python3 script/run2025.py [NAME]
```

Currently support: `graph`, `nfa`, `stlc`, `stack`, `filesystem`, `ifc_store`, `ifc_add`, `ifc_load`.