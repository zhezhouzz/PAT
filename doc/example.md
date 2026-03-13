#### Input File Formats

As a verification tool for representation invariant of datatypes that is
implemented by an underline stateful library, **Marple** expects input that contains
the specification of underline stateful library and a representation invariant
shared by all interfaces. For example, when **Marple** can type check an
interface `INTERFACE` via the following command (introduced in [HAT Type
check](#hat-type-check)):

    $ ./_build/default/bin/main.exe ri-type-check meta-config.json ADT_DIR/INTERFACE.ml

a folder (`ADT_DIR`) should contain the following files:

- `ADT_DIR`
  + `lib_nty.ml` (the basic (OCaml) typing for the underline stateful library)
  + `lib_rty.ml` (the HAT typing for the underline stateful library)
  + `automata_preds.ml` (automata predicates, e.g., 𝑃stored in Example 4.1, it is optional)
  + `ri.ml` (representation invariant shared by all interfaces)
  + `INTERFACE.ml` (source code and HAT of this interface)

#### Format of `lib_nty.ml`

It is the same as the value declaration in OCaml signature:

```c
val NAME : OCAML_TYPE
```

#### Format of `lib_rty.ml`

It has the following format where `HAT` is defined in [Syntax of HAT](#syntax-of-hat).

```c
let[@libRty] NAME = HAT
```

where `NAME` is simply a string.

#### Format of `ri.ml` and `automata_preds.ml`

It has the following format where `SFA` is defined in [Syntax of HAT](#syntax-of-hat).

```c
let[@pred] NAME (ARG : OCAML_TYPE) ... = SFA
```

#### Format of `INTERFACE.ml`

It has the following format where `HAT` is defined in [Syntax of HAT](#syntax-of-hat).

```c
let INTERFACE = OCAML_EXPR

let[@assertRty] INTERFACE = HTY
```

The source code `OCAML_EXPR` expected by **Marple** is simply an OCaml function listing. Currently, **Marple** handles only a subset of OCaml, it does not handle features involving references and effects, parametric polymorphism, or concurrency. Additionally, all functions should be annotated with precise input and output type; all left-hand-side variables in a let binding should be annotated with its precise type.

#### Syntax of HAT

The syntax of the `HAT` and `SFA` is similar to the OCaml let expression but with [attributes](https://v2.ocaml.org/manual/attributes.html):

```c
VAR := string
OCAML_TYPE:= the OCmal type

OP := "==" | "!=" | "+" | "-" | "<" | ">" | ...
EFFOP := string

LIT :=
| "true"
| "false"
| INT
| VAR
| OP VAR ...

PROP :=
| LIT
| "implies" PROP PROP
| "iff" PROP PROP
| PROP "&&" PROP
| PROP "||" PROP
| "not" PROP
| "fun ((" NAME " [@forall]) : " OCAML_TYPE ") ->" PROP // ∀NAME:OCAML_TYPE. PROP

RTY :=
| "(" PROP ":[%" VAR ":" OCAML_TYPE "])" // base type
| "fun ?l(" NAME "=" RTY ") -> " HAT // arrow type
| "fun ((" NAME ":" OCAML_TYPE ")) [@ghost]) -> " HAT // ghost arrow type

EVENT_ARG :=
| VAR
| "(" OCAML_EXPR "[@d])" // ∼𝑣𝑥 in type aliases in Figure 4

SFA_PRED := NAME // automata predicate, e.g., 𝑃stored in Example 4.1

SFA :=
| EFFOP "(" EVENT_ARG "," ... "," VAR "," PROP ")" // symbolic event <EFFOP ... EVENT_ARG ... = VAR | PROP >
| "_X" SFA // next modality
| "_G" SFA // global modality
| "_F" SFA // final modality
| "lastL" // last modality
| SFA ";" SFA // concat
| SFA "&&" SFA // intersection
| SFA "||" SFA // union
| "not" SFA // complement
| SFA_PRED LIT ... // automata predicate application, e.g., 𝑃exists (k) in Example 4.2

HAT :=
| RTY
| "[|" HAT ";" HAT ";" ... "|]" // intersection type
| "{ pre = " SFA "; res =" RTY "; post = " SFA "}" // HAT [pre] rty [post]
| "{ pre = " SFA "; res =" RTY "; newadding = " SFA "}" // HAT [pre] rty [pre; newadding]

```

The definition of the coverage type is consistent with Figure 4. Precisely,
+ the value literal is defined as `LIT`.
+ the qualifier is defined as `PROP`.
+ the refinement type is defined as `RTY`.
+ the Symbolic Finite Automata is defined as `SFA`. Notice that, the type alias `∼𝑣𝑥` is notated by `[@d]`. We also accept the automata predicates application, e.g., `𝑃exists (k)` in Example 4.2.
+ the Hoare Automata Types is defined as `HAT`, we use an abbreviation with the
  `newadding` field when the postcondition automata is just appending new events
  to the precondition automata.
+ Our syntax share the same syntax sugar with OCaml programs, thus, for example

```
let[@assertRty] add ((p : Path.t) [@ghost]) ?l:(path = (true : [%v: Path.t]))
    ?l:(content = (true : [%v: Bytes.t])) =
  { pre = rI p; res = (true : [%v: bool]); post = rI p }
```

can be desugared as

```ocaml
let[@assertRty] add =
    fun ((p : Path.t) [@ghost]) ->
    fun ?l:(path = (true : [%v: Path.t])) ->
    fun ?l:(content = (true : [%v: Bytes.t])) ->
  { pre = rI p; res = (true : [%v: bool]); post = rI p }
```
