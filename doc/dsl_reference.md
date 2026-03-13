# uHAT DSL Specification Reference

This document provides a detailed reference for the Domain Specific Language (DSL) used to specify properties of programs in the uHAT (Underapproximated Hoare Automata Type) framework. The DSL is embedded in OCaml syntax, utilizing a custom parser that transforms valid OCaml ASTs into the internal AST.

Files containing these specifications typically end in `_spec.ml` or `task.ml` and are located in `benchmarks/ADT/` or `benchmarks/MonkeyDB/`.

## 1. Overview

A specification file consists of four main components:
1.  **Pure Operators**: Uninterpreted functions and axioms used in logic formulas.
2.  **Propositions (Qualifiers)**: The logic formulas used in refinement types and axioms.
3.  **Effectful Operations**: Definitions of events (generator requests and system observations).
4.  **uHATs (Underapproximated Hoare Automata Types)**: The core specifications describing valid traces for operations.
5.  **Global Properties**: System-wide temporal properties (goals), typically expressed as negations (counter-examples).

## 2. Pure Operators (Uninterpreted Functions & Axioms)

Pure operators are mathematical functions used within the logic formulas of your specification. They correspond to the "Pure Operators" in Figure 2 of the paper.

### 2.1 Uninterpreted Functions

**Syntax:**
```ocaml
val ( OP_NAME ) : TYPE_SIGNATURE
val NAME : TYPE_SIGNATURE
```

**Example:**
```ocaml
val ( == ) : 'a. 'a -> 'a -> bool
val is_root : Path.t -> bool
val parent : Path.t -> Path.t
```

### 2.2 Axioms

You can provide axioms to describe the semantic meaning of these operators. Axioms are First-Order Logic (FOL) formulas that are assumed to be true.

**Syntax:**

The fundamental syntax for an axiom is:

```ocaml
let[@axiom] AXIOM_NAME = PROP
```

Commonly, we use a "sugared" form that looks like a function definition to represent universal quantification (Sigma type shorthand):

```ocaml
let[@axiom] AXIOM_NAME (ARG : TYPE) = PROP
```

This is equivalent to:
```ocaml
let[@axiom] AXIOM_NAME = fun ((ARG : TYPE) [@forall]) -> PROP
```

**Examples:**
```ocaml
(* A byte is either a file or a directory *)
let[@axiom] isFileOrDir (b : Byte.t) = isFile b || isDir b

(* A byte cannot be both a file and a directory *)
let[@axiom] isDirNotFile (b : Byte.t) = not (isFile b && isDir b)

(* A path is root if and only if its parent is itself *)
let[@axiom] parent_is_root (p : Path.t) = iff (p == parent p) (is_root p)
```

## 3. Propositions (Qualifiers)

Propositions (`PROP`) and Literals (`LIT`) form the basis of the logic used in axioms and refinement types.

**Syntax (from doc/example.md):**

```
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
```

**Examples:**
*   `x > 0`
*   `isFile b || isDir b`
*   `implies (x > 0) (y > 0)`

## 4. Effectful Operations (Events)

Effectful operations represent the interactions in the system. They are categorized into:
*   **Generator Requests (`gen`)**: Operations initiated by the client/environment.
*   **System Observations (`obs`)**: Responses or side-effects produced by the System Under Test (SUT).

**Syntax:**
```ocaml
val NAME : < FIELD : TYPE; ... > [@@TAG]
```

*   **Object Type (`< ... >`)**: We use OCaml's object type syntax to define the arguments and return values of the operation.
    *   For a request (e.g., `pushReq`), the fields represent the arguments.
    *   For a response (e.g., `popResp`), the fields represent the return values.
    *   An empty record `< >` implies a `unit` type.
*   **Attributes (`[@@...]`)**:
    *   `[@@gen]`: Marks the operation as a generator request.
    *   `[@@obs]`: Marks the operation as a system observation.

**Examples:**

```ocaml
(* A request to push an element, taking no arguments here because the argument 
   is handled logically in the uHAT, or implied to be unit if empty *)
val popReq : < > [@@gen]

(* A response returning an integer element *)
val popResp : < elem : int > [@@obs]

(* A request to push, implicitly taking arguments defined later in the uHAT *)
val pushReq : < elem : int > [@@gen]
```

## 5. Refinement Types

Refinement Types allow us to constrain the values of arguments and return types using logical predicates. In the uHAT DSL, these are constructed using standard OCaml expressions that are parsed into specific RTY constructs.

### 5.1 Base Refinement Types

A base refinement type `{v: T | P(v)}` is represented using an OCaml constraint syntax with a special extension point `[%v: T]`.

**Syntax:**
```ocaml
(PROP : [%v: TYPE])
```

*   `PROP`: The refinement predicate. The variable being refined is implicitly named `v` (or derived from context).
*   `TYPE`: The underlying OCaml type.

**Examples:**
```ocaml
(true : [%v: int])        (* {v: int | true} - Any integer *)
(v > 0 : [%v: int])       (* {v: int | v > 0} - Positive integers *)
(is_root v : [%v: Path.t]) (* {v: Path.t | is_root v} - Root paths *)
```

### 5.2 Arrow Types (Dependent Functions)

Arrow types represent functions where the argument can be used in the refinement of the return type (or subsequent arguments). We use OCaml's labeled argument syntax to bind the argument name.

**Syntax:**
```ocaml
fun ?l:(ARG_NAME = BASE_RTY) -> BODY_RTY
```

*   `ARG_NAME`: The name of the argument variable.
*   `BASE_RTY`: The refinement type of the argument (e.g., `(true : [%v: int])`).
*   `BODY_RTY`: The return type, which can be another arrow, a uHAT triple, or an intersection.

**Example:**
```ocaml
fun ?l:(x = (true : [%v: int])) -> ...
```

### 5.3 Ghost Arrow Types

Ghost arguments are used for specification purposes but do not exist in the runtime code.

**Syntax:**
```ocaml
fun (ARG_NAME : TYPE) -> BODY_RTY
```

**Example:**
```ocaml
fun (g : int) -> ...
```

### 5.4 Intersection Types

Intersection types allow specifying multiple behaviors for the same operation (e.g., different behaviors based on input values). This is represented using OCaml arrays `[| ... |]`.

**Syntax:**
```ocaml
[| RTY1; RTY2; ... |]
```

**Example (from filesystem_spec.ml):**
```ocaml
let createReq =
  [|
    (* Case 1: Parent is root *)
    (fun ?l:(p = (is_root (parent v) : [%v: Path.t])) ... -> ...);
    
    (* Case 2: Parent is not root *)
    (fun ?l:(p = (not (is_root v) : [%v: Path.t])) ... -> ...);
  |]
```

## 6. uHATs (Underapproximated Hoare Automata Types)

uHATs specify the behavior of operations using a triple `(History, FirstEvent, Future)`. This triple is the "body" or "return type" of the refinement type definition.

**Syntax:**

```ocaml
let NAME = (HISTORY_REGEX, ATOMIC_EVENT, FUTURE_REGEX)
```

The body of a uHAT is simply a tuple of three elements, corresponding to `[H]x:t[F]` in the paper, but split structurally:

### 6.2. Regex (SFA) Syntax

The following syntax defines the Symbolic Finite Automata (SFA) used for History and Future components.

**Regex Operators:**

*   `epsilonA`: Empty sequence (ε).
*   `emptyA`: Empty set (∅).
*   `anyA`: Wildcard matching any single event (.).
*   `allA`: Matches any sequence of events (`.*`).
*   `starA RE`: Kleene star (`RE*`).
*   `not RE`: Complement (`REᶜ`).
*   `RE1 || RE2`: Union (`RE1 ∪ RE2`).
*   `RE1 && RE2`: Intersection (`RE1 ∩ RE2`).
*   `RE1 - RE2`: Set difference (`RE1 \ RE2`).
*   `RE1; RE2`: Concatenation (Sequence).
*   `repeat N RE`: Repeats `RE` exactly `N` times.
*   `range [| atom1; atom2; ... |]`: Matches any one of the atoms in the array.
*   `parallel [| atom1; atom2; ... |]`: Matches any interleaving of the atoms.

**Atomic Predicates:**
Events are written as `EventName (ArgumentConstraint)`.
*   `PushReq (elem == x)`: Matches a `PushReq` event where the `elem` field equals `x`.
*   `PopReq true`: Matches any `PopReq` event (predicate is true).

## 7. Global Properties (Goals)

Global properties specify invariants or temporal goals that the system must satisfy. They are identified by the `[@goal]` attribute.

**Important:** In this framework, global properties are typically specified as **negations** (counter-examples). You define a regex that describes a *violation* of the property (a "bad" trace). If the system can generate a trace matching this regex, the property is violated.

**Syntax:**
```ocaml
let[@goal] NAME (GHOST_ARGS) = REGEX
```

**Example:**

```ocaml
(* Goal: This describes a BUG trace.
   1. We push an element 'y'.
   2. We NEVER see a PopResp returning 'y' (starA (anyA - PopResp (elem == y))).
   3. But eventually, the system claims the stack is empty (IsEmptyResp (isEmpty == true)).
   
   If this trace exists, it means the stack "lost" element 'y', which is a violation. *)
let[@goal] stack (y : int) =
  allA;
  PushReq (elem == y);
  starA (anyA - PopResp (elem == y));
  IsEmptyResp (isEmpty == true)
```

## 8. Syntax Summary & Implementation Details

Since this DSL reuses the OCaml parser, here is the mapping from OCaml constructs to internal constructs:

| Concept | OCaml Syntax | Internal Representation |
| :--- | :--- | :--- |
| **Pure Op** | `val (op) : type` | `PrimDecl` |
| **Axiom** | `let[@axiom] name = prop` | `PrAxiom` |
| **Event (Gen)** | `val name : <...> [@@gen]` | `MsgNtDecl { kind=Gen }` |
| **Event (Obs)** | `val name : <...> [@@obs]` | `MsgNtDecl { kind=Obs }` |
| **uHAT** | `let name args = (H, A, F)` | `MsgDecl` |
| **Goal** | `let[@goal] name = ...` | `SynGoal` |
| **Refinement** | `(phi : [%v: type])` | `RtyBase` |
| **Intersection** | `[| uHAT1; uHAT2 |]` (Array) | `RtyInter` |
| **Arrow** | `fun ?l:(x=RTY) -> ...` | `RtyArr` |
| **Ghost Arrow** | `fun (x:T) -> ...` | `RtyGArr` |
