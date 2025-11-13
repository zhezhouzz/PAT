include Common
open Sexplib.Std
include Zutils
include Typectx
include Myconfig
open AutomataLibrary

type rich_srl = Nt.nt sevent rich_regex [@@deriving show, eq, ord]
type srl = SFA.CharSet.t regex

let default_v = "v"

type cty = { nty : Nt.nt; phi : Nt.nt prop } [@@deriving show, eq, ord]

type 'r pat =
  | RtyBase of cty
  | RtyHAF of { history : 'r; adding : 'r; future : 'r }
  | RtyHAParallel of {
      history : 'r;
      adding_se : Nt.nt sevent;
      parallel : Nt.nt sevent list;
    }
    (* parse only *)
  | RtyGArr of { arg : string; argnty : Nt.nt; retrty : 'r pat }
  | RtyArr of { arg : string; argcty : cty; retrty : 'r pat }
  | RtyInter of 'r pat * 'r pat
[@@deriving show, eq, ord]

type stlcTy = StlcInt | StlcArrow of stlcTy * stlcTy
[@@deriving show, eq, ord, sexp, yojson]

type stlcTerm =
  | StlcVar of int
  | StlcConst of int
  | StlcAbs of { absTy : stlcTy; absBody : stlcTerm }
  | StlcApp of { appFun : stlcTerm; appArg : stlcTerm }
[@@deriving show, eq, ord]

type value =
  | VVar of (Nt.nt, string) typed
  | VConst of constant
  | VCStlcTy of stlcTy
  | VCIntList of int list
  | VTu of value list
  | VProj of value * int
  | VField of value * string
  | VRecord of (string * value) list
[@@deriving sexp, show, eq, ord, yojson]

type trace_elem = { op : string; args : constant list }
[@@deriving show, eq, ord]

type trace = trace_elem list [@@deriving show, eq, ord]

let default_self_name = "recfun"
let default_iter_var = "iter"#:Nt.int_ty
let default_bound_var = "bound"#:Nt.int_ty

type term =
  | CVal of (Nt.nt, value) typed
  | CLetE of {
      rhs : (Nt.nt, term) typed;
      lhs : (Nt.nt, string) typed list;
      body : (Nt.nt, term) typed;
    }
  | CAppOp of { op : (Nt.nt, string) typed; args : (Nt.nt, value) typed list }
  | CObs of { op : (Nt.nt, string) typed; prop : Nt.nt prop }
  | CGen of { op : (Nt.nt, string) typed; args : (Nt.nt, value) typed list }
  | CUnion of (Nt.nt, term) typed list
  | CAssume of (Nt.nt list * Nt.nt prop)
  (* | CWhile of { body : (Nt.nt, term) typed; cond : Nt.nt prop } *)
  (* | KStar of { body : (Nt.nt, term) typed } *)
  | CFix of { retBranch : (Nt.nt, term) typed; recBranch : (Nt.nt, term) typed }
  | CFixApp of {
      cfix : (Nt.nt, term) typed option;
      iterV : (Nt.nt, term) typed;
      boundV : (Nt.nt, value) typed;
    }
  | CAssertP of Nt.nt prop
[@@deriving sexp, show, eq, ord]

type syn_goal = {
  name : string;
  qvs : (Nt.nt, string) typed list;
  prop : rich_srl;
}

type message_kind = Gen | Obs | ObsRecv [@@deriving sexp, show, eq, ord]

let is_generative = function Gen -> true | Obs -> false | ObsRecv -> false
let is_observable = function Gen -> false | Obs -> true | ObsRecv -> true

type 'r item =
  | PrimDecl of { name : string; nt : Nt.nt }
  | MsgNtDecl of { msgkind : message_kind; name : string; nt : Nt.nt }
  | MsgDecl of { name : string; pat : 'r pat }
  | SynGoal of syn_goal
  | PrAxiom of { name : string; prop : Nt.nt prop }

(* For Synthesis *)

type linear_regex_elem =
  | LinearChar of Nt.nt sevent
  | LinearStar of SFA.CharSet.t

type act = {
  aop : string;
  aargs : (Nt.nt, string) typed list;
  aid : int option;
  aparent : int option;
  achildren : int list option;
  tmp : int;
}
[@@deriving eq, ord, sexp]

type line_elem =
  | LineAct of act
  (* | LineMultiChar of SFA.CharSet.t *)
  | LineStarMultiChar of SFA.CharSet.t (* | LineStar of SFA.CharSet.t regex *)
[@@deriving eq, ord, sexp]

open Zdatatype

module ActMap = Stdlib.Map.Make (struct
  type t = act

  let compare act1 act2 =
    let res = compare act1.aop act2.aop in
    if res == 0 then List.compare compare act1.aargs act2.aargs else res
end)

type line = { gprop : Nt.nt prop; elems : line_elem list } [@@deriving sexp]

type synMidResult =
  | SynMidPlan of line
  | SynMidKStar of {
      old_goal : line;
      pre_len : int;
      line_b1 : line;
      line_b2 : line;
      line_b2_pre_len : int;
      v : value;
    }
[@@deriving sexp]

type syn_env = {
  event_rich_rtyctx : rich_srl pat ctx;
  event_rtyctx : srl pat ctx;
  event_tyctx : t ctx;
  msgkind_ctx : message_kind ctx;
  tyctx : Nt.t ctx;
  goals : syn_goal StrMap.t;
  axioms : Nt.nt prop StrMap.t;
}

exception IsolationViolation of string
exception NoBugDetected of string

type isolation = Serializable | Causal | ReadCommitted | ReadUncommitted
[@@deriving sexp, show, eq, ord]

let isolation_of_string = function
  | "Serializable" -> Serializable
  | "Causal" -> Causal
  | "ReadCommitted" -> ReadCommitted
  | "ReadUncommitted" -> ReadUncommitted
  | _ -> Zutils.(_die_with [%here] "invalid isolation")

let __counter = ref 0
let ghost_event_names = [ "tyOpen"; "tyClose"; "depth" ]
