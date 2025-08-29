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
[@@deriving show, eq, ord, sexp]

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
[@@deriving sexp, show, eq, ord]

type trace_elem = { op : string; args : constant list }
[@@deriving show, eq, ord]

type trace = trace_elem list [@@deriving show, eq, ord]

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
  | CWhile of { body : (Nt.nt, term) typed; cond : Nt.nt prop }
  | CAssertP of Nt.nt prop
[@@deriving sexp, show, eq, ord]

type syn_goal = { qvs : (Nt.nt, string) typed list; prop : rich_srl }
type message_kind = Gen | Obs [@@deriving sexp, show, eq, ord]

let is_generative = function Gen -> true | Obs -> false
let is_observable = function Gen -> false | Obs -> true

type 'r item =
  | PrimDecl of { name : string; nt : Nt.nt }
  | MsgNtDecl of { msgkind : message_kind; name : string; nt : Nt.nt }
  | MsgDecl of { name : string; pat : 'r pat }
  | SynGoal of syn_goal

(* For Synthesis *)

type linear_regex =
  | LChar of Nt.nt sevent
  (* | LMultiChar of SFA.CharSet.t *)
  | LStar of SFA.CharSet.t regex
[@@deriving eq, ord]

type act = { aid : int; aop : string; aargs : (Nt.nt, string) typed list }
[@@deriving eq, ord]

type line_elem =
  | LineAct of act
  (* | LineMultiChar of SFA.CharSet.t *)
  | LineStar of SFA.CharSet.t regex
[@@deriving eq, ord]

open Zdatatype

module ActMap = Stdlib.Map.Make (struct
  type t = act

  let compare act1 act2 =
    let res = compare act1.aid act2.aid in
    if res == 0 then List.compare compare act1.aargs act2.aargs else res
end)

type line = { gprop : Nt.nt prop; elems : line_elem list }

type plan = {
  freeVars : (Nt.nt, string) typed list; (* extential variables *)
  assigns : string StrMap.t;
      (* the assignments of local variables, for efficiency *)
  line : line; (* one linear sequential code *)
  actMap : int ActMap.t; (* the map from act to id *)
  checkedActs : int list IntMap.t; (* the acts within the task to type check *)
}

type syn_env = {
  event_rtyctx : srl pat ctx;
  event_tyctx : t ctx;
  msgkind_ctx : message_kind ctx;
  tyctx : Nt.t ctx;
  goal : syn_goal option;
}

exception IsolationViolation of string
exception NoBugDetected of string
