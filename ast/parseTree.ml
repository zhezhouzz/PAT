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

type value = VVar of (Nt.nt, string) typed | VConst of constant
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
type plan_elem =
  | PlanAct of { op : string; args : (Nt.nt, string) typed list }
  | PlanActBuffer of {
      op : string;
      args : (Nt.nt, string) typed list;
      phi : Nt.nt prop;
    }
  | PlanSe of Nt.nt sevent
  | PlanStarInv of SFA.CharSet.t
  | PlanStar of SFA.CharSet.t regex
[@@deriving eq, ord]

type plan = plan_elem list

type syn_env = {
  event_rtyctx : srl pat ctx;
  event_tyctx : t ctx;
  msgkind_ctx : message_kind ctx;
  tyctx : Nt.t ctx;
  goal : syn_goal option;
}
