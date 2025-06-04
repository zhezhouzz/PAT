open Zutils
open Prop
open Sexplib.Std
open AutomataLibrary

type 't ltlf =
  | EventL of 't sevent
  | LorL of 't ltlf * 't ltlf
  | LandL of 't ltlf * 't ltlf
  | SeqL of 't ltlf * 't ltlf
  | NegL of 't ltlf
  | NextL of 't ltlf
  | UntilL of 't ltlf * 't ltlf
  | LastL
  | FinalL of 't ltlf
  | GlobalL of 't ltlf
  | ForallL of { qv : ('t, string) typed; body : 't ltlf }
  | ExistsL of { qv : ('t, string) typed; body : 't ltlf }
[@@deriving sexp, show, eq, ord]

type 't t_p_expr = ('t, 't lit) typed [@@deriving sexp, show, eq, ord]

let primitive_function = [ "this"; "null"; "halt" ]

type assign_kind = Assign | AssignSeqAdd | AssignSetAdd | AssignMapAdd
[@@deriving sexp, show, eq, ord]

type foreach_kind = ForeachSet | ForeachSeq | ForeachMap
[@@deriving sexp, show, eq, ord]

type 't p_stmt =
  | PMute of 't t_p_expr
  | PAssign of {
      assign_kind : assign_kind;
      lvalue : 't t_p_expr;
      rvalue : 't t_p_expr;
    }
  | PIf of {
      condition : 't t_p_expr;
      tbranch : 't p_block;
      fbranch : 't p_block option;
    }
  | PForeach of {
      foreach_kind : foreach_kind;
      iter : (Nt.nt, string) typed;
      iterable : 't t_p_expr;
      body : 't p_block;
    }
  | PWhile of { condition : 't t_p_expr; body : 't p_block }
  | PReturn of 't t_p_expr
  | PPrintf of (string * 't t_p_expr list)
  | PSend of { dest : 't t_p_expr; event_name : string; payload : 't t_p_expr }
  | PRecieve of {
      input : (Nt.nt, string) typed;
      event_name : string;
      body : 't p_block;
    }
  | PGoto of string
  | PBreak

and 't p_block = 't p_stmt list [@@deriving sexp, show, eq, ord]

let rec p_stmt_retty loc stmt =
  match stmt with
  | PMute _ -> []
  | PIf { tbranch; fbranch; _ } ->
      let t1 = p_block_retty loc tbranch in
      let t2 =
        match fbranch with None -> [] | Some x -> p_block_retty loc x
      in
      t1 @ t2
  | PForeach { body; _ } -> p_block_retty loc body
  | PWhile { body; _ } -> p_block_retty loc body
  | PRecieve { body; _ } -> p_block_retty loc body
  | PReturn e -> [ e.ty ]
  | PAssign _ | PPrintf _ | PSend _ | PGoto _ | PBreak -> []

and p_block_retty loc e = List.concat_map (p_stmt_retty loc) e

let get_p_stmt_retty loc stmt =
  let res = Zdatatype.List.slow_rm_dup Nt.equal_nt @@ p_stmt_retty loc stmt in
  match res with [] -> [ Nt.unit_ty ] | _ -> res

let get_p_block_retty loc stmt =
  let res = Zdatatype.List.slow_rm_dup Nt.equal_nt @@ p_block_retty loc stmt in
  match res with [] -> [ Nt.unit_ty ] | _ -> res

type 't p_closure = {
  local_vars : (Nt.nt, string) typed list;
  block : 't p_block;
}
[@@deriving sexp, show, eq, ord]

type func_label = Plain | Entry | Exit | Listen of string
[@@deriving sexp, show, eq, ord]

type 't p_func = {
  name : string;
  func_label : func_label;
  params : ('t, string) typed list;
  retty : 't;
  closure : 't p_closure;
}
[@@deriving sexp, show, eq, ord]

let get_p_func_var x =
  x.name#:(Nt.construct_arr_tp (List.map _get_ty x.params, x.retty))

type state_label = Hot | Cold | Start [@@deriving sexp, show, eq, ord]

type 't p_state = {
  name : string;
  state_label : state_label list;
  state_body : 't p_func list;
}
[@@deriving sexp, show, eq, ord]

type 't p_machine = {
  name : string;
  local_vars : (Nt.nt, string) typed list;
  local_funcs : 't p_func list;
  states : 't p_state list;
}
[@@deriving sexp, show, eq, ord]

type top_level_decl_kind = TopType | TopEvent | TopVar
[@@deriving sexp, show, eq, ord]

type 't p_global_prop = { name : string; prop : 't prop }
[@@deriving sexp, show, eq, ord]

type 't p_payload_prop = {
  name : string;
  self_event : (string, string) typed;
  prop : 't prop;
}
[@@deriving sexp, show, eq, ord]

type 't template =
  | TPReturn of 't t_p_expr
  | TPIf of {
      condition : 't prop;
      tbranch : 't template option;
      fbranch : 't template option;
    }
[@@deriving sexp, show, eq, ord]

type 't p_payload_gen = {
  gen_name : string;
  self_event_name : string;
  local_vars : ('t, string) typed list;
  content : 't template;
}
[@@deriving sexp, show, eq, ord]

type 't p_syn = {
  name : string;
  gen_num : (string * 't t_p_expr) list;
  cnames : string list;
}
[@@deriving sexp, show, eq, ord]

type 't p_item =
  | PVisible of string list
  | PTopSimplDecl of { kind : top_level_decl_kind; tvar : ('t, string) typed }
  | PEnumDecl of (string * string list)
  (* | PMachine of 't p_machine *)
  | PGlobalProp of 't p_global_prop
  | PPayload of 't p_payload_prop
  | PPayloadGen of 't p_payload_gen
  | PSyn of 't p_syn
[@@deriving sexp, show, eq, ord]

open Typectx

type basic_typing_ctx = {
  prop_ctx : Nt.nt prop ctx;
  payload_ctx : Nt.nt p_payload_prop ctx;
  payload_gen_ctx : Nt.nt p_payload_gen ctx;
  syn_ctx : Nt.nt p_syn ctx;
  machine_ctx : unit ctx;
  event_ctx : Nt.nt ctx;
  ctx : Nt.nt ctx;
}

let mk_basic_typing_ctx =
  {
    prop_ctx = emp;
    payload_ctx = emp;
    payload_gen_ctx = emp;
    syn_ctx = emp;
    machine_ctx = emp;
    event_ctx = emp;
    ctx = emp;
  }

(** Synthesis *)

type value = VVar of (Nt.nt, string) typed | VConst of constant
[@@deriving sexp, show, eq, ord]

type trace_elem = string * constant list [@@deriving show, eq, ord]
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
  | CUnion of term list
  | CAssert of value
  | CAssume of (Nt.nt list * Nt.nt prop)
  | CAssertP of Nt.nt prop
[@@deriving sexp, show, eq, ord]

open Prop
open AutomataLibrary

type srl = Nt.nt sevent regex [@@deriving show, eq, ord]

type syn_goal = { qvs : (Nt.nt, string) typed list; prop : srl }
[@@deriving show, eq, ord]

type 'r item =
  | PrimDecl of { name : string; nt : Nt.nt }
  | MsgNtDecl of {
      generative : bool;
      name : string;
      nt : Nt.nt;
      recvable : bool;
    }
  | SynGoal of syn_goal
[@@deriving show, eq, ord]

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

open Typectx

type syn_env = {
  gen_ctx : bool ctx;
  recvable_ctx : bool ctx;
  event_tyctx : Nt.t ctx;
  tyctx : Nt.t ctx;
  goal : syn_goal option;
}
