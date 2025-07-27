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

type syn_goal = { qvs : (Nt.nt, string) typed list; prop : rich_srl }

type 'r item =
  | PrimDecl of { name : string; nt : Nt.nt }
  | MsgNtDecl of {
      generative : bool;
      name : string;
      nt : Nt.nt;
      recvable : bool;
    }
  | MsgDecl of { name : string; pat : 'r pat }
  | SynGoal of syn_goal

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
  gen_ctx : bool ctx;
  recvable_ctx : bool ctx;
  event_tyctx : t ctx;
  tyctx : Nt.t ctx;
  goal : syn_goal option;
}

let mk_value_tt = (VConst U)#:Nt.unit_ty
let mk_term_tt = CVal mk_value_tt

let term_to_nt = function
  | CVal v -> v.ty
  | CLetE { body; _ } -> body.ty
  | CAppOp { op; _ } -> snd @@ Nt.destruct_arr_tp op.ty
  | CObs { op; _ } -> snd @@ Nt.destruct_arr_tp op.ty
  | CGen _ | CUnion _ | CAssert _ | CAssertP _ -> Nt.unit_ty
  (* | CRandom nt -> nt *)
  | CAssume (nts, _) -> Nt.Ty_tuple nts

let mk_let lhs rhs body =
  let ty =
    match lhs with
    | [] -> Nt.unit_ty
    | [ x ] -> x.ty
    | _ -> Nt.Ty_tuple (List.map _get_ty lhs)
  in
  CLetE { lhs; rhs = rhs#:ty; body = body#:(term_to_nt body) }

let term_concat term body =
  CLetE { lhs = []; rhs = term#:Nt.unit_ty; body = body#:Nt.unit_ty }

let mk_inter_type l =
  match l with
  | [] -> _die [%here]
  | h :: t -> List.fold_left (fun x y -> RtyInter (x, y)) h t

let erase_cty { nty; _ } = nty

let rec erase_rty = function
  | RtyBase cty -> erase_cty cty
  | RtyHAF _ | RtyHAParallel _ -> Nt.unit_ty
  | RtyGArr { retrty; _ } -> erase_rty retrty
  | RtyArr { argcty; retrty; _ } ->
      Nt.mk_arr (erase_cty argcty) (erase_rty retrty)
  | RtyInter (t1, t2) ->
      let t1, t2 = map2 erase_rty (t1, t2) in
      let t = Nt.unify_two_types [%here] [] (t1, t2) in
      t

let mk_haf (history, adding, future) = RtyHAF { history; adding; future }

let rec is_singleton_pat = function
  | RtyBase _ | RtyHAF _ | RtyHAParallel _ -> true
  | RtyGArr { retrty; _ } | RtyArr { retrty; _ } -> is_singleton_pat retrty
  | RtyInter _ -> false

let rec pat_to_triple = function
  | RtyInter (t1, t2) -> pat_to_triple t1 @ pat_to_triple t2
  | _ as r ->
      if is_singleton_pat r then [ r ]
      else _die_with [%here] "not a well-formed HAF type"

let qv_to_cqv { x; ty } = { x; ty = { nty = ty; phi = mk_true } }
let value_to_nt = function VVar x -> x.ty | VConst c -> constant_to_nt c
let value_to_tvalue v = v#:(value_to_nt v)
let value_to_lit = function VVar x -> AVar x | VConst c -> AC c

let mk_term_gen env op args e =
  let nty = _get_force [%here] env.event_tyctx op in
  term_concat (CGen { op = op#:nty; args = List.map value_to_tvalue args }) e

let mk_term_assertP prop e =
  if is_true prop then e else term_concat (CAssertP prop) e

let mk_term_assume args prop e =
  match args with
  | [] -> if is_true prop then e else _die_with [%here] "not true"
  | _ -> mk_let args (CAssume (List.map _get_ty args, prop)) e

let mk_term_obs env op args prop e =
  let nty = _get_force [%here] env.event_tyctx op in
  mk_let args (CObs { op = op#:nty; prop }) e

let rctx_to_prefix rctx =
  List.fold_right
    (fun x (qvs, prop) ->
      let x' = x.x#:x.ty.nty in
      let phi = subst_prop_instance default_v (AVar x') x.ty.phi in
      (x' :: qvs, smart_add_to phi prop))
    (ctx_to_list rctx) ([], mk_true)

let destruct_pat_inner loc r =
  let rec aux r =
    match r with
    | RtyInter _ -> _die loc
    | RtyGArr _ -> _die_with loc "never"
    | RtyBase _ | RtyHAF _ | RtyHAParallel _ -> ([], r)
    | RtyArr { argcty; retrty; arg } ->
        let args, t = aux retrty in
        ((arg#:argcty) :: args, t)
  in
  aux r

let destruct_pat loc r =
  let rec aux r =
    match r with
    | RtyInter _ -> _die loc
    | RtyBase _ | RtyHAF _ | RtyHAParallel _ | RtyArr _ ->
        ([], destruct_pat_inner loc r)
    | RtyGArr { argnty; retrty; arg } ->
        let args, t = aux retrty in
        ((arg#:argnty) :: args, t)
  in
  aux r

let destruct_hap loc = function
  | RtyHAParallel { history; adding_se; parallel } ->
      (history, adding_se, parallel)
  | _ -> _die loc

let subst_cty name lit cty =
  { cty with phi = subst_prop_instance name lit cty.phi }

let subst_raw_sregex name lit r =
  map_regex (SFA.CharSet.map (subst_sevent_instance name lit)) r

let subst_pat name lit t =
  let rec aux = function
    | RtyBase cty -> RtyBase (subst_cty name lit cty)
    | RtyHAF { history; adding; future } ->
        let history, adding, future =
          map3 (subst_raw_sregex name lit) (history, adding, future)
        in
        RtyHAF { history; adding; future }
    | RtyHAParallel { history; adding_se; parallel } ->
        let history = subst_raw_sregex name lit history in
        let adding_se = subst_sevent_instance name lit adding_se in
        let parallel = List.map (subst_sevent_instance name lit) parallel in
        RtyHAParallel { history; adding_se; parallel }
    | RtyArr { arg; argcty; retrty } ->
        if String.equal arg name then RtyArr { arg; argcty; retrty }
        else
          RtyArr
            { arg; argcty = subst_cty name lit argcty; retrty = aux retrty }
    | RtyGArr { arg; argnty; retrty } ->
        if String.equal arg name then RtyGArr { arg; argnty; retrty }
        else RtyGArr { arg; argnty; retrty = aux retrty }
    | RtyInter (t1, t2) -> RtyInter (aux t1, aux t2)
  in
  aux t

let map_cty (f : Nt.t -> Nt.t) ({ nty; phi } : cty) =
  { nty = f nty; phi = map_prop f phi }

let map_pat (mapr : (Nt.t -> Nt.t) -> 'r -> 'r) (f : Nt.t -> Nt.t) (t : 'r pat)
    =
  let rec aux = function
    | RtyBase cty -> RtyBase (map_cty f cty)
    | RtyHAF { history; adding; future } ->
        let history, adding, future = map3 (mapr f) (history, adding, future) in
        RtyHAF { history; adding; future }
    | RtyHAParallel { history; adding_se; parallel } ->
        let history = (mapr f) history in
        let adding_se = map_sevent f adding_se in
        let parallel = List.map (map_sevent f) parallel in
        RtyHAParallel { history; adding_se; parallel }
    | RtyArr { arg; argcty; retrty } ->
        if String.equal arg arg then RtyArr { arg; argcty; retrty }
        else RtyArr { arg; argcty = map_cty f argcty; retrty = aux retrty }
    | RtyGArr { arg; argnty; retrty } ->
        if String.equal arg arg then RtyGArr { arg; argnty; retrty }
        else RtyGArr { arg; argnty; retrty = aux retrty }
    | RtyInter (t1, t2) -> RtyInter (aux t1, aux t2)
  in
  aux t

let rec fresh_pat t =
  match t with
  | RtyBase _ | RtyHAF _ | RtyHAParallel _ -> t
  | RtyArr { arg; argcty; retrty } ->
      let arg' = Rename.unique_var arg in
      let retrty = subst_pat arg (AVar arg'#:argcty.nty) retrty in
      RtyArr { arg = arg'; argcty; retrty = fresh_pat retrty }
  | RtyGArr { arg; argnty; retrty } ->
      let arg' = Rename.unique_var arg in
      let retrty = subst_pat arg (AVar arg'#:argnty) retrty in
      RtyGArr { arg = arg'; argnty; retrty = fresh_pat retrty }
  | RtyInter (t1, t2) -> RtyInter (fresh_pat t1, fresh_pat t2)

open Zdatatype

let layout_trace_elem (op, args) =
  spf "%s(%s)" op (List.split_by_comma layout_constant args)

let layout_trace = List.split_by "; " layout_trace_elem
