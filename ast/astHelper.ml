open ParseTree
open Zdatatype

(** Core Language *)

let value_to_nt = function
  | VVar x -> x.ty
  | VConst c -> constant_to_nt c
  | VCStlcTy _ -> mk_p_abstract_ty "stlcTy"

let value_to_tvalue v = v#:(value_to_nt v)

let value_to_lit = function
  | VVar x -> AVar x
  | VConst c -> AC c
  | VCStlcTy _ ->
      _die_with [%here] "stlc constant cannot be converted into literal"

let mk_value_tt = (VConst U)#:Nt.unit_ty
let mk_term_tt = CVal mk_value_tt
let mk_value_const c = VConst c
let mk_value_int n = mk_value_const (I n)
let mk_value_bool b = mk_value_const (B b)
let mk_value_string s = mk_value_const (S s)
let mk_value_stlcTy ty = VCStlcTy ty

let term_to_nt = function
  | CVal v -> v.ty
  | CLetE { body; _ } -> body.ty
  | CAppOp { op; _ } -> snd @@ Nt.destruct_arr_tp op.ty
  | CObs { op; _ } -> snd @@ Nt.destruct_arr_tp op.ty
  | CGen _ | CUnion _ | CAssertP _ | CWhile _ -> Nt.unit_ty
  | CAssume (nts, _) -> Nt.Ty_tuple nts

let mk_let lhs rhs body =
  let ty =
    match lhs with
    | [] -> Nt.unit_ty
    | [ x ] -> x.ty
    | _ -> Nt.Ty_tuple (List.map _get_ty lhs)
  in
  CLetE { lhs; rhs = rhs#:ty; body = body#:(term_to_nt body) }

let rec term_concat term body =
  match term with
  | CLetE { lhs; rhs; body = { x = CVal { x = VConst U; _ }; _ } } ->
      CLetE { lhs; rhs; body = body#:(term_to_nt body) }
  | CLetE { lhs; rhs; body = e } ->
      let body = term_concat e.x body in
      CLetE { lhs; rhs; body = body#:(term_to_nt body) }
  | _ -> CLetE { lhs = []; rhs = term#:Nt.unit_ty; body = body#:Nt.unit_ty }

let mk_term_gen env op args e =
  let nty = _get_force [%here] env op in
  term_concat (CGen { op = op#:nty; args = List.map value_to_tvalue args }) e

let mk_term_assertP prop e =
  if is_true prop then e else term_concat (CAssertP prop) e

let mk_term_assume args prop e =
  match args with
  | [] -> if is_true prop then e else _die_with [%here] "not true"
  | _ -> mk_let args (CAssume (List.map _get_ty args, prop)) e

let mk_term_obs env op args prop e =
  let nty = _get_force [%here] env op in
  mk_let args (CObs { op = op#:nty; prop }) e

let mk_term_obs_fresh ctx op k =
  let nty = _get_force [%here] ctx op in
  let args = Nt.get_record_types nty in
  let args = List.map (fun x -> (Rename.unique_var x.x)#:x.ty) args in
  mk_term_obs ctx op args mk_true (k args)

let mk_term_assume_fresh nty prop k =
  let arg = (Rename.unique_var "x")#:nty in
  mk_term_assume [ arg ] prop (k arg)

let mk_while_term body cond = CWhile { body = body#:(term_to_nt body); cond }

let mk_kleene_while body =
  let x = (Rename.unique_var "cond")#:Nt.bool_ty in
  let boolgen = mk_term_assume [ x ] mk_true mk_term_tt in
  let body = term_concat body boolgen in
  mk_while_term body (lit_to_prop (mk_var_eq_c [%here] x (B true)))

(** Trace Language *)

let layout_trace_elem { op; args } =
  spf "%s(%s)" op (List.split_by_comma layout_constant args)

let layout_trace = List.split_by "; " layout_trace_elem

(** Refinement Types *)

(* Erasure *)

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

(* Freshness *)

let rec fresh_srl_pat t =
  match t with
  | RtyBase _ | RtyHAF _ | RtyHAParallel _ -> t
  | RtyArr { arg; argcty; retrty } ->
      let arg' = Rename.unique_var arg in
      let retrty =
        Subst.subst_srl_pat_instance arg (AVar arg'#:argcty.nty) retrty
      in
      RtyArr { arg = arg'; argcty; retrty = fresh_srl_pat retrty }
  | RtyGArr { arg; argnty; retrty } ->
      let arg' = Rename.unique_var arg in
      let retrty =
        Subst.subst_srl_pat_instance arg (AVar arg'#:argnty) retrty
      in
      RtyGArr { arg = arg'; argnty; retrty = fresh_srl_pat retrty }
  | RtyInter (t1, t2) -> RtyInter (fresh_srl_pat t1, fresh_srl_pat t2)

let qv_to_cqv { x; ty } = { x; ty = { nty = ty; phi = mk_true } }
let mk_haf (history, adding, future) = RtyHAF { history; adding; future }

let mk_inter_type l =
  match l with
  | [] -> _die [%here]
  | h :: t -> List.fold_left (fun x y -> RtyInter (x, y)) h t

let rec is_singleton_pat = function
  | RtyBase _ | RtyHAF _ | RtyHAParallel _ -> true
  | RtyGArr { retrty; _ } | RtyArr { retrty; _ } -> is_singleton_pat retrty
  | RtyInter _ -> false

let rec pat_to_triple = function
  | RtyInter (t1, t2) -> pat_to_triple t1 @ pat_to_triple t2
  | _ as r ->
      if is_singleton_pat r then [ r ]
      else _die_with [%here] "not a well-formed HAF type"

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

(** Type Context *)
let rctx_to_prefix rctx =
  List.fold_right
    (fun x (qvs, prop) ->
      let x' = x.x#:x.ty.nty in
      let phi = subst_prop_instance default_v (AVar x') x.ty.phi in
      (x' :: qvs, smart_add_to phi prop))
    (ctx_to_list rctx) ([], mk_true)
