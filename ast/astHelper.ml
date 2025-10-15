open ParseTree
open Zdatatype

(** Core Language *)

let layout_typed_var { x; ty } = spf "%s:%s" x (Nt.layout_nt ty)
let layout_typed_var_list l = List.split_by_comma layout_typed_var l

let value_to_nt = function
  | VVar x -> x.ty
  | VConst c -> constant_to_nt c
  | VCStlcTy _ -> mk_p_abstract_ty "stlcTy"
  | VCIntList _ -> Ty_constructor ("list", [ Nt.int_ty ])

let value_to_tvalue v = v#:(value_to_nt v)

let value_to_lit = function
  | VVar x -> AVar x
  | VConst c -> AC c
  | VCStlcTy _ | VCIntList _ ->
      _die_with [%here] "stlc constant cannot be converted into literal"

let mk_value_tt = (VConst U)#:Nt.unit_ty
let mk_term_tt = CVal mk_value_tt
let mk_value_const c = VConst c
let mk_value_int n = mk_value_const (I n)
let mk_value_bool b = mk_value_const (B b)
let mk_value_string s = mk_value_const (S s)
let mk_value_stlcTy ty = VCStlcTy ty
let mk_value_intList xs = VCIntList xs
let mk_list_ty ty = Nt.Ty_constructor ("list", [ ty ])

let term_to_nt = function
  | CVal v -> v.ty
  | CLetE { body; _ } -> body.ty
  | CAppOp { op; _ } -> snd @@ Nt.destruct_arr_tp op.ty
  | CObs { op; _ } -> snd @@ Nt.destruct_arr_tp op.ty
  | CGen _ | CUnion _ | CAssertP _ -> Nt.unit_ty
  (* | CWhile _ | KStar _ -> Nt.unit_ty *)
  | CFix _ | CFixApp _ -> Nt.unit_ty
  | CAssume (nts, _) -> Nt.Ty_tuple nts

let term_to_tterm t = t#:(term_to_nt t)

let mk_let lhs rhs body =
  let ty =
    match lhs with
    | [] -> Nt.unit_ty
    | [ x ] -> x.ty
    | _ -> Nt.Ty_tuple (List.map _get_ty lhs)
  in
  CLetE { lhs; rhs = rhs#:ty; body = body#:(term_to_nt body) }

let mk_let_ rhs body =
  mk_let [ (Rename.unique_var "dummy")#:Nt.unit_ty ] rhs body

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

let mk_term_union e1 e2 = CUnion [ e1#:Nt.unit_ty; e2#:Nt.unit_ty ]

let mk_term_assume args prop e =
  match args with
  | [] -> if is_true prop then e else _die_with [%here] "not true"
  | _ -> mk_let args (CAssume (List.map _get_ty args, prop)) e

let mk_term_obs env op args prop e =
  let nty = _get_force [%here] env op in
  mk_let args (CObs { op = op#:nty; prop }) e

let mk_term_obs_prop_fresh ctx op k =
  let nty = _get_force [%here] ctx op in
  let args = Nt.get_record_types nty in
  let args = List.map (fun x -> (Rename.unique_var x.x)#:x.ty) args in
  let prop, e = k args in
  mk_term_obs ctx op args prop e

let mk_term_obs_fresh ctx op k =
  let k = fun args -> (mk_true, k args) in
  mk_term_obs_prop_fresh ctx op k

let mk_term_assume_fresh nty prop k =
  let arg = (Rename.unique_var "x")#:nty in
  mk_term_assume [ arg ] (prop arg) (k arg)

let mk_term_assume_fresh_true ctx k =
  mk_term_assume_fresh ctx (fun _ -> mk_true) k

let mk_term_assume_fresh_neq ctx args k =
  match args with
  | [] -> mk_term_assume_fresh_true ctx k
  | _ ->
      let prop y =
        And
          (List.map
             (fun x -> Not (lit_to_prop (mk_var_eq_var [%here] x y)))
             args)
      in
      mk_term_assume_fresh ctx prop k

let mk_term_assume_fresh_geq_zero ctx k =
  let prop y =
    lit_to_prop
      (AAppOp
         ( ">="#:Nt.(construct_arr_tp ([ int_ty; int_ty ], bool_ty)),
           [ lit_to_tlit (AVar y); lit_to_tlit (AC (I 0)) ] ))
  in
  mk_term_assume_fresh ctx prop k

let mk_term_assume_fresh_geq_zero_lt_x ctx x k =
  let prop1 y =
    lit_to_prop
      (AAppOp
         ( ">="#:Nt.(construct_arr_tp ([ int_ty; int_ty ], bool_ty)),
           [ lit_to_tlit (AVar y); lit_to_tlit (AC (I 0)) ] ))
  in
  let prop2 y =
    lit_to_prop
      (AAppOp
         ( ">"#:Nt.(construct_arr_tp ([ int_ty; int_ty ], bool_ty)),
           [ lit_to_tlit (AVar x); lit_to_tlit (AVar y) ] ))
  in
  mk_term_assume_fresh ctx (fun y -> And [ prop1 y; prop2 y ]) k

let mk_term_assume_fresh_eq_one ctx k =
  let prop y =
    lit_to_prop
      (AAppOp
         ( "=="#:Nt.(construct_arr_tp ([ int_ty; int_ty ], bool_ty)),
           [ lit_to_tlit (AVar y); lit_to_tlit (AC (I 0)) ] ))
  in
  mk_term_assume_fresh ctx prop k

(* let mk_while_term body cond = CWhile { body = body#:(term_to_nt body); cond }

let mk_kleene_while body =
  let x = (Rename.unique_var "cond")#:Nt.bool_ty in
  let boolgen = mk_term_assume [ x ] mk_true mk_term_tt in
  let body = term_concat body boolgen in
  mk_while_term body (lit_to_prop (mk_var_eq_c [%here] x (B true))) *)

let mk_rec retBranch recBranch e =
  let f =
    CFix
      {
        retBranch = retBranch#:(term_to_nt retBranch);
        recBranch = recBranch#:(term_to_nt recBranch);
      }
  in
  CLetE
    {
      lhs = [ (Rename.unique_var "dummy")#:Nt.unit_ty ];
      rhs = f#:Nt.unit_ty;
      body = e#:Nt.unit_ty;
    }

let mk_rec_app iterV boundV =
  CFixApp
    {
      cfix = None;
      iterV = term_to_tterm iterV;
      boundV = value_to_tvalue boundV;
    }

let mk_rec_app_0 e =
  let open Nt in
  mk_term_assume_fresh_geq_zero int_ty (fun x ->
      mk_let_ (mk_rec_app (CVal (mk_value_const (I 0))#:Nt.int_ty) (VVar x)) e)

let mk_iter = value_to_tvalue (VVar default_iter_var)
let plus_op = "+"#:Nt.(mk_arr Nt.int_ty @@ mk_arr Nt.int_ty Nt.int_ty)
let minus_op = "-"#:Nt.(mk_arr Nt.int_ty @@ mk_arr Nt.int_ty Nt.int_ty)

let mk_incr_raw v =
  CAppOp
    {
      op = plus_op;
      args = [ v#:Nt.int_ty; value_to_tvalue (mk_value_const (I 1)) ];
    }

let mk_minus_raw v =
  CAppOp
    {
      op = minus_op;
      args = [ v#:Nt.int_ty; value_to_tvalue (mk_value_const (I 1)) ];
    }

let mk_incr v k =
  let arg = (Rename.unique_var "x")#:Nt.int_ty in
  mk_let [ arg ] (mk_incr_raw v) (k arg)

let mk_minus v k =
  let arg = (Rename.unique_var "x")#:Nt.int_ty in
  mk_let [ arg ] (mk_minus_raw v) (k arg)

let mk_rec_app_incr e =
  let iterV_plus1 = mk_incr_raw (VVar default_iter_var) in
  mk_let_ (mk_rec_app iterV_plus1 (VVar default_bound_var)) e

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
  | RtyHAF { history; adding; future } -> (history, adding, future)
  | _ -> _die loc

(** Type Context *)
let rctx_to_prefix rctx =
  List.fold_right
    (fun x (qvs, prop) ->
      let x' = x.x#:x.ty.nty in
      let phi = subst_prop_instance default_v (AVar x') x.ty.phi in
      (x' :: qvs, smart_add_to phi prop))
    (ctx_to_list rctx) ([], mk_true)

(* Plan *)
(* type plan = {
  freeVars : (Nt.nt, string) typed list; (* extential variables *)
  line : line; (* one linear sequential code *)
  actMap : int ActMap.t; (* the map from act to id *)
  checkedActs : int list IntMap.t; (* the acts within the task to type check *)
} *)

let _get_freeVars line =
  List.slow_rm_dup (fun x y -> String.equal x.x y.x)
  @@ List.concat_map
       (function LineAct { aargs; _ } -> aargs | _ -> [])
       line.elems

let _get_aids line =
  List.concat_map
    (function LineAct { aid; _ } -> [ aid ] | _ -> [])
    line.elems

let _get_checkedActs line =
  List.fold_right
    (fun x acc ->
      match x with
      | LineAct { aid = Some aid; achildren = Some ids; _ } ->
          IntMap.add aid ids acc
      | _ -> acc)
    line.elems IntMap.empty

let msubst_lit m = msubst subst_lit_instance m

let subst_value_with_value x value = function
  | VVar y -> if String.equal x y.x then value else VVar y
  | VConst c -> VConst c
  | VCStlcTy ty -> VCStlcTy ty
  | VCIntList xs -> VCIntList xs

let lit_to_value loc = function
  | AVar x -> VVar x
  | AC c -> VConst c
  | _ -> _die loc

let subst_name_in_line_elem x z r =
  let open AutomataLibrary.SFA in
  match r with
  | LineAct { aid; aop; aargs; aparent; achildren; tmp } ->
      LineAct
        {
          aid;
          aop;
          aargs = List.map (subst_name_qv x z) aargs;
          aparent;
          achildren;
          tmp;
        }
  (* | LineStar r -> LineStar (subst_regex_instance x (AVar z) r) *)
  (* | LineMultiChar cs ->
      LineMultiChar
        (CharSet.map (AutomataLibrary.subst_sevent_instance x (AVar z)) cs) *)
  | LineStarMultiChar cs ->
      LineStarMultiChar
        (CharSet.map (AutomataLibrary.subst_sevent_instance x (AVar z)) cs)

let subst_name_in_line x z line =
  { line with elems = List.map (subst_name_in_line_elem x z) line.elems }

let subst_plan x z = List.map (subst_name_in_line x z)

let comple_cs cs cs' =
  let open AutomataLibrary.SFA in
  let cs =
    CharSet.filter_map
      (fun { op; vs; phi } ->
        let phis =
          CharSet.fold
            (fun se' phis ->
              if String.equal op se'.op then se'.phi :: phis else phis)
            cs' []
        in
        let phi = smart_add_to phi (smart_not (smart_or phis)) in
        Some { op; vs; phi })
      cs
  in
  cs

let msubst_prop (vars1, vars2) prop =
  let m =
    List.map (fun (x, y) -> (x.x, AVar y)) @@ _safe_combine [%here] vars1 vars2
  in
  msubst subst_prop_instance m prop

let prop_to_conjuncts phi =
  let rec aux prop =
    match prop with
    | Lit x -> [ Lit x ]
    | And xs -> List.concat_map aux xs
    | _ -> [ prop ]
  in
  aux phi
