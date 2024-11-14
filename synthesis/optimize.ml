open Language
open Common
open Gamma
open AutomataLibrary

let eliminate_buffer_elem ({ bvs; bprop }, elem) =
  match elem with
  | PlanActBuffer { op; args; phi } ->
      let elem = PlanAct { op; args } in
      let bprop = smart_add_to phi bprop in
      ({ bvs; bprop }, elem)
  | _ as elem -> ({ bvs; bprop }, elem)

let eliminate_buffer_plan_aux (gamma, plan) =
  List.fold_left
    (fun (gamma, plan) elem ->
      let gamma, elem = eliminate_buffer_elem (gamma, elem) in
      (gamma, plan @ [ elem ]))
    (gamma, []) plan

let eliminate_buffer_plan_goal { gamma; plan; pg; solved } =
  let gamma, plan = eliminate_buffer_plan_aux (gamma, plan) in
  { gamma; plan; pg; solved }

let eliminate_buffer_plan_mid_goal { gamma; pre; mid; post; pg; solved } =
  let gamma, pre = eliminate_buffer_plan_aux (gamma, pre) in
  let gamma, mid = eliminate_buffer_elem (gamma, mid) in
  let gamma, post = eliminate_buffer_plan_aux (gamma, post) in
  { gamma; pre; mid; post; pg; solved }

let eliminate_buffer_plan_pair_goal
    { gamma; preSolved; postUnsolved; pg; solved } =
  let gamma, preSolved = eliminate_buffer_plan_aux (gamma, preSolved) in
  let gamma, postUnsolved = eliminate_buffer_plan_aux (gamma, postUnsolved) in
  { gamma; preSolved; postUnsolved; pg; solved }

let lit_to_equation = function
  | AAppOp (op, [ a; b ]) when String.equal eq_op op.x ->
      if is_var_c a.x && is_var_c b.x then Some (a.x, b.x) else None
  | _ -> None

module ConstMap = Map.Make (struct
  type t = constant

  let compare = compare_constant
end)

open Zdatatype

let eq_const_optimize lits =
  let eqs =
    List.filter_map
      (function
        | AAppOp (op, [ { x = AVar x; _ }; { x = AC c; _ } ])
          when String.equal "==" op.x ->
            Some (x, c)
        | _ -> None)
      lits
  in
  let m =
    List.fold_right
      (fun (x, c) ->
        ConstMap.update c (function
          | None -> Some [ x ]
          | Some l -> Some (x :: l)))
      eqs ConstMap.empty
  in
  let eq_var x y = String.equal x.x y.x in
  let m = ConstMap.map (List.slow_rm_dup eq_var) m in
  let m =
    ConstMap.map
      (fun l ->
        let ps = List.c_n_2 l in
        let ps = List.filter (fun (x, y) -> not (eq_var x y)) ps in
        List.map (fun (x, y) -> mk_lit_eq_lit x.ty (AVar x) (AVar y)) ps)
      m
  in
  let res = ConstMap.fold (fun _ l res -> l @ res) m [] in
  let () =
    _log "syn" @@ fun _ ->
    Pp.printf "@{<bold>new eqs:@} %s\n" @@ List.split_by_comma layout_lit res
  in
  res

let eq_in_prop_to_subst_map { bvs; bprop } =
  let conjs = to_conjs bprop in
  let lits = List.filter_map to_lit_opt conjs in
  let lits = lits @ eq_const_optimize lits in
  let eqs = List.filter_map lit_to_equation lits in
  let eqvs =
    List.filter_map (function AVar x, AVar y -> Some (x, y) | _ -> None) eqs
  in
  let subst_eqs x lit = List.map (map2 (subst_name_qv x lit)) in
  let rec aux (eqs, res) =
    match eqs with
    | [] -> res
    | (x, y) :: eqs ->
        let comp a b =
          let c = compare (String.length a) (String.length b) in
          if c == 0 then compare a b else c
        in
        let c = comp x.x y.x in
        if c == 0 then aux (eqs, res)
        else if c > 0 then
          let res = res @ [ (x.x, y) ] in
          let eqs = subst_eqs x.x y eqs in
          aux (eqs, res)
        else
          let res = res @ [ (y.x, x) ] in
          let eqs = subst_eqs y.x x eqs in
          aux (eqs, res)
  in
  let m = aux (eqvs, []) in
  let prop =
    List.fold_right (fun (x, lit) -> subst_prop_instance x (AVar lit)) m bprop
  in
  let bvs =
    List.filter
      (fun x -> not (List.exists (fun (y, _) -> String.equal x.x y) m))
      bvs
  in
  let bprop = simpl_eq_in_prop prop in
  ({ bvs; bprop }, m)

let optimize_back_goal_aux { gamma; pre; mid; post; pg; solved } =
  let gamma = Gamma.simplify gamma in
  let gamma, m = eq_in_prop_to_subst_map gamma in
  let pre, post = map2 (msubst Plan.subst_plan m) (pre, post) in
  let mid = msubst Plan.subst_elem m mid in
  let pg = List.map (msubst Plan.subst_elem m) pg in
  let solved = List.map (msubst Plan.subst_elem m) solved in
  let goal' = { gamma; pre; mid; post; pg; solved } in
  (m, goal')

let optimize_goal { gamma; plan; pg; solved } =
  let gamma = Gamma.simplify gamma in
  let gamma, m = eq_in_prop_to_subst_map gamma in
  let pg = List.map (msubst Plan.subst_elem m) pg in
  let solved = List.map (msubst Plan.subst_elem m) solved in
  { gamma; plan = msubst Plan.subst_plan m plan; pg; solved }

let optimize_back_goal goal =
  let m, goal' = optimize_back_goal_aux goal in
  let p goal () = simp_print_mid goal in
  let () = simp_print_opt_judgement (p goal) m (p goal') in
  goal'

let optimize_back_goal_with_args goal args =
  let m, goal' = optimize_back_goal_aux goal in
  let args' =
    List.filter
      (fun x -> not (List.exists (fun (y, _) -> String.equal x.x y) m))
      args
  in
  let p goal () = simp_print_mid goal in
  let () = simp_print_opt_judgement (p goal) m (p goal') in
  (goal', args')

let optimize_back_goal_with_args_record goal args record =
  let m, goal' = optimize_back_goal_aux goal in
  let args' =
    List.filter
      (fun x -> not (List.exists (fun (y, _) -> String.equal x.x y) m))
      args
  in
  let () = record := msubst Plan.subst_elem m !record in
  let p goal () = simp_print_mid goal in
  let () = simp_print_opt_judgement (p goal) m (p goal') in
  (goal', args')

(** optimize prop *)

let to_fvec_lit = function
  | Lit lit -> Some (lit.x, true)
  | Not (Lit lit) -> Some (lit.x, false)
  | _ -> None

let to_fvec_clauze prop =
  let rec aux = function
    | And l ->
        let l' = List.concat @@ List.filter_map aux l in
        if List.length l' == List.length l then Some l' else None
    | _ as p ->
        let* cell = to_fvec_lit p in
        Some [ cell ]
  in
  aux prop

let to_fvec_dnf =
  let rec aux = function
    | Or l ->
        let l' = List.concat @@ List.filter_map aux l in
        if List.length l' == List.length l then Some l' else None
    | _ as p ->
        let* cell = to_fvec_clauze p in
        Some [ cell ]
  in
  aux

module Predictable = struct
  type lit = Nt.t Prop.lit
  type prop = Nt.t Prop.prop

  let mk_true = mk_true
  let mk_false = mk_false
  let mk_lit lit = Lit lit #: Nt.Ty_bool
  let mk_ite cond bencht benchf = Ite (Lit cond #: Nt.Ty_bool, bencht, benchf)
  let mk_and = smart_add_to
  let mk_or l1 l2 = smart_or [ l1; l2 ]
  let mk_not p = Not p
  let layout_lit = layout_lit
  let layout_prop = layout_prop
end

module DT = Dtree.Dt.F (Predictable)

let simp_fvec_prop_opt prop =
  let do_sort =
    List.sort (fun a b -> compare_lit Nt.compare_nt (fst a) (fst b))
  in
  let ftab =
    List.sort (fun a b -> compare_lit Nt.compare_nt a b) @@ get_lits prop
  in
  if List.length ftab == 0 then None
  else
    let* dnf = to_fvec_dnf prop in
    let dnf' =
      List.filter_map
        (fun l ->
          let ftab', l = List.split @@ do_sort l in
          if List.equal (equal_lit Nt.equal_nt) ftab ftab' then Some l else None)
        dnf
    in
    let* pos =
      if List.length dnf == List.length dnf' then Some dnf' else None
    in
    let prop = DT.classify_as_prop (Array.of_list ftab) pos in
    Some prop

let simp_fvec_prop prop =
  match simp_fvec_prop_opt prop with Some prop -> prop | None -> prop

let simp_fvec_se { op; vs; phi } = { op; vs; phi = simp_fvec_prop phi }
let simp_fvec_cs cs = SFA.CharSet.map simp_fvec_se cs
let simp_fvec_raw_regex r = SFA.raw_reg_map simp_fvec_cs r

let simp_fvec_elem = function
  | PlanActBuffer { op; args; phi } ->
      PlanActBuffer { op; args; phi = simp_fvec_prop phi }
  | PlanAct { op; args } -> PlanAct { op; args }
  | PlanSe { op; vs; phi } -> PlanSe { op; vs; phi = simp_fvec_prop phi }
  | PlanStarInv cs -> PlanStarInv (simp_fvec_cs cs)
  | PlanStar r -> PlanStar (simp_fvec_raw_regex r)

let simp_fvec_plan = List.map simp_fvec_elem
