(* include Common *)
include Language
open Zdatatype

(* let check_valid_feature (qvs, gprop) p =
  let aux p =
    let q = smart_forall qvs @@ smart_implies gprop p in
    let () = Pp.printf "@{<bold>check_valid_feature:@}\n%s\n" (layout_prop q) in
    Prover.check_valid (None, q)
  in
  (not (aux (lit_to_prop p))) && not (aux @@ Not (lit_to_prop p)) *)

let pre_simplify_lit (qvs, pre) lit =
  let q1 = smart_forall qvs @@ smart_implies pre (lit_to_prop lit) in
  let q2 = smart_forall qvs @@ smart_implies pre (Not (lit_to_prop lit)) in
  (* let () = Pp.printf "@{<bold>pre_simplify_lit:@}\n%s\n" (layout_prop q1) in *)
  (* let () = Pp.printf "@{<bold>pre_simplify_lit:@}\n%s\n" (layout_prop q2) in *)
  match (Prover.check_valid (None, q1), Prover.check_valid (None, q2)) with
  | true, true -> _die [%here]
  | true, false -> mk_true
  | false, true -> mk_false
  | false, false -> lit_to_prop lit

let pre_simplify_prop (qvs, pre) prop =
  let rec aux = function
    | Lit lit -> pre_simplify_lit (qvs, pre) lit.x
    | Not p -> Not (aux p)
    | And lits -> smart_and (List.map aux lits)
    | Or lits -> smart_or (List.map aux lits)
    | Implies (p1, p2) -> smart_implies (aux p1) (aux p2)
    | Ite _ -> _die [%here]
    | Iff (p1, p2) -> smart_iff (aux p1) (aux p2)
    | Forall _ | Exists _ -> _die [%here]
  in
  aux prop

let check_valid_feature (_, gprop) p =
  let q = smart_add_to (lit_to_prop p) gprop in
  (* let () = Pp.printf "@{<bold>check_valid_feature:@}\n%s\n" (layout_prop q) in *)
  Prover.check_sat_bool (None, q)

let check_valid_pre prop = not (Prover.check_valid (None, prop))

let build_features _ (lits : Nt.nt lit list) =
  (* let lits = List.filter (check_valid_feature (qvs, gprop)) lits in *)
  let fvs = List.init (List.length lits) (fun _ -> [ true; false ]) in
  let fvs = List.choose_list_list fvs in
  let fvs =
    List.map
      (fun l ->
        smart_and
          (List.mapi
             (fun idx x ->
               let lit = lit_to_prop (List.nth lits idx) in
               if x then lit else Not lit)
             l))
      fvs
  in
  (* let fvs = List.filter check_valid_pre fvs in *)
  fvs

(* let build_fvtab env lits =
  let () =
    Pp.printf "@{<bold>lits:@} %s\n" (List.split_by_comma layout_typed_lit lits)
  in
  (* Remove boolean constants *)
  let lits =
    List.filter (function { x = AC (B _); _ } -> false | _ -> true) lits
  in
  let bvars, lits =
    List.partition
      (function
        | { x = AVar x; _ } when Nt.equal_nt x.ty Nt.bool_ty -> true
        | _ -> false)
      lits
  in
  let additional =
    match get_opt env.tyctx ">" with
    | None -> []
    | Some _ ->
        (* if true then [] *)
        (* else *)
        let int_lits =
          List.filter (fun lit -> Nt.equal_nt Nt.int_ty lit.ty) lits
        in
        let () =
          Pp.printf "@{<bold>int lits:@} %s\n"
            (List.split_by_comma layout_typed_lit int_lits)
        in
        let pairs = List.combination_l int_lits 2 in
        let ltlits =
          let lt = ">"#:Nt.(construct_arr_tp ([ int_ty; int_ty ], bool_ty)) in
          List.map
            (fun l ->
              match l with
              | [ x; y ] -> AAppOp (lt, [ x; y ])
              | _ -> _die [%here])
            pairs
        in
        let () =
          Pp.printf "@{<bold>ltlits:@} %s\n"
            (List.split_by_comma layout_lit ltlits)
        in
        ltlits
  in
  let bvars = List.map _get_x bvars in
  let res = bvars @ Rawdesym.mybuild_euf lits @ additional in
  let () =
    Pp.printf "@{<bold>build_fvtab:@} %s\n" (List.split_by_comma layout_lit res)
  in
  res *)

let mk_eq_features (abd_vars : (Nt.nt, string) typed list) =
  let l = List.combination_l abd_vars 2 in
  let l =
    List.filter_map
      (function
        | [ x; y ] ->
            if Nt.equal_nt x.ty y.ty && not (String.equal x.x y.x) then
              Some (mk_var_eq_var [%here] x y)
            else None
        | _ -> _die_with [%here] "never")
      l
  in
  l

let mk_b_features (abd_vars : (Nt.nt, string) typed list) =
  List.filter_map
    (fun x -> if Nt.equal_nt Nt.bool_ty x.ty then Some (AVar x) else None)
    abd_vars

let mk_basic_features (abd_vars : (Nt.nt, string) typed list) =
  let l1 = mk_eq_features abd_vars in
  let l2 = mk_b_features abd_vars in
  l1 @ l2

let mk_fvtab (qvs, abd_vars, gprop) =
  let _ = Prop.get_lits gprop in
  (* let () =
    Pp.printf "@{<bold>qvs:@}\n%s\n" (List.split_by_comma layout_qv qvs)
  in
  let () =
    Pp.printf "@{<bold>abd_vars:@}\n%s\n"
      (List.split_by_comma layout_qv abd_vars)
  in *)
  let lits = mk_basic_features abd_vars in
  (* let () =
    Pp.printf "@{<bold>lits:@}\n%s\n" (List.split_by_comma layout_lit lits)
  in *)
  build_features (qvs, gprop) lits

let do_abduction (qvs, abd_vars, gprop) =
  let fvs = mk_fvtab (qvs, abd_vars, gprop) in
  (* let () =
    Pp.printf "@{<bold>feature vectors:@}\n%s\n"
      (List.split_by_comma layout_prop fvs)
  in *)
  match fvs with
  | [] -> mk_true
  | _ ->
      (* let () =
        Pp.printf "@{<bold>fvs:@}\n%s\n" (List.split_by_comma layout_prop fvs)
      in *)
      let fvs =
        List.filter
          (fun p -> Prover.check_sat_bool (None, smart_add_to p gprop))
          fvs
      in
      smart_or fvs
(* 
let mk_abd_prop fvs =
  match fvs with [] -> None | _ -> Some (simp_fvec_prop @@ smart_or fvs)

let mk_raw_all env =
  let l =
    List.map (fun x ->
        match x.ty with
        | Nt.Ty_record { fds; _ } -> { op = x.x; vs = fds; phi = mk_true }
        | _ -> _die_with [%here] "never")
    @@ ctx_to_list env.event_tyctx
  in
  if List.length l == 0 then _die [%here] else SFA.CharSet.of_list l

let check_regex_nonempty _ _ r =
  let really_do_check_regex_nonempty () =
    let _, r' =
      _die_with [%here] "unimp"
      (* Rawdesym.desymbolic_symbolic_rewregex env.tyctx env.event_tyctx (bprop, r) *)
    in
    let () =
      Pp.printf "@{<bold>check_regex_nonempty@}: %s\n" (SFA.layout_regex r)
    in
    let () =
      Pp.printf "@{<bold>check_regex_nonempty@}: %s\n" (DesymFA.layout_regex r')
    in
    not @@ DesymFA.emptiness r'
  in
  really_do_check_regex_nonempty ()

let abduction_automata env { bvs; bprop } (a : SFA.CharSet.t regex) abd_vars =
  let really_do_abduction () =
    let lits = Rawdesym.mk_global_ftab env.tyctx (bvs @ abd_vars, bprop, a) in
    let lits =
      List.filter
        (fun lit ->
          not
            (List.is_empty
            @@ List.interset String.equal (fv_lit_id lit)
                 (List.map _get_x abd_vars)))
        (build_fvtab env @@ List.map (fun l -> l#:(lit_to_nt l)) lits)
    in
    let fvs = build_features { bvs; bprop } (abd_vars, lits) in
    let () =
      Pp.printf "@{<bold>abduction_automata fvs:@}: %i\n" (List.length fvs)
    in
    (* let checker gamma (_, prop) = check_valid gamma prop in *)
    let fvs =
      do_abduction { bvs; bprop }
        (fun gamma ->
          let () = Pp.printf "@{<bold>gamma@}: %s\n" (Gamma.layout gamma) in
          let res = check_regex_nonempty env gamma a in
          let () =
            Pp.printf "@{<bold>@{<yellow>abduction_automata@}@}: %b\n" res
          in
          res)
        (abd_vars, fvs)
    in
    (* let () = if List.length fvs > 1 then _die [%here] in *)
    fvs
  in
  Stat.stat_nonempty_check really_do_abduction

let abduction_plan _ _ _ _ = _die_with [%here] "unimp"

let abduction_mid_goal env gamma (plan1, elem, plan2) abd_vars =
  let plan = plan1 @ [ elem ] @ plan2 in
  let fvs = abduction_plan env gamma plan abd_vars in
  match fvs with
  | [] ->
      let () =
        if String.equal (Plan.elem_to_se env.event_tyctx elem).op "putReq" then
          _die [%here]
      in
      None
  | _ ->
      Some
        {
          bvs = gamma.bvs @ abd_vars;
          bprop = smart_add_to (smart_or fvs) gamma.bprop;
        } *)
