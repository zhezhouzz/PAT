open Zdatatype
(* open AutomataLibrary *)

(* open Common *)
open Ast

let partial_func = [ "fstTy"; "sndTy" ]

let do_rename_plan ass (gprop, prog) =
  let gprop =
    msubst subst_prop_instance (List.map (fun (x, y) -> (x, AVar y)) ass) gprop
  in
  let gprop = simpl_eq_in_prop gprop in
  let prog =
    msubst subst_term_instance (List.map (fun (x, y) -> (x, AVar y)) ass) prog
  in
  (gprop, prog)

let mk_eq_from_prev (gprop, term) =
  let select_by_ty ty lvars =
    List.filter (fun x -> Nt.equal_nt x.ty ty) lvars
  in
  let try_find_ass lvars (gprop, x) =
    let ass = select_by_ty x.ty lvars in
    (* let () =
      Pp.printf "@{<bold>[%s]ass:@} %s\n" (Nt.layout x.ty)
        (List.split_by_comma layout_typed_var ass)
    in
    let () =
      Pp.printf "@{<bold>[%s]gprop:@} %s\n" (Nt.layout x.ty) (layout_prop gprop)
    in *)
    let ass =
      List.filter_map
        (fun y ->
          let phi = mk_var_eq_var [%here] x y in
          let q = smart_add_to gprop (Not (lit_to_prop phi)) in
          if Prover.check_sat_bool (None, q) then None else Some (x, y))
        ass
    in
    match ass with [] -> None | (x, y) :: _ -> Some (x.x, y)
  in
  let rec aux lvars (gprop, term) =
    match term with
    | CLetE { lhs; rhs; body } -> (
        match rhs with
        | { x = CGen { op; args }; _ } ->
            let args =
              List.filter_map
                (fun x -> match x.x with VVar x -> Some x | _ -> None)
                args
            in
            let ass, args' =
              List.fold_left
                (fun (ass, args) x ->
                  match try_find_ass lvars (gprop, x) with
                  | Some (x, y) -> (ass @ [ (x, y) ], args @ [ y ])
                  | None -> (ass, args @ [ x ]))
                ([], []) args
            in
            let vargs =
              List.map
                (fun x ->
                  match
                    List.find_opt (fun (z, _) -> String.equal x.x z) ass
                  with
                  | Some (_, y) -> { x = VVar y; ty = x.ty }
                  | None -> { x = VVar x; ty = x.ty })
                args
            in
            let rhs = { rhs with x = CGen { op; args = vargs } } in
            let gprop, body_x = do_rename_plan ass (gprop, body.x) in
            let lvars = lvars @ args' in
            let gprop', body' = aux lvars (gprop, body_x) in
            (gprop', CLetE { lhs; rhs; body = body'#:body.ty })
        | { x = CObs { prop; _ }; _ } ->
            let lvars = lvars @ lhs in
            let gprop', body' = aux lvars (smart_add_to prop gprop, body.x) in
            (gprop', CLetE { lhs; rhs; body = body'#:body.ty })
        | _ -> _die [%here])
    | CVal _ -> (gprop, term)
    | _ -> _die [%here]
  in
  aux [] (gprop, term)

let is_gen_var x =
  if String.length x < 4 then None
  else
    let prefix = String.sub x 0 4 in
    if String.equal prefix "__x" then
      Some (int_of_string (String.sub x 4 (String.length x - 4)))
    else None

let is_obs_var x =
  if String.length x < 4 then None
  else
    let prefix = String.sub x 0 4 in
    if String.equal prefix "__y" then
      Some (int_of_string (String.sub x 4 (String.length x - 4)))
    else None

let not_valid_subst (x, y) =
  match (is_gen_var x, is_obs_var y.x) with
  | Some i, Some j -> i > j
  | _, _ -> true

let rename_plan ass (gen_vars, gprop, prog) =
  let ass =
    List.filter_map
      (fun (x, y) ->
        if List.exists (fun z -> String.equal x z) gen_vars then Some (x, y)
        else if List.exists (fun z -> String.equal y.x z) gen_vars then
          Some (y.x, x#:y.ty)
        else None)
      ass
  in
  let ass = List.filter not_valid_subst ass in
  let () =
    Pp.printf "@{<bold>ass:@} %s\n"
      (List.split_by_comma (fun (x, y) -> spf "%s -> %s" x y.x) ass)
  in
  match ass with
  | [] -> None
  | _ ->
      let _ = Prover.check_sat_bool (None, gprop) in
      let () = Pp.printf "@{<bold>gprop:@} %s\n" (layout_prop gprop) in
      let gprop, prog = do_rename_plan ass (gprop, prog) in
      let () = Pp.printf "@{<bold>gprop:@} %s\n" (layout_prop gprop) in
      let _ = Prover.check_sat_bool (None, gprop) in
      Some (gen_vars, gprop, prog)

let simp_partial_func (gen_vars, gprop, prog) =
  let ps = prop_to_conjuncts gprop in
  let lits = List.filter_map to_lit_opt ps in
  (* let () = Pp.printf "lits: %s\n" (List.split_by_comma layout_lit lits) in *)
  let pfuncs =
    List.filter_map
      (fun lit ->
        match lit with
        | AAppOp (op, [ { x = ATu args; _ } ])
          when List.exists (fun x -> String.equal x op.x) partial_func -> (
            (* let () = Pp.printf "op: %s\n" (show_lit lit) in *)
            match args with
            | [ { x = AVar x; _ }; { x = AVar y; _ } ] ->
                let () = Pp.printf "op: %s, x: %s, y: %s\n" op.x x.x y.x in
                Some ((op.x, x.x), y)
            | _ -> None)
        | _ -> None)
      lits
  in
  (* let () =
    Pp.printf "@{<bold>pfuncs:@} %s\n"
      (List.split_by_comma
         (fun ((op, x), y) -> spf "%s(%s, %s)" op x y.x)
         pfuncs)
  in *)
  let tab = Hashtbl.create 10 in
  let () =
    List.iter
      (fun ((op, x), y) ->
        match Hashtbl.find_opt tab (op, x) with
        | Some l -> Hashtbl.replace tab (op, x) (y :: l)
        | None -> Hashtbl.add tab (op, x) [ y ])
      pfuncs
  in
  (* let () =
    Hashtbl.iter
      (fun (op, x) l ->
        Printf.printf "op: %s, x: %s, l: %s\n" op x
          (List.split_by_comma (fun y -> y.x) l))
      tab
  in *)
  let ass =
    Hashtbl.fold
      (fun (_, _) l acc ->
        match l with
        | [] -> acc
        | x :: l ->
            let ass = List.map (fun y -> (x.x, y)) l in
            ass @ acc)
      tab []
  in
  rename_plan ass (gen_vars, gprop, prog)

let get_assign_names conjs vs =
  let assignments =
    List.filter_map
      (fun x ->
        let l = List.filter_map (fun prop -> is_eq_phi x prop) conjs in
        let l = List.filter_map (function AVar x -> Some x | _ -> None) l in
        match l with
        | [] -> None
        | y :: _ ->
            (* let () =
              Pp.printf "@{<bold>assignment:@} %s -> %s\n" (layout_typed_var x)
                (layout_typed_var y)
            in *)
            Some (x.x, y))
      vs
  in
  if List.length assignments == 0 then None else Some assignments

let simp_eq (gen_vars, gprop, prog) =
  let fvs = fv_prop gprop in
  match get_assign_names (prop_to_conjuncts gprop) fvs with
  | None -> None
  | Some ass ->
      let line = rename_plan ass (gen_vars, gprop, prog) in
      line

let rec simp plan =
  let isUpdated1, plan1 =
    match simp_eq plan with None -> (false, plan) | Some plan1 -> (true, plan1)
  in
  let isUpdated2, plan2 =
    match simp_partial_func plan1 with
    | None -> (false, plan1)
    | Some plan2 -> (true, plan2)
  in
  if isUpdated1 || isUpdated2 then simp plan2 else plan2

let simp_plan { gprop; elems } =
  let fvs = fv_prop gprop in
  match get_assign_names (prop_to_conjuncts gprop) fvs with
  | None -> { gprop; elems }
  | Some ass -> (
      (* let () =
        Pp.printf "@{<bold>ass:@} %s\n"
          (List.split_by_comma (fun (x, y) -> spf "%s -> %s" x y.x) ass)
      in *)
      match ass with
      | [] -> { gprop; elems }
      | _ ->
          let _ = Prover.check_sat_bool (None, gprop) in
          (* let () = Pp.printf "@{<bold>gprop:@} %s\n" (layout_prop gprop) in *)
          let gprop =
            msubst subst_prop_instance
              (List.map (fun (x, y) -> (x, AVar y)) ass)
              gprop
          in
          let gprop = simpl_eq_in_prop gprop in
          let line = msubst subst_name_in_line ass { gprop; elems } in
          (* let () = Pp.printf "@{<bold>gprop:@} %s\n" (layout_prop line.gprop) in *)
          let _ = Prover.check_sat_bool (None, gprop) in
          line)
