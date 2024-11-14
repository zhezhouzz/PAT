(* include Common *)
include Language
open Gamma
open Zdatatype
open Optimize

let check_valid_feature gamma lit =
  let aux lit = Gamma.check_valid gamma lit in
  (not (aux (lit_to_prop lit))) && not (aux @@ Not (lit_to_prop lit))

let check_valid_pre gamma prop = not (Gamma.check_valid gamma (Not prop))

let build_features { bvs; bprop } (abd_vars, lits) =
  let lits =
    List.filter (check_valid_feature { bvs = bvs @ abd_vars; bprop }) lits
  in
  let fvs = List.init (List.length lits) (fun _ -> [ true; false ]) in
  let fvs = List.choose_list_list fvs in
  let fvs =
    List.map
      smart_and
      #. (List.mapi (fun idx x ->
              let lit = lit_to_prop @@ List.nth lits idx in
              if x then lit else Not lit))
      fvs
  in
  let fvs = List.filter (check_valid_pre { bvs = bvs @ abd_vars; bprop }) fvs in
  fvs

let do_abduction { bvs; bprop } (checker : gamma -> bool) (abd_vars, fvs) =
  let fvs =
    List.filter
      (fun p -> checker { bvs = bvs @ abd_vars; bprop = smart_add_to bprop p })
      fvs
  in
  fvs

let mk_abd_prop fvs =
  match fvs with [] -> None | _ -> Some (simp_fvec_prop @@ smart_or fvs)

let build_fvtab env lits =
  let () =
    _log "syn" @@ fun _ ->
    Pp.printf "@{<bold>lits:@} %s\n" (List.split_by_comma layout_typed_lit lits)
  in
  (* Remove boolean constants *)
  let lits =
    List.filter (function { x = AC (B _); _ } -> false | _ -> true) lits
  in
  let bvars, lits =
    List.partition
      (function
        | { x = AVar x; _ } when Nt.equal_nt x.ty Nt.Ty_bool -> true
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
          List.filter (fun lit -> Nt.equal_nt Nt.Ty_int lit.ty) lits
        in
        let () =
          _log "syn" @@ fun _ ->
          Pp.printf "@{<bold>int lits:@} %s\n"
            (List.split_by_comma layout_typed_lit int_lits)
        in
        let pairs = List.combination_l int_lits 2 in
        let ltlits =
          let lt = ">" #: Nt.(construct_arr_tp ([ Ty_int; Ty_int ], Ty_bool)) in
          List.map
            (fun l ->
              match l with
              | [ x; y ] -> AAppOp (lt, [ x; y ])
              | _ -> _die [%here])
            pairs
        in
        let () =
          _log "syn" @@ fun _ ->
          Pp.printf "@{<bold>ltlits:@} %s\n"
            (List.split_by_comma layout_lit ltlits)
        in
        ltlits
  in
  let bvars = List.map _get_x bvars in
  let res = bvars @ Rawdesym.mybuild_euf lits @ additional in
  let () =
    _log "syn" @@ fun _ ->
    Pp.printf "@{<bold>build_fvtab:@} %s\n" (List.split_by_comma layout_lit res)
  in
  res

let mk_raw_all env =
  let l =
    List.map (fun x -> { op = x.x; vs = x.ty; phi = mk_true })
    @@ ctx_to_list env.event_tyctx
  in
  if List.length l == 0 then _die [%here] else SFA.CharSet.of_list l

let check_regex_nonempty env { bprop; _ } r =
  let really_do_check_regex_nonempty () =
    let _, r' =
      Rawdesym.desymbolic_symbolic_rewregex env.tyctx env.event_tyctx (bprop, r)
    in
    let () =
      _log "syn" @@ fun _ ->
      Pp.printf "@{<bold>check_regex_nonempty@}: %s\n" (SFA.layout_raw_regex r)
    in
    let () =
      _log "syn" @@ fun _ ->
      Pp.printf "@{<bold>check_regex_nonempty@}: %s\n"
        (DesymFA.layout_raw_regex r')
    in
    not @@ DesymFA.emptiness r'
  in
  really_do_check_regex_nonempty ()

let abduction_automata env { bvs; bprop } (a : SFA.raw_regex) abd_vars =
  let really_do_abduction () =
    let lits = Rawdesym.mk_global_ftab env.tyctx (bvs @ abd_vars, bprop, a) in
    let lits =
      List.filter
        (fun lit ->
          not
            (List.is_empty
            @@ List.interset String.equal (fv_lit_id lit)
                 (List.map _get_x abd_vars)))
        (build_fvtab env @@ List.map (fun l -> l #: (lit_to_nt l)) lits)
    in
    let fvs = build_features { bvs; bprop } (abd_vars, lits) in
    let () =
      _log "syn" @@ fun _ ->
      Pp.printf "@{<bold>abduction_automata fvs:@}: %i\n" (List.length fvs)
    in
    (* let checker gamma (_, prop) = check_valid gamma prop in *)
    let fvs =
      do_abduction { bvs; bprop }
        (fun gamma ->
          let () =
            _log "syn" @@ fun _ ->
            Pp.printf "@{<bold>gamma@}: %s\n" (Gamma.layout gamma)
          in
          let res = check_regex_nonempty env gamma a in
          let () =
            _log "syn" @@ fun _ ->
            Pp.printf "@{<bold>@{<yellow>abduction_automata@}@}: %b\n" res
          in
          res)
        (abd_vars, fvs)
    in
    fvs
  in
  Stat.stat_nonempty_check really_do_abduction

let abduction_plan env gamma plan abd_vars =
  let a = Plan.plan_to_raw_regex env.event_tyctx plan in
  abduction_automata env gamma a abd_vars

let abduction_mid_goal env gamma (plan1, elem, plan2) abd_vars =
  let plan = plan1 @ [ elem ] @ plan2 in
  let fvs = abduction_plan env gamma plan abd_vars in
  match fvs with
  | [] -> None
  | _ ->
      Some
        {
          bvs = gamma.bvs @ abd_vars;
          bprop = smart_add_to (smart_or fvs) gamma.bprop;
        }
