open Language
open AutomataLibrary
open PropTypecheck

type t = Nt.t

let _log = Myconfig._log_preprocess

let constraint_cty_type_check (bctx : basic_typing_ctx) (bc : BC.bc)
    ({ phi; nty } : cty) =
  let ctx = add_to_right bctx.ctx default_v#:nty in
  let bc, phi = constraint_prop_type_check ctx bc phi in
  (bc, { nty; phi })

let constraint_haft_type_check
    (achecker : basic_typing_ctx -> BC.bc -> 'a -> BC.bc * 'a)
    (bctx : basic_typing_ctx) (bc : BC.bc) (rty : 'a haft) =
  let rec aux bctx bc rty =
    (* let () = _log @@ fun _ -> Printf.printf "rty: %s\n" (layout_haft rty) in *)
    match rty with
    | RtyBase cty ->
        let bc, cty = constraint_cty_type_check bctx bc cty in
        (bc, RtyBase cty)
    | RtyHAF { history; adding; future } ->
        let bc, history = achecker bctx bc history in
        let bc, adding = achecker bctx bc adding in
        let bc, future = achecker bctx bc future in
        (bc, RtyHAF { history; adding; future })
    | RtyHAParallel { history; adding_se; parallel } ->
        let bc, history = achecker bctx bc history in
        let bc, adding_se = constraint_sevent_type_check bctx bc adding_se in
        let bc, parallel =
          List.fold_left
            (fun (bc, l) se ->
              let bc, se' = constraint_sevent_type_check bctx bc se in
              (bc, l @ [ se' ]))
            (bc, []) parallel
        in
        (bc, RtyHAParallel { history; adding_se; parallel })
    | RtyArr { arg; argcty; retrty } ->
        let bc, argcty = constraint_cty_type_check bctx bc argcty in
        let argnty = erase_cty argcty in
        let ctx' =
          if Nt.is_base_tp argnty then add_to_right bctx.ctx arg#:argnty
          else _die_with [%here] "not a base type"
        in
        let bc, retrty = aux { bctx with ctx = ctx' } bc retrty in
        (bc, RtyArr { argcty; arg; retrty })
    | RtyGArr { arg; argnty; retrty } ->
        let ctx' =
          if Nt.is_base_tp argnty then add_to_right bctx.ctx arg#:argnty
          else _die_with [%here] "not a base type"
        in
        let bc, retrty = aux { bctx with ctx = ctx' } bc retrty in
        (bc, RtyGArr { argnty; arg; retrty })
    | RtyInter (t1, t2) ->
        let bc, t1 = aux bctx bc t1 in
        let bc, t2 = aux bctx bc t2 in
        (bc, RtyInter (t1, t2))
  in
  aux bctx bc rty

let rich_symbolic_regex_haft_type_check event_ctx ctx r =
  let bctx = { event_ctx; ctx } in
  let bc, r =
    constraint_haft_type_check constraint_rich_regex_type_check bctx
      (BC.empty []) r
  in
  let sol = solve bc in
  let f = Nt.msubst_nt sol in
  let mapr f = map_rich_regex (map_sevent f) in
  let r = map_haft mapr f r in
  (* let () = Pp.printf "@{<bold>Before subst:@}\n%s\n" (layout_typed_raw_term term) in *)
  r
