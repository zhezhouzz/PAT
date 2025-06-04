open Language
open Zutils
open PropTypecheck
open Typectx
open Zdatatype

type t = Nt.t

let _log = Myconfig._log_preprocess

let get_event_type_from_type event_ctx ty =
  match ty with
  | Nt.Ty_constructor (name, []) -> get_opt event_ctx name
  | Nt.Ty_record { alias = Some name; _ } -> get_opt event_ctx name
  | _ -> None

let constraint_p_prop_type_check bctx (bc : BC.bc) (prop : t prop) =
  let rec aux ctx bc prop =
    match prop with
    | Lit lit ->
        let bc, lit = constraint_lit_type_check ctx bc lit in
        let bc, _ = BC.add bc (lit.ty, Nt.bool_ty) in
        (bc, Lit lit.x#:Nt.bool_ty)
    | Implies (e1, e2) ->
        let bc, e1 = aux ctx bc e1 in
        let bc, e2 = aux ctx bc e2 in
        (bc, Implies (e1, e2))
    | Ite (e1, e2, e3) ->
        let bc, e1 = aux ctx bc e1 in
        let bc, e2 = aux ctx bc e2 in
        let bc, e3 = aux ctx bc e3 in
        (bc, Ite (e1, e2, e3))
    | Not e ->
        let bc, e = aux ctx bc e in
        (bc, Not e)
    | And es ->
        let bc, es =
          List.fold_left
            (fun (bc, res) e ->
              let bc, e = aux ctx bc e in
              (bc, res @ [ e ]))
            (bc, []) es
        in
        (bc, And es)
    | Or es ->
        let bc, es =
          List.fold_left
            (fun (bc, res) e ->
              let bc, e = aux ctx bc e in
              (bc, res @ [ e ]))
            (bc, []) es
        in
        (bc, Or es)
    | Iff (e1, e2) ->
        let bc, e1 = aux ctx bc e1 in
        let bc, e2 = aux ctx bc e2 in
        (bc, Iff (e1, e2))
    | Forall { qv; body } ->
        let qv = Nt.__force_typed [%here] qv in
        let qv_ty =
          match get_event_type_from_type bctx.event_ctx qv.ty with
          | None -> qv.ty
          | Some ty -> ty
        in
        let bc, body = aux (add_to_right ctx qv.x#:qv_ty) bc body in
        (bc, Forall { qv = qv.x#:qv_ty; body })
    | Exists { qv; body } ->
        let qv = Nt.__force_typed [%here] qv in
        let qv_ty =
          match get_event_type_from_type bctx.event_ctx qv.ty with
          | None -> qv.ty
          | Some ty -> ty
        in
        let bc, body = aux (add_to_right ctx qv.x#:qv_ty) bc body in
        (bc, Exists { qv = qv.x#:qv_ty; body })
  in
  aux bctx.ctx bc prop

let rec constraint_p_stmt_type_check bctx (bc : BC.bc) e =
  match e with
  | PMute lit ->
      let bc, lit = constraint_lit_type_check bctx.ctx bc lit in
      (bc, PMute lit)
  | PAssign { assign_kind; lvalue; rvalue } ->
      let bc, lvalue = constraint_lit_type_check bctx.ctx bc lvalue in
      let bc, rvalue = constraint_lit_type_check bctx.ctx bc rvalue in
      let bc, _ =
        let lty =
          match assign_kind with
          | Assign -> rvalue.ty
          | AssignSeqAdd -> mk_p_seq_ty rvalue.ty
          | AssignSetAdd -> mk_p_set_ty rvalue.ty
          | AssignMapAdd -> (
              match rvalue.ty with
              | Nt.Ty_tuple [ k; v ] -> mk_p_map_ty k v
              | _ -> _die [%here])
        in
        BC.add bc (lvalue.ty, lty)
      in
      (bc, PAssign { assign_kind; lvalue; rvalue })
  | PIf { condition; tbranch; fbranch } ->
      let bc, condition = constraint_lit_type_check bctx.ctx bc condition in
      let bc, tbranch = constraint_p_block_type_check bctx bc tbranch in
      let bc, fbranch =
        match fbranch with
        | None -> (bc, None)
        | Some fbranch ->
            let bc, fbranch = constraint_p_block_type_check bctx bc fbranch in
            (bc, Some fbranch)
      in
      let bc, _ = BC.add bc (condition.ty, Nt.bool_ty) in
      (bc, PIf { condition; tbranch; fbranch })
  | PForeach { foreach_kind; iter; iterable; body } ->
      let bc, iterable = constraint_lit_type_check bctx.ctx bc iterable in
      let iter = Nt.__force_typed [%here] iter in
      let bctx' = { bctx with ctx = add_to_right bctx.ctx iter } in
      let bc, body = constraint_p_block_type_check bctx' bc body in
      let bc =
        match foreach_kind with
        | ForeachSeq -> fst (BC.add bc (iterable.ty, mk_p_seq_ty iter.ty))
        | ForeachSet -> fst (BC.add bc (iterable.ty, mk_p_set_ty iter.ty))
        | ForeachMap ->
            let var = Rename.fresh_type_var () in
            let bc = BC.add_type_var bc var in
            let iterable_ty = mk_p_map_ty iter.ty (Nt.mk_type_var var) in
            fst (BC.add bc (iterable.ty, iterable_ty))
      in
      (bc, PForeach { foreach_kind; iter; iterable; body })
  | PWhile { condition; body } ->
      let bc, condition = constraint_lit_type_check bctx.ctx bc condition in
      let bc, body = constraint_p_block_type_check bctx bc body in
      let bc, _ = BC.add bc (condition.ty, Nt.bool_ty) in
      (bc, PWhile { condition; body })
  | PReturn e ->
      let bc, e = constraint_lit_type_check bctx.ctx bc e in
      (bc, PReturn e)
  | PPrintf (str, es) ->
      let bc, es =
        List.fold_left
          (fun (bc, res) e ->
            let bc, e = constraint_lit_type_check bctx.ctx bc e in
            (bc, res @ [ e ]))
          (bc, []) es
      in
      (bc, PPrintf (str, es))
  | PSend { dest; event_name; payload } ->
      let event_ty =
        match get_opt bctx.event_ctx event_name with
        | None -> _die [%here]
        | Some res -> res
      in
      let bc, dest = constraint_lit_type_check bctx.ctx bc dest in
      let bc, payload = constraint_lit_type_check bctx.ctx bc payload in
      let bc, _ = BC.add bc (payload.ty, event_ty) in
      (bc, PSend { dest; event_name; payload })
  | PRecieve { input; event_name; body } ->
      let event_ty =
        match get_opt bctx.event_ctx event_name with
        | None -> _die [%here]
        | Some res -> res
      in
      let bc, body = constraint_p_block_type_check bctx bc body in
      let bc, _ = BC.add bc (input.ty, event_ty) in
      (bc, PRecieve { input; event_name; body })
  | PGoto loc -> (bc, PGoto loc)
  | PBreak -> (bc, PBreak)

and constraint_p_block_type_check bctx (bc : BC.bc) es =
  List.fold_left
    (fun (bc, res) e ->
      let bc, e = constraint_p_stmt_type_check bctx bc e in
      (bc, res @ [ e ]))
    (bc, []) es

let solve bc =
  let open BC in
  let solution = Normalty.type_unification StrMap.empty bc.cs in
  match solution with
  | None -> _die_with [%here] "raw term normal type error"
  | Some sol ->
      Pp.printf "@{<bold>Solution:@}\n%s\n"
        (List.split_by_comma (fun (x, ty) -> spf "%s -> %s" x (Nt.layout ty))
        @@ StrMap.to_kv_list sol);
      sol

let constraint_p_closure_type_check bctx (bc : BC.bc) { local_vars; block } =
  let bctx = { bctx with ctx = add_to_rights bctx.ctx local_vars } in
  let bc, block = constraint_p_block_type_check bctx bc block in
  (bc, { local_vars; block })

let p_func_type_check bctx { name; func_label; params; retty; closure } =
  let bctx = { bctx with ctx = add_to_rights bctx.ctx params } in
  let bc, closure =
    constraint_p_closure_type_check bctx (BC.empty []) closure
  in
  let cs =
    List.map (fun x -> (x, retty)) @@ get_p_block_retty [%here] closure.block
  in
  let bc = List.fold_left (fun res c -> fst @@ BC.add res c) bc cs in
  let sol = solve bc in
  let closure = map_p_closure (Nt.msubst_nt sol) closure in
  (* let () = Pp.printf "@{<bold>Before subst:@}\n%s\n" (layout_typed_raw_term term) in *)
  { name; func_label; params; retty; closure }

let p_state_type_check bctx { name; state_label; state_body } =
  let state_body = List.map (p_func_type_check bctx) state_body in
  { name; state_label; state_body }

let p_machine_type_check bctx { name; local_vars; local_funcs; states } =
  let local_funcs_vars = List.map get_p_func_var local_funcs in
  let bctx =
    { bctx with ctx = add_to_rights bctx.ctx (local_funcs_vars @ local_vars) }
  in
  let states = List.map (p_state_type_check bctx) states in
  { name; local_vars; local_funcs; states }

let rec constraint_p_template_type_check bctx retty (bc : BC.bc) e =
  (* let () = Pp.printf "retty = %s\n" (Nt.layout_nt retty) in *)
  let bc, e =
    match e with
    | TPIf { condition; tbranch; fbranch } ->
        let bc, condition = constraint_prop_type_check bctx.ctx bc condition in
        let bc, tbranch =
          match tbranch with
          | None -> (bc, None)
          | Some tbranch ->
              let bc, tbranch =
                constraint_p_template_type_check bctx retty bc tbranch
              in
              (bc, Some tbranch)
        in
        let bc, fbranch =
          match fbranch with
          | None -> (bc, None)
          | Some fbranch ->
              let bc, fbranch =
                constraint_p_template_type_check bctx retty bc fbranch
              in
              (bc, Some fbranch)
        in
        (bc, TPIf { condition; tbranch; fbranch })
    | TPReturn e ->
        let bc, e = constraint_lit_type_check bctx.ctx bc e in
        let bc, _ = BC.add bc (e.ty, retty) in
        (bc, TPReturn e)
  in
  let () = Pp.printf "%s\n" (layout_p_templat 0 e) in
  let () = Pp.printf "BC = %s\n" (BC.layout bc) in
  (bc, e)

let p_item_type_check bctx item =
  match item with
  | PVisible es -> (bctx, PVisible es)
  | PEnumDecl (name, es) ->
      let es = List.map (fun x -> x#:(Nt.mk_uninterp name)) es in
      let ctx = add_to_rights bctx.ctx es in
      ({ bctx with ctx }, item)
  | PTopSimplDecl { kind = TopType; _ } -> (bctx, item)
  | PTopSimplDecl { kind = TopVar; tvar } ->
      let ctx = add_to_right bctx.ctx tvar in
      ({ bctx with ctx }, item)
  | PTopSimplDecl { kind = TopEvent; tvar } ->
      let fds = Nt.as_record [%here] tvar.ty in
      let tvar_ty = Nt.mk_record (Some tvar.x) fds in
      let tvar = tvar.x#:tvar_ty in
      let event_ctx = add_to_right bctx.event_ctx tvar in
      ({ bctx with event_ctx }, item)
  | PGlobalProp { name; prop } ->
      let bc, prop = constraint_p_prop_type_check bctx (BC.empty []) prop in
      let sol = solve bc in
      let prop = map_prop (Nt.msubst_nt sol) prop in
      let prop_ctx = add_to_right bctx.prop_ctx name#:prop in
      ({ bctx with prop_ctx }, PGlobalProp { name; prop })
  | PPayload { name; self_event; prop } ->
      (* let () = *)
      (*   Printf.printf "%s\n" (Typectx.layout_ctx Nt.layout bctx.event_ctx) *)
      (* in *)
      let e = self_event.x#:(_get_force [%here] bctx.event_ctx self_event.ty) in
      let ctx' = add_to_right bctx.ctx e in
      let ctx' = add_to_right ctx' "trace"#:(Nt.mk_uninterp "trace") in
      let bc, prop =
        constraint_p_prop_type_check { bctx with ctx = ctx' } (BC.empty []) prop
      in
      let sol = solve bc in
      let prop = map_prop (Nt.msubst_nt sol) prop in
      let payload = { name; self_event; prop } in
      let payload_ctx = add_to_right bctx.payload_ctx name#:payload in
      ({ bctx with payload_ctx }, PPayload payload)
  | PPayloadGen { gen_name; self_event_name; local_vars; content } ->
      let event_ty = _get_force [%here] bctx.event_ctx self_event_name in
      let local_vars' =
        List.map
          (fun x ->
            let x_ty =
              match get_event_type_from_type bctx.event_ctx x.ty with
              | None -> x.ty
              | Some ty -> ty
            in
            x.x#:x_ty)
          local_vars
      in
      let local_vars' =
        [
          "this"#:p_machine_ty;
          "trace"#:(Nt.mk_uninterp "trace");
          "destMachines"#:(mk_p_set_ty p_machine_ty);
        ]
        @ local_vars'
      in
      let bctx' = { bctx with ctx = add_to_rights bctx.ctx local_vars' } in
      let bc, content =
        constraint_p_template_type_check bctx' event_ty (BC.empty []) content
      in
      let sol = solve bc in
      let content = map_p_template (Nt.msubst_nt sol) content in
      let payload_gen = { gen_name; self_event_name; local_vars; content } in
      let payload_gen_ctx =
        add_to_right bctx.payload_gen_ctx self_event_name#:payload_gen
      in
      ({ bctx with payload_gen_ctx }, PPayloadGen payload_gen)
  | PSyn { name; gen_num; cnames } ->
      let f (x, ass) =
        let _ = _get_force [%here] bctx.event_ctx x in
        match ass.x with
        | AC (I _) -> (x, ass.x#:Nt.int_ty)
        | AVar y ->
            let y_ty = _get_force [%here] bctx.ctx y.x in
            if Nt.equal_nt y_ty Nt.int_ty then
              (x, (AVar y.x#:Nt.int_ty)#:Nt.int_ty)
            else _die [%here]
        | _ -> _die [%here]
      in
      let dom =
        (List.map _get_x @@ ctx_to_list bctx.prop_ctx)
        @ (List.map _get_x @@ ctx_to_list bctx.payload_ctx)
        @ List.map (fun x -> x.ty.gen_name)
        @@ ctx_to_list bctx.payload_gen_ctx
      in
      let gen_num = List.map f gen_num in
      let () =
        if not (List.for_all (fun c -> List.exists (String.equal c) dom) cnames)
        then _die [%here]
      in
      let syn = { name; gen_num; cnames } in
      ({ bctx with syn_ctx = add_to_right bctx.syn_ctx name#:syn }, PSyn syn)

let p_items_type_check bctx items =
  List.fold_left
    (fun (bctx, items) item ->
      let bctx, item = p_item_type_check bctx item in
      (bctx, items @ [ item ]))
    (bctx, []) items
