open Language
open Zdatatype

let mk_op_apply_vars op var1 var2 =
  let lit1, lit2 = map2 lit_to_tlit (AVar var1, AVar var2) in
  let op = op#:(Nt.construct_arr_tp ([ lit1.ty; lit2.ty ], Nt.bool_ty)) in
  lit_to_tlit (AAppOp (op, [ lit1; lit2 ]))

let lit_as_temp_constraint_opt lit =
  match lit.x with
  | AAppOp (op, [ e1; e2 ]) when not (Nt.equal_nt Nt.int_ty e1.ty) ->
      let* e1, e2 =
        match (e1.x, e2.x) with
        | AVar e1, AVar e2 -> Some (e1.x#:Nt.int_ty, e2.x#:Nt.int_ty)
        | _ -> None
      in
      Some
        (match op.x with
        | "<" | ">" | "==" | "<=" | ">=" -> mk_op_apply_vars op.x e1 e2
        | _ -> _die [%here])
  | _ -> None

type prefix = (Nt.nt, string) typed list

let prefix_to_prop prefix =
  let prefix = List.map (fun x -> x.x#:Nt.int_ty) prefix in
  let rec aux prev rest =
    match rest with
    | [] -> mk_true
    | cur :: rest ->
        let p = aux cur rest in
        smart_add_to (Lit (mk_op_apply_vars "<" prev cur)) p
  in
  match prefix with [] -> mk_true | cur :: rest -> aux cur rest

let check_valid_under_prefix prefix lit =
  let fvs = fv_lit lit.x in
  if
    List.exists
      (fun fv -> List.for_all (fun qv -> not (String.equal fv.x qv.x)) prefix)
      fvs
  then _die [%here]
  else
    let prop =
      smart_forall prefix (smart_implies (prefix_to_prop prefix) (Lit lit))
    in
    (* let () = Pp.printf "%s\n" (layout_prop prop) in *)
    Prover.check_valid (None, prop)

let list_insert l (idx, x) =
  let rec aux l idx =
    if idx <= 0 then x :: l
    else match l with [] -> [ x ] | y :: l -> y :: aux l (idx - 1)
  in
  let res = aux l idx in
  (* Pp.printf "insert (%i, %s) into %s = %s\n" idx x.x *)
  (*   (List.split_by_comma _get_x l) *)
  (*   (List.split_by_comma _get_x res); *)
  res

let generate_domain event_ctx prefix (x, body) =
  let eq_options =
    List.map
      (fun y ->
        let lit = AVar y.x#:x.ty in
        let prop = subst_prop_instance x.x lit body in
        (* let () = *)
        (*   Printf.printf "subst: %s ----> %s\n" (layout_prop body) *)
        (*     (layout_prop prop) *)
        (* in *)
        (prefix, None, prop))
      prefix
  in
  let ord_options =
    List.init
      (List.length prefix + 1)
      (fun idx ->
        let prefix' = list_insert prefix (idx, x) in
        let x' = x.x#:(_get_force [%here] event_ctx (Nt.layout x.ty)) in
        (* let () = Printf.printf "new x': %s : %s\n" x'.x (Nt.layout x'.ty) in *)
        (prefix', Some x', body))
  in
  eq_options @ ord_options

open Preprocess

let fol_to_ltl event_ctx (prop : Nt.t prop) =
  (* let _ = _die [%here] in *)
  let rec aux (prefix, prop) =
    (* Pp.printf "prefix %s\n" (List.split_by_comma _get_x prefix); *)
    (* Pp.printf "prop %s\n" (layout_prop prop); *)
    match prop with
    | Lit lit -> (
        match lit_as_temp_constraint_opt lit with
        | None ->
            (* Pp.printf "lit %s\n" (layout_lit lit.x); *)
            Lit lit
        | Some lit ->
            let res = check_valid_under_prefix prefix lit in
            (* Pp.printf "%s = %b\n" (layout_lit lit.x) res; *)
            if res then mk_true else mk_false)
    | Forall { qv; body } -> aux_q Nt.Fa prefix qv body
    | Exists { qv; body } -> aux_q Nt.Ex prefix qv body
    | And l -> smart_and (List.map aux (List.map (fun x -> (prefix, x)) l))
    | Not p -> smart_not (aux (prefix, p))
    | Or l -> smart_or (List.map aux (List.map (fun x -> (prefix, x)) l))
    | Implies (p1, p2) -> smart_implies (aux (prefix, p1)) (aux (prefix, p2))
    | Iff (p1, p2) -> smart_iff (aux (prefix, p1)) (aux (prefix, p2))
    | Ite _ -> _die_with [%here] "unimp"
  and aux_q fa prefix qv body =
    let construct qv body = function
      | Nt.Fa -> smart_forall [ qv ] body
      | Nt.Ex -> smart_exists [ qv ] body
    in
    if List.length prefix == 0 then construct qv (aux ([ qv ], body)) fa
    else
      match get_event_type_from_type event_ctx qv.ty with
      | None -> construct qv (aux (prefix, body)) fa
      | _ -> (
          let l = generate_domain event_ctx prefix (qv, body) in
          let props =
            List.map
              (function
                | prefix, None, body -> aux (prefix, body)
                | prefix, Some qv, body -> construct qv (aux (prefix, body)) fa)
              l
          in
          match fa with Nt.Fa -> smart_and props | Nt.Ex -> smart_or props)
  in
  let res = aux ([], prop) in
  Prop.SimplProp.eval_arithmetic res

type reS = FO of Nt.t prop | SO of SFA.reg

let lift_epr =
  let rec aux (fvar, evar) = function
    | Lit _ -> (fvar, evar)
    | Implies (e1, e2) -> aux (fvar, evar) (Or [ e1; Not e2 ])
    | Ite _ -> _die_with [%here] "unimp"
    | Not p ->
        let evar, fvar = aux (evar, fvar) p in
        (fvar, evar)
    | And es -> List.fold_left aux (fvar, evar) es
    | Or es -> List.fold_left aux (fvar, evar) es
    | Iff _ -> _die_with [%here] "unimp"
    | Forall { qv; body } -> aux (qv :: fvar, evar) body
    | Exists { qv; body } -> aux (fvar, qv :: evar) body
  in
  aux

let ltl_to_sregex event_ctx (prop : Nt.t prop) =
  let mk_se ty f =
    let op =
      match ty with
      | Nt.Ty_record { alias = Some op; _ } -> op
      | _ -> _die [%here]
    in
    let v = "v"#:ty in
    { op; vs = [ v ]; phi = f v }
  in
  let mk_others op =
    let others =
      List.filter (fun x -> not (String.equal x.x op)) @@ ctx_to_list event_ctx
    in
    let others = List.map (fun x -> mk_top_sevent [%here] x.x x.ty) others in
    others
  in
  let cs_to_r cs = MultiChar (SFA.CharSet.of_list cs) in
  let all_ses phi =
    SFA.CharSet.of_list
    @@ List.map
         (fun x ->
           let se = mk_top_sevent [%here] x.x x.ty in
           { se with phi })
         (ctx_to_list event_ctx)
  in
  let all phi = MultiChar (all_ses phi) in
  (* let star_frame qv phi = *)
  (*   let se = mk_se qv.ty (fun v -> subst_prop_instance qv.x (AVar v) phi) in *)
  (*   let others = mk_others se.op in *)
  (*   SFA.star (cs_to_r (se :: others)) *)
  (* in *)
  let rec aux prop =
    (* Pp.printf "prefix %s\n" (List.split_by_comma _get_x prefix); *)
    (* Pp.printf "prop %s\n" (layout_prop prop); *)
    match prop with
    | Lit _ -> SFA.star (all prop)
    | Forall { qv; body } ->
        let r = aux body in
        let se = mk_se qv.ty (fun v -> Lit (mk_op_apply_vars "==" v qv)) in
        let neg_se = mk_se qv.ty (fun v -> Lit (mk_op_apply_vars "!=" v qv)) in
        let frame = SFA.star (cs_to_r (neg_se :: mk_others se.op)) in
        SFA.alt frame (SFA.seq [ SFA.star (all mk_true); cs_to_r [ se ]; r ])
    | Exists { qv; body } ->
        let r = aux body in
        let se = mk_se qv.ty (fun v -> Lit (mk_op_apply_vars "==" v qv)) in
        SFA.seq [ SFA.star (all mk_true); cs_to_r [ se ]; r ]
    | And l -> SFA.inter_list (List.map aux l)
    | Not p -> SFA.comple (all_ses mk_true) (aux p)
    | Or l -> SFA.alt_list (List.map aux l)
    | Implies (p1, p2) -> aux (Or [ Not p1; p2 ])
    | Iff _ -> _die [%here]
    | Ite _ -> _die_with [%here] "unimp"
    (* | Forall { qv; body } -> *)
    (*     let f = function *)
    (*       | FO phi -> cont (SO (star_frame qv phi)) *)
    (*       | SO r -> *)
    (*           let se = *)
    (*             mk_se qv.ty (fun v -> Lit (mk_op_apply_vars "==" v qv)) *)
    (*           in *)
    (*           let neg_se = *)
    (*             mk_se qv.ty (fun v -> Lit (mk_op_apply_vars "!=" v qv)) *)
    (*           in *)
    (*           let others = mk_others se.op in *)
    (*           let frame = SFA.star (cs_to_r (neg_se :: others)) in *)
    (*           let r' = SFA.alt frame (SFA.seq [ all; cs_to_r [ se ]; r ]) in *)
    (*           cont (SO r') *)
    (*     in *)
    (*     aux f body *)
    (* | Exists _ -> _die [%here] *)
  in
  let fvars, evars = lift_epr ([], []) prop in
  let res = aux prop in
  let _ = Pp.printf "regex %s\n" (SFA.layout_regex res) in
  let fa = SFA.compile_regex_to_dfa res in
  let fa =
    SFA.dfa_realize
      (fun { phi; vs; _ } ->
        let prop = smart_forall fvars (smart_exists (evars @ vs) phi) in
        Prover.check_sat_bool (None, prop))
      fa
  in
  let _ =
    Pp.printf "fa\n";
    SFA.display_dfa fa
  in
  fa

let fol_to_ltl_prop_ctx ctx =
  let res = Typectx.map_ctx (fol_to_ltl ctx.event_ctx) ctx.payload_ctx in
  let _ =
    List.map (fun x -> Pp.printf "prop %s\n%s\n" x.x (layout_prop x.ty))
    @@ Typectx.ctx_to_list res
  in
  let res = Typectx.map_ctx (ltl_to_sregex ctx.event_ctx) res in
  res
