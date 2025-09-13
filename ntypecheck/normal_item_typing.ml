open Language
open AutomataLibrary
open Zdatatype

let init_env =
  {
    goals = StrMap.empty;
    event_tyctx = emp;
    msgkind_ctx = emp;
    tyctx = emp;
    event_rtyctx = emp;
    axioms = StrMap.empty;
  }

let add_to_env (env : syn_env) = function
  | PrimDecl { name; nt } ->
      { env with tyctx = add_to_right env.tyctx name#:nt }
  | MsgNtDecl { msgkind; name; nt } ->
      let nty =
        if Nt.equal_nt Nt.unit_ty nt then
          _die_with [%here]
            "when parse event with unit type, should build an empty record type"
        else nt
      in
      let event_tyctx = add_to_right env.event_tyctx name#:nty in
      let msgkind_ctx = add_to_right env.msgkind_ctx name#:msgkind in
      { env with event_tyctx; msgkind_ctx }
  | MsgDecl _ -> env
  | SynGoal _ -> env
  | PrAxiom _ -> env

let desugar_reg (env : syn_env) reg =
  let op_names = List.map _get_x (ctx_to_list env.event_tyctx) in
  let reg =
    rich_regex_desugar env.event_tyctx (CtxOp { op_names; body = reg })
  in
  reg

let map_fa_pat f pat =
  let rec aux = function
    | RtyBase cty -> RtyBase cty
    | RtyHAF { history; adding; future } ->
        RtyHAF { history = f history; adding = f adding; future = f future }
    | RtyHAParallel { history; adding_se; parallel } ->
        RtyHAParallel { history = f history; adding_se; parallel }
    | RtyArr { arg; argcty; retrty } ->
        RtyArr { arg; argcty; retrty = aux retrty }
    | RtyGArr { arg; argnty; retrty } ->
        RtyGArr { arg; argnty; retrty = aux retrty }
    | RtyInter (t1, t2) -> RtyInter (aux t1, aux t2)
  in
  aux pat

let desugar_pat (env : syn_env) pat =
  map_fa_pat (fun r -> SFA.rich_regex_to_regex @@ desugar_reg env r) pat

let rich_symbolic_global_prop_type_check event_ctx ctx (qvs, prop) =
  let bctx = { event_ctx; ctx = add_to_rights ctx qvs } in
  let bc, prop =
    constraint_rich_regex_type_check bctx (PropTypecheck.BC.empty []) prop
  in
  let sol = solve bc in
  let f = Nt.msubst_nt sol in
  let prop = map_rich_regex (map_sevent f) prop in
  prop

let type_check_item env = function
  | MsgDecl { name; pat } ->
      let pat =
        Normal_rty_typing.rich_symbolic_regex_pat_type_check env.event_tyctx
          env.tyctx pat
      in
      {
        env with
        event_rtyctx = add_to_right env.event_rtyctx name#:(desugar_pat env pat);
      }
  | SynGoal { name; qvs; prop } -> (
      let prop =
        rich_symbolic_global_prop_type_check env.event_tyctx env.tyctx
          (qvs, prop)
      in
      match StrMap.find_opt env.goals name with
      | None ->
          { env with goals = StrMap.add name { name; qvs; prop } env.goals }
      | Some _ -> _die_with [%here] "multiple goals")
  | PrAxiom { name; prop } ->
      let prop = PropTypecheck.prop_type_check env.tyctx [] prop in
      {
        env with
        axioms =
          StrMap.update name
            (function
              | None -> Some prop
              | Some _ -> _die_with [%here] (spf "duplicate axiom: %s" name))
            env.axioms;
      }
  | _ -> env

let struct_check env l =
  let env = List.fold_left add_to_env env l in
  let () = Printf.printf "%s\n" (layout_syn_env env) in
  let env = List.fold_left type_check_item env l in
  let () =
    Prover.update_axioms
      (List.map
         (fun (name, prop) -> (name, [], prop))
         (StrMap.to_kv_list env.axioms))
  in
  env
