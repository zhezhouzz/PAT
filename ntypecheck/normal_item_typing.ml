open Language
open AutomataLibrary

let init_env =
  {
    goal = None;
    event_tyctx = emp;
    gen_ctx = emp;
    recvable_ctx = emp;
    tyctx = emp;
    event_rtyctx = emp;
  }

let add_to_env (env : syn_env) = function
  | PrimDecl { name; nt } ->
      { env with tyctx = add_to_right env.tyctx name#:nt }
  | MsgNtDecl { generative; recvable; name; nt } ->
      let nty =
        if Nt.equal_nt Nt.unit_ty nt then
          _die_with [%here]
            "when parse event with unit type, should build an empty record type"
        else nt
      in
      let event_tyctx = add_to_right env.event_tyctx name#:nty in
      let gen_ctx = add_to_right env.gen_ctx name#:generative in
      let recvable_ctx = add_to_right env.recvable_ctx name#:recvable in
      { env with event_tyctx; gen_ctx; recvable_ctx }
  | MsgDecl _ -> env
  | SynGoal _ -> env

let desugar_reg (env : syn_env) reg =
  let op_names = List.map _get_x (ctx_to_list env.event_tyctx) in
  let reg = rich_regex_desugar (CtxOp { op_names; body = reg }) in
  reg

let map_fa_haft f haft =
  let rec aux t =
    match t with
    | RtyBase cty -> RtyBase cty
    | RtyHAF { history; adding; future } ->
        let history, adding, future = map3 f (history, adding, future) in
        RtyHAF { history; adding; future }
    | RtyHAParallel { history; adding_se; parallel } ->
        let history = f history in
        RtyHAParallel { history; adding_se; parallel }
    | RtyArr { arg; argcty; retrty } ->
        RtyArr { arg; argcty; retrty = aux retrty }
    | RtyGArr { arg; argnty; retrty } ->
        RtyGArr { arg; argnty; retrty = aux retrty }
    | RtyInter (t1, t2) -> RtyInter (aux t1, aux t2)
  in
  aux haft

let desugar_haft (env : syn_env) haft =
  map_fa_haft (fun r -> SFA.rich_regex_to_regex @@ desugar_reg env r) haft

(* NOTE: the whole spec items are first-order *)
let item_check (env : syn_env) = function
  | MsgDecl { name; haft } ->
      let haft =
        Normal_rty_typing.rich_symbolic_regex_haft_type_check env.event_tyctx
          env.tyctx haft
      in
      {
        env with
        event_rtyctx =
          add_to_right env.event_rtyctx name#:(desugar_haft env haft);
      }
  | SynGoal { qvs; prop } -> (
      match env.goal with
      | Some _ -> _die_with [%here] "multiple goals"
      | _ ->
          let prop = desugar_reg env (ComplementA prop) in
          let () =
            Pp.printf "@{<bold>After:@} %s\n" (layout_rich_symbolic_regex prop)
          in
          let prop =
            rich_symbolic_regex_type_check env.event_tyctx
              (add_to_rights env.tyctx qvs)
              prop
          in
          { env with goal = Some { qvs; prop } })
  | _ -> env

let struct_check env l =
  let env = List.fold_left add_to_env env l in
  let () = Printf.printf "%s\n" (layout_syn_env env) in
  let env = List.fold_left item_check env l in
  env
