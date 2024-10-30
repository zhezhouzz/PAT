open Language
open Zdatatype
open Refine
open MkTerm
open Common

let mk_synthesis_goal (env : syn_env) =
  let qvs, reg =
    match env.goal with
    | None -> _die_with [%here] "no goal"
    | Some { qvs; prop } -> (qvs, prop)
  in
  (* let rctx = *)
  (*   add_to_rights emp *)
  (*     (List.map (fun x -> x.x #: { nt = x.ty; phi = mk_true }) qvs) *)
  (* in *)
  (* let () = *)
  (*   Pp.printf "\n@{<red>After smart negate:@} %s\n" (layout_symbolic_regex reg) *)
  (* in *)
  let op_names = List.map _get_x (ctx_to_list env.event_tyctx) in
  let reg =
    desugar env.event_tyctx (SyntaxSugar (CtxOp { op_names; body = reg }))
  in
  let reg = delimit_context reg in
  let () =
    Pp.printf "\n@{<red>Original Reg:@} %s\n" (layout_symbolic_regex reg)
  in
  let goal = SFA.regex_to_raw reg in
  let () = Stat.goal_size := Stat.count_quantifier_rawregex goal in
  (Gamma.{ bvs = qvs; bprop = mk_true }, goal)

let synthesize env goal =
  let () = setup_clock None in
  let real_do_synthesize () =
    let aux () = deductive_synthesis_reg env goal in
    let* g = Stat.stat_refine aux in
    let () = Pp.printf "\n@{<red>Result gamma:@} %s\n" (Gamma.layout g.gamma) in
    let () =
      Pp.printf "\n@{<red>Result program:@} %s\n" (Plan.layout_plan g.plan)
    in
    (* let () = _die [%here] in *)
    let term = instantiation env (g.gamma, g.plan) in
    Some term
  in
  Stat.stat_total real_do_synthesize
(* Some (reverse_instantiation env res) *)

let syn_timeout timebound env =
  let () = setup_clock (Some timebound) in
  let goal = mk_synthesis_goal env in
  let res =
    try match deductive_synthesis env goal with _ -> !result_buffer
    with Timeout (_, results) -> (* let () = raise exn in *)
                                 results
  in
  (* let () = _die [%here] in *)
  let terms = List.map (fun g -> instantiation env (g.gamma, g.plan)) res in
  let () = Pp.printf "\n@{<red>Get %i results:@}\n" (List.length terms) in
  terms

let syn_one env =
  match synthesize env @@ mk_synthesis_goal env with
  | None -> _die_with [%here] "synthesis fails"
  | Some term -> term
