open Language
open Zdatatype
open AutomataLibrary
open Refine

(* open MkTerm *)

let synthesize (env : syn_env) =
  let _, reg =
    match env.goal with
    | None -> _die_with [%here] "no goal"
    | Some { qvs; prop } -> (qvs, prop)
  in
  let op_names = List.map _get_x (ctx_to_list env.event_tyctx) in
  let reg =
    rich_regex_desugar env.event_tyctx (CtxOp { op_names; body = reg })
  in
  let r = SFA.rich_regex_to_regex reg in
  let () = Pp.printf "\n@{<red>Original Reg:@} %s\n" (SFA.layout_regex r) in
  let plans = deductive_synthesis env r in
  let () = Pp.printf "\n@{<yellow>Result plans:@}\n" in
  List.iter (fun p -> Plan.print_plan p) plans;
  (* let term = instantiation env (g.gamma, g.plan) in *)
  plans
