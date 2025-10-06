open Language
open Zdatatype
(* open AutomataLibrary
open Refine *)

(* open MkTerm *)

let tmp_plans_file = "/tmp/plans.sexp"

let save_plans plans =
  let sexp = Sexplib.Sexp.List (List.map sexp_of_synMidResult plans) in
  Sexplib.Sexp.save tmp_plans_file sexp

let load_plans () =
  let sexp = Sexplib.Sexp.load_sexp tmp_plans_file in
  Sexplib.Std.list_of_sexp synMidResult_of_sexp sexp

let output_prefix = "output"

let save_progs name terms =
  let output_file = spf "%s/%s.scm" output_prefix name in
  let sexp = Sexplib.Sexp.List (List.map sexp_of_term terms) in
  Sexplib.Sexp.save output_file sexp

let load_progs name () =
let _ = Pp.printf "@{<yellow>synthesis.ml:@}  26\n" in
  let output_file = spf "%s/%s.scm" output_prefix name in
let _ = Pp.printf "@{<yellow>synthesis.ml:@}  28\n" in
  let sexp = Sexplib.Sexp.load_sexp output_file in
let _ = Pp.printf "@{<yellow>synthesis.ml:@}  30\n" in
  Sexplib.Std.list_of_sexp term_of_sexp sexp

let synthesize (env : syn_env) name =
  let qvs, reg =
    match StrMap.find_opt env.goals name with
    | None -> _die_with [%here] "no goal"
    | Some { qvs; prop; _ } -> (qvs, prop)
  in
  let op_names = List.map _get_x (ctx_to_list env.event_tyctx) in
  let reg =
    rich_regex_desugar env.event_tyctx (CtxOp { op_names; body = reg })
  in
  let m = List.map (fun x -> (x.x, AVar (Rename.unique_var x.x)#:x.ty)) qvs in
  let () =
    Pp.printf "@{<bold>m:@} %s\n"
      (List.split_by_comma (fun (x, y) -> spf "%s -> %s" x (layout_lit y)) m)
  in
  let reg = msubst subst_rich_regex_instance m reg in
  let r = SFA.rich_regex_to_regex reg in
  let () = Pp.printf "\n@{<red>Original Reg:@} %s\n" (SFA.layout_regex r) in
  (* let () = _die [%here] in *)
  let plans = Refine.deductive_synthesis env r in
  let () = Pp.printf "\n@{<yellow>Result plans:@}\n" in
  let () = save_plans plans in
  let () = Pp.printf "@{<bold>load plans:@}%s\n" name in
  let plans = load_plans () in
  List.iter (fun p -> Plan.print_mid_result p) plans;
  (* let term = instantiation env (g.gamma, g.plan) in *)
  let progs = List.map (fun p -> Compile.compile_term env p) plans in
  let () = Pp.printf "@{<bold>num of progs:@}%i\n" (List.length progs) in
  let () =
    List.iteri
      (fun i p -> Pp.printf "@{<bold>Prog[%i]:@}\n%s\n" i (layout_term p))
      progs
  in
  progs
