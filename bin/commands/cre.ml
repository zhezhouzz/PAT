open Caux
open Language
open Ntypecheck
open OcamlParser
open Zdatatype

let parse = Oparse.parse_imp_from_file

let read_ocaml_file source_file () =
  let _ = Pp.printf "@{<yellow>cre.ml:@}  10\n" in
  let code = Oparse.parse_imp_from_file ~sourcefile:source_file in
  let _ = Pp.printf "@{<yellow>cre.ml:@}  12\n" in
  let code = ocaml_structure_to_items code in
  code

let read_source_file source_file () =
  let postfix = List.last @@ Core.String.split source_file ~on:'.' in
  match postfix with
  | "ml" -> read_ocaml_file source_file ()
  (* | "s" -> FrontSpec.parse source_file *)
  (* | "p" -> FrontSpec.parse source_file *)
  | _ -> failwith @@ spf "wrong file extension *.%s" postfix

let read_functional_p_file source_file () =
  let postfix = List.last @@ Core.String.split source_file ~on:'.' in
  match postfix with
  (* | "funcp" -> FrontFuncP.parse source_file *)
  | _ -> failwith @@ spf "wrong file extension *.%s" postfix

(* let read_p source_file () = *)
(*   let code = read_functional_p_file source_file () in *)
(*   let code = Ptypecheck.p_items_infer emp code in *)
(*   () *)

let read_prop_from_ml source_file () =
  let open Language in
  let code = read_source_file source_file () in
  let prop =
    match code with PrAxiom { prop; _ } :: _ -> prop | _ -> _die [%here]
  in
  prop

let test_prop env name () =
  let open Language in
  let () = Printf.printf "z3: %s\n" Z3.Version.to_string in
  let ic = In_channel.open_text (spf "/tmp/%s.scm" name) in
  try
    let str = In_channel.input_all ic in
    let prop = prop_of_sexp Nt.nt_of_sexp @@ Sexplib.Sexp.of_string str in
    (* let fvs =
      List.slow_rm_dup (fun x y -> String.equal x.x y.x) @@ Prop.fv_prop prop
    in
    let prop = Prop.smart_exists fvs prop in *)
    let () = Printf.printf "prop: %s\n" (Prop.layout_prop__raw prop) in
    let prop =
      read_prop_from_ml
        "/Users/zhezhou/workspace/research/ocaml_workspace/PAT/data/queries/t.ml"
        ()
    in
    let prop = Zutils.PropTypecheck.prop_type_check env.tyctx [] prop in
    let () = Printf.printf "new prop: %s\n" (Prop.layout_prop prop) in
    (* let fvs = Prop.fv_prop prop in *)
    (* let prop = Prop.smart_exists fvs prop in *)
    (* let () = Printf.printf "prop: %s\n" (Prop.layout_prop prop) in *)
    (* let prop = Prop.SimplProp.simpl_query prop in *)
    (* let () = Printf.printf "prop: %s\n" (Prop.layout_prop prop) in *)
    In_channel.close ic;
    let res = Prover.check_sat (None, prop) in
    (match res with
    | SmtUnsat -> Printf.printf "unsat prop: %s\n" (layout_prop prop)
    | SmtSat model ->
        Printf.printf "model:\n%s\n" @@ Z3.Model.to_string model;
        ()
    | Timeout ->
        Printf.printf "timeout\n";
        ());
    _die [%here]
  with e -> raise e

let read_syn source_file () =
  let code = read_source_file source_file () in
  (* let () = Printf.printf "%s\n" (layout_structure code) in *)
  let env = Ntypecheck.(struct_check init_env code) in
  let () = Printf.printf "%s\n" (layout_syn_env env) in
  let () = Stat.init_algo_complexity () in
  (* let () = test_prop "timeout" () in *)
  let term = Synthesis.synthesize env in
  ()

let handle_syn_result (env, num_assert, term) name =
  let () = Stat.dump (env, num_assert, term) name in
  let output_file = spf "output/%s.scm" name in
  let () = Pp.printf "@{<bold>Output file:@}:\n%s\n" output_file in
  let oc = Out_channel.open_text output_file in
  try
    Sexplib.Sexp.output oc @@ sexp_of_term term;
    Out_channel.close oc
  with e ->
    Out_channel.close oc;
    raise e

let do_syn ?(num_expected = 1) name source_file () =
  let code = read_source_file source_file () in
  (* let () = Printf.printf "%s\n" (layout_structure code) in *)
  let env = Ntypecheck.(struct_check init_env code) in
  let () = Stat.init_algo_complexity () in
  (* let () = test_prop env "timeout" () in *)
  let num_assert, prog =
    Stat.stat_total (fun () -> Synthesis.synthesize env name num_expected)
  in
  let () = handle_syn_result (env, num_assert, prog) name in
  (* let () = Synthesis.save_progs name progs in *)
  ()

let do_naive_syn name source_file size timebound () =
  let code = read_source_file source_file () in
  (* let () = Printf.printf "%s\n" (layout_structure code) in *)
  let env = Ntypecheck.(struct_check init_env code) in
  let () = Stat.init_algo_complexity () in
  (* let () = test_prop env "timeout" () in *)
  let res = Synthesis.naive_synthesize env name size timebound in
  let result =
    {
      Stat.naive_syn_name = name;
      Stat.naive_syn_size = size;
      Stat.naive_syn_timebound = timebound;
      Stat.naive_syn_success = (match res with Some x -> true | None -> false);
      Stat.naive_syn_time = (match res with Some x -> x | None -> -1.0);
    }
  in
  Stat.save_naive_syn_result result;
  (* let () = Synthesis.save_progs name progs in *)
  ()

let do_parse ?(num_expected = 1) name source_file () =
  let code = read_source_file source_file () in
  (* let () = Printf.printf "%s\n" (layout_structure code) in *)
  let env = Ntypecheck.(struct_check init_env code) in
  let () = Stat.init_algo_complexity () in
  (* let () = test_prop env "timeout" () in *)
  let () = Stat.dump_complexity env name in
  (* let () = Synthesis.save_progs name progs in *)
  ()

let syn_term source_file output_file () =
  let code = read_source_file source_file () in
  (* let () = Printf.printf "%s\n" (layout_structure code) in *)
  let env = Ntypecheck.(struct_check init_env code) in
  let () = Printf.printf "%s\n" (layout_syn_env env) in
  let () = Stat.init_algo_complexity () in
  let start_time = Sys.time () in
  let term = Synthesis.synthesize env in
  let exec_time = Sys.time () -. start_time in
  let () = Pp.printf "@{<bold>synthesis time: %f@}\n" exec_time in
  ()

let benchmark_convension task_name benchname =
  let source_file = spf "benchmarks/%s/task.ml" benchname in
  let output_file = spf "output/%s" task_name in
  let stat_file = spf "stat/.%s.json" task_name in
  let pheader = spf "benchmarks/%s/pheader.ml" benchname in
  let poutput = spf "penv/%s/PSyn/SynClient.p" benchname in
  (source_file, output_file, stat_file, pheader, poutput)

let syn_benchmark task_name benchname () =
  let source_file, output_file, stat_file, _, _ =
    benchmark_convension task_name benchname
  in
  let code = read_source_file source_file () in
  (* let () = Printf.printf "%s\n" (layout_structure code) in *)
  let env = Ntypecheck.(struct_check init_env code) in
  let () = Printf.printf "%s\n" (layout_syn_env env) in
  let () = Stat.init_algo_complexity () in
  let start_time = Sys.time () in
  let term = Synthesis.synthesize env in
  let exec_time = Sys.time () -. start_time in
  ()

(* let syn_term_timeout source_file output_file timebound () =
  let code = read_source_file source_file () in
  let () = Pp.printf "@{<bold>Time bound:@} %f\n" timebound in
  (* let () = _die [%here] in *)
  let env = Ntypecheck.(struct_check init_env code) in
  let () = Printf.printf "%s\n" (layout_syn_env env) in
  let start_time = Sys.time () in
  let terms = Synthesis.syn_timeout timebound env in
  let exec_time = Sys.time () -. start_time in
  let avg_time = exec_time /. float_of_int (ListLabels.length terms) in
  let () = Pp.printf "@{<bold>synthesis time: %f@}\n" avg_time in
  List.iteri
    (fun i term ->
      let output_file = spf "%s_%i.scm" output_file i in
      let oc = Out_channel.open_text output_file in
      try
        Sexplib.Sexp.output oc @@ sexp_of_term term;
        Out_channel.close oc
      with e ->
        Out_channel.close oc;
        raise e)
    terms *)

let load_syn_result source_file output_file =
  let code = read_source_file source_file () in
  let env = Ntypecheck.(struct_check init_env code) in
  let () = Printf.printf "%s\n" (layout_syn_env env) in
  let ic = In_channel.open_text output_file in
  let sexp = Sexplib.Sexp.load_sexp output_file in
  let term = term_of_sexp sexp in
  (env, term)

let eval_aux source_file output_file () =
  let output_file = spf "%s.scm" output_file in
  let env, term = load_syn_result source_file output_file in
  let () = Printf.printf "%s\n" (layout_term term) in
  ((env, term), (0.0, 0.0))

let eval source_file output_file () =
  let _, rate = eval_aux source_file output_file () in
  ()

let eval_benchmark task_name benchname () =
  let source_file, output_file, stat_file, _, _ =
    benchmark_convension task_name benchname
  in
  let (env, term), (rate, n_retry) = eval_aux source_file output_file () in
  let () = Stat.update_when_eval rate n_retry stat_file in
  ()

let compile_to_p_aux source_file output_file p_output_file () =
  let output_file = spf "%s.scm" output_file in
  (* let p_tyctx = *)
  (*   ocaml_structure_to_p_tyctx *)
  (*     (Oparse.parse_imp_from_file ~sourcefile:pheader_file) *)
  (* in *)
  let env, term = load_syn_result source_file output_file in
  let content = Pbackend.compile_syn_result env term in
  let oc = open_out p_output_file in
  let () =
    try
      Printf.fprintf oc "%s\n" content;
      close_out oc
    with e ->
      close_out oc;
      raise e
  in
  ()

let compile_to_p task_name benchname =
  let source_file, output_file, _, _, p_output_file =
    benchmark_convension task_name benchname
  in
  compile_to_p_aux source_file output_file p_output_file
(* let output_file = spf "%s.scm" output_file in *)

let show_term output_file () =
  let ic = In_channel.open_text output_file in
  let sexp = Sexplib.Sexp.load_sexp output_file in
  let term = term_of_sexp sexp in
  let () =
    Pp.printf "@{<bold>synthesized program:@}\n%s\n" (layout_term term)
  in
  ()

let two_param message f =
  Command.basic ~summary:message
    Command.Let_syntax.(
      let%map_open config_file =
        flag "config"
          (optional_with_default Myconfig.default_meta_config_path regular_file)
          ~doc:"config file path"
      and file1 = anon ("file1" %: regular_file)
      and source_file = anon ("source_code_file" %: regular_file) in
      let () = Myconfig.meta_config_path := config_file in
      f file1 source_file)

let tag_and_file message f =
  Command.basic ~summary:message
    Command.Let_syntax.(
      let%map_open config_file =
        flag "config"
          (optional_with_default Myconfig.default_meta_config_path regular_file)
          ~doc:"config file path"
      and file1 = anon ("file1" %: string)
      and source_file = anon ("source_code_file" %: regular_file) in
      let () = Myconfig.meta_config_path := config_file in
      f file1 source_file)

let tag_and_file_int message f =
  Command.basic ~summary:message
    Command.Let_syntax.(
      let%map_open config_file =
        flag "config"
          (optional_with_default Myconfig.default_meta_config_path regular_file)
          ~doc:"config file path"
      and file1 = anon ("file1" %: string)
      and source_file = anon ("source_code_file" %: regular_file)
      and num = anon ("int" %: int) in
      let () = Myconfig.meta_config_path := config_file in
      f file1 source_file num)

let three_param message f =
  Command.basic ~summary:message
    Command.Let_syntax.(
      let%map_open config_file =
        flag "config"
          (optional_with_default Myconfig.default_meta_config_path regular_file)
          ~doc:"config file path"
      and file1 = anon ("file2" %: regular_file)
      and file2 = anon ("file3" %: string)
      and file3 = anon ("file3" %: string) in
      let () = Myconfig.meta_config_path := config_file in
      f file1 file2 file3)

let zero_param message f =
  Command.basic ~summary:message
    Command.Let_syntax.(
      let%map_open config_file =
        flag "config"
          (optional_with_default Myconfig.default_meta_config_path regular_file)
          ~doc:"config file path"
      in
      let () = Myconfig.meta_config_path := config_file in
      f)

let one_param message f =
  Command.basic ~summary:message
    Command.Let_syntax.(
      let%map_open config_file =
        flag "config"
          (optional_with_default Myconfig.default_meta_config_path regular_file)
          ~doc:"config file path"
      and source_file = anon ("source_code_file" %: regular_file) in
      let () = Myconfig.meta_config_path := config_file in
      f source_file)

let one_param message f =
  Command.basic ~summary:message
    Command.Let_syntax.(
      let%map_open config_file =
        flag "config"
          (optional_with_default Myconfig.default_meta_config_path regular_file)
          ~doc:"config file path"
      and source_file = anon ("source_code_file" %: regular_file) in
      let () = Myconfig.meta_config_path := config_file in
      f source_file)

let one_param_string message f =
  Command.basic ~summary:message
    Command.Let_syntax.(
      let%map_open config_file =
        flag "config"
          (optional_with_default Myconfig.default_meta_config_path regular_file)
          ~doc:"config file path"
      and source_file = anon ("source_code_file" %: string) in
      let () = Myconfig.meta_config_path := config_file in
      f source_file)

let param_string_int message f =
  Command.basic ~summary:message
    Command.Let_syntax.(
      let%map_open config_file =
        flag "config"
          (optional_with_default Myconfig.default_meta_config_path regular_file)
          ~doc:"config file path"
      and str_option = anon ("string" %: string)
      and int_option = anon ("int" %: int) in
      let () = Myconfig.meta_config_path := config_file in
      f str_option int_option)

let file_and_string message f =
  Command.basic ~summary:message
    Command.Let_syntax.(
      let%map_open config_file =
        flag "config"
          (optional_with_default Myconfig.default_meta_config_path regular_file)
          ~doc:"config file path"
      and source_file = anon ("source_code_file" %: regular_file)
      and file1 = anon ("file1" %: string) in
      let () = Myconfig.meta_config_path := config_file in
      f source_file file1)

let string_and_string message f =
  Command.basic ~summary:message
    Command.Let_syntax.(
      let%map_open config_file =
        flag "config"
          (optional_with_default Myconfig.default_meta_config_path regular_file)
          ~doc:"config file path"
      and source_file = anon ("source_code_file" %: string)
      and file1 = anon ("file1" %: string) in
      let () = Myconfig.meta_config_path := config_file in
      f source_file file1)

let string_string_int_float message f =
  Command.basic ~summary:message
    Command.Let_syntax.(
      let%map_open config_file =
        flag "config"
          (optional_with_default Myconfig.default_meta_config_path regular_file)
          ~doc:"config file path"
      and source_file = anon ("source_code_file" %: string)
      and file1 = anon ("file1" %: string)
      and size = anon ("size" %: int)
      and timebound = anon ("timebound" %: float) in
      let () = Myconfig.meta_config_path := config_file in
      f source_file file1 size timebound)

let timeout_param message f =
  Command.basic ~summary:message
    Command.Let_syntax.(
      let%map_open config_file =
        flag "config"
          (optional_with_default Myconfig.default_meta_config_path regular_file)
          ~doc:"config file path"
      and source_file = anon ("source_code_file" %: regular_file)
      and file1 = anon ("file1" %: string)
      and timebound = anon ("timebound" %: float) in
      let () = Myconfig.meta_config_path := config_file in
      f source_file file1 timebound)

let four_param_string message f =
  Command.basic ~summary:message
    Command.Let_syntax.(
      let%map_open config_file =
        flag "config"
          (optional_with_default Myconfig.default_meta_config_path regular_file)
          ~doc:"config file path"
      and file1 = anon ("file1" %: regular_file)
      and file2 = anon ("file2" %: regular_file)
      and file3 = anon ("file3" %: regular_file)
      and file4 = anon ("file4" %: string) in
      let () = Myconfig.meta_config_path := config_file in
      f file1 file2 file3 file4)

type eval_result =
  | SampleResult of int * float * float
  | UntilDetectResult of int * Interpreter.msg list

let random_stat_file = "stat/.run_random_p.json"
let syn_stat_file = "stat/.run_syn.json"
let default_stat_file = "stat/.run_default.json"

let load_eval_stat filename =
  if not (Sys.file_exists filename) then (
    let oc = open_out filename in
    output_string oc "{}";
    close_out oc);
  let ic = open_in filename in
  let len = in_channel_length ic in
  let content = really_input_string ic len in
  close_in ic;
  Yojson.Safe.from_string content

let update_eval_stat file_name (name, mode, res) =
  match res with
  | SampleResult (_, rate, time) ->
      let data = load_eval_stat file_name in
      let data =
        let open Yojson.Safe.Util in
        let assoc_list = to_assoc data in
        let entry = `List [ `Float rate; `Float time ] in
        let assoc_list =
          match List.assoc_opt name assoc_list with
          | Some _ ->
              List.filter_map
                (fun (k, v) ->
                  if String.equal k name then Some (k, entry) else Some (k, v))
                assoc_list
          | None -> assoc_list @ [ (name, entry) ]
        in
        `Assoc assoc_list
      in
      let () =
        Pp.printf "@{<bold>update eval stat:@} %s\n"
          (Yojson.Safe.to_string data)
      in
      let oc = open_out file_name in
      output_string oc (Yojson.Safe.to_string data);
      close_out oc
  | UntilDetectResult _ -> ()

let test_eval mode s (converge_bound : int) () =
  let eval f =
    match mode with
    | "sample" ->
        let n_successed, rate, exec_time =
          Interpreter.eval_sample ~number_bound:(Some converge_bound)
            ~time_bound:None f
        in
        SampleResult (n_successed, rate, exec_time)
    | "detect" ->
        let n_retry, his = Interpreter.eval_until_detect_bug converge_bound f in
        UntilDetectResult (n_retry, his)
    | _ -> _die_with [%here] "unknown mode"
  in
  let res =
    match s with
    (* | "queue" ->
      let open Adt.Queue in
      let test () = Interpreter.once (init, [ main ], check_membership_queue) in
      eval test *)
    | "stack" ->
        let open Adt.Stack in
        let main = Synthesis.load_prog s () in
        (* let main = [ rec_main ] in *)
        let test () = Interpreter.once (init, main, check_membership_stack) in
        eval test
    | "set" ->
        let open Adt.Set in
        let main = Synthesis.load_prog s () in
        let test () = Interpreter.once (init, main, check_membership_set) in
        eval test
    | "hashtable" ->
        let open Adt.Hashtable in
        let main = Synthesis.load_prog s () in
        let test () =
          Interpreter.once (init, main, check_membership_hashtable)
        in
        eval test
    (* | "hashtable" ->
        let open Adt.Hashtable_random in
        test_fn () *)
    | "filesystem" ->
        let open Adt.Filesystem in
        let main = Synthesis.load_prog s () in
        let test () = Interpreter.once (init, main, filesystem_last_delete) in
        eval test
    | "graph" ->
        let open Adt.Graph in
        let main = Synthesis.load_prog s () in
        let test () = Interpreter.once (init, main, trace_is_not_connected) in
        eval test
    | "nfa" ->
        let open Adt.Nfa in
        let main = Synthesis.load_prog s () in
        let test () = Interpreter.once (init, main, trace_is_not_nfa) in
        eval test
    | "stlc1" ->
        let open Adt.Stlc_simple in
        let main = Synthesis.load_prog s () in
        (* let main = [ main_rec ] in *)
        let test () = Interpreter.once (init, main, trace_eval_correct) in
        eval test
    | "stlc2" ->
        let open Adt.Stlc_moti in
        let main = Synthesis.load_prog s () in
        (* let main = [ main_rec ] in *)
        let test () = Interpreter.once (init, main, trace_eval_correct) in
        eval test
    | "ifc_store" ->
        let open Adt.Ifc in
        let () = set_ruleset_store () in
        let main = Synthesis.load_prog s () in
        let test () = Interpreter.once (init, main, trace_enni) in
        eval test
    | "ifc_add" ->
        let open Adt.Ifc in
        let () = set_ruleset_add () in
        let main = Synthesis.load_prog s () in
        let test () = Interpreter.once (init, main, trace_enni) in
        eval test
    | "ifc_load" ->
        let open Adt.Ifc in
        let () = set_ruleset_load () in
        let main = Synthesis.load_prog s () in
        let test () = Interpreter.once (init, main, trace_enni) in
        eval test
    | "read_cc" ->
        let open MonkeyBD in
        let open Common in
        let open ReadWriteDB in
        BackendMariaDB.MyMariaDB.maria_context "readwrite" Causal (fun () ->
            let main = Synthesis.load_prog s () in
            let test () =
              Interpreter.once
                (ReadWriteDB.init, main, Read.check_read_atomicity)
            in
            eval test)
    | "cart_rc" ->
        let open MonkeyBD in
        let open Common in
        let open Cart in
        BackendMariaDB.MyMariaDB.maria_context "cart" ReadCommitted (fun () ->
            let main = Synthesis.load_prog s () in
            let test () =
              Interpreter.once
                (CartDB.init, main, CartDB.check_isolation_level Serializable)
            in
            eval test)
    | "cart_cc" ->
        let open MonkeyBD in
        let open Common in
        let open Cart in
        BackendMariaDB.MyMariaDB.maria_context "cart" Causal (fun () ->
            let main = Synthesis.load_prog s () in
            let test () =
              Interpreter.once
                (CartDB.init, main, CartDB.check_isolation_level Serializable)
            in
            eval test)
    | "twitter_rc" ->
        let open MonkeyBD in
        let open Common in
        let open Twitter in
        BackendMariaDB.MyMariaDB.maria_context "twitter" ReadCommitted
          (fun () ->
            let main = Synthesis.load_prog s () in
            let test () =
              Interpreter.once
                ( TwitterDB.init,
                  main,
                  TwitterDB.check_isolation_level Serializable )
            in
            eval test)
    | "twitter_cc" ->
        let open MonkeyBD in
        let open Common in
        let open Twitter in
        BackendMariaDB.MyMariaDB.maria_context "twitter" Causal (fun () ->
            let main = Synthesis.load_prog s () in
            let test () =
              Interpreter.once
                ( TwitterDB.init,
                  main,
                  TwitterDB.check_isolation_level Serializable )
            in
            eval test)
    | "courseware_rc" ->
        let open MonkeyBD in
        let open Common in
        let open CoursewareDB in
        BackendMariaDB.MyMariaDB.maria_context "courseware" ReadCommitted
          (fun () ->
            let main = Synthesis.load_prog s () in
            let test () =
              Interpreter.once
                ( CoursewareDB.init,
                  main,
                  CoursewareDB.check_isolation_level Serializable )
            in
            eval test)
    | "courseware_cc" ->
        let open MonkeyBD in
        let open Common in
        let open CoursewareDB in
        BackendMariaDB.MyMariaDB.maria_context "courseware" Causal (fun () ->
            let main = Synthesis.load_prog s () in
            let test () =
              Interpreter.once
                ( CoursewareDB.init,
                  main,
                  CoursewareDB.check_isolation_level Serializable )
            in
            eval test)
    | "smallbank_rc" ->
        let open MonkeyBD in
        let open Common in
        let open Smallbank in
        BackendMariaDB.MyMariaDB.maria_context "smallbank" ReadCommitted
          (fun () ->
            let main = Synthesis.load_prog s () in
            let test () =
              Interpreter.once
                ( SmallBankDB.init,
                  main,
                  SmallBankDB.check_isolation_level Serializable )
            in
            eval test)
    | "smallbank_cc" ->
        let open MonkeyBD in
        let open Common in
        let open Smallbank in
        BackendMariaDB.MyMariaDB.maria_context "smallbank" Causal (fun () ->
            let main = Synthesis.load_prog s () in
            let test () =
              Interpreter.once
                ( SmallBankDB.init,
                  main,
                  SmallBankDB.check_isolation_level Serializable )
            in
            eval test)
    | "cart" ->
        let open MonkeyBD in
        let open Common in
        let open Cart in
        BackendMariaDB.MyMariaDB.maria_context "cart" ReadCommitted (fun () ->
            let test () =
              Interpreter.once
                (CartDB.init, main, CartDB.check_isolation_level Serializable)
            in
            eval test)
    (* | "treiber-stack" ->
        let open MonkeyBD in
        let open Common in
        let open TreiberStack in
        let test () =
          Interpreter.once
            (init Causal, [ main ], StackDB.serializable_trace_checker)
        in
        eval test *)
    | _ -> _die_with [%here] "unknown benchmark"
  in
  let () = update_eval_stat syn_stat_file (s, "syn", res) in
  ()

let test_envs =
  [
    ("stack", Adt.Stack.test_env);
    ("set", Adt.Set.test_env);
    ("filesystem", Adt.Filesystem.test_env);
    ("graph", Adt.Graph.test_env);
    ("nfa", Adt.Nfa.test_env);
    ("stlc1", Adt.Stlc_simple.test_env);
    ("stlc2", Adt.Stlc_moti.test_env);
    ("ifc_store", Adt.Ifc.test_env);
    ("ifc_add", Adt.Ifc.test_env);
    ("ifc_load", Adt.Ifc.test_env);
    ("read_cc", MonkeyBD.Read.test_env Causal);
    ("cart_rc", MonkeyBD.Cart.test_env ReadCommitted);
    ("cart_cc", MonkeyBD.Cart.test_env Causal);
    ("courseware_rc", MonkeyBD.Courseware.test_env ReadCommitted);
    ("courseware_cc", MonkeyBD.Courseware.test_env Causal);
    ("smallbank_rc", MonkeyBD.Smallbank.test_env ReadCommitted);
    ("smallbank_cc", MonkeyBD.Smallbank.test_env Causal);
    ("twitter_rc", MonkeyBD.Twitter.test_env ReadCommitted);
    ("twitter_cc", MonkeyBD.Twitter.test_env Causal);
  ]

let default_random_test_config =
  [
    ("numOp", 15);
    ("numOpGraph", 20);
    ("numElem", 10);
    ("numSet", 30);
    ("numApp", 5);
    ("tyDepthBound", 5);
    ("constRange", 10);
    ("numUserDB", 5);
    ("numItemDB", 5);
    ("numOpDB", 10);
    ("numStudentDB", 5);
    ("numCourseDB", 5);
    ("numBalanceDB", 20);
  ]

let test_random mode s converge_bound time_bound () =
  let () = BackendMariaDB.MyMariaDB.set_single_connection_mode true in
  let eval f =
    match mode with
    | "detect" ->
        let converge_bound =
          match converge_bound with
          | Some x -> x
          | None -> _die_with [%here] "converge bound not set"
        in
        let n_retry, his = Interpreter.eval_until_detect_bug converge_bound f in
        UntilDetectResult (n_retry, his)
    | "sample" ->
        let n_successed, rate, exec_time =
          Interpreter.eval_sample ~number_bound:converge_bound ~time_bound f
        in
        SampleResult (n_successed, rate, exec_time)
    | _ -> _die_with [%here] "unknown mode"
  in
  let res =
    match s with
    | "hashtable" ->
        let open Adt.Hashtable_random in
        test_fn ()
    (* | "hashtable" ->
        let open Adt.Hashtable in
        let test () =
          Interpreter.seq_random_test
            ( init,
              (fun () -> randomTest { numKeys = 8; numVals = 10; numOp = 20 }),
              check_membership_hashtable )
        in
        eval test *)
    | _ -> (
        let env = List.assoc_opt s test_envs in
        match env with
        | None -> _die_with [%here] "unknown benchmark"
        | Some env ->
            if env.if_concurrent then
              match env.database_ctx with
              | None -> _die_with [%here] "isolation not set"
              | Some { dbname; isolation } ->
                  BackendMariaDB.MyMariaDB.maria_context dbname isolation
                    (fun () ->
                      let test () =
                        Interpreter.random_test
                          ( env.init_test_env,
                            (fun () ->
                              env.random_test_gen default_random_test_config),
                            env.property )
                      in
                      eval test)
            else
              let test () =
                Interpreter.seq_random_test
                  ( env.init_test_env,
                    (fun () -> env.random_test_gen default_random_test_config),
                    env.property )
              in
              eval test)
  in
  let () = update_eval_stat random_stat_file (s, "random", res) in
  ()

let cmds =
  [
    ("test-eval", param_string_int "test eval" (test_eval "detect"));
    ( "test-random",
      param_string_int "test random" (fun name n ->
          test_random "detect" name (Some n) None) );
    ("sample-syn", param_string_int "sample syn" (test_eval "sample"));
    ( "sample-random",
      param_string_int "sample random" (fun name n ->
          test_random "sample" name None (Some n)) );
    ("read-syn", one_param "read syn" read_syn);
    ("do-syn", tag_and_file "read syn" do_syn);
    ("do-naive", string_string_int_float "do naive syn" do_naive_syn);
    ("do-parse", tag_and_file "read parse" do_parse);
    ("syn-one", file_and_string "syn one" syn_term);
    ("syn-benchmark", string_and_string "run benchmark" syn_benchmark);
    ("eval-benchmark", string_and_string "run benchmark" eval_benchmark);
    (* ("syn-timeout", timeout_param "syn timeout" syn_term_timeout); *)
    ("eval", file_and_string "eval" eval);
    (* ("compile-to-p", four_param_string "compile to p language" compile_to_p); *)
    ("compile-to-p", string_and_string "compile to p language" compile_to_p);
    ("show-term", one_param "show term" show_term);
    ("test-db", one_param_string "run cart" BackendMariaDB.test_cart);
    ( "test-non-repeatable-read",
      one_param_string "run non repeatable read"
        BackendMariaDB.test_non_repeatable_read );
    ( "test-dirty-read",
      one_param_string "run dirty read" BackendMariaDB.test_dirty_read );
    ("test-causal", one_param_string "run causal" BackendMariaDB.test_causal);
    ( "test-dirty-read-concurrent",
      one_param_string "run dirty read concurrent"
        BackendMariaDB.test_dirty_read_concurrent );
    ("test-stuck", one_param_string "run stuck" BackendMariaDB.test_stuck);
    (* ("read-automata", one_param "read_automata" read_automata); *)
    (* ("read-sfa", one_param "read_sfa" read_sfa); *)
    (* ("read-p", one_param "read_p" read_p); *)
    (* ("read-p-sfa", three_param "read_p" read_p_and_spec); *)
    (* ("random-p-sfa", three_param "read_p" random_read_p_and_spec); *)
    (* ("read-p-wrapper", file_and_string "p-wrapper" p_wrapper); *)
    (* ("read-p-repo", one_param_string "p-wrapper" read_p_repo); *)
  ]
