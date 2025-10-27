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

let read_syn source_file () =
  let code = read_source_file source_file () in
  (* let () = Printf.printf "%s\n" (layout_structure code) in *)
  let env = Ntypecheck.(struct_check init_env code) in
  let () = Printf.printf "%s\n" (layout_syn_env env) in
  let () = Stat.init_algo_complexity () in
  let term = Synthesis.synthesize env in
  ()

let do_syn name source_file () =
let _ = Pp.printf "@{<yellow>cre.ml:@}  43 (%s)\n" source_file in
  let code = read_source_file source_file () in
let _ = Pp.printf "@{<yellow>cre.ml:@}  45\n" in
  (* let () = Printf.printf "%s\n" (layout_structure code) in *)
  let env = Ntypecheck.(struct_check init_env code) in
let _ = Pp.printf "@{<yellow>cre.ml:@}  48\n" in
  let () = Printf.printf "%s\n" (layout_syn_env env) in
  let () = Stat.init_algo_complexity () in
  let progs = Synthesis.synthesize env name in
  let () = Synthesis.save_progs name progs in
  ()

let handle_syn_result env (exec_time, output_file, term) =
  let () = Stat.dump (env, term) ".stat.json" in
  let output_file = spf "%s.scm" output_file in
  let oc = Out_channel.open_text output_file in
  try
    Sexplib.Sexp.output oc @@ sexp_of_term term;
    Out_channel.close oc
  with e ->
    Out_channel.close oc;
    raise e

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

let benchmark_convension benchname =
  let source_file = spf "benchmarks/%s/task.ml" benchname in
  let output_file = spf "output/%s" benchname in
  let stat_file = spf "stat/.%s.json" benchname in
  let pheader = spf "benchmarks/%s/pheader.ml" benchname in
  let poutput = spf "penv/%s/PSyn/SynClient.p" benchname in
  (source_file, output_file, stat_file, pheader, poutput)

let syn_benchmark benchname () =
  let source_file, output_file, stat_file, _, _ =
    benchmark_convension benchname
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

(* let eval_aux source_file output_file () =
  let output_file = spf "%s.scm" output_file in
  let env, term = load_syn_result source_file output_file in
  let () = Printf.printf "%s\n" (layout_term term) in
  let () = Interpreter.interpret env term in
  let rate = Interpreter.interpret_sample env term 1000 in
  ((env, term), rate) *)

let eval_aux source_file output_file () =
  let output_file = spf "%s.scm" output_file in
  let env, term = load_syn_result source_file output_file in
  let () = Printf.printf "%s\n" (layout_term term) in
  (0, 0.0)

let eval source_file output_file () =
  let _, rate = eval_aux source_file output_file () in
  ()

let eval_benchmark benchname () =
  let source_file, output_file, stat_file, _, _ =
    benchmark_convension benchname
  in
  (* let (env, term), (rate, n_retry) = eval_aux source_file output_file () in
  let () = Stat.update_when_eval (env, term) rate n_retry stat_file in *)
  ()

let compile_to_p_aux source_file output_file p_output_file () =
  let output_file = spf "%s.scm" output_file in
  (* let p_tyctx = *)
  (*   ocaml_structure_to_p_tyctx *)
  (*     (Oparse.parse_imp_from_file ~sourcefile:pheader_file) *)
  (* in *)
  let env, term = load_syn_result source_file output_file in
  let content = _die_with [%here] "unimp" in
  (* Pbackend.compile_syn_result env term in *)
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

let compile_to_p benchname =
  let source_file, output_file, _, _, p_output_file =
    benchmark_convension benchname
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

let two_param_string message f =
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

let test_eval s converge_bound () =
  let eval = Interpreter.eval_until_detect_bug converge_bound in
  match s with
  (* | "queue" ->
      let open Adt.Queue in
      let test () = Interpreter.once (init, [ main ], check_membership_queue) in
      let _ = eval test in
      () *)
  | "stack" ->
      let open Adt.Stack in
      let main = Synthesis.load_progs s () in
      (* let main = [ rec_main ] in *)
      let test () = Interpreter.once (init, main, check_membership_stack) in
      let _ = eval test in
      ()
  (* | "set" ->
  | "set" ->
      let open Adt.Set in
      let test () = Interpreter.once (init, [ main ], check_membership_set) in
      let _ = eval test in
      () *)
  | "hashtable" ->
      let open Adt.Hashtable in
      let main = Synthesis.load_progs s () in
      let test () = Interpreter.once (init, main, check_membership_hashtable) in
      let _ = eval test in
      ()
  | "filesystem" ->
      let open Adt.Filesystem in
      let main = Synthesis.load_progs s () in
      let test () = Interpreter.once (init, main, filesystem_last_delete) in
      let _ = eval test in
      ()
  | "graph" ->
      let open Adt.Graph in
      let main = Synthesis.load_progs s () in
      let test () = Interpreter.once (init, main, trace_is_not_connected) in
      let _ = eval test in
      ()
  | "nfa" ->
      let open Adt.Nfa in
      let main = Synthesis.load_progs s () in
      let test () = Interpreter.once (init, main, trace_is_not_nfa) in
      let _ = eval test in
      ()
  | "stlc" ->
      let open Adt.Stlc_moti in
      let main = Synthesis.load_progs s () in
      (* let main = [ main_rec ] in *)
      let test () = Interpreter.once (init, main, trace_eval_correct) in
      let _ = eval test in
      ()
  | "ifc_store" ->
      let open Adt.Ifc in
      let () = set_ruleset_store () in
      let main = Synthesis.load_progs s () in
      let test () = Interpreter.once (init, main, trace_enni) in
      let _ = eval test in
      ()
  | "ifc_add" ->
      let open Adt.Ifc in
      let () = set_ruleset_add () in
      let main = Synthesis.load_progs s () in
      let test () = Interpreter.once (init, main, trace_enni) in
      let _ = eval test in
      ()
  | "ifc_load" ->
      let open Adt.Ifc in
      let () = set_ruleset_load () in
      let main = Synthesis.load_progs s () in
      let test () = Interpreter.once (init, main, trace_enni) in
      let _ = eval test in
      ()
  | "ifcStore" ->
      let open Adt.Ifc in
      let () = test_store_b_main () in
      ()
  | "ifcAdd" ->
      let open Adt.Ifc in
      let () = test_add_main () in
      ()
  | "ifcLoad" ->
      let open Adt.Ifc in
      let () = test_load_main () in
      ()
  | "cart_rc" ->
      let open MonkeyBD in
      let open Common in
      let open Cart in
      BackendMariaDB.MyMariaDB.maria_context "cart" ReadCommitted (fun () ->
          let main = Synthesis.load_progs s () in
          let test () =
            Interpreter.once
              (CartDB.init, main, CartDB.check_isolation_level Serializable)
          in
          let _ = eval test in
          ())
  | "cart_cc" ->
      let open MonkeyBD in
      let open Common in
      let open Cart in
      BackendMariaDB.MyMariaDB.maria_context "cart" Causal (fun () ->
          let main = Synthesis.load_progs s () in
          let test () =
            Interpreter.once
              (CartDB.init, main, CartDB.check_isolation_level Serializable)
          in
          let _ = eval test in
          ())
  | "twitter_rc" ->
      let open MonkeyBD in
      let open Common in
      let open Twitter in
      BackendMariaDB.MyMariaDB.maria_context "twitter" ReadCommitted (fun () ->
          let main = Synthesis.load_progs s () in
          let test () =
            Interpreter.once
              (TwitterDB.init, main, TwitterDB.check_isolation_level Serializable)
          in
          let _ = eval test in
          ())
  | "twitter_cc" ->
      let open MonkeyBD in
      let open Common in
      let open Twitter in
      BackendMariaDB.MyMariaDB.maria_context "twitter" Causal (fun () ->
          let main = Synthesis.load_progs s () in
          let test () =
            Interpreter.once
              (TwitterDB.init, main, TwitterDB.check_isolation_level Serializable)
          in
          let _ = eval test in
          ())
    | "smallbank_cc" ->
      let open MonkeyBD in
      let open Common in
      let open Smallbank in
      BackendMariaDB.MyMariaDB.maria_context "smallbank" Causal (fun () ->
          let main = Synthesis.load_progs s () in
          let test () =
            Interpreter.once
              (SmallBankDB.init, main, SmallBankDB.check_isolation_level Serializable)
          in
          let _ = eval test in
          ())
  (* | "courseware_rc" ->
      let open MonkeyBD in
      let open Common in
      let open CoursewareDB in
      BackendMariaDB.MyMariaDB.maria_context "courseware" ReadCommitted
        (fun () ->
          let main = Synthesis.load_progs s () in
          let test () =
            Interpreter.once
              ( CoursewareDB.init,
                main,
                CoursewareDB.check_isolation_level Serializable )
          in
          let _ = eval test in
          ())
  | "courseware_cc" ->
      let open MonkeyBD in
      let open Common in
      let open CoursewareDB in
      BackendMariaDB.MyMariaDB.maria_context "courseware" Causal (fun () ->
          let main = Synthesis.load_progs s () in
          let test () =
            Interpreter.once
              ( CoursewareDB.init,
                main,
                CoursewareDB.check_isolation_level Serializable )
          in
          let _ = eval test in
          ())
  | "cart" ->
      let open MonkeyBD in
      let open Common in
      let open Cart in
      BackendMariaDB.MyMariaDB.maria_context "cart" ReadCommitted (fun () ->
          let test () =
            Interpreter.once
              (CartDB.init, [ main ], CartDB.check_isolation_level Serializable)
          in
          let _ = eval test in
          ())
  (* | "smallbank" ->
      let open MonkeyBD in
      let open Common in
      let open Smallbank in
      let test () =
        Interpreter.once
          (init Causal, [ main ], SmallbankDB.serializable_trace_checker)
      in
      let _ = eval test in
      () *)
  (* | "twitter" ->
      let open MonkeyBD in
      let open Common in
      let open Twitter in
      let test () =
        Interpreter.once
          (init Causal, [ main ], TwitterDB.serializable_trace_checker)
      in
      let _ = eval test in
      ()
  | "courseware" ->
      let open MonkeyBD in
      let open Common in
      let open Courseware in
      let test () =
        Interpreter.once
          (init Causal, [ main ], CoursewareDB.serializable_trace_checker)
      in
      let _ = eval test in
      ()
  | "treiber-stack" ->
      let open MonkeyBD in
      let open Common in
      let open TreiberStack in
      let test () =
        Interpreter.once
          (init Causal, [ main ], StackDB.serializable_trace_checker)
      in
      let _ = eval test in
      () *)
  | _ -> _die_with [%here] "unknown benchmark"

let test_envs =
  [
    ("stack", Adt.Stack.test_env);
    ("filesystem", Adt.Filesystem.test_env);
    ("graph", Adt.Graph.test_env);
    ("nfa", Adt.Nfa.test_env);
    ("stlc", Adt.Stlc_moti.test_env);
    ("ifc_store", Adt.Ifc.test_env);
    ("ifc_add", Adt.Ifc.test_env);
    ("ifc_load", Adt.Ifc.test_env);
    ("cart_rc", MonkeyBD.Cart.test_env ReadCommitted);
    ("cart_cc", MonkeyBD.Cart.test_env Causal);
    ("courseware_rc", MonkeyBD.Courseware.test_env ReadCommitted);
    ("courseware_cc", MonkeyBD.Courseware.test_env Causal);
  ]

let default_random_test_config =
  [
    ("numOp", 15);
    ("numElem", 10);
    ("numApp", 5);
    ("tyDepthBound", 5);
    ("constRange", 10);
    ("numUserDB", 5);
    ("numItemDB", 5);
    ("numOpDB", 3);
    ("numStudentDB", 5);
    ("numCourseDB", 5);
  ]

let test_random s converge_bound () =
  let eval = Interpreter.eval_until_detect_bug converge_bound in
  match s with
  (* | "smallbank" ->
      let open MonkeyBD in
      let open Common in
      let open Smallbank in
      let test () =
        Interpreter.random_test
          ( init Causal,
            (fun () -> random_user { numUser = 4; numOp = 2 }),
            SmallbankDB.serializable_trace_checker )
      in
      let _ = eval test in
      ()
  | "treiber-stack" ->
      let open MonkeyBD in
      let open Common in
      let open TreiberStack in
      let test () =
        Interpreter.random_test
          ( init ReadCommitted,
            (fun () -> qc_stack { numElems = 4; numOp = 2 }),
            StackDB.serializable_trace_checker )
      in
      let _ = eval test in
      () *)
  | "twitter_rc" ->
      let open MonkeyBD in
      let open Common in
      let open Twitter in
      BackendMariaDB.MyMariaDB.maria_context "twitter" ReadCommitted (fun () ->
          let test () =
            Interpreter.random_test
              ( TwitterDB.init,
                (fun () -> random_user { numUser = 4; numTweet = 4; numOp = 15 }),
                TwitterDB.check_isolation_level Serializable )
          in
          let _ = eval test in
          ())
  | "twitter_cc" ->
      let open MonkeyBD in
      let open Common in
      let open Twitter in
      BackendMariaDB.MyMariaDB.maria_context "twitter" Causal (fun () ->
          let test () =
            Interpreter.random_test
              ( TwitterDB.init,
                (fun () -> random_user { numUser = 4; numTweet = 4; numOp = 15 }),
                TwitterDB.check_isolation_level Serializable )
          in
          let _ = eval test in
          ())
    (*| "smallbank_rc" ->
      let open MonkeyBD in
      let open Common in
      let open SmallBank in
      BackendMariaDB.MyMariaDB.maria_context "smallbank" ReadCommitted (fun () ->
          let test () =
            Interpreter.random_test
              ( TwitterDB.init,
                (fun () -> random_user { numUser = 4; numTweet = 4; numOp = 15 }),
                TwitterDB.check_isolation_level Serializable )
          in
          let _ = eval test in
          ()) *)
    (*| "smallbank_cc" ->
      let open MonkeyBD in
      let open Common in
      let open SmallBank in
      BackendMariaDB.MyMariaDB.maria_context "smallbank" Causal (fun () ->
          let test () =
            Interpreter.random_test
              ( TwitterDB.init,
                (fun () -> random_user { numUser = 4; numTweet = 4; numOp = 15 }),
                TwitterDB.check_isolation_level Serializable )
          in
          let _ = eval test in
          ()) *)
  (*| "courseware" ->
      let open MonkeyBD in
      let open Common in
      let open Courseware in
      let open CoursewareDB in
      let test () =
        Interpreter.random_test
          ( CoursewareDB.init,
            (fun () ->
              random_operations { numStudent = 4; numCourse = 4; numOp = 2 }),
            check_isolation_level Serializable )
      in
      let _ = eval test in
      () *)
  | "hashtable" ->
      let open Adt.Hashtable in
      let test () =
        Interpreter.seq_random_test
          ( init,
            (fun () -> randomTest { numKeys = 8; numVals = 10; numOp = 20 }),
            check_membership_hashtable )
      in
      let _ = eval test in
      ()
  (* | "cart_rc" ->
      let open MonkeyBD in
      let open Common in
      let open Cart in
      BackendMariaDB.MyMariaDB.maria_context "cart" ReadCommitted (fun () ->
          let test () =
            Interpreter.random_test
              ( CartDB.init,
                (fun () -> random_user { numUser = 4; numItem = 4; numOp = 3 }),
                CartDB.check_isolation_level Serializable )
          in
          let _ = eval test in
          ())
  | "cart_cc" ->
      let open MonkeyBD in
      let open Common in
      let open Cart in
      BackendMariaDB.MyMariaDB.maria_context "cart" Causal (fun () ->
          let test () =
            Interpreter.random_test
              ( CartDB.init,
                (fun () -> random_user { numUser = 4; numItem = 4; numOp = 3 }),
                CartDB.check_isolation_level Serializable )
          in
          let _ = eval test in
          ()) *)
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
                    let _ = eval test in
                    ())
          else
            let test () =
              Interpreter.seq_random_test
                ( env.init_test_env,
                  (fun () -> env.random_test_gen default_random_test_config),
                  env.property )
            in
            let _ = eval test in
            ())

(* | "filesystem" ->
            let open Adt.Filesystem in
            let test () =
              Interpreter.seq_random_test
                (init, (fun () -> randomTest { numOp = 15 }), filesystem_last_delete)
            in
            let _ = eval test in
            ()
        | "graph" ->
            let open Adt.Graph in
            let test () =
              Interpreter.seq_random_test
                (init, (fun () -> randomTest { numOp = 15 }), trace_is_not_connected)
            in
            let _ = eval test in
            ()
        | "nfa" ->
            let open Adt.Nfa in
            let test () =
              Interpreter.seq_random_test
                (init, (fun () -> randomTest { numOp = 15 }), trace_is_not_nfa)
            in
            let _ = eval test in
            ()
        | "stlc" ->
            let open Adt.Stlc in
            (* let () = testAst () in
            let () = _die_with [%here] "done" in *)
            let test () =
              Interpreter.seq_random_test
                ( init,
                  (fun () ->
                    randomTest { numApp = 4; tyDepthBound = 4; constRange = 4 }),
                  trace_eval_correct )
            in
            let _ = eval test in
            ()
        | "ifc_store" ->
            let open Adt.Ifc in
            let () = set_ruleset_store () in
            let test () =
              Interpreter.seq_random_test
                (init, (fun () -> randomTest { numOp = 15 }), trace_enni)
            in
            let _ = eval test in
            ()
        | "ifc_add" ->
            let open Adt.Ifc in
            let () = set_ruleset_add () in
            let test () =
              Interpreter.seq_random_test
                (init, (fun () -> randomTest { numOp = 15 }), trace_enni)
            in
            let _ = eval test in
            ()
        | "ifc_load" ->
            let open Adt.Ifc in
            let () = set_ruleset_load () in
            let test () =
              Interpreter.seq_random_test
                (init, (fun () -> randomTest { numOp = 15 }), trace_enni)
            in
            let _ = eval test in
            ()
        | "stack" ->
            let open Adt.Stack in
            let test () =
              Interpreter.seq_random_test
                ( init,
                  (fun () -> randomTest { numElem = 10; numOp = 15 }),
                  check_membership_stack )
            in
            let _ = eval test in
            () *)

let cmds =
  [
    ("test-eval", param_string_int "test eval" test_eval);
    ("test-random", param_string_int "test random" test_random);
    ("read-syn", one_param "read syn" read_syn);
    ("do-syn", tag_and_file "read syn" do_syn);
    ("syn-one", two_param_string "syn one" syn_term);
    ("syn-benchmark", one_param_string "run benchmark" syn_benchmark);
    ("eval-benchmark", one_param_string "run benchmark" eval_benchmark);
    (* ("syn-timeout", timeout_param "syn timeout" syn_term_timeout); *)
    ("eval", two_param_string "eval" eval);
    (* ("compile-to-p", four_param_string "compile to p language" compile_to_p); *)
    ("compile-to-p", one_param_string "compile to p language" compile_to_p);
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
    (* ("read-p-wrapper", two_param_string "p-wrapper" p_wrapper); *)
    (* ("read-p-repo", one_param_string "p-wrapper" read_p_repo); *)
  ]
