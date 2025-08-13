open Caux
open Language
open Ntypecheck
open OcamlParser
open Zdatatype

let parse = Oparse.parse_imp_from_file

let read_ocaml_file source_file () =
  let code = Oparse.parse_imp_from_file ~sourcefile:source_file in
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
  let term = Synthesis.syn_one env in
  ()

let syn_term source_file output_file () =
  let code = read_source_file source_file () in
  (* let () = Printf.printf "%s\n" (layout_structure code) in *)
  let env = Ntypecheck.(struct_check init_env code) in
  let () = Printf.printf "%s\n" (layout_syn_env env) in
  let () = Stat.init_algo_complexity () in
  let start_time = Sys.time () in
  let term = Synthesis.syn_one env in
  let exec_time = Sys.time () -. start_time in
  let () = Pp.printf "@{<bold>synthesis time: %f@}\n" exec_time in
  let () = Stat.dump (env, term) ".stat.json" in
  let output_file = spf "%s.scm" output_file in
  let oc = Out_channel.open_text output_file in
  try
    Sexplib.Sexp.output oc @@ sexp_of_term term;
    Out_channel.close oc
  with e ->
    Out_channel.close oc;
    raise e

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
  let term = Synthesis.syn_one env in
  let exec_time = Sys.time () -. start_time in
  let () = Pp.printf "@{<bold>synthesis time: %f@}\n" exec_time in
  let () = Stat.dump (env, term) stat_file in
  let output_file = spf "%s.scm" output_file in
  let oc = Out_channel.open_text output_file in
  try
    Sexplib.Sexp.output oc @@ sexp_of_term term;
    Out_channel.close oc
  with e ->
    Out_channel.close oc;
    raise e

let syn_term_timeout source_file output_file timebound () =
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
    terms

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

let test_eval s () =
  match s with
  | "twitter" ->
      let open MonkeyBD in
      let open Common in
      let open Twitter in
      let test () =
        Interpreter.once
          (init Causal, main, TwitterDB.serializable_trace_checker)
      in
      let _ = Interpreter.eval_until_detect_bug test in
      ()
  | "cart" ->
      let open MonkeyBD in
      let open Common in
      let open Cart in
      let test () =
        Interpreter.once (init Causal, main, CartDB.serializable_trace_checker)
      in
      let _ = Interpreter.eval_until_detect_bug test in
      ()
  | "courseware" ->
      let open MonkeyBD in
      let open Common in
      let open Courseware in
      let test () =
        Interpreter.once
          (init Causal, main, CoursewareDB.serializable_trace_checker)
      in
      let _ = Interpreter.eval_until_detect_bug test in
      ()
  | "treiber-stack" ->
      let open MonkeyBD in
      let open Common in
      let open TreiberStack in
      let test () =
        Interpreter.once (init Causal, main, StackDB.serializable_trace_checker)
      in
      let _ = Interpreter.eval_until_detect_bug test in
      ()
  | "stlc" ->
      let open Stlc in
      let test () =
        Interpreter.once
          ( init,
            main,
            fun _ ->
              try
                let _ = Stlc.mstep_stlcTerm !EvaluationCtx._tmp in
                true
              with _ -> false )
      in
      let _ = Interpreter.eval_until_detect_bug test in
      ()
  | _ -> _die_with [%here] "unknown benchmark"

let test_random s () =
  match s with
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
      let _ = Interpreter.eval_until_detect_bug test in
      ()
  | "twitter" ->
      let open MonkeyBD in
      let open Common in
      let open Twitter in
      let test () =
        Interpreter.random_test
          ( init Causal,
            (fun () -> random_user { numUser = 4; numOp = 2 }),
            TwitterDB.serializable_trace_checker )
      in
      let _ = Interpreter.eval_until_detect_bug test in
      ()
  | "courseware" ->
      let open MonkeyBD in
      let open Common in
      let open Courseware in
      let test () =
        Interpreter.random_test
          ( init Causal,
            (fun () -> random_user { numCourse = 4; numUser = 4; numOp = 2 }),
            CoursewareDB.serializable_trace_checker )
      in
      let _ = Interpreter.eval_until_detect_bug test in
      ()
  | "cart" ->
      let open MonkeyBD in
      let open Common in
      let open Cart in
      let test () =
        Interpreter.random_test
          ( init Causal,
            (fun () -> random_user { numUser = 4; numItem = 4; numOp = 2 }),
            CartDB.serializable_trace_checker )
      in
      let _ = Interpreter.eval_until_detect_bug test in
      ()
  | "todoMVC" -> _die_with [%here] "unimp"
  | "stlc" -> _die_with [%here] "unimp"
  | _ -> _die_with [%here] "unknown benchmark"

let cmds =
  [
    ("test-eval", one_param_string "test eval" test_eval);
    ("test-random", one_param_string "test random" test_random);
    ("read-syn", one_param "read syn" read_syn);
    ("syn-one", two_param_string "syn one" syn_term);
    ("syn-benchmark", one_param_string "run benchmark" syn_benchmark);
    ("eval-benchmark", one_param_string "run benchmark" eval_benchmark);
    ("syn-timeout", timeout_param "syn timeout" syn_term_timeout);
    ("eval", two_param_string "eval" eval);
    (* ("compile-to-p", four_param_string "compile to p language" compile_to_p); *)
    ("compile-to-p", one_param_string "compile to p language" compile_to_p);
    ("show-term", one_param "show term" show_term);
    (* ("read-automata", one_param "read_automata" read_automata); *)
    (* ("read-sfa", one_param "read_sfa" read_sfa); *)
    (* ("read-p", one_param "read_p" read_p); *)
    (* ("read-p-sfa", three_param "read_p" read_p_and_spec); *)
    (* ("random-p-sfa", three_param "read_p" random_read_p_and_spec); *)
    (* ("read-p-wrapper", two_param_string "p-wrapper" p_wrapper); *)
    (* ("read-p-repo", one_param_string "p-wrapper" read_p_repo); *)
  ]
