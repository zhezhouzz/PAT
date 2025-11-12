(** Scheduler for message passing *)

include Common
include Pool
module Eval = Eval
module Sample = Sample
open Language

let random_test (init, main, checker) =
  Pool.init ();
  init ();
  random_scheduler main;
  let his = !Runtime.hisTrace in
  let () =
    if checker his then raise (NoBugDetected "no bug detected") else ()
  in
  his

let seq_random_test (init, main, checker) =
  Pool.init ();
  init ();
  eager_scheduler main;
  let his = !Runtime.hisTrace in
  let () =
    if checker his then raise (NoBugDetected "no bug detected") else ()
  in
  List.filter (fun msg -> not (String.equal msg.ev.op "dummy")) his

let once (init, main, checker) =
  (* let main = List.nth main (Random.int (List.length main)) in *)
  Pool.init ();
  init ();
  run (fun () -> Eval.eval_to_unit main);
  let his = !Runtime.hisTrace in
  let () =
    if checker his then raise (NoBugDetected "no bug detected") else ()
  in
  (* let () =
    if checker his then
      _die_with [%here]
        "should not happen: the trace doesn't realized the expect errounous \
         pattern"
    else ()
  in *)
  his

let over_number_bound number_bound n =
  match number_bound with
  | Some number_bound -> n >= number_bound
  | None -> false

let over_time_bound time_bound exec_time =
  match time_bound with
  | Some time_bound ->
      (* let () = Printf.printf "@{<red>Time bound: %i@}\n" time_bound in
      let () = Printf.printf "@{<red>Exec time: %f@}\n" exec_time in
      let _ = read_line () in *)
      exec_time > float_of_int time_bound
  | None -> false

let eval_sample ~number_bound ~time_bound test =
  let start_time = Sys.time () in
  let rec aux (successed : int) (used : int) =
    let exec_time = Sys.time () -. start_time in
    if over_time_bound time_bound exec_time then (exec_time, successed, used)
    else if over_number_bound number_bound used then (exec_time, successed, used)
    else
      try
        let _ = test () in
        aux (successed + 1) (used + 1)
      with
      | RuntimeInconsistent msg ->
          Pp.printf "@{<red>Error:@} %s\n" msg;
          aux successed (used + 1)
      | IsolationViolation _ ->
          Pp.printf "@{<red>Error:@} %s\n" "isolation violation";
          aux successed (used + 1)
      | NoBugDetected _ ->
          Pp.printf "@{<red>Error:@} %s\n" "no bug detected";
          aux successed (used + 1)
      | e -> raise e
  in
  let exec_time, successed, total = aux 0 0 in
  let rate = float_of_int total /. float_of_int successed in
  let exec_time =
    if successed > 0 then exec_time /. float_of_int successed else -1.0
  in
  let () = Pp.printf "@{<red>Success rate: %f@}\n" rate in
  (successed, rate, exec_time)

let eval_until_detect_bug converge_bound test =
  let rec aux (i : int) =
    let () = Pp.printf "@{<red>Repeat for %i times@}\n" i in
    if i >= converge_bound then
      _die_with [%here] "too many time until consistent(1)"
    else
      let i = i + 1 in
      try
        let his = test () in
        (i, his)
      with
      (* | Sample.SampleTooManyTimes ->
          Pp.printf "@{<red>Error:@} %s\n" "sample too many times";
          aux i *)
      | RuntimeInconsistent msg ->
          Pp.printf "@{<red>Error:@} %s\n" msg;
          aux i
      | IsolationViolation _ ->
          Pp.printf "@{<red>Error:@} %s\n" "isolation violation";
          aux i
      | NoBugDetected _ ->
          Pp.printf "@{<red>Error:@} %s\n" "no bug detected";
          aux i
      | e -> raise e
  in
  let (i, his), _ = Stat.stat_function (fun () -> aux 0) in
  let () = Pp.printf "@{<red>Repeat for %i times@}\n" i in
  let () =
    Pp.printf "@{<red>Trace@}\n%s\n"
      (List.map layout_msg his |> String.concat "; ")
  in
  (i, his)

let layout_time_to_detect time_to_detect =
  match time_to_detect with
  | Some time_to_detect -> string_of_float time_to_detect
  | None -> "Timeout"

let eval_by_time time_bound test =
  let start_time = Sys.time () in
  let rec aux (num_sampled : int) (num_bug_detected : int) =
    let () = Pp.printf "@{<red>Repeat for %i times@}\n" num_sampled in
    let exec_time = Sys.time () -. start_time in
    if exec_time > time_bound then (exec_time, num_sampled, num_bug_detected)
    else
      let num_sampled = num_sampled + 1 in
      try
        let _ = test () in
        let num_bug_detected = num_bug_detected + 1 in
        aux num_sampled num_bug_detected
      with
      (* | Sample.SampleTooManyTimes ->
          Pp.printf "@{<red>Error:@} %s\n" "sample too many times";
          aux i *)
      | RuntimeInconsistent msg ->
          Pp.printf "@{<red>Error:@} %s\n" msg;
          aux num_sampled num_bug_detected
      | IsolationViolation _ ->
          Pp.printf "@{<red>Error:@} %s\n" "isolation violation";
          aux num_sampled num_bug_detected
      | NoBugDetected _ ->
          Pp.printf "@{<red>Error:@} %s\n" "no bug detected";
          aux num_sampled num_bug_detected
      | e -> raise e
  in
  let exec_time, num_sampled, num_bug_detected = aux 0 0 in
  let time_to_detect =
    if num_bug_detected > 0 then
      Some (exec_time /. float_of_int num_bug_detected)
    else None
  in
  let () =
    Pp.printf "@{<red>Repeat %i, detect %i, time to detect: %s @}\n" num_sampled
      num_bug_detected
      (layout_time_to_detect time_to_detect)
  in
  (num_sampled, num_bug_detected, time_to_detect)
