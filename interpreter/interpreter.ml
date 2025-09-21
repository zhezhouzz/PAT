(** Scheduler for message passing *)

include Common
include Pool
module Eval = Eval
module Sample = Sample
open Language

let converge_bound = 100

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
  let main = List.nth main (Random.int (List.length main)) in
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

let eval_until_detect_bug test =
  let rec aux (i : int) =
    let () = Pp.printf "@{<red>Repeat for %i times@}\n" i in
    if i > converge_bound then
      _die_with [%here] "too many time until consistent"
    else
      try
        let his = test () in
        (i, his)
      with
      | RuntimeInconsistent msg ->
          Pp.printf "@{<red>Error:@} %s\n" msg;
          aux (i + 1)
      | IsolationViolation _ ->
          Pp.printf "@{<red>Error:@} %s\n" "isolation violation";
          aux (i + 1)
      | NoBugDetected _ ->
          Pp.printf "@{<red>Error:@} %s\n" "no bug detected";
          aux (i + 1)
      | e -> raise e
  in
  let i, his = aux 0 in
  let () = Pp.printf "@{<red>Repeat for %i times@}\n" i in
  let () =
    Pp.printf "@{<red>Trace@}\n%s\n"
      (List.map layout_msg his |> String.concat "; ")
  in
  (i, his)

let eval_sample test total =
  let rec aux (successed : int) (used : int) =
    if used >= total then successed
    else
      try
        test ();
        aux (successed + 1) (used + 1)
      with
      | RuntimeInconsistent _ -> aux successed (used + 1)
      | e -> raise e
  in
  aux 0 0
