(** Scheduler for message passing *)

include Common
include Pool
module Eval = Eval
open Language

let once init main =
  Pool.init ();
  init ();
  run (fun () -> Eval.eval_to_unit main)

let eval_until_consistent init main =
  let rec aux (i : int) : int =
    let () = Pp.printf "@{<red>Repeat for %i times@}\n" i in
    if i > 1000 then _die_with [%here] "too many time until consistent"
    else
      try
        once init main;
        i
      with
      | RuntimeInconsistent _ | IsolationViolation _ -> aux (i + 1)
      | e -> raise e
  in
  let i = aux 0 in
  let () = Pp.printf "@{<red>Repeat for %i times@}\n" i in
  i

let eval_sample init main total =
  let rec aux (successed : int) (used : int) =
    if used >= total then successed
    else
      try
        once init main;
        aux (successed + 1) (used + 1)
      with
      | RuntimeInconsistent _ -> aux successed (used + 1)
      | e -> raise e
  in
  aux 0 0
