include Eval
open Runtime
open Language

let interpret env term =
  let runtime = init_runtime env default_sample_domain in
  let runtime, _ = eval_until_consistent (runtime, term) in
  let () =
    Pp.printf "@{<bold>trace:@}\n@{<orange>%s@}\n" (layout_trace runtime.trace)
  in
  ()

let interpret_sample env term total =
  let runtime = init_runtime env default_sample_domain in
  let num = eval_sample (runtime, term) total in
  let rate = float_of_int (100 * num) /. float_of_int total in
  let n_retry = float_of_int total /. float_of_int num in
  let () =
    Pp.printf "@{<bold>ratio:@}\n@{<orange>%i/%i = %f | avg.retry: %f@}\n" num
      total rate n_retry
  in
  (rate, n_retry)
