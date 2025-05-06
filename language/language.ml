include Plang_parser
include Front
include AutomataLibrary
include Syntax

module Stat = struct
  type task_complexity = { n_op : int; n_qualifier : int } [@@deriving yojson]

  type result_complexity = {
    n_var : int;
    n_obs : int;
    n_gen : int;
    n_assert : int;
  }
  [@@deriving yojson]

  type algo_complexity = {
    n_bt : int;
    n_sat : int;
    n_nonempty : int;
    t_sat : float;
    t_nonempty : float;
    t_refine : float;
    t_total : float;
    n_forward : int;
    n_backward : int;
  }
  [@@deriving yojson]

  let record : algo_complexity option ref = ref None

  type stat = {
    task_complexity : task_complexity;
    result_complexity : result_complexity;
    algo_complexity : algo_complexity;
    rate : float;
    n_retry : float;
  }
  [@@deriving yojson]

  open Zdatatype

  let count_quantifier_prop phi = if is_true phi || is_false phi then 0 else 1
  let count_quantifier_se ({ phi; _ } : 't sevent) = count_quantifier_prop phi

  let count_quantifier_charset cs =
    SFA.CharSet.fold (fun se res -> res + count_quantifier_se se) cs 0

  let count_quantifier_rawregex =
    let rec aux = function
      | Empty -> 0
      | Eps -> 0
      | MultiChar cs -> count_quantifier_charset cs
      | Seq l -> List.left_reduce [%here] ( + ) @@ List.map aux l
      | Inters (r1, r2) -> aux r1 + aux r2
      | Alt (r1, r2) -> aux r1 + aux r2
      | Comple (cs, r) -> count_quantifier_charset cs + aux r
      | Star r -> aux r
    in
    aux

  let goal_size = ref 0

  let count_vars term =
    let rec aux = function
      | CLetE { lhs; body; _ } -> lhs @ aux body.x
      | CUnion ts -> List.concat_map aux ts
      | _ -> []
    in
    let l = List.map _get_x @@ aux term in
    let l = List.slow_rm_dup String.equal l in
    List.length l

  let count_obs =
    let rec aux = function
      | CLetE { body; rhs; _ } -> aux rhs.x + aux body.x
      | CUnion ts -> List.left_reduce [%here] ( + ) @@ List.map aux ts
      | CObs _ -> 1
      | _ -> 0
    in
    aux

  let count_gen =
    let rec aux = function
      | CLetE { body; rhs; _ } -> aux rhs.x + aux body.x
      | CUnion ts -> List.left_reduce [%here] ( + ) @@ List.map aux ts
      | CGen _ -> 1
      | _ -> 0
    in
    aux

  let count_assert =
    let rec aux = function
      | CLetE { body; rhs; _ } -> aux rhs.x + aux body.x
      | CUnion ts -> List.left_reduce [%here] ( + ) @@ List.map aux ts
      | CAssertP phi -> if not (is_true phi) then 1 else 0
      | CAssume (_, phi) -> if not (is_true phi) then 1 else 0
      | CObs { prop; _ } -> if not (is_true prop) then 1 else 0
      | _ -> 0
    in
    aux

  let mk_result_complexity (term : term) : result_complexity =
    {
      n_var = count_vars term;
      n_obs = count_obs term;
      n_gen = count_gen term;
      n_assert = count_assert term;
    }

  let init_algo_complexity () =
    record :=
      Some
        {
          n_bt = 0;
          n_sat = 0;
          n_nonempty = 0;
          t_sat = 0.0;
          t_nonempty = 0.0;
          t_total = 0.0;
          t_refine = 0.0;
          n_forward = 0;
          n_backward = 0;
        }

  let incr_backtrack () =
    match !record with
    | None -> _die_with [%here] "stat not init"
    | Some r -> record := Some { r with n_bt = r.n_bt + 1 }

  let incr_forward () =
    match !record with
    | None -> _die_with [%here] "stat not init"
    | Some r -> record := Some { r with n_forward = r.n_forward + 1 }

  let incr_backward () =
    match !record with
    | None -> _die_with [%here] "stat not init"
    | Some r -> record := Some { r with n_backward = r.n_backward + 1 }

  let stat_function f =
    let start_time = Sys.time () in
    let res = f () in
    let exec_time = Sys.time () -. start_time in
    (res, exec_time)

  let stat_sat_query f =
    let res, exec_time = stat_function f in
    let () =
      match !record with
      | None -> _die_with [%here] "stat not init"
      | Some r ->
          record :=
            Some { r with t_sat = r.t_sat +. exec_time; n_sat = r.n_sat + 1 }
    in
    res

  let stat_nonempty_check f =
    let res, exec_time = stat_function f in
    let () =
      match !record with
      | None -> _die_with [%here] "stat not init"
      | Some r ->
          record :=
            Some
              {
                r with
                t_nonempty = r.t_nonempty +. exec_time;
                n_nonempty = r.n_nonempty + 1;
              }
    in
    res

  let stat_refine f =
    let res, exec_time = stat_function f in
    let () =
      match !record with
      | None -> _die_with [%here] "stat not init"
      | Some r -> record := Some { r with t_refine = exec_time }
    in
    res

  let stat_total f =
    let res, exec_time = stat_function f in
    let () =
      match !record with
      | None -> _die_with [%here] "stat not init"
      | Some r -> record := Some { r with t_total = exec_time }
    in
    res

  let update rate filename =
    let stat =
      match stat_of_yojson @@ Yojson.Safe.from_file filename with
      | Result.Ok x -> x
      | Error _ -> _die [%here]
    in
    let stat = { stat with rate } in
    let () = Yojson.Safe.to_file filename @@ stat_to_yojson stat in
    ()

  let update_retry n_retry filename =
    let stat =
      match stat_of_yojson @@ Yojson.Safe.from_file filename with
      | Result.Ok x -> x
      | Error _ -> _die [%here]
    in
    let stat = { stat with n_retry } in
    let () = Yojson.Safe.to_file filename @@ stat_to_yojson stat in
    ()
end

module Prover = struct
  include Prover

  let check_sat_bool p = Stat.stat_sat_query (fun () -> check_sat_bool p)
  let check_valid p = Stat.stat_sat_query (fun () -> check_valid p)
end
