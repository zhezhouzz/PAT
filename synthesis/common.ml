open Language
open AutomataLibrary
open Zdatatype

module LitSet = Set.Make (struct
  type t = Nt.nt lit

  let compare = compare_lit Nt.compare_nt
end)

module ConstSet = Set.Make (struct
  type t = constant

  let compare = compare_constant
end)

module TVSet = Set.Make (struct
  type t = (Nt.nt, string) typed

  let compare x y = String.compare x.x y.x
end)

module LitMap = Map.Make (struct
  type t = Nt.nt lit

  let compare = compare_lit Nt.compare_nt
end)

module BlistSet = Set.Make (struct
  type t = bool list

  let compare = List.compare Bool.compare
end)

open Zdatatype
open Plan

(* let simp_print_gamma_judgement { bvs; bprop } =
  Pp.printf "@{<bold>@{<red>Γ:@} %s |@} %s\n"
    (List.split_by_comma _get_x bvs)
    (layout_prop bprop)

let print_gamma_judgement { bvs; bprop } =
  Pp.printf "@{<bold>@{<red>Γ:@}@} %s | %s\n" (layout_qvs bvs)
    (layout_prop bprop)

let simp_print_mid_judgement (pre, cur, post) =
  let open Plan in
  Pp.printf "@{<bold>[@} %s @{<bold>]@}\n %s\n@{<bold>[@} %s @{<bold>]@}\n\n"
    (omit_layout pre) (layout_elem cur) (omit_layout post)

let print_mid_judgement (pre, cur, post) =
  let open Plan in
  Pp.printf "@{<bold>[@} %s @{<bold>]@}\n %s\n@{<bold>[@} %s @{<bold>]@}\n\n"
    (layout pre) (layout_elem cur) (layout post)

let simp_print_plan_judgement plan =
  let open Plan in
  Pp.printf "@{<bold>[@} %s @{<bold>]@}\n\n" (omit_layout plan)

let print_plan_judgement plan =
  let open Plan in
  Pp.printf "@{<bold>[@} %s @{<bold>]@}\n\n" (layout plan)

let simp_print_back_judgement { gamma; pre; mid; post; pg; solved } =
  Pp.printf "@{<bold>@{<yellow>Backword:@}@}\n";
  simp_print_gamma_judgement gamma;
  PG.print_preserve_goals "preserve" pg;
  PG.print_preserve_goals "solved" solved;
  simp_print_mid_judgement (pre, mid, post)

let simp_print_mid { gamma; pre; mid; post; pg; solved } =
  simp_print_gamma_judgement gamma;
  PG.print_preserve_goals "preserve" pg;
  PG.print_preserve_goals "solved" solved;
  simp_print_mid_judgement (pre, mid, post)

let print_mid { gamma; pre; mid; post; pg; solved } =
  simp_print_gamma_judgement gamma;
  PG.print_preserve_goals "preserve" pg;
  PG.print_preserve_goals "solved" solved;
  print_mid_judgement (pre, mid, post)

let simp_print_forward_judgement { gamma; preSolved; postUnsolved; pg; solved }
    =
  Pp.printf "@{<bold>@{<yellow>Forward:@}@}\n";
  simp_print_gamma_judgement gamma;
  PG.print_preserve_goals "preserve" pg;
  PG.print_preserve_goals "solved" solved;
  Pp.printf "@{<bold>[@} %s @{<bold>]@}\n@{<bold>[@} %s @{<bold>]@}\n\n"
    (omit_layout preSolved) (omit_layout postUnsolved)

let simp_print_syn_judgement { gamma; plan; pg; solved } =
  Pp.printf "@{<bold>@{<yellow>Synthesis:@}@}\n";
  simp_print_gamma_judgement gamma;
  PG.print_preserve_goals "preserve" pg;
  PG.print_preserve_goals "solved" solved;
  simp_print_plan_judgement plan

let print_syn_judgement { gamma; plan; pg; solved } =
  Pp.printf "@{<bold>@{<yellow>Synthesis:@}@}\n";
  simp_print_gamma_judgement gamma;
  PG.print_preserve_goals "preserve" pg;
  PG.print_preserve_goals "solved" solved;
  print_plan_judgement plan

let simp_print_opt_judgement p1 m p2 =
  Pp.printf "@{<bold>@{<yellow>Optimize:@}@}\n";
  p1 ();
  Pp.printf "@{<yellow>Map:@} %s\n"
    (List.split_by "; " (fun (x, y) -> spf "%s --> %s" x y.x) m);
  p2 ()
 *)

(* let simp_print_instantiation gamma (gamma', plan) =
  Pp.printf "@{<bold>@{<yellow>Instantiation:@} With@}\n";
  simp_print_gamma_judgement gamma;
  Pp.printf "@{<yellow>Instantiation:@}\n";
  simp_print_gamma_judgement gamma';
  Pp.printf "%s\n" @@ Plan.omit_layout plan *)

let devide_linear_regex_list_by_op op future =
  let rec aux prefix = function
    | [] -> None
    | LinearChar se :: rest when String.equal se.op op -> (
        let other = aux (prefix @ [ LinearChar se ]) rest in
        match other with
        | None ->
            Some (linear_regex_to_regex prefix, se, linear_regex_to_regex rest)
        | Some _ ->
            (* HACK: assume each op only has one sevent. *)
            _die [%here])
    | elem :: rest -> aux (prefix @ [ elem ]) rest
  in
  aux [] future

let rec filter_rule_by_future op = function
  | RtyHAF { history; adding; future } ->
      let futures = regex_to_linear_regex future in
      let futures =
        List.filter_map (devide_linear_regex_list_by_op op) futures
      in
      List.map (fun res -> (res, RtyHAF { history; adding; future })) futures
  | RtyHAParallel _ -> _die [%here]
  | RtyArr { arg; argcty; retrty } ->
      let l = filter_rule_by_future op retrty in
      List.map (fun (res, retrty) -> (res, RtyArr { arg; argcty; retrty })) l
  | RtyGArr { arg; argnty; retrty } ->
      let l = filter_rule_by_future op retrty in
      List.map (fun (res, retrty) -> (res, RtyGArr { arg; argnty; retrty })) l
  | _ -> _die [%here]

let rec deinter_pat pat =
  match pat with
  | RtyHAF _ -> [ pat ]
  | RtyHAParallel _ -> _die [%here]
  | RtyArr { arg; argcty; retrty } ->
      let l = deinter_pat retrty in
      List.map (fun retrty -> RtyArr { arg; argcty; retrty }) l
  | RtyGArr { arg; argnty; retrty } ->
      let l = deinter_pat retrty in
      List.map (fun retrty -> RtyGArr { arg; argnty; retrty }) l
  | RtyInter (pat1, pat2) ->
      let res1, res2 = map2 deinter_pat (pat1, pat2) in
      res1 @ res2
  | RtyBase _ -> _die [%here]

let select_rule_by_future env op =
  List.concat_map
    (fun x ->
      let l = pat_to_triple x.ty in
      let l = List.concat_map (filter_rule_by_future op) l in
      l)
    (List.map (fun x -> x.x#:(fresh_srl_pat x.ty))
    @@ ctx_to_list env.event_rtyctx)

let select_rule_by_op env op =
  match Typectx.get_opt env.event_rtyctx op with
  | None -> _die [%here]
  | Some pat ->
      let pat = fresh_srl_pat pat in
      deinter_pat pat

let clearn_trace trace =
  List.filter_map
    (function
      | MultiChar c -> Some (charset_to_se [%here] c)
      | Star _ -> None
      | _ -> _die [%here])
    trace

let timebound = ref None
let start_time = ref 0.0

exception Timeout of float * line list

let result_buffer = ref []

let record_result (result : line) =
  match !timebound with
  | None -> Some result
  | Some _ ->
      let () = result_buffer := result :: !result_buffer in
      Some result

let setup_clock bound =
  timebound := bound;
  start_time := Sys.time ()

let get_exec_time () = Sys.time () -. !start_time

let try_timeout () =
  match !timebound with
  | None -> ()
  | Some bound ->
      let exec_time = get_exec_time () in
      if exec_time > bound then raise (Timeout (exec_time, !result_buffer))
      else ()

let shuffle d =
  Random.self_init ();
  let nd = List.map (fun c -> (Random.bits (), c)) d in
  let sond = List.sort compare nd in
  List.map snd sond

let if_explore = ref false
let set_explore () = if_explore := true
(* if Random.bool () then if_explore := true else () *)

let explore_backtrack f l =
  let f x =
    let () = try_timeout () in
    f x
  in
  let l = if !if_explore then shuffle l else l in
  List.fold_left
    (fun res x ->
      match res with
      | Some _ -> res
      | None ->
          (* let () = _die_with [%here] "backtrack fail" in *)
          let res = f x in
          let () =
            match res with None -> Stat.incr_backtrack () | Some _ -> ()
          in
          res)
    None l

let backtrack f l =
  match !timebound with
  | Some _ -> explore_backtrack f l
  | _ ->
      List.fold_left
        (fun res x ->
          match res with
          | Some _ -> res
          | None ->
              (* let () = _die_with [%here] "backtrack fail" in *)
              let res = f x in
              let () =
                match res with None -> Stat.incr_backtrack () | Some _ -> ()
              in
              res)
        None l

let get_available_rty env se =
  let l = ctx_to_list env.event_rtyctx in
  let l = List.filter (fun x -> String.equal x.x se.op) l in
  let l = List.map (fun x -> (se, x.ty)) l in
  let l =
    List.concat_map
      (fun (se, x) -> List.map (fun x -> (se, x)) @@ pat_to_triple x)
      l
  in
  l

let get_available_rty_with_fresh_name env =
  List.map (fun x -> x.x#:(fresh_srl_pat x.ty)) @@ ctx_to_list env.event_rtyctx
