open Language

type sregex = Nt.t sevent raw_regex
(* type 'a sgoal = Gamma.gamma * 'a *)

open Gamma
open Zdatatype
open Plan

module PG = struct
  type t = plan_elem list

  let print_preserve_goals txt pg =
    _log "syn" @@ fun _ ->
    Pp.printf "@{<bold>%s:@}\n" txt;
    List.iter (fun c -> Pp.printf "%s: %s\n" txt (layout_elem c)) pg

  (* let mk_preserve_subgoal plan = gather_actions plan *)

  let remove_preserve_subgoal elem pg =
    let pg' = List.filter (fun elem' -> not (equal_plan_elem elem elem')) pg in
    if List.length pg' + 1 < List.length pg then _die [%here] else pg'

  let in_preserve_subgoal elem = List.exists (equal_plan_elem elem)

  let concat pg1 pg2 =
    let pg1 =
      List.filter (fun elem -> not (in_preserve_subgoal elem pg2)) pg1
    in
    pg1 @ pg2

  (* let sanity_check plan pg = *)
  (*   let pg' = mk_preserve_subgoal plan in *)
  (*   List.for_all (fun elem -> in_preserve_subgoal elem pg) pg' *)
end

type mid_plan_goal = {
  gamma : gamma;
  pre : plan;
  mid : plan_elem;
  post : plan;
  pg : plan;
  solved : plan;
}

type pair_plan_goal = {
  gamma : gamma;
  preSolved : plan;
  postUnsolved : plan;
  pg : plan;
  solved : plan;
}

type plan_goal = { gamma : gamma; plan : plan; pg : plan; solved : plan }

(* let sanity_check_mid_plan_goal { pg; pre; mid; post; _ } = *)
(*   PG.sanity_check (pre @ [ mid ] @ post) pg *)

let simp_print_gamma_judgement { bvs; bprop } =
  _log "syn" @@ fun _ ->
  Pp.printf "@{<bold>@{<red>Γ:@} %s |@} %s\n"
    (List.split_by_comma _get_x bvs)
    (layout_prop bprop)

let print_gamma_judgement { bvs; bprop } =
  _log "syn" @@ fun _ ->
  Pp.printf "@{<bold>@{<red>Γ:@}@} %s | %s\n" (layout_qvs bvs)
    (layout_prop bprop)

let simp_print_mid_judgement (pre, cur, post) =
  _log "syn" @@ fun _ ->
  let open Plan in
  Pp.printf "@{<bold>[@} %s @{<bold>]@}\n %s\n@{<bold>[@} %s @{<bold>]@}\n\n"
    (omit_layout pre) (layout_elem cur) (omit_layout post)

let print_mid_judgement (pre, cur, post) =
  _log "syn" @@ fun _ ->
  let open Plan in
  Pp.printf "@{<bold>[@} %s @{<bold>]@}\n %s\n@{<bold>[@} %s @{<bold>]@}\n\n"
    (layout pre) (layout_elem cur) (layout post)

let simp_print_plan_judgement plan =
  _log "syn" @@ fun _ ->
  _log "syn" @@ fun _ ->
  let open Plan in
  Pp.printf "@{<bold>[@} %s @{<bold>]@}\n\n" (omit_layout plan)

let print_plan_judgement plan =
  _log "syn" @@ fun _ ->
  let open Plan in
  Pp.printf "@{<bold>[@} %s @{<bold>]@}\n\n" (layout plan)

let simp_print_back_judgement { gamma; pre; mid; post; pg; solved } =
  _log "syn" @@ fun _ ->
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
  _log "syn" @@ fun _ ->
  Pp.printf "@{<bold>@{<yellow>Forward:@}@}\n";
  simp_print_gamma_judgement gamma;
  PG.print_preserve_goals "preserve" pg;
  PG.print_preserve_goals "solved" solved;
  Pp.printf "@{<bold>[@} %s @{<bold>]@}\n@{<bold>[@} %s @{<bold>]@}\n\n"
    (omit_layout preSolved) (omit_layout postUnsolved)

let simp_print_syn_judgement { gamma; plan; pg; solved } =
  _log "syn" @@ fun _ ->
  Pp.printf "@{<bold>@{<yellow>Synthesis:@}@}\n";
  simp_print_gamma_judgement gamma;
  PG.print_preserve_goals "preserve" pg;
  PG.print_preserve_goals "solved" solved;
  simp_print_plan_judgement plan

let print_syn_judgement { gamma; plan; pg; solved } =
  _log "syn" @@ fun _ ->
  Pp.printf "@{<bold>@{<yellow>Synthesis:@}@}\n";
  simp_print_gamma_judgement gamma;
  PG.print_preserve_goals "preserve" pg;
  PG.print_preserve_goals "solved" solved;
  print_plan_judgement plan

let simp_print_opt_judgement p1 m p2 =
  _log "syn" @@ fun _ ->
  Pp.printf "@{<bold>@{<yellow>Optimize:@}@}\n";
  p1 ();
  Pp.printf "@{<yellow>Map:@} %s\n"
    (List.split_by "; " (fun (x, y) -> spf "%s --> %s" x y.x) m);
  p2 ()

let plan_goal_size x =
  List.length
    (List.filter
       (function PlanAct _ | PlanActBuffer _ | PlanSe _ -> true | _ -> false)
       x)

let back_goal_size { pre; post; _ } =
  1 + plan_goal_size pre + plan_goal_size post

let simp_print_instantiation gamma (gamma', plan) =
  _log "syn" @@ fun _ ->
  Pp.printf "@{<bold>@{<yellow>Instantiation:@} With@}\n";
  simp_print_gamma_judgement gamma;
  Pp.printf "@{<yellow>Instantiation:@}\n";
  simp_print_gamma_judgement gamma';
  Pp.printf "%s\n" @@ Plan.omit_layout plan

let choose_one l =
  List.init (List.length l) (fun i ->
      let x = List.nth l i in
      let rest = List.filteri (fun j _ -> i != j) l in
      (x, rest))

let rec filter_rule_by_future op = function
  | RtyHAParallel { parallel; adding_se; history } ->
      (* HACK: assume each op only has one sevent. *)
      let ses, parallel' =
        List.partition
          (fun se -> String.equal op (_get_sevent_name se))
          parallel
      in
      List.map (fun (se, rest) ->
          (se, RtyHAParallel { parallel = rest @ parallel'; adding_se; history }))
      @@ choose_one ses
  | RtyArr { arg; argcty; retrty } ->
      let l = filter_rule_by_future op retrty in
      List.map (fun (se, retrty) -> (se, RtyArr { arg; argcty; retrty })) l
  | RtyGArr { arg; argnt; retrty } ->
      let l = filter_rule_by_future op retrty in
      List.map (fun (se, retrty) -> (se, RtyGArr { arg; argnt; retrty })) l
  | _ -> _die [%here]

let select_rule_by_future env op =
  List.concat_map
    (fun x ->
      let l = haft_to_triple x.ty in
      let l = List.concat_map (filter_rule_by_future op) l in
      l)
    (List.map (fun x -> x.x #: (fresh_haft x.ty))
    @@ ctx_to_list env.event_rtyctx)

let charset_to_se loc s =
  let open SFA in
  match List.of_seq @@ CharSet.to_seq s with [ x ] -> x | _ -> _die loc

let clearn_trace trace =
  List.filter_map
    (function
      | MultiChar c -> Some (charset_to_se [%here] c)
      | Star _ -> None
      | _ -> _die [%here])
    trace

let timebound = ref None
let start_time = ref 0.0

exception Timeout of float * plan_goal list

let result_buffer = ref []

let is_dup g1 g2 =
  let get_trace p = p.plan in
  let tr1, tr2 = map2 get_trace (g1, g2) in
  List.equal equal_plan_elem tr1 tr2

let record_result (result : plan_goal) =
  match !timebound with
  | None -> Some result
  | Some _ ->
      (* let () = _die [%here] in *)
      let () =
        if not (List.exists (is_dup result) !result_buffer) then
          result_buffer := result :: !result_buffer
      in
      (* result_buffer := result :: !result_buffer; *)
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
