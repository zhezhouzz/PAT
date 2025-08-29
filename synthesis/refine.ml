open Language
open Common
open Zdatatype
open SFA

(* open Optimize *)
(* open Norm *)
open Plan
(* open Gamma *)

let synthesis_counter = ref 0

let incrAndStop n =
  if !synthesis_counter >= n then _die [%here]
  else synthesis_counter := !synthesis_counter + 1

let forward_synthesis_counter = ref 0

let forward_incrAndStop n =
  if !forward_synthesis_counter >= n then _die [%here]
  else forward_synthesis_counter := !forward_synthesis_counter + 1

type search_strategy = DFS | BFS

let search_strategy_to_string = function DFS -> "DFS" | BFS -> "BFS"
let _search_strategy = ref DFS

let rec search_strategy (f : 'a -> 'a list) plans =
  match !_search_strategy with
  | DFS -> (
      match plans with
      | [] -> _die_with [%here] "no more plans"
      | plan :: plans -> (
          match f plan with
          | [] -> search_strategy f plans
          | plans' -> plans' @ plans))
  | BFS -> _die_with [%here] "unimp"

let result_expection = 1
let simp_print_syn_judgement plan = print_plan plan

let rec deductive_synthesis env (_, r) : plan list =
  let lines = global_prop_to_lines r in
  let plans = List.map (fun line -> new_plan [] line) lines in
  let rec refinement_loop (res, plans) =
    if List.length res >= result_expection then res
    else if List.length plans == 0 then _die_with [%here] "no more plans"
    else
      let plans = search_strategy (refine_one_step env) plans in
      let wf_plans, plans = List.partition well_formed_plan plans in
      refinement_loop (res @ wf_plans, plans)
  in
  refinement_loop ([], plans)

and refine_one_step env (goal : plan) : plan list =
  let () = simp_print_syn_judgement goal in
  let ids = underived_act_ids goal in
  match ids with
  | id :: _ -> backward env goal id
  | [] -> (
      match unchecked_act_ids goal with
      | id :: _ -> forward env goal id
      | [] -> [ goal ])

and backward env (goal : plan) mid : plan list =
  let _, (_, midAct, _) = plan_divide_by_task_id goal mid in
  let op = midAct.aop in
  let () = Printf.printf "%i\n" !forward_synthesis_counter in
  if is_gen env op then
    let goal = label_as_gen_act goal mid in
    [ goal ]
  else
    let rules = select_rule_by_future env op in
    let () =
      List.iteri
        (fun i ((_, se, _), pat) ->
          let () =
            Pp.printf
              "@{<bold>available rty %i@}\n@{<red>se@}: %s\n@{<red>pat@}: %s\n"
              i (layout_sevent se)
              (layout_pat layout_regex pat)
          in
          ())
        rules
    in
    let handle ((future1, se, future2), pat) =
      let () =
        Pp.printf "@{<bold>use rty@}\n@{<red>se@}: %s\n@{<red>pat@}: %s\n"
          (layout_sevent se)
          (layout_pat layout_regex pat)
      in
      let gargs, (args, retrty) = destruct_pat [%here] pat in
      let goal = plan_add_cargs goal args in
      let goal = { goal with freeVars = gargs @ goal.freeVars } in
      let history, dep_se, _ = destruct_hap [%here] retrty in
      let dep_se =
        match dep_se with
        | MultiChar s ->
            if CharSet.cardinal s == 1 then CharSet.choose s else _die [%here]
        | _ -> _die [%here]
      in
      let plans =
        backward_merge env.event_tyctx goal mid
          ( history,
            dep_se,
            linear_regex_list_to_regex future1,
            se,
            linear_regex_list_to_regex future2 )
      in
      plans
    in
    let goals = List.concat_map handle rules in
    goals

and forward env (goal : plan) mid : plan list =
  let _, (_, midAct, _) = plan_divide_by_task_id goal mid in
  let op = midAct.aop in
  let rules = select_rule_by_op env op in
  let handle pat =
    let gargs, (args, retrty) = destruct_pat [%here] pat in
    let goal = plan_add_cargs goal args in
    let goal = { goal with freeVars = gargs @ goal.freeVars } in
    let history, se, p = destruct_hap [%here] retrty in
    let plans = forward_merge env.event_tyctx goal mid (history, se, p) in
    plans
  in
  let goals = List.concat_map handle rules in
  goals
