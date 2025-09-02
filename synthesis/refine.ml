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

type search_strategy = DFS | BFS | BoundBFS of int

let search_strategy_to_string = function
  | DFS -> "DFS"
  | BFS -> "BFS"
  | BoundBFS i -> spf "BoundBFS %i" i

let _search_strategy = ref (BoundBFS 1)
let layout_bound = 5

let first_n_list n list =
  if n >= List.length list then (list, [])
  else
    ( List.sublist ~start_included:0 ~end_excluded:n list,
      List.sublist ~start_included:n ~end_excluded:(List.length list) list )

let rec search_strategy (f : 'a -> 'a list) plans =
  (* For efficienct, we should unify the plans first *)
  (* let plans = unify_lines plans in *)
  match plans with
  | [] -> _die_with [%here] "no more plans"
  | plan :: plans' -> (
      match !_search_strategy with
      | DFS -> (
          match f plan with
          | [] -> search_strategy f plans
          | plans'' -> plans'' @ plans')
      | BFS -> List.concat_map f plans
      | BoundBFS i ->
          let plan1, plan2 = first_n_list i plans in
          let plan1 = List.concat_map f plan1 in
          plan1 @ plan2)

let rec merge_new_goals old_goals new_goals =
  match (old_goals, new_goals) with
  | [], _ -> new_goals
  | _, [] -> old_goals
  | g1 :: old_goals, g2 :: new_goals ->
      g2 :: g1 :: merge_new_goals old_goals new_goals

let result_expection = 3

let simp_print_syn_judgement plan =
  let () = Pp.printf "@{<bold>@{<red>Synthesis plan:@}@}\n" in
  print_plan plan

let layout_candidate_plans plans =
  let len = List.length plans in
  let plans, rest = first_n_list layout_bound plans in
  List.iteri
    (fun i plan ->
      Pp.printf "@{<bold>@{<red>%i:@}@}\n%s\n" i (omit_layout_line plan))
    plans;
  if List.length rest > 0 then
    Pp.printf "@{<bold>@{<red>total (%i); rest is omitted@}@}\n" len;
  Pp.printf "\n"

let rec deductive_synthesis env r : line list =
  let plans = regex_to_lines r in
  let plans =
    List.map
      (fun plan ->
        let _, plan, _ = register_line_under_plan [] plan in
        plan)
      plans
  in
  let rec refinement_loop (res, plans) =
    if List.length res >= result_expection then res
    else if List.length plans == 0 then _die_with [%here] "no more plans"
    else
      let plans = search_strategy (refine_one_step env) plans in
      let plans = unify_lines plans in
      let () = layout_candidate_plans plans in
      Pp.printf "\n@{<bold>@{<red>res(%i) plans pool(%i):@}@}\n"
        (List.length res) (List.length plans);
      let _ = input_line stdin in
      let wf_plans, plans = List.partition finished_plan plans in
      let new_goals = List.concat_map (gen_new_act env) wf_plans in
      refinement_loop (res @ wf_plans, merge_new_goals plans new_goals)
  in
  refinement_loop ([], plans)

and refine_one_step env (goal : line) : line list =
  let () = simp_print_syn_judgement goal in
  let ids = underived_act_ids goal in
  match ids with
  | id :: _ ->
      let () = Pp.printf "@{<bold>@{<red>backward@} on %i@}\n" id in
      backward env goal id
  | [] -> (
      match unchecked_act_ids goal with
      | id :: _ ->
          let () = Pp.printf "@{<bold>@{<red>forward@} on %i@}\n" id in
          forward env goal id
      | [] -> [ goal ])

and gen_new_act env (goal : line) : line list =
  let () =
    Pp.printf "@{<bold>@{<red>gen new act@} on line@}\n%s\n"
      (omit_layout_line goal)
  in
  let rules = select_gen_rules env in
  match rules with
  | [] -> []
  | (op, _) :: _ ->
      let goals = gen_merge goal op in
      goals

and backward env (goal : line) mid : line list =
  let _, (_, midAct, _) = line_divide_by_task_id goal mid in
  let op = midAct.aop in
  if is_gen env op then
    let goal = line_label_as_gen_act goal mid in
    [ goal ]
  else
    let rules = select_rule_by_future env op in
    let () =
      List.iteri
        (fun i ((_, se, _), pat) ->
          let () =
            Pp.printf "@{<bold>rty[%i]:@}\n@{<red>se@}: %s\n@{<red>pat@}: %s\n"
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
      let _, (args, retrty) = destruct_pat [%here] pat in
      let goal = plan_add_cargs goal args in
      let history, dep_se, _ = destruct_hap [%here] retrty in
      let dep_se =
        match dep_se with
        | MultiChar s ->
            if CharSet.cardinal s == 1 then CharSet.choose s else _die [%here]
        | _ -> _die [%here]
      in
      backward_merge goal mid (history, dep_se, future1, se, future2)
    in
    let goals = List.concat_map handle rules in
    layout_candidate_plans goals;
    goals

and forward env (goal : line) mid : line list =
  let _, (_, midAct, _) = line_divide_by_task_id goal mid in
  let op = midAct.aop in
  let rules = select_rule_by_op env op in
  let handle pat =
    let _, (args, retrty) = destruct_pat [%here] pat in
    let goal = plan_add_cargs goal args in
    let history, se, p = destruct_hap [%here] retrty in
    forward_merge goal mid (history, se, p)
  in
  let goals = List.concat_map handle rules in
  goals
