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

type search_strategy = DFS | BFS | BoundBFS of int | UnSortedDFS of int

type strategy = {
  search : search_strategy;
  layout_bound : int;
  result_expection : int;
  search_new_goals : bool;
  pause : bool;
  addKstar : bool;
}

let _strategy =
  ref
    {
      search = DFS;
      layout_bound = 5;
      result_expection = 5;
      pause = false;
      search_new_goals = false;
      addKstar = true;
    }

let init_strategy env =
  let num_gen =
    List.length
    @@ List.filter (fun x -> is_generative x.ty) (ctx_to_list env.msgkind_ctx)
  in
  let num_obs =
    List.length
    @@ List.filter (fun x -> is_observable x.ty) (ctx_to_list env.msgkind_ctx)
  in
  let () = Pp.printf "num_gen: %i, num_obs: %i\n" num_gen num_obs in
  if StrMap.cardinal env.axioms > 3 || num_obs > num_gen + 3 then
    _strategy :=
      {
        !_strategy with
        search = UnSortedDFS 1;
        search_new_goals = false;
        result_expection = 1;
      }
  else if num_gen <= 4 && StrMap.cardinal env.axioms == 0 then
    _strategy :=
      { !_strategy with search = UnSortedDFS 1; search_new_goals = true }
  else
    _strategy :=
      { !_strategy with search = BoundBFS 10; search_new_goals = false }

let search_strategy_to_string = function
  | DFS -> "DFS"
  | BFS -> "BFS"
  | UnSortedDFS i -> spf "UnSortedDFS %i" i
  | BoundBFS i -> spf "BoundBFS %i" i

let layout_strategy
    {
      search;
      layout_bound;
      result_expection;
      search_new_goals;
      pause;
      addKstar;
    } =
  spf
    "search: %s\n\
     layout_bound: %i\n\
     result_expection: %i\n\
     search_new_goals: %b\n\
     pause: %baddKstar: %b"
    (search_strategy_to_string search)
    layout_bound result_expection search_new_goals pause addKstar

let merge_new_goals old_goals new_goals =
  if !_strategy.search_new_goals then
    let rec aux old_goals new_goals =
      match (old_goals, new_goals) with
      | [], _ -> new_goals
      | _, [] -> old_goals
      | g1 :: old_goals, g2 :: new_goals -> g2 :: g1 :: aux old_goals new_goals
    in
    aux old_goals new_goals
  else old_goals @ new_goals

let try_pause () =
  if !_strategy.pause then
    let _ = input_line stdin in
    ()
  else ()

let first_n_list n list =
  if n >= List.length list then (list, [])
  else
    ( List.sublist ~start_included:0 ~end_excluded:n list,
      List.sublist ~start_included:n ~end_excluded:(List.length list) list )

let rec search_on_strategy (f : 'a -> 'a list) plans =
  (* For efficienct, we should unify the plans first *)
  (* let plans = unify_lines plans in *)
  match plans with
  | [] -> _die_with [%here] "no more plans"
  | plan :: plans' -> (
      match !_strategy.search with
      | DFS -> (
          match f plan with
          | [] -> search_on_strategy f plans
          | plans'' -> plans'' @ plans')
      | BFS -> List.concat_map f plans
      | UnSortedDFS i ->
          let plan1, plan2 = first_n_list i plans in
          let plan1 = List.concat_map f plan1 in
          plan1 @ plan2
      | BoundBFS i ->
          let plans =
            List.sort (fun x y -> Int.compare (line_size x) (line_size y)) plans
          in
          let plan1, _ = first_n_list i plans in
          let plan1 = List.concat_map f plan1 in
          plan1)

let simp_print_syn_judgement plan =
  let () = Pp.printf "@{<bold>@{<red>Synthesis plan:@}@}\n" in
  print_plan plan

let layout_candidate_plans plans =
  let len = List.length plans in
  let plans, rest = first_n_list !_strategy.layout_bound plans in
  List.iteri
    (fun i plan ->
      Pp.printf "@{<bold>@{<red>%i:@}@}\n%s\n%s\n" i (omit_layout_line plan)
        (layout_plan_checkedActs plan))
    plans;
  if List.length rest > 0 then
    Pp.printf "@{<bold>@{<red>total (%i); rest is omitted@}@}\n" len;
  Pp.printf "\n"

let rec deductive_synthesis env r : synMidResult list =
  let plans = regex_to_lines r in
  let plans =
    List.map
      (fun plan ->
        let _, plan, _ = register_line_under_plan [] plan in
        plan)
      plans
  in
  let () = init_strategy env in
  let () =
    Pp.printf "@{<bold>@{<red>strategy:@}@}\n%s\n" (layout_strategy !_strategy)
  in
  let () = try_pause () in
  (* let () = _die [%here] in *)
  let rec refinement_loop (res, plans) =
    if List.length res >= !_strategy.result_expection then res
    else if List.length plans == 0 then []
    else
      let plans = search_on_strategy (refine_one_step env) plans in
      (* let plans = List.map LineOpt.optimize_line plans in *)
      (* let plans = unify_lines plans in *)
      let () = layout_candidate_plans plans in
      Pp.printf "\n@{<bold>@{<red>res(%i) plans pool(%i):@}@}\n"
        (List.length res) (List.length plans);
      let _ = try_pause () in
      let wf_plans, plans = List.partition finished_plan plans in
      (* let wf_plans = unify_lines wf_plans in *)
      let new_goals = List.concat_map (gen_new_act env) wf_plans in
      refinement_loop (res @ wf_plans, merge_new_goals plans new_goals)
  in
  let res = refinement_loop ([], plans) in
  if List.length res == 0 then _die_with [%here] "no more plans"
  else if !_strategy.addKstar then
    let res' = List.concat_map (gen_new_kstar env) res in
    (* let () =
      Pp.printf "@{<bold>@{<red>gen new kstar result@}@}\n";
      List.iter (fun (x, y, z) -> print_mid_result (SynMidKStar (x, y, z))) res';
      Pp.printf "\n";
      _die [%here]
    in *)
    let () =
      _strategy :=
        {
          !_strategy with
          search = UnSortedDFS 1;
          search_new_goals = false;
          result_expection = 1;
        }
    in
    let res' =
      List.fold_left
        (fun final_res (pre_len, line, post_len) ->
          if List.length final_res >= 1 then final_res
          else
            let r = refinement_loop ([], [ line ]) in
            let r = List.map (fun x -> SynMidKStar (pre_len, x, post_len)) r in
            (* let () =
              if List.length r > 0 then (
                Pp.printf "@{<bold>@{<red>gen new kstar@} on line@}\n%s\n"
                  (omit_layout_line line);
                Pp.printf "@{<bold>@{<red>gen new kstar result@} on line@}\n";
                print_mid_result (List.nth r 0);
                _die [%here])
              else ()
            in *)
            let final_res = final_res @ r in
            final_res)
        [] res'
    in
    res' (* List.map (fun x -> SynMidPlan x) res @ res' *)
  else List.map (fun x -> SynMidPlan x) res

and refine_one_step env (goal : line) : line list =
  let () = simp_print_syn_judgement goal in
  let ids = underived_act_ids goal in
  match ids with
  | id :: _ ->
      let () = Pp.printf "@{<bold>@{<red>backward@} on %i@}\n" id in
      let _ = try_pause () in
      backward env goal id
  | [] -> (
      match unchecked_act_ids goal with
      | id :: _ ->
          let () = Pp.printf "@{<bold>@{<red>forward@} on %i@}\n" id in
          let _ = try_pause () in
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

and gen_new_kstar env (goal : line) : (int * line * int) list =
  let () =
    Pp.printf "@{<bold>@{<red>gen new act@} on line@}\n%s\n"
      (omit_layout_line goal)
  in
  let lines = mk_singleton_star goal in
  let rules = select_gen_rules env in
  match rules with
  | [] -> []
  | (op, _) :: _ ->
      let goals =
        List.concat_map
          (fun (pre_len, g, post_len) ->
            let lines = gen_merge g op in
            List.map (fun line -> (pre_len, line, post_len)) lines)
          lines
      in
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
    let goals =
      List.sort (fun x y -> Int.compare (line_size x) (line_size y)) goals
    in
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
  let goals =
    List.sort (fun x y -> Int.compare (line_size x) (line_size y)) goals
  in
  goals
