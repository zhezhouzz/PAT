open Language
open Common
open Zdatatype
open Optimize
open Norm
open Plan
open Gamma

let cur_to_obs { op; vs; phi } =
  let args = List.map (fun x -> (Rename.unique x.x) #: x.ty) vs in
  let phi =
    List.fold_right
      (fun (x, y) p -> subst_prop_instance x.x (AVar y) p)
      (_safe_combine [%here] vs args)
      phi
  in
  (args, phi, PlanAct { args; op })

let plan_to_acts (gamma, plan) =
  let pg, gamma, plan =
    List.fold_left
      (fun (pg, gamma, plan) elem ->
        match elem with
        | PlanSe cur ->
            let args, phi, elem = cur_to_obs cur in
            let gamma =
              { bvs = gamma.bvs @ args; bprop = smart_add_to phi gamma.bprop }
            in
            (pg @ [ elem ], gamma, plan @ [ elem ])
        | _ -> (pg, gamma, plan @ [ elem ]))
      ([], gamma, []) plan
  in
  (pg, (gamma, plan))

let synthesis_counter = ref 0

let incrAndStop n =
  if !synthesis_counter >= n then _die [%here]
  else synthesis_counter := !synthesis_counter + 1

let forward_synthesis_counter = ref 0

let forward_incrAndStop n =
  if !forward_synthesis_counter >= n then _die [%here]
  else forward_synthesis_counter := !forward_synthesis_counter + 1

let rec deductive_synthesis_reg env goal : plan_goal option =
  let goals = normalize_goal env goal in
  (* NOTE: in the beginning, there is not pending sub goals *)
  let goals =
    List.map (fun (gamma, plan) -> { gamma; plan; pg = []; solved = [] }) goals
  in
  let res = List.filter_map (deductive_synthesis_trace env) goals in
  match res with [] -> None | g :: _ -> Some g

and deductive_synthesis env goal : plan_goal option =
  let goals = normalize_goal env goal in
  (* NOTE: in the beginning, there is not pending sub goals *)
  let goals =
    List.map (fun (gamma, plan) -> { gamma; plan; pg = []; solved = [] }) goals
  in
  let res = List.filter_map (deductive_synthesis_trace env) goals in
  let () = match res with _ :: _ -> set_explore () | _ -> () in
  deductive_synthesis env goal

and gather_subgoal_from_plan goal =
  let pg', (ga, plan') = plan_to_acts (goal.gamma, goal.plan) in
  let pg' = PG.concat pg' goal.pg in
  let () =
    _log "syn" @@ fun _ ->
    Pp.printf "@{<green>previous remaining goals:@} %s\n"
      (Plan.layout_plan goal.pg);
    Pp.printf "@{<green>new remaining goals:@} %s\n" (Plan.layout_plan pg')
  in
  let goal = { goal with gamma = ga; pg = pg'; plan = plan' } in
  let goal = eliminate_buffer_plan_goal goal in
  goal

and gather_subgoal_from_plan_mid (goal : mid_plan_goal) =
  let pg', (ga, pre') = plan_to_acts (goal.gamma, goal.pre) in
  let pg' = PG.concat pg' goal.pg in
  let () =
    _log "syn" @@ fun _ ->
    Pp.printf "@{<green>previous remaining goals:@} %s\n"
      (Plan.layout_plan goal.pg);
    Pp.printf "@{<green>new remaining goals:@} %s\n" (Plan.layout_plan pg')
  in
  let goal = { goal with pg = pg'; gamma = ga; pre = pre' } in
  let goal = eliminate_buffer_plan_mid_goal goal in
  goal

and deductive_synthesis_trace env (goal : plan_goal) : plan_goal option =
  let () = simp_print_syn_judgement goal in
  let rec handle goal =
    let goal = gather_subgoal_from_plan goal in
    let () = simp_print_syn_judgement goal in
    match List.last_destruct_opt goal.pg with
    | None ->
        let goal = eliminate_buffer_plan_goal goal in
        let goal = { goal with plan = remove_star [%here] goal.plan } in
        record_result goal
    | Some (_, mid) ->
        let pre, _, post = Plan.divide_by_elem mid goal.plan in
        let () =
          _log "syn" @@ fun _ ->
          Pp.printf "@{<green>right most@} %s\n" (Plan.layout_elem mid)
        in
        let back_goal =
          {
            gamma = goal.gamma;
            pre;
            mid;
            post;
            pg = goal.pg;
            solved = goal.solved;
          }
        in
        let back_goal = optimize_back_goal back_goal in
        let* goal = backward env back_goal in
        handle goal
  in
  handle goal

and forward env (goal : mid_plan_goal) =
  (* let () = simp_print_forward_judgement goal in *)
  let init_elem = ref goal.mid in
  let counter = ref 0 in
  let rec aux (goal : pair_plan_goal) =
    let () = counter := !counter + 1 in
    let () = simp_print_forward_judgement goal in
    let () =
      _log "syn" @@ fun _ ->
      Printf.printf "init_elem %s\n" (Plan.omit_layout_elem !init_elem)
    in
    match goal.postUnsolved with
    | [] -> Some goal
    | elem :: postUnsolved -> (
        let op =
          match elem with
          | PlanSe { op; _ } -> Some op
          | PlanAct { op; _ } as elem ->
              if
                equal_plan_elem elem !init_elem
                && PG.in_preserve_subgoal elem goal.pg
              then Some op
              else None
          | _ -> None
        in
        match op with
        | None ->
            aux
              { goal with postUnsolved; preSolved = goal.preSolved @ [ elem ] }
        | Some op ->
            let () =
              _log "syn" @@ fun _ ->
              Printf.printf "do: %s\n" (Plan.layout_elem elem)
            in
            let () = Stat.incr_forward () in
            let hafts =
              haft_to_triple @@ fresh_haft
              @@ _get_force [%here] env.event_rtyctx op
            in
            let handle haft =
              let gargs, (args, retrty) = destruct_haft [%here] haft in
              let history, se, p = destruct_hap [%here] retrty in
              (* NOTE: history should be well-formed. *)
              let () =
                _log "syn" @@ fun _ ->
                Printf.printf "history: %s\n" (SFA.layout_raw_regex history)
              in
              let history_plan = raw_regex_to_plan history in
              let dep_elem =
                match elem with
                | PlanSe cur ->
                    let args = List.map (fun x -> x.x #: x.ty.nt) args in
                    Plan.smart_and_se cur (PlanAct { op; args })
                | PlanAct _ -> Plan.smart_and_se se elem
                | _ -> _die [%here]
              in
              let dep_elem =
                match dep_elem with
                | None -> _die_with [%here] "never"
                | Some x -> x
              in
              let args, arg_phis =
                List.split @@ List.map destruct_cty_var args
              in
              let gamma =
                {
                  bvs = goal.gamma.bvs;
                  bprop = smart_and (goal.gamma.bprop :: arg_phis);
                }
              in
              let p = List.map (fun se -> PlanSe se) p in
              let posts = Plan.insert p postUnsolved in
              let () =
                _log "syn" @@ fun _ ->
                Pp.printf "@{<bold>len(posts)@}: %i\n" (List.length posts)
              in
              let pres = Plan.merge_plan goal.preSolved history_plan in
              let () =
                _log "syn" @@ fun _ ->
                Pp.printf "@{<bold>len(pres)@}: %i\n" (List.length pres)
              in
              let goals =
                List.map (fun (pre, post) ->
                    let goal =
                      eliminate_buffer_plan_mid_goal
                        {
                          gamma;
                          pre;
                          mid = dep_elem;
                          post;
                          pg = goal.pg;
                          solved = goal.solved;
                        }
                    in
                    (goal, gargs @ args))
                @@ List.cross pres posts
              in
              goals
            in
            let goals = List.concat_map handle hafts in
            let goals =
              List.map
                (fun (goal, args) ->
                  let goal, args' =
                    optimize_back_goal_with_args_record goal args init_elem
                  in
                  (goal, args'))
                goals
            in
            let () =
              _log "syn" @@ fun _ ->
              Pp.printfBold "len(subgoals) " @@ spf "%i\n" (List.length goals)
            in
            let () =
              _log "syn" @@ fun _ ->
              Pp.printfBold "len(subgoals) " @@ spf "%i\n" (List.length goals)
            in
            let () =
              _log "syn" @@ fun _ ->
              List.iter
                (fun (goal, vars) ->
                  let () =
                    Pp.printfBold "vars:" @@ spf "%s\n" (layout_qvs vars)
                  in
                  simp_print_mid goal)
                goals
            in
            let abd_and_backtract (goal, args') =
              let () =
                _log "syn" @@ fun _ ->
                Pp.printf "@{<bold>After Opt@}: (%s)\n" (layout_qvs args');
                simp_print_mid goal
              in
              let* gamma' =
                Abduction.abduction_mid_goal env goal.gamma
                  (goal.pre, goal.mid, goal.post)
                  args'
              in
              let goal = { goal with gamma = gamma' } in
              let () =
                _log "syn" @@ fun _ ->
                Pp.printf "@{<bold>After Abduction@}:\n";
                simp_print_mid goal
              in
              let solved =
                if equal_plan_elem !init_elem goal.mid then goal.solved
                else goal.solved @ [ goal.mid ]
              in
              aux
                {
                  gamma = goal.gamma;
                  preSolved = goal.pre @ [ goal.mid ];
                  postUnsolved = goal.post;
                  pg = goal.pg;
                  solved;
                }
            in
            backtrack abd_and_backtract goals)
  in
  match goal with
  | { gamma; pre; mid; post; pg; solved } ->
      if PG.in_preserve_subgoal mid solved then Some goal
      else
        let goal =
          aux { gamma; preSolved = pre; postUnsolved = mid :: post; pg; solved }
        in
        let* goal = goal in
        let goal = eliminate_buffer_plan_pair_goal goal in
        let () =
          _log "syn" @@ fun _ ->
          Pp.printf "find %s in\n%s\n"
            (Plan.layout_elem !init_elem)
            (Plan.omit_layout goal.preSolved)
        in
        let pre, mid, post = Plan.divide_by_elem !init_elem goal.preSolved in
        Some
          {
            gamma = goal.gamma;
            pre;
            mid;
            post;
            pg = goal.pg;
            solved = goal.solved;
          }

and backward env (goal : mid_plan_goal) : plan_goal option =
  let op = Plan.elem_to_op [%here] goal.mid in
  let* goal = forward env goal in
  let goal = gather_subgoal_from_plan_mid goal in
  let goal = { goal with pg = PG.remove_preserve_subgoal goal.mid goal.pg } in
  let goal = optimize_back_goal goal in
  let () = simp_print_back_judgement goal in
  (* let () = Printf.printf "%i\n" !forward_synthesis_counter in *)
  if is_gen env op then
    let () = Stat.incr_backward () in
    Some
      {
        gamma = goal.gamma;
        plan = goal.pre @ [ goal.mid ] @ goal.post;
        pg = goal.pg;
        solved = goal.solved;
      }
  else
    let handle (se, haft) =
      let () =
        _log "syn" @@ fun _ ->
        Pp.printf "@{<bold>use rty@}\n@{<red>se@}: %s\n@{<red>haft@}: %s\n"
          (layout_se se)
          (layout_haft SFA.layout_raw_regex haft)
      in
      let () = Stat.incr_backward () in
      let elem =
        match Plan.smart_and_se se goal.mid with
        | Some x -> x
        | None -> _die [%here]
      in
      let gargs, (args, retrty) = destruct_haft [%here] haft in
      let history, dep_se, p = destruct_hap [%here] retrty in
      let () =
        _log "syn" @@ fun _ ->
        Pp.printf "@{<bold>dep_se:@} %s\n" (layout_se dep_se)
      in
      (* NOTE: history should be well-formed. *)
      let history_plan = raw_regex_to_plan history in
      let () =
        _log "syn" @@ fun _ ->
        Pp.printf "@{<bold>history_plan:@} %s\n" (Plan.omit_layout history_plan)
      in
      let p = List.map (fun se -> PlanSe se) p in
      let fs = Plan.parallel_interleaving p in
      let () =
        List.iter
          (fun (p1, p2) ->
            _log "syn" @@ fun _ ->
            Pp.printf "@{<bold>fs@}: %s -- %s\n" (Plan.layout p1)
              (Plan.layout p2))
          fs
      in
      let dep_elem =
        (* NOTE: the payload should just conj of eq *)
        let op, _, _ = _get_sevent_fields dep_se in
        let args = List.map (fun x -> x.x #: x.ty.nt) args in
        PlanAct { op; args }
      in
      let args, arg_phis = List.split @@ List.map destruct_cty_var args in
      let gamma =
        Gamma.simplify
          { goal.gamma with bprop = smart_and (goal.gamma.bprop :: arg_phis) }
      in
      let gamma, elem = eliminate_buffer_elem (gamma, elem) in
      (* NOTE: try exactly match *)
      let merge_with_history =
        List.concat_map (fun ((pre1, dep_elem', pre2), post) ->
            let pre1s = Plan.merge_plan history_plan pre1 in
            let () =
              _log "syn" @@ fun _ ->
              Pp.printfBold "pre1:" @@ spf " %s\n" (omit_layout_plan pre1)
            in
            let () =
              _log "syn" @@ fun _ ->
              Pp.printfBold "history_plan:"
              @@ spf " %s\n" (omit_layout_plan history_plan)
            in
            let () =
              _log "syn" @@ fun _ -> Pp.printfBold "pre1 after merge:" "\n"
            in
            let () = List.iter simp_print_plan_judgement pre1s in
            List.map (fun pre1 -> ((pre1, dep_elem', pre2), post)) pre1s)
      in
      let exactly_goals =
        let pres = exactly_match dep_se goal.pre in
        let () = _log "syn" @@ fun _ -> Pp.printfBold "exactly pre:" "\n" in
        let () = List.iter simp_print_mid_judgement pres in
        let res =
          List.concat_map
            (fun (pre1, mid, pre2) ->
              List.concat_map
                (fun (f11, f12) ->
                  let pres = Plan.insert f11 pre1 in
                  let posts = Plan.insert f12 goal.post in
                  let l = List.cross pres posts in
                  List.map (fun (pre1, post) -> ((pre1, mid, pre2), post)) l)
                fs)
            pres
        in
        let res = merge_with_history res in
        let goals =
          List.map
            (fun ((pre1, dep_elem', pre2), post) ->
              {
                gamma;
                pre = pre1;
                mid = dep_elem';
                post = pre2 @ [ elem ] @ post;
                solved = goal.solved @ [ elem ];
                pg = PG.remove_preserve_subgoal dep_elem' goal.pg;
              })
            res
        in
        let goals = List.map eliminate_buffer_plan_mid_goal goals in
        let vars = gargs @ args in
        let goals =
          List.map
            (fun g ->
              let g, vars = optimize_back_goal_with_args g vars in
              (vars, { g with pg = PG.remove_preserve_subgoal g.mid g.pg }))
            goals
        in
        goals
      in
      let normal_goals =
        let res =
          List.concat_map
            (fun (f11, f12) ->
              let () = _log "syn" @@ fun _ -> Pp.printfBold "init post:" "\n" in
              let () =
                _log "syn" @@ fun _ ->
                Pp.printf "%s\n" @@ Plan.omit_layout_plan goal.post
              in
              let posts = Plan.insert f12 goal.post in
              (* let old_cur =  { op; vs; phi = smart_add_to phi' phi } in *)
              let f11' = dep_elem :: f11 in
              let pres =
                List.map (Plan.divide_by_elem dep_elem)
                @@ Plan.insert f11' goal.pre
              in
              let () = _log "syn" @@ fun _ -> Pp.printfBold "pres:" "\n" in
              let () = List.iter simp_print_mid_judgement pres in
              List.cross pres posts)
            fs
        in
        let res = merge_with_history res in
        let goals =
          List.map
            (fun ((pre1, dep_elem', pre2), post) ->
              {
                gamma;
                pre = pre1;
                mid = dep_elem';
                post = pre2 @ [ elem ] @ post;
                solved = goal.solved @ [ elem ];
                pg = goal.pg;
              })
            res
        in
        let goals = List.map eliminate_buffer_plan_mid_goal goals in
        let vars = gargs @ args in
        let goals =
          List.map
            (fun g ->
              let g, vars = optimize_back_goal_with_args g vars in
              (vars, g))
            goals
        in
        goals
      in
      let () =
        _log "syn" @@ fun _ ->
        Pp.printfBold "gargs:" @@ spf "%s\n" (layout_qvs gargs);
        Pp.printfBold "args:" @@ spf "%s\n" (layout_qvs args);
        Pp.printfBold "len(exactly) " @@ spf "%i\n" (List.length exactly_goals);
        List.iter (fun (_, g) -> simp_print_mid g) exactly_goals;
        Pp.printfBold "len(exactly) " @@ spf "%i\n" (List.length normal_goals);
        List.iter (fun (_, g) -> simp_print_mid g) normal_goals
      in
      normal_goals @ exactly_goals
    in
    let rules = select_rule_by_future env op in
    let () =
      List.iteri
        (fun i (se, haft) ->
          let () =
            _log "syn" @@ fun _ ->
            Pp.printf
              "@{<bold>available rty %i@}\n@{<red>se@}: %s\n@{<red>haft@}: %s\n"
              i (layout_se se)
              (layout_haft SFA.layout_raw_regex haft)
          in
          ())
        rules
    in
    let goals = List.concat_map handle rules in
    let goals =
      List.map
        (fun (args, g) ->
          let g, args' = optimize_back_goal_with_args g args in
          let () =
            _log "syn" @@ fun _ ->
            Pp.printf "@{<bold>After Opt@}: (%s)\n" (layout_qvs args');
            simp_print_mid g
          in
          (args', g))
        goals
    in
    let () =
      _log "syn" @@ fun _ ->
      List.iteri
        (fun i (args', g) ->
          Pp.printf "@{<bold>[%i]After Opt@}: (%s)\n" i (layout_qvs args');
          simp_print_mid g)
        goals
    in
    let abd_and_backtract (args', (g : mid_plan_goal)) =
      let* gamma' =
        Abduction.abduction_mid_goal env g.gamma (g.pre, g.mid, g.post) args'
      in
      let g' = { g with gamma = gamma' } in
      let () =
        _log "syn" @@ fun _ ->
        Pp.printf "@{<bold>After Abduction@}:\n";
        simp_print_mid g'
      in
      backward env g'
    in
    backtrack abd_and_backtract (List.rev goals)
