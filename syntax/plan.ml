open Zdatatype
open AutomataLibrary
open Common
open Ast
open SFA
include Line

let layout_assigns m =
  StrMap.fold (fun x y acc -> spf "%s; %s -> %s" acc x y) m ""

let layout_massage_chain (x, xs) =
  spf "%s->[%s]" (string_of_int x) (List.split_by_comma string_of_int xs)

let print_plan line =
  Pp.printf "@{<bold>freeVars:@} %s\n"
    (layout_typed_var_list (_get_freeVars line));
  Pp.printf "@{<bold>line:@} %s\n" (omit_layout_line line);
  Pp.printf "@{<bold>elems:@} %s\n" (layout_line_elems line.elems);
  Pp.printf "@{<bold>checkedActs:@} %s\n"
    (List.split_by_comma layout_massage_chain
    @@ IntMap.to_kv_list (_get_checkedActs line))

let register_act_under_plan ids act =
  match act.aid with
  | Some _ -> None
  | None ->
      let aid = fresh_aid ids in
      Some ({ act with aid = Some aid }, aid)

let register_elems_under_plan ids elems =
  let rec aux (ids, elems, newIds) = function
    | [] -> (ids, elems, newIds)
    | LineAct act :: post -> (
        match register_act_under_plan ids act with
        | None -> aux (ids, elems @ [ LineAct act ], newIds) post
        | Some (act, aid) ->
            aux (aid :: ids, elems @ [ LineAct act ], aid :: newIds) post)
    | elem :: post -> aux (ids, elems @ [ elem ], newIds) post
  in
  let ids, elems, newIds = aux (ids, [], []) elems in
  (ids, elems, newIds)

let register_line_under_plan ids { gprop; elems } =
  let ids, elems, newIds = register_elems_under_plan ids elems in
  (ids, { gprop; elems }, newIds)

let label_as_gen_act act = { act with aparent = Some root_aid }

let is_checked_act act =
  match act.achildren with None -> false | Some _ -> true

let is_derived_act act = match act.aparent with None -> false | Some _ -> true

let force_get_act_id act =
  match act.aid with None -> _die_with [%here] "never" | Some aid -> aid

let unchecked_act_ids line =
  let acts = line_get_acts line in
  let acts = List.filter (fun act -> not (is_checked_act act)) acts in
  List.map force_get_act_id acts

let underived_act_ids line =
  let acts = line_get_acts line in
  let acts = List.filter (fun act -> not (is_derived_act act)) acts in
  List.map force_get_act_id acts

(* let well_formed_plan plan =
  match (unchecked_act_ids plan, underived_act_ids plan) with
  | [], [] -> true
  | _ -> false *)

let finished_plan plan =
  match (unchecked_act_ids plan, underived_act_ids plan) with
  | [], [] -> true
  | _ -> false

let mk_se ctx op f =
  let vs = _get_force [%here] ctx op in
  let vs =
    match vs with
    | Nt.Ty_record { fds; _ } -> fds
    | _ -> _die_with [%here] "unimp"
  in
  { op; vs; phi = f vs }

let mk_se_with_args ctx op args =
  mk_se ctx op (fun vs ->
      smart_and
        (List.map2
           (fun x y -> lit_to_prop (mk_lit_eq_lit [%here] (AVar x) (AVar y)))
           vs args))

let prop_to_conjuncts phi =
  let rec aux prop =
    match prop with
    | Lit x -> [ Lit x ]
    | And xs -> List.concat_map aux xs
    | _ -> [ prop ]
  in
  aux phi

let get_assigns conjs vs =
  let assignments =
    List.filter_map
      (fun x ->
        let l = List.filter_map (fun prop -> is_eq_phi x prop) conjs in
        match l with
        | [] -> None
        | [ AVar y ] -> Some y
        | [ _ ] -> None
        | _ ->
            let () =
              Pp.printf "@{<bold>multiple assignments:@} %s = %s\n" x.x
                (List.split_by_comma (fun x -> layout_lit x) l)
            in
            _die_with [%here] "never")
      vs
  in
  if List.length assignments < List.length vs then None else Some assignments

let plan_task_id_to_act line task_id =
  let res =
    List.find_map
      (fun act -> if act.aid == task_id then Some act else None)
      (line_get_acts line)
  in
  match res with Some act -> act | None -> _die_with [%here] "never"

let elems_divide_by_task_id elems task_id =
  let rec aux pre = function
    | [] -> _die_with [%here] "never"
    | LineAct ({ aid = Some aid; _ } as act) :: post ->
        if aid == task_id then (pre, act, post)
        else aux (pre @ [ LineAct act ]) post
    | elem :: post -> aux (pre @ [ elem ]) post
  in
  aux [] elems

let line_divide_by_task_id { gprop; elems } task_id =
  (gprop, elems_divide_by_task_id elems task_id)

(* let merge_pre_with_history_automata (pre, history) =
  let () = Pp.printf "@{<bold>before merge_pre_with_history_automata@}\n" in
  inter_line_with_regex pre history

let merge_post_with_future_automata plan (post, future) =
  let futures = regex_to_lines future in
  let res =
    List.map
      (fun future ->
        let plan, future = register_line_under_plan plan future in
        let post = inter_lines post future in
        (plan, post))
      futures
  in
  res *)

let charset_to_se loc s =
  let open SFA in
  match List.of_seq @@ CharSet.to_seq s with [ x ] -> x | _ -> _die loc

let merge_act_with_cur_automata ((prop, act), cur) =
  match cur with
  | MultiChar cs -> (
      match merge_act_with_charset (prop, act) cs with
      | Some phi -> (smart_and [ phi; prop ], act)
      | None -> _die_with [%here] "never")
  | _ -> _die_with [%here] "never"

let gen_merge line op =
  let knownIds = get_aids line in
  let rec aux { gprop; elems } =
    match elems with
    | [] -> []
    | LineAct act :: post ->
        List.map
          (fun { gprop; elems } -> { gprop; elems = LineAct act :: elems })
          (aux { gprop; elems = post })
    | LineStarMultiChar c :: post ->
        let res1 =
          List.map
            (fun { gprop; elems } ->
              { gprop; elems = LineStarMultiChar c :: elems })
            (aux { gprop; elems = post })
        in
        let res2 =
          let m = charset_to_smap c in
          match StrMap.find_opt m op with
          | Some (vs, phi) ->
              let phi, act = se_to_dummy_act { op; vs; phi } in
              let gprop = smart_and [ phi; gprop ] in
              if _check_sat gprop then
                let elems =
                  LineStarMultiChar c :: LineAct act :: LineStarMultiChar c
                  :: post
                in
                [ { gprop; elems } ]
              else []
          | None -> []
        in
        res2 @ res1
  in
  let res = aux line in
  let res =
    List.map
      (fun { gprop; elems } ->
        let _, elems, _ = register_elems_under_plan knownIds elems in
        { gprop; elems })
      res
  in
  res

let forward_merge line id (history, cur, future) =
  let knownIds = get_aids line in
  let gprop, (pre, act, post) = line_divide_by_task_id line id in
  let gprop, act = merge_act_with_cur_automata ((gprop, act), cur) in
  let res =
    inter_line_with_regex (fun _ -> true) { gprop; elems = pre } history
  in
  List.concat_map
    (fun { gprop; elems = pre } ->
      let res =
        inter_line_with_regex
          (fun act -> not (is_derived_act act))
          { gprop; elems = post } future
      in
      List.map
        (fun { gprop; elems = post } ->
          let knownIds, post, newIds =
            register_elems_under_plan knownIds post
          in
          (* let act, act_aid, knownIds =
            match register_act_under_plan knownIds act with
            | None -> _die_with [%here] "never"
            | Some (act, aid) -> (act, aid, aid :: knownIds)
          in *)
          let _, pre, _ = register_elems_under_plan knownIds pre in
          let elems = pre @ [ LineAct act ] @ post in
          let act_aid = force_get_act_id act in
          let elems =
            List.fold_right
              (fun id elems -> elems_add_id_parent elems id act_aid)
              newIds elems
          in
          let elems = elems_add_id_children elems act_aid newIds in
          { gprop; elems })
        res)
    res

(* let get_id_from_varname plan varname =
  let res =
    ActMap.fold
      (fun act id res ->
        if List.exists (fun x -> x.x == varname) act.aargs then Some id else res)
      plan.actMap None
  in
  match res with None -> _die_with [%here] "never" | Some id -> id *)

(* let locate_backward_act ctx plan elems
    (history1, (prev : Nt.nt sevent), history2) =
  let rec aux (res, pre1) = function
    | [] -> res
    | LineAct act :: pre2 when String.equal act.aop prev.op ->
        if is_checked_act plan act then aux (res, pre1 @ [ LineAct act ]) pre2
        else
          let actProp, act = merge_act_with_se (act, prev) in
          let line1s =
            merge_pre_with_history_automata ctx plan (pre1, history1)
          in
          List.concat_map
            (fun line1 ->
              let line2s =
                merge_pre_with_history_automata ctx plan (pre2, history2)
              in
              List.concat_map
                (fun line2 ->
                  let gprop = smart_and [ line1.gprop; actProp; line2.gprop ] in
                  let line =
                    {
                      gprop;
                      elems = line1.elems @ [ LineAct act ] @ line2.elems;
                    }
                  in
                  aux (line :: res, pre1 @ [ LineAct act ]) pre2)
                line2s)
            line1s
    | elem :: post -> aux (res, pre1 @ [ elem ]) post
  in
  aux ([], []) elems *)

let plan_add_cargs plan args =
  let freeVars = _get_freeVars plan in
  let args, arg_phis = List.split @@ List.map destruct_cty_var args in
  match List.interset (fun x y -> String.equal x.x y.x) args freeVars with
  | [] -> { plan with gprop = smart_and (plan.gprop :: arg_phis) }
  | _ -> _die_with [%here] "never"

let insert_prev_se_into_pre line se =
  line_insert_se (fun act -> not (is_checked_act act)) line se

let backward_merge plan curId (history1, prev, history2, cur, future) =
  let () =
    Pp.printf "@{<bold>backward_merge@}[%i]: %s\n" curId (omit_layout_line plan);
    Pp.printf "%s ; <%s> ; %s ; <%s> ; %s\n" (layout_regex history1)
      (layout_sevent prev) (layout_regex history2) (layout_sevent cur)
      (layout_regex future)
  in
  let knownIds = get_aids plan in
  let gprop, (pre, curAct, post) = line_divide_by_task_id plan curId in
  match merge_act_with_se (gprop, curAct) cur with
  | None ->
      let () = _die_with [%here] "never" in
      []
  | Some phi ->
      let () =
        Pp.printf "@{<bold>backward_merge@} curActProp: %s; curAct: %s\n"
          (layout_prop phi) (layout_act curAct)
      in
      let gprop = smart_and [ phi; gprop ] in
      let res =
        insert_prev_se_into_pre { gprop; elems = pre } (fresh_aid knownIds, prev)
      in
      List.concat_map
        (fun (prev_id, line) ->
          let () =
            Pp.printf "@{<bold>backward merge@} prev_id: %i - %s\n" prev_id
              (omit_layout_line line)
          in
          let knownIds = prev_id :: knownIds in
          let gprop, (pre1, prevAct, pre2) =
            line_divide_by_task_id line prev_id
          in
          let () = Pp.printf "@{<bold>merge pre1@}\n" in
          let res =
            inter_line_with_regex
              (fun _ -> true)
              { gprop; elems = pre1 } history1
          in
          List.concat_map
            (fun { gprop; elems = pre1 } ->
              let () =
                Pp.printf "@{<bold>backward merge@} pre1: %s\n"
                  (omit_layout_line_elems pre1)
              in
              let () = Pp.printf "@{<bold>merge pre2@}\n" in
              let res =
                inter_line_with_regex
                  (fun _ -> true)
                  { gprop; elems = pre2 } history2
              in
              List.concat_map
                (fun { gprop; elems = pre2 } ->
                  let () = Pp.printf "@{<bold>merge post@}\n" in
                  let res =
                    inter_line_with_regex
                      (fun act -> not (is_derived_act act))
                      { gprop; elems = post } future
                  in
                  List.map
                    (fun { gprop; elems = post } ->
                      let () =
                        Pp.printf
                          "@{<bold>backward merge result@} %s ; <%s> ; %s ; \
                           <%s> ; %s\n"
                          (omit_layout_line_elems pre1)
                          (layout_act prevAct)
                          (omit_layout_line_elems pre2)
                          (layout_act curAct)
                          (omit_layout_line_elems post)
                      in
                      let knownIds, post, newIds =
                        register_elems_under_plan knownIds
                          (pre2 @ [ LineAct curAct ] @ post)
                      in
                      let _, pre, _ = register_elems_under_plan knownIds pre1 in
                      let elems = pre @ [ LineAct prevAct ] @ post in
                      let achildren = curId :: newIds in
                      let elems =
                        List.fold_right
                          (fun id elems -> elems_add_id_parent elems id prev_id)
                          achildren elems
                      in
                      let elems =
                        elems_add_id_children elems prev_id achildren
                      in
                      { gprop; elems })
                    res)
                res)
            res)
        res

(* let left_most_se plan = *)
(*   let rec aux (pre, rest) = *)
(*     match rest with *)
(*     | [] -> None *)
(*     | PlanSe cur :: post -> Some (pre, cur, post) *)
(*     | elem :: post -> aux (pre @ [ elem ], post) *)
(*   in *)
(*   aux ([], plan) *)

(* let right_most_se plan = *)
(*   let* pre, cur, post = left_most_se (List.rev plan) in *)
(*   (\* let () = if !counter >= 2 then _die [%here] in *\) *)
(*   Some (List.rev post, cur, List.rev pre) *)

let plan_size plan = List.length (line_get_acts plan)
