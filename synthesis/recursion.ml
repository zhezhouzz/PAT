open Language

(* open Common *)
open Zdatatype

(* open SFA *)
open Plan

let rec drop_action = function
  | [] -> []
  | (_, LineAct _) :: rest -> rest
  | (_, LineStarMultiChar _) :: rest -> drop_action rest

let independent_subtrace elems =
  let rec aux (prefix, pending) (elems : (int * line_elem) list) =
    match elems with
    | [] -> if List.length pending == 0 then Some prefix else None
    | elem :: rest -> (
        match elem with
        | _, LineStarMultiChar _ -> aux (prefix @ [ elem ], pending) rest
        | _, LineAct act ->
            if
              line_size { gprop = mk_true; elems = List.map snd prefix } > 0
              && List.length pending == 0
            then Some prefix
            else
              let aid = act_get_id act in
              let pending' =
                act_get_children act @ List.filter (fun x -> x != aid) pending
              in
              aux (prefix @ [ elem ], pending') rest)
  in
  aux ([], []) elems

let find_mid_core elems =
  let rec aux candicate (elems : (int * line_elem) list) =
    match elems with
    | [] -> candicate
    | elem :: rest -> (
        match elem with
        | _, LineStarMultiChar _ -> aux (Some elem) rest
        | _, LineAct act -> (
            match act.aparent with
            | None -> aux (Some elem) rest
            | Some _ -> candicate))
  in
  aux None elems

let rec valid_subtrace num_act = function
  | [] -> true
  | (_, LineAct _) :: rest ->
      if num_act >= 0 then valid_subtrace (num_act + 1) rest else false
  | (_, LineStarMultiChar _) :: rest ->
      if num_act <= 0 then valid_subtrace num_act rest
      else valid_subtrace (-1) rest

let drop_children elems children =
  let rec aux prefix = function
    | [] -> prefix
    | (i, LineAct act) :: rest ->
        let aid = act_get_id act in
        if List.exists (fun x -> x == aid) children then prefix
        else aux (prefix @ [ (i, LineAct act) ]) rest
    | (i, LineStarMultiChar cs) :: rest ->
        aux (prefix @ [ (i, LineStarMultiChar cs) ]) rest
  in
  aux [] elems

let drop_by_name elems name =
  let rec aux prefix = function
    | [] -> prefix
    | (i, LineAct act) :: rest ->
        if String.equal name act.aop then prefix
        else aux (prefix @ [ (i, LineAct act) ]) rest
    | (i, LineStarMultiChar cs) :: rest ->
        aux (prefix @ [ (i, LineStarMultiChar cs) ]) rest
  in
  aux [] elems

let rec get_core_subtrace (elems : (int * line_elem) list) :
    (int * line_elem) list option =
  let () =
    Pp.printf "@{<bold>@{<red>get_core_subtrace@} on elems@} %s\n"
      (omit_layout_line_elems (List.map snd elems))
  in
  if valid_subtrace 0 elems then Some elems
  else
    match elems with
    | [] -> None
    | (_, LineAct act) :: rest when String.equal act.aop "abs" ->
        let elems =
          match rest with
          | [] -> _die [%here]
          | (_, LineAct _) :: rest -> drop_by_name rest "endAbs"
          | (_, LineStarMultiChar _) :: _ -> _die [%here]
        in
        get_core_subtrace elems
    | (_, LineAct act) :: rest ->
        (* let aid = act_get_id act in *)
        let children = act_get_children act in
        let elems = drop_children rest children in
        get_core_subtrace elems
    | (_, LineStarMultiChar _) :: rest -> get_core_subtrace rest

let get_adjacent_elems elems id =
  let rec aux condidate elems =
    match elems with
    | [] -> condidate
    | (i, LineAct act) :: rest ->
        if i == id then aux (condidate @ [ (i, LineAct act) ]) rest
        else aux [] rest
    | (i, LineStarMultiChar cs) :: rest ->
        aux (condidate @ [ (i, LineStarMultiChar cs) ]) rest
  in
  aux [] elems

let get_mid elems elems' =
  let elems = List.mapi (fun i elem -> (i, elem)) elems in
  let rec aux prefix global curr =
    (* let () =
      Pp.printf "@{<bold>@{<red>get_mid@}  global: %s, curr: %s@}\n"
        (List.split_by_comma (fun (x, _) -> spf "%i" x) global)
        (List.split_by_comma (fun (x, _) -> spf "%i" x) curr)
    in *)
    match (global, curr) with
    | _, [] -> (prefix, List.map snd global)
    | (i, elem) :: global', (j, _) :: curr' ->
        if i == j then aux prefix global' curr'
        else if i < j then aux (prefix @ [ elem ]) global' curr
        else _die [%here]
    | _, _ -> _die [%here]
  in
  aux [] elems elems'

let erase_act act =
  let aargs = List.map (fun { x; ty } -> (spf "zz%s" x)#:ty) act.aargs in
  (* let () =
    Pp.printf "@{<bold>@{<red>args@}@} %s --> %s \n"
      (layout_qvs act.aargs) (layout_qvs aargs)
  in *)
  { act with aparent = None; achildren = None; aid = None; aargs }

let erase_core elems =
  List.map
    (function
      | LineAct act -> LineAct (erase_act act)
      | LineStarMultiChar cs -> LineStarMultiChar cs)
    elems

let mk_all_cs env =
  let op_names = List.map _get_x (ctx_to_list env.event_tyctx) in
  let atoms = List.map (op_to_top_sevent env.event_tyctx) op_names in
  let all_cs = SFA.CharSet.of_list atoms in
  all_cs

let fill_core env elems =
  let all_cs = mk_all_cs env in
  match elems with
  | [] -> []
  | [ LineStarMultiChar _ ] -> elems
  | LineAct _ :: _ -> _die [%here]
  | LineStarMultiChar _ :: rest -> (
      match List.rev rest with
      | LineAct _ :: _ ->
          [ LineStarMultiChar all_cs ] @ rest @ [ LineStarMultiChar all_cs ]
      | _ -> LineStarMultiChar all_cs :: rest)

let template_1 =
  ([ "tyOpen"; "var"; "tyClose" ], [ "tyOpen"; "const"; "tyClose" ], [ "depth" ])

let match_template_raw template elems =
  let rec aux prefix (matched, temp) rest =
    (* let () =
        Pp.printf "@{<bold>@{<red>match_template@} [%s] on elems@} %s\n"
          (List.split_by_comma (fun x -> x) temp)
          (layout_line_elems rest)
      in *)
    match temp with
    | [] -> (
        let l = List.rev (line_elems_drop_stars elems) in
        match l with
        | LineAct { aop; aargs = [ arg ]; _ } :: _ when String.equal aop "depth"
          ->
            Some (prefix, matched, rest, VVar arg)
        | _ -> Some (prefix, matched, rest, VConst (I 0)))
    | op :: temp -> (
        match rest with
        | [] -> None
        | LineAct act :: rest ->
            if String.equal act.aop op then
              aux prefix (matched @ [ LineAct act ], temp) rest
            else aux (prefix @ [ LineAct act ]) ([], template) rest
        | LineStarMultiChar cs :: rest ->
            if List.length matched == 0 then
              aux (prefix @ [ LineStarMultiChar cs ]) (matched, op :: temp) rest
            else
              aux prefix (matched @ [ LineStarMultiChar cs ], op :: temp) rest)
  in
  aux [] ([], template) elems

let match_template (template, template_back, template_ghost) elems =
  match match_template_raw template elems with
  | Some (prefix, matched, rest, v) ->
      let match_back line =
        let line =
          if List.length template_ghost == 0 then line
          else
            match match_template_raw template_ghost line.elems with
            | None -> line
            | Some (_, _, post, _) ->
                let goal = { gprop = line.gprop; elems = post } in
                goal
        in
        match match_template_raw template_back line.elems with
        | None -> None
        | Some (pre, _, post, _) ->
            let goal = { gprop = line.gprop; elems = pre @ post } in
            Some (goal, List.length pre)
      in
      Some (prefix, matched, rest, match_back, v)
  | None -> None

let generalize_goal elems =
  match elems with
  | [ LineAct act0; LineAct act1; LineAct act2 ] ->
      [
        LineAct act0;
        LineAct { act1 with aargs = [ default_iter_var ] };
        LineAct act2;
      ]
  | _ -> _die [%here]

let fill_core_2 new_goal goal =
  let matched = match_template template_1 goal.elems in
  match matched with
  | Some (prefix, matched, postfix, match_back, v) ->
      let prefix = line_elems_drop_stars prefix in
      let postfix = line_elems_drop_stars postfix in
      (* let matched = line_elems_drop_stars matched in *)
      let matched' = generalize_goal matched in
      let goal = { gprop = goal.gprop; elems = prefix @ postfix } in
      let line_b1 = { gprop = mk_true; elems = matched' } in
      let line_b2 = new_goal in
      Some ((goal, List.length prefix), line_b1, line_b2, match_back, v)
  | None -> None

let select_template init_r goal =
  let () =
    Pp.printf "@{<bold>@{<red>select_template@} on line@}\n%s\n"
      (omit_layout_line goal)
  in
  let () =
    Pp.printf "@{<bold>@{<red>select_template@} on line@}\n%s\n"
      (layout_line goal)
  in
  let plans = regex_to_lines init_r in
  let plans =
    List.map
      (fun plan ->
        let _, plan, _ = register_line_under_plan [] plan in
        plan)
      plans
  in
  let new_goal = match plans with [] -> _die [%here] | plan :: _ -> plan in
  let core = fill_core_2 new_goal goal in
  match core with
  | Some core -> Some core
  | None -> (
      let match_func goal =
        match
          get_core_subtrace (List.mapi (fun i elem -> (i, elem)) goal.elems)
        with
        | None -> None
        | Some elems' ->
            let () =
              Pp.printf "@{<bold>@{<red>select_core@} on line@}\n%s\n"
                (layout_line
                   { gprop = goal.gprop; elems = List.map snd elems' })
            in
            let prefix, postfix = get_mid goal.elems elems' in
            let prefix, core, postfix =
              (prefix, List.map snd elems', postfix)
            in
            let () =
              Pp.printf "@{<bold>@{<red>select_prefix@} on line@}\n%s\n"
                (omit_layout_line { gprop = goal.gprop; elems = prefix });
              Pp.printf "@{<bold>@{<red>select_core@} on line@}\n%s\n"
                (omit_layout_line { gprop = goal.gprop; elems = core });
              Pp.printf "@{<bold>@{<red>select_postfix@} on line@}\n%s\n"
                (omit_layout_line { gprop = goal.gprop; elems = postfix })
            in
            let goal = { gprop = goal.gprop; elems = prefix @ postfix } in
            Some (goal, core, List.length prefix)
      in
      match match_func goal with
      | None -> None
      | Some (goal, core, pre_len) ->
          let line_b1 =
            { gprop = mk_true; elems = line_elems_drop_stars core }
          in
          let match_back line =
            match match_func line with
            | None -> None
            | Some (goal, _, pre_len) -> Some (goal, pre_len)
          in
          Some ((goal, pre_len), line_b1, new_goal, match_back, VConst (I 0)))

let fill_core_1 env line =
  let all_cs = mk_all_cs env in
  match line.elems with
  | [ LineAct act0; LineAct act1; LineAct act2; LineAct act3 ] ->
      let sid = "zi"#:Nt.int_ty in
      let ty = "zty"#:(Nt.Ty_constructor ("stlcTy", [])) in
      let act0', act1', act2', act3' =
        map4 erase_act (act0, act1, act2, act3)
      in
      let elems =
        [
          LineAct { act0' with aargs = [ default_iter_var ] };
          LineAct { act1' with aargs = [ sid ] };
          LineStarMultiChar all_cs;
          LineAct act2';
          LineStarMultiChar all_cs;
          LineAct { act3' with aargs = [ sid; ty ] };
        ]
      in
      let line =
        {
          gprop =
            lit_to_prop
              (AAppOp
                 ( "is_int_ty"#:(Nt.construct_arr_tp ([ ty.ty ], Nt.bool_ty)),
                   [ tvar_to_lit ty ] ));
          elems;
        }
      in
      let _, line, _ =
        register_line_under_plan (get_aids_is_exists line) line
      in
      line
  | elems -> { gprop = line.gprop; elems }
