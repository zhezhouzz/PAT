open Zdatatype
open AutomataLibrary
open Common
open Ast
open SFA

let root_aid = -1
let dummy_aid = -100
let se_to_regex x = MultiChar (CharSet.singleton x)

let layout_act { aop; aargs; aid } =
  let op = spf "%i~%s" aid aop in
  tpEvent (spf "%s(%s)" op (layout_qvs aargs))

let layout_line_elem_aux omit = function
  | LineAct act -> layout_act act
  | LineStar r -> if omit then "□*" else SFA.layout_regex (Star r)
(* | LineMultiChar cs -> SFA.layout_regex (MultiChar cs) *)

let layout_line { gprop; elems } =
  let line = List.split_by ";" (layout_line_elem_aux false) elems in
  spf "prop: %s\nline: %s" (layout_prop gprop) line

let omit_layout_line { gprop; elems } =
  let line = List.split_by ";" (layout_line_elem_aux true) elems in
  spf "prop: %s\nline: %s" (layout_prop gprop) line

let layout_assigns m =
  StrMap.fold (fun x y acc -> spf "%s; %s -> %s" acc x y) m ""

let layout_massage_chain (x, xs) =
  spf "%s->[%s]" (string_of_int x) (List.split_by_comma string_of_int xs)

let layout_actMap m =
  ActMap.fold (fun x y acc -> spf "%s; %s -> %i" acc (layout_act x) y) m ""

let print_plan { line; freeVars; assigns; checkedActs; _ } =
  Pp.printf "@{<bold>freeVars:@} %s\n" (layout_typed_var_list freeVars);
  Pp.printf "@{<bold>assigns:@} %s\n" (layout_assigns assigns);
  Pp.printf "@{<bold>line:@} %s\n" (omit_layout_line line);
  Pp.printf "@{<bold>checkedActs:@} %s\n"
    (List.split_by_comma layout_massage_chain @@ IntMap.to_kv_list checkedActs)

let line_get_acts line =
  List.filter_map (function LineAct act -> Some act | _ -> None) line.elems

let get_aids line = List.map (fun e -> e.aid) (line_get_acts line)
let fresh_aid actMap = ActMap.cardinal actMap

let linear_regex_to_regex = function
  | LChar se -> se_to_regex se
  (* | LMultiChar cs -> MultiChar cs *)
  | LStar cs -> Star cs

let linear_regex_list_to_regex l = seq (List.map linear_regex_to_regex l)

let se_to_dummy_act { op; vs; phi } =
  let vs' = List.map (fun x -> (Rename.unique_var x.x)#:x.ty) vs in
  let () =
    Pp.printf "@{<bold>se_to_dummy_act: fresh vars:@} %s\n"
      (layout_typed_var_list vs')
  in
  let m =
    List.map (fun (x, y) -> (x.x, AVar y)) (_safe_combine [%here] vs vs')
  in
  let phi = msubst subst_prop_instance m phi in
  (phi, { aop = op; aargs = vs'; aid = dummy_aid })

(* let linear_regex_to_line_elem prop = function
  | LChar { op; vs; phi } ->
      let prop', act = se_to_dummy_act { op; vs; phi } in
      (smart_add_to prop' prop, LineAct act)
  (* | LMultiChar cs -> (prop, LineMultiChar cs) *)
  | LStar cs -> (prop, LineStar cs)

let linear_regex_to_line l =
  List.fold_left
    (fun { gprop; elems } r ->
      let gprop, elem = linear_regex_to_line_elem gprop r in
      { gprop; elems = elems @ [ elem ] })
    { gprop = mk_true; elems = [] }
    l *)

let register_act_under_plan_with_gprop plan { aid; aop; aargs } =
  if aid == dummy_aid then
    let act = { aop; aargs; aid = fresh_aid plan.actMap } in
    let plan = { plan with actMap = ActMap.add act act.aid plan.actMap } in
    let () =
      match
        List.interset (fun x y -> String.equal x.x y.x) aargs plan.freeVars
      with
      | [] -> ()
      | _ ->
          let () =
            Pp.printf
              "@{<bold>register_act_under_plan_with_gprop: freeVars:@} %s\n"
              (layout_typed_var_list plan.freeVars);
            Pp.printf "@{<bold>register_act_under_plan_with_gprop: act:@} %s\n"
              (layout_act act)
          in
          _die_with [%here] "never"
    in
    let plan = { plan with freeVars = aargs @ plan.freeVars } in
    let plan =
      {
        plan with
        assigns =
          List.fold_right (fun x -> StrMap.add x.x x.x) aargs plan.assigns;
      }
    in
    Some (plan, act)
  else None

let register_elems_under_plan plan elems =
  let rec aux (plan, elems, newIds) = function
    | [] -> (plan, elems, newIds)
    | LineAct act :: post -> (
        match register_act_under_plan_with_gprop plan act with
        | None -> aux (plan, elems @ [ LineAct act ], newIds) post
        | Some (plan, act) ->
            let newIds = newIds @ [ act.aid ] in
            aux (plan, elems @ [ LineAct act ], newIds) post)
    | elem :: post -> aux (plan, elems @ [ elem ], newIds) post
  in
  let plan, elems, newIds = aux (plan, [], []) elems in
  (plan, elems, newIds)

let register_line_under_plan plan { gprop; elems } =
  let plan, elems, _ = register_elems_under_plan plan elems in
  (plan, { gprop; elems })

let update_plan_with_line plan line =
  let () =
    Pp.printf "@{<bold>update_plan_with_line@} line: %s\n" (layout_line line)
  in
  let plan, line = register_line_under_plan plan line in
  let actMap =
    List.fold_left
      (fun m act -> if ActMap.mem act m then m else ActMap.add act act.aid m)
      plan.actMap (line_get_acts line)
  in
  let plan = { plan with line; actMap } in
  plan

let label_as_gen_act plan id =
  let checkedActs =
    IntMap.update root_aid
      (function None -> _die_with [%here] "never" | Some x -> Some (id :: x))
      plan.checkedActs
  in
  { plan with checkedActs }

let new_plan freeVars line =
  let plan =
    {
      line;
      freeVars;
      assigns = StrMap.empty;
      checkedActs = IntMap.(add root_aid [] empty);
      actMap = ActMap.empty;
    }
  in
  update_plan_with_line plan line

let is_checked_act plan act = IntMap.mem act.aid plan.checkedActs

let is_derived_act plan act =
  IntMap.exists (fun _ ids -> List.mem act.aid ids) plan.checkedActs

let unchecked_act_ids plan =
  let acts = line_get_acts plan.line in
  let acts = List.filter (fun act -> not (is_checked_act plan act)) acts in
  List.map (fun act -> act.aid) acts

let underived_act_ids plan =
  let acts = line_get_acts plan.line in
  let acts = List.filter (fun act -> not (is_derived_act plan act)) acts in
  List.map (fun act -> act.aid) acts

let well_formed_plan plan =
  match (unchecked_act_ids plan, underived_act_ids plan) with
  | [], [] -> true
  | _ -> false

let regex_to_linear_regex_list r =
  let rec aux = function
    | Empty -> []
    | Eps -> [ [] ]
    | MultiChar cs -> List.map (fun se -> [ LChar se ]) (CharSet.to_list cs)
    | Star cs -> [ [ LStar cs ] ]
    | Seq [] -> [ [] ]
    | Seq (x :: xs) ->
        let x = aux x in
        let xs = aux (Seq xs) in
        let res =
          List.fold_left (fun res x -> res @ List.map (fun y -> x @ y) xs) [] x
        in
        res
    | Alt (x, y) -> aux x @ aux y
    | Inters _ | Comple _ -> _die_with [%here] "never"
  in
  aux r

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

let plan_elem_to_regex ctx = function
  | LineAct { aop; aargs; _ } ->
      MultiChar (CharSet.singleton (mk_se_with_args ctx aop aargs))
  (* | LineMultiChar cs -> MultiChar cs *)
  | LineStar r -> r

let line_to_regex ctx l = seq (List.map (plan_elem_to_regex ctx) l)

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

let try_recover_act actMap { op; vs; phi } =
  (* let () =
    Pp.printf "@{<bold>try_recover_act@} op: %s vs: %s phi: %s\n" op
      (layout_typed_var_list vs) (layout_prop phi)
  in *)
  match get_assigns (prop_to_conjuncts phi) vs with
  | Some aargs ->
      let act = { aop = op; aargs; aid = dummy_aid } in
      let act =
        match ActMap.find_opt act actMap with
        | Some id -> { act with aid = id }
        | None -> act
        (* let () =
              Pp.printf "@{<bold>try_recover_act@} act: %s\n" (layout_act act)
            in
            let () = Pp.printf "@{<bold>actMap@} %s\n" (layout_actMap actMap) in
            _die_with [%here] "never" *)
      in
      let phi =
        List.fold_left
          (fun phi (x, y) -> subst_prop_instance x.x (AVar y) phi)
          phi
        @@ List.combine vs aargs
      in
      let phi = simpl_eq_in_prop phi in
      Some (act, phi)
  | None -> None

let linear_regex_to_line_elem actMap prop = function
  | LChar se -> (
      match try_recover_act actMap se with
      | None ->
          let prop', act = se_to_dummy_act se in
          (smart_add_to prop' prop, LineAct act)
      | Some (act, phi) -> (smart_add_to phi prop, LineAct act))
  (* | LMultiChar cs -> (prop, LineMultiChar cs) *)
  | LStar cs -> (prop, LineStar cs)

let linear_regex_to_lines actMap gprop r =
  List.fold_left
    (fun { gprop; elems } r ->
      let gprop, elem = linear_regex_to_line_elem actMap gprop r in
      { gprop; elems = elems @ [ elem ] })
    { gprop; elems = [] } r

let regex_to_lines actMap prop r =
  let ls = regex_to_linear_regex_list r in
  List.map (linear_regex_to_lines actMap prop) ls

let global_prop_to_lines = regex_to_lines ActMap.empty mk_true

(* let left_most_se plan = match get_acts plan with [] -> None | x :: _ -> Some x

let right_most_se plan =
  let* pre, cur, post = left_most_se (List.rev plan) in
  let () =
    Pp.printf "@{<green>right most@} se[%s] in %s\n" (layout_sevent cur)
      (layout plan)
  in
  Some (List.rev post, cur, List.rev pre) *)

let inter_regex r1 r2 =
  SFA.(dfa_to_reg @@ minimize @@ compile_regex_to_dfa @@ inter r1 r2)

let inter_lines (ctx, actMap) line1 line2 =
  let r1 = line_to_regex ctx line1.elems in
  let r2 = line_to_regex ctx line2.elems in
  let r = inter_regex r1 r2 in
  let ls = regex_to_lines actMap (smart_add_to line1.gprop line2.gprop) r in
  ls

let plan_task_id_to_act plan task_id =
  let actMap = plan.actMap in
  let act =
    ActMap.fold
      (fun act id res ->
        match res with
        | None -> if id == task_id then Some act else None
        | Some _ -> res)
      actMap None
  in
  match act with Some act -> act | None -> _die_with [%here] "never"

let elems_divide_by_task_id elems task_id =
  let rec aux pre = function
    | [] -> _die_with [%here] "never"
    | LineAct act :: post ->
        if act.aid == task_id then (pre, act, post)
        else aux (pre @ [ LineAct act ]) post
    | elem :: post -> aux (pre @ [ elem ]) post
  in
  aux [] elems

let plan_divide_by_task_id plan task_id =
  (plan.line.gprop, elems_divide_by_task_id plan.line.elems task_id)

let merge_pre_with_history_automata ctx plan (pre, history) =
  let pre = line_to_regex ctx pre in
  let r = inter_regex pre history in
  let ls = regex_to_lines plan.actMap mk_true r in
  ls

let merge_post_with_future_automata ctx plan (post, future) =
  let lregexs = regex_to_linear_regex_list future in
  let lines = List.map (linear_regex_to_lines plan.actMap mk_true) lregexs in
  let res =
    List.fold_left
      (fun res future ->
        let plan, future = register_line_under_plan plan future in
        let post =
          inter_lines (ctx, plan.actMap)
            { gprop = mk_true; elems = post }
            future
        in
        res @ List.map (fun x -> (plan, x)) post)
      [] lines
  in
  res

let merge_act_with_se (({ aop; aargs; aid } as act), ({ op; vs; phi } as se)) =
  let () =
    Pp.printf "@{<bold>merge_act_with_se@} act: %s to se: %s\n" (layout_act act)
      (layout_sevent se)
  in
  let () = if not (String.equal op act.aop) then _die_with [%here] "never" in
  let m =
    List.map (fun (x, y) -> (x.x, AVar y)) (_safe_combine [%here] vs aargs)
  in
  let phi = msubst subst_prop_instance m phi in
  (phi, { aop; aargs; aid })

let charset_to_se loc s =
  let open SFA in
  match List.of_seq @@ CharSet.to_seq s with [ x ] -> x | _ -> _die loc

let merge_act_with_cur_automata (act, cur) =
  let se =
    match cur with
    | MultiChar cs -> charset_to_se [%here] cs
    | _ -> _die_with [%here] "never"
  in
  merge_act_with_se (act, se)

let forward_merge ctx plan id (history, cur, future) =
  let gprop, (pre, act, post) = plan_divide_by_task_id plan id in
  let propCur, act = merge_act_with_cur_automata (act, cur) in
  let pres = merge_pre_with_history_automata ctx plan (pre, history) in
  List.concat_map
    (fun pre ->
      let res = merge_post_with_future_automata ctx plan (post, future) in
      let res =
        List.map
          (fun (plan, post) ->
            let gprop = smart_and [ gprop; pre.gprop; propCur; post.gprop ] in
            let plan, post_elems, newIds =
              register_elems_under_plan plan post.elems
            in
            let line =
              { gprop; elems = pre.elems @ [ LineAct act ] @ post_elems }
            in
            let plan = update_plan_with_line plan line in
            let checkedActs = IntMap.add id newIds plan.checkedActs in
            { plan with checkedActs })
          res
      in
      res)
    pres

let get_id_from_varname plan varname =
  let res =
    ActMap.fold
      (fun act id res ->
        if List.exists (fun x -> x.x == varname) act.aargs then Some id else res)
      plan.actMap None
  in
  match res with None -> _die_with [%here] "never" | Some id -> id

let locate_backward_act ctx plan elems
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
  aux ([], []) elems

let plan_add_cargs plan args =
  let args, arg_phis = List.split @@ List.map destruct_cty_var args in
  let line =
    { plan.line with gprop = smart_and (plan.line.gprop :: arg_phis) }
  in
  { plan with line; freeVars = args @ plan.freeVars }

let backward_merge ctx plan id (history1, prev, history2, cur, future) =
  let gprop, (pre, curAct, post) = plan_divide_by_task_id plan id in
  let curActProp, curAct = merge_act_with_se (curAct, cur) in
  let pre_lines_from_located =
    (* In this case, we don't add new prev act into pre *)
    let lines = locate_backward_act ctx plan pre (history1, prev, history2) in
    List.map (fun line -> (plan, line)) lines
  in
  let pre_lines_from_inter =
    let phi, prevAct = se_to_dummy_act prev in
    let plan, prevAct =
      match register_act_under_plan_with_gprop plan prevAct with
      | None -> _die [%here]
      | Some res -> res
    in
    let history =
      seq [ history1; line_to_regex ctx [ LineAct prevAct ]; history2 ]
    in
    let pres = merge_pre_with_history_automata ctx plan (pre, history) in
    List.map
      (fun pre -> (plan, { pre with gprop = smart_and [ phi; pre.gprop ] }))
      pres
  in
  let pres = pre_lines_from_located @ pre_lines_from_inter in
  let plans =
    List.concat_map
      (fun (plan, pre) ->
        let res = merge_post_with_future_automata ctx plan (post, future) in
        List.map
          (fun (plan, post) ->
            let gprop =
              smart_and [ gprop; pre.gprop; curActProp; post.gprop ]
            in
            let elems = pre.elems @ [ LineAct curAct ] @ post.elems in
            let line = { gprop; elems } in
            update_plan_with_line plan line)
          res)
      pres
  in
  plans

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

let plan_size plan = List.length (line_get_acts plan.line)
