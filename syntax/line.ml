open Zdatatype
open AutomataLibrary
open Common
open Ast
open SFA

let root_aid = -1

(* let dummy_aid = -100 *)
let se_to_regex x = MultiChar (CharSet.singleton x)

let layout_act { aop; aargs; aid; _ } =
  let op =
    spf "%s~%s"
      (match aid with Some aid -> string_of_int aid | None -> "?")
      aop
  in
  tpEvent (spf "%s(%s)" op (layout_qvs aargs))

let layout_line_elem_aux omit = function
  | LineAct act -> layout_act act
  (* | LineStar r -> if omit then "□*" else SFA.layout_regex (Star r) *)
  (* | LineMultiChar r -> if omit then "□" else SFA.layout_regex (MultiChar r) *)
  | LineStarMultiChar r ->
      if omit then "□*" else SFA.layout_regex (Star (MultiChar r))

let layout_line_elems elems =
  List.split_by ";" (layout_line_elem_aux false) elems

let layout_line { gprop; elems } =
  let line = layout_line_elems elems in
  spf "prop: %s\nline: %s" (layout_prop gprop) line

let omit_layout_line { gprop; elems } =
  let line = List.split_by ";" (layout_line_elem_aux true) elems in
  spf "prop: %s\nline: %s" (layout_prop gprop) line

let line_get_acts line =
  List.filter_map (function LineAct act -> Some act | _ -> None) line.elems

let get_aids line =
  (* only can be applied with registered line *)
  List.map
    (fun e -> match e.aid with Some aid -> aid | None -> _die [%here])
    (line_get_acts line)

let fresh_aid ids =
  List.fold_left (fun max id -> if max > id then max else id + 1) 0 ids

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
  (phi, { aop = op; aargs = vs'; aid = None; aparent = None; achildren = None })

let seq_line x y =
  { gprop = smart_and [ x.gprop; y.gprop ]; elems = x.elems @ y.elems }

type linear_regex_elem = LinearChar of Nt.nt sevent | LinearStar of CharSet.t

let regex_to_linear_regex r =
  let rec aux = function
    | Empty -> []
    | Eps -> [ [] ]
    | MultiChar cs ->
        List.map (fun se -> [ LinearChar se ]) (CharSet.to_list cs)
    | Star (MultiChar cs) -> [ [ LinearStar cs ] ]
    | Star _ ->
        _die_with [%here] "never"
        (* [ { gprop = mk_true; elems = [ LineStar cs ] } ] *)
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

let linear_regex_to_ses =
  List.filter_map (function LinearChar se -> Some se | LinearStar _ -> None)

let linear_regex_has_op op r =
  let ses = linear_regex_to_ses r in
  List.exists (fun (se : Nt.nt sevent) -> String.equal op se.op) ses

let linear_regex_elem_to_regex = function
  | LinearChar se -> se_to_regex se
  | LinearStar cs -> Star (MultiChar cs)

let linear_regex_to_regex r = seq (List.map linear_regex_elem_to_regex r)
let linear_regexs_to_regexs rs = alt_list (List.map linear_regex_to_regex rs)

let linear_regex_to_line r =
  let rec aux { gprop; elems } = function
    | [] -> { gprop; elems }
    | LinearChar se :: rest ->
        let phi, act = se_to_dummy_act se in
        aux { gprop = smart_and [ phi; gprop ]; elems = [ LineAct act ] } rest
    | LinearStar cs :: rest ->
        aux { gprop; elems = [ LineStarMultiChar cs ] } rest
  in
  aux { gprop = mk_true; elems = [] } r

let regex_to_lines r =
  let lr = regex_to_linear_regex r in
  List.map linear_regex_to_line lr

let elems_modify_by_id elems id f =
  List.map
    (function
      | LineAct ({ aid = Some aid; _ } as act) when aid == id -> LineAct (f act)
      | elem -> elem)
    elems

let act_add_parent parent act = { act with aparent = Some parent }
let act_add_children children act = { act with achildren = Some children }

let elems_add_id_parent elems id parent =
  elems_modify_by_id elems id (act_add_parent parent)

let elems_add_id_children elems id children =
  elems_modify_by_id elems id (act_add_children children)

let line_modify_by_id line id f =
  { line with elems = elems_modify_by_id line.elems id f }

let line_label_as_gen_act line id =
  line_modify_by_id line id (act_add_parent root_aid)

(* let line_to_id_regex any line =
  let aux = function
    | LineAct act -> MultiChar (IntSet.of_list [ act.aid ])
    | LineStarMultiChar _ -> Star (MultiChar any)
  in
  seq (List.map aux line.elems) *)

let rec merge_ids res ids1 ids2 =
  match (ids1, ids2) with
  | [], [] -> res
  | [], _ -> List.map (fun r -> r @ ids2) res
  | _, [] -> List.map (fun r -> r @ ids1) res
  | id1 :: ids1, id2 :: ids2 ->
      let res1 = List.map (fun r -> r @ [ id1 ]) res in
      let res2 = List.map (fun r -> r @ [ id2 ]) res in
      merge_ids res1 ids1 (id2 :: ids2) @ merge_ids res2 (id1 :: ids1) ids2

let line_elems_get_act_by_id id elems =
  List.find_map
    (function LineAct act when act.aid = id -> Some act | _ -> None)
    elems

let line_get_act_by_id id line =
  match line_elems_get_act_by_id id line.elems with
  | Some act -> act
  | None -> _die [%here]

(* type line_scratch =
  | ScratchAct of act
  | ScratchStar of CharSet.t
  | ScratchStarAny

let line_scratch_to_line (prop, scratch) =
  let elems =
    List.map
      (fun x ->
        match x with
        | ScratchAct act -> LineAct act
        | ScratchStar c -> LineStarMultiChar c
        | ScratchStarAny -> _die [%here])
      scratch
  in
  { gprop = prop; elems } *)

let charset_to_smap cs =
  CharSet.fold
    (fun { op; vs; phi } m ->
      StrMap.update op
        (function
          | None -> Some (vs, phi)
          | Some (vs', phi') ->
              let () =
                if not (List.equal (fun x y -> x.x == y.x) vs vs') then
                  _die [%here]
              in
              let phi = smart_and [ phi; phi' ] in
              Some (vs, phi))
        m)
    cs StrMap.empty

let smap_to_charset m =
  let res =
    CharSet.of_list
      (List.map (fun (op, (vs, phi)) -> { op; vs; phi }) (StrMap.to_kv_list m))
  in
  if CharSet.is_empty res then None else Some res

let unify_charset cs =
  let m = charset_to_smap cs in
  smap_to_charset m

let inter_charset cs1 cs2 =
  let m1 = charset_to_smap cs1 in
  let m2 = charset_to_smap cs2 in
  let m =
    StrMap.fold
      (fun op (vs, phi) res ->
        match StrMap.find_opt m2 op with
        | None -> res
        | Some (vs', phi') ->
            let () =
              if not (List.equal (fun x y -> x.x == y.x) vs vs') then
                _die [%here]
            in
            let phi = smart_and [ phi; phi' ] in
            StrMap.add op (vs, phi) res)
      m1 StrMap.empty
  in
  smap_to_charset m

let check_sat_se prop cs =
  let m = charset_to_smap cs in
  let m =
    StrMap.filter
      (fun _ (_, phi) -> Prover.check_sat_bool (None, smart_and [ prop; phi ]))
      m
  in
  smap_to_charset m

let merge_charset prop cs1 cs2 =
  match inter_charset cs1 cs2 with
  | None -> None
  | Some cs -> check_sat_se prop cs

let merge_act_with_phi (prop, act) (vs, phi) =
  let s =
    List.map (fun (x, y) -> (x.x, AVar y)) (_safe_combine [%here] vs act.aargs)
  in
  let phi = msubst subst_prop_instance s phi in
  if Prover.check_sat_bool (None, smart_and [ prop; phi ]) then Some phi
  else None

let merge_act_with_se (prop, act) { op; vs; phi } =
  if act.aop != op then None else merge_act_with_phi (prop, act) (vs, phi)

let merge_act_with_charset (prop, act) cs =
  let m = charset_to_smap cs in
  match StrMap.find_opt m act.aop with
  | None -> None
  | Some (vs, phi) -> merge_act_with_phi (prop, act) (vs, phi)

let rec is_empty_lr = function
  | [] -> true
  | LinearChar _ :: _ -> false
  | LinearStar _ :: rest -> is_empty_lr rest

let rec merge_line_with_linear_regex if_reuse (gprop, prefix) (elems, lr) =
  match (elems, lr) with
  | [], _ -> if is_empty_lr lr then [ { gprop; elems = prefix } ] else []
  | _ :: _, [] -> []
  | LineAct act :: elems', LinearChar se :: lr' ->
      if if_reuse act then
        match merge_act_with_se (gprop, act) se with
        | None -> []
        | Some phi ->
            let gprop = smart_and [ phi; gprop ] in
            merge_line_with_linear_regex if_reuse
              (gprop, prefix @ [ LineAct act ])
              (elems', lr')
      else []
  | LineAct act :: elems', LinearStar c :: lr' ->
      let res1 =
        merge_line_with_linear_regex if_reuse (gprop, prefix) (elems, lr')
      in
      let res2 =
        match merge_act_with_charset (gprop, act) c with
        | Some phi ->
            let gprop = smart_and [ phi; gprop ] in
            merge_line_with_linear_regex if_reuse
              (gprop, prefix @ [ LineAct act ])
              (elems', lr')
        | None -> []
      in
      res1 @ res2
  | LineStarMultiChar c :: elems', LinearChar se :: lr' ->
      let res1 =
        merge_line_with_linear_regex if_reuse (gprop, prefix) (elems', lr)
      in
      let phi, act = se_to_dummy_act se in
      let gprop = smart_and [ phi; gprop ] in
      let res2 =
        match merge_act_with_charset (gprop, act) c with
        | Some phi ->
            let gprop = smart_and [ phi; gprop ] in
            merge_line_with_linear_regex if_reuse
              (gprop, prefix @ [ LineAct act ])
              (elems', lr')
        | None -> []
      in
      res1 @ res2
  | LineStarMultiChar c :: elems', LinearStar c' :: lr' ->
      let res1 =
        merge_line_with_linear_regex if_reuse (gprop, prefix) (elems', lr)
      in
      let res2 =
        merge_line_with_linear_regex if_reuse (gprop, prefix) (elems, lr')
      in
      let res3 =
        match merge_charset gprop c c' with
        | Some c'' ->
            merge_line_with_linear_regex if_reuse
              (gprop, prefix @ [ LineStarMultiChar c'' ])
              (elems', lr')
        | None -> []
      in
      res1 @ res2 @ res3
(* let rec merge_line_to_scratch gprop (res : (Nt.t prop * line_scratch list) list)
    (line_scratch, elems) =
  match (line_scratch, elems) with
  | [], _ -> List.map (fun (prop, r) -> (smart_and [ gprop; prop ], r)) res
  | _ :: _, [] -> []
  | ScratchAct act :: line_scratch', LineAct act' :: elems' ->
      if act.aid == act'.aid then
        let res =
          List.map (fun (prop, r) -> (prop, r @ [ ScratchAct act ])) res
        in
        merge_line_to_scratch gprop res (line_scratch', elems')
      else []
  | ScratchAct act :: line_scratch', LineStarMultiChar c :: elems' -> (
      match line_elems_get_act_by_id act.aid elems with
      | Some _ -> merge_line_to_scratch gprop res (line_scratch, elems)
      | None ->
          let res =
            List.filter_map
              (fun (prop, r) ->
                match
                  merge_act_with_charset (smart_and [ gprop; prop ], act) c
                with
                | Some phi ->
                    Some (smart_and [ phi; prop ], r @ [ ScratchAct act ])
                | None -> None)
              res
          in
          let res1 = merge_line_to_scratch gprop res (line_scratch', elems') in
          let res2 = merge_line_to_scratch gprop res (line_scratch', elems) in
          res1 @ res2)
  | ScratchStar _ :: _, LineAct _ :: _ -> []
  | ScratchStar c :: line_scratch', LineStarMultiChar c' :: elems' ->
      let res =
        List.filter_map
          (fun (prop, r) ->
            let c = merge_charset (smart_and [ gprop; prop ]) c c' in
            match c with
            | None -> None
            | Some c -> Some (prop, r @ [ ScratchStar c ]))
          res
      in
      let res1 = merge_line_to_scratch gprop res (line_scratch', elems') in
      let res2 = merge_line_to_scratch gprop res (line_scratch', elems) in
      res1 @ res2
  | ScratchStarAny :: _, LineAct _ :: _ -> []
  | ScratchStarAny :: line_scratch', LineStarMultiChar c' :: elems' ->
      let res =
        List.map (fun (prop, r) -> (prop, r @ [ ScratchStar c' ])) res
      in
      let res1 = merge_line_to_scratch gprop res (line_scratch', elems') in
      let res2 = merge_line_to_scratch gprop res (line_scratch', elems) in
      res1 @ res2 *)
(* let inter_lines line1 line2 =
  (* let open IntAutomata in *)
  let ids1 = List.map (fun act -> act.aid) (line_get_acts line1) in
  let ids2 = List.map (fun act -> act.aid) (line_get_acts line2) in
  let dummy_merged_line =
    { gprop = mk_true; elems = line1.elems @ line2.elems }
  in
  let idss = merge_ids [] ids1 ids2 in
  let make_line_scratch ids =
    let acts =
      List.map
        (fun id ->
          let act = line_get_act_by_id id dummy_merged_line in
          ScratchAct act)
        ids
    in
    let res = List.concat_map (fun act -> [ act; ScratchStarAny ]) acts in
    ScratchStarAny :: res
  in
  let gprop = smart_and [ line1.gprop; line2.gprop ] in
  let scratches = List.map (fun x -> (gprop, make_line_scratch x)) idss in
  let scratches =
    List.concat_map
      (fun (prop, s) -> merge_line_to_scratch prop [] (s, line1.elems))
      scratches
  in
  let scratches =
    List.concat_map
      (fun (prop, s) -> merge_line_to_scratch prop [] (s, line2.elems))
      scratches
  in
  let lines = List.map line_scratch_to_line scratches in
  lines *)

let inter_line_with_regex if_reuse line1 regex =
  let lrs = regex_to_linear_regex regex in
  List.concat_map
    (fun lr ->
      merge_line_with_linear_regex if_reuse (line1.gprop, []) (line1.elems, lr))
    lrs

let line_insert_se if_reuse { gprop; elems } (id, se) =
  let rec aux (res, prefix) = function
    | [] -> res
    | LineAct act :: rest ->
        if if_reuse act then
          match merge_act_with_se (gprop, act) se with
          | None -> aux (res, prefix @ [ LineAct act ]) rest
          | Some phi ->
              let gprop = smart_and [ phi; gprop ] in
              let elems = prefix @ [ LineAct act ] @ rest in
              aux ((id, { gprop; elems }) :: res, prefix @ [ LineAct act ]) rest
        else aux (res, prefix @ [ LineAct act ]) rest
    | LineStarMultiChar c :: rest -> (
        let phi, act = se_to_dummy_act se in
        match merge_act_with_charset (smart_and [ phi; gprop ], act) c with
        | None -> aux (res, prefix @ [ LineStarMultiChar c ]) rest
        | Some phi ->
            let act = { act with aid = Some id } in
            let gprop = smart_and [ phi; gprop ] in
            let elems =
              prefix
              @ [ LineStarMultiChar c; LineAct act; LineStarMultiChar c ]
              @ rest
            in
            aux
              ((id, { gprop; elems }) :: res, prefix @ [ LineStarMultiChar c ])
              rest)
  in
  aux ([], []) elems
