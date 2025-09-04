open Zdatatype
open AutomataLibrary
open Common
open Ast
open SFA

let _check_sat prop =
  let res = Prover.check_sat_bool (None, prop) in
  (* let () = Pp.printf "@{<bold>_check_sat[%b]@} %s\n" res (layout_prop prop) in *)
  res

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

let omit_layout_line_elems elems =
  List.split_by ";" (layout_line_elem_aux true) elems

let layout_linear_elem_aux omit = function
  | LinearChar se -> layout_sevent se
  | LinearStar r -> if omit then "□*" else SFA.layout_regex (Star (MultiChar r))

let layout_linear_elems elems =
  List.split_by ";" (layout_linear_elem_aux false) elems

let omit_layout_linear_elems elems =
  List.split_by ";" (layout_linear_elem_aux true) elems

let layout_line { gprop; elems } =
  let line = layout_line_elems elems in
  spf "prop: %s\nline: %s" (layout_prop gprop) line

let omit_layout_line { gprop; elems } =
  let line = List.split_by ";" (layout_line_elem_aux true) elems in
  spf "prop: %s\nline: %s" (layout_prop gprop) line

let line_get_acts line =
  List.filter_map (function LineAct act -> Some act | _ -> None) line.elems

let act_get_id act = match act.aid with Some aid -> aid | None -> _die [%here]

let get_aids line =
  (* only can be applied with registered line *)
  List.map act_get_id (line_get_acts line)

let fresh_aid ids =
  List.fold_left (fun max id -> if max > id then max else id + 1) 0 ids

let new_var x =
  let y = Rename.unique_var x.x in
  if String.equal y x.x then
    let z = Rename.unique_var x.x in
    if String.equal z x.x then _die [%here] else z#:x.ty
  else y#:x.ty

let se_to_dummy_act { op; vs; phi } =
  let vs' = List.map new_var vs in
  let () =
    Pp.printf "@{<bold>se_to_dummy_act: fresh vars:@} %s\n"
      (layout_typed_var_list vs')
  in
  let m =
    List.map (fun (x, y) -> (x.x, AVar y)) (_safe_combine [%here] vs vs')
  in
  let phi = msubst subst_prop_instance m phi in
  ( phi,
    {
      aop = op;
      aargs = vs';
      aid = None;
      aparent = None;
      achildren = None;
      tmp = -1;
    } )

let seq_line x y =
  { gprop = smart_and [ x.gprop; y.gprop ]; elems = x.elems @ y.elems }

let desyntax_regex r = dfa_to_reg @@ minimize @@ compile_regex_to_dfa r

let regex_to_linear_regex r =
  let rec aux = function
    | Empty -> []
    | Eps -> [ [] ]
    | MultiChar cs ->
        List.map (fun se -> [ LinearChar se ]) (CharSet.to_list cs)
    | Star r -> (
        match desyntax_regex r with
        | MultiChar cs -> [ [ LinearStar cs ] ]
        | _ -> _die_with [%here] "never")
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
        aux
          { gprop = smart_and [ phi; gprop ]; elems = elems @ [ LineAct act ] }
          rest
    | LinearStar cs :: rest ->
        aux { gprop; elems = elems @ [ LineStarMultiChar cs ] } rest
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
                if not (List.equal (fun x y -> String.equal x.x y.x) vs vs')
                then _die [%here]
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
              if not (List.equal (fun x y -> String.equal x.x y.x) vs vs') then
                _die [%here]
            in
            let phi = smart_and [ phi; phi' ] in
            StrMap.add op (vs, phi) res)
      m1 StrMap.empty
  in
  let res = smap_to_charset m in
  (* let () =
    Pp.printf "@{<bold>inter_charset@} %s inter %s = %s\n" (layout_charset cs1)
      (layout_charset cs2)
      (match res with Some cs -> layout_charset cs | None -> "None")
  in *)
  res

let check_sat_se prop cs =
  let m = charset_to_smap cs in
  let m =
    StrMap.filter (fun _ (_, phi) -> _check_sat @@ smart_and [ prop; phi ]) m
  in
  smap_to_charset m

let merge_charset prop cs1 cs2 =
  match inter_charset cs1 cs2 with
  | None -> None
  | Some cs -> check_sat_se prop cs

let merge_act_with_phi (prop, act) (vs, phi) =
  let () =
    Pp.printf "@{<bold>merge_act_with_phi@} %s ; %s\n" (layout_qvs act.aargs)
      (layout_qvs vs)
  in
  let s =
    List.map (fun (x, y) -> (x.x, AVar y)) (_safe_combine [%here] vs act.aargs)
  in
  let phi = msubst subst_prop_instance s phi in
  if _check_sat @@ smart_and [ prop; phi ] then Some phi else None

let merge_act_with_se (prop, act) { op; vs; phi } =
  let () =
    Pp.printf "@{<bold>merge_act_with_se@} %s ; %s\n" (layout_act act)
      (layout_sevent { op; vs; phi })
  in
  if not (String.equal act.aop op) then None
  else merge_act_with_phi (prop, act) (vs, phi)

let merge_act_with_charset (prop, act) cs =
  let m = charset_to_smap cs in
  match StrMap.find_opt m act.aop with
  | None -> None
  | Some (vs, phi) -> merge_act_with_phi (prop, act) (vs, phi)

let rec is_empty_lr = function
  | [] -> true
  | LinearChar _ :: _ -> false
  | LinearStar _ :: rest -> is_empty_lr rest

let rec is_empty_line_elems = function
  | [] -> true
  | LineAct _ :: _ -> false
  | LineStarMultiChar _ :: rest -> is_empty_line_elems rest

let simplify_line { gprop; elems } =
  let rec aux prevCs elems =
    match elems with
    | [] -> (
        match prevCs with None -> [] | Some c -> [ LineStarMultiChar c ])
    | LineAct act :: elems' -> (
        match prevCs with
        | None -> LineAct act :: aux prevCs elems'
        | Some c -> [ LineStarMultiChar c; LineAct act ] @ aux None elems')
    | LineStarMultiChar c :: elems' ->
        let prevCs =
          match prevCs with
          | None -> Some c
          | Some prevCs -> (
              match inter_charset c prevCs with
              | None -> _die [%here]
              | Some c -> Some c)
        in
        aux prevCs elems'
  in
  let elems = aux None elems in
  { gprop; elems }

let clear_tmp_in_elem = function
  | LineAct act -> LineAct { act with tmp = -1 }
  | elem -> elem

let clear_tmp_in_line line =
  { line with elems = List.map clear_tmp_in_elem line.elems }

let merge_line_with_acts if_reuse line ses =
  let line = clear_tmp_in_line line in
  let multi_concat x l =
    List.map (fun { gprop; elems } -> { gprop; elems = x @ elems }) l
  in
  let rec aux { gprop; elems } ses =
    let () =
      Pp.printf "@{<bold>merge_line_with_acts@} elems: %s\n"
        (omit_layout_line_elems elems)
    in
    let () =
      Pp.printf "@{<bold>merge_line_with_acts@} ses: %s\n"
        (List.split_by_comma
           (fun (idx, se) -> spf "%i, %s" idx (layout_sevent se))
           ses)
    in
    match (elems, ses) with
    | _, [] -> [ { gprop; elems } ]
    | [], _ -> []
    | LineAct act :: elems', (idx, se) :: ses' ->
        let res2 =
          if if_reuse act then
            match merge_act_with_se (gprop, act) se with
            | None -> []
            | Some phi ->
                let gprop = smart_and [ phi; gprop ] in
                let act = { act with tmp = idx } in
                multi_concat [ LineAct act ]
                  (aux { gprop; elems = elems' } ses')
          else []
        in
        let res1 =
          multi_concat [ LineAct act ] (aux { gprop; elems = elems' } ses)
        in
        res1 @ res2
    | LineStarMultiChar c :: elems', (idx, se) :: ses' ->
        let res2 =
          let phi, act = se_to_dummy_act se in
          let gprop = smart_and [ phi; gprop ] in
          let act = { act with tmp = idx } in
          match merge_act_with_charset (gprop, act) c with
          | None ->
              let () =
                Pp.printf "@{<bold>merge_line_with_acts@} failed: %s\n"
                  (layout_prop phi)
              in
              []
          | Some phi ->
              let gprop = smart_and [ phi; gprop ] in
              multi_concat
                [ LineStarMultiChar c; LineAct act; LineStarMultiChar c ]
                (aux { gprop; elems } ses')
        in
        let res1 =
          multi_concat [ LineStarMultiChar c ]
            (aux { gprop; elems = elems' } ses)
        in
        res1 @ res2
  in
  let res = aux line ses in
  res

let fill_line line lr =
  let opt_line_cons x l =
    match l with
    | None -> None
    | Some { gprop; elems } -> Some { gprop; elems = x :: elems }
  in
  let rec aux { gprop; elems } lr =
    match (elems, lr) with
    | [], _ ->
        if is_empty_lr (List.map snd lr) then Some { gprop; elems }
        else _die [%here]
    | _, [] -> _die [%here]
    | LineStarMultiChar c :: elems', (_, LinearStar c') :: _ -> (
        let c'' = merge_charset gprop c c' in
        match c'' with
        | None -> None
        | Some c'' ->
            opt_line_cons (LineStarMultiChar c'')
              (aux { gprop; elems = elems' } lr))
    | LineStarMultiChar _ :: elems', (_, LinearChar _) :: _ ->
        aux { gprop; elems = elems' } lr
    | LineAct act :: elems', (idx, LinearChar _) :: lr' ->
        if act.tmp == -1 then None
        else if act.tmp == idx then
          opt_line_cons (LineAct act) (aux { gprop; elems = elems' } lr')
        else _die [%here]
    | LineAct act :: elems', (_, LinearStar c') :: lr' ->
        if act.tmp == -1 then
          let phi = merge_act_with_charset (gprop, act) c' in
          match phi with
          | None -> None
          | Some phi ->
              let gprop = smart_and [ phi; gprop ] in
              opt_line_cons (LineAct act) (aux { gprop; elems = elems' } lr)
        else aux { gprop; elems } lr'
  in
  aux line lr

let layout_tmp_results tag lines =
  Pp.printf "@{<bold>%s@} lines: %i\n" tag (List.length lines);
  List.iter
    (fun line ->
      Pp.printf "@{<bold>%s@} lines: %s\n%s\n" tag (omit_layout_line line)
        (layout_line_elems line.elems))
    lines

let merge_line_with_linear_regex if_reuse line lr =
  match lr with
  | [] ->
      if is_empty_line_elems line.elems then
        [ { gprop = line.gprop; elems = [] } ]
      else []
  | _ ->
      let lr = List.mapi (fun idx se -> (idx, se)) lr in
      let ses =
        List.filter_map
          (fun (idx, se) ->
            match se with LinearChar se -> Some (idx, se) | _ -> None)
          lr
      in
      let lines = merge_line_with_acts if_reuse line ses in
      layout_tmp_results "merge" lines;
      let lines = List.map simplify_line lines in
      layout_tmp_results "simplify" lines;
      let lines = List.filter_map (fun line -> fill_line line lr) lines in
      layout_tmp_results "fill" lines;
      let lines = List.map clear_tmp_in_line lines in
      lines

(* let merge_line_with_lr line lr =
  let multi_cons x l = List.map (fun l -> x :: l) l in
  let rec aux ({gprop; elems}, lr) =
    match (elems, lr) with
    | _, [] -> if is_empty_line_elems elems then [ {gprop; elems} ] else []
    | [], _ -> if is_empty_lr lr then [ {gprop; elems} ] else []
    | [LineAct act], [LinearStar c] ->
      let phi = merge_act_with_charset (gprop, act) c in
      (match phi with
      | None -> []
      | Some phi -> [{gprop = smart_and [phi; gprop]; elems}])
    | [LineAct act], [LinearChar se] ->
      let phi = merge_act_with_se (gprop, act) se in
      (match phi with
      | None -> []
      | Some phi -> [{gprop = smart_and [phi; gprop]; elems}])
    | [LineAct _], LinearStar _ :: lr' ->
      aux ({gprop; elems}, lr')
    | [LineAct act], LinearChar se :: lr' ->
      let phi = merge_act_with_se (gprop, act) se in
      (match phi with
      | None -> []
      | Some phi -> aux ({gprop = smart_and [phi; gprop]; elems}, lr'))
    | [LineStarMultiChar c], LinearStar c' :: lr' ->
      let c'' = merge_charset gprop c c' in
      (match c'' with
      | None -> []
      | Some c'' ->
        multi_cons (LineStarMultiChar c'') @@
         aux ({gprop; elems}, lr'))
    | [LineStarMultiChar c], LinearChar se :: lr' ->
      let p, act = se_to_dummy_act se in
      let gprop = smart_and [p; gprop] in
      let phi = merge_act_with_charset (gprop, act) c in
      (match phi with
      | None -> []
      | Some phi ->
        multi_cons (LineAct act) @@
         aux ({gprop = smart_and [phi; gprop]; elems}, lr')) 
         
    | LineAct act :: elems', LinearChar se :: lr' ->
      
  in
  aux (elems, ids) *)

let locate_linear_regex_to_line elems (ids : (int * string) list) =
  let multi_cons x l = List.map (fun l -> x :: l) l in
  let rec aux (elems, (ids : (int * string) list)) =
    match (elems, ids) with
    | [], [] -> [ [] ]
    | [], _ -> []
    | _, [] -> [ [] ]
    | LineAct act :: elems', (id, op) :: ids' ->
        let res1 = multi_cons (LineAct act, []) @@ aux (elems', ids) in
        let res2 =
          if String.equal act.aop op then
            multi_cons (LineAct act, [ id ]) @@ aux (elems', ids')
          else []
        in
        res1 @ res2
    | LineStarMultiChar c :: elems', _ ->
        let rec acc (prev : int list) ids =
          match ids with
          | [] -> multi_cons (LineStarMultiChar c, prev) @@ aux (elems', [])
          | (id, op) :: ids' ->
              let res1 =
                multi_cons (LineStarMultiChar c, prev) @@ aux (elems', ids)
              in
              let res2 =
                if CharSet.exists (fun c -> String.equal c.op op) c then
                  acc (prev @ [ id ]) ids'
                else []
              in
              res1 @ res2
        in
        acc [] ids
  in
  aux (elems, ids)

(* let strach_to_elems scracth lr =
  match lr with
  | [] -> _die_with [%here] "never"
  | e :: lr' ->
    let rec aux scracth (id, e, lr) =
      match scracth with
      | [] -> _die_with [%here] "never"
      | (LineAct act, [id']) :: scracth' ->
        if if_reuse act then
          match merge_act_with_se (gprop, act) e with
          | None -> []
          | Some phi ->
              let gprop = smart_and [ phi; gprop ] in
              aux (scracth', lr')
        match e with
        | LinearStar c -> (e, ids) :: aux (scracth', lr')
        | LinearChar se -> (LineAct se, ids) :: aux (scracth', lr')
    match e with
    | LinearStar c -> (e, ids) :: aux (scracth, lr')
    | LinearChar se -> (LineAct se, ids) :: aux (scracth, lr')

   *)

let get_lr_elem_by_id lr id =
  match List.nth_opt lr id with Some elem -> elem | None -> _die [%here]

let get_prev_lr_star_by_id lr id =
  if id == 0 then None
  else
    match get_lr_elem_by_id lr (id - 1) with
    | LinearStar c -> Some c
    | _ -> None

let get_next_lr_star_by_id lr id =
  match List.nth_opt lr (id + 1) with
  | Some (LinearStar c) -> Some c
  | _ -> None

(* let fill_rest scracth len =
    let fill_ids (start, ids, post) =
      let start = match start with
      | None -> (match ids with
      | [] -> _die [%here]
      | id :: _ -> id) 
      | Some id -> id
    in 
    let rec aux cur = function
    | [] -> if cur < post then cur :: aux (cur + 1) [] else []
    |  *)
(* let rec aux prev scracth =
    match scracth with
    | [] -> []
    | [(e, ids)] ->
    | (LineAct act, ids) :: elems', (id, le) :: lr' ->
      if 
      if if_reuse act then
        match merge_act_with_se (gprop, act) se with
        | None -> []
        | Some phi ->
            let gprop = smart_and [ phi; gprop ] in
            aux (gprop, prefix @ [ LineAct act ]) (elems', lr')
      else []
  | LineAct act :: elems', LinearStar c :: lr' ->
      let res1 = aux (gprop, prefix) (elems, lr') in
      let res2 =
        match merge_act_with_charset (gprop, act) c with
        | Some phi ->
            let gprop = smart_and [ phi; gprop ] in
            aux (gprop, prefix @ [ LineAct act ]) (elems', lr)
        | None -> []
      in
      res1 @ res2
  in
  aux [] len *)

(* let ids_to_lrs_list ids lr =
  let rec aux = function
    | [], _ -> []
    | _, [] -> _die_with [%here] "never"
    | id :: ids', (i, e) :: lr' ->
        if id == i - 1 then
          match e with
          | LinearChar _ -> aux (id :: ids', lr')
          | LinearStar _ -> e :: aux (id :: ids', lr')
        else if id == i then 
          (match ids', lr' with
          | 
          )
          
          e :: aux (ids', lr')
        else if id < i - 1 then aux (id :: ids', lr')
        else _die [%here]
  in
  aux (ids, lr) *)

(* let expand_line_elem (elem, ids) lr = *)

(* let merge_line_with_linear_regex if_reuse (gprop, prefix) (elems, lr) =
  let rec aux (gprop, prefix) (elems, lr) =
    (* let () =
    Pp.printf
      "@{<bold>aux@}\n\
       prefix: %s\n\
       line: %s\n\
       regex: %s\n"
      (omit_layout_line { gprop; elems = prefix })
      (omit_layout_line_elems elems)
      (omit_layout_linear_elems lr)
  in *)
    match (elems, lr) with
    | [], _ -> if is_empty_lr lr then [ { gprop; elems = prefix } ] else []
    | _ :: _, [] ->
        []
        (* if is_empty_line_elems elems then [ { gprop; elems = prefix } ] else [] *)
    | LineAct act :: elems', LinearChar se :: lr' ->
        if if_reuse act then
          match merge_act_with_se (gprop, act) se with
          | None -> []
          | Some phi ->
              let gprop = smart_and [ phi; gprop ] in
              aux (gprop, prefix @ [ LineAct act ]) (elems', lr')
        else []
    | LineAct act :: elems', LinearStar c :: lr' ->
        let res1 = aux (gprop, prefix) (elems, lr') in
        let res2 =
          match merge_act_with_charset (gprop, act) c with
          | Some phi ->
              let gprop = smart_and [ phi; gprop ] in
              aux (gprop, prefix @ [ LineAct act ]) (elems', lr)
          | None -> []
        in
        res1 @ res2
    | LineStarMultiChar c :: elems', LinearChar se :: lr' ->
        let res1 = aux (gprop, prefix) (elems', lr) in
        let phi, act = se_to_dummy_act se in
        let gprop = smart_and [ phi; gprop ] in
        let res2 =
          match merge_act_with_charset (gprop, act) c with
          | Some phi ->
              let gprop = smart_and [ phi; gprop ] in
              aux (gprop, prefix @ [ LineAct act ]) (elems', lr')
          | None -> []
        in
        res1 @ res2
    | LineStarMultiChar c :: elems', LinearStar c' :: lr' ->
        (* let res1 =
        aux  (gprop, prefix) (elems', lr)
      in *)
        let res2 = aux (gprop, prefix) (elems, lr') in
        let res3 =
          match merge_charset gprop c c' with
          | Some c'' ->
              (* let () =
              Pp.printf "@{<bold>merge_charset@}  %s\n" (layout_charset c'')
            in *)
              aux (gprop, prefix @ [ LineStarMultiChar c'' ]) (elems', lr)
          | None -> []
        in
        (* let () =
        List.iteri
          (fun i x -> Pp.printf "@{<bold>res3[%i]:@} %s\n" i (layout_line x))
          res3
      in *)
        res2 @ res3
  in
  if _check_sat gprop then
    match lr with
    | [] -> if is_empty_line_elems elems then [ { gprop; elems = [] } ] else []
    | _ -> aux (gprop, prefix) (elems, lr)
  else [] *)

let inter_line_with_regex if_reuse line1 regex =
  let lrs = regex_to_linear_regex regex in
  List.concat_map
    (fun lr ->
      let () = Pp.printf "@{<bold>inter_line_with_regex@}\n" in
      let () = Pp.printf "@{<bold>line:@} %s\n" (omit_layout_line line1) in
      let () = Pp.printf "@{<bold>lr:@} %s\n" (layout_regex regex) in
      let res = merge_line_with_linear_regex if_reuse line1 lr in
      let () = Pp.printf "@{<bold>res: %i\n" (List.length res) in
      let () =
        List.iteri
          (fun i x ->
            Pp.printf "@{<bold>res[%i]:@} %s\n%s\n" i (omit_layout_line x)
              (layout_line_elems x.elems))
          res
      in
      res)
    lrs

let line_insert_se if_reuse { gprop; elems } (id, se) =
  let rec aux (res, prefix) = function
    | [] -> res
    | LineAct act :: rest ->
        if if_reuse act then
          let () =
            Pp.printf "@{<bold>try merge act(%s) with se@}\n" (layout_act act)
          in
          match merge_act_with_se (gprop, act) se with
          | None ->
              let () = Pp.printf "@{<bold>act merge failed@}\n" in
              aux (res, prefix @ [ LineAct act ]) rest
          | Some phi ->
              let () = Pp.printf "@{<bold>act merge successed@}\n" in
              let gprop = smart_and [ phi; gprop ] in
              let elems = prefix @ [ LineAct act ] @ rest in
              aux
                ( (act_get_id act, { gprop; elems }) :: res,
                  prefix @ [ LineAct act ] )
                rest
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

let _check_eq gprop1 gprop2 =
  if equal_prop Nt.equal_nt gprop1 gprop2 then true
  else not (_check_sat (Not (Iff (gprop1, gprop2))))

let _check_eq_charset c1 c2 =
  let m1 = charset_to_smap c1 in
  let m2 = charset_to_smap c2 in
  let res =
    if StrMap.cardinal m1 == StrMap.cardinal m2 then
      StrMap.for_all
        (fun op (_, phi1) ->
          match StrMap.find_opt m2 op with
          | None -> false
          | Some (_, phi2) -> _check_eq phi1 phi2)
        m1
    else false
  in
  let () =
    Pp.printf "@{<bold>check_eq_charset@} %s ?= %s :: %b\n" (layout_charset c1)
      (layout_charset c2) res
  in
  res

let unify_line line1 line2 =
  let () =
    Pp.printf "@{<bold>unify_line@}\n%s\n%s\n" (omit_layout_line line1)
      (omit_layout_line line2)
  in
  let rec aux elems1 (gprop2, elems2) =
    match (elems1, elems2) with
    | [], [] ->
        let () =
          Pp.printf "@{<bold>unify check@} %s ?= %s\n" (layout_prop line1.gprop)
            (layout_prop gprop2)
        in
        _check_eq line1.gprop gprop2
    | [], _ -> false
    | _, [] -> false
    | LineAct act1 :: elems1', LineAct act2 :: elems2' ->
        if String.equal act1.aop act2.aop then
          let m =
            List.map
              (fun (x, y) -> (x.x, AVar y))
              (_safe_combine [%here] act2.aargs act1.aargs)
          in
          let gprop2 = msubst subst_prop_instance m gprop2 in
          aux elems1' (gprop2, elems2')
        else false
    | LineAct _ :: _, _ -> false
    | LineStarMultiChar c1 :: elems1', LineStarMultiChar c2 :: elems2' ->
        if _check_eq_charset c1 c2 then aux elems1' (gprop2, elems2') else false
    | LineStarMultiChar _ :: _, _ -> false
  in
  aux line1.elems (line2.gprop, line2.elems)

let unify_lines lines =
  let length = List.length lines in
  let res = List.slow_rm_dup unify_line lines in
  let () =
    Pp.printf "@{<bold>unify_lines@} %i -> %i\n" length (List.length res)
  in
  res
