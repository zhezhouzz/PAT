open Language
open Common
open Zdatatype
open Optimize
open Gamma

let raw_regex_to_trace = function Seq l -> l | _ -> _die [%here]
let se_to_raw_regex se = MultiChar (SFA.CharSet.singleton se)

let raw_regex_to_cs r =
  let rec aux r =
    match r with
    | MultiChar cs -> Some cs
    | Comple (cs, r) ->
        let* cs' = aux r in
        Some (Plan.comple_cs cs cs')
    | Inters (r1, r2) ->
        let* cs1 = aux r1 in
        let* cs2 = aux r2 in
        Some (Plan.inter_cs cs1 cs2)
    | _ -> None
  in
  aux r

let raw_regex_to_plan_elem r =
  let open SFA in
  let r = simp_fvec_raw_regex r in
  match r with
  | MultiChar cs ->
      let se = charset_to_se [%here] cs in
      PlanSe se
  | Star r -> (
      match raw_regex_to_cs r with
      | Some cs -> PlanStarInv cs
      | None ->
          let () = Printf.printf "Not a star:\n %s\n" (layout_regex r) in
          _die [%here])
  | Seq _ | Empty | Eps | Alt _ | Inters _ | Comple _ -> _die [%here]

let raw_regex_to_plan =
  let rec aux r =
    match r with
    | Empty -> _die [%here]
    | Eps -> []
    | MultiChar _ | Star _ -> [ raw_regex_to_plan_elem r ]
    | Alt _ | Inters _ | Comple _ ->
        let () = Printf.printf "%s\n" (SFA.layout_regex r) in
        _die [%here]
    | Seq l -> List.concat_map aux l
  in
  aux

let normalize_desym_regex2 (rawreg : SFA.CharSet.t regex) =
  let open SFA in
  (* let () = Pp.printf "@{<bold>start@}: %s\n" (layout_raw_regex rawreg) in *)
  let rec aux rawreg =
    match rawreg with
    | Empty | Eps | MultiChar _ -> rawreg
    | Alt (r1, r2) -> alt (aux r1) (aux r2)
    | Comple (cs1, Comple (cs2, r)) ->
        let () =
          Pp.printf "@{<bold>double comp@}: %s\n" (layout_regex rawreg)
        in
        let cs1 = CharSet.filter (fun c -> not (CharSet.mem c cs2)) cs1 in
        if CharSet.is_empty cs1 then aux r else Alt (Star (MultiChar cs1), aux r)
    | Comple (cs, r) -> (
        match aux r with
        | Star (MultiChar cs') ->
            let () =
              Pp.printf "@{<bold>opt comple1@}: %s\n" (layout_regex rawreg)
            in
            let cs'' = CharSet.filter (fun c -> not (CharSet.mem c cs')) cs in
            Star (MultiChar cs'')
        | _ as r ->
            let () =
              Pp.printf "@{<bold>opt comple fail@}: %s\n" (layout_regex r)
            in
            rawreg)
    | Inters _ ->
        let () = Pp.printf "@{<bold>opt inters@}: %s\n" (layout_regex rawreg) in
        rawreg
    | Seq l -> seq (List.map aux l)
    | Star r -> Star r
  in
  aux rawreg

let normalize_gamma env { bvs; bprop } r =
  let ftab = Rawdesym.mk_global_ftab env.tyctx (bvs, bprop, r) in
  let () = _assert [%here] "assume start from true" (is_true bprop) in
  let fvecs =
    List.of_seq @@ Rawdesym.BlistSet.to_seq @@ Rawdesym.mk_fvec_from_ftab ftab
  in
  let _, lit2int = Rawdesym.mk_li_map ftab in
  let props = List.map (fun l -> Rawdesym.blist_to_prop l lit2int) fvecs in
  let props =
    List.filter
      (fun p -> Prover.check_sat_bool (None, smart_exists bvs p))
      props
  in
  List.map (fun bprop -> { bvs; bprop }) props

let normalize_goal_aux _ (_, reg) =
  let () =
    Pp.printf "\n@{<bold>Before Normalize:@}\n%s\n" (SFA.layout_regex reg)
  in
  let _, reg =
    _die_with [%here] "unimp"
    (* Rawdesym.desymbolic_symbolic_rewregex env.tyctx
      env.event_tyctx (gamma.bprop, reg) *)
  in
  let () =
    Pp.printf "\n@{<bold>After Desym:@}\n%s\n" (DesymFA.layout_regex reg)
  in
  let reg = Rawdesym.normalize_desym_regex reg in
  let open DesymFA in
  let () = Printf.printf "reg: %s\n" (layout_regex reg) in
  if emptiness reg then [] else _die_with [%here] "unimp"
(* let unf = raw_regex_to_union_normal_form unify_charset_by_op reg in *)
(* let unf = List.map (List.map (Rawdesym.resym_regex desym_ctx)) reg in
    let unf =
      List.map (fun l -> (gamma, List.map raw_regex_to_plan_elem l)) unf
    in
    unf *)

let normalize_goal env (gamma, reg) =
  let gammas = normalize_gamma env gamma reg in
  let res =
    List.concat_map (fun gamma -> normalize_goal_aux env (gamma, reg)) gammas
  in
  (* let () = Pp.printf "@{<bold>Goals:\n@}" in *)
  (* let () = List.iter simp_print_mid_judgement res in *)
  (* let () = _die [%here] in *)
  res
