open Zutils
open OcamlParser
open Parsetree
open Zdatatype
open Ast
open AutomataLibrary

let layout_cty { nty; phi } = spf "v:%s | %s" (Nt.layout nty) (layout_prop phi)

let vars_phi_of_expr expr =
  let rec aux expr =
    match expr.pexp_desc with
    | Pexp_constraint (e', ct) ->
        let v = get_self ct in
        let vs, phi = aux e' in
        (v :: vs, phi)
    | _ -> ([], prop_of_expr expr)
  in
  let vs, prop = aux expr in
  (List.rev vs, prop)

let cty_of_expr expr =
  match vars_phi_of_expr expr with
  | [ { x; ty } ], phi when String.equal x default_v -> { nty = ty; phi }
  | _ -> _die_with [%here] (Pprintast.string_of_expression expr)

let rec layout_pat f = function
  | RtyBase cty ->
      if is_true cty.phi then Nt.layout cty.nty else spf "{%s}" (layout_cty cty)
  | RtyHAF { history; adding; future } ->
      spf "[%s][%s][%s]" (f history) (f adding) (f future)
  | RtyHAParallel { history; adding_se; parallel } ->
      spf "[%s][%s][| %s |]" (f history) (layout_sevent adding_se)
        (List.split_by " " layout_sevent parallel)
  | RtyArr { arg; argcty; retrty } ->
      let str =
        if is_true argcty.phi then Nt.layout argcty.nty
        else spf "{%s}" (layout_cty argcty)
      in
      spf "(%s:%s) → %s" arg str (layout_pat f retrty)
  | RtyGArr { arg; argnty; retrty } ->
      spf "(%s:%s) ⇢ %s" arg (Nt.layout argnty) (layout_pat f retrty)
  | RtyInter (pat1, pat2) ->
      spf "%s ⊓ %s" (layout_pat f pat1) (layout_pat f pat2)

let parallel_to_regex ses =
  let al = StarA AnyA in
  let ses_to_regex l =
    let res = List.concat_map (fun se -> [ MultiAtomic [ se ]; al ]) l in
    al :: res
  in
  let multi_lorA l =
    let l = List.map (fun x -> SeqA (ses_to_regex x)) l in
    match l with
    | [] -> _die_with [%here] "never"
    | x :: l -> List.fold_left (fun acc y -> LorA (acc, y)) x l
  in
  match ses with
  | [] -> StarA AnyA
  | [ se ] -> SeqA (ses_to_regex [ se ])
  | [ se1; se2 ] -> multi_lorA [ [ se1; se2 ]; [ se2; se1 ] ]
  | [ se1; se2; se3 ] ->
      multi_lorA
        [
          [ se1; se2; se3 ];
          [ se1; se3; se2 ];
          [ se2; se1; se3 ];
          [ se2; se3; se1 ];
          [ se3; se1; se2 ];
          [ se3; se2; se1 ];
        ]
  | _ -> _die_with [%here] "never"

let rec pat_of_expr expr =
  match expr.pexp_desc with
  | Pexp_constraint _ -> RtyBase (cty_of_expr expr)
  | Pexp_fun (_, patexpr, pattern, body) -> (
      let retrty = pat_of_expr body in
      match patexpr with
      | None ->
          let x =
            match pattern.ppat_desc with
            | Ppat_constraint (name, ct) ->
                (id_of_pattern name)#:(Nt.core_type_to_t ct)
            | _ -> _die_with [%here] "wrong format"
          in
          RtyGArr { argnty = x.ty; arg = x.x; retrty }
      | Some patexpr ->
          let arg = id_of_pattern pattern in
          let argcty = cty_of_expr patexpr in
          RtyArr { argcty; arg; retrty })
  | Pexp_let (_, [ vb ], body) ->
      let retrty = pat_of_expr body in
      let arg = id_of_pattern vb.pvb_pat in
      let argcty = cty_of_expr vb.pvb_expr in
      RtyArr { argcty; arg; retrty }
  | Pexp_tuple [ h; a; f ] -> (
      match f.pexp_desc with
      | Pexp_array es ->
          let history = rich_symbolic_regex_of_expr h in
          let adding_se = sevent_of_expr a in
          let parallel = List.map sevent_of_expr es in
          RtyHAF
            {
              history;
              adding = MultiAtomic [ adding_se ];
              future = parallel_to_regex parallel;
            }
      | _ ->
          let history, adding, future =
            map3 rich_symbolic_regex_of_expr (h, a, f)
          in
          RtyHAF { history; adding; future })
  | Pexp_array ls -> mk_inter_type @@ List.map pat_of_expr ls
  | _ ->
      _die_with [%here]
        (spf "wrong refinement type: %s" (Pprintast.string_of_expression expr))

(* let rec locally_rename_haft ctx = function
  | RtyBase cty -> RtyBase cty
  | RtyHAF { history; adding; future } ->
      let history, adding, future =
        map3 (locally_rename (ctx_to_list ctx)) (history, adding, future)
      in
      RtyHAF { history; adding; future }
  | RtyHAParallel { history; adding_se; parallel } ->
      let history = (locally_rename (ctx_to_list ctx)) history in
      let adding_se = (locally_rename_se (ctx_to_list ctx)) adding_se in
      let parallel = List.map (locally_rename_se (ctx_to_list ctx)) parallel in
      RtyHAParallel { history; adding_se; parallel }
  | RtyArr { arg; argcty; retrty } ->
      RtyArr { arg; argcty; retrty = locally_rename_haft ctx retrty }
  | RtyGArr { arg; argnt; retrty } ->
      RtyGArr { arg; argnt; retrty = locally_rename_haft ctx retrty }
  | RtyInter (haft1, haft2) ->
      RtyInter (locally_rename_haft ctx haft1, locally_rename_haft ctx haft2) *)
