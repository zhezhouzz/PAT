open Language
open Zutils
open Zdatatype

let _log = Myconfig._log "inline"

type constructor_type = string list * Nt.nt

let layout_constructor_type x = spf "%s = %s" x.x (Nt.layout x.ty)
let layout_alias l = split_by "\n" layout_constructor_type l

let inline_record { x; ty = alias_ty } ty =
  let f ts = if List.length ts != 0 then _die [%here] else alias_ty in
  let ty = Nt.subst_constructor_nt (x, f) ty in
  match alias_ty with
  | Nt.Ty_record { alias; fds } ->
      let core = (List.map _get_x fds, alias) in
      Nt.subst_alias_in_record_nt core ty
  | _ -> ty

(* let self_inline l = *)
(*   let rec aux l = *)
(*     match l with *)
(*     | [] -> [] *)
(*     | decl :: l -> *)
(*         let l = *)
(*           List.map *)
(*             (fun { x; ty = record_ty } -> *)
(*               { x; ty = inline_record decl record_ty }) *)
(*             l *)
(*         in *)
(*         decl :: aux l *)
(*   in *)
(*   aux l *)

(* let item_mk_type_alias_ctx items = *)
(*   let f e = *)
(*     match e with *)
(*     | PTopSimplDecl { kind = TopType; tvar } -> *)
(*         let ty = *)
(*           match tvar.ty with *)
(*           | Nt.Ty_record { fds; _ } -> Nt.mk_record (Some tvar.x) fds *)
(*           | _ -> tvar.ty *)
(*         in *)
(*         [ tvar.x#:ty ] *)
(*     | _ -> [] *)
(*   in *)
(*   let l = List.concat_map f items in *)
(*   self_inline l *)

let item_inline decls items =
  let inline decl nt =
    let res = inline_record decl nt in
    let () =
      _log @@ fun () ->
      Printf.printf "decl %s = %s\n" decl.x (Nt.layout decl.ty);
      Printf.printf "inline %s ==> %s\n" (Nt.layout nt) (Nt.layout res)
    in
    res
  in
  let items =
    List.fold_left
      (fun items decl -> List.map (map_p_item (inline decl)) items)
      items decls
  in
  let if_do_inline = function TopType | TopEvent -> true | TopVar -> false in
  let rec f (decls, inlined) items =
    match items with
    | [] -> (decls, inlined)
    | PTopSimplDecl { kind; tvar } :: items when if_do_inline kind ->
        let ty =
          match tvar.ty with
          | Nt.Ty_record { fds; _ } -> Nt.mk_record (Some tvar.x) fds
          | _ -> tvar.ty
        in
        let decl = tvar.x#:ty in
        let items = List.map (map_p_item (inline decl)) items in
        let decls = decls @ [ decl ] in
        f
          (decls @ [ decl ], inlined @ [ PTopSimplDecl { kind; tvar = decl } ])
          items
    | item :: items -> f (decls, inlined @ [ item ]) items
  in
  f ([], []) items

(* let%test "inline_alias" = *)
(*   let () = *)
(*     Myconfig.meta_config_path := *)
(*       "/Users/zhezzhou/workspace/CoverageType/meta-config.json" *)
(*   in *)
(*   let test_file = *)
(*     "/Users/zhezzhou/workspace/CoverageType/data/inline_test/alias.ml" *)
(*   in *)
(*   let items = *)
(*     ocaml_structure_to_items *)
(*     @@ OcamlParser.Oparse.parse_imp_from_file ~sourcefile:test_file *)
(*   in *)
(*   let () = Pp.printf "@{<bold>Parse:@}\n%s\n" (layout_structure items) in *)
(*   let decls, items = item_inline items in *)
(*   let () = Pp.printf "@{<bold>Result:@}\n%s\n" (layout_structure items) in *)
(*   false *)

(* let rec map_prop_qv (f : 't -> 's) (prop_e : 't prop) = *)
(*   match prop_e with *)
(*   | Lit _t__tlittyped0 -> Lit _t__tlittyped0 *)
(*   | Implies (_tprop0, _tprop1) -> *)
(*       Implies (map_prop_qv f _tprop0, map_prop_qv f _tprop1) *)
(*   | Ite (_tprop0, _tprop1, _tprop2) -> *)
(*       Ite (map_prop_qv f _tprop0, map_prop_qv f _tprop1, map_prop_qv f _tprop2) *)
(*   | Not _tprop0 -> Not (map_prop_qv f _tprop0) *)
(*   | And _tproplist0 -> And (List.map (map_prop_qv f) _tproplist0) *)
(*   | Or _tproplist0 -> Or (List.map (map_prop_qv f) _tproplist0) *)
(*   | Iff (_tprop0, _tprop1) -> Iff (map_prop_qv f _tprop0, map_prop_qv f _tprop1) *)
(*   | Forall { qv; body } -> Forall { qv = qv#=>f; body = map_prop_qv f body } *)
(*   | Exists { qv; body } -> Exists { qv = qv#=>f; body = map_prop_qv f body } *)

(* let inline_quantified_event_into_event_type ectx prop = *)
(*   map_prop_qv *)
(*     (fun ty -> *)
(*       match ty with *)
(*       | Nt.Ty_constructor (name, []) -> ( *)
(*           match Typectx.get_opt ectx name with None -> ty | Some ty' -> ty') *)
(*       | _ -> ty) *)
(*     prop *)

(* let inline_quantified_event_in_item ectx item = *)
(*   map_p_item_on_prop (inline_quantified_event_into_event_type ectx) item *)

(* let inline_quantified_event_in_items items = *)
(*   let ectx = *)
(*     List.fold_left *)
(*       (fun ctx -> function *)
(*         | PTopSimplDecl { kind = TopEvent; tvar } -> add_to_right ctx tvar *)
(*         | _ -> ctx) *)
(*       Typectx.emp items *)
(*   in *)
(*   List.map (inline_quantified_event_in_item ectx) items *)
