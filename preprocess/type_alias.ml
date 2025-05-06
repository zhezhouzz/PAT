open Language
open Zutils
open Zdatatype

let _log = Myconfig._log "inline"

type constructor_type = string list * Nt.nt

let layout_constructor_type x =
  spf "%s (%s) = %s" x.x (StrList.to_string (fst x.ty)) (Nt.layout (snd x.ty))

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
  let rec f (decls, inlined) items =
    match items with
    | [] -> (decls, inlined)
    | PTopSimplDecl { kind = TopType; tvar } :: items ->
        let ty =
          match tvar.ty with
          | Nt.Ty_record { fds; _ } -> Nt.mk_record (Some tvar.x) fds
          | _ -> tvar.ty
        in
        let decl = tvar.x#:ty in
        let items = List.map (map_p_item (inline decl)) items in
        let decls = decls @ [ decl ] in
        f
          ( decls @ [ decl ],
            inlined @ [ PTopSimplDecl { kind = TopType; tvar = decl } ] )
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
