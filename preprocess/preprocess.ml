(* include Normalization *)
open Language
open Zutils
include Basic_typing

let _ctxs = ref None
let _log = Myconfig._log_preprocess
let predefined_files = [ "basic_typing.ml" ]
(* let predefined_files = [] *)

let load_ctxs () =
  match !_ctxs with
  | Some ctxs -> ctxs
  | None ->
      let prim_path = Myconfig.get_prim_path () in
      let items =
        List.concat_map
          (fun file -> parse_plang (spf "%s/%s" prim_path.predefined_path file))
          predefined_files
      in
      let decls, items = Type_alias.item_inline [] items in
      let bctx, _ = p_items_type_check mk_basic_typing_ctx items in
      let res = (decls, bctx) in
      _ctxs := Some res;
      res

let load_bctx () =
  let _, bctx = load_ctxs () in
  bctx

let load_alias () =
  let alias, _ = load_ctxs () in
  alias

open Zdatatype

let get_visible items =
  List.slow_rm_dup String.equal
  @@ List.concat_map (function PVisible es -> es | _ -> []) items

let filter_events items =
  let es = get_visible items in
  match es with
  | [] -> items
  | _ ->
      List.filter_map
        (fun item ->
          match item with
          | PVisible _ -> None
          | PTopSimplDecl { kind = TopEvent; tvar } ->
              if List.exists (String.equal tvar.x) es then Some item else None
          | _ -> Some item)
        items

let preproress source_files =
  let items = List.concat_map parse_plang source_files in
  (* let () = Pp.printf "@{<bold>result:@}\n%s\n" (layout_p_items items) in *)
  let items = filter_events items in
  let _, items = Type_alias.item_inline (load_alias ()) items in
  (* let () = Pp.printf "@{<bold>result:@}\n%s\n" (layout_p_items items) in *)
  (* let () = _die [%here] in *)
  (* let () = _die [%here] in *)
  let bctx, items = p_items_type_check (load_bctx ()) items in
  (bctx, items)
