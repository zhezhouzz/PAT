let v_name = "v"
let v_ret_name = "vret"

open Zutils
include Prop

let str_eq_to_bv y x = match x with Some x -> String.equal x y | None -> false
(* let vs_names n = List.init n (fun i -> spf "%s%i" "x_" i) *)

let get_record_ty_fds loc ty =
  match ty with Nt.Ty_record { fds; _ } -> fds | _ -> _failatwith loc "die"

(* let rename_qv x = (Rename.unique x.x) #: x.ty *)
let name_in_qvs name l = List.exists (fun x -> String.equal x.x name) l

let get_record_ty_fds_from_opt = function
  | Some ty -> get_record_ty_fds [%here] ty
  | None -> []

let mk_p_abstract_ty name = Nt.Ty_constructor (name, [])
let mk_p_set_ty ty = Nt.Ty_constructor ("set", [ ty ])
let mk_p_seq_ty ty = Nt.Ty_constructor ("seq", [ ty ])
let mk_p_map_ty ty1 ty2 = Nt.Ty_constructor ("map", [ ty1; ty2 ])
let mk_p_event_ty = mk_p_abstract_ty "event"
let mk_p_ref_ty ty = Nt.Ty_constructor ("ref", [ ty ])
let mk_p_record_ty vs = Nt.Ty_record { alias = None; fds = vs }
let mk_p_string_ty = mk_p_abstract_ty "string"
let mk_p_regex_ty = mk_p_abstract_ty "regex"

let is_p_abstact_ty name = function
  | Nt.Ty_constructor (name', []) when String.equal name name' -> true
  | _ -> false

let mk_p_tuple_ty vs =
  Nt.Ty_record
    { alias = None; fds = List.mapi (fun i ty -> (string_of_int i)#:ty) vs }

let mk_p_machine_ty = mk_p_abstract_ty "machine"

let get_p_primitive_construnctor_name = function
  | Nt.Ty_constructor (name, []) -> name
  | _ -> "bad name"

let get_p_record_fds = function
  | Nt.Ty_record { fds; _ } -> fds
  | _ -> _failatwith [%here] "die"

let mk_p_named_record_ty name vs =
  Nt.Ty_record
    {
      alias = Some name;
      fds = List.mapi (fun i ty -> (string_of_int i)#:ty) vs;
    }

let is_empty_record_ty = function
  | Nt.Ty_record { fds = []; _ } -> true
  | _ -> false

let is_record_ty = function Nt.Ty_record _ -> true | _ -> false

(* Filesystem *)

open Zdatatype

let get_parent_path path =
  let path = String.split_on_char '/' path in
  let path = List.filter (fun x -> not (String.equal x "")) path in
  (* let () =
    Pp.printf "path list: %s\n"
      (List.split_by_comma (fun x -> spf "|%s|" x) path)
  in *)
  match List.last_destruct_opt path with
  | None -> None
  | Some (parent, _) -> Some ("/" ^ String.concat "/" parent)
