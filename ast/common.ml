open Zutils
open Prop

(* let v_name = "v" *)
(* let v_ret_name = "vret" *)
(* let str_eq_to_bv y x = match x with Some x -> String.equal x y | None -> false *)

(* let _get_record_ty_fields loc ty = *)
(*   match ty with Nt.Ty_record l -> l | _ -> _failatwith loc "die" *)

let name_in_qvs name l = List.exists (fun x -> String.equal x.x name) l
let mk_pty name = Nt.Ty_constructor (name, [])
let p_machine_ty = mk_pty "machine"
let p_event_ty = mk_pty "event"
let p_string_ty = mk_pty "string"
let p_regex_ty = mk_pty "regex"
let mk_p_set_ty ty = Nt.Ty_constructor ("set", [ ty ])
let mk_p_seq_ty ty = Nt.Ty_constructor ("seq", [ ty ])
let mk_p_map_ty ty1 ty2 = Nt.Ty_constructor ("map", [ ty1; ty2 ])

let is_p_abstact_ty name = function
  | Nt.Ty_constructor (name', []) when String.equal name name' -> true
  | _ -> false

let is_empty_record_ty ty =
  match Nt.get_record_types ty with [] -> true | _ -> false
