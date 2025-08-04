open Ast
open Zdatatype

(* let layout_states f s = *)
(*   List.split_by_comma f @@ List.of_seq @@ StateSet.to_seq s *)

let layout_qv x = spf "(%s: %s)" x.x (Nt.layout x.ty)
let layout_qvs = List.split_by " " layout_qv
let p_prim_types = [ "int"; "bool"; "machine"; "any"; "string" ]

let rec is_p_prim_type = function
  | Nt.Ty_record { fds; _ } -> List.for_all (fun x -> is_p_prim_type x.ty) fds
  | Nt.Ty_tuple l -> List.for_all is_p_prim_type l
  | Nt.Ty_constructor (name, [])
    when List.exists (String.equal name) p_prim_types ->
      true
  | Nt.Ty_constructor (name, [ nt ]) ->
      (String.equal "set" name || String.equal "req" name) && is_p_prim_type nt
  | Nt.Ty_constructor (name, [ nt1; nt2 ]) ->
      String.equal "map" name && is_p_prim_type nt1 && is_p_prim_type nt2
  | _ -> false

let get_absty nt =
  let rec aux = function
    | Nt.Ty_record { fds; _ } -> List.concat_map (fun x -> aux x.ty) fds
    | Nt.Ty_tuple l -> List.concat_map aux l
    | Nt.Ty_constructor (name, [])
      when List.exists (String.equal name) p_prim_types ->
        []
    | Nt.Ty_constructor (name, []) -> [ name ]
    | Nt.Ty_constructor (_, [ nt ]) -> aux nt
    | Nt.Ty_constructor (_, [ nt1; nt2 ]) -> aux nt1 @ aux nt2
    | _ -> _die_with [%here] (Nt.layout nt)
  in
  List.slow_rm_dup String.equal (aux nt)

let rec layout_stlcTy = function
  | StlcInt -> "int"
  | StlcArrow (ty1, ty2) ->
      let s1 =
        match ty1 with
        | StlcArrow _ -> "(" ^ layout_stlcTy ty1 ^ ")"
        | _ -> layout_stlcTy ty1
      in
      let s2 =
        match ty2 with
        | StlcArrow _ -> "(" ^ layout_stlcTy ty2 ^ ")"
        | _ -> layout_stlcTy ty2
      in
      spf "%s -> %s" s1 s2

let rec layout_stlcTerm = function
  | StlcVar x -> spf "[%d]" x
  | StlcConst n -> string_of_int n
  | StlcAbs { absTy; absBody } ->
      spf "\\(%s).%s" (layout_stlcTy absTy) (layout_stlcTerm absBody)
  | StlcApp { appFun; appArg } ->
      spf "(%s %s)" (layout_stlcTerm appFun) (layout_stlcTerm appArg)

let layout_value = function
  | VVar qv -> layout_qv qv
  | VConst c -> layout_constant c
  | VCStlcTy ty -> layout_stlcTy ty

let is_gen env op = is_generative @@ _get_force [%here] env.msgkind_ctx op
let is_obs env op = is_observable @@ _get_force [%here] env.msgkind_ctx op

let destruct_cty_var x =
  let x' = x.x#:x.ty.nty in
  let phi = subst_prop_instance default_v (AVar x') x.ty.phi in
  (x', phi)
