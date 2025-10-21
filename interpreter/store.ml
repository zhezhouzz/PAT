open Language
open Zdatatype

type t = value StrMap.t

let const_to_bool loc = function B b -> b | _ -> _die_with loc "never"

let value_to_const loc = function
  | VConst c -> c
  | _ -> _die_with loc "never: var_to_const"

let value_to_bool loc v = value_to_const loc v |> const_to_bool loc

let eval_app_op op cs =
  let cs = match cs with [ VTu vs ] -> vs | _ -> cs in
  let res =
    match (op.x, cs) with
    | "==", [ a; b ] -> B (equal_value a b)
    | "!=", [ a; b ] -> B (not (equal_value a b))
    | ">", [ VConst (I a); VConst (I b) ] -> B (a > b)
    | "parent", [ VConst (S a) ] -> (
        match get_parent_path a with None -> S "/" | Some p -> S p)
    | "is_root", [ VConst (S a) ] ->
        if String.equal a "/" then B true else B false
    | "<", [ VConst (I a); VConst (I b) ] -> B (a < b)
    | ">=", [ VConst (I a); VConst (I b) ] -> B (a >= b)
    | "<=", [ VConst (I a); VConst (I b) ] -> B (a <= b)
    | "+", [ VConst (I a); VConst (I b) ] -> I (a + b)
    | "-", [ VConst (I a); VConst (I b) ] -> I (a - b)
    | "*", [ VConst (I a); VConst (I b) ] -> I (a * b)
    | "/", [ VConst (I a); VConst (I b) ] -> I (a / b)
    | "is_int_ty", [ VCStlcTy ty ] -> (
        match ty with StlcInt -> B true | _ -> B false)
    | "fstTy", [ VCStlcTy ty1; VCStlcTy ty2 ] -> (
        match ty1 with
        | StlcArrow (ty11, _) -> B (equal_stlcTy ty11 ty2)
        | _ -> B false)
    | "sndTy", [ VCStlcTy ty1; VCStlcTy ty2 ] -> (
        match ty1 with
        | StlcArrow (_, ty12) -> B (equal_stlcTy ty12 ty2)
        | _ -> B false)
    | _ ->
        let () =
          Pp.printf "@{<red>%s(%s)@} --> ?\n" op.x
            (List.split_by_comma layout_value cs)
        in
        _die_with [%here] "unimp"
  in
  (* let () =
    Pp.printf "@{<red>%s(%s)@} --> %s\n" op.x
      (List.split_by_comma layout_constant cs)
      (layout_constant res)
  in *)
  (* let () = if String.equal op.x "parent" then _die_with [%here] "unimp" in *)
  VConst res

let eval_app_op op vs = eval_app_op op vs

let eval_qv store x =
  match StrMap.find_opt store x.x with Some c -> c | None -> _die [%here]

let rec eval_value store = function
  | VVar x -> eval_qv store x
  | VConst c -> VConst c
  | VCStlcTy ty -> VCStlcTy ty
  | VCIntList xs -> VCIntList xs
  | VTu vs -> VTu (List.map (eval_value store) vs)
  | VProj (v, i) -> (
      match eval_value store v with
      | VTu vs -> List.nth vs i
      | _ -> _die_with [%here] "never")
  | VField (v, s) -> (
      match eval_value store v with
      | VRecord fds -> (
          match List.find_opt (fun (x, _) -> String.equal x s) fds with
          | Some (_, v) -> v
          | None -> _die_with [%here] "never")
      | _ -> _die_with [%here] "never")
  | VRecord fds ->
      VRecord (List.map (fun (s, v) -> (s, eval_value store v)) fds)

let meval_value store values = List.map (fun v -> eval_value store v.x) values

let eval_lit store lit : constant =
  let rec aux lit : value =
    match lit with
    | AC c -> VConst c
    | AAppOp (op, args) -> eval_app_op op (List.map (fun x -> aux x.x) args)
    | ATu l -> VTu (List.map (fun x -> aux x.x) l)
    | AProj (r, i) -> (
        match aux r.x with
        | VTu vs -> List.nth vs i
        | _ -> _die_with [%here] "never")
    | ARecord fds -> VRecord (List.map (fun (x, lit) -> (x, aux lit.x)) fds)
    | AField (r, i) -> (
        match aux r.x with
        | VRecord fds -> (
            match List.find_opt (fun (x, _) -> String.equal x i) fds with
            | Some (_, v) -> v
            | None -> _die_with [%here] "never")
        | _ -> _die_with [%here] "never")
    | AVar x ->
        (* let () = Pp.printf "@{<bold>eval_qv:@} %s\n" (layout_qv x) in *)
        let c = eval_qv store x in
        (* let () = Pp.printf "@{<bold>eval_qv result:@} %s\n" (layout_value c) in *)
        c
  in
  match aux lit with VConst c -> c | _ -> _die_with [%here] "never"

let eval_prop store prop =
  let rec aux = function
    | Lit lit -> const_to_bool [%here] @@ eval_lit store lit.x
    | Implies (p1, p2) -> if aux p1 then aux p2 else true
    | And ps -> List.fold_left (fun res p -> res && aux p) true ps
    | Or ps -> List.fold_left (fun res p -> res || aux p) false ps
    | Not p -> not (aux p)
    | Iff (p1, p2) -> aux p1 == aux p2
    | Ite (p1, p2, p3) -> if aux p1 then aux p2 else aux p3
    | Forall _ | Exists _ -> _die_with [%here] "never"
  in
  aux prop

let layout store =
  List.split_by " ;" (fun (x, c) -> spf "%s --> %s" x (layout_value c))
  @@ StrMap.to_kv_list store

let curStore : t ref = ref StrMap.empty

let fadd (vs, cs) store =
  StrMap.add_seq
    (List.to_seq
    @@ List.map (fun (x, c) -> (x.x, c))
    @@ _safe_combine [%here] vs cs)
    store

let add (vs, cs) = curStore := fadd (vs, cs) !curStore
let add_list s = curStore := StrMap.add_seq (List.to_seq s) !curStore
let get () = !curStore
let set store = curStore := store
let init () = curStore := StrMap.empty
