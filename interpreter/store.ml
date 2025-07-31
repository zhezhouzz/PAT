open Language
open Zdatatype

type t = constant StrMap.t

let const_to_bool loc = function B b -> b | _ -> _die_with loc "never"

let eval_app_op op cs =
  match (op.x, cs) with
  | "==", [ a; b ] -> B (equal_constant a b)
  | ">", [ I a; I b ] -> B (a > b)
  | _ -> _die_with [%here] "unimp"

let eval_qv store x =
  match StrMap.find_opt store x.x with Some c -> c | None -> _die [%here]

let eval_value store = function VVar x -> eval_qv store x | VConst c -> c
let meval_value store values = List.map (fun v -> eval_value store v.x) values

let eval_lit store lit =
  let rec aux lit =
    match lit with
    | AC c -> AC c
    | AAppOp (op, args) ->
        AC (eval_app_op op @@ List.map typed_aux_to_const args)
    | ATu l -> ATu (List.map typed_aux l)
    | AProj (r, i) -> (
        match aux r.x with
        | ATu l -> (List.nth l i).x
        | _ -> _die_with [%here] "never")
    | ARecord fds -> ARecord (List.map (fun (x, lit) -> (x, typed_aux lit)) fds)
    | AField (r, i) -> (
        match aux r.x with
        | ARecord fds -> (
            match List.find_opt (fun (x, _) -> String.equal x i) fds with
            | Some (_, lit) -> lit.x
            | None -> _die_with [%here] "never")
        | _ -> _die_with [%here] "never")
    | AVar x -> AC (eval_qv store x)
  and typed_aux_to_const lit =
    match (typed_aux lit).x with AC c -> c | _ -> _die_with [%here] "never"
  and typed_aux lit = (aux lit.x)#:lit.ty in
  typed_aux_to_const lit

let eval_prop store prop =
  let rec aux = function
    | Lit lit -> const_to_bool [%here] @@ eval_lit store lit
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
  List.split_by " ;" (fun (x, c) -> spf "%s --> %s" x (layout_constant c))
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
