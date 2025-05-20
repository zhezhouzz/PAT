open ParseTree
open Zutils
open Prop
open Zdatatype

let fv_t_p_expr = typed_fv_lit
let substract = List.substract (typed_eq String.equal)
let rm_dup = List.slow_rm_dup (fun x y -> String.equal x.x y.x)

let rec fv_p_stmt (e : 't p_stmt) =
  match e with
  | PMute lit -> fv_t_p_expr lit
  | PAssign { assign_kind; lvalue; rvalue } ->
      let lvalue, rvalue = map2 fv_t_p_expr (lvalue, rvalue) in
      lvalue @ rvalue
  | PIf { condition; tbranch; fbranch } ->
      let condition = fv_t_p_expr condition in
      let tbranch = fv_p_block tbranch in
      let fbranch = match fbranch with None -> [] | Some x -> fv_p_block x in
      condition @ tbranch @ fbranch
  | PForeach { iter; iterable; body; _ } ->
      let elem = [ iter ] in
      let set = fv_t_p_expr iterable in
      let body = fv_p_block body in
      set @ substract body elem
  | PWhile { condition; body } ->
      let map = fv_t_p_expr condition in
      let body = fv_p_block body in
      map @ body
  | PReturn e -> fv_t_p_expr e
  | PPrintf (str, es) -> List.concat_map fv_t_p_expr es
  | PSend { dest; event_name; payload } ->
      let dest, payload = map2 fv_t_p_expr (dest, payload) in
      dest @ payload
  | PRecieve { input; body; _ } ->
      let body = fv_p_block body in
      substract body [ input ]
  | PGoto _ -> []
  | PBreak -> []

and fv_p_block (e : 't p_block) = rm_dup @@ List.concat_map fv_p_stmt e

let fv_p_closure ({ local_vars; block } : 't p_closure) =
  let block = fv_p_block block in
  substract block local_vars

let fv_p_func ({ name; func_label; params; retty; closure } : 't p_func) =
  let closure = fv_p_closure closure in
  substract closure params

let fv_p_state ({ name; state_label; state_body } : 't p_state) =
  let state_body = List.concat_map fv_p_func state_body in
  rm_dup @@ state_body

let fv_p_machine ({ name; local_vars; local_funcs; states } : 't p_machine) =
  let local_funcs_vars = List.map get_p_func_var local_funcs in
  let local_funcs = List.concat_map fv_p_func local_funcs in
  let states = List.concat_map fv_p_state states in
  substract (local_funcs @ states) (local_funcs_vars @ local_vars)

let fv_p_item (item : 't p_item) =
  match item with
  | PEnumDecl _ -> []
  | PTopSimplDecl _ -> []
  | PGlobalProp { prop; _ } -> fv_prop prop
  | PPayload { prop; _ } -> fv_prop prop
  | PPayloadGen { body; _ } -> fv_t_p_expr body
  | PSyn { gen_num; _ } ->
      List.concat_map (fun (_, _, ass) -> fv_t_p_expr ass) gen_num

let fv_t_p_expr_id e = fv_typed_id_to_id fv_t_p_expr e
let fv_p_stmt_id e = fv_typed_id_to_id fv_p_stmt e
let fv_p_block_id e = fv_typed_id_to_id fv_p_block e
let fv_p_closure_id e = fv_typed_id_to_id fv_p_closure e
let fv_p_func_id e = fv_typed_id_to_id fv_p_func e
let fv_p_state_id e = fv_typed_id_to_id fv_p_state e
let fv_p_machine_id e = fv_typed_id_to_id fv_p_machine e
let fv_p_item_id e = fv_typed_id_to_id fv_p_item e
