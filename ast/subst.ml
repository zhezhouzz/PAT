open ParseTree
open Zutils
open Prop

let subst_t_p_expr = typed_subst_lit

let rec subst_p_stmt (_x : string) f (e : 't p_stmt) =
  match e with
  | PMute lit -> PMute (subst_t_p_expr _x f lit)
  | PAssign { assign_kind; lvalue; rvalue } ->
      let lvalue, rvalue = map2 (subst_t_p_expr _x f) (lvalue, rvalue) in
      PAssign { assign_kind; lvalue; rvalue }
  | PIf { condition; tbranch; fbranch } ->
      let condition = subst_t_p_expr _x f condition in
      let tbranch = subst_p_block _x f tbranch in
      let fbranch = fbranch >|= subst_p_block _x f in
      PIf { condition; tbranch; fbranch }
  | PForeach { foreach_kind; iter; iterable; body } ->
      let iterable = subst_t_p_expr _x f iterable in
      let body =
        if String.equal iter.x _x then body else subst_p_block _x f body
      in
      PForeach { foreach_kind; iter; iterable; body }
  | PWhile { condition; body } ->
      let map = subst_t_p_expr _x f condition in
      let body = subst_p_block _x f body in
      PWhile { condition; body }
  | PReturn e -> PReturn (subst_t_p_expr _x f e)
  | PPrintf (str, es) -> PPrintf (str, List.map (subst_t_p_expr _x f) es)
  | PSend { dest; event_name; payload } ->
      let dest, payload = map2 (subst_t_p_expr _x f) (dest, payload) in
      PSend { dest; event_name; payload }
  | PRecieve { input; event_name; body } ->
      let body =
        if String.equal input.x _x then body else subst_p_block _x f body
      in
      PRecieve { input; event_name; body }
  | PGoto loc -> PGoto loc
  | PBreak -> PBreak

and subst_p_block _x f (e : 't p_block) = List.map (subst_p_stmt _x f) e

let subst_p_closure _x f ({ local_vars; block } : 't p_closure) =
  let block = subst_p_block _x f block in
  { local_vars; block }

let subst_p_func _x f ({ name; func_label; params; retty; closure } : 't p_func)
    =
  let closure = subst_p_closure _x f closure in
  { name; func_label; params; retty; closure }

let subst_p_state _x f ({ name; state_label; state_body } : 't p_state) =
  let state_body = List.map (subst_p_func _x f) state_body in
  { name; state_label; state_body }

let subst_p_machine _x f
    ({ name; local_vars; local_funcs; states } : 't p_machine) =
  let local_funcs = List.map (subst_p_func _x f) local_funcs in
  let states = List.map (subst_p_state _x f) states in
  { name; local_vars; local_funcs; states }

let subst_p_item _x f (item : 't p_item) =
  match item with
  | PEnumDecl (name, es) -> PEnumDecl (name, es)
  | PTopSimplDecl { kind; tvar } -> PTopSimplDecl { kind; tvar }
  | PGlobalProp { name; prop } ->
      PGlobalProp { name; prop = subst_prop _x f prop }
  | PPayload { name; self_event; prop } ->
      PPayload { name; self_event; prop = subst_prop _x f prop }
  | PPayloadGen { name; self_event; body } ->
      PPayloadGen { name; self_event; body = subst_t_p_expr _x f body }
  | PSyn { name; gen_num; cnames } ->
      let gen_num =
        List.map
          (fun (x, dest, ass) -> (x, dest, subst_t_p_expr _x f ass))
          gen_num
      in
      PSyn { name; gen_num; cnames }

let subst_t_p_expr_instance x inst e =
  subst_f_to_instance subst_t_p_expr x inst e

let subst_p_stmt_instance x inst e = subst_f_to_instance subst_p_stmt x inst e
let subst_p_block_instance x inst e = subst_f_to_instance subst_p_block x inst e

let subst_p_closure_instance x inst e =
  subst_f_to_instance subst_p_closure x inst e

let subst_p_func_instance x inst e = subst_f_to_instance subst_p_func x inst e
let subst_p_state_instance x inst e = subst_f_to_instance subst_p_state x inst e

let subst_p_machine_instance x inst e =
  subst_f_to_instance subst_p_machine x inst e

let subst_p_item_instance x inst e = subst_f_to_instance subst_p_item x inst e
