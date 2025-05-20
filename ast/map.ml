open ParseTree
open Zutils
open Prop

let map_t_p_expr = typed_map_lit

let rec map_p_stmt (f : 't -> 's) (e : 't p_stmt) =
  match e with
  | PMute lit -> PMute (map_t_p_expr f lit)
  | PAssign { assign_kind; lvalue; rvalue } ->
      let lvalue, rvalue = map2 (map_t_p_expr f) (lvalue, rvalue) in
      PAssign { assign_kind; lvalue; rvalue }
  | PIf { condition; tbranch; fbranch } ->
      let condition = map_t_p_expr f condition in
      let tbranch = map_p_block f tbranch in
      let fbranch = fbranch >|= map_p_block f in
      PIf { condition; tbranch; fbranch }
  | PForeach { foreach_kind; iter; iterable; body } ->
      let iter = iter#=>f in
      let iterable = map_t_p_expr f iterable in
      let body = map_p_block f body in
      PForeach { foreach_kind; iter; iterable; body }
  | PWhile { condition; body } ->
      let condition = map_t_p_expr f condition in
      let body = map_p_block f body in
      PWhile { condition; body }
  | PReturn e -> PReturn (map_t_p_expr f e)
  | PPrintf (str, es) -> PPrintf (str, List.map (map_t_p_expr f) es)
  | PSend { dest; event_name; payload } ->
      let dest, payload = map2 (map_t_p_expr f) (dest, payload) in
      PSend { dest; event_name; payload }
  | PRecieve { input; event_name; body } ->
      let input = input#=>f in
      let body = map_p_block f body in
      PRecieve { input; event_name; body }
  | PGoto loc -> PGoto loc
  | PBreak -> PBreak

and map_p_block (f : 't -> 's) (e : 't p_block) = List.map (map_p_stmt f) e

let map_p_closure (f : 't -> 's) ({ local_vars; block } : 't p_closure) =
  let local_vars = List.map (fun x -> x#=>f) local_vars in
  let block = map_p_block f block in
  { local_vars; block }

let map_p_func (f : 't -> 's)
    ({ name; func_label; params; retty; closure } : 't p_func) =
  let params = List.map (fun x -> x#=>f) params in
  let retty = f retty in
  let closure = map_p_closure f closure in
  { name; func_label; params; retty; closure }

let map_p_state (f : 't -> 's) ({ name; state_label; state_body } : 't p_state)
    =
  let state_body = List.map (map_p_func f) state_body in
  { name; state_label; state_body }

let map_p_machine (f : 't -> 's)
    ({ name; local_vars; local_funcs; states } : 't p_machine) =
  let local_vars = List.map (fun x -> x#=>f) local_vars in
  let local_funcs = List.map (map_p_func f) local_funcs in
  let states = List.map (map_p_state f) states in
  { name; local_vars; local_funcs; states }

let map_p_item (f : 't -> 's) (item : 't p_item) =
  match item with
  | PEnumDecl (name, es) -> PEnumDecl (name, es)
  | PTopSimplDecl { kind; tvar } -> PTopSimplDecl { kind; tvar = tvar#=>f }
  | PGlobalProp { name; prop } -> PGlobalProp { name; prop = map_prop f prop }
  | PPayload { name; self_event; prop } ->
      PPayload { name; self_event; prop = map_prop f prop }
  | PPayloadGen { name; self_event; body } ->
      PPayloadGen { name; self_event; body = map_t_p_expr f body }
  | PSyn { name; gen_num; cnames } ->
      let gen_num =
        List.map (fun (x, dest, ass) -> (x, dest, map_t_p_expr f ass)) gen_num
      in
      PSyn { name; gen_num; cnames }

let map_p_item_on_prop (f : 't prop -> 't prop) (item : 't p_item) =
  match item with
  | PEnumDecl (name, es) -> PEnumDecl (name, es)
  | PTopSimplDecl { kind; tvar } -> PTopSimplDecl { kind; tvar }
  | PGlobalProp { name; prop } -> PGlobalProp { name; prop = f prop }
  | PPayload { name; self_event; prop } ->
      PPayload { name; self_event; prop = f prop }
  | PPayloadGen { name; self_event; body } ->
      PPayloadGen { name; self_event; body }
  | PSyn { name; gen_num; cnames } -> PSyn { name; gen_num; cnames }
