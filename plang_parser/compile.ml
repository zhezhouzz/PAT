open Ast
open Zdatatype

let rec layout_pnt t =
  let open Nt in
  let get_var_id x =
    (* Printf.printf "%s\n" x; *)
    String.sub x 1 (String.length x - 1)
  in
  let rec aux = function
    | Ty_constructor (name, [ ty ]) when String.equal name "set" ->
        spf "set[%s]" (aux ty)
    | Ty_constructor (name, [ ty ]) when String.equal name "seq" ->
        spf "seq[%s]" (aux ty)
    | Ty_constructor (name, [ ty1; ty2 ]) when String.equal name "map" ->
        spf "map[%s, %s]" (aux ty1) (aux ty2)
    | Ty_constructor (name, []) -> name
    | Ty_constructor _ -> _die [%here]
    | Ty_arrow (t1, t2) -> spf "%s -> %s" (aux t1) (aux t2)
    | Ty_var x -> get_var_id x
    | Ty_poly (x, body) -> spf "forall %s. %s" (get_var_id x) (aux body)
    | Ty_tuple ts when List.length ts > 1 ->
        spf "(%s)" @@ List.split_by ", " aux ts
    | Ty_record { fds; _ } -> (
        (* let alias = match alias with None -> "_noalias" | Some x -> x in *)
        match fds with
        | [] -> ""
        | x :: _ when String.equal x.x "0" ->
            let l = List.map _get_ty fds in
            spf "(%s)" @@ List.split_by ", " layout_pnt l
        | _ ->
            spf "(%s)"
            @@ List.split_by ", "
                 (fun { x = a; ty = b } -> layout_pnt_typed a b)
                 fds)
    | _ as t -> layout t
  in
  aux t

and layout_pnt_typed str x =
  if Nt.equal_nt x Nt.unit_ty then str else spf "%s: %s" str (layout_pnt x)

let layout_pnt_typed_var x = spf "%s: %s" x.x (layout_pnt x.ty)

let layout_const = function
  | B b -> string_of_bool b
  | I i -> string_of_int i
  | S str -> spf "\"%s\"" str
  | U -> ""
  | C c -> spf "'%c'" c
  | F f -> string_of_float f

let mk_indent n str = spf "%s%s" (String.init (n * 2) (fun _ -> ' ')) str
let mk_indent_line n str = spf "%s%s\n" (String.init (n * 2) (fun _ -> ' ')) str

let binop =
  [
    "+"; "-"; "*"; "/"; "^"; ">"; ">="; "<"; "<="; "=="; "!="; "&&"; "||"; "mod";
  ]

let unop = [ "!"; "not" ]

let layout_p_expr =
  let rec aux expr =
    match expr.x with
    | AC c -> layout_const c
    | AAppOp (op, [ e1 ]) when String.equal op.x "raise" ->
        spf "%s %s" op.x (aux e1)
    | AAppOp (op, [ e1; e2 ]) when String.equal op.x "seq_nth" ->
        spf "%s[%s]" (aux e1) (aux e2)
    | AAppOp (op, [ e1; e2 ]) when String.equal op.x "map_get" ->
        spf "%s[%s]" (aux e1) (aux e2)
    | AAppOp (op, [ e1; e2 ]) when List.exists (String.equal op.x) binop ->
        spf "(%s %s %s)" (aux e1) op.x (aux e2)
    | AAppOp (op, [ e1 ]) when List.exists (String.equal op.x) unop ->
        spf "%s%s" op.x (aux e1)
    | AAppOp (op, args) -> spf "%s(%s)" op.x (List.split_by_comma aux args)
    | ATu [ e ] -> spf "(%s,)" (aux e)
    | ATu args -> spf "(%s)" (List.split_by_comma aux args)
    | ARecord [ (x, lit) ] -> spf "(%s = %s,)" x (aux lit)
    | ARecord fds ->
        spf "(%s)"
          (List.split_by_comma (fun (x, lit) -> spf "%s = %s" x (aux lit)) fds)
    | AProj (lit, n) -> spf "%s.%i" (aux lit) n
    | AField (lit, n) -> spf "%s.%s" (aux lit) n
    | AVar x -> x.x
  in
  aux

let layout_p_prop =
  let rec aux = function
    | Lit lit -> layout_p_expr lit
    | Implies (p1, p2) -> spf "(%s ==> %s)" (aux p1) (aux p2)
    | And [ p ] -> aux p
    | Or [ p ] -> aux p
    | And ps -> spf "(%s)" @@ List.split_by " && " aux ps
    | Or ps -> spf "(%s)" @@ List.split_by " || " aux ps
    | Not p -> spf "(not %s)" (aux p)
    | Iff (p1, p2) -> spf "(%s == %s)" (aux p1) (aux p2)
    | Ite _ -> _die_with [%here] "unimp"
    | Forall { qv; body } ->
        spf "(forall (%s). %s)" (layout_pnt_typed_var qv) (aux body)
    | Exists { qv; body } ->
        spf "(exists (%s). %s)" (layout_pnt_typed_var qv) (aux body)
  in
  aux

let rec layout_p_stmt n stmt =
  let last_semi = mk_indent_line n "}" in
  match stmt with
  | PMute lit -> mk_indent_line n @@ spf "%s;" @@ layout_p_expr lit
  | PAssign { assign_kind = Assign; lvalue; rvalue } ->
      mk_indent_line n
      @@ spf "%s = %s;" (layout_p_expr lvalue) (layout_p_expr rvalue)
  | PAssign { assign_kind = AssignSetAdd; lvalue; rvalue } ->
      mk_indent_line n
      @@ spf "%s += (%s);" (layout_p_expr lvalue) (layout_p_expr rvalue)
  | PAssign { assign_kind = AssignSeqAdd; lvalue; rvalue } ->
      mk_indent_line n
      @@ spf "%s += (0, %s);" (layout_p_expr lvalue) (layout_p_expr rvalue)
  | PAssign { assign_kind = AssignMapAdd; lvalue; rvalue } -> (
      match rvalue.x with
      | ATu [ e1; e2 ] ->
          mk_indent_line n
          @@ spf "%s[%s] = %s;" (layout_p_expr lvalue) (layout_p_expr e1)
               (layout_p_expr e2)
      | _ -> _die [%here])
  | PReturn e -> (
      match e.x with
      | AVar x when String.equal x.x "halt" ->
          mk_indent_line n @@ spf "raise halt;"
      | _ -> mk_indent_line n @@ spf "return %s;" (layout_p_expr e))
  | PPrintf (format, es) ->
      mk_indent_line n
      @@ spf "print format(\"%s\", %s);" format
           (List.split_by ", " layout_p_expr es)
  | PSend { dest; event_name; payload } ->
      mk_indent_line n
      @@ spf "send %s, %s, %s;" (layout_p_expr dest) event_name
           (layout_p_expr payload)
  | PGoto state -> mk_indent_line n @@ spf "goto %s;" state
  | PBreak -> mk_indent_line n "break;"
  | PIf { condition; tbranch; fbranch } ->
      let head =
        mk_indent_line n @@ spf "if (%s) {" (layout_p_expr condition)
      in
      let tbranch = layout_p_block (n + 1) tbranch in
      let last =
        match fbranch with
        | None -> last_semi
        | Some fbranch ->
            let mid = mk_indent_line n @@ spf "} else {" in
            let fbranch = layout_p_block (n + 1) fbranch in
            spf "%s%s%s" mid fbranch last_semi
      in
      spf "%s%s%s" head tbranch last
  | PForeach { iter; iterable; body; _ } ->
      let head =
        mk_indent_line n
        @@ spf "foreach (%s in %s) {" iter.x (layout_p_expr iterable)
      in
      let body = layout_p_block (n + 1) body in
      spf "%s%s%s" head body last_semi
  | PWhile { condition; body } ->
      let head =
        mk_indent_line n @@ spf "while (%s) {" (layout_p_expr condition)
      in
      let body = layout_p_block (n + 1) body in
      spf "%s%s%s" head body last_semi
  | PRecieve { input; event_name; body } ->
      let first =
        let fds = Nt.as_record [%here] input.ty in
        if List.length fds == 0 then
          mk_indent_line n @@ spf "receive { case %s: {" event_name
        else
          mk_indent_line n
          @@ spf "receive { case %s: (%s) {\n" event_name
               (layout_pnt_typed_var input)
      in
      let body = layout_p_block (n + 1) body in
      let last = mk_indent n "}}" in
      spf "%s%s%s" first body last

and layout_p_block n stmts =
  let stmts = List.map (layout_p_stmt n) stmts in
  String.concat "" stmts

let layout_p_closure n { local_vars; block } =
  let local_vars_str =
    List.split_by ""
      (fun x ->
        mk_indent_line (n + 1) @@ spf "var %s;" @@ layout_pnt_typed_var x)
      local_vars
  in
  let block = layout_p_block (n + 1) block in
  spf "%s%s" local_vars_str block

let layout_func_label = function
  | Plain -> "plain"
  | Entry -> "entry"
  | Exit -> "exit"
  | Listen name -> spf "on %s do" name

let layout_p_func n { name; func_label; params; retty; closure } =
  let params_str = List.split_by ", " layout_pnt_typed_var params in
  let head =
    match func_label with
    | Plain ->
        if List.length params == 0 then
          layout_pnt_typed (spf "fun %s()" name) retty ^ " {"
        else layout_pnt_typed (spf "fun %s (%s)" name params_str) retty ^ " {"
    | Entry ->
        if List.length params == 0 then layout_pnt_typed "entry" retty ^ " {"
        else layout_pnt_typed (spf "entry (%s)" params_str) retty ^ " {"
    | Exit -> "exit"
    | Listen name ->
        let prefix = spf "on %s do" name in
        if List.length params == 0 then spf "%s {" prefix
        else layout_pnt_typed (spf "%s (%s)" prefix params_str) retty ^ " {"
  in
  let closure = layout_p_closure n closure in
  let last = mk_indent_line n "}" in
  spf "%s%s%s" (mk_indent_line n head) closure last

let layout_state_label = function
  | Hot -> "hot"
  | Cold -> "cold"
  | Start -> "start"

let layout_state_labels = function
  | [] -> ""
  | [ x ] -> layout_state_label x ^ " "
  | [ Start; x ] | [ x; Start ] ->
      spf "%s %s " (layout_state_label Start) (layout_state_label x)
  | _ -> _die [%here]

let layout_p_state n { name; state_label; state_body } =
  let head =
    mk_indent_line n
    @@ spf "%sstate %s {" (layout_state_labels state_label) name
  in
  let state_body_str = List.split_by "" (layout_p_func (n + 1)) state_body in
  let last = mk_indent_line n "}" in
  spf "%s%s%s" head state_body_str last

let layout_p_machine n { name; local_vars; local_funcs; states } =
  let head = mk_indent_line n @@ spf "machine %s {" name in
  let local_vars_str =
    List.split_by ""
      (fun x ->
        mk_indent_line (n + 1) @@ spf "var %s;" @@ layout_pnt_typed_var x)
      local_vars
  in
  let local_funcs_str = List.split_by "" (layout_p_func (n + 1)) local_funcs in
  let states_str = List.split_by "" (layout_p_state (n + 1)) states in
  let last = mk_indent_line n "}" in
  spf "%s%s%s%s%s" head local_vars_str states_str local_funcs_str last

let layout_p_payload_prop n { name; self_event; prop } =
  let head =
    mk_indent_line n
    @@ spf "prop %s on %s do %s with" name self_event.ty self_event.x
  in
  let prop = mk_indent_line (n + 1) @@ spf "%s;" @@ layout_p_prop prop in
  spf "%s%s" head prop

let layout_p_payload_gen n { name; self_event; body } =
  let head =
    mk_indent_line n
    @@ spf "prop %s on %s do %s =" name self_event.ty self_event.x
  in
  let prop = mk_indent_line (n + 1) @@ spf "%s;" @@ layout_p_expr body in
  spf "%s%s" head prop

let layout_p_syn n { name; gen_num; cnames } =
  let gen_num =
    List.map
      (fun (x, dest, ass) ->
        let dest = lit_to_tlit @@ AVar dest#:(mk_p_set_ty p_machine_ty) in
        (x, lit_to_tlit @@ ATu [ dest; ass ]))
      gen_num
  in
  let record = lit_to_tlit @@ ARecord gen_num in
  let head =
    mk_indent_line n @@ spf "syn %s on %s with" name (layout_p_expr record)
  in
  let cs =
    mk_indent_line (n + 1)
    @@ spf "%s;"
    @@ List.split_by_comma (fun x -> x) cnames
  in
  spf "%s%s" head cs

let layout_p_item n item =
  match item with
  | PEnumDecl (name, es) ->
      mk_indent_line n
      @@ spf "enum %s {%s}" name (List.split_by ", " (fun x -> x) es)
  | PTopSimplDecl { kind = TopType; tvar } ->
      mk_indent_line n @@ spf "type %s = %s;" tvar.x (layout_pnt tvar.ty)
  | PTopSimplDecl { kind = TopVar; tvar } ->
      if Nt.is_base_tp tvar.ty then
        mk_indent_line n @@ spf "param %s : %s;" tvar.x (layout_pnt tvar.ty)
      else if is_builtin_op tvar.x then
        mk_indent_line n @@ spf "fun ( %s ): %s;" tvar.x (layout_pnt tvar.ty)
      else mk_indent_line n @@ spf "fun %s : %s;" tvar.x (layout_pnt tvar.ty)
  | PTopSimplDecl { kind = TopEvent; tvar } ->
      mk_indent_line n @@ spf "event %s : %s;" tvar.x (layout_pnt tvar.ty)
  | PGlobalProp { name; prop } ->
      let head = mk_indent_line n @@ spf "prop %s =" name in
      let prop = mk_indent_line (n + 1) @@ spf "%s;" @@ layout_p_prop prop in
      spf "%s%s" head prop
  | PPayload x -> layout_p_payload_prop n x
  | PPayloadGen x -> layout_p_payload_gen n x
  | PSyn x -> layout_p_syn n x

let layout_p_items items =
  String.concat "\n" @@ List.map (layout_p_item 0) items
