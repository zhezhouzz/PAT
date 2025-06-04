open Language
open Zdatatype

let implies_func = "impl"#:Nt.(construct_arr_tp ([ bool_ty; bool_ty ], bool_ty))
let iff_func = "iff"#:Nt.(construct_arr_tp ([ bool_ty; bool_ty ], bool_ty))
let and_func = "&&"#:Nt.(construct_arr_tp ([ bool_ty; bool_ty ], bool_ty))
let or_func = "||"#:Nt.(construct_arr_tp ([ bool_ty; bool_ty ], bool_ty))
let not_func = "!"#:Nt.(construct_arr_tp ([ bool_ty ], bool_ty))
let plus_func = "+"#:Nt.(construct_arr_tp ([ int_ty; int_ty ], int_ty))
let minus_func = "-"#:Nt.(construct_arr_tp ([ int_ty; int_ty ], int_ty))
let lt_func = "<"#:Nt.(construct_arr_tp ([ int_ty; int_ty ], bool_ty))
let le_func = "<="#:Nt.(construct_arr_tp ([ int_ty; int_ty ], bool_ty))

let eq_str_func =
  "=="#:Nt.(construct_arr_tp ([ p_string_ty; p_string_ty ], bool_ty))

let eq_int_func = "=="#:Nt.(construct_arr_tp ([ int_ty; int_ty ], bool_ty))

let event_type_gen (nt : Nt.t) =
  Nt.mk_tuple [%here] [ Nt.int_ty; p_machine_ty; p_machine_ty; nt ]

let var_to_p_expr var = lit_to_tlit (AVar var)
let mk_p_app f args = lit_to_tlit (AAppOp (f, args))
let mk_p_tuple lits = lit_to_tlit (ATu lits)
let mk_p_record lits = lit_to_tlit (ARecord lits)
let mk_p_feild lit fd = lit_to_tlit (AField (lit, fd))
let mk_p_machine_domain_var s = s#:(mk_p_set_ty p_machine_ty)
let mk_p_self = var_to_p_expr "this"#:p_machine_ty
let mk_p_true = lit_to_tlit (AC (B true))
let mk_p_false = lit_to_tlit (AC (B false))
let mk_p_str s = lit_to_tlit (AC (S s))
let mk_p_while_true body = PWhile { condition = mk_p_true; body }
let mk_p_halt = PReturn (var_to_p_expr "halt"#:Nt.unit_ty)

let mk_default ty =
  mk_p_app "default"#:Nt.unit_ty [ var_to_p_expr (Nt.layout ty)#:ty ]

let mk_p_map_get m idx =
  let t1, t2 =
    match m.ty with
    | Nt.Ty_constructor (str, [ t1; t2 ]) when String.equal str "map" -> (t1, t2)
    | _ -> _die [%here]
  in
  mk_p_app "map_get"#:Nt.(construct_arr_tp ([ m.ty; t1 ], t2)) [ m; idx ]

let mk_p_closure local_vars block =
  let local_vars =
    List.slow_rm_dup
      (fun x y -> String.equal x.x y.x && Nt.equal_nt x.ty y.ty)
      local_vars
  in
  { local_vars; block }

let mk_p_int i = lit_to_tlit (AC (I i))
let mk_p_0 = lit_to_tlit (AC (I 0))
let mk_p_1 = lit_to_tlit (AC (I 1))

let mk_p_sum es =
  List.fold_left (fun res e -> mk_p_app plus_func [ e; res ]) mk_p_0 es

let mk_and_sum es =
  List.fold_left (fun res e -> mk_p_app and_func [ e; res ]) mk_p_true es

let mk_or_sum es =
  List.fold_left (fun res e -> mk_p_app or_func [ e; res ]) mk_p_false es

let mk_p_choose arg =
  let choose_ty =
    match arg.ty with
    | Nt.Ty_constructor (str, []) when String.equal str "int" ->
        Nt.(construct_arr_tp ([ int_ty ], int_ty))
    | Nt.Ty_constructor (str, [ nt ]) when String.equal str "set" ->
        Nt.(construct_arr_tp ([ arg.ty ], nt))
    | Nt.Ty_constructor (str, [ nt ]) when String.equal str "seq" ->
        Nt.(construct_arr_tp ([ arg.ty ], nt))
    | _ -> _die [%here]
  in
  let choose = "choose"#:choose_ty in
  mk_p_app choose [ arg ]

let get_size arg =
  mk_p_app "sizeof"#:Nt.(construct_arr_tp ([ arg.ty ], int_ty)) [ arg ]

let event_lit_gen (qv : (Nt.t, string) typed) =
  lit_to_tlit (AVar qv#=>event_type_gen)

let get_event_pos (qv : (Nt.t, string) typed) =
  lit_to_tlit (AProj (event_lit_gen qv, 0))

let get_event_src (qv : (Nt.t, string) typed) =
  lit_to_tlit (AProj (event_lit_gen qv, 1))

let get_event_dest (qv : (Nt.t, string) typed) =
  lit_to_tlit (AProj (event_lit_gen qv, 2))

let get_event_content (qv : (Nt.t, string) typed) =
  lit_to_tlit (AProj (event_lit_gen qv, 3))

let _destruct_record loc nt =
  match nt with
  | Nt.Ty_record { alias = Some name; fds } -> (name, fds)
  | _ -> _die loc

let qv_seq nt =
  let name, _ = _destruct_record [%here] nt in
  let name = spf "%s_seq" name in
  name#:(mk_p_seq_ty (event_type_gen nt))

let mk_p_assign_var x expr =
  PAssign { assign_kind = Assign; lvalue = lit_to_tlit (AVar x); rvalue = expr }

let mk_p_seq_assign_var x expr =
  PAssign
    { assign_kind = AssignSeqAdd; lvalue = lit_to_tlit (AVar x); rvalue = expr }

let mk_p_set_assign_var x expr =
  PAssign
    { assign_kind = AssignSetAdd; lvalue = lit_to_tlit (AVar x); rvalue = expr }

let mk_p_map_assign_var x expr1 expr2 =
  PAssign
    {
      assign_kind = AssignMapAdd;
      lvalue = lit_to_tlit (AVar x);
      rvalue = mk_p_tuple [ expr1; expr2 ];
    }

let mk_p_if condition tbranch fbranch =
  match fbranch with
  | [] -> PIf { condition; tbranch; fbranch = None }
  | _ -> PIf { condition; tbranch; fbranch = Some fbranch }

let mk_p_foreach_seq iter seq body =
  PForeach
    { foreach_kind = ForeachSeq; iter; iterable = lit_to_tlit (AVar seq); body }

let mk_p_send dest event_name payload =
  PSend { dest = var_to_p_expr dest#:p_machine_ty; event_name; payload }

let mk_seq_vars event_ctx =
  List.map (fun x -> qv_seq x.ty) @@ ctx_to_list event_ctx

let mk_p_trace event_ctx =
  let seqs = mk_seq_vars event_ctx in
  let record = mk_p_record (List.map (fun x -> (x.x, var_to_p_expr x)) seqs) in
  record

let mk_seq_length_function ctx =
  let input = "trace"#:(mk_p_trace ctx.event_ctx).ty in
  let seqs =
    List.map (fun x -> get_size (mk_p_feild (var_to_p_expr input) x.x))
    @@ mk_seq_vars ctx.event_ctx
  in
  let block = [ PReturn (mk_p_sum seqs) ] in
  let closure = mk_p_closure [] block in
  {
    name = "get_seq_length";
    func_label = Plain;
    params = [ input ];
    retty = Nt.int_ty;
    closure;
  }

let seq_length ctx =
  mk_p_app
    (get_p_func_var (mk_seq_length_function ctx))
    [ mk_p_trace ctx.event_ctx ]

(** P global functions and variables *)

let get_p_keys lit =
  match lit.ty with
  | Nt.Ty_constructor (_, [ ty; _ ]) ->
      let keys_ty = Nt.construct_arr_tp ([ lit.ty ], mk_p_set_ty ty) in
      mk_p_app "keys"#:keys_ty [ lit ]
  | _ -> _die [%here]

let mk_counter_map = "counterMap"#:(mk_p_map_ty p_string_ty Nt.int_ty)
let actions_space = get_p_keys (tvar_to_lit mk_counter_map)
