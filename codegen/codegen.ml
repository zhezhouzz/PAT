include Prop_gen
open Common
open Language
open Zdatatype
open Gen

let ctx_to_names ctx = List.map _get_x @@ Typectx.ctx_to_list ctx
let mk_counter_map = "counterMap"#:(mk_p_map_ty p_string_ty Nt.int_ty)
let action_space = "actions"#:(mk_p_set_ty p_string_ty)

let mk_p_init_state_with_entry name func =
  let func = { func with func_label = Entry } in
  { name; state_label = [ Start ]; state_body = [ func ] }

let mk_record_event_function ctx nt =
  let x = "x"#:nt in
  let src = "src"#:p_machine_ty in
  let dest = "dest"#:p_machine_ty in
  let idx = seq_length ctx in
  let block =
    [
      mk_p_seq_assign_var (qv_seq nt)
        (mk_p_tuple
           [ idx; var_to_p_expr src; var_to_p_expr dest; var_to_p_expr x ]);
    ]
  in
  let closure = mk_p_closure [] block in
  {
    name = spf "record_%s" (qv_seq nt).x;
    func_label = Plain;
    params = [ src; dest; x ];
    retty = Nt.unit_ty;
    closure;
  }

let record_event ctx (src, dest, v) =
  PMute
    (mk_p_app
       (get_p_func_var (mk_record_event_function ctx v.ty))
       [ src; dest; v ])

let receive_function ctx name =
  let event_ty = _get_force [%here] ctx.event_ctx name in
  let input = "input"#:event_ty in
  let block =
    [
      record_event ctx (mk_p_self, mk_p_self, var_to_p_expr input); PGoto "Main";
    ]
  in
  let closure = mk_p_closure [] block in
  {
    name = "";
    func_label = Listen name;
    params = [ input ];
    retty = Nt.unit_ty;
    closure;
  }

let compile_init_state _ gen_num =
  let counter_init_block =
    List.map
      (fun (x, _, expr) -> mk_p_map_assign_var mk_counter_map (mk_p_str x) expr)
      gen_num
  in
  let actions =
    List.map
      (fun (x, _, _) -> mk_p_set_assign_var action_space (mk_p_str x))
      gen_num
  in
  let closure =
    mk_p_closure [] (actions @ counter_init_block @ [ PGoto "Main" ])
  in
  let entry =
    {
      name = "init";
      func_label = Entry;
      params = [];
      retty = Nt.unit_ty;
      closure;
    }
  in
  mk_p_init_state_with_entry "Init" entry

let try_send_function ctx gen_num (name, s) =
  let cur_counter =
    mk_p_map_get (var_to_p_expr mk_counter_map) (mk_p_str name)
  in
  let counter_cond =
    mk_p_if (mk_p_app le_func [ cur_counter; mk_p_0 ]) [ PReturn mk_p_false ] []
  in
  let event_ty = _get_force [%here] ctx.event_ctx name in
  let payload = "payload"#:event_ty in
  let payload_init =
    mk_p_assign_var payload (generate_by_type gen_num event_ty)
  in
  let dest = "dest"#:p_machine_ty in
  let dest_init =
    mk_p_assign_var dest
      (mk_p_choose (var_to_p_expr (mk_p_machine_domain_var s)))
  in
  let check_validate_block =
    check_validate_block ctx gen_num (dest, name, payload)
  in
  let send_stmt = mk_p_send dest name (var_to_p_expr payload) in
  let counter_stmt =
    mk_p_map_assign_var mk_counter_map (mk_p_str name)
      (mk_p_app minus_func [ cur_counter; mk_p_1 ])
  in
  let block =
    [ dest_init; payload_init; counter_cond ]
    @ check_validate_block
    @ [
        record_event ctx (mk_p_self, var_to_p_expr dest, var_to_p_expr payload);
        counter_stmt;
        send_stmt;
        PReturn mk_p_true;
      ]
  in
  let closure = mk_p_closure [ dest; payload ] block in
  {
    name = spf "send_%s" name;
    func_label = Plain;
    params = [];
    retty = Nt.bool_ty;
    closure;
  }

let send_function ctx gen_num =
  let action = "action"#:p_string_ty in
  let action_init =
    mk_p_assign_var action (mk_p_choose (var_to_p_expr action_space))
  in
  let stmts =
    List.map
      (fun (x, s, _) ->
        let body =
          mk_p_if
            (mk_p_app
               (get_p_func_var (try_send_function ctx gen_num (x, s)))
               [])
            [ PBreak ] []
        in
        mk_p_if
          (mk_p_app eq_str_func [ var_to_p_expr action; mk_p_str x ])
          [ body ] [])
      gen_num
  in
  let block = [ mk_p_while_true (action_init :: stmts) ] in
  let closure = mk_p_closure [ action ] block in
  {
    name = "doSend";
    func_label = Plain;
    params = [];
    retty = Nt.unit_ty;
    closure;
  }

let send_event ctx gen_num =
  mk_p_app (get_p_func_var (send_function ctx gen_num)) []

let compile_main_state ctx gen_num =
  let recv_events =
    List.filter (fun x ->
        List.for_all (fun (y, _, _) -> not (String.equal x.x y)) gen_num)
    @@ ctx_to_list ctx.event_ctx
  in
  let receive_functions =
    List.map (fun x -> receive_function ctx x.x) recv_events
  in
  let cur_counters =
    List.map
      (fun (name, _, _) ->
        mk_p_map_get (var_to_p_expr mk_counter_map) (mk_p_str name))
      gen_num
  in
  let counter_cond =
    mk_p_if
      (mk_p_app lt_func [ mk_p_0; mk_p_sum cur_counters ])
      [ PMute (send_event ctx gen_num) ]
      []
  in
  let entry =
    {
      name = "";
      func_label = Entry;
      params = [];
      retty = Nt.unit_ty;
      closure = mk_p_closure [] [ counter_cond ];
    }
  in
  {
    name = "Main";
    state_label = [];
    state_body = [ entry ] @ receive_functions;
  }

let compile_syn_client ctx { name; gen_num; cnames } =
  let payload_prop_names = ctx_to_names ctx.payload_ctx in
  let payload_gen_names = ctx_to_names ctx.payload_gen_ctx in
  let prop_names, cnames =
    List.partition
      (fun x -> List.exists (String.equal x) payload_prop_names)
      cnames
  in
  let _, cnames =
    List.partition
      (fun x -> List.exists (String.equal x) payload_gen_names)
      cnames
  in
  let () = if List.length cnames != 0 then _die [%here] in
  let validate_functions =
    List.map (compile_validate_function ctx gen_num) prop_names
  in
  let record_event_functions =
    mk_seq_length_function ctx
    :: (List.map (fun x -> mk_record_event_function ctx x.ty)
       @@ ctx_to_list ctx.event_ctx)
  in
  let send_functions =
    [ send_function ctx gen_num ]
    @ List.map (fun (x, s, _) -> try_send_function ctx gen_num (x, s)) gen_num
  in
  let machine_gen = machine_gen gen_num in
  let seqs = mk_seq_vars ctx.event_ctx in
  let init_state = compile_init_state ctx gen_num in
  {
    name;
    local_vars = [ action_space; mk_counter_map ] @ seqs;
    local_funcs =
      [ machine_gen; int_gen ] @ validate_functions @ record_event_functions
      @ send_functions;
    states = [ init_state; compile_main_state ctx gen_num ];
  }

let compile ctx filename =
  let machines =
    List.map (fun x -> compile_syn_client ctx x.ty)
    @@ Typectx.ctx_to_list ctx.syn_ctx
  in
  let l = List.map (fun f -> layout_p_machine 0 f) machines in
  let oc = open_out filename in
  let () = List.iter (fun f -> Printf.fprintf oc "%s\n\n" f) l in
  close_out oc
