include Prop_gen
open Common
open Language
open Zdatatype
open Gen

let ctx_to_names ctx = List.map _get_x @@ Typectx.ctx_to_list ctx

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
      (fun (x, expr) -> mk_p_map_assign_var mk_counter_map (mk_p_str x) expr)
      gen_num
  in
  let closure = mk_p_closure [] (counter_init_block @ [ PGoto "Main" ]) in
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

let rec compile_template ctx gen_num (initied, payload, qvs, template) :
    Nt.t p_closure =
  match template with
  | TPReturn e ->
      let e = lit_gen ctx gen_num qvs e in
      mk_p_closure []
        [ mk_p_assign_var initied mk_p_true; mk_p_assign_var payload e ]
  | TPIf { condition; tbranch; fbranch } ->
      let res = mk_temp_bool_var () in
      let condition = compile_prop ctx gen_num qvs (res, condition) in
      let tbranch =
        compile_template_option ctx gen_num (initied, payload, qvs, tbranch)
      in
      let fbranch =
        compile_template_option ctx gen_num (initied, payload, qvs, fbranch)
      in
      let local_vars =
        condition.local_vars @ tbranch.local_vars @ fbranch.local_vars
      in
      let stmt = mk_p_if (var_to_p_expr res) tbranch.block fbranch.block in
      mk_p_closure local_vars (condition.block @ [ stmt ])

and compile_template_option ctx gen_num (initied, payload, qvs, template) :
    Nt.t p_closure =
  match template with
  | None -> mk_p_closure [] []
  | Some template ->
      compile_template ctx gen_num (initied, payload, qvs, template)

let compile_prop_gen_option ctx gen_num (payload, name, event_ty) =
  let default = mk_p_assign_var payload (generate_by_type event_ty) in
  let default = mk_p_closure [] [ default ] in
  match get_opt ctx.payload_gen_ctx name with
  | None -> default
  | Some { local_vars; content; _ } ->
      let inited = "inited"#:Nt.bool_ty in
      let closure =
        compile_template ctx gen_num (inited, payload, local_vars, content)
      in
      let iters, block =
        List.fold_right
          (fun qv (iters, body) ->
            let iter = qv#=>event_type_gen in
            let body =
              mk_p_foreach_seq qv#=>event_type_gen (qv_seq qv.ty)
                (body @ [ mk_p_if (var_to_p_expr inited) [ PBreak ] [] ])
            in
            (iter :: iters, [ body ]))
          local_vars ([], closure.block)
      in
      let block =
        [ mk_p_assign_var inited mk_p_false ]
        @ block
        @ [
            mk_p_if (var_to_p_expr inited) []
              [ mk_p_assign_var payload (generate_by_type event_ty) ];
          ]
      in
      mk_p_closure ([ inited ] @ iters @ closure.local_vars) block

let try_send_function ctx gen_num name =
  let dest_machines = "destMachines" in
  let cur_counter =
    mk_p_map_get (var_to_p_expr mk_counter_map) (mk_p_str name)
  in
  let counter_cond =
    mk_p_if (mk_p_app le_func [ cur_counter; mk_p_0 ]) [ PReturn mk_p_false ] []
  in
  let event_ty = _get_force [%here] ctx.event_ctx name in
  let payload = "payload"#:event_ty in
  let payload_init =
    compile_prop_gen_option ctx gen_num (payload, name, event_ty)
  in
  let dest = "dest"#:p_machine_ty in
  let dest_init =
    mk_p_assign_var dest
      (mk_p_choose (var_to_p_expr (mk_p_machine_domain_var dest_machines)))
  in
  let check_validate_block =
    check_validate_block ctx gen_num (dest, name, payload)
  in
  let send_stmt = mk_p_send dest.x name (var_to_p_expr payload) in
  let counter_stmt =
    mk_p_map_assign_var mk_counter_map (mk_p_str name)
      (mk_p_app minus_func [ cur_counter; mk_p_1 ])
  in
  let block =
    [ counter_cond; dest_init ]
    @ payload_init.block @ check_validate_block
    @ [
        record_event ctx (mk_p_self, var_to_p_expr dest, var_to_p_expr payload);
        counter_stmt;
        send_stmt;
        PReturn mk_p_true;
      ]
  in
  let closure =
    mk_p_closure ([ dest; payload ] @ payload_init.local_vars) block
  in
  {
    name = spf "send_%s" name;
    func_label = Plain;
    params = [];
    retty = Nt.bool_ty;
    closure;
  }

let send_function ctx gen_num =
  let action = "action"#:p_string_ty in
  let action_init = mk_p_assign_var action (mk_p_choose actions_space) in
  let stmts =
    List.map
      (fun (x, _) ->
        let body =
          mk_p_if
            (mk_p_app (get_p_func_var (try_send_function ctx gen_num x)) [])
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
        List.for_all (fun (y, _) -> not (String.equal x.x y)) gen_num)
    @@ ctx_to_list ctx.event_ctx
  in
  let receive_functions =
    List.map (fun x -> receive_function ctx x.x) recv_events
  in
  let cur_counters =
    List.map
      (fun (name, _) ->
        mk_p_map_get (var_to_p_expr mk_counter_map) (mk_p_str name))
      gen_num
  in
  let counter_cond =
    mk_p_if
      (mk_p_app lt_func [ mk_p_0; mk_p_sum cur_counters ])
      [ PMute (send_event ctx gen_num) ]
      [ mk_p_send "creator" "eGClientDone" mk_p_self ]
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

let sanity_check_ctx_by_cname ctx cnames =
  let payload_ctx =
    filter_ctx_name
      (fun x -> List.exists (String.equal x) cnames)
      ctx.payload_ctx
  in
  let payload_gen_ctx =
    ctx_from_list
    @@ List.filter
         (fun x -> List.exists (String.equal x.ty.gen_name) cnames)
         (ctx_to_list ctx.payload_gen_ctx)
  in
  if
    List.length (ctx_to_names payload_ctx)
    + List.length (ctx_to_names payload_gen_ctx)
    != List.length cnames
  then (
    Printf.printf "%s + %s\n"
      (StrList.to_string (ctx_to_names payload_ctx))
      (StrList.to_string (ctx_to_names payload_gen_ctx));
    Printf.printf "!= %s\n" (StrList.to_string cnames);
    _die [%here])
  else { ctx with payload_ctx; payload_gen_ctx }

let compile_syn_client ctx { name; gen_num; cnames } =
  let ctx = sanity_check_ctx_by_cname ctx cnames in
  let validate_functions =
    List.map
      (compile_validate_function ctx gen_num)
      (ctx_to_names ctx.payload_ctx)
  in
  let record_event_functions =
    List.map (fun x -> mk_record_event_function ctx x.ty)
    @@ ctx_to_list ctx.event_ctx
  in
  let send_functions =
    [ send_function ctx gen_num ]
    @ List.map (fun (x, _) -> try_send_function ctx gen_num x) gen_num
  in
  let seqs = mk_seq_vars ctx.event_ctx in
  let init_state = compile_init_state ctx gen_num in
  let () =
    Printf.printf "type trace =\n(%s)\n"
      (List.split_by ",\n " (fun x -> spf "%s: %s" x.x (layout_pnt x.ty)) seqs)
  in
  {
    name;
    local_vars = [ mk_counter_map ] @ seqs;
    local_funcs =
      [ int_gen ] @ validate_functions @ record_event_functions @ send_functions;
    states = [ init_state; compile_main_state ctx gen_num ];
  }

let compile ctx filename =
  let machines =
    List.map (fun x -> compile_syn_client ctx x.ty)
    @@ Typectx.ctx_to_list ctx.syn_ctx
  in
  let header =
    let trace_type =
      PTopSimplDecl
        { kind = TopType; tvar = "trace"#:(mk_p_trace ctx.event_ctx).ty }
    in
    let creator =
      PTopSimplDecl { kind = TopVar; tvar = "creator"#:p_machine_ty }
    in
    let destMachines =
      PTopSimplDecl
        { kind = TopVar; tvar = "destMachines"#:(mk_p_set_ty p_machine_ty) }
    in
    let gevent =
      PTopSimplDecl { kind = TopEvent; tvar = "eGClientDone"#:p_machine_ty }
    in
    [ trace_type; creator; destMachines; gevent ]
  in
  let header = layout_p_items header in
  let seq_function = layout_p_func 0 @@ mk_seq_length_function ctx in
  let l = List.map (fun f -> layout_p_machine 0 f) machines in
  let oc = open_out filename in
  let () = Printf.fprintf oc "%s\n" header in
  let () = Printf.fprintf oc "%s\n" seq_function in
  let () = List.iter (fun f -> Printf.fprintf oc "%s\n\n" f) l in
  close_out oc
