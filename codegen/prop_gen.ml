open Language
open Zdatatype
open Common

let dest_machine = "dest"#:p_machine_ty

let prop_gen ctx gen_num (qvs : (Nt.t, string) typed list) (prop : Nt.t prop) =
  let var_in_qvs x = List.exists (fun y -> String.equal x.x y.x) qvs in
  let rec aux_lit lit =
    let res =
      match lit.x with
      | AC _ -> lit.x
      | AVar x -> if var_in_qvs x then (get_event_content x).x else lit.x
      | ATu es -> ATu (List.map aux_lit es)
      | AProj (e, i) -> AProj (aux_lit e, i)
      | ARecord fds -> ARecord (List.map (fun (x, lit) -> (x, aux_lit lit)) fds)
      | AField (e, fd) -> AField (aux_lit e, fd)
      | AAppOp (op, [ { x = AVar _; _ } ]) when String.equal op.x "self" ->
          mk_p_self.x
      | AAppOp (op, [ { x = AVar x; _ } ]) when String.equal op.x "dest" ->
          if var_in_qvs x then (get_event_dest x).x else AVar dest_machine
      | AAppOp (op, [ { x = AVar x; _ } ]) when String.equal op.x "src" ->
          if List.exists (fun (y, _, _) -> String.equal x.x y) gen_num then
            _die [%here]
          else if var_in_qvs x then (get_event_src x).x
          else mk_p_self.x
      | AAppOp (op, [ { x = AVar x; _ } ]) when String.equal op.x "last" ->
          let x = get_event_pos x in
          let len = seq_length ctx in
          let last = mk_p_app minus_func [ len; mk_p_1 ] in
          (mk_p_app eq_int_func [ x; last ]).x
      | AAppOp (op, [ { x = AVar x; _ }; { x = AVar y; _ } ])
        when var_in_qvs x && var_in_qvs y ->
          let args = List.map get_event_pos [ x; y ] in
          AAppOp (op, args)
      | AAppOp (op, args) -> AAppOp (op, List.map aux_lit args)
    in
    lit_to_tlit res
  in
  let rec aux p =
    let res =
      match p with
      | Lit lit -> (aux_lit lit).x
      | Implies (p1, p2) -> AAppOp (implies_func, [ aux p1; aux p2 ])
      | Not p -> AAppOp (not_func, [ aux p ])
      | And [] -> _die [%here]
      | And [ p ] -> (aux p).x
      | And (p :: res) -> AAppOp (and_func, [ aux p; aux (And res) ])
      | Or [] -> _die [%here]
      | Or [ p ] -> (aux p).x
      | Or (p :: res) -> AAppOp (or_func, [ aux p; aux (Or res) ])
      | Iff (p1, p2) -> AAppOp (iff_func, [ aux p1; aux p2 ])
      | Forall _ | Exists _ | Ite _ -> _die [%here]
    in
    lit_to_tlit res
  in
  aux prop

let flip =
  List.map (fun (q, x) ->
      match q with Nt.Fa -> (Nt.Ex, x) | Nt.Ex -> (Nt.Fa, x))

let to_prenex_form (prop : Nt.t prop) =
  let rec aux prop =
    match prop with
    | Lit _ -> ([], prop)
    | Implies (p1, p2) ->
        let prefix1, p1 = aux p1 in
        let prefix2, p2 = aux p2 in
        (flip prefix1 @ prefix2, Implies (p1, p2))
    | Not p ->
        let prefix, p = aux p in
        (flip prefix, Not p)
    | And ps ->
        let prefixs, ps = List.split @@ List.map aux ps in
        (List.concat prefixs, And ps)
    | Or ps ->
        let prefixs, ps = List.split @@ List.map aux ps in
        (List.concat prefixs, Or ps)
    | Iff (p1, p2) -> (
        let prefix1, p1 = aux p1 in
        let prefix2, p2 = aux p2 in
        match (prefix1, prefix2) with
        | [], [] -> ([], Iff (p1, p2))
        | _ -> _die [%here])
    | Forall { qv; body } ->
        let prefix, p = aux body in
        ((Nt.Fa, qv) :: prefix, p)
    | Exists { qv; body } ->
        let prefix, p = aux body in
        ((Nt.Ex, qv) :: prefix, p)
    | Ite _ -> _die [%here]
  in
  aux prop

let unique_qvs qvs =
  let qvs' = StrSet.of_list @@ List.map _get_x qvs in
  List.length qvs == StrSet.cardinal qvs'

let qv_res qv = (spf "%s_res" qv.x)#:Nt.bool_ty

let compile_prop ctx gen_num prop =
  (* let () = Printf.printf "Raw Prop: %s\n" (layout_prop prop) in *)
  let prefix, _ = to_prenex_form prop in
  (* let () = Printf.printf "Prop: %s\n" (layout_prop prop) in *)
  let qvs = List.map snd prefix in
  let () = if not (unique_qvs qvs) then _die [%here] in
  let res = "res"#:Nt.bool_ty in
  let counter = ref 0 in
  let mk_temp_bool_var () =
    let res = (spf "temp%i" !counter)#:Nt.bool_ty in
    counter := !counter + 1;
    res
  in
  let rec aux res prop =
    let prefix, _ = to_prenex_form prop in
    match prefix with
    | [] -> ([], [ mk_p_assign_var res (prop_gen ctx gen_num qvs prop) ])
    | _ -> (
        match prop with
        | Forall { qv; body } ->
            let res' = qv_res qv in
            let local_vars, body = aux res' body in
            let condition =
              mk_p_if (var_to_p_expr res') []
                [ mk_p_assign_var res mk_p_false; PBreak ]
            in
            let iter = qv#=>event_type_gen in
            let local_vars = res' :: iter :: local_vars in
            ( local_vars,
              [
                mk_p_assign_var res mk_p_true;
                mk_p_foreach_seq iter (qv_seq qv.ty) (body @ [ condition ]);
              ] )
        | Exists { qv; body } ->
            let res' = qv_res qv in
            let local_vars, body = aux res' body in
            let condition =
              mk_p_if (var_to_p_expr res')
                [ mk_p_assign_var res mk_p_true; PBreak ]
                []
            in
            let iter = qv#=>event_type_gen in
            let local_vars = res' :: iter :: local_vars in
            ( local_vars,
              [
                mk_p_assign_var res mk_p_false;
                mk_p_foreach_seq qv#=>event_type_gen (qv_seq qv.ty)
                  (body @ [ condition ]);
              ] )
        | And es ->
            let vars, local_vars, block =
              List.fold_left
                (fun (vars, local_vars, blocks) e ->
                  let tmp = mk_temp_bool_var () in
                  let local_vars', blocks' = aux tmp e in
                  (vars @ [ tmp ], local_vars @ local_vars', blocks @ blocks'))
                ([], [], []) es
            in
            ( local_vars @ vars,
              block
              @ [
                  mk_p_assign_var res (mk_and_sum (List.map var_to_p_expr vars));
                ] )
        | Or es ->
            let local_vars, block =
              List.fold_left
                (fun (vars, blocks) e ->
                  let tmp = mk_temp_bool_var () in
                  let vars', blocks' = aux tmp e in
                  (vars @ vars' @ [ tmp ], blocks @ blocks'))
                ([], []) es
            in
            ( local_vars,
              block
              @ [
                  mk_p_assign_var res
                    (mk_or_sum (List.map var_to_p_expr local_vars));
                ] )
        | _ -> _die [%here])
  in
  let local_vars, block = aux res prop in
  let local_vars = res :: local_vars in
  let closure =
    mk_p_closure local_vars (block @ [ PReturn (var_to_p_expr res) ])
  in
  closure

let compile_validate_function ctx gen_num name : Nt.t p_func =
  let { name; self_event; prop } =
    Typectx._get_force [%here] ctx.payload_ctx name
  in
  let event_ty = _get_force [%here] ctx.event_ctx self_event.ty in
  let params = [ dest_machine; self_event.x#:event_ty ] in
  let func_label = Plain in
  let retty = Nt.bool_ty in
  let closure = compile_prop ctx gen_num prop in
  { name; func_label; params; retty; closure }

let check_validate_block ctx gen_num (dest, event_name, payload) =
  let props =
    List.filter (fun (x : (t p_payload_prop, string) typed) ->
        String.equal event_name x.ty.self_event.ty)
    @@ ctx_to_list ctx.payload_ctx
  in
  let validate_functions =
    List.map (fun x -> compile_validate_function ctx gen_num x.x) props
  in
  let validate_functions = List.map get_p_func_var validate_functions in
  let mk_stmt f =
    mk_p_if
      (mk_p_app f [ var_to_p_expr dest; var_to_p_expr payload ])
      [] [ PReturn mk_p_false ]
  in
  List.map mk_stmt validate_functions
