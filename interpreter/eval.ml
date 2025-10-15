open Language
open Common
open Store
open Pool

let rec subst_rec fix (e : term) =
  match e with
  | CLetE { lhs; rhs; body } ->
      CLetE
        { lhs; rhs = typed_subst_rec fix rhs; body = typed_subst_rec fix body }
  | CAppOp _ | CObs _ | CGen _ | CAssertP _ | CAssume _ | CVal _ -> e
  | CUnion es -> CUnion (List.map (typed_subst_rec fix) es)
  | CFix { retBranch; recBranch } ->
      CFix
        {
          retBranch = typed_subst_rec fix retBranch;
          recBranch = typed_subst_rec fix recBranch;
        }
  | CFixApp { cfix; iterV; boundV } ->
      let cfix =
        match cfix with Some _ -> _die_with [%here] "never" | None -> Some fix
      in
      (* let () = Pp.printf "@{<bold>new cfix:@} %s\n" (layout_term fix.x) in *)
      CFixApp { cfix; iterV; boundV }

and typed_subst_rec fix e = e#->(subst_rec fix)

(* let counter = ref 0 *)

let rec eval code =
  (* let () =
    if !counter > 15 then _die_with [%here] "stop" else counter := !counter + 1
  in *)
  let tid = 0 in
  let () = Runtime.step_counter := !Runtime.step_counter + 1 in
  let () =
    _log "eval" @@ fun _ ->
    Pp.printf "@{<bold>Eval(%i):@}\n" !Runtime.step_counter;
    Pool.Runtime.print ();
    Pp.printf "@{<blue>Store:@} %s\n" (Store.layout (Store.get ()));
    Pp.printf "@{<blue>MsgBuffer:@} %s\n" (MsgBuffer.layout ());
    Pp.printf "@{<orange>Term:@}\n%s\n" (layout_term code)
  in
  match code with
  | CVal v ->
      let c = eval_value (Store.get ()) v.x in
      [ c ]
  | CLetE { lhs; rhs = { x = CAssume (_, prop); _ }; body } ->
      let s = Sample.sample_phi (Store.get ()) (lhs, prop) in
      Store.add_list s;
      eval body.x
  | CLetE { lhs; rhs = { x = CObs { op; prop }; _ }; body } -> (
      let msg =
        Effect.perform
          (Obs
             ( op.x,
               fun ev ->
                 let st = Store.fadd (lhs, ev.args) (Store.get ()) in
                 eval_prop st prop ))
      in
      match msg with
      | None -> raise (RuntimeInconsistent "no msg is available for given prop")
      | Some msg ->
          (* let () = Pp.printf "@{<bold>Perform:@} %s\n" (layout_msg msg) in *)
          let () = Store.add (lhs, msg.ev.args) in
          if eval_prop (Store.get ()) prop then eval body.x
          else raise (RuntimeInconsistent "prop is not satisfied (need retry)"))
  | CLetE { lhs = _; rhs = { x = CFix _; _ } as fix; body } ->
      let body = typed_subst_rec fix body in
      eval body.x
  | CLetE { lhs; rhs; body } ->
      let res = eval rhs.x in
      let () = Store.add (lhs, res) in
      eval body.x
  | CAppOp { op; args } ->
      let cs = meval_value (Store.get ()) args in
      let cs = List.map (value_to_const [%here]) cs in
      let c = eval_app_op_const op cs in
      [ VConst c ]
  | CObs _ -> _die_with [%here] "never"
  | CGen { op; args } ->
      let args = meval_value (Store.get ()) args in
      let () =
        Effect.perform
          (Gen { src = tid; dest = None; ev = { op = op.x; args } })
      in
      []
  | CUnion [] -> _die_with [%here] "never"
  | CUnion es -> eval (Sample.choose_from_list es).x
  | CAssertP phi ->
      if eval_prop (Store.get ()) phi then []
      else raise (RuntimeInconsistent "assertion is not satisfied (need retry)")
  | CAssume _ -> _die_with [%here] "never"
  | CFix _ -> _die_with [%here] "never"
  | CFixApp
      {
        cfix = Some ({ x = CFix { retBranch; recBranch }; _ } as fix);
        iterV;
        boundV;
      } ->
      let iterI =
        match eval iterV.x with
        | [ VConst (I i) ] -> i
        | _ -> _die_with [%here] "never"
      in
      let boundI =
        match eval_value (Store.get ()) boundV.x with
        | VConst (I i) -> i
        | _ -> _die_with [%here] "never"
      in
      let retBranch =
        term_to_tterm
          (subst_term_instance default_iter_var.x (AC (I iterI))
          @@ subst_term_instance default_bound_var.x (AC (I boundI)) retBranch.x
          )
      in
      let recBranch =
        term_to_tterm
          (subst_term_instance default_iter_var.x (AC (I iterI))
          @@ subst_term_instance default_bound_var.x (AC (I boundI)) recBranch.x
          )
      in
      let recBranch = typed_subst_rec fix recBranch in
      (* let () =
        Pp.printf "@{<bold>retBranch:@} %s\n" (layout_term retBranch.x)
      in
      let () =
        Pp.printf "@{<bold>recBranch:@} %s\n" (layout_term recBranch.x)
      in *)
      if iterI >= boundI then eval retBranch.x else eval recBranch.x
  | CFixApp _ -> _die_with [%here] "never"
(* | KStar { body } ->
      if Random.bool () then [] else eval (term_concat body.x code)
  | CWhile { body; cond } ->
      let () =
        match eval body.x with
        | [] | [ VConst U ] -> ()
        | _ -> _die_with [%here] "never"
      in
      if eval_prop (Store.get ()) cond then eval (CWhile { body; cond }) else [] *)

let eval_to_unit code =
  let _ = eval code in
  let () = Pp.printf "@{<blue>Code:@}\n%s\n" (layout_term code) in
  Pool.Runtime.print_hisTrace ();
  ()
