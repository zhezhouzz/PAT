open Language
open Common
open Store
open Pool

let rec eval code =
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
          let () = Pp.printf "@{<bold>Perform:@} %s\n" (layout_msg msg) in
          let () = Store.add (lhs, msg.ev.args) in
          if eval_prop (Store.get ()) prop then eval body.x
          else raise (RuntimeInconsistent "prop is not satisfied (need retry)"))
  | CLetE { lhs; rhs; body } ->
      let () = Store.add (lhs, eval rhs.x) in
      eval body.x
  | CAppOp { op; args } -> eval (CAppOp { op; args })
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
  | KStar { body } ->
      if Random.bool () then [] else eval (term_concat body.x code)
  | CWhile { body; cond } ->
      let () =
        match eval body.x with
        | [] | [ VConst U ] -> ()
        | _ -> _die_with [%here] "never"
      in
      if eval_prop (Store.get ()) cond then eval (CWhile { body; cond }) else []

let eval_to_unit code =
  let _ = eval code in
  let () = Pp.printf "@{<blue>Code:@}\n%s\n" (layout_term code) in
  Pool.Runtime.print_hisTrace ();
  ()
