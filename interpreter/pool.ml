(** Scheduler for message passing *)

open Language
open Zdatatype
open Effect.Deep
open Common

let default_tid = 0

module Runtime = struct
  let step_counter = ref 0
  let _curTid = ref 0
  let _counter = ref 1
  let hdPool : (int, handler) Hashtbl.t = Hashtbl.create 10
  let asyncPool : (string, async_handler) Hashtbl.t = Hashtbl.create 10
  let hisTrace : msg list ref = ref []
  let layout_handler_summary i (hd : handler) = spf "%i[%s]" i hd.op

  let print () =
    Pp.printf "@{<blue>Tid:@} %i\n" !_curTid;
    Pp.printf "@{<blue>Pool:@} %s\n"
      (Hashtbl.fold
         (fun i hd str -> spf "%s %s" (layout_handler_summary i hd) str)
         hdPool "")

  let print_hisTrace () =
    Pp.printf "@{<blue>HisTrace:@} %s\n"
      (List.map layout_msg_concise !hisTrace |> String.concat " ")
end

open Runtime

let init () =
  Hashtbl.clear hdPool;
  Hashtbl.clear asyncPool;
  MsgBuffer.init ();
  Store.init ();
  _curTid := 0;
  _counter := 1;
  hisTrace := [];
  step_counter := 0

let new_tid () =
  let tid = !_counter in
  _counter := tid + 1;
  tid

let addHandler (hd : handler) =
  if Hashtbl.mem hdPool hd.tid then _die_with [%here] "Handler already exists"
  else Hashtbl.add hdPool hd.tid hd

let register_handler op k =
  let hd = { tid = new_tid (); op; k } in
  addHandler hd

let register_async_has_ret op k =
  if Hashtbl.mem asyncPool op then
    _die_with [%here] "Async handler already exists"
  else Hashtbl.add asyncPool op { has_ret = true; k }

let register_async_no_ret op k =
  if Hashtbl.mem asyncPool op then
    _die_with [%here] "Async handler already exists"
  else
    Hashtbl.add asyncPool op
      {
        has_ret = false;
        k =
          (fun ev ->
            k ev;
            ev);
      }

let hdPoolNames () = Hashtbl.fold (fun _ h acc -> h.op :: acc) hdPool []
let appendToHisTrace msg = hisTrace := !hisTrace @ [ msg ]

let select_handler_by_tid tid =
  match Hashtbl.find_opt hdPool tid with
  | None -> _die_with [%here] "No handler found"
  | Some h -> h

let select_handler msg =
  match msg.dest with
  | Some i ->
      let hd = select_handler_by_tid i in
      Hashtbl.remove hdPool i;
      hd
  | None ->
      let handlers =
        Hashtbl.fold
          (fun _ h acc -> if String.equal h.op msg.ev.op then h :: acc else acc)
          hdPool []
      in
      let handler =
        match handlers with
        | [] -> _die_with [%here] "No handler found"
        | [ h ] -> h
        | _ ->
            _die_with [%here]
              "Warning: multiple handlers for the same operation"
      in
      let hd = { handler with tid = new_tid () } in
      hd

let handle_obs op f =
  Printf.printf "obs:tid %i\n" !_curTid;
  assert (default_tid == !_curTid);
  let msgs = MsgBuffer.find_by_op op in
  (* let () =
    Printf.printf "msgs0: %s\n" (List.map layout_msg msgs |> String.concat "\n")
  in *)
  let msgs =
    List.filter
      (fun msg ->
        let b = List.exists (String.equal msg.ev.op) (hdPoolNames ()) in
        (* let () =
          Printf.printf "msg: %s; %s =? %b\n" (layout_msg msg) msg.ev.op b
        in *)
        b)
      msgs
  in
  let () =
    match msgs with [] -> _die_with [%here] "No handler found" | _ -> ()
  in
  let () =
    Printf.printf "msgs1: %s\n" (List.map layout_msg msgs |> String.concat " ")
  in
  let msgs =
    List.filter
      (fun msg ->
        match Hashtbl.find_opt asyncPool msg.ev.op with
        | None -> f msg.ev
        | Some { has_ret = true; _ } -> true
        | Some { has_ret = false; _ } -> f msg.ev)
      msgs
  in
  let () =
    Printf.printf "msgs2: %s\n" (List.map layout_msg msgs |> String.concat " ")
  in
  let msg =
    match msgs with
    | [] -> _die_with [%here] "No message found"
    | _ -> List.nth msgs (Random.int (List.length msgs))
  in
  let hd = select_handler msg in
  MsgBuffer.consume msg;
  let msg' = { msg with dest = Some hd.tid } in
  match Hashtbl.find_opt asyncPool msg.ev.op with
  | None -> Some (hd, (fun ev -> ev), msg')
  | Some { k; _ } -> Some (hd, k, msg')

let jump_to_tid tid = _curTid := tid
let jump_back () = _curTid := default_tid

let handle_gen msg =
  Printf.printf "obs:gen %i\n" !_curTid;
  assert (default_tid == !_curTid);
  let hd = select_handler msg in
  appendToHisTrace msg;
  hd

let sendTo (dest, ev) =
  let msg = { src = !_curTid; dest; ev } in
  Effect.perform (Send msg)

let send (op, args) = sendTo (None, { op; args })
let recv op = Effect.perform (Recv op)

let announce (op, args) =
  sendTo (Some !_curTid, { op; args });
  let _ = recv op in
  ()

let async (op, args) =
  let msg = { src = !_curTid; dest = Some !_curTid; ev = { op; args } } in
  Effect.perform (Async msg)

let mk_dummy_msg () =
  let msg =
    {
      src = default_tid;
      dest = Some default_tid;
      ev = { op = "dummy"; args = [] };
    }
  in
  msg

let random_select_handler () =
  let msgs =
    if Hashtbl.mem hdPool default_tid then mk_dummy_msg () :: !MsgBuffer.buffer
    else !MsgBuffer.buffer
  in
  let () =
    Printf.printf "msgs: %s\n" (List.map layout_msg msgs |> String.concat " ")
  in
  let msg = List.nth msgs (Random.int (List.length msgs)) in
  let () = Printf.printf "select msg: %s\n" (layout_msg msg) in
  let hd = select_handler msg in
  let msg' = { msg with dest = Some hd.tid } in
  let msg' =
    match Hashtbl.find_opt asyncPool msg.ev.op with
    | None -> msg'
    | Some { k; _ } -> { msg' with ev = k msg.ev }
  in
  if not (String.equal msg.ev.op "dummy") then MsgBuffer.consume msg;
  appendToHisTrace msg';
  (hd, msg')

(* let dummy_hd_ids =
    Hashtbl.fold
      (fun id h acc -> if String.equal h.op "dummy" then id :: acc else acc)
      hdPool []
  in
  let direct_hds =
    Hashtbl.fold
      (fun id _ acc ->
        if
          List.exists
            (fun msg ->
              match msg.dest with Some dest -> dest == id | None -> false)
            !MsgBuffer.buffer
        then id :: acc
        else acc)
      hdPool []
  in
  let available_hds =
    Hashtbl.fold
      (fun id h acc ->
        let b =
          List.exists (fun msg -> String.equal msg.ev.op h.op) !MsgBuffer.buffer
        in
        if b then id :: acc else acc)
      hdPool []
  in *)
(* let hd_ids = List.slow_rm_dup ( == ) (dummy_hd_ids @ direct_hds @ available_hds) in
  let id = Random.int (List.length hd_ids) in
  let hd = Hashtbl.find hdPool id in
  let msg =  *)

let rec run main =
  match_with main ()
    {
      retc = (fun _ -> ());
      exnc = (fun e -> raise e);
      effc =
        (fun (type b) (eff : b Effect.t) ->
          match eff with
          | Send msg ->
              let () =
                _log "eval" @@ fun _ ->
                Pp.printf "@{<bold>SEND:@} %s\n" (layout_msg msg)
              in
              Some
                (fun (k : (b, _) continuation) ->
                  MsgBuffer.add msg;
                  continue k ())
          | Recv op ->
              let () =
                _log "eval" @@ fun _ -> Pp.printf "@{<bold>RECV:@} %s\n" op
              in
              Some
                (fun (k : (b, _) continuation) ->
                  let hd =
                    { tid = !_curTid; op; k = (fun msg -> continue k msg) }
                  in
                  addHandler hd)
          | Async msg ->
              let () =
                _log "eval" @@ fun _ ->
                Runtime.print ();
                Pp.printf "@{<bold>ASYNC:@} %s\n" (layout_msg msg)
              in
              MsgBuffer.add msg;
              Some
                (fun (k : (b, _) continuation) ->
                  let hd =
                    {
                      tid = !_curTid;
                      op = msg.ev.op;
                      k = (fun msg -> continue k msg);
                    }
                  in
                  addHandler hd)
          | Gen msg ->
              let () =
                _log "eval" @@ fun _ ->
                Pp.printf "@{<bold>GEN:@} %s\n" (layout_msg msg)
              in
              let hd = handle_gen msg in
              Some
                (fun (k : (b, _) continuation) ->
                  jump_to_tid hd.tid;
                  run (fun () -> hd.k msg);
                  jump_back ();
                  continue k ())
          | Obs (op, f) ->
              let () =
                _log "eval" @@ fun _ -> Pp.printf "@{<bold>OBS:@} %s\n" op
              in
              Some
                (fun (k : (b, _) continuation) ->
                  match handle_obs op f with
                  | Some (hd, ak, msg) ->
                      jump_to_tid hd.tid;
                      let msg = { msg with ev = ak msg.ev } in
                      ( _log "eval" @@ fun _ ->
                        Pp.printf "@{<bold>OBS:@} %s\n" (layout_msg msg) );
                      appendToHisTrace msg;
                      run (fun () -> hd.k msg);
                      jump_back ();
                      continue k (Some msg)
                  | None -> continue k None)
          | _ -> None);
    }

let rec random_scheduler main =
  let reschedule () =
    let hd, msg = random_select_handler () in
    jump_to_tid hd.tid;
    random_scheduler (fun () -> hd.k msg)
  in
  let print_state () =
    _log "eval" @@ fun _ ->
    Runtime.print ();
    Pp.printf "@{<blue>Store:@} %s\n" (Store.layout (Store.get ()))
  in
  match_with main ()
    {
      retc = (fun _ -> ());
      exnc = (fun e -> raise e);
      effc =
        (fun (type b) (eff : b Effect.t) ->
          match eff with
          | Send msg ->
              let () =
                _log "eval" @@ fun _ ->
                print_state ();
                Pp.printf "@{<bold>SEND:@} %s\n" (layout_msg msg)
              in
              Some
                (fun (k : (b, _) continuation) ->
                  let hd =
                    {
                      tid = !_curTid;
                      op = "dummy";
                      k = (fun _ -> continue k ());
                    }
                  in
                  MsgBuffer.add msg;
                  addHandler hd;
                  reschedule ())
          | Recv op ->
              let () =
                _log "eval" @@ fun _ ->
                print_state ();
                Pp.printf "@{<bold>RECV:@} %s\n" op
              in
              Some
                (fun (k : (b, _) continuation) ->
                  let hd =
                    { tid = !_curTid; op; k = (fun msg -> continue k msg) }
                  in
                  addHandler hd;
                  reschedule ())
          | Async msg ->
              let () =
                _log "eval" @@ fun _ ->
                print_state ();
                Pp.printf "@{<bold>ASYNC:@} %s\n" (layout_msg msg)
              in
              Some
                (fun (k : (b, _) continuation) ->
                  let hd =
                    {
                      tid = !_curTid;
                      op = msg.ev.op;
                      k = (fun msg -> continue k msg);
                    }
                  in
                  MsgBuffer.add msg;
                  addHandler hd;
                  reschedule ())
          | End ->
              let () = _log "eval" @@ fun _ -> Pp.printf "@{<bold>END:@}\n" in
              Some (fun (_ : (b, _) continuation) -> reschedule ())
          | Gen _ | Obs _ -> _die_with [%here] "never"
          | _ -> None);
    }
