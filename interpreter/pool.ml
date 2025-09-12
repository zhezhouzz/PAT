(** Scheduler for message passing *)

open Language
open Zdatatype
open Effect.Deep
open Common

let default_tid = 0

module Runtime = struct
  exception RuntimeError of string

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
         hdPool "");
    Pp.printf "@{<blue>MsgBuffer:@} %s\n"
      (List.map layout_msg !MsgBuffer.buffer |> String.concat " ")

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
  match msgs with
  | [] -> None
  | _ -> (
      let () =
        Printf.printf "msgs1: %s\n"
          (List.map layout_msg msgs |> String.concat " ")
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
        Printf.printf "msgs2: %s\n"
          (List.map layout_msg msgs |> String.concat " ")
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
      | Some { k; _ } -> Some (hd, k, msg'))

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

let random_select_handler curMsg =
  let () = Printf.printf "random_select_handler\n" in
  let msgs =
    if Hashtbl.mem hdPool default_tid then mk_dummy_msg () :: !MsgBuffer.buffer
    else !MsgBuffer.buffer
  in
  let () =
    Printf.printf "msgs: %s\n" (List.map layout_msg msgs |> String.concat " ")
  in
  let msg = List.nth (List.rev msgs) (Random.int (List.length msgs)) in
  let msg =
    match curMsg with
    | None -> msg
    | Some msg' -> if Random.int 100 < 1 then msg else msg'
  in
  let () = Printf.printf "select msg: %s\n" (layout_msg msg) in
  let hd = select_handler msg in
  let msg' = { msg with dest = Some hd.tid } in
  if not (String.equal msg.ev.op "dummy") then MsgBuffer.consume msg;
  match Hashtbl.find_opt asyncPool msg.ev.op with
  | None -> (hd, (fun ev -> ev), msg')
  | Some { k; _ } -> (hd, k, msg')

let eager_select_handler _ =
  let msg =
    match !MsgBuffer.buffer with
    | [] -> mk_dummy_msg ()
    | [ msg ] -> msg
    | _ -> _die_with [%here] "Multiple messages found"
  in
  let () = Printf.printf "select msg: %s\n" (layout_msg msg) in
  let hd = select_handler msg in
  let msg' = { msg with dest = Some hd.tid } in
  if not (String.equal msg.ev.op "dummy") then MsgBuffer.consume msg;
  match Hashtbl.find_opt asyncPool msg.ev.op with
  | None -> (hd, (fun ev -> ev), msg')
  | Some { k; _ } -> (hd, k, msg')

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
                  | None ->
                      _log "eval" @@ fun _ ->
                      Pp.printf "@{<yellow>cannot find handler for obs:@} %s\n"
                        op;
                      continue k None)
          | _ -> None);
    }

let rec random_scheduler main =
  let print_state loc =
    _log "eval" @@ fun _ ->
    Runtime.print ();
    (* Pp.printf "@{<blue>Store:@} %s\n" (Store.layout (Store.get ())); *)
    Printf.printf "loc: %s\n" (pos_to_string loc)
  in
  let reschedule curMsg =
    if List.length !MsgBuffer.buffer <= 0 then ()
    else
      let () = print_state [%here] in
      let hd, ak, msg = random_select_handler curMsg in
      (* let _ = Out_channel.flush stdout in
      let _ = print_string "Press Enter to continue..." in
      let _ = input_line stdin in *)
      jump_to_tid hd.tid;
      let msg = { msg with ev = ak msg.ev } in
      let () = Printf.printf "new msg: %s\n" (layout_msg msg) in
      appendToHisTrace msg;
      random_scheduler (fun () -> hd.k msg)
  in
  match_with main ()
    {
      retc = (fun _ -> reschedule None);
      exnc = (fun e -> raise e);
      effc =
        (fun (type b) (eff : b Effect.t) ->
          match eff with
          | Send msg ->
              let () =
                _log "eval" @@ fun _ ->
                print_state [%here];
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
                  reschedule (Some msg))
          | Recv op ->
              let () =
                _log "eval" @@ fun _ ->
                print_state [%here];
                Pp.printf "@{<bold>RECV:@} %s\n" op
              in
              Some
                (fun (k : (b, _) continuation) ->
                  let hd =
                    { tid = !_curTid; op; k = (fun msg -> continue k msg) }
                  in
                  addHandler hd;
                  reschedule None)
          | Async msg ->
              let () =
                _log "eval" @@ fun _ ->
                print_state [%here];
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
                  addHandler hd;
                  reschedule (Some msg))
          | End ->
              let () = _log "eval" @@ fun _ -> Pp.printf "@{<bold>END:@}\n" in
              Some (fun (_ : (b, _) continuation) -> reschedule None)
          | Gen _ | Obs _ -> _die_with [%here] "never"
          | _ -> None);
    }

let rec eager_scheduler main =
  let print_state loc =
    _log "eval" @@ fun _ ->
    Runtime.print ();
    (* Pp.printf "@{<blue>Store:@} %s\n" (Store.layout (Store.get ())); *)
    Printf.printf "loc: %s\n" (pos_to_string loc)
  in
  let check_end () =
    match Hashtbl.find_opt hdPool default_tid with
    | None -> List.length !MsgBuffer.buffer <= 0
    | Some _ -> false
  in
  let reschedule curMsg =
    let () = print_state [%here] in
    if check_end () then ()
    else
      let hd, ak, msg = eager_select_handler curMsg in
      jump_to_tid hd.tid;
      let msg = { msg with ev = ak msg.ev } in
      let () = Printf.printf "new msg: %s\n" (layout_msg msg) in
      appendToHisTrace msg;
      eager_scheduler (fun () -> hd.k msg)
  in
  match_with main ()
    {
      retc = (fun _ -> reschedule None);
      exnc = (fun e -> raise e);
      effc =
        (fun (type b) (eff : b Effect.t) ->
          match eff with
          | Send msg ->
              let () =
                _log "eval" @@ fun _ ->
                print_state [%here];
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
                  reschedule (Some msg))
          | Recv op ->
              let () =
                _log "eval" @@ fun _ ->
                print_state [%here];
                Pp.printf "@{<bold>RECV:@} %s\n" op
              in
              Some
                (fun (k : (b, _) continuation) ->
                  let hd =
                    { tid = !_curTid; op; k = (fun msg -> continue k msg) }
                  in
                  addHandler hd;
                  reschedule None)
          | Async msg ->
              let () =
                _log "eval" @@ fun _ ->
                print_state [%here];
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
                  addHandler hd;
                  reschedule (Some msg))
          | End ->
              let () = _log "eval" @@ fun _ -> Pp.printf "@{<bold>END:@}\n" in
              Some (fun (_ : (b, _) continuation) -> reschedule None)
          | Gen _ | Obs _ -> _die_with [%here] "never"
          | _ -> None);
    }
