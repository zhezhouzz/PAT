(** Scheduler for message passing *)

open Language
open Zdatatype
open Effect.Deep
open Common

let default_tid = 0

module Runtime = struct
  let _curTid = ref 0
  let _counter = ref 1
  let hdPool : (int, handler) Hashtbl.t = Hashtbl.create 10
  let asyncPool : (string, ev -> ev) Hashtbl.t = Hashtbl.create 10
  let hisTrace : msg list ref = ref []
  let layout_handler_summary i hd = spf "%i[%s]" i hd.op

  let print () =
    Pp.printf "@{<blue>Tid:@} %i\n" !_curTid;
    Pp.printf "@{<blue>Pool:@} %s\n"
      (Hashtbl.fold
         (fun i hd str -> spf "%s %s" (layout_handler_summary i hd) str)
         hdPool "")
end

open Runtime

let init () =
  Hashtbl.clear hdPool;
  Hashtbl.clear asyncPool;
  _curTid := 0;
  _counter := 1;
  hisTrace := []

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

let register_async op k =
  if Hashtbl.mem asyncPool op then
    _die_with [%here] "Async handler already exists"
  else Hashtbl.add asyncPool op k

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

let try_handle_op op f =
  let msgs = MsgBuffer.find_by_op op in
  (* let () =
    Printf.printf "msgs0: %s\n" (List.map layout_msg msgs |> String.concat "\n")
  in *)
  (* let () = Printf.printf "pool: %s\n" (hdPoolNames () |> String.concat "\n") in *)
  let msgs =
    List.filter
      (fun msg -> List.exists (String.equal msg.ev.op) (hdPoolNames ()))
      msgs
  in
  (* let () =
    Printf.printf "msgs1: %s\n" (List.map layout_msg msgs |> String.concat "\n")
  in *)
  let msgs =
    List.filter
      (fun msg ->
        match Hashtbl.find_opt asyncPool msg.ev.op with
        | None -> f msg.ev
        | Some _ -> true)
      msgs
  in
  (* let () =
    Printf.printf "msgs2: %s\n" (List.map layout_msg msgs |> String.concat "\n")
  in *)
  let msg =
    match msgs with
    | [] -> _die_with [%here] "No message found"
    | _ -> List.nth msgs (Random.int (List.length msgs))
  in
  let hd = select_handler msg in
  let msg' = { msg with dest = Some hd.tid } in
  match Hashtbl.find_opt asyncPool msg.ev.op with
  | None -> Some (msg, msg', hd)
  | Some k ->
      let msg' = { msg' with ev = k msg.ev } in
      if f msg'.ev then Some (msg, msg', hd) else None

let jump_to_tid tid = _curTid := tid
let jump_back () = _curTid := default_tid

let handle_obs op f =
  Printf.printf "obs:tid %i\n" !_curTid;
  assert (default_tid == !_curTid);
  match try_handle_op op f with
  | Some (orginal_msg, msg, hd) ->
      MsgBuffer.consume orginal_msg;
      appendToHisTrace msg;
      Some (hd, msg)
  | None -> None

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
                  addHandler hd)
          | Gen msg ->
              let () =
                _log "eval" @@ fun _ ->
                Pp.printf "@{<bold>GEN:@} %s\n" (layout_msg msg)
              in
              Some
                (fun (k : (b, _) continuation) ->
                  let hd = handle_gen msg in
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
                  | Some (hd, msg) ->
                      jump_to_tid hd.tid;
                      run (fun () -> hd.k msg);
                      jump_back ();
                      continue k (Some msg)
                  | None -> continue k None)
          | _ -> None);
    }
