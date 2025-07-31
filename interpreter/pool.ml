(** Scheduler for message passing *)

open Language
open Zdatatype
open Effect.Deep
open Common

let _curTid = ref 0
let _counter = ref 1
let default_tid = 0
let controller = ref None

let invokeController () =
  match !controller with
  | None -> _die_with [%here] "No controller"
  | Some c -> Eval.eval c

let set_controller c = controller := Some c

let new_tid () =
  let tid = !_counter in
  _counter := tid + 1;
  tid

let hdPool : (int, handler) Hashtbl.t = Hashtbl.create 10

let addHandler (hd : handler) =
  if Hashtbl.mem hdPool hd.tid then _die_with [%here] "Handler already exists"
  else Hashtbl.add hdPool hd.tid hd

let hdPoolNames () = Hashtbl.fold (fun _ h acc -> h.op :: acc) hdPool []
let hisTrace : msg list ref = ref []
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
  let msgs = List.filter (fun msg -> f msg.ev) msgs in
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
  (msg, msg', hd)

let jump_to_tid tid = _curTid := tid
let jump_back () = _curTid := default_tid

let handle_obs op f =
  Printf.printf "obs:tid %i\n" !_curTid;
  assert (default_tid == !_curTid);
  let orginal_msg, msg, hd = try_handle_op op f in
  MsgBuffer.consume orginal_msg;
  appendToHisTrace msg;
  (hd, msg)

let handle_gen msg =
  Printf.printf "obs:gen %i\n" !_curTid;
  assert (default_tid == !_curTid);
  let hd = select_handler msg in
  appendToHisTrace msg;
  hd.k msg

let register_handler op k =
  let hd = { tid = new_tid (); op; k } in
  addHandler hd

let sendTo (dest, ev) =
  let msg = { src = !_curTid; dest; ev } in
  Effect.perform (Send msg)

let send ev = sendTo (None, ev)
let recv op = Effect.perform (Recv op)

let announce (op, ev) =
  sendTo (Some !_curTid, { op; args = ev });
  let _ = recv op in
  ()

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
          | Gen msg ->
              let () =
                _log "eval" @@ fun _ ->
                Pp.printf "@{<bold>GEN:@} %s\n" (layout_msg msg)
              in
              Some
                (fun (k : (b, _) continuation) ->
                  let () = run (fun () -> handle_gen msg) in
                  continue k ())
          | Obs (op, f) ->
              let () =
                _log "eval" @@ fun _ -> Pp.printf "@{<bold>OBS:@} %s\n" op
              in
              Some
                (fun (k : (b, _) continuation) ->
                  let hd, msg = handle_obs op f in
                  jump_to_tid hd.tid;
                  run (fun () -> hd.k msg);
                  jump_back ();
                  continue k msg)
          | _ -> None);
    }
