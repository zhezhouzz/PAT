open Language
open Interpreter

module DB = struct
  type cell = { content : int; next : int }

  let keyCounter = ref 0

  let fresh () =
    let key = !keyCounter in
    keyCounter := key + 1;
    key

  let headKey = ref 0
  let kvStore : (int, cell) Hashtbl.t = Hashtbl.create 10
  let compare old _ = old == !headKey
  let write x = headKey := x
  let read () = !headKey

  let get x =
    match Hashtbl.find_opt kvStore x with Some y -> y | None -> _die [%here]

  let put x y =
    match Hashtbl.find_opt kvStore x with
    | Some y -> Hashtbl.replace kvStore x y
    | None -> Hashtbl.add kvStore x y
end

let readHandler (_ : msg) =
  let x = DB.read () in
  send { op = "readResp"; args = [ I x ] }

let writeHandler (msg : msg) =
  let x = match msg.ev.args with [ I x ] -> x | _ -> _die [%here] in
  DB.write x;
  send { op = "writeResp"; args = [] }

let getHandler (msg : msg) =
  let x = match msg.ev.args with [ I x ] -> x | _ -> _die [%here] in
  let cell = DB.get x in
  send { op = "getResp"; args = [ I cell.content; I cell.next ] }

let putHandler (msg : msg) =
  let x, y =
    match msg.ev.args with
    | [ I x; I y; I z ] -> (x, DB.{ content = y; next = z })
    | _ -> _die [%here]
  in
  DB.put x y;
  send { op = "putResp"; args = [] }

let casHandler (msg : msg) =
  let x = match msg.ev.args with [ I x; I _ ] -> x | _ -> _die [%here] in
  let cond = x == DB.read () in
  send { op = "casResp"; args = [ B cond ] }

let do_read () =
  let () = send { op = "readReq"; args = [] } in
  let msg = recv "readResp" in
  match msg.ev.args with [ I x ] -> x | _ -> _die [%here]

let do_write v =
  let () = send { op = "writeReq"; args = [ I v ] } in
  let _ = recv "writeResp" in
  ()

let do_get x =
  let () = send { op = "getReq"; args = [ I x ] } in
  let msg = recv "getResp" in
  match msg.ev.args with
  | [ I x; I y ] -> DB.{ content = x; next = y }
  | _ -> _die [%here]

let do_put x y =
  let () = send { op = "putReq"; args = DB.[ I x; I y.content; I y.next ] } in
  let _ = recv "putResp" in
  ()

let do_cas old n =
  let () = send { op = "casReq"; args = [ I old; I n ] } in
  let msg = recv "casResp" in
  let cond = match msg.ev.args with [ B b ] -> b | _ -> _die [%here] in
  if cond then DB.write n;
  cond

let pushHandler (msg : msg) =
  let rec aux (v : int) =
    let oldHeadKey = do_read () in
    let newHead = DB.{ content = v; next = oldHeadKey } in
    let newHeadKey = DB.fresh () in
    if do_cas oldHeadKey newHeadKey then
      let () = do_put newHeadKey newHead in
      ()
    else aux v
  in
  match msg.ev.args with [ I v ] -> aux v | _ -> _die [%here]

let popHandler (_ : msg) =
  let rec aux () =
    let oldHeadKey = do_read () in
    if oldHeadKey == 0 then send { op = "popResp"; args = [ I 0 ] }
    else
      let oldHead = do_get oldHeadKey in
      if do_cas oldHeadKey oldHead.next then
        send { op = "popResp"; args = [ I oldHead.content ] }
      else aux ()
  in
  aux ()

let initHandler (_ : msg) =
  let () = do_write 0 in
  send { op = "initResp"; args = [] }

let initRespHandler (_ : msg) = ()
let pushRespHandler (_ : msg) = ()
let popRespHandler (_ : msg) = ()

let init () =
  register_handler "readReq" readHandler;
  register_handler "writeReq" writeHandler;
  register_handler "getReq" getHandler;
  register_handler "putReq" putHandler;
  register_handler "casReq" casHandler;
  register_handler "initReq" initHandler;
  register_handler "pushReq" pushHandler;
  register_handler "popReq" popHandler;
  register_handler "initResp" initRespHandler;
  register_handler "pushResp" pushRespHandler;
  register_handler "popResp" popRespHandler

open Nt

let record l = Ty_record { alias = None; fds = l }

let testCtx =
  Typectx.add_to_rights Typectx.emp
    [
      "readReq"#:(record []);
      "readResp"#:(record [ "x"#:int_ty ]);
      "writeReq"#:(record [ "x"#:int_ty ]);
      "writeResp"#:(record []);
      "getReq"#:(record [ "x"#:int_ty ]);
      "getResp"#:(record [ "x"#:int_ty; "y"#:int_ty ]);
      "putReq"#:(record [ "x"#:int_ty; "y"#:int_ty; "z"#:int_ty ]);
      "putResp"#:(record []);
      "casReq"#:(record [ "x"#:int_ty; "y"#:int_ty ]);
      "casResp"#:(record [ "x"#:bool_ty ]);
      "initReq"#:(record []);
      "initResp"#:(record []);
      "pushReq"#:(record [ "x"#:int_ty ]);
      "pushResp"#:(record []);
      "popReq"#:(record []);
      "popResp"#:(record [ "x"#:int_ty ]);
    ]

let genInitReq body = mk_term_gen testCtx "initReq" [] body
let genPushReq x body = mk_term_gen testCtx "pushReq" [ VVar x ] body
let genPopReq body = mk_term_gen testCtx "popReq" [] body

let mk_term_obs_fresh op k =
  let nty = _get_force [%here] testCtx op in
  let args = get_record_types nty in
  let args = List.map (fun x -> (Rename.unique_var x.x)#:x.ty) args in
  mk_term_obs testCtx op args mk_true (k args)

let obsWriteReq k =
  mk_term_obs_fresh "writeReq" (function [ x ] -> k x | _ -> _die [%here])

let obsWriteResp k =
  mk_term_obs_fresh "writeResp" (function [] -> k () | _ -> _die [%here])

let obsInitResp k =
  mk_term_obs_fresh "initResp" (function [] -> k () | _ -> _die [%here])

let main =
  genInitReq
    (obsWriteReq (fun _ ->
         obsWriteResp (fun () -> obsInitResp (fun () -> mk_term_tt))))
