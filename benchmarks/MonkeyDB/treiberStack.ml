open Language
open Interpreter

module DB = struct
  type cell = { content : int; next : int }

  let keyCounter = ref 1

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

let readAsync (ev : ev) =
  let x = DB.read () in
  { ev with args = [ I x ] }

let writeAsync (ev : ev) =
  let x = match ev.args with [ I x ] -> x | _ -> _die [%here] in
  DB.write x;
  { ev with args = [ I x ] }

let getAsync (ev : ev) =
  let x = match ev.args with [ I x ] -> x | _ -> _die [%here] in
  let cell = DB.get x in
  { ev with args = [ I x; I cell.content; I cell.next ] }

let putAsync (ev : ev) =
  let x, y =
    match ev.args with
    | [ I x; I y; I z ] -> (x, DB.{ content = y; next = z })
    | _ -> _die [%here]
  in
  DB.put x y;
  { ev with args = [ I x; I y.content; I y.next ] }

let casHandler (msg : msg) =
  let x = match msg.ev.args with [ I x; I _ ] -> x | _ -> _die [%here] in
  let cond = x == DB.read () in
  sendTo (Some msg.src, { op = "casResp"; args = [ B cond ] })

let do_read () =
  let msg = async ("read", []) in
  match msg.ev.args with [ I x ] -> x | _ -> _die [%here]

let do_write v =
  let _ = async ("write", [ I v ]) in
  ()

let do_get x =
  let msg = async ("get", [ I x ]) in
  match msg.ev.args with
  | [ I _; I y; I z ] -> DB.{ content = y; next = z }
  | _ -> _die [%here]

let do_put x y =
  let _ = async ("put", DB.[ I x; I y.content; I y.next ]) in
  ()

let do_cas old n =
  let () = send ("casReq", [ I old; I n ]) in
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
    if oldHeadKey == 0 then send ("popResp", [ I 0 ])
    else
      let oldHead = do_get oldHeadKey in
      if do_cas oldHeadKey oldHead.next then
        send ("popResp", [ I oldHead.content ])
      else aux ()
  in
  aux ()

let initHandler (_ : msg) =
  let () = do_write 0 in
  send ("initResp", [])

let initRespHandler (_ : msg) = ()
let pushRespHandler (_ : msg) = ()
let popRespHandler (_ : msg) = ()

let init () =
  Interpreter.init ();
  register_async "read" readAsync;
  register_async "write" writeAsync;
  register_async "get" getAsync;
  register_async "put" putAsync;
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
      "read"#:(record [ "x"#:int_ty ]);
      "write"#:(record [ "x"#:int_ty ]);
      "get"#:(record [ "x"#:int_ty; "y"#:int_ty; "z"#:int_ty ]);
      "put"#:(record [ "x"#:int_ty; "y"#:int_ty; "z"#:int_ty ]);
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

let obsWrite k =
  mk_term_obs_fresh testCtx "write" (function
    | [ x ] -> k x
    | _ -> _die [%here])

let obsRead k =
  mk_term_obs_fresh testCtx "read" (function [ x ] -> k x | _ -> _die [%here])

let obsGet k =
  mk_term_obs_fresh testCtx "get" (function
    | [ x; y; z ] -> k x y z
    | _ -> _die [%here])

let obsPut k =
  mk_term_obs_fresh testCtx "put" (function
    | [ x; y; z ] -> k x y z
    | _ -> _die [%here])

let obsCasReq k =
  mk_term_obs_fresh testCtx "casReq" (function
    | [ x; y ] -> k x y
    | _ -> _die [%here])

let obsCasResp k =
  mk_term_obs_fresh testCtx "casResp" (function
    | [ x ] -> k x
    | _ -> _die [%here])

let obsInitResp k =
  mk_term_obs_fresh testCtx "initResp" (function
    | [] -> k ()
    | _ -> _die [%here])

let obsPushResp k =
  mk_term_obs_fresh testCtx "pushResp" (function
    | [] -> k ()
    | _ -> _die [%here])

let obsPopResp k =
  mk_term_obs_fresh testCtx "popResp" (function
    | [ x ] -> k x
    | _ -> _die [%here])

let main1 =
  let initProcedure e =
    genInitReq (obsWrite (fun _ -> obsInitResp (fun () -> e)))
  in
  let pushProcedure e =
    mk_term_assume_fresh int_ty mk_true (fun x ->
        genPushReq x
          (obsRead (fun _ ->
               obsCasReq (fun _ _ ->
                   obsCasResp (fun _ -> obsPut (fun _ _ _ -> e))))))
  in
  let popProcedure e =
    genPopReq
      (genPopReq
         (obsRead (fun _ ->
              obsRead (fun _ ->
                  obsGet (fun _ _ _ ->
                      obsGet (fun _ _ _ ->
                          obsCasReq (fun _ _ ->
                              obsCasReq (fun _ _ ->
                                  obsCasResp (fun _ ->
                                      obsCasResp (fun _ ->
                                          obsPopResp (fun _ ->
                                              obsPopResp (fun _ -> e))))))))))))
  in
  initProcedure (pushProcedure (popProcedure mk_term_tt))

let main =
  let initProcedure e =
    genInitReq (obsWrite (fun _ -> obsInitResp (fun () -> e)))
  in
  let pushProcedure =
    mk_kleene_while
    @@ mk_term_assume_fresh int_ty mk_true (fun x ->
           genPushReq x
             (obsRead (fun _ ->
                  obsCasReq (fun _ _ ->
                      obsCasResp (fun _ -> obsPut (fun _ _ _ -> mk_term_tt))))))
  in
  let popProcedure e =
    genPopReq
      (genPopReq
         (obsRead (fun _ ->
              obsRead (fun _ ->
                  obsGet (fun _ _ _ ->
                      obsGet (fun _ _ _ ->
                          obsCasReq (fun _ _ ->
                              obsCasReq (fun _ _ ->
                                  obsCasResp (fun _ ->
                                      obsCasResp (fun _ ->
                                          obsPopResp (fun _ ->
                                              obsPopResp (fun _ -> e))))))))))))
  in
  initProcedure (term_concat pushProcedure (popProcedure mk_term_tt))
