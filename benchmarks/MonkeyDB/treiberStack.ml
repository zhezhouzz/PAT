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
  { ev with args = [ mk_value_int x ] }

let writeAsync (ev : ev) =
  let x = match ev.args with [ VConst (I x) ] -> x | _ -> _die [%here] in
  DB.write x;
  { ev with args = [ mk_value_int x ] }

let getAsync (ev : ev) =
  let x = match ev.args with [ VConst (I x) ] -> x | _ -> _die [%here] in
  let cell = DB.get x in
  {
    ev with
    args = [ mk_value_int x; mk_value_int cell.content; mk_value_int cell.next ];
  }

let putAsync (ev : ev) =
  let x, y =
    match ev.args with
    | [ VConst (I x); VConst (I y); VConst (I z) ] ->
        (x, DB.{ content = y; next = z })
    | _ -> _die [%here]
  in
  DB.put x y;
  {
    ev with
    args = [ mk_value_int x; mk_value_int y.content; mk_value_int y.next ];
  }

let casHandler (msg : msg) =
  let x =
    match msg.ev.args with
    | [ VConst (I x); VConst (I _) ] -> x
    | _ -> _die [%here]
  in
  let cond = x == DB.read () in
  sendTo (Some msg.src, { op = "casResp"; args = [ mk_value_bool cond ] })

let do_read () =
  let msg = async ("read", []) in
  match msg.ev.args with [ VConst (I x) ] -> x | _ -> _die [%here]

let do_write v =
  let _ = async ("write", [ mk_value_int v ]) in
  ()

let do_get x =
  let msg = async ("get", [ mk_value_int x ]) in
  match msg.ev.args with
  | [ VConst (I _); VConst (I y); VConst (I z) ] -> DB.{ content = y; next = z }
  | _ -> _die [%here]

let do_put x y =
  let _ =
    async
      ("put", DB.[ mk_value_int x; mk_value_int y.content; mk_value_int y.next ])
  in
  ()

let do_cas old n =
  let () = send ("casReq", [ mk_value_int old; mk_value_int n ]) in
  let msg = recv "casResp" in
  let cond =
    match msg.ev.args with [ VConst (B b) ] -> b | _ -> _die [%here]
  in
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
  match msg.ev.args with [ VConst (I v) ] -> aux v | _ -> _die [%here]

let popHandler (_ : msg) =
  let rec aux () =
    let oldHeadKey = do_read () in
    if oldHeadKey == 0 then send ("popResp", [ mk_value_int 0 ])
    else
      let oldHead = do_get oldHeadKey in
      if do_cas oldHeadKey oldHead.next then
        send ("popResp", [ mk_value_int oldHead.content ])
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

let qc_stack n =
  let stack_length = ref 0 in
  let used = ref [] in
  let rec genPushElem () =
    let x = Random.int 100 in
    if List.mem x !used then genPushElem ()
    else
      let () = used := x :: !used in
      x
  in
  let () = send ("initReq", []) in
  let rec loop n =
    if n <= 0 then ()
    else if !stack_length > 0 then
      if Random.bool () then
        let () = stack_length := !stack_length + 1 in
        send ("pushReq", [ mk_value_int (genPushElem ()) ])
      else
        let () = stack_length := !stack_length - 1 in
        send ("popReq", [])
    else send ("pushReq", [ mk_value_int (genPushElem ()) ]);
    loop (n - 1)
  in
  let () = loop n in
  Effect.perform End

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
