open Language
open Interpreter
open Common
open StackDB

let do_cas old n =
  do_trans (fun tid ->
      let key = do_read tid in
      if key == old then
        let _ = do_write tid n in
        true
      else false)

let pushReqHandler (msg : msg) =
  let rec aux (v : int) =
    let oldHeadKey = do_trans (fun tid -> do_read tid) in
    let newHead = (v, oldHeadKey) in
    let newHeadKey = fresh_key () in
    let cas_res = do_cas oldHeadKey newHeadKey in
    if cas_res then do_trans (fun tid -> do_put tid newHeadKey newHead)
    else aux v
  in
  match msg.ev.args with
  | [ VConst (I v) ] ->
      let _ = aux v in
      send ("pushResp", [])
  | _ -> _die [%here]

let popReqHandler (msg : msg) =
  let rec aux () =
    let oldHeadKey, oldHead =
      do_trans (fun tid ->
          let oldHeadKey = do_read tid in
          let oldHead = do_get tid oldHeadKey in
          (oldHeadKey, oldHead))
    in
    if oldHeadKey == 0 then 0
    else
      let cas_res = do_cas oldHeadKey (snd oldHead) in
      if cas_res then fst oldHead else aux ()
  in
  match msg.ev.args with
  | [] ->
      let v = aux () in
      send ("popResp", [ mk_value_int v ])
  | _ -> _die [%here]

let initReqHandler (_ : msg) =
  let _ = do_trans (fun tid -> do_write tid 0) in
  send ("initResp", [])

let initRespHandler (_ : msg) = ()
let pushRespHandler (_ : msg) = ()
let popRespHandler (_ : msg) = ()

let init () =
  Interpreter.init ();
  register_async_has_ret "read" readAsync;
  register_async_no_ret "write" writeAsync;
  register_async_has_ret "get" getAsync;
  register_async_no_ret "put" putAsync;
  register_handler "initReq" initReqHandler;
  register_handler "pushReq" pushReqHandler;
  register_handler "popReq" popReqHandler;
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
