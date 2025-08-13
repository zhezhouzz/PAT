open Language
open Interpreter
open Common
module StackDB = PairDB
open StackDB

let do_cas old n =
  do_trans (fun tid ->
      let key = do_read tid in
      if key == old then
        let _ = do_write tid n in
        true
      else false)

let topReqHandler (msg : msg) =
  let aux () =
    let oldHeadKey, oldHead =
      do_trans (fun tid ->
          let oldHeadKey = do_read tid in
          let oldHead = do_get tid oldHeadKey in
          (oldHeadKey, oldHead))
    in
    if oldHeadKey == 0 then 0 else fst oldHead
  in
  match msg.ev.args with
  | [] ->
      let v = aux () in
      send ("topResp", [ mk_value_int v ])
  | _ -> _die [%here]

let pushReqHandler (msg : msg) =
  let rec aux (v : int) =
    let oldHeadKey = do_trans (fun tid -> do_read tid) in
    let newHead = (v, oldHeadKey) in
    let newHeadKey = fresh_key () in
    let cas_res =
      do_trans (fun tid ->
          let key = do_read tid in
          if key == oldHeadKey then
            let _ = do_write tid newHeadKey in
            let _ = do_put tid newHeadKey newHead in
            true
          else false)
    in
    if cas_res then () else aux v
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
let topRespHandler (_ : msg) = ()

let init isolation_level () =
  register_async_has_ret "begin" beginAsync;
  register_async_has_ret "commit" commitAsync;
  register_async_has_ret "read" readAsync;
  register_async_no_ret "write" writeAsync;
  register_async_has_ret "get" getAsync;
  register_async_no_ret "put" putAsync;
  register_handler "initReq" initReqHandler;
  register_handler "pushReq" pushReqHandler;
  register_handler "popReq" popReqHandler;
  register_handler "topReq" topReqHandler;
  register_handler "initResp" initRespHandler;
  register_handler "pushResp" pushRespHandler;
  register_handler "popResp" popRespHandler;
  register_handler "topResp" topRespHandler;
  StackDB.init isolation_level

open Nt

let record l = Ty_record { alias = None; fds = l }

let testCtx =
  Typectx.add_to_rights Typectx.emp
    ([
       "initReq"#:(record []);
       "initResp"#:(record []);
       "pushReq"#:(record [ "x"#:int_ty ]);
       "pushResp"#:(record []);
       "popReq"#:(record []);
       "popResp"#:(record [ "x"#:int_ty ]);
       "topReq"#:(record []);
       "topResp"#:(record [ "x"#:int_ty ]);
     ]
    @ event_typectx)

let gen name args body =
  mk_term_gen testCtx name (List.map (fun x -> VVar x) args) body

let obs name k = mk_term_obs_fresh testCtx name (fun _ -> k)
let obsInitResp e = mk_term_obs_fresh testCtx "initResp" (fun _ -> e)
let obsPushResp e = mk_term_obs_fresh testCtx "pushResp" (fun _ -> e)
let obsPopResp e = mk_term_obs_fresh testCtx "popResp" (fun _ -> e)
let obsTopResp e = mk_term_obs_fresh testCtx "topResp" (fun _ -> e)

let obsBegin k =
  mk_term_obs_fresh testCtx "begin" (function
    | tid' :: _ -> k tid'
    | _ -> _die [%here])

let obsCommit tid k =
  mk_term_obs_prop_fresh testCtx "commit" (function
    | tid' :: _ ->
        let prop = lit_to_prop (mk_var_eq_var [%here] tid tid') in
        (prop, k)
    | _ -> _die [%here])

let obsWrite tid k =
  mk_term_obs_prop_fresh testCtx "write" (function
    | tid' :: _ ->
        let prop = lit_to_prop (mk_var_eq_var [%here] tid tid') in
        (prop, k)
    | _ -> _die [%here])

let obsRead tid k =
  mk_term_obs_prop_fresh testCtx "read" (function
    | tid' :: _ ->
        let prop = lit_to_prop (mk_var_eq_var [%here] tid tid') in
        (prop, k)
    | _ -> _die [%here])

let obsGet tid k =
  mk_term_obs_prop_fresh testCtx "get" (function
    | tid' :: _ ->
        let prop = lit_to_prop (mk_var_eq_var [%here] tid tid') in
        (prop, k)
    | _ -> _die [%here])

let obsPut tid k =
  mk_term_obs_prop_fresh testCtx "put" (function
    | tid' :: _ ->
        let prop = lit_to_prop (mk_var_eq_var [%here] tid tid') in
        (prop, k)
    | _ -> _die [%here])

let main =
  mk_term_assume_fresh_true int_ty (fun x ->
      mk_term_assume_fresh int_ty
        (fun y -> lit_to_prop (mk_var_eq_var [%here] x y))
        (fun y ->
          let initProcedure e =
            gen "initReq" []
              (obsBegin (fun tid ->
                   obsWrite tid @@ obsCommit tid @@ obsInitResp e))
          in
          let pushProcedure e =
            gen "pushReq" [ x ]
              (obsBegin (fun tid1 ->
                   obsRead tid1 @@ obsCommit tid1
                   @@ gen "pushReq" [ y ]
                        (obsBegin (fun tid2 ->
                             obsRead tid2 @@ obsCommit tid2
                             @@ obsBegin (fun tid1' ->
                                    obsBegin (fun tid2' ->
                                        obsRead tid1' @@ obsWrite tid1'
                                        @@ obsPut tid1' @@ obsRead tid2'
                                        @@ obsWrite tid2' @@ obsPut tid2'
                                        @@ obsCommit tid2' @@ obsCommit tid1'
                                        @@ obsPushResp @@ obsPushResp e))))))
          in
          let topProcedure e =
            gen "topReq" []
              (obsBegin (fun tid1 ->
                   obsRead tid1 @@ obsGet tid1 @@ obsCommit tid1 @@ obsTopResp e))
          in
          initProcedure (pushProcedure (topProcedure mk_term_tt))))

let main1 =
  mk_term_assume_fresh_true int_ty (fun x ->
      mk_term_assume_fresh int_ty
        (fun y -> lit_to_prop (mk_var_eq_var [%here] x y))
        (fun _ ->
          let initProcedure e =
            gen "initReq" []
              (obsBegin (fun tid ->
                   obsWrite tid @@ obsCommit tid @@ obsInitResp e))
          in
          let pushProcedure e =
            gen "pushReq" [ x ]
              (obsBegin (fun tid ->
                   obsRead tid @@ obsCommit tid
                   @@ obsBegin (fun tid2 ->
                          obsRead tid2 @@ obsWrite tid2 @@ obsPut tid2
                          @@ obsCommit tid2 @@ obsPushResp e)))
          in
          let popProcedure e =
            gen "popReq" []
              (obsBegin (fun tid1 ->
                   obsRead tid1 @@ obsGet tid1 @@ obsCommit tid1
                   @@ gen "popReq" []
                        (obsBegin (fun tid2 ->
                             obsRead tid2 @@ obsGet tid2
                             @@ obsCommit tid2
                                  (obsBegin (fun tid1' ->
                                       obsRead tid1' @@ obsWrite tid1'
                                       @@ obsBegin (fun tid2' ->
                                              obsRead tid2' @@ obsWrite tid2'
                                              @@ obsCommit tid2'
                                              @@ obsCommit tid1' @@ obsPopResp
                                              @@ obsPopResp e)))))))
          in
          initProcedure (pushProcedure (popProcedure mk_term_tt))))

type stack_bench_config = { numElems : int; numOp : int }

let qc_stack { numElems; numOp } =
  let elems = List.init numElems (fun i -> i + 1) in
  let random_elem () = List.nth elems (Random.int numElems) in
  let random_push () = send ("pushReq", [ mk_value_int (random_elem ()) ]) in
  let random_pop () = send ("popReq", []) in
  let () = send ("initReq", []) in
  let rec loop n =
    if n <= 0 then ()
    else (
      if Random.bool () then random_push () else random_pop ();
      loop (n - 1))
  in
  let () = loop numOp in
  Effect.perform End

(* let main =
  let initProcedure e =
    genInitReq (obsWrite (fun _ -> obsInitResp (fun () -> e)))
  in
  let pushProcedure =
    mk_kleene_while
    @@ mk_term_assume_fresh_true int_ty (fun x ->
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
  initProcedure (term_concat pushProcedure (popProcedure mk_term_tt)) *)
