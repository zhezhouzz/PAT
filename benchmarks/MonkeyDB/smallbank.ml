open Language
open Interpreter
open Common
module SmallbankDB = IntDB
open SmallbankDB

let transferCheckingReqHandler (msg : msg) =
  let aux (user1 : int) (user2 : int) (amount : int) =
    do_trans (fun tid ->
        let balance1 = do_selectChecking tid user1 in
        let balance2 = do_selectChecking tid user2 in
        if balance1 + 10 < amount then ()
        else
          let _ = do_updateChecking tid user1 (balance1 - amount) in
          let _ = do_updateChecking tid user2 (balance2 + amount) in
          ())
  in
  match msg.ev.args with
  | [ VConst (I user1); VConst (I user2); VConst (I amount) ] ->
      let () = aux user1 user2 amount in
      send ("transferCheckingResp", [])
  | _ -> _die [%here]

let transferSavingReqHandler (msg : msg) =
  let aux (user1 : int) (user2 : int) (amount : int) =
    do_trans (fun tid ->
        let balance1 = do_selectSaving tid user1 in
        let balance2 = do_selectSaving tid user2 in
        if balance1 + 10 < amount then ()
        else
          let _ = do_updateSaving tid user1 (balance1 - amount) in
          let _ = do_updateSaving tid user2 (balance2 + amount) in
          ())
  in
  match msg.ev.args with
  | [ VConst (I user1); VConst (I user2); VConst (I amount) ] ->
      let () = aux user1 user2 amount in
      send ("transferSavingResp", [])
  | _ -> _die [%here]

let initAccountReqHandler (msg : msg) =
  let aux (user : int) (checking : int) (saving : int) =
    do_trans (fun tid ->
        let _ = do_updateChecking tid user checking in
        let _ = do_updateSaving tid user saving in
        ())
  in
  match msg.ev.args with
  | [ VConst (I user); VConst (I checking); VConst (I saving) ] ->
      let () = aux user checking saving in
      send ("initAccountResp", [])
  | _ -> _die [%here]

let showCheckingReqHandler (msg : msg) =
  let aux (user : int) = do_trans (fun tid -> do_selectChecking tid user) in
  match msg.ev.args with
  | [ VConst (I user) ] ->
      let balance = aux user in
      send ("showCheckingResp", [ mk_value_int balance ])
  | _ -> _die [%here]

let showSavingReqHandler (msg : msg) =
  let aux (user : int) = do_trans (fun tid -> do_selectSaving tid user) in
  match msg.ev.args with
  | [ VConst (I user) ] ->
      let balance = aux user in
      send ("showSavingResp", [ mk_value_int balance ])
  | _ -> _die [%here]

let transferCheckingRespHandler (_ : msg) = ()
let transferSavingRespHandler (_ : msg) = ()
let initAccountRespHandler (_ : msg) = ()
let showCheckingRespHandler (_ : msg) = ()
let showSavingRespHandler (_ : msg) = ()

let init isolation_level () =
  register_async_has_ret "begin" beginAsync;
  register_async_has_ret "commit" commitAsync;
  register_async_has_ret "get" getAsync;
  register_async_no_ret "put" putAsync;
  register_async_has_ret "selectName" selectNameAsync;
  register_async_no_ret "updateName" updateNameAsync;
  register_async_has_ret "selectSaving" selectSavingAsync;
  register_async_no_ret "updateSaving" updateSavingAsync;
  register_async_has_ret "selectChecking" selectCheckingAsync;
  register_async_no_ret "updateChecking" updateCheckingAsync;
  register_handler "transferCheckingReq" transferCheckingReqHandler;
  register_handler "transferSavingReq" transferSavingReqHandler;
  register_handler "initAccountReq" initAccountReqHandler;
  register_handler "showCheckingReq" showCheckingReqHandler;
  register_handler "showSavingReq" showSavingReqHandler;
  register_handler "transferCheckingResp" transferCheckingRespHandler;
  register_handler "transferSavingResp" transferSavingRespHandler;
  register_handler "initAccountResp" initAccountRespHandler;
  register_handler "showCheckingResp" showCheckingRespHandler;
  register_handler "showSavingResp" showSavingRespHandler;
  SmallbankDB.init isolation_level

open Nt

let record l = Ty_record { alias = None; fds = l }

let testCtx =
  Typectx.add_to_rights Typectx.emp
    ([
       "transferCheckingReq"#:(record
                                 [
                                   "user1"#:int_ty;
                                   "user2"#:int_ty;
                                   "amount"#:int_ty;
                                 ]);
       "transferCheckingResp"#:(record []);
       "transferSavingReq"#:(record
                               [
                                 "user1"#:int_ty;
                                 "user2"#:int_ty;
                                 "amount"#:int_ty;
                               ]);
       "transferSavingResp"#:(record []);
       "initAccountReq"#:(record
                            [
                              "user"#:int_ty;
                              "checking"#:int_ty;
                              "saving"#:int_ty;
                            ]);
       "initAccountResp"#:(record []);
       "showCheckingReq"#:(record [ "user"#:int_ty ]);
       "showCheckingResp"#:(record [ "balance"#:int_ty ]);
       "showSavingReq"#:(record [ "user"#:int_ty ]);
       "showSavingResp"#:(record [ "balance"#:int_ty ]);
     ]
    @ event_typectx)

let gen name args body =
  mk_term_gen testCtx name (List.map (fun x -> VVar x) args) body

let obs name k = mk_term_obs_fresh testCtx name (fun _ -> k)

let obsBegin k =
  mk_term_obs_fresh testCtx "begin" (function
    | tid' :: _ -> k tid'
    | _ -> _die [%here])

let obsSelectName tid k =
  mk_term_obs_prop_fresh testCtx "selectName" (function
    | tid' :: _ ->
        let prop = lit_to_prop (mk_var_eq_var [%here] tid tid') in
        (prop, k)
    | _ -> _die [%here])

let obsUpdateName tid k =
  mk_term_obs_prop_fresh testCtx "updateName" (function
    | tid' :: _ ->
        let prop = lit_to_prop (mk_var_eq_var [%here] tid tid') in
        (prop, k)
    | _ -> _die [%here])

let obsSelectSaving tid k =
  mk_term_obs_prop_fresh testCtx "selectSaving" (function
    | tid' :: _ ->
        let prop = lit_to_prop (mk_var_eq_var [%here] tid tid') in
        (prop, k)
    | _ -> _die [%here])

let obsUpdateSaving tid k =
  mk_term_obs_prop_fresh testCtx "updateSaving" (function
    | tid' :: _ ->
        let prop = lit_to_prop (mk_var_eq_var [%here] tid tid') in
        (prop, k)
    | _ -> _die [%here])

let obsSelectChecking tid k =
  mk_term_obs_prop_fresh testCtx "selectChecking" (function
    | tid' :: _ ->
        let prop = lit_to_prop (mk_var_eq_var [%here] tid tid') in
        (prop, k)
    | _ -> _die [%here])

let obsUpdateChecking tid k =
  mk_term_obs_prop_fresh testCtx "updateChecking" (function
    | tid' :: _ ->
        let prop = lit_to_prop (mk_var_eq_var [%here] tid tid') in
        (prop, k)
    | _ -> _die [%here])

let obsCommit tid k =
  mk_term_obs_prop_fresh testCtx "commit" (function
    | tid' :: _ ->
        let prop = lit_to_prop (mk_var_eq_var [%here] tid tid') in
        (prop, k)
    | _ -> _die [%here])

let obsSelectCheckingResp e =
  mk_term_obs_fresh testCtx "selectCheckingResp" (fun _ -> e)

let obsSelectSavingResp e =
  mk_term_obs_fresh testCtx "selectSavingResp" (fun _ -> e)

let obsSelectNameResp e =
  mk_term_obs_fresh testCtx "selectNameResp" (fun _ -> e)

let obsTransferCheckingResp e =
  mk_term_obs_fresh testCtx "transferCheckingResp" (fun _ -> e)

let obsTransferSavingResp e =
  mk_term_obs_fresh testCtx "transferSavingResp" (fun _ -> e)

let obsInitAccountResp e =
  mk_term_obs_fresh testCtx "initAccountResp" (fun _ -> e)

let obsShowCheckingResp e =
  mk_term_obs_fresh testCtx "showCheckingResp" (fun _ -> e)

let obsShowSavingResp e =
  mk_term_obs_fresh testCtx "showSavingResp" (fun _ -> e)

let main =
  mk_term_assume_fresh_true int_ty (fun user1 ->
      mk_term_assume_fresh_true int_ty (fun user2 ->
          mk_term_assume_fresh int_ty
            (fun x -> lit_to_prop (mk_var_eq_c [%here] x (I 2)))
            (fun amount ->
              gen "transferCheckingReq" [ user1; user2; amount ]
              @@ obsBegin (fun tid1 ->
                     obsSelectChecking tid1 @@ obsSelectChecking tid1
                     @@ obsUpdateChecking tid1 @@ obsUpdateChecking tid1
                     @@ gen "transferCheckingReq" [ user2; user1; amount ]
                     @@ obsBegin (fun tid2 ->
                            obsSelectChecking tid2 @@ obsSelectChecking tid2
                            @@ obsUpdateChecking tid2 @@ obsUpdateChecking tid2
                            @@ obsCommit tid2 @@ obsTransferCheckingResp
                            @@ obsCommit tid1 @@ obsTransferCheckingResp
                            @@ gen "showCheckingReq" [ user1 ]
                            @@ obsBegin (fun tid3 ->
                                   obsSelectChecking tid3 @@ obsCommit tid3
                                   @@ obsShowCheckingResp mk_term_tt))))))

type smallbank_bench_config = { numUser : int; numOp : int }

let random_user { numUser; numOp } =
  let users = List.init numUser (fun i -> i + 1) in
  let random_transfer_checking () =
    let user1 = List.nth users (Random.int numUser) in
    let user2 = List.nth users (Random.int numUser) in
    let amount = Random.int 100 in
    send
      ( "transferCheckingReq",
        [ mk_value_int user1; mk_value_int user2; mk_value_int amount ] )
  in
  let random_transfer_saving () =
    let user1 = List.nth users (Random.int numUser) in
    let user2 = List.nth users (Random.int numUser) in
    let amount = Random.int 100 in
    send
      ( "transferSavingReq",
        [ mk_value_int user1; mk_value_int user2; mk_value_int amount ] )
  in
  let random_show () =
    let user = List.nth users (Random.int numUser) in
    send ("showCheckingReq", [ mk_value_int user ])
  in
  let random_init () =
    let user = List.nth users (Random.int numUser) in
    let checking = Random.int 100 in
    let saving = Random.int 100 in
    send
      ( "initAccountReq",
        [ mk_value_int user; mk_value_int checking; mk_value_int saving ] )
  in
  let rec genOp restNum =
    if restNum <= 0 then ()
    else
      let () = Pp.printf "@{<yellow>restNum@}: %i\n" restNum in
      (match Random.int 4 with
      | 0 -> random_transfer_checking ()
      | 1 -> random_transfer_saving ()
      | 2 -> random_show ()
      | _ -> random_init ());
      genOp (restNum - 1)
  in
  let () = genOp numOp in
  let () = Pp.printf "@{<red>End with numOp@}\n%i\n" numOp in
  Effect.perform End
