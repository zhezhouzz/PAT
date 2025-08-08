open Common
open Language
open Interpreter
open CartDB

let addItemReqHandler (msg : msg) =
  let aux (user : int) (item : int) =
    do_trans (fun tid ->
        let oldCart = do_get tid user in
        let newCart = item :: oldCart in
        do_put tid user newCart)
  in
  match msg.ev.args with
  | [ VConst (I user); VConst (I item) ] -> aux user item
  | _ -> _die [%here]

let deleteItemReqHandler (msg : msg) =
  let aux (user : int) (item : int) =
    do_trans (fun tid ->
        let oldCart = do_get tid user in
        let newCart = List.filter (fun x -> x <> item) oldCart in
        do_put tid user newCart)
  in
  match msg.ev.args with
  | [ VConst (I user); VConst (I item) ] -> aux user item
  | _ -> _die [%here]

let addItemRespHandler (_ : msg) = ()
let deleteItemRespHandler (_ : msg) = ()

let init () =
  Interpreter.init ();
  register_async_has_ret "begin" beginAsync;
  register_async_no_ret "commit" commitAsync;
  register_async_has_ret "get" getAsync;
  register_async_no_ret "put" putAsync;
  register_handler "addItemReq" addItemReqHandler;
  register_handler "deleteItemReq" deleteItemReqHandler;
  register_handler "addItemResp" addItemRespHandler;
  register_handler "deleteItemResp" deleteItemRespHandler;
  CartDB.init ReadCommitted

open Nt

let record l = Ty_record { alias = None; fds = l }

let testCtx =
  Typectx.add_to_rights Typectx.emp
    ([
       "addItemReq"#:(record [ "x"#:int_ty; "y"#:int_ty ]);
       "addItemResp"#:(record []);
       "deleteItemReq"#:(record [ "x"#:int_ty; "y"#:int_ty ]);
       "deleteItemResp"#:(record []);
     ]
    @ event_typectx)

let gen name args body =
  mk_term_gen testCtx name (List.map (fun x -> VVar x) args) body

let obs name k = mk_term_obs_fresh testCtx name (fun _ -> k)

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
  mk_term_assume_fresh int_ty mk_true (fun user ->
      mk_term_assume_fresh int_ty mk_true (fun item ->
          gen "addItemReq" [ user; item ]
          @@ gen "deleteItemReq" [ user; item ]
          @@ obsBegin (fun tid1 ->
                 obsBegin (fun tid2 ->
                     obsGet tid2 @@ obsGet tid1 @@ obsPut tid1 @@ obsPut tid2
                     @@ obsCommit tid2 @@ obsCommit tid1
                     @@ gen "deleteItemResp" [] @@ gen "addItemResp" []
                     @@ gen "addItemReq" [ user; item ]
                     @@ obsBegin (fun tid3 ->
                            obsGet tid3 @@ obsPut tid3 @@ obsCommit tid3
                            @@ gen "addItemResp" [] mk_term_tt)))))
