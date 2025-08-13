open Language
open Interpreter
open Common
module TwitterDB = ListDB
open TwitterDB

let followReqHandler (msg : msg) =
  let aux (user : int) =
    do_trans (fun tid ->
        let followings = do_read tid in
        let newFollowings = user :: followings in
        let _ = do_write tid newFollowings in
        ())
  in
  match msg.ev.args with
  | [ VConst (I user) ] ->
      let () = aux user in
      send ("followResp", [])
  | _ -> _die [%here]

let unfollowReqHandler (msg : msg) =
  let aux (user : int) =
    do_trans (fun tid ->
        let followings = do_read tid in
        let newFollowings = List.filter (fun x -> x != user) followings in
        let _ = do_write tid newFollowings in
        ())
  in
  match msg.ev.args with
  | [ VConst (I user) ] ->
      let () = aux user in
      send ("unfollowResp", [])
  | _ -> _die [%here]

let timelineReqHandler (msg : msg) =
  let aux (user : int) =
    do_trans (fun tid ->
        let followings = do_read tid in
        if List.mem user followings then do_get tid user else [])
  in
  match msg.ev.args with
  | [ VConst (I user) ] ->
      let content = aux user in
      send ("timelineResp", [ mk_value_intList content ])
  | _ -> _die [%here]

let followRespHandler (_ : msg) = ()
let unfollowRespHandler (_ : msg) = ()
let timelineRespHandler (_ : msg) = ()

let init isolation_level () =
  register_async_has_ret "begin" beginAsync;
  register_async_has_ret "commit" commitAsync;
  register_async_has_ret "get" getAsync;
  register_async_no_ret "put" putAsync;
  register_async_has_ret "read" readAsync;
  register_async_no_ret "write" writeAsync;
  register_handler "followReq" followReqHandler;
  register_handler "unfollowReq" unfollowReqHandler;
  register_handler "timelineReq" timelineReqHandler;
  register_handler "followResp" followRespHandler;
  register_handler "unfollowResp" unfollowRespHandler;
  register_handler "timelineResp" timelineRespHandler;
  TwitterDB.init isolation_level

open Nt

let record l = Ty_record { alias = None; fds = l }

let testCtx =
  Typectx.add_to_rights Typectx.emp
    ([
       "followReq"#:(record [ "x"#:int_ty ]);
       "followResp"#:(record []);
       "timelineReq"#:(record [ "x"#:int_ty ]);
       "timelineResp"#:(record []);
       "unfollowReq"#:(record [ "x"#:int_ty ]);
       "unfollowResp"#:(record []);
     ]
    @ event_typectx)

let gen name args body =
  mk_term_gen testCtx name (List.map (fun x -> VVar x) args) body

let obs name k = mk_term_obs_fresh testCtx name (fun _ -> k)
let obsFollowResp e = mk_term_obs_fresh testCtx "followResp" (fun _ -> e)
let obsTimelineResp e = mk_term_obs_fresh testCtx "timelineResp" (fun _ -> e)
let obsUnfollowResp e = mk_term_obs_fresh testCtx "unfollowResp" (fun _ -> e)

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

let obsRead tid k =
  mk_term_obs_prop_fresh testCtx "read" (function
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

let main =
  mk_term_assume_fresh_true int_ty (fun user ->
      gen "followReq" [ user ]
      @@ obsBegin (fun tid1 ->
             gen "unfollowReq" [ user ]
             @@ obsBegin (fun tid2 ->
                    obsRead tid2 @@ obsRead tid1 @@ obsWrite tid1
                    @@ obsWrite tid2 @@ obsCommit tid2 @@ obsCommit tid1
                    @@ obsFollowResp @@ obsUnfollowResp
                    @@ gen "followReq" [ user ]
                    @@ obsBegin (fun tid3 ->
                           obsRead tid3 @@ obsWrite tid3 @@ obsCommit tid3
                           @@ obsFollowResp mk_term_tt))))

type twitter_bench_config = { numUser : int; numOp : int }

let random_user { numUser; numOp } =
  let users = List.init numUser (fun i -> i + 1) in
  let random_follow () =
    let user = List.nth users (Random.int numUser) in
    send ("followReq", [ mk_value_int user ])
  in
  let random_unfollow () =
    let user = List.nth users (Random.int numUser) in
    send ("unfollowReq", [ mk_value_int user ])
  in
  let rec genOp restNum =
    if restNum <= 0 then ()
    else
      let () = Pp.printf "@{<yellow>restNum@}: %i\n" restNum in
      if Random.bool () then random_follow () else random_unfollow ();
      genOp (restNum - 1)
  in
  let () = genOp numOp in
  let () = Pp.printf "@{<red>End with numOp@}\n%i\n" numOp in
  Effect.perform End
