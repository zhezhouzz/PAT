open Language
open Interpreter

module type Config = sig
  val values_to_json : value list -> Yojson.Basic.t
  val json_to_values : Yojson.Basic.t -> value list
  val key_to_string : constant -> string
  val string_to_key : string -> constant
end

module MyDB (C : Config) = struct
  module DB = BackendMariaDB.MyMariaDB
  open C

  let beginAsync (ev : ev) =
    let tid = DB.raw_begin ~thread_id:!Runtime._curTid in
    { ev with args = [ mk_value_int tid ] }

  let commitAsync (ev : ev) =
    let tid =
      match ev.args with [ VConst (I tid) ] -> tid | _ -> _die [%here]
    in
    let cid = DB.raw_commit ~tid in
    { ev with args = [ mk_value_int tid; mk_value_int cid ] }

  let _getAsync table (ev : ev) =
    let tid, key =
      match ev.args with
      | [ VConst (I tid); VConst key ] -> (tid, key)
      | _ -> _die [%here]
    in
    let prev_tid, prev_cid, value =
      DB.raw_get ~tid ~table ~key:(key_to_string key)
    in
    {
      ev with
      args =
        [
          mk_value_int tid;
          VConst key;
          mk_value_int prev_tid;
          mk_value_int prev_cid;
        ]
        @ json_to_values value;
    }

  let _putAsync table (ev : ev) =
    let () =
      Printf.printf "start _putAsync %s : %s\n" table
        (Yojson.Basic.to_string (values_to_json ev.args))
    in
    let tid, key, v =
      match ev.args with
      | VConst (I tid) :: VConst key :: v -> (tid, key, v)
      | _ -> _die [%here]
    in
    let () =
      DB.raw_put ~tid ~table ~key:(key_to_string key) ~json:(values_to_json v)
    in
    ()

  let clear () = DB.raw_clear_db ()

  let check_isolation_level isolation_level _ =
    match isolation_level with
    | ReadCommitted -> DB.check_read_committed ()
    | Causal -> DB.check_causal ()
    | Serializable -> DB.check_serializable ()
    | ReadUncommitted -> true
end

let safe_to_basic_via_string (json : Yojson.Safe.t) : Yojson.Basic.t =
  Yojson.Safe.to_string json |> Yojson.Basic.from_string

let basic_to_safe_via_string (json : Yojson.Basic.t) : Yojson.Safe.t =
  Yojson.Basic.to_string json |> Yojson.Safe.from_string

module Config = struct
  let values_to_json vs =
    `List (List.map (fun x -> safe_to_basic_via_string @@ value_to_yojson x) vs)

  let json_to_values j =
    let js =
      Yojson.Basic.Util.to_list j
      |> List.map (fun x -> basic_to_safe_via_string x)
    in
    List.map
      (fun x ->
        match value_of_yojson x with Ok v -> v | Error _ -> _die [%here])
      js

  let key_to_string i = layout_constant i

  let string_to_key s =
    match int_of_string_opt s with Some i -> I i | None -> S s
end

module IntDB = struct
  include MyDB (Config)
end

module TreiberStack = struct
  include MyDB (Config)

  let do_get tid key =
    let msg = async ("get", [ mk_value_int tid; VConst key ]) in
    match msg.ev.args with
    | _ :: _ :: _ :: _ :: args -> args
    | _ -> _die [%here]

  let do_put tid key v = async ("put", [ mk_value_int tid; VConst key ] @ v)

  let do_trans f =
    let msg = async ("beginT", []) in
    let tid =
      match msg.ev.args with [ VConst (I tid) ] -> tid | _ -> _die [%here]
    in
    let res = f tid in
    let _ = async ("commit", [ mk_value_int tid ]) in
    res

  let getAsync (ev : ev) = _getAsync "stack" ev
  let putAsync (ev : ev) = _putAsync "stack" ev
  let readAsync (ev : ev) = _getAsync "cell" ev
  let writeAsync (ev : ev) = _putAsync "cell" ev
end






module CartDB = struct
  module D = MyDB (Config)
  include D

  let getAsync (ev : ev) = _getAsync "cart" ev
  let putAsync (ev : ev) = _putAsync "cart" ev

  let do_get tid key =
    let msg = async ("get", [ mk_value_int tid; mk_value_int key ]) in
    match msg.ev.args with
    | _ :: _ :: _ :: _ :: args -> args
    | _ -> _die [%here]

  let do_put tid key v =
    async ("put", [ mk_value_int tid; mk_value_int key ] @ v)

  let do_trans f =
    let msg = async ("beginT", []) in
    let tid =
      match msg.ev.args with [ VConst (I tid) ] -> tid | _ -> _die [%here]
    in
    let res = f tid in
    let _ = async ("commit", [ mk_value_int tid ]) in
    res



  let int_list_to_values l = [ VCIntList l ]

  let values_to_int_list l =
    match l with [ VCIntList l ] -> l | _ -> _die [%here]

  (* add item *)
  let async_add_item ~thread_id user item () =
    let open Lwt.Syntax in
    let* tid = DB.async_begin ~thread_id () in
    try
      let* _, _, oldCart =
        DB.async_get ~tid ~table:"cart" ~key:(string_of_int user) ()
      in
      let oldCart =
        match Config.json_to_values oldCart with
        | [ VCIntList l ] -> l
        | _ -> _die [%here]
      in
      let newCart =
        if List.mem item oldCart then oldCart else item :: oldCart
      in
      let* () =
        DB.async_put ~tid ~table:"cart" ~key:(string_of_int user)
          ~json:(Config.values_to_json [ VCIntList newCart ])
          ()
      in
      let* _ = DB.async_commit ~tid () in
      Lwt.return_unit
    with BackendMariaDB.DBKeyNotFound _ ->
      let* _ = DB.async_release_connection ~tid () in
      Lwt.return_unit

  let addItemReqHandler (msg : msg) =
    let aux (user : int) (item : int) =
      do_trans (fun tid ->
          let oldCart = values_to_int_list (do_get tid user) in
          let newCart =
            if List.mem item oldCart then oldCart else item :: oldCart
          in
          let _ = do_put tid user (int_list_to_values newCart) in
          ())
    in
    match msg.ev.args with
    | [ VConst (I user); VConst (I item) ] ->
        let _ = aux user item in
        send ("addItemResp", [])
    | _ -> _die [%here]

  (* delete item *)
  let async_delete_item ~thread_id user item () =
    let open Lwt.Syntax in
    let* tid = DB.async_begin ~thread_id () in
    try
      let* _, _, oldCart =
        DB.async_get ~tid ~table:"cart" ~key:(string_of_int user) ()
      in
      let oldCart =
        match Config.json_to_values oldCart with
        | [ VCIntList l ] -> l
        | _ -> _die [%here]
      in
      let newCart = List.filter (fun x -> x <> item) oldCart in
      let* () =
        DB.async_put ~tid ~table:"cart" ~key:(string_of_int user)
          ~json:(Config.values_to_json [ VCIntList newCart ])
          ()
      in
      let* _ = DB.async_commit ~tid () in
      Lwt.return_unit
    with BackendMariaDB.DBKeyNotFound _ ->
      let* _ = DB.async_release_connection ~tid () in
      Lwt.return_unit

  let deleteItemReqHandler (msg : msg) =
    let aux (user : int) (item : int) =
      do_trans (fun tid ->
          let oldCart = values_to_int_list (do_get tid user) in
          let newCart = List.filter (fun x -> x <> item) oldCart in
          let _ = do_put tid user (int_list_to_values newCart) in
          ())
    in
    match msg.ev.args with
    | [ VConst (I user); VConst (I item) ] ->
        let _ = aux user item in
        send ("deleteItemResp", [])
    | _ -> _die [%here]

  (* new user *)
  let async_new_user ~thread_id user () =
    let open Lwt.Syntax in
    let* tid = DB.async_begin ~thread_id () in
    try
      let* () =
        DB.async_put ~tid ~table:"cart" ~key:(string_of_int user)
          ~json:(Config.values_to_json [ VCIntList [] ])
          ()
      in
      let* _ = DB.async_commit ~tid () in
      Lwt.return_unit
    with BackendMariaDB.DBKeyNotFound _ ->
      let* _ = DB.async_release_connection ~tid () in
      Lwt.return_unit

  let newUserReqHandler (msg : msg) =
    let aux (user : int) =
      do_trans (fun tid ->
          let _ = do_put tid user (int_list_to_values []) in
          ())
    in
    match msg.ev.args with
    | [ VConst (I user) ] ->
        let _ = aux user in
        send ("newUserResp", [])
    | _ -> _die [%here]

  let addItemRespHandler (_ : msg) = ()
  let deleteItemRespHandler (_ : msg) = ()
  let newUserRespHandler (_ : msg) = ()

  let init () =
    register_async_has_ret "beginT" beginAsync;
    register_async_has_ret "commit" commitAsync;
    register_async_has_ret "get" getAsync;
    register_async_no_ret "put" putAsync;
    register_handler "addItemReq" addItemReqHandler;
    register_handler "deleteItemReq" deleteItemReqHandler;
    register_handler "addItemResp" addItemRespHandler;
    register_handler "deleteItemResp" deleteItemRespHandler;
    register_handler "newUserReq" newUserReqHandler;
    register_handler "newUserResp" newUserRespHandler;
    D.clear ()

  let testCtx =
    let open Nt in
    let record l = Ty_record { alias = None; fds = l } in
    Typectx.add_to_rights Typectx.emp
      [
        "beginT"#:(record [ "tid"#:int_ty ]);
        "commit"#:(record [ "tid"#:int_ty; "cid"#:int_ty ]);
        "put"#:(record
                  [ "tid"#:int_ty; "key"#:int_ty; "value"#:(mk_list_ty int_ty) ]);
        "get"#:(record
                  [
                    "tid"#:int_ty;
                    "key"#:int_ty;
                    "prev_tid"#:int_ty;
                    "prev_cid"#:int_ty;
                    "value"#:(mk_list_ty int_ty);
                  ]);
        "newUserReq"#:(record [ "user"#:int_ty ]);
        "newUserResp"#:(record []);
        "addItemReq"#:(record [ "user"#:int_ty; "item"#:int_ty ]);
        "addItemResp"#:(record []);
        "deleteItemReq"#:(record [ "user"#:int_ty; "item"#:int_ty ]);
        "deleteItemResp"#:(record []);
      ]
end














module TwitterDB = struct
  (* twitter db operations: get the follow list
                            update the follow list (this pair suffices for follow and unfollow)
                            tweet
                            get tweets for a user (timeline, newsfeed) *)

  module D = MyDB (Config)
  include D

  let selectFollowsAsync (ev : ev) = _getAsync "follows" ev
  let selectTweetsAsync (ev : ev) = _getAsync "tweets" ev
  let updateFollowsAsync (ev : ev) = _putAsync "follows" ev
  let updateTweetsAsync (ev : ev) = _putAsync "tweets" ev

  let do_selectFollows tid user =
    let msg = async ("selectFollows", [ mk_value_int tid; mk_value_int user ]) in
    match msg.ev.args with
    | _ :: _ :: _ :: _ :: args -> args
    | _ -> _die [%here]

  let do_selectTweets tid user =
    let msg = async ("selectTweets", [ mk_value_int tid; mk_value_int user ]) in
    match msg.ev.args with
    | _ :: _ :: _ :: _ :: args -> args
    | _ -> _die [%here]

  let do_updateFollows tid user v =
    async ("updateFollows", [ mk_value_int tid; mk_value_int user ] @ v)

  let do_updateTweets tid user v = 
    async ("updateTweets", [ mk_value_int tid; mk_value_int user ] @ v)

  let do_trans f =
    let msg = async ("beginT", []) in
    let tid =
      match msg.ev.args with [ VConst (I tid) ] -> tid | _ -> _die [%here]
    in
    let res = f tid in
    let _ = async ("commit", [ mk_value_int tid ]) in
    res

  let int_list_to_values l = [ VCIntList l ]

  let values_to_int_list l =
    match l with [ VCIntList l ] -> l | _ -> _die [%here]

  (* new user / login *)
  let async_new_user ~thread_id user () =
    let open Lwt.Syntax in
    let* tid = DB.async_begin ~thread_id () in
    try
      let* () =
        DB.async_put ~tid ~table:"follows" ~key:(string_of_int user)
          ~json:(Config.values_to_json [ VCIntList [] ])
          ()
      in
      let* () =
        DB.async_put ~tid ~table:"tweets" ~key:(string_of_int user)
          ~json:(Config.values_to_json [ VCIntList [] ])
          ()
      in
      let* _ = DB.async_commit ~tid () in
      Lwt.return_unit
    with BackendMariaDB.DBKeyNotFound _ ->
      let* _ = DB.async_release_connection ~tid () in
      Lwt.return_unit

  let newUserReqHandler (msg : msg) =
    let aux (user : int) =
      do_trans (fun tid ->
          let _ = do_updateFollows tid user (int_list_to_values []) in
          let _ = do_updateTweets tid user (int_list_to_values []) in
          ())
    in
    match msg.ev.args with
    | [ VConst (I user) ] ->
        let _ = aux user in
        send ("newUserResp", [])
    | _ -> _die [%here]

  (* following a user *)
  let async_follow ~thread_id user follow_o () =
    let open Lwt.Syntax in
    let* tid = DB.async_begin ~thread_id () in
    try
      let* _, _, oldFollows = 
        DB.async_get ~tid ~table:"follows" ~key:(string_of_int user) ()
      in
      let oldFollows = 
        match Config.json_to_values oldFollows with
        | [ VCIntList l ] -> l
        | _ -> _die [%here]
      in
      let newFollows = 
        if List.mem follow_o oldFollows then oldFollows else follow_o :: oldFollows (*TODO: is it bad to stop the transaction?*)
      in
      let* () = 
        DB.async_put ~tid ~table:"follows" ~key:(string_of_int user)
          ~json:(Config.values_to_json [ VCIntList newFollows ])
          ()
      in
      let* _ = DB.async_commit ~tid () in
      Lwt.return_unit
    with BackendMariaDB.DBKeyNotFound _ ->
      let* _ = DB.async_release_connection ~tid () in
      Lwt.return_unit

  let followReqHandler (msg : msg) = 
    let aux (user : int) (follow_o : int) =
      do_trans (fun tid ->
          let oldFollows = values_to_int_list (do_selectFollows tid user) in
          let newFollows = if List.mem follow_o oldFollows then oldFollows else follow_o :: oldFollows
          in
          let _ = do_updateFollows tid user (int_list_to_values newFollows) in
          ())
      in
      match msg.ev.args with
      | [ VConst (I user); VConst (I follow_o) ] ->
          let _ = aux user follow_o in
          send ("followResp", [])
      | _ -> _die [%here]


  (* unfollowing a user *)
  let async_unfollow ~thread_id user unfollow_o () =
    let open Lwt.Syntax in
    let* tid = DB.async_begin ~thread_id () in
    try
      let* _, _, oldFollows = 
        DB.async_get ~tid ~table:"follows" ~key:(string_of_int user) ()
      in
      let oldFollows = 
        match Config.json_to_values oldFollows with
        | [ VCIntList l ] -> l
        | _ -> _die [%here]
      in
      let newFollows = 
        List.filter (fun x -> x <> unfollow_o) oldFollows
      in
      let* () = 
        DB.async_put ~tid ~table:"follows" ~key:(string_of_int user)
          ~json:(Config.values_to_json [ VCIntList newFollows ])
          ()
      in
      let* _ = DB.async_commit ~tid () in
      Lwt.return_unit
    with BackendMariaDB.DBKeyNotFound _ ->
      let* _ = DB.async_release_connection ~tid () in
      Lwt.return_unit

  let unfollowReqHandler (msg : msg) = 
    let aux (user : int) (unfollow_o : int) =
      do_trans (fun tid ->
          let oldFollows = values_to_int_list (do_selectFollows tid user) in
          let newFollows = List.filter (fun x -> x <> unfollow_o) oldFollows
          in
          let _ = do_updateFollows tid user (int_list_to_values newFollows) in
          ())
      in
      match msg.ev.args with
      | [ VConst (I user); VConst (I unfollow_o) ] ->
          let _ = aux user unfollow_o in
          send ("unfollowResp", [])
      | _ -> _die [%here]


  (* posting new tweets *)
  let async_post_tweet ~thread_id user tweet () =
    let open Lwt.Syntax in 
    let* tid = DB.async_begin ~thread_id () in
    try
      let* _, _, oldTweets = 
        DB.async_get ~tid ~table:"tweets" ~key:(string_of_int user) ()
      in
      let oldTweets = 
        match Config.json_to_values oldTweets with
        | [ VCIntList l ] -> l
        | _ -> _die [%here]
      in
      let newTweets =
        tweet :: oldTweets
      in
      let* () = 
        DB.async_put ~tid ~table:"tweets" ~key:(string_of_int user)
          ~json:(Config.values_to_json [ VCIntList newTweets ])
          ()
      in
      let* _ = DB.async_commit ~tid () in 
      Lwt.return_unit
    with BackendMariaDB.DBKeyNotFound _ ->
      let* _ = DB.async_release_connection ~tid () in
      Lwt.return_unit

  let postTweetReqHandler (msg : msg) =
    let aux (user : int) (tweet : int) =
      do_trans (fun tid ->
          let oldTweets = values_to_int_list (do_selectTweets tid user) in
          let newTweets = tweet :: oldTweets
          in 
          let _ = do_updateTweets tid user (int_list_to_values newTweets) in
          ())
    in
    match msg.ev.args with
    | [ VConst (I user); VConst (I item) ] ->
        let _ = aux user item in
        send ("postTweetResp", [])
    | _ -> _die [%here]


  let timelineReqHandler (msg : msg) = 
    let aux (user : int) =
      do_trans (fun tid ->
          let tweets = values_to_int_list (do_selectTweets tid user) in
          tweets)
    in
    match msg.ev.args with
    | [ VConst (I user) ] ->
        let tweets = aux user in
        send ("timelineResp", [ mk_value_intList tweets ])
    | _ -> _die [%here]


  (* get timeline *)
  let async_timeline ~thread_id user () = 
    let open Lwt.Syntax in
    let* tid = DB.async_begin ~thread_id () in
    try
      let* _, _, tweets = 
        DB.async_get ~tid ~table:"tweets" ~key:(string_of_int user) ()
      in
      Lwt.return tweets
    with BackendMariaDB.DBKeyNotFound s ->
      let* _ = DB.async_release_connection ~tid () in
      Lwt.fail (BackendMariaDB.DBKeyNotFound s)


  let newUserRespHandler (_ : msg) = ()
  let followRespHandler (_ : msg) = ()
  let unfollowRespHandler (_ : msg) = ()
  let postTweetRespHandler (_ : msg) = ()
  let timelineRespHandler (_ : msg) = ()

  let init () =
    register_async_has_ret "beginT" beginAsync;
    register_async_has_ret "commit" commitAsync;
    register_async_has_ret "selectFollows" selectFollowsAsync;
    register_async_no_ret "updateFollows" updateFollowsAsync;
    register_async_has_ret "selectTweets" selectTweetsAsync;
    register_async_no_ret "updateTweets" updateTweetsAsync;
    register_handler "newUserReq" newUserReqHandler;
    register_handler "followReq" followReqHandler;
    register_handler "unfollowReq" unfollowReqHandler;
    register_handler "postTweetReq" postTweetReqHandler;
    register_handler "timelineReq" timelineReqHandler;
    register_handler "newUserResp" newUserRespHandler;
    register_handler "followResp" followRespHandler;
    register_handler "unfollowResp" unfollowRespHandler;
    register_handler "postTweetResp" postTweetRespHandler;
    register_handler "timelineResp" timelineRespHandler;
    D.clear ()

  let testCtx =
    let open Nt in
    let record l = Ty_record { alias = None; fds = l } in
    Typectx.add_to_rights Typectx.emp
      [
        "beginT"#:(record [ "tid"#:int_ty ]);
        "commit"#:(record [ "tid"#:int_ty; "cid"#:int_ty ]);
        "selectFollows"#:(record
                            [
                              "tid"#:int_ty;
                              "key"#:int_ty;
                              "prev_tid"#:int_ty;
                              "prev_cid"#:int_ty;
                              "value"#:(mk_list_ty int_ty);
                            ]);
        "updateFollows"#:(record
                            [ "tid"#:int_ty; "key"#:int_ty; "value"#:(mk_list_ty int_ty) ]);
        "selectTweets"#:(record
                            [
                              "tid"#:int_ty;
                              "key"#:int_ty;
                              "prev_tid"#:int_ty;
                              "prev_cid"#:int_ty;
                              "value"#:(mk_list_ty int_ty);
                            ]);
        "updateTweets"#:(record
                            [ "tid"#:int_ty; "key"#:int_ty; "value"#:(mk_list_ty int_ty) ]);
        "newUserReq"#:(record [ "user"#:int_ty ]);
        "newUserResp"#:(record []);
        "followReq"#:(record [ "user"#:int_ty; "follow_o"#:int_ty ]);
        "followResp"#:(record []);
        "unfollowReq"#:(record [ "user"#:int_ty; "unfollow_o"#:int_ty ]);
        "unfollowResp"#:(record []);
        "postTweetReq"#:(record [ "user"#:int_ty; "tweet"#:int_ty ]);
        "postTweetResp"#:(record []);
        "timelineReq"#:(record [ "user"#:int_ty ]);
        "timelineResp"#:(record [ "tweets"#:(mk_list_ty int_ty) ]);
      ]
end