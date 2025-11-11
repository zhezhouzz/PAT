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


  let _getAsyncTable db table (ev : ev) =
    let tid, key =
      match ev.args with
      | [ VConst (I tid); VConst key ] -> (tid, key)
      | _ -> _die [%here]
    in
    let prev_tid, prev_cid, value =
      DB.table_raw_get ~tid ~db ~table ~key:(key_to_string key)
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

  let _putAsyncTable db table (ev : ev) =
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
      DB.table_raw_put ~tid ~db ~table ~key:(key_to_string key) ~json:(values_to_json v)
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




module TreiberStackDB = struct
  (* Tstack transactions: Just CAS? No. Add cell, remove cell (bimyou), CAS *)
  (*
     Top
     Push - uses compare and swap
     Pop - uses compare and swap
  *)

  module D = MyDB (Config)
  include D

  let db_str = "stack"

  let topKey = -1
  let emptyVal = -1
  (*let _opCount = ref 0*)

  let getAsync (ev : ev) = _getAsync db_str ev
  let putAsync (ev : ev) = _putAsync db_str ev

  let do_get tid key = 
    let msg = (*if key == topKey
      then async ("getTop", [ mk_value_int tid; mk_value_int key ])
      else*) async ("get", [ mk_value_int tid; mk_value_int key ])
    in
    match msg.ev.args with
    | _ :: _ :: _ :: _ :: [ VConst (I value) ; VConst (I next) ] -> (value, next)
    | _ -> _die [%here]

  let do_put tid key value next =
    (*if key == topKey
      then async ("putTop", [ mk_value_int tid; mk_value_int key ; mk_value_int value; mk_value_int next])
      else *)async ("put", [ mk_value_int tid; mk_value_int key ; mk_value_int value; mk_value_int next ])

  let do_trans f =
    let msg = async ("beginT", []) in
    let tid =
      match msg.ev.args with [ VConst (I tid) ] -> tid | _ -> _die [%here]
    in
    let res = f tid in
    let _ = async ("commit", [ mk_value_int tid ]) in
    (*let () = _opCount := !_opCount + 1 *)
    res

  let async_cas tid old_head new_head =
    let open Lwt.Syntax in
    let* _, _, key =
      DB.async_get ~tid ~table:db_str ~key:(string_of_int topKey) ()
    in
    let key =
      match Config.json_to_values key with
        | [ VConst (I _) ; VConst (I next) ] -> next
        | _ -> _die [%here]
    in
    if key == old_head then
      let* () = DB.async_put ~tid ~table:db_str ~key:(string_of_int topKey)
               ~json:(Config.values_to_json [ VConst (I emptyVal) ; VConst (I new_head)]) ()
      in Lwt.return_true
    else Lwt.return_false


  let do_cas tid old_head new_head =
      let key = do_get tid topKey in
      let key =
        match key with
        | (_ , next) -> next
      in
      if key == old_head then
        let _ = do_put tid topKey emptyVal new_head in
        true
      else let _ = (Printf.printf "key: 
      %d\n old_head: %d\nfailed CAS\n" key old_head) in false

  (* init *)
  let async_init ~thread_id () =
    let open Lwt.Syntax in
    let* tid = DB.async_begin ~thread_id () in
    try
      let* () =
        DB.async_put ~tid ~table:db_str ~key:(string_of_int topKey)
        ~json:(Config.values_to_json [ VConst (I emptyVal) ; VConst (I tid)]) ()
      in
      let* _ = DB.async_commit ~tid () in
      Lwt.return_unit
    with BackendMariaDB.DBKeyNotFound _ ->
      let* _ = DB.async_release_connection ~tid () in
      Lwt.return_unit

  let initReqHandler (msg : msg) =
    let aux () =
      do_trans (fun tid ->
          let _ = do_put tid topKey emptyVal tid in
          let _ = Printf.printf "INIT REQ\n" in
          ())
    in
    match msg.ev.args with
    | [] -> let _ = aux () in
            send ("initResp", [])
    | _ -> _die [%here]

  (* push *) 
  let async_push ~thread_id element () = 
    let open Lwt.Syntax in
    let* tid = DB.async_begin ~thread_id () in
    let rec try_cas () =
      let fail_cas () = try_cas () in
      let pass_cas () = let* _ = DB.async_commit ~tid () in
                        Lwt.return_unit in
      let* _, _, old_head =
          DB.async_get ~tid ~table:db_str ~key:(string_of_int topKey) ()
        in
        let old_head =
          match Config.json_to_values old_head with
            | [ VConst (I _) ; VConst (I next) ] -> next
            | _ -> _die [%here]
        in
        let* () =
          DB.async_put ~tid ~table:db_str ~key:(string_of_int tid)
          ~json:(Config.values_to_json [ VConst (I element) ; VConst (I old_head)]) ()
        in
        let* cas = async_cas tid old_head tid in
        if cas then pass_cas () else fail_cas ()
    in
    try
      try_cas ()
    with BackendMariaDB.DBKeyNotFound _ ->
      let* _ = DB.async_release_connection ~tid () in
      Lwt.return_unit


  let pushReqHandler (msg : msg) = 
    let aux (element : int) =
      let rec try_cas (tid : int) = 
        let fail_cas (tid : int) = try_cas (tid) in
        let pass_cas () = () in
        let old_head =
          match do_get tid topKey with
          | (_ , next) -> next
        in
        let _ = do_put tid tid element old_head in
        if (do_cas tid old_head tid)
          then 
            let _ = send ("passCAS", [ mk_value_int tid ; mk_value_int old_head ; mk_value_int tid ])
              in pass_cas ()
          else
            let _ = send ("failCAS", [ mk_value_int tid ; mk_value_int old_head ; mk_value_int tid ])
              in fail_cas tid
      in
      do_trans (fun tid ->
        try_cas (tid))
    in
    match msg.ev.args with
    | [ VConst (I v) ] ->
        let _ = aux v in
        send ("pushResp", [])
    | _ -> _die [%here]

  (* pop *)
  let async_pop ~thread_id () =
    let open Lwt.Syntax in
    let* tid = DB.async_begin ~thread_id () in
    let rec try_cas () =
      let fail_cas () = try_cas () in
      let pass_cas (v : int) = let* _ = DB.async_commit ~tid () in
                        Lwt.return v in
      let* _, _, old_head =
        DB.async_get ~tid ~table:db_str ~key:(string_of_int topKey) ()
      in
      let old_head =
        match Config.json_to_values old_head with
          | [ VConst (I _) ; VConst (I next) ] -> next
          | _ -> _die [%here]
      in
      let* _, _, old_head_value =
        DB.async_get ~tid ~table:db_str ~key:(string_of_int old_head) ()
      in
      let top_value, new_head =
        match Config.json_to_values old_head_value with
          | [ VConst (I v) ; VConst (I next) ] -> v, next
          | _ -> _die [%here]
      in
      let* cas = async_cas tid old_head new_head in
      if cas then pass_cas (top_value) else fail_cas ()
    in
    try
      try_cas ()
    with BackendMariaDB.DBKeyNotFound s ->
      let* _ = DB.async_release_connection ~tid () in
      Lwt.fail (BackendMariaDB.DBKeyNotFound s)

  let popReqHandler (msg : msg) =

    let aux () =
      let rec try_cas (tid : int) = 
        let fail_cas (tid : int) = let _ = send ("failCAS", []) in
                                  try_cas (tid) in
        let pass_cas (v : int) = 
                                 v in
        let old_head =
          match do_get tid topKey with
          | (_, next) -> next
        in
        let old_head_value, new_head = match do_get tid old_head with
          | (v, next) -> v, next
        in
        if (do_cas tid old_head new_head)
          then 
            (*let _ = send ("passCAS", [ mk_value_int tid ; mk_value_int old_head ; mk_value_int new_head ])
              in*) pass_cas old_head_value
          else
            (*let _ = send ("failCAS", [ mk_value_int tid ; mk_value_int old_head ; mk_value_int new_head ])
              in*) fail_cas tid
      in
      do_trans (fun tid ->
        try_cas (tid))
    in
    match msg.ev.args with
    | [] -> let v = aux () in
            send ("popResp", [ mk_value_int v ])
    | _ -> _die [%here]
        

  let initRespHandler (_ : msg) = ()
  let pushRespHandler (_ : msg) = ()
  let popRespHandler (_ : msg) = ()
  let passCASHandler (_ : msg) = ()
  let failCASHandler (_ : msg) = ()
  
  let init () =
    register_async_has_ret "beginT" beginAsync;
    register_async_has_ret "commit" commitAsync;
    register_async_has_ret "getTop" getAsync;
    register_async_no_ret "putTop" putAsync;
    register_async_has_ret "get" getAsync;
    register_async_no_ret "put" putAsync;
    register_handler "initReq" initReqHandler;
    register_handler "initResp" initRespHandler;
    (*register_async_has_ret "CAS" async_cas;*)
    register_handler "passCAS" passCASHandler;
    register_handler "failCAS" failCASHandler;
    register_handler "pushReq" pushReqHandler;
    register_handler "popReq" popReqHandler;
    register_handler "pushResp" pushRespHandler;
    register_handler "popResp" popRespHandler;
    D.clear()

  let testCtx =
    let open Nt in
    let record l = Ty_record { alias = None; fds = l } in
    Typectx.add_to_rights Typectx.emp
      [
        "beginT"#:(record [ "tid"#:int_ty ]);
        "commit"#:(record [ "tid"#:int_ty; "cid"#:int_ty ]);
        "put"#:(record
                  [ "tid"#:int_ty; "key"#:int_ty; "value"#:int_ty; "next"#:int_ty ]);
        "get"#:(record
                  [
                    "tid"#:int_ty;
                    "key"#:int_ty;
                    "prev_tid"#:int_ty;
                    "prev_cid"#:int_ty;
                    "value"#:int_ty;
                  ]);
        "passCAS"#:(record
                  [ "tid"#:int_ty; "old_head"#:int_ty; "new_head"#:int_ty ]);
        "failCAS"#:(record
                  [ "tid"#:int_ty; "old_head"#:int_ty; "new_head"#:int_ty ]);
        "initReq"#:(record []);
        "initResp"#:(record []);
        "pushReq"#:(record [ "element"#:int_ty ]);
        "pushResp"#:(record []);
        "popReq"#:(record []);
        "popResp"#:(record [ "element"#:int_ty ]);
      ]



end



module SmallBankDB = struct

  (* SmallBank todo:
     * how to use strings instead of ints, so that we can use customer name instead of customer id? 
          Should we just make sure every new customer has a different name and id, but make them both numbers?
     * incorporate more checks; we did the one for write check, but we haven't done the ones for amalgamate and send payment
     * write the spec, obviously
     * create a new accounts function (to populate the database)
  *)

  module D = MyDB (Config)
  include D

  let db_str = "smallbank"
  let acc_str = "accounts"
  let sav_str = "savings"
  let chk_str = "checking"


  let async_get ~tid ~table ~key () =
    let _ = Printf.printf "in async get\n" in
    DB.table_async_get ~tid ~db:db_str ~table ~key ()
  let async_put ~tid ~table ~key ~json () =
    let _ = Printf.printf "in async put\n" in
    DB.table_async_put ~tid ~db:db_str ~table ~key ~json ()



  let selectAccountsAsync (ev : ev) = _getAsyncTable db_str acc_str ev
  let selectSavingsAsync (ev : ev) = _getAsyncTable db_str sav_str ev
  let selectCheckingAsync (ev : ev) = _getAsyncTable db_str chk_str ev
  let updateAccountsAsync (ev : ev) = _putAsyncTable db_str acc_str ev
  let updateSavingsAsync (ev : ev) = _putAsyncTable db_str sav_str ev
  let updateCheckingAsync (ev : ev) = _putAsyncTable db_str chk_str ev

  let do_selectAccounts tid name = 
    let msg = async ("selectAccounts", [ mk_value_int tid; mk_value_int name ]) in
    match msg.ev.args with
    | _ :: _ :: _ :: _ :: [ VConst (I v) ] -> v
    | _ -> _die [%here]

  let do_selectSavings tid custid = 
    let msg = async ("selectSavings", [ mk_value_int tid; mk_value_int custid ]) in
    match msg.ev.args with
    | _ :: _ :: _ :: _ :: [ VConst (I v) ] -> v
    | _ -> _die [%here]

  let do_selectChecking tid custid = 
    let msg = async ("selectChecking", [ mk_value_int tid; mk_value_int custid ]) in
    match msg.ev.args with
    | _ :: _ :: _ :: _ :: [ VConst (I v) ] -> v
    | _ -> _die [%here]

  let do_updateAccounts tid name custid =
    async ("updateAccounts", [ mk_value_int tid; mk_value_int name ; mk_value_int custid])

  let do_updateSavings tid custid bal =
    async ("updateSavings", [ mk_value_int tid; mk_value_int custid ; mk_value_int bal])

  let do_updateChecking tid custid bal =
    async ("updateChecking", [ mk_value_int tid; mk_value_int custid ; mk_value_int bal])

  let do_trans f =
    let msg = async ("beginT", []) in
    let tid =
      match msg.ev.args with [ VConst (I tid) ] -> tid | _ -> _die [%here]
    in
    let res = f tid in
    let _ = Pp.printf "calculated result\n" in
    let _ = async ("commit", [ mk_value_int tid ]) in
    let _ = Pp.printf "return from commit\n" in
    res

  (* open accounts *)
  let async_openAccounts ~thread_id name custid () =
    let open Lwt.Syntax in
    let* tid = DB.async_begin ~thread_id () in
    try
      let* () = 
        async_put ~tid ~table:acc_str ~key:(string_of_int name)
        ~json:(Config.values_to_json [ VConst (I (custid))]) ()
      in
      let* () =
        async_put ~tid ~table:sav_str ~key:(string_of_int custid)
        ~json:(Config.values_to_json [ VConst (I 0)]) ()     
      in
      let* () =
        async_put ~tid ~table:chk_str ~key:(string_of_int custid)
        ~json:(Config.values_to_json [ VConst (I 0)]) ()  
      in
      let* _ = DB.async_commit ~tid () in
      Lwt.return_unit
    with BackendMariaDB.DBKeyNotFound _ ->
      let* _ = DB.async_release_connection ~tid () in
      Lwt.return_unit

  let openAccountsReqHandler (msg : msg) =
    let aux (name : int) (custid : int) =
      do_trans (fun tid ->
          let _ = do_updateAccounts tid name custid in
          let _ = do_updateSavings tid custid 0 in
          let _ = do_updateChecking tid custid 0 in
          ())
    in
    match msg.ev.args with
    | [ VConst (I custid); VConst (I name) ] ->
        let _ = aux custid name in
        send ("openAccountsResp", [])
    | _ -> _die [%here]

  (* amalgamate *)
  let async_amalgamate ~thread_id custid0 custid1 () =
    let open Lwt.Syntax in
    let* tid = DB.async_begin ~thread_id () in
    try
      let* _, _, sBal0 =
        async_get ~tid ~table:sav_str ~key:(string_of_int custid0) ()
      in
      let sBal0 =
        match Config.json_to_values sBal0 with 
          | [ VConst (I v) ] -> v
          | _ -> _die [%here]
      in
      let* _, _, cBal0 =
        async_get ~tid ~table:chk_str ~key:(string_of_int custid0) ()
      in
      let cBal0 =
        match Config.json_to_values cBal0 with
          | [ VConst (I v) ] -> v
          | _ -> _die [%here]
      in
      let* _, _, cBal1 =
        async_get ~tid ~table:chk_str ~key:(string_of_int custid1) ()
      in
      let cBal1 =
        match Config.json_to_values cBal1 with
        | [ VConst (I v) ] -> v
        | _ -> _die [%here]
      in
      let* () =
        async_put ~tid ~table:sav_str ~key:(string_of_int custid0)
        ~json:(Config.values_to_json [ VConst (I 0) ]) ()
      in
      let* () =
        async_put ~tid ~table:chk_str ~key:(string_of_int custid0)
        ~json:(Config.values_to_json [ VConst (I 0) ]) ()
      in
      let* () =
        async_put ~tid ~table:chk_str ~key:(string_of_int custid1)
        ~json:(Config.values_to_json [ VConst (I (sBal0 + cBal0 + cBal1))]) ()
      in
      let* _ = DB.async_commit ~tid () in
      Lwt.return_unit
    with BackendMariaDB.DBKeyNotFound _ ->
      let* _ = DB.async_release_connection ~tid () in
      Lwt.return_unit

  let amalgamateReqHandler (msg : msg) =
    let aux (custid0 : int) (custid1 : int) =
      do_trans (fun tid ->
          let sBal0 = do_selectSavings tid custid0 in
          let cBal0 = do_selectChecking tid custid0 in
          let cBal1 = do_selectChecking tid custid1 in
          let _ = do_updateSavings tid custid0 0 in
          let _ = do_updateChecking tid custid0 0 in
          let total = (sBal0 + cBal0 + cBal1) in
          let _ = do_updateChecking tid custid1 total in
          ())
    in
    match msg.ev.args with
    | [ VConst (I custid0); VConst (I custid1) ] ->
        let _ = aux custid0 custid1 in
        send ("amalgamateResp", [])
    | _ -> _die [%here]

  (* balance *)
  let async_balance ~thread_id name () =
    let open Lwt.Syntax in
    let* tid = DB.async_begin ~thread_id () in
    try
      let* _, _, custid = 
        async_get ~tid ~table:acc_str ~key:(string_of_int name) ()
      in
      let custid =
        match Config.json_to_values custid with
        | [ VConst (I v) ] -> v
        | _ -> _die [%here]
      in
      let* _, _, sBal = 
        async_get ~tid ~table:sav_str ~key:(string_of_int custid) ()
      in
      let sBal =
        match Config.json_to_values sBal with
        | [ VConst (I v) ] -> v
        | _ -> _die [%here]
      in
      let* _, _, cBal = 
        async_get ~tid ~table:chk_str ~key:(string_of_int custid) ()
      in
      let cBal =
        match Config.json_to_values cBal with
        | [ VConst (I v) ] -> v
        | _ -> _die [%here]
      in
      Lwt.return (sBal + cBal)
    with BackendMariaDB.DBKeyNotFound s ->
      let* _ = DB.async_release_connection ~tid () in
      Lwt.fail (BackendMariaDB.DBKeyNotFound s)

  let balanceReqHandler (msg : msg) =
    let _ = Pp.printf ("running balance req handler\n") in
    let aux (name : int) =
      do_trans (fun tid ->
          let custid = do_selectAccounts tid name in
          let _ = Pp.printf "custid: %d\n" custid in
          let sBal = do_selectSavings tid custid in
          let _ = Pp.printf "sBal: %d\n" sBal in
          let cBal = do_selectChecking tid custid in
          let _ = Pp.printf "cBal: %d\n" cBal in
          (sBal + cBal))
    in
    match msg.ev.args with
    | [ VConst (I name) ] ->
        let balance = aux name in
        let _ = Pp.printf ("sending balance resp\n") in
        send ("balanceResp", [ mk_value_int balance ])
    | _ -> _die [%here]

  (* deposit checking *)
  let async_depositChecking ~thread_id name amount () =
    let open Lwt.Syntax in
    let* tid = DB.async_begin ~thread_id () in
    try
      let* _, _, custid =
        async_get ~tid ~table:acc_str ~key:(string_of_int name) ()
      in
      let custid =
        match Config.json_to_values custid with
        | [ VConst (I v) ] -> v
        | _ -> _die [%here]
      in
      let* _, _, cBal =
        async_get ~tid ~table:chk_str ~key:(string_of_int custid) ()
      in
      let cBal =
        match Config.json_to_values cBal with
        | [ VConst (I v) ] -> v
        | _ -> _die [%here]
      in
      let* () =
        async_put ~tid ~table:chk_str ~key:(string_of_int custid)
        ~json:(Config.values_to_json [ VConst (I (cBal + amount))]) ()
      in
      let* _ = DB.async_commit ~tid () in
      Lwt.return_unit
    with BackendMariaDB.DBKeyNotFound _ ->
      let* _ = DB.async_release_connection ~tid () in
      Lwt.return_unit

  let depositCheckingReqHandler (msg : msg) =
    let aux (name : int) (amount: int) =
      do_trans (fun tid ->
          let custid = do_selectAccounts tid name in
          let cBal = do_selectChecking tid custid in
          let _ = do_updateChecking tid custid (cBal + amount) in
          ())
    in
    match msg.ev.args with
    | [ VConst (I name); VConst (I amount) ] ->
        let _ = aux name amount in
        send ("depositCheckingResp", [])
    | _ -> _die [%here]

  (* send payment *)
  let async_sendPayment ~thread_id srcid destid amount () =
    let open Lwt.Syntax in
    let* tid = DB.async_begin ~thread_id () in
    try
      let* _, _, cBalSrc =
        async_get ~tid ~table:chk_str ~key:(string_of_int srcid) ()
      in
      let cBalSrc =
        match Config.json_to_values cBalSrc with
        | [ VConst (I v) ] -> v
        | _ -> _die [%here]
      in
      let* _, _, cBalDest =
        async_get ~tid ~table:chk_str ~key:(string_of_int destid) ()
      in
      let cBalDest = 
        match Config.json_to_values cBalDest with
        | [ VConst (I v) ] -> v
        | _ -> _die [%here]
      in
      let* () =
        async_put ~tid ~table:chk_str ~key:(string_of_int srcid)
        ~json:(Config.values_to_json [ VConst (I (cBalSrc - amount))]) ()
      in
      let* () =
        async_put ~tid ~table:chk_str ~key:(string_of_int destid)
        ~json:(Config.values_to_json [ VConst (I (cBalDest + amount))]) ()
      in
      let* _ = DB.async_commit ~tid () in
      Lwt.return_unit
    with BackendMariaDB.DBKeyNotFound _ ->
      let* _ = DB.async_release_connection ~tid () in
      Lwt.return_unit
  
  let sendPaymentReqHandler (msg : msg) =
    let aux (srcid : int) (destid : int) (amount : int) =
      do_trans (fun tid ->
          let cBalSrc = do_selectChecking tid srcid in
          let cBalDest = do_selectChecking tid destid in
          let _ = do_updateChecking tid srcid (cBalSrc - amount) in
          let _ = do_updateChecking tid srcid (cBalDest + amount) in
          ())
      in
      match msg.ev.args with
      | [ VConst (I srcid); VConst (I destid); VConst (I amount) ] ->
          let _ = aux srcid destid amount in
          send ("sendPaymentsResp", [])
      | _ -> _die [%here]

  (* transact savings *)
  let async_transactSavings ~thread_id name amount () =
    let open Lwt.Syntax in
    let* tid = DB.async_begin ~thread_id () in
    try
      let* _, _, custid =
        async_get ~tid ~table:acc_str ~key:(string_of_int name) ()
      in
      let custid =
        match Config.json_to_values custid with
        | [ VConst (I v) ] -> v
        | _ -> _die [%here]
      in
      let* _, _, sBal =
        async_get ~tid ~table:sav_str ~key:(string_of_int custid) ()
      in
      let sBal =
        match Config.json_to_values sBal with
        | [ VConst (I v) ] -> v
        | _ -> _die [%here]
      in
      let* () = 
        async_put ~tid ~table:sav_str ~key:(string_of_int custid)
        ~json:(Config.values_to_json [ VConst (I (sBal + amount))]) ()
      in
      let* _ = DB.async_commit ~tid () in
      Lwt.return_unit
    with BackendMariaDB.DBKeyNotFound _ ->
      let* _ = DB.async_release_connection ~tid () in
      Lwt.return_unit

  let transactSavingsReqHandler (msg : msg) =
    let aux (name : int) (amount: int) =
      do_trans (fun tid ->
          let custid = do_selectAccounts tid name in
          let sBal = do_selectSavings tid custid in
          let _ = do_updateSavings tid custid (sBal + amount) in
          ())
      in
      match msg.ev.args with
      | [ VConst (I name); VConst (I amount) ] ->
          let _ = aux name amount in
          send ("transactSavingsResp", [])
      | _ -> _die [%here]

  (* write check *)
  let async_writeCheck ~thread_id name amount () =
    let open Lwt.Syntax in
    let* tid = DB.async_begin ~thread_id () in
    try
      let* _, _, custid = 
        async_get ~tid ~table:acc_str ~key:(string_of_int name) ()
      in
      let custid = 
        match Config.json_to_values custid with
        | [ VConst (I v) ] -> v
        | _ -> _die [%here]
      in
      let* _, _, sBal = 
        async_get ~tid ~table:sav_str ~key:(string_of_int custid) ()
      in
      let sBal =
        match Config.json_to_values sBal with
        | [ VConst (I v) ] -> v
        | _ -> _die [%here]
      in
      let* _, _, cBal = 
        async_get ~tid ~table:chk_str ~key:(string_of_int custid) ()
      in
      let cBal =
        match Config.json_to_values cBal with
        | [ VConst (I v) ] -> v
        | _ -> _die [%here]
      in
      let total = sBal + cBal in
      let* () =
        if (total < amount) then
          async_put ~tid ~table:chk_str ~key:(string_of_int custid)
          ~json:(Config.values_to_json [ VConst (I (cBal - (amount + 1)))]) ()
        else
          async_put ~tid ~table:chk_str ~key:(string_of_int custid)
          ~json:(Config.values_to_json [ VConst (I (cBal - amount))]) ()
      in
      let* _ = DB.async_commit ~tid () in
      Lwt.return_unit
    with BackendMariaDB.DBKeyNotFound _ ->
      let* _ = DB.async_release_connection ~tid () in
      Lwt.return_unit
  
  let writeCheckReqHandler (msg : msg) =
    let aux (name : int) (amount : int) =
      do_trans (fun tid ->
          let custid = do_selectAccounts tid name in
          let sBal = do_selectSavings tid custid in
          let cBal = do_selectChecking tid custid in
          let total = sBal + cBal in
          let _ = if (total < amount)
                    then do_updateChecking tid custid (cBal - amount)
                    else do_updateChecking tid custid (cBal - (amount + 1))
                  in
          ())
    in
    match msg.ev.args with
      | [ VConst (I name); VConst (I amount) ] ->
          let _ = aux name amount in
          send ("writeCheckResp", [])
      | _ -> _die [%here]


  let openAccountsRespHandler (_ : msg) = ()
  let amalgamateRespHandler (_ : msg) = ()
  (*let balanceRespHandler (_ : msg) = ()*)

  let balanceRespHandler (msg : msg) =
    let balance = match msg.ev.args with [ VConst (I v) ] -> v | _ -> _die [%here] in
    send ("balanceResp", [ mk_value_int balance ])

  let depositCheckingRespHandler (_ : msg) = ()
  let sendPaymentRespHandler (_ : msg) = ()
  let transactSavingsRespHandler (_ : msg) = ()
  let writeCheckRespHandler (_ : msg) = ()


  let init () =
    register_async_has_ret "beginT" beginAsync;
    register_async_has_ret "commit" commitAsync;
    register_async_has_ret "selectAccounts" selectAccountsAsync;
    register_async_no_ret "updateAccounts" updateAccountsAsync;
    register_async_has_ret "selectSavings" selectSavingsAsync;
    register_async_no_ret "updateSavings" updateSavingsAsync;
    register_async_has_ret "selectChecking" selectCheckingAsync;
    register_async_no_ret "updateChecking" updateCheckingAsync;
    register_handler "openAccountsReq" openAccountsReqHandler;
    register_handler "amalgamateReq" amalgamateReqHandler;
    register_handler "balanceReq" balanceReqHandler;
    register_handler "depositCheckingReq" depositCheckingReqHandler;
    register_handler "sendPaymentReq" sendPaymentReqHandler;
    register_handler "transactSavingsReq" transactSavingsReqHandler;
    register_handler "writeCheckReq" writeCheckReqHandler;
    register_handler "openAccountsResp" openAccountsRespHandler;
    register_handler "amalgamateResp" amalgamateRespHandler;
    register_handler "balanceResp" balanceRespHandler;
    register_handler "depositCheckingResp" depositCheckingRespHandler;
    register_handler "sendPaymentResp" sendPaymentRespHandler;
    register_handler "transactSavingsResp" transactSavingsRespHandler;
    register_handler "writeCheckResp" writeCheckRespHandler;
    D.clear ()

  let testCtx =
    let open Nt in
    let record l = Ty_record { alias = None; fds = l } in
    Typectx.add_to_rights Typectx.emp
      [
        "beginT"#:(record [ "tid"#:int_ty ]);
        "commit"#:(record [ "tid"#:int_ty; "cid"#:int_ty ]);
        "selectAccounts"#:(record
                            [
                              "tid"#:int_ty;
                              "key"#:int_ty;
                              "prev_tid"#:int_ty;
                              "prev_cid"#:int_ty;
                              "value"#:(int_ty);
                            ]);
        "updateAccounts"#:(record
                            [ "tid"#:int_ty; "key"#:int_ty; "value"#:(int_ty) ]);
        "selectSavings"#:(record
                            [
                              "tid"#:int_ty;
                              "key"#:int_ty;
                              "prev_tid"#:int_ty;
                              "prev_cid"#:int_ty;
                              "value"#:(int_ty);
                            ]);
        "updateSavings"#:(record
                            [ "tid"#:int_ty; "key"#:int_ty; "value"#:(int_ty) ]);
        "selectChecking"#:(record
                            [
                              "tid"#:int_ty;
                              "key"#:int_ty;
                              "prev_tid"#:int_ty;
                              "prev_cid"#:int_ty;
                              "value"#:(int_ty);
                            ]);
        "updateChecking"#:(record
                            [ "tid"#:int_ty; "key"#:int_ty; "value"#:(int_ty) ]);   
        "openAccountsReq"#:(record [ "name"#:int_ty; "custid"#:int_ty ]);
        "openAccountsResp"#:(record []);                     
        "amalgamateReq"#:(record [ "custid0"#:int_ty; "custid1"#:int_ty ]);
        "amalgamateResp"#:(record []);
        "balanceReq"#:(record [ "name"#:int_ty ]);
        "balanceResp"#:(record [ "balance"#:int_ty ]);
        "depositCheckingReq"#:(record [ "name"#:int_ty; "amount"#:int_ty ]);
        "depositCheckingResp"#:(record []);
        "sendPaymentReq"#:(record [ "srcid"#:int_ty; "destid"#:int_ty; "amount"#:int_ty ]);
        "sendPaymentResp"#:(record []);
        "transactSavingsReq"#:(record [ "name"#:int_ty; "amount"#:int_ty ]);
        "transactSavingsResp"#:(record []);
        "writeCheckReq"#:(record [ "name"#:int_ty; "amount"#:int_ty ]);
        "writeCheckResp"#:(record []);
      ]

end




module TwitterDB = struct
  (* twitter db operations: get the follow list
                            update the follow list (this pair suffices for follow and unfollow)
                            tweet
                            get tweets for a user (timeline, newsfeed) *)

  module D = MyDB (Config)
  include D

  let db_str = "twitter"
  let flw_str = "follows"
  let twt_str = "tweets"

  let async_get ~tid ~table ~key () =
    let _ = Printf.printf "in async get\n" in
    DB.table_async_get ~tid ~db:db_str ~table ~key ()
  let async_put ~tid ~table ~key ~json () =
    let _ = Printf.printf "in async put\n" in
    DB.table_async_put ~tid ~db:db_str ~table ~key ~json ()

  let selectFollowsAsync (ev : ev) = _getAsyncTable db_str flw_str ev
  let selectTweetsAsync (ev : ev) = _getAsyncTable db_str twt_str ev
  let updateFollowsAsync (ev : ev) = _putAsyncTable db_str flw_str ev
  let updateTweetsAsync (ev : ev) = _putAsyncTable db_str twt_str ev

  let do_selectFollows tid user =
    let msg = async ("selectFollows", [ mk_value_int tid; mk_value_int user ]) in
    match msg.ev.args with
    | _ :: _ :: _ :: _ :: args -> args
    | _ -> _die [%here]

  let do_selectTweets tid user =
    let msg = async ("selectTweets", [mk_value_int tid; mk_value_int user ]) in
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
        DB.async_put ~tid ~table:flw_str ~key:(string_of_int user)
          ~json:(Config.values_to_json [ VCIntList [] ])
          ()
      in
      let* () =
        DB.async_put ~tid ~table:twt_str ~key:(string_of_int user)
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
        DB.async_get ~tid ~table:flw_str ~key:(string_of_int user) ()
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
        DB.async_put ~tid ~table:flw_str ~key:(string_of_int user)
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
          let _ = Pp.printf ("sending follow resp\n") in
          send ("followResp", [])
      | _ -> _die [%here]


  (* unfollowing a user *)
  let async_unfollow ~thread_id user unfollow_o () =
    let open Lwt.Syntax in
    let* tid = DB.async_begin ~thread_id () in
    try
      let* _, _, oldFollows = 
        DB.async_get ~tid ~table:flw_str ~key:(string_of_int user) ()
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
        DB.async_put ~tid ~table:flw_str ~key:(string_of_int user)
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
    let* _ = Lwt_io.printlf "510 %d" thread_id in
    let* tid = DB.async_begin ~thread_id () in
    let* _ = Lwt_io.printlf "512 %d" thread_id in
    try
      let* _, _, oldTweets = 
        DB.async_get ~tid ~table:twt_str ~key:(string_of_int user) ()
      in
      let* _ = Lwt_io.printlf "517 %d" thread_id in
      let oldTweets = 
        match Config.json_to_values oldTweets with
        | [ VCIntList l ] -> l
        | _ -> _die [%here]
      in
      let newTweets =
        tweet :: oldTweets
      in
      let* _ = Lwt_io.printlf "526 %d" thread_id in
      let* () = 
        DB.async_put ~tid ~table:twt_str ~key:(string_of_int user)
          ~json:(Config.values_to_json [ VCIntList newTweets ])
          ()
      in
      let* _ = Lwt_io.printlf "532 %d !%d" thread_id tid in
      let* _ = DB.async_commit ~tid () in 
      let* _ = Lwt_io.printlf "534 %d !%d" thread_id tid in
      Lwt.return_unit
    with BackendMariaDB.DBKeyNotFound _ ->
      let* _ = Lwt_io.printlf "537 %d" thread_id in
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
        DB.async_get ~tid ~table:twt_str ~key:(string_of_int user) ()
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


(*
module TPCCDB = struct 
  module D = MyDB (Config)
  include D

  let db_str = "tpcc"
  let district_table = "district"
  let warehouse_table = "warehouse"
  let item_table = "item"
  let stock_table = "stock"
  let customer_table = "customer"
  let history_table = "history"
  let openorder_table = "oorder"
  let orderline_table = "order_line"
  let neworder_table = "new_order"

  let selectDistrictAsync (ev : ev) = _getAsyncTable db_str district_table ev
  let selectWarehouseAsync (ev : ev) = _getAsyncTable db_str warehouse_table ev
  let selectItemAsync (ev : ev) = _getAsyncTable db_str item_table ev
  let selectStockAsync (ev : ev) = _getAsyncTable db_str stock_table ev
  let selectCustomerAsync (ev : ev) = _getAsyncTable db_str customer_table ev
  let selectHistoryAsync (ev : ev) = _getAsyncTable db_str history_table ev
  let selectOpenOrderAsync (ev : ev) = _getAsyncTable db_str openorder_table ev
  let selectOrderLineAsync (ev : ev) = _getAsyncTable db_str orderline_table ev
  let selectNewOrderAsync (ev : ev) = _getAsyncTable db_str neworder_table ev

  let updateDistrictAsync (ev : ev) = _putAsyncTable db_str district_table ev
  let updateWarehouseAsync (ev : ev) = _putAsyncTable db_str warehouse_table ev
  let updateItemAsync (ev : ev) = _putAsyncTable db_str item_table ev
  let updateStockAsync (ev : ev) = _putAsyncTable db_str stock_table ev
  let updateCustomerAsync (ev : ev) = _putAsyncTable db_str customer_table ev
  let updateHistoryAsync (ev : ev) = _putAsyncTable db_str history_table ev
  let updateOpenOrderAsync (ev : ev) = _putAsyncTable db_str openorder_table ev
  let updateOrderLineAsync (ev : ev) = _putAsyncTable db_str orderline_table ev
  let updateNewOrderAsync (ev : ev) = _putAsyncTable db_str neworder_table ev


  let do_selectCustomer tid c_id =
    let msg = async ("selectCustomer", [ mk_value_int tid; mk_value_int c_id ]) in
    match msg.ev.args with
    | _ :: _ :: _ :: _ :: args -> args
    | _ -> _die [%here]

  let do_selectDistrict tid d_id =
    let msg = async ("selectDistrict", [ mk_value_int tid; mk_value_int d_id ]) in
    match msg.ev.args with
    | _ :: _ :: _ :: _ :: args -> args
    | _ -> _die [%here]

  let do_selectHistory tid h_c_id =
    let msg = async ("selectHistory", [ mk_value_int tid; mk_value_int h_c_id ]) in
    match msg.ev.args with
    | _ :: _ :: _ :: _ :: args -> args
    | _ -> _die [%here]

  let do_selectItem tid i_id =
    let msg = async ("selectItem", [ mk_value_int tid; mk_value_int i_id ]) in
    match msg.ev.args with
    | _ :: _ :: _ :: _ :: args -> args
    | _ -> _die [%here]

  let do_selectNewOrder tid no_o_id =
    let msg = async ("selectNewOrder", [ mk_value_int tid; mk_value_int no_o_id ]) in
    match msg.ev.args with
    | _ :: _ :: _ :: _ :: args -> args
    | _ -> _die [%here]

  let do_selectOpenOrder tid o_id =
    let msg = async ("selectOpenOrder", [ mk_value_int tid; mk_value_int o_id ]) in
    match msg.ev.args with
    | _ :: _ :: _ :: _ :: args -> args
    | _ -> _die [%here]

  

  let do_updateCustomer tid c_id c_w_id c_d_id c_discount
                        c_credit c_last c_first c_credit_lim
                        c_balance c_ytd_payment c_payment_cnt
                        c_delivery_cnt c_street_1 c_street_2
                        c_city c_state c_zip c_phone c_since
                        c_middle c_data =
    async ("updateCustomer", [
      mk_value_int tid;
      mk_value_int c_id;
      mk_value_int c_w_id;
      mk_value_int c_d_id;
      mk_value_int c_discount; (*decimal(4,4)*)
      mk_value_int c_credit; (*char(2)*)
      mk_value_int c_last; (*varchar(16)*)
      mk_value_int c_first; (*varchar(16)*)
      mk_value_int c_credit_lim; (*decimal(12,2)*)
      mk_value_int c_balance; (*decimal(12,2)*)
      mk_value_int c_ytd_payment; (*float*)
      mk_value_int c_payment_cnt;
      mk_value_int c_delivery_cnt;
      mk_value_int c_street_1; (*varchar(20)*)
      mk_value_int c_street_2; (*varchar(20)*)
      mk_value_int c_city; (*varchar(20)*)
      mk_value_int c_state; (*char(2)*)
      mk_value_int c_zip; (*char(9)*)
      mk_value_int c_phone; (*char(16)*)
      mk_value_int c_since; (*timestamp, default current*)
      mk_value_int c_middle; (*char(2)*)
      mk_value_int c_data; (*varchar(500)*)
    ])

  let do_updateDistrict tid d_id d_w_id d_ytd d_tax
                        d_next_o_id d_name d_street_1
                        d_street_2 d_city d_state d_zip =
    async ("updateDistrict", [ 
      mk_value_int tid;
      mk_value_int d_id;
      mk_value_int d_w_id;
      mk_value_int d_ytd; (*decimal (12,2)*)
      mk_value_int d_tax; (*decimal (12,2)*)
      mk_value_int d_next_o_id;
      mk_value_int d_name; (*varchar(10)*)
      mk_value_int d_street_1; (*varchar(20)*)
      mk_value_int d_street_2; (*varchar(20)*)
      mk_value_int d_city; (*carchar(20)*)
      mk_value_int d_state; (*char(2)*)
      mk_value_int d_zip; (*char(9)*)
    ])

  let do_updateHistory tid h_c_id h_c_d_id h_c_w_id
                       h_d_id h_w_id h_date h_amount h_data =
    async ("updateHistory", [
      mk_value_int tid;
      mk_value_int h_c_id;
      mk_value_int h_c_d_id;
      mk_value_int h_c_w_id;
      mk_value_int h_d_id;
      mk_value_int h_w_id;
      mk_value_int h_date; (*timestamp*)
      mk_value_int h_amount; (*decimal(6,2)*)
      mk_value_int h_data (*varchar(24)*)
    ])

  let do_updateItem tid i_id i_name i_price i_data i_im_id =
    async ("updateItem", [
      mk_value_int tid;
      mk_value_int i_id;
      mk_value_int i_name; (*varchar(24)*)
      mk_value_int i_price; (*decimal(5,2)*)
      mk_value_int i_data; (*varchar(50)*)
      mk_value_int i_im_id
    ])

  let do_updateNewOrder tid no_w_id no_d_id no_o_id =
    async ("updateNewOrder", [
      mk_value_int tid;
      mk_value_int no_w_id;
      mk_value_int no_d_id;
      mk_value_int no_o_id;
    ])

  let do_updateOpenOrder tid o_w_id o_d_id o_id o_c_id
                         o_carrier_id o_ol_cnt o_all_local
                         o_entry_d =
    async ("updateOpenOrder", [
      mk_value_int tid;
      mk_value_int o_w_id;
      mk_value_int o_d_id;
      mk_value_int o_id;
      mk_value_int o_c_id;
      mk_value_int o_carrier_id;
      mk_value_int o_ol_cnt; (*decimal(2,0)*)
      mk_value_int o_all_local; (*decimal(1,0)*)
      mk_value_int o_entry_d; (*timestamp default current*)
    ])
(*
CREATE TABLE ORDER_LINE (
  OL_W_ID INT NOT NULL,
  OL_D_ID INT NOT NULL,
  OL_O_ID INT NOT NULL,
  OL_NUMBER INT NOT NULL,
  OL_I_ID INT NOT NULL,
  OL_DELIVERY_D TIMESTAMP NULL DEFAULT NULL,
  OL_AMOUNT DECIMAL(6,2) NOT NULL,
  OL_SUPPLY_W_ID INT NOT NULL,
  OL_QUANTITY DECIMAL(2,0) NOT NULL,
  OL_DIST_INFO CHAR(24) NOT NULL,
  PRIMARY KEY (OL_W_ID,OL_D_ID,OL_O_ID,OL_NUMBER)
);

CREATE TABLE STOCK (
  S_W_ID INT NOT NULL,
  S_I_ID INT NOT NULL,
  S_QUANTITY DECIMAL(4,0) NOT NULL,
  S_YTD DECIMAL(8,2) NOT NULL,
  S_ORDER_CNT INT NOT NULL,
  S_REMOTE_CNT INT NOT NULL,
  S_DATA VARCHAR(50) NOT NULL,
  S_DIST_01 CHAR(24) NOT NULL,
  S_DIST_02 CHAR(24) NOT NULL,
  S_DIST_03 CHAR(24) NOT NULL,
  S_DIST_04 CHAR(24) NOT NULL,
  S_DIST_05 CHAR(24) NOT NULL,
  S_DIST_06 CHAR(24) NOT NULL,
  S_DIST_07 CHAR(24) NOT NULL,
  S_DIST_08 CHAR(24) NOT NULL,
  S_DIST_09 CHAR(24) NOT NULL,
  S_DIST_10 CHAR(24) NOT NULL,
  PRIMARY KEY (S_W_ID,S_I_ID)
);

CREATE TABLE WAREHOUSE (
  W_ID INT NOT NULL,
  W_YTD DECIMAL(12,2) NOT NULL,
  W_TAX DECIMAL(4,4) NOT NULL,
  W_NAME VARCHAR(10) NOT NULL,
  W_STREET_1 VARCHAR(20) NOT NULL,
  W_STREET_2 VARCHAR(20) NOT NULL,
  W_CITY VARCHAR(20) NOT NULL,
  W_STATE CHAR(2) NOT NULL,
  W_ZIP CHAR(9) NOT NULL,
  PRIMARY KEY (W_ID)
);
*)

  let do_trans f =
    let msg = async ("beginT", []) in
    let tid =
      match msg.ev.args with [ VConst (I tid) ] -> tid | _ -> _die [%here]
    in
    let res = f tid in
    let _ = async ("commit", [ mk_value_int tid ]) in
    res

  (* delivery *)
  let async_delivery = true

  let deliveryReqHandler = true

  (* new order *)
  let async_new_order = true

  let newOrderReqHandler = true

  (* order status *)
  let async_order_status = true

  let orderStatusReqHandler = true

  (* payment *)
  let async_payment = true

  let paymentReqHandler = true

  (* stock level *)
  let async_stock_level = true

  let stockLevelReqHandler = true



  let deliveryRespHandler (_ : msg) = ()
  let newOrderRespHandler (_ : msg) = ()
  let orderStatusRespHandler (_ : msg) = ()
  let paymentRespHandler (_ : msg) = ()
  let stockLevelRespHandler (_ : msg) = ()

  let init () =
    (* database *)
    register_async_has_ret "beginT" beginAsync;
    register_async_has_ret "commit" commitAsync;
    register_async_has_ret "selectDistrict" selectDistrictAsync;
    register_async_no_ret "updateDistrict" updateDistrictAsync;
    register_async_has_ret "selectWarehouse" selectWarehouseAsync;
    register_async_no_ret "updateWarehouse" updateWarehouseAsync;
    register_async_has_ret "selectItem" selectItemAsync;
    register_async_no_ret "updateItem" updateItemAsync;
    register_async_has_ret "selectStock" selectStockAsync;
    register_async_no_ret "updateStock" updateStockAsync;
    register_async_has_ret "selectCustomer" selectCustomerAsync;
    register_async_no_ret "updateCustomer" updateCustomerAsync;
    register_async_has_ret "selectHistory" selectHistoryAsync;
    register_async_no_ret "updateHistory" updateHistoryAsync;
    register_async_has_ret "selectOOrder" selectOOrderAsync;
    register_async_no_ret "updateOOrder" updateOOrderAsync;
    register_async_has_ret "selectOrderLine" selectOrderLineAsync;
    register_async_no_ret "updateOrderLine" updateOrderLineAsync;
    register_async_has_ret "selectNewOrder" selectNewOrderAsync;
    register_async_no_ret "updateNewOrder" updateNewOrderAsync;
    (* procedures *)
    register_handler "deliveryReq" deliveryReqHandler;
    register_handler "newOrderReq" newOrderReqHandler;
    register_handler "orderStatusReq" orderStatusReqHandler;
    register_handler "paymentReq" paymentReqHandler;
    register_handler "stockLevelReq" stockLevelReqHandler;
    register_handler "deliveryResp" deliveryRespHandler;
    register_handler "newOrderResp" newOrderRespHandler;
    register_handler "orderStatusResp" orderStatusRespHandler;
    register_handler "paymentResp" paymentRespHandler;
    register_handler "stockLevelResp" stockLevelRespHandler;
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
  *)