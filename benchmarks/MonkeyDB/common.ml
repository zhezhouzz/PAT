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




module TreiberStackDB = struct
  (* Tstack transactions: Just CAS? No. Add cell, remove cell (bimyou), CAS *)
  (*
     Top
     Push - uses compare and swap
     Pop - uses compare and swap
  *)

  module D = MyDB (Config)
  include D

  let topKey = 0
  (*let _opCount = ref 0*)

  let readAsync (ev : ev) = _getAsync "stack" ev
  let writeAsync (ev : ev) = _putAsync "stack" ev


  let do_read tid key = 
    let msg = async ("read", [ mk_value_int tid; mk_value_int key ]) in
    match msg.ev.args with
    | _ :: _ :: _ :: _ :: [ VConst (I value) ; VConst (I next) ] -> value, next
    | _ -> _die [%here]

  let do_write tid key value next =
    async ("updateAccounts", [ mk_value_int tid; mk_value_int key ; mk_value_int value; mk_value_int next])


  let do_trans f =
    let msg = async ("beginT", []) in
    let tid =
      match msg.ev.args with [ VConst (I tid) ] -> tid | _ -> _die [%here]
    in
    let res = f tid in
    let _ = async ("commit", [ mk_value_int tid ]) in
    (*let () = _opCount := !_opCount + 1 *)
    res

  let async_cas old_head new_head tid =
      let* _, _, key =
        DB.async_get ~tid ~table:"stack" ~key:(string_of_int topKey)
      in
      let key =
        match Config.json_to_values key with
          | [ VConst (I _) ; VConst (I next) ] -> next
          | _ -> _die [%here]
      in
      if key == old_head then
        let () = DB.async_put ~tid ~table:"stack" ~key:(string_of_int topKey)
                 ~json (Config.values_to_json [ VConst (I -1) ; VConst (I new_head)])
        in true
      else false


  let do_cas old_head new_head tid =
      let key = do_read topKey in
      let key =
        match Config.json_to_values topKey with
        | [ VConst (I _) ; VConst (I key)] -> key
        | _ -> _die [%here]
      in
      if key == old_head then
        let _ = do_write tid new_head in
        true
      else false

  (* top *)
  (*
  let async_top ~thread_id () =
    let open Lwt.Syntax in
    let* tid = DB.async_begin ~thread_id () in
    try
      (* TODO the top operation stuff *)
    with BackendMariaDB.DBKeyNotFound _ ->
      let* _ = DB.async_release_connection ~tid () in
      Lwt.return_unit

  let topReqHandler (msg : msg) =
    let aux () =
      do_trans (fun tid ->
        let _ = (* TODO some database interactions here *) in
        ())
    in
    match msg.ev.args with
    | [] ->
        let v = aux () in
        send ("topResp", [ mk_value_int v ])
    | _ -> _die [%here]
  *)
  (* push *) 

  let async_push ~thread_id element () = 
    let open Lwt.Syntax in
    let* tid = DB.async_begin ~thread_id () in
    try
      let* _, _, old_head =
        DB.async_get ~tid ~table:"stack" ~key:(string_of_int topKey)
      in
      let old_head =
        match Config.json_to_values old_head with
          | [ VConst (I _) ; VConst (I next) ] -> next
          | _ -> _die [%here]
      in
      let cas = ref false in
      while !cas do
        let () =
          DB.async_put ~tid ~table:"stack" ~key:(string_of_int tid)
          ~json (Config.values_to_json [ VConst (I element) ; VConst (I old_head)])
        in
        let () = cas := (async_cas old_head tid tid)
        in
        ()
      done
      let* _ = DB.async_commit ~tid () in
      Lwt.return_unit
    with BackendMariaDB.DBKeyNotFound _ ->
      let* _ = DB.async_release_connection ~tid () in
      Lwt.return_unit

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
    match msg.ev.args with
    | [ VConst (I v) ] ->
        let _ = aux v in
        send ("pushResp", [])
    | _ -> _die [%here]

  (* pop *)
  let async_pop ~thread_id () =
    let open Lwt.Syntax in
    let* tid = DB.async_begin ~thread_id () in
    try
      let cas = ref false in
      while not !cas do
        let* _, _, old_head =
          DB.async_get ~tid ~table:"stack" ~key:(string_of_int topKey)
        in
        let old_head =
          match Config.json_to_values old_head with
            | [ VConst (I _) ; VConst (I next) ] -> next
            | _ -> _die [%here]
        in
        let* _, _, old_head_value =
          DB.async_get ~tid ~table:"stack" ~key:(string_of_int old_head)
        in
        let top_value, new_head =
          match Config.json_to_values old_head with
            | [ VConst (I v) ; VConst (I next) ] -> v, next
            | _ -> _die [%here]
        in
        let () = cas := (async_cas old_head new_head tid)
        in
        ()
      done
      let* _ = DB.async_commit ~tid () in
      Lwt.return top_value
    with BackendMariaDB.DBKeyNotFound _ ->
      let* _ = DB.async_release_connection ~tid () in
      Lwt.fail (BackendMariaDB.DBKeyNotFound s)

  let popReqHandler (msg : msg) =
    let aux () =
      do_trans (fun tid ->
        let _ = (* TODO some database interactions here *) in
        ())
    in
    match msg.ev.args with
    | [] ->
        let v = aux () in
        send ("popResp", [ mk_value_int v ])
    | _ -> _die [%here]
        
  
  let init () =
    register_async_has_ret "beginT" beginAsync;
    register_async_has_ret "commit" commitAsync;
    (*register_async_has_ret "selectTop" selectTopAsync;
    register_async_no_ret "updateTop" updateTopAsync; *)
    register_async_has_ret "selectCell" selectCellAsync;
    register_async_no_ret "updateCell" updateCellAsync;
    (*register_handler "topReq" topReqHandler;*)
    register_handler "pushReq" pushReqHandler;
    register_handler "popReq" popReqHandler;
    (*register_handler "topResp" topRespHandler;*)
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
        "selectTop"#:(record
                        [
                          "tid"#:int_ty;
                          TODO
                        ]);
        "updateTop"#:(record
                        [ "tid"#:int_ty; TODO ]);
        "selectCell"#:(record
                          [
                            "tid"#:int_ty;
                            TODO
                          ]);
        "updateCell"#:(record
                          [ "tid"#:int_ty; TODO ]);

        "topReq"#:(record [] TODO);
        "topResp"#:(record [ "top"#:int_ty ]);
        "pushReq"#:(record [ "element"#:int_ty ]);
        "pushResp"#:(record []);
        "popReq"#:(record []);
        "popResp"#:(record [ "element"#int_ty ]);
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

  let selectAccountsAsync (ev : ev) = _getAsync "accounts" ev
  let selectSavingsAsync (ev : ev) = _getAsync "savings" ev
  let selectCheckingAsync (ev : ev) = _getAsync "checking" ev
  let updateAccountsAsync (ev : ev) = _putAsync "accounts" ev
  let updateSavingsAsync (ev : ev) = _putAsync "savings" ev
  let updateCheckingAsync (ev : ev) = _putAsync "checking" ev

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
        DB.async_put ~tid ~table:"accounts" ~key:(string_of_int name)
        ~json:(Config.values_to_json [ VConst (I (custid))]) ()
      in
      let* () =
        DB.async_put ~tid ~table:"savings" ~key:(string_of_int custid)
        ~json:(Config.values_to_json [ VConst (I 0)]) ()     
      in
      let* () =
        DB.async_put ~tid ~table:"checking" ~key:(string_of_int custid)
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
        DB.async_get ~tid ~table:"savings" ~key:(string_of_int custid0) ()
      in
      let sBal0 =
        match Config.json_to_values sBal0 with 
          | [ VConst (I v) ] -> v
          | _ -> _die [%here]
      in
      let* _, _, cBal0 =
        DB.async_get ~tid ~table:"checking" ~key:(string_of_int custid0) ()
      in
      let cBal0 =
        match Config.json_to_values cBal0 with
          | [ VConst (I v) ] -> v
          | _ -> _die [%here]
      in
      let* _, _, cBal1 =
        DB.async_get ~tid ~table:"checking" ~key:(string_of_int custid1) ()
      in
      let cBal1 =
        match Config.json_to_values cBal1 with
        | [ VConst (I v) ] -> v
        | _ -> _die [%here]
      in
      let* () =
        DB.async_put ~tid ~table:"savings" ~key:(string_of_int custid0)
        ~json:(Config.values_to_json [ VConst (I 0) ]) ()
      in
      let* () =
        DB.async_put ~tid ~table:"checking" ~key:(string_of_int custid0)
        ~json:(Config.values_to_json [ VConst (I 0) ]) ()
      in
      let* () =
        DB.async_put ~tid ~table:"checking" ~key:(string_of_int custid1)
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
        DB.async_get ~tid ~table:"accounts" ~key:(string_of_int name) ()
      in
      let custid =
        match Config.json_to_values custid with
        | [ VConst (I v) ] -> v
        | _ -> _die [%here]
      in
      let* _, _, sBal = 
        DB.async_get ~tid ~table:"savings" ~key:(string_of_int custid) ()
      in
      let sBal =
        match Config.json_to_values sBal with
        | [ VConst (I v) ] -> v
        | _ -> _die [%here]
      in
      let* _, _, cBal = 
        DB.async_get ~tid ~table:"checking" ~key:(string_of_int custid) ()
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
        DB.async_get ~tid ~table:"accounts" ~key:(string_of_int name) ()
      in
      let custid =
        match Config.json_to_values custid with
        | [ VConst (I v) ] -> v
        | _ -> _die [%here]
      in
      let* _, _, cBal =
        DB.async_get ~tid ~table:"checking" ~key:(string_of_int custid) ()
      in
      let cBal =
        match Config.json_to_values cBal with
        | [ VConst (I v) ] -> v
        | _ -> _die [%here]
      in
      let* () =
        DB.async_put ~tid ~table:"checking" ~key:(string_of_int custid)
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
        DB.async_get ~tid ~table:"checking" ~key:(string_of_int srcid) ()
      in
      let cBalSrc =
        match Config.json_to_values cBalSrc with
        | [ VConst (I v) ] -> v
        | _ -> _die [%here]
      in
      let* _, _, cBalDest =
        DB.async_get ~tid ~table:"checking" ~key:(string_of_int destid) ()
      in
      let cBalDest = 
        match Config.json_to_values cBalDest with
        | [ VConst (I v) ] -> v
        | _ -> _die [%here]
      in
      let* () =
        DB.async_put ~tid ~table:"checking" ~key:(string_of_int srcid)
        ~json:(Config.values_to_json [ VConst (I (cBalSrc - amount))]) ()
      in
      let* () =
        DB.async_put ~tid ~table:"checking" ~key:(string_of_int destid)
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
        DB.async_get ~tid ~table:"accounts" ~key:(string_of_int name) ()
      in
      let custid =
        match Config.json_to_values custid with
        | [ VConst (I v) ] -> v
        | _ -> _die [%here]
      in
      let* _, _, sBal =
        DB.async_get ~tid ~table:"savings" ~key:(string_of_int custid) ()
      in
      let sBal =
        match Config.json_to_values sBal with
        | [ VConst (I v) ] -> v
        | _ -> _die [%here]
      in
      let* () = 
        DB.async_put ~tid ~table:"savings" ~key:(string_of_int custid)
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
        DB.async_get ~tid ~table:"accounts" ~key:(string_of_int name) ()
      in
      let custid = 
        match Config.json_to_values custid with
        | [ VConst (I v) ] -> v
        | _ -> _die [%here]
      in
      let* _, _, sBal = 
        DB.async_get ~tid ~table:"savings" ~key:(string_of_int custid) ()
      in
      let sBal =
        match Config.json_to_values sBal with
        | [ VConst (I v) ] -> v
        | _ -> _die [%here]
      in
      let* _, _, cBal = 
        DB.async_get ~tid ~table:"checking" ~key:(string_of_int custid) ()
      in
      let cBal =
        match Config.json_to_values cBal with
        | [ VConst (I v) ] -> v
        | _ -> _die [%here]
      in
      let total = sBal + cBal in
      let* () =
        if (total < amount) then
          DB.async_put ~tid ~table:"checking" ~key:(string_of_int custid)
          ~json:(Config.values_to_json [ VConst (I (cBal - (amount + 1)))]) ()
        else
          DB.async_put ~tid ~table:"checking" ~key:(string_of_int custid)
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
          let _ = Pp.printf ("sending follow resp\n") in
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
    let* _ = Lwt_io.printlf "510 %d" thread_id in
    let* tid = DB.async_begin ~thread_id () in
    let* _ = Lwt_io.printlf "512 %d" thread_id in
    try
      let* _, _, oldTweets = 
        DB.async_get ~tid ~table:"tweets" ~key:(string_of_int user) ()
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
        DB.async_put ~tid ~table:"tweets" ~key:(string_of_int user)
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