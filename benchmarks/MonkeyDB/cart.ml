open Common
open Language
module CartDB = CartDB
open CartDB
open Nt

let gen name args body =
  mk_term_gen testCtx name (List.map (fun x -> VVar x) args) body

let obs name k = mk_term_obs_fresh testCtx name (fun _ -> k)
let obsAddItemResp e = mk_term_obs_fresh testCtx "addItemResp" (fun _ -> e)

let obsDeleteItemResp e =
  mk_term_obs_fresh testCtx "deleteItemResp" (fun _ -> e)

let obsNewUserResp e = mk_term_obs_fresh testCtx "newUserResp" (fun _ -> e)

let obsBegin k =
  mk_term_obs_fresh testCtx "beginT" (function
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

let obsGetPrev tid prev_tid k =
  mk_term_obs_prop_fresh testCtx "get" (function
    | tid' :: _ :: prev_tid' :: _ ->
        let prop1 = lit_to_prop (mk_var_eq_var [%here] tid tid') in
        let prop2 = lit_to_prop (mk_var_eq_var [%here] prev_tid prev_tid') in
        (And [ prop1; prop2 ], k)
    | _ -> _die [%here])

let obsPut tid k =
  mk_term_obs_prop_fresh testCtx "put" (function
    | tid' :: _ ->
        let prop = lit_to_prop (mk_var_eq_var [%here] tid tid') in
        (prop, k)
    | _ -> _die [%here])

(* let main =
  mk_term_assume_fresh_true int_ty (fun user ->
      mk_term_assume_fresh_true int_ty (fun item ->
          gen "newUserReq" [ user ]
          @@ obsBegin (fun tid0 ->
                 obsPut tid0 @@ obsCommit tid0 @@ obsNewUserResp
                 @@ gen "addItemReq" [ user; item ]
                 @@ gen "deleteItemReq" [ user; item ]
                 @@ obsBegin (fun tid1 ->
                        obsBegin (fun tid2 ->
                            obsGet tid2 @@ obsGet tid1 @@ obsPut tid1
                            @@ obsCommit tid1 @@ obsPut tid2 @@ obsAddItemResp
                            @@ gen "addItemReq" [ user; item ]
                            @@ obsBegin (fun tid3 ->
                                   obsGetPrev tid3 tid1 @@ obsCommit tid2
                                   @@ obsDeleteItemResp @@ obsPut tid3
                                   @@ obsCommit tid3
                                   @@ obsAddItemResp mk_term_tt)))))) *)

let main =
  mk_term_assume_fresh_true int_ty (fun user ->
      mk_term_assume_fresh_true int_ty (fun item ->
          gen "newUserReq" [ user ]
          @@ obsBegin (fun tid0 ->
                 obsPut tid0 @@ obsCommit tid0 @@ obsNewUserResp
                 @@ gen "addItemReq" [ user; item ]
                 @@ gen "deleteItemReq" [ user; item ]
                 @@ obsBegin (fun tid1 ->
                        obsBegin (fun tid2 ->
                            obsGet tid2 @@ obsPut tid2 @@ obsGet tid1
                            @@ obsCommit tid2 @@ obsDeleteItemResp
                            @@ obsPut tid1 @@ obsCommit tid1
                            @@ obsAddItemResp mk_term_tt)))))

type cart_bench_config = { numUserDB : int; numItemDB : int; numOpDB : int }

let parse_config config =
  let open Interpreter in
  let numUserDB = get_config_value config "numUserDB" in
  let numItemDB = get_config_value config "numItemDB" in
  let numOpDB = get_config_value config "numOpDB" in
  { numUserDB; numItemDB; numOpDB }

let num_connection = 3

let random_user config =
  let { numUserDB; numItemDB; numOpDB } = parse_config config in
  let open Lwt.Syntax in
  let users = List.init numUserDB (fun i -> i + 1) in
  let items = List.init numItemDB (fun i -> i + 1) in
  let random_add ~thread_id () =
    let user = List.nth users (Random.int numUserDB) in
    let item = List.nth items (Random.int numItemDB) in
    async_add_item ~thread_id user item ()
  in
  let random_delete ~thread_id () =
    let user = List.nth users (Random.int numUserDB) in
    let item = List.nth items (Random.int numItemDB) in
    async_delete_item ~thread_id user item ()
  in
  let random_new_user ~thread_id () =
    let user = List.nth users (Random.int numUserDB) in
    async_new_user ~thread_id user ()
  in
  let random_option ~thread_id () =
    match Random.int 3 with
    | 0 -> random_add ~thread_id ()
    | 1 -> random_delete ~thread_id ()
    | _ -> random_new_user ~thread_id ()
  in
  let rec genOp ~thread_id restNum =
    if restNum <= 0 then
      let () =
        Pp.printf "@{<red>[thread: %i] End with numOpDB@}\n%i\n" thread_id
          numOpDB
      in
      Lwt.return_unit
    else
      let () =
        Pp.printf "@{<yellow>[thread: %i] restNum@}: %i\n" thread_id restNum
      in
      let* _ = random_option ~thread_id () in
      genOp ~thread_id (restNum - 1)
  in
  let () =
    Lwt_main.run
    @@ Lwt.join
         [
           genOp ~thread_id:0 numOpDB;
           genOp ~thread_id:1 numOpDB;
           genOp ~thread_id:2 numOpDB;
         ]
  in
  ()

open Interpreter

let test_env isolation =
  {
    if_concurrent = true;
    database_ctx = Some { dbname = "cart"; isolation };
    init_test_env = CartDB.init;
    default_test_prog = [];
    property = CartDB.check_isolation_level Serializable;
    random_test_gen = random_user;
  }
