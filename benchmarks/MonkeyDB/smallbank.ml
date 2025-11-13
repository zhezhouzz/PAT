open Common
open Language
module SmallBankDB = SmallBankDB
open SmallBankDB
(*open Nt*)

let gen name args body =
  mk_term_gen testCtx name (List.map (fun x -> VVar x) args) body

let obs name k = mk_term_obs_fresh testCtx name (fun _ -> k)

let obsAmalgamateResp e =
  mk_term_obs_fresh testCtx "amalgamateResp" (fun _ -> e)

let obsBalanceResp e = mk_term_obs_fresh testCtx "balanceResp" (fun _ -> e)

let obsDepositCheckingRespHandler e =
  mk_term_obs_fresh testCtx "depositCheckingResp" (fun _ -> e)

let obsSendPaymentRespHandler e =
  mk_term_obs_fresh testCtx "sendPaymentResp" (fun _ -> e)

let obsTransactSavingsRespHandler e =
  mk_term_obs_fresh testCtx "transactSavingsResp" (fun _ -> e)

let obsWriteCheckRespHandler e =
  mk_term_obs_fresh testCtx "writeCheckResp" (fun _ -> e)

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

let obsSelectAccounts tid k =
  mk_term_obs_prop_fresh testCtx "selectAccounts" (function
    | tid' :: _ ->
        let prop = lit_to_prop (mk_var_eq_var [%here] tid tid') in
        (prop, k)
    | _ -> _die [%here])

let obsSelectAccountsPrev tid prev_tid k =
  mk_term_obs_prop_fresh testCtx "selectAccounts" (function
    | tid' :: _ :: prev_tid' :: _ ->
        let prop1 = lit_to_prop (mk_var_eq_var [%here] tid tid') in
        let prop2 = lit_to_prop (mk_var_eq_var [%here] prev_tid prev_tid') in
        (And [ prop1; prop2 ], k)
    | _ -> _die [%here])

let obsUpdateAccounts tid k =
  mk_term_obs_prop_fresh testCtx "updateAccounts" (function
    | tid' :: _ ->
        let prop = lit_to_prop (mk_var_eq_var [%here] tid tid') in
        (prop, k)
    | _ -> _die [%here])

let obsSelectSavings tid k =
  mk_term_obs_prop_fresh testCtx "selectSavings" (function
    | tid' :: _ ->
        let prop = lit_to_prop (mk_var_eq_var [%here] tid tid') in
        (prop, k)
    | _ -> _die [%here])

let obsSelectSavingsPrev tid prev_tid k =
  mk_term_obs_prop_fresh testCtx "selectSavings" (function
    | tid' :: _ :: prev_tid' :: _ ->
        let prop1 = lit_to_prop (mk_var_eq_var [%here] tid tid') in
        let prop2 = lit_to_prop (mk_var_eq_var [%here] prev_tid prev_tid') in
        (And [ prop1; prop2 ], k)
    | _ -> _die [%here])

let obsUpdateSavings tid k =
  mk_term_obs_prop_fresh testCtx "updateSavings" (function
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

let obsSelectCheckingPrev tid prev_tid k =
  mk_term_obs_prop_fresh testCtx "selectChecking" (function
    | tid' :: _ :: prev_tid' :: _ ->
        let prop1 = lit_to_prop (mk_var_eq_var [%here] tid tid') in
        let prop2 = lit_to_prop (mk_var_eq_var [%here] prev_tid prev_tid') in
        (And [ prop1; prop2 ], k)
    | _ -> _die [%here])

let obsUpdateChecking tid k =
  mk_term_obs_prop_fresh testCtx "updateChecking" (function
    | tid' :: _ ->
        let prop = lit_to_prop (mk_var_eq_var [%here] tid tid') in
        (prop, k)
    | _ -> _die [%here])

type twitter_bench_config = { numUser : int; numBalance : int; numOp : int }

let num_connection = 3

let parse_config config =
  let numUser = List.assoc "numUserDB" config in
  let numBalance = List.assoc "numBalanceDB" config in
  let numOp = List.assoc "numOpDB" config in
  { numUser; numBalance; numOp }

let random_user config =
  let { numUser; numBalance; numOp } = parse_config config in
  let open Lwt.Syntax in
  let open SmallBankDB in
  let users = List.init numUser (fun i -> i + 1) in
  let rec fill_users ~thread_id i () =
    match List.nth_opt users i with
    | Some user ->
        let* () = fill_users ~thread_id (i + 1) () in
        async_openAccounts ~thread_id user user ()
    | None -> Lwt.return_unit
  in
  let random_send_payment ~thread_id () =
    let srcid = List.nth users (Random.int numUser) in
    let dstid = List.nth users (Random.int numUser) in
    let amount = Random.int numBalance in
    async_sendPayment ~thread_id srcid dstid amount ()
  in
  let random_deposit_checking ~thread_id () =
    let user = List.nth users (Random.int numUser) in
    let amount = Random.int numBalance in
    async_depositChecking ~thread_id user amount ()
  in
  let random_balance ~thread_id () =
    let user = List.nth users (Random.int numUser) in
    async_balance ~thread_id user ()
  in
  let random_amalgamate ~thread_id () =
    let custid0 = List.nth users (Random.int numUser) in
    let custid1 = List.nth users (Random.int numUser) in
    async_amalgamate ~thread_id custid0 custid1 ()
  in
  let random_open_account ~thread_id () =
    let user = List.nth users (Random.int numUser) in
    async_openAccounts ~thread_id user user ()
  in
  let random_transact_savings ~thread_id () =
    let user = List.nth users (Random.int numUser) in
    let amount = Random.int numBalance in
    async_transactSavings ~thread_id user amount ()
  in
  let random_write_check ~thread_id () =
    let user = List.nth users (Random.int numUser) in
    let amount = Random.int numBalance in
    async_writeCheck ~thread_id user amount ()
  in
  let random_option ~thread_id () =
    match Random.int 7 with
    | 0 -> random_send_payment ~thread_id ()
    | 1 -> random_deposit_checking ~thread_id ()
    | 2 ->
        let* _ = random_balance ~thread_id () in
        Lwt.return_unit
    | 3 -> random_amalgamate ~thread_id ()
    | 4 -> random_open_account ~thread_id ()
    | 5 -> random_transact_savings ~thread_id ()
    | _ -> random_write_check ~thread_id ()
  in
  let rec genOp ~thread_id restNum =
    if restNum <= 0 then
      let () =
        Pp.printf "@{<red>[thread: %i] End with numOpDB@}\n%i\n" thread_id numOp
      in
      Lwt.return_unit
    else
      let () =
        Pp.printf "@{<yellow>[thread: %i] restNum@}: %i\n" thread_id restNum
      in
      let* () = Lwt_unix.sleep 0.001 in
      let* _ = random_option ~thread_id () in
      genOp ~thread_id (restNum - 1)
  in
  let () =
    Lwt_main.run
    @@ Lwt.bind (fill_users ~thread_id:0 0 ()) (fun () ->
           Lwt.join
             [
               genOp ~thread_id:0 numOp;
               genOp ~thread_id:1 numOp;
               genOp ~thread_id:2 numOp;
             ])
  in
  ()

open Interpreter

let test_env isolation =
  {
    if_concurrent = true;
    database_ctx = Some { dbname = "smallbank"; isolation };
    init_test_env = SmallBankDB.init;
    default_test_prog = [];
    property = SmallBankDB.check_isolation_level Serializable;
    random_test_gen = random_user;
  }
