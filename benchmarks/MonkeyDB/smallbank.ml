open Common
open Language
module SmallBankDB = SmallBankDB
open SmallBankDB
(*open Nt*)

let gen name args body =
  mk_term_gen testCtx name (List.map (fun x -> VVar x) args) body

let obs name k = mk_term_obs_fresh testCtx name (fun _ -> k)


let obsAmalgamateResp e = mk_term_obs_fresh testCtx "amalgamateResp" (fun _ -> e)

let obsBalanceResp e = mk_term_obs_fresh testCtx "balanceResp" (fun _ -> e)

let obsDepositCheckingResp e = mk_term_obs_fresh testCtx "depositCheckingResp" (fun _ -> e)

let obsSendPaymentResp e = mk_term_obs_fresh testCtx "sendPaymentResp" (fun _ -> e)

let obsTransactSavingsResp e = mk_term_obs_fresh testCtx "transactSavingsResp" (fun _ -> e)

let obsWriteCheckResp e = mk_term_obs_fresh testCtx "writeCheckResp" (fun _ -> e)


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
        (And [ prop1 ; prop2 ], k)
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

(*
type twitter_bench_config = { numUser : int; numTweet : int; numOp : int }

let num_connection = 3

let random_user { numUser; numTweet; numOp } =
  let open Lwt.Syntax in
  let users = List.init numUser (fun i -> i + 1) in
  let tweets = List.init numTweet (fun i -> i + 1) in
  let rec fill_users ~thread_id i () = 
    match List.nth_opt users i with
    | Some user -> let* () = fill_users ~thread_id (i+1) () in async_new_user ~thread_id user ()
    | None -> Lwt.return_unit
  in
  let random_new_user ~thread_id () =
    let user = List.nth users (Random.int numUser) in
    async_new_user ~thread_id user ()
  in
  let random_follow ~thread_id () =
    let user = List.nth users (Random.int numUser) in
    let follow_o = List.nth users (Random.int numUser) in
    async_follow ~thread_id user follow_o ()
  in
  let random_unfollow ~thread_id () =
    let user = List.nth users (Random.int numUser) in
    let unfollow_o = List.nth users (Random.int numUser) in
    async_unfollow ~thread_id user unfollow_o ()
  in
  let random_tweet ~thread_id () =
    let user = List.nth users (Random.int numUser) in
    let tweet = List.nth tweets (Random.int numTweet) in
    async_post_tweet ~thread_id user tweet ()
  in
  let random_option ~thread_id () =
    match (Random.int 4) + 3 with
    | 0 -> random_new_user ~thread_id ()
    | 2 -> random_follow ~thread_id ()
    | 1 -> random_unfollow ~thread_id ()
    | _-> random_tweet ~thread_id ()
  in
  let rec genOp ~thread_id restNum =
    if restNum <= 0 then
      let () =
        Pp.printf "@{<red>[thread: %i] End with numOp@}\n%i\n" thread_id numOp
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
    @@ 
    Lwt.bind (fill_users ~thread_id:0 0 ())
             (fun() -> Lwt.join
                    [
                      genOp ~thread_id:0 numOp;
                      genOp ~thread_id:1 numOp;
                      genOp ~thread_id:2 numOp;
                    ])
  in
  ()
*)