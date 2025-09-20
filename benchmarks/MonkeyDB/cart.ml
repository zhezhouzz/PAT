open Common
open Language
open Interpreter
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

type cart_bench_config = { numUser : int; numItem : int; numOp : int }

let random_user { numUser; numItem; numOp } =
  let users = List.init numUser (fun i -> i + 1) in
  let items = List.init numItem (fun i -> i + 1) in
  let random_add () =
    let user = List.nth users (Random.int numUser) in
    let item = List.nth items (Random.int numItem) in
    send ("addItemReq", [ mk_value_int user; mk_value_int item ])
  in
  let random_delete () =
    let user = List.nth users (Random.int numUser) in
    let item = List.nth items (Random.int numItem) in
    send ("deleteItemReq", [ mk_value_int user; mk_value_int item ])
  in
  let rec genOp restNum =
    if restNum <= 0 then ()
    else
      let () = Pp.printf "@{<yellow>restNum@}: %i\n" restNum in
      if Random.bool () then random_add () else random_delete ();
      genOp (restNum - 1)
  in
  let () = genOp numOp in
  let () = Pp.printf "@{<red>End with numOp@}\n%i\n" numOp in
  Effect.perform End
