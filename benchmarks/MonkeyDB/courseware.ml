open Language
open Interpreter
open Common
module CoursewareDB = ListDB
open CoursewareDB

let enrollReqHandler (msg : msg) =
  let aux (user : int) (course : int) =
    do_trans (fun tid ->
        let enrollments = do_get tid user in
        if List.mem course enrollments then ()
        else
          let _ = do_put tid user (course :: enrollments) in
          ())
  in
  match msg.ev.args with
  | [ VConst (I user); VConst (I course) ] ->
      let () = aux user course in
      send ("enrollResp", [])
  | _ -> _die [%here]

let unenrollReqHandler (msg : msg) =
  let aux (user : int) (course : int) =
    do_trans (fun tid ->
        let enrollments = do_get tid user in
        let newEnrollments = List.filter (fun x -> x != course) enrollments in
        if List.length newEnrollments == List.length enrollments then ()
        else
          let _ = do_put tid user newEnrollments in
          ())
  in
  match msg.ev.args with
  | [ VConst (I user); VConst (I course) ] ->
      let () = aux user course in
      send ("unenrollResp", [])
  | _ -> _die [%here]

let getEnrollmentsReqHandler (msg : msg) =
  let aux (user : int) = do_trans (fun tid -> do_get tid user) in
  match msg.ev.args with
  | [ VConst (I user) ] ->
      let enrollments = aux user in
      send ("getEnrollmentsResp", [ mk_value_intList enrollments ])
  | _ -> _die [%here]

let enrollRespHandler (_ : msg) = ()
let unenrollRespHandler (_ : msg) = ()
let getEnrollmentsRespHandler (_ : msg) = ()

let init isolation_level () =
  register_async_has_ret "beginT" beginAsync;
  register_async_has_ret "commit" commitAsync;
  register_async_has_ret "get" getAsync;
  register_async_no_ret "put" putAsync;
  register_handler "enrollReq" enrollReqHandler;
  register_handler "unenrollReq" unenrollReqHandler;
  register_handler "enrollResp" enrollRespHandler;
  register_handler "unenrollResp" unenrollRespHandler;
  register_handler "getEnrollmentsReq" getEnrollmentsReqHandler;
  register_handler "getEnrollmentsResp" getEnrollmentsRespHandler;
  CoursewareDB.init isolation_level

open Nt

let record l = Ty_record { alias = None; fds = l }

let testCtx =
  Typectx.add_to_rights Typectx.emp
    ([
       "enrollReq"#:(record [ "x"#:int_ty; "y"#:int_ty ]);
       "enrollResp"#:(record []);
       "unenrollReq"#:(record [ "x"#:int_ty; "y"#:int_ty ]);
       "unenrollResp"#:(record []);
       "getEnrollmentsReq"#:(record [ "x"#:int_ty ]);
       "getEnrollmentsResp"#:(record [ "x"#:(mk_list_ty int_ty) ]);
     ]
    @ event_typectx)

let gen name args body =
  mk_term_gen testCtx name (List.map (fun x -> VVar x) args) body

let obs name k = mk_term_obs_fresh testCtx name (fun _ -> k)
let obsEnrollResp e = mk_term_obs_fresh testCtx "enrollResp" (fun _ -> e)
let obsUnenrollResp e = mk_term_obs_fresh testCtx "unenrollResp" (fun _ -> e)

let obsGetEnrollmentsResp e =
  mk_term_obs_fresh testCtx "getEnrollmentsResp" (fun _ -> e)

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

let obsGet2 tid prev_tid k =
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

let main =
  mk_term_assume_fresh_true int_ty (fun user ->
      mk_term_assume_fresh_true int_ty (fun course1 ->
          mk_term_assume_fresh int_ty
            (fun x -> Not (lit_to_prop (mk_var_eq_var [%here] course1 x)))
            (fun course2 ->
              gen "enrollReq" [ user; course1 ]
              @@ obsBegin (fun tid1 ->
                     gen "enrollReq" [ user; course2 ]
                     @@ obsBegin (fun tid2 ->
                            obsGet tid2 @@ obsGet tid1 @@ obsPut tid1
                            @@ obsPut tid2 @@ obsCommit tid2 @@ obsCommit tid1
                            @@ obsEnrollResp @@ obsEnrollResp
                            @@ gen "getEnrollmentsReq" [ user ]
                            @@ obsBegin (fun tid3 ->
                                   obsGet2 tid3 tid1 @@ obsCommit tid3
                                   @@ obsGetEnrollmentsResp mk_term_tt))))))

type courseware_bench_config = { numCourse : int; numUser : int; numOp : int }

let random_user { numCourse; numUser; numOp } =
  let users = List.init numUser (fun i -> i + 1) in
  let courses = List.init numCourse (fun i -> i + 1) in
  let random_enroll () =
    let user = List.nth users (Random.int numUser) in
    let course = List.nth courses (Random.int numCourse) in
    send ("enrollReq", [ mk_value_int user; mk_value_int course ])
  in
  let random_unenroll () =
    let user = List.nth users (Random.int numUser) in
    let course = List.nth courses (Random.int numCourse) in
    send ("unenrollReq", [ mk_value_int user; mk_value_int course ])
  in
  let random_getEnrollments () =
    let user = List.nth users (Random.int numUser) in
    send ("getEnrollmentsReq", [ mk_value_int user ])
  in
  let rec genOp restNum =
    if restNum <= 0 then ()
    else
      let () = Pp.printf "@{<yellow>restNum@}: %i\n" restNum in
      (match Random.int 3 with
      | 0 -> random_getEnrollments ()
      | 1 -> random_enroll ()
      | _ -> random_unenroll ());
      genOp (restNum - 1)
  in
  let () = genOp numOp in
  let () = Pp.printf "@{<red>End with numOp@}\n%i\n" numOp in
  Effect.perform End
