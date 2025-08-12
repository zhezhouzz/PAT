open Language
open Interpreter
open Common
module CoursewareDB = ListDB
open CoursewareDB

let enrollReqHandler (msg : msg) =
  let aux (user : int) (course : int) =
    do_trans (fun tid ->
        let enrollments = do_get tid user in
        let newEnrollments =
          if List.mem course enrollments then enrollments
          else course :: enrollments
        in
        let _ = do_put tid user newEnrollments in
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
        let _ = do_put tid user newEnrollments in
        ())
  in
  match msg.ev.args with
  | [ VConst (I user); VConst (I course) ] ->
      let () = aux user course in
      send ("unenrollResp", [])
  | _ -> _die [%here]

let enrollRespHandler (_ : msg) = ()
let unenrollRespHandler (_ : msg) = ()

let init isolation_level () =
  register_async_has_ret "begin" beginAsync;
  register_async_has_ret "commit" commitAsync;
  register_async_has_ret "get" getAsync;
  register_async_no_ret "put" putAsync;
  register_handler "enrollReq" enrollReqHandler;
  register_handler "unenrollReq" unenrollReqHandler;
  register_handler "enrollResp" enrollRespHandler;
  register_handler "unenrollResp" unenrollRespHandler;
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
     ]
    @ event_typectx)

let gen name args body =
  mk_term_gen testCtx name (List.map (fun x -> VVar x) args) body

let obs name k = mk_term_obs_fresh testCtx name (fun _ -> k)
let obsEnrollResp e = mk_term_obs_fresh testCtx "enrollResp" (fun _ -> e)
let obsUnenrollResp e = mk_term_obs_fresh testCtx "unenrollResp" (fun _ -> e)

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
      mk_term_assume_fresh int_ty mk_true (fun course ->
          gen "enrollReq" [ user; course ]
          @@ obsBegin (fun tid1 ->
                 gen "unenrollReq" [ user; course ]
                 @@ obsBegin (fun tid2 ->
                        obsGet tid2 @@ obsGet tid1 @@ obsPut tid1 @@ obsPut tid2
                        @@ obsCommit tid2 @@ obsCommit tid1 @@ obsEnrollResp
                        @@ obsUnenrollResp
                        @@ gen "enrollReq" [ user; course ]
                        @@ obsBegin (fun tid3 ->
                               obsGet tid3 @@ obsPut tid3 @@ obsCommit tid3
                               @@ obsEnrollResp mk_term_tt)))))

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
  let rec genOp restNum =
    if restNum <= 0 then ()
    else
      let () = Pp.printf "@{<yellow>restNum@}: %i\n" restNum in
      if Random.bool () then random_enroll () else random_unenroll ();
      genOp (restNum - 1)
  in
  let () = genOp numOp in
  let () = Pp.printf "@{<red>End with numOp@}\n%i\n" numOp in
  Effect.perform End
