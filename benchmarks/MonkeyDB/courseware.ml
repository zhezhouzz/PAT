open Language
module CoursewareDB = CoursewareDB
open CoursewareDB

let gen name args body =
  mk_term_gen testCtx name (List.map (fun x -> VVar x) args) body

let obs name k = mk_term_obs_fresh testCtx name (fun _ -> k)

let obsRegisterStudentResp e = 
  mk_term_obs_fresh testCtx "registerStudentResp" (fun _ -> e)

let obsDeregisterStudentResp e = 
  mk_term_obs_fresh testCtx "deregisterStudentResp" (fun _ -> e)

let obsCreateCourseResp e = 
  mk_term_obs_fresh testCtx "createCourseResp" (fun _ -> e)

let obsDeleteCourseResp e = 
  mk_term_obs_fresh testCtx "deleteCourseResp" (fun _ -> e)

let obsEnrollStudentResp e =
  mk_term_obs_fresh testCtx "enrollStudentResp" (fun _ -> e)

let obsGetEnrollmentsResp e = 
  mk_term_obs_fresh testCtx "getEnrollmentsResp" (fun _ -> e)

let obsBegin k =
  mk_term_obs_fresh testCtx "beginT" (function
    | tid' :: _ -> k tid'
    | _ -> _die [%here])

(* Helper functions to create obs functions*)

let mk_obs name testCtx tid k =
  mk_term_obs_prop_fresh testCtx name (function
    | tid' :: _ ->
        let prop = lit_to_prop (mk_var_eq_var [%here] tid tid') in
        (prop, k)
    | _ -> _die [%here])

let mk_obs_prev name testCtx tid prev_tid k =
  mk_term_obs_prop_fresh testCtx name (function
    | tid' :: _ :: prev_tid' :: _ ->
        let prop1 = lit_to_prop (mk_var_eq_var [%here] tid tid') in
        let prop2 = lit_to_prop (mk_var_eq_var [%here] prev_tid prev_tid') in
        (And [ prop1; prop2 ], k)
    | _ -> _die [%here])
    
(* Obs functions *)

let obsCommit tid k = mk_obs "commit" testCtx tid k
let obsGetStudents tid k = mk_obs "getStudents" testCtx tid k
let obsPutStudents tid k = mk_obs "putStudents" testCtx tid k
let obsGetCourses tid k = mk_obs "getCourses" testCtx tid k
let obsPutCourses tid k = mk_obs "putCourses" testCtx tid k
let obsGetEnrollments tid k = mk_obs "getEnrollments" testCtx tid k
let obsPutEnrollments tid k = mk_obs "putEnrollments" testCtx tid k

let obsGetStudentsPrev tid prev_tid k = mk_obs_prev "getStudents" testCtx tid prev_tid k
let obsGetCoursesPrev tid prev_tid k = mk_obs_prev "getCourses" testCtx tid prev_tid k
let obsGetEnrollmentsPrev tid prev_tid k = mk_obs_prev "getEnrollments" testCtx tid prev_tid k

(* let main =
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
                                   @@ obsGetEnrollmentsResp mk_term_tt)))))) *)

type courseware_bench_config = { 
  numStudent : int; 
  numCourse : int; 
  numOp : int 
}

let num_connection = 3

let random_operations { numStudent; numCourse; numOp } =
  let open Lwt.Syntax in
  let students = List.init numStudent (fun i -> i + 1) in
  let courses = List.init numCourse (fun i -> i + 1) in
  
  let random_register_student ~thread_id () =
    let student = List.nth students (Random.int numStudent) in
    let* _ = async_register_student ~thread_id student () in
    Lwt.return_unit
  in
  
  let random_deregister_student ~thread_id () =
    let student = List.nth students (Random.int numStudent) in
    let* _ = async_deregister_student ~thread_id student () in
    Lwt.return_unit
  in
  
  let random_create_course ~thread_id () =
    let course = List.nth courses (Random.int numCourse) in
    let* _ = async_create_course ~thread_id course () in
    Lwt.return_unit
  in
  
  let random_delete_course ~thread_id () =
    let course = List.nth courses (Random.int numCourse) in
    let* _ = async_delete_course ~thread_id course () in
    Lwt.return_unit
  in
  
  let random_enroll_student ~thread_id () =
    let student = List.nth students (Random.int numStudent) in
    let course = List.nth courses (Random.int numCourse) in
    let* _ = async_enroll_student ~thread_id student course () in
    Lwt.return_unit
  in
  
  let random_get_enrollments ~thread_id () =
    let student = List.nth students (Random.int numStudent) in
    let* _ = async_get_enrollments ~thread_id student () in
    Lwt.return_unit
  in
  
  let random_option ~thread_id () =
    match Random.int 6 with
    | 0 -> random_register_student ~thread_id ()
    | 1 -> random_deregister_student ~thread_id ()
    | 2 -> random_create_course ~thread_id ()
    | 3 -> random_delete_course ~thread_id ()
    | 4 -> random_enroll_student ~thread_id ()
    | _ -> random_get_enrollments ~thread_id ()
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
  
  let threads = List.init num_connection (fun thread_id ->
    genOp ~thread_id numOp
  ) in
  
  let () = Lwt_main.run @@ Lwt.join threads in
  ()
