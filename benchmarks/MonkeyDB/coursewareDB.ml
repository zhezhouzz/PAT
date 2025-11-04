open Language
open Interpreter
open Common
module D = MyDB (Config)
include D

(* 
courseware db operations:
  register student
  create course
  enroll student in course

4 tables: 
  students: student_id -> is registered (bool)
  courses: course_id -> exists (bool)
  student_enrollments: student_id -> list of course_ids

  had to remove deregister student and delete course because they couldn't be 
  translated to the spec; also removed course_enrollments table because adding 
  puts and gets to it broke the spec somehow (made synthesis never terminate)
*)

let dbname = "courseware"
let getStudentsAsync (ev : ev) = _getAsync "students" ev
let putStudentsAsync (ev : ev) = _putAsync "students" ev
let getCoursesAsync (ev : ev) = _getAsync "courses" ev
let putCoursesAsync (ev : ev) = _putAsync "courses" ev
let getStudentEnrollmentsAsync (ev : ev) = _getAsync "student_enrollments" ev
let putStudentEnrollmentsAsync (ev : ev) = _putAsync "student_enrollments" ev

let do_getStudents tid student_id =
  let msg =
    async ("getStudents", [ mk_value_int tid; mk_value_int student_id ])
  in
  match msg.ev.args with _ :: _ :: _ :: _ :: args -> args | _ -> _die [%here]

let do_getCourses tid course_id =
  let msg =
    async ("getCourses", [ mk_value_int tid; mk_value_int course_id ])
  in
  match msg.ev.args with _ :: _ :: _ :: _ :: args -> args | _ -> _die [%here]

let do_getStudentEnrollments tid student_id =
  let msg =
    async
      ("getStudentEnrollments", [ mk_value_int tid; mk_value_int student_id ])
  in
  match msg.ev.args with _ :: _ :: _ :: _ :: args -> args | _ -> _die [%here]

let do_putStudents tid student_id v =
  async ("putStudents", [ mk_value_int tid; mk_value_int student_id ] @ v)

let do_putCourses tid course_id v =
  async ("putCourses", [ mk_value_int tid; mk_value_int course_id ] @ v)

let do_putStudentEnrollments tid student_id v =
  async
    ("putStudentEnrollments", [ mk_value_int tid; mk_value_int student_id ] @ v)

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

let bool_to_values b = [ VConst (B b) ]
let values_to_bool l = match l with [ VConst (B b) ] -> b | _ -> _die [%here]

(* Helper functions *)
let async_is_student_registered ~tid ~student_id =
  let open Lwt.Syntax in
  Lwt.catch
    (fun () ->
      let* _, _, student_json =
        DB.table_async_get ~db:dbname ~tid ~table:"students"
          ~key:(string_of_int student_id) ()
      in
      Lwt.return (values_to_bool (Config.json_to_values student_json)))
    (function
      | BackendMariaDB.DBKeyNotFound _ -> Lwt.return false | exn -> Lwt.fail exn)

let is_student_registered ~tid ~student_id =
  try values_to_bool (do_getStudents tid student_id) with _ -> false

let async_is_course_created ~tid ~course_id =
  let open Lwt.Syntax in
  Lwt.catch
    (fun () ->
      let* _, _, course_json =
        DB.table_async_get ~db:dbname ~tid ~table:"courses"
          ~key:(string_of_int course_id) ()
      in
      Lwt.return (values_to_bool (Config.json_to_values course_json)))
    (function
      | BackendMariaDB.DBKeyNotFound _ -> Lwt.return false | exn -> Lwt.fail exn)

let is_course_created ~tid ~course_id =
  try values_to_bool (do_getCourses tid course_id) with _ -> false

(* Courseware operations *)

(* Register a student *)
let async_register_student ~thread_id student_id () =
  let open Lwt.Syntax in
  let* tid = DB.async_begin ~thread_id () in
  (* Check if student already registered *)
  let* registered = async_is_student_registered ~tid ~student_id in
  if registered then
    let* _ = DB.async_release_connection ~tid () in
    Lwt.return_false
  else
    (* Register student *)
    let* () =
      DB.table_async_put ~db:dbname ~tid ~table:"students"
        ~key:(string_of_int student_id)
        ~json:(Config.values_to_json [ VConst (B true) ])
        ()
    in
    (* Initialize empty enrollment list *)
    let* () =
      DB.table_async_put ~db:dbname ~tid ~table:"student_enrollments"
        ~key:(string_of_int student_id)
        ~json:(Config.values_to_json [ VCIntList [] ])
        ()
    in
    let* _ = DB.async_commit ~tid () in
    Lwt.return_true

let registerStudentReqHandler (msg : msg) =
  let aux (student_id : int) =
    do_trans (fun tid ->
        if is_student_registered ~tid ~student_id then false
        else
          let _ = do_putStudents tid student_id (bool_to_values true) in
          let _ =
            do_putStudentEnrollments tid student_id (int_list_to_values [])
          in
          true)
  in
  match msg.ev.args with
  | [ VConst (I student_id) ] ->
      let success = aux student_id in
      send ("registerStudentResp", [ VConst (B success) ])
  | _ -> _die [%here]

let async_create_course ~thread_id course_id () =
  let open Lwt.Syntax in
  let* tid = DB.async_begin ~thread_id () in
  let* created = async_is_course_created ~tid ~course_id in
  if created then
    let* _ = DB.async_release_connection ~tid () in
    Lwt.return_false
  else
    (* Course does not exist, create course *)
    let* () =
      DB.table_async_put ~db:dbname ~tid ~table:"courses"
        ~key:(string_of_int course_id)
        ~json:(Config.values_to_json [ VConst (B true) ])
        ()
    in
    let* _ = DB.async_commit ~tid () in
    Lwt.return_true

let createCourseReqHandler (msg : msg) =
  let aux (course_id : int) =
    do_trans (fun tid ->
        if is_course_created ~tid ~course_id then false
        else
          let _ = do_putCourses tid course_id (bool_to_values true) in
          true)
  in
  match msg.ev.args with
  | [ VConst (I course_id) ] ->
      let success = aux course_id in
      send ("createCourseResp", [ VConst (B success) ])
  | _ -> _die [%here]

let async_enroll_student ~thread_id student_id course_id () =
  let open Lwt.Syntax in
  let* tid = DB.async_begin ~thread_id () in
  (* Make sure student and course both exist *)
  let* student_registered = async_is_student_registered ~tid ~student_id in
  let* course_exists = async_is_course_created ~tid ~course_id in
  if (not student_registered) || not course_exists then
    let* _ = DB.async_release_connection ~tid () in
    Lwt.return_false
  else
    (* Student and course both exist *)
    (* Update student's enrollments *)
    let* _, _, enrollments =
      DB.table_async_get ~db:dbname ~tid ~table:"student_enrollments"
        ~key:(string_of_int student_id) ()
    in
    let oldEnrollments =
      match Config.json_to_values enrollments with
      | [ VCIntList l ] -> l
      | _ -> _die [%here]
    in
    let newEnrollments =
      if List.mem course_id oldEnrollments then oldEnrollments
      else course_id :: oldEnrollments
    in
    let* () =
      DB.table_async_put ~db:dbname ~tid ~table:"student_enrollments"
        ~key:(string_of_int student_id)
        ~json:(Config.values_to_json [ VCIntList newEnrollments ])
        ()
    in
    let* _ = DB.async_commit ~tid () in
    Lwt.return_true

let enrollStudentReqHandler (msg : msg) =
  let aux (student_id : int) (course_id : int) =
    do_trans (fun tid ->
        if
          (not (is_student_registered ~tid ~student_id))
          || not (is_course_created ~tid ~course_id)
        then false
        else
          let oldEnrollments =
            values_to_int_list (do_getStudentEnrollments tid student_id)
          in
          let newEnrollments =
            if List.mem course_id oldEnrollments then oldEnrollments
            else course_id :: oldEnrollments
          in
          let _ =
            do_putStudentEnrollments tid student_id
              (int_list_to_values newEnrollments)
          in
          true)
  in
  match msg.ev.args with
  | [ VConst (I student_id); VConst (I course_id) ] ->
      let success = aux student_id course_id in
      send ("enrollStudentResp", [ VConst (B success) ])
  | _ -> _die [%here]

let async_unenroll_student ~thread_id student_id course_id () =
  let open Lwt.Syntax in
  let* tid = DB.async_begin ~thread_id () in
  (* Make sure student and course both exist *)
  let* student_registered = async_is_student_registered ~tid ~student_id in
  let* course_exists = async_is_course_created ~tid ~course_id in
  if (not student_registered) || not course_exists then
    let* _ = DB.async_release_connection ~tid () in
    Lwt.return_false
  else
    (* Student and course both exist *)
    (* Update student's enrollments *)
    let* _, _, enrollments =
      DB.table_async_get ~db:dbname ~tid ~table:"student_enrollments"
        ~key:(string_of_int student_id) ()
    in
    let oldEnrollments =
      match Config.json_to_values enrollments with
      | [ VCIntList l ] -> l
      | _ -> _die [%here]
    in
    let newEnrollments = List.filter ((<>) course_id) oldEnrollments in
    let* () =
      DB.table_async_put ~db:dbname ~tid ~table:"student_enrollments"
        ~key:(string_of_int student_id)
        ~json:(Config.values_to_json [ VCIntList newEnrollments ])
        ()
    in
    let* _ = DB.async_commit ~tid () in
    Lwt.return_true

let unenrollStudentReqHandler (msg : msg) =
  let aux (student_id : int) (course_id : int) =
    do_trans (fun tid ->
        if
          (not (is_student_registered ~tid ~student_id))
          || not (is_course_created ~tid ~course_id)
        then false
        else
          let oldEnrollments =
            values_to_int_list (do_getStudentEnrollments tid student_id)
          in
          let newEnrollments = List.filter ((<>) course_id) oldEnrollments in
          let _ =
            do_putStudentEnrollments tid student_id
              (int_list_to_values newEnrollments)
          in
          true)
  in
  match msg.ev.args with
  | [ VConst (I student_id); VConst (I course_id) ] ->
      let success = aux student_id course_id in
      send ("enrollStudentResp", [ VConst (B success) ])
  | _ -> _die [%here]

let registerStudentRespHandler (_ : msg) = ()
let createCourseRespHandler (_ : msg) = ()
let enrollStudentRespHandler (_ : msg) = ()
let unenrollStudentRespHandler (_ : msg) = ()

let init () =
  register_async_has_ret "beginT" beginAsync;
  register_async_has_ret "commit" commitAsync;

  register_async_has_ret "getStudents" getStudentsAsync;
  register_async_no_ret "putStudents" putStudentsAsync;
  register_async_has_ret "getCourses" getCoursesAsync;
  register_async_no_ret "putCourses" putCoursesAsync;
  register_async_has_ret "getStudentEnrollments" getStudentEnrollmentsAsync;
  register_async_no_ret "putStudentEnrollments" putStudentEnrollmentsAsync;

  register_handler "registerStudentReq" registerStudentReqHandler;
  register_handler "createCourseReq" createCourseReqHandler;
  register_handler "enrollStudentReq" enrollStudentReqHandler;
  register_handler "unenrollStudentReq" unenrollStudentReqHandler;

  register_handler "registerStudentResp" registerStudentRespHandler;
  register_handler "createCourseResp" createCourseRespHandler;
  register_handler "enrollStudentResp" enrollStudentRespHandler;
  register_handler "unenrollStudentResp" unenrollStudentRespHandler;

  D.clear ()

let testCtx =
  let open Nt in
  let record l = Ty_record { alias = None; fds = l } in
  Typectx.add_to_rights Typectx.emp
    [
      "beginT"#:(record [ "tid"#:int_ty ]);
      "commit"#:(record [ "tid"#:int_ty; "cid"#:int_ty ]);
      "getStudents"#:(record
                        [
                          "tid"#:int_ty;
                          "key"#:int_ty;
                          "prev_tid"#:int_ty;
                          "prev_cid"#:int_ty;
                          "registered"#:bool_ty;
                        ]);
      "putStudents"#:(record
                        [ "tid"#:int_ty; "key"#:int_ty; "registered"#:bool_ty ]);
      "getCourses"#:(record
                       [
                         "tid"#:int_ty;
                         "key"#:int_ty;
                         "prev_tid"#:int_ty;
                         "prev_cid"#:int_ty;
                         "exists"#:bool_ty;
                       ]);
      "putCourses"#:(record [ "tid"#:int_ty; "key"#:int_ty; "exists"#:bool_ty ]);
      "getStudentEnrollments"#:(record
                                  [
                                    "tid"#:int_ty;
                                    "key"#:int_ty;
                                    "prev_tid"#:int_ty;
                                    "prev_cid"#:int_ty;
                                    "courses"#:(mk_list_ty int_ty);
                                  ]);
      "putStudentEnrollments"#:(record
                                  [
                                    "tid"#:int_ty;
                                    "key"#:int_ty;
                                    "courses"#:(mk_list_ty int_ty);
                                  ]);
      "registerStudentReq"#:(record [ "student_id"#:int_ty ]);
      "registerStudentResp"#:(record [ "success"#:bool_ty ]);
      "createCourseReq"#:(record [ "course_id"#:int_ty ]);
      "createCourseResp"#:(record [ "success"#:bool_ty ]);
      "enrollStudentReq"#:(record [ "student_id"#:int_ty; "course_id"#:int_ty ]);
      "enrollStudentResp"#:(record [ "success"#:bool_ty ]);
      "unenrollStudentReq"#:(record [ "student_id"#:int_ty; "course_id"#:int_ty ]);
      "unenrollStudentResp"#:(record [ "success"#:bool_ty ]);
    ]
