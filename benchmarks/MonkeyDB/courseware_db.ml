open Language
open Interpreter
open Common

module CoursewareDB = struct
  module D = MyDB (Config)
  include D

  (* courseware db operations:
    register student
    deregister student
    create course
    delete course
    enroll student in course
    get enrollments for a student
  *)

  (*
  3 tables: 
    students: student_id -> is registered (bool)
    courses: course_id -> exists (bool)
    enrollments: student_id -> list of course_ids
  *)

  let getStudentsAsync (ev : ev) = _getAsync "students" ev
  let putStudentsAsync (ev : ev) = _putAsync "students" ev
  let getCoursesAsync (ev : ev) = _getAsync "courses" ev
  let putCoursesAsync (ev : ev) = _putAsync "courses" ev
  let getEnrollmentsAsync (ev : ev) = _getAsync "enrollments" ev
  let putEnrollmentsAsync (ev : ev) = _putAsync "enrollments" ev

  let do_getStudents tid student_id =
    let msg = async ("getStudents", [ mk_value_int tid; mk_value_int student_id ]) in
    match msg.ev.args with
    | _ :: _ :: _ :: _ :: args -> args
    | _ -> _die [%here]

  let do_getCourses tid course_id =
    let msg = async ("getCourses", [ mk_value_int tid; mk_value_int course_id ]) in
    match msg.ev.args with
    | _ :: _ :: _ :: _ :: args -> args
    | _ -> _die [%here]

  let do_getEnrollments tid student_id =
    let msg = async ("getEnrollments", [ mk_value_int tid; mk_value_int student_id ]) in
    match msg.ev.args with
    | _ :: _ :: _ :: _ :: args -> args
    | _ -> _die [%here]

  let do_putStudents tid student_id v =
    async ("putStudents", [ mk_value_int tid; mk_value_int student_id ] @ v)

  let do_putCourses tid course_id v =
    async ("putCourses", [ mk_value_int tid; mk_value_int course_id ] @ v)

  let do_putEnrollments tid student_id v =
    async ("putEnrollments", [ mk_value_int tid; mk_value_int student_id ] @ v)

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

  let values_to_bool l =
    match l with [ VConst (B b) ] -> b | _ -> _die [%here]

  (* Register a student *)
  let async_register_student ~thread_id student_id () =
    let open Lwt.Syntax in
    let* tid = DB.async_begin ~thread_id () in
    try
      let* _, _, _ = 
        DB.async_get ~tid ~table:"students" ~key:(string_of_int student_id) ()
      in
      (* Student already exists *)
      let* _ = DB.async_release_connection ~tid () in
      Lwt.return_false
    with BackendMariaDB.DBKeyNotFound _ ->
      let* () = 
        DB.async_put ~tid ~table:"students" ~key:(string_of_int student_id)
          ~json:(Config.values_to_json [ VConst (B true) ])
          ()
      in
      (* Initialize empty enrollment list *)
      let* () = 
        DB.async_put ~tid ~table:"enrollments" ~key:(string_of_int student_id)
          ~json:(Config.values_to_json [ VCIntList [] ])
          ()
      in
      let* _ = DB.async_commit ~tid () in
      Lwt.return_true

  let registerStudentReqHandler (msg : msg) =
    let aux (student_id : int) =
      do_trans (fun tid ->
          try
            let _ = do_getStudents tid student_id in
            false (* Student already exists *)
          with _ ->
            (* Create student *)
            let _ = do_putStudents tid student_id (bool_to_values true) in
            let _ = do_putEnrollments tid student_id (int_list_to_values []) in
            true)
    in
    match msg.ev.args with
    | [ VConst (I student_id) ] ->
        let success = aux student_id in
        send ("registerStudentResp", [ VConst (B success) ])
    | _ -> _die [%here]

  let async_deregister_student ~thread_id student_id () =
    let open Lwt.Syntax in
    let* tid = DB.async_begin ~thread_id () in
    try
      (* Check that student exists before deregistering*)
      let* _, _, _ = 
        DB.async_get ~tid ~table:"students" ~key:(string_of_int student_id) ()
      in
      (* Deregister student and clear their enrollments *)
      let* () = 
        DB.async_put ~tid ~table:"students" ~key:(string_of_int student_id)
        ~json:(Config.values_to_json [ VConst (B false) ])
        ()
      in
      let* () = 
        DB.async_put ~tid ~table:"enrollments" ~key:(string_of_int student_id)
        ~json:(Config.values_to_json [ VCIntList [] ])
        ()
      in
      let* _ = DB.async_commit ~tid () in
      Lwt.return_true
    with BackendMariaDB.DBKeyNotFound _ ->
      let* _ = DB.async_release_connection ~tid () in
      Lwt.return_false

  let deregisterStudentReqHandler (msg : msg) =
    let aux (student_id : int) =
      do_trans (fun tid ->
          try
            let _ = do_getStudents tid student_id in
            let _ = do_putStudents tid student_id (bool_to_values false) in
            true
          with _ ->
            false)
    in
    match msg.ev.args with
    | [ VConst (I student_id) ] ->
        let success = aux student_id in
        send ("deregisterStudentResp", [ VConst (B success) ])
    | _ -> _die [%here]

  let async_create_course ~thread_id course_id () =
    let open Lwt.Syntax in
    let* tid = DB.async_begin ~thread_id () in
    try
      let* _, _, _ = 
        DB.async_get ~tid ~table:"courses" ~key:(string_of_int course_id) ()
      in
      (* Course already exists *)
      let* _ = DB.async_release_connection ~tid () in
      Lwt.return_false
    with BackendMariaDB.DBKeyNotFound _ ->
      let* () = 
        DB.async_put ~tid ~table:"courses" ~key:(string_of_int course_id)
          ~json:(Config.values_to_json [ VConst (B true) ])
          ()
      in
      let* _ = DB.async_commit ~tid () in
      Lwt.return_true

  let createCourseReqHandler (msg : msg) =
    let aux (course_id : int) =
      do_trans (fun tid ->
          try
            let _ = do_getCourses tid course_id in
            false (* Course already exists *)
          with _ ->
            let _ = do_putCourses tid course_id (bool_to_values true) in
            true)
    in
    match msg.ev.args with
    | [ VConst (I course_id) ] ->
        let success = aux course_id in
        send ("createCourseResp", [ VConst (B success) ])
    | _ -> _die [%here]

  let async_delete_course ~thread_id course_id () =
    let open Lwt.Syntax in
    let* tid = DB.async_begin ~thread_id () in
    try
      let* _, _, _ = 
        DB.async_get ~tid ~table:"courses" ~key:(string_of_int course_id) ()
      in
      let* () = 
        DB.async_put ~tid ~table:"courses" ~key:(string_of_int course_id)
          ~json:(Config.values_to_json [ VConst (B false) ])
          ()
      in
      let* _ = DB.async_commit ~tid () in
      Lwt.return_true
    with BackendMariaDB.DBKeyNotFound _ ->
      let* _ = DB.async_release_connection ~tid () in
      Lwt.return_false

  let deleteCourseReqHandler (msg : msg) =
    let aux (course_id : int) =
      do_trans (fun tid ->
          try
            let _ = do_getCourses tid course_id in
            let _ = do_putCourses tid course_id (bool_to_values false) in
            true
          with _ ->
            false)
    in
    match msg.ev.args with
    | [ VConst (I course_id) ] ->
        let success = aux course_id in
        send ("deleteCourseResp", [ VConst (B success) ])
    | _ -> _die [%here]

  let async_enroll_student ~thread_id student_id course_id () =
    let open Lwt.Syntax in
    let* tid = DB.async_begin ~thread_id () in
    try
      (* Make sure student and course both exist *)
      let* _, _, student_value = 
        DB.async_get ~tid ~table:"students" ~key:(string_of_int student_id) ()
      in
      let* _, _, course_value = 
        DB.async_get ~tid ~table:"courses" ~key:(string_of_int course_id) ()
      in
      let student_is_registered = 
        values_to_bool (Config.json_to_values student_value) in
      let course_exists = 
        values_to_bool (Config.json_to_values course_value) in
      if not student_is_registered || not course_exists then (
        let* _ = DB.async_release_connection ~tid () in
        Lwt.return_false
      ) else
      (* Student and course both exist, update enrollments *)
      let* _, _, enrollments = 
        DB.async_get ~tid ~table:"enrollments" ~key:(string_of_int student_id) ()
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
        DB.async_put ~tid ~table:"enrollments" ~key:(string_of_int student_id)
          ~json:(Config.values_to_json [ VCIntList newEnrollments ])
          ()
      in
      let* _ = DB.async_commit ~tid () in
      Lwt.return_true
    with BackendMariaDB.DBKeyNotFound _ ->
      let* _ = DB.async_release_connection ~tid () in
      Lwt.return_false

  let enrollStudentReqHandler (msg : msg) =
    let aux (student_id : int) (course_id : int) =
      do_trans (fun tid ->
          try
            let student_is_registered = values_to_bool (do_getStudents tid student_id) in
            let course_exists = values_to_bool (do_getCourses tid course_id) in
            if not student_is_registered || not course_exists then
              false
            else
            let oldEnrollments = values_to_int_list (do_getEnrollments tid student_id) in
            let newEnrollments = 
              if List.mem course_id oldEnrollments then oldEnrollments 
              else course_id :: oldEnrollments
            in
            let _ = do_putEnrollments tid student_id (int_list_to_values newEnrollments) in
            true
          with _ ->
            false)
    in
    match msg.ev.args with
    | [ VConst (I student_id); VConst (I course_id) ] ->
        let success = aux student_id course_id in
        send ("enrollStudentResp", [ VConst (B success) ])
    | _ -> _die [%here]

  let getEnrollmentsReqHandler (msg : msg) =
    let aux (student_id : int) =
      do_trans (fun tid ->
          try
            let enrollments = values_to_int_list (do_getEnrollments tid student_id) in
            Some enrollments
          with _ ->
            None)
    in
    match msg.ev.args with
    | [ VConst (I student_id) ] ->
        (match aux student_id with
         | Some enrollments -> 
             send ("getEnrollmentsResp", [ mk_value_intList enrollments ])
         | None -> 
             send ("getEnrollmentsResp", [ mk_value_intList [] ]))
    | _ -> _die [%here]

  let registerStudentRespHandler (_ : msg) = ()
  let deregisterStudentRespHandler (_ : msg) = ()
  let createCourseRespHandler (_ : msg) = ()
  let deleteCourseRespHandler (_ : msg) = ()
  let enrollStudentRespHandler (_ : msg) = ()
  let getEnrollmentsRespHandler (_ : msg) = ()

  let init () =
    register_async_has_ret "beginT" beginAsync;
    register_async_has_ret "commit" commitAsync;

    register_async_has_ret "getStudents" getStudentsAsync;
    register_async_no_ret "putStudents" putStudentsAsync;
    register_async_has_ret "getCourses" getCoursesAsync;
    register_async_no_ret "putCourses" putCoursesAsync;
    register_async_has_ret "getEnrollments" getEnrollmentsAsync;
    register_async_no_ret "putEnrollments" putEnrollmentsAsync;

    register_handler "registerStudentReq" registerStudentReqHandler;
    register_handler "deregisterStudentReq" deregisterStudentReqHandler;
    register_handler "createCourseReq" createCourseReqHandler;
    register_handler "deleteCourseReq" deleteCourseReqHandler;
    register_handler "enrollStudentReq" enrollStudentReqHandler;
    register_handler "getEnrollmentsReq" getEnrollmentsReqHandler;

    register_handler "registerStudentResp" registerStudentRespHandler;
    register_handler "deregisterStudentResp" deregisterStudentRespHandler;
    register_handler "createCourseResp" createCourseRespHandler;
    register_handler "deleteCourseResp" deleteCourseRespHandler;
    register_handler "enrollStudentResp" enrollStudentRespHandler;
    register_handler "getEnrollmentsResp" getEnrollmentsRespHandler;
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
                            [ 
                              "tid"#:int_ty; 
                              "key"#:int_ty; 
                              "registered"#:bool_ty; 
                            ]);
        "getCourses"#:(record
                            [
                              "tid"#:int_ty;
                              "key"#:int_ty;
                              "prev_tid"#:int_ty;
                              "prev_cid"#:int_ty;
                              "exists"#:bool_ty;
                            ]);
        "putCourses"#:(record
                            [ 
                              "tid"#:int_ty; 
                              "key"#:int_ty; 
                              "exists"#:bool_ty 
                            ]);
        "getEnrollments"#:(record
                            [
                              "tid"#:int_ty;
                              "key"#:int_ty;
                              "prev_tid"#:int_ty;
                              "prev_cid"#:int_ty;
                              "courses"#:(mk_list_ty int_ty);
                            ]);
        "putEnrollments"#:(record
                            [ 
                              "tid"#:int_ty; 
                              "key"#:int_ty; 
                              "courses"#:(mk_list_ty int_ty) 
                            ]);

        "registerStudentReq"#:(record [ "student_id"#:int_ty ]);
        "registerStudentResp"#:(record [ "success"#:bool_ty ]);
        "deregisterStudentReq"#:(record [ "student_id"#:int_ty ]);
        "deregisterStudentResp"#:(record [ "success"#:bool_ty ]);
        "createCourseReq"#:(record [ "course_id"#:int_ty ]);
        "createCourseResp"#:(record [ "success"#:bool_ty ]);
        "deleteCourseReq"#:(record [ "course_id"#:int_ty ]);
        "deleteCourseResp"#:(record [ "success"#:bool_ty ]);
        "enrollStudentReq"#:(record [ "student_id"#:int_ty; "course_id"#:int_ty ]);
        "enrollStudentResp"#:(record [ "success"#:bool_ty ]);
        "getEnrollmentsReq"#:(record [ "student_id"#:int_ty ]);
        "getEnrollmentsResp"#:(record [ "courses"#:(mk_list_ty int_ty) ]);
      ]
end