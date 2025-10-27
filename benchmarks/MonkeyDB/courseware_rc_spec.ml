val ( == ) : 'a. 'a -> 'a -> bool
val ( >= ) : int -> int -> bool
val insert : int -> int list -> int list
val remove : int -> int list -> int list
val emp : int list -> bool
val contains : int -> int list -> bool

(* Add axioms for contains *)

let[@axiom] emp_same (l1 : int list) (l2 : int list) =
  implies (emp l1 && emp l2) (l1 == l2)

let[@axiom] insert_not_emp (x : int) (l : int list) = not (emp (insert x l))

let[@axiom] insert_not_eq (x: int) (l : int list) = not (insert x l == l)

let[@axiom] remove_insert (x : int) (l : int list) = 
  (remove x (insert x l) == l)

let[@axiom] remove_insert2 (x : int) (l : int list) = 
  (remove x (insert x (insert x l)) == insert x l)

let[@axiom] remove_emp (x : int) (l : int list) =
  implies (emp l) (remove x l == l)

let[@axiom] insert_remove_emp (x : int) (l : int list) = 
  implies (emp l) (emp (remove x (insert x l)))

let[@axiom] contains_insert (x : int) (l : int list) = 
  contains x (insert x l)

(* This assumes no duplicates, which should hold for the courseware application *)
let[@axiom] contains_insert_remove (x : int) (l : int list) = 
  not (contains x (remove x l))

let[@axiom] contains_emp (x : int) (l : int list) =
  implies (emp l) (not (contains x l))

(* Basic Typing *)

val beginT : < tid : int > [@@obs]
val commit : < tid : int ; cid : int > [@@obs]

(* Get/Put for each table *)

val getStudents : 
  < tid : int ; prevTid : int ; prevCid : int ; key : int ; value : bool >
[@@obs]
val putStudents : < tid : int ; key : int ; value : bool > [@@obs]

val getCourses : 
  < tid : int ; prevTid : int ; prevCid : int ; key : int ; value : bool >
[@@obs]
val putCourses : < tid : int ; key : int ; value : bool > [@@obs]

val getStudentEnrollments : 
  < tid : int ; prevTid : int ; prevCid : int ; key : int ; value : int list >
[@@obs]
val putStudentEnrollments : < tid : int ; key : int ; value : int list > [@@obs]

val getCourseEnrollments : 
  < tid : int ; prevTid : int ; prevCid : int ; key : int ; value : int list >
[@@obs]
val putCourseEnrollments : < tid : int ; key : int ; value : int list > [@@obs]

(* Courseware operations *)

val registerStudentReq : < student_id : int > [@@gen]
val registerStudentResp : < success : bool > [@@obs]

val deregisterStudentReq : < student_id : int > [@@gen]
val deregisterStudentResp : < success : bool > [@@obs]

val createCourseReq : < course_id : int > [@@gen]
val createCourseResp : < success : bool > [@@obs]

val deleteCourseReq : < course_id : int > [@@gen]
val deleteCourseResp : < success : bool > [@@obs]

val enrollStudentReq : < student_id : int ; course_id : int > [@@gen]
val enrollStudentResp : < success : bool > [@@obs]

val getStudentEnrollmentsReq : < student_id : int > [@@gen]
val getStudentEnrollmentsResp : < courses : int list > [@@obs]

val getCourseEnrollmentsReq : < course_id : int > [@@gen]
val getCourseEnrollmentsResp : < students : int list > [@@obs]

(* Read Committed *)
(* Invariant: For any transaction with tid = i, there does not exist a previous transaction with tid > i. *)
let beginT ?l:(i = (true : [%v: int])) =
  (starA (anyA - BeginT (tid >= i)), BeginT (tid == i), allA)

(* Invariant: Each transaction is committed exactly once; there does not exist a previous commit with cid > j. *)
let commit ?l:(i = (true : [%v: int])) ?l:(j = (true : [%v: int])) =
  ( 
    (
      starA (anyA - Commit (tid == i || cid >= j));
      BeginT (tid == i);
      starA (anyA - Commit (tid == i || cid >= j))
    ),
    Commit (tid == i && cid == j),
    starA (anyA - BeginT (tid == i) - Commit (tid == i || cid == j))
  )

let getStudents =
  [|
    (* Read most recent committed transaction *)
    (fun ?l:(i = (true : [%v: int]))
      ?l:(pi = (true : [%v: int]))
      ?l:(pj = (true : [%v: int]))
      ?l:(k = (true : [%v: int]))
      ?l:(z = (true : [%v: bool]))
    ->
      ( (starA (anyA - PutStudents (tid == i && key == k));
         PutStudents (tid == i && key == k && value == z);
         starA (anyA - PutStudents (tid == i && key == k));
         Commit (tid == pi && cid == pj);
         starA (anyA - Commit true - PutStudents (tid == i && key == k))),
        GetStudents
          (tid == i && key == k && prevTid == pi && prevCid == pj
          && (not (tid == prevTid))
          && value == z),
        allA ));
  |]

let putStudents ?l:(i = (true : [%v: int])) ?l:(k = (true : [%v: int]))
    ?l:(z = (true : [%v: bool])) =
  ( (allA;
     BeginT (tid == i);
     starA (anyA - Commit (tid == i))),
    PutStudents (tid == i && key == k && value == z),
    allA )

let getCourses =
  [|
    (* Read most recent committed transaction *)
    (fun ?l:(i = (true : [%v: int]))
      ?l:(pi = (true : [%v: int]))
      ?l:(pj = (true : [%v: int]))
      ?l:(k = (true : [%v: int]))
      ?l:(z = (true : [%v: bool]))
    ->
      ( (starA (anyA - PutCourses (tid == i && key == k));
         PutCourses (tid == i && key == k && value == z);
         starA (anyA - PutCourses (tid == i && key == k));
         Commit (tid == pi && cid == pj);
         starA (anyA - Commit true - PutCourses (tid == i && key == k))),
        GetCourses
          (tid == i && key == k && prevTid == pi && prevCid == pj
          && (not (tid == prevTid))
          && value == z),
        allA ));
  |]

let putCourses ?l:(i = (true : [%v: int])) ?l:(k = (true : [%v: int]))
    ?l:(z = (true : [%v: bool])) =
  ( (allA;
     BeginT (tid == i);
     starA (anyA - Commit (tid == i))),
    PutCourses (tid == i && key == k && value == z),
    allA )

let getStudentEnrollments =
  [|
    (* Read most recent committed transaction *)
    (fun ?l:(i = (true : [%v: int]))
      ?l:(pi = (true : [%v: int]))
      ?l:(pj = (true : [%v: int]))
      ?l:(k = (true : [%v: int]))
      ?l:(z = (true : [%v: int list]))
    ->
      ( (starA (anyA - PutStudentEnrollments (tid == i && key == k));
         PutStudentEnrollments (tid == i && key == k && value == z);
         starA (anyA - PutStudentEnrollments (tid == i && key == k));
         Commit (tid == pi && cid == pj);
         starA (anyA - Commit true - PutStudentEnrollments (tid == i && key == k))),
        GetStudentEnrollments
          (tid == i && key == k && prevTid == pi && prevCid == pj
          && (not (tid == prevTid))
          && value == z),
        allA ));
  |]

let putStudentEnrollments ?l:(i = (true : [%v: int])) ?l:(k = (true : [%v: int]))
    ?l:(z = (true : [%v: int list])) =
  ( (allA;
     BeginT (tid == i);
     starA (anyA - Commit (tid == i))),
    PutStudentEnrollments (tid == i && key == k && value == z),
    allA )

let getCourseEnrollments =
  [|
    (* Read most recent committed transaction *)
    (fun ?l:(i = (true : [%v: int]))
      ?l:(pi = (true : [%v: int]))
      ?l:(pj = (true : [%v: int]))
      ?l:(k = (true : [%v: int]))
      ?l:(z = (true : [%v: int list]))
    ->
      ( (starA (anyA - PutCourseEnrollments (tid == i && key == k));
         PutCourseEnrollments (tid == i && key == k && value == z);
         starA (anyA - PutCourseEnrollments (tid == i && key == k));
         Commit (tid == pi && cid == pj);
         starA (anyA - Commit true - PutCourseEnrollments (tid == i && key == k))),
        GetCourseEnrollments
          (tid == i && key == k && prevTid == pi && prevCid == pj
          && (not (tid == prevTid))
          && value == z),
        allA ));
  |]

let putCourseEnrollments ?l:(i = (true : [%v: int])) ?l:(k = (true : [%v: int]))
    ?l:(z = (true : [%v: int list])) =
  ( (allA;
     BeginT (tid == i);
     starA (anyA - Commit (tid == i))),
    PutCourseEnrollments (tid == i && key == k && value == z),
    allA )

(* Courseware operations *)

let registerStudentReq (i : int) 
  ?l:(x = (true : [%v: int])) 
  ?l:(y = (true : [%v: bool])) =
  (
    allA,
    RegisterStudentReq (student_id == x),
    (
      BeginT (tid == i);
      GetStudents (tid == i && key == x);
      PutStudents (tid == i && key == x && value == true);
      PutStudentEnrollments (tid == i && key == x && emp value);
      Commit (tid == i);
      RegisterStudentResp (success == y);
      allA
    )
  )

let registerStudentResp ?l:(x = (true : [%v: bool])) = 
  (
    allA, 
    RegisterStudentResp (success == x), 
    allA
  )

let deregisterStudentReq (i : int) (l : int list)
  ?l:(x = (true : [%v: int]))
  ?l:(y = (true : [%v: bool])) = 
  (
    allA,
    DeregisterStudentReq (student_id == x),
    (
      BeginT (tid == i);
      GetStudents (tid == i && key == x);
      PutStudents (tid == i && key == x && value == false);
      GetStudentEnrollments (tid == i && key == x && value == l);
      (* TODO: figure out how to encode the loop *)
      (* starA (

      ); *)
      PutStudentEnrollments (tid == i && key == x && emp value);
      Commit (tid == i);
      DeregisterStudentResp (success == y);
      allA
    )
  )

let deregisterStudentResp ?l:(x = (true : [%v: bool])) = 
  (
    allA, 
    DeregisterStudentResp (success == x), 
    allA
  )

let createCourseReq (i : int) ?l:(x = (true : [%v: int])) = 
  (
    allA,
    CreateCourseReq (course_id == x),
    (
      BeginT (tid == i);
      GetCourses (tid == i && key == x);
      PutCourses (tid == i && key == x && value == true);
      PutCourseEnrollments (tid == i && key == x && emp value);
      Commit (tid == i);
      CreateCourseResp (success == true);
      allA
    )
  )

let createCourseResp ?l:(x = (true : [%v: bool])) = 
  (
    allA,
    CreateCourseResp (success == x),
    allA
  )

let deleteCourseReq (i : int) (l : int list)
  ?l:(x = (true : [%v: int])) =
  (
    allA,
    DeleteCourseReq (course_id == x),
    (
      BeginT (tid == i);
      GetCourses (tid == i && key == x);
      PutCourses (tid == i && key == x && value == false);
      GetCourseEnrollments (tid == i && key == x && value == l);
      (* TODO: loop *)
      (* starA (

      ); *)
      PutCourseEnrollments (tid == i && key == x && emp value);
      Commit (tid == i);
      DeleteCourseResp (success == true);
      allA
    )
  )

let deleteCourseResp ?l:(x = (true : [%v: bool])) = 
  (
    allA,
    DeleteCourseResp (success == x),
    allA
  )

let enrollStudentReq (i : int) (l_1 : int list) (l_2 : int list)
  ?l:(x = (true : [%v: int]))
  ?l:(y = (true : [%v: int])) =
  (
    allA,
    EnrollStudentReq (student_id == x && course_id == y),
    (
      BeginT (tid == i);
      GetStudents (tid == i && key == x);
      GetCourses (tid == i && key == y);
      GetStudentEnrollments (tid == i && key == x && value == l_1);
      PutStudentEnrollments (tid == i && key == x && value == insert y l_1);
      GetCourseEnrollments (tid == i && key == y && value == l_2);
      PutCourseEnrollments (tid == i && key == y && value == insert x l_2);
      Commit (tid == i);
      EnrollStudentResp (success == true);
      allA
    )
  )

let enrollStudentResp ?l:(x = (true : [%v: bool])) = 
  (
    allA,
    EnrollStudentResp (success == x),
    allA
  )
  
let getStudentEnrollmentsReq (i : int) (l: int list)
  ?l:(x = (true : [%v: int])) =
  (
    allA,
    GetStudentEnrollmentsReq (student_id == x),
    (
      BeginT (tid == i);
      GetStudents (tid == i && key == x);
      GetStudentEnrollments (tid == i && key == x && value == l);
      Commit (tid == i);
      GetStudentEnrollmentsResp (courses == l);
      allA
    )
  )

let getStudentEnrollmentsResp ?l:(x = (true : [%v: int list])) =
  (
    allA,
    GetStudentEnrollmentsResp (courses == x),
    allA
  )

let getCourseEnrollmentsReq (i : int) (l : int list)
  ?l:(x = (true : [%v: int])) = 
  (
    allA,
    GetCourseEnrollmentsReq (course_id == x),
    (
      BeginT (tid == i);
      GetCourses (tid == i && key == x);
      GetCourseEnrollments (tid == i && key == x && value == l);
      Commit (tid == i);
      GetCourseEnrollmentsResp (students == l);
      allA
    )
  )

let getCourseEnrollmentsResp ?l:(x = (true : [%v: int list])) =
  (
    allA, 
    GetCourseEnrollmentsResp (students == x),
    allA
  )

(* Goals *)

(* Register a student, check if they are registered *)
let[@goal] courseware_rc (x : int) =
  allA;
  RegisterStudentReq (student_id == x);
  starA (anyA - DeregisterStudentReq (student_id == x));
  GetStudents (key == x && not (value == true));
  allA

(* Enroll student, check if they are enrolled *)
let[@goal] courseware_rc_enroll (x : int) (y : int) =
  allA;
  RegisterStudentReq (student_id == x);
  EnrollStudentReq (student_id == x && course_id == y);
  starA (anyA - DeregisterStudentReq (student_id == x) - DeleteCourseReq (course_id == y));
  GetStudentEnrollments (key == x && not (contains y value));
  allA

(* Enroll student, delete course, check if student is still enrolled *)
let[@goal] courseware_rc_enroll_delete (x : int) (y : int) =
  allA;
  RegisterStudentReq (student_id == x);
  EnrollStudentReq (student_id == x && course_id == y);
  DeleteCourseReq (course_id == y);
  starA (anyA - DeregisterStudentReq (student_id == x));
  GetStudentEnrollments (key == x && contains y value);
  allA