val ( == ) : 'a. 'a -> 'a -> bool
val ( >= ) : int -> int -> bool
val insert : int -> int list -> int list
val remove : int -> int list -> int list
val emp : int list -> bool

let[@axiom] emp_same (l1 : int list) (l2 : int list) =
  implies (emp l1 && emp l2) (l1 == l2)

let[@axiom] insert_not_emp (x : int) (l : int list) = not (emp (insert x l))

let[@axiom] remove_insert (x : int) (l : int list) =
  implies (emp l) (remove x (insert x l) == l)

let[@axiom] remove_emp (x : int) (l : int list) =
  implies (emp l) (remove x l == l)

(* Basic Typing *)

val beginT : < tid : int > [@@obs]
val commit : < tid : int ; cid : int > [@@obs]

val get :
  < tid : int ; prevTid : int ; prevCid : int ; key : int ; value : int list >
[@@obs]
val put : < tid : int ; key : int ; value : int list > [@@obs]

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
  ( (starA (anyA - Commit (tid == i || cid >= j));
     BeginT (tid == i);
     starA (anyA - Commit (tid == i || cid >= j))),
    Commit (tid == i && cid == j),
    starA (anyA - Put (tid == i) - Get (tid == i)) )

let put ?l:(i = (true : [%v: int])) ?l:(k = (true : [%v: int]))
    ?l:(z = (true : [%v: int list])) =
  ( (allA;
     BeginT (tid == i);
     starA (anyA - Commit (tid == i))),
    Put (tid == i && key == k && value == z),
    allA )

let get =
  [|
    (* Read most recent committed transaction *)
    (fun ?l:(i = (true : [%v: int]))
      ?l:(pi = (true : [%v: int]))
      ?l:(pj = (true : [%v: int]))
      ?l:(k = (true : [%v: int]))
      ?l:(z = (true : [%v: int list]))
    ->
      ( (starA (anyA - Put (tid == i && key == k));
         Put (tid == i && key == k && value == z);
         starA (anyA - Put (tid == i && key == k));
         Commit (tid == pi && cid == pj);
         starA (anyA - Commit true - Put (tid == i && key == k))),
        Get
          (tid == i && key == k && prevTid == pi && prevCid == pj
          && (not (tid == prevTid))
          && value == z),
        allA ));
  |]

(* Courseware operations *)

let registerStudentReq (i : int) ?l:(x = (true : [%v: int])) = 
  (
    allA,
    RegisterStudentReq (student_id == x),
    (
      BeginT (tid == i);
      GetStudents (tid == i && key == x);
      PutStudents (tid == i && key == x && value == true);
      PutStudentEnrollments (tid == i && key == x && emp value);
      Commit (tid == i);
      RegisterStudentResp (success == true);
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
  ?l:(x = (true : [%v: int])) = 
  (
    allA,
    DeregisterStudentReq (student_id == x),
    (
      BeginT (tid == i);
      GetStudents (tid == i && key == x);
      PutStudents (tid == i && key == x && value == false);
      GetStudentEnrollments (tid == i && key == x && value == l);
      (* TODO: figure out how to encode the loop *)
      star A (

      );
      PutStudentEnrollments (tid == i && key == x && emp value);
      Commit (tid == i);
      DeregisterStudentReq (success == true);
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
      starA (

      );
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
      GetStudents (tid == i && value == x);
      GetCourses (tid == i && value == y);
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

let GetStudentEnrollmentsResp ?l:(x = (true : [%v: int list])) =
  (
    allA,
    GetStudentEnrollmentsResp (courses == l),
    allA
  )

let getCourseEnrollmentsReq (i : int) (l : int list)
  ?l:(x = (true : [%v: int])) = 
  (
    allA,
    GetCourseEnrollmentsReq (course_id : x),
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
    GetCourseEnrollmentsResp (students == l),
    allA
  )

(* Register a student, no conflicting deregistrations, check if they are registered, they are not *)
let[@goal] courseware_rc (tid : int) (sid : int) =
  allA;
  RegisterStudentReq (student_id == sid);
  starA (anyA - DeregisterStudentReq (student_id == sid));
  Get (key == sid && not (value == true));
  allA
