val ( == ) : 'a. 'a -> 'a -> bool
val ( >= ) : int -> int -> bool
val insert : int -> int list -> int list
val remove : int -> int list -> int list
val emp : int list -> bool

let[@axiom] emp_same (l1 : int list) (l2 : int list) =
  implies (emp l1 && emp l2) (l1 == l2)

let[@axiom] insert_not_eq (x : int) (l : int list) = not (insert x l == l)

let[@axiom] remove_insert1 (x : int) (l : int list) =
  implies (emp l) (remove x (insert x l) == l)

let[@axiom] remove_insert2 (x : int) (l : int list) =
  implies (emp l) (remove x (insert x (insert x l)) == insert x l)

let[@axiom] remove_emp (x : int) (l : int list) =
  implies (emp l) (remove x l == l)

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

val registerStudentReq : < user : int > [@@gen]
val registerStudentResp : < > [@@obs]

(* val deregisterStudentReq : < student_id : int > [@@gen]
val deregisterStudentResp : < success : bool > [@@obs] *)

val createCourseReq : < course_id : int > [@@gen]
val createCourseResp : < success : bool > [@@obs]

(* val deleteCourseReq : < course_id : int > [@@gen]
val deleteCourseResp : < success : bool > [@@obs] *)

val enrollStudentReq : < user : int ; item : int > [@@gen]
val enrollStudentResp : < > [@@obs]
val unenrollStudentReq : < user : int ; item : int > [@@gen]
val unenrollStudentResp : < > [@@obs]

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
    starA (anyA - PutStudentEnrollments (tid == i) - GetStudentEnrollments (tid == i)) )


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

let putStudentEnrollments ?l:(i = (true : [%v: int])) ?l:(k = (true : [%v: int]))
    ?l:(z = (true : [%v: int list])) =
  ( (allA;
     BeginT (tid == i);
     starA (anyA - Commit (tid == i))),
    PutStudentEnrollments (tid == i && key == k && value == z),
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
         PutStudentEnrollments (tid == pi && key == k && value == z);
         starA (anyA - PutStudentEnrollments (tid == i && key == k));
         Commit (tid == pi && cid == pj);
         starA (anyA - Commit true - PutStudentEnrollments (tid == i && key == k))),
        GetStudentEnrollments
          (tid == i && key == k && prevTid == pi && prevCid == pj
          && (not (tid == prevTid))
          && value == z),
        allA ));
  |]

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

let registerStudentReq (i : int) ?l:(x = (true : [%v: int])) =
  ( 
    starA (anyA - RegisterStudentReq true 
      - GetStudentEnrollments (key == x) - PutStudentEnrollments (key == x)
      - GetStudents (key == x) - PutStudents (key == x)),
    RegisterStudentReq (user == x),
    (
      BeginT (tid == i);
      PutStudentEnrollments (tid == i && key == x && emp value);
      PutStudents (tid == i && key == x && value == true);
      Commit (tid == i);
      RegisterStudentResp true;
      starA (anyA - RegisterStudentReq true)
    ) 
  )

let registerStudentResp = (allA, RegisterStudentResp true, allA)

(* let deregisterStudentReq (i : int) (l : int list)
  ?l:(x = (true : [%v: int]))
  ?l:(y = (true : [%v: bool])) = 
  (
    allA,
    DeregisterStudentReq (student_id == x),
    (
      BeginT (tid == i);
      GetStudents (tid == i && key == x);
      allA;
      PutStudents (tid == i && key == x && value == false);
      starA (anyA - PutStudents (key == x));
      (* GetStudentEnrollments (tid == i && key == x && value == l);
      (* TODO: figure out how to encode the loop *)
      (* starA (

      ); *)
      PutStudentEnrollments (tid == i && key == x && emp value); *)
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
  ) *)

let createCourseReq (i : int) ?l:(x = (true : [%v: int])) = 
  (
    allA,
    CreateCourseReq (course_id == x),
    (
      BeginT (tid == i);
      PutCourses (tid == i && key == x && value == true);
      PutCourseEnrollments (tid == i && key == x && emp value);
      Commit (tid == i);
      CreateCourseResp true;
      allA
    )
  )

let createCourseResp = (allA, CreateCourseResp true, allA)

(* let deleteCourseReq (i : int) (l : int list)
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
  ) *)

let enrollStudentReq (i : int) (l : int list) ?l:(x = (true : [%v: int]))
    ?l:(y = (true : [%v: int])) =
  ( 
    allA,
    EnrollStudentReq (user == x && item == y),
    (
      BeginT (tid == i);
      GetStudentEnrollments (tid == i && key == x && value == l);
      allA;
      PutStudentEnrollments (tid == i && key == x && value == insert y l);
      starA (anyA - PutStudentEnrollments (key == x));
      (* no write - write conflict *)
      Commit (tid == i);
      EnrollStudentResp true;
      allA
    ) 
  )

let enrollStudentResp = (allA, EnrollStudentResp true, allA)

let unenrollStudentReq (i : int) (l : int list) ?l:(x = (true : [%v: int]))
    ?l:(y = (true : [%v: int])) =
  ( 
    allA,
    UnenrollStudentReq (user == x && item == y),
    (
      BeginT (tid == i);
      GetStudentEnrollments (tid == i && key == x && value == l);
      allA;
      PutStudentEnrollments (tid == i && key == x && value == remove y l);
      starA (anyA - PutStudentEnrollments (key == x));
      Commit (tid == i);
      UnenrollStudentResp true;
      allA
    ) 
  )

let unenrollStudentResp = (allA, UnenrollStudentResp true, allA)

(* Goals *)

let[@goal] courseware_rc (x : int) (y : int list) =
  allA;
  PutStudentEnrollments (key == x && value == y);
  starA (anyA - PutStudentEnrollments (key == x));
  GetStudentEnrollments (key == x && not (value == y));
  allA