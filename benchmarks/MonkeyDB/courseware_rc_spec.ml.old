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
val enrollReq : < user : int ; course : int > [@@gen]
val enrollResp : < > [@@obs]
val unenrollReq : < user : int ; course : int > [@@gen]
val unenrollResp : < > [@@obs]

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
    (* No previous committed transaction *)
    (fun ?l:(i = (true : [%v: int]))
      ?l:(pi = (true : [%v: int]))
      ?l:(pj = (true : [%v: int]))
      ?l:(k = (true : [%v: int]))
      ?l:(z = (true : [%v: int list]))
    ->
      ( starA (anyA - Commit true),
        Get (tid == i && key == k && emp value && prevTid == -1 && prevCid == -1),
        allA ));
    (* Read within the same transaction *)
    (fun ?l:(i = (true : [%v: int]))
      ?l:(pi = (true : [%v: int]))
      ?l:(pj = (true : [%v: int]))
      ?l:(k = (true : [%v: int]))
      ?l:(z = (true : [%v: int list]))
    ->
      ( (allA;
         Put (tid == i && key == k && value == z);
         starA (anyA - Commit (tid == i))),
        Get
          (tid == i && key == k && prevTid == pi && prevCid == pj
         && prevTid == tid),
        allA ));
    (* Read most recent committed transaction *)
    (fun ?l:(i = (true : [%v: int]))
      ?l:(pi = (true : [%v: int]))
      ?l:(pj = (true : [%v: int]))
      ?l:(k = (true : [%v: int]))
      ?l:(z = (true : [%v: int list]))
    ->
      ( (starA (anyA - Put (tid == i && key == k));
         Put (tid == pi && key == k && value == z);
         starA (anyA - Put (tid == i && key == k));
         Commit (tid == pi && cid == pj);
         starA (anyA - Commit true - Put (tid == i && key == k))),
        Get
          (tid == i && key == k && prevTid == pi && prevCid == pj
          && not (tid == prevTid)),
        allA ));
  |]

(* Cart *)
let enrollReq (i : int) (l : int list) ?l:(x = (true : [%v: int]))
    ?l:(y = (true : [%v: int])) =
  ( allA,
    EnrollReq (user == x && course == y),
    (BeginT (tid == i);
     Get (tid == i && key == x && value == l);
     allA;
     Put (tid == i && key == x && value == insert y l);
     allA;
     Commit (tid == i);
     EnrollResp true;
     allA) )

let enrollResp = (allA, EnrollResp true, allA)

let unenrollReq (i : int) (l : int list) ?l:(x = (true : [%v: int]))
    ?l:(y = (true : [%v: int])) =
  ( allA,
    UnenrollReq (user == x && course == y),
    (BeginT (tid == i);
     Get (tid == i && key == x && value == l);
     allA;
     Put (tid == i && key == x && value == remove y l);
     allA;
     Commit (tid == i);
     UnenrollResp true;
     allA) )

let unenrollResp = (allA, UnenrollResp true, allA)

(* Global Properties *)
let[@goal] courseware_rc (x : int) (y : int list) =
  allA;
  Put (key == x && value == y);
  starA (anyA - Put (key == x));
  Get (key == x && not (value == y));
  allA
