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

val read : < tid : int ; prevTid : int ; prevCid : int ; value : int list >
[@@obs]

val write : < tid : int ; value : int list > [@@obs]
val followReq : < user : int > [@@gen]
val followResp : < > [@@obs]
val unfollowReq : < user : int > [@@gen]
val unfollowResp : < > [@@obs]

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
    starA (anyA - Write (tid == i) - Read (tid == i)) )

let write ?l:(i = (true : [%v: int])) ?l:(z = (true : [%v: int list])) =
  ( (allA;
     BeginT (tid == i);
     starA (anyA - Commit (tid == i))),
    Write (tid == i && value == z),
    allA )

let read =
  [|
    (* No previous committed transaction *)
    (fun ?l:(i = (true : [%v: int]))
      ?l:(pi = (true : [%v: int]))
      ?l:(pj = (true : [%v: int]))
      ?l:(z = (true : [%v: int list]))
    ->
      ( starA (anyA - Commit true),
        Read (tid == i && emp value && prevTid == -1 && prevCid == -1),
        allA ));
    (* Read within the same transaction *)
    (fun ?l:(i = (true : [%v: int]))
      ?l:(pi = (true : [%v: int]))
      ?l:(pj = (true : [%v: int]))
      ?l:(z = (true : [%v: int list]))
    ->
      ( (allA;
         Write (tid == i && value == z);
         starA (anyA - Commit (tid == i))),
        Read (tid == i && prevTid == pi && prevCid == pj && prevTid == tid),
        allA ));
    (* Read most recent committed transaction *)
    (fun ?l:(i = (true : [%v: int]))
      ?l:(pi = (true : [%v: int]))
      ?l:(pj = (true : [%v: int]))
      ?l:(z = (true : [%v: int list]))
    ->
      ( (starA (anyA - Write (tid == i));
         Write (tid == pi && value == z);
         starA (anyA - Write (tid == i));
         Commit (tid == pi && cid == pj);
         starA (anyA - Commit true - Write (tid == i))),
        Read (tid == i && prevTid == pi && prevCid == pj && not (tid == prevTid)),
        allA ));
  |]

(* Twitter *)
let followReq (i : int) (l : int list) ?l:(x = (true : [%v: int]))
    ?l:(y = (true : [%v: int])) =
  ( allA,
    FollowReq (user == x),
    (BeginT (tid == i);
     Read (tid == i && value == l);
     allA;
     Write (tid == i && value == insert y l);
     allA;
     Commit (tid == i);
     FollowResp true;
     allA) )

let followResp = (allA, FollowResp true, allA)

let unfollowReq (i : int) (l : int list) ?l:(x = (true : [%v: int]))
    ?l:(y = (true : [%v: int])) =
  ( allA,
    UnfollowReq (user == x),
    (BeginT (tid == i);
     Read (tid == i && value == l);
     allA;
     Write (tid == i && value == remove y l);
     allA;
     Commit (tid == i);
     UnfollowResp true;
     allA) )

let unfollowResp = (allA, UnfollowResp true, allA)

(* Global Properties *)
let[@goal] twitter_rc (y : int list) =
  allA;
  Write (value == y);
  starA (anyA - Write (value == y));
  Read (not (value == y));
  allA
