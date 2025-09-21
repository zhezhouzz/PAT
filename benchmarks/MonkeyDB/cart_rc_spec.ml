val ( == ) : 'a. 'a -> 'a -> bool
val ( >= ) : int -> int -> bool
val cons : int -> int list -> int list
val remove : int -> int list -> int list
val emp : int list -> bool

let[@axiom] emp_same (l1 : int list) (l2 : int list) =
  implies (emp l1 && emp l2) (l1 == l2)

let[@axiom] cons_not_eq (x : int) (l : int list) = not (cons x l == l)

let[@axiom] remove_cons1 (x : int) (l : int list) =
  implies (emp l) (remove x (cons x l) == l)

let[@axiom] remove_cons2 (x : int) (l : int list) =
  implies (emp l) (remove x (cons x (cons x l)) == cons x l)

let[@axiom] remove_emp (x : int) (l : int list) =
  implies (emp l) (remove x l == l)

(* Basic Typing *)

val beginT : < tid : int > [@@obs]
val commit : < tid : int ; cid : int > [@@obs]

val get :
  < tid : int ; prevTid : int ; prevCid : int ; key : int ; value : int list >
[@@obs]

val put : < tid : int ; key : int ; value : int list > [@@obs]
val newUserReq : < user : int > [@@gen]
val newUserResp : < > [@@obs]
val addItemReq : < user : int ; item : int > [@@gen]
val addItemResp : < > [@@obs]
val deleteItemReq : < user : int ; item : int > [@@gen]
val deleteItemResp : < > [@@obs]

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
         Put (tid == pi && key == k && value == z);
         starA (anyA - Put (tid == i && key == k));
         Commit (tid == pi && cid == pj);
         starA (anyA - Commit true - Put (tid == i && key == k))),
        Get
          (tid == i && key == k && prevTid == pi && prevCid == pj
          && (not (tid == prevTid))
          && value == z),
        allA ));
  |]

(* Cart *)

let newUserReq (i : int) ?l:(x = (true : [%v: int])) =
  ( starA (anyA - NewUserReq true - Get (key == x) - Put (key == x)),
    NewUserReq (user == x),
    (BeginT (tid == i);
     Put (tid == i && key == x && emp value);
     Commit (tid == i);
     NewUserResp true;
     starA (anyA - NewUserReq true)) )

let newUserResp = (allA, NewUserResp true, allA)

let addItemReq (i : int) (l : int list) ?l:(x = (true : [%v: int]))
    ?l:(y = (true : [%v: int])) =
  ( allA,
    AddItemReq (user == x && item == y),
    (BeginT (tid == i);
     Get (tid == i && key == x && value == l);
     allA;
     Put (tid == i && key == x && value == cons y l);
     starA (anyA - Put (key == x));
     (* no write - write conflict *)
     Commit (tid == i);
     AddItemResp true;
     allA) )

let addItemResp = (allA, AddItemResp true, allA)

let deleteItemReq (i : int) (l : int list) ?l:(x = (true : [%v: int]))
    ?l:(y = (true : [%v: int])) =
  ( allA,
    DeleteItemReq (user == x && item == y),
    (BeginT (tid == i);
     Get (tid == i && key == x && value == l);
     allA;
     Put (tid == i && key == x && value == remove y l);
     starA (anyA - Put (key == x));
     Commit (tid == i);
     DeleteItemResp true;
     allA) )

let deleteItemResp = (allA, DeleteItemResp true, allA)

(* Global Properties *)
let[@goal] cart_rc (xx : int) (yy : int list) =
  allA;
  Put (key == xx && value == yy);
  starA (anyA - Put (key == xx));
  Get (key == xx && not (value == yy));
  allA
