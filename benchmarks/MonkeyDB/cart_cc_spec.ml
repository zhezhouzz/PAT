val ( == ) : 'a. 'a -> 'a -> bool
val ( >= ) : int -> int -> bool
val ( < ) : int -> int -> bool
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
val addItemReq : < user : int ; item : int > [@@gen]
val addItemResp : < > [@@obs]
val deleteItemReq : < user : int ; item : int > [@@gen]
val deleteItemResp : < > [@@obs]

(* Causal Consistency *)
(* Invariant: For any transaction with tid = i, there does not exist a previous transaction with tid > i. *)
let beginT ?l:(i = (true : [%v: int])) =
  (starA (anyA - BeginT (tid >= i)), BeginT (tid == i), allA)

(* Invariant: Each transaction is committed exactly once; there does not exist a previous commit with cid > j. *)
let commit ?l:(i = (true : [%v: int])) ?l:(j = (true : [%v: int])) =
  ( (starA (anyA - Commit (tid == i || cid >= j));
     BeginT (tid == i);
     starA (anyA - Commit (tid == i || cid >= j))),
    Commit (tid == i && cid == j),
    allA )

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
    (* Read one previous committed transaction *)
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
         starA (anyA - Put (tid == i && key == k))),
        Get
          (tid == i && key == k && prevTid == pi && prevCid == pj
          && not (tid == prevTid)),
        starA (anyA - Commit (tid == i && cid < pj)) ));
  |]

(* Cart *)
let addItemReq (i : int) (l : int list) ?l:(x = (true : [%v: int]))
    ?l:(y = (true : [%v: int])) =
  ( allA,
    AddItemReq (user == x && item == y),
    (BeginT (tid == i);
     Get (tid == i && key == x && value == l);
     allA;
     Put (tid == i && key == x && value == cons y l);
     allA;
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
     allA;
     Commit (tid == i);
     DeleteItemResp true;
     allA) )

let deleteItemResp = (allA, DeleteItemResp true, allA)

(* Global Properties *)
let[@goal] cart_cc (x : int) (y : int list) =
  allA;
  Put (key == x && value == y);
  starA (anyA - Put (key == x));
  Get (key == x && not (value == y));
  allA
