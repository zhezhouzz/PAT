val ( == ) : 'a. 'a -> 'a -> bool
val ( >= ) : int -> int -> bool
val ( < ) : int -> int -> bool
val ( + ) : int -> int -> int
val cons : int -> int -> int
val remove : int -> int -> int
val emp : int -> bool

(* Basic Typing *)

val beginT : < tid : int > [@@obs]
val commit : < tid : int ; cid : int > [@@obs]

val get :
  < tid : int ; prevTid : int ; prevCid : int ; key : int ; value : int >
[@@obs]

val put : < tid : int ; key : int ; value : int > [@@obs]
val writeReq : < value : int > [@@gen]
val writeResp : < value : int > [@@obs]
val readReq : < > [@@gen]
val readResp : < value : int > [@@obs]

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
    ?l:(z = (true : [%v: int])) =
  ( (allA;
     BeginT (tid == i);
     starA (anyA - Commit (tid == i))),
    Put (tid == i && key == k && value == z),
    allA )

let get =
  [|
    (* Read one previous committed transaction *)
    (fun ?l:(i = (true : [%v: int]))
      ?l:(pi = (true : [%v: int]))
      ?l:(pj = (true : [%v: int]))
      ?l:(k = (true : [%v: int]))
      ?l:(z = (true : [%v: int]))
    ->
      ( (starA (anyA - Put (tid == i && key == k));
         Put (tid == pi && key == k && value == z);
         starA (anyA - Put (tid == i && key == k));
         Commit (tid == pi && cid == pj);
         starA (anyA - Put (tid == i && key == k))),
        Get
          (tid == i && key == k && prevTid == pi && prevCid == pj
          && (not (tid == prevTid))
          && value == z),
        starA (anyA - Commit (tid == i && cid < pj)) ));
  |]

(* Cart *)

let writeReq (i : int) ?l:(x = (true : [%v: int])) =
  ( allA,
    WriteReq true,
    (BeginT (tid == i);
     Put (tid == i && key == 0 && value == x);
     starA (anyA - Put (key == 0));
     (* no write - write conflict *)
     Commit (tid == i);
     WriteResp (value == x);
     allA) )

let writeResp =
 fun ?l:(x = (true : [%v: int])) -> (allA, WriteResp (value == x), allA)

let readReq (i : int) (x : int) =
  ( allA,
    ReadReq true,
    (BeginT (tid == i);
     allA;
     Get (tid == i && key == 0 && value == x);
     Commit (tid == i);
     ReadResp (value == x);
     allA) )

let readResp =
 fun ?l:(x = (true : [%v: int])) -> (allA, ReadResp (value == x), allA)

(* Global Properties *)
let[@goal] read_cc (y : int) =
  allA;
  WriteResp (value == y);
  ReadReq true;
  allA;
  ReadResp (not (value == y));
  allA
