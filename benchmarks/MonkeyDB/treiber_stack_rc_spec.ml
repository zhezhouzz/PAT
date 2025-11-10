val ( == ) : 'a. 'a -> 'a -> bool
val ( >= ) : int -> int -> bool

(* Basic Typing *)

val beginT : < tid : int > [@@obs]
val commit : < tid : int ; cid : int > [@@obs]
val put : < tid : int ; key : int ; value : int ; next : int > [@@obs]

val get :
  < tid : int
  ; prevTid : int
  ; prevCid : int
  ; key : int
  ; value : int
  ; next : int >
[@@obs]

val write : < tid : int ; addr : int > [@@obs]
val read : < tid : int ; prevTid : int ; prevCid : int ; addr : int > [@@obs]
val initReq : < > [@@gen]
val pushReq : < elem : int > [@@gen]
val popReq : < > [@@gen]
val popResp : < elem : int > [@@obs]

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
    ?l:(z = (true : [%v: int])) ?l:(n = (true : [%v: int])) =
  ( (allA;
     BeginT (tid == i);
     starA (anyA - Commit (tid == i))),
    Put (tid == i && key == k && value == z && next == n),
    allA )

let get =
  [|
    (* No previous committed transaction *)
    (fun ?l:(i = (true : [%v: int]))
      ?l:(pi = (true : [%v: int]))
      ?l:(pj = (true : [%v: int]))
      ?l:(k = (true : [%v: int]))
      ?l:(z = (true : [%v: int]))
      ?l:(n = (true : [%v: int]))
    ->
      ( starA (anyA - Commit true),
        Get
          (tid == i && key == k && value == z && next == n && prevTid == -1
         && prevCid == -1),
        allA ));
    (* Read within the same transaction *)
    (fun ?l:(i = (true : [%v: int]))
      ?l:(pi = (true : [%v: int]))
      ?l:(pj = (true : [%v: int]))
      ?l:(k = (true : [%v: int]))
      ?l:(z = (true : [%v: int]))
      ?l:(n = (true : [%v: int]))
    ->
      ( (allA;
         Put (tid == i && key == k && value == z && next == n);
         starA (anyA - Commit (tid == i))),
        Get
          (tid == i && key == k && value == z && next == n && prevTid == pi
         && prevCid == pj && prevTid == tid),
        allA ));
    (* Read most recent committed transaction *)
    (fun ?l:(i = (true : [%v: int]))
      ?l:(pi = (true : [%v: int]))
      ?l:(pj = (true : [%v: int]))
      ?l:(k = (true : [%v: int]))
      ?l:(z = (true : [%v: int]))
      ?l:(n = (true : [%v: int]))
    ->
      ( (starA (anyA - Put (tid == i && key == k));
         Put (tid == pi && key == k && value == z);
         starA (anyA - Put (tid == i && key == k));
         Commit (tid == pi && cid == pj);
         starA (anyA - Commit true - Put (tid == i && key == k))),
        Get
          (tid == i && key == k && value == z && next == n && prevTid == pi
         && prevCid == pj
          && not (tid == prevTid)),
        allA ));
  |]

let write ?l:(i = (true : [%v: int])) ?l:(z = (true : [%v: int])) =
  ( (allA;
     BeginT (tid == i);
     starA (anyA - Commit (tid == i))),
    Write (tid == i && addr == z),
    allA )

let read =
  [|
    (* No previous committed transaction *)
    (fun ?l:(i = (true : [%v: int]))
      ?l:(pi = (true : [%v: int]))
      ?l:(pj = (true : [%v: int]))
      ?l:(z = (true : [%v: int]))
    ->
      ( starA (anyA - Commit true),
        Read (tid == i && addr == z && prevTid == -1 && prevCid == -1),
        allA ));
    (* Read within the same transaction *)
    (fun ?l:(i = (true : [%v: int]))
      ?l:(pi = (true : [%v: int]))
      ?l:(pj = (true : [%v: int]))
      ?l:(z = (true : [%v: int]))
    ->
      ( (allA;
         Write (tid == i && addr == z);
         starA (anyA - Commit (tid == i))),
        Read
          (tid == i && addr == z && prevTid == pi && prevCid == pj
         && prevTid == tid),
        allA ));
    (* Read most recent committed transaction *)
    (fun ?l:(i = (true : [%v: int]))
      ?l:(pi = (true : [%v: int]))
      ?l:(pj = (true : [%v: int]))
      ?l:(z = (true : [%v: int]))
    ->
      ( (starA (anyA - Write (tid == i));
         Write (tid == pi && addr == z);
         starA (anyA - Write (tid == i));
         Commit (tid == pi && cid == pj);
         starA (anyA - Commit true - Write (tid == i))),
        Read
          (tid == i && addr == z && prevTid == pi && prevCid == pj
          && not (tid == prevTid)),
        allA ));
  |]

(* Treiber Stack *)
let initReq (i : int) =
  ( allA,
    InitReq true,
    (BeginT (tid == i);
     Write (tid == i);
     allA;
     Commit (tid == i);
     allA) )

let pushReq (i : int) (olda : int) (ii : int) (newa : int)
    ?l:(x = (true : [%v: int])) =
  ( allA,
    PushReq (elem == x),
    (BeginT (tid == i);
     Read (tid == i && addr == olda);
     Commit (tid == i);
     allA;
     BeginT (tid == ii);
     Read (tid == ii && addr == olda);
     allA;
     Write (tid == ii && addr == newa && not (addr == olda));
     Put (tid == ii && key == newa && value == x && next == newa);
     allA;
     Commit (tid == ii);
     allA) )

let popReq (i : int) (olda : int) (ii : int) (newa : int) (x : int) =
  ( allA,
    PushReq (elem == x),
    (BeginT (tid == i);
     Read (tid == i && addr == olda);
     Get (tid == i && key == olda && value == x && next == newa);
     Commit (tid == i);
     allA;
     BeginT (tid == ii);
     Read (tid == ii && addr == olda);
     allA;
     Write (tid == ii && addr == newa && not (addr == olda));
     allA;
     Commit (tid == ii);
     PopResp (elem == x);
     allA) )

let popResp ?l:(x = (true : [%v: int])) = (allA, PopResp (elem == x), allA)

(* Global Properties *)
let[@goal] treiber_stack_rc (x : int) (y : int) =
  allA;
  Put (key == x && value == y);
  starA (anyA - Put (key == x));
  Get (key == x && not (value == y));
  allA
