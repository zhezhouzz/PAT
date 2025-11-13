val ( == ) : 'a. 'a -> 'a -> bool
val ( >= ) : int -> int -> bool
val ( < ) : int -> int -> bool

(* Basic Typing *)

val topKey : int
val emptyVal : int
val bottom : int

val beginT : < tid : int > [@@obs]
val commit : < tid : int ; cid : int > [@@obs]

(*
val beginCAS < tid : int ; cas_id : int > [@@obs]
val endCAS < tid : int ; cas_id : int > [@@obs]
*)

val passCAS : < tid : int ; old_head : int ; new_head : int > [@@obs]
val failCAS : < tid : int ; old_head : int ; new_head : int > [@@obs]

val get :
  < tid : int ; prevTid : int ; prevCid : int ; key : int ; value : int ; next : int >
[@@obs]

val put : < tid : int ; key : int ; value : int ; next : int > [@@obs]

val initReq : < > [@@gen]
val initResp : < > [@@obs]
val pushReq : < elem : int > [@@gen]
val pushResp : < > [@@obs]

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

(* compare and swap *)

let passCAS ?l:(i = (true : [%v: int])) ?l:(o = (true : [%v: int])) ?l:(n = (true : [%v: int])) =
  ( (allA;
     Get (tid == i && next == o && key == topKey && value == emptyVal);
     allA;
     Put (tid == i && next == n && key == topKey && value == emptyVal)),
    PassCAS (tid == i && old_head == o && new_head == n),
    allA )

let failCAS ?l:(i = (true : [%v: int])) ?l:(o = (true : [%v: int])) ?l:(n = (true : [%v: int])) =
  ( (allA;
     Get (tid == i && (not (next == o)) && key == topKey && value == emptyVal);
     allA),
    FailCAS (tid == i && old_head == o && new_head == n),
    allA )


let put ?l:(i = (true : [%v: int])) ?l:(k = (true : [%v: int]))
    ?l:(x = (true : [%v: int])) ?l:(y = (true : [%v: int])) =
  ( (allA;
     BeginT (tid == i);
     starA (anyA - Commit (tid == i))),
    Put (tid == i && key == k && next == x && value == y && not (key == next)),
    allA )

let get =
  [|
    (* Read one previous committed transaction *)
    (fun ?l:(i = (true : [%v: int]))
      ?l:(pi = (true : [%v: int]))
      ?l:(pj = (true : [%v: int]))
      ?l:(k = (true : [%v: int]))
      ?l:(y = (true : [%v: int]))
      ?l:(x = (true : [%v: int]))
    ->
      ( (starA (anyA - Put (tid == i && key == k));
         Put (tid == pi && key == k && value == y && next == x);
         starA (anyA - Put (tid == i && key == k));
         Commit (tid == pi && cid == pj);
         starA (anyA - Put (tid == i && key == k))),
        Get
          (tid == i && key == k && prevTid == pi && prevCid == pj
          && (not (tid == prevTid))
          && value == y
          && next == x),
        starA (anyA - Commit (tid == i && cid < pj)) ));
  |]

(* stack operations *)

let initReq (i : int) (x : int) =
  ( epsilonA,
    InitReq true,
    (BeginT (tid == i);
     Put (tid == i && key == topKey && next == bottom && value == emptyVal);
     Commit (tid == i);
     InitResp true;
     allA) )

let initResp = (allA, InitResp true, allA)

let pushReq (i : int) (y : int) (x : int) (a : int) (* a represents the key of a head for CASfail, model 0/1 CASfail *)
            ?l:(e = (true : [%v : int])) =
  ( allA,
    PushReq (elem == e),
    (BeginT (tid == i);
     (*allA;*)
     (*epsilonA || ((Get (tid == i && key == topKey && next == a);
                   Put (tid == i && key == y && value == e && next == a);
                   allA;
                   Get (tid == i && key == topKey && not (next == a));
                   FailCAS (tid == i && old_head == a && new_head == y)));*)
     Get (tid == i && key == topKey && next == x && value == emptyVal);
     Put (tid == i && key == y && value == e && next == x);

     Get (tid == i && next == x && key == topKey && value == emptyVal);
     (*allA;*)
     Put (tid == i && next == y && key == topKey && value == emptyVal);
     PassCAS (tid == i && old_head == x && new_head == y);

     (*allA;*)
     Commit (tid == i);
     PushResp true;
     allA ) )
  


let pushResp = (allA, PushResp true, allA)

let popReq (i : int) (x : int) (y : int) (e : int) (a : int) = (* a represents the key of a head for CASfail, model 0/1 CASfail *)
  ( allA,
    PopReq true,
    (BeginT (tid == i);
     (*allA;*)
     (*epsilonA || ((Get (tid == i && key == topKey && next == a && value == emptyVal);
                   allA;
                   Get (tid == i && key == a && next == y);
                   FailCAS (tid == i && old_head == a && new_head == y)));*)
        (Get (tid == i && key == topKey && next == x && value == emptyVal);
         Get (tid == i && key == x && value == e && next == y);
         Get (tid == i && next == x && key == topKey && value == emptyVal);
         allA;
         Put (tid == i && next == y && key == topKey && value == emptyVal);
         PassCAS (tid == i && old_head == x && new_head == y);
         Commit (tid == i);
         PopResp (elem == e);) 
      || 
        (Get (tid == i && key == topKey && next == bottom && value == emptyVal);
         Commit (tid == i);
         PopResp (elem == emptyVal));
     allA ) )

let popResp = (allA, PopResp true, allA)



(* global properties *)




let[@goal] t_stack_cc (e : int) (t_1 : int) (t_2 : int) = (* low detail: looking for a program *)

  (* setup *)
  InitReq true;
  allA;
  InitResp true;

  PushReq (elem == e && not (elem == emptyVal));
  starA (anyA - PushReq true - PopReq true);
  PushResp true;

  (* behavior under test *)
  PopReq true;
  starA (anyA - PushReq true);
  PopReq true;
  starA (anyA - PushReq true);

  PopResp (elem == e);
  starA (anyA - PushReq true);
  PopResp (elem == e)

