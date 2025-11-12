val ( == ) : 'a. 'a -> 'a -> bool
val ( >= ) : int -> int -> bool
val ( < ) : int -> int -> bool

(* Basic Typing *)

val topKey : int
val emptyVal : int

val beginT : < tid : int > [@@obs]
val commit : < tid : int ; cid : int > [@@obs]

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
    Put (tid == i && key == k && next == x && value == y),
    allA )

let get =
  [|
    (* Read one previous committed transaction *)
    (fun ?l:(i = (true : [%v: int]))
      ?l:(pi = (true : [%v: int]))
      ?l:(pj = (true : [%v: int]))
      ?l:(k = (true : [%v: int]))
      ?l:(x = (true : [%v: int]))
      ?l:(y = (true : [%v: int]))
    ->
      ( (starA (anyA - Put (tid == i && key == k));
         Put (tid == pi && key == k && next == x && value == y);
         starA (anyA - Put (tid == i && key == k));
         Commit (tid == pi && cid == pj);
         starA (anyA - Commit true - Put (tid == i && key == k))),
        Get
          (tid == i && key == k && prevTid == pi && prevCid == pj
          && (not (tid == prevTid))
          && next == x
          && value == y),
        allA ));
  |]


(* stack operations *)

let initReq (i : int) (x : int) =
  ( epsilonA,
    InitReq true,
    (BeginT (tid == i);
     Put (tid == i && key == topKey && next == x && value == emptyVal);
     Commit (tid == i);
     InitResp true;
     allA) )

let initResp = (allA, InitResp true, allA)

let pushReq (i : int) (y : int) (x : int) (a : int) (* a represents the key of a head for CASfail, model 0/1 CASfail *)
            ?l:(e = (true : [%v : int])) =
  ( allA,
    PushReq (elem == e),
    (BeginT (tid == i);
     allA;
     (*epsilonA || ((Get (tid == i && key == topKey && next == a);
                   Put (tid == i && key == y && value == e && next == a);
                   starA (anyA - PassCAS (tid == i) - Put (key == y));
                   FailCAS (tid == i && old_head == a && new_head == y)));*)
     Get (tid == i && key == topKey && next == x && value == emptyVal);
     Put (tid == i && key == y && value == e && next == x);

     (*allA;*)
     Get (tid == i && next == x && key == topKey && value == emptyVal);
     (*allA;*)
     Put (tid == i && next == y && key == topKey && value == emptyVal);
     (*allA;*)
     PassCAS (tid == i && old_head == x && new_head == y);

     allA;
     Commit (tid == i);
     PushResp true;
     allA ) )
     
let pushResp = (allA, PushResp true, allA)

let popReq (i : int) (x : int) (y : int) (e : int) (a : int) = (* a represents the key of a head for CASfail, model 0/1 CASfail *)
  ( allA,
    PopReq true,
    (BeginT (tid == i);
     allA;
     epsilonA || ((Get (tid == i && key == topKey && next == a && value == emptyVal);
                   Get (tid == i && key == a && next == y);
                   starA (anyA - PassCAS (tid == i));
                   FailCAS (tid == i && old_head == a && new_head == y)));
     Get (tid == i && key == topKey && next == x && value == emptyVal);
     Get (tid == i && key == x && value == e && next == y);
     Put (tid == i && key == topKey && value == emptyVal && next == y);
     PassCAS (tid == i && old_head == x && new_head == y);
     starA (anyA - Put (tid == i));
     Commit (tid == i);
     PopResp true;
     allA ) )

let popResp = (allA, PopResp true, allA)



(* global properties *)
(*
let[@goal] t_stack (*treiber_stack_rc*) (x : int) (y : int) =
  InitReq true;
  allA;
  PassCAS (new_head == x);
  starA (anyA - PassCAS true);
  Get (key == topKey && not (next == x));
  allA
*)
(*
let[@goal] t_stack (x : int) (* rc *) =
  InitReq true;
  allA;
  Put (key == topKey && next == x);
  starA (anyA - Put (key == topKey && next == x));
  Get (key == topKey && not (next == x));
  allA
*)

let[@goal] t_stack_rc (x : int) (e_1 : int) (e_2 : int) (t_1 : int) (t_2 : int) = (* low detail: looking for a program *)
  InitReq true;
  (*allA;*)
  Put true;
  Commit true;
  InitResp true;
  
  PushReq (elem == e_1);
  BeginT (tid == t_1);
  Get (tid == t_1);
  Put (tid == t_1);
  Get (tid == t_1);
  Put (tid == t_1);
  PassCAS (tid == t_1);
  Commit (tid == t_1);
  PushResp true
  (*
  PushReq (elem == e_2);
  BeginT (tid == t_2);
  Get (tid == t_2);
  Put (tid == t_2);
  Get (tid == t_2);
  Put (tid == t_2);
  PassCAS (tid == t_2);
  PushResp true*)
(*InitReq true;
  allA;
  Put (key == topKey && next == x);
  starA (anyA - Put (key == topKey));
  Get (key == topKey && not (next == x));
  allA;*)

(*
let[@goal] t_stack (* rc *) (* high detail: most of the outline of the program we're looking for *)
      (x : int) (y_1 : int) (y_2 : int) (t_0 : int) (t_1 : int) (t_2 : int) (a : int) (b : int) = 
  InitReq true;
  allA;
  InitResp true;

  (* t_1 *)
  PushReq (elem == a);
  BeginT (tid == t_1);
  Get (tid == t_1 && key == topKey && next == x && value == emptyVal);
  Put (tid == t_1 && key == y_1 && next == x && value == a);
  Get (tid == t_1 && next == x && key == topKey && value == emptyVal);
  Put (tid == t_1 && next == y_1 && key == topKey && value == emptyVal);


  (* t_2 *)
  PushReq (elem == b);
  BeginT (tid == t_2);
  Get (tid == t_2 && key == topKey && next == x && value == emptyVal);
  Put (tid == t_2 && key == y_2 && next == x && value == b);
  Get (tid == t_2 && key == x && key == topKey && value == emptyVal);
  Put (tid == t_2 && key == topKey && next == y_2 && value == emptyVal);
  Commit (tid == t_2);
  PushResp true;

  Commit (tid == t_1);
  starA (anyA - Put (key == topKey));
  Get (key == topKey && not (next == x));
  allA;
*)


  (*InitReq true;
  allA;
  Put (key == topKey && next == x);
  starA (anyA - Put (key == topKey));
  Get (key == topKey && not (next == x));
  allA;*)
  (*InitReq true;
  allA;
  PushReq true;
  allA;
  PushReq true;
  allA;
  Put (key == topKey && next == x);
  starA (anyA - Put (key == topKey));
  (*PushReq true;
  starA (anyA - Put (key == topKey));*)
  Get (key == topKey && not (next == x));
  allA*)

(*let[@goal] smallbank_rc (c : int) (b : int) =
  allA;
  UpdateSavings (custid == c && balance == b);
  starA (anyA - UpdateSavings (custid == c));
  SelectSavings (custid == c && not (balance == b));
  allA*)



  (*
  Get (key == topKey && next == x);
  starA (anyA - PassCAS true);
  Get (key == topKey && (not (next == x)));
  allA
  *)
  
  (*Put (key == x && value == y);
  starA (anyA - Put (key == x));
  Get (key == x && not (value == y));
  allA*)

(* the next read after our successful CAS should have our value in it *)