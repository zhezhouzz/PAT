val ( == ) : 'a. 'a -> 'a -> bool
(* Basic Typing *)

val pushReq : < elem : int > [@@gen]
val initStackReq : < > [@@gen]
val popReq : < > [@@gen]
val popResp : < elem : int > [@@obs]
val isEmptyReq : < > [@@gen]
val isEmptyResp : < isEmpty : bool > [@@obs]

(* PATs *)
let pushReq ?l:(x = (true : [%v: int])) =
  ( starA (anyA - PushReq (elem == x)),
    PushReq (elem == x),
    (allA;
     PopReq true;
     allA) )

let initStackReq = (allA, InitStackReq true, allA)

let popReq =
  ( allA,
    PopReq true,
    (PopResp true;
     allA) )

let popResp = (allA, PopResp true, starA (anyA - PushReq true))

let isEmptyReq =
  ( allA,
    IsEmptyReq true,
    (IsEmptyResp true;
     allA) )

let isEmptyResp ?l:(z = (true : [%v: bool])) =
  (allA, IsEmptyResp (isEmpty == z), allA)

(* Global Properties *)
(* previous inserted element is lost *)
let[@goal] stack (y : int) =
  allA;
  PushReq (elem == y);
  starA (anyA - PopResp (elem == y));
  IsEmptyResp (isEmpty == true)
