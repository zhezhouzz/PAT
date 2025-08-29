val ( == ) : 'a. 'a -> 'a -> bool
val initStackReq : < > [@@gen]
val pushReq : < elem : int > [@@gen]
val popReq : < > [@@gen]
val popResp : < elem : int > [@@obs]
val isEmptyReq : < > [@@gen]
val isEmptyResp : < isEmpty : bool > [@@obs]

let initStackReq = (allA, InitStackReq true, [||])

let pushReq ?l:(x = (true : [%v: int])) =
  (allA, PushReq (elem == x), [| PopReq true |])

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
  (allA, IsEmptyResp (isEmpty == z), [||])

let[@goal] elemLost (y : int) =
  allA;
  PushReq (elem == y);
  starA (anyA - PopResp (elem == y));
  IsEmptyResp (isEmpty == true)
