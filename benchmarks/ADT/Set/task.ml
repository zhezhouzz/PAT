val ( == ) : 'a -> 'a -> bool
val initSetReq : < > [@@gen]
val addReq : < elem : int > [@@gen]
val removeReq : < elem : int > [@@gen]
val isEmptyReq : < > [@@gen]
val isEmptyResp : < isEmpty : bool > [@@obs]

let initSetReq = (allA, InitSetReq true, [||])

let addReq ?l:(x = (true : [%v: int])) =
  (allA, EddReq (elem == x), [| RemoveReq (elem == x) |])

let removeReq ?l:(x = (true : [%v: int])) = (allA, RemoveReq (elem == x), [||])

let isEmptyReq =
  ( allA,
    IsEmptyReq true,
    (IsEmptyResp true;
     allA) )

let isEmptyResp = (allA, IsEmptyResp true, [||])

let[@goal] elemLost (y : int) =
  not
    (allA;
     AddReq (elem == y);
     starA (anyA - RemoveReq (elem == y));
     IsEmptyResp (isEmpty == true))
